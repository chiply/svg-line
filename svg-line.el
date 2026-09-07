;;; svg-line.el --- SVG-rendered tab-bar, tab-line, header-line and mode-line -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Charlie Holland

;; Author: Charlie Holland <mister.chiply@gmail.com>
;; Maintainer: Charlie Holland <mister.chiply@gmail.com>
;; URL: https://github.com/chiply/svg-line
;; x-release-please-start-version
;; Version: 0.1.7
;; x-release-please-end
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, faces, frames

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; svg-line renders the tab-bar, tab-line, header-line and mode-line as
;; SVG images instead of laid-out text.  An SVG image can be any height
;; and is positioned at exact pixel coordinates, which makes possible
;; things the text engine cannot do uniformly:
;;
;;   - multi-line bars (status info, breadcrumbs) of arbitrary height;
;;   - per-line left/right alignment on EVERY line (not just the last,
;;     and without the `:align-to'-on-a-non-final-line redisplay freeze);
;;   - tab lines that WRAP overflowing tabs onto new rows instead of
;;     truncating or horizontally scrolling.
;;
;; Two layout modes:
;;   `lines'  -- rows of (LEFT . RIGHT); left flush-left, right flush-right.
;;   `wrap'   -- a flow of items wrapped across as many rows as needed,
;;               with per-item "current" highlighting (for tab lines).
;;
;; This package is the rendering ENGINE only -- it ships no content and no
;; colours of its own.  You supply a `:content' function and styling in
;; your config and bind it to a target:
;;
;;   (svg-line-define 'my-mode-line
;;     :target 'mode-line
;;     :content #'my-mode-line-rows          ; -> list of (LEFT . RIGHT)
;;     :active  #'mode-line-window-selected-p
;;     :background        (lambda () my-active-bg)
;;     :inactive-background (lambda () my-inactive-bg))
;;   (svg-line-activate 'my-mode-line)
;;
;; Colour/font options accept a literal value OR a zero-argument function
;; evaluated on every render -- so theme-dependent colours (e.g. branching
;; on a dark/light predicate) live in your config and the engine stays
;; theme-agnostic.
;;
;; The renderers are SAFE: each segment is evaluated exactly once (the
;; discipline that avoids redisplay feedback loops), and rendering is
;; wrapped so a Lisp error shows inline instead of breaking the display
;; and re-entrant renders return the last good value instead of looping.

;;; Code:

(require 'svg)
(require 'cl-lib)
(require 'dom)
(require 'color)
(require 'subr-x)
(require 'tab-bar)

(defgroup svg-line nil
  "SVG-rendered tab-bar, tab-line, header-line and mode-line."
  :group 'convenience
  :prefix "svg-line-")

(defcustom svg-line-font nil
  "Default font family for SVG line text.
nil means use the `default' face family at render time."
  :type '(choice (const :tag "default face family" nil) string))

(defcustom svg-line-font-size 15
  "Default font size, in pixels, for SVG line text."
  :type 'integer)

(defcustom svg-line-line-pad 4
  "Default extra vertical padding, in pixels, added per rendered row."
  :type 'integer)

(defcustom svg-line-char-advance nil
  "Per-character advance, in pixels, for run-based layout -- or nil to auto-derive.
This is the assumed pixel width of one monospace character as librsvg renders
the configured font at the configured size.  It positions everything the SVG
text engine does not place by itself: right-aligned content, inline progress
pies and bars, the hit/hover boxes of interactive `:seg' runs, and the point at
which `wrap'-layout rows break.  (Plain all-text rows use exact font anchoring
and ignore it.)

It cannot be measured from Emacs -- librsvg rasterises text with its own font
stack, whose metrics differ from Emacs's -- so it is a calibration constant.
nil derives it from the font size via `svg-line-char-advance-ratio', which
scales correctly across font sizes; set a number to pin the exact advance for
your font (raise it if right-aligned/hover content sits too far left, lower it
if too far right).  A spec's `:char-advance' overrides this per line."
  :type '(choice (const :tag "Auto (font-size * ratio)" nil) number))

(defcustom svg-line-char-advance-ratio 0.6
  "Per-character advance as a fraction of the font size.
Used to derive `svg-line-char-advance' when it (and a spec's `:char-advance')
is nil.  0.6 suits a typical monospace font; condensed faces want less, wide
faces more.  Deriving from the font size keeps run-based layout aligned when
the font size changes, which a fixed pixel advance would not."
  :type 'number)

(defcustom svg-line-glyph-scale 1.3
  "Font-size multiplier for icon (Nerd-Font PUA) glyphs within line text.
Nerd-font icon glyphs are drawn smaller than a text cell; a value >1
enlarges just those glyphs (via a larger `<tspan>'), so icons read at a
comparable weight to the text.  1.0 disables the effect."
  :type 'number)

(defun svg-line--char-advance (explicit font-size &optional ratio)
  "Resolve the per-character advance to use.
EXPLICIT -- a spec's `:char-advance' or `svg-line-char-advance' -- wins when
non-nil; otherwise derive it from FONT-SIZE via RATIO, defaulting to
`svg-line-char-advance-ratio'.

Prefer the RATIO form when a line names its own `:font': a ratio is a
property of the FAMILY and survives text scaling, because it is applied to
whatever font size the line ends up drawn at.  A pinned `:char-advance' is
a property of one family at one size, so it has to be re-scaled alongside
the font, and drifts as the rounding of the two diverges."
  (if explicit
      explicit
    ;; Deliberately NOT rounded.  A real font's advance is rarely a whole
    ;; number of pixels (Terminess is 0.5 em -- 7.5px at font-size 15), and
    ;; rounding it is a per-character error that accumulates: half a pixel
    ;; over a forty-character row is twenty pixels of drift by the right
    ;; margin.  Positions are rounded where they are EMITTED instead, so the
    ;; error stays sub-pixel however long the row.
    (max 1 (* font-size (or ratio svg-line-char-advance-ratio)))))

(defun svg-line--glyph-char-p (ch)
  "Non-nil if CH is in a Nerd-Font / icon Private-Use code range."
  (or (and (>= ch #xE000)   (<= ch #xF8FF))     ; BMP PUA
      (and (>= ch #xF0000)  (<= ch #xFFFFD))    ; Plane-15 PUA-A
      (and (>= ch #x100000) (<= ch #x10FFFD)))) ; Plane-16 PUA-B

(defun svg-line--split-glyph-runs (str)
  "Split STR into a list of (GLYPHP . SUBSTRING); GLYPHP t for icon-glyph runs."
  (let ((runs nil) (n (length str)))
    (when (> n 0)
      (let ((start 0) (cur (svg-line--glyph-char-p (aref str 0))))
        (dotimes (i n)
          (let ((g (svg-line--glyph-char-p (aref str i))))
            (unless (eq g cur)
              (push (cons cur (substring str start i)) runs)
              (setq start i cur g))))
        (push (cons cur (substring str start n)) runs)))
    (nreverse runs)))

(defun svg-line--glyph-advance (char-advance font-size)
  "Advance of ONE icon glyph, given the text CHAR-ADVANCE at FONT-SIZE.
Icon glyphs are drawn in an enlarged tspan (`svg-line-glyph-scale'), so they
advance proportionally more than a text character does.  `svg-line--add-text'
rounds that tspan's size to a whole number, so derive from the ROUNDED size --
otherwise the width reserved here and the width drawn there disagree."
  (if (and (> svg-line-glyph-scale 1.0) (> font-size 0))
      (* char-advance (/ (float (round (* font-size svg-line-glyph-scale)))
                         font-size))
    char-advance))

(defun svg-line--string-width (str char-advance font-size &optional font)
  "Advance width in pixels of STR at CHAR-ADVANCE and FONT-SIZE in FONT.

Text advances by CHAR-ADVANCE per character, icon glyphs by
`svg-line--glyph-advance' -- they are drawn larger, so they take more room.
Counting both at one flat advance is what let an icon overrun its cell: the
glyph is drawn at `svg-line-glyph-scale' but only 1x was reserved for it, so
every icon in a row pushed the content after it that much further right than
the layout believed.

What a corrected font's advance is inflated BY is trailing whitespace -- the
renderer draws the outline in the right place and then moves the pen too far
-- so the reserved width is the true advance and nothing needs adding for
it.  FONT is taken for symmetry with the drawing side and to keep the two
descriptions of a string's width in one place."
  (ignore font)
  (if (<= svg-line-glyph-scale 1.0)
      (* (length str) char-advance)
    (let ((ga (svg-line--glyph-advance char-advance font-size))
          (w 0))
      (dolist (run (svg-line--split-glyph-runs str) w)
        (setq w (+ w (* (length (cdr run)) (if (car run) ga char-advance))))))))

(defcustom svg-line-measure-fonts t
  "Whether to measure a font's real ink by rendering it.

The masthead icon is placed from the ink box of the glyph it draws -- how
much of the em that glyph actually paints, and where.  Those numbers differ
per family (Terminess inks half its em, Monaspace nearly two thirds), so
measuring is what lets one set of geometry serve any font.

nil falls back to fixed constants describing a Terminess-like font, which is
what this package assumed before it could measure.  Set it to nil if the
probe renders are unwelcome; they are cached per font and glyph, and only
happen on a graphical display."
  :type 'boolean)

(defconst svg-line--ink-probe-size 120
  "Font size, in px, at which glyph ink is probed.
Big enough that rounding the render to whole pixels is noise, small enough
that the throwaway raster is trivial.")

(defconst svg-line-icon-ink-fallback '(0.5 0.45 0.0 0.5583)
  "Icon ink box assumed when a font cannot be measured.
\(WIDTH HEIGHT LEFT TOP), each a fraction of the font size: the ink's size,
its offset from the text origin, and its top above the baseline.  These are
Terminess's numbers -- the font this package's masthead geometry grew up
around -- and were hardcoded until it learned to measure.")

(defconst svg-line-icon-ink-probe (string #xF0614)
  "Glyph whose ink calibrates masthead SIZING for a family.
One glyph per font, not per icon: `:icon-scale' is meant to hold its meaning
while the drawn glyph changes, so the normalisation has to come from the
FAMILY rather than from whichever icon is up.  A Nerd-Font Material Design
codepoint, present in every patched family this draws with.")

(defvar svg-line--ink-cache (make-hash-table :test 'equal)
  "Cache of (GLYPH . FONT) -> measured ink box.")

(defvar svg-line--advance-cache (make-hash-table :test 'equal)
  "Cache of FONT -> measured per-character advance, as a fraction of the em.")

(defvar svg-line--native-cache (make-hash-table :test 'equal)
  "Cache of FONT -> advance the font itself declares, as a fraction of the em.")

;;;###autoload
(defun svg-line-forget-font-metrics ()
  "Drop the cached font measurements.
Run after installing, removing or replacing a font."
  (interactive)
  (clrhash svg-line--ink-cache)
  (clrhash svg-line--advance-cache)
  (clrhash svg-line--native-cache))

(defun svg-line--intrinsic-size (svg)
  "Rendered size of the SVG string SVG as (WIDTH . HEIGHT), or nil.

An SVG carrying no width/height renders at its content's bounding box, so
the image this creates is exactly as big as the ink inside it.  That is what
makes a renderer metric -- which librsvg never reports -- readable from
Lisp, and it is the same renderer that will draw the line, so the answer is
about the right font rather than about Emacs's idea of it."
  (and svg-line-measure-fonts
       (seq-some #'display-graphic-p (frame-list))
       (ignore-errors
         (let ((image-scaling-factor 1.0))
           (image-size (create-image svg 'svg t) t)))))

(defun svg-line--probe-extent (font glyph size body)
  "Extent from the SVG origin of GLYPH in FONT at SIZE, wrapped in BODY.
BODY is a format string taking the `<text>' element."
  (svg-line--intrinsic-size
   (format "<svg xmlns=\"http://www.w3.org/2000/svg\">%s</svg>"
           (format body
                   (format "<text x=\"0\" y=\"0\" font-family=\"%s\" font-size=\"%d\">%s</text>"
                           (svg-line--xml-escape font) size
                           (svg-line--xml-escape glyph))))))

(defun svg-line-glyph-ink (glyph font)
  "Ink box of GLYPH drawn in FONT, as fractions of the font size.

Returns (WIDTH HEIGHT LEFT TOP): the painted box's size, how far right of
the text origin it starts, and how far above the baseline it reaches.  Falls
back to `svg-line-icon-ink-fallback' when measuring is off or impossible.

Two renders, because of how the measurement works.  An SVG carrying no
width/height sizes itself to its content -- but Emacs reports that size as
the extent from the SVG ORIGIN to the content's far corner, not as the
content's own bounding box.  One render therefore pins only the ink's right
and bottom edges.  The second draws the glyph rotated a half turn about a
known point, which swaps those edges for the other two, and the four
numbers together give the box.

The alternative -- a probe whose baseline sits at y=0 -- does not work: a
glyph's ink is entirely ABOVE its baseline, so the content lies at negative
y and the renderer answers with a default size instead of a measurement."
  (or (svg-line--measure-ink glyph font) svg-line-icon-ink-fallback))

(defun svg-line--measure-ink (glyph font)
  "Measure GLYPH's ink box in FONT, or nil when it cannot be measured.
The measuring half of `svg-line-glyph-ink', kept separate because some
callers need to know that no measurement happened rather than receive a
stand-in: the icon fallback describes an ICON, and is the wrong answer for,
say, a cap height."
  (let ((key (cons glyph font)))
    (or (gethash key svg-line--ink-cache)
        (let* ((sz svg-line--ink-probe-size)
               (k (* 2 sz))             ; far enough that the turned glyph is
                                        ; wholly in positive coordinates
               (plain (svg-line--probe-extent
                       font glyph sz (format "<g transform=\"translate(0,%d)\">%%s</g>" sz)))
               (turned (and plain
                            (svg-line--probe-extent
                             font glyph sz
                             (format "<g transform=\"translate(%d,%d) rotate(180)\">%%s</g>" k k)))))
          (when (and plain turned (> (car plain) 0) (> (cdr plain) 0)
                     (> (car turned) 0) (> (cdr turned) 0))
            (let* ((left (- k (car turned)))              ; ink's left bearing
                   (top  (- (cdr turned) k))              ; ink's top over the baseline
                   (w    (- (car plain) left))            ; right edge back to left
                   (h    (+ (- (cdr plain) sz) top)))     ; bottom under baseline, plus top
              (when (and (> w 0) (> h 0))
                (puthash key (list (/ w (float sz)) (/ h (float sz))
                                   (/ left (float sz)) (/ top (float sz)))
                         svg-line--ink-cache))))))))

(defcustom svg-line-correct-tracking t
  "Whether to correct a renderer that advances a font wrongly.

librsvg reads some fonts' advance widths incorrectly while drawing their
outlines correctly.  Monaspace is one: its `hmtx' says a character is 0.62
em wide and its cap height 0.73 em, which is what Emacs renders and what the
font's own tables say -- but librsvg advances 0.7775 em, a quarter too far,
while sizing the glyphs right.  The text then reads correctly shaped but
conspicuously loose, and visibly wider than the same font in a buffer.

With this on, the gap is closed with negative `letter-spacing', which moves
the glyphs back onto the font's true pitch WITHOUT distorting them -- the
outlines were never wrong, only the spacing between them.  Layout then uses
the true advance, so a line takes the width the font actually asks for.

nil draws whatever the renderer does, which is the honest thing if you
suspect the correction of doing harm."
  :type 'boolean)

(defun svg-line-font-advance-native (font)
  "Advance of one character in FONT as the FONT ITSELF declares it, or nil.

Read from the font through Emacs, which -- unlike librsvg for some
families -- agrees with the `hmtx' table.  Measured at a large size so that
rounding the advance to whole pixels is noise."
  (or (gethash font svg-line--native-cache)
      (and (seq-some #'display-graphic-p (frame-list))
           (ignore-errors
             (let* ((size 400)
                    (entity (find-font (font-spec :family font :size size)))
                    (obj (and entity (open-font entity size)))
                    (glyph (and obj (aref (font-get-glyphs obj 0 1 "M") 0)))
                    (adv (and glyph (aref glyph 4))))
               (when (and adv (> adv 0))
                 (puthash font (/ adv (float size)) svg-line--native-cache)))))))

(defun svg-line-font-advance (font)
  "Advance of one character in FONT, as a fraction of the font size, or nil.
The advance the text will EFFECTIVELY have: the font's own when the renderer
is being corrected (`svg-line-correct-tracking'), otherwise the renderer's."
  (or (and svg-line-correct-tracking (svg-line-font-advance-native font))
      (svg-line-font-advance-rendered font)))

(defun svg-line-tracking-ratio (font)
  "Letter-spacing FONT needs to draw at its true pitch.
As a fraction of the font size; 0 when no correction applies."
  (or (and svg-line-correct-tracking
           (let ((native (svg-line-font-advance-native font))
                 (drawn (svg-line-font-advance-rendered font)))
             (and native drawn (- native drawn))))
      0))

(defun svg-line-font-advance-rendered (font)
  "Advance of one character in FONT, as a fraction of the font size, or nil.

Measured through librsvg, so it is the width the text will actually be drawn
at rather than the width Emacs would draw it at -- the two disagree, and for
some families badly (a font whose CFF `FontMatrix' contradicts its `head'
units-per-em is read differently by FreeType and by the platform's own
shaper).  Two lengths are rendered and subtracted, which cancels both the
glyph's side bearings and the origin offset in the reported extent.

Used to lay out a run that names its own `:font'.  nil when measuring is off
or unavailable, in which case the caller keeps the line's own advance."
  (or (gethash font svg-line--advance-cache)
      (let* ((sz svg-line--ink-probe-size)
             (body (format "<g transform=\"translate(0,%d)\">%%s</g>" sz))
             (short (svg-line--probe-extent font (make-string 4 ?M) sz body))
             (long  (and short (svg-line--probe-extent font (make-string 24 ?M) sz body)))
             (ratio (and long (/ (- (car long) (car short)) 20.0 sz))))
        (when (and ratio (> ratio 0))
          (puthash font ratio svg-line--advance-cache)))))

(defcustom svg-line-normalise-font-size 'cap
  "How `:font-size' is made to mean the same size in every family.

A font size names an em, and families fill their em to very different
degrees: at the same nominal size Monaspace's capitals stand 18% taller than
Terminess's, and each character takes 24% more width.  So a bar retuned from
one family to another comes out looking nothing like it did.

There are two defensible things to hold constant, and they disagree:

  `cap' (or t)  match the height of the CAPITALS.  Preserves legibility;
                a wide family then takes more room than it used to.
  `advance'     match the WIDTH of a character.  Preserves the footprint --
                what fits on the line -- at the cost of smaller type.

A float in [0, 1] blends the two, 0 being `cap' and 1 being `advance'.  nil
takes the size literally, which is what SVG means by it and what this
package did before it could measure.

`cap' is the default because the two now nearly agree, and they do so only
because of `svg-line-correct-tracking'.  Uncorrected, librsvg walks
Monaspace a quarter wider than the font asks, the bases disagree by 35%, and
no single choice looks right -- which is what the blend was for.  With the
renderer corrected the width cost of matching cap heights is about 7%, and
matching the thing a reader actually judges size by wins."
  :type '(choice (const :tag "Match cap height" cap)
                 (const :tag "Match advance" advance)
                 (float :tag "Blend (0 = cap, 1 = advance)")
                 (const :tag "Literal" nil)))

(defconst svg-line-cap-height-reference 0.6167
  "Cap height, as a fraction of the em, that `:font-size' is normalised to.
Terminess's, measured -- so a Terminess line is scaled by exactly 1.0 and
does not move.  Raise it to make every family read larger.")

(defconst svg-line-advance-reference 0.5
  "Character advance, as a fraction of the em, that `:font-size' is normalised to.
Terminess's, measured, for the same reason as `svg-line-cap-height-reference':
together they make the reference family the fixed point, so a line already
tuned in it is not disturbed by normalising against it.")

(defun svg-line-cap-height (font)
  "Ink height of a capital in FONT, as a fraction of the font size, or nil."
  (nth 1 (svg-line--measure-ink "M" font)))

(defun svg-line--font-size-factor (font)
  "Scale making FONT draw at the reference size.
Per `svg-line-normalise-font-size': matching cap height, matching advance,
or a blend.  1.0 when normalising is off, or when the measurement the chosen
basis needs is unavailable -- an unmeasurable font is left alone rather than
guessed at."
  (let* ((mode svg-line-normalise-font-size)
         (weight (cond ((null mode) nil)
                       ((eq mode 'advance) 1.0)
                       ((memq mode '(cap t)) 0.0)
                       ((numberp mode) (max 0.0 (min 1.0 (float mode))))
                       (t 0.0))))
    (if (null weight)
        1.0
      (let* ((cap (and (< weight 1.0) (svg-line-cap-height font)))
             (adv (and (> weight 0.0) (svg-line-font-advance font)))
             (fc (and cap (> cap 0.01) (/ svg-line-cap-height-reference cap)))
             (fa (and adv (> adv 0.01) (/ svg-line-advance-reference adv))))
        (cond ((and fc fa) (+ (* (- 1.0 weight) fc) (* weight fa)))
              ;; one basis missing: fall back to whichever measured, rather
              ;; than to 1.0, which would leave the size unnormalised
              (fc fc)
              (fa fa)
              (t 1.0))))))

(defun svg-line--font-size-for (font nominal)
  "NOMINAL font size adjusted so FONT draws at the reference cap height."
  (max 1 (round (* nominal (svg-line--font-size-factor font)))))

(defun svg-line--relative-size (base other fz)
  "FZ rescaled so OTHER draws at the same cap height BASE does at FZ.
Keeps a run that names its own family optically level with the line around
it, however differently that family fills its em."
  (let ((bc (svg-line-cap-height base))
        (oc (svg-line-cap-height other)))
    (if (and bc oc (> oc 0.01)) (max 1 (round (* fz (/ bc oc)))) fz)))

(defun svg-line--run-face (run font fz char-advance)
  "Return (FAMILY SIZE ADVANCE) to draw RUN with.
The line's own FONT, FZ and CHAR-ADVANCE unless the run names a `:font',
in which case that family, sized to match optically and advanced at its own
rate."
  (let ((rf (and (eq (car run) :seg) (plist-get (nth 2 run) :font))))
    (if (or (null rf) (equal rf font))
        (list font fz char-advance)
      (let* ((sfz (svg-line--relative-size font rf fz))
             (adv (svg-line-font-advance rf)))
        (list rf sfz (if adv (* sfz adv) char-advance))))))

(defun svg-line--run-advance (run font char-advance fz)
  "Per-character advance for RUN on a line of FONT at FZ, else CHAR-ADVANCE.

A run that names its own `:font' is drawn in that family and so advances at
that family's rate, not the line's.  Measured when it can be
\(`svg-line-font-advance'); otherwise the line's CHAR-ADVANCE stands in,
which is exactly right for the common case of mixing metrically compatible
cuts of one superfamily and merely approximate otherwise."
  (nth 2 (svg-line--run-face run font fz char-advance)))

(defun svg-line--icon-size-factor (font)
  "Correction making `:icon-scale' mean the same in FONT as in the reference.

`:icon-scale' has always meant \"how much of the bar height the icon's INK
should fill\" -- that is why values above 1 are normal, the em being mostly
empty around a Nerd glyph.  It could only mean that for one font, though,
while the ink fraction was a constant: Terminess inks 0.46 of its em and
Monaspace 0.57, so the same scale draws a masthead a fifth larger in the
latter, overflowing its cell and painting over the row icons beside it.

Dividing the reference ink by the measured one takes that out.  A font
whose ink matches `svg-line-icon-ink-fallback' -- Terminess, which these
numbers were taken from -- gets exactly 1.0 and does not move."
  (let ((h (nth 1 (svg-line-glyph-ink svg-line-icon-ink-probe font))))
    (if (> h 0.01) (/ (nth 1 svg-line-icon-ink-fallback) h) 1.0)))

(defun svg-line--xml-escape (text)
  "Escape the XML metacharacters &, <, > and \" in TEXT for SVG.
A private escaper so the package does not depend on svg.el internals
\(e.g. `svg--encode-text', whose stability is not guaranteed).

The double quote is escaped as well because the font probes format their
values into ATTRIBUTES, which are quote-delimited -- a family name carrying
a quote would otherwise close the attribute early and the rest of the name
would be parsed as markup.  It is valid in text content too, and renders
identically there, so one escaper serves both."
  (replace-regexp-in-string
   "[&<>\"]"
   (lambda (m) (pcase m ("&" "&amp;") ("<" "&lt;") (">" "&gt;") ("\"" "&quot;")))
   text t t))

(defun svg-line--add-text (svg str &rest props)
  "Append a `<text>' for STR to SVG, enlarging Nerd-Font glyph runs.
PROPS keywords: :x :y :font :font-size :fill :weight :anchor.  Glyph runs
are wrapped in a larger `<tspan>' (per `svg-line-glyph-scale') with a small
baseline shift so they stay vertically centred; librsvg flows the tspans,
so no manual positioning is needed."
  (let* ((fz (plist-get props :font-size))
         (scale svg-line-glyph-scale)
         (big (round (* fz scale)))
         (shift (round (/ (* fz (- scale 1.0)) 2.0)))
         ;; negative when the renderer advances this family too far; it pulls
         ;; the glyphs back onto the font's own pitch without touching their
         ;; shapes (see `svg-line-correct-tracking')
         (tr (svg-line-tracking-ratio (plist-get props :font)))
         (attrs (list (cons 'x (plist-get props :x))
                      (cons 'y (plist-get props :y))
                      (cons 'font-family (plist-get props :font))
                      (cons 'font-size fz)
                      (cons 'fill (plist-get props :fill))
                      ;; keep inter-tspan spaces: SVG otherwise trims leading
                      ;; whitespace at a tspan boundary, swallowing the space
                      ;; after an enlarged glyph
                      (cons 'xml:space "preserve"))))
    (unless (zerop tr) (push (cons 'letter-spacing (* tr fz)) attrs))
    (when (plist-get props :weight) (push (cons 'font-weight (plist-get props :weight)) attrs))
    (when (plist-get props :anchor) (push (cons 'text-anchor (plist-get props :anchor)) attrs))
    (let ((node (dom-node 'text (nreverse attrs))) (cur-dy 0) (prev-size nil))
      (dolist (run (svg-line--split-glyph-runs str))
        (let* ((glyphp (and (car run) (> scale 1.0)))
               (target (if glyphp shift 0))
               (dy (- target cur-dy))
               (size (if glyphp big fz))
               ;; letter-spacing reaches only the gaps BETWEEN characters, so
               ;; a tspan's own trailing gap keeps the renderer's error and
               ;; the next tspan starts that much too far right.  Pull it
               ;; back, or an icon would space the text after it apart -- and
               ;; a run of one character, which has no gaps at all, could not
               ;; be corrected in any other way.
               (dx (and prev-size (not (zerop tr)) (* tr prev-size))))
          (setq cur-dy target prev-size size)
          (dom-append-child
           node (dom-node 'tspan
                          (append (when glyphp (list (cons 'font-size big)))
                                  ;; the enlarged glyph needs the correction
                                  ;; scaled to the size it is drawn at
                                  (when (and glyphp (not (zerop tr)))
                                    (list (cons 'letter-spacing (* tr big))))
                                  (when dx (list (cons 'dx dx)))
                                  (unless (zerop dy) (list (cons 'dy dy))))
                          ;; encode <>& like `svg-text' does (svg-print emits
                          ;; text content verbatim, so escape it ourselves)
                          (svg-line--xml-escape (cdr run))))))
      (dom-append-child svg node))))

;;;; Value resolution
;; ----------------------------------------------------------------
;; Every styling option may be a literal or a zero-arg function; the
;; function form is what makes theme-dependent colours possible.

(defun svg-line--val (v)
  "Resolve V: call it when it is a function, else return it."
  (if (functionp v) (funcall v) v))

(defun svg-line--color (c)
  "Normalise colour C to a 6-digit \"#RRGGBB\" string for SVG.
Emacs colours are often names or the 12-digit \"#RRRRGGGGBBBB\" form,
which SVG/librsvg does not accept.  Hex forms are converted directly
\(display-independent); names are resolved via `color.el'.  A value that
is already 6-digit hex passes through; nil returns nil."
  (cond
   ((null c) nil)
   ((not (stringp c)) c)
   ((string-match-p "\\`#[0-9a-fA-F]\\{6\\}\\'" c) c)
   ;; 12-digit #RRRRGGGGBBBB -> high byte of each 16-bit channel
   ((string-match "\\`#\\([0-9a-fA-F]\\{4\\}\\)\\([0-9a-fA-F]\\{4\\}\\)\\([0-9a-fA-F]\\{4\\}\\)\\'" c)
    (concat "#" (substring (match-string 1 c) 0 2)
            (substring (match-string 2 c) 0 2)
            (substring (match-string 3 c) 0 2)))
   ;; 3-digit #RGB -> #RRGGBB
   ((string-match "\\`#\\([0-9a-fA-F]\\)\\([0-9a-fA-F]\\)\\([0-9a-fA-F]\\)\\'" c)
    (concat "#" (make-string 2 (aref (match-string 1 c) 0))
            (make-string 2 (aref (match-string 2 c) 0))
            (make-string 2 (aref (match-string 3 c) 0))))
   ;; named colour (or anything else): resolve, else pass through unchanged
   (t (let ((rgb (ignore-errors (color-name-to-rgb c))))
        (if rgb (apply #'color-rgb-to-hex (append rgb '(2))) c)))))

;;;; Segment rendering
;; ----------------------------------------------------------------
;; A "segment" is a string (used verbatim), a zero-arg function (called,
;; result normalised), a BOUND VARIABLE symbol (its value is used, like a
;; `mode-line-format' construct), or anything else (contributes nothing).
;; A function/variable value may be a string, a tab-bar menu-item
;; `(KEY menu-item STR . _)', a list of such, or nil.  Each segment is
;; evaluated exactly once.

(defun svg-line--menu-item-string (item)
  "Return the display string of a tab-bar menu-item ITEM (its third element).
A plain string is returned verbatim (without round-tripping through
`format-mode-line', which yields \"\" in batch); a mode-line construct is
formatted."
  (let ((s (nth 2 item)))
    (cond ((stringp s) (substring-no-properties s))
          (s (format-mode-line s))
          (t ""))))

(defun svg-line--item->string (r)
  "Normalise a segment result R to a plain string."
  (cond
   ((null r) "")
   ((stringp r) (substring-no-properties r))
   ((and (consp r) (eq (nth 1 r) 'menu-item)) (svg-line--menu-item-string r))
   ((and (consp r) (consp (car r)))
    (mapconcat (lambda (it)
                 (if (and (consp it) (eq (nth 1 it) 'menu-item))
                     (svg-line--menu-item-string it) ""))
               r ""))
   (t (format "%s" r))))

(defun svg-line-render-segments (segments)
  "Render SEGMENTS to one plain string, each evaluated exactly once.
This is the text-only path: it flattens segments to a string and does
not interpret `:svg-bar'/`:svg-pie' progress tokens.  The engine itself
renders through `svg-line--render-runs' (which does handle those tokens);
this function is provided for callers that just want the flattened text."
  (mapconcat (lambda (s)
               (cond ((stringp s) s)
                     ((functionp s) (svg-line--item->string (funcall s)))
                     ((and (symbolp s) (boundp s)) (svg-line--item->string (symbol-value s)))
                     (t "")))
             segments ""))

;;;; Runs -- text interleaved with progress bars / pies
;; ----------------------------------------------------------------
;; A `lines' side may mix text with progress bars and pies.  A segment
;; value (or literal) of (:svg-bar FRACTION WIDTH FILL BG) or
;; (:svg-pie FRACTION FILL BG) becomes a non-text run; everything else
;; contributes text.  `svg-line--render-runs' lowers a segment list to a
;; run list -- (:text STR), (:bar FRAC W FILL BG), (:pie FRAC FILL BG) --
;; coalescing adjacent text, each segment evaluated exactly once.
;; (Icons are drawn as Nerd-Font glyphs in the text itself, so they need
;; no run of their own; see `svg-line--add-text' / `svg-line-glyph-scale'.)

(defun svg-line--render-runs (segments)
  "Lower SEGMENTS to a list of runs, each segment evaluated exactly once.
Run forms: (:text STR), (:bar FRACTION WIDTH FILL BG), (:pie FRACTION FILL BG),
or (:seg STR PLIST) for an interactive text run (see `svg-line-seg').
A segment value of (:svg-segs ITEM ...) is spliced (each ITEM processed as a
sub-value), so one segment function can emit several interactive runs (e.g.
per-crumb breadcrumbs)."
  (let ((runs '()) (buf ""))
    (cl-labels
        ((flush () (when (> (length buf) 0)
                     (push (list :text buf) runs) (setq buf "")))
         (add (v)
           (cond
            ((null v))
            ((and (consp v) (eq (car v) :svg-bar)) (flush) (push (cons :bar (cdr v)) runs))
            ((and (consp v) (eq (car v) :svg-pie)) (flush) (push (cons :pie (cdr v)) runs))
            ((and (consp v) (eq (car v) :svg-seg))
             (let ((txt (svg-line--item->string (cadr v))))
               (when (> (length txt) 0)
                 (flush) (push (list :seg txt (cddr v)) runs))))
            ((and (consp v) (eq (car v) :svg-segs))
             (dolist (c (cdr v)) (add c)))
            (t (setq buf (concat buf (svg-line--item->string v)))))))
      (dolist (s segments)
        (let ((v (cond ((stringp s) s)
                       ((and (consp s) (memq (car s) '(:svg-bar :svg-pie :svg-seg :svg-segs))) s)
                       ((functionp s) (funcall s))
                       ((and (symbolp s) (boundp s)) (symbol-value s))
                       (t nil))))
          (add v)))
      (flush))
    (nreverse runs)))

(defun svg-line--runs-all-text-p (runs)
  "Non-nil if RUNS are entirely text (so the side can use exact text anchoring)."
  (cl-every (lambda (r) (eq (car r) :text)) runs))

(defun svg-line--runs-ltrim (runs)
  "Drop leading blank `:text' RUNS and left-trim the first text run.
Mirrors the exact-anchor path's leading trim so run-laid content (with inline
pies/bars/segments) still starts flush at the left inset."
  (while (and runs (eq (caar runs) :text) (string-blank-p (nth 1 (car runs))))
    (setq runs (cdr runs)))
  (if (and runs (eq (caar runs) :text))
      (cons (list :text (string-trim-left (nth 1 (car runs)))) (cdr runs))
    runs))

(defun svg-line--runs-rtrim (runs)
  "Drop trailing blank `:text' RUNS and right-trim the last text run.
Mirrors the exact-anchor path's trailing trim so a run-laid right side stays
flush at the right edge instead of leaving a gap from empty trailing segments."
  (setq runs (nreverse runs))
  (while (and runs (eq (caar runs) :text) (string-blank-p (nth 1 (car runs))))
    (setq runs (cdr runs)))
  (when (and runs (eq (caar runs) :text))
    (setq runs (cons (list :text (string-trim-right (nth 1 (car runs)))) (cdr runs))))
  (nreverse runs))

(defun svg-line--run-width (run font char-advance fz)
  "Advance width in pixels of a single RUN on a line whose family is FONT.
Text advances by CHAR-ADVANCE per character -- icon glyphs by more, see
`svg-line--string-width', and a run naming its own `:font' by that family's
rate, see `svg-line--run-advance' -- and bars and pies derive their size from
the font size FZ."
  (pcase (car run)
    (:text (svg-line--string-width (nth 1 run) char-advance fz font))
    (:seg  (cl-destructuring-bind (sfont sfz sadv)
               (svg-line--run-face run font fz char-advance)
             (svg-line--string-width (nth 1 run) sadv sfz sfont)))
    (:pie  (+ (round (* fz 0.76)) (round (* 0.3 fz))))   ; diameter + gap
    (:bar  (+ (nth 2 run) (round (* 0.3 fz))))
    (_ 0)))

(defun svg-line--runs-width (runs font char-advance fz)
  "Total advance width in pixels of RUNS (for right alignment).
FONT, CHAR-ADVANCE and font size FZ are passed through to
`svg-line--run-width'."
  (apply #'+ (mapcar (lambda (r) (svg-line--run-width r font char-advance fz)) runs)))

;;;; Image builders (public, pure: data in, svg object out)
;; ----------------------------------------------------------------

(defvar svg-line--seg-acc nil
  "Accumulator for interactive-run placements in the current `lines' render.
Each entry is (X TOP W (TEXT . PLIST)), pushed by `svg-line--draw-runs' and
harvested by `svg-line-image'.  A side channel so `svg-line--draw-runs' keeps
its simple return contract (the ending x).")

(defvar svg-line--lines-placements nil
  "Interactive-segment placements from the last `svg-line-image' call.
Each entry is (X TOP W (TEXT . PLIST)); a side channel like
`svg-line--wrap-placements'.")
(defvar svg-line--lines-lh 0
  "Row height from the last `svg-line-image' call.  Side channel.")

(defvar svg-line--seg-shape 'round
  "Shape of a segment's background.  Bound by `svg-line-image\' per line.
A side channel for the same reason as `svg-line--seg-acc\': it is a property
of the whole image, and threading it through `svg-line--draw-runs\' and its
three call sites would say nothing the binding does not.")

(defvar svg-line--seg-slant nil
  "Pixels of angle on a shaped segment background.  Bound by `svg-line-image\'.")

(defun svg-line--seg-box (svg x top cw lh fill)
  "Paint a CW-by-LH segment background at X,TOP on SVG in FILL.

Shaped by `svg-line--seg-shape\':

  `round\'   a rounded pill (the default, and what every bar drew before
             the others existed)
  `square\'  the same rectangle, corners left alone
  `arrow\'   a powerline chevron: square left edge, right edge drawn to a
             point `svg-line--seg-slant\' pixels deep
  `slant\'   a parallelogram, both edges leaning the same way

The angle is cut INTO the segment rather than added on: a chip\'s label
carries its own leading and trailing space, so the point eats padding that
was already there and the shape never reaches past CW into whatever is
drawn next.  That is what lets a shaped chip sit in a line laid out for
rectangles without shifting anything."
  (let* ((s (max 0 (min (or svg-line--seg-slant 0) (max 0 (1- cw)))))
         (r (+ x cw))
         (b (+ top lh))
         (m (+ top (/ lh 2))))
    (pcase svg-line--seg-shape
      ('square (svg-rectangle svg x top cw lh :fill fill))
      ('arrow (svg-polygon svg (list (cons x top) (cons (- r s) top)
                                     (cons r m)
                                     (cons (- r s) b) (cons x b))
                           :fill fill))
      ('slant (svg-polygon svg (list (cons (+ x s) top) (cons r top)
                                     (cons (- r s) b) (cons x b))
                           :fill fill))
      (_ (svg-rectangle svg x top cw lh :fill fill :rx 3)))))

(defun svg-line--draw-runs (svg runs x top fz lh font char-advance foreground
                                &optional hovered hover-color)
  "Draw RUNS left-to-right in SVG starting at X (row top at TOP).
Text advances by CHAR-ADVANCE per character; bars and pies by their own
width.  FOREGROUND is the fallback fill.  An interactive (:seg STR PLIST)
run is drawn like text, gets a HOVER-COLOR box when its `:id' equals HOVERED,
and its placement (X TOP WIDTH (STR . PLIST)) is pushed onto
`svg-line--seg-acc' for click/hover hit-testing.  Returns the ending x."
  (dolist (run runs)
    (pcase (car run)
      (:text (let ((str (nth 1 run)))
               (when (> (length str) 0)
                 (svg-line--add-text svg str :x x :y (+ top fz)
                                     :font font :font-size fz :fill foreground))))
      (:seg  (let* ((str (nth 1 run))
                    (plist (nth 2 run))
                    ;; a segment may name its own family -- that is what lets
                    ;; one line mix faces, e.g. a handwriting cut for one chip
                    ;; among mechanical ones.  It is sized to match the line
                    ;; optically and advanced at its own rate.
                    (face (svg-line--run-face run font fz char-advance))
                    (sfont (nth 0 face))
                    (sfz (nth 1 face))
                    (sadv (nth 2 face))
                    (cw (svg-line--string-width str sadv sfz sfont))
                    (id (plist-get plist :id))
                    (hov (and hover-color hovered id (equal id hovered)))
                    (col (plist-get plist :color))
                    (face (plist-get plist :face))
                    (bg (plist-get plist :bg))        ; persistent background pill
                    (weight (plist-get plist :weight)) ; e.g. `bold'
                    (fill (cond (col (svg-line--color col))
                                (face (svg-line--color
                                       (face-foreground face nil 'default)))
                                (t foreground))))
               (when bg
                 (svg-line--seg-box svg x top cw lh (svg-line--color bg)))
               (when hov
                 (svg-line--seg-box svg x top cw lh hover-color))
               (when (> (length str) 0)
                 (svg-line--add-text svg str :x x :y (+ top fz)
                                     :font sfont :font-size sfz :fill fill :weight weight))
               (push (list x top cw (cons str plist)) svg-line--seg-acc)))
      (:pie  (let* ((frac (max 0.0 (min 1.0 (float (nth 1 run)))))
                    (fill (svg-line--color (or (nth 2 run) foreground)))
                    (bg   (svg-line--color (or (nth 3 run) "#d4dcea")))
                    (r  (* fz 0.38))
                    ;; leading-only gap: the pie's right edge lands at the
                    ;; run end, so a rightmost pie sits flush at the margin.
                    (cx (+ x (round (* 0.3 fz)) r))
                    (cy (+ top (/ lh 2.0))))
               (svg-line--draw-pie-at svg cx cy r frac fill bg)))
      (:bar  (let* ((frac (max 0.0 (min 1.0 (float (nth 1 run)))))
                    (bw (nth 2 run))
                    (fill (or (nth 3 run) foreground))
                    (bg (nth 4 run))
                    (bh (max 3 (round (* fz 0.5))))
                    (by (+ top (max 0 (/ (- lh bh) 2)))))
               (when bg (svg-rectangle svg x by bw bh :fill (svg-line--color bg) :rx 2))
               (svg-rectangle svg x by (max 1 (round (* bw frac))) bh
                              :fill (svg-line--color fill) :rx 2))))
    (setq x (+ x (svg-line--run-width run font char-advance fz))))
  x)

(defun svg-line--draw-pie-at (svg cx cy r frac fill bg)
  "Draw a progress pie on SVG centred at CX,CY radius R for FRAC in [0,1].
FILL and BG are already-resolved colours."
  (svg-circle svg cx cy r :fill bg)
  (if (>= frac 0.999)
      (svg-circle svg cx cy r :fill fill)
    (when (> frac 0.001)
      (let* ((theta (* 2 float-pi frac))
             (ex (+ cx (* r (sin theta))))
             (ey (- cy (* r (cos theta))))
             (large (if (> frac 0.5) 1 0)))
        (dom-append-child
         svg (dom-node 'path
                       (list (cons 'd (format "M %g %g L %g %g A %g %g 0 %d 1 %g %g Z"
                                              cx cy cx (- cy r) r r large ex ey))
                             (cons 'fill fill))))))))

(defcustom svg-line-clock-ticks 4
  "How many tick marks a `:clock' span wears: 12, 4, or 0 for none.
Twelve is a lot of ink at bar sizes -- the marks crowd the rim and the face
reads as a texture rather than a clock.  Four keeps the orientation."
  :type '(choice (const :tag "None" 0) (const :tag "Quarters" 4)
                 (const :tag "Hours" 12)))

(defun svg-line--draw-clock (svg cx cy r color &optional accent)
  "Draw an analog clock face on SVG centred at CX,CY radius R, showing now.
COLOR is the rim/ticks/hour-hand colour; ACCENT (or COLOR) the minute hand.

Everything is in floating point.  Rounding the strokes to whole pixels is
what made this read as blocky at bar sizes: at r=20 the rim, the ticks and
both hands all collapsed onto one or two pixels and the face lost its
hierarchy entirely.  Sub-pixel strokes antialias instead, so the weights
stay distinct however small the bar -- which also means the same code looks
right under a font that makes the bar 51px tall and one that makes it 57.

The hands carry a short tail past the pivot, as a real watch does; it is
what stops them reading as bars radiating from a dot."
  (let* ((tm (decode-time))
         (mn (decoded-time-minute tm))
         (hr (mod (decoded-time-hour tm) 12))
         (ma (* (/ mn 60.0) 2 float-pi))
         (ha (* (/ (+ hr (/ mn 60.0)) 12.0) 2 float-pi))
         (col (svg-line--color color))
         (acc (svg-line--color (or accent color)))
         (tail (* r 0.16)))
    (cl-flet ((hand (ang len w c)
                (svg-line svg
                          (- cx (* tail (sin ang))) (+ cy (* tail (cos ang)))
                          (+ cx (* len (sin ang)))  (- cy (* len (cos ang)))
                          :stroke c :stroke-width w :stroke-linecap "round")))
      ;; a hairline rim, not a ring: at this size a heavy circle is the whole
      ;; picture and the hands disappear inside it
      (svg-circle svg cx cy r :fill "none" :stroke col
                  :stroke-width (* r 0.045) :stroke-opacity 0.55)
      (when (> svg-line-clock-ticks 0)
        (dotimes (i svg-line-clock-ticks)
          (let ((a (* (/ (float i) svg-line-clock-ticks) 2 float-pi))
                (r1 (* r 0.74)) (r2 (* r 0.88)))
            (svg-line svg (+ cx (* r1 (sin a))) (- cy (* r1 (cos a)))
                      (+ cx (* r2 (sin a))) (- cy (* r2 (cos a)))
                      :stroke col :stroke-width (* r 0.05)
                      :stroke-linecap "round" :stroke-opacity 0.75))))
      (hand ha (* r 0.46) (* r 0.115) col)
      (hand ma (* r 0.74) (* r 0.075) acc)
      (svg-circle svg cx cy (* r 0.06) :fill acc))))

;;;###autoload
(defun svg-line-span-metrics (font font-size line-pad rows)
  "Return (HEIGHT . RADIUS) for a row-spanning overlay covering ROWS rows.

FONT and FONT-SIZE are the line's, so the normalised drawn size is what gets
used (see `svg-line-normalise-font-size'); LINE-PAD is its per-row padding.

Exported because an edge-aligned `:clock' or `:pie' reserves no room for
itself -- the caller has to widen the line's `:right-margin' (or `:pad') to
keep the rows clear of it, and would otherwise have to duplicate this
geometry and keep the copy in step."
  (let* ((fz (svg-line--font-size-for font font-size))
         (h (* rows (+ fz line-pad))))
    (cons h (max 3 (round (* (/ h 2.0) 0.86))))))

(defun svg-line--span-cx (align gap r width pad)
  "Centre x for a row-spanning overlay of radius R on a WIDTH-wide image.
ALIGN is `left', `right' or nil/`center'; GAP insets it from that edge, the
same way it does for an `:image' span.  PAD is the image's left inset."
  (pcase align
    ('left  (+ pad (or gap 0) r))
    ('right (- width (or gap 0) r))
    (_ (/ width 2))))

(defun svg-line--svg-intrinsic-size (svg-string)
  "Parse (WIDTH . HEIGHT) in px from an SVG STRING's root element.
Defaults each dimension to 1 if absent."
  (cons (if (string-match "\\bwidth=\"\\([0-9.]+\\)" svg-string)
            (max 1 (round (string-to-number (match-string 1 svg-string)))) 1)
        (if (string-match "\\bheight=\"\\([0-9.]+\\)" svg-string)
            (max 1 (round (string-to-number (match-string 1 svg-string)))) 1)))

(defun svg-line--embed-image (svg data x y w h)
  "Embed SVG markup DATA as a base64 data-URI <image> on SVG at X,Y sized W*H.
Rasterised by librsvg at W*H, so it can soften under further scaling -- prefer
`svg-line--splice-svg' for SVG payloads, which stays vector and renders sharp."
  (dom-append-child
   svg (dom-node 'image
                 (list (cons 'x x) (cons 'y y) (cons 'width w) (cons 'height h)
                       (cons 'href (concat "data:image/svg+xml;base64,"
                                           (base64-encode-string
                                            (encode-coding-string data 'utf-8) t)))))))

(defun svg-line--attr-num (v)
  "Coerce an SVG attribute V (number or string) to a number; default 1."
  (cond ((numberp v) v) ((stringp v) (string-to-number v)) (t 1)))

(defun svg-line--parse-svg (s)
  "Parse SVG string S into an svg.el DOM root node, or nil."
  (and (stringp s)
       (ignore-errors
         (with-temp-buffer (insert s)
           (libxml-parse-xml-region (point-min) (point-max))))))

(defun svg-line--splice-svg (svg dom x y scale)
  "Splice DOM's children into SVG under a translate(X,Y) scale(SCALE) group.
Keeps everything vector (renders sharp at device resolution), unlike a
rasterised <image>."
  (let ((g (dom-node 'g (list (cons 'transform
                                    (format "translate(%d,%d) scale(%g)" x y scale))))))
    ;; Deep-copy each child: DOM may be a cached/shared node (e.g. a daily
    ;; date widget reused across renders), and splicing its child cons cells
    ;; into another tree aliases them.  copy-tree keeps the splice fully
    ;; independent of the caller's DOM.
    (dolist (c (dom-children dom)) (dom-append-child g (copy-tree c)))
    (dom-append-child svg g)))

(defconst svg-line-rule-supported t
  "Non-nil in versions whose layouts accept `:rule'.
A caller that wants an inset rule but must still work against an older
svg-line -- falling back to the `:overline' of the face the image sits on,
which is full width -- can test this rather than the version string.")

;;;###autoload
(cl-defun svg-line-image (rows &key
                               (width 100)
                               (font (or svg-line-font (face-attribute 'default :family nil t)))
                               (font-size svg-line-font-size)
                               (line-pad svg-line-line-pad)
                               (pad 0)
                               (pad-y 0)
                               (margin 0)
                               (margin-y 0)
                               (right-margin 0)
                               (rule nil)
                               (rule-height 1)
                               (rule-margin nil)
                               (seg-shape 'round)
                               (seg-slant nil)
                               (char-advance svg-line-char-advance)
                               (char-advance-ratio svg-line-char-advance-ratio)
                               (foreground "#000000")
                               (background nil)
                               (hovered nil)
                               (hover-color nil)
                               (icon nil)
                               (icon-color nil)
                               (icon-width nil)
                               (icon-scale 0.74)
                               (spans nil))
  "Build a `lines'-layout SVG from ROWS.
Each ROW is either a cons (LEFT . RIGHT) -- left- and right-aligned content --
or a vector [LEFT CENTER RIGHT] which adds horizontally-centred content.
Each of LEFT, CENTER and RIGHT is either:
  - a STRING, drawn with exact font anchoring (flush-left at PAD, centred at
    WIDTH/2, or flush-right at WIDTH minus RIGHT-MARGIN); or
  - a list of RUNS, drawn with CHAR-ADVANCE spacing so it can carry inline
    pies, progress bars and interactive segments.  A run is (:text STR),
    (:pie FRACTION FILL BG), (:bar FRACTION PIXELWIDTH FILL BG) or
    (:seg STR PLIST); see `svg-line--render-runs'.  A `:seg' whose PLIST
    carries a `:font' is drawn -- and laid out -- in that family instead of
    FONT, so one line can mix faces.
FONT, FONT-SIZE, LINE-PAD, PAD, FOREGROUND and BACKGROUND set the text
family, size, per-row vertical padding, left inset and colours.  PAD-Y insets
the rows from the top AND bottom of the image, which LINE-PAD cannot do: that
one grows the space BELOW each row, so it separates rows and pads the bottom
but never the top.  PAD-Y is the vertical partner to PAD, and is either a
number (both ends) or a cons (TOP . BOTTOM) -- the asymmetric form is how a
bar gets clear space on the side facing its neighbour without an equal gap
on the far side.  BACKGROUND is painted only behind the ROWS, never behind
PAD-Y, so the inset reads as space between bars rather than as more bar.
An interactive (:seg ...) run whose `:id' equals HOVERED gets a HOVER-COLOR
box; the placements of all such runs are left in `svg-line--lines-placements'
\(with row height in `svg-line--lines-lh') for click/hover hit-testing.
ICON, when non-nil, is a (usually Nerd-Font) glyph drawn ONCE at the left
spanning the FULL image height (a multi-row \"masthead\" icon); ICON-COLOR
sets its fill, ICON-WIDTH the horizontal space it reserves (default: the image
height, i.e. square) and ICON-SCALE how much of that height the glyph's INK
should fill.  Ink, not em box: a Nerd glyph paints only about half of its em,
and how much exactly differs per family, so the glyph is measured
\(`svg-line-glyph-ink') and sized from that.  ICON-SCALE therefore means the
same thing in every font -- values above 1 are still normal, the em being
mostly empty.  The left-aligned content is inset past it.  Returns an svg
object."
  (let* ((foreground (svg-line--color foreground))
         (background (svg-line--color background))
         (hover-color (svg-line--color hover-color))
         ;; the NOMINAL size names an em, and families fill their em very
         ;; differently; normalise it so the drawn capitals are the same
         ;; height whatever the family (`svg-line-normalise-font-size')
         (fz (svg-line--font-size-for font font-size))
         (char-advance (svg-line--char-advance char-advance fz char-advance-ratio))
         (lh (+ fz line-pad))
         (rx (max 0 (- width margin right-margin)))
         ;; CH is the rows' own height; HEIGHT adds the vertical inset.  The
         ;; masthead icon is sized and centred on CH, not HEIGHT, so padding
         ;; the image does not inflate the icon along with it.
         (pad-y (svg-line--pad-y pad-y))
         (pad-top (car pad-y))
         (pad-bot (cdr pad-y))
         (margin-y (svg-line--pad-y margin-y))
         (mtop (car margin-y))
         (mbot (cdr margin-y))
         (ch (max 1 (* lh (length rows))))
         ;; BG-H is the painted block: the rows plus their PADDING.  MARGIN-Y
         ;; sits outside it and is never painted, so it reads as space between
         ;; this bar and its neighbour rather than as more bar.
         (bg-h (+ pad-top ch pad-bot))
         (height (max 1 (+ mtop bg-h mbot)))
         (rows-y (+ mtop pad-top))
         (x0 (+ margin pad))
         (ink (and icon (svg-line-glyph-ink icon font)))
         (isz0 (and icon (max 1 (round (* ch icon-scale
                                          (svg-line--icon-size-factor font))))))
         ;; Reserved icon width.  `square' reserves the full image height (a
         ;; square cell); an integer reserves that many pixels; otherwise
         ;; reserve only ~the ink width plus a small margin -- Nerd-Font icon
         ;; glyphs carry lots of empty em-box padding (ink is ~0.5 of the font
         ;; size), and to fill the height the glyph is scaled past it via
         ;; ICON-SCALE (the oversized em is clipped to the image).
         (iw (cond ((not icon) 0)
                   ((eq icon-width 'square) ch)
                   ((numberp icon-width) icon-width)
                   (t (+ (round (* isz0 (nth 0 ink))) (round (* fz 0.12))))))
         ;; Shrink the glyph if its ink would not fit the cell it was given.
         ;; The size above fills the cell's HEIGHT, which is all that ever
         ;; mattered while the icons were Terminess's -- those are as wide as
         ;; they are tall.  A family whose glyphs are wider than that reaches
         ;; past the cell's right edge and paints over the row content beside
         ;; it, and since both are drawn in the same ink the content does not
         ;; look overlapped so much as MISSING.
         (isz (and icon
                   (max 1 (min isz0
                               (floor (/ (float iw) (max 0.01 (nth 0 ink))))
                               (floor (/ (float ch) (max 0.01 (nth 1 ink))))))))
         (left-x0 (+ x0 iw))
         (svg (svg-create width height))
         (svg-line--seg-shape seg-shape)
         (svg-line--seg-slant (or seg-slant (round (* lh 0.3))))
         (svg-line--seg-acc nil))
    ;; The background stops at the padding rather than filling the whole
    ;; image: PAD-Y exists to put clear space between this bar and the one
    ;; next to it, and a rect drawn over that space would just make the bar
    ;; taller instead -- the bars would still butt together, only thicker.
    (when background
      (svg-rectangle svg margin mtop (max 1 (- width (* 2 margin))) bg-h
                     :fill background))
    ;; RULE: a hairline along the TOP of the image, inset from both edges.
    ;; Drawn here rather than left to the `:overline' of the face the image
    ;; sits on: redisplay paints a face attribute across the face's whole
    ;; extent, and for a window-width image that is the whole window, so an
    ;; overline can never be inset.  Inside the SVG it is inset like
    ;; everything else -- by MARGIN unless RULE-MARGIN overrides, so by
    ;; default it lines up with the background rect above rather than
    ;; reaching past it.  Drawn at y=0, i.e. OUTSIDE margin-y, because the
    ;; rule marks the edge of the WINDOW, not the edge of the painted bar.
    (when rule
      (let ((rm (or rule-margin margin)))
        (svg-rectangle svg rm 0 (max 1 (- width (* 2 rm)))
                       (max 1 rule-height) :fill rule)))
    ;; full-height masthead icon on the left, drawn once for the whole image.
    ;; The glyph's ink sits in the left ~half of its em box, so to centre the
    ;; ink within the reserved cell we shift the draw origin left by ~a quarter
    ;; of the em (clamped to PAD); for a tight cell this collapses to flush-left.
    (when icon
      ;; INK is (WIDTH HEIGHT LEFT TOP) as fractions of the em, measured from
      ;; this glyph in this font.  Its centre therefore sits LEFT + WIDTH/2
      ;; right of the origin and TOP - HEIGHT/2 above the baseline; offset by
      ;; those to centre the INK in the reserved cell rather than the mostly
      ;; empty em box around it.  These were constants -- 0.255 and 0.335,
      ;; which is Terminess measured by hand -- and so put every other font's
      ;; icon in the wrong place.
      ;;
      ;; The floor is on the INK's left edge, not on the glyph's origin.
      ;; Clamping the origin to PAD looks like the same thing and is not: a
      ;; glyph whose ink sits well right of its origin needs to be drawn from
      ;; further left than PAD to land centred, and stopping it at PAD instead
      ;; slides the whole ink right, out through the cell's other side and
      ;; over the row content beginning there.
      (let* ((svg-line-glyph-scale 1.0)  ; size the glyph explicitly, not via the run scale
             (icx (+ (nth 2 ink) (/ (nth 0 ink) 2.0)))
             (icy (- (nth 3 ink) (/ (nth 1 ink) 2.0)))
             (ix (max (- x0 (round (* isz (nth 2 ink))))
                      (- (+ x0 (/ iw 2)) (round (* isz icx))))))
        (svg-line--add-text svg icon
                            :x ix
                            :y (round (+ rows-y (/ ch 2.0) (* isz icy)))
                            :font font :font-size isz
                            :fill (svg-line--color (or icon-color foreground)))))
    (cl-loop for row in rows
             for i from 0
             for top = (+ rows-y (* lh i))
             for y = (+ top fz)
             for l = (if (vectorp row) (aref row 0) (car row))
             for c = (if (vectorp row) (aref row 1) nil)
             for r = (if (vectorp row) (aref row 2) (cdr row))
             do (progn
                  ;; LEFT: flush-left (past the masthead icon).  Trim leading
                  ;; whitespace so the visible content starts at LEFT-X0.
                  (cond
                   ((and (stringp l) (> (length (string-trim-left l)) 0))
                    (svg-line--add-text svg (string-trim-left l) :x left-x0 :y y
                                        :font font :font-size fz :fill foreground))
                   ((consp l)
                    (svg-line--draw-runs svg (svg-line--runs-ltrim l) left-x0 top fz lh
                                         font char-advance foreground
                                         hovered hover-color)))
                  ;; CENTER: centred on WIDTH/2.  Trim both sides so the visible
                  ;; content is what gets centred.
                  (cond
                   ((and (stringp c) (> (length (string-trim c)) 0))
                    (svg-line--add-text svg (string-trim c) :x (/ width 2) :y y :anchor "middle"
                                        :font font :font-size fz :fill foreground))
                   ((consp c)
                    (let* ((cc (svg-line--runs-rtrim (svg-line--runs-ltrim c)))
                           (cw (svg-line--runs-width cc font char-advance fz)))
                      (svg-line--draw-runs svg cc (max x0 (/ (- width cw) 2))
                                           top fz lh font char-advance foreground
                                           hovered hover-color))))
                  ;; RIGHT: flush-right.  Trim trailing whitespace so the
                  ;; visible content reaches the edge (empty trailing segments
                  ;; or a datum's trailing space would otherwise push it left).
                  (cond
                   ((and (stringp r) (> (length (string-trim-right r)) 0))
                    (svg-line--add-text svg (string-trim-right r) :x rx :y y :anchor "end"
                                        :font font :font-size fz :fill foreground))
                   ((consp r)
                    (let ((rr (svg-line--runs-rtrim r)))
                      (svg-line--draw-runs svg rr (max x0 (- rx (svg-line--runs-width rr font char-advance fz)))
                                           top fz lh font char-advance foreground
                                           hovered hover-color))))))
    ;; Centred, row-spanning overlays drawn once over a row range, on top of
    ;; the rows (whose `:center' should be empty there to avoid collision).
    ;; SPEC: (:clock (ROW-A . ROW-B) COLOR ACCENT &optional ALIGN GAP) or
    ;;       (:pie   (ROW-A . ROW-B) FRACTION FILL BG &optional ALIGN GAP).
    ;; Rows 0-indexed, inclusive.  ALIGN is `left', `right' or nil for
    ;; centred, GAP its inset from that edge -- as for an `:image' span.
    ;; An edge-aligned overlay does NOT reserve room: give the line a
    ;; `:right-margin' (or `:pad') wide enough that the rows stop clear of it.
    (dolist (span spans)
      (when (consp span)
        (let* ((rng (nth 1 span))
               (a (if (consp rng) (car rng) 0))
               (b (if (consp rng) (cdr rng) (1- (length rows))))
               (sh (* lh (1+ (- b a))))
               (cx (/ width 2))
               (cy (round (+ rows-y (* lh a) (/ sh 2.0))))
               (r (max 3 (round (* (/ sh 2.0) 0.86)))))
          (pcase (car span)
            (:clock (svg-line--draw-clock
                     svg (svg-line--span-cx (nth 4 span) (nth 5 span) r width pad)
                     cy r (or (nth 2 span) foreground) (nth 3 span)))
            (:pie   (svg-line--draw-pie-at
                     svg (svg-line--span-cx (nth 5 span) (nth 6 span) r width pad)
                     cy r
                     (max 0.0 (min 1.0 (float (nth 2 span))))
                     (svg-line--color (or (nth 3 span) foreground))
                     (svg-line--color (or (nth 4 span) "#d4dcea"))))
            ;; (:image (ROW-A . ROW-B) IMAGE-OR-SVG &optional ALIGN GAP)
            ;; IMAGE-OR-SVG: an Emacs image (its :data, an SVG string) or a raw
            ;; SVG string.  Scaled to the span height, aligned left/center/right.
            ;; (:image (ROW-A . ROW-B) SVG &optional ALIGN GAP)
            ;; SVG: an svg.el DOM node, a raw SVG string, or an Emacs image whose
            ;; :data is SVG.  Spliced as vectors (sharp), scaled to span height.
            ;; (:flank (ROW-A . ROW-B) LEFT RIGHT &optional COLOR GAP GLYPH-SIZE)
            ;; Two text clusters flanking the centred clock/pie, baseline
            ;; centred on the span, drawn in the bar font so Nerd-Font glyphs
            ;; resolve.  LEFT sits just left of the overlay (right-anchored),
            ;; RIGHT just right (left-anchored).  Each side is either a STRING
            ;; (drawn whole at FONT-SIZE) or a (TIME . GLYPH) cons -- the GLYPH
            ;; is drawn nearest the clock at GLYPH-SIZE (default 1.7*FONT-SIZE,
            ;; so squat Nerd-Font weather/icon glyphs read at text scale) and
            ;; TIME sits on its outer side at FONT-SIZE.
            (:flank
             (let* ((left (nth 2 span))
                    (right (nth 3 span))
                    (col (svg-line--color (or (nth 4 span) foreground)))
                    (gap (or (nth 5 span) (round (* fz 0.6))))
                    (gsz (or (nth 6 span) (round (* fz 1.7))))
                    ;; the flanking glyph is drawn at GSZ, so it advances
                    ;; GSZ * the font's em ratio -- which CHAR-ADVANCE at FZ
                    ;; already encodes.  Was hardcoded to 0.5em, which is
                    ;; Terminess's ratio and nobody else's.
                    (gw (round (* gsz (/ (float char-advance) fz))))
                    (tgap (max 1 (round (* fz 0.1))))
                    (tyt (round (+ cy (* fz 0.36))))
                    (tyg (round (+ cy (* gsz 0.36))))
                    (xl (- cx r gap)) (xr (+ cx r gap)))
               (cl-flet ((txt (s x y sz anchor)
                           (when (and (stringp s) (> (length s) 0))
                             (svg-text svg s :x x :y y :text-anchor anchor
                                       :font-family font :font-size sz :fill col))))
                 ;; LEFT: TIME (outer) then GLYPH (inner, nearest clock).
                 (if (consp left)
                     (progn (txt (cdr left) xl tyg gsz "end")
                            (txt (car left) (- xl gw tgap) tyt fz "end"))
                   (txt left xl tyt fz "end"))
                 ;; RIGHT: GLYPH (inner, nearest clock) then TIME (outer).
                 (if (consp right)
                     (progn (txt (cdr right) xr tyg gsz "start")
                            (txt (car right) (+ xr gw tgap) tyt fz "start"))
                   (txt right xr tyt fz "start")))))
            (:image
             (let* ((v (nth 2 span))
                    (align (or (nth 3 span) 'center))
                    (gap (or (nth 4 span) 0))
                    (dom (cond ((and (consp v) (eq (car v) 'svg)) v)
                               ((stringp v) (svg-line--parse-svg v))
                               ((and (consp v) (eq (car v) 'image))
                                (svg-line--parse-svg (plist-get (cdr v) :data))))))
               (when (and (consp dom) (eq (car dom) 'svg))
                 (let* ((attrs (cadr dom))
                        (iw (max 1 (round (svg-line--attr-num (cdr (assq 'width attrs))))))
                        (ih (max 1 (round (svg-line--attr-num (cdr (assq 'height attrs))))))
                        (scale (/ (float sh) ih))
                        (dw (max 1 (round (* iw scale))))
                        (ix (pcase align
                              ('left (+ pad gap))
                              ('right (max pad (- width dw gap)))
                              (_ (round (- cx (/ dw 2.0))))))
                        (iy (round (- cy (/ sh 2.0)))))
                   (svg-line--splice-svg svg dom ix iy scale)))))))))
    (setq svg-line--lines-placements (nreverse svg-line--seg-acc)
          svg-line--lines-lh lh)
    svg))

;;;###autoload
;;;; line interactivity (clicks, menus, hover) -- see also `svg-line-define'
;; ----------------------------------------------------------------
;; Shared by both layouts: `wrap' items (LABEL . STATE) and `lines'
;; interactive segments (TEXT . PLIST) both reduce to placements
;; (X TOP W (LABEL . PLIST)), so one set of hit-test / help / click
;; functions drives clicks, hover boxes and echo help for every bar.

(defcustom svg-line-hover-highlight nil
  "When non-nil, draw a background behind the interactive item under the mouse.
Applies to `wrap' items (tab-line tabs) and `lines' interactive segments
\(mode-line / header-line / tab-bar indicators).  Needs `show-help-function'
wired to call `svg-line--note-help' (the package can't change that global
itself); the mouse enter/move/leave signal arrives through the help-echo
machinery.  See the tab-line config."
  :type 'boolean)

(defcustom svg-line-help-face 'svg-line-help
  "Face applied to an interactive item's hover help, or nil to leave it unstyled."
  :type '(choice (const :tag "No face" nil) face))

(defcustom svg-line-freeze-in-minibuffer '(tab-line)
  "Targets whose windows keep their last render while a minibuffer is active.
Completion sessions preview candidate buffers by swapping them into a
window (consult, embark, ...).  Each swap perturbs a per-window bar --
most visibly a `wrap' tab-line, which gains the preview buffer as a tab
and can re-flow onto a different number of rows, so the bar pops taller
and shorter as the user moves through candidates.  While a minibuffer is
active, a target listed here keeps showing the display string it last
rendered for that window OUTSIDE the minibuffer; normal rendering
resumes the moment the minibuffer closes.  Set to nil to disable."
  :type '(repeat (choice (const tab-line) (const header-line)
                         (const mode-line) (const tab-bar))))

(defvar svg-line--freeze-cache (make-hash-table :test 'eq :weakness 'key)
  "WINDOW -> alist of (NAME . DISPLAY-STRING) from the last unfrozen render.
Weak on the window, so entries die with their windows.  Read (instead of
rendering) while a minibuffer is active for targets in
`svg-line-freeze-in-minibuffer'; written on every render outside one.")

(defface svg-line-help '((t :inherit highlight))
  "Face for an interactive item's hover help (its `help-echo').
With tooltips off the help shows in the echo area, where this contrasting
background makes the cue stand out; the face is preserved into the echo area.")

(defvar svg-line--hovered nil
  "Id of the interactive item under the mouse (its `:id'), or nil.
Set by `svg-line--note-help'; the renderer draws a hover box behind the item
whose `:id' matches.  Ids must be unique per item across all visible windows
\(e.g. include the buffer for a per-window bar), or several boxes would draw.")

(defvar svg-line--wrap-map nil
  "Image map built by the last `svg-line-wrap-image' call, or nil.
A side channel so `svg-line-wrap-image' keeps returning a plain svg object
\(its documented contract) while `svg-line--build-wrap' can still pick up the
per-item hot-spots to put on the image descriptor.")

(defvar svg-line--wrap-placements nil
  "Placements (X TOP CW ITEM) from the last `svg-line-wrap-image' call.
Side channel, like `svg-line--wrap-map'.")
(defvar svg-line--wrap-lh 0
  "Row height from the last `svg-line-wrap-image' call.  Side channel.")

(defvar-local svg-line--placements nil
  "Per-buffer alist (NAME . (LH . PLACEMENTS)) of each line's last hot-spots.
PLACEMENTS are (X TOP W ITEM); ITEM is (LABEL . STATE) for `wrap' or
\(TEXT . PLIST) for `lines'.  Keyed by line NAME so several bars in one buffer
\(tab-line + header-line + mode-line) don't clobber each other.  Hit-tested on
click/hover so the layout is never recomputed (which would need
`with-selected-window' during redisplay -- unsafe).")

(defvar svg-line--placements-global nil
  "Global mirror of `svg-line--placements', keyed by NAME.
The per-window bars (mode/header/tab line) hit-test the buffer-local copy via
the window under the mouse, but the FRAME-level tab bar isn't tied to a buffer
\(its mouse posn reports the frame, not a window), so it reads this mirror.")

(defun svg-line--store-placements (name lh placements)
  "Record PLACEMENTS (row height LH) for line NAME (buffer-local and global)."
  (let ((entry (cons name (cons lh placements))))
    (setq-local svg-line--placements
                (cons entry (assq-delete-all name svg-line--placements)))
    (setq svg-line--placements-global
          (cons entry (assq-delete-all name svg-line--placements-global)))))

(defun svg-line--placements-for (name)
  "Return (LH . PLACEMENTS) recorded for line NAME in the current buffer, or nil."
  (cdr (assq name svg-line--placements)))

(defun svg-line--hit (pcons x y)
  "Return the ITEM in PCONS (LH . PLACEMENTS) covering image pixel (X, Y), or nil."
  (let ((lh (car pcons)))
    ;; NB: `item', not `it' (the latter is anaphoric in `when ... return it').
    (cl-loop for (px top cw item) in (cdr pcons)
             when (and (<= px x) (< x (+ px cw)) (<= top y) (< y (+ top lh)))
             return item)))

;;;###autoload
(defun svg-line-seg (text &rest plist)
  "Return an interactive `lines' segment carrying TEXT and PLIST.
PLIST keys: `:id' (unique hover/identity key), `:help', `:action' (a command
run on left/middle click), `:action-help' (the \"click to ...\" hint), `:menu'
\(an alist (LABEL . COMMAND) for right-click), `:color'/`:face' (text fill),
`:bg' (a persistent background pill), `:weight' (e.g. \"bold\") and `:font'
\(a family of its own).

`:font' is how one line carries several faces -- a handwriting cut for one
chip among mechanical ones, say.  The segment is laid out at THAT family's
advance, measured (`svg-line-font-advance') rather than assumed, so the
families need not be metrically related and what follows the segment still
starts clear of it.

Use as a segment in a `lines' content side; the engine tracks its pixel extent
and wires click/hover/menu just like a `wrap' tab.  Returns nil for empty TEXT
\(so an absent indicator contributes nothing).  See `svg-line-define'."
  (let ((s (svg-line--item->string text)))
    (and (> (length s) 0) (cons :svg-seg (cons s plist)))))

;;;###autoload
(defun svg-line-segs (&rest items)
  "Return a spliced group of ITEMS (strings or `svg-line-seg' forms).
A single `lines' segment can thus emit several runs -- e.g. per-crumb
breadcrumbs.  nil ITEMS are dropped."
  (cons :svg-segs (delq nil items)))

;;;###autoload
(defun svg-line-map-string-regions (str fn)
  "Map FN over the keymap regions of propertized STR, collecting non-nil results.
STR is split into maximal regions delimited by changes in its `keymap' /
`local-map' text property.  For each region FN is called with four arguments:
  TEXT     the region's unpropertized substring;
  START    its start index in STR;
  HANDLER  the region's mouse-1 command (a function) -- looked up in its map as
           `[mode-line mouse-1]', `[header-line mouse-1]' or `[mouse-1]' -- or
           nil when the region carries no such binding;
  HELP     the region's `help-echo' text property (usually a string), or nil.
FN returns an item (typically a string, or an `svg-line-seg' form) or nil; the
non-nil results are collected in order.  This is the splitting and
handler-extraction primitive behind `svg-line-segs-from-string'; call it
directly to render existing clickable mode-line content (a breadcrumb header
line, `which-func', VC, ...) as svg-line segments with your own action/help/id
\(e.g. a direct jump derived from the region's other text properties)."
  (let ((out nil) (i 0) (n (length str)))
    (while (< i n)
      (let* ((km (or (get-text-property i 'keymap str)
                     (get-text-property i 'local-map str)))
             (next (min (or (next-single-property-change i 'keymap str) n)
                        (or (next-single-property-change i 'local-map str) n)))
             ;; `lookup-key' returns an integer (not nil) for a too-long key,
             ;; so take the first binding that is actually `functionp'.
             (handler (and (keymapp km)
                           (seq-some (lambda (k)
                                       (let ((b (lookup-key km k)))
                                         (and (functionp b) b)))
                                     (list [mode-line mouse-1]
                                           [header-line mouse-1]
                                           [mouse-1]))))
             (item (funcall fn (substring-no-properties str i next) i handler
                            (get-text-property i 'help-echo str))))
        (when item (push item out))
        (setq i next)))
    (nreverse out)))

;;;###autoload
(defun svg-line-segs-from-string (str &optional id-prefix)
  "Convert a propertized mode-line/header-line STR into interactive segments.
Existing mode-line content -- a breadcrumb header line, `which-func', a VC
indicator, ... -- already carries `keymap'/`local-map' text properties whose
mouse-1 binding performs the click action and a `help-echo' for the tooltip.
Each region whose map binds a real command to mouse-1 becomes an interactive
`svg-line-seg' whose `:action' invokes that command and whose `:help' is the
region's `help-echo' (first line); the remaining regions stay plain text.  The
result is an `svg-line-segs' group usable as a `lines' content segment, so
existing clickable mode-line content can be rendered by svg-line with its click
and hover affordances intact.

The click invokes the bound command with the originating mouse event, so a
handler that reads its window/position from that event (the usual mode-line
convention) still works.  ID-PREFIX namespaces the per-segment hover `:id's
\(each is (ID-PREFIX . N), defaulting to (svg-line-seg . N)) -- pass a value
unique per bar/window when several share an indicator.  Returns nil for an
empty STR.  For finer control (a custom action/help/id) build on
`svg-line-map-string-regions' directly."
  (when (and (stringp str) (> (length str) 0))
    (let ((idx 0) (prefix (or id-prefix 'svg-line-seg)))
      (apply #'svg-line-segs
             (svg-line-map-string-regions
              str
              (lambda (text _start handler help)
                (if (and handler (> (length (string-trim text)) 0))
                    (progn
                      (setq idx (1+ idx))
                      (svg-line-seg text
                                    :id (cons prefix idx)
                                    :help (and (stringp help)
                                               (substring-no-properties
                                                (car (split-string help "\n"))))
                                    :action handler))
                  text)))))))

(defun svg-line--wrap-place (items width font char-advance fz gap lh
                             &optional center x0 x1 y0)
  "Return placements (X TOP CW ITEM) for ITEMS in a `wrap' layout.
WIDTH bounds each row; FONT, CHAR-ADVANCE, FZ, GAP and LH set per-item width
and row height.  FZ is needed because a label's icon glyphs are drawn larger
than its text and so advance further -- see `svg-line--string-width'.
X0 and X1 bound the flow horizontally (items start at X0 and wrap
at X1, both absolute so the caller can fold a margin and a padding into
them); Y0 is the absolute top of the first row.  When CENTER is non-nil and
the items all fit on a single row (no wrapping), that row is centred between
X0 and X1, so an inset background stays symmetrical.
Shared by drawing (`svg-line-wrap-image') and click hit-testing
\(`svg-line--seg-at') so both agree on where each item sits: insetting HERE is
what keeps the hover and click boxes under the tabs they moved with."
  (let* ((x0 (or x0 0))
         (y0 (or y0 0))
         (right (max (1+ x0) (or x1 width)))
         (x x0) (row 0) (out nil))
    (dolist (it items)
      (let* ((label (car it))
             (cw (svg-line--string-width label char-advance fz font))
             (w  (+ cw (* gap char-advance))))
        (when (and (> x x0) (> (+ x w) right))
          (setq x x0 row (1+ row)))
        ;; X and CW are rounded HERE, at the point of emission, while the
        ;; flow itself keeps accumulating fractionally.  These placements
        ;; become image-map rectangles, which must be whole pixels; rounding
        ;; the advance instead would drift the row (see
        ;; `svg-line--char-advance').
        (push (list (round x) (+ y0 (* row lh)) (round cw) it) out)
        (setq x (+ x w))))
    (setq out (nreverse out))
    ;; centre a single (un-wrapped) row: shift every placement right by half
    ;; the slack, so few tabs sit centred rather than flush-left.
    (when (and center out
               (= y0 (apply #'max 0 (mapcar (lambda (p) (nth 1 p)) out))))
      (let* ((rowwidth (- (apply #'max 0 (mapcar (lambda (p) (+ (nth 0 p) (nth 2 p))) out))
                          x0))
             (offset (- (/ (- (+ x0 right) rowwidth) 2) x0)))
        (when (> offset 0)
          (setq out (mapcar (lambda (p) (cons (+ (nth 0 p) offset) (cdr p))) out)))))
    out))

(defun svg-line--tab-help (item)
  "Compose, face and tag the hover help for wrap ITEM, or nil.
The string is tagged with the item's STATE `:id' in the `svg-line-tab' text
property so `svg-line--note-help' can track which item the mouse is over."
  (let ((state (cdr item)))
    (when (consp state)
      (let* ((help (plist-get state :help))
             (ah   (plist-get state :action-help))
             (parts (delq nil
                          (list help
                                (and (plist-get state :action) ah (concat "click to " ah))
                                (and (plist-get state :menu) "right-click for menu")))))
        (when parts
          (let ((s (string-join parts "  ·  ")))
            (when svg-line-help-face
              (setq s (propertize s 'face svg-line-help-face)))
            (setq s (propertize s 'svg-line-tab (plist-get state :id)))
            s))))))

(cl-defun svg-line-wrap-image (items &key
                                     (width 100)
                                     (font (or svg-line-font (face-attribute 'default :family nil t)))
                                     (font-size svg-line-font-size)
                                     (line-pad svg-line-line-pad)
                                     (char-advance svg-line-char-advance)
                                     (char-advance-ratio svg-line-char-advance-ratio)
                                     (pad 0)
                                     (pad-y 0)
                                     (margin 0)
                                     (margin-y 0)
                                     (rule nil)
                                     (rule-height 1)
                                     (rule-margin nil)
                                     (lead nil)
                                     (lead-x nil)
                                     (gap 3)
                                     (foreground "#000000")
                                     (background nil)
                                     (current-foreground nil)
                                     (current-background nil)
                                     (modified-foreground nil)
                                     (modified-background nil)
                                     (tab-background nil)
                                     (hovered nil)
                                     (hover-color nil)
                                     (center nil))
  "Build a `wrap'-layout SVG from ITEMS, a list of (LABEL . STATE).
Items flow left-to-right and wrap onto new rows at WIDTH.  GAP is the
inter-item gap in character widths.  CHAR-ADVANCE-RATIO sets the advance as
a fraction of the font size when CHAR-ADVANCE pins no pixel value.

Spacing comes in two kinds, as in CSS.  MARGIN and MARGIN-Y sit OUTSIDE the
background: clear space that separates this bar from whatever is next to it.
PAD and PAD-Y sit INSIDE it, between the background edge and the pills --
which is what makes the pills read as floating in a container, their boxes
being drawn at the full row height so that without it a pill fills the
background exactly.  MARGIN narrows the painted background itself, so a bar
can be visibly narrower than the window it sits in.  Each vertical one is a
number, or a cons (TOP . BOTTOM) for an uneven gap.
FONT, FONT-SIZE, LINE-PAD,
CHAR-ADVANCE, FOREGROUND and BACKGROUND set the text family, size,
per-row padding, character advance and base colours.  When CENTER is
non-nil and the items fit on a single row, that row is centred within
WIDTH.  Returns an svg object.

STATE selects how each item is styled and made interactive:
  - nil / non-nil atom  -- treated as CURRENTP (backward compatible);
  - a plist             -- `:current' / `:modified' for styling, plus the
    optional `:id' `:help' `:action' `:action-help' `:menu' for hover/click
    (see `svg-line-define').

A current item is drawn bold over CURRENT-BACKGROUND; a modified item uses
MODIFIED-FOREGROUND (and MODIFIED-BACKGROUND when set); an ordinary item whose
`:id' equals HOVERED gets a HOVER-COLOR box, and any other ordinary item gets a
TAB-BACKGROUND box when that is non-nil (so inactive tabs can be delineated like
the built-in tab line; nil leaves them transparent).  Items with
`:help'/`:action'/`:menu' become image map hot-spots (per-item help-echo + hand
pointer)."
  (let* ((foreground (svg-line--color foreground))
         (background (svg-line--color background))
         (current-foreground (svg-line--color current-foreground))
         (current-background (svg-line--color current-background))
         (modified-foreground (svg-line--color modified-foreground))
         (modified-background (svg-line--color modified-background))
         (tab-background (svg-line--color tab-background))
         (hover-color (svg-line--color hover-color))
         ;; the NOMINAL size names an em, and families fill their em very
         ;; differently; normalise it so the drawn capitals are the same
         ;; height whatever the family (`svg-line-normalise-font-size')
         (fz (svg-line--font-size-for font font-size))
         (char-advance (svg-line--char-advance char-advance fz char-advance-ratio))
         (lh (+ fz line-pad))
         (pad-y (svg-line--pad-y pad-y))
         (pad-top (car pad-y))
         (pad-bot (cdr pad-y))
         (margin-y (svg-line--pad-y margin-y))
         (mtop (car margin-y))
         (mbot (cdr margin-y))
         (rows-y (+ mtop pad-top))
         (x0 (+ margin pad))
         (x1 (max (1+ x0) (- width margin pad)))
         (placements (svg-line--wrap-place items width font char-advance fz gap lh
                                           center x0 x1 rows-y))
         ;; The placer has already put the first row at ROWS-Y, so the tallest
         ;; placement accounts for the top margin and padding both.
         (rows-bottom (apply #'max (+ rows-y lh)
                             (mapcar (lambda (p) (+ (nth 1 p) lh)) placements)))
         (bg-h (+ pad-top (- rows-bottom rows-y) pad-bot))
         (height (max 1 (+ mtop bg-h mbot)))
         (svg (svg-create width height))
         (map nil))
    ;; As in the `lines' layout: MARGIN/MARGIN-Y sit OUTSIDE this rect and are
    ;; never painted (space between bars), PAD/PAD-Y inside it (space around
    ;; the pills, which is what makes them read as floating in a container
    ;; rather than filling one).
    (when background
      (svg-rectangle svg margin mtop (max 1 (- width (* 2 margin))) bg-h
                     :fill background))
    ;; RULE: a hairline along the TOP of the image, inset from both edges.
    ;; Drawn here rather than left to the `:overline' of the face the image
    ;; sits on: redisplay paints a face attribute across the face's whole
    ;; extent, and for a window-width image that is the whole window, so an
    ;; overline can never be inset.  Inside the SVG it is inset like
    ;; everything else -- by MARGIN unless RULE-MARGIN overrides, so by
    ;; default it lines up with the background rect above rather than
    ;; reaching past it.  Drawn at y=0, i.e. OUTSIDE margin-y, because the
    ;; rule marks the edge of the WINDOW, not the edge of the painted bar.
    (when rule
      (let ((rm (or rule-margin margin)))
        (svg-rectangle svg rm 0 (max 1 (- width (* 2 rm)))
                       (max 1 rule-height) :fill rule)))
    ;; LEAD: drawn in the LEFT MARGIN -- the strip between the window edge and
    ;; X0 that the flow never reaches, because X0 is `margin' + `pad' in.
    ;; Deliberately NOT an item: `svg-line--wrap-place' would give it a slot,
    ;; and a slot that appears and disappears pushes every tab sideways as it
    ;; comes and goes.  For something transient -- a window-picking key, up
    ;; for as long as it takes to press it -- a bar that jumps is worse than
    ;; one with an empty margin.  Same pill as a current tab: this is the one
    ;; thing on the line asking to be read.
    (when (and lead (> (length lead) 0))
      (let ((lw (max 1 (round (svg-line--string-width lead char-advance fz font))))
            (lx (or lead-x pad)))
        (when current-background
          (svg-rectangle svg lx rows-y lw lh :fill current-background :rx 3))
        (svg-line--add-text svg lead :x lx :y (+ rows-y fz)
                            :font font :font-size fz
                            :fill (or current-foreground foreground)
                            :weight "bold")))
    (dolist (p placements)
      (cl-destructuring-bind (px top cw it) p
        (let* ((label (car it))
               (state (cdr it))
               (currentp  (if (consp state) (plist-get state :current) state))
               (modifiedp (and (consp state) (plist-get state :modified)))
               (hoveredp  (and hover-color (consp state) hovered
                               (equal (plist-get state :id) hovered)))
               (box  (cond ((and currentp modifiedp) (or modified-foreground current-background))
                           (currentp  current-background)
                           (modifiedp modified-background)
                           (hoveredp  hover-color)
                           (t tab-background)))
               (fill (cond (currentp  (or current-foreground foreground))
                           (modifiedp (or modified-foreground foreground))
                           (t foreground))))
          (when box
            (svg-rectangle svg px top cw lh :fill box :rx 3))
          (svg-line--add-text svg label :x px :y (+ top fz)
                              :font font :font-size fz :fill fill
                              :weight (if currentp "bold" "normal"))
          ;; image-map hot-spot for hover/click
          (let ((eh (svg-line--tab-help it)))
            (when (and (consp state)
                       (or eh (plist-get state :action) (plist-get state :menu)))
              (let ((props (list 'pointer 'hand)))
                (when eh (setq props (append props (list 'help-echo eh))))
                (push (list (cons 'rect (cons (cons px top) (cons (+ px cw) (+ top lh))))
                            (make-symbol (format "svg-line-tab-%d-%d" px top))
                            props)
                      map)))))))
    ;; Stash the hot-spot map and placements in side channels (so this fn keeps
    ;; returning a plain svg object) and return the svg object.
    (setq svg-line--wrap-map (nreverse map)
          svg-line--wrap-placements placements
          svg-line--wrap-lh lh)
    svg))

;;;###autoload
(defun svg-line-display (svg &optional props)
  "Wrap SVG object as a one-space string carrying it as a display image.
PROPS, if given, are extra `svg-image' keywords (e.g. (:map MAP)).
Pinned to `:scale' 1.0: the image IS the line at its exact target pixel
width, so it must NOT inherit `image-scaling-factor' (auto), which would
scale it with the default font and overflow the frame.  Scale the line by
scaling its `:font-size'/`:char-advance' instead, not the image."
  (propertize " " 'display (apply #'svg-image svg :ascent 'center :scale 1.0 props)))

;;;; Safety wrapper
;; ----------------------------------------------------------------
;; Guards against (a) a Lisp error in a content function breaking the
;; display, and (b) a render that re-enters the render machinery (a
;; feedback loop), which returns the last good value instead of looping.

(defvar svg-line--rendering nil
  "Non-nil while a line is rendering; blocks re-entrant renders.")
(defvar svg-line--last-good (make-hash-table :test 'eq)
  "Per-line last successfully rendered value, keyed by line name.")

(defun svg-line-safe (name thunk)
  "Call THUNK for line NAME, guarding errors and re-entrancy."
  (if svg-line--rendering
      (gethash name svg-line--last-good " ")
    (let ((svg-line--rendering t))
      (condition-case err
          (puthash name (funcall thunk) svg-line--last-good)
        (error (propertize (format " ⚠ %s: %s " name (error-message-string err))
                           'face 'error))))))

;;;; Line registry + spec resolution
;; ----------------------------------------------------------------

(defvar svg-line--registry (make-hash-table :test 'eq)
  "Map of line NAME -> plist with :spec :renderer :saved keys.")

(defun svg-line--entry (name)
  "Return the registry entry for NAME, or nil."
  (gethash name svg-line--registry))

(defun svg-line--spec (name)
  "Return the spec plist for line NAME."
  (plist-get (svg-line--entry name) :spec))

(defun svg-line--opt (spec key &optional default)
  "Resolve option KEY from SPEC (value-or-function), else DEFAULT."
  (let ((v (plist-member spec key)))
    (if v (svg-line--val (cadr v)) default)))

(defun svg-line--window-width ()
  "Pixel width available to a window-scoped bar.

NOT `window-pixel-width': that one \"includes the fringes and margins of
WINDOW as well as any vertical dividers or scroll bars belonging to WINDOW\",
and a bar is not drawn across those.  Using it directly makes every image
too wide by the divider -- harmless while dividers are a hairline, but with a
wide one the right-aligned content is pushed clean off the visible edge and
clipped, in every window except the last of its row (which has no right
divider and so renders correctly, making it look like a split bug)."
  (max 1 (- (window-pixel-width)
            (or (ignore-errors (window-right-divider-width)) 0)
            (or (and (fboundp 'window-scroll-bar-width)
                     (ignore-errors (window-scroll-bar-width)))
                0))))

(defun svg-line--width (spec)
  "Resolve the pixel width for SPEC."
  (let ((w (or (plist-get spec :width)
               (if (eq (plist-get spec :target) 'tab-bar) 'frame 'window))))
    (max 1 (pcase w
             ('frame (frame-inner-width))
             ('window (svg-line--window-width))
             ((pred functionp) (funcall w))
             ((pred integerp) w)
             (_ 100)))))

(defun svg-line--active-p (spec)
  "Return non-nil if SPEC's `:active' predicate is absent or holds."
  (let ((p (plist-get spec :active)))
    (or (null p) (funcall p))))

;;;; Per-spec builders
;; ----------------------------------------------------------------

(defun svg-line--side (segments)
  "Render SEGMENTS to a side value: a plain string if all text, else a run list."
  (let ((runs (svg-line--render-runs segments)))
    (if (svg-line--runs-all-text-p runs)
        (mapconcat (lambda (r) (nth 1 r)) runs "")
      runs)))

;;;; Text-scale responsiveness
;; ----------------------------------------------------------------
;; The line image is pinned to :scale 1.0 (see `svg-line-display'), so it
;; never inherits `image-scaling-factor' and overflows.  To still track the
;; default font size (e.g. `default-text-scale', or `set-face-attribute' on
;; `default'), the layout SIZES -- font-size, line-pad, padding, advance --
;; are scaled by the ratio of the current `default'-face height to a
;; captured reference, so the line RE-RENDERS larger/sharper instead.

(defcustom svg-line-scale-with-text-scale t
  "When non-nil, scale line sizes with the `default'-face height.
Lets lines track `default-text-scale' (font-size, line-pad, padding and
char-advance grow proportionally).  nil keeps a fixed pixel size regardless
of the default font."
  :type 'boolean)

(defvar svg-line--base-text-height nil
  "Reference `default'-face :height for a text scale of 1.0 (captured once).
Reset to nil to re-capture (e.g. after changing the unscaled default font).")

(defun svg-line--text-scale ()
  "Factor relating the current `default'-face height to the reference.
Returns 1.0 when scaling is disabled or unavailable."
  (if (not svg-line-scale-with-text-scale)
      1.0
    (let ((h (ignore-errors (face-attribute 'default :height nil 'default))))
      (when (and (numberp h) (null svg-line--base-text-height))
        (setq svg-line--base-text-height h))
      (if (and (numberp h) (numberp svg-line--base-text-height)
               (> svg-line--base-text-height 0))
          (/ (float h) svg-line--base-text-height)
        1.0))))

(defun svg-line--scaled (size)
  "Scale pixel SIZE by the current text-scale factor (integer result).
SIZE may be a cons (A . B) -- both halves are scaled -- so the asymmetric
form of `:pad-y' survives text scaling like every other measurement."
  (if (consp size)
      (cons (round (* (car size) (svg-line--text-scale)))
            (round (* (cdr size) (svg-line--text-scale))))
    (round (* size (svg-line--text-scale)))))

(defun svg-line--pad-y (pad-y)
  "Normalise PAD-Y to (TOP . BOTTOM).
A number pads both ends equally; a cons (TOP . BOTTOM) pads them
separately, which is how a bar gets a gap on the side facing its neighbour
without an equal one on the side facing away."
  (cond ((consp pad-y) (cons (or (car pad-y) 0) (or (cdr pad-y) 0)))
        ((numberp pad-y) (cons pad-y pad-y))
        (t (cons 0 0))))

(defun svg-line--row-segs (row)
  "Return (LEFT-SEGS CENTER-SEGS RIGHT-SEGS) for a `lines' content ROW.
ROW is either a cons (LEFT-SEGS . RIGHT-SEGS) -- no centre -- or a plist with
`:left'/`:center'/`:right' keys for a three-part row."
  (if (keywordp (car-safe row))
      (list (plist-get row :left) (plist-get row :center) (plist-get row :right))
    (list (car row) nil (cdr row))))

(defun svg-line--build-lines (spec)
  "Build the `lines' SVG for SPEC.
Each content row is a cons (LEFT-SEGMENTS . RIGHT-SEGMENTS) or a plist
\(:left L :center C :right R) for a centred middle (see `svg-line--row-segs').
A segment may emit a progress bar (:svg-bar ...), pie (:svg-pie ...) or
interactive segment (:svg-seg ...) token (see `svg-line--render-runs' and
`svg-line-seg'); a side with any such token is laid out with CHAR-ADVANCE
spacing, otherwise with exact text anchoring.  The hovered interactive segment
gets a hover box.  Sizes scale with the default font (see
`svg-line-scale-with-text-scale')."
  (let* ((active (svg-line--active-p spec))
         (fg (or (and (not active) (svg-line--opt spec :inactive-foreground))
                 (svg-line--opt spec :foreground "#000000")))
         (bg (if active
                 (svg-line--opt spec :background)
               (or (svg-line--opt spec :inactive-background)
                   (svg-line--opt spec :background))))
         (sc (svg-line--text-scale)))
    (svg-line-image
     (mapcar (lambda (row)
               (cl-destructuring-bind (l c r) (svg-line--row-segs row)
                 (if c
                     (vector (svg-line--side l) (svg-line--side c) (svg-line--side r))
                   (cons (svg-line--side l) (svg-line--side r)))))
             (funcall (plist-get spec :content)))
     :width (svg-line--width spec)
     :font (svg-line--opt spec :font
                          (or svg-line-font (face-attribute 'default :family nil t)))
     :font-size (svg-line--scaled (svg-line--opt spec :font-size svg-line-font-size))
     :line-pad (svg-line--scaled (svg-line--opt spec :line-pad svg-line-line-pad))
     :pad (svg-line--scaled (svg-line--opt spec :pad 0))
     :pad-y (svg-line--scaled (svg-line--opt spec :pad-y 0))
     :margin (svg-line--scaled (svg-line--opt spec :margin 0))
     :margin-y (svg-line--scaled (svg-line--opt spec :margin-y 0))
     :right-margin (svg-line--scaled (svg-line--opt spec :right-margin 0))
     :rule (svg-line--opt spec :rule nil)
     :rule-height (svg-line--scaled (svg-line--opt spec :rule-height 1))
     :rule-margin (let ((m (svg-line--opt spec :rule-margin nil)))
                    (and m (svg-line--scaled m)))
     :seg-shape (svg-line--opt spec :seg-shape 'round)
     :seg-slant (let ((v (svg-line--opt spec :seg-slant nil)))
                  (and v (svg-line--scaled v)))
     ;; nil lets `svg-line-image' derive the advance from the (scaled) font
     ;; size; an explicit value is scaled to match.
     :char-advance (let ((e (or (svg-line--opt spec :char-advance nil)
                                svg-line-char-advance)))
                     (and e (* e sc)))
     ;; NOT scaled: a ratio is scale-free, and `svg-line-image' applies it to
     ;; the already-scaled font size.
     :char-advance-ratio (svg-line--opt spec :char-advance-ratio
                                        svg-line-char-advance-ratio)
     :foreground fg
     :background bg
     :hovered svg-line--hovered
     :hover-color (or (svg-line--opt spec :hover-color)
                      (face-background 'highlight nil 'default) "#444466")
     :icon (svg-line--opt spec :icon)
     :icon-color (or (and (not active) (svg-line--opt spec :inactive-icon-color))
                     (svg-line--opt spec :icon-color))
     :icon-width (let ((w (svg-line--opt spec :icon-width)))
                   (if (eq w 'square) 'square (and (numberp w) (svg-line--scaled w))))
     :icon-scale (svg-line--opt spec :icon-scale 0.74)
     :spans (let ((s (svg-line--opt spec :spans)))
              (if (functionp s) (funcall s) s)))))

(defun svg-line--build-wrap (spec)
  "Build the `wrap' layout for SPEC, returning (SVG . MAP).
MAP is the per-item image-map hot-spots (nil when no interactive items).
When SPEC has an `:active' predicate that is false, the inactive variant
of each colour applies (falling back to the active colour when unset),
mirroring the `lines' layout."
  (let* ((active (svg-line--active-p spec))
         (sc (svg-line--text-scale))
         (pick (lambda (key inactive-key &optional default)
                 (if active
                     (svg-line--opt spec key default)
                   (or (svg-line--opt spec inactive-key)
                       (svg-line--opt spec key default)))))
         (svg
    (svg-line-wrap-image (funcall (plist-get spec :content))
                         :width (svg-line--width spec)
                         :font (svg-line--opt spec :font
                                              (or svg-line-font (face-attribute 'default :family nil t)))
                         :font-size (svg-line--scaled (svg-line--opt spec :font-size svg-line-font-size))
                         :line-pad (svg-line--scaled (svg-line--opt spec :line-pad svg-line-line-pad))
                         :char-advance (let ((e (or (svg-line--opt spec :char-advance nil)
                                                    svg-line-char-advance)))
                                         (and e (* e sc)))
                         :char-advance-ratio (svg-line--opt spec :char-advance-ratio
                                                            svg-line-char-advance-ratio)
                         :pad (svg-line--scaled (svg-line--opt spec :pad 0))
                         :pad-y (svg-line--scaled (svg-line--opt spec :pad-y 0))
                         :margin (svg-line--scaled (svg-line--opt spec :margin 0))
                         :margin-y (svg-line--scaled (svg-line--opt spec :margin-y 0))
                         :rule (svg-line--opt spec :rule nil)
                         :rule-height (svg-line--scaled (svg-line--opt spec :rule-height 1))
                         :rule-margin (let ((m (svg-line--opt spec :rule-margin nil)))
                                        (and m (svg-line--scaled m)))
                         :lead (svg-line--opt spec :lead nil)
                         :lead-x (let ((x (svg-line--opt spec :lead-x nil)))
                                   (and x (svg-line--scaled x)))
                         :gap (svg-line--opt spec :gap 3)
                         :foreground (funcall pick :foreground :inactive-foreground "#000000")
                         :background (funcall pick :background :inactive-background)
                         :current-foreground (funcall pick :current-foreground :inactive-current-foreground)
                         :current-background (funcall pick :current-background :inactive-current-background)
                         :modified-foreground (funcall pick :modified-foreground :inactive-modified-foreground)
                         :modified-background (funcall pick :modified-background :inactive-modified-background)
                         :tab-background (funcall pick :tab-background :inactive-tab-background)
                         :hovered svg-line--hovered
                         :hover-color (or (svg-line--opt spec :hover-color)
                                          (face-background 'highlight nil 'default)
                                          "#444466")
                         :center (svg-line--opt spec :center))))
    (cons svg svg-line--wrap-map)))

;;;; interactivity: hover tracking + click/menu dispatch (wrap + lines)
;; ----------------------------------------------------------------

(defun svg-line--popup-menu (title items)
  "Pop up a menu of ITEMS at the current event and run the chosen command.
ITEMS is an alist of (LABEL . COMMAND); TITLE labels the menu."
  (let ((choice (x-popup-menu last-input-event
                              (list (or title "svg-line") (cons "" items)))))
    (when choice
      (if (commandp choice) (call-interactively choice) (funcall choice)))))

(defvar svg-line--hover-timer nil
  "Idle timer that applies a hover re-render off the redisplay path.")

(defun svg-line--note-help (help)
  "Update the hovered item from HELP and re-render if it changed.
Wire `show-help-function' to call this (then display HELP): it fires on mouse
enter, move AND leave (leave with nil), so the hovered item's `:id' -- carried
in HELP's `svg-line-tab' text property -- can be tracked and a hover box drawn.
Works for both `wrap' tabs and `lines' interactive segments.  The re-render is
DEFERRED to an idle timer: this runs during the help-echo display (itself
during redisplay), and forcing a redisplay synchronously here would re-enter
the renderer and degrade the other lines (the safety guard returns a stale
value)."
  (when svg-line-hover-highlight
    (let ((id (and (stringp help) (> (length help) 0)
                   (get-text-property 0 'svg-line-tab help))))
      (unless (equal id svg-line--hovered)
        (setq svg-line--hovered id)
        (when (timerp svg-line--hover-timer) (cancel-timer svg-line--hover-timer))
        (setq svg-line--hover-timer
              (run-with-idle-timer 0 nil (lambda () (force-mode-line-update t))))))))

(defun svg-line--seg-at-posn (name posn)
  "Return line NAME's interactive item under POSN, or nil.
Handles both bar kinds: a per-window bar (mode/header/tab line) reports a live
window and IMAGE-relative `posn-object-x-y', so we hit-test that window's
buffer-local placements; the FRAME-level tab bar reports the frame and nil
object coords, so we use the area-relative `posn-x-y' against the global
placement mirror.  No layout recompute and no `with-selected-window' (which
would corrupt redisplay when called from a help-echo function)."
  (let* ((win (and posn (posn-window posn)))
         (obj (and posn (posn-object-x-y posn))))
    (if (windowp win)
        ;; per-window bar: image coords = object-x-y, placements = buffer-local
        (let ((x (car-safe obj)) (y (cdr-safe obj)))
          (when (and (numberp x) (numberp y))
            (with-current-buffer (window-buffer win)
              (svg-line--hit (svg-line--placements-for name) x y))))
      ;; frame-level bar (tab-bar): image coords = posn-x-y, placements = global
      (let* ((xy (and posn (posn-x-y posn)))
             (x (car-safe xy)) (y (cdr-safe xy)))
        (when (and (numberp x) (numberp y))
          (svg-line--hit (cdr (assq name svg-line--placements-global)) x y))))))

(defun svg-line--seg-at (name posn)
  "Return line NAME's interactive item under POSN (a click), or nil."
  (svg-line--seg-at-posn name posn))

(defun svg-line--seg-help-fn (name)
  "Return a `help-echo' FUNCTION for line NAME.
A special area (tab line, header line, mode line, tab bar) does not fire image
map *area* help-echo, but it does call a STRING-level help-echo function on
mouse move.  We turn the frame-relative `mouse-pixel-position' into a posn with
`posn-at-x-y' and hit-test it (see `svg-line--seg-at-posn'), then return the
item's help (tagged, so `svg-line--note-help' tracks hover).
When the pointer is over the bar but NOT on an item, the hover box and help are
cleared explicitly and an empty help returned -- the whole bar is one display
string, and Emacs does not reliably re-fire `show-help-function' with nil while
the pointer stays within it, so the previous hover would otherwise linger."
  (lambda (_win _obj _pos)
    (let* ((mp (mouse-pixel-position))
           (frame (car mp)) (mx (cadr mp)) (my (cddr mp))
           (posn (and (framep frame) (numberp mx) (numberp my)
                      (ignore-errors (posn-at-x-y mx my frame))))
           (item (and posn (svg-line--seg-at-posn name posn))))
      (if item
          (svg-line--tab-help item)
        (svg-line--note-help nil)   ; clear a lingering hover box
        ""))))                      ; empty help clears the echo cue

(defun svg-line--seg-make-click-map (name)
  "Return a keymap dispatching clicks for line NAME.
Left/middle click runs the item's `:action'; right click pops its `:menu'.
The window the click landed in is selected first, so an action like
`switch-to-buffer' (a tab-line tab) affects THAT window -- matching the
default tab-line behaviour -- rather than whichever window was selected.
Bindings are duplicated under a catch-all default so the click resolves
whether or not the special area prepends an event prefix."
  (let* ((run (lambda ()
                (interactive)
                (let* ((ev (event-start last-input-event))
                       (win (posn-window ev))
                       (it (svg-line--seg-at name ev))
                       (cmd (and (consp (cdr-safe it)) (plist-get (cdr it) :action))))
                  (when cmd
                    (when (window-live-p win) (select-window win))
                    (call-interactively cmd)))))
         (menu (lambda ()
                 (interactive)
                 (let* ((ev (event-start last-input-event))
                        (win (posn-window ev))
                        (it (svg-line--seg-at name ev))
                        (items (and (consp (cdr-safe it)) (plist-get (cdr it) :menu))))
                   (when items
                     (when (window-live-p win) (select-window win))
                     (svg-line--popup-menu (car it) items)))))
         (sub (make-sparse-keymap)) (km (make-sparse-keymap)))
    (dolist (k (list [mouse-1] [mouse-2]))
      (define-key km k run) (define-key sub k run))
    (define-key km [down-mouse-3] menu) (define-key sub [down-mouse-3] menu)
    (dolist (k (list [down-mouse-1] [down-mouse-2] [mouse-3]))
      (define-key km k #'ignore) (define-key sub k #'ignore))
    (define-key km [t] sub)
    km))

(defun svg-line--interactive (str name has-spots &optional pointer)
  "Attach click/hover/help props to display STR for line NAME, when HAS-SPOTS.
A special area honours STRING-level keymap/help-echo but not image map *area*
properties, so clicks and hover are driven from the string: a click keymap
\(dispatched by pixel position), a help-echo FUNCTION (mouse-move hover +
tooltip) and, when POINTER is given, that mouse pointer.  (The hover BOX is
drawn into the SVG itself from `svg-line--hovered'.)"
  (if has-spots
      (let ((s (propertize str
                           'keymap (svg-line--seg-make-click-map name)
                           'help-echo (svg-line--seg-help-fn name))))
        (if pointer (propertize s 'pointer pointer) s))
    str))

(defun svg-line--render (name)
  "Render line NAME to a display string (error/loop guarded).
For a target in `svg-line-freeze-in-minibuffer', returns the window's
last non-minibuffer render while a minibuffer is active (redisplay
evaluates a window's format with that window selected, so
`selected-window' identifies it)."
  (svg-line-safe
   name
   (lambda ()
     (let* ((spec (svg-line--spec name))
            (freezable (memq (plist-get spec :target)
                             svg-line-freeze-in-minibuffer))
            (win (and freezable (selected-window)))
            (in-mini (and freezable (active-minibuffer-window)))
            (ctx (svg-line--opt spec :context-buffer)))
       (or (and in-mini win
                (cdr (assq name (gethash win svg-line--freeze-cache))))
           (let ((str
                  ;; :context-buffer pins content evaluation to a stable
                  ;; buffer -- e.g. a frame bar whose buffer-dependent
                  ;; segments would otherwise flip as completion previews
                  ;; swap the selected window's buffer, repainting the bar
                  ;; with alternating images (a visible flash).
                  (with-current-buffer (if (buffer-live-p ctx) ctx (current-buffer))
                    (if (eq (or (plist-get spec :layout) 'lines) 'wrap)
                        (let ((s (svg-line-display (car (svg-line--build-wrap spec)))))
                          (svg-line--store-placements name svg-line--wrap-lh svg-line--wrap-placements)
                          ;; the whole wrap line is tabs, so a hand pointer everywhere fits
                          (svg-line--interactive s name (and svg-line--wrap-placements t) 'hand))
                      (let ((svg (svg-line--build-lines spec)))
                        (svg-line--store-placements name svg-line--lines-lh svg-line--lines-placements)
                        ;; a lines bar has large non-interactive gaps, so no global pointer
                        (svg-line--interactive (svg-line-display svg) name
                                               (and svg-line--lines-placements t)))))))
             (when (and win (not in-mini) (window-live-p win))
               (setf (alist-get name (gethash win svg-line--freeze-cache)) str))
             str))))))

(defun svg-line--renderer (name)
  "Return (creating if needed) the named renderer function symbol for NAME."
  (let ((sym (intern (format "svg-line--render-%s" name))))
    (defalias sym (lambda () (svg-line--render name))
      (format "Render the `%s' svg-line (made by `svg-line-define')." name))
    sym))

;;;; tab-bar interactivity (frame-level: clicks via advice, hover via poll)
;; ----------------------------------------------------------------
;; The tab bar -- unlike the mode/header/tab line -- does NOT honour a display
;; string's `keymap' or `help-echo': it routes mouse events through `tab-bar-map'
;; -> `tab-bar-mouse-*' (which read a `menu-item' property) and never fires a
;; per-position help-echo.  So a `tab-bar' svg-line gets its interactivity here,
;; automatically, when it is activated: clicks by advising the tab-bar mouse
;; commands to hit-test our placements, and hover by polling the mouse position
;; while the pointer is over the tab bar.  Set up by `svg-line--install' and
;; torn down by `svg-line--uninstall' for the `tab-bar' target.

(defvar svg-line--tab-bar-lines nil
  "Active svg-line names installed on the `tab-bar' target.")
(defvar svg-line--tab-bar-hover-timer nil
  "Repeating timer driving tab-bar hover; nil when not running.")
(defvar svg-line--tab-bar-hover-was-over nil
  "Non-nil if the last poll found the pointer over a tab-bar item.")
(defvar svg-line--tab-bar-hover-last nil
  "(ID . TEXT) of the help last fed to `show-help-function' by the poll.
Used to skip repeats: re-showing identical help every poll repaints the
echo area, and each repaint re-runs every mode-line's :eval -- an 8 Hz
redisplay treadmill for as long as the pointer rests on the tab bar.")

(defun svg-line--tab-bar-item-at (posn)
  "Return the interactive item under POSN for any active tab-bar svg-line, or nil."
  (and posn
       (cl-some (lambda (name)
                  (let ((it (svg-line--seg-at-posn name posn)))
                    (and (consp it) (consp (cdr it)) it)))
                svg-line--tab-bar-lines)))

(defun svg-line--tab-bar-mouse-down-advice (orig event &rest args)
  "Around-advice for `tab-bar-mouse-down-1'.
If EVENT lands on a tab-bar svg-line item, run its `:action'; otherwise call
ORIG with EVENT and ARGS (the default tab-bar behaviour)."
  (let* ((it (svg-line--tab-bar-item-at (event-start event)))
         (cmd (and it (plist-get (cdr it) :action))))
    (if cmd (call-interactively cmd)
      (apply orig event args))))

(defun svg-line--tab-bar-context-menu-advice (orig event &rest args)
  "Around-advice for `tab-bar-mouse-context-menu'.
If EVENT lands on a tab-bar svg-line item, pop its `:menu'; otherwise call ORIG
with EVENT and ARGS (the default tab-bar context menu)."
  (let* ((it (svg-line--tab-bar-item-at (event-start event)))
         (menu (and it (plist-get (cdr it) :menu))))
    (if menu (svg-line--popup-menu (car it) menu)
      (apply orig event args))))

(defun svg-line--tab-bar-hover-poll ()
  "Drive hover help/box for a tab-bar svg-line from the mouse position.
The tab bar gives no per-position help-echo, so we poll: when the pointer is
over a tab-bar item, feed its (tagged) help through `show-help-function' -- the
same path the per-window bars use -- which shows the echo cue and sets the
hovered id; on leaving, clear it once.  Help is only re-fed when the hovered
item or its text actually changes (`svg-line--tab-bar-hover-last'): calling
`show-help-function' with the same help every poll repaints the echo area,
which re-evaluates every mode-line each cycle."
  (when (and svg-line-hover-highlight svg-line--tab-bar-lines
             (functionp show-help-function))
    (let* ((mp (mouse-pixel-position))
           (frame (car mp)) (mx (cadr mp)) (my (cddr mp))
           (posn (and (framep frame) (integerp mx) (integerp my)
                      (ignore-errors (posn-at-x-y mx my frame))))
           (over (and posn (eq (posn-area posn) 'tab-bar)))
           (item (and over (svg-line--tab-bar-item-at posn)))
           (help (and item (svg-line--tab-help item))))
      (cond
       (over
        (let ((key (cons (and (stringp help) (> (length help) 0)
                              (get-text-property 0 'svg-line-tab help))
                         (and (stringp help) (substring-no-properties help)))))
          (unless (equal key svg-line--tab-bar-hover-last)
            (setq svg-line--tab-bar-hover-last key)
            (ignore-errors (funcall show-help-function help))))
        (setq svg-line--tab-bar-hover-was-over t))
       (svg-line--tab-bar-hover-was-over
        (ignore-errors (funcall show-help-function nil))
        (setq svg-line--tab-bar-hover-was-over nil
              svg-line--tab-bar-hover-last nil))))))

(defun svg-line--tab-bar-enable ()
  "Enable tab-bar click/hover interactivity (idempotent)."
  (advice-add 'tab-bar-mouse-down-1 :around #'svg-line--tab-bar-mouse-down-advice)
  (advice-add 'tab-bar-mouse-context-menu :around #'svg-line--tab-bar-context-menu-advice)
  (unless (timerp svg-line--tab-bar-hover-timer)
    (setq svg-line--tab-bar-hover-timer
          (run-with-timer 0.12 0.12 #'svg-line--tab-bar-hover-poll))))

(defun svg-line--tab-bar-disable ()
  "Tear down tab-bar interactivity once no tab-bar svg-line remains active."
  (unless svg-line--tab-bar-lines
    (advice-remove 'tab-bar-mouse-down-1 #'svg-line--tab-bar-mouse-down-advice)
    (advice-remove 'tab-bar-mouse-context-menu #'svg-line--tab-bar-context-menu-advice)
    (when (timerp svg-line--tab-bar-hover-timer)
      (cancel-timer svg-line--tab-bar-hover-timer))
    (setq svg-line--tab-bar-hover-timer nil
          svg-line--tab-bar-hover-was-over nil
          svg-line--tab-bar-hover-last nil)))

;;;; Definition + activation
;; ----------------------------------------------------------------

;;;###autoload
(defun svg-line-define (name &rest spec)
  "Define an svg-line NAME from SPEC (a plist) and create its renderer.
Recognised SPEC keys:
  :target  one of `tab-bar' `mode-line' `header-line' `tab-line' (required)
  :layout  `lines' (default) or `wrap'
  :content a function returning the line's content (required):
             - for `lines': a list of (LEFT-SEGMENTS . RIGHT-SEGMENTS); a
               segment may be a string, a function, or a pie
               (:svg-pie FRAC FILL BG) / progress-bar (:svg-bar FRAC W FILL BG)
               token (or a function returning one).  Icons are Nerd-Font
               glyphs in the text, so they need no token of their own.
             - for `wrap':  a list of (LABEL . STATE), where STATE is a
               CURRENTP atom or a plist with `:current'/`:modified' keys
  :width   `frame', `window', an integer, or a function (default by target)
  :context-buffer  a function returning a buffer to make current while
           evaluating :content, or nil for the buffer current at render
           time.  Pins a frame-level bar's buffer-dependent segments to
           a stable context -- e.g. the buffer the user came from --
           while a minibuffer session previews other buffers, which
           would otherwise flip the segments (and repaint the bar with
           alternating images) on every preview.
  :font :font-size :line-pad :char-advance :char-advance-ratio
           :char-advance-ratio is the per-character advance as a FRACTION of
           the font size, and is the one to set when a line names its own
           :font -- it is a property of that family, so it stays right as the
           font size changes.  :char-advance pins the advance in pixels
           instead, for one family at one size.
  :pad :pad-y :margin :margin-y :right-margin
           Spacing, in the CSS sense.  :margin/:margin-y are OUTSIDE the
           background -- clear space separating this bar from its neighbour,
           and :margin also narrows the painted background itself.
           :pad/:pad-y (and :right-margin, whose name predates this split)
           are INSIDE it, between the background edge and the content.  With
           no :background the two are indistinguishable; they diverge the
           moment a bar is painted.  Each vertical one takes a number or a
           cons (TOP . BOTTOM).  :line-pad is neither: it grows the space
           below EACH ROW, so it separates rows and pads the bottom but never
           the top.  Both layouts.
  :rule :rule-height :rule-margin
           A hairline along the TOP of the image: :rule is its colour (nil,
           the default, draws none), :rule-height its thickness in pixels
           (1), :rule-margin its inset from both edges (nil = :margin, so it
           lines up with the painted background).  It sits at the very top,
           outside :margin-y, marking the edge of the WINDOW rather than of
           the bar.  This is what the `:overline' of the face the image sits
           on cannot do: redisplay paints that across the face's whole
           extent, which for a window-width image is the whole window.
           Both layouts.
  :seg-shape :seg-slant
           `lines' only.  The background shape of an interactive segment --
           the chips a mode line is built from.  `round' (default), `square',
           `arrow' (a powerline chevron) or `slant' (a parallelogram);
           :seg-slant is how deep the angle cuts, in pixels (default ~0.3 of
           the row height).  See `svg-line--seg-box': the angle is cut into
           the segment, not added to it, so a shaped chip occupies exactly
           the width a rectangular one did.
  :lead :lead-x
           `wrap' only.  :lead is a short string drawn as a pill in the LEFT
           MARGIN -- the strip outside the item flow, which is otherwise
           empty -- at :lead-x pixels from the window edge (default :pad).
           Outside the flow on purpose: an item would be given a slot, and a
           slot that comes and goes shifts every tab with it.  Styled as a
           current tab.  Not in the hit-test map: it is an indicator, not a
           target.
  :foreground :background
  :active   a predicate; when present and false, inactive variants apply
  :inactive-foreground :inactive-background
  `wrap' only:
  :gap
  :current-foreground :current-background
  :modified-foreground :modified-background
  :tab-background   box behind every ordinary (non-current) tab, to delineate
                    inactive tabs like the built-in tab line (nil = transparent)
  :inactive-current-foreground :inactive-current-background
  :inactive-modified-foreground :inactive-modified-background
  :inactive-tab-background
Each styling value may be a literal or a zero-arg function,
evaluated on every render."
  (unless (plist-get spec :target)
    (error "Missing :target for svg-line %S" name))
  (unless (functionp (plist-get spec :content))
    (error "Missing :content function for svg-line %S" name))
  (let ((entry (or (svg-line--entry name) (list :saved nil))))
    (setq entry (plist-put entry :spec spec))
    (setq entry (plist-put entry :renderer (svg-line--renderer name)))
    (puthash name entry svg-line--registry))
  name)

(defun svg-line--install (name)
  "Install line NAME's renderer on its target, saving the prior value."
  (let* ((entry (svg-line--entry name))
         (spec (plist-get entry :spec))
         (sym (plist-get entry :renderer))
         (target (plist-get spec :target)))
    (pcase target
      ('tab-bar
       (setq entry (plist-put entry :saved (cons 'value tab-bar-format)))
       (setq tab-bar-format (list sym))
       (cl-pushnew name svg-line--tab-bar-lines)
       (svg-line--tab-bar-enable))
      ('mode-line
       (setq entry (plist-put entry :saved (cons 'value (default-value 'mode-line-format))))
       (setq-default mode-line-format `((:eval (,sym)))))
      ('header-line
       (setq entry (plist-put entry :saved (cons 'value (default-value 'header-line-format))))
       (setq-default header-line-format `((:eval (,sym)))))
      ('tab-line
       ;; tab-line-format is buffer-local in many buffers but always calls
       ;; the `tab-line-format' FUNCTION, so override that to catch them all.
       (setq entry (plist-put entry :saved (cons 'advice sym)))
       (advice-add 'tab-line-format :override sym))
      (_ (error "Unknown :target %S for svg-line %S" target name)))
    (puthash name entry svg-line--registry)
    (force-mode-line-update t)))

(defun svg-line--uninstall (name)
  "Restore line NAME's target to the value saved at install time."
  (let* ((entry (svg-line--entry name))
         (spec (plist-get entry :spec))
         (saved (plist-get entry :saved))
         (target (plist-get spec :target)))
    (when saved
      (pcase (cons target (car saved))
        (`(tab-bar . value)
         (setq tab-bar-format (cdr saved))
         (setq svg-line--tab-bar-lines (delq name svg-line--tab-bar-lines))
         (svg-line--tab-bar-disable))
        (`(mode-line . value)   (setq-default mode-line-format (cdr saved)))
        (`(header-line . value) (setq-default header-line-format (cdr saved)))
        (`(tab-line . advice)   (advice-remove 'tab-line-format (cdr saved))))
      (setq entry (plist-put entry :saved nil))
      (puthash name entry svg-line--registry))
    (force-mode-line-update t)))

;;;###autoload
(defun svg-line-active-p (name)
  "Return non-nil if line NAME is currently installed on its target."
  (and (svg-line--entry name)
       (plist-get (svg-line--entry name) :saved)
       t))

(defun svg-line--read-name (prompt &optional predicate)
  "Read a defined svg-line NAME (a symbol) from the minibuffer with PROMPT.
PREDICATE, if non-nil, filters the offered names (called with a symbol)."
  (let* ((names (cl-remove-if-not (or predicate #'always)
                                  (hash-table-keys svg-line--registry))))
    (unless names (user-error "No svg-lines defined (see `svg-line-define')"))
    (intern (completing-read prompt (mapcar #'symbol-name names) nil t))))

;;;###autoload
(defun svg-line-activate (name)
  "Activate the svg-line NAME on its target."
  (interactive (list (svg-line--read-name
                      "Activate svg-line: "
                      (lambda (n) (not (svg-line-active-p n))))))
  (unless (svg-line--entry name)
    (error "No svg-line named %S (use `svg-line-define')" name))
  (unless (svg-line-active-p name)
    (svg-line--install name))
  name)

;;;###autoload
(defun svg-line-deactivate (name)
  "Deactivate the svg-line NAME, restoring its target."
  (interactive (list (svg-line--read-name "Deactivate svg-line: "
                                          #'svg-line-active-p)))
  (when (svg-line-active-p name)
    (svg-line--uninstall name))
  name)

;;;###autoload
(defun svg-line-toggle (name)
  "Toggle the svg-line NAME on its target."
  (interactive (list (svg-line--read-name "Toggle svg-line: ")))
  (if (svg-line-active-p name)
      (svg-line-deactivate name)
    (svg-line-activate name)))

(provide 'svg-line)
;;; svg-line.el ends here
