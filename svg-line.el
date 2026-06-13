;;; svg-line.el --- SVG-rendered tab-bar, tab-line, header-line and mode-line -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Charlie Holland

;; Author: Charlie Holland <mister.chiply@gmail.com>
;; Maintainer: Charlie Holland <mister.chiply@gmail.com>
;; URL: https://github.com/chiply/svg-line
;; x-release-please-start-version
;; Version: 0.1.4
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

(defun svg-line--char-advance (explicit font-size)
  "Resolve the per-character advance to use.
EXPLICIT -- a spec's `:char-advance' or `svg-line-char-advance' -- wins when
non-nil; otherwise derive it from FONT-SIZE via `svg-line-char-advance-ratio'."
  (if explicit
      explicit
    (max 1 (round (* font-size svg-line-char-advance-ratio)))))

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

(defun svg-line--xml-escape (text)
  "Escape the XML metacharacters &, < and > in TEXT for SVG text content.
A private escaper so the package does not depend on svg.el internals
\(e.g. `svg--encode-text', whose stability is not guaranteed)."
  (replace-regexp-in-string
   "[&<>]"
   (lambda (m) (pcase m ("&" "&amp;") ("<" "&lt;") (">" "&gt;")))
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
         (attrs (list (cons 'x (plist-get props :x))
                      (cons 'y (plist-get props :y))
                      (cons 'font-family (plist-get props :font))
                      (cons 'font-size fz)
                      (cons 'fill (plist-get props :fill))
                      ;; keep inter-tspan spaces: SVG otherwise trims leading
                      ;; whitespace at a tspan boundary, swallowing the space
                      ;; after an enlarged glyph
                      (cons 'xml:space "preserve"))))
    (when (plist-get props :weight) (push (cons 'font-weight (plist-get props :weight)) attrs))
    (when (plist-get props :anchor) (push (cons 'text-anchor (plist-get props :anchor)) attrs))
    (let ((node (dom-node 'text (nreverse attrs))) (cur-dy 0))
      (dolist (run (svg-line--split-glyph-runs str))
        (let* ((glyphp (and (car run) (> scale 1.0)))
               (target (if glyphp shift 0))
               (dy (- target cur-dy)))
          (setq cur-dy target)
          (dom-append-child
           node (dom-node 'tspan
                          (append (when glyphp (list (cons 'font-size big)))
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

(defun svg-line--run-width (run char-advance fz)
  "Advance width in pixels of a single RUN.
Text advances by CHAR-ADVANCE per character; bars and pies derive their
size from the font size FZ."
  (pcase (car run)
    (:text (* (length (nth 1 run)) char-advance))
    (:seg  (* (length (nth 1 run)) char-advance))
    (:pie  (+ (round (* fz 0.76)) (round (* 0.3 fz))))   ; diameter + gap
    (:bar  (+ (nth 2 run) (round (* 0.3 fz))))
    (_ 0)))

(defun svg-line--runs-width (runs char-advance fz)
  "Total advance width in pixels of RUNS (for right alignment).
CHAR-ADVANCE and font size FZ are passed through to `svg-line--run-width'."
  (apply #'+ (mapcar (lambda (r) (svg-line--run-width r char-advance fz)) runs)))

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
                    (cw (* (length str) char-advance))
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
                 (svg-rectangle svg x top cw lh :fill (svg-line--color bg) :rx 3))
               (when hov
                 (svg-rectangle svg x top cw lh :fill hover-color :rx 3))
               (when (> (length str) 0)
                 (svg-line--add-text svg str :x x :y (+ top fz)
                                     :font font :font-size fz :fill fill :weight weight))
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
    (setq x (+ x (svg-line--run-width run char-advance fz))))
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

(defun svg-line--draw-clock (svg cx cy r color &optional accent)
  "Draw an analog clock face on SVG centred at CX,CY radius R, showing now.
COLOR is the rim/ticks/hour-hand colour; ACCENT (or COLOR) the minute hand."
  (let* ((tm (decode-time))
         (mn (decoded-time-minute tm))
         (hr (mod (decoded-time-hour tm) 12))
         (ma (* (/ mn 60.0) 2 float-pi))
         (ha (* (/ (+ hr (/ mn 60.0)) 12.0) 2 float-pi))
         (col (svg-line--color color))
         (acc (svg-line--color (or accent color))))
    (cl-flet ((hand (ang len w c)
                (svg-line svg cx cy (round (+ cx (* len (sin ang))))
                          (round (- cy (* len (cos ang))))
                          :stroke c :stroke-width w :stroke-linecap "round")))
      (svg-circle svg cx cy r :fill "none" :stroke col
                  :stroke-width (max 1 (round (* r 0.09))))
      (dotimes (i 12)
        (let* ((a (* (/ i 12.0) 2 float-pi)) (r1 (* r 0.80)) (r2 (* r 0.93)))
          (svg-line svg (round (+ cx (* r1 (sin a)))) (round (- cy (* r1 (cos a))))
                    (round (+ cx (* r2 (sin a)))) (round (- cy (* r2 (cos a))))
                    :stroke col :stroke-width (max 1 (round (* r 0.055))))))
      (hand ha (* r 0.50) (max 1 (round (* r 0.14))) col)
      (hand ma (* r 0.80) (max 1 (round (* r 0.09))) acc)
      (svg-circle svg cx cy (max 1 (round (* r 0.09))) :fill acc))))

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

;;;###autoload
(cl-defun svg-line-image (rows &key
                               (width 100)
                               (font (or svg-line-font (face-attribute 'default :family nil t)))
                               (font-size svg-line-font-size)
                               (line-pad svg-line-line-pad)
                               (pad 0)
                               (right-margin 0)
                               (char-advance svg-line-char-advance)
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
    (:seg STR PLIST); see `svg-line--render-runs'.
FONT, FONT-SIZE, LINE-PAD, PAD, FOREGROUND and BACKGROUND set the text
family, size, per-row vertical padding, left inset and colours.  An
interactive (:seg ...) run whose `:id' equals HOVERED gets a HOVER-COLOR
box; the placements of all such runs are left in `svg-line--lines-placements'
\(with row height in `svg-line--lines-lh') for click/hover hit-testing.
ICON, when non-nil, is a (usually Nerd-Font) glyph drawn ONCE at the left
spanning the FULL image height (a multi-row \"masthead\" icon); ICON-COLOR
sets its fill, ICON-WIDTH the horizontal space it reserves (default: the image
height, i.e. square) and ICON-SCALE its size as a fraction of the height.  The
left-aligned content is inset past it.  Returns an svg object."
  (let* ((foreground (svg-line--color foreground))
         (background (svg-line--color background))
         (hover-color (svg-line--color hover-color))
         (fz font-size)
         (char-advance (svg-line--char-advance char-advance fz))
         (lh (+ fz line-pad))
         (rx (max 0 (- width right-margin)))
         (height (max 1 (* lh (length rows))))
         (isz (and icon (max 1 (round (* height icon-scale)))))
         ;; Reserved icon width.  `square' reserves the full image height (a
         ;; square cell); an integer reserves that many pixels; otherwise
         ;; reserve only ~the ink width plus a small margin -- Nerd-Font icon
         ;; glyphs carry lots of empty em-box padding (ink is ~0.5 of the font
         ;; size), and to fill the height the glyph is scaled past it via
         ;; ICON-SCALE (the oversized em is clipped to the image).
         (iw (cond ((not icon) 0)
                   ((eq icon-width 'square) height)
                   ((numberp icon-width) icon-width)
                   (t (+ (round (* isz 0.55)) (round (* fz 0.12))))))
         (left-x0 (+ pad iw))
         (svg (svg-create width height))
         (svg-line--seg-acc nil))
    (when background (svg-rectangle svg 0 0 width height :fill background))
    ;; full-height masthead icon on the left, drawn once for the whole image.
    ;; The glyph's ink sits in the left ~half of its em box, so to centre the
    ;; ink within the reserved cell we shift the draw origin left by ~a quarter
    ;; of the em (clamped to PAD); for a tight cell this collapses to flush-left.
    (when icon
      ;; Empirically (for typical icon glyphs) the ink is ~0.51 of the em, its
      ;; vertical centre sits ~0.335 em above the baseline and its horizontal
      ;; centre ~0.255 em right of the origin; offset by those to centre the ink
      ;; in the reserved cell (clamped to PAD so a tight cell stays flush-left).
      (let ((svg-line-glyph-scale 1.0)   ; size the glyph explicitly, not via the run scale
            (ix (max pad (- (+ pad (/ iw 2)) (round (* isz 0.255))))))
        (svg-line--add-text svg icon
                            :x ix
                            :y (round (+ (/ height 2.0) (* isz 0.335)))
                            :font font :font-size isz
                            :fill (svg-line--color (or icon-color foreground)))))
    (cl-loop for row in rows
             for i from 0
             for top = (* lh i)
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
                           (cw (svg-line--runs-width cc char-advance fz)))
                      (svg-line--draw-runs svg cc (max pad (/ (- width cw) 2))
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
                      (svg-line--draw-runs svg rr (max pad (- rx (svg-line--runs-width rr char-advance fz)))
                                           top fz lh font char-advance foreground
                                           hovered hover-color))))))
    ;; Centred, row-spanning overlays drawn once over a row range, on top of
    ;; the rows (whose `:center' should be empty there to avoid collision).
    ;; SPEC: (:clock (ROW-A . ROW-B) COLOR ACCENT) or
    ;;       (:pie   (ROW-A . ROW-B) FRACTION FILL BG).  Rows 0-indexed, inclusive.
    (dolist (span spans)
      (when (consp span)
        (let* ((rng (nth 1 span))
               (a (if (consp rng) (car rng) 0))
               (b (if (consp rng) (cdr rng) (1- (length rows))))
               (sh (* lh (1+ (- b a))))
               (cx (/ width 2))
               (cy (round (+ (* lh a) (/ sh 2.0))))
               (r (max 3 (round (* (/ sh 2.0) 0.86)))))
          (pcase (car span)
            (:clock (svg-line--draw-clock svg cx cy r (or (nth 2 span) foreground)
                                          (nth 3 span)))
            (:pie   (svg-line--draw-pie-at svg cx cy r
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
                    (gw (round (* gsz 0.5)))       ; Terminess Mono glyph advance ~0.5em
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
\(an alist (LABEL . COMMAND) for right-click) and `:color'/`:face' (text fill).
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

(defun svg-line--wrap-place (items width char-advance gap lh &optional center)
  "Return placements (X TOP CW ITEM) for ITEMS in a `wrap' layout.
WIDTH bounds each row; CHAR-ADVANCE, GAP and LH set per-item width and row
height.  When CENTER is non-nil and the items all fit on a single row (no
wrapping), the row is centred horizontally within WIDTH.  Shared by drawing
\(`svg-line-wrap-image') and click hit-testing (`svg-line--seg-at') so both
agree on where each item sits."
  (let ((x 0) (row 0) (out nil))
    (dolist (it items)
      (let* ((label (car it))
             (cw (* (length label) char-advance))
             (w  (+ cw (* gap char-advance))))
        (when (and (> x 0) (> (+ x w) width))
          (setq x 0 row (1+ row)))
        (push (list x (* row lh) cw it) out)
        (setq x (+ x w))))
    (setq out (nreverse out))
    ;; centre a single (un-wrapped) row: shift every placement right by half
    ;; the slack, so few tabs sit centred rather than flush-left.
    (when (and center out
               (= 0 (apply #'max 0 (mapcar (lambda (p) (nth 1 p)) out))))
      (let* ((rowwidth (apply #'max 0 (mapcar (lambda (p) (+ (nth 0 p) (nth 2 p))) out)))
             (offset (/ (- width rowwidth) 2)))
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
                                     (gap 3)
                                     (foreground "#000000")
                                     (background nil)
                                     (current-foreground nil)
                                     (current-background nil)
                                     (modified-foreground nil)
                                     (modified-background nil)
                                     (hovered nil)
                                     (hover-color nil)
                                     (center nil))
  "Build a `wrap'-layout SVG from ITEMS, a list of (LABEL . STATE).
Items flow left-to-right and wrap onto new rows at WIDTH.  GAP is the
inter-item gap in character widths.  FONT, FONT-SIZE, LINE-PAD,
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
`:id' equals HOVERED gets a HOVER-COLOR box.  Items with `:help'/`:action'/
`:menu' become image map hot-spots (per-item help-echo + hand pointer)."
  (let* ((foreground (svg-line--color foreground))
         (background (svg-line--color background))
         (current-foreground (svg-line--color current-foreground))
         (current-background (svg-line--color current-background))
         (modified-foreground (svg-line--color modified-foreground))
         (modified-background (svg-line--color modified-background))
         (hover-color (svg-line--color hover-color))
         (fz font-size)
         (char-advance (svg-line--char-advance char-advance fz))
         (lh (+ fz line-pad))
         (placements (svg-line--wrap-place items width char-advance gap lh center))
         (height (max 1 (apply #'max lh (mapcar (lambda (p) (+ (nth 1 p) lh)) placements))))
         (svg (svg-create width height))
         (map nil))
    (when background (svg-rectangle svg 0 0 width height :fill background))
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
                           (hoveredp  hover-color)))
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

(defun svg-line--width (spec)
  "Resolve the pixel width for SPEC."
  (let ((w (or (plist-get spec :width)
               (if (eq (plist-get spec :target) 'tab-bar) 'frame 'window))))
    (max 1 (pcase w
             ('frame (frame-inner-width))
             ('window (window-pixel-width))
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
  "Scale pixel SIZE by the current text-scale factor (integer result)."
  (round (* size (svg-line--text-scale))))

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
     :right-margin (svg-line--scaled (svg-line--opt spec :right-margin 0))
     ;; nil lets `svg-line-image' derive the advance from the (scaled) font
     ;; size; an explicit value is scaled to match.
     :char-advance (let ((e (or (svg-line--opt spec :char-advance nil)
                                svg-line-char-advance)))
                     (and e (* e sc)))
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
                         :gap (svg-line--opt spec :gap 3)
                         :foreground (funcall pick :foreground :inactive-foreground "#000000")
                         :background (funcall pick :background :inactive-background)
                         :current-foreground (funcall pick :current-foreground :inactive-current-foreground)
                         :current-background (funcall pick :current-background :inactive-current-background)
                         :modified-foreground (funcall pick :modified-foreground :inactive-modified-foreground)
                         :modified-background (funcall pick :modified-background :inactive-modified-background)
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
hovered id; on leaving, clear it once."
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
        (ignore-errors (funcall show-help-function help))
        (setq svg-line--tab-bar-hover-was-over t))
       (svg-line--tab-bar-hover-was-over
        (ignore-errors (funcall show-help-function nil))
        (setq svg-line--tab-bar-hover-was-over nil))))))

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
          svg-line--tab-bar-hover-was-over nil)))

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
  :font :font-size :line-pad :pad :right-margin :char-advance
  :foreground :background
  :active   a predicate; when present and false, inactive variants apply
  :inactive-foreground :inactive-background
  `wrap' only:
  :gap
  :current-foreground :current-background
  :modified-foreground :modified-background
  :inactive-current-foreground :inactive-current-background
  :inactive-modified-foreground :inactive-modified-background
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
