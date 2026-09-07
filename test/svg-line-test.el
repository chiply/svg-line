;;; svg-line-test.el --- ERT tests for svg-line -*- lexical-binding: t -*-

;; Run with:
;;   emacs -Q --batch -L .. -l svg-line-test.el -f ert-run-tests-batch-and-exit
;; from the test/ directory.

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path
             (file-name-directory
              (directory-file-name
               (file-name-directory
                (or load-file-name buffer-file-name)))))
(require 'svg-line)

;; Avoid depending on the batch frame's default font family.
(setq svg-line-font "Monospace")

;;;; Segment rendering

(ert-deftest svg-line/render-segments-mixed ()
  "Strings pass through, functions are called, others are ignored."
  (should (equal (svg-line-render-segments '("a" "b")) "ab"))
  (should (equal (svg-line-render-segments (list "x" (lambda () "y"))) "xy"))
  (should (equal (svg-line-render-segments (list (lambda () nil) 'ignored "z")) "z")))

(ert-deftest svg-line/item->string-menu-items ()
  "A menu-item or list of menu-items normalises to its display string."
  (should (equal (svg-line--item->string '(key menu-item "hi" ignore)) "hi"))
  (should (equal (svg-line--item->string
                  '((k1 menu-item "a" ignore) (k2 menu-item "b" ignore)))
                 "ab"))
  (should (equal (svg-line--item->string nil) "")))

(ert-deftest svg-line/val-literal-or-function ()
  (should (equal (svg-line--val "x") "x"))
  (should (equal (svg-line--val (lambda () 42)) 42)))

(ert-deftest svg-line/color-normalisation ()
  "Colours are reduced to 6-digit hex SVG understands (display-independent)."
  (should (equal (svg-line--color "#57c071477e0a") "#57717e"))  ; 12-digit
  (should (equal (svg-line--color "#e7edf6") "#e7edf6"))          ; 6-digit pass-through
  (should (equal (svg-line--color "#abc") "#aabbcc"))             ; 3-digit
  (should (equal (svg-line--color nil) nil)))

;;;; lines layout

(ert-deftest svg-line/image-lines-height-and-anchor ()
  "Row count drives height; the right column is anchored to the end."
  (let ((svg (svg-line-image '(("L1" . "R1") ("L2" . ""))
                             :width 200 :font "Monospace" :font-size 10 :line-pad 4)))
    (should (= (dom-attr svg 'height) 28))      ; 2 rows * (10+4)
    (let ((texts (dom-by-tag svg 'text)))
      (should (= (length texts) 3))             ; L1, R1, L2 (empty R2 skipped)
      (should (member "end" (mapcar (lambda (tx) (dom-attr tx 'text-anchor)) texts))))))

(ert-deftest svg-line/image-lines-background-rect ()
  (should (dom-by-tag (svg-line-image '(("a" . "")) :width 100 :font "Monospace"
                                      :background "#112233")
                      'rect))
  (should-not (dom-by-tag (svg-line-image '(("a" . "")) :width 100 :font "Monospace")
                          'rect)))

(ert-deftest svg-line/image-center-row-vector ()
  "A vector row [L C R] draws the middle centred (text-anchor=middle at WIDTH/2)."
  (let* ((svg (svg-line-image (list (vector "L" "MID" "R"))
                              :width 200 :font "Monospace" :font-size 10))
         (texts (dom-by-tag svg 'text))
         (mid (seq-find (lambda (tx) (equal (dom-attr tx 'text-anchor) "middle")) texts)))
    (should mid)
    (should (= (dom-attr mid 'x) 100))      ; width/2
    (should (= (length texts) 3))))         ; L, MID, R

(ert-deftest svg-line/row-segs-cons-and-plist ()
  "`svg-line--row-segs' parses both a (LEFT . RIGHT) cons and a centred plist."
  (should (equal (svg-line--row-segs '((a b) . (c))) '((a b) nil (c))))
  (should (equal (svg-line--row-segs '(:left (a) :center (m) :right (z)))
                 '((a) (m) (z)))))

;;;; wrap layout

(ert-deftest svg-line/wrap-wraps-onto-rows ()
  "More items in a narrow width produce more rows (taller image)."
  (let* ((items (mapcar (lambda (i) (cons (format "tab%d" i) nil))
                        (number-sequence 1 20)))
         (narrow (svg-line-wrap-image items :width 80 :font "Monospace"
                                      :font-size 10 :line-pad 4 :char-advance 8 :gap 1))
         (wide   (svg-line-wrap-image items :width 4000 :font "Monospace"
                                      :font-size 10 :line-pad 4 :char-advance 8 :gap 1)))
    (should (= (dom-attr wide 'height) 14))      ; all fit on one row
    (should (> (dom-attr narrow 'height) (dom-attr wide 'height)))))

(ert-deftest svg-line/wrap-center-single-row ()
  "With CENTER, items that fit on one row are shifted right (centred)."
  (let* ((items (list (cons "aa" nil) (cons "bb" nil)))
         (flush    (svg-line--wrap-place items 1000 "Monospace" 8 16 1 14 nil))
         (centered (svg-line--wrap-place items 1000 "Monospace" 8 16 1 14 t)))
    (should (= 0 (nth 0 (car flush))))         ; flush-left: first item at x=0
    (should (> (nth 0 (car centered)) 0))      ; centred: shifted right
    ;; the inter-item offset is preserved (whole row shifts by the same amount)
    (should (= (- (nth 0 (nth 1 centered)) (nth 0 (nth 0 centered)))
               (- (nth 0 (nth 1 flush))    (nth 0 (nth 0 flush)))))))

(ert-deftest svg-line/wrap-center-multi-row-stays-flush ()
  "CENTER only affects a single row; a wrapped layout keeps its flush-left flow."
  (let* ((items (mapcar (lambda (i) (cons (format "tab%d" i) nil))
                        (number-sequence 1 20)))
         (centered (svg-line--wrap-place items 80 "Monospace" 8 16 1 14 t)))
    (should (= 0 (nth 0 (car centered))))                       ; first item flush
    (should (> (apply #'max (mapcar (lambda (p) (nth 1 p)) centered)) 0)))) ; wrapped

(ert-deftest svg-line/wrap-current-box ()
  "A current item draws exactly one highlight rect (no full background)."
  (let ((svg (svg-line-wrap-image '(("a" . nil) ("b" . t))
                                  :width 1000 :font "Monospace"
                                  :current-background "#0000ff")))
    (should (= (length (dom-by-tag svg 'rect)) 1))))

(ert-deftest svg-line/wrap-modified-plist-state ()
  "A plist STATE marks `:modified'; it draws a box and uses MODIFIED-FOREGROUND."
  (let ((svg (svg-line-wrap-image
              '(("a" . nil) ("b" . (:current nil :modified t)))
              :width 1000 :font "Monospace" :font-size 10
              :foreground "#000000"
              :modified-foreground "#c1641e" :modified-background "#ffeedd")))
    ;; one rect for the modified item's box
    (should (= (length (dom-by-tag svg 'rect)) 1))
    ;; the modified label is drawn in the modified foreground
    (let ((fills (mapcar (lambda (tx) (dom-attr tx 'fill)) (dom-by-tag svg 'text))))
      (should (member "#c1641e" fills))
      (should (member "#000000" fills)))))

(ert-deftest svg-line/wrap-current-modified-accent-box ()
  "A current+modified item keeps its readable bold label but tints the box
with the modified accent so the unsaved state stays visible."
  (let ((svg (svg-line-wrap-image
              '(("a" . (:current t :modified t)))
              :width 1000 :font "Monospace"
              :current-foreground "#ffffff" :current-background "#2a4d77"
              :modified-foreground "#c1641e")))
    (let ((tx   (car (dom-by-tag svg 'text)))
          (rect (car (dom-by-tag svg 'rect))))
      ;; label stays readable (white, bold)
      (should (equal (dom-attr tx 'fill) "#ffffff"))
      (should (equal (dom-attr tx 'font-weight) "bold"))
      ;; box is tinted with the modified accent, not the plain current bg
      (should (equal (dom-attr rect 'fill) "#c1641e")))))

(ert-deftest svg-line/wrap-inactive-palette ()
  "With a false `:active' predicate, the wrap layout uses inactive colours."
  (svg-line-define 'test-wrap-inactive
    :target 'tab-line :layout 'wrap
    :content (lambda () '(("a" . t)))
    :active (lambda () nil)
    :current-background "#2a4d77"
    :inactive-current-background "#9aa9bd")
  (let* ((svg (svg-line--build-wrap (svg-line--spec 'test-wrap-inactive)))
         (rect (car (dom-by-tag svg 'rect))))
    (should (equal (dom-attr rect 'fill) "#9aa9bd"))))

;;;; runs (text / bars / pies)

(defvar svg-line-test--seg)
(ert-deftest svg-line/segments-bound-variable ()
  "A bound variable symbol segment renders its value (mode-line-format style)."
  (let ((svg-line-test--seg "VX"))
    (should (equal (svg-line-render-segments '(svg-line-test--seg)) "VX"))
    (should (equal (nth 1 (car (svg-line--render-runs '(svg-line-test--seg)))) "VX"))))

(ert-deftest svg-line/render-runs-lowers-tokens ()
  "Segments lower to text/bar runs, coalescing adjacent text."
  (let ((runs (svg-line--render-runs
               (list "a" "b"
                     (lambda () "c")
                     '(:svg-bar 0.5 40 "#222222" "#eeeeee")))))
    (should (equal (mapcar #'car runs) '(:text :bar)))
    (should (equal (nth 1 (nth 0 runs)) "abc"))))       ; adjacent text coalesced

(ert-deftest svg-line/lines-bar-run ()
  "A run-list side renders a progress bar (track + fill = 2 rects) and text."
  (let* ((left  (list (list :text "hi")))
         (right (list (list :bar 0.5 40 "#2a4d77" "#eeeeee")))
         (svg (svg-line-image (list (cons left right))
                              :width 400 :font "Monospace" :font-size 10
                              :pad 4 :char-advance 8)))
    (should (= (length (dom-by-tag svg 'rect)) 2))      ; bar: track + fill
    (should (string-match-p "hi" (dom-texts svg)))))

(ert-deftest svg-line/lines-pie-run ()
  "A partial :pie run draws a background circle plus a wedge path."
  (let ((svg (svg-line-image
              (list (cons "x" (list (list :pie 0.25 "#2a4d77" "#d4dcea"))))
              :width 300 :font "Monospace" :font-size 12 :char-advance 8)))
    (should (= (length (dom-by-tag svg 'circle)) 1))    ; background circle
    (should (= (length (dom-by-tag svg 'path)) 1))))    ; the wedge

(ert-deftest svg-line/lines-pie-full ()
  "A full :pie run draws two circles (background + fill) and no wedge."
  (let ((svg (svg-line-image
              (list (cons "x" (list (list :pie 1.0 "#2a4d77" "#d4dcea"))))
              :width 300 :font "Monospace" :font-size 12 :char-advance 8)))
    (should (= (length (dom-by-tag svg 'circle)) 2))
    (should (= (length (dom-by-tag svg 'path)) 0))))

;;;; interactive segments (lines layout)

(ert-deftest svg-line/seg-constructor ()
  "`svg-line-seg' builds a :svg-seg form, and returns nil for empty text."
  (should (equal (svg-line-seg "hi" :id 'x :action #'ignore)
                 '(:svg-seg "hi" :id x :action ignore)))
  (should-not (svg-line-seg ""))
  (should-not (svg-line-seg nil)))

(ert-deftest svg-line/segs-splice-form ()
  "`svg-line-segs' groups items and drops nils."
  (should (equal (svg-line-segs (svg-line-seg "a" :id 1) nil " / "
                                (svg-line-seg "b" :id 2))
                 '(:svg-segs (:svg-seg "a" :id 1) " / " (:svg-seg "b" :id 2)))))

(ert-deftest svg-line/render-runs-seg-and-splice ()
  "A :svg-seg lowers to a :seg run (text + plist); :svg-segs splices; empty drops."
  (let ((runs (svg-line--render-runs
               (list "x"
                     (svg-line-seg "B" :id 'b :action #'ignore)
                     (svg-line-segs (svg-line-seg "c1" :id 'c1) "·"
                                    (svg-line-seg "c2" :id 'c2))
                     (lambda () (svg-line-seg "" :id 'empty))))))  ; empty -> dropped
    (should (equal (mapcar #'car runs) '(:text :seg :seg :text :seg)))
    ;; the leading literal text stays its own run; seg carries text + plist
    (should (equal (nth 1 (nth 1 runs)) "B"))
    (should (equal (plist-get (nth 2 (nth 1 runs)) :id) 'b))
    ;; the "·" separator between c1 and c2 is plain text
    (should (equal (nth 1 (nth 3 runs)) "·"))))

;;;; char-advance derivation

(ert-deftest svg-line/char-advance-default-auto ()
  "The default advance is nil (auto-derived), not a fixed pixel constant."
  (should (null svg-line-char-advance)))

(ert-deftest svg-line/char-advance-explicit-and-derived ()
  "An explicit advance wins; nil derives from font size and scales with it."
  (let ((svg-line-char-advance-ratio 0.6))
    (should (= 8 (svg-line--char-advance 8 15)))     ; explicit wins
    (should (= 9 (svg-line--char-advance nil 15)))   ; round(15*0.6)
    (should (= 12 (svg-line--char-advance nil 20)))  ; scales with size
    (should (= 1 (svg-line--char-advance nil 1)))))  ; never below 1

(ert-deftest svg-line/char-advance-derived-drives-layout ()
  "With no explicit advance, a :seg's recorded width tracks the derived advance."
  (let* ((svg-line-char-advance nil)
         (svg-line-char-advance-ratio 0.6)
         (svg-line--lines-placements nil)
         (seg (list (svg-line-seg "AAA" :id 'a)))   ; 3 chars
         (_ (svg-line-image (list (cons (svg-line--side seg) nil))
                            :width 400 :font "Monospace" :font-size 20))
         (p (car svg-line--lines-placements)))
    (should p)
    ;; placement is (X TOP W ITEM); W = 3 chars * derived advance (12) = 36
    (should (= (nth 2 p) (* 3 (svg-line--char-advance nil 20))))))

;;;; svg-line-segs-from-string

(ert-deftest svg-line/segs-from-string ()
  "Regions with a mouse-1 keymap become interactive segs; others stay text."
  (let* ((km (let ((m (make-sparse-keymap)))
               (define-key m [mode-line mouse-1] #'ignore) m))
         (str (concat "plain "
                      (propertize "click" 'keymap km 'help-echo "do it\nmore")))
         (group (svg-line-segs-from-string str 'test)))
    (should (eq (car group) :svg-segs))
    (let ((items (cdr group)))
      (should (member "plain " items))            ; no keymap -> literal text
      (let ((seg (cl-find-if (lambda (x) (and (consp x) (eq (car x) :svg-seg)))
                             items)))
        (should seg)
        (should (equal (nth 1 seg) "click"))
        (should (eq (plist-get (cddr seg) :action) #'ignore))
        (should (equal (plist-get (cddr seg) :help) "do it"))  ; first line only
        (should (equal (plist-get (cddr seg) :id) '(test . 1)))))))

(ert-deftest svg-line/segs-from-string-no-binding-stays-text ()
  "A keymap without a mouse-1 command leaves the region as plain text."
  (let* ((km (make-sparse-keymap))   ; no mouse-1 binding
         (str (propertize "label" 'keymap km))
         (group (svg-line-segs-from-string str)))
    (should (equal (cdr group) '("label")))))

(ert-deftest svg-line/segs-from-string-empty ()
  (should-not (svg-line-segs-from-string ""))
  (should-not (svg-line-segs-from-string nil)))

(ert-deftest svg-line/map-string-regions ()
  "The primitive yields (TEXT START HANDLER HELP) per keymap region."
  (let* ((km (let ((m (make-sparse-keymap)))
               (define-key m [mouse-1] #'ignore) m))
         (str (concat "ab" (propertize "CD" 'keymap km 'help-echo "hi")))
         (calls (svg-line-map-string-regions
                 str (lambda (text start handler help)
                       (list text start (and handler t) help)))))
    (should (equal calls
                   '(("ab" 0 nil nil)
                     ("CD" 2 t "hi"))))))

(ert-deftest svg-line/seg-placements-recorded ()
  "Drawing interactive segments records their placements (X TOP W ITEM)."
  (let* ((svg-line--lines-placements nil)
         (left (list (svg-line-seg "AA" :id 'a)))
         (right (list (svg-line-seg "BB" :id 'b)))
         (_ (svg-line-image (list (cons (svg-line--side left) (svg-line--side right)))
                            :width 400 :font "Monospace" :font-size 10
                            :char-advance 8))
         (ps svg-line--lines-placements))
    (should (= (length ps) 2))
    ;; each placement is (X TOP W (TEXT . PLIST)); ids round-trip
    (should (equal (sort (mapcar (lambda (p) (plist-get (cdr (nth 3 p)) :id)) ps)
                         (lambda (x y) (string< (symbol-name x) (symbol-name y))))
                   '(a b)))
    ;; left seg sits at x=0; right seg is anchored further right
    (let ((xa (car (cl-find 'a ps :key (lambda (p) (plist-get (cdr (nth 3 p)) :id)))))
          (xb (car (cl-find 'b ps :key (lambda (p) (plist-get (cdr (nth 3 p)) :id))))))
      (should (= xa 0))
      (should (> xb xa)))))

(ert-deftest svg-line/seg-hover-box ()
  "An interactive seg whose :id equals HOVERED draws a hover box; none otherwise."
  (let* ((side (svg-line--side (list (svg-line-seg "AA" :id 'a)
                                     " " (svg-line-seg "BB" :id 'b))))
         (rows (list (cons side nil))))
    (should (= 1 (length (dom-by-tag
                          (svg-line-image rows :width 400 :font "Monospace"
                                          :font-size 10 :char-advance 8
                                          :hovered 'a :hover-color "#445")
                          'rect))))
    (should (= 0 (length (dom-by-tag
                          (svg-line-image rows :width 400 :font "Monospace"
                                          :font-size 10 :char-advance 8
                                          :hovered 'zzz :hover-color "#445")
                          'rect))))))

(ert-deftest svg-line/placements-per-name ()
  "Placement storage is per-line-name and buffer-local (no cross-bar clobber)."
  (with-temp-buffer
    (svg-line--store-placements 'bar1 14 '((0 0 16 ("x" :id 1))))
    (svg-line--store-placements 'bar2 14 '((0 0 24 ("y" :id 2))))
    (should (equal (svg-line--placements-for 'bar1) '(14 (0 0 16 ("x" :id 1)))))
    (should (equal (svg-line--placements-for 'bar2) '(14 (0 0 24 ("y" :id 2)))))
    ;; updating one leaves the other intact
    (svg-line--store-placements 'bar1 14 '((0 0 99 ("z" :id 3))))
    (should (equal (svg-line--placements-for 'bar1) '(14 (0 0 99 ("z" :id 3)))))
    (should (equal (svg-line--placements-for 'bar2) '(14 (0 0 24 ("y" :id 2)))))))

(ert-deftest svg-line/seg-help-composed-and-tagged ()
  "An item's help joins help/action-help/menu hints and is tagged with its :id."
  (let* ((item (cons "buf.el" '(:id buf :help "buffer: buf.el"
                                    :action ignore :action-help "switch to it"
                                    :menu (("Kill" . ignore)))))
         (svg-line-help-face nil)             ; isolate from face propertization
         (h (svg-line--tab-help item)))
    (should (string-match-p "buffer: buf.el" h))
    (should (string-match-p "click to switch to it" h))
    (should (string-match-p "right-click for menu" h))
    (should (eq (get-text-property 0 'svg-line-tab h) 'buf))))

;;;; safety wrapper

(ert-deftest svg-line/safe-error-fallback ()
  "An error in the thunk yields a visible string, not a signal."
  (let ((svg-line--rendering nil)
        (svg-line--last-good (make-hash-table :test 'eq)))
    (let ((r (svg-line-safe 'x (lambda () (error "boom")))))
      (should (stringp r))
      (should (string-match-p "boom" r)))))

(ert-deftest svg-line/safe-reentrancy-returns-last-good ()
  "A re-entrant render returns the last good value rather than looping."
  (let ((svg-line--last-good (make-hash-table :test 'eq)))
    (puthash 'y "GOOD" svg-line--last-good)
    (let ((svg-line--rendering t))
      (should (equal (svg-line-safe 'y (lambda () "NEW")) "GOOD")))))

;;;; define + activate

(ert-deftest svg-line/define-creates-renderer ()
  (skip-unless (image-type-available-p 'svg))
  (svg-line-define 'test-line
    :target 'tab-bar :layout 'lines
    :content (lambda () '((("hello") . nil))))
  (should (svg-line--entry 'test-line))
  (should (fboundp 'svg-line--render-test-line))
  (let ((s (svg-line--render-test-line)))
    (should (stringp s))
    (should (get-text-property 0 'display s))))

(ert-deftest svg-line/define-requires-target-and-content ()
  (should-error (svg-line-define 'bad :content (lambda () nil)))
  (should-error (svg-line-define 'bad :target 'tab-bar)))

(ert-deftest svg-line/activate-and-deactivate-tab-bar ()
  "Activation installs the renderer on tab-bar-format; deactivation restores."
  (let ((tab-bar-format '(original)))
    (svg-line-define 'test-tb :target 'tab-bar :content (lambda () '((("x") . nil))))
    (svg-line-activate 'test-tb)
    (should (svg-line-active-p 'test-tb))
    (should (equal tab-bar-format '(svg-line--render-test-tb)))
    (svg-line-deactivate 'test-tb)
    (should-not (svg-line-active-p 'test-tb))
    (should (equal tab-bar-format '(original)))))

;;;; minibuffer freeze

(ert-deftest svg-line/freeze-returns-cached-during-minibuffer ()
  "A frozen target returns the window's pre-minibuffer render while active."
  (skip-unless (image-type-available-p 'svg))
  (let ((calls 0))
    (svg-line-define 'test-freeze
      :target 'tab-line :layout 'wrap
      :content (lambda () (cl-incf calls) (list (cons (format "tab%d" calls) t))))
    (clrhash svg-line--freeze-cache)
    (let ((svg-line-freeze-in-minibuffer '(tab-line)))
      ;; render once outside a minibuffer -> cached for this window
      (let ((first (svg-line--render 'test-freeze)))
        (should (stringp first))
        ;; "in a minibuffer": the cached string comes back, no re-render
        (cl-letf (((symbol-function 'active-minibuffer-window)
                   (lambda () (selected-window))))
          (should (eq (svg-line--render 'test-freeze) first)))
        (should (= calls 1))
        ;; outside again: re-renders and refreshes the cache
        (should-not (eq (svg-line--render 'test-freeze) first))
        (should (= calls 2))))))

(ert-deftest svg-line/freeze-disabled-target-renders-live ()
  "A target not in `svg-line-freeze-in-minibuffer' renders normally."
  (skip-unless (image-type-available-p 'svg))
  (let ((calls 0))
    (svg-line-define 'test-nofreeze
      :target 'tab-line :layout 'wrap
      :content (lambda () (cl-incf calls) (list (cons "t" t))))
    (let ((svg-line-freeze-in-minibuffer nil))
      (svg-line--render 'test-nofreeze)
      (cl-letf (((symbol-function 'active-minibuffer-window)
                 (lambda () (selected-window))))
        (svg-line--render 'test-nofreeze))
      (should (= calls 2)))))

(ert-deftest svg-line/context-buffer-pins-content-evaluation ()
  "Content runs with the :context-buffer current; nil/dead falls through."
  (skip-unless (image-type-available-p 'svg))
  (let ((ctx-buf (generate-new-buffer "ctx"))
        (seen nil))
    (unwind-protect
        (progn
          (svg-line-define 'test-ctx
            :target 'tab-bar :layout 'lines
            :context-buffer (lambda () ctx-buf)
            :content (lambda ()
                       (setq seen (current-buffer))
                       '((("x") . nil))))
          (with-temp-buffer
            (svg-line--render 'test-ctx)
            (should (eq seen ctx-buf)))
          ;; dead context buffer falls through to the current buffer
          (kill-buffer ctx-buf)
          (with-temp-buffer
            (let ((here (current-buffer)))
              (svg-line--render 'test-ctx)
              (should (eq seen here)))))
      (when (buffer-live-p ctx-buf) (kill-buffer ctx-buf)))))

(ert-deftest svg-line/freeze-uncached-window-renders-fresh ()
  "With no cached render for the window, a minibuffer render falls through."
  (skip-unless (image-type-available-p 'svg))
  (let ((calls 0))
    (svg-line-define 'test-freeze-miss
      :target 'tab-line :layout 'wrap
      :content (lambda () (cl-incf calls) (list (cons "t" t))))
    (clrhash svg-line--freeze-cache)
    (let ((svg-line-freeze-in-minibuffer '(tab-line)))
      (cl-letf (((symbol-function 'active-minibuffer-window)
                 (lambda () (selected-window))))
        (should (stringp (svg-line--render 'test-freeze-miss)))
        (should (= calls 1))
        ;; and it did NOT poison the cache while frozen
        (should-not (gethash (selected-window) svg-line--freeze-cache))))))

(provide 'svg-line-test)
;;; svg-line-test.el ends here

;;;; Spacing: margin (outside the background) vs padding (inside it)

(ert-deftest svg-line/pad-y-normalises ()
  "PAD-Y takes a number (both ends) or a cons (TOP . BOTTOM)."
  (should (equal (svg-line--pad-y 5) '(5 . 5)))
  (should (equal (svg-line--pad-y '(2 . 7)) '(2 . 7)))
  (should (equal (svg-line--pad-y nil) '(0 . 0))))

(ert-deftest svg-line/spacing-defaults-are-a-no-op ()
  "With every spacing option at 0 the image is what it was without them."
  (let ((rows '(("l" . "r") ("l2" . "r2"))))
    (should (= (dom-attr (svg-line-image rows :width 200 :font-size 10 :line-pad 4)
                         'height)
               (dom-attr (svg-line-image rows :width 200 :font-size 10 :line-pad 4
                                         :pad 0 :pad-y 0 :margin 0 :margin-y 0)
                         'height)))))

(ert-deftest svg-line/lines-margin-y-and-pad-y-both-add-height ()
  "Both insets grow the image; only PAD-Y is covered by the background."
  (let* ((rows '(("l" . "r") ("l2" . "r2")))
         (lh (+ 10 4))
         (base (svg-line-image rows :width 200 :font-size 10 :line-pad 4))
         (padded (svg-line-image rows :width 200 :font-size 10 :line-pad 4
                                 :pad-y 3 :margin-y 5 :background "#123456"))
         (bg (car (dom-by-tag padded 'rect))))
    (should (= (dom-attr base 'height) (* 2 lh)))
    (should (= (dom-attr padded 'height) (+ (* 2 lh) 6 10)))
    ;; the background starts below the margin and covers rows + padding only
    (should (= 5 (dom-attr bg 'y)))
    (should (= (+ (* 2 lh) 6) (dom-attr bg 'height)))))

(ert-deftest svg-line/lines-margin-narrows-the-background ()
  "MARGIN insets the painted background from both side edges."
  (let ((bg (car (dom-by-tag (svg-line-image '(("l" . "r")) :width 200 :font-size 10
                                             :margin 20 :background "#123456")
                             'rect))))
    (should (= 20 (dom-attr bg 'x)))
    (should (= 160 (dom-attr bg 'width)))))

(ert-deftest svg-line/lines-uneven-pad-y ()
  "A cons PAD-Y pads the two ends differently."
  (let* ((img (svg-line-image '(("l" . "r")) :width 200 :font-size 10 :line-pad 4
                              :pad-y '(2 . 9) :background "#123456"))
         (bg (car (dom-by-tag img 'rect))))
    (should (= (+ 14 2 9) (dom-attr img 'height)))
    (should (= 0 (dom-attr bg 'y)))))

(ert-deftest svg-line/wrap-pad-and-margin-place-items ()
  "In `wrap\='), X0 is MARGIN+PAD and rows start at MARGIN-Y+PAD-Y."
  (let ((p (car (svg-line--wrap-place (list (cons "aa" nil)) 1000 "Monospace" 8 16 1 14 nil 30 970 7))))
    (should (= 30 (nth 0 p)))
    (should (= 7 (nth 1 p)))))

(ert-deftest svg-line/wrap-pad-y-lets-pills-float ()
  "PAD-Y leaves background above and below the row, so item boxes float in it."
  (let* ((img (svg-line-wrap-image (list (cons "aa" t)) :width 200 :font-size 10
                                   :line-pad 4 :pad-y 4 :margin-y 2
                                   :background "#123456" :current-background "#abcdef"))
         (rects (dom-by-tag img 'rect))
         (bg (car rects))
         (pill (cadr rects)))
    (should (= (+ 14 8 4) (dom-attr img 'height)))
    (should (= 2 (dom-attr bg 'y)))
    ;; the pill sits strictly inside the background block
    (should (> (dom-attr pill 'y) (dom-attr bg 'y)))
    (should (< (+ (dom-attr pill 'y) (dom-attr pill 'height))
               (+ (dom-attr bg 'y) (dom-attr bg 'height))))))

(ert-deftest svg-line/wrap-margin-narrows-the-background ()
  "MARGIN narrows the wrap background and the flow is centred in what remains."
  (let ((bg (car (dom-by-tag (svg-line-wrap-image (list (cons "aa" nil)) :width 200
                                                  :font-size 10 :margin 25
                                                  :background "#123456")
                             'rect))))
    (should (= 25 (dom-attr bg 'x)))
    (should (= 150 (dom-attr bg 'width)))))

(ert-deftest svg-line/window-width-excludes-divider ()
  "The bar width drops the right divider, which `window-pixel-width\=' includes."
  (cl-letf (((symbol-function 'window-pixel-width) (lambda (&rest _) 500))
            ((symbol-function 'window-right-divider-width) (lambda (&rest _) 30))
            ((symbol-function 'window-scroll-bar-width) (lambda (&rest _) 0)))
    (should (= 470 (svg-line--window-width))))
  (cl-letf (((symbol-function 'window-pixel-width) (lambda (&rest _) 500))
            ((symbol-function 'window-right-divider-width) (lambda (&rest _) 0))
            ((symbol-function 'window-scroll-bar-width) (lambda (&rest _) 0)))
    (should (= 500 (svg-line--window-width)))))

;;;; Font metrics: advance, icon advance, and the ink box

(ert-deftest svg-line/char-advance-ratio-and-explicit ()
  "An explicit advance wins; otherwise the ratio scales with the font size."
  (should (= 8 (svg-line--char-advance 8 15 0.5)))
  (should (= 7.5 (svg-line--char-advance nil 15 0.5)))
  (should (= 11.6625 (svg-line--char-advance nil 15 0.7775)))
  ;; no ratio given falls back to the global one
  (let ((svg-line-char-advance-ratio 0.6))
    (should (= 9.0 (svg-line--char-advance nil 15 nil))))
  ;; NOT rounded: half a pixel per character is twenty pixels over a row
  (should (= 7.5 (svg-line--char-advance nil 15 0.5))))

(ert-deftest svg-line/string-width-counts-icons-larger ()
  "Icon glyphs advance by their enlarged size, text by the plain advance."
  (let ((icon (string #xF0614)))
    (let ((svg-line-glyph-scale 1.0))
      (should (= 22.5 (svg-line--string-width "abc" 7.5 15)))
      (should (= 30.0 (svg-line--string-width (concat "abc" icon) 7.5 15))))
    (let ((svg-line-glyph-scale 1.4))
      ;; the tspan is drawn at (round (* 15 1.4)) = 21, so the glyph advances
      ;; 7.5 * 21/15 = 10.5 -- not 7.5, which is what used to be reserved
      (should (= 10.5 (svg-line--glyph-advance 7.5 15)))
      (should (= 33.0 (svg-line--string-width (concat "ab" icon "c") 7.5 15)))
      ;; and a run reports the same width, so what follows it starts clear
      (should (= 33.0 (svg-line--run-width (list :text (concat "ab" icon "c")) "Monospace" 7.5 15)))
      (should (= 33.0 (svg-line--run-width (list :seg (concat "ab" icon "c") nil) "Monospace" 7.5 15))))))

(ert-deftest svg-line/right-aligned-runs-reach-the-margin ()
  "A right-aligned run ends at the right edge, whatever the font's advance."
  (let* ((icon (string #xF0614))
         (svg-line-glyph-scale 1.4)
         (label (concat "right " icon)))       ; 6 text characters + 1 icon
    (dolist (ratio '(0.5 0.7775))
      (let* ((ca (* 15 ratio))
             (want (- 600 (+ (* 6 ca) (svg-line--glyph-advance ca 15))))
             (svg (svg-line-image (list (cons nil (list (list :text label))))
                                  :width 600 :font "Monospace" :font-size 15
                                  :char-advance nil :char-advance-ratio ratio
                                  :right-margin 0))
             (x (dom-attr (car (dom-by-tag svg 'text)) 'x)))
        (should (< (abs (- x want)) 0.001))))))

(ert-deftest svg-line/glyph-ink-falls-back-without-measurement ()
  "With measuring off, the ink box is the documented constant."
  (let ((svg-line-measure-fonts nil))
    (svg-line-forget-font-metrics)
    (should (equal (svg-line-glyph-ink "x" "Monospace") svg-line-icon-ink-fallback))
    ;; and the reference font therefore needs no size correction
    (should (= 1.0 (svg-line--icon-size-factor "Monospace")))))

(ert-deftest svg-line/seg-font-falls-back-to-the-line-font ()
  "A segment with no `:font\=', or the line's own, lays out at the line advance."
  (let ((svg-line-measure-fonts nil))
    (svg-line-forget-font-metrics)
    (should (= 7.5 (svg-line--run-advance (list :seg "ab" nil) "A" 7.5 15)))
    (should (= 7.5 (svg-line--run-advance (list :seg "ab" '(:font "A")) "A" 7.5 15)))
    ;; a different family that cannot be measured keeps the line's advance,
    ;; which is right for metrically compatible cuts of one superfamily
    (should (= 7.5 (svg-line--run-advance (list :seg "ab" '(:font "B")) "A" 7.5 15)))
    ;; a plain text run never carries a font
    (should (= 7.5 (svg-line--run-advance (list :text "ab") "A" 7.5 15)))))

(ert-deftest svg-line/seg-font-is-drawn-and-laid-out ()
  "A segment names its family on its own `<text>\=', and the run after it clears."
  (let* ((svg-line-glyph-scale 1.0)
         (svg (svg-line-image
               (list (cons (list (list :seg "ab" '(:font "Other"))
                                 (list :text "cd"))
                           nil))
               :width 400 :font "Base" :font-size 15
               :char-advance nil :char-advance-ratio 0.5 :pad 0 :margin 0))
         (texts (dom-by-tag svg 'text)))
    (should (equal "Other" (dom-attr (nth 0 texts) 'font-family)))
    (should (equal "Base"  (dom-attr (nth 1 texts) 'font-family)))
    ;; "ab" is two characters at 7.5, so the text run starts at 15
    (should (= 15.0 (dom-attr (nth 1 texts) 'x)))))

(ert-deftest svg-line/font-size-normalisation-is-opt-out ()
  "With normalising off, `:font-size' is taken literally."
  (let ((svg-line-normalise-font-size nil))
    (should (= 15 (svg-line--font-size-for "Anything" 15)))
    (should (= 1.0 (svg-line--font-size-factor "Anything"))))
  ;; and an unmeasurable font is left alone rather than guessed at
  (let ((svg-line-measure-fonts nil))
    (svg-line-forget-font-metrics)
    (should (null (svg-line-cap-height "Anything")))
    (should (= 15 (svg-line--font-size-for "Anything" 15)))
    ;; the icon fallback describes an ICON and must not stand in for a cap
    (should (equal svg-line-icon-ink-fallback (svg-line-glyph-ink "M" "Anything")))
    (should (null (svg-line--measure-ink "M" "Anything")))))

(ert-deftest svg-line/masthead-ink-is-clamped-to-its-cell ()
  "A glyph whose ink is wider than its cell is shrunk, not left to overflow."
  (let* ((svg-line-measure-fonts nil)
         ;; the fallback ink is half an em wide, so a cell narrower than
         ;; (SIZE * 0.5) must pull the size down
         (svg (svg-line-image (list (cons "x" nil))
                              :width 400 :font "Monospace" :font-size 15
                              :char-advance-ratio 0.5
                              :icon "I" :icon-width 20 :icon-scale 4.0))
         (icon-text (car (dom-by-tag svg 'text)))
         (size (dom-attr icon-text 'font-size)))
    ;; 20px of cell at half an em of ink caps the size at 40
    (should (<= (* size (nth 0 svg-line-icon-ink-fallback)) 20))
    (should (<= size 40))))

(ert-deftest svg-line/normalise-basis-selects-and-blends ()
  "The normalisation basis picks cap, advance, or a blend of the two."
  (cl-letf (((symbol-function 'svg-line-cap-height) (lambda (_) 0.725))
            ((symbol-function 'svg-line-font-advance) (lambda (_) 0.7775)))
    (let* ((fc (/ svg-line-cap-height-reference 0.725))
           (fa (/ svg-line-advance-reference 0.7775)))
      (let ((svg-line-normalise-font-size 'cap))
        (should (< (abs (- (svg-line--font-size-factor "F") fc)) 1e-6)))
      (let ((svg-line-normalise-font-size t))     ; t is a synonym for cap
        (should (< (abs (- (svg-line--font-size-factor "F") fc)) 1e-6)))
      (let ((svg-line-normalise-font-size 'advance))
        (should (< (abs (- (svg-line--font-size-factor "F") fa)) 1e-6)))
      (let ((svg-line-normalise-font-size 0.5))
        (should (< (abs (- (svg-line--font-size-factor "F") (/ (+ fc fa) 2))) 1e-6)))
      (let ((svg-line-normalise-font-size nil))
        (should (= 1.0 (svg-line--font-size-factor "F"))))
      ;; out-of-range blends clamp rather than extrapolate
      (let ((svg-line-normalise-font-size 5.0))
        (should (< (abs (- (svg-line--font-size-factor "F") fa)) 1e-6)))))
  ;; the reference family is the fixed point under every basis
  (cl-letf (((symbol-function 'svg-line-cap-height)
             (lambda (_) svg-line-cap-height-reference))
            ((symbol-function 'svg-line-font-advance)
             (lambda (_) svg-line-advance-reference)))
    (dolist (mode '(cap advance 0.0 0.5 1.0))
      (let ((svg-line-normalise-font-size mode))
        (should (< (abs (- (svg-line--font-size-factor "Ref") 1.0)) 1e-6))))))

(ert-deftest svg-line/tracking-corrects-a-mis-advanced-font ()
  "The correction closes the renderer's error, and the width accounts for it."
  (cl-letf (((symbol-function 'svg-line-font-advance-native) (lambda (_) 0.62))
            ((symbol-function 'svg-line-font-advance-rendered) (lambda (_) 0.7775)))
    (let ((svg-line-correct-tracking t))
      ;; the effective advance is the font's own, not the renderer's
      (should (= 0.62 (svg-line-font-advance "F")))
      (should (< (abs (- (svg-line-tracking-ratio "F") -0.1575)) 1e-9))
      ;; the width reserved is the TRUE advance: what the renderer over-advances
      ;; by is trailing whitespace, never ink, so there is nothing to pad for
      (let ((svg-line-glyph-scale 1.0))
        (should (= 62.0 (svg-line--string-width (make-string 10 ?M) 6.2 10 "F")))
        (should (= 62.0 (svg-line--string-width (make-string 10 ?M) 6.2 10)))))
    ;; opting out hands back the renderer's own advance and no correction
    (let ((svg-line-correct-tracking nil))
      (should (= 0.7775 (svg-line-font-advance "F")))
      (should (= 0 (svg-line-tracking-ratio "F"))))))

(ert-deftest svg-line/tracking-reaches-the-svg ()
  "A tracked line carries letter-spacing on its text."
  (cl-letf (((symbol-function 'svg-line-font-advance-native) (lambda (_) 0.5))
            ((symbol-function 'svg-line-font-advance-rendered) (lambda (_) 0.6))
            ((symbol-function 'svg-line-cap-height) (lambda (_) 0.6167)))
    (let* ((svg-line-correct-tracking t)
           (svg-line-glyph-scale 1.0)
           (svg (svg-line-image (list (cons "hello" nil))
                                :width 300 :font "F" :font-size 20
                                :char-advance-ratio 0.5))
           (text (car (dom-by-tag svg 'text))))
      ;; -0.1 em at 20px (the cap basis leaves the size alone here)
      (should (< (abs (- (dom-attr text 'letter-spacing) -2.0)) 1e-6)))))

(ert-deftest svg-line/tracking-pulls-later-tspans-back ()
  "An icon does not space apart the text after it: each tspan gets a `dx\='."
  (cl-letf (((symbol-function 'svg-line-font-advance-native) (lambda (_) 0.5))
            ((symbol-function 'svg-line-font-advance-rendered) (lambda (_) 0.6)))
    (let* ((svg-line-correct-tracking t)
           (svg-line-glyph-scale 1.4)
           (svg (svg-create 200 40)))
      (svg-line--add-text svg (concat "ab" (string #xF0614) "cd")
                          :x 0 :y 20 :font "F" :font-size 10 :fill "#000")
      (let* ((spans (dom-by-tag svg 'tspan))
             (dxs (mapcar (lambda (n) (dom-attr n 'dx)) spans)))
        (should (= 3 (length spans)))
        ;; the first tspan flows from the text origin, the rest are pulled back
        ;; by the previous tspan's un-tracked gap -- at its own drawn size
        (should (null (nth 0 dxs)))
        (should (< (abs (- (nth 1 dxs) -1.0)) 1e-6))      ; after text at 10px
        (should (< (abs (- (nth 2 dxs) -1.4)) 1e-6))))))  ; after a glyph at 14px

(ert-deftest svg-line/no-tracking-emits-no-dx ()
  "A font the renderer advances correctly is left entirely alone."
  (cl-letf (((symbol-function 'svg-line-font-advance-native) (lambda (_) 0.5))
            ((symbol-function 'svg-line-font-advance-rendered) (lambda (_) 0.5)))
    (let* ((svg-line-glyph-scale 1.4)
           (svg (svg-create 200 40)))
      (svg-line--add-text svg (concat "ab" (string #xF0614) "cd")
                          :x 0 :y 20 :font "F" :font-size 10 :fill "#000")
      (dolist (n (dom-by-tag svg 'tspan))
        (should (null (dom-attr n 'dx)))
        (should (null (dom-attr n 'letter-spacing))))
      (should (null (dom-attr (car (dom-by-tag svg 'text)) 'letter-spacing))))))

(ert-deftest svg-line/xml-escape-covers-attribute-context ()
  "The escaper is safe for attribute values, not just text content.
The font probes format a family name into a quote-delimited attribute, so a
name carrying a quote would close it early and the rest would parse as
markup."
  (should (equal "Plain" (svg-line--xml-escape "Plain")))
  (should (equal "A&amp;B" (svg-line--xml-escape "A&B")))
  (should (equal "&lt;i&gt;" (svg-line--xml-escape "<i>")))
  (should (equal "Odd&quot;Name" (svg-line--xml-escape "Odd\"Name")))
  ;; nothing of the injected markup survives into the probe
  (let ((probe (svg-line--xml-escape "X\" onload=\"evil()")))
    (should-not (string-match-p "\"" probe))))
