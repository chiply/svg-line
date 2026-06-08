# svg-line

[![CI](https://github.com/chiply/svg-line/actions/workflows/ci.yml/badge.svg)](https://github.com/chiply/svg-line/actions/workflows/ci.yml)
[![License: GPL-3.0](https://img.shields.io/badge/License-GPL%203.0-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

Render the **tab-bar**, **tab-line**, **header-line** and **mode-line** as SVG
images instead of laid-out text. Supports Emacs 29.1+ (graphical only).

## Overview

An SVG image can be any height and is positioned at exact pixel coordinates, so
svg-line makes possible things the text engine cannot do uniformly:

- **Multi-line bars** of arbitrary height (a 3-row tab-bar, a 2-row breadcrumb
  header-line, a multi-row mode-line).
- **Per-line left / centre / right alignment** on *every* row — not just the
  last, and without the `:align-to`-on-a-non-final-line redisplay freeze.
- **Tab lines that wrap** overflowing tabs onto new rows instead of truncating
  or horizontally scrolling, with optional single-row centring.
- **Interactive indicators**: any element can carry a left-click action, a
  right-click menu, hover help and a hover highlight — across all four bars,
  including the otherwise–uncooperative tab-bar.

svg-line is the rendering **engine** only: it ships no content and no colours of
its own. You supply a `:content` function and styling, then bind it to a target.

## Installation

### With elpaca (use-package)

```elisp
(use-package svg-line
  :ensure (:host github :repo "chiply/svg-line"))
```

### With straight.el (use-package)

```elisp
(use-package svg-line
  :straight (:host github :repo "chiply/svg-line"))
```

### Manual

```elisp
(add-to-list 'load-path "/path/to/svg-line")
(require 'svg-line)
```

## Quick start

Define a line and activate it:

```elisp
(svg-line-define 'my-mode-line
  :target 'mode-line
  :layout 'lines
  :content (lambda ()
             ;; one or more rows; each is (LEFT . RIGHT) or a
             ;; (:left L :center C :right R) plist for a centred middle
             (list (cons (list (buffer-name))
                         (list (format-mode-line "%l:%c")))))
  :active  #'mode-line-window-selected-p
  :background (lambda () "#e7edf6")
  :foreground (lambda () "#2a4d77"))

(svg-line-activate 'my-mode-line)     ; M-x svg-line-deactivate / svg-line-toggle
```

Colour and font options accept a literal value **or** a zero-argument function
evaluated on every render, so theme-dependent colours live in your config and
the engine stays theme-agnostic.

## Layouts

- **`lines`** — rows of `(LEFT . RIGHT)` (or `[LEFT CENTER RIGHT]` / a
  `:left`/`:center`/`:right` plist). A segment is a string, a zero-argument
  function, a bound-variable symbol, or a token: `(:svg-bar FRAC W FILL BG)`,
  `(:svg-pie FRAC FILL BG)`, or an interactive `(:svg-seg TEXT . PLIST)` built
  with `svg-line-seg`. Used for the mode-line, header-line and tab-bar.
- **`wrap`** — a flow of `(LABEL . STATE)` items wrapped across rows, with
  per-item "current"/"modified" highlighting. Used for tab lines.

## Interactive indicators

Build a clickable segment with `svg-line-seg`:

```elisp
(svg-line-seg "buffer.el"
  :id 'ml-buffer                     ; unique hover/identity key
  :help "buffer: buffer.el"
  :action #'switch-to-buffer         ; left/middle click
  :action-help "switch buffer"       ; the "click to …" hint
  :menu '(("Save" . save-buffer)     ; right click
          ("Kill" . kill-this-buffer)))
```

For `wrap` lines, put `:id`/`:help`/`:action`/`:action-help`/`:menu` in each
item's STATE plist. Enable the hover highlight with `svg-line-hover-highlight`
(it needs `show-help-function` wired to call `svg-line--note-help`; see the
docstring). Clicks select the window they land in, and the tab-bar's clicks and
hover are wired up automatically when a `tab-bar` line is activated.

## License

GPL-3.0. See [LICENSE](LICENSE).
