# Contributing to svg-line

Thanks for your interest in contributing! This guide covers everything you
need to get started.

## Prerequisites

- **Emacs 29.1+**
- **[Eask](https://emacs-eask.github.io/)** CLI
- A **graphical** Emacs (svg-line draws SVG images; it does nothing in `emacs -nw`)

## Setup

```sh
git clone https://github.com/chiply/svg-line.git
cd svg-line
eask install-deps --dev
```

## Development workflow

### Compile

```sh
eask compile
```

### Test

```sh
eask test ert test/svg-line-test.el
```

### Lint

CI runs all four linters — make sure they pass locally before pushing:

```sh
eask lint package
eask lint checkdoc
eask lint elisp-lint
eask lint relint
```

## Pull request process

1. Branch from `main`.
2. Use **[Conventional Commits](https://www.conventionalcommits.org/)**
   (`feat:`, `fix:`, `chore:`, etc.) — release-please uses these to
   automate versioning and changelogs.
3. Keep commits focused; one logical change per commit.
4. CI must pass (tests on Emacs 29.4, 30.2, and snapshot; lints on 29.4).
5. A maintainer review is required before merging.

## Code style

- Add `;;; -*- lexical-binding: t; -*-` to every source file.
- Prefix all public symbols with `svg-line-` and private symbols with
  `svg-line--`.
- Include a docstring for every public function and variable.
- Follow standard Emacs Lisp conventions (two-space body indent, etc.).

## License

This project is licensed under the **GNU General Public License v3.0**. By
submitting a contribution you agree that your work will be distributed under
the same license.
