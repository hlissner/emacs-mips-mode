[![MIT](https://img.shields.io/badge/license-MIT-green.svg)](./LICENSE)
[![MELPA](http://melpa.org/packages/mips-mode-badge.svg)](http://melpa.org/#/mips-mode)
[![MELPA Stable](http://stable.melpa.org/packages/mips-mode-badge.svg)](http://stable.melpa.org/#/mips-mode)

# mips-mode

An Emacs major mode for MIPS Assembly code, based off [haxor-mode]. Written
for the [MIPS Assembly track on exercism.io](http://exercism.io/languages/mips).

> MIPS Reference Sheet: http://www.cburch.com/cs/330/reading/mips-ref.pdf

## Installation

mips-mode is available on MELPA. To install:

`M-x package-install RET mips-mode`

## Usage

Use `(require 'mips-mode)`, it will set up itself.

Alternatively, for use-package users:

``` emacs-lisp
(use-package mips-mode :mode "\\.mips$")
```


[haxor-mode]: https://github.com/krzysztof-magosa/haxor-mode
