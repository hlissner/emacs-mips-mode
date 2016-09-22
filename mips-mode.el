;;; mips-mode.el --- Major-mode for MIPS assembly
;;
;; Copyright (C) 2016 Henrik Lissner
;;
;; Author: Henrik Lissner <http://github/hlissner>
;; Maintainer: Henrik Lissner <henrik@lissner.net>
;; Created: September 8, 2016
;; Modified: September 9, 2016
;; Version: 1.0.1
;; Keywords: mips assembly
;; Homepage: https://github.com/hlissner/emacs-mips-mode
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; A major mode for MIPS Assembly based off [haxor-mode]. Written for the
;; MIPS Assembly track on exercism.io.
;;
;;; Code:

(defconst mips-keywords
  '(;; instructions
    "add"
    "sub"
    "addi"
    "addu"
    "subu"
    "addiu"
    ;;
    "mult"
    "div"
    "rem"
    "multu"
    "divu"
    "mfhi"
    "mflo"
    "mul"
    ;;
    "not"
    "and"
    "or"
    "nor"
    "xor"
    "andi"
    "ori"
    "xori"
    ;;
    "sll"
    "srl"
    "sra"
    "sllv"
    "srlv"
    "srav"
    ;;
    "slt"
    "sltu"
    "slti"
    "sltiu"
    ;;
    "j"
    "jal"
    "jr"
    "jalr"
    "beq"
    "bne"
    "syscall"
    ;;
    "lui"
    "lb"
    "lbu"
    "lh"
    "lhu"
    "lw"
    "sb"
    "sh"
    "sw"
    ;;
    "ll"
    "sc"

    ;; pseudo instructions;
    "b"
    "bal"
    "bge"
    "bgt"
    "ble"
    "blt"
    "bgez"
    "blez"
    "bgtz"
    "bltz"
    "bnez"
    "beqz"
    "bltzal"
    "bgezal"
    "bgtu"
    "la"
    "li"
    "move"
    "nop"
    "clear"

    ;; floating point instuctions
    "add.s"
    "sub.s"
    "mul.s"
    "div.s"
    "add.d"
    "sub.d"
    "mul.d"
    "div.d"
    "c.lt.s"
    "c.lt.d"
    ))

(defconst mips-defs
  '(".align"
    ".ascii"
    ".asciiz"
    ".byte"
    ".data"
    ".double"
    ".extern"
    ".float"
    ".globl"
    ".half"
    ".kdata"
    ".ktext"
    ".space"
    ".text"
    ".word"))

(defconst mips-font-lock-defaults
  `((;; numbers
     ("\\_<-?[0-9]+\\>" . font-lock-constant-face)
     ;; stuff enclosed in "
     ("\"\\.\\*\\?" . font-lock-string-face)
     ;; labels
     ("[a-zA-Z][a-zA-Z_0-9]*:" . font-lock-function-name-face)
     (,(regexp-opt mips-keywords 'words) . font-lock-keyword-face)
     ;; coprocessor load-store instructions
     ("[sl]wc[1-9]" . font-lock-keyword-face)
     (,(regexp-opt mips-defs 'words) . font-lock-preprocessor-face)
     ;; registers
     ("$\\(f?[0-2][0-9]\\|f?3[01]\\|[ft]?[0-9]\\|[vk][01]\\|a[0-3]\\|s[0-7]\\|[gsf]p\\|ra\\|at\\|zero\\)" . font-lock-type-face)
     ;; ("$\\([a-z0-9]\\{2\\}\\|zero\\)" . font-lock-constant-face)
     ;; special characters
     (":\\|,\\|;\\|{\\|}\\|=>\\|@\\|\\$\\|=" . font-lock-builtin-face))))

(defcustom mips-tab-width tab-width
  "Width of a tab for MIPS mode"
  :tag "Tab width"
  :type 'integer)

;;;###autoload
(define-derived-mode mips-mode prog-mode "MIPS Assembly"
  "Major mode for editing MIPS assembler code."
  (setq font-lock-defaults mips-font-lock-defaults)
  (when mips-tab-width
    (setq tab-width mips-tab-width))

  (setq comment-start "#")
  (setq comment-end "")

  (modify-syntax-entry ?# "< b" mips-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" mips-mode-syntax-table))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mips\\'" . mips-mode))

(provide 'mips-mode)
;;; mips-mode.el ends here
