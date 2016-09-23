;;; mips-mode.el --- Major-mode for MIPS assembly
;;
;; Copyright (C) 2016 Henrik Lissner
;;
;; Author: Henrik Lissner <http://github/hlissner>
;; Maintainer: Henrik Lissner <henrik@lissner.net>
;; Created: September 8, 2016
;; Modified: September 22, 2016
;; Version: 1.1.0
;; Keywords: mips assembly
;; Homepage: https://github.com/hlissner/emacs-mips-mode
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; A major mode for MIPS Assembly based off [haxor-mode]. Written for the
;; MIPS Assembly track on exercism.io. A MIPS interpreter such as spim
;; must be installed for the code evaluation features.
;;
;;; Code:

(defgroup mips nil
  "Major mode for editing MIPS assembly"
  :prefix "mips-"
  :group 'languages
  :link '(url-link :tag "Github" "https://github.com/hlissner/emacs-mips-mode")
  :link '(emacs-commentary-link :tag "Commentary" "ng2-mode"))

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
  :group 'mips
  :type 'integer)

(defcustom mips-interpreter "spim"
  "Interpreter to run mips code in"
  :tag "MIPS Interpreter"
  :group 'mips
  :type 'string)

(defvar mips-map
  (let ((map (make-keymap)))
    (define-key map (kbd "<backtab>") 'mips-dedent)
    (define-key map (kbd "C-c C-c") 'mips-run-buffer)
    (define-key map (kbd "C-c C-r") 'mips-run-region)
    map)
  "Keymap for mips-mode")

(defun mips--interpreter-buffer-name ()
  "Return a buffer name for the preferred mips interpreter"
  (format "*%s*" mips-interpreter))

(defun mips--interpreter-file-arg ()
  "Return the appropriate argument to accept a file for the current mips interpreter"
  (cond ((equal mips-interpreter "spim") "-file")))

(defun mips--get-indent-level (&optional line)
  "Returns the number of spaces indenting the last label."
  (interactive)
  (- (save-excursion
       (goto-line (or line (line-number-at-pos)))
       (back-to-indentation)
       (current-column))
     (save-excursion
       (goto-line (or line (line-number-at-pos)))
       (beginning-of-line)
       (current-column))))

(defun mips--last-label-line ()
  "Returns the line of the last label"
  (save-excursion
    (previous-line)
    (end-of-line)
    (re-search-backward "^[ \t]*\\w+:")
    (line-number-at-pos)))

(defun mips-indent ()
  (interactive)
  (indent-line-to (+ mips-tab-width
                     (mips--get-indent-level (mips--last-label-line)))))

(defun mips-dedent ()
  (interactive)
  (indent-line-to (- (mips--get-indent-level) mips-tab-width)))

(defun mips-run-buffer ()
  "Run the current buffer in a mips interpreter, and display the output in another window"
  (interactive)
  (let ((tmp-file (format "/tmp/mips-%s" (file-name-base))))
    (write-region (point-min) (point-max) tmp-file nil nil nil nil)
    (mips-run-file tmp-file)
    (delete-file tmp-file)))

(defun mips-run-region ()
  "Run the current region in a mips interpreter, and display the output in another window"
  (interactive)
  (let ((tmp-file (format "/tmp/mips-%s" (file-name-base))))
    (write-region (region-beginning) (region-end) tmp-file nil nil nil nil)
    (mips-run-file tmp-file)
    (delete-file tmp-file)))

(defun mips-run-file (&optional filename)
  "Run the file in a mips interpreter, and display the output in another window.
The interpreter will open filename. If filename is nil, it will open the current
buffer's file"
  (interactive)
  (let ((file (or filename (buffer-file-name))))
    (when (buffer-live-p (get-buffer (mips--interpreter-buffer-name)))
      (kill-buffer (mips--interpreter-buffer-name)))
    (start-process mips-interpreter
                   (mips--interpreter-buffer-name)
                   mips-interpreter (mips--interpreter-file-arg) file))
  (switch-to-buffer-other-window (mips--interpreter-buffer-name))
  (read-only-mode t)
  (help-mode))

;;;###autoload
(define-derived-mode mips-mode prog-mode "MIPS Assembly"
  "Major mode for editing MIPS assembler code."

  (setq font-lock-defaults mips-font-lock-defaults)
  (when mips-tab-width
    (setq tab-width mips-tab-width))

  (setq comment-start "#")
  (setq comment-end "")

  (use-local-map mips-map)
  (setq indent-line-function 'mips-indent)

  (modify-syntax-entry ?# "< b" mips-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" mips-mode-syntax-table))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mips\\'" . mips-mode))

(provide 'mips-mode)
;;; mips-mode.el ends here
