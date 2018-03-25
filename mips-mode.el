;;; mips-mode.el --- Major-mode for MIPS assembly
;;
;; Copyright (C) 2016-2018 Henrik Lissner
;;
;; Author: Henrik Lissner <http://github/hlissner>
;; Maintainer: Henrik Lissner <henrik@lissner.net>
;; Created: September 8, 2016
;; Modified: March 21, 2018
;; Version: 1.1.1
;; Package-Version: 20180321.211
;; Keywords: languages mips assembly
;; Homepage: https://github.com/hlissner/emacs-mips-mode
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; A major mode for MIPS Assembly, loosely based off haxor-mode. Written for the
;; MIPS Assembly track on exercism.io. A MIPS interpreter such as spim must be
;; installed for the code evaluation features.
;;
;;; Code:

(defgroup mips nil
  "Major mode for editing MIPS assembly."
  :prefix "mips-"
  :group 'languages
  :link '(url-link :tag "Github" "https://github.com/hlissner/emacs-mips-mode")
  :link '(emacs-commentary-link :tag "Commentary" "ng2-mode"))

(defcustom mips-tab-width tab-width
  "Width of a tab for `mips-mode'. It is also the opcode column."
  :tag "Tab width"
  :group 'mips
  :type 'integer)

(defcustom mips-operand-offset mips-tab-width
  "The width of separation between the opcode and the registers/immediate values."
  :tag "MIPS assembly opcode / operand offset."
  :group 'mips
  :type 'integer)

(defcustom mips-base-line 0
  "The base line from which indentation is started."
  :tag "Indent base line."
  :group 'mips
  :type 'integer)

(defcustom mips-comment-base-line 0
  "The column to which comments are lined up."
  :tag "Comment base line."
  :group 'mips
  :type 'integer)

(defcustom mips-interpreter "spim"
  "Path to the mips interpreter executable for running mips code with."
  :tag "MIPS Interpreter"
  :group 'mips
  :type 'string)

(defvar mips-keywords
  '(;; Arithmetic insturctions
    "add"
    "sub"
    "addi"
    "addu"
    "subu"
    "addiu"
    ;; Multiplication/division
    "mult"
    "div"
    "rem"
    "multu"
    "divu"
    "mfhi"
    "mflo"
    "mul"
    "mulu"
    "mulo"
    "mulou"
    ;; Bitwise operations
    "not"
    "and"
    "or"
    "nor"
    "xor"
    "andi"
    "ori"
    "xori"
    ;; Shifts
    "sll"
    "srl"
    "sra"
    "sllv"
    "srlv"
    "srav"
    ;; Comparisons
    "seq"
    "sne"
    "sgt"
    "sgtu"
    "sge"
    "sgeu"
    "slt"
    "sltu"
    "slti"
    "sltiu"
    ;; Jump/branch
    "j"
    "jal"
    "jr"
    "jalr"
    "beq"
    "bne"
    "syscall"
    ;; Load/store
    "lui"
    "lb"
    "lbu"
    "lh"
    "lhu"
    "lw"
    "lwl"
    "lwr"
    "sb"
    "sh"
    "sw"
    "swl"
    "swr"
    ;; Concurrent load/store
    "ll"
    "sc"
    ;; Trap handling
    "break"
    "teq"
    "teqi"
    "tge"
    "tgei"
    "tgeu"
    "tgeiu"
    "tlt"
    "tlti"
    "tltu"
    "tltiu"
    "tne"
    "tnei"
    "rfe"
    ;; Pseudoinstructions
    "b"
    "bal"
    "bge"
    "bgt"
    "ble"
    "blt"
    "bgeu"
    "bleu"
    "bltu"
    "bgtu"
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
    "movz"
    "movn"
    "nop"
    "clear"
    ;; Deprecated branch-hint pseudoinstructions
    "beql"
    "bnel"
    "bgtzl"
    "bgezl"
    "bltzl"
    "blezl"
    "bltzall"
    "bgezall"
    ;; Floating point instuctions
    ;; Arithmetic
    "add.s"
    "add.d"
    "sub.s"
    "sub.d"
    "mul.s"
    "mul.d"
    "div.s"
    "div.d"
    ;; Comparison
    "c.lt.s"
    "c.lt.d"
    "c.gt.s"
    "c.gt.d"
    "madd.s"
    "madd.d"
    "msub.s"
    "msub.d"
    "movt.s"
    "movt.d"
    "movn.s"
    "movn.d"
    "movz.s"
    "movz.d"
    "trunc.w.d"
    "trunc.w.s"
    ;; Conversion
    "cvt.s.d"
    "cvt.d.s"
    ;; Math
    "abs.s"
    "abs.d"
    "sqrt.s"
    "sqrt.d"
    ;; Load-store
    "l.s"
    "l.d"
    "s.s"
    "s.d"))

(defvar mips-defs
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

(defvar mips-font-lock-defaults
  `((;; numbers
     ("\\_<-?[0-9]+\\>" . font-lock-constant-face)
     ;; stuff enclosed in "
     ("\"\\.\\*\\?" . font-lock-string-face)
     ;; labels
     ("[a-zA-Z_0-9]*:" . font-lock-function-name-face)
     (,(regexp-opt mips-keywords 'words) . font-lock-keyword-face)
     ;; coprocessor load-store instructions
     ("[sl]wc[1-9]" . font-lock-keyword-face)
     (,(regexp-opt mips-defs) . font-lock-preprocessor-face)
     ;; registers
     ("$\\(f?[0-2][0-9]\\|f?3[01]\\|[ft]?[0-9]\\|[vk][01]\\|a[0-3]\\|s[0-7]\\|[gsf]p\\|ra\\|at\\|zero\\)" . font-lock-type-face)
     ;; ("$\\([a-z0-9]\\{2\\}\\|zero\\)" . font-lock-constant-face)
     ;; special characters
     (":\\|,\\|;\\|{\\|}\\|=>\\|@\\|\\$\\|=" . font-lock-builtin-face))))

;;
(defun mips--interpreter-buffer-name ()
  "Return a buffer name for the preferred mips interpreter"
  (format "*%s*" mips-interpreter))

(defun mips--interpreter-file-arg ()
  "Return the appropriate argument to accept a file for the current mips interpreter"
  (cond ((equal mips-interpreter "spim") "-file")))

(defun mips--last-label-line ()
  "Returns the line of the last label"
  (save-excursion
    (previous-line)
    (end-of-line)
    (re-search-backward mips-re-label)
    (line-number-at-pos)))

;;; INDENTATION LOGIC

(defvar mips-re-label "^[ \t]*[a-zA-Z_0-9]*:[ \t]?*")

(defvar mips-re-label-instruction "^\\([ \t]*[a-zA-Z0-9_]*:?[ \t]+\\)[a-zA-Z]+[^\n]")

(defvar mips-re-directive "^[ \t]?+\\.")

(defvar mips-re-comment "^[ \t]?+#")

(defun mips-line ()
  (thing-at-point 'line t))

(defun mips-line-label-p ()
  (string-match-p mips-re-label (mips-line)))

(defun mips-short-label-p ()
  (let* ((line (mips-line)) (pos (string-match ":" line)))
    (< (length (subseq line 0 pos)) mips-tab-width)))

(defun mips-mark-before-indent-column-p ()
  (< (current-column) mips-tab-width))

(defun mips-line-has-opcode-p ()
  (string-match-p mips-re-label-instruction (mips-line)))

(defun mips-line-has-register-p ()
  (string-match "\\$" (mips-line)))

(defun mips-empty-line-p ()
  (string-match-p "^[ \t]+$" (mips-line)))

(defun mips-operand-column ()
  (+ mips-operand-offset mips-tab-width))

;; indent
(defun mips-indent ()
  (interactive)
  (when (or (eobp) (and (bobp) (eobp)))
    (open-line 1))
  (cond ((mips-line-comment-p)
         (mips-indent-comment))
        ((mips-line-directive-p)
         (mips-indent-directive))
        ((mips-line-label-p)
         (mips-indent-label))
        (t (mips-indent-instruction))))

;; comment
(defun mips-line-comment-p ()
  (string-match-p mips-re-comment (mips-line)))

(defun mips-indent-comment ()
  (indent-line-to mips-comment-base-line))

;; directive
(defun mips-line-directive-p ()
  (string-match-p mips-re-directive (mips-line)))

(defun mips-indent-directive ()
  (save-mark-and-excursion
   (indent-line-to mips-tab-width))
  (move-to-column (mips-register-column) t))

;; labels, instructions
(defun mips-indent-label ()
  (save-mark-and-excursion
   (indent-line-to mips-base-line))
  (cond ((and (mips-short-label-p) (mips-line-label-only-p))
         (move-to-column mips-tab-width t))
        ((mips-line-label-only-p)
         (if (eolp)
           (move-to-column mips-tab-width t)
           (end-of-line)))
        ((and (mips-line-has-opcode-p)
              (mips-short-label-p))
         (mips-indent-opcode-column))
        (t (message "Unhandled format or long label."))))

(defun mips-indent-instruction ()
  (save-mark-and-excursion
   (indent-line-to mips-tab-width))
  (cond ((mips-empty-line-p)
         (move-to-column mips-tab-width t))
        (t (mips-indent-opcode-column))))

;; opcode
(defun mips-indent-opcode-column ()
  (beginning-of-line)
  (when (mips-line-label-p)
    (search-forward ":"))
  (re-search-forward "[\.a-zA-Z]")
  (backward-char)
  (pad tab-width 32)
  (if (mips-line-has-register-p)
    (mips-indent-operand-column)
    (progn
      (move-to-column (mips-operand-column) t)
      (eol-before-comment))))

;; operands
(defun mips-indent-operand-column ()
  (beginning-of-line)
  (search-forward "$")
  (backward-char)
  (pad (mips-register-column) 32)
  (eol-before-comment))

(defun eol-before-comment ()
  (let ((comment (string-match "\\#" (mips-line) (current-column))))
    (if comment
      (progn
        (move-to-column comment)
        (backward-word)
        (forward-word))
      (end-of-line))))

(defun pad (column &optional char)
  (while (< (current-column) column)
    (insert (or char 32))))

;;;

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
  (pop-to-buffer (mips--interpreter-buffer-name))
  (read-only-mode t)
  (help-mode))

(defun mips-goto-label (label)
  "Jump to the label entitled LABEL."
  (interactive "sGo to Label: ")
  (let ((orig-pt (point)))
    (beginning-of-buffer)
    (unless (re-search-forward (format "[ \t]*%s:" label))
      (goto-char orig-pt))))

(defun mips-goto-label-at-cursor ()
  "Jump to the label that matches the symbol at point."
  (interactive)
  (mips-goto-label (symbol-at-point)))

(defvar mips-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<backtab>") #'mips-dedent)
    (define-key map (kbd "C-c C-c")   #'mips-run-buffer)
    (define-key map (kbd "C-c C-r")   #'mips-run-region)
    (define-key map (kbd "C-c C-l")   #'mips-goto-label-at-cursor)
    map)
  "Keymap for `mips-mode'.")

;;;###autoload
(define-derived-mode mips-mode prog-mode "MIPS Assembly"
  "Major mode for editing MIPS assembler code."
  (setq font-lock-defaults mips-font-lock-defaults
        comment-start "#"
        comment-end ""
        indent-line-function 'mips-indent)
  (when mips-tab-width
    (setq tab-width mips-tab-width))
  (modify-syntax-entry ?#  "< b" mips-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" mips-mode-syntax-table))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mips\\'" . mips-mode))

(provide 'mips-mode)
;;; mips-mode.el ends here
