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

(defcustom mips-interpreter "spim"
  "Path to the mips interpreter executable."
  :tag "MIPS Interpreter"
  :group 'mips
  :type 'string)

(defcustom mips-tab-width tab-width
  "Width of a tab for `mips-mode'."
  :tag "Tab width"
  :group 'mips
  :type 'integer)

(defcustom mips-baseline-column 0
  "Label definitions are aligned to this column."
  :tag "Indentation baseline."
  :group 'mips
  :type 'integer)

(defcustom mips-operator-column tab-width
  "Operators and directives are indented to this column."
  :tag "Operator column."
  :group 'mips
  :type 'integer)

(defcustom mips-operands-column tab-width
  "Operands such as registers and label references are indented
to this column."
  :tag "Register/reference column."
  :group 'mips
  :initialize (lambda (s v) (set-default s (* 2 mips-operator-column)))
  :type 'integer)

(defcustom mips-comments-column 30
  "Comments are indented to this column."
  :tag "Comment column."
  :group 'mips
  :type 'integer)

(defcustom mips-cycle-indent t
  "When non-nil, point jumps to the next imporatnt column when
  indenting. Can be used to loop through the various elements of
  a statement. "
  :tag "Cycle indent."
  :group 'mips
  :type 'boolean)

;;;

(defun mips--interpreter-buffer-name ()
  "Return a buffer name for the preferred mips interpreter"
  (format "*%s*" mips-interpreter))

(defun mips--interpreter-file-arg ()
  "Return the appropriate argument to accept a file for the
current mips interpreter"
  (cond ((equal mips-interpreter "spim") "-file")))

(defun mips--last-label-line ()
  "Returns the line of the last label"
  (save-excursion
    (previous-line)
    (end-of-line)
    (re-search-backward mips-re-label)
    (line-number-at-pos)))

(defun mips-run-buffer ()
  "Run the current buffer in a mips interpreter, and display the
output in another window"
  (interactive)
  (let ((tmp-file (format "/tmp/mips-%s" (file-name-base))))
    (write-region (point-min) (point-max) tmp-file nil nil nil nil)
    (mips-run-file tmp-file)
    (delete-file tmp-file)))

(defun mips-run-region ()
  "Run the current region in a mips interpreter, and display the
output in another window"
  (interactive)
  (let ((tmp-file (format "/tmp/mips-%s" (file-name-base))))
    (write-region (region-beginning) (region-end) tmp-file nil nil nil nil)
    (mips-run-file tmp-file)
    (delete-file tmp-file)))

(defun mips-run-file (&optional filename)
  "Run the file in a mips interpreter, and display the output in another window.
The interpreter will open filename. If filename is nil, it will
open the current buffer's file"
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

;;;;;;;;;;;;;;;;;
;; INDENTATION ;;
;;;;;;;;;;;;;;;;;

;; FIXME: tab indenting support is very unstable.

(defvar mips-line-re
  "\\(?:[ \t]*\\)?\\([a-zA-Z0-9_]*:\\)?\\(?:[ \t]+\\)?\\([\.a-zA-Z0-9_]*\\)?\\(?:[ \t]*\\)\\([^#\n^]+?\\)?\\(?:[ \t]*\\)?\\(#[^\n]*\\)?$"
  "An (excessive-looking) regexp to match and group possible MIPS
  assembly statements. Four capture groups will hold the tokens:
  1. `labeldef' 2. `operator' 3. `operands' 4. `comments'")

(defmacro defpadder (name column matching-group)
  "Define a function called NAME to indent one column of a
MIPS assembly statement. Place point at the first non-whitespace
character of REGEXP MATCHING-GROUP and pad it to COLUMN."
  `(defun ,name ()
     (string-match mips-line-re (thing-at-point 'line t))
     (when (wholenump (match-beginning ,matching-group))
       (move-to-column (match-beginning ,matching-group))
       (when (< (current-column) (match-end ,matching-group))
         (while (/= (current-column) ,column)
           (if (> (current-column) ,column)
             (if (member (preceding-char) '(11 32))
               (delete-backward-char 1)
               (progn (move-to-column ,column t)
                      (message "%s bumped into a wall!" ',name)))
             (insert (if indent-tabs-mode 11 32))))))))

(defpadder mips-labeldef mips-baseline-column 1)
(defpadder mips-operator mips-operator-column 2)
(defpadder mips-operands mips-operands-column 3)
(defpadder mips-comments mips-comments-column 4)

(defun mips-indent ()
  "Indent line at point and cycle the cursor."
  (interactive)
  (save-mark-and-excursion
   (mips-labeldef) (mips-operator)
   (mips-operands) (mips-comments))
  (when mips-cycle-indent
    (mips-cycle-cursor)))

(defun mips-dedent ()
  (interactive)
  (beginning-of-line)
  (skip-chars-forward " \t")
  (while (not (bolp))
    (if (member (preceding-char) '(11 32))
      (delete-backward-char 1 nil))))

(defun mips-cycle-cursor ()
  (cond ((or (bolp) (< (current-column) mips-operator-column))
         (move-to-column mips-operator-column t))
        ((< (current-column) mips-operands-column)
         (move-to-column mips-operands-column t))
        ((< (current-column) mips-comments-column)
         (move-to-column mips-comments-column t))
        ((eolp) (beginning-of-line))
        (t (end-of-line))))

;;;;;;;;;;;;;
;; FONTIFY ;;
;;;;;;;;;;;;;

(defvar mips-font-lock-keywords
  '( ;; Arithmetic insturctions
    "add" "sub" "addi" "addu" "subu" "addiu"
    ;; Multiplication/division
    "mult" "div" "rem" "multu" "divu" "mfhi" "mflo" "mul" "mulu" "mulo" "mulou"
    ;; Bitwise operations
    "not" "and" "or" "nor" "xor" "andi" "ori" "xori"
    ;; Shifts
    "sll" "srl" "sra" "sllv" "srlv" "srav"
    ;; Comparisons
    "seq" "sne" "sgt" "sgtu" "sge" "sgeu" "slt" "sltu" "slti" "sltiu"
    ;; Jump/branch
    "j" "jal" "jr" "jalr" "beq" "bne" "syscall"
    ;; Load/store
    "lui" "lb" "lbu" "lh" "lhu" "lw" "lwl" "lwr" "sb" "sh" "sw" "swl" "swr"
    ;; Concurrent load/store
    "ll" "sc"
    ;; Trap handling
    "break" "teq" "teqi" "tge" "tgei" "tgeu" "tgeiu" "tlt" "tlti" "tltu" "tltiu" "tne" "tnei" "rfe"
    ;; Pseudoinstructions
    "b" "bal" "bge" "bgt" "ble" "blt" "bgeu" "bleu" "bltu" "bgtu" "bgez" "blez" "bgtz" "bltz" "bnez"
    "beqz" "bltzal" "bgezal" "bgtu" "la" "li" "move" "movz" "movn" "nop" "clear"
    ;; Deprecated branch-hint pseudoinstructions
    "beql" "bnel" "bgtzl" "bgezl" "bltzl" "blezl" "bltzall" "bgezall"
    ;; Floating point instuctions
    ;; Arithmetic
    "add.s" "add.d" "sub.s" "sub.d" "mul.s" "mul.d" "div.s" "div.d"
    ;; Comparison
    "c.lt.s" "c.lt.d" "c.gt.s" "c.gt.d" "madd.s" "madd.d" "msub.s" "msub.d" "movt.s" "movt.d"
    "movn.s" "movn.d" "movz.s" "movz.d" "trunc.w.d" "trunc.w.s"
    ;; Conversion
    "cvt.s.d" "cvt.d.s"
    ;; Math
    "abs.s" "abs.d" "sqrt.s" "sqrt.d"
    ;; Load-store
    "l.s" "l.d" "s.s" "s.d"))

(defvar mips-font-lock-directives
  '(".align" ".ascii" ".asciiz" ".byte" ".data" ".double" ".extern" ".float"
    ".globl" ".half" ".kdata" ".ktext" ".space" ".text" ".word"))

(defvar mips-font-lock-defaults
  `((;; numbers
     ("\\_<-?[0-9]+\\>" . font-lock-constant-face)
     ;; stuff enclosed in "
     ("\"\\.\\*\\?" . font-lock-string-face)
     ;; labels
     ("[a-zA-Z_0-9]*:" . font-lock-function-name-face)
     (,(regexp-opt mips-font-lock-keywords 'words) . font-lock-keyword-face)
     ;; coprocessor load-store instructions
     ("[sl]wc[1-9]" . font-lock-keyword-face)
     (,(regexp-opt mips-font-lock-directives) . font-lock-preprocessor-face)
     ;; registers
     ("$\\(f?[0-2][0-9]\\|f?3[01]\\|[ft]?[0-9]\\|[vk][01]\\|a[0-3]\\|s[0-7]\\|[gsf]p\\|ra\\|at\\|zero\\)" . font-lock-type-face)
     ;; ("$\\([a-z0-9]\\{2\\}\\|zero\\)" . font-lock-constant-face)
     ;; special characters
     (":\\|,\\|;\\|{\\|}\\|=>\\|@\\|\\$\\|=" . font-lock-builtin-face))))

;;;;;;;;;;
;; MODE ;;
;;;;;;;;;;

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
