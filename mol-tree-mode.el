;;; mol-tree-mode.el --- Major mode for editing $mol tree files

;; Copyright (C) 2021 Olexandr Sydorchuk

;; Author: Olexandr Sydorchuk <olexandr.syd@gmail.com>
;; Package-Requires: ((emacs "24.3"))
;; Keywords: major mode data
;; Version: 0.9.0
;; URL: https://github.com/osv/mol-tree-mode

;; This file is not part of Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along
;; with this program; if not, write to the Free Software Foundation, Inc.,
;; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

;;; Commentary:

;; This is a major mode for editing files in the $mol tree data
;; serialization format.
;; See https://github.com/nin-jin/tree.d for more info about this file formal.
;; See https://github.com/hyoo-ru/mam_mol/blob/master/view for .view.tree format
;;
;; Indentation is inspired by sws-mode.el

;;; Installation:

;; To install, just drop this file into a directory in your
;; `load-path' and (optionally) byte-compile it.  To automatically
;; handle files ending in '.tree', add something like:
;;
;;    (require 'mol-tree-mode)
;;    (add-to-list 'auto-mode-alist '("\\.tree\\'" . mol-tree-mode))
;;
;; to your .emacs file.
;;
;; Hotkeys:
;; S-tab, backtab - indent
;; C-S-right, C-S-left - shift region by 1 tab

;;; Known Bugs:

;;; Code:


;; User definable variables

;;;###autoload
(defgroup mol-tree nil
  "Support for the mol-tree serialization format"
  :group 'languages
  :prefix "mol-tree-")

(defcustom mol-tree-mode-hook nil
  "*Hook run by `mol-tree-mode'."
  :type 'hook
  :group 'mol-tree)

(defface mol-tree-error-face
   '((((class color)) (:background "red" :foreground "black"))
     (t (:reverse-video t)))
  "Face to use for highlighting white/tab space error in $mol .tree files."
  :group 'faces
  :group 'mol-tree)


;; Constants

(defconst mol-tree-fl-raw-re
  "\\\\.*"
  "Regexp for syntax highlighting raw data or string started from backslash.")

(defconst mol-tree-fl-comment-re
  "\\(-[ \t].*\\|-\\)$"
  "Regexp for syntax highlighting comment block.")

(defconst mol-tree-fl-error-space-re
  "^[ \t]*?\\([ ]+\\)"
  "Regexp mathing a whitespace using in begining of line.")

(defconst mol-tree-fl-error-tabs-re
  "\\w\\([\t]+\\|  +\\)"
  "Regexp mathing a tabs after literal.")

(defconst mol-tree-fl-array-type-re
  "\\W\\([/]\\)\\w"
  "Regexp mathing word after array.")

(defconst mol-tree-fl-symbols-re
  "\\W\\([*^/]\\)\\W"
  "Regexp matching array, hash: *, /, ^.")

(defconst mol-tree-fl-builtin-values-re
  "\\b\\(true\\|false\\|null\\|NaN\\)\\b"
  "Regexp matching a builtin values like true, false, null.")


;; Font-lock support

(defvar mol-tree-font-lock-keywords
  `(
    (,mol-tree-fl-builtin-values-re . font-lock-builtin-face)
    (,mol-tree-fl-symbols-re . (1 font-lock-keyword-face))
    (,mol-tree-fl-array-type-re . (1 font-lock-keyword-face))
    ;; errors
    (,mol-tree-fl-error-space-re 1 'mol-tree-error-face t)
    (,mol-tree-fl-error-tabs-re 1 'mol-tree-error-face t)
    ;; comment started with "-"
    (,mol-tree-fl-comment-re 0 'font-lock-comment-face t)
    ;; string
    (,mol-tree-fl-raw-re 0 'font-lock-string-face t))
  "Font lock keywords for mol-tree mode.")


;; Indentation and electric keys

(defmacro mol-tree-line-as-string ()
  "Return the current line as a string."
  `(buffer-substring (point-at-bol) (point-at-eol)))

(defun mol-tree-previous-indentation ()
  "Gets indentation of previous line."
  (save-excursion
    (forward-line -1)
    (if (bobp) 0
      (progn
        (while (and (looking-at "^[ \t]*$") (not (bobp))) (forward-line -1))
        (current-indentation)))))

(defun mol-tree-max-indent ()
  "Calculates max indentation."
  (+ (mol-tree-previous-indentation) tab-width))

(defun mol-tree-empty-line-p ()
  "If line is completely empty."
  (= (point-at-bol) (point-at-eol)))

(defun mol-tree-point-to-bot ()
  "Move point to beginning of text."
  (beginning-of-line-text))

(defun mol-tree-do-indent-line ()
  "Perform line indentation."
  ;;if we are not tabbed out past max indent
  (if (mol-tree-empty-line-p)
      (indent-to (mol-tree-max-indent))
    (if (< (current-indentation) (mol-tree-max-indent))
        (indent-to (+ (current-indentation) tab-width))
      ;; if at max indent move text to beginning of line
      (progn
        (beginning-of-line)
        (delete-horizontal-space)))))

(defun mol-tree-indent-line ()
  "Indent current line."
  (interactive)
  (if (eq this-command 'indent-for-tab-command)
      (if mark-active
          (mol-tree-indent-region (region-beginning) (region-end))
        (if (mol-tree-at-bot-p)
            (mol-tree-do-indent-line)
          (mol-tree-point-to-bot)))
    (indent-to (mol-tree-previous-indentation))))

(defun mol-tree-at-bol-p ()
  "If point is at beginning of line."
  (interactive)
  (= (point) (point-at-bol)))

(defun mol-tree-at-bot-p ()
  "If point is at beginning of text."
  (= (point) (+
              (if indent-tabs-mode
                  (/ (current-indentation) tab-width)
                (current-indentation))
              (point-at-bol))))

(defun mol-tree-indent-to (num)
  "Force indentation to level including those below current level."
  (save-excursion
    (beginning-of-line)
    (delete-horizontal-space)
    (indent-to num)))

(defun mol-tree-move-region (begin end prog)
  "Moves left is dir is null, otherwise right. prog is '+ or '-."
  (save-excursion
    (let (first-indent indent-diff
	  (num-lines-indented (count-words-region begin end)))
      (goto-char begin)
      (setq first-indent (current-indentation))
      (mol-tree-indent-to
       (funcall prog first-indent tab-width))
      (setq indent-diff (- (current-indentation) first-indent))
      (forward-line 1)
      ;; move other lines based on movement of first line
      (dotimes (i (- num-lines-indented 1))
	(mol-tree-indent-to (+ (current-indentation) indent-diff))
	(forward-line 1)))))

(defun mol-tree-indent-region (begin end)
  "Indents the selected region."
  (interactive)
  (mol-tree-move-region begin end '+))


;; shift left or rigth block

(defun mol-tree-shift-region (distance)
  "Move region left or right."
  (let ((mark (mark)))
    (save-excursion
      (indent-rigidly (region-beginning) (region-end) distance)
      (push-mark mark t t)
      ;; Tell the command loop not to deactivate the mark
      ;; for transient mark mode
      (setq deactivate-mark nil))))

(defun mol-tree-shift-right ()
  "Move selected region to the right."
  (interactive)
  (mol-tree-shift-region tab-width))

(defun mol-tree-shift-left ()
  "Move selected region to the left."
  (interactive)
  (mol-tree-shift-region (- tab-width)))


;; Mode setup

(defvar mol-tree-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [S-tab] 'mol-tree-dendent-line)
    (define-key map [backtab] 'mol-tree-dendent-line)
    (define-key map [C-S-right] 'mol-tree-shift-right)
    (define-key map [C-S-left] 'mol-tree-shift-left)
    map)
  "Keymap used in `mol-tree-mode' buffers.")

(defvar mol-tree-mode-syntax-table
  (let ((syntax-table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" syntax-table)
    (modify-syntax-entry ?? "." syntax-table)
    (modify-syntax-entry ?! "." syntax-table)
    syntax-table)
  "Syntax table in use in `mol-tree-mode' buffers.")

;;;###autoload
(define-derived-mode mol-tree-mode fundamental-mode "$mol Tree"
  "Major mode to edit $mol tree files.

\\{mol-tree-mode-map}"
  :syntax-table mol-tree-mode-syntax-table

  (setq font-lock-defaults '(mol-tree-font-lock-keywords))

  (setq-local indent-line-function 'mol-tree-indent-line)
  (setq-local indent-region-function 'mol-tree-indent-region)

  ;; ensure using tabs for indention only
  (setq-local indent-tabs-mode t)
  (setq-local tab-always-indent t))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.tree\\'" . mol-tree-mode))

(provide 'mol-tree-mode)

;;; mol-tree-mode.el ends here
