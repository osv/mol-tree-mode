;;; mol-tree-view-mode.el --- Major mode for $mol .view.tree
;;;
;;; URL: https://github.com/osv/mol-tree-mode
;;; Author: Sydorchuk Olexandr
;;;

;;; Code:

(defcustom mol-tree-view-block-literal-search-lines 100
  "*Maximum number of lines to search for start of block literals."
  :type 'integer
  :group 'mol-tree)


;; Constants

(defconst mol-tree-view-modeblank-line-re "^ *$"
  "Regexp matching a line containing only (valid) whitespace.")

(defconst mol-tree-view-mode-block-literal-re
  "\\(?:^\\(?:--- \\)?\\|{\\|\\(?: *[-,] +\\)+\\) *\\(?:\\(?:[^-:,#!\n{\\[ ]\\|[^#!\n{\\[ ]\\S-\\)[^#\n]*? *: \\)?\t*\\(?:- .*\n\\|\n\\)"
  "Regexp matching a line beginning of comment block.")



;; (defun mol-tree-view-font-lock-block-literals (bound)
;;   "Find lines within block literals.
;; Find the next line of the first (if any) block literal after point and
;; prior to BOUND.  Returns the beginning and end of the block literal
;; line in the match data, as consumed by `font-lock-keywords' matcher
;; functions.  The function begins by searching backwards to determine
;; whether or not the current line is within a block literal.  This could
;; be time-consuming in large buffers, so the number of lines searched is
;; artificially limited to the value of
;; `mol-tree-view-block-literal-search-lines'."
;;   (if (eolp) (goto-char (1+ (point))))
;;   (unless (or (eobp) (>= (point) bound))
;;     (let ((begin (point))
;;           (end (min (1+ (point-at-eol)) bound)))
;;       (goto-char (point-at-bol))
;;       (while (and (looking-at mol-tree-view-modeblank-line-re)
;;                   (not (bobp)))
;;         (forward-line -1))
;;       (let ((nlines mol-tree-view-block-literal-search-lines)
;;             (min-level (current-indentation)))
;;         (forward-line -1)
;;         (while (and (/= nlines 0)
;;                     (/= min-level 0)
;;                     (not (looking-at mol-tree-view-mode-block-literal-re))
;;                     (not (bobp)))
;;           (setq nlines (1- nlines))
;;           (unless (looking-at mol-tree-view-modeblank-line-re)
;;             (setq min-level (min min-level (current-indentation))))
;;           (forward-line -1))
;;         (when (looking-at-p " *- ")
;;           (setq min-level (- min-level 2)))
;;         (cond
;;          ((and (< (current-indentation) min-level)
;;                (looking-at mol-tree-view-mode-block-literal-re))
;;           (goto-char end)
;;           (put-text-property begin end 'mol-tree-block-literal t)
;;           (set-match-data (list begin end))
;;           t)
;;          ((progn
;;             (goto-char begin)
;;             (re-search-forward (concat mol-tree-view-mode-block-literal-re
;;                                        " *\\(.*\\)\n")
;;                                bound t))
;;           (let ((range (nthcdr 2 (match-data))))
;;             (put-text-property (car range) (cadr range) 'mol-tree-block-literal t)
;;             (set-match-data range))
;;           t))))))

(defvar mol-tree-view-font-lock-keywords
  `(
    ;; (mol-tree-font-lock-block-literals 0 font-lock-comment-face)
    (,mol-tree-fl-builtin-values-re . font-lock-builtin-face)
    ("\\w\\([?!]\\)\\w" . (1 font-lock-comment-delimiter-face))
    ("[?!]\\(\\w+\\)" . (1 font-lock-constant-face))
    ;; Foo $bar
    ("\\([[:upper:]]\\w*\\).*?\\$" . (1 font-lock-variable-name-face))
    ;; $foo_bar
    ("$\\w+" . font-lock-type-face)
    ;; /foo - type of array
    ("\\W[/]\\(\\w+\\)" . (1 font-lock-type-face))
    ;; * ^ / <= <=> =>
    (,mol-tree-fl-symbols-re . (1 font-lock-keyword-face))
    (,mol-tree-fl-array-type-re . (1 font-lock-keyword-face))
    ("\\W\\(<=>\\|<=\\|=>\\)\\W" . (1 font-lock-keyword-face))
    ;; errors
    (,mol-tree-fl-error-space-re 1 'mol-tree-error-face t)
    (,mol-tree-fl-error-tabs-re 1 'mol-tree-error-face t)
    (,mol-tree-fl-comment-re 0 'font-lock-comment-face t)
    ("\\(\\w+\\(<=>\\|<=\\|=>\\)\\|\\(<=>\\|<=\\|=>\\)\\w+\\)" 0 'mol-tree-error-face)
    ;; strings
    (,mol-tree-fl-raw-re 0 'font-lock-string-face t)
    ("@\\W*\\\\.*$" 0 'font-lock-doc-face t))
    "Font lock keywords for mol-tree-view mode.")


;; Mode setup

;;;###autoload
(define-derived-mode mol-tree-view-mode mol-tree-mode "$mol View Tree"
  "Major mode to edit $mol tree files.

\\{mol-tree-mode-map}"

  (setq font-lock-defaults '(mol-tree-view-font-lock-keywords t))
  (setq-local comment-start "- ")
  (setq-local comment-start-skip "-+ *"))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.view\\.tree\\'" . mol-tree-view-mode))

(provide 'mol-tree-view-mode)

;;; mol-tree-view-mode.el ends here
