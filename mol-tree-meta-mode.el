;;; mol-tree-meta-mode.el --- Major mode for $mol .meta.tree
;;;
;;; URL: https://github.com/osv/mol-tree-mode
;;; Author: Sydorchuk Olexandr
;;;

;;; Code:

(require 'mol-tree-mode)

(defvar mol-tree-meta-font-lock-keywords
  `(
    ("^\\(pack\\|include\\|require\\|deploy\\)\\b" . (1 font-lock-builtin-face))
    ("^pack +\\(\\w+\\)\\b" . (1 font-lock-variable-name-face))
    ("^pack +\\w+ +\\(git\\)\\b" . (1 font-lock-function-name-face))
    ;; * ^ /
    (,mol-tree-fl-symbols-re . (1 font-lock-keyword-face))
    (,mol-tree-fl-array-type-re . (1 font-lock-keyword-face))
    ;; errors
    (,mol-tree-fl-error-space-re 1 'mol-tree-error-face t)
    (,mol-tree-fl-error-tabs-re 1 'mol-tree-error-face t)
    (,mol-tree-fl-comment-re 0 'font-lock-comment-face t)
    ;; strings
    (,mol-tree-fl-raw-re 0 'font-lock-string-face t)
    )
    "Font lock keywords for mol-tree-meta mode.")


;; Mode setup

;;;###autoload
(define-derived-mode mol-tree-meta-mode mol-tree-mode "$mol Meta Tree"
  "Major mode to edit $mol meta tree files.

\\{mol-tree-mode-map}"

  (setq font-lock-defaults '(mol-tree-meta-font-lock-keywords t))
  (setq-local comment-start "- ")
  (setq-local comment-start-skip "-+ *"))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.meta\\.tree\\'" . mol-tree-meta-mode))

(provide 'mol-tree-meta-mode)

;;; mol-tree-meta-mode.el ends here
