;;; mol-tree-autoload.el
;;;
;;; URL: https://github.com/osv/mol-tree-mode
;;; Author: Sydorchuk Olexandr
;;;

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.tree\\'" . mol-tree-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.view\\.tree\\'" . mol-tree-view-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.meta\\.tree\\'" . mol-tree-meta-mode))

;;; mol-tree-view-mode.el ends here
