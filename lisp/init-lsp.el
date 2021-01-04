;;;; lsp-mode --- Language Server Protocol (LSP) setup
;;;; Commentary:

;;;; Code:
(when (maybe-require-package 'lsp-mode)
  (require 'lsp-mode)
  (require 'lsp-solargraph))

(add-hook 'ruby-mode-hook #'lsp)

(provide 'init-lsp)

;;; init-lsp.el ends here
