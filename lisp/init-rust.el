;;; init-rust.el --- Support for the Rust language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package rust-mode
  :ensure t
  :config
  (setq rust-format-on-save t))

(use-package flymake-clippy
  :ensure t
  :hook (rust-mode . flymake-clippy-setup-backend))

(use-package flycheck-rust :ensure t)

(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; (use-package rustic
;;   :ensure t
;;   :config
;;   (setq lsp-eldoc-hook nil)
;;   (setq lsp-enable-symbol-highlighting nil)
;;   (setq lsp-signature-auto-activate nil)
;;   (setq rustic-format-on-save t)
;;   (setq rustic-lsp-client 'eglot)
;;   (add-hook 'rust-mode-hook
;;             (lambda ()
;;               (setq indent-tabs-mode nil))))

(provide 'init-rust)
;;; init-rust.el ends here
