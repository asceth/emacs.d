;;; init-rust.el --- Support for the Rust language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'rust-mode)
  (with-eval-after-load 'rust-mode
    (add-hook 'rust-mode-hook
              (lambda ()
                (setq indent-tabs-mode nil)))
    (add-hook 'rust-mode-hook 'eglot-ensure)))

;; (when (maybe-require-package 'flycheck-rust)
;; (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(setq rust-format-on-save t)

(provide 'init-rust)
;;; init-rust.el ends here
