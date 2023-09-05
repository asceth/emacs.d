;;;; lsp-mode --- Language Server Protocol (LSP) setup
;;;; Commentary:

;;;; Code:
(when (maybe-require-package 'lsp-mode)
  (require 'lsp-mode)
  (require 'lsp-solargraph)
  ;; config
  (setq lsp-eldoc-render-all t)
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-idle-delay 0.6)
  ;; rust
  (setq lsp-rust-server 'rust-analyzer)
  (setq lsp-rust-analyzer-cargo-watch-command "clippy")
  (setq lsp-rust-analyzer-server-display-inlay-hints t)
  (setq lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (setq lsp-rust-analyzer-display-chaining-hints t)
  (setq lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (setq lsp-rust-analyzer-display-closure-return-type-hints t)
  (setq lsp-rust-analyzer-display-parameter-hints t)
  (setq lsp-rust-analyzer-display-reborrow-hints t)

  (when (maybe-require-package 'lsp-ui)
    (add-hook 'lsp-mode-hook 'lsp-ui-mode)
    (setq lsp-ui-peek-always-show t)
    (setq lsp-ui-sideline-show-hover t)
    (setq lsp-ui-doc-enable nil)))

(add-hook 'ruby-mode-hook #'lsp)
(setq-default lsp-typeprof-use-bundler t)


(add-hook 'rust-mode-hook #'lsp)

(add-hook 'c-mode-hook #'lsp-deferred)
(add-hook 'c++-mode-hook #'lsp-deferred)

(maybe-require-package 'lsp-ui)
(when (maybe-require-package 'lsp-treemacs)
  (lsp-treemacs-sync-mode +1))

;; unreal engine support
(setq lsp-clients-clangd-args '("-j=3"
                                "--all-scopes-completion"
                                "--background-index"
                                "--clang-tidy"
                                "--completion-style=bundled" ; detailed
                                "--header-insertion=never" ; iwyu
                                "--header-insertion-decorators=0"))
;;(after! lsp-clangd (set-lsp-priority! 'clangd 2))

(provide 'init-lsp)

;;; init-lsp.el ends here
