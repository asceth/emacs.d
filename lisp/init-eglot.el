;;; init-eglot.el --- LSP support via eglot          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; For Emacs >= 27
;;(setq read-process-output-max (* 1024 1024))

(defun manually-activate-flymake ()
  (add-hook 'flymake-diagnostic-functions #'eglot-flymake-backend nil t)
  (flymake-mode 1))

(use-package eglot
  :ensure t
  :hook ((rust-mode . eglot-ensure)
         (eglot-managed-mode . manually-activate-flymake)
         (ruby-mode . eglot-ensure)
         (eglot-managed-mode . (lambda () (eglot-inlay-hints-mode -1))))
  :config
  (setq eldoc-idle-delay 0.75)
  (setq eglot-ignored-server-capabilities '(:inlayHintProvider))
  ;; Use below if eglot-events-buffer-size is not 0 and it's slow
  (advice-add 'jsonrpc--log-event :override #'ignore)
  ;; (fset #'jsonrpc--log-event #'ignore)
  ;; do not log events, so no debugging
  ;; (setf (plist-get eglot-events-buffer-config :size) 0)
  ;; if last buffer is killed for a language server, kill the server
  (setq eglot-autoshutdown t)
  (add-to-list 'eglot-stay-out-of 'flymake)
  (add-to-list 'eglot-server-programs '((rust-ts-mode rust-mode) .
                                        ("rust-analyzer"
                                         :initializationOptions ( :procMacro (:attributes (:enable t)
                                                                                          :enable t)
                                                                  :lens (:enable :json-false)
                                                                  :check (:command "clippy")
                                                                  :cargo (:buildScripts (:enable t)
                                                                                        :features "all"))))))


;; (when (maybe-require-package 'eglot)
;;   (maybe-require-package 'consult-eglot)
;;
;;   (with-eval-after-load 'eglot
;;     (add-to-list 'eglot-server-programs `(rust-mode . ("rust-analyzer" :initializationOptions
;;                                                        ( :procMacro (:attributes (:enable t)
;;                                                                                  :enable t)
;;                                                          :lens (:enable :json-false)
;;                                                          :check (:command "clippy")
;;                                                          :cargo (:buildScripts (:enable t)
;;                                                                                :features "all")))))
;;
;;     (setq eldoc-idle-delay 0.75)
;;     (setq flymake-no-changes-timeout 0.5)
;;
;;     ;; No event buffers, disable providers cause a lot of hover traffic. Shutdown unused servers.
;;     (setq eglot-events-buffer-size 0
;;           eglot-ignored-server-capabilities '(:hoverProvider
;;                                               :inlayHintProvider
;;                                               :documentOnTypeFormattingProvider
;;                                               :documentHighlightProvider)
;;           eglot-autoshutdown t)))

;; (with-eval-after-load 'eglot
;; (add-to-list 'eglot-server-programs '((ruby-mode ruby-ts-mode) "ruby-lsp"))))


;; (add-hook 'ruby-mode-hook #'eglot-ensure)


(provide 'init-eglot)
;;; init-eglot.el ends here
