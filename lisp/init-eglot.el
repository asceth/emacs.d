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
         (eglot-managed-mode . manually-activate-flymake))
  :config
  (add-to-list 'eglot-stay-out-of 'flymake))


;; (when (maybe-require-package 'eglot)
;;   (maybe-require-package 'consult-eglot)

;;   (with-eval-after-load 'eglot
;;     (add-to-list 'eglot-server-programs `(rust-mode . ("rust-analyzer" :initializationOptions
;;                                                        ( :procMacro (:attributes (:enable t)
;;                                                                                  :enable t)
;;                                                          :lens (:enable :json-false)
;;                                                          :check (:command "clippy")
;;                                                          :cargo (:buildScripts (:enable t)
;;                                                                                :features "all")))))

;;     (setq eldoc-idle-delay 0.75)
;;     (setq flymake-no-changes-timeout 0.5)

;;     ;; No event buffers, disable providers cause a lot of hover traffic. Shutdown unused servers.
;;     (setq eglot-events-buffer-size 0
;;           eglot-ignored-server-capabilities '(:hoverProvider
;;                                               :inlayHintProvider
;;                                               :documentOnTypeFormattingProvider
;;                                               :documentHighlightProvider)
;;           eglot-autoshutdown t)))




(provide 'init-eglot)
;;; init-eglot.el ends here
