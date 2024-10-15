;;; init-eglot.el --- LSP support via eglot          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; For Emacs >= 27
(setq read-process-output-max (* 1024 1024))


(when (maybe-require-package 'eglot)
  (maybe-require-package 'consult-eglot)

  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs `(rust-mode . ("rust-analyzer" :initializationOptions
                                                       ( :procMacro (:enable t)
							 :check (:command "clippy")
							 :cargo (:buildScripts (:enable t)
                                                                               :features "all"))))))
  
  (add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1)))

  (setq eldoc-idle-delay 0.75)
  (setq flymake-no-changes-timeout 0.5)

  ;; No event buffers, disable providers cause a lot of hover traffic. Shutdown unused servers.
  (setq eglot-events-buffer-size 0
        ;; eglot-ignored-server-capabilities '(:hoverProvider
        ;;                                     :documentHighlightProvider)
        eglot-autoshutdown t))




(provide 'init-eglot)
;;; init-eglot.el ends here
