;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Do not produce backtraces on errors
(setq debug-on-error nil)

(let ((minver "27.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "28.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-benchmarking) ;; Measure startup time

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-a-win* (eq system-type 'windows-nt))


;; Adjust garbage collection threshold for early startup (see use of gcmh below)
(setq gc-cons-threshold (* 128 1024 1024))


;; Process performance tuning

(setq read-process-output-max (* 4 1024 1024))
(setq process-adaptive-read-buffering nil)

(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Bootstrap config

(setq custom-file (locate-user-emacs-file "custom.el"))

(require 'init-utils)
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
;; Calls (package-initialize)
(require 'init-elpa)      ;; Machinery for installing required packages
(require 'init-exec-path) ;; Set up $PATH


;; General performance tuning
(when (require-package 'gcmh)
  (setq gcmh-high-cons-threshold (* 128 1024 1024))
  (add-hook 'after-init-hook (lambda ()
                               (gcmh-mode)
                               (diminish 'gcmh-mode))))

(setq jit-lock-defer-time 0)


;; Allow users to provide an optional "init-preload-local.el"
(require 'init-preload-local nil t)

;; Load configs for specific features and modes

(require-package 'diminish)
(maybe-require-package 'scratch)
(require-package 'command-log-mode)

(require 'init-scratch)
(require 'init-themes)
(require 'init-osx-keys)
(require 'init-gui-frames)
(require 'init-dired)
(require 'init-isearch)
(require 'init-grep)
(require 'init-uniquify)
(require 'init-ibuffer)
(require 'init-flymake)
(require 'init-eglot)

(require 'init-recentf)
(require 'init-minibuffer)
(require 'init-hippie-expand)
(require 'init-corfu)
(require 'init-windows)
(require 'init-sessions)
(require 'init-mmm)

(require 'init-editing-utils)
(require 'init-whitespace)

(require 'init-vc)
(require 'init-git)

(require 'init-projectile)

(require 'init-compile)
(require 'init-markdown)
(require 'init-csv)
;(require 'init-erlang)
(require 'init-javascript)
(require 'init-php)
(require 'init-nxml)
(require 'init-html)
(require 'init-css)
(require 'init-go)
(require 'init-haml)
(require 'init-http)
(require 'init-python)
(require 'init-haskell)
(require 'init-ruby)
(require 'init-rails)
(require 'init-sql)
(require 'init-terraform)
;;(require 'init-ocaml)
;;(require 'init-j)
;;(require 'init-nim)
(require 'init-rust)
(require 'init-wgsl)
(require 'init-toml)
(require 'init-yaml)
(require 'init-docker)
(maybe-require-package 'nginx-mode)

(maybe-require-package 'just-mode)
(when (maybe-require-package 'just-ts-mode)
  ;; Undo overly-optimistic autoloading, so that things still work in
  ;; Emacs 29 without treesitter
  (sanityinc/remove-auto-mode  'just-ts-mode))
(maybe-require-package 'justl)

;(require 'init-csharp)
(require 'init-unreal)
(require 'init-gdscript)

(require 'init-paredit)
(require 'init-sly)
(require 'init-lisp)

; Language Server Protocol support
;; (require 'init-lsp)

(when *spell-check-support-enabled*
  (require 'init-spelling))

(require 'init-snippets)
(require 'init-misc)

(require 'init-folding)

;;(require 'init-dash)

;;(require 'init-ledger)
;;(require 'init-lua)
;;(require 'init-uiua)
;;(require 'init-zig)
;;(require 'init-terminals)

;; Extra packages which don't require any configuration
(require-package 'sudo-edit)
(require-package 'gnuplot)
;;(require-package 'lua-mode)
(require-package 'htmlize)
(when *is-a-mac*
  (require-package 'osx-location))
(maybe-require-package 'dotenv-mode)
(maybe-require-package 'shfmt)

;;(when (maybe-require-package 'uptimes)
;;  (setq-default uptimes-keep-count 200)
;;  (add-hook 'after-init-hook (lambda () (require 'uptimes))))

(when (fboundp 'global-eldoc-mode)
  (add-hook 'after-init-hook 'global-eldoc-mode))

;;(require 'init-direnv)


;; Allow access from emacsclient
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

;; Variables configured via the interactive 'customize' interface
(when (file-exists-p custom-file)
  (load custom-file))

;; Locales (setting them earlier in this file doesn't work in X)
(require 'init-locales)

;; Allow users to provide an optional "init-local" containing personal settings
(require 'init-local nil t)

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
