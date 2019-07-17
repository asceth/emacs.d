;; TODO: smerge-mode
(require-package 'git-blamed)
(require-package 'gitignore-mode)
(require-package 'gitconfig-mode)
(maybe-require-package 'git-timemachine)


(require-package 'fullframe)

(when (maybe-require-package 'git-commit)
  (add-hook 'git-commit-mode-hook 'goto-address-mode))


;; Convenient binding for vc-git-grep
(after-load 'vc
  (define-key vc-prefix-map (kbd "f") 'vc-git-grep))


(maybe-require-package 'git-messenger)
;; Though see also vc-annotate's "n" & "p" bindings
(after-load 'vc
  (setq git-messenger:show-detail t)
  (define-key vc-prefix-map (kbd "p") #'git-messenger:popup-message))


(provide 'init-git)
