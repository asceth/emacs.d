(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines nil)
(setq whitespace-line-column 80)
(setq whitespace-style '(face tabs lines-tail trailing tab-mark))
                                        ; (setq whitespace-global-modes '(not 'mode))
(add-hook 'after-init-hook 'global-whitespace-mode)


;;; Whitespace

(defun sanityinc/no-trailing-whitespace ()
  "Turn off display of trailing whitespace in this buffer."
  (setq show-trailing-whitespace nil))

;; But don't show trailing whitespace in SQLi, inf-ruby etc.
(dolist (hook '(special-mode-hook
                Info-mode-hook
                eww-mode-hook
                term-mode-hook
                comint-mode-hook
                compilation-mode-hook
                text-mode
                twittering-mode-hook
                minibuffer-setup-hook))
  (add-hook hook #'sanityinc/no-trailing-whitespace))


(add-hook 'before-save-hook 'whitespace-cleanup)

(global-set-key [remap just-one-space] 'cycle-spacing)


(provide 'init-whitespace)
