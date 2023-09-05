;;; init-whitespace.el --- Special handling for whitespace -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines nil)
(setq whitespace-line-column 80)
(setq whitespace-style '(face tabs lines-tail trailing tab-mark))
                                        ; (setq whitespace-global-modes '(not 'mode))
(add-hook 'after-init-hook 'global-whitespace-mode)


;;; Whitespace

(defun sanityinc/show-trailing-whitespace ()
  "Enable display of trailing whitespace in this buffer."
  (setq-local show-trailing-whitespace t))

(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook 'sanityinc/show-trailing-whitespace))

(require-package 'whitespace-cleanup-mode)
(add-hook 'after-init-hook 'global-whitespace-cleanup-mode)
(with-eval-after-load 'whitespace-cleanup-mode
  (diminish 'whitespace-cleanup-mode))

(global-set-key [remap just-one-space] 'cycle-spacing)

(add-hook 'before-save-hook 'whitespace-cleanup)

(provide 'init-whitespace)
;;; init-whitespace.el ends here
