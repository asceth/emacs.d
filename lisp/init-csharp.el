(require-package 'csharp-mode)
(require-package 'omnisharp)

(eval-after-load
  'company
  '(add-to-list 'company-backends #'company-omnisharp))

(defun asceth-csharp-mode-setup ()
  (omnisharp-mode)
  (company-mode)
  (flycheck-mode)

  (setq indent-tabs-mode nil)
  (setq c-syntactic-indentation t)
  (setq c-set-style "bsd")
  (setq c-basic-offset 2)
  (setq truncate-lines t)
  (setq tab-width 2))

(add-hook 'csharp-mode-hook 'asceth-csharp-mode-setup t)

(provide 'init-csharp)
