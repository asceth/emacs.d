(require-package 'omnisharp)

(defun asceth-csharp-mode-setup ()
  (omnisharp-mode)
  (flycheck-mode)

  (setq indent-tabs-mode nil)
  (setq c-syntactic-indentation t)
  (setq c-set-style "bsd")
  (setq c-basic-offset 2)
  (setq truncate-lines t)
  (setq tab-width 2))

(add-hook 'csharp-mode-hook 'asceth-csharp-mode-setup t)

(provide 'init-csharp)
