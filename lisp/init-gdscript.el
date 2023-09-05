(require-package 'gdscript-mode)

(setq gdscript-use-tab-indents nil)
(setq gdscript-indent-offset 2)

(defun asceth-gdscript-mode-setup ()
  (rainbow-mode)
  (setq whitespace-style '(face lines-tail trailing))
  (setq whitespace-line-column 100))

(add-hook 'gdscript-mode-hook 'asceth-gdscript-mode-setup t)

(provide 'init-gdscript)
