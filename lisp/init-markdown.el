(when (maybe-require-package 'markdown-mode)
  (add-auto-mode 'markdown-mode "\\.md")
  (add-hook 'markdown-mode-hook
            (lambda ()
              (setq-local whitespace-style '(face tabs trailing tab-mark))))
  (after-load 'whitespace-cleanup-mode
    (push 'markdown-mode whitespace-cleanup-mode-ignore-modes)))


(provide 'init-markdown)
