;;; init-snippets.el --- yasnippet config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; Configure Yasnippet

(when (maybe-require-package 'yasnippet)
  (yas-global-mode 1))

(provide 'init-snippets)
;;; init-snippets.el ends here
