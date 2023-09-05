;;; init-go.el --- Support for the Go language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'go-mode)
(require-package 'flycheck-golangci-lint)

(add-auto-mode 'go-mode
               "\\.go\\'")

(provide 'init-go)
