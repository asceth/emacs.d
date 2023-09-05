;;; init-php.el --- Support for working with PHP -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'php-mode)
  (maybe-require-package 'smarty-mode)
  (maybe-require-package 'twig-mode)

  (c-add-style
   "phpcc"
   `((c-basic-offset . 2)
     (c-offsets-alist . (
                         (arglist-close . php-lineup-arglist-close)
                         (arglist-cont . (first php-lineup-cascaded-calls 0))
                         (arglist-cont-nonempty . (first php-lineup-cascaded-calls c-lineup-arglist))
                         (arglist-intro . php-lineup-arglist-intro)
                         (case-label . +)
                         (class-open . 0)
                         (comment-intro . 0)
                         (inclass . +)
                         (inexpr-class . 0)
                         (inlambda . 0)
                         (inline-open . 0)
                         (namespace-open . 0)
                         (lambda-intro-cont . +)
                         (label . +)
                         (statement-cont . 0)
                         ;;(statement-cont . (first php-lineup-cascaded-calls php-lineup-string-cont +))
                         (substatement-open . 0)
                         (topmost-intro-cont . (first php-lineup-cascaded-calls +))))
     (c-indent-comments-syntactically-p . t)
     (indent-tabs-mode . nil)
     (tab-width . 2)
     (fill-column . 78)
     (show-trailing-whitespace . t)
     (php-style-delete-trailing-whitespace . t)))

  (add-hook 'php-mode-hook
            (lambda ()
              (subword-mode 1)
              (php-set-style "phpcc")
              (setq c-basic-offset 2)
              (setq-local syntax-propertize-function nil)
              (remove-hook 'syntax-propertize-extend-region-functions
                           #'php-syntax-propertize-extend-region t)
              (when (eq (buffer-size) 0)
                (insert "<?php\n\n")))))

(advice-add 'php--syntax-propertize-quotes-in-comment :override #'ignore)
(advice-add 'php-syntax-propertize-function :override #'ignore)
(setq php-phpdoc-font-lock-keywords nil)

(provide 'init-php)
;;; init-php.el ends here
