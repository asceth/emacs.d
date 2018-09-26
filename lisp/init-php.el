(when (maybe-require-package 'php-mode)
  (maybe-require-package 'smarty-mode)
  (maybe-require-package 'twig-mode)
  (maybe-require-package 'php-extras)

  (c-add-style
   "phpcc"
   `((c-basic-offset . 2)
     (c-offsets-alist . ((arglist-close . php-lineup-arglist-close)
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
                         (statement-cont . (first php-lineup-cascaded-calls php-lineup-string-cont +))
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

              (when (eq (buffer-size) 0)
                (insert "<?php\n\n"))))

  (with-eval-after-load "php"
    (defun php-beginning-of-defun-regexp (&optional arg)
      "Move to the beginning of the ARGth PHP function from point.
Implements PHP version of `beginning-of-defun-function'."
      (interactive "p")
      (let ((arg (or arg 1)))
        (while (> arg 0)
          (re-search-backward php-beginning-of-defun-regexp
                              nil 'noerror)
          (setq arg (1- arg)))
        (while (< arg 0)
          (end-of-line 1)
          (let ((opoint (point)))
            (beginning-of-defun 1)
            (condition-case nil
                (progn
                  (forward-list 2))
              (error
               (progn
                 (goto-char opoint))))
            (forward-line 1)
            (if (eq opoint (point))
                (re-search-forward php-beginning-of-defun-regexp
                                   nil 'noerror))
            (setq arg (1+ arg))))))))


(provide 'init-php)
