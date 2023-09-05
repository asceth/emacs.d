;;; init-ruby.el --- Support for the Ruby language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Basic ruby setup
(require-package 'ruby-hash-syntax)

(add-auto-mode 'ruby-mode
               "\\.rxml\\'"
               "\\.rjs\\'" "\\.irbrc\\'" "\\.pryrc\\'" "\\.builder\\'" "\\.ru\\'"
               "\\.gemspec\\'" "Kirkfile\\'")
(add-auto-mode 'conf-mode "Gemfile\\.lock\\'")

(setq-default
 ruby-use-encoding-map t
 ruby-insert-encoding-magic-comment t)

(add-hook 'ruby-mode-hook 'subword-mode)

(with-eval-after-load 'page-break-lines
  (add-to-list 'page-break-lines-modes 'ruby-mode))

(require-package 'rspec-mode)


(define-derived-mode brewfile-mode ruby-mode "Brewfile"
  "A major mode for Brewfiles, used by homebrew-bundle on MacOS.")

(add-auto-mode 'brewfile-mode "Brewfile\\'")


;;; Inferior ruby
(require-package 'inf-ruby)
(with-eval-after-load 'inf-ruby
  (defun sanityinc/ruby-load-file (&optional choose-file)
    (interactive "P")
    (if (or choose-file (not buffer-file-name))
        (call-interactively 'ruby-load-file)
      (save-some-buffers)
      (ruby-load-file buffer-file-name)))
  (define-key inf-ruby-minor-mode-map [remap ruby-load-file] 'sanityinc/ruby-load-file))



;;; Ruby compilation
(require-package 'ruby-compilation)

(with-eval-after-load 'ruby-mode
  (define-key ruby-mode-map [S-f7] 'ruby-compilation-this-buffer)
  (define-key ruby-mode-map [f7] 'ruby-compilation-this-test))

(with-eval-after-load 'ruby-compilation
  (defalias 'rake 'ruby-compilation-rake))



;;; Robe
(when (maybe-require-package 'robe)
  (with-eval-after-load 'ruby-mode
    (add-hook 'ruby-mode-hook 'robe-mode)))



;;; ri support
(require-package 'yari)
(defalias 'ri 'yari)



(require-package 'bundler)


(when (maybe-require-package 'yard-mode)
  (add-hook 'ruby-mode-hook 'yard-mode)
  (with-eval-after-load 'yard-mode
    (diminish 'yard-mode)))


;;; ERB
(require-package 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; (require-package 'mmm-mode)

;; (require 'derived)

;; (defun sanityinc/set-up-mode-for-erb (mode)
;;   (add-hook (derived-mode-hook-name mode) (lambda () (require 'mmm-erb)))
;;   (mmm-add-mode-ext-class mode "\\.erb\\'" 'erb))

;; (dolist (mode '(html-mode html-erb-mode nxml-mode))
;;   (sanityinc/set-up-mode-for-erb mode)
;;   (mmm-add-mode-ext-class mode "\\.r?html\\(\\.erb\\)?\\'" 'html-js)
;;   (mmm-add-mode-ext-class mode "\\.r?html\\(\\.erb\\)?\\'" 'html-css))



;; Ruby - my convention for heredocs containing SQL

;; (require-package 'mmm-mode)
;; (eval-after-load 'mmm-mode
;;   '(progn
;;      (mmm-add-classes
;;       '((ruby-heredoc-sql
;;          :submode sql-mode
;;          :front "<<-?[\'\"]?\\(end_sql\\)[\'\"]?"
;;          :save-matches 1
;;          :front-offset (end-of-line 1)
;;          :back "^[ \t]*~1$"
;;          :delimiter-mode nil)))
;;      (mmm-add-mode-ext-class 'ruby-mode "\\.rb\\'" 'ruby-heredoc-sql)))

;; (add-to-list 'mmm-set-file-name-for-modes 'ruby-mode)

;; (require 'align)

;; (add-to-list 'align-rules-list
;;              '(ruby-comma-delimiter
;;                (regexp . ",\\(\\s-*\\)[^# \t\n]")
;;                (repeat . t)
;;                (modes  . '(ruby-mode))))

;; (add-to-list 'align-rules-list
;;              '(ruby-hash-literal
;;                (regexp . "\\(\\s-*\\)=>\\s-*[^# \t\n]")
;;                (group 2 3)
;;                (repeat . t)
;;                (modes  . '(ruby-mode))))

;; (add-to-list 'align-rules-list
;;              '(ruby-hash-literal2
;;                (regexp . "[a-z0-9]:\\(\\s-*\\)[^# \t\n]")
;;                (repeat . t)
;;                (modes  . '(ruby-mode))))

;; (add-to-list 'align-rules-list
;;              '(ruby-assignment-literal
;;                (regexp . "\\(\\s-*\\)=\\s-*[^# \t\n]")
;;                (repeat . t)
;;                (modes  . '(ruby-mode))))

;; (add-to-list 'align-rules-list
;;              '(ruby-xmpfilter-mark
;;                (regexp . "\\(\\s-*\\)# => [^#\t\n]")
;;                (repeat . nil)
;;                (modes  . '(ruby-mode))))

;;; Magic comments

(add-hook 'ruby-mode-hook 'add-ruby-magic-comment-hook)

(defun add-ruby-magic-comment-hook ()
  "Insert the frozen string literal magic comment on save for ruby files."
  (add-hook
   (cond ((boundp 'before-save-hook)
          (make-local-variable 'before-save-hook)
          'before-save-hook)
         ((boundp 'write-contents-functions) 'write-contents-functions)
         ((boundp 'write-contents-hooks) 'write-contents-hooks))
   #'ruby-mode-set-frozen-string-literal
   nil 'local))

(defun ruby-mode-set-frozen-string-literal ()
  "Insert a magic comment for frozen string literals."
  (interactive)
  (let* ((variable "frozen_string_literal")
         (value "true")
         (prefix "# ")
         (prefix-re (concat "^" (regexp-quote prefix)))
         (suffix "")
         (suffix-re (concat (regexp-quote suffix) "$"))
         replaced-pos
         end)
    (save-excursion
      (widen)
      (goto-char (point-min))
      (if (re-search-forward "^[^#]" (point-max) t)
          (if (eq (point) (point-max))
              (setq end (point-min))
            (progn
              (backward-char)
              (setq end (point)))))
      (goto-char (point-min))
      (if (re-search-forward
           (format "%s%s:.*%s" prefix-re variable suffix-re) (point-max) t)
          (setq replaced-pos (point)))
      (cond
       ((null replaced-pos)
        (goto-char end)
        (insert (format "%s%s: %s%s\n" prefix variable value suffix)))))))

(provide 'init-ruby)
;;; init-ruby.el ends here
