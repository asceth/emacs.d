;;
;; User Settings
;;

(setq-default user-email-address "machinist@asceth.com")

(set-fontset-font t nil (font-spec :size 11 :name "EmojiOne"))
;; (set-fontset-font t nil (font-spec :size 11 :name "IPAexMincho"))
;; (set-face-attribute 'default t :font "Terminess Powerline-11")
;; (set-face-attribute 'default nil :font "Terminess Powerline-11")
;; (set-face-attribute 'default nil :font "IPAexMincho-11")
;; (set-face-attribute 'default nil :font "Terminus (TTF) for Windows-9.0")

(if *is-a-win*
    (progn
      (set-fontset-font t nil (font-spec :size 11 :name "IPAexMincho"))
      (set-face-attribute 'default nil :font "Fira Code-9"))
  (progn
    (set-fontset-font t nil (font-spec :size 11 :name "IPAexMincho"))
    ;; (set-face-attribute 'default nil :font "Terminess Powerline-11")
    (set-face-attribute 'default nil :font "Fira Code-10")
    ;; (set-frame-font "Terminess Powerline 10" nil t)
    (set-frame-font "Fira Code-10" nil t)))

(when (require-package 'ligature)
  ;; Enable the www ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))

  ;; Enable ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                                       ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                                       "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                                       "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                                       "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                                       "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                                       "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                                       "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                                       "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                                       "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))

  (global-ligature-mode 't))

(setq column-number-indicator-zero-based nil)

(set-frame-font "Fira Code-10" nil t)

(setq split-height-threshold nil
      split-weidth-threshold nil)


;;
;; Paths
;;

;; Not required anymore, once I put Git in C: instead of Program Files (space caused issues).
;; (setq exec-path (append exec-path (list (expand-file-name "C:/Git/usr/bin"))))


;;
;; Keybindings
;;

;; I don't have any lost love for the <insert> key.
(global-set-key [insert] (lambda () (interactive)))
(global-set-key [insertchar] (lambda () (interactive)))

;; Map some keys to find-function/find-variable
;; Use C-x F to find function, C-x V to find variable
(define-key ctl-x-map "F" 'find-function)
(define-key ctl-x-map "V" 'find-variable)

;; Map C-x f to find-file-at-point
(global-set-key "\C-xf" 'find-file-at-point)

;; Map C-j to indent-region
(global-set-key "\C-j" 'indent-region)
(global-set-key "\C-i" 'indent-according-to-mode)

;; Map C-x s to save-buffer
(global-set-key "\C-xs" 'save-buffer)

;; Map C-o to align, previously open-line
(global-set-key "\C-o" 'align-current)

;; Map key sequences to Meta
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; I don't use compose-mail
(global-set-key "\C-xm" 'execute-extended-command)
(global-set-key "\C-cm" 'execute-extended-command)

;; Backspace is evil... maybe
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

;; Quick keys to move across buffer
(global-set-key "\C-x." 'end-of-buffer)
(global-set-key "\C-x," 'beginning-of-buffer)

;; No lost love for whatever was there previously
(global-set-key "\C-xi" 'replace-string)
(global-set-key "\C-x;" 'query-replace-regexp)

;; Bind C-x / to goto line number
(global-set-key "\C-x/" 'goto-line)

;; Sometimes I just can't stop pressing Control!  :\
(global-set-key "\C-x\C-o" 'other-window)

(global-set-key "\C-xy" 'clipboard-yank)
(global-set-key "\C-xj" 'clipboard-kill-region)

;; bind C-x u to revert buffer (undo was there previously)
(global-set-key "\C-xu" 'revert-buffer)

(global-set-key "\C-c\C-rh" 'ruby-toggle-hash-syntax)

;; bind C-t to fill-paragraph (transpose-chars was there previously)
(global-set-key "\C-t" 'fill-paragraph)

;; use alt as meta since xmonad uses meta
;;(setq x-alt-keysym 'meta)

;;
;; Indents
;;

(setq-default fill-column 90)

;; Ruby
(add-hook 'ruby-mode-hook
          '(lambda () (custom-set-variables
                  '(ruby-deep-indent-paren 0))))

;; CSS indent depth
(setq css-mode-indent-depth 2)
(setq css-indent-offset 2)

;; JS
(setq js-indent-level 2)

;; nginx
(setq nginx-indent-level 2)

;; C
(setq c-basic-offset 2)
(setq c-default-style "bsd")

;; Java
(setq default-tab-width 2)

(add-hook 'java-mode-hook
          (lambda ()
            (c-set-style "gnu")))


;; Shell scripts
(add-hook 'sh-mode-hook
          (lambda ()
            (setq sh-basic-offset 2)
            (setq sh-indentation 2)))

;; haml
(add-hook 'haml-mode-hook
          (lambda ()
            (setq electric-indent-inhibit t)
            (setq indent-tabs-mode nil)))

;; web-mode
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-style-padding 2)
(setq web-mode-script-padding 2)
(setq web-mode-block-padding 0)


(defun align-to-colon (begin end)
  "Align region to colon"
  (interactive "r")
  (align-regexp begin end
                (rx ":" (group (zero-or-more (syntax whitespace))) ) 1 1 ))

(defun align-to-rocket (begin end)
  "Align region to rocket"
  (interactive "r")
  (align-regexp begin end "\\(\\s-*\\)=>"))

;; ",\\(\\s-*\\)"
(defun align-to-comma (begin end)
  "Align region to commas"
  (interactive "r")
  (align-regexp begin end
                (rx "," (group (zero-or-more (syntax whitespace)))) 1 1 t))


(provide 'init-local)
;;; init-local.el ends here
