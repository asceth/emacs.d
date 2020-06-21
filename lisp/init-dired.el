(setq-default dired-dwim-target t)

;; List directories first by using Lisp-only ls command
(require 'ls-lisp)

;; Prefer g-prefixed coreutils version of standard utilities when available
(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

(when (maybe-require-package 'diredfl)
  (after-load 'dired
    (diredfl-global-mode)))

(after-load 'dired
  (setq dired-recursive-deletes 'top)
  (define-key dired-mode-map [mouse-2] 'dired-find-file))

(when (maybe-require-package 'diff-hl)
  (after-load 'dired
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode)))

(after-load 'dired
  (add-hook 'dired-mode-hook
            '(lambda ()
               ;; By default dired-find-alternate-file is bound to 'a' and '<RET>' is bound
               ;; dired-find-file
               (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
               ;; Allow us to edit filenames from the dired buffer, 'e' was previously bound
               ;; to dired-find-file
               (define-key dired-mode-map "e" 'wdired-change-to-wdired-mode)
               (define-key dired-mode-map "r" 'dired-create-directory))))

(put 'dired-find-alternate-file 'disabled nil)

;; group directories first
(setq dired-listing-switches "-aBhl  --group-directories-first")
(setq ls-lisp-use-insert-directory-program t)

(if (eq system-type 'windows-nt)
    (setq insert-directory-program "C:/Program Files/Git/usr/bin/ls.exe"))


;; Map C-x C-d to dired-at-point
;; (global-set-key "\C-xd" 'dired-at-point)
(global-set-key "\C-x\C-d" 'dired-at-point)

(provide 'init-dired)
;;; init-dired.el ends here
