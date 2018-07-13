(setq-default dired-dwim-target t)

;; Prefer g-prefixed coreutils version of standard utilities when available
(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

(when (maybe-require-package 'diredfl)
  (after-load 'dired
    (diredfl-global-mode)))

(after-load 'dired
  (setq dired-recursive-deletes 'top)
  (define-key dired-mode-map [mouse-2] 'dired-find-file)
  (define-key dired-mode-map (kbd "C-c C-p") 'wdired-change-to-wdired-mode))

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


(provide 'init-dired)
