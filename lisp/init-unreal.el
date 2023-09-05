;;;; Unreal Engine support
;;;; Commentary:

;;;; Code:

;; (maybe-require-package 'cmake-mode)
;; (maybe-require-package 'cuda-mode)
;; (maybe-require-package 'demangle-mode)
;; (maybe-require-package 'disaster)
;; (maybe-require-package 'modern-cpp-font-lock)
;; (maybe-require-package 'opencl-mode)
;; (when (maybe-require-package 'glsl-mode))


;;; Configuration variables

(defconst om-clang-format-location
  (pcase system-type
    ('darwin     (om-config-executable-find "clang-format"))
    ('gnu/linux  (om-config-executable-find "clang-format"))
    ('windows-nt "C:\\Program Files\\LLVM\\bin\\clang-format.exe"))
  "The absolute path to the clang-format executable.")

;;;; Keys
(defconst om-kbd-clang-format-buffer      (kbd "C-c f"))

;;; C++ config

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-hook
 'c++-mode-hook
 (lambda ()
   (setq c-default-style "bsd"
         c-basic-offset  4
         tab-width       4)
   (c-set-offset 'innamespace 0)
   ;; https://www.emacswiki.org/emacs/ElectricPair
   (electric-pair-mode +1)
   ;; https://wikemacs.org/wiki/Subword-mode
   (subword-mode +1)
   (display-line-numbers-mode +1)
   ;; Set the right margin according to Epic Games conding standard
   (setq-local fill-column 120)
   (setq-local whitespace-style '(face lines-tail trailing))
   (local-set-key om-kbd-clang-format-buffer #'clang-format-buffer)))

(defun add-unreal-macro-names-with-semicolon ()
  "Add Unreal Engine special macros to fix indentation."
  (setq c-macro-names-with-semicolon (append c-macro-names-with-semicolon (append ue-attributes ue-generated-body-macro)))
                                        ;  (add-to-list 'c-macro-names-with-semicolon "GENERATED_BODY")
                                        ;  (add-to-list 'c-macro-names-with-semicolon "UCLASS")
                                        ;  (add-to-list 'c-macro-names-with-semicolon "UPROPERTY")
  (c-make-macro-with-semi-re))
(add-hook 'c++-mode-hook 'add-unreal-macro-names-with-semicolon)

;;; Packages

;; https://github.com/ludwigpacifici/modern-cpp-font-lock
(when (maybe-require-package 'modern-cpp-font-lock)
  (modern-c++-font-lock-global-mode +1))


;; https://clang.llvm.org/docs/ClangFormat.html
(when (maybe-require-package 'clang-format)
  (setq clang-format-executable om-clang-format-location))

;;
(when (maybe-require-package 'ue)
  (with-eval-after-load 'ue
    (define-key ue-mode-map (kbd "C-c u") 'ue-command-map))
  (ue-global-mode +1))


(provide 'init-unreal)
;;; init-unreal.el ends here
