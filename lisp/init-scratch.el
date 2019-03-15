;;; init-scratch.el --- Setup emacs scratch buffer
;;;
;;; Commentary:
;;;
;;; Code:

;; (require-package 'quoted-scratch)

(defgroup quoted-scratch nil
  "Customization group for `quoted-scratch'."
  :group 'environment)

(defface qs-quote-face
  '((((class color) (background dark))
     (:foreground "LemonChiffon" :height 1.2))
    (t (:foreground "Black")))
  "Face for a quote."
  :group 'quoted-scratch)

(defcustom qs-auroville-quality-face
  '(:foreground "Sienna" :height 3.0)
  "Face for showing an Auroville quality."
  :group 'quoted-scratch
  :type 'face)

(defcustom qs-show-auroville-quality t
  "Show an Auroville quality along with the quote."
  :group 'quoted-scratch
  :type 'face)

(defun qs-get-auroville-quality ()
  "Return one of the Auroville qualities."
  (let ((index (string-to-number (format-time-string "%d")))
        (qualities
         '(

           "
                  誠実
                Seijitsu
                Sincerity"

           "
                  謙虚
                 Kenkyo
                Humility"

           "
                  感謝
                 Kansha
                Gratitude"

           "
                 忍耐力
              Nintai-ryoku
              Perseverance"

           "
                  吸引
                 Kyūin
               Aspiration"

           "
                 感受性
                Kanjusei
               Receptivity"

           "
                  進捗
               Shinchoku
               Progress"

           "
                  勇気
                  Yūki
                 Courage"
           "
                   善
                  Zen
                Goodness"

           "
                 寛大さ
               Kandai-sa
               Generosity"

           "
                  平等
                 Byōdō
                Equality"

           "
                  平和
                 Heiwa
                 Peace")))

    (propertize (nth  (mod index (length qualities)) qualities)
                'font-lock-face qs-auroville-quality-face
                'rear-nonsticky t)))

(defun qs-generate-scratch-message (&optional quote-string)
  "Generate message content for scratch buffer.
Make sure you set the :text-type text property to :quote-string.
If argument QUOTE-STRING is provided, use that as the quote."
  (format "%s%s\n\n"
          ""
          (qs-get-auroville-quality)))

(setq-default initial-scratch-message (qs-generate-scratch-message))
(setq-default initial-major-mode 'text-mode)

(provide 'init-scratch)

;;; init-scratch.el ends here
