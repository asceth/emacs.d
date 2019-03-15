;;; init-polymode.el --- Installs and configures multiple modes support with
;;;                      polymode
;;;
;;; Commentary:
;;;
;;; Code:

(require-package 'polymode)
(require-package 'poly-erb)
(require-package 'poly-ansible)

(require 'poly-erb)
(require 'poly-ansible)

(provide 'init-polymode)
;;; init-polymode.el ends here
