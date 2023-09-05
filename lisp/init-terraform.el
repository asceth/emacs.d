;;; init-terraform.el --- Support for working with Terraform -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (require-package 'terraform-mode)
  (maybe-require-package 'terraform-docs))

(provide 'init-terraform)
;;; init-terraform.el ends here
