;; -*- lexical-binding: t -*-
;;; package --- Summary

(require 'treemacs)
(require 'docker-compose-mode)
(require 'flymake-ruby)

(defun my-setup-ruby-mode ()
  (eglot-ensure)
  (flymake-ruby-load))

(defun my-setup-js-mode ()
  (eglot-ensure))

(defun my-setup-typescript-mode ()
  (eglot-ensure))

(defun my-setup-css-mode ()
  (css-eldoc-enable)
  (emmet-mode))

(defun my-setup-php-mode ()
  (setq-local indent-tabs-mode nil
	      indent-line-function 'indent-relative  ;; fix for weird gnu braces style
	      tab-width 4)
  (eglot-ensure))

(defun my-setup-opascal-mode ()
  (setq-local indent-tabs-mode nil
	      tab-width 2)
  (eglot-ensure))

(defun my/setup-docker-compose-mode()
  (setq-local company-backends '((company-capf
				  company-dabbrev-code
				  company-files
				  company-yasnippet))))

(setq opascal-indent-level 2
      opascal-case-label-indent 2
      treemacs-recenter-after-project-jump 'always
      treemacs-recenter-after-file-follow 'always
      treemacs-no-delete-other-windows t
      treemacs-tag-follow-delay 0.1
      docker-command "podman"
      docker-compose-command "podman-compose"
      )

(treemacs-project-follow-mode 1)
(treemacs-follow-mode 1)
(editorconfig-mode 1)
(text-scale-set -1)

(global-set-key (kbd "<f7>") 'treemacs)

(add-hook 'docker-compose-mode-hook 'my/setup-docker-compose-mode)
(add-hook 'js-mode-hook 'my-setup-js-mode)
(add-hook 'typescript-ts-mode-hook 'my-setup-typescript-mode)
(add-hook 'css-mode-hook 'my-setup-css-mode)
(add-hook 'php-ts-mode-hook 'my-setup-php-mode)
(add-hook 'opascal-mode-hook 'my-setup-opascal-mode)
(add-hook 'ruby-mode-hook 'my-setup-ruby-mode)

(add-to-list 'auto-mode-alist '("docker-compose\\.y.?ml$" . docker-compose-mode))
(add-to-list 'auto-mode-alist '("\\.dockerignore$" . gitignore-mode))
(add-to-list 'auto-mode-alist '("composer\\.lock" . js-json-mode))
(add-to-list 'auto-mode-alist '("\\.php$" . php-ts-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.tsx$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.ejs$" . mhtml-mode))
(add-to-list 'auto-mode-alist '("\\.\\(pas\\|pp\\|dpr\\)\\'" . opascal-mode))
(add-to-list 'auto-mode-alist '("\\.lpr$" . opascal-mode)) ;; Lazarus program

(provide 'work)

;;; work.el ends here
