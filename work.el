(require 'treemacs)
(require 'docker-compose-mode)
(require 'php-mode)
(require 'lsp-pascal)

(defun my-setup-js2-mode ()
  (lsp))

(defun my-setup-typescript-mode ()
  (lsp))

(defun my-setup-css-mode ()
  (css-eldoc-enable)
  (emmet-mode))

(defun my-setup-php-mode ()
  (lsp))

(defun my-setup-opascal-mode ()
  (lsp))

(defun my-setup-lsp-mode ()
  (setq-local company-backends '((company-capf))))

(defun my-setup-lsp-ui-mode ()
  (lsp-ui-doc-mode 1)
  (lsp-ui-sideline-mode 1))

(defun my/setup-docker-compose-mode()
  (setq-local company-backends '((company-capf
						   company-dabbrev-code
						   company-files
						   company-yasnippet))))

(setq opascal-indent-level 2
	  opascal-case-label-indent 2
	  lsp-pascal-fpcdir "/usr/share/fpcsrc/3.2.2"
	  lsp-prefer-flymake nil
	  lsp-enable-symbol-highlighting nil
	  lsp-references-exclude-definition t
	  lsp-signature-doc-lines 10
	  lsp-headline-breadcrumb-segments '(file symbols)
	  lsp-headerline-breadcrumb-enable nil
	  lsp-log-io nil
	  lsp-use-plists t
	  lsp-ui-doc-delay 1
	  lsp-ui-doc-max-height 50
	  lsp-ui-doc-alignment 'frame
	  lsp-ui-doc-show-with-cursor nil
	  lsp-ui-doc-position 'bottom
	  lsp-ui-sideline-diagnostics-max-lines 4
	  treemacs-recenter-after-project-jump 'always
	  treemacs-recenter-after-file-follow 'always
	  treemacs-no-delete-other-windows t
	  treemacs-tag-follow-delay 0.1
	  docker-command "podman"
	  docker-compose-command "podman-compose"
	  php-mode-coding-style 'psr2
	  )

(treemacs-project-follow-mode 1)
(treemacs-follow-mode 1)

(define-key php-mode-map (kbd "C-d") nil)
(global-set-key (kbd "<f8>") 'treemacs)

(add-hook 'docker-compose-mode-hook 'my/setup-docker-compose-mode)
(add-hook 'js2-mode-hook 'my-setup-js2-mode)
(add-hook 'typescript-ts-mode-hook 'my-setup-typescript-mode)
(add-hook 'css-mode-hook 'my-setup-css-mode)
(add-hook 'php-mode-hook 'my-setup-php-mode)
(add-hook 'opascal-mode-hook 'my-setup-opascal-mode)
(add-hook 'lsp-mode-hook 'my-setup-lsp-mode)
(add-hook 'lsp-ui-mode-hook 'my-setup-lsp-ui-mode)

(add-to-list 'auto-mode-alist '("docker-compose\\.y.?ml$" . docker-compose-mode))
(add-to-list 'auto-mode-alist '("\\.dockerignore$" . gitignore-mode))
(add-to-list 'auto-mode-alist '("composer\\.lock" . js-json-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.\\(pas\\|pp\\|dpr\\)\\'" . opascal-mode))
(add-to-list 'auto-mode-alist '("\\.lpr$" . opascal-mode)) ;; Lazarus program
