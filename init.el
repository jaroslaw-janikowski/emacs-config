(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")
			 ("gnu" . "https://elpa.gnu.org/packages/"))
      package-archive-priorities '(("melpa" . 1)
				   ("melpa-stable" . 2)
				   ("nongnu" . 3)
				   ("gnu" . 4)))
(package-initialize)

(require 'crux)
(require 'nxml-mode)
(require 'hexl)
(require 'yasnippet)
(require 'helm-find)
(require 'web-mode)
(require 'magit)
(require 'multiple-cursors)
(require 'org-present)
(require 'visual-fill-column)
(require 'restclient)
(require 'which-key)
(require 'evil-nerd-commenter)
(require 'nginx-mode)
(require 'treemacs)
(require 'docker-compose-mode)
(require 'denote)
(require 'beframe)

;; Custom project management
(setq my/project-current-root "~/.emacs.d/")

(defun my/project-pre-hook()
  (if (not (string-prefix-p my/project-current-root default-directory))
	  (setq default-directory my/project-current-root)))

(add-hook 'pre-command-hook 'my/project-pre-hook)

(defun my/project-root (project-path)
  (interactive "DKatalog root: ")
  (if (file-directory-p project-path)
	  (setq my/project-current-root project-path)
	(message "Nie udało się zmienić katalogu projektu. Typuj ścieżki dokładniej!")))

(global-set-key (kbd "C-x p p") 'my/project-root)
;; end of custom project management

(defun my/helm-find()
  (interactive)
  (helm-find-1 my/project-current-root))

(defun my/tab-bar-groups()
  (list (cond ((eq major-mode 'dired-mode) "shell")
			  ((eq major-mode 'eshell-mode) "shell")
			  ((string-prefix-p "*" (buffer-name)) "emacs")
			  ((string-match-p ".newsrc-dribble" (buffer-name)) "emacs")
			  ((t "user")))))

(defun my/forward-paragraph()
  (interactive)
  (forward-paragraph)
  (next-line))

(defun my/backward-paragraph()
  (interactive)
  (previous-line)
  (backward-paragraph)
  (next-line))

(defun my/create-new-eshell()
  (interactive)
  (eshell 'N))

(defun insert-current-date()
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun my/org-present-start()
  (setq visual-fill-column-width 110
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1)
  (visual-line-mode 1)
  (display-line-numbers-mode 0)
  (org-present-big))

(defun my/org-present-end()
  (visual-fill-column-mode 0)
  (display-line-numbers-mode t)
  (org-present-small))

(defun my/grep-under-cursor(arg)
  (interactive "P")
  (let ((word (thing-at-point 'word)))
    (if word
	(helm-grep-ag (expand-file-name command-line-default-directory) arg))))

(defun my/on-escape()
  (interactive)
  (company-abort)
  (mc/keyboard-quit))

(defun my/move-line-up ()
  (interactive)
  (transpose-lines 1)
  (previous-logical-line 2))

(defun my/move-line-down ()
  (interactive)
  (next-logical-line 1)
  (transpose-lines 1)
  (previous-logical-line 1))

(defun my/in-start-tag-p ()
  (let ((token-end (nxml-token-before))
	(pod (1+ (point)))
	(token-start xmltok-start))
    (or (eq xmltok-type 'partial-start-tag)
	(and (memq xmltok-type '(start-tag empty-element partial-empty-element))
	     (>= token-end pos)))))

(defun my/finish-element ()
  (interactive)
  (if (my/in-start-tag-p)
      (nxml-balanced-close-start-tag-inline)
    (insert ">")))

(defun my/nxml-newline ()
  "Insert a newline, indenting the current line and the newline appropriately in nxml-mode."
  (interactive)
  (if (and (char-before) (char-after)
	   (char-equal (char-before) ?>)
	   (char-equal (char-after) ?<))
      (let ((indentation (current-indentation)))
	(newline)
	(indent-line-to (+ indentation 4))
	(newline)
	(indent-line-to indentation)
	(previous-line)
	(end-of-line))
    (newline-and-indent)))

(defun my/create-new-file ()
  "Create a new empty buffer and save it to a file."
  (interactive)
  (let ((buffer (generate-new-buffer "untitled")))
    (switch-to-buffer buffer)
    (write-file (read-file-name "Save as: "))))

(defun my/create-new-directory ()
  "Create new empty directory."
  (interactive)
  (make-directory (read-directory-name "Save as: ")))

(defun my/setup-text-mode()
  (setq company-backends '((company-dabbrev)))
  (hl-line-mode)
  (flyspell-mode)
  (ispell-change-dictionary "polish"))

(defun my/setup-prog-mode()
  (setq company-backends '((company-yasnippet
			    company-dabbrev-code
			    company-keywords
			    company-files
			    company-capf))
		show-trailing-whitespace t)
  (hl-line-mode)
  (flyspell-prog-mode)
  (display-line-numbers-mode 1)
  (display-fill-column-indicator-mode)
  (ispell-change-dictionary "british")
  (diff-hl-mode)
  (rainbow-mode)
  (idle-highlight-mode t))

(defun my/setup-makefile-mode()
  (setq company-backends '(company-files
						   company-dabbrev-code
						   company-yasnippet)))

(defun my/setup-elisp-mode()
  (setq company-backends '((company-capf company-elisp company-files company-yasnippet company-dabbrev-code))))

(defun my/setup-docker-compose-mode()
  (setq company-backends '(company-capf
						   company-dabbrev-code
						   company-files
						   company-yasnippet)))

(defun my/setup-gnus-article-mode()
  (text-scale-decrease 1)
  (hl-line-mode))

(defun my/setup-gnus-summary-mode()
  (text-scale-decrease 2)
  (hl-line-mode))

(defun my/setup-gnus-group-mode()
  (text-scale-decrease 1)
  (gnus-topic-mode)
  (hl-line-mode))

(defun my/setup-restclient-mode()
  (setq company-backends '((company-restclient company-yasnippet))))

(defun my/setup-nginx-mode()
  (setq company-backends '((company-files company-yasnippet company-nginx))))

(defun my/setup-eshell()
  (setq company-backends '((company-files company-yasnippet company-shell))))

(defun my/setup-term-mode())

(defun my/create-new-term()
  (interactive)
  (term "/bin/bash"))

(defun my/dired-new()
  (interactive)
  (dired default-directory))

(defun my/setup-dired()
  (hl-line-mode)
  (nerd-icons-dired-mode))

(defun my/setup-python-mode()
  (eglot-ensure))

(defun my/setup-sqli-mode()
  (toggle-truncate-lines t))

(defun my/setup-sql-mode()
  (sql-set-dialect 'postgres))

(defun my/setup-eww-mode()
  (hl-line-mode))

(defun my/on-before-save()
  (delete-trailing-whitespace))

(setq warning-minimum-level :error
	  debug-on-error t
      inhibit-startup-screen t
	  inhibit-splash-screen t
      byte-compile-warnings nil
      gc-cons-threshold 10000000
      gc-cons-percentage 0.1
      garbage-collection-messages nil
      make-backup-files nil
	  backup-inhibited nil
      auto-save-default nil
      load-prefer-newer t
      read-process-output-max (* 3 1024 1024) ;; 3mb
      max-lisp-eval-depth 10000
      max-specpdl-size 10000
      package--init-file-ensured t
      create-lockfiles nil
      large-file-warning-threshold 100000000
      ring-bell-function 'ignore
      use-short-answers t
	  read-answer-short t
	  kill-ring-max 60
	  isearch-lazy-count t
	  shell-command-prompt-show-cwd t
	  ansi-color-for-comint-mode t
	  shell-input-autoexpand 'input
	  shell-highlight-undef-enable t
	  shell-kill-buffer-on-exit t
	  explicit-shell-file-name "/bin/bash"
	  comint-prompt-read-only t
	  comint-buffer-maximum-size 9999
	  comint-completion-autolist t
	  comint-input-ignoredups t
      confirm-kill-processes nil
      confirm-kill-emacs nil
      confirm-nonexistent-file-or-buffer nil
	  completion-ignore-case t
	  read-buffer-completion-ignore-case t
	  read-file-name-completion-ignore-case t
	  enable-recursive-minibuffers t
	  resize-mini-windows t
      require-final-newline t
      executable-prefix-env t
      initial-scratch-message nil
      enable-local-variables nil
	  blink-matching-paren t
	  remote-file-name-inhibit-delete-by-moving-to-trash t
	  remote-file-name-inhibit-auto-save t
	  tramp-connection-timeout (* 60 10)
	  tramp-default-remote-shell "/bin/bash"
	  tramp-verbose 7
	  kill-do-not-save-duplicates t
	  comment-empty-lines t
	  comment-fill-column nil
	  comment-multi-line t
	  comment-style 'multi-line
      dired-auto-revert-buffer t
      dired-confirm-shell-command nil
      dired-clean-confirm-killing-deleted-buffers nil
	  dired-recursive-copies 'always
      dired-recursive-deletes 'always
      dired-no-confirm t
	  dired-make-directory-clickable t
	  dired-mouse-drag-files t
	  dired-clean-up-buffers-too t
	  dired-clean-confirm-killing-deleted-buffers t
	  default-input-method "english"
	  default-transient-input-method "polish"
	  initial-buffer-choice t  ;; always start with *scratch*
	  frame-resize-pixelwise t
	  frame-inhibit-implied-resize t
	  frame-title-format '("%b")
      initial-major-mode 'org-mode
      nxml-slash-autocomplete-flag t
      nxml-mode-map (make-keymap)
	  custom-file (make-temp-file "emacs-custom-")  ;; brak dopisywania customize
      company-minimum-prefix-length 1
      company-idle-delay 0
      company-selection-wrap-around t
      company-files-exclusions '(".git/" ".DS_Store" "__pycache__/" ".venv/"
								 ".mypy_cache/")
      company-dabbrev-minimum-length 2
      company-tooltip-align-annotations t
	  eglot-autoshutdown t
      mc/always-run-for-all t
      python-shell-interpreter "python3"
      python-shell-interpreter-args "-i --simple-prompt"
      org-confirm-babel-evaluate nil
	  org-src-window-setup 'current-window
	  org-edit-src-persistent-message nil
	  org-edit-src-content-indentation 4
      org-hide-leading-stars t
      org-return-follows-link t
      org-support-shift-select t
      org-hide-emphasis-markers nil
      org-babel-python-command "python3 -i --simple-prompt --quiet --pprint --no-banner --no-confirm-exit"
      org-babel-hash-show-time t
      org-startup-indented t
      org-startup-with-inline-images t
      org-startup-with-latex-preview t
      org-adapt-indentation t
      org-special-ctrl-o nil
	  org-special-ctrl-a/e nil
	  org-special-ctrl-k nil
	  org-adapt-indentation nil
	  org-M-RET-may-split-line '((default . nil))
	  diff-default-read-only t
	  diff-advance-after-apply-hunk t
	  diff-update-on-the-fly t
	  diff-font-lock-prettify nil
	  vc-follow-symlinks t
	  calendar-christian-all-holidays-flag t
	  calendar-mark-holidays-flag t
	  calendar-holidays '((holiday-fixed 1 1 "Nowy Rok")
						  (holiday-easter-etc 0 "Pierwszy dzień Wielkanocy")
						  (holiday-easter-etc 1 "Drugi dzień Wielkanocy")
						  (holiday-fixed 1 6 "Trzech Króli")
						  (holiday-fixed 5 1 "Święto Pracy")
						  (holiday-fixed 5 3 "Trzeciego Maja")
						  (holiday-easter-etc 50 "Zielone Świątki")
						  (holiday-easter-etc 60 "Boże Ciało")
						  (holiday-fixed 8 15 "Wniebowzięcie Najświętszej Maryi Panny")
						  (holiday-fixed 11 1 "Wszystkich Świętych")
						  (holiday-fixed 11 11 "Święto Niepodległości")
						  (holiday-fixed 12 25 "Wigilia Bożego Narodzenia")
						  (holiday-fixed 12 26 "Boże Narodzenie"))
	  ispell-choices-buffer "*ispell-top-choices*"
	  message-confirm-send nil
	  message-kill-buffer-on-exit t
	  message-wide-reply-confirm-recipients t
	  eww-history-limit 100
	  eww-browse-url-new-window-is-tab nil
	  eww-search-prefix "https://html.duckduckgo.com/html/?q="
      geiser-default-implementation 'guile
	  display-buffer-alist '(("(\\*Occur\\*|\\*helm.+)"
							  (display-buffer-reuse-mode-window display-buffer-below-selected)
							  (window-height . fit-window-to-buffer)
							  (dedicated . t))

							 ("\\'\\*Async Shell Command\\*\\'"
							  (display-buffer-no-window))

							 ("\\*ispell-top-choices\\*.*"
							  (display-buffer-reuse-mode-window display-buffer-below-selected) (window-height . fit-window-to-buffer))

							 ("^\\*eshell\\*$"
							  (display-buffer-same-window))

							 ("\\(magit: .+\\|magit-log.+\\|magit-revision.+\\)"
							  (display-buffer-same-window)))

	  switch-to-prev-buffer-skip-regexp '("^\\*helm"
										  "^magit:"
										  "^magit-process:"
										  "^magit-diff:"
										  "^\\*scratch\\*$"
										  "^\\*Warnings\\*$"
										  "^\\*Messages\\*$"
										  "^\\.newsrc-dribble$"
										  "^\\*Async-native-compile-log\\*$")
      web-mode-engines-alist '(("php" . "\\.phtml\\'")
			       ("blade" . "\\.blade\\."))
      web-mode-markup-indent-offset 4
      web-mode-css-indent-offset 4
      web-mode-code-indent-offset 4
      web-mode-enable-auto-pairing t
      web-mode-enable-current-element-highlight t
      web-mode-enable-current-column-highlight t
	  tabbar-buffer-groups-function 'my/tab-bar-groups
	  browse-url-new-window-flag t
      browse-url-browser-function 'eww-browse-url
      restclient-log-request t
      eww-auto-rename-buffer 'title
	  treemacs-recenter-after-project-jump 'always
	  treemacs-recenter-after-file-follow 'always
	  treemacs-no-delete-other-windows t
	  treemacs-tag-follow-delay 0.1
	  docker-command "podman"
	  docker-compose-command "podman-compose"
	  proced-auto-update-flag t
	  proced-tree-flag t
	  proced-enable-color-flag t
	  denote-directory "~/denote"
	  denote-known-keywords '("emacs" "programowanie"
							  "technika", "informatyka"
							  "elektronika")
	  denote-sort-keywords t
	  denote-prompts '(title keywords)
	  denote-date-prompt-use-org-read-date t
	  beframe-global-buffers '("*scratch*" "*Messages*" "*Backtrace*")
	  yas-indent-line 'auto
	  yas-also-auto-indent-first-line t
	  smtpmail-debug-info t
	  smtpmail-debug-verb t
	  read-mail-command 'gnus
	  mail-user-agent 'gnus-user-agent
	  message-send-mail-function 'smtpmail-send-it
	  send-mail-function 'smtpmail-send-it
	  nntp-maximum-request 4  ;; max dla eternal-september
	  gnus-always-read-dribble-file nil
	  gnus-use-dribble-file nil
	  gnus-save-newsrc-file nil
	  gnus-read-newsrc-file nil
	  gnus-inhibit-demon t
	  compilation-ask-about-save nil
	  compilation-scroll-output t
      restclient-same-buffer-response nil)

(setq-default dired-kill-when-opening-new-dired-buffer t
              c-default-style "k&r"
              c-basic-offset 4
              c-electric-flag t
			  comment-column 0
			  tab-width 4
			  adaptive-wrap-extra-indent 0
			  display-fill-column-indicator-column 80
			  comint-scroll-to-bottom-on-input t
			  comint-scroll-to-bottom-on-output nil
			  eglot-workspace-configuration '(:pylsp (:plugins (:jedi_completion (:fuzzy t) :jedi (:environment ".venv")))))

(defalias 'yes-or-no-p 'y-or-n-p)
;;(load-theme 'monokai t)
(load-theme 'modus-vivendi t)
(show-paren-mode t)
(tool-bar-mode -1)
(cua-mode t)
(menu-bar-mode -1)
(save-place-mode t)
(column-number-mode t)
(delete-selection-mode t)
(global-auto-revert-mode 1)
(global-tab-line-mode)
(toggle-frame-maximized)
(set-language-environment "UTF-8")
(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-modified-p nil)
(global-visual-line-mode +1)
(global-so-long-mode 1)
(electric-pair-mode t)
(helm-mode 1)
(global-company-mode)
(yas-global-mode 1)
(beframe-mode 1)
(org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp .t)
							 (python . t)
							 (shell . t)
							 (scheme . t)))
(which-key-mode)
(treemacs-project-follow-mode 1)
(treemacs-follow-mode 1)

(set-face-attribute 'region nil :background "#666")
(set-face-attribute 'org-block nil :background "#222")
(set-face-attribute 'org-block-begin-line nil :background "#333")
(set-face-attribute 'org-block-end-line nil :background "#333")

;; hotkeys
(global-set-key (kbd "<escape>") 'my/on-escape)
(global-set-key (kbd "S-<delete>") 'kill-whole-line)
(global-set-key (kbd "C-/") 'evilnc-comment-or-uncomment-lines)
(global-set-key (kbd "C-<down>") 'scroll-up-line)
(global-set-key (kbd "C-<up>") 'scroll-down-line)
(global-set-key (kbd "C-S-<up>") 'my/move-line-up)
(global-set-key (kbd "C-S-<down>") 'my/move-line-down)
(global-set-key (kbd "C-z") 'undo-only)
(global-set-key (kbd "C-S-z") 'undo-redo)
(global-set-key (kbd "S-g") 'goto-line)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-q") 'save-buffers-kill-emacs)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "C-S-p") 'helm-M-x)
(global-set-key (kbd "C-o") 'imenu)
(global-set-key (kbd "M-x") 'kill-this-buffer)
(global-set-key (kbd "M-d") 'insert-current-date)
(global-set-key (kbd "M-<up>") 'my/backward-paragraph)
(global-set-key (kbd "M-<down>") 'my/forward-paragraph)
(global-set-key (kbd "C-p") 'my/helm-find)
(global-set-key (kbd "C-f") 'helm-occur)
(global-set-key (kbd "C-S-f") 'helm-do-grep-ag)
(global-set-key (kbd "<f11>") 'my/grep-under-cursor)
(global-set-key (kbd "C-r") 'query-replace)
(global-set-key (kbd "C-S-r") 'query-replace-regexp)
(global-set-key (kbd "C-<space>") 'company-complete)
(global-set-key (kbd "C-n") 'my/create-new-file)
(global-set-key (kbd "C-S-n") 'my/create-new-directory)
(global-set-key (kbd "C-S-t") 'my/create-new-eshell)
(global-set-key (kbd "M-<right>") 'switch-to-next-buffer)
(global-set-key (kbd "M-<left>") 'switch-to-prev-buffer)
(global-set-key (kbd "C-t") 'my/create-new-term)
(global-set-key (kbd "C-S-d") 'crux-duplicate-current-line-or-region)
(global-set-key (kbd "C-d") 'mc/mark-next-like-this)
(global-set-key (kbd "C-k") 'mc/skip-to-next-like-this)
(global-set-key (kbd "<f1>") 'embark-act)
(global-set-key (kbd "<f6>") 'eglot-format)
(global-set-key (kbd "<f8>") 'treemacs)
(global-set-key (kbd "C-x d") 'my/dired-new)
(global-set-key (kbd "C-x b") 'beframe-switch-buffer)
(global-set-key (kbd "<f12>") 'denote-open-or-create)
(global-set-key (kbd "C-l") 'denote-link-or-create)
(global-set-key (kbd "<Scroll_Lock>") 'scroll-lock-mode)
(define-key mc/keymap (kbd "<return>") nil)
(define-key nxml-mode-map (kbd ">") 'my/finish-element)
(define-key nxml-mode-map (kbd "RET") 'my/nxml-newline)
(define-key hexl-mode-map (kbd "M-<right>") nil)
(define-key hexl-mode-map (kbd "M-<left>") nil)
(define-key hexl-mode-map (kbd "C-q") nil)
(define-key hexl-mode-map (kbd "M-x") nil)
(define-key org-mode-map (kbd "M-<left>") nil)
(define-key org-mode-map (kbd "M-<right>") nil)
(define-key org-mode-map (kbd "M-<up>") nil)
(define-key org-mode-map (kbd "M-<down>") nil)
(define-key org-mode-map (kbd "C-]") 'org-do-demote)
(define-key org-mode-map (kbd "C-}") 'org-do-demote-subtree)
;; (define-key org-mode-map (kbd "C-[") 'org-do-promote)
(define-key org-mode-map (kbd "C-{") 'org-do-promote-subtree)
(define-key python-mode-map (kbd "C-]") 'python-indent-shift-right)
;; (define-key python-mode-map (kbd "C-[") 'python-indent-shift-left)
(define-key org-mode-map (kbd "C-S-<right>") nil)
(define-key org-mode-map (kbd "C-S-<left>") nil)
(define-key org-mode-map (kbd "C-S-<up>") 'org-move-subtree-up)
(define-key org-mode-map (kbd "C-S-<down>") 'org-move-subtree-down)
(define-key package-menu-mode-map (kbd "M-<left>") nil)
(define-key package-menu-mode-map (kbd "M-<right>") nil)

(add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode)
(add-hook 'nxml-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'my/setup-text-mode)
(add-hook 'prog-mode-hook 'my/setup-prog-mode)
(add-hook 'python-mode-hook 'my/setup-python-mode)
(add-hook 'emacs-lisp-mode-hook 'my/setup-elisp-mode)
(add-hook 'sql-interactive-mode-hook 'my/setup-sqli-mode)
(add-hook 'sql-mode-hook 'my/setup-sql-mode)
(add-hook 'before-save-hook 'my/on-before-save)
(add-hook 'org-present-mode-hook 'my/org-present-start)
(add-hook 'org-present-mode-hook 'my/org-present-end)
(add-hook 'eshell-mode-hook 'my/setup-eshell)
(add-hook 'nginx-mode-hook 'my/setup-nginx-mode)
(add-hook 'dired-mode-hook 'my/setup-dired)
(add-hook 'restclient-mode-hook 'my/setup-restclient-mode)
(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
(add-hook 'term-mode-hook 'my/setup-term-mode)
(add-hook 'makefile-mode-hook 'my/setup-makefile-mode)
(add-hook 'eww-mode-hook 'my/setup-eww-mode)
(add-hook 'docker-compose-mode-hook 'my/setup-docker-compose-mode)
(add-hook 'gnus-article-mode-hook 'my/setup-gnus-article-mode)
(add-hook 'gnus-summary-mode-hook 'my/setup-gnus-summary-mode)
(add-hook 'gnus-group-mode-hook 'my/setup-gnus-group-mode)

(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.env\\..*\\'" . dotenv-mode))
(add-to-list 'auto-mode-alist '("docker-compose\\.y.?ml$" . docker-compose-mode))
(add-to-list 'auto-mode-alist '("\\.service$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.csv$" . csv-mode))
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.eln$" . hexl-mode))
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("README$" . text-mode))
(add-to-list 'auto-mode-alist '("LICENSE$" . text-mode))
(add-to-list 'auto-mode-alist '("CHANGELOG$" . text-mode))

(let ((private-settings (file-name-concat user-emacs-directory "private.el")))
  (if (file-exists-p private-settings) (load-file private-settings)))

;;; init.el ends here
