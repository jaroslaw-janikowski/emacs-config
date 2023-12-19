(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")
			 ("gnu" . "https://elpa.gnu.org/packages/"))
      package-archive-priorities '(("melpa" . 4)
				   ("melpa-stable" . 3)
				   ("nongnu" . 2)
				   ("gnu" . 1)))
(package-initialize)
(add-to-list 'load-path (file-name-concat user-emacs-directory "mods"))

(require 'centaur-tabs)
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
  (let ((default-directory command-line-default-directory))
    (eshell 'N)))

(defun my/duplicate-line()
  (interactive)
  (duplicate-line)
  (next-line))

(defun insert-current-date()
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun my/centaur-tabs-groups ()
  "Zasady grupowania buforów w centaur-tabs."
  (list
   (cond
    ((derived-mode-p 'mastodon-mode) "Media")
    ((derived-mode-p 'eww-mode) "Media")
    ((derived-mode-p 'elpher-mode) "Media")
    ((derived-mode-p 'newsticker-mode) "Media")
	((derived-mode-p 'eshell-mode) "Shell")
	((derived-mode-p 'dired-mode) "Shell")

    ((or (string-equal "*" (substring (buffer-name) 0 1))
	 (memq major-mode '(magit-process-mode
			    magit-status-mode
			    magit-diff-mode
			    magit-log-mode
			    magit-file-mode
			    magit-blob-mode
			    magit-blame-mode
			    )))
     "Emacs")

    ((derived-mode-p 'emacs-lisp-mode)
     "Elisp")
    ((memq major-mode '(org-mode org-agenda-mode diary-mode))
     "OrgMode")
    (t
     (centaur-tabs-get-group-name (current-buffer))))))

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
  (keyboard-escape-quit)
  (mc/keyboard-quit))

(defun helm-find--build-cmd-line ()
  "Specjalna wersja funkcji zapewniająca wyszukiwanie rekurencyjne dla helm-find."
  (require 'find-cmd)
  (let* ((command-line-default-directory (or (file-remote-p command-line-default-directory 'localname)
                                command-line-default-directory))
         (patterns+options (split-string helm-pattern "\\(\\`\\| +\\)\\* +"))
         (fold-case (helm-set-case-fold-search (car patterns+options)))
         (patterns (split-string (car patterns+options)))
         (additional-options (and (cdr patterns+options)
                                  (list (concat (cadr patterns+options) " "))))
         (ignored-dirs ())
         (ignored-files (when helm-findutils-skip-boring-files
                          (cl-loop for f in completion-ignored-extensions
                                   if (string-match "/$" f)
                                   do (push (replace-match "" nil t f)
                                            ignored-dirs)
                                   else collect (concat "*" f))))
         (path-or-name (if helm-findutils-search-full-path
                           '(ipath path) '(iname name)))
         (name-or-iname (if fold-case
                            (car path-or-name) (cadr path-or-name))))
    (find-cmd (and ignored-dirs
                   `(prune (name ,@ignored-dirs)))
              (and ignored-files
                   `(not (name ,@ignored-files)))
              `(and ,@(mapcar
                       (lambda (pattern)
                         `(,name-or-iname ,(concat "**" pattern "*"))) ;;; <- fix
                       patterns)
                    ,@additional-options))))

(defun helm-find (arg)
  "Patch umożliwiający wysukiwanie plików z poziomu katalogu root projektu."
  (interactive "P")
  (let ((directory
	 (if arg
	     (file-name-as-directory
	      (read-directory-name "DefaultDirectory: "))
	   command-line-default-directory)))  ;;; <- fix
    (helm-find-1 directory)))

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

(defun my/c-mode-newline-between-braces ()
  "Wcięcie gdy kursor znajduje się pomiędzy klamrami {|}."
  (interactive)
  (if (and (eq major-mode 'c-mode)
           (eq (char-after) ?})
           (eq (char-before) ?{))
      (progn
        (newline-and-indent)
        (c-indent-line-or-region)
        (forward-line -1)
        (end-of-line)
        (newline-and-indent)
        (c-indent-line-or-region))
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
  (setq company-backends '((company-ispell company-dabbrev)))
  (flyspell-mode)
  (ispell-change-dictionary "polish"))

(defun my/setup-prog-mode()
  (setq company-backends '((company-yasnippet
			    company-dabbrev-code
			    company-keywords
			    company-files
			    company-capf)))
  (flyspell-prog-mode)
  (display-fill-column-indicator-mode)
  (ispell-change-dictionary "british")
  (idle-highlight-mode t))

(defun my/setup-eshell()
  (setq company-backends '((company-files company-yasnippet))))

(defun my/setup-dired()
  (nerd-icons-dired-mode))

(defun my/setup-python-mode()
  (eglot-ensure))

(defun my/setup-sqli-mode()
  (toggle-truncate-lines t))

(defun my/setup-sql-mode()
  (sql-set-dialect 'postgres))

(defun my/on-before-save()
  (delete-trailing-whitespace))

(setq warning-minimum-level :error
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
	  kill-do-not-save-duplicates t
	  comment-empty-lines t
	  comment-fill-column nil
	  comment-multi-line t
	  comment-style 'multi-line
      dired-auto-revert-buffer t
      dired-confirm-shell-command nil
      dired-clean-confirm-killing-deleted-buffers nil
      dired-no-confirm t
	  dired-recursive-copies 'always
      dired-recursive-deletes 'always
      dired-deletion-confirmer '(lambda (x) t)
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
      centaur-tabs-style "slant"
      centaur-tabs-set-icons t
      centaur-tabs-set-bar 'over
      centaur-tabs-show-navigation-buttons t
      centaur-tabs-buffer-groups-function 'my/centaur-tabs-groups
      company-minimum-prefix-length 2
      centaur-tabs-set-modified-marker "*"
      company-idle-delay 0
      company-selection-wrap-around t
      company-files-exclusions '(".git/" ".DS_Store" "__pycache__/" ".venv/")
      company-dabbrev-minimum-length 2
      company-tooltip-align-annotations t
      mc/always-run-for-all t
      python-shell-interpreter "ipython3"
      python-shell-interpreter-args "-i --simple-prompt"
      org-confirm-babel-evaluate nil
	  org-src-window-setup 'current-window
	  org-edit-src-persistent-message nil
      org-hide-leading-stars t
      org-return-follows-link t
      org-support-shift-select t
      org-hide-emphasis-markers nil
      org-babel-python-command "ipython3 -i --simple-prompt --quiet --pprint --no-banner --no-confirm-exit"
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
	  ispell-choices-buffer "*ispell-top-choices*"
	  message-confirm-send nil
	  message-kill-buffer-on-exit t
	  message-wide-reply-confirm-recipients t
	  eww-history-limit 100
	  eww-browse-url-new-window-is-tab nil
      geiser-default-implementation 'guile
      centaur-tabs-excluded-prefixes '("*Messages*"
				       "*epc"
				       "*helm find*"
				       "*helm"
				       "*Helm"
				       "*help"
				       "*Help"
				       "*Compile-Log"
				       "*lsp"
				       "*LSP"
				       "*company"
				       "*Flycheck"
				       "*Async-native-compile-log*"
				       "*Warnings"
				       "*Messages*"
				       "*html*"
					   "magit-process: "
					   "magit-diff: "
					   "magit: "
					   "COMMIT_EDITMSG"
				       "*Flymake log*"
				       "*EGLOT"
				       "*straight"
				       "*Backtrace*"
				       "*Ediff"
				       "*ediff"
				       "*tramp"
					   "*which-key*"
				       "plstore "
				       )
	  display-buffer-alist '(
							 ("\\'\\*Async Shell Command\\*\\'"
							  (display-buffer-no-window))
							 ("\\*ispell-top-choices\\*.*"
							  (display-buffer-reuse-mode-window display-buffer-below-selected)
							  (window-height . fit-window-to-buffer))
							 ("\\(magit: .+\\|magit-log.+\\|magit-revision.+\\)"
							  (display-buffer-full-frame)))
      web-mode-engines-alist '(("php" . "\\.phtml\\'")
			       ("blade" . "\\.blade\\."))
      web-mode-markup-indent-offset 4
      web-mode-css-indent-offset 4
      web-mode-code-indent-offset 4
      web-mode-enable-auto-pairing t
      web-mode-enable-current-element-highlight t
      web-mode-enable-current-column-highlight t
      browse-url-browser-function 'eww-browse-url
      restclient-log-request t
      eww-auto-rename-buffer 'title
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
			  comint-scroll-to-bottom-on-output nil)

(defalias 'yes-or-no-p 'y-or-n-p)
(load-theme 'monokai t)
(show-paren-mode t)
(tool-bar-mode -1)
(cua-mode t)
(menu-bar-mode -1)
(save-place-mode t)
(centaur-tabs-mode t)
(global-display-line-numbers-mode t)
(global-hl-line-mode t)
(column-number-mode t)
(delete-selection-mode t)
(global-auto-revert-mode 1)
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
(org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp .t)
							 (python . t)
							 (shell . t)
							 (scheme . t)))
(which-key-mode)

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
(global-set-key (kbd "C-g") 'goto-line)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-q") 'save-buffers-kill-emacs)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "M-g") 'centaur-tabs--groups-menu)
(global-set-key (kbd "C-S-p") 'helm-M-x)
(global-set-key (kbd "M-x") 'kill-this-buffer)
(global-set-key (kbd "M-d") 'insert-current-date)
(global-set-key (kbd "M-<left>") 'centaur-tabs-backward)
(global-set-key (kbd "M-<right>") 'centaur-tabs-forward)
(global-set-key (kbd "M-<up>") 'my/backward-paragraph)
(global-set-key (kbd "M-<down>") 'my/forward-paragraph)
(global-set-key (kbd "C-p") 'helm-find)
(global-set-key (kbd "C-f") 'helm-occur)
(global-set-key (kbd "C-S-f") 'helm-do-grep-ag)
(global-set-key (kbd "<f12>") 'my/grep-under-cursor)
(global-set-key (kbd "C-r") 'query-replace)
(global-set-key (kbd "C-S-r") 'query-replace-regexp)
(global-set-key (kbd "C-<space>") 'company-complete)
(global-set-key (kbd "C-n") 'my/create-new-file)
(global-set-key (kbd "C-S-n") 'my/create-new-directory)
(global-set-key (kbd "C-S-t") 'my/create-new-eshell)
(global-set-key (kbd "C-S-d") 'my/duplicate-line)
(global-set-key (kbd "C-d") 'mc/mark-next-like-this)
(global-set-key (kbd "C-k") 'mc/skip-to-next-like-this)
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
(define-key org-mode-map (kbd "C-]") 'org-indent-item-tree)
;; (define-key org-mode-map (kbd "C-[") 'org-outdent-item-tree)
(define-key org-mode-map (kbd "C-S-<right>") nil)
(define-key org-mode-map (kbd "C-S-<left>") nil)
(define-key org-mode-map (kbd "C-S-<up>") nil)
(define-key org-mode-map (kbd "C-S-<down>") nil)

(add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode)
(add-hook 'c-mode-hook (lambda () (local-set-key (kbd "RET") 'my/c-mode-newline-between-braces)))
(add-hook 'nxml-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'my/setup-text-mode)
(add-hook 'prog-mode-hook 'my/setup-prog-mode)
(add-hook 'python-mode-hook 'my/setup-python-mode)
(add-hook 'sql-interactive-mode-hook 'my/setup-sqli-mode)
(add-hook 'sql-mode-hook 'my/setup-sql-mode)
(add-hook 'before-save-hook 'my/on-before-save)
(add-hook 'org-present-mode-hook 'my/org-present-start)
(add-hook 'org-present-mode-hook 'my/org-present-end)
(add-hook 'eshell-mode-hook 'my/setup-eshell)
(add-hook 'dired-mode-hook 'my/setup-dired)

(add-to-list 'auto-mode-alist '("^.*\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.service" . conf-mode))
(add-to-list 'auto-mode-alist '("README" . text-mode))
(add-to-list 'auto-mode-alist '("LICENSE" . text-mode))
(add-to-list 'auto-mode-alist '("CHANGELOG" . text-mode))

(let ((private-settings (file-name-concat user-emacs-directory "private.el")))
  (if (file-exists-p private-settings) (load-file private-settings)))

;;; init.el ends here
