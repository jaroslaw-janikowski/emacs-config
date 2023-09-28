(add-to-list 'load-path (file-name-concat user-emacs-directory "mods"))

(require 'package)
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

(defun centaur-tabs-hide-tab(tab)
  (let ((name (format "%s" tab)))
    (or
     (window-dedicated-p (selected-window))
     (string-prefix-p "*helm find*" name)
     (string-prefix-p "*helm occur*" name)
     (string-prefix-p "*Async-native-compile-log*" name)
     (string-prefix-p "magit:" name)
     (string-prefix-p "magit-process:" name)
     (string-prefix-p "magit-diff:" name)
     )))

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
  (flyspell-mode))

(defun my/setup-prog-mode()
  (setq company-backends '((company-yasnippet company-dabbrev-code company-keywords company-files company-capf)))
  (flyspell-prog-mode)
  (idle-highlight-mode t))

(defun my/setup-eshell()
  (setq company-backends '((company-files))))

(defun my/setup-python-mode()
  (eglot-ensure))

(defun my/setup-sqli-mode()
  (toggle-truncate-lines t))

(defun my/on-before-save()
  (delete-trailing-whitespace))

(load-theme 'wheatgrass)
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
(global-visual-line-mode +1)
(electric-pair-mode t)
(helm-mode 1)
(global-company-mode)
(yas-global-mode 1)
(org-babel-do-load-languages 'org-babel-load-languages '((python . t) (shell . t) (scheme . t)))

(setq warning-minimum-level :error
      inhibit-startup-screen t
      byte-compile-warnings nil
      gc-cons-threshold 10000000
      gc-cons-percentage 0.1
      garbage-collection-messages nil
      make-backup-files nil
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
      confirm-kill-processes nil
      package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")
			 ("gnu" . "https://elpa.gnu.org/packages/"))
      package-archive-priorities '(("melpa" . 4)
				   ("melpa-stable" . 3)
				   ("nongnu" . 2)
				   ("gnu" . 1))
      require-final-newline t
      executable-prefix-env t
      initial-scratch-message nil
      tab-width 4
      enable-local-variables nil
      dired-auto-revert-buffer t
      initial-major-mode 'org-mode
      nxml-slash-autocomplete-flag t
      nxml-mode-map (make-keymap)
      centaur-tabs-style "slant"
      centaur-tabs-set-icons t
      centaur-tabs-set-bar 'over
      centaur-tabs-show-navigation-buttons t
      company-minimum-prefix-length 2
      centaur-tabs-set-modified-marker "*"
      company-idle-delay 0
      company-selection-wrap-around t
      company-files-exclusions '(".git/" ".DS_Store" "__pycache__/" ".venv/")
      company-dabbrev-minimum-length 2
      python-shell-interpreter "ipython3"
      python-shell-interpreter-args "-i --simple-prompt"
      org-confirm-babel-evaluate nil
      org-hide-leading-stars t
      org-return-follows-link t
      org-support-shift-select t
      org-hide-emphasis-markers t
      org-babel-python-command "ipython3 -i --simple-prompt"
      geiser-default-implementation 'guile
      switch-to-prev-buffer-skip-regexp '("^\*Messages\*"
					  "^\*Async-native-compile-log\*"
					  "^\*Compile-Log\*"
					  "^\*Warnings\*"
					  "^\*Messages\*"
					  "^\*html\*"
					  "^\*helm*"
					  "^\*Flymake log\*"
					  "^\*EGLOT"
					  "^\*Backtrace\*"
					  "^magit"
					  )
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
      restclient-same-buffer-response nil)

(setq-default dired-kill-when-opening-new-dired-buffer t
              c-default-style "k&r"
              c-basic-offset 4
              c-electric-flag t
	      adaptive-wrap-extra-indent 0)

(set-face-attribute 'region nil :background "#666")

;; hotkeys
(global-set-key (kbd "<escape>") 'my/on-escape)
(global-set-key (kbd "S-<delete>") 'kill-whole-line)
(global-set-key (kbd "C-/") 'comment-or-uncomment-region)
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
(global-set-key (kbd "C-S-p") 'helm-M-x)
(global-set-key (kbd "M-x") 'kill-this-buffer)
(global-set-key (kbd "M-<left>") 'centaur-tabs-backward)
(global-set-key (kbd "M-<right>") 'centaur-tabs-forward)
(global-set-key (kbd "C-p") 'helm-find)
(global-set-key (kbd "C-f") 'helm-occur)
(global-set-key (kbd "C-S-f") 'helm-do-grep-ag)
(global-set-key (kbd "<f12>") 'my/grep-under-cursor)
(global-set-key (kbd "C-<space>") 'company-complete)
(global-set-key (kbd "C-n") 'my/create-new-file)
(global-set-key (kbd "C-S-n") 'my/create-new-directory)
(global-set-key (kbd "C-S-t") 'eshell)
(global-set-key (kbd "C-d") 'mc/mark-next-like-this)
(define-key mc/keymap (kbd "<return>") nil)
(define-key nxml-mode-map (kbd ">") 'my/finish-element)
(define-key nxml-mode-map (kbd "RET") 'my/nxml-newline)
(define-key hexl-mode-map (kbd "M-<right>") nil)
(define-key hexl-mode-map (kbd "M-<left>") nil)
(define-key hexl-mode-map (kbd "C-q") nil)
(define-key hexl-mode-map (kbd "M-x") nil)
(define-key org-mode-map (kbd "M-<left>") nil)
(define-key org-mode-map (kbd "M-<right>") nil)
(define-key org-mode-map (kbd "C-]") 'org-indent-item-tree)
(define-key org-mode-map (kbd "C-[") 'org-outdent-item-tree)
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
(add-hook 'before-save-hook 'my/on-before-save)
(add-hook 'org-present-mode-hook 'my/org-present-start)
(add-hook 'org-present-mode-hook 'my/org-present-end)
(add-hook 'eshell-mode-hook 'my/setup-eshell)

(add-to-list 'auto-mode-alist '("^.*\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(if (file-exists-p "private.el") (load-file "private.el"))

;;; init.el ends here
