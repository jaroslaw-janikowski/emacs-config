(require 'nxml-mode)
(require 'yasnippet)
(require 'helm-find)

(defun helm-find--build-cmd-line ()
  "Specjalna wersja funkcji zapewniająca wyszukiwanie rekurencyjne dla helm-find."
  (require 'find-cmd)
  (let* ((default-directory (or (file-remote-p default-directory 'localname)
                                default-directory))
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

(add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode)
(add-hook 'c-mode-hook (lambda () (local-set-key (kbd "RET") 'my/c-mode-newline-between-braces)))
(add-hook 'nxml-mode-hook 'display-line-numbers-mode)

(load-theme 'wheatgrass)
(show-paren-mode t)
(tool-bar-mode -1)
(cua-mode t)
(menu-bar-mode -1)
(save-place-mode t)
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
(helm-mode 1)
(global-company-mode)
(yas-global-mode 1)

(setq warning-minimum-level :error
      inhibit-startup-screen t
      byte-compile-warnings nil
      gc-cons-threshold 5000000
      make-backup-files nil
      auto-save-default nil
      create-lockfiles nil
      large-file-warning-threshold 100000000
      ring-bell-function 'ignore
      use-short-answers t
      confirm-kill-processes nil
      require-final-newline t
      executable-prefix-env t
      initial-scratch-message nil
      tab-width 4
      enable-local-variables nil
      nxml-slash-autocomplete-flag t
      nxml-mode-map (make-keymap)
      company-minimum-prefix-length 2
      company-idle-delay 0
      company-selection-wrap-around t
      company-files-exclusions '(".git/" ".DS_Store" "__pycache__/"))

(setq-default dired-kill-when-opening-new-dired-buffer t
              c-default-style "k&r"
              c-basic-offset 4
              c-electric-flag t
	      adaptive-wrap-extra-indent 0)

(add-to-list 'company-backends '(company-yasnippet))

;; hotkeys
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "S-<delete>") 'kill-whole-line)
(global-set-key (kbd "C-/") 'comment-or-uncomment-region)
(global-set-key (kbd "C-<down>") 'scroll-up-line)
(global-set-key (kbd "C-<up>") 'scroll-down-line)
(global-set-key (kbd "C-S-<up>") 'my/move-line-up)
(global-set-key (kbd "C-S-<down>") 'my/move-line-down)
(global-set-key (kbd "C-z") 'undo-only)
(global-set-key (kbd "C-S-z") 'undo-redo)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-q") 'save-buffers-kill-emacs)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "C-S-p") 'helm-M-x)
(global-set-key (kbd "M-x") 'kill-this-buffer)
(global-set-key (kbd "M-<left>") 'previous-buffer)
(global-set-key (kbd "M-<right>") 'next-buffer)
(global-set-key (kbd "C-p") 'helm-find)
(global-set-key (kbd "C-f") 'helm-occur)
(global-set-key (kbd "C-n") 'my/create-new-file)
(global-set-key (kbd "C-S-n") 'my/create-new-directory)
(define-key nxml-mode-map (kbd ">") 'my/finish-element)
(define-key nxml-mode-map (kbd "RET") 'my/nxml-newline)

;;; init.el ends here
