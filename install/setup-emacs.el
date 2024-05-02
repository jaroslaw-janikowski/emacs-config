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

(package-refresh-contents)
(package-install 'adaptive-wrap)
(package-install 'drag-stuff)
(package-install 'company)
(package-install 'company-quickhelp)
(package-install 'yasnippet)
(package-install 'geiser-guile)
(package-install 'markdown-mode)
(package-install 'web-mode)
(package-install 'magit)
(package-install 'git-modes)
(package-install 'multiple-cursors)
(package-install 'idle-highlight-mode)
(package-install 'docker)
(package-install 'dockerfile-mode)
(package-install 'org-present)
(package-install 'visual-fill-column)
(package-install 'which-key)
(package-install 'evil-nerd-commenter)
(package-install 'crux)
(package-install 'dotenv-mode)
(package-install 'company-shell)
(package-install 'nginx-mode)
(package-install 'company-nginx)
(package-install 'restclient)
(package-install 'company-restclient)
(package-install 'diff-hl)
(package-install 'csv-mode)
(package-install 'rainbow-mode)
(package-install 'embark)
(package-install 'treemacs)
(package-install 'dired-subtree)
(package-install 'nerd-icons)
(package-install 'nerd-icons-dired)
(package-install 'docker-compose-mode)
(package-install 'denote)
(package-install 'dape) ;; nie działa w emacs 28
(package-install 'beframe)
(package-install 'pyvenv)
(package-install 'mpv)
(package-install 'editorconfig)
(package-install 'editorconfig-generate)
(package-install 'nsis-mode)
(package-install 'apache-mode)
(package-install 'uuidgen)
(package-install 'helm-fuzzy-find)
(package-install 'lorem-ipsum)
(package-install 'gptel)
(package-install 'emmet-mode)
(package-install 'css-eldoc)
(package-install 'js-doc)
(package-install 'js2-mode)
(package-install 'tide)
(package-install 'ac-php)
(package-install 'company-php)
(package-install 'lsp-mode)
(package-install 'lsp-ui)
(package-install 'consult)
(nerd-icons-install-fonts)
