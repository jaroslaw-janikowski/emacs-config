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
(package-install 'helm)
(package-install 'company)
(package-install 'yasnippet)
(package-install 'geiser-guile)
(package-install 'markdown-mode)
(package-install 'web-mode)
(package-install 'magit)
(package-install 'multiple-cursors)
(package-install 'idle-highlight-mode)
(package-install 'docker)
(package-install 'dockerfile-mode)
(package-install 'org-present)
(package-install 'visual-fill-column)
(package-install 'centaur-tabs)
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
(package-install 'monokai-theme)
(package-install 'nerd-icons)
(package-install 'nerd-icons-dired)
(package-install 'php-mode)
(package-install 'docker-compose-mode)
(package-install 'denote)
(package-install 'beframe)
(package-install 'dape)
(package-install 'pyvenv)
(package-install 'company-php)
(package-install 'ac-php)
(package-install 'mpv)
(package-install 'editorconfig)
(package-install 'editorconfig-generate)
(nerd-icons-install-fonts)
