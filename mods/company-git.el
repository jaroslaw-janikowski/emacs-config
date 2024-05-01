(require 'company)
(require 'cl-lib)

(defconst completions
  '(#("git status" 0 1
	  (:annotation
	   "company-git"
	   :help
	   "help str"))
	#("git reset" 0 1
	  (:annotation
	   "company-git"
	   :help
	   "sdf dsfds"))
	#("git reset --hard" 0 1
	  (:annotation
	  "company-git"
	  :help
	  "sdfsd"))
	#("git reset --soft" 0 1
	  (:annotation
	   "company-git"
	   :help
	   "sdfsdf"))
	#("git pull" 0 1
	  (:annotation
	   "company-git"
	   :help
	   "dfsd fds"))
	#("git push" 0 1
	  (:annotation
	   "company-git"
	   :help
	   "sfsdfsd"))
	#("git clean" 0 1
	  (:annotation
	   "company-git"
	   :help
	   "sdfdsf"))
	#("git clean -df" 0 1
	  (:annotation
	   "company-git"
	   :help
	   "sdfdsgd"))
	#("git clone --depth=1" 0 1
	  (:annotation "company-git" :help "Clone repository with minimal depth."))
	#("git remote" 0 1
	  (:annotation
	   "company-git"
	   :help
	   "sdfds"))))

(defun company-git-annotation (s)
  (format " %s" (get-text-property 0 :annotation s)))

(defun company-git-meta (s)
  (get-text-property 0 :help s))

(defun company-git (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
	(interactive (company-begin-backend 'company-git))
	(prefix (company-grab-line "git .*"))
	(candidates (cl-remove-if-not
				 (lambda (c) (string-prefix-p arg c))
				 completions))
	(meta (company-git-meta arg))
	(annotation (company-git-annotation arg))))

(provide 'company-git)
