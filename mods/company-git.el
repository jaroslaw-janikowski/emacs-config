(require 'company)
(require 'cl-lib)

(defvar company-git--context "")

(defun company-git--completions ()
  (cond
   ((string= company-git--context "git")
	'(#("status" 0 1 (:annotation "company-git" :help "sdfdsf"))
	  #("reset" 0 1 (:annotation "company-git" :help "sdfsdf"))
	  #("pull" 0 1 (:annotation "company-git" :help "sdfsdf"))
	  #("push" 0 1 (:annotation "company-git" :help "sdfsdf"))
	  #("clean" 0 1 (:annotation "company-git" :help "sdfsdf"))
	  #("clone" 0 1 (:annotation "company-git" :help "sdfsdf"))
	  #("remote" 0 1 (:annotation "company-git" :help "sdfsdf"))))
	((string= company-git--context "reset")
	 '(#("--hard" 0 1 (:annotation "company-git" :help "sdfsdf"))
	   #("--soft" 0 1 (:annotation "company-git" :help "sdfsdf"))))
	((string= company-git--context "clean")
	 '(#("-df" 0 1 (:annotation "company-git" :help "sdfds"))))
	((string= company-git--context "clone")
	 '(#("--depth=1" 0 1 (:annotation "company-git" :help "sdfds"))))
   (t nil)))

(defun company-git-annotation (s)
  (format " %s" (get-text-property 0 :annotation s)))

(defun company-git-meta (s)
  (get-text-property 0 :help s))

(defun company-git (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
	(interactive (company-begin-backend 'company-git))
	(prefix (when (looking-back "\\([a-zA-Z0-9]+\\) \\(.*\\)" 10 t)
			  (progn
				(setq company-git--context (match-string 1))
				;; (message "%s %s" company-git--context (match-string 2))
				(match-string 2))))
	(candidates (cl-remove-if-not
				 (lambda (c) (string-prefix-p arg c))
				 (company-git--completions)))
	(meta (company-git-meta arg))
	(annotation (company-git-annotation arg))))

(provide 'company-git)
