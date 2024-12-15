(require 'exwm)

(defun my-exwm-update-title ()
  (pcase exwm-class-name
	("Firefox" (exwm-workspace-rename-buffer (format "Firefox: %s" exwm-title)))))

(exwm-enable)
(exwm-randr-mode)

(add-hook 'exwm-randr-screen-change-hook
		  (lambda ()
			(message "Resizing screen...")  ;; Do not remove. Resize does not work without this line :P
			(exwm-randr-refresh)
			(message "Done.")))

(add-hook 'exwm-update-title-hook #'my-exwm-update-title)

;;; exwmcfg.el ends here
