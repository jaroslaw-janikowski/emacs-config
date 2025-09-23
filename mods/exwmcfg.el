;; -*- lexical-binding: t -*-

(require 'exwm)

(defun my-exwm-update-title ()
  (pcase exwm-class-name
    ("Firefox" (exwm-workspace-rename-buffer (format "Firefox: %s" exwm-title)))))

(defun my-exwm-update-screen ()
  (message "Resizing screen...")  ;; Do not remove. Resize does not work without this line :P
  (exwm-randr-refresh)
  (message "Done."))

(add-hook 'exwm-randr-screen-change-hook #'my-exwm-update-screen)
(add-hook 'exwm-update-title-hook #'my-exwm-update-title)

(exwm-input-set-key (kbd "M-<right>") 'tab-line-switch-to-next-tab)
(exwm-input-set-key (kbd "M-<left>") 'tab-line-switch-to-prev-tab)

(exwm-wm-mode)
(exwm-randr-mode)

(provide 'exwmcfg)

;;; exwmcfg.el ends here
