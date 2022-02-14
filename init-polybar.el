      ;; Make sure the server is started (better to do this in your main Emacs config!)
      (server-start)

        (defvar efs/polybar-process nil
          "Holds the process of the running Polybar instance, if any")

        (defun efs/kill-panel ()
          (interactive)
          (when efs/polybar-process
            (ignore-errors
              (kill-process efs/polybar-process)))
          (setq efs/polybar-process nil))

        (defun efs/start-panel ()
          (interactive)
          (efs/kill-panel)
          (setq efs/polybar-process (start-process-shell-command "polybar" nil "polybar panel")))

(defun efs/send-polybar-hook (module-name hook-index)
  (start-process-shell-command "polybar-msg" nil (format "polybar-msg hook %s %s" module-name hook-index)))

(defun efs/send-polybar-exwm-workspace ()
  (efs/send-polybar-hook "exwm-workspace" 1))

  (defun efs/polybar-exwm-workspace ()
      (pcase exwm-workspace-current-index
        (0 "")
        (1 "")
        (2 "")
        (3 "")
        (4 "")))

;; Update panel indicator when workspace changes
(add-hook 'exwm-workspace-switch-hook #'efs/send-polybar-exwm-workspace)
