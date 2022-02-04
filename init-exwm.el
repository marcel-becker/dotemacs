(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpas" . "https://melpa.org/packages/")))
(setq marcel-lisp-dir "~/Dropbox/.emacs.d/")

(require 'gnutls)
(add-to-list 'gnutls-trustfiles "/usr/local/etc/openssl/cert.pem")


(setq package-enable-at-startup nil)
(setq package-check-signature nil)
(setq package--init-file-ensured nil)
(setq package-user-dir (concat marcel-lisp-dir "elpa"))
(add-to-list 'load-path (concat marcel-lisp-dir "elpa"))
(add-to-list 'load-path (expand-file-name "~/Dropbox/.emacs.d/elpa/xelb-0.18"))
(add-to-list 'load-path (expand-file-name "~/Dropbox/.emacs.d/elpa/exwm-0.26"))

(package-initialize)
(require 'use-package)
(setq use-package-always-ensure t
      use-package-verbose nil
      use-package-compute-statistics nil)


(defun efs/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun efs/set-wallpaper ()
  (interactive)
  ;; NOTE: You will need to update this to a valid background path!
  (start-process-shell-command
   "feh" nil  "feh --bg-scale /usr/share/backgrounds/matt-mcnulty-nyc-2nd-ave.jpg"))

(defun efs/exwm-init-hook ()
  ;; Make workspace 1 be the one where we land at startup
  (exwm-workspace-switch-create 1)

  ;; Open eshell by default
  ;;(eshell)

  ;; Show battery status in the mode line
  ;;(display-battery-mode 1)

  ;; Show the time and date in modeline
  (setq display-time-day-and-date t)
  ;;(display-time-mode 1)
  ;; Also take a look at display-time-format and format-time-string

  ;; Start the Polybar panel
  (efs/start-panel)

  ;; Launch apps that will run in the background
  (efs/run-in-background "nm-applet")
;;  (efs/run-in-background "pasystray")
  (efs/run-in-background "blueman-applet"))

(defun efs/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(defun efs/exwm-update-title ()
  (pcase exwm-class-name
    ("Firefox" (exwm-workspace-rename-buffer (format "Firefox: %s" exwm-title)))))

(defun efs/configure-window-by-class ()
  (interactive)
  (pcase exwm-class-name
    ("Firefox" (exwm-workspace-move-window 2))
    ("Sol" (exwm-workspace-move-window 3))
    ("mpv" (exwm-floating-toggle-floating)
     (exwm-layout-toggle-mode-line))))


(use-package windower)

(use-package desktop-environment
  :after exwm
  :config (desktop-environment-mode)
  :custom
  (desktop-environment-brightness-small-increment "2%+")
  (desktop-environment-brightness-small-decrement "2%-")
  (desktop-environment-brightness-normal-increment "5%+")
  (desktop-environment-brightness-normal-decrement "5%-"))



(use-package exwm
  :init
  ;; Set the default number of workspaces
  (setq exwm-workspace-number 5)
  (menu-bar-mode -1)
  (tool-bar-mode 1)

  ;; When window "class" updates, use it to set the buffer name
  (add-hook 'exwm-update-class-hook #'efs/exwm-update-class)

  ;; When window title updates, use it to set the buffer name
  (add-hook 'exwm-update-title-hook #'efs/exwm-update-title)

  ;; Configure windows as they're created
  (add-hook 'exwm-manage-finish-hook #'efs/configure-window-by-class)

  ;; When EXWM starts up, do some extra confifuration
  (add-hook 'exwm-init-hook #'efs/exwm-init-hook)

  ;; Rebind CapsLock to Ctrl
  ;;(start-process-shell-command "xmodmap" nil "xmodmap ~/.emacs.d/exwm/Xmodmap")

  ;; Set the screen resolution (update this to be the correct resolution for your screen!)
  (require 'exwm-randr)
  (exwm-randr-enable)
  ;;(start-process-shell-command "xrandr" nil "xrandr --output Virtual-1 --primary --mode 2048x1152 --pos 0x0 --rotate normal")
  ;;(start-process-shell-command "xrandr" nil "xrandr --output LVDS-1 --primary --mode 1024x768 --pos 0x0 --rotate normal --output VGA-1 --off --output HDMI-1 --off --output DP-1 --off")
  ;; Set the wallpaper after changing the resolution
  ;;(efs/set-wallpaper)

  ;; Load the system tray before exwm-init
;;  (require 'exwm-systemtray)
;;  (setq exwm-systemtray-height 32)
;;  (exwm-systemtray-enable)

  ;; These keys should always pass through to Emacs
  (setq exwm-input-prefix-keys
        '(?\C-x
          ?\C-u
          ?\C-h
          ?\M-x
          ?\M-`
          ?\M-&
          ?\M-:
          ?\C-\M-j  ;; Buffer list
          ?\C-\ ))  ;; Ctrl+Space

  ;; Ctrl+Q will enable the next key to be sent directly
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  ;; Set up global key bindings.  These always work, no matter the input state!
  ;; Keep in mind that changing this list after EXWM initializes has no effect.
  (setq exwm-input-global-keys
        `(
          ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
          ([?\s-r] . exwm-reset)
          ;; Move between windows
          ([s-left] . windmove-left)
          ([s-right] . windmove-right)
          ([s-up] . windmove-up)
          ([s-down] . windmove-down)

          ;; Launch applications via shell command
          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))

          ;; Switch workspace
          ([?\s-w] . exwm-workspace-switch)
          ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))

          ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))
          ,@(mapcar (lambda (c)
                         `(,(kbd (format "C-s-%d" c)) .
                           (lambda ()
                             (interactive)
                             (exwm-workspace-move-window ,c)
                             (exwm-workspace-switch ,c))))
                       (number-sequence 0 9))

          ;; 'S-s-N': Move window to, and switch to, a certain workspace.
          ;; ,@(cl-mapcar (lambda (c n)
          ;;                `(,(kbd (format "s-%c" c)) .
          ;;                  (lambda ()
          ;;                    (interactive)
          ;;                    (exwm-workspace-move-window ,n)
          ;;                    (exwm-workspace-switch ,n))))
          ;;              '(?\) ?! ?@ ?# ?$ ?% ?^ ?& ?* ?\()
          ;;              ;; '(?\= ?! ?\" ?# ?¤ ?% ?& ?/ ?\( ?\))
          ;;              (number-sequence 0 9))

          (,(kbd "s-b") . helm-mini) ;; list and select buffers
          (,(kbd "s-c") . helm-resume) ;; Continue in latest helm selection buffer
          (,(kbd "s-G") . helm-locate) ;; locate file, based in Linux locate command
          ;; (,(kbd "s-g") . mu-helm-file-search) ;; Grep search in files, see https://www.manueluberti.eu/emacs/2020/02/22/ripgrepping-with-helm/
          ;; (,(kbd "s-g") . ambrevar/helm-grep-git-or-ag) ;; Grep search in files, see https://gitlab.com/ambrevar/dotfiles/-/blob/master/.emacs.d/lisp/init-helm.el
          (,(kbd "s-g") . helm-do-grep-ag) ;; Grep search in files
          (,(kbd "s-r") . helm-run-external-command) ;; Start an application, such as google-chrome
          (,(kbd "s-W") . helm-exwm-switch-browser) ;; Switch to some browser windows
          (,(kbd "s-m") . (lambda () ;; Toggle display of mode-line and minibuffer, in an EXWM window
                            (interactive)
                            (exwm-layout-toggle-mode-line)
                            (exwm-workspace-toggle-minibuffer)))
          (,(kbd "s-i") . exwm-input-toggle-keyboard) ;; Toggle between "line-mode" and "char-mode" in an EXWM window
          ;; 's-r': Reset (to line-mode).
          (,(kbd "s-R") . exwm-reset)

          ;; Bind "s-<f2>" to "slock", a simple X display locker.
          (,(kbd "s-<f2>") . (lambda ()
                               (interactive)
                               (start-process "" nil "/usr/bin/slock")))
          (,(kbd "s-h") . windmove-left)  ;; Move to window to the left of current one. Uses universal arg
          (,(kbd "s-j") . windmove-down)  ;; Move to window below current one. Uses universal arg
          (,(kbd "s-k") . windmove-up)    ;; Move to window above current one. Uses universal arg
          (,(kbd "s-l") . windmove-right) ;; Move to window to the right of current one. Uses universal arg
          (,(kbd "s-f") . helm-find-files)
          (,(kbd "s-<tab>") . windower-switch-to-last-buffer) ;; Switch to last open buffer in current window
          (,(kbd "s-o") . windower-toggle-single) ;; Toggle between multiple windows, and a single window
          (,(kbd "s-O") . windower-toggle-split)  ;; Toggle between vertical and horizontal split. Only works with exactly two windows.
          (,(kbd "s-H") . windower-swap-left)  ;; Swap current window with the window to the left
          (,(kbd "s-J") . windower-swap-below) ;; Swap current window with the window below
          (,(kbd "s-K") . windower-swap-above) ;; Swap current window with the window above
          (,(kbd "s-L") . windower-swap-right) ;; Swap current window with the window to the right
          (,(kbd "s-F") . exwm-floating-toggle-floating) ;; Toggle the current window between floating and non-floating states
          (,(kbd "s-Q") . exwm-layout-toggle-fullscreen) ;; Toggle fullscreen mode, when in an EXWM window.
          (,(kbd "s-D") . kill-this-buffer)
          (,(kbd "s-x") . helm-M-x)
          (,(kbd "s-<backspace>") . kill-this-buffer)
          ))

  (exwm-input-set-key (kbd "s-SPC") 'counsel-linux-app)
  ;; Rename buffer to window title.
  (defun ambrevar/exwm-rename-buffer-to-title () (exwm-workspace-rename-buffer exwm-title))

  (add-hook 'exwm-update-title-hook 'ambrevar/exwm-rename-buffer-to-title)
  ;; Ensure that EXWM input mode is displayed in mode line
  (add-hook 'exwm-input--input-mode-change-hook
            'force-mode-line-update)
  ;; Allow resizing of non-floating windows, with mouse.
  (setq window-divider-default-bottom-width 8
        window-divider-default-right-width 8)
  (window-divider-mode)
  ;; Allow switching to EXWM buffers not belonging to current workspace.
  ;; This behaviour takes some getting used to, I guess thats why its not default
  (setq exwm-layout-show-all-buffers t))
  ;;:config



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
  ;;  (setq efs/polybar-process (start-process-shell-command "polybar" nil "polybar -qpanel /home/becker/.config/polybar/launch-exwm.sh")))
  (setq efs/polybar-process (start-process-shell-command "polybar" nil "polybar -q panel -c /home/becker/.config/polybar/config-exwm.ini")))

(defun efs/send-polybar-hook (module-name hook-index)
  (start-process-shell-command "polybar-msg" nil (format "polybar-msg hook %s %s" module-name hook-index)))

(defun efs/send-polybar-exwm-workspace ()
  (efs/send-polybar-hook "exwm-workspace" 1)
  (efs/send-polybar-hook "exwm-workspace-2" 1)
  )

(defun efs/polybar-exwm-workspace ()
  (pcase exwm-workspace-current-index
    (0 "")
    (1 "")
    (2 "")
    (3 "")
    (4 "")))

;; Update panel indicator when workspace changes
(add-hook 'exwm-workspace-switch-hook #'efs/send-polybar-exwm-workspace)

(use-package helm-exwm
  :ensure t
  :config
  (setq helm-exwm-emacs-buffers-source (helm-exwm-build-emacs-buffers-source))
  (setq helm-exwm-source (helm-exwm-build-source))
  (setq helm-mini-default-sources `(helm-exwm-emacs-buffers-source
                                    helm-exwm-source
                                    helm-source-recentf)))

(load-file "~/.emacs.d/init.el")
(load-file "~/Dropbox/.emacs.d/exwm-telephone-line-mode-line.el")

;; (use-package telephone-line
;;  :ensure t)

;;  (defun ambrevar/bottom-right-window-p ()
;;   "Determines whether the last (i.e. bottom-right) window of the
;;   active frame is showing the buffer in which this function is
;;   executed."
;;   (let* ((frame (selected-frame))
;;          (right-windows (window-at-side-list frame 'right))
;;          (bottom-windows (window-at-side-list frame 'bottom))
;;          (last-window (car (seq-intersection right-windows bottom-windows))))
;;     (eq (current-buffer) (window-buffer last-window))))

;; (defun jw/telephone-misc-if-exwm-or-last-window ()
;;   "Renders the mode-line-misc-info string for display in the
;;   mode-line if the currently active window is the last one in the
;;   frame, or an exwm window.

;;   The idea is to not display information like the current time,
;;   load, battery levels on all buffers.
;;   And to display input mode only in exwm windows."

;;   (when (or (ambrevar/bottom-right-window-p)
;;             exwm-window-type)
;;     (telephone-line-raw mode-line-misc-info t))
;; )

;; (defun jw/input-mode-str ()
;;   "Return string representing input mode, if window is of type EXWM"
;;   (if exwm-window-type
;;       (if (eq exwm--input-mode 'line-mode)
;;         (format "l")
;;         (format "c"))
;;     (format "")))

;; (defun jw/workspace-index ()
;;   "Return string representing current EXWM workspace index"
;;   (if (ambrevar/bottom-right-window-p)
;;     (format "[%s]" (exwm-workspace--position (selected-frame)))
;;     (format "")))

;; (defun jw/format-workspace-index-and-input-mode ()
;;   "Return string [workspace_index]input-mode depending on exwm-window or bottom-right window"
;;   (format "%s%s" (jw/workspace-index) (jw/input-mode-str))
;; )

;; (defun ambrevar/telephone-line-setup ()
;;   (telephone-line-defsegment telephone-line-last-window-segment ()
;;     (jw/telephone-misc-if-exwm-or-last-window))

;;   ;; Display the current EXWM workspace index in the mode-line
;;   (telephone-line-defsegment telephone-line-exwm-workspace-index ()
;;     (jw/format-workspace-index-and-input-mode))

;;   ;; Define a highlight font for ~ important ~ information in the last
;;   ;; window.
;;   (defface special-highlight '((t (:foreground "white" :background "#5f627f"))) "")
;;   (add-to-list 'telephone-line-faces
;;                '(highlight . (special-highlight . special-highlight)))

;;   (setq telephone-line-lhs
;;         '((nil . (telephone-line-position-segment))
;;           (accent . (telephone-line-buffer-segment))))

;;   (setq telephone-line-rhs
;;         '((accent . (telephone-line-major-mode-segment))
;;           (nil . (telephone-line-last-window-segment
;;                   telephone-line-exwm-workspace-index))
;;           ))

;;   (setq telephone-line-primary-left-separator 'telephone-line-tan-left
;;         telephone-line-primary-right-separator 'telephone-line-tan-right
;;         telephone-line-secondary-left-separator 'telephone-line-tan-hollow-left
;;         telephone-line-secondary-right-separator 'telephone-line-tan-hollow-right)

;;   (telephone-line-mode 1))

;; ;;(ambrevar/telephone-line-setup)

(exwm-enable)
