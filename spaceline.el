;; 2017-06-16 Fri 18:28 marcelbecker
;; https://github.com/mclear-tools/dotemacs/blob/master/config.org
(use-package spaceline
  :ensure t
  :init
  (progn
    ;; size of modeline
    ;; (setq powerline-height 18)
    (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
    ;; (setq-default powerline-default-separator nil)
    ;; (setq-default powerline-default-separator 'utf-8)
    ;; (setq powerline-utf-8-separator-left #xe0b0)
    ;; (setq powerline-utf-8-separator-right #xe0b2)
    ;; (set-face-attribute 'mode-line nil :font "Source Code Pro-16")

    ;; small triangles
    ;; (setq powerline-utf-8-separator-left #x25ba)
    ;; (setq powerline-utf-8-separator-right #x25c4)
    ;; (setq powerline-text-scale-factor 1.1)
    ;; half circles
    ;; (setq powerline-utf-8-separator-left 9687
    ;;       powerline-utf-8-separator-right 9686)


    (defun powerline-buffer-id (&optional face pad)
      (powerline-raw
       (format-mode-line
        (concat " " (propertize
                     ;;              (format-mode-line mode-line-buffer-identification)
                     (if buffer-file-name
                         (concat (shorten-directory default-directory 30)
                                 (file-name-nondirectory buffer-file-name)
                                 )
                       (buffer-name))
                     'face face
                     'mouse-face 'mode-line-highlight
                     'help-echo "Buffer name\n\ mouse-1: Previous buffer\n\ mouse-3: Next buffer"
                     'local-map (let ((map (make-sparse-keymap)))
                                  (define-key map [mode-line mouse-1] 'mode-line-previous-buffer)
                                  (define-key map [mode-line mouse-3] 'mode-line-next-buffer)
                                  map))))
       face pad))

    ;; slant (requires srbg support)
    (setq-default powerline-default-separator 'arrow
                   powerline-height 30)
    (setq spaceline-separator-dir-left '(left . left))
    (setq spaceline-separator-dir-right '(right . right))
    (setq winum-auto-setup-mode-line nil)
    (setq powerline-image-apple-rgb t)


    ;; fancy git icon for the modeline
    (defadvice vc-mode-line (after strip-backend () activate)
    (when (stringp vc-mode)
      (let ((gitlogo (replace-regexp-in-string "^ Git." ":" vc-mode)))
            (setq vc-mode gitlogo)))))
  :config
  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main))))
  (spaceline-helm-mode 1)
  (spaceline-spacemacs-theme)
  (setq spaceline-buffer-encoding-abbrev-p nil
        spaceline-window-numbers-unicode t
        spaceline-line-column-p nil
        spaceline-buffer-id-p t
        spaceline-buffer-modified-p t
        spaceline-buffer-size-p nil
        spaceline-minor-modes-separator "|"))

;;Spaceline All The Icons
;;Pretty icons for spaceline using all-the-icons.el. This is a nice package but still a bit buggy. Also adds a couple seconds to the load time.

;; Mode line
(use-package spaceline
  :config
  (require 'spaceline-config))


(use-package spaceline-config :ensure spaceline
  :config
  (spaceline-helm-mode 1)
  (spaceline-spacemacs-theme)
  )


(use-package spaceline-all-the-icons
  :after spaceline
  :if (display-graphic-p)
  :config (spaceline-all-the-icons-theme)
  ;; Configuration
  ;; (spaceline-all-the-icons--setup-package-updates) ;; Enable package update indicator
  (spaceline-all-the-icons--setup-git-ahead)       ;; Enable # of commits ahead of upstream in git
  (spaceline-all-the-icons--setup-paradox)         ;; Enable Paradox mode line
  (spaceline-all-the-icons--setup-neotree)         ;; Enable Neotree mode line
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state
        spaceline-all-the-icons-separator-type 'arrow
        spaceline-all-the-icons-icon-set-modified 'circle
        spaceline-all-the-icons-icon-set-git-stats 'diff-icons
        spaceline-all-the-icons-icon-set-window-numbering 'solid
        spaceline-all-the-icons-primary-separator "")
   ;; Toggles
   (spaceline-toggle-all-the-icons-battery-status-off)
   (spaceline-toggle-all-the-icons-buffer-size-off)
   (spaceline-toggle-all-the-icons-buffer-position-on)
   (spaceline-toggle-all-the-icons-buffer-path-on)
   (spaceline-toggle-all-the-icons-hud-on)
   (spaceline-toggle-all-the-icons-narrowed-on)
   (spaceline-toggle-all-the-icons-flycheck-status-off)
   (spaceline-toggle-all-the-icons-flycheck-status-info-off)
   (spaceline-toggle-all-the-icons-vc-icon-off)
   (spaceline-toggle-all-the-icons-vc-status-off)
   (spaceline-toggle-all-the-icons-git-status-on)
   (spaceline-toggle-all-the-icons-git-ahead-on)
   (spaceline-toggle-all-the-icons-position-on)
   (spaceline-toggle-all-the-icons-minor-modes-off)

   ;; (spaceline-toggle-all-the-icons-weather-on)
   ;; (spaceline-toggle-all-the-icons-time-off)
)


;; Fancy Battery
(use-package fancy-battery
  :init
  (fancy-battery-mode)
  :config
  (setq-default battery-update-interval 10)
  (set-face-attribute 'fancy-battery-charging nil
  :foreground "dark blue" :weight 'bold)
  (set-face-attribute 'fancy-battery-discharging nil
  :foreground "dark magenta" :weight 'bold)
  (set-face-attribute 'fancy-battery-critical nil
  :foreground "dark red" :weight 'bold))

;; Display Time
(setq display-time-format " %a %b %d | %H:%M |")
(display-time-mode)

;;Hide mode line
;; Hide mode line. From http://bzg.fr/emacs-hide-mode-line.html
(defvar-local hidden-mode-line-mode nil)
(defvar-local hide-mode-line nil)
(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global t
  :variable hidden-mode-line-mode
  :group 'editing-basics
  (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
  (force-mode-line-update)
  ;; Apparently force-mode-line-update is not always enough to
  ;; redisplay the mode-line
  (redraw-display)
  (when (and (called-interactively-p 'interactive)
             hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "Hidden Mode Line Mode enabled.  "
             "Use M-x hidden-mode-line-mode to make the mode-line appear."))))
