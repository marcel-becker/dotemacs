;;http://pragmaticemacs.com/emacs/get-that-spacemacs-look-without-spacemacs/
(use-package spacemacs-theme
  :defer t
  :init
  (load-theme 'spacemacs-dark t)
  (setq spacemacs-theme-org-agenda-height nil)
  (setq spacemacs-theme-org-height nil))


 ;; set sizes here to stop spacemacs theme resizing these
(set-face-attribute 'org-level-1 nil :height 1.0)
(set-face-attribute 'org-level-2 nil :height 1.0)
(set-face-attribute 'org-level-3 nil :height 1.0)
(set-face-attribute 'org-scheduled-today nil :height 1.0)
(set-face-attribute 'org-agenda-date-today nil :height 1.1)
(set-face-attribute 'org-table nil :foreground "#008787")


(use-package spaceline
  :demand t
  :ensure t
  :config
  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main)))))

(use-package spaceline-config
  :demand t
  :ensure spaceline
  :init
  (setq powerline-default-separator 'arrow)
  (setq powerline-height 30)
  (setq spaceline-separator-dir-left '(left . left))
  (setq spaceline-separator-dir-right '(right . right))
  :config
  (spaceline-helm-mode 1)
  (setq winum-auto-setup-mode-line nil)
  (setq spaceline-buffer-encoding-abbrev-p nil
        spaceline-window-numbers-unicode t
        spaceline-line-column-p t
        spaceline-buffer-id-p t
        spaceline-remote-host-p t
        spaceline-buffer-modified-p t
        spaceline-buffer-size-p nil
        spaceline-minor-modes-separator " ")

  (spaceline-spacemacs-theme))
