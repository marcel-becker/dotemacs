(use-package god-mode :ensure t)

(global-set-key (kbd "<home>") 'god-local-mode)


(setq original-active-background (face-attribute 'mode-line :background))
(setq original-active-foreground (face-attribute 'mode-line :foreground))

(setq original-inactive-background (face-attribute 'mode-line-inactive :background))
(setq original-inactive-foreground (face-attribute 'mode-line-inactive :foreground))



(defun c/god-mode-update-cursor ()
  (let ((limited-colors-p (> 257 (length (defined-colors)))))
    (cond (god-local-mode
           (progn
             (set-face-background 'mode-line  "red" )
             (set-face-foreground 'mode-line  "white" )
             ;;(set-face-background 'mode-line-inactive "red")
             ))
          (t
           (progn
             (set-face-background 'mode-line original-active-background)
             (set-face-foreground 'mode-line original-active-foreground)
             ;;(set-face-background 'mode-line-inactive (if limited-colors-p "black" "#0a2832"))
             )))))

(add-hook 'god-mode-enabled-hook 'c/god-mode-update-cursor)
(add-hook 'god-mode-disabled-hook 'c/god-mode-update-cursor)

(define-key god-local-mode-map (kbd ".") 'repeat)
(define-key god-local-mode-map (kbd "i") 'god-local-mode)
(define-key god-local-mode-map (kbd "") 'god-local-mode)
(global-set-key (kbd "C-x C-1") 'delete-other-windows)
(global-set-key (kbd "C-x C-2") 'split-window-below)
(global-set-key (kbd "C-x C-3") 'split-window-right)
(global-set-key (kbd "C-x C-0") 'delete-window)
