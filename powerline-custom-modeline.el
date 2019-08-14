;; Base faces, face1 is active, face2 is inactive
(defface my-pl-face1 '((t (:foreground "white" :background "blue1" :inherit mode-line))) nil :group 'powerline)
(defface my-pl-face2 '((t (:foreground "dimgray" :background "#000040" ))) nil :group 'powerline)
(defface my-pl-face3 '((t (:foreground "black" :background "dark orange" :inherit mode-line))) nil :group 'powerline)
(defface my-pl-face4 '((t (:foreground "dimgray" :background "#402000"))) nil :group 'powerline)

(defface my-pl-indi-romod-face1 '((t (:inherit my-pl-face1))) nil :group 'powerline)
(defface my-pl-indi-romod-face2 '((t (:inherit my-pl-face2))) nil :group 'powerline)
(defface my-pl-indi-narrow-face1 '((t (:inherit my-pl-face1))) nil :group 'powerline)
(defface my-pl-indi-narrow-face2 '((t (:inherit my-pl-face2))) nil :group 'powerline)
(defface my-pl-size-face1 '((t (:inherit my-pl-face1))) nil :group 'powerline)
(defface my-pl-size-face2 '((t (:inherit my-pl-face2))) nil :group 'powerline)
(defface my-pl-mule-face1 '((t (:inherit my-pl-face1))) nil :group 'powerline)
(defface my-pl-mule-face2 '((t (:inherit my-pl-face2))) nil :group 'powerline)
(defface my-pl-name-face1 '((t (:inherit my-pl-face3 :foreground "white" :background "darkred"))) nil :group 'powerline)
(defface my-pl-name-face2 '((t (:inherit my-pl-face4 :background "#400000"))) nil :group 'powerline)
(defface my-pl-dir-face1 '((t (:inherit mode-line :foreground "dimgray"))) nil :group 'powerline)
(defface my-pl-dir-face2 '((t (:inherit mode-line-inactive :foreground "dimgray"))) nil :group 'powerline)
(defface my-pl-major-face1 '((t (:inherit my-pl-face1))) nil :group 'powerline)
(defface my-pl-major-face2 '((t (:inherit my-pl-face2))) nil :group 'powerline)
(defface my-pl-process-face1 '((t (:inherit my-pl-face1))) nil :group 'powerline)
(defface my-pl-process-face2 '((t (:inherit my-pl-face2))) nil :group 'powerline)
(defface my-pl-minor-face1 '((t (:inherit my-pl-face3))) nil :group 'powerline)
(defface my-pl-minor-face2 '((t (:inherit my-pl-face4))) nil :group 'powerline)
(defface my-pl-lincol-face1 '((t (:inherit my-pl-face1))) nil :group 'powerline)
(defface my-pl-lincol-face2 '((t (:inherit my-pl-face2))) nil :group 'powerline)
(when (display-graphic-p)
  (set-face-attribute 'mode-line-highlight nil :foreground "white" :weight 'bold)
  (set-face-attribute 'my-pl-indi-romod-face1 nil :weight 'bold)
  (set-face-attribute 'my-pl-name-face1 nil :weight 'bold))


;;*** Powerline functions
;;Here I define [[http://amitp.blogspot.com/2011/08/emacs-custom-mode-line.html][Amit's]] shorten-directory function, so ;;that the directory
;;in the mode-line isn't too long.


(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))


;;*** finally the mode line
;;And finally we build our mode line:


(use-package powerline
  :if (and (display-graphic-p) (not noninteractive))
  :ensure t
  :config
  (powerline-default-theme)
  (setq powerline-default-separator 'arrow)
  (setq powerline-height 18)
  (setq powerline-display-buffer-size nil)
  (setq powerline-display-mule-info nil)
  (setq powerline-display-hud nil)

  ;; used for modes + line numbers
  (set-face-attribute 'powerline-active1   nil :foreground "black" :background "dark orange")
  (set-face-attribute 'powerline-inactive1 nil :foreground "black" :background "DarkOrange4")
  ;; used for left/right border, indicator + empty space
  (set-face-attribute 'powerline-active2   nil :background "blue1")
  (set-face-attribute 'powerline-inactive2 nil :background "blue4")

  ;; This detects the current state of narrowing. It is a slight
  ;; modification of the original function powerline-narrow from
  ;; powerline.el: It displays a unicode flag as well.
  ;; (defpowerline my-powerline-narrow
  (defun my-powerline-narrow (&optional face pad)
    (let (real-point-min real-point-max)
      (save-excursion
        (save-restriction
          (widen)
          (setq real-point-min (point-min)
                real-point-max (point-max))))
      (when (or (/= real-point-min (point-min))
                (/= real-point-max (point-max)))
        (propertize (char-to-string #x2691)
                    'mouse-face 'mode-line-highlight
                    'help-echo "mouse-1: Remove narrowing from the current buffer"
                    'local-map (make-mode-line-mouse-map
                                'mouse-1 'mode-line-widen)))))

  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (indi-romod-face (if active 'my-pl-indi-romod-face1 'my-pl-indi-romod-face2))
                          (indi-narrow-face (if active 'my-pl-indi-narrow-face1 'my-pl-indi-narrow-face2))
                          (size-face (if active 'my-pl-size-face1 'my-pl-size-face2))
                          (mule-face (if active 'my-pl-mule-face1 'my-pl-mule-face2))
                          (name-face (if active 'my-pl-name-face1 'my-pl-name-face2))
                          (dir-face (if active 'my-pl-dir-face1 'my-pl-dir-face2))
                          (major-face (if active 'my-pl-major-face1 'my-pl-major-face2))
                          (process-face (if active 'my-pl-process-face1 'my-pl-process-face2))
                          (minor-face (if active 'my-pl-minor-face1 'my-pl-minor-face2))
                          (lincol-face (if active 'my-pl-lincol-face1 'my-pl-lincol-face2))

                          (separator-left (intern (format "powerline-%s-%s"
                                                          (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           (powerline-current-separator)
                                                           (cdr powerline-default-separator-dir))))
                          (erc-active (and (boundp 'erc-modified-channels-object)
                                           (> (length erc-modified-channels-object) 0)))

                          (lhs (append (list ;; modified/readonly status
                                        (powerline-raw "%*" indi-romod-face 'l)
                                        (my-powerline-narrow indi-narrow-face 'l)
                                        (powerline-raw " " indi-narrow-face)

                                        ;; buffer size
                                        (when powerline-display-buffer-size
                                          (powerline-buffer-size size-face 'l))

                                        ;; Multilingual environment
                                        (when powerline-display-mule-info
                                          (powerline-raw mode-line-mule-info mule-face 'l))

                                        ;; Directory, Buffer
                                        (funcall separator-right mule-face name-face)
                                        ;; (powerline-buffer-id name-face 'l) ;; this gives wrong foreground
                                        (powerline-raw "%b " name-face 'l)
                                        (funcall separator-left name-face dir-face)

                                        (when (and (buffer-file-name)
                                                   (not (file-remote-p default-directory)))
                                          (powerline-raw (shorten-directory default-directory 25)
                                                         dir-face 'l))

                                        (powerline-raw " " dir-face)
                                        (funcall separator-right dir-face major-face)

                                        ;; Major mode
                                        (powerline-major-mode major-face 'l)

                                        ;; Empty space
                                        (powerline-raw " " major-face)

                                        ;; Some process status
                                        (powerline-process process-face))

                                       ;; Minor mode
                                       (append (if (split-string (format-mode-line minor-mode-alist))
                                                   (list (powerline-minor-modes minor-face 'l)
                                                         (funcall separator-left minor-face mode-line))
                                                 (list (funcall separator-left major-face mode-line))))
                                       ))

                          (rhs (append (list
                                        ;; ERC channel tracking status
                                        (when erc-active
                                          (funcall separator-right mode-line name-face))
                                        (when erc-active
                                          (powerline-raw erc-modified-channels-object name-face))
                                        (funcall separator-right (if erc-active name-face mode-line) lincol-face)

                                        ;; line number, column number
                                        (powerline-raw "%l:%c " lincol-face 'l)

                                        ;; XPM containing the position
                                        (when powerline-display-hud
                                          (powerline-hud my-pl-face1 my-pl-face3))
                                        )))
                          )
                     (concat (powerline-render lhs)
                             (powerline-fill mode-line (powerline-width rhs))
                             (powerline-render rhs))))))
  )
