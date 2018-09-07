(use-package telephone-line)

(use-package unicode-fonts
  :config
  (unicode-fonts-setup)
)

;; specify font for all unicode characters
(when (member "DejaVu Sans Mono for Powerline" (font-family-list))
  (set-fontset-font t 'unicode "DejaVu Sans Mono for Powerline" nil 'prepend))


;; Format for time string
(setq display-time-string-forms
      '((propertize (concat "TIME: " 24-hours ":" minutes " " am-pm))))



(set-face-attribute 'mode-line nil
                    :foreground "Orange" ;;Yellow"
                    :background "Blue2"
                    :inverse-video nil
                    :weight 'bold
                    :box '(:line-width 6 :color "orange" :style nil))

;; (set-face-attribute 'mode-line-inactive nil
;;                     ;;:foreground "Orange" ;;Yellow"
;;                     :background "gray22"
;;                     :inverse-video nil
;;                     ;;:weight 'bold
;;                     ;;:box '(:line-width 6 :color "orange" :style nil)
;;                     )



(set-face-attribute 'telephone-line-accent-active nil
                    :foreground "Black" ;;Yellow"
                    :background "Orange"
                    :inverse-video nil
                    :weight 'bold
                    :box '(:line-width 6 :color "orange" :style nil))

(make-face 'mode-line-mode-face)
(make-face 'mode-line-folder-face)
(make-face 'mode-line-filename-face)

(set-face-attribute 'mode-line-mode-face nil
                    :inherit 'mode-line
                    :foreground "gray80")


(set-face-attribute 'mode-line-folder-face nil
                    :inherit 'mode-line
                    :foreground "white" ;"gray22"
                    :weight 'bold)

(set-face-attribute 'mode-line-filename-face nil
                    :inherit 'mode-line-mode-face
                    :foreground "#eab700"
                    :weight 'extra-bold)



;; Helper function
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




(setq spaceline-window-numbers-unicode t)

;; from https://github.com/TheBB/spaceline/blob/master/spaceline-segments.el
(defun spaceline--unicode-number (str)
  "Return a nice unicode representation of a single-digit number STR."
  (cond
   ((string= "1" str) "➊")
   ((string= "2" str) "➋")
   ((string= "3" str) "➌")
   ((string= "4" str) "➍")
   ((string= "5" str) "➎")
   ((string= "6" str) "➏")
   ((string= "7" str) "➐")
   ((string= "8" str) "➑")
   ((string= "9" str) "➒")
   ((string= "10" str) "➓")
   (t str)))



(defun get-window-number ()
  "The current window number.
Requires either `winum-mode' or `window-numbering-mode' to be enabled."
  (let* ((num (cond
               ((bound-and-true-p winum-mode)
                (winum-get-number))
               ((bound-and-true-p window-numbering-mode)
                (window-numbering-get-number))
               (t nil)))
         (str (when num (int-to-string num))))
    (when num
      (if spaceline-window-numbers-unicode
          (spaceline--unicode-number str)
        (propertize str 'face 'bold)))))



(telephone-line-defsegment my-telephone-line-window-number-segment (&optional in-unicode)
  (when (bound-and-true-p winum-mode)
        (propertize (get-window-number) 'face `winum-face)))




(telephone-line-defsegment* my-telephone-line-buffer-segment ()
  `(" "
    ;;mode-line-mule-info
    ;;mode-line-modified
    ;;mode-line-client
    ;;mode-line-remote
    mode-line-frame-identification
    ,(if buffer-file-name (telephone-line-raw (shorten-directory default-directory 30) t))
    ,(telephone-line-raw  "%b"  t)
    " "
    ;;,(telephone-line-raw mode-line-buffer-identification t)
    ))


(setq telephone-line-height 30)
(setq telephone-line-lhs
      '((evil   . (telephone-line-evil-tag-segment))
        (nil    . (my-telephone-line-window-number-segment))
        (accent . (telephone-line-input-info-segment))
        (nil    . (my-telephone-line-buffer-segment))
        (accent . (;;telephone-line-vc-segment
                   ;;telephone-line-erc-modified-channels-segment
                   ;;telephone-line-process-segment
                   ;;telephone-line-misc-info-segment
                   ))
        (nil    . nil)
        (accent    . (telephone-line-minor-mode-segment
                   ;;telephone-line-buffer-segment
                   ))
        (accent . nil)
        (nil    . nil)
        ))



(setq telephone-line-center-lhs
      '((nil . (telephone-line-vc-segment))))

(setq telephone-line-rhs
      '(
        (nil . nil)
        (accent . nil)
        (nil . (telephone-line-major-mode-segment))
        (accent    . (telephone-line-misc-info-segment))
        (evil   . (telephone-line-airline-position-segment))
        ))


(telephone-line-mode 1)

(setq mode-line-format
      (if telephone-line-mode
          `("%e" ,@(telephone-line--generate-mode-line))
        telephone-line--default-mode-line))
