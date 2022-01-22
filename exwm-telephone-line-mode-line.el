(use-package telephone-line)

(use-package unicode-fonts
  :config
  (unicode-fonts-setup))


(setq telephone-line-primary-left-separator 'telephone-line-utf-abs-left
      telephone-line-secondary-left-separator 'telephone-line-utf-abs-hollow-left
      telephone-line-primary-right-separator 'telephone-line-utf-abs-right
      telephone-line-secondary-right-separator 'telephone-line-utf-abs-hollow-right
      )


(defvar my-font-height-modifier 10
  "Default value to increment the size by when jacking into a monitor.")
(defun my-increase-modeline-font ()
  (interactive)
  (set-face-attribute 'mode-line nil :height (+ (face-attribute 'mode-line :height)
                                                my-font-height-modifier)))

(defun my-decrease-modeline-font ()
  (interactive)
  (set-face-attribute 'mode-line nil :height (- (face-attribute 'mode-line :height)
                                                my-font-height-modifier)))



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
                    :height (* (window-font-height) 6)
                    :box '(:line-width 6 :color "orange" :style nil)
                    )

(set-face-attribute 'mode-line-inactive nil
                    ;;:foreground "Orange"
                    :background "gray22"
                    :inverse-video nil
                    :weight 'bold
                    :height (* (window-font-height) 6)
                    :box '(:line-width 6 :color "white" :style nil)
                    )



(set-face-attribute 'telephone-line-accent-active nil
                    :foreground "Black" ;;Yellow"
                    :background  "Orange"
                    :inverse-video nil
                    :weight 'bold
                    :height (* (window-font-height) 6)
                    :box '(:line-width 6 :color "orange" :style nil))

(set-face-attribute 'telephone-line-accent-inactive nil
                    :foreground "White"
                    :background "Black"
                    :inverse-video nil
                    :weight 'bold
                    :height (* (window-font-height) 6)
                    ;;:box '(:line-width 6 :color "orange" :style nil)
                    :box '(:line-width 6 :color "white" :style nil)
                    )

(set-face-attribute 'telephone-line-evil nil
                    :foreground "Orange" ;;Yellow"
                    :background "Blue2"
                    :inverse-video nil
                    :weight 'bold
                    :height (* (window-font-height) 6)
                    :box '(:line-width 6 :color "orange" :style nil))




(set-face-attribute 'telephone-line-evil-visual nil :background "light sea green")




(make-face 'mode-line-mode-face)
(make-face 'mode-line-folder-face)
(make-face 'mode-line-filename-face)
(make-face 'mode-line-index-face)



(set-face-attribute 'mode-line-mode-face nil
                    :inherit 'mode-line
                    :foreground "gray80")


(set-face-attribute 'mode-line-folder-face nil
                    :inherit 'mode-line
                    :foreground "white" ;"gray22"
                    :weight 'bold)

(set-face-attribute 'mode-line-filename-face nil
                    :inherit 'mode-line
                    :foreground "#eab700"
                    :weight 'ultra-bold)



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


(defun get-exwm-workspace-number ()
  (let* ((num (cond
               ((bound-and-true-p exwm-workspace-current-index)
                exwm-workspace-current-index)
               (t nil)))
         (str (when num (int-to-string num))))
    (when num
      (if spaceline-window-numbers-unicode
          (spaceline--unicode-number str)
        (propertize str 'face 'bold)))))




;; (defface my-winnum-face
;;   '((t ((:inherit mode-line :height (* (window-font-height) 20)))))
;;   "Face for the window number in the mode line"
;;   :group 'telephone-line
;;   )
;; (set-face-attribute 'my-winnum-face nil
;;                     :height 1000);; (* (window-font-height) 60))

(set-face-attribute 'winum-face nil :height (* 6 (window-font-height)) :weight 'extra-bold)


(telephone-line-defsegment my-telephone-line-window-number-segment (&optional in-unicode)
  (when (bound-and-true-p winum-mode)
    (propertize (get-window-number) 'face 'winum-face)))


(defun jw/input-mode-str ()
  "Return string representing input mode, if window is of type EXWM"
  (if exwm-window-type
      (if (eq exwm--input-mode 'line-mode)
          (format " Ⓛ ")
        (format " Ⓒ "))
    (format "")))

;; Display the current EXWM workspace index in the mode-line
(telephone-line-defsegment telephone-line-exwm-workspace-index ()
  (when (bound-and-true-p exwm-workspace-current-index)
    (propertize (format "wksp:%s %s" (get-exwm-workspace-number) (jw/input-mode-str)) 'face 'winum-face))
  ;;(format "Wksp:%s" (get-exwm-workspace-number)))
  )


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


;;(setq telephone-line-height 20) ;; 30
(setq telephone-line-lhs
      '((evil   . (telephone-line-evil-tag-segment))
        (nil    . (my-telephone-line-window-number-segment))
        (accent . nil)
        (nil    . (telephone-line-exwm-workspace-index))
        (accent . (telephone-line-input-info-segment))
        (nil    . (my-telephone-line-buffer-segment))
        (accent . (;;telephone-line-vc-segment
                   ;;telephone-line-erc-modified-channels-segment
                   ;;telephone-line-process-segment
                   ;;telephone-line-misc-info-segment
                   ))
        (nil    . nil)
        (nil    . (telephone-line-major-mode-segment
                   ;;telephone-line-buffer-segment
                   ))
        (nil    . nil)
        (accent . nil)
        (nil    . nil)
        (accent . nil)
        (nil . nil)
        (accent . nil)
        ))



(setq telephone-line-center-lhs
      '(
        (accent . (telephone-line-misc-info-segment))

        (nil . nil)
        (accent . nil)
        (nil . nil)
        (accent . nil)
                                        ;(nil . nil)
        (nil . (telephone-line-vc-segment))
        (accent . nil)
        (nil . nil)
        (accent . nil)
        (nil . nil)
        (accent . (telephone-line-airline-position-segment))
        (nil . nil)
        (accent . nil)
        (nil . nil)
        (accent . nil)
        (nil . nil)
        ;;(accent . nil)
        ;;(nil . nil)
        ))

(setq telephone-line-rhs
      '(
        ;;(nil . nil)
        ;;(accent . nil)
        ;;(nil . nil)
        ;;(accent    . nil );;(telephone-line-misc-info-segment))
        ;;(nil   . nil) ;(telephone-line-airline-position-segment))
        (nil . (telephone-line-minor-mode-segment))
        (evil  . (telephone-line-evil-tag-segment))
        ))


(telephone-line-mode 1)

(setq mode-line-format
      (if telephone-line-mode
          `("%e" ,@(telephone-line--generate-mode-line))
        telephone-line--default-mode-line))
