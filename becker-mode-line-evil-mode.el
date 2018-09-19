;; Mode line setup

(if running-macos
    (setq ns-use-srgb-colorspace nil)
  (setq powerline-image-apple-rgb t)
  )

(defvar modeline-selected-window nil)
(add-function :before pre-redisplay-function
              (lambda (_wins) (setq modeline-selected-window (selected-window))))
;;(defun modeline-record-selected-window ()
;; (setq modeline-selected-window (selected-window)))

(defun modeline-selected-window-active ()
  "Return whether the current window is active."
  (eq modeline-selected-window (selected-window)))



;; Format for time string
(setq display-time-string-forms
      '((propertize (concat "TIME: " 24-hours ":" minutes " " am-pm))))


(defface my-powerline-active1 '((t (:background "blue2" :foreground "white" :inherit mode-line)))
  "Powerline face 1."
  :group 'powerline)

(defface my-powerline-active2 '((t (:background "Orange" :foreground "white" :inherit mode-line)))
  "Powerline face 2."
  :group 'powerline)


(setq face1  'my-powerline-active1
      face2  'my-powerline-active2
      separator-right (powerline-arrow-right face2 face1)
      separator-left (powerline-arrow-left face1 face2))


(setq
 marcel-mode-line-format
 '(
   ;;(:propertize  " " 'face 'mode-line-face);; 'display '((space :width 1 :align-to left-fringe)))

   (:eval  (propertize " " 'display separator-right))
   " "
   (:eval (get-window-number))
   " "
  ;; (:eval  (propertize " " 'display separator-right))
   (:eval  (propertize " " 'display separator-left))

   (:eval (my/evil-mode-state))
   ;; read-only or modified status
   (:eval
    (cond (buffer-read-only
           (propertize " RO " 'face 'mode-line-read-only-face))
          ((buffer-modified-p)
           (propertize " ** " 'face 'mode-line-modified-face))
          (t "      ")))


   ;; emacsclient [default -- keep?]
   mode-line-client
   ;;  " "
   ;;  "   "
   ;; directory and buffer/file name
   (:propertize  (:eval (shorten-directory default-directory 30))
                 face mode-line-folder-face)
   (:propertize "%b"  face mode-line-filename-face)
   ;; narrow [default -- keep?]
   " %n "
   ;; mode indicators: vc, recursive edit, major mode, minor modes, process, global
   (vc-mode vc-mode)

   ;; Position, including warning for 80 columns
   (:propertize "Line: %4l" face mode-line-position-face)
   (:eval
    (propertize " Col: %3c" 'face
                (if (>= (current-column) 80)
                    'mode-line-80col-face
                  'mode-line-position-face)))
   "  %["
   (:propertize mode-name face mode-line-mode-face)
   "%] "
   (:eval (propertize (format-mode-line minor-mode-alist)
                      'face 'mode-line-minor-mode-face))
   (:propertize mode-line-process face mode-line-process-face)
   "    "

   (:eval (propertize " " 'display '((space :align-to (- right-fringe 10)))))
   (global-mode-string global-mode-string)
   ;;   "    "
   ;; nyan-mode uses nyan cat as an alternative to %p
   ;;(:eval (when nyan-mode (list (nyan-create))))
   ))


(setq-default mode-line-format marcel-mode-line-format)
(setq  mode-line-format marcel-mode-line-format)


;; (setq-default
;;  mode-line-format
;;  '(
;;    (:propertize  " " 'face 'mode-line-face);; 'display '((space :width 1 :align-to left-fringe)))
;;    (:eval (my/evil-mode-state))

;;    ;; read-only or modified status
;;    (:eval
;;     (cond (buffer-read-only
;;            (propertize " RO " 'face 'mode-line-read-only-face))
;;           ((buffer-modified-p)
;;            (propertize " ** " 'face 'mode-line-modified-face))
;;           (t "      ")))


;;    ;; emacsclient [default -- keep?]
;;    mode-line-client
;;    " "
;;    "   "
;;    ;; directory and buffer/file name
;;    (:propertize  (:eval (shorten-directory default-directory 30))
;;                  face mode-line-folder-face)
;;    (:propertize "%b"  face mode-line-filename-face)
;;    ;; narrow [default -- keep?]
;;    " %n "
;;    ;; mode indicators: vc, recursive edit, major mode, minor modes, process, global
;;    (vc-mode vc-mode)

;;    ;; Position, including warning for 80 columns
;;    (:propertize "Line: %4l" face mode-line-position-face)
;;    (:eval
;;     (propertize " Col: %3c" 'face
;;                 (if (>= (current-column) 80)
;;                     'mode-line-80col-face
;;                   'mode-line-position-face)))

;;    "  %["
;;    (:propertize mode-name face mode-line-mode-face)
;;    "%] "
;;    (:eval (propertize (format-mode-line minor-mode-alist)
;;                       'face 'mode-line-minor-mode-face))
;;    (:propertize mode-line-process face mode-line-process-face)
;;    "    "

;;   ;; (:eval (propertize " " 'display '((space :align-to (- right-fringe 10)))))
;;    (global-mode-string global-mode-string)
;;    ;;   "    "
;;    ;; nyan-mode uses nyan cat as an alternative to %p
;;    ;;(:eval (when nyan-mode (list (nyan-create))))
;;    ))





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


;; Extra mode line faces
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-folder-face)
(make-face 'mode-line-filename-face)
(make-face 'mode-line-position-face)
(make-face 'mode-line-mode-face)
(make-face 'mode-line-minor-mode-face)
(make-face 'mode-line-process-face)
(make-face 'mode-line-80col-face)

(set-face-attribute 'mode-line nil
                    :foreground "Orange" ;;Yellow"
                    :background "Blue2"
                    :inverse-video nil
                    :weight 'bold
                    :box '(:line-width 6 :color "orange" :style nil))

(set-face-attribute 'mode-line-mode-face nil
                    ;;:inherit 'mode-line-mode-face
                    :foreground "gray80")

(set-face-attribute 'mode-line-inactive nil
                    :foreground "white";"gray80"
                    :background "gray60"
                    :inverse-video nil
                    :box '(:line-width 6 :color "black" :style nil))

(set-face-attribute 'mode-line-read-only-face nil
                    :inherit 'mode-line-mode-face
                    :foreground "white"
                    :box '(:line-width 6 :color "orange")
                    :weight 'bold)


(set-face-attribute 'mode-line-modified-face nil
                    :inherit 'mode-line-mode-face
                    :foreground "orange"
                    ;;:background "#ffffff"
                    :weight 'extra-bold
                   :box '(:line-width 6 :color "orange"))

(set-face-attribute 'mode-line-folder-face nil
                    :inherit 'mode-line-mode-face
                    :foreground "white" ;"gray60"
                    :weight 'bold)

(set-face-attribute 'mode-line-filename-face nil
                    :inherit 'mode-line-mode-face
                    :foreground "#eab700"
                    :weight 'extra-bold)

(set-face-attribute 'mode-line-position-face nil
                    :inherit 'mode-line-mode-face
                    :family "DejaVu Sans Mono"
                    :height 120)



(set-face-attribute 'mode-line-minor-mode-face nil
                    :inherit 'mode-line-mode-face
                    :foreground "orange" ;;"gray40"
                    :family "DejaVu Sans Mono"
                    :height 140)

(set-face-attribute 'mode-line-process-face nil
                    :inherit 'mode-line-mode-face
                    :foreground "#718c00"
                    :height 140)


(set-face-attribute 'mode-line-80col-face nil
                    :inherit 'mode-line-position-face
                    :foreground "black"
                    :background "#eab700")


(make-face 'mode-line-evil-normal-face)
(make-face 'mode-line-evil-insert-face)
(make-face 'mode-line-evil-visual-face)
(make-face 'mode-line-evil-replace-face)
(make-face 'mode-line-evil-emacs-face)
(make-face 'mode-line-evil-motion-face)


(set-face-attribute 'mode-line-evil-normal-face nil
                    :inherit 'mode-line-face
                    ;;:inherit 'mode-line-mode-face
                    :foreground "black"
                    :background "green"
                    :weight 'bold
                    :box '(:line-width 6 :color "orange"))

(set-face-attribute 'mode-line-evil-insert-face nil
                    :inherit 'mode-line-mode-face
                    :foreground "white"
                    :background "blue2"
                    :weight 'bold)

(set-face-attribute 'mode-line-evil-emacs-face nil
                    :inherit 'mode-line-mode-face
                    :foreground "white"
                    :background "blue2"
                    :weight 'bold)

(set-face-attribute 'mode-line-evil-visual-face nil
                    :inherit 'mode-line-mode-face
                    :foreground "black"
                    :background "light sea green"
                    :weight 'bold)

(set-face-attribute 'mode-line-evil-replace-face nil
                    :inherit 'mode-line-mode-face
                    :foreground "white"
                    :background "red"
                    :weight 'bold)

(set-face-attribute 'mode-line-evil-motion-face nil
                    :inherit 'mode-line-mode-face
                    :foreground "white"
                    :background "black"
                    :weight 'bold)

(setq evil-normal-state-tag "NORMAL")
(setq evil-insert-state-tag "INSERT")
(setq evil-visual-state-tag "VISUAL")
(setq evil-emacs-state-tag "EMACS")
(setq evil-replace-state-tag "REPLACE")



(defun my/evil-mode-state ()
  (cond
   ((and (bound-and-true-p evil-mode) (modeline-selected-window-active))
    (let* ((tag  (concat " " (upcase (symbol-name evil-state)) " "))
           (face (get-modeline-face-for-evil-state)))
      ;;(print tag)
      ;;(print face)
      (propertize tag 'face face)
            ))
   (t
    "")))

;;;###autoload
(defun get-modeline-face-for-evil-state ()
  "Function to select appropriate face based on `evil-state'."
  (let* ((face (intern (concat "mode-line-evil-" (symbol-name evil-state) "-face"))))
    (if (facep face) face nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Nice mode line with arrows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun arrow-right-xpm (color1 color2)
  "Return an XPM right arrow string representing."
  (format "/* XPM */
static char * arrow_right[] = {
\"12 18 2 1\",
\". c %s\",
\"  c %s\",
\".           \",
\"..          \",
\"...         \",
\"....        \",
\".....       \",
\"......      \",
\".......     \",
\"........    \",
\".........   \",
\".........   \",
\"........    \",
\".......     \",
\"......      \",
\".....       \",
\"....        \",
\"...         \",
\"..          \",
\".           \"};"  color1 color2))

(defun arrow-left-xpm (color1 color2)
  "Return an XPM right arrow string representing."
  (format "/* XPM */
static char * arrow_right[] = {
\"12 18 2 1\",
\". c %s\",
\"  c %s\",
\"           .\",
\"          ..\",
\"         ...\",
\"        ....\",
\"       .....\",
\"      ......\",
\"     .......\",
\"    ........\",
\"   .........\",
\"   .........\",
\"    ........\",
\"     .......\",
\"      ......\",
\"       .....\",
\"        ....\",
\"         ...\",
\"          ..\",
\"           .\"};"  color2 color1))


(defvar mode-line-show-minor-modes nil)

(setq color1 "Red") ;; "#777777")
(setq color2 "Orange") ;; "#555555"))
(setq scale-factor 10.0

      arrow-right-1 (create-image (arrow-right-xpm color1 color2) 'xpm t :ascent 'center :scale scale-factor)
      arrow-right-2 (create-image (arrow-right-xpm color2 "None") 'xpm t :ascent 'center :scale scale-factor)
      arrow-left-1  (create-image (arrow-left-xpm color2 color1) 'xpm t :ascent 'center :scale scale-factor)
      arrow-left-2  (create-image (arrow-left-xpm "None" color2) 'xpm t :ascent 'center :scale scale-factor))

;;   (setq-default mode-line-format
;;                 (list  '(:eval (concat (propertize " %* %I %b" 'face 'mode-line-color-1)
;;                                        (propertize " " 'display arrow-right-1)))
;;                        '(:eval (concat (propertize " %[%m%] " 'face 'mode-line-color-2
;;                                                    'mouse-face 'mode-line-color-2
;;                                                    'local-map (make-mode-line-mouse-map
;;                                                                'mouse-1 (lambda () (interactive)
;;                                                                           (setq mode-line-show-minor-modes
;;                                                                                 (not mode-line-show-minor-modes))
;;                                                                           (redraw-modeline))))
;;                                        (propertize " " 'display arrow-right-2)))

;;                        '(:eval (if mode-line-show-minor-modes mode-line-modes
;;                                  global-mode-string))

;;                        ;; Justify right by filling with spaces to right fringe - 16
;;                        ;; (16 should be computed rahter than hardcoded)
;;                        '(:eval (propertize " " 'display '((space :align-to (- right-fringe 20)))))

;;                        '(:eval (concat (propertize " " 'display arrow-left-2)
;;                                        (propertize " %6p " 'face 'mode-line-color-2)))
;;                        '(:eval (concat (propertize " " 'display arrow-left-1)
;;                                        (propertize "%4l:%2c      " 'face 'mode-line-color-1)))
;;                        ))

;;   (make-face 'mode-line-color-1)
;;   (set-face-attribute 'mode-line-color-1 nil
;;                       :foreground "#fff"
;;                       :background color1)

;;   (make-face 'mode-line-color-2)
;;   (set-face-attribute 'mode-line-color-2 nil
;;                       :foreground "#fff"
;;                       :background color2)

;;   (set-face-attribute 'mode-line nil
;;                       :foreground "#fff"
;;                       :background "#2b2b2b"
;;                       :box nil)
;;   (set-face-attribute 'mode-line-inactive nil
;;                       :foreground "#fff"
;;                       :background color2))
