(defmacro with-face (str &rest properties)
    `(propertize ,str 'face (list ,@properties)))


(defun my-buffer-name-header-string ()
  (if (buffer-file-name
       (let* ((buffer-name-str (file-name-nondirectory buffer-file-name))
              (buffer-dir (file-name-directory buffer-file-name)))
          (concat (with-face buffer-dir
                           :background "blue2"
                           :foreground "white"
                           :box '(:line-width 6 :color "orange")
                           :weight 'bold
                           )
                  (with-face buffer-name-str
                           :weight 'bold
                           :background "blue2"
                           :box '(:line-width 6 :color "orange")
                           :foreground "#eab700"
                           :weight 'extra-bold)
                           )))
      (let* ((buffer-name-str (buffer-name))
             (name-size (length buffer-name-str))
             (buffer-name-string (concat buffer-name (make-string (- 60 name-size) ?\s))))
         (with-face buffer-name-str
                         :weight 'bold
                         :background "blue2"
                         :box '(:line-width 6 :color "orange")
                         :foreground "#eab700"
                         :weight 'extra-bold)
              )))


  (defun sl/make-header ()
    "  "
    (let* ((sl/full-header (abbreviate-file-name buffer-file-name))
           (sl/header (file-name-directory sl/full-header))
           (sl/drop-str "[...]"))
      (if (> (length sl/full-header)
             (window-body-width))
          (if (> (length sl/header)
                 (window-body-width))
              (progn
                (concat (with-face sl/drop-str
                                   :background "blue"
                                   :weight 'bold
                                   )
                        (with-face (substring sl/header
                                              (+ (- (length sl/header)
                                                    (window-body-width))
                                                 (length sl/drop-str))
                                              (length sl/header))
                                   :background "red"
                                   :weight 'bold
                                   )))
            (concat (with-face sl/header
                               :background "red"
                               :foreground "#8fb28f"
                               :weight 'bold
                               )))
        (concat (with-face sl/header
                           :background "blue2"
                           :foreground "white"
                           :box '(:line-width 6 :color "orange")
                           :weight 'bold
                           )
                (with-face (file-name-nondirectory buffer-file-name)
                           :weight 'bold
                           :background "blue2"
                           :box '(:line-width 6 :color "orange")
                           :foreground "#eab700"
                           :weight 'extra-bold)
                           ))))

(make-face 'mb-header-line-face)
(set-face-attribute 'mb-header-line-face nil
                    :background "blue2"
                    :foreground "orange"
                    :inverse-video nil
                    :weight 'extra-bold
                    :box '(:line-width 6 :color "orange" :style nil))

(make-face 'mb-header-line-face-inactive)
(set-face-attribute 'mb-header-line-face-inactive nil
                    :inherit 'mode-line-inactive
                    ;;:background "gray60"
                    ;;:foreground "white"
                    ;;:inverse-video nil
                    :weight 'extra-bold
                    :box '(:line-width 6 :color "gray60" :style nil))



(make-face 'mb-header-line-buffer-name-face)
(set-face-attribute 'mb-header-line-buffer-name-face nil
                    :background "blue2"
                    :foreground "white"
                    :inverse-video nil
                    :weight 'extra-bold
                    :box '(:line-width 6 :color "orange" :style nil))


(make-face 'mb-header-line-time-face)
(set-face-attribute 'mb-header-line-time-face nil
                    :background "blue2"
                    :foreground "#eab700"
                    :inverse-video nil
                    :weight 'bold
                    :box '(:line-width 6 :color "orange" :style nil))

(make-face 'mb-header-line-inactive)
(set-face-attribute 'mb-header-line-inactive nil
                    :foreground "white";"gray80"
                    :background "gray60"
                    :inverse-video nil
                    :box '(:line-width 6 :color "black" :style nil))


(make-face 'mb-header-line-folder-face )
(set-face-attribute 'mb-header-line-folder-face  nil
                    :inherit 'mode-line-mode-face
                    :background "blue2"
                    :foreground "white" ;"gray60"
                    :weight 'bold
                    :inverse-video nil
                    :box '(:line-width 6 :color "orange" :style nil))

(make-face 'mb-header-line-filename-face)
(set-face-attribute 'mb-header-line-filename-face nil
                    :inherit 'mode-line-mode-face
                    :background "blue2"
                    :foreground "orange"
                    :weight 'extra-bold
                    :box '(:line-width 6 :color "orange" :style nil))



;; (defun my-display-header ()
;;   (interactive)
;;   (setq
;;    header-line-format
;;    (concat
;;         (if (buffer-file-name)
;;             (propertize " FILE:   " 'face 'mb-header-line-face)
;;           (propertize " BUFFER: " 'face 'mb-header-line-face))
;;         ;; directory and buffer/file name
;;         (if (buffer-file-name)
;;             (propertize  (shorten-directory default-directory 30)
;;                          'face 'mb-header-line-folder-face))
;;         (propertize (concat "%b" (make-string 20 ?\s))  'face 'mb-header-line-filename-face)
;;         (propertize  " "  'face 'mb-header-line-face 'display '((space :align-to (- (+ right-margin right-fringe) 18))))
;;         (propertize display-time-string 'face 'mb-header-line-time-face)
;;         (propertize  " "  'face 'mb-header-line-face 'display '((space :align-to  (+ 1 right-margin right-fringe))))
;;         ))
;;    (force-mode-line-update))

;; (add-hook 'buffer-list-update-hook 'my-display-header)




(defvar modeline-selected-window nil)

(defun header-line-set-selected-window ()
  (when (not (minibuffer-window-active-p (frame-selected-window)))
    (setq modeline-selected-window (selected-window))))

(add-function :before pre-redisplay-function
              (lambda (_wins) (setq modeline-selected-window (selected-window))))
;;(defun modeline-record-selected-window ()
;; (setq modeline-selected-window (selected-window)))


(defun modeline-selected-window-active ()
  "Return whether the current window is active."
  (eq modeline-selected-window (selected-window)))



(add-hook 'window-configuration-change-hook #'header-line-set-selected-window)
(add-hook 'focus-in-hook #'header-line-set-selected-window)
(defadvice select-window (after header-line-select-window activate)
  "Set telephone-line's selected window value for use in determining the active mode-line."
  (header-line-set-selected-window))
(defadvice select-frame (after header-line-select-frame activate)
  "Set telephone-line's selected window value for use in determining the active mode-line."
  (header-line-set-selected-window))

;;(format-mode-line header-line-format)

(defun my-display-header ()
  (interactive)
  ;;(message (format "Active window %s this window %s active %s" modeline-selected-window (selected-window) (modeline-selected-window-active)))
  (setq header-line-format
        '(:eval
          (let* ((active (modeline-selected-window-active))
                 (face (if active 'mb-header-line-face 'mb-header-line-face-inactive))
                 (folder (if active 'mb-header-line-folder-face 'mb-header-line-face-inactive))
                 (file (if active 'mb-header-line-filename-face 'mb-header-line-face-inactive))
                 (time (if active 'mb-header-line-time-face 'mb-header-line-face-inactive)))
            (concat
             (if (buffer-file-name)
                 (propertize " FILE:   " 'face face)
               (propertize " BUFFER: " 'face face))
             ;; directory and buffer/file name
             (if (buffer-file-name)
                 (propertize  (shorten-directory default-directory 30)
                              'face folder))
             (propertize (concat "%b" (make-string 20 ?\s))  'face file)
             (propertize  " "  'face face 'display '((space :align-to (- (+ right-margin right-fringe) 18))))
             (propertize display-time-string 'face time)
             (propertize  " "  'face face 'display '((space :align-to  (+ 1 right-margin right-fringe))))
             ))))
   (force-mode-line-update t))

(add-hook 'buffer-list-update-hook 'my-display-header)



;; (setq
;;  header-line-format
;;  '("%e"
;;   (:eval (if (buffer-file-name)
;;       (propertize " FILE:   " 'face face)
;;     (propertize " BUFFER: " 'face face)))
;;   ;; directory and buffer/file name
;;   (:eval (if (buffer-file-name)
;;             (propertize  (shorten-directory default-directory 30)
;;                          'face folder)))
;;   (:eval (propertize (concat "%b" (make-string 20 ?\s))  'face file))
;;   (:eval (propertize  " "  'face face 'display '((space :align-to (- (+ right-margin right-fringe) 18)))))
;;   (:eval (propertize display-time-string 'face time))
;;   (:eval (propertize  " "  'face face 'display '((space :align-to  (+ 1 right-margin right-fringe))))
;;   )))





;; (defun my-update-header ()
;;   (mapc
;;    (lambda (window)
;;      (with-current-buffer (window-buffer window)
;;        ;; don't mess with buffers that don't have a header line
;;        (when header-line-format
;;          (let ((original-format (get 'header-line-format 'original))
;;                (inactive-face 'mb-header-line-face-inactive)) ; change this to your favorite inactive header line face
;;            ;; if we didn't save original format yet, do it now
;;            (when (not original-format)
;;              (put 'header-line-format 'original header-line-format)
;;              (setq original-format header-line-format))
;;            ;; check if this window is selected, set faces accordingly
;;            (if (eq window  (selected-window))
;;                (setq header-line-format original-format)
;;              (setq header-line-format `(:propertize ,original-format face ,inactive-face)))))))
;;    (window-list))
;;   (force-mode-line-update t)
;;   )

;; (remove-hook 'buffer-list-update-hook 'my-display-header)
;; (add-hook 'buffer-list-update-hook 'my-update-header)


;; (defun get-header-format (header-face)
;;     (let (( local-header-line-format
;;           '("" ;; invocation-name
;;             (:eval (concat (propertize " " 'face 'header-face 'display '((space :align-to (- left-fringe 3))))
;;                            (propertize " FILE   " 'face 'header-face)
;;                            (if (buffer-file-name)
;;                                       (sl/make-header)
;;                                     "%b")
;;                            (propertize  " " 'face 'header-face 'display '((space :align-to (- right-fringe 16))))
;;                            (propertize display-time-string 'face 'header-face )
;;                            (propertize  " " 'face 'header-face 'display '((space :align-to (+ right-fringe 3)))))))))
;;     local-header-line-format))



;; (defun my-update-line-header ()
;;   (interactive)
;;   (mapc
;;    (lambda (window)
;;      (with-current-buffer (window-buffer window)
;;        (if (eq window (selected-window))
;;            (setq header-line-format (get-header-format 'mb-header-line-face))
;;          (setq header-line-format (get-header-format 'mb-header-line-inactive)))))
;;    (window-list))
;;   (force-mode-line-update)
;;   )


;; (add-hook 'buffer-list-update-hook #'my-update-line-header)
