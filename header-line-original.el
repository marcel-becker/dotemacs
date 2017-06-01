(defmacro with-face (str &rest properties)
  `(propertize ,str 'face (list ,@properties)))


;(setq display-time-day-and-date t
 ;;     display-time-24hr-format nil)


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
                                   ;;:background "blue"
                                   :weight 'bold
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
                   )
                        (with-face (substring sl/header
                                              (+ (- (length sl/header)
                                                    (window-body-width))
                                                 (length sl/drop-str))
                                              (length sl/header))
                                   ;;:background "red"
                                   :weight 'bold
                                   )))
            (concat (with-face sl/header
                               ;;:background "red"
                               ;;:foreground "#8fb28f"
                               :weight 'bold
                               )))
        (concat (with-face sl/header
                           ;;:background "blue2"
                           :foreground "white"
                           ;;:box '(:line-width 6 :color "orange")
                           :weight 'bold
                           )
                (with-face (file-name-nondirectory buffer-file-name)
                           ;;:weight 'bold
                           ;;:background "blue2"
                           ;;:box '(:line-width 6 :color "orange")
                           ;;:foreground "#eab700"
                           :weight 'extra-bold)
                           ))))


;; (set-face-attribute 'header-line nil
;;                     :background "blue2"
;;                     :foreground "#eab700"
;;                     :inverse-video nil
;;                     :weight 'bold
;;                     :box '(:line-width 6 :color "orange" :style nil))


(defun sl/display-header ()
  (interactive)
    (setq header-line-format
          '("" ;; invocation-name
            (:eval (propertize " " 'display '((space :align-to (- left-fringe 3)))))
            " FILE   "
            (:eval (if (buffer-file-name)
                       (sl/make-header)
                     "%b"))
            (:eval (propertize  " " 'display '((space :align-to (- right-fringe 16)))))
            display-time-string
            (:eval (propertize  " " 'display '((space :align-to (+ right-fringe 3)))))

            ))
    (force-mode-line-update)
    )



  (add-hook 'buffer-list-update-hook
            'sl/display-header)
