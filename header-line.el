(defmacro with-face (str &rest properties)
    `(propertize ,str 'face (list ,@properties)))

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
                    :inverse-video nil
                    :weight 'bold
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



(defun sl/display-header ()
  (interactive)
    (setq header-line-format
     ;     '("" ;; invocation-name
            (concat (propertize " " 'face 'mb-header-line-face 'display '((space :align-to (- left-fringe 3))))
                    (propertize " FILE   " 'face 'mb-header-line-face)
                    (if (buffer-file-name)
                        (sl/make-header))
                    (if (not (buffer-file-name))
                        (propertize "%b" 'face 'mb-header-line-face))
                    (propertize  " " 'face 'mb-header-line-face 'display '((space :align-to (- right-fringe 16))))
                    (propertize display-time-string 'face 'mb-header-line-time-face )
                    (propertize  " " 'face 'mb-header-line-face 'display '((space :align-to (+ right-fringe 3))))))
    ;)
    (force-mode-line-update)
    )


  (add-hook 'buffer-list-update-hook 'sl/display-header)



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
