 (defmacro with-face (str &rest properties)
    `(propertize ,str 'face (list ,@properties)))

  (defun sl/make-header ()
    ""
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
                                   ;; :background "red"
                                   :weight 'bold
                                   )))
            (concat (with-face sl/header
                               ;; :background "red"
                               :foreground "#8fb28f"
                               :weight 'bold
                               )))
        (concat (with-face sl/header
                           ;; :background "green"
                           ;; :foreground "black"
                           :weight 'bold
                           :foreground "#8fb28f"
                           )
                (with-face (file-name-nondirectory buffer-file-name)
                           :weight 'bold
                           ;; :background "red"
                           )))))


(set-face-attribute 'header-line nil
                    :background "blue2"
                    :foreground "#eab700"
                    :inverse-video nil
                    :weight 'bold
                    :box '(:line-width 6 :color "orange" :style nil))


(defun sl/display-header ()
  (interactive)
    (setq header-line-format
          '("" ;; invocation-name
            (propertize " " 'display '((space :align-to (- left-fringe 3))))
            (:eval (if (buffer-file-name)
                       (sl/make-header)
                     "%b"))

            ))
    (force-mode-line-update)
    )



  (add-hook 'buffer-list-update-hook
            'sl/display-header)
