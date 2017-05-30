;package-activated-list is a variable defined in `package.el'.
;Its value is

 (let ((elget-lib (concat marcel-lisp-dir "/el-get/el-get")))
   (if (file-exists-p elget-lib)
       (add-to-list 'load-path elget-lib)))

(unless (require 'el-get nil 'noerror)
   (with-current-buffer
       (url-retrieve-synchronously
        "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
     (goto-char (point-max))
     (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path (concat marcel-lisp-dir "/el-get/el-get/recipes"))
(setq el-get-default-process-sync t  el-get-verbose t)


(require 'el-get-elpa)
;; Build the El-Get copy of the package.el packages if we have not
;; built it before.  Will have to look into updating later ...
(unless (file-directory-p el-get-recipe-path-elpa)
  (el-get-elpa-build-local-recipes))




(with-output-to-temp-buffer "*scratch*"
  (dolist (p all-el-get-packages)
    (if (not (memq p  my:el-get-packages))
        (princ (format "\npackage %s not in list" p))))
  (switch-to-buffer "*scratch*"))

(defun print-list ()
  (let ((my-list my-packages))
    (dolist (package my-list)
      (message "%s" package)
      )))




(with-output-to-temp-buffer "*scratch*"
  (dolist (p spacemacs-packages)
    (if (not (memq p  all-el-get-packages))
        (princ (format "\npackage %s not in list" p))))
  (switch-to-buffer "*scratch*"))


;; (dolist (p my-packages)  (when (not (package-installed-p p)) (package-install p)))

;; (dolist (p spacemacs-packages) (when (not (package-installed-p p)) (package-install p)))

;(dolist (p all-packages) (when (not (package-installed-p p)) (package-install p)))



(el-get 'sync spacemacs-packages)
(el-get 'sync el-get-packages)
(el-get 'sync my-packages)
(el-get 'sync all-el-get-packages)

(el-get 'sync all-packages)


(with-output-to-temp-buffer "*scratch*"
  (dolist (p my:all-elpa-packages)
    (if (not (memq p  all-el-get-packages))
        (princ (format "\npackage %s not in list" p))))
  (switch-to-buffer "*scratch*"))


(with-output-to-temp-buffer "*scratch*"
  (dolist (p my:all-my-packages) (princ (format "(use-package %s :ensure t)\n" p)))
  (switch-to-buffer "*scratch*"))

(with-output-to-temp-buffer "*scratch*"
  (dolist (p my:all-my-packages) (princ (format "%s\n" p)))
  (switch-to-buffer "*scratch*"))


(with-output-to-temp-buffer "*scratch*"
  (print (sort (delete-dups (append my-packages el-get-packages spacemacs-packages all-el-get-packages my:el-get-packages)) 'string<))
  (switch-to-buffer "*scratch*"))


(defun my-load-packages ()
  (interactive)
(dolist (p my:all-elpa-packages)
  (progn (message "installing package %s" p)
         (when (not (package-installed-p p)) (package-install p))
         (message "loading package %s" p)
         (require p))))


(require 'spacemacs-dark-theme)

(el-get 'sync my:all-my-packages)


(with-output-to-temp-buffer "*scratch*"
  (let ((all-packages (sort (delete-dups (append spacemacs-dotfile  my-init-elpa-packages spacemacs-init-packages )) 'string<)))
    ;;(print (sort (delete-dups (append spacemacs-dotfile  my-init-elpa-packages spacemacs-init-packages )) 'string<))
    (dolist (p all-packages)
      (princ (format "%s\n" p))
  (switch-to-buffer "*scratch*"))))
