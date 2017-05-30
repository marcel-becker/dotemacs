;;package-activated-list is a variable defined in `package.el'.
;; Its value is

(defvar marcel-lisp-dir
(if (eq system-type 'windows-nt)      ; Windows
    (cond ((file-exists-p "C:/Dropbox/.emacs.d")
          "C:/Dropbox/.emacs.d")
          ((file-exists-p "D:/Dropbox/.emacs.d")
           "D:/Dropbox/.emacs.d")
          (t
           (expand-file-name "~/.emacs.d")))
  (cond ((file-exists-p  "~/Dropbox/.emacs.d")
         "~/Dropbox/.emacs.d")
        (t
         (expand-file-name "~/.emacs.d"))))
"Address of Marcel's lisp libraries.")


(defvar running-ms-windows
  (eq system-type 'windows-nt))

(defvar running-macos
  (eq system-type 'darwin))

(defvar running-linux
  (eq system-type 'gnu/linux))


;; key bindings
(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-modifier 'alt)
  (setq mac-right-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  (setq mac-allow-anti-aliasing t)
  (global-set-key [kp-delete] 'delete-char)) ;; sets fn-delete to be right-delete


;; add everything under ~/.emacs.d to it
(unless (boundp 'marcel-lisp-dir)
  (defvar marcel-lisp-dir
    (expand-file-name "~/.emacs.d")
    "Address of Marcel's lisp libraries."))

(setq user-emacs-directory marcel-lisp-dir)

;;; ADD Marcel'S LISP LIBRARY TO `load-path'.
(add-to-list 'load-path  (concat marcel-lisp-dir "/"))
