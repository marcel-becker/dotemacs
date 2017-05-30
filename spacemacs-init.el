;;; spacemacs-init.el --- Spacemacs Initialization File

(setq user-emacs-directory (expand-file-name "~/Dropbox/.spacemacs.d/"))
(add-to-list 'load-path user-emacs-directory)
(setq package-user-dir (concat user-emacs-directory "/elpa"))
(add-to-list 'load-path package-user-dir)

(load (concat user-emacs-directory "init.el"))
