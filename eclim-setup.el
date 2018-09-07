(use-package eclim)
(global-eclim-mode)


(setq eclimd-autostart t)

(defun my-java-mode-hook ()
    (eclim-mode t))

(add-hook 'java-mode-hook 'my-java-mode-hook)

(custom-set-variables
  '(eclim-eclipse-dirs '("~/eclipse"))
  '(eclim-executable "/home/becker/eclipse/plugins/org.eclim_2.8.0/bin/eclim"))


;;(use-package  eclimd)

(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)

(use-package company)
(use-package company-emacs-eclim)
(company-emacs-eclim-setup)
(global-company-mode t)
(setq company-emacs-eclim-ignore-case t)


(setq eclim-auto-save t)


;; regular auto-complete initialization
;;(require 'auto-complete-config)
;;(ac-config-default)
;; add the emacs-eclim source
;;(require 'ac-emacs-eclim-source)
;;(ac-emacs-eclim-config)








(use-package flymake)
(defun my-flymake-init ()
  (list "my-java-flymake-checks"
  (list (flymake-init-create-temp-buffer-copy
  'flymake-create-temp-with-folder-structure))))
(add-to-list 'flymake-allowed-file-name-masks
             '("\\.java$" my-flymake-init flymake-simple-cleanup))

(custom-set-variables
  '(eclim-eclipse-dirs '("/Applications/Eclipse.app/Contents/Eclipse"))
  '(eclim-executable "/Applications/Eclipse.app/Contents/Eclipse/eclim"))


(add-hook 'java-mode-hook (lambda ()
                            (setq c-basic-offset 2)
                            (setq fill-column 100)
                            (fci-mode t)
                            (subword-mode t)
                            (local-set-key (kbd "C-M-h") 'windmove-left)
                            (hs-minor-mode 1)))
