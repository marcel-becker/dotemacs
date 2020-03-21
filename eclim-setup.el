(use-package eclim
  :config
  (defun my-java-mode-hook ()
    (eclim-mode t))
  ;;  (global-eclim-mode)
  (setq eclim-auto-save t)
  (setq eclimd-autostart t)
  (setq help-at-pt-display-when-idle t)
  (setq help-at-pt-timer-delay 0.1)
  (help-at-pt-set-timer)
  (add-hook 'java-mode-hook 'my-java-mode-hook))


(use-package company)
(use-package company-emacs-eclim
  :config
  (company-emacs-eclim-setup)
  (global-company-mode t)
  (setq company-emacs-eclim-ignore-case t))

;;(use-package flymake)
;; (defun my-flymake-init ()
;;   (list "my-java-flymake-checks"
;;   (list (flymake-init-create-temp-buffer-copy
;;   'flymake-create-temp-with-folder-structure))))
;; (add-to-list 'flymake-allowed-file-name-masks
;;              '("\\.java$" my-flymake-init flymake-simple-cleanup))

(when running-linux
  (custom-set-variables
   '(eclim-eclipse-dirs '("~/eclipse"))
   '(eclim-executable "/home/becker/eclipse/plugins/org.eclim_2.8.0/bin/eclim")))

(when running-macos
(custom-set-variables
 '(eclim-eclipse-dirs '("/Applications/Eclipse.app/Contents/Eclipse"))
 '(eclim-executable "/Applications/Eclipse.app/Contents/Eclipse/plugins/org.eclim_2.8.0/bin/eclim")))

(add-hook 'java-mode-hook (lambda ()
                            (setq c-basic-offset 2)
                            (setq fill-column 120)
                            (fci-mode t)
                            (subword-mode t)
                            (local-set-key (kbd "C-M-h") 'windmove-left)
                            (hs-minor-mode 1)))
