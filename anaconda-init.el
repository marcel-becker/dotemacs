(use-package anaconda-mode :defer t)
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

(use-package company-anaconda
  :ensure t
  :hook (python-mode . (lambda () (add-to-list (make-local-variable 'company-backends)'(company-anaconda :with company-capf)))))


(use-package pyimport)
;;(use-package importmagic)
;; pip install isort
(use-package py-isort)

(use-package ein
  :config
  (require 'ein-notebook)
  (require 'ein-subpackages)
  (setq ein:completion-backend 'ein:use-company-backend)
  (custom-set-faces
   '(ein:cell-input-area ((t (:background "#09223F"))))
   '(ein:cell-output-area ((t (:background "blue2" :foreground "white"))))
   '(ein:cell-input-prompt ((t (:inherit header-line :background "Orange" :foreground "Black"  :weight bold)))))
  )


(global-flycheck-mode)
(add-hook 'python-mode-hook
          (lambda ()
            (anaconda-mode)
            (anaconda-eldoc-mode)
            ;;          (importmagic-mode)
            (add-to-list 'helm-boring-buffer-regexp-list "\\*epc con")
            (local-set-key (kbd "C-x C-d") 'anaconda-mode-show-doc)
            (local-set-key (kbd "C-x C-w") 'anaconda-mode-find-definitions)
            (add-hook 'before-save-hook 'pyimport-remove-unused)
            ;;        (add-hook 'before-save-hook 'importmagic-fix-imports)
            (add-hook 'before-save-hook 'pyimpsort-buffer)
            (add-hook 'before-save-hook 'blacken-buffer)
            (set (make-local-variable 'compile-command) (concat "python3 " (buffer-name)))))
