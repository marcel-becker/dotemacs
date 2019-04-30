(use-package anaconda-mode :defer t)
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

(use-package company-anaconda
  :ensure t
  :hook (python-mode . (lambda () (add-to-list (make-local-variable 'company-backends)'(company-anaconda :with company-capf)))))
