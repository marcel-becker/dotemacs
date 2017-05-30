(setq my:el-python-packages
 '(
   elpy
   ))

(el-get 'sync my:el-python-packages)

(when (require 'elpy nil t)
  (elpy-enable)
  (elpy-use-ipython)
  )

(setq elpy-rpc-backend "jedi")
;(autoload 'jedi:setup "jedi" nil t)
;(add-hook 'python-mode-hook 'jedi:setup)
