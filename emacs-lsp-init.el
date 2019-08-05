(require 'cc-mode)

(use-package lsp-mode
  :init (setq lsp-inhibit-message nil ; you may set this to t to hide messages from message area
              lsp-eldoc-render-all nil
              lsp-highlight-symbol-at-point nil)
  :hook ((python-mode . lsp)
         (java-mode . lsp))
  :config

  ;;  (lsp-define-stdio-client lsp-python "python"
  ;;                         #'projectile-project-root
  ;;                       '("pyls"))
  ;;  (add-hook 'python-mode-hook
  ;;          (lambda ()
  ;;          (lsp-python-enable)))

  (setq
   python-indent-offset 4
   python-shell-interpreter "jupyter"
   python-shell-interpreter-args "console --simple-prompt"
   python-shell-prompt-detect-failure-warning nil)
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "jupyter")
  )

(use-package company-lsp
  :after  company
  :config
  (push 'company-lsp company-backends)
  (setq company-lsp-enable-snippet t
        company-lsp-cache-candidates t)
  )

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (require 'lsp-ui-imenu)
  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)
  (require 'lsp-ui-flycheck)
  (add-hook 'lsp-after-open-hook (lambda () (lsp-ui-flycheck-enable 1)))
  (setq lsp-ui-sideline-enable t
        lsp-ui-sideline-show-symbol t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-sideline-update-mode 'point)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  )

(use-package helm-lsp :commands helm-lsp-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
;; optionally if you want to use debugger

(use-package lsp-java
  :after lsp
  :requires (lsp-ui-flycheck lsp-ui-sideline)
  :config
  (add-hook 'java-mode-hook  'lsp-java-enable)
  (add-hook 'java-mode-hook  #'lsp)
  (add-hook 'java-mode-hook  'flycheck-mode)
  (add-hook 'java-mode-hook  'company-mode)
  (add-hook 'java-mode-hook  (lambda () (lsp-ui-flycheck-enable t)))
  (add-hook 'java-mode-hook  'lsp-ui-sideline-mode)
  (setq lsp-java--workspace-folders (list "~/src/rspace-eclipse/scharp")))


(use-package dap-mode
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t)
  (require 'dap-java)
  (require 'dap-python))


;; pip install python-language-server[all]
;; pip instll pyls-mypy Mypy type checking for Python 3
;; pip intall pyls-isort Isort import sort code formatting
;; pip install pyls-black for code formatting using Black
;; pip3 install virtualenvwrapper
;; pip install keras
;; pip install tensorflow
;; pip install matplotlib
;; pip install -U tensorflow==2.0.0-beta1
;; pip install ipython
;; pip install jupyter -U
;; pip install --upgrade pyzmq
;; pip install python-language-server[all]
;; pip install autoflake
;; pip install isort
;; pip install flake8
;; pip install black
;; pip install -U tensorflow-datasets
;; pip install pyflakes
;; pip instll pyls-mypy
;; pip install pyls-mypy
;; pip install pyls-isort
;; pip install pyls-black
;; pip install "ptvsd>=4.2"
;; (use-package lsp-python-ms
;;   :defer 0.3
;;   :config
;;   :hook (python-mode . (lambda ()
;;                          (require 'lsp-python-ms)
;;                          (lsp)))  ; or lsp-deferred
;;   :custom
;;   (lsp-python-ms-dir (expand-file-name "~/.emacs.d/elisp/python-language-server/output/bin/Release/"))
;;   (lsp-python-ms-executable "~/.emacs.d/elisp/python-language-server/output/bin/Release/linux-x64/publish/Microsoft.Python.LanguageServer"))
