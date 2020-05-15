;; pip -vvv install argparse --no-cache-dir
;; # Either of these
;; pip install rope
;; pip install jedi
;; # flake8 for code checks
;; pip install flake8
;; # importmagic for automatic imports
;; pip install importmagic
;; # and autopep8 for automatic PEP8 formatting
;; pip install autopep8
;; # and yapf for code formatting
;; ###pip install yapf

;;(defvar company-idle-delay 0)

;;(setq my:el-python-packages
;;(
;;anything
;;anything-ipython
;;company-jedi
;;pymacs
;;pylookup
;;ein
;;smartrep
;;      ))

                                        ;(el-get 'sync my:el-python-packages)

;; (require 'deferred)
;; (require 'highlight-indentation)
;; (require 'py-autopep8)
;; (require 'pydoc)
;; (require 'pydoc-info)
;; (require 'python)
;; (require 'python-pep8)
;; (require 's)
;;(require 'python-environment)
;;(require 'pyvenv)

;; https://github.com/porterjamesj/virtualenvwrapper.el
;; venv-workon
;; venv-deactivate
;; venv-mkvirtualenv
(use-package virtualenvwrapper
  :config
  (venv-initialize-interactive-shells) ;; if you want interactive shell support
  (venv-initialize-eshell) ;; if you want eshell support
  ;; (setq venv-location '("/path/to/project1-env/" "/path/to/ptoject2-env/"))
  (setq venv-location "~/PythonEnvs")
  )

(use-package jedi)
(use-package jedi-core)


(use-package elpy
  :diminish "elpy"
  :config
  (elpy-enable)
  (setq elpy-rpc-backend "jedi")
  (define-key python-mode-map (kbd "RET") 'newline-and-indent)
  (setq python-shell-interpreter "jupyter"
        python-shell-interpreter-args "console --simple-prompt"
        python-shell-prompt-detect-failure-warning nil)
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "jupyter")

  (setq
   ;;   python-shell-interpreter "ipython"
   ;;   python-shell-interpreter-args "--colors Linux --pylab  --matplotlib"

   python-shell-prompt-regexp "In \\[[0-9]+\\]: " ;; "In \: "
   python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: " ;; "Out\: "
   python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
   python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))"
   python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))")
  )


(use-package py-autopep8)

;; Fixing a key binding bug in elpy
(define-key yas-minor-mode-map (kbd "C-c k") 'yas-expand)
;; Fixing another key binding bug in iedit mode
(define-key global-map (kbd "C-c i") 'iedit-mode)
;; Custom Keybinds
;; (add-hook 'python-mode-hook
;;         (lambda ()
;;                 (local-set-key (kbd "C-c p") 'pylint)
;;                 (local-set-key (kbd "C-c o") 'pep8)
;;                 (local-set-key (kbd "C-c y") 'ipython)
;;                 (fci-mode 1)
;;                 ))

(setq company-selection-wrap-around t
      company-show-numbers t
      company-tooltip-flip-when-above t)


;;(defun my/python-mode-hook ()
;;   (add-to-list 'company-backends 'company-jedi))
;;(add-hook 'python-mode-hook 'my/python-mode-hook)

(use-package company-jedi
  :config
  (add-to-list 'company-backends '(company-jedi company-files)))



;; Use M-x venv-workon to activate virtualenvs and M-x venv-deactivate deactivate them.
(setq python-shell-virtualenv-path "~/PythonEnvs")
(setq python-environment-directory "~/PythonEnvs")

(add-hook 'pyvenv-post-activate-hooks 'pyvenv-restart-python)
(when (and (equal (getenv "VIRTUAL_ENV") nil)
           (file-exists-p (concat (getenv "WORKON_HOME") "/planx-eclipse")))
  (pyvenv-workon "planx-eclipse")
  (setq python-environment-default-root-name  "planx-eclipse"))

(when (getenv "VIRTUAL_ENV")
  (let ((virtual-env (getenv "VIRTUAL_ENV")))
    (setq python-environment-default-root-name
          (file-name-nondirectory (directory-file-name (file-name-directory (getenv "VIRTUAL_ENV")))))))


(setq-default mode-line-format (cons '(:exec pyvenv-virtual-env-name) mode-line-format))
(setq eshell-prompt-function
      (lambda ()  (concat pyvenv-virtual-env-name " $ ")))


;;(setq elpy-rpc-backend "rope")
(setq elpy-rpc-backend "jedi")
(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)


(require 'color)
;; (let ((bg (face-attribute 'default :background)))
;;   (custom-set-faces
;;    `(company-tooltip ((t (:inherit default :background "skyblue"))))
;;    `(company-scrollbar-bg ((t (:background "green"))));;,(color-lighten-name bg 10)))))
;;    `(company-scrollbar-fg ((t (:background "green"))));;,(color-lighten-name bg 5)))))
;;    `(company-tooltip-selection ((t (:inherit font-lock-function-name-face :background "black" :foreground "yellow"))))
;;    `(company-tooltip-common ((t (:inherit font-lock-constant-face :background "black"))))))


;; Start PEP8
(require 'tramp)
(require 'compile)
;;(require 'jedi)
;;(autoload 'python-pep8 "python-pep8")
;;(autoload 'pep8 "python-pep8")
;;(load "python-pep8")

(setq flycheck-flake8-maximum-line-length 120)
(setq flycheck-highlighting-mode 'lines)

(setq gud-pdb-command-name "python -m pdb")

;;(add-hook 'python-mode-hook 'highlight-indentation-mode)
;;(add-hook 'python-mode-hook 'highlight-indentation-current-column-mode)
(set-face-background 'highlight-indentation-face "RoyalBlue3")
                                        ;(set-face-background 'highlight-indentation-current-column-face "RoyalBlue3")

                                        ;(add-hook 'python-mode-hook 'minimap-mode)

(add-hook 'before-save-hook 'delete-trailing-whitespace)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; PYLOOKUP
;;;;;;;; https://taesoo.org/proj/pylookup.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; add pylookup to your loadpath, ex) "~/.lisp/addons/pylookup"
;;(setq pylookup-dir (concat marcel-lisp-dir "/el-get/pylookup"))
;;(add-to-list 'load-path pylookup-dir)
;; load pylookup when compile time
;;(use-package pylookup)

;; set executable file and db file
;;(setq pylookup-program (concat pylookup-dir "/pylookup.py"))
;;(setq pylookup-db-file (concat pylookup-dir "/pylookup.db"))

;; to speedup, just load it on demand
;;(autoload 'pylookup-lookup "pylookup"
;;  "Lookup SEARCH-TERM in the Python HTML indexes." t)
;;(autoload 'pylookup-update "pylookup"
;;  "Run pylookup-update and create the database at `pylookup-db-file'." t)
;;(global-set-key "\C-ch" 'pylookup-lookup)


(require 'pydoc-info)
(info-lookup-add-help
 :mode 'python-mode
 :parse-rule 'pydoc-info-python-symbol-at-point
 :doc-spec
 '(("(python)Index" pydoc-info-lookup-transform-entry)
   ("(TARGETNAME)Index" pydoc-info-lookup-transform-entry)))


(when (load "flymake" t)
  (defun flymake-pycheckers-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list (expand-file-name "pycheckers.py" marcel-lisp-dir) (list local-file)))))



(when (load "flymake" t)
  (defun flymake-pep8-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "pep8" (list "–repeat" local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pep8-init)))

(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "pyflakes" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))

(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "epylint" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init)))

(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'find-file-hook 'flymake-find-file-hook)


(defadvice flymake-post-syntax-check
    (before flymake-force-check-was-interrupted)
  (setq flymake-check-was-interrupted t))
(ad-activate 'flymake-post-syntax-check)


(when (require 'flycheck nil t)
  (setq elpy-modules (remove 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))



;; https://github.com/jorgenschaefer/elpy/issues/137
;;If you want to use pyflakes you probably don't want pylint or
;;flake8. To disable those checkers, add the following to your
;;init.el:
;;; (when (require 'flycheck-pyflakes nil t)
;; (add-hook 'python-mode-hook 'flycheck-mode)
;; (add-to-list 'flycheck-disabled-checkers 'python-flake8)
;; (add-to-list 'flycheck-disabled-checkers 'python-pylint)
;; )

;; (setq python-environment-directory
;;       (concat marcel-lisp-dir
;;               (cond
;;                (running-linux
;;                 "/.python-environments/linux/")
;;                (running-macos
;;                 "/.python-environments/mac")
;;                )))

;; (defun my-flymake-show-help ()
;;   (when (get-char-property (point) 'flymake-overlay)
;;     (let ((help (get-char-property (point) 'help-echo)))
;;       (if help (message "%s" help)))))

;; ;; Hook up flymake to use the PEP8 style
;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (unless (eq buffer-file-name nil) (flycheck-mode 1))
;;             (local-set-key [f2] 'flycheck-previous-error)
;;             (local-set-key [f3] 'flycheck-next-error)))

;; ;; Custom Keybinds
;; (add-hook 'python-mode-hook
;;         (lambda ()
;;                 (local-set-key (kbd "C-c p") 'pylint)
;;                 (local-set-key (kbd "C-c o") 'pep8)
;;                 (local-set-key (kbd "C-c i") 'ipython)
;;                 (local-set-key (kbd "C-c f") 'my-python-toggle-fold)
;;                 (fci-mode 1)
;;                 ))
