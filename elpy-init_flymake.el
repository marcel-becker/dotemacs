(defvar company-idle-delay 0)

(setq my:el-python-packages
 '(
   elpy
   anything
   anything-ipython
   jedi
   python-pep8
   pymacs
   highlight-indentation
   pydoc-info
   pylookup
   py-autopep8
   ;;ein
   ;;smartrep
   json
   ))

(el-get 'sync my:el-python-packages)


(el-get 'sync '(elpy))
(elpy-enable)
(elpy-use-ipython)
;;(elpy-clean-modeline)

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "--colors Linux --pylab  --matplotlib"
      python-shell-prompt-regexp "In \\[[0-9]+\\]: " ;; "In \: "
      python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: " ;; "Out\: "
      python-shell-completion-setup-code
      "from IPython.core.completerlib import module_completion"
      python-shell-completion-module-string-code
      "';'.join(module_completion('''%s'''))"
      python-shell-completion-string-code
      "';'.join(get_ipython().Completer.all_completions('''%s'''))")

;; Fixing a key binding bug in elpy
(define-key yas-minor-mode-map (kbd "C-c k") 'yas-expand)
;; Fixing another key binding bug in iedit mode
(define-key global-map (kbd "C-c i") 'iedit-mode)
;; Custom Keybinds
(add-hook 'python-mode-hook
        (lambda ()
                (local-set-key (kbd "C-c p") 'pylint)
                (local-set-key (kbd "C-c o") 'pep8)
                (local-set-key (kbd "C-c y") 'ipython)
                (fci-mode 1)
                ))

(setq company-selection-wrap-around t
      company-show-numbers t
      company-tooltip-flip-when-above t)


;;(setq elpy-rpc-backend "rope")

(require 'color)
(let ((bg (face-attribute 'default :background)))
  (custom-set-faces
   `(company-tooltip ((t (:inherit default :background "skyblue"))))
   `(company-scrollbar-bg ((t (:background "green"))));;,(color-lighten-name bg 10)))))
   `(company-scrollbar-fg ((t (:background "green"))));;,(color-lighten-name bg 5)))))
   `(company-tooltip-selection ((t (:inherit font-lock-function-name-face :background "black" :foreground "yellow"))))
   `(company-tooltip-common ((t (:inherit font-lock-constant-face :background "black"))))))


;; Start PEP8
(require 'tramp)
(require 'compile)
(require 'jedi)
(autoload 'python-pep8 "python-pep8")
(autoload 'pep8 "python-pep8")
(load "python-pep8")
(setq flycheck-flake8-maximum-line-length 120)

(require 'highlight-indentation)
(add-hook 'python-mode-hook 'highlight-indentation-mode)
(add-hook 'python-mode-hook 'highlight-indentation-current-column-mode)
(set-face-background 'highlight-indentation-face "RoyalBlue3")
;(set-face-background 'highlight-indentation-current-column-face "RoyalBlue3")

;(add-hook 'python-mode-hook 'minimap-mode)

(add-hook 'before-save-hook 'delete-trailing-whitespace)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; PYLOOKUP
;;;;;;;; https://taesoo.org/proj/pylookup.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; add pylookup to your loadpath, ex) "~/.lisp/addons/pylookup"
(setq pylookup-dir (concat marcel-lisp-dir "/el-get/pylookup"))
(add-to-list 'load-path pylookup-dir)
;; load pylookup when compile time
(require 'pylookup)

;; set executable file and db file
(setq pylookup-program (concat pylookup-dir "/pylookup.py"))
(setq pylookup-db-file (concat pylookup-dir "/pylookup.db"))

;; to speedup, just load it on demand
(autoload 'pylookup-lookup "pylookup"
  "Lookup SEARCH-TERM in the Python HTML indexes." t)
(autoload 'pylookup-update "pylookup"
  "Run pylookup-update and create the database at `pylookup-db-file'." t)
(global-set-key "\C-ch" 'pylookup-lookup)


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
      (list (concat marcel-lisp-dir "/pycheckers.py") (list local-file)))))



;; (when (load "flymake" t)
;;   (defun flymake-pylint-init ()
;;     (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                        'flymake-create-temp-inplace))
;;            (local-file (file-relative-name
;;                         temp-file
;;                         (file-name-directory buffer-file-name))))
;;       (list "pep8" (list "–repeat" local-file))))
;;   (add-to-list 'flymake-allowed-file-name-masks
;;                '("\\.py\\'" flymake-pylint-init)))

;; (when (load "flymake" t)
;;    (defun flymake-pyflakes-init ()
;;            (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                               'flymake-create-temp-inplace))
;;                   (local-file (file-relative-name
;;                            temp-file
;;                            (file-name-directory buffer-file-name))))
;;              (list "pyflakes" (list local-file))))
;;          (add-to-list 'flymake-allowed-file-name-masks
;;                       '("\\.py\\'" flymake-pyflakes-init)))

;; (add-hook 'find-file-hook 'flymake-find-file-hook)


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
