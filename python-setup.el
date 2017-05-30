
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PYTHON
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; ;; now set our own packages
(setq
 my:el-python-packages
 '(
   anything
   anything-ipython
   python-mode
   python-mode-utils
   jedi
   python-pep8
   pymacs
   highlight-indentation
   rope
   ropemacs
   pydoc-info
   pylookup
   py-autopep8
   ;;ein
   ;;smartrep
   json
   ))

(el-get 'sync my:el-python-packages)

(setq ropemacs-enable-shortcuts nil)
(setq ropemacs-local-prefix "C-c /")
(setq ropemacs-global-prefix "C-c C-p")

(when (featurep 'python) (unload-feature 'python t))
;;(require 'python)
(add-to-list 'load-path (concat marcel-lisp-dir "/el-get/python-mode/"))
(setq py-install-directory (concat marcel-lisp-dir "/el-get/python-mode/"))
(require 'python-mode)


;; Start PEP8
(require 'tramp)
(require 'compile)
(require 'jedi)
(require 'pydoc-info)
(info-lookup-add-help
   :mode 'python-mode
   :parse-rule 'pydoc-info-python-symbol-at-point
   :doc-spec
   '(("(python)Index" pydoc-info-lookup-transform-entry)
     ("(TARGETNAME)Index" pydoc-info-lookup-transform-entry)))


(autoload 'python-pep8 "python-pep8")
(autoload 'pep8 "python-pep8")
(load "python-pep8")
(require 'auto-complete)

(require 'yasnippet)
(yas-global-mode 1)
(yas-reload-all)
(setq yas/trigger-key (kbd "C-c <kp-multiply>"))
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "C-<tab>") 'yas-expand)


;(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

(require 'highlight-indentation)
(add-hook 'python-mode-hook 'highlight-indentation-mode)
(add-hook 'python-mode-hook 'highlight-indentation-current-column-mode)
(set-face-background 'highlight-indentation-face "RoyalBlue3")
;(set-face-background 'highlight-indentation-current-column-face "RoyalBlue3")

;(add-hook 'python-mode-hook 'minimap-mode)


(defun my-python-toggle-fold ()
  "Toggle fold all lines larger than indentation on current line"
  (interactive)
  (let ((col 1))
    (save-excursion
      (back-to-indentation)
      (setq col (+ 1 (current-column)))
      (set-selective-display
       (if selective-display nil (or col 1))))))



(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "pep8" (list "–repeat" local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init)))

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

(add-hook 'find-file-hook 'flymake-find-file-hook)


(defun my-flymake-show-help ()
  (when (get-char-property (point) 'flymake-overlay)
    (let ((help (get-char-property (point) 'help-echo)))
      (if help (message "%s" help)))))



;; Start autocomplete and jedi for refactoring
(add-hook 'python-mode-hook 'auto-complete-mode)

;(add-hook 'python-mode-hook 'jedi:ac-setup)


(setq python-environment-directory
      (concat marcel-lisp-dir
              (cond
               (running-linux
                "/.python-environments/linux/")
               (running-macos
                "/.python-environments/mac")
                )))


;; Hook up flymake to use the PEP8 style
(add-hook 'python-mode-hook
          (lambda ()
            (unless (eq buffer-file-name nil) (flycheck-mode 1))
            (local-set-key [f2] 'flycheck-previous-error)
            (local-set-key [f3] 'flycheck-next-error)))

;; Custom Keybinds
(add-hook 'python-mode-hook
        (lambda ()
                (local-set-key (kbd "C-c p") 'pylint)
                (local-set-key (kbd "C-c o") 'pep8)
                (local-set-key (kbd "C-c i") 'ipython)
                (local-set-key (kbd "C-c f") 'my-python-toggle-fold)
                (fci-mode 1)
                ))

;; IPython command line args
;; use –colors=LightBG if you have a white background
;; –pylab automatically imports all pylab functions
;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (setq py-python-command-args (quote ("–-colors" "Linux" "-i" "–-pylab" "--matplotlib")))))

;; (setq py-python-command-args '("--matplotlib" "--colors" "LightBG"))

(add-hook 'python-mode-hook
          (lambda ()
            (jedi:setup)
            (jedi:ac-setup)
            (local-set-key "\C-cd" 'jedi:show-doc)
            (local-set-key (kbd "s-SPC") 'jedi:complete)
            (local-set-key (kbd "M-.") 'jedi:goto-definition)
            ))

(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
(setq py-electric-colon-active t)


(add-hook 'post-command-hook 'my-flymake-show-help)
;; Start autopair to complete brackets and quotes
(add-hook 'python-mode-hook 'autopair-mode)

;(defvar py-mode-map python-mode-map)
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "--colors Linux --pylab  --matplotlib"
      python-shell-prompt-regexp "In \\[[0-9]+\\]: " ;; "In \: "
      python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: " ;; "Out\: "
      python-shell-completion-setup-code
      "from IPython.core.completerlib import module_completion"
      python-shell-icompletion-setup-code
      "from IPython.core.completerlib import module_completion"
      python-shell-completion-module-string-code
      "';'.join(module_completion('''%s'''))"
      python-shell-completion-string-code
      "';'.join(get_ipython().Completer.all_completions('''%s'''))")

(setq py-shell-name "ipython")

;; Pylint options
;; Defaults don't allow single letter variable names like
;; A for a matrix or x for a vector
(setq pylint-options "–function-rgx='[a-z_][A-Za-z0-9_]{2,30}' –argument-rgx='[A-Za-z_[A-Za-z0-9_]{1,30}' –output-format=parseable")

;; Initialize Pymacs
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
;; Initialize Rope
(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-autoimport t)
(setq ropemacs-codeassist-maxfixes 3)



(defun rope-before-save-actions ()
  ;; Does nothing but save us from an error.
  )
(defun rope-after-save-actions ()
  ;; Does nothing but save us from an error.
  )
(defun rope-exiting-actions ()
  ;; Does nothing but save us from an error.
  )



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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Auto-completion
;;;  Integrates:
;;;   1) Rope
;;;   2) Yasnippet
;;;   all with AutoComplete.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun prefix-list-elements (list prefix)
  (let (value)
    (nreverse
     (dolist (element list value)
      (setq value (cons (format "%s%s" prefix element) value))))))

(defvar ac-source-rope
  '((candidates
     . (lambda ()
         (prefix-list-elements (rope-completions) ac-target))))
  "Source for Rope")

(defun ac-python-find ()
  "Python `ac-find-function'."
  (require 'thingatpt)
  (let ((symbol (car-safe (bounds-of-thing-at-point 'symbol))))
    (if (null symbol)
        (if (string= "." (buffer-substring (- (point) 1) (point)))
            (point)
          nil)
      symbol)))

(defun ac-python-candidate ()
  "Python `ac-candidates-function'"
  (let (candidates)
    (dolist (source ac-sources)
      (if (symbolp source)
          (setq source (symbol-value source)))
      (let* ((ac-limit (or (cdr-safe (assq 'limit source)) ac-limit))
             (requires (cdr-safe (assq 'requires source)))
             cand)
        (if (or (null requires)
                (>= (length ac-target) requires))
            (setq cand
                  (delq nil
                        (mapcar (lambda (candidate)
                                  (propertize candidate 'source source))
                                (funcall (cdr (assq 'candidates source)))))))
        (if (and (> ac-limit 1)
                 (> (length cand) ac-limit))
            (setcdr (nthcdr (1- ac-limit) cand) nil))
        (setq candidates (append candidates cand))))
    (delete-dups candidates)))

(add-hook 'python-mode-hook
          (lambda ()
            (auto-complete-mode 1)

            (set (make-local-variable 'ac-sources)
                 (append ac-sources '(ac-source-rope) '(ac-source-yasnippet)))
            (set (make-local-variable 'ac-find-function) 'ac-python-find)
            (set (make-local-variable 'ac-candidate-function) 'ac-python-candidate)
            (set (make-local-variable 'ac-auto-start) 1)))

;;Ryan's python specific tab completion
(defun mb-python-tab ()
  ;; Try the following:
  ;; 1) Do a yasnippet expansion
  ;; 2) Do a Rope code completion
  ;; 3) Do an indent
  (interactive)
  (if (eql (ac-start) 0)
      (indent-for-tab-command)))

(defadvice ac-start (before advice-turn-on-auto-start activate)
  (set (make-local-variable 'ac-auto-start) t))

(defadvice ac-cleanup (after advice-turn-off-auto-start activate)
  (set (make-local-variable 'ac-auto-start) nil))

;;(define-key python-mode-map "\t" 'my-python-tab)


;;(require 'ein)

(add-hook 'find-file-hooks 'my-load-python-template)

(defun my-load-python-template ()
  (interactive)
  (when (and
         (string-match "\\.py$" (buffer-file-name))
         (eq 1 (point-max)))
    (insert-file (concat marcel-lisp-dir  "/template.py"))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;; macros
(defmacro after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,mode
     '(progn ,@body)))

;;;; auto-complete
(after 'auto-complete
  (setq ac-auto-show-menu .1)
  (setq ac-use-menu-map t)
;  (setq ac-disable-inline t)
  (setq ac-candidate-menu-min 0)
  (add-to-list 'ac-dictionary-directories (concat marcel-lisp-dir "/dict")))


(after 'auto-complete-config
  (ac-config-default)
  ;;(add-hook 'ein:notebook-multilang-mode-hook 'auto-complete-mode)
  (setq-default ac-sources (append '(ac-source-yasnippet ac-source-imenu) ac-sources))
  (when (file-exists-p (concat marcel-lisp-dir "/el-get/pymacs"))
    (add-to-list 'load-path (concat marcel-lisp-dir "/el-get/pymacs"))
    (setq ropemacs-enable-autoimport t)
    (ac-ropemacs-initialize)
    (ac-ropemacs-setup)))

(after "auto-complete-autoloads"
  (require 'auto-complete-config))

;;;; python

;; (defun python-config-python ()
;;   "Configure python.el to defaults of using python."
;;   (interactive)
;;   (setq python-shell-virtualenv-path "venv"
;;         python-shell-interpreter "python"
;;         python-shell-prompt-regexp ">>> "
;;         python-shell-prompt-output-regexp ""
;;         ;; python-shell-setup-codes '(python-shell-completion-setup-code python-ffap-setup-code python-eldoc-setup-code)
;;         python-shell-completion-module-string-code ""
;;         python-shell-completion-string-code "';'.join(__COMPLETER_all_completions('''%s'''))
;; "))

;; (defun python-config-ipython ()
;;   "Configure python.el to handle ipython."
;;   (interactive)
;;   ;; (add-to-list 'python-shell-setup-codes 'python-shell-icompletion-setup-code)
;;   (setq python-shell-interpreter "ipython"
;;         python-shell-interpreter-args ""
;;         python-shell-prompt-regexp "In \\[[0-9]+\\]: "
;;         python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
;;         ;; python-shell-setup-codes '(python-shell-icompletion-setup-code python-ffap-setup-code python-eldoc-setup-code)
;;         python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
;;         python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))

;; (setq python-shell-icompletion-setup-code "from IPython.core.completerlib import module_completion")
;; (setq python-shell-virtualenv-path "~/.virtualenv/default")

;;(after 'python (python-config-ipython))

;; (setq ein:use-smartrep nil
;;       ein:use-auto-complete t
;;       ein:complete-on-dot t
;;       ein:notebook-console-executable (expand-file-name "~/.virtualenv/default/bin/ipython")
;;       ein:notebook-console-security-dir (expand-file-name "~/.ipython/profile_default/security"))


(defun python-modes-init ()
  "initialization for all python modes"
  (interactive)
  (make-face 'font-lock-statement-face)
  (set-face-attribute
   'font-lock-statement-face nil :inherit font-lock-variable-name-face)
  (setq font-lock-statement-face 'font-lock-statement-face)
  (font-lock-add-keywords
   'python-mode
   `((,(rx symbol-start (or "def" "class" "return" "as" "try" "except" "raise") symbol-end)
      0 font-lock-statement-face)))
  (font-lock-add-keywords 'python-mode
                          `((,(rx symbol-start (or "import" "from")
                                  symbol-end) 0 font-lock-preprocessor-face)))
  (make-face 'font-lock-operator-face)
  (set-face-attribute
   'font-lock-operator-face nil :inherit font-lock-keyword-face)
  (setq font-lock-operator-face 'font-lock-operator-face)
  (font-lock-add-keywords
   'python-mode
   `((,(rx symbol-start (or "in" "and" "or" "is" "not") symbol-end)
      0 font-lock-operator-face)))
  (font-lock-add-keywords
   'python-mode
   `(("^[       ]*\\(@\\)\\([a-zA-Z_][a-zA-Z_0-9.]+\\)\\((.+)\\)?"
      (1 'font-lock-keyword-face)
      (2 'font-lock-function-name-face)))))




(require 'comint)
(define-key comint-mode-map (kbd "M-") 'comint-next-input)
(define-key comint-mode-map (kbd "M-") 'comint-previous-input)
(define-key comint-mode-map [down] 'comint-next-matching-input-from-input)
(define-key comint-mode-map [up] 'comint-previous-matching-input-from-input)

;(require 'anything)
;(require 'anything-ipython)
;(when (require 'anything-show-completion nil t)
;  (use-anything-show-completion 'anything-ipython-complete '(length initial-pattern)))


;; (add-hook 'comint-mode-hook
;;           (lambda ()
;;             (face-remap-add-relative 'hl-line '((:background "gray90" ) hl-line))
;;             (face-remap-add-relative 'comint-highlight-prompt '((:foreground "green4") comint-highlight-prompt))
;;             (face-remap-add-relative 'comint-highlight-output '((:foreground "red4") comint-highlight-output))
;;             (face-remap-add-relative 'default '((:background "white" :foreground "black") default))))


;(copy-face 'default 'comint-highlight-prompt)
;; (copy-face 'default 'comint-highlight-output)
;; (set-face-foreground 'comint-highlight-prompt "green4")
;; (set-face-foreground 'comint-highlight-output "green1")


(require 'color-theme-buffer-local)
   (add-hook 'python-mode 
     (lambda nil (color-theme-buffer-local 'color-theme-cobalt (current-buffer))))

(provide 'python-setup)
;;; python-setup ends here
