;;; Time-stamp: "2016-09-08 Thu 16:21 marcelbecker on beckermac.local"
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; set the load path
;;;
;;; Add the code below to your ~/.emacs.d/init.el to load
;;; the shared version of the init file.
;;;
;; (defvar marcel-lisp-dir
;; (if (eq system-type 'windows-nt)      ; Windows
;;     (cond ((file-exists-p "C:/Dropbox/.emacs.d")
;;            (setenv "HOME" "C:/Dropbox")
;;           "C:/Dropbox/.emacs.d")
;;           ((file-exists-p "D:/Dropbox/.emacs.d")
;;            (setenv "HOME" "D:/Dropbox")
;;            "D:/Dropbox/.emacs.d")
;;           (t
;;            (expand-file-name "~/.emacs.d")))
;;   (cond ((file-exists-p  "~/Dropbox/.emacs.d")
;;          "~/Dropbox/.emacs.d")
;;         (t
;;          (expand-file-name "~/.emacs.d"))))
;; "Address of Marcel's lisp libraries.")

;; (setq user-init-file (expand-file-name "init.el" marcel-lisp-dir))
;; (if (not (eq user-init-file (expand-file-name "~/.emacs.d")))
;;     (load-file user-init-file))

(message  (concat "Loading " load-file-name))

;; Setting the running environment
(defvar running-ms-windows
  (eq system-type 'windows-nt))

(defvar running-macos
  (eq system-type 'darwin))

(defvar running-linux
  (eq system-type 'gnu/linux))


;; key bindings
(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-allow-anti-aliasing t)
  ;;(require 'redo+)
  ;;(require 'mac-key-mode)
  ;;(mac-key-mode 1)
  (setq mac-option-modifier 'alt)
  (setq mac-right-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char)) ;; sets fn-delete to be right-delete


;; add everything under ~/.emacs.d to it
(unless (boundp 'marcel-lisp-dir)
  (defvar marcel-lisp-dir
    (if running-ms-windows ; Windows
        (cond ((file-exists-p "C:/Dropbox/.emacs.d")
               (setenv "HOME" "C:/Dropbox")
               "C:/Dropbox/.emacs.d")
              ((file-exists-p "D:/Dropbox/.emacs.d")
               (setenv "HOME" "D:/Dropbox")
               "D:/Dropbox/.emacs.d")
              (t
               (expand-file-name "~/.emacs.d")))
      (cond ((file-exists-p  "~/Dropbox/.emacs.d")
             "~/Dropbox/.emacs.d")
            (t
             (expand-file-name "~/.emacs.d"))))
    "Address of Marcel's lisp libraries."))

;;(load-file (expand-file-name "~/.spacemacs.d/init.el"))





(setq user-emacs-directory marcel-lisp-dir)

;;; ADD Marcel'S LISP LIBRARY TO `load-path'.
(add-to-list 'load-path  (concat marcel-lisp-dir "/"))
(add-to-list 'load-path  (concat marcel-lisp-dir "/el-get/use-package/"))



(defun reduce-hostname (name suffixes)
  (if suffixes
      (reduce-hostname
       (substring name 0 (string-match (car suffixes) name))
       (cdr suffixes))
    name))

(defconst machine-nickname
  (reduce-hostname (system-name) (list "\\.kestrel\\.edu" "\\.CMU\\.EDU" "\\.CS" "\\.MACH" "\\.SOAR" "\\.CIMDS" "\\.RI" "\\.local")))



;;   LOGNAME and USER are expected in many Emacs packages
;;   Check these environment variables.
(if (and (null (getenv "USER"))
         ;; Windows includes variable USERNAME, which is copied to
         ;; LOGNAME and USER respectively.
         (getenv "USERNAME"))
    (setenv "USER" (getenv "USERNAME")))

(if (and (getenv "LOGNAME")
         ;;  Bash shell defines only LOGNAME
         (null (getenv "USER")))
    (setenv "USER" (getenv "LOGNAME")))

(if (and (getenv "USER")
         (null (getenv "LOGNAME")))
    (setenv "LOGNAME" (getenv "USER")))

(if (null (getenv "SPECWARE4"))
    (setenv "SPECWARE4"
            (if running-ms-windows
                "c:/src/Specware"
              (expand-file-name "~/src/specware"))))


(defvar missing-packages-list nil
  "List of packages that `try-require' can't find.")

;; attempt to load a feature/library, failing silently
(defun try-require (feature)
  "Attempt to load a library or module. Return true if the
library given as argument is successfully loaded. If not, instead
of an error, just add the package to a list of missing packages."
  (condition-case err
      ;; protected form
      (progn
        (message "Checking for library `%s'..." feature)
        (if (stringp feature)
            (load-library feature)
          (require feature))
        (message "Checking for library `%s'... Found" feature))
    ;; error handler
    (file-error  ; condition
     (progn
       (message "Checking for library `%s'... Missing" feature)
       (add-to-list 'missing-packages-list feature 'append))
     nil)))


(set-frame-font
 (cond (running-ms-windows
        "DejaVu Sans Mono 11")
       (running-macos
        ;;"Source Code Pro 16")
        "DejaVu Sans Mono 18")
       ;;        "Geneva 13")
       ((not running-macos)
        "DejaVu Sans Mono 13")))

;;; Nice size for the default window
(defun get-default-height ()
  (min 60 (/ (- (display-pixel-height) 200) (frame-char-height))))

(defun get-default-x-frame-position ()
  (- (/ (display-pixel-width) 2) 400))

(defun get-default-y-frame-position ()
  (- (/ (display-pixel-height) 2) (/ (get-default-height) 2)))

(setq default-frame-alist
      '((cursor-color . "white")
        (mouse-color . "white")
        (foreground-color . "white")
        (cursor-type . box)
        (tool-bar-lines . 0)
        ;;(top . 50)
        ;;(left . 50)
        (width . 130)
        ))

(add-to-list 'default-frame-alist (cons 'height (get-default-height)))
(add-to-list 'default-frame-alist
             (if (eq (user-uid) 0)
                 '(background-color . "gray38")
               '(background-color . "#09223F")
               ;;'(background-color . "DodgerBlue4")
               ))


;; Set Frame width/height
(defun arrange-frame (w h x y)
  "Set the width, height, and x/y position of the current frame"
  (let ((frame (selected-frame)))
    (delete-other-windows)
    (set-frame-position frame x y)
    (set-frame-size frame w h)))



(arrange-frame 130 (get-default-height) 500 100)
;;(arrange-frame 130 (get-default-height) (get-default-x-frame-position) (get-default-y-frame-position))

;; Needed to run Specware on Emacs24
(setenv "SPECWARE_INIT_FORM" "NIL")


;; open my Emacs init file
(defun my-open-dot-emacs ()
  "Opening `~/.emacs.d/init.el'"
  (interactive)
  (find-file (concat marcel-lisp-dir "/init.el")))
(global-set-key (kbd "<S-f3>") 'my-open-dot-emacs)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; EL-GET
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To update el-get packages manually

(let ((elget-lib (concat marcel-lisp-dir "/el-get/el-get")))
  (if (file-exists-p elget-lib)
      (add-to-list 'load-path elget-lib)))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path (concat marcel-lisp-dir "/el-get/el-get/recipes"))
(setq el-get-default-process-sync t
      el-get-verbose t)


(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ;;("melpas" . "http://stable.melpa.org/packages/")
                         ))

(setq package-check-signature nil)
(setq package-user-dir (concat marcel-lisp-dir "/elpa"))
(add-to-list 'load-path (concat marcel-lisp-dir "/elpa"))
;;(package-initialize)

;;(when (not package-archive-contents)
;;  (package-refresh-contents))

(require 'el-get-elpa)
;;(el-get-emacswiki-build-local-recipes)
;;(el-get-elpa-build-local-recipes)


;; set local recipes
;; (setq
;;  el-get-sources
;;  '((:name buffer-move			; have to add your own keys
;;    :after (progn
;;         (global-set-key (kbd "<C-S-up>")     'buf-move-up)
;;         (global-set-key (kbd "<C-S-down>")   'buf-move-down)
;;         (global-set-key (kbd "<C-S-left>")   'buf-move-left)
;;         (global-set-key (kbd "<C-S-right>")  'buf-move-right)))

;;    (:name smex				; a better (ido like) M-x
;;    :after (progn
;;         (setq smex-save-file "~/.emacs.d/.smex-items")
;;         (global-set-key (kbd "M-x") 'smex)
;;         (global-set-key (kbd "M-X") 'smex-major-mode-commands)))

;;    (:name magit				; git meet emacs, and a binding
;;    :after (progn
;;         (global-set-key (kbd "C-x C-z") 'magit-status)))

;;    (:name goto-last-change		; move pointer back to last change
;;    :after (progn
;;         ;; when using AZERTY keyboard, consider C-x C-_
;;         (global-set-key (kbd "C-x C-/") 'goto-last-change)))))

;; ;; ;; now set our own packages
(setq
 my:el-get-packages
 '(
   ;;ac-anything
   ;;ac-anything2
   ac-ispell
   ac-python
   ace-link
   ace-window
   adaptive-wrap
   aggressive-indent
   ample-regexps
   anaconda-mode
   anything
   anything-ipython
   anzu
   async
   auctex
   auto-complete
   auto-dictionary
   auto-highlight-symbol
   auto-yasnippet
;;   autopair
   autopep8
   avy
   bind-key
   bind-map
   bookmark+
   buffer-move
   cl-lib
   clean-aindent-mode
   color-theme
   color-theme-tango
   company
   company-anaconda
   company-jedi
   company-mode
   company-quickhelp
   company-statistics
   ctable
   cython-mode
   dash
   deferred
   define-word
   diff+
   diminish
   dockerfile-mode
   el-get
   elisp-slime-nav
   elpy
   epc
   epl
   escreen
   eval-sexp-fu
   evil
   evil-anzu
   evil-args
   evil-escape
   evil-exchange
   evil-iedit-state
   evil-indent-textobject
   evil-jumper
   evil-leader
   evil-lisp-state
   evil-matchit
   evil-nerd-commenter
   evil-numbers
   evil-search-highlight-persist
   evil-surround
   evil-tutor
   evil-visualstar
   exec-path-from-shell
   expand-region
   f
   fancy-battery
   ffap-
   file-template
   fill-column-indicator
   find-file-in-project
   flx
   flx-ido
   flycheck
   flymake
   frame-cmds
   frame-fns
   fuzzy
   gh-md
   git-commit
   git-messenger
   git-modes
   git-timemachine
   gitattributes-mode
   gitconfig-mode
   gitignore-mode
   golden-ratio
   google-translate
   goto-chg
   goto-last-change
   header2
   ;; helm
   ;; helm-ag
   ;; helm-anything
   ;; helm-c-yasnippet
   ;; helm-core
   ;; helm-descbinds
   ;; helm-gitignore
   ;; helm-make
   ;; helm-mode-manager
   ;; helm-projectile
   ;; helm-pydoc
   ;; helm-swoop
   ;; helm-themes
   ;; helm-spotify
   highlight
   highlight-indentation
   highlight-numbers
   highlight-parentheses
   hungry-delete
   hy-mode
   hydra
   icicles
   ido-vertical-mode
   idomenu
   iedit
   indent-guide
   info+
   jedi
   jedi-core
   json
   json-mode
   json-reformat
   json-rpc
   json-snatcher
   let-alist
   leuven-theme
   linum-relative
   lua-mode
   macrostep
   magit
   magit-gitflow
   magit-popup
   markdown-mode
   markdown-toc
   menu-bar+
   move-text
   neotree
   nginx-mode
   nose
   open-junk-file
   ;;package
   package-build
   page-break-lines
   paradox
   parent-mode
   pcre2el
   pip-requirements
   pkg-info
   popup
   popwin
   pos-tip
   powerline
   projectile
   py-autopep8
   pycomplete+
   pydoc
   pydoc-info
   pyenv-mode
   pylookup
   pytest
   python-environment
   python-mode
   python-pep8
   pythonic
   pyvenv
   quelpa
   rainbow-delimiters
   rainbow-mode
   recentf-buffer
   recentf-ext
   request
   s
   seq
   shell-command
   smartparens
   smartrep
   smeargle
   smex
   smooth-scroll
   smooth-scrolling
   spacemacs-theme
   spinner
   spotify
   spray
   sr-speedbar
   swiper
   switch-window
   tabbar
   tabbar-extension
   unbound
   undo-tree
   use-package
   vi-tilde-fringe
   virtualenvwrapper
   vline
   volatile-highlights
   websocket
   which-key
   window-number
   window-numbering
   with-editor
   yasnippet
   yasnippet-snippets
   yasnippets
   zencoding-mode

   hexrgb
   ;;   one-key
   ;;   one-key-dir
   ;;   one-key-yas
   ;;   one-key-bmkp
   ))

;; Install new packages and init already installed packages
(message  "Initializing el-get packages")
(el-get 'sync my:el-get-packages)
(message  "Initializing elpa packages")
(package-initialize)


(eval-when-compile
  (require 'use-package))
(require 'diminish)       ;; if you use :diminish
(require 'bind-key)               ;; if you use any :bind variant


(setq use-package-always-ensure t)

;; (use-package helm :ensure t)
;; (use-package    helm-ag :ensure t)
;; (use-package    helm-anything :ensure t)
;; (use-package    helm-c-yasnippet :ensure t)
;; (use-package    helm-descbinds :ensure t)
;; (use-package    helm-gitignore :ensure t)
;; (use-package    helm-make :ensure t)
;; (use-package    helm-mode-manager :ensure t)
;; (use-package    helm-projectile :ensure t)
;; (use-package    helm-pydoc :ensure t)
;; (use-package    helm-swoop :ensure t)
;; (use-package    helm-themes :ensure t)
;; (use-package    helm-package :ensure t)
;; (use-package    helm-ls-git :ensure t)
;; (use-package    helm-git-files :ensure t)
;; (use-package    helm-helm-commands :ensure t)

;; (let ((subl-lib (concat marcel-lisp-dir "/sublimity")))
;;    (if (file-exists-p subl-lib)
;;        (add-to-list 'load-path subl-lib)))
;; (require 'sublimity)
;; (require 'sublimity-scroll)
;; (require 'sublimity-map)
;;(require 'sublimity-attractive)


;;
;; Some recipes require extra tools to be installed
;;
;; Note: el-get-install requires git, so we know we have at least that.
;;
;;(when (el-get-executable-find "cvs")
;;  (add-to-list 'my:el-get-packages 'emacs-goodies-el)) ; the debian addons for emacs

;; (when (el-get-executable-find "svn")
;;   (loop for p in '(psvn          ; M-x svn-status
;;                    yasnippet		; powerful snippet mode
;;         )
;;  do (add-to-list 'my:el-get-packages p)))

;; (setq my:el-get-packages
;;       (append
;;        my:el-get-packages
;;        (loop for src in el-get-sources collect (el-get-source-name src))))




;; (let* ((ecb-library
;;         (concat marcel-lisp-dir "/ecb-cvs/ecb")))
;;   (when (file-exists-p ecb-library)
;;     (add-to-list 'load-path ecb-library)))


(defun my-load-python ()
  (interactive)
  (let* ((python-setup (concat marcel-lisp-dir  "/elpy-init.el"))
         (anaconda-setup (concat marcel-lisp-dir  "/anaconda-init.el")))
    (when (file-exists-p python-setup)
      (load-file python-setup))
    ;;(when (file-exists-p anaconda-setup)
    ;;(load-file anaconda-setup))
    ))

(add-hook 'python-mode-hook (function my-load-python))

(defun my-load-slime ()
  (interactive)
  (let* ((slime-library (concat marcel-lisp-dir  "/slime/")))
    (when (file-exists-p slime-library)
      (add-to-list 'load-path slime-library)
      (require 'slime-autoloads)
      (eval-after-load "slime"
        '(progn
           (add-to-list 'load-path (concat marcel-lisp-dir "/slime/contrib"))
                                        ;(slime-setup '(slime-fancy slime-banner slime-repl slime-autodoc  slime-typeout-frame))
           (slime-setup '(slime-repl))
           (setq slime-complete-symbol*-fancy t)
           (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
           (global-set-key "\C-cs" 'slime-selector)
           (setq inferior-lisp-program ; your Lisp system
                 (if running-ms-windows "sbcl.exe --noinform" "/usr/local/bin/sbcl --noinform"))
           )))))



;; (let ((elib-lib (concat marcel-lisp-dir "/elib-1.0/")))
;;   (if (file-exists-p elib-lib)
;;       (add-to-list 'load-path elib-lib)))

;; ;(let ((jde-lib (concat marcel-lisp-dir "/jdee/lisp/")))
;; ;  (when (file-exists-p jde-lib)
;; ;    (add-to-list 'load-path jde-lib)
;; ;    (require 'jde)))


;; (let ((scala-lib (concat marcel-lisp-dir "/el-get/scala-mode/")))
;;   (when (file-exists-p scala-lib)
;;       (add-to-list 'load-path scala-lib)
;;       (autoload 'scala-mode "scala-mode")
;;       (add-hook 'scala-mode-hook '(lambda () (yas/minor-mode-on)))
;;       (add-hook 'scala-mode-hook '(lambda () (scala-mode-feature-electric-mode)))
;;       (setq yas/my-directory (concat scala-lib "contrib/yasnippet/snippets"))
;;       ;;(yas/load-directory yas/my-directory)
;;       ))

;; ;; Ensime is the emacs enhanced scala mode.
;; (let ((ensime-lib (concat marcel-lisp-dir "/ensime-2.9.2/elisp")))
;;   (when (file-exists-p ensime-lib)
;;       (add-to-list 'load-path ensime-lib)
;;       (require 'ensime)
;;       (add-hook 'scala-mode-hook 'ensime-scala-mode-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;[Package]


;; Add in your own as you wish:
(defvar my-packages
  '()
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; (let ((vline-lib (concat marcel-lisp-dir "/el-get/vline")))
;;   (when (file-exists-p vline-lib)
;;     (add-to-list 'load-path vline-lib)
;;     (require 'vline)))

(require 'vline)
;;(require 'col-highlight)
;;(require 'tabbar-extension)

(setq recentf-save-file (concat marcel-lisp-dir "/recentf-" machine-nickname))

(require 'recentf)
(require 'recentf-buffer)
(require 'recentf-ext)


(defun recentf-save-list ()
  "Save the recent list.
Load the list from the file specified by `recentf-save-file',
merge the changes of your current session, and save it back to
the file."
  (interactive)
  (let ((instance-list (copy-list recentf-list)))
    (recentf-load-list)
    (recentf-merge-with-default-list instance-list)
    (recentf-write-list-to-file)))

(defun recentf-merge-with-default-list (other-list)
  "Add all items from `other-list' to `recentf-list'."
  (dolist (oitem other-list)
    ;; add-to-list already checks for equal'ity
    (add-to-list 'recentf-list oitem)))

(defun recentf-write-list-to-file ()
  "Write the recent files list to file.
Uses `recentf-list' as the list and `recentf-save-file' as the
file to write to."
  (condition-case error
      (with-temp-buffer
        (erase-buffer)
        (set-buffer-file-coding-system recentf-save-file-coding-system)
        (insert (format recentf-save-file-header (current-time-string)))
        (recentf-dump-variable 'recentf-list recentf-max-saved-items)
        (recentf-dump-variable 'recentf-filter-changer-current)
        (insert "\n \n;;; Local Variables:\n"
                (format ";;; coding: %s\n" recentf-save-file-coding-system)
                ";;; End:\n")
        (write-file (expand-file-name recentf-save-file))
        (when recentf-save-file-modes
          (set-file-modes recentf-save-file recentf-save-file-modes))
        nil)
    (error
     (warn "recentf mode: %s" (error-message-string error)))))


(setq recentf-save-file (concat marcel-lisp-dir "/recentf-" machine-nickname))
(setq recentf-auto-cleanup 'never)
(recentf-mode 1)
(setq recentf-auto-cleanup 'never)
(run-at-time nil (* 20 60) 'recentf-save-list)


(require 'savehist)
(setq savehist-file (concat marcel-lisp-dir "/savehistory-" machine-nickname))
(savehist-mode 1)


;; find convenient unbound keystrokes
(require 'unbound)                  ; `M-x describe-unbound-keys'
(require 'switch-window)
                                        ;(require 'lacarte)

(setq recentf-max-saved-items 100)
(setq recentf-max-menu-items 60)
                                        ;(global-set-key [?\e ?\M-x] 'lacarte-execute-menu-command)
                                        ;(global-set-key (kbd "C-x C-r") 'icicle-recent-file)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

                                        ; save the place in files
(require 'saveplace)
(setq-default save-place t)
(setq save-place-forget-unreadable-files nil)
(setq save-place-file (concat marcel-lisp-dir "/places-" machine-nickname))


;;(load-file (concat marcel-lisp-dir "/ido-init.el"))
;;(load-file (concat marcel-lisp-dir "/helm-init.el"))
;;(load-file (concat marcel-lisp-dir "/icicles-init.el"))


(require 'yasnippet)
(yas-global-mode 1)
(setq yas-snippet-dirs
      (list (concat marcel-lisp-dir "/el-get/yasnippet/snippets")
            (concat marcel-lisp-dir "/el-get/yasnippet-snippets")
            (concat marcel-lisp-dir "/el-get/yasnippets")
            (concat marcel-lisp-dir "/snippets")
            ))
(yas-global-mode 1)

(message "Loading anzu")
(require 'anzu)
(global-anzu-mode +1)

(set-face-attribute 'anzu-mode-line nil
                    :foreground "yellow" :weight 'bold)

(custom-set-variables
 '(anzu-mode-lighter "")
 '(anzu-deactivate-region t)
 '(anzu-search-threshold 1000)
 '(anzu-replace-threshold 50)
 '(anzu-replace-to-string-separator " => "))

(define-key isearch-mode-map [remap isearch-query-replace]  #'anzu-isearch-query-replace)
(define-key isearch-mode-map [remap isearch-query-replace-regexp] #'anzu-isearch-query-replace-regexp)

;; C-x C-j opens dired with the cursor right on the file you're editing
(require 'dired-x)

(setq stack-trace-on-error t)
(setq debug-on-error t)
(setq max-lisp-eval-depth 1000)
(setq inhibit-startup-message t)
;;; Make sure there is a newline at the end of each file!
(setq require-final-newline t)
;;; features you probably don't want to use
(put 'narrow-to-page 'disabled t)
(put 'narrow-to-region 'disabled t)
(put 'eval-expression 'disabled nil)
(put 'downcase-region 'disabled nil)
;;; flash instead of beeping
(setq visual-bell t)
(blink-cursor-mode 0)
(transient-mark-mode t)
(show-paren-mode t)
(setq truncate-lines t)
(line-number-mode 1)
(global-linum-mode 1)
(global-hl-line-mode 1)  ; highlight current line
;; dim the ignored part of the file name
(file-name-shadow-mode 1)

;; minibuffer completion incremental feedback
(icomplete-mode)

;; ignore case when reading a file name completion
(setq read-file-name-completion-ignore-case t)
;; ignore case when reading a buffer name
(setq read-buffer-completion-ignore-case t)
;; do not consider case significant in completion (GNU Emacs default)
(setq completion-ignore-case t)

;; visually indicate buffer boundaries and scrolling
(setq indicate-buffer-boundaries t)

;; highlight trailing whitespaces in all modes
(setq-default show-trailing-whitespace t)
;; Delete trailing whitespace when saving (compliance with PEP8)
;;(add-hook 'before-save-hook 'delete-trailing-whitespace)
;;no extra whitespace after lines
;; http://emacsredux.com/blog/2013/05/16/whitespace-cleanup/
;;'whitespace-cleanup is better than delete-trailing-whitespace
;;(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook 'whitespace-cleanup)


(setq suggest-key-bindings 10)

(require 'auto-complete-config)
(ac-config-default)
(defadvice auto-complete-mode (around disable-auto-complete-for-python)
  (unless (eq major-mode 'python-mode) ad-do-it))

(ad-activate 'auto-complete-mode)

(require 'window-number)
(window-number-mode 1)
(require 'window-numbering)

                                        ;(require 'flycheck)
                                        ;(add-hook 'after-init-hook #'global-flycheck-mode)

;; Line to indicate column limit for program lines
(require 'fill-column-indicator)
(setq fci-rule-column 120)
                                        ;(define-globalized-minor-mode
                                        ;  global-fci-mode fci-mode (lambda () (fci-mode 1)))
                                        ;(global-fci-mode t)


;; which-key is a minor mode for Emacs that displays the key
;; bindings following your currently entered incomplete command (a
;; prefix) in a popup. For example, after enabling the minor mode if
;; you enter C-x and wait for the default of 1 second the minibuffer
;; will expand with all of the available key bindings that follow
;; C-x (or as many as space allows given your settings).
(require 'which-key)
(which-key-mode)


;;smex - A smarter M-x compleation ------------
(if (require 'smex nil 'noerror)
    (progn
      (smex-initialize)
      (global-set-key (kbd "M-x") 'smex)
      (global-set-key (kbd "M-X") 'smex-major-mode-commands)
      (setq smex-save-file (concat marcel-lisp-dir "/smex-items-" machine-nickname)))         ; don't save state to "~/.smex-items"
  (icomplete-mode t));---------------------------------------------



;; don't let the cursor go into minibuffer prompt
(setq minibuffer-prompt-properties (quote (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))

(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

;;(require 'autopair)
;;(autopair-global-mode) ;; to enable in all buffers



(require 'rainbow-delimiters)
(rainbow-delimiters-mode-enable)

;;Rectangular markings-----------------------
;;COOL! C-RET gives rectangular marking for copy/paste, extremely useful
;;for tables. NOTE, second line needed for rectangle, but also gives
;; (transient-mark-mode t) = visualize C-SPC-marking (i.e. highlight)
(setq cua-enable-cua-keys nil) ;;only for rectangle, don't use C-x/c/v for copy/paste
(cua-mode t)                   ;;gives rectangle + same as "(pc-selection-mode)" (=shift+arrow highlights)
;;--------------------------------------------

;; Use the clipboard, pretty please, so that copy/paste "works"
(setq x-select-enable-clipboard t)
;; Navigate windows with M-<arrows>
(windmove-default-keybindings 'meta)
(setq windmove-wrap-around t)

;; winner-mode provides C-<left> to get back to previous window layout
(winner-mode 1)


(setq linum-format " %d ")
;; To make emacs use spaces instead of tabs (Added by Art Lee on 2/19/2008)
(setq-default indent-tabs-mode nil)
(setq mail-default-reply-to "becker@kestrel.edu")
(display-time)
(add-hook 'before-save-hook 'time-stamp)
(setq minibuffer-max-depth nil)

(add-hook 'json-mode-hook
          (lambda ()
            (setq js-indent-level 2)))

;;;Answer y or n instead of yes or no at minibar prompts.
(defalias 'yes-or-no-p 'y-or-n-p)

(defun newline-indents ()
  "Bind Return to `newline-and-indent' in the local keymap."
  (local-set-key "\C-m" 'newline-and-indent))


;; scrolling like gos-emacs
(defun scroll-one-line-up (&optional arg)
  "Scroll the selected window up one line (or N lines)."
  (interactive "p")
  (scroll-up (or arg 1)))

(defun scroll-one-line-down (&optional arg)
  "Scroll the selected window down one line (or N lines)."
  (interactive "p")
  (scroll-down (or arg 1)))

(defun line-to-top-of-window ()
  "Scroll the selected window up so that the current line is at the top."
  (interactive)
  (recenter 0))

;; full screen
(defun fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))


(global-set-key [f6] 'line-to-top-of-window)
(global-set-key [M-f11] 'fullscreen)
(global-set-key [S-f11] 'fullscreen)
(global-set-key [s-f12] 'revert-buffer)
(global-set-key [S-f12] 'revert-buffer)
(global-set-key (kbd "<M-f6>") 'recenter)
(global-set-key (kbd "<M-up>") 'scroll-one-line-up)
(global-set-key (kbd "<M-down>") 'scroll-one-line-down)
(global-set-key (kbd "C-c ;") 'comment-region)
(global-set-key (kbd "M-/") 'comment-region)
(global-set-key (kbd "C-x o") 'switch-window)
(global-set-key (kbd "<s-home>") 'end-of-buffer)
(global-set-key [f7] 'text-scale-increase)
(global-set-key (kbd "M-+") 'text-scale-increase)
(global-set-key [S-f7] 'text-scale-decrease)
(global-set-key (kbd "M--") 'text-scale-decrease)


(message "Loading swiper")
(require 'swiper)
(require 'ivy)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
;;(global-set-key (kbd "C-c C-s") 'swiper)
;; (global-set-key (kbd "C-c C-r") 'ivy-resume)
;; (global-set-key (kbd "M-x") 'counsel-M-x)
;; (global-set-key (kbd "C-x C-f") 'counsel-find-file)
;; (global-set-key (kbd "<f1> f") 'counsel-describe-function)
;; (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
;; (global-set-key (kbd "<f1> l") 'counsel-load-library)
;; (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
;; (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
;; (global-set-key (kbd "C-c g") 'counsel-git)
;; (global-set-key (kbd "C-c j") 'counsel-git-grep)
;; (global-set-key (kbd "C-c k") 'counsel-ag)
;; (global-set-key (kbd "C-x l") 'counsel-locate)
;; (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
;; (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)

;; whenever an external process changes a file underneath emacs, and there
;; was no unsaved changes in the corresponding buffer, just revert its
;; content to reflect what's on-disk.
(global-auto-revert-mode 1)


;;Awesome copy/paste!----------------------
;;My most used hack! If nothing is marked/highlighted, and you copy or cut
;;(C-w or M-w) then use column 1 to end. No need to "C-a C-k" or "C-a C-w" etc.
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))
;;--------------------------------------------



(defun set-case ()
  "Sets case-sensitive search mode"
  (interactive)
  (setq case-fold-search nil)
  (message "Searching is now case-sensitive"))

(defun set-nocase ()
  "Sets case-insensitive search mode"
  (interactive)
  (setq case-fold-search t)
  (message "Searching is now case-insensitive"))


(autoload 'window-number-mode "window-number"
  "A global minor mode that enables selection of windows according to
 numbers with the C-x C-j prefix.  Another mode,
 `window-number-meta-mode' enables the use of the M- prefix."
  t)

(autoload 'window-number-meta-mode "window-number"
  "A global minor mode that enables use of the M- prefix to select
 windows, use `window-number-mode' to display the window numbers in
 the mode-line."
  t)

(window-number-mode 1)
(window-number-meta-mode 1)

;; ----------------------------------------------------------[Window Number]

;; ----------------------------------------------------------[Info Back Button]
(defun add-browser-backspace-key-to-Info-mode ()
  "Add some browser styled nav keys for `Info-mode'.
  The following keys are added:
 【Backspace】 for `Info-history-back'
 【Shift+Backspace】 for `Info-history-forward'."
  (progn
    (local-set-key (kbd "<backspace>") 'Info-history-back)
    (local-set-key (kbd "<S-backspace>") 'Info-history-forward)
    ;; (local-set-key (kbd "<mouse-8>") 'Info-history-back) 5-button mouse. the mouse numbering depends on your OS and mouse. Call “describe-key” then press mouse button to find out
    )
  ;; note: on Linux Firefox, you have to turn on Backspace key for previous page. In the preference.
  )

(add-hook 'Info-mode-hook 'browser-nav-keys)
;; ----------------------------------------------------------[Info Back Button]

(autoload 'c++-mode "cc-mode" "C++ Editing Mode" t)
(autoload 'c-mode "c-mode" "C Editing Mode"   t)
(autoload 'magit-status "magit" nil t)
(autoload 'c++-mode  "cc-mode" "C++ Editing Mode" t)
(autoload 'c-mode    "cc-mode" "C Editing Mode" t)
(autoload 'objc-mode "cc-mode" "Objective-C Editing Mode" t)

                                        ;(require 'python)
                                        ;(autoload 'python-mode "python-mode" "Python editing mode." t)
                                        ;(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; set tab distance to something, so it doesn't change randomly and confuse people
(setq c-basic-offset 4)

;; Tell Emacs to use the function above in certain editing modes.
(add-hook 'lisp-mode-hook             (function newline-indents))
(add-hook 'emacs-lisp-mode-hook       (function newline-indents))
(add-hook 'lisp-interaction-mode-hook (function newline-indents))
(add-hook 'scheme-mode-hook           (function newline-indents))
(add-hook 'c-mode-hook                (function newline-indents))
(add-hook 'c++-mode-hook              (function newline-indents))
(add-hook 'java-mode-hook             (function newline-indents))
(add-hook 'python-mode-hook           (function newline-indents))


;; Text-based modes (including mail, TeX, and LaTeX modes) are auto-filled.
(add-hook 'text-mode-hook (function turn-on-auto-fill))


;; This is how emacs tells the file type by the file suffix.
(setq auto-mode-alist
      (append '(("\\.mss$" . scribe-mode))
              '(("\\.bib$" . bibtex-mode))
              '(("\\.tex$" . latex-mode))
              '(("\\.obj$" . lisp-mode))
              '(("\\.st$"  . smalltalk-mode))
              '(("\\.Z$"   . uncompress-while-visiting))
              '(("\\.cs$"  . indented-text-mode))
              '(("\\.C$"   . c++-mode))
              '(("\\.cc$"  . c++-mode))
              '(("\\.icc$" . c++-mode))
              '(("\\.h$"   . c++-mode))
              '(("\\.c$"   . c-mode))
              '(("\\.y$"   . c-mode))
              '(("\\.py$"  . python-mode))
              '(("\\.sl\\'" . slang-mode))
              '(("\\.sml$" . sml-mode))
              '(("\\.sig$" . sml-mode))
              '(("\\.ML$"  . sml-mode))
              '(("\\.cm\\'" . sml-cm-mode))
              '(("\\.grm\\'" . sml-yacc-mode))
              '(("\\.g\\'" . antlr-mode))
              '(("\\.scala$" . scala-mode))
              auto-mode-alist))

;;  html-mode
(add-hook 'html-mode-hook
          '(lambda ()
             (auto-fill-mode 1)
             (define-key html-mode-map [(<)] 'self-insert-command)
             (define-key html-mode-map [(>)] 'self-insert-command)
             (define-key html-mode-map [(&)] 'self-insert-command)
             (define-key html-mode-map [(control c) (<)] 'html-less-than)
             (define-key html-mode-map [(control c) (>)] 'html-greater-than)
             (define-key html-mode-map [(control c) (&)] 'html-ampersand)))

(setq next-number 0)

(define-key global-map [f1]
  '(lambda nil (interactive)
     (print buffer-file-name (get-buffer "scratch"))
     ;;(format t "~%~A" buffer-file-name)(edebug)
     (if (string= (file-name-extension buffer-file-name) "lisp")
         (insert
          ";;;-*- Mode: common-lisp ; Package: USER ; Base: 10; Syntax: lisp  -*-
;;;-------------------------------------------------------------------------
;;;               Copyright (C) 2012 by Kestrel Technology
;;;                          All Rights Reserved
;;;-------------------------------------------------------------------------
;;;
;;;
;;; $Id: init.el,v 1.12 2005/04/14 18:16:45 becker Exp $
;;;
;;;
;;; $Log$
;;;
;;;
;;;
")
       (if (string= (file-name-extension buffer-file-name) "sl")
           (insert
            "%%%-*- Mode: slang-mode ; Package: USER ; Base: 10; Syntax: slang  -*-
%%%-------------------------------------------------------------------------
%%%               Copyright (C) 2012 by Kestrel Technology
%%%                          All Rights Reserved
%%%-------------------------------------------------------------------------
%%%
%%%
%%% $Id: init.el,v 1.12 2005/04/14 18:16:45 becker Exp $
%%%
%%% $Log$
%%%
%%%
%%%
"
            )))))


(autoload 'auto-make-header "header2")
(require 'my-python-header)

;; Load the font-lock package.
(require 'font-lock)
;; Maximum colors
(setq font-lock-maximum-decoration t)
;; Turn on font-lock in all modes that support it
(global-font-lock-mode t)


(setenv "GS_LIB" "C:/Program Files/gs/gs9.05/lib;C:/Program Files/gs/gs9.05/fonts")
(setq ps-lpr-command "C:/Program Files/gs/gs9.05/bin/gswin32c")
(setq ps-lpr-switches '("-q" "-dNOPAUSE" "-dBATCH" "-sDEVICE=mswinprn"))
(setq ps-printer-name t)

(autoload 'sgml-mode "psgml" "Major mode to edit SGML files." t)
(autoload 'xml-mode "psgml" "Major mode to edit XML files." t)

(setq semantic-load-turn-everything-on t)


(defconst my-speedbar-buffer-name " SPEEDBAR")

;; (defun my-speedbar-no-separate-frame ()
;;   (interactive)
;;   (when (not (buffer-live-p speedbar-buffer))
;;     (setq speedbar-buffer (get-buffer-create my-speedbar-buffer-name)
;;           speedbar-frame (selected-frame)
;;           dframe-attached-frame (selected-frame)
;;           speedbar-select-frame-method 'attached
;;           speedbar-verbosity-level 0
;;           speedbar-last-selected-file nil)
;;     (set-buffer speedbar-buffer)
;;     (speedbar-mode)
;;     (speedbar-reconfigure-keymaps)
;;     (speedbar-update-contents)
;;     (speedbar-set-timer 1)
;;     (make-local-hook 'kill-buffer-hook)
;;     (add-hook 'kill-buffer-hook
;;               (lambda () (when (eq (current-buffer) speedbar-buffer)
;;                            (setq speedbar-frame nil
;;                                  dframe-attached-frame nil
;;                                  speedbar-buffer nil)
;;                            (speedbar-set-timer nil)))))
;;     (set-window-buffer (selected-window)
;;                        (get-buffer my-speedbar-buffer-name)))
;; (global-set-key (kbd "M-S M-S") 'my-speedbar-no-separate-frame)
;; (autoload 'speedbar-frame-mode "speedbar" "Popup a speedbar frame" t)
;; (autoload 'speedbar-get-focus "speedbar" "Jump to speedbar frame" t)
;; (setq speedbar-use-images t)


;; (setq speedbar-frame-parameters '((minibuffer . nil)
;;                                   (border-width . 0)
;;                                   (internal-border-width . 0)
;;                                   (menu-bar-lines . 0)
;;                                   (tool-bar-lines . 0)
;;                                   (modeline . t)
;;                                   (name . "SpeedBar")
;;                                   (width . 24)
;;                                   (height . 60)
;;                                   (unsplittable . t)))

                                        ;(require 'graphene)
                                        ;(require 'project-persist-drawer)
                                        ;(require 'ppd-sr-speedbar) ;; or another adaptor
                                        ;(project-persist-drawer-mode t)

(message "Loading sr-speedbar")
;(require 'sr-speedbar)
;(setq speedbar-use-images t)
;(global-set-key [C-f4] 'sr-speedbar-toggle)
;(global-set-key (kbd "<f4>") 'sr-speedbar-select-window)
;(setq speedbar-directory-unshown-regexp "^\\(CVS\\|RCS\\|SCCS\\|\\.\\.*$\\)\\'")
;(setq speedbar-directory-unshown-regexp  "^\\(\\.*\\)\\'")

(setq-default ispell-program-name
              (cond (running-ms-windows
                     ;;"c:/Program Files/Aspell6/x64/bin/aspell.exe"
                     "aspell")
                    (running-macos
                     "/opt/local/bin/aspell")
                    (t
                     (if (file-executable-p "/usr/bin/hunspell")
                         "/usr/bin/hunspell"
                       "/usr/bin/aspell"))))

(setq ediff-diff-program ; your Lisp system
      (cond (running-ms-windows
             "c:/cygwin/bin/diff")
            (t
             "diff")))

(setq grep-command "grep -i -nH -e -r ")

(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(autoload 'flyspell-delay-command "flyspell" "Delay on command." t)
(autoload 'tex-mode-flyspell-verify "flyspell" "" t)



;; ;; If antlr-mode is not part of your distribution, put this file into your
;; ;; load-path and the following into your ~/.emacs:
(autoload 'antlr-mode "antlr-mode" nil t)
(add-hook 'speedbar-load-hook		; would be too late in antlr-mode.el
          (lambda () (speedbar-add-supported-extension ".g")))

;; If you edit ANTLR's source files, you might also want to use
(autoload 'antlr-set-tabs "antlr-mode")
(add-hook 'java-mode-hook 'antlr-set-tabs)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initial setup
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This assumes that Cygwin is installed in C:\cygwin (the
;; default) and that C:\cygwin\bin is not already in your
;; Windows Path (it generally should not be).

;;(when (and running-ms-windows ; Windows
;;  (require 'cygwin-mount nil t))
;;  (setenv "PATH" (concat "c:/cygwin/bin;" (getenv "PATH")))
;;  (setq exec-path (cons "c:/cygwin/bin/" exec-path))
;;    (require 'setup-cygwin)
;;  ;(cygwin-mount-activate)
;;  )



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (A) M-x shell: This change M-x shell permanently
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Would call Windows command interpreter. Change it.

;;(setq shell-file-name
;;      (if running-ms-windows ; Windows
;;          "bash.exe" "bash"))

;;(setenv "SHELL" shell-file-name)
;;(setq explicit-shell-file-name shell-file-name)

(defun cygwin-shell ()
  "Run cygwin bash in shell mode."
  (interactive)
  (when running-ms-windows
    (let ((explicit-shell-file-name "C:/cygwin/bin/bash"))
      (when (require 'cygwin-mount nil t)
        (cygwin-mount-activate))

      (setq binary-process-input t)
      (setq w32-quote-process-args ?\")
      ;; (setenv "PATH"
      ;;         (concat ".:/usr/local/bin:/mingw/bin:/bin:"
      ;;                 (replace-regexp-in-string " " "\\\\ "
      ;;                 (replace-regexp-in-string "\\\\" "/"
      ;;                 (replace-regexp-in-string "\\([A-Za-z]\\):" "/\\1"
      ;;                 (getenv "PATH"))))))
      (call-interactively 'shell))))




;;*** 41.3 Shell Mode

;; ;; general command interpreter in a window stuff
;; (when (try-require 'comint)

;;   ;; `M-s'    `comint-next-matching-input'
;;   ;; `M-r'    `comint-previous-matching-input'
;;   ;; `M-n'    `comint-next-input'
;;   ;; `M-p'    `comint-previous-input'
;;   ;; `C-up'   `last command'

;;   ;; don't add input matching the last on the input ring
;;   (setq-default comint-input-ignoredups t)

;;   ;; input to interpreter causes (only) the selected window to scroll
;;   (setq-default comint-scroll-to-bottom-on-input "this")

;;   ;; output to interpreter causes (only) the selected window to scroll
;;   (setq-default comint-scroll-to-bottom-on-output "this")

;;   ;; show the maximum output when the window is scrolled
;;   (setq-default comint-scroll-show-maximum-output t)

;;   ;; ignore short commands as well as duplicates
;;   (setq comint-min-history-size 5)
;;   (make-variable-buffer-local 'comint-min-history-size)
;;   (setq-default comint-input-filter
;;                 (function
;;                  (lambda (str)
;;                    (and (not (string-match "\\`\\s *\\'" str))
;;                         (> (length str) comint-min-history-size)))))

;;   ;; functions to call after output is inserted into the buffer
;;   ;(setq-default comint-output-filter-functions
;;    ;;; go to the end of buffer
;;   ;;;   '(comint-postoutput-scroll-to-bottom))

;;   ;; get rid of the ^M characters
;;   (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
;;   (add-hook 'comint-output-filter-functions  'shell-strip-ctrl-m nil t)

;;   ;; prompt in the minibuffer for password and send without echoing
;;   ;; (for example, with `su' command)
;;   (add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

;;   ;; use the `up' and `down' arrow keys to traverse through the previous
;;   ;; commands
;;   (defun my-shell-mode-hook ()
;;     "Customize my shell-mode."
;;     (local-set-key (kbd "<up>") 'comint-previous-input)
;;     (local-set-key (kbd "<down>") 'comint-next-input))

;;   (add-hook 'shell-mode-hook 'my-shell-mode-hook))


;; (require 'shell-command)
;; (shell-command-completion-mode)

(set-default 'mode-line-buffer-identification
             (list (concat "Emacs[" machine-nickname "]: %17b")))

(load-file (concat marcel-lisp-dir  "/becker-mode-line.el"))
(load-file (concat marcel-lisp-dir  "/header-line.el"))

(setq frame-title-format
      '("EMACS: [" (:eval (getenv "USERNAME")) "@"
        (:eval (downcase (system-name))) "]: "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b")) " [%*]"))


(setq-default header-line-format
              (list '(:eval (concat
                             (propertize " " 'display '((space :align-to (- right-fringe 16))))
                             display-time-string))))

;;(progn
;;  (define-key minibuffer-local-completion-map " " 'minibuffer-complete-word)
;;  (define-key minibuffer-local-filename-completion-map " " 'minibuffer-complete-word)
;;  (define-key minibuffer-local-must-match-map " " 'minibuffer-complete-word))

(defun unix-file ()
  "Change the current buffer to Latin 1 with Unix line-ends."
  (interactive)
  (set-buffer-file-coding-system 'iso-latin-1-unix t))

(defun dos-file ()
  "Change the current buffer to Latin 1 with DOS line-ends."
  (interactive)
  (set-buffer-file-coding-system 'iso-latin-1-dos t))

(defun mac-file ()
  "Change the current buffer to Latin 1 with Mac line-ends."
  (interactive)
  (set-buffer-file-coding-system 'iso-latin-1-mac t))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;[Latex/Tex]
(message "Loading tex")
(require 'tex-mik)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
;; the default flyspell behaviour
(put 'LaTex-mode 'flyspell-mode-predicate 'tex-mode-flyspell-verify)

(setq reftex-plug-into-AUCTeX t)
(setq TeX-source-specials-mode t)
(setq-default TeX-master "master") ; Query for master file.
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
(setq TeX-source-correlate-start-server t)
(setq TeX-source-correlate-method 'synctex)
(setq TeX-PDF-mode t)

;;(setq TeX-view-program-list '(("Evince" "evince --page-index=%(outpage) %o")))
;;(setq TeX-view-program-selection '((output-pdf "Evince")))
;;(setq TeX-output-view-style '("^pdf$" "." "C:/Program Files (x86)/SumatraPDF/SumatraPDF.exe %o"))

(setq TeX-view-program-list
      '(("SumatraPDF" "\"C:/Program Files (x86)/SumatraPDF/SumatraPDF.exe\" -reuse-instance %o")
        ("Okular" "okular --unique %o#src:%n%b")
        ("Skim" "/Applications/TeX/Skim.app/Contents/SharedSupport/displayline %n %o %b")))
;;("Skim" "/Applications/TeX/Skim.app/Contents/SharedSupport/displayline %q")))


(setq TeX-view-program-selection
      (cond (running-ms-windows
             '((output-pdf "SumatraPDF")
               (output-dvi "Yap")))
            (running-linux
             '((output-pdf "Okular")
               (output-dvi "Okular")))
            (running-macos
             '((output-pdf "Skim")))))



;; (eval-after-load "tex"
;;   (progn
;;     (add-to-list 'TeX-expand-list
;;                  '("%u" okular-make-url))
;;     (add-to-list 'TeX-expand-list
;;                  '("%q" skim-make-url))))

;; (defun okular-make-url () (concat
;;      "file://"
;;      (expand-file-name (funcall file (TeX-output-extension) t)
;;          (file-name-directory (TeX-master-file)))
;;      "#src:"
;;      (TeX-current-line)
;;      (TeX-current-file-name-master-relative)))

;; (defun skim-make-url () (concat
;;      (TeX-current-line)
;;      " "
;;      (expand-file-name (funcall file (TeX-output-extension) t)
;;          (file-name-directory (TeX-master-file)))
;;      " "
;;      (buffer-file-name)))

(if running-ms-windows
    (require 'sumatra-forward))

(when running-macos
  (setenv "PATH" (concat (getenv "PATH") ":/usr/texbin"))
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
  (setq exec-path (append exec-path '("/usr/texbin")))
  (setq exec-path (append exec-path '("/usr/local/bin"))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;[ACL2.0 Stuff]

(setq *acl2-interface-dir* "D:/ACL2-4.3/acl2-sources/interface/emacs/")

(setq inferior-acl2-program "sbcl --core d:/ACL2-4.3/acl2-sources/saved_acl2.core")

(autoload 'run-acl2 ;;emacs 19.27 only at this time
  (concat *acl2-interface-dir* "top-start-inferior-acl2")
  "Begin ACL2 in an inferior ACL2 mode buffer."
  t)



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; [ CTAGS ]

;;(require 'ctags-update)
;;(ctags-update-minor-mode 1)
(setq path-to-ctags
      (if running-ms-windows
          "etags.exe"
        "etags"))

(setq default-tags-file
      (if running-ms-windows "D:/Source/TAGS" (expand-file-name "~/src/TAGS")))


(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s --append -f %s -R %s" path-to-ctags default-tags-file (directory-file-name dir-name))))


;;horizontal-to-vertical
;; Idea and starter code from Benjamin Rutt (rutt.4+news@osu.edu) on comp.emacs
(defun my-window-horizontal-to-vertical ()
  "Switches from a horizontal split to a vertical split."
  (interactive)
  (let ((one-buf (window-buffer (selected-window)))
        (buf-point (point)))
    (other-window 1)
    (delete-other-windows)
    (split-window-horizontally)
    (switch-to-buffer one-buf)
    (goto-char buf-point)))

;; vertical-to-horizontal
;; complement of above created by rgb 11/2004
(defun my-window-vertical-to-horizontal ()
  "Switches from a vertical split to a horizontal split."
  (interactive)
  (let ((one-buf (window-buffer (selected-window)))
        (buf-point (point)))
    (other-window 1)
    (delete-other-windows)
    (split-window-vertically)
    (switch-to-buffer one-buf)
    (goto-char buf-point)))


(defun my-toggle-window-split ()
  "Vertical split shows more of each line, horizontal split shows
more lines. This code toggles between them. It only works for
frames with exactly two windows."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

;;  C-x h runs the command mark-whole-buffer
;;    C-M-\ runs the command indent-region
;;You can also insert something like:
(defun my-indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))
(global-set-key (kbd "<f5>") 'my-indent-buffer)


(global-set-key (kbd "C-c |") 'my-toggle-window-split)
(global-set-key (kbd "C-c \\") 'my-window-horizontal-to-vertical)
(global-set-key (kbd "C-c /") 'my-window-vertical-to-horizontal)

(message "menu-bar+")
(require 'menu-bar+)

;; (let ((menu-bar+-lib (concat marcel-lisp-dir "/el-get/menu-bar+")))
;;   (when (file-exists-p menu-bar+-lib)
;;     (add-to-list 'load-path menu-bar+-lib)
;;     (eval-after-load "menu-bar" '(require 'menu-bar+))))


(message "Loading time-stamp")
(when (try-require 'time-stamp)
  ;; format of the string inserted by `M-x time-stamp'
  (setq time-stamp-format "%Y-%02m-%02d %3a %02H:%02M %u on %s")
  ;; `YYYY-MM-DD Weekday HH:MM user on system'
  ;; see `system-time-locale' for non-numeric formatted items of time
  ;; update time stamps every time you save a buffer
  (add-hook 'write-file-hooks 'time-stamp))

;; insert a time stamp string
(defun my-insert-time-stamp ()
  "Insert a time stamp."
  (interactive "*")
  (insert (format "%s %s %s %s"
                  comment-start
                  (format-time-string "%Y-%m-%d %a %H:%M")
                  (user-login-name)
                  comment-end)))

(defun my-insert-date (prefix)
  "Insert the current date in ISO format. With prefix-argument,
add day of week. With two prefix arguments, add day of week and
time."
  (interactive "P")
  (let ((format (cond ((not prefix) "%Y-%m-%d")
                      ((equal prefix '(4)) "%Y-%m-%d %a")
                      ((equal prefix '(16)) "%Y-%m-%d %a %H:%M"))))
    (insert (format-time-string format))))


(load "load-specware")

;;  (push "C:/acl90express/eli" load-path)
;;  (load "fi-site-init.el")

;;  (setq fi:common-lisp-image-name "C:/acl90express/allegro-express.exe")
;;  (setq fi:common-lisp-image-file "C:/acl90express/allegro-express.dxl")
;;  (setq fi:common-lisp-directory "C:/src/Specware")
;; (setq fi:lisp-mode-hook
;;       (function
;;        (lambda ()
;;   (let ((map (current-local-map)))
;;     (define-key map "\C-c."	'find-tag)
;;     (define-key map "\C-c,"	'tags-loop-continue)
;;     (define-key map "\e."	'fi:lisp-find-definition)
;;     (define-key map "\e,"	'fi:lisp-find-next-definition)))))



(let ((lilypond-lib "c:/Program Files (x86)/LilyPond/usr/share/emacs/site-lisp"))
  (when (file-exists-p lilypond-lib)
    (add-to-list 'load-path lilypond-lib)
    (autoload 'LilyPond-mode "lilypond-mode" "LilyPond Editing Mode" t)
    (add-to-list 'auto-mode-alist '("\\.ly$" . LilyPond-mode))
    (add-to-list 'auto-mode-alist '("\\.ily$" . LilyPond-mode))
    (add-hook 'LilyPond-mode-hook (lambda () (turn-on-font-lock)))))



(defun my-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char) (insert "\n"))
    (indent-region begin end))
  (message "Ah, much better!"))



;; eclipse-java-style is the same as the "java" style (copied from
;; cc-styles.el) with the addition of (arglist-cont-nonempty . ++) to
;; c-offsets-alist to make it more like default Eclipse formatting -- function
;; arguments starting on a new line are indented by 8 characters
;; (++ = 2 x normal offset) rather than lined up with the arguments on the
;; previous line
(defconst eclipse-java-style
  '((c-basic-offset . 4)
    (c-comment-only-line-offset . (0 . 0))
    ;; the following preserves Javadoc starter lines
    (c-offsets-alist . ((inline-open . 0)
                        (topmost-intro-cont    . +)
                        (statement-block-intro . +)
                        (knr-argdecl-intro     . 5)
                        (substatement-open     . +)
                        (substatement-label    . +)
                        (label                 . +)
                        (statement-case-open   . +)
                        (statement-cont        . +)
                        (arglist-intro  . c-lineup-arglist-intro-after-paren)
                        (arglist-close  . c-lineup-arglist)
                        (access-label   . 0)
                        (inher-cont     . c-lineup-java-inher)
                        (func-decl-cont . c-lineup-java-throws)
                        (arglist-cont-nonempty . ++)
                        )))
  "Eclipse Java Programming Style")
(c-add-style "ECLIPSE" eclipse-java-style)
(customize-set-variable 'c-default-style (quote ((java-mode . "eclipse") (awk-mode . "awk") (other . "gnu"))))



;; (defun my-minimap-toggle ()
;;   "Toggle minimap for current buffer."
;;   (interactive)
;;   (if (not (boundp 'minimap-bufname))
;;       (setq minimap-bufname nil))
;;   (if (null minimap-bufname)
;;       (progn (minimap-create)
;;       (set-frame-width (selected-frame) 180))
;;     (progn (minimap-kill)
;;     (set-frame-width (selected-frame) 140))))


                                        ;(require 'color-theme)


  ;;;; This snippet enables lua-mode
;; This line is not necessary, if lua-mode.el is already on your load-path


(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

(message "Loading docker-mode")
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

(defun my-keytable (arg)
  "Print the key bindings in a tabular form."
  (interactive "sEnter a modifier string:")
  (with-output-to-temp-buffer "*Key table*"
    (let* ((i 0)
           (keys (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"
                       "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
                       "<return>" "<down>" "<up>" "<right>" "<left>"
                       "<home>" "<end>" "<f1>" "<f2>" "<f3>" "<f4>" "<f5>"
                       "<f6>" "<f7>" "<f8>" "<f9>" "<f10>" "<f11>" "<f12>"
                       "1" "2" "3" "4" "5" "6" "7" "8" "9" "0"
                       "`" "~" "!" "@" "#" "$" "%" "^" "&" "*" "(" ")" "-"
                       "_" "=" "+" "\\" "|" "{" "[" "]" "}" ";" "'" ":"
                       "\"" "<" ">" "," "." "/" "?"))
           (n (length keys))
           (modifiers (list "" "S-" "C-" "M-" "M-C-"))
           (k))
      (or (string= arg "") (setq modifiers (list arg)))
      (setq k (length modifiers))
      (princ (format " %-10.10s |" "Key"))
      (let ((j 0))
        (while (< j k)
          (princ (format " %-28.28s |" (nth j modifiers)))
          (setq j (1+ j))))
      (princ "\n")
      (princ (format "_%-10.10s_|" "__________"))
      (let ((j 0))
        (while (< j k)
          (princ (format "_%-28.28s_|"
                         "_______________________________"))
          (setq j (1+ j))))
      (princ "\n")
      (while (< i n)
        (princ (format " %-10.10s |" (nth i keys)))
        (let ((j 0))
          (while (< j k)
            (let* ((binding
                    (key-binding (read-kbd-macro (concat (nth j modifiers)
                                                         (nth i keys)))))
                   (binding-string "_"))
              (when binding
                (if (eq binding 'self-insert-command)
                    (setq binding-string (concat "'" (nth i keys) "'"))
                  (setq binding-string (format "%s" binding))))
              (setq binding-string
                    (substring binding-string 0 (min (length
                                                      binding-string) 28)))
              (princ (format " %-28.28s |" binding-string))
              (setq j (1+ j)))))
        (princ "\n")
        (setq i (1+ i)))
      (princ (format "_%-10.10s_|" "__________"))
      (let ((j 0))
        (while (< j k)
          (princ (format "_%-28.28s_|"
                         "_______________________________"))
          (setq j (1+ j))))))
  (delete-window)
  (hscroll-mode)
  (setq truncate-lines t))



;;; Buffer scrolling
(message "Loading smooth-scroll")
(require 'smooth-scroll)
(setq redisplay-dont-pause t)
(setq  scroll-margin 1)
(setq  scroll-step 1)
(setq  scroll-conservatively 10000)
(setq  scroll-preserve-screen-position 1)
(setq auto-window-vscroll nil)
                                        ;(setq  smooth-scroll/vscroll-step-size 1)
                                        ;(smooth-scroll-mode 1)


(setq custom-file (concat marcel-lisp-dir "/custom.el"))
(load custom-file 'noerror)

(message "Loading rainbow-delimiters")
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)


;;; init.el ends here
(message "Loading tramp")
(require 'tramp)
(defun my-connect-remote ()
  (interactive)
  (dired "/ubuntu@10.130.2.77:/home/ubuntu/src")
  ;;(dired "/becker-openstack:/home/ubuntu/src")
  )

(defun my-connect-ptt ()
  (interactive)
  (dired "/DoD_Admin@50.225.83.4#422:/home/DoD_Admin/becker")
  )


(setq tramp-default-method "ssh")


(message "Loading browse-kill-ring")
(when (require 'browse-kill-ring nil 'noerror)
  (browse-kill-ring-default-keybindings))
(global-set-key "\C-cy" '(lambda ()
                           (interactive)
                           (popup-menu 'yank-menu)))

(message "Loading hexgrb")
(require 'hexrgb)
;;(message "Loading one-key")
;;(require 'one-key)
;;(message "Loading one-key-dir")
;;(require 'one-key-dir)
;;(message "Loading one-key-yas")
;;(require 'one-key-yas)
;;(require 'one-key-bmkp)
;(global-set-key (kbd "C-<f5>") 'one-key-open-associated-menu-set)

(message "Loading find-file-in-project")
(require 'find-file-in-project)
(global-set-key (kbd "C-c M-f") 'find-file-in-project)
