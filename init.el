;;; Time-stamp: "2018-09-27 Thu 16:02 marcelbecker on kestrelimac"
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
  (setq mac-option-modifier 'alt)
  (setq mac-right-option-modifier 'super)
  ;;  (setq mac-right-command-modifier 'super)
  (setq mac-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char)
  (setq ns-use-srgb-colorspace nil)
  (setq powerline-image-apple-rgb t)
  (setq mac-allow-anti-aliasing t)
  ) ;; sets fn-delete to be right-delete


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

(defconst user-cache-directory
  (expand-file-name (concat user-emacs-directory ".cache/"))
  "My emacs storage area for persistent files.")
;; create the `user-cache-directory' if it doesn't exist
(make-directory user-cache-directory t)


;;; ADD Marcel'S LISP LIBRARY TO `load-path'.
;;(add-to-list 'load-path marcel-lisp-dir )


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



;; (defvar missing-packages-list nil
;;   "List of packages that `try-require' can't find.")

;; ;; attempt to load a feature/library, failing silently
;; (defun try-require (feature)
;;   "Attempt to load a library or module. Return true if the
;; library given as argument is successfully loaded. If not, instead
;; of an error, just add the package to a list of missing packages."
;;   (condition-case err
;;       ;; protected form
;;       (progn
;;         (message "Checking for library `%s'..." feature)
;;         (if (stringp feature)
;;             (load-library feature)
;;           (require feature))
;;         (message "Checking for library `%s'... Found" feature))
;;     ;; error handler
;;     (file-error  ; condition
;;      (progn
;;        (message "checking for library `%s'... Missing" feature)
;;        (add-to-list 'missing-packages-list feature 'append))
;;      nil)))


(tool-bar-mode -1)

;; use C-u C-x = to describe face at point.
(setq default-frame-font
      (cond (running-ms-windows
             "DejaVu Sans Mono 11")
            (running-macos
             ;;"Source Code Pro 16")
             "DejaVu Sans Mono 18")
            ;;        "Geneva 13")
            ((not running-macos)
             "DejaVu Sans Mono 13")))

(set-frame-font default-frame-font)

;;; Nice size for the default window
(defun get-default-height ()
  (min 60 (/ (- (display-pixel-height) 200) (frame-char-height))))



(defun get-default-x-frame-position ()
  (- (/ (display-pixel-width) 2) 400))

(defun get-default-y-frame-position ()
  (- (/ (display-pixel-height) 2) (/ (get-default-height) 2)))


;; workarea -- Position and size of the work area in pixels in the
;;             form of (X Y WIDTH HEIGHT)
;;
;; Use (display-monitor-attributes-list) to get monitor info
;;
;; (- (elt (window-pixel-edges) 3)
;;   (elt (window-inside-pixel-edges) 3))
;;
;; 2/3 of the workarea height
(defun my-get-default-frame-height ()
  (let* ((workarea (frame-monitor-workarea))
         (height (nth 3 workarea)))
    (floor  (- height 200) (frame-char-height))))


(defun my-get-default-x-frame-position ()
  (let* ((workarea (frame-monitor-workarea))
         (width (nth 2 workarea))
         (display-x (nth 0 workarea)))
    (+ (floor width 6) display-x)))

(defun my-get-default-y-frame-position ()
  (let* ((workarea (frame-monitor-workarea))
         (width (nth 3 workarea))
         (display-y (nth 1 workarea)))
    (+ 100 display-y)))


(setq default-frame-alist
      '((cursor-color . "white")
        (mouse-color . "white")
        (foreground-color . "white")
        (cursor-type . box)
        (tool-bar-lines . 0)
        ;;(top . 50)
        ;;(left . 50)
        ;;(width . 180)
        ))

(setq initial-frame-alist
      '((cursor-color . "white")
        (mouse-color . "white")
        (foreground-color . "white")
        (cursor-type . box)
        (tool-bar-lines . 0)
        ;;(top . 50)
        ;;(left . 50)
        ;;(width . 180)
        ))


;; Set Frame width/height
(defun arrange-frame (w h x y)
  "Set the width, height, and x/y position of the current frame"
  (let ((frame (selected-frame)))
    (delete-other-windows)
    (set-frame-position frame x y)
    (set-frame-size frame w h)))



(let* ((frame-font (cons 'font default-frame-font))
       (default-height (my-get-default-frame-height))
       (frame-height (cons 'height default-height))
       (frame-width (cons 'width 180))
       (frame-top (cons 'top (my-get-default-y-frame-position)))
       (frame-left (cons 'left (my-get-default-x-frame-position)))
       (frame-background-color (if (eq (user-uid) 0)
                                   '(background-color . "gray38")
                                 '(background-color . "#09223F")
                                 )))
  (add-to-list 'default-frame-alist frame-font)
  (add-to-list 'initial-frame-alist frame-font)

  (add-to-list 'default-frame-alist frame-height)
  (add-to-list 'initial-frame-alist frame-height)

  (add-to-list 'default-frame-alist frame-background-color)

  (add-to-list 'initial-frame-alist frame-background-color)

  (add-to-list 'default-frame-alist frame-width)
  (add-to-list 'initial-frame-alist frame-width)

  (add-to-list 'default-frame-alist frame-top)
  (add-to-list 'initial-frame-alist frame-top)

  (add-to-list 'default-frame-alist frame-left)
  (add-to-list 'initial-frame-alist frame-left)

  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))

  (message  "Frame alist %s" initial-frame-alist)
  (arrange-frame 180 (my-get-default-frame-height) (my-get-default-x-frame-position) (my-get-default-y-frame-position))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ELPA Packages
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ;;("melpa" . "https://stable.melpa.org/packages/")
                         ("melpas" . "https://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ))



;; use
;; brew install libressl
(require 'gnutls)
(add-to-list 'gnutls-trustfiles "/usr/local/etc/openssl/cert.pem")

;; (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
;;     (not (gnutls-available-p))))
;;        (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
;;   (add-to-list 'package-archives (cons "melpa" url) t))


(message "Loading use-package")
(setq package-check-signature nil)
(setq package-user-dir (concat marcel-lisp-dir "elpa"))
(add-to-list 'load-path (concat marcel-lisp-dir "elpa"))
;;(when (not package-archive-contents)
;;(package-refresh-contents))
(if (version< emacs-version "28.0")
    (package-initialize))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))


(setq my-elpa-packages
      '(
        ac-helm
        ac-ispell
        ac-python
        ace-flyspell
        ace-jump-helm-line
        ace-jump-mode
        ace-link
        ace-window
        adaptive-wrap
        ;;afternoon-theme
        aggressive-indent
        ;;alect-themes
        alert
        all-the-icons
        ample-regexps
        ;;ample-theme
        ;;ample-zen-theme
        anaconda-mode
        ;;anti-zenburn-theme
        ;;anything
        anzu
        ;;apropospriate-theme
        async
        ;;auctex
        auto-compile
        ;;auto-complete
        ;;auto-complete-auctex
        auto-dictionary
        auto-highlight-symbol
        auto-yasnippet
        autopair
        autothemer
        avy
        ;;badwolf-theme
        bind-key
        bind-map
        ;;birds-of-paradise-plus-theme
        browse-kill-ring
        ;;bubbleberry-theme
        buffer-move
        ;;busybee-theme
        ;;cherry-blossom-theme
        cl-lib
        clean-aindent-mode
        ;;clues-theme
        coffee-mode
        color-theme
        ;;color-theme-sanityinc-solarized
        ;;color-theme-sanityinc-tomorrow
        ;;color-theme-tango
        column-enforce-mode
        company
        company-anaconda
        company-jedi
        company-quickhelp
        company-statistics
        company-tern
        counsel
        csv-mode
        ctable
        ;;cyberpunk-theme
        cython-mode
        ;;dakrone-theme
        ;;darkburn-theme
        ;;darkmine-theme
        ;;darkokai-theme
        ;;darktooth-theme
        dash
        dash-functional
        deferred
        define-word
        diff-hl
        diminish
        ;;dired+
        dired-atool
        dired-avfs
        ;;dired-details
        ;;dired-details+
        dired-dups
        dired-efap
        dired-explorer
        dired-fdclone
        dired-filetype-face
        dired-filter
        dired-hacks-utils
        dired-imenu
        dired-launch
        dired-narrow
        ;;dired-nav-enhance
        dired-open
        dired-quick-sort
        dired-rainbow
        dired-single
        ;;dired-sort
        ;;dired-sort-menu
        ;;dired-sort-menu+
        dired-subtree
        dired-toggle
        dired-toggle-sudo
        diredful
        direx
        direx-grep
        ;;django-theme
        dockerfile-mode
        ;;doremi
        ;;doremi-frm
        ;;doremi-cmd
        ;;dracula-theme
        dumb-jump
        ;;el-get
        elisp-slime-nav
        elpy
        ;;emacs-eclim
        epc
        epl
        ;;escreen
        ;;espresso-theme
        eval-sexp-fu
        ;; evil
        ;; evil-anzu
        ;; evil-args
        ;; evil-ediff
        ;; evil-escape
        ;; evil-exchange
        ;; evil-iedit-state
        ;; evil-indent-plus
        ;; evil-indent-textobject
        ;; evil-leader
        ;; evil-lisp-state
        ;; evil-magit
        ;; evil-matchit
        ;; evil-mc
        ;; evil-nerd-commenter
        ;; evil-numbers
        ;; evil-search-highlight-persist
        ;; evil-surround
        ;; evil-tutor
        ;; evil-unimpaired
        ;; evil-visual-mark-mode
        ;; evil-visualstar
        exec-path-from-shell
        expand-region
        eyebrowse
        f
        fancy-battery
        ;;farmhouse-theme
        fill-column-indicator
        find-file-in-project
        ;;firebelly-theme
        ;;flatland-theme
        ;;flatui-theme
        flx
        flx-ido
        flycheck
        flycheck-pos-tip
        flymake
        flyspell-correct
        flyspell-correct-helm
        ;;frame-cmds
        ;;frame-fns
        fringe-helper
        fuzzy
        ;;gandalf-theme
        gh-md
        git-commit
        git-gutter
        git-gutter+
        git-gutter-fringe
        git-gutter-fringe+
        git-link
        git-messenger
        git-timemachine
        gitattributes-mode
        gitconfig-mode
        gitignore-mode
        gntp
        gnuplot
        golden-ratio
        google-translate
        ;;gotham-theme
        goto-chg
        goto-last-change
        ;;grandshell-theme
        ;;gruber-darker-theme
        ;;gruvbox-theme
        ;;hc-zenburn-theme
        header2
        helm
        helm-ag
        helm-c-yasnippet
        helm-company
        ;;helm-core
        helm-descbinds
        helm-flx
        ;;helm-git
        helm-git-files
        helm-gitignore
        helm-helm-commands
        helm-ls-git
        helm-make
        helm-mode-manager
        ;;helm-package
        helm-projectile
        helm-pydoc
        helm-spotify
        helm-swoop
        helm-themes
        ;;help-fns+
        ;;hemisu-theme
        ;;heroku-theme
        ;;hexrgb
        hide-comnt
        highlight
        highlight-indentation
        highlight-numbers
        highlight-parentheses
        hl-todo
        hlinum
        htmlize
        hungry-delete
        hy-mode
        hydra
        ;;icicles
        ;;ido-vertical-mode
        ;;idomenu
        iedit
        indent-guide
        ;;info+
        ;;inkpot-theme
        ;;ir-black-theme
        ;;jazz-theme
        ;;jbeans-theme
        jedi
        jedi-core
        js-doc
        js2-mode
        js2-refactor
        json
        json-mode
        json-reformat
        json-rpc
        json-snatcher
        latex-preview-pane
        let-alist
        ;;leuven-theme
        ;;light-soap-theme
        link-hint
        linum-relative
        livid-mode
        log4e
        lorem-ipsum
        ;;lua-mode
        ;;lush-theme
        macrostep
        ;;madhat2r-theme
        magit
        magit-gitflow
        magit-popup
        ;;majapahit-theme
        makey
        markdown-mode
        markdown-toc
        ;;material-theme
        ;;menu-bar+
        ;;minimal-theme
        mmm-mode
        ;;moe-theme
        ;;molokai-theme
        ;;monochrome-theme
        ;;monokai-theme
        move-text
        multiple-cursors
        ;;mustang-theme
        mwim
        ;;naquadah-theme
        neotree
        nginx-mode
        ;;niflheim-theme
        ;;noctilux-theme
        nose
        ;;nxml-mode
        ;;obsidian-theme
        ;;occidental-theme
        ;;oldlace-theme
        ;;omtose-phellack-theme
        open-junk-file
        org
        org-bullets
        org-download
        ;;org-plus-contrib
        org-pomodoro
        org-present
        org-projectile
        ;;organic-green-theme
        orgit
        ;;package
        ;;package-build
        packed
        page-break-lines
        paradox
        parent-mode
        ;;pastels-on-dark-theme
        pcache
        pcre2el
        persp-mode
        ;;phoenix-dark-mono-theme
        ;;phoenix-dark-pink-theme
        pip-requirements
        pkg-info
        ;;planet-theme
        popup
        popup-kill-ring
        popwin
        pos-tip
        powerline
        ;;professional-theme
        projectile
        ;;purple-haze-theme
        py-autopep8
        ;;pycomplete
        pydoc
        pydoc-info
        pyenv-mode
        pytest
        python-environment
        python-mode
        python-pep8
        pythonic
        pyvenv
        quelpa
        ;;railscasts-theme
        rainbow-delimiters
        rainbow-mode
        recentf-ext
        ;;redo+
        request
        restart-emacs
        ;;reverse-theme
        s
        seq
        ;;seti-theme
        shell-command
        simple-httpd
        skewer-mode
        smartparens
        smartrep
        smart-mode-line
        smart-mode-line-powerline-theme
        smeargle
        smooth-scroll
        smooth-scrolling
        ;;smyx-theme
        ;;soft-charcoal-theme
        ;;soft-morning-theme
        ;;soft-stone-theme
        ;;solarized-theme
        ;;soothe-theme
        ;;spacegray-theme
        spaceline
        spaceline-all-the-icons
        ;;spaceline-segments
        ;;spaceline-config
        ;;spacemacs-theme
        spinner
        spotify
        spray
        sr-speedbar
        ;;subatomic-theme
        ;;subatomic256-theme
        ;;sublime-themes
        ;;sunny-day-theme
        swiper
        swiper-helm
        switch-window
        tabbar
        ;;tango-2-theme
        ;;tango-plus-theme
        ;;tangotango-theme
        ;;tao-theme
        tern
        toc-org
        ;;toxi-theme
        ;;tronesque-theme
        ;;twilight-anti-bright-theme
        ;;twilight-bright-theme
        ;;twilight-theme
        ;;ujelly-theme
        unbound
        ;;underwater-theme
        undo-tree
        unfill
        ;;use-package
        ;;use-package-el-get
        uuidgen
        vi-tilde-fringe
        virtualenvwrapper
        vline
        volatile-highlights
        web-beautify
        websocket
        which-key
        window-number
        ;;window-numbering
        winum
        with-editor
        ws-butler
        yaml-mode
        yasnippet
        ;;zen-and-art-theme
        ;;zenburn-theme
        zencoding-mode
        ;;zonokai-theme
        ))


;; (dolist (p my-elpa-packages)
;;   (progn
;;     (when (not (package-installed-p p))
;;       (message "installing package %s" p)
;;       (package-install p))
;;     (message "loading package %s" p)
;;     (require p)
;;     ))


;;(package-initialize)
;;(message "Loading use-package")
;;(require 'use-package)

(use-package diminish :ensure t :diminish "")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; EL-GET
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To update el-get packages manually

(let ((elget-lib (concat marcel-lisp-dir "el-get/el-get")))
  (if (file-exists-p elget-lib)
      (add-to-list 'load-path elget-lib)))

(message "Loading el-get")
;; (use-package el-get
;;    :init
;;    (with-current-buffer
;;        (url-retrieve-synchronously
;;      "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
;;      (goto-char (point-max))
;;      (eval-print-last-sexp))
;;   )

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))


(add-to-list 'el-get-recipe-path
             (concat marcel-lisp-dir "el-get/el-get/recipes"))
(setq el-get-default-process-sync t
      el-get-verbose t)


(message "Loading el-get-elpa")
;;(use-package el-get-elpa)
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


(setq my-el-get-packages
      '(
        diff+
        dired+
        dired-column-widths
        dired-details
        dired-details+
        dired-sort
        dired-sort-menu
        dired-sort-menu+
        doremi
        doremi-cmd
        doremi-frm
        doremi-mac
        escreen
        facemenu+
        faces+
        ffap-
        file-template
        frame-cmds
        frame-fns
        helm-anything
        help-fns+
        menu-bar+
        pycomplete+
        recentf-buffer
        ring+
        setup-keys
        zoom-frm
        ))


;; Install new packages and init already installed packages
(message  "Initializing el-get packages")
(el-get 'sync my-el-get-packages)


;; (let ((fit-frame-lib (concat marcel-lisp-dir "/el-get/fit-frame")))
;;   (if (file-exists-p fit-frame-lib)
;;       (add-to-list 'load-path fit-frame-lib)))


;; (dolist (p my-el-get-packages)
;;    (progn
;;      (when (not (package-installed-p p))
;;       (message "installing package %s" p)
;;       ;;(package-install p)
;;       )
;;      (when (featurep p)
;;        (message "loading package %s" p)
;;        (condition-case nil
;;     (require p nil :noerror)
;;   (error nil)))))


;; open my Emacs init file
(defun my-open-dot-emacs ()
  "Opening `~/.emacs.d/init.el'"
  (interactive)
  (find-file (concat marcel-lisp-dir "init.el")))
(global-set-key (kbd "<S-f3>") 'my-open-dot-emacs)


(message "Loading auto-dim-other-buffers")
(use-package auto-dim-other-buffers
  :diminish "DIM"
  :init
  (add-hook 'after-init-hook
            (lambda ()
              (when (fboundp 'auto-dim-other-buffers-mode)
                (auto-dim-other-buffers-mode t)))))


(message "Loading org stuff")
(use-package org  :defer t
  ;;(use-package org-install)
  ;;(use-package ob-tangle)
  :config
  ;; make org mode allow eval of some langs
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (clojure . t)
     (python . t)
     (ruby . t)))
  ;; stop emacs asking for confirmation
  (setq org-confirm-babel-evaluate nil)
  (setq org-src-fontify-natively t)
  (setq org-support-shift-select t)
  )

(eval-after-load "org"
  '(progn
     (eval-after-load "cua-base"
       '(progn
          (defadvice org-call-for-shift-select (before org-call-for-shift-select-cua activate)
            (if (and cua-mode
                     org-support-shift-select
                     (not (use-region-p)))
                (cua-set-mark)))))))




;; open my Emacs init file
(defun my-open-notes ()
  "Opening `~/Dropbox/EmacsOrg/MarcelNotes.org'"
  (interactive)
  (find-file "~/Dropbox/EmacsOrg/MarcelNotes.org"))
(global-set-key (kbd "<M-S-f3>") 'my-open-notes)


;;(message "Loading diminish")
;;(use-package diminish)  ;; if you use :diminish

(message "Loading bind-key")
(use-package bind-key)  ;; if you use any :bind variant


(setq use-package-always-ensure t)

(message "restart emacs")
(use-package restart-emacs
  :commands restart-emacs)


(use-package wgrep
  :ensure t
  )
(use-package wgrep-ag
  :ensure t
  )


;; (use-package sublimity)
;; (use-package sublimity-scroll)
;; (use-package sublimity-map)
;;(use-package sublimity-attractive)



(defun my-load-python ()
  (interactive)
  (let* ((python-setup (concat marcel-lisp-dir  "elpy-init.el"))
         (anaconda-setup (concat marcel-lisp-dir  "anaconda-init.el")))
    (when (file-exists-p python-setup)
      (load-file python-setup))
    ;;(when (file-exists-p anaconda-setup)
    ;;(load-file anaconda-setup))
    ))

(defun my-load-helm ()
  (interactive)
  (let* ((helm-setup (concat marcel-lisp-dir  "helm-init.el")))
    (when (file-exists-p helm-setup)
      (load-file helm-setup))))

(defun my-load-bookmarks ()
  (interactive)
  (let* ((helm-setup (concat marcel-lisp-dir  "visual-bookmarks-init.el")))
    (when (file-exists-p helm-setup)
      (load-file helm-setup))))

(defun my-load-evil ()
  (interactive)
  (let* ((evil-setup (concat marcel-lisp-dir  "evil-init.el")))
    (when (file-exists-p evil-setup)
      (load-file evil-setup))))

(defun my-load-treemacs ()
  (interactive)
  (let* ((init-setup (concat marcel-lisp-dir  "treemacs-init.el")))
    (when (file-exists-p init-setup)
      (load-file init-setup))))


(setenv "WORKON_HOME" "~/PythonEnvs")
;;(add-hook 'python-mode-hook (function my-load-python))

(defun my-load-slime ()
  (interactive)
  (let* ((slime-library (concat marcel-lisp-dir  "slime/")))
    (when (file-exists-p slime-library)
      (add-to-list 'load-path slime-library)
      (use-package slime-autoloads)
      (eval-after-load "slime"
        '(progn
           (add-to-list 'load-path (concat marcel-lisp-dir "slime/contrib"))
           ;;(slime-setup '(slime-fancy slime-banner slime-repl slime-autodoc  slime-typeout-frame))
           (slime-setup '(slime-repl))
           (setq slime-complete-symbol*-fancy t)
           (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
           (global-set-key "\C-cs" 'slime-selector)
           (setq inferior-lisp-program ; your Lisp system
                 (if running-ms-windows "sbcl.exe --noinform" "/usr/local/bin/sbcl --noinform"))
           )))))


(message "Loading vline")
(use-package vline)
;;(require 'col-highlight)
;;(require 'tabbar-extension)


(message "Loading recentf")
(use-package recentf
  :init
  (progn
    (setq recentf-save-file (concat marcel-lisp-dir "recentf-" machine-nickname))
    (setq recentf-auto-cleanup 'never)
    (recentf-mode 1)
    (run-at-time nil (* 20 60) 'recentf-save-list)
    (setq recentf-max-saved-items 100)
    (setq recentf-max-menu-items 60)
    ;;(global-set-key [?\e ?\M-x] 'lacarte-execute-menu-command)
    ;;(global-set-key (kbd "C-x C-r") 'icicle-recent-file)
    (global-set-key (kbd "C-x C-r") 'recentf-open-files)
    (add-to-list 'recentf-exclude (concat marcel-lisp-dir "elpa"))
    (add-to-list 'recentf-exclude  ".*-autoloads\\.el\\'")
    (add-to-list 'recentf-exclude ".cache")
    (add-to-list 'recentf-exclude ".cask")
    (add-to-list 'recentf-exclude "bookmarks")
    (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
    (use-package recentf-ext)
    ))


;;(use-package recentf-buffer)
;;(use-package redo+)


(message "Loading undo-tree")
;;Graphical undo
(use-package undo-tree
  :commands (undo-tree-undo undo-tree-visualize)
  :diminish "UNDO"
  :init
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  (let ((undo-dir (concat user-cache-directory "undo")))
    (setq undo-tree-history-directory-alist '((".*" . ,undo-dir))))
  (global-undo-tree-mode))


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



(message "Loading savehist")
(use-package savehist
  :init
  (progn
    (setq savehist-file (concat marcel-lisp-dir "savehistory-" machine-nickname))
    (savehist-mode 1)))


(message "Loading unbound")
;; find convenient unbound keystrokes
(use-package unbound)                  ; `M-x describe-unbound-keys'
(message "Loading switch-window")
(use-package switch-window
  :custom-face
  (switch-window-label ((t (:inherit font-lock-keyword-face :height 3.0))))
  :config
  ;; (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-minibuffer-shortcut ?0)
  (setq switch-window-multiple-frames t)
  (with-eval-after-load 'ivy
    (setq switch-window-preferred 'ivy))
  (unless (display-graphic-p)
    (setq switch-window-shortcut-appearance 'asciiart)))
;;(use-package lacarte)


;; Easy window config switching
(use-package eyebrowse
  :hook (after-init . eyebrowse-mode))

(use-package popwin
  :commands popwin-mode
  :hook (after-init . popwin-mode)
  :config
  (bind-key "C-z" popwin:keymap)

  ;; don't use default value but manage it ourselves
  (setq popwin:special-display-config
        '(;; Emacs
          ("*Help*" :dedicated t :position bottom :stick t :noselect t)
          ("*compilation*" :dedicated t :position bottom :stick t :noselect t :height 0.4)
          ("*Compile-Log*" :dedicated t :position bottom :stick t :noselect t :height 0.4)
          ("*Warnings*" :dedicated t :position bottom :stick t :noselect t)
          ("*Completions*" :dedicated t :position bottom :stick t :noselect nil)
          ("*Pp Eval Output*" :dedicated t :position bottom :stick t :noselect t)
          ("*Shell Command Output*" :dedicated t :position bottom :stick t :noselect nil)
          ("\*Async Shell Command\*.+" :regexp t :position bottom :stick t :noselect nil)
          ("^*Man.+*$" :regexp t :position bottom :stick t :noselect t :height 0.4)
          ("^*WoMan.+*$" :regexp t :position bottom)
          ("^*Backtrace.+*$" :regexp t :dedicated t :position bottom :stick t :noselect nil)
          ("^*helpful .+*$" :regexp t :position bottom :stick t :noselect t :height 0.4)

          ;; Kill Ring
          ("*Kill Ring*" :dedicated t :position bottom)

          ;; Org
          ("*Org todo*" :dedicated t :position bottom :stick t :noselect nil :height 0.2)

          ;; Flycheck
          ("\*flycheck errors\*.+*$" :regexp t :position bottom :stick t :noselect nil)

          ;; Youdao dict
          ("*Youdao Dictionary*" :dedicated t :position bottom)

          ;; Paradox
          ("*Paradox Report*" :dedicated t :position bottom :noselect nil)

          ;; List
          ("*Colors*" :dedicated t :position bottom)
          ("*Process List*" :dedicated t :position bottom)
          ("*Process-Environment*" :dedicated t :position bottom)

          ;; undo-tree
          (" *undo-tree*" :dedicated t :position right :stick t :noselect nil :width 60)

          ;; Search
          ("*grep*" :dedicated t :position bottom :stick t :noselect nil)
          ("*ag search*" :dedicated t :position bottom :stick t :noselect nil :height 0.4)
          ("*rg*" :dedicated t :position bottom :stick t :noselect nil :height 0.4)
          ("*pt-search*" :dedicated t :position bottom :stick t :noselect nil :height 0.4)
          ("*Occur*" :dedicated t :position bottom :stick t :noselect nil)
          ("\*ivy-occur.+*$" :regexp t :position bottom :stick t :noselect nil)
          ;; ("*xref*" :dedicated t :position bottom :stick t :noselect nil)

          ;; VC
          ("*vc-diff*" :dedicated t :position bottom :stick t :noselect nil)
          ("*vc-change-log*" :dedicated t :position bottom :stick t :noselect nil)

          ;; Magit
          ;; (magit-status-mode :dedicated t :position bottom :stick t :height 0.5)
          ;; (magit-diff-mode :dedicated t :position bottom :stick t :noselect t :height 0.5)

          ;; Script
          ("*eshell*" :dedicated t :position bottom :stick t :noselect nil :height 0.4)
          ("*shell*" :dedicated t :position bottom :stick t :noselect nil :height 0.4)
          ("*Python*" :dedicated t :position bottom :stick t :noselect t)
          ("*Ruby*" :dedicated t :position bottom :stick t :noselect t)
          ("*quickrun*" :dedicated t :position bottom :stick t :noselect t)

          ;; Go
          ("^*godoc.+*$" :regexp t :position bottom :stick nil :noselect nil)
          ("*golint*" :dedicated t :position bottom :stick t :noselect nil)
          ("*govet*" :dedicated t :position bottom :stick t :noselect nil)
          ("*go-guru-output*" :dedicated t :position bottom :stick t :noselect nil)
          ("*Gofmt Errors*" :dedicated t :position bottom :stick t :noselect nil)
          ("*Go Test*" :dedicated t :position bottom :stick t :noselect nil)

          ;; Test
          ("*ert*" :dedicated t :position bottom :stick t :noselect nil)
          ("*nosetests*" :dedicated t :position bottom :stick t :noselect nil))))



;; save the place in files
(message "Loading saveplace")
(use-package saveplace
  :init
  (save-place-mode 1)
  (setq save-place-forget-unreadable-files nil)
  (setq save-place-file (concat marcel-lisp-dir "places-" machine-nickname)))



(message "Loading yasnippet")
(use-package yasnippet
  :diminish (yas-minor-mode . "")
  :init
  ;; (setq yas-snippet-dirs
  ;;       (list (concat marcel-lisp-dir "el-get/yasnippet/snippets")
  ;;             (concat marcel-lisp-dir "el-get/yasnippet-snippets")
  ;;             (concat marcel-lisp-dir "el-get/yasnippets")
  ;;             (concat marcel-lisp-dir "snippets")
  ;;             ))
  (yas-global-mode 1)
  )

(message "Loading anzu")
(use-package anzu
  :init
  (progn
    (global-anzu-mode +1)
    (set-face-attribute 'anzu-mode-line nil
                        :foreground "yellow" :weight 'bold)))

(define-key isearch-mode-map [remap isearch-query-replace]  #'anzu-isearch-query-replace)
(define-key isearch-mode-map [remap isearch-query-replace-regexp] #'anzu-isearch-query-replace-regexp)

;; C-x C-j opens dired with the cursor right on the file you're editing
;;(use-package dired-x)



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
(global-hl-line-mode 1)			; highlight current line


;;; Keep the highlight color 10% darker than the default background face
;;; https://emacs.stackexchange.com/questions/9740/how-to-define-a-good-highlight-face
(message "Loading color")
(use-package color)
;;(use-package pallet)

(defun my-set-hl-line-color-based-on-theme ()
  "Sets the hl-line face to have no foregorund and a background
    that is 10% darker than the default face's background."
  (interactive)
  (set-face-attribute 'hl-line nil
                      :foreground nil
                      :background (color-darken-name (face-background 'default) 10)))

;;(add-hook 'global-hl-line-mode-hook 'my-set-hl-line-color-based-on-theme)



;; dim the ignored part of the file name
(file-name-shadow-mode 1)

;;Line wrap
(global-visual-line-mode)
(setq line-move-visual t) ;; move via visual lines
(diminish 'visual-line-mode "")


(message "Loading hlinum")
(use-package hlinum
  :init
  (hlinum-activate))

(message "Loading expand-region")
(use-package expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(set-face-attribute 'region nil :background "magenta1" :foreground "#ffffff")

(set-face-attribute 'hl-line nil :background "#666666")
(set-face-attribute 'linum-highlight-face nil :background "#666666")

;; minibuffer completion incremental feedback
;;(icomplete-mode)

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
(setq tab-always-indent 'complete)  ;; use 't when company is disabled
(use-package company
  :ensure t
  :diminish "CMP"
  :bind (("A-." . company-complete)
         ("C-c C-y" . company-yasnippet)
         :map company-active-map
         ("<escape>" . company-abort)
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next)
         ("TAB" . company-complete-common-or-cycle)
         ("<tab>" . company-complete-common-or-cycle)
         ("S-TAB" . company-select-previous)
         ("<backtab>" . company-select-previous)
         :map company-search-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next))
  :hook (after-init . global-company-mode)
  :config
  (setq company-tooltip-align-annotations t ; aligns annotation to the right
        company-tooltip-limit 12            ; bigger popup window
        company-idle-delay .2               ; decrease delay before autocompletion popup shows
        company-echo-delay 0                ; remove annoying blinking
        company-minimum-prefix-length 2
        company-require-match nil
        company-dabbrev-ignore-case nil
        company-show-numbers t
        company-dabbrev-downcase nil)
  (add-hook 'after-init-hook 'global-company-mode)
  (global-company-mode)
  ;; Popup documentation for completion candidates
  (when (display-graphic-p)
    (use-package company-quickhelp
      :bind (:map company-active-map
                  ("M-h" . company-quickhelp-manual-begin))
      :hook (global-company-mode . company-quickhelp-mode)
      :config (setq company-quickhelp-delay 0.8))))



;; Replace Strings with Regexes
(use-package visual-regexp
  :bind (("A-%" . vr/replace)
         ("M-%" . vr/query-replace)))


;;(use-package auto-complete-config)
;;(ac-config-default)
;;(defadvice auto-complete-mode (around disable-auto-complete-for-python)
;;  (unless (eq major-mode 'python-mode) ad-do-it))
;;(ad-activate 'auto-complete-mode)



;; ----------------------------------------------------------[Window Number]

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

(use-package window-number
  :init
  (window-number-mode 1)
  :diminish "WN"
  )

;; numbered window shortcuts
;; (It numbers windows and you can switch them easily with `M-<number>').
;; (use-package window-numbering
;;   :init
;;   (progn
;;     (window-numbering-mode 1)
;;     (window-number-meta-mode 1)))


(use-package winum
  :ensure t
  :init
  (setq winum-keymap
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "C-`") 'winum-select-window-by-number)
          (define-key map (kbd "C-²") 'winum-select-window-by-number)
          (define-key map (kbd "M-0") 'winum-select-window-0-or-10)
          (define-key map (kbd "M-1") 'winum-select-window-1)
          (define-key map (kbd "M-2") 'winum-select-window-2)
          (define-key map (kbd "M-3") 'winum-select-window-3)
          (define-key map (kbd "M-4") 'winum-select-window-4)
          (define-key map (kbd "M-5") 'winum-select-window-5)
          (define-key map (kbd "M-6") 'winum-select-window-6)
          (define-key map (kbd "M-7") 'winum-select-window-7)
          (define-key map (kbd "M-8") 'winum-select-window-8)
          map))
  (setq winum-auto-setup-mode-line nil)
  :config
  (defun winum-assign-0-to-neotree ()
    (when (string-match-p (buffer-name) ".*\\*NeoTree\\*.*") 10))
  (add-to-list 'winum-assign-functions #'winum-assign-0-to-neotree)
  (set-face-attribute 'winum-face nil :weight 'bold)

  (setq window-numbering-scope            'global
        winum-reverse-frame-list          nil
        winum-auto-assign-0-to-minibuffer t
        winum-assign-func                 'my-winum-assign-func
        winum-auto-setup-mode-line        t
        winum-mode-line-position          1
        winum-ignored-buffers             '(" *which-key*"))
  (winum-mode))


;; ;;Quickly jump between windows using ace-window, I used it frequently and bind it F1.
(message "Loading ace-window")
(use-package ace-window
  :init
  (global-set-key (kbd "<f1>") 'ace-window)
  (setq aw-scope 'frame))

;; ;; ;; ----------------------------------------------------------[Window Number]


;; ;; ;;(use-package flycheck)
;; ;; ;;(add-hook 'after-init-hook #'global-flycheck-mode)

;; ;; ;; Line to indicate column limit for program lines
;; ;; (message "Loading fill-column-indicator")
;; ;; (use-package fill-column-indicator
;; ;;   :init
;; ;;   (progn
;; ;;     (setq fci-rule-column 140)
;; ;;     (setq fci-handle-truncate-lines nil)
;; ;;     (define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
;; ;;     (global-fci-mode 1)
;; ;;     (defun auto-fci-mode (&optional unused)
;; ;;       (if (> (window-width) fci-rule-column)
;; ;;           (fci-mode 1)
;; ;;         (fci-mode 0))
;; ;;       )
;; ;;     (add-hook 'after-change-major-mode-hook 'auto-fci-mode)
;; ;;     (add-hook 'window-configuration-change-hook 'auto-fci-mode)))


;; ;; ;; which-key is a minor mode for Emacs that displays the key
;; ;; ;; bindings following your currently entered incomplete command (a
;; ;; ;; prefix) in a popup. For example, after enabling the minor mode if
;; ;; ;; you enter C-x and wait for the default of 1 second the minibuffer
;; ;; ;; will expand with all of the available key bindings that follow
;; ;; ;; C-x (or as many as space allows given your settings).
(message "Loading which-key")
(use-package which-key
  :init
  (which-key-mode)
  :config

  ;; copied from which-key.el to turn off header-line
  (defun which-key--init-buffer ()
    "Initialize which-key buffer"
    (unless (buffer-live-p which-key--buffer)
      (setq which-key--buffer (get-buffer-create which-key-buffer-name))
      (with-current-buffer which-key--buffer
        ;; suppress confusing minibuffer message
        (let (message-log-max)
          (toggle-truncate-lines 1)
          (message ""))
        (setq-local cursor-type nil)
        (setq-local cursor-in-non-selected-windows nil)
        (setq-local mode-line-format nil)
        (setq-local header-line-format nil)
        (setq-local word-wrap nil)
        (setq-local show-trailing-whitespace nil)
        (run-hooks 'which-key-init-buffer-hook))))

  (setq which-key-side-window-max-height 0.5
        which-key-show-prefix 'modeline
        which-key-min-display-lines 5)
  )


;; ;; ;;smex - A smarter M-x completion ------------
;; ;; ;; (message "Loading smex")
;; ;; ;; (use-package smex
;; ;; ;;   :init
;; ;; ;;   (progn
;; ;; ;;       (smex-initialize)
;; ;; ;;       (global-set-key (kbd "M-x") 'smex)
;; ;; ;;       (global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; ;; ;;       (setq smex-save-file (concat marcel-lisp-dir "smex-items-" machine-nickname)))         ; don't save state to "~/.smex-items"
;; ;; ;;   ;;(icomplete-mode t)
;; ;; ;;   )
;; ;; ;;---------------------------------------------



;; ;; ;; don't let the cursor go into minibuffer prompt
;; ;; (setq minibuffer-prompt-properties (quote (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))

(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

;; ;; ;;(use-package autopair)
;; ;; ;;(autopair-global-mode) ;; to enable in all buffers


(message "Loading rainbow-delimiters")
(use-package rainbow-delimiters
  :init
  (rainbow-delimiters-mode-enable))

;; ;; ;;Rectangular markings-----------------------
;; ;; ;;COOL! C-RET gives rectangular marking for copy/paste, extremely useful
;; ;; ;;for tables. NOTE, second line needed for rectangle, but also gives
;; ;; ;; (transient-mark-mode t) = visualize C-SPC-marking (i.e. highlight)
;; ;; (setq cua-enable-cua-keys nil) ;;only for rectangle, don't use C-x/c/v for copy/paste
;; ;; (cua-mode t)                   ;;gives rectangle + same as "(pc-selection-mode)" (=shift+arrow highlights)
;; ;; ;;--------------------------------------------

;; ;; ;; Clipboard
;; ;; ;; Use the clipboard, pretty please, so that copy/paste "works"
(setq x-select-enable-clipboard t)

;; Merge system's and Emacs' clipboard
(setq select-enable-clipboard t)
;; Save whatever’s in the current (system) clipboard before
;; replacing it with the Emacs’ text.
;; https://github.com/dakrone/eos/blob/master/eos.org
(setq save-interprogram-paste-before-kill t)




;; ;; ;; Navigate windows with M-<arrows>
;; ;; (windmove-default-keybindings 'meta)
;; ;; (setq windmove-wrap-around t)

;; ;; ;; winner-mode provides C-<left> to get back to previous window layout
;; ;; (winner-mode 1)


(setq linum-format " %d ")
;; ;; ;; To make emacs use spaces instead of tabs (Added by Art Lee on 2/19/2008)
(setq-default indent-tabs-mode nil)
;; ;; (setq mail-default-reply-to "becker@kestrel.edu")
(display-time)
(add-hook 'before-save-hook 'time-stamp)
(setq minibuffer-max-depth nil)

(add-hook 'json-mode-hook
          (lambda ()
            (setq js-indent-level 2)))

;; ;; ;;;Answer y or n instead of yes or no at minibar prompts.
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


;; An all-in-one comment command to rule them all
(use-package comment-dwim-2
  :bind ("M-;" . comment-dwim-2))

;; Drag stuff (lines, words, region, etc...) around
(use-package drag-stuff
  :ensure t
  :diminish "DRG"
  :commands drag-stuff-define-keys
  :hook (after-init . drag-stuff-global-mode)
  :config
  (add-to-list 'drag-stuff-except-modes 'org-mode)
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))


;; Edit multiple regions in the same way simultaneously
(use-package iedit
  :defines desktop-minor-mode-table
  :bind (("C-;" . iedit-mode)
         ("C-x r RET" . iedit-rectangle-mode)
         :map isearch-mode-map ("C-;" . iedit-mode-from-isearch)
         :map esc-map ("C-;" . iedit-execute-last-modification)
         :map help-map ("C-;" . iedit-mode-toggle-on-function))
  :config
  ;; Avoid restoring `iedit-mode'
  (with-eval-after-load 'desktop
    (add-to-list 'desktop-minor-mode-table
                 '(iedit-mode nil))))

;; Multiple cursors
(use-package multiple-cursors
  :bind (("C-S-c C-S-c"   . mc/edit-lines)
         ("C->"           . mc/mark-next-like-this)
         ("C-<"           . mc/mark-previous-like-this)
         ("C-c C-<"       . mc/mark-all-like-this)
         ("C-M->"         . mc/skip-to-next-like-this)
         ("C-M-<"         . mc/skip-to-previous-like-this)
         ("s-<mouse-1>"   . mc/add-cursor-on-click)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)
         :map mc/keymap
         ("C-|" . mc/vertical-align-with-space)))

;; Smartly select region, rectangle, multi cursors
(use-package smart-region
  :hook (after-init . smart-region-on))

;; Goto last change
(use-package goto-chg
  :bind ("C-," . goto-last-change))


;; Framework for mode-specific buffer indexes
(use-package imenu
  :ensure nil
  :bind (("C-." . imenu)))

;; Windows-scroll commands
;;( use-package pager
;;  :bind (("\C-v"   . pager-page-down)
;;         ([next]   . pager-page-down)
;;         ("\ev"    . pager-page-up)
;;         ([prior]  . pager-page-up)
;;         ([M-up]   . pager-row-up)
;;         ([M-kp-8] . pager-row-up)
;;         ([M-down] . pager-row-down)
;;         ([M-kp-2] . pager-row-down))
;;)

;; Discover key bindings and their meaning for the current Emacs major mode
(use-package discover-my-major
  :bind (("C-h M-m" . discover-my-major)
         ("C-h M-M" . discover-my-mode)))


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
(use-package swiper)
(use-package ivy :diminish "")
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(global-set-key (kbd "C-c C-s") 'swiper-helm)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
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


;; Iterate through CamelCase words
(global-subword-mode 1)
(diminish 'subword-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; neotree
(message "Loading and configuring neotree")

(use-package all-the-icons)
(use-package neotree)

(global-set-key [f8] 'neotree-toggle)


(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (ffip-project-root))
        (file-name (buffer-file-name)))
    (if project-dir
        (progn
          (neotree-dir project-dir)
          (neotree-find file-name))
      (message "Could not find git project root."))))

(global-set-key (kbd "C-c C-p") 'neotree-project-dir)
;; every time when the neotree window is  opened, it will try to find current
;; file and jump to node.
(setq-default neo-smart-open t)
;; change root automatically when running `projectile-switch-project`
(use-package projectile :ensure t :diminish "PRJ")
(setq projectile-switch-project-action 'neotree-projectile-action)
(setq neo-theme (if window-system 'icons 'nerd)) ; 'classic, 'nerd, 'ascii, 'arrow
(setq neo-vc-integration '(face char))
(setq neo-show-hidden-files t)
(setq neo-toggle-window-keep-p t)
(setq neo-force-change-root t)
(setq neo-window-fixed-size nil)
(setq neo-window-width 50)

(add-hook 'neotree-mode-hook (lambda () (setq-local mode-line-format nil)))

(defun neotree-resize-window (&rest _args)
  "Resize neotree window.
https://github.com/jaypei/emacs-neotree/pull/110"
  (interactive)
  (neo-buffer--with-resizable-window
   (let ((fit-window-to-buffer-horizontally t))
     (fit-window-to-buffer))))

(add-hook 'neo-change-root-hook #'neotree-resize-window)
(add-hook 'neo-enter-hook #'neotree-resize-window)


;; (defun set-window-number-0 ()
;;   "Custom number assignment for special buffers."
;;   (mapc (lambda (w)
;;           (when (and (boundp 'neo-global--window)
;;                      (eq w neo-global--window))
;;             (window-numbering-assign w 0)))
;;         (window-list)))
;; (add-hook 'window-numbering-before-hook 'set-window-number-0)
;; (setq window-numbering-auto-assign-0-to-minibuffer nil)
;; (set-window-number-0)


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

;; https://stackoverflow.com/questions/88399/how-do-i-duplicate-a-whole-line-in-emacs
(defun duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")
  ;; save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))
  ;; local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion
      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))
      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; insert the line arg times
        (while (> count 0)
          (newline)         ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count))))
      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
    ) ; end-of-let
  ;; put the point in the lowest line and return
  (next-line arg))

;; http://emacsredux.com/blog/2013/04/02/move-current-line-up-or-down/
(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

;; http://emacsredux.com/blog/2013/04/02/move-current-line-up-or-down/
(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key (kbd "M-A-<up>") 'duplicate-line)
(global-set-key (kbd "M-A-<down>") 'duplicate-line)
(global-set-key (kbd "A-<up>") 'move-line-up)
(global-set-key (kbd "A-<down>") 'move-line-down)

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
(autoload 'sgml-mode "psgml" "Major mode to edit SGML files." t)
(autoload 'xml-mode "psgml" "Major mode to edit XML files." t)



;;(use-package python)
;;(autoload 'python-mode "python-mode" "Python editing mode." t)
;;(add-to-list 'interpreter-mode-alist '("python" . python-mode))

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

(define-key global-map [S-f1]
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


;; ;; (autoload 'auto-make-header "header2")
;; ;; (require 'my-python-header)

;; Load the font-lock package.
;;(use-package font-lock)
;; Maximum colors
(setq font-lock-maximum-decoration t)
;; Turn on font-lock in all modes that support it
(global-font-lock-mode t)

(tool-bar-mode -1)



;;(setq semantic-load-turn-everything-on t)


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

;;(use-package graphene)
;;(use-package project-persist-drawer)
;;(use-package ppd-sr-speedbar) ;; or another adaptor
;;(project-persist-drawer-mode t)

;;(message "Loading sr-speedbar")
;;(use-package sr-speedbar)
;;(setq speedbar-use-images t)
;;(global-set-key [C-f4] 'sr-speedbar-toggle)
;;(global-set-key (kbd "<f4>") 'sr-speedbar-select-window)
;;(setq speedbar-directory-unshown-regexp "^\\(CVS\\|RCS\\|SCCS\\|\\.\\.*$\\)\\'")
;;(setq speedbar-directory-unshown-regexp  "^\\(\\.*\\)\\'")

(setq-default ispell-program-name
              (cond (running-ms-windows
                     ;;"c:/Program Files/Aspell6/x64/bin/aspell.exe"
                     "aspell")
                    (running-macos
                     "/usr/local/bin/aspell")
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
(use-package flyspell
  :ensure t
  :defer t
  :diminish "")


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
;;  (use-package cygwin-mount nil t))
;;  (setenv "PATH" (concat "c:/cygwin/bin;" (getenv "PATH")))
;;  (setq exec-path (cons "c:/cygwin/bin/" exec-path))
;;    (use-package setup-cygwin)
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
      (use-package cygwin-mount
        :init
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


;; (use-package shell-command)
;; (shell-command-completion-mode)


;; And bury the scratch buffer, don't kill it
(defadvice kill-buffer (around kill-buffer-around-advice activate)
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
      ad-do-it)))


(set-default 'mode-line-buffer-identification
             (list (concat "Emacs[" machine-nickname "]: %17b")))


(defun my-load-modeline ()
  (interactive)
  (message "Loading telephone line mode line")
  ;;(load-file (concat marcel-lisp-dir  "becker-mode-line-evil-mode.el"))
  (load-file (concat marcel-lisp-dir  "telephone-line-mode-line.el"))
  (message "Loading header line")
  (load-file (concat marcel-lisp-dir  "header-line.el"))
  ;;(winum--install-mode-line)
  )

(message "Loading mode line")
(my-load-modeline)


(setq frame-title-format
      '("EMACS: [" (:eval (getenv "USERNAME")) "@"
        (:eval (downcase (system-name))) "]: "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b")) " [%*]"))



;; (setq-default header-line-format
;;               (list '(:eval (concat
;;                              (propertize " " 'display '((space :align-to (- right-fringe 16))))
;;                              display-time-string))))

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
;;(use-package tex)
;;(use-package latex)
;;(use-package auctex)
;;(use-package auto-complete-auctex :defer t)
(use-package company-auctex :defer t)
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
      '(;;("SumatraPDF" "\"C:/Program Files (x86)/SumatraPDF/SumatraPDF.exe\" -reuse-instance %o")
        ;;("Okular" "okular --unique %o#src:%n%b")
        ("Skim" "/Applications/TeX/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))
;;("Skim" "/Applications/TeX/Skim.app/Contents/SharedSupport/displayline %q")))


(add-hook 'LaTeX-mode-hook
          (lambda ()
            (push
             '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
               :help "Run latexmk on file")
             TeX-command-list)))
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))


(setq TeX-view-program-selection
      (cond (running-ms-windows
             '((output-pdf "SumatraPDF")
               (output-dvi "Yap")))
            (running-linux
             '((output-pdf "Okular")
               (output-dvi "Okular")))
            (running-macos
             '((output-pdf "Skim")))))


(defun my-load-latex ()
  (interactive)
  (let* ((latex-setup (concat marcel-lisp-dir  "latex-init.el")))
    (when (file-exists-p latex-setup)
      (load-file latex-setup))))
(my-load-latex)
(latex-preview-pane-enable)

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
    (use-package sumatra-forward))

(when running-macos
  (setenv "PATH" (concat (getenv "PATH") ":/usr/texbin"))
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
  (setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin"))
  (setq exec-path (append exec-path '("/usr/texbin")))
  (setq exec-path (append exec-path '("/Library/TeX/texbin")))
  (setq exec-path (append exec-path '("/usr/local/bin"))))

;; ;; ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;[ACL2.0 Stuff]

;; ;; (setq *acl2-interface-dir* "D:/ACL2-4.3/acl2-sources/interface/emacs/")
;; ;; (setq inferior-acl2-program "sbcl --core d:/ACL2-4.3/acl2-sources/saved_acl2.core")
;; ;; (autoload 'run-acl2 ;;emacs 19.27 only at this time
;; ;;   (concat *acl2-interface-dir* "top-start-inferior-acl2")
;; ;;   "Begin ACL2 in an inferior ACL2 mode buffer."
;; ;;   t)



;; ;; ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; [ CTAGS ]

(use-package ctags-update)
(ctags-auto-update-mode 1)
(setq path-to-ctags
      (if running-ms-windows
          "etags.exe"
        "/usr/local/bin/ctags"))

(setq default-tags-file
      (if running-ms-windows "D:/Source/TAGS" (expand-file-name "~/src/TAGS")))
(setq tags-table-list (list default-tags-file (expand-file-name "~/src/rspace-eclipse/scharp/TAGS")
                            ))

(defun my-create-ctags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s --append -f %s -R %s" path-to-ctags default-tags-file (directory-file-name dir-name))))


(defun create-etags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (eshell-command
   (format "find %s -type f -name \"*.java\" | etags -" dir-name)))

(message "Loading doremi")
(use-package hexrgb)
(require 'doremi)
(require 'doremi-frm)
(require 'doremi-cmd)



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

(setq split-height-threshold 0)
(setq split-width-threshold nil)

;; C-x h runs the command mark-whole-buffer
;; C-M-\ runs the command indent-region
;; You can also insert something like:
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

;; (let ((menu-bar+-lib (concat marcel-lisp-dir "el-get/menu-bar+")))
;;   (when (file-exists-p menu-bar+-lib)
;;     (add-to-list 'load-path menu-bar+-lib)
;;     (eval-after-load "menu-bar" '(use-package menu-bar+))))


(message "Loading time-stamp")
(use-package time-stamp
  :config
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



;; ;; (let ((lilypond-lib "c:/Program Files (x86)/LilyPond/usr/share/emacs/site-lisp"))
;; ;;   (when (file-exists-p lilypond-lib)
;; ;;     (add-to-list 'load-path lilypond-lib)
;; ;;     (autoload 'LilyPond-mode "lilypond-mode" "LilyPond Editing Mode" t)
;; ;;     (add-to-list 'auto-mode-alist '("\\.ly$" . LilyPond-mode))
;; ;;     (add-to-list 'auto-mode-alist '("\\.ily$" . LilyPond-mode))
;; ;;     (add-hook 'LilyPond-mode-hook (lambda () (turn-on-font-lock)))))



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

(use-package company-emacs-eclim
  :config
  (company-emacs-eclim-setup))

(use-package ansi-color
  :config
  (defun colorize-compilation-buffer ()
    (toggle-read-only)
    (ansi-color-apply-on-region compilation-filter-start (point))
    (toggle-read-only))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))

(use-package eclim
  :defer t
  :config
  (require 'eclimd)
  (setq eclimd-autostart t)
  (setq company-eclim-auto-save t)
  (setq company-eclim-executable "/Applications/eclipse-photon/Eclipse.app/Contents/Eclipse/plugins/org.eclim_2.8.0/bin/eclim")
  (setq eclim-eclipse-dirs '("/Applications/eclipse-photon/Eclipse.app/Contents/Eclipse"))
  (setq eclim-executable "/Applications/eclipse-photon/Eclipse.app/Contents/Eclipse/plugins/org.eclim_2.8.0/bin/eclim")
  (defun my-java-mode-init ()
    (eclim-mode t)
    (setq company-backend 'company-eclim))
  (add-hook 'java-mode-hook 'my-java-mode-init))



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


;; =================================================
;; SET COLOR THEME FOR A FRAME OR BUFFER ONLY

;;(use-package color-theme)
;; set default color theme
;;(color-theme-blue-sea)
;; create some frames with different color themes
;;(let ((color-theme-is-global nil))
;;  (select-frame (make-frame))
;;  (color-theme-gnome2)
;;  (select-frame (make-frame))
;;  (color-theme-standard))

;;(use-package color-theme-buffer-local)
;;(add-hook 'java-mode
;;          (lambda nil (color-theme-buffer-local 'color-theme-robin-hood (current-buffer))))

;;(color-theme-select)


  ;;;; This snippet enables lua-mode
;; This line is not necessary, if lua-mode.el is already on your load-path


;; ;; (autoload 'lua-mode "lua-mode" "Lua editing mode." t)
;; ;; (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
;; ;; (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

(message "Loading docker-mode")
(use-package dockerfile-mode :ensure t :defer t)
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


(defun my-extended-keytable (arg)
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
           (modifiers (list "" "S-" "s-" "C-" "C-S-" "M-" "M-C-" "M-S-" "A-"))
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

;;; https://emacs.stackexchange.com/questions/653/how-can-i-find-out-in-which-keymap-a-key-is-bound
(defun my-locate-key-binding (key)
  "Determine in which keymap KEY is defined."
  (interactive "kPress key: ")
  (let ((ret
         (list
          (key-binding-at-point key)
          (minor-mode-key-binding key)
          (local-key-binding key)
          (global-key-binding key))))
    (when (called-interactively-p 'any)
      (message "At Point: %s\nMinor-mode: %s\nLocal: %s\nGlobal: %s"
               (or (nth 0 ret) "")
               (or (mapconcat (lambda (x) (format "%s: %s" (car x) (cdr x)))
                              (nth 1 ret) "\n             ")
                   "")
               (or (nth 2 ret) "")
               (or (nth 3 ret) "")))
    ret))

(defun key-binding-at-point (key)
  (mapcar (lambda (keymap) (when (keymapp keymap)
                             (lookup-key keymap key)))
          (list
           ;; More likely
           (get-text-property (point) 'keymap)
           (mapcar (lambda (overlay)
                     (overlay-get overlay 'kleymap))
                   (overlays-at (point)))
           ;; Less likely
           (get-text-property (point) 'local-map)
           (mapcar (lambda (overlay)
                     (overlay-get overlay 'local-map))
                   (overlays-at (point))))))

(defun my-keymaps-at-point ()
  "List entire keymaps present at point."
  (interactive)
  (let ((map-list
         (list
          (mapcar (lambda (overlay)
                    (overlay-get overlay 'keymap))
                  (overlays-at (point)))
          (mapcar (lambda (overlay)
                    (overlay-get overlay 'local-map))
                  (overlays-at (point)))
          (get-text-property (point) 'keymap)
          (get-text-property (point) 'local-map))))
    (apply #'message
           (concat
            "Overlay keymap: %s\n"
            "Overlay local-map: %s\n"
            "Text-property keymap: %s\n"
            "Text-property local-map: %s")
           map-list)))


;;; Buffer scrolling
(message "Loading smooth-scroll")
(use-package smooth-scroll)
(setq redisplay-dont-pause t)
(setq  scroll-margin 3)
(setq  scroll-step 1)
(setq  scroll-conservatively 10000)
(setq  scroll-preserve-screen-position 1)
(setq auto-window-vscroll nil)
;;(setq  smooth-scroll/vscroll-step-size 1)
;;(smooth-scroll-mode 1)


(message "Loading rainbow-delimiters")
;; Highlight brackets according to their depth
(use-package rainbow-delimiters :ensure t :diminish nil)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)


;; Colorize color names in buffers
(use-package rainbow-mode
  :diminish "RNB")



;; ;; ;;; init.el ends here
;; ;; (message "Loading tramp")
;; ;; (use-package tramp)
;; ;; (defun my-connect-remote ()
;; ;;   (interactive)
;; ;;   (dired "/ubuntu@10.130.2.77:/home/ubuntu/src")
;; ;;   ;;(dired "/becker-openstack:/home/ubuntu/src")
;; ;;   )

;; ;; (defun my-connect-ptt ()
;; ;;   (interactive)
;; ;;   (dired "/DoD_Admin@50.225.83.4#422:/home/DoD_Admin/becker")
;; ;;   )


;; ;; (setq tramp-default-method "ssh")


(message "Loading browse-kill-ring")
(use-package browse-kill-ring
  :init
  (progn
    (browse-kill-ring-default-keybindings))
  (global-set-key "\C-cy" '(lambda ()
                             (interactive)
                             (popup-menu 'yank-menu))))

;; ;; (message "Loading hexgrb")

;;(message "Loading one-key")
;;(use-package one-key)
;;(message "Loading one-key-dir")
;;(use-package one-key-dir)
;;(message "Loading one-key-yas")
;;(use-package one-key-yas)
;;(use-package one-key-bmkp)
;;(global-set-key (kbd "C-<f5>") 'one-key-open-associated-menu-set)

(message "Loading find-file-in-project")
(use-package find-file-in-project)
(global-set-key (kbd "C-c M-f") 'find-file-in-project)

;;(use-package spacemacs-dark-theme)
;; (use-package powerline)

;; "Loading custom file")
(setq custom-file (concat marcel-lisp-dir "custom.el"))
(load custom-file 'noerror)




;;No bells and no visible “bell” either!
(setq visible-bell nil) ;; The default
(setq ring-bell-function 'ignore)
;; Silence warnings generated by a function's being redefine by =defadvice=.
(setq ad-redefinition-action 'accept)

;;No startup message
;; Change     the echo message
(defun display-startup-echo-area-message ()
  (message ""))


(defun open-dir-in-iterm ()
  "Open the current directory of the buffer in iTerm."
  (interactive)
  (let* ((iterm-app-path "/Applications/iTerm.app")
         (iterm-brew-path "/opt/homebrew-cask/Caskroom/iterm2/2.1.4/iTerm.app")
         (iterm-path (if (file-directory-p iterm-app-path)
                         iterm-app-path
                       iterm-brew-path)))
    (shell-command (concat "open -a " iterm-path " ."))))
(global-set-key (kbd "C-x t") 'open-dir-in-iterm)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GIT STUFF
(message "Loading git stuff")
(use-package git-gutter+
  :ensure t
  :init (global-git-gutter+-mode)
  :config (progn
            (define-key git-gutter+-mode-map (kbd "C-x n") 'git-gutter+-next-hunk)
            (define-key git-gutter+-mode-map (kbd "C-x p") 'git-gutter+-previous-hunk)
            (define-key git-gutter+-mode-map (kbd "C-x v =") 'git-gutter+-show-hunk)
            (define-key git-gutter+-mode-map (kbd "C-x r") 'git-gutter+-revert-hunks)
            (define-key git-gutter+-mode-map (kbd "C-x t") 'git-gutter+-stage-hunks)
            (define-key git-gutter+-mode-map (kbd "C-x c") 'git-gutter+-commit)
            (define-key git-gutter+-mode-map (kbd "C-x C") 'git-gutter+-stage-and-commit)
            (define-key git-gutter+-mode-map (kbd "C-x C-y") 'git-gutter+-stage-and-commit-whole-buffer)
            (define-key git-gutter+-mode-map (kbd "C-x U") 'git-gutter+-unstage-whole-buffer))
  :diminish (git-gutter+-mode . "GUT"))

(setq git-gutter+-hide-gutter t)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EDIFF SETUP

(if (locate-library "ediff")
    (progn
      (autoload 'ediff-files "ediff")
      (autoload 'ediff-buffers "ediff")

      (eval-after-load "ediff" '(progn
                                  (message "doing ediff customisation")
                                  (setq diff-switches               "-u"
                                        ediff-custom-diff-options   "-U3"
                                        ediff-split-window-function 'split-window-horizontally
                                        ediff-window-setup-function 'ediff-setup-windows-plain)

                                  (add-hook 'ediff-startup-hook 'ediff-toggle-wide-display)
                                  (add-hook 'ediff-cleanup-hook 'ediff-toggle-wide-display)
                                  (add-hook 'ediff-suspend-hook 'ediff-toggle-wide-display)))))


(defun my-package-reinstall-activated ()
  "Reinstall all activated packages."
  (interactive)
  (dolist (package-name package-activated-list)
    (when (package-installed-p package-name)
      (unless (ignore-errors                   ;some packages may fail to install
                (package-reinstall package-name)
                (warn "Package %s failed to reinstall" package-name))))))


;;;;;;;;;;;;;;;;;;;;;;



;; (display-time-mode)
;; (use-package smart-mode-line)
;; (use-package smart-mode-line-powerline-theme)
;; (setq powerline-arrow-shape 'curve)
;; (setq powerline-default-separator-dir '(right . left))
;; (setq powerline-default-separator 'arrow)
;; (setq sml/theme 'powerline)
;; (setq sml/mode-width 0)
;; (setq sml/name-width 20)
;; (rich-minority-mode 1)
;; (setf rm-blacklist "")
;; (sml/setup)


;;(use-package spaceline-config)
;;(spaceline-spacemacs-theme)

;;(use-package powerline)
;;(powerline-default-theme)
;;(powerline-center-theme)
;; (powerline-center-evil-theme)
;; (powerline-vim-theme)
;; (powerline-nano-theme)

;; (set-face-attribute 'mode-line nil
;;                     :foreground "Black"
;;                     :background "DarkOrange"
;;                     :weight 'bold
;;                     :box nil)

(my-load-helm)
(my-load-treemacs)
(my-load-bookmarks)


(use-package outline-magic)
;; (add-hook 'outline-mode-hook
;;           (lambda ()
;;             (require 'outline-cycle)))

(add-hook 'outline-minor-mode-hook
          (lambda ()
            (require 'outline-magic)
            (define-key outline-minor-mode-map [(f10)] 'outline-cycle)))



(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)


(diminish 'eldoc-mode "")


;; Use this to print all fonts
(defun my-print-all-fonts ()
  (let ((str "The quick brown fox jumps over the lazy dog ´`''\"\"1lI|¦!Ø0Oo{[()]}.,:; ")
        (font-families (cl-remove-duplicates
                        (sort (font-family-list)
                              (lambda(x y) (string< (upcase x) (upcase y))))
                        :test 'string=)))
    (dolist (ff font-families)
      (insert
       (propertize str 'font-lock-face `(:family ,ff))               ff "\n"
       (propertize str 'font-lock-face `(:family ,ff :slant italic)) ff "\n"))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; visible mark - show where mark is                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defface visible-mark-active ;; put this before (require 'visible-mark)
  '((((type tty) (class mono)))
    (t (:background "magenta"))) "")
(use-package visible-mark
  :ensure t
  :config
  (global-visible-mark-mode -1) ;; or add (visible-mark-mode) to specific hooks
  (setq visible-mark-max 2)
  (setq visible-mark-faces `(visible-mark-face1 visible-mark-face2)))


(use-package interaction-log
  :config
  (interaction-log-mode +1)
  (defun open-interaction-log ()
    (interactive)
    (display-buffer ilog-buffer-name))
  (bind-key "A-l" 'open-interaction-log))


(setq paradox-github-token '76d271dd2c6e2f893557ba978663af6cc65d3087)
