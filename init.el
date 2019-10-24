;;; Time-stamp: "2019-10-14 Mon 09:10 marcelbecker on beckermac.local"
;;;
;; use this to profile Emacs initialization.
;; ./nextstep/Emacs.app/Contents/MacOS/Emacs -Q -l ~/Dropbox/.emacs.d/profile-dotemacs.el --eval "(setq profile-dotemacs-file (setq load-file-name \"~/Dropbox/.emacs.d/init.el\") marcel-lisp-dir \"~/Dropbox/.emacs.d/\")" -f profile-dotemacs

;;(load-file "profile-dotemacs.el")
;;(profile-dotemacs)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; By default Emacs triggers garbage collection at ~0.8MB which makes
;; startup really slow. Since most systems have at least 64MB of memory,
;; we increase it during initialization.
(setq gc-cons-threshold 100000000)
(add-hook 'after-init-hook #'(lambda ()
                               ;; restore after startup
                               (setq gc-cons-threshold 800000)))
;;(message  (concat "Loading " load-file-name))


;;; PROFILE INIT
;;; FROM: https://www.reddit.com/r/emacs/comments/8eozfl/advanced_techniques_for_reducing_emacs_startup/
;; use to measure load time
(defconst emacs-start-time (current-time))
(setq last-checkpoint-time emacs-start-time)

;; The time since the load began
(defun time-since-load-start()
  (let* ((current (current-time))
         (delta-start  (float-time (time-subtract current emacs-start-time)))
         (delta-load (float-time (time-subtract current last-checkpoint-time))))
    (setq last-checkpoint-time current)
    (list delta-start delta-load)
    ))

;; Use to track load time through file
(defun display-init-load-time-checkpoint (checkpoint)
  (let ((deltas (time-since-load-start)))
    ;;    (message "%s %s %s" deltas (first deltas) (last deltas))
    (message "Loading init %s checkpoint %s Total Time (%.3fs) Load Time (%.3fs)"
             load-file-name checkpoint (car deltas) (cadr deltas))))
(display-init-load-time-checkpoint "Loading init file")


;; Setting the running environment
(defvar running-ms-windows
  (eq system-type 'windows-nt))
(defvar running-macos
  (eq system-type 'darwin))
(defvar running-linux
  (eq system-type 'gnu/linux))


;; key bindings
(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-modifier 'alt)
  (setq mac-right-option-modifier 'super)
  (setq mac-right-command-modifier 'super)
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


(display-init-load-time-checkpoint "Setting up user cache")
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

;;(trace-function 'byte-compile)
;;(trace-function 'byte-compile-file)

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




(display-init-load-time-checkpoint "Starting to setup frame parameters")

(tool-bar-mode -1)
(blink-cursor-mode 0)
(transient-mark-mode t)
(show-paren-mode t)
(line-number-mode 1)
(global-linum-mode 1)
(linum-mode 1)

;; Non-nil means draw block cursor as wide as the glyph under it.
;; For example, if a block cursor is over a tab, it will be drawn as
;; wide as that tab on the display.
(setq x-stretch-cursor t)


(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable which function mode and set the header line to display both the
;; path and the function we're in
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(which-function-mode t)


;; Use native line numbers
;; (when (version<= "26.0.50" emacs-version )
;; ;; native line numbers
;;   (setq display-line-numbers t
;;         display-line-numbers-current-absolute t
;;         display-line-numbers-width 5
;;         display-line-numbers-widen t)
;;   (set-face-attribute 'line-number nil
;;                       :inherit 'default)
;;   (set-face-attribute 'line-number-current-line nil
;;                       :weight 'ultra-bold :inherit 'hl-line)
;;   ;;(global-display-line-numbers-mode)
;;   )

;; highlight current line
(global-hl-line-mode 1)
;; whenever an external process changes a file underneath emacs, and there
;; was no unsaved changes in the corresponding buffer, just revert its
;; content to reflect what's on-disk.
(global-auto-revert-mode 1)
;; Turn on font-lock in all modes that support it
(global-font-lock-mode t)



(setq stack-trace-on-error t)
;;(setq debug-on-error t)
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
;;No bells and no visible “bell” either!
(setq visible-bell nil) ;; The default
(setq ring-bell-function 'ignore)
(setq truncate-lines t)
;; Maximum colors
(setq font-lock-maximum-decoration t)
;; Silence warnings generated by a function's being redefine by =defadvice=.
(setq ad-redefinition-action 'accept)



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



(display-init-load-time-checkpoint "Configuring emacs frame")

(let* ((frame-font (cons 'font default-frame-font))
       (default-height (my-get-default-frame-height))
       (frame-height (cons 'height default-height))
       (frame-width (cons 'width 180))
       (frame-top (cons 'top (my-get-default-y-frame-position)))
       (frame-left (cons 'left (my-get-default-x-frame-position)))
       (bg-color  (if (eq (user-uid) 0) "gray38" "#09223F"))
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

  (set-face-attribute 'default nil :background bg-color :foreground "white")
  ;;(message  "Frame alist %s" initial-frame-alist)
  (arrange-frame 180 (my-get-default-frame-height) (my-get-default-x-frame-position) (my-get-default-y-frame-position))
  )


(display-init-load-time-checkpoint "Finished configuring emacs frame")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ELPA Packages
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(display-init-load-time-checkpoint "Setting up ELPA")
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

(display-init-load-time-checkpoint "Loading use-package")

(setq package-check-signature nil)
(setq package-user-dir (concat marcel-lisp-dir "elpa"))
(add-to-list 'load-path (concat marcel-lisp-dir "elpa"))
(setq package-enable-at-startup nil)
;;(when (not package-archive-contents)
;;(package-refresh-contents))
(when (version< emacs-version "28.0")
  (display-init-load-time-checkpoint "Calling package-initialize")
  (package-initialize)
;;  (package-activate-all)
  (display-init-load-time-checkpoint "Done with package-initialize")
  )


(unless (package-installed-p 'use-package)
  (display-init-load-time-checkpoint "Installing use-package")
  (package-refresh-contents)
  (package-install 'use-package))

(display-init-load-time-checkpoint "Requiring use-package")
(require 'use-package)
(setq use-package-always-ensure t
      use-package-verbose t
      use-package-compute-statistics t)

(defun my-init-benchmark ()
  (display-init-load-time-checkpoint "Loading benchmark-init")
  (use-package benchmark-init
    :ensure t
    :config
    ;; To disable collection of benchmark data after init is done.
    (add-hook 'after-init-hook 'benchmark-init/deactivate))
  )
;; Use this to profile the initialization
;;(my-init-benchmark)

(setq my-elpa-packages
      '(
        ;;ace-flyspell
        ;;ace-jump-helm-line
        ;;ace-jump-mode
        ;;ace-link
        ;;ace-window
        ;;adaptive-wrap
        ;;afternoon-theme
        ;;aggressive-indent
        ;;alect-themes
        ;;alert
        ;;all-the-icons
        ;;ample-regexps
        ;;ample-theme
        ;;ample-zen-theme
        ;;anaconda-mode
        ;;anti-zenburn-theme
        ;;anything
        ;;anzu
        ;;apropospriate-theme
        ;;async
        ;;auctex
        ;;auto-compile
        ;;auto-complete
        ;;auto-complete-auctex
        ;;auto-dictionary
        ;;auto-highlight-symbol
        ;;auto-yasnippet
        ;;autopair
        ;;autothemer
        ;;avy
        ;;badwolf-theme
        ;;bind-key
        ;;bind-map
        ;;birds-of-paradise-plus-theme
        ;;browse-kill-ring
        ;;bubbleberry-theme
        ;;buffer-move
        ;;busybee-theme
        ;;cherry-blossom-theme
        ;;cl-lib
        ;;clean-aindent-mode
        ;;clues-theme
        ;;coffee-mode
        ;;color-theme-modern
        ;;color-theme-sanityinc-solarized
        ;;color-theme-sanityinc-tomorrow
        ;;color-theme-tango
        ;;column-enforce-mode
        ;;company
        ;;company-anaconda
        ;;company-jedi
        ;;company-quickhelp
        ;;company-statistics
        ;;company-tern
        ;;counsel
        ;;csv-mode
        ;; ctable
        ;;cyberpunk-theme
        ;;cython-mode
        ;;dakrone-theme
        ;;darkburn-theme
        ;;darkmine-theme
        ;;darkokai-theme
        ;;darktooth-theme
        ;;dash
        ;;dash-functional
        ;;deferred
        ;;define-word
        ;;diff-hl
        ;;diminish
        ;;dired+
        ;;dired-atool
        ;;dired-avfs
        ;;dired-details
        ;;dired-details+
        ;;dired-dups
        ;;dired-efap
        ;;dired-explorer
        ;;dired-fdclone
        ;;dired-filetype-face
        ;;dired-filter
        ;;dired-hacks-utils
        ;;dired-imenu
        ;;dired-launch
        ;;dired-narrow
        ;;dired-nav-enhance
        ;;dired-open
        ;;dired-quick-sort
        ;;dired-rainbow
        ;;dired-single
        ;;dired-sort
        ;;dired-sort-menu
        ;;dired-sort-menu+
        ;;dired-subtree
        ;;dired-toggle
        ;;dired-toggle-sudo
        ;;diredful
        ;;direx
        ;;direx-grep
        ;;django-theme
        ;;dockerfile-mode
        ;;doremi
        ;;doremi-frm
        ;;doremi-cmd
        ;;dracula-theme
        ;;dumb-jump
        ;;el-get
        ;;elisp-slime-nav
        ;;elpy
        ;;emacs-eclim
        ;;epc
        ;;epl
        ;;escreen
        ;;espresso-theme
        ;;eval-sexp-fu
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
        ;;exec-path-from-shell
        ;;expand-region
        ;;eyebrowse
        ;;f
        ;;fancy-battery
        ;;farmhouse-theme
        ;;fill-column-indicator
        ;;find-file-in-project
        ;;firebelly-theme
        ;;flatland-theme
        ;;flatui-theme
        ;;flx
        ;;flx-ido
        ;;flycheck
        ;;flycheck-pos-tip
        ;;flymake
        ;;flyspell-correct
        ;;flyspell-correct-helm
        ;;frame-cmds
        ;;frame-fns
        ;;fringe-helper
        ;;fuzzy
        ;;gandalf-theme
        ;;gh-md
        ;;git-commit
        ;;git-gutter
        ;;git-gutter+
        ;;git-gutter-fringe
        ;;git-gutter-fringe+
        ;;git-link
        ;;git-messenger
        ;;git-timemachine
        ;;gitattributes-mode
        ;;gitconfig-mode
        ;;gitignore-mode
        ;;gntp
        ;;gnuplot
        ;;golden-ratio
        ;;google-translate
        ;;gotham-theme
        ;;goto-chg
        ;;goto-last-change
        ;;grandshell-theme
        ;;gruber-darker-theme
        ;;gruvbox-theme
        ;;hc-zenburn-theme
        ;;header2
        ;;helm
        ;;helm-ag
        ;;helm-c-yasnippet
        ;;helm-company
        ;;helm-core
        ;;helm-descbinds
        ;;helm-flx
        ;;helm-git
        ;;helm-git-files
        ;;helm-gitignore
        ;;helm-helm-commands
        ;;helm-ls-git
        ;;helm-make
        ;;helm-mode-manager
        ;;helm-package
        ;;helm-projectile
        ;;helm-pydoc
        ;; helm-spotify
        ;;helm-swoop
        ;;helm-themes
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
        ;;hlinum
        htmlize
        hungry-delete
        hy-mode
        ;;hydra
        ;;icicles
        ;;ido-vertical-mode
        ;;idomenu
        ;;iedit
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
        ;;magit
        ;;magit-gitflow
        ;;magit-popup
        ;;majapahit-theme
        ;;makey
        ;;markdown-mode
        ;;markdown-toc
        ;;material-theme
        ;;menu-bar+
        ;;minimal-theme
        mmm-mode
        ;;moe-theme
        ;;molokai-theme
        ;;monochrome-theme
        ;;monokai-theme
        ;;move-text
        ;;  multiple-cursors
        ;;mustang-theme
        ;;mwim
        ;;naquadah-theme
        ;;neotree
        ;;nginx-mode
        ;;niflheim-theme
        ;;noctilux-theme
        nose
        ;;nxml-mode
        ;;obsidian-theme
        ;;occidental-theme
        ;;oldlace-theme
        ;;omtose-phellack-theme
        ;;open-junk-file
        ;;org
        ;;org-bullets
        ;;org-download
        ;;org-plus-contrib
        ;;org-pomodoro
        ;;org-present
        ;;org-projectile
        ;;organic-green-theme
        ;;orgit
        ;;package
        ;;package-build
        packed
        page-break-lines
        paradox
        parent-mode
        ;;pastels-on-dark-theme
        ;;pcache
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
        ;;quelpa
        ;;railscasts-theme
        ;;rainbow-delimiters
        ;;rainbow-mode
        ;;recentf-ext
        ;;redo+
        ;;request
        ;;restart-emacs
        ;;reverse-theme
        ;;s
        ;;seq
        ;;seti-theme
        ;;shell-command
        ;;simple-httpd
        ;;skewer-mode
        ;;        smartparens
        ;;smartrep
        ;;smart-mode-line
        ;;smart-mode-line-powerline-theme
        ;;smeargle
        ;;smooth-scroll
        ;;smooth-scrolling
        ;;smyx-theme
        ;;soft-charcoal-theme
        ;;soft-morning-theme
        ;;soft-stone-theme
        ;;solarized-theme
        ;;soothe-theme
        ;;spacegray-theme
        ;;spaceline
        ;;spaceline-all-the-icons
        ;;spaceline-segments
        ;;spaceline-config
        ;;spacemacs-theme
        ;;spinner
        ;;spotify
        ;;spray
        ;;sr-speedbar
        ;;subatomic-theme
        ;;subatomic256-theme
        ;;sublime-themes
        ;;sunny-day-theme
        ;;swiper
        ;;swiper-helm
        ;;switch-window
        ;;tabbar
        ;;tango-2-theme
        ;;tango-plus-theme
        ;;tangotango-theme
        ;;tao-theme
        ;;tern
        ;;toc-org
        ;;toxi-theme
        ;;tronesque-theme
        ;;twilight-anti-bright-theme
        ;;twilight-bright-theme
        ;;twilight-theme
        ;;ujelly-theme
        ;;unbound
        ;;underwater-theme
        ;;undo-tree
        unfill
        ;;use-package
        ;;use-package-el-get
        uuidgen
        ;;vi-tilde-fringe
        ;;virtualenvwrapper
        ;;vline
        volatile-highlights
        web-beautify
        websocket
        ;;which-key
        ;;window-number
        ;;window-numbering
        ;;winum
        ;;with-editor
        ;;ws-butler
        ;;yaml-mode
        ;;yasnippet
        ;;zen-and-art-theme
        ;;zenburn-theme
        ;;zencoding-mode
        ;;zonokai-theme
        ))




(use-package diminish :ensure t :diminish "")
(use-package paradox)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; EL-GET
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To update el-get packages manually

;; (let ((elget-lib (concat marcel-lisp-dir "el-get/el-get")))
;;   (if (file-exists-p elget-lib)
;;       (add-to-list 'load-path elget-lib)))

;;(message "Loading el-get")
;;(display-init-load-time-checkpoint "Loading el-get")
;; (use-package el-get
;;    :init
;;    (with-current-buffer
;;        (url-retrieve-synchronously
;;      "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
;;      (goto-char (point-max))
;;      (eval-print-last-sexp))
;;   )

;; (unless (require 'el-get nil 'noerror)
;;   (with-current-buffer
;;       (url-retrieve-synchronously
;;        "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
;;     (goto-char (point-max))
;;     (eval-print-last-sexp)))


;; (add-to-list 'el-get-recipe-path
;;              (concat marcel-lisp-dir "el-get/el-get/recipes"))
;; (setq el-get-default-process-sync t
;;       el-get-verbose t)


;;(message "Loading el-get-elpa")
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
;;         (glqobal-set-key (kbd "<C-S-right>")  'buf-move-right)))

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

(display-init-load-time-checkpoint "Loading quelpa")
(use-package quelpa
  :init
  (setq quelpa-self-upgrade-p nil)
  (setq quelpa-update-melpa-p nil))
(use-package quelpa-use-package)

(display-init-load-time-checkpoint "Loading diff+")
(use-package diff+
  :quelpa (diff+ :fetcher wiki))


(display-init-load-time-checkpoint "Loading faces+")
(use-package faces+
  :quelpa (faces+ :fetcher wiki))

(display-init-load-time-checkpoint "Loading frame-fns")
(use-package frame-fns
  :quelpa (frame-fns :fetcher wiki))
(use-package frame-cmds
  :quelpa (frame-cmds :fetcher wiki))


(display-init-load-time-checkpoint "Loading doremi")
(use-package doremi
  :quelpa (doremi :fetcher wiki))
(use-package doremi-cmd
  :quelpa (doremi-cmd :fetcher wiki))
;;(use-package doremi-frm
;;:quelpa (doremi-frm :fetcher wiki))
(use-package doremi-mac
  :quelpa (doremi-mac :fetcher wiki))

(display-init-load-time-checkpoint "Loading menu-bar+")
(use-package menu-bar+
  :quelpa (menu-bar+ :fetcher wiki))


(display-init-load-time-checkpoint "Loading f")
(use-package f :defer t)
(use-package cl-lib :defer t)
(use-package s :defer t)
(use-package seq :defer t)
(use-package shell-command :defer t)



(display-init-load-time-checkpoint "Loading all-the-icons")
(use-package all-the-icons)

;; (setq my-el-get-packages
;;       '(
;;         ;;diff+
;;         ;;dired+
;;         ;;dired-column-widths
;;         ;;dired-details
;;         ;;dired-details+
;;         ;;dired-sort
;;         ;;dired-sort-menu
;;         ;;dired-sort-menu+
;;         doremi
;;         doremi-cmd
;;         doremi-frm
;;         doremi-mac
;;         escreen
;;         facemenu+
;;         faces+
;;         ffap-
;;         file-template
;;         frame-cmds
;;         frame-fns
;;         ;;helm-anything
;;         help-fns+
;;         menu-bar+
;;         pycomplete+
;;         recentf-buffer
;;         ring+
;;         setup-keys
;;         zoom-frm
;;         ))


;; Install new packages and init already installed packages
;;(message  "Initializing el-get packages")
;;(display-init-load-time-checkpoint "Initializing el-get packages")
;;(el-get 'sync my-el-get-packages)
;;(display-init-load-time-checkpoint "Done initializing el-get packages")


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


(display-init-load-time-checkpoint "Loading doom themes")
(defun my-load-doom-themes ()
  (use-package doom-themes
    :config
    ;; Global settings (defaults)
    (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
          doom-themes-enable-italic t) ; if nil, italics is universally disabled

    ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
    ;; may have their own settings.
    ;;  (load-theme 'doom-one t)
    (load-theme 'doom-city-lights t)

    ;; Enable flashing mode-line on errors
    ;;(doom-themes-visual-bell-config)

    ;;   ;; Enable custom neotree theme (all-the-icons must be installed!)
    (doom-themes-neotree-config)
    (doom-themes-treemacs-config)

    ;;   ;; Corrects (and improves) org-mode's native fontification.
    (doom-themes-org-config)
    ))
(my-load-doom-themes)



;;(message "Loading auto-dim-other-buffers")
(display-init-load-time-checkpoint "Loading auto-dim-other-buffers")
(use-package auto-dim-other-buffers
  :diminish " " ;;"DIM"
  :init
  (add-hook 'after-init-hook
            (lambda ()
              (when (fboundp 'auto-dim-other-buffers-mode)
                (auto-dim-other-buffers-mode t)))))



(display-init-load-time-checkpoint "Loading bind-key")
(use-package bind-key)  ;; if you use any :bind variant
(use-package bind-map)

(display-init-load-time-checkpoint "Loading restart-emacs")

(use-package restart-emacs
  :commands restart-emacs)

(display-init-load-time-checkpoint "Loading wgrep")
(use-package wgrep
  :defer t
  :ensure t
  )

(display-init-load-time-checkpoint "Loading wgrep-ag")
(use-package wgrep-ag
  :ensure t
  :defer t)


(display-init-load-time-checkpoint "Loading popwin")

(define-prefix-command 'my-C-Z-key-map)
(bind-key "C-z" my-C-Z-key-map)

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



;; (use-package sublimity)
;; (use-package sublimity-scroll)
;; (use-package sublimity-map)
;; (use-package sublimity-attractive)


(defun my-load-init-file (filename)
  (let* ((init-file (concat marcel-lisp-dir  filename)))
    (when (file-exists-p init-file)
      (load-file init-file))))

(display-init-load-time-checkpoint "Loading dired extensions")
(defun my-load-dired ()
  (interactive)
  (my-load-init-file "init-dired.el"))
(my-load-dired)

(display-init-load-time-checkpoint "Loading hydra definitions")
(defun my-load-hydra ()
  (interactive)
  (my-load-init-file "hydra-init.el"))
(my-load-hydra)


(display-init-load-time-checkpoint "Loading emms definitions")
(defun my-load-emms ()
  (interactive)
  (my-load-init-file "emms-init.el"))
(my-load-emms)


(defun my-load-python ()
  (interactive)
  ;;  (my-load-init-file "elpy-init.el")
  (my-load-init-file "anaconda-init.el"))



(defun my-load-helm ()
  (interactive)
  (my-load-init-file  "helm-init.el"))


(defun my-load-bookmarks ()
  (interactive)
  (my-load-init-file "visual-bookmarks-init.el"))

(defun my-load-evil ()
  (interactive)
  (my-load-init-file "evil-init.el"))

(defun my-load-treemacs ()
  (interactive)
  (my-load-init-file "treemacs-init.el"))


(defun my-load-shackle ()
  (interactive)
  (my-load-init-file "shackle-init.el"))
(my-load-shackle)

(defun my-load-org()
  (interactive)
  (my-load-init-file "org-init.el"))

(defun my-load-language-server ()
  (interactive)
  (my-load-init-file "emacs-lsp-init.el"))



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


(display-init-load-time-checkpoint "Loading vline")
(use-package vline
  :defer t
  :diminish "vl"
  )

;;(require 'col-highlight)
;;(require 'tabbar-extension)

(display-init-load-time-checkpoint "Loading recentf")
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
    (add-to-list 'recentf-exclude "*.aux")
    (add-to-list 'recentf-exclude "*.log")
    (add-to-list 'recentf-exclude "recentf*")
    (add-to-list 'recentf-exclude "*.gz")
    (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
    (use-package recentf-ext)
    ))



;;(use-package recentf-buffer)
;;(use-package redo+)

(display-init-load-time-checkpoint "Loading undo-tree")

;;Graphical undo
(use-package undo-tree
  :defer t
  :commands (undo-tree-undo undo-tree-visualize)
  :diminish " "
  :init
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  (let ((undo-dir (concat user-cache-directory "undo")))
    (setq undo-tree-history-directory-alist '((".*" . ,undo-dir))))
  (global-undo-tree-mode))
(display-init-load-time-checkpoint "Done loading undo tree")

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


(display-init-load-time-checkpoint "Loading savehist")
(use-package savehist
  :init
  (progn
    (setq savehist-file (concat marcel-lisp-dir "savehistory-" machine-nickname))
    (savehist-mode 1)))


(display-init-load-time-checkpoint "Loading unbound")
;; find convenient unbound keystrokes
(use-package unbound)                  ; `M-x describe-unbound-keys'
(use-package free-keys)

(display-init-load-time-checkpoint "Loading switch window")
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


(display-init-load-time-checkpoint "Loading eyebrowse")
;; Easy window config switching
(use-package eyebrowse
  :hook (after-init . eyebrowse-mode))

;; save the place in files
(display-init-load-time-checkpoint "Loading saveplace")
(use-package saveplace
  :init
  (save-place-mode 1)
  (setq save-place-forget-unreadable-files nil)
  (setq save-place-file (concat marcel-lisp-dir "places-" machine-nickname)))
(display-init-load-time-checkpoint "Done loading saveplace")

(display-init-load-time-checkpoint "Loading yasnippet")
(use-package yasnippet
  :diminish (yas-minor-mode . "")
  :init
  ;; (setq yas-snippet-dirs
  ;;       (list (concat marcel-lisp-dir "el-get/yasnippet/snippets")
  ;;             (concat marcel-lisp-dir "el-get/yasnippet-snippets")
  ;;             (concat marcel-lisp-dir "el-get/yasnippets")
  ;;             (concat marcel-lisp-dir "snippets")
  ;;             ))
  (use-package yasnippet-snippets)
  (yas-global-mode 1)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "C-c s") #'yas-expand)
  )


(display-init-load-time-checkpoint "Loading anzu")
(use-package anzu
  :init
  (progn
    (global-anzu-mode +1)
    (set-face-attribute 'anzu-mode-line nil
                        :foreground "yellow" :weight 'bold))
  :config
  (define-key isearch-mode-map [remap isearch-query-replace]  #'anzu-isearch-query-replace)
  (define-key isearch-mode-map [remap isearch-query-replace-regexp] #'anzu-isearch-query-replace-regexp))


;;; Keep the highlight color 10% darker than the default background face
;;; https://emacs.stackexchange.com/questions/9740/how-to-define-a-good-highlight-face
(display-init-load-time-checkpoint "Loading color")
(use-package color)
;;(use-package pallet)



;;(add-hook 'global-hl-line-mode-hook 'my-set-hl-line-color-based-on-theme)



;; dim the ignored part of the file name
(file-name-shadow-mode 1)
;;Line wrap
(global-visual-line-mode)
(setq line-move-visual t) ;; move via visual lines
(diminish 'visual-line-mode "VizLine")


(display-init-load-time-checkpoint "Loading hlinum")
(use-package hlinum
  :init
  (hlinum-activate)
  :config
  (set-face-attribute 'hl-line nil :inherit nil :background "#666666"  :foreground nil :weight 'bold)
  ;;(set-face-attribute 'linum-highlight-face nil :background "#666666")
  (set-face-attribute 'linum-highlight-face nil :inherit 'hl-line :weight 'ultra-bold)

  (defun my-set-hl-line-color-based-on-theme ()
    "Sets the hl-line face to have no foregorund and a background
    that is 10% darker than the default face's background."
    (interactive)
    (set-face-attribute 'hl-line nil
                        :foreground nil
                        :background (color-darken-name (face-background 'default) 10)))


  (defun my-set-hl-line-color-lighten-based-on-theme ()
    "Sets the hl-line face to have no foregorund and a background
    that is 10% darker than the default face's background."
    (interactive)
    (set-face-attribute 'hl-line nil
                        :foreground nil
                        :background (color-lighten-name (face-background 'default) 10)))
  (my-set-hl-line-color-based-on-theme)
  )


(display-init-load-time-checkpoint "Loading expand region")

(use-package expand-region
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

(set-face-attribute 'region nil :background "magenta1" :foreground "#ffffff")


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

(display-init-load-time-checkpoint "Loading company")
(use-package company
  :ensure t
  :diminish " Ⓒ" ;;"CIA"
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
         ("C-g" . company-abort)
         ("<left>" . company-abort)

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
        company-require-match 'never
        company-dabbrev-ignore-case nil
        company-show-numbers t
        company-dabbrev-downcase nil)

  (custom-set-faces
   '(company-preview  ((t (:foreground "dark gray" :underline t))))
   '(company-preview-common ((t (:inherit company-preview))))
   '(company-preview-search ((t (:inherit company-preview :background "yellow"))))
   '(company-scrollbar-bg ((t (:inherit 'company-tooltip :background "gray20" :foreground "black" :weight bold))))
   '(company-scrollbar-fg ((t ( :background "gray40" :foreground "black" :weight bold))))
   '(company-template-field ((t (:background "magenta" :foreground "black"))))
   '(company-tooltip   ((t (:background "light gray" :foreground "black"))))
   '(company-tooltip-annotation ((t (:background "brightwhite" :foreground "black"))))
   '(company-tooltip-annotation-selection ((t (:background "color-253"))))
   '(company-tooltip-common  ((((type x)) (:inherit company-tooltip :weight bold)) (t (:inherit company-tooltip))))
   '(company-tooltip-common-selection  ((((type x)) (:inherit company-tooltip-selection :weight bold)) (t (:inherit company-tooltip-selection))))
   '(company-tooltip-mouse ((t (:foreground "black"))))
   '(company-tooltip-search ((t (:background "brightwhite" :foreground "black"))))
   '(company-tooltip-selection   ((t (:background "steel blue" :foreground "white" :weight bold))))
   '(popup-menu-face     ((t :foreground "red"   :background "#49483E")))
   '(popup-menu-selection-face     ((t :background "#349B8D"   :foreground "#BBF7EF"))))

  (add-hook 'after-init-hook 'global-company-mode)
  (add-to-list 'company-backends 'company-dabbrev t)
  (add-to-list 'company-backends 'company-ispell t)
  (add-to-list 'company-backends 'company-files t)
  (use-package company-shell)
  (add-to-list 'company-backends 'company-shell t)
  (use-package company-anaconda)
  (add-to-list 'company-backends 'company-anaconda t)
  (use-package company-jedi)
  (add-to-list 'company-backends 'company-jedi t)
  (use-package company-tern)
  (add-to-list 'company-backends 'company-tern t)
  (use-package company-statistics)
  (add-to-list 'company-backends 'company-statistics t)
  (global-company-mode)
  ;; (use-package company-posframe
  ;;   :diminish "PosFr"
  ;;   :config
  ;;   (company-posframe-mode 1)
  ;;   )

  ;; (custom-set-faces
  ;;  '(company-tooltip-common
  ;;    ((t (:inherit company-tooltip :weight bold :underline nil))))
  ;;  '(company-tooltip-common-selection
  ;;    ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))


  ;; Popup documentation for completion candidates
  (when (display-graphic-p)
    (use-package company-quickhelp
      :bind (:map company-active-map
                  ("M-h" . company-quickhelp-manual-begin))
      :hook (global-company-mode . company-quickhelp-mode)
      :config
      (company-quickhelp-mode 1)
      (setq company-quickhelp-use-propertized-text t)
      (setq company-quickhelp-color-foreground "white")
      (setq company-quickhelp-color-background "black")
      (setq company-quickhelp-delay 0.4))))


(defun my-load-company-box ()
  (interactive)
  (use-package company-box
    :defer    t
    :diminish " ⓐ" ;;"CiaBox"
    :after (all-the-icons company)
    :hook (company-mode . company-box-mode)
    :functions (my-company-box--make-line
                my-company-box-icons--elisp)
    :commands (company-box--get-color
               company-box--resolve-colors
               company-box--add-icon
               company-box--apply-color
               company-box--make-line
               company-box-icons--elisp)

    :init
    (setq company-box-frame-parameters
          '((left . -1)
            (no-accept-focus . t)
            (no-focus-on-map . t)
            (min-width  . t)
            (min-height  . t)
            (width  . 30)
            (height  . 30)
            (internal-border-width . 5)
            (vertical-scroll-bars . nil)
            (horizontal-scroll-bars . nil)
            (menu-bar-lines . 0)
            (tool-bar-lines . 0)
            (line-spacing . 1)
            ;; (unsplittable . nil)
            (undecorated . t)
            (top . -1)
            (visibility . nil)
            (mouse-wheel-frame . nil)
            (no-other-frame . t)
            (cursor-type . nil)
            (drag-internal-border . t)
            (left-fringe . 5)
            (right-fringe . 5)
            (no-special-glyphs . t)))
    (setq company-box-doc-frame-parameters
          '((internal-border-width . 5)
            (foreground-color . "white")
            (background-color . "black")
            (no-accept-focus . t)
            (no-focus-on-map . t)
            )
          )

    (setq company-box-icons-alist 'company-box-icons-all-the-icons)
    :config
    ;; (setq company-box-backends-colors
    ;;        '((company-lsp      . "#e0f9b5")
    ;;          (company-elisp    . "#e0f9b5")
    ;;          (company-files    . "#ffffc2")
    ;;          (company-keywords . "#ffa5a5")
    ;;          (company-capf     . "#bfcfff")
    ;;          (company-dabbrev  . "#bfcfff"))
    ;;       )
    (company-box--set-frame nil)
    (set-frame-parameter nil 'company-box-doc-frame nil)

    (setq company-box-backends-colors nil
          company-box-show-single-candidate t
          company-box-max-candidates 50
          company-box-doc-delay 0.4)

    (setq company-box-icons-unknown (concat (all-the-icons-material "find_in_page") " "))
    (setq company-box-icons-elisp
          (list
           (concat (all-the-icons-faicon "tag") " ")
           (concat (all-the-icons-faicon "cog") " ")
           (concat (all-the-icons-faicon "cube") " ")
           (concat (all-the-icons-material "color_lens") " ")))
    (setq company-box-icons-yasnippet (concat (all-the-icons-faicon "bookmark") " "))
    (setq company-box-icons-lsp
          `((1 .  ,(concat (all-the-icons-faicon   "text-height")    " ")) ;; Text
            (2 .  ,(concat (all-the-icons-faicon   "tags")           " ")) ;; Method
            (3 .  ,(concat (all-the-icons-faicon   "tag" )           " ")) ;; Function
            (4 .  ,(concat (all-the-icons-faicon   "tag" )           " ")) ;; Constructor
            (5 .  ,(concat (all-the-icons-faicon   "cog" )           " ")) ;; Field
            (6 .  ,(concat (all-the-icons-faicon   "cog" )           " ")) ;; Variable
            (7 .  ,(concat (all-the-icons-faicon   "cube")           " ")) ;; Class
            (8 .  ,(concat (all-the-icons-faicon   "cube")           " ")) ;; Interface
            (9 .  ,(concat (all-the-icons-faicon   "cube")           " ")) ;; Module
            (10 . ,(concat (all-the-icons-faicon   "cog" )           " ")) ;; Property
            (11 . ,(concat (all-the-icons-material "settings_system_daydream") " ")) ;; Unit
            (12 . ,(concat (all-the-icons-faicon   "cog" )           " ")) ;; Value
            (13 . ,(concat (all-the-icons-material "storage")        " ")) ;; Enum
            (14 . ,(concat (all-the-icons-material "closed_caption") " ")) ;; Keyword
            (15 . ,(concat (all-the-icons-faicon   "bookmark")       " ")) ;; Snippet
            (16 . ,(concat (all-the-icons-material "color_lens")     " ")) ;; Color
            (17 . ,(concat (all-the-icons-faicon   "file-text-o")    " ")) ;; File
            (18 . ,(concat (all-the-icons-material "refresh")        " ")) ;; Reference
            (19 . ,(concat (all-the-icons-faicon   "folder-open")    " ")) ;; Folder
            (20 . ,(concat (all-the-icons-material "closed_caption") " ")) ;; EnumMember
            (21 . ,(concat (all-the-icons-faicon   "square")         " ")) ;; Constant
            (22 . ,(concat (all-the-icons-faicon   "cube")           " ")) ;; Struct
            (23 . ,(concat (all-the-icons-faicon   "calendar")       " ")) ;; Event
            (24 . ,(concat (all-the-icons-faicon   "square-o")       " ")) ;; Operator
            (25 . ,(concat (all-the-icons-faicon   "arrows")         " "))) ;; TypeParameter
          ))

  ;; Support `company-common'
  (defun my-company-box--make-line (candidate)
    (-let* (((candidate annotation len-c len-a backend) candidate)
            (color (company-box--get-color backend))
            ((c-color a-color i-color s-color) (company-box--resolve-colors color))
            (icon-string (and company-box--with-icons-p (company-box--add-icon candidate)))
            (candidate-string (concat (propertize (or company-common "") 'face 'company-tooltip-common)
                                      (substring (propertize candidate 'face 'company-box-candidate) (length company-common) nil)))
            (align-string (when annotation
                            (concat " " (and company-tooltip-align-annotations
                                             (propertize " " 'display `(space :align-to (- right-fringe ,(or len-a 0) 1)))))))
            (space company-box--space)
            (icon-p company-box-enable-icon)
            (annotation-string (and annotation (propertize annotation 'face 'company-box-annotation)))
            (line (concat (unless (or (and (= space 2) icon-p) (= space 0))
                            (propertize " " 'display `(space :width ,(if (or (= space 1) (not icon-p)) 1 0.75))))
                          (company-box--apply-color icon-string i-color)
                          (company-box--apply-color candidate-string c-color)
                          align-string
                          (company-box--apply-color annotation-string a-color)))
            (len (length line)))
      (add-text-properties 0 len (list 'company-box--len (+ len-c len-a)
                                       'company-box--color s-color)
                           line)
      line))
  (advice-add #'company-box--make-line :override #'my-company-box--make-line)

  ;; Prettify icons
  (defun my-company-box-icons--elisp (candidate)
    (when (derived-mode-p 'emacs-lisp-mode)
      (let ((sym (intern candidate)))
        (cond ((fboundp sym) 'Function)
              ((featurep sym) 'Module)
              ((facep sym) 'Color)
              ((boundp sym) 'Variable)
              ((symbolp sym) 'Text)
              (t . nil)))))
  (advice-add #'company-box-icons--elisp :override #'my-company-box-icons--elisp)

  (with-eval-after-load 'all-the-icons
    (declare-function all-the-icons-faicon 'all-the-icons)
    (declare-function all-the-icons-material 'all-the-icons)
    (setq company-box-icons-all-the-icons
          `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.9 :v-adjust -0.2))
            (Text . ,(all-the-icons-faicon "text-width" :height 0.85 :v-adjust -0.05))
            (Method . ,(all-the-icons-faicon "cube" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-purple))
            (Function . ,(all-the-icons-faicon "cube" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-purple))
            (Constructor . ,(all-the-icons-faicon "cube" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-purple))
            (Field . ,(all-the-icons-faicon "tag" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-lblue))
            (Variable . ,(all-the-icons-faicon "tag" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-lblue))
            (Class . ,(all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-orange))
            (Interface . ,(all-the-icons-material "share" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-lblue))
            (Module . ,(all-the-icons-material "view_module" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-lblue))
            (Property . ,(all-the-icons-faicon "wrench" :height 0.85 :v-adjust -0.05))
            (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.9 :v-adjust -0.2))
            (Value . ,(all-the-icons-material "format_align_right" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-lblue))
            (Enum . ,(all-the-icons-material "storage" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-orange))
            (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.9 :v-adjust -0.2))
            (Snippet . ,(all-the-icons-material "format_align_center" :height 0.9 :v-adjust -0.2))
            (Color . ,(all-the-icons-material "palette" :height 0.9 :v-adjust -0.2))
            (File . ,(all-the-icons-faicon "file-o" :height 0.9 :v-adjust -0.05))
            (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.9 :v-adjust -0.2))
            (Folder . ,(all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05))
            (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-lblue))
            (Constant . ,(all-the-icons-faicon "square-o" :height 0.9 :v-adjust -0.05))
            (Struct . ,(all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-orange))
            (Event . ,(all-the-icons-faicon "bolt" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-orange))
            (Operator . ,(all-the-icons-material "control_point" :height 0.9 :v-adjust -0.2))
            (TypeParameter . ,(all-the-icons-faicon "arrows" :height 0.85 :v-adjust -0.05))
            (Template . ,(all-the-icons-material "format_align_center" :height 0.9 :v-adjust -0.2)))))
  )

(my-load-company-box)


(display-init-load-time-checkpoint "Loading visual-regex")
;; Replace Strings with Regexes
(use-package visual-regexp
  :defer t
  :bind (("A-%" . vr/replace)
         ("M-%" . vr/query-replace)))

;; ----------------------------------------------------------[Window Number]

(display-init-load-time-checkpoint "Loading winum")
;; (autoload 'window-number-mode "window-number"
;;   "A global minor mode that enables selection of windows according to
;;  numbers with the C-x C-j prefix.  Another mode,
;;  `window-number-meta-mode' enables the use of the M- prefix."
;;   t)

;; (autoload 'window-number-meta-mode "window-number"
;;   "A global minor mode that enables use of the M- prefix to select
;;  windows, use `window-number-mode' to display the window numbers in
;;  the mode-line."
;;   t)

;; (use-package window-number
;;   :init
;;   (window-number-mode 1)
;;   :diminish "WN"
;;   )



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


;; Quickly jump between windows using ace-window, I used it frequently and bind it F1.
(display-init-load-time-checkpoint "Loading ace-window")
(use-package ace-window
  :init
  (global-set-key (kbd "<f1>") 'ace-window)
  (setq aw-scope 'frame))
(display-init-load-time-checkpoint "Loading ace-window")

;; ;; ;; ----------------------------------------------------------[Window Number]


(use-package flycheck
  :diminish " "
  ;;:config
  ;;(global-flycheck-mode 1)
  ;;(add-hook 'after-init-hook #'global-flycheck-mode)
  )

(use-package flycheck-pos-tip)
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
(display-init-load-time-checkpoint "Loading which-key")
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

  ;; (use-package ivy-posframe)
  ;; (use-package which-key-posframe
  ;;   :init
  ;;   (setq which-key-posframe-border-width 10)
  ;;   (set-face-attribute 'which-key-posframe nil :background "purple" :foreground "white")
  ;;   (set-face-attribute 'which-key-posframe-border nil :background "Yellow")
  ;;   :config
  ;;   (which-key-posframe-mode)
  ;;   (setq which-key-posframe-poshandler 'posframe-poshandler-window-bottom-left-corner))
  )

(display-init-load-time-checkpoint "Done loading which-key")

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


;; Highlight brackets according to their depth
(display-init-load-time-checkpoint "Loading rainbow-delimiters")
(use-package rainbow-delimiters
  :ensure t
  :diminish "RainDel"
  :init
  (rainbow-delimiters-mode-enable)
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(rainbow-delimiters-depth-1-face ((t (:foreground "dark orange"))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "deep pink"))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "chartreuse"))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "deep sky blue"))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "yellow"))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "orchid"))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "spring green"))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "sienna1")))))
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))



;; Colorize color names in buffers
(display-init-load-time-checkpoint "Loading rainbow-mode")
(use-package rainbow-mode
  :diminish "RNB")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Origami - Does code folding, ie hide the body of an
;; if/else/for/function so that you can fit more code on your screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package origami
  :ensure t
  :commands (origami-mode)
  :bind (:map origami-mode-map
              ("C-c o :" . origami-recursively-toggle-node)
              ("C-c o a" . origami-toggle-all-nodes)
              ("C-c o t" . origami-toggle-node)
              ("C-c o o" . origami-show-only-node)
              ("C-c o u" . origami-undo)
              ("C-c o U" . origami-redo)
              ("C-c o C-r" . origami-reset)
              )
  :config
  (setq origami-show-fold-header t)
  ;; The python parser currently doesn't fold if/for/etc. blocks, which is
  ;; something we want. However, the basic indentation parser does support
  ;; this with one caveat: you must toggle the node when your cursor is on
  ;; the line of the if/for/etc. statement you want to collapse. You cannot
  ;; fold the statement by toggling in the body of the if/for/etc.
  (add-to-list 'origami-parser-alist '(python-mode . origami-indent-parser))
  :init
  (add-hook 'prog-mode-hook 'origami-mode)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Beacon-mode: flash the cursor when switching buffers or scrolling
;;              the goal is to make it easy to find the cursor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package beacon
  :ensure t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function beacon-mode "beacon.el"))
  :config
  (beacon-mode t))

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
(display-init-load-time-checkpoint "Loading comment-dwim-2")
(use-package comment-dwim-2
  :bind ("M-;" . comment-dwim-2))

;; Drag stuff (lines, words, region, etc...) around
(display-init-load-time-checkpoint "Loading drag-stuff")
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
(display-init-load-time-checkpoint "Loading iedit")
(use-package iedit
  :defines desktop-minor-mode-table
  :bind (("C-c i" . iedit-mode)
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
(display-init-load-time-checkpoint "Loading multiple cursors")
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
(display-init-load-time-checkpoint "Loading smart regions")
(use-package smart-region
  :hook (after-init . smart-region-on))

;; Goto last change
(display-init-load-time-checkpoint "Loading goto last change")
(use-package goto-chg
  :bind ("C-," . goto-last-change)
  :config
  (global-set-key [(control ?.)] 'goto-last-change)
  (global-set-key [(control ?,)] 'goto-last-change-reverse)
  )


;; Framework for mode-specific buffer indexes
(display-init-load-time-checkpoint "Loading imenu")
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


(display-init-load-time-checkpoint "Loading avy")
(use-package avy
  :ensure t
  :bind (("M-s g" . avy-goto-word-1)))


(display-init-load-time-checkpoint "Loading swiper")
(use-package swiper :defer t)
(use-package counsel :defer t)
(use-package swiper-helm)
(display-init-load-time-checkpoint "Done loading swiper")

(display-init-load-time-checkpoint "Loading ivy")

;; To use this, just run ivy-push-view to store the current view,
;; and optionally give it a name (a useful default we be
;; offered). This will then be offered when you switch buffer using
;; ivy-switch-buffer (which you are using automatically if you use
;; ivy-mode). To make these ivy-views appear in your buffer list,
;; you might need to set the option
(use-package ivy
  :diminish ""
  :defer t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (global-set-key (kbd "C-c C-s") 'swiper-helm)
  (global-set-key (kbd "C-c C-r") 'ivy-resume))
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




;; Iterate through CamelCase words
(global-subword-mode 1)
(diminish 'subword-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; neotree
;;(message "Loading and configuring neotree")
;; https://github.com/domtronn/all-the-icons.el#installation
;;(all-the-icons-insert-icons-for 'alltheicon 1)   ;; Prints all the icons for `alltheicon' font set
;;(all-the-icons-insert-icons-for 'octicon 1)   ;; Prints all the icons for the `octicon' family
;;(all-the-icons-insert-icons-for 'fileicon 1)
;; (all-the-icons-insert-icons-for 'wiicon 1)
;; and makes the icons height 10
;;(all-the-icons-insert-icons-for 'faicon 1 0.5) ;; Prints all the icons for the `faicon' family
;; and also waits 0.5s between printing each one
(display-init-load-time-checkpoint "Loading all the icons")
(use-package all-the-icons)


(display-init-load-time-checkpoint "Loading neotree")
(use-package neotree :defer t)
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

(display-init-load-time-checkpoint "Done Loading neotree")

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


(defun duplicate-line-or-region (&optional n)
  "Duplicate current line, or region if active.
    With argument N, make N copies.
    With negative N, comment out original line and use the absolute value."
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region        ;Get region if active, otherwise line
                      (buffer-substring (region-beginning) (region-end))
                    (prog1 (thing-at-point 'line)
                      (end-of-line)
                      (if (< 0 (forward-line 1)) ;Go to beginning of next line, or make a new one
                          (newline))))))
        (dotimes (i (abs (or n 1)))     ;Insert N times, or once if not specified
          (insert text))))
    (if use-region nil                  ;Only if we're working with a line (not a region)
      (let ((pos (- (point) (line-beginning-position)))) ;Save column
        (if (> 0 n)                             ;Comment out original with negative arg
            (comment-region (line-beginning-position) (line-end-position)))
        (forward-line 1)
        (forward-char pos)))))


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


(global-set-key (kbd "M-A-<up>") 'duplicate-line-or-region)
(global-set-key (kbd "M-A-<down>") 'duplicate-line-or-region)
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

(display-init-load-time-checkpoint "Loading programming mode stuff")

(autoload 'c++-mode "cc-mode" "C++ Editing Mode" t)
(autoload 'c-mode "c-mode" "C Editing Mode"   t)
(autoload 'magit-status "magit" nil t)
(autoload 'c++-mode  "cc-mode" "C++ Editing Mode" t)
(autoload 'c-mode    "cc-mode" "C Editing Mode" t)
(autoload 'objc-mode "cc-mode" "Objective-C Editing Mode" t)
(autoload 'sgml-mode "psgml" "Major mode to edit SGML files." t)
(autoload 'xml-mode "psgml" "Major mode to edit XML files." t)


;; set tab distance to something, so it doesn't change randomly and confuse people
(setq c-basic-offset 2)
(setq tab-width 2)

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
              '(("\\.csv$"  . csv-mode))
              '(("\\.py$"  . python-mode))
              ;;'(("\\.py$"  . python-mode))
              '(("\\.py$"  . lsp-mode))
              '(("\\.py$"  . anaconda-mode))
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


(display-init-load-time-checkpoint "Done loading programming mode stuff")

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


(setq ediff-diff-program ; your Lisp system
      (cond (running-ms-windows
             "c:/cygwin/bin/diff")
            (t
             "diff")))

(setq grep-command "grep -i -nH -e -r ")

;;(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
;;(autoload 'flyspell-delay-command "flyspell" "Delay on command." t)


(use-package flyspell
  :ensure t
  :defer t
  :diminish "Spell"
  :config
  (setq ispell-list-command "--list")
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

  (flyspell-mode-off)
  (flyspell-mode 0)
  (autoload 'tex-mode-flyspell-verify "flyspell" "" t)
  (global-set-key (kbd "C-S-<f8>") 'flyspell-mode)
  (global-set-key (kbd "C-M-<f8>") 'flyspell-buffer)
  )
(use-package ace-flyspell)
(use-package flyspell-correct-helm
  :bind ("C-M-;" . flyspell-correct-wrapper)
  :init
  (setq flyspell-correct-interface #'flyspell-correct-helm))
;;(use-package auto-dictionary
;;  :config
;;  (add-hook 'flyspell-mode-hook (lambda () (auto-dictionary-mode 1))))




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

;;(message "Loading mode line")
(display-init-load-time-checkpoint "Loading modeline")
(my-load-modeline)
(display-init-load-time-checkpoint "Done loading modeline")



(setq frame-title-format
      '("EMACS: [" (:eval (getenv "USERNAME")) "@"
        (:eval (downcase (system-name))) "]: "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b")) " [%*]"))

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


(defun my-put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;[Latex/Tex]



;; TO BUILD PDF TOOLS ON MAC:
;; PKG_CONFIG_PATH=/usr/local/opt/libffi/lib/pkgconfig:/usr/local/opt/zlib/lib/pkgconfig:/usr/local/lib/pkgconfig:/opt/X11/lib/pkgconfig \\
;; /Users/marcelbecker/Dropbox/.emacs.d/elpa/pdf-tools-20191007.1436/build/server/autobuild -i /Users/marcelbecker/Dropbox/.emacs.d/elpa/pdf-tools-20191007.1436/
(display-init-load-time-checkpoint "Loading pdf-tools")
(defun my-load-pdf-tools()
  (interactive)
  (my-load-init-file "pdf-tools-init.el"))
(my-load-pdf-tools)

(display-init-load-time-checkpoint "Loading tex")
(defun my-load-latex ()
  (interactive)
  (my-load-init-file "latex-init.el"))
(my-load-latex)


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

(display-init-load-time-checkpoint "Loading buffer-move")
(use-package buffer-move)


;; ;; ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; [ CTAGS ]
(display-init-load-time-checkpoint "Loading ctags update")
(use-package ctags-update
  :diminish "Tags"
  :init
  (ctags-auto-update-mode 1)
  :config
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

  (setq tags-add-tables nil)
  )


;;horizontal-to-vertical
;; Idea and starter code from Benjamin Rutt (rutt.4+news@osu.edu) on comp.emacs
(defun my-rotate-window-horizontal-to-vertical ()
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
(defun my-rotate-window-vertical-to-horizontal ()
  "Switches from a vertical split to a horizontal split."
  (interactive)
  (let ((one-buf (window-buffer (selected-window)))
        (buf-point (point)))
    (other-window 1)
    (delete-other-windows)
    (split-window-vertically)
    (switch-to-buffer one-buf)
    (goto-char buf-point)))


(defun my-rotate-toggle-window-split ()
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


(global-set-key (kbd "C-c |") 'my-rotate-toggle-window-split)
(global-set-key (kbd "C-c \\") 'my-rotate-window-horizontal-to-vertical)
(global-set-key (kbd "C-c /") 'my-rotate-window-vertical-to-horizontal)


;; transpose-frame
;; flop-frame
;; flip-frame
;; rotate-frame
;; rotate-frame-clockwise
;; rotate-frame-anticlockwise
(use-package transpose-frame)


(display-init-load-time-checkpoint "Loading menubar+")
(require 'menu-bar+)


(display-init-load-time-checkpoint "Loading time-stamp")
;;(message "Loading time-stamp")
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

;; (use-package company-emacs-eclim
;;   :config
;;   (company-emacs-eclim-setup))


(display-init-load-time-checkpoint "Loading ansi-color")
(use-package ansi-color
  :config
  (defun colorize-compilation-buffer ()
    (toggle-read-only)
    (ansi-color-apply-on-region compilation-filter-start (point))
    (toggle-read-only))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))
(display-init-load-time-checkpoint "Done loading ansi-color")
;; (use-package eclim
;;   :defer t
;;   :config
;;   (require 'eclimd)
;;   (setq eclimd-autostart t)
;;   (setq company-eclim-auto-save t)
;;   (setq company-eclim-executable "/Applications/eclipse-photon/Eclipse.app/Contents/Eclipse/plugins/org.eclim_2.8.0/bin/eclim")
;;   (setq eclim-eclipse-dirs '("/Applications/eclipse-photon/Eclipse.app/Contents/Eclipse"))
;;   (setq eclim-executable "/Applications/eclipse-photon/Eclipse.app/Contents/Eclipse/plugins/org.eclim_2.8.0/bin/eclim")
;;   (defun my-java-mode-init ()
;;     (eclim-mode t)
;;     (setq company-backend 'company-eclim))
;;   (add-hook 'java-mode-hook 'my-java-mode-init))



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


(display-init-load-time-checkpoint "Loading docker-mode")
(use-package dockerfile-mode :ensure t :defer t
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

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


(defun my-display-minor-mode-key-priority  ()
  "Print out minor mode's key priority.
URL `http://ergoemacs.org/emacs/minor_mode_key_priority.html'
Version 2017-01-27"
  (interactive)
  (mapc
   (lambda (x) (prin1 (car x)) (terpri))
   minor-mode-map-alist))

;;; Buffer scrolling
(display-init-load-time-checkpoint "Loading smooth-scroll")
(use-package smooth-scroll)
(setq redisplay-dont-pause t)
(setq scroll-margin 3)
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq scroll-preserve-screen-position 1)
(setq auto-window-vscroll nil)
;;(setq  smooth-scroll/vscroll-step-size 1)
;;(smooth-scroll-mode 1)

(display-init-load-time-checkpoint "Loading smart-jump")
(use-package smart-jump
  :ensure t
  :config (smart-jump-setup-default-registers))



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

(display-init-load-time-checkpoint "Loading browse-kill-ring")

(use-package browse-kill-ring
  :init
  (progn
    (browse-kill-ring-default-keybindings))
  (global-set-key "\C-cy" '(lambda ()
                             (interactive)
                             (popup-menu 'yank-menu))))



;;(message "Loading one-key")
;;(use-package one-key)
;;(message "Loading one-key-dir")
;;(use-package one-key-dir)
;;(message "Loading one-key-yas")
;;(use-package one-key-yas)
;;(use-package one-key-bmkp)
;;(global-set-key (kbd "C-<f5>") 'one-key-open-associated-menu-set)

(display-init-load-time-checkpoint "Loading find-file-in-project")
(use-package find-file-in-project
  :config
  (global-set-key (kbd "C-c M-f") 'find-file-in-project))

;;(use-package spacemacs-dark-theme)
;; (use-package powerline)

;; "Loading custom file")
(display-init-load-time-checkpoint "Loading custom file")
(setq custom-file (concat marcel-lisp-dir "custom.el"))
(load custom-file 'noerror)


;; No startup message
;; Change the echo message
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

(display-init-load-time-checkpoint "Loading git-gutter")
(defun my-load-gitgutter ()
  (interactive)
  (my-load-init-file "gitgutter-init.el"))
(my-load-gitgutter)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EDIFF SETUP
(display-init-load-time-checkpoint "Loading ediff")
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

(display-init-load-time-checkpoint "Loading helm")
(setq tramp-verbose 6)
;; CTRL-Backspace disables auto-expansion
(my-load-helm)
(display-init-load-time-checkpoint "Done loading helm")




(display-init-load-time-checkpoint "Loading org")
;;(my-load-org-toolkit)
(display-init-load-time-checkpoint "Done loading org stuff")

;; open my Emacs init file
(defun my-open-notes ()
  "Opening `~/Dropbox/EmacsOrg/MarcelNotes.org'"
  (interactive)
  (find-file "~/Dropbox/EmacsOrg/MarcelNotes.org"))
(global-set-key (kbd "<M-S-f3>") 'my-open-notes)




(display-init-load-time-checkpoint "Loading treemacs")
(my-load-treemacs)
(display-init-load-time-checkpoint "Done Loading treemacs")

(display-init-load-time-checkpoint "Loading bookmarks")
(my-load-bookmarks)
(display-init-load-time-checkpoint "Done Loading bookmarks")


(display-init-load-time-checkpoint "Loading language server")
;;(my-load-language-server)
(display-init-load-time-checkpoint "Done Loading language server")



(display-init-load-time-checkpoint "Loading outline-magic")
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



(display-init-load-time-checkpoint "Loading system packages")
(use-package system-packages)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; visible mark - show where mark is                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(display-init-load-time-checkpoint "Loading visible mark")
(defface visible-mark-active ;; put this before (require 'visible-mark)
  '((((type tty) (class mono)))
    (t (:background "magenta"))) "")
(use-package visible-mark
  :ensure t
  :config
  (global-visible-mark-mode -1) ;; or add (visible-mark-mode) to specific hooks
  (setq visible-mark-max 2)
  (setq visible-mark-faces `(visible-mark-face1 visible-mark-face2)))

(display-init-load-time-checkpoint "Loading interaction log")
(use-package interaction-log
  :config
  (interaction-log-mode +1)
  (defun open-interaction-log ()
    (interactive)
    (display-buffer ilog-buffer-name))
  (bind-key "A-l" 'open-interaction-log))
(display-init-load-time-checkpoint "Done loading interaction log")


(setq paradox-github-token '76d271dd2c6e2f893557ba978663af6cc65d3087)


(display-init-load-time-checkpoint "Done loading init file")
(setq inhibit-compacting-font-caches t)


;;(setq spacemacs-start-directory "~/src/emacs-spacemacs/.emacs.d/")
;;(setenv "HOME" "/Users/marcelbecker/src/emacs-spacemacs/")
;;(setq spacemacs-start-directory "~/.emacs.d/")
;;(load-file (concat spacemacs-start-directory "init.el"))




;; Adds letters to helm buffers to assist selection.
;; It does not look good.
;; (use-package ace-jump-helm-line
;;   :after helm
;;   :config
;;   (define-key helm-map (kbd "C-'") 'ace-jump-helm-line)
;;   (setq ace-jump-helm-line-style 'pre)
;;   (setq ace-jump-helm-line-background t)
;;   (setq ace-jump-helm-line-default-action 'select)
;;   (setq ace-jump-helm-line-select-key ?e) ;; this line is not needed
;;   ;; Set the move-only and persistent keys
;;   (setq ace-jump-helm-line-move-only-key ?o)
;;   (setq ace-jump-helm-line-persistent-key ?p)
;;   ;; enable idle execution for `helm-mini'
;;   (ace-jump-helm-line-idle-exec-add 'helm-mini)
;;   ;; enable hints preview
;;   (ace-jump-helm-line-autoshow-mode +1)
;;   ;; use `linum-mode' to show
;;   (setq ace-jump-helm-line-autoshow-mode-use-linum t))

(use-package ace-jump-mode
  :config
  (define-key global-map (kbd "C-c SPC") 'ace-jump-mode))

(use-package ace-link
  :config
  (ace-link-setup-default)
  (define-key org-mode-map (kbd "M-o") 'ace-link-org))

(use-package adaptive-wrap)

(use-package aggressive-indent)

(use-package dockerfile-mode
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

;;https://github.com/immerrr/ample-regexps.el
(use-package ample-regexps)
(use-package async)

;; Highlights symbol under cursor
;; https://github.com/mhayashi1120/auto-highlight-symbol-mode
(use-package auto-highlight-symbol
  :diminish "AHS")


(use-package color-theme-modern
  :config
  (defadvice load-theme
      (before theme-dont-propagate activate)
    (mapc #'disable-theme custom-enabled-themes))

  (setq my-cur-theme nil)
  (defun my-cycle-theme ()
    "Cycle through a list of themes, my-themes"
    (interactive)
    (when my-cur-theme
      (disable-theme my-cur-theme)
      (setq my-themes (append my-themes (list my-cur-theme))))
    (setq my-cur-theme (pop my-themes))
    (load-theme my-cur-theme t))

  ;; Switch to the first theme in the list above
  ;;(my-cycle-theme)

  ;; Bind this to C-t
  (global-set-key (kbd "C-t") 'my-cycle-theme)
  )

(use-package column-enforce-mode
  :diminish "COL"
  :config
  (setq column-enforce-comments nil)
  (setq column-enforce-column 120)
  (global-column-enforce-mode t))

(use-package csv-mode)
(use-package ctable)
(use-package dash)
(use-package dash-functional)
(use-package deferred)
(use-package define-word)

;; diff-hl-mode highlights uncommitted changes on the left side of the
;; window, allows you to jump between and revert them selectively.
;; https://github.com/dgutov/diff-hl
(use-package diff-hl
  :config
  (global-diff-hl-mode))

(use-package dumb-jump
  :after hydra
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window)
         ("C-z j" . hydra-dumb-jump/body))
  :config
  (setq dumb-jump-selector 'helm) ;; (setq dumb-jump-selector 'ivy)
  (defhydra hydra-dumb-jump (:color blue :columns 3)
    "Dumb Jump"
    ("j" dumb-jump-go "Go")
    ("o" dumb-jump-go-other-window "Other window")
    ("e" dumb-jump-go-prefer-external "Go external")
    ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
    ("i" dumb-jump-go-prompt "Prompt")
    ("l" dumb-jump-quick-look "Quick look")
    ("b" dumb-jump-back "Back"))
  :ensure t)

(use-package epc)
(use-package eval-sexp-fu)

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))


(use-package fancy-battery)

;; use display-fill-column-indicator-mode of fci-mode to display
;; a line in column
(use-package fill-column-indicator
  :config
  (setq-default fci-rule-column 120)
  (setq fci-handle-truncate-lines nil)
  (fci-mode -1)
  )

(use-package find-file-in-project)

;; After install gh-md.el you can use the functions
;;gh-md-render-region and gh-md-render-buffer to generate a preview of
;;the markdown content of a buffer.
(use-package gh-md)
(use-package gnuplot)
(use-package golden-ratio)
(use-package google-translate :defer t)
(use-package json)
(use-package json-mode)
(use-package json-reformat)
(use-package json-rpc)
(use-package json-snatcher)


(use-package markdown-mode)
(use-package markdown-toc)
(use-package move-text)
(use-package request)
(use-package tern)
(use-package yaml-mode :defer t)


;; Highlight symbols in code
(use-package erefactor)

;; Highlights the expression evaluated
(use-package eval-sexp-fu)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load hungry Delete, caus we're lazy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set hungry delete:
(use-package hungry-delete
  :ensure t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function global-hungry-delete-mode "hungry-delete.el"))
  :config
  (global-hungry-delete-mode t)
  )


;; (use-package doom-themes
;;   :config
;;   ;; Global settings (defaults)
;;   (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;         doom-themes-enable-italic t) ; if nil, italics is universally disabled

;;   ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
;;   ;; may have their own settings.
;; ;;  (load-theme 'doom-one t)

;;   ;; Enable flashing mode-line on errors
;;   ;;(doom-themes-visual-bell-config)

;;   ;; Enable custom neotree theme (all-the-icons must be installed!)
;;   (doom-themes-neotree-config)
;;   (doom-themes-treemacs-config)

;;   ;; Corrects (and improves) org-mode's native fontification.
;;   (doom-themes-org-config)
;;   )

;;(setq spacemacs-start-directory (concat marcel-lisp-dir "spacemacs/"))
;;(trace-function #'load)
;;(trace-function #'load-file)
;;(setq spacemacs-start-directory  "~/Dropbox/spacemacs/.emacs.d/")
;;(setq spacemacs-start-directory  "/Users/marcelbecker/src/emacs-spacemacs/.emacs.d/")

;;(load-file "~/src/emacs-spacemacs/.spacemacs")
;;(load-file "/Users/marcelbecker/src/emacs-spacemacs/.emacs.d/core/core-versions.el")
;;(load-file "/Users/marcelbecker/src/emacs-spacemacs/.emacs.d/core/core-load-paths.el")
;;(setq package-user-dir "/Users/marcelbecker/src/emacs-spacemacs/.emacs.d/elpa/")
;; (package-initialize)
;;(load-file "/Users/marcelbecker/src/emacs-spacemacs/.emacs.d/core/core-load-paths.el")
;;(load-file "/Users/marcelbecker/Dropbox/spacemacs/.emacs.d/core/core-keybindings.el")
;; (dotspacemacs/layers)
;; (dotspacemacs/init)
;;(load-file "/Users/marcelbecker/src/emacs-spacemacs/.emacs.d/core/core-spacemacs.el")
;;(load-file (concat spacemacs-start-directory "init.el"))
