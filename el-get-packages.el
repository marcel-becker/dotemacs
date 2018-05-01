;;package-activated-list is a variable defined in `package.el'.
;;Its value is

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
  (setq mac-command-modifier 'meta)
  (setq mac-allow-anti-aliasing t)
  (global-set-key [kp-delete] 'delete-char)) ;; sets fn-delete to be right-delete

(defvar marcel-lisp-dir
  (if (eq system-type 'windows-nt)      ; Windows
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
  "Address of Marcel's lisp libraries.")

;; add everything under ~/.emacs.d to it
(unless (boundp 'marcel-lisp-dir)
  (defvar marcel-lisp-dir
    (expand-file-name "~/.emacs.d")
    "Address of Marcel's lisp libraries."))

(setq user-emacs-directory marcel-lisp-dir)

;;; ADD Marcel'S LISP LIBRARY TO `load-path'.
(add-to-list 'load-path  (concat marcel-lisp-dir "/"))


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
(setq el-get-default-process-sync t  el-get-verbose t)


(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("melpas" . "http://stable.melpa.org/packages/")
                         ))

(setq package-check-signature nil)
(setq package-user-dir (concat marcel-lisp-dir "/elpa"))
(add-to-list 'load-path (concat marcel-lisp-dir "/elpa"))
;;(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(require 'el-get-elpa)
(el-get-emacswiki-refresh)
;;(el-get-emacswiki-build-local-recipes)

;; Build the El-Get copy of the package.el packages if we have not
;; built it before.  Will have to look into updating later ...
(unless (file-directory-p el-get-recipe-path-elpa)
  (el-get-elpa-build-local-recipes))


(defun print-list ()
  (let ((my-list my-packages))
    (dolist (package my-list)
      (message "%s" package))))

(setq
 all-el-get-packages
 '(
   ac-anything
   ac-anything2
   ac-ispell
   ac-python
   ace-isearch
   ace-jump-mode
   ace-link
   ace-window
   adaptive-wrap
   aggressive-indent
   ample-regexps
   anaconda-mode
   angular-snippets
   anything
   anything-ipython
   anzu
   apropos+
   apropos-fn+var
   async
   auctex
   auto-complete
   auto-dictionary
   auto-highlight-symbol
   auto-yasnippet
   autofit-frame
   autopair
   autopep8
   avy
   bind-key
   bind-map
   bookmark+
   bookmark+-1
   bookmark+-bmu
   bookmark+-key
   bookmark+-lit
   bookmark+-mac
   buffer-move
   cl-lib
   clean-aindent-mode
   cmds-menu
   color-theme
   color-theme-tango
   company
   company-anaconda
   company-jedi
   company-mode
   company-quickhelp
   company-statistics
   csv-mode
   ctable
   cython-mode
   dash
   deferred
   define-word
   diff+
   diminish
   dired+
   dired-atool
   dired-avfs
   dired-column-widths
   dired-details
   dired-details+
   dired-dups
   dired-efap
   dired-extension
   dired-fdclone
   dired-filetype-face
   dired-filter
   dired-hacks
   dired-hacks-utils
   dired-imenu
   dired-jb-misc-extras
   dired-k
   dired-lis
   dired-narrow
   dired-nav-enhance
   dired-open
   dired-plus
   dired-quick-sort
   dired-rainbow
   dired-ranger
   dired-single
   dired-sort
   dired-sort-map
   dired-sort-menu
   dired-sort-menu+
   dired-subtree
   dired-sync
   dired-toggle
   dired-toggle-sudo
   dired-view
   dired-xattr
   diredful
   direx
   direx-grep
   discover
   dockerfile-mode
   doremi
   doremi-frm
   el-get
   el-swank-fuzzy
   elisp-slime-nav
   elpy
   emacs-async
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
   faces+
   fancy-battery
   ffap-
   file-template
   fill-column-indicator
   find-file-in-project
   fit-frame
   flx
   flx-ido
   flycheck
   flymake
   frame-cmds
   frame-fns
   fuzzy
   fuzzy-match
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
   helm
   helm-ag
   helm-anything
   helm-c-yasnippet
   helm-core
   helm-descbinds
   helm-git
   helm-gitignore
   helm-make
   helm-mode-manager
   helm-projectile
   helm-pydoc
   helm-spotify
   helm-swoop
   helm-themes
   help+20
   hexrgb
   highlight
   highlight-indentation
   highlight-numbers
   highlight-parentheses
   hl-line+
   htmlize
   hungry-delete
   hy-mode
   hydra
   icicles
   icicles-chg
   icicles-cmd1
   icicles-cmd2
   icicles-doc1
   icicles-doc2
   icicles-face
   icicles-fn
   icicles-mac
   icicles-mcmd
   icicles-mode
   icicles-opt
   icicles-var
   icomplete+
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
   lacarte
   let-alist
   leuven-theme
   levenshtein
   linkd
   linum-relative
   lua-mode
   macrostep
   magit
   magit-gitflow
   magit-popup
   makey
   markdown-mode
   markdown-toc
   menu-bar+
   misc-cmds
   misc-fns
   mouse3
   move-text
   multi
   naked
   neotree
   nginx-mode
   noflet
   nose
   open-junk-file
   package
   package-build
   page-break-lines
   palette
   paradox
   parent-mode
   pcre2el
   pip-requirements
   pkg-info
   popup
   popwin
   pos-tip
   powerline
   pp+
   projectile
   py-autopep8
   pycomplete+
   pydoc
   pydoc-info
   pyenv-mode
   pylookup
   pymacs
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
   ring+
   rope
   ropemacs
   ropemode
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
   strings
   subr-21
   swiper
   switch-window
   synonyms
   tabbar
   tabbar-extension
   thingatpt+
   unaccent
   unbound
   undo-tree
   use-package
   vi-tilde-fringe
   virtualenvwrapper
   vline
   volatile-highlights
   w32-browser
   w32browser-dlgopen
   websocket
   which-key
   wid-edit+
   windata
   window-number
   window-numbering
   with-editor
   yasnippet
   yasnippet-snippets
   yasnippets
   zencoding-mode
   ))

(el-get 'sync all-el-get-packages)
