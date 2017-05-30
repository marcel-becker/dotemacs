(let ((elget-lib (concat marcel-lisp-dir "/el-get/el-get")))
  (if (file-exists-p elget-lib)
      (add-to-list 'load-path elget-lib)))
(require 'el-get nil 'noerror)

;; (let ((icicles-lib (concat marcel-lisp-dir "/icicles/")))
;;   (if (file-exists-p icicles-lib)
;;       (add-to-list 'load-path icicles-lib)))

(unless (boundp 'x-max-tooltip-size)
  (setq x-max-tooltip-size '(80 . 40)))

(setq
 my:el-get-icicles-packages
 '(
   icicles
   ;;icicles-cmd1
   ;;icicles-cmd2
   ;;icicles-face
   icicles-fn
   icicles-mac
   icicles-mcmd
   icicles-mode icicles-opt
   icicles-var
   ;;advice
   ;;advice-preload
   ;;apropos
   apropos+
   apropos-fn+var
   ;;autofit-frame
   ;;avoid
   ;;bookmark
   bookmark+
   bookmark+-1
   bookmark+-bmu
   bookmark+-key
   bookmark+-lit
   bookmark+-mac
   ;;cl
   cmds-menu
   ;;col-highlight
   ;;crosshairs
   ;;cus-edit
   ;;cus-face
   ;;cus-load
   ;;cus-start
   ;;cus-theme
   ;;dired
   ;;dired+

   dired-atool  dired-avfs
dired-column-widths     dired-details   dired-details+
dired-dups  dired-efap  dired-extension
dired-fdclone   dired-filetype-face     dired-filter
dired-hacks
;;dired-hacks-utils
dired-imenu
;dired-isearch
dired-jb-misc-extras    dired-k
dired-lis   dired-narrow    dired-nav-enhance
dired-open  dired-plus  dired-quick-sort
dired-rainbow   dired-ranger    dired-single
dired-sort  dired-sort-map  dired-sort-menu
dired-sort-menu+    dired-subtree   dired-sync
dired-toggle    dired-toggle-sudo   dired-view
dired-xattr     diredful    direx
direx-grep


   doremi
   doremi-frm
   ;;easymenu
   el-swank-fuzzy
   ;;ffap
   ffap-
   ;;fit-frame
   frame-cmds
   frame-fns
   fuzzy
   fuzzy-match
   help+20
   hexrgb
   highlight
   hl-line+
   icicles-chg
   icicles-doc1
   icicles-doc2
   icomplete+
   ;;image-dired
   ;;image-file
   ;;info
   info+
   info+20
   ;;kmacro
   lacarte
   levenshtein
   linkd
   ;;menu-bar
   menu-bar+
   misc-cmds
   misc-fns
   mouse3
   ;;mwheel
   naked
   package
   palette
   ;;pp
   pp+
   ;;regexp-opt
   ;;ring
   ring+
   second-sel
   strings
   subr-21
   synonyms
   ;;thingatpt
   thingatpt+
   unaccent
   vline
   w32-browser
   w32browser-dlgopen
   ;;wid-edit
   wid-edit+
   ;;widget
   ))


;;(el-get 'sync my:el-get-icicles-packages)

 (dolist (p my:el-get-icicles-packages)
   (progn
     (when (not (package-installed-p p))
       ( message "installing package %s" p)
       (package-install p))
     (message "loading package %s" p)
     ;;     (require p nil :noerror)
     ))


(require 'icicles)
(setq autofit-frames-flag nil)
(icy-mode 1)
