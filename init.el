;;; -*- lexical-binding: t -*-
;;; Time-stamp: "2024-04-23 Tue 17:36 marcelbecker on Mac-Studio.local"
;;;
;;;  __  __                    _   ____            _
;;; |  \/  | __ _ _ __ ___ ___| | | __ )  ___  ___| | _____ _ __
;;; | |\/| |/ _` | '__/ __/ _ \ | |  _ \ / _ \/ __| |/ / _ \ '__|
;;; | |  | | (_| | | | (_|  __/ | | |_) |  __/ (__|   <  __/ |
;;; |_|  |_|\__,_|_|  \___\___|_| |____/ \___|\___|_|\_\___|_|
;;;
;; use this to profile Emacs initialization.
;; ./nextstep/Emacs.app/Contents/MacOS/Emacs -Q -l \
;; ~/Dropbox/.emacs.d/profile-dotemacs.el
;; --eval "(setq profile-dotemacs-file (setq load-file-name \"~/Dropbox/.emacs.d/init.el\") marcel-lisp-dir \"~/Dropbox/.emacs.d/\")"
;;-f profile-dotemacs


;; Use this to create a new prefix
;; (fset     'my-cmds-prefix (make-sparse-keymap))
;; (defconst  my-cmds-map    (symbol-function 'my-cmds-prefix))
;; (let ((former-ctrl-r (key-binding "\C-r")))
;;   (and (not (equal 'my-cmds-prefix former-ctrl-r))
;;    (define-key my-cmds-map "\C-r"  former-ctrl-r)))
;; (define-key global-map   "\C-r"     'my-cmds-prefix)
;; (define-key my-cmds-map "."        'set-mark-command)
;; or from https://www.masteringemacs.org/article/mastering-key-bindings-emacs
;; (defun mp-insert-date ()
;;   (interactive)
;;   (insert (format-time-string "%x")))
;; (defun mp-insert-time ()
;;   (interactive)
;;   (insert (format-time-string "%X")))
;; (global-set-key (kbd "C-c i d") 'mp-insert-date)
;; (global-set-key (kbd "C-c i t") 'mp-insert-time)



(setq stack-trace-on-error t)
(setq debug-on-error t)
;;(setq debug-on-signal t)
;;(setq debug-on-message "quote")

;;(load-file "profile-dotemacs.el")
;;(profile-dotemacs)
;; To profile elisp functions:
;;(use-package benchmark)
;; in scratch buffer CTRL-J
;;(benchmark-elapse (pcache-kill-emacs-hook))
(defmacro measure-time (&rest body)
  "Measure and return the running time of the code block."
  (declare (indent defun))
  (let ((start (make-symbol "start")))
    `(let ((,start (float-time)))
       ,@body
       (- (float-time) ,start))))


;; Read the contents of a buffer into a string
(defun my-buffer-contents (&optional buffer-or-name)
  (with-current-buffer (if buffer-or-name buffer-or-name (current-buffer))
    (buffer-substring-no-properties (point-min) (point-max))))

;; Open buffer in emacs.
;; In scratch buffer, CTRL-J
;;(buffer-content "init.el")

;; Read all the s-expressions in a buffer and add to a list
(defun my-read-buffer-sexps (buffer-or-name)
  (let ((sexps '())
        (sexp nil)
        (buf (if buffer-or-name buffer-or-name (current-buffer))))
    (with-current-buffer buf
      (goto-char (point-min))
      (ignore-errors
        (while (setq sexp (read (current-buffer)))
          ;;(message "%s" sexp)
          (print sexp)
          (push sexp sexps))))
    sexps))
;; Open buffer in emacs,
;; In scratch buffer, CTRL-J
;;(my-read-buffer-sexps "init.el")


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
;;           "C:/Dropbox/.emacs.d/")
;;           ((file-exists-p "D:/Dropbox/.emacs.d")
;;            (setenv "HOME" "D:/Dropbox")
;;            "D:/Dropbox/.emacs.d/")
;;           (t
;;            (expand-file-name "~/.emacs.d/")))
;;   (cond ((file-exists-p  "~/Dropbox/.emacs.d")
;;          "~/Dropbox/.emacs.d/")
;;         (t
;;          (expand-file-name "~/.emacs.d/"))))
;; "Address of Marcel's lisp libraries.")

;;  (setq user-init-file (expand-file-name "init.el" marcel-lisp-dir))
;;  (if (not (eq user-init-file (expand-file-name "~/.emacs.d")))
;;      (let ((file-name-handler-alist nil))
;;        (load-file user-init-file))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; By default Emacs triggers garbage collection at ~0.8MB which makes
;; startup really slow. Since most systems have at least 64MB of memory,
;; we increase it during initialization.
(setq gc-cons-threshold 100000000)
(add-hook 'after-init-hook #'(lambda ()
                               ;; restore after startup
                               (setq gc-cons-threshold 800000)))
(setq inhibit-compacting-font-caches t)
(setq frame-inhibit-implied-resize t)
;;(message  (concat "Loading " load-file-name))

;; UTF-8 support
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))



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
  (setq mac-right-control-modifier 'super)
  ;;  (setq mac-right-option-modifier 'super)
  ;;(setq mac-right-command-modifier 'super)
  (setq mac-right-command-modifier 'meta)
  (setq mac-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char)

  (setq ns-use-srgb-colorspace nil)
  (setq powerline-image-apple-rgb t)
  (setq mac-allow-anti-aliasing t)
  ) ;; sets fn-delete to be right-delete


(global-set-key (kbd "M-z") 'undo-tree-visualize)

;; add everything under ~/.emacs.d to it
(unless (boundp 'marcel-lisp-dir)
  (defvar marcel-lisp-dir
    (if running-ms-windows ; Windows
        (cond ((file-exists-p "C:/Dropbox/.emacs.d")
               (setenv "HOME" "C:/Dropbox")
               "C:/Dropbox/.emacs.d/")
              ((file-exists-p "D:/Dropbox/.emacs.d")
               (setenv "HOME" "D:/Dropbox")
               "D:/Dropbox/.emacs.d/")
              (t
               (expand-file-name "~/.emacs.d")))
      (cond ((file-exists-p  "~/Dropbox/.emacs.d")
             "~/Dropbox/.emacs.d/")
            (t
             (expand-file-name "~/.emacs.d/"))))
    "Address of Marcel's lisp libraries."))




(setq user-emacs-directory marcel-lisp-dir)


(display-init-load-time-checkpoint "Setting up user cache")
(defconst user-cache-directory
  (expand-file-name (concat user-emacs-directory ".cache/"))
  "My emacs storage area for persistent files.")
;; create the `user-cache-directory' if it doesn't exist
(make-directory user-cache-directory t)
(display-init-load-time-checkpoint "Done setting up user cache")

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
;;(debug-on-entry 'warn)
;;(debug-on-entry 'warning)
;;(debug-on-entry 'display-warning)
;; (defun dont-delay-compile-warnings (fun type &rest args)
;;   (if (eq type 'bytecomp)
;;       (let ((after-init-time t))
;;         (apply fun type args))
;;     (apply fun type args)))
;; (advice-add 'display-warning :around #'dont-delay-compile-warnings)


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




(display-init-load-time-checkpoint "Starting to setup Emacs GUI Preferences")

(tool-bar-mode -1)
(blink-cursor-mode 0)
(transient-mark-mode t)
(show-paren-mode t)
;;(line-number-mode 1)
;;(global-linum-mode 1)
;;(linum-mode 1)


(global-hl-line-mode 1)
(display-line-numbers-mode 1)
(global-display-line-numbers-mode 1)
;; Face for the highlighted current line.
(set-face-attribute 'hl-line nil :inherit nil :background "#666666"  :foreground nil :weight 'ultra-bold)
;; Face for the line number
(set-face-attribute 'line-number-current-line nil :background "#666666"  :foreground nil :weight 'ultra-bold)
;; highlight current line





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

;; whenever an external process changes a file underneath emacs, and there
;; was no unsaved changes in the corresponding buffer, just revert its
;; content to reflect what's on-disk.
;; Turn on font-lock in all modes that support it
(global-auto-revert-mode 1)
(global-font-lock-mode t)
(setq max-lisp-eval-depth 1000)
(setq inhibit-startup-message t)
(setq delete-by-moving-to-trash t)
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

;; dim the ignored part of the file name
(file-name-shadow-mode 1)
;;Line wrap
(global-visual-line-mode)
(setq line-move-visual t) ;; move via visual lines


;; ignore case when reading a file name completion
(setq read-file-name-completion-ignore-case t)
;; ignore case when reading a buffer name
(setq read-buffer-completion-ignore-case t)
;; do not consider case significant in completion (GNU Emacs default)
(setq completion-ignore-case t)

;; visually indicate buffer boundaries and scrolling
(setq indicate-buffer-boundaries t)

;; highlight trailing whitespaces in all modes
(setq-default show-trailing-whitespace nil)
(defun my-buf-show-trailing-whitespace ()
  (interactive)
  (setq show-trailing-whitespace t))
(add-hook 'prog-mode-hook #'my-buf-show-trailing-whitespace)

;; Delete trailing whitespace when saving (compliance with PEP8)
;;(add-hook 'before-save-hook 'delete-trailing-whitespace)
;;no extra whitespace after lines
;; http://emacsredux.com/blog/2013/05/16/whitespace-cleanup/
;;'whitespace-cleanup is better than delete-trailing-whitespace
;;(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)


(setq suggest-key-bindings 10)
(setq tab-always-indent 'complete)  ;; use 't when company is disabled


;; ;; Clipboard
;; Use the clipboard, pretty please, so that copy/paste "works"
;; Merge system's and Emacs' clipboard
(setq
 select-enable-primary t
 select-enable-clipboard t);; Save whatever’s in the current (system) clipboard before
;; replacing it with the Emacs’ text.
;; https://github.com/dakrone/eos/blob/master/eos.org
(setq save-interprogram-paste-before-kill t)

;; Prompts should go in the minibuffer, not in a GUI.
(setq use-dialog-box nil)

;; Answer y or n instead of yes or no at minibar prompts.
(defalias 'yes-or-no-p 'y-or-n-p)

(display-init-load-time-checkpoint "Done with setup Emacs GUI Preferences")

(display-init-load-time-checkpoint "Starting to setup frame parameters")

;; use C-u C-x = to describe face at point.
(setq default-frame-font
      (cond (running-ms-windows
             "DejaVu Sans Mono 11")
            (running-macos
             "Source Code Pro for Powerline-18:medium"
             ;;"Menlo for Powerline-18:regular"
             ;;"DejaVu Sans Mono 18")
             ;;        "Geneva 13")
             )
            ((not running-macos)
             "DejaVu Sans Mono 14")))

(set-frame-font default-frame-font)

(set-face-attribute 'region nil :background "magenta1" :foreground "#ffffff")


(display-init-load-time-checkpoint "Done setting default font")

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
    (floor  (- height 250) (frame-char-height))))


(defun my-get-default-x-frame-position ()
  (let* ((workarea (frame-monitor-workarea))
         (width (nth 2 workarea))
         (display-x (nth 0 workarea)))
    (+ (floor width 6) display-x)))

(defun my-get-default-y-frame-position ()
  (let* ((workarea (frame-monitor-workarea))
         (width (nth 3 workarea))
         (display-y (nth 1 workarea)))
    (+ 80 display-y)))

(setq initial-frame-alist
      '((cursor-color . "white")
        (mouse-color . "white")
        (foreground-color . "white")
        (cursor-type . box)
        (tool-bar-lines . 0)
        ;;(left-fringe . 20)
        ;;(right-fringe . 20)
        ;;(top . 50)
        ;;(left . 50)
        ;;(width . 180)
        ))


;;(set-face-attribute 'fringe nil :background "yellow")

;; Set Frame width/height
(defun my-arrange-frame (w h x y)
  "Set the width, height, and x/y position of the current frame"
  (let ((frame (selected-frame)))
    (delete-other-windows)
    (message (with-output-to-string (princ " w = ")(princ w)
                                    (princ " h = ")(princ h)
                                    (princ " x = ")(princ x)
                                    (princ " y = ")(princ y)))
    (set-frame-position frame x y)
    (set-frame-size frame w h)))



(display-init-load-time-checkpoint "Configuring emacs frame")


;; This function will center the initial emacs frame using 50% the width and 90% of the height of the monitor.
(defun my-set-initial-frame ()
  (interactive t)
  (let* ((width-factor 0.5)
         (height-factor 0.9)
         (a-width (* (display-pixel-width) width-factor))
         (a-height (* (display-pixel-height) height-factor))
         (a-left (truncate (/ (- (display-pixel-width) a-width) 2)))
         (a-top (truncate (/ (- (display-pixel-height) a-height) 2))))
    (set-frame-position (selected-frame) a-left a-top)
    (set-frame-size (selected-frame) (truncate a-width)  (truncate a-height) t)))
(setq frame-resize-pixelwise t)
;;(my-set-initial-frame)


;;; Use (frame-parameters) [Ctrl-J] in *scratch* buffer to see frame parameters.
;;;
(let* ((frame-font (cons 'font default-frame-font))
       (default-height (my-get-default-frame-height))
       (frame-height   (cons 'height default-height))
       (frame-width    (cons 'width 180))
       (frame-top      (cons 'top  (my-get-default-y-frame-position)))
       (frame-left     (cons 'left (my-get-default-x-frame-position)))
       (bg-color       (if (eq (user-uid) 0) "gray38" "#09223F"))
       (frame-background-color (if (eq (user-uid) 0)
                                   '(background-color . "gray38")
                                 '(background-color . "#09223F")
                                 )))
  (add-to-list 'default-frame-alist frame-font)
  (add-to-list 'initial-frame-alist frame-font)
  (add-to-list 'default-frame-alist frame-background-color)
  (add-to-list 'initial-frame-alist frame-background-color)
  (set-face-attribute 'default nil :background bg-color :foreground "white")

  (when (> (display-pixel-width) 7000)
    (setq frame-height (cons 'height 87)
          frame-width  (cons 'width 231)
          frame-top    (cons 'top 63)
          frame-left   (cons 'left 2483))
    (set-frame-position (selected-frame) 2483 63)
    (set-frame-size (selected-frame) 231 87 nil))


  (when window-system

    (add-to-list 'default-frame-alist frame-height)
    (add-to-list 'initial-frame-alist frame-height)

    (add-to-list 'default-frame-alist frame-width)
    (add-to-list 'initial-frame-alist frame-width)

    (add-to-list 'default-frame-alist frame-top)
    (add-to-list 'initial-frame-alist frame-top)

    (add-to-list 'default-frame-alist frame-left)
    (add-to-list 'initial-frame-alist frame-left)

    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
    (add-to-list 'default-frame-alist '(ns-appearance . dark))

    ;;(message  "Frame alist %s" initial-frame-alist)
    ;;(arrange-frame 180 (my-get-default-frame-height) (my-get-default-x-frame-position) (my-get-default-y-frame-position))
    ))

(defun my-example-make-frame ()
  "Doc-string."
  (interactive)
  (make-frame '((name . "HELLO-WORLD")
                (font . "-*-Courier-normal-normal-normal-*-18-*-*-*-m-0-iso10646-1")
                (top . 100)
                (left . 100)
                (left-fringe . 8)
                (right-fringe . 8)
                (vertical-scroll-bars . right)
                (cursor-color . "yellow")
                (cursor-type . (box . 1))
                (background-color . "black")
                (foreground-color . "white")
                (tool-bar-lines . 0)
                (menu-bar-lines . 0)
                ;(width . (text-pixels . 400))
                ;(height . (text-pixels . 400))
                )))

(display-init-load-time-checkpoint "Finished configuring emacs frame")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ELPA Packages
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(display-init-load-time-checkpoint "Setting up ELPA")
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpas" . "https://melpa.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ;; ( "org" . "http://orgmode.org/elpa/")
                         ))



;; use
;; brew install libressl
(require 'gnutls)
(add-to-list 'gnutls-trustfiles "/usr/local/etc/openssl/cert.pem")

;; (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
;;     (not (gnutls-available-p))))
;;        (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
;;   (add-to-list 'package-archives (cons "melpa" url) t))

(display-init-load-time-checkpoint "Loading package")

(setq package-enable-at-startup nil)
(setq package-check-signature nil)
(setq package--init-file-ensured nil)
(setq package-user-dir (concat marcel-lisp-dir "elpa"))
(add-to-list 'load-path (concat marcel-lisp-dir "elpa"))

;;(when (not package-archive-contents)
;;(package-refresh-contents))
(when (version< emacs-version "28.0"))
(display-init-load-time-checkpoint "Calling package-initialize")
(setq package-quickstart-file (concat marcel-lisp-dir "package-quickstart.el"))

;;(setq package-quickstart t)
;;(package-initialize)

;; (defvar cache-file "~/.emacs.d/cache/autoloads")
;; (defun my-package-initialize ()
;;   (interactive)
;;   (unless (load cache-file t t)
;;     (setq package-activated-list nil)
;;     (package-initialize)
;;     (with-temp-buffer
;;       (cl-pushnew package-user-dir load-path :test #'string=)
;;       (dolist (desc (delq nil (mapcar #'cdr package-alist)))
;;         (let ((load-file-name (concat (package--autoloads-file-name desc) ".el")))
;;           (when (file-readable-p load-file-name)
;;             (condition-case _
;;                 (while t (insert (read (current-buffer))))
;;               (end-of-file)))))
;;       (prin1 `(setq load-path ',load-path
;;                     auto-mode-alist ',auto-mode-alist
;;                     Info-directory-list ',Info-directory-list)
;;              (current-buffer))
;;       (write-file (concat cache-file ".el"))
;;       (byte-compile-file cache-file))))

;; (my-package-initialize)


(display-init-load-time-checkpoint "Done with package-initialize")
;;  (package-activate-all)
;;(display-init-load-time-checkpoint "Done with package-activate")

;; Use this to recompile all packages
;; (byte-recompile-directory (expand-file-name "~/Dropbox/.emacs.d/elpa") 0)

(when (boundp 'native-comp-eln-load-path)
  (setq package-native-compile t)
  (setq comp-deferred-compilation t)
  (setq native-comp-speed 3) ;; maximum native Elisp speed!
  ;; native-compile all Elisp files under a directory
  ;;(native-compile-async (expand-file-name "~/Dropbox/.emacs.d/elpa") 'recursively t)
  )


(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package '(use-package :build t))
(setq straight-use-package-by-default t)

(unless (package-installed-p 'use-package)
  (display-init-load-time-checkpoint "Installing use-package")
  (package-refresh-contents)
  (package-install 'use-package)
  (display-init-load-time-checkpoint "Done installing use-package")
  )


(require 'use-package)
(setq ;;use-package-always-ensure t
 use-package-verbose nil
 use-package-compute-statistics nil)
(display-init-load-time-checkpoint "Done loading use-package")

(straight-use-package 'org)
(use-package package-utils)

(use-package quelpa
  ;;  :ensure t
  :init
  (setq quelpa-self-upgrade-p nil)
  (setq quelpa-upgrade-p nil)
  (setq quelpa-checkout-melpa-p nil)
  (setq quelpa-update-melpa-p nil))

(use-package quelpa-use-package
  ;;  :ensure t
  :config
  (quelpa-use-package-activate-advice))


(display-init-load-time-checkpoint "Done loading quelpa")

(defun my-init-benchmark ()
  (display-init-load-time-checkpoint "Loading benchmark-init")
  (use-package benchmark-init
    :config
    ;; To disable collection of benchmark data after init is done.
    (add-hook 'after-init-hook 'benchmark-init/deactivate))
  )
;; Use this to profile the initialization
;;(my-init-benchmark)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SPACEMACS LOAD

;;(setenv "HOME" "/Users/marcelbecker/src/emacs-spacemacs/")
;;(setq spacemacs-start-directory "~/.spacemacs.d/")
;;(setq user-emacs-directory "~/.spacemacs.d/")
;;(debug-on-variable-change 'package-user-dir)
;;(trace-function 'load-file)
;;(trace-function 'load)
;;(trace-function 'configuration-layer//install-from-elpa)
;;(trace-function 'configuration-layer/get-elpa-package-install-directory)
;;(load-file (concat spacemacs-start-directory "core/core-versions.el"))
;;(load-file (concat spacemacs-start-directory "core/core-load-paths.el"))
;;(load-file (concat spacemacs-start-directory "core/core-dumper.el"))
;;(load-file (concat spacemacs-start-directory "core/core-keybindings.el"))
;;(load-file (concat spacemacs-start-directory "init.el"))



(use-package discover
  :config
  (global-discover-mode 1))

(use-package diminish
  ;;  :ensure t
  :diminish "")


(use-package f)
(display-init-load-time-checkpoint "Done Loading f")
(use-package cl-lib)
(display-init-load-time-checkpoint "Done Loading cl-lib")
(use-package s)
(display-init-load-time-checkpoint "Done Loading s")
(use-package seq)
(display-init-load-time-checkpoint "Done Loading seq")
(use-package shell-command)
(display-init-load-time-checkpoint "Done Loading shell-command")


;; https://github.com/domtronn/all-the-icons.el#installation
;;(all-the-icons-insert-icons-for 'alltheicon 1)   ;; Prints all the icons for `alltheicon' font set
;;(all-the-icons-insert-icons-for 'octicon 1)   ;; Prints all the icons for the `octicon' family
;;(all-the-icons-insert-icons-for 'fileicon 1)
;; (all-the-icons-insert-icons-for 'wiicon 1)
;; and makes the icons height 10
;;(all-the-icons-insert-icons-for 'faicon 1 0.5) ;; Prints all the icons for the `faicon' family
;; and also waits 0.5s between printing each one


(use-package all-the-icons)
(use-package all-the-icons-ibuffer
  :after all-the-icons
  ;;:ensure t
  :init (all-the-icons-ibuffer-mode 1)
  :hook (dired-mode . all-the-icons-dired-mode)
  :config
  ;; The default icon size in ibuffer.
  (setq all-the-icons-ibuffer-icon-size 1.0)

  ;; The default vertical adjustment of the icon in ibuffer.
  (setq all-the-icons-ibuffer-icon-v-adjust 0.0)

  ;; Use human readable file size in ibuffer.
  (setq  all-the-icons-ibuffer-human-readable-size t)

  ;; A list of ways to display buffer lines with `all-the-icons'.
  ;; See `ibuffer-formats' for details.
  ;;all-the-icons-ibuffer-formats
  )
(display-init-load-time-checkpoint "Done Loading all-the-icons")

;; (use-package major-mode-icons
;;   :config
;;   (setq major-mode-icons-icons-style 'all-the-icons))

(use-package mode-icons
  :config
  (mode-icons-mode 1))

(use-package neotree
  :defer t
  :config
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
  (setq projectile-switch-project-action 'neotree-projectile-action)
  (setq neo-theme (if window-system 'icons 'nerd)) ; 'Classic, 'nerd, 'ascii, 'arrow
  (setq neo-vc-integration '(face char))
  (setq neo-show-hidden-files t)
  (setq neo-toggle-window-keep-p t)
  (setq neo-force-change-root t)
  (setq neo-window-fixed-size nil)
  (setq neo-window-width 60)
  (setq neo-window-position 'right)
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
  )

(display-init-load-time-checkpoint "Done Loading neotree")



;; open my Emacs init file
(defun my-open-dot-emacs ()
  "Opening `~/.emacs.d/init.el'"
  (interactive)
  (find-file (concat marcel-lisp-dir "init.el")))
(global-set-key (kbd "<S-f3>") 'my-open-dot-emacs)


(defun my-load-doom-themes ()
  (interactive)
  (display-init-load-time-checkpoint "Loading doom themes")
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
    (doom-themes-org-config))
  (display-init-load-time-checkpoint "Loading doom themes"))




;;(message "Loading auto-dim-other-buffers")
;; This does not work with native compiled emacs
(unless (boundp 'native-comp-eln-load-path)
  (use-package auto-dim-other-buffers
    :diminish " " ;;"DIM"
    :init
    (add-hook 'after-init-hook
              (lambda ()
                (when (fboundp 'auto-dim-other-buffers-mode)
                  (auto-dim-other-buffers-mode t))))))


(use-package auto-dim-other-buffers
  :diminish " " ;;"DIM"
  :init
  (add-hook 'after-init-hook
            #'(lambda ()
                (when (fboundp 'auto-dim-other-buffers-mode)
                  (auto-dim-other-buffers-mode t)))))

(display-init-load-time-checkpoint "Done Loading auto-dim-other-buffers")


(use-package bind-key)  ;; if you use any :bind variant
(use-package bind-map)
(display-init-load-time-checkpoint "Done Loading bind-key")


(use-package restart-emacs
  :commands restart-emacs)
(display-init-load-time-checkpoint "Done Loading restart-emacs")

(use-package wgrep
  :defer t
  ;;  :init (require 'wgrep)
  )

(display-init-load-time-checkpoint "Done Loading wgrep")

(use-package wgrep-ag
  :after wgrep)
(display-init-load-time-checkpoint "Done Loading wgrep-ag")



(define-prefix-command 'my-C-Z-key-map)
(bind-key "C-z" my-C-Z-key-map)

(use-package popwin
  :commands popwin-mode
  :hook (after-init . popwin-mode)
  :config
  (bind-key "A-z" popwin:keymap)

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


(display-init-load-time-checkpoint "Done Loading popwin")

;; (use-package sublimity)
;; (use-package sublimity-scroll)
;; (use-package sublimity-map)
;; (use-package sublimity-attractive)


(defun my-load-init-file (filename)
  (let* ((init-file (concat marcel-lisp-dir  filename)))
    (when (file-exists-p init-file)
      (load-file init-file))))


(defun my-load-quelpa-packages ()
  (interactive)
  (display-init-load-time-checkpoint "Loading quelpa packages")
  (my-load-init-file "quelpa-init.el")
  (display-init-load-time-checkpoint "Done loading quelpa packages"))

(defun my-load-dired ()
  (interactive)
  (display-init-load-time-checkpoint "Loading dired extensions")
  (my-load-init-file "dired-init.el")
  (display-init-load-time-checkpoint "Done loading dired extensions"))


(defun my-load-hydra ()
  (interactive)
  (display-init-load-time-checkpoint "Loading hydra definitions")
  (my-load-init-file "hydra-init.el")
  (display-init-load-time-checkpoint "Done loading hydra definitions"))

(defun my-load-emms ()
  (interactive)
  (display-init-load-time-checkpoint "Loading emms definitions")
  (my-load-init-file "emms-init.el")
  (display-init-load-time-checkpoint "Done loading emms definitions"))

(defun my-load-python ()
  (interactive)
  (display-init-load-time-checkpoint "Loading python anaconda")
  ;;  (my-load-init-file "elpy-init.el")
  (my-load-init-file "anaconda-init.el")
  (display-init-load-time-checkpoint "Done loading python anaconda"))

(defun my-load-helm ()
  (interactive)
  (display-init-load-time-checkpoint "Loading helm")
  (my-load-init-file  "helm-init.el")
  (display-init-load-time-checkpoint "Done loading helm"))


(defun my-load-vertico ()
  (interactive)
  (display-init-load-time-checkpoint "Loading vertico/consult")
  (my-load-init-file  "vertico-consult-init.el")
  (display-init-load-time-checkpoint "Done loading vertico/consult"))


(defun my-load-bookmarks ()
  (interactive)
  (display-init-load-time-checkpoint "Loading bookmarks")
  (my-load-init-file "visual-bookmarks-init.el")
  (display-init-load-time-checkpoint "Done loading bookmarks"))

(defun my-load-evil ()
  (interactive)
  (display-init-load-time-checkpoint "Loading evil")
  (my-load-init-file "evil-init.el")
  (display-init-load-time-checkpoint "Done loading evil"))

(defun my-load-treemacs ()
  (interactive)
  (display-init-load-time-checkpoint "Loading treemacs")
  (my-load-init-file "treemacs-init.el")
  (display-init-load-time-checkpoint "Done loading treemacs"))


(defun my-load-shackle ()
  (interactive)
  (display-init-load-time-checkpoint "Loading shackle")
  (my-load-init-file "shackle-init.el")
  (display-init-load-time-checkpoint "Loading shackle"))


(defun my-load-org()
  (interactive)
  (display-init-load-time-checkpoint "Loading org")
  (my-load-init-file "org-init.el")
  (display-init-load-time-checkpoint "Loading org"))

(defun my-load-language-server ()
  (interactive)
  (display-init-load-time-checkpoint "Loading language server")
  (my-load-init-file "emacs-lsp-init.el")
  (display-init-load-time-checkpoint "Done loading language server"))
(defun my-load-lsp ()
  (interactive)
  (my-load-language-server))



(defun my-load-gitgutter ()
  (interactive)
  (display-init-load-time-checkpoint "Loading gitgutter")
  (my-load-init-file "gitgutter-init.el")
  (display-init-load-time-checkpoint "Done loading gitgutter"))


(defun my-load-lilypond ()
  (interactive)
  (my-load-init-file "lilypond-init.el"))

(defun my-load-search-engine ()
  (interactive)
  (my-load-init-file "engine-init.el"))
(my-load-search-engine)


;; (defun my-load-recentf()
;;   (interactive)
;;   (display-init-load-time-checkpoint "Loading recentf")
;;   (my-load-init-file "recentf-init.el")
;;   (display-init-load-time-checkpoint "Done loading recentf"))


(setenv "WORKON_HOME" "~/PythonEnvs")
;;(add-hook 'python-mode-hook (function my-load-python))

;; (defun my-load-slime ()
;;   (interactive)
;;   (let* ((slime-library (concat marcel-lisp-dir  "slime/")))
;;     (when (file-exists-p slime-library)
;;       (add-to-list 'load-path slime-library)
;;       (use-package slime-autoloads)
;;       (eval-after-load "slime"
;;         '(progn
;;            (add-to-list 'load-path (concat marcel-lisp-dir "slime/contrib"))
;;            ;;(slime-setup '(slime-fancy slime-banner slime-repl slime-autodoc  slime-typeout-frame))
;;            (slime-setup '(slime-repl))
;;            (setq slime-complete-symbol*-fancy t)
;;            (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
;;            (global-set-key "\C-cs" 'slime-selector)
;;            (setq inferior-lisp-program ; your Lisp system
;;                  (if running-ms-windows "sbcl.exe --noinform" "/usr/local/bin/sbcl --noinform"))
;;            )))))


(use-package vline
  :defer t
  :diminish "vl")
(display-init-load-time-checkpoint "Done Loading vline")

;;(require 'col-highlight)
;;(require 'tabbar-extension)
;;(use-package redo+)



;;Graphical undo
(use-package undo-tree
  :defer t
  :commands (undo-tree-undo undo-tree-visualize)
  :diminish " "
  :config
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  (let ((undo-dir (concat user-cache-directory "undo")))
    (setq undo-tree-history-directory-alist `((".*" . ,undo-dir))))
  (global-undo-tree-mode))
(display-init-load-time-checkpoint "Done loading undo tree")

(defun undo-tree-save-history-from-hook () nil)
(setq undo-tree-auto-save-history nil)


(use-package recentf
  ;;:ensure t
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
    )
  :config
  (defun recentf-save-list ()
    "Save the recent list.
Load the list from the file specified by `recentf-save-file',
merge the changes of your current session, and save it back to
the file."
    (interactive)
    (let ((instance-list (copy-sequence recentf-list)))
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
  )
(display-init-load-time-checkpoint "Done Loading recentf")

(use-package savehist
  :init
  (setq savehist-file (concat marcel-lisp-dir "savehistory-" machine-nickname))
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history 1)
  (setq savehist-additional-variables
        '(kill-ring
          search-ring
          regexp-search-ring
          command-history
          set-variable-value-history
          custom-variable-history
          query-replace-history
          read-expression-history
          minibuffer-history
          read-char-history
          face-name-history
          bookmark-history
          file-name-history))
  (savehist-mode 1))
(display-init-load-time-checkpoint "Done Loading savehist")


;; find convenient unbound keystrokes
(use-package unbound)  ;; `M-x describe-unbound-keys'
(display-init-load-time-checkpoint "Done Loading unbound")
(use-package free-keys
  :config
  (setq free-keys-modifiers '("" "C" "M" "C-M" "C-S" "C-M-S" "A" "s" "H")))
(display-init-load-time-checkpoint "Done Loading free-keys")

(use-package switch-window
  :custom-face
  (switch-window-label ((t (:inherit font-lock-keyword-face :height 3.0))))
  :config
  ;; (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-minibuffer-shortcut ?0)
  (setq switch-window-multiple-frames t)

  ;; (setq switch-window-auto-resize-window t)
  ;; (setq switch-window-default-window-size '(0.8 . 0.8)) ;80% of frame size

  ;;  (with-eval-after-load 'ivy
  ;;    (setq switch-window-preferred 'ivy))
  (setq switch-window-input-style 'minibuffer)
  (unless (display-graphic-p)
    (setq switch-window-shortcut-appearance 'asciiart)))
(display-init-load-time-checkpoint "Done Loading switch window")


;;(display-init-load-time-checkpoint "Loading eyebrowse")
;; Easy window config switching
;;(use-package eyebrowse
;;  :hook (after-init . eyebrowse-mode))

;; save the place in files
(use-package saveplace
  :init
  (save-place-mode 1)
  (setq save-place-forget-unreadable-files nil)
  (setq save-place-file (concat marcel-lisp-dir "saveplace-" machine-nickname)))
(display-init-load-time-checkpoint "Done loading saveplace")

(defun my-load-yasnippet ()
  (interactive)
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
    (yas-global-mode 1)
    (define-key yas-minor-mode-map (kbd "<tab>") nil)
    (define-key yas-minor-mode-map (kbd "TAB") nil)
    (define-key yas-minor-mode-map (kbd "C-c s") #'yas-expand)
    :config
    (use-package yasnippet-snippets))
  (display-init-load-time-checkpoint "Done Loading yasnippet"))


(use-package anzu
  :config
  (progn
    (global-anzu-mode +1)
    (set-face-attribute 'anzu-mode-line nil
                        :foreground "yellow" :weight 'bold))
  (define-key isearch-mode-map [remap isearch-query-replace]  #'anzu-isearch-query-replace)
  (define-key isearch-mode-map [remap isearch-query-replace-regexp] #'anzu-isearch-query-replace-regexp))
(display-init-load-time-checkpoint "Done Loading anzu")


;;; Keep the highlight color 10% darker than the default background face
;;; https://emacs.stackexchange.com/questions/9740/how-to-define-a-good-highlight-face
(use-package color)
;;(use-package pallet)
(display-init-load-time-checkpoint "Done Loading color")



;;(add-hook 'global-hl-line-mode-hook 'my-set-hl-line-color-based-on-theme)
(use-package diminish
  :config
  (diminish 'visual-line-mode "VizLine"))


;; (use-package hlinum
;;   ;;:ensure t
;;   ;;  :init

;;   :dis
;;   :config
;;   ;;(hlinum-activate)
;;   (set-face-attribute 'hl-line nil :inherit nil
;;                       :background "#666666"
;;                       :foreground 'unspecified
;;                       :weight 'bold)
;;   ;;(set-face-attribute 'linum-highlight-face nil :background "#666666")
;;   (set-face-attribute 'linum-highlight-face nil :inherit 'hl-line :weight 'ultra-bold)

;;   (defun my-set-hl-line-color-based-on-theme ()
;;     "Sets the hl-line face to have no foregorund and a background
;;     that is 10% darker than the default face's background."
;;     (interactive)
;;     (set-face-attribute 'hl-line nil
;;                         :foreground 'unspecified
;;                         :background (color-darken-name (face-background 'default) 10)))


;;   (defun my-set-hl-line-color-lighten-based-on-theme ()
;;     "Sets the hl-line face to have no foregorund and a background
;;     that is 10% darker than the default face's background."
;;     (interactive)
;;     (set-face-attribute 'hl-line nil
;;                         :foreground 'unspecified
;;                         :background (color-lighten-name (face-background 'default) 10)))

;;   (defun my-set-hl-line-color-lighten ()
;;     "Sets the hl-line face to have no foregorund and a background
;;     that is 10% darker than the default face's background."
;;     (interactive)
;;     (set-face-attribute 'hl-line nil
;;                         :foreground 'unspecified
;;                         :background (color-lighten-name (face-background 'hl-line) 10)))

;;   (defun my-set-hl-line-color-darken ()
;;     "Sets the hl-line face to have no foregorund and a background
;;     that is 10% darker than the default face's background."
;;     (interactive)
;;     (set-face-attribute 'hl-line nil
;;                         :foreground 'unspecified
;;                         :background (color-darken-name (face-background 'hl-line) 10)))
;;   ;;(my-set-hl-line-color-based-on-theme)
;;   )
;; (display-init-load-time-checkpoint "Done Loading hlinum")



(use-package expand-region
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

(display-init-load-time-checkpoint "Done Loading expand region")



;; minibuffer completion incremental feedback
;;(icomplete-mode)


(when (boundp 'window-divider-mode)
  (setq window-divider-default-places t
        window-divider-default-bottom-width 5
        window-divider-default-right-width 8)
  (custom-set-faces
   '(window-divider ((t (:foreground "orange"))))
   '(window-divider-first-pixel ((t (:foreground "orange"))))
   '(window-divider-last-pixel ((t (:foreground "orange"))))
   )
  (window-divider-mode t))
(display-init-load-time-checkpoint "Done setting window divider mode")

(use-package emojify
  :hook (after-init . global-emojify-mode))

(defun my-load-company ()
  (interactive)

  ;; Does not do anything.
  (use-package company-posframe
    ;;    :ensure t
    :bind (:map company-posframe-active-map
                ("M-<f1>". company-posframe-quickhelp-toggle)
                ("M-<f2>" . company-posframe-quickhelp-scroll-up)
                ("M-<f3>" . company-posframe-quickhelp-scroll-down))
    :config
    (setq company-posframe-show-indicator t)
    (setq company-posframe-show-metadata t)
    (setq company-posframe-quickhelp-delay 0)
    (company-posframe-mode 1))

  (use-package company
    ;;    :ensure t
    :diminish " Ⓒ" ;;"CIA"
    :bind
    (("A-."       . company-complete)
     ("C-c C-y"   . company-yasnippet)
     :map company-active-map
     ("<escape>"  . company-abort)
     ("<right>"   . company-abort)
     ("<left>"    . company-abort)
     ("C-g"       . company-abort)
     ("C-p"       . company-select-previous)
     ("C-n"       . company-select-next)
     ("TAB"       . company-complete-common-or-cycle)
     ("<tab>"     . company-complete-common-or-cycle)
     ("S-TAB"     . company-select-previous)
     ("<backtab>" . company-select-previous)
     ("C-/"       . company-search-candidates)
     ("C-M-/"     . company-filter-candidates)
     ("C-d"       . company-show-doc-buffer)
     ("C-c s"     . company-yasnippet)
     :map company-search-map
     ("C-p"       . company-select-previous)
     ("C-n"       . company-select-next))
    :hook (after-init . global-company-mode)
    :init
    (setq company-format-margin-function #'company-vscode-dark-icons-margin)
    :config
    (setq company-tooltip-align-annotations t ; aligns annotation to the right
          company-tooltip-limit 12            ; bigger popup window
          company-idle-delay .2               ; decrease delay before autocompletion popup shows
          company-echo-delay 0                ; remove annoying blinking
          company-minimum-prefix-length 2
          company-require-match 'never
          company-dabbrev-ignore-case nil
          company-show-quick-access t
          company-dabbrev-downcase nil)
    (setq company-selection-wrap-around t)
    (use-package company-emoji)
    (use-package company-shell)
    (use-package company-org-block)
    (setq company-backends '(company-capf
                             company-yasnippet
                             company-keywords
                             company-files
                             company-elisp
                             company-ispell
                             company-semantic
                             company-org-block))
    (custom-set-faces
     '(company-preview                        ((t (:foreground "dark gray" :underline t))))
     '(company-preview-common                 ((t (:inherit company-preview))))
     '(company-preview-search                 ((t (:inherit company-preview :background "yellow"))))
     '(company-tooltip                        ((t (:background "light gray" :foreground "black"))))
     '(company-tooltip-selection              ((t (:background "steel blue" :foreground "white" :weight bold))))
     '(company-tooltip-scrollbar-track        ((t (:inherit company-tooltip :background "gray20" :foreground "black" :weight bold))))
     '(company-tooltip-scrollbar-thumb        ((t (:background "gray40" :foreground "black" :weight bold))))
     '(company-template-field                 ((t (:background "magenta" :foreground "black"))))
     '(company-tooltip-annotation             ((t (:background "white" :foreground "black"))))
     '(company-tooltip-annotation-selection   ((t (:background "steel blue"))))
     '(company-tooltip-common                 ((((type x)) (:inherit company-tooltip :foreground "blue" :weight bold))
                                               (t (:inherit company-tooltip :foreground "blue"  :weight bold :underline nil))))
     '(company-tooltip-common-selection       ((((type x)) (:inherit company-tooltip-selection :foreground "blue" :weight bold))
                                               (t (:inherit company-tooltip-selection :foreground "blue" :weight bold :underline nil))))
     '(company-tooltip-mouse                  ((t (:foreground "black"))))
     '(company-tooltip-search                 ((t (:background "red" :foreground "black"))))
     '(company-tooltip-quick-access           ((t (:foreground "black"))))
     '(company-tooltip-quick-access-selection ((t (:foreground "white" :weight bold))))
     '(popup-menu-face                        ((t :foreground "red"   :background "#49483E")))
     '(popup-menu-selection-face              ((t :background "#349B8D"   :foreground "#BBF7EF")))
     )



    (setq company-backends '((company-capf company-dabbrev-code)
                             company-yasnippet
                             company-keywords
                             company-files
                             company-elisp
                             company-ispell
                             company-semantic
                             company-org-block))


    (add-to-list 'company-backends '(company-capf company-dabbrev-code) t)
    (add-to-list 'company-backends '(company-capf company-ispell) t)
    (add-to-list 'company-backends '(company-capf company-files) t)
    (add-to-list 'company-backends '(company-capf company-shell) t)
    (add-to-list 'company-backends '(company-capf company-emoji) t)
    ;; Add yasnippet support for all company backends
    ;; https://github.com/syl20bnr/spacemacs/pull/179
    ;; https://emacs.stackexchange.com/questions/10431/get-company-to-show-suggestions-for-yasnippet-names?rq=1
    (defvar company-mode/enable-yas t
      "Enable yasnippet for all backends.")
    (defun company-mode/backend-with-yas (backend)
      (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
          backend
        (append (if (consp backend) backend (list backend))
                '(:with company-yasnippet))))
    (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

    ;;  (use-package company-anaconda)
    ;;(add-to-list 'company-backends 'company-anaconda t)
    ;;(use-package company-jedi)
    ;;(add-to-list 'company-backends 'company-jedi t)
    ;;(use-package company-tern)
    ;;(add-to-list 'company-backends 'company-tern t)
    (global-company-mode)
    ;; (use-package company-posframe
    ;;   :diminish "PosFr"
    ;;   :config
    ;;   (company-posframe-mode 1)
    ;;   )

    ;; Better sorting and filtering
    (use-package company-prescient
      :init (company-prescient-mode 1))

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
        (setq company-quickhelp-delay 0.4)))
    (defun --set-emoji-font (frame)
      "Adjust the font settings of FRAME so Emacs can display emoji properly."
      (if (eq system-type 'darwin)
          ;; For NS/Cocoa
          (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") frame 'prepend)
        ;; For Linux
        (set-fontset-font t 'symbol (font-spec :family "Symbola") frame 'prepend)))

    ;; For when Emacs is started in GUI mode:
    (--set-emoji-font nil)
    ;; Hook for when a frame is created with emacsclient
    ;; see https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Frames.html
    (add-hook 'after-make-frame-functions '--set-emoji-font)
    ))


;;(my-load-company)
(display-init-load-time-checkpoint "Done Loading company")

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

    ;; original values from company-box
    (setq company-box-frame-parameters
          '((left . -1)
            (no-accept-focus . t)
            (no-focus-on-map . t)
            (min-width  . 0)
            (width  . 0)
            (min-height  . 0)
            (height  . 0)
            (internal-border-width . 1)
            (horizontal-scroll-bars . nil)
            (left-fringe . 0)
            (right-fringe . 0)
            (menu-bar-lines . 0)
            (tool-bar-lines . 0)
            (line-spacing . 0)
            ;; (unsplittable . nil)
            (undecorated . t)
            (top . -1)
            (visibility . nil)
            (mouse-wheel-frame . nil)
            (no-other-frame . t)
            (cursor-type . nil)
            (drag-internal-border . t)
            (left-fringe . 0)
            (right-fringe . 0)
            (no-special-glyphs . t))
          "Frame parameters used to create the frame.")


    ;; (setq company-box-frame-parameters
    ;;       '(;;(left . -1)
    ;;         (no-accept-focus . t)
    ;;         (no-focus-on-map . t)
    ;;         ;;(min-width  . t)
    ;;         ;;(min-height  . t)
    ;;         ;;(width  . 30)
    ;;         ;;(height  . 30)
    ;;         (internal-border-width . 5)
    ;;         (vertical-scroll-bars . nil)
    ;;         (horizontal-scroll-bars . nil)
    ;;         (menu-bar-lines . 0)
    ;;         (tool-bar-lines . 0)
    ;;         (line-spacing . 1)
    ;;         ;; (unsplittable . nil)
    ;;         (undecorated . t)
    ;;         ;;(top . -1)
    ;;         (visibility . nil)
    ;;         (mouse-wheel-frame . nil)
    ;;         (no-other-frame . t)
    ;;         (cursor-type . nil)
    ;;         (drag-internal-border . t)
    ;;         (left-fringe . 5)
    ;;         (right-fringe . 5)
    ;;         (no-special-glyphs . t)))
    ;; (setq company-box-doc-frame-parameters
    ;;       '((internal-border-width . 5)
    ;;         (foreground-color . "white")
    ;;         (background-color . "black")
    ;;         (no-accept-focus . t)
    ;;         (no-focus-on-map . t)
    ;;         )
    ;;       )

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
    (declare-function all-the-icons-fileicon 'all-the-icons)
    (declare-function all-the-icons-faicon 'all-the-icons)
    (declare-function all-the-icons-material 'all-the-icons)
    (declare-function all-the-icons-octicon 'all-the-icons)
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
            (Template . ,(all-the-icons-material "format_align_center" :height 0.9 :v-adjust -0.2))))
    (setq company-box-icons-alist 'company-box-icons-all-the-icons))
  )





;; Replace Strings with Regexes
(use-package visual-regexp
  :defer t
  :bind (("A-%" . vr/replace)
         ("M-%" . vr/query-replace)))
(display-init-load-time-checkpoint "Done Loading visual-regex")
;; ----------------------------------------------------------[Window Number]


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

  (setq winum-scope            'global
        winum-reverse-frame-list          nil
        winum-auto-assign-0-to-minibuffer t
        winum-assign-func            'my-winum-assign-func
        winum-auto-setup-mode-line        t
        winum-mode-line-position          1
        winum-ignored-buffers             '(" *which-key*"))
  (winum-mode))

(display-init-load-time-checkpoint "Done Loading winum")


;; Quickly jump between windows using ace-window, I used it frequently and bind it F1.

(use-package ace-window
  :config ;;  :init
  (global-set-key (kbd "<f1>") 'ace-window)
  (global-set-key (kbd "C-x o") 'ace-window)
  (global-set-key (kbd "M-O") 'ace-swap-window)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-minibuffer-flag t)
  (setq aw-scope 'global))
(display-init-load-time-checkpoint "Done Loading ace-window")

;; ;; ;; ----------------------------------------------------------[Window Number]


(use-package flycheck
  :diminish " "
  ;;:config
  ;;(global-flycheck-mode 1)
  ;;(add-hook 'after-init-hook #'global-flycheck-mode)
  )

(use-package flycheck-pos-tip)
(display-init-load-time-checkpoint "Done Loading flycheck")

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

(use-package which-key
  ;;  :init
  :config
  (which-key-mode)
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
        (setq-local which-key-max-description-length 55)
        (setq-local which-key-add-column-padding 5)
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

(global-set-key (kbd "C-c h b") 'describe-personal-keybindings)
(use-package remind-bindings
  :hook (after-init . remind-bindings-initialise)
  :bind (("C-c C-b" . remind-bindings-togglebuffer)
         ("C-c C-d" . 'remind-bindings-specific-mode)))


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

(use-package rainbow-delimiters
  :diminish "RainDel"
  :config ;;:init
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
   '(rainbow-delimiters-depth-8-face ((t (:foreground "NavajoWhite3"))))
   '(rainbow-delimiters-depth-9-face ((t (:foreground "slate gray"))))
   )
  ;;:config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))
(display-init-load-time-checkpoint "Done Loading rainbow-delimiters")


;; Colorize color names in buffers
(use-package rainbow-mode
  :diminish "RNB")
(display-init-load-time-checkpoint "Done loading rainbow-mode")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Origami - Does code folding, ie hide the body of an
;; if/else/for/function so that you can fit more code on your screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package origami
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
  (add-hook 'prog-mode-hook 'origami-mode))
(display-init-load-time-checkpoint "Done loading origami")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Beacon-mode: flash the cursor when switching buffers or scrolling
;;              the goal is to make it easy to find the cursor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package beacon
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function beacon-mode "beacon.el"))
  :config
  (setq
   beacon-blink-when-window-scrolls t
   beacon-blink-when-window-changes t
   beacon-blink-when-point-moves t
   beacon-size 60)
  (beacon-mode t))
(display-init-load-time-checkpoint "Done loading beacon")

;; ;; ;;Rectangular markings-----------------------
;; ;; ;;COOL! C-RET gives rectangular marking for copy/paste, extremely useful
;; ;; ;;for tables. NOTE, second line needed for rectangle, but also gives
;; ;; ;; (transient-mark-mode t) = visualize C-SPC-marking (i.e. highlight)
;; ;; (setq cua-enable-cua-keys nil) ;;only for rectangle, don't use C-x/c/v for copy/paste
;; ;; (cua-mode t)                   ;;gives rectangle + same as "(pc-selection-mode)" (=shift+arrow highlights)
;; ;; ;;--------------------------------------------




;; ;; ;; Navigate windows with M-<arrows>
;; ;; (windmove-default-keybindings 'meta)
;; ;; (setq windmove-wrap-around t)

;; ;; ;; winner-mode provides C-<left> to get back to previous window layout
;; ;; (winner-mode 1)


(setq linum-format " %d ")
;;(setq linum-format "\u2502 %6d \u2502\u2502")
;; ;; ;; To make emacs use spaces instead of tabs (Added by Art Lee on 2/19/2008)
(setq-default indent-tabs-mode nil)
;; ;; (setq mail-default-reply-to "becker@kestrel.edu")
(display-time)
(add-hook 'before-save-hook 'time-stamp)
(setq minibuffer-max-depth nil)

(add-hook 'json-mode-hook
          #'(lambda ()
              (setq js-indent-level 2)))


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
(display-init-load-time-checkpoint "Done loading comment-dwim-2")

;; Drag stuff (lines, words, region, etc...) around
(use-package drag-stuff
  :diminish "DRG"
  :commands drag-stuff-define-keys
  :hook (after-init . drag-stuff-global-mode)
  :config
  (add-to-list 'drag-stuff-except-modes 'org-mode)
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))
(display-init-load-time-checkpoint "Done loading drag-stuff")




;; Edit multiple regions in the same way simultaneously
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
(display-init-load-time-checkpoint "Done loading iedit")

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
(display-init-load-time-checkpoint "Done Loading multiple cursors")

;; Smartly select region, rectangle, multi cursors
(use-package smart-region
  :hook (after-init . smart-region-on))
(display-init-load-time-checkpoint "Done loading smart regions")

;; Goto last change
(use-package goto-chg
  :bind ("C-." . goto-last-change)
  :config
  (global-set-key [(control ?.)] 'goto-last-change)
  (global-set-key [(control ?,)] 'goto-last-change-reverse)
  )
(display-init-load-time-checkpoint "Done Loading goto last change")


;; Framework for mode-specific buffer indexes

(use-package imenu
  :bind (("C-'" . imenu)))
(display-init-load-time-checkpoint "Done Loading imenu")
;; Windows-scroll commands
;;  (use-package pager
;;  :bind (("\C-v"   . pager-page-down)
;;         ([next]   . pager-page-down)
;;         ("\ev"    . pager-page-up)
;;         ([prior]  . pager-page-up)
;;         ([M-up]   . pager-row-up)
;;         ([M-kp-8] . pager-row-up)
;;         ([M-down] . pager-row-down)
;;         ([M-kp-2] . pager-row-down))
;; )

;; Discover key bindings and their meaning for the current Emacs major mode
(use-package discover-my-major
  :bind (("C-h M-m" . discover-my-major)
         ("C-h M-M" . discover-my-mode)))
(display-init-load-time-checkpoint "Done Loading discover-my-mode")

(global-set-key [f6] 'line-to-top-of-window)
(global-set-key [M-f11] 'fullscreen)
(global-set-key [S-f11] 'fullscreen)
(global-set-key [M-f12] 'revert-buffer)
(global-set-key [S-f12] 'revert-buffer)
(global-set-key (kbd "<M-f6>") 'recenter)
(global-set-key (kbd "<M-up>") 'scroll-one-line-up)
(global-set-key (kbd "<M-down>") 'scroll-one-line-down)
(global-set-key (kbd "C-c ;") 'comment-region)
(global-set-key (kbd "M-/") 'comment-region)
(global-set-key (kbd "M-o") 'switch-window)
(global-set-key (kbd "<s-home>") 'end-of-buffer)
(global-set-key [f7] 'text-scale-increase)
(global-set-key (kbd "M-+") 'text-scale-increase)
(global-set-key [S-f7] 'text-scale-decrease)
(global-set-key (kbd "M--") 'text-scale-decrease)


(use-package avy
  :bind (("M-s g" . avy-goto-word-1)))
(display-init-load-time-checkpoint "Done Loading avy")

(use-package swiper :defer t)
(display-init-load-time-checkpoint "Done Loading swiper")
;;(use-package counsel :defer t)
;;(display-init-load-time-checkpoint "Done Loading counsel")




;; To use this, just run ivy-push-view to store the current view,
;; and optionally give it a name (a useful default we be
;; offered). This will then be offered when you switch buffer using
;; ivy-switch-buffer (which you are using automatically if you use
;; ivy-mode). To make these ivy-views appear in your buffer list,
;; you might need to set the option
;; (use-package ivy
;;   :diminish ""
;;   :defer t
;;   :config
;;   (ivy-mode 1)
;;   (setq ivy-use-virtual-buffers t)
;;   :bind
;;   (("C-c C-r" . ivy-resume)))

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
;;(display-init-load-time-checkpoint "Done Loading ivy")



;; Iterate through CamelCase words
(global-subword-mode 1)
(diminish 'subword-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; neotree
;;(message "Loading and configuring neotree")

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
  (forward-line arg))


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
(global-set-key (kbd "M-d") 'duplicate-line-or-region)
(global-set-key (kbd "M-A-<down>") 'duplicate-line-or-region)
(global-set-key (kbd "A-<up>") 'move-line-up)
(global-set-key (kbd "A-<down>") 'move-line-down)


;;### Move selected regions up or down
;; It is commands like these one that enable rapid reorganization of your prose when writing one sentence per row.
;; Thank you to DivineDomain for the suggested upgrade.
;; Source: https://www.emacswiki.org/emacs/MoveText
(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column t)))))

(defun move-line-region-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-line-region-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(global-set-key (kbd "M-C-<down>") 'move-line-region-down)
(global-set-key (kbd "M-C-<up>") 'move-line-region-up)



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


(defun sort-lines-nocase ()
  (interactive)
  (let ((sort-fold-case t))
    (call-interactively 'sort-lines)))

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

(add-hook 'Info-mode-hook 'add-browser-backspace-key-to-Info-mode)
;; ----------------------------------------------------------[Info Back Button]

(display-init-load-time-checkpoint "Loading programming mode stuff")

(use-package gradle-mode
  :config
  (setq gradle-use-gradlew t)
  (setq gradle-gradlew-executable "./gradlew"))
(use-package groovy-mode)

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
(setq fill-column 120)

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
              ;;              '(("\\.tex$" . latex-mode))
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
              '(("\\.java$"  . java-mode))
              '(("\\.py$"  . python-mode))
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
              '(("\\.gradle$" . groovy-mode))
              '(("\\.gradle$" . gradle-mode))
              auto-mode-alist))

;;  html-mode
(add-hook 'html-mode-hook
          (lambda ()
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
            (lambda nil (interactive)
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
;; (add-hook 'speedbar-load-hook		; would be too late in antlr-mode.el
;;          (lambda () (speedbar-add-supported-extension ".g")))


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
;;(global-set-key [M-f4] 'sr-speedbar-select-window)
;;(setq speedbar-directory-unshown-regexp "^\\(CVS\\|RCS\\|SCCS\\|\\.\\.*$\\)\\'")
;;(setq speedbar-directory-unshown-regexp  "^\\(\\.*\\)\\'")


(setq ediff-diff-program ; your Lisp system
      (cond (running-ms-windows
             "c:/cygwin/bin/diff")
            (t
             "diff")))

(setq grep-command "grep -i -nH -e -r ")


(use-package flyspell
  :defer t
  :diminish "FlSpl"
  :config
  (setq ispell-list-command "--list")
  (setq-default ispell-program-name
                (cond (running-ms-windows
                       ;;"c:/Program Files/Aspell6/x64/bin/aspell.exe"
                       "aspell")
                      (running-macos
                       (if (file-executable-p "/usr/local/bin/aspell")
                           "/usr/local/bin/aspell"
                         (if (file-executable-p "/opt/homebrew/bin/aspell")
                             "/opt/homebrew/bin/aspell")))
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

(display-init-load-time-checkpoint "Done Loading flyspell")

(use-package ace-flyspell)
(display-init-load-time-checkpoint "Done Loading ace-flyspell")

(use-package flyspell-correct-helm
  :after helm
  :bind ("C-M-;" . flyspell-correct-wrapper)
  :config
  (setq flyspell-correct-interface #'flyspell-correct-helm))
(display-init-load-time-checkpoint "Done Loading flyspell-correct-helm")

;;(use-package auto-dictionary
;;  :config
;;  (add-hook 'flyspell-mode-hook (lambda () (auto-dictionary-mode 1))))




;; ;; If antlr-mode is not part of your distribution, put this file into your
;; ;; load-path and the following into your ~/.emacs:
(autoload 'antlr-mode "antlr-mode" nil t)
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

(setq shell-file-name
      (cond (running-ms-windows ; Windows
             "bash.exe")
            (running-macos
             "zsh")
            (t
             "bash")))

(setenv "SHELL" shell-file-name)
(setq explicit-shell-file-name shell-file-name)

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

(use-package bash-completion
  :config
  (bash-completion-setup))

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
  (display-init-load-time-checkpoint "Loading modeline")
  (load-file (concat marcel-lisp-dir  "telephone-line-mode-line.el"))
  (load-file (concat marcel-lisp-dir  "header-line.el"))
  (display-init-load-time-checkpoint "Done loading modeline")
  )

(defun my-load-leadkey ()
  (interactive)
  (display-init-load-time-checkpoint "Loading leadkey")
  (load-file (concat marcel-lisp-dir  "leadkey-init.el"))
  (display-init-load-time-checkpoint "Done loading leadkey")
  )



(setq frame-title-format
      '("EMACS: [" (:eval (or (getenv "USERNAME") (getenv "USER"))) "@"
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

(defun my-load-pdf-tools()
  (interactive)
  (display-init-load-time-checkpoint "Loading pdf-tools")
  (my-load-init-file "pdf-tools-init.el")
  (display-init-load-time-checkpoint "Done loading pdf-tools"))


(defun my-load-latex ()
  (interactive)
  (display-init-load-time-checkpoint "Loading tex")
  (my-load-init-file "latex-init.el")
  (display-init-load-time-checkpoint "Done loading tex"))

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

(use-package buffer-move)
(display-init-load-time-checkpoint "Done Loading buffer-move")


;; ;; ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; [ CTAGS ]

(use-package ctags-update
  :diminish "Tags"
  :init
  ;;(ctags-auto-update-mode 1)
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

  (setq tags-add-tables nil))

(display-init-load-time-checkpoint "Done Loading ctags update")

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
(global-set-key (kbd "M-A-l") 'my-indent-buffer)


(defun my-kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (cl-remove-if-not
               '(lambda (x)
                  (or (buffer-file-name x)
                      (eq 'dired-mode (buffer-local-value 'major-mode x))))
               (buffer-list)))))

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


;;(display-init-load-time-checkpoint "Loading menubar+")
;;(require 'menu-bar+)



;;(message "Loading time-stamp")
(use-package time-stamp
  :config
  ;; format of the string inserted by `M-x time-stamp'
  (setq time-stamp-format "%Y-%02m-%02d %3a %02H:%02M %u on %s")
  ;; `YYYY-MM-DD Weekday HH:MM user on system'
  ;; see `system-time-locale' for non-numeric formatted items of time
  ;; update time stamps every time you save a buffer
  (add-hook 'write-file-functions 'time-stamp)

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
      (insert (format-time-string format)))))

(display-init-load-time-checkpoint "Done Loading time-stamp")

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

;; (defconst eclipse-java-style
;;   '((c-basic-offset . 2)
;;     (c-comment-only-line-offset . (0 . 0))
;;     ;; the following preserves Javadoc starter lines
;;     (c-offsets-alist . ((inline-open . 0)
;;                         (topmost-intro-cont    . +)
;;                         (statement-block-intro . +)
;;                         (substatement-open     . +)
;;                         (knr-argdecl-intro     . 5)
;;                         (substatement-label    . +)
;;                         (label                 . +)
;;                         (statement-case-open   . +)
;;                         (statement-cont        . +)
;;                         (arglist-intro  . c-lineup-arglist-intro-after-paren)
;;                         (arglist-close  . c-lineup-arglist)
;;                         (access-label   . 0)
;;                         (inher-cont     . c-lineup-java-inher)
;;                         (func-decl-cont . c-lineup-java-throws)
;;                         (arglist-cont-nonempty . ++)
;;                         )))
;;   "Eclipse Java Programming Style")
;;(c-add-style "ECLIPSE" eclipse-java-style)
;;(customize-set-variable 'c-default-style (quote ((java-mode . "eclipse") (awk-mode . "awk") (other . "gnu"))))

;; (use-package company-emacs-eclim
;;   :config
;;   (company-emacs-eclim-setup))



(use-package ansi-color
  :config
  (defun colorize-compilation-buffer ()
    (read-only-mode)
    (ansi-color-apply-on-region compilation-filter-start (point))
    (read-only-mode))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))

(add-hook 'compilation-filter-hook
          (lambda () (ansi-color-apply-on-region (point-min) (point-max))))
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
  ;;(hscroll-mode)
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
  ;;  (hscroll-mode)
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
(use-package smooth-scroll
  :config
  (setq redisplay-dont-pause t)
  (setq scroll-margin 3)
  (setq scroll-step 1)
  (setq scroll-conservatively 10000)
  (setq scroll-preserve-screen-position 1)
  (setq auto-window-vscroll nil)
  ;;(setq  smooth-scroll/vscroll-step-size 1)
  ;;(smooth-scroll-mode 1)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
  (setq mouse-wheel-progressive-speed nil)

  )
(display-init-load-time-checkpoint "Loading smooth-scroll")

(use-package smart-jump
  :defer 5
  :config (smart-jump-setup-default-registers))
(display-init-load-time-checkpoint "Done Loading smart-jump")



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



(use-package browse-kill-ring
  :config
  (browse-kill-ring-default-keybindings)
  (global-set-key (kbd "C-c y")
                  (lambda ()
                    (interactive)
                    (popup-menu 'yank-menu))))
(display-init-load-time-checkpoint "Done Loading browse-kill-ring")


;;(message "Loading one-key")
;;(use-package one-key)
;;(message "Loading one-key-dir")
;;(use-package one-key-dir)
;;(message "Loading one-key-yas")
;;(use-package one-key-yas)
;;(use-package one-key-bmkp)
;;(global-set-key (kbd "C-<f5>") 'one-key-open-associated-menu-set)

(use-package find-file-in-project
  :bind
  (("C-c M-f" . find-file-in-project)
   ("M-S-o" . find-file-in-project)))
(display-init-load-time-checkpoint "Done Loading find-file-in-project")


;; No startup message
;; Change the echo message
(defun display-startup-echo-area-message ()
  (message ""))


(defun my-open-dir-in-iterm ()
  "Open the current directory of the buffer in iTerm."
  (interactive)
  (let* ((iterm-app-path "/Applications/iTerm.app")
         (iterm-brew-path "/opt/homebrew-cask/Caskroom/iterm2/2.1.4/iTerm.app")
         (iterm-path (if (file-directory-p iterm-app-path)
                         iterm-app-path
                       iterm-brew-path)))
    (shell-command (concat "open -a " iterm-path " ."))))
(global-set-key (kbd "C-x t") 'my-open-dir-in-iterm)


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
(display-init-load-time-checkpoint "Done loading ediff")

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


;; open my Emacs init file
(defun my-open-notes ()
  "Opening `~/Dropbox/EmacsOrg/MarcelNotes.org'"
  (interactive)
  (find-file "~/Dropbox/EmacsOrg/MarcelNotes.org"))
(global-set-key (kbd "<M-S-f3>") 'my-open-notes)


(use-package outline-magic
  ;; (add-hook 'outline-mode-hook
  ;;           (lambda ()
  ;;             (require 'outline-cycle)))
  :defer t
  :config
  (add-hook 'outline-minor-mode-hook
            (lambda ()
              (define-key outline-minor-mode-map [(f10)] 'outline-cycle))))
(display-init-load-time-checkpoint "Done Loading outline-magic")

(diminish 'eldoc-mode "")

;; Use this to print all fonts
;;(dolist (font (font-family-list)) (insert (format ";; \"%s\"\n" font)))
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



(use-package system-packages)
(display-init-load-time-checkpoint "Done Loading system packages")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; visible mark - show where mark is                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package visible-mark
  :init
  (defface visible-mark-active ;; put this before (require 'visible-mark)
    '((((type tty) (class mono)))
      (t (:background "magenta")))
    "")
  :config
  (global-visible-mark-mode -1) ;; or add (visible-mark-mode) to specific hooks
  (setq visible-mark-max 2)
  (setq visible-mark-faces `(visible-mark-face1 visible-mark-face2)))
(display-init-load-time-checkpoint "Done Loading visible mark")

(defun my-load-interaction-log ()
  (interactive)
  (display-init-load-time-checkpoint "Loading interaction log")
  (use-package interaction-log
    ;;:defer 10
    :config
    (interaction-log-mode +1)
    (defun open-interaction-log ()
      (interactive)
      (display-buffer ilog-buffer-name))
    (bind-key "A-l" 'open-interaction-log))
  (display-init-load-time-checkpoint "Done loading interaction log")
  )

(setq paradox-github-token '76d271dd2c6e2f893557ba978663af6cc65d3087)


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
(display-init-load-time-checkpoint "Done loading ace jump mode")


(use-package ace-link
  :after org
  :config
  (ace-link-setup-default)
  (define-key org-mode-map (kbd "M-o") 'ace-link-org))
(display-init-load-time-checkpoint "Done loading ace link mode")

(use-package adaptive-wrap)
(display-init-load-time-checkpoint "Done loading adaptive wrap mode")

(use-package aggressive-indent)
(display-init-load-time-checkpoint "Done loading aggressive indent mode")

(use-package dockerfile-mode
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))
(display-init-load-time-checkpoint "Done loading dockerfile  mode")

;;https://github.com/immerrr/ample-regexps.el
(use-package ample-regexps)
(display-init-load-time-checkpoint "Done loading ample regexps")
(use-package async)
(display-init-load-time-checkpoint "Done loading async")

;; Highlights symbol under cursor
;; https://github.com/mhayashi1120/auto-highlight-symbol-mode
(use-package auto-highlight-symbol
  :diminish "AHS")
(display-init-load-time-checkpoint "Done loading auto-highlight-symbol")

(use-package color-theme-modern
  :config
  (defadvice load-theme
      (before theme-dont-propagate activate)
    (mapc #'disable-theme custom-enabled-themes))

  (setq my-cur-theme nil)
  (setq my-themes nil)
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
(display-init-load-time-checkpoint "Done loading color theme modern")

(use-package column-enforce-mode
  :diminish "COL"
  :config
  (setq column-enforce-comments nil)
  (setq column-enforce-column 120)
  (global-column-enforce-mode t))
(display-init-load-time-checkpoint "Done loading column enforce mode")

(use-package csv-mode)
(display-init-load-time-checkpoint "Done loading cvs-mode")
(use-package ctable)
(display-init-load-time-checkpoint "Done loading ctable")
(use-package dash)
(display-init-load-time-checkpoint "Done loading dash")

(use-package deferred)
(display-init-load-time-checkpoint "Done loading deferred")
(use-package define-word)
(display-init-load-time-checkpoint "Done loading define-word")

;; diff-hl-mode highlights uncommitted changes on the left side of the
;; window, allows you to jump between and revert them selectively.
;; https://github.com/dgutov/diff-hl
(use-package diff-hl
  :config
  (use-package solarized-theme)
  (global-diff-hl-mode))
(display-init-load-time-checkpoint "Done loading diff-hl")


(use-package dumb-jump
  :after hydra helm
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
    ("b" dumb-jump-back "Back")))
(display-init-load-time-checkpoint "Done loading dumb-jump")

(use-package epc)
(display-init-load-time-checkpoint "Done loading epc")


(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    ;;     (setq exec-path-from-shell-check-startup-files nil)
    (exec-path-from-shell-initialize)))


;;(use-package fancy-battery)
;;(display-init-load-time-checkpoint "Done loading fancy battery")

;; use display-fill-column-indicator-mode of fci-mode to display
;; a line in column
(use-package fill-column-indicator
  :config
  (setq-default fci-rule-column 120)
  (setq fci-handle-truncate-lines nil)
  (fci-mode -1))
(display-init-load-time-checkpoint "Done loading fill-column-indicator")



;; After install gh-md.el you can use the functions
;;gh-md-render-region and gh-md-render-buffer to generate a preview of
;;the markdown content of a buffer.
(use-package gh-md)
(display-init-load-time-checkpoint "Done loading gh-md")
(use-package gnuplot)
(display-init-load-time-checkpoint "Done loading gnuplot")
(use-package golden-ratio
  :config
  (setq golden-ratio-auto-scale t)
  (setq golden-ratio-exclude-modes
        '("ediff-mode"
          "eshell-mode"
          "dired-mode"
          "pdf-mode"
          ))
  ;;(golden-ratio-mode 1)
  )
(display-init-load-time-checkpoint "Done loading golden-ratio")
(use-package google-translate :defer t)
(use-package json)
(use-package json-mode)
(use-package json-reformat)
(use-package json-rpc)
(use-package json-snatcher)

(display-init-load-time-checkpoint "Done loading json")

(use-package markdown-mode
  ;;:ensure t
  )
(use-package markdown-toc)
(display-init-load-time-checkpoint "Done loading markdown")
(use-package move-text)
(use-package request)
(use-package tern)
(use-package yaml-mode :defer t)
(display-init-load-time-checkpoint "Done loading yaml-mode")

;; Highlight symbols in code
(let ((byte-compile-warnings nil))
  (use-package erefactor))
(display-init-load-time-checkpoint "Done loading erefactor")

;; Highlights the expression evaluated
(let ((byte-compile-warnings nil))
  (use-package eval-sexp-fu))
(display-init-load-time-checkpoint "Done loading eval-sexp-fu")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load hungry Delete, caus we're lazy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set hungry delete:
(use-package hungry-delete
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function global-hungry-delete-mode "hungry-delete.el"))
  :config
  (global-hungry-delete-mode t))
(display-init-load-time-checkpoint "Done loading hungry-delete")

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
(put 'erase-buffer 'disabled nil)




;;; REMAP FACES FOR ISEARCH
;;; https://stackoverflow.com/questions/19473143/i-search-how-to-know-for-sure-whether-focus-is-still-in-the-minibuffer
;; (add-hook 'isearch-mode-hook
;;           (lambda ()
;;             (set (make-local-variable 'isearch-mode-line-face-remap-cookie)
;;                  (face-remap-add-relative
;;                   'mode-line '((:foreground "ivory" :background "red") mode-line)))))

;; (add-hook 'isearch-mode-end-hook
;;           (lambda ()
;;             (face-remap-remove-relative isearch-mode-line-face-remap-cookie)))


;; (add-hook 'isearch-mode-hook
;;           (lambda ()
;;             (set (make-local-variable 'isearch-default-face-remap-cookie)
;;                  (face-remap-add-relative
;;                   'default '((:background "yellow") default)))))

;; (add-hook 'isearch-mode-end-hook
;;           (lambda ()
;;             (face-remap-remove-relative isearch-default-face-remap-cookie)))


(defun enter-minibuffer-setup ()
  ;; (whitespace-mode t)
  ;; (set-face-attribute 'mode-line nil
  ;;                     :height 160 :foreground "gray70" :background "black" :box '(:line-width 1 :color "black"))
  ;; (set-face-attribute 'minibuffer-prompt nil :background "black" :foreground "red")
  ;; (set (make-local-variable 'face-remapping-alist)
  ;;      '((default :background "black" :foreground "yellow")))
  (message "Enter minibuffer setup called")
  (set (make-local-variable 'minibuffer-mode-line-face-remap-cookie)
       (face-remap-add-relative
        'mode-line '((:foreground "ivory" :background "red") mode-line)))
  )

(defun exit-minibuffer-setup ()
  ;;(message "Exit minibuffer")
  ;; (cond
  ;;  ((or save-as-variable multi-extract-variable multi-attach-variable)
  ;;   (set-face-attribute 'mode-line nil :height 160 :foreground "black" :background "#eab700"))
  ;;  (t (set-face-attribute 'mode-line nil :height 160 :foreground "black" :background "gray70" :box nil)))
  ;; (set-face-attribute 'minibuffer-prompt nil :background "white" :foreground "cyan")
  (message "Exit minibuffer setup called")
  (face-remap-remove-relative minibuffer-mode-line-face-remap-cookie)
  )

;; (add-hook 'minibuffer-setup-hook 'enter-minibuffer-setup)
;; (add-hook 'minibuffer-exit-hook 'exit-minibuffer-setup)


;;(debug-on-entry 'byte-compile-file)



;; (use-package modus-themes
;;   :ensure
;;   :init
;;   ;; Add all your customizations prior to loading the themes
;;   (setq modus-themes-italic-constructs t
;;         modus-themes-bold-constructs nil
;;         modus-themes-region '(bg-only no-extend))

;;   ;; Load the theme files before enabling a theme
;;   (modus-themes-load-themes)
;;   :config
;;   ;; Load the theme of your choice:
;;   ;;(modus-themes-load-operandi) ;; OR
;;   (modus-themes-load-vivendi)
;;   :bind ("<f9>" . modus-themes-toggle))



(my-load-hydra)
;;(my-load-emms)
;;(my-load-python)
;; CTRL-Backspace disables auto-expansion
(my-load-treemacs)
(my-load-helm)
;;(my-load-vertico)
(my-load-bookmarks)

(my-load-shackle)
;;(my-load-org)
;;(my-load-gitgutter)
(my-load-pdf-tools)
(my-load-latex)

;;(my-load-modeline)
(my-load-quelpa-packages)

(my-load-dired)
(my-load-interaction-log)

;;;;; NOT USED
;;(my-load-evil)
;;(my-load-language-server)
;;(my-load-doom-themes)
;;(my-load-company-box)
;;(my-load-org-toolkit)
;;(my-load-yasnippet)


(defun fix-terminal-colors ()
  "Installs a copy of eterm-color terminfo."
  (interactive)
  (let ((path-to-emacs-app "/Applications/Emacs.app"))
    (shell-command
     (format "tic -o ~/.terminfo %s/Contents/Resources/etc/e/eterm-color.ti"
             path-to-emacs-app))))

;; "Loading custom file")
(display-init-load-time-checkpoint "Loading custom file")
(setq custom-file (concat marcel-lisp-dir "custom.el"))
(load custom-file 'noerror)
(display-init-load-time-checkpoint "Done loading custom file")


;; (use-package modus-themes
;;   :ensure
;;   :init
;;   ;; Add all your customizations prior to loading the themes
;;   (setq modus-themes-italic-constructs t
;;         modus-themes-bold-constructs nil
;;         modus-themes-region '(bg-only no-extend))

;;   ;; Load the theme files before enabling a theme
;;   (modus-themes-load-themes)
;;   :config
;;   ;; Load the theme of your choice:
;;   ;;(modus-themes-load-operandi) ;; OR
;;   (modus-themes-load-vivendi)
;;   :bind ("<f9>" . modus-themes-toggle))


(my-load-leadkey)
(my-load-modeline)

(use-package psession
  :config
  (psession-mode 1)
  (psession-savehist-mode 1)
  (psession-autosave-mode 1)
  :init
  (setq psession-elisp-objects-default-directory
        (locate-user-emacs-file (concat "elisp-objects-" machine-nickname "/"))))


(display-init-load-time-checkpoint "Done loading init file")

;;; I want a key to open the current buffer all over the screen.
(defun my-all-over-the-screen ()
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (split-window-horizontally)
  (balance-windows)
  (follow-mode t))


;;(use-package moom
;;  :defer t
;;  :config
;;  (setq moom-use-font-module nil)
;;  (moom-mode 1))

;;(use-package symon
;;  :config
;;  (symon-mode))


;; (use-package helpful
;;   :ensure t
;;   :config
;;   ;; Note that the built-in `describe-function' includes both functions
;;   ;; and macros. `helpful-function' is functions only, so we provide
;;   ;; `helpful-callable' as a drop-in replacement.
;;   (global-set-key (kbd "C-h f") #'helpful-callable)

;;   (global-set-key (kbd "C-h v") #'helpful-variable)
;;   (global-set-key (kbd "C-h k") #'helpful-key)

;;   ;; Lookup the current symbol at point. C-c C-d is a common keybinding
;;   ;; for this in lisp modes.
;;   (global-set-key (kbd "C-c C-d") #'helpful-at-point)

;;   ;; Look up *F*unctions (excludes macros).
;;   ;;
;;   ;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
;;   ;; already links to the manual, if a function is referenced there.
;;   (global-set-key (kbd "C-h F") #'helpful-function)

;;   ;; Look up *C*ommands.
;;   ;;
;;   ;; By default, C-h C is bound to describe `describe-coding-system'. I
;;   ;; don't find this very useful, but it's frequently useful to only
;;   ;; look at interactive functions.
;;   (global-set-key (kbd "C-h C") #'helpful-command)
;;   )



(use-package keycast
  :config
  ;; This works with doom-modeline, inspired by this comment:
  ;; https://github.com/tarsius/keycast/issues/7#issuecomment-627604064
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line."
    :global t
    (if keycast-mode
        (add-hook 'pre-command-hook 'keycast--update t)
      (remove-hook 'pre-command-hook 'keycast--update)))
  (add-to-list 'global-mode-string '("" mode-line-keycast " "))
  (keycast-mode))


;; (use-package paradox
;;   :config
;;   (paradox-enable))

;; (set-default 'server-socket-dir "~/.emacs.d/server")
;; (if (functionp 'window-system)
;;     (when (and (window-system)
;;            (>= emacs-major-version 24))
;;       (server-start)))

;; Keep the initial frame where it is.  If the frame is resized or
;; moved while init is loading, after init is loaded, the frame
;; position and size is reset and the emacs window will jump on the
;; screen.  This code keeps the current position and size of the
;; window (frame).
(let* ((frame-parameters (frame-parameters))
       (top (assoc 'top frame-parameters))
       (left (assoc 'left frame-parameters))
       (height (min 85 (frame-height)))
       (width (frame-width))
       (background-color (assoc 'background-color frame-parameters))
       (foreground-color (assoc 'foreground-color frame-parameters))
       (alist (list top left)))
  ;;(message "\n Before %s" default-frame-alist)
  (modify-frame-parameters nil alist)
  (when window-system
    ;;(set-frame-size nil width height)
    (setf (alist-get 'width default-frame-alist) width)
    (setf (alist-get 'width initial-frame-alist) width)
    (setf (alist-get 'height default-frame-alist) height)
    (setf (alist-get 'height initial-frame-alist) height)
    (setf (alist-get 'top default-frame-alist) (cdr top))
    (setf (alist-get 'top initial-frame-alist) (cdr top))
    (setf (alist-get 'left default-frame-alist) (cdr left))
    (setf (alist-get 'left initial-frame-alist) (cdr left))
    ;;(arrange-frame 180 height (my-get-default-x-frame-position) (my-get-default-y-frame-position))
    )
  (setf (alist-get 'background-color default-frame-alist) (cdr background-color))
  (setf (alist-get 'background-color initial-frame-alist) (cdr background-color))
  (setf (alist-get 'foreground-color default-frame-alist) (cdr foreground-color))
  (setf (alist-get 'foreground-color initial-frame-alist) (cdr foreground-color))
  (set-face-attribute 'region nil :background "magenta1" :foreground "#ffffff"))


(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (defun track-mouse (e))
  (setq mouse-sel-mode t)
  )

;; REMOVE THIS
;; ONLY TO TEST NATIVE COMPILED
;; (setq debug-on-signal t)

(use-package frog-jump-buffer
  ;;:ensure t
  :config
  (setq frog-jump-buffer-use-all-the-icons-ivy t)
  (set-face-attribute
   'frog-menu-posframe-background-face
   nil :background "dark cyan")
  (set-face-attribute
   'frog-menu-border
   nil :background "black")
  )


(use-package graphviz-dot-mode
  ;;:ensure t
  :config
  (setq graphviz-dot-indent-width 4))



(use-package editorconfig
  ;;  :ensure t
  :config
  (editorconfig-mode 1))

(global-set-key [remap dabbrev-expand] 'hippie-expand)


;; (add-hook 'vterm-mode-hook
;;           (lambda ()
;;             (set (make-local-variable 'buffer-face-mode-face) 'Hack)
;;                  (buffer-face-mode t)))

;; REMOVE THIS
;; ONLY TO TEST NATIVE COMPILED
;;(setq debug-on-signal t)
(put 'upcase-region 'disabled nil)
(put 'list-threads 'disabled nil)



;;; Use this
;;; This piece of advice allows you to see the function call sequence that resulted in each message in the Messages buffer:
;;(defadvice message (before who-said-that activate)
;;  "Find out who said that thing. and say so."
;;  (let ((trace nil) (n 1) (frame nil))
;;    (while (setq frame (backtrace-frame n))
;;      (setq n     (1+ n)
;;            trace (cons (cadr frame) trace)) )
;;    (ad-set-arg 0 (concat "<<%S>>:\n" (ad-get-arg 0)))
;;    (ad-set-args 1 (cons trace (ad-get-args 1))) ))

;; To deactivate this, call

;;(ad-disable-advice 'message 'before 'who-said-that)
;;(ad-update 'message)

;;(use-package centered-window-mode)
(use-package writeroom-mode
  ;;:ensure t
  )




;;;;;;
;;;
;;; EMACS RESTCLIENT in org mode
;;; Use this in an org file to get the results of a rest endpoing query:
;; #+name: api-url
;; : http://localhost:6333



;; #+HEADER: :var name="test"
;; #+HEADER: :var password="test"
;; #+name: get-post
;; #+begin_src restclient :var api=api-url :exports both :results value raw :jq . :wrap src json
;;   GET  http://localhost:6333/Planner/loadScenario?scenarioId=Pacifica&planName=Plan+A&atoDay=AA
;;   Content-Type: application/json
;; #+end_src

;; #+RESULTS: get-post
;; #+begin_src json
;; {
;;   "response": {
;;     "messageId": "5837908c-2933-4dc3-96fb-7397fa4a282c",
;;     "responderId": "Planner",
;;     "requestMessageId": "b409ca29-b9e4-4e5d-9fe0-870d77368fcc",
;;     "requestorId": "ScharpPlannerClient",
;;     "function": "loadScenario",
;;     "errorMessage": ""
;;   },
;;   "scenarioId": "Pacifica",
;;   "planName": "Plan A",
;;   "atoDay": "AA",
;;   "planId": "d6c9b6ca-47f3-4de7-9a07-ea4d19233732",
;;   "planCreator": ""
;; }
;; #+end_src

(use-package restclient
  :config
  (setq restclient-log-request t)
  )

(use-package ob-restclient
  :after org)


(use-package fzf
  :bind
    ;; Don't forget to set keybinds!
  :config
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
        fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s"
        ;; command used for `fzf-grep-*` functions
        ;; example usage for ripgrep:
        ;; fzf/grep-command "rg --no-heading -nH"
        fzf/grep-command "grep -nrH"
        ;; If nil, the fzf buffer will appear at the top of the window
        fzf/position-bottom t
        fzf/window-height 15))


(use-package transient-dwim
  :bind ("M-=" . transient-dwim-dispatch))

(setq stack-trace-on-error nil)
(setq debug-on-error nil)




;; ;; (native-compile-async "/Users/marcelbecker/src/emacs/lisp" 'recursively)
;; ;; (native-compile-async "/Users/marcelbecker/src/emacs/lisp" 'recursively)


;;(use-package treesit-auto
;;:config
;;(treesit-auto-add-to-auto-mode-alist 'all))
