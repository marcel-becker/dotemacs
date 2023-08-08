(require 'dired)
(require 'dired-x)

;; Pack and unpacking of files.
;; https://github.com/HKey/dired-atool
;;(use-package dired-atool
;;  :config
;;  (dired-atool-setup))

(use-package dired-avfs)
(use-package dired-dups)
;; This causes message Invalid face reference: quote
(use-package dired-efap)
;;(use-package dired-explorer)
(use-package dired-fdclone)
(use-package dired-filetype-face)
(use-package dired-hacks-utils)
(use-package dired-imenu)
(use-package dired-launch)
(use-package dired-narrow)
(use-package dired-open)
(use-package dired-quick-sort)
(use-package dired-rainbow)

(use-package ranger
  :defer t
  :config
  (setq ranger-show-hidden t)
  (setq ranger-preview-file t)
  (setq ranger-dont-show-binary t)
  (setq ranger-cleanup-on-disable t)
  )

(use-package dired-ranger)
(use-package dired-toggle)
(use-package dired-toggle-sudo)
(use-package diredful)



(use-package all-the-icons-dired)


(use-package dired-column-widths
  :quelpa (dired-column-widths :fetcher github :repo "emacsmirror/dired-column-widths"))


;; Use ')' in dired window to show or hide file details
(use-package dired-details
  :quelpa (dired-details :fetcher github :repo "emacsmirror/dired-details"))
(use-package dired-details+
  :quelpa (dired-details+ :fetcher github :repo "emacsmirror/dired-details-plus")
  :config
  (setq dired-details-initially-hide nil))


(use-package dired-quick-sort
  :quelpa (dired-quick-sort :fetcher github :repo "emacsmirror/dired-quick-sort"))

(use-package dired-sort-menu
  :quelpa (dired-sort-menu :fetcher github :repo "emacsmirror/dired-sort-menu"))

(use-package dired-sort-menu+
  :quelpa (dired-sort-menu+ :fetcher github :repo "emacsmirror/dired-sort-menu-plus"))




;; Adapt ls for mac
(when (eq system-type 'darwin)
  (require 'ls-lisp)
  ;; use brew install coreutils to get gls
  (setq ls-lisp-use-insert-directory-program t
        insert-directory-program (cond
                                  ((file-exists-p "/usr/local/bin/gls")
                                   "/usr/local/bin/gls")
                                  ((file-exists-p "/opt/homebrew/bin/gls")
                                   "/opt/homebrew/bin/gls"))))


;; Omitting
(setq-default dired-omit-files "^\\.[^.]+"
              dired-omit-mode t)

;; Adapt ls lisp format
(if (boundp 'ls-lisp-ignore-case)
    (setq ls-lisp-ignore-case t))
(if (boundp 'ls-lisp-dirs-first)
    (setq ls-lisp-dirs-first t))
;;(if (boundp 'ls-lisp-use-localized-time-format)
;;    (setq ls-lisp-use-localized-time-format t))
;;(if (boundp 'ls-lisp-format-time-list)
;;    (setq ls-lisp-format-time-list '("%Y-%m-%d %H:%M" "%Y-%m-%d %H:%M")))

(setq ls-lisp-format-time-list '("%b %e %H:%M" "%b %e  %Y"))

(put 'dired-find-alternate-file 'disabled nil)


(setq dired-dwim-target t)

;; Compression
(setq auto-compression-mode t)

;; Recursive
(setq dired-recursive-deletes 'top)
(setq dired-recursive-copies 'always)

;; Details information
(setq dired-listing-switches "-ahlXF")
;;(setq  dired-details-hidden-string "[...]")
;; allow editing file permissions

;;I used dired-narrow to filter the view down to just the pdf files
;;I use C-x C-q to make the dired buffer editable
;;I move to the group write permission spot on the first line and then use multiple cursors to add a cursor for each line
;; I hit w to set the write permission, RET to quit multiple cursors, and C-c C-c to make the change permanent
(setq wdired-allow-to-change-permissions t)



;; Keys
(define-key dired-mode-map (kbd "C-o") 'dired-omit-mode)
(define-key dired-mode-map (kbd "<C-return>") 'dired-open-native)
(define-key dired-mode-map (kbd "e") 'dired-open-externally)


;; Diff
(defun ora-ediff-files ()
  (interactive)
  (let ((files (dired-get-marked-files))
        (wnd (current-window-configuration)))
    (if (<= (length files) 2)
        (let ((file1 (car files))
              (file2 (if (cdr files)
                         (cadr files)
                       (read-file-name
                        "file: "
                        (dired-dwim-target-directory)))))
          (if (file-newer-than-file-p file1 file2)
              (ediff-files file2 file1)
            (ediff-files file1 file2))
          (add-hook 'ediff-after-quit-hook-internal
                    (lambda ()
                      (setq ediff-after-quit-hook-internal nil)
                      (set-window-configuration wnd))))
      (error "no more than 2 files should be marked"))))
(define-key dired-mode-map "E" 'ora-ediff-files)



(use-package peep-dired
  :bind (:map dired-mode-map ( "P" . peep-dired)))


(use-package dired-narrow
  :bind
  (:map dired-mode-map ("N" .  dired-narrow)))


(use-package dired-single
  :bind
  (:map dired-mode-map
        ([return] . dired-single-buffer)
        ([mouse-1]  . dired-single-buffer-mouse)))


(use-package dired-collapse
  :defer
  :init
  (add-hook 'dired-mode-hook 'dired-collapse-mode))



(use-package dired-filter
  :after    dired
  :bind (:map dired-mode-map
              ("/" . dired-filter-map))
  :hook ((dired-mode . dired-filter-mode)
         (dired-mode . dired-filter-group-mode))
  :init (setq dired-filter-revert 'never
              dired-filter-group-saved-groups
              '(("default"
                 ("Git"
                  (directory . ".git")
                  (file . ".gitignore"))
                 ("Directory"
                  (directory))
                 ("PDF"
                  (extension . "pdf"))
                 ("LaTeX"
                  (extension "tex" "bib"))
                 ("Source"
                  (extension "c" "cpp" "hs" "rb" "py" "r" "cs" "el" "lisp" "html" "js" "css"))
                 ("Doc"
                  (extension "md" "rst" "txt"))
                 ("Org"
                  (extension . "org"))
                 ("Archives"
                  (extension "zip" "rar" "gz" "bz2" "tar"))
                 ("Images"
                  (extension "jpg" "JPG" "webp" "png" "PNG" "jpeg" "JPEG" "bmp" "BMP" "TIFF" "tiff" "gif" "GIF"))))))


(use-package dired-subtree
  :bind (:map dired-mode-map
              ("i"  . 'dired-subtree-insert)
              (";" . 'dired-subtree-remove)))


(use-package image-dired+
  :config
  (setq auto-image-file-mode t)
  (eval-after-load 'image-dired+ '(image-diredx-async-mode 1)))


;; (use-package dired-rainbow
;;   :ensure nil
;;   :defer t
;;   :init
;;   (eval-after-load 'dired '(require 'dired-rainbow))
;;   :config
;;   ;; (setq dired-audio-files-extensions
;;   ;;   '("mp3" "MP3" "ogg" "OGG" "flac" "FLAC" "wav" "WAV")
;;   ;;   "Dired Audio files extensions")
;;   ;; (dired-rainbow-define audio "#329EE8" dired-audio-files-extensions)

;;   ;; (setq dired-video-files-extensions
;;   ;;   '("vob" "VOB" "mkv" "MKV" "mpe" "mpg" "MPG" "mp4" "MP4" "ts" "TS" "m2ts"
;;   ;;     "M2TS" "avi" "AVI" "mov" "MOV" "wmv" "asf" "m2v" "m4v" "mpeg" "MPEG" "tp")
;;   ;;   "Dired Video files extensions")
;;   ;; (dired-rainbow-define video "#455AFC" dired-video-files-extensions)

;;   (dired-rainbow-define html "#4e9a06" ("htm" "html" "xhtml"))
;;   (dired-rainbow-define xml "DarkGreen" ("xml" "xsd" "xsl" "xslt" "wsdl"))
;;   (dired-rainbow-define document "#ce5c00" ("doc" "docx" "odt" "pdb" "pdf" "ps" "rtf" "djvu"))
;;   (dired-rainbow-define image "#ff4b4b" ("jpg" "png" "jpeg" "gif"))
;;   (dired-rainbow-define sourcefile "#3F82FD" ("el" "groovy" "gradle" "py" "c" "cc" "h" "java" "pl" "rb"))
;;   (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
;;   (dired-rainbow-define compressed "#ad7fa8" ("zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
;;   (dired-rainbow-define packaged "#e6a8df" ("deb" "rpm"))
;;   (dired-rainbow-define encrypted "LightBlue" ("gpg" "pgp"))
;;   (dired-rainbow-define-chmod executable-unix "Green" "-.*x.*")
;;   (dired-rainbow-define log (:inherit default :italic t) ".*\\.log")
;;   )


(use-package dired-rainbow
  :after    dired
  :commands dired-rainbow-define dired-rainbow-define-chmod
  :init
  (eval-after-load 'dired '(require 'dired-rainbow))
  :config
  (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
  (dired-rainbow-define html        "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
  (dired-rainbow-define xml         "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
  (dired-rainbow-define document    "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
  (dired-rainbow-define markdown    "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
  (dired-rainbow-define database    "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
  (dired-rainbow-define media       "#de751f" ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
  (dired-rainbow-define image       "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
  (dired-rainbow-define log         "#c17d11" ("log"))
  (dired-rainbow-define shell       "#06993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
  (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
  (dired-rainbow-define compiled    "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
  (dired-rainbow-define executable  "#8cc4ff" ("exe" "msi"))
  (dired-rainbow-define compressed  "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
  (dired-rainbow-define packaged    "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
  (dired-rainbow-define encrypted   "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
  (dired-rainbow-define fonts       "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
  (dired-rainbow-define partition   "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
  (dired-rainbow-define vc          "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
  (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*"))





(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-subtree-line-prefix "__")
  (setq dired-sidebar-theme 'icons)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-width 50)
  (setq dired-sidebar-use-custom-font nil))
