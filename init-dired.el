(require 'dired)
(require 'dired-x)

;; Pack and unpacking of files.
;; https://github.com/HKey/dired-atool
;;(use-package dired-atool
;;  :config
;;  (dired-atool-setup))

(use-package dired-avfs)
(use-package dired-dups)
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
(use-package dired-ranger)
(use-package dired-toggle)
(use-package dired-toggle-sudo)
(use-package diredful)



(use-package all-the-icons-dired :ensure t)


(use-package dired-column-widths
  :quelpa (dired-column-widths :fetcher wiki))


;; Use ')' in dired window to show or hide file details
(use-package dired-details
  :quelpa (dired-details :fetcher wiki))
(use-package dired-details+
  :quelpa (dired-details+ :fetcher wiki)
  :config
  (setq dired-details-initially-hide nil))


(use-package dired-sort
  :quelpa (dired-sort :fetcher wiki))

(use-package dired-sort-menu
  :quelpa (dired-sort-menu :fetcher wiki))

(use-package dired-sort-menu+
  :quelpa (dired-sort-menu+ :fetcher wiki))




;; Adapt ls for mac
(when (eq system-type 'darwin)
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program t
        insert-directory-program "/usr/local/bin/gls"))

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
  :ensure t
  :config
  (define-key dired-mode-map (kbd "P") 'peep-dired)
  )


(use-package dired-narrow
  :ensure t
  :config
  (define-key dired-mode-map (kbd "/") 'dired-narrow)
  )


(use-package dired-single
  :ensure t
  :config
  (define-key dired-mode-map [return] 'dired-single-buffer)
  (define-key dired-mode-map [mouse-1] 'dired-single-buffer-mouse)
  )


(use-package dired-collapse
  :ensure t
  :defer
  :init
  (add-hook 'dired-mode-hook 'dired-collapse-mode))


(use-package dired-filter
  :ensure t
  :init
  (add-hook 'dired-mode-hook 'dired-filter-group-mode)
  :config
  (setq dired-filter-group-saved-groups
        '(("default"
           ("Directories" (directory))
           ("PDF"
            (extension . "pdf"))
           ("LaTeX"
            (extension "tex" "bib"))
           ("Org"
            (extension . "org"))
           ("Archives"
            (extension "zip" "rar" "gz" "bz2" "tar"))
           ("Multimedia"
            (extension "ogg" "flv" "mpg" "avi" "mp4" "mp3"))
           )))
  )


(use-package dired-subtree
  :ensure t
  :config
  (define-key dired-mode-map "i" 'dired-subtree-insert)
  (define-key dired-mode-map ";" 'dired-subtree-remove)
  )


(use-package image-dired+
  :ensure t
  :config
  (setq auto-image-file-mode t)
  (eval-after-load 'image-dired+ '(image-diredx-async-mode 1)))


(use-package dired-rainbow
  :ensure t
  :defer t
  :init
  (eval-after-load 'dired '(require 'dired-rainbow))

  :config

  ;; (setq dired-audio-files-extensions
  ;;   '("mp3" "MP3" "ogg" "OGG" "flac" "FLAC" "wav" "WAV")
  ;;   "Dired Audio files extensions")
  ;; (dired-rainbow-define audio "#329EE8" dired-audio-files-extensions)

  ;; (setq dired-video-files-extensions
  ;;   '("vob" "VOB" "mkv" "MKV" "mpe" "mpg" "MPG" "mp4" "MP4" "ts" "TS" "m2ts"
  ;;     "M2TS" "avi" "AVI" "mov" "MOV" "wmv" "asf" "m2v" "m4v" "mpeg" "MPEG" "tp")
  ;;   "Dired Video files extensions")
  ;; (dired-rainbow-define video "#455AFC" dired-video-files-extensions)

  (dired-rainbow-define html "#4e9a06" ("htm" "html" "xhtml"))
  (dired-rainbow-define xml "DarkGreen" ("xml" "xsd" "xsl" "xslt" "wsdl"))

  (dired-rainbow-define document "#ce5c00" ("doc" "docx" "odt" "pdb" "pdf" "ps" "rtf" "djvu"))
  (dired-rainbow-define image "#ff4b4b" ("jpg" "png" "jpeg" "gif"))

  (dired-rainbow-define sourcefile "#3F82FD" ("el" "groovy" "gradle" "py" "c" "cc" "h" "java" "pl" "rb"))

  (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
  (dired-rainbow-define compressed "#ad7fa8" ("zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
  (dired-rainbow-define packaged "#e6a8df" ("deb" "rpm"))
  (dired-rainbow-define encrypted "LightBlue" ("gpg" "pgp"))

  (dired-rainbow-define-chmod executable-unix "Green" "-.*x.*")

  (dired-rainbow-define log (:inherit default :italic t) ".*\\.log")
  )
