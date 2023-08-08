;;; lilypond-init.el --- Startup code for LilyPond mode
;;
;; Instructions, extracted from Documentation/topdocs/INSTALL.texi:

;; Emacs mode for entering music and running LilyPond is contained in
;; the source archive as `lilypond-mode.el', `lilypond-indent.el',
;; `lilypond-font-lock.el' and `lilypond-words.el'. You should install
;; these files to a directory included in your `load-path'.
;; File `lilypond-init.el' should be placed to `load-path/site-start.d/'
;; or appended to your `~/.emacs' or `~/.emacs.el'.

;; As a user, you may want add your source path or, e.g., `~/site-lisp/' to
;; your `load-path'. Append the following line (modified) to your `~/.emacs':

;; (setq load-path (append (list (expand-file-name "/Users/marcelbecker/src/lilypond/lilypond-binaries/scripts/lilypond/install/share/emacs/site-lisp")) load-path))
(setq load-path (append (list (expand-file-name "/usr/local/share/emacs/site-lisp/lilypond")) load-path))
;; (setenv "PATH" (concat "/Users/marcelbecker/src/lilypond/lilypond-binaries/scripts/lilypond/install/bin:" (getenv "PATH")))

(use-package loop)
(use-package edebug)
(use-package edebug-x)
(use-package edebug-inline-result)


(load "lilypond-mode")

(autoload 'LilyPond-mode "lilypond-mode" "LilyPond Editing Mode" t)
(add-to-list 'auto-mode-alist '("\\.ly\\'" . LilyPond-mode))
(add-to-list 'auto-mode-alist '("\\.ily\\'" . LilyPond-mode))
;; (setq LilyPond-pdf-command
(add-hook 'LilyPond-mode-hook (lambda () (turn-on-font-lock)))
(add-hook 'LilyPond-mode-hook (lambda () (electric-indent-local-mode -1)))
;;(setenv "LYEDITOR" "emacsclient --no-wait +%(line)s:%(column)s %(file)s")
;;(setenv "LYEDITOR" "emacs")


;; (defun LilyPond-source-correlate-handle-LilyPond-region (file line col)
;;   "Translate backward search info with respect to `TeX-region'.
;; That is, if FILE is `TeX-region', update FILE to the real tex
;; file and LINE to (+ LINE offset-of-region).  Else, return nil."

;;   (with-current-buffer (or (find-buffer-visiting file)
;;                            (find-file-noselect file))
;;     (goto-char 0)
;;     ;; Same regexp used in `preview-parse-messages'.  XXX: XEmacs doesn't
;;     ;; support regexp classes, so we can't use "[:digit:]" here.
;;     (when (re-search-forward "!offset(\\([---0-9]+\\))" nil t)
;;       (let ((offset (string-to-number (match-string-no-properties 1))))
;;         (list (expand-file-name (buffer-file-name))
;;               (+ line offset) col)))))

(defun LilyPond-pdf-tools-sync-view ()
  "Focus the focused page/paragraph in `pdf-view-mode'.
If `TeX-source-correlate-mode' is disabled, only find and pop to
the output PDF file.  Used by default for the PDF Tools viewer
entry in `TeX-view-program-list-builtin'."
  ;; Make sure `pdf-tools' is at least in the `load-path', but the user must
  ;; take care of properly loading and installing the package.  We used to test
  ;; "(featurep 'pdf-tools)", but that doesn't play well with deferred loading.
  (unless (fboundp 'pdf-tools-install)
    (error "PDF Tools are not available"))
  (unless (string-equal major-mode "LilyPond-mode")
    (error "PDF Tools only work with buffers in LilyPond-mode"))
  (let* ((filename (file-name-sans-extension buffer-file-name))
         (pdf (concat (substring (LilyPond-get-master-file) 0 -3) ".pdf"))
         )
    (cl-destructuring-bind (pdf page _x1 y1 _x2 _y2)
        (find-position-in-pdf-file pdf)
      ;; (message "Pdf %s page %s x1 %s y1 %s" pdf page _x1 y1)
      ;;(pop-to-buffer (or (find-buffer-visiting pdf)
      ;;               (find-file-noselect pdf)))))
      (let ((buffer (or (find-buffer-visiting pdf)
                        (find-file-noselect pdf))))
        (with-selected-window (display-buffer
                               buffer pdf-sync-forward-display-action)
          (pdf-util-assert-pdf-window)
          (when page
            (pdf-view-goto-page page)
            (when y1
              (let ((top (* y1 (cdr (pdf-view-image-size))))
                    (edge-x (* _x1 (car (pdf-view-image-size))))
                    )
                 (message "y1 %s top %s _x1 %s edge-x %s image size %s" y1 top _x1 edge-x (pdf-view-image-size))
                (pdf-lilypond-util-tooltip-arrow (round top) (round edge-x) 10))))
          ;;(with-current-buffer buffer
          ;;(run-hooks 'pdf-sync-forward-hook))
          )))))



;; (defun pdf-lilypond-util-tooltip-arrow-old (image-top image-left &optional timeout)
;;   (pdf-util-assert-pdf-window)
;;   (when (floatp image-top)
;;     (setq image-top
;;           (round (* image-top (cdr (pdf-view-image-size))))))
;;   (let* ((x-gtk-use-system-tooltips t) ;allow for display property in tooltip
;;          (dx (+
;;               image-left
;;               (or (car (window-margins)) 0)
;;               (car (window-fringes))))
;;          (dy image-top)
;;          (pos (list dx dy (+ dx (* 2 (frame-char-width))) (+ dy (* 2 (frame-char-height)))))
;;          (vscroll
;;           (pdf-util-required-vscroll pos))
;;          (tooltip-frame-parameters
;;           `((border-width . 1)
;;             (internal-border-width . 1)
;;             (width . 50)
;;             (height . 50)
;;             ,@tooltip-frame-parameters))
;;          (tooltip-hide-delay (or timeout 10)))


;;     (when vscroll
;;       (image-set-window-vscroll vscroll))
;;     (setq dx image-left)
;;     (setq dy (max 0 (- dy
;;                        (cdr (pdf-view-image-offset))
;;                        (window-vscroll nil t)
;;                        (frame-char-height))))
;;     (setq dy image-top)
;;     (when (overlay-get (pdf-view-current-overlay) 'before-string)
;;       (let* ((e (window-inside-pixel-edges))
;;              (xw (pdf-util-with-edges (e) e-width)))
;;         (cl-incf dx (/ (- xw (car (pdf-view-image-size t))) 2))))


;;     (pdf-util-tooltip-in-window
;;      (propertize
;;       " \n " 'display (propertize
;;                        "\u2192" ;;right arrow
;;                        'display '(height 5)
;;                        'face `(:foreground
;;                                "orange red"
;;                                :background
;;                                ,(if (bound-and-true-p pdf-view-midnight-minor-mode)
;;                                     (cdr pdf-view-midnight-colors)
;;                                   "white"))))
;;      dx dy)))


(defun pdf-lilypond-util-tooltip-arrow (image-top image-left &optional timeout)
  (pdf-util-assert-pdf-window)
  (message "Tooltip Arrow Image-Top = %s image-left = %s" image-top image-left)
  (when (floatp image-top)
    (setq image-top
          (round (* image-top (cdr (pdf-view-image-size))))))
  (message "Tooltip Arrow Image-Top = %s image-left = %s" image-top image-left)
  (let* (x-gtk-use-system-tooltips ;allow for display property in tooltip
         (dx (+ image-left
              (or (car (window-margins)) 0)
                (car (window-fringes))))
         (dy image-top)
         (pos (list dx dy (+ dx (* 2 (frame-char-width)))
                    (+ dy (* 2 (frame-char-height)))))
         (vscroll
          (pdf-util-required-vscroll pos))
         (tooltip-frame-parameters
          `((border-width . 0)
            (internal-border-width . 0)
            (width . 0)
            (height . 0)
            (background . red)
            (foreground . blue)
            ,@tooltip-frame-parameters))
         (tooltip-hide-delay (or timeout 3)))
    (message "Tooltip arrow Before dx = %s dy = %s vscroll %s" dx dy vscroll)
    (when vscroll
      (image-set-window-vscroll vscroll))
    (setq dy (max 0 (- dy
                       (cdr (pdf-view-image-offset))
                       (window-vscroll nil t)
                       (frame-char-height))))
    (when (overlay-get (pdf-view-current-overlay) 'before-string)
      (let* ((e (window-inside-pixel-edges))
             (xw (pdf-util-with-edges (e) e-width)))
        (message "Tooltip arrow dx = %s dy = %s xw %s" dx dy xw)
        (cl-incf dx (/ (- xw (car (pdf-view-image-size t))) 2))))
    (message "Tooltip arrow dx = %s dy = %s " dx dy)
    (pdf-util-tooltip-in-window
     (propertize
      " a\n\ta\n\ta\ta\n " 'display (propertize
                    "\u2192" ;;right arrow
                    'display '(height 10)
                    'face `(:foreground
                            "orange red"
                            :background "black"

                            ;; ,(cond
                            ;;   ((bound-and-true-p pdf-view-midnight-minor-mode)
                            ;;    (cdr pdf-view-midnight-colors))
                            ;;   ((bound-and-true-p pdf-view-themed-minor-mode)
                            ;;    (face-background 'default nil))
                            ;;   (t "red"))

                            )))
     dx dy)))
     ;;image-left image-top)))



(defun lilypond-sync-forward-search ()
  "Display the PDF location corresponding to LINE, COLUMN."
  (interactive)
  (LilyPond-pdf-tools-sync-view))


(defun find-position-in-pdf-file (pdf)
  (let* ((filename (buffer-file-name))
         (line (line-number-at-pos))
         (column (current-column))
         position-in-file)
    (cl-loop
     for page from 1 to (pdf-info-number-of-pages pdf) ;; From pdf-tools/pdf-sync.el
     if position-in-file
     do (progn
          ;; (message "Returning position in file from 1\n%s" position-in-file)
          (cl-return position-in-file))
     else
     do (cl-loop
         for annotation in (pdf-info-pagelinks page pdf)
         if position-in-file
         do (progn
              ;; (message "Returning position in file from 2\n%s" position-in-file)
              (cl-return position-in-file))
         else
         do (let ((uri (alist-get 'uri annotation))
                  (edges (alist-get 'edges annotation)))
              ;; (message "Page %i \nAnnot %s\nEdges %s" page uri edges)
              (save-match-data
                (progn
                  (string-match "textedit://\\(/.*\\):\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\)" uri)
                  (let* ((filename  (match-string 1 uri))
                         (uri-line (string-to-number (match-string 2 uri)))
                         (uri-column1 (string-to-number (match-string 3 uri)))
                         (uri-column2 (string-to-number (match-string 4 uri))))
                    (when (and (string-equal filename (buffer-file-name))
                               (= uri-line line)
                               (>= column uri-column1)
                               (<= column uri-column2))
                      ;; (message "Line %d Col %d returning position for URL %s" line column uri)
                      (setq position-in-file (cons pdf (cons page edges)))
                      ;; (message "Position-in-file %s" position-in-file)
                      (cl-return position-in-file))))))))
    (progn
      ;; (message "Returning position in file from bottom of function\n%s" position-in-file)
      (unless position-in-file
        ;; (message "Position in file is empty")
        (setq position-in-file  (cons pdf (cons 1  '(0.0 0.0 0.0 0.0)))))
      position-in-file
      )))




;;;;;;
;; Modified from LilyPond-mode.el to change the PDFView function.
;;
(defun LilyPond-command (name file)
  "Run command NAME on the file you get by calling FILE.

FILE is a function return a file name.  It has one optional argument,
the extension to use on the file.

Use the information in LilyPond-command-alist to determine how to run the
command."

  (let ((entry (assoc name LilyPond-command-alist)))
    (if entry
        (let ((command (LilyPond-command-expand (cadr entry)
                                                (apply file nil)))
              (jobs nil)
              (job-string "no jobs"))
          (if (member name (list  "ViewPS"));; (member name (list "View" "ViewPS"))
              ;; is USR1 a right signal for viewps?
              (let ((buffer-xdvi (get-buffer-create (concat "*" name "*"))))
                ;; what if XEDITOR is set to gedit or so, should we steal it?
                (if (not (getenv "XEDITOR"))
                    (setenv "XEDITOR" "emacsclient --no-wait +%l:%c %f"))
                (if LilyPond-kick-xdvi
                    (let ((process-xdvi (get-buffer-process buffer-xdvi)))
                      (if process-xdvi
                          (signal-process (process-id process-xdvi) 'SIGUSR1)
                        (LilyPond-shell-process name buffer-xdvi command)))
                  (LilyPond-shell-process name buffer-xdvi command)))
            (progn
              (when  (member name '("View" "ViewPDF")) ;; (string-equal name "ViewPDF")
                (setq job-string "pdf-tools")
                (LilyPond-pdf-tools-sync-view)
                )
              (if (string-equal name "Midi")
                  (progn
                    (setq command (concat LilyPond-midi-command " " (LilyPond-string-current-midi)))
                    (if (LilyPond-kill-midi)
                        (setq job-string nil)))) ; either stop or start playing
              (if (string-equal name "MidiAll")
                  (progn
                    (setq command (concat LilyPond-all-midi-command " " (LilyPond-string-all-midi)))
                    (LilyPond-kill-midi))) ; stop and start playing
              (if (and (member name (list "Midi" "MidiAll")) job-string)
                  (if (file-newer-than-file-p
                       (LilyPond-get-master-file)
                       (concat (substring (LilyPond-get-master-file) 0 -3) ".midi"))
                      (if (y-or-n-p "Midi older than source. Reformat midi?")
                          (progn
                            (LilyPond-command-formatmidi)
                            (while (LilyPond-running)
                              (message "Starts playing midi once it is built.")
                              (sit-for 0 100))))))
              (if (member name (list "LilyPond" "TeX" "2Midi" "2PS"
                                     "Book" "LaTeX"))
                  (if (setq jobs (LilyPond-running))
                      (progn
                        (setq job-string "Process") ; could also suggest compiling after process has ended
                        (while jobs
                          (setq job-string (concat job-string " \"" (pop jobs) "\"")))
                        (setq job-string (concat job-string " is already running; kill it to proceed "))
                        (if (y-or-n-p job-string)
                            (progn
                              (setq job-string "no jobs")
                              (LilyPond-kill-jobs)
                              (while (LilyPond-running)
                                (sit-for 0 100)))
                          (setq job-string nil)))))

              (setq LilyPond-command-next
                    (let* ((entry (assoc name LilyPond-command-alist))
                           (next-command (nth 3 (cdr entry))))
                      (or next-command
                          LilyPond-command-default)))

              (if (string-equal job-string "no jobs")
                  (LilyPond-compile-file command name))))))))

;; (unless (assoc "View" LilyPond-command-alist )
;;   (add-to-list 'TeX-view-program-list)
;;                '("View" TeX-pdf-tools-sync-view)))

;;(add-to-list 'LilyPond-command-alist   '("View" (TeX-pdf-tools-sync-view)))
;;(setq LilyPond-pdf-command "/Applications/Skim.app/Contents/MacOS/Skim")


;; From pdf-tools/pdf-sync.el
;;(pdf-info-number-of-pages "/Users/marcelbecker/src/lilypond/scores/FantasiaParaUnGentilhombre/fantasia_edited.pdf")


;;(let ((annotations (pdf-info-pagelinks 1 ;;"/Users/marcelbecker/src/lilypond/scores/FantasiaParaUnGentilhombre/fantasia_edited.pdf")))
;; (cl-loop for anot in annotations
;;       do (print anot)))
;; Look at pdf-toos/pdf-sync-forward-search for how to go to the
;; correct position in the pdf.


;; From pdf-tools/pdf-links.el
;; If the link contains textedit, then open the file using URI.
(defun pdf-links-browse-uri-default (uri)
  "Modified version of the pdf-tools function that opens a URI.
   Modified to include handling textedit uri schemas created by LilyPond.

Open the string URI using Org.

  Wraps the URI in \[\[ ... \]\] and calls `org-open-link-from-string'
  on the resulting string."
  (cl-check-type uri string)

  (if (string-prefix-p "textedit" uri)
      (progn (message "Opening textedir uri %s" uri)
             (lilypond-open-textedit-uri uri))
    (progn
      (message "Opening `%s' with Org" uri)
      (cond
       ((fboundp 'org-link-open-from-string)
        (org-link-open-from-string (format "[[%s]]" uri)))
       ;; For Org 9.2 and older
       ((fboundp 'org-open-link-from-string)
        (org-open-link-from-string (format "[[%s]]" uri)))))))


(defun lilypond-open-textedit-uri (uri)
  (save-match-data
    (and (string-match "textedit://\\(/.*\\):\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\)" uri)
         (let* ((split-width-threshold 0)
                (split-height-threshold nil)
                (filename  (match-string 1 uri))
                (line (string-to-number (match-string 2 uri)))
                (column (string-to-number (match-string 3 uri))))
           ;; (message "File %s line %s column %s" filename line column)
           (find-file-other-window filename)
           (goto-char (point-min))
           (forward-line (1- line))
           (move-to-column column)
           ))))



(defun bury-compile-buffer-if-successful (buffer string)
  "Bury a compilation buffer if succeeded without warnings "
  (when (and
         (buffer-live-p buffer)
         (string-match "compilation" (buffer-name buffer))
         (string-match "finished" string)
         (not
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward "warning" nil t))))
    (run-with-timer 1 nil
                    (lambda (buf)
                      (delete-window (get-buffer-window (get-buffer "*compilation*")))
                      ;;(bury-buffer buf)
                      ;;(switch-to-prev-buffer (get-buffer-window buf) 'kill)
                      )
                    buffer)
    ;;(delete-window (get-buffer-window (get-buffer "*compilation*")))
    ))
(add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)



;; reduce the size of compilation window
(defun my-compilation-hook ()
  (when (not (get-buffer-window "*compilation*"))
    (save-selected-window
      (save-excursion
        (let* ((w (split-window-vertically))
               (h (window-height w)))
          ;; (message "W %s H %s" w h)
          (select-window w)
          ;; (message "select window")
          (switch-to-buffer "*compilation*")
          ;; (message "switch to buffer")
          ;;(shrink-window (- h 10))
          (shrink-window  10)
          )))))
(add-hook 'compilation-mode-hook 'my-compilation-hook)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Test for performance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun LilyPond-calculate-indent ()
  "Return appropriate indentation for current line as lilypond code.
In usual case returns an integer: the column to indent to.
Returns nil if line starts inside a string"
  (save-excursion
    (message "calculate indent point before %s" (point))
    (beginning-of-line)
    (message "calculate indent point after beginning of line %s" (point))
    (let ((indent-point (point))
          (case-fold-search nil)
          state)
      (setq containing-sexp (save-excursion (LilyPond-scan-containing-sexp)))
      (message "calculate indent point after scan containing sexp point %s Containing sexp %s" (point) containing-sexp)
      (beginning-of-defun)
      (message "calculate indent point after beginning of defun point %s indent-point %s" (point) indent-point)
      (while (< (point) indent-point)
        (message "calculate indent inside while loop  point %s indent-point %s" (point) indent-point)
        (setq state (parse-partial-sexp (point) indent-point 0)))
      ;; (setq containing-sexp (car (cdr state))) is the traditional way for languages
      ;; with simpler parenthesis delimiters
      (message "calculate indent after while loop state =  %s" state)
      (cond ((nth 3 state)
             ;; point is in the middle of a string
             nil)
            ((nth 4 state)
             ;; point is in the middle of a block comment
             (LilyPond-calculate-indent-within-blockcomment))
            ((null containing-sexp)
             ;; Line is at top level - no indent
             (beginning-of-line)
             0)
            (t
             ;; Find previous non-comment character.
             (goto-char indent-point)
             (LilyPond-backward-to-noncomment containing-sexp)
             ;; Now we get the answer.
             ;; Position following last unclosed open.
             (goto-char containing-sexp)
             (or
              ;; Is line first statement after an open brace or bracket?
              ;; If no, find that first statement and indent like it.
              (save-excursion
                (forward-char 1)
                ;; Skip over comments following open brace.
                (skip-chars-forward " \t\n")
                (cond ((looking-at "%{")
                       (while  (progn
                                 (and (not (looking-at "%}"))
                                      (< (point) (point-max))))
                         (forward-line 1)
                         (skip-chars-forward " \t\n"))
                       (forward-line 1)
                       (skip-chars-forward " \t\n"))
                      ((looking-at "%")
                       (while (progn (skip-chars-forward " \t\n")
                                     (looking-at "%"))
                         (forward-line 1))))
                ;; The first following code counts
                ;; if it is before the line we want to indent.
                (and (< (point) indent-point)
                     (current-column)))
              ;; If no previous statement,
              ;; indent it relative to line brace is on.
              ;; For open brace in column zero, don't let statement
              ;; start there too.  If LilyPond-indent-level is zero, use
              ;; LilyPond-brace-offset instead
              (+ (if (and (bolp) (zerop LilyPond-indent-level))
                     (cond ((= (following-char) ?{)
                            LilyPond-brace-offset)
                           ((= (following-char) ?<)
                            LilyPond-angle-offset)
                           ((= (following-char) ?\[)
                            LilyPond-square-offset)
                           ((= (following-char) ?\))
                            LilyPond-scheme-paren-offset)
                           (t
                            0))
                   LilyPond-indent-level)
                 (progn
                   (skip-chars-backward " \t")
                   (current-indentation)))))))))
