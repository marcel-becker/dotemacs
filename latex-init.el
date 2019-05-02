(use-package tex-site
  :ensure auctex
  :defer t
  :after (tex latex)
  :config

  ;; Spelling
  (setq ispell-tex-skip-alists
        '((
           ;;("%\\[" . "%\\]") ; AMStex block comment...
           ;; All the standard LaTeX keywords from L. Lamport's guide:
           ;; \cite, \hspace, \hspace*, \hyphenation, \include, \includeonly
           ;; \input, \label, \nocite, \rule (in ispell - rest included here)
           ("\\\\addcontentsline"              ispell-tex-arg-end 2)
           ("\\\\add\\(tocontents\\|vspace\\)" ispell-tex-arg-end)
           ("\\\\\\([aA]lph\\|arabic\\)"   ispell-tex-arg-end)
           ("\\\\author"                         ispell-tex-arg-end)
           ;; New regexps here --- kjh
           ("\\\\\\(text\\|paren\\)cite" ispell-tex-arg-end)
           ("\\\\cite\\(t\\|p\\|year\\|yearpar\\)" ispell-tex-arg-end)
           ("\\\\bibliographystyle"                ispell-tex-arg-end)
           ("\\\\makebox"                  ispell-tex-arg-end 0)
           ("\\\\e?psfig"                  ispell-tex-arg-end)
           ("\\\\document\\(class\\|style\\)" .
            "\\\\begin[ \t\n]*{[ \t\n]*document[ \t\n]*}"))
          (
           ;; delimited with \begin.  In ispell: displaymath, eqnarray,
           ;; eqnarray*, equation, minipage, picture, tabular,
           ;; tabular* (ispell)
           ("\\(figure\\|table\\)\\*?"     ispell-tex-arg-end 0)
           ("\\(equation\\|eqnarray\\)\\*?"     ispell-tex-arg-end 0)
           ("list"                                 ispell-tex-arg-end 2)
           ("program" . "\\\\end[ \t\n]*{[ \t\n]*program[ \t\n]*}")
           ("verbatim\\*?"."\\\\end[ \t\n]*{[ \t\n]*verbatim\\*?[ \t\n]*}")
           ("lstlisting\\*?"."\\\\end[ \t\n]*{[ \t\n]*lstlisting\\*?[ \t\n]*}"))))
  )

;;(use-package tex)
;;(use-package latex)
;;(use-package auctex)
;;(use-package auto-complete-auctex :defer t)
(use-package latex-preview-pane)
(use-package auctex-latexmk
  :config
  (auctex-latexmk-setup)
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  )
(use-package company-auctex
  :defer t
  :after (auctex company)
  :config (company-auctex-init))

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
;; the default flyspell behaviour
(put 'LaTex-mode 'flyspell-mode-predicate 'tex-mode-flyspell-verify)

(setq reftex-plug-into-AUCTeX t)
(setq TeX-source-specials-mode t)
(setq-default TeX-master nil) ; Query for master file.
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
(setq TeX-source-correlate-start-server t)
(setq TeX-source-correlate-method 'synctex)
(setq TeX-PDF-mode t)


;;(setq TeX-view-program-list '(("Evince" "evince --page-index=%(outpage) %o")))
;;(setq TeX-view-program-selection '((output-pdf "Evince")))
;;(setq TeX-output-view-style '("^pdf$" "." "C:/Program Files (x86)/SumatraPDF/SumatraPDF.exe %o"))



(add-hook 'LaTeX-mode-hook
          (lambda ()
            (push
             '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
               :help "Run latexmk on file")
             TeX-command-list)))
(add-hook 'TeX-mode-hook
          '(lambda ()
             (setq TeX-command-default "latexmk")
             (setq TeX-view-program-selection
                   (cond (running-ms-windows
                          '((output-pdf "SumatraPDF")
                            (output-dvi "Yap")))
                         (running-linux
                          '((output-pdf "Okular")
                            (output-dvi "Okular")))
                         (running-macos
                          ;;'((output-pdf "Skim"))
                          '((output-pdf "PDF Tools"))

                          )))))


(use-package company-auctex
  :ensure t
  :hook
  (latex-mode . (company-auctex-init)))


(use-package company-bibtex
  :ensure t
  :hook
  (latex-mode . (lambda () (add-to-list (make-local-variable 'company-backends) '(company-bibtex))))
  (org-mode . (lambda () (add-to-list (make-local-variable 'company-backends) '(company-bibtex)))))

(use-package company-reftex
  :ensure t
  :hook
  (latex-mode . (lambda () (add-to-list (make-local-variable 'company-backends) '(company-reftex-labels company-reftex-citations))))
  (org-mode . (lambda () (add-to-list (make-local-variable 'company-backends) '(company-reftex-labels company-reftex-citations)))))

(use-package company-math
  :ensure t
  :hook
  (latex-mode . (lambda () (add-to-list (make-local-variable 'company-backends) '(company-math-symbols-unicode))))
  (org-mode . (lambda () (add-to-list (make-local-variable 'company-backends) '(company-math-symbols-unicode)))))


;; Update PDF buffers after successful LaTeX runs
(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)


(setq TeX-view-program-list
      '(;;("SumatraPDF" "\"C:/Program Files (x86)/SumatraPDF/SumatraPDF.exe\" -reuse-instance %o")
        ;;("Okular" "okular --unique %o#src:%n%b")
        ("Skim" "/Applications/TeX/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))
;;("Skim" "/Applications/TeX/Skim.app/Contents/SharedSupport/displayline %q")))


;; Use pdf-tools to open PDF files
;; Test to see if PDF Tools works better than Skim
(setq TeX-view-program-selection '((output-pdf "PDF Tools")))

;; add "PDF Tools" to the list of possible PDF tools
(unless (assoc "PDF Tools" TeX-view-program-list)
  (add-to-list 'TeX-view-program-list
               '("PDF Tools" TeX-pdf-tools-sync-view)))


;;; Brent.Longborough's .emacs
;;; FROM:
;;; http://tex.stackexchange.com/questions/50827/a-simpletons-guide-to-tex-workflow-with-emacs


;;; AUCTeX
;; Customary Customization, p. 1 and 16 in the manual, and http://www.emacswiki.org/emacs/AUCTeX#toc2
(setq TeX-parse-self t); Enable parse on load.
(setq TeX-auto-save t); Enable parse on save.
(setq-default TeX-master nil)

(setq TeX-PDF-mode t); PDF mode (rather than DVI-mode)

(add-hook 'TeX-mode-hook 'flyspell-mode); Enable Flyspell mode for TeX modes such as AUCTeX. Highlights all misspelled words.
(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode); Enable Flyspell program mode for emacs lisp mode, which highlights all misspelled words in comments and strings.
(setq ispell-dictionary "english"); Default dictionary. To change do M-x ispell-change-dictionary RET.
(add-hook 'TeX-mode-hook
          (lambda () (TeX-fold-mode 1))); Automatically activate TeX-fold-mode.
(setq LaTeX-babel-hyphen nil); Disable language-specific hyphen insertion.

;; " expands into csquotes macros (for this to work babel must be loaded after csquotes).
(setq LaTeX-csquotes-close-quote "}"
      LaTeX-csquotes-open-quote "\\enquote{")

;; LaTeX-math-mode http://www.gnu.org/s/auctex/manual/auctex/Mathematics.html
(add-hook 'TeX-mode-hook 'LaTeX-math-mode)

;;; RefTeX
;; Turn on RefTeX for AUCTeX http://www.gnu.org/s/auctex/manual/reftex/reftex_5.html
(add-hook 'TeX-mode-hook 'turn-on-reftex)

(eval-after-load 'reftex-vars; Is this construct really needed?
  '(progn
     (setq reftex-cite-prompt-optional-args t); Prompt for empty optional arguments in cite macros.
     ;; Make RefTeX interact with AUCTeX, http://www.gnu.org/s/auctex/manual/reftex/AUCTeX_002dRefTeX-Interface.html
     (setq reftex-plug-into-AUCTeX t)
     ;; So that RefTeX also recognizes \addbibresource. Note that you
     ;; can't use $HOME in path for \addbibresource but that "~"
     ;; works.
     (setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource"))
     ;; (setq reftex-default-bibliography '("UNCOMMENT LINE AND INSERT PATH TO YOUR BIBLIOGRAPHY HERE")); So that RefTeX in Org-mode knows bibliography
     (latex-preview-pane-enable)
     (setcdr (assoc 'caption reftex-default-context-regexps) "\\\\\\(rot\\|sub\\)?caption\\*?[[{]"); Recognize \subcaptions, e.g. reftex-citation
     (setq reftex-cite-format; Get ReTeX with biblatex, see http://tex.stackexchange.com/questions/31966/setting-up-reftex-with-biblatex-citation-commands/31992#31992
           '((?t . "\\textcite[]{%l}")
             (?a . "\\autocite[]{%l}")
             (?c . "\\cite[]{%l}")
             (?s . "\\smartcite[]{%l}")
             (?f . "\\footcite[]{%l}")
             (?n . "\\nocite{%l}")
             (?b . "\\blockcquote[]{%l}{}")))))

;; Fontification (remove unnecessary entries as you notice them) http://lists.gnu.org/archive/html/emacs-orgmode/2009-05/msg00236.html http://www.gnu.org/software/auctex/manual/auctex/Fontification-of-macros.html
(setq font-latex-match-reference-keywords
      '(
        ;; biblatex
        ("printbibliography" "[{")
        ("addbibresource" "[{")
        ;; Standard commands
        ;; ("cite" "[{")
        ("Cite" "[{")
        ("parencite" "[{")
        ("Parencite" "[{")
        ("footcite" "[{")
        ("footcitetext" "[{")
        ;; ;; Style-specific commands
        ("textcite" "[{")
        ("Textcite" "[{")
        ("smartcite" "[{")
        ("Smartcite" "[{")
        ("cite*" "[{")
        ("parencite*" "[{")
        ("supercite" "[{")
                                        ; Qualified citation lists
        ("cites" "[{")
        ("Cites" "[{")
        ("parencites" "[{")
        ("Parencites" "[{")
        ("footcites" "[{")
        ("footcitetexts" "[{")
        ("smartcites" "[{")
        ("Smartcites" "[{")
        ("textcites" "[{")
        ("Textcites" "[{")
        ("supercites" "[{")
        ;; Style-independent commands
        ("autocite" "[{")
        ("Autocite" "[{")
        ("autocite*" "[{")
        ("Autocite*" "[{")
        ("autocites" "[{")
        ("Autocites" "[{")
        ;; Text commands
        ("citeauthor" "[{")
        ("Citeauthor" "[{")
        ("citetitle" "[{")
        ("citetitle*" "[{")
        ("citeyear" "[{")
        ("citedate" "[{")
        ("citeurl" "[{")
        ;; Special commands
        ("fullcite" "[{")))

(setq font-latex-match-textual-keywords
      '(
        ;; biblatex brackets
        ("parentext" "{")
        ("brackettext" "{")
        ("hybridblockquote" "[{")
        ;; Auxiliary Commands
        ("textelp" "{")
        ("textelp*" "{")
        ("textins" "{")
        ("textins*" "{")
        ;; supcaption
        ("subcaption" "[{")))

(setq font-latex-match-variable-keywords
      '(
        ;; amsmath
        ("numberwithin" "{")
        ;; enumitem
        ("setlist" "[{")
        ("setlist*" "[{")
        ("newlist" "{")
        ("renewlist" "{")
        ("setlistdepth" "{")
        ("restartlist" "{")))



;; https://emacs.stackexchange.com/questions/40850/run-tex-command-run-all-when-there-are-no-changes-in-tex-file
(defun my-latex-force-compile ()
  "Set the file modification times on the current file, then call
TeX-command-sequence.
This forces a complete recompilation of the document, even if the source
(.tex) is older than any existing outputs (.pdf etc)."
  (interactive)
;;  (set-file-times (buffer-file-name)) ;; sets mod time to current time
  (set-buffer-modified-p t) (save-buffer)
  (TeX-command-sequence t t))
