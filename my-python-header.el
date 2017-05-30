;;; header2.el --- Support for creation and update of file headers.
;;
;; Filename: header2.el
;; Description: Support for creation and update of file headers.
;; Author: Lynn Slater
;;         Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 1996-2015, Drew Adams, all rights reserved.
;; Copyright (C) 1989 Free Software Foundation, Inc.
;; Copyright (C) 1988 Lynn Randolph Slater, Jr.
;; Created: Tue Aug  4 17:06:46 1987
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Thu Jan  1 10:48:48 2015 (-0800)
;;           By: dradams
;;     Update #: 1890
;; URL: http://www.emacswiki.org/header2.el
;; Doc URL: http://emacswiki.org/AutomaticFileHeaders
;; Keywords: tools, docs, maint, abbrev, local
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x, 25.x
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Support for creation and update of file headers.
;;
;; Some of this code and commentary were originally written by Lynn
;; Slater as file `header.el'.  Drew Adams updated it and maintains it
;; as `header2.el'.  The original is here:
;; `http://www.emacswiki.org/cgi-bin/wiki/download/OriginalHeaderEl'.
;;
;; Commands (interactive functions) defined here:
;;
;;   `make-header', `make-revision', `make-divider',
;;   `make-box-comment', `update-file-header'.
;;
;; Other functions defined here:
;;
;;   `auto-make-header', `auto-update-file-header',
;;   `delete-and-forget-line', `header-AFS', `header-author',
;;   `header-blank', `header-code', `header-commentary',
;;   `header-compatibility', `header-copyright',
;;   `header-creation-date', `header-date-string',
;;   `header-description', `header-doc-url',`header-end-line',
;;   `header-eof', `header-file-name', `header-free-software',
;;   `header-history', `header-keywords', `header-lib-requires',
;;   `header-maintainer', `header-mode-line',
;;   `header-modification-author', `header-modification-date',
;;   `header-multiline', `header-pkg-requires',
;;   `header-prefix-string', `header-rcs-id', `header-rcs-log',
;;   `header-sccs', `header-shell', `header-status', `header-title',
;;   `header-toc', `header-update-count', `header-url',
;;   `header-version', `headerable-file-p', `make-box-comment',
;;   `make-divider', `make-revision', `nonempty-comment-end',
;;   `nonempty-comment-start', `register-file-header-action',
;;   `section-comment-start', `true-mode-name', `uniquify-list',
;;   `update-file-name', `update-last-modified-date',
;;   `update-last-modifier', `update-lib-requires',
;;   `update-write-count'.
;;
;; User options (variables) defined here:
;;
;;   `header-copyright-notice', `header-date-format',
;;   `header-history-label', `header-max', `make-header-hook'.
;;
;; Other variables defined here:
;;
;;   `file-header-update-alist', `header-auto-update-enabled',
;;   `header-multiline', `header-prefix-string', `return-to'.
;;
;;
;; To have Emacs update file headers automatically whenever you save a
;; file, put this in your init file (~/.emacs):
;;
;;   (autoload 'auto-update-file-header "header2")
;;   (add-hook 'write-file-hooks 'auto-update-file-header)
;;
;; To have Emacs add a file header whenever you create a new file in
;; some mode, put this in your init file (~/.emacs):
;;
;;   (autoload 'auto-make-header "header2")
;;   (add-hook 'emacs-lisp-mode-hook 'auto-make-header)
;;   (add-hook 'c-mode-common-hook   'auto-make-header)
;;   ...
;;
;;
;;
;; From the original header.el text by Lynn Slater:
;;
;;     This file is particularly useful with the file-declarations
;;     package also by Lynn Slater.  Read the first 20% of this file
;;     to learn how to customize.
;;
;;     From: eddie.mit.edu!think!ames!indetech.com!lrs (Lynn Slater)
;;     To: info-gnu-emacs@prep.ai.mit.edu
;;     Subject: Automatic header creation and maintenance
;;     Date: Wed, 1 Nov 89 09:33 PST
;;
;;     Enclosed is code to automatically create and maintain file
;;     headers.  This code is cleaner and mush more easily customized
;;     than any of my previous header postings.
;;
;;     New in this release are customizations that allow headers to be
;;     created and maintained from the command line.  This is good for
;;     projects with some vi die-hards or when headers are being added
;;     in mass for the first time.
;;
;;     Example:
;;        cd $EMACS/lisp
;;        headers -make *.el
;;
;;     I have found file headers to be very valuable in project
;;     development.  I always know who has been where and how many
;;     times they were there.  Most often, I also know what they did.
;;     The update count and last modified date are very useful in
;;     determining the proper version of a file to use.  I have often
;;     thought that it would be easier to integrate patches from
;;     individuals to gnu tools such as gcc and g++ if I knew for
;;     certain what version of a particular file they were working
;;     from.  If all had headers, I would see the update count and
;;     date in the "diff -c" output and would be able to find or
;;     recreate the file to patch accordingly.
;;
;;     In this message are three files:
;;       header.el - Emacs header functions and instructions
;;       headers.1  - Man page for command line headers useage
;;       headers    - Shell script for command-line headers.
;;
;; Text by Lynn Slater, updated as needed:
;;
;;     Mode-specific headers:
;;     ---------------------
;;      Not all headers need look alike.  Suppose that you have a unix script mode
;;      and want it to have a shell specifier line that all other headers do not
;;      have.  To do this, Place the following line in a hook called when the
;;      mode is invoked or in the code that establishes the mode:
;;
;;         (add-hook 'make-header-hook 'header-shell nil t)

;;      The header building blocks are sensitive to the different comment
;;      characters in different modes.

;;     Mode specific update actions:
;;     ----------------------------
;;      Suppose something needs to be automatically maintained only in certain
;;      modes.  An example is the .TH macro in man pages.  You can create mode-
;;      specific update actions by placing code like the following in the
;;      mode creation function of the mode hook.
;;
;;        (register-file-header-action
;;          "^\.TH[ \t]+[^\" \t]+[ \t]+[^\" \t]+[ \t]+\"\\([^\"]*\\)\""
;;         'update-last-modified-date-macro)
;;
;;     Define individual header elements.  These are the building blocks
;;     used to construct a site specific header.  You may add your own
;;     functions either in this file or in your `.emacs' file.  The
;;     variable `make-header-hook' specifies the functions that will
;;     actually be called.
;;
;; Note on change-control systems:
;;
;;  If you use `header2.el' in a change-control system, such as RCS,
;;  you might need to leave it checked out.  This is because any
;;  change-control keywords in the file will be expanded during
;;  check-in.  Normally, you will want those keywords to be inserted
;;  in file headers unexpanded.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2014/07/23 dadams
;;     header-free-software: Updated per latest GNU boilerplate.
;; 2014/01/13 dadams
;;     Added: nonempty-comment-start, nonempty-comment-end.
;;     Removed variables comment-start-p, comment-end-p.
;;     header-multiline, header-code, header-eof, header-end-line, header-prefix-string:
;;       Use nonempty-comment-end.
;;     header-mode-line, header-end-line: Use nonempty-comment-start.
;;     make-header: Remove let bindings of comment-start-p, comment-end-p.
;; 2013/07/22 dadams
;;     Added: header-pkg-requires, for ELPA/package.el.  Added to make-header-hook.
;; 2012/08/23 dadams
;;     Added: header-doc-url.
;;     make-header-hook: Added header-doc-url to default value.
;; 2011/12/19 dadams
;;     delete-and-forget-line: Use line-end-position, not end-of-line + point.
;; 2011/11/15 dadams
;;     header-date-string:
;;       Use UTC format from http://www.w3.org/TR/NOTE-datetime.  Thx to Lennart Borgman.
;; 2011/02/03 dadams
;;     Added: header-auto-update-enabled.
;;     auto-update-file-header: Respect header-auto-update-enabled.  Thx to Le Wang.
;; 2011/01/04 dadams
;;     Removed autoload cookies from non-interactive functions.
;; 2010/08/03 dadams
;;     update-file-name: Use ---, not just -, in title line, per newer standard.
;;     make-revision: Escape ; in string, for Emacs 20 (else C-M-q problem).
;; 2010/04/12 dadams
;;     header-history-label: Change log -> Change Log.
;; 2009/10/25 dadams
;;     Renamings from lib-require.el.  If you use that library, you must update it.
;;       lib-requires-header -> libreq-file-header
;;       insert-lib-requires-as-comment -> libreq-insert-lib-requires-as-comment
;; 2009/09/24 dadams
;;     header-multiline: Use a marker for END, and go to it after insert multiline.
;;     header-eof: Go to point-max and insert newline.
;; 2008/09/06 dadams
;;     update-write-count: Keep rest of line, after number.  Thx to Johan Vromans.
;;     Added update-VCS-version, commented out.
;; 2008/08/06 dadams
;;     header-date-string: Use %z, not %Z - the latter no longer works on Windows.
;; 2008/07/11 dadams
;;     header-title, header-file-name, header-eof:
;;       Use buffer-file-name, if available.  Thx Juan Miguel Cejuela for suggestion.
;; 2008/03/14 dadams
;;     header-free-software: Update version 2 -> version 3 of GPL.
;; 2008/01/18 dadams
;;     header-creation-date: Added time zone also.  Thx to Sebastian Luque.
;;     Added: header-date-(string|format).
;;     header-creation-date, update-last-modified-date: Use header-date-format.
;; 2007/12/12 dadams
;;     INCOMPATIBLE CHANGE - If you previously used update-file-header as a
;;                           write-file-hook, change it to auto-update-file-header.
;;     Added auto-update-file-header.  Uses new update-file-header.
;;     update-file-header: Made unconditional.  Thx to Lennart Borgman.
;; 2007/03/25 dadams
;;     make-header: Use let*, so comment-end-p is bound in header-prefix-string.
;; 2006/01/13 dadams
;;     Added: header-url.
;; 2006/01/07 dadams
;;     Added :link.
;; 2005/11/04 dadams
;;     update-last-modified-date: Added timezone.
;; 2005/10/21 dadams
;;     Added header-free-software, header-multiline (vars & fns).
;;     Updated make-header-hooks.
;;     update-lib-requires:
;;       Use error msg if insert-lib-requires-as-comment errors.
;;       Made buffer-file-name filter outermost.
;;       Got rid of locate-library filter.
;;     header-code, header-eof: Include comment-end case.
;;     Changed defvar to defcustom.
;;     auto-make-header: Make sure its a file buffer.
;;     Protect lib-requires-header with boundp.
;;     Renamed make-header-hooks to make-header-hook.
;;     Cleaned up Commentary.  Added .emacs instructions, note on change control.
;;     header-prefix-string: Don't bother to bind comment-end-p.
;; 2005/10/19 dadams
;;     Increased header max default value from 2000 to 50000.
;; 2005/10/18 dadams
;;     Added: update-lib-requires, header-lib-requires, header-version.
;;     make-header-hooks:
;;       Use header-version, not header-rcs-id.  Use header-lib-requires.
;;       Don't use header-rcs-log.
;;     update-last-modifier: inlined code for non-empty-name-p.
;;     Require lib-requires.el.
;; 2004/10/01 dadams
;;     auto-make-header: not if read-only
;;     header-rcs-log: Split string so it won't be overwritten by vc.el
;;       Thanks to Steve Taylor for this fix.
;; 2004/06/04 dadams
;;     header-eof: Removed "`" and "'" around file name.
;; 1996/04/04 dadams
;;     Mods for modes like C, etc.
;;     1. make-header-hooks: Removed header-blank before: header-commentary,
;;        header-history and header-code.  Added 2 header-blank's after
;;        header-commentary.
;;     2. Added section-comment-start.
;;     3. header-file-name: Only use header-prefix-string if 1-char comment-start.
;;     4. header-commentary,header-history,header-code: Use section-comment-start.
;;     5. header-code: Only add ":\n\n\n\n\n" if 1-char comment-start.
;;     6. header-eof: Removed extra " ".
;; 1996/03/18 dadams
;;     Added defvars for return-to, explicit-shell-file-name, c-style .
;; 1996/02/12 dadams
;;     Added auto-make-header.
;; 1995/09/04 dadams
;;     Adapted to std GNU maintenance form (see file lisp-mnt.el).
;;     1) Distinguished sections from subsections.  Changed order.
;;     2) No longer use header-mode-line (conflicts with GNU maintenance std).
;;     3) Added header-eof, header-history-label.
;;     4) Removed header-purpose (use just header-commentary).
;;     5) Redefined: make-revision, header-file-name, header-history,
;;        header-rcs-id, header-sccs, header-copyright.
;; 1995/08/08 dadams
;;     Added header-maintainer, header-keywords, header-commentary, header-code.
;; 1995/08/02 dadams
;;     header-rcs -> header-rcs-id, header-rcs-log, and changed order.
;; 1995/07/31 dadams
;;     1. Corrected SCCS & RCS strings (need to be uninstantiated here).\
;;     2. Added defvar for header-prefix-string (not really needed).
;;     3. Commented out stuff that needs Lynn Slater's command-line-hooks.
;; 28-Apr-1995 dadams
;;     Added default for comment-start in make-revision.
;; 11/11/89 -- Darryl Okahata, HP NMD (darrylo%hpnmd@hpcea.HP.COM)
;; 25-Sep-1989          Lynn Slater
;;    added -default-mode ahd headerable-file-p
;; 10-Sep-1989          Lynn Slater
;;    Seperated out header-mode-line and header-end.  Headers are now really
;;    easy to modify.  Added instructions for mode-specific headers.
;; 8-Aug-1989           Lynn Slater
;;    Changed structure to allow site/user customized headers
;; 24-Jun-1989          Lynn Slater
;;    restructured file, made the order of header actions not be significant.
;; 22-Jun-1989          Lynn Slater
;;    Made file header actions easier to declare
;;    Made sccs and rcs support be user settable.
;;    Added c-style support
;; 25-Jan-1989          Lynn Slater
;;    Added make-doc command
;; 25-Jan-1989          Lynn Slater
;;    made the make-revision command include the last-modified data
;; 31-Aug-1988          Lynn Slater
;;    Made the make-revision work in most modes
;;    Added the update-file-name command
;; 1-Mar-1988           Lynn Slater
;;   made the headers be as sensitive as possible to the proper
;;   comment chars.
;; 1-Mar-1988           Lynn Slater
;;   Made the mode be declared in each header
;; 26-Feb-1988          Lynn Slater
;;   added the make-revision call
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'header2)                      ; Ensure loaded before compile.


;; Change this as you like.
;; Note that the Elisp manual, node Library Headers, suggests putting copyright just
;; after header-description.  That is not done here, by default, because I feel that
;; copyright is not the first information people are looking for.  Otherwise, this
;; default value corresponds to what the Elisp manual recommends for Emacs Lisp.
(setq make-header-hook '(
                         ;;header-mode-line
                         header-python-script
                         ;;header-title
                         header-blank
                         header-divider
                         header-blank
                         header-file-name
                         header-description
                         ;;header-status
                         header-author
                         ;;header-maintainer
                         ;;header-copyright
                         header-creation-date
                         header-blank
                         header-divider
                         header-blank
                         ;;header-rcs-id
                         ;;header-version
                         ;;header-pkg-requires
                         ;;header-sccs
                         ;;header-modification-date
                         ;;header-modification-author
                         ;;header-update-count
                         ;;header-url
                         ;;header-doc-url
                         ;;header-keywords
                         ;;header-compatibility
                         ;;header-blank
                         ;;header-lib-requires
                         ;;header-end-line
                         ;;header-commentary
                         ;;header-blank
                         ;;header-blank
                         ;;header-blank
                         ;;header-end-line
                         ;;header-history
                         ;;header-blank
                         ;;header-blank
                         ;; header-rcs-log
                         ;;header-end-line
                         ;;header-free-software
                         header-blank
                         header-code
                         header-main
                         header-eof
                         ))



;;; Functions ----------------------------------------------

(defsubst nonempty-comment-start ()
  "Return `comment-start', or nil if it is an empty string."
  (and (not (equal "" comment-start))  comment-start))

(defsubst nonempty-comment-end ()
  "Return `comment-end', or nil if it is an empty string."
  (and (not (equal "" comment-end))  comment-end))

(defsubst header-blank ()
  "Insert an empty comment to file header (after `header-prefix-string')."
  (insert header-prefix-string  "\n"))

(defsubst header-divider ()
  "Insert a line with a number of header prefix string (after `header-prefix-string')."
  (insert (make-string 70 (aref comment-start 0))  "\n"))


;; Major section headings

(defsubst section-comment-start ()
  "Comment start of major section headings."
  (if (= (length comment-start) 1)      ; e.g. Lisp: ";; \n;;;"
      (concat header-prefix-string "\n" comment-start header-prefix-string)
    (concat "\n" comment-start)))       ; e.g. C: "\n/*"

(defsubst header-title ()
  "Insert buffer's file name and leave room for a description.
In `emacs-lisp-mode', this should produce the title line for library
packages."
  (insert (concat comment-start (and (= 1 (length comment-start)) header-prefix-string)
                  (if (buffer-file-name)
                      (file-name-nondirectory (buffer-file-name))
                    (buffer-name))
                  " File: " "\n"))
  (setq return-to  (1- (point))))

(defsubst header-file-name ()
  "Insert \"Filename: \" line, using buffer's file name."
  (insert header-prefix-string " Filename: "
          (if (buffer-file-name)
              (file-name-nondirectory (buffer-file-name))
            (buffer-name))
          "\n"))

(defsubst header-description ()
  "Insert \"Description: \" line."
  (insert header-prefix-string " Description: \n"))

(defsubst header-author ()
  "Insert current user's name (`user-full-name') as this file's author."
  (insert header-prefix-string " Author: " (user-full-name) " (mbecker@1nn0v8.io)\n"))

(defsubst header-maintainer ()
  "Insert \"Maintainer: \" line."
  (insert header-prefix-string " Maintainer: \n"))

(defun header-copyright ()
  "Insert `header-copyright-notice', unless nil."
  (when header-copyright-notice
    (let ((start  (point)))
      (insert header-copyright-notice)
      (save-restriction
        (narrow-to-region start (point))
        (goto-char (point-min))
        ;; Must now insert header prefix.  Cannot just replace string,
        ;; because that would cause too many undo boundries.
        (insert header-prefix-string)
        (while (progn (skip-chars-forward "^\n") (looking-at "\n"))
          (forward-char 1) (unless (eolp) (insert header-prefix-string)))
        (goto-char (point-max))))))

(defsubst header-creation-date ()
  "Insert today's time, date, and time zone as file creation date."
  (insert header-prefix-string " Created: ")
  (insert (header-date-string) "\n"))

(defun header-date-string ()
  "Current date and time."
  (format-time-string
   (cond ((stringp header-date-format) header-date-format)
         (header-date-format "%a %B %e %Y (%T) (%z)")
         (t                  "%Y-%m-%dT%T%z")) ; An alternative: "%a %b %e %T %Y (UTC)"
   (current-time)
   (not header-date-format)))

(defsubst header-rcs-id ()
  "Insert lines to record RCS id information (\"$Id$\n\")."
  (insert header-prefix-string "Version: $Id$\n"))

(defsubst header-version ()
  "Insert lines to record version information."
  (insert header-prefix-string "Version: \n"))

(defsubst header-sccs ()
  "Insert a line to record SCCS version information."
  (insert header-prefix-string "Version: %W%    %E%    %U%\n"))

(defsubst header-pkg-requires ()
  "Insert a line to record `Package-Requires' information."
  (insert header-prefix-string "Package-Requires: ()\n"))

(defsubst header-commentary ()
  "Insert \"Commentary: \" line."
  (insert (concat (section-comment-start) "Commentary: \n")))

(defsubst header-history ()
  "Insert `header-history-label' into header for use by `make-revision'.
Without this, `make-revision' inserts `header-history-label' after the header."
  (insert (concat (section-comment-start) header-history-label "\n")))

(defun header-free-software ()
  "Insert text saying that this is free software."
  (let ((header-multiline  header-free-software))
    (header-multiline)))

(defun header-multiline ()
  "Insert multiline comment.  The comment text is in `header-multiline'."
  (let ((lineno  1)
        beg end nb-lines)
    (beginning-of-line)
    (if (nonempty-comment-end)
        (insert "\n" comment-start)
      (header-blank)
      (insert header-prefix-string))
    (setq beg  (point))
    (insert header-multiline)
    (setq end       (point-marker)
          nb-lines  (count-lines beg end))
    (goto-char beg)
    (forward-line 1)
    (while (< lineno nb-lines)
      (insert header-prefix-string)
      (forward-line 1)
      (setq lineno  (1+ lineno)))
    (goto-char end)
    (when (nonempty-comment-end) (insert "\n"))
    (insert comment-end)
    (insert "\n")
    (unless (nonempty-comment-end)
      (header-blank)
      (header-end-line))))

(defsubst header-code ()
  "Insert \"Code: \" line."
  (insert (concat "\n" (section-comment-start) " Code:" (nonempty-comment-end) "\n")))


(defsubst header-main ()
  "Insert python main function."
  (insert "def main():\n")
  (insert "    pass\n\n\n")
  (insert "if __name__ == \"__main__\":\n")
  (insert "    main()\n"))



(defsubst header-eof ()
  "Insert comment indicating end of file."
  (goto-char (point-max))
  (insert "\n")
  (unless (nonempty-comment-end) (header-end-line))
  (insert comment-start
          (concat (and (= 1 (length comment-start)) header-prefix-string)
                  (if (buffer-file-name)
                      (file-name-nondirectory (buffer-file-name))
                    (buffer-name))
                  " ends here"
                  (or (nonempty-comment-end) "\n"))))

(defsubst header-modification-date ()
  "Insert todays date as the time of last modification.
This is normally overwritten with each file save."
  (insert header-prefix-string "Last-Updated: \n"))

(defsubst header-modification-author ()
  "Insert current user's name as the last person who modified the file.
This is normally overwritten with each file save."
  (insert header-prefix-string "          By: \n"))

(defsubst header-update-count ()
  "Insert a count of the number of times the file has been saved."
  (insert header-prefix-string "    Update #: 0\n"))

(defsubst header-url ()
  "Insert \"URL: \" line."
  (insert header-prefix-string "URL: \n"))

(defsubst header-doc-url ()
  "Insert \"Doc URL: \" line."
  (insert header-prefix-string "Doc URL: \n"))

(defsubst header-keywords ()
  "Insert \"Keywords: \" line."
  (insert header-prefix-string "Keywords: \n"))

(defsubst header-compatibility ()
  "Insert a \"Compatibility: \" line."
  (insert header-prefix-string "Compatibility: \n"))

(defsubst header-lib-requires ()
  "Insert list of libraries required by this one."
  (when (and (eq major-mode 'emacs-lisp-mode) (boundp 'libreq-file-header))
    (insert libreq-file-header)         ; Defined in `lib-requires.el'.
    (insert ";;   None\n;;\n")))

(defsubst header-status ()
  "Insert a \"Status: \" line."
  (insert header-prefix-string "Status: \n"))

(defsubst header-toc ()
  "Insert a \"Table of Contents: \" line."
  (insert header-prefix-string  "Table of Contents: \n" header-prefix-string
          "\n"))

(defsubst header-rcs-log ()
  "Insert lines to record RCS log information (\"$Log$\n\")."
  (insert header-prefix-string
          (concat "RCS $"               ; String split prevents `vc.el' overwrite.
                  "Log$\n")))           ; Thanks to Steve Taylor.

(defsubst header-AFS ()
  "Insert a line to record SHAPE information."
  (insert header-prefix-string "AFSID: $__Header$\n"))

(defsubst header-shell ()
  "Insert a kernal shell specifier line.
Uses the same shell named in `explicit-shell-file-name', the ESHELL
environment variable, the SHELL environment variable, or
'/bin/sh'.  (This is the same shell that the shell command uses.)"
  (insert "#!" (or (and (boundp 'explicit-shell-file-name)
                        explicit-shell-file-name)
                   (getenv "ESHELL")
                   (getenv "SHELL")
                   "/bin/sh")
          "\n"))


(defsubst header-python-script ()
  (insert "#!/usr/bin/env python\n"))

(defun header-mode-line ()
  "Insert a \" -*- Mode: \" line."
  (let* ((mode-declaration  (concat " -*- Mode: " (true-mode-name)
                                    (if (assoc 'c-style (buffer-local-variables))
                                        (concat "; C-Style: " (symbol-name c-style))
                                      "")
                                    " -*- "))
         (md-length         (length mode-declaration)))
    (insert (cond ((and comment-start (= 1 (length comment-start)))
                   ;; Assume comment start char is also fill char.
                   (concat comment-start comment-start
                           (make-string (/ (- 77 md-length) 2)
                                        (aref comment-start 0))
                           mode-declaration
                           (make-string (/ (- 78 md-length) 2)
                                        (aref comment-start 0))))
                  ((nonempty-comment-start) ; Assume spaces fill the gaps.
                   (concat comment-start
                           (make-string (/ (- 79 md-length
                                              (length comment-start)) 2)
                                        ?\ )
                           mode-declaration))
                  (t                    ; No comment-start.  Assume Lisp.
                   (concat ";;" (make-string (/ (- 77 md-length) 2) ?\;)
                           mode-declaration
                           (make-string (/ (- 78 md-length) 2) ?\;))))
            "\n")))

(defsubst header-end-line ()
  "Insert a divider line."
  (insert (cond ((nonempty-comment-end))
                ((and comment-start (= 1 (length comment-start)))
                 (make-string 70 (aref comment-start 0)))
                ((nonempty-comment-start))
                (t (make-string 70 ?\;)))
          "\n"))



;; Register the automatic actions to take for file headers during a save
;; See the second part of the file for explanations.
;; ---------------------------------------------------------------------
;; (register-file-header-action "^.* *\\(.*\\) *\\-\\-" 'update-file-name)
;; (register-file-header-action "\$VERSION[ \t]*=[ \t]*\"\\([0-9]+\\.\\)+"
;;                              'update-write-count)

(register-file-header-action "Last-Updated[ \t]*: " 'update-last-modified-date)
(register-file-header-action "          By[ \t]*: " 'update-last-modifier)
(register-file-header-action "    Update #[ \t]*: " 'update-write-count)
(when (boundp 'libreq-file-header)
  (register-file-header-action libreq-file-header 'update-lib-requires))


;; Header and file division header creation code
;; ---------------------------------------------
(defun true-mode-name ()
  "Return name of mode in a form such that mode may be re-established
by calling the function named by appending \"-name\" to this string.
This differs from variable `mode-name' in that this is guaranteed to
work even when the value has embedded spaces or other junk."
  (let ((major-mode-name  (symbol-name major-mode)))
    (capitalize (substring major-mode-name 0
                           (or   (string-match "-mode" major-mode-name)
                                 (length major-mode-name))))))

(defun header-prefix-string ()
  "Return a mode-specific prefix string for use in headers.
Is sensitive to language-dependent comment conventions."
  (cond
    ;; E.g. Lisp.
    ((and comment-start (= 1 (length comment-start)))
     (concat comment-start comment-start " "))

    ;; E.g. C++ and ADA.
    ;; Special case, three letter comment-start where the first and
    ;; second letters are the same.
    ((and comment-start (= 3 (length comment-start))
          (equal (aref comment-start 1) (aref comment-start 0)))
     comment-start)

    ;; E.g. C.
    ;; Other three-letter comment-start -> grab the middle character
    ((and comment-start (= 3 (length comment-start)))
     (concat " " (list (aref comment-start 1)) " "))

    ((and comment-start  (not (nonempty-comment-end)))

     ;; Note: no comment end implies that the full comment-start must be
     ;; used on each line.
     comment-start)
    (t ";; ")))       ; Use Lisp as default.


(defsubst update-last-modifier ()
  "Update the line that indicates who last modified the file."
  (delete-and-forget-line)
  (insert (format "%s" (let ((ufn  (user-full-name)))
                         (if (and ufn (not (string= "" ufn))) ufn (user-login-name))))))

(defsubst update-last-modified-date ()
  "Update the line that indicates the last-modified date."
  (delete-and-forget-line)
  (insert (header-date-string)))

(defun update-file-name ()
  "Update the line that indicates the file name."
  (beginning-of-line)
  ;; Verify looking at a file name for this mode.
  (when (looking-at (concat (regexp-quote (header-prefix-string)) " *\\(.*\\) *\\-\\-"))
    (goto-char (match-beginning 1))
    (delete-region (match-beginning 1) (match-end 1))
    (insert (file-name-nondirectory (buffer-file-name)) " ---")))

(defun update-lib-requires ()
  "Update the lines that show what libraries are required by this one.
This uses function `libreq-insert-lib-requires-as-comment' from
library `lib-requires.el'.

Note: If a byte-compiled file (`*.elc') for the library is available,
it is used when determining library dependencies, in preference to the
source library - this is the standard behavior of `load-library'.  The
list of required libraries reflects the dependencies indicated in the
byte-compiled file, not the source file.  If the byte-compiled file is
out-of-date with respect to its required libraries, so will be the
result of `update-lib-requires'."
  (when (buffer-file-name)              ; Do nothing if not a file buffer.
    (let ((lib  (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
      (when (and (eq major-mode 'emacs-lisp-mode)
                 (fboundp 'libreq-insert-lib-requires-as-comment))
        (goto-char (match-beginning 0))
        ;; Verify looking at `libreq-file-header'"
        (when (looking-at (regexp-quote libreq-file-header))
          (delete-and-forget-line) (delete-char 1)
          (delete-and-forget-line) (delete-char 1)
          (while (not (looking-at "^;;$")) (delete-and-forget-line) (delete-char 1))
          (delete-and-forget-line) (delete-char 1)
          (condition-case err
              ;; (let ((load-path (cons (file-name-directory (buffer-file-name))
              ;;                        load-path)))
              (libreq-insert-lib-requires-as-comment lib) ; Tries to load LIB.
            ;;   )
            ;; Typically, user just now added `provide' and must load again.
            (error (insert libreq-file-header (header-prefix-string) "  "
                           (error-message-string err) ".\n;;\n"))))))))


(provide 'my-python-header)

;;; my-python-header.el ends here
