(setq user-full-name "Sacha Chua"
      user-mail-address "sacha@sachachua.com")

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (package-refresh-contents))

(add-to-list 'load-path "~/elisp")
(add-to-list 'load-path "~/elisp/artbollocks-mode")
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(use-package auto-compile
  :ensure t
  :init (auto-compile-on-load-mode))

(load "~/.emacs.secrets" t)

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

(when window-system
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1))

(use-package winner
  :ensure winner
  :init (winner-mode 1))

(setq sentence-end-double-space nil)

(use-package helm
  :ensure helm
  :diminish helm-mode
  :init
  (progn 
    (require 'helm-config) 
    (setq helm-candidate-number-limit 100)
    ;; From https://gist.github.com/antifuchs/9238468
    (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
          helm-input-idle-delay 0.01  ; this actually updates things
                                        ; reeeelatively quickly.
          helm-quick-update t
          helm-M-x-requires-pattern nil
          helm-ff-skip-boring-files t)
    (helm-mode))
  :config
  (progn
    ;; I don't like the way switch-to-buffer uses history, since
    ;; that confuses me when it comes to buffers I've already
    ;; killed. Let's use ido instead.
    (add-to-list 'helm-completing-read-handlers-alist 
                 '(switch-to-buffer . ido))
    (add-to-list 'helm-completing-read-handlers-alist 
                 '(rename-file . ido))
    (add-to-list 'helm-completing-read-handlers-alist 
                 '(dired-rename-file . ido))
    ;; Unicode
    (add-to-list 'helm-completing-read-handlers-alist 
                 '(insert-char . ido)))
  :bind (("C-c h" . helm-mini) 
         ("C-h a" . helm-apropos)
         ("M-y" . helm-show-kill-ring)
         ("M-x" . helm-M-x)
         ("C-x c o" . helm-occur)
         ("C-x c s" . helm-swoop)
         ("C-x c b" . sacha/helm-do-grep-book-notes)
         ("C-x c SPC" . helm-all-mark-rings)))
(ido-mode -1) ;; Turn off ido mode in case I enabled it accidentally

(use-package helm-descbinds
  :bind (("C-h b" . helm-descbinds)
         ("C-h w" . helm-descbinds)))

(defvar sacha/book-notes-directory "~/Dropbox/books")
(defun sacha/helm-do-grep-book-notes ()
  "Search my book notes."
  (interactive)
  (helm-do-grep-1 (list sacha/book-notes-directory)))

(use-package smart-mode-line
  :init
  (progn
  (setq-default
   mode-line-format 
   '("%e"
     mode-line-front-space
     mode-line-mule-info
     mode-line-client
     mode-line-modified
     mode-line-remote
     mode-line-frame-identification
     mode-line-buffer-identification
     "   "
     mode-line-position
     (vc-mode vc-mode)
     "  "
     mode-line-modes
     mode-line-misc-info
     mode-line-end-spaces))))

(fset 'yes-or-no-p 'y-or-n-p)

(use-package miniedit
  :ensure miniedit
  :commands minibuffer-edit
  :init (miniedit-install))

(defadvice color-theme-alist (around sacha activate)
  (if (ad-get-arg 0)
      ad-do-it
    nil))
(use-package color-theme :ensure t)
(use-package color-theme-solarized :ensure t)
(defun sacha/setup-color-theme ()
  (interactive)
  (color-theme-solarized-dark)
  (set-face-foreground 'secondary-selection "darkblue")
  (set-face-background 'secondary-selection "lightblue")
  (set-face-background 'font-lock-doc-face "black")
  (set-face-foreground 'font-lock-doc-face "wheat")
  (set-face-background 'font-lock-string-face "black")
  (set-face-foreground 'org-todo "green")
  (set-face-background 'org-todo "black"))
 
(use-package color-theme
  :init
  (when window-system
    (sacha/setup-color-theme)))

(when window-system
  (custom-set-faces
   '(erc-input-face ((t (:foreground "antique white"))))
   '(helm-selection ((t (:background "ForestGreen" :foreground "black"))))
   '(org-agenda-clocking ((t (:inherit secondary-selection :foreground "black"))) t)
   '(org-agenda-done ((t (:foreground "dim gray" :strike-through nil))))
   '(org-done ((t (:foreground "PaleGreen" :weight normal :strike-through t))))
   '(org-clock-overlay ((t (:background "SkyBlue4" :foreground "black"))))
   '(org-headline-done ((((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon" :strike-through t))))
   '(outline-1 ((t (:inherit font-lock-function-name-face :foreground "cornflower blue"))))))

(use-package undo-tree
  :ensure undo-tree
  :diminish undo-tree-mode
  :init
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

(use-package guide-key
  :diminish guide-key-mode
  :init
  (progn
  (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-c"))
  (guide-key-mode 1)))  ; Enable guide-key-mode

(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (list (line-beginning-position)
        (line-beginning-position 2)))))

(bind-key "C-x p" 'pop-to-mark-command)
(setq set-mark-command-repeat-pop t)

(bind-key "C-+" 'text-scale-increase)
(bind-key "C--" 'text-scale-decrease)

(use-package helm-swoop
 :bind
 (("C-S-s" . helm-swoop)
  ("M-i" . helm-swoop)
  ("M-s s" . helm-swoop)
  ("M-s M-s" . helm-swoop)
  ("M-I" . helm-swoop-back-to-last-point)
  ("C-c M-i" . helm-multi-swoop)
  ("C-x M-i" . helm-multi-swoop-all)
  )
 :config
 (progn
   (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
   (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop))
)

(use-package windmove
  :bind
  (("<f2> <right>" . windmove-right)
   ("<f2> <left>" . windmove-left)
   ("<f2> <up>" . windmove-up)
   ("<f2> <down>" . windmove-down)
   ))

(defun sacha/vsplit-last-buffer (prefix)
  "Split the window vertically and display the previous buffer."
  (interactive "p")
  (split-window-vertically)
  (other-window 1 nil)
  (if (= prefix 1)
    (switch-to-next-buffer)))
(defun sacha/hsplit-last-buffer (prefix)
  "Split the window horizontally and display the previous buffer."
  (interactive "p")
  (split-window-horizontally)
  (other-window 1 nil)
  (if (= prefix 1) (switch-to-next-buffer)))
(bind-key "C-x 2" 'sacha/vsplit-last-buffer)
(bind-key "C-x 3" 'sacha/hsplit-last-buffer)

(defun sacha/search-word-backward ()
  "Find the previous occurrence of the current word."
  (interactive)
  (let ((cur (point)))
    (skip-syntax-backward "w_")
    (goto-char
     (if (re-search-backward (concat "\\_<" (current-word) "\\_>") nil t)
         (match-beginning 0)
       cur))))

(defun sacha/search-word-forward ()
  "Find the next occurrence of the current word."
  (interactive)
  (let ((cur (point)))
    (skip-syntax-forward "w_")
    (goto-char
     (if (re-search-forward (concat "\\_<" (current-word) "\\_>") nil t)
         (match-beginning 0)
       cur))))
(defadvice search-for-keyword (around sacha activate)
  "Match in a case-insensitive way."
  (let ((case-fold-search t))
    ad-do-it))
(global-set-key '[M-up] 'sacha/search-word-backward)
(global-set-key '[M-down] 'sacha/search-word-forward)

(mapcar
 (lambda (r)
   (set-register (car r) (cons 'file (cdr r))))
 '((?i . "~/.emacs.d/Sacha.org")
   (?o . "~/personal/organizer.org")
   (?b . "~/personal/business.org")
   (?B . "~/Dropbox/books")
   (?e . "~/code/dev/emacs-notes/tasks.org")
   (?w . "~/Dropbox/public/sharing/index.org")
   (?W . "~/Dropbox/public/sharing/blog.org")
   (?j . "~/personal/journal.org")
   (?I . "~/Dropbox/Inbox")
   (?g . "~/sachac.github.io/evil-plans/index.org")
   (?c . "~/code/dev/elisp-course.org")
   (?l . "~/dropbox/public/sharing/learning.org")))

(defun sacha/key-chord-define (keymap keys command)
  "Define in KEYMAP, a key-chord of two keys in KEYS starting a COMMAND.
\nKEYS can be a string or a vector of two elements. Currently only elements
that corresponds to ascii codes in the range 32 to 126 can be used.
\nCOMMAND can be an interactive function, a string, or nil.
If COMMAND is nil, the key-chord is removed.

MODIFICATION: Do not define the transposed key chord.
"
  (if (/= 2 (length keys))
      (error "Key-chord keys must have two elements"))
  ;; Exotic chars in a string are >255 but define-key wants 128..255 for those
  (let ((key1 (logand 255 (aref keys 0)))
        (key2 (logand 255 (aref keys 1))))
    (define-key keymap (vector 'key-chord key1 key2) command)))
(fset 'key-chord-define 'sacha/key-chord-define)

(defun sacha/switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun sacha/org-check-agenda ()
  "Peek at agenda."
  (interactive)
  (cond
   ((derived-mode-p 'org-agenda-mode)
    (if (window-parent) (delete-window) (bury-buffer)))
   ((get-buffer "*Org Agenda*")
    (switch-to-buffer-other-window "*Org Agenda*"))
   (t (org-agenda nil "a"))))

(defvar sacha/key-chord-command-map (make-sparse-keymap))
(bind-key "h" 'emms-pause sacha/key-chord-command-map)
(bind-key "u" 'emms-pause sacha/key-chord-command-map)
(bind-key "t" 'emms-seek-backward sacha/key-chord-command-map)
(bind-key "s" 'emms-seek-to sacha/key-chord-command-map)
(bind-key "f" 'windmove-right sacha/key-chord-command-map)
(bind-key "b" 'windmove-left sacha/key-chord-command-map)
(bind-key "n" 'windmove-down sacha/key-chord-command-map)
(bind-key "p" 'windmove-up sacha/key-chord-command-map)

(defvar sacha/windmove-map (make-sparse-keymap))
(bind-key "h" 'windmove-left sacha/windmove-map)
(bind-key "t" 'windmove-up sacha/windmove-map)
(bind-key "n" 'windmove-down sacha/windmove-map)
(bind-key "s" 'windmove-right sacha/windmove-map)
(bind-key "<left>" 'windmove-left sacha/windmove-map)
(bind-key "<up>" 'windmove-up sacha/windmove-map)
(bind-key "<down>" 'windmove-down sacha/windmove-map)
(bind-key "<right>" 'windmove-right sacha/windmove-map)

(use-package key-chord
  :init
  (progn 
    (fset 'key-chord-define 'sacha/key-chord-define)
    (setq key-chord-one-key-delay 0.16)
    (key-chord-mode 1)
    ;; k can be bound too
    (key-chord-define-global "uu"     'undo)
    (key-chord-define-global "jj"     'ace-jump-word-mode)
    (key-chord-define-global "yy"     sacha/windmove-map)
    ;; y is now free
    (key-chord-define-global "jw"     'ace-window)
    (key-chord-define-global "jl"     'ace-jump-line-mode)
    ;(key-chord-define-global "jz"     'ace-jump-zap-up-to-char)
    ;(key-chord-define-global "jZ"     'ace-jump-zap-to-char)
    (key-chord-define-global "FF"     'find-file)
    (key-chord-define-global "qq"     'sacha/org-quick-clock-in-task)
    (key-chord-define-global "hh"     sacha/key-chord-command-map)
    (key-chord-define-global "hc"     'emms-seek-forward)
    (key-chord-define-global "xx"     'er/expand-region)
    (key-chord-define-global "  "     'sacha/insert-space-or-expand)
    (key-chord-define-global "JJ"     'sacha/switch-to-previous-buffer)))

(use-package smartscan
  :init (global-smartscan-mode t))

(require 'find-dired)
(setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))

(defun sacha/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'sacha/smarter-move-beginning-of-line)

(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode)

(defun prelude-copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(add-to-list 'load-path "~/elisp/recursive-narrow")
(defun sacha/recursive-narrow-dwim-org ()
    (if (derived-mode-p 'org-mode) 
         (cond ((or (org-at-block-p) (org-in-src-block-p)) (org-narrow-to-block))
               (t (org-narrow-to-subtree))))
)
(use-package recursive-narrow
  :config
  (add-hook 'recursive-narrow-dwim-functions 'sacha/recursive-narrow-dwim-org)
  :bind
  (("C-x n w" . recursive-widen)
   ("C-x n n" . recursive-narrow-or-widen-dwim)))

(defun xah-toggle-margin-right ()
  "Toggle the right margin between `fill-column' or window width.
This command is convenient when reading novel, documentation."
  (interactive)
  (if (eq (cdr (window-margins)) nil)
      (set-window-margins nil 0 (- (window-body-width) fill-column))
    (set-window-margins nil 0 0)))

(use-package artbollocks-mode
  :init
  (progn
    (setq artbollocks-weasel-words-regex
          (concat "\\b" (regexp-opt
                         '("one of the"
                           "should"
                           "just"
                           "sort of"
                           "a lot"
                           "probably"
                           "maybe"
                           "perhaps"
                           "I think"
                           "really"
                           "pretty"
                           "nice"
                           "action"
                           "utilize"
                           "leverage") t) "\\b"))
    ;; Don't show the art critic words, or at least until I figure
    ;; out my own jargon
    (setq artbollocks-jargon nil)))

(defun sacha/unfill-paragraph (&optional region)
    "Takes a multi-line paragraph and makes it into a single line of text."
    (interactive (progn
                   (barf-if-buffer-read-only)
                   (list t)))
    (let ((fill-column (point-max)))
      (fill-paragraph nil region)))
(bind-key "M-Q" 'sacha/unfill-paragraph)

(defun sacha/fill-or-unfill-paragraph (&optional unfill region)
    "Fill paragraph (or REGION).
  With the prefix argument UNFILL, unfill it instead."
    (interactive (progn
                   (barf-if-buffer-read-only)
                   (list (if current-prefix-arg 'unfill) t)))
    (let ((fill-column (if unfill (point-max) fill-column)))
      (fill-paragraph nil region)))
(bind-key "M-q" 'sacha/fill-or-unfill-paragraph)

(remove-hook 'text-mode-hook #'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

(defmacro sacha/insert-unicode (unicode-name)
  `(lambda () (interactive)
     (insert-char (cdr (assoc-string ,unicode-name (ucs-names))))))
(bind-key "C-x 8 s" (sacha/insert-unicode "ZERO WIDTH SPACE"))
(bind-key "C-x 8 S" (sacha/insert-unicode "SNOWMAN"))

(bind-key "M-SPC" 'cycle-spacing)

(bind-key "M-/" 'hippie-expand)

(use-package org
  :diminish org-mode
  )

(setq org-modules '(org-bbdb 
                      org-gnus
                      org-drill
                      org-info
                      org-jsinfo
                      org-habit
                      org-irc
                      org-mouse
                      org-annotate-file
                      org-eval
                      org-expiry
                      org-interactive-query
                      org-man
                      org-panel
                      org-screen
                      org-toc))
(eval-after-load 'org
 '(org-load-modules-maybe t))
(setq org-expiry-inactive-timestamps t)

(bind-key "C-c r" 'org-capture)
(bind-key "C-c a" 'org-agenda)
(bind-key "C-c l" 'org-store-link)
(bind-key "C-c L" 'org-insert-link-global)
(bind-key "C-c O" 'org-open-at-point-global)
(bind-key "<f9> <f9>" 'org-agenda-list)
(bind-key "<f9> <f8>" (lambda () (interactive) (org-capture nil "r")))
(bind-key "C-TAB" 'org-cycle org-mode-map)
(bind-key "C-c v" 'org-show-todo-tree org-mode-map)
(bind-key "C-c C-r" 'org-refile org-mode-map)
(bind-key "C-c R" 'org-reveal org-mode-map)

(eval-after-load 'org
  '(bind-key "C-M-w" 'append-next-kill org-mode-map))

(setq org-goto-interface 'outline
      org-goto-max-level 10)
(require 'imenu)
(setq org-startup-folded nil)
(bind-key "M-o" 'imenu)
(bind-key "C-c j" 'org-clock-goto) ;; jump to current task from anywhere
(bind-key "C-c C-w" 'org-refile)
(setq org-cycle-include-plain-lists 'integrate)

(defun sacha/org-follow-entry-link ()
  "Follow the defined link for this entry."
  (interactive)
  (if (org-entry-get (point) "LINK")
      (org-open-link-from-string (org-entry-get (point) "LINK"))
    (org-open-at-point)))

(bind-key "C-c o" 'sacha/org-follow-entry-link org-mode-map)

(defun sacha/org-link-projects (location)
  "Add link properties between the current subtree and the one specified by LOCATION."
  (interactive
   (list (let ((org-refile-use-cache nil))
     (org-refile-get-location "Location"))))
  (let ((link1 (org-store-link nil)) link2)
    (save-window-excursion
      (org-refile 4 nil location)
      (setq link2 (org-store-link nil))
      (org-set-property "LINK" link1))
    (org-set-property "LINK" link2)))

(setq org-directory "~/personal")
(setq org-default-notes-file "~/personal/organizer.org")

(defun sacha/yank-more ()
  (interactive)
  (insert "[[")
  (yank)
  (insert "][more]]"))
(global-set-key (kbd "<f6>") 'sacha/yank-more)

(defun sacha/org-insert-heading-for-next-day ()
  "Insert a same-level heading for the following day."
  (interactive)
  (let ((new-date
         (seconds-to-time
          (+ 86400.0
             (float-time
              (org-read-date nil 'to-time (elt (org-heading-components) 4)))))))
    (org-insert-heading-after-current)
    (insert (format-time-string "%Y-%m-%d\n\n" new-date))))

(defvar sacha/org-basic-task-template "* TODO %^{Task}    
SCHEDULED: %^t
:PROPERTIES:
:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}
:END:
%?
" "Basic task data")
(setq org-capture-templates
      `(("t" "Tasks" entry 
         (file+headline "~/personal/organizer.org" "Tasks")
         ,sacha/org-basic-task-template)
        ("T" "Quick task" entry 
         (file+headline "~/personal/organizer.org" "Tasks")
         "* TODO %^{Task}"
         :immediate-finish t)
        ("e" "Emacs idea" entry
         (file+headline "~/code/dev/emacs-notes/tasks.org" "Emacs")
         "* TODO %^{Task}"
         :immediate-finish t)
        ("b" "Business task" entry
         (file+headline "~/personal/business.org" "Tasks")
         ,sacha/org-basic-task-template)
        ("p" "People task" entry
         (file+headline "~/personal/people.org" "Tasks")
         ,sacha/org-basic-task-template)
        ("j" "Journal entry" plain
         (file+datetree "~/personal/journal.org")
         "%K - %a\n%i\n%?\n"
         :unnarrowed t)
        ("J" "Journal entry with date" plain
         (file+datetree+prompt "~/personal/journal.org")
         "%K - %a\n%i\n%?\n"
         :unnarrowed t)
        ("db" "Done - Business" entry
         (file+headline "~/personal/business.org" "Tasks")
         "* DONE %^{Task}\nSCHEDULED: %^t\n%?")
        ("dp" "Done - People" entry
         (file+headline "~/personal/people.org" "Tasks")
         "* DONE %^{Task}\nSCHEDULED: %^t\n%?")
        ("dt" "Done - Task" entry
         (file+headline "~/personal/organizer.org" "Tasks")
         "* DONE %^{Task}\nSCHEDULED: %^t\n%?")
        ("q" "Quick note" item
         (file+headline "~/personal/organizer.org" "Quick notes"))
        ("l" "Ledger entries")
        ("lm" "MBNA" plain
         (file "~/personal/ledger")
         "%(org-read-date) %^{Payee}
  Liabilities:MBNA  
  Expenses:%^{Account}  $%^{Amount}
" :immediate-finish t)
        ("ln" "No Frills" plain
         (file "~/personal/ledger")
         "%(let ((org-read-date-prefer-future nil)) (org-read-date)) * No Frills
  Liabilities:MBNA  
  Assets:Wayne:Groceries  $%^{Amount}
" :immediate-finish t)    
        ("lc" "Cash" plain
         (file "~/personal/ledger")
         "%(org-read-date) * %^{Payee}
  Expenses:Cash 
  Expenses:%^{Account}  %^{Amount}
")             
        ("b" "Book" entry
         (file+datetree "~/personal/books.org" "Inbox")
         "* %^{Title}  %^g
%i
*Author(s):* %^{Author} \\\\
*ISBN:* %^{ISBN}

%?

*Review on:* %^t \\
%a
%U"
         :clock-in :clock-resume)
         ("c" "Contact" entry (file "~/personal/contacts.org")
          "* %(org-contacts-template-name)
:PROPERTIES:
:EMAIL: %(org-contacts-template-email)
:END:")
         ("n" "Daily note" table-line (file+olp "~/personal/organizer.org" "Daily notes")
          "| %u | %^{Note} |"
          :immediate-finish t)
         ("r" "Notes" entry
          (file+datetree "~/personal/organizer.org")
          "* %?\n\n%i\n"
          )))
(bind-key "C-M-r" 'org-capture)

(defun sacha/org-capture-refile-and-jump ()
  (interactive)
  (org-capture-refile)
  (org-refile-goto-last-stored))
(require 'org-capture)
(bind-key "C-c C-r" 'sacha/org-capture-refile-and-jump org-capture-mode-map)

(setq org-reverse-note-order t)
(setq org-refile-use-outline-path nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-refile-use-cache nil)
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
(setq org-blank-before-new-entry nil)

(require 'org-clock)
(defun sacha/org-entry-wpm ()
  (interactive)
  (save-restriction
    (save-excursion
      (org-narrow-to-subtree)
      (goto-char (point-min))
      (let* ((words (count-words-region (point-min) (point-max)))
       (minutes (org-clock-sum-current-item))
       (wpm (/ words minutes)))
  (message "WPM: %d (words: %d, minutes: %d)" wpm words minutes)
  (kill-new (number-to-string wpm))))))

(setq org-todo-keywords
 '((sequence
    "TODO(t)"  ; next action
    "TOBLOG(b)"  ; next action
    "STARTED(s)"
    "WAITING(w@/!)"
    "SOMEDAY(.)" "|" "DONE(x!)" "CANCELLED(c@)")
   (sequence "LEARN" "DO" "TEACH")
   (sequence "TOSKETCH" "SKETCHED" "|" "POSTED")
   (sequence "TODELEGATE(-)" "DELEGATED(d)" "|" "COMPLETE(x)")))

(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "green" :weight bold))
        ("DONE" . (:foreground "cyan" :weight bold))
        ("WAITING" . (:foreground "red" :weight bold))
        ("SOMEDAY" . (:foreground "gray" :weight bold))))

(setq org-tags-exclude-from-inheritance '("project"))

(add-to-list 'org-speed-commands-user '("N" org-narrow-to-subtree))
(add-to-list 'org-speed-commands-user '("W" widen))
(defun sacha/org-agenda-for-subtree ()
  (interactive)
  (if (derived-mode-p 'org-agenda-mode)
    (let* ((marker (or (org-get-at-bol 'org-marker)
                       (org-agenda-error)))
           (hdmarker (or (org-get-at-bol 'org-hd-marker) marker))
           (pos (marker-position marker))
           (col (current-column))
           newhead)
      (org-with-remote-undo (marker-buffer marker)
        (with-current-buffer (marker-buffer marker)
          (widen)
          (let ((org-agenda-view-columns-initially t))
            (org-agenda nil "t" 'subtree)))))
    (let ((org-agenda-view-columns-initially t))
      (org-agenda nil "t" 'subtree))))
(add-to-list 'org-speed-commands-user '("T" sacha/org-agenda-for-subtree))

(setq org-tag-alist '(("@work" . ?b) 
                      ("@home" . ?h) 
                      ("@writing" . ?w)
                      ("@errands" . ?e) 
                      ("@drawing" . ?d)
                      ("@coding" . ?c)
                      ("@phone" . ?p)
                      ("@reading" . ?r)
                      ("@computer" . ?l)
                      ("quantified" . ?q)
                      ("lowenergy" . ?0)
                      ("highenergy" . ?1)))

(add-to-list 'org-global-properties
      '("Effort_ALL". "0:05 0:15 0:30 1:00 2:00 3:00 4:00"))

(defun sacha/org-clock-in-set-state-to-started ()
  "Mark STARTED when clocked in."
  (save-excursion
    (catch 'exit
      (cond
       ((derived-mode-p 'org-agenda-mode)
        (let* ((marker (or (org-get-at-bol 'org-marker)
                           (org-agenda-error)))
               (hdmarker (or (org-get-at-bol 'org-hd-marker) marker))
               (pos (marker-position marker))
               (col (current-column))
               newhead)
          (org-with-remote-undo (marker-buffer marker)
            (with-current-buffer (marker-buffer marker)
              (widen)
              (goto-char pos)
              (org-back-to-heading t)
              (if (org-get-todo-state)
                  (org-todo "STARTED"))))))
       (t (if (org-get-todo-state)
                  (org-todo "STARTED")))))))
(use-package org
 :init
 (progn
  (setq org-clock-idle-time nil)
  (setq org-log-done 'time)
  (setq org-clock-persist t)
  (setq org-clock-report-include-clocking-task t))
 :config 
 (progn 
  (org-clock-persistence-insinuate)
  (add-hook 'org-clock-in-hook 'sacha/org-clock-in-set-state-to-started)))

(setq org-log-into-drawer "LOGBOOK")
(setq org-clock-into-drawer 1)

(setq org-habit-graph-column 80)
(setq org-habit-show-habits-only-for-today nil)

(add-hook 'org-clock-in-prepare-hook
          'sacha/org-mode-ask-effort)

(defun sacha/org-mode-ask-effort ()
  "Ask for an effort estimate when clocking in."
  (unless (org-entry-get (point) "Effort")
    (let ((effort
           (completing-read
            "Effort: "
            (org-entry-get-multivalued-property (point) "Effort"))))
      (unless (equal effort "")
        (org-set-property "Effort" effort)))))

(defvar sacha/org-agenda-limit-items nil "Number of items to show in agenda to-do views; nil if unlimited.")
(eval-after-load 'org
'(defadvice org-agenda-finalize-entries (around sacha activate)
  (if sacha/org-agenda-limit-items
      (progn
        (setq list (mapcar 'org-agenda-highlight-todo list))
        (setq ad-return-value
              (subseq list 0 sacha/org-agenda-limit-items))
        (when org-agenda-before-sorting-filter-function
          (setq list (delq nil (mapcar org-agenda-before-sorting-filter-function list))))
        (setq ad-return-value
              (mapconcat 'identity
                         (delq nil 
                               (subseq
                                (sort list 'org-entries-lessp)
                                0
                                sacha/org-agenda-limit-items))
                         "\n")))
    ad-do-it)))

;; Get this from https://raw.github.com/chenfengyuan/elisp/master/next-spec-day.el
(load "~/elisp/next-spec-day.el" t)

(setq org-agenda-files
      (delq nil
            (mapcar (lambda (x) (and (file-exists-p x) x))
                    '("~/personal/organizer.org"
                      "~/personal/people.org"
                      "~/personal/business.org"
                      "~/Dropbox/public/sharing/index.org"
                      "~/dropbox/public/sharing/learning.org"
                      "~/code/dev/emacs-notes/tasks.org"
                      "~/sachac.github.io/evil-plans/index.org"
                      "~/personal/cooking.org"
                      "~/personal/routines.org"))))

(setq org-agenda-span 2)
(setq org-agenda-sticky nil)
(setq org-agenda-inhibit-startup t)
(setq org-agenda-use-tag-inheritance nil)
(setq org-agenda-show-log t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
(setq org-agenda-time-grid
      '((daily today require-timed)
       "----------------"
       (800 1000 1200 1400 1600 1800)))
(setq org-columns-default-format "%50ITEM %12SCHEDULED %TODO %3PRIORITY %Effort{:} %TAGS")

(bind-key "Y" 'org-agenda-todo-yesterday org-agenda-mode-map)

(setq org-agenda-start-on-weekday 6)

(defun sacha/org-agenda-project-agenda ()
  "Return the project headline and up to `sacha/org-agenda-limit-items' tasks."
  (save-excursion
    (let* ((marker (org-agenda-new-marker))
           (heading
            (org-agenda-format-item "" (org-get-heading) (org-get-category) nil))
           (org-agenda-restrict t)
           (org-agenda-restrict-begin (point))
           (org-agenda-restrict-end (org-end-of-subtree 'invisible))
           ;; Find the TODO items in this subtree
           (list (org-agenda-get-day-entries (buffer-file-name) (calendar-current-date) :todo)))
      (org-add-props heading
          (list 'face 'defaults
                'done-face 'org-agenda-done
                'undone-face 'default
                'mouse-face 'highlight
                'org-not-done-regexp org-not-done-regexp
                'org-todo-regexp org-todo-regexp
                'org-complex-heading-regexp org-complex-heading-regexp
                'help-echo
                (format "mouse-2 or RET jump to org file %s"
                        (abbreviate-file-name
                         (or (buffer-file-name (buffer-base-buffer))
                             (buffer-name (buffer-base-buffer))))))
        'org-marker marker
        'org-hd-marker marker
        'org-category (org-get-category)
        'type "tagsmatch")
      (concat heading "\n"
              (org-agenda-finalize-entries list)))))
        
  (defun sacha/org-agenda-projects-and-tasks (match)
    "Show TODOs for all `org-agenda-files' headlines matching MATCH."
    (interactive "MString: ")
    (let ((todo-only nil))
      (if org-agenda-overriding-arguments
          (setq todo-only (car org-agenda-overriding-arguments)
                match (nth 1 org-agenda-overriding-arguments)))
      (let* ((org-tags-match-list-sublevels
              org-tags-match-list-sublevels)
             (completion-ignore-case t)
             rtn rtnall files file pos matcher
             buffer)
        (when (and (stringp match) (not (string-match "\\S-" match)))
          (setq match nil))
        (setq matcher (org-make-tags-matcher match)
              match (car matcher) matcher (cdr matcher))
        (catch 'exit
          (if org-agenda-sticky
              (setq org-agenda-buffer-name
                    (if (stringp match)
                        (format "*Org Agenda(%s:%s)*"
                                (or org-keys (or (and todo-only "M") "m")) match)
                      (format "*Org Agenda(%s)*" (or (and todo-only "M") "m")))))
          (org-agenda-prepare (concat "TAGS " match))
          (org-compile-prefix-format 'tags)
          (org-set-sorting-strategy 'tags)
          (setq org-agenda-query-string match)
          (setq org-agenda-redo-command
                (list 'org-tags-view `(quote ,todo-only)
                      (list 'if 'current-prefix-arg nil `(quote ,org-agenda-query-string))))
          (setq files (org-agenda-files nil 'ifmode)
                rtnall nil)
          (while (setq file (pop files))
            (catch 'nextfile
              (org-check-agenda-file file)
              (setq buffer (if (file-exists-p file)
                               (org-get-agenda-file-buffer file)
                             (error "No such file %s" file)))
              (if (not buffer)
                  ;; If file does not exist, error message to agenda
                  (setq rtn (list
                             (format "ORG-AGENDA-ERROR: No such org-file %s" file))
                        rtnall (append rtnall rtn))
                (with-current-buffer buffer
                  (unless (derived-mode-p 'org-mode)
                    (error "Agenda file %s is not in `org-mode'" file))
                  (save-excursion
                    (save-restriction
                      (if org-agenda-restrict
                          (narrow-to-region org-agenda-restrict-begin
                                            org-agenda-restrict-end)
                        (widen))
                      (setq rtn (org-scan-tags 'sacha/org-agenda-project-agenda matcher todo-only))
                      (setq rtnall (append rtnall rtn))))))))
          (if org-agenda-overriding-header
              (insert (org-add-props (copy-sequence org-agenda-overriding-header)
                          nil 'face 'org-agenda-structure) "\n")
            (insert "Headlines with TAGS match: ")
            (add-text-properties (point-min) (1- (point))
                                 (list 'face 'org-agenda-structure
                                       'short-heading
                                       (concat "Match: " match)))
            (setq pos (point))
            (insert match "\n")
            (add-text-properties pos (1- (point)) (list 'face 'org-warning))
            (setq pos (point))
            (unless org-agenda-multi
              (insert "Press `C-u r' to search again with new search string\n"))
            (add-text-properties pos (1- (point)) (list 'face 'org-agenda-structure)))
          (org-agenda-mark-header-line (point-min))
          (when rtnall
            (insert (mapconcat 'identity rtnall "\n") ""))
          (goto-char (point-min))
          (or org-agenda-multi (org-agenda-fit-window-to-buffer))
          (add-text-properties (point-min) (point-max)
                               `(org-agenda-type tags
                                                 org-last-args (,todo-only ,match)
                                                 org-redo-cmd ,org-agenda-redo-command
                                                 org-series-cmd ,org-cmd))
          (org-agenda-finalize)
          (setq buffer-read-only t)))))

(defvar sacha/org-agenda-contexts
    '((tags-todo "+@phone")
      (tags-todo "+@work")
      (tags-todo "+@drawing")
      (tags-todo "+@coding")
      (tags-todo "+@writing")
      (tags-todo "+@computer")
      (tags-todo "+@home")
      (tags-todo "+@errands"))
    "Usual list of contexts.")
  (defun sacha/org-agenda-skip-scheduled ()
    (org-agenda-skip-entry-if 'scheduled 'deadline 'regexp "\n]+>"))
  (setq org-agenda-custom-commands
        `(("T" tags-todo "TODO=\"TODO\"-goal-routine-SCHEDULED={.+}")
          ("b" todo ""
           ((org-agenda-files '("~/personal/business.org"))))
          ("B" todo ""
           ((org-agenda-files '("~/Dropbox/books"))))
          ("o" todo ""
           ((org-agenda-files '("~/personal/organizer.org"))))
          ("c" todo ""
           ((org-agenda-prefix-format "")
            (org-agenda-cmp-user-defined 'sacha/org-sort-agenda-items-todo)
            (org-agenda-view-columns-initially t)
            ))
          ;; Weekly review
          ("w" "Weekly review" agenda ""
           ((org-agenda-span 7)
            (org-agenda-log-mode 1)))
          ("W" "Weekly review sans routines" agenda "" 
           ((org-agenda-span 7) 
            (org-agenda-log-mode 1)
            (org-agenda-tag-filter-preset '("-routine"))))
          ("2" "Bi-weekly review" agenda "" ((org-agenda-span 14) (org-agenda-log-mode 1)))
          ("gb" "Business" todo ""  
           ((org-agenda-files '("~/personal/business.org"))
            (org-agenda-view-columns-initially t)))
          ("gc" "Coding" tags-todo "@coding" 
           ((org-agenda-view-columns-initially t)))
          ("gw" "Writing" tags-todo "@writing"
           ((org-agenda-view-columns-initially t)))
          ("gp" "Publishing" tags-todo "@publishing"
           ((org-agenda-view-columns-initially t)))
          ("gP" "Phone" tags-todo "@phone"
           ((org-agenda-view-columns-initially t)))
          ("gd" "Drawing" tags-todo "@drawing"
           ((org-agenda-view-columns-initially t)))
          ("gh" "Home" tags-todo "@home"
           ((org-agenda-view-columns-initially t)))
          ("ge" "Errands" tags-todo "@errands"
           ((org-agenda-view-columns-initially t)))
          ("0" "Top 3 by context"
           ,sacha/org-agenda-contexts
           ((org-agenda-sorting-strategy '(priority-up effort-down))
            (sacha/org-agenda-limit-items 3)))
          (")" "All by context"
           ,sacha/org-agenda-contexts
           ((org-agenda-sorting-strategy '(priority-down effort-down))
            (sacha/org-agenda-limit-items nil)))
          ("9" "Unscheduled top 3 by context"
           ,sacha/org-agenda-contexts
           ((org-agenda-skip-function 'sacha/org-agenda-skip-scheduled)
            (org-agenda-sorting-strategy '(priority-down effort-down))
            (sacha/org-agenda-limit-items 3)))
          ("(" "All unscheduled by context"
           ,sacha/org-agenda-contexts
           ((org-agenda-skip-function 'sacha/org-agenda-skip-scheduled)
            (org-agenda-sorting-strategy '(priority-down effort-down))
            ))
          ("d" "Timeline for today" ((agenda "" ))
           ((org-agenda-ndays 1)
            (org-agenda-show-log t)
            (org-agenda-log-mode-items '(clock closed))
            (org-agenda-clockreport-mode t)
            (org-agenda-entry-types '())))
          ("." "Waiting for" todo "WAITING")
          ("u" "Unscheduled tasks" tags-todo "-someday-TODO=\"SOMEDAY\"-TODO=\"DELEGATED\"-TODO=\"WAITING\"-project"
           ((org-agenda-skip-function 'sacha/org-agenda-skip-scheduled)
            (org-agenda-view-columns-initially t)
            (org-tags-exclude-from-inheritance '("project"))
            (org-agenda-overriding-header "Unscheduled TODO entries: ")
            (org-columns-default-format "%50ITEM %TODO %3PRIORITY %Effort{:} %TAGS")
            (org-agenda-sorting-strategy '(todo-state-up priority-down effort-up tag-up category-keep))))
          ("U" "Unscheduled tasks outside projects" tags-todo "-project"
           ((org-agenda-skip-function 'sacha/org-agenda-skip-scheduled)
            (org-tags-exclude-from-inheritance nil)
            (org-agenda-view-columns-initially t)
            (org-agenda-overriding-header "Unscheduled TODO entries outside projects: ")
            (org-agenda-sorting-strategy '(todo-state-up priority-down tag-up category-keep effort-down))))
          ("P" "By priority"
           ((tags-todo "+PRIORITY=\"A\"")
            (tags-todo "+PRIORITY=\"B\"")
            (tags-todo "+PRIORITY=\"\"")
            (tags-todo "+PRIORITY=\"C\""))
           ((org-agenda-prefix-format "%-10c %-10T %e ")
            (org-agenda-sorting-strategy '(priority-down tag-up category-keep effort-down))))
          ("pp" tags "+project-someday-TODO=\"DONE\""
           ((org-tags-exclude-from-inheritance '("project"))
            (org-agenda-sorting-strategy '(priority-down tag-up category-keep effort-down))))
          ("p." tags "+project-TODO=\"DONE\""
           ((org-tags-exclude-from-inheritance '("project"))
            (org-agenda-sorting-strategy '(priority-down tag-up category-keep effort-down))))
          ("S" tags-todo "TODO=\"STARTED\"")
          ("C" "Cooking"
           ((tags "vegetables")
            (tags "chicken")
            (tags "beef")
            (tags "pork")
            (tags "other"))
           ((org-agenda-files '("~/personal/cooking.org"))
            (org-agenda-view-columns-initially t)
            (org-agenda-sorting-strategy '(scheduled-up time-down todo-state-up)))
           )
          ("2" "List projects with tasks" sacha/org-agenda-projects-and-tasks
           "+PROJECT"
             ((sacha/org-agenda-limit-items 3)))))
(bind-key "<apps> a" 'org-agenda)

(defun sacha/org-agenda-done (&optional arg)
  "Mark current TODO as done.
This changes the line at point, all other lines in the agenda referring to
the same tree node, and the headline of the tree node in the Org-mode file."
  (interactive "P")
  (org-agenda-todo "DONE"))
;; Override the key definition for org-exit
(define-key org-agenda-mode-map "x" 'sacha/org-agenda-done)

(defun sacha/org-agenda-mark-done-and-add-followup ()
    "Mark the current TODO as done and add another task after it.
Creates it at the same level as the previous task, so it's better to use
this with to-do items than with projects or headings."
    (interactive)
    (org-agenda-todo "DONE")
    (org-agenda-switch-to)
    (org-capture 0 "t"))
;; Override the key definition
(define-key org-agenda-mode-map "X" 'sacha/org-agenda-mark-done-and-add-followup)

(defun sacha/org-agenda-new ()
  "Create a new note or task at the current agenda item.
Creates it at the same level as the previous task, so it's better to use
this with to-do items than with projects or headings."
  (interactive)
  (org-agenda-switch-to)
  (org-capture 0))
;; New key assignment
(define-key org-agenda-mode-map "N" 'sacha/org-agenda-new)

(setq org-agenda-sorting-strategy
      '((agenda time-up priority-down tag-up effort-up category-keep)
        (todo user-defined-up todo-state-up priority-down effort-up)
        (tags user-defined-up)
        (search category-keep)))
(setq org-agenda-cmp-user-defined 'sacha/org-sort-agenda-items-user-defined)    
(require 'cl)
(defun sacha/org-get-context (txt)
  "Find the context."
  (car (member-if
        (lambda (item) (string-match "@" item))
        (get-text-property 1 'tags txt))))

(defun sacha/org-compare-dates (a b)
  "Return 1 if A should go after B, -1 if B should go after A, or 0 if a = b."
  (cond
   ((and (= a 0) (= b 0)) nil)
   ((= a 0) 1)
   ((= b 0) -1)
   ((> a b) 1)
   ((< a b) -1)
   (t nil)))

(defun sacha/org-complete-cmp (a b)
  (let* ((state-a (or (get-text-property 1 'todo-state a) ""))
         (state-b (or (get-text-property 1 'todo-state b) "")))
    (or
     (if (member state-a org-done-keywords-for-agenda) 1)
     (if (member state-b org-done-keywords-for-agenda) -1))))

(defun sacha/org-date-cmp (a b)
  (let* ((sched-a (or (get-text-property 1 'org-scheduled a) 0))
         (sched-b (or (get-text-property 1 'org-scheduled b) 0))
         (deadline-a (or (get-text-property 1 'org-deadline a) 0))
         (deadline-b (or (get-text-property 1 'org-deadline b) 0)))
    (or
     (sacha/org-compare-dates
      (sacha/org-min-date sched-a deadline-a)
      (sacha/org-min-date sched-b deadline-b)))))

(defun sacha/org-min-date (a b)
  "Return the smaller of A or B, except for 0."
  (funcall (if (and (> a 0) (> b 0)) 'min 'max) a b))

(defun sacha/org-sort-agenda-items-user-defined (a b)
  ;; compare by deadline, then scheduled date; done tasks are listed at the very bottom
  (or
   (sacha/org-complete-cmp a b)
   (sacha/org-date-cmp a b)))

(defun sacha/org-context-cmp (a b)
  "Compare CONTEXT-A and CONTEXT-B."
  (let ((context-a (sacha/org-get-context a))
        (context-b (sacha/org-get-context b)))
    (cond
     ((null context-a) +1)
     ((null context-b) -1)
     ((string< context-a context-b) -1)
     ((string< context-b context-a) +1)
     (t nil))))

(defun sacha/org-sort-agenda-items-todo (a b)
  (or
   (org-cmp-time a b)
   (sacha/org-complete-cmp a b)
   (sacha/org-context-cmp a b)
   (sacha/org-date-cmp a b)
   (org-cmp-todo-state a b)
   (org-cmp-priority a b)
   (org-cmp-effort a b)))

(defun sacha/org-agenda-list-unscheduled (&rest ignore)
  "Create agenda view for tasks that are unscheduled and not done."
  (let* ((org-agenda-todo-ignore-with-date t)
   (org-agenda-overriding-header "List of unscheduled tasks: "))
    (org-agenda-get-todos)))
(setq org-stuck-projects
      '("+PROJECT-MAYBE-DONE"
        ("TODO")
        nil
        "\\<IGNORE\\>"))

(defvar sacha/weekly-review-line-regexp 
  "^  \\([^:]+\\): +\\(Sched[^:]+: +\\)?TODO \\(.*?\\)\\(?:[      ]+\\(:[[:alnum:]_@#%:]+:\\)\\)?[        ]*$"
  "Regular expression matching lines to include.")
(defvar sacha/weekly-done-line-regexp 
  "^  \\([^:]+\\): +.*?\\(?:Clocked\\|Closed\\):.*?\\(?:TODO\\|DONE\\) \\(.*?\\)\\(?:[       ]+\\(:[[:alnum:]_@#%:]+:\\)\\)?[        ]*$"
  "Regular expression matching lines to include as completed tasks.")

(defun sacha/quantified-get-hours (category time-summary)
  "Return the number of hours based on the time summary."
  (if (stringp category)
      (if (assoc category time-summary) (/ (cdr (assoc category time-summary)) 3600.0) 0)
    (apply '+ (mapcar (lambda (x) (sacha/quantified-get-hours x time-summary)) category))))

(defun sacha/org-summarize-focus-areas ()
  "Summarize previous and upcoming tasks as a list."
  (interactive)
  (let ((base-date (apply 'encode-time (org-read-date-analyze "-fri" nil '(0 0 0))))
        (line-re sacha/weekly-review-line-regexp)
        (done-re sacha/weekly-done-line-regexp)
        business relationships life business-next relationships-next life-next string 
        start end time-summary biz-time)
    (setq start (format-time-string "%Y-%m-%d" (days-to-time (- (time-to-number-of-days base-date) 6))))
    (setq end (format-time-string "%Y-%m-%d" (days-to-time (1+ (time-to-number-of-days base-date)))))
    (setq time-summary (quantified-summarize-time start end))
    (setq biz-time (sacha/quantified-get-hours "Business" time-summary))
    (save-window-excursion
      (org-agenda nil "W")
      (setq string (buffer-string))
      (with-temp-buffer
        (insert string)
        (goto-char (point-min))
        (while (re-search-forward line-re nil t)
          (cond
           ((string= (match-string 1) "routines") nil) ; skip routine tasks
           ((or (string= (match-string 1) "business") (string= (match-string 1) "tasks"))
            (add-to-list 'business-next (concat "  - [ ] " (match-string 3))))
           ((string= (match-string 1) "people")
            (add-to-list 'relationships-next (concat "  - [ ] " (match-string 3))))
           (t (add-to-list 'life-next (concat "  - [ ] " (match-string 3))))))))
    (save-window-excursion
      (org-agenda nil "W")
      (org-agenda-later -1)
      (org-agenda-log-mode 16)
      (setq string (buffer-string))
      ;; Get any completed tasks from the current week as well
      (org-agenda-later 1)
      (org-agenda-log-mode 16)
      (setq string (concat string "\n" (buffer-string)))
      (with-temp-buffer
        (insert string)
        (goto-char (point-min))
        (while (re-search-forward done-re nil t)
          (cond
           ((string= (match-string 1) "routines") nil) ; skip routine tasks
           ((or (string= (match-string 1) "business") (string= (match-string 1) "tasks"))
            (add-to-list 'business (concat "  - [X] " (match-string 2))))
           ((string= (match-string 1) "people")
            (add-to-list 'relationships (concat "  - [X] " (match-string 2))))
           (t (add-to-list 'life (concat "  - [X] " (match-string 2))))))))
    (setq string
          (concat
           (format "- *Business* (%.1fh - %d%%)\n" biz-time (/ biz-time 1.68))
           (mapconcat 'identity business "\n") "\n"
           (mapconcat 'identity business-next "\n")
           "\n"
           (format "  - *Earn* (%.1fh - %d%% of Business)\n"
                   (sacha/quantified-get-hours "Business - Earn" time-summary)
                   (/ (sacha/quantified-get-hours "Business - Earn" time-summary) (* 0.01 biz-time)))
           (format "  - *Build* (%.1fh - %d%% of Business)\n"
                   (sacha/quantified-get-hours "Business - Build" time-summary)
                   (/ (sacha/quantified-get-hours "Business - Build" time-summary) (* 0.01 biz-time)))
           (format "    - *Drawing* (%.1fh)\n"
                   (sacha/quantified-get-hours '("Business - Build - Drawing"
                                                 "Business - Build - Book review")  time-summary))
           (format "    - *Delegation* (%.1fh)\n"
                   (sacha/quantified-get-hours "Business - Build - Delegation" time-summary))
           (format "    - *Packaging* (%.1fh)\n"
                   (sacha/quantified-get-hours "Business - Build - Packaging" time-summary))
           (format "    - *Paperwork* (%.1fh)\n"
                   (sacha/quantified-get-hours "Business - Build - Paperwork"  time-summary))
           (format "  - *Connect* (%.1fh - %d%% of Business)\n"
                   (sacha/quantified-get-hours "Business - Connect" time-summary)
                   (/ (sacha/quantified-get-hours "Business - Connect" time-summary) (* 0.01 biz-time)))
           (format "- *Relationships* (%.1fh - %d%%)\n"
                   (sacha/quantified-get-hours '("Discretionary - Social"
                                                 "Discretionary - Family") time-summary)
                   (/ (sacha/quantified-get-hours '("Discretionary - Social"
                                                    "Discretionary - Family") time-summary) 1.68))
           (mapconcat 'identity (sort relationships 'string<) "\n") "\n"
           (mapconcat 'identity (sort relationships-next 'string<) "\n")
           "\n"
           (format "- *Discretionary - Productive* (%.1fh - %d%%)\n"
                   (sacha/quantified-get-hours "Discretionary - Productive" time-summary)
                   (/ (sacha/quantified-get-hours "Discretionary - Productive" time-summary) 1.68))
           (format "  - *Emacs* (%.1fh - %d%% of all)\n"
                   (sacha/quantified-get-hours "Discretionary - Productive - Emacs" time-summary)
                   (/ (sacha/quantified-get-hours "Discretionary - Productive - Emacs" time-summary) 1.68))
           (mapconcat 'identity (sort life 'string<) "\n") "\n"
           (mapconcat 'identity (sort life-next 'string<) "\n") "\n"
           (format "  - *Writing* (%.1fh)\n"
                   (sacha/quantified-get-hours "Discretionary - Productive - Writing" time-summary))
           (format "- *Discretionary - Play* (%.1fh - %d%%)\n"
                   (sacha/quantified-get-hours "Discretionary - Play" time-summary)
                   (/ (sacha/quantified-get-hours "Discretionary - Play" time-summary) 1.68))
                                        ;                 (format "- *Discretionary - Travel* (%.1fh - %d%%)\n"
                                        ;                         (sacha/quantified-get-hours "Discretionary - Travel" time-summary)
                                        ;                         (/ (sacha/quantified-get-hours "Discretionary - Travel" time-summary) 1.68))
           (format "- *Personal routines* (%.1fh - %d%%)\n"
                   (sacha/quantified-get-hours "Personal" time-summary)
                   (/ (sacha/quantified-get-hours "Personal" time-summary) 1.68))
           (format "- *Unpaid work* (%.1fh - %d%%)\n"
                   (sacha/quantified-get-hours "Unpaid work" time-summary)
                   (/ (sacha/quantified-get-hours "Unpaid work" time-summary) 1.68))
           (format "- *Sleep* (%.1fh - %d%% - average of %.1f per day)\n"
                   (sacha/quantified-get-hours "Sleep" time-summary)
                   (/ (sacha/quantified-get-hours "Sleep" time-summary) 1.68)
                   (/ (sacha/quantified-get-hours "Sleep" time-summary) 7)
                   )))
    (if (called-interactively-p 'any)
        (insert string)
      string)))

(defun sacha/org-add-line-item-task (task)
  (interactive "MTask: ")
  (org-insert-heading)
  (insert "[ ] " task)
  (let ((org-capture-entry '("t" "Tasks" entry
                             (file+headline "~/personal/organizer.org" "Tasks")
                             "")))
    (org-capture nil "t")
    (insert "TODO " task "\nSCHEDULED: <" (org-read-date) ">")))
;(define-key org-mode-map (kbd "C-c t") 'sacha/org-add-line-item-task)

(defun sacha/org-prepare-weekly-review ()
  "Prepare weekly review template."
  (interactive)
  (let ((base-date (apply 'encode-time (org-read-date-analyze "-fri" nil '(0 0 0))))
        start end)
    (setq start (format-time-string "%Y-%m-%d" (days-to-time (- (time-to-number-of-days base-date) 6))))
    (setq end (format-time-string "%Y-%m-%d" (days-to-time (1+ (time-to-number-of-days base-date)))))
    (outline-next-heading)
      (insert
       "*** Weekly review: Week ending " (format-time-string "%B %e, %Y" base-date) "  :weekly:\n"
       "*Blog posts*\n\n"
       "*Sketches*\n\n" 
       (sacha/flickr-export-and-extract start end) "\n"
       "*Link round-up*\n\n"
       (sacha/evernote-export-and-extract start end)
       "\n\n*Focus areas and time review*\n\n"
       (sacha/org-summarize-focus-areas)
       "\n")))

(defun _sacha/clean-up-flickr-list (list)
    (setq list
          (replace-regexp-in-string "\\([0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\)"
                                    "\\1.\\2.\\3" list))
    (setq list
          (replace-regexp-in-string "\\[\"" "[" list))
    (setq list
          (replace-regexp-in-string "<a href=\"\"\\([^\"]+\\).*?>.*?</a>"
                                    "[[\\1][\\2]]" list))
    (setq list
          (replace-regexp-in-string "\"
" "" (replace-regexp-in-string "\"\\]" "]" list))))

  (defun _sacha/format-flickr-link-for-org (x)
    (let ((title (assoc-default "FileName" x)))
      (format
       "- [[%s][%s]] %s"
       (assoc-default "URL" x)
       title
       (if (string= (assoc-default "Description" x) "")
           ""
         (concat "- "
                 (replace-regexp-in-string
                  "<a href=\"\"\\(.*?\\)\"\".*?>\\(.*?\\)</a>"
                  "[[\\1][\\2]]"
                  (assoc-default "Description" x)))))))


  (defun _sacha/parse-and-filter-flickr-csv-buffer (start end)
    (sort
     (delq nil
           (mapcar (lambda (x)
                     (if (and (string< (assoc-default "FileName" x) end)
                              (org-string<= start (assoc-default "FileName" x)))
                         x))
                   (csv-parse-buffer t)))
     (lambda (a b)
       (string< (assoc-default "FileName" a)
                (assoc-default "FileName" b)))))


  (defun sacha/flickr-export-and-extract (start end &optional do-insert update-db)
    "Download Flickr metadata and extract the relevant part."
    (interactive (list (org-read-date) (org-read-date) t current-prefix-arg))
    (when update-db (shell-command "c:/sacha/dropbox/bin/flickr.bat"))
    (if do-insert
        (insert (sacha/flickr-extract-links-for-review "c:/sacha/dropbox/bin/flickr_metadata.csv" start end))
      (sacha/flickr-extract-links-for-review "c:/sacha/dropbox/bin/flickr_metadata.csv" start end)))


  (defun sacha/flickr-extract-links-for-review (filename start end)
  "Extract Flickr titles and URLs from FILENAME from START to END.
  The file should be a CSV downloaded by the Flickr metadata downloader.
         Start date and end date should be strings in the form yyyy-mm-dd."
    (require 'csv)
    (let (list)
      (with-temp-buffer
        (insert-file-contents filename)
        (goto-char (point-min))
        (setq list
              (mapconcat
               '_sacha/format-flickr-link-for-org
               (_sacha/parse-and-filter-flickr-csv-buffer start end)
               "\n"))
        (setq list (_sacha/clean-up-flickr-list list))
        (if (called-interactively-p 'any)
            (insert list)
          list))))

(defun kensanata/resolve-redirect (url)
  "Resolve shortened URL by launching `curl --head' and parsing the result."
  (let* ((curl (shell-command-to-string
                (format "curl --silent --head %s" url)))
         (location (when (and (string-match "^HTTP/1\.1 301" curl)
                              (string-match "^Location: \\(.*\\)" curl))
                     (match-string 1 curl))))
    (or location url)))

(defun sacha/resolve-urls-in-region (beg end)
  "Expand URLs between BEG and END."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward org-bracket-link-regexp nil t)
        (replace-match (save-match-data (kensanata/resolve-redirect
                                         (match-string 1))) t t nil 1))
      (goto-char (point-min))
      (while (re-search-forward org-link-re-with-space nil t)
        (replace-match (save-match-data (kensanata/resolve-redirect
                                         (match-string 0))) t t nil)))))

(defun sacha/open-urls-in-region (beg end)
  "Open URLs between BEG and END."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward org-plain-link-re nil t)
        (org-open-at-point)))))

(defun sacha/evernote-export-and-extract (start-date end-date)
  "Extract notes created on or after START-DATE and before END-DATE."
  (let ((filename "c:/sacha/tmp/Evernote.enex"))
    (call-process 
     "c:/Program Files (x86)/Evernote/Evernote/enscript.exe"
     nil t t
     "exportNotes"
     "/q" (concat
           " tag:roundup"
           " created:" (replace-regexp-in-string "-" "" start-date)
           " -created:" (replace-regexp-in-string "-" "" end-date))
     "/f" filename)
    (sacha/evernote-extract-links-for-review filename)))

(defun sacha/evernote-extract-links-for-review (filename)
  "Extract note names and URLs from FILENAME.
     The file should be an ENEX export."
  (interactive (list (read-file-name "File: ")
                     (org-read-date)
                     (org-read-date)))
  (let (list)
    (with-temp-buffer
      (insert-file-contents filename)
      (goto-char (point-min))
      (while (re-search-forward "<title>\\(.+?\\)</title>\\(.*?\n\\)*?.*?href=\"\\(.*?\\)\"" nil t)
        (setq list
              (cons
               (cons
                (match-string-no-properties 1)
                (match-string-no-properties 3)) list))))
    (setq list
          (mapconcat (lambda (x)
                       (concat "- [["
                               (kensanata/resolve-redirect (cdr x))
                               "][" (car x) "]]: ")) list "\n"))
          (if (called-interactively-p 'any)
              (insert list)
            list)))

(defun sacha/evernote-export-and-extract-journal ()
    "Extract and file journal entries."
    (interactive)
    (let ((filename "c:\\sacha\\tmp\\journal.enex")
          (journal-file "~/personal/journal.org"))
      (call-process 
       "c:/Program Files (x86)/Evernote/Evernote/enscript.exe"
       nil t t
       "exportNotes"
       "/q" (concat
             " notebook:!Inbox"
             " intitle:Journal")
       "/f" filename)
      (sacha/evernote-process-journal-entries filename journal-file)))

  (defun sacha/evernote-process-journal-entries (filename journal-file)
    "Insert all the journal entries if they do not yet exist."
    (let ((data (car (xml-parse-file filename))))
      (mapc (lambda (x)
              (if (and  (listp x) (equal (car x) 'note))
                  (sacha/evernote-create-journal-note x journal-file)))
            data)))

  (defun sacha/evernote-get-creation-date (note)
    "Return NOTE's created date as (month day year)."
    (let ((created (cadr (assoc-default 'created note))))
      (list (string-to-number (substring created 4 6)) ; month
            (string-to-number (substring created 6 8)) ; day
            (string-to-number (substring created 0 4))))) ; year

(defun sacha/evernote-create-journal-note (note journal-file)
  "Save the given NOTE to the JOURNAL-FILE."
  (with-current-buffer (find-file journal-file)
    (org-datetree-find-date-create (sacha/evernote-get-creation-date note))
    (forward-line 1)
    (when (org-at-heading-p) (save-excursion (insert "\n")))
    (let ((content (sacha/evernote-convert-content-to-org note)))
      (unless (save-excursion
                (re-search-forward (regexp-quote content) 
                (max (point) (save-excursion (org-end-of-subtree t))) t))
        (insert content)))))

(defun sacha/evernote-convert-content-to-org (note)
  "Convert Evernote content for NOTE to HTML"
  (with-temp-buffer
    (insert (cadr (assoc-default 'content note)))
    (goto-char (point-min))
    (while (re-search-forward "div>" nil t)
      (replace-match "p>"))
    (shell-command-on-region (point-min) (point-max) "pandoc -f html -t org" nil t)
    (goto-char (point-min))
    (while (re-search-forward "^\\\\+" nil t)
     (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward "\\\\+$" nil t)
     (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward "\n\n\n+" nil t)
     (replace-match "\n\n"))
    (s-trim (buffer-string))))

(defun sacha/org-move-line-to-destination ()
      "Moves the current list item to <<destination>> in the current buffer.
If no <<destination>> is found, move it to the end of the list
and indent it one level."
      (interactive)
      (save-window-excursion
        (save-excursion
          (let ((string
                 (buffer-substring-no-properties
                  (line-beginning-position) (line-end-position)))
                found)
            (delete-region (line-beginning-position) (1+ (line-end-position)))
            (save-excursion
              (goto-char (point-min))
              (when (re-search-forward "<<destination>>" nil t)
                (insert "\n" (make-string (- (match-beginning 0) (line-beginning-position)) ?\ ) (s-trim string))
                (setq found t)))
            (unless found
              (org-end-of-item-list)
              (insert string "\n"))))))
    (bind-key "C-c d" 'sacha/org-move-line-to-destination org-mode-map)

(defun sacha/org-move-line-to-end-of-list ()
  "Move the current list item to the end of the list."
  (interactive)
  (save-excursion
    (let ((string (buffer-substring-no-properties (line-beginning-position)
                                                  (line-end-position))))
      (delete-region (line-beginning-position) (1+ (line-end-position)))
      (org-end-of-item-list)
      (insert string))))

(defun sacha/org-review-month (start-date)
  "Review the month's clocked tasks and time."
  (interactive (list (org-read-date)))
  ;; Set to the beginning of the month
  (setq start-date (concat (substring start-date 0 8) "01"))
  (let ((org-agenda-show-log t)
        (org-agenda-start-with-log-mode t)
        (org-agenda-start-with-clockreport-mode t)
        (org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3)))
    (org-agenda-list nil start-date 'month)))

(defun _sacha/extract-posts-from-webpage (url)
  (with-current-buffer (url-retrieve-synchronously url)
    (goto-char (point-min))
    (re-search-forward "<pre>")
    (buffer-substring
     (point)
     (progn (re-search-forward "</pre>") (match-beginning 0)))))

(defun sacha/org-prepare-monthly-review ()
  (interactive)
  (let* ((date (decode-time))
         (month (elt date 4))
         (year (elt date 5))
         start-date
         end-date
         posts
         sketches)
    (calendar-increment-month month year -1)
    (setq start-date (format "%4d-%02d-01" year month)
          end-date (format "%4d-%02d-01" (elt date 5) (elt date 4))
          posts (_sacha/extract-posts-from-webpage
                 (format "http://sachachua.com/blog/%4d/%d?org=1"
                         year month))
          sketches (sacha/flickr-export-and-extract start-date end-date nil t)) 
    (insert
     "\n\n** Monthly review: "
     (format-time-string "%B %Y" (encode-time 0 0 0 1 month year))
     "  :monthly:review:\n"
     "*Blog posts*\n"
     posts "\n\n"
     "*Sketches*\n\n"
     sketches)))

(eval-after-load 'org
  '(progn
     (bind-key "C-c k" 'org-cut-subtree org-mode-map)
     (setq org-yank-adjusted-subtrees t)))

(defun sacha/fix-flickr-list (beg end)
  (interactive "r")
  (save-excursion (save-restriction
    (narrow-to-region beg end)
    (goto-char (point-min))
    (while (re-search-forward "\\([0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\)" nil t)
      (replace-match (concat (match-string 1) "." (match-string 2) "." (match-string 3)) nil t)))))

(defun sacha/org-file-blog-index-entries (beg end location)
  "Copy entries into blog.org."
  (interactive
   (list
    (if (region-active-p) (point) (line-beginning-position))
    (if (region-active-p) (mark) (1+ (line-end-position)))
    (let ((org-refile-targets
           '(("~/Dropbox/Public/sharing/blog.org" . (:maxlevel . 3)))))
      (save-excursion (org-refile-get-location "Location")))))
  (let ((s
         (replace-regexp-in-string
          "^[ \t]*- \\(\\[X\\] \\)?"
          "- [X] "
          (buffer-substring-no-properties beg end))))
    ;; if we're already in blog.org, delete the previous entry
    (if (string= buffer-file-name (expand-file-name "~/Dropbox/Public/sharing/blog.org"))
        (delete-region beg end))
    (save-window-excursion
      (save-excursion
        (find-file (nth 1 location))
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (nth 3 location))
            (looking-at org-outline-regexp)
            (forward-line 1)
            (insert s)
            (org-update-statistics-cookies nil)))))))
(bind-key "C-c f" 'sacha/org-file-blog-index-entries org-mode-map)

(setq org-export-with-section-numbers nil)
(setq org-html-include-timestamps nil)

(if (string= system-name "webdev")
       (setq sacha/emacs-notes-directory "~/code/dev/emacs-notes")
     (setq sacha/emacs-notes-directory "c:/sacha/code/dev/emacs-notes"))
    (setq org-publish-project-alist
          '(("public"
             :base-directory "c:/sacha/Dropbox/public"
             :publishing-directory "c:/sacha/Dropbox/public"
             :publishing-function sacha/org-html-publish-to-html-trustingly
             )
            ("sharing"
             :base-directory "c:/sacha/Dropbox/public/sharing"
             :publishing-directory "c:/sacha/Dropbox/public/sharing"
             :publishing-function sacha/org-html-publish-to-html-trustingly
             )
            ("emacs-config"
             :base-directory "~/.emacs.d"
             :publishing-directory "~/.emacs.d"
             :publishing-function sacha/org-html-publish-to-html-trustingly
             )
            ("book-notes"
             :base-directory "c:/sacha/Dropbox/books"
             :publishing-directory "c:/sacha/Dropbox/books/html"
             :publishing-function sacha/org-html-publish-to-html-trustingly
             :makeindex t)))
(load "~/code/dev/emacs-chats/build-site.el" t)
(load "~/code/dev/emacs-notes/build-site.el" t)

(defun sacha/org-publish-maybe ()
  (interactive)
  (save-excursion
    (when (org-publish-get-project-from-filename
           (buffer-file-name (buffer-base-buffer)) 'up)
      (org-publish-current-file))))
(bind-key "C-c C-p C-p" 'sacha/org-publish-maybe org-mode-map)

(defun sacha/org-publish-and-browse ()
  (interactive)
  (save-buffer)
  (sacha/org-publish-maybe)
  (browse-url (org-export-output-file-name ".html" nil default-directory)))
(bind-key "<apps> b" 'sacha/org-publish-and-browse)

(use-package org2blog 
  :load-path "~/code/org2blog"
  :config
   (progn
    (setq org-export-with-toc nil)
    (setq org-html-toplevel-hlevel 2)
    (setq org-export-htmlize-output-type 'css)
    (defadvice org2blog/wp-post-buffer (around sacha activate)
    (let ((org-confirm-babel-evaluate nil)
          (org-html-toplevel-hlevel 3))
      ad-do-it))))
(use-package htmlize :ensure htmlize)

(defun sacha/org-html-export-trustingly ()
  (interactive)
  (let ((org-confirm-babel-evaluate nil))
    (org-html-export-to-html)))

(defun sacha/org-html-publish-to-html-trustingly (plist filename pub-dir)
  (let ((org-confirm-babel-evaluate nil))
    (org-html-publish-to-html plist filename pub-dir)))

(setq org-html-head "<link rel=\"stylesheet\" type=\"text/css\"
href=\"http://sachachua.com/blog/wp-content/themes/sacha-v3/foundation/css/foundation.min.css\"></link>
<link rel=\"stylesheet\" type=\"text/css\" href=\"http://sachachua.com/org-export.css\"></link>
<link rel=\"stylesheet\" type=\"text/css\" href=\"http://sachachua.com/blog/wp-content/themes/sacha-v3/style.css\"></link>
<script src=\"http://ajax.googleapis.com/ajax/libs/jquery/1.11.0/jquery.min.js\"></script>")
(setq org-html-htmlize-output-type 'css)
(setq org-src-fontify-natively t)

(setq org-html-preamble "<a name=\"top\" id=\"top\"></a>")
(setq org-html-postamble "
<style type=\"text/css\">
.back-to-top {
    position: fixed;
    bottom: 2em;
    right: 0px;
    text-decoration: none;
    color: #000000;
    background-color: rgba(235, 235, 235, 0.80);
    font-size: 12px;
    padding: 1em;
    display: none;
}

.back-to-top:hover {    
    background-color: rgba(135, 135, 135, 0.50);
}
</style>

<div class=\"back-to-top\">
<a href=\"#top\">Back to top</a> | <a href=\"mailto:sacha@sachachua.com\">E-mail me</a>
</div>

<script type=\"text/javascript\">
    var offset = 220;
    var duration = 500;
    jQuery(window).scroll(function() {
        if (jQuery(this).scrollTop() > offset) {
            jQuery('.back-to-top').fadeIn(duration);
        } else {
            jQuery('.back-to-top').fadeOut(duration);
        }
    });
</script>")

(defun sacha/org-copy-region-as-html (beg end &optional level)
  "Make it easier to copy code for Wordpress posts and other things."
  (interactive "r\np")
  (let ((org-export-html-preamble nil)
        (org-html-toplevel-hlevel (or level 3)))
    (kill-new
     (org-export-string-as (buffer-substring beg end) 'html t))))

(defun sacha/org-copy-subtree-as-html ()
  (interactive)
  (sacha/org-copy-region-as-html
   (org-back-to-heading)
   (org-end-of-subtree)))

(setq org-html-checkbox-type 'unicode)
(setq org-html-checkbox-types
 '((unicode (on . "<span class=\"task-done\">&#x2611;</span>") 
            (off . "<span class=\"task-todo\">&#x2610;</span>") 
            (trans . "<span class=\"task-in-progress\">[-]</span>"))))

(setq org-structure-template-alist 
      '(("s" "#+begin_src ?\n\n#+end_src" "<src lang=\"?\">\n\n</src>")
        ("e" "#+begin_example\n?\n#+end_example" "<example>\n?\n</example>")
        ("q" "#+begin_quote\n?\n#+end_quote" "<quote>\n?\n</quote>")
        ("v" "#+BEGIN_VERSE\n?\n#+END_VERSE" "<verse>\n?\n</verse>")
        ("c" "#+BEGIN_COMMENT\n?\n#+END_COMMENT")
        ("p" "#+BEGIN_PRACTICE\n?\n#+END_PRACTICE")
        ("l" "#+begin_src emacs-lisp\n?\n#+end_src" "<src lang=\"emacs-lisp\">\n?\n</src>")
        ("L" "#+latex: " "<literal style=\"latex\">?</literal>")
        ("h" "#+begin_html\n?\n#+end_html" "<literal style=\"html\">\n?\n</literal>")
        ("H" "#+html: " "<literal style=\"html\">?</literal>")
        ("a" "#+begin_ascii\n?\n#+end_ascii")
        ("A" "#+ascii: ")
        ("i" "#+index: ?" "#+index: ?")
        ("I" "#+include %file ?" "<include file=%file markup=\"?\">")))

(setq org-link-abbrev-alist
    '(("google" . "http://www.google.com/search?q=")
("gmap" . "http://maps.google.com/maps?q=%s")
("blog" . "http://sachachua.com/blog/p/")))

(setq org-use-effective-time t)

(defun sacha/org-use-speed-commands-for-headings-and-lists ()
  "Activate speed commands on list items too."
  (or (and (looking-at org-outline-regexp) (looking-back "^\**"))
      (save-excursion (and (looking-at (org-item-re)) (looking-back "^[ \t]*")))))
(setq org-use-speed-commands 'sacha/org-use-speed-commands-for-headings-and-lists)

(add-to-list 'org-speed-commands-user '("x" org-todo "DONE"))
(add-to-list 'org-speed-commands-user '("y" org-todo-yesterday "DONE"))
(add-to-list 'org-speed-commands-user '("!" sacha/org-clock-in-and-track))
(add-to-list 'org-speed-commands-user '("s" call-interactively 'org-schedule))
(add-to-list 'org-speed-commands-user '("d" sacha/org-move-line-to-destination))
(add-to-list 'org-speed-commands-user '("i" call-interactively 'org-clock-in))
(add-to-list 'org-speed-commands-user '("o" call-interactively 'org-clock-out))
(add-to-list 'org-speed-commands-user '("$" call-interactively 'org-archive-subtree))
(bind-key "!" 'sacha/org-clock-in-and-track org-agenda-mode-map)

(setq org-attach-store-link-p 'attached)
(setq org-attach-auto-tag nil)

(defun sacha/org-summarize-task-status ()
  "Count number of tasks by status. 
Probably should make this a dblock someday."
  (interactive)
  (let (result)
    (org-map-entries
     (lambda ()
       (let ((todo (elt (org-heading-components) 2)))
         (if todo
             (if (assoc todo result)
                 (setcdr (assoc todo result)
                         (1+ (cdr (assoc todo result))))
               (setq result (cons (cons todo 1) result)))))))
    (message "%s" (mapconcat (lambda (x) (format "%s: %d" (car x) (cdr x)))
                             result "\n"))))

(setq org-ditaa-jar-path "C:/Sacha/Dropbox/bin/ditaa.jar")
(setq org-startup-with-inline-images t)
(use-package org
 :config
 (progn
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)
   (ditaa . t) 
   (sh . t)
   (R . t)))
(add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))))

(setq org-src-window-setup 'current-window)

(use-package ox-reveal
 :ensure ox-reveal)

(defun sacha/org-share-emacs ()
  "Share my Emacs configuration."
  (interactive)
  (let* ((destination-dir "~/Dropbox/Public/")
         (destination-filename "sacha-emacs.org"))
    (with-current-buffer (find-file "~/.emacs.d/Sacha.org")
      (save-restriction
        (save-excursion
          (widen)
          (write-region (point-min) (point-max) 
                        (expand-file-name destination-filename destination-dir))
          (with-current-buffer (find-file-noselect (expand-file-name
                                                    destination-filename destination-dir))
            (org-babel-tangle-file buffer-file-name 
                                   (expand-file-name
                                    "sacha-emacs.el" destination-dir) "emacs-lisp")
            (org-html-export-to-html)))))))

(setq org-enforce-todo-dependencies t)
(setq org-track-ordered-property-with-tag t)
(setq org-agenda-dim-blocked-tasks t)

(setq yas-indent-line 'fixed)
  (defun sacha/convert-sketch-title-to-filename (text)
    (setq text (replace-regexp-in-string "[?!]$" "" text))
    (setq text (replace-regexp-in-string "[?!:] " " - " text)))
  (ert-deftest sacha/convert-sketch-title-to-filename ()
    (should (string= (sacha/convert-sketch-title-to-filename "Test") "Test"))
    (should (string= (sacha/convert-sketch-title-to-filename "Another Test!") "Another Test"))
    (should (string= (sacha/convert-sketch-title-to-filename "Does this work? Yes") "Does this work - Yes"))
    (should (string= (sacha/convert-sketch-title-to-filename "Title: Subtitle") "Title - Subtitle"))
    )

(defun sacha/prepare-sketchnote-file ()
  (interactive)
  (let* ((base-name (org-entry-get-with-inheritance  "BASENAME"))
         (filename (expand-file-name (concat base-name ".tif") "~/dropbox/inbox/")))
    (unless base-name (error "Missing basename property"))
    (if (file-exists-p filename)
        (error "File already exists")
        (copy-file "g:/drawing-templates/custom/0 - base.tif" filename))
      (shell-command (concat (shell-quote-argument "C:/Program Files/Autodesk/SketchBook Pro 7/SketchBookPro.exe ")
                             (shell-quote-argument filename) " &"))))

(defun sacha/convert-sketched-book-to-png ()
  "Convert TIFF to PNG."
  (interactive)
  (let ((basename (org-entry-get-with-inheritance "BASENAME")))
    (shell-command (format "convert \"c:/sacha/dropbox/inbox/%s.tif\" \"c:/sacha/dropbox/inbox/%s.png\""
                           basename
                           basename))))

(defun sacha/index-sketched-book ()
  "Add entries to sketched books index."
  (interactive)
  (let* ((title (org-entry-get-with-inheritance "SHORT_TITLE"))
        (author (org-entry-get-with-inheritance "AUTHOR"))
        (basename (org-entry-get-with-inheritance "BASENAME"))
        (base-file (format "~/Dropbox/inbox/%s.png" basename)))
    (when (file-exists-p base-file)
      (copy-file base-file
                 (format "~/Dropbox/packaging/sketched-books/%s.png" basename) t t)
      (copy-file base-file
                 (format "g:/documents/photosync/Visual Book Reviews/%s.png" basename) t t))
    (find-file "~/Dropbox/packaging/sketched-books/index.org")
    (vc-git-register (list (format "%s.png" basename)))
    (goto-char (point-min))
    (re-search-forward "<<insert-point>>")                              
    (insert (format "\n- [[file:%s.png][%s - %s (sketched %s)]]\n  [[file:%s.png]]\n\n"
                    basename
                    title
                    author
                    (substring basename 0 10)
                    basename))
    (find-file "~/Dropbox/packaging/sketched-books/ebook.org")
    (goto-char (point-min))
    (re-search-forward "<<insert-point>>")
    (insert (format "\n* %s - %s (sketched %s)\n\n[[file:%s.png]]\n\n"
                    title
                    author
                    (substring basename 0 10)
                    basename))))

(defun sacha/package-sketched-book ()
  "Add the latest sketch and package the collection."
  (interactive)
  (shell-command
   (format "plink -A vagrant@127.0.0.1 -P 2222 \"cd ~/dropbox/Packaging/sketched-books; git add '%s.png'; git commit -m 'Added %s - %s' -a; git push; make all\" &"
           (org-entry-get-with-inheritance "BASENAME")
           (org-entry-get-with-inheritance "SHORT_TITLE")
           (org-entry-get-with-inheritance "AUTHOR"))))

(defun sacha/org-link-youtube-time (url beg end)
  "Link times of the form h:mm to YouTube video at URL.
Works on region defined by BEG and END."
  (interactive (list (read-string "URL: ") (point) (mark)))
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward "\\([0-9]+\\):\\([0-9]+\\)" nil t)
        (replace-match (concat "[[" url "#t=" (match-string 1) "h" (match-string 2) "m][" (match-string 0) "]]") nil t)))))

(defun sacha/org-archive-done-tasks ()
  "Archive finished or cancelled tasks."
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "TODO=\"DONE\"|TODO=\"CANCELLED\"" (if (org-before-first-heading-p) 'file 'tree)))

(defun sacha/copy-code-as-org-block-and-gist (beg end)
  (interactive "r")
  (let ((filename (file-name-base))
        (mode (symbol-name major-mode))
        (contents
         (condition-case nil (buffer-substring beg end)
           (mark-inactive (buffer-string))))
        (gist (condition-case nil (gist-region beg end)
                    (mark-inactive (gist-buffer)))))
    (kill-new
     (format "\n[[%s][Gist: %s]]\n#+begin_src %s\n%s\n#+end_src\n"
             (oref (oref gist :data) :html-url) filename
             (replace-regexp-in-string "-mode$" "" mode)
             contents))))

;; from FAQ at http://web-mode.org/ for smartparens
(defun sacha/web-mode-hook ()
  (setq web-mode-enable-auto-pairing nil))

(defun sacha/sp-web-mode-is-code-context (id action context)
  (when (and (eq action 'insert)
             (not (or (get-text-property (point) 'part-side)
                      (get-text-property (point) 'block-side))))
    t))

(use-package web-mode
  :ensure web-mode
  :init (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  :config
  (progn
    (setq web-mode-enable-current-element-highlight t)
    (setq web-mode-ac-sources-alist
          '(("css" . (ac-source-css-property))
            ("html" . (ac-source-words-in-buffer ac-source-abbrev)))
          )))

(setq-default tab-width 2)

(global-set-key (kbd "RET") 'newline-and-indent)

(when (eq system-type 'windows-nt)
  (setenv "CYGWIN" "nodosfilewarning")
  (setq shell-file-name "C:/emacs/libexec/emacs/24.4/i686-pc-mingw32/cmdproxy.exe")
  (add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)
  (add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt nil t))

(use-package expand-region
  :ensure expand-region
  :bind ("C-=" . er/expand-region))

(setq edebug-trace t)

(use-package "eldoc"
  :diminish eldoc-mode
  :commands turn-on-eldoc-mode
  :init
  (progn
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)))

;; C-c C-v l : elint current buffer in clean environment.
;; C-c C-v L : elint current buffer by multiple emacs binaries.
;;             See `erefactor-lint-emacsen'
;; C-c C-v r : Rename symbol in current buffer.
;;             Resolve `let' binding as long as i can.
;; C-c C-v R : Rename symbol in requiring modules and current buffer.
;; C-c C-v h : Highlight current symbol in this buffer
;;             and suppress `erefacthr-highlight-mode'.
;; C-c C-v d : Dehighlight all by above command.
;; C-c C-v c : Switch prefix bunch of symbols.
;;             ex: '(hoge-var hoge-func) -> '(foo-var foo-func)
;; C-c C-v ? : Display flymake elint warnings/errors

  (use-package erefactor
    :ensure erefactor
    :config
    (define-key emacs-lisp-mode-map "\C-c\C-v" erefactor-map))

(define-key emacs-lisp-mode-map (kbd "C-c .") 'find-function-at-point)
(bind-key "C-c f" 'find-function)

(defun sacha/sort-sexps-in-region (beg end)
    "Can be handy for sorting out duplicates.
Sorts the sexps from BEG to END. Leaves the point at where it
couldn't figure things out (ex: syntax errors)."
    (interactive "r")
    (let ((input (buffer-substring beg end))
          list last-point form result)
      (save-restriction
        (save-excursion
          (narrow-to-region beg end)
          (goto-char (point-min))
          (setq last-point (point-min))
          (setq form t)
          (while (and form (not (eobp)))
            (setq form (ignore-errors (read (current-buffer))))
            (when form
              (add-to-list
               'list
               (cons
                (prin1-to-string form)
                (buffer-substring last-point (point))))
              (setq last-point (point))))
          (setq list (sort list (lambda (a b) (string< (car a) (car b)))))
          (delete-region (point-min) (point))
          (insert (mapconcat 'cdr list "\n"))))))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :commands yas-global-mode
  :init
  (progn
    (add-hook 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
    (setq yas-key-syntaxes '("w_" "w_." "^ "))
    (setq yas-installed-snippets-dir "~/elisp/yasnippet-snippets")
    (setq yas-expand-only-for-last-commands '(self-insert-command))
    (yas-global-mode 1))
  :config
  (bind-key "\t" 'hippie-expand yas-minor-mode-map)
  (add-to-list 'yas-prompt-functions 'shk-yas/helm-prompt))
;;        (global-set-key (kbd "C-c y") (lambda () (interactive)
;;                                         (yas/load-directory "~/elisp/snippets")))

(defun shk-yas/helm-prompt (prompt choices &optional display-fn)
  "Use helm to select a snippet. Put this into `yas/prompt-functions.'"
  (interactive)
  (setq display-fn (or display-fn 'identity))
  (if (require 'helm-config)
      (let (tmpsource cands result rmap)
        (setq cands (mapcar (lambda (x) (funcall display-fn x)) choices))
        (setq rmap (mapcar (lambda (x) (cons (funcall display-fn x) x)) choices))
        (setq tmpsource
              (list
               (cons 'name prompt)
               (cons 'candidates cands)
               '(action . (("Expand" . (lambda (selection) selection))))
               ))
        (setq result (helm-other-buffer '(tmpsource) "*helm-select-yasnippet"))
        (if (null result)
            (signal 'quit "user quit!")
          (cdr (assoc result rmap))))
    nil))

(setq default-cursor-color "gray")
(setq yasnippet-can-fire-cursor-color "purple")

;; It will test whether it can expand, if yes, cursor color -> green.
(defun yasnippet-can-fire-p (&optional field)
  (interactive)
  (setq yas--condition-cache-timestamp (current-time))
  (let (templates-and-pos)
    (unless (and yas-expand-only-for-last-commands
                 (not (member last-command yas-expand-only-for-last-commands)))
      (setq templates-and-pos (if field
                                  (save-restriction
                                    (narrow-to-region (yas--field-start field)
                                                      (yas--field-end field))
                                    (yas--templates-for-key-at-point))
                                (yas--templates-for-key-at-point))))
    (and templates-and-pos (first templates-and-pos))))

(defun sacha/change-cursor-color-when-can-expand (&optional field)
  (interactive)
  (set-cursor-color (if (sacha/can-expand)
                        yasnippet-can-fire-cursor-color
                      default-cursor-color)))

(defun sacha/can-expand ()
  "Return true if right after an expandable thing."
  (or (abbrev--before-point) (yasnippet-can-fire-p)))

                                        ; As pointed out by Dmitri, this will make sure it will update color when needed.
(add-hook 'post-command-hook 'sacha/change-cursor-color-when-can-expand)

(defun sacha/insert-space-or-expand ()
  "For binding to the SPC SPC keychord."
  (interactive)
  (condition-case nil (or (sacha/hippie-expand-maybe nil) (insert "  "))))

(defun sacha/hippie-expand-maybe (arg)
  "Try to expand text before point, using multiple methods.
The expansion functions in `hippie-expand-try-functions-list' are
tried in order, until a possible expansion is found.  Repeated
application of `hippie-expand' inserts successively possible
expansions.
With a positive numeric argument, jumps directly to the ARG next
function in this list.  With a negative argument or just \\[universal-argument],
undoes the expansion."
  (interactive "P")
  (if (or (not arg)
          (and (integerp arg) (> arg 0)))
      (let ((first (or (= he-num -1)
                       (not (equal this-command last-command)))))
        (if first
            (progn
              (setq he-num -1)
              (setq he-tried-table nil)))
        (if arg
            (if (not first) (he-reset-string))
          (setq arg 0))
        (let ((i (max (+ he-num arg) 0)))
          (while (not (or (>= i (length hippie-expand-try-functions-list))
                          (apply (nth i hippie-expand-try-functions-list)
                                 (list (= he-num i)))))
            (setq i (1+ i)))
          (setq he-num i))
        (if (>= he-num (length hippie-expand-try-functions-list))
            (progn (setq he-num -1) nil)
          (if (and hippie-expand-verbose
                   (not (window-minibuffer-p)))
              (message "Using %s"
                       (nth he-num hippie-expand-try-functions-list)))))
    (if (and (>= he-num 0)
             (eq (marker-buffer he-string-beg) (current-buffer)))
        (progn
          (setq he-num -1)
          (he-reset-string)
          (if (and hippie-expand-verbose
                   (not (window-minibuffer-p)))
              (message "Undoing expansions"))))))

(column-number-mode 1)

(setq vc-diff-switches '("-b" "-B" "-u"))
(setq vc-git-diff-switches nil)

(defvar sacha/javascript-test-regexp (concat (regexp-quote "/** Testing **/") "\\(.*\n\\)*")
  "Regular expression matching testing-related code to remove.
See `sacha/copy-javascript-region-or-buffer'.")

(defun sacha/copy-javascript-region-or-buffer (beg end)
  "Copy the active region or the buffer, wrapping it in script tags.
Add a comment with the current filename and skip test-related
code. See `sacha/javascript-test-regexp' to change the way
test-related code is detected."
  (interactive "r")
  (unless (region-active-p)
    (setq beg (point-min) end (point-max)))
  (kill-new
   (concat
    "<script type=\"text/javascript\">\n"
    (if (buffer-file-name) (concat "// " (file-name-nondirectory (buffer-file-name)) "\n") "")
    (replace-regexp-in-string
     sacha/javascript-test-regexp
     ""
     (buffer-substring (point-min) (point-max))
     nil)
    "\n</script>")))

(use-package js2-mode
  :ensure t
  :commands js2-mode
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
    (setq-default js2-basic-offset 2))
  :config
  (progn 
    (bind-key "C-x C-e" 'js-send-last-sexp js2-mode-map)
    (bind-key "C-M-x" 'js-send-last-sexp-and-go js2-mode-map)
    (bind-key "C-c b" 'js-send-buffer js2-mode-map)
    (bind-key "C-c C-b" 'js-send-buffer-and-go js2-mode-map)
    (bind-key "C-c w" 'sacha/copy-javascript-region-or-buffer js2-mode-map)
    (bind-key "C-c l" 'js-load-file-and-go js2-mode-map)))

(use-package tern
  :init
  (progn
  (add-hook 'js2-mode-hook 'tern-mode)))

(defun sacha/magit-commit-all ()
    "Publish the current file and commit all the current changes."
    (interactive)
    (sacha/org-publish-maybe)
    (magit-status default-directory)
    (magit-stage-all)
    (call-interactively 'magit-log-edit))

  (use-package magit
    :ensure magit
    :load-path "~/elisp/magit"
    :config
    (progn
      (when (equal system-type 'windows-nt)
        (setq magit-git-executable "c:/program files (x86)/git/bin/git.exe"))
      (setq magit-diff-options '("-b")) ; ignore whitespace
      (defvar sacha/magit-limit-to-directory nil "Limit magit status to a specific directory.")
      (defun sacha/magit-status-in-directory (directory)
        "Displays magit status limited to DIRECTORY.
Uses the current `default-directory', or prompts for a directory
if called with a prefix argument. Sets `sacha/magit-limit-to-directory'
so that it's still active even after you stage a change. Very experimental."
        (interactive (list (expand-file-name
                            (if current-prefix-arg
                                (read-directory-name "Directory: ")
                              default-directory))))
        (setq sacha/magit-limit-to-directory directory)
        (magit-status directory))

      (defadvice magit-insert-untracked-files (around sacha activate)
        (if sacha/magit-limit-to-directory
            (magit-with-section (section untracked 'untracked "Untracked files:" t)
              (let ((files (cl-mapcan
                            (lambda (f)
                              (when (eq (aref f 0) ??) (list f)))
                            (magit-git-lines
                             "status" "--porcelain" "--" sacha/magit-limit-to-directory))))
                (if (not files)
                    (setq section nil)
                  (dolist (file files)
                    (setq file (magit-decode-git-path (substring file 3)))
                    (magit-with-section (section file file)
                      (insert "\t" file "\n")))
                  (insert "\n"))))
          ad-do-it))

      (defadvice magit-insert-unstaged-changes (around sacha activate)
        (if sacha/magit-limit-to-directory
            (let ((magit-current-diff-range (cons 'index 'working))
                  (magit-diff-options (copy-sequence magit-diff-options)))
              (magit-git-insert-section (unstaged "Unstaged changes:")
                  #'magit-wash-raw-diffs
                "diff-files"
                "--" sacha/magit-limit-to-directory
                ))
          ad-do-it))

      (defadvice magit-insert-staged-changes (around sacha activate)
        "Limit to `sacha/magit-limit-to-directory' if specified."
        (if sacha/magit-limit-to-directory
            (let ((no-commit (not (magit-git-success "log" "-1" "HEAD"))))
              (when (or no-commit (magit-anything-staged-p))
                (let ((magit-current-diff-range (cons "HEAD" 'index))
                      (base (if no-commit
                                (magit-git-string "mktree")
                              "HEAD"))
                      (magit-diff-options (append '("--cached") magit-diff-options)))
                  (magit-git-insert-section (staged "Staged changes:")
                      (apply-partially #'magit-wash-raw-diffs t)
                    "diff-index" "--cached" base "--" sacha/magit-limit-to-directory))))
          ad-do-it)))
    :bind (("C-x v d" . magit-status)
           ("C-x v C-d" . sacha/magit-status-in-directory)
           ("C-x v p" . magit-push) 
           ("C-x v c" . sacha/magit-commit-all)))

(defun sacha/recursive-find-file (file &optional directory)
  "Find the first FILE in DIRECTORY or its parents."
  (setq directory (or directory (file-name-directory (buffer-file-name)) (pwd)))
  (if (file-exists-p (expand-file-name file directory))
      (expand-file-name file directory)
    (unless (string= directory "/")
      (sacha/recursive-find-file file (expand-file-name ".." directory)))))

(defun sacha/find-tags ()
  "Set the TAGS file."
  (set (make-variable-buffer-local 'tags-table-list) nil)
  (set (make-variable-buffer-local 'tags-file-name) 
       (sacha/recursive-find-file "TAGS")))

(eval-after-load 'drupal-mode
  '(progn
     (add-hook 'drupal-mode-hook 'sacha/find-tags)))

(use-package projectile
  :ensure projectile
  :diminish projectile-mode
  :init 
  (progn
    (setq projectile-keymap-prefix (kbd "C-c p")) 
    (setq projectile-completion-system 'default)
    (setq projectile-enable-caching t)
    (projectile-global-mode)))
(use-package helm-projectile
   :ensure helm-projectile)



(use-package rinari :ensure rinari)
(use-package bundler :ensure bundler)
    (use-package robe
      :ensure robe
      :init
      (progn (add-hook 'ruby-mode-hook 'robe-mode)
             (add-hook 'robe-mode-hook 'ac-robe-setup)
             (add-hook 'ruby-mode-hook 'auto-complete-mode)))

(defun sacha/rspec-verify-single ()
  "Runs the specified example at the point of the current buffer."
  (interactive)
  (rspec-run-single-file
   (concat 
     (rspec-spec-file-for (buffer-file-name))
     ":" 
     (save-restriction
               (widen)
               (number-to-string (line-number-at-pos))))
   (rspec-core-options)))

(use-package rspec-mode
  :ensure rspec-mode
  :config
  (progn 
    (setq rspec-command-options "--fail-fast --format documentation")
    (bind-key "C-c , ," 'rspec-rerun rspec-mode-map)
    (fset 'rspec-verify-single 'sacha/rspec-verify-single)))

(add-hook 'sass-mode-hook
          (lambda () (setq indent-tabs-mode nil)))
(setq-default indent-tabs-mode nil)

(use-package skewer-mode
  :ensure skewer-mode
  :config (skewer-setup))

(use-package company
  :ensure company
  :config
  (add-hook 'prog-mode-hook 'company-mode))

(defun sacha/ledger-go-to-beginning-of-entry ()
  "Move to the beginning of the current entry."
  (while (and (not (bobp))
              (eq (ledger-context-line-type (ledger-context-at-point))
                  'acct-transaction))
    (forward-line -1)))

(defun sacha/ledger-entry-date ()
  "Returns the date of the entry containing point or nil."
  (save-excursion
    (sacha/ledger-go-to-beginning-of-entry)
    (let ((context-info (ledger-context-other-line 0)))
      (when (eq (ledger-context-line-type context-info) 'entry)
        (goto-char (line-beginning-position))
        (if (looking-at "\\([-0-9\\./]+\\)")
            (match-string-no-properties 1))))))

(defun sacha/ledger-guess-mbna ()
  "Adds a sub-account for the dates for my credit card transactions."
  (interactive)
  (save-excursion
    (sacha/ledger-go-to-beginning-of-entry)
    (forward-line 1)
    (let ((amount 0) (date (sacha/ledger-entry-date)) month)
      (if (string-match "[0-9]+[-\\.]\\([0-9]+\\)[-\\.]\\([0-9]+\\)" date)
          (setq month (string-to-number (match-string 1 date))))
      ;; Is this a payment or a charge?
      (save-excursion
        (while (and (eq (ledger-context-line-type (ledger-context-at-point))
                        'acct-transaction)
                    (not (eobp)))
          (let ((context (ledger-context-at-point)))
            (if (ledger-context-field-value context 'amount)
                (if (string-match "MBNA" (ledger-context-field-value context 'account))
                    (setq amount (string-to-number (ledger-context-field-value context 'amount)))
                  (setq amount (- (string-to-number (ledger-context-field-value context 'amount)))))))
          (forward-line 1)))
      (save-excursion
        (while (and (eq (ledger-context-line-type (ledger-context-at-point))
                        'acct-transaction)
                    (not (eobp)))
          (let ((context (ledger-context-at-point)))
            (if (string-match "MBNA" (ledger-context-field-value context 'account))
                (if (re-search-forward "\\(MBNA\\)[ \t]*[-$\.0-9]*[ \t]*$" (line-end-position) t)
                    (replace-match
                     (concat "MBNA:"
                             (elt
                              '("January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December")
                              (% (+ (if (> amount 0) 10 11) month) 12)))
                             t t nil 1))))
          (forward-line 1))))))

(use-package erc
  :ensure erc
  :config
  (setq erc-autojoin-channels-alist '(("freenode.net"
         "#org-mode"
         "#hacklabto"
         "#emacs"))
  erc-server "irc.freenode.net"
  erc-nick "sachac"))

(defun sacha/org-clock-in-and-track ()
  "Start the clock running. Clock into Quantified Awesome."
  (interactive)
  (if (derived-mode-p 'org-agenda-mode) (org-agenda-clock-in) (org-clock-in))
  (call-interactively 'sacha/org-quantified-track))
(bind-key "!" 'sacha/org-clock-in-and-track org-agenda-mode-map)

(defmacro sacha/with-org-task (&rest body)
  "Run BODY within the current agenda task, clocked task, or cursor task."
  `(cond
    ((derived-mode-p 'org-agenda-mode)
     (let* ((marker (org-get-at-bol 'org-marker))
            (buffer (marker-buffer marker))
            (pos (marker-position marker)))
       (with-current-buffer buffer
         (save-excursion
           (save-restriction
             (widen)
             (goto-char pos)
             ,@body)))))
    ((and (derived-mode-p 'org-mode) (org-at-heading-p)) (save-excursion ,@body))
    ((org-clocking-p) (save-excursion (org-clock-goto) ,@body))
    ((derived-mode-p 'org-mode) ,@body)))

(defun sacha/org-quantified-track (&optional category note)
  "Create a tracking record using CATEGORY and NOTE.
Default to the current task in the agenda, the currently-clocked
entry, or the current subtree in Org."
  (interactive (list nil nil))
  (unless (and category note)
    (sacha/with-org-task
     (setq category (or category
                        (org-entry-get-with-inheritance "QUANTIFIED")))
     (unless category
       (setq category (read-string "Category: "))
       (org-set-property "QUANTIFIED" category))
     (setq note (or note (elt (org-heading-components) 4) (read-string "Note: ")))))
  (quantified-track (concat category " | " note)))
  
(defun sacha/org-quick-clock-in-task (location jump)
  "Track and clock in on the specified task.
If JUMP is non-nil or the function is called with the prefix argument, jump to that location afterwards."
  (interactive (list (save-excursion (org-refile-get-location "Location")) current-prefix-arg))
  (when location
    (if jump
        (progn (org-refile 4 nil location) (sacha/org-clock-in-and-track))
      (save-window-excursion
        (org-refile 4 nil location)
        (sacha/org-clock-in-and-track)))))
(bind-key "C-c q" 'sacha/org-quick-clock-in-task)

(use-package quantified)

(defun sacha/compare-times (clocked estimated)
  (if (and (> (length clocked) 0) estimated)
      (format "%.2f"
            (/ (* 1.0 (org-hh:mm-string-to-minutes clocked))
               (org-hh:mm-string-to-minutes estimated)))
    ""))

(use-package ess-site
  :ensure ess
  :commands R)

(defvar sacha/workrave-file (expand-file-name ".\\Workrave\\historystats" (getenv "AppData")))

(defun sacha/workrave-transform-statistics (&optional file)
  (interactive (list sacha/workrave-file))
  (with-current-buffer (find-file-noselect file)
  ;; D day month-1 year hour min day month-1 year hour min
    (let ((result "Date\tStart\tEnd\tClicks\tKeystrokes\n"))
      (goto-char (point-min))
      (while (re-search-forward "^D \\(.*\\)" nil t)
  (let ((dates (split-string (match-string 1))))
    (if (re-search-forward "^m \\(.*\\)" nil t)
        (let ((info (split-string (match-string 1))))
    (setq result
          (concat result
            (format "%d-%d-%s\t%s:%02d\t%s:%02d\t%s\t%s\n"
              (+ 1900 (string-to-number (elt dates 2))) ; year
              (1+ (string-to-number (elt dates 1))) ; month
              (elt dates 0) ; day
              (elt dates 3) ; start hour
              (string-to-number (elt dates 4)) ; start min
              (elt dates 8) ; end hour
              (string-to-number (elt dates 9)) ; end min
              (elt info 5) ; clicks
              (elt info 6) ; keystrokes
              )))))))
      (if (interactive-p)
    (kill-new result)
  result))))

(defun sacha/strip-blog-share ()
  (interactive)
  (let (base)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward 
              "<div class=\"sharedaddy sd-sharing-enabled\">.*?<div class=\"sharing-clear\"></div></div></div></div>" nil t)
        (replace-match "")))))

(defun sacha/artrage-export-png (directory &optional prefix)
          "Change an Artrage script file (arscript) to export images to DIRECTORY. 
    If PREFIX is specified, use that instead of image-."
          (interactive "MPath: ")
          (unless (file-directory-p directory)
            (make-directory directory t))
          (while (re-search-forward "[0-9\\.]+s" nil t)
            (replace-match "0.000s"))
          (goto-char (point-min))
          (while (search-forward "<StrokeEvent>" nil t)
            (replace-match (concat 
                            "EvType: Command    CommandID: ExportLayer    Idx: -1    Channels: NO    Path: \""
                            directory
                            "/" (or prefix "image-")
                            ".png\"
<StrokeEvent>") t t)))

(defadvice face-attribute (around sacha activate)
  (if (symbolp (ad-get-arg 0))
      ad-do-it))

(defadvice ido-sort-mtime (around sacha activate)
  (setq ido-temp-list
        (sort ido-temp-list 
              (lambda (a b)
                (let ((ta (or (nth 5 (file-attributes (concat ido-current-directory a))) '(0 0)))
                      (tb (or (nth 5 (file-attributes (concat ido-current-directory b))) '(0 0))))
                  (if (= (nth 0 ta) (nth 0 tb))
                      (> (nth 1 ta) (nth 1 tb))
                    (> (nth 0 ta) (nth 0 tb)))))))
  (setq ad-return-value
        (ido-to-end  ;; move . files to end (again)
         (delq nil (mapcar
                    (lambda (x) (if (string-equal (substring x 0 1) ".") x))
                    ido-temp-list)))))

;(setq eimp-mogrify-program "c:/Program Files/ImageMagick-6.8.3-Q16/mogrify.exe")

(use-package multiple-cursors
  :ensure multiple-cursors
  :bind 
   (("C-c m a" . mc/mark-all-like-this)
    ("C-c m m" . mc/mark-all-like-this-dwim)
    ("C-c m l" . mc/edit-lines)
    ("C-c m n" . mc/mark-next-like-this)
    ("C-c m p" . mc/mark-previous-like-this)
    ("C-c m s" . mc/mark-sgml-tag-pair)
    ("C-c m d" . mc/mark-all-like-this-in-defun)))
(use-package phi-search
  :ensure phi-search)
(use-package phi-search-mc
  :ensure phi-search-mc
  :config
  (phi-search-mc/setup-keys))
(use-package mc-extras
  :ensure mc-extras
  :config
    (define-key mc/keymap (kbd "C-. =") 'mc/compare-chars))

(use-package edit-list
  :ensure edit-list
  :commands edit-list)

(use-package ace-jump-mode
  :ensure ace-jump-mode)
;; I use the jj key-chord for this; see the definitions for key-chord

(use-package ace-window
  :ensure ace-window
  :config (setq aw-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s))
  :bind ("C-x o" . ace-window))

(use-package ace-isearch
  :ensure ace-isearch
  :init (global-ace-isearch-mode 1))

(use-package ace-jump-zap
  :ensure ace-jump-zap
  :bind
  (("M-z" . ace-jump-zap-up-to-char-dwim)
   ("C-M-z" . ace-jump-zap-to-char-dwim)))

(setq tramp-default-method "plink")
(setq tramp-auto-save-directory "c:\\sacha\\tmp")

(use-package smartparens
  :ensure smartparens
  :diminish smartparens
  :config
  (progn
    (require 'smartparens-config)
    (add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
    (add-hook 'emacs-lisp-mode-hook 'show-smartparens-mode)

;;;;;;;;;;;;;;;;;;;;;;;;
    ;; keybinding management

    (define-key sp-keymap (kbd "C-c s r n") 'sp-narrow-to-sexp)
    (define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
    (define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)
    (define-key sp-keymap (kbd "C-M-d") 'sp-down-sexp)
    (define-key sp-keymap (kbd "C-M-a") 'sp-backward-down-sexp)
    (define-key sp-keymap (kbd "C-S-a") 'sp-beginning-of-sexp)
    (define-key sp-keymap (kbd "C-S-d") 'sp-end-of-sexp)

    (define-key sp-keymap (kbd "C-M-e") 'sp-up-sexp)
    (define-key emacs-lisp-mode-map (kbd ")") 'sp-up-sexp)
    (define-key sp-keymap (kbd "C-M-u") 'sp-backward-up-sexp)
    (define-key sp-keymap (kbd "C-M-t") 'sp-transpose-sexp)

    (define-key sp-keymap (kbd "C-M-n") 'sp-next-sexp)
    (define-key sp-keymap (kbd "C-M-p") 'sp-previous-sexp)

    (define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)
    (define-key sp-keymap (kbd "C-M-w") 'sp-copy-sexp)

    (define-key sp-keymap (kbd "M-<delete>") 'sp-unwrap-sexp)
    (define-key sp-keymap (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)

    (define-key sp-keymap (kbd "C-<right>") 'sp-forward-slurp-sexp)
    (define-key sp-keymap (kbd "C-<left>") 'sp-forward-barf-sexp)
    (define-key sp-keymap (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
    (define-key sp-keymap (kbd "C-M-<right>") 'sp-backward-barf-sexp)

    (define-key sp-keymap (kbd "M-D") 'sp-splice-sexp)
    (define-key sp-keymap (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
    (define-key sp-keymap (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
    (define-key sp-keymap (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)

    (define-key sp-keymap (kbd "C-]") 'sp-select-next-thing-exchange)
    (define-key sp-keymap (kbd "C-<left_bracket>") 'sp-select-previous-thing)
    (define-key sp-keymap (kbd "C-M-]") 'sp-select-next-thing)

    (define-key sp-keymap (kbd "M-F") 'sp-forward-symbol)
    (define-key sp-keymap (kbd "M-B") 'sp-backward-symbol)

    (define-key sp-keymap (kbd "C-c s t") 'sp-prefix-tag-object)
    (define-key sp-keymap (kbd "C-c s p") 'sp-prefix-pair-object)
    (define-key sp-keymap (kbd "C-c s c") 'sp-convolute-sexp)
    (define-key sp-keymap (kbd "C-c s a") 'sp-absorb-sexp)
    (define-key sp-keymap (kbd "C-c s e") 'sp-emit-sexp)
    (define-key sp-keymap (kbd "C-c s p") 'sp-add-to-previous-sexp)
    (define-key sp-keymap (kbd "C-c s n") 'sp-add-to-next-sexp)
    (define-key sp-keymap (kbd "C-c s j") 'sp-join-sexp)
    (define-key sp-keymap (kbd "C-c s s") 'sp-split-sexp)

;;;;;;;;;;;;;;;;;;
    ;; pair management

    (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
    (sp-local-pair 'web-mode "<" nil :when '(sacha/sp-web-mode-is-code-context))

;;; markdown-mode
    (sp-with-modes '(markdown-mode gfm-mode rst-mode)
      (sp-local-pair "*" "*" :bind "C-*")
      (sp-local-tag "2" "**" "**")
      (sp-local-tag "s" "```scheme" "```")
      (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

;;; tex-mode latex-mode
    (sp-with-modes '(tex-mode plain-tex-mode latex-mode)
      (sp-local-tag "i" "1d5f8e69396c521f645375107197ea4dfbc7b792quot;<" "1d5f8e69396c521f645375107197ea4dfbc7b792quot;>"))

;;; html-mode
    (sp-with-modes '(html-mode sgml-mode web-mode)
      (sp-local-pair "<" ">"))

;;; lisp modes
    (sp-with-modes sp--lisp-modes
      (sp-local-pair "(" nil :bind "C-("))))

(setq browse-url-firefox-program
      "C:/Program Files (x86)/Mozilla Firefox/firefox.exe")

(use-package emms
  :ensure emms
  :config
  (progn
    (require 'emms-player-simple)
    (require 'emms-source-file)
    (require 'emms-source-playlist)
    (require 'emms-player-mplayer)
    (setq emms-player-list '(emms-player-mplayer))
    )
  :bind
  (("C-c e SPC" . emms-pause)
   ("C-c e e" . emms-pause)
   ("C-c e +" . emms-seek-forward)
   ("C-c e -" . emms-seek-backward)
   ("C-c e s" . emms-seek)
   ("C-c e [" . sacha/emms-player-mplayer-slow-down)
   ("C-c e ]" . sacha/emms-player-mplayer-speed-up)))

(bind-key "C-c t s"  'sacha/split-sentence-and-capitalize org-mode-map)
(bind-key "C-c t -"  'sacha/split-sentence-delete-word-and-capitalize org-mode-map)
(bind-key "C-c t d"  'sacha/delete-word-and-capitalize org-mode-map)

(defun sacha/split-sentence-and-capitalize ()
  (interactive)
  (delete-char 1)
  (insert ".")
  (capitalize-word 1))
(defun sacha/split-sentence-delete-word-and-capitalize ()
  (interactive)
  (delete-char 1)
  (insert ".")
  (kill-word 1)
  (capitalize-word 1))
(defun sacha/delete-word-and-capitalize ()
  (interactive)
  (skip-syntax-backward "w")
  (kill-word 1)
  (capitalize-word 1))

(defun sacha/emms-player-mplayer-set-speed (speed)
  "Depends on mplayer's -slave mode"
  (interactive "MSpeed: ")
  (process-send-string emms-player-simple-process-name 
     (format "speed_set %s\n" speed)))

(defvar sacha/emms-player-mplayer-speed-increment 0.1)

(defun sacha/emms-player-mplayer-speed-up ()
  "Depends on mplayer's -slave mode"
  (interactive)
  (process-send-string emms-player-simple-process-name 
     (format "speed_incr %f\n" sacha/emms-player-mplayer-speed-increment)))
(defun sacha/emms-player-mplayer-slow-down ()
  "Depends on mplayer's -slave mode"
  (interactive)
  (process-send-string emms-player-simple-process-name 
     (format "speed_incr %f\n" (- 0 sacha/emms-player-mplayer-speed-increment))))

(add-to-list 'load-path "~/elisp/sdic-2.1.3/lisp")
(require 'sdic)
(cond ((file-exists-p "~/Dropbox/Japanese/edict.txt")
       (setq sdic-waei-dictionary-list
       (cons
        '(sdicf-client "~/Dropbox/Japanese/edict.txt" (add-keys-to-headword t))
        sdic-waei-dictionary-list))))
(cond ((file-exists-p "~/Dropbox/Japanese/jedict.sdic.txt")
       (setq sdic-waei-dictionary-list
       (cons
        '(sdicf-client "~/Dropbox/Japanese/jedict.sdic.txt" (add-keys-to-headword t))
        sdic-waei-dictionary-list))))

(defvar sacha/org-quantified-categories 
    '(("Business" 
       ("Earn" . "Business - Earn") 
       ("E1" . "Business - Earn - Consulting - E1") 
       ("Connect" . "Business - Connect") 
       ("Build" . "Business - Build"))
      ("Discretionary"
       ("Social" . "Discretionary - Social")
       ("Productive" . "Discretionary - Productive")
       ("Writing" . "Discretionary - Productive - Writing")
       ("Emacs" . "Discretionary - Productive - Emacs")
       ("Play" . "Discretionary - Play"))
      ("Personal" ;("Biking" . "Personal - Bike")
       ("Routines" . "Personal - Routines"))
      ("Sleep" nil)
      ("Unpaid work" 
       ("Commuting" . "Unpaid work - Subway")
       ("Cook" . "Unpaid work - Cook")
       ("Tidy" . "Unpaid work - Tidy up")))
    "Categories for time summary.")
  
  (defun sacha/org-summarize-time-use (&optional start end)
    (require 'quantified)
    (interactive)
    (unless start (setq start (format-time-string "%Y-%m-%d" (days-to-time (- (time-to-number-of-days base-date) 6)))))
    (unless end (setq end (format-time-string "%Y-%m-%d" (days-to-time (1+ (time-to-number-of-days base-date))))))
    (let ((time-summary (quantified-summarize-time start end))
          (categories sacha/org-quantified-categories)
          result)
      (setq result
            (mapconcat
             (lambda (a)
               (if (assoc (car a) time-summary)
                   (concat
                    (format "- %s: %.1f hours" (car a) (/ (cdr (assoc (car a) time-summary)) 3600.0))
                    (if (cdr a)
                        (let ((detail
                               (delq nil
                                     (mapcar (lambda (b)
                                               (if (assoc (cdr b) time-summary)
                                                   (format "%s: %.1f"
                                                           (car b)
                                                           (/ (cdr (assoc (cdr b) time-summary)) 3600.0))
                                                 nil))
                                             (cdr a)))))
                          (if detail
                              (concat " (" (mapconcat 'identity detail ", ") ")")
                            ""))
                      "")
                    (if (string-equal (car a) "Sleep")
                        (format " - average of %.1f hours per day" (/ (cdr (assoc (car a) time-summary)) 3600.0 7.0))
                      "")
                    "\n")))
       categories ""))
(if (called-interactively-p)
    (insert result)
  result)))

(defun sacha/org-summarize-upcoming-week ()
  "Summarize upcoming tasks as a list."
  (interactive)
  (org-agenda nil "w")
  (let ((string (buffer-string))
        business relationships life)
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (while (re-search-forward sacha/weekly-review-line-regexp nil t)
        (cond
         ((string= (match-string 1) "routines") nil) ; skip routine tasks
         ((string= (match-string 1) "business")
          (add-to-list 'business (concat "  - [ ] " (match-string 3))))
         ((string= (match-string 1) "people")
          (add-to-list 'relationships (concat "  - [ ] " (match-string 3))))
         (t (add-to-list 'life (concat "  - [ ] " (match-string 3)))))))
    (setq string
          (concat
      "*Plans for next week*\n"
      "- Business\n"
      (mapconcat 'identity business "\n")
      "\n- Relationships\n"
      (mapconcat 'identity relationships "\n")
      "\n- Life\n"
      (mapconcat 'identity life "\n")))
    (if (called-interactively-p)
        (kill-new string)
      string)))

(defun sacha/org-summarize-previous-week ()
  "Summarize previously-completed tasks as a list."
  (interactive)
  (save-window-excursion
    (org-agenda nil "w")
    (org-agenda-later -1)
    (org-agenda-log-mode 16)
    (let ((string (buffer-string))
          business relationships life)
      (with-temp-buffer
        (insert string)
      (goto-char (point-min))
      (while (re-search-forward sacha/weekly-review-line-regexp nil t)
        (cond
         ((string= (match-string 1) "routines") nil) ; skip routine tasks
         ((string= (match-string 1) "business")
          (add-to-list 'business (concat "  - " (match-string 2))))
         ((string= (match-string 1) "people")
          (add-to-list 'relationships (concat "  - " (match-string 2))))
         (t (add-to-list 'life (concat "  - " (match-string 2)))))))
    (setq string
          (concat
           "*Accomplished this week*\n\n"
           "- Business\n"
           (mapconcat 'identity business "\n")
           "\n- Relationships\n"
           (mapconcat 'identity relationships "\n")
           "\n- Life\n"
           (mapconcat 'identity life "\n")))
    (if (called-interactively-p)
        (kill-new string)
      string))))

(defun sacha/animate-emacs-chat ()
  (interactive)
  (text-scale-set 6)
  (erase-buffer)
  (sit-for 3)
  (let ((list '("Emacs Chat: Sacha Chua"
                "interviewed by Bastien Guerry"
                ""
                "July 24, 2013"
                "sachachua.com/emacs-chat"))
        (approx-width 41)
        (approx-height 16)
        row)
    (setq row (/ (- approx-height (length list)) 2))
    (mapcar
     (lambda (x)
       (animate-string x
                       row
                       (/ (- approx-width (length x)) 2))
       (setq row (1+ row)))
     list)))

(defface 2048-face-2    '((t . (:background "khaki" :foreground "black"))) "Face for the tile 2" :group '2048-faces)
(defface 2048-face-4    '((t . (:background "burlywood" :foreground "black"))) "Face for the tile 4" :group '2048-faces)
(defface 2048-face-8    '((t . (:background "orange3" :foreground "black"))) "Face for the tile 8" :group '2048-faces)
(defface 2048-face-16   '((t . (:background "orange" :foreground "black"))) "Face for the tile 16" :group '2048-faces)
(defface 2048-face-32   '((t . (:background "orange red" :foreground "black"))) "Face for the tile 32" :group '2048-faces)
(defface 2048-face-64   '((t . (:background "firebrick" :foreground "white"))) "Face for the tile 64" :group '2048-faces)
(defface 2048-face-128  '((t . (:background "dark red" :foreground "white"))) "Face for the tile 128" :group '2048-faces)
(defface 2048-face-256  '((t . (:background "dark magenta" :foreground "white"))) "Face for the tile 256" :group '2048-faces)
(defface 2048-face-512  '((t . (:background "magenta" :foreground "black"))) "Face for the tile 512" :group '2048-faces)
(defface 2048-face-1024 '((t . (:background "gold" :foreground "black"))) "Face for the tile 1024" :group '2048-faces)
(defface 2048-face-2048 '((t . (:background "yellow" :foreground "black"))) "Face for the tile 2048" :group '2048-faces)


  (defun sacha/2048-set-font-size ()
    (text-scale-set 5)
    (goto-char (point-min)))

  (use-package 2048-game
    :config
    (progn
     (add-hook '2048-mode-hook 'sacha/2048-set-font-size)))

(setenv "PATH" (concat "\"c:/program files/postgresql/9.3/bin;\"" (getenv "PATH")))
