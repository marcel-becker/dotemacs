(use-package hydra
  :bind (
         ;;("C-c L" . hydra-ledger/body)
         ;;("C-c b" . hydra-btoggle/body)
         ;;("C-c c" . hydra-clock/body)
         ;;("C-c e" . hydra-erc/body)
         ("C-z c" . hydra-flycheck/body)
         ;;("C-c g" . hydra-go-to-file/body)
         ("C-z m" . hydra-magit/body)
         ("C-z o" . hydra-org/body)
         ("C-z p" . hydra-projectile/body)
         ;;("C-c q" . hydra-query/body)
         ("C-z s" . hydra-spelling/body)
         ("C-z t" . hydra-tex/body)
         ("C-z T" . hydra-tool/body)
         ("C-c u" . hydra-upload/body)
         ("C-z w" . hydra-window/body)
         ("C-z q" . hydra-window-move-and-split)
         ("C-z C-e" . emms-player-simple-mpv-hydra/body)
         )
  :config
  (set-face-attribute 'hydra-face-red      nil :foreground "Red"        :bold t)
  (set-face-attribute 'hydra-face-blue     nil :foreground "RoyalBlue3" :bold t)
  (set-face-attribute 'hydra-face-amaranth nil :foreground "#e52b50"    :bold t)
  (set-face-attribute 'hydra-face-pink     nil :foreground "HotPink1"   :bold t)
  (set-face-attribute 'hydra-face-teal     nil :foreground "#367588"    :bold t)
  (setq hydra-hint-display-type  'message
        lv-use-separator t)
  (add-hook 'hydra-posframe-mode-hook
            (lambda ()
              (setq show-trailing-whitespace nil)))

  )

(use-package major-mode-hydra
  :after hydra
  :bind
  ([f13] . major-mode-hydra)
  :preface
  (defun with-alltheicon (icon str &optional height v-adjust)
    "Displays an icon from all-the-icon."
    (s-concat (all-the-icons-alltheicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

  (defun with-faicon (icon str &optional height v-adjust)
    "Displays an icon from Font Awesome icon."
    (s-concat (all-the-icons-faicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

  (defun with-fileicon (icon str &optional height v-adjust)
    "Displays an icon from the Atom File Icons package."
    (s-concat (all-the-icons-fileicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

  (defun with-octicon (icon str &optional height v-adjust)
    "Displays an icon from the GitHub Octicons."
    (s-concat (all-the-icons-octicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))
  :config
  (setq major-mode-hydra-title-generator
        '(lambda (mode)
           (s-concat "\n"
                     (s-repeat 10 " ")
                     (all-the-icons-icon-for-mode mode :v-adjust 0.05)
                     " "
                     (symbol-name mode)
                     " commands")))
  )

(bind-key*
 "C-z w"
 (defhydra hydra-window (:color red :hint nil)
   "
Split^             ^Delete^         ^Window Move^   ^Splitter Move^    ^Frames^        ^Misc^
--------------------------------------------------------------------------------------------------------
_v_: right         _o_:others       _h_: left       _H_: left          _f_: new        _m_: mark
_x_: below         _da_: ace        _j_: down       _J_: down          _df_: delete    _a_: ace
_|_: move right    _dw_: window     _k_: up         _K_: up            ^          ^    _u_: winner undo
___: move down     _db_: buffer     _l_: right      _L_:right          ^          ^    _r_: winner redo
^             ^    _df_: frame      _s_: swap       ^             ^    ^          ^    _q_: quit
"
   ("v" split-window-right)
   ("x" split-window-below)
   ("|" (lambda ()
          (interactive)
          (split-window-right)
          (windmove-right)))
   ("_" (lambda ()
          (interactive)
          (split-window-below)
          (windmove-down)))


   ("h" windmove-left)
   ("j" windmove-down)
   ("k" windmove-up)
   ("l" windmove-right)
   ("H" hydra-move-splitter-left)
   ("J" hydra-move-splitter-down)
   ("K" hydra-move-splitter-up)
   ("L" hydra-move-splitter-right)

   ;;("t" transpose-frame "'")
   ;; winner-mode must be enabled
   ("u" winner-undo)
   ("r" winner-redo) ;;Fixme, not working?
   ("o" delete-other-windows :exit t)
   ("a" ace-window :exit t)
   ("f" new-frame :exit t)
   ("s" ace-swap-window)
   ("da" ace-delete-window)
   ("dw" delete-window)
   ("db" kill-this-buffer)
   ("df" delete-frame :exit t)
   ;;("i" ace-maximize-window "ace-one" :color blue)
   ;;("b" ido-switch-buffer "buf")
   ("m" headlong-bookmark-jump)
   ("q" nil)
   ))

(setq major-mode-hydra-title-generator
      '(lambda (mode)
         (s-concat "\n"
                   (s-repeat 10 " ")
                   (all-the-icons-icon-for-mode mode :v-adjust 0.05)
                   " "
                   (symbol-name mode)
                   " commands")))


(defun hydra-move-splitter-left (arg)
  "Move window splitter left."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

(defun hydra-move-splitter-right (arg)
  "Move window splitter right."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

(defun hydra-move-splitter-up (arg)
  "Move window splitter up."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

(defun hydra-move-splitter-down (arg)
  "Move window splitter down."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))

;; window movement / management
(bind-key*
 "C-z q"
 (defhydra hydra-window-move-and-split (:color red :hint nil)
   "
Movement      ^Split^            ^Switch^        ^Resize^
----------------------------------------------------------------
_j_ ←           _v_ertical         _b_uffer        _u_ ←
_k_ ↓           _h_orizontal       _f_ind files    _i_ ↓
_l_ ↑           _1_only this       _P_rojectile    _o_ ↑
_;_ →           _d_elete           _s_wap          _p_ →
_F_ollow        _e_qualize         _[_backward     _8_0 columns
_q_uit          ^        ^         _]_forward
"
   ("j" windmove-left)
   ("k" windmove-down)
   ("l" windmove-up)
   (";" windmove-right)
   ("[" previous-buffer)
   ("]" next-buffer)
   ("u" hydra-move-splitter-left)
   ("i" hydra-move-splitter-down)
   ("o" hydra-move-splitter-up)
   ("p" hydra-move-splitter-right)
   ("b" ivy-switch-buffer)
   ("f" counsel-find-file)
   ("P" counsel-projectile-find-file)
   ("F" follow-mode)
   ("s" switch-window-then-swap-buffer)
   ("8" set-80-columns)
   ("v" split-window-right)
   ("h" split-window-below)
   ("3" split-window-right)
   ("2" split-window-below)
   ("d" delete-window)
   ("1" delete-other-windows)
   ("e" balance-windows)
   ("q" nil)))


(bind-key*
 "C-z #"
 (defhydra hydra-resize-window (:color red)
   ""
   ("<left>" shrink-window-horizontally "-narrower-")
   ("<right>" enlarge-window-horizontally "-wider-")
   ("<down>" shrink-window "|shorter|")
   ("<up>" enlarge-window "|longer|")
   ("=" balance-windows "equal")
   ("q"  nil)))


;; avy
(bind-key*
 "C-z a"
 (defhydra hydra-avy (:color blue)
   "Avy-Goto"
   ("c" avy-goto-char "char")
   ("C" avy-goto-char-2 "char-2")
   ("w" avy-goto-word-1 "word")
   ("." avy-goto-word-1)
   ("s" avy-goto-subword-1 "subword")
   ("l" avy-goto-line "line")
   ("q" nil "quit")))


(bind-key* "C-z C-f"
(pretty-hydra-define hydra-frames
  (:color teal :quit-key "q" :title (with-faicon "arrows" "Frames" 1 -0.05))
  ("Move"
   (("d" move-frame-down "Down" :color pink)
    ("u" move-frame-up   "Up" :color pink)
    ("l" move-frame-left "Left" :color pink)
    ("r" move-frame-right "Right" :color pink))
   "Position"
   (("t" (move-frame-to-screen-top nil) "Screen Top" )
    ("b" (move-frame-to-screen-bottom t) "Screen Bottom" )
    ("L" (move-frame-to-screen-left nil) "Screen Left" )
    ("R" (move-frame-to-screen-right nil) "Screen Right")
    )
   "Resize"
   (("<" shrink-frame "Decrease Height" :color pink)
    (">" enlarge-frame "Increase Height" :color pink)
    ("<left>" enlarge-frame-horizontally "Increase Width" :color pink)
    ("<right>" shrink-frame-horizontally "Decrease Width" :color pink))
   "Other"
   (("M" flycheck-manual "manual")
    ("v" flycheck-verify-setup "verify setup"))))
)

;;https://github.com/rememberYou/.emacs.d/blob/master/config.org
(pretty-hydra-define hydra-flycheck
  (:color teal :quit-key "q" :title (with-faicon "plane" "Flycheck" 1 -0.05))
  ("Checker"
   (("?" flycheck-describe-checker "describe")
    ("d" flycheck-disable-checker "disable")
    ("m" flycheck-mode "mode")
    ("s" flycheck-select-checker "select"))
   "Errors"
   (("<" flycheck-previous-error "previous" :color pink)
    (">" flycheck-next-error "next" :color pink)
    ("f" flycheck-buffer "check")
    ("l" flycheck-list-errors "list"))
   "Other"
   (("M" flycheck-manual "manual")
    ("v" flycheck-verify-setup "verify setup"))))


(pretty-hydra-define hydra-magit
  (:color teal :quit-key "q" :title (with-alltheicon "git" "Magit" 1 -0.05))
  ("Action"
   (("b" magit-blame "blame")
    ("c" magit-clone "clone")
    ("i" magit-init "init")
    ("l" magit-log-buffer-file "commit log (current file)")
    ("L" magit-log-current "commit log (project)")
    ("s" magit-status "status"))))


(pretty-hydra-define hydra-org
  (:hint nil :color teal :quit-key "q" :title (with-fileicon "org" "Org" 1 -0.05))
  ("Action"
   (("A" my/org-archive-done-tasks "archive")
    ("a" org-agenda "agenda")
    ("c" org-capture "capture")
    ("d" org-decrypt-entry "decrypt")
    ("i" org-insert-link-global "insert-link")
    ("j" my/org-jump "jump-task")
    ("k" org-cut-subtree "cut-subtree")
    ("o" org-open-at-point-global "open-link")
    ("r" org-refile "refile")
    ("s" org-store-link "store-link")
    ("t" org-show-todo-tree "todo-tree"))))


(pretty-hydra-define hydra-projectile
  (:hint nil :color teal :quit-key "q" :title (with-faicon "rocket" "Projectile" 1 -0.05))
  ("Buffers"
   (("b" counsel-projectile-switch-to-buffer "list")
    ("k" projectile-kill-buffers "kill all")
    ("S" projectile-save-project-buffers "save all"))
   "Find"
   (("d" counsel-projectile-find-dir "directory")
    ("D" projectile-dired "root")
    ("f" counsel-projectile-find-file "file")
    ("p" counsel-projectile-switch-project "project"))
   "Other"
   (("i" projectile-invalidate-cache "reset cache"))
   "Search"
   (("r" projectile-replace "replace")
    ("R" projectile-replace-regexp "regexp replace")
    ("s" counsel-rg "search"))))

(pretty-hydra-define hydra-query
  (:hint nil :color teal :quit-key "q" :title (with-faicon "search" "Engine-Mode" 1 -0.05))
  ("Query"
   (("a" engine/search-amazon "amazon")
    ("c" engine/search-amazon "ctan")
    ("d" engine/search-duckduckgo "duckduckgo")
    ("g" engine/search-github "github")
    ("i" engine/search-google-images "google images")
    ("m" engine/search-google-maps "google maps")
    ("s" engine/search-stack-overflow "stack overflow")
    ("w" engine/search-wikipedia "wikipedia")
    ("y" engine/search-youtube "youtube"))))


(pretty-hydra-define hydra-spelling
  (:hint nil :color teal :quit-key "q" :title (with-faicon "magic" "Spelling" 1 -0.05))
  ("Checker"
   (("c" langtool-correct-buffer "correction")
    ("C" langtool-check-done "clear")
    ("d" ispell-change-dictionary "dictionary")
    ("l" (message "Current language: %s (%s)" langtool-default-language ispell-current-dictionary) "language")
    ("s" my/switch-language "switch")
    ("w" wiki-summary "wiki"))
   "Errors"
   (("<" flyspell-correct-previous "previous" :color pink)
    (">" flyspell-correct-next "next" :color pink)
    ("f" langtool-check "find"))))

(pretty-hydra-define hydra-tex
  (:hint nil :color teal :quit-key "q" :title (with-fileicon "tex" "LaTeX" 1 -0.05))
  ("Action"
   (("g" reftex-goto-label "goto")
    ("r" reftex-query-replace-document "replace")
    ("s" counsel-rg "search")
    ("t" reftex-toc "table of content"))))


(defhydra hydra-dired (:hint nil :color pink)
  "
  _+_ mkdir          _v_iew           _m_ark             _(_ details        _i_nsert-subdir    wdired
  _C_opy             _O_ view other   _U_nmark all       _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit
  _D_elete           _o_pen other     _u_nmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
  _R_ename           _M_ chmod        _t_oggle           _g_ revert buf     _e_ ediff          C-c ESC : abort
  _Y_ rel symlink    _G_ chgrp        _E_xtension mark   _s_ort             _=_ pdiff
  _S_ymlink          ^ ^              _F_ind marked      _._ toggle hydra   \\ flyspell
  _r_sync            ^ ^              ^ ^                ^ ^                _?_ summary
  _z_ compress-file  _A_ find regexp
  _Z_ compress       _Q_ repl regexp

  T - tag prefix
  "
  ("\\" dired-do-ispell)
  ("(" dired-hide-details-mode)
  (")" dired-omit-mode)
  ("+" dired-create-directory)
  ("=" diredp-ediff)         ;; smart diff
  ("?" dired-summary)
  ("$" diredp-hide-subdir-nomove)
  ("A" dired-do-find-regexp)
  ("C" dired-do-copy)        ;; Copy all marked files
  ("D" dired-do-delete)
  ("E" dired-mark-extension)
  ("e" dired-ediff-files)
  ("F" dired-do-find-marked-files)
  ("G" dired-do-chgrp)
  ("g" revert-buffer)        ;; read all directories again (refresh)
  ("i" dired-maybe-insert-subdir)
  ("l" dired-do-redisplay)   ;; relist the marked or singel directory
  ("M" dired-do-chmod)
  ("m" dired-mark)
  ("O" dired-display-file)
  ("o" dired-find-file-other-window)
  ("Q" dired-do-find-regexp-and-replace)
  ("R" dired-do-rename)
  ("r" dired-do-rsynch)
  ("S" dired-do-symlink)
  ("s" dired-sort-toggle-or-edit)
  ("t" dired-toggle-marks)
  ("U" dired-unmark-all-marks)
  ("u" dired-unmark)
  ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
  ("w" dired-kill-subdir)
  ("Y" dired-do-relsymlink)
  ("z" diredp-compress-this-file)
  ("Z" dired-do-compress)
  ("q" nil)
  ("." nil :color blue))



;; This setting example emulates default key bindings of mpv player as mutch as possible.

(defvar emms-player-simple-mpv-hydra-docstring
  "
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃      Keyboard Control for emms simple player of mpv      ┃
┣━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┫
   _Q_      Quit emms-player-simple-mpv-hydra.
  ─────────────────────────────
   _<left>_ and _<right>_
          Seek backward/forward 5 seconds.
   _S-<left>_ and _S-<right>_
          Seek backward/forward 1 seconds.
   _<down>_ and <up>
          Seek backward/forward 1 minute.
   _S-<down>_ and S-<upt>
          Seek backward/forward 5 seconds.
  ─────────────────────────────
   _\[_ and _\]_
          Decrease/increase current playback speed by 10 %%%%.
   _\{_ and _\}_
          Halve/double current playback speed.
   _<backspace>_
          Reset playback speed to normal.
  ─────────────────────────────
   _<_ and _>_
          Go backward/forward in the playlist.
  _<return>_
          Go forward in the playlist.
  ─────────────────────────────
  _p_ / _SPC_
          Pause (pressing again unpauses).
  ─────────────────────────────
   _q_      Stop playing and quit.
  ─────────────────────────────
   _/_ and _*_
          Decrease/increase volume.
   _9_ and _0_
          Decrease/increase volume.
  ─────────────────────────────
   _m_      Mute sound.
  ─────────────────────────────
   _f_      Toggle fullscreen.
  ─────────────────────────────
   _T_      Toggle stay-on-top.
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛

"
  "Docstring for `emms-player-simple-mpv-hydra/body'.")


(eval-after-load "hydra"
  `(defhydra emms-player-simple-mpv-hydra     (:foreign-keys warn :hint nil)
     ,emms-player-simple-mpv-hydra-docstring
     ("Q" nil)
     ("<left>"    (lambda () (interactive) (emms-seek -5)))
     ("S-<left>"  (lambda () (interactive) (emms-seek -1)))
     ("<down>"    (lambda () (interactive) (emms-seek -60)))
     ("S-<down>"  (lambda () (interactive) (emms-seek -5)))
     ("<right>"   (lambda () (interactive) (emms-seek 5)))
     ("S-<right>" (lambda () (interactive) (emms-seek 1)))
     ("<up>"      (lambda () (interactive) (emms-seek 60)))
     ("S-<up>"    (lambda () (interactive) (emms-seek 5)))
     ("["  emms-player-simple-mpv-speed-decrease)
     ("]"  emms-player-simple-mpv-speed-increase)
     ("{"  emms-player-simple-mpv-speed-halve)
     ("}"  emms-player-simple-mpv-speed-double)
     ("<backspace>" emms-player-simple-mpv-speed-normal)
     ("<" emms-player-simple-mpv-playlist-prev)
     (">" emms-player-simple-mpv-playlist-next)
     ("<return>" emms-player-simple-mpv-playlist-next)
     ("p" emms-pause)
     ("SPC" emms-pause)
     ("q" (lambda () (interactive)
            (when (y-or-n-p "emms-stop?")
              (emms-stop))) :exit t)
     ("/" emms-volume-lower)
     ("*" emms-volume-raise)
     ("9" emms-volume-lower)
     ("0" emms-volume-raise)
     ("m" emms-player-simple-mpv-mute)
     ("f" emms-player-simple-mpv-fullscreen)
     ("T" emms-player-simple-mpv-ontop)))



(bind-key*
 "C-z C-e"
 (pretty-hydra-define emms-player-simple-mpv-hydra
   (:color red :foreign-keys warn :hint nil :title  (with-faicon "headphones" "Music" 1 -0.05))
   ("Seek"
    (("q" nil)
     ("<left>"    (lambda () (interactive) (emms-seek -5)) "Back 5s")
     ("S-<left>"  (lambda () (interactive) (emms-seek -1)) "Back 1s")
     ("<down>"    (lambda () (interactive) (emms-seek -60)) "Back 1m")
     ("S-<down>"  (lambda () (interactive) (emms-seek -5)) "Back 5s")
     ("<right>"   (lambda () (interactive) (emms-seek 5)) "Forward 5s")
     ("S-<right>" (lambda () (interactive) (emms-seek 1)) "Forward 1s")
     ("<up>"      (lambda () (interactive) (emms-seek 60)) "Forward 1m")
     ("S-<up>"    (lambda () (interactive) (emms-seek 5)) "Forward 5s"))

    "Speed"
    (("["  emms-player-simple-mpv-speed-decrease "decrease")
     ("]"  emms-player-simple-mpv-speed-increase "increase")
     ("{"  emms-player-simple-mpv-speed-halve "half")
     ("}"  emms-player-simple-mpv-speed-double "double")
     ("<backspace>" emms-player-simple-mpv-speed-normal "normal"))
    "Playlist"
    (("<" emms-player-simple-mpv-playlist-prev "Prev")
     (">" emms-player-simple-mpv-playlist-next "Next")
     ("<return>" emms-player-simple-mpv-playlist-next "Next"))
    "Playback"
    (("p" emms-pause "pause")
     ("SPC" emms-pause "pause")
     ("Q" (lambda () (interactive)
            (when (y-or-n-p "emms-stop?")
              (emms-stop)))
      :exit t
      "Quit EMMS"))
    "Volume"
    (("/" emms-volume-lower "lower")
     ("*" emms-volume-raise "raise")
     ("9" emms-volume-lower "lower")
     ("0" emms-volume-raise "raise")
     ("m" emms-player-simple-mpv-mute "mute" :toggle t))
    "Player"
    (("f" emms-player-simple-mpv-fullscreen "fullscreen" :toggle t)
     ("T" emms-player-simple-mpv-ontop "on top" :toggle t)))))


(major-mode-hydra-define emacs-lisp-mode nil
  ("Eval"
   (("b" eval-buffer "buffer")
    ("e" eval-defun "defun")
    ("r" eval-region "region"))
   "REPL"
   (("I" ielm "ielm"))
   "Test"
   (("t" ert "prompt")
    ("T" (ert t) "all")
    ("F" (ert :failed) "failed"))
   "Doc"
   (("d" describe-foo-at-point "thing-at-pt")
    ("f" describe-function "function")
    ("v" describe-variable "variable")
    ("i" info-lookup-symbol "info lookup"))))
