(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-output-view-style
   (quote
    (("^dvi$" "^pstricks$\\|^pst-\\|^psfrag$" "dvips %d -o && start \"\" %f")
     ("^dvi$"
      ("^a5\\(?:comb\\|paper\\)$" "^landscape$")
      "windvi %d %dS -qpaper a5r -s 0")
     ("^dvi$" "^a5\\(?:comb\\|paper\\)$" "windvi %d %dS -qpaper a5")
     ("^dvi$" "^b5paper$" "windvi %d %dS -qpaper b5")
     ("^dvi$"
      ("^landscape$" "^pstricks$\\|^psfrag$")
      "dvips -t landscape %d -o && start \"\" %f")
     ("^dvi$" "^letterpaper$" "windvi %d %dS -qpaper us")
     ("^dvi$" "^legalpaper$" "windvi %d %dS -qpaper legal")
     ("^dvi$" "^executivepaper$" "windvi %d %dS -qpaper 7.25x10.5in")
     ("^dvi$" "^landscape$" "windvi %d %dS -qpaper a4r")
     ("^dvi$" "." "windvi %d %dS")
     ("^pdf$" "." "C:/Program Files (x86)/SumatraPDF/SumatraPDF.exe %o")
     ("^html?$" "." "start \"\" %o"))))
 '(TeX-view-program-selection
   (quote
    (((output-dvi style-pstricks)
      "dvips and start")
     (output-dvi "Yap")
     (output-pdf "SumatraPDF")
     (output-html "start"))))
 '(anzu-deactivate-region t)
 '(anzu-mode-lighter "")
 '(anzu-replace-threshold 50)
 '(anzu-replace-to-string-separator " => ")
 '(anzu-search-threshold 1000)
 '(aquamacs-additional-fontsets nil t)
 '(aquamacs-customization-version-id 307 t)
 '(aquamacs-tool-bar-user-customization nil t)
 '(auto-save-default nil)
 '(backup-inhibited t t)
 '(blink-cursor-mode nil)
 '(c-default-style
   (quote
    ((java-mode . "eclipse")
     (awk-mode . "awk")
     (other . "gnu"))))
 '(cursor-type (quote box))
 '(custom-safe-themes
   (quote
    ("810ab30a73c460f5c49ede85d1b9af3429ff2dff652534518fa1de7adc83d0f6" "6c0a087a4f49c04d4002393ffd149672f70e4ab38d69bbe8b39059b61682b61c" "39dd7106e6387e0c45dfce8ed44351078f6acd29a345d8b22e7b8e54ac25bac4" "cab317d0125d7aab145bc7ee03a1e16804d5abdfa2aa8738198ac30dc5f7b569" "f782ed87369a7d568cee28d14922aa6d639f49dd676124d817dd82c8208985d0" "8062d7fd259d3232d69b38db3b15d4ac44a70bf620cbc5b3926a6e16c74d6a5a" "60f04e478dedc16397353fb9f33f0d895ea3dab4f581307fbf0aa2f07e658a40" "7e3316825e6cdf7829743c88e66f85743f9665ec7cbb76846e7f00b938fef481" "71b172ea4aad108801421cc5251edb6c792f3adbaecfa1c52e94e3d99634dee7" "d2622a2a2966905a5237b54f35996ca6fda2f79a9253d44793cfe31079e3c92b" "501caa208affa1145ccbb4b74b6cd66c3091e41c5bb66c677feda9def5eab19c" "72cc9ae08503b8e977801c6d6ec17043b55313cda34bcf0e6921f2f04cf2da56" default)))
 '(default-frame-alist
    (quote
     ((background-color . "#09223F")
      (height . 50)
      (cursor-color . "white")
      (mouse-color . "white")
      (foreground-color . "white")
      (cursor-type . box)
      (width . 130))))
 '(dialog-frame-plist (quote (width 90 height 20 top 20)))
 '(display-time-mode t)
 '(ecb-options-version "2.40")
 '(echo-keystrokes 0.01)
 '(evil-want-Y-yank-to-eol nil)
 '(fci-rule-color "gray80")
 '(fill-column 120)
 '(frame-title-format (quote ("%f - " user-real-login-name "@" system-name)) t)
 '(global-hl-line-mode t)
 '(global-linum-mode t)
 '(highlight-symbol-colors
   (quote
    ("#EFFF00" "#73CD4F" "#83DDFF" "MediumPurple1" "#66CDAA" "DarkOrange" "HotPink1" "#809FFF" "#ADFF2F")))
 '(hl-paren-background-colors
   (quote
    ("#00FF99" "#CCFF99" "#FFCC99" "#FF9999" "#FF99CC" "#CC99FF" "#9999FF" "#99CCFF" "#99FFCC" "#7FFF00")))
 '(hl-paren-colors (quote ("#326B6B")))
 '(ido-auto-merge-work-directories-length nil)
 '(ido-create-new-buffer (quote always))
 '(ido-enable-flex-matching t)
 '(ido-enable-prefix nil)
 '(ido-ignore-extensions t)
 '(ido-max-prospects 8)
 '(ido-use-filename-at-point (quote guess))
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(linum-format "  %d  ")
 '(minimap-dedicated-window nil)
 '(minimap-hide-fringes t)
 '(minimap-recreate-window nil)
 '(minimap-update-delay 0)
 '(minimap-window-location (quote right))
 '(ns-alternate-modifier (quote alt))
 '(ns-right-alternate-modifier (quote super))
 '(ns-tool-bar-display-mode (quote both) t)
 '(ns-tool-bar-size-mode (quote regular) t)
 '(one-key-special-keybindings
   (quote
    ((dir-documentation documentation "Show one-key-dir documentation"
                        (lambda nil
                          (finder-commentary
                           (locate-library "one-key-dir"))
                          (setq one-key-menu-window-configuration nil)
                          nil))
     (quit-close "q" "Quit and close menu window"
                 (lambda nil
                   (setq one-key-buffer-temp-action
                         (quote close))))
     (quit-open "C-q" "Quit, but keep menu window open"
                (lambda nil
                  (setq one-key-buffer-temp-action
                        (quote deselect))))
     (toggle-persistence "<C-menu>" "Toggle menu persistence"
                         (lambda nil
                           (if one-key-buffer-match-action
                               (setq one-key-buffer-match-action nil one-key-buffer-miss-match-action nil)
                             (setq one-key-buffer-match-action
                                   (quote close)
                                   one-key-buffer-miss-match-action
                                   (quote close)))))
     (toggle-display "<menu>" "Toggle menu display" one-key-menu-window-toggle)
     (next-menu "<left>" "Change to left menu"
                (lambda nil
                  (setq one-key-buffer-menu-number
                        (mod
                         (1- one-key-buffer-menu-number)
                         (length one-key-buffer-menu-alists))
                        one-key-default-menu-number one-key-buffer-menu-number)
                  (one-key-update-buffer-contents)
                  (one-key-set-window-state
                   (nth one-key-window-toggle-pos one-key-window-toggle-sequence))))
     (prev-menu "<right>" "Change to right menu"
                (lambda nil
                  (setq one-key-buffer-menu-number
                        (mod
                         (1+ one-key-buffer-menu-number)
                         (length one-key-buffer-menu-alists))
                        one-key-default-menu-number one-key-buffer-menu-number)
                  (one-key-update-buffer-contents)
                  (one-key-set-window-state
                   (nth one-key-window-toggle-pos one-key-window-toggle-sequence))))
     (skip-menus-left "<C-left>" "Skip menus to left"
                      (lambda nil
                        (let*
                            ((nummenus
                              (length one-key-buffer-menu-alists))
                             (skipnum
                              (max
                               (round
                                (* nummenus 0.333))
                               2)))
                          (setq one-key-buffer-menu-number
                                (mod
                                 (- one-key-buffer-menu-number skipnum)
                                 nummenus)
                                one-key-default-menu-number one-key-buffer-menu-number)
                          (one-key-update-buffer-contents)
                          (one-key-set-window-state
                           (nth one-key-window-toggle-pos one-key-window-toggle-sequence)))))
     (skip-menus-right "<C-right>" "Skip menus to right"
                       (lambda nil
                         (let*
                             ((nummenus
                               (length one-key-buffer-menu-alists))
                              (skipnum
                               (max
                                (round
                                 (* nummenus 0.333))
                                2)))
                           (setq one-key-buffer-menu-number
                                 (mod
                                  (+ one-key-buffer-menu-number skipnum)
                                  nummenus)
                                 one-key-default-menu-number one-key-buffer-menu-number)
                           (one-key-update-buffer-contents)
                           (one-key-set-window-state
                            (nth one-key-window-toggle-pos one-key-window-toggle-sequence)))))
     (up "<up>" "Scroll/move up one line" one-key-menu-window-scroll-up-line)
     (down "<down>" "Scroll/move down one line"
           #[128 "\302\300\303\301\"\"\207"
                 [one-key-menu-window-scroll-up-line
                  (t)
                  apply append]
                 6 "

(fn &rest ARGS2)"])
     (scroll-down "<prior>" "Scroll menu down one page"
                  #[128 "\302\300\303\301\"\"\207"
                        [one-key-menu-window-scroll-up
                         (t)
                         apply append]
                        6 "

(fn &rest ARGS2)"])
     (scroll-up "<next>" "Scroll menu up one page" one-key-menu-window-scroll-up)
     (documentation "<S-f1>" "Show one-key documentation"
                    (lambda nil
                      (finder-commentary
                       (locate-library "one-key"))))
     (save-menu "C-s" "Save state of current menu"
                (lambda nil
                  (one-key-save-menu one-key-buffer-this-name one-key-buffer-full-list)))
     (toggle-help "<f1>" "Toggle this help buffer"
                  (lambda nil
                    (if
                        (get-buffer-window one-key-help-buffer-name)
                        (one-key-set-window-state
                         (quote hidehelp))
                      (one-key-set-window-state
                       (quote showhelp))
                      (one-key-show-help one-key-buffer-special-keybindings))))
     (toggle-row/column-order "<f2>" "Toggle column/row ordering of items"
                              (lambda nil
                                (setq one-key-column-major-order
                                      (not one-key-column-major-order))
                                (one-key-update-buffer-contents)))
     (sort-next "<f3>" "Sort items by next method"
                #[128 "\302\300\303\301\"\"\207"
                      [one-key-sort-items-by-next-method
                       (t)
                       apply append]
                      6 "

(fn &rest ARGS2)"])
     (sort-prev "<C-f3>" "Sort items by previous method"
                #[128 "\302\300\303\301\"\"\207"
                      [one-key-sort-items-by-next-method
                       (t t)
                       apply append]
                      6 "

(fn &rest ARGS2)"])
     (reverse-order "<f4>" "Reverse order of items" one-key-reverse-item-order)
     (limit-items "/" "Limit items to those matching regexp"
                  (lambda nil
                    (setq one-key-buffer-filter-regex
                          (read-regexp "Regular expression matching items to be filtered"))
                    (one-key-update-buffer-contents)))
     (highlight-items "C-/" "Highlight items matching regexp"
                      (lambda nil
                        (let
                            ((regex
                              (read-regexp "Regular expression matching items to be coloured"))
                             (bgcolour
                              (read-color "Colour: ")))
                          (one-key-highlight-matching-items bgcolour
                                                            (lambda
                                                              (item)
                                                              (string-match regex
                                                                            (cdar item)))))))
     (edit-item "<f5>" "Edit a menu item" one-key-edit-menu-item)
     (delete-item "<f6>" "Delete a menu item" one-key-delete-menu-item)
     (kill-items "<f7>" "Copy/kill coloured items" one-key-copy/kill-items)
     (yank-items "<C-f7>" "Yank copied items" one-key-yank-items)
     (swap-keys "<f8>" "Swap menu item keys" one-key-swap-menu-items)
     (add-item "<f9>" "Add a menu item" one-key-prompt-to-add-menu-item)
     (add-menu "<C-f9>" "Add a menu" one-key-add-menus)
     (remove-menu "<C-S-f9>" "Remove this menu" one-key-delete-menus)
     (move-item "<f10>" "Reposition item (with arrow keys)"
                (lambda nil
                  (let
                      ((key
                        (one-key-key-description
                         (read-event "Enter key of item to be moved"))))
                    (setq one-key-current-item-being-moved key))))
     (donate "<f11>" "Donate to support further development"
             (lambda nil
               (browse-url "http://onekeydonate.dynalias.net")))
     (report-bug "<C-f11>" "Report a bug" one-key-submit-bug-report)
     (show-menusets "C-h" "Show menus in menu set"
                    (lambda nil
                      (let*
                          ((key
                            (read-event "Enter the key for the menu set"))
                           (item
                            (one-key-get-menu-item key one-key-buffer-full-list))
                           (menuset
                            (assoc
                             (cdar item)
                             one-key-sets-of-menus-alist))
                           (desc
                            (car menuset))
                           (names
                            (cdr menuset)))
                        (message "%S" names))))
     (customize-menusets "C-c" "Customize menu sets"
                         (lambda nil
                           (one-key-set-window-state
                            (quote close))
                           (with-selected-window
                               (previous-window)
                             (customize-group
                              (quote one-key-menu-sets)))))
     (change-default-menuset "<f5>" "Change default menu set"
                             (lambda nil
                               (let*
                                   ((key
                                     (read-event "Press the key of item to set as default"))
                                    (item
                                     (one-key-get-menu-item key one-key-buffer-full-list))
                                    (name
                                     (cdar item))
                                    (pos
                                     (position "menu-sets" one-key-buffer-menu-names :test
                                               (quote equal))))
                                 (if name
                                     (eval
                                      (\`
                                       (customize-save-variable
                                        (quote one-key-default-menu-set)
                                        (\,
                                         (substring-no-properties name))))))
                                 (if pos
                                     (setf
                                      (nth pos one-key-buffer-menu-alists)
                                      (one-key-build-menu-sets-menu-alist))))
                               (one-key-update-buffer-contents)))
     (save-menuset save-menu "Save current menu set"
                   (lambda nil
                     (let*
                         ((names
                           (mapcar
                            (quote car)
                            one-key-sets-of-menus-alist))
                          (newname
                           (read-string "Name for menu set: "))
                          (validnames
                           (remq nil
                                 (mapcar
                                  (lambda
                                    (name)
                                    (if
                                        (one-key-get-menu-type name)
                                        name))
                                  one-key-buffer-menu-names)))
                          newset oldsets)
                       (unless
                           (and
                            (member newname names)
                            (not
                             (y-or-n-p "A menu set with that name already exists, overwrite it?")))
                         (setq newset
                               (if
                                   (y-or-n-p "Include \"menu-sets\" menu?")
                                   (append
                                    (list newname)
                                    validnames)
                                 (remove "menu-sets"
                                         (append
                                          (list newname)
                                          validnames))))
                         (setq oldsets
                               (remove-if
                                (lambda
                                  (item)
                                  (string=
                                   (car item)
                                   newname))
                                one-key-sets-of-menus-alist))
                         (if
                             (y-or-n-p "Associate menu set with major-mode?")
                             (let
                                 ((mode
                                   (with-selected-window
                                       (or one-key-buffer-associated-window
                                           (selected-window))
                                     major-mode)))
                               (eval
                                (\`
                                 (customize-save-variable
                                  (quote one-key-associations-for-menu-sets)
                                  (quote
                                   (\,
                                    (one-key-add-to-alist
                                     (quote one-key-associations-for-menu-sets)
                                     (cons mode newname))))))))
                           (if
                               (y-or-n-p "Associate menu set with current buffer?")
                               (let
                                   ((regex
                                     (with-selected-window
                                         (or one-key-buffer-associated-window
                                             (selected-window))
                                       (concat "^"
                                               (regexp-quote
                                                (buffer-name))
                                               "$"))))
                                 (eval
                                  (\`
                                   (customize-save-variable
                                    (quote one-key-associations-for-menu-sets)
                                    (quote
                                     (\,
                                      (one-key-add-to-alist
                                       (quote one-key-associations-for-menu-sets)
                                       (cons regex newname))))))))))
                         (eval
                          (\`
                           (customize-save-variable
                            (quote one-key-sets-of-menus-alist)
                            (quote
                             (\,
                              (append oldsets
                                      (list newset)))))))))
                     (one-key-update-buffer-contents)))
     (rebuild-menu "<M-f11>" "Rebuild the menu" one-key-rebuild-menu)
     (read-tree-up "RET" "Complete current list"
                   (lambda nil
                     (setq selected-item
                           (quote goup))))
     (read-tree-up2 ")" "Complete current list"
                    (lambda nil
                      (setq selected-item
                            (quote goup))))
     (read-tree-down "SPC" "Start new list recursively"
                     (lambda nil
                       (setq selected-item
                             (quote godown))))
     (read-tree-down2 "(" "Start new list recursively"
                      (lambda nil
                        (setq selected-item
                              (quote godown))))
     (read-tree-delete "<backspace>" "Remove last item from list"
                       (lambda nil
                         (setq selected-item
                               (quote del))))
     (read-logical-negate "!" "Negate next item"
                          (lambda nil
                            (setq selected-item
                                  (quote not)))))))
 '(org-agenda-files (quote ("~/Dropbox/EmacsOrg/1.org")))
 '(package-selected-packages
   (quote
    (spaceline-all-the-icons swiper-helm nlinum yascroll ace-jump-mode makey ranger zoom-frm spacemacs-whitespace-cleanup spaceline-config org-mime org-agenda ob image-mode helm-spacemacs-help helm-spacemacs-faq evil-org dired-x centered-cursor-mode hybrid-mode holy-mode evil-evilified-state unfill solarized-theme madhat2r-theme request winum zonokai-theme zenburn-theme zen-and-art-theme yaml-mode ws-butler window-numbering which-key web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme toc-org tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme spacemacs-theme spaceline spacegray-theme soothe-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle seti-theme reverse-theme restart-emacs rainbow-delimiters railscasts-theme quelpa purple-haze-theme professional-theme popwin planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme persp-mode pcre2el pastels-on-dark-theme paradox orgit organic-green-theme org-projectile org-present org-pomodoro org-plus-contrib org-download org-bullets open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme niflheim-theme neotree naquadah-theme mwim mustang-theme move-text monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode minimal-theme material-theme markdown-toc majapahit-theme magit-gitflow macrostep lush-theme lorem-ipsum livid-mode linum-relative link-hint light-soap-theme json-mode js2-refactor js-doc jbeans-theme jazz-theme ir-black-theme inkpot-theme info+ indent-guide ido-vertical-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt heroku-theme hemisu-theme help-fns+ helm-themes helm-swoop helm-pydoc helm-projectile helm-package helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-company helm-c-yasnippet helm-anything helm-ag hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme google-translate golden-ratio gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gh-md gandalf-theme flyspell-correct-helm flycheck-pos-tip flx-ido flatui-theme flatland-theme firebelly-theme fill-column-indicator farmhouse-theme fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu espresso-theme elisp-slime-nav dumb-jump dracula-theme django-theme diff-hl define-word darktooth-theme darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme company-tern company-statistics column-enforce-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized coffee-mode clues-theme clean-aindent-mode cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes aggressive-indent afternoon-theme adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell autothemer web-beautify livid-mode skewer-mode simple-httpd json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc company-tern dash-functional tern coffee-mode yaml-mode zonokai-theme zenburn-theme zen-and-art-theme underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme spacegray-theme soothe-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme seti-theme reverse-theme railscasts-theme purple-haze-theme professional-theme planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme pastels-on-dark-theme organic-green-theme omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme niflheim-theme naquadah-theme mustang-theme monokai-theme monochrome-theme molokai-theme moe-theme minimal-theme material-theme majapahit-theme lush-theme light-soap-theme jbeans-theme jazz-theme ir-black-theme inkpot-theme heroku-theme hemisu-theme hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme gandalf-theme flatui-theme flatland-theme firebelly-theme farmhouse-theme espresso-theme dracula-theme django-theme darktooth-theme darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clues-theme cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes afternoon-theme org-projectile pcache org-present org-pomodoro alert log4e gntp org-download htmlize gnuplot flycheck-pos-tip pos-tip ws-butler uuidgen toc-org spaceline restart-emacs persp-mode spinner orgit org org-plus-contrib org-bullets mwim mmm-mode markdown-mode lorem-ipsum link-hint hydra hl-todo parent-mode highlight-indentation hide-comnt help-fns+ projectile pkg-info epl request helm-flx helm-company gitignore-mode git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter flyspell-correct-helm flyspell-correct flx eyebrowse evil-visual-mark-mode evil-unimpaired evil-mc evil-magit magit magit-popup git-commit with-editor smartparens evil-indent-plus iedit evil-ediff anzu evil goto-chg undo-tree highlight dumb-jump f s diminish company column-enforce-mode bind-map bind-key yasnippet auto-compile packed dash ace-jump-helm-line helm avy helm-core async auto-complete popup package-build diff-hl zencoding-mode window-numbering window-number which-key websocket volatile-highlights vline virtualenvwrapper vi-tilde-fringe use-package unbound tabbar switch-window swiper sr-speedbar spray spotify spacemacs-theme smooth-scrolling smooth-scroll smeargle smartrep shell-command redo+ recentf-ext rainbow-mode rainbow-delimiters quelpa python-pep8 python-mode pytest pyenv-mode pydoc-info pydoc pycomplete py-autopep8 powerline popwin popup-kill-ring pip-requirements pcre2el paradox page-break-lines open-junk-file nxml-mode nose nginx-mode neotree move-text menu-bar+ markdown-toc magit-gitflow macrostep mac-key-mode lua-mode linum-relative leuven-theme json-rpc json-mode jedi info+ indent-guide idomenu ido-vertical-mode icicles hy-mode hungry-delete highlight-parentheses highlight-numbers hexrgb helm-themes helm-swoop helm-spotify helm-pydoc helm-projectile helm-package helm-mode-manager helm-make helm-ls-git helm-helm-commands helm-gitignore helm-git-files helm-git helm-descbinds helm-c-yasnippet helm-anything helm-ag header2 goto-last-change google-translate golden-ratio gitconfig-mode gitattributes-mode git-timemachine git-messenger gh-md fuzzy frame-cmds flycheck flx-ido fill-column-indicator fancy-battery expand-region exec-path-from-shell evil-visualstar evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-matchit evil-lisp-state evil-leader evil-indent-textobject evil-iedit-state evil-exchange evil-escape evil-args evil-anzu eval-sexp-fu escreen emacs-eclim elpy elisp-slime-nav el-get dockerfile-mode define-word cython-mode csv-mode company-statistics company-quickhelp company-jedi company-anaconda color-theme-tango clean-aindent-mode buffer-move browse-kill-ring+ autopair auto-yasnippet auto-highlight-symbol auto-dictionary auctex ample-regexps aggressive-indent adaptive-wrap ace-window ace-link ace-flyspell ac-python ac-ispell ac-helm)))
 '(puppet-indent-level tab-width)
 '(ruby-indent-level tab-width)
 '(show-paren-delay 0)
 '(slime-repl-enable-presentations t)
 '(speedbar-frame-parameters
   (quote
    ((minibuffer)
     (width . 40)
     (border-width . 0)
     (menu-bar-lines . 0)
     (tool-bar-lines . 0)
     (unsplittable . t)
     (left-fringe . 0))))
 '(speedbar-show-unknown-files t)
 '(tab-width 4)
 '(visual-line-mode nil t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:foreground "#eee" :background "#011827")) (((class color) (min-colors 256)) (:foreground "#eee" :background "black"))))
 '(company-scrollbar-bg ((t (:background "#200c3d795063"))))
 '(company-scrollbar-fg ((t (:background "#74c0a7b7c87b"))))
 '(company-tooltip ((t (:foreground "#bf17bf17bf17" :background "#009e0ee0182d"))))
 '(company-tooltip-common ((t (:foreground "#00009999834f" :background "#009e0ee0182d"))))
 '(company-tooltip-selection ((t (:foreground "#023658" :background "#c6c6c6"))))
 '(font-lock-comment-face ((t (:foreground "#74c0a7b7c87b"))))
 '(header-line ((t (:background "blue2" :foreground "orange"))))
 '(hl-line ((t (:background "blue4" :foreground nil))))
 '(icicle-complete-input ((t (:foreground "#B19E6A64B19E" :weight bold))))
 '(ido-subdir ((t (:foreground "#A6E22E"))))
 '(one-key-keystroke ((t (:foreground "light green"))))
 '(region ((t (:background "#000000000000" :foreground "#e73fe73fe73f"))))
 '(slime-repl-inputed-output-face ((t (:foreground "CadetBlue1")))))
