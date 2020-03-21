(setq tramp-verbose 6)

;;(use-package helm :ensure nil :diminish "H")
(use-package helm :diminish " ")
(display-init-load-time-checkpoint "Loading helm")

(use-package helm-config :ensure helm :demand t :diminish "H")
(display-init-load-time-checkpoint "Done Loading helm config")

(use-package    helm-ag)
(display-init-load-time-checkpoint "Done Loading helm ag")


;(display-init-load-time-checkpoint "Loading helm snippets")
;(use-package    helm-c-yasnippet :after yasnippet)

(use-package    helm-company)
(display-init-load-time-checkpoint "Done Loading helm company")

(use-package    helm-descbinds )
(display-init-load-time-checkpoint "Done Loading helm descbinds")

(use-package    helm-gitignore )
(display-init-load-time-checkpoint "Done Loading helm gitignore")

(use-package    helm-mode-manager )
(display-init-load-time-checkpoint "Done Loading helm mode-manager")

(use-package    helm-projectile )
(display-init-load-time-checkpoint "Done Loading helm projectile")

(use-package    helm-pydoc )
(display-init-load-time-checkpoint "Done Loading helm pydoc")

(use-package    helm-swoop )
(display-init-load-time-checkpoint "Done Loading helm swoop")

(use-package    helm-themes)
(display-init-load-time-checkpoint "Done Loading helm themes")

;;(use-package    helm-ls-git :ensure nil)
(use-package    helm-git-files )
(display-init-load-time-checkpoint "Done Loading helm git-files")

(use-package    helm-helm-commands )
(display-init-load-time-checkpoint "Done Loading helm commands")


(use-package helm-flx
  :config
  (setq helm-flx-for-helm-find-files nil)
  (helm-flx-mode 1))
(display-init-load-time-checkpoint "Done Loading helm flx")


(use-package helm-etags-plus
  :config
  (global-set-key (kbd "M-.") 'helm-etags-plus-select))
(display-init-load-time-checkpoint "Done Loading helm etags")

(use-package helm-bibtex
  :defer t)
(display-init-load-time-checkpoint "Done Loading helm bibtex")


(use-package swiper-helm
  :after helm
  :bind (("C-c C-s" . swiper-helm)))


;;(display-init-load-time-checkpoint "Loading helm spotify")
;;(use-package  helm-spotify-plus :ensure nil)


(autoload 'helm-descbinds      "helm-descbinds" t)
(autoload 'helm-eshell-history "helm-eshell"    t)
(autoload 'helm-esh-pcomplete  "helm-eshell"    t)

(display-init-load-time-checkpoint "Configuring helm")
(setq helm-adaptive-history-file (concat marcel-lisp-dir "helm-history")
      helm-buffers-fuzzy-matching t  ; fuzzy matching buffer names when non-nil useful in helm-mini that lists buffers
      helm-recentf-fuzzy-match t
      helm-M-x-fuzzy-match t
      helm-candidate-number-limit 500 ; limit the number of displayed candidates
      helm-ff-auto-update-initial-value nil
      helm-ff-file-name-history-use-recentf t
      helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.
      helm-move-to-line-cycle-in-source nil ; move to end or beginning of source when reaching top or bottom of source.
      helm-scroll-amount 4 ; scroll 4 lines other window using M-<next>/M-<prior>
      helm-split-window-in-side-p nil ;; open helm buffer inside current window, not occupy whole other window
      helm-yank-symbol-first                 t
      helm-ff-transformer-show-only-basename nil
      helm-mini-default-sources '(helm-source-buffers-list
                                  helm-source-recentf
                                  helm-source-bookmarks
                                  helm-source-buffer-not-found)
      ;; Save buffer when helm-multi-swoop-edit completeo
      helm-multi-swoop-edit-save t
      ;; If this value is t, split window inside the current window
      helm-swoop-split-with-multiple-windows t
      ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
      helm-swoop-split-direction 'split-window-vertically
      ;; If nil, you can slightly boost invoke speed in exchange for text color
      helm-swoop-speed-or-color t
      )

(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

(set-face-attribute 'helm-selection nil :background "purple" :foreground "white" :weight 'bold)
(set-face-attribute 'helm-header nil :background "SkyBlue4" :foreground "wheat1" :weight 'ultra-bold :height 1.2)
(set-face-attribute 'helm-source-header nil :height 1.2)
(set-face-attribute 'helm-match nil :foreground "dark cyan" :weight 'bold)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "A-x") 'helm-M-x)
;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "C-h a") 'helm-apropos)
(global-set-key (kbd "C-h i") 'helm-info-emacs)
(global-set-key (kbd "C-h b") 'helm-descbinds)
(global-set-key (kbd "C-h A") 'helm-ag)


;; use mdfind for mac
(setq helm-locate-command
      (case system-type
        ('gnu/linux "locate -i -r %s")
        ('berkeley-unix "locate -i %s")
        ('windows-nt "es %s")
        ('darwin "mdfind -name %s %s")
        (t "locate %s")))


(global-set-key (kbd "C-x b")   'helm-mini)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-m") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x f")   'helm-for-files)
(global-set-key (kbd "C-x C-r") 'helm-recentf)
(global-set-key (kbd "C-x r l") 'helm-filtered-bookmarks)
(global-set-key (kbd "M-y")     'helm-show-kill-ring)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "M-s o")   'helm-swoop)
(global-set-key (kbd "M-s /")   'helm-multi-swoop)
(global-set-key (kbd "M-s s")   'helm-multi-swoop-all)
(global-set-key (kbd "C-x c!")  'helm-calcul-expression)
(global-set-key (kbd "C-x c:")  'helm-eval-expression-with-eldoc)
(global-set-key (kbd "C-c h C-c w") 'helm-wikipedia-suggest)
(global-set-key (kbd "C-c h x") 'helm-register)
;; (global-set-key (kbd "C-x r j") 'jump-to-register)

(define-key 'help-command (kbd "C-f") 'helm-apropos)
(define-key 'help-command (kbd "r") 'helm-info-emacs)
(define-key 'help-command (kbd "C-l") 'helm-locate-library)



(define-key helm-map (kbd "M-o") #'helm-previous-source)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
(define-key helm-map (kbd "C-p")   'helm-previous-line)
(define-key helm-map (kbd "C-n")   'helm-next-line)
(define-key helm-map (kbd "C-M-n") 'helm-next-source)
(define-key helm-map (kbd "C-M-p") 'helm-previous-source)
(define-key helm-map (kbd "M-N")   'helm-next-source)
(define-key helm-map (kbd "M-P")   'helm-previous-source)
(define-key helm-map (kbd "<S-down>")   'helm-next-source)
(define-key helm-map (kbd "<S-up>")   'helm-previous-source)


(define-key helm-grep-mode-map (kbd "<return>")  'helm-grep-mode-jump-other-window)
(define-key helm-grep-mode-map (kbd "n")  'helm-grep-mode-jump-other-window-forward)
(define-key helm-grep-mode-map (kbd "p")  'helm-grep-mode-jump-other-window-backward)
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map (kbd "TAB")     #'helm-esh-pcomplete)
              (define-key eshell-mode-map (kbd "C-c C-l") #'helm-eshell-history)))


;; show minibuffer history with Helm
(define-key minibuffer-local-map (kbd "M-p") 'helm-minibuffer-history)
(define-key minibuffer-local-map (kbd "M-n") 'helm-minibuffer-history)



;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)

;; From helm-swoop to helm-multi-swoop-all
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)

;; Instead of helm-multi-swoop-all, you can also use helm-multi-swoop-current-mode
(define-key helm-swoop-map (kbd "M-m") 'helm-multi-swoop-current-mode-from-helm-swoop)

;; Move up and down like isearch
(define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
(define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)



(setq helm-projectile-sources-list (cons 'helm-source-projectile-files-list
                                         (remove 'helm-source-projectile-files-list
                                                 helm-projectile-sources-list)))

(define-key projectile-mode-map (kbd "C-c p /")
  #'(lambda ()
      (interactive)
      (helm-ag (projectile-project-root))))



;;(add-to-list 'helm-completing-read-handlers-alist '(find-file))
;;(add-to-list 'helm-completing-read-handlers-alist '(find-file-other-window))
(display-init-load-time-checkpoint "Done Configuring helm")


(display-init-load-time-checkpoint "Loading helm projectile mode")
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(setq projectile-switch-project-action 'helm-projectile)
(display-init-load-time-checkpoint "Done loading helm projectile mode")


(define-key company-mode-map (kbd "C-:") 'helm-company)
(define-key company-active-map (kbd "C-:") 'helm-company)

;; An Emacs-based interface to the package manager of your operating system.
(use-package helm-system-packages
  :config
  (when running-macos
  ;; Unlike the Helm variant, we need to specify our OS pacman.
    (setq system-packages-package-manager 'brew)))


(display-init-load-time-checkpoint "Setting helm mode on")
(helm-mode 1)
(display-init-load-time-checkpoint "Done setting helm mode on")
