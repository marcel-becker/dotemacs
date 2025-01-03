;;; -*- lexical-binding: t -*-


(setq evil-want-integration t)
(setq evil-want-keybinding nil)
(use-package  evil-collection
  :after evil
  :config
  (evil-collection-init))


(use-package  evil
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq display-line-numbers 'relative)
  :config
  (evil-mode 1))

(evil-set-leader 'normal (kbd "SPC"))
(evil-define-key 'normal 'global
  (kbd "<leader>x") 'execute-extended-command)

(use-package  evil-anzu)
(use-package  evil-args)
(use-package  evil-ediff)
(use-package  evil-escape)
(use-package  evil-exchange)
(use-package  evil-iedit-state)
(use-package  evil-indent-plus)
(use-package  evil-indent-textobject)
(use-package  evil-lisp-state)
(use-package  evil-magit)
(use-package  evil-matchit)
(use-package  evil-mc)
(use-package  evil-nerd-commenter)
(use-package  evil-numbers)
(use-package  evil-search-highlight-persist)
(use-package  evil-surround
  :config
  (global-evil-leader-mode))

(use-package  evil-tutor)
;; (use-package  evil-unimpaired)
(use-package  evil-visual-mark-mode)
(use-package  evil-visualstar)


(use-package evil-goggles
  :config
  (evil-goggles-mode)

  ;; optionally use diff-mode's faces; as a result, deleted text
  ;; will be highlighed with `diff-removed` face which is typically
  ;; some red color (as defined by the color theme)
  ;; other faces such as `diff-added` will be used for other actions
  (evil-goggles-use-diff-faces))

(setq evil-normal-state-cursor '(box "red")
      evil-insert-state-cursor '(box "green")
      evil-visual-state-cursor '(box "magenta")
      evil-emacs-state-cursor  '(box "white")
      )


(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(define-key evil-normal-state-map [home] 'evil-insert-state)
(define-key evil-insert-state-map [home] 'evil-normal-state)
(define-key evil-visual-state-map [home] 'evil-normal-state)
(define-key evil-replace-state-map [home] 'evil-normal-state)

(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

 ;;; esc quits

(define-key evil-normal-state-map "\C-y" 'yank)
(define-key evil-insert-state-map "\C-y" 'yank)
(define-key evil-visual-state-map "\C-y" 'yank)
(define-key evil-insert-state-map "\C-e" 'end-of-line)
(define-key evil-normal-state-map "\C-e" 'end-of-line)
(define-key evil-normal-state-map "\C-w" 'evil-delete)
(define-key evil-insert-state-map "\C-w" 'evil-delete)
(define-key evil-insert-state-map "\C-r" 'search-backward)
(define-key evil-visual-state-map "\C-w" 'evil-delete)

(define-key evil-insert-state-map (kbd "A-a") 'beginning-of-visual-line)

(define-key evil-normal-state-map (kbd "+") 'er/expand-region)
(define-key evil-visual-state-map (kbd "x") 'er/expand-region)
(define-key evil-visual-state-map (kbd "X") 'er/contract-region)
(define-key evil-visual-state-map (kbd "+") 'er/expand-region)
(define-key evil-visual-state-map (kbd "_") 'er/contract-region)


(setq evil-normal-state-tag "NORMAL")
(setq evil-insert-state-tag "INSERT")
(setq evil-visual-state-tag "VISUAL")
(setq evil-emacs-state-tag "EMACS")
(setq evil-replace-state-tag "REPLACE")

;;  (use-package spaceline :ensure nil
;;   :config
;;  ;; (spaceline-spacemacs-theme)
;;   (spaceline-emacs-theme)
;; )

(use-package powerline)

;;(powerline-default-theme)
;;(powerline-center-evil-theme)

;; (use-package powerline-evil
;;     :ensure nil
;;     :config
;;     (powerline-evil-center-color-theme))


(force-mode-line-update t)

(setq evil-disable-insert-state-bindings t)

(setq original-background (face-attribute 'mode-line :background))
(setq original-foreground (face-attribute 'mode-line :foreground))

(setq normal-state-background "DarkBlue")
(setq normal-state-foreground "white")


(setq insert-state-background "blue")
(setq insert-state-foreground "white")


(setq visual-state-background "light sea green")
(setq emacs-state-background "violet")
(setq replace-state-background "black")

(defun my-add-evil-mode-line-change-hook()
  (interactive)
  (add-hook 'evil-normal-state-entry-hook
            (lambda ()
              (set-face-attribute 'mode-line nil :background normal-state-background)
              (set-face-attribute 'mode-line nil :foreground normal-state-foreground)))

  (add-hook 'evil-normal-state-exit-hook
            (lambda ()
              (set-face-attribute 'mode-line nil :background original-background)
              (set-face-attribute 'mode-line nil :foreground original-foreground)
              ))

  (add-hook 'evil-insert-state-entry-hook
            (lambda ()
              (set-face-attribute 'mode-line nil :background insert-state-background)
              (set-face-attribute 'mode-line nil :foreground insert-state-foreground)
              ))
  (add-hook 'evil-insert-state-exit-hook
            (lambda ()
              (set-face-attribute 'mode-line nil :background original-background)
              (set-face-attribute 'mode-line nil :foreground original-foreground)
              ))

  (add-hook 'evil-visual-state-entry-hook
            (lambda ()
              (set-face-attribute 'mode-line nil :background visual-state-background)))
  (add-hook 'evil-visual-state-exit-hook
            (lambda ()
              (set-face-attribute 'mode-line nil :background original-background)))

  (add-hook 'evil-emacs-state-entry-hook
            (lambda ()
              (set-face-attribute 'mode-line nil :background emacs-state-background)))
  (add-hook 'evil-emacs-state-exit-hook
            (lambda ()
              (set-face-attribute 'mode-line nil :background original-background)))

  (add-hook 'evil-replace-state-entry-hook
            (lambda ()
              (set-face-attribute 'mode-line nil :background replace-state-background)
              (set-face-attribute 'mode-line nil :foreground "white")
              ))
  (add-hook 'evil-replace-state-exit-hook
            (lambda ()
              (set-face-attribute 'mode-line nil :background original-background))))


(use-package  evil-leader
  :after evil
  :config
  ;;(evil-set-leader 'normal (kbd "SPC"))
  (evil-set-leader nil (kbd "SPC"))
  (global-evil-leader-mode))


;; Make a visual selection with `v` or `V`, and then hit `*` to search
;; the selection forward, or # to search that selection backward.
;; If the evil-visualstar/persistent option is not nil, visual-state
;; will remain in effect, allowing for repeated * or #.
(global-evil-visualstar-mode t)
