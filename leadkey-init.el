;;; -*- lexical-binding: t -*-

(use-package leader-key
:config
(global-set-key (kbd "C-M-SPC") nil)       ;; so it doesn't conflict
(setq leader-key-root-key "C-M-SPC")
(setq leader-key-pred #'(lambda () t))   ;; always active
(leader-key-mode t)
(with-eval-after-load 'projectile
  (leader-key-define-key "p" projectile-command-map "projectile"))
(with-eval-after-load 'flycheck
  (leader-key-define-key "f" flycheck-command-map "flycheck"))

(leader-key-define-key "m" #'magit-status)
(leader-key-define-key "g" #'magit-file-dispatch)

(leader-key-define-key "<left>" #'window-jump-left)
(leader-key-define-key "<right>" #'window-jump-right)
(leader-key-define-key "<up>" #'window-jump-up)
(leader-key-define-key "<down>" #'window-jump-down)
(leader-key-define-key "0" #'+switch-window-then-delete-window-and-balance "delete-window")
(leader-key-define-key "1" #'switch-window-then-maximize "maximize")
(leader-key-define-key "2" #'+switch-window-then-split-below-switch-and-balance "split-below")
(leader-key-define-key "3" #'+switch-window-then-split-right-switch-and-balance "split-right")

(leader-key-define-key "SPC" #'set-mark-command)

(leader-key-describe-key "x" "emacs")       ;; optional, for which-key
(leader-key-define-key "x SPC" #'rectangle-mark-mode)
(leader-key-define-key "x ;" #'eval-expression)
(leader-key-define-key "x e" #'eval-last-sexp)
(leader-key-define-key "x k" #'kill-some-buffers)
(leader-key-define-key "x p" #'paradox-list-packages)
(leader-key-define-key "x q" #'save-buffers-kill-terminal)
(leader-key-define-key "x s" #'save-some-buffers)
(leader-key-define-key "x w" #'write-file)
(leader-key-define-key "x x" #'execute-extended-command)
)
