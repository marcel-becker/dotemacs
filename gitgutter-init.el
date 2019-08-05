(use-package git-link)
(use-package git-messenger
  :config
  (global-set-key (kbd "C-x v p") 'git-messenger:popup-message)

  (define-key git-messenger-map (kbd "m") 'git-messenger:copy-message)

  ;; Use magit-show-commit for showing status/diff commands
  (custom-set-variables
   '(git-messenger:use-magit-popup t)))

(use-package  git-timemachine)

(use-package gitattributes-mode)
(use-package gitconfig-mode)
(use-package gitignore-mode)

(use-package git-gutter+
  :ensure t
  :defer t
  :init (global-git-gutter+-mode)
  :config (progn
            (setq git-gutter+-hide-gutter t)
            (define-key git-gutter+-mode-map (kbd "C-x n") 'git-gutter+-next-hunk)
            (define-key git-gutter+-mode-map (kbd "C-x p") 'git-gutter+-previous-hunk)
            (define-key git-gutter+-mode-map (kbd "C-x v =") 'git-gutter+-show-hunk)
            (define-key git-gutter+-mode-map (kbd "C-x r") 'git-gutter+-revert-hunks)
            (define-key git-gutter+-mode-map (kbd "C-x t") 'git-gutter+-stage-hunks)
            (define-key git-gutter+-mode-map (kbd "C-x c") 'git-gutter+-commit)
            (define-key git-gutter+-mode-map (kbd "C-x C") 'git-gutter+-stage-and-commit)
            (define-key git-gutter+-mode-map (kbd "C-x C-y") 'git-gutter+-stage-and-commit-whole-buffer)
            (define-key git-gutter+-mode-map (kbd "C-x U") 'git-gutter+-unstage-whole-buffer))
  :diminish (git-gutter+-mode . "GG"))
(use-package git-gutter-fringe+)


(use-package magit)
(use-package magit-popup)
