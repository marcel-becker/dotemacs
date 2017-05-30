(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(setq ido-file-extensions-order '(".org" ".txt" ".py" ".emacs" ".xml" ".el" ".ini" ".cfg" ".cnf"))

(defun my-ido-choose-from-recentf ()
  "Use ido to select a recently opened file from the `recentf-list'"
  (interactive)
  (let ((home (expand-file-name (getenv "HOME"))))
    (find-file
     (ido-completing-read "Recentf open: "
                          (mapcar (lambda (path)
                                    (replace-regexp-in-string home "~" path))
                                  recentf-list)
                          nil t))))


(setq ido-save-directory-list-file (concat marcel-lisp-dir "/ido.last"))
(setq ido-use-filename-at-point 'guess)
(setq ido-show-dot-for-dired t)
(setq ido-default-buffer-method 'selected-window)
                                        ;
;; default key to switch buffer is C-x b, but that's not easy enough
;;
;; when you do that, to kill emacs either close its frame from the window
;; manager or do M-x kill-emacs.  Don't need a nice shortcut for a once a
;; week (or day) action.
(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
;;(global-set-key (kbd "C-x C-c") 'ido-switch-buffer)
(global-set-key (kbd "C-x B") 'ibuffer)


;; Display ido results vertically, rather than horizontally
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
;;(defun ido-disable-line-truncation ()
;;(set (make-local-variable 'truncate-lines) nil))
;;(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
;;(defun ido-define-keys () ;; C-n/p is more intuitive in vertical layout
;;  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
;;  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
;;(add-hook 'ido-setup-hook 'ido-define-keys)


(global-set-key [(C f11)] 'my-ido-choose-from-recentf)
