;;http://ergoemacs.org/misc/ergoemacs_vi_mode.html
(use-package xah-fly-keys)

;; make key 【home】 key do activate command mode.
(global-set-key (kbd "<home>") 'xah-fly-command-mode-activate)
;; make key 【end】 to activate insertion mode.
(global-set-key (kbd "<end>") 'xah-fly-insert-mode-activate)


(xah-fly-keys-set-layout "qwerty")
(xah-fly-keys 1)
