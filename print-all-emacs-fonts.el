(defun my-list-all-fonts-2 ()
  (let ((str "The quick brown fox jumps over the lazy dog 1234567890 ´`''\"\"1lI|¦!Ø0Oo{[()]}.,:; ")
        (font-families (cl-remove-duplicates
                        (sort (font-family-list)
                              (lambda(x y) (string< (upcase x) (upcase y))))
                        :test 'string=)))
    (dolist (ff font-families)
      (insert
       (propertize str 'font-lock-face `(:family ,ff))               ff "\n"
       (propertize str 'font-lock-face `(:family ,ff :weight bold))               ff " bold" "\n"
       (propertize str 'font-lock-face `(:family ,ff :slant italic)) ff " italic" "\n"))))

(defun my-list-all-fonts-2 ()
  (dolist (font-family (font-family-list))
    (insert
     (propertize
      (concat "The quick brown fox jumps over the lazy dog 1234567890 (){}[] #$%& (" font-family ")")
      'font-lock-face `((:family ,font-family)))
     "(" font-family ")" "\n")))



;;;;;
;;;;;
;;;;; From: http://ergoemacs.org/emacs/emacs_switching_fonts.html
;;;;; use this to cycle fonts in buffer
(defvar xah-font-list nil "A list of fonts for `xah-cycle-font' to cycle from.")

(setq xah-font-list
      (cond
       ((string-equal system-type "windows-nt")
        '(
          "Courier-10"
          "Lucida Console-10"
          "Segoe UI Symbol-12"
          "Lucida Sans Unicode-10"
          ))
       ((string-equal system-type "gnu/linux")
        '(
          "DejaVu Sans Mono-10"
          "DejaVu Sans-10"
          "Symbola-13"
          ))
       ((string-equal system-type "darwin") ; Mac
        '(
          "DejaVu Sans Mono-16"
          "DejaVu Sans-16"
          "Courier-16"
          "Menlo-16"
          "Meslo LG L DZ for Powerline-16"
          "Meslo LG L for Powerline-16"
          "Meslo LG M DZ for Powerline-16"
          "Meslo LG M for Powerline-16"
          "Meslo LG S DZ for Powerline-16"
          "Source Code Pro-16"
          "Source Code Pro-16:regular"
          "Source Code Pro-16:normal"
          "Source Code Pro-16:medium"
          "Source Code Pro-16:bold"
          ))))

(defun xah-cycle-font (@n)
  "Change font in current frame.
Each time this is called, font cycles thru a predefined list of fonts in the variable `xah-font-list' .
If @n is 1, cycle forward.
If @n is -1, cycle backward.
See also `xah-cycle-font-next', `xah-cycle-font-previous'.

URL `http://ergoemacs.org/emacs/emacs_switching_fonts.html'
Version 2015-09-21"
  (interactive "p")
  ;; this function sets a property “state”. It is a integer. Possible values are any index to the fontList.
  (let ($fontToUse $stateBefore $stateAfter )
    (setq $stateBefore (if (get 'xah-cycle-font 'state) (get 'xah-cycle-font 'state) 0))
    (setq $stateAfter (% (+ $stateBefore (length xah-font-list) @n) (length xah-font-list)))
    (setq $fontToUse (nth $stateAfter xah-font-list))
    (set-frame-font $fontToUse t)
    ;; (set-frame-parameter nil 'font $fontToUse)
    (message "Current font is: %s" $fontToUse )
    (put 'xah-cycle-font 'state $stateAfter)))

(defun xah-cycle-font-next ()
  "Switch to the next font, in current window.
See `xah-cycle-font'."
  (interactive)
  (xah-cycle-font 1))

(defun xah-cycle-font-previous ()
  "Switch to the previous font, in current window.
See `xah-cycle-font'."
  (interactive)
  (xah-cycle-font -1))
