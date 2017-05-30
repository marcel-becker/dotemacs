(defun find-duplicate-lines (&optional insertp interp)
    (interactive "i\np")
    (let ((max-pon (line-number-at-pos (point-max)))
	  (gather-dups))
      (while (< (line-number-at-pos) max-pon) (= (forward-line) 0)
	     (let ((this-line (buffer-substring-no-properties (line-beginning-position 1) (line-end-position 1)))
		   (next-line (buffer-substring-no-properties (line-beginning-position 2) (line-end-position 2))))
	       (when  (equal this-line next-line)  (setq gather-dups (cons this-line gather-dups)))))
      (if (or insertp interp)
	  (save-excursion (new-line) (princ gather-dups (current-buffer)))
      gather-dups)))


(defun uniquify-all-lines-region (start end)
  "Find duplicate lines in region START to END keeping first occurrence."
  (interactive "*r")
  (save-excursion
    (let ((end (copy-marker end)))
        (while
            (progn
              (goto-char start)
              (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
          (replace-match "\\1\n\\2")))))

(defun uniquify-all-lines-buffer ()
  "Delete duplicate lines in buffer and keep first occurrence."
    (interactive "*")
    (uniquify-all-lines-region (point-min) (point-max)))
