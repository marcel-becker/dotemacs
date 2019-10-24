;;http://amitp.blogspot.com/2017/01/emacs-spaceline-mode-line.html

(defun hsl (H S L) ; convenience fn
  (apply 'color-rgb-to-hex (color-hsl-to-rgb (/ H 360.0) S L)))

(defun face (face &rest spec) ; convenience fn
  (face-spec-set face (list (cons t spec))))
(setq powerline-text-scale-factor 0.8)

;;(face 'mode-line :family "M+ 1m" :height 1.0 :background "gray20" :foreground "gray80" :box nil)

(set-face-attribute 'mode-line nil
                    :foreground "Orange" ;;Yellow"
                    :background "Blue2"
                    :inverse-video nil
                    :weight 'bold
                    ;; :height (* (window-font-height) 10)
                    :box '(:line-width 6 :color "orange" :style nil))

(set-face-attribute 'mode-line-inactive nil
                    ;;                     ;;:foreground "Orange" ;;Yellow"
                    ;;                     :background "gray22"
                    ;;                     :inverse-video nil
                    ;;                     ;;:weight 'bold
                    ;;                     ;;:box '(:line-width 6 :color "orange" :style nil)
                    :box nil
                    )


(face 'mode-line-inactive :inherit 'mode-line :background "gray55" :foreground "white" :box nil)
(face 'mode-line-highlight :inherit 'mode-line :background "GoldenRod2" :foreground "white"
      :box '(:line-width -2 :color "GoldenRod2" :style released-button))

(face 'powerline-active1   :inherit 'mode-line          :height powerline-text-scale-factor :background "gray30")
(face 'powerline-inactive1 :inherit 'mode-line-inactive :height powerline-text-scale-factor)
(face 'powerline-active2   :inherit 'mode-line          :height powerline-text-scale-factor :background "gray40")
(face 'powerline-inactive2 :inherit 'mode-line-inactive :height powerline-text-scale-factor)

(face 'spaceline-highlight :inherit 'mode-line :foreground "white" :background "gray80" :height powerline-text-scale-factor)

(face 'amitp/spaceline-personal-active   :inherit 'spaceline-highlight :background (hsl 200 0.5 0.5))
(face 'amitp/spaceline-personal-inactive :inherit 'spaceline-highlight :background (hsl 200 0.2 0.5))
(face 'amitp/spaceline-work-active       :inherit 'spaceline-highlight :background (hsl 0 0.5 0.5))
(face 'amitp/spaceline-work-inactive     :inherit 'spaceline-highlight :background (hsl 0 0.2 0.5))
(face 'amitp/spaceline-other-active      :inherit 'spaceline-highlight :background (hsl 300 0.4 0.5))
(face 'amitp/spaceline-other-inactive    :inherit 'spaceline-highlight :background (hsl 300 0.15 0.5))

(face 'my/spaceline-read-only :background (hsl 300 0.15 0.5) :foreground "gray80" :box `(:line-width -2 :color ,(hsl 300 0.4 0.5)))
(face 'my/spaceline-modified :background "GoldenRod2" :foreground "black")
(face 'my/spaceline-unicode-character :inherit 'mode-line :foreground "black" :background (hsl 50 1.0 0.5))
(face 'amitp/spaceline-filename :family "Helvetica Neue" :foreground nil :background nil :weight 'normal :height (/ 1.0 powerline-text-scale-factor))


(use-package powerline :ensure t)

(defun amitp/spaceline-face (face active)
  "For spaceline-face-func"
  ;; Spaceline will use face1/face2 for the segments, and line for the
  ;; blank space between the left and the right sides. It will use highlight
  ;; when the segment calls for :face highlight-face. I find the default behavior
  ;; weird, as it maps face1/face2 to powerline-{in,}active1 and mode-line, and
  ;; uses powerline-{in,}active2 for the blank space. I'm going to use my own faces
  ;; instead.
  (pcase (cons face active)
    ('(face1 . t)   'powerline-active1)
    ('(face1 . nil) 'powerline-inactive1)
    ('(face2 . t)   'powerline-active2)
    ('(face2 . nil) 'powerline-inactive2)
    ('(line . t)    'mode-line)

    ('(line . nil)  'mode-line-inactive)
    ('(highlight . t)
     (case amitp/buffer-type
       (work     'amitp/spaceline-work-active)
       (personal 'amitp/spaceline-personal-active)
       (t        'amitp/spaceline-other-active)))
    ('(highlight . nil)
     (case amitp/buffer-type
       (work     'amitp/spaceline-work-inactive)
       (personal 'amitp/spaceline-personal-inactive)
       (t        'amitp/spaceline-other-inactive)))
    (_ 'error)))

(setq spaceline-face-func 'amitp/spaceline-face)

(defvar-local amitp/buffer-type 'other "Set to 'personal or 'work or 'other per buffer")

(defun amitp/set-local-colors ()
  "Set amitp/buffer-type and also tabbar color"
  (let ((personal (face-background 'amitp/spaceline-personal-active))
        (work (face-background 'amitp/spaceline-work-active)))
    (cond
     ((s-starts-with? "*" (buffer-name)) (setq amitp/buffer-type 'other))
     ((string-match "redblobgames" (or (buffer-file-name) default-directory))
      (setq amitp/buffer-type 'work)
      (face-remap-add-relative 'tabbar-selected :background work :box nil))
     ((string-match "amitp" (or (buffer-file-name) default-directory))
      (setq amitp/buffer-type 'personal)
      (face-remap-add-relative 'tabbar-selected :background personal :box nil)))))

(cl-loop for buffer in (buffer-list) do
         (with-current-buffer buffer (amitp/set-local-colors)))
(add-hook 'find-file-hook #'amitp/set-local-colors)
(add-hook 'dired-mode-hook #'amitp/set-local-colors)
(add-hook 'change-major-mode-hook #'amitp/set-local-colors)
(add-hook 'temp-buffer-setup-hook #'amitp/set-local-colors)


(defun powerline-evil-tag ()
  "Get customized tag value for current evil state."
  (let* ((visual-block (and (evil-visual-state-p)
                            (eq evil-visual-selection 'block)))
         (visual-line (and (evil-visual-state-p)
                           (eq evil-visual-selection 'line))))
    (cond ((eq powerline-evil-tag-style 'visual-expanded)
           (cond (visual-block " +V+ ")
                 (visual-line " -V- ")
                 (t evil-mode-line-tag)))
          ((eq powerline-evil-tag-style 'verbose)
           (upcase (concat (symbol-name evil-state)
                           (cond (visual-block " BLOCK")
                                 (visual-line " LINE")))))
          (t evil-mode-line-tag))))


(defcustom powerline-evil-tag-style 'visual-expanded
  "The style to use for displaying the evil state tag.
Valid Values: standard, verbose, visual-expanded"
  :group 'powerline
  :type '(choice (const standard)
                 (const verbose)
                 (const visual-expanded)))


;;;###autoload
(defun powerline-evil-face ()
  "Function to select appropriate face based on `evil-state'."
  (let* ((face (intern (concat "powerline-evil-" (symbol-name evil-state) "-face"))))
    (if (facep face) face nil)))

(use-package spaceline-config
  :ensure spaceline
  :config
  (spaceline-helm-mode 1)

  (spaceline-define-segment my/buffer-status
    "Buffer status (read-only, modified), with color"
    (cond (buffer-read-only (propertize "RO" 'face 'my/spaceline-read-only))
          ((buffer-modified-p) (propertize "**" 'face 'my/spaceline-modified))
          (t "  ")))

  (spaceline-define-segment my/evil-state
    "Evil state"
    ;; (let* ((active (powerline-selected-window-active))
    ;;        (mode-line (if active 'mode-line 'mode-line-inactive))
    ;;        (face1 (if active 'powerline-active1 'powerline-inactive1))
    ;;        (face2 (if active 'powerline-active2 'powerline-inactive2))
    ;;        (separator-left (intern (format "powerline-%s-%s"
    ;;                                        (powerline-current-separator)
    ;;                                        (car powerline-default-separator-dir))))
    ;;        (separator-right (intern (format "powerline-%s-%s"
    ;;                                         (powerline-current-separator)
    ;;                                         (cdr powerline-default-separator-dir)))))
    ;;   (if (split-string (format-mode-line minor-mode-alist))
    ;;       (if evil-mode
    ;;         ;; (funcall separator-right face2 evil-face)
    ;;           (propertize (powerline-evil-tag) 'face 'my/spaceline-read-only)
    ;;           ;;    (powerline-raw " " evil-face)
    ;;             ;;  (funcall separator-left evil-face face2)))
    ;;     ;;(list (powerline-minor-modes face2 'l)
    ;;         (powerline-raw " " face2))
    ;;       ;;    (funcall separator-right face2 face1)))
    ;;         (powerline-raw (powerline-evil-tag) (powerline-evil-face))
    ;;         ;;(funcall separator-right evil-face face1)
    ;;         )
    ;;(propertize "NORMAL" 'face 'my/spaceline-read-only)
    ;;(list "NORMAL")
    (powerline-evil-tag)
    )


  (spaceline-define-segment amitp/project-id
    "Name of project, or folder"
    (propertize
     (amitp/shorten-directory
      (cond (buffer-file-name (amitp/project-root-for-file buffer-file-name))
            (t (amitp/project-root-for-directory default-directory)))
      (- (window-width) (length (amitp/spaceline-buffer-id)) 60))
     'face 'amitp/spaceline-filename))

  (spaceline-define-segment amitp/buffer-id
    "Name of filename relative to project, or buffer id"
    (propertize
     (amitp/spaceline-buffer-id)
     'face 'amitp/spaceline-filename))

  (spaceline-define-segment my/unicode-character
    "Description of unicode character we're currently on"
    (let ((ch (following-char)))
      (when (and ch (>= ch 127))
        (get-char-code-property (following-char) 'name))))

  (spaceline-define-segment my/week-number
    "Year and week number, which I use for marking my projects"
    (format-time-string "W%y%V"))


  (setq-default
   powerline-height 30
   powerline-default-separator 'arrow
   spaceline-flycheck-bullet "❖ %s"
   spaceline-separator-dir-left '(right . right)
   spaceline-separator-dir-right '(left . left))
  ;; When there are segments that may or may not appear, they will
  ;; affect the alternating background colors. I try to put the
  ;; indicators that appear/disappear the most towards the center.
  (spaceline-install
    'main
    '(
      (my/buffer-status :tight-left t)
      (buffer-id :face highlight-face)
      ;;(amitp/project-id :tight-right t)
      ;;  (amitp/buffer-id :tight-left t :face highlight-face)
      (process :when active)
      (my/evil-state :tight-left t :face highlight-face)
      )
    '((selection-info :face region :when mark-active)
      (my/unicode-character :face my/spaceline-unicode-character :when active)
      ((flycheck-error flycheck-warning flycheck-info) :when active)
      (which-function)
      (version-control :when active)
      (("L" line column) :separator ":" :when active)
      (my/week-number :when active)
      ;;(global :face highlight-face)
      (major-mode)
      ))
  (spaceline-spacemacs-theme)
  )


(defun amitp/spaceline-buffer-id ()
  (cond (buffer-file-name
         (s-chop-prefix (amitp/project-root-for-file buffer-file-name) buffer-file-name))
        (t (s-trim (powerline-buffer-id 'mode-line-buffer-id)))))
