(require 'ranger)

(ranger-override-dired-mode t)
(setq helm-descbinds-window-style 'same-window)
(setq ranger-show-dotfiles t)

;;;Preview Window Options
;Set the default preference to preview selected file.
(setq ranger-preview-file t)
;You can choose to show previews literally, or through find-file, toggled by zi.

(setq ranger-show-literal t)
;You can set the size of the preview windows as a fraction of the frame size.

;(setq ranger-width-preview 0.55)
;You probably don't want to open certain files like videos when previewing. To ignore certain files when moving over them you can customize the following to your liking:

(setq ranger-excluded-extensions '("mkv" "iso" "mp4"))
(setq ranger-dont-show-binary t)


;; Window Decoration and Appearance

;; Ranger by default modifies the header-line in dired to make a more consistent appearance to the real ranger. Setting to nil will disable this feature.

(setq ranger-modify-header t)
;; Define custom function used to output header of primary ranger window. Must return a string that is placed in the header-line.

(setq ranger-header-func 'ranger-header-line)
;Define custom function used to output header of parent and preview windows. Must return a string that is placed in the header-line.

(setq ranger-parent-header-func 'ranger-parent-header-line)
(setq ranger-preview-header-func 'ranger-preview-header-line)
;The cursor can also be hidden for a seamless user experience showing just the cursor line. This feature can be disabled in the cases for themes that do not have a visible cursor line

(setq ranger-hide-cursor nil)
;Delays

;Certain window display options are defined with a delay for a better user experience. The below options can be customized to adjust time to display the preview and the footer information.

;(setq ranger-footer-delay 0.2)
;(setq ranger-preview-delay 0.040)

;Parent Window Options

;You can set the number of folders to nest to the left, adjusted by z- and z+.

(setq ranger-parent-depth 3)
;You can set the size of the parent windows as a fraction of the frame size.

(setq ranger-width-parents 0.12)
;When increasing number of nested parent folders, set max width as fraction of frame size to prevent filling up entire frame with parents.

(setq ranger-max-parent-width 0.12)
