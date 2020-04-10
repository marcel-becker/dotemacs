;;brew install mpg321 flac123  libtag mp3info mdp
;; https://computingforgeeks.com/install-configure-mpd-ncmpcpp-macos/
;; $ mkdir -p ~/.mpd/playlists
;; $ touch ~/.mpd/{mpd.conf,mpd.db,mpd.log,mpd.pid,mpdstate}
;; $ cat ~/.mpd/mpd.conf

;; music_directory "~/Music"
;; playlist_directory "~/.mpd/playlists"
;; db_file "~/.mpd/mpd.db"
;; log_file "~/.mpd/mpd.log"
;; pid_file "~/.mpd/mpd.pid"
;; state_file "~/.mpd/mpdstate"
;; auto_update "yes"
;; auto_update_depth "2"
;; follow_outside_symlinks "yes"
;; follow_inside_symlinks "yes"

;; audio_output {
;;  type "osx"
;;  name "CoreAudio"
;;  mixer_type "software"
;; }

;; decoder {
;;  plugin "mp4ff"
;;  enabled "no"
;; }

;; bind_to_address "127.0.0.1"
;; port "6600"
;; user "jmutai"

;; # Visualizer
;; audio_output {
;;  type "fifo"
;;  name "my_fifo"
;;  path "/tmp/mpd.fifo"
;;  format "44100:16:2"
;;  auto_resample "no"
;;  use_mmap "yes"
;; }


(use-package emms
  :config
  (require 'emms-setup)
  (emms-all)
  (require 'emms-player-simple)
  (require 'emms-source-file)
  (require 'emms-source-playlist)
  ;;    (require 'emms-player-mplayer)
  (require 'emms-player-vlc)
  (require 'emms-playlist-sort)
  ;; Use only libtag for tagging.
  (require 'emms-info-libtag)
  (require 'emms-streams)
  (require 'emms-stream-info)
  (require 'emms-playing-time)
  (emms-default-players)
  (setq emms-playlist-buffer-name "*Music*")
  (setq emms-info-libtag-program-name (expand-file-name  "bin/emms-print-metadata" marcel-lisp-dir))
  (if (executable-find emms-info-libtag-program-name)
      (setq emms-info-functions '(emms-info-libtag)))
  ;;(setq emms-info-functions '(emms-info-mp3info))

  (setq emms-source-file-default-directory "/Volumes/MusicFiles/Music/")
  (setq emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find)
  (setq emms-browser-covers 'emms-browser-cache-thumbnail)
  (setq emms-stream-default-action "play")
  ;; «enable-emms-scoring» (to ".enable-emms-scoring")
  (setq emms-score-enabled-p t)
  ;; «Start-browser-with-album» (to ".Start-browser-with-album")
  (setq emms-browser-default-browse-type 'info-album)
  (setq emms-browser-default-covers (list (expand-file-name "emms/cover_small.jpg" marcel-lisp-dir)))
  (emms-history-load)
  (emms-mode-line 1)
  (emms-playing-time 1)
;;  (define-key emms-stream-mode-map (kbd "s") 'emms-stop)

  :bind
  (("C-c +" . emms-volume-mode-plus)
   ("C-c -" . emms-volume-mode-minus)
   ("C-c e e" . emms)
   ("C-c e SPC" . emms-pause)
   ("C-c e p" . emms-pause)
   ("C-c e +" . emms-seek-forward)
   ("C-c e -" . emms-seek-backward)
   ("C-c e s" . emms-seek)
   ("C-c e b" . emms-smart-browse)
   ("C-c e r" . emms-player-mpd-update-all-reset-cache)
   ("C-c e m" . my-emms-mpd-set-player)
   ("C-c e v" . my-emms-vlc-set-player)
   ("C-c e x" . my-emms-mpv-set-player)
   ("C-c e y" . my-emms-mplayer-set-player)
   ("C-c e n" . my-emms-mpd-kill-music-daemon)
   ("C-c e u" . my-emms-mpd-update-database)
   ("C-c e <up>" . emms-start)
   ("C-c e <down>" . emms-stop)
   ("C-c e <left>" . emms-previous)
   ("C-c e <right>" . emms-next)
   ;;("<XF86AudioPlay>" . emms-pause)
   ;;("<XF86AudioStop>" . emms-stop)
   ;;("<XF86AudioPrev>" . emms-previous)
   ;;("<XF86AudioNext>" . emms-next)
   ("C-c e [" . my/emms-player-mplayer-slow-down)
   ("C-c e ]" . my/emms-player-mplayer-speed-up)))


(defun my-emms-add-all-players ()
  (interactive)
  (setq emms-player-list '(emms-player-vlc-playlist
                           emms-player-vlc
                           emms-player-mpg321
                           emms-player-ogg123
                           emms-player-mplayer
                           emms-player-mpv
                           emms-player-mpd
                           ))
  )


(define-prefix-command 'my-emms-map)
(bind-keys :map global-map
           :prefix-map my-emms-map
           :prefix "<f9>"
           ("r" . emms-streams)
           ("+" . emms-volume-raise)
           ("-" . emms-volume-lower)
           ("b" . emms-smart-browse)
           ("t" . emms-player-mpd-show)
           ("s" . emms-stop)
           ("RET" . emms-start)
           ("c" . emms-browser-clear-playist)
           ("p" . emms-pause)
           (">" . emms-next)
           ("<" . emms-previous)
           ("<right>" . emms-next)
           ("<left>" . emms-previous)
           ("<up>" . emms-pause)
           ("m" . emms-mode-line-toggle))





(defun my-emms-vlc-set-player ()
  (interactive)
  (require 'emms-player-vlc)
  (define-emms-simple-player vlc '(file url)
    (concat "\\`\\(http[s]?\\|mms\\)://\\|"
            (apply #'emms-player-simple-regexp
                   emms-player-base-format-list))
    "vlc" "--extraintf=rc")
  (define-emms-simple-player vlc-playlist '(streamlist)
    "\\`http[s]?://"
    "vlc" "--extraintf=rc")
  (setq emms-player-list '(emms-player-vlc emms-player-vlc-playlist))
  (setq emms-player-vlc-command-name
        "/Applications/VLC.app/Contents/MacOS/VLC")
  (setq emms-player-vlc-parameters '("--extraintf=rc"))
  (setq emms-stream-info-backend 'vlc)
  )


(defun my-emms-mplayer-set-player ()
  (interactive)
  (require 'emms-player-mplayer)
  (define-emms-simple-player mplayer '(file url)
    (regexp-opt '(".ogg" ".mp3" ".wav" ".mpg" ".mpeg" ".wmv" ".wma"
                  ".mov" ".avi" ".divx" ".ogm" ".asf" ".mkv" "http://" "mms://"
                  ".rm" ".rmvb" ".mp4" ".flac" ".vob" ".m4a" ".flv" ".ogv" ".pls"))
    "mplayer" "-slave" "-quiet" "-really-quiet" "-fullscreen")
  (setq emms-player-list '(emms-player-mplayer))
  )


(defun my-emms-mpv-set-player ()
  (interactive)
  (require 'emms-player-mpv)
  ;; (define-emms-simple-player mpv '(file url streamlist playlist)
  ;;   (concat "\\`\\(http\\|mms\\)://\\|"
  ;;           (emms-player-simple-regexp
  ;;            "ogg" "mp3" "wav" "mpg" "mpeg" "wmv" "wma"
  ;;            "mov" "avi" "divx" "ogm" "ogv" "asf" "mkv"
  ;;            "rm" "rmvb" "mp4" "flac" "vob" "m4a" "ape"
  ;;            "flv" "webm" "aac"))
  ;;   "mpv" "--quiet" "--really-quiet --no-audio-display")
  (setq emms-player-list '(emms-player-mpv))
  (add-to-list 'emms-player-mpv-parameters "--no-audio-display")
  (add-to-list 'emms-info-functions 'emms-info-cueinfo)
  (add-to-list 'emms-info-functions 'emms-info-ogginfo)
  (add-to-list 'emms-info-functions 'emms-info-mp3info)

  (defun ambrevar/emms-play-on-add (old-pos)
    "Play tracks when calling `emms-browser-add-tracks' if nothing
is currently playing."
    (interactive)
    (when (or (not emms-player-playing-p)
              emms-player-paused-p
              emms-player-stopped-p)
      (with-current-emms-playlist
        (goto-char old-pos)
        ;; if we're sitting on a group name, move forward
        (unless (emms-playlist-track-at (point))
          (emms-playlist-next))
        (emms-playlist-select (point)))
      (emms-stop)
      (emms-start)))
  (add-hook 'emms-browser-tracks-added-hook 'ambrevar/emms-play-on-add)


  (defun emms-player-mpv-volume-change (v)
    "Change the volume by V for mac."
    (let* ((cmd (format "%d" (* 10 v))))
      (emms-player-mpv-cmd `(add volume ,cmd))
      (emms-player-mpv-volume-query)))


  (when running-macos
    (custom-set-variables
     '(emms-volume-change-function #'emms-player-mpv-volume-change)))


  (defun emms-player-mpv-volume-set (v)
    "Set the volume to V for mac."
    (interactive)
    (emms-player-mpv-cmd `(set_property volume ,v))
    (emms-player-mpv-volume-query))


  (defun emms-player-mpv-volume-mute-unmute ()
    "Toggle mute status for mac."
    (interactive)
    (emms-player-mpv-cmd  '(cycle mute))
    (emms-player-mpv-ipc-req-send  '(get_property mute)
                                   (lambda (pts-end err)
                                     (message "Pts End %s Err =  %s" pts-end err)
                                     (if err
                                         (unless (and (stringp err)
                                                      (string= err "property unavailable"))
                                           (message "Calling ems-player-mpv-ipc-req-error printer")
                                           (emms-player-mpv-ipc-req-error-printer pts-end err))
                                       (when pts-end
                                         (message "Mute: %s" pts-end))))))


  (defun emms-player-mpv-volume-query ()
    "Show the volume settings for mac"
    (interactive)
    (emms-player-mpv-ipc-req-send  '(get_property volume)
                                   (lambda (pts-end err)
                                     (if err
                                         (unless (and (stringp err)
                                                      (string= err "property unavailable"))
                                           (message "Calling ems-player-mpv-ipc-req-error printer")
                                           (emms-player-mpv-ipc-req-error-printer pts-end err))
                                       (when pts-end
                                         (message "Volume %s" pts-end))))))

  )



(defun my-emms-mpd-set-player ()
  (interactive)
  (require 'emms-player-mpd)
  (setq emms-seek-seconds 5
        emms-player-list '(emms-player-mpd)
        emms-info-functions '(emms-info-mpd)
        emms-volume-change-function 'emms-volume-mpd-change
        emms-player-mpd-music-directory emms-source-file-default-directory
        ) ;FIXME: use mpd.conf
  ;; set mpd address and port
  (setq emms-player-mpd-server-name "localhost")
  (setq emms-player-mpd-server-port "6600")
  (my-emms-mpd-start-music-daemon)
  )


;; https://github.com/daedreth/UncleDavesEmacs#emms-with-mpd
(defun my-emms-mpd-start-music-daemon ()
  "Start MPD, connects to it and syncs the metadata cache."
  (interactive)
  (shell-command "mpd")
  (my-emms-mpd-update-database)
  (emms-player-mpd-connect)
  (emms-cache-set-from-mpd-all)
  (message "MPD Started!"))
;;(global-set-key (kbd "s-m c") 'mpd/start-music-daemon)

;; https://github.com/daedreth/UncleDavesEmacs#emms-with-mpd
(defun my-emms-mpd-kill-music-daemon ()
  "Stops playback and kill the music daemon."
  (interactive)
  (emms-stop)
  (call-process "killall" nil nil nil "mpd")
  (message "MPD Killed!"))
;;(global-set-key (kbd "s-m k") 'mpd/kill-music-daemon)

;; https://github.com/daedreth/UncleDavesEmacs#emms-with-mpd
(defun my-emms-mpd-update-database ()
  "Updates the MPD database synchronously."
  (interactive)
  (call-process "mpc" nil nil nil "update")
  (message "MPD Database Updated!"))
;;(global-set-key (kbd "s-m u") 'mpd/update-database)

;;(emms-player-mpd-connect)
;; (emms-player-mpd-update-all)
;;(emms-cache-set-from-mpd-all)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Settings for MPV
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package emms-player-simple-mpv
;;   :config
;; ;; This plugin provides control functions (e.g. ab-loop, speed, fullscreen).
;; (require 'emms-player-simple-mpv-control-functions)
;; )

;; (use-package emms-player-simple-mpv
;;   :config
;; ;; This plugin provides control functions (e.g. ab-loop, speed, fullscreen).
;; (require 'emms-player-simple-mpv-control-functions)
;; )

;; (defun emms-player-mpv-volume-change (v)
;;   "Change the volume by V for mpv."
;;   (let ((cmd (format "echo 'add volume %d' > %s" v emms-mpv-input-file)))
;;     (call-process-shell-command cmd nil nil nil)))

;; (defun emms-player-mpv-volume-set (v)
;;   "Set the volume to V for mpv."
;;   (interactive "nmpv volume : ")
;;   (let ((cmd (format "echo 'set volume %d' > %s" v emms-mpv-input-file)))
;;     (call-process-shell-command cmd nil nil nil)))

;; (defun emms-player-mpv-volume-mute ()
;;   "Toggle mute status for mpv."
;;   (interactive)
;;   (let ((cmd (format "echo 'cycle mute' > %s" emms-mpv-input-file)))
;;     (call-process-shell-command cmd nil nil nil)))


;; (custom-set-variables
;;   '(emms-volume-change-function #'emms-player-mpv-volume-change))


;; (define-emms-simple-player-mpv my-mpv '(file url streamlist playlist)
;;     (concat "\\`\\(http[s]?\\|mms\\)://\\|"
;;             (apply #'emms-player-simple-regexp
;;                    "aac" "pls" "m3u"
;;                    emms-player-base-format-list))
;;     "mpv" "--no-terminal" "--force-window=no" "--audio-display=no")

;; (emms-player-simple-mpv-add-to-converters
;;  'emms-player-my-mpv "." '(playlist)
;;  (lambda (track-name) (format "--playlist=%s" track-name)))

;; (setq  emms-player-list '(emms-player-my-mpv))


(defun emms-player-mac-volume-change (v)
  "Change the volume by V for mac."
  (let* (
         (volume (call-process-shell-command "osascript -e 'output volume of (get volume settings)'" nil nil nil))
         (new-volume (max 0 (+ volume v)))
         (cmd (format "osascript -e 'set volume output volume (output volume of (get volume settings) + %d)'" (* v 10))))
    (message (format "%s" cmd))
    (shell-command cmd )))


(when running-macos
  (custom-set-variables
   '(emms-volume-change-function #'emms-player-mac-volume-change)))


(defun emms-player-mac-volume-set (v)
  "Set the volume to V for mac."
  (interactive "mac volume (from 0 to 10): ")
  (let ((cmd (format "osascript -e 'set volume %d'" v)))
    (call-process-shell-command cmd nil nil nil)))


(defun emms-player-mac-volume-mute-unmute ()
  "Toggle mute status for mac."
  (interactive)
  (let ((cmd  "osascript -e 'set volume output muted not (output muted of (get volume settings))'"))
    (call-process-shell-command cmd nil nil nil)))

;; change the volume by 1%
;; osascript -e "set volume output volume (output volume of (get volume settings) + 1) --100%"
(defun emms-player-mac-volume-query ()
  "Show the volume settings for mac"
  (interactive)
  (message (format "%s" (shell-command-to-string "osascript -e '(get volume settings)'" ))))

(use-package helm-emms
  :defer t
  :after (helm emms))



(defun chunyang-emms-indicate-seek (_sec)
  (let* ((total-playing-time (emms-track-get
                              (emms-playlist-current-selected-track)
                              'info-playing-time))
         (elapsed/total (/ (* 100 emms-playing-time) total-playing-time)))
    (with-temp-message (format "[%-100s] %2d%%"
                               (make-string elapsed/total ?=)
                               elapsed/total)
      (sit-for 2))))

(add-hook 'emms-player-seeked-functions #'chunyang-emms-indicate-seek 'append)



;; «Update-mpd-directory» (to ".Update-mpd-directory")

(defun my-emms-update-and-clean-cache ()
  (interactive)
  (when emms-cache-db
    (clrhash emms-cache-db)
    (and (file-exists-p emms-cache-file)
         (delete-file emms-cache-file))
    (and (file-exists-p emms-history-file)
         (delete-file emms-history-file))
    (with-current-buffer (find-file-noselect emms-cache-file)
      (save-buffer))
    (emms-add-directory-tree "~/Music/")))


(defun my-emms-track-simple-description (track)
  "Simple function to give a user-readable description of a track.
If it's a file track, just return the file name.  Otherwise,
return the type and the name with a colon in between.
Hex-encoded characters in URLs are replaced by the decoded
character."
  (let ((type (emms-track-type track)))
    (cond ((eq 'file type)
           (file-name-sans-extension
            (file-name-nondirectory (emms-track-name track))))
          ((eq 'url type)
           (emms-format-url-track-name (emms-track-name track)))
          (t (concat (symbol-name type)
                     ": " (emms-track-name track))))))
(setq emms-track-description-function 'my-emms-track-simple-description)

;; (defun emms-player-vlc-volume-query ()
;;   "Show the volume settings for vlc"
;;   (interactive)
;;   (process-send-string emms-player-simple-process-name "volume 100\n")
;;   (message (format "%s" (shell-command-to-string "osascript -e '(get volume settings)'" ))))



;; (defun emms-player-vlc-pause ()
;;   "Depends on vlc's rc mode."
;;   (interactive)
;;   (process-send-string
;;    emms-player-simple-process-name "pause\n"))

;; (defun emms-player-vlc-seek (sec)
;;   "Seek relative within a stream."
;;   (when (not (= 0 sec))
;;     (process-send-string
;;      emms-player-simple-process-name
;;      (if (< 0 sec) "fastforward\n" "rewind\n"))))

;; (defun emms-player-vlc-seek-to (sec)
;;   "Seek to time SEC within the stream."
;;   (process-send-string
;;    emms-player-simple-process-name
;;    (format "seek %d\n" sec)))
