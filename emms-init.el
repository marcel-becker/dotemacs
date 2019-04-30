(use-package emms
  :config
  (progn
    (require 'emms-setup)
    (require 'emms-player-simple)
    (require 'emms-source-file)
    (require 'emms-source-playlist)
    (require 'emms-player-mplayer)
    (setq emms-player-list '(emms-player-mplayer))
    )
  :bind
  (("C-c e SPC" . emms-pause)
   ("C-c e e" . emms-pause)
   ("C-c e +" . emms-seek-forward)
   ("C-c e -" . emms-seek-backward)
   ("C-c e s" . emms-seek)
   ("C-c e [" . my/emms-player-mplayer-slow-down)
   ("C-c e ]" . my/emms-player-mplayer-speed-up)))


(emms-standard)
(emms-all)
(emms-default-players)
(define-emms-simple-player mplayer '(file url)
      (regexp-opt '(".ogg" ".mp3" ".wav" ".mpg" ".mpeg" ".wmv" ".wma"
                    ".mov" ".avi" ".divx" ".ogm" ".asf" ".mkv" "http://" "mms://"
                    ".rm" ".rmvb" ".mp4" ".flac" ".vob" ".m4a" ".flv" ".ogv" ".pls"))
      "mplayer" "-slave" "-quiet" "-really-quiet" "-fullscreen")
