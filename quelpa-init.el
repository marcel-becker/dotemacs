(display-init-load-time-checkpoint "Loading diff+")
(use-package diff+
  :quelpa (diff+ :fetcher github :repo "emacsmirror/diff-plus"))


(display-init-load-time-checkpoint "Loading faces+")
(use-package faces+
  :quelpa (faces+  :fetcher github :repo "emacsmirror/faces-plus"))

(display-init-load-time-checkpoint "Loading frame-fns")
(use-package frame-fns
  :quelpa (frame-fns :fetcher github :repo "emacsmirror/frame-fns"))
(use-package frame-cmds
  :quelpa (frame-cmds :fetcher github :repo "emacsmirror/frame-cmds"))


(display-init-load-time-checkpoint "Loading doremi")
(use-package doremi
  :quelpa (doremi :fetcher github :repo "emacsmirror/doremi"))
(use-package doremi-cmd
  :quelpa (doremi-cmd :fetcher github :repo "emacsmirror/doremi-cmd"))
;;(use-package doremi-frm
;;:quelpa (doremi-frm :fetcher wiki))
(use-package doremi-mac
  :quelpa (doremi-mac :fetcher github :repo "emacsmirror/doremi-mac"))

(display-init-load-time-checkpoint "Loading menu-bar+")
(use-package menu-bar+
  :quelpa (menu-bar+ :fetcher github :repo "emacsmirror/menu-bar-plus"))
