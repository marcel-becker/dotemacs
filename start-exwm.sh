#!/bin/sh

# Run the screen compositor
compton &

# Enable screen locking on suspend
# xss-lock -- slock &

# Fire it up
exec dbus-launch emacs --debug-init -mm -l /home/becker/Dropbox/.emacs.d/init-exwm.el
#exec dbus-launch /usr/local/bin/emacs --mm --debug-init \
#     --eval "(add-to-list 'load-path (expand-file-name \"~/Dropbox/.emacs.d/elpa/xelb-0.18\"))" \
#     --eval "(add-to-list 'load-path (expand-file-name \"~/Dropbox/.emacs.d/elpa/exwm-0.26\"))" \
#     --eval "(require 'exwm)" \
#     -f exwm-enable
     #-l /home/becker/.emacs.d/exwm-desktop.el
# exec /usr/local/bin/emacsclient -c
