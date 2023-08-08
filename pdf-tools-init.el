;; Build pdf-tools on the mac:
;;PKG_CONFIG_PATH=/usr/local/opt/libffi/lib/pkgconfig:/usr/local/opt/zlib/lib/pkgconfig:/usr/local/lib/pkgconfig:/opt/X11/lib/pkgconfig ;;/Users/marcelbecker/Dropbox/.emacs.d/elpa/pdf-tools-20190413.2018/build/server/autobuild -i /Users/marcelbecker/Dropbox/.emacs.d/elpa/pdf-tools-20190
(when running-macos
  (use-package pdf-tools
    :defer t
    :init
    (setq pdf-view-use-unicode-ligther nil)
    :config
    ;; initialise
    ;;    (pdf-tools-install t t t)
    (pdf-loader-install)
    ;; open pdfs scaled to fit page
    (setq pdf-view-use-unicode-ligther nil)
    (setq-default pdf-view-display-size 'fit-page)
    ;; more fine-grained zooming
    ;;  (setq pdf-view-resize-factor 1.1)
    (add-hook 'pdf-view-mode-hook (lambda() (linum-mode -1)))
    )
  )
