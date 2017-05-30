;; Set of functions to set the environment and load specware files. 



;; Needed to run Specware on Emacs24
(setenv "SPECWARE_INIT_FORM" "NIL")

(setenv "SPECWARE4"
        (if running-ms-windows
            "c:/src/Specware"
          (expand-file-name "~/src/specware")))


(setenv "PLANWARE"
        (if running-ms-windows
            "c:/src/planware"
          (expand-file-name "~/src/planware")))


(setenv "LISP_EXECUTABLE"
        (if running-ms-windows
            "C:/src/Specware/Applications/Specware/bin/windows/Specware4.exe"))

(setenv "LISP_HEAP_IMAGE"
        (if running-ms-windows
            "NONE"))

(setenv "LISP_DIRECTORY"
        (if running-ms-windows
            "C:/src/Specware/"
        (expand-file-name "~/src/specware")))

(setenv "SPECWARE_BIN"
        (if running-ms-windows
            "C:/src/Specware/Applications/Specware/bin/windows"
          (expand-file-name "~/src/specware/Applications/Specware/bin/windows")
          ))

(setenv "SPECWARE_EXECUTABLE"
        (if running-ms-windows
            "C:/src/Specware/Applications/Specware/bin/windows/Specware4.exe"))

(setenv "SWPATH"
        (cond
         (running-ms-windows
            "C:/;c:/src/planware/;c:/src/specware/")
         (running-macos
          "/:/Users/marcelbecker/src/planware:/Users/marcelbecker/src/specware/")
         (t
          "/:/home/becker/src/planware:/home/becker/src/specware/")))

(defun my-load-specware ()
  (interactive)
  (let ((specware-slime    
	 (if running-ms-windows
	     "C:/src/Specware/Library/IO/Emacs/load-slime.el"
	   (expand-file-name "~/src/specware/Library/IO/Emacs/load-slime.el"))))
    (when (file-exists-p specware-slime)
      (load specware-slime)
      ;(require 'slime)
      ;(slime-setup '(slime-fancy slime-repl slime-autodoc  slime-typeout-frame))
      )))


(defun my-load-planware ()
  (interactive)
  (let ((planware-init    
	 (if running-ms-windows
	     "C:/src/planware/Sources/UI/Emacs/planware.el"
	   (expand-file-name "~/src/planware/Sources/UI/Emacs/planware.el"))))
    (when (file-exists-p planware-init)
      (load planware-init)
      )))

(setq *sbcl-size* 
      (cond
       (running-ms-windows
        16000)
       (running-macos
        2000)
       (t
        16000)))

      
;;(my-load-specware)
;;(my-load-planware)

(setq sw:definition-introducing-words
  (regexp-opt '("axiom"
		"conjecture"
		"def"
		"op"
		"theorem"
		"type"
                "mode-machine"
                "mode"
                "transition"
                )))

;(defvar sw:basic-unit-intro-regexp "^\\(\\sw+\\)\\s-*=\\s-*")

