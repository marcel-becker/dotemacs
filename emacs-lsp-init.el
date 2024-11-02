(require 'cc-mode)
(require 'compile)

(use-package lsp-mode
  ;;  :ensure t
  :bind
  (:map lsp-mode-map
        (("C-M-b" . lsp-find-implementation)
         ("M-RET" . lsp-execute-code-action)))

  :custom
  (lsp-diagnostics-provider :capf)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-headerline-breadcrumb-segments '(project file symbols))
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-file-watch-threshold 2500)
  (setq lsp-inhibit-message nil ; you may set this to t to hide messages from message area
        lsp-eldoc-render-all nil
        lsp-highlight-symbol-at-point nil)
  (setq lsp-prefer-flymake nil)
  (setq flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  (setq lsp-log-io t)
  (setq lsp-print-performance t)
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 10 1024 1024))
  :hook ((python-mode . lsp)
         (java-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration)
         )
  :config

  ;;  (lsp-define-stdio-client lsp-python "python"
  ;;                         #'projectile-project-root
  ;;                       '("pyls"))
  ;;  (add-hook 'python-mode-hook
  ;;          (lambda ()
  ;;          (lsp-python-enable)))
  (lsp-enable-which-key-integration t)
  (setq
   python-indent-offset 4
   python-shell-interpreter "jupyter"
   python-shell-interpreter-args "console --simple-prompt"
   python-shell-prompt-detect-failure-warning nil)
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "jupyter")
  )

(use-package company-lsp
  ;;  :ensure t
  :after  company
  :config
  (push 'company-lsp company-backends)
  (setq company-lsp-enable-snippet t
        company-lsp-cache-candidates t)
  )

(use-package lsp-ui
  ;;  :ensure t
  ;;  :commands lsp-ui-mode
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (require 'lsp-ui-imenu)
  (require 'lsp-ui-sideline)
  (require 'lsp-ui-doc)
  (require 'lsp-ui-flycheck)
  (require 'lsp-ui-peek)
  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)
  ;;  (add-hook 'lsp-after-open-hook (lambda () (lsp-ui-flycheck-enable 1)))
  (setq lsp-ui-sideline-enable t
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-symbol t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-ignore-duplicate t
        ;; lsp-ui-sideline-update-mode 'point
        lsp-ui-peek-enable t
        lsp-ui-peek-show-directory t
        lsp-ui-doc-enable t
        lsp-ui-doc-position 'top
        lsp-ui-imenu-enable t
        )
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package helm-xref)
;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)
(use-package helm-lsp :commands helm-lsp-workspace-symbol
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol)
  )

(use-package lsp-treemacs
  ;;  :ensure t
  :config
  ;;(lsp-metals-treeview-enable t)
  (lsp-treemacs-sync-mode 1)
  ;;(setq lsp-metals-treeview-show-when-views-received t)
  )


(use-package lsp-java
  ;;  :ensure t
  :requires (lsp-ui-flycheck lsp-ui-sideline)
  :config
  (setq lsp-java-configuration-runtimes
        '[(:name "JavaSE-21"
                 :path
                 "/Library/Java/JavaVirtualMachines/temurin-21.jdk/Contents/Home"
                 ;;"/Library/Java/JavaVirtualMachines/temurin-21.jdk"
                 :default t)
          ])
  (setq lsp-java-jdt-download-url
        ;;     "http://download.eclipse.org/che/che-ls-jdt/snapshots/che-jdt-language-server-latest.tar.gz")
        ;;"https://www.eclipse.org/downloads/download.php?file=/jdtls/milestones/1.33.0/jdt-language-server-1.30.1-202312071447.tar.gz")
        ;;"https://www.eclipse.org/downloads/download.php?file=/jdtls/milestones/1.33.0/jdt-language-server-1.33.0-202402151717.tar.gz"
        ;;"https://www.eclipse.org/downloads/download.php?file=/jdtls/milestones/1.39.0/jdt-language-server-1.34.0-202404031240.tar.gz"
        "https://download.eclipse.org/jdtls/milestones/1.39.0/jdt-language-server-1.39.0-202408291433.tar.gz"
        )
  ;;    (setq lsp-java-workspace-dir  "~/src/scharp-ft/scharp-planner/")
  (setenv "JAVA_HOME" "/Library/Java/JavaVirtualMachines/temurin-23.jdk/Contents/Home")
  (setq lsp-java-java-path "/Library/Java/JavaVirtualMachines/temurin-23.jdk/Contents/Home/bin/java")
  (setq lsp-java-import-gradle-java-home "/Library/Java/JavaVirtualMachines/temurin-23.jdk/Contents/Home/bin/java")
  (setq lsp-java-trace-server "verbose") ;; values are "off", "verbose", "messages"
  (setq lsp-java-inhibit-message nil)
  (setq lsp-inhibit-message nil)
  (setq lsp-print-io t)
  (defun my-java-lsp-setup ()
    (interactive)
    (message "Setting up java lsp")
    (setq tab-width 2
          c-basic-offset 2
          c-max-one-liner-length 120)
    (toggle-truncate-lines 1)
    (setq-local tab-width 2)
    (setq-local c-basic-offset 2)
    (setq fill-column 120)
    ;;(setq lsp-java-format-settings-url "file://Users/marcelbecker/src/scharp-ft/scharp-planner/eclipse-utils/EclipseRspaceFormatter.xml")
    ;;(debug-on-variable-change lsp-java-format-settings-url)
    (setq lsp-java-format-settings-url "file://Users/marcelbecker/Dropbox/.emacs.d/EclipseFormat.xml")
    (setq lsp-java-format-settings-profile "Marcel-100-Width")

    (setq lsp-java-vmargs
          (list "-noverify"
                "-Xmx30G"
                ;;"-XX:+UseG1GC"
                "-XX:+UseStringDeduplication"
                "-XX:+UseParallelGC"
                "-XX:GCTimeRatio=4"
                "-XX:AdaptiveSizePolicyWeight=90"
                "-Dsun.zip.disableMemoryMapping=true"
                "-Xms1000m"
                "-XX:+EnableDynamicAgentLoading"
                ;; "-javaagent:/Users/marcelbecker/Downloads/lombok/lombok.jar"
                "-javaagent:/Users/marcelbecker/.gradle/caches/modules-2/files-2.1/org.projectlombok/lombok/1.18.34/ec547ef414ab1d2c040118fb9c1c265ada63af14/lombok-1.18.34.jar"
                ;;"-Xbootclasspath/a:/Users/marcelbecker/Downloads/lombok/lombok.jar"
                ))
    ;; (setq lsp-file-watch-ignored
    ;;       '(".idea" ".ensime_cache" ".eunit" "node_modules"
    ;;         ".git" ".hg" ".fslckout" "_FOSSIL_"
    ;;         ".bzr" "_darcs" ".tox" ".svn" ".stack-work"
    ;;         "build"))
    ;;(setq lsp-java-import-order '["" "java" "javax" "#"])
    ;; Don't organize imports on save
    (setq lsp-java-save-actions-organize-imports nil)
    )
  ;; (remove-hook 'java-mode-hook  'lsp-java-enable)
  (add-hook 'java-mode-hook 'my-java-lsp-setup)
  (add-hook 'java-mode-hook  'lsp)
  (add-hook 'java-mode-hook  'flycheck-mode)
  (add-hook 'java-mode-hook  'company-mode)
  ;;(add-hook 'java-mode-hook  (lambda () (lsp-ui-flycheck-enable t)))
  (add-hook 'java-mode-hook  'lsp-ui-sideline-mode)
  ;;  (add-hook 'java-mode-hook #'lsp-java-boot-lens-mode)

  )


(use-package dap-mode
  :after lsp-mode
  :bind
  (:map dap-mode-map
        ;; Intellij Debug Shortcuts
        ("<f7>" . dap-step-in)
        ("<f8>" . dap-next)
        ("S-<f8>" . dap-step-out)
        ("M-<f8>" . dap-breakpoint-toggle)
        ("<f9>" . dap-continue)
        ("<M-f2>" . dap-delete-all-sessions)
        )
  :custom
  ;;(dap-python-debugger 'debugpy)
  (dap-python-executable "/usr/local/bin/python3")
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-auto-configure-mode)
  ;; use tooltips for mouse hover
  ;; if it is not enabled `dap-mode' will use the minibuffer.
  (tooltip-mode 1);; enables mouse hover support
  (dap-tooltip-mode 1)
  ;; use tooltips for mouse hover
  ;; if it is not enabled `dap-mode' will use the minibuffer.
  (dap-ui-controls-mode 1)
  (require 'dap-java)
  (require 'dap-python)
  (setq lsp-enable-dap-auto-configure nil)
  ;; ECLIPSE Debug Shortcuts
  ;; (global-set-key (kbd "<f5>") 'dap-step-in)
  ;; (global-set-key (kbd "<f6>") 'dap-next)
  ;; (global-set-key (kbd "<f7>") 'dap-step-out)
  ;; (global-set-key (kbd "<f8>") 'dap-continue)


  (dap-register-debug-template
   "localhost:5005"
   (list :type "java"
         :request "attach"
         :hostName "localhost"
         :port 5005))
  (dap-register-debug-template
   "Scharp Unitest"
   (list :type "java"
         :request "launch"
         :vmArgs "-ea"
         :cwd (cdr (project-current))
         :projectName "scharp-planner"
         :env '((VCAP_SERVICES_KRID_CREDENTIALS_AUTHDOMAIN . https://id-api.apps.krstaging.kesselrun.us)
                (VCAP_SERVICES_KRID_CREDENTIALS_CLIENTID . anything)
                (VCAP_SERVICES_KRID_CREDENTIALS_CLIENTSECRET . anything)
                (VCAP_SERVICES_AMQ_CREDENTIALS_USERNAME . admin)
                (VCAP_SERVICES_AMQ_CREDENTIALS_PD . admin)
                (SPRING_PROFILES_ACTIVE . "rebelAllianceDisabled,nodb,test,nossl")
                (SPRING_BOOT_DEFAULT_TEST_PASSWORD . test)
                (VCAP_SERVICES_SCHARP_PLANNER_MYSQL_CREDENTIALS_USERNAME . root)
                (VCAP_SERVICES_SCHARP_PLANNER_MYSQL_CREDENTIALS_PASSWORD . scharp)
                (ENABLE_SSL . false))
         ))
  )


(use-package jarchive
  :hook (after-init . jarchive-setup))

(with-eval-after-load 'lsp-mode
  ;; :global/:workspace/:file
  (setq lsp-modeline-diagnostics-scope :workspace))

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

;; optionally if you want to use debugger


;; (use-package lsp-intellij
;;   :after (lsp-mode lsp-ui)
;;   )


;; ANSI-escape coloring in compilation-mode
;; {{ http://stackoverflow.com/questions/13397737/ansi-coloring-in-compilation-mode
(ignore-errors
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (unless (featurep 'ansi-color) (require 'ansi-color))
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))

(require 'lsp-java-boot)

;; to enable the lenses
(add-hook 'lsp-mode-hook #'lsp-lens-mode)
(add-hook 'java-mode-hook #'lsp-java-boot-lens-mode)
(add-hook 'java-mode-hook #'lsp-ui-mode)
;;(add-hook 'java-mode-hook #'lsp-intellij-enable)


;;(setq lsp-java-format-settings-url "file://Users/marcelbecker/Dropbox/.emacs.d/EclipseFormat.xml")
;;(setq lsp-java-format-settings-profile "Marcel-100-Width")

;; use (regexp-quote "\[\w*\] \[\w*\] (ERROR) \w* \((\w*\.java):([0-9]+)\).*$")
;; to print the escaped string.
;; C-u C-x C-e to print the string in the buffer
;; (defun re-seq (regexp string)
;;   "Get a list of all regexp matches in a string"
;;   (save-match-data
;;     (let ((pos 0)
;;           matches)
;;       (while (string-match regexp string pos)
;;         (push (match-string 0 string) matches)
;;         (setq pos (match-end 0)))
;;       matches)))

;; ; Sample URL
;; (setq urlreg "\\(?:http://\\)?www\\(?:[./#\+-]\\w*\\)+")
;; ; Sample invocation
;; (re-seq urlreg (buffer-string))

;; (s-match-strings-all
;; "\\[\\w*\\] \\[\\w*\\] \\(ERROR\\) \\w* (\\(\\w*.java\\):\\([0-9]+\\))\.*$"
;; "[] [main] ERROR readScenarioFromJsonObject (ScenarioLoaderFromAvroJson.java:103) - St = Tue, 4 Sep 2018 06:00:00 GMT Et = Wed, 5 Sep 2018 06:00:00 GMT")


(add-to-list 'compilation-error-regexp-alist 'my-message-error)
(add-to-list 'compilation-error-regexp-alist-alist
             '(my-message-error
               "^\\[[_[:alnum:][:space:]-]*\\] \\[[_[:alnum:][:space:]-]*\\] \\(ERROR\\) <?\\w*>? (\\(\\w*.java\\):\\([0-9]+\\))\.*$"
               2 3 nil nil nil
               (1 compilation-error-face)))
(add-to-list 'compilation-error-regexp-alist 'my-message-info)
(add-to-list 'compilation-error-regexp-alist-alist
             '(my-message-info
               "^\\[[_[:alnum:][:space:]-]*\\] \\[[_[:alnum:][:space:]-]*\\] \\(INFO\\)  <?\\w*>? (\\(\\w*.java\\):\\([0-9]+\\))\.*$"
               2 3 nil nil nil
               (1 compilation-info-face)))

(add-to-list 'compilation-error-regexp-alist 'my-message-warn)
(add-to-list 'compilation-error-regexp-alist-alist
             '(my-message-warn
               "^\\[[_[:alnum:][:space:]-]*\\] \\[[_[:alnum:][:space:]-]*\\] \\(WARN\\)  <?\\w*>? (\\(\\w*.java\\):\\([0-9]+\\))\.*$"
               2 3 nil nil nil
               (1 compilation-info-face)))


(defun my-load-ccl ()
  (interactive)
  (use-package ccls
    :hook ((c-mode c++-mode objc-mode) .
           (lambda () (require 'ccls) (lsp)))
    :config
    (setq ccls-executable "ccls")))




;; pip install python-language-server[all]
;; pip instll pyls-mypy Mypy type checking for Python 3
;; pip intall pyls-isort Isort import sort code formatting
;; pip install pyls-black for code formatting using Black
;; pip3 install virtualenvwrapper
;; pip install keras
;; pip install tensorflow
;; pip install matplotlib
;; pip install -U tensorflow==2.0.0-beta1
;; pip install ipython
;; pip install jupyter -U
;; pip install --upgrade pyzmq
;; pip install python-language-server[all]
;; pip install autoflake
;; pip install isort
;; pip install flake8
;; pip install black
;; pip install -U tensorflow-datasets
;; pip install pyflakes
;; pip instll pyls-mypy
;; pip install pyls-mypy
;; pip install pyls-isort
;; pip install pyls-black
;; pip install "ptvsd>=4.2"
;; (use-package lsp-python-ms
;;   :defer 0.3
;;   :config
;;   :hook (python-mode . (lambda ()
;;                          (require 'lsp-python-ms)
;;                          (lsp)))  ; or lsp-deferred
;;   :custom
;;   (lsp-python-ms-dir (expand-file-name "~/.emacs.d/elisp/python-language-server/output/bin/Release/"))
;;   (lsp-python-ms-executable "~/.emacs.d/elisp/python-language-server/output/bin/Release/linux-x64/publish/Microsoft.Python.LanguageServer"))

(defun my-use-pyright-as-python-server ()
  (interactive)
  (use-package lsp-pyright
    ;;    :ensure t
    :config
    (setq lsp-pyright-python-executable-cmd "/usr/local/bin/python3")
    :hook (python-mode . (lambda ()
                           (require 'lsp-pyright)
                           (lsp)))))  ; or lsp-deferred


(defun my-use-jedi-as-python-server ()
  (interactive)
  (use-package lsp-jedi
    ;;    :ensure t
    :config
    (with-eval-after-load "lsp-mode"
      (add-to-list 'lsp-disabled-clients 'pyls)
      ;;(add-to-list 'lsp-enabled-clients 'jedi)
      )))


(my-use-jedi-as-python-server)


(defun my-eglot-init ()
  (interactive)
  (use-package eglot :straight t)
  (use-package eglot-java
    :straight t
    :config
    (add-hook 'java-mode-hook 'eglot-java-mode)
    (add-hook 'java-mode-hook 'eglot-ensure)
    (with-eval-after-load 'eglot-java
      (define-key eglot-java-mode-map (kbd "C-c l n") #'eglot-java-file-new)
      (define-key eglot-java-mode-map (kbd "C-c l x") #'eglot-java-run-main)
      (define-key eglot-java-mode-map (kbd "C-c l t") #'eglot-java-run-test)
      (define-key eglot-java-mode-map (kbd "C-c l N") #'eglot-java-project-new)
      (define-key eglot-java-mode-map (kbd "C-c l T") #'eglot-java-project-build-task)
      (define-key eglot-java-mode-map (kbd "C-c l R") #'eglot-java-project-build-refresh)))

;;  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
  ;; (add-hook 'c-mode-hook 'eglot-ensure)
  ;; (add-hook 'c++-mode-hook 'eglot-ensure))


(defconst my-eclipse-jdt-home
  "/Users/marcelbecker/Dropbox/.emacs.d/.cache/lsp/eclipse.jdt.ls/features/org.eclipse.equinox.executable_3.8.2600.v20240722-2106.jar")

(defun my-eclipse-jdt-contact (interactive)
  (let ((cp (getenv "CLASSPATH")))
    (setenv "CLASSPATH" (concat cp ":" my-eclipse-jdt-home))
    (unwind-protect
        (eglot--eclipse-jdt-contact nil)
      (setenv "CLASSPATH" cp))))

;; (setcdr (assq 'java-mode eglot-server-programs) #'my-eclipse-jdt-contact)



;; https://www.mortens.dev/blog/emacs-and-the-language-server-protocol/
(defun netrom/helm-lsp-workspace-symbol-at-point ()
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively #'helm-lsp-workspace-symbol)))

(defun netrom/helm-lsp-global-workspace-symbol-at-point ()
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively #'helm-lsp-global-workspace-symbol)))

(setq netrom--general-lsp-hydra-heads
      '(;; Xref
        ("d" xref-find-definitions "Definitions" :column "Xref")
        ("D" xref-find-definitions-other-window "-> other win")
        ("r" xref-find-references "References")
        ("s" netrom/helm-lsp-workspace-symbol-at-point "Helm search")
        ("S" netrom/helm-lsp-global-workspace-symbol-at-point "Helm global search")

        ;; Peek
        ("C-d" lsp-ui-peek-find-definitions "Definitions" :column "Peek")
        ("C-r" lsp-ui-peek-find-references "References")
        ("C-i" lsp-ui-peek-find-implementation "Implementation")

        ;; LSP
        ("p" lsp-describe-thing-at-point "Describe at point" :column "LSP")
        ("C-a" lsp-execute-code-action "Execute code action")
        ("R" lsp-rename "Rename")
        ("t" lsp-goto-type-definition "Type definition")
        ("i" lsp-goto-implementation "Implementation")
        ("f" helm-imenu "Filter funcs/classes (Helm)")
        ("C-c" lsp-describe-session "Describe session")

        ;; Flycheck
        ("l" lsp-ui-flycheck-list "List errs/warns/notes" :column "Flycheck"))

      netrom--misc-lsp-hydra-heads
      '(;; Misc
        ("q" nil "Cancel" :column "Misc")
        ("b" pop-tag-mark "Back")))

;; Create general hydra.
(eval `(defhydra netrom/lsp-hydra (:color blue :hint nil)
         ,@(append
            netrom--general-lsp-hydra-heads
            netrom--misc-lsp-hydra-heads)))

(add-hook 'lsp-mode-hook
          (lambda () (local-set-key (kbd "C-z C-j") 'netrom/lsp-hydra/body)))
