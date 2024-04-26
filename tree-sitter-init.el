;;; -*- lexical-binding: t -*-

(use-package treesit
  :straight (:type built-in)
  :if (featurep 'treesit)
  :config
  (setq treesit-language-source-alist
        (mapcar
         (lambda (item)
           (let ((lang (nth 0 item))
                 (url (nth 1 item))
                 (rev (nth 2 item))
                 (source-dir (nth 3 item)))
             `(,lang ,url ,rev ,source-dir
                     ,(executable-find "gcc") ,(executable-find "c++"))))
         '((bash "https://github.com/tree-sitter/tree-sitter-bash")
           (cmake "https://github.com/uyha/tree-sitter-cmake")
           (css "https://github.com/tree-sitter/tree-sitter-css")
           (elisp "https://github.com/Wilfred/tree-sitter-elisp")
           (go "https://github.com/tree-sitter/tree-sitter-go")
           (html "https://github.com/tree-sitter/tree-sitter-html")
           (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
           (json "https://github.com/tree-sitter/tree-sitter-json")
           (make "https://github.com/alemuller/tree-sitter-make")
           (markdown "https://github.com/ikatyang/tree-sitter-markdown")
           (python "https://github.com/tree-sitter/tree-sitter-python")
           (toml "https://github.com/tree-sitter/tree-sitter-toml")
           (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
           (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
           (yaml "https://github.com/ikatyang/tree-sitter-yaml"))))
  (setq treesit-font-lock-level 4)
  (setq major-mode-remap-alist
        '((typescript-mode . typescript-ts-mode)
          (js-mode . javascript-ts-mode)
          (python-mode . python-ts-mode)
          (json-mode . json-ts-mode)))
  (cl-loop for (old-mode . new-mode) in major-mode-remap-alist
           do (my/set-smartparens-indent new-mode)
           do (set (intern (concat (symbol-name new-mode) "-hook"))
                   (list
                    (eval `(lambda ()
                             (run-hooks
                              ',(intern (concat (symbol-name old-mode) "-hook")))))))))
