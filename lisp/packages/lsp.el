;;; lsp.el --- Language Server Protocol configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configures LSP-mode, UI enhancements, and Company-mode for auto-completion.
;; Supports PHP, TypeScript, JavaScript, HTML, CSS, C and Janet.

;;; Code:

(use-package lsp-mode
  :ensure t
  :diminish "LSP"
  :commands (lsp lsp-deferred)
  :init
  ;; Automatically start LSP for your specified languages
  (dolist (hook '(css-mode-hook
                  css-ts-mode-hook
                  html-mode-hook
                  c-mode-hook
                  c-ts-mode-hook
                  typescript-ts-mode-hook
                  tsx-ts-mode-hook
                  janet-ts-mode-hook
                  python-mode-hook
                  python-ts-mode-hook
                  lisp-mode-hook
                  emacs-lisp-mode-hook)) ; Added elisp just in case
    (add-hook hook #'lsp-deferred)))

(use-package lsp-completion
  :no-require
  :hook ((lsp-mode . lsp-completion-mode)))

(use-package lsp-ui
  :ensure t
  :commands
  (lsp-ui-doc-show
   lsp-ui-doc-glance)
  :bind (:map lsp-mode-map
              ("C-c C-d" . 'lsp-ui-doc-glance))
  :after (lsp-mode evil)
  :config (setq lsp-ui-doc-enable t
                evil-lookup-func #'lsp-ui-doc-glance ; Makes K in evil-mode toggle the doc for symbol at point
                lsp-ui-doc-show-with-cursor nil      ; Don't show doc when cursor is over symbol - too distracting
                lsp-ui-doc-include-signature t       ; Show signature
                lsp-ui-doc-position 'at-point))

;; Flycheck for better error highlighting than Flymake
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package apheleia
  :ensure apheleia
  :diminish ""
  :init
  ;; Equivalent to: vim.g.disable_autoformat = false
  (defvar my/disable-autoformat nil
    "Global flag to toggle auto-formatting on save.")

  :config
  ;; 1. Define and customize formatters with explicit 4-space rules
  (setf (alist-get 'prettier apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath "--tab-width" "4"))
  (setf (alist-get 'prettier-json apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath "--tab-width" "4"))
  (setf (alist-get 'stylua apheleia-formatters)
        '("stylua" "-"))
  (setf (alist-get 'yamlfmt apheleia-formatters)
        '("yamlfmt" "-"))

  ;; 2. Replicate Neovim's formatters_by_ft map
  (setq apheleia-mode-alist
        '((lua-mode          . stylua)
          (lua-ts-mode       . stylua)
          (js-ts-mode        . prettier)
          (js2-mode          . prettier)
          (typescript-ts-mode . prettier)
          (tsx-ts-mode       . prettier)
          (yaml-mode         . yamlfmt)
          (yaml-ts-mode      . yamlfmt)
          (html-mode         . prettier)
          (json-ts-mode      . prettier-json)
          (markdown-mode     . prettier)
          (css-mode          . prettier)
          (css-ts-mode       . prettier)
          (c-mode             . clang-format)
          (c-ts-mode          . clang-format)
          (c++-mode           . clang-format)
          (c++-ts-mode        . clang-format)))

  ;; 3. Replicate Neovim's disable_autoformat logic check on buffer save
  (setq apheleia-inhibit-functions
        (list (lambda () my/disable-autoformat)))

  ;; Turn it on everywhere globally (Equivalent to BufWritePre event)
  (apheleia-global-mode +1))

(use-package lsp-tailwindcss
  :straight '(lsp-tailwindcss :type git :host github :repo "merrickluo/lsp-tailwindcss")
  :init (setq lsp-tailwindcss-add-on-mode t)
  :config
  (dolist (tw-major-mode
           '(css-mode
             css-ts-mode
             typescript-mode
             typescript-ts-mode
             tsx-ts-mode
             js2-mode
             js-ts-mode
             clojure-mode))
    (add-to-list 'lsp-tailwindcss-major-modes tw-major-mode)))

(use-package treesit
  :mode (("\\.tsx\\'" . tsx-ts-mode)
         ("\\.js\\'"  . typescript-ts-mode)
         ("\\.mjs\\'" . typescript-ts-mode)
         ("\\.mts\\'" . typescript-ts-mode)
         ("\\.cjs\\'" . typescript-ts-mode)
         ("\\.ts\\'"  . typescript-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode)
         ("\\.json\\'" . json-ts-mode)
         ("\\.Dockerfile\\'" . dockerfile-ts-mode))
  :preface


  (defun os/setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((css        . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
               (bash         "https://github.com/tree-sitter/tree-sitter-bash")
               (html       . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src"))
               (json       . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
               (python     . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
               (go           "https://github.com/tree-sitter/tree-sitter-go" "v0.20.0")
               (markdown     "https://github.com/ikatyang/tree-sitter-markdown")
               (make         "https://github.com/alemuller/tree-sitter-make")
               (elisp        "https://github.com/Wilfred/tree-sitter-elisp")
               (cmake        "https://github.com/uyha/tree-sitter-cmake")
               (c            "https://github.com/tree-sitter/tree-sitter-c")
               (cpp          "https://github.com/tree-sitter/tree-sitter-cpp")
               (toml         "https://github.com/tree-sitter/tree-sitter-toml")
               (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
               (yaml       . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))))
      (add-to-list 'treesit-language-source-alist grammar)
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))
  :config
  (os/setup-install-grammars)
  (dolist (mapping
           '((python-mode     . python-ts-mode)
             (css-mode        . css-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (js-mode         . typescript-ts-mode)
             (js2-mode        . typescript-ts-mode)
             (c-mode          . c-ts-mode)
             (c++-mode        . c++-ts-mode)
             (c-or-c++-mode   . c-or-c++-ts-mode)
             (bash-mode       . bash-ts-mode)
             (json-mode       . json-ts-mode)
             (js-json-mode    . json-ts-mode)
             (sh-mode         . bash-ts-mode)
             (sh-base-mode    . bash-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping)))

(use-package janet-ts-mode
  :vc (:url "https://github.com/sogaiu/janet-ts-mode"
			:rev :newest))
(with-eval-after-load 'lsp-mode
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("janet-lsp"))
                    :major-modes '(janet-mode janet-ts-mode)
                    :server-id 'janet-lsp)))

;; Optional: Automatically start lsp-mode when opening Janet files
(add-hook 'janet-ts-mode-hook #'lsp-deferred)
(add-hook 'janet-mode-hook #'lsp-deferred)

(provide 'lsp)
;;; lsp.el ends here
