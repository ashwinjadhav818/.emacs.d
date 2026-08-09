;;; init.el --- Personal Emacs configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; Entry point for my Emacs setup.
;; Loads custom Lisp modules, applies performance tweaks, and sets up packages.

;;; Code:

;; Load Path Setup
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/packages" user-emacs-directory))

;; Performance Tweaks
;; Increase garbage collection threshold to reduce GC pauses during heavy ops.
(setq gc-cons-threshold #x40000000)

;; Increase process output read size (default is 4KB, bump to 4MB).
(setq read-process-output-max (* 4 1024 1024))

;; reset GC threshold after startup for balance
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold #x1000000)))

;; Modules
(require 'packages)     ;; package.el / straight.el / use-package setup
(require 'functions)    ;; helper functions
(require 'config)       ;; general settings
(require 'keymaps)      ;; global + leader keymaps
(require 'completion)

(require 'themes)       ;; theme setup
(require 'editing)      ;; editing enhancements
(require 'org)          ;; org
(require 'ui)           ;; UI elements (modeline, dashboard, etc.)
(require 'lsp)          ;; LSP and coding
(require 'wm)           ;; emacs as a operating system

(provide 'init)
;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("cee8b387a39aeb2a749b02950ea46d6844b8d3786ac5da8623fdbe569c934aae"
     "723cb1022e81737895b010f315c08dccdfb4cb9b4d86256a4fb222c4a923a698"
     "b8f9f3d4d3c78b1959be9e40769475890c5cda484e052395a5001f50dbe28016" default))
 '(package-vc-selected-packages
   '((janet-ts-mode :url "https://github.com/sogaiu/janet-ts-mode"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
