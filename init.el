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
(require 'config)       ;; general settings
(require 'functions)    ;; helper functions
(require 'keymaps)      ;; global + leader keymaps

(require 'themes)       ;; theme setup
(require 'editing)      ;; editing enhancements
(require 'terminal)     ;; terminal emulation (vterm/ansi-term/etc.)
(require 'misc)         ;; misc quality-of-life tweaks
(require 'org-config)   ;; org + org-roam
(require 'org-appearance) ;; org visuals (org-modern, bullets, etc.)
(require 'programming)  ;; prog-mode, lsp, treesit, etc.
(require 'ui)           ;; UI elements (modeline, dashboard, etc.)

(provide 'init)
;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("e91f1b9f4c25dac9f3754e9c8f0cbd6689998b331db7b28f71251c4c65a21272"
	 "13e8dba66fb98aed0693d05cbf34194a1d53747187e14f74faf297e9ee56e26b"
	 "a04a7a55f4ab59a04bdd9f21ed6c4a7441f1f3dfbcd8d7d9ab77f7a5283f021b"
	 "296ddf1bfca4c95f0266e3f11dbbe6413d68bb4db190f708c3cf70c9fc50f09f"
	 "8fab847727c66250080a8c84454c8950d4ee854a9a56cfb60ece9c0145e12f4a"
	 "01be51d3a575f5f565aca6485b135e39ad5662d071326bc38855154fd062fc32"
	 "74ba8278e74fbd0826b137f3589500a830b91eb8911a8873f10a2857fc406eda"
	 "cfc1e49eec0d57e2f70ef3a6db3b543bbb2edcf16a9f164231afcc4d1263f9fb"
	 "4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d"
	 default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
