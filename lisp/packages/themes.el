;;; themes.el --- Theme configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Theme setup for Emacs, including Doom Opera and Modus themes.

;;; Code:

;; -------------------------------
;; Minimal Themes
;; -------------------------------
(use-package minimal-theme
  :straight (minimal-theme :type git :host github :repo "ashwinjadhav818/emacs-minimal-theme")
  :config
  (load-theme 'minimal-black t))


;; -------------------------------
;; Modus Themes
;; -------------------------------
(use-package modus-themes
  :ensure t
  )

;; -------------------------------
;; Nord Theme
;; -------------------------------
(use-package nord-theme
  :ensure t
  )

(provide 'themes)
;;; themes.el ends here
