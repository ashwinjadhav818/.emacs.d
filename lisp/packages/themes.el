;;; themes.el --- Theme configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Theme setup for Emacs, including Doom Opera and Modus themes.

;;; Code:

;; -------------------------------
;; Modus Themes
;; -------------------------------
(use-package modus-themes
  :ensure t
  :config
  (load-theme 'modus-vivendi t)
  )

;; -------------------------------
;; Nord Theme
;; -------------------------------
(use-package nord-theme
  :ensure t
  )

(provide 'themes)
;;; themes.el ends here
