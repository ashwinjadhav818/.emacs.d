;;; themes.el --- Theme configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Theme setup for Emacs, including Doom Opera and Modus themes.

;;; Code:

;; -------------------------------
;; Doom Themes (Doom Opera as default)
;; -------------------------------
(use-package doom-themes
  :ensure t
  :init
  ;; Load Doom Opera theme by default
  (load-theme 'doom-opera :no-confirm)
  )

;; -------------------------------
;; Modus Themes (optional fallback)
;; -------------------------------
(use-package modus-themes
  :ensure t
  ;; You can switch to Modus with: (load-theme 'modus-vivendi :no-confirm)
  )

;; -------------------------------
;; Nord Theme (optional fallback)
;; -------------------------------
(use-package nord-theme
  :ensure t
  ;; You can switch to Nord with: (load-theme 'nord :no-confirm)
  )

(provide 'themes)
;;; themes.el ends here
