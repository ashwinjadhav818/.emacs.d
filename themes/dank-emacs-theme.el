;;; dank-emacs-theme.el --- Enhanced theme using Matugen SCSS variables with dank16 colors -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Generated (Enhanced)
;; Version: 1.3
;; Package-Requires: ((emacs "24.1"))
;; Keywords: faces

;;; Commentary:

;; An enhanced theme using Matugen SCSS variables integrated with dank16 colors:
;; - Rich color palette from dank16 for vibrant syntax highlighting
;; - Improved contrast and readability
;; - Better source block distinction with refined backgrounds
;; - Enhanced org-mode styling with hidden asterisks
;; - Superior visual hierarchy and modern aesthetics

;;; Code:

(deftheme dank-emacs "Enhanced theme using Matugen variables with dank16 color integration.")

;; Define all the color variables (replaced by template processor)
(let* ((bg "#111410")
      (err "#f48b6e")  ; Red from dank16
      (err-container "#93000a")
      (on-background "#e1e3db")
      (on-err "#690005")
      (on-err-container "#ffdad6")
      (on-primary "#023905")
      (on-primary-container "#000000")
      (on-secondary "#21351d")
      (on-secondary-container "#cfe7c4")
      (on-surface "#e1e3db")
      (on-surface-variant "#c1c9bb")
      (on-tertiary "#003737")
      (on-tertiary-container "#000000")
      (outline-color "#8b9386")
      (outline-variant "#41493e")
      (primary "#9cd58f")
      (primary-container "#689e5d")
      (secondary "#b5cdac")
      (secondary-container "#374c32")
      (shadow "#000000")
      (surface "#111410")
      (surface-container "#282b26")
      (surface-container-high "#323630")
      (surface-container-highest "#282b26")
      (surface-container-low "#191c18")
      (surface-container-lowest "#0c0f0b")
      (surface-variant "#41493e")
      (tertiary "#6bd7d7")
      (tertiary-container "#27a0a0")

      ;; Enhanced dank16 colors for better syntax highlighting
      (dank-red "#f48b6e")          ; Bright red
      (dank-red-alt "#ffb49f")      ; Alternative red
      (dank-green "#6ed56a")        ; Vibrant green
      (dank-green-bright "#a9ffa5") ; Bright green
      (dank-yellow "#ffee72")       ; Warm yellow
      (dank-yellow-bright "#fff4a5") ; Bright yellow
      (dank-blue "#89ca7a")         ; Blue-green
      (dank-magenta "#326227")      ; Teal-magenta
      (dank-cyan "#9cd58f")         ; Bright cyan
      (dank-cyan-bright "#b5eaa8") ; Brightest cyan
      (dank-cyan-dark "#cfffc4")   ; Dark cyan
      (dank-teal "#e0ffd9")        ; Dark teal
      (dank-fg "#d4dfd2")           ; Light foreground
      (dank-gray "#828a80")         ; Gray
      (dank-white "#f9fff8")       ; White

      ;; Map success colors to green
      (success "#6ed56a")
      (on-success "#003737")
      (success-container "#27a0a0")
      (on-success-container "#000000")

      ;; Map fixed colors
      (primary-fixed "#b7f2a8")
      (primary-fixed-dim "#9cd58f")
      (secondary-fixed "#d1eac7")
      (secondary-fixed-dim "#b5cdac")
      (tertiary-fixed "#89f4f3")
      (tertiary-fixed-dim "#6bd7d7")
      (on-primary-fixed "#002201")
      (on-primary-fixed-variant "#1e511a")
      (on-secondary-fixed "#0d200a")
      (on-secondary-fixed-variant "#374c32")
      (on-tertiary-fixed "#002020")
      (on-tertiary-fixed-variant "#004f50")

      ;; Map inverse colors
      (inverse-on-surface "#2e312c")
      (inverse-primary "#366a30")
      (inverse-surface "#e1e3db")

      ;; Terminal colors from dank16
      (term0 "#111410")
      (term1 "#f48b6e")
      (term2 "#6ed56a")
      (term3 "#ffee72")
      (term4 "#89ca7a")
      (term5 "#326227")
      (term6 "#9cd58f")
      (term7 "#d4dfd2")
      (term8 "#828a80")
      (term9 "#ffb49f")
      (term10 "#a9ffa5")
      (term11 "#fff4a5")
      (term12 "#b5eaa8")
      (term13 "#cfffc4")
      (term14 "#e0ffd9")
      (term15 "#f9fff8"))

  (custom-theme-set-faces
   'dank-emacs
   ;; Basic faces
   `(default ((t (:background ,bg :foreground ,on-background))))
   `(cursor ((t (:background ,dank-cyan-bright))))
   `(highlight ((t (:background ,primary-container :foreground ,on-primary-container))))
   `(region ((t (:background ,primary-container :foreground ,dank-cyan-bright :extend t))))
   `(secondary-selection ((t (:background ,secondary-container :foreground ,on-secondary-container :extend t))))
   `(isearch ((t (:background ,dank-yellow :foreground ,bg :weight bold))))
   `(lazy-highlight ((t (:background ,secondary-container :foreground ,dank-yellow-bright))))
   `(vertical-border ((t (:foreground ,surface-variant))))
   `(border ((t (:background ,surface-variant :foreground ,surface-variant))))
   `(fringe ((t (:background ,surface :foreground ,dank-gray))))
   `(shadow ((t (:foreground ,dank-gray))))
   `(link ((t (:foreground ,dank-cyan-bright :underline t))))
   `(link-visited ((t (:foreground ,dank-magenta :underline t))))
   `(success ((t (:foreground ,success))))
   `(warning ((t (:foreground ,dank-yellow))))
   `(error ((t (:foreground ,err))))
   `(match ((t (:background ,dank-yellow :foreground ,bg :weight bold))))

   ;; Font-lock - enhanced with dank16 colors for vibrant syntax highlighting
   `(font-lock-builtin-face ((t (:foreground ,dank-cyan-bright))))
   `(font-lock-comment-face ((t (:foreground ,dank-gray :slant italic))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,outline-variant))))
   `(font-lock-constant-face ((t (:foreground ,dank-yellow-bright :weight bold))))
   `(font-lock-doc-face ((t (:foreground ,dank-fg :slant italic))))
   `(font-lock-function-name-face ((t (:foreground ,dank-cyan :weight bold))))
   `(font-lock-keyword-face ((t (:foreground ,dank-red-alt :weight bold))))
   `(font-lock-string-face ((t (:foreground ,dank-green))))
   `(font-lock-type-face ((t (:foreground ,dank-yellow))))
   `(font-lock-variable-name-face ((t (:foreground ,dank-fg))))
   `(font-lock-warning-face ((t (:foreground ,err :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,dank-teal))))
   `(font-lock-negation-char-face ((t (:foreground ,dank-red))))

   ;; Show paren
   `(show-paren-match ((t (:background ,primary-container :foreground ,dank-cyan-bright :weight bold))))
   `(show-paren-mismatch ((t (:background ,err-container :foreground ,on-err-container :weight bold))))

   ;; Mode line - improved status bar styling
   `(mode-line ((t (:background ,surface-container :foreground ,on-surface :box nil))))
   `(mode-line-inactive ((t (:background ,surface :foreground ,dank-gray :box nil))))
   `(mode-line-buffer-id ((t (:foreground ,dank-cyan :weight bold))))
   `(mode-line-emphasis ((t (:foreground ,dank-cyan :weight bold))))
   `(mode-line-highlight ((t (:foreground ,dank-cyan-bright :box nil))))

   ;; Improved Source blocks - seamless integration
   `(org-block ((t (:background ,surface-container-low :extend t :inherit fixed-pitch))))
   `(org-block-begin-line ((t (:background ,surface-container-low :foreground ,dank-teal :extend t :slant italic :inherit fixed-pitch))))
   `(org-block-end-line ((t (:background ,surface-container-low :foreground ,dank-teal :extend t :slant italic :inherit fixed-pitch))))
   `(org-code ((t (:background ,surface-container-low :foreground ,dank-yellow-bright :inherit fixed-pitch))))
   `(org-verbatim ((t (:background ,surface-container-low :foreground ,dank-cyan :inherit fixed-pitch))))
   `(org-meta-line ((t (:foreground ,dank-gray :slant italic))))

   ;; Org mode with enhanced colors and hidden asterisks
   `(org-level-1 ((t (:foreground ,dank-cyan :weight bold :height 1.2))))
   `(org-level-2 ((t (:foreground ,dank-blue :weight bold :height 1.1))))
   `(org-level-3 ((t (:foreground ,dank-magenta :weight bold))))
   `(org-level-4 ((t (:foreground ,dank-green :weight bold))))
   `(org-level-5 ((t (:foreground ,dank-yellow :weight bold))))
   `(org-level-6 ((t (:foreground ,dank-cyan-bright :weight bold))))
   `(org-level-7 ((t (:foreground ,dank-red-alt :weight bold))))
   `(org-level-8 ((t (:foreground ,dank-teal :weight bold))))
   `(org-document-title ((t (:foreground ,dank-cyan :weight bold :height 1.3))))
   `(org-document-info ((t (:foreground ,dank-blue))))
   `(org-todo ((t (:foreground ,dank-red :weight bold))))
   `(org-done ((t (:foreground ,success :weight bold))))
   `(org-headline-done ((t (:foreground ,dank-gray))))
   `(org-hide ((t (:foreground ,bg))))
   `(org-ellipsis ((t (:foreground ,dank-blue :underline nil))))
   `(org-table ((t (:foreground ,dank-magenta :inherit fixed-pitch))))
   `(org-formula ((t (:foreground ,dank-yellow-bright :inherit fixed-pitch))))
   `(org-checkbox ((t (:foreground ,dank-cyan :weight bold :inherit fixed-pitch))))
   `(org-date ((t (:foreground ,dank-teal :underline t))))
   `(org-special-keyword ((t (:foreground ,dank-gray :slant italic))))
   `(org-tag ((t (:foreground ,dank-gray :weight normal))))

   ;; Magit with enhanced diff colors
   `(magit-section-highlight ((t (:background ,surface-container-low))))
   `(magit-diff-hunk-heading ((t (:background ,surface-container :foreground ,dank-gray))))
   `(magit-diff-hunk-heading-highlight ((t (:background ,surface-container-high :foreground ,on-surface))))
   `(magit-diff-context ((t (:foreground ,dank-gray))))
   `(magit-diff-context-highlight ((t (:background ,surface-container-low :foreground ,on-surface))))
   `(magit-diff-added ((t (:background ,success-container :foreground ,dank-green-bright))))
   `(magit-diff-added-highlight ((t (:background ,success-container :foreground ,dank-green-bright :weight bold))))
   `(magit-diff-removed ((t (:background ,err-container :foreground ,dank-red-alt))))
   `(magit-diff-removed-highlight ((t (:background ,err-container :foreground ,dank-red-alt :weight bold))))
   `(magit-hash ((t (:foreground ,dank-gray))))
   `(magit-branch-local ((t (:foreground ,dank-blue :weight bold))))
   `(magit-branch-remote ((t (:foreground ,dank-cyan :weight bold))))

   ;; Company
   `(company-tooltip ((t (:background ,surface-container :foreground ,on-surface))))
   `(company-tooltip-selection ((t (:background ,primary-container :foreground ,dank-cyan-bright))))
   `(company-tooltip-common ((t (:foreground ,dank-cyan))))
   `(company-tooltip-common-selection ((t (:foreground ,dank-cyan-bright :weight bold))))
   `(company-tooltip-annotation ((t (:foreground ,dank-yellow))))
   `(company-scrollbar-fg ((t (:background ,dank-cyan))))
   `(company-scrollbar-bg ((t (:background ,surface-variant))))
   `(company-preview ((t (:foreground ,dank-gray :slant italic))))
   `(company-preview-common ((t (:foreground ,dank-cyan :slant italic))))

   ;; Ido
   `(ido-first-match ((t (:foreground ,dank-cyan :weight bold))))
   `(ido-only-match ((t (:foreground ,dank-green :weight bold))))
   `(ido-subdir ((t (:foreground ,dank-blue))))
   `(ido-indicator ((t (:foreground ,dank-red))))
   `(ido-virtual ((t (:foreground ,dank-gray))))

   ;; Helm
   `(helm-selection ((t (:background ,primary-container :foreground ,dank-cyan-bright))))
   `(helm-match ((t (:foreground ,dank-cyan :weight bold))))
   `(helm-source-header ((t (:background ,surface-container-high :foreground ,dank-cyan :weight bold :height 1.1))))
   `(helm-candidate-number ((t (:foreground ,dank-yellow :weight bold))))
   `(helm-ff-directory ((t (:foreground ,dank-cyan :weight bold))))
   `(helm-ff-file ((t (:foreground ,on-surface))))
   `(helm-ff-executable ((t (:foreground ,dank-green))))

   ;; corfu
   `(corfu-default ((t (:background ,surface-container :foreground ,on-surface))))
   `(corfu-current ((t (:background ,primary-container :foreground ,dank-cyan-bright))))

   ;; Which-key
   `(which-key-key-face ((t (:foreground ,dank-cyan :weight bold))))
   `(which-key-separator-face ((t (:foreground ,outline-variant))))
   `(which-key-command-description-face ((t (:foreground ,on-surface))))
   `(which-key-group-description-face ((t (:foreground ,dank-blue))))
   `(which-key-special-key-face ((t (:foreground ,dank-yellow :weight bold))))

   ;; Line numbers
   `(line-number ((t (:foreground ,dank-gray :inherit default))))
   `(line-number-current-line ((t (:foreground ,dank-cyan :weight bold :inherit default))))

   ;; Parenthesis matching
   `(sp-show-pair-match-face ((t (:background ,primary-container :foreground ,dank-cyan-bright))))
   `(sp-show-pair-mismatch-face ((t (:background ,err-container :foreground ,on-err-container))))

   ;; Rainbow delimiters - vibrant colors
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,dank-cyan))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,dank-yellow))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,dank-green))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,dank-blue))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,dank-magenta))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,dank-cyan-bright))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,dank-yellow-bright))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,dank-green-bright))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,dank-red-alt))))
   `(rainbow-delimiters-mismatched-face ((t (:foreground ,err :weight bold))))
   `(rainbow-delimiters-unmatched-face ((t (:foreground ,err :weight bold))))

   ;; Dired
   `(dired-directory ((t (:foreground ,dank-cyan :weight bold))))
   `(dired-ignored ((t (:foreground ,dank-gray))))
   `(dired-flagged ((t (:foreground ,dank-red))))
   `(dired-marked ((t (:foreground ,dank-yellow :weight bold))))
   `(dired-symlink ((t (:foreground ,dank-magenta :slant italic))))
   `(dired-header ((t (:foreground ,dank-cyan :weight bold :height 1.1))))

   ;; Terminal colors
   `(term-color-black ((t (:foreground ,term0 :background ,term0))))
   `(term-color-red ((t (:foreground ,term1 :background ,term1))))
   `(term-color-green ((t (:foreground ,term2 :background ,term2))))
   `(term-color-yellow ((t (:foreground ,term3 :background ,term3))))
   `(term-color-blue ((t (:foreground ,term4 :background ,term4))))
   `(term-color-magenta ((t (:foreground ,term5 :background ,term5))))
   `(term-color-cyan ((t (:foreground ,term6 :background ,term6))))
   `(term-color-white ((t (:foreground ,term7 :background ,term7))))

   ;; EShell
   `(eshell-prompt ((t (:foreground ,dank-cyan :weight bold))))
   `(eshell-ls-directory ((t (:foreground ,dank-cyan :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,dank-magenta :slant italic))))
   `(eshell-ls-executable ((t (:foreground ,dank-green))))
   `(eshell-ls-archive ((t (:foreground ,dank-yellow))))
   `(eshell-ls-backup ((t (:foreground ,dank-gray))))
   `(eshell-ls-clutter ((t (:foreground ,dank-red))))
   `(eshell-ls-missing ((t (:foreground ,dank-red))))
   `(eshell-ls-product ((t (:foreground ,on-surface-variant))))
   `(eshell-ls-readonly ((t (:foreground ,dank-gray))))
   `(eshell-ls-special ((t (:foreground ,dank-blue))))
   `(eshell-ls-unreadable ((t (:foreground ,dank-gray))))

   ;; Improved markdown mode
   `(markdown-header-face ((t (:foreground ,dank-cyan :weight bold))))
   `(markdown-header-face-1 ((t (:foreground ,dank-cyan :weight bold :height 1.2))))
   `(markdown-header-face-2 ((t (:foreground ,dank-blue :weight bold :height 1.1))))
   `(markdown-header-face-3 ((t (:foreground ,dank-magenta :weight bold))))
   `(markdown-header-face-4 ((t (:foreground ,dank-green :weight bold))))
   `(markdown-inline-code-face ((t (:foreground ,dank-yellow-bright :background ,surface-container-low :inherit fixed-pitch))))
   `(markdown-code-face ((t (:background ,surface-container-low :extend t :inherit fixed-pitch))))
   `(markdown-pre-face ((t (:background ,surface-container-low :inherit fixed-pitch))))
   `(markdown-table-face ((t (:foreground ,dank-magenta :inherit fixed-pitch))))

   ;; Web mode
   `(web-mode-html-tag-face ((t (:foreground ,dank-cyan))))
   `(web-mode-html-tag-bracket-face ((t (:foreground ,dank-gray))))
   `(web-mode-html-attr-name-face ((t (:foreground ,dank-yellow))))
   `(web-mode-html-attr-value-face ((t (:foreground ,dank-green))))
   `(web-mode-css-selector-face ((t (:foreground ,dank-cyan))))
   `(web-mode-css-property-name-face ((t (:foreground ,dank-blue))))
   `(web-mode-css-string-face ((t (:foreground ,dank-green))))

   ;; Flycheck
   `(flycheck-error ((t (:underline (:style wave :color ,err)))))
   `(flycheck-warning ((t (:underline (:style wave :color ,dank-yellow)))))
   `(flycheck-info ((t (:underline (:style wave :color ,dank-blue)))))
   `(flycheck-fringe-error ((t (:foreground ,err))))
   `(flycheck-fringe-warning ((t (:foreground ,dank-yellow))))
   `(flycheck-fringe-info ((t (:foreground ,dank-blue))))

   ;; Mini-buffer customization
   `(minibuffer-prompt ((t (:foreground ,dank-cyan :weight bold))))

   ;; Improved search highlighting
   `(lsp-face-highlight-textual ((t (:background ,primary-container :foreground ,dank-cyan-bright :weight bold))))
   `(lsp-face-highlight-read ((t (:background ,secondary-container :foreground ,dank-yellow-bright :weight bold))))
   `(lsp-face-highlight-write ((t (:background ,tertiary-container :foreground ,dank-green-bright :weight bold))))

   ;; Info and help modes
   `(info-title-1 ((t (:foreground ,dank-cyan :weight bold :height 1.3))))
   `(info-title-2 ((t (:foreground ,dank-blue :weight bold :height 1.2))))
   `(info-title-3 ((t (:foreground ,dank-magenta :weight bold :height 1.1))))
   `(info-title-4 ((t (:foreground ,dank-green :weight bold))))
   `(Info-quoted ((t (:foreground ,dank-yellow))))
   `(info-menu-header ((t (:foreground ,dank-cyan :weight bold))))
   `(info-menu-star ((t (:foreground ,dank-cyan))))
   `(info-node ((t (:foreground ,dank-blue :weight bold))))

   ;; Tabs
   `(tab-bar ((t (:background ,surface-container :foreground ,on-surface :box nil))))
   `(tab-bar-tab ((t (:background ,surface-container-high :foreground ,dank-cyan :weight bold :box nil))))
   `(tab-bar-tab-inactive ((t (:background ,surface :foreground ,dank-gray :box nil))))

   `(tab-line ((t (:background ,surface-container :foreground ,on-surface :box nil))))
   `(tab-line-tab ((t (:background ,surface :foreground ,dank-gray :box nil))))
   `(tab-line-tab-current ((t (:background ,surface-container-high :foreground ,dank-cyan :weight bold :box nil))))
   `(tab-line-tab-inactive ((t (:background ,surface :foreground ,dank-gray :box nil))))
   `(tab-line-highlight ((t (:background ,surface-container-highest :foreground ,dank-cyan-bright))))

   `(centaur-tabs-default ((t (:background ,surface-container :foreground ,on-surface))))
   `(centaur-tabs-selected ((t (:background ,surface-container-high :foreground ,dank-cyan :weight bold))))
   `(centaur-tabs-unselected ((t (:background ,surface :foreground ,dank-gray))))
   `(centaur-tabs-selected-modified ((t (:background ,surface-container-high :foreground ,dank-yellow :weight bold))))
   `(centaur-tabs-unselected-modified ((t (:background ,surface :foreground ,dank-yellow))))
   `(centaur-tabs-active-bar-face ((t (:background ,dank-cyan))))

   ;; Fixed-pitch faces
   `(fixed-pitch ((t (:family "monospace"))))
   `(fixed-pitch-serif ((t (:family "monospace serif"))))

   ;; Variable-pitch face
   `(variable-pitch ((t (:family "sans serif"))))
   ))

;; Add org-mode hooks for hiding leading stars
(with-eval-after-load 'org
  (setq org-hide-leading-stars t)
  (setq org-startup-indented t))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'dank-emacs)
;;; dank-emacs-theme.el ends here
