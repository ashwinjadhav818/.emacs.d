;;; ashwal-theme.el --- Enhanced Ashwal via JSON -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Enhanced Integration
;; Version: 2.1
;; Package-Requires: ((emacs "24.1"))
;; Keywords: faces

;;; Commentary:

;; An enhanced theme pulling a vibrant color scheme dynamically from Ashwal's JSON cache.
;; Adjusted mapping roles to compensate for low-contrast dark palettes.

(require 'json)

(defun xres-get-json (name)
  "Fetch a color from the ashwal JSON cache."
  (let* ((json-object-type 'hash-table)
         (json-key-type 'string)
         ;; Path to the generated color file
         (colors-json (json-read-file "~/.cache/ashwal/colors.json"))
         (colors-dict (gethash "colors" colors-json))
         (special-dict (gethash "special" colors-json)))
    (cond
     ((string= name "background") (gethash "background" special-dict))
     ((string= name "foreground") (gethash "foreground" special-dict))
     ((string= name "cursor")     (gethash "cursor"     special-dict))
     (t (gethash name colors-dict)))))

(deftheme ashwal "Enhanced dark theme pulling from dynamic JSON configurations.")

(let* ((class '((class color) (min-colors 89)))

       ;; --- Core Palette from JSON ---
       (bg         (xres-get-json "background"))
       (fg         (xres-get-json "foreground"))
       (cursor-bg  (xres-get-json "cursor"))

       ;; ANSI/Base Slots
       (term0      (xres-get-json "color0"))  ; Deep Base
       (term1      (xres-get-json "color1"))  ; Dark Blue-Gray
       (term2      (xres-get-json "color2"))  ; Muted Teal-Blue
       (term3      (xres-get-json "color3"))  ; Dark Purple
       (term4      (xres-get-json "color4"))  ; Mid Blue
       (term5      (xres-get-json "color5"))  ; Muted Blue
       (term6      (xres-get-json "color6"))  ; Slate Cyan
       (term7      (xres-get-json "color7"))  ; Light Gray
       (term8      (xres-get-json "color8"))  ; Bright Gray / Comments
       (term9      (xres-get-json "color9"))  ; Bright Blue-Purple
       (term10     (xres-get-json "color10")) ; Vibrant Blue
       (term11     (xres-get-json "color11")) ; Bright Indigo
       (term12     (xres-get-json "color12")) ; Sky Blue
       (term13     (xres-get-json "color13")) ; Royal Blue
       (term14     (xres-get-json "color14")) ; Ice Cyan
       (term15     (xres-get-json "color15")) ; Crisp Foreground

       ;; --- Shifted Mapping Roles to boost visibility ---
       (ash-red           term9)   ; Swapped to brighter blue-purple for syntax
       (ash-red-alt       term11)  ; High-vis accent
       (ash-green         term10)  ; Using the brighter variant for strings/success
       (ash-green-bright  term14)  ; Clear pop accent
       (ash-yellow        term12)  ; Bright sky blue for keywords/warnings
       (ash-yellow-bright term14)
       (ash-blue          term4)   ; Standard Blue
       (ash-magenta       term13)  ; High contrast mid-tone
       (ash-cyan          term6)
       (ash-cyan-bright   term14)  ; Clear accent
       (ash-gray          term8)
       (ash-teal          term14)

       ;; UI Context Containers (Lifted using mid-tones to step out of total black)
       (border             term1)
       (region-bg          term4)   ; Switched from dark color2 to a much more visible term4
       (surface            term1)   ; Pop out UI panels away from pure #090b0e
       (surface-container  term2)   ; Delineated containers
       (surface-low        term1)   ; Block elements
       (outline-variant    term5))

  (custom-theme-set-faces
   'ashwal

   ;; --- Basic Stuff ---
   `(default ((,class (:background ,bg :foreground ,fg))))
   `(cursor ((,class (:background ,cursor-bg :inverse-video t))))
   `(highlight ((,class (:background ,region-bg :foreground ,fg))))
   `(region ((,class (:background ,region-bg :foreground ,term15 :extend t))))
   `(secondary-selection ((,class (:background ,term2 :extend t))))
   `(vertical-border ((,class (:foreground ,border))))
   `(border ((,class (:background ,border :foreground ,border))))
   `(fringe ((,class (:background ,bg :foreground ,ash-gray))))
   `(shadow ((,class (:foreground ,ash-gray))))
   `(link ((,class (:foreground ,ash-cyan-bright :underline t))))
   `(link-visited ((,class (:foreground ,ash-magenta :underline t))))
   `(success ((,class (:foreground ,ash-green))))
   `(warning ((,class (:foreground ,ash-yellow))))
   `(error ((,class (:foreground ,term12)))) ; Swapped for visibility
   `(match ((,class (:background ,ash-yellow :foreground ,bg :weight bold))))

   ;; --- Minibuffer & Search ---
   `(minibuffer-prompt ((,class (:foreground ,ash-cyan-bright :weight bold))))
   `(isearch ((,class (:background ,ash-yellow :foreground ,bg :weight bold))))
   `(isearch-fail ((,class (:background ,term9 :foreground ,term15 :weight bold))))
   `(lazy-highlight ((,class (:background ,term1 :foreground ,ash-yellow-bright))))

   ;; --- Syntax Highlighting (Font-lock) ---
   `(font-lock-builtin-face ((,class (:foreground ,ash-cyan-bright :weight bold))))
   `(font-lock-comment-face ((,class (:foreground ,ash-gray :slant italic))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,outline-variant))))
   `(font-lock-constant-face ((,class (:foreground ,ash-blue :weight bold))))
   `(font-lock-doc-face ((,class (:foreground ,term7 :slant italic))))
   `(font-lock-function-name-face ((,class (:foreground ,ash-cyan-bright :weight bold))))
   `(font-lock-keyword-face ((,class (:foreground ,ash-yellow :weight bold))))
   `(font-lock-string-face ((,class (:foreground ,ash-green))))
   `(font-lock-type-face ((,class (:foreground ,term15 :slant italic))))
   `(font-lock-variable-name-face ((,class (:foreground ,fg))))
   `(font-lock-warning-face ((,class (:foreground ,ash-yellow :weight bold))))
   `(font-lock-preprocessor-face ((,class (:foreground ,ash-teal))))
   `(font-lock-negation-char-face ((,class (:foreground ,ash-cyan-bright))))

   ;; --- Paren Match ---
   `(show-paren-match ((,class (:background ,region-bg :foreground ,term15 :weight bold))))
   `(show-paren-mismatch ((,class (:background ,term9 :foreground ,fg :weight bold))))
   `(sp-show-pair-match-face ((,class (:background ,region-bg :foreground ,term15))))
   `(sp-show-pair-mismatch-face ((,class (:background ,term9))))

   ;; --- Modeline (Using elevated contrasting layers) ---
   `(mode-line ((,class (:background ,surface-container :foreground ,term15 :box nil))))
   `(mode-line-inactive ((,class (:background ,surface :foreground ,ash-gray :box nil))))
   `(mode-line-buffer-id ((,class (:weight bold :foreground ,ash-cyan-bright))))
   `(mode-line-emphasis ((,class (:foreground ,ash-cyan-bright :weight bold))))
   `(mode-line-highlight ((,class (:foreground ,term15 :box nil))))

   ;; --- Org Mode ---
   `(org-level-1 ((,class (:foreground ,ash-yellow :weight bold :height 1.2))))
   `(org-level-2 ((,class (:foreground ,ash-cyan-bright :weight bold :height 1.1))))
   `(org-level-3 ((,class (:foreground ,ash-magenta :weight bold))))
   `(org-level-4 ((,class (:foreground ,ash-blue :weight bold))))
   `(org-level-5 ((,class (:foreground ,ash-green :weight bold))))
   `(org-level-6 ((,class (:foreground ,ash-cyan :weight bold))))
   `(org-level-7 ((,class (:foreground ,ash-red-alt :weight bold))))
   `(org-level-8 ((,class (:foreground ,term7 :weight bold))))
   `(org-document-title ((,class (:foreground ,fg :weight bold :height 1.3))))
   `(org-document-info ((,class (:foreground ,ash-blue))))
   `(org-todo ((,class (:background ,surface :foreground ,term12 :weight bold))))
   `(org-done ((,class (:background ,surface :foreground ,ash-green :weight bold))))
   `(org-headline-done ((,class (:foreground ,ash-gray))))
   `(org-hide ((,class (:foreground ,bg))))
   `(org-ellipsis ((,class (:foreground ,ash-blue :underline nil))))
   `(org-table ((,class (:background ,surface-low :foreground ,ash-magenta :inherit fixed-pitch))))
   `(org-formula ((,class (:foreground ,ash-yellow-bright :inherit fixed-pitch))))
   `(org-checkbox ((,class (:foreground ,ash-cyan-bright :weight bold :inherit fixed-pitch))))
   `(org-date ((,class (:foreground ,ash-teal :underline t))))
   `(org-special-keyword ((,class (:foreground ,ash-gray :slant italic))))
   `(org-tag ((,class (:background ,surface-low :foreground ,fg))))
   `(org-block ((,class (:background ,surface-low :extend t :inherit fixed-pitch))))
   `(org-block-begin-line ((,class (:background ,surface-low :foreground ,ash-teal :extend t :slant italic :inherit fixed-pitch))))
   `(org-block-end-line ((,class (:background ,surface-low :foreground ,ash-teal :extend t :slant italic :inherit fixed-pitch))))
   `(org-code ((,class (:background ,surface-low :foreground ,ash-yellow-bright :inherit fixed-pitch))))
   `(org-verbatim ((,class (:background ,surface-low :foreground ,ash-cyan :inherit fixed-pitch))))

   ;; --- Magit ---
   `(magit-section-highlight ((,class (:background ,surface))))
   `(magit-diff-hunk-heading ((,class (:background ,surface :foreground ,term7))))
   `(magit-diff-hunk-heading-highlight ((,class (:background ,surface-container :foreground ,fg))))
   `(magit-diff-context ((,class (:foreground ,ash-gray))))
   `(magit-diff-context-highlight ((,class (:background ,surface-low :foreground ,fg))))
   `(magit-diff-added ((,class (:background ,term1 :foreground ,ash-green))))
   `(magit-diff-added-highlight ((,class (:background ,term2 :foreground ,ash-green-bright :weight bold))))
   `(magit-diff-removed ((,class (:background ,term1 :foreground ,term9))))
   `(magit-diff-removed-highlight ((,class (:background ,term3 :foreground ,term12 :weight bold))))
   `(magit-hash ((,class (:foreground ,ash-gray))))
   `(magit-branch-local ((,class (:foreground ,ash-blue :weight bold))))
   `(magit-branch-remote ((,class (:foreground ,ash-cyan :weight bold))))

   ;; --- Completion (Company / Corfu) ---
   `(company-tooltip ((,class (:background ,surface-container :foreground ,fg))))
   `(company-tooltip-selection ((,class (:background ,region-bg :foreground ,term15))))
   `(company-tooltip-common ((,class (:foreground ,ash-cyan-bright))))
   `(company-tooltip-common-selection ((,class (:foreground ,term15 :weight bold))))
   `(company-tooltip-annotation ((,class (:foreground ,ash-yellow))))
   `(company-scrollbar-fg ((,class (:background ,ash-cyan-bright))))
   `(company-scrollbar-bg ((,class (:background ,border))))
   `(corfu-default ((,class (:background ,surface-container :foreground ,fg))))
   `(corfu-current ((,class (:background ,region-bg :foreground ,term15))))

   ;; --- IDO / Helm ---
   `(ido-first-match ((,class (:foreground ,ash-cyan-bright :weight bold))))
   `(ido-only-match ((,class (:foreground ,fg :weight bold))))
   `(ido-subdir ((,class (:foreground ,fg :weight bold))))
   `(helm-selection ((,class (:background ,region-bg :foreground ,term15))))
   `(helm-match ((,class (:foreground ,ash-cyan-bright :weight bold))))

   ;; --- Which-Key ---
   `(which-key-key-face ((,class (:foreground ,ash-cyan-bright :weight bold))))
   `(which-key-separator-face ((,class (:foreground ,outline-variant))))
   `(which-key-command-description-face ((,class (:foreground ,fg))))
   `(which-key-group-description-face ((,class (:foreground ,ash-blue))))

   ;; --- Line Numbers ---
   `(line-number ((,class (:foreground ,ash-gray :inherit default))))
   `(line-number-current-line ((,class (:foreground ,ash-cyan-bright :weight bold :inherit default))))

   ;; --- Rainbow Delimiters ---
   `(rainbow-delimiters-depth-1-face ((,class (:foreground ,ash-cyan-bright))))
   `(rainbow-delimiters-depth-2-face ((,class (:foreground ,ash-yellow))))
   `(rainbow-delimiters-depth-3-face ((,class (:foreground ,ash-green))))
   `(rainbow-delimiters-depth-4-face ((,class (:foreground ,ash-blue))))
   `(rainbow-delimiters-depth-5-face ((,class (:foreground ,ash-magenta))))
   `(rainbow-delimiters-depth-6-face ((,class (:foreground ,term7))))
   `(rainbow-delimiters-depth-7-face ((,class (:foreground ,ash-yellow-bright))))
   `(rainbow-delimiters-depth-8-face ((,class (:foreground ,ash-green-bright))))
   `(rainbow-delimiters-depth-9-face ((,class (:foreground ,term13))))

   ;; --- Tabs / Centaur Tabs ---
   `(tab-bar ((t (:background ,surface :foreground ,fg :box nil))))
   `(tab-line ((t (:background ,surface :foreground ,fg :box nil))))
   `(tab-bar-tab ((t (:background ,region-bg :foreground ,term15 :box nil))))
   `(tab-bar-tab-inactive ((t (:background ,surface :foreground ,ash-gray :box nil))))
   `(centaur-tabs-default ((t (:background ,surface :foreground ,fg :box nil))))
   `(centaur-tabs-selected ((t (:background ,region-bg :foreground ,term15 :box nil))))
   `(centaur-tabs-unselected ((t (:background ,surface :foreground ,ash-gray :box nil))))
   `(centaur-tabs-selected-modified ((t (:background ,surface :foreground ,term12 :box nil))))
   `(centaur-tabs-active-bar-face ((t (:background ,ash-yellow :box nil))))

   ;; --- Flycheck ---
   `(flycheck-error ((,class (:underline (:style wave :color ,term12)))))
   `(flycheck-warning ((,class (:underline (:style wave :color ,ash-yellow)))))
   `(flycheck-info ((,class (:underline (:style wave :color ,ash-blue)))))

   ;; --- Pitch Fixes ---
   `(fixed-pitch ((t (:family "monospace"))))
   `(fixed-pitch-serif ((t (:family "monospace serif"))))
   `(variable-pitch ((t (:family "sans serif"))))
   ))

;; Star visibility hacks for clean minimal aesthetics
(with-eval-after-load 'org
  (setq org-hide-leading-stars t)
  (setq org-startup-indented t))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'ashwal)
;;; ashwal-theme.el ends here
