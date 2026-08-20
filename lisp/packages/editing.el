;;; editing.el --- Packages related to editing -*- lexical-binding: t; -*-
;;; Commentary:
;; Packages and configuration that makes editing that much better

;;; Code:

(use-package treesit-auto
  :ensure t
  :straight t
  :after emacs
  :custom
  (treesit-auto-install 'prompt)
  :config
  (require 'treesit-auto)
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode t))

(use-package undo-tree
  :defer t
  :ensure t
  :straight t
  :hook
  (after-init . global-undo-tree-mode)
  :init
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t
        undo-limit 800000                     ;; Limit for undo entries.
        undo-strong-limit 12000000            ;; Strong limit for undo entries.
        undo-outer-limit 120000000)           ;; Outer limit for undo entries.
  :config
  ;; Set the directory where `undo-tree' will save its history files.
  ;; This keeps undo history across sessions, stored in a cache directory.
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/.cache/undo"))))


;; code folding
(use-package kirigami
  :ensure t
  :config)

;; global formatting
(use-package apheleia
  :ensure t
  :config
  ;; Add nixfmt support
  (setf (alist-get 'nixfmt apheleia-formatters) '("nixfmt"))
  (setf (alist-get 'nix-mode apheleia-mode-alist) 'nixfmt)
  (setf (alist-get 'nix-ts-mode apheleia-mode-alist) 'nixfmt)
  (apheleia-global-mode 1))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default fill-column 80)
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(setq display-line-numbers-type 'relative)
(global-visual-line-mode t)
(setq delete-by-moving-to-trash t)
(setq auto-save-default t)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'hs-minor-mode)

(with-eval-after-load 'electric
  (setq electric-pair-preserve-balance t)
  (setq electric-pair-inhibit-predicate #'electric-pair-conservative-inhibit)
  (setq electric-pair-pairs
        '((?\" . ?\")
          (?\` . ?\`)
          (?\( . ?\))
          (?\[ . ?\])
          (?\{ . ?\}))))
(electric-pair-mode 1)

;; fix <> closing automatically in org-mode
(with-eval-after-load 'org
  (add-hook 'org-mode-hook
            (lambda ()
              (setq-local electric-pair-inhibit-predicate
                          (lambda (c)
                            (if (char-equal c ?<)
                                t
                              (electric-pair-conservative-inhibit c)))))))

(with-eval-after-load 'paren
  (setq show-paren-delay 0)
  (setq show-paren-style 'parenthesis)
  (setq show-paren-when-point-inside-paren t)
  (setq show-paren-when-point-in-periphery t))
(show-paren-mode 1)

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Elisp
(use-package aggressive-indent
  :ensure t
  :hook (emacs-lisp-mode . aggressive-indent-mode))

(use-package highlight-defined
  :ensure t
  :hook (emacs-lisp-mode . highlight-defined-mode))

(use-package elisp-refs
  :ensure t)

(provide 'editing)

;; PDF
(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))

(use-package org-noter
  :ensure t
  :after org
  :config
  (org-noter-enable-org-roam-integration)
  (setq org-noter-highlight-selected-text t)
  (setq org-noter-max-short-selected-text-length 80)
  (setq org-noter-always-create-frame nil)
  (setq org-noter-kill-frame-at-session-end t)
  (setq org-noter-auto-save-last-location t)
  (setq org-noter-notes-window-location 'horizontal-split))

;;; editing.el ends here
