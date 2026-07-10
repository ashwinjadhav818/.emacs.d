;;; completion.el --- completion stack -*- lexical-binding: t; -*-
;;; Code:

(use-package vertico
  :ensure t
  :init
  (vertico-mode 1)
  :custom
  (vertico-cycle t))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode 1))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :ensure t
  :defer t
  :init
  (setq xref-show-xrefs-function    #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :bind
  (("C-x b"   . consult-buffer)
   ("C-x C-f" . consult-find)
   ("M-g g"   . consult-goto-line)
   ("M-s r"   . consult-ripgrep)
   ("M-s l"   . consult-line)
   ("M-s f"   . consult-fd)
   ("M-y"     . consult-yank-pop))
  :config
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   :preview-key '(:debounce 0.4 any)))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package nerd-icons-completion
  :ensure t
  :demand t
  :config
  (nerd-icons-completion-mode)
  (nerd-icons-completion-marginalia-setup))

(use-package corfu
  :ensure t
  :custom
  (corfu-auto        t)
  (corfu-auto-delay  0)
  (corfu-preview-current t)
  (corfu-auto-prefix 1)
  (corfu-cycle        t)
  (corfu-quit-no-match t)

  ;; Tab-and-Go settings so TAB behaves properly with indentation
  (corfu-preselect 'valid) ;; Don't automatically select the first item violently

  :bind
  (:map corfu-map
        ;; Make TAB fall back to normal indentation if we haven't selected anything
        ("<tab>" . corfu-insert)
        ("TAB" . corfu-insert)
        ;; Use Enter/Return to confirm a selection instead of TAB hijacking it
        ("<return>" . corfu-insert)
        ("RET" . corfu-insert)
        ;; Use arrow keys, C-n/C-p, or M-n/M-p to move through the menu smoothly
        ("M-n" . corfu-next)
        ("M-p" . corfu-previous))
  :config
  (corfu-popupinfo-mode)
  :init
  (global-corfu-mode))

;; remove ispell word completion
(setq text-mode-ispell-word-completion nil)

;; Global fallback capfs for non-eglot buffers.
;; eglot-managed buffers get their own stack rebuilt in grammars.el.
(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block))

(provide 'completion)
