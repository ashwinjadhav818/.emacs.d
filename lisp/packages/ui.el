;;; ui.el --- Packages related to ui -*- lexical-binding: t; -*-
;;; Commentary:
;; Packages and configuration that add ui and eye candy packages

;;; Code:

;;; ELDOC-BOX
(use-package eldoc-box
  :ensure t
  :straight t
  :defer t)


(use-package diff-hl
  :defer t
  :straight t
  :ensure t
  :hook
  (find-file . (lambda ()
                 (global-diff-hl-mode)
                 (diff-hl-flydiff-mode)
                 (diff-hl-margin-mode)))
  :custom
  (diff-hl-side 'left)
  (diff-hl-margin-symbols-alist '((insert . "┃")
                                  (delete . "-")
                                  (change . "┃")
                                  (unknown . "┆")
                                  (ignored . "i"))))

;;; RAINBOW DELIMITERS
(use-package rainbow-delimiters
  :defer t
  :straight t
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode))

;;; PULSAR
(use-package pulsar
  :defer t
  :straight t
  :ensure t
  :hook
  (after-init . pulsar-global-mode)
  :config
  (setq pulsar-pulse t)
  (setq pulsar-delay 0.025)
  (setq pulsar-iterations 10)
  (setq pulsar-face 'evil-ex-lazy-highlight)

  (add-to-list 'pulsar-pulse-functions 'evil-scroll-down)
  (add-to-list 'pulsar-pulse-functions 'flymake-goto-next-error)
  (add-to-list 'pulsar-pulse-functions 'flymake-goto-prev-error)
  (add-to-list 'pulsar-pulse-functions 'evil-yank)
  (add-to-list 'pulsar-pulse-functions 'evil-yank-line)
  (add-to-list 'pulsar-pulse-functions 'evil-delete)
  (add-to-list 'pulsar-pulse-functions 'evil-delete-line)
  (add-to-list 'pulsar-pulse-functions 'evil-jump-item)
  (add-to-list 'pulsar-pulse-functions 'diff-hl-next-hunk)
  (add-to-list 'pulsar-pulse-functions 'diff-hl-previous-hunk))

;;; DOOM MODELINE
(use-package doom-modeline
  :ensure t
  :straight t
  :defer t
  :custom
  (doom-modeline-buffer-file-name-style 'buffer-name)
  (doom-modeline-project-detection 'project)
  (doom-modeline-buffer-name t)
  (doom-modeline-vcs-max-length 25)
  :config
  (if ek-use-nerd-fonts
      (setq doom-modeline-icon t)
    (setq doom-modeline-icon nil))
  :hook
  (after-init . doom-modeline-mode))

;;; NEOTREE
(use-package neotree
  :ensure t
  :straight t
  :custom
  (neo-show-hidden-files t)
  (neo-theme 'nerd)
  (neo-vc-integration '(face char)) 
  :defer t
  :config
  (if ek-use-nerd-fonts
      (setq neo-theme 'nerd-icons)
    (setq neo-theme 'nerd)))

;;; DASHBOARD
;; An extensible emacs startup screen showing you what’s most important.
(use-package dashboard
  :straight t
  :config
  ;; Initialize
  (add-hook 'after-init-hook #'dashboard-insert-startupify-lists)
  (add-hook 'after-init-hook #'dashboard-initialize)

  ;; Set dashboard as startup screen
  (setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
  (dashboard-setup-startup-hook))

  ;; Config
  (setq dashboard-startup-banner "~/.emacs.d/icons/emacs.svg")    ; custom emacs icon
  (setq dashboard-image-banner-max-height 200)
  (setq dashboard-display-icons-p t)     ; display icons on both GUI and terminal
  (setq dashboard-icon-type 'nerd-icons) ; use `nerd-icons' package
  (setq dashboard-center-content t)
  (setq dashboard-items '((recents   . 5)
                        (projects  . 5)
                        (agenda    . 5)))

;;; NERD ICONS
(use-package nerd-icons
  :if ek-use-nerd-fonts
  :ensure t 
  :straight t
  :defer t) 


;;; NERD ICONS Dired
(use-package nerd-icons-dired
  :if ek-use-nerd-fonts
  :ensure t
  :straight t
  :defer t 
  :hook
  (dired-mode . nerd-icons-dired-mode))

;;; NERD ICONS COMPLETION
(use-package nerd-icons-completion
  :if ek-use-nerd-fonts
  :ensure t
  :straight t
  :after (:all nerd-icons marginalia)
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;;; CENTUAR TABS
;; The `centuar-tabs` package provides a sleek and modern tab bar
(use-package centaur-tabs
  :straight t
  :defer nil
  :config
  ;; Enable the tab bar
  (centaur-tabs-mode t)

  ;; Style and markers
  (setq centaur-tabs-style "zigzag")
  (setq centaur-tabs-set-bar 'under)
  (setq x-underline-at-descent-line t)
  (setq centaur-tabs-modified-marker "*")

  ;; Enable icons
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-icon-type 'nerd-icons)

  ;; Grouping behavior
  (setq centaur-tabs-group-by-projectile-project t)
  (setq centaur-tabs-headline-match t)

  "Hide centaur-tabs in the *dashboard* buffer."
  (defun my/dashboard-hide-tabs ()
  (when (string= (buffer-name) "*dashboard*")
    (centaur-tabs-local-mode t)))

  (add-hook 'dashboard-mode-hook #'my/dashboard-hide-tabs)


  :bind
  ("C-<tab>"   . centaur-tab-forward)
  ("C-S-<tab>" . centaur-tabs-backward))


;;; CENTERed-CURSOR-MODE
(use-package centered-cursor-mode
  :demand
  :config
  (global-centered-cursor-mode))

;;; PERFECT-MARGIN
(use-package perfect-margin
  :custom
  (perfect-margin-visible-width 128)
  :config
  ;; enable perfect-mode
  (perfect-margin-mode t)
  ;; auto-center minibuffer windows
  (setq perfect-margin-ignore-filters nil)
  ;; auto-center special windows
  (setq perfect-margin-ignore-regexps nil)
  ;; add additinal bding on margin area
  (dolist (margin '("<left-margin> " "<right-margin> "))
	(global-set-key (kbd (concat margin "<mouse-1>")) 'ignore)
	(global-set-key (kbd (concat margin "<mouse-3>")) 'ignore)
	(dolist (multiple '("" "double-" "triple-"))
	  (global-set-key (kbd (concat margin "<" multiple "wheel-up>")) 'mwheel-scroll)
	  (global-set-key (kbd (concat margin "<" multiple "wheel-down>")) 'mwheel-scroll))))

;; ULTRA-SCROLL
;; The `ultra-scroll' package enables smooth scroll.
(use-package ultra-scroll
  :init
  (setq scroll-conservatively 1
        scroll-margin 0)
  :config
    (ultra-scroll-mode 1))

;;; VERTICO
;; Vertico enhances the completion experience in Emacs by providing a
;; vertical selection interface for both buffer and minibuffer completions.
;; Unlike traditional minibuffer completion, which displays candidates
;; in a horizontal format, Vertico presents candidates in a vertical list,
;; making it easier to browse and select from multiple options.
(use-package vertico
  :ensure t
  :straight t
  :hook
  (after-init . vertico-mode)
  :custom
  (vertico-count 10)
  (vertico-resize nil)
  (vertico-cycle nil))

;;; ORDERLESS
;; Orderless enhances completion in Emacs by allowing flexible pattern matching.
;; It works seamlessly with Vertico, enabling you to use partial strings and
;; regular expressions to find files, buffers, and commands more efficiently.
;; This combination provides a powerful and customizable completion experience.
(use-package orderless
  :ensure t
  :straight t
  :defer t
  :after vertico
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))


;;; MARGINALIA
;; Marginalia enhances the completion experience in Emacs by adding
;; additional context to the completion candidates. This includes
;; helpful annotations such as documentation and other relevant
;; information, making it easier to choose the right option.
(use-package marginalia
  :ensure t
  :straight t
  :hook
  (after-init . marginalia-mode))


;;; CONSULT
;; Consult provides powerful completion and narrowing commands for Emacs.
;; It integrates well with other completion frameworks like Vertico, enabling
;; features like previews and enhanced register management. It's useful for
;; navigating buffers, files, and xrefs with ease.
(use-package consult
  :ensure t
  :straight t
  :defer t
  :init
  ;; Enhance register preview with thin lines and no mode line.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult for xref locations with a preview feature.
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))


;;; EMBARK
;; Embark provides a powerful contextual action menu for Emacs, allowing
;; you to perform various operations on completion candidates and other items.
;; It extends the capabilities of completion frameworks by offering direct
;; actions on the candidates.
(use-package embark
  :ensure t
  :straight t
  :defer t)


;;; EMBARK-CONSULT
;; Embark-Consult provides a bridge between Embark and Consult, ensuring
;; that Consult commands, like previews, are available when using Embark.
(use-package embark-consult
  :ensure t
  :straight t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)) ;; Enable preview in Embark collect mode.

(provide 'ui)
