;;; packages.el --- Setup Packages -*- lexical-binding: t; -*-
;;; Commentary:
;; Setup straight.el and package.el

;;; Code:

(setq package-enable-at-startup nil)

;; Bootstraps `straight.el'
(setq straight-check-for-modifications nil)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package '(project :type built-in))
(straight-use-package 'use-package)


(require 'package)

;; Add MELPA (Milkypostman's Emacs Lisp Package Archive) to the list of package archives.
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Define a global customizable variable `ek-use-nerd-fonts' to control the use of
(defcustom ek-use-nerd-fonts t
  "Configuration for using Nerd Fonts Symbols."
  :type 'boolean
  :group 'appearance)

(provide 'packages)
;;; packages.el ends here
