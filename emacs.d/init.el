;;; init.el --- Rômulo Jales's init.el file -*- lexical-binding: t -*-
;;; Commentary:
;;; My Emacs' init file.
;;; Two main guidelines:
;;; 1 - Keep things documented and with references
;;; 2 - Use the most of Emacs offers out of the box.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A home for the customizations
;; https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Saving-Customizations
(setq custom-file "~/.emacs.d/emacs-custom.el")
;; Loading all runtime customizations
(load custom-file 'noerror) ; noerror for when it is the very first
			    ; time running this init.el emacs


;; Where to save the backups
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Backup.html
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Look'n'feel

;; not showing the startup page. Also disables splash-screen
(setq inhibit-startup-message t)

;; How autocomplete should work
(setq completion-styles '(flex basic partial-completion ))

;; Disables the scroll bars, both horizontal and vertical. It saves
;; some screen space
(scroll-bar-mode -1)

;; no need for top tool bar.q
(tool-bar-mode -1)

;; Make the screen maximized not full-screen
(toggle-frame-maximized)

;; Display line numbers in all buffers
(global-display-line-numbers-mode)

;; Highlitght the current line
(global-hl-line-mode)

;; saves the current state of emacs
(desktop-save-mode 1)

;; use y or n instead of yes or no.
(setopt use-short-answers t)

;; some space between the lines
(setq line-spacing 0.4)

;; Disabling this key because ctrl-z in emacs puts the emacs in background
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme
(use-package modus-themes
  :ensure t
  :config
  (load-theme 'modus-operandi t))

; Custom configurations for MacOS
(when (eq system-type 'darwin)
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Mac-_002f-GNUstep-Customization.html#Modifier-keys
  ;;  disabling right option as META modifier this will allow us to
  ;;  use accent and write other characteres.
  (setq ns-right-alternate-modifier 'none
	ns-right-command-modifier 'super
	ns-alternate-modifier 'alt
	ns-command-modifier 'meta))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vertico - a completion for the minibuffer system which presents the options in
;; vertical
;; Https://github.com/minad/vertico
(use-package vertico
  :ensure t
  :hook
  (vertico-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIT
;; Magit - https://magit.vc
;; Install and configure magit

;; magit dependencies
(use-package compat
  :ensure t)

(use-package llama
  :ensure t)

(use-package cond-let
  :ensure t
  :vc (:url "https://github.com/tarsius/cond-let" :rev "v0.1.0"))

(use-package transient
  :ensure t)

(use-package with-editor
  :ensure t)

;; Magit
(use-package magit
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eglot

(use-package eglot
  :hook ((prog-mode . eglot-ensure)
	 (yaml-ts-mode . eglot-ensure))
  :bind (:map eglot-mode-map
           ("C-c a" . eglot-code-actions)
           ("C-c o" . eglot-code-actions-organize-imports)
           ("C-c r" . eglot-rename)
           ("C-c f" . eglot-format)
	   ("C-c d" . eldoc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flymake
(use-package flymake
  :ensure t
)

;; An autocomplete system
(use-package corfu
  :ensure t
  :config (global-corfu-mode))

(use-package orderless
  :ensure t
  :config
  (setq orderless-smart-case nil)
  (setq completion-styles '(orderless basic))
  (setq completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :ensure t
  :hook (marginalia-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Docker
(use-package dockerfile-mode
  :ensure t)

;;; init.el ends here
