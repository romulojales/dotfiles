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
(setopt line-spacing 0.4)

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
           ("C-c o" . eglot-code-action-organize-imports)
           ("C-c r" . eglot-rename)
           ("C-c f" . eglot-format)
	   ("C-c d" . eldoc)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Treesit
;; map files to <lang>-ts-mode
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))
;; treesit-fold - a package to fold and unfold  *-ts-mode buffers
;; https://github.com/emacs-tree-sitter/treesit-fold
(use-package treesit-fold
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Linter
;; Flymake
(use-package flymake
  :ensure t
)

;; An autocomplete system
(use-package corfu
  :ensure t
  :config (global-corfu-mode))

;; When suggesting, don't care about the order of the workd
(use-package orderless
  :ensure t
  :config
  (setq orderless-smart-case nil)
  (setq completion-styles '(orderless basic))
  (setq completion-category-overrides '((file (styles basic partial-completion)))))

;; when on the minibuffer, Adds information about the command that one is typing
(use-package marginalia
  :ensure t
  :init (marginalia-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Terraform

;; Hcl is a mode for edding terraform configuring files
(use-package hcl-mode
  :ensure t
  :vc (:url "https://github.com/hcl-emacs/hcl-mode"
	    :rev "b2a03a446c1fe324ff494c28b9321486fa6fc672"))

;; the terraform mode
(use-package terraform-mode
  :ensure t
  :vc (:url "https://github.com/hcl-emacs/terraform-mode" :rev "v1.1.0"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Docker
;; Schema highligt 
(use-package dockerfile-mode
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Envrc - A package to configure per project enviroments
;; "https://github.com/purcell/envrc

;; dependency
(use-package inheritenv
  :ensure t
  :vc (:url "https://github.com/purcell/inheritenv" :rev "0.2"))

;; the package
(use-package envrc
  :ensure t
  :vc (:url "https://github.com/purcell/envrc" :rev "0.12")
  :hook (after-init . envrc-global-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editorconfig - coding styles
;; https://editorconfig.org/ 
(use-package editorconfig
  :ensure t
  :hook (after-init))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Init.el ends here
;; Spell check
(use-package flyspell
  :ensure t
  :config (flyspell-mode))

;;; init.el ends here
