;;; init.el ---  My init file
;;; Commentary:
;;; My Emacs vanila init file

;;; Code:
(global-set-key [remap list-buffers] 'ibuffer)
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(fido-mode)
(desktop-save-mode 1)

(setq mac-command-modifier      'meta
      mac-option-modifier       'alt
      mac-right-option-modifier nil)

(setq tab-always-indent 'complete)
(setq completion-styles '(flex basic partial-completion require))

(global-display-line-numbers-mode)
(setq display-line-numbers 'relative)

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

(straight-use-package 'use-package)

(use-package treesit-auto
  :straight (treesit-auto :type git :host github :repo "renzmann/treesit-auto")
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package treesit-fold
  :straight (teesit-fold :type git :host github :repo "emacs-tree-sitter/treesit-fold")
  :init (global-treesit-fold-mode)
  (global-treesit-fold-indicators-mode)
  )

(use-package emacs
  :ensure nil
  :init
  (set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 100))

(use-package eglot
  :hook (prog-mode . eglot-ensure)
  :init (setq eglot-stay-out-of '(flymake))
  :bind (:map eglot-mode-map
           ("C-c a" . eglot-code-actions)
           ("C-c o" . eglot-code-actions-organize-imports)
           ("C-c r" . eglot-rename)
           ("C-c f" . eglot-format)
	   ("C-c d" . eldoc)))

(use-package eldoc
  :init
  (global-eldoc-mode))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))
;;; init.el ends here
