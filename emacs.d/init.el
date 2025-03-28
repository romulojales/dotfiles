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
  (set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 115))

(use-package eglot
  :hook ((prog-mode . eglot-ensure)
	 (yaml-ts-mode . eglot-ensure))
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

(use-package exec-path-from-shell
  :ensure t
  :straight (exec-path-from-shell)
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  )

(use-package envrc
  :ensure t
  :straight (envrc)
  :hook (after-init . envrc-global-mode))

(use-package flycheck
  :ensure t
  :straight (flycheck)
  :init (global-flycheck-mode)
  )

(use-package company
  :ensure t
  :straight (company)
  :hook (prog-mode . company-mode))

(use-package yasnippet
  :ensure t
  :straight (yasnippet)
  :config (yas-global-mode t)
  )

(use-package flycheck-eglot
  :ensure t
  :straight (flycheck-eglot :type git :host github :repo "flycheck/flycheck-eglot")
  :after (flycheck eglot)
  :config
   (global-flycheck-eglot-mode 1)
   )

(use-package magit
  :ensure t
  :straight (magit)
  )

(use-package all-the-icons
  :ensure t
  :straight (all-the-icons)
  :if (display-graphic-p))

(use-package treemacs
  :ensure t
  :straight (treemacs)
  :hook ((treemacs-mode . treemacs-project-follow-mode)
         (emacs-startup . treemacs))
  :bind ("<f5>" . treemacs)
  :custom
  (treemacs-hide-dot-git-directory nil)
  (treemacs-is-never-other-window t)
  (treemacs-project-follow-into-home t))



;;; init.el ends here
