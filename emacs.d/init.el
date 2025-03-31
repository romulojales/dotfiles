;;; init.el ---  My init file
;;; Commentary:
;;; My Emacs vanila init file

;;; Code:

;; General configurations

;; When listing buffers, use ibuffer.
(global-set-key [remap list-buffers] 'ibuffer)

;; Not display the startup message. go direct to coding
(setq inhibit-startup-message t)
;; Not display the tool bar
(tool-bar-mode -1)

;; Not display the scroll bar
(toggle-scroll-bar -1)

;; How auto-complete should work.
(fido-mode)

;; saves the current state of emacs
(desktop-save-mode 1)

;; controlling some attributes based on operating system.
(when (eq system-type 'darwin)
  ;; Mapping mac keyboard keys.
   (setq mac-command-modifier      'meta
	 mac-option-modifier       'alt
	 mac-right-option-modifier nil)
   
   ;; ls in macos does not have --dired option
   (setq-default dired-use-ls-dired nil)
   )


(setq tab-always-indent 'complete)
(setq completion-styles '(flex basic partial-completion require))

(global-display-line-numbers-mode)

;; configuring straight
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

;; Eldoc is a documentation minibuffer.
(use-package eldoc
  :init
  (global-eldoc-mode))

(use-package treesit-auto
  :straight (treesit-auto :type git :host github :repo "renzmann/treesit-auto")
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;; 
(use-package treesit-fold
  :straight (teesit-fold :type git :host github :repo "emacs-tree-sitter/treesit-fold")
  :init (global-treesit-fold-mode)
  (global-treesit-fold-indicators-mode)
  )

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
  :defer t
  :straight (treemacs)
  :hook ((treemacs-mode . treemacs-project-follow-mode)
         (emacs-startup . treemacs))
  :bind ("<f5>" . treemacs)
  :custom
  (treemacs-hide-dot-git-directory nil)
  (treemacs-is-never-other-window t)
  (treemacs-project-follow-into-home t))

(use-package treemacs-all-the-icons
  :ensure t
  :straight (treemacs-all-the-icons)
  :config
  (treemacs-load-theme "all-the-icons")
  )

(use-package treemacs-magit
  :straight (treemacs-magit)
  :after (treemacs magit)
  :ensure t)

(use-package all-the-icons-ibuffer
  :ensure t
  :straight (all-the-icons-ibuffer)
  :defer
  :custom
  (all-the-icons-ibuffer-formats
   `((mark modified read-only locked vc-status-mini
           ;; Here you may adjust by replacing :right with :center or :left
           ;; According to taste, if you want the icon further from the name
           " " ,(if all-the-icons-ibuffer-icon
                    '(icon 2 2 :left :elide)
                  "")
           ,(if all-the-icons-ibuffer-icon
                (propertize " " 'display `(space :align-to 8))
              "")
           (name 18 18 :left :elide)
           " " (size-h 9 -1 :right)
           " " (mode+ 16 16 :left :elide)
           " " (vc-status 16 16 :left)
           " " vc-relative-file)
     (mark " " (name 16 -1) " " filename)))

  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))

(use-package ibuffer-vc
  :ensure t
  :straight (ibuffer-vc)
  :hook (ibuffer . (lambda ()
                     (ibuffer-vc-set-filter-groups-by-vc-root)
                     (unless (eq ibuffer-sorting-mode 'alphabetic)
                       (ibuffer-do-sort-by-vc-status)
                       )
                     )))

(use-package vertico
	     :ensure t
	     :straight (vertico)
	     :init
	     (vertico-mode)
	     )

(use-package vertico-posframe
  :ensure t
  :straight (vertico-posframe)
  :config (vertico-posframe-mode 1)
  :custom
  (vertico-posframe-parameters
   '((left-fringe . 8)
     (right-fringe . 8))))

(use-package git-gutter
  :ensure t
  :straight (git-gutter)
  :hook (after-init . global-git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.2)
  )

(use-package modus-themes
  :straight t
  :config
  (load-theme 'modus-operandi t))

(use-package marginalia
  :straight t
  :init
  (marginalia-mode))

(use-package emacs
  :ensure nil
  :init
  (set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 115)
  :custom
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

;;; init.el ends here
