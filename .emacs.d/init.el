;;; init.el --- thekuwayama
;;; Commentary
;;; Code:



;;; not to make backup-files like 'hoge.txt~'
(setq make-backup-files nil)

;;; not to display startup-message
(setq inhibit-startup-message t)

;;; Tab = Space * 4
(setq tab-width 4)
(setq-default indent-tabs-mode nil)

;;; scroll
(setq scroll-step 1)

;;; Japanese
(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)



;;; package
(require 'package)

(package-initialize)
(setq package-archives
      '(("org" . "https://orgmode.org/elpa/")
        ("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")))

(unless package-archive-contents
  (package-refresh-contents))

(setq custom-file
      (expand-file-name "package-selected-packages.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))



;;; use-package
(when (not (package-installed-p 'use-package))
  (package-install 'use-package))

(require 'use-package)



;;; ruby
(use-package ruby-mode
  :ensure t
  :mode ("\\.rb$" "\\.gemspec$" "Rakefile" "Gemfile")
  :interpreter "ruby"
  :commands ruby-mode
  :config
  (add-hook 'ruby-mode-hook #'lsp)

  (defun my-ruby-mode-hook ()
    (use-package ruby-end)
    (use-package rubocop)
    (use-package ruby-block
      :config
      (ruby-block-mode t)
      (setq ruby-block-highlight-toggle t))))

;;; golang
(use-package go-mode
  :ensure t
  :mode "\\.go$"
  :commands go-mode
  :config
  (add-hook 'go-mode-hook #'lsp)
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package flycheck-golangci-lint
  :ensure t
  :if (executable-find "golangci-lint")
  :hook (go-mode . flycheck-golangci-lint-setup))

;;; scala
(use-package scala-mode
  :ensure t
  :mode "\\.s\\(cala\\|bt\\)$"
  :commands scala-mode
  :config (add-hook 'scala-mode-hook #'lsp))

(use-package lsp-metals
  :config
  (setq lsp-metals-treeview-show-when-views-received t))

;;; rust
(use-package rust-mode
  :ensure t
  :mode "\\.rs$"
  :commands rust-mode
  :custom rust-format-on-save t
  :config (add-hook 'rust-mode-hook #'lsp))

(add-to-list 'exec-path (expand-file-name "$HOME/.cargo/bin"))



;;; Language Server
(setq lsp-enable-file-watchers nil)

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :custom (lsp-rust-server 'rust-analyzer))

(use-package lsp-ui
  :ensure t)

(use-package company-lsp
  :ensure t)



;;; jump
(use-package dumb-jump
  :ensure t
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g b" . dumb-jump-back))
  :config (setq dumb-jump-selector 'helm))
