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

;;; disable line-wrapping
(set-default 'truncate-lines t)
(setq truncate-partial-width-windows t)

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
  (add-hook 'ruby-mode-hook 'company-mode)

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
  (add-hook 'before-save-hook #'gofmt-before-save)
  (add-hook 'go-mode-hook 'company-mode)
  (add-hook 'go-mode-hook #'lsp))

(setq gofmt-command "goimports")

(use-package flycheck-golangci-lint
  :ensure t
  :if (executable-find "golangci-lint")
  :hook (go-mode . flycheck-golangci-lint-setup))

;;; scala
(use-package scala-mode
  :ensure t
  :mode "\\.s\\(cala\\|bt\\)$"
  :commands scala-mode
  :config
  (add-hook 'scala-mode-hook #'lsp)
  (add-hook 'scala-mode-hook 'company-mode))

(use-package lsp-metals
  :config
  (setq lsp-prefer-flymake nil)
  (setq lsp-metals-treeview-show-when-views-received t)
  :hook (lsp-mode . lsp-lens-mode))

;;; rust
(use-package rust-mode
  :ensure t
  :config
  (setq rust-format-on-save t)
  (add-hook 'rust-mode-hook #'lsp)
  (add-hook 'rust-mode-hook 'company-mode))

(use-package flycheck-rust)

(add-to-list 'exec-path (expand-file-name "$HOME/.cargo/bin"))

;;; ts
(use-package web-mode
  :ensure t
  :mode "\\.[jt]sx?")

;;; python
(use-package python-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook #'lsp))



;;; Language Server
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :custom
  (lsp-rust-server 'rust-analyzer)
  (lsp-enable-file-watchers nil)
  (lsp-file-watch-threshold 2000))

(use-package lsp-ui
  :ensure t)



;;; jump
(use-package rg
  :ensure t
  :config (rg-enable-default-bindings))

  

;;; company
(use-package company
  :ensure t)



;;; snippet
(use-package yasnippet)



;;; github-browse-file
(use-package github-browse-file
  :config
  (setq github-browse-file-show-line-at-point t))
