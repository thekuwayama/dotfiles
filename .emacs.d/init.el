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
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("gnu" . "https://elpa.gnu.org/packages/") t)

(require 'cl)
(defvar my/favorite/el
  '(
    ruby-mode
    ruby-end
    rubocop
    go-mode
    rust-mode
    markdown-mode
    yaml-mode
    dockerfile-mode
    terraform-mode
    scala-mode
    protobuf-mode
    auto-complete
    flycheck
    undo-tree
    lsp-mode
    lsp-ui
    company-lsp
    ))

(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(dolist (package my/favorite/el)
  (unless (package-installed-p package)
    (package-install package)))

(setq custom-file
      (expand-file-name "package-selected-packages.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;; language server
(require 'lsp-mode)
(require 'lsp-ui)
(require 'company-lsp)

;;; https://github.com/emacs-lsp/lsp-mode#supported-languages
; gem install solargraph
(add-hook 'ruby-mode-hook #'lsp)  
; go get -u github.com/saibing/bingo
(add-hook 'go-mode-hook #'lsp)
; rustup update && rustup component add rls rust-analysis rust-src
(add-hook 'rust-mode-hook #'lsp)
; coursier bootstrap --java-opt -Xss4m --java-opt -Xms100m --java-opt -Dmetals.client=emacs org.scalameta:metals_2.12:0.8.0 -r bintray:scalacenter/releases -r sonatype:snapshots -o /usr/local/bin/metals-emacs -f
(add-hook 'scala-mode-hook #'lsp)
