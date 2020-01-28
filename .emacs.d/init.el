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
    auto-complete
    flycheck
    undo-tree
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
