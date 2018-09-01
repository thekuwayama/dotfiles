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
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

(require 'cl)
(defvar my/favorite/el
  '(
    auto-complete
    popup
    flycheck
    markdown-mode
    yaml-mode
    undo-tree
    ))

(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(dolist (package my/favorite/el)
  (unless (package-installed-p package)
    (package-install package)))
