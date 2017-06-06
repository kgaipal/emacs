;; dot-emacs file
;; kgaipal@gmail.com

(require 'package)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; MELPA and Marmalade packages
;; http://stable.melpa.org/#/getting-started
;; https://marmalade-repo.org/#download
(add-to-list 'package-archives
             ;; '("marmalade" . "http://marmalade-repo.org/packages/")
             ;; '("melpa-stable" . "https://stable.melpa.org/packages/") t)
             '("melpa" . "https://melpa.org/packages/"))


;; ================= Load Customizations ===============

;; customization functions and settings which are not defined in this file because they
;; are too big and too many and need explaination
(load-file "~/.emacs.d/dot-emacs-extensions.el")

;; emacs packages specific customization
(load-file "~/.emacs.d/packages-customization.el")

;; customization in mode-line format
(load-file "~/.emacs.d/mode-line-customization.el")

;; Note: Load these files last as they have my custom code/shortcuts which may be
;; overridden from above packages if this is loaded last
(load-file "~/.emacs.d/keyboard-shortcuts.el")

;; custom options
(setq custom-file "~/.emacs.d/custom-options.el")
(load custom-file)

;; platforms specic tweaks for themes;
;; doing this after theme is loaded
(when (or (eq system-type 'windows-nt) (eq system-type 'msdos))
  (set-face-attribute 'default nil :height 130 :family "Courier New"))
