;; Load all essential plugins/files
;; kgaipal@gmail.com

(require 'package)

;; MELPA packages
;; http://stable.melpa.org/#/getting-started
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; ;; Marmalade packages
;; ;; https://marmalade-repo.org/#download
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/"))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Some helpful functions which are not defined in this file because they are too big
(load-file "dot-emacs-extensions.el")

;; GNU Coding style
(load-file "gnu-coding-style.el")

;; Customization in mode-line format
(load-file "mode-line-customization.el")

;; Load these files last, as they have my custom code/shortcuts which may be
;; overridden from above packages if this is loaded last
(load-file "keyboard-shortcuts.el")
