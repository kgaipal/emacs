;; Load all essential plugins/files
;; kgaipal@gmail.com

;; MELPA packages
;; http://stable.melpa.org/#/getting-started
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; GNU Coding style
(load-file "~/.emacs.d/gnu-coding-style.el")

;; Some helpful functions which are not defined in this file because they are too big
(load-file "~/.emacs.d/dot-emacs-extensions.el")

;; Customization in mode-line format
(load-file "~/.emacs.d/mode-line-customization.el")

;; Load these files last, as they have my custom code/shortcuts which may be
;; overridden from above packages if this is loaded last
(load-file "~/.emacs.d/keyboard-shortcuts.el")
