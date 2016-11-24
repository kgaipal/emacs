;; Load all essential plugins/files
;; kgaipal@gmail.com

;; GNU Coding style
(load-file "~/.emacs.d/gnu-coding-style.el")

;; Some helpful functions which are not defined in this file because they are too big
(load-file "~/.emacs.d/dot-emacs-extensions.el")

;; Customization in mode-line format
(load-file "~/.emacs.d/mode-line-customization.el")

;; Load these files last, as they have my custom code/shortcuts which may be
;; overridden from above packages if this is loaded last
(load-file "~/.emacs.d/keyboard-shortcuts.el")
