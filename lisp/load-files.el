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

;; Install external packages from MELPA or M-x list-packages on a new emacs installation
;; see variable package-selected-packages
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))

(require 'use-package)

;; TODO: make this auto iterate package list from 'package-selected-packages and install all
;; TODO: turn off read only mode (view-mode) so that elpa can download
(use-package buffer-move :ensure buffer-move)
(use-package csharp-mode :ensure csharp-mode)
(use-package highlight-symbol :ensure highlight-symbol)
(use-package tfs :ensure tfs)
(use-package unbound :ensure unbound)
