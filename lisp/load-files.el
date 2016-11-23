;; Load all essential plugins/files
;; kgaipal@gmail.com

;; Moves the buffer from one side to other
;; [http://stackoverflow.com/questions/1774832/how-to-swap-the-buffers-in-2-windows-emacs]
(load-file "~/.emacs.d/external/buffer-move.el")
(require 'buffer-move)

;; GNU Coding style
(load-file "~/.emacs.d/gnu-coding-style.el")

;; Highlight current word under the cursor
;; [http://xahlee.blogspot.com/2010/05/emacs-isearch-of-current-work.html]
(load-file "~/.emacs.d/external/highlight-symbol.el")
(require 'highlight-symbol)

;; ;; xcscope for searching in large code trees
;; ;; [https://github.com/dkogan/xcscope.el]
;; (load-file "~/.emacs.d/external/xcscope.el")
;; (require 'xcscope)
;; (cscope-setup)

;; Some helpful functions which are not defined in this file because they are too big
(load-file "~/.emacs.d/util-functions.el")

;; Customization in mode-line format
(load-file "~/.emacs.d/mode-line-customization.el")

;; Load these files last, as they have my custom code/shortcuts which may be
;; overridden from above packages if this is loaded last
(load-file "~/.emacs.d/keyboard-shortcuts.el")

;; C# mode
(load-file "~/.emacs.d/external/csharp-mode.el")
(flymake-mode-off)            ; disable flymake-mode as this doesnt work on windows anyway

;; Microsoft TFS Server plugin for VC
(load-file "~/.emacs.d/external/tfs.el")
(require 'tfs)
