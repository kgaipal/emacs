;; Some keyboard shorcuts I changed/assigned which are more intuitve for my current keyboard layout
;; Last edited on: 2011-09-14
;; by Kshitij Gaipal

;; Load this file in your ".emacs" file:
;; (load-file "~/.emacs.d/lisp/mykbd-codes.el")

;; Basic indentation
(global-set-key (kbd "<backtab>")'decrease-left-margin)
(global-set-key (kbd "<C-tab>") 'increase-left-margin)

;; Auto reload file by F5 keystroke
(defun refresh-file () 
  (interactive) (revert-buffer t t t))
(global-set-key [f5] 'refresh-file)

;; Jump to line number
;; http://geosoft.no/development/emacs.html
(global-set-key "\C-l" 'goto-line) ; [Ctrl]-[L]

;; Toggle between source/header files
(global-set-key [f4] 'ff-find-other-file)

;; Selecting all text 
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "C-x C-a") 'mark-whole-buffer)

(global-set-key (kbd "C-z") 'undo) ; Ctrl+z
(global-set-key (kbd "M-z") 'redo) ;  Ctrl+u

;; Kill whole line or word
(global-set-key (kbd "M-d") 'kill-whole-line)
(global-set-key (kbd "M-w") 'kill-word)

;; Kill buffer
;; Its painful to see keyboard macro message popping up always when you hit C-x k
(global-unset-key (kbd "C-x k"))
(global-set-key (kbd "M-k") 'kill-buffer)

;; A much more easier shortcut to invoke (keyboard-escape-quit) 
;; Its painful to see keyboard macro message popping up always when you hit C-x k
(global-set-key (kbd "M-g") 'keyboard-escape-quit)

;; Avoiding killing (or suspending) emacs accidently while
;; cut-copy-paste (or undo/redo) operations
(global-unset-key (kbd "C-x C-c"))
(global-unset-key (kbd "C-x C-z"))
(global-set-key (kbd "C-x C-q") 'save-buffers-kill-terminal)

;; ;; Allow selecting text with SHIFT+UP_ARROW
;; (global-set-key (kbd "<select>") 'previous-line)

;; Defining my own aliases.
;; Some usefull ones are copied from here:
;; http://xahlee.org/emacs/emacs_alias.html
;; (defalias 'list-buffers 'ibuffer)

;; IBuffer module provides better switching
;; http://www.emacswiki.org/emacs/IbufferMode
(require 'ibuf-ext)
(add-to-list 'ibuffer-never-show-predicates "^\\*") ;hiding the unnecessary buffers 
(global-set-key (kbd "M-b") 'ibuffer)
(global-set-key (kbd "M-o") 'iswitchb-buffer)

;; Marking a buffer as readonly
(global-set-key (kbd "M-r") 'toggle-read-only)

;; Open/Find file command can also be invoked with this now
(global-set-key (kbd "M-f") 'find-file)

;; Switch to another frame/window (by Sylvain Utard)
;; http://nex-3.com/posts/45-efficient-window-switching-in-emacs#comments
(global-set-key [M-left] 'windmove-left)          ; move to left windnow
(global-set-key [M-right] 'windmove-right)        ; move to right window
(global-set-key [M-up] 'windmove-up)              ; move to upper window
(global-set-key [M-down] 'windmove-down)          ; move to downer window

;; Save file
(global-set-key (kbd "M-s") 'save-buffer)


;; Applying proper keyboard shortcut for jumping to TAGS 
;; TODO(kgaipal@gmail.com): Make it interactively search for next occurence of the TAG
;; will prefer the F2 key for that!

;; (global-unset-key (kbd "<f2>-<f2>"))
(global-set-key (kbd "<f2>") 'find-tag)
(global-set-key (kbd "S-<f2>") 'pop-tag-mark)

;; Toggle ECB windows (very usefull in grep mode)
(defvar ecb-windows-visible nil)
(defun ecb-toggle-windows-visibility () 
  (interactive) (if (eq ecb-windows-visible nil)
		    (progn 
		      (ecb-show-ecb-windows) 
		      (setq ecb-windows-visible t))
		  (progn 
		    (ecb-hide-ecb-windows) 
		    (setq ecb-windows-visible nil))))

(global-set-key (kbd "<f3>") 'ecb-toggle-windows-visibility)