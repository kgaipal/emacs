;; Some keyboard shorcuts I changed/assigned which are more intuitve for my current
;; keyboard layout
;; Last edited on: Dec 13, 2014
;; by Kshitij Gaipal

;; Load this file in your ".emacs" file:
;; (load-file "~/.emacs.d/keyboard-shortcuts.el")

;; Basic indentation
(global-set-key (kbd "<backtab>") 'my-decrease-left-margin)

;; Auto reload file by F5 keystroke
(global-set-key [f5] 'refresh-file)

;; Jump to line number
;; http://geosoft.no/development/emacs.html
(global-set-key (kbd "C-x C-l") 'goto-line)

;; ;; Proper shortcuts for searching text (forward)
;; ;; http://sralab.com/2006/12/20/emacs-cut-paste-and-nice-other-hotkeys/
;; (global-set-key (kbd "C-f") 'isearch-forward)
;; (define-key isearch-mode-map [(control f)] (lookup-key isearch-mode-map (kbd "C-s")))
;; (define-key minibuffer-local-isearch-map [(control f)]
;; (lookup-key minibuffer-local-isearch-map (kbd "C-f")))

;; Toggle between source/header files
(global-set-key (kbd "<f4>") 'ff-find-other-file)

;; Undo/Redo
(global-set-key (kbd "C-z") 'undo)

;; We want another key to suggest keywords completions
(global-set-key (kbd "C-/") 'dabbrev-expand)

;; Invoke Alt+x by alternate command
;; http://sites.google.com/site/steveyegge2/effective-emacs
(global-set-key (kbd "C-l") 'execute-extended-command) ;analogous to shifting focus to URL bar in FireFox

;; Kill whole line and unset accidental region deletion
;; (global-set-key (kbd "C-w") 'kill-buffer)
;; (global-set-key (kbd "C-w") 'subword-kill)
;; (global-set-key (kbd "M-d") 'kill-whole-line)

;; Avoiding killing (or suspending) emacs accidently while
;; cut-copy-paste (or undo/redo) operations
(global-unset-key (kbd "C-x C-c"))	;stop emacs
(global-unset-key (kbd "C-x C-z"))	;hide emacs to taskbar, does not makes sense for terminal mode
(global-unset-key (kbd "C-x q"))	;disable command kbd-macro-query, until I give up killing emcas from terminal
;; (global-set-key (kbd "C-x q") 'save-buffers-kill-terminal)

;; (global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-o") 'ido-switch-buffer)

;; marking a buffer as readonly
(global-set-key (kbd "M-r") 'read-only-mode)
(global-set-key (kbd "<f2>") 'read-only-mode)

;; Switch to another frame/window (by Sylvain Utard)
;; http://nex-3.com/posts/45-efficient-window-switching-in-emacs#comments
(global-set-key (kbd "C-c C-<left>")  'windmove-left)  ; move to left windnow
(global-set-key (kbd "C-c C-<right>") 'windmove-right) ; move to right window
(global-set-key (kbd "C-c C-<up>")    'windmove-up)    ; move to upper window
(global-set-key (kbd "C-c C-<down>")  'windmove-down)  ; move to downer window
;; more alias for the same thing
;; (global-set-key (kbd "M-j") 'windmove-left)  ; move to left windnow
;; (global-set-key (kbd "M-k") 'windmove-right) ; move to right window
;; (global-set-key (kbd "M-p") 'windmove-up)    ; move to upper window
;; (global-set-key (kbd "M-n") 'windmove-down)  ; move to downer window

;; ;; Toggle ECB windows for overflowing text
;; (global-set-key (kbd "<f9>") 'ecb-toggle-windows-visibility)

;; Enlarge/Shrink window size just like tmux
(global-set-key (kbd "C-x <C-up>") 'enlarge-window)
(global-set-key (kbd "C-x <C-down>") 'shrink-window)

;; Highlight current symbol under cursor
;; http://xahlee.blogspot.com/2010/05/emacs-isearch-of-current-work.html
;; (global-set-key (kbd "<f3>") 'highlight-symbol-prev)
;; (global-set-key (kbd "C-<f3>") 'highlight-symbol-next)
(global-set-key (kbd "C-x .") 'search-at-point)

;; Split window horizontally/vertically as in tmux
(global-set-key (kbd "C-x |") 'split-window-horizontally)
(global-set-key (kbd "C-x -") 'split-window-vertically)

;; Scroll other window up/down
(global-set-key (kbd "C-M-p") 'scroll-other-window-down)
(global-set-key (kbd "C-M-n") 'scroll-other-window)

;; Move across buffers
(global-set-key (kbd "C-x n") 'next-buffer)
(global-unset-key (kbd "C-x C-n"))
(global-set-key (kbd "C-x p") 'previous-buffer)
(global-unset-key (kbd "C-x C-p"))

;; Disable the RET command since it is close to C-n for moving to next line
;; (global-unset-key (kbd "C-m"))

(global-unset-key (kbd "C-o"))		; I accidently press this instead of C-p
;; (global-unset-key (kbd "C-c C-d"))	; I accidently press this instead of C-x C-s (save file)

;; Disable backwork kill sentence
(global-unset-key (kbd "C-x DEL"))

;; ;; Find next defined tag
;; (global-set-key (kbd "C-x M-.") ')

;; whitespace-cleanup
(global-set-key (kbd "C-x C-k") 'whitespace-cleanup)

;; Disable some commands
(put 'kbd-macro-query 'disabled t)	;conflicts with key sequence "C-x q"
(put 'compose-mail 'disabled t)		;conflicts with key sequence "C-l"

