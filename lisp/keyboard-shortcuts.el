;; Intuitive key combinations for frequent operations
;; kgaipal@gmail.com

;; Disable some commands
(put 'kbd-macro-query 'disabled t)	;conflicts with key sequence "C-x q"
(put 'compose-mail 'disabled t)		;conflicts with key sequence "C-l"

;; Disable some shotcuts since they are cat typed mistakenly for other regular keys
(global-unset-key (kbd "C-o"))		;I accidently press this instead of C-p
(global-unset-key (kbd "C-x DEL"))      ;disable backwork kill sentence
(global-unset-key (kbd "C-x C-c"))	;dont stop emacs
(global-unset-key (kbd "C-x C-z"))	;dont hide emacs into taskbar
(global-unset-key (kbd "C-x q"))	;disable command kbd-macro-query, until I give up killing emcas from terminal
(global-unset-key (kbd "C-x C-n"))      ;already disabled, but this suppress annoying prompt
(global-unset-key (kbd "C-x C-p"))      ;already disabled, but this suppress annoying prompt

;; Basic indentation
(global-set-key (kbd "<backtab>") 'decrease-left-margin-wrapper)
(global-set-key (kbd "C-\\") 'increase-left-margin-wrapper)

;; Auto reload file by F5 keystroke
(global-set-key [f5] 'refresh-file)
(global-set-key [C-f5] 'revert-all-buffers)

;; Jump to line number
;; http://geosoft.no/development/emacs.html
(global-set-key (kbd "C-x C-l") 'goto-line)

;; Toggle between source/header files
(global-set-key (kbd "<f2>") 'ff-find-other-file)

;; Undo/Redo
(global-set-key (kbd "C-z") 'undo)

;; We want another key to suggest keywords completions
(global-set-key (kbd "C-/") 'dabbrev-expand)

;; Invoke Alt+x by alternate command; its analogous to shifting focus to URL bar in
;; firefox
;; http://sites.google.com/site/steveyegge2/effective-emacs
(global-set-key (kbd "C-l") 'execute-extended-command)

;; marking a buffer as readonly
(global-set-key (kbd "M-r") 'read-only-mode)

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
(global-set-key (kbd "C-x p") 'previous-buffer)

;; quick toggling window split for grep/find/ag/ripgrep search results when searched
;; results are too long
(global-set-key (kbd "C-M-w") 'toggle-window-split)

;; dumb-jump mode
(if (package-installed-p 'dumb-jump)
    (progn
      (global-set-key (kbd "C-M-l") 'dumb-jump-go)
      (global-set-key (kbd "C-M-o") 'dumb-jump-back)))

;; magit
(if (package-installed-p 'magit)
    (progn
      (global-set-key (kbd "C-M-g") 'magit-status)))


;; ivy mode
(if (package-installed-p 'ivy)
    (progn
      (global-set-key (kbd "C-x C-o") 'ivy-switch-buffer)))
;; (global-set-key (kbd "C-x C-b") 'ibuffer)
;; (global-set-key (kbd "C-x C-o") 'ido-switch-buffer)

;; finding text can use any project grep/ripgrep/silver_searcher
;; see wrapper: find-text-in-project-root
(global-set-key (kbd "C-M-.") 'find-text-in-project-root)

;; visual studio like shortcut for finding files in project
(global-set-key (kbd "C-.") 'find-files-in-project-root-using-counsel)

;; edit-at-point
(if (package-installed-p 'edit-at-point)
    (progn
      ;; sample keybinding config:
      ;; (require 'bind-key)
      ;; (bind-keys
      ;;   ("C-S-a". edit-at-point-word-copy)
      ;;   ("C-S-b". edit-at-point-word-cut)
      ;;   ("C-S-c". edit-at-point-word-delete)
      ;;   ("C-S-d". edit-at-point-word-paste)
      ;;   ("C-S-e". edit-at-point-symbol-copy)
      ;;   ("C-S-f". edit-at-point-symbol-cut)
      ;;   ("C-S-g". edit-at-point-symbol-delete)
      ;;   ("C-S-h". edit-at-point-symbol-paste)
      ;;   ("C-S-i". edit-at-point-str-copy)
      ;;   ("C-S-j". edit-at-point-str-cut)
      ;;   ("C-S-k". edit-at-point-str-delete)
      ;;   ("C-S-l". edit-at-point-str-paste)
      ;;   ("C-S-m". edit-at-point-line-copy)
      ;;   ("C-S-n". edit-at-point-line-cut)
      ;;   ("C-S-o". edit-at-point-line-delete)
      ;;   ("C-S-p". edit-at-point-line-paste)
      ;;   ("C-S-q". edit-at-point-line-dup)
      ;;   ("C-S-r". edit-at-point-line-up)
      ;;   ("C-S-s". edit-at-point-line-down)
      ;;   ("C-S-t". edit-at-point-paren-copy)
      ;;   ("C-S-u". edit-at-point-paren-cut)
      ;;   ("C-S-v". edit-at-point-paren-delete)
      ;;   ("C-S-w". edit-at-point-paren-paste)
      ;;   ("C-S-x". edit-at-point-paren-dup)
      ;;   ("C-S-y". edit-at-point-defun-copy)
      ;;   ("C-S-z". edit-at-point-defun-cut)
      ;;   ("C-{"  . edit-at-point-defun-delete)
      ;;   ("C-:"  . edit-at-point-defun-paste)
      ;;   ("C-\"" . edit-at-point-defun-dup))
      (global-set-key (kbd "C-S-d") 'edit-at-point-symbol-delete)
      (global-set-key (kbd "C-S-w") 'edit-at-point-symbol-copy)))

;; fixup whitespace
(global-set-key (kbd "M-SPC") 'fixup-whitespace)

;; replace-string
(global-set-key (kbd "M-s M-s") 'replace-string)

;; omnisharp specific
(global-set-key (kbd "<f12>") 'omnisharp-go-to-definition)
(global-set-key (kbd "C-<f12>") 'pop-tag-mark)
