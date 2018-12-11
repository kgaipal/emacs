;; Utility functions, hooks, and customizations for .emacs
;; kgaipal@gmail.com

;; Mark the buffer read only to avoid cat typing in a newly opened buffer
;; http://stackoverflow.com/questions/5154309/how-to-make-a-opened-buffer-read-only-without-reloading-again-with-find-file-rea
(add-hook 'find-file-hook
          (lambda ()
            (when (and (buffer-file-name)
                       (file-exists-p (buffer-file-name))
                       (file-writable-p (buffer-file-name)))
              (if (or (and (>= (length (buffer-name)) 6)
                           (string= "COMMIT" (substring (buffer-name) 0 6)))
                      (string= "el" (file-name-extension (buffer-file-name))))
                  (read-only-mode -1)
                (read-only-mode t))
              (if (string= "/scp:" (substring (buffer-file-name) 0 5))
                  (auto-save-mode -1)))))

;; Revert all buffers http://www.emacswiki.org/emacs/RevertBuffer#toc4
(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (not (buffer-modified-p)))
        (revert-buffer t t t)
        (toggle-read-only t))))
  (message "Reverting all buffers...done") )

;; Highlight C++11 keywords
(setq c-C++-access-key "override\\|final")
(font-lock-add-keywords 'c++-mode '(("override\\|final" . font-lock-constant-face)))

;; Highlight postgres keywords too when in SQL mode
(add-hook 'sql-mode-hook 'sql-highlight-postgres-keywords)

;; Common features in all c-mode
(add-hook 'c-mode-common-hook
          (lambda ()
            (if (package-installed-p 'fic-mode)
                (fic-mode t))
            (electric-pair-local-mode 1)
            (which-function-mode t)
            (subword-mode 1)))

;; fixing wierd control characters in shell mode
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Auto reload file by F5 keystroke
(defun refresh-file ()
  (interactive) (revert-buffer t t t)
  (toggle-read-only t))

;; Indenting text using tabs, unless the region is marked,
;; we cannot tab so marking the whole line if its not set
;; http://emacswiki.org/emacs/.emacs-ChristianRovner.el
(defun expand-region-to-whole-lines ()
  "Expand the region to make it encompass whole lines.
If the region is not active, activate the current line."
  (if (not mark-active)
      ;; Create region from current line
      (progn
        (beginning-of-line)
        (set-mark (point))
        (end-of-line))
    ;; The mark is active, expand region
    (let ((beg (region-beginning))
          (end (region-end)))
      (goto-char beg)
      (beginning-of-line)
      (set-mark (point))
      (goto-char end)
      (unless (bolp) (end-of-line)))))

(defun increase-left-margin-wrapper ()
  "Increase left margin in region after expanding it to whole lines."
  (interactive)
  (let (deactivate-mark)
    (expand-region-to-whole-lines)
    (increase-left-margin (region-beginning) (region-end) nil)))

(defun decrease-left-margin-wrapper ()
  "Decrease left margin in region after expanding it to whole lines."
  (interactive)
  (let (deactivate-mark)
    (expand-region-to-whole-lines)
    (decrease-left-margin (region-beginning) (region-end) nil)))

;; vim like highlight and search a word forward
;; http://emacswiki.org/emacs/.emacs-ChristianRovner.el
(defun search-at-point ()
  "Highlight word at point by starting an isearch on that word."
  (interactive)
  (isearch-mode nil)
  (highlight-symbol-remove-all)
  (let ((word (symbol-name (symbol-at-point))))
    (setq isearch-string word
          isearch-message word
          isearch-forward word)
    (kill-new word))
  (isearch-update))

;; Copies the real path of the buffer we are working-on into the minibuffer,
;; and also in the yank (paste) buffers of the emacs application
;; http://stackoverflow.com/questions/3669511/the-function-to-show-current-files-full-path-in-mini-buffer
(defun copy-buffer-realpath ()
  "Copies the realpath of the current buffer in kill ring."
  (interactive)
  (message "'%s' copied!" (buffer-file-name))
  (kill-new (file-truename buffer-file-name)))

;; Custom column for showing file or process directory in ibuffer
(define-ibuffer-column file-or-process-directory
  (:name "Directory")
  (if buffer-file-name
      (file-name-directory buffer-file-name)
    ""))

;; Hide unversion files in vc-dired mode
;; http://pastebin.com/9EVj2P0R
(defun vc-dir-hide-unregistered ()
  "Hide unregistered items and modified directories from display.
Only show the modified files."
  (interactive)
  (let ((crt (ewoc-nth vc-ewoc -1))
        (first (ewoc-nth vc-ewoc 0)))
    ;; Go over from the last item to the first and remove the
    ;; unregistered files and directories with no child files.
    (while (not (eq crt first))
      (let* ((data (ewoc-data crt))
             (dir (vc-dir-fileinfo->directory data))
             (next (ewoc-next vc-ewoc crt))
             (prev (ewoc-prev vc-ewoc crt))
             ;; ewoc-delete does not work without this...
             (inhibit-read-only t))
        (when (or
               ;; Remove directories with no child files.
               (and dir
                    (or
                     ;; Nothing follows this directory.
                     (not next)
                     ;; Next item is a directory.
                     ;; (vc-dir-fileinfo->directory (ewoc-data next))))

                     ;; hide the directory entry, we want to show only the modified files
                     (vc-dir-fileinfo->directory data)))
               ;; Remove files in the unregistered state.
               (eq (vc-dir-fileinfo->state data) 'unregistered))
          (ewoc-delete vc-ewoc crt))
        (setq crt prev)))))

;; Toggle the layout (vertical -> horizontal and vice versa)
;; of the frame only if it is split into 2 windows
;; http://www.emacswiki.org/emacs/ToggleWindowSplit
(defun toggle-window-split ()
  "Toggle the layout (vertical -> horizontal and vice versa)
of the frame only if it is split into exactly 2 windows."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))
    (if (< (count-windows) 2)
        (message "Nothing to toggle")
      (message "Won't toggle for more than 2 windows!"))))

;; Highlight current line only in ibuffer
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (if (string= "*Ibuffer*" (buffer-name))
                (hl-line-mode 1)
              (hl-line-mode -1))))

;; Change the font face in vc-dir-mode buffer
(add-hook 'vc-dir-mode-hook
          (lambda ()
            (if (string= "*vc-dir*" (buffer-name))
                (hl-line-mode 1)
              (hl-line-mode -1))))

;; Restore windows layout when done with ediff mode
;; http://emacswiki.org/emacs/EdiffMode#toc3
(defvar my-ediff-bwin-config nil "Window configuration before ediff.")
(defcustom my-ediff-bwin-reg ?b
  "*Register to be set up to hold `my-ediff-bwin-config'
    configuration.")

(defvar my-ediff-awin-config nil "Window configuration after ediff.")
(defcustom my-ediff-awin-reg ?e
  "*Register to be used to hold `my-ediff-awin-config' window
    configuration.")

(defun my-ediff-bsh ()
  "Function to be called before any buffers or window setup for
    ediff."
  (setq my-ediff-bwin-config (current-window-configuration))
  (when (characterp my-ediff-bwin-reg)
    (set-register my-ediff-bwin-reg
                  (list my-ediff-bwin-config (point-marker)))))

(defun my-ediff-ash ()
  "Function to be called after buffers and window setup for ediff."
  (setq my-ediff-awin-config (current-window-configuration))
  (when (characterp my-ediff-awin-reg)
    (set-register my-ediff-awin-reg
                  (list my-ediff-awin-config (point-marker)))))

(defun my-ediff-qh ()
  "Function to be called when ediff quits."
  (when my-ediff-bwin-config
    (set-window-configuration my-ediff-bwin-config)))

(add-hook 'ediff-before-setup-hook 'my-ediff-bsh)
(add-hook 'ediff-after-setup-windows-hook 'my-ediff-ash 'append)
(add-hook 'ediff-quit-hook 'my-ediff-qh)

(add-hook 'php-mode-hook
          (lambda ()
            (setq comment-start "#")
            (setq comment-end "")))

;; Start a new emacs shell and rename it uniquely.
;; http://stackoverflow.com/a/2788843/398328
(defun new-shell (buffer-name)
  "Start a new emacs shell and rename it uniquely."
  (interactive "s shell-buffer name: ")

  ;; option #1
  ;; (ansi-term "/bin/bash")
  ;; (rename-buffer (concat "*term:" buffer-name "*"))

  ;; option #2
  (shell (concat "*shell:" buffer-name "*"))

  ;; option #3
  ;; (eshell )
  ;; (rename-buffer (concat  "*shell:" buffer-name "*"))

  (message (concat "Renamed the new shell to *shell:" buffer-name "*")))

(defun insert-todo ()
  "Inserts a 'TODO (username):' commented at point"
  (interactive)
  (kill-new (concat "TODO (" (getenv "USERNAME") "): "))
  (yank))
(defalias 'todo 'insert-todo)

(defun insert-fixme ()
  "Inserts a 'FIXME (username):' commented at point"
  (interactive)
  (kill-new (concat "FIXME (" (getenv "USERNAME") "): "))
  (yank))
(defalias 'fixme 'insert-fixme)

(defun find-files-in-project-root (wildcard)
  "Find files interactively from the project ROOT."
  (interactive "s wildcard: ")
  (find-name-dired desktop-dirname wildcard))

(defun find-text-in-project-root (wildcard)
  "Find text interactively from the project ROOT. If desktop is not loaded, default search
 directory is set as /tmp"
  (interactive "s wildcard: ")
  (require 'ripgrep)
  (if (< (length wildcard) 4)
      (message "enter atleast 4 chars")
    (if (boundp 'desktop-dirname)
        (ripgrep-regexp wildcard desktop-dirname)
      (ripgrep-regexp wildcard "~/code/"))))

(defun find-files-in-project-root-using-counsel ()
  "Find files interactively using 'counsel' package from the project ROOT."
  (interactive)
  (counsel-file-jump nil desktop-dirname))

;; Insert current date and time
(defun insert-current-date ()
  (interactive)
  (insert (shell-command-to-string "echo -n $(date +'%b %d, %Y')")))

;; Set the tab width
;; http://www.chemie.fu-berlin.de/chemnet/use/info/cc-mode/cc-mode_6.html#SEC17
(setq-default default-tab-width 8)
(setq-default c-basic-indent 0)
(setq-default c-basic-offset 4)
(c-set-offset 'substatement-open 0)
(setq c-default-style "linux" c-basic-offset 4)

;; Shorten the required response in mode-buffer from yes/no to y/n
;; http://www.youtube.com/watch?v=a-jRN_ba41w
(fset 'yes-or-no-p 'y-or-n-p)

;; Winners mode to preserve split windows
;; http://ergoemacs.org/emacs/emacs_winner_mode.html
(winner-mode 1)

;; When we move forward word-by-word ... also stop at ';'
;; Helpfull stop running past to next line in c-mode/c++-mode
(modify-syntax-entry ?$ "w")

;; setup for various files [extensions] and their editing modes
(setq auto-mode-alist
      (append '(("\\.psql$" . sql-mode)
                ("\\.h$" . c++-mode)
                ("\\.ini$" . windows-conf-mode)
                ("\\.config$" . nxml-mode)
                ("\\.csproj$" . nxml-mode)
                ("\\.scons$" . python-mode)
                ("\\.cshtml$" . html-mode)
                ("SConstruct$" . python-mode)
                ("CMakeLists.txt$" . makefile-mode)) auto-mode-alist))

;; ~/.local/bin PATH
(defvar local-bin-path (concat (getenv "HOME") "/.local/bin/"))

;; platforms specic tweaks
(if (or (eq system-type 'windows-nt) (eq system-type 'msdos))
    (progn
      (defvar path-sep ";")

      ;; disable menu bar
      (menu-bar-mode 0)

      ;; remove the hook to check the vc-status on any file;
      ;; this makes emacs 1-2 slow on windows
      ;; http://stackoverflow.com/questions/8837712/emacs-creates-buffers-very-slowly
      (remove-hook 'find-file-hooks 'vc-find-file-hook))
  (progn
    (defvar path-sep ":")))

;; TODO (kgaipal): below is unnecessary if path is set as 'System Variable'
(setenv "PATH" (concat local-bin-path path-sep (getenv "PATH")))

;; Using external grep and find programs
(setq grep-program "grepk"
      find-program "findk"
      grep-command (concat grep-program " "))

;; path to custom themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; desktop session customization
(setq desktop-path '("/tmp" "C:/msys64/tmp")) ;save in tmp location for quick/dirty changes
(setq desktop-restore-eager 2)                ;restore only 2 buffers
(setq desktop-save (quote if-exists))         ;save without prompting if desktop files exists
(setq desktop-lazy-idle-delay 30)             ;responsiveness increases
(setq desktop-lazy-verbose nil)               ;this slows down

(add-hook 'desktop-after-read-hook
          (lambda ()

            ;; preserve some buffers when switching desktop-sessions
            (setq desktop-clear-preserve-buffers
                  (append '("&bitlbee"
                            "\\*Group\\*"
                            "\\*Summary INBOX\\*"
                            "\\*shell\\:[[:alnum:]]+\\*"
                            "\\*term\\:[[:alnum:]]+\\*"
                            "\\.newsrc-dribble"
                            "erc\\:[[:alnum:]]+")
                          desktop-clear-preserve-buffers))

            ;; frame title showing desktop path
            (set-frame-name (file-name-nondirectory (directory-file-name desktop-dirname)))))

;; dont remove byte-order marker (BOM) in xml files
;; https://superuser.com/questions/41254/make-emacs-not-remove-the-bom-from-xml-files
(setq auto-coding-regexp-alist
  (delete (rassoc 'utf-16be-with-signature auto-coding-regexp-alist)
  (delete (rassoc 'utf-16le-with-signature auto-coding-regexp-alist)
  (delete (rassoc 'utf-8-with-signature auto-coding-regexp-alist)
          auto-coding-regexp-alist))))

;; convenient short names for swapping windows
(defun swap () (interactive) (buf-move-left))
(defun swapr () (interactive) (buf-move-right))

;; dont add final newline in xml config files automatically
(add-hook 'nxml-mode-hook
          (lambda ()
            (setq require-final-newline nil
                  truncate-lines t)))

;; displays current buffer encoding system
(defun display-buffer-encoding ()
  "displays current buffer encoding system"
    (interactive)
  (message "%s" buffer-file-coding-system))

;; make header for source file
(defun make-header ()

  (defun eol ()
    "\n")

  (defun bom ()
    "﻿")

  (defun line-seperator ()
    (make-string 80 ?-))

  (defun comment-marker ()
    "// ")

  (defun company-name ()
    "Microsoft Corporation")

  (defun author-name ()
    "Kshitij Gaipal")

  (defun header-start ()
    (concat (bom) (comment-marker) (line-seperator) (eol)))

  (defun header-end ()
    (concat (comment-marker) (line-seperator) (eol)))

  (defun author-block ()
    (concat (comment-marker) "<author name=\"" (author-name) "\" />") (eol))

  (defun copyright-block ()
    (concat
     (comment-marker) "<copyright file=\"" (buffer-name) "\" company=\"" (company-name) "\" >" (eol)
     (comment-marker) "    Copyright (c) " (company-name) ". All rights reserved." (eol)
     (comment-marker) "</copyright>" (eol)))

  "Creates and Inserts the header for a source file"
  (interactive)
  (let ((header (concat
                 (header-start)
                 (copyright-block)
                 (header-end))))
    (kill-new header)
    (message header)))

;;
(defun compile-visual-studio ()
  "Creates and Inserts the header for a source file"
  (interactive)
  (compile "c:/msys64/home/ksgaipal/code/confs/scripts/vs-make.bat"))
