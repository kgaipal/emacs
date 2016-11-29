;; Utility functions, hooks, and customizations for .emacs
;; kgaipal@gmail.com

;; Force single instance of emacs running on the system
(server-start)

;; Mark the buffer read only to avoid cat typing in a newly opened buffer
;; http://stackoverflow.com/questions/5154309/how-to-make-a-opened-buffer-read-only-without-reloading-again-with-find-file-rea
(add-hook 'find-file-hook
          '(lambda ()
             (when (and (buffer-file-name)
                        (file-exists-p (buffer-file-name))
                        (file-writable-p (buffer-file-name)))
               (toggle-read-only t)
               (if (string= "/scp:"
                            (substring (buffer-file-name) 0 5))
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

;; Highlight Qt specific keywords just like public,protected, private keywords in c++
;; http://www.emacswiki.org/emacs/QtMode#toc3
(setq c-C++-access-key "\\<\\(slots\\|signals\\|private\\|protected\\|public\\)\\>[ \t]*[(slots\\|signals)]*[ \t]*:")
(font-lock-add-keywords 'c++-mode '(("\\<\\(Q_[A-Z]*\\|\\Q[A-Z][A-Za-z]*\\|public slots\\|public signals\\|private slots\\|private signals\\|protected slots\\|protected signals\\)\\>" . font-lock-constant-face)))

;; Highlight C++11 keywords
(setq c-C++-access-key "override\\|final")
(font-lock-add-keywords 'c++-mode '(("override\\|final" . font-lock-constant-face)))

;; Highlight postgres keywords too when in SQL mode
(add-hook 'sql-mode-hook 'sql-highlight-postgres-keywords)

;; Common features in all c-mode
(add-hook 'c-mode-common-hook
          (lambda ()
            (which-function-mode t)
            (subword-mode 1)))

;; C# mode
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(add-hook 'csharp-mode-hook
          (lambda ()
            (message "TODO: customize csharp-mode-hook")
            ;; (which-function-mode nil)
            ;; (flymake-mode -1)
            ))

;; fixing wierd control characters in shell mode
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Auto reload file by F5 keystroke
(defun refresh-file ()
  (interactive) (revert-buffer t t t)
  (toggle-read-only t))

;; Cut-Copy-Paste in emacs-nox (Emacs without X)
;; http://hugoheden.wordpress.com/2009/03/08/copypaste-with-emacs-in-terminal/
;; I prefer using the "clipboard" selection (the one the
;; typically is used by c-c/c-v) before the primary selection
;; (that uses mouse-select/middle-button-click)
(setq x-select-enable-clipboard t)

;; If emacs is run in a terminal, the clipboard- functions have no
;; effect. Instead, we use of xsel, see
;; http://www.vergenet.net/~conrad/software/xsel/ -- "a command-line
;; program for getting and setting the contents of the X selection"
(unless window-system
  (when (getenv "DISPLAY")
    ;; Callback for when user cuts
    (defun xsel-cut-function (text &optional push)
      ;; Insert text to temp-buffer, and "send" content to xsel stdin
      (with-temp-buffer
        (insert text)
        ;; I prefer using the "clipboard" selection (the one the
        ;; typically is used by c-c/c-v) before the primary selection
        ;; (that uses mouse-select/middle-button-click)
        (call-process-region
         (point-min) (point-max) "xsel" nil 0 nil "--clipboard" "--input")))

    ;; Call back for when user pastes
    (defun xsel-paste-function()
      ;; Find out what is current selection by xsel. If it is different
      ;; from the top of the kill-ring (car kill-ring), then return
      ;; it. Else, nil is returned, so whatever is in the top of the
      ;; kill-ring will be used.
      (let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))
        (unless (string= (car kill-ring) xsel-output)
          xsel-output )))

    ;; Note: Disable the hooks for now because this doesnt
    ;; work while editing remote machines (xsel may not always be installed)

    ;; Attach callbacks to hooks
    ;; (setq interprogram-cut-function 'xsel-cut-function)
    ;; (setq interprogram-paste-function 'xsel-paste-function)

    ;; Idea from
    ;; http://shreevatsa.wordpress.com/2006/10/22/emacs-copypaste-and-x/
    ;; http://www.mail-archive.com/help-gnu-emacs@gnu.org/msg03577.html
    ))

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

(defun my-increase-left-margin ()
  "Increase left margin in region after expanding it to whole lines."
  (interactive)
  (let (deactivate-mark)
    (expand-region-to-whole-lines)
    (increase-left-margin (region-beginning) (region-end) nil)))

(defun my-decrease-left-margin ()
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

;; Insert current date and time
(defun insert-current-date () (interactive)
       (insert (shell-command-to-string "echo -n $(date +'%b %d, %Y')")))

;; Copies the current buffer in visual studio for editing; for now this
;; commands needs to be run manually; ideally it should be open directly
(defun open-buffer-in-vs () (interactive)
       (let ((vs-cmd (concat "\""
                             (shell-command-to-string (concat "where devenv"))
                             "\" /edit \"" (buffer-file-name) "\"")))
         (kill-new vs-cmd)))

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

;; IBuffer module provides better switching
;; http://www.emacswiki.org/emacs/IbufferMode
(require 'ibuf-ext)
;; hiding the unnecessary buffers
(add-to-list 'ibuffer-never-show-predicates "^\\*")

;; set the desktop-path to current directory only
(setq desktop-path '("."))

;; custom ibuffer format
(setq ibuffer-formats '((mark modified read-only " "
                              (name 50 100 :left :elide) " "
                              (file-or-process-directory))))

;; When we move forward word-by-word ... also stop at ';'
;; Helpfull stop running past to next line in c-mode/c++-mode
(modify-syntax-entry ?$ "w")

;; hiding some unwanted extensions in dired mode
;; http://www.emacswiki.org/emacs/DiredOmitMode
(require 'dired-x)
(setq-default dired-omit-files-p t)	; this is buffer-local variable
(setq dired-omit-files
      (concat dired-omit-files "\\|^\\..+$"))

;; setup for various files [extensions] and their editing modes
(setq auto-mode-alist
      (append '(("\\.psql$" . sql-mode)
                ("\\.cs$" . csharp-mode)
                ("\\.h$" . c++-mode)
                ("\\.ts$" . c++-mode)
                ("\\.ini$" . windows-conf-mode)
                ("\\.scons$" . python-mode)
                ("\\.cshtml$" . html-mode)
                ("SConstruct$" . python-mode)
                ("CMakeLists.txt$" . makefile-mode)) auto-mode-alist))

;; ~/.local/bin PATH
(defvar local-bin-path (concat (getenv "HOME") "/.local/bin/"))

;; platforms specic tweaks
(if (or (eq system-type 'windows-nt) (eq system-type 'msdos))
    (progn
      (defvar git-path (concat (getenv "HOME") "/code/git-sdk/usr/bin/"))

      (set-face-attribute 'default nil :height 130)

      ;; TODO: below is unnecessary if path is set as 'System Variable'
      (setenv "PATH" (concat git-path ";" (getenv "PATH")))
      (setenv "PATH" (concat local-bin-path ";" (getenv "PATH")))

      ;; disable menu bar
      (menu-bar-mode 0)

      ;; remove the hook to check the vc-status on any file;
      ;; this makes emacs 1-2 slow on windows
      ;; http://stackoverflow.com/questions/8837712/emacs-creates-buffers-very-slowly
      (remove-hook 'find-file-hooks 'vc-find-file-hook))
  (progn
    (setenv "PATH" (concat local-bin-path ":" (getenv "PATH")))
    (setq exec-path (append exec-path 'local-bin-path))))

;; Using external grep and find programs
;; Note: remove " . " from $(GIT_SDK_ROOT)/mingw64/share/emacs/25.1/lisp/find-dired.el
;; byte compile and reload emacs for find-dired to work with findk
(setq grep-program "grepk "
      find-program "findk "
      grep-command grep-program
      grep-find-command (concat find-program " -type f -exec " grep-command " {} \\;"))

;; Install external packages from [M]ELPA
;; http://stackoverflow.com/a/21065066
;; http://ergoemacs.org/emacs/emacs_package_system.html
;; https://github.com/emacs-tw/awesome-emacs
(defvar packages-to-restore
  '(
    ag
    annotate-depth
    anzu
    bm
    buffer-move
    clang-format
    csharp-mode
    direx
    dumb-jump
    highlight-symbol
    indent-guide
    mode-icons
    restart-emacs
    symon
    tfs
    unbound
    whitespace-cleanup-mode
    ))

;; try this too for convenience
;; https://github.com/technomancy/better-defaults

(defun restore-packages ()
  "Restore packages from [M]ELPA"
  (interactive)

  (if (not (package-installed-p 'use-package))
      (progn
        (package-refresh-contents)
        (package-install 'use-package)))

  (require 'use-package)

  (dolist (p packages-to-restore)
    (message "restoring %s" p)
    ;; (use-package p :ensure p))  ;; not working
    (package-install p))

  (message "done"))
