;; Some utility functions and hooks which I found rather helpful in certain tasks
;; Last edited on: Dec 13, 2014
;; by Kshitij Gaipal

;; Load this file in your ".emacs" file:
;; (load-file "~/.emacs.d/util-functions.el")

;; ;; Defining my own aliases.
;; ;; Some usefull ones are copied from here:
;; ;; http://xahlee.org/emacs/emacs_alias.html
;; (defalias 'list-buffers 'ibuffer)

;; ;; Python indent using tabs instead of spaces
;; (add-hook 'python-mode-hook
;;        (lambda ()
;;          (setq indent-tabs-mode t)
;;          (setq python-indent 4)
;;          (setq tab-width 4)))

;; Mark the buffer read only to avoid cat typing in a newly opened buffer
;; http://stackoverflow.com/questions/5154309/how-to-make-a-opened-buffer-read-only-without-reloading-again-with-find-file-rea
(add-hook 'find-file-hook
          '(lambda ()
             (when (and (buffer-file-name)
                        (file-exists-p (buffer-file-name))
                        (file-writable-p (buffer-file-name)))
               (toggle-read-only 1)
               (if (string= "/scp:"
                            (substring (buffer-file-name) 0 5))
                   (auto-save-mode -1)))))

;; Revert all buffers [http://www.emacswiki.org/emacs/RevertBuffer#toc4]
(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (not (buffer-modified-p)))
        (revert-buffer t t t)
        (toggle-read-only 1) )))
  (message "Reverting all buffers...done") )

;; Highlight Qt specific keywords just like public,protected, private keywords in c++
;; [http://www.emacswiki.org/emacs/QtMode#toc3]
(setq c-C++-access-key "\\<\\(slots\\|signals\\|private\\|protected\\|public\\)\\>[ \t]*[(slots\\|signals)]*[ \t]*:")
(font-lock-add-keywords 'c++-mode '(("\\<\\(Q_[A-Z]*\\|\\Q[A-Z][A-Za-z]*\\|public slots\\|public signals\\|private slots\\|private signals\\|protected slots\\|protected signals\\)\\>" . font-lock-constant-face)))

;; Highlight C++11 keywords
(setq c-C++-access-key "override\\|final")
(font-lock-add-keywords 'c++-mode '(("override\\|final" . font-lock-constant-face)))

;; right now c++-mode does not work in .h file, so forcing it.
;; [http://stackoverflow.com/questions/3312114/how-to-tell-emacs-to-open-h-file-in-c-mode]
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-hook 'c-mode-common-hook
          (lambda ()
            (which-function-mode t)
            (subword-mode 1)))

;; Auto reload file by F5 keystroke
(defun refresh-file ()
  (interactive) (revert-buffer t t t)
  (toggle-read-only 1))

;; ;; Search some string in all open buffers (use ibuffer)
;; ;; http://stackoverflow.com/questions/2641211/emacs-interactively-search-open-buffers#_=_
;; (defun my-multi-occur-in-matching-buffers (regexp &optional allbufs)
;;   "Show all lines matching REGEXP in all buffers."
;;   (interactive (occur-read-primary-args))
;;   (multi-occur-in-matching-buffers ".*" regexp))

;; ;; Toggle ECB windows (very usefull in grep mode)
;; (defvar ecb-windows-visible nil)
;; (defun ecb-toggle-windows-visibility ()
;;   (interactive) (if (eq ecb-windows-visible nil)
;;                  (progn
;;                    (ecb-show-ecb-windows)
;;                    (setq ecb-windows-visible t))
;;                (progn
;;                  (ecb-hide-ecb-windows)
;;                  (setq ecb-windows-visible nil))))

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

;; ;; make completion buffers disappear after 3 seconds.
;; ;; http://snarfed.org/why_i_dont_run_shells_inside_emacs
;; (add-hook 'completion-setup-hook
;;        (lambda () (run-at-time 3 nil
;;                                (lambda () (delete-windows-on "*Completions*")
;;                                  (message "Deleted some windows")))))

;; Start a new emacs shell and rename it uniquely.
;; http://stackoverflow.com/a/2788843/398328
(defun new-shell (buffer-name)
  "Start a new emacs shell and rename it uniquely."
  (interactive "s shell-buffer name: ")

  ;; option #1
  ;; (ansi-term "/bin/bash")
  ;; (rename-buffer (concatenate 'string "*term:" buffer-name "*"))

  ;; option #2
  (shell (concatenate 'string "*shell:" buffer-name "*"))

  ;; option #3
  ;; (eshell )
  ;; (rename-buffer (concatenate 'string "*shell:" buffer-name "*"))

  (message (concatenate 'string "Renamed the new shell to *shell:" buffer-name "*")))

;; Insert current date and time
(defun insert-current-date () (interactive)
       (insert (shell-command-to-string "echo -n $(date +'%b %d, %Y')")))

;; Copies the current buffer in visual studio for editing; for now this command needs to
;; be run manually; ideally it should be open directly
(defun open-buffer-in-vs () (interactive)
       (let ((cmd (concat "\"C:/Program Files (x86)/Microsoft Visual Studio 14.0/Common7/IDE/devenv.exe\""
                          " /edit " "\"" (buffer-file-name) "\"")))(kill-new cmd)))
