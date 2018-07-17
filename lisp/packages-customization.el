;; emacs external packages configuration
;; kgaipal@gmail.com

;; omnisharp specific
(if (package-installed-p 'omnisharp)
    (progn

      ;; auto start server and flycheck mode
      ;; https://github.com/Omnisharp/omnisharp-emacs
      (add-hook 'csharp-mode-hook 'omnisharp-mode)
      (add-hook 'csharp-mode-hook #'flycheck-mode)

      ;; manually update version from https://github.com/OmniSharp/omnisharp-roslyn/releases/
      (setq omnisharp-expected-server-version "1.32.1")))

;; auto upgrade packags daily
(if (and (package-installed-p 'spu)
         (package-installed-p 'use-package))
    (progn
      (use-package spu
        :defer 120 ;; defer package loading for some seconds
        :config (spu-package-upgrade-daily))))

;; turn on occur-x-mode when occur is used
(if (package-installed-p 'occur-x)
    (progn
      (require 'occur-x)
      (add-hook 'occur-mode-hook 'turn-on-occur-x-mode)))

;; pretty display in selecting buffers through ivy
(if (package-installed-p 'ivy-rich)
    (progn
      (require 'ivy-rich)
      (ivy-set-display-transformer
       'ivy-switch-buffer
       'ivy-rich-switch-buffer-transformer)))

;; counsel specific
(if (package-installed-p 'counsel)
    (progn
      (setq counsel-find-file-ignore-regexp "\\(bin/\\|Debug/\\|Release/\\|TestResults/\\|/Fakes/\\|/FakesAssemblies/\\|Backup*/\\|\\.dll\\|\\.o\\|\\.wadcfgx\\|\\.vs\\|*\\.min\\.*\\|*\\.DotSettings\\.user\\)")))

;; ivy mode specific
(if (package-installed-p 'ivy)
    (progn
      (setq ivy-extra-directories (quote ("./")))
      (ivy-mode t)
      (setq ivy-height 5)))

;; csharp-mode specific
(if (package-installed-p 'csharp-mode)
    (progn
      (add-hook 'csharp-mode-hook
                (lambda ()

                  ;; code style for this mode only
                  (c-set-style "C#")

                  ;; all overrides after style is set above
                  (c-set-offset 'arglist-intro '+)
                  (c-set-offset 'topmost-intro-cont '0)

                  (setq require-final-newline nil) ;disable require-final-newline like xml mode
                  (fic-mode t)
                  (electric-pair-local-mode 1))
                )))

;; auto completion specific
(if (package-installed-p 'auto-complete)
    (progn
      (global-auto-complete-mode t)
      (setq ac-modes (append '(csharp-mode) ac-modes))))

;; ripgrep specific
(if (package-installed-p 'ripgrep)
    (progn
      ;; turn on coloring like grep
      ;; https://github.com/nlamirault/ripgrep.el/issues/20
      (setq ripgrep-arguments (quote ("" "--smart-case")))))

;; dumb-jump mode specific
;; set ripgrep and default searcher
(if (package-installed-p 'dumb-jump)
    (progn
      (setq dumb-jump-max-find-time 10)
      (setq dumb-jump-prefer-searcher "rg")
      (setq dumb-jump-default-project "/tmp/")))

;; Turn on anzu mode globally
(if (package-installed-p 'anzu)
    (progn
      (global-anzu-mode t)))

;; Turn on anzu mode globally
(if (package-installed-p 'indent-guide)
    (progn
      (indent-guide-global-mode t)))

;; Global option for ws-butler
(if (package-installed-p 'ws-butler)
    (progn
      (setq ws-butler-convert-leading-tabs-or-spaces nil)
      (ws-butler-global-mode t)))

;; Turn off global magit-auto-revert-mode since this slows down when too many buffers are
;; open and git commands (like git checkout -- ) are issued outside of magit emacs/magit
;; environment. Some improvements based on https://magit.vc/manual/magit/Performance.html
(if (package-installed-p 'magit)
    (progn
      (setq magit-auto-revert-mode nil)
      (setq magit-process-connection-type nil) ;make it fast on windows
      (setq magit-refresh-status-buffer nil)
      (setq magit-refresh-verbose t)

      ;; turning off diff options for speed
      (setq magit-diff-highlight-indentation nil)
      (setq magit-diff-highlight-trailing nil)
      (setq magit-diff-paint-whitespace nil)
      (setq magit-diff-highlight-hunk-body nil)
      (setq magit-diff-refine-hunk nil)
      (setq magit-revision-insert-related-refs nil)

      ;; When refreshing the "references buffer" is slow, then that’s usually because
      ;; several hundred refs are being displayed. The best way to address that is to
      ;; display fewer refs, obviously.
      ;;
      ;; If you are not, or only mildly, interested in seeing the list of tags, then start
      ;; by not displaying them:
      (remove-hook 'magit-refs-sections-hook 'magit-insert-tags)

      ;; When you initiate a commit, then Magit by default automatically shows a diff of
      ;; the changes you are about to commit. For large commits this can take a long time,
      ;; which is especially distracting when you are committing large amounts of
      ;; generated data which you don’t actually intend to inspect before committing. This
      ;; behavior can be turned off using:
      (remove-hook 'server-switch-hook 'magit-commit-diff)

      ;; add back following if speed is acceptable (total right now 4.6s)
      ;; 3.2s magit-insert-status-headers
      ;; 1.1s magit-insert-untracked-files
      ;; 0.7s magit-insert-unstaged-changes
      ;; 0.9s magit-insert-staged-changes
      (setq magit-status-sections-hook
            (quote
             (
              magit-insert-untracked-files
              magit-insert-unstaged-changes
              magit-insert-staged-changes
              )))

      ;; ;; disble vc backend just for Git because magit exists
      ;; (setq vc-handled-backends (delq 'Git vc-handled-backends))

      (if (package-installed-p 'magit-gitflow)
          (progn
            (add-hook 'magit-mode-hook 'turn-on-magit-gitflow)))
      ))

;; Install external packages from [M]ELPA
;; http://stackoverflow.com/a/21065066
;; http://ergoemacs.org/emacs/emacs_package_system.html
;; https://github.com/emacs-tw/awesome-emacs
;;
;; [OLD]:
;; aggressive-fill-paragraph
;; aggressive-indent
;; clang-format
;; dumb-jump
;; realgud
;; rg
;; unbound
;;
;; [TRYING]:
;; highlight-escape-sequences
;; electric-spacing
;; electric-operator
;; easy-repeat
;; easy-kill-extras
;; dynamic-ruler
;; disk
;; dired-dups
;; magit
;; magit-gitflow
;; try this too for convenience
;; https://github.com/technomancy/better-defaults
(defvar packages-to-restore
  '(
    annotate-depth
    anzu
    auctex
    auto-complete
    buffer-move
    counsel
    csharp-mode
    edit-at-point
    fic-mode
    fold-this
    highlight-symbol
    indent-guide
    ivy-rich
    magit
    msvc
    occur-x
    omnisharp
    restart-emacs
    ripgrep
    spu
    web-mode
    ws-butler
    xah-find
    ))

(defun restore-packages ()
  "Restore packages from [M]ELPA"
  (interactive)

  (package-refresh-contents)

  (if (not (package-installed-p 'use-package))
      (progn
        (package-refresh-contents)
        (package-install 'use-package)))

  (require 'use-package)

  (dolist (p packages-to-restore)
    (message "restoring %s" p)
    ;; (use-package p :ensure p))  ;; not working
    (package-install p))

  (message "done!"))
