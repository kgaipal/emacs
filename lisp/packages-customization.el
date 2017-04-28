;; emacs external packages configuration
;; kgaipal@gmail.com

(if (package-installed-p 'aggressive-fill-paragraph)
    (progn
      (setq aggressive-fill-paragraph-mode t)))

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
      (setq counsel-find-file-ignore-regexp nil)))

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
                  ;; disable require-final-newline like xml mode
                  (setq require-final-newline nil)

                  (electric-pair-mode 1)        ;; Emacs 24
                  (electric-pair-local-mode 1)) ;; Emacs 25
                )))

;; auto completion specific
(if (package-installed-p 'auto-complete)
    (progn
      (global-auto-complete-mode t)
      (setq ac-modes (append '(csharp-mode) ac-modes))))

;; ripgrep specific
(if (package-installed-p 'ripgrep)
    (progn
      (setq ripgrep-arguments (quote ("--smart-case")))))

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
;; environment
(if (package-installed-p 'magit)
    (progn
      (setq magit-auto-revert-mode nil)

      (if (package-installed-p 'magit-gitflow)
          (progn
            (add-hook 'magit-mode-hook 'turn-on-magit-gitflow)))
      ))

;; Install external packages from [M]ELPA
;; http://stackoverflow.com/a/21065066
;; http://ergoemacs.org/emacs/emacs_package_system.html
;; https://github.com/emacs-tw/awesome-emacs
;;
;; OLD:
;; clang-format
;; dumb-jump
;; realgud
;; rg
;;
;; try this too for convenience
;; https://github.com/technomancy/better-defaults
(defvar packages-to-restore
  '(
    aggressive-fill-paragraph
    aggressive-indent
    annotate-depth
    anzu
    auto-complete
    buffer-move
    counsel
    csharp-mode
    edit-at-point
    fold-this
    highlight-symbol
    indent-guide
    ivy-rich
    magit
    magit-gitflow
    occur-x
    restart-emacs
    ripgrep
    tfs
    unbound
    web-mode
    ws-butler
    ))

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

  (message "done!"))
