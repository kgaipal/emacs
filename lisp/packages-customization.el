;; emacs external packages configuration
;; kgaipal@gmail.com


;; csharp-mode specific
(defun my-csharp-mode-hook ()
  (electric-pair-mode 1)       ;; Emacs 24
  (electric-pair-local-mode 1) ;; Emacs 25
  )
(add-hook 'csharp-mode-hook 'my-csharp-mode-hook)

(if (package-installed-p 'auto-complete)
    (progn
      (require 'auto-complete)
      (global-auto-complete-mode t)
      (setq ac-modes (append '(csharp-mode) ac-modes))))

(if (package-installed-p 'ripgrep)
    (progn
      (setq ripgrep-arguments (quote ("--smart-case")))))

(if (package-installed-p 'dumb-jump)
    (progn
      (setq dumb-jump-default-project "/tmp/")))

;; Turn on anzu mode globally
(if (package-installed-p 'anzu)
    (progn
      (global-anzu-mode t)))

;; Turn on anzu mode globally
(if (package-installed-p 'indent-guide)
    (progn
      (setq indent-guide-global-mode t)))

;; Global option for ws-butler
(if (package-installed-p 'ws-butler)
    (progn
      (require 'ws-butler)
      (setq ws-butler-convert-leading-tabs-or-spaces nil)
      (ws-butler-global-mode t)))

;; Turn off global magit-auto-revert-mode since this slows down when too many buffers are
;; open and git commands (like git checkout -- ) are issued outside of magit emacs/magit
;; environment
(if (package-installed-p 'magit)
    (progn
      (require 'magit)
      (setq magit-auto-revert-mode nil)))

;; Install external packages from [M]ELPA
;; http://stackoverflow.com/a/21065066
;; http://ergoemacs.org/emacs/emacs_package_system.html
;; https://github.com/emacs-tw/awesome-emacs
;;
;; try this too for convenience
;; https://github.com/technomancy/better-defaults
(defvar packages-to-restore
  '(
    annotate-depth
    anzu
    auto-complete
    bm
    buffer-move
    clang-format
    csharp-mode
    dumb-jump
    fold-this
    highlight-symbol
    indent-guide
    magit
    occur-x
    open-in-msvs
    realgud
    restart-emacs
    rg
    ripgrep
    tfs
    typescript-mode
    unbound
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
