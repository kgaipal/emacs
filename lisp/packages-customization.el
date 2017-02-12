;; emacs external packages configuration
;; kgaipal@gmail.com

(if (package-installed-p 'ripgrep)
    (progn
      (setq ripgrep-arguments (quote ("--smart-case")))))

(if (package-installed-p 'dumb-jump)
    (progn
      (setq dumb-jump-ag-cmd "rg")
      (setq dumb-jump-default-project (concat (getenv "HOME") "/code/"))))

;; Turn on anzu mode globally
(if (package-installed-p 'anzu)
    (progn
      (global-anzu-mode t)))

;; Turn on global whitespace-cleanup on file saving
(if (package-installed-p 'whitespace-cleanup-mode)
    (progn
      (global-whitespace-cleanup-mode t)))

;; Turn off global magit-auto-revert-mode since this slows down when too many buffers are
;; open and git commands (like git checkout -- ) are issued outside of magit emacs/magit
;; environment
(if (package-installed-p 'magit)
    (progn
      (require 'magit)
      (magit-auto-revert-mode nil)))

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
    whitespace-cleanup-mode
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
