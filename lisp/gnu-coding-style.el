;; GNU coding convention in Emacs as per mentioned at https://gcc.gnu.org/wiki/FormattingCodeForGCC
;; Kshitij Gaipal
;; Feb 20, 2015

;; This line is not actually needed, it is on by default.
(setq-default indent-tabs-mode t)

(add-hook 'c-mode-hook 'linux-c-mode)

(defun linux-c-mode()
  (setq c-basic-offset 2) ;; TAB offset set to 2
  (setq fill-column 80)
  (c-set-style "gnu"))
