;; gnus configuration
;; Kshitij Gaipal

;; desktop notification
(load-file "~/.emacs.d/gnus-notify.el")

;; Identification
(setq user-full-name "Kshitij Gaipal"
      user-mail-address "kgaipal@bomgar.com")

;; Primary IMAP account
(require 'nnir)
(setq gnus-select-method
      '(nnimap "office"
	       (nnimap-address "outlook.office365.com")
	       (nnimap-server-port 993)
	       (nnir-search-engine imap)
	       (nnimap-stream ssl)))

;; Sendmail options
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-auth-credentials '(("smtp.office365.com" "587" "kgaipal@bomgar.com" nil))
      smtpmail-default-smtp-server "smtp.office365.com"
      smtpmail-smtp-server "smtp.office365.com"
      smtpmail-smtp-service 587
      smtpmail-starttls-credentials '(("smtp.office365.com" "587" nil nil)))

;; preserve current window layout while opening Gnus windows
(setq gnus-use-full-window nil)

;; make gnus faster
;; https://www.gnu.org/software/emacs/manual/html_node/efaq/Making-Gnus-faster.html
(setq gnus-check-new-newsgroups nil
      gnus-check-bogus-newsgroups nil
      gnus-show-threads nil
      gnus-use-cross-reference nil
      gnus-nov-is-evil nil)

;; gnus: check email periodically
;; http://stackoverflow.com/q/1053245
(gnus-demon-add-handler 'gnus-group-get-new-news 15 t)

;; notification for new emails as per http://stackoverflow.com/a/9611516
(setq gnus-parameters
      '(("INBOX"
	 (gnus-use-adaptive-scoring nil)
	 (gnus-use-scoring nil)
	 (visible . t)
	 (display . all)
	 (modeline-notify . t))))

;; See http://blog.binchen.org/posts/notes-on-using-gnus.html

;; misc configuration
(set-face-attribute 'gnus-group-mail-3-empty-face nil
		    :foreground "dark magenta"
		    :box 1
		    :weight 'bold)

;; default "%U%R%z%I%(%[%4L: %-23,23f%]%) %s\n"
(setq gnus-summary-line-format "%U%R%z%I%(%[%P %d: %-23,23f%]%) %s\n"
      gnus-summary-normal-read '(t (:foreground "DarkGray")))
