;; Custom mode line setup, special thanks to Amit
;; http://amitp.blogspot.com/2011/08/emacs-custom-mode-line.html

;; slight color change makes easier to distinguish between active/inactive buffer's modeline
(set-face-attribute 'mode-line nil
		    :background "gray"
		    :foreground "black"
		    :box -1)

(set-face-attribute 'mode-line-inactive nil
		    :background "dim gray")

;; custom faces
(make-face 'mode-line-col-limit-exceeds-face)
(make-face 'mode-line-col-limit-exceeds-inactive-face)

(set-face-attribute 'mode-line-col-limit-exceeds-face nil
		    :inherit 'mode-line
		    :background "orange")
(set-face-attribute 'mode-line-col-limit-exceeds-inactive-face nil
		    :inherit 'mode-line-inactive
		    :background "orange")

;; set mode-line format
(setq-default
 mode-line-format
 '(
   ;; fuzzy position in the buffer
   (:propertize "%p|")

   ;; ;; absolute position in the buffer
   ;; (:propertize "-L:%l-")

   ;; absolute position, including warning for 100 columns
   (:eval
    (cond
     ((and (>= (current-column) 0)
	   (< (current-column) 10))
      (propertize "--%1c"))
     ((and (>= (current-column) 10)
	   (<= (current-column) 80))
      (propertize "-%2c"))
     ((> (current-column) 80)
      (propertize "%3c" 'face 'mode-line-col-limit-exceeds-face))
     (t (propertize "%3c"))))

   ;; remote file?
   (:propertize "-%1@")

   ;; read only buffer
   (:eval
    (cond (buffer-read-only "%%")
	  (t "-")))

   ;; modified buffer
   (:eval
    (cond ((buffer-modified-p) "*-")
	  (t "--")))

   ;; Buffer/file name
   (:propertize "%b" face mode-line-buffer-id)

   ;; which-func mode
   " - " which-func-format " -"

   ;; mode indicators: vc, recursive edit, major mode, minor modes, process, global
   (vc-mode vc-mode)
   "--"
   (:propertize mode-line-process)
   "-%["
   (:propertize mode-name)
   "%]"

   ;; (:eval (propertize (format-mode-line minor-mode-alist)))
   (global-mode-string global-mode-string)
   "%-"
   ))
