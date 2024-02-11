;; Tracks currently active MPRIS2 track and allows easy insertion into a buffer.
;;
;; Usage:
;;
;;   M-x mpris2-yank-current-track

(defun my/mpris2-property-changed (svc props _)
  (when-let ((metadata (alist-get "Metadata" props nil nil #'equal))
			 (metadata-1 (caar metadata)))
	(setq my/*mpris2-metadata* metadata-1)))

(defun my/mpris2-current-track ()
  "Returns the name of the last active MPRIS2 track"
  (let* ((metadata my/*mpris2-metadata*)
		 (xesam-artist (alist-get "xesam:artist" metadata nil nil #'equal))
		 (xesam-title (alist-get "xesam:title" metadata nil nil #'equal)))
	(format "%s - %s"
			(string-join (flatten-list xesam-artist) ", ")
			(string-join (flatten-list xesam-title)))))

(defun mpris2-yank-current-track ()
  "Inserts the name of the last active MPRIS2 track at point"
  (interactive)
  (insert (my/mpris2-current-track)))

;; Clean-up existing registration
(when (and (boundp 'my/*mpris2-registration*) my/*mpris2-registration*)
  (dbus-unregister-object my/*mpris2-registration*)
  (setq my/*mpris2-registration* nil))

;; Register handler for media events
(setq my/*mpris2-registration*
	  (dbus-register-signal
	   :session
	   nil
	   "/org/mpris/MediaPlayer2"
	   "org.freedesktop.DBus.Properties"
	   "PropertiesChanged"
	   #'my/mpris2-property-changed))
