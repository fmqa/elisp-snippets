;; Pipes Erc messages into a TTS program.
;;
;; Usage:
;;   M-x erc-tts-start
;;   then specify the command line for your TTS program
;;
;; The "TTS program" must be a executable that reads lines to speak on STDIN
;; and outputs sound.
;;
;; For piper-tts aka. https://github.com/rhasspy/piper the following shell wrapper can be used:
;;
;; LISTING 1 `piper-wrapper' SCRIPT
;;
;;   #!/bin/bash
;;   set -eu
;;
;;   cleanup() {
;;       test -z "${WORKSPACE:-}" || rm -rf -- "${WORKSPACE}"
;;   }
;;
;;   ROOTDIR=$(dirname "$0")
;;   WORKSPACE=$(mktemp -d --suffix=-piper-$$)
;;
;;   trap cleanup INT TERM HUP QUIT EXIT
;;
;;   "${ROOTDIR}/piper" -d "${WORKSPACE}" "$@" | while read -r WAV
;;   do
;;       aplay "${WAV}"
;;       unlink "${WAV}"
;;   done
;;
;; The above script should be placed in the piper-tts root directory, where the `piper' binary
;; is located. The command line passed should be similar to e.g.
;;
;;   piper-wrapper --model <model path>
;;
;; Where <model path> is e.g. en_US-amy-low.onnx (or a different voice of your choosing).
;; The aplay command may be replaced with any command that plays RIFF WAV files.

(defvar erc-tts--last-channel "" "Last channel name handled by erc-tts")
(defvar erc-tts--last-speaker "" "Last speaker name handled by erc-tts")
(defvar erc-tts--process
  nil
  "TTS subprocess. Should be a filter-like program that accepts lines to speak from STDIN")

(defun erc-tts--dispatch (text)
  (when (and erc-tts--process (process-live-p erc-tts--process))
    (process-send-string erc-tts--process (format "%s\n" text))))

(defun erc-tts--do (&rest args)
  (if-let ((speaker-bounds (erc--get-speaker-bounds)))
      (let* ((speaker-start (car speaker-bounds))
             (speaker-end (cdr speaker-bounds))
             (msg-start (min (point-max) (1+ speaker-end)))
             (msg-end (or (next-single-property-change msg-start 'field) (point-max)))
             (msg (string-trim (substring-no-properties (buffer-substring msg-start msg-end))))
             (speaker (substring-no-properties (buffer-substring speaker-start speaker-end)))
             (channel-name (buffer-name))
             (is-action (eq ?\* (char-after)))
             (omit-channel
              (let ((channel-name-eq (string-equal channel-name erc-tts--last-channel)))
                (setq erc-tts--last-channel channel-name)
                channel-name-eq))
             (omit-speaker
              (let ((speaker-name-eq (string-equal speaker erc-tts--last-speaker)))
                (setq erc-tts--last-speaker speaker)
                speaker-name-eq)))
        (erc-tts--dispatch
         (if omit-channel
             (if is-action
                 (format "%s %s" speaker msg)
               (if omit-speaker msg (format "%s says: %s" speaker msg)))
           (format (if is-action "On %s, %s %s" "On %s, %s says: %s") channel-name speaker msg))))))

(defun erc-tts--sentinel (process event)
  (princ (format "Process %s %s" process event) (process-buffer process))
  (when (not (process-live-p process))
    (setq erc-tts--process nil)))

(defun erc-tts-start (program)
  (interactive "sTTS program: ")
  (when (and (not erc-tts--process) program)
    (when (setq erc-tts--process (start-process-shell-command "erc-tts" "*erc-tts*" program))
      (set-process-sentinel erc-tts--process #'erc-tts--sentinel)
      (add-hook 'erc-insert-post-hook #'erc-tts--do))))

(defun erc-tts-stop ()
  (interactive)
  (when erc-tts--process
    (process-send-eof erc-tts--process))
  (remove-hook 'erc-insert-post-hook #'erc-tts--do)
  (setq erc-tts--last-channel "")
  (setq erc-tts--last-speaker ""))
