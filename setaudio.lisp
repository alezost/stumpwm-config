;;; setaudio.lisp --- control audio parameters through "setaudio"

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 28 May 2013

;;; Code:

(in-package :stumpwm)

(defvar *audio-program* "setaudio"
  "Name of 'setaudio' program.")

(defvar *audio-scontrols* '("Master" "PCM" "Line")
  "List of simple controls for managing.")

(defvar *audio-current-scontrol-num* 0
  "The number of currently used simple control.")

(defun audio-get-current-scontrol ()
  "Return current simple control from `*audio-scontrols*'."
  (nth *audio-current-scontrol-num* *audio-scontrols*))

(defun audio-get-next-scontrol ()
  "Return next simple control from `*audio-scontrols*'."
  (setq *audio-current-scontrol-num*
        (if (>= *audio-current-scontrol-num*
                (- (length *audio-scontrols*) 1))
            0
            (+ 1 *audio-current-scontrol-num*)))
  (audio-get-current-scontrol))

(defun audio-set (&optional args ctl no-osd)
  "Execute command for setting audio values for simple control CTL.
If CTL is nil, use the current one.
If NO-OSD is non-nil, do not show osd with current audio settings.
ARGS is a list of other arguments passed to `*audio-program*'."
  (run-prog
   *audio-program*
   :args (append (unless no-osd '("--show-osd"))
                 (list "--quiet"
                       (or ctl (audio-get-current-scontrol)))
                 args)
   :wait nil :search t))

(defcommand audio-set-current-scontrol (&rest args) (:rest)
  "Set (or show without ARGS) audio values for current simple control."
  (audio-set args))

(defcommand audio-next-scontrol () ()
  "Switch simple control."
  (audio-set nil (audio-get-next-scontrol)))

;;; setaudio.lisp ends here
