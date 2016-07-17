;;; setaudio.lisp --- Control audio parameters through "setaudio"

;; Copyright © 2013-2016 Alex Kost <alezost@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

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
