;;; sound.lisp --- Set sound parameters and show them in OSD

;; Copyright © 2013–2025 Alex Kost <alezost@gmail.com>

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

;;; Commentary:

;; This file provides a couple of commands to set sound parameters
;; (volume and muteness).  It looks mostly like a wrapper around
;; 'amixer' command, except that 'osd-sound' is called instead.
;;
;; This 'osd-sound' is a simple shell script that sends some Guile
;; expression to Guile-Daemon <https://github.com/alezost/guile-daemon>.
;; 2 things eventually happen: amixer is called and the sound value is
;; displayed in OSD.
;;
;; 'osd-sound' script can be found in my Guile-Daemon config:
;; <https://github.com/alezost/guile-daemon-config/blob/master/scripts/osd-sound>.

;;; Code:

(in-package :stumpwm)

(defvar al/sound-program "osd-sound"
  "Name of a program to be called with amixer arguments.")

(defvar al/sound-scontrols '("Master" "Line")
  "List of simple controls for managing.")

(defvar al/sound-current-scontrol-num 0
  "The number of the currently used simple control.")

(defvar al/sound-volume nil
  "Sound volume of the current simple control.
This variable should have (TIME VOLUME ON) value, where TIME is the
time (seconds since epoch) of the latest update, VOLUME is the latest
value of the sound volume and ON is a boolean value showing if the sound
is on or off (muted).")

(defun al/sound-get-current-scontrol ()
  "Return the current simple control from `al/sound-scontrols'."
  (nth al/sound-current-scontrol-num al/sound-scontrols))

(defun al/sound-get-next-scontrol ()
  "Return next simple control from `al/sound-scontrols'."
  (setf al/sound-current-scontrol-num
        (if (>= al/sound-current-scontrol-num
                (- (length al/sound-scontrols) 1))
            0
            (+ 1 al/sound-current-scontrol-num)))
  (al/sound-get-current-scontrol))

(defun al/sound-call (&rest args)
  "Execute `al/sound-program' using amixer ARGS."
  (run-prog al/sound-program
            :args args :wait nil :search t))

(defcommand al/sound-set-current-scontrol (&rest args) (:rest)
  "Set sound value for the current simple control.
ARGS are the rest amixer arguments after 'sset CONTROL'."
  (apply #'al/sound-call "sset" (al/sound-get-current-scontrol) args)
  (setf al/sound-volume (list (get-universal-time) nil nil)))

(defcommand al/sound-current-scontrol () ()
  "Show sound value of the current simple control."
  (al/sound-call "sget" (al/sound-get-current-scontrol)))

(defcommand al/sound-next-scontrol () ()
  "Switch simple control and show its sound value."
  (al/sound-call "sget" (al/sound-get-next-scontrol))
  (setf al/sound-volume (list (get-universal-time) nil nil)))

(defun al/sound-update-volume (&optional scontrol)
  "Update `al/sound-volume' with SCONTROL (\"Master\" by default)."
  (let* ((cmd (concat "amixer sget "
                      (or scontrol (al/sound-get-current-scontrol))
                      " | sed -rn '$s/[^[]+\\[([0-9]+)%\\].+\\[([a-z]+)\\].*/\\1 \\2/p'"))
         (output (run-shell-command cmd t))
         (res (first (split-string output '(#\newline))))
         (res (split-string res " "))
         (vol (first res))
         (on  (string= "on" (second res))))
    (setf al/sound-volume
          (list (get-universal-time) vol on))))

;;; sound.lisp ends here
