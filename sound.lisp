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

(defvar al/sound-scontrol (car al/sound-scontrols)
  "Currently used simple control.")

(al/defun-with-delay 60 al/sound-volume (&optional (scontrol al/sound-scontrol))
  "Return sound value for SCONTROL.
This function checks sound only if more than 60 seconds passed since the
last call.  If you wish to force the update, set `al/sound-volume-update'
variable to t.

Returned value is (VOLUME ON) list, VOLUME is a string with numeric
value of the sound volume and ON is a boolean value showing if the sound
is on or off (muted)."
  (let* ((cmd (concat "amixer sget "
                      scontrol
                      " | sed -rn '$s/[^[]+\\[([0-9]+)%\\].+\\[([a-z]+)\\].*/\\1 \\2/p'"))
         (output (run-shell-command cmd t))
         (res (first (split-string output '(#\newline))))
         (res (split-string res " "))
         (vol (first res))
         (on  (string= "on" (second res))))
    (list vol on)))

(defun al/sound-update-soon ()
  "Update mode line after some delay.
This delay is needed after calling `al/sound-call' to give the sound
machinery some time to be really set."
  (unless al/sound-volume-update
    (al/run-after-sleep 2
      (setf al/sound-volume-update t)
      (update-all-mode-lines))))

(defun al/sound-call (&rest args)
  "Execute `al/sound-program' using amixer ARGS."
  (run-prog al/sound-program
            :args args :wait nil :search t))

(defcommand al/sound-set-current-scontrol (&rest args) (:rest)
  "Set sound value for the current simple control.
ARGS are the rest amixer arguments after 'sset CONTROL'."
  (apply #'al/sound-call "sset" al/sound-scontrol args)
  (al/sound-update-soon))

(defcommand al/sound-current-scontrol () ()
  "Show sound value of the current simple control."
  (al/sound-call "sget" al/sound-scontrol))

(defcommand al/sound-next-scontrol () ()
  "Switch simple control and show its sound value."
  (al/sound-call "sget"
                 (setf al/sound-scontrol
                       (al/next-list-element al/sound-scontrols
                                             al/sound-scontrol)))
  (setf al/sound-volume-update t))

;;; sound.lisp ends here
