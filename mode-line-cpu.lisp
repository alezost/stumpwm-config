;;; mode-line-cpu.lisp --- CPU info for the mode line

;; Copyright © 2007 Anonymous Coward, Jonathan Moore Liles
;; Copyright © 2019–2025 Alex Kost <alezost@gmail.com>

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

;; This file originates from
;; <https://github.com/stumpwm/stumpwm-contrib/blob/master/modeline/cpu>.
;; I wanted to display a more sophisticated CPU info, so I adjusted that
;; file for my needs.
;;
;; Info on CPU usage ("/proc/stat" file) is taken from "man proc".

;;; Code:

(defpackage #:al/stumpwm-cpu
  (:use :common-lisp
        :stumpwm)
  (:export #:cpu-mode-line-type
           #:cpu-mode-line-types
           #:cpu-mode-line-string))

(in-package #:al/stumpwm-cpu)

(defvar cpu-mode-line-types '(short long)
  "Available types for `cpu-mode-line-string'.")

(defvar cpu-mode-line-type (car cpu-mode-line-types)
  "Current mode line string type.
Must be one of `cpu-mode-line-types'.")

(defvar last-user-time 0)
(defvar last-system-time 0)
(defvar last-idle-time 0)
(defvar last-iowait-time 0)
(defvar last-irq-time 0)

(defun current-cpu-usage ()
  "Return the average CPU usage since the last call.
1st value is a total percent of CPU usage.
2nd value is percent of CPU time spent in user mode.
3rd value is percent of CPU time spent in system mode.
4th value is percent of CPU time spent waiting for IO to complete.
5th value is percent of CPU time spent servicing interrupts."
  (let ((cpu% 0)
        (user% 0)
        (system% 0)
        (io% 0)
        (irq% 0))
    (with-open-file (stat #P"/proc/stat" :direction :input)
      (read stat)               ; read the first "cpu" word
      (let* ((cur-user-time   (+ (read stat) (read stat)))
             (cur-system-time (read stat))
             (cur-idle-time   (read stat))
             (cur-iowait-time (read stat))
             (cur-irq-time    (+ (read stat) (read stat)))
             (user-time       (- cur-user-time   last-user-time))
             (system-time     (- cur-system-time last-system-time))
             (idle-time       (- cur-idle-time   last-idle-time))
             (iowait-time     (- cur-iowait-time last-iowait-time))
             (irq-time        (- cur-irq-time    last-irq-time))
             (cpu-time        (+ user-time system-time iowait-time irq-time))
             (total-time      (+ cpu-time idle-time)))
        (unless (zerop total-time)
          (setf cpu%    (/ cpu-time    total-time)
                user%   (/ user-time   total-time)
                system% (/ system-time total-time)
                io%     (/ iowait-time total-time)
                irq%    (/ irq-time    total-time)
                last-user-time   cur-user-time
                last-system-time cur-system-time
                last-idle-time   cur-idle-time
                last-iowait-time cur-iowait-time
                last-irq-time    cur-irq-time))))
    (values cpu% user% system% io% irq%)))

(defun cpu-mode-line-string ()
  "Return a string with CPU info suitable for the mode-line."
  (multiple-value-bind (cpu user system io irq)
      (current-cpu-usage)
    (flet ((d (float)
             (round (* 100 float))))
      (let* ((cpu (d cpu))
             (cpu-str (al/ml-zone-string cpu)))
        (al/ml-string
         (case cpu-mode-line-type
           (short cpu-str)
           (long
            (let ((user   (d user))
                  (system (d system))
                  (io     (d io))
                  (irq    (d irq)))
              (concat cpu-str
                      (al/ml-string "(" :reset t)
                      (format nil "~2,'0D ~2,'0D ~D ~D"
                              user system io irq)
                      (al/ml-string ")" :reset t))))
           (t ""))
         :fg "7")))))

;;; mode-line-cpu.lisp ends here
