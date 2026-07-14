;;; ml-cpu.lisp --- CPU info for the mode line

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

(defpackage #:al/ml-cpu
  (:use :cl :al/hwinfo)
  (:import-from :al
                #:concat)
  (:import-from :al/stumpwm
                #:al/ml-string
                #:al/ml-zone-string)
  (:export #:ml-next-type
           #:ml-string))

(in-package #:al/ml-cpu)


;;; mode-line string

(defvar *ml-types* '(short long)
  "Available types for `ml-string'.")

(defvar *ml-type* (car *ml-types*)
  "Current mode line string type.
Must be one of `*ml-types*'.")

(defun ml-next-type ()
  "Set `*ml-type*' to the next available type."
  (setf *ml-type*
        (al:next-list-element *ml-types* *ml-type* #'eq)))

(defun ml-string ()
  "Return a string with CPU info suitable for the mode-line."
  (multiple-value-bind (cpu user system io irq)
      (cpu-usage)
    (flet ((d (float)
             (round (* 100 float))))
      (let* ((cpu (d cpu))
             (cpu-str (al/ml-zone-string cpu)))
        (al/ml-string
         (case *ml-type*
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

;;; ml-cpu.lisp ends here
