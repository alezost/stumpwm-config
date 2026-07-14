;;; ml-net.lisp --- Network info for the mode line

;; Copyright © 2009 Vitaly Mayatskikh
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
;; <https://github.com/stumpwm/stumpwm-contrib/blob/master/modeline/net>.
;; I do not like some things that module does, also I wanted to make a
;; more sophisticated formatting, so I adjusted it for my needs.

;; Meaning of "/sys/class/net/*" and "/sys/class/rfkill/rfkillN/*" files
;; can be found at:
;; <https://www.kernel.org/doc/Documentation/ABI/testing/sysfs-class-net>,
;; <https://www.kernel.org/doc/Documentation/ABI/stable/sysfs-class-rfkill>.

;;; Code:

(defpackage #:al/ml-net
  (:use :cl :stumpwm :al/hwinfo)
  (:import-from :al/stumpwm
                #:al/ml-string
                #:al/ml-zone-string)
  (:export #:next-device
           #:ml-string))

(in-package #:al/ml-net)

(defvar *devices*
  (delete "lo" (net-devices) :test #'equal)
  "List of available network devices (interfaces).")

(defvar *device*
  ;; At first, search for "wlp*" (wlan), then for "enp*" (eth).
  (or (find-if (lambda (name) (ppcre:scan "^w" name))
               *devices*)
      (find-if (lambda (name) (ppcre:scan "^e" name))
               *devices*))
  "Currently used network device.")

(defcommand next-device () ()
  "Set `*device*' to the next net device."
  (setf *device*
        (al:next-list-element *devices* *device*)))

(defun format-float (num)
  "Return formatted string from the floating number NUM."
  (cond
    ((>= 10 num)
     (format nil "~4,2F" num))
    ((>= 100 num)
     (format nil "~4,1F" num))
    (t
     (format nil "~4D" (round num)))))

(defun format-bytes (bytes)
  "Return formatted string from the number of BYTES."
  (if (numberp bytes)
      (let ((mb (/ bytes 1e6)))
        (if (> mb 1)
            (concat (format-float mb)
                    (al/ml-string "M" :fg "2"))
            (concat (format-float (/ bytes 1e3))
                    (al/ml-string "k" :reset t))))
      ""))

(defun ml-string ()
  "Return a string with NET info suitable for the mode-line."
  (multiple-value-bind (state down up)
      (net-state *device*)
    (let ((color (ecase state
                   (:up      "^b^6*")
                   (:down    "^B^5*")
                   (:soft    "^b^7*")
                   (:hard    "")
                   (:unknown "^B^1*"))))
      (concat
       (al/ml-string (concat color *device*))
       (and down up
            (al/ml-string (concat " " (format-bytes down)
                                  " " (format-bytes up))
                          :fg "7"))))))

;;; ml-net.lisp ends here
