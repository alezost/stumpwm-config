;;; mode-line-memory.lisp --- Memory info for the mode line

;; Copyright © 2025 Alex Kost <alezost@gmail.com>

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

;; Some code from this file originates from
;; <https://github.com/stumpwm/stumpwm-contrib/blob/master/modeline/mem>.

;;; Code:

(defpackage #:al/stumpwm-memory
  (:use :common-lisp
        :alexandria
        :stumpwm)
  (:export #:memory-mode-line-type
           #:memory-mode-line-types
           #:memory-mode-line-string))

(in-package #:al/stumpwm-memory)

(defvar memory-mode-line-types '(short long)
  "Available types for `memory-mode-line-string'.")

(defvar memory-mode-line-type (car memory-mode-line-types)
  "Current mode line string type.
Must be one of `memory-mode-line-types'.")

(defun memory-info ()
  "Return total and available memory values."
  (with-open-file (stream #P"/proc/meminfo" :if-does-not-exist nil)
    (when stream
      (do ((line (read-line stream nil nil)
                 (read-line stream nil nil))
           (total nil)
           (available nil))
          ((or (and total available)
               (null line))
           (values total available))
        (when-let* ((split (cl-ppcre:split "\\s*:\\s*" line))
                    (key   (first split))
                    (value (second split)))
          (cond
            ((string= key "MemTotal")
             (setf total (read-from-string value)))
            ((string= key "MemAvailable")
             (setf available (read-from-string value)))))))))

(defun format-float (num)
  "Return formatted string from the floating number NUM."
  (cond
    ((> 10 num)
     (format nil "~3,1F" num))
    (t
     (format nil "~3D" (round num)))))

(defun format-kilo-bytes (kb &rest args)
  "Return formatted string from the number of kilo-bytes KB.
The rest ARGS are passed to `al/ml-string'."
  (if (numberp kb)
      (let ((gb (/ kb 1e6)))
        (if (> gb 1)
            (concat (apply #'al/ml-string (format-float gb) args)
                    "G")
            (concat (apply #'al/ml-string (format-float (/ kb 1e3)) args)
                    "M")))
      ""))

(defun memory-mode-line-string ()
  "Return a string with memory info suitable for the mode-line.
TYPE can be one of the following symbols: `short', `long', `next'."
  (multiple-value-bind (total available)
      (memory-info)
    (let* ((used (- total available))
           (used% (round (* 100 (/ used total))))
           (used-str
             (concat (format-kilo-bytes used :fg "#e08880")
                     (al/ml-string "(" :reset t)
                     (al/ml-zone-string used%
                                        :zones '(90 70 50)
                                        :format "~2,'0D")
                     (al/ml-string ")" :reset t))))
      (al/ml-string
       (case memory-mode-line-type
         (short (concat " " used-str))
         (long
          (let (;;(stump-total (sb-ext:dynamic-space-size))
                (stump-used (sb-kernel:dynamic-usage)))
            (concat (format-kilo-bytes (/ stump-used 1e3))
                    " " used-str " "
                    (format-kilo-bytes available :fg "#78c078"))))
         (t ""))
       :fg "7"))))

;;; mode-line-memory.lisp ends here
