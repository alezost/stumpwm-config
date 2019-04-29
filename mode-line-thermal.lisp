;;; mode-line-thermal.lisp --- Thermal zones info for the mode line

;; Copyright © 2007 Anonymous Coward, Jonathan Moore Liles
;; Copyright © 2019 Alex Kost <alezost@gmail.com>

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

;; Documentation on "/sys/class/thermal/*" can be found at:
;; <https://github.com/torvalds/linux/blob/master/Documentation/thermal/sysfs-api.txt>

;;; Code:

(defpackage #:al/stumpwm-thermal
  (:use :common-lisp
        :stumpwm)
  (:export #:all-thermal-zones
           #:thermal-zones-mode-line-string))

(in-package #:al/stumpwm-thermal)

(defun all-thermal-zones ()
  "Return a list of files of all thermal zones."
  (remove nil
          (mapcar (lambda (dir)
                    (when (ppcre:scan "thermal_zone" (namestring dir))
                      (let ((file (make-pathname
                                   :directory (pathname-directory dir)
                                   :name "temp")))
                        (and (al/file-readable? file)
                             file))))
                  (list-directory #P"/sys/class/thermal/"))))

(defun thermal-zone-temperature (zone)
  "Return temperature of a thermal ZONE file."
  (round (/ (al/read-sys-file zone t)
            1000)))

(defun thermal-zones-mode-line-string (&rest zones)
  "Return a string with thermal ZONES info suitable for the mode-line."
  (if (null zones)
      ""
      (concat
       "^[^b^7*"
       (if (cdr zones)          ; not a single zone
           (apply #'concat
                  "°C:"
                  (mapcar (lambda (zone)
                            (format nil " ~D"
                                    (thermal-zone-temperature zone)))
                          zones))
           (format nil "~D°C"
                   (thermal-zone-temperature (car zones))))
       "^]")))

;;; mode-line-thermal.lisp ends here
