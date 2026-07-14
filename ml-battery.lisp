;;; ml-battery.lisp --- Battery info for the mode line

;; Copyright © 2008 Julian Stecklina
;; Copyright © 2018–2025 Alex Kost <alezost@gmail.com>

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
;; <https://github.com/stumpwm/stumpwm-contrib/blob/master/modeline/battery-portable>.
;; I do not like some things that module does, so I adjusted it for my
;; needs.

;; Meaning of "/sys/class/power_supply/<BAT>/*" files can be found at
;; <https://www.kernel.org/doc/Documentation/ABI/testing/sysfs-class-power>.

;;; Code:

(defpackage #:al/ml-battery
  (:use :cl :al/hwinfo)
  (:import-from :al
                #:concat)
  (:import-from :al/stumpwm
                #:al/ml-string
                #:al/ml-zone-string)
  (:export #:ml-string))

(in-package #:al/ml-battery)

(defun format-hours (time)
  "Return 'HH:MM' string from TIME.
TIME is a floating number of hours."
  (if (numberp time)
      (multiple-value-bind (hours rem)
          (truncate time)
        (format nil "~D:~2,'0D" hours (floor (* rem 60))))
      ""))

(defun ml-string (battery)
  "Return a string with BATTERY info suitable for the mode-line."
  (multiple-value-bind (state percent time)
      (battery-state battery)
    (al/ml-string
     (ecase state
       (:unknown "(no info)")
       (:charged (concat (write-to-string percent) "%%"))
       ((:charging :discharging)
        (concat (al/ml-zone-string percent :format "~2D" :reverse t)
                (if (eq state :charging)
                    (al/ml-string "+" :fg "2" :bright t)
                    (al/ml-string "-" :fg "1" :bright t))
                (format-hours time))))
     :fg "7")))

;;; ml-battery.lisp ends here
