;;; mode-line-battery.lisp --- Battery info for the mode line

;; Copyright © 2008 Julian Stecklina
;; Copyright © 2018–2019 Alex Kost <alezost@gmail.com>

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

;;; Code:

(defpackage #:al/stumpwm-battery
  (:use :common-lisp
        :stumpwm)
  (:export #:all-batteries
           #:battery-mode-line-string))

(in-package #:al/stumpwm-battery)

(defun power-supply-parameter (path name &optional to-number)
  "Return a line (string) from 'PATH/NAME' sysfs file.
If TO-NUMBER is non-nil, convert this string into a number.
Return nil in case of any error."
  (stumpwm::al/read-sys-file
   (merge-pathnames (make-pathname :name name)
                    path)
   to-number))

(defun all-batteries ()
  "Return a list of file paths of all batteries."
  (remove nil
          (mapcar (lambda (path)
                    (handler-case
                        (when (string= "Battery"
                                       (power-supply-parameter path "type"))
                          path)
                      (file-error () nil)))
                  (list-directory "/sys/class/power_supply/"))))

(defun battery-time-left (battery &optional (type :charging))
  "Return estimated time left for BATTERY to become empty.
Returned time is a floating number of hours.
Return nil, if it is impossible to calculate.
Calculation is performed depending on TYPE, which should be either
`:charging' or `:discharging'."
  (let ((consumed (or (power-supply-parameter battery "current_now" t)
                      (power-supply-parameter battery "power_now" t))))
    (if (zerop consumed)
        0
        (let ((left (or (power-supply-parameter battery "charge_now" t)
                        (power-supply-parameter battery "energy_now" t))))
          (case type
            (:discharging (/ left consumed))
            (:charging
             (let ((full (or (power-supply-parameter
                              battery "charge_full" t)
                             (power-supply-parameter
                              battery "energy_full" t))))
               (/ (- full left) consumed))))))))

(defun battery-state (battery)
  (if (string= (power-supply-parameter battery "present") "0")
      :unknown
      (let ((state   (power-supply-parameter battery "status"))
            (percent (power-supply-parameter battery "capacity" t)))
        (cond
          ((string= state "Full") (values :charged percent))
          ((string= state "Discharging")
           (values :discharging percent
                   (battery-time-left battery :discharging)))
          ((string= state "Charging")
           (values :charging percent
                   (battery-time-left battery :charging)))
          (t :unknown)))))

(defun format-hours (time)
  "Return 'HH:MM' string from TIME.
TIME is a floating number of hours."
  (if (numberp time)
      (multiple-value-bind (hours rem)
          (truncate time)
        (format nil "~D:~2,'0D" hours (floor (* rem 60))))
      ""))

(defun battery-mode-line-string (battery)
  "Return a string with BATTERY info suitable for the mode-line."
  (multiple-value-bind (state percent time)
      (battery-state battery)
    (ecase state
      (:unknown (format nil "(no info)"))
      (:charged (format nil "~D%%" percent))
      ((:charging :discharging)
       (format nil "^[~A~D%%~A^]~A"
               (bar-zone-color percent 90 60 30 t)
               percent
               (if (eq state :charging) "^B^2+" "^B^1-")
               (format-hours time))))))

;;; mode-line-battery.lisp ends here
