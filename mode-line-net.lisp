;;; mode-line-net.lisp --- Network info for the mode line

;; Copyright © 2009 Vitaly Mayatskikh
;; Copyright © 2019–2021 Alex Kost <alezost@gmail.com>

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

(defpackage #:al/stumpwm-net
  (:use :common-lisp
        :stumpwm)
  (:export #:net-devices
           #:net-device
           #:net-mode-line-string))

(in-package #:al/stumpwm-net)

(defvar net-devices
  (delete "lo"
          (mapcar (lambda (dir)
                    ;; Is there a better way to do this?
                    (first (last (pathname-directory dir))))
                  (uiop:subdirectories "/sys/class/net/"))
          :test #'equal)
  "List of available network devices (interfaces).")

(defvar net-device
  ;; At first, search for "wlp*" (wlan), then for "enp*" (eth).
  (or (find-if (lambda (name) (ppcre:scan "^w" name))
               net-devices)
      (find-if (lambda (name) (ppcre:scan "^e" name))
               net-devices))
  "Currently used network device.")

(defun net-device-file-name (&optional (device net-device))
  "Return sysfs file name of the DEVICE."
  (concat "/sys/class/net/" device))

(defun net-device-parameter (file-name &key (device net-device)
                                            to-number)
  "Return a line (string) from '/sys/class/net/DEVICE/FILE-NAME'.
If DEVICE is nil, use `net-device'.
If TO-NUMBER is non-nil, convert this string into a number.
Return nil in case of any error."
  (al/read-sys-file
   (concat (net-device-file-name device) "/" file-name)
   to-number))

(defvar net-rfkill-dirs nil
  "Alist of (DEVICE . RFKILL-DIR) pairs.")

(defun net-rfkill-dir (device)
  "Return the sysfs rfkill directory for the network DEVICE."
  (let ((assoc (assoc device net-rfkill-dirs)))
    (if assoc
        (cdr assoc)
        (let ((dir (car (directory (concat (net-device-file-name device)
                                           "/phy*/rfkill*")))))
          (push (cons device dir) net-rfkill-dirs)
          dir))))

(defun net-rfkill-state (&optional (device net-device))
  "Return the current rfkill state of the network DEVICE.
If the interface is blocked, return `:hard' or `:soft'.
Otherwise, return nil."
  (let ((dir (net-rfkill-dir device)))
    (defun blocked? (type)
      (not (zerop (al/read-sys-file
                   (merge-pathnames dir type) t))))
    (and dir
         (or (and (blocked? "hard") :hard)
             (and (blocked? "soft") :soft)))))

(defvar last-rx 0)
(defvar last-tx 0)
(defvar last-time 0)

(defun net-state (&optional (device net-device))
  "Return values for the current state of the network DEVICE.

If the interface is 'rfkill'-ed, return `:soft' or `:hard'.

If the interface is down, return `:down'.

If the interface is up, return `:up download-speed upload-speed' values.

Otherwise, return `:unknown' value."
  (or (net-rfkill-state device)
      (let ((state (net-device-parameter "operstate"
                                         :device device)))
        (cond
          ((string= state "down")
           :down)
          ((string= state "up")
           (let* ((now (/ (get-internal-real-time)
                          internal-time-units-per-second))
	          (rx (net-device-parameter "statistics/rx_bytes"
                                            :device device
                                            :to-number t))
	          (tx (net-device-parameter "statistics/tx_bytes"
                                            :device device
                                            :to-number t))
                  (dt (- now last-time))
                  (drx (- rx last-rx))
                  (dtx (- tx last-tx)))
             (setq last-rx rx
	           last-tx tx
	           last-time now)
             (values :up
                     (round (/ drx dt))
	             (round (/ dtx dt)))))
          (t :unknown)))))

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
            (concat (format-float mb) "^[^2M^]")
            (concat (format-float (/ bytes 1e3)) "^[^nk^]")))
      ""))

(defun net-mode-line-string ()
  "Return a string with NET info suitable for the mode-line."
  (multiple-value-bind (state down up)
      (net-state net-device)
    (let ((fmt-device (ecase state
                        (:up      "^b^6*~A")
                        (:down    "^B^5*~A")
                        (:soft    "^b^7*~A")
                        (:hard    "~A")
                        (:unknown "^B^1*~A"))))
      (concat "^["
              (if (and down up)
                  (format nil (concat fmt-device "^7 ~A ~A")
                          net-device
                          (format-bytes down)
                          (format-bytes up))
                  (format nil fmt-device net-device))
              "^]"))))

;;; mode-line-net.lisp ends here
