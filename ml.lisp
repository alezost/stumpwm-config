;;; ml.lisp --- Utils for the mode line

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

;;; Code:

(defpackage :ml
  (:use :cl :al :al/hwinfo)
  (:export #:separate
           #:ml-string
           #:zone-string
           #:title-string
           #:cpu-string
           #:cpu-string-next-type
           #:memory-string
           #:memory-string-next-type
           #:net-string
           #:next-net-device
           #:battery-string
           #:thermal-string))

(in-package :ml)

(defun separate (&rest strings)
  "Concatenate mode-line STRINGS using separator."
  (apply #'concat " | " strings))

(defun ml-string (string &key fg bg (bright nil bright-set) reset click)
  ;; For details on the color and mode-line machinery, see
  ;; (info "(stumpwm) Colors") and (info "(stumpwm) Mode-line Interaction").
  "Make STRING a mode-line string with FG and BG colors.

FG and BG can be nil or a string containing either a single digit (a
number from `*colors*' list) or #XXXXXX value.

If BRIGHT is set and is non-nil, use bright color.

If RESET is non-nil, use \"^n\" construct and ignore other arguments.

CLICK should have ID or (ID [ARGS ...]) form.  It is used to make STRING
clickable."
  (flet ((color (raw)
           (if (= 1 (length raw))
               raw
               (concat "\"" raw "\""))))
    (let* ((str (if reset
                    (concat "^n" string)
                    (let ((fc (and (stringp fg)
                                   (concat "^(:fg " (color fg) ")")))
                          (bc (and (stringp bg)
                                   (concat "^(:bg " (color bg) ")"))))
                      (concat (and bright-set (if bright "^B" "^b"))
                              fc bc string))))
           (str (if click
                    (apply #'stumpwm:format-with-on-click-id
                           str
                           (if (listp click) click (list click)))
                    str)))
      (concat "^[" str "^]"))))

(defun zone-string (number &key (colors '("^B^1*" "^B^5*" "^B"))
                                   (zones '(90 60 30)) reverse
                                   (format "~3D") (ending "%%"))
  ;; This is a replacement for `bar-zone-color'.
  "Format NUMBER with FORMAT string, make it colored, and add ENDING to it.

ZONES is a descending list of integers that is compared to NUMBER.
COLORS is a list of respecting color specifications.
ZONES and COLORS lists must have the same length.

If REVERSE is non-nil, reverse the order of comparing ZONES and NUMBER."
  (let ((compare (if reverse #'<= #'>=))
        (num-str (format nil format number))
        (color nil))
    (do ((zones (if reverse (reverse zones) zones)
                (cdr zones))
         (colors colors (cdr colors)))
        ((or color (null zones))
         color)
      (when (funcall compare number (car zones))
        (setf color (car colors))))
    (concat (if color
                (ml-string (concat color num-str))
                num-str)
            ending)))

(defun title-string (string)
  "Make STRING a title string."
  (ml-string string :fg "#a0aa98"))

(defun format-hours (time)
  "Return 'HH:MM' string from TIME.
TIME is a floating number of hours."
  (if (numberp time)
      (multiple-value-bind (hours rem)
          (truncate time)
        (format nil "~D:~2,'0D" hours (floor (* rem 60))))
      ""))


;;; CPU

(defvar *cpu-string-types* '(short long)
  "Available types for `cpu-string'.")

(defvar *cpu-string-type* (car *cpu-string-types*)
  "Current mode line string type.
Must be one of `*cpu-string-types*'.")

(defun cpu-string-next-type ()
  "Set `*cpu-string-type*' to the next available type."
  (setf *cpu-string-type*
        (next-list-element *cpu-string-types*
                           *cpu-string-type*
                           #'eq)))

(defun cpu-string ()
  "Return a string with CPU info suitable for the mode-line."
  (multiple-value-bind (cpu user system io irq)
      (cpu-usage)
    (flet ((d (float)
             (round (* 100 float))))
      (let* ((cpu (d cpu))
             (cpu-str (zone-string cpu)))
        (ml-string
         (case *cpu-string-type*
           (short cpu-str)
           (long
            (let ((user   (d user))
                  (system (d system))
                  (io     (d io))
                  (irq    (d irq)))
              (concat cpu-str
                      (ml-string "(" :reset t)
                      (format nil "~2,'0D ~2,'0D ~D ~D"
                              user system io irq)
                      (ml-string ")" :reset t))))
           (t ""))
         :fg "7")))))


;;; Memory

(defvar *memory-string-types* '(short long)
  "Available types for `memory-string'.")

(defvar *memory-string-type* (car *memory-string-types*)
  "Current mode line string type.
Must be one of `*memory-string-types*'.")

(defun memory-string-next-type ()
  "Set `*memory-string-type*' to the next available type."
  (setf *memory-string-type*
        (next-list-element *memory-string-types*
                           *memory-string-type*
                           #'eq)))

(defun memory-format-float (num)
  "Return formatted string from the floating number NUM."
  (cond
    ((> 10 num)
     (format nil "~3,1F" num))
    (t
     (format nil "~3D" (round num)))))

(defun memory-format-kilo-bytes (kb &rest args)
  "Return formatted string from the number of kilo-bytes KB.
The rest ARGS are passed to `ml-string'."
  (if (numberp kb)
    (let ((gb (/ kb 1e6)))
      (if (> gb 1)
        (concat (apply #'ml-string (memory-format-float gb) args)
                "G")
        (concat (apply #'ml-string (memory-format-float (/ kb 1e3)) args)
                "M")))
    ""))

(defun memory-string ()
  "Return a string with memory info suitable for the mode-line.
TYPE can be one of the following symbols: `short', `long', `next'."
  (multiple-value-bind (total available)
      (memory-info)
    (let* ((used (- total available))
           (used% (round (* 100 (/ used total))))
           (used-str
             (concat (memory-format-kilo-bytes used :fg "#e08880")
                     (ml-string "(" :reset t)
                     (zone-string used%
                                  :zones '(90 70 50)
                                  :format "~2,'0D")
                     (ml-string ")" :reset t))))
      (ml-string
       (case *memory-string-type*
         (short (concat " " used-str))
         (long
          (let (;;(stump-total (sb-ext:dynamic-space-size))
                (stump-used (sb-kernel:dynamic-usage)))
            (concat (memory-format-kilo-bytes (/ stump-used 1e3))
                    " " used-str " "
                    (memory-format-kilo-bytes available :fg "#78c078"))))
         (t ""))
       :fg "7"))))


;;; Battery

(defun battery-string (battery)
  "Return a string with BATTERY info suitable for the mode-line."
  (multiple-value-bind (state percent time)
      (battery-state battery)
    (ml-string
     (ecase state
       (:unknown "(no info)")
       (:charged (concat (write-to-string percent) "%%"))
       ((:charging :discharging)
        (concat (zone-string percent :format "~2D" :reverse t)
                (if (eq state :charging)
                    (ml-string "+" :fg "2" :bright t)
                    (ml-string "-" :fg "1" :bright t))
                (format-hours time))))
     :fg "7")))


;;; Net

(defvar *net-devices*
  (delete "lo" (net-devices) :test #'equal)
  "List of available network devices (interfaces).")

(defvar *net-device*
  ;; At first, search for "wlp*" (wlan), then for "enp*" (eth).
  (or (find-if (lambda (name) (ppcre:scan "^w" name))
               *net-devices*)
      (find-if (lambda (name) (ppcre:scan "^e" name))
               *net-devices*))
  "Currently used network device.")

(defun next-net-device ()
  "Set `*net-device*' to the next net device."
  (setf *net-device*
        (next-list-element *net-devices* *net-device*)))

(defun net-format-float (num)
  "Return formatted string from the floating number NUM."
  (cond
    ((>= 10 num)
     (format nil "~4,2F" num))
    ((>= 100 num)
     (format nil "~4,1F" num))
    (t
     (format nil "~4D" (round num)))))

(defun net-format-bytes (bytes)
  "Return formatted string from the number of BYTES."
  (if (numberp bytes)
    (let ((mb (/ bytes 1e6)))
      (if (> mb 1)
        (concat (net-format-float mb)
                (ml-string "M" :fg "2"))
        (concat (net-format-float (/ bytes 1e3))
                (ml-string "k" :reset t))))
    ""))

(defun net-string ()
  "Return a string with NET info suitable for the mode-line."
  (multiple-value-bind (state down up)
      (net-state *net-device*)
    (let ((color (ecase state
                   (:up      "^b^6*")
                   (:down    "^B^5*")
                   (:soft    "^b^7*")
                   (:hard    "")
                   (:unknown "^B^1*"))))
      (concat
       (ml-string (concat color *net-device*))
       (and down up
            (ml-string (concat " " (net-format-bytes down)
                               " " (net-format-bytes up))
                       :fg "7"))))))


;;; Thermal zones

(defun thermal-string (&rest zones)
  "Return a string with thermal ZONES info suitable for the mode-line."
  (if (null zones)
    ""
    (flet ((temp (zone)
             (zone-string (thermal-zone-temperature zone)
                          :zones '(70 60 50)
                          :format "~2D"
                          :ending "")))
      (ml-string
       (if (cdr zones)
         (concat "°C: " (mapconcat #'temp zones " "))
         (concat (temp (car zones)) "°C"))
       :fg "7"))))

;;; ml.lisp ends here
