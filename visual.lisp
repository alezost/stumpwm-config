;;; visual.lisp --- Visual appearance: colors, fonts, mode line, ...

;; Copyright © 2013–2016, 2018–2022 Alex Kost <alezost@gmail.com>

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

(in-package :stumpwm)


;;; Colors

;; Yellow and magenta are swapped to show keys in yellow.
(setf *colors*
      '("black"                 ; 0
        "red"                   ; 1
        "green"                 ; 2
        "magenta"               ; 3
        "#44d0ff"               ; 4
        "yellow"                ; 5
        "cyan"                  ; 6
        "white"                 ; 7
        "AntiqueWhite3"
        "khaki3")
      *bar-hi-color* "^B^5*")
(update-color-map (current-screen))

(defmacro al/set-color (val color)
  "Similar to `set-any-color', but without updating colors."
  `(dolist (s *screen-list*)
     (setf (,val s) (alloc-color s ,color))))

(al/set-color screen-fg-color (hex-to-xlib-color "#e5e8ef"))
(al/set-color screen-bg-color "gray15")
(al/set-color screen-focus-color "black")
(al/set-color screen-unfocus-color "gray20")
(al/set-color screen-border-color "ForestGreen")
(al/set-color screen-float-focus-color "DeepSkyBlue")
(al/set-color screen-float-unfocus-color "gray15")
(update-colors-all-screens)


;;; Grabbed pointer

(setq
 *grab-pointer-character* 40
 *grab-pointer-character-mask* 41
 *grab-pointer-foreground* (hex-to-xlib-color "#3db270")
 *grab-pointer-background* (hex-to-xlib-color "#2c53ca"))


;;; mode-line auxiliary code

(defvar al/ml-separator " | ")

(defun al/ml-separate (str)
  "Concatenate `al/ml-separator' and STR."
  (concat al/ml-separator str))

(defun al/ml-string (str &key (fg "7") bg (bright nil bright-set))
  "Make STR a mode-line string with FG and BG colors.
FG and BG can be nil or a string containing either a single digit (a
number from `*colors*' list) or #XXXXXX value.
If BRIGHT is set and is non-nil, use bright color."
  ;; See (info "(stumpwm) Colors") for details on the color machinery.
  (let ((fc (and (stringp fg)
                 (concat "^(:fg "
                         (if (= 1 (length fg))
                             fg
                             (concat "\"" fg "\""))
                         ")")))
        (bc (and (stringp bg)
                 (concat "^(:bg "
                         (if (= 1 (length bg))
                             bg
                             (concat "\"" bg "\""))
                         ")"))))
    (concat "^[" (and bright-set (if bright "^B" "^b"))
            fc bc str "^]")))

(defun al/ml-title-string (str)
  "Make STR a title string."
  (al/ml-string str :fg "#a0aa98"))

(defun al/ml-window-class (&optional (str "%c"))
  "Window class color construct for mode-line and window list."
  (al/ml-string str :fg "#e0e044" :bg "#4d4da0"))


;;; mode-line date

(defvar al/date-refresh-time 120)

(al/defun-with-delay
 al/date-refresh-time al/ml-date ()
 (al/ml-string (time-format "%a %d %b")))


;;; mode-line cpu

(al/load "mode-line-cpu")

(defvar al/cpu-refresh-time 3)

(al/defun-with-delay
 al/cpu-refresh-time al/ml-cpu ()
 (al/ml-separate
  (concat (al/ml-title-string "CPU")
          (al/stumpwm-cpu:cpu-mode-line-string))))


;;; mode-line thermal

(al/load "mode-line-thermal")

(defvar al/thermal-zone
  (car (al/stumpwm-thermal:all-thermal-zones)))

(defvar al/thermal-zones-refresh-time 6)

(al/defun-with-delay
 al/thermal-zones-refresh-time al/ml-thermal-zones ()
 (al/ml-separate
  (al/stumpwm-thermal:thermal-zones-mode-line-string al/thermal-zone)))

(defun al/ml-thermal-zones-maybe ()
  (if al/thermal-zone
      (al/ml-thermal-zones)
      ""))


;;; mode-line net

(al/load "mode-line-net")

(defvar al/net-refresh-time 6)

(al/defun-with-delay
 al/net-refresh-time al/ml-net ()
 (al/ml-separate (al/stumpwm-net:net-mode-line-string)))

(defcommand al/mode-line-next-net-device () ()
  "Set next net device in the mode-line."
  (setf al/stumpwm-net:net-device
        (al/next-list-element al/stumpwm-net:net-devices
                              al/stumpwm-net:net-device)))


;;; mode-line battery

(al/load "mode-line-battery")

(defvar al/battery (car (al/stumpwm-battery:all-batteries)))

(defvar al/battery-refresh-time 60)

(al/defun-with-delay
 al/battery-refresh-time al/ml-battery ()
 (al/ml-separate
  (concat (al/ml-title-string "Bat ")
          (al/stumpwm-battery:battery-mode-line-string al/battery))))

(defun al/ml-battery-maybe ()
  (if al/battery
      (al/ml-battery)
      ""))


;;; mode-line backlight

(defvar al/ml-backlight nil)

(defun al/ml-backlight-string (bl)
  (and (stringp bl)
       (not (string= "" bl))
       (al/ml-separate
        (concat (al/ml-title-string "BL ")
                (al/ml-string bl :fg "#66bbff")))))

(defun al/ml-backlight ()
  (when (null al/backlight)
    (al/update-backlight))
  (let ((bl      (car al/backlight))
        (bl-time (cdr al/backlight))
        (ml-time (cdr al/ml-backlight)))
    (if bl
        (when (or (null ml-time) (> bl-time ml-time))
          (setf al/ml-backlight
                (cons (al/ml-backlight-string bl) bl-time)))
        ;; Do not refresh mode-line for 2 seconds after the backlight update.
        (when (> (get-universal-time) (+ 2 bl-time))
          (al/update-backlight)
          (setf al/ml-backlight
                (cons (al/ml-backlight-string (car al/backlight))
                      (cdr al/backlight))))))
  (car al/ml-backlight))


;;; mode-line sound

(defvar al/ml-sound nil)
(defvar al/sound-refresh-time 30)

(defun al/ml-sound-string (vol)
  (and (stringp vol)
       (not (string= "" vol))
       (al/ml-separate
        (concat (al/ml-title-string "Snd ")
                (al/ml-string vol :fg "#50e050")))))

(defun al/ml-sound ()
  (when (null al/sound-volume)
    (al/sound-update-volume))
  (let ((vol      (car al/sound-volume))
        (vol-time (cdr al/sound-volume))
        (ml-time  (cdr al/ml-sound))
        (now      (get-universal-time)))
    (if (or (and (null vol)
                 ;; Do not refresh mode-line for 2 seconds after the
                 ;; latest sound update.
                 (> now (+ 2 vol-time)))
            (and ml-time
                 (> (get-universal-time)
                    (+ ml-time al/sound-refresh-time))))
        (progn
          (al/sound-update-volume)
          (setf al/ml-sound
                (cons (al/ml-sound-string (car al/sound-volume))
                      (cdr al/sound-volume))))
        (when (and vol
                   (or (null ml-time)
                       (> vol-time ml-time)))
          (setf al/ml-sound
                (cons (al/ml-sound-string vol) vol-time)))))
  (car al/ml-sound))


;;; mode-line keyboard

(defun al/ml-locks ()
  (defun bool->color (bool)
    (if bool "^B^2" ""))
  (let ((mods (xlib:device-state-locked-mods
               (xlib:get-state *display*))))
    (al/ml-separate
     (format nil "^[~ACaps^] ^[~ANum^]"
             (bool->color (al/mod-lock-state +caps-lock+ mods))
             (bool->color (al/mod-lock-state +num-lock+ mods))))))

(defun al/ml-layout ()
  (al/ml-separate
   (al/ml-string (al/layout-string (al/current-layout)))))


;;; mode-line windows

(defvar al/window-alist nil
  "Alist of (CLASS . NUM) pairs of the available windows.
CLASS is a window class; NUM is the number of windows of this class.")

(defvar al/current-window nil
  "Class of the current window.")

(defun al/update-window-alist (&rest _)
  "Refresh the value of `al/window-alist'."
  (declare (ignore _))
  (setf al/window-alist nil)
  (mapc (lambda (w)
          (let* ((wc (window-class w))
                 (entry (assoc wc al/window-alist :test #'string=)))
            (if entry
                (setf (cdr entry)
                      (1+ (or (cdr entry) 1)))
                (push (list wc) al/window-alist))))
        (screen-windows (current-screen))))

(defun al/update-current-window (&rest _)
  "Refresh the value of `al/current-window'."
  (declare (ignore _))
  (setf al/current-window
        (window-class (current-window))))

(add-hook *focus-window-hook*   'al/update-current-window)
(add-hook *new-window-hook*     'al/update-window-alist)
(add-hook *destroy-window-hook* 'al/update-window-alist)

(defun al/ml-windows ()
  (when (and al/window-alist al/current-window)
    (al/ml-separate
     (format
      nil "~{~A~^ ~}"
      (mapcar (lambda (assoc)
                (destructuring-bind (class . num)
                    assoc
                  (concat
                   (if (string= class al/current-window)
                       (al/ml-window-class (concat " " class " "))
                       (al/ml-string (concat " " class " ")
                                     :fg "#a0a0a0" :bg "#555555"))
                   (and num
                        (al/ml-string (concat " " (write-to-string num) " ")
                                      :bg "#407777")))))
              al/window-alist)))))


;;; Visual appearance and mode-line settings

(setf
 *window-format* (concat "%m%n%s" (al/ml-window-class) " %70t")
 *window-info-format* (concat "^>" (al/ml-window-class)
                              "^b^6* %w^7*x^6*%h"
                              '(#\newline) "^7*%t")
 *time-format-string-default*
 (format nil "^5*%k:%M:%S~%^2*%A~%^7*%d %B~%^7*%d.%m.%Y")

 *time-modeline-string* "%k:%M"
 *mode-line-timeout* 5
 *screen-mode-line-format*
 '((:eval (al/ml-date))
   " ^[^5*%d^]"                 ; time
   " ^[^2*%n^]"                 ; group name
   (:eval (al/ml-cpu))
   (:eval (al/ml-thermal-zones-maybe))
   (:eval (al/ml-net))
   (:eval (al/ml-battery-maybe))
   (:eval (al/ml-windows))
   "^>"
   (:eval (al/ml-sound))
   (:eval (al/ml-backlight))
   (:eval (al/ml-layout))
   (:eval (al/ml-locks))))

(al/mode-line-on)


;;; Time format

;; `time-hour' uses "~2,D" format which leads to an extra space if the
;; current hour is a one-digit integer.  I don't like this extra space
;; (i.e., I prefer "9:40" instead of " 9:40").
;; The same for `time-day-of-month'.

(defun al/time-hour ()
  (format nil "~D" (getf (time-plist) :hour)))

(defun al/time-day-of-month ()
  (format nil "~D" (getf (time-plist) :dom)))

(defun al/time-set-format-alist (char symbol)
  "Replace function name for CHAR by SYMBOL in `*time-format-string-alist*'."
  (setf (cadr (find-if (lambda (elt)
                         (eq char (car elt)))
                       *time-format-string-alist*))
        symbol))

(al/time-set-format-alist #\k 'al/time-hour)
(al/time-set-format-alist #\d 'al/time-day-of-month)


;;; Fonts

(or (set-font "-*-terminus-bold-r-normal-*-22-*-*-*-*-*-*-*")
    (set-font "9x15bold"))

;;; visual.lisp ends here
