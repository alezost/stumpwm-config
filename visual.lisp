;;; visual.lisp --- Visual appearance: colors, fonts, mode line, ...

;; Copyright © 2013–2026 Alex Kost <alezost@gmail.com>

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

(in-package :al/stumpwm)


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
        "khaki3"))
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


;;; mode-line strings

(defun ml/separate (&rest strings)
  "Concatenate mode-line STRINGS using separator."
  (apply #'concat " | " strings))

(defun ml/string (string &key fg bg (bright nil bright-set) reset click)
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
                    (apply #'format-with-on-click-id
                           str
                           (if (listp click) click (list click)))
                    str)))
      (concat "^[" str "^]"))))

(defun ml/zone-string (number &key (colors '("^B^1*" "^B^5*" "^B"))
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
                (ml/string (concat color num-str))
                num-str)
            ending)))

(defun ml/title-string (string)
  "Make STRING a title string."
  (ml/string string :fg "#a0aa98"))

(defun ml/format-hours (time)
  "Return 'HH:MM' string from TIME.
TIME is a floating number of hours."
  (if (numberp time)
      (multiple-value-bind (hours rem)
          (truncate time)
        (format nil "~D:~2,'0D" hours (floor (* rem 60))))
      ""))


;;; mode-line cpu

(defvar *ml/cpu-string-types* '(short long)
  "Available types for `ml/cpu-string'.")

(defvar *ml/cpu-string-type* (car *ml/cpu-string-types*)
  "Current mode line string type.
Must be one of `*ml/cpu-string-types*'.")

(defun ml/cpu-string-next-type ()
  "Set `*ml/cpu-string-type*' to the next available type."
  (setf *ml/cpu-string-type*
        (next-list-element *ml/cpu-string-types*
                           *ml/cpu-string-type*
                           #'eq)))

(defun ml/cpu-string ()
  "Return a string with CPU info suitable for the mode-line."
  (multiple-value-bind (cpu user system io irq)
      (cpu-usage)
    (flet ((d (float)
             (round (* 100 float))))
      (let* ((cpu (d cpu))
             (cpu-str (ml/zone-string cpu)))
        (ml/string
         (case *ml/cpu-string-type*
           (short cpu-str)
           (long
            (let ((user   (d user))
                  (system (d system))
                  (io     (d io))
                  (irq    (d irq)))
              (concat cpu-str
                      (ml/string "(" :reset t)
                      (format nil "~2,'0D ~2,'0D ~D ~D"
                              user system io irq)
                      (ml/string ")" :reset t))))
           (t ""))
         :fg "7")))))

(defun-with-delay 5 ml/cpu ()
  (ml/separate
   (ml/title-string "CPU")
   (format-with-on-click-id (ml/cpu-string)
                            :ml/toggle-cpu)))

(defun ml/toggle-cpu (&rest _)
  (declare (ignore _))
  (ml/cpu-string-next-type)
  (setf *ml/cpu-update* t)
  (update-all-mode-lines))

(register-ml-on-click-id :ml/toggle-cpu #'ml/toggle-cpu)


;;; mode-line memory

(defvar *ml/memory-string-types* '(short long)
  "Available types for `memory-string'.")

(defvar *ml/memory-string-type* (car *ml/memory-string-types*)
  "Current mode line string type.
Must be one of `*ml/memory-string-types*'.")

(defun ml/memory-string-next-type ()
  "Set `*ml/memory-string-type*' to the next available type."
  (setf *ml/memory-string-type*
        (next-list-element *ml/memory-string-types*
                           *ml/memory-string-type*
                           #'eq)))

(defun ml/memory-format-float (num)
  "Return formatted string from the floating number NUM."
  (cond
    ((> 10 num)
     (format nil "~3,1F" num))
    (t
     (format nil "~3D" (round num)))))

(defun ml/memory-format-kilo-bytes (kb &rest args)
  "Return formatted string from the number of kilo-bytes KB.
The rest ARGS are passed to `ml/string'."
  (if (numberp kb)
    (let ((gb (/ kb 1e6)))
      (if (> gb 1)
        (concat (apply #'ml/string (ml/memory-format-float gb) args)
                "G")
        (concat (apply #'ml/string (ml/memory-format-float (/ kb 1e3)) args)
                "M")))
    ""))

(defun ml/memory-string ()
  "Return a string with memory info suitable for the mode-line.
TYPE can be one of the following symbols: `short', `long', `next'."
  (multiple-value-bind (total available)
      (memory-info)
    (let* ((used (- total available))
           (used% (round (* 100 (/ used total))))
           (used-str
             (concat (ml/memory-format-kilo-bytes used :fg "#e08880")
                     (ml/string "(" :reset t)
                     (ml/zone-string used%
                                     :zones '(90 70 50)
                                     :format "~2,'0D")
                     (ml/string ")" :reset t))))
      (ml/string
       (case *ml/memory-string-type*
         (short (concat " " used-str))
         (long
          (let (;;(stump-total (sb-ext:dynamic-space-size))
                (stump-used (sb-kernel:dynamic-usage)))
            (concat (ml/memory-format-kilo-bytes (/ stump-used 1e3))
                    " " used-str " "
                    (ml/memory-format-kilo-bytes available :fg "#78c078"))))
         (t ""))
       :fg "7"))))

(defun-with-delay 8 ml/memory ()
  (ml/separate
   (format-with-on-click-id (ml/title-string "Mem")
                            :ml/show-memory)
   (format-with-on-click-id (ml/memory-string)
                            :ml/toggle-memory)))

(defun ml/show-memory (&rest _)
  (declare (ignore _))
  (al/mode-line-message
   (with-output-to-string (*standard-output*)
     (room))
   :center))

(defun ml/toggle-memory (&rest _)
  (declare (ignore _))
  (ml/memory-string-next-type)
  (setf *ml/memory-update* t)
  (update-all-mode-lines))

(register-ml-on-click-id :ml/show-memory #'ml/show-memory)
(register-ml-on-click-id :ml/toggle-memory #'ml/toggle-memory)


;;; mode-line thermal

(defvar *ml/thermal-zones*
  (thermal-zones))

(defvar *ml/current-thermal-zones*
  (and *ml/thermal-zones*
       (list (car *ml/thermal-zones*))))

(defun ml/thermal-string (&rest zones)
  "Return a string with thermal ZONES info suitable for the mode-line."
  (if (null zones)
    ""
    (flet ((temp (zone)
             (ml/zone-string (thermal-zone-temperature zone)
                             :zones '(70 60 50)
                             :format "~2D"
                             :ending "")))
      (ml/string
       (if (cdr zones)
         (concat "°C: " (mapconcat #'temp zones " "))
         (concat (temp (car zones)) "°C"))
       :fg "7"))))

(defun-with-delay 6 ml/thermal-zones ()
  (ml/separate
   (format-with-on-click-id
    (apply #'ml/thermal-string
           *ml/current-thermal-zones*)
    :ml/toggle-thermal-zones)))

(defun ml/toggle-thermal-zones (&rest _)
  (declare (ignore _))
  (when *ml/thermal-zones*
    (setf *ml/current-thermal-zones*
          (if (cdr *ml/current-thermal-zones*)
              (list (car *ml/thermal-zones*))
              *ml/thermal-zones*)
          *ml/thermal-zones-update* t)
    (update-all-mode-lines)))

(register-ml-on-click-id :ml/toggle-thermal-zones
                         #'ml/toggle-thermal-zones)

(defun ml/thermal-zones-maybe ()
  (if *ml/current-thermal-zones*
      (ml/thermal-zones)
      ""))


;;; mode-line net

(defvar *ml/net-devices*
  (delete "lo" (net-devices) :test #'equal)
  "List of available network devices (interfaces).")

(defvar *ml/net-device*
  ;; At first, search for "wlp*" (wlan), then for "enp*" (eth).
  (or (find-if (lambda (name) (ppcre:scan "^w" name))
               *ml/net-devices*)
      (find-if (lambda (name) (ppcre:scan "^e" name))
               *ml/net-devices*))
  "Currently used network device.")

(defun ml/net-next-device ()
  "Set `*ml/net-device*' to the next net device."
  (setf *ml/net-device*
        (next-list-element *ml/net-devices* *ml/net-device*)))

(defun ml/net-format-float (num)
  "Return formatted string from the floating number NUM."
  (cond
    ((>= 10 num)
     (format nil "~4,2F" num))
    ((>= 100 num)
     (format nil "~4,1F" num))
    (t
     (format nil "~4D" (round num)))))

(defun ml/net-format-bytes (bytes)
  "Return formatted string from the number of BYTES."
  (if (numberp bytes)
    (let ((mb (/ bytes 1e6)))
      (if (> mb 1)
        (concat (ml/net-format-float mb)
                (ml/string "M" :fg "2"))
        (concat (ml/net-format-float (/ bytes 1e3))
                (ml/string "k" :reset t))))
    ""))

(defun ml/net-string ()
  "Return a string with NET info suitable for the mode-line."
  (multiple-value-bind (state down up)
      (net-state *ml/net-device*)
    (let ((color (ecase state
                   (:up      "^b^6*")
                   (:down    "^B^5*")
                   (:soft    "^b^7*")
                   (:hard    "")
                   (:unknown "^B^1*"))))
      (concat
       (ml/string (concat color *ml/net-device*))
       (and down up
            (ml/string (concat " " (ml/net-format-bytes down)
                               " " (ml/net-format-bytes up))
                       :fg "7"))))))

(defun-with-delay 6 ml/net ()
  (ml/separate
   (format-with-on-click-id (ml/net-string)
                            :ml/toggle-net-device)))

(defun ml/toggle-net-device (&rest _)
  (declare (ignore _))
  (ml/net-next-device)
  (setf *ml/net-update* t)
  (update-all-mode-lines))

(register-ml-on-click-id :ml/toggle-net-device
                         #'ml/toggle-net-device)


;;; mode-line battery

(defvar *ml/battery* (car (batteries)))

(defun ml/battery-string (battery)
  "Return a string with BATTERY info suitable for the mode-line."
  (multiple-value-bind (state percent time)
      (battery-state battery)
    (ml/string
     (ecase state
       (:unknown "(no info)")
       (:charged (concat (write-to-string percent) "%%"))
       ((:charging :discharging)
        (concat (ml/zone-string percent :format "~2D" :reverse t)
                (if (eq state :charging)
                    (ml/string "+" :fg "2" :bright t)
                    (ml/string "-" :fg "1" :bright t))
                (ml/format-hours time))))
     :fg "7")))

(defun-with-delay 60 ml/battery ()
  (ml/separate
   (ml/title-string "Bat ")
   (ml/battery-string *ml/battery*)))

(defun ml/battery-maybe ()
  (if *ml/battery*
      (ml/battery)
      ""))


;;; mode-line backlight

(defun ml/backlight-string (bl)
  (when bl
    (ml/separate
     (ml/title-string "BL ")
     (ml/string (princ-to-string bl)
                :fg "#66bbff"
                :click :ml/update-backlight))))

(defun-with-delay nil ml/backlight ()
  (ml/backlight-string (al/backlight)))

(defcommand al/backlight* (&rest args) (:rest)
  "Call `al/backlight' and update backlight value in the mode-line."
  (apply #'al/backlight args)
  (setf *ml/backlight-update* t)
  (update-all-mode-lines))

(defun ml/update-backlight (&rest _)
  (declare (ignore _))
  (al/backlight* "get"))

(register-ml-on-click-id :ml/update-backlight #'ml/update-backlight)


;;; mode-line sound

(defun ml/sound-string (vol &optional (vol-on t))
  (when vol
    (ml/separate
     (ml/title-string "Snd ")
     (ml/string (princ-to-string vol)
                :fg (if vol-on "#50e050" "#fa3333")
                :click :ml/update-sound))))

(defun-with-delay nil ml/sound ()
  (apply #'ml/sound-string (al/sound)))

(defcommand al/sound* (&rest args) (:rest)
  "Call `al/sound' and update sound value in the mode-line."
  (apply #'al/sound args)
  (setf *ml/sound-update* t)
  (update-all-mode-lines))

(defun ml/update-sound (&rest _)
  (declare (ignore _))
  (al/sound* "get"))

(register-ml-on-click-id :ml/update-sound #'ml/update-sound)


;;; mode-line keyboard

(defun ml/locks ()
  (let ((mods (xlib:device-state-locked-mods
               (xlib:get-state *display*))))
    (flet ((color (lock)
             (if (al/mod-lock-on-p lock mods)
                 "^B^2" "")))
      (ml/separate
       (ml/string (concat (color +caps-lock+) "Caps")
                     :click :ml/toggle-caps-lock)
       " "
       (ml/string (concat (color +num-lock+) "Num")
                     :click :ml/toggle-num-lock)))))

(defun ml/toggle-caps-lock (&rest _)
  (declare (ignore _))
  (al/toggle-mod-lock +caps-lock+)
  (update-all-mode-lines))

(defun ml/toggle-num-lock (&rest _)
  (declare (ignore _))
  (al/toggle-mod-lock +num-lock+)
  (update-all-mode-lines))

(register-ml-on-click-id :ml/toggle-caps-lock #'ml/toggle-caps-lock)
(register-ml-on-click-id :ml/toggle-num-lock #'ml/toggle-num-lock)

(defun ml/layout ()
  (ml/separate
   (ml/string (al/layout-string (al/current-layout))
              :fg "7"
              :click :ml/toggle-layout)))

(defun ml/toggle-layout (&rest _)
  (declare (ignore _))
  (al/set-display-layout (mod (1+ (al/current-layout))
                              (length al/layout-alist)))
  (update-all-mode-lines))

(register-ml-on-click-id :ml/toggle-layout #'ml/toggle-layout)


;;; mode-line windows

(defvar al/window-alist nil
  "Alist of (CLASS . NUM) pairs of the available windows.
CLASS is a window class; NUM is the number of windows of this class.")

(defvar al/current-window nil
  "Class of the current window.")

(defvar al/previous-window nil
  "Class of the previous window.")

(defun al/update-window-alist (&rest _)
  "Refresh the value of `al/window-alist'."
  (declare (ignore _))
  (setf al/window-alist nil)
  (mapc (lambda (w)
          (if-letn ((wc (window-class w))
                    (entry (assoc wc al/window-alist :test #'string=)))
            (setf (cdr entry)
                  (1+ (or (cdr entry) 1)))
            (push (list wc) al/window-alist)))
        (screen-windows (current-screen))))

(defun al/update-current-window (current previous)
  "Refresh the value of `al/current-window' and `al/previous-window'."
  (flet ((class (win)
           (and win (window-class win))))
    (setf al/current-window  (class (or current (current-window)))
          al/previous-window (class previous))))

(add-hook *focus-window-hook*   'al/update-current-window)
(add-hook *new-window-hook*     'al/update-window-alist)
(add-hook *destroy-window-hook* 'al/update-window-alist)

(defvar ml/class-type-alist
  '((current  "#d8d844" "#3838a0")
    (previous "#b0b0b0" "#505078")
    (nil      "#a0a0a0" "#555555")))

(defun ml/window-class (&optional (string "%c") (type 'current))
  "Window class color construct for mode-line and window list."
  (if-let ((assoc (assoc type ml/class-type-alist)))
    (ml/string (concat " " string " ")
               :fg (second assoc)
               :bg (third assoc))
    string))

(defun ml/windows ()
  (when al/window-alist
    (flet ((window= (class1 class2)
             (and class1 class2
                  (string= class1 class2))))
      (ml/separate
       (mapconcat
        (lambda (assoc)
          (destructuring-bind (class . num)
              assoc
            (concat (format-with-on-click-id
                     (ml/window-class
                      (or class "")
                      (cond ((window= class al/current-window)
                             'current)
                            ((window= class al/previous-window)
                             'previous)))
                     :ml/toggle-window class)
                    (and num
                         (ml/string (concat " " (write-to-string num) " ")
                                    :fg "7" :bg "#407777")))))
        al/window-alist
        " ")))))

(defun ml/toggle-window (_code class &rest _rest)
  (declare (ignore _code _rest))
  (al/focus-class-window class 'next))

(register-ml-on-click-id :ml/toggle-window #'ml/toggle-window)


;;; mode-line time

(defun ml/show-time (&rest _)
  (declare (ignore _))
  (al/mode-line-message
   (time-format *time-format-string-default*)))

(register-ml-on-click-id :ml/show-time #'ml/show-time)


;;; mode-line group

(defun ml/next-group (&rest _)
  (declare (ignore _))
  (gnext))

(register-ml-on-click-id :ml/next-group #'ml/next-group)


;;; Visual appearance and mode-line settings

(setf
 *window-format* (concat "%m%n%s" (ml/window-class) " %70t")
 *window-info-format* (concat "^>" (ml/window-class)
                              "^b^6* %w^7*x^6*%h"
                              '(#\newline) "^7*%t")
 *time-format-string-default*
 (format nil "^5*%k:%M:%S~%^2*%A~%^7*%d %B~%^7*%d.%m.%Y")

 *time-modeline-string* "%k:%M"
 *mode-line-timeout* 5
 *screen-mode-line-format*
 `(,(ml/string "%d" :fg "5" :click :ml/show-time)
   " "
   ,(ml/string "%n" :fg "2" :click :ml/next-group)
   (:eval (ml/cpu))
   (:eval (ml/memory))
   (:eval (ml/thermal-zones-maybe))
   (:eval (ml/net))
   (:eval (ml/battery-maybe))
   (:eval (ml/windows))
   "^>"
   (:eval (ml/sound))
   (:eval (ml/backlight))
   (:eval (ml/layout))
   (:eval (ml/locks))))

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
