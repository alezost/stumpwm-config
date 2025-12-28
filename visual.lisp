;;; visual.lisp --- Visual appearance: colors, fonts, mode line, ...

;; Copyright © 2013–2025 Alex Kost <alezost@gmail.com>

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


;;; mode-line cpu

(al/defun-with-delay 5 al/ml-cpu ()
  (al/ml-separate
   (al/ml-title-string "CPU")
   (format-with-on-click-id (al/ml-cpu:ml-string)
                            :al/ml-toggle-cpu)))

(defun al/ml-toggle-cpu (&rest _)
  (declare (ignore _))
  (al/ml-cpu:ml-next-type)
  (setf al/ml-cpu-update t)
  (update-all-mode-lines))

(register-ml-on-click-id :al/ml-toggle-cpu #'al/ml-toggle-cpu)


;;; mode-line memory

(al/defun-with-delay 8 al/ml-memory ()
  (al/ml-separate
   (format-with-on-click-id (al/ml-title-string "Mem")
                            :al/ml-show-memory)
   (format-with-on-click-id (al/stumpwm-memory:memory-mode-line-string)
                            :al/ml-toggle-memory)))

(defun al/ml-show-memory (&rest _)
  (declare (ignore _))
  (al/mode-line-message
   (with-output-to-string (*standard-output*)
     (room))
   :center))

(defun al/ml-toggle-memory (&rest _)
  (declare (ignore _))
  (al/stumpwm-memory:memory-mode-line-next-type)
  (setf al/ml-memory-update t)
  (update-all-mode-lines))

(register-ml-on-click-id :al/ml-show-memory #'al/ml-show-memory)
(register-ml-on-click-id :al/ml-toggle-memory #'al/ml-toggle-memory)


;;; mode-line thermal

(defvar al/all-thermal-zones
  (al/stumpwm-thermal:all-thermal-zones))

(defvar al/current-thermal-zones
  (and al/all-thermal-zones
       (list (car al/all-thermal-zones))))

(al/defun-with-delay 6 al/ml-thermal-zones ()
  (al/ml-separate
   (format-with-on-click-id
    (apply #'al/stumpwm-thermal:thermal-zones-mode-line-string
           al/current-thermal-zones)
    :al/ml-toggle-thermal-zones)))

(defun al/ml-toggle-thermal-zones (&rest _)
  (declare (ignore _))
  (when al/all-thermal-zones
    (setf al/current-thermal-zones
          (if (cdr al/current-thermal-zones)
              (list (car al/all-thermal-zones))
              al/all-thermal-zones)
          al/ml-thermal-zones-update t)
    (update-all-mode-lines)))

(register-ml-on-click-id :al/ml-toggle-thermal-zones
                         #'al/ml-toggle-thermal-zones)

(defun al/ml-thermal-zones-maybe ()
  (if al/current-thermal-zones
      (al/ml-thermal-zones)
      ""))


;;; mode-line net

(al/defun-with-delay 6 al/ml-net ()
  (al/ml-separate
   (format-with-on-click-id (al/stumpwm-net:net-mode-line-string)
                            :al/ml-toggle-net-device)))

(defun al/ml-toggle-net-device (&rest _)
  (declare (ignore _))
  (al/stumpwm-net:net-next-device)
  (setf al/ml-net-update t)
  (update-all-mode-lines))

(register-ml-on-click-id :al/ml-toggle-net-device
                         #'al/ml-toggle-net-device)


;;; mode-line battery

(defvar al/battery (car (al/ml-battery:all-batteries)))

(al/defun-with-delay 60 al/ml-battery ()
  (al/ml-separate
   (al/ml-title-string "Bat ")
   (al/ml-battery:ml-string al/battery)))

(defun al/ml-battery-maybe ()
  (if al/battery
      (al/ml-battery)
      ""))


;;; mode-line backlight

(defun al/ml-backlight-string (bl)
  (and (stringp bl)
       (not (string= "" bl))
       (al/ml-separate
        (al/ml-title-string "BL ")
        (al/ml-string bl :fg "#66bbff"))))

(defun al/ml-backlight ()
  (al/ml-backlight-string (al/backlight)))


;;; mode-line sound

(defun al/ml-sound-string (vol &optional (vol-on t))
  (and (stringp vol)
       (not (string= "" vol))
       (al/ml-separate
        (al/ml-title-string "Snd ")
        (al/ml-string vol :fg (if vol-on "#50e050" "#fa3333")
                      :click :al/ml-toggle-sound))))

(defun al/ml-sound ()
  (apply #'al/ml-sound-string (al/sound-volume)))

(defun al/ml-toggle-sound (&rest _)
  (declare (ignore _))
  (al/sound-set-current-scontrol "toggle"))

(register-ml-on-click-id :al/ml-toggle-sound #'al/ml-toggle-sound)


;;; mode-line keyboard

(defun al/ml-locks ()
  (let ((mods (xlib:device-state-locked-mods
               (xlib:get-state *display*))))
    (flet ((color (lock)
             (if (al/mod-lock-on-p lock mods)
                 "^B^2" "")))
      (al/ml-separate
       (al/ml-string (concat (color +caps-lock+) "Caps")
                     :click :al/ml-toggle-caps-lock)
       " "
       (al/ml-string (concat (color +num-lock+) "Num")
                     :click :al/ml-toggle-num-lock)))))

(defun al/ml-toggle-caps-lock (&rest _)
  (declare (ignore _))
  (al/toggle-mod-lock +caps-lock+)
  (update-all-mode-lines))

(defun al/ml-toggle-num-lock (&rest _)
  (declare (ignore _))
  (al/toggle-mod-lock +num-lock+)
  (update-all-mode-lines))

(register-ml-on-click-id :al/ml-toggle-caps-lock #'al/ml-toggle-caps-lock)
(register-ml-on-click-id :al/ml-toggle-num-lock #'al/ml-toggle-num-lock)

(defun al/ml-layout ()
  (al/ml-separate
   (al/ml-string (al/layout-string (al/current-layout))
                 :fg "7"
                 :click :al/ml-toggle-layout)))

(defun al/ml-toggle-layout (&rest _)
  (declare (ignore _))
  (al/set-display-layout (mod (1+ (al/current-layout))
                              (length al/layout-alist)))
  (update-all-mode-lines))

(register-ml-on-click-id :al/ml-toggle-layout #'al/ml-toggle-layout)


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

(defun al/ml-window-class (&optional (str " %c "))
  "Window class color construct for mode-line and window list."
  (al/ml-string str :fg "#d8d844" :bg "#3838a0"))

(defun al/ml-windows ()
  (when (and al/window-alist al/current-window)
    (al/ml-separate
     (al/mapconcat
      (lambda (assoc)
        (destructuring-bind (class . num)
            assoc
          (concat
           (if (string= class al/current-window)
               (format-with-on-click-id
                (al/ml-window-class (concat " " class " "))
                :al/ml-toggle-window class)
               (al/ml-string (concat " " class " ")
                             :fg "#a0a0a0" :bg "#555555"
                             :click (list :al/ml-toggle-window
                                          class)))
           (and num
                (al/ml-string (concat " " (write-to-string num) " ")
                              :fg "7" :bg "#407777")))))
      al/window-alist
      " "))))

(defun al/ml-toggle-window (_code class &rest _rest)
  (declare (ignore _code _rest))
  (al/focus-class-window class 'next))

(register-ml-on-click-id :al/ml-toggle-window #'al/ml-toggle-window)


;;; mode-line time

(defun al/ml-show-time (&rest _)
  (declare (ignore _))
  (al/mode-line-message
   (time-format *time-format-string-default*)))

(register-ml-on-click-id :al/ml-show-time #'al/ml-show-time)


;;; mode-line group

(al/defun-with-delay nil al/ml-group ()
  (let* ((screen (current-screen))
         (groups (sort-groups screen))
         (cur-group (screen-current-group screen))
         (next-group (al/next-list-element groups cur-group #'eq)))
    (al/ml-string (group-name cur-group)
                  :fg "2"
                  :click (list :ml-on-click-switch-to-group
                               (group-name next-group)))))

(defun al/ml-group-update (&rest _)
  (declare (ignore _))
  (setf al/ml-group-update t)
  (update-all-mode-lines))

(add-hook *focus-group-hook* 'al/ml-group-update)


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
 `(,(al/ml-string "%d" :fg "5" :click :al/ml-show-time)
   " "
   (:eval (al/ml-group))
   (:eval (al/ml-cpu))
   (:eval (al/ml-memory))
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
