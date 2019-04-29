;;; visual.lisp --- Visual appearance: colors, fonts, mode line, ...

;; Copyright © 2013–2016, 2018–2019 Alex Kost <alezost@gmail.com>

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
(al/set-color screen-focus-color "DeepSkyBlue")
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

(al/load "mode-line-cpu")

(defvar al/cpu-refresh-time 3)

(al/defun-with-delay
 al/cpu-refresh-time al/mode-line-cpu ()
 (al/stumpwm-cpu:cpu-mode-line-string))


;;; mode-line thermal

(al/load "mode-line-thermal")

(defvar al/thermal-zone
  (car (al/stumpwm-thermal:all-thermal-zones)))

(defvar al/thermal-zones-refresh-time 6)

(al/defun-with-delay
 al/thermal-zones-refresh-time al/mode-line-thermal-zones ()
 (al/stumpwm-thermal:thermal-zones-mode-line-string al/thermal-zone))

(defun al/mode-line-thermal-zones-maybe ()
  (if al/thermal-zone
      (concat " | " (al/mode-line-thermal-zones))
      ""))


;;; mode-line net

(al/load "mode-line-net")

(defvar al/net-refresh-time 6)

(al/defun-with-delay
 al/net-refresh-time al/mode-line-net ()
 (al/stumpwm-net:net-mode-line-string))


;;; mode-line battery

(al/load "mode-line-battery")

(defvar al/battery (car (al/stumpwm-battery:all-batteries)))

(defvar al/battery-refresh-time 60)

(al/defun-with-delay
 al/battery-refresh-time al/mode-line-battery ()
 (al/stumpwm-battery:battery-mode-line-string al/battery))


;;; mode-line keyboard

(defun al/mode-line-locks ()
  (defun bool->color (bool)
    (if bool "^B^2" ""))
  (let ((mods (xlib:device-state-locked-mods
               (xlib:get-state *display*))))
    (format nil "^[~ACaps^] ^[~ANum^]"
            (bool->color (al/mod-lock-state +caps-lock+ mods))
            (bool->color (al/mod-lock-state +num-lock+ mods)))))

(defun al/mode-line-layout ()
  (format nil "^[^7*~A^]"
          (al/layout-string (al/current-layout))))


;;; Visual appearance and mode-line settings

(setf
 *window-info-format*
 (format nil "^>^B^5*%c ^b^6*%w^7*x^6*%h^7*~%%t")

 *time-format-string-default*
 (format nil "^5*%H:%M:%S~%^2*%A~%^7*%d %B")

 *time-modeline-string* "%k:%M"
 *mode-line-timeout* 3
 *screen-mode-line-format*
 `("^[^5*%d^]"                  ; time
   " ^[^2*%n^]"                 ; group name
   " | " (:eval (al/mode-line-cpu))
   (:eval (al/mode-line-thermal-zones-maybe))
   " | " (:eval (al/mode-line-net))
   ,(if al/battery
      '(" | " (:eval (al/mode-line-battery)))
      "")
   "^>"
   (:eval (al/mode-line-layout))
   " | " (:eval (al/mode-line-locks))))

(al/mode-line-on)

;; (set-font "-*-dejavu sans mono-medium-r-normal-*-*-115-*-*-*-*-*-1")
(set-font "9x15bold")

;;; visual.lisp ends here
