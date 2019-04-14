;;; settings.lisp --- General settings: variables, hooks, ...

;; Copyright © 2013–2019 Alex Kost <alezost@gmail.com>

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


;;; Windows, frames and groups

;; Name the default group.
(setf (group-name (car (screen-groups (current-screen))))
      "tile1")
(gnewbg "tile2")
(gnewbg-float "float")

(defvar al/frames1 nil)

(defun al/make-frames1 ()
  "Return a frame layout (list of frames) for `al/frames1'."
  (let* ((screen    (current-screen))
         (s-width   (screen-width screen))
         (s-height  (screen-height screen))
         (f0-width  (/ s-width 2))
         (f0-height (* 3 (/ f0-width 4)))
         (f0 (make-frame
              :number 0
              :x 0 :y 0
              :width f0-width
              :height f0-height))
         (f1 (make-frame
              :number 1
              :x 0 :y f0-height
              :width f0-width
              :height (- s-height f0-height)))
         (f2 (make-frame
              :number 2
              :x f0-width :y 0
              :width (- s-width f0-width)
              :height s-height)))
    (list f0 f2 f1)))

(defcommand al/frames1 (&optional (populatep t)) ()
  "Show layout of 3 frames with one frame having 4/3 ratio."
  (al/set-frames (or al/frames1
                     (setf al/frames1 (al/make-frames1)))
                 populatep))


;;; Keyboard layouts

;; This is needed because stumpwm opens display before extension
;; definition.
(xlib::initialize-extensions *display*)
(xlib:enable-xkeyboard *display*)

(set-display-layout 0)
(enable-per-window-layout)


;;; Misc

(setf *top-level-error-action* :message)

;; Original `send-fake-key' sends only "press" event.
(setf (symbol-function 'send-fake-key)
      (lambda (win key) (al/send-key key win)))

(setf *deny-raise-request*
      '((:class "Conkeror")
        (:class "Firefox")
        (:class "Icecat")
        (:class "FLTK")         ; xcas
        (:class "libreoffice-writer")))

(al/banish-pointer)

;; (setf *debug-level* 10)  ; show all debug info

;;; settings.lisp ends here
