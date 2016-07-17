;;; settings.lisp --- General settings: variables, hooks, ...

;; Copyright © 2013-2016 Alex Kost <alezost@gmail.com>

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


;;; Groups

;; Name the default group.
(setf (group-name (car (screen-groups (current-screen))))
      "tile1")
(gnewbg "tile2")
(gnewbg-float "float")


;;; Layouts

(layout-set 0)
(layout-enable-per-window)


;;; Misc

(setf *top-level-error-action* :message)

;; Original `send-fake-key' sends only "press" event.
(setf (symbol-function 'send-fake-key)
      (lambda (win key) (al/send-key key win)))

(push '(:class "Conkeror") *deny-raise-request*)
(push '(:class "libreoffice-writer") *deny-raise-request*)

;; (setf *debug-level* 10)  ; show all debug info

;;; settings.lisp ends here
