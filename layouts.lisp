;;; layouts.lisp --- Switching keyboard layouts

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

;;; Commentary:

;; This file uses xkeyboard extension
;; <https://github.com/filonenko-mikhail/clx-xkeyboard> (i installed it
;; with quicklisp and compiled stumpwm with it).  A big part of the
;; following code came from the stumpwm example of that extension.

;; This file provides some functions and commands for working with
;; layouts.  I use it for:
;;
;; - different key bindings for different layouts,
;; - different layouts for different windows,
;; - setting internal input method in emacs if it is the current window
;;   (by sending a specified key sequence to it) instead of the global
;;   layout switching.

;;; Code:

(in-package :stumpwm)

;; We need it because stumpwm opens display before extension definition.
(xlib::initialize-extensions *display*)
(xlib:enable-xkeyboard *display*)

(defun layout-get-current-layout (display)
  "Return current keyboard layout."
  (xlib:device-state-locked-group (xlib:get-state display)))

(defun layout-get-window-layout (win &optional (default nil))
  "Return keyboard layout of a specified window WIN.
If a window does not have a layout property, return DEFAULT."
  (getf (xlib:window-plist (window-xwin win))
        :keyboard-layout default))

(defun layout-window-changed (window previous-window)
  (let ((current-layout (layout-get-current-layout *display*)))
    (when previous-window
      (setf (getf (xlib:window-plist (window-xwin previous-window))
                  :keyboard-layout)
            current-layout)
      (when window
        (let ((window-layout (layout-get-window-layout window current-layout)))
          (when (not (equal current-layout window-layout))
            (xlib:lock-group *display* :group window-layout)))))))

(defun layout-group-changed (group previous-group)
  (let ((previous-window (group-current-window previous-group))
        (window (group-current-window group)))
    (layout-window-changed window previous-window)))

(defcommand layout-enable-per-window () ()
  "Enable changing keyboard layouts per window."
  (add-hook *focus-window-hook* 'layout-window-changed)
  (add-hook *focus-group-hook*  'layout-group-changed))

(defcommand layout-disable-per-window () ()
  "Disable changing keyboard layouts per window."
  (remove-hook *focus-window-hook* 'layout-window-changed)
  (remove-hook *focus-group-hook*  'layout-group-changed))

(defcommand layout-set (num &optional key)
    ((:number "Layout number: ") :key)
  "Set keyboard layout to a specified layout (xkb group) number NUM.
If current window is emacs, send a key sequence KEY to it (if specified)."
  (and (al/emacs-window-p)
       key
       (setq num 0)
       (al/send-key key))
  (xlib:lock-group *display* :group num)
  (xlib:display-finish-output *display*))

;;; layouts.lisp ends here
