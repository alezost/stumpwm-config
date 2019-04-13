;;; xkb.lisp --- Wrapper for clx-xkeyboard library

;; Copyright © 2013–2016, 2019 Alex Kost <alezost@gmail.com>

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
;; <https://github.com/filonenko-mikhail/clx-xkeyboard> (I installed it
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

;; Also I use clx-xkeyboard to control CapsLock, NumLock (to get their
;; values for the mode line and to change these values).

;;; Code:

(in-package :stumpwm)

;; We need it because stumpwm opens display before extension definition.
(xlib::initialize-extensions *display*)
(xlib:enable-xkeyboard *display*)


;;; Keyboard layouts

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


;;; Mod locks (CapsLock, NumLock, etc.)

;; These constants were found experimentally (I didn't bother to find
;; the meaning of the higher bits).  I didn't find any mention of the
;; possible values of "ModLocks" in the XKeyboard Protocol Specification
;; <https://www.x.org/releases/current/doc/kbproto/xkbproto.html>.
;; So what is the source of these values (where are they hard-coded)?
(defconstant +shift-lock+ #b1)
(defconstant +caps-lock+  #b10)
(defconstant +ctrl-lock+  #b100)
(defconstant +alt-lock+   #b1000)
(defconstant +num-lock+   #b10000)
(defconstant +mod3-lock+  #b100000)     ; Hyper
(defconstant +mod4-lock+  #b1000000)    ; Super

(defun al/mod-lock-state (mod mods)
  "Return t if MOD lock is enabled in MODS bits.
Return nil otherwise."
  (not (zerop (logand mod mods))))

(defun al/set-mod-locks (mod-locks &optional affect-mod-locks)
  "Set key mod locks according to MOD-LOCKS bits.
If AFFECT-MOD-LOCKS is nil, use the value of MOD-LOCKS."
  (xlib:latch-lock-state
   *display*
   :mod-locks mod-locks
   :affect-mod-locks (or affect-mod-locks mod-locks)
   :lock-group nil
   :group-lock 0
   :mod-latches 0
   :affect-mod-latches 0
   :latch-group nil
   :group-latch 0)
  (xlib:display-finish-output *display*))

(defun al/toggle-mod-lock (mod-lock)
  "Toggle MOD-LOCK key."
  (if (al/mod-lock-state mod-lock
                         (xlib:device-state-locked-mods
                          (xlib:get-state *display*)))
      (al/set-mod-locks 0 mod-lock)
      (al/set-mod-locks mod-lock)))

(defcommand al/toggle-caps-lock () ()
  "Toggle CapsLock key."
  (al/toggle-mod-lock +caps-lock+))

;;; xkb.lisp ends here
