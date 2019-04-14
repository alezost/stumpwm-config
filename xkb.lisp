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


;;; Keyboard layouts

(defun current-layout (&optional (display *display*))
  "Return current keyboard layout."
  (xlib:device-state-locked-group (xlib:get-state display)))

(defun window-layout (window)
  "Return keyboard layout of a specified WINDOW."
  (getf (xlib:window-plist (window-xwin window))
        :keyboard-layout))

(defun set-display-layout (group &optional (display *display*))
  "Set keyboard layout to a specified GROUP."
  (xlib:lock-group display :group group)
  (xlib:display-finish-output display))

(defun update-window-layout (window previous-window)
  "Update keyboard layout when switching from PREVIOUS-WINDOW to WINDOW."
  (let ((current-layout (current-layout)))
    (when previous-window
      (setf (getf (xlib:window-plist (window-xwin previous-window))
                  :keyboard-layout)
            current-layout)
      (when window
        (let ((window-layout (window-layout window)))
          (when (and window-layout
                     (not (equal current-layout window-layout)))
            (set-display-layout window-layout)))))))

(defun update-group-layout (group previous-group)
  "Update keyboard layout when switching from PREVIOUS-GROUP to GROUP."
  (update-window-layout (group-current-window group)
                        (group-current-window previous-group)))

(defcommand enable-per-window-layout () ()
  "Enable changing keyboard layouts per window."
  (add-hook *focus-window-hook* 'update-window-layout)
  (add-hook *focus-group-hook*  'update-group-layout))

(defcommand disable-per-window-layout () ()
  "Disable changing keyboard layouts per window."
  (remove-hook *focus-window-hook* 'update-window-layout)
  (remove-hook *focus-group-hook*  'update-group-layout))

(defcommand set-layout (group &optional key)
    ((:number "Layout number: ") :key)
  "Set keyboard layout to a specified xkb GROUP.
If current window is emacs, send a key sequence KEY to it (if specified)."
  (when (and key (al/emacs-window-p))
    (setq group 0)
    (al/send-key key))
  (set-display-layout group))


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
