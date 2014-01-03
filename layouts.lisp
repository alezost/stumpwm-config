;;; layouts.lisp --- switching keyboard layouts

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 28 Jun 2013

;;; Commentary:

;; This file uses xkeyboard extension
;; <https://github.com/filonenko-mikhail/clx-xkeyboard> (i installed it
;; with quicklisp and compiled stumpwm with it).  A big part of the
;; following code came from the stumpwm example of that extension.

;; This file provides some functions and commands for working with
;; layouts.  I use it for:
;; - different key bindings for switching to different layouts,
;; - ability to have different layouts for different windows,
;; - if current window is emacs, send a specified key sequence to it
;;   (for internal setting input method) instead of the global layout
;;   switching.

;;; Code:

(in-package :stumpwm)

;; we need it because
;; stumpwm opens display before extension definition
(xlib::initialize-extensions *display*)
(xlib:enable-xkeyboard *display*)

(defun get-current-layout (display)
  "Return current keyboard layout."
  (xlib:device-state-locked-group (xlib:get-state display)))

(defun get-window-layout (win &optional (default nil))
  "Return keyboard layout of a specified window WIN.
If a window does not have a layout property, return DEFAULT."
  (getf (xlib:window-plist (window-xwin win))
        :keyboard-layout default))

(defun window-focus-changed (window previous-window)
  (let ((current-layout (get-current-layout *display*)))
    (when previous-window
      (setf (getf (xlib:window-plist (window-xwin previous-window))
                  :keyboard-layout)
            current-layout)
      (when window
        (let ((window-layout (get-window-layout window current-layout)))
          (when (not (equal current-layout window-layout))
            (xlib:lock-group *display* :group window-layout)))))))

(defun group-focus-changed (group previous-group)
  (let ((previous-window (stumpwm::group-current-window previous-group))
        (window (stumpwm::group-current-window group)))
    (window-focus-changed window previous-window)))

(defcommand enable-per-window-layout () ()
  "Enable changing keyboard layouts per window."
  (add-hook *focus-window-hook* 'window-focus-changed)
  (add-hook *focus-group-hook*  'group-focus-changed))

(defcommand disable-per-window-layout () ()
  "Enable changing keyboard layouts per window."
  (remove-hook *focus-window-hook* 'window-focus-changed)
  (remove-hook *focus-group-hook*  'group-focus-changed))

(defvar *emacs-layout-key-alist*
  '((0 . "s-7")
    (1 . "s-8")
    (2 . "s-9"))
  "Associations list of layout numbers and key bindings for emacs.
Key bindings are strings accepted by `kbd'.")

(defun get-emacs-layout-key (num)
  "Return a key value by a layout (group) number.
Key binding is taken from `*emacs-layout-key-alist*'.
Return nil, if there is no association."
  (kbd (cdr (assoc num *emacs-layout-key-alist*))))

(defcommand set-layout (num) ((:number "layout number: "))
  "Set keyboard layout to a specified layout (xkb group) number.
If current window is emacs, send an appropriate key to it.
The key is defined by `get-emacs-layout-key'."
  (if (utl-emacs-window-p)
      (let ((key (get-emacs-layout-key num)))
        (when key
          ;; (or (eq 0 (get-current-layout *display*))
          ;;     (xlib:lock-group *display* :group 0))
          (setq num 0)
          (utl-send-key key))))
  (xlib:lock-group *display* :group num)
  (xlib:display-finish-output *display*))

(set-layout 0)
(enable-per-window-layout)

;;; layouts.lisp ends here
