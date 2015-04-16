;;; visual.lisp --- visual appearance: colors, fonts, mode line, resizing,...

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 28 Jun 2013

;;; Code:

(in-package :stumpwm)


;;; Colors

;; yellow and magenta are swapped to show keys in yellow
(setf *colors*
      '("black" "red" "green" "magenta" "blue" "yellow" "cyan" "white"))
(update-color-map (current-screen))

(defmacro utl-set-color (val color)
  "Similar to `set-any-color', but without updating colors."
  `(dolist (s *screen-list*)
     (setf (,val s) (alloc-color s ,color))))

(utl-set-color screen-fg-color (hex-to-xlib-color "#e5e8ef"))
(utl-set-color screen-bg-color "gray15")
(utl-set-color screen-focus-color "DeepSkyBlue")
(utl-set-color screen-border-color "ForestGreen")
(utl-set-color screen-float-focus-color "DeepSkyBlue")
(utl-set-color screen-float-unfocus-color "gray15")
(update-colors-all-screens)


;;; Grabbed pointer

(setq
 *grab-pointer-character* 40
 *grab-pointer-character-mask* 41
 *grab-pointer-foreground* (hex-to-xlib-color "#3db270")
 *grab-pointer-background* (hex-to-xlib-color "#2c53ca"))


;;; Visual appearance and the mode-line

(set-normal-gravity :bottom)
(setf *message-window-gravity* :bottom-right)
(setf *input-window-gravity*   :center)
(setf *window-info-format* (format nil "^>^B^5*%c ^b^6*%w^7*x^6*%h^7*~%%t"))
(setf *time-format-string-default* (format nil "^5*%H:%M:%S~%^2*%A~%^7*%d %B"))
(setf *mouse-focus-policy* :click)

(setf
 ;; *mode-line-position* :bottom
 *mode-line-timeout* 3
 *screen-mode-line-format* '("^5*" (:eval (time-format "%k:%M")) " ^2*%n ")
 ;; *screen-mode-line-format* '((:eval (format nil " ^2*foo~%bar")))  ; multiline mode-line
 )
(utl-mode-line-on)

;; (set-font "-*-dejavu sans mono-medium-r-normal-*-*-115-*-*-*-*-*-1")
(set-font "9x15bold")


;;; Message after a part of key sequence

;; idea from <https://github.com/sabetts/stumpwm/wiki/FAQ>
(defun key-seq-msg (key key-seq cmd)
  "Show a message with current incomplete key sequence."
  (declare (ignore key))
  (or (eq *top-map* *resize-map*)
      (stringp cmd)
      (let ((*message-window-gravity* :bottom-left))
        (message "~A" (print-key-seq (reverse key-seq))))))

(add-hook *key-press-hook* 'key-seq-msg)

;;; visual.lisp ends here
