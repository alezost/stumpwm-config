;;; settings.lisp --- general settings: variables, hooks, ...

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 28 Dec 2013

;;; Code:

(in-package :stumpwm)


;;; Groups

;; name the default group
(setf (group-name (car (screen-groups (current-screen)))) "main")
(gnewbg-float "float")


;;; Misc

(setf *top-level-error-action* :message)

;; original `send-fake-key' sends only press event
(setf (symbol-function 'send-fake-key)
      (lambda (win key) (al-send-key key win)))

(push '(:class "Conkeror") *deny-raise-request*)
(push '(:class "libreoffice-writer") *deny-raise-request*)

;; (setf *debug-level* 10)  ; show all debug info

;;; settings.lisp ends here
