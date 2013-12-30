;;; init.lisp --- vital settings and loading other files

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 28 Jun 2013

;;; Code:

(in-package :stumpwm)

(swank:create-server :dont-close t)

;; tip to check what happened:
;; (screen-last-msg (current-screen))


;;; Loading additional rc files

(defvar *al-load-path*
  (merge-pathnames "progs/lisp/stumpwm/" (user-homedir-pathname))
  "A directory with initially loaded files.")

(defun al-load (filename)
  "Load a file FILENAME from `*al-load-path*'."
  (load (merge-pathnames (concat filename ".lisp")
                         *al-load-path*)))

(al-load "keys")
(al-load "utils")
(al-load "settings")
(al-load "visual")
(al-load "layouts")
(al-load "mana")

;;; init.lisp ends here
