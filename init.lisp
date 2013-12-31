;;; init.lisp --- vital settings and loading other files

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 28 Jun 2013

;;; Commentary:

;; This file should be symlinked by "~/.stumpwmrc".

;;; Code:

(in-package :stumpwm)

(swank:create-server :dont-close t)

;; tip to check what happened:
;; (screen-last-msg (current-screen))


;;; Loading additional rc files

(defvar *al-load-directory*
  (directory-namestring
   (truename (merge-pathnames (user-homedir-pathname)
                              ".stumpwmrc")))
  "A directory with initially loaded files.")

(defun al-load (filename)
  "Load a file FILENAME (without extension) from `*al-load-directory*'."
  (let ((file (merge-pathnames (concat filename ".lisp")
                               *al-load-directory*)))
    (if (probe-file file)
        (load file)
        (format *error-output* "File '~a' doesn't exist." file))))

(al-load "keys")
(al-load "utils")
(al-load "settings")
(al-load "visual")
(al-load "layouts")
(al-load "mana")

;;; init.lisp ends here
