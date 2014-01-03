;;; init.lisp --- vital settings and loading other files

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 28 Jun 2013

;;; Commentary:

;; This file should be symlinked by "~/.stumpwmrc".
;; I compile stumpwm with swank, so i don't need to load it.

;;; Code:

(in-package :stumpwm)

(swank:create-server :dont-close t)


;;; Loading additional rc files

(defvar *utl-load-directory*
  (directory-namestring
   (truename (merge-pathnames (user-homedir-pathname)
                              ".stumpwmrc")))
  "A directory with initially loaded files.")

(defun utl-load (filename)
  "Load a file FILENAME (without extension) from `*utl-load-directory*'."
  (let ((file (merge-pathnames (concat filename ".lisp")
                               *utl-load-directory*)))
    (if (probe-file file)
        (load file)
        (format *error-output* "File '~a' doesn't exist." file))))

(utl-load "keys")
(utl-load "utils")
(utl-load "settings")
(utl-load "visual")
(utl-load "layouts")
(utl-load "mana")

;;; init.lisp ends here
