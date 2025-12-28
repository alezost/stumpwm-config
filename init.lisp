;;; init.lisp --- Vital settings and loading other files

;; Copyright © 2013–2025 Alex Kost <alezost@gmail.com>

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

;; This file should be symlinked by "~/.stumpwmrc".  In the past, this
;; file loaded the rest config files directly, using the following
;; function:
;;
;; (defun al/load (filename)
;;   "Load a file FILENAME (without extension) from `al/init-directory'."
;;   (let ((file (merge-pathnames (concat filename ".lisp")
;;                                al/init-directory)))
;;     (if (probe-file file)
;;         (load file)
;;         (format *error-output* "File '~a' doesn't exist." file))))
;;
;; Nowadays, I use `asdf:load-system' to load "al-stumpwm-config.asd"
;; which specifies the rest config files.

;; My config depends on `swank' and `xkeyboard' CL packages (installed
;; with Quicklisp).  I compile stumpwm image with these packages.
;; To do it, I add the following lines:
;;
;;   (require 'swank)
;;   (require 'xkeyboard)
;;
;; to "load-stumpwm.lisp.in" file (then, "./configure" and "make" will
;; do it).

;;; Code:

(in-package :stumpwm)

(defvar al/init-directory
  (directory-namestring
   (truename (merge-pathnames (user-homedir-pathname)
                              ".stumpwmrc")))
  "Directory with stumpwm config files.")

(redirect-all-output (merge-pathnames "log" al/init-directory))

;; Loading the rest config.
(push al/init-directory asdf:*central-registry*)
(asdf:load-system "al-stumpwm-config")

;; This is needed to have SSH prompt (via GnuPG) in the current X session.
;; See "man gpg-agent" for details.
(run-shell-command "gpg-connect-agent updatestartuptty /bye")

;;; init.lisp ends here
