;;; mode-line.lisp --- Utils for mode-line

;; Copyright © 2019–2025 Alex Kost <alezost@gmail.com>

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

;;; Code:

(in-package :stumpwm)

;; These symbols are used in "mode-line-*.lisp" files.
(export '(al/ml-string
          al/ml-zone-string))

(defvar al/ml-separator " | ")

(defun al/ml-separate (&rest strings)
  "Concatenate `al/ml-separator' and STRINGS."
  (apply #'concat al/ml-separator strings))

(defun al/ml-string (string &key fg bg (bright nil bright-set) reset click)
  ;; For details on the color and mode-line machinery, see
  ;; (info "(stumpwm) Colors") and (info "(stumpwm) Mode-line Interaction").
  "Make STRING a mode-line string with FG and BG colors.

FG and BG can be nil or a string containing either a single digit (a
number from `*colors*' list) or #XXXXXX value.

If BRIGHT is set and is non-nil, use bright color.

If RESET is non-nil, use \"^n\" construct and ignore other arguments.

CLICK should have ID or (ID [ARGS ...]) form.  It is used to make STRING
clickable."
  (flet ((color (raw)
           (if (= 1 (length raw))
               raw
               (concat "\"" raw "\""))))
    (let* ((str (if reset
                    (concat "^n" string)
                    (let ((fc (and (stringp fg)
                                   (concat "^(:fg " (color fg) ")")))
                          (bc (and (stringp bg)
                                   (concat "^(:bg " (color bg) ")"))))
                      (concat (and bright-set (if bright "^B" "^b"))
                              fc bc string))))
           (str (if click
                    (apply #'format-with-on-click-id
                           str
                           (if (listp click) click (list click)))
                    str)))
      (concat "^[" str "^]"))))

(defun al/ml-zone-string (number &key (colors '("^B^1*" "^B^5*" "^B"))
                                   (zones '(90 60 30)) reverse
                                   (format "~3D") (ending "%%"))
  ;; This is a replacement for `bar-zone-color'.
  "Format NUMBER with FORMAT string, make it colored, and add ENDING to it.

ZONES is a descending list of integers that is compared to NUMBER.
COLORS is a list of respecting color specifications.
ZONES and COLORS lists must have the same length.

If REVERSE is non-nil, reverse the order of comparing ZONES and NUMBER."
  (let ((compare (if reverse #'<= #'>=))
        (num-str (format nil format number))
        (color nil))
    (do ((zones (if reverse (reverse zones) zones)
                (cdr zones))
         (colors colors (cdr colors)))
        ((or color (null zones))
         color)
      (when (funcall compare number (car zones))
        (setf color (car colors))))
    (concat (if color
                (al/ml-string (concat color num-str))
                num-str)
            ending)))

(defun al/ml-title-string (string)
  "Make STRING a title string."
  (al/ml-string string :fg "#a0aa98"))

;;; mode-line.lisp ends here
