;;; ttf.lisp --- TTF fonts

;; Copyright © 2019 Alex Kost <alezost@gmail.com>

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

;; This file should be loaded only if "ttf-fonts" module (from
;; "stumpwm-contrib" modules) is loaded.

;; This font setting is extracted from my main configuration to avoid a
;; potential error: if "ttf-fonts" module is not loaded, stumpwm fails
;; to read the following lisp expression, as `xft:font' class is used
;; there.

;;; Code:

(in-package :stumpwm)

(defun al/preferred-font ()
  "Return preferred font for `set-font'.
Return nil, if there are some problems with this font."
  (let ((font (handler-case
                  (make-instance 'xft:font
                                 :family "Liberation Mono"
                                 :subfamily "Bold"
                                 :size 12)
                ;; Error means that (xft:cache-fonts) was never run, or
                ;; the font is not installed.
                (error () nil))))
    (and font
         ;; If a font was moved (for example, from
         ;; </usr/share/fonts/TTF> to </usr/share/fonts/liberation>),
         ;; then the font cache should be updated.  Otherwise, setting
         ;; this font leads stumpwm to error.
         (probe-file (xft::get-font-pathname font))
         font)))

(defun al/set-preferred-font (&optional try)
  "Set preferred font.
If it fails, and TRY is nil, update fonts cache, and try to set it again.
If it fails, and TRY is non-nil, display an error message."
  (let ((font (al/preferred-font)))
    (if font
        (set-font font)
        (if try
            (echo "^[^B^1*ERROR^] during setting TTF font.
^[^6*(xft:cache-fonts)^] does not help!")
            (progn
              (echo "Updating fonts cache...")
              (xft:cache-fonts)
              (al/set-preferred-font 1))))))

(al/set-preferred-font)

;;; ttf.lisp ends here
