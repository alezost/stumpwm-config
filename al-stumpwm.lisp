;;; al-stumpwm.lisp --- Package definition

;; Copyright © 2026 Alex Kost <alezost@gmail.com>

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

(defpackage :al/stumpwm
  (:use :cl :al :al/hwinfo :stumpwm)
  (:shadowing-import-from :al :concat))

(in-package :stumpwm)

;; These symbols (and many more) should have been exported by StumpWM.
(export
 '(top-maps
   *tile-group-top-map*
   *tile-group-root-map*
   *float-group-top-map*
   *float-group-root-map*
   *help-map*
   *input-map*
   input-abort
   input-line-string
   input-line-position
   input-submit
   input-delete-backward-char
   input-delete-forward-char
   input-backward-char
   input-forward-char
   input-backward-word
   input-forward-word
   input-backward-kill-word
   input-forward-kill-word
   input-move-beginning-of-line
   input-move-end-of-line
   input-kill-line
   input-kill-to-beginning
   input-history-back
   input-history-forward
   input-complete-backward
   input-complete-forward
   sort-windows
   float-window-move-resize
   *float-window-border*
   *float-window-title-height*
   float-group
   tile-group
   tile-group-last-frame
   tile-group-frame-head
   tile-group-current-frame
   focus-all
   window-by-id
   window-property
   hide-window
   switch-to-group
   make-frame
   focus-frame
   focus-frame-after
   populate-frames
   group-frames
   window-frame
   frame-window
   sync-frame-windows
   dump-group
   restore-group
   screen-current-group
   key-to-keycode+state
   get-kmaps-at-key-seq
   print-key-seq
   dereference-kmaps
   minor-mode-sync-keys-hook-function
   run-prog
   alloc-color
   screen-fg-color
   screen-bg-color
   screen-focus-color
   screen-unfocus-color
   screen-float-focus-color
   screen-float-unfocus-color
   screen-border-color
   update-colors-all-screens
   hex-to-xlib-color
   *grab-pointer-character*
   *grab-pointer-character-mask*
   *grab-pointer-foreground*
   *grab-pointer-background*
   update-all-mode-lines
   *window-info-format*
   time-plist
   *time-format-string-alist*))

;;; al-stumpwm.lisp ends here
