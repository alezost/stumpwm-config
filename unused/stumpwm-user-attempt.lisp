;; 2019-04-16 I tried to move from `:stumpwm' to `:stumpwm-user' module
;; by prepending the following code to my "init.lisp" (and replacing
;; `:stumpwm' module name in other files).  Unfortunately, it didn't
;; work in one very important place (maybe in some others as well, but
;; this one is enough).  When I pressed "s-t c" (bound to `colon'
;; command), I got the following echo message:
;;
;;   Error In Command 'colon': unknown type specifier: FLOAT-GROUP
;;
;; Apparently, it is some bug in stumpwm, but I didn't bother to dig it.
;; So I will stick to :stumpwm module.  It's not a problem after all,
;; since all my procedures, macros and variables are prefixed with `al/'.



;;; Exporting missing symbols from :stumpwm

;; "missing" means they are internal for `:stumpwm' module, so they
;; cannot be accessed directly from `:stumpwm-user' module (without
;; prefixing them with `stumpwm::').  And since I use these symbols in
;; my config, I export them here.

(in-package :stumpwm)

(export
 '(run-prog

   ;; Key maps and procedures
   *tile-group-top-map*
   *tile-group-root-map*
   *float-group-top-map*
   *float-group-root-map*
   print-key-seq
   key-to-keycode+state

   ;; Groups, frames and windows
   switch-to-group
   group-frames
   tile-group-last-frame
   tile-group-frame-head
   tile-group-current-frame
   make-frame
   focus-frame
   focus-frame-after
   screen-current-group
   window-frame
   frame-window
   populate-frames
   focus-frame
   sync-frame-windows
   dump-group
   restore-group
   *window-info-format*
   window-property
   find-matching-windows
   focus-all
   sort-windows
   *float-window-border*
   *float-window-title-height*
   float-window-move-resize

   ;; Colors
   *bar-hi-color*
   screen-fg-color
   screen-bg-color
   screen-focus-color
   screen-border-color
   update-colors-all-screens
   hex-to-xlib-color))


(in-package :stumpwm-user)

