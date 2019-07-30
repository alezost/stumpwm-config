;; This file should be loaded only if "ttf-fonts" module (from
;; "stumpwm-contrib" modules) is loaded.

;; This font setting is extracted from my main configuration to avoid a
;; potential error: if "ttf-fonts" module is not loaded, stumpwm fails
;; to read the following lisp expression, as `xft:font' class is used
;; there.

(set-font
 (make-instance 'xft:font
                :family "Liberation Mono"
                :subfamily "Bold"
                :size 12))
