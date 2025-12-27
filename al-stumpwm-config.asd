(defsystem "al-stumpwm-config"
  :author "Alex Kost"
  :description "StumpWM configuration"
  :license "GPL3"
  :version "0.1"
  :depends-on ("stumpwm" "swank" "xkeyboard")
  :components
  ((:static-file "COPYING")
   (:file "keys")
   (:file "utils")
   (:file "xkb")
   (:file "sound")
   (:file "settings")
   (:file "mode-line")
   (:file "mode-line-cpu")
   (:file "mode-line-memory")
   (:file "mode-line-thermal")
   (:file "mode-line-net")
   (:file "mode-line-battery")
   (:file "visual"))
  :serial t)
