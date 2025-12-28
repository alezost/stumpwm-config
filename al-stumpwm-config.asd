(defsystem "al-stumpwm-config"
  :author "Alex Kost"
  :description "StumpWM configuration"
  :license "GPL3"
  :version "0.1"
  :depends-on ("stumpwm" "swank" "xkeyboard")
  :components
  ((:static-file "COPYING")
   (:file "keys")
   (:file "swank")
   (:file "utils")
   (:file "xkb")
   (:file "sound")
   (:file "settings")
   (:file "ml")
   (:file "ml-cpu")
   (:file "ml-memory")
   (:file "ml-thermal")
   (:file "ml-net")
   (:file "ml-battery")
   (:file "visual"))
  :serial t)
