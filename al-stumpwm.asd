(defsystem :al-stumpwm
  :author "Alex Kost"
  :description "StumpWM configuration"
  :license "GPL3"
  :version "0.1"
  :depends-on ("stumpwm" "al" "xkeyboard")
  :components
  ((:static-file "COPYING")
   (:file "al-stumpwm")
   (:file "keys")
   (:file "utils")
   (:file "xkb")
   (:file "settings")
   (:file "ml")
   (:file "visual"))
  :serial t)
