(in-package :stumpwm)


;;; Binding keys in "sub-keymap"

(defvar *my-frame-map* (make-sparse-keymap))
(define-key *root-map* (kbd "f") '*my-frame-map*)
(define-key *my-frame-map* (kbd "w") "frame-windowlist")


;;; Creating a "permanent" map

(defvar *sample-map* (make-sparse-keymap) "sample keymap")
(define-key *sample-map* (kbd "s-n") "pull-hidden-next")
(define-key *sample-map* (kbd "s-q") "exit-sample-map")
(define-key *top-map*    (kbd "s-1") "enter-sample-map")

(defcommand enter-sample-map () ()
  "Enter to sample-map."
  (message "We are in sample-map!")
  (push-top-map *sample-map*))

(defcommand exit-sample-map () ()
  "Exit from sample-map."
  (message "We are in top-map again!")
  (pop-top-map))


