;;; mana.lisp --- interacting with Mana

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 25 Aug 2013

;;; Commentary:

;; Mana is "The Mana World" game <https://themanaworld.org/>.

;; Some commands for starting mana client and automating boring actions
;; (like bat daily quest).  No, no, i'm not a bot; i just don't like to
;; press keys while watching my character playing.

;;; Code:

(in-package :stumpwm)

(defvar *mana-client-name* "manaplus"
  "Name of a mana client program.")

(defvar *mana-thread* nil
  "A thread used in functions for sending keys to the mana window.")

(defvar *mana-quit-sending-p* nil
  "Predicate for defining quit from sending keys to the mana window.")

(defvar *mana-check-window-p* t
  "If non-nil, check whether current window is the mana one.")

(defun mana-window-p (win)
  "Return T if WIN is mana window."
  (string= "manaplus" (window-class win)))

(defun mana-send-keys (keys-list &key (sleep 0.5) loopp msg)
  "Send keys to the current mana window.

Each object of KEYS-LIST should be suitable for KEYS argument of
`al-send-keys'.
If LOOPP is t, send the last keys of KEYS-LIST in a loop.
Show a message MSG if it is specified.
For SLEEP meaning see `al-send-keys'."
  (let ((win (current-window)))
    (if (or (null *mana-check-window-p*)
            (mana-window-p win))
        (progn
          (setf *mana-quit-sending-p* nil)
          (and msg (echo msg))
          (setf *mana-thread*
                (let ((last (car (last keys-list))))
                  (sb-thread:make-thread
                   (lambda ()
                     (mapc (lambda (obj)
                             (al-send-keys obj :win win :sleep sleep
                                           :loopp (if (eq obj last) loopp)
                                           :loop-quit-var '*mana-quit-sending-p*))
                           keys-list))
                   :name "sending-keys-to-mana"))))
        (echo "Not Mana window."))))

(defcommand mana-attack () ()
  "Send attack mixed with movements keys in a loop to the mana window."
  (mana-send-keys
   (list (lambda ()
           (al-get-random-obj '(("a" . 0.7)
                                ("p" . 0.1)
                                ("." . 0.05)
                                ("e" . 0.05)
                                ("o" . 0.05)
                                ("u" . 0.05)))))
   :sleep (lambda () (al-random-float 0.3 1.5))
   :loopp t
   :msg "Mana attack begins..."))

(defcommand mana-quick-attack () ()
  "Send attack key in a loop to the mana window."
  (mana-send-keys
   '("a")
   :sleep (lambda () (al-random-float 0.2 0.8))
   :loopp t
   :msg "Mana quick attack begins..."))

(defvar *mana-bat-quest-start-keys*
  '("n" "t" "Down" "Down" "SPC" "SPC" "Down" "SPC" "SPC" "SPC" "Down" "SPC" "SPC" "SPC" "SPC" "SPC")
  "A list of strings for starting the bat quest.")

(defvar *mana-bat-quest-cont-keys*
  '("n" "t" "Down" "Down" "SPC" "SPC" "SPC" "SPC")
  "A list of strings for continuing the bat quest.")

(defcommand mana-bat-quest-start () ()
  "Send keys for starting a talk with Arkim."
  (mana-send-keys (list *mana-bat-quest-start-keys*)
                  :sleep (lambda () (al-random-float 0.7 1.2))
                  :msg "Bat quest (start) begins..."))

(defcommand mana-bat-quest-cont () ()
  "Send keys for continuing a talk with Arkim."
  (mana-send-keys (list *mana-bat-quest-cont-keys*)
                  :sleep (lambda () (al-random-float 0.7 1.2))
                  :loopp t
                  :msg "Bat quest (continuation) begins..."))

(defcommand mana-bat-quest-full () ()
  "Send keys for a full talk with Arkim."
  (mana-send-keys (list *mana-bat-quest-start-keys*
                        *mana-bat-quest-cont-keys*)
                  :sleep (lambda () (al-random-float 0.7 1.2))
                  :loopp t
                  :msg "Bat quest begins..."))

(defun mana-thread-alive-p (&optional alive-msg-p dead-msg-p)
  "Return T if mana thread is still alive.
Optinally show a message about the state of the thread, according to
ALIVE-MSG-P and DEAD-MSG-P "
  (if (and *mana-thread* (sb-thread:thread-alive-p *mana-thread*))
      (progn (and alive-msg-p (echo "^b^7*Mana thread is ^2*alive^7*."))
             t)
      (progn (and dead-msg-p (echo "^b^7*Mana thread is ^B^1*dead^b^7*."))
             nil)))

(defcommand mana-state () ()
  "Show a state of mana thread."
  (mana-thread-alive-p t t))

(defcommand mana-break () ()
  "Stop sending keys to mana window."
  (when (mana-thread-alive-p nil t)
    (setf *mana-quit-sending-p* t)))

(defcommand mana-kill () ()
  "Kill mana thread (if `mana-break' doesn't help)."
  (when (mana-thread-alive-p nil t)
    (sb-thread:terminate-thread *mana-thread*)
    (echo "^b^7*Mana thread was ^B^1*killed^b^7*.")))

;; I don't like to write password and to choose a server and a character
;; each time i start mana client, so i wrote a little emacs function
;; `mana-get-shell-string' for returning a full mana shell command
;; (password is taken from "~/.authinfo.gpg")
(defcommand mana-exec () ()
  "Start manaplus client."
  (let ((mana-cmd (make-array '(0) :element-type 'base-char
                              :fill-pointer 0 :adjustable t)))
    (with-output-to-string (out mana-cmd)
      (if (= 0 (sb-ext::process-exit-code
                (run-prog "emacsclient" :args '("-e" "(mana-get-shell-string)")
                          :output out :wait t :search t)))
          (run-shell-command (string-trim '(#\" #\NewLine) mana-cmd))
          (echo "Emacsclient has ^B^1*failed")))))

;;; mana.lisp ends here
