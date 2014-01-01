;;; utils.lisp --- additional variables, functions and commands

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 28 Jun 2013

;;; Code:

(in-package :stumpwm)


;;; Windows, frames and groups

(defcommand al-gmove-to-other-group () ()
  "Move the current window to the other group and go to that group."
  (let ((group (car (remove-if (lambda (g) (eq g (current-group)))
                         (screen-groups (current-screen))))))
    (if group
        (progn (gmove group)
               (switch-to-group group))
        (echo "There is only one group."))))

;;; Showing and toggling the root window

(defvar *al-window-configuration* nil
  "Last saved window configuration.")

(defcommand al-show-root () ()
  "Show root window."
  ;; make one frame if necessary
  ;; (let ((*executing-stumpwm-command* t)) ; suppress message
  ;;   (only))
  (if (cdr (group-frames (current-group)))
       (only))
  (fclear))

(defcommand al-toggle-root () ()
  "Toggle between root window and last window configuration."
  (if (current-window)
      (progn
        (setf *al-window-configuration* (dump-group (current-group)))
        (al-show-root))
      ;; current window is root
      (if *al-window-configuration*
        (restore-group (current-group) *al-window-configuration*)
        (echo "There is no saved window configuration yet."))))

;;; Moving floating windows

(defcommand move-window-right (val) (:number)
  "Move current floating window right by VAL."
  (float-window-move-resize (current-window)
                            :x (+ (window-x (current-window)) val)))

(defcommand move-window-down (val) (:number)
  "Move current floating window down by VAL."
  (float-window-move-resize (current-window)
                            :y (+ (window-y (current-window)) val)))

(defcommand (float-window-gravity float-group)
    (gravity) ((:gravity "Gravity: "))
  "Move current floating window to a particular place of the screen.
GRAVITY controls where the window will appear.  Possible values are:
:center, :top, :right, :bottom, :left, :top, :top-left, :bottom-right,
:bottom-left."
  (let* ((screen-width  (screen-width (current-screen)))
         (screen-height (screen-height (current-screen)))
         (window-width  (+ (window-width (current-window))
                           (* 2 *float-window-border*)))
         (window-height (+ (window-height (current-window))
                           *float-window-border*
                           *float-window-title-height*))
         (x-right  (- screen-width window-width))
         (x-center (round (/ x-right 2)))
         (y-bottom (- screen-height window-height))
         (y-center (round (/ y-bottom 2)))
         (coords (ccase gravity
                   (:center       (cons x-center y-center))
                   (:top-left     (cons 0 0))
                   (:top          (cons nil 0))
                   (:top-right    (cons x-right 0))
                   (:right        (cons x-right nil))
                   (:bottom-right (cons x-right y-bottom))
                   (:bottom       (cons nil y-bottom))
                   (:bottom-left  (cons 0 y-bottom))
                   (:left         (cons 0 nil)))))
    (float-window-move-resize (current-window)
                              :x (car coords) :y (cdr coords))))


;;; Sending keys to windows

(defun al-send-key (key &optional (win (current-window)))
  "Send a fake key press and key release events to win."
  (let ((xwin (window-xwin win)))
    (multiple-value-bind (code state) (key-to-keycode+state key)
      ;; TODO duplicating code is not good
      (xlib:send-event xwin :key-press (xlib:make-event-mask :key-press)
                       :display *display*
                       :root (screen-root (window-screen win))
                       :x 0 :y 0 :root-x 0 :root-y 0
                       :window xwin :event-window xwin
                       :code code
                       :state state)
      (xlib:send-event xwin :key-release (xlib:make-event-mask :key-release)
                       :display *display*
                       :root (screen-root (window-screen win))
                       :x 0 :y 0 :root-x 0 :root-y 0
                       :window xwin :event-window xwin
                       :code code
                       :state state)
      (xlib:display-finish-output *display*))))

(defun al-send-keys (keys &key (win (current-window))
                            (sleep 0) loop loop-quit-var)
  "Send keys to the window WIN.

KEYS is a string for `kbd', a list of such strings or functions or a
function returning a string or a list.

SLEEP is a time between sending keys or a function for defining
this time.

If LOOP is t, send keys in a loop (the whole combination of strings,
lists and functions in KEYS will be repeated).  It will be broken when
a variable which name is passed to LOOP-QUIT-VAR returns t.  Be aware,
infinite loop is not a joke."
  (labels ((send-key (key)
             (al-send-key (kbd key) win)
             ;; (print key)
             (sleep (if (numberp sleep)
                        sleep
                        (funcall sleep))))
           (send-keys (key-def &optional loop)
             (loop
                do (cond
                     ((stringp key-def)
                      (send-key key-def))
                     ((listp key-def)
                      (dolist (key key-def) (send-keys key)))
                     ((functionp key-def)
                      (send-keys (funcall key-def)))
                     (t (error "Keys should be a string, a list or a function")))
                while (and loop
                           (null (and loop-quit-var (eval loop-quit-var)))))))
    (send-keys keys loop)
    (echo "Quitting sending keys.")))


;;; Interacting with emacs

(defun emacs-window-p (&optional (win (current-window)))
  "Return T if WIN is emacs window."
  (and win (string= "Emacs" (window-class win))))

(defcommand send-key-to-emacs (key) (:key)
  "Raise emacs window and send a key to it."
  (let ((win (if (window-matches-properties-p (current-window) :class "Emacs")
                 (current-window)
                 (car (find-matching-windows '(:class "Emacs") 'all-groups 'all-screens)))))
    (if win
        (progn
          (frame-raise-window (window-group win) (window-frame win) win)
          (al-send-key key win))
        (echo "No emacs window."))))

(defvar *al-fnext-emacs-p* t
  "If non-nil, always send a key to emacs with `al-fnext'.")

(defcommand al-fnext (key) (:key)
  "Similar to `fnext', but send a KEY to emacs if it's the current
window in a single frame or if `*al-fnext-emacs-p*' is non-nil."
  (if (and (emacs-window-p)
           (or *al-fnext-emacs-p*
               (null (cdr (group-frames (current-group))))))
       (send-key-to-emacs key)
       (fnext)))

(defcommand al-fnext-emacs-toggle () ()
  "Toggle `*al-fnext-emacs-p*'."
  (setf *al-fnext-emacs-p* (not *al-fnext-emacs-p*))
  (if *al-fnext-emacs-p*
      (echo "Switching between emacs windows.")
      (echo "Switching between frames.")))

(defcommand emacs () ()
  "Start emacs unless it is already running, in which case focus it."
  (run-or-raise "systemctl --user start emacs@0" '(:class "Emacs")))

(defcommand emacs-eval (arg) ((:shell "emacs-eval "))
  "Evaluate ARG with emacsclient."
  (run-prog "emacsclient" :args (list "--eval" arg) :wait nil :search t))

(defcommand emacs-eval-show (arg) ((:shell "emacs-eval "))
  "Evaluate ARG with emacsclient and raise emacs."
  (emacs-eval arg)
  (or (emacs-window-p) (emacs)))


;;; Interacting with conkeror

(defcommand conkeror () ()
  "Start conkeror unless it is already running, in which case focus it."
  (run-or-raise "systemctl --user start conkeror@0" '(:class "Conkeror")))

(defcommand conkeror-browse (url) ((:shell "Browse URL: "))
  "Browse URL with conkeror."
  (run-prog "conkeror" :args (list url) :wait nil :search t))

(defcommand conkeror-browse-show (url) ((:shell "Browse URL: "))
  "Browse URL with conkeror and raise conkeror."
  (conkeror-browse url)
  (conkeror))

(defcommand conkeror-eval (arg) ((:shell "conkeror-eval "))
  "Evaluate ARG with 'conkeror -f'."
  (run-prog "conkeror" :args (list "-f" arg) :wait nil :search t))

(defcommand conkeror-eval-show (arg) ((:shell "conkeror-eval "))
  "Evaluate ARG with 'conkeror -f' and raise conkeror."
  (conkeror-eval arg)
  (conkeror))


;;; Interacting with other progs

(defcommand xterm () ()
  "Start xterm unless it is already running, in which case focus it."
  (run-or-raise "systemctl --user start xterm@0" '(:class "XTerm")))

(defcommand firefox () ()
  "Start firefox unless it is already running, in which case focus it."
  (run-or-raise "systemctl --user start firefox@0" '(:class "Firefox")))

(defcommand gtypist (&optional file) (:string)
  "Start gtypist loading a .typ-file, if it is specified."
  (run-shell-command (concat "xterm -e 'gtypist --color=0,7 " file "'")))


;;; Mode line

(defun al-mode-line-pos (pos)
  "Put the mode line at a position POS (can be :TOP or :BOTTOM)."
  (let ((screen (current-screen))
        (head (current-head)))
    (enable-mode-line screen head nil)
    (setf *mode-line-position* pos)
    (enable-mode-line screen head t
                      '*screen-mode-line-format*)))

(defcommand al-mode-line-on () ()
  "Turn the mode line on unconditionally."
  (enable-mode-line (current-screen) (current-head) t
                    '*screen-mode-line-format*))

(defcommand al-mode-line-bottom () ()
  "Put the mode line on the bottom of the screen."
  (al-mode-line-pos :bottom))

(defcommand al-mode-line-top () ()
  "Put the mode line on the top of the screen."
  (al-mode-line-pos :top))


;;; Controlling sound

(load (merge-pathnames "progs/lisp/setaudio.lisp"
                       (user-homedir-pathname)))
(defcommand setaudio (&optional &rest args) (:rest)
  "Set (or show without args) audio."
  (audio-set args))
(defcommand next-scontrol () ()
  "Switch simple control."
  (audio-set nil (audio-set-next-scontrol)))


;;; Misc

(defun al-random-float (bot top &optional (state *random-state*))
  "Return a random float between BOT and TOP bounds."
  (+ bot (random (- top bot) state)))

(defun al-get-random-obj (objs)
  "Return a random object from alist OBJS.

Each association is a pair of object and its probability (from 0 to
1).  If the total probability is lower than 1, there is a chance to
get nil."
  (let ((rnd (random 1.0))
        (prob 0))
    (loop
       for elm in objs
       do (setq prob (+ prob (cdr elm)))
       if (< rnd prob)
       return (car elm))))

(defcommand al-yank-primary () ()
  "Insert X primary selection into the current window."
  (window-send-string (get-x-selection)))

;;; utils.lisp ends here
