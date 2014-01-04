;;; utils.lisp --- additional variables, functions and commands

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 28 Jun 2013

;;; Code:

(in-package :stumpwm)


;;; Windows, frames and groups

(defcommand utl-gmove-to-other-group () ()
  "Move the current window to the other group and go to that group."
  (let ((group (car (remove-if (lambda (g) (eq g (current-group)))
                         (screen-groups (current-screen))))))
    (if group
        (progn (gmove group)
               (switch-to-group group))
        (echo "There is only one group."))))

;;; Showing and toggling the root window

(defvar *utl-window-configuration* nil
  "Last saved window configuration.")

(defcommand utl-show-root () ()
  "Show root window."
  ;; make one frame if necessary
  ;; (let ((*executing-stumpwm-command* t)) ; suppress message
  ;;   (only))
  (if (cdr (group-frames (current-group)))
       (only))
  (fclear))

(defcommand utl-toggle-root () ()
  "Toggle between root window and last window configuration."
  (if (current-window)
      (progn
        (setf *utl-window-configuration* (dump-group (current-group)))
        (utl-show-root))
      ;; current window is root
      (if *utl-window-configuration*
        (restore-group (current-group) *utl-window-configuration*)
        (echo "There is no saved window configuration yet."))))

;;; Focusing floating windows

(defun utl-float-window-focus-forward (window-list
                                   &optional (window (group-current-window
                                                      (current-group))))
  "Focus the next window in WINDOW-LIST from the window WINDOW."
  (let* ((wins (cdr (member window window-list)))
         (nw (if wins
                 (car wins)
                 ;; If the last window in the list is focused, then
                 ;; focus the first one.
                 (car window-list))))
    (and nw (focus-window nw))))

(defcommand (utl-float-window-other float-group) () ()
  "Focus previously focused floating window."
  (focus-window (cadr (group-windows (current-group)))))

(defcommand (utl-float-window-next float-group) () ()
  "Focus next floating window."
  (utl-float-window-focus-forward (sort-windows (current-group))))

(defcommand (utl-float-window-previous float-group) () ()
  "Focus previous floating window."
  (utl-float-window-focus-forward (nreverse (sort-windows (current-group)))))

;;; Moving floating windows

(defcommand (utl-move-float-window float-group)
    (x y) ((:number "+ X: ") (:number "+ Y: "))
  "Move current floating window by X and Y pixels."
  (float-window-move-resize
   (current-window)
   :x (+ (window-x (current-window)) x)
   :y (+ (window-y (current-window)) y)))

(defcommand (utl-resize-float-window float-group)
    (width height) ((:number "+ Width: ") (:number "+ Height: "))
  "Resize current floating window by WIDTH and HEIGHT pixels."
  (float-window-move-resize
   (current-window)
   :width (+ (window-width (current-window)) width)
   :height (+ (window-height (current-window)) height)))

(defcommand (utl-float-window-gravity float-group)
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

(defun utl-send-key (key &optional (win (current-window)))
  "Send key press and key release events for KEY to window WIN."
  (let ((xwin (window-xwin win)))
    (multiple-value-bind (code state) (key-to-keycode+state key)
      (flet ((send (event)
               (xlib:send-event xwin event (xlib:make-event-mask event)
                                :display *display*
                                :root (screen-root (window-screen win))
                                :x 0 :y 0 :root-x 0 :root-y 0
                                :window xwin :event-window xwin
                                :code code
                                :state state)))
        (send :key-press)
        (send :key-release)
        (xlib:display-finish-output *display*)))))

(defun utl-send-keys (keys &key (win (current-window))
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
             (utl-send-key (kbd key) win)
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

(defun utl-emacs-window-p (&optional (win (current-window)))
  "Return T if WIN is emacs window."
  (and win (string= "Emacs" (window-class win))))

(defcommand utl-send-key-to-emacs (key) (:key)
  "Raise emacs window and send a key to it."
  (let ((win (if (window-matches-properties-p (current-window) :class "Emacs")
                 (current-window)
                 (car (find-matching-windows '(:class "Emacs") 'all-groups 'all-screens)))))
    (if win
        (progn
          (frame-raise-window (window-group win) (window-frame win) win)
          (utl-send-key key win))
        (echo "No emacs window."))))

(defvar *utl-fnext-emacs-p* t
  "If non-nil, always send a key to emacs with `utl-fnext'.")

(defcommand utl-fnext (key) (:key)
  "Similar to `fnext', but send a KEY to emacs if it's the current
window in a single frame or if `*utl-fnext-emacs-p*' is non-nil."
  (if (and (utl-emacs-window-p)
           (or *utl-fnext-emacs-p*
               (null (cdr (group-frames (current-group))))))
       (utl-send-key-to-emacs key)
       (fnext)))

(defcommand utl-fnext-emacs-toggle () ()
  "Toggle `*utl-fnext-emacs-p*'."
  (setf *utl-fnext-emacs-p* (not *utl-fnext-emacs-p*))
  (if *utl-fnext-emacs-p*
      (echo "Switching between emacs windows.")
      (echo "Switching between frames.")))

(defcommand utl-emacs () ()
  "Start emacs unless it is already running, in which case focus it."
  (run-or-raise "systemctl --user start emacs@0" '(:class "Emacs")))

(defcommand utl-emacs-eval (arg) ((:shell "emacs-eval: "))
  "Evaluate ARG with emacsclient."
  (run-prog "emacsclient" :args (list "--eval" arg) :wait nil :search t))

(defcommand utl-emacs-eval-show (arg) ((:shell "emacs-eval: "))
  "Evaluate ARG with emacsclient and raise emacs."
  (utl-emacs-eval arg)
  (or (utl-emacs-window-p) (utl-emacs)))


;;; Interacting with conkeror

(defcommand utl-conkeror () ()
  "Start conkeror unless it is already running, in which case focus it."
  (run-or-raise "systemctl --user start conkeror@0" '(:class "Conkeror")))

(defcommand utl-conkeror-browse (url) ((:shell "Browse URL: "))
  "Browse URL with conkeror."
  (run-prog "conkeror" :args (list url) :wait nil :search t))

(defcommand utl-conkeror-browse-show (url) ((:shell "Browse URL: "))
  "Browse URL with conkeror and raise conkeror."
  (utl-conkeror-browse url)
  (utl-conkeror))

(defcommand utl-conkeror-eval (arg) ((:shell "conkeror-eval: "))
  "Evaluate ARG with 'conkeror -f'."
  (run-prog "conkeror" :args (list "-f" arg) :wait nil :search t))

(defcommand utl-conkeror-eval-show (arg) ((:shell "conkeror-eval: "))
  "Evaluate ARG with 'conkeror -f' and raise conkeror."
  (utl-conkeror-eval arg)
  (utl-conkeror))


;;; Interacting with other progs

(defcommand utl-xterm () ()
  "Start xterm unless it is already running, in which case focus it."
  (run-or-raise "systemctl --user start xterm@0" '(:class "XTerm")))

(defcommand utl-firefox () ()
  "Start firefox unless it is already running, in which case focus it."
  (run-or-raise "systemctl --user start firefox@0" '(:class "Firefox")))

(defcommand utl-gtypist (&optional file) (:string)
  "Start gtypist loading a .typ-file, if it is specified."
  (run-shell-command (concat "xterm -e 'gtypist --color=0,7 " file "'")))


;;; Mode line

(defun utl-mode-line-pos (pos)
  "Put the mode line at a position POS (can be :TOP or :BOTTOM)."
  (let ((screen (current-screen))
        (head (current-head)))
    (enable-mode-line screen head nil)
    (setf *mode-line-position* pos)
    (enable-mode-line screen head t
                      '*screen-mode-line-format*)))

(defcommand utl-mode-line-on () ()
  "Turn the mode line on unconditionally."
  (enable-mode-line (current-screen) (current-head) t
                    '*screen-mode-line-format*))

(defcommand utl-mode-line-bottom () ()
  "Put the mode line on the bottom of the screen."
  (utl-mode-line-pos :bottom))

(defcommand utl-mode-line-top () ()
  "Put the mode line on the top of the screen."
  (utl-mode-line-pos :top))


;;; Misc

(defun utl-random-float (bot top &optional (state *random-state*))
  "Return a random float between BOT and TOP bounds."
  (+ bot (random (- top bot) state)))

(defun utl-get-random-obj (objs)
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

(defcommand utl-yank-primary () ()
  "Insert X primary selection into the current window."
  (window-send-string (get-x-selection)))

;;; utils.lisp ends here
