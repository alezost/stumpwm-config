;;; utils.lisp --- Additional variables, functions and commands

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 28 Jun 2013

;;; Code:


;;; Floating windows

(in-package :stumpwm.floating-group)

(defun al/float-window-focus-forward
    (window-list &optional (window (group-current-window
                                    (current-group))))
  "Focus the next window in WINDOW-LIST from the window WINDOW."
  (let* ((wins (cdr (member window window-list)))
         (nw (if wins
                 (car wins)
                 ;; If the last window in the list is focused, then
                 ;; focus the first one.
                 (car window-list))))
    (and nw (focus-window nw))))

(defcommand (al/float-window-other float-group) () ()
  "Focus previously focused floating window."
  (focus-window (cadr (group-windows (current-group)))))

(defcommand (al/float-window-next float-group) () ()
  "Focus next floating window."
  (al/float-window-focus-forward
   (stumpwm::sort-windows (current-group))))

(defcommand (al/float-window-previous float-group) () ()
  "Focus previous floating window."
  (al/float-window-focus-forward
   (nreverse (stumpwm::sort-windows (current-group)))))

(defcommand (al/move-float-window float-group)
    (x y) ((:number "+ X: ") (:number "+ Y: "))
  "Move current floating window by X and Y pixels."
  (float-window-move-resize
   (current-window)
   :x (+ (window-x (current-window)) x)
   :y (+ (window-y (current-window)) y)))

(defcommand (al/resize-float-window float-group)
    (width height) ((:number "+ Width: ") (:number "+ Height: "))
  "Resize current floating window by WIDTH and HEIGHT pixels."
  (float-window-move-resize
   (current-window)
   :width (+ (window-width (current-window)) width)
   :height (+ (window-height (current-window)) height)))

(defcommand (al/float-window-gravity float-group)
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
    (float-window-move-resize
     (current-window)
     :x (car coords) :y (cdr coords))))


;;; Windows, frames and groups

(in-package :stumpwm)

(defun al/class-window-p (class &optional (win (current-window)))
  "Return T if a window WIN is of class CLASS."
  (and win (string= class (window-class win))))

(defcommand al/focus-window-by-class (class) ((:string "Window class: "))
  "Focus window class CLASS.
Return the window or nil if there is no such."
  (if (al/class-window-p class)
      (current-window)
      (let ((win (car (or ;; priority to the window from the current group
                       (find-matching-windows (list :class class) nil nil)
                       (find-matching-windows (list :class class) t t)))))
        (if win
            (focus-all win)
            (message "No ~a window." class))
        win)))

(defcommand al/gmove-to-other-group () ()
  "Move the current window to the other group and go to that group."
  (let ((group (car (remove-if (lambda (g) (eq g (current-group)))
                               (screen-groups (current-screen))))))
    (if group
        (progn (gmove group)
               (switch-to-group group))
        (echo "There is only one group."))))

;;; Showing and toggling the root window

(defvar *al/window-configuration* nil
  "Last saved window configuration.")

(defcommand al/show-root () ()
  "Show root window."
  (when (cdr (group-frames (current-group)))
    ;; Make one frame if necessary.
    (only))
  (fclear))

(defcommand al/toggle-root () ()
  "Toggle between root window and last window configuration."
  (if (current-window)
      (progn
        (setf *al/window-configuration* (dump-group (current-group)))
        (al/show-root))
      ;; Current window is root.
      (if *al/window-configuration*
        (restore-group (current-group) *al/window-configuration*)
        (echo "There is no saved window configuration yet."))))


;;; Sending keys to windows

(defcommand al/send-key (key &optional (win (current-window))) (:key)
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

(defun al/send-keys (keys &key (win (current-window))
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
             (al/send-key (kbd key) win)
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


;;; Interacting with shepherd user services

;; The following makes sense only for my shepherd user services, which
;; can be started in different X instances/displays/VTs:
;; <https://github.com/alezost/shepherd-config>

(defun al/herd-command (service &optional (action "restart")
                                   (display (getenv "DISPLAY")))
  "Return 'herd ACTION SERVICE:DISPLAY' command.
DISPLAY is a display number (can be a number or string optionally
beginning with ':') where a service is started."
  (format nil "herd ~a ~a:~a"
          action service
          (if (numberp display)
              display
              (string-left-trim ":" display))))


;;; Interacting with emacs

(defun al/emacs-window-p (&optional (win (current-window)))
  "Return T if WIN is emacs window."
  (al/class-window-p "Emacs" win))

(defcommand al/send-key-to-emacs (key) ((:key "Key: "))
  "Focus emacs window and send KEY to it."
  (let ((win (al/focus-window-by-class "Emacs")))
    (and win (al/send-key key win))))

(defcommand al/emacs () ()
  "Start emacs unless it is already running, in which case focus it."
  (run-or-raise (al/herd-command "emacs")
                '(:class "Emacs")))

(defcommand al/emacs-eval (arg &optional server-name) ((:shell "emacs-eval: "))
  "Evaluate ARG with emacsclient."
  (let ((args (list "--eval" arg)))
    (when server-name
      (setq args (append (list "--socket-name" server-name) args)))
    (run-prog "emacsclient" :args args :wait nil :search t)))

(defcommand al/emacs-eval-show (arg) ((:shell "emacs-eval: "))
  "Evaluate ARG with emacsclient and raise emacs."
  (al/emacs-eval arg)
  (or (al/emacs-window-p) (al/emacs)))

(defcommand al/emms-eval (arg &optional (server-name "server-emms"))
    ((:shell "emms-eval: "))
  "Evaluate ARG with emacsclient."
  (al/emacs-eval arg server-name))

(defcommand al/emms-eval-show (arg) ((:shell "emms-eval: "))
  "Evaluate ARG with emacsclient and raise emacs."
  (al/emms-eval arg)
  (or (al/emacs-window-p) (al/emacs)))


;;; Interacting with conkeror

(defcommand al/conkeror () ()
  "Start conkeror unless it is already running, in which case focus it."
  (run-or-raise (al/herd-command "conkeror")
                '(:class "Conkeror")))

(defcommand al/conkeror-browse (url) ((:shell "Browse URL: "))
  "Browse URL with conkeror."
  (run-prog "conkeror" :args (list url) :wait nil :search t))

(defcommand al/conkeror-browse-show (url) ((:shell "Browse URL: "))
  "Browse URL with conkeror and raise conkeror."
  (al/conkeror-browse url)
  (al/conkeror))

(defcommand al/conkeror-eval (arg) ((:shell "conkeror-eval: "))
  "Evaluate ARG with 'conkeror -f'."
  (run-prog "conkeror" :args (list "-f" arg) :wait nil :search t))

(defcommand al/conkeror-eval-show (arg) ((:shell "conkeror-eval: "))
  "Evaluate ARG with 'conkeror -f' and raise conkeror."
  (al/conkeror-eval arg)
  (al/conkeror))


;;; Interacting with other progs

(defcommand al/xterm () ()
  "Start xterm unless it is already running, in which case focus it."
  (run-or-raise (al/herd-command "xterm")
                '(:class "XTerm")))

(defcommand al/firefox () ()
  "Start firefox unless it is already running, in which case focus it."
  (run-or-raise (al/herd-command "firefox")
                '(:class "Firefox")))


;;; Mode line

(defun al/mode-line-pos (pos)
  "Put the mode line at a position POS (can be :TOP or :BOTTOM)."
  (let ((screen (current-screen))
        (head (current-head)))
    (enable-mode-line screen head nil)
    (setf *mode-line-position* pos)
    (enable-mode-line screen head t)))

(defcommand al/mode-line-on () ()
  "Turn the mode line on unconditionally."
  (enable-mode-line (current-screen) (current-head) t))

(defcommand al/mode-line-bottom () ()
  "Put the mode line on the bottom of the screen."
  (al/mode-line-pos :bottom))

(defcommand al/mode-line-top () ()
  "Put the mode line on the top of the screen."
  (al/mode-line-pos :top))


;;; Misc

(defun al/random-float (bot top &optional (state *random-state*))
  "Return a random float between BOT and TOP bounds."
  (+ bot (random (- top bot) state)))

(defun al/get-random-obj (objs)
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

(defcommand al/yank-primary () ()
  "Insert X primary selection into the current window."
  (window-send-string (get-x-selection)))

(defvar *al/ignore-emacs* nil
  "If non-nil, do not treat emacs specially in `al/next'.")

(defcommand al/next (&optional key) (:key)
  "Select next frame or window or emacs window.
If current window is emacs and `*al/ignore-emacs*' is nil, send key
sequence KEY to it.
If current group is tiling, select next frame.
If current group is floating, select next window."
  (if (and key
           (al/emacs-window-p)
           (null *al/ignore-emacs*)
           ;; Ignore emacs anyway, if it has a single window.
           ;; The following code checks WINDOWS_NUM window property.
           ;; You can "teach" emacs to update this property by adding
           ;; this to your .emacs:
           ;;   (add-hook 'window-configuration-change-hook
           ;;             (lambda () (when (display-graphic-p)
           ;;                          (x-change-window-property
           ;;                           "WINDOWS_NUM"
           ;;                           (string (length (window-list)))
           ;;                           nil nil nil t))))
           (let ((windows-num (car (window-property (current-window)
                                                    :WINDOWS_NUM))))
             (or (null windows-num)
                 (/= 1 windows-num))))
      (al/send-key-to-emacs key)
      (if (eq (type-of (current-group)) 'tile-group)
          (fnext)
          (stumpwm.floating-group:al/float-window-next))))

(defcommand al/toggle-ignore-emacs () ()
  "Toggle `*al/ignore-emacs*'."
  (setf *al/ignore-emacs* (not *al/ignore-emacs*))
  (message "^b^7*Switching between emacs windows ~a^b^7*."
            (if *al/ignore-emacs* "^B^1*disabled" "^2*enabled")))

;;; utils.lisp ends here
