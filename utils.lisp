;;; utils.lisp --- additional variables, functions and commands

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 28 Jun 2013

;;; Code:

(in-package :stumpwm)


;;; Windows, frames and groups

(defun utl-class-window-p (class &optional (win (current-window)))
  "Return T if a window WIN is of class CLASS."
  (and win (string= class (window-class win))))

(defcommand utl-focus-window-by-class (class) ((:string "Window class: "))
  "Focus window class CLASS.
Return the window or nil if there is no such."
  (if (utl-class-window-p class)
      (current-window)
      (let ((win (car (or ;; priority to the window from the current group
                       (find-matching-windows (list :class class) nil nil)
                       (find-matching-windows (list :class class) t t)))))
        (if win
            (focus-all win)
            (message "No ~a window." class))
        win)))

(defvar *utl-ignore-emacs* nil
  "If non-nil, do not treat emacs specially in `utl-next'.")

(defcommand utl-next (&optional key) (:key)
  "Select next frame or window or emacs window.
If current window is emacs and `*utl-ignore-emacs*' is nil, send key
sequence KEY to it.
If current group is tiling, select next frame.
If current group is floating, select next window."
  (if (and key
           (utl-emacs-window-p)
           (null *utl-ignore-emacs*)
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
      (utl-send-key-to-emacs key)
      (if (eq (type-of (current-group)) 'tile-group)
          (fnext)
          (utl-float-window-next))))

(defcommand utl-toggle-ignore-emacs () ()
  "Toggle `*utl-ignore-emacs*'."
  (setf *utl-ignore-emacs* (not *utl-ignore-emacs*))
  (message "^b^7*Switching between emacs windows ~a^b^7*."
            (if *utl-ignore-emacs* "^B^1*disabled" "^2*enabled")))

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

(defcommand utl-send-key (key &optional (win (current-window))) (:key)
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


;;; Interacting with systemd user services

;; The following makes sense only for my systemd user units, which can be
;; started in different X instances/displays/VTs:
;; <https://github.com/alezost/systemd-user-units>

(defun utl-get-systemctl-user-cmd
    (service &optional (cmd "start") (display (getenv "DISPLAY")))
  "Return 'systemctl --user' command for SERVICE.
SERVICE is a service name (string).
CMD is a systemctl command.
DISPLAY is a display number (can be a number or string optionally
beginning with ':') where a service is started."
  (format nil "systemctl --user ~a ~a@~a"
          cmd service
          (if (numberp display)
              display
              (string-left-trim ":" display))))


;;; Interacting with emacs

(defun utl-emacs-window-p (&optional (win (current-window)))
  "Return T if WIN is emacs window."
  (utl-class-window-p "Emacs"))

(defcommand utl-send-key-to-emacs (key) ((:key "Key: "))
  "Focus emacs window and send KEY to it."
  (let ((win (utl-focus-window-by-class "Emacs")))
    (and win (utl-send-key key win))))

(defcommand utl-emacs () ()
  "Start emacs unless it is already running, in which case focus it."
  (run-or-raise (utl-get-systemctl-user-cmd "emacs")
                '(:class "Emacs")))

(defcommand utl-emacs-trunk () ()
  "Start emacs-trunk unless it is already running."
  (run-shell-command (utl-get-systemctl-user-cmd "emacs-trunk")))

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
  (run-or-raise (utl-get-systemctl-user-cmd "conkeror")
                '(:class "Conkeror")))

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
  (run-or-raise (utl-get-systemctl-user-cmd "xterm")
                '(:class "XTerm")))

(defcommand utl-firefox () ()
  "Start firefox unless it is already running, in which case focus it."
  (run-or-raise (utl-get-systemctl-user-cmd "firefox")
                '(:class "Firefox")))

(defcommand utl-gcolor2 () ()
  "Start gcolor2 unless it is already running, in which case focus it."
  (run-or-raise "gcolor2" '(:class "Gcolor2")))

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
    (enable-mode-line screen head t)))

(defcommand utl-mode-line-on () ()
  "Turn the mode line on unconditionally."
  (enable-mode-line (current-screen) (current-head) t))

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
