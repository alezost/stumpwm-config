;;; utils.lisp --- Additional variables, functions and commands

;; Copyright © 2013–2022 Alex Kost <alezost@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(in-package :stumpwm)

;; These symbols are used in "mode-line-*.lisp" files.
(export
 '(al/read-sys-file
   al/file-readable?))

(defun al/executable-exists? (name)
  "Return t, if NAME executable exists in PATH."
  (zerop
   (nth-value 2
              (uiop:run-program (concat "command -v " name)
                                :force-shell t
                                :ignore-error-status t))))

(defun al/read-sys-file (file-name &optional to-number)
  "Return a line (string) from FILE-NAME sysfs file.
If TO-NUMBER is non-nil, convert this string into a number.
Return nil in case of any error."
  (let ((file-name (probe-file file-name)))
    (and file-name
         (let ((param (with-open-file (file file-name)
                        (read-line-from-sysfs file))))
           (if to-number
               (parse-integer param :junk-allowed t)
               param)))))

(defun al/file-readable? (file)
  "Return t, if FILE is available for reading."
  (handler-case
      (with-open-file (f file)
        (and (read-line f) t))
    (stream-error () nil)))


;;; Floating windows

(defun al/float-window-focus-forward
    (window-list &optional (window (group-current-window
                                    (current-group))))
  "Focus the next window in WINDOW-LIST after WINDOW."
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
   (sort-windows (current-group))))

(defcommand (al/float-window-previous float-group) () ()
  "Focus previous floating window."
  (al/float-window-focus-forward
   (nreverse (sort-windows (current-group)))))

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

(defun al/raise-window (props &optional (all-groups *run-or-raise-all-groups*)
                                        (all-screens *run-or-raise-all-screens*))
  "Switch to a window matching PROPS.
This is similar to `run-or-raise' except it shows a message instead of
running a shell command if there is no suitable window."
  (let* ((matches (find-matching-windows props all-groups all-screens))
         (other-matches (member (current-window) matches))
         (win (if (> (length other-matches) 1)
                  (second other-matches)
                  (first matches))))
    (if win
        (focus-all win)
        (message "No window matching ~a" props))))

(defcommand al/gmove-to-other-group () ()
  "Move the current window to the other group and go to that group."
  (let ((group (car (remove-if (lambda (g) (eq g (current-group)))
                               (screen-groups (current-screen))))))
    (if group
        (progn (gmove group)
               (switch-to-group group))
        (echo "There is only one group."))))

(defcommand (al/fother tile-group) () ()
  "Jump to the previously selected frame.
This is a substitution for `fother': the problem with `fother' is that
it does nothing if the last frame does not exist anymore.  This command
simply moves the focus to the next existing frame."
  (let* ((group      (current-group))
         (frames     (group-frames group))
         (last-frame (tile-group-last-frame group)))
    (if (and last-frame
             (find last-frame frames))
        (focus-frame group last-frame)
        (focus-frame-after group frames))))

(defun al/set-frames (frames &optional (populatep t))
  "Display FRAMES in the current group.
The first frame will become the current one and will contain the current
window.  If POPULATEP is nil, do not populate the rest frames with
windows."
  (let* ((screen     (current-screen))
         (group      (screen-current-group screen))
         (head       (current-head group))
         (cur-window (group-current-window group))
         (cur-frame  (first frames)))
    (mapc (lambda (w)
            (setf (window-frame w) cur-frame))
          (group-windows group))
    (mapc (lambda (f)
            (setf (frame-window f) nil))
          (rest frames))
    (setf (frame-window cur-frame) cur-window
          (tile-group-frame-head group head) frames)
    (when populatep
      (populate-frames group))
    (focus-frame group cur-frame)
    (update-decoration cur-window)
    (sync-frame-windows group cur-frame)))

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


;;; Interacting with Shepherd user services

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

(defun al/shepherd-service-started-p
    (service &optional (display (getenv "DISPLAY")))
  "Return non-nil, if Shepherd SERVICE is running."
  (let ((output (run-shell-command
                 (al/herd-command service "status" display)
                 t)))
    (search "started" output)))

(defcommand al/toggle-shepherd-service
    (service &optional (display (getenv "DISPLAY")))
    ((:string "toggle Shepherd service: "))
  "Start/stop Shepherd SERVICE on DISPLAY."
  (let* ((startedp (al/shepherd-service-started-p service display)))
    (run-shell-command (al/herd-command service
                                        (if startedp "stop" "start")
                                        display))
    (message (concat "^5*~a~a^7* has been "
                     (if startedp
                         "^B^1*stopped"
                         "^2*started")
                     "^b^7*.")
             service display)))


;;; Interacting with emacs

(defun al/emacs-window-p (&optional (window (current-window)))
  "Return non-nil, if WINDOW is Emacs window in the current frame."
  (and (al/class-window-p "Emacs" window)
       (or (not (eq (type-of (current-group)) 'tile-group))
           (eq (frame-window (tile-group-current-frame (current-group)))
               window))))

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


;;; Interacting with browser

(defvar *al/browsers*
  '(("firefox" . "firefox")
    ("icecat" . "IceCat"))
  "Alist of browsers.
Each assoc should have a form of `*al/current-browser*'.")

(defvar *al/current-browser* nil
  "Browser used by `al/browser' command.
The value should be a cons of program name and window class of this
program.")

(defun al/current-browser ()
  "Return the currently used browser."
  (or *al/current-browser*
      (setf *al/current-browser*
            (or (find-if (lambda (assoc)
                           (al/executable-exists? (car assoc)))
                         *al/browsers*)
                (progn
                  (echo "No working browsers found among `*al/browsers*'")
                  (car *al/browsers*))))))

(defcommand al/browser (&optional args) (:rest)
  "Start browser unless it is already running, in which case focus it."
  (let ((browser (al/current-browser)))
    (if args
        (progn
          (run-shell-command (concat (car browser) " " args))
          (al/browser))
        (run-or-raise (car browser) `(:class ,(cdr browser))))))

(defcommand al/browse (url) ((:shell "Browse URL: "))
  "Browse URL with `*al/current-browser*'."
  (run-prog (car (al/current-browser))
            :args (list url) :wait nil :search t))

(defcommand al/browse-show (url) ((:shell "Browse URL: "))
  "Browse URL with `*al/current-browser*' and raise it."
  (al/browse url)
  (al/browser))


;;; Interacting with other progs

(defcommand al/xterm () ()
  "Start xterm unless it is already running, in which case focus it."
  (run-or-raise (al/herd-command "xterm")
                '(:class "XTerm")))

(defcommand al/mpv () ()
  "Switch to the next mpv window."
  (al/raise-window '(:class "mpv")))

(defcommand al/toggle-unclutter () ()
  "Start/stop 'unclutter' on the current display."
  (al/toggle-shepherd-service "unclutter"))


;;; Backlight brightness

(defvar al/backlight nil
  "Backlight brightness of the current display.
This variable should have (STRING . TIME) value, where STRING is the
latest value of the backlight, and TIME is the time (seconds since epoch)
of the latest update.")

(defun al/update-backlight ()
  "Update `al/backlight'."
  (let* ((output (run-shell-command "xbacklight -get" t))
         (strings (split-string output '(#\newline)))
         (backlights (mapcar (lambda (str)
                               (and (not (string= "" str)) str))
                             strings))
         (backlight (first (delete nil backlights)))
         ;; Convert "22.000000" to "22"
         (backlight (and (stringp backlight)
                         (first (split-string backlight ".")))))
    (setf al/backlight
          (cons backlight
                (if backlight
                    (get-universal-time)
                    ;; If backlight is not available, do not update it
                    ;; for a day.
                    (+ (get-universal-time) (* 24 3600)))))))

(defcommand al/set-backlight (&rest args) (:rest)
  "Set backlight brightness of the current display.
Pass ARGS as arguments to 'xbacklight' shell command."
  ;; XXX For some reason, the following line works if 'al/set-backlight'
  ;; is evaluated as a function but does not work if it is called as a
  ;; command.
  ;;(run-prog "osd-backlight" :args args :wait t :search t)
  (run-shell-command (format nil "osd-backlight ~{~A~^ ~}" args))
  (setf al/backlight (cons nil (get-universal-time))))


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

(defun al/next-list-element (list element)
  ;; XXX Is there a ready-to-use function for this?
  "Return an element from LIST which follows ELEMENT.
If ELEMENT is the last element of the LIST, return the first one.
Return nil, if ELEMENT is not in the LIST."
  (let ((rest list))
    (loop
       until (equal (car rest) element)
       do (setq rest (cdr rest))
       if (null rest) return nil
       finally (return (or (cadr rest) (car list))))))

(defcommand al/banish-pointer () ()
  "Move mouse pointer to the top/center of the current screen."
  (let* ((screen (current-screen))
         (width  (screen-width screen)))
    (xlib:warp-pointer (screen-root screen) (/ width 2) 0)))

(defcommand al/yank-primary () ()
  "Insert X primary selection into the current window."
  (window-send-string (get-x-selection)))

(defvar *al/ignore-emacs* nil
  "If non-nil, do not treat Emacs specially by `al/switch-frame-or-window'.")

(defun al/switch-frame-or-window (switch-tile switch-float &optional key)
  "Select frame or window or emacs window.
If current window is emacs and `*al/ignore-emacs*' is nil, send key
sequence KEY to it.
If current group is tiling, call SWITCH-TILE procedure.
If current group is floating, call SWITCH-FLOAT procedure."
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
          (funcall switch-tile)
          (funcall switch-float))))

(defcommand al/other (&optional key) (:key)
  "Select previously selected frame or window or emacs window.
If current window is emacs and `*al/ignore-emacs*' is nil, send key
sequence KEY to it.
If current group is tiling, select previously selected frame.
If current group is floating, select previously selected window."
  (al/switch-frame-or-window #'al/fother #'al/float-window-other key))

(defcommand al/next (&optional key) (:key)
  "Select next frame or window or emacs window.
If current window is emacs and `*al/ignore-emacs*' is nil, send key
sequence KEY to it.
If current group is tiling, select next frame.
If current group is floating, select next window."
  (al/switch-frame-or-window #'fnext #'al/float-window-next key))

(defcommand al/toggle-ignore-emacs () ()
  "Toggle `*al/ignore-emacs*'."
  (setf *al/ignore-emacs* (not *al/ignore-emacs*))
  (message "^b^7*Switching between emacs windows ~a^b^7*."
            (if *al/ignore-emacs* "^B^1*disabled" "^2*enabled")))

(defmacro al/defun-with-delay (seconds name args &rest body)
  "Define NAME function with ARGS and BODY.
It is like a usual `defun', except when the function is called, it is
evaluated only if the number of SECONDS has already been passed since
the last call.  If this time has not been passed yet, the previous value
of the function is returned without evaluation.

For example, the following `delayed-time' function will return a new
time string only every 10 seconds:

  (al/defun-with-delay
   10 delayed-time ()
   (time-format \"%H:%M:%S\"))
"
  (let ((next-time-var  (make-symbol "next-time"))
        (last-value-var (make-symbol "last-value")))
    `(let ((,next-time-var 0)
           ,last-value-var)
       (defun ,name ,args
         (let ((now (get-universal-time)))
           (if (< now ,next-time-var)
               ,last-value-var
               (setf ,next-time-var (+ now ,seconds)
                     ,last-value-var (progn ,@body))))))))

;;; utils.lisp ends here
