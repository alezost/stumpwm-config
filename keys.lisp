;;; keys.lisp --- key bindings (Dvorak layout)

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 28 Jun 2013

;;; Code:

(in-package :stumpwm)

(set-prefix-key (kbd "s-t"))


;;; Functional/additional keys (placed on the same keys)

(defvar utl-f-keys-alist
  '(("F1"  . "Help")
    ("F2"  . "SunUndo")
    ("F3"  . "SunAgain")
    ("F4"  . "XF86New")
    ("F5"  . "XF86Open")
    ("F6"  . "XF86Close")
    ("F7"  . "XF86Reply")
    ("F8"  . "XF86Forward")
    ("F9"  . "XF86Send")
    ("F10" . "XF86Spell")
    ("F11" . "XF86Save")
    ("F12" . "SunPrint_Screen"))
  "Alist of functional and additional keys bound to the same commands.")

(defun utl-define-key (map key command)
  "Similar to `define-key', except KEY should be a string.
If KEY is a functional key from `utl-f-keys-alist', bind COMMAND to the
additional key."
  (define-key map (kbd key) command)
  (let ((add-key (cdr (assoc key utl-f-keys-alist :test 'equal))))
    (when add-key
      (define-key map (kbd add-key) command))))

;; Bind "H-<key>" to sending <key> to the current window
;; (<key> is a key from `utl-f-keys-alist').
(dolist (key-assoc utl-f-keys-alist)
  (let ((fun-key (car key-assoc))
        (add-key (cdr key-assoc)))
    (flet ((dk (bound-key sent-key)
             (define-key *top-map*
                 (kbd (concat "H-" bound-key))
               (concat "utl-send-key " sent-key))))
      (dk fun-key fun-key)
      (dk add-key fun-key))))


;;; Managing windows, frames & groups

(utl-define-key *root-map* "F"   "fselect")
(utl-define-key *root-map* "="   "balance-frames")
(utl-define-key *root-map* "s-c" "delete-window")
(utl-define-key *root-map* "C"   "kill-window")

(utl-define-key *top-map* "s-r"   "utl-toggle-root")
(utl-define-key *top-map* "s-w"   "windowlist")
(utl-define-key *top-map* "M-s-w" "vgroups")
(utl-define-key *top-map* "M-s-g" "grouplist")
(utl-define-key *top-map* "H-o"   "utl-next H-o")
(utl-define-key *top-map* "s-H-o" "utl-toggle-ignore-emacs")
(utl-define-key *top-map* "s-n"   "gother")
(utl-define-key *top-map* "M-s-n" "utl-gmove-to-other-group")

(utl-define-key *tile-group-top-map* "s-f"   "fullscreen")
(utl-define-key *tile-group-top-map* "s-v"   "vsplit")
(utl-define-key *tile-group-top-map* "s-z"   "hsplit")
(utl-define-key *tile-group-top-map* "s-x"   "only")

;;; Focusing windows

(utl-define-key *tile-group-top-map*  "s-b"            "pull-hidden-other")
(utl-define-key *tile-group-top-map*  "M-s-b"          "pull-hidden-next")
(utl-define-key *tile-group-top-map*  "s-TAB"          "pull-hidden-next")
(utl-define-key *tile-group-top-map*  "s-ISO_Left_Tab" "pull-hidden-previous")
(utl-define-key *float-group-top-map* "s-b"            "utl-float-window-other")
(utl-define-key *float-group-top-map* "M-s-b"          "utl-float-window-next")
(utl-define-key *float-group-top-map* "s-TAB"          "utl-float-window-next")
(utl-define-key *float-group-top-map* "s-ISO_Left_Tab" "utl-float-window-previous")

;;; Moving/resizing windows

;; Use numpad keys for manipulating windows:
;;   [C-]M-<key> for resizing
;;   [C-]s-<key> for moving
;;   C-<key> for moving to the screen edges

(utl-define-key *float-group-top-map* "s-KP_Begin" "utl-float-window-gravity center")

(defun utl-define-numpad-key-xy (map modifier cmd val)
  (flet ((dk (key x y)
           (define-key map (kbd (concat modifier key))
             (format nil "~a ~D ~D" cmd x y))))
    (dk "KP_Home"      (- val) (- val))
    (dk "KP_Up"        0       (- val))
    (dk "KP_Page_Up"   val     (- val))
    (dk "KP_Right"     val     0)
    (dk "KP_Page_Down" val     val)
    (dk "KP_Down"      0       val)
    (dk "KP_End"       (- val) val)
    (dk "KP_Left"      (- val) 0)))

(utl-define-numpad-key-xy *float-group-top-map* "s-"   "utl-move-float-window" 10)
(utl-define-numpad-key-xy *float-group-top-map* "C-s-" "utl-move-float-window" 1)
(utl-define-numpad-key-xy *float-group-top-map* "M-"   "utl-resize-float-window" 10)
(utl-define-numpad-key-xy *float-group-top-map* "C-M-" "utl-resize-float-window" 1)

(defun utl-define-numpad-key-gravity (map modifier cmd)
  (flet ((dk (key gravity)
           (define-key map (kbd (concat modifier key))
             (format nil "~a ~a" cmd gravity))))
    (dk "KP_Begin"     "center")
    (dk "KP_Home"      "top-left")
    (dk "KP_Up"        "top")
    (dk "KP_Page_Up"   "top-right")
    (dk "KP_Right"     "right")
    (dk "KP_Page_Down" "bottom-right")
    (dk "KP_Down"      "bottom")
    (dk "KP_End"       "bottom-left")
    (dk "KP_Left"      "left")))

(utl-define-numpad-key-gravity *tile-group-top-map*  "C-" "gravity")
(utl-define-numpad-key-gravity *float-group-top-map* "C-" "utl-float-window-gravity")

;;; Resizing frames

(utl-define-key *top-map* "s-XF86AudioRaiseVolume"     "resize   0  10")
(utl-define-key *top-map* "s-XF86AudioLowerVolume"     "resize   0 -10")
(utl-define-key *top-map* "C-s-XF86AudioRaiseVolume"   "resize   0   1")
(utl-define-key *top-map* "C-s-XF86AudioLowerVolume"   "resize   0  -1")
(utl-define-key *top-map* "M-s-XF86AudioRaiseVolume"   "resize  10   0")
(utl-define-key *top-map* "M-s-XF86AudioLowerVolume"   "resize -10   0")
(utl-define-key *top-map* "C-M-s-XF86AudioRaiseVolume" "resize   1   0")
(utl-define-key *top-map* "C-M-s-XF86AudioLowerVolume" "resize  -1   0")


;;; Controlling sound

(utl-define-key *top-map* "XF86AudioMute"            "audio-set-current-scontrol toggle")
(utl-define-key *top-map* "M-XF86AudioMute"          "audio-next-scontrol")
(utl-define-key *top-map* "XF86AudioRaiseVolume"     "audio-set-current-scontrol 1+")
(utl-define-key *top-map* "XF86AudioLowerVolume"     "audio-set-current-scontrol 1-")
(utl-define-key *top-map* "M-XF86AudioRaiseVolume"   "audio-set-current-scontrol 3+")
(utl-define-key *top-map* "M-XF86AudioLowerVolume"   "audio-set-current-scontrol 3-")
(utl-define-key *top-map* "C-M-XF86AudioRaiseVolume" "audio-set-current-scontrol 60%")
(utl-define-key *top-map* "C-M-XF86AudioLowerVolume" "audio-set-current-scontrol 20%")


;;; Controlling EMMS

(utl-define-key *top-map* "C-XF86AudioPlay"   "utl-emms-eval (emms-play-file \"~/docs/audio/grass.wav\")")
(utl-define-key *top-map* "XF86AudioPlay"     "utl-emms-eval (emms-pause)")
(utl-define-key *top-map* "XF86AudioStop"     "utl-emms-eval (emms-pause)")
(utl-define-key *top-map* "H-XF86AudioStop"   "utl-emms-eval (emms-stop)")
(utl-define-key *top-map* "s-XF86AudioStop"   "utl-emms-eval (emms-stop)")
(utl-define-key *top-map* "H-XF86AudioPrev"   "utl-emms-eval (emms-previous)")
(utl-define-key *top-map* "s-XF86AudioPrev"   "utl-emms-eval (emms-previous)")
(utl-define-key *top-map* "H-XF86AudioNext"   "utl-emms-eval (emms-next)")
(utl-define-key *top-map* "s-XF86AudioNext"   "utl-emms-eval (emms-next)")
(utl-define-key *top-map* "H-XF86AudioPlay"   "utl-emms-eval (utl-emms-first)")
(utl-define-key *top-map* "s-XF86AudioPlay"   "utl-emms-eval (utl-emms-first)")
(utl-define-key *top-map* "XF86AudioPrev"     "utl-emms-eval (utl-emms-seek-backward 10)")
(utl-define-key *top-map* "XF86AudioNext"     "utl-emms-eval (utl-emms-seek-forward  10)")
(utl-define-key *top-map* "C-XF86AudioPrev"   "utl-emms-eval (utl-emms-seek-backward 3)")
(utl-define-key *top-map* "C-XF86AudioNext"   "utl-emms-eval (utl-emms-seek-forward  3)")
(utl-define-key *top-map* "M-XF86AudioPrev"   "utl-emms-eval (utl-emms-seek-backward 60)")
(utl-define-key *top-map* "M-XF86AudioNext"   "utl-emms-eval (utl-emms-seek-forward  60)")
(utl-define-key *top-map* "C-M-XF86AudioPrev" "utl-emms-eval (utl-emms-seek-backward 180)")
(utl-define-key *top-map* "C-M-XF86AudioNext" "utl-emms-eval (utl-emms-seek-forward  180)")
(utl-define-key *top-map* "XF86AudioMedia"    "utl-emms-eval (emms-smart-browse)")
(utl-define-key *top-map* "XF86Music"         "utl-emms-eval (utl-emms-notify)")


;;; Miscellaneous bindings

(utl-define-key *resize-map* "s-g" "abort-iresize")
(utl-define-key *top-map* "s-g" "abort")
(utl-define-key *top-map* "s-h" '*help-map*)
(utl-define-key *top-map* "s-u" "time")
(utl-define-key *top-map* "s-i" "info")
(utl-define-key *top-map* "s-l" "lastmsg")
(utl-define-key *top-map* "s-d" "utl-send-key-to-emacs XF86Spell")
(utl-define-key *top-map* "XF86Documents"    "utl-emacs-eval-show (find-file \"~/notes/bookmarks.org\")")
(utl-define-key *top-map* "C-XF86Documents"  "utl-emacs-eval-show (org-capture 1 \"n\")")
(utl-define-key *top-map* "M-XF86Documents"  "utl-emacs-eval-show (find-file \"~/notes/tasks.org\")")
(utl-define-key *top-map* "S-XF86Documents"  "utl-emacs-eval-show (find-file \"~/notes/info.org\")")
(utl-define-key *top-map* "XF86Calculator"   "utl-emacs-eval-show (calc)")
(utl-define-key *top-map* "C-XF86Calculator" "utl-emacs-eval-show (calendar)")
(utl-define-key *top-map* "H-ESC" "exec mosdctl --hide-all")
(utl-define-key *top-map* "s-7" "layout-set 0 s-7")
(utl-define-key *top-map* "s-8" "layout-set 1 s-8")
(utl-define-key *top-map* "s-9" "layout-set 2 s-9")
(utl-define-key *top-map* "H-y" "utl-yank-primary")
(utl-define-key *top-map* "F12"                 "exec capture desktop")
(utl-define-key *top-map* "M-F12"               "exec capture image")
(utl-define-key *top-map* "M-SunPrint_Screen"   "exec capture image")
(utl-define-key *top-map* "C-S-F12"             "exec capture video")
(utl-define-key *top-map* "C-S-SunPrint_Screen" "exec capture video")
(utl-define-key *top-map* "s-F12"               "exec clock")
(utl-define-key *top-map* "s-SunPrint_Screen"   "exec clock")
(utl-define-key *top-map* "C-M-s-XF86LogOff"    "exec sudo shutdown now")
(utl-define-key *top-map* "XF86TouchpadToggle"  "exec touchpad-toggle")
(utl-define-key *top-map* "XF86Sleep"           "exec monitor blank")
(utl-define-key *top-map* "C-XF86Sleep"         "exec monitor suspend")
(utl-define-key *top-map* "M-XF86Sleep"         "exec monitor off")

;; root map
(utl-define-key *root-map* "V"   "version")
(utl-define-key *root-map* "c"   "colon")
(utl-define-key *root-map* "v"   "eval")
(utl-define-key *root-map* "i"   "list-window-properties")
(utl-define-key *root-map* "s-b" "ratwarp 800 0")

;; menu map
(utl-define-key *menu-map* "s-c" 'menu-up)
(utl-define-key *menu-map* "s-t" 'menu-down)
(utl-define-key *menu-map* "C-." 'menu-up)
(utl-define-key *menu-map* "C-e" 'menu-down)
(utl-define-key *menu-map* "M-." 'menu-scroll-up)
(utl-define-key *menu-map* "M-e" 'menu-scroll-down)
(utl-define-key *menu-map* "s-m" 'menu-finish)
(utl-define-key *menu-map* "s-g" 'menu-abort)

;; input map
(utl-define-key *input-map* "C-p" 'input-delete-backward-char)
(utl-define-key *input-map* "M-p" 'input-backward-kill-word)
(utl-define-key *input-map* "C-," 'input-delete-forward-char)
(utl-define-key *input-map* "M-," 'input-forward-kill-word)
(utl-define-key *input-map* "C-u" 'input-forward-char)
(utl-define-key *input-map* "M-u" 'input-forward-word)
(utl-define-key *input-map* "C-o" 'input-backward-char)
(utl-define-key *input-map* "M-o" 'input-backward-word)
(utl-define-key *input-map* "C-a" 'input-move-beginning-of-line)
(utl-define-key *input-map* "C-i" 'input-move-end-of-line)
(utl-define-key *input-map* "M-<" 'input-kill-line)
(utl-define-key *input-map* "M-P" 'input-kill-to-beginning)
(utl-define-key *input-map* "M-." 'input-history-back)
(utl-define-key *input-map* "M-e" 'input-history-forward)
(utl-define-key *input-map* "C-m" 'input-submit)


;;; Web jumps

(defvar *utl-web-map* (make-sparse-keymap)
  "Keymap for quick browsing with conkeror.")
(defvar *utl-web-wiki-map* (make-sparse-keymap)
  "Keymap for quick browsing wikipedia with conkeror.")
(utl-define-key *top-map* "F5" '*utl-web-map*)
(utl-define-key *utl-web-map* "F5" "utl-conkeror-eval-show find-url-new-buffer")
(utl-define-key *utl-web-map* "g"  "utl-conkeror-eval-show internet-search-github-prompted")
(utl-define-key *utl-web-map* "G"  "utl-conkeror-eval-show internet-search-google-prompted")
(utl-define-key *utl-web-map* "d"  "utl-conkeror-eval-show internet-search-duckduckgo-prompted")
(utl-define-key *utl-web-map* "c"  "utl-conkeror-eval-show internet-search-conkeror-prompted")
(utl-define-key *utl-web-map* "e"  "utl-conkeror-eval-show internet-search-emacswiki-prompted")
(utl-define-key *utl-web-map* "a"  "utl-conkeror-eval-show internet-search-archwiki-prompted")
(utl-define-key *utl-web-map* "A"  "utl-conkeror-eval-show internet-search-arch-package-prompted")
(utl-define-key *utl-web-map* "m"  "utl-conkeror-eval-show internet-search-multitran-prompted")
(utl-define-key *utl-web-map* "i"  "utl-conkeror-eval-show internet-search-ip-prompted")
(utl-define-key *utl-web-map* "y"  "utl-conkeror-eval-show internet-search-youtube-prompted")
(utl-define-key *utl-web-map* "p"  "utl-conkeror-eval-show internet-search-python3-prompted")
(utl-define-key *utl-web-map* "z"  "utl-conkeror-browse-show zeus")
(utl-define-key *utl-web-map* "s"  "utl-conkeror-browse-show http://news.sportbox.ru/video?channel=all")
(utl-define-key *utl-web-map* "t"  "utl-conkeror-browse-show http://tv.yandex.ru/4/?period=all-day")
(utl-define-key *utl-web-map* "T"  "utl-conkeror-browse-show http://www.programma.tv")
(utl-define-key *utl-web-map* "M"  "utl-conkeror-browse-show https://maps.google.com/maps?hl=ru")
(utl-define-key *utl-web-map* "W"  "utl-conkeror-browse-show http://www.gismeteo.ru/city/hourly/5039/")
(utl-define-key *utl-web-map* "w" '*utl-web-wiki-map*)
(utl-define-key *utl-web-wiki-map* "e" "utl-conkeror-eval-show internet-search-wikipedia-en-prompted")
(utl-define-key *utl-web-wiki-map* "r" "utl-conkeror-eval-show internet-search-wikipedia-ru-prompted")

;; tv and radio jumps
(defvar *utl-tv-radio-map* (make-sparse-keymap)
  "Keymap for quick access to tv and radio resources.")
(utl-define-key *top-map* "F6" '*utl-tv-radio-map*)
(utl-define-key *utl-tv-radio-map* "v" "utl-emms-eval (emms-play-url \"mms://live.rfn.ru/vesti_fm\")")
(utl-define-key *utl-tv-radio-map* "o" "utl-conkeror-browse-show http://www.onlinetv.ru/")
(utl-define-key *utl-tv-radio-map* "e" "utl-send-key-to-emacs C-M-s-e")


;;; Executing progs

(defvar *utl-exec-map* (make-sparse-keymap)
  "Keymap for executing shell commands or switching to running applications.")
(utl-define-key *top-map* "s-m" '*utl-exec-map*)
(utl-define-key *utl-exec-map* "s-m" "exec")
(utl-define-key *utl-exec-map* "e" "utl-emacs")
(utl-define-key *utl-exec-map* "E" "utl-emacs-trunk")
(utl-define-key *utl-exec-map* "t" "utl-xterm")
(utl-define-key *utl-exec-map* "T" "exec xterm")
(utl-define-key *utl-exec-map* "c" "utl-conkeror")
(utl-define-key *utl-exec-map* "f" "utl-firefox")
(utl-define-key *utl-exec-map* "v" "exec slimevolley")
(utl-define-key *utl-exec-map* "g" "utl-gcolor2")
(utl-define-key *utl-exec-map* "G" "exec gcolor2")
(utl-define-key *utl-exec-map* "m" "utl-emacs-eval (mana-start)")
(utl-define-key *utl-exec-map* "7" "utl-gtypist d.typ")
(utl-define-key *utl-exec-map* "8" "utl-gtypist ru.typ")
(utl-define-key *utl-exec-map* "9" "utl-gtypist")


;;; Mode line

(defvar *utl-mode-line-map* (make-sparse-keymap)
  "Keymap for controlling the mode line.")
(utl-define-key *top-map* "M-s-m" '*utl-mode-line-map*)
(utl-define-key *utl-mode-line-map* "M-s-m" "mode-line")
(utl-define-key *utl-mode-line-map* "t" "mode-line")
(utl-define-key *utl-mode-line-map* "." "utl-mode-line-top")
(utl-define-key *utl-mode-line-map* "e" "utl-mode-line-bottom")


;;; Mana

(defvar *utl-mana-map* (make-sparse-keymap)
  "Keymap for sending keys to Mana.")
(utl-define-key *top-map* "s-j" '*utl-mana-map*)
(utl-define-key *utl-mana-map* "s-j" "mana-state")
(utl-define-key *utl-mana-map* "a" "mana-attack")
(utl-define-key *utl-mana-map* "A" "mana-quick-attack")
(utl-define-key *utl-mana-map* "H" "mana-bat-quest-full")
(utl-define-key *utl-mana-map* "h" "mana-bat-quest-cont")
(utl-define-key *utl-mana-map* "l" "utl-emacs-eval-show (find-file \"~/.local/share/mana/logs/server.themanaworld.org/\")")
(utl-define-key *utl-mana-map* "m" "utl-emacs-eval-show (find-file \"~/notes/tmw/monsters.org\")")
(utl-define-key *utl-mana-map* "q" "utl-emacs-eval-show (find-file \"~/notes/tmw/quests.org\")")
(utl-define-key *utl-mana-map* "s" "utl-emacs-eval-show (find-file \"~/notes/tmw/spells.org\")")
(utl-define-key *utl-mana-map* "S" "utl-emacs-eval-show (find-file \"~/src/tmw/tmwa-server-data/world/map/npc\")")
(utl-define-key *utl-mana-map* "b" "mana-break")
(utl-define-key *utl-mana-map* "k" "mana-kill")
(utl-define-key *utl-mana-map* "w" "utl-conkeror-eval-show internet-search-mana-prompted")

;;; keys.lisp ends here
