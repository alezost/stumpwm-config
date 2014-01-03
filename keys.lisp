;;; keys.lisp --- key bindings (Dvorak layout)

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 28 Jun 2013

;;; Code:

(in-package :stumpwm)

(set-prefix-key (kbd "s-t"))


;;; Managing windows, frames & groups

(define-key *root-map* (kbd "F"   ) "fselect")
(define-key *root-map* (kbd "="   ) "balance-frames")
(define-key *root-map* (kbd "s-c" ) "delete-window")
(define-key *root-map* (kbd "C"   ) "kill-window")

(define-key *top-map* (kbd "s-r"   ) "utl-toggle-root")
(define-key *top-map* (kbd "s-w"   ) "windowlist")
(define-key *top-map* (kbd "M-s-w" ) "vgroups")
(define-key *top-map* (kbd "H-o"   ) "utl-fnext H-o")
(define-key *top-map* (kbd "s-H-o" ) "utl-fnext-emacs-toggle")
(define-key *top-map* (kbd "s-n"   ) "gother")
(define-key *top-map* (kbd "M-s-n" ) "utl-gmove-to-other-group")

(define-key *tile-group-top-map* (kbd "s-f"   ) "fullscreen")
(define-key *tile-group-top-map* (kbd "s-v"   ) "vsplit")
(define-key *tile-group-top-map* (kbd "M-s-z" ) "hsplit")
(define-key *tile-group-top-map* (kbd "s-x"   ) "only")

;;; Focusing windows

(define-key *tile-group-top-map*  (kbd "s-b"            ) "pull-hidden-other")
(define-key *tile-group-top-map*  (kbd "s-s"            ) "pull-hidden-next")
(define-key *tile-group-top-map*  (kbd "M-s-s"          ) "pull-hidden-previous")
(define-key *tile-group-top-map*  (kbd "s-TAB"          ) "pull-hidden-next")
(define-key *tile-group-top-map*  (kbd "s-ISO_Left_Tab" ) "pull-hidden-previous")
(define-key *float-group-top-map* (kbd "s-b"            ) "utl-float-window-other")
(define-key *float-group-top-map* (kbd "s-s"            ) "utl-float-window-next")
(define-key *float-group-top-map* (kbd "M-s-s"          ) "utl-float-window-previous")
(define-key *float-group-top-map* (kbd "s-TAB"          ) "utl-float-window-next")
(define-key *float-group-top-map* (kbd "s-ISO_Left_Tab" ) "utl-float-window-previous")

;;; Moving/resizing windows

;; Use numpad keys for manipulating windows:
;;   [C-]M-<key> for resizing
;;   [C-]s-<key> for moving
;;   C-<key> for moving to the screen edges

(define-key *float-group-top-map* (kbd "s-KP_Begin") "utl-float-window-gravity center")

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

(define-key *top-map* (kbd "s-XF86AudioRaiseVolume") "resize 0 10")
(define-key *top-map* (kbd "s-XF86AudioLowerVolume") "resize 0 -10")
(define-key *top-map* (kbd "C-s-XF86AudioRaiseVolume") "resize 0 1")
(define-key *top-map* (kbd "C-s-XF86AudioLowerVolume") "resize 0 -1")
(define-key *top-map* (kbd "M-s-XF86AudioRaiseVolume") "resize 10 0")
(define-key *top-map* (kbd "M-s-XF86AudioLowerVolume") "resize -10 0")
(define-key *top-map* (kbd "C-M-s-XF86AudioRaiseVolume") "resize 1 0")
(define-key *top-map* (kbd "C-M-s-XF86AudioLowerVolume") "resize -1 0")


;;; Controlling sound

(define-key *top-map* (kbd "XF86AudioMute"            ) "utl-setaudio toggle")
(define-key *top-map* (kbd "M-XF86AudioMute"          ) "utl-next-scontrol")
(define-key *top-map* (kbd "XF86AudioRaiseVolume"     ) "utl-setaudio 1+")
(define-key *top-map* (kbd "XF86AudioLowerVolume"     ) "utl-setaudio 1-")
(define-key *top-map* (kbd "M-XF86AudioRaiseVolume"   ) "utl-setaudio 3+")
(define-key *top-map* (kbd "M-XF86AudioLowerVolume"   ) "utl-setaudio 3-")
(define-key *top-map* (kbd "C-M-XF86AudioRaiseVolume" ) "utl-setaudio 60%")
(define-key *top-map* (kbd "C-M-XF86AudioLowerVolume" ) "utl-setaudio 20%")


;;; Controlling EMMS

(define-key *top-map* (kbd "C-XF86AudioPlay" ) "utl-emacs-eval (emms-play-file \"~/docs/audio/grass.wav\")")
(define-key *top-map* (kbd "XF86AudioPlay"   ) "utl-emacs-eval (emms-pause)")
(define-key *top-map* (kbd "XF86AudioStop"   ) "utl-emacs-eval (emms-stop)")
(define-key *top-map* (kbd "XF86AudioPrev"   ) "utl-emacs-eval (emms-previous)")
(define-key *top-map* (kbd "XF86AudioNext"   ) "utl-emacs-eval (emms-next)")
(define-key *top-map* (kbd "C-XF86AudioPrev" ) "utl-emacs-eval (emms-seek-backward)")
(define-key *top-map* (kbd "C-XF86AudioNext" ) "utl-emacs-eval (emms-seek-forward)")
(define-key *top-map* (kbd "M-XF86AudioPrev" ) "utl-emacs-eval (emms-big-seek-backward)")
(define-key *top-map* (kbd "M-XF86AudioNext" ) "utl-emacs-eval (emms-big-seek-forward)")
(define-key *top-map* (kbd "XF86AudioMedia"  ) "utl-emacs-eval (emms-smart-browse)")


;;; Miscellaneous bindings

(define-key *resize-map* (kbd "s-g") "abort-iresize")
(define-key *top-map* (kbd "s-g"   ) "abort")
(define-key *top-map* (kbd "s-h"   ) '*help-map*)
(define-key *top-map* (kbd "s-u"   ) "time")
(define-key *top-map* (kbd "s-i"   ) "info")
(define-key *top-map* (kbd "s-l"   ) "lastmsg")
(define-key *top-map* (kbd "s-z"   ) "utl-emacs-eval-show (eshell)")
(define-key *top-map* (kbd "s-d"               ) "utl-send-key-to-emacs XF86Spell")
(define-key *top-map* (kbd "XF86HomePage"      ) "firefox")
(define-key *top-map* (kbd "XF86Documents"     ) "utl-emacs-eval-show (find-file \"~/notes/bookmarks.org\")")
(define-key *top-map* (kbd "C-XF86Documents"   ) "utl-emacs-eval-show (org-capture 1 \"n\")")
(define-key *top-map* (kbd "M-XF86Documents"   ) "utl-emacs-eval-show (find-file \"~/notes/tasks.org\")")
(define-key *top-map* (kbd "S-XF86Documents"   ) "utl-emacs-eval-show (find-file \"~/notes/info.org\")")
(define-key *top-map* (kbd "XF86Calculator"    ) "utl-emacs-eval-show (calc)")
(define-key *top-map* (kbd "C-XF86Calculator"  ) "utl-emacs-eval-show (calendar)")
(define-key *top-map* (kbd "H-ESC"             ) "exec mosdctl --hide-all")
(define-key *top-map* (kbd "s-7"               ) "set-layout 0")
(define-key *top-map* (kbd "s-8"               ) "set-layout 1")
(define-key *top-map* (kbd "s-9"               ) "set-layout 2")
(define-key *top-map* (kbd "H-y"               ) "utl-yank-primary")
(define-key *top-map* (kbd "s-SunPrint_Screen" ) "exec clock")
(define-key *top-map* (kbd "SunPrint_Screen"   ) "exec scrot \"/home/alexx/temp/screenshots/%Y-%m-%d_%H%M%S.png\"")
(define-key *top-map* (kbd "M-SunPrint_Screen" ) "exec scrot -s \"/home/alexx/temp/screenshots/%Y-%m-%d_%H%M%S.png\"")
(define-key *top-map* (kbd "s-XF86LogOff"      ) "exec systemctl --user start lxde@0")
(define-key *top-map* (kbd "C-M-s-XF86LogOff"  ) "exec sudo shutdown now")

;; root map
(define-key *root-map* (kbd "V"   ) "version")
(define-key *root-map* (kbd "c"   ) "colon")
(define-key *root-map* (kbd "v"   ) "eval")
(define-key *root-map* (kbd "i"   ) "list-window-properties")
(define-key *root-map* (kbd "s-b" ) "banish")

;; menu map
(define-key *menu-map* (kbd "s-c") 'menu-up)
(define-key *menu-map* (kbd "s-t") 'menu-down)
(define-key *menu-map* (kbd "C-.") 'menu-up)
(define-key *menu-map* (kbd "C-e") 'menu-down)
(define-key *menu-map* (kbd "M-.") 'menu-scroll-up)
(define-key *menu-map* (kbd "M-e") 'menu-scroll-down)
(define-key *menu-map* (kbd "s-m") 'menu-finish)
(define-key *menu-map* (kbd "s-g") 'menu-abort)

;; input map
(define-key *input-map* (kbd "C-p") 'input-delete-backward-char)
(define-key *input-map* (kbd "M-p") 'input-backward-kill-word)
(define-key *input-map* (kbd "C-,") 'input-delete-forward-char)
(define-key *input-map* (kbd "M-,") 'input-forward-kill-word)
(define-key *input-map* (kbd "C-u") 'input-forward-char)
(define-key *input-map* (kbd "M-u") 'input-forward-word)
(define-key *input-map* (kbd "C-o") 'input-backward-char)
(define-key *input-map* (kbd "M-o") 'input-backward-word)
(define-key *input-map* (kbd "C-a") 'input-move-beginning-of-line)
(define-key *input-map* (kbd "C-i") 'input-move-end-of-line)
(define-key *input-map* (kbd "M-<") 'input-kill-line)
(define-key *input-map* (kbd "M-P") 'input-kill-to-beginning)
(define-key *input-map* (kbd "M-.") 'input-history-back)
(define-key *input-map* (kbd "M-e") 'input-history-forward)
(define-key *input-map* (kbd "C-m") 'input-submit)


;;; Web jumps

(defvar *utl-web-map* (make-sparse-keymap)
  "Keymap for quick browsing with conkeror.")
(defvar *utl-web-wiki-map* (make-sparse-keymap)
  "Keymap for quick browsing wikipedia with conkeror.")
(define-key *top-map*    (kbd "XF86Open") '*utl-web-map*)
(define-key *utl-web-map* (kbd "XF86Open") "utl-conkeror-eval-show find-url-new-buffer")
(define-key *utl-web-map* (kbd "g") "utl-conkeror-eval-show internet-search-github-prompted")
(define-key *utl-web-map* (kbd "G") "utl-conkeror-eval-show internet-search-google-prompted")
(define-key *utl-web-map* (kbd "d") "utl-conkeror-eval-show internet-search-duckduckgo-prompted")
(define-key *utl-web-map* (kbd "c") "utl-conkeror-eval-show internet-search-conkeror-prompted")
(define-key *utl-web-map* (kbd "e") "utl-conkeror-eval-show internet-search-emacswiki-prompted")
(define-key *utl-web-map* (kbd "a") "utl-conkeror-eval-show internet-search-archwiki-prompted")
(define-key *utl-web-map* (kbd "A") "utl-conkeror-eval-show internet-search-arch-package-prompted")
(define-key *utl-web-map* (kbd "m") "utl-conkeror-eval-show internet-search-multitran-prompted")
(define-key *utl-web-map* (kbd "i") "utl-conkeror-eval-show internet-search-ip-prompted")
(define-key *utl-web-map* (kbd "y") "utl-conkeror-eval-show internet-search-youtube-prompted")
(define-key *utl-web-map* (kbd "p") "utl-conkeror-eval-show internet-search-python3-prompted")
(define-key *utl-web-map* (kbd "z") "utl-conkeror-browse-show zeus")
(define-key *utl-web-map* (kbd "s") "utl-conkeror-browse-show http://news.sportbox.ru/video?channel=all")
(define-key *utl-web-map* (kbd "S") "utl-conkeror-browse-show http://www.stopstream.com")
(define-key *utl-web-map* (kbd "t") "utl-conkeror-browse-show http://www.programma.tv")
(define-key *utl-web-map* (kbd "k") "utl-conkeror-browse-show http://www.keyhero.com/typing-tests-wpm/")
(define-key *utl-web-map* (kbd "M") "utl-conkeror-browse-show https://maps.google.com/maps?hl=ru")
(define-key *utl-web-map* (kbd "W") "utl-conkeror-browse-show http://www.gismeteo.ru/city/hourly/5039/")
(define-key *utl-web-map* (kbd "w") '*utl-web-wiki-map*)
(define-key *utl-web-wiki-map* (kbd "e") "utl-conkeror-eval-show internet-search-wikipedia-en-prompted")
(define-key *utl-web-wiki-map* (kbd "r") "utl-conkeror-eval-show internet-search-wikipedia-ru-prompted")

;; tv and radio jumps
(defvar *utl-tv-radio-map* (make-sparse-keymap)
  "Keymap for quick access to tv and radio resources.")
(defvar *utl-echo-map* (make-sparse-keymap)
  "Keymap for quick access to 'Эхо Москвы' resources.")
(define-key *top-map* (kbd "XF86Close") '*utl-tv-radio-map*)
;; (define-key *utl-tv-radio-map* (kbd "r") "utl-conkeror-browse-show http://player.tvrain.ru/iframe_with_ova_large.php")
(define-key *utl-tv-radio-map* (kbd "r") "utl-conkeror-browse-show http://slon.ru/tvrain/player/iframe_with_ova.php")
(define-key *utl-tv-radio-map* (kbd "v") "utl-emacs-eval (emms-play-url \"mms://live.rfn.ru/vesti_fm\")")
(define-key *utl-tv-radio-map* (kbd "o") "utl-conkeror-browse-show http://www.onlinetv.ru/")
(define-key *utl-tv-radio-map* (kbd "e") '*utl-echo-map*)
(define-key *utl-echo-map* (kbd "a") "utl-conkeror-browse-show http://www.echo.msk.ru/sounds/stream.html")
(define-key *utl-echo-map* (kbd "v") "utl-conkeror-browse-show http://echomsk.onlinetv.ru/player")
(define-key *utl-echo-map* (kbd "s") "utl-emacs-eval-show (call-interactively 'echo-schedule)")
(define-key *utl-echo-map* (kbd "p") "utl-emacs-eval-show (call-interactively 'echo-program-task)")


;;; Executing progs

(defvar *utl-exec-map* (make-sparse-keymap)
  "Keymap for executing shell commands or switching to running applications.")
(define-key *top-map*     (kbd "s-m") '*utl-exec-map*)
(define-key *utl-exec-map* (kbd "s-m") "exec")
(define-key *utl-exec-map* (kbd "e"  ) "utl-emacs")
(define-key *utl-exec-map* (kbd "E"  ) "exec emacs")
(define-key *utl-exec-map* (kbd "t"  ) "utl-xterm")
(define-key *utl-exec-map* (kbd "T"  ) "exec xterm")
(define-key *utl-exec-map* (kbd "c"  ) "utl-conkeror")
(define-key *utl-exec-map* (kbd "f"  ) "utl-firefox")
(define-key *utl-exec-map* (kbd "v"  ) "exec slimevolley")
(define-key *utl-exec-map* (kbd "g"  ) "exec gcolor2")
(define-key *utl-exec-map* (kbd "M"  ) "exec manaplus")
(define-key *utl-exec-map* (kbd "m"  ) "mana-exec")
(define-key *utl-exec-map* (kbd "7"  ) "utl-gtypist d.typ")
(define-key *utl-exec-map* (kbd "8"  ) "utl-gtypist ru.typ")
(define-key *utl-exec-map* (kbd "9"  ) "utl-gtypist")
(define-key *utl-exec-map* (kbd "s-9") "utl-emacs-eval-show (call-interactively 'gtypist-exec-prompt)")


;;; Mode line

(defvar *utl-mode-line-map* (make-sparse-keymap)
  "Keymap for controlling the mode line.")
(define-key *top-map* (kbd "M-s-m") '*utl-mode-line-map*)
(define-key *utl-mode-line-map* (kbd "M-s-m") "mode-line")
(define-key *utl-mode-line-map* (kbd "t") "mode-line")
(define-key *utl-mode-line-map* (kbd ".") "utl-mode-line-top")
(define-key *utl-mode-line-map* (kbd "e") "utl-mode-line-bottom")


;;; Mana

(defvar *utl-mana-map* (make-sparse-keymap)
  "Keymap for sending keys to Mana.")
(define-key *top-map*     (kbd "s-j") '*utl-mana-map*)
(define-key *utl-mana-map* (kbd "s-j") "mana-state")
(define-key *utl-mana-map* (kbd "a") "mana-attack")
(define-key *utl-mana-map* (kbd "A") "mana-quick-attack")
(define-key *utl-mana-map* (kbd "H") "mana-bat-quest-full")
(define-key *utl-mana-map* (kbd "h") "mana-bat-quest-cont")
(define-key *utl-mana-map* (kbd "l") "utl-emacs-eval-show (find-file \"~/.local/share/mana/logs/server.themanaworld.org/\")")
(define-key *utl-mana-map* (kbd "m") "utl-emacs-eval-show (find-file \"~/notes/tmw/monsters.org\")")
(define-key *utl-mana-map* (kbd "q") "utl-emacs-eval-show (find-file \"~/notes/tmw/quests.org\")")
(define-key *utl-mana-map* (kbd "s") "utl-emacs-eval-show (find-file \"~/notes/tmw/spells.org\")")
(define-key *utl-mana-map* (kbd "S") "utl-emacs-eval-show (find-file \"~/src/tmw/tmwa-server-data/world/map/npc\")")
(define-key *utl-mana-map* (kbd "b") "mana-break")
(define-key *utl-mana-map* (kbd "k") "mana-kill")
(define-key *utl-mana-map* (kbd "w") "utl-conkeror-eval-show internet-search-mana-prompted")

;;; keys.lisp ends here
