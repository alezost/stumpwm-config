;;; keys.lisp --- key bindings (Dvorak layout)

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 28 Jun 2013

;;; Code:

(in-package :stumpwm)

(set-prefix-key (kbd "s-t"))
(define-key *top-map* (kbd "s-h") '*help-map*)


;;; Managing windows, frames & groups

(define-key *top-map* (kbd "s-r"       ) "al-toggle-root")
(define-key *top-map* (kbd "s-f"       ) "fullscreen")
(define-key *top-map* (kbd "s-w"       ) "windowlist")
(define-key *top-map* (kbd "M-s-w"     ) "vgroups")
(define-key *top-map* (kbd "s-b"       ) "pull-hidden-other")
(define-key *top-map* (kbd "s-s"       ) "pull-hidden-next")
(define-key *top-map* (kbd "M-s-s"     ) "pull-hidden-previous")
(define-key *top-map* (kbd "s-TAB"     ) "pull-hidden-next")
(define-key *top-map* (kbd "H-o"       ) "al-fnext H-o")
(define-key *top-map* (kbd "s-H-o"     ) "al-fnext-emacs-toggle")
(define-key *top-map* (kbd "s-n"       ) "gother")
(define-key *top-map* (kbd "M-s-n"     ) "al-gmove-to-other-group")
(define-key *top-map* (kbd "s-v"       ) "vsplit")
(define-key *top-map* (kbd "M-s-z"     ) "hsplit")
(define-key *top-map* (kbd "s--"       ) "iresize")
(define-key *top-map* (kbd "s-x"       ) "only")


;;; Moving/resizing windows

;; Use numpad keys for manipulating windows:
;;   [C-]M-<key> for resizing
;;   [C-]s-<key> for moving
;;   C-<key> for moving to the screen edges

(define-key *float-group-top-map* (kbd "s-KP_Begin") "float-window-gravity center")

(defun define-numpad-key-xy (map modifier cmd val)
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

(define-numpad-key-xy *float-group-top-map* "s-"   "move-float-window" 10)
(define-numpad-key-xy *float-group-top-map* "C-s-" "move-float-window" 1)
(define-numpad-key-xy *float-group-top-map* "M-"   "resize-float-window" 10)
(define-numpad-key-xy *float-group-top-map* "C-M-" "resize-float-window" 1)

(defun define-numpad-key-gravity (map modifier cmd)
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

(define-numpad-key-gravity *tile-group-top-map*  "C-" "gravity")
(define-numpad-key-gravity *float-group-top-map* "C-" "float-window-gravity")


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

(define-key *top-map* (kbd "XF86AudioMute"            ) "setaudio toggle")
(define-key *top-map* (kbd "M-XF86AudioMute"          ) "next-scontrol")
(define-key *top-map* (kbd "XF86AudioRaiseVolume"     ) "setaudio 1+")
(define-key *top-map* (kbd "XF86AudioLowerVolume"     ) "setaudio 1-")
(define-key *top-map* (kbd "M-XF86AudioRaiseVolume"   ) "setaudio 3+")
(define-key *top-map* (kbd "M-XF86AudioLowerVolume"   ) "setaudio 3-")
(define-key *top-map* (kbd "C-M-XF86AudioRaiseVolume" ) "setaudio 60%")
(define-key *top-map* (kbd "C-M-XF86AudioLowerVolume" ) "setaudio 20%")


;;; Controlling EMMS

(define-key *top-map* (kbd "C-XF86AudioPlay" ) "emacs-eval (emms-play-file \"~/docs/audio/grass.wav\")")
(define-key *top-map* (kbd "XF86AudioPlay"   ) "emacs-eval (emms-pause)")
(define-key *top-map* (kbd "XF86AudioStop"   ) "emacs-eval (emms-stop)")
(define-key *top-map* (kbd "XF86AudioPrev"   ) "emacs-eval (emms-previous)")
(define-key *top-map* (kbd "XF86AudioNext"   ) "emacs-eval (emms-next)")
(define-key *top-map* (kbd "C-XF86AudioPrev" ) "emacs-eval (emms-seek-backward)")
(define-key *top-map* (kbd "C-XF86AudioNext" ) "emacs-eval (emms-seek-forward)")
(define-key *top-map* (kbd "M-XF86AudioPrev" ) "emacs-eval (emms-big-seek-backward)")
(define-key *top-map* (kbd "M-XF86AudioNext" ) "emacs-eval (emms-big-seek-forward)")
(define-key *top-map* (kbd "XF86AudioMedia"  ) "emacs-eval (emms-smart-browse)")


;;; Miscellaneous bindings

(define-key *resize-map* (kbd "s-g") "abort-iresize")
(define-key *top-map* (kbd "s-g"   ) "abort")
(define-key *top-map* (kbd "s-u"   ) "time")
(define-key *top-map* (kbd "s-i"   ) "info")
(define-key *top-map* (kbd "s-l"   ) "lastmsg")
(define-key *top-map* (kbd "s-z"   ) "banish")
(define-key *top-map* (kbd "s-d"               ) "send-key-to-emacs XF86Spell")
(define-key *top-map* (kbd "XF86HomePage"      ) "firefox")
(define-key *top-map* (kbd "XF86Documents"     ) "emacs-eval-show (find-file \"~/notes/bookmarks.org\")")
(define-key *top-map* (kbd "C-XF86Documents"   ) "emacs-eval-show (org-capture 1 \"n\")")
(define-key *top-map* (kbd "M-XF86Documents"   ) "emacs-eval-show (find-file \"~/notes/tasks.org\")")
(define-key *top-map* (kbd "S-XF86Documents"   ) "emacs-eval-show (find-file \"~/notes/info.org\")")
(define-key *top-map* (kbd "XF86Calculator"    ) "emacs-eval-show (calc)")
(define-key *top-map* (kbd "C-XF86Calculator"  ) "emacs-eval-show (calendar)")
(define-key *top-map* (kbd "H-ESC"             ) "exec mosdctl --hide-all")
(define-key *top-map* (kbd "s-7"               ) "set-layout 0")
(define-key *top-map* (kbd "s-8"               ) "set-layout 1")
(define-key *top-map* (kbd "s-9"               ) "set-layout 2")
(define-key *top-map* (kbd "H-y"               ) "al-yank-primary")
(define-key *top-map* (kbd "s-SunPrint_Screen" ) "exec clock")
(define-key *top-map* (kbd "SunPrint_Screen"   ) "exec scrot \"/home/alexx/temp/screenshots/%Y-%m-%d_%H%M%S.png\"")
(define-key *top-map* (kbd "M-SunPrint_Screen" ) "exec scrot -s \"/home/alexx/temp/screenshots/%Y-%m-%d_%H%M%S.png\"")
(define-key *top-map* (kbd "s-XF86LogOff"      ) "exec systemctl --user start lxde@0")
(define-key *top-map* (kbd "C-M-s-XF86LogOff"  ) "exec sudo shutdown now")

;; root map
(define-key *root-map* (kbd "F"   ) "fselect")
(define-key *root-map* (kbd "="   ) "balance-frames")
(define-key *root-map* (kbd "s-c" ) "delete-window")
(define-key *root-map* (kbd "C"   ) "kill-window")
(define-key *root-map* (kbd "V"   ) "version")
(define-key *root-map* (kbd "c"   ) "colon")
(define-key *root-map* (kbd "v"   ) "eval")
(define-key *root-map* (kbd "i"   ) "list-window-properties")

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

(defvar *al-web-map* (make-sparse-keymap)
  "Keymap for quick browsing with conkeror.")
(defvar *al-web-wiki-map* (make-sparse-keymap)
  "Keymap for quick browsing wikipedia with conkeror.")
(define-key *top-map*    (kbd "XF86Open") '*al-web-map*)
(define-key *al-web-map* (kbd "XF86Open") "conkeror-eval-show find-url-new-buffer")
(define-key *al-web-map* (kbd "g") "conkeror-eval-show internet-search-github-prompted")
(define-key *al-web-map* (kbd "G") "conkeror-eval-show internet-search-google-prompted")
(define-key *al-web-map* (kbd "d") "conkeror-eval-show internet-search-duckduckgo-prompted")
(define-key *al-web-map* (kbd "c") "conkeror-eval-show internet-search-conkeror-prompted")
(define-key *al-web-map* (kbd "e") "conkeror-eval-show internet-search-emacswiki-prompted")
(define-key *al-web-map* (kbd "a") "conkeror-eval-show internet-search-archwiki-prompted")
(define-key *al-web-map* (kbd "A") "conkeror-eval-show internet-search-arch-package-prompted")
(define-key *al-web-map* (kbd "m") "conkeror-eval-show internet-search-multitran-prompted")
(define-key *al-web-map* (kbd "i") "conkeror-eval-show internet-search-ip-prompted")
(define-key *al-web-map* (kbd "y") "conkeror-eval-show internet-search-youtube-prompted")
(define-key *al-web-map* (kbd "p") "conkeror-eval-show internet-search-python3-prompted")
(define-key *al-web-map* (kbd "z") "conkeror-browse-show zeus")
(define-key *al-web-map* (kbd "s") "conkeror-browse-show http://news.sportbox.ru/video?channel=all")
(define-key *al-web-map* (kbd "S") "conkeror-browse-show http://www.stopstream.com")
(define-key *al-web-map* (kbd "t") "conkeror-browse-show http://www.programma.tv")
(define-key *al-web-map* (kbd "k") "conkeror-browse-show http://www.keyhero.com/typing-tests-wpm/")
(define-key *al-web-map* (kbd "M") "conkeror-browse-show https://maps.google.com/maps?hl=ru")
(define-key *al-web-map* (kbd "W") "conkeror-browse-show http://www.gismeteo.ru/city/hourly/5039/")
(define-key *al-web-map* (kbd "w") '*al-web-wiki-map*)
(define-key *al-web-wiki-map* (kbd "e") "conkeror-eval-show internet-search-wikipedia-en-prompted")
(define-key *al-web-wiki-map* (kbd "r") "conkeror-eval-show internet-search-wikipedia-ru-prompted")

;; tv and radio jumps
(defvar *al-tv-radio-map* (make-sparse-keymap)
  "Keymap for quick access to tv and radio resources.")
(defvar *al-echo-map* (make-sparse-keymap)
  "Keymap for quick access to 'Эхо Москвы' resources.")
(define-key *top-map* (kbd "XF86Close") '*al-tv-radio-map*)
;; (define-key *al-tv-radio-map* (kbd "r") "conkeror-browse-show http://player.tvrain.ru/iframe_with_ova_large.php")
(define-key *al-tv-radio-map* (kbd "r") "conkeror-browse-show http://slon.ru/tvrain/player/iframe_with_ova.php")
(define-key *al-tv-radio-map* (kbd "v") "emacs-eval (emms-play-url \"mms://live.rfn.ru/vesti_fm\")")
(define-key *al-tv-radio-map* (kbd "o") "conkeror-browse-show http://www.onlinetv.ru/")
(define-key *al-tv-radio-map* (kbd "e") '*al-echo-map*)
(define-key *al-echo-map* (kbd "a") "conkeror-browse-show http://www.echo.msk.ru/sounds/stream.html")
(define-key *al-echo-map* (kbd "v") "conkeror-browse-show http://echomsk.onlinetv.ru/player")
(define-key *al-echo-map* (kbd "s") "emacs-eval-show (call-interactively 'echo-schedule)")
(define-key *al-echo-map* (kbd "p") "emacs-eval-show (call-interactively 'echo-program-task)")


;;; Executing progs

(defvar *al-exec-map* (make-sparse-keymap)
  "Keymap for executing shell commands or switching to running applications.")
(define-key *top-map*     (kbd "s-m") '*al-exec-map*)
(define-key *al-exec-map* (kbd "s-m") "exec")
(define-key *al-exec-map* (kbd "e"  ) "emacs")
(define-key *al-exec-map* (kbd "E"  ) "exec emacs")
(define-key *al-exec-map* (kbd "t"  ) "xterm")
(define-key *al-exec-map* (kbd "T"  ) "exec xterm")
(define-key *al-exec-map* (kbd "c"  ) "conkeror")
(define-key *al-exec-map* (kbd "f"  ) "firefox")
(define-key *al-exec-map* (kbd "v"  ) "exec slimevolley")
(define-key *al-exec-map* (kbd "g"  ) "exec gcolor2")
(define-key *al-exec-map* (kbd "M"  ) "exec manaplus")
(define-key *al-exec-map* (kbd "m"  ) "mana-exec")
(define-key *al-exec-map* (kbd "7"  ) "gtypist d.typ")
(define-key *al-exec-map* (kbd "8"  ) "gtypist ru.typ")
(define-key *al-exec-map* (kbd "9"  ) "gtypist")
(define-key *al-exec-map* (kbd "s-9") "emacs-eval-show (call-interactively 'gtypist-exec-prompt)")


;;; Mode line

(defvar *al-mode-line-map* (make-sparse-keymap)
  "Keymap for controlling the mode line.")
(define-key *top-map* (kbd "M-s-m") '*al-mode-line-map*)
(define-key *al-mode-line-map* (kbd "M-s-m") "mode-line")
(define-key *al-mode-line-map* (kbd "t") "mode-line")
(define-key *al-mode-line-map* (kbd ".") "al-mode-line-top")
(define-key *al-mode-line-map* (kbd "e") "al-mode-line-bottom")


;;; Mana

(defvar *al-mana-map* (make-sparse-keymap)
  "Keymap for sending keys to Mana.")
(define-key *top-map*     (kbd "s-j") '*al-mana-map*)
(define-key *al-mana-map* (kbd "s-j") "mana-state")
(define-key *al-mana-map* (kbd "a") "mana-attack")
(define-key *al-mana-map* (kbd "A") "mana-quick-attack")
(define-key *al-mana-map* (kbd "H") "mana-bat-quest-full")
(define-key *al-mana-map* (kbd "h") "mana-bat-quest-cont")
(define-key *al-mana-map* (kbd "l") "emacs-eval-show (find-file \"~/.local/share/mana/logs/server.themanaworld.org/\")")
(define-key *al-mana-map* (kbd "m") "emacs-eval-show (find-file \"~/notes/tmw/monsters.org\")")
(define-key *al-mana-map* (kbd "q") "emacs-eval-show (find-file \"~/notes/tmw/quests.org\")")
(define-key *al-mana-map* (kbd "s") "emacs-eval-show (find-file \"~/notes/tmw/spells.org\")")
(define-key *al-mana-map* (kbd "S") "emacs-eval-show (find-file \"~/src/tmw/tmwa-server-data/world/map/npc\")")
(define-key *al-mana-map* (kbd "b") "mana-break")
(define-key *al-mana-map* (kbd "k") "mana-kill")
(define-key *al-mana-map* (kbd "w") "conkeror-eval-show internet-search-mana-prompted")

;;; keys.lisp ends here
