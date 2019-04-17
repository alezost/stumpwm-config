## About

My configuration for [StumpWM](https://github.com/stumpwm/stumpwm).  I
separate my settings into several files instead of using a single
`.stumpwmrc`.

## Files

- `init.lisp` – The main file for loading the other ones (`~/.stumpwmrc` is a
  symlink to this file).
- `keys.lisp` – All my keybindings (I use dvorak layout, so some of them
  may look weird).
- `xkb.lisp` – Configuration for [clx-xkeyboard
  extension](https://github.com/filonenko-mikhail/clx-xkeyboard).  I use
  it to switch keyboard layout and to get/set the state of CapsLock and
  NumLock keys.
- `settings.lisp` – Some miscellaneous settings.
- `sound.lisp` – An interface for setting sound volume and showing it in
  OSD.  I use [Guile-Daemon](https://github.com/alezost/guile-daemon) and
  [Guile-XOSD](https://github.com/alezost/guile-xosd) to do this.  See
  [my Guile-Daemon config](https://github.com/alezost/guile-daemon-config)
  for details.
- `utils.lisp` – Additional functions and commands I use (all symbols
  there have `al/` prefix).
- `visual.lisp` – Visual settings: colors, mode-line and other visual
  appearance.
- `unused` directory contains some old and unused code.

## Feedback

I am not an expert in Lisp, I just like StumpWM a lot, so if you find
errors or if you see that something can be implemented better, you may
send me a message about that.
