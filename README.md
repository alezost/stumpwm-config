## About

My configuration for [StumpWM](https://github.com/stumpwm/stumpwm).  I
separate my settings into several files instead of using a single
`.stumpwmrc`.

## Files

- `init.lisp` – The main file for loading the other ones (`~/.stumpwmrc` is a
  symlink to this file).
- `keys.lisp` – All my keybindings (I use dvorak layout, so some of them
  may look weird).
- `layouts.lisp` – Configuration for
  [clx-xkeyboard extension](https://github.com/filonenko-mikhail/clx-xkeyboard).
- `mana.lisp` – Some commands for convenient playing
  [The Mana World](https://themanaworld.org/).
- `setaudio.lisp` – An interface to `setaudio` program.  This file is
  probably of no interest: `setaudio` is a simplified version of
  `amixer` I wrote (and have not published yet), the main difference is
  that it shows OSD with volume parameters.
- `settings.lisp` – Some miscellaneous settings.
- `utils.lisp` – Additional functions and commands I use (all symbols
  there have `al/` prefix).
- `visual.lisp` – Visual settings: colors, mode-line and other visual
  appearance.

## Feedback

I am not an expert in Lisp, I just like StumpWM a lot, so if you find
errors or if you see that something can be implemented better, you may
send me a message about that.
