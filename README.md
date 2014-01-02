## About

My configuration of [StumpWM](https://github.com/sabetts/stumpwm).  I
separated my settings into several files instead of using a single
`.stumpwmrc`.

I hope you find something useful here.

## Files

- `init.lisp` – main file for loading the other ones (`~/.stumpwmrc` is a
  symlink to this file)
- `keys.lisp` – all keybindings (i use dvorak layout, so some of them
  may look weird)
- `utils.lisp` – additional functions and commands i use (many of them
  are prefixed with "al-", which is ugly I know)
- `visual.lisp` – visual settings: colors, mode-line and other visual
  appearance
- `settings.lisp` – some miscellaneous settings
- `layouts.lisp` – configuration for
  [clx-xkeyboard extension](https://github.com/filonenko-mikhail/clx-xkeyboard)
- `mana.lisp` – some commands for convenient playing
  [The Mana World](https://themanaworld.org/)

## Feedback

I am not an expert in Lisp, I just like StumpWM a lot, so if you find
errors or if you see that something can be implemented better, you may
send me a letter about that.
