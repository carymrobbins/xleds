# xleds

Controls the brightness of your keyboard backlight.
Usage is nearly identical to [xbacklight](https://github.com/tcatm/xbacklight).

```
Usage: xleds [options]

Options:
  -get
  -set <percentage> or = <percentage>
  -inc <percentage> or + <percentage>
  -dec <percentage> or - <percentage>
```

If you are using `xbindkeys`, you can use something like the following in your `~/.xbindkeysrc` -

```
"xleds -dec 20"
  XF86KbdBrightnessDown

"xleds -inc 20"
  XF86KbdBrightnessUp
```

You can install this using `make install`, which mostly delegates to stack.
Note that installing requires `sudo` so it can change the owner of the compiled
binary to root and set the suid bit.
