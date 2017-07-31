# xleds

Controls the brightness of your keyboard backlight.
Usage is nearly identical to [xbacklight](https://github.com/tcatm/xbacklight).

```
Usage: xleds [key|screen] [command]

Commands:
  -get
  -set <amount|percentage%> or = <amount|percentage%>
  -inc <amount|percentage%> or + <amount|percentage%>
  -dec <amount|percentage%> or - <amount|percentage%>
```

If you are using `xbindkeys`, you can use something like the following in your `~/.xbindkeysrc` -

```
"xleds screen - 10%"
  XF86MonBrightnessDown

"xleds screen + 10%"
  XF86MonBrightnessUp

"xleds screen - 1%"
  Shift + XF86MonBrightnessDown

"xleds screen + 1%"
  Shift + XF86MonBrightnessUp

"xleds key - 10"
  XF86KbdBrightnessDown

"xleds key + 10"
  XF86KbdBrightnessUp

"xleds key - 1"
  Shift + XF86KbdBrightnessDown

"xleds key + 1"
  Shift + XF86KbdBrightnessUp
```

You can install this using `make install`, which mostly delegates to stack.
Note that installing requires `sudo` so it can change the owner of the compiled
binary to root and set the suid bit.
