#!/bin/bash

xrandr --dpi 192
xrdb -load ~/.Xresources

# diable the beep
xset -b

# clipboard management
autocutsel -fork &
autocutsel -selection PRIMARY -fork &

# i3
exec dbus-launch --exit-with-session i3
