#!/bin/bash

if [[ -f ~/.Xresources ]]; then
  xrdb -load ~/.Xresources
fi

# Handle being run inside VMWare VM
if command -v vmware-user > /dev/null 2>&1; then
  vmware-user
fi

if command -v compton > /dev/null 2>&1; then
  compton -CGb --vsync opengl
fi

if command -v gnome-settings-daemon > /dev/null 2>&1; then
  gnome-settings-daemon &
fi
#cinnamon-settings-daemon &

if command -v gnome-keyring-daemon > /dev/null 2>&1; then
  gnome-keyring-daemon
fi

if [[ -f /usr/lib/policykit-1-gnome/polkit-gnome-authentication-agent-1 ]]; then
  /usr/lib/policykit-1-gnome/polkit-gnome-authentication-agent-1 &
fi

if command -v nm-applet > /dev/null 2>&1; then
  nm-applet &
fi

if [[ -f ~/.fehbg ]] ; then
  sh ~/.fehbg
fi

