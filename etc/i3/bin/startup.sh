#!/bin/bash

if [[ -f ~/.Xresources ]]; then
  xrdb -load ~/.Xresources
fi

# Handle being run inside VMWare VM
if command -v vmware-user > /dev/null 2>&1; then
  vmware-user
fi

if command -v compton > /dev/null 2>&1; then
  compton -CGb
fi

if command -v gnome-settings-daemon > /dev/null 2>&1; then
  gnome-settings-daemon &
fi

if [[ -f ~/.fehbg ]] ; then
  sh ~/.fehbg
fi
