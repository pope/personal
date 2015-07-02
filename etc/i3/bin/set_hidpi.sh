#!/bin/bash

dpi=192
scale=2
if [[ $1 == "0" ]]; then
    dpi=96
    scale=1
fi

echo $dpi
echo $scale

gsettings set org.gnome.desktop.interface scaling-factor $scale
sed -i -e "s/Xft.dpi: [0-9]\+/Xft.dpi: $dpi/" ~/.Xresources
xrandr --dpi $dpi
sh $HOME/.fehbg
i3-msg -q "restart"
