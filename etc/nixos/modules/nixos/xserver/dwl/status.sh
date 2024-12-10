#!/usr/bin/env bash

while true; do
	upower -i /org/freedesktop/UPower/devices/DisplayDevice \
		| awk '/percentage/ { printf("🔋%0.0f%% | ", $2); }'

	date +"🕑 %T 📅 %F"

	sleep 1
done
