#!/usr/bin/env bash

while true; do
	playerctl --all-players metadata --format '{{status}}{{artist}} - {{title}}' 2>/dev/null \
		| sed -e 's/^Playing/▶️ /' -e 's/^Paused/⏸️ /' -e 's/^Stop/⏹️ /' \
		| awk '/^.+$/ { printf("%s | ", $0); }'

	pamixer --get-volume-human \
		| awk '
			/^(1[0-9][0-9]|[987][0-9])%$/ { printf("🔊%s", $0); }
			/^[456][0-9]%$/ { printf("🔉%s", $0); }
			/^[123]?[0-9]%$/ { printf("🔈%s", $0); }
			/mute/ { printf("🔇"); }'

	echo -n " | "

	# df | awk '$6 == "/" { printf("💾%s ", $5); }'
	# free -m | awk '/Mem:/ { printf("🐏%0.0f%% | ", $3 / $2 * 100); }'


	upower -i /org/freedesktop/UPower/devices/DisplayDevice \
		| awk '/percentage/ { printf("🔋%0.0f%% | ", $2); }'

	date +"🕑%H:%M 📅%F"

	sleep 1
done
