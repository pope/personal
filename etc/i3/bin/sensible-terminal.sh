#!/bin/bash
# This script tries to exec a terminal emulator by trying some known terminal
# emulators.
for terminal in $TERMINAL urxvt i3-sensible-terminal; do
    if command -v $terminal > /dev/null 2>&1; then
        exec $terminal "$@"
    fi
done
