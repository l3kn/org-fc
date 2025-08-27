#!/usr/bin/env bash
#
# Run Emacs under Xvfb to control window size
# and execute export script

OUTFILE="${1:-frame.svg}"
SIZE="${2:-600x400}"
LISPFILE="${3:-export.el}"

# Start Xvfb in the background
Xvfb :98 -screen 0 ${SIZE}x24 &
XVFB_PID=$!
export DISPLAY=:98

# Run emacs with your prepared init
emacs --quick --load "$LISPFILE"

# Kill Xvfb
kill $XVFB_PID
