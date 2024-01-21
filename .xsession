# Load .Xresources, if it exists.
resources=$HOME/.Xresources
if [ -f "$resources" ]; then
    /usr/X11R6/bin/xrdb -load "$resources"
fi

# Start Simple X Hotkey Daemon.
# Commented out since bspwmrc will launch this.
#sxkhd &

# TODO: Start polybar.

# TODO: Use feh to set background

bspwm
