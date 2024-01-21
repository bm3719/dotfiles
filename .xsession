# Load .Xresources, if it exists.
resources=$HOME/.Xresources
if [ -f "$resources" ]; then
    /usr/X11R6/bin/xrdb -load "$resources"
fi

# Start Simple X Hotkey Daemon.
# Commented out since bspwmrc will launch this.
#sxkhd &

# Start oxbar
/home/bm3719/src/c/oxbar/oxbar bcm-bottom&

# Use feh to set background.
feh --bg-tile $HOME/tmp/pure_black.jpg

# Load custom .Xmodmap.
xmodmap ~/.Xmodmap

# Launch WM.
bspwm
