# Load .Xresources, if it exists.
resources=$HOME/.Xresources
if [ -f "$resources" ]; then
    /usr/X11R6/bin/xrdb -load "$resources"
fi

xset dpms 600 1800 3200

# Start Simple X Hotkey Daemon.
# Commented out since bspwmrc will launch this.
#sxkhd &

# Start oxbar
/home/bm3719/src/c/oxbar/oxbar -t "%Y-%m-%d %H:%M:%S" bcm-bottom &

# Use feh to set background.
feh --bg-tile $HOME/tmp/pure_black.jpg

# Load custom .Xmodmap.
xmodmap ~/.Xmodmap

# Launch WM.
bspwm
