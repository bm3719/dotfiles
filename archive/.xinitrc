xrdb -merge ~/.Xdefaults

xset b 100 0 0
#xset r rate 140 200

#xset fp+ /usr/local/share/fonts/terminus-font
xset fp+ /usr/local/share/fonts/code2000
#xset fp+ /usr/local/share/fonts/bitstream-vera
xset fp+ /usr/local/share/fonts/OTF
xset fp+ /usr/local/share/fonts/dejavu

# Install this if I have cutbuffer apps.
autocutsel &
autocutsel -s PRIMARY &

#xmodmap -e "keycode 233 = Page_Down"
#xmodmap -e "keycode 234 = Page_Up"
xmodmap -e "remove Lock = Caps_Lock"
xmodmap -e "keysym Caps_Lock = Control_L"
xmodmap -e "add Control = Control_L"
xmodmap -e "remove mod1 = Alt_R"
xmodmap -e "add mod3 = Alt_R"
xmodmap -e "keysym Alt_L = Meta_L Alt_L"

#/usr/local/bin/xmonad &
$HOME/.xmonad/xmonad &

# Wait for xmonad
wait $!
