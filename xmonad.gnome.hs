-- xmonad.hs - bm3719
--
-- Time-stamp: <2012-05-03 13:49:30 (bm3719)>
-- DESC: A version of xmonad.hs for Gnome integration.

import XMonad
import XMonad.Config.Gnome
import XMonad.Layout.NoBorders

myManageHook = composeAll [
  (className =? "Pidgin" <&&> title =? "Buddy List")           --> doFloat,
  (className =? "Gnome-panel" <&&> title =? "Run Application") --> doFloat,
  className =? "MPlayer"                                       --> doFloat,
  className =? "Gimp"                                          --> doFloat,
  resource  =? "desktop_window"                                --> doIgnore,
  resource  =? "kdesktop"                                      --> doIgnore ]

main = xmonad $ gnomeConfig {
  terminal           = "urxvtcd -sl 10000 -ls",
  focusFollowsMouse  = True,
  borderWidth        = 1,
  modMask            = mod4Mask,
  workspaces         = ["1", "2", "3", "4", "5"],
  normalBorderColor  = "#404040",
  focusedBorderColor = "#A9A9F5",
  manageHook         = myManageHook <+> manageHook gnomeConfig }
