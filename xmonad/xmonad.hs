import XMonad
import qualified XMonad.StackSet as W
import XMonad.Layout
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

import XMonad.Layout.Gaps
import XMonad.Layout.LayoutHints
import XMonad.Layout.Mosaic
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts

import XMonad.Prompt

import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Scratchpad
import System.IO
import Graphics.X11.ExtraTypes.XF86

-- NOTES: 0.10 works much better than 0.9, unfortunately distros mostly package 0.9 atm
-- xmobar and fullscreen flash vids (youtube): http://code.google.com/p/xmobar/issues/detail?id=41

-- TODO: would still like fullscreen flash vids to not crop and leave xmobar drawn
-- TODO: remove the red border when doing fullscreen? tried adding 'smartBorders' to the layoutHook but that didn't work
-- TODO: hook in TopicSpaces, start specific apps on specific workspaces

main = do
  xmproc <- spawnPipe "/usr/local/bin/xmobar ~/.xmonad/xmobarrc"
  xmonad $ def {
      modMask = mod4Mask
    , terminal = "urxvt"
--    , layoutHook = avoidStruts $ layoutHook def
    , layoutHook = myLayout
    , manageHook = myManageHook
    , workspaces = myWorkspaces
    , handleEventHook = docksEventHook <+> handleEventHook def
    , startupHook = docksStartupHook <+> startupHook def
    , logHook = dynamicLogWithPP $ myPP
                { ppOutput = hPutStrLn xmproc }
  } `additionalKeys` [ ((0, xF86XK_MonBrightnessUp), spawn "xbacklight +20")
                     , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -20")
                     , ((mod4Mask, xK_0), windows $ W.greedyView "messenger")
                     , ((mod4Mask, xK_Down), scratchpadSpawnActionTerminal "urxvt")
                     ]

-- |
myManageHook = composeOne [ isFullscreen -?> doFullFloat
                          , isDialog     -?> doCenterFloat ]
               <+> composeAll [ className =? "Gimp"        --> doFloat
                              , className =? "Gnome-panel" --> doIgnore
                              , className =? "Gtkdialog"   --> doFloat
                              ]
               <+> manageDocks

myWorkspaces = ["www", "emacs", "3", "4", "5", "6", "7", "8", "irc", "messenger"]

myLayout = avoidStruts $ toggleLayouts (noBorders Full)
  (smartBorders (tiled
                ||| Full
                ||| mosaic 2 [3,2]
                ||| spacing 5 (Mirror tiled)
                ||| layoutHints (tabbed shrinkText myTab)))
  where
    tiled   = layoutHints $ ResizableTall nmaster delta ratio []
    nmaster = 1
    delta   = 2/100
    ratio   = 1/2

-- * Themes

-- decoration theme
myDeco = def
    { activeColor         = "orange"
    , inactiveColor       = "#222222"
    , urgentColor         = "yellow"
    , activeBorderColor   = "orange"
    , inactiveBorderColor = "#222222"
    , urgentBorderColor   = "yellow"
    , activeTextColor     = "orange"
    , inactiveTextColor   = "#222222"
    , urgentTextColor     = "yellow"
    , decoHeight          = 10 }

-- tab theme
myTab = def
    { activeColor         = "black"
    , inactiveColor       = "black"
    , urgentColor         = "yellow"
    , activeBorderColor   = "orange"
    , inactiveBorderColor = "#222222"
    , urgentBorderColor   = "black"
    , activeTextColor     = "orange"
    , inactiveTextColor   = "#8e8e8e"
    , urgentTextColor     = "yellow" }

-- shell prompt theme
mySP = def
    { bgColor           = "black"
    , fgColor           = "white"
    , bgHLight          = "gray"
    , fgHLight          = "black"
    , borderColor       = "orange"
    , promptBorderWidth = 1
    , position          = Bottom
    , height            = 20
    --, autoComplete      = Just 1000
    , historySize       = 1000 }

myPP :: PP
myPP = def { ppCurrent = xmobarColor "yellow" "" . wrap "[" "]"
           , ppVisible = wrap "(" ")"
           , ppTitle   = const ""
           , ppUrgent  = xmobarColor "red" "yellow"
           , ppLayout  =
               (\ x -> case x of
                   "Hinted ResizableTall"                  -> "[|]"
                   "Spacing 5 Mirror Hinted ResizableTall" -> "[-]"
                   "Hinted Tabbed Simplest"                -> "[T]"
                   "Full"                                  -> "[ ]"
                   _                                       -> x )
           }
