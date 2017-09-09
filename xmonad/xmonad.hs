{- XMonad core
--------------------------------------------------}
import XMonad
import qualified XMonad.StackSet as W

{- Actions
--------------------------------------------------}
import XMonad.Actions.CopyWindow (copyToAll, killAllOtherCopies)
import XMonad.Actions.Search (promptSearch, duckduckgo)

{- Hooks
--------------------------------------------------}
import XMonad.Hooks.DynamicLog (PP(..), dynamicLogWithPP, xmobarColor, wrap)
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook
                                , docksStartupHook, manageDocks)
import XMonad.Hooks.ManageHelpers (composeOne, isFullscreen, isDialog
                                  , doFullFloat, doCenterFloat, (-?>))

{- Layout related stuff
--------------------------------------------------}
import XMonad.Layout.LayoutHints (layoutHints)
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.ResizableTile (ResizableTall(..))
import XMonad.Layout.Spacing (spacing)
import XMonad.Layout.Tabbed (Theme(..), tabbed, shrinkText)
import XMonad.Layout.ToggleLayouts (toggleLayouts)

{- Prompt
--------------------------------------------------}
import XMonad.Prompt (XPConfig(..))
import XMonad.Prompt.AppLauncher (launchApp)

{- Utils
---------------------------------------------------}
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Scratchpad (scratchpadSpawnActionTerminal)


import System.IO (Handle, hPutStrLn)
import System.Environment (setEnv)
import Graphics.X11.ExtraTypes.XF86 (xF86XK_MonBrightnessUp
                                    , xF86XK_MonBrightnessDown)

main :: IO ()
main = do
  xmproc <- spawnPipe "/usr/local/bin/xmobar ~/.xmonad/xmobarrc"
  setEnv "BROWSER" "conkeror"
  xmonad $ def
    { modMask = mod4Mask
    , terminal = "termite"
    , layoutHook = myLayout
    , manageHook = myManageHook
    , workspaces = myWorkspaces
    , handleEventHook = docksEventHook <+> handleEventHook def
    , startupHook = docksStartupHook <+> startupHook def
    , logHook = myLogHook xmproc
  } `additionalKeys` myAdditionalKeys

myLogHook :: Handle -> X ()
myLogHook h = do
  let check ws | ws == "NSP" = mempty
               | otherwise = ws
  dynamicLogWithPP myPP {ppHidden = check, ppOutput = hPutStrLn h}

myManageHook :: ManageHook
myManageHook = composeOne [ isFullscreen -?> doFullFloat
                          , isDialog     -?> doCenterFloat ]
               <+> composeAll [ className =? "Gimp"        --> doFloat
                              , className =? "Gnome-panel" --> doIgnore
                              , className =? "Gtkdialog"   --> doFloat
                              ]
               <+> manageDocks

-- | Workspaces and their names.
-- Super-{1-9} and Super-Shift-{1-9} is used to jump to the workspace
-- with the given index(+1) in the list.
myWorkspaces :: [String]
myWorkspaces = [ "www"
               , "emacs"
               , "3"
               , "4"
               , "5"
               , "6"
               , "7"
               , "audio"
               , "irc"
               , "messenger"
               ]

-- | List of keybindings and actions to take when they are pressed
-- Defines additionally Super-0 and Super-Shift-0 for
-- the "messenger" workspace
myAdditionalKeys :: [((KeyMask, KeySym), X ())]
myAdditionalKeys =
  [ ((0, xF86XK_MonBrightnessUp), spawn "xbacklight +20")
  , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -20")
  , ((mod4Mask, xK_0), windows $ W.greedyView "messenger")
  , ((mod4Mask .|. shiftMask, xK_0), windows $ W.shift "messenger")
  , ((mod4Mask, xK_Down), scratchpadSpawnActionTerminal "termite")
  , ((mod4Mask, xK_v ), windows copyToAll)
  , ((mod4Mask .|. shiftMask, xK_v ),  killAllOtherCopies)
  , ((mod4Mask, xK_g), promptSearch mySP{historySize=0} duckduckgo)
  , ((mod4Mask, xK_e), spawn "/home/jassob/.local/bin/startemacs")
  , ((mod4Mask, xK_b), launchApp mySP "conkeror")
  ]

myLayout = avoidStruts $ toggleLayouts (noBorders Full)
  (smartBorders (tiled
                 ||| Full
                 ||| spacing 5 (Mirror tiled)
                 ||| layoutHints (tabbed shrinkText myTab)))
  where
    tiled   = layoutHints $ ResizableTall nmaster delta ratio []
    nmaster = 1
    delta   = 2/100
    ratio   = 1/2

-- * Themes

-- tab theme
myTab :: Theme
myTab = def { fontName   = "xft:inconsolata:size=12"
            , decoHeight = 30 }

-- shell prompt theme
mySP :: XPConfig
mySP = def { font   = "xft:inconsolata:size=12"
           , height = 40 }

myPP :: PP
myPP = def { ppCurrent = xmobarColor "yellow" "" . wrap "[" "]"
           , ppVisible = wrap "(" ")"
           , ppTitle   = const ""
           , ppUrgent  = xmobarColor "red" "yellow"
           , ppLayout  = formatLayout
           }
  where formatLayout x = case x of
          "Hinted ResizableTall"                  -> "[|]"
          "Spacing 5 Mirror Hinted ResizableTall" -> "[-]"
          "Hinted Tabbed Simplest"                -> "[T]"
          "Full"                                  -> "[ ]"
          _                                       -> x
