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
import XMonad.Hooks.SetWMName (setWMName)

{- Layout related stuff
--------------------------------------------------}
import XMonad.Layout.LayoutHints (layoutHints)
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.ResizableTile (ResizableTall(..))
import XMonad.Layout.Spacing (spacing)
import XMonad.Layout.Tabbed (Theme(..), tabbed, shrinkText)
import XMonad.Layout.ToggleLayouts (ToggleLayout(Toggle), toggleLayouts)

{- Prompt
--------------------------------------------------}
import XMonad.Prompt (XPConfig(..))
import XMonad.Prompt.AppLauncher (launchApp)
import XMonad.Prompt.Pass (passPrompt, passGeneratePrompt)

{- Utils
---------------------------------------------------}
import XMonad.Util.Cursor (setDefaultCursor)
import XMonad.Util.Run (spawnPipe, safeSpawn)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.NamedScratchpad (NamedScratchpad(..), defaultFloating, namedScratchpadAction)

import System.IO (Handle, hPutStrLn)
import System.Environment (setEnv)
import Graphics.X11.ExtraTypes.XF86 (xF86XK_MonBrightnessUp
                                    , xF86XK_MonBrightnessDown)

myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "termite"

myScratchpads :: [NamedScratchpad]
myScratchpads =
  [ NS "ncmpcpp" "termite -e ncmpcpp -t mopidy" (title =? "mopidy") defaultFloating
  , NS "termite" "termite -t scratchpad"        (title =? "scratchpad") defaultFloating
  , NS "firefox" "firefox"                      (className =? "Firefox") defaultFloating
  ]

-- | Stuff that will run every time XMonad is either started or restarted.
myStartupHook :: X ()
myStartupHook = -- safeSpawn "compton" comptonArgs
                safeSpawn "sxhkd" []
                <+> setDefaultCursor xC_left_ptr
                <+> setWMName "LG3D"
                <+> spawn "$HOME/.fehbg"
                <+> docksStartupHook
  where comptonArgs = [ "--fading", "--fade-delta", "5", "--fade-out-step", "0.08"
                      , "--fade-in-step", "0.08", "--shadow", "--shadow-opacity", "0.5"
                      , "--no-dock-shadow", "--clear-shadow", "--inactive-opacity", "0.9"
                      ]

myManageHook :: ManageHook
myManageHook = composeOne [ isFullscreen -?> doFullFloat
                          , isDialog     -?> doCenterFloat ]
               <+> composeAll [ className =? "Gimp"        --> doFloat
                              , className =? "Gnome-panel" --> doIgnore
                              , className =? "Gtkdialog"   --> doFloat
                              , className =? "Pinentry"    --> doFloat
                              ]
               <+> manageDocks

myWorkspaces :: [String]
myWorkspaces = map makeClickable $ zip ([1..9] ++ [0]) ws
  where -- Creates a clickable action that will jump to the workspace
        makeClickable :: (Int, String) -> String
        makeClickable (idx, wsn) = "<action=xdotool key super+" ++ show idx
                                   ++ " button=1>" ++ wsn ++ "</action>"

        ws :: [String]
        ws = zipWith makeLabel [1..10] icons

        makeLabel :: Int -> Char -> String
        makeLabel index icon = show index ++ ": <fn=1>" ++ icon : "</fn> "

        icons :: [Char]
        icons = [ '\xf269', '\xf120', '\xf121', '\xf02d', '\xf128',
                  '\xf128', '\xf128', '\xf001', '\xf292', '\xf0e6' ]

myAdditionalKeys :: [((KeyMask, KeySym), X ())]
myAdditionalKeys = workspaceKeybindings ++
  -- Change screen brightness
  [ ((0, xF86XK_MonBrightnessUp), safeSpawn "xbacklight" ["+20"])
  , ((0, xF86XK_MonBrightnessDown), safeSpawn "xbacklight" ["-20"])

  -- Toggle fullscreen
  , ((myModMask, xK_f),               sendMessage (Toggle "Full"))

  -- Hide or show a terminal
  , ((myModMask, xK_Down),            namedScratchpadAction myScratchpads "termite")

  -- Hide or show firefox
  , ((myModMask, xK_w),               namedScratchpadAction myScratchpads "firefox")

  -- Hide or show a cli mpd client
  , ((myModMask, xK_Up),              namedScratchpadAction myScratchpads "ncmpcpp")

  -- Copy current window to every workspace
  , ((myModMask, xK_v ),              windows copyToAll)

  -- Remove all other copies of this window
  , ((myModMask .|. shiftMask, xK_v ), killAllOtherCopies)

  -- Start emacs
  , ((myModMask, xK_e),               safeSpawn "/home/jassob/.local/bin/startemacs" [])

  -- Search with $BROWSER
  , ((myModMask, xK_g),               promptSearch mySP{historySize=0} duckduckgo)

  -- Open url with Conkeror
  , ((myModMask, xK_b),               launchApp mySP "conkeror")

  -- Prompt for password from password-store
  , ((myModMask, xK_p),               passPrompt mySP)

  -- Run dmenu
  , ((myModMask, xK_d),               safeSpawn "dmenu_run" [])

  -- Prompt for a name to store a password that will be generated
  , ((myModMask .|. shiftMask, xK_p), passGeneratePrompt mySP)
  ]


-- | Creates keybindings for workspaces.
workspaceKeybindings :: [((KeyMask, KeySym), X ())]
workspaceKeybindings = concat . map makeKeybinding $ pairs
  where pairs = zip myWorkspaces $ [xK_1 .. xK_9] ++ [xK_0]

        -- | Creates two keybindings for every workspace, one for
        -- switching to that workspace and one for moving the window
        -- to that workspace.
        makeKeybinding :: (String, KeySym) -> [((KeyMask, KeySym), X ())]
        makeKeybinding (ws, key) =
          [ ((myModMask, key), windows $ W.greedyView ws)
          , ((shiftMask .|. myModMask, key), windows $ W.shift ws)]

-- toggleLayouts makes it possible for us to toggle the first layout
-- argument, while remembering the previous layout. Here we can toggle full-screen.
myLayout = toggleLayouts (noBorders Full) $ spacedWithBorders $
           tiled ||| (Mirror tiled) ||| layoutHints (tabbed shrinkText myTab)
  where
    spacedWithBorders = avoidStruts . smartBorders . spacing 5
    tiled   = layoutHints $ ResizableTall 1 (2/100) (1/2) []

-- * Themes

-- shell prompt theme
mySP :: XPConfig
mySP = def { font   = "xft:inconsolata:size=12"
           , height = 40 }

myTab :: Theme
myTab = def { fontName = "xft:inconsolata:size=12"
            , decoHeight = 46
            }

-- | Log configuration
myPP :: Handle -> PP
myPP h = def
  { ppCurrent = xmobarColor "orange" ""
  , ppVisible = wrap "(" ")"
  , ppTitle   = const ""
  , ppUrgent  = xmobarColor "red" "yellow"
  , ppLayout  = formatLayout
  , ppHidden  = hideScratchpad
  , ppOutput  = hPutStrLn h
  }
  where formatLayout x = case x of
          "Spacing 5 Hinted ResizableTall"        -> "[|]"
          "Spacing 5 Mirror Hinted ResizableTall" -> "[-]"
          "Spacing 5 Hinted Tabbed Simplest"      -> "[T]"
          "Full"                                  -> "[ ]"
          _                                       -> x

        hideScratchpad ws | ws == "NSP" = mempty
                          | otherwise = ws

-- | Wire it all up and start XMonad
main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
  setEnv "BROWSER" "conkeror"
  xmonad $ def { modMask = myModMask
              , terminal = myTerminal
              , layoutHook = myLayout
              , manageHook = myManageHook
              , workspaces = myWorkspaces
              , handleEventHook = docksEventHook <+> handleEventHook def
              , startupHook = myStartupHook
              , logHook = dynamicLogWithPP $ myPP xmproc
              } `additionalKeys` myAdditionalKeys
