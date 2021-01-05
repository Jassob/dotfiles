{-
  My XMonad configuration.

  Here be monads, hapless wanderers beware!

TODO
---------
-}

import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           System.Exit
import           System.IO

{- XMonad core
-------------------------------------------------}
import           XMonad
import qualified XMonad.StackSet               as W

{- Actions
-------------------------------------------------}
import           XMonad.Actions.CopyWindow

{- Hooks
-------------------------------------------------}
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.Place
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.EwmhDesktops

{- Layout related stuff
-------------------------------------------------}
import           XMonad.Layout.BinarySpacePartition
import           XMonad.Layout.Gaps
import           XMonad.Layout.NoBorders
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Spacing
import           XMonad.Layout.Tabbed
import           XMonad.Layout.ToggleLayouts

{- Utils
-------------------------------------------------}
import           XMonad.Util.Run
import           XMonad.Util.NamedScratchpad

myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "alacritty"

myScratchpads :: [NamedScratchpad]
myScratchpads =
  [ NS "mupad" "spotify" (className =? "Spotify") doFullFloat
  , NS "termpad"
       (termArgs "termpad" "tmux new-session -A -s termpad")
       (title =? "termpad")
       doFullFloat
  , NS "empad"
       "~/.local/bin/startemacs -n server"
       (title =? "server")
       doCenterFloat
  , NS "chatpad"
       (termArgs "chatpad" "~/.local/bin/wchat")
       (title =? "chatpad")
       doFullFloat
  ]
  where termArgs tit cmd = unwords [myTerminal, "-t", tit, "-e", cmd]

-- | Stuff that will run every time XMonad is either started or restarted.
myStartupHook :: X ()
myStartupHook = do
  safeSpawn "pkill" ["trayer"]
  dir <- getXMonadDir
  safeSpawn "run" [dir ++ "/xmobar-trayer.sh"]
  setWMName "LG3D"
  docksStartupHook

myManageHook :: ManageHook
myManageHook =
  composeOne [isFullscreen -?> doFullFloat, isDialog -?> doCenterFloat]
    <+> composeAll
          [ className =? "Gimp" --> doFloat
          , className =? "Gnome-panel" --> doIgnore
          , className =? "Gtkdialog" --> doFloat
          , className =? "Pinentry" --> doFloat
          , className =? "gcr-prompter" --> doCenterFloat
          , className =? "Spotify" --> doCenterFloat
          , title =? "xmessage" --> doCenterFloat
          ]
    <+> namedScratchpadManageHook myScratchpads
    <+> manageDocks

myWorkspaces :: [String]
myWorkspaces = ws
 where
  ws :: [String]
  ws = zipWith makeLabel [1 .. 10] icons

  makeLabel :: Int -> Char -> String
  makeLabel index icon = show index ++ ": <fn=1>" ++ icon : "</fn> "

  icons :: String
  icons = "\xf269\xf120\xf121\xf02d\xf128\xf128\xf128\xf001\xf292\xf086"

myKeys :: XConfig Layout -> Map (KeyMask, KeySym) (X ())
myKeys XConfig { modMask = modm } =
  M.fromList
    $  workspaceKeybindings
    ++ screenWorkspaceKeybindings
    ++
  -- launching and killing programs
       [ ((modm .|. shiftMask, xK_Return), spawn myTerminal)
       , ((modm .|. shiftMask, xK_c), kill)
       , ((modm, xK_space), sendMessage NextLayout)
       , ( (modm, xK_n)
         , refresh
         )

  -- move focus up or down the window stack
       , ((modm, xK_Tab), windows W.focusDown)
       , ((modm .|. shiftMask, xK_Tab), windows W.focusUp)
       , ((modm, xK_j), windows W.focusDown)
       , ((modm, xK_k), windows W.focusUp)
       , ((modm, xK_m), windows W.focusMaster)
       , ( (modm, xK_w)
         , spawn "rofi -show window"
         )

  -- modifying the window order
       , ((modm, xK_Return), windows W.swapMaster)
       , ((modm .|. shiftMask, xK_j), windows W.swapDown)
       , ((modm .|. shiftMask, xK_k), windows W.swapUp)
       , ((modm .|. altMask, xK_l), sendMessage $ ExpandTowards R)
       , ((modm .|. altMask, xK_h), sendMessage $ ExpandTowards L)
       , ((modm .|. altMask, xK_j), sendMessage $ ExpandTowards D)
       , ((modm .|. altMask, xK_k), sendMessage $ ExpandTowards U)
       , ((modm .|. altMask, xK_r), sendMessage Rotate)
       , ((modm .|. altMask, xK_m), sendMessage Swap)
       , ((modm .|. altMask, xK_n), sendMessage FocusParent)
       , ((modm .|. ctrlMask, xK_n), sendMessage SelectNode)
       , ( (modm .|. shiftMask, xK_n)
         , sendMessage MoveNode
         )

  -- floating layer support
       , ((modm, xK_t), withFocused $ windows . W.sink)
       , ( (modm, xK_c)
         , placeFocused $ withGaps (16, 16, 16, 16) (smart (0.5, 0.5))
         )

  -- quit
       , ( (modm .|. shiftMask, xK_q)
         , io exitSuccess
         )

  -- restart
       , ( (modm, xK_q)
         , spawn
           "if type xmonad; then xmonad --recompile && xmonad --restart;\
            \else xmessage xmonad not in \\$PATH: \"$PATH\"; fi"
         )

  -- Run xmessage with a summary of the default keybindings (useful
  -- for beginners)
       , ( (modm .|. shiftMask, xK_plus)
         , spawn $ concat ["echo \"", help, "\" | xmessage -file -"]
         )

  -- Toggle fullscreen
       , ( (modm, xK_f)
         , sendMessage (Toggle "Full")
         )

    -- Hide or show named scratchpads
       , ((modm, xK_Down), namedScratchpadAction myScratchpads "termpad")
       , ((modm, xK_Up), namedScratchpadAction myScratchpads "mupad")
       , ((modm, xK_Right), namedScratchpadAction myScratchpads "empad")
       , ( (modm, xK_Left)
         , namedScratchpadAction myScratchpads "chatpad"
         )

    -- Copy current window to every workspace
       , ( (modm, xK_v)
         , windows copyToAll
         )

    -- Remove all other copies of this window
       , ((modm .|. shiftMask, xK_v), killAllOtherCopies)
       ]
 where
  ctrlMask = controlMask
  altMask  = mod1Mask

-- | Creates keybindings for workspaces.
workspaceKeybindings :: [((KeyMask, KeySym), X ())]
workspaceKeybindings = concatMap makeKeybinding pairs
 where
  pairs = zip myWorkspaces $ [xK_1 .. xK_9] ++ [xK_0]

  -- | Creates two keybindings for every workspace, one for
  -- switching to that workspace and one for moving the window
  -- to that workspace.
  makeKeybinding :: (String, KeySym) -> [((KeyMask, KeySym), X ())]
  makeKeybinding (ws, key) =
    [ ((myModMask, key)              , windows $ W.greedyView ws)
    , ((myModMask .|. shiftMask, key), windows $ W.shift ws)
    ]


-- mod-{a,s,d} %! Switch to physical/Xinerama screens 1, 2, or 3
-- mod-shift-{a,s,d} %! Move client to screen 1, 2, or 3
screenWorkspaceKeybindings :: [((KeyMask, KeySym), X ())]
screenWorkspaceKeybindings = concatMap makeKeybinding pairs
 where
  pairs = zip [0 ..] [xK_a, xK_s, xK_d]
  action sc f = screenWorkspace sc >>= flip whenJust (windows . f)

  -- | Creates two keybindings for every screenworkspace, one
  -- for switching to that workspace and one for moving the
  -- window to that workspace.
  makeKeybinding :: (ScreenId, KeySym) -> [((KeyMask, KeySym), X ())]
  makeKeybinding (sc, key) =
    [ ((myModMask, key)              , action sc W.view)
    , ((myModMask .|. shiftMask, key), action sc W.shift)
    ]


-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help = unlines
  [ "The mod button is Mod5 (or Super)'. My keybindings:"
  , ""
  , "-- launching and killing programs"
  , "mod-Shift-Enter         Launch termite"
  , "mod-Shift-c             Close/kill the focused window"
  , "mod-Space               Rotate through the available layout algorithms"
  , "mod-Shift-Space         Reset the layouts on the current workSpace"
  , "mod-down                Open terminal scratchpad"
  , "mod-up                  Open music scratchpad"
  , "mod-right               Open Emacs scratchpad"
  , "mod-left                Open Weechat scratchpad"
  , "mod-n                   Resize/refresh viewed windows to the correct size"
  , "mod-f                   Toggle fullscreen"
  , ""
  , "-- move focus up or down the window stack"
  , "mod-Tab                Move focus to the next window"
  , "mod-Shift-Tab          Move focus to the previous window"
  , "mod-j                  Move focus to the next window"
  , "mod-k                  Move focus to the previous window"
  , "mod-m                  Move focus to the master window"
  , ""
  , "-- modifying the window order"
  , "mod-Return             Swap the focused window and the master window"
  , "mod-Shift-j            Swap the focused window with the next window"
  , "mod-Shift-k            Swap the focused window with the previous window"
  , ""
  , "-- floating layer support"
  , "mod-t                  Push window back into tiling"
  , "mod-c                  Put floating window in center"
  , ""
  , "-- quit, or restart"
  , "mod-Shift-q            Quit xmonad"
  , "mod-q                  Restart xmonad"
  , ""
  , "-- Workspaces & screens"
  , "mod-[1..9]             Switch to workSpace N"
  , "mod-Shift-[1..9]       Move client to workspace N"
  , "mod-{a,s,d}            Switch to physical/Xinerama screens 1, 2, or 3"
  , "mod-Shift-{a,s,d}      Move client to screen 1, 2, or 3"
  , "mod-v                  Show current window on every workspace"
  , "mod-Shift-v            Remove all other copies of current window"
  , "mod-Alt-[l,h,j,k]      Expand window towards right, left, down and up"
  , "mod-Alt-r              Rotate the partition tree"
  , "mod-Alt-s              Swap two nodes in the partition tree"
  , "mod-Alt-n              Focus on the parent node in the partition tree"
  , "mod-Ctrl-n             Select focused node"
  , "mod-Shift-n            Move selected node"
  , ""
  , "-- Mouse bindings: default actions bound to mouse events"
  , "mod-button1            Float the window and move it by dragging"
  , "mod-button2            Raise the window to the top of the stack"
  , "mod-button3            Float the window and resize it by dragging"
  , ""
  , "-- Help"
  , "mod-Shift-plus         Show this help message."
  ]

-- toggleLayouts makes it possible for us to toggle the first layout
-- argument, while remembering the previous layout. Here we can toggle
-- full-screen.
myLayout =
  toggleLayouts (noBorders Full) . avoidStruts . smartBorders $ layouts
 where
  layouts =
    gaps [(U, 5), (R, 5), (D, 5), (L, 5)]
      $ (spacing 5 emptyBSP ||| tabs)
  tabs  = tabbed shrinkText $ def { fontName = "xft:Iosevka Nerd Font Mono:style=Regular" }

-- | Log configuration
myPP :: Handle -> PP
myPP h = def { ppCurrent = xmobarColor "#83a598" ""
             , ppVisible = wrap "(" ")"
             , ppTitle   = xmobarColor "#d3869b" "" . shorten 20
             , ppUrgent  = xmobarColor "red" "yellow"
             , ppLayout  = xmobarColor "#fabd2f" "" . formatLayout
             , ppHidden  = hideScratchpad
             , ppOutput  = hPutStrLn h
             }
 where
  formatLayout x = case x of
    "Tabbed Simplest" -> "[T]"
    "Spacing BSP"     -> "[+]"
    _                 -> x
  hideScratchpad ws | ws == "NSP" = mempty
                    | otherwise   = ws

-- | Wire it all up and start XMonad
main :: IO ()
main = do
  xmproc <- getXMonadDir >>= \dir -> spawnPipe $ "xmobar " ++ dir ++ "/xmobarrc"
  xmonad $ ewmh def { modMask            = myModMask
                    , terminal           = myTerminal
                    , layoutHook         = myLayout
                    , manageHook         = myManageHook
                    , workspaces         = myWorkspaces
                    , handleEventHook = docksEventHook <+> handleEventHook def
                    , startupHook        = myStartupHook
                    , logHook            = dynamicLogWithPP $ myPP xmproc
                    , keys               = myKeys
                    , normalBorderColor  = "#474646"
                    , focusedBorderColor = "#83a598"
                    }
