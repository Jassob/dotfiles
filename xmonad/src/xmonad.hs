{- XMonad core
--------------------------------------------------}
import XMonad
import qualified XMonad.StackSet as W

{- Actions
--------------------------------------------------}
import XMonad.Actions.CopyWindow (copyToAll, killAllOtherCopies)

{- Hooks
--------------------------------------------------}
import XMonad.Hooks.DynamicLog (PP(..), dynamicLogWithPP, xmobarColor, wrap)
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook
                                , docksStartupHook, manageDocks)
import XMonad.Hooks.ManageHelpers (composeOne, isFullscreen, isDialog
                                  , doFullFloat, doCenterFloat, (-?>))
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Hooks.EwmhDesktops (ewmh)

{- Layout related stuff
--------------------------------------------------}
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.ResizableTile (ResizableTall(..))
import XMonad.Layout.Spacing (spacing)
import XMonad.Layout.Tabbed (simpleTabbed)
import XMonad.Layout.ToggleLayouts (ToggleLayout(Toggle), toggleLayouts)

{- Utils
---------------------------------------------------}
import XMonad.Util.Cursor (setDefaultCursor)
import XMonad.Util.Run (spawnPipe, safeSpawn, runProcessWithInput)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.NamedScratchpad (NamedScratchpad(..), defaultFloating, namedScratchpadAction)

import Data.Map (Map, fromList, toList)
import System.Environment (setEnv)
import System.Exit (exitSuccess)
import System.IO (Handle, hPutStrLn)

myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "termite"

myScratchpads :: [NamedScratchpad]
myScratchpads =
  [ NS "ncmpcpp" "termite -e ncmpcpp -t mopidy" (title =? "mopidy") defaultFloating
  , NS "termite" "termite -t scratchpad"        (title =? "scratchpad") defaultFloating
  ]

-- | Stuff that will run every time XMonad is either started or restarted.
myStartupHook :: X ()
myStartupHook = safeSpawn "sxhkd" []
                <+> setDefaultCursor xC_left_ptr
                <+> setWMName "LG3D"
                <+> safeSpawn "feh" ["--bg-scale", "~/.background-image"]
                <+> docksStartupHook

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
myWorkspaces = zipWith (curry makeClickable) ([1..9] ++ [0]) ws
  where -- Creates a clickable action that will jump to the workspace
        makeClickable :: (Int, String) -> String
        makeClickable (idx, wsn) = "<action=xdotool key super+" ++ show idx
                                   ++ " button=1>" ++ wsn ++ "</action>"

        ws :: [String]
        ws = zipWith makeLabel [1..10] icons

        makeLabel :: Int -> Char -> String
        makeLabel index icon = show index ++ ": <fn=1>" ++ icon : "</fn> "

        icons :: String
        icons = "\xf269\xf120\xf121\xf02d\xf128\xf128\xf128\xf001\xf292\xf0e6"

myKeys :: XConfig Layout -> Map (KeyMask, KeySym) (X ())
myKeys conf@XConfig {XMonad.modMask = modMask} = fromList $
  workspaceKeybindings ++ screenWorkspaceKeybindings ++
  -- launching and killing programs
  [ ((modMask .|. shiftMask, xK_Return), spawn myTerminal)
  , ((modMask .|. shiftMask, xK_c     ), kill)

  , ((modMask,               xK_space ), sendMessage NextLayout)
  , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
  , ((modMask,               xK_n     ), refresh)

  -- move focus up or down the window stack
  , ((modMask,               xK_Tab   ), windows W.focusDown)
  , ((modMask .|. shiftMask, xK_Tab   ), windows W.focusUp  )
  , ((modMask,               xK_j     ), windows W.focusDown)
  , ((modMask,               xK_k     ), windows W.focusUp  )
  , ((modMask,               xK_m     ), windows W.focusMaster)

  -- modifying the window order
  , ((modMask,               xK_Return), windows W.swapMaster)
  , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )
  , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )

  -- resizing the master/slave ratio
  , ((modMask,               xK_h     ), sendMessage Shrink)
  , ((modMask,               xK_l     ), sendMessage Expand)

  -- floating layer support
  , ((modMask,               xK_t     ), withFocused $ windows . W.sink)

  -- increase or decrease number of windows in the master area
  , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))
  , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))

  -- quit
  , ((modMask .|. shiftMask, xK_q     ), io exitSuccess)

  -- restart
  , ((modMask              , xK_q     ), spawn "if type xmonad; \
                                                 \then xmonad --recompile && xmonad --restart; \
                                                 \else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")

  -- Run xmessage with a summary of the default keybindings (useful
  -- for beginners)
  , ((modMask .|. shiftMask, xK_plus), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))

  -- Toggle fullscreen
  , ((modMask              , xK_f), sendMessage (Toggle "Full"))

    -- Hide or show a terminal
  , ((modMask              , xK_Down), namedScratchpadAction myScratchpads "termite")

    -- Hide or show a cli mpd client
  , ((modMask              , xK_Up), namedScratchpadAction myScratchpads "ncmpcpp")

    -- Copy current window to every workspace
  , ((modMask              , xK_v ), windows copyToAll)

    -- Remove all other copies of this window
  , ((modMask .|. shiftMask, xK_v ), killAllOtherCopies)
  ]

-- | Creates keybindings for workspaces.
workspaceKeybindings :: [((KeyMask, KeySym), X ())]
workspaceKeybindings = concatMap makeKeybinding pairs
  where pairs = zip myWorkspaces $ [xK_1 .. xK_9] ++ [xK_0]

        -- | Creates two keybindings for every workspace, one for
        -- switching to that workspace and one for moving the window
        -- to that workspace.
        makeKeybinding :: (String, KeySym) -> [((KeyMask, KeySym), X ())]
        makeKeybinding (ws, key) =
          [ ((myModMask              , key), windows $ W.greedyView ws)
          , ((myModMask .|. shiftMask, key), windows $ W.shift ws)]


-- mod-{a,s,d} %! Switch to physical/Xinerama screens 1, 2, or 3
-- mod-shift-{a,s,d} %! Move client to screen 1, 2, or 3
screenWorkspaceKeybindings :: [((KeyMask, KeySym), X ())]
screenWorkspaceKeybindings = concatMap makeKeybinding pairs
  where pairs = zip [0..] [xK_a, xK_s, xK_d]
        action sc f = screenWorkspace sc >>= flip whenJust (windows . f)

        -- | Creates two keybindings for every screenworkspace, one
        -- for switching to that workspace and one for moving the
        -- window to that workspace.
        makeKeybinding :: (ScreenId, KeySym) -> [((KeyMask, KeySym), X ())]
        makeKeybinding (sc, key) =
          [ ((myModMask              , key), action sc W.view)
          , ((myModMask .|. shiftMask, key), action sc W.shift)]


-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help = unlines
  [ "The mod button is Mod5 (or Super)'. My keybindings:"
  , ""
  , "-- launching and killing programs"
  , "mod-Shift-Enter  Launch termite"
  , "mod-Shift-c      Close/kill the focused window"
  , "mod-Space        Rotate through the available layout algorithms"
  , "mod-Shift-Space  Reset the layouts on the current workSpace to default"
  , "mod-down         Open termite scratchpad"
  , "mod-up           Open ncmpcpp scratchpad"
  , "mod-n            Resize/refresh viewed windows to the correct size"
  , "mod-f            Toggle fullscreen"
  , ""
  , "-- move focus up or down the window stack"
  , "mod-Tab        Move focus to the next window"
  , "mod-Shift-Tab  Move focus to the previous window"
  , "mod-j          Move focus to the next window"
  , "mod-k          Move focus to the previous window"
  , "mod-m          Move focus to the master window"
  , ""
  , "-- modifying the window order"
  , "mod-Return   Swap the focused window and the master window"
  , "mod-Shift-j  Swap the focused window with the next window"
  , "mod-Shift-k  Swap the focused window with the previous window"
  , ""
  , "-- resizing the master/slave ratio"
  , "mod-h  Shrink the master area"
  , "mod-l  Expand the master area"
  , ""
  , "-- floating layer support"
  , "mod-t  Push window back into tiling; unfloat and re-tile it"
  , ""
  , "-- increase or decrease number of windows in the master area"
  , "mod-comma  (mod-,)   Increment the number of windows in the master area"
  , "mod-period (mod-.)   Deincrement the number of windows in the master area"
  , ""
  , "-- quit, or restart"
  , "mod-Shift-q  Quit xmonad"
  , "mod-q        Restart xmonad"
  , ""
  , "-- Workspaces & screens"
  , "mod-[1..9]         Switch to workSpace N"
  , "mod-Shift-[1..9]   Move client to workspace N"
  , "mod-{a,s,d}        Switch to physical/Xinerama screens 1, 2, or 3"
  , "mod-Shift-{a,s,d}  Move client to screen 1, 2, or 3"
  , "mod-v              Show current window on every workspace"
  , "mod-Shift-v        Remove all other copies of current window"
  , ""
  , "-- Mouse bindings: default actions bound to mouse events"
  , "mod-button1  Set the window to floating mode and move by dragging"
  , "mod-button2  Raise the window to the top of the stack"
  , "mod-button3  Set the window to floating mode and resize by dragging"
  , ""
  , "-- Help"
  , "mod-Shift-plus    Show this help message."
  , ""
  ]

-- toggleLayouts makes it possible for us to toggle the first layout
-- argument, while remembering the previous layout. Here we can toggle
-- full-screen.
myLayout = toggleLayouts (noBorders Full) $ borders $
           spacedTiled ||| Mirror spacedTiled ||| simpleTabbed
  where
    borders = avoidStruts . smartBorders
    spacedTiled = spacing 5 $ ResizableTall 1 (2/100) (1/2) []

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
          "Spacing 5 ResizableTall"        -> "[|]"
          "Spacing 5 Mirror ResizableTall" -> "[-]"
          "Tabbed Simplest"                -> "[T]"
          "Full"                           -> "[ ]"
          _                                -> x

        hideScratchpad ws | ws == "NSP" = mempty
                          | otherwise = ws

-- | Wire it all up and start XMonad
main :: IO ()
main = do
  xmproc <- getXMonadDir >>= \ dir -> spawnPipe $ "xmobar " ++ dir ++ "/xmobarrc"
  xmonad $ ewmh def
    { modMask = myModMask
    , terminal = myTerminal
    , layoutHook = myLayout
    , manageHook = myManageHook
    , workspaces = myWorkspaces
    , handleEventHook = docksEventHook <+> handleEventHook def
    , startupHook = myStartupHook
    , logHook = dynamicLogWithPP $ myPP xmproc
    , keys = myKeys
    }
