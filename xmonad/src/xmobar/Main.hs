import           Xmobar

config :: Config
config = defaultConfig
  { font        = "Iosevka Nerd Font Mono 16"
  , alpha       = 0x50
  , bgColor     = "black"
  , fgColor     = "#f8f8f8"
  , position    = TopSize L 95 29

  -- general behavior
  , allDesktops = True    -- show on all desktops
  , sepChar     = "%"
  , alignSep    = "}{"
  , template    =
    "%XMonadLog%\
      \ } \
      \<action=`$HOME/.local/bin/startemacs -e '(org-agenda-list)'` button=1>\
      \\61555 %date%\
      \</action>\
      \ { \
      \<action=`$HOME/.local/bin/toggle-dunst` button=1>\
      \%disturb%\
      \</action> \
      \<action=`$HOME/.local/bin/toggle-awake` button=1>\
      \| %awake% \
      \</action>\
      \<action=`$HOME/.local/bin/startemacs -e '(mu4e)'` button=1>\
      \| \61664 (personal: %personal%, work: %work%) \
      \</action>\
      \| %memory% \
      \<action=`pavucontrol` button=1>\
      \| %vol%\
      \</action> \
      \| %battery% "
  , commands    =
    [ Run XMonadLog
    , Run $ Date
      "<action=`$BROWSER https://calendar.google.com`>%b %d, %H:%M</action>"
      "date"
      10
    , Run $ Battery
      [ "--template"
      , "<acstatus> <left>"
      , "-L"
      , "10"
      , "-H"
      , "75"
      , "--"
      , "-P" -- Add percentage
      , "-L"
      , "25"
      , "-H"
      , "75"
      , "-h"
      , "green"
      , "-m"
      , "darkorange"
      , "-l"
      , "darkred"
      , "-i"
      , "\62840"
      , "-O"
      , "\62851"
      , "-o"
      , ""
      , "--highs"
      , "\62849"
      , "--mediums"
      , "\62845"
      , "--lows"
      , "\62850"
      , "-A"
      , "10"
      , "-a"
      , "notify-send -u critical 'Battery level low, connect charger!'"
      ]
      5
    , Run $ Mpris2
      "spotify"
      [ "-t"
      , "<action=`playerctl play-pause`><artist> - <title></action>"
      , "-M"
      , "20"
      , "-x"
      , ""
      ]
      10
    , Run $ ComX
      "sh"
      [ "-c"
      , "mu find flag:unread and maildir:/INBOX and not flag:trashed | wc -l"
      ]
      "N/A"
      "inbox"
      10
    , Run $ ComX
      "sh"
      ["-c", "mu find flag:flagged and not flag:trashed | uniq | wc -l"]
      "N/A"
      "starred"
      10
    , Run $ Com "sh" ["/home/jassob/.xmonad/xmobar-volume.sh"] "vol" 10
    , Run $ Com "sh" ["-c", "checkupdates | wc -l"] "checkupdates" 3600
    , Run $ Com "sh" ["/home/jassob/.xmonad/xmobar-disturb.sh"] "disturb" 10
    , Run $ Com "sh" ["/home/jassob/.xmonad/xmobar-awake.sh"] "awake" 10
    , Run $ Memory ["-t", "Mem: <usedratio>%"] 10
    ]
  }

main :: IO ()
main = xmobar config
