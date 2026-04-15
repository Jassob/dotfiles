# Changing the theme

These scripts are used to set the theme and activate/deactivate dark mode.

## Prerequisites

In order to make certain applications that rely on xdg-desktop-portal
(e.g. Google Chrome) adhere to color scheme `xdg-desktop-portal` needs
to be installed and configured:

```bash
$ sudo apt install xdg-desktop-portal xdg-desktop-portal-gtk
$ mkdir ~/.config/xdg-desktop-portal
$ cat > ~/.config/xdg-desktop-portal/portals.conf <<EOF
[preferred]
default=gtk
org.freedesktop.impl.portal.Settings=gtk
EOF
$ systemctl --user restart xdg-desktop-portal
```

Any misbehaving application needs to be restarted to pick up DBUS
messages from xdg-desktop-portal.
