{ stdenv, fetchurl, makeDesktopItem
, jre, libX11, libXext, libXcursor, libXrandr, libXxf86vm
, openjdk
, mesa, openal
, useAlsa ? false, alsaOss ? null }:
with stdenv.lib;

assert useAlsa -> alsaOss != null;

let
  desktopItem = makeDesktopItem {
    name = "feed-the-beast";
    exec = "ftb";
    icon = "feed-the-beast";
    comment = "Heavily modded Minecraft with tech mods";
    desktopName = "Minecraft";
    genericName = "minecraft";
    categories = "Game;";
  };

in stdenv.mkDerivation {
  name = "feed-the-beast";

  src = fetchurl {
    url = "http://ftb.cursecdn.com/FTB2/launcher/FTB_Launcher.jar";
    sha256 = "3b1d1a89cfda1dcb2fbebdae91d1eef63a7b1d50340b99896f456ae79f24ea81";
  };

  phases = "installPhase";

  installPhase = ''
    set -x
    mkdir -pv $out/bin
    cp -v $src $out/FTB_Launcher.jar

    cat > $out/bin/ftb << EOF
    #!${stdenv.shell}

    export LD_LIBRARY_PATH=\$LD_LIBRARY_PATH:${makeLibraryPath [ libX11 libXext libXcursor libXrandr libXxf86vm mesa openal ]}
    ${if useAlsa then "${alsaOss}/bin/aoss" else "" } \
      ${jre}/bin/java -jar $out/FTB_Launcher.jar
    EOF

    chmod +x $out/bin/ftb

    mkdir -p $out/share/applications
    ln -s ${desktopItem}/share/applications/* $out/share/applications/

    ${openjdk}/bin/jar xf $out/FTB_Launcher.jar image/logo_ftb.png
    install -D image/logo_ftb.png $out/share/icons/hicolor/32x32/apps/feed-the-beast.png
  '';

  meta = {
      description = "Heavily modded Minecraft with tech mods";
      homepage = http://www.feed-the-beast.com;
      maintainers = with stdenv.lib.maintainers; [ cpages ryantm ];
      license = stdenv.lib.licenses.unfreeRedistributable;
  };
}
