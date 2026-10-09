#!/bin/bash

# Installs only the packages the Codebot library depends on. It does not
# install FPC, Lazarus, the library itself or the demo projects.

if [ "$(id -u)" -eq 0 ]; then
  echo "Please run this script as your normal user, not as root or with sudo."
  echo "It will ask for administrator access only if packages need installing."
  exit 1
fi

echo "This script will install the packages the Codebot library depends on"
echo "on your Linux computer."
echo

# Asks a yes or no question, the answer defaults to yes
ask() {
  read -r -p "$1 [Y/n] " A < /dev/tty
  case "$A" in
    [nN]|[nN][oO]) echo no ;;
    *) echo yes ;;
  esac
}

if [ "$(ask "Do you want to install the Codebot dependencies?")" = no ]; then
  echo "Installation cancelled."
  exit 0
fi

# Optional features
OPENGL=$(ask "Include OpenGL rendering support?")
WEBKIT=$(ask "Include the web browser control?")
TERMINAL=$(ask "Include terminal controls?")

# Find the package manager
if command -v apt-get > /dev/null; then
  PM=apt
elif command -v dnf > /dev/null; then
  PM=dnf
elif command -v zypper > /dev/null; then
  PM=zypper
elif command -v pacman > /dev/null; then
  PM=pacman
else
  echo "Could not find a supported package manager (apt, dnf, zypper or pacman)."
  exit 1
fi

# Returns success if a package is installed
installed() {
  case $PM in
    apt) dpkg-query -W -f='${Status}' "$1" 2> /dev/null | grep -q "install ok installed" ;;
    dnf|zypper) rpm -q --whatprovides "$1" > /dev/null 2>&1 ;;
    pacman) pacman -Q "$1" > /dev/null 2>&1 ;;
  esac
}

# Installs packages with sudo
pm_install() {
  case $PM in
    apt)
      if [ -z "$APT_UPDATED" ]; then
        sudo apt-get update || return 1
        APT_UPDATED=yes
      fi
      sudo apt-get install -y "$@" ;;
    dnf) sudo dnf install -y "$@" ;;
    zypper) sudo zypper --non-interactive install "$@" ;;
    pacman) sudo pacman -S --needed --noconfirm "$@" ;;
  esac
}

# Prints the packages from the arguments which are not installed
missing() {
  for P in "$@"; do
    installed "$P" || echo "$P"
  done
}

# Essential packages
case $PM in
  apt) ESSENTIAL="build-essential libgtk-3-dev libxml2-dev" ;;
  dnf|zypper) ESSENTIAL="gcc make binutils glibc-devel pkgconfig(gtk+-3.0) pkgconfig(libxml-2.0)" ;;
  pacman) ESSENTIAL="base-devel gtk3 libxml2" ;;
esac

NEEDED=$(echo $(missing $ESSENTIAL))
if [ -n "$NEEDED" ]; then
  echo
  echo "The following essential packages are required:"
  echo "  $NEEDED"
  echo "Installing them needs administrator access, so you may be asked for your password."
  pm_install $NEEDED || { echo "Could not install the essential packages."; exit 1; }
fi

# Dependency packages for the selected features
case $PM in
  apt)
    PKG_OPENGL="libsdl2-dev libsdl2-image-dev libassimp-dev"
    PKG_VIDEO="libmpv-dev"
    PKG_WEBKIT="libwebkit2gtk-4.1-dev"
    PKG_TERMINAL="libvte-2.91-dev" ;;
  dnf|zypper)
    PKG_OPENGL="pkgconfig(sdl2) pkgconfig(SDL2_image) pkgconfig(assimp)"
    PKG_VIDEO="pkgconfig(mpv)"
    PKG_WEBKIT="pkgconfig(webkit2gtk-4.1)"
    PKG_TERMINAL="pkgconfig(vte-2.91)" ;;
  pacman)
    PKG_OPENGL="sdl2 sdl2_image assimp"
    PKG_VIDEO="mpv"
    PKG_WEBKIT="webkit2gtk-4.1"
    PKG_TERMINAL="vte3" ;;
esac

DEPENDS=""
[ "$OPENGL" = yes ] && DEPENDS="$DEPENDS $PKG_OPENGL"
[ "$WEBKIT" = yes ] && DEPENDS="$DEPENDS $PKG_WEBKIT"
[ "$TERMINAL" = yes ] && DEPENDS="$DEPENDS $PKG_TERMINAL"

NEEDED=$(echo $(missing $DEPENDS))
if [ -n "$NEEDED" ]; then
  echo
  echo "The following packages are required for the features you selected:"
  echo "  $NEEDED"
  echo "Installing them needs administrator access, so you may be asked for your password."
  pm_install $NEEDED || { echo "Could not install the feature packages."; exit 1; }
fi

# Video support needs libmpv, which some distributions do not provide. If it
# cannot be installed, explain how to disable video in the codebot packages.
VIDEO=no
if [ "$OPENGL" = yes ]; then
  VIDEO=yes
  NEEDED=$(echo $(missing $PKG_VIDEO))
  if [ -n "$NEEDED" ]; then
    echo
    echo "Video support needs: $NEEDED"
    if ! pm_install $NEEDED; then
      echo
      echo "Video support is not available on this system. To build codebot_render"
      echo "without it, change this line in source/codebot_render/render.inc:"
      echo "  {\$define videowidget}"
      echo "to:"
      echo "  {.\$define videowidget}"
      VIDEO=no
    fi
  fi
fi

# Material Design Icons font, used by the controls for their glyphs. It is
# copied from the library this script is in, or downloaded from GitHub, and
# copied again when there is a newer font.
FONTS=$HOME/.local/share/fonts
FONT=$(cd "$(dirname "$0")" 2> /dev/null && pwd)/../../fonts/materialdesignicons.ttf
FONT_URL=https://raw.githubusercontent.com/sysrpl/Codebot.Cross/master/fonts/materialdesignicons.ttf
if [ ! -f "$FONT" ]; then
  FONT=$(mktemp)
  TEMP_FONT=yes
  if ! curl -fsSL -o "$FONT" "$FONT_URL" 2> /dev/null &&
     ! wget -q -O "$FONT" "$FONT_URL" 2> /dev/null; then
    echo
    echo "Could not download the Material Design Icons font, skipping it."
    rm -f "$FONT"
    FONT=
  fi
fi
if [ -n "$FONT" ] && ! cmp -s "$FONT" "$FONTS/materialdesignicons.ttf"; then
  echo
  echo "Installing the Material Design Icons font"
  mkdir -p "$FONTS"
  cp "$FONT" "$FONTS/materialdesignicons.ttf"
  command -v fc-cache > /dev/null && fc-cache -f "$FONTS"
fi
[ -n "$TEMP_FONT" ] && rm -f "$FONT"

echo
echo "The Codebot dependencies are installed."
echo
echo "Features available:"
echo "  Core and controls  yes"
echo "  OpenGL rendering   $OPENGL"
echo "  Video playback     $VIDEO"
echo "  Web browser        $WEBKIT"
echo "  Terminal           $TERMINAL"
