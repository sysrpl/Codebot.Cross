#!/bin/bash

# Installs only the packages the Codebot SDL library (the codebot,
# codebot_render and codebot_render_sdl packages) depends on, on Raspberry Pi
# OS. It does not install FPC, lazbuild, the library itself or the demo
# projects.

if [ "$(id -u)" -eq 0 ]; then
  echo "Please run this script as your normal user, not as root or with sudo."
  echo "It will ask for administrator access to install packages."
  exit 1
fi

echo "This script will install the packages the Codebot SDL library depends on."
echo

# Asks a yes or no question, the answer defaults to yes
ask() {
  read -r -p "$1 [Y/n] " A < /dev/tty
  case "$A" in
    [nN]|[nN][oO]) echo no ;;
    *) echo yes ;;
  esac
}

if [ "$(ask "Do you want to install the Codebot SDL dependencies?")" = no ]; then
  echo "Installation cancelled."
  exit 0
fi

echo
echo "Installing the required packages..."
sudo apt-get update || exit 1
sudo apt-get install -y \
  build-essential \
  libsdl2-dev \
  libsdl2-image-dev \
  libassimp-dev \
  libxml2-dev \
  libmpv-dev \
  libssl-dev \
  libgl1-mesa-dev \
  libglu1-mesa-dev \
  libegl1-mesa-dev \
  libgles2-mesa-dev || exit 1

# Material Design Icons font, used by the widgets for their glyphs. It is
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
echo "The Codebot SDL dependencies are installed."
