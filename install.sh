#!/bin/bash

if [ "$(id -u)" -eq 0 ]; then
  echo "Please run this script as your normal user, not as root or with sudo."
  echo "It will ask for administrator access only if packages need installing."
  exit 1
fi

echo "This script will install the Free Pascal compiler (FPC) and the Lazarus IDE"
echo "on your Linux computer."
echo
read -r -p "Do you want to install FPC and Lazarus? [y/N] " ANSWER < /dev/tty

case "$ANSWER" in
  [yY]|[yY][eE][sS]) ;;
  *)
    echo "Installation cancelled."
    exit 0
    ;;
esac

# Optional features, all default to yes
OPENGL=yes
WEBKIT=yes
TERMINAL=yes

if command -v whiptail > /dev/null; then
  CHOICES=$(whiptail --title "Optional Features" --separate-output \
    --checklist "Select the features to install (space toggles, enter accepts):" 12 64 3 \
    opengl "OpenGL rendering support" ON \
    webkit "Web browser control" ON \
    terminal "Terminal controls" ON \
    3>&1 1>&2 2>&3 < /dev/tty) || { echo "Installation cancelled."; exit 0; }
  OPENGL=no
  WEBKIT=no
  TERMINAL=no
  for C in $CHOICES; do
    case $C in
      opengl) OPENGL=yes ;;
      webkit) WEBKIT=yes ;;
      terminal) TERMINAL=yes ;;
    esac
  done
else
  ask() {
    read -r -p "$1 [Y/n] " A < /dev/tty
    case "$A" in
      [nN]|[nN][oO]) echo no ;;
      *) echo yes ;;
    esac
  }
  OPENGL=$(ask "Include OpenGL rendering support?")
  WEBKIT=$(ask "Include the web browser control?")
  TERMINAL=$(ask "Include terminal controls?")
fi

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

# Returns success if a package is available to install
available() {
  case $PM in
    apt) apt-cache show "$1" > /dev/null 2>&1 ;;
    dnf) dnf -q info "$1" > /dev/null 2>&1 ;;
    zypper) zypper -q search -x "$1" > /dev/null 2>&1 ;;
    pacman) pacman -Si "$1" > /dev/null 2>&1 ;;
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
  apt) ESSENTIAL="build-essential curl libgtk-3-dev" ;;
  dnf|zypper) ESSENTIAL="gcc make binutils glibc-devel curl pkgconfig(gtk+-3.0)" ;;
  pacman) ESSENTIAL="base-devel curl gtk3" ;;
esac

NEEDED=$(missing $ESSENTIAL)

# 7-Zip is checked by its command, older releases name the package differently
if ! command -v 7z > /dev/null && ! command -v 7zz > /dev/null; then
  if available 7zip; then
    NEEDED="$NEEDED 7zip"
  else
    case $PM in
      apt) NEEDED="$NEEDED p7zip-full" ;;
      dnf) NEEDED="$NEEDED p7zip p7zip-plugins" ;;
      *) NEEDED="$NEEDED p7zip" ;;
    esac
  fi
fi

NEEDED=$(echo $NEEDED)
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
    PKG_OPENGL="libsdl2-dev libassimp-dev"
    PKG_VIDEO="libmpv-dev"
    PKG_WEBKIT="libwebkit2gtk-4.1-dev"
    PKG_TERMINAL="libvte-2.91-dev" ;;
  dnf|zypper)
    PKG_OPENGL="pkgconfig(sdl2) pkgconfig(assimp)"
    PKG_VIDEO="pkgconfig(mpv)"
    PKG_WEBKIT="pkgconfig(webkit2gtk-4.1)"
    PKG_TERMINAL="pkgconfig(vte-2.91)" ;;
  pacman)
    PKG_OPENGL="sdl2 assimp"
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
# cannot be installed, video is disabled in the codebot packages.
VIDEO=no
if [ "$OPENGL" = yes ]; then
  VIDEO=yes
  NEEDED=$(echo $(missing $PKG_VIDEO))
  if [ -n "$NEEDED" ]; then
    echo
    echo "Video support needs: $NEEDED"
    if ! pm_install $NEEDED; then
      echo "Video support is not available on this system and will be disabled."
      VIDEO=no
    fi
  fi
fi

# Install folder
INSTALL_DIR=$HOME/Development/Pascal
PROMPT="Install FPC and Lazarus into this folder:"

while true; do
  if command -v whiptail > /dev/null; then
    INSTALL_DIR=$(whiptail --title "Install Folder" --inputbox "$PROMPT" 12 72 "$INSTALL_DIR" \
      3>&1 1>&2 2>&3 < /dev/tty) || { echo "Installation cancelled."; exit 0; }
  else
    echo
    echo "$PROMPT"
    read -r -e -i "$INSTALL_DIR" -p "> " INSTALL_DIR < /dev/tty
  fi

  # Expand a leading ~ and remove a trailing slash
  INSTALL_DIR=${INSTALL_DIR/#\~/$HOME}
  INSTALL_DIR=${INSTALL_DIR%/}

  if [ -z "$INSTALL_DIR" ]; then
    PROMPT="Please enter a folder name. Install FPC and Lazarus into this folder:"
  elif [ -e "$INSTALL_DIR/fpc" ] || [ -e "$INSTALL_DIR/lazarus" ]; then
    PROMPT="An fpc or lazarus folder already exists in $INSTALL_DIR. Please pick another folder:"
  else
    break
  fi
done

# Download
REPO=sysrpl/Codebot.Cross

# If a step fails or the script is interrupted from here on, remove what was
# installed. The install folder was checked above to have no fpc or lazarus
# folder, so everything removed here was created by this script.
cleanup() {
  [ -n "$CLEANUP" ] || return
  echo
  echo "Installation failed. Removing the partly installed files..."
  cd /
  rm -rf "$INSTALL_DIR/fpc" "$INSTALL_DIR/lazarus"
  rm -f "$INSTALL_DIR/freepascal.desktop" "$INSTALL_DIR/lazarus.desktop"
  [ -n "$FPC_ARCHIVE" ] && rm -f "$INSTALL_DIR/$FPC_ARCHIVE"
  [ -n "$LAZ_ARCHIVE" ] && rm -f "$INSTALL_DIR/$LAZ_ARCHIVE"
  if [ -n "$COPIED_APPS" ]; then
    rm -f "$APPS/freepascal.desktop" "$APPS/lazarus.desktop"
  fi
  [ -n "$MADE_DIR" ] && rmdir "$INSTALL_DIR" 2> /dev/null
}
trap cleanup EXIT
trap 'exit 1' INT TERM

[ -d "$INSTALL_DIR" ] || MADE_DIR=yes
CLEANUP=yes

mkdir -p "$INSTALL_DIR" || { echo "Could not create $INSTALL_DIR"; exit 1; }
cd "$INSTALL_DIR" || exit 1

echo
echo "Finding the latest build..."
URLS=$(curl -fsSL "https://api.github.com/repos/$REPO/releases?per_page=100" |
  grep -o '"browser_download_url": *"[^"]*/download/build-[0-9]*/[^"]*-x86_64-linux\.7z"' |
  sed 's/.*"\(https[^"]*\)"/\1/')
if [ -z "$URLS" ]; then
  echo "Could not find a build on GitHub. Check your internet connection and try again."
  exit 1
fi

# The newest build has the highest build number
BUILD=$(echo "$URLS" | sed 's|.*/download/build-\([0-9]*\)/.*|\1|' | sort -n | tail -1)
FPC_URL=$(echo "$URLS" | grep "/download/build-$BUILD/fpc\.")
LAZ_URL=$(echo "$URLS" | grep "/download/build-$BUILD/lazarus\.")
if [ -z "$FPC_URL" ] || [ -z "$LAZ_URL" ]; then
  echo "Build $BUILD on GitHub is missing the fpc or lazarus archive."
  exit 1
fi

FPC_ARCHIVE=$(basename "$FPC_URL")
LAZ_ARCHIVE=$(basename "$LAZ_URL")

echo "Build $BUILD will be downloaded into $INSTALL_DIR"
echo
for URL in "$FPC_URL" "$LAZ_URL"; do
  echo "Downloading $(basename "$URL")"
  curl -fL --progress-bar -o "$(basename "$URL")" "$URL" ||
    { echo "Download failed: $URL"; exit 1; }
done

# Extract the archives, then delete them
SEVENZIP=$(command -v 7z || command -v 7zz)

for ARCHIVE in "$FPC_ARCHIVE" "$LAZ_ARCHIVE"; do
  echo
  "$SEVENZIP" x -y "$ARCHIVE" || { echo "Could not extract $ARCHIVE"; exit 1; }
done
rm -f "$FPC_ARCHIVE" "$LAZ_ARCHIVE"

# Replace the original build path with the install folder
OLD_DIR=/home/gigauser/Development/Pascal
NEW_DIR=$(printf '%s' "$INSTALL_DIR" | sed 's/[\\|&]/\\&/g')
grep -rlIF "$OLD_DIR" lazarus fpc *.desktop 2> /dev/null | while read -r F; do
  sed -i "s|$OLD_DIR|$NEW_DIR|g" "$F"
done

# Terminal script used by freepascal.desktop. The desktop entry uses Terminal=true
# so each desktop opens it in its own default terminal program.
cat > fpc/bin/fpc-terminal.sh <<TERM
#!/bin/bash
cd $(printf '%q' "$INSTALL_DIR/fpc")
. ./setup
cd ~
exec "\${SHELL:-/bin/bash}"
TERM
chmod +x fpc/bin/fpc-terminal.sh

cat > freepascal.desktop <<DESK
[Desktop Entry]
Name=Free Pascal Terminal
Comment=Open a new terminal with the fpc program made available
Icon=utilities-terminal
Exec="$INSTALL_DIR/fpc/bin/fpc-terminal.sh"
Terminal=true
Type=Application
Categories=Development;
DESK

cat > lazarus.desktop <<DESK
[Desktop Entry]
Name=Lazarus
Comment=Lazarus IDE powered by Free Pascal
StartupWMClass=Lazarus
Icon=$INSTALL_DIR/lazarus/images/icons/lazarus.svg
Exec="$INSTALL_DIR/lazarus/lazarus.sh"
Terminal=false
Type=Application
Categories=Development;IDE;
DESK

# Add both launchers to the applications menu
APPS=$HOME/.local/share/applications
mkdir -p "$APPS"
cp freepascal.desktop lazarus.desktop "$APPS/"
COPIED_APPS=yes
command -v update-desktop-database > /dev/null && update-desktop-database "$APPS" 2> /dev/null

# Set up the compiler configuration and path
cd "$INSTALL_DIR/fpc" || exit 1
. ./setup

# Build the Lazarus IDE with the packages listed in its configuration
cd ../lazarus || exit 1
echo
echo "Building the Lazarus IDE. This can take several minutes..."
make useride LAZBUILDOPTS="--lazarusdir=. --pcp=config --ws=gtk3" ||
  { echo "The Lazarus IDE could not be built."; exit 1; }

# Finished, nothing to clean up
CLEANUP=

echo
echo "Installation complete."
echo
echo "FPC and Lazarus were installed into $INSTALL_DIR"
echo
echo "Two desktop files were added to your applications menu:"
echo "  Lazarus               - starts the Lazarus IDE"
echo "  Free Pascal Terminal  - opens a terminal with the fpc compiler available"
echo
echo "Copies of the desktop files are in $APPS and $INSTALL_DIR"
