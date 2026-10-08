#!/bin/bash

if [ "$(id -u)" -eq 0 ]; then
  echo "Please run this script as your normal user, not as root or with sudo."
  echo "It will ask for administrator access only if packages need installing."
  exit 1
fi

echo "This script will install the Free Pascal compiler (FPC) and the Lazarus IDE"
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

if [ "$(ask "Do you want to install FPC and Lazarus?")" = no ]; then
  echo "Installation cancelled."
  exit 0
fi

# Optional features
OPENGL=$(ask "Include OpenGL rendering support?")
WEBKIT=$(ask "Include the web browser control?")
TERMINAL=$(ask "Include terminal controls?")
DEMOS=$(ask "Install the demo projects?")

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
  apt) ESSENTIAL="build-essential curl libgtk-3-dev libxml2-dev" ;;
  dnf|zypper) ESSENTIAL="gcc make binutils glibc-devel curl pkgconfig(gtk+-3.0) pkgconfig(libxml-2.0)" ;;
  pacman) ESSENTIAL="base-devel curl gtk3 libxml2" ;;
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
  echo
  echo "$PROMPT"
  read -r -e -i "$INSTALL_DIR" -p "> " INSTALL_DIR < /dev/tty

  # Expand a leading ~ and remove a trailing slash
  INSTALL_DIR=${INSTALL_DIR/#\~/$HOME}
  INSTALL_DIR=${INSTALL_DIR%/}

  if [ -z "$INSTALL_DIR" ]; then
    PROMPT="Please enter a folder name. Install FPC and Lazarus into this folder:"
  elif [ -x "$INSTALL_DIR/fpc/bin/fpc" ] && [ -x "$INSTALL_DIR/lazarus/lazbuild" ]; then
    # FPC and Lazarus were installed before, so the download is skipped and
    # only the Codebot library and demos are added or updated
    HAVE_FPC=yes
    break
  elif [ -e "$INSTALL_DIR/fpc" ] || [ -e "$INSTALL_DIR/lazarus" ]; then
    PROMPT="An incomplete fpc or lazarus folder exists in $INSTALL_DIR. Please pick another folder:"
  else
    break
  fi
done

if [ -n "$HAVE_FPC" ]; then
  echo
  echo "FPC and Lazarus are already installed in $INSTALL_DIR, skipping the"
  echo "download. The Codebot library and demos will be added or updated."
fi

CODEBOT_DIR=$INSTALL_DIR/Libraries/Codebot
DEMOS_DIR=$INSTALL_DIR/Projects/Demos

# Download
REPO=sysrpl/Codebot.Cross

# Puts back the folder an update replaced, or removes a folder this script
# downloaded when there was none before
restore() {
  [ -n "$1" ] || return
  if [ -d "$1.old" ]; then
    rm -rf "$1"
    mv "$1.old" "$1"
  elif [ -n "$2" ]; then
    rm -rf "$1"
  fi
}

# If a step fails or the script is interrupted from here on, remove what was
# installed. The fpc and lazarus folders and the desktop files are only removed
# when this script downloaded them, so an existing install is never removed.
cleanup() {
  [ -n "$CLEANUP" ] || return
  echo
  echo "Installation failed. Removing the partly installed files..."
  cd /
  if [ -z "$HAVE_FPC" ]; then
    rm -rf "$INSTALL_DIR/fpc" "$INSTALL_DIR/lazarus"
    rm -f "$INSTALL_DIR/freepascal.desktop" "$INSTALL_DIR/lazarus.desktop"
    if [ -n "$COPIED_APPS" ]; then
      rm -f "$APPS/freepascal.desktop" "$APPS/lazarus.desktop"
    fi
  fi
  [ -n "$FPC_ARCHIVE" ] && rm -f "$INSTALL_DIR/$FPC_ARCHIVE"
  [ -n "$LAZ_ARCHIVE" ] && rm -f "$INSTALL_DIR/$LAZ_ARCHIVE"
  rm -rf "$INSTALL_DIR/codebot.tmp" "$INSTALL_DIR/demos.tmp"
  rm -f "$INSTALL_DIR/codebot.zip" "$INSTALL_DIR/demos.zip"
  restore "$DEMOS_DIR" "$MADE_DEMOS"
  restore "$CODEBOT_DIR" "$MADE_CODEBOT"
  rmdir "$INSTALL_DIR/Projects" "$INSTALL_DIR/Libraries" 2> /dev/null
  [ -n "$MADE_DIR" ] && rmdir "$INSTALL_DIR" 2> /dev/null
}
trap cleanup EXIT
trap 'exit 1' INT TERM

[ -d "$INSTALL_DIR" ] || MADE_DIR=yes
CLEANUP=yes

mkdir -p "$INSTALL_DIR" || { echo "Could not create $INSTALL_DIR"; exit 1; }
cd "$INSTALL_DIR" || exit 1

# The 7zip package names its command 7zz on some releases
SEVENZIP=$(command -v 7z || command -v 7zz)

# Download FPC and Lazarus for a new install
if [ -z "$HAVE_FPC" ]; then
  echo
  echo "Finding the latest build..."
  RELEASES=$(curl -fsSL "https://api.github.com/repos/$REPO/releases?per_page=100")
  URLS=$(echo "$RELEASES" |
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
  for ARCHIVE in "$FPC_ARCHIVE" "$LAZ_ARCHIVE"; do
    echo
    "$SEVENZIP" x -y "$ARCHIVE" || { echo "Could not extract $ARCHIVE"; exit 1; }
  done
  rm -f "$FPC_ARCHIVE" "$LAZ_ARCHIVE"
fi

# Downloads the default branch of a GitHub repository into a folder, or updates
# the folder when that branch has a newer commit. The commit downloaded is kept in
# a .commit file in the folder to compare with next time. A folder holding a
# git clone is never replaced. Returns success only when the folder was
# downloaded.
#
#   update_folder <title> <repository> <folder> <temporary name> <made variable>
update_folder() {
  local TITLE=$1 SOURCE=$2 DIR=$3 TMP=$4 MADE=$5
  local REMOTE LOCAL
  echo
  if [ -d "$DIR/.git" ]; then
    echo "$DIR is a git clone, keeping it."
    return 1
  fi
  # Find the latest commit without git, the GitHub API or a download. Asking
  # for archive/HEAD.zip, the zip of the default branch, makes GitHub answer
  # with a redirect to the zip of that branch's latest commit:
  #
  #   location: https://codeload.github.com/<repository>/zip/<commit>
  #
  # Only the headers are requested (curl -I) and the redirect is not followed
  # (no -L), so nothing is downloaded. The commit is cut from the end of the
  # location header, after removing the carriage returns ending each header
  # line. HEAD works for repositories using master or main. If GitHub cannot
  # be reached or answers differently, REMOTE is left empty.
  REMOTE=$(curl -fsSI "https://github.com/$SOURCE/archive/HEAD.zip" 2> /dev/null |
    tr -d '\r' | sed -n 's|^[Ll]ocation: .*/zip/\([0-9a-f]*\)$|\1|p')
  case "$REMOTE" in
    *[!0-9a-f]*|"") REMOTE="" ;;
  esac
  if [ -e "$DIR" ]; then
    LOCAL=$(tr -d '[:space:]' < "$DIR/.commit" 2> /dev/null)
    if [ -z "$REMOTE" ]; then
      echo "Could not check GitHub for a newer $TITLE, keeping $DIR."
      return 1
    fi
    if [ "$REMOTE" = "$LOCAL" ]; then
      echo "$TITLE is up to date (commit ${LOCAL:0:7})."
      return 1
    fi
    echo "Updating $TITLE to commit ${REMOTE:0:7}"
  else
    if [ -z "$REMOTE" ]; then
      echo "Could not find $TITLE on GitHub. Check your internet connection and try again."
      exit 1
    fi
    echo "Downloading $TITLE (commit ${REMOTE:0:7})"
    printf -v "$MADE" yes
  fi

  # Download that exact commit, so .commit matches what was installed
  curl -fL --progress-bar -o "$TMP.zip" \
    "https://github.com/$SOURCE/archive/$REMOTE.zip" ||
    { echo "Download failed: $TITLE"; exit 1; }

  # The zip holds a single top folder, extract it and move it into place. An
  # existing folder is kept as .old until the new one is in place.
  rm -rf "$TMP.tmp"
  "$SEVENZIP" x -y -o"$TMP.tmp" "$TMP.zip" > /dev/null ||
    { echo "Could not extract $TMP.zip"; exit 1; }
  mkdir -p "$(dirname "$DIR")"
  rm -rf "$DIR.old"
  [ -e "$DIR" ] && mv "$DIR" "$DIR.old"
  mv "$TMP.tmp"/* "$DIR" || { echo "Could not install $TITLE"; exit 1; }
  echo "$REMOTE" > "$DIR/.commit"
  rm -rf "$TMP.tmp" "$TMP.zip"
  return 0
}

# Codebot library
update_folder "the Codebot library" "$REPO" "$CODEBOT_DIR" codebot MADE_CODEBOT

# Material Design Icons font, used by the controls for their glyphs. It is
# copied again when the library has a newer font.
FONTS=$HOME/.local/share/fonts
FONT=$CODEBOT_DIR/fonts/materialdesignicons.ttf
if [ -f "$FONT" ] && ! cmp -s "$FONT" "$FONTS/materialdesignicons.ttf"; then
  echo
  echo "Installing the Material Design Icons font"
  mkdir -p "$FONTS"
  cp "$FONT" "$FONTS/"
  command -v fc-cache > /dev/null && fc-cache -f "$FONTS"
fi

# Demo projects from the Codebot.Demos repository
if [ "$DEMOS" = yes ]; then
  update_folder "the demo projects" sysrpl/Codebot.Demos "$DEMOS_DIR" demos MADE_DEMOS
fi

# Replace the original build path with the install folder. This is only done
# to a new download, so the path is never replaced twice.
if [ -z "$HAVE_FPC" ]; then
  OLD_DIR=/home/gigauser/Development/Pascal
  NEW_DIR=$(printf '%s' "$INSTALL_DIR" | sed 's/[\\|&]/\\&/g')
  grep -rlIF "$OLD_DIR" lazarus fpc 2> /dev/null | while read -r F; do
    sed -i "s|$OLD_DIR|$NEW_DIR|g" "$F"
  done
fi

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

# File managers only show the icon of a desktop file and run it on a double
# click when it is executable and, on some desktops, marked as trusted
chmod +x freepascal.desktop lazarus.desktop
if command -v gio > /dev/null; then
  gio set freepascal.desktop metadata::trusted true 2> /dev/null
  gio set lazarus.desktop metadata::trusted true 2> /dev/null
fi

# Add both launchers to the applications menu
APPS=$HOME/.local/share/applications
mkdir -p "$APPS"
cp freepascal.desktop lazarus.desktop "$APPS/"
COPIED_APPS=yes
command -v update-desktop-database > /dev/null && update-desktop-database "$APPS" 2> /dev/null

# Set up the compiler configuration and path
cd "$INSTALL_DIR/fpc" || exit 1
. ./setup

cd ../lazarus || exit 1

# Register the codebot packages for the selected features. Runtime packages are
# linked, design packages are marked to be installed into the IDE.
link_package() {
  ./lazbuild --lazarusdir=. --pcp=config --add-package-link "$CODEBOT_DIR/source/$1/$1.lpk" > /dev/null ||
    { echo "Could not register the $1 package."; exit 1; }
}

install_package() {
  ./lazbuild --lazarusdir=. --pcp=config --add-package "$CODEBOT_DIR/source/$1/$1.lpk" > /dev/null ||
    { echo "Could not add the $1 package to the IDE."; exit 1; }
}

# Without libmpv, turn off the video widget in the render package
if [ "$VIDEO" = no ]; then
  sed -i 's/^{\$define videowidget}/{.$define videowidget}/' "$CODEBOT_DIR/source/codebot_render/render.inc"
fi

echo
echo "Adding the codebot packages to Lazarus"
link_package codebot
link_package codebot_controls
install_package codebot_controls_design
if [ "$OPENGL" = yes ]; then
  link_package codebot_render
  link_package codebot_render_controls
  link_package codebot_render_sdl
  install_package codebot_render_design
fi
if [ "$WEBKIT" = yes ]; then
  link_package codebot_webkit
  install_package codebot_webkit_design
fi
if [ "$TERMINAL" = yes ]; then
  link_package codebot_terminal
  install_package codebot_terminal_design
fi
# Build the Lazarus IDE with the packages listed in its configuration
echo
echo "Building the Lazarus IDE. This can take several minutes..."
make useride LAZBUILDOPTS="--lazarusdir=. --pcp=config --ws=gtk3" ||
  { echo "The Lazarus IDE could not be built."; exit 1; }

# Finished, nothing to clean up. Remove the folders replaced by updates.
CLEANUP=
rm -rf "$CODEBOT_DIR.old" "$DEMOS_DIR.old"

echo
echo "Installation complete."
echo
echo "FPC and Lazarus are installed in $INSTALL_DIR"
echo "The Codebot library is in $CODEBOT_DIR"
[ "$DEMOS" = yes ] && echo "The demo projects are in $DEMOS_DIR"
echo
echo "Two desktop files were added to your applications menu:"
echo "  Lazarus               - starts the Lazarus IDE"
echo "  Free Pascal Terminal  - opens a terminal with the fpc compiler available"
echo
echo "Copies of the desktop files are in $APPS and $INSTALL_DIR"

# Optionally build the demo projects, then optionally run the mega demo
if [ "$DEMOS" = yes ] && [ -d "$DEMOS_DIR" ]; then
  echo
  if [ "$(ask "Do you want to build the demo projects?")" = yes ]; then
    LAZBUILD=("$INSTALL_DIR/lazarus/lazbuild" "--lazarusdir=$INSTALL_DIR/lazarus"
      "--pcp=$INSTALL_DIR/lazarus/config" --ws=gtk3 -q)
    LOG=$(mktemp)
    FAILED=""

    # Without libmpv the video widget is turned off in the demos as well
    if [ "$VIDEO" = no ]; then
      grep -rl '^{\$define videowidget}' "$DEMOS_DIR" --include=*.pas 2> /dev/null |
        while read -r F; do
          sed -i 's/^{\$define videowidget}/{.$define videowidget}/' "$F"
        done
    fi

    echo
    while read -r PROJECT; do
      NAME=$(basename "$PROJECT" .lpi)
      # Demos which need a feature that was not installed are skipped
      if { [ "$OPENGL" = no ] && grep -q 'PackageName Value="codebot_render' "$PROJECT"; } ||
         { [ "$WEBKIT" = no ] && grep -q 'PackageName Value="codebot_webkit' "$PROJECT"; } ||
         { [ "$TERMINAL" = no ] && grep -q 'PackageName Value="codebot_terminal' "$PROJECT"; }; then
        echo "Skipping $NAME, it needs a feature which was not installed"
        continue
      fi
      echo "Building $NAME"
      if ! "${LAZBUILD[@]}" "$PROJECT" < /dev/null >> "$LOG" 2>&1; then
        echo "  $NAME could not be built"
        FAILED="$FAILED $NAME"
      fi
    done < <(find "$DEMOS_DIR" -name '*.lpi' -not -path '*/backup/*' | sort)

    if [ -n "$FAILED" ]; then
      echo
      echo "Some demo projects could not be built:$FAILED"
      echo "The build messages are in $LOG"
    else
      rm -f "$LOG"
      echo
      echo "The demo projects were built."
      MEGADEMO=$DEMOS_DIR/MegaDemo/megademo
      if [ -x "$MEGADEMO" ]; then
        echo
        if [ "$(ask "Do you want to run the mega demo?")" = yes ]; then
          (cd "$DEMOS_DIR/MegaDemo" && nohup ./megademo > /dev/null 2>&1 &)
        fi
      fi
    fi
  fi
fi
