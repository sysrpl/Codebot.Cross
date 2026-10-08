#!/bin/bash

if [ "$(id -u)" -eq 0 ]; then
  echo "Please run this script as your normal user, not as root or with sudo."
  echo "It will ask for administrator access to install packages."
  exit 1
fi

# The compiler and lazbuild in the archives are built for 64 bit ARM Linux
if [ "$(uname -m)" != aarch64 ]; then
  echo "This script is for the Raspberry Pi running 64 bit Raspberry Pi OS, but"
  echo "this computer is $(uname -m). Use install.sh on other computers."
  exit 1
fi

echo "This script will install FPC and the Codebot SDL library."
echo

# Asks a yes or no question, the answer defaults to yes
ask() {
  read -r -p "$1 [Y/n] " A < /dev/tty
  case "$A" in
    [nN]|[nN][oO]) echo no ;;
    *) echo yes ;;
  esac
}

if [ "$(ask "Do you want to install FPC and the Codebot SDL library?")" = no ]; then
  echo "Installation cancelled."
  exit 0
fi

DEMOS=$(ask "Install the demo projects?")

# Install folder
INSTALL_DIR=$HOME/Development/Pascal
PROMPT="Install FPC and the Codebot library into this folder:"

while true; do
  echo
  echo "$PROMPT"
  read -r -e -i "$INSTALL_DIR" -p "> " INSTALL_DIR < /dev/tty

  # Expand a leading ~ and remove a trailing slash
  INSTALL_DIR=${INSTALL_DIR/#\~/$HOME}
  INSTALL_DIR=${INSTALL_DIR%/}

  if [ -z "$INSTALL_DIR" ]; then
    PROMPT="Please enter a folder name. Install FPC and the Codebot library into this folder:"
  elif [ -x "$INSTALL_DIR/fpc/bin/fpc" ] && [ -x "$INSTALL_DIR/lazarus/lazbuild" ]; then
    # FPC and lazbuild were installed before, so the packages and the
    # download are skipped and only the Codebot library and demos are added
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
  echo "FPC and lazbuild are already installed in $INSTALL_DIR, skipping the"
  echo "package install and the download."
else
  echo
  echo "Installing the required packages..."
  sudo apt-get update || exit 1
  sudo apt-get install -y \
    build-essential \
    curl \
    7zip \
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
fi

REPO=sysrpl/Codebot.Cross
RELEASE=https://github.com/$REPO/releases/download/raspberrypi
ARCHIVES="fpc.raspberrypi.7z lazarus.raspberrypi.7z"

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
# installed. The fpc and lazarus folders are only removed when this script
# downloaded them, so an existing install is never removed.
cleanup() {
  [ -n "$CLEANUP" ] || return
  echo
  echo "Installation failed. Removing the partly installed files..."
  cd /
  [ -z "$HAVE_FPC" ] && rm -rf "$INSTALL_DIR/fpc" "$INSTALL_DIR/lazarus"
  for ARCHIVE in $ARCHIVES; do
    rm -f "$INSTALL_DIR/$ARCHIVE"
  done
  rm -rf "$INSTALL_DIR/codebot.tmp" "$INSTALL_DIR/demos.tmp"
  rm -f "$INSTALL_DIR/codebot.zip" "$INSTALL_DIR/demos.zip"
  restore "$DEMOS_DIR" "$MADE_DEMOS"
  restore "$CODEBOT_DIR" "$MADE_CODEBOT"
  rmdir "$INSTALL_DIR/Projects" "$INSTALL_DIR/Libraries" 2> /dev/null
  [ -n "$COPIED_LAZARUS" ] && rm -f "$HOME/.local/bin/lazarus"
  [ -n "$MADE_DIR" ] && rmdir "$INSTALL_DIR" 2> /dev/null
}
trap cleanup EXIT
trap 'exit 1' INT TERM

[ -d "$INSTALL_DIR" ] || MADE_DIR=yes
CLEANUP=yes

mkdir -p "$INSTALL_DIR" || { echo "Could not create $INSTALL_DIR"; exit 1; }
cd "$INSTALL_DIR" || exit 1

# The 7zip package names its command 7zz on older releases
SEVENZIP=$(command -v 7z || command -v 7zz)
if [ -z "$SEVENZIP" ]; then
  echo "Could not find 7-Zip. Install it with: sudo apt install 7zip"
  exit 1
fi

# Download FPC and the trimmed Lazarus with lazbuild, then extract them
if [ -z "$HAVE_FPC" ]; then
  echo
  for ARCHIVE in $ARCHIVES; do
    echo "Downloading $ARCHIVE"
    curl -fL --progress-bar -o "$ARCHIVE" "$RELEASE/$ARCHIVE" ||
      { echo "Download failed: $RELEASE/$ARCHIVE"; exit 1; }
  done

  for ARCHIVE in $ARCHIVES; do
    echo
    "$SEVENZIP" x -y "$ARCHIVE" > /dev/null || { echo "Could not extract $ARCHIVE"; exit 1; }
    rm -f "$ARCHIVE"
  done

  # The archives were made in /home/pi/Development/Pascal. Replace that path
  # with the install folder in every file holding it: fpc.cfg, the lazbuild
  # configuration with its codebot package links, and the lazarus script.
  # This is only done to a new download, so the path is never replaced twice.
  OLD_DIR=/home/pi/Development/Pascal
  NEW_DIR=$(printf '%s' "$INSTALL_DIR" | sed 's/[\\|&]/\\&/g')
  grep -rlIF "$OLD_DIR" fpc lazarus 2> /dev/null | while read -r F; do
    sed -i "s|$OLD_DIR|$NEW_DIR|g" "$F"
  done
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
CODEBOT_DIR=$INSTALL_DIR/Libraries/Codebot
# Only the codebot, codebot_render and codebot_render_sdl packages are used,
# so delete everything else in the source folder of a new download
if update_folder "the Codebot library" "$REPO" "$CODEBOT_DIR" codebot MADE_CODEBOT; then
  for ITEM in "$CODEBOT_DIR"/source/*; do
    case "$(basename "$ITEM")" in
      codebot|codebot_render|codebot_render_sdl) ;;
      *) rm -rf "$ITEM" ;;
    esac
  done
fi

# Material Design Icons font, used by the widgets for their glyphs. It is
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
DEMOS_DIR=$INSTALL_DIR/Projects/Demos
if [ "$DEMOS" = yes ]; then
  # The Codebot.* demos use the LCL, which is not installed, so delete them
  # from a new download
  if update_folder "the demo projects" sysrpl/Codebot.Demos "$DEMOS_DIR" demos MADE_DEMOS; then
    rm -rf "$DEMOS_DIR"/Codebot.*
  fi
fi

# Register the codebot packages with lazbuild, so projects using them find
# them in the install folder. This is done every time the script runs, which
# keeps the links pointing at the library after it is updated.
export PPC_CONFIG_PATH=$INSTALL_DIR/fpc/bin
export PATH=$PPC_CONFIG_PATH:$PATH
echo
echo "Registering the codebot packages with lazbuild"
for PACKAGE in codebot codebot_render codebot_render_sdl; do
  "$INSTALL_DIR/lazarus/lazbuild" --lazarusdir="$INSTALL_DIR/lazarus" \
    --pcp="$INSTALL_DIR/lazarus/config" \
    --add-package-link "$CODEBOT_DIR/source/$PACKAGE/$PACKAGE.lpk" > /dev/null ||
    { echo "Could not register the $PACKAGE package."; exit 1; }
done

# The lazarus command builds and runs the project in the current folder. It is
# copied to ~/.local/bin, which is put on the path below if it is not already.
[ -d "$HOME/.local/bin" ] || MADE_BIN=yes
mkdir -p "$HOME/.local/bin"
cp "$INSTALL_DIR/lazarus/lazarus" "$HOME/.local/bin/lazarus" ||
  { echo "Could not copy the lazarus command to $HOME/.local/bin"; exit 1; }
chmod +x "$HOME/.local/bin/lazarus"
COPIED_LAZARUS=yes

# Put fpc on the path of new terminals. The block is the marker line and the
# two lines after it. If it exists for another install folder it is replaced.
MARK="# Free Pascal path"
FPC_LINE="export PPC_CONFIG_PATH=$(printf '%q' "$INSTALL_DIR/fpc/bin")"
if ! grep -qxF "$FPC_LINE" "$HOME/.bashrc" 2> /dev/null; then
  if grep -qxF "$MARK" "$HOME/.bashrc" 2> /dev/null; then
    sed -i "/^$MARK\$/,+2d" "$HOME/.bashrc"
    echo
    echo "Updated the FPC path in ~/.bashrc to $INSTALL_DIR/fpc/bin"
  fi
  cat >> "$HOME/.bashrc" <<BASHRC

$MARK
$FPC_LINE
export PATH="\$PPC_CONFIG_PATH:\$PATH"
BASHRC
fi

# Put ~/.local/bin on the path of new terminals when this script created it or
# it is not on the path already. The check in ~/.bashrc stops it being added
# twice when ~/.profile has added it at login.
BIN_MARK="# ~/.local/bin path"
case ":$PATH:" in
  *":$HOME/.local/bin:"*) ON_PATH=yes ;;
esac
if { [ -n "$MADE_BIN" ] || [ -z "$ON_PATH" ]; } &&
   ! grep -qF "$BIN_MARK" "$HOME/.bashrc" 2> /dev/null; then
  cat >> "$HOME/.bashrc" <<'BASHRC'

# ~/.local/bin path
case ":$PATH:" in
  *":$HOME/.local/bin:"*) ;;
  *) export PATH="$HOME/.local/bin:$PATH" ;;
esac
BASHRC
fi

# Finished, nothing to clean up. Remove the folders replaced by updates.
CLEANUP=
rm -rf "$CODEBOT_DIR.old" "$DEMOS_DIR.old"

echo
echo "Installation complete."
echo
echo "FPC and lazbuild were installed into $INSTALL_DIR"
echo "The Codebot library is in $CODEBOT_DIR"
[ "$DEMOS" = yes ] && echo "The demo projects are in $DEMOS_DIR"
echo
echo "To use fpc and the lazarus command, open a new terminal or run this in"
echo "the terminal you have open:"
echo "  source ~/.bashrc"
echo
echo "Then in a folder holding a project, type:"
echo "  lazarus          to build the project"
echo "  lazarus rebuild  to rebuild it and everything it uses"
echo "  lazarus run      to build and run the project"

# Optionally build the demo projects, then optionally run the mega demo
if [ "$DEMOS" = yes ] && [ -d "$DEMOS_DIR" ]; then
  echo
  if [ "$(ask "Do you want to build the demo projects?")" = yes ]; then
    FAILED=""
    echo
    while read -r PROJECT; do
      NAME=$(basename "$PROJECT" .lpi)
      if ! (cd "$(dirname "$PROJECT")" && "$INSTALL_DIR/lazarus/lazarus" < /dev/null); then
        FAILED="$FAILED $NAME"
      fi
    done < <(find "$DEMOS_DIR" -name '*.lpi' -not -path '*/backup/*' | sort)

    if [ -n "$FAILED" ]; then
      echo
      echo "Some demo projects could not be built:$FAILED"
    else
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

# Show how to build and run a demo
if [ "$DEMOS" = yes ] && [ -d "$DEMOS_DIR/MegaDemo" ]; then
  echo
  echo "To build and run a demo, change to its folder and type lazarus run."
  echo "For example, to run the mega demo:"
  echo "  cd $(printf '%q' "$DEMOS_DIR/MegaDemo")"
  echo "  lazarus run"
fi
