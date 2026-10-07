#!/bin/bash
# Builds the audio decoders used by the Codebot audio units as one static
# library, libcodebotaudio.
#
# The library holds minimp3 for mp3 audio, libogg and libvorbis with
# vorbisfile for ogg vorbis audio, and libxmp for tracker music. All of them
# are decoders only. The vorbis encoder is left out, as are the parts of
# libxmp which unpack compressed modules.
#
# The library is written to lib/<cpu>-<os> using the Free Pascal names for the
# cpu and os. The codebot_render package adds this folder to the library
# path of programs that use it, and the Pascal units link it statically.
#
# Pass win64 to cross compile for 64 bit Windows with MinGW-w64, which writes
# the library to lib/x86_64-win64.

set -e
cd "$(dirname "$0")"
FLAGS="-fPIC"
# libxmp is built without its depackers and ProWizard loaders, which write
# temporary files and run other programs to unpack compressed modules
XMP="-DLIBXMP_NO_DEPACKERS -DLIBXMP_NO_PROWIZARD -DHAVE_POWF=1 -DHAVE_DIRENT=1"
if [ "$1" = "win64" ]; then
  CC=${CC:-x86_64-w64-mingw32-gcc}
  AR=${AR:-x86_64-w64-mingw32-ar}
  CPU=x86_64
  OS=win64
  # Keep the objects free of stack protector and fortify calls, and use the
  # UCRT printf rather than the MinGW one, which needs the C startup code
  FLAGS="-fno-stack-protector -U_FORTIFY_SOURCE -D__USE_MINGW_ANSI_STDIO=0"
elif [ -n "$1" ]; then
  echo "Unknown target $1, the only target is win64"
  exit 1
else
  CC=${CC:-gcc}
  AR=${AR:-ar}
  CPU=$(uname -m)
  case "$CPU" in
    arm64) CPU=aarch64 ;;
    i686|i586|i486) CPU=i386 ;;
  esac
  OS=$(uname -s | tr 'A-Z' 'a-z')
fi
OUT=lib/$CPU-$OS
OBJ=$OUT/obj
rm -rf "$OBJ"
mkdir -p "$OBJ"

# Compiles the sources of one library. The first argument names the library
# and prefixes its objects, as two of the libraries have a misc.c. The second
# holds its compiler flags and the rest are its sources.
compile() {
  local NAME=$1 OPTS=$2 SRC
  shift 2
  for SRC in "$@"; do
    $CC -O2 $FLAGS -DNDEBUG -w $OPTS -c "$SRC" \
      -o "$OBJ/$NAME-$(echo "${SRC%.c}" | tr '/' '-').o"
  done
  echo "Compiled $NAME"
}

compile minimp3 "" minimp3/minimp3.c
compile ogg "-Iogg/include" ogg/src/*.c
compile vorbis "-Iogg/include -Ivorbis/include -Ivorbis/lib" vorbis/lib/*.c
# xmp/sources.txt lists the sources of libxmp in the order of its own build
compile xmp "-std=gnu90 -DLIBXMP_STATIC $XMP -Ixmp/include -Ixmp/src" \
  $(sed 's|^|xmp/|' xmp/sources.txt)

rm -f "$OUT/libcodebotaudio.a"
$AR rcs "$OUT/libcodebotaudio.a" "$OBJ"/*.o
rm -rf "$OBJ"
echo "Built $OUT/libcodebotaudio.a"
# Free Pascal links no C runtime on Windows, so copy the MinGW-w64 libraries
# the Pascal units link with libcodebotaudio.a. libkernel32.a is the import
# library for the one Windows function libxmp calls.
if [ "$OS" = "win64" ]; then
  for LIB in libmingwex.a libmingw32.a libucrt.a libgcc.a libkernel32.a; do
    cp "$($CC -print-file-name=$LIB)" "$OUT/"
  done
  echo "Copied the MinGW-w64 runtime libraries to $OUT"
fi
