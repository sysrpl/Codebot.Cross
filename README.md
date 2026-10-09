# Codebot Cross Platform Library

This is the official git repository for the Codebot Cross library. It contains the source code and assets for a set of Free Pascal and Lazarus packages. It has been updated to work with the Lazarus GTK3 widgetset.

The official landing page for the library, with detailed information including installation, documentation, and examples, is [located here](https://cross.codebot.org). A tour of every package is in [source/packages.txt](source/packages.txt).

## Package Codebot

The Codebot package defines types and routines for general purpose use. These include items such as string and file system handling, collections, advanced graphics contexts, network sockets, animation, and much more.

## Package Codebot Controls

The Codebot Controls package defines classes and routines related to visual controls. Many of these controls are original, unique, and all make use of the advanced ISurface cross platform drawing context provided by the Codebot package. Some of the controls in this package include TContentGrid, TIndeterminateProgress, THuePicker and more. Custom forms and custom IDE designers are also included in this package.

## Package Codebot Render

The Codebot Render package provides an organized and easy to use class library for working with OpenGL and OpenGL ES, shader programming, vertex, render, and pixel buffers, as well as 2D physics, audio, and input processing. Scenes can be hosted inside an LCL form with codebot_render_controls, or in an SDL window with codebot_render_sdl.

## Package Codebot Terminal

The Codebot Terminal package defines TTerminal, a terminal emulator control which runs the user's shell inside a form.

## Package Codebot WebKit

The Codebot WebKit package defines TWebBrowser, a web browser control with an address bar and a status indicator.

## Installation

The scripts folder contains installers which set up the Free Pascal compiler, the Lazarus IDE, this library, its dependencies, and optionally the demo projects.

| Platform | Script |
| --- | --- |
| Linux | [scripts/install-linux.sh](scripts/install-linux.sh) |
| Raspberry Pi (64 bit Raspberry Pi OS) | [scripts/install-raspberrypi.sh](scripts/install-raspberrypi.sh) |
| Windows (64 bit) | [scripts/install-win.ps1](scripts/install-win.ps1) |

On Windows, run the installer in PowerShell with:

```
irm https://www.getlazarus.org/install-win.ps1 | iex
```

On Linux, run the installer as your normal user, not as root. It asks for administrator access only when packages need installing.


## Dependencies

The core codebot and codebot_controls packages need only the essential libraries. OpenGL rendering, the web browser, and the terminal each need extra libraries.

### Installing only the dependencies

If you already have Free Pascal and Lazarus, use the scripts in [scripts/dependencies](scripts/dependencies). Run them from the repository root, as your normal user.

Linux:

```
bash scripts/dependencies/install-linux.sh
```

Raspberry Pi:

```
bash scripts/dependencies/install-raspberrypi.sh
```

Windows (PowerShell):

```
powershell -ExecutionPolicy Bypass -File scripts\dependencies\install-win.ps1
```

### Linux

Essential:

| Package manager | Packages |
| --- | --- |
| apt | build-essential libgtk-3-dev libxml2-dev |
| dnf / zypper | gcc make binutils glibc-devel pkgconfig(gtk+-3.0) pkgconfig(libxml-2.0) |
| pacman | base-devel gtk3 libxml2 |

Optional features:

| Feature | Codebot packages | apt | dnf / zypper | pacman |
| --- | --- | --- | --- | --- |
| OpenGL rendering | codebot_render, codebot_render_controls, codebot_render_sdl | libsdl2-dev libsdl2-image-dev libassimp-dev | pkgconfig(sdl2) pkgconfig(SDL2_image) pkgconfig(assimp) | sdl2 sdl2_image assimp |
| Video playback | codebot_render | libmpv-dev | pkgconfig(mpv) | mpv |
| Web browser | codebot_webkit | libwebkit2gtk-4.1-dev | pkgconfig(webkit2gtk-4.1) | webkit2gtk-4.1 |
| Terminal | codebot_terminal | libvte-2.91-dev | pkgconfig(vte-2.91) | vte3 |

Without libmpv, disable video by changing `{$define videowidget}` to `{.$define videowidget}` in `source/codebot_render/render.inc`.

### Raspberry Pi

```
build-essential libsdl2-dev libsdl2-image-dev libassimp-dev libxml2-dev libmpv-dev libssl-dev
libgl1-mesa-dev libglu1-mesa-dev libegl1-mesa-dev libgles2-mesa-dev
```

### Windows

DLLs, installed into `%LOCALAPPDATA%\bin`:

| DLL | Used for |
| --- | --- |
| SDL2.dll | codebot_render: windows, OpenGL, input, audio, images |
| libassimp-5.dll | codebot_render: 3D models |
| libssl-3-x64.dll, libcrypto-3-x64.dll | codebot: secure sockets |

The terminal control (VTE) is not available on Windows.
