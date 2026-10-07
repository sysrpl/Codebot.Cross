(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ Codebot.Terminal.Types holds what the terminal control shares with the
  units which implement it for each widgetset. Those units cannot use the unit
  of the control, as the two would then depend on each other. }

unit Codebot.Terminal.Types;

{$i terminal.inc}

interface

{ TTerminalElement is a part of a terminal which has a color of its own.
  teHighlight is the color behind selected text and teHighlightText is the
  color of the selected text itself. }

type
  TTerminalElement = (teFore, teBack, teBold, teDim, teCursor, teHighlight,
    teHighlightText);

{ TTerminalPalette chooses the sixteen colors programs in a terminal ask for
  by number, such as the colors of file names listed by ls. They are black,
  red, green, yellow, blue, magenta, cyan, and white, followed by a brighter
  one of each.

  tpDefault leaves them to the VTE library. tpTango is the palette of the
  gnome terminal, tpLinux of the Linux console, and tpXterm of xterm.
  tpSolarized is the Solarized palette. }

  TTerminalPalette = (tpDefault, tpTango, tpLinux, tpXterm, tpSolarized);

  { The sixteen colors of a palette as $RRGGBB values }
  TTerminalPaletteColors = array[0..15] of LongWord;

const
  TerminalPalettes: array[tpTango..tpSolarized] of TTerminalPaletteColors = (
    (
    $2E3436, $CC0000, $4E9A06, $C4A000, $3465A4, $75507B, $06989A, $D3D7CF,
    $555753, $EF2929, $8AE234, $FCE94F, $729FCF, $AD7FA8, $34E2E2, $EEEEEC
    ), (
    $000000, $AA0000, $00AA00, $AA5500, $0000AA, $AA00AA, $00AAAA, $AAAAAA,
    $555555, $FF5555, $55FF55, $FFFF55, $5555FF, $FF55FF, $55FFFF, $FFFFFF
    ), (
    $000000, $CD0000, $00CD00, $CDCD00, $0000EE, $CD00CD, $00CDCD, $E5E5E5,
    $7F7F7F, $FF0000, $00FF00, $FFFF00, $5C5CFF, $FF00FF, $00FFFF, $FFFFFF
    ), (
    $073642, $DC322F, $859900, $B58900, $268BD2, $D33682, $2AA198, $EEE8D5,
    $002B36, $CB4B16, $586E75, $657B83, $839496, $6C71C4, $93A1A1, $FDF6E3
    ));

{ ITerminalEvents is used to notify a terminal control of changes in its
  terminal widget }

type
  ITerminalEvents = interface
  ['{3D7B1F64-8E2A-4C95-B0D3-5A6E9C1F7248}']
    { The terminal has shown its first output }
    procedure TerminalReady;
    { The program running in the terminal has ended }
    procedure TerminalExited;
  end;

implementation

end.
