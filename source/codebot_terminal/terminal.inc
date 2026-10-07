{$i ../codebot/codebot.inc}

{ The terminal package embeds a terminal emulator in a control using the VTE
  library, which is a Gtk widget, and the library is linked when the package
  is built. The entire package is restricted to the Gtk2 and Gtk3 widgetsets,
  and building it with any other widgetset stops here.

  The libvte-2.91-dev package must be installed for Gtk3, or the libvte-dev
  package for Gtk2. }

{$ifdef lclgtk3}
  {$define terminalgtk3}
{$else}
  {$ifdef lclgtk2}
    {$define terminalgtk2}
  {$else}
    {$fatal The codebot_terminal package requires the Gtk2 or Gtk3 widgetset}
  {$endif}
{$endif}
