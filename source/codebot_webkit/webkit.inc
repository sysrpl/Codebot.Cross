{$i ../codebot/codebot.inc}

{ The webkit package embeds WebKitGTK in a control, and the WebKitGTK library
  it uses is built for Gtk3. The entire package is restricted to the Gtk3
  widgetset, and building it with any other widgetset stops here. }

{$ifndef lclgtk3}
  {$fatal The codebot_webkit package requires the Gtk3 widgetset}
{$endif}
