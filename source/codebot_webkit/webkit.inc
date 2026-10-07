{$i ../codebot/codebot.inc}

{ The webkit package embeds a web view in a control. On Linux it uses
  WebKitGTK, which is built for Gtk3, and on Windows it uses Microsoft Edge
  WebView2. The package is restricted to the Gtk3 and Win32 widgetsets, and
  building it with any other widgetset stops here. }

{$if not (defined(lclgtk3) or defined(lclwin32))}
  {$fatal The codebot_webkit package requires the Gtk3 or Win32 widgetset}
{$endif}
