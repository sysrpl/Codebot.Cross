{$i ../codebot/codebot.inc}

{ The webkit package embeds a web view in a control. On Linux it uses
  WebKitGTK, which is built for Gtk3, on Windows it uses Microsoft Edge
  WebView2, and on macOS it uses the WebKit framework of the system. The
  package is restricted to the Gtk3, Win32, and Cocoa widgetsets, and building
  it with any other widgetset stops here. }

{$if not (defined(lclgtk3) or defined(lclwin32) or defined(lclcocoa))}
  {$fatal The codebot_webkit package requires the Gtk3, Win32, or Cocoa widgetset}
{$endif}
