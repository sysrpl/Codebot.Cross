{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit codebot;

interface

uses
  Codebot.Constants, Codebot.Core, Codebot.System, Codebot.Collections, 
  Codebot.Interop.Windows.Direct2D, Codebot.Interop.Windows.GdiPlus, 
  Codebot.Interop.Windows.ImageCodecs, Codebot.Interop.Windows.Msxml, 
  Codebot.Interop.Linux.NetWM, Codebot.Interop.Linux.Xml2, 
  Codebot.Interop.Sockets, Codebot.Interop.OpenSSL, Codebot.Text, 
  Codebot.Cryptography, Codebot.Text.Xml, Codebot.Networking, 
  Codebot.Networking.Storage, Codebot.Networking.Ftp, Codebot.Networking.Web, 
  Codebot.Forms.Management, Codebot.Forms.Floating, Codebot.Forms.Popup, 
  Codebot.Forms.Widget, Codebot.Graphics.Windows.ImageBitmap, 
  Codebot.Graphics.Windows.InterfacedBitmap, Codebot.Graphics, 
  Codebot.Graphics.Extras, Codebot.Graphics.Types, 
  Codebot.Graphics.Windows.SurfaceGdiPlus, 
  Codebot.Graphics.Windows.SurfaceD2D, Codebot.Graphics.Linux.SurfaceCairo, 
  Codebot.Controls.Tooltips, Codebot.Controls.Extras, 
  Codebot.Controls.Scrolling, Codebot.Controls.Sliders, Codebot.Input.Hotkeys, 
  Codebot.Input.MouseMonitor, Codebot.Controls, Codebot.Controls.Colors, 
  Codebot.Controls.Edits, Codebot.Controls.Banner, Codebot.Controls.Grids, 
  Codebot.Design.ImageListEditor, Codebot.Design.SurfaceBitmapEditor, 
  Codebot.Controls.Buttons, Codebot.Graphics.Markup, 
  Codebot.Controls.Containers, Codebot.Controls.Highlighter, 
  Codebot.Animation, Codebot.Geometry, Codebot.Debug, Codebot.Unique, 
  Codebot.Files, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('codebot', @Register);
end.
