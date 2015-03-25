{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit codebot;

interface

uses
  Codebot.Constants, Codebot.Core, Codebot.System, Codebot.Collections, 
  Codebot.Interop.Windows.Direct2D, Codebot.Interop.Windows.GdiPlus, 
  Codebot.Interop.Windows.ImageCodecs, Codebot.Interop.Windows.Msxml, 
  Codebot.Interop.Unix.Xml2, Codebot.Interop.Sockets, Codebot.Interop.OpenSSL, 
  Codebot.Text, Codebot.Cryptography, Codebot.Text.Xml, Codebot.Networking, 
  Codebot.Networking.Storage, Codebot.Networking.Ftp, Codebot.Networking.Web, 
  Codebot.Graphics.Windows.ImageBitmap, 
  Codebot.Graphics.Windows.InterfacedBitmap, Codebot.Graphics, 
  Codebot.Graphics.Types, Codebot.Graphics.Windows.SurfaceGdiPlus, 
  Codebot.Graphics.Windows.SurfaceD2D, Codebot.Graphics.Linux.SurfaceCairo, 
  Codebot.Controls.Extras, Codebot.Controls.Scrolling, 
  Codebot.Controls.Sliders, Codebot.Forms.Popup, Codebot.Input.MouseMonitor, 
  Codebot.Controls, Codebot.Controls.Colors, Codebot.Controls.Edits, 
  Codebot.Controls.Banner, Codebot.Controls.Grids, 
  Codebot.Design.ImageListEditor, Codebot.Design.SurfaceBitmapEditor, 
  Codebot.Controls.Buttons, Codebot.Graphics.Markup, 
  Codebot.Controls.Containers, Codebot.Debug, Codebot.Animation, 
  Codebot.Geometry, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('codebot', @Register);
end.
