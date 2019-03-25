{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit codebotctrls;

{$warn 5023 off : no warning about unused units}
interface

uses
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
  Codebot.Files, Codebot.Networking.Unix, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('codebotctrls', @Register);
end.
