{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit codebot_controls;

{$warn 5023 off : no warning about unused units}
interface

uses
  Codebot.Controls.Banner, Codebot.Controls.Buttons, Codebot.Controls.Colors, 
  Codebot.Controls.Containers, Codebot.Controls.Edits, Codebot.Controls.Grids, 
  Codebot.Controls.Highlighter, Codebot.Controls, Codebot.Controls.Scrolling, 
  Codebot.Controls.Sliders, Codebot.Controls.Tooltips, Codebot.Debug, 
  Codebot.Forms.Floating, Codebot.Forms.Management, Codebot.Forms.Popup, 
  Codebot.Forms.Widget, Codebot.Input.Hotkeys, Codebot.Input.MouseMonitor, 
  Codebot.Interop.Linux.NetWM, Codebot.Unique, Codebot.Controls.Extras, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('codebot_controls', @Register);
end.
