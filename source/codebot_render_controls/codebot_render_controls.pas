{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
  }

unit codebot_render_controls;

{$warn 5023 off : no warning about unused units}
interface

uses
  Codebot.Render.Controls, Codebot.Render.Controls.Gtk2,
  Codebot.Render.Controls.Gtk3, Codebot.Render.Controls.Windows,
  Codebot.Render.Scenes.Controller, Codebot.Render.Controls.Child,
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('codebot_render_controls', @Register);
end.
