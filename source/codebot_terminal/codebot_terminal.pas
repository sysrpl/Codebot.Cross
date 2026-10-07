{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit codebot_terminal;

{$warn 5023 off : no warning about unused units}
interface

uses
  Codebot.Terminal.Types, Codebot.Terminal.Controls,
  Codebot.Terminal.Controls.Gtk3, Codebot.Terminal.Controls.Gtk2,
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('codebot_terminal', @Register);
end.
