{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
  }

unit codebot_render_design;

{$warn 5023 off : no warning about unused units}
interface

uses
  Codebot.Render.Registration, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('Codebot.Render.Registration',
    @Codebot.Render.Registration.Register);
end;

initialization
  RegisterPackage('codebot_render_design', @Register);
end.
