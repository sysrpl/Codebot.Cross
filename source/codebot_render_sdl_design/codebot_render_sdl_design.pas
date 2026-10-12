{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
  }

unit codebot_render_sdl_design;

{$warn 5023 off : no warning about unused units}
interface

uses
  Codebot.Render.Sdl.Registration, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('Codebot.Render.Sdl.Registration',
    @Codebot.Render.Sdl.Registration.Register);
end;

initialization
  RegisterPackage('codebot_render_sdl_design', @Register);
end.
