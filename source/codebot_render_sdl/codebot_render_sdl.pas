{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
  }

unit codebot_render_sdl;

{$warn 5023 off : no warning about unused units}
interface

uses
  Codebot.Render.Application, Codebot.Platform.SDL, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('codebot_render_sdl', @Register);
end.
