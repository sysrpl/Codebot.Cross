{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit codebot_rendering;

{$warn 5023 off : no warning about unused units}
interface

uses
  Codebot.Render.Contexts, Codebot.Render.Shaders, Codebot.Render.Scenes, 
  Codebot.Render.Scenes.Controller, Codebot.Render.Buffers, Codebot.GLES, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('codebot_rendering', @Register);
end.
