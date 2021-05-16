{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit codebot_controls_design;

{$warn 5023 off : no warning about unused units}
interface

uses
  Codebot.Design.Registration, Codebot.Design.Editors, Codebot.Design.Forms, 
  Codebot.Design.ImageListEditor, Codebot.Design.SurfaceBitmapEditor, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('Codebot.Design.Registration', 
    @Codebot.Design.Registration.Register);
end;

initialization
  RegisterPackage('codebot_controls_design', @Register);
end.
