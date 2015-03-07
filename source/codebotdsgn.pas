{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit codebotdsgn;

interface

uses
  Codebot.Design.Registration, Codebot.Design.Editors, Codebot.Design.Forms, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('Codebot.Design.Registration', 
    @Codebot.Design.Registration.Register);
end;

initialization
  RegisterPackage('codebotdsgn', @Register);
end.
