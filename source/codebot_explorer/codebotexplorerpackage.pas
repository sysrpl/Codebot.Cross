{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit CodebotExplorerPackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  Codebot.Explorer.Registration, Codebot.Design.AppExplorer,
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('Codebot.Explorer.Registration',
    @Codebot.Explorer.Registration.Register);
end;

initialization
  RegisterPackage('codebot_explorer', @Register);
end.
