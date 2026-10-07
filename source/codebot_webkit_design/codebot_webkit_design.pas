{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
  }

unit codebot_webkit_design;

{$warn 5023 off : no warning about unused units}
interface

uses
  Codebot.WebKit.Registration, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('Codebot.WebKit.Registration',
    @Codebot.WebKit.Registration.Register);
end;

initialization
  RegisterPackage('codebot_webkit_design', @Register);
end.
