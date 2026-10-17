{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit CodebotFrameDesignPackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  Codebot.Frame.Registration, Codebot.Frame.ConvertDialog,
  Codebot.Frame.Packages, Codebot.Frame.Lfm, Codebot.Frame.Source,
  Codebot.Frame.Resources, Codebot.Frame.Converter, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('Codebot.Frame.Registration',
    @Codebot.Frame.Registration.Register);
end;

initialization
  RegisterPackage('codebot_frame_design', @Register);
end.
