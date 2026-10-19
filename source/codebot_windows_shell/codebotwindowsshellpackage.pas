{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit CodebotWindowsShellPackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  Codebot.Interop.Windows.Shell, Codebot.Windows.Explorer,
  Codebot.Windows.Shell.Tools, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('codebot_windows_shell', @Register);
end.
