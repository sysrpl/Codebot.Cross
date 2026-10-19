{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
  }

unit CodebotWindowsShellDesignPackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  Codebot.Windows.Shell.Registration, Codebot.Windows.Explorer.BindingEditor,
  Codebot.Windows.Explorer.Editors, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('Codebot.Windows.Shell.Registration',
    @Codebot.Windows.Shell.Registration.Register);
end;

initialization
  RegisterPackage('codebot_windows_shell_design', @Register);
end.
