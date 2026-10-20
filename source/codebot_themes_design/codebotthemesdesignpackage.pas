{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit CodebotThemesDesignPackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  Codebot.Themes.Registration, Codebot.Themes.Palettes,
  Codebot.Themes.Manager, Codebot.Themes.Editor, Codebot.Themes.Dialog,
  Codebot.Themes.Windows.DarkMode, Codebot.Themes.Windows.Colors,
  Codebot.Themes.Windows.Hooks, Codebot.Themes.Gtk3, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('Codebot.Themes.Registration',
    @Codebot.Themes.Registration.Register);
end;

initialization
  RegisterPackage('codebot_themes_design', @Register);
end.
