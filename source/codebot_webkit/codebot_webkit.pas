{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
  }

unit codebot_webkit;

{$warn 5023 off : no warning about unused units}
interface

uses
  Codebot.Interop.WebKit, Codebot.WebKit.Controls,
  Codebot.WebKit.Controls.Gtk3, Codebot.WebKit.Controls.Extras,
  Codebot.Interop.WebView2, Codebot.WebKit.Controls.Win, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('codebot_webkit', @Register);
end.
