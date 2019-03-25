{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit codebot;

{$warn 5023 off : no warning about unused units}
interface

uses
  Codebot.Constants, Codebot.Core, Codebot.System, Codebot.Collections, 
  Codebot.Interop.Windows.Direct2D, Codebot.Interop.Windows.GdiPlus, 
  Codebot.Interop.Windows.ImageCodecs, Codebot.Interop.Windows.Msxml, 
  Codebot.Interop.Linux.NetWM, Codebot.Interop.Linux.Xml2, 
  Codebot.Interop.Sockets, Codebot.Interop.OpenSSL, Codebot.Text, 
  Codebot.Cryptography, Codebot.Text.Xml, Codebot.Networking, 
  Codebot.Networking.Storage, Codebot.Networking.Ftp, Codebot.Networking.Web, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('codebot', @Register);
end.
