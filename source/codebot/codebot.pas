{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit codebot;

{$warn 5023 off : no warning about unused units}
interface

uses
  Codebot.Animation, Codebot.Collections, Codebot.Constants, Codebot.Core, 
  Codebot.Cryptography, Codebot.Geometry, 
  Codebot.Platform, Codebot.Graphics.Types, Codebot.Interop.Linux.Xml2, 
  Codebot.Interop.OpenSSL, Codebot.Interop.Sockets, 
  Codebot.Interop.Windows.Direct2D, Codebot.Interop.Windows.GdiPlus, 
  Codebot.Interop.Windows.ImageCodecs, Codebot.Interop.Windows.Msxml, 
  Codebot.Networking.Ftp, Codebot.Networking.Storage, Codebot.Networking.Unix, 
  Codebot.Networking.Web, Codebot.System, Codebot.Support, 
  Codebot.Text.Formats, Codebot.Text.Json, Codebot.Text, Codebot.Text.Xml, 
  Codebot.Text.Store, Codebot.IO.SerialPort, Codebot.Unique, Codebot.Process, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('codebot', @Register);
end.
