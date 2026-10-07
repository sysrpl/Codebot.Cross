(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.webkit.registration.txt> }
unit Codebot.WebKit.Registration;

{$i ../codebot_webkit/webkit.inc}

interface

uses
  Classes,
  Codebot.WebKit.Controls,
  Codebot.WebKit.Controls.Extras;

procedure Register;

implementation

{$r palette_icons.res}

procedure Register;
begin
  { Components }
  RegisterComponents('Codebot Webview', [TWebBrowser, TWebAddressBar,
    TWebStatusIndicator, TWebInspector]);
end;

end.
