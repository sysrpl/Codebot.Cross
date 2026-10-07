(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.render.registration.txt> }
unit Codebot.Render.Registration;

{$i ../codebot/codebot.inc}

interface

uses
  Classes,
  Codebot.Render.Controls;

procedure Register;

implementation

{$r palette_icons.res}

procedure Register;
begin
  { Components }
  RegisterComponents('Codebot Render', [TGraphicsBox]);
end;

end.
