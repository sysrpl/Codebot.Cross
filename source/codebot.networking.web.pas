(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified March 2015                                 *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.networking.web.txt> }
unit Codebot.Networking.Web;

{$i codebot.inc}

interface

uses
  Classes,
  SysUtils,
  Codebot.System,
  Codebot.Text,
  Codebot.Text.Xml,
  Codebot.Networking;

type
  TResponseHeader = record
    Code: Integer;
    Status: string;
    Keys:  TNamedStrings;
    procedure Reset;
    function Extract(var Response: string): Boolean;
  end;

implementation

procedure TResponseHeader.Reset;
begin
  Code := 0;
  Status.Length := 0;
  Keys.Clear;
end;

function TResponseHeader.Extract(var Response: string): Boolean;
const
  Breaks: array[0..3] of string = (#10#10, #13#10#13#10, #13#13, #10#13#10#13);
var
  First, Index: Integer;
  I, J: Integer;
  Lines, Row: StringArray;
begin
  Result := False;
  if Code > 0 then
    Exit;
  First := -1;
  Index := -1;
  for I := Low(Breaks) to High(Breaks) do
  begin
    J := Response.IndexOf(Breaks[I]);
    if J < 1 then
      Continue;
    if (First < 0) or (J < First) then
    begin
      First := J;
      Index := I;
    end;
  end;
  if Index > -1 then
  begin
    Lines := Response.FirstOf(Breaks[Index]).Split(#10);
    for I := Lines.Lo to Lines.Hi do
      if I = 0 then
      begin
        Row := Lines[I].Words;
        if Row.Length > 1 then
          Code := StrToIntDef(Row[1], 0);
        if Row.Length > 2 then
          Status := Row[2];
      end
      else
        Keys.Add(Lines[I].FirstOf(':').Trim, Lines[I].SecondOf(':').Trim);
    Response := Response.SecondOf(Breaks[Index]);
    Result := True;
  end;
end;

end.

