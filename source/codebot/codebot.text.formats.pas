(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified September 2019                             *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.text.formats.txt> }
unit Codebot.Text.Formats;

{$i codebot.inc}

interface

uses
  Codebot.System;

{$region textformats}
{ Attempt to translate a json string to an xml string [group textformats] }
(*function JsonToXml(const Json: string): string;

{ Attempt to translate an xml string to an json string [group textformats] }
function XmlToJson(const Xml: string): string;

{ Perform a quick test to determine if a string contains json data [group textformats] }
function IsJson(const S: string): Boolean;

{ Perform a quick test to determine if a string contains xml data [group textformats] }
function IsXml(const S: string): Boolean;*)
{$endregion}

implementation

{$region formats}
(*function JsonEnumData(Data: TJSONData; Level: Integer): string;
var
  Item: TJSONEnum;
  K, S: string;
begin
  Result := '';
  S := '';
  SetLength(S, Level);
  FillChar(S[1], Level, ' ');
  for Item in Data do
  begin
    K := Item.Key;
    if K[1] in ['0'..'9'] then
      K := 'item' + K;
    if Item.Value.JSONType in [jtArray, jtObject] then
    begin
      if Item.Value.JSONType = jtArray then
        Result := Result + S + '<' + K + ' type="array">'#10
      else
        Result := Result + S + '<' + K + ' type="object">'#10;
      Result := Result + JsonEnumData(Item.Value, Level + 2);
      Result := Result + S + '</' + K + '>'#10;
    end
    else
    case Item.Value.JSONType of
      jtNull: Result := Result + S + '<' + K + ' type="null"/>'#10;
      jtBoolean: Result := Result + S + '<' + K + ' type="bool">' + Item.Value.AsString + '</' + K + '>'#10;
      jtNumber: Result := Result + S + '<' + K + ' type="number">' + Item.Value.AsString + '</' + K + '>'#10;
    else
      Result := Result + S + '<' + K + '>' + Item.Value.AsString + '</' + k + '>'#10;
    end;
  end;
end;

function JsonToXml(const Json: string): string;
const
  Header = '<?xml version="1.0" encoding="UTF-8"?>'#10;
var
  P: TJSONParser;
  D: TJSONData;
  S: string;
begin
  try
    P := TJSONParser.Create(Json, [joUTF8]);
    try
      D := P.Parse;
      if D.JSONType = jtArray then
        S := 'array'
      else
        S := 'object';
      Result := Header + '<' + S + '>'#10 + JsonEnumData(D, 2) + '</' + S + '>';
    finally
      P.Free;
    end;
  except
    Result := '';
  end;
end;

function XmlEnumObject(N: INode; Level: Integer; IsArray: Boolean): string;
var
  List: INodeList;
  Item: INode;
  K, S, Prefix: string;
  I, J: Integer;
begin
  Result := '';
  S := '';
  SetLength(S, Level);
  FillChar(S[1], Level, ' ');
  List := N.Nodes;
  J := List.Count - 1;
  for I := 0 to J do
  begin
    Item := List[I];
    K := Item.Filer.ReadStr('@type');
    if IsArray then
      Prefix := S
    else
      Prefix := S + '"' + Item.Name + '": ';
    if Item.Nodes.Count > 0 then
    begin
      if K = 'array' then
      begin
        Result := Result + Prefix + '['#10 + XmlEnumObject(Item, Level + 2, True);
        Result := Result + S + ']'
      end
      else
      begin
        Result := Result + Prefix + '{'#10 + XmlEnumObject(Item, Level + 2, False);
        Result := Result + S + '}'
      end;
    end
    else
    begin
      if K = 'array' then
        Result := Result + Prefix + '[ ]'
      else if K = 'object' then
        Result := Result + Prefix + '{ }'
      else if K = 'null' then
        Result := Result + Prefix + 'null'
      else if K = '' then
        Result := Result + Prefix + '"' + Item.Text + '"'
      else
        Result := Result + Prefix + Item.Text;
    end;
    if I = J then
      Result := Result + #10
    else
      Result := Result + ','#10
  end;
end;

function XmlToJson(const Xml: string): string;
var
  D: IDocument;
  R: INode;
  S: string;
begin
  D := DocumentCreate;
  D.Text := Xml;
  R := D.Root;
  if R <> nil then
  begin
    S := D.Root.Name;
    if S = 'array' then
      Result := '['#10 +  XmlEnumObject(R, 2, True) + ']'
    else
      Result := '{'#10 +  XmlEnumObject(R, 2, False) + '}';
  end
  else
    Result := '';
end;

function IsJson(const S: string): Boolean;
var
  P: PChar;
begin
  Result := False;
  P := PChar(S);
  while P[0] > #0 do
  begin
    if P[0] <= ' ' then
    begin
      Inc(P);
      Continue;
    end;
    Exit(P[0] in ['[', '{']);
  end;
end;

function IsXml(const S: string): Boolean;
var
  P: PChar;
begin
  Result := False;
  P := PChar(S);
  while P[0] > #0 do
  begin
    if P[0] <= ' ' then
    begin
      Inc(P);
      Continue;
    end;
    Exit(P[0] = '<');
  end;
end;*)
{$endregion}

end.
