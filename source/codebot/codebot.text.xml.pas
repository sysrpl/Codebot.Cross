(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified August 2019                                *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.text.xml.txt> }
unit Codebot.Text.Xml;

{$i codebot.inc}

interface

uses
  { Free pascal units }
  SysUtils, Classes,
  { Codebot units }
  Codebot.System,
  Codebot.Text,
  Codebot.Cryptography;

{$region xml interface}
{TODO: Add documentation}

type
  TNodeKind = (nkDocument, nkElement, nkAttribute, nkText, nkOther);

  INodeList = interface;
  IDocument = interface;

{ IFiler }

  IFiler = interface
    ['{3DC4CC5C-AFFC-449F-9983-11FE39194CF5}']
    function GetDocument: IDocument;
    procedure Encrypt(const Key, Value: string);
    function Decrypt(const Key: string): string;
    function ReadStr(const Key: string; const DefValue: string = ''; Stored: Boolean = False): string;
    procedure WriteStr(const Key, Value: string);
    function ReadBool(const Key: string; const DefValue: Boolean = False; Stored: Boolean = False): Boolean;
    procedure WriteBool(const Key: string; Value: Boolean);
    function ReadInt(const Key: string; const DefValue: Integer = 0; Stored: Boolean = False): Integer;
    procedure WriteInt(const Key: string; Value: Integer);
    function ReadInt64(const Key: string; const DefValue: Int64 = 0; Stored: Boolean = False): Int64;
    procedure WriteInt64(const Key: string; Value: Int64);
    function ReadFloat(const Key: string; const DefValue: Single = 0; Stored: Boolean = False): Single;
    procedure WriteFloat(const Key: string; Value: Single);
    function ReadDate(const Key: string; const DefValue: TDateTime = 0; Stored: Boolean = False): TDateTime;
    procedure WriteDate(const Key: string; Value: TDateTime);
    property Document: IDocument read GetDocument;
  end;

{ INode }

  INode = interface
    ['{BC90FD97-E83D-41BB-B4D8-3E25AA5EB2C6}']
    function GetDocument: IDocument;
    function GetParent: INode;
    function GetFiler: IFiler;
    function GetAttributes: INodeList;
    function GetNodes: INodeList;
    function GetKind: TNodeKind;
    function GetName: string;
    function GetText: string;
    procedure SetText(const Value: string);
    function GetXml: string;
    procedure SetXml(const Value: string);
    function Instance: Pointer;
    function Next: INode;
    function SelectNode(const XPath: string): INode;
    function SelectList(const XPath: string): INodeList; overload;
    function SelectList(const XPath: string; out List: INodeList): Boolean; overload;
    function Force(const Path: string): INode;
    property Document: IDocument read GetDocument;
    property Parent: INode read GetParent;
    property Filer: IFiler read GetFiler;
    property Attributes: INodeList read GetAttributes;
    property Nodes: INodeList read GetNodes;
    property Kind: TNodeKind read GetKind;
    property Name: string read GetName;
    property Text: string read GetText write SetText;
    property Xml: string read GetXml write SetXml;
  end;

{ INodeList }

  INodeList = interface(IEnumerable<INode>)
    ['{D36A2B84-D31D-4134-B878-35E8D33FD067}']
    function GetCount: Integer;
    function GetByName(const Name: string): INode; overload;
    function GetByIndex(Index: Integer): INode; overload;
    procedure Clear;
    procedure Add(Node: INode); overload;
    function Add(const Name: string): INode; overload;
    procedure Remove(Node: INode); overload;
    procedure Remove(const Name: string); overload;
    property Count: Integer read GetCount;
    property ByName[const Name: string]: INode read GetByName;
    property ByIndex[Index: Integer]: INode read GetByIndex; default;
  end;

{ IDocument }

  IDocument = interface(INode)
    ['{B713CB91-C809-440A-83D1-C42BDF806C4A}']
    procedure SetRoot(Value: INode);
    function GetRoot: INode;
    procedure Beautify;
    function CreateAttribute(const Name: string): INode;
    function CreateElement(const Name: string): INode;
    procedure Load(const FileName: string);
    procedure Save(const FileName: string);
    property Root: INode read GetRoot write SetRoot;
  end;

type
  TEncryptionFunc = function(const S: string): string;

var
  EncryptFunc: TEncryptionFunc;
  DecryptFunc: TEncryptionFunc;

{ Create a new xml document }
function DocumentCreate: IDocument;
function NewDocument: IDocument;
{ Create a new filer given a document and a node }
function FilerCreate(Document: IDocument; Node: INode): IFiler;
{$endregion}

{$region xml settings file}
{ Load a filer from the application settings file }
function SettingsLoad: IFiler;
{ Save a filer to the application settings file }
procedure SettingsSave(Filer: IFiler);
{$endregion}

{ Check if an xml is properly closed }
function XmlValidate(const Xml: string): Boolean;

implementation

{$ifdef linux}
  {$i codebot.text.xml.linux.inc}
{$endif}
{$ifdef windows}
  {$i codebot.text.xml.windows.inc}
{$endif}

{$region xml interface}
type
  TFiler = class(TInterfacedObject, IFiler)
  private
    FDocument: IDocument;
    FNode: INode;
  public
    function GetDocument: IDocument;
    procedure Encrypt(const Key, Value: string);
    function Decrypt(const Key: string): string;
    function ReadStr(const Key: string; const DefValue: string = ''; Stored: Boolean = False): string;
    procedure WriteStr(const Key, Value: string);
    function ReadBool(const Key: string; const DefValue: Boolean = False; Stored: Boolean = False): Boolean;
    procedure WriteBool(const Key: string; Value: Boolean);
    function ReadInt(const Key: string; const DefValue: Integer = 0; Stored: Boolean = False): Integer;
    procedure WriteInt(const Key: string; Value: Integer);
    function ReadInt64(const Key: string; const DefValue: Int64 = 0; Stored: Boolean = False): Int64;
    procedure WriteInt64(const Key: string; Value: Int64);
    function ReadFloat(const Key: string; const DefValue: Single = 0; Stored: Boolean = False): Single;
    procedure WriteFloat(const Key: string; Value: Single);
    function ReadDate(const Key: string; const DefValue: TDateTime = 0; Stored: Boolean = False): TDateTime;
    procedure WriteDate(const Key: string; Value: TDateTime);
  public
    constructor Create(Document: IDocument; Node: INode);
  end;

constructor TFiler.Create(Document: IDocument; Node: INode);
begin
  inherited Create;
  FDocument := Document;
  FNode := Node;
end;

function TFiler.GetDocument: IDocument;
begin
  Result := FDocument;
end;

procedure TFiler.Encrypt(const Key, Value: string);
begin
  if Assigned(EncryptFunc) then
    WriteStr(Key, EncryptFunc(Value))
end;

function TFiler.Decrypt(const Key: string): string;
begin
  if Assigned(DecryptFunc) then
    Result := DecryptFunc(ReadStr(Key))
  else
    Result := '';
end;

function TFiler.ReadStr(const Key: string; const DefValue: string = ''; Stored: Boolean = False): string;
var
  N: INode;
begin
  N := FNode.SelectNode(Key);
  if N <> nil then
  begin
    Result := N.Text;
    Exit;
  end;
  if Stored then
    WriteStr(Key, DefValue);
  Result := DefValue;
end;

procedure TFiler.WriteStr(const Key, Value: string);
var
  N: INode;
begin
  N := FNode.SelectNode(Key);
  if N = nil then
    N := FNode.Force(Key);
  if N = nil then
    Exit;
  N.Text := Value;
end;

const
  BoolStr: array[Boolean] of string = ('false', 'true');

function StrToBoolDef(S: string; DefValue: Boolean): Boolean;
begin
  S := LowerCase(Trim(S));
  Result := DefValue;
  if (S = 'true') or (S = 'y') or (S = 'yes') or (S = 't') or (S = '1') then
    Result := True
  else if (S = 'false') or (S = 'n') or (S = 'no') or (S = 'f') or (S = '0') then
    Result := False;
end;

function TFiler.ReadBool(const Key: string; const DefValue: Boolean = False; Stored: Boolean = False): Boolean;
var
  S: string;
begin
  S := ReadStr(Key, BoolStr[DefValue], Stored);
  Result := StrToBoolDef(S, DefValue);
end;

procedure TFiler.WriteBool(const Key: string; Value: Boolean);
begin
  WriteStr(Key, BoolStr[Value]);
end;

function TFiler.ReadInt(const Key: string; const DefValue: Integer = 0; Stored: Boolean = False): Integer;
var
  S: string;
begin
  S := ReadStr(Key, IntToStr(DefValue), Stored);
  Result := StrToIntDef(S, DefValue);
end;

procedure TFiler.WriteInt(const Key: string; Value: Integer);
begin
  WriteStr(Key, IntToStr(Value));
end;

function TFiler.ReadInt64(const Key: string; const DefValue: Int64 = 0; Stored: Boolean = False): Int64;
var
  S: string;
begin
  S := ReadStr(Key, IntToStr(DefValue), Stored);
  Result := StrToInt64Def(S, DefValue);
end;

procedure TFiler.WriteInt64(const Key: string; Value: Int64);
begin
  WriteStr(Key, IntToStr(Value));
end;


function TFiler.ReadFloat(const Key: string; const DefValue: Single = 0; Stored: Boolean = False): Single;
var
  S: string;
begin
  S := ReadStr(Key, FloatToStr(DefValue), Stored);
  Result := StrToFloatDef(S, DefValue);
end;

procedure TFiler.WriteFloat(const Key: string; Value: Single);
begin
  WriteStr(Key, FloatToStr(Value));
end;

function TFiler.ReadDate(const Key: string; const DefValue: TDateTime = 0; Stored: Boolean = False): TDateTime;
var
  S: string;
begin
  S := ReadStr(Key, DateTimeToStr(DefValue), Stored);
  Result := StrToDateTimeDef(S, DefValue);
end;

procedure TFiler.WriteDate(const Key: string; Value: TDateTime);
begin
  WriteStr(Key, DateTimeToStr(Value));
end;

function FilerCreate(Document: IDocument; Node: INode): IFiler;
begin
  Result := TFiler.Create(Document, Node);
end;
{$endregion}

{$region xml settings file}
const
  SSettingsFile = 'settings.xml';

function Load(const Folder: string): IFiler;
var
  D: IDocument;
  S: string;
begin
  S := PathCombine(Folder, SSettingsFile);
  D := DocumentCreate;
  D.Load(S);
  Result := D.Force('settings').Filer;
end;

function SettingsLoad: IFiler;
begin
  Result := Load(ConfigAppDir(False, True))
end;

procedure Save(const Folder: string; Filer: IFiler);
var
  S: string;
begin
  S := PathCombine(Folder, SSettingsFile);
  Filer.Document.Save(S);
end;

procedure SettingsSave(Filer: IFiler);
begin
  Save(ConfigAppDir(False, True), Filer);
end;
{$endregion}

function FilerEncrypt(const S: string): string;
begin
  Result := Base64Encode(Encrypt(S));
end;

function FilerDecrypt(const S: string): string;
begin
  Result := Decrypt(Base64Decode(S).AsString);
end;

function XmlValidate(const Xml: string): Boolean;
var
  OpenTag, CloseTag, CloseBracket: Integer;
  Closed: Boolean;
  I: Integer;
begin
  OpenTag := Xml.MatchCount('<');
  I := Xml.MatchCount('</') * 2 + Xml.MatchCount('/>');
  Closed := I > 0;
  CloseTag := I;
  I := Xml.MatchCount('?>');
  Inc(CloseTag, I);
  CloseBracket := Xml.MatchCount('>');
  Result := Closed and (OpenTag = CloseTag) and (OpenTag = CloseBracket);
end;

initialization
  EncryptFunc := FilerEncrypt;
  DecryptFunc := FilerDecrypt;
end.

