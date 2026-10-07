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
{ TNodeKind identifies the type of an xml node }

type
  TNodeKind = (nkDocument, nkElement, nkAttribute, nkText, nkOther);

  {doc off}
  INodeList = interface;
  IDocument = interface;
  {doc on}

{ IFiler reads and writes typed values to xml nodes selected by an xpath key.
  Read methods return a default value when the key is not found, and when
  Stored is true the default value is also written to the document.
  See also
  <link Overview.Codebot.Text.Xml.IFiler, IFiler members> }

  IFiler = interface
    ['{3DC4CC5C-AFFC-449F-9983-11FE39194CF5}']
    {doc off}
    function GetDocument: IDocument;
    {doc on}
    { Write an encrypted string value }
    procedure Encrypt(const Key, Value: string);
    { Read and decrypt a string value }
    function Decrypt(const Key: string): string;
    { Read a string value }
    function ReadStr(const Key: string; const DefValue: string = ''; Stored: Boolean = False): string;
    { Write a string value }
    procedure WriteStr(const Key, Value: string);
    { Read a boolean value }
    function ReadBool(const Key: string; const DefValue: Boolean = False; Stored: Boolean = False): Boolean;
    { Write a boolean value }
    procedure WriteBool(const Key: string; Value: Boolean);
    { Read an integer value }
    function ReadInt(const Key: string; const DefValue: Integer = 0; Stored: Boolean = False): Integer;
    { Write an integer value }
    procedure WriteInt(const Key: string; Value: Integer);
    { Read a 64 bit integer value }
    function ReadInt64(const Key: string; const DefValue: Int64 = 0; Stored: Boolean = False): Int64;
    { Write a 64 bit integer value }
    procedure WriteInt64(const Key: string; Value: Int64);
    { Read a float value }
    function ReadFloat(const Key: string; const DefValue: Single = 0; Stored: Boolean = False): Single;
    { Write a float value }
    procedure WriteFloat(const Key: string; Value: Single);
    { Read a date value }
    function ReadDate(const Key: string; const DefValue: TDateTime = 0; Stored: Boolean = False): TDateTime;
    { Write a date value }
    procedure WriteDate(const Key: string; Value: TDateTime);
    { The document the filer writes to }
    property Document: IDocument read GetDocument;
  end;

{ INode is an element, attribute, text, or document node in an xml tree
  See also
  <link Overview.Codebot.Text.Xml.INode, INode members> }

  INode = interface
    ['{BC90FD97-E83D-41BB-B4D8-3E25AA5EB2C6}']
    {doc off}
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
    {doc on}
    { The underlying platform node object }
    function Instance: Pointer;
    { The next sibling node or nil if there is none }
    function Next: INode;
    { Select the first node matching an xpath or nil if there is no match }
    function SelectNode(const XPath: string): INode;
    { Select a list of nodes matching an xpath }
    function SelectList(const XPath: string): INodeList; overload;
    { Select a list of nodes matching an xpath returning true if any matched }
    function SelectList(const XPath: string; out List: INodeList): Boolean; overload;
    { Return the node at a path creating any missing elements along the way }
    function Force(const Path: string): INode;
    { The document which owns the node }
    property Document: IDocument read GetDocument;
    { The parent node or nil if there is none }
    property Parent: INode read GetParent;
    { A filer which reads and writes values relative to this node }
    property Filer: IFiler read GetFiler;
    { The attributes of an element }
    property Attributes: INodeList read GetAttributes;
    { The child nodes }
    property Nodes: INodeList read GetNodes;
    { The type of node }
    property Kind: TNodeKind read GetKind;
    { The name of the node }
    property Name: string read GetName;
    { The text content of the node }
    property Text: string read GetText write SetText;
    { The node and its children as xml }
    property Xml: string read GetXml write SetXml;
  end;

{ INodeList is a list of attributes or child nodes
  See also
  <link Overview.Codebot.Text.Xml.INodeList, INodeList members> }

  INodeList = interface(IEnumerable<INode>)
    ['{D36A2B84-D31D-4134-B878-35E8D33FD067}']
    {doc off}
    function GetCount: Integer;
    function GetByName(const Name: string): INode; overload;
    function GetByIndex(Index: Integer): INode; overload;
    {doc on}
    { Remove all nodes from the list }
    procedure Clear;
    { Add a node to the list }
    procedure Add(Node: INode); overload;
    { Add a new node by name returning the node }
    function Add(const Name: string): INode; overload;
    { Remove a node from the list }
    procedure Remove(Node: INode); overload;
    { Remove a node by name from the list }
    procedure Remove(const Name: string); overload;
    { The number of nodes in the list }
    property Count: Integer read GetCount;
    { Nodes indexed by name }
    property ByName[const Name: string]: INode read GetByName;
    { Nodes indexed by an integer }
    property ByIndex[Index: Integer]: INode read GetByIndex; default;
  end;

{ IDocument is the root of an xml tree
  See also
  <link Overview.Codebot.Text.Xml.IDocument, IDocument members> }

  IDocument = interface(INode)
    ['{B713CB91-C809-440A-83D1-C42BDF806C4A}']
    {doc off}
    procedure SetRoot(Value: INode);
    function GetRoot: INode;
    {doc on}
    { Format the document with indentation }
    procedure Beautify;
    { Create a new attribute node owned by the document }
    function CreateAttribute(const Name: string): INode;
    { Create a new element node owned by the document }
    function CreateElement(const Name: string): INode;
    { Load the document from a file }
    procedure Load(const FileName: string);
    { Save the document to a file }
    procedure Save(const FileName: string);
    { The root element of the document }
    property Root: INode read GetRoot write SetRoot;
  end;

{ TEncryptionFunc is used by IFiler to encrypt and decrypt values }

type
  TEncryptionFunc = function(const S: string): string;

{ The functions used by IFiler.Encrypt and IFiler.Decrypt }

var
  EncryptFunc: TEncryptionFunc;
  DecryptFunc: TEncryptionFunc;

{ Create a new xml document }
function DocumentCreate: IDocument;
{ Create a new xml document, same as DocumentCreate }
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

{ Check if an xml string is well formed }
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

