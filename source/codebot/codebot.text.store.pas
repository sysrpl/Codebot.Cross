(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified September 2021                             *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.text.store.txt> }
unit Codebot.Text.Store;

{$i codebot.inc}

interface

uses
  Codebot.System,
  Codebot.Text.Xml,
  Codebot.Text.Json,
  Classes;

{ TDataTextFormat determines how TTextStorage.Data is interpreted }

type
  TDataTextFormat = (dfNone, dfJson, dfXml);

{ TTextItem is a named block of text stored by TTextStorage }

  TTextItem = class(TCollectionItem)
  private
    FName: string;
    FText: TStrings;
    procedure SetText(Value: TStrings);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    { Copy the name and text from another text item }
    procedure Assign(Source: TPersistent); override;
  published
    { The name used to find the item }
    property Name: string read FName write FName;
    { The lines of text stored by the item }
    property Text: TStrings read FText write SetText;
  end;

{ TTextItems is a collection of named blocks of text }

  TTextItems = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TTextItem;
  public
    constructor Create(AOwner: TPersistent);
    { Add a new empty item }
    function Add: TTextItem;
    { Find an item by name ignoring case, returning nil if there is no match }
    function Find(const Name: string): TTextItem;
    { Items indexed by an integer }
    property Items[Index: Integer]: TTextItem read GetItem; default;
  end;

{ TTextStorage stores text at design time. Data holds a single block of text
  which can be treated as json or xml. Items holds any number of named blocks
  of text which can be read or written by name using Values. }

  TTextStorage = class(TComponent)
  private
    FDataFormat: TDataTextFormat;
    FData: TStrings;
    FJson: TJsonNode;
    FXml: IDocument;
    FJsonChanged: Boolean;
    FXmlChanged: Boolean;
    FUseAppConfig: Boolean;
    FItems: TTextItems;
    function GetValue(const Name: string): string;
    procedure SetValue(const Name: string; const Value: string);
    procedure SetItems(Value: TTextItems);
    procedure SetData(Value: TStrings);
    procedure SetDataFormat(Value: TDataTextFormat);
    procedure TextChanged(Sender: TObject);
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Load Data from a file named after the component in the application
      config directory }
    procedure LoadAppConfig;
    { Commit and save Data to a file named after the component in the
      application config directory }
    procedure SaveAppConfig;
    { Write changes made through AsJson or AsXml back into Data }
    procedure Commit;
    { Discard changes made through AsJson or AsXml so they are reparsed from Data }
    procedure Restore;
    { Parse Data as json, setting DataFormat to dfJson }
    function AsJson: TJsonNode;
    { Parse Data as xml, setting DataFormat to dfXml }
    function AsXml: IDocument;
    { Values reads the text of a named item, raising an exception if there
      is no item with that name. Writing adds the item if it does not exist. }
    property Values[Name: string]: string read GetValue write SetValue; default;
  published
    { Data holds a single block of text treated as json or xml }
    property Data: TStrings read FData write SetData;
    { Items holds named blocks of text }
    property Items: TTextItems read FItems write SetItems;
    { The format of Data }
    property DataFormat: TDataTextFormat read FDataFormat write SetDataFormat;
    { When true Data is loaded from the application config directory at runtime }
    property UseAppConfig: Boolean read FUseAppConfig write FUseAppConfig;
  end;

implementation

uses
  SysUtils;

{ TTextItem }

constructor TTextItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FText := TStringList.Create;
end;

destructor TTextItem.Destroy;
begin
  FText.Free;
  inherited Destroy;
end;

procedure TTextItem.Assign(Source: TPersistent);
begin
  if Source is TTextItem then
  begin
    FName := TTextItem(Source).FName;
    FText.Assign(TTextItem(Source).FText);
  end
  else
    inherited Assign(Source);
end;

function TTextItem.GetDisplayName: string;
begin
  if FName <> '' then
    Result := FName
  else
    Result := inherited GetDisplayName;
end;

procedure TTextItem.SetText(Value: TStrings);
begin
  if Value <> FText then
    FText.Assign(Value);
end;

{ TTextItems }

constructor TTextItems.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TTextItem);
end;

function TTextItems.GetItem(Index: Integer): TTextItem;
begin
  Result := TTextItem(inherited Items[Index]);
end;

function TTextItems.Add: TTextItem;
begin
  Result := TTextItem(inherited Add);
end;

function TTextItems.Find(const Name: string): TTextItem;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := GetItem(I);
    if SameText(Result.Name, Name) then
      Exit;
  end;
  Result := nil;
end;

{ TTextStorage }

constructor TTextStorage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataFormat := dfNone;
  FJsonChanged := True;
  FXmlChanged := True;
  FData := TStringList.Create;
  TStringList(FData).OnChange := TextChanged;
  FItems := TTextItems.Create(Self);
end;

destructor TTextStorage.Destroy;
begin
  if UseAppConfig then
    SaveAppConfig;
  FJSon.Free;
  FItems.Free;
  FData.Free;
  inherited Destroy;
end;

procedure TTextStorage.Loaded;
begin
  inherited Loaded;
  if UseAppConfig then
    LoadAppConfig;
end;

const
  DataExt: array[TDataTextFormat] of string = ('', '.json', '.xml');

procedure TTextStorage.LoadAppConfig;
var
  FileName: string;
begin
  if FDataFormat = dfNone then
    Exit;
  FileName := Name;
  FileName := FileName.Trim.ToLower;
  if FileName = '' then
    Exit;
  FileName := PathCombine(ConfigAppDir(False, False), FileName) + DataExt[FDataFormat];
  if FileExists(FileName) then
    FData.LoadFromFile(FileName);
end;

procedure TTextStorage.SaveAppConfig;
var
  FileName: string;
begin
  if FDataFormat = dfNone then
    Exit;
  FileName := Name;
  FileName := FileName.Trim.ToLower;
  if FileName = '' then
    Exit;
  Commit;
  FileName := PathCombine(ConfigAppDir(False, True), FileName) + DataExt[FDataFormat];
  FData.SaveToFile(FileName);
end;

procedure TTextStorage.Commit;
begin
  TStringList(FData).OnChange := nil;
  if FDataFormat = dfJson then
    FData.Text := AsJson.Value
  else if FDataFormat = dfXml then
  begin
    AsXml.Beautify;
    FData.Text := AsXml.Text;
  end;
  FJsonChanged := False;
  FXmlChanged := False;
  TStringList(FData).OnChange  := TextChanged;
end;

procedure TTextStorage.Restore;
begin
  FJsonChanged := True;
  FXmlChanged := True;
end;

function TTextStorage.AsJson: TJsonNode;
var
  S: string;
begin
  FDataFormat := dfJson;
  if FJson = nil then
    FJson := TJsonNode.Create;
  if FJsonChanged then
  begin
    FJsonChanged := False;
    FXmlChanged := True;
    S := FData.Text.Trim;
    if S <> '' then
      FJson.Parse(S)
    else
      FJson.Parse('{ }');
    if FXml <> nil then
      FXml.Nodes.Clear;
  end;
  Result := FJson;
end;

function TTextStorage.AsXml: IDocument;
var
  S: string;
begin
  FDataFormat := dfXml;
  if FXml = nil then
    FXml := DocumentCreate;
  if FXmlChanged then
  begin
    FXmlChanged := False;
    FJsonChanged := True;
    S := FData.Text.Trim;
    if S <> '' then
      FXml.Nodes.Clear
    else
      FXml.Xml := S;
    if FJson <> nil then
      FJson.Parse('{ }');
  end;
  Result := FXml;
end;

function TTextStorage.GetValue(const Name: string): string;
var
  Item: TTextItem;
begin
  Item := FItems.Find(Name);
  if Item = nil then
    raise EListError.CreateFmt('Text item ''%s'' not found in %s', [Name, Self.Name]);
  Result := Item.Text.Text;
end;

procedure TTextStorage.SetValue(const Name: string; const Value: string);
var
  Item: TTextItem;
begin
  Item := FItems.Find(Name);
  if Item = nil then
  begin
    Item := FItems.Add;
    Item.Name := Name;
  end;
  Item.Text.Text := Value;
end;

procedure TTextStorage.SetItems(Value: TTextItems);
begin
  if Value <> FItems then
    FItems.Assign(Value);
end;

procedure TTextStorage.SetData(Value: TStrings);
begin
  if Value = FData then
    Exit;
  FData.Assign(Value);
end;

procedure TTextStorage.SetDataFormat(Value: TDataTextFormat);
begin
  if Value = FDataFormat then
    Exit;
  FDataFormat := Value;
  TextChanged(nil);
end;

procedure TTextStorage.TextChanged(Sender: TObject);
begin
  if csLoading in ComponentState then
    Exit;
  FJsonChanged := True;
  FXmlChanged := True;
  if FJson <> nil then
    FJson.Parse('{ }');
  if FXml <> nil then
    FXml.Nodes.Clear;
end;

end.

