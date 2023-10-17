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

{ TDataTextFormat }

type
  TDataTextFormat = (dfNone, dfJson, dfXml);

{ TTextStorage }

  TTextStorage = class(TComponent)
  private
    FDataFormat: TDataTextFormat;
    FData: TStrings;
    FJson: TJsonNode;
    FXml: IDocument;
    FJsonChanged: Boolean;
    FXmlChanged: Boolean;
    FUseAppConfig: Boolean;
    procedure SetData(Value: TStrings);
    procedure SetDataFormat(Value: TDataTextFormat);
    procedure TextChanged(Sender: TObject);
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadAppConfig;
    procedure SaveAppConfig;
    procedure Commit;
    procedure Restore;
    function AsJson: TJsonNode;
    function AsXml: IDocument;
  published
    property Data: TStrings read FData write SetData;
    property DataFormat: TDataTextFormat read FDataFormat write SetDataFormat;
    property UseAppConfig: Boolean read FUseAppConfig write FUseAppConfig;
  end;

implementation

{ TTextStorage }

constructor TTextStorage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataFormat := dfNone;
  FJsonChanged := True;
  FXmlChanged := True;
  FData := TStringList.Create;
  TStringList(FData).OnChange := TextChanged;
end;

destructor TTextStorage.Destroy;
begin
  if UseAppConfig then
    SaveAppConfig;
  FJSon.Free;
  inherited Destroy;
end;

procedure TTextStorage.Loaded;
begin
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

