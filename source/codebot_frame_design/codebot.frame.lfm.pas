(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ Codebot.Frame.Lfm reads the text of a form file into a tree of objects and
  properties. Simple values are kept as Pascal source, so that they can be
  written into code as they are. }

unit Codebot.Frame.Lfm;

{$mode delphi}

interface

uses
  Classes, SysUtils, Contnrs;

{ TLfmValueKind is the kind of value a property has }

type
  TLfmValueKind = (
    { A number, string, identifier, or set }
    lvSimple,
    { A list of values in parentheses, such as the lines of a TStrings }
    lvList,
    { Binary data in braces, such as an image }
    lvBinary,
    { A collection of items in angle brackets }
    lvCollection);

{ TLfmProperty is a property and its value }

  TLfmProperty = class
  public
    { The name of the property, which can be a path such as Font.Style }
    Name: string;
    Kind: TLfmValueKind;
    { The value of a simple property as Pascal source }
    Value: string;
    { True if the value of a simple property is a single identifier, such as
      a component name, an enumeration value, or the name of an event handler }
    IsIdent: Boolean;
    { The items of a list, each as Pascal source }
    Items: TStringList;
    { The items of a collection, which are TLfmItem objects }
    CollectionItems: TObjectList;
    { The text of the property as it appears in the form file }
    Source: string;
    constructor Create;
    destructor Destroy; override;
  end;

{ TLfmItem is an item of a collection }

  TLfmItem = class
  public
    { The properties of the item, which are TLfmProperty objects }
    Properties: TObjectList;
    constructor Create;
    destructor Destroy; override;
  end;

{ TLfmObject is a component in a form file }

  TLfmObject = class
  public
    Name: string;
    { The class name of the component }
    TypeName: string;
    { True if the component is a frame placed on the form }
    IsInline: Boolean;
    { True if the component is inherited from an ancestor form }
    IsInherited: Boolean;
    { The object which contains this one, or nil for the root }
    Parent: TLfmObject;
    { The properties of the component, which are TLfmProperty objects }
    Properties: TObjectList;
    { The components inside this one, which are TLfmObject objects }
    Children: TObjectList;
    constructor Create;
    destructor Destroy; override;
    { Finds a property by name, returning nil if it is not set }
    function FindProperty(const AName: string): TLfmProperty;
  end;

{ ELfmError is raised when the text of a form file cannot be read }

  ELfmError = class(Exception);

{ Reads the text of a form file, returning its root object }
function ParseLfm(const Text: string): TLfmObject;

implementation

{ TLfmProperty }

constructor TLfmProperty.Create;
begin
  inherited Create;
  Items := TStringList.Create;
  CollectionItems := TObjectList.Create(True);
end;

destructor TLfmProperty.Destroy;
begin
  CollectionItems.Free;
  Items.Free;
  inherited Destroy;
end;

{ TLfmItem }

constructor TLfmItem.Create;
begin
  inherited Create;
  Properties := TObjectList.Create(True);
end;

destructor TLfmItem.Destroy;
begin
  Properties.Free;
  inherited Destroy;
end;

{ TLfmObject }

constructor TLfmObject.Create;
begin
  inherited Create;
  Properties := TObjectList.Create(True);
  Children := TObjectList.Create(True);
end;

destructor TLfmObject.Destroy;
begin
  Children.Free;
  Properties.Free;
  inherited Destroy;
end;

function TLfmObject.FindProperty(const AName: string): TLfmProperty;
var
  I: Integer;
begin
  for I := 0 to Properties.Count - 1 do
  begin
    Result := TLfmProperty(Properties[I]);
    if SameText(Result.Name, AName) then
      Exit;
  end;
  Result := nil;
end;

{ TLfmParser }

type
  TLfmTokenKind = (ltEnd, ltIdent, ltNumber, ltString, ltBinary, ltSymbol);

  TLfmParser = class
  private
    FText: string;
    FPos: Integer;
    FLastEnd: Integer;
    FKind: TLfmTokenKind;
    FToken: string;
    FTokenStart: Integer;
    procedure Error(const Message: string);
    procedure Next;
    function IsSymbol(C: Char): Boolean;
    function IsWord(const Word: string): Boolean;
    procedure Expect(C: Char);
    function ParseObject(Parent: TLfmObject): TLfmObject;
    function ParseProperty: TLfmProperty;
    function ParseStrings: string;
    function ParseItem: string;
    procedure ParseValue(Prop: TLfmProperty);
  public
    constructor Create(const Text: string);
    function Parse: TLfmObject;
  end;

constructor TLfmParser.Create(const Text: string);
begin
  inherited Create;
  FText := Text;
  FPos := 1;
  Next;
end;

procedure TLfmParser.Error(const Message: string);
var
  Line, I: Integer;
begin
  Line := 1;
  for I := 1 to FTokenStart - 1 do
    if FText[I] = #10 then
      Inc(Line);
  raise ELfmError.CreateFmt('%s on line %d of the form file', [Message, Line]);
end;

procedure TLfmParser.Next;
const
  IdentChars = ['A'..'Z', 'a'..'z', '0'..'9', '_', '.'];
  HexChars = ['0'..'9', 'A'..'F', 'a'..'f'];
var
  C: Char;
  Start: Integer;
begin
  FLastEnd := FPos;
  while (FPos <= Length(FText)) and (FText[FPos] <= ' ') do
    Inc(FPos);
  FTokenStart := FPos;
  if FPos > Length(FText) then
  begin
    FKind := ltEnd;
    FToken := '';
    Exit;
  end;
  Start := FPos;
  C := FText[FPos];
  if C in ['A'..'Z', 'a'..'z', '_'] then
  begin
    FKind := ltIdent;
    while (FPos <= Length(FText)) and (FText[FPos] in IdentChars) do
      Inc(FPos);
  end
  else if (C in ['0'..'9']) or ((C in ['-', '+']) and (FPos < Length(FText)) and
    (FText[FPos + 1] in ['0'..'9', '.'])) then
  begin
    FKind := ltNumber;
    Inc(FPos);
    while FPos <= Length(FText) do
    begin
      C := FText[FPos];
      if C in ['0'..'9', '.'] then
        Inc(FPos)
      else if (C in ['e', 'E']) and (FPos < Length(FText)) then
      begin
        Inc(FPos);
        if FText[FPos] in ['-', '+'] then
          Inc(FPos);
      end
      else
        Break;
    end;
  end
  else if C = '$' then
  begin
    FKind := ltNumber;
    Inc(FPos);
    while (FPos <= Length(FText)) and (FText[FPos] in HexChars) do
      Inc(FPos);
  end
  else if C in ['''', '#'] then
  begin
    { A string is quoted text and character codes with nothing between them }
    FKind := ltString;
    while (FPos <= Length(FText)) and (FText[FPos] in ['''', '#']) do
      if FText[FPos] = '''' then
      begin
        Inc(FPos);
        while FPos <= Length(FText) do
          if FText[FPos] = '''' then
          begin
            if (FPos < Length(FText)) and (FText[FPos + 1] = '''') then
              Inc(FPos, 2)
            else
              Break;
          end
          else
            Inc(FPos);
        if FPos > Length(FText) then
          Error('A string is not closed');
        Inc(FPos);
      end
      else
      begin
        Inc(FPos);
        if (FPos <= Length(FText)) and (FText[FPos] = '$') then
        begin
          Inc(FPos);
          while (FPos <= Length(FText)) and (FText[FPos] in HexChars) do
            Inc(FPos);
        end
        else
          while (FPos <= Length(FText)) and (FText[FPos] in ['0'..'9']) do
            Inc(FPos);
      end;
  end
  else if C = '{' then
  begin
    FKind := ltBinary;
    while (FPos <= Length(FText)) and (FText[FPos] <> '}') do
      Inc(FPos);
    if FPos > Length(FText) then
      Error('Binary data is not closed');
    Inc(FPos);
  end
  else
  begin
    FKind := ltSymbol;
    Inc(FPos);
  end;
  FToken := Copy(FText, Start, FPos - Start);
end;

function TLfmParser.IsSymbol(C: Char): Boolean;
begin
  Result := (FKind = ltSymbol) and (FToken = C);
end;

function TLfmParser.IsWord(const Word: string): Boolean;
begin
  Result := (FKind = ltIdent) and SameText(FToken, Word);
end;

procedure TLfmParser.Expect(C: Char);
begin
  if not IsSymbol(C) then
    Error('Expected ' + C + ' but found ' + FToken);
  Next;
end;

function TLfmParser.ParseObject(Parent: TLfmObject): TLfmObject;
var
  S: string;
begin
  Result := TLfmObject.Create;
  try
    Result.Parent := Parent;
    Result.IsInline := IsWord('inline');
    Result.IsInherited := IsWord('inherited');
    Next;
    if FKind <> ltIdent then
      Error('Expected the name of an object');
    S := FToken;
    Next;
    if IsSymbol(':') then
    begin
      Next;
      if FKind <> ltIdent then
        Error('Expected the class of an object');
      Result.Name := S;
      Result.TypeName := FToken;
      Next;
    end
    else
      Result.TypeName := S;
    { An index of the object is not needed }
    if IsSymbol('[') then
    begin
      while not IsSymbol(']') and (FKind <> ltEnd) do
        Next;
      Next;
    end;
    while not IsWord('end') do
    begin
      if FKind = ltEnd then
        Error('The object ' + Result.Name + ' is not closed');
      if IsWord('object') or IsWord('inline') or IsWord('inherited') then
        Result.Children.Add(ParseObject(Result))
      else
        Result.Properties.Add(ParseProperty);
    end;
    Next;
  except
    Result.Free;
    raise;
  end;
end;

function TLfmParser.ParseProperty: TLfmProperty;
var
  Start: Integer;
begin
  if FKind <> ltIdent then
    Error('Expected a property name but found ' + FToken);
  Result := TLfmProperty.Create;
  try
    Start := FTokenStart;
    Result.Name := FToken;
    Next;
    Expect('=');
    ParseValue(Result);
    Result.Source := Copy(FText, Start, FLastEnd - Start);
  except
    Result.Free;
    raise;
  end;
end;

{ Reads a string which can be continued on the next line using a + }

function TLfmParser.ParseStrings: string;
begin
  Result := FToken;
  Next;
  while IsSymbol('+') do
  begin
    Next;
    if FKind <> ltString then
      Error('Expected a string after +');
    Result := Result + ' + ' + FToken;
    Next;
  end;
end;

{ Reads an item of a list }

function TLfmParser.ParseItem: string;
begin
  case FKind of
    ltString:
      Result := ParseStrings;
    ltNumber, ltIdent:
      begin
        Result := FToken;
        Next;
      end;
  else
    Error('Unexpected ' + FToken + ' in a list');
  end;
end;

procedure TLfmParser.ParseValue(Prop: TLfmProperty);
var
  Item: TLfmItem;
begin
  Prop.Kind := lvSimple;
  case FKind of
    ltString:
      Prop.Value := ParseStrings;
    ltNumber:
      begin
        Prop.Value := FToken;
        Next;
      end;
    ltIdent:
      begin
        Prop.Value := FToken;
        Prop.IsIdent := True;
        Next;
      end;
    ltBinary:
      begin
        Prop.Kind := lvBinary;
        Prop.Value := FToken;
        Next;
      end;
    ltSymbol:
      if IsSymbol('[') then
      begin
        Prop.Value := '[';
        Next;
        while not IsSymbol(']') do
        begin
          if FKind = ltEnd then
            Error('A set is not closed');
          if IsSymbol(',') then
            Prop.Value := Prop.Value + ', '
          else
            Prop.Value := Prop.Value + FToken;
          Next;
        end;
        Prop.Value := Prop.Value + ']';
        Next;
      end
      else if IsSymbol('(') then
      begin
        Prop.Kind := lvList;
        Next;
        while not IsSymbol(')') do
        begin
          if FKind = ltEnd then
            Error('A list is not closed');
          Prop.Items.Add(ParseItem);
        end;
        Next;
      end
      else if IsSymbol('<') then
      begin
        Prop.Kind := lvCollection;
        Next;
        while not IsSymbol('>') do
        begin
          if not IsWord('item') then
            Error('Expected an item but found ' + FToken);
          Next;
          if IsSymbol('[') then
          begin
            while not IsSymbol(']') and (FKind <> ltEnd) do
              Next;
            Next;
          end;
          Item := TLfmItem.Create;
          Prop.CollectionItems.Add(Item);
          while not IsWord('end') do
          begin
            if FKind = ltEnd then
              Error('An item is not closed');
            Item.Properties.Add(ParseProperty);
          end;
          Next;
        end;
        Next;
      end
      else
        Error('Unexpected ' + FToken);
  else
    Error('Expected a value');
  end;
end;

function TLfmParser.Parse: TLfmObject;
begin
  if not (IsWord('object') or IsWord('inherited') or IsWord('inline')) then
    Error('Expected an object');
  Result := ParseObject(nil);
end;

function ParseLfm(const Text: string): TLfmObject;
var
  Parser: TLfmParser;
begin
  Parser := TLfmParser.Create(Text);
  try
    Result := Parser.Parse;
  finally
    Parser.Free;
  end;
end;

end.
