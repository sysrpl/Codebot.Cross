(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ Codebot.Frame.Source finds the parts of the unit of a form or frame which
  change when it is converted into a control, and edits its text. Only the
  tokens of the unit are read, so the comments, directives, and code which
  are not changed are kept exactly as they are. }

unit Codebot.Frame.Source;

{$mode delphi}

interface

uses
  Classes, SysUtils;

{ TPasTokenKind is the kind of a token of Pascal source }

type
  TPasTokenKind = (ptIdent, ptNumber, ptString, ptSymbol, ptDirective);

{ TPasToken is a token of Pascal source. Comments and spaces are not tokens,
  but compiler directives are. }

  TPasToken = record
    Kind: TPasTokenKind;
    { The position of the first character of the token }
    StartPos: Integer;
    { The position after the last character of the token }
    EndPos: Integer;
    Text: string;
  end;

{ TSourceEdit replaces Count characters at Pos with Text }

  TSourceEdit = record
    Pos: Integer;
    Count: Integer;
    Text: string;
  end;

{ TFormSource is the unit of a form or frame }

  TFormSource = class
  private
    FText: string;
    FTokens: array of TPasToken;
    FEdits: array of TSourceEdit;
    FError: string;
    procedure Tokenize;
    function Fail(const Message: string): Boolean;
    function ReadUses(Index: Integer; Units: TStrings): Integer;
  public
    { True if the unit is written in Delphi mode, where the address of an
      event handler is assigned without @ }
    IsDelphiMode: Boolean;
    { The index of the token of the unit name }
    UnitNameIndex: Integer;
    { The index of the implementation keyword }
    ImplementationIndex: Integer;
    { The index of the token code is inserted before at the end of the unit,
      which is the initialization or finalization keyword, or the final end }
    UnitEndIndex: Integer;
    { The index of the semicolon which ends the uses clause of the interface,
      or -1 if the interface has no uses clause }
    InterfaceUsesEnd: Integer;
    { The index of the interface keyword }
    InterfaceIndex: Integer;
    { The index of the semicolon which ends the uses clause of the
      implementation, or -1 if it has no uses clause }
    ImplementationUsesEnd: Integer;
    { The index of the name of the form class in its declaration }
    ClassIndex: Integer;
    { The index of the ancestor of the form class }
    AncestorIndex: Integer;
    { The index of the end of the form class declaration }
    ClassEndIndex: Integer;
    { The units in the uses clauses of the interface and the implementation }
    InterfaceUses: TStringList;
    ImplementationUses: TStringList;
    { The names declared in the form class, in lowercase }
    ClassMembers: TStringList;
    { The names of global variables whose type is the form class }
    FormVariables: TStringList;
    constructor Create(const Text: string);
    destructor Destroy; override;
    { Finds the parts of the unit for the form class FormClass. Fields of the
      class named in ComponentNames are removed, along with the variables of
      the form class and the form resource directive. Returns False if the
      unit could not be understood, with the reason in Error. }
    function Analyze(const FormClass: string; ComponentNames: TStrings): Boolean;
    { Replaces every use of the identifier OldName with NewName }
    procedure ReplaceIdent(const OldName, NewName: string);
    { Adds an edit of the text of the unit }
    procedure AddEdit(Pos, Count: Integer; const Text: string);
    { Inserts Text at the start of the line of the token Index }
    procedure InsertBeforeLine(Index: Integer; const Text: string);
    { Inserts Text after the token Index }
    procedure InsertAfter(Index: Integer; const Text: string);
    { Replaces the token Index with Text }
    procedure ReplaceToken(Index: Integer; const Text: string);
    { Removes the tokens from First to Last, along with their line if nothing
      else is on it }
    procedure RemoveTokens(First, Last: Integer);
    { Returns the line of the token Index, starting at 1 }
    function LineOf(Index: Integer): Integer;
    { Returns the text of the unit with all of the edits made }
    function Apply: string;
    function TokenCount: Integer;
    function Token(Index: Integer): TPasToken;
    { True if the token Index is the identifier or keyword Word }
    function IsWord(Index: Integer; const Word: string): Boolean;
    { True if the token Index is the symbol S }
    function IsSymbol(Index: Integer; const S: string): Boolean;
    property Error: string read FError;
  end;

implementation

constructor TFormSource.Create(const Text: string);
begin
  inherited Create;
  FText := Text;
  InterfaceUses := TStringList.Create;
  ImplementationUses := TStringList.Create;
  ClassMembers := TStringList.Create;
  ClassMembers.Sorted := True;
  ClassMembers.Duplicates := dupIgnore;
  FormVariables := TStringList.Create;
  Tokenize;
end;

destructor TFormSource.Destroy;
begin
  FormVariables.Free;
  ClassMembers.Free;
  ImplementationUses.Free;
  InterfaceUses.Free;
  inherited Destroy;
end;

procedure TFormSource.Tokenize;
const
  IdentStart = ['A'..'Z', 'a'..'z', '_', '&'];
  IdentChars = ['A'..'Z', 'a'..'z', '_', '0'..'9'];
  HexChars = ['0'..'9', 'A'..'F', 'a'..'f'];
var
  Count, P, Start: Integer;
  Kind: TPasTokenKind;
  C: Char;

  procedure Add;
  begin
    if Count = Length(FTokens) then
      SetLength(FTokens, Count * 2 + 256);
    FTokens[Count].Kind := Kind;
    FTokens[Count].StartPos := Start;
    FTokens[Count].EndPos := P;
    FTokens[Count].Text := Copy(FText, Start, P - Start);
    Inc(Count);
  end;

begin
  Count := 0;
  P := 1;
  while P <= Length(FText) do
  begin
    C := FText[P];
    Start := P;
    if C <= ' ' then
      Inc(P)
    else if C = '{' then
    begin
      while (P <= Length(FText)) and (FText[P] <> '}') do
        Inc(P);
      Inc(P);
      if (Start < Length(FText)) and (FText[Start + 1] = '$') then
      begin
        Kind := ptDirective;
        Add;
      end;
    end
    else if (C = '(') and (P < Length(FText)) and (FText[P + 1] = '*') then
    begin
      Inc(P, 2);
      while (P < Length(FText)) and not ((FText[P] = '*') and (FText[P + 1] = ')')) do
        Inc(P);
      Inc(P, 2);
    end
    else if (C = '/') and (P < Length(FText)) and (FText[P + 1] = '/') then
    begin
      while (P <= Length(FText)) and not (FText[P] in [#10, #13]) do
        Inc(P);
    end
    else if C in ['''', '#'] then
    begin
      Kind := ptString;
      while (P <= Length(FText)) and (FText[P] in ['''', '#']) do
        if FText[P] = '''' then
        begin
          Inc(P);
          while P <= Length(FText) do
            if FText[P] = '''' then
            begin
              if (P < Length(FText)) and (FText[P + 1] = '''') then
                Inc(P, 2)
              else
                Break;
            end
            else if FText[P] in [#10, #13] then
              Break
            else
              Inc(P);
          Inc(P);
        end
        else
        begin
          Inc(P);
          if (P <= Length(FText)) and (FText[P] = '$') then
            Inc(P);
          while (P <= Length(FText)) and (FText[P] in HexChars) do
            Inc(P);
        end;
      Add;
    end
    else if (C in IdentStart) and not ((C = '&') and (P < Length(FText)) and
      (FText[P + 1] in ['0'..'9'])) then
    begin
      Kind := ptIdent;
      Inc(P);
      while (P <= Length(FText)) and (FText[P] in IdentChars) do
        Inc(P);
      Add;
    end
    else if C in ['0'..'9', '$', '%', '&'] then
    begin
      Kind := ptNumber;
      Inc(P);
      while (P <= Length(FText)) and (FText[P] in HexChars + ['.']) do
      begin
        { A range such as 1..2 is a number followed by .. }
        if (FText[P] = '.') and (P < Length(FText)) and (FText[P + 1] = '.') then
          Break;
        Inc(P);
      end;
      Add;
    end
    else
    begin
      Kind := ptSymbol;
      Inc(P);
      if P <= Length(FText) then
        if ((C = ':') and (FText[P] = '=')) or ((C = '.') and (FText[P] = '.')) or
          ((C = '<') and (FText[P] in ['=', '>'])) or ((C = '>') and (FText[P] = '=')) then
          Inc(P);
      Add;
    end;
  end;
  SetLength(FTokens, Count);
end;

function TFormSource.TokenCount: Integer;
begin
  Result := Length(FTokens);
end;

function TFormSource.Token(Index: Integer): TPasToken;
begin
  Result := FTokens[Index];
end;

function TFormSource.IsWord(Index: Integer; const Word: string): Boolean;
begin
  Result := (Index >= 0) and (Index < Length(FTokens)) and
    (FTokens[Index].Kind = ptIdent) and SameText(FTokens[Index].Text, Word);
end;

function TFormSource.IsSymbol(Index: Integer; const S: string): Boolean;
begin
  Result := (Index >= 0) and (Index < Length(FTokens)) and
    (FTokens[Index].Kind = ptSymbol) and (FTokens[Index].Text = S);
end;

function TFormSource.Fail(const Message: string): Boolean;
begin
  FError := Message;
  Result := False;
end;

{ Reads the units of the uses clause at Index, returning the index of the
  semicolon which ends it }

function TFormSource.ReadUses(Index: Integer; Units: TStrings): Integer;
var
  S: string;
begin
  Result := Index + 1;
  S := '';
  while (Result < Length(FTokens)) and not IsSymbol(Result, ';') do
  begin
    if IsWord(Result, 'in') then
    begin
      { The file of a unit is skipped }
      Inc(Result, 2);
      Continue;
    end;
    if FTokens[Result].Kind = ptIdent then
      S := S + FTokens[Result].Text
    else if IsSymbol(Result, '.') then
      S := S + '.'
    else if IsSymbol(Result, ',') then
    begin
      Units.Add(S);
      S := '';
    end;
    Inc(Result);
  end;
  if S <> '' then
    Units.Add(S);
end;

function TFormSource.Analyze(const FormClass: string; ComponentNames: TStrings): Boolean;
const
  SectionWords: array[0..9] of string = ('implementation', 'type', 'const',
    'var', 'threadvar', 'resourcestring', 'procedure', 'function', 'uses',
    'operator');

  function IsSectionWord(Index: Integer): Boolean;
  var
    S: string;
  begin
    for S in SectionWords do
      if IsWord(Index, S) then
        Exit(True);
    Result := False;
  end;

var
  Depth, Parens, VarIndex, I, J: Integer;
  S: string;
begin
  UnitNameIndex := -1;
  InterfaceIndex := -1;
  ImplementationIndex := -1;
  UnitEndIndex := -1;
  InterfaceUsesEnd := -1;
  ImplementationUsesEnd := -1;
  ClassIndex := -1;
  AncestorIndex := -1;
  ClassEndIndex := -1;
  for I := 0 to Length(FTokens) - 1 do
    if FTokens[I].Kind = ptDirective then
    begin
      S := LowerCase(FTokens[I].Text);
      if S.StartsWith('{$mode delphi') then
        IsDelphiMode := True
      else if S.StartsWith('{$mode ') then
        IsDelphiMode := False;
    end
    else if (UnitNameIndex < 0) and IsWord(I, 'unit') then
      UnitNameIndex := I + 1
    else if (InterfaceIndex < 0) and IsWord(I, 'interface') then
      InterfaceIndex := I
    else if (ImplementationIndex < 0) and IsWord(I, 'implementation') then
      ImplementationIndex := I
    else if (ImplementationIndex >= 0) and (UnitEndIndex < 0) and
      (IsWord(I, 'initialization') or IsWord(I, 'finalization')) then
      UnitEndIndex := I;
  if (UnitNameIndex < 0) or (InterfaceIndex < 0) or (ImplementationIndex < 0) then
    Exit(Fail('The unit does not have a unit name, an interface, and an implementation.'));
  if UnitEndIndex < 0 then
  begin
    I := Length(FTokens) - 1;
    while (I > 0) and not (IsSymbol(I, '.') and IsWord(I - 1, 'end')) do
      Dec(I);
    if I <= ImplementationIndex then
      Exit(Fail('The end of the unit could not be found.'));
    UnitEndIndex := I - 1;
  end;
  { The uses clauses }
  if IsWord(InterfaceIndex + 1, 'uses') then
    InterfaceUsesEnd := ReadUses(InterfaceIndex + 1, InterfaceUses);
  if IsWord(ImplementationIndex + 1, 'uses') then
    ImplementationUsesEnd := ReadUses(ImplementationIndex + 1, ImplementationUses);
  { The declaration of the form class }
  for I := InterfaceIndex to ImplementationIndex - 5 do
    if IsWord(I, FormClass) and IsSymbol(I + 1, '=') and IsWord(I + 2, 'class') and
      IsSymbol(I + 3, '(') and (FTokens[I + 4].Kind = ptIdent) then
    begin
      ClassIndex := I;
      AncestorIndex := I + 4;
      Break;
    end;
  if ClassIndex < 0 then
    Exit(Fail('The declaration of ' + FormClass + ' could not be found in the interface.'));
  I := AncestorIndex;
  while (I < ImplementationIndex) and not IsSymbol(I, ')') do
    Inc(I);
  Depth := 1;
  Parens := 0;
  Inc(I);
  while I < ImplementationIndex do
  begin
    if IsSymbol(I, '(') or IsSymbol(I, '[') then
      Inc(Parens)
    else if IsSymbol(I, ')') or IsSymbol(I, ']') then
      Dec(Parens)
    else if Parens = 0 then
      if IsWord(I, 'end') then
      begin
        Dec(Depth);
        if Depth = 0 then
        begin
          ClassEndIndex := I;
          Break;
        end;
      end
      else if IsWord(I, 'record') then
        Inc(Depth)
      else if (IsWord(I, 'class') or IsWord(I, 'object')) and IsSymbol(I - 1, '=') and
        not IsWord(I + 1, 'of') and not IsSymbol(I + 1, ';') then
        Inc(Depth)
      else if Depth = 1 then
      begin
        { The names of methods and properties }
        if IsWord(I, 'procedure') or IsWord(I, 'function') or IsWord(I, 'constructor') or
          IsWord(I, 'destructor') or IsWord(I, 'property') then
        begin
          if FTokens[I + 1].Kind = ptIdent then
            ClassMembers.Add(LowerCase(FTokens[I + 1].Text));
        end
        { A field of a component is removed, as the control declares its own }
        else if (FTokens[I].Kind = ptIdent) and IsSymbol(I + 1, ':') and
          (FTokens[I + 2].Kind = ptIdent) and IsSymbol(I + 3, ';') then
        begin
          if ComponentNames.IndexOf(FTokens[I].Text) > -1 then
          begin
            RemoveTokens(I, I + 3);
            Inc(I, 3);
          end
          else
            ClassMembers.Add(LowerCase(FTokens[I].Text));
        end;
      end;
    Inc(I);
  end;
  if ClassEndIndex < 0 then
    Exit(Fail('The end of the declaration of ' + FormClass + ' could not be found.'));
  { The global variables of the form class are removed }
  VarIndex := -1;
  I := ClassEndIndex + 1;
  while I < ImplementationIndex do
  begin
    if IsWord(I, 'var') then
      VarIndex := I
    else if IsSectionWord(I) then
      VarIndex := -1
    else if (VarIndex > -1) and (FTokens[I].Kind = ptIdent) and IsSymbol(I + 1, ':') and
      IsWord(I + 2, FormClass) and IsSymbol(I + 3, ';') then
    begin
      FormVariables.Add(FTokens[I].Text);
      { The var keyword goes too when this is the only variable it declares }
      if (VarIndex = I - 1) and IsSectionWord(I + 4) then
        RemoveTokens(VarIndex, I + 3)
      else
        RemoveTokens(I, I + 3);
      VarIndex := -1;
      Inc(I, 3);
    end;
    Inc(I);
  end;
  { The form resource is not used by a control }
  for I := ImplementationIndex to Length(FTokens) - 1 do
    if FTokens[I].Kind = ptDirective then
    begin
      S := LowerCase(FTokens[I].Text);
      if (S.StartsWith('{$r ') or S.StartsWith('{$resource ')) and
        ((Pos('.lfm', S) > 0) or (Pos('.dfm', S) > 0) or (Pos('.xfm', S) > 0)) then
        RemoveTokens(I, I);
    end;
  for J := 0 to ComponentNames.Count - 1 do
    ClassMembers.Add(LowerCase(ComponentNames[J]));
  Result := True;
end;

procedure TFormSource.ReplaceIdent(const OldName, NewName: string);
var
  I: Integer;
begin
  for I := 0 to Length(FTokens) - 1 do
    if IsWord(I, OldName) then
      ReplaceToken(I, NewName);
end;

procedure TFormSource.AddEdit(Pos, Count: Integer; const Text: string);
var
  I: Integer;
begin
  I := Length(FEdits);
  SetLength(FEdits, I + 1);
  FEdits[I].Pos := Pos;
  FEdits[I].Count := Count;
  FEdits[I].Text := Text;
end;

procedure TFormSource.InsertBeforeLine(Index: Integer; const Text: string);
var
  P: Integer;
begin
  P := FTokens[Index].StartPos;
  while (P > 1) and not (FText[P - 1] in [#10, #13]) do
    Dec(P);
  AddEdit(P, 0, Text);
end;

procedure TFormSource.InsertAfter(Index: Integer; const Text: string);
begin
  AddEdit(FTokens[Index].EndPos, 0, Text);
end;

procedure TFormSource.ReplaceToken(Index: Integer; const Text: string);
begin
  AddEdit(FTokens[Index].StartPos, FTokens[Index].EndPos - FTokens[Index].StartPos, Text);
end;

procedure TFormSource.RemoveTokens(First, Last: Integer);
var
  StartPos, EndPos, P: Integer;
begin
  StartPos := FTokens[First].StartPos;
  EndPos := FTokens[Last].EndPos;
  { The whole line goes if only spaces are before and after the tokens }
  P := StartPos;
  while (P > 1) and (FText[P - 1] in [' ', #9]) do
    Dec(P);
  if (P = 1) or (FText[P - 1] in [#10, #13]) then
  begin
    StartPos := P;
    P := EndPos;
    while (P <= Length(FText)) and (FText[P] in [' ', #9]) do
      Inc(P);
    if (P <= Length(FText)) and (FText[P] = #13) then
      Inc(P);
    if (P <= Length(FText)) and (FText[P] = #10) then
      Inc(P);
    if (P > Length(FText)) or (FText[P - 1] in [#10, #13]) then
      EndPos := P;
  end;
  AddEdit(StartPos, EndPos - StartPos, '');
end;

function TFormSource.LineOf(Index: Integer): Integer;
var
  I: Integer;
begin
  Result := 1;
  for I := 1 to FTokens[Index].StartPos - 1 do
    if FText[I] = #10 then
      Inc(Result);
end;

function TFormSource.Apply: string;
var
  Edits: array of TSourceEdit;
  Dropped: array of Boolean;
  Edit: TSourceEdit;
  Last, I, J: Integer;
begin
  { The edits are sorted by position, keeping the order of edits at the same
    position, and then made from the end so that positions stay valid }
  Edits := Copy(FEdits);
  for I := 1 to Length(Edits) - 1 do
  begin
    Edit := Edits[I];
    J := I - 1;
    while (J >= 0) and (Edits[J].Pos > Edit.Pos) do
    begin
      Edits[J + 1] := Edits[J];
      Dec(J);
    end;
    Edits[J + 1] := Edit;
  end;
  { An edit inside text which another edit replaces is dropped, such as the
    rename of the form class in the declaration of a variable being removed }
  SetLength(Dropped, Length(Edits));
  for I := 0 to Length(Edits) - 1 do
    if Edits[I].Count > 0 then
      for J := 0 to Length(Edits) - 1 do
        if (J <> I) and not Dropped[I] then
          if ((Edits[J].Pos > Edits[I].Pos) and (Edits[J].Pos < Edits[I].Pos + Edits[I].Count)) or
            ((Edits[J].Pos = Edits[I].Pos) and (Edits[J].Count > 0) and
            (Edits[J].Count < Edits[I].Count)) then
            Dropped[J] := True;
  Result := FText;
  Last := MaxInt;
  for I := Length(Edits) - 1 downto 0 do
  begin
    if Dropped[I] then
      Continue;
    Edit := Edits[I];
    if Edit.Pos + Edit.Count > Last then
      Continue;
    Delete(Result, Edit.Pos, Edit.Count);
    Insert(Edit.Text, Result, Edit.Pos);
    Last := Edit.Pos;
  end;
end;

end.
