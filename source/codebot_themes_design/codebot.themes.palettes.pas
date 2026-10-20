(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ Codebot.Themes.Palettes holds the themes of the IDE, with the palette of
  system colors and the font of each one. The themes are loaded from JSON
  files. It has no code of a platform, so that the colors of a theme can be
  shown anywhere. }

{ <include docs/codebot.themes.palettes.txt> }
unit Codebot.Themes.Palettes;

{$mode delphi}

interface

uses
  SysUtils, Graphics, LCLType;

{ TPaletteColors holds a color for each system color as $RRGGBB, indexed by
  the COLOR_ constants }

type
  TPaletteColors = array[0..MAX_SYS_COLORS] of LongInt;

{ TThemeInfo is a theme. A theme which is not custom uses the colors and
  visual styles of the system, and its colors are not used. A dark theme
  uses the dark parts of the visual styles of the system. FontNames lists
  the fonts of the theme in the order they are preferred, so that one theme
  can name a font of each system. A theme with no font names and a size of
  0 uses the font of the system. ButtonRadius is the radius of the corners
  of buttons in pixels, or -1 to leave them as the system has them. }

  TThemeInfo = record
    Name: string;
    Custom: Boolean;
    Dark: Boolean;
    Colors: TPaletteColors;
    FontNames: array of string;
    FontSize: Integer;
    ButtonRadius: Integer;
  end;

const
  { A system color which is left as it is }
  NoColor = -1;
  { The names of the system colors in a theme file, indexed by the COLOR_
    constants. Index 25 is not used. }
  ColorNames: array[0..MAX_SYS_COLORS] of string = (
    'scrollBar', 'background', 'activeCaption', 'inactiveCaption', 'menu',
    'window', 'windowFrame', 'menuText', 'windowText', 'captionText',
    'activeBorder', 'inactiveBorder', 'appWorkspace', 'highlight',
    'highlightText', 'btnFace', 'btnShadow', 'grayText', 'btnText',
    'inactiveCaptionText', 'btnHighlight', '3dDkShadow', '3dLight',
    'infoText', 'infoBk', '', 'hotLight', 'gradientActiveCaption',
    'gradientInactiveCaption', 'menuHilight', 'menuBar', 'form');
  { The colors of the Dark theme, which is used when no theme files are
    found }
  DarkColors: TPaletteColors = (
    $3E3E42, { COLOR_SCROLLBAR }
    $1E1E1E, { COLOR_BACKGROUND }
    $2D2D30, { COLOR_ACTIVECAPTION }
    $2D2D30, { COLOR_INACTIVECAPTION }
    $2B2B2B, { COLOR_MENU }
    $1E1E1E, { COLOR_WINDOW }
    $3F3F46, { COLOR_WINDOWFRAME }
    $F1F1F1, { COLOR_MENUTEXT }
    $DCDCDC, { COLOR_WINDOWTEXT }
    $F1F1F1, { COLOR_CAPTIONTEXT }
    $3F3F46, { COLOR_ACTIVEBORDER }
    $3F3F46, { COLOR_INACTIVEBORDER }
    $252526, { COLOR_APPWORKSPACE }
    $264F78, { COLOR_HIGHLIGHT }
    $FFFFFF, { COLOR_HIGHLIGHTTEXT }
    $2D2D30, { COLOR_BTNFACE }
    $1E1E1E, { COLOR_BTNSHADOW }
    $808080, { COLOR_GRAYTEXT }
    $F1F1F1, { COLOR_BTNTEXT }
    $A0A0A0, { COLOR_INACTIVECAPTIONTEXT }
    $3E3E42, { COLOR_BTNHIGHLIGHT }
    $141414, { COLOR_3DDKSHADOW }
    $3E3E40, { COLOR_3DLIGHT }
    $F1F1F1, { COLOR_INFOTEXT }
    $424245, { COLOR_INFOBK }
    NoColor, { 25 is not used }
    $569CD6, { COLOR_HOTLIGHT }
    $2D2D30, { COLOR_GRADIENTACTIVECAPTION }
    $2D2D30, { COLOR_GRADIENTINACTIVECAPTION }
    $3E3E40, { COLOR_MENUHILIGHT }
    $2D2D30, { COLOR_MENUBAR }
    $2D2D30  { COLOR_FORM }
  );

var
  { The colors of the system, which the dialog shows for themes which are
    not custom. They start as the colors of the default light theme of
    Windows, and are replaced with the colors the system has when those are
    known. }
  SystemColors: TPaletteColors = (
    $C8C8C8, { COLOR_SCROLLBAR }
    $000000, { COLOR_BACKGROUND }
    $FFFFFF, { COLOR_ACTIVECAPTION }
    $FFFFFF, { COLOR_INACTIVECAPTION }
    $F2F2F2, { COLOR_MENU }
    $FFFFFF, { COLOR_WINDOW }
    $646464, { COLOR_WINDOWFRAME }
    $000000, { COLOR_MENUTEXT }
    $000000, { COLOR_WINDOWTEXT }
    $000000, { COLOR_CAPTIONTEXT }
    $B4B4B4, { COLOR_ACTIVEBORDER }
    $F4F7FC, { COLOR_INACTIVEBORDER }
    $ABABAB, { COLOR_APPWORKSPACE }
    $0078D7, { COLOR_HIGHLIGHT }
    $FFFFFF, { COLOR_HIGHLIGHTTEXT }
    $F0F0F0, { COLOR_BTNFACE }
    $A0A0A0, { COLOR_BTNSHADOW }
    $6D6D6D, { COLOR_GRAYTEXT }
    $000000, { COLOR_BTNTEXT }
    $000000, { COLOR_INACTIVECAPTIONTEXT }
    $FFFFFF, { COLOR_BTNHIGHLIGHT }
    $696969, { COLOR_3DDKSHADOW }
    $D0D0D0, { COLOR_3DLIGHT }
    $000000, { COLOR_INFOTEXT }
    $FFFFE1, { COLOR_INFOBK }
    NoColor, { 25 is not used }
    $0066CC, { COLOR_HOTLIGHT }
    $B9D1EA, { COLOR_GRADIENTACTIVECAPTION }
    $D7E4F2, { COLOR_GRADIENTINACTIVECAPTION }
    $CCE8FF, { COLOR_MENUHILIGHT }
    $FFFFFF, { COLOR_MENUBAR }
    $F0F0F0  { COLOR_FORM }
  );
  { The themes, in the order the dialog lists them: System Default first,
    then the others by name }
  ThemeList: array of TThemeInfo;

{ Reads a theme from a JSON file, returning false if the file can not be
  read or has no name. A color the file leaves out is left as it is. }
function ThemeLoadFile(const FileName: string; out Theme: TThemeInfo): Boolean;
{ Fills ThemeList with System Default, Dark, and the themes of the JSON files
  in each folder. A theme with the name of one loaded before replaces it, so
  the later folders win. }
procedure ThemesLoad(const Folders: array of string);
{ The index of the theme used until another is chosen, which is Dark }
function DefaultTheme: Integer;
{ The index of the theme named Name in ThemeList, or DefaultTheme if there is
  no such theme }
function FindTheme(const Name: string): Integer;
{ The system color Index of the theme at Theme in ThemeList }
function ThemeColor(Theme, Index: Integer): TColor;
{ The first font of the theme at Theme in ThemeList which is installed, or
  default if none of them are }
function ThemeFontName(Theme: Integer): string;

implementation

uses
  Forms, Codebot.Text.Json;

const
  SystemName = 'System Default';
  DarkName = 'Dark';

{ The theme of the system, and the Dark theme used when no theme files are
  found }

function SystemTheme: TThemeInfo;
var
  I: Integer;
begin
  Result.Name := SystemName;
  Result.Custom := False;
  Result.Dark := False;
  for I := Low(Result.Colors) to High(Result.Colors) do
    Result.Colors[I] := NoColor;
  Result.FontNames := nil;
  Result.FontSize := 0;
  Result.ButtonRadius := -1;
end;

function DarkTheme: TThemeInfo;
begin
  Result.Name := DarkName;
  Result.Custom := True;
  Result.Dark := True;
  Result.Colors := DarkColors;
  Result.FontNames := ['Segoe UI', 'Ubuntu Sans', 'Noto Sans'];
  {$ifdef linux}
  Result.FontSize := 10;
  {$else}
  Result.FontSize := 9;
  {$endif}
  Result.ButtonRadius := -1;
end;

{ Colors are written as #RRGGBB }

function ParseColor(const S: string): LongInt;
begin
  if (Length(S) = 7) and (S[1] = '#') then
    Result := StrToIntDef('$' + Copy(S, 2, 6), NoColor)
  else
    Result := NoColor;
end;

function ColorIndex(const Name: string): Integer;
var
  I: Integer;
begin
  for I := Low(ColorNames) to High(ColorNames) do
    if (ColorNames[I] <> '') and SameText(ColorNames[I], Name) then
      Exit(I);
  Result := -1;
end;

{ A font is written as one name or as an array of names }

procedure ReadFontNames(Node: TJsonNode; var Theme: TThemeInfo);
var
  Item: TJsonNode;
  S: string;
begin
  if Node.Kind = nkString then
  begin
    S := Trim(Node.AsString);
    if S <> '' then
      Theme.FontNames := [S];
    Exit;
  end;
  if Node.Kind <> nkArray then
    Exit;
  Theme.FontNames := nil;
  for Item in Node do
    if Item.Kind = nkString then
    begin
      S := Trim(Item.AsString);
      if S <> '' then
        Theme.FontNames := Theme.FontNames + [S];
    end;
end;

function ThemeLoadFile(const FileName: string; out Theme: TThemeInfo): Boolean;
var
  Doc, Node, Item: TJsonNode;
  I: Integer;
begin
  Result := False;
  Theme := SystemTheme;
  Theme.Custom := True;
  Doc := TJsonNode.Create;
  try
    try
      Doc.LoadFromFile(FileName);
    except
      Exit;
    end;
    if not Doc.Find('name', Node) or (Node.Kind <> nkString) then
      Exit;
    Theme.Name := Trim(Node.AsString);
    if Theme.Name = '' then
      Exit;
    if Doc.Find('dark', Node) and (Node.Kind = nkBool) then
      Theme.Dark := Node.AsBoolean;
    if Doc.Find('font/name', Node) then
      ReadFontNames(Node, Theme);
    if Doc.Find('font/size', Node) and (Node.Kind = nkNumber) then
      Theme.FontSize := Round(Node.AsNumber);
    { Fonts of the same size look smaller on Linux than on Windows }
    {$ifdef linux}
    if Doc.Find('font/linuxSize', Node) and (Node.Kind = nkNumber) then
      Theme.FontSize := Round(Node.AsNumber);
    {$endif}
    if Doc.Find('buttonRadius', Node) and (Node.Kind = nkNumber) then
      Theme.ButtonRadius := Round(Node.AsNumber);
    if Doc.Find('colors', Node) and (Node.Kind = nkObject) then
      for Item in Node do
      begin
        I := ColorIndex(Item.Name);
        if (I >= 0) and (Item.Kind = nkString) then
          Theme.Colors[I] := ParseColor(Item.AsString);
      end;
    Result := True;
  finally
    Doc.Free;
  end;
end;

function IndexOfTheme(const Name: string): Integer;
var
  I: Integer;
begin
  for I := Low(ThemeList) to High(ThemeList) do
    if SameText(ThemeList[I].Name, Name) then
      Exit(I);
  Result := -1;
end;

procedure AddTheme(const Theme: TThemeInfo);
var
  I: Integer;
begin
  I := IndexOfTheme(Theme.Name);
  if I < 0 then
  begin
    I := Length(ThemeList);
    SetLength(ThemeList, I + 1);
  end;
  ThemeList[I] := Theme;
end;

procedure ThemesLoad(const Folders: array of string);
var
  Search: TSearchRec;
  Theme, Swap: TThemeInfo;
  Folder: string;
  I, J: Integer;
begin
  ThemeList := nil;
  AddTheme(SystemTheme);
  AddTheme(DarkTheme);
  for I := Low(Folders) to High(Folders) do
  begin
    if Folders[I] = '' then
      Continue;
    Folder := IncludeTrailingPathDelimiter(Folders[I]);
    if FindFirst(Folder + '*.json', faAnyFile, Search) = 0 then
    try
      repeat
        { The theme which is not custom can not be replaced }
        if ThemeLoadFile(Folder + Search.Name, Theme) and
          (not SameText(Theme.Name, SystemName)) then
          AddTheme(Theme);
      until FindNext(Search) <> 0;
    finally
      FindClose(Search);
    end;
  end;
  { System Default stays first and the others are sorted by name }
  for I := 2 to High(ThemeList) do
  begin
    Swap := ThemeList[I];
    J := I - 1;
    while (J > 0) and (CompareText(ThemeList[J].Name, Swap.Name) > 0) do
    begin
      ThemeList[J + 1] := ThemeList[J];
      Dec(J);
    end;
    ThemeList[J + 1] := Swap;
  end;
end;

function DefaultTheme: Integer;
begin
  Result := IndexOfTheme(DarkName);
  if Result < 0 then
    Result := 0;
end;

function FindTheme(const Name: string): Integer;
begin
  Result := IndexOfTheme(Name);
  if Result < 0 then
    Result := DefaultTheme;
end;

function ThemeColor(Theme, Index: Integer): TColor;
var
  C: LongInt;
begin
  if (Index < 0) or (Index > MAX_SYS_COLORS) then
    Exit(clBlack);
  if ThemeList[Theme].Custom then
    C := ThemeList[Theme].Colors[Index]
  else
    C := SystemColors[Index];
  { A color the theme leaves out shows as the color of the system }
  if C = NoColor then
    C := SystemColors[Index];
  if C = NoColor then
    Exit(clBlack);
  Result := RGBToColor((C shr 16) and $FF, (C shr 8) and $FF, C and $FF);
end;

{ The fonts are looked for when they are used rather than when the themes
  are loaded, as the fonts of the system may not be known yet }

function ThemeFontName(Theme: Integer): string;
var
  S: string;
begin
  for S in ThemeList[Theme].FontNames do
    if SameText(S, 'default') or (Screen.Fonts.IndexOf(S) >= 0) then
      Exit(S);
  Result := 'default';
end;

initialization
  ThemesLoad([]);
end.
