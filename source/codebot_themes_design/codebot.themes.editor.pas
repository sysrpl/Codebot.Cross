(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ Codebot.Themes.Editor lists the color schemes of the source editor of the
  IDE, reads their colors, and chooses the scheme and the font the source
  editor uses }

unit Codebot.Themes.Editor;

{$mode delphi}

interface

uses
  Classes, Graphics;

{ TEditorStyle is the look of one kind of text in a color scheme. A color is
  clNone or clDefault when the scheme leaves it to the default text. }

type
  TEditorStyle = record
    Fore: TColor;
    Back: TColor;
    Style: TFontStyles;
  end;

{ Adds the names of the color schemes of the source editor to Strings }
procedure EditorSchemeNames(Strings: TStrings);
{ The name of the color scheme Pascal source is shown with, or an empty
  string if the IDE has no color schemes }
function EditorSchemeCurrent: string;
{ Reads the look of the text which Pascal source shows with an attribute,
  such as 'Comment', 'Reserved word', or 'ahaDefault', in a color scheme,
  returning false if the scheme or the attribute does not exist }
function EditorSchemeStyle(const Scheme, Attribute: string;
  out Style: TEditorStyle): Boolean;
{ The name and the size of the font of the source editor }
function EditorFontName: string;
function EditorFontSize: Integer;
{ Adds the names of the fonts whose characters all have the same width to
  Strings, which are the fonts suited to source }
procedure EditorMonospaceFonts(Strings: TStrings);
{ Gives the source of every language the color scheme, and the source
  editor the font, saves them in the options of the editor, and shows them
  in every editor which is open. Nothing is done if nothing changed, as
  every editor is loaded again. An empty scheme or font name is left as it
  is. }
procedure EditorApply(const Scheme, FontName: string; FontSize: Integer);

implementation

uses
  LCLType, LCLIntf,
  EditorSyntaxHighlighterDef, EditorOptionsIntf, LazEditHighlighter,
  LazEditTextAttributes;

{ The highlighter of Pascal source is the one for files ending in .pas }

function PascalId: TIdeSyntaxHighlighterID;
begin
  Result := IdeHighlighterUnknownId;
  if Assigned(IdeSyntaxHighlighters) then
    Result := IdeSyntaxHighlighters.GetIdForFileExtension('pas');
end;

procedure EditorSchemeNames(Strings: TStrings);
var
  Scheme: IColorScheme;
  I: Integer;
begin
  if not Assigned(IdeColorSchemeList) then
    Exit;
  for I := 0 to IdeColorSchemeList.Count - 1 do
  begin
    Scheme := IdeColorSchemeList.GetScheme(I);
    if Assigned(Scheme) then
      Strings.Add(Scheme.GetName);
  end;
end;

function EditorSchemeCurrent: string;
var
  Id: TIdeSyntaxHighlighterID;
  Highlighter: TLazEditCustomHighlighter;
begin
  Result := '';
  Id := PascalId;
  if (Id < IdeHighlighterStartId) or (IDEEditorOptions = nil) then
    Exit;
  Highlighter := IdeSyntaxHighlighters.SharedInstances[Id];
  if Highlighter <> nil then
    Result := IDEEditorOptions.GetColorSchemeName(Highlighter.LanguageName);
end;

function EditorSchemeStyle(const Scheme, Attribute: string;
  out Style: TEditorStyle): Boolean;
var
  Id: TIdeSyntaxHighlighterID;
  ColorScheme: IColorScheme;
  Language: IColorSchemeLanguage;
  Source: IColorSchemeAttribute;
  Dest: TLazEditTextAttribute;
begin
  Result := False;
  Style.Fore := clNone;
  Style.Back := clNone;
  Style.Style := [];
  Id := PascalId;
  if (Id < IdeHighlighterStartId) or (not Assigned(IdeColorSchemeList)) then
    Exit;
  ColorScheme := IdeColorSchemeList.GetScheme(Scheme);
  if not Assigned(ColorScheme) then
    Exit;
  Language := ColorScheme.GetLanguageForHighlighter(Id);
  if not Assigned(Language) then
    Exit;
  Source := Language.GetAttributeIntf(Attribute);
  if not Assigned(Source) then
    Exit;
  Dest := TLazEditTextAttribute.Create(Attribute, Attribute);
  try
    Source.ApplyTo(Dest);
    Style.Fore := Dest.Foreground;
    Style.Back := Dest.Background;
    Style.Style := Dest.Style;
  finally
    Dest.Free;
  end;
  Result := True;
end;

function EditorFontName: string;
begin
  if IDEEditorOptions <> nil then
    Result := IDEEditorOptions.EditorFontName
  else
    Result := '';
end;

function EditorFontSize: Integer;
begin
  if IDEEditorOptions <> nil then
    Result := IDEEditorOptions.EditorFontSize
  else
    Result := 0;
end;

{ Fonts whose names start with @ are the vertical forms of other fonts }

function EnumFont(var LogFont: TEnumLogFontEx; var Metric: TNewTextMetricEx;
  FontType: LongInt; Data: LParam): LongInt; stdcall;
var
  Strings: TStrings;
  Name: string;
begin
  Result := 1;
  Strings := TStrings(PtrUInt(Data));
  Name := LogFont.elfLogFont.lfFaceName;
  if (Name = '') or (Name[1] = '@') then
    Exit;
  if (LogFont.elfLogFont.lfPitchAndFamily and FIXED_PITCH = FIXED_PITCH) and
    (Strings.IndexOf(Name) < 0) then
    Strings.Add(Name);
end;

procedure EditorMonospaceFonts(Strings: TStrings);
var
  DC: HDC;
  LogFont: TLogFont;
begin
  FillChar(LogFont, SizeOf(LogFont), 0);
  LogFont.lfCharSet := DEFAULT_CHARSET;
  DC := GetDC(0);
  try
    EnumFontFamiliesEx(DC, @LogFont, EnumFont, PtrInt(Strings), 0);
  finally
    ReleaseDC(0, DC);
  end;
end;

{ The options dialog of the IDE keeps a scheme for each language, so every
  language is given the scheme, and DoAfterWrite saves the options and
  shows them in every editor }

procedure EditorApply(const Scheme, FontName: string; FontSize: Integer);
var
  Highlighter: TLazEditCustomHighlighter;
  Changed: Boolean;
  I: Integer;
begin
  if IDEEditorOptions = nil then
    Exit;
  Changed := False;
  if (Scheme <> '') and (Scheme <> EditorSchemeCurrent) and
    Assigned(IdeSyntaxHighlighters) then
  begin
    for I := IdeHighlighterStartId to IdeSyntaxHighlighters.Count - 1 do
    begin
      Highlighter := IdeSyntaxHighlighters.SharedInstances[I];
      if Highlighter <> nil then
        IDEEditorOptions.SetColorSchemeName(Highlighter.LanguageName, Scheme);
    end;
    Changed := True;
  end;
  if (FontName <> '') and (FontName <> IDEEditorOptions.EditorFontName) then
  begin
    IDEEditorOptions.EditorFontName := FontName;
    Changed := True;
  end;
  if (FontSize > 0) and (FontSize <> IDEEditorOptions.EditorFontSize) then
  begin
    IDEEditorOptions.EditorFontSize := FontSize;
    Changed := True;
  end;
  if Changed then
    IDEEditorOptions.DoAfterWrite(False);
end;

end.
