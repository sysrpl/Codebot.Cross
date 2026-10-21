(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ Codebot.Themes.Manager applies the themes of Codebot.Themes.Palettes to
  every window of a program, and holds TThemeManager, the component which
  chooses the theme of a program from a form }

{ <include docs/codebot.themes.manager.txt> }
unit Codebot.Themes.Manager;

{$mode delphi}

{$if not ((defined(windows) and defined(cpu64)) or defined(lclgtk3))}
  {$error Codebot themes need 64 bit Windows with the win32 widgetset or Linux with the gtk3 widgetset}
{$endif}

interface

uses
  Classes;

{ Installs what the themes need and applies the theme at Index in ThemeList,
  returning false if themes can not be used by this widgetset. Load the
  themes with ThemesLoad before calling it if you want themes of your own. }
function ThemesStart(Index: Integer): Boolean;
{ Takes the theme away and everything that ThemesStart installed, leaving
  the program as it was }
procedure ThemesStop;
{ True between ThemesStart and ThemesStop }
function ThemesStarted: Boolean;
{ Applies the theme at Index in ThemeList }
procedure ThemesApply(Index: Integer);
{ The index in ThemeList of the theme in use }
function ThemesCurrent: Integer;

{ TThemeManager themes every window of the program with the theme named by
  Theme when the program runs. The theme belongs to the whole program rather
  than to the form the component is on. Themes names the themes the program
  can switch between, and the JSON of each one is stored in the form, so the
  program needs no theme files. While the form is designed nothing is
  themed, so the IDE keeps its own theme. }

type
  TThemeManager = class(TComponent)
  private
    FTheme: string;
    FThemes: TStringList;
    { The names and the JSON of the themes read from the form }
    FDataNames: TStringList;
    FDataJson: TStringList;
    procedure SetTheme(const Value: string);
    function GetThemes: TStrings;
    procedure SetThemes(Value: TStrings);
    procedure ThemesChange(Sender: TObject);
    procedure ReadData(Reader: TReader);
    procedure WriteData(Writer: TWriter);
    function ThemeJson(const Name: string): string;
    procedure Apply;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { The name of the theme the program uses. An empty name leaves the
      program with the look of the system. Choosing a theme while the form
      is designed adds it to Themes. }
    property Theme: string read FTheme write SetTheme;
    { The names of the themes stored in the form, which the program can
      switch between with Theme }
    property Themes: TStrings read GetThemes write SetThemes;
  end;

implementation

uses
  SysUtils, Forms, Graphics,
  {$ifdef windows}
  Codebot.Themes.Windows.Colors,
  Codebot.Themes.Windows.Hooks,
  {$endif}
  {$ifdef lclgtk3}
  Codebot.Themes.Gtk3,
  {$endif}
  Codebot.Themes.Palettes;

{ TFontWatcher gives each form the font of the theme when the form is
  created }

type
  TFontWatcher = class
  public
    procedure FormAdded(Sender: TObject; Form: TCustomForm);
  end;

{ The theme in use is kept by its name, as themes added to ThemeList or taken
  out of it move the others }

var
  CurrentName: string;
  Started: Boolean;
  Watcher: TFontWatcher;

{ Controls take the font of their form unless they have one of their own }

procedure ApplyFont(Form: TCustomForm; const Name: string; Size: Integer);
begin
  Form.Font.Name := Name;
  Form.Font.Size := Size;
end;

procedure ApplyFonts(const Name: string; Size: Integer);
var
  I: Integer;
begin
  for I := 0 to Screen.CustomFormCount - 1 do
    ApplyFont(Screen.CustomForms[I], Name, Size);
  Screen.HintFont.Name := Name;
  Screen.HintFont.Size := Size;
end;

procedure TFontWatcher.FormAdded(Sender: TObject; Form: TCustomForm);
var
  I: Integer;
begin
  I := ThemesCurrent;
  ApplyFont(Form, ThemeFontName(I), ThemeList[I].FontSize);
end;

function ThemesCurrent: Integer;
begin
  Result := IndexOfTheme(CurrentName);
  if Result < 0 then
    Result := 0;
end;

function ThemesStarted: Boolean;
begin
  Result := Started;
end;

procedure ThemesApply(Index: Integer);
{$ifdef windows}
var
  Theme: TThemeInfo;
{$endif}
begin
  if (not Started) or (Index < Low(ThemeList)) or (Index > High(ThemeList)) then
    Exit;
  CurrentName := ThemeList[Index].Name;
  {$ifdef windows}
  Theme := ThemeList[Index];
  { The menu font is set first, so that the menus measure their items with
    it when the windows are themed again }
  if ThemeFontName(Index) = 'default' then
    DarkHooksSetMenuFont('')
  else
    DarkHooksSetMenuFont(ThemeFontName(Index));
  if Theme.Custom then
    DarkColorsSetPalette(Theme.Colors, Theme.Dark);
  DarkHooksSetTheme(Theme.Custom);
  {$endif}
  {$ifdef lclgtk3}
  Gtk3ThemesSetTheme(Index);
  {$endif}
  ApplyFonts(ThemeFontName(Index), ThemeList[Index].FontSize);
end;

function ThemesStart(Index: Integer): Boolean;
begin
  Result := Started;
  if Started then
    Exit;
  if (Index < Low(ThemeList)) or (Index > High(ThemeList)) then
    Index := DefaultTheme;
  {$ifdef windows}
  if not DarkColorsInstall then
    Exit;
  { The palette is chosen before the hooks are installed, so that they know
    whether it is dark }
  if ThemeList[Index].Custom then
    DarkColorsSetPalette(ThemeList[Index].Colors, ThemeList[Index].Dark);
  DarkColorsEnable(ThemeList[Index].Custom);
  if not DarkHooksInstall then
  begin
    DarkColorsUninstall;
    Exit;
  end;
  {$endif}
  {$ifdef lclgtk3}
  if not Gtk3ThemesInstall then
    Exit;
  {$endif}
  Started := True;
  Watcher := TFontWatcher.Create;
  Screen.AddHandlerFormAdded(Watcher.FormAdded);
  ThemesApply(Index);
  Result := True;
end;

procedure ThemesStop;
begin
  if not Started then
    Exit;
  Started := False;
  Screen.RemoveHandlerFormAdded(Watcher.FormAdded);
  FreeAndNil(Watcher);
  ApplyFonts('default', 0);
  {$ifdef windows}
  DarkHooksSetTheme(False);
  DarkHooksUninstall;
  DarkColorsUninstall;
  { The brushes the LCL cached while the widgetset had the class of this
    package are made again }
  UpdateHandleObjects(True);
  {$endif}
  {$ifdef lclgtk3}
  Gtk3ThemesUninstall;
  {$endif}
end;

{ TThemeManager }

const
  SystemName = 'System Default';

constructor TThemeManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FThemes := TStringList.Create;
  FThemes.OnChange := ThemesChange;
  FDataNames := TStringList.Create;
  FDataJson := TStringList.Create;
end;

destructor TThemeManager.Destroy;
begin
  FDataJson.Free;
  FDataNames.Free;
  FThemes.Free;
  inherited Destroy;
end;

function TThemeManager.GetThemes: TStrings;
begin
  Result := FThemes;
end;

procedure TThemeManager.SetThemes(Value: TStrings);
begin
  FThemes.Assign(Value);
end;

{ A theme taken out of Themes while the form is designed can no longer be
  the theme of the program }

procedure TThemeManager.ThemesChange(Sender: TObject);
begin
  if (csDesigning in ComponentState) and (not (csLoading in ComponentState)) and
    (FTheme <> '') and (FThemes.IndexOf(FTheme) < 0) then
    FTheme := '';
end;

procedure TThemeManager.SetTheme(const Value: string);
begin
  if FTheme = Value then
    Exit;
  FTheme := Value;
  if csLoading in ComponentState then
    Exit;
  if csDesigning in ComponentState then
  begin
    if (FTheme <> '') and (not SameText(FTheme, SystemName)) and
      (FThemes.IndexOf(FTheme) < 0) then
      FThemes.Add(FTheme);
  end
  else
    Apply;
end;

{ The JSON of the themes is stored in the form as a list of strings, one for
  each name in Themes }

procedure TThemeManager.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('ThemeData', ReadData, WriteData, FThemes.Count > 0);
end;

procedure TThemeManager.ReadData(Reader: TReader);
var
  Json: string;
  Info: TThemeInfo;
begin
  FDataNames.Clear;
  FDataJson.Clear;
  Reader.ReadListBegin;
  while not Reader.EndOfList do
  begin
    Json := Reader.ReadString;
    if ThemeLoadText(Json, Info) then
    begin
      FDataNames.Add(Info.Name);
      FDataJson.Add(Json);
    end;
  end;
  Reader.ReadListEnd;
end;

{ While the form is designed the JSON is taken from the themes the IDE has,
  so a theme changed in its file is stored again the next time the form is
  saved. A theme the IDE does not have keeps the JSON read from the form. }

function TThemeManager.ThemeJson(const Name: string): string;
var
  I: Integer;
begin
  Result := '';
  if csDesigning in ComponentState then
  begin
    I := IndexOfTheme(Name);
    if (I >= 0) and ThemeList[I].Custom then
      Exit(ThemeSaveText(ThemeList[I]));
  end;
  I := FDataNames.IndexOf(Name);
  if I >= 0 then
    Result := FDataJson[I];
end;

procedure TThemeManager.WriteData(Writer: TWriter);
var
  Json: string;
  I: Integer;
begin
  Writer.WriteListBegin;
  for I := 0 to FThemes.Count - 1 do
  begin
    Json := ThemeJson(FThemes[I]);
    if Json <> '' then
      Writer.WriteString(Json);
  end;
  Writer.WriteListEnd;
end;

{ The themes stored in the form are added to ThemeList when the program
  runs, and never while the form is designed, as ThemeList then holds the
  themes of the IDE }

procedure TThemeManager.Loaded;
var
  Info: TThemeInfo;
  I: Integer;
begin
  inherited Loaded;
  if csDesigning in ComponentState then
    Exit;
  for I := 0 to FDataJson.Count - 1 do
    if (FThemes.IndexOf(FDataNames[I]) >= 0) and
      ThemeLoadText(FDataJson[I], Info) then
      ThemeAdd(Info);
  Apply;
end;

{ Themes are only installed once a custom theme is chosen, so a program
  whose theme is empty, System Default, or unknown is left as the system
  draws it }

procedure TThemeManager.Apply;
var
  Index: Integer;
begin
  if csDesigning in ComponentState then
    Exit;
  Index := IndexOfTheme(FTheme);
  if Index < 0 then
    Index := IndexOfTheme(SystemName);
  if Started then
    ThemesApply(Index)
  else if (Index >= 0) and ThemeList[Index].Custom then
    ThemesStart(Index);
end;

end.
