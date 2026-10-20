(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ Codebot.Themes.Manager applies the themes of Codebot.Themes.Palettes to
  the IDE, and remembers the theme chosen between sessions of the IDE }

unit Codebot.Themes.Manager;

{$mode delphi}

interface

{ Installs what the themes need and applies the theme saved by ThemesSave,
  returning false if themes can not be used on this system }
function ThemesStart: Boolean;
{ Takes the theme away and everything that ThemesStart installed, leaving
  the IDE as it was }
procedure ThemesStop;
{ Applies the theme at Index in ThemeList }
procedure ThemesApply(Index: Integer);
{ The index in ThemeList of the theme in use }
function ThemesCurrent: Integer;
{ Saves the name of the theme in use to the configuration of the IDE }
procedure ThemesSave;

implementation

uses
  SysUtils, Classes, Forms, Graphics, BaseIDEIntf, LazConfigStorage,
  LazIDEIntf, PackageIntf,
  {$ifdef windows}
  Codebot.Themes.Windows.Colors,
  Codebot.Themes.Windows.Hooks,
  {$endif}
  Codebot.Themes.Palettes;

const
  ConfigFile = 'codebotthemes.xml';
  ConfigPath = 'Theme/Name';
  PackageName = 'codebot_themes_design';
  { The folder of theme files beside the package, and the folder of the
    themes of the user in the configuration folder of the IDE }
  PackageFolder = 'themes';
  UserFolder = 'codebotthemes';

{ TFontWatcher gives each form of the IDE the font of the theme when the form
  is created }

type
  TFontWatcher = class
  public
    procedure FormAdded(Sender: TObject; Form: TCustomForm);
  end;

var
  Current: Integer;
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
begin
  ApplyFont(Form, ThemeList[Current].FontName, ThemeList[Current].FontSize);
end;

{ The themes of the user are loaded last, so that they replace the themes
  of the package with the same name }

procedure LoadThemes;
var
  Package: TIDEPackage;
  Folders: array[0..1] of string;
begin
  Folders[0] := '';
  Folders[1] := '';
  if PackageEditingInterface <> nil then
  begin
    Package := PackageEditingInterface.FindPackageWithName(PackageName);
    if Package <> nil then
      Folders[0] := IncludeTrailingPathDelimiter(Package.DirectoryExpanded) +
        PackageFolder;
  end;
  if LazarusIDE <> nil then
    Folders[1] := IncludeTrailingPathDelimiter(LazarusIDE.GetPrimaryConfigPath) +
      UserFolder;
  ThemesLoad(Folders);
end;

{ A file which is missing or can not be read leaves the default theme }

function LoadThemeName: string;
var
  Storage: TConfigStorage;
begin
  Result := ThemeList[DefaultTheme].Name;
  if not Assigned(GetIDEConfigStorage) then
    Exit;
  try
    Storage := GetIDEConfigStorage(ConfigFile, True);
    try
      Result := Storage.GetValue(ConfigPath, Result);
    finally
      Storage.Free;
    end;
  except
    Result := ThemeList[DefaultTheme].Name;
  end;
end;

procedure ThemesSave;
var
  Storage: TConfigStorage;
begin
  if not Assigned(GetIDEConfigStorage) then
    Exit;
  Storage := GetIDEConfigStorage(ConfigFile, True);
  try
    Storage.SetValue(ConfigPath, ThemeList[Current].Name);
    Storage.WriteToDisk;
  finally
    Storage.Free;
  end;
end;

function ThemesCurrent: Integer;
begin
  Result := Current;
end;

procedure ThemesApply(Index: Integer);
{$ifdef windows}
var
  Theme: TThemeInfo;
{$endif}
begin
  if (not Started) or (Index < Low(ThemeList)) or (Index > High(ThemeList)) then
    Exit;
  Current := Index;
  {$ifdef windows}
  Theme := ThemeList[Index];
  { The menu font is set first, so that the menus measure their items with
    it when the windows are themed again }
  if Theme.FontName = 'default' then
    DarkHooksSetMenuFont('')
  else
    DarkHooksSetMenuFont(Theme.FontName);
  if Theme.Custom then
    DarkColorsSetPalette(Theme.Colors, Theme.Dark);
  DarkHooksSetTheme(Theme.Custom);
  {$endif}
  ApplyFonts(ThemeList[Index].FontName, ThemeList[Index].FontSize);
end;

function ThemesStart: Boolean;
begin
  Result := Started;
  if Started then
    Exit;
  {$ifdef windows}
  if not DarkColorsInstall then
    Exit;
  LoadThemes;
  Current := FindTheme(LoadThemeName);
  { The palette is chosen before the hooks are installed, so that they know
    whether it is dark }
  if ThemeList[Current].Custom then
    DarkColorsSetPalette(ThemeList[Current].Colors, ThemeList[Current].Dark);
  DarkColorsEnable(ThemeList[Current].Custom);
  if not DarkHooksInstall then
  begin
    DarkColorsUninstall;
    Exit;
  end;
  Started := True;
  Watcher := TFontWatcher.Create;
  Screen.AddHandlerFormAdded(Watcher.FormAdded);
  ThemesApply(Current);
  Result := True;
  {$endif}
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
end;

end.
