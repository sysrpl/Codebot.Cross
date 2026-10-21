(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ Codebot.Themes.Registration applies the saved theme to the windows of the
  IDE when the package is registered, adds the Themes item to the Tools
  menu, and takes the theme away again before the package is unloaded. It
  also adds TThemeManager to the component palette, with the editors which
  choose its themes. }

{ <include docs/codebot.themes.registration.txt> }
unit Codebot.Themes.Registration;

{$mode delphi}

interface

uses
  Classes, PropEdits, ComponentEditors,
  Codebot.Themes.Manager;

{ TThemeManagerEditor shows the dialog which chooses the themes of a
  TThemeManager when the component is double clicked }

type
  TThemeManagerEditor = class(TComponentEditor)
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

{ TThemeNamePropertyEditor lists every theme the IDE has for the Theme
  property of a TThemeManager, and shows the dialog from its button.
  Choosing a theme stores it in the form. }

  TThemeNamePropertyEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure Edit; override;
  end;

{ TThemeListPropertyEditor shows the dialog for the Themes property of a
  TThemeManager, and the names of the themes as its value }

  TThemeListPropertyEditor = class(TClassPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: ansistring; override;
    procedure Edit; override;
  end;

procedure Register;

implementation

uses
  SysUtils, MenuIntf, ModuleUnloadIntf,
  {$ifdef windows}
  Codebot.Themes.Windows.Hooks,
  {$endif}
  Codebot.Themes.Palettes,
  Codebot.Themes.Editor,
  Codebot.Themes.Install,
  Codebot.Themes.Dialog,
  Codebot.Themes.Picker;

{ Applies and saves the theme chosen. The color scheme and the font of the
  source editor are saved by the IDE with its other editor options, and are
  only applied when they change, as applying them reloads every editor. }

procedure ApplyChoice(const Choice: TThemeChoice);
begin
  if Choice.Theme <> ThemesCurrent then
  begin
    ThemesApply(Choice.Theme);
    ThemesSaveName;
  end;
  EditorApply(Choice.Scheme, Choice.FontName, Choice.FontSize);
end;

{ Shows the dialog, and applies the choice made if the user pressed OK }

procedure ThemesClick(Sender: TObject);
var
  Choice: TThemeChoice;
begin
  Choice.Theme := ThemesCurrent;
  Choice.Scheme := EditorSchemeCurrent;
  Choice.FontName := EditorFontName;
  Choice.FontSize := EditorFontSize;
  if ShowThemesDialog(Choice, ApplyChoice) then
    ApplyChoice(Choice);
end;

{ Shows the dialog which chooses the themes of a TThemeManager, returning
  true if they were changed. The themes are set before the theme, as the
  theme must be one of them. }

function EditThemeManager(Manager: TThemeManager): Boolean;
var
  Names: TStringList;
  Theme: string;
begin
  Names := TStringList.Create;
  try
    Names.Assign(Manager.Themes);
    Theme := Manager.Theme;
    Result := ShowThemePicker(Names, Theme);
    if Result then
    begin
      Manager.Themes := Names;
      Manager.Theme := Theme;
    end;
  finally
    Names.Free;
  end;
end;

{ TThemeManagerEditor }

procedure TThemeManagerEditor.Edit;
begin
  if EditThemeManager(Component as TThemeManager) then
    Modified;
end;

procedure TThemeManagerEditor.ExecuteVerb(Index: Integer);
begin
  Edit;
end;

function TThemeManagerEditor.GetVerb(Index: Integer): string;
begin
  Result := 'Edit Themes ...';
end;

function TThemeManagerEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TThemeNamePropertyEditor }

function TThemeNamePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paValueList, paRevertable];
end;

procedure TThemeNamePropertyEditor.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := Low(ThemeList) to High(ThemeList) do
    Proc(ThemeList[I].Name);
end;

procedure TThemeNamePropertyEditor.Edit;
begin
  if EditThemeManager(GetComponent(0) as TThemeManager) then
    Modified;
end;

{ TThemeListPropertyEditor }

function TThemeListPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TThemeListPropertyEditor.GetValue: ansistring;
var
  Names: TStrings;
  I: Integer;
begin
  Names := TStrings(GetObjectValue);
  Result := '';
  if Names <> nil then
    for I := 0 to Names.Count - 1 do
      if Result = '' then
        Result := Names[I]
      else
        Result := Result + ', ' + Names[I];
  if Result = '' then
    Result := '(none)';
end;

procedure TThemeListPropertyEditor.Edit;
begin
  if EditThemeManager(GetComponent(0) as TThemeManager) then
    Modified;
end;

{ An IDE built from dynamic packages calls this before it unloads a library.
  When the library is this package, the theme is taken away, as nothing may
  call into the library once it is gone. When it is another library, the
  slots patched in it are forgotten. The IDE removes the menu item itself. }

procedure ModuleUnloading(Module: HMODULE);
begin
  if AddressInModule(Module, @ModuleUnloading) then
  begin
    RemoveModuleUnloadHandler(ModuleUnloading);
    ThemesStop;
  end
  {$ifdef windows}
  else
    DarkHooksModuleUnloaded(Module)
  {$endif};
end;

{ The windows of the IDE exist when the package is registered, so the theme
  is applied to every window that is there. The menu item is not added when
  themes can not be used on this system, but the component still is, as the
  programs it is used in may run elsewhere. }

procedure Register;
begin
  RegisterComponents('Codebot Themes', [TThemeManager]);
  RegisterPropertyEditor(TypeInfo(string), TThemeManager, 'Theme',
    TThemeNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TThemeManager, 'Themes',
    TThemeListPropertyEditor);
  RegisterComponentEditor(TThemeManager, TThemeManagerEditor);
  ThemesReload;
  if not ThemesStart(FindTheme(ThemesLoadName)) then
    Exit;
  AddModuleUnloadHandler(ModuleUnloading);
  RegisterIDEMenuCommand(itmOptionsDialogs, 'CodebotThemesItem', 'Themes ...',
    nil, ThemesClick);
end;

end.
