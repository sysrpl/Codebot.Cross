(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ Codebot.Themes.Registration applies the saved theme to the windows of the
  IDE when the package is registered, adds the Themes item to the Tools
  menu, and takes the theme away again before the package is unloaded }

{ <include docs/codebot.themes.registration.txt> }
unit Codebot.Themes.Registration;

{$mode delphi}

interface

procedure Register;

implementation

uses
  MenuIntf, ModuleUnloadIntf,
  {$ifdef windows}
  Codebot.Themes.Windows.Hooks,
  {$endif}
  Codebot.Themes.Manager,
  Codebot.Themes.Editor,
  Codebot.Themes.Dialog;

{ Applies and saves the theme chosen. The color scheme and the font of the
  source editor are saved by the IDE with its other editor options, and are
  only applied when they change, as applying them reloads every editor. }

procedure ApplyChoice(const Choice: TThemeChoice);
begin
  if Choice.Theme <> ThemesCurrent then
  begin
    ThemesApply(Choice.Theme);
    ThemesSave;
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
  is applied to every window that is there. Nothing is added when themes can
  not be used on this system. }

procedure Register;
begin
  if not ThemesStart then
    Exit;
  AddModuleUnloadHandler(ModuleUnloading);
  RegisterIDEMenuCommand(itmOptionsDialogs, 'CodebotThemesItem', 'Themes ...',
    nil, ThemesClick);
end;

end.
