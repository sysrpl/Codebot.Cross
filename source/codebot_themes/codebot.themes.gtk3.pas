(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ Codebot.Themes.Gtk3 themes every window of the program with the GTK3
  widgetset by adding one style sheet to the screen. GTK restyles every
  widget when the style sheet changes, including the widgets created later,
  so nothing is patched. The system colors the LCL paints with are set to
  the colors of the palette after the widgetset reads them again, so that
  the controls the LCL draws itself match the controls GTK draws. }

{ <include docs/codebot.themes.gtk3.txt> }
unit Codebot.Themes.Gtk3;

{$mode delphi}

interface
{$ifdef lclgtk3}

{ Adds the style sheet to the screen, returning false if the widgetset is
  not GTK3 }
function Gtk3ThemesInstall: Boolean;
{ Removes the style sheet and gives the windows the colors of the system
  again }
procedure Gtk3ThemesUninstall;
{ Themes every window with the theme at Index in ThemeList }
procedure Gtk3ThemesSetTheme(Index: Integer);

{$endif}

implementation
{$ifdef lclgtk3}

uses
  SysUtils, Classes, AVL_Tree, Controls, Forms, Graphics, LCLType, LCLIntf,
  LCLPlatformDef, LCLResCache, InterfaceBase, WSControls,
  LazGtk3, LazGdk3, LazGlib2, LazGObject2, Gtk3Procs, Gtk3Objects,
  Codebot.Themes.Palettes;

const
  { Above the theme of the system and below the style sheets the widgetset
    gives single widgets, so that a control with a color of its own keeps
    it }
  ThemePriority = GTK_STYLE_PROVIDER_PRIORITY_APPLICATION - 1;
  DarkSetting = 'gtk-application-prefer-dark-theme';
  ThemeSetting = 'gtk-theme-name';
  { The rules of the style sheet, which use the colors the style sheet
    defines before them. Each color of the palette is named cb_ followed by
    its name in ColorNames. }
  ThemeRules =
    'window, window.background, dialog, .background {' +
    '  background-color: @cb_btnFace; color: @cb_btnText; }' +
    'headerbar, .titlebar {' +
    '  background-image: none; background-color: @cb_activeCaption;' +
    '  color: @cb_captionText; border-color: @cb_activeBorder; box-shadow: none; }' +
    'headerbar:backdrop, .titlebar:backdrop {' +
    '  background-color: @cb_inactiveCaption; color: @cb_inactiveCaptionText; }' +
    'label:disabled { color: @cb_grayText; }' +
    'separator, paned > separator { background-color: @cb_btnShadow; }' +
    'frame > border, scrolledwindow.frame, .frame { border-color: @cb_windowFrame; }' +
    { Menus }
    'menubar, .menubar {' +
    '  background-image: none; background-color: @cb_menuBar; color: @cb_menuText;' +
    '  border-color: @cb_windowFrame; box-shadow: none; }' +
    'menubar > menuitem { color: @cb_menuText; }' +
    'menubar > menuitem:hover {' +
    '  background-color: @cb_menuHilight; color: @cb_menuText; box-shadow: none; }' +
    'window.popup, menu, .menu, .context-menu {' +
    '  background-image: none; background-color: @cb_menu; color: @cb_menuText;' +
    '  border-color: @cb_windowFrame; }' +
    { A popup menu is drawn by its window, whose outline is a shadow when the
      windows are composited }
    '.csd.popup decoration, window.popup decoration {' +
    '  box-shadow: 0 0 0 1px @cb_windowFrame, 0 2px 4px alpha(black, 0.15); }' +
    'menu menuitem, .menu menuitem { color: @cb_menuText; }' +
    'menu menuitem:hover, .menu menuitem:hover {' +
    '  background-color: @cb_menuHilight; color: @cb_menuText; }' +
    'menu menuitem:disabled, menu menuitem:disabled label { color: @cb_grayText; }' +
    'menu separator, .menu separator { background-color: @cb_btnShadow; }' +
    'menu arrow, menu check, menu radio { color: @cb_menuText; }' +
    { The arrows which scroll a menu too long for the screen }
    'menu > arrow {' +
    '  background-image: none; background-color: @cb_menu; color: @cb_menuText;' +
    '  border-color: @cb_btnShadow; box-shadow: none; }' +
    'menu > arrow:hover { background-color: @cb_menuHilight; }' +
    'menu > arrow:disabled { background-color: @cb_menu; color: @cb_grayText; }' +
    'tooltip, tooltip.background {' +
    '  background-image: none; background-color: @cb_infoBk; color: @cb_infoText;' +
    '  border: 1px solid @cb_windowFrame; box-shadow: none; text-shadow: none; }' +
    'tooltip * { color: @cb_infoText; }' +
    { Buttons }
    'button {' +
    '  background-image: none; background-color: @cb_btnFace; color: @cb_btnText;' +
    '  border-color: @cb_windowFrame; box-shadow: none; text-shadow: none;' +
    '  -gtk-icon-shadow: none; outline-color: alpha(@cb_btnText, 0.3); }' +
    'button:hover { background-color: @cb_btnHighlight; }' +
    'button.flat { background-color: transparent; border-color: transparent; }' +
    'button.flat:hover { background-color: @cb_btnHighlight; }' +
    'toolbar button, .toolbar button { border-color: transparent; }' +
    'button:active, button:checked {' +
    '  background-color: mix(@cb_btnFace, @cb_highlight, 0.35); }' +
    'button:disabled, button:disabled label {' +
    '  background-color: @cb_btnFace; color: @cb_grayText; }' +
    'check, radio {' +
    '  background-image: none; background-color: @cb_window; color: @cb_windowText;' +
    '  border-color: mix(@cb_windowFrame, @cb_windowText, 0.3); box-shadow: none; }' +
    'check:checked, radio:checked, check:indeterminate, radio:indeterminate {' +
    '  background-color: @cb_highlight; color: @cb_highlightText;' +
    '  border-color: @cb_highlight; }' +
    'check:disabled, radio:disabled { background-color: @cb_btnFace; color: @cb_grayText; }' +
    { Edits and lists }
    'entry, spinbutton, textview, textview text {' +
    '  background-image: none; background-color: @cb_window; color: @cb_windowText;' +
    '  border-color: @cb_windowFrame; box-shadow: none; caret-color: @cb_windowText; }' +
    'entry:disabled, spinbutton:disabled { background-color: @cb_btnFace; color: @cb_grayText; }' +
    'selection, entry selection, textview text selection, label selection {' +
    '  background-color: @cb_highlight; color: @cb_highlightText; }' +
    'treeview, treeview.view, iconview, .view, list, row, calendar {' +
    '  background-color: @cb_window; color: @cb_windowText; }' +
    'treeview:selected, .view:selected, row:selected, calendar:selected {' +
    '  background-color: @cb_highlight; color: @cb_highlightText; }' +
    'treeview header button {' +
    '  background-color: @cb_btnFace; color: @cb_btnText; border-color: @cb_windowFrame; }' +
    { Tabs }
    'notebook, notebook > stack { background-color: @cb_btnFace; }' +
    'notebook > header {' +
    '  background-image: none; background-color: @cb_appWorkspace;' +
    '  border-color: @cb_windowFrame; box-shadow: none; }' +
    'notebook > header tab {' +
    '  background-color: transparent; color: @cb_grayText; border-color: transparent; }' +
    'notebook > header tab:hover { background-color: @cb_btnHighlight; color: @cb_btnText; }' +
    'notebook > header tab:checked {' +
    '  background-color: @cb_btnHighlight; color: @cb_btnText;' +
    '  border-color: @cb_windowFrame; }' +
    'notebook > header tab label { color: inherit; }' +
    'notebook > header.top tab:checked { box-shadow: inset 0 -3px @cb_highlight; }' +
    'notebook > header.bottom tab:checked { box-shadow: inset 0 3px @cb_highlight; }' +
    'notebook > header.left tab:checked { box-shadow: inset -3px 0 @cb_highlight; }' +
    'notebook > header.right tab:checked { box-shadow: inset 3px 0 @cb_highlight; }' +
    { Bars }
    'scrollbar { background-image: none; background-color: @cb_scrollBar;' +
    '  border-color: @cb_windowFrame; }' +
    'scrollbar slider { background-color: mix(@cb_scrollBar, @cb_btnText, 0.35); }' +
    'scrollbar slider:hover { background-color: mix(@cb_scrollBar, @cb_btnText, 0.5); }' +
    'scrollbar slider:active { background-color: @cb_highlight; }' +
    'progressbar trough, scale trough, levelbar trough {' +
    '  background-image: none; background-color: @cb_scrollBar;' +
    '  border-color: @cb_windowFrame; }' +
    'progressbar progress, scale highlight, levelbar block.filled {' +
    '  background-image: none; background-color: @cb_highlight;' +
    '  border-color: @cb_highlight; }' +
    'scale slider {' +
    '  background-image: none; background-color: @cb_btnHighlight;' +
    '  border-color: @cb_windowFrame; box-shadow: none; }' +
    'actionbar, actionbar > revealer > box, .action-bar, searchbar,' +
    '  searchbar > revealer > box, .inline-toolbar, infobar, infobar > revealer > box {' +
    '  background-image: none; background-color: @cb_btnFace; color: @cb_btnText;' +
    '  border-color: @cb_windowFrame; box-shadow: none; }' +
    { The places at the side of the file dialog }
    '.sidebar, placessidebar, placessidebar list, placessidebar row,' +
    '  stacksidebar, stacksidebar list, stacksidebar row {' +
    '  background-color: @cb_btnFace; color: @cb_btnText; border-color: @cb_windowFrame; }' +
    'placessidebar row:hover, stacksidebar row:hover { background-color: @cb_btnHighlight; }' +
    'placessidebar row:selected, stacksidebar row:selected {' +
    '  background-color: @cb_highlight; color: @cb_highlightText; }' +
    'placessidebar row:disabled label { color: @cb_grayText; }' +
    'popover, popover.background {' +
    '  background-image: none; background-color: @cb_menu; color: @cb_menuText;' +
    '  border-color: @cb_windowFrame; box-shadow: 0 2px 4px alpha(black, 0.15); }' +
    'statusbar, toolbar, .toolbar {' +
    '  background-image: none; background-color: @cb_btnFace; color: @cb_btnText;' +
    '  border-color: transparent; }';

var
  Installed: Boolean;
  Provider: PGtkCssProvider;
  { A copy of the theme in use, as themes added to ThemeList or taken out of
    it move the others }
  Current: TThemeInfo;
  { The system colors and the dark setting as they were when the style sheet
    was added }
  SavedColors: array[0..MAX_SYS_COLORS] of DWORD;
  SavedDark: Boolean;
  { The callback which sets the system colors once the widgetset has read
    them again }
  IdleSource: guint;

{ Converts a color of the LCL, which is $BBGGRR, to $RRGGBB }

function SwapColor(C: DWORD): LongInt;
begin
  Result := ((C and $FF) shl 16) or (C and $FF00) or ((C shr 16) and $FF);
end;

{ Colors are written as #RRGGBB }

function CssColor(C: TColor): string;
begin
  C := ColorToRGB(C);
  Result := Format('#%.2x%.2x%.2x', [Red(C), Green(C), Blue(C)]);
end;

{ The colors the widgetset looks up by name are defined along with the
  colors of the palette, so that what it reads matches what is drawn }

function ThemeSheet(Index: Integer): string;
var
  Colors: TStringList;
  Name: string;
  I: Integer;

  procedure Define(const ColorName: string; ColorIndex: Integer);
  begin
    Colors.Add(Format('@define-color %s %s;',
      [ColorName, CssColor(ThemeColor(Index, ColorIndex))]));
  end;

begin
  Colors := TStringList.Create;
  try
    for I := Low(ColorNames) to High(ColorNames) do
      if ColorNames[I] <> '' then
        Define('cb_' + ColorNames[I], I);
    Define('theme_bg_color', COLOR_BTNFACE);
    Define('theme_fg_color', COLOR_BTNTEXT);
    Define('theme_base_color', COLOR_WINDOW);
    Define('theme_text_color', COLOR_WINDOWTEXT);
    Define('theme_selected_bg_color', COLOR_HIGHLIGHT);
    Define('theme_selected_fg_color', COLOR_HIGHLIGHTTEXT);
    Define('insensitive_bg_color', COLOR_BTNFACE);
    Define('insensitive_fg_color', COLOR_GRAYTEXT);
    Define('theme_unfocused_bg_color', COLOR_INACTIVECAPTION);
    Define('theme_unfocused_fg_color', COLOR_INACTIVECAPTIONTEXT);
    Define('theme_unfocused_base_color', COLOR_WINDOW);
    Define('theme_unfocused_text_color', COLOR_WINDOWTEXT);
    Define('theme_unfocused_selected_bg_color', COLOR_HIGHLIGHT);
    Define('theme_unfocused_selected_fg_color', COLOR_HIGHLIGHTTEXT);
    Define('theme_tooltip_bg_color', COLOR_INFOBK);
    Define('theme_tooltip_fg_color', COLOR_INFOTEXT);
    Define('borders', COLOR_WINDOWFRAME);
    Define('unfocused_borders', COLOR_INACTIVEBORDER);
    Result := Colors.Text + ThemeRules;
  finally
    Colors.Free;
  end;
  if ThemeList[Index].ButtonRadius >= 0 then
    Result := Result + Format('button { border-radius: %dpx; }',
      [ThemeList[Index].ButtonRadius]);
  { Controls with a font of their own keep it, as the widgetset gives them
    style sheets with a higher priority }
  Name := ThemeFontName(Index);
  if Name <> 'default' then
    Result := Result + Format('* { font-family: "%s"; }', [Name]);
  if ThemeList[Index].FontSize > 0 then
    Result := Result + Format('* { font-size: %dpt; }',
      [ThemeList[Index].FontSize]);
end;

function GetDarkSetting: Boolean;
var
  Value: TGValue;
begin
  FillByte(Value, SizeOf(Value), 0);
  Value.init(G_TYPE_BOOLEAN);
  g_object_get_property(PGObject(gtk_settings_get_default), DarkSetting, @Value);
  Result := Value.get_boolean;
  Value.unset;
end;

procedure SetDarkSetting(Dark: Boolean);
var
  Value: TGValue;
begin
  if GetDarkSetting = Dark then
    Exit;
  FillByte(Value, SizeOf(Value), 0);
  Value.init(G_TYPE_BOOLEAN);
  Value.set_boolean(Dark);
  g_object_set_property(PGObject(gtk_settings_get_default), DarkSetting, @Value);
  Value.unset;
end;

{ The widgetset reads the system colors from a widget of each kind, which it
  makes when a color is first asked for. They are all made now, so that none
  is made later and reads over the colors of the palette. }

procedure ReadSysColors;
const
  Kinds: array[0..11] of TLazGtkStyle = (lgsButton, lgsCheckbox,
    lgsRadiobutton, lgsWindow, lgsMenu, lgsMenuBar, lgsMenuitem,
    lgsVerticalScrollbar, lgsHorizontalScrollbar, lgsTooltip, lgsMemo,
    lgsFrame);
var
  Kind: TLazGtkStyle;
begin
  ReleaseAllStyles;
  for Kind in Kinds do
    GetStyleWidget(Kind);
end;

{ The cached brushes of the system colors are given the colors again }

procedure UpdateBrushes;
var
  Brush: HBRUSH;
  I: Integer;
begin
  for I := 0 to MAX_SYS_COLORS do
  begin
    Brush := GetSysColorBrush(I);
    if (Brush = 0) or (Brush = HBRUSH(-1)) then
      Continue;
    TGtk3Brush(Brush).Context := nil;
    TGtk3Brush(Brush).Color := TColor(SysColorMap[I]);
  end;
end;

{ TPenCache gives access to the pens the LCL keeps for reuse }

type
  TPenCache = class(TResourceCache);

{ The widgetset turns the system color of a pen into a fixed color when the
  pen is made, and the LCL keeps pens for reuse by the color they were asked
  for, so the pens of system colors are given the colors again }

procedure UpdatePens;
var
  Node: TAvlTreeNode;
  Pen: TGtk3Pen;
begin
  PenResourceCache.Lock;
  try
    Node := TPenCache(PenResourceCache).FItems.FindLowest;
    while Node <> nil do
    begin
      Pen := TGtk3Pen(TResourceCacheItem(Node.Data).Handle);
      if (Pen <> nil) and IsSysColor(Pen.LogPen.lopnColor) then
      begin
        Pen.Context := nil;
        Pen.Color := ColorToRGB(TColor(Pen.LogPen.lopnColor));
      end;
      Node := TPenCache(PenResourceCache).FItems.FindSuccessor(Node);
    end;
  finally
    PenResourceCache.Unlock;
  end;
end;

{ The widgetset turns a system color of a control into a fixed color when
  the color is set, so the color is set again. System colors are negative,
  and clDefault is not. }

procedure UpdateControl(Control: TWinControl);
var
  I: Integer;
begin
  if not Control.HandleAllocated then
    Exit;
  if Control.Color < 0 then
    TWSWinControlClass(Control.WidgetSetClass).SetColor(Control);
  Control.Invalidate;
  for I := 0 to Control.ControlCount - 1 do
    if Control.Controls[I] is TWinControl then
      UpdateControl(TWinControl(Control.Controls[I]));
end;

procedure UpdateControls;
var
  I: Integer;
begin
  for I := 0 to Screen.CustomFormCount - 1 do
    UpdateControl(Screen.CustomForms[I]);
end;

{ This runs after the widgetset has read the system colors from the style
  sheet, which it does when it is told the theme of GTK changed }

function ThemeIdle(Data: gpointer): gboolean; cdecl;
var
  I: Integer;
begin
  IdleSource := 0;
  Result := False;
  if not Installed then
    Exit;
  ReadSysColors;
  if Current.Custom then
    for I := 0 to MAX_SYS_COLORS do
      if Current.Colors[I] <> NoColor then
        SysColorMap[I] := DWORD(RGBToColor((Current.Colors[I] shr 16) and $FF,
          (Current.Colors[I] shr 8) and $FF, Current.Colors[I] and $FF));
  UpdateBrushes;
  UpdatePens;
  UpdateControls;
end;

{ The widgetset reads the system colors again and paints every window again
  when the name of the GTK theme changes, so it is told the name changed
  without changing it }

procedure NotifyThemeChanged;
begin
  g_object_notify(PGObject(gtk_settings_get_default), ThemeSetting);
end;

function Gtk3ThemesInstall: Boolean;
var
  I: Integer;
begin
  Result := Installed;
  if Installed then
    Exit;
  if (WidgetSet = nil) or (WidgetSet.LCLPlatform <> lpGtk3) or
    (gdk_screen_get_default = nil) then
    Exit;
  { The widgetset reads every system color it knows before they are saved,
    and the dialog shows them for the theme of the system }
  ReadSysColors;
  for I := 0 to MAX_SYS_COLORS do
  begin
    SavedColors[I] := SysColorMap[I];
    if SystemColors[I] <> NoColor then
      SystemColors[I] := SwapColor(SysColorMap[I]);
  end;
  SavedDark := GetDarkSetting;
  Provider := gtk_css_provider_new;
  gtk_style_context_add_provider_for_screen(gdk_screen_get_default,
    PGtkStyleProvider(Provider), ThemePriority);
  Current := ThemeList[0];
  Installed := True;
  Result := True;
end;

procedure Gtk3ThemesUninstall;
var
  I: Integer;
begin
  if not Installed then
    Exit;
  Installed := False;
  { Nothing may call into the package once it is gone }
  if IdleSource <> 0 then
    g_source_remove(IdleSource);
  IdleSource := 0;
  gtk_style_context_remove_provider_for_screen(gdk_screen_get_default,
    PGtkStyleProvider(Provider));
  g_object_unref(Provider);
  Provider := nil;
  SetDarkSetting(SavedDark);
  for I := 0 to MAX_SYS_COLORS do
    SysColorMap[I] := SavedColors[I];
  ReadSysColors;
  UpdateBrushes;
  UpdatePens;
  UpdateControls;
  NotifyThemeChanged;
end;

procedure Gtk3ThemesSetTheme(Index: Integer);
var
  Sheet: string;
  I: Integer;
begin
  if (not Installed) or (Index < Low(ThemeList)) or (Index > High(ThemeList)) then
    Exit;
  Current := ThemeList[Index];
  { Colors the widgetset does not read again keep the colors of the system }
  for I := 0 to MAX_SYS_COLORS do
    SysColorMap[I] := SavedColors[I];
  if ThemeList[Index].Custom then
  begin
    Sheet := ThemeSheet(Index);
    SetDarkSetting(ThemeList[Index].Dark);
  end
  else
  begin
    Sheet := '';
    SetDarkSetting(SavedDark);
  end;
  gtk_css_provider_load_from_data(Provider, PChar(Sheet), -1, nil);
  NotifyThemeChanged;
  { The widgetset reads the colors in a callback of its own, which must run
    before this one, so this one is added again after it }
  if IdleSource <> 0 then
    g_source_remove(IdleSource);
  IdleSource := g_idle_add(ThemeIdle, nil);
end;

{$endif}

end.
