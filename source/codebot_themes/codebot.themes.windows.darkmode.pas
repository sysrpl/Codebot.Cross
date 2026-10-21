(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ Codebot.Themes.Windows.DarkMode calls the undocumented routines of
  uxtheme.dll which switch the native controls of Windows 10 1809 and later
  between their light and dark visual styles }

{ <include docs/codebot.themes.windows.darkmode.txt> }
unit Codebot.Themes.Windows.DarkMode;

{$mode delphi}

interface
{$ifdef windows}

uses
  Windows, SysUtils;

{ TPreferredAppMode is the mode of the whole program. pamAllowDark follows
  the app mode chosen in the settings of Windows. }

type
  TPreferredAppMode = (pamDefault, pamAllowDark, pamForceDark, pamForceLight);

{ The build number of Windows, read with RtlGetVersion so that it is correct
  without a manifest }
function WindowsBuild: LongWord;
{ Loads the routines from uxtheme.dll by ordinal, returning false if Windows
  is too old or one is missing }
function DarkModeLoad: Boolean;
{ Describes which routines were found, for the log }
function DarkModeReport: string;
{ True if the settings of Windows choose dark apps }
function ShouldAppsUseDarkMode: Boolean;
{ Sets the mode of the whole program and refreshes the theme of menus }
procedure SetAppMode(Mode: TPreferredAppMode);
{ True if Mode results in dark controls }
function AppModeIsDark(Mode: TPreferredAppMode): Boolean;
{ Switches the title bar of a top level window }
procedure SetTitleBarDark(Wnd: HWND; Dark: Boolean);
{ True if ClassName is the class of a combo box. The LCL registers its combo
  boxes under its own class, LCLComboBox followed by a suffix. }
function IsComboBoxClass(const ClassName: string): Boolean;
{ Switches one window to the dark parts of its visual style }
procedure ThemeWindow(Wnd: HWND; Dark: Boolean);
{ Switches a top level window, its title bar, and all of its child windows }
procedure SetWindowDarkMode(Wnd: HWND; Dark: Boolean);

{$endif}

implementation
{$ifdef windows}

const
  { Windows 10 1809, the first build with dark visual styles }
  BuildDarkMode = 17763;
  { Windows 10 1903, where ordinal 135 became SetPreferredAppMode }
  BuildPreferredAppMode = 18362;
  { Windows 10 20H1, where the title bar attribute moved from 19 to 20 }
  BuildImmersiveDarkMode = 18985;
  DWMWA_USE_IMMERSIVE_DARK_MODE_OLD = 19;
  DWMWA_USE_IMMERSIVE_DARK_MODE = 20;

function RtlGetVersion(var Info: TOSVersionInfoW): LongInt; stdcall;
  external 'ntdll.dll';
function DwmSetWindowAttribute(Wnd: HWND; Attribute: DWORD; Value: Pointer;
  Size: DWORD): HRESULT; stdcall; external 'dwmapi.dll';
function SetWindowTheme(Wnd: HWND; SubAppName, SubIdList: PWideChar): HRESULT;
  stdcall; external 'uxtheme.dll';

var
  Loaded: Boolean;
  LoadTried: Boolean;
  Build: LongWord;
  { Ordinal 104 }
  RefreshImmersiveColorPolicyState: procedure; stdcall;
  { Ordinal 132 }
  ShouldAppsUseDarkModeProc: function: Boolean; stdcall;
  { Ordinal 133 }
  AllowDarkModeForWindow: function(Wnd: HWND; Allow: Boolean): Boolean; stdcall;
  { Ordinal 135 on 1809 }
  AllowDarkModeForApp: function(Allow: Boolean): Boolean; stdcall;
  { Ordinal 135 on 1903 and later }
  SetPreferredAppMode: function(Mode: Integer): Integer; stdcall;
  { Ordinal 136 }
  FlushMenuThemes: procedure; stdcall;

function WindowsBuild: LongWord;
var
  Info: TOSVersionInfoW;
begin
  FillChar(Info, SizeOf(Info), 0);
  Info.dwOSVersionInfoSize := SizeOf(Info);
  if (RtlGetVersion(Info) = 0) and (Info.dwMajorVersion >= 10) then
    Result := Info.dwBuildNumber
  else
    Result := 0;
end;

function DarkModeLoad: Boolean;

  function Find(Lib: HMODULE; Ordinal: Word): Pointer;
  begin
    Result := GetProcAddress(Lib, PAnsiChar(PtrUInt(Ordinal)));
  end;

var
  Lib: HMODULE;
  P: Pointer;
begin
  if LoadTried then
    Exit(Loaded);
  LoadTried := True;
  Build := WindowsBuild;
  if Build < BuildDarkMode then
    Exit(False);
  Lib := LoadLibrary('uxtheme.dll');
  if Lib = 0 then
    Exit(False);
  @RefreshImmersiveColorPolicyState := Find(Lib, 104);
  @ShouldAppsUseDarkModeProc := Find(Lib, 132);
  @AllowDarkModeForWindow := Find(Lib, 133);
  P := Find(Lib, 135);
  if Build < BuildPreferredAppMode then
    @AllowDarkModeForApp := P
  else
    @SetPreferredAppMode := P;
  @FlushMenuThemes := Find(Lib, 136);
  Loaded := Assigned(RefreshImmersiveColorPolicyState) and
    Assigned(ShouldAppsUseDarkModeProc) and Assigned(AllowDarkModeForWindow) and
    (P <> nil) and Assigned(FlushMenuThemes);
  Result := Loaded;
end;

function DarkModeReport: string;

  function Found(Present: Boolean): string;
  begin
    if Present then
      Result := 'found'
    else
      Result := 'missing';
  end;

begin
  DarkModeLoad;
  Result := 'Windows build ' + IntToStr(Build) + LineEnding +
    '104 RefreshImmersiveColorPolicyState: ' + Found(Assigned(RefreshImmersiveColorPolicyState)) + LineEnding +
    '132 ShouldAppsUseDarkMode: ' + Found(Assigned(ShouldAppsUseDarkModeProc)) + LineEnding +
    '133 AllowDarkModeForWindow: ' + Found(Assigned(AllowDarkModeForWindow)) + LineEnding;
  if Build < BuildPreferredAppMode then
    Result := Result + '135 AllowDarkModeForApp: ' + Found(Assigned(AllowDarkModeForApp)) + LineEnding
  else
    Result := Result + '135 SetPreferredAppMode: ' + Found(Assigned(SetPreferredAppMode)) + LineEnding;
  Result := Result + '136 FlushMenuThemes: ' + Found(Assigned(FlushMenuThemes)) + LineEnding;
  if Loaded then
    Result := Result + 'Dark mode is available'
  else
    Result := Result + 'Dark mode is not available';
end;

function ShouldAppsUseDarkMode: Boolean;
begin
  if not DarkModeLoad then
    Exit(False);
  Result := ShouldAppsUseDarkModeProc;
end;

procedure SetAppMode(Mode: TPreferredAppMode);
begin
  if not DarkModeLoad then
    Exit;
  if Build < BuildPreferredAppMode then
    AllowDarkModeForApp(Mode in [pamAllowDark, pamForceDark])
  else
    SetPreferredAppMode(Ord(Mode));
  RefreshImmersiveColorPolicyState;
  FlushMenuThemes;
end;

function AppModeIsDark(Mode: TPreferredAppMode): Boolean;
begin
  case Mode of
    pamAllowDark: Result := ShouldAppsUseDarkMode;
    pamForceDark: Result := DarkModeLoad;
  else
    Result := False;
  end;
end;

procedure SetTitleBarDark(Wnd: HWND; Dark: Boolean);
var
  Value: BOOL;
  Attribute: DWORD;
begin
  if not DarkModeLoad then
    Exit;
  AllowDarkModeForWindow(Wnd, Dark);
  Value := Dark;
  if Build < BuildImmersiveDarkMode then
    Attribute := DWMWA_USE_IMMERSIVE_DARK_MODE_OLD
  else
    Attribute := DWMWA_USE_IMMERSIVE_DARK_MODE;
  DwmSetWindowAttribute(Wnd, Attribute, @Value, SizeOf(Value));
end;

function IsComboBoxClass(const ClassName: string): Boolean;
begin
  Result := SameText(ClassName, 'ComboBox') or
    SameText(Copy(ClassName, 1, Length('LCLComboBox')), 'LCLComboBox');
end;

{ Single line edits and combo boxes have their dark parts under DarkMode_CFD,
  headers under DarkMode_ItemsView, and the other controls, along with the
  scroll bars of a multi line edit, under DarkMode_Explorer. Passing nil
  restores the default theme. }

procedure ThemeWindow(Wnd: HWND; Dark: Boolean);
var
  Buffer: array[0..255] of WideChar;
  ClassName: string;
begin
  if not DarkModeLoad then
    Exit;
  AllowDarkModeForWindow(Wnd, Dark);
  Buffer[0] := #0;
  GetClassNameW(Wnd, @Buffer[0], Length(Buffer));
  ClassName := UpperCase(string(UnicodeString(PWideChar(@Buffer[0]))));
  if IsComboBoxClass(ClassName) then
    ClassName := 'COMBOBOX';
  if not Dark then
    SetWindowTheme(Wnd, nil, nil)
  else if (ClassName = 'EDIT') and
    (GetWindowLong(Wnd, GWL_STYLE) and ES_MULTILINE <> 0) then
    SetWindowTheme(Wnd, 'DarkMode_Explorer', nil)
  else if (ClassName = 'EDIT') or (ClassName = 'COMBOBOX') then
    SetWindowTheme(Wnd, 'DarkMode_CFD', nil)
  else if ClassName = 'SYSHEADER32' then
    SetWindowTheme(Wnd, 'DarkMode_ItemsView', nil)
  else
    SetWindowTheme(Wnd, 'DarkMode_Explorer', nil);
end;

function EnumChild(Wnd: HWND; Param: LPARAM): BOOL; stdcall;
begin
  ThemeWindow(Wnd, Param <> 0);
  SendMessage(Wnd, WM_THEMECHANGED, 0, 0);
  Result := True;
end;

procedure SetWindowDarkMode(Wnd: HWND; Dark: Boolean);
begin
  if not DarkModeLoad then
    Exit;
  SetTitleBarDark(Wnd, Dark);
  SendMessage(Wnd, WM_THEMECHANGED, 0, 0);
  EnumChildWindows(Wnd, EnumChild, LPARAM(Ord(Dark)));
  { The title bar is repainted when the window is activated again }
  SendMessage(Wnd, WM_NCACTIVATE, 0, 0);
  SendMessage(Wnd, WM_NCACTIVATE, 1, 0);
  RedrawWindow(Wnd, nil, 0, RDW_INVALIDATE or RDW_ERASE or RDW_FRAME or
    RDW_ALLCHILDREN);
end;

{$endif}
end.
