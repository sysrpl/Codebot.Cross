(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ Codebot.Themes.Windows.Hooks themes every window of the program by
  intercepting the routines Windows and the LCL draw with, rather than
  changing one control at a time. The import tables of the program and of
  comctl32.dll are patched, so that:

  - GetSysColor and GetSysColorBrush return the colors of the palette of
    Codebot.Themes.Windows.Colors
  - CreateWindowExW gives each new window its dark visual style, its dark
    title bar, and a dark menu bar
  - OpenThemeData records which class each theme handle belongs to and opens
    the dark variant of the class when Windows has one
  - DrawThemeBackground paints the parts which have no dark variant, such as
    tabs, headers, status bars, progress bars, track bars, and tool bars
  - DrawThemeText draws the text of those parts in the dark text color

  The routine variables of the UxTheme unit, which the LCL draws with, are
  pointed at the same hooks. Every loaded module outside the Windows folder
  is patched, which covers the LCL when it is in a library of its own. }

{ <include docs/codebot.themes.windows.hooks.txt> }
unit Codebot.Themes.Windows.Hooks;

{$mode delphi}

interface
{$ifdef windows}

uses
  Windows, SysUtils, CommCtrl, UxTheme, Themes, Graphics,
  Codebot.Themes.Windows.DarkMode,
  Codebot.Themes.Windows.Colors;

{ Installs the hooks, returning false if dark mode is not available. When
  forms exist already, call DarkHooksSetTheme afterwards to theme them. }
function DarkHooksInstall: Boolean;
{ Describes what was patched, for the log }
function DarkHooksReport: string;
{ Switches the theme while the program runs. When Custom is true the palette
  of Codebot.Themes.Windows.Colors is used, with the dark parts of the visual
  styles of Windows if the palette is dark. When Custom is false the hooks
  stay installed and pass everything through to Windows. Windows which exist
  already are themed again. }
procedure DarkHooksSetTheme(Custom: Boolean);
{ Sets the name of the font the LCL draws menus in, or uses the font of
  Windows when Face is empty. Menus measure their items when they are first
  shown, so menus already shown keep the size of their items. }
procedure DarkHooksSetMenuFont(const Face: string);
{ Takes the hooks out of every module and every window, so that no code of
  this unit is called again. Switch to the theme of Windows with
  DarkHooksSetTheme(False) first, so that the windows are light. }
procedure DarkHooksUninstall;
{ Forgets the slots patched in a module which is being unloaded, so that
  they are not written to after the module is gone }
procedure DarkHooksModuleUnloaded(Module: HMODULE);

{$endif}

implementation
{$ifdef windows}

const
  WM_UAHDRAWMENU = $0091;
  WM_UAHDRAWMENUITEM = $0092;
  MenuSubclassId = 1;
  ObjIdMenu = -3;
  DTBG_CLIPRECT = 1;

type
  { TThemeKind is the class a theme handle was opened for }
  TThemeKind = (tkOther, tkButton, tkTab, tkHeader, tkProgress, tkStatus,
    tkTrackBar, tkToolBar, tkEdit, tkComboBox, tkScrollBar, tkTreeView,
    tkListView, tkToolTip, tkMenu, tkListBox, tkRebar);

  TThemeEntry = record
    Theme: HTHEME;
    Kind: TThemeKind;
    Count: Integer;
  end;

  TDTBGOpts = record
    dwSize: DWORD;
    dwFlags: DWORD;
    rcClip: TRect;
  end;
  PDTBGOpts = ^TDTBGOpts;

  { The menu bar messages are not documented. These records follow the ones
    used by Notepad++ and other programs. }

  TUAHMenu = record
    Menu: HMENU;
    DC: HDC;
    Flags: DWORD;
  end;
  PUAHMenu = ^TUAHMenu;

  TUAHMenuItemMetrics = record
    case Integer of
      0: (SizeBar: array[0..1] of TSize);
      1: (SizePopup: array[0..3] of TSize);
  end;

  TUAHMenuPopupMetrics = record
    Widths: array[0..3] of DWORD;
    UpdateMaxWidths: DWORD;
  end;

  TUAHMenuItem = record
    Position: Integer;
    ItemMetrics: TUAHMenuItemMetrics;
    PopupMetrics: TUAHMenuPopupMetrics;
  end;

  TUAHDrawMenuItem = record
    DrawItem: TDrawItemStruct;
    Menu: TUAHMenu;
    Item: TUAHMenuItem;
  end;
  PUAHDrawMenuItem = ^TUAHDrawMenuItem;

  TMenuItemText = record
    cbSize: UINT;
    fMask: UINT;
    fType: UINT;
    fState: UINT;
    wID: UINT;
    hSubMenu: HMENU;
    hbmpChecked: HBITMAP;
    hbmpUnchecked: HBITMAP;
    dwItemData: ULONG_PTR;
    dwTypeData: PWideChar;
    cch: UINT;
    hbmpItem: HBITMAP;
  end;

  { The parts of a module's import tables }

  TImportDescriptor = record
    OriginalFirstThunk: DWORD;
    TimeDateStamp: DWORD;
    ForwarderChain: DWORD;
    Name: DWORD;
    FirstThunk: DWORD;
  end;
  PImportDescriptor = ^TImportDescriptor;

  TDelayDescriptor = record
    Attributes: DWORD;
    DllName: DWORD;
    ModuleHandle: DWORD;
    AddressTable: DWORD;
    NameTable: DWORD;
    BoundTable: DWORD;
    UnloadTable: DWORD;
    TimeDateStamp: DWORD;
  end;
  PDelayDescriptor = ^TDelayDescriptor;

  { THook is a routine which is replaced in the import tables }

  THook = record
    Dll: string;
    Name: string;
    New: Pointer;
    Old: PPointer;
  end;

  { The fonts of SPI_GETNONCLIENTMETRICS, in the ANSI and the wide forms }

  TMetricsFontA = record
    Height, Width, Escapement, Orientation, Weight: LongInt;
    Italic, Underline, StrikeOut, CharSet, OutPrecision, ClipPrecision,
    Quality, PitchAndFamily: Byte;
    FaceName: array[0..31] of AnsiChar;
  end;

  TMetricsFontW = record
    Height, Width, Escapement, Orientation, Weight: LongInt;
    Italic, Underline, StrikeOut, CharSet, OutPrecision, ClipPrecision,
    Quality, PitchAndFamily: Byte;
    FaceName: array[0..31] of WideChar;
  end;

  TNonClientMetricsA = record
    Size, BorderWidth, ScrollWidth, ScrollHeight, CaptionWidth, CaptionHeight: Integer;
    CaptionFont: TMetricsFontA;
    SmCaptionWidth, SmCaptionHeight: Integer;
    SmCaptionFont: TMetricsFontA;
    MenuWidth, MenuHeight: Integer;
    MenuFont, StatusFont, MessageFont: TMetricsFontA;
  end;
  PNonClientMetricsA = ^TNonClientMetricsA;

  TNonClientMetricsW = record
    Size, BorderWidth, ScrollWidth, ScrollHeight, CaptionWidth, CaptionHeight: Integer;
    CaptionFont: TMetricsFontW;
    SmCaptionWidth, SmCaptionHeight: Integer;
    SmCaptionFont: TMetricsFontW;
    MenuWidth, MenuHeight: Integer;
    MenuFont, StatusFont, MessageFont: TMetricsFontW;
  end;
  PNonClientMetricsW = ^TNonClientMetricsW;

function GetMenuItemText(Menu: HMENU; Item: UINT; ByPosition: BOOL;
  var Info: TMenuItemText): BOOL; stdcall; external 'user32.dll' name 'GetMenuItemInfoW';
function EnumProcessModules(Process: THandle; Modules: Pointer; Size: DWORD;
  var Needed: DWORD): BOOL; stdcall; external 'psapi.dll';

var
  { The palette of Codebot.Themes.Windows.Colors is used }
  Active: Boolean;
  { The palette is dark, so the dark parts of the visual styles are used }
  DarkStyle: Boolean;
  Installed: Boolean;
  Report: string;
  ThemeList: array of TThemeEntry;
  OldGetSysColor: function(Index: Integer): DWORD; stdcall;
  OldGetSysColorBrush: function(Index: Integer): HBRUSH; stdcall;
  OldDeleteObject: function(Obj: HGDIOBJ): BOOL; stdcall;
  OldDrawEdge: function(DC: HDC; R: PRect; Edge, Flags: UINT): BOOL; stdcall;
  OldSystemParametersInfoA: function(Action, Param: UINT; Data: Pointer; WinIni: UINT): BOOL; stdcall;
  OldSystemParametersInfoW: function(Action, Param: UINT; Data: Pointer; WinIni: UINT): BOOL; stdcall;
  OldLoadLibraryA: function(Name: PAnsiChar): HMODULE; stdcall;
  OldLoadLibraryW: function(Name: PWideChar): HMODULE; stdcall;
  OldLoadLibraryExA: function(Name: PAnsiChar; Reserved: THandle; Flags: DWORD): HMODULE; stdcall;
  OldLoadLibraryExW: function(Name: PWideChar; Reserved: THandle; Flags: DWORD): HMODULE; stdcall;
  MenuFace: UnicodeString;
  OldCreateWindowExW: function(ExStyle: DWORD; ClassName, WindowName: PWideChar;
    Style: DWORD; X, Y, Width, Height: Integer; Parent: HWND; Menu: HMENU;
    Instance: HINST; Param: Pointer): HWND; stdcall;
  OldOpenThemeData: function(Wnd: HWND; ClassList: PWideChar): HTHEME; stdcall;
  OldOpenThemeDataEx: function(Wnd: HWND; ClassList: PWideChar; Flags: DWORD): HTHEME; stdcall;
  OldOpenThemeDataForDpi: function(Wnd: HWND; ClassList: PWideChar; Dpi: UINT): HTHEME; stdcall;
  OldCloseThemeData: function(Theme: HTHEME): HRESULT; stdcall;
  OldDrawThemeBackground: function(Theme: HTHEME; DC: HDC; Part, State: Integer;
    R, Clip: PRect): HRESULT; stdcall;
  OldDrawThemeBackgroundEx: function(Theme: HTHEME; DC: HDC; Part, State: Integer;
    R: PRect; Options: PDTBGOpts): HRESULT; stdcall;
  OldDrawThemeText: function(Theme: HTHEME; DC: HDC; Part, State: Integer;
    Text: PWideChar; Count: Integer; Flags, Flags2: DWORD; R: PRect): HRESULT; stdcall;
  OldDrawThemeTextEx: function(Theme: HTHEME; DC: HDC; Part, State: Integer;
    Text: PWideChar; Count: Integer; Flags: DWORD; R: PRect; Options: PDTTOPTS): HRESULT; stdcall;

{ Theme handles }

function KindOfName(const Name: string): TThemeKind;
begin
  if Name = 'button' then
    Result := tkButton
  else if Name = 'tab' then
    Result := tkTab
  else if Name = 'header' then
    Result := tkHeader
  else if (Name = 'progress') or (Name = 'indeterminatebar') then
    Result := tkProgress
  else if Name = 'status' then
    Result := tkStatus
  else if Name = 'trackbar' then
    Result := tkTrackBar
  else if Name = 'toolbar' then
    Result := tkToolBar
  else if Name = 'edit' then
    Result := tkEdit
  else if Name = 'combobox' then
    Result := tkComboBox
  else if Name = 'scrollbar' then
    Result := tkScrollBar
  else if Name = 'treeview' then
    Result := tkTreeView
  else if Name = 'listview' then
    Result := tkListView
  else if Name = 'tooltip' then
    Result := tkToolTip
  else if Name = 'menu' then
    Result := tkMenu
  else if Name = 'listbox' then
    Result := tkListBox
  else if Name = 'rebar' then
    Result := tkRebar
  else
    Result := tkOther;
end;

{ The first class of a class list, without a sub application name, in
  lowercase. 'Explorer::TreeView;TreeView' becomes 'treeview'. }

function BaseClass(ClassList: PWideChar): string;
var
  I: Integer;
begin
  if ClassList = nil then
    Exit('');
  Result := LowerCase(string(UnicodeString(ClassList)));
  I := Pos(';', Result);
  if I > 0 then
    SetLength(Result, I - 1);
  I := Pos('::', Result);
  if I > 0 then
    Delete(Result, 1, I + 1);
end;

{ The dark variant of a class, or an empty string if Windows has none }

function DarkClass(Kind: TThemeKind): UnicodeString;
begin
  case Kind of
    tkButton: Result := 'DarkMode_Explorer::Button';
    tkScrollBar: Result := 'DarkMode_Explorer::ScrollBar';
    tkTreeView: Result := 'DarkMode_Explorer::TreeView';
    tkListView: Result := 'DarkMode_Explorer::ListView';
    tkToolTip: Result := 'DarkMode_Explorer::Tooltip';
    tkEdit: Result := 'DarkMode_CFD::Edit';
    tkComboBox: Result := 'DarkMode_CFD::Combobox';
    tkHeader: Result := 'DarkMode_ItemsView::Header';
  else
    Result := '';
  end;
end;

{ Windows gives every opener of a class the same handle, so each handle is
  counted and only forgotten when the last opener closes it }

procedure Remember(Theme: HTHEME; Kind: TThemeKind);
var
  I: Integer;
begin
  if (Theme = 0) or (Kind = tkOther) then
    Exit;
  for I := 0 to Length(ThemeList) - 1 do
    if ThemeList[I].Theme = Theme then
    begin
      ThemeList[I].Kind := Kind;
      Inc(ThemeList[I].Count);
      Exit;
    end;
  for I := 0 to Length(ThemeList) - 1 do
    if ThemeList[I].Theme = 0 then
    begin
      ThemeList[I].Theme := Theme;
      ThemeList[I].Kind := Kind;
      ThemeList[I].Count := 1;
      Exit;
    end;
  I := Length(ThemeList);
  SetLength(ThemeList, I + 1);
  ThemeList[I].Theme := Theme;
  ThemeList[I].Kind := Kind;
  ThemeList[I].Count := 1;
end;

procedure Forget(Theme: HTHEME);
var
  I: Integer;
begin
  for I := 0 to Length(ThemeList) - 1 do
    if ThemeList[I].Theme = Theme then
    begin
      Dec(ThemeList[I].Count);
      if ThemeList[I].Count <= 0 then
        ThemeList[I].Theme := 0;
      Exit;
    end;
end;

function KindOfTheme(Theme: HTHEME): TThemeKind;
var
  I: Integer;
begin
  if Theme <> 0 then
    for I := 0 to Length(ThemeList) - 1 do
      if ThemeList[I].Theme = Theme then
        Exit(ThemeList[I].Kind);
  Result := tkOther;
end;

{ Painting }

procedure Fill(DC: HDC; const R: TRect; Index: Integer);
begin
  FillRect(DC, R, DarkBrush(Index));
end;

procedure Frame(DC: HDC; const R: TRect; Index: Integer);
begin
  FrameRect(DC, R, DarkBrush(Index));
end;

procedure Line(DC: HDC; Left, Top, Right, Bottom: Integer; Index: Integer);
var
  R: TRect;
begin
  R.Left := Left;
  R.Top := Top;
  R.Right := Right;
  R.Bottom := Bottom;
  Fill(DC, R, Index);
end;

{ The border of an edit, combo box, or list box, which is two pixels wide }

procedure Border(DC: HDC; const R: TRect; Focused: Boolean);
var
  Inner: TRect;
begin
  if Focused then
    Frame(DC, R, COLOR_GRAYTEXT)
  else
    Frame(DC, R, COLOR_3DLIGHT);
  Inner := R;
  InflateRect(Inner, -1, -1);
  Frame(DC, Inner, COLOR_WINDOW);
end;

{ A check mark centered in R }

procedure CheckMark(DC: HDC; const R: TRect; Index: Integer);
var
  Pen: HPEN;
  OldPen: HGDIOBJ;
  X, Y, S: Integer;
begin
  X := (R.Left + R.Right) div 2;
  Y := (R.Top + R.Bottom) div 2;
  S := (R.Bottom - R.Top) div 4;
  if S < 3 then
    S := 3;
  Pen := CreatePen(PS_SOLID, 2, DarkColor(Index));
  OldPen := SelectObject(DC, Pen);
  MoveToEx(DC, X - S, Y, nil);
  LineTo(DC, X - S div 3, Y + S * 2 div 3);
  LineTo(DC, X + S, Y - S * 2 div 3);
  SelectObject(DC, OldPen);
  DeleteObject(Pen);
end;

{ An arrow pointing right centered in R, for submenus }

{ An arrow pointing down centered in R, for combo boxes }

procedure ArrowDown(DC: HDC; const R: TRect; Index: Integer);
var
  Points: array[0..2] of TPoint;
  OldPen, OldBrush: HGDIOBJ;
  X, Y, S: Integer;
begin
  X := (R.Left + R.Right) div 2;
  Y := (R.Top + R.Bottom) div 2;
  S := (R.Right - R.Left) div 5;
  if S < 3 then
    S := 3;
  Points[0].X := X - S;
  Points[0].Y := Y - S div 2;
  Points[1].X := X + S;
  Points[1].Y := Y - S div 2;
  Points[2].X := X;
  Points[2].Y := Y + S div 2;
  OldPen := SelectObject(DC, GetStockObject(NULL_PEN));
  OldBrush := SelectObject(DC, DarkBrush(Index));
  Polygon(DC, Points, 3);
  SelectObject(DC, OldBrush);
  SelectObject(DC, OldPen);
end;

{ The face of a push button with rounded corners }

procedure PushButton(DC: HDC; const R: TRect; Face, Edge: Integer);
var
  Pen: HPEN;
  OldPen, OldBrush: HGDIOBJ;
begin
  Pen := CreatePen(PS_SOLID, 1, DarkColor(Edge));
  OldPen := SelectObject(DC, Pen);
  OldBrush := SelectObject(DC, DarkBrush(Face));
  RoundRect(DC, R.Left, R.Top, R.Right, R.Bottom, 6, 6);
  SelectObject(DC, OldBrush);
  SelectObject(DC, OldPen);
  DeleteObject(Pen);
end;

{ The box of a check box or the circle of a radio button, as a square centered
  in R. The states come in groups of four, which are unchecked, checked, and
  mixed, each one normal, hot, pressed, and disabled. A checked box is filled
  with the accent color, like the check box of the dialog. }

procedure CheckGlyph(DC: HDC; const R: TRect; Radio: Boolean; State: Integer);
var
  Box, Dot: TRect;
  Group, Look, Face, Edge, S, Y: Integer;
  Pen: HPEN;
  OldPen, OldBrush: HGDIOBJ;
begin
  Group := (State - 1) div 4;
  Look := (State - 1) mod 4;
  { The implicit and excluded states of check boxes look unchecked }
  if Group > 2 then
    Group := 0;
  S := R.Right - R.Left;
  if R.Bottom - R.Top < S then
    S := R.Bottom - R.Top;
  Box.Left := (R.Left + R.Right - S) div 2;
  Box.Top := (R.Top + R.Bottom - S) div 2;
  Box.Right := Box.Left + S;
  Box.Bottom := Box.Top + S;
  if Group = 0 then
  begin
    Face := COLOR_WINDOW;
    Edge := COLOR_3DLIGHT;
    case Look of
      { Hot }
      1: Edge := COLOR_GRAYTEXT;
      { Pressed }
      2:
        begin
          Face := COLOR_BTNHIGHLIGHT;
          Edge := COLOR_GRAYTEXT;
        end;
      { Disabled }
      3: Face := COLOR_BTNFACE;
    end;
  end
  else
  begin
    if Look = 3 then
      Face := COLOR_GRAYTEXT
    else
      Face := COLOR_HOTLIGHT;
    Edge := Face;
  end;
  Pen := CreatePen(PS_SOLID, 1, DarkColor(Edge));
  OldPen := SelectObject(DC, Pen);
  OldBrush := SelectObject(DC, DarkBrush(Face));
  if Radio then
    Ellipse(DC, Box.Left, Box.Top, Box.Right, Box.Bottom)
  else
    RoundRect(DC, Box.Left, Box.Top, Box.Right, Box.Bottom, 4, 4);
  SelectObject(DC, OldBrush);
  SelectObject(DC, OldPen);
  DeleteObject(Pen);
  if Group = 0 then
    Exit;
  if Radio then
  begin
    { The dot of a radio button, where a null pen leaves out the right and
      bottom edges }
    Dot := Box;
    InflateRect(Dot, -(S div 3), -(S div 3));
    OldPen := SelectObject(DC, GetStockObject(NULL_PEN));
    OldBrush := SelectObject(DC, DarkBrush(COLOR_WINDOW));
    Ellipse(DC, Dot.Left, Dot.Top, Dot.Right + 1, Dot.Bottom + 1);
    SelectObject(DC, OldBrush);
    SelectObject(DC, OldPen);
  end
  else if Group = 1 then
    CheckMark(DC, Box, COLOR_WINDOW)
  else
  begin
    { A mixed check box shows a dash }
    Y := (Box.Top + Box.Bottom) div 2;
    Line(DC, Box.Left + S div 4, Y - 1, Box.Right - S div 4, Y + 1, COLOR_WINDOW);
  end;
end;

procedure ArrowRight(DC: HDC; const R: TRect; Index: Integer);
var
  Points: array[0..2] of TPoint;
  OldPen, OldBrush: HGDIOBJ;
  X, Y, S: Integer;
begin
  X := (R.Left + R.Right) div 2;
  Y := (R.Top + R.Bottom) div 2;
  S := (R.Bottom - R.Top) div 5;
  if S < 3 then
    S := 3;
  Points[0].X := X - S div 2;
  Points[0].Y := Y - S;
  Points[1].X := X + S div 2;
  Points[1].Y := Y;
  Points[2].X := X - S div 2;
  Points[2].Y := Y + S;
  OldPen := SelectObject(DC, GetStockObject(NULL_PEN));
  OldBrush := SelectObject(DC, DarkBrush(Index));
  Polygon(DC, Points, 3);
  SelectObject(DC, OldBrush);
  SelectObject(DC, OldPen);
end;

{ Paints a part which has no dark variant, returning false to leave it to
  the visual style }

function PaintPart(Kind: TThemeKind; DC: HDC; Part, State: Integer;
  const R: TRect; Clip: PRect): Boolean;
var
  Saved, X, Y: Integer;
begin
  Result := False;
  if not (Kind in [tkButton, tkTab, tkHeader, tkProgress, tkStatus, tkTrackBar,
    tkToolBar, tkMenu, tkEdit, tkComboBox, tkListBox, tkRebar]) then
    Exit;
  Saved := SaveDC(DC);
  try
    if Clip <> nil then
      IntersectClipRect(DC, Clip.Left, Clip.Top, Clip.Right, Clip.Bottom);
    Result := True;
    case Kind of
      { The background of a cool bar, such as the one which holds the tool
        bars of the IDE }
      tkRebar:
        case Part of
          { The root, the bands, and the background }
          0, 3, 6:
            Fill(DC, R, COLOR_BTNFACE);
          { The splitters between bands }
          7, 8:
            Fill(DC, R, COLOR_3DLIGHT);
        else
          Result := False;
        end;
      tkMenu:
        case Part of
          { The menu bar }
          7:
            Fill(DC, R, COLOR_MENUBAR);
          8:
            if State in [2, 3, 5, 6] then
              Fill(DC, R, COLOR_MENUHILIGHT)
            else
              Fill(DC, R, COLOR_MENUBAR);
          { Popup menus }
          9, 13:
            Fill(DC, R, COLOR_MENU);
          10:
            Frame(DC, R, COLOR_3DLIGHT);
          11:
            if State in [2, 4] then
              CheckMark(DC, R, COLOR_GRAYTEXT)
            else
              CheckMark(DC, R, COLOR_MENUTEXT);
          12:
            if State <> 3 then
              Fill(DC, R, COLOR_3DLIGHT);
          14:
            if State in [2, 4] then
              Fill(DC, R, COLOR_MENUHILIGHT)
            else
              Fill(DC, R, COLOR_MENU);
          15:
            begin
              Y := (R.Top + R.Bottom) div 2;
              Line(DC, R.Left, Y, R.Right, Y + 1, COLOR_3DLIGHT);
            end;
          16:
            if State = 2 then
              ArrowRight(DC, R, COLOR_GRAYTEXT)
            else
              ArrowRight(DC, R, COLOR_MENUTEXT);
        else
          Result := False;
        end;
      tkEdit:
        case Part of
          { The text area and the background }
          1, 3:
            Fill(DC, R, COLOR_WINDOW);
          { The background with its border }
          5:
            begin
              Fill(DC, R, COLOR_WINDOW);
              Border(DC, R, State in [2, 3]);
            end;
          { The borders }
          6..9:
            Border(DC, R, State in [2, 3]);
        else
          Result := False;
        end;
      tkComboBox:
        case Part of
          { The drop down button, on either side }
          1, 6, 7:
            begin
              if State in [2, 3] then
                Fill(DC, R, COLOR_BTNHIGHLIGHT)
              else
                Fill(DC, R, COLOR_WINDOW);
              if State = 4 then
                ArrowDown(DC, R, COLOR_GRAYTEXT)
              else
                ArrowDown(DC, R, COLOR_WINDOWTEXT);
            end;
          { The background }
          2, 3:
            Fill(DC, R, COLOR_WINDOW);
          { The border }
          4:
            Border(DC, R, State in [2, 3]);
          { The body of a combo box which can not be typed in }
          5:
            begin
              if State in [2, 3] then
                Fill(DC, R, COLOR_BTNHIGHLIGHT)
              else
                Fill(DC, R, COLOR_WINDOW);
              Border(DC, R, State in [2, 3]);
            end;
        else
          Result := False;
        end;
      tkListBox:
        { The borders of list boxes }
        if Part in [1..4] then
          Border(DC, R, State in [2, 3])
        else
          Result := False;
      tkButton:
        case Part of
          { Push buttons }
          1:
            case State of
              { Hot }
              2: PushButton(DC, R, COLOR_3DLIGHT, COLOR_GRAYTEXT);
              { Pressed }
              3: PushButton(DC, R, COLOR_HIGHLIGHT, COLOR_HOTLIGHT);
              { Disabled }
              4: PushButton(DC, R, COLOR_BTNFACE, COLOR_3DLIGHT);
              { The default button, which has the focus }
              5, 6: PushButton(DC, R, COLOR_BTNHIGHLIGHT, COLOR_HOTLIGHT);
            else
              PushButton(DC, R, COLOR_BTNHIGHLIGHT, COLOR_3DLIGHT);
            end;
          { Radio buttons and check boxes }
          2, 3:
            CheckGlyph(DC, R, Part = 2, State);
          { The frame of a group box }
          4:
            Frame(DC, R, COLOR_3DLIGHT);
        else
          Result := False;
        end;
      tkTab:
        case Part of
          { Tabs which are not selected }
          1..4:
            begin
              if State = 2 then
                Fill(DC, R, COLOR_3DLIGHT)
              else
                Fill(DC, R, COLOR_BTNFACE);
              Frame(DC, R, COLOR_3DLIGHT);
            end;
          { The selected tab }
          5..8:
            begin
              Fill(DC, R, COLOR_BTNHIGHLIGHT);
              Frame(DC, R, COLOR_3DLIGHT);
            end;
          { The pane around the pages }
          9:
            begin
              Fill(DC, R, COLOR_BTNFACE);
              Frame(DC, R, COLOR_3DLIGHT);
            end;
          { The body of a page }
          10, 11:
            Fill(DC, R, COLOR_BTNFACE);
        else
          Result := False;
        end;
      tkHeader:
        case Part of
          0..3:
            begin
              if State in [2, 5, 8] then
                Fill(DC, R, COLOR_3DLIGHT)
              else if State in [3, 6, 9] then
                Fill(DC, R, COLOR_BTNHIGHLIGHT)
              else
                Fill(DC, R, COLOR_BTNFACE);
              Line(DC, R.Right - 1, R.Top + 2, R.Right, R.Bottom - 2, COLOR_3DLIGHT);
              Line(DC, R.Left, R.Bottom - 1, R.Right, R.Bottom, COLOR_3DLIGHT);
            end;
        else
          Result := False;
        end;
      tkProgress:
        case Part of
          { The bar }
          1, 2, 11, 12:
            begin
              Fill(DC, R, COLOR_WINDOW);
              Frame(DC, R, COLOR_3DLIGHT);
            end;
          { The chunks and the fill }
          3..6:
            Fill(DC, R, COLOR_HOTLIGHT);
          { The animated overlays are left out }
          7..10: ;
        else
          Result := False;
        end;
      tkStatus:
        case Part of
          0, 2:
            Fill(DC, R, COLOR_BTNFACE);
          1:
            begin
              Fill(DC, R, COLOR_BTNFACE);
              Line(DC, R.Right - 1, R.Top + 2, R.Right, R.Bottom - 2, COLOR_3DLIGHT);
            end;
          { The gripper is left out }
          3: ;
        else
          Result := False;
        end;
      tkTrackBar:
        case Part of
          { The track }
          1, 2:
            Fill(DC, R, COLOR_3DLIGHT);
          { The thumb }
          3..8:
            if State = 5 then
              Fill(DC, R, COLOR_3DLIGHT)
            else if State in [2, 3] then
              Fill(DC, R, COLOR_HOTLIGHT)
            else
              Fill(DC, R, COLOR_GRAYTEXT);
          { The tick marks }
          9, 10:
            Fill(DC, R, COLOR_GRAYTEXT);
        else
          Result := False;
        end;
      tkToolBar:
        case Part of
          { Buttons }
          1..4:
            case State of
              2, 6, 7, 8: Fill(DC, R, COLOR_3DLIGHT);
              3, 5: Fill(DC, R, COLOR_BTNHIGHLIGHT);
            end;
          { Separators }
          5:
            begin
              X := (R.Left + R.Right) div 2;
              Line(DC, X, R.Top + 2, X + 1, R.Bottom - 2, COLOR_3DLIGHT);
            end;
          6:
            begin
              Y := (R.Top + R.Bottom) div 2;
              Line(DC, R.Left + 2, Y, R.Right - 2, Y + 1, COLOR_3DLIGHT);
            end;
        else
          Result := False;
        end;
    end;
  finally
    RestoreDC(DC, Saved);
  end;
end;

{ The text color of a part, returning false to leave it to the visual style }

function TextColor(Kind: TThemeKind; Part, State: Integer; out Color: COLORREF): Boolean;
var
  Disabled: Boolean;
begin
  Result := True;
  Disabled := False;
  case Kind of
    tkButton:
      case Part of
        { Push buttons draw light text in their dark variant }
        { Push buttons }
        1: Disabled := State = 4;
        { Radio buttons and check boxes, whose states come in groups of four }
        2, 3: Disabled := (State - 1) mod 4 = 3;
        { Group boxes }
        4: Disabled := State = 2;
      else
        Result := False;
      end;
    tkTab, tkToolBar:
      Disabled := State = 4;
    tkHeader, tkStatus: ;
    tkComboBox:
      Disabled := State = 4;
    tkTreeView:
      { Tree items, the only part with text }
      if Part = 1 then
        Disabled := State = 4
      else
        Result := False;
    tkMenu:
      case Part of
        { Menu bar items }
        8: Disabled := State in [4, 5, 6];
        { Popup menu items }
        14: Disabled := State in [3, 4];
      end;
  else
    Result := False;
  end;
  if Disabled then
    Color := DarkColor(COLOR_GRAYTEXT)
  else if Kind = tkMenu then
    Color := DarkColor(COLOR_MENUTEXT)
  else if Kind in [tkComboBox, tkTreeView] then
    Color := DarkColor(COLOR_WINDOWTEXT)
  else
    Color := DarkColor(COLOR_BTNTEXT);
end;

{ The hooks }

function HookGetSysColor(Index: Integer): DWORD; stdcall;
begin
  if Active and DarkColorMapped(Index) then
    Result := DarkColor(Index)
  else
    Result := OldGetSysColor(Index);
end;

function HookGetSysColorBrush(Index: Integer): HBRUSH; stdcall;
begin
  if Active and DarkColorMapped(Index) then
    Result := DarkBrush(Index)
  else
    Result := OldGetSysColorBrush(Index);
end;

{ Brushes of system colors must not be deleted, and code which deletes them
  anyway must not delete the dark ones. This holds when it is light too, as
  the LCL deletes the brushes it cached while it was dark when it switches. }

function HookDeleteObject(Obj: HGDIOBJ): BOOL; stdcall;
begin
  if IsDarkBrush(Obj) then
    Result := True
  else
    Result := OldDeleteObject(Obj);
end;

{ The LCL draws menus in the menu font of SPI_GETNONCLIENTMETRICS, so the
  name of that font is replaced with the one of the theme. The size is the
  one of Windows. }

function HookSystemParametersInfoA(Action, Param: UINT; Data: Pointer;
  WinIni: UINT): BOOL; stdcall;
var
  Face: AnsiString;
  I: Integer;
begin
  Result := OldSystemParametersInfoA(Action, Param, Data, WinIni);
  if Result and (Action = SPI_GETNONCLIENTMETRICS) and (Data <> nil) and (MenuFace <> '') then
  begin
    Face := AnsiString(MenuFace);
    with PNonClientMetricsA(Data).MenuFont do
    begin
      FillChar(FaceName, SizeOf(FaceName), 0);
      for I := 1 to Length(Face) do
        if I <= High(FaceName) then
          FaceName[I - 1] := Face[I];
    end;
  end;
end;

function HookSystemParametersInfoW(Action, Param: UINT; Data: Pointer;
  WinIni: UINT): BOOL; stdcall;
var
  I: Integer;
begin
  Result := OldSystemParametersInfoW(Action, Param, Data, WinIni);
  if Result and (Action = SPI_GETNONCLIENTMETRICS) and (Data <> nil) and (MenuFace <> '') then
    with PNonClientMetricsW(Data).MenuFont do
    begin
      FillChar(FaceName, SizeOf(FaceName), 0);
      for I := 1 to Length(MenuFace) do
        if I <= High(FaceName) then
          FaceName[I - 1] := MenuFace[I];
    end;
end;

{ Edges, such as the separators of classic menus and the borders of panels,
  are drawn by Windows in its own light colors. The outer edge is drawn in
  the light color of the dark theme and the inner edge in its shadow. }

function HookDrawEdge(DC: HDC; R: PRect; Edge, Flags: UINT): BOOL; stdcall;
const
  Outer = BDR_RAISEDOUTER or BDR_SUNKENOUTER;
  Inner = BDR_RAISEDINNER or BDR_SUNKENINNER;
var
  Rc: TRect;
  Ring: Integer;
  Color: Integer;
begin
  if (not Active) or (R = nil) or (Flags and BF_DIAGONAL <> 0) then
    Exit(OldDrawEdge(DC, R, Edge, Flags));
  Rc := R^;
  for Ring := 0 to 1 do
  begin
    if (Ring = 0) and (Edge and Outer = 0) then
      Continue;
    if (Ring = 1) and (Edge and Inner = 0) then
      Continue;
    if Ring = 0 then
      Color := COLOR_3DLIGHT
    else
      Color := COLOR_BTNSHADOW;
    if Flags and BF_TOP <> 0 then
    begin
      Line(DC, Rc.Left, Rc.Top, Rc.Right, Rc.Top + 1, Color);
      Inc(Rc.Top);
    end;
    if Flags and BF_BOTTOM <> 0 then
    begin
      Line(DC, Rc.Left, Rc.Bottom - 1, Rc.Right, Rc.Bottom, Color);
      Dec(Rc.Bottom);
    end;
    if Flags and BF_LEFT <> 0 then
    begin
      Line(DC, Rc.Left, Rc.Top, Rc.Left + 1, Rc.Bottom, Color);
      Inc(Rc.Left);
    end;
    if Flags and BF_RIGHT <> 0 then
    begin
      Line(DC, Rc.Right - 1, Rc.Top, Rc.Right, Rc.Bottom, Color);
      Dec(Rc.Right);
    end;
  end;
  if Flags and BF_MIDDLE <> 0 then
    Fill(DC, Rc, COLOR_BTNFACE);
  if Flags and BF_ADJUST <> 0 then
    R^ := Rc;
  Result := True;
end;

function OpenTheme(Wnd: HWND; ClassList: PWideChar; Flags: DWORD; Dpi: UINT;
  Which: Integer): HTHEME;

  function Open(Name: PWideChar): HTHEME;
  begin
    case Which of
      1: Result := OldOpenThemeDataEx(Wnd, Name, Flags);
      2: Result := OldOpenThemeDataForDpi(Wnd, Name, Dpi);
    else
      Result := OldOpenThemeData(Wnd, Name);
    end;
  end;

var
  Kind: TThemeKind;
  Name: UnicodeString;
begin
  { Handles opened while it is light are remembered too, so that the count of
    a handle shared with dark windows stays right }
  Kind := KindOfName(BaseClass(ClassList));
  Result := 0;
  if DarkStyle then
  begin
    Name := DarkClass(Kind);
    if Name <> '' then
      Result := Open(PWideChar(Name));
  end;
  if Result = 0 then
    Result := Open(ClassList);
  Remember(Result, Kind);
end;

function HookOpenThemeData(Wnd: HWND; ClassList: PWideChar): HTHEME; stdcall;
begin
  Result := OpenTheme(Wnd, ClassList, 0, 0, 0);
end;

function HookOpenThemeDataEx(Wnd: HWND; ClassList: PWideChar; Flags: DWORD): HTHEME; stdcall;
begin
  Result := OpenTheme(Wnd, ClassList, Flags, 0, 1);
end;

function HookOpenThemeDataForDpi(Wnd: HWND; ClassList: PWideChar; Dpi: UINT): HTHEME; stdcall;
begin
  Result := OpenTheme(Wnd, ClassList, 0, Dpi, 2);
end;

function HookCloseThemeData(Theme: HTHEME): HRESULT; stdcall;
begin
  Forget(Theme);
  Result := OldCloseThemeData(Theme);
end;

function HookDrawThemeBackground(Theme: HTHEME; DC: HDC; Part, State: Integer;
  R, Clip: PRect): HRESULT; stdcall;
begin
  if Active and (R <> nil) and PaintPart(KindOfTheme(Theme), DC, Part, State, R^, Clip) then
    Result := S_OK
  else
    Result := OldDrawThemeBackground(Theme, DC, Part, State, R, Clip);
end;

function HookDrawThemeBackgroundEx(Theme: HTHEME; DC: HDC; Part, State: Integer;
  R: PRect; Options: PDTBGOpts): HRESULT; stdcall;
var
  Clip: PRect;
begin
  Clip := nil;
  if (Options <> nil) and (Options.dwFlags and DTBG_CLIPRECT <> 0) then
    Clip := @Options.rcClip;
  if Active and (R <> nil) and PaintPart(KindOfTheme(Theme), DC, Part, State, R^, Clip) then
    Result := S_OK
  else
    Result := OldDrawThemeBackgroundEx(Theme, DC, Part, State, R, Options);
end;

function HookDrawThemeTextEx(Theme: HTHEME; DC: HDC; Part, State: Integer;
  Text: PWideChar; Count: Integer; Flags: DWORD; R: PRect; Options: PDTTOPTS): HRESULT; stdcall;
var
  Color: COLORREF;
  Opts: TDTTOpts;
begin
  if Active and TextColor(KindOfTheme(Theme), Part, State, Color) then
  begin
    if Options <> nil then
      Opts := Options^
    else
    begin
      FillChar(Opts, SizeOf(Opts), 0);
      Opts.dwSize := SizeOf(Opts);
    end;
    Opts.dwFlags := Opts.dwFlags or DTT_TEXTCOLOR;
    Opts.crText := Color;
    Result := OldDrawThemeTextEx(Theme, DC, Part, State, Text, Count, Flags, R, @Opts);
  end
  else
    Result := OldDrawThemeTextEx(Theme, DC, Part, State, Text, Count, Flags, R, Options);
end;

function HookDrawThemeText(Theme: HTHEME; DC: HDC; Part, State: Integer;
  Text: PWideChar; Count: Integer; Flags, Flags2: DWORD; R: PRect): HRESULT; stdcall;
var
  Color: COLORREF;
begin
  if Active and Assigned(OldDrawThemeTextEx) and TextColor(KindOfTheme(Theme), Part, State, Color) then
    Result := HookDrawThemeTextEx(Theme, DC, Part, State, Text, Count, Flags, R, nil)
  else
    Result := OldDrawThemeText(Theme, DC, Part, State, Text, Count, Flags, Flags2, R);
end;

{ The menu bar }

procedure PaintMenuBar(Wnd: HWND; Menu: PUAHMenu);
var
  Info: MENUBARINFO;
  Window, R: TRect;
begin
  FillChar(Info, SizeOf(Info), 0);
  Info.cbSize := SizeOf(Info);
  if not GetMenuBarInfo(Wnd, ObjIdMenu, 0, @Info) then
    Exit;
  GetWindowRect(Wnd, Window);
  R := Info.rcBar;
  OffsetRect(R, -Window.Left, -Window.Top);
  Fill(Menu.DC, R, COLOR_MENUBAR);
end;

procedure PaintMenuItem(Wnd: HWND; Item: PUAHDrawMenuItem);
var
  Text: array[0..255] of WideChar;
  Info: TMenuItemText;
  State, Flags: UINT;
  R: TRect;
begin
  FillChar(Info, SizeOf(Info), 0);
  Info.cbSize := SizeOf(Info);
  Info.fMask := MIIM_STRING;
  Info.dwTypeData := @Text[0];
  Info.cch := Length(Text) - 1;
  Text[0] := #0;
  GetMenuItemText(Item.Menu.Menu, Item.Item.Position, True, Info);
  State := Item.DrawItem.itemState;
  R := Item.DrawItem.rcItem;
  if State and (ODS_HOTLIGHT or ODS_SELECTED) <> 0 then
    Fill(Item.DrawItem.hDC, R, COLOR_MENUHILIGHT)
  else
    Fill(Item.DrawItem.hDC, R, COLOR_MENUBAR);
  if State and (ODS_GRAYED or ODS_DISABLED or ODS_INACTIVE) <> 0 then
    SetTextColor(Item.DrawItem.hDC, DarkColor(COLOR_GRAYTEXT))
  else
    SetTextColor(Item.DrawItem.hDC, DarkColor(COLOR_MENUTEXT));
  SetBkMode(Item.DrawItem.hDC, TRANSPARENT);
  Flags := DT_CENTER or DT_SINGLELINE or DT_VCENTER;
  if State and ODS_NOACCEL <> 0 then
    Flags := Flags or DT_HIDEPREFIX;
  DrawTextW(Item.DrawItem.hDC, @Text[0], -1, R, Flags);
end;

{ Windows draws a light line between the menu bar and the client area }

procedure DrawMenuLine(Wnd: HWND);
var
  Client, Window, R: TRect;
  DC: HDC;
begin
  if GetMenu(Wnd) = 0 then
    Exit;
  GetClientRect(Wnd, Client);
  MapWindowPoints(Wnd, 0, Client, 2);
  GetWindowRect(Wnd, Window);
  OffsetRect(Client, -Window.Left, -Window.Top);
  R := Client;
  R.Bottom := R.Top;
  Dec(R.Top);
  DC := GetWindowDC(Wnd);
  Fill(DC, R, COLOR_MENUBAR);
  ReleaseDC(Wnd, DC);
end;

function MenuSubclassProc(Wnd: HWND; Msg: UINT; W: WPARAM; L: LPARAM;
  Id: UINT_PTR; RefData: DWORD_PTR): LRESULT; stdcall;
begin
  if Active then
    case Msg of
      WM_UAHDRAWMENU:
        begin
          PaintMenuBar(Wnd, PUAHMenu(L));
          Exit(1);
        end;
      WM_UAHDRAWMENUITEM:
        begin
          PaintMenuItem(Wnd, PUAHDrawMenuItem(L));
          Exit(1);
        end;
      WM_NCPAINT, WM_NCACTIVATE:
        begin
          Result := DefSubclassProc(Wnd, Msg, W, L);
          DrawMenuLine(Wnd);
          Exit;
        end;
    end;
  Result := DefSubclassProc(Wnd, Msg, W, L);
end;

{ New windows

  The menu bar subclass must be the outermost window procedure. The LCL
  subclasses its forms right after CreateWindowExW returns and calls the
  procedure it replaced as the default one, so a subclass installed earlier
  would call the LCL, which calls the subclass again. Windows which can have
  a menu bar are kept pending until a message arrives after their procedure
  is no longer the one of their class. }

var
  Pending: array of HWND;
  CallHook: HHOOK;

procedure AddPending(Wnd: HWND);
var
  I: Integer;
begin
  for I := 0 to Length(Pending) - 1 do
    if Pending[I] = 0 then
    begin
      Pending[I] := Wnd;
      Exit;
    end;
  I := Length(Pending);
  SetLength(Pending, I + 1);
  Pending[I] := Wnd;
end;

{ Popup menus

  The window of a popup menu, of class #32768, is created and drawn by
  Windows. Its border and its separators are drawn in light colors whatever
  the LCL draws for the items, so they are painted again after Windows paints
  the window, and after it prints the window for the menu animation. }

const
  MN_GETHMENU = $01E1;
  PopupSubclassId = 2;

function MenuItemRect(Wnd: HWND; Menu: HMENU; Item: UINT; var R: TRect): BOOL;
  stdcall; external 'user32.dll' name 'GetMenuItemRect';

{ Paints the separators of a popup menu, with the client area of the menu
  window at DX, DY in DC }

procedure PaintPopupSeparators(Wnd: HWND; DC: HDC; DX, DY: Integer);
var
  Menu: HMENU;
  Info: TMenuItemText;
  R: TRect;
  I, Y: Integer;
begin
  Menu := HMENU(SendMessage(Wnd, MN_GETHMENU, 0, 0));
  if Menu = 0 then
    Exit;
  for I := 0 to GetMenuItemCount(Menu) - 1 do
  begin
    FillChar(Info, SizeOf(Info), 0);
    Info.cbSize := SizeOf(Info);
    Info.fMask := MIIM_FTYPE;
    if not GetMenuItemText(Menu, I, True, Info) then
      Continue;
    if Info.fType and MFT_SEPARATOR = 0 then
      Continue;
    { For a popup menu the rectangle is found without the owner window }
    if not MenuItemRect(0, Menu, I, R) then
      Continue;
    MapWindowPoints(0, Wnd, R, 2);
    OffsetRect(R, DX, DY);
    Fill(DC, R, COLOR_MENU);
    Y := (R.Top + R.Bottom) div 2;
    Line(DC, R.Left + 4, Y, R.Right - 4, Y + 1, COLOR_3DLIGHT);
  end;
end;

{ The client area of a window in the coordinates of the window }

function ClientInWindow(Wnd: HWND): TRect;
var
  Window: TRect;
begin
  GetWindowRect(Wnd, Window);
  GetClientRect(Wnd, Result);
  MapWindowPoints(Wnd, 0, Result, 2);
  OffsetRect(Result, -Window.Left, -Window.Top);
end;

{ Paints the border of a popup menu in DC, which is in the coordinates of
  the window }

procedure PaintPopupBorder(Wnd: HWND; DC: HDC);
var
  Window, Client: TRect;
  Saved: Integer;
begin
  GetWindowRect(Wnd, Window);
  OffsetRect(Window, -Window.Left, -Window.Top);
  Client := ClientInWindow(Wnd);
  Saved := SaveDC(DC);
  ExcludeClipRect(DC, Client.Left, Client.Top, Client.Right, Client.Bottom);
  Fill(DC, Window, COLOR_MENU);
  Frame(DC, Window, COLOR_3DLIGHT);
  RestoreDC(DC, Saved);
end;

{ Paints the client area of a popup menu in DC: the background, the items,
  which Windows prints and the LCL draws, and then the separators }

procedure PaintPopupClient(Wnd: HWND; DC: HDC);
var
  R: TRect;
begin
  GetClientRect(Wnd, R);
  Fill(DC, R, COLOR_MENU);
  DefSubclassProc(Wnd, WM_PRINTCLIENT, WPARAM(DC), PRF_CLIENT);
  PaintPopupSeparators(Wnd, DC, 0, 0);
end;

{ The client area is painted into a bitmap and then copied to the window, so
  that the light background and separators Windows draws are never seen }

procedure PaintPopup(Wnd: HWND);
var
  PS: TPaintStruct;
  R: TRect;
  MemDC: HDC;
  Bitmap, OldBitmap: HBITMAP;
begin
  BeginPaint(Wnd, PS);
  GetClientRect(Wnd, R);
  MemDC := CreateCompatibleDC(PS.hdc);
  Bitmap := CreateCompatibleBitmap(PS.hdc, R.Right, R.Bottom);
  OldBitmap := SelectObject(MemDC, Bitmap);
  PaintPopupClient(Wnd, MemDC);
  BitBlt(PS.hdc, 0, 0, R.Right, R.Bottom, MemDC, 0, 0, SRCCOPY);
  SelectObject(MemDC, OldBitmap);
  DeleteObject(Bitmap);
  DeleteDC(MemDC);
  EndPaint(Wnd, PS);
end;

function PopupSubclassProc(Wnd: HWND; Msg: UINT; W: WPARAM; L: LPARAM;
  Id: UINT_PTR; RefData: DWORD_PTR): LRESULT; stdcall;
var
  DC: HDC;
  Client: TRect;
  Origin: TPoint;
begin
  case Msg of
    WM_NCDESTROY:
      begin
        RemoveWindowSubclass(Wnd, PopupSubclassProc, PopupSubclassId);
        Exit(DefSubclassProc(Wnd, Msg, W, L));
      end;
    { The border is drawn only in the colors of the theme }
    WM_NCPAINT:
      if Active then
      begin
        DC := GetWindowDC(Wnd);
        PaintPopupBorder(Wnd, DC);
        ReleaseDC(Wnd, DC);
        Exit(0);
      end;
    { The background is painted with the items }
    WM_ERASEBKGND:
      if Active then
        Exit(1);
    WM_PAINT:
      if Active then
      begin
        PaintPopup(Wnd);
        Exit(0);
      end;
    { The menu animation prints the window into a bitmap before it is shown }
    WM_PRINT:
      if Active then
      begin
        DC := HDC(W);
        if L and PRF_NONCLIENT <> 0 then
        begin
          PaintPopupBorder(Wnd, DC);
          Client := ClientInWindow(Wnd);
        end
        else
          Client := Default(TRect);
        if L and PRF_CLIENT <> 0 then
        begin
          GetViewportOrgEx(DC, @Origin);
          SetViewportOrgEx(DC, Origin.X + Client.Left, Origin.Y + Client.Top, nil);
          PaintPopupClient(Wnd, DC);
          SetViewportOrgEx(DC, Origin.X, Origin.Y, nil);
        end;
        Exit(0);
      end;
    WM_PRINTCLIENT:
      if Active then
      begin
        PaintPopupClient(Wnd, HDC(W));
        Exit(0);
      end;
  end;
  Result := DefSubclassProc(Wnd, Msg, W, L);
end;

{ Combo boxes

  The drop down list of a combo box is drawn by Windows with the colors the
  combo box returns for WM_CTLCOLORLISTBOX, which the LCL leaves to Windows.
  The LCL registers its combo boxes as a class of its own whose procedure is
  the LCL's, and keeps the procedure of Windows as the default one, so this
  subclass sits above the LCL without calling itself again. }

const
  ComboSubclassId = 3;

function ComboSubclassProc(Wnd: HWND; Msg: UINT; W: WPARAM; L: LPARAM;
  Id: UINT_PTR; RefData: DWORD_PTR): LRESULT; stdcall;
begin
  case Msg of
    WM_NCDESTROY:
      RemoveWindowSubclass(Wnd, ComboSubclassProc, ComboSubclassId);
    { The edit of a combo box which can be typed in asks for its colors too }
    WM_CTLCOLORLISTBOX, WM_CTLCOLOREDIT:
      if Active then
      begin
        SetTextColor(HDC(W), DarkColor(COLOR_WINDOWTEXT));
        SetBkColor(HDC(W), DarkColor(COLOR_WINDOW));
        Exit(LRESULT(DarkBrush(COLOR_WINDOW)));
      end;
  end;
  Result := DefSubclassProc(Wnd, Msg, W, L);
end;

{ Client edges

  Edits, memos, list boxes, and other windows with WS_EX_CLIENTEDGE have a
  sunken border two pixels wide which Windows draws itself in light colors.
  It is painted again after the window has handled WM_NCPAINT, from a hook
  which runs when a window procedure returns, so that no window is
  subclassed. }

var
  RetHook: HHOOK;

procedure PaintClientEdge(Wnd: HWND);
var
  R, Inner: TRect;
  DC: HDC;
  Color: Integer;
begin
  GetWindowRect(Wnd, R);
  OffsetRect(R, -R.Left, -R.Top);
  DC := GetWindowDC(Wnd);
  if GetFocus = Wnd then
    Color := COLOR_GRAYTEXT
  else
    Color := COLOR_3DLIGHT;
  Frame(DC, R, Color);
  Inner := R;
  InflateRect(Inner, -1, -1);
  Frame(DC, Inner, COLOR_WINDOW);
  ReleaseDC(Wnd, DC);
end;

function CallWndRetHook(Code: LongInt; W: WPARAM; L: LPARAM): LRESULT; stdcall;
var
  Msg: PCWPRETSTRUCT;
begin
  if (Code = HC_ACTION) and Active then
  begin
    Msg := PCWPRETSTRUCT(L);
    case Msg.message of
      WM_NCPAINT, WM_SETFOCUS, WM_KILLFOCUS:
        if GetWindowLong(Msg.hwnd, GWL_EXSTYLE) and WS_EX_CLIENTEDGE <> 0 then
          PaintClientEdge(Msg.hwnd);
    end;
  end;
  Result := CallNextHookEx(RetHook, Code, W, L);
end;

function IsPopupMenuWindow(Wnd: HWND): Boolean;
var
  Buffer: array[0..15] of WideChar;
begin
  Buffer[0] := #0;
  GetClassNameW(Wnd, @Buffer[0], Length(Buffer));
  Result := lstrcmpW(@Buffer[0], '#32768') = 0;
end;

function CallWndProcHook(Code: LongInt; W: WPARAM; L: LPARAM): LRESULT; stdcall;
var
  Msg: PCWPSTRUCT;
  I: Integer;
begin
  if Code = HC_ACTION then
  begin
    Msg := PCWPSTRUCT(L);
    if (Msg.message = WM_CREATE) and IsPopupMenuWindow(Msg.hwnd) then
      SetWindowSubclass(Msg.hwnd, PopupSubclassProc, PopupSubclassId, 0);
  end;
  if (Code = HC_ACTION) and (Length(Pending) > 0) then
  begin
    Msg := PCWPSTRUCT(L);
    for I := 0 to Length(Pending) - 1 do
      if (Pending[I] <> 0) and (Pending[I] = Msg.hwnd) then
      begin
        if Msg.message = WM_NCDESTROY then
          Pending[I] := 0
        else if GetWindowLongPtrW(Msg.hwnd, GWLP_WNDPROC) <>
          GetClassLongPtrW(Msg.hwnd, GCLP_WNDPROC) then
        begin
          Pending[I] := 0;
          SetWindowSubclass(Msg.hwnd, MenuSubclassProc, MenuSubclassId, 0);
        end;
        Break;
      end;
  end;
  Result := CallNextHookEx(CallHook, Code, W, L);
end;

procedure DarkenWindow(Wnd: HWND);
var
  Buffer: array[0..255] of WideChar;
  Info: TComboBoxInfo;
begin
  ThemeWindow(Wnd, DarkStyle);
  if GetWindowLong(Wnd, GWL_STYLE) and WS_CHILD = 0 then
  begin
    SetTitleBarDark(Wnd, DarkStyle);
    if GetWindowLong(Wnd, GWL_STYLE) and WS_CAPTION = WS_CAPTION then
      AddPending(Wnd);
  end;
  { The drop down list of a combo box is created by Windows itself, and asks
    the combo box for its colors }
  Buffer[0] := #0;
  GetClassNameW(Wnd, @Buffer[0], Length(Buffer));
  if IsComboBoxClass(string(UnicodeString(PWideChar(@Buffer[0])))) then
  begin
    SetWindowSubclass(Wnd, ComboSubclassProc, ComboSubclassId, 0);
    FillChar(Info, SizeOf(Info), 0);
    Info.cbSize := SizeOf(Info);
    if GetComboBoxInfo(Wnd, @Info) and (Info.hwndList <> 0) then
      ThemeWindow(Info.hwndList, DarkStyle);
  end;
end;

function HookCreateWindowExW(ExStyle: DWORD; ClassName, WindowName: PWideChar;
  Style: DWORD; X, Y, Width, Height: Integer; Parent: HWND; Menu: HMENU;
  Instance: HINST; Param: Pointer): HWND; stdcall;
begin
  Result := OldCreateWindowExW(ExStyle, ClassName, WindowName, Style, X, Y,
    Width, Height, Parent, Menu, Instance, Param);
  if Result = 0 then
    Exit;
  if Active then
    DarkenWindow(Result)
  { A window created while it is light still gets the menu bar subclass, so
    that its menu bar is dark when the program is switched to dark }
  else if GetWindowLong(Result, GWL_STYLE) and (WS_CHILD or WS_CAPTION) = WS_CAPTION then
    AddPending(Result);
end;

{ Import tables

  Every slot of an import table which is patched is recorded with its module
  and its old routine, so that it can be put back. Libraries can be loaded
  and unloaded on other threads, so the records are guarded by a lock. }

type
  TPatchedSlot = record
    Module: HMODULE;
    Slot: PPointer;
    Old: Pointer;
    New: Pointer;
  end;

function GetModuleHandleExW(Flags: DWORD; Name: Pointer; var Module: HMODULE): BOOL;
  stdcall; external 'kernel32.dll';

const
  GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT = 2;
  GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS = 4;
  LOAD_LIBRARY_AS_DATAFILE = 2;
  LOAD_LIBRARY_AS_DATAFILE_EXCLUSIVE = $40;
  LOAD_LIBRARY_AS_IMAGE_RESOURCE = $20;

var
  Lock: TRTLCriticalSection;
  HookList: array of THook;
  Slots: array of TPatchedSlot;
  PatchedModules: array of HMODULE;
  WindowsDir: string;
  { The routine variables of the UxTheme unit before they were hooked }
  SavedOpenThemeData, SavedCloseThemeData, SavedDrawThemeBackground,
  SavedDrawThemeText, SavedDrawThemeTextEx: Pointer;

{ The module an address belongs to, or 0 if it belongs to none }

function ModuleOfAddress(Address: Pointer): HMODULE;
begin
  Result := 0;
  if not GetModuleHandleExW(GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS or
    GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT, Address, Result) then
    Result := 0;
end;

procedure WritePointer(Slot: PPointer; Value: Pointer);
var
  Old: DWORD;
begin
  if VirtualProtect(Slot, SizeOf(Pointer), PAGE_READWRITE, Old) then
  begin
    Slot^ := Value;
    VirtualProtect(Slot, SizeOf(Pointer), Old, Old);
  end;
end;

{ Replaces the routines of Hooks found in one table of imports from Dll. Names
  is the table of imported names, or nil if the module has none, in which
  case the addresses are compared. }

function PatchTable(Module: HMODULE; Names, Addresses: PPtrUInt; const Dll: string;
  const Hooks: array of THook): Integer;
var
  Base: PByte;
  OrdinalFlag: PtrUInt;
  Name: PAnsiChar;
  I, J: Integer;
begin
  Base := PByte(Module);
  Result := 0;
  OrdinalFlag := PtrUInt(1) shl (SizeOf(PtrUInt) * 8 - 1);
  while Addresses^ <> 0 do
  begin
    Name := nil;
    if (Names <> nil) and (Names^ and OrdinalFlag = 0) then
      Name := PAnsiChar(Base + Names^ + 2);
    for I := Low(Hooks) to High(Hooks) do
      if SameText(Hooks[I].Dll, Dll) and (Hooks[I].Old^ <> nil) then
        if ((Name <> nil) and (StrIComp(Name, PAnsiChar(AnsiString(Hooks[I].Name))) = 0)) or
          ((Names = nil) and (Pointer(Addresses^) = Hooks[I].Old^)) then
        begin
          if Pointer(Addresses^) <> Hooks[I].New then
          begin
            J := Length(Slots);
            SetLength(Slots, J + 1);
            Slots[J].Module := Module;
            Slots[J].Slot := PPointer(Addresses);
            Slots[J].Old := Pointer(Addresses^);
            Slots[J].New := Hooks[I].New;
            WritePointer(PPointer(Addresses), Hooks[I].New);
            Inc(Result);
          end;
          Break;
        end;
    Inc(Addresses);
    if Names <> nil then
      Inc(Names);
  end;
end;

function DataDirectory(Base: PByte; Index: Integer): PImageDataDirectory;
var
  Dos: PIMAGE_DOS_HEADER;
  Nt: PImageNtHeaders;
begin
  Result := nil;
  Dos := PIMAGE_DOS_HEADER(Base);
  if Dos.e_magic <> IMAGE_DOS_SIGNATURE then
    Exit;
  Nt := PImageNtHeaders(Base + Dos._lfanew);
  if Nt.Signature <> IMAGE_NT_SIGNATURE then
    Exit;
  Result := @Nt.OptionalHeader.DataDirectory[Index];
  if (Result.VirtualAddress = 0) or (Result.Size = 0) then
    Result := nil;
end;

function PatchModule(Module: HMODULE; const Hooks: array of THook): Integer;
var
  Base: PByte;
  Dir: PImageDataDirectory;
  Import: PImportDescriptor;
  Delay: PDelayDescriptor;
  Names: PPtrUInt;
begin
  Result := 0;
  Base := PByte(Module);
  Dir := DataDirectory(Base, IMAGE_DIRECTORY_ENTRY_IMPORT);
  if Dir <> nil then
  begin
    Import := PImportDescriptor(Base + Dir.VirtualAddress);
    while Import.Name <> 0 do
    begin
      if Import.OriginalFirstThunk <> 0 then
        Names := PPtrUInt(Base + Import.OriginalFirstThunk)
      else
        Names := nil;
      Inc(Result, PatchTable(Module, Names, PPtrUInt(Base + Import.FirstThunk),
        string(PAnsiChar(Base + Import.Name)), Hooks));
      Inc(Import);
    end;
  end;
  Dir := DataDirectory(Base, IMAGE_DIRECTORY_ENTRY_DELAY_IMPORT);
  if Dir <> nil then
  begin
    Delay := PDelayDescriptor(Base + Dir.VirtualAddress);
    { Only descriptors which hold relative addresses are understood }
    while (Delay.DllName <> 0) and (Delay.Attributes and 1 <> 0) do
    begin
      if Delay.NameTable <> 0 then
        Names := PPtrUInt(Base + Delay.NameTable)
      else
        Names := nil;
      Inc(Result, PatchTable(Module, Names, PPtrUInt(Base + Delay.AddressTable),
        string(PAnsiChar(Base + Delay.DllName)), Hooks));
      Inc(Delay);
    end;
  end;
end;

{ Modules outside the Windows folder belong to the program. Of the modules of
  Windows only comctl32.dll is patched, as it draws the common controls. }

function ShouldPatch(Module: HMODULE): Boolean;
var
  Buffer: array[0..MAX_PATH] of WideChar;
  FileName: string;
begin
  Buffer[0] := #0;
  GetModuleFileNameW(Module, @Buffer[0], MAX_PATH);
  FileName := LowerCase(string(UnicodeString(PWideChar(@Buffer[0]))));
  Result := (Pos(WindowsDir, FileName) <> 1) or
    (ExtractFileName(FileName) = 'comctl32.dll');
end;

function Hook(const Dll, Name: string; New: Pointer; Old: PPointer): THook;
begin
  Result.Dll := Dll;
  Result.Name := Name;
  Result.New := New;
  Result.Old := Old;
  Old^ := GetProcAddress(GetModuleHandle(PChar(Dll)), PChar(Name));
end;

function IsPatched(Module: HMODULE): Boolean;
var
  I: Integer;
begin
  for I := 0 to Length(PatchedModules) - 1 do
    if PatchedModules[I] = Module then
      Exit(True);
  Result := False;
end;

function ModuleName(Module: HMODULE): string;
var
  Buffer: array[0..MAX_PATH] of WideChar;
begin
  Buffer[0] := #0;
  GetModuleFileNameW(Module, @Buffer[0], MAX_PATH);
  Result := ExtractFileName(string(UnicodeString(PWideChar(@Buffer[0]))));
end;

{ Patches every loaded module which should be patched and has not been, and
  adds a line about each one to the report }

procedure PatchNewModules;
var
  Modules: array[0..1023] of HMODULE;
  Needed: DWORD;
  I, J, Count, Patched: Integer;
begin
  EnterCriticalSection(Lock);
  try
    Needed := 0;
    if not EnumProcessModules(GetCurrentProcess, @Modules[0], SizeOf(Modules), Needed) then
    begin
      Report := Report + 'The modules of the program could not be listed' + LineEnding;
      Exit;
    end;
    Count := Needed div SizeOf(HMODULE);
    if Count > Length(Modules) then
      Count := Length(Modules);
    for I := 0 to Count - 1 do
      if (not IsPatched(Modules[I])) and ShouldPatch(Modules[I]) then
      begin
        Patched := PatchModule(Modules[I], HookList);
        J := Length(PatchedModules);
        SetLength(PatchedModules, J + 1);
        PatchedModules[J] := Modules[I];
        Report := Report + 'Patched ' + IntToStr(Patched) + ' imports of ' +
          ModuleName(Modules[I]) + LineEnding;
      end;
  finally
    LeaveCriticalSection(Lock);
  end;
end;

{ Libraries loaded after the hooks were installed, and the libraries they
  bring with them, are patched once they are loaded. Libraries loaded only
  as data or resources have no code to patch. }

function HookLoadLibraryA(Name: PAnsiChar): HMODULE; stdcall;
begin
  Result := OldLoadLibraryA(Name);
  if (Result <> 0) and Installed then
    PatchNewModules;
end;

function HookLoadLibraryW(Name: PWideChar): HMODULE; stdcall;
begin
  Result := OldLoadLibraryW(Name);
  if (Result <> 0) and Installed then
    PatchNewModules;
end;

const
  DataOnly = LOAD_LIBRARY_AS_DATAFILE or LOAD_LIBRARY_AS_DATAFILE_EXCLUSIVE or
    LOAD_LIBRARY_AS_IMAGE_RESOURCE;

function HookLoadLibraryExA(Name: PAnsiChar; Reserved: THandle; Flags: DWORD): HMODULE; stdcall;
begin
  Result := OldLoadLibraryExA(Name, Reserved, Flags);
  if (Result <> 0) and Installed and (Flags and DataOnly = 0) then
    PatchNewModules;
end;

function HookLoadLibraryExW(Name: PWideChar; Reserved: THandle; Flags: DWORD): HMODULE; stdcall;
begin
  Result := OldLoadLibraryExW(Name, Reserved, Flags);
  if (Result <> 0) and Installed and (Flags and DataOnly = 0) then
    PatchNewModules;
end;

{ Puts back every patched slot whose module is still loaded and which still
  holds the hook, so that nothing calls into this code after it is gone }

procedure RestoreSlots;
var
  I: Integer;
begin
  EnterCriticalSection(Lock);
  try
    for I := Length(Slots) - 1 downto 0 do
      if (Slots[I].Module <> 0) and (ModuleOfAddress(Slots[I].Slot) = Slots[I].Module) and
        (Slots[I].Slot^ = Slots[I].New) then
        WritePointer(Slots[I].Slot, Slots[I].Old);
    Slots := nil;
    PatchedModules := nil;
  finally
    LeaveCriticalSection(Lock);
  end;
end;

function DarkHooksInstall: Boolean;
var
  Buffer: array[0..MAX_PATH] of WideChar;
begin
  Result := False;
  if Installed then
    Exit(True);
  if not DarkModeLoad then
  begin
    Report := 'Dark mode is not available, so nothing was hooked';
    Exit;
  end;
  LoadLibrary('uxtheme.dll');
  LoadLibrary('psapi.dll');
  SetLength(HookList, 19);
  HookList[0] := Hook('user32.dll', 'GetSysColor', @HookGetSysColor, @@OldGetSysColor);
  HookList[1] := Hook('user32.dll', 'GetSysColorBrush', @HookGetSysColorBrush, @@OldGetSysColorBrush);
  HookList[2] := Hook('user32.dll', 'CreateWindowExW', @HookCreateWindowExW, @@OldCreateWindowExW);
  HookList[3] := Hook('gdi32.dll', 'DeleteObject', @HookDeleteObject, @@OldDeleteObject);
  HookList[4] := Hook('uxtheme.dll', 'OpenThemeData', @HookOpenThemeData, @@OldOpenThemeData);
  HookList[5] := Hook('uxtheme.dll', 'OpenThemeDataEx', @HookOpenThemeDataEx, @@OldOpenThemeDataEx);
  HookList[6] := Hook('uxtheme.dll', 'OpenThemeDataForDpi', @HookOpenThemeDataForDpi, @@OldOpenThemeDataForDpi);
  HookList[7] := Hook('uxtheme.dll', 'CloseThemeData', @HookCloseThemeData, @@OldCloseThemeData);
  HookList[8] := Hook('uxtheme.dll', 'DrawThemeBackground', @HookDrawThemeBackground, @@OldDrawThemeBackground);
  HookList[9] := Hook('uxtheme.dll', 'DrawThemeBackgroundEx', @HookDrawThemeBackgroundEx, @@OldDrawThemeBackgroundEx);
  HookList[10] := Hook('uxtheme.dll', 'DrawThemeText', @HookDrawThemeText, @@OldDrawThemeText);
  HookList[11] := Hook('uxtheme.dll', 'DrawThemeTextEx', @HookDrawThemeTextEx, @@OldDrawThemeTextEx);
  HookList[12] := Hook('user32.dll', 'DrawEdge', @HookDrawEdge, @@OldDrawEdge);
  HookList[13] := Hook('user32.dll', 'SystemParametersInfoA', @HookSystemParametersInfoA, @@OldSystemParametersInfoA);
  HookList[14] := Hook('user32.dll', 'SystemParametersInfoW', @HookSystemParametersInfoW, @@OldSystemParametersInfoW);
  HookList[15] := Hook('kernel32.dll', 'LoadLibraryA', @HookLoadLibraryA, @@OldLoadLibraryA);
  HookList[16] := Hook('kernel32.dll', 'LoadLibraryW', @HookLoadLibraryW, @@OldLoadLibraryW);
  HookList[17] := Hook('kernel32.dll', 'LoadLibraryExA', @HookLoadLibraryExA, @@OldLoadLibraryExA);
  HookList[18] := Hook('kernel32.dll', 'LoadLibraryExW', @HookLoadLibraryExW, @@OldLoadLibraryExW);
  if not (Assigned(OldGetSysColor) and Assigned(OldGetSysColorBrush) and
    Assigned(OldCreateWindowExW) and Assigned(OldDeleteObject) and
    Assigned(OldOpenThemeData) and Assigned(OldCloseThemeData) and
    Assigned(OldDrawThemeBackground) and Assigned(OldDrawThemeText) and
    Assigned(OldDrawThemeTextEx) and Assigned(OldDrawThemeBackgroundEx) and
    Assigned(OldDrawEdge) and Assigned(OldSystemParametersInfoA) and
    Assigned(OldSystemParametersInfoW) and Assigned(OldLoadLibraryA) and
    Assigned(OldLoadLibraryW) and Assigned(OldLoadLibraryExA) and
    Assigned(OldLoadLibraryExW)) then
  begin
    HookList := nil;
    Report := 'A routine to hook was not found, so nothing was hooked';
    Exit;
  end;
  Active := True;
  DarkStyle := DarkColorsPaletteIsDark;
  CallHook := SetWindowsHookExW(WH_CALLWNDPROC, CallWndProcHook, 0, GetCurrentThreadId);
  RetHook := SetWindowsHookExW(WH_CALLWNDPROCRET, CallWndRetHook, 0, GetCurrentThreadId);
  if DarkStyle then
    SetAppMode(pamForceDark)
  else
    SetAppMode(pamForceLight);
  Buffer[0] := #0;
  GetWindowsDirectoryW(@Buffer[0], MAX_PATH);
  WindowsDir := IncludeTrailingPathDelimiter(LowerCase(string(UnicodeString(PWideChar(@Buffer[0])))));
  Report := '';
  PatchNewModules;
  { The LCL calls uxtheme through the routine variables of the UxTheme unit }
  SavedOpenThemeData := @UxTheme.OpenThemeData;
  SavedCloseThemeData := @UxTheme.CloseThemeData;
  SavedDrawThemeBackground := @UxTheme.DrawThemeBackground;
  SavedDrawThemeText := @UxTheme.DrawThemeText;
  SavedDrawThemeTextEx := @UxTheme.DrawThemeTextEx;
  if Assigned(UxTheme.OpenThemeData) then
    @UxTheme.OpenThemeData := @HookOpenThemeData;
  if Assigned(UxTheme.CloseThemeData) then
    @UxTheme.CloseThemeData := @HookCloseThemeData;
  if Assigned(UxTheme.DrawThemeBackground) then
    @UxTheme.DrawThemeBackground := @HookDrawThemeBackground;
  if Assigned(UxTheme.DrawThemeText) then
    @UxTheme.DrawThemeText := @HookDrawThemeText;
  if Assigned(UxTheme.DrawThemeTextEx) then
    @UxTheme.DrawThemeTextEx := @HookDrawThemeTextEx;
  { The LCL opened its themes before the hooks, so they are opened again }
  ThemeServices.UpdateThemes;
  Report := Report + 'Hooks are active';
  Installed := True;
  Result := True;
end;

function DarkHooksReport: string;
begin
  Result := Report;
end;

{ Windows which existed before the hooks were installed have not had the
  subclasses new windows get, so they are added here. The LCL has subclassed
  these windows already, so the subclasses are outermost. Adding a subclass
  which is there already only updates it. }

function EnumExistingChild(Wnd: HWND; Param: LPARAM): BOOL; stdcall;
var
  Buffer: array[0..255] of WideChar;
  Info: TComboBoxInfo;
begin
  Buffer[0] := #0;
  GetClassNameW(Wnd, @Buffer[0], Length(Buffer));
  if IsComboBoxClass(string(UnicodeString(PWideChar(@Buffer[0])))) then
  begin
    SetWindowSubclass(Wnd, ComboSubclassProc, ComboSubclassId, 0);
    FillChar(Info, SizeOf(Info), 0);
    Info.cbSize := SizeOf(Info);
    if GetComboBoxInfo(Wnd, @Info) and (Info.hwndList <> 0) then
      ThemeWindow(Info.hwndList, Param <> 0);
  end;
  Result := True;
end;

function EnumThreadWindow(Wnd: HWND; Param: LPARAM): BOOL; stdcall;
begin
  SetWindowDarkMode(Wnd, Param <> 0);
  if GetWindowLong(Wnd, GWL_STYLE) and (WS_CHILD or WS_CAPTION) = WS_CAPTION then
    SetWindowSubclass(Wnd, MenuSubclassProc, MenuSubclassId, 0);
  EnumChildWindows(Wnd, EnumExistingChild, Param);
  if GetMenu(Wnd) <> 0 then
    Windows.DrawMenuBar(Wnd);
  Result := True;
end;

procedure DarkHooksSetMenuFont(const Face: string);
begin
  MenuFace := UnicodeString(Face);
end;

procedure DarkHooksSetTheme(Custom: Boolean);
begin
  if not Installed then
    Exit;
  Active := Custom;
  DarkStyle := Custom and DarkColorsPaletteIsDark;
  DarkColorsEnable(Custom);
  if DarkStyle then
    SetAppMode(pamForceDark)
  else
    SetAppMode(pamForceLight);
  { Themes are opened again so that they are opened through the hooks with
    the new style, and the brushes the LCL caches for system colors are made
    again from the new colors }
  ThemeServices.UpdateThemes;
  UpdateHandleObjects(True);
  EnumThreadWindows(GetCurrentThreadId, EnumThreadWindow, LPARAM(Ord(DarkStyle)));
end;

{ Removes the subclasses of a window, which does nothing for a subclass the
  window does not have }

procedure RemoveSubclasses(Wnd: HWND);
begin
  RemoveWindowSubclass(Wnd, MenuSubclassProc, MenuSubclassId);
  RemoveWindowSubclass(Wnd, PopupSubclassProc, PopupSubclassId);
  RemoveWindowSubclass(Wnd, ComboSubclassProc, ComboSubclassId);
end;

function EnumRemoveChild(Wnd: HWND; Param: LPARAM): BOOL; stdcall;
begin
  RemoveSubclasses(Wnd);
  Result := True;
end;

function EnumRemoveWindow(Wnd: HWND; Param: LPARAM): BOOL; stdcall;
begin
  RemoveSubclasses(Wnd);
  EnumChildWindows(Wnd, EnumRemoveChild, 0);
  Result := True;
end;

{ Restores the routine variables of the UxTheme unit which still hold the
  hooks }

procedure RestoreUxTheme;
begin
  if @UxTheme.OpenThemeData = @HookOpenThemeData then
    @UxTheme.OpenThemeData := SavedOpenThemeData;
  if @UxTheme.CloseThemeData = @HookCloseThemeData then
    @UxTheme.CloseThemeData := SavedCloseThemeData;
  if @UxTheme.DrawThemeBackground = @HookDrawThemeBackground then
    @UxTheme.DrawThemeBackground := SavedDrawThemeBackground;
  if @UxTheme.DrawThemeText = @HookDrawThemeText then
    @UxTheme.DrawThemeText := SavedDrawThemeText;
  if @UxTheme.DrawThemeTextEx = @HookDrawThemeTextEx then
    @UxTheme.DrawThemeTextEx := SavedDrawThemeTextEx;
end;

procedure DarkHooksUninstall;
begin
  if not Installed then
    Exit;
  Installed := False;
  Active := False;
  DarkStyle := False;
  MenuFace := '';
  if CallHook <> 0 then
    UnhookWindowsHookEx(CallHook);
  CallHook := 0;
  if RetHook <> 0 then
    UnhookWindowsHookEx(RetHook);
  RetHook := 0;
  Pending := nil;
  EnumThreadWindows(GetCurrentThreadId, EnumRemoveWindow, 0);
  RestoreUxTheme;
  RestoreSlots;
  HookList := nil;
  ThemeList := nil;
  SetAppMode(pamDefault);
  { The LCL opens its themes again without the hooks }
  ThemeServices.UpdateThemes;
end;

procedure DarkHooksModuleUnloaded(Module: HMODULE);
var
  I, J: Integer;
begin
  EnterCriticalSection(Lock);
  try
    for I := 0 to Length(Slots) - 1 do
      if Slots[I].Module = Module then
        Slots[I].Module := 0;
    for I := Length(PatchedModules) - 1 downto 0 do
      if PatchedModules[I] = Module then
      begin
        for J := I to Length(PatchedModules) - 2 do
          PatchedModules[J] := PatchedModules[J + 1];
        SetLength(PatchedModules, Length(PatchedModules) - 1);
      end;
  finally
    LeaveCriticalSection(Lock);
  end;
end;

initialization
  Active := False;
  InitCriticalSection(Lock);
finalization
  { When the program ends without DarkHooksUninstall, the hooks are taken
    out of the other modules, which may still run after this unit is gone }
  Active := False;
  Installed := False;
  if CallHook <> 0 then
    UnhookWindowsHookEx(CallHook);
  if RetHook <> 0 then
    UnhookWindowsHookEx(RetHook);
  RestoreUxTheme;
  RestoreSlots;
  DoneCriticalSection(Lock);
{$endif}
end.
