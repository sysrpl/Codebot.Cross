(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ Codebot.Themes.Windows.Colors replaces the system colors the LCL uses on
  Windows with the colors of a theme. The class of the widgetset object is
  swapped for a descendant which overrides GetSysColor and GetSysColorBrush,
  so clBtnFace, clWindow, clWindowText, and the other system colors return
  the colors of the palette in use, the colors of one of the themes of
  Codebot.Themes.Palettes. }

{ <include docs/codebot.themes.windows.colors.txt> }
unit Codebot.Themes.Windows.Colors;

{$mode delphi}

interface
{$ifdef windows}

uses
  Windows, SysUtils, LCLType, InterfaceBase, Win32Int,
  Codebot.Themes.Palettes;

{ Swaps the class of the widgetset, returning false if it is not the win32
  widgetset. Call it before any form is created, because brushes the LCL
  creates from system colors are cached with the colors they had then. }
function DarkColorsInstall: Boolean;
{ Gives the widgetset its own class back and turns the colors off }
procedure DarkColorsUninstall;
{ Turns the dark colors on or off }
procedure DarkColorsEnable(Value: Boolean);
{ True if the dark colors are on }
function DarkColorsEnabled: Boolean;
{ Chooses the palette, and whether it is dark. A color of NoColor is left
  as the system has it. The LCL must make its brushes again afterwards,
  which DarkHooksSetTheme does. }
procedure DarkColorsSetPalette(const Colors: TPaletteColors; Dark: Boolean);
{ True if the palette in use is dark, so that the dark parts of the visual
  styles of Windows go with it }
function DarkColorsPaletteIsDark: Boolean;
{ True if the dark colors are on and replace the system color Index }
function DarkColorMapped(Index: Integer): Boolean;
{ The dark value of the system color Index as a Windows color }
function DarkColor(Index: Integer): COLORREF;
{ A brush of the dark value of the system color Index, owned by this unit }
function DarkBrush(Index: Integer): HBRUSH;
{ True if Brush is one of the brushes owned by this unit }
function IsDarkBrush(Brush: HGDIOBJ): Boolean;

{$endif}

implementation
{$ifdef windows}

{ TDarkWidgetSet must not add fields, as an existing object takes its class }

type
  TDarkWidgetSet = class(TWin32WidgetSet)
  public
    function GetSysColor(NIndex: Integer): DWORD; override;
    function GetSysColorBrush(NIndex: Integer): HBrush; override;
  end;

var
  Enabled: Boolean;
  Current: TPaletteColors;
  CurrentDark: Boolean;
  Brushes: array[0..MAX_SYS_COLORS] of HBRUSH;
  { Brushes of palettes no longer in use, which may still be selected or
    cached somewhere, so they are kept until the program ends }
  Retired: array of HBRUSH;

{ Converts $RRGGBB to a Windows color, which is $BBGGRR }

function SwapRGB(C: LongInt): DWORD;
begin
  Result := ((C and $FF) shl 16) or (C and $FF00) or ((C shr 16) and $FF);
end;

function DarkColorMapped(Index: Integer): Boolean;
begin
  Result := Enabled and (Index >= 0) and (Index <= MAX_SYS_COLORS) and
    (Current[Index] <> NoColor);
end;

function DarkColor(Index: Integer): COLORREF;
begin
  if (Index >= 0) and (Index <= MAX_SYS_COLORS) and (Current[Index] <> NoColor) then
    Result := SwapRGB(Current[Index])
  else
    Result := 0;
end;

function DarkBrush(Index: Integer): HBRUSH;
begin
  if (Index < 0) or (Index > MAX_SYS_COLORS) then
    Exit(0);
  if Brushes[Index] = 0 then
    Brushes[Index] := CreateSolidBrush(DarkColor(Index));
  Result := Brushes[Index];
end;

function IsDarkBrush(Brush: HGDIOBJ): Boolean;
var
  I: Integer;
begin
  if Brush <> 0 then
  begin
    for I := Low(Brushes) to High(Brushes) do
      if Brushes[I] = Brush then
        Exit(True);
    for I := 0 to Length(Retired) - 1 do
      if Retired[I] = Brush then
        Exit(True);
  end;
  Result := False;
end;

{ TDarkWidgetSet }

function TDarkWidgetSet.GetSysColor(NIndex: Integer): DWORD;
begin
  if DarkColorMapped(NIndex) then
    Result := DarkColor(NIndex)
  else
    Result := inherited GetSysColor(NIndex);
end;

function TDarkWidgetSet.GetSysColorBrush(NIndex: Integer): HBrush;
begin
  if DarkColorMapped(NIndex) then
    Result := DarkBrush(NIndex)
  else
    Result := inherited GetSysColorBrush(NIndex);
end;

function DarkColorsInstall: Boolean;
begin
  Result := False;
  if WidgetSet = nil then
    Exit;
  if WidgetSet.ClassType = TDarkWidgetSet then
    Exit(True);
  if WidgetSet.ClassType <> TWin32WidgetSet then
    Exit;
  PPointer(WidgetSet)^ := Pointer(TDarkWidgetSet);
  Result := True;
end;

procedure DarkColorsUninstall;
begin
  Enabled := False;
  if (WidgetSet <> nil) and (WidgetSet.ClassType = TDarkWidgetSet) then
    PPointer(WidgetSet)^ := Pointer(TWin32WidgetSet);
end;

procedure DarkColorsEnable(Value: Boolean);
begin
  Enabled := Value;
end;

function DarkColorsEnabled: Boolean;
begin
  Result := Enabled;
end;

procedure DarkColorsSetPalette(const Colors: TPaletteColors; Dark: Boolean);
var
  I, J: Integer;
begin
  CurrentDark := Dark;
  if CompareMem(@Colors, @Current, SizeOf(Current)) then
    Exit;
  for I := Low(Brushes) to High(Brushes) do
    if Brushes[I] <> 0 then
    begin
      J := Length(Retired);
      SetLength(Retired, J + 1);
      Retired[J] := Brushes[I];
      Brushes[I] := 0;
    end;
  Current := Colors;
end;

function DarkColorsPaletteIsDark: Boolean;
begin
  Result := CurrentDark;
end;

{ Each brush is taken out of the lists before it is deleted, as a hook of
  DeleteObject refuses to delete the brushes in them }

procedure FreeBrushes;
var
  Brush: HBRUSH;
  I: Integer;
begin
  Enabled := False;
  for I := Low(Brushes) to High(Brushes) do
    if Brushes[I] <> 0 then
    begin
      Brush := Brushes[I];
      Brushes[I] := 0;
      DeleteObject(Brush);
    end;
  for I := 0 to Length(Retired) - 1 do
  begin
    Brush := Retired[I];
    Retired[I] := 0;
    DeleteObject(Brush);
  end;
end;

initialization
  Enabled := False;
  Current := DarkColors;
  CurrentDark := True;
finalization
  FreeBrushes;
{$endif}
end.
