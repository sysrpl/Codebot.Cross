(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ Codebot.Terminal.Controls.Gtk2 implements the terminal control for the Gtk2
  widgetset using version 0.28 of the VTE library, which is linked when a
  program using this unit is built. Building needs the libvte-dev package to
  be installed. While the control is being designed it paints a message. }

unit Codebot.Terminal.Controls.Gtk2;

{$i terminal.inc}

interface

{$ifdef terminalgtk2}
uses
  Classes, SysUtils, Graphics, Controls, LCLType, WSControls, WSLCLClasses,
  Codebot.Terminal.Types;

{ TWSTerminal is the widgetset class of the terminal control. The methods
  after CreateHandle do nothing unless the control has a terminal widget. }

type
  TWSTerminal = class(TWSWinControl)
  published
    class function CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLHandle; override;
  public
    { True if the control has a terminal widget, false if it paints itself }
    class function HasTerminal(AWinControl: TWinControl): Boolean;
    { Set the sixteen numbered colors together with the text and background
      colors. The other colors must be set again afterwards. }
    class procedure SetPalette(AWinControl: TWinControl; Palette: TTerminalPalette;
      Fore, Back: TColor);
    class procedure SetColor(AWinControl: TWinControl; Element: TTerminalElement; Color: TColor);
    class procedure SetFont(AWinControl: TWinControl; Font: TFont);
    { Show or hide the vertical scroll bar beside the terminal }
    class procedure SetScrollBar(AWinControl: TWinControl; Visible: Boolean);
    { Replace the terminal with a new one running the shell }
    class procedure Restart(AWinControl: TWinControl);
  end;

{ The VTE library is linked, so a terminal is always available }
function TerminalLoad: Boolean;
{$endif}

implementation

{$ifdef terminalgtk2}
uses
  GLib2, Gtk2, Gtk2Def, Gtk2Proc, Pango, Gtk2WSControls;

{ The part of the VTE library which is used }

{$define libvte := external 'vte'}

const
  VTE_PTY_DEFAULT = 0;
  G_SPAWN_SEARCH_PATH = 4;

type
  PVteTerminal = Pointer;

  TVteColor = packed record
    pixel: LongWord;
    red, green, blue: Word;
  end;
  PVteColor = ^TVteColor;

function vte_terminal_new: PGtkWidget; cdecl; libvte;
function vte_terminal_fork_command_full(terminal: PVteTerminal; pty_flags: LongWord;
  working_directory: PChar; argv, envv: PPChar; spawn_flags: LongWord;
  child_setup, child_setup_data, child_pid, error: Pointer): gboolean; cdecl; libvte;
procedure vte_terminal_set_allow_bold(terminal: PVteTerminal; allow_bold: gboolean); cdecl; libvte;
procedure vte_terminal_set_font(terminal: PVteTerminal; font_desc: PPangoFontDescription); cdecl; libvte;
procedure vte_terminal_set_color_foreground(terminal: PVteTerminal; color: PVteColor); cdecl; libvte;
procedure vte_terminal_set_color_background(terminal: PVteTerminal; color: PVteColor); cdecl; libvte;
procedure vte_terminal_set_color_bold(terminal: PVteTerminal; color: PVteColor); cdecl; libvte;
procedure vte_terminal_set_color_dim(terminal: PVteTerminal; color: PVteColor); cdecl; libvte;
procedure vte_terminal_set_colors(terminal: PVteTerminal; foreground, background,
  palette: PVteColor; palette_size: PtrInt); cdecl; libvte;
procedure vte_terminal_set_default_colors(terminal: PVteTerminal); cdecl; libvte;
function vte_terminal_get_adjustment(terminal: PVteTerminal): PGtkAdjustment; cdecl; libvte;

function TerminalLoad: Boolean;
begin
  Result := True;
end;

{ The widget of the control is a frame holding the terminal. The widget info
  of the control is kept on the terminal under the key the widgetset uses, so
  both the widgetset and the signals of the terminal can find it. A key of
  its own marks the widget as a terminal, since while designing the frame
  holds a plain widget instead. }

const
  InfoKey = 'widgetinfo';
  TerminalKey = 'terminalwidget';
  ReadyKey = 'terminalready';
  { The box holding the terminal and its scroll bar, and the scroll bar,
    are kept on the terminal }
  BoxKey = 'terminalbox';
  BarKey = 'terminalbar';

function GetEvents(Widget: PGtkWidget; out Events: ITerminalEvents): Boolean;
var
  Info: PWidgetInfo;
begin
  Events := nil;
  Info := PWidgetInfo(g_object_get_data(PGObject(Widget), InfoKey));
  Result := (Info <> nil) and (Info.LCLObject <> nil) and
    Supports(Info.LCLObject, ITerminalEvents, Events);
end;

{ The terminal sends contents-changed whenever what it shows changes. The
  first time is when the shell has written its prompt. }

procedure TerminalContentsChanged(Widget: PGtkWidget); cdecl;
var
  Events: ITerminalEvents;
begin
  if g_object_get_data(PGObject(Widget), ReadyKey) <> nil then
    Exit;
  g_object_set_data(PGObject(Widget), ReadyKey, Widget);
  if GetEvents(Widget, Events) then
    Events.TerminalReady;
end;

procedure TerminalChildExited(Widget: PGtkWidget); cdecl;
var
  Events: ITerminalEvents;
begin
  if GetEvents(Widget, Events) then
    Events.TerminalExited;
end;

{ Make a terminal running the shell of the user, and place it in the frame
  inside a box with a vertical scroll bar to its right }

procedure NewTerminal(Info: PWidgetInfo);
var
  Shell: string;
  Args: array[0..1] of PChar;
  Box, Bar: PGtkWidget;
begin
  Shell := g_getenv('SHELL');
  if Shell = '' then
    Shell := '/bin/bash';
  Args[0] := PChar(Shell);
  Args[1] := nil;
  Info.ClientWidget := vte_terminal_new;
  g_object_set_data(PGObject(Info.ClientWidget), InfoKey, Info);
  g_object_set_data(PGObject(Info.ClientWidget), TerminalKey, Info);
  g_signal_connect(Info.ClientWidget, 'contents-changed', G_CALLBACK(@TerminalContentsChanged), nil);
  g_signal_connect(Info.ClientWidget, 'child-exited', G_CALLBACK(@TerminalChildExited), nil);
  vte_terminal_set_allow_bold(Info.ClientWidget, True);
  vte_terminal_fork_command_full(Info.ClientWidget, VTE_PTY_DEFAULT, nil, @Args[0],
    nil, G_SPAWN_SEARCH_PATH, nil, nil, nil, nil);
  Box := gtk_hbox_new(False, 0);
  Bar := gtk_vscrollbar_new(vte_terminal_get_adjustment(Info.ClientWidget));
  gtk_box_pack_start(GTK_BOX(Box), Info.ClientWidget, True, True, 0);
  gtk_box_pack_start(GTK_BOX(Box), Bar, False, False, 0);
  g_object_set_data(PGObject(Info.ClientWidget), BoxKey, Box);
  g_object_set_data(PGObject(Info.ClientWidget), BarKey, Bar);
  gtk_container_add(GTK_CONTAINER(Info.CoreWidget), Box);
  gtk_widget_show_all(Info.CoreWidget);
end;

{ The terminal of a control, or nil if the control paints itself }

function Terminal(AWinControl: TWinControl; out Info: PWidgetInfo): PVteTerminal;
begin
  Result := nil;
  Info := nil;
  if (not AWinControl.HandleAllocated) or (csDesigning in AWinControl.ComponentState) then
    Exit;
  Info := GetWidgetInfo({%H-}Pointer(AWinControl.Handle));
  if (Info <> nil) and (Info.ClientWidget <> nil) and
    (g_object_get_data(PGObject(Info.ClientWidget), TerminalKey) = Info) then
    Result := Info.ClientWidget;
end;

{ TWSTerminal }

class function TWSTerminal.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLHandle;
var
  Info: PWidgetInfo;
  Style: PGtkRCStyle;
  Allocation: TGTKAllocation;
begin
  { Initialize widget info }
  Info := CreateWidgetInfo(gtk_frame_new(nil), AWinControl, AParams);
  Info.LCLObject := AWinControl;
  Info.Style := AParams.Style;
  Info.ExStyle := AParams.ExStyle;
  Info.WndProc := {%H-}PtrUInt(AParams.WindowClass.lpfnWndProc);
  { Configure core and client }
  gtk_frame_set_shadow_type(PGtkFrame(Info.CoreWidget), GTK_SHADOW_NONE);
  Style := gtk_widget_get_modifier_style(Info.CoreWidget);
  Style.xthickness := 0;
  Style.ythickness := 0;
  gtk_widget_modify_style(Info.CoreWidget, Style);
  GTK_WIDGET_SET_FLAGS(Info.CoreWidget, GTK_CAN_FOCUS);
  { While designing the frame holds a plain widget which the control paints
    on. The widgetset has no window of its own to fall back on for a custom
    control, so the frame is made either way. }
  if csDesigning in AWinControl.ComponentState then
  begin
    Info.ClientWidget := CreateFixedClientWidget(True);
    g_object_set_data(PGObject(Info.ClientWidget), InfoKey, Info);
    gtk_container_add(GTK_CONTAINER(Info.CoreWidget), Info.ClientWidget);
    gtk_widget_show_all(Info.CoreWidget);
  end
  else
    NewTerminal(Info);
  Allocation.X := AParams.X;
  Allocation.Y := AParams.Y;
  Allocation.Width := AParams.Width;
  Allocation.Height := AParams.Height;
  gtk_widget_size_allocate(Info.CoreWidget, @Allocation);
  TGtk2WSWinControl.SetCallbacks(PGtkObject(Info.CoreWidget), TComponent(Info.LCLObject));
  Result := {%H-}TLCLHandle(Info.CoreWidget);
end;

class function TWSTerminal.HasTerminal(AWinControl: TWinControl): Boolean;
var
  Info: PWidgetInfo;
begin
  Result := Terminal(AWinControl, Info) <> nil;
end;

{ Convert a color of the LCL, which is $BBGGRR. Each part of a gdk color is
  sixteen bits. }

function ColorToVte(Color: TColor): TVteColor;
var
  RGB: LongInt;
begin
  RGB := ColorToRGB(Color);
  Result.pixel := 0;
  Result.red := (RGB and $FF) * $101;
  Result.green := ((RGB shr 8) and $FF) * $101;
  Result.blue := ((RGB shr 16) and $FF) * $101;
end;

class procedure TWSTerminal.SetPalette(AWinControl: TWinControl; Palette: TTerminalPalette;
  Fore, Back: TColor);
var
  T: PVteTerminal;
  Info: PWidgetInfo;
  F, B: TVteColor;
  Colors: array[0..15] of TVteColor;
  V: LongWord;
  I: Integer;
begin
  T := Terminal(AWinControl, Info);
  if T = nil then
    Exit;
  if Palette = tpDefault then
  begin
    vte_terminal_set_default_colors(T);
    Exit;
  end;
  { The colors of a palette are $RRGGBB }
  for I := Low(Colors) to High(Colors) do
  begin
    V := TerminalPalettes[Palette][I];
    Colors[I].pixel := 0;
    Colors[I].red := ((V shr 16) and $FF) * $101;
    Colors[I].green := ((V shr 8) and $FF) * $101;
    Colors[I].blue := (V and $FF) * $101;
  end;
  F := ColorToVte(Fore);
  B := ColorToVte(Back);
  vte_terminal_set_colors(T, @F, @B, @Colors[0], Length(Colors));
end;

class procedure TWSTerminal.SetColor(AWinControl: TWinControl; Element: TTerminalElement;
  Color: TColor);
var
  T: PVteTerminal;
  Info: PWidgetInfo;
  C: TVteColor;
begin
  T := Terminal(AWinControl, Info);
  if T = nil then
    Exit;
  C := ColorToVte(Color);
  case Element of
    teFore: vte_terminal_set_color_foreground(T, @C);
    teBack: vte_terminal_set_color_background(T, @C);
    teBold: vte_terminal_set_color_bold(T, @C);
    teDim: vte_terminal_set_color_dim(T, @C);
    { Setting the cursor color with this version of the library removes the
      cursor entirely, so it is left alone }
    teCursor: ;
    { This version of the library has a color for behind selected text but
      none for the text, which keeps the color it had. A color behind it
      could then make the text impossible to read, so neither is set and
      the terminal swaps the text and background colors of a selection. }
    teHighlight, teHighlightText: ;
  end;
end;

{ The font is described to pango by its name, style, and size, such as
  'Monospace Bold 10'. A size of zero leaves the size to the terminal. }

class procedure TWSTerminal.SetFont(AWinControl: TWinControl; Font: TFont);
var
  T: PVteTerminal;
  Info: PWidgetInfo;
  D: PPangoFontDescription;
  S: string;
begin
  T := Terminal(AWinControl, Info);
  if T = nil then
    Exit;
  S := Font.Name;
  if (S = '') or (LowerCase(S) = 'default') then
    S := 'Monospace';
  if fsBold in Font.Style then
    S := S + ' Bold';
  if fsItalic in Font.Style then
    S := S + ' Italic';
  if Font.Size > 0 then
    S := S + ' ' + IntToStr(Font.Size);
  D := pango_font_description_from_string(PChar(S));
  if D = nil then
    Exit;
  vte_terminal_set_font(T, D);
  pango_font_description_free(D);
end;

class procedure TWSTerminal.SetScrollBar(AWinControl: TWinControl; Visible: Boolean);
var
  T: PVteTerminal;
  Info: PWidgetInfo;
  Bar: PGtkWidget;
begin
  T := Terminal(AWinControl, Info);
  if T = nil then
    Exit;
  Bar := PGtkWidget(g_object_get_data(PGObject(T), BarKey));
  if Bar = nil then
    Exit;
  if Visible then
    gtk_widget_show(Bar)
  else
    gtk_widget_hide(Bar);
end;

class procedure TWSTerminal.Restart(AWinControl: TWinControl);
var
  Info: PWidgetInfo;
begin
  if Terminal(AWinControl, Info) = nil then
    Exit;
  { Destroying the box destroys the terminal and scroll bar inside it }
  gtk_widget_destroy(PGtkWidget(g_object_get_data(PGObject(Info.ClientWidget), BoxKey)));
  NewTerminal(Info);
end;
{$endif}

end.
