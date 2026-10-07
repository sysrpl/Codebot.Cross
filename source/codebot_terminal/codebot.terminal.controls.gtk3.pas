(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ Codebot.Terminal.Controls.Gtk3 implements the terminal control for the Gtk3
  widgetset using version 2.91 of the VTE library, which is linked when a
  program using this unit is built. Building needs the libvte-2.91-dev package
  to be installed. While the control is being designed it has an ordinary
  window which the control paints on. }

unit Codebot.Terminal.Controls.Gtk3;

{$i terminal.inc}

interface

{$ifdef terminalgtk3}
uses
  Classes, SysUtils, Graphics, Controls, LCLType, WSControls, WSLCLClasses,
  Codebot.Terminal.Types;

{ TWSTerminal is the widgetset class of the terminal control. The methods
  after CreateHandle do nothing unless the control has a terminal widget. }

type
  TWSTerminal = class(TWSWinControl)
  published
    class function CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): HWND; override;
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
    { Clear the terminal and run the shell in it again }
    class procedure Restart(AWinControl: TWinControl);
  end;

{ The VTE library is linked, so a terminal is always available }
function TerminalLoad: Boolean;
{$endif}

implementation

{$ifdef terminalgtk3}
uses
  LazGLib2, LazGObject2, LazGdk3, LazGtk3, LazPango1, Gtk3Widgets;

{ The part of the VTE library which is used }

{$define libvte := external 'vte-2.91'}

const
  VTE_PTY_DEFAULT = 0;
  G_SPAWN_SEARCH_PATH = 4;

type
  PVteTerminal = Pointer;

function vte_terminal_new: PGtkWidget; cdecl; libvte;
procedure vte_terminal_spawn_async(terminal: PVteTerminal; pty_flags: LongWord;
  working_directory: PChar; argv, envv: PPChar; spawn_flags: LongWord;
  child_setup, child_setup_data, child_setup_data_destroy: Pointer;
  timeout: LongInt; cancellable, callback, user_data: Pointer); cdecl; libvte;
procedure vte_terminal_reset(terminal: PVteTerminal; clear_tabstops,
  clear_history: gboolean); cdecl; libvte;
procedure vte_terminal_set_font(terminal: PVteTerminal; font_desc: PPangoFontDescription); cdecl; libvte;
procedure vte_terminal_set_color_foreground(terminal: PVteTerminal; color: PGdkRGBA); cdecl; libvte;
procedure vte_terminal_set_color_background(terminal: PVteTerminal; color: PGdkRGBA); cdecl; libvte;
procedure vte_terminal_set_color_bold(terminal: PVteTerminal; color: PGdkRGBA); cdecl; libvte;
procedure vte_terminal_set_color_cursor(terminal: PVteTerminal; color: PGdkRGBA); cdecl; libvte;
procedure vte_terminal_set_color_highlight(terminal: PVteTerminal; color: PGdkRGBA); cdecl; libvte;
procedure vte_terminal_set_color_highlight_foreground(terminal: PVteTerminal; color: PGdkRGBA); cdecl; libvte;
procedure vte_terminal_set_colors(terminal: PVteTerminal; foreground, background,
  palette: PGdkRGBA; palette_size: PtrUInt); cdecl; libvte;
procedure vte_terminal_set_default_colors(terminal: PVteTerminal); cdecl; libvte;

function TerminalLoad: Boolean;
begin
  Result := True;
end;

{ TGtk3Terminal is a scrolled window holding a VTE terminal. The scrolled
  window is the widget of the control, and gives the terminal its vertical
  scroll bar. Terminal is the VTE widget inside it. }

type
  TGtk3Terminal = class(TGtk3Widget)
  private
    FTerminal: PGtkWidget;
    FReady: Boolean;
  protected
    function CreateWidget(const Params: TCreateParams): PGtkWidget; override;
  public
    procedure SetFocus; override;
    procedure SetScrollBar(Visible: Boolean);
    property Terminal: PGtkWidget read FTerminal;
    function GtkEventKey(Sender: PGtkWidget; Event: PGdkEvent; AKeyPress: Boolean): Boolean; override; cdecl;
    function GetEvents(out Events: ITerminalEvents): Boolean;
    { Run the shell of the user in the terminal }
    procedure Spawn;
  end;

{ The terminal sends contents-changed whenever what it shows changes. The
  first time is when the shell has written its prompt. }

procedure TerminalContentsChanged(Widget: PGtkWidget; Data: TGtk3Terminal); cdecl;
var
  Events: ITerminalEvents;
begin
  if Data.FReady then
    Exit;
  Data.FReady := True;
  if Data.GetEvents(Events) then
    Events.TerminalReady;
end;

procedure TerminalChildExited(Widget: PGtkWidget; Status: gint; Data: TGtk3Terminal); cdecl;
var
  Events: ITerminalEvents;
begin
  if Data.GetEvents(Events) then
    Events.TerminalExited;
end;

{ The terminal scrolls itself, so the scrolled window only adds the scroll
  bar. There is never a horizontal one, as a terminal wraps its lines. The
  bar is beside the terminal and not over it, so it does not hide text. }

function TGtk3Terminal.CreateWidget(const Params: TCreateParams): PGtkWidget;
var
  Scrolled: PGtkScrolledWindow;
begin
  FTerminal := vte_terminal_new();
  g_signal_connect_data(PGObject(FTerminal), 'contents-changed',
    TGCallback(@TerminalContentsChanged), Self, nil, G_CONNECT_DEFAULT);
  g_signal_connect_data(PGObject(FTerminal), 'child-exited',
    TGCallback(@TerminalChildExited), Self, nil, G_CONNECT_DEFAULT);
  Scrolled := gtk_scrolled_window_new(nil, nil);
  gtk_scrolled_window_set_policy(Scrolled, GTK_POLICY_NEVER, GTK_POLICY_ALWAYS);
  gtk_scrolled_window_set_overlay_scrolling(Scrolled, False);
  gtk_container_add(PGtkContainer(Scrolled), FTerminal);
  gtk_widget_show(FTerminal);
  { The terminal is the central widget, which ties it to the control as well
    as the scrolled window. Without this the widgetset finds no control for
    the widget with the input focus, and passes each key to the input method
    of the form, which takes every character before the terminal sees it. }
  FCentralWidget := FTerminal;
  Result := PGtkWidget(Scrolled);
end;

{ The input focus belongs to the terminal, not the scrolled window around it }

procedure TGtk3Terminal.SetFocus;
begin
  if FTerminal <> nil then
    gtk_widget_grab_focus(FTerminal)
  else
    inherited SetFocus;
end;

{ An external policy hides the scroll bar but leaves the terminal able to
  scroll with the mouse wheel and the keyboard }

procedure TGtk3Terminal.SetScrollBar(Visible: Boolean);
begin
  if Widget = nil then
    Exit;
  if Visible then
    gtk_scrolled_window_set_policy(PGtkScrolledWindow(Widget), GTK_POLICY_NEVER,
      GTK_POLICY_ALWAYS)
  else
    gtk_scrolled_window_set_policy(PGtkScrolledWindow(Widget), GTK_POLICY_NEVER,
      GTK_POLICY_EXTERNAL);
end;

{ GtkEventKey replaces the one inherited from TGtk3Widget, which passes keys
  through the input method of the LCL and stops some keys, such as tab and
  return, from reaching the widget. The terminal has its own input method and
  needs every key. }

function TGtk3Terminal.GtkEventKey(Sender: PGtkWidget; Event: PGdkEvent; AKeyPress: Boolean): Boolean; cdecl;
begin
  Result := False;
end;

{ The terminal can send signals while it is being destroyed, at which point
  the control might not exist }

function TGtk3Terminal.GetEvents(out Events: ITerminalEvents): Boolean;
begin
  Events := nil;
  Result := (LCLObject <> nil) and Supports(LCLObject, ITerminalEvents, Events);
end;

procedure TGtk3Terminal.Spawn;
var
  Shell: string;
  Args: array[0..1] of PChar;
begin
  Shell := g_getenv('SHELL');
  if Shell = '' then
    Shell := '/bin/bash';
  Args[0] := PChar(Shell);
  Args[1] := nil;
  FReady := False;
  vte_terminal_spawn_async(FTerminal, VTE_PTY_DEFAULT, nil, @Args[0], nil,
    G_SPAWN_SEARCH_PATH, nil, nil, nil, -1, nil, nil, nil);
end;

function Terminal(AWinControl: TWinControl): TGtk3Terminal;
var
  Widget: TGtk3Widget;
begin
  Result := nil;
  if not AWinControl.HandleAllocated then
    Exit;
  Widget := TGtk3Widget(AWinControl.Handle);
  if Widget is TGtk3Terminal then
    Result := TGtk3Terminal(Widget);
end;

{ TWSTerminal }

class function TWSTerminal.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  T: TGtk3Terminal;
begin
  if csDesigning in AWinControl.ComponentState then
    Exit({%H-}TLCLHandle(TGtk3WinControlPanel.Create(AWinControl, AParams)));
  T := TGtk3Terminal.Create(AWinControl, AParams);
  T.Spawn;
  Result := {%H-}TLCLHandle(T);
end;

class function TWSTerminal.HasTerminal(AWinControl: TWinControl): Boolean;
begin
  Result := Terminal(AWinControl) <> nil;
end;

{ Convert a color of the LCL, which is $BBGGRR }

function ColorToGdk(Color: TColor): TGdkRGBA;
var
  RGB: LongInt;
begin
  RGB := ColorToRGB(Color);
  Result.red := (RGB and $FF) / $FF;
  Result.green := ((RGB shr 8) and $FF) / $FF;
  Result.blue := ((RGB shr 16) and $FF) / $FF;
  Result.alpha := 1;
end;

class procedure TWSTerminal.SetPalette(AWinControl: TWinControl; Palette: TTerminalPalette;
  Fore, Back: TColor);
var
  T: TGtk3Terminal;
  F, B: TGdkRGBA;
  Colors: array[0..15] of TGdkRGBA;
  V: LongWord;
  I: Integer;
begin
  T := Terminal(AWinControl);
  if T = nil then
    Exit;
  if Palette = tpDefault then
  begin
    vte_terminal_set_default_colors(T.Terminal);
    Exit;
  end;
  { The colors of a palette are $RRGGBB }
  for I := Low(Colors) to High(Colors) do
  begin
    V := TerminalPalettes[Palette][I];
    Colors[I].red := ((V shr 16) and $FF) / $FF;
    Colors[I].green := ((V shr 8) and $FF) / $FF;
    Colors[I].blue := (V and $FF) / $FF;
    Colors[I].alpha := 1;
  end;
  F := ColorToGdk(Fore);
  B := ColorToGdk(Back);
  vte_terminal_set_colors(T.Terminal, @F, @B, @Colors[0], Length(Colors));
end;

class procedure TWSTerminal.SetColor(AWinControl: TWinControl; Element: TTerminalElement;
  Color: TColor);
var
  T: TGtk3Terminal;
  C: TGdkRGBA;
begin
  T := Terminal(AWinControl);
  if T = nil then
    Exit;
  C := ColorToGdk(Color);
  case Element of
    teFore: vte_terminal_set_color_foreground(T.Terminal, @C);
    teBack: vte_terminal_set_color_background(T.Terminal, @C);
    teBold: vte_terminal_set_color_bold(T.Terminal, @C);
    teCursor: vte_terminal_set_color_cursor(T.Terminal, @C);
    teHighlight: vte_terminal_set_color_highlight(T.Terminal, @C);
    teHighlightText: vte_terminal_set_color_highlight_foreground(T.Terminal, @C);
    { This version of the library has no color for dim text }
    teDim: ;
  end;
end;

{ The font is described to pango by its name, style, and size, such as
  'Monospace Bold 10'. A size of zero leaves the size to the terminal. }

class procedure TWSTerminal.SetFont(AWinControl: TWinControl; Font: TFont);
var
  T: TGtk3Terminal;
  D: PPangoFontDescription;
  S: string;
begin
  T := Terminal(AWinControl);
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
  vte_terminal_set_font(T.Terminal, D);
  pango_font_description_free(D);
end;

class procedure TWSTerminal.SetScrollBar(AWinControl: TWinControl; Visible: Boolean);
var
  T: TGtk3Terminal;
begin
  T := Terminal(AWinControl);
  if T <> nil then
    T.SetScrollBar(Visible);
end;

class procedure TWSTerminal.Restart(AWinControl: TWinControl);
var
  T: TGtk3Terminal;
begin
  T := Terminal(AWinControl);
  if T = nil then
    Exit;
  vte_terminal_reset(T.Terminal, True, True);
  T.Spawn;
end;
{$endif}

end.
