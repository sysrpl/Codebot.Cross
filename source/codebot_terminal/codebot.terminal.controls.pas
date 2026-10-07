(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ Codebot.Terminal.Controls holds TTerminal, a control which is a terminal
  emulator running the shell of the user. It uses the VTE library of the
  gnome terminal, and the package only builds with the Gtk2 and Gtk3
  widgetsets.

  The library is linked when a program using the control is built, which
  needs the libvte-2.91-dev package for Gtk3 or the libvte-dev package for
  Gtk2. While the control is being designed there is no terminal, and the
  control paints itself instead as a prompt and a cursor in its colors and
  font. }

unit Codebot.Terminal.Controls;

{$i terminal.inc}

interface

uses
  Classes, SysUtils, Controls, Graphics, LCLType, LCLIntf, LMessages,
  Codebot.Terminal.Types;

{ TTerminalColors are the colors of a terminal. Highlight is the color behind
  selected text and HighlightText is the color of the selected text, which
  are dark text on a light background by default so a selection can be read.
  Dim is not used with Gtk3. Cursor, Highlight, and HighlightText are not
  used with Gtk2, where a selection swaps the text and background colors.

  Palette chooses the sixteen colors which programs ask for by number, such
  as the colors of file names listed by ls and of a colored prompt. It is
  the palette of the gnome terminal by default. }

type
  TTerminalColors = class(TPersistent)
  private
    FColors: array[TTerminalElement] of TColor;
    FPalette: TTerminalPalette;
    FOnChange: TNotifyEvent;
    procedure SetPalette(Value: TTerminalPalette);
    procedure SetColor(Index: Integer; Value: TColor);
    function GetColor(Index: Integer): TColor;
  protected
    { Invoke OnChange }
    procedure Change;
  public
    { Create the colors, which are silver text on black, and black text on
      silver where text is selected }
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    { OnChange is invoked when a color changes }
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Foreground: TColor index 0 read GetColor write SetColor default clSilver;
    property Background: TColor index 1 read GetColor write SetColor default clBlack;
    property Bold: TColor index 2 read GetColor write SetColor default clWhite;
    property Dim: TColor index 3 read GetColor write SetColor default clGray;
    property Cursor: TColor index 4 read GetColor write SetColor default clWhite;
    property Highlight: TColor index 5 read GetColor write SetColor default clSilver;
    property HighlightText: TColor index 6 read GetColor write SetColor default clBlack;
    property Palette: TTerminalPalette read FPalette write SetPalette default tpTango;
  end;

{ TCustomTerminal is a terminal emulator. The shell starts when the control
  is first shown. OnReady is invoked when the shell has written its prompt,
  and OnTerminate when the shell ends, such as when exit is typed. Restart
  clears the terminal and starts the shell again, and can be called from
  OnTerminate to keep the terminal going. }

  TCustomTerminal = class(TWinControl, ITerminalEvents)
  private
    FCanvas: TControlCanvas;
    FColors: TTerminalColors;
    FScrollBar: Boolean;
    FOnReady: TNotifyEvent;
    FOnTerminate: TNotifyEvent;
    procedure SetColors(Value: TTerminalColors);
    procedure SetScrollBar(Value: Boolean);
    procedure ColorsChange(Sender: TObject);
    procedure UpdateTerminal;
    { ITerminalEvents }
    procedure TerminalReady;
    procedure TerminalExited;
    procedure WMPaint(var Message: TLMPaint); message LM_PAINT;
  protected
    class procedure WSRegisterClass; override;
    procedure InitializeWnd; override;
    procedure DestroyWnd; override;
    procedure FontChanged(Sender: TObject); override;
    procedure PaintWindow(DC: HDC); override;
    { Paint the control when it has no terminal, using ControlCanvas }
    procedure Paint; virtual;
    { ControlCanvas is used to paint the control when it has no terminal }
    property ControlCanvas: TControlCanvas read FCanvas;
    { Invoke OnReady }
    procedure DoReady; virtual;
    { Invoke OnTerminate }
    procedure DoTerminate; virtual;
    { The colors of the terminal }
    property Colors: TTerminalColors read FColors write SetColors;
    { When true a vertical scroll bar is shown beside the terminal to scroll
      back through its output. When false the mouse wheel still scrolls. }
    property ScrollBar: Boolean read FScrollBar write SetScrollBar default True;
    { OnReady is invoked when the shell has written its prompt }
    property OnReady: TNotifyEvent read FOnReady write FOnReady;
    { OnTerminate is invoked when the shell ends }
    property OnTerminate: TNotifyEvent read FOnTerminate write FOnTerminate;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { True if the control is showing a terminal, false if it paints a message }
    function HasTerminal: Boolean;
    { Clear the terminal and start the shell again }
    procedure Restart;
  end;

{ TTerminal }

  TTerminal = class(TCustomTerminal)
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property Colors;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentShowHint;
    property PopupMenu;
    property ScrollBar;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseDockManager;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnGetDockCaption;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnReady;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    property OnTerminate;
  end;

{ Returns true, as the package only builds where a terminal can be shown }
function TerminalAvailable: Boolean;

implementation

uses
  WSLCLClasses,
{$ifdef terminalgtk2}
  Codebot.Terminal.Controls.Gtk2;
{$endif}
{$ifdef terminalgtk3}
  Codebot.Terminal.Controls.Gtk3;
{$endif}

function TerminalAvailable: Boolean;
begin
  Result := TerminalLoad;
end;

{ TTerminalColors }

constructor TTerminalColors.Create;
begin
  inherited Create;
  FColors[teFore] := clSilver;
  FColors[teBack] := clBlack;
  FColors[teBold] := clWhite;
  FColors[teDim] := clGray;
  FColors[teCursor] := clWhite;
  FColors[teHighlight] := clSilver;
  FColors[teHighlightText] := clBlack;
  FPalette := tpTango;
end;

procedure TTerminalColors.SetPalette(Value: TTerminalPalette);
begin
  if Value = FPalette then
    Exit;
  FPalette := Value;
  Change;
end;

procedure TTerminalColors.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TTerminalColors.Assign(Source: TPersistent);
var
  C: TTerminalColors;
  E: TTerminalElement;
begin
  if Source = Self then
    Exit;
  if Source is TTerminalColors then
  begin
    C := Source as TTerminalColors;
    for E := Low(FColors) to High(FColors) do
      FColors[E] := C.FColors[E];
    FPalette := C.FPalette;
    Change;
  end
  else
    inherited Assign(Source);
end;

procedure TTerminalColors.SetColor(Index: Integer; Value: TColor);
var
  E: TTerminalElement;
begin
  E := TTerminalElement(Index);
  if Value = FColors[E] then
    Exit;
  FColors[E] := Value;
  Change;
end;

function TTerminalColors.GetColor(Index: Integer): TColor;
begin
  Result := FColors[TTerminalElement(Index)];
end;

{ TCustomTerminal }

var
  Registered: Boolean;

class procedure TCustomTerminal.WSRegisterClass;
begin
  inherited WSRegisterClass;
  if Registered then
    Exit;
  Registered := True;
  RegisterWSComponent(TCustomTerminal, TWSTerminal);
end;

constructor TCustomTerminal.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColors := TTerminalColors.Create;
  FColors.OnChange := ColorsChange;
  FScrollBar := True;
  ControlStyle := ControlStyle - [csSetCaption];
  Width := 300;
  Height := 200;
  TabStop := True;
  ParentFont := False;
  Font.Name := 'Monospace';
end;

destructor TCustomTerminal.Destroy;
begin
  { The colors are used when painting, so they outlive the window }
  inherited Destroy;
  FColors.Free;
end;

function TCustomTerminal.HasTerminal: Boolean;
begin
  Result := HandleAllocated and TWSTerminal.HasTerminal(Self);
end;

{ A terminal is created with each window, so the colors and font kept by the
  control are applied to the new terminal }

procedure TCustomTerminal.UpdateTerminal;
var
  E: TTerminalElement;
begin
  if not HasTerminal then
    Exit;
  { The palette is set first, as setting it puts the other colors back to
    what the library chooses }
  TWSTerminal.SetPalette(Self, FColors.Palette, FColors.Foreground, FColors.Background);
  for E := Low(TTerminalElement) to High(TTerminalElement) do
    TWSTerminal.SetColor(Self, E, FColors.FColors[E]);
  TWSTerminal.SetFont(Self, Font);
  TWSTerminal.SetScrollBar(Self, FScrollBar);
end;

procedure TCustomTerminal.InitializeWnd;
begin
  inherited InitializeWnd;
  UpdateTerminal;
end;

procedure TCustomTerminal.DestroyWnd;
begin
  FreeAndNil(FCanvas);
  inherited DestroyWnd;
end;

procedure TCustomTerminal.SetColors(Value: TTerminalColors);
begin
  FColors.Assign(Value);
end;

procedure TCustomTerminal.SetScrollBar(Value: Boolean);
begin
  if Value = FScrollBar then
    Exit;
  FScrollBar := Value;
  if HasTerminal then
    TWSTerminal.SetScrollBar(Self, FScrollBar);
end;

procedure TCustomTerminal.ColorsChange(Sender: TObject);
begin
  if HasTerminal then
    UpdateTerminal
  else
    Invalidate;
end;

procedure TCustomTerminal.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);
  if HasTerminal then
    TWSTerminal.SetFont(Self, Font)
  else
    Invalidate;
end;

{ A window control does not paint itself unless custom paint is in its
  control state while the paint message is handled }

procedure TCustomTerminal.WMPaint(var Message: TLMPaint);
begin
  if (csDestroying in ComponentState) or (not HandleAllocated) then
    Exit;
  Include(FControlState, csCustomPaint);
  inherited WMPaint(Message);
  Exclude(FControlState, csCustomPaint);
end;

{ The control only paints when it has no terminal, which draws itself. The
  canvas is given the device context being painted for as long as Paint
  runs. }

procedure TCustomTerminal.PaintWindow(DC: HDC);
var
  Changed: Boolean;
begin
  if HasTerminal then
    Exit;
  if FCanvas = nil then
  begin
    FCanvas := TControlCanvas.Create;
    FCanvas.Control := Self;
  end;
  Changed := (not FCanvas.HandleAllocated) or (FCanvas.Handle <> DC);
  if Changed then
    FCanvas.Handle := DC;
  Paint;
  if Changed then
    FCanvas.Handle := 0;
end;

{ Paint draws what a terminal looks like when it starts: a prompt followed by
  a cursor at the top left, in the colors and font of the control, inside a
  dashed outline }

procedure TCustomTerminal.Paint;
const
  Margin = 4;
  Prompt = 'user@linux:~$ ';
var
  W, H: Integer;
begin
  FCanvas.Brush.Style := bsSolid;
  FCanvas.Brush.Color := FColors.Background;
  FCanvas.Pen.Color := FColors.Foreground;
  FCanvas.Pen.Style := psDash;
  FCanvas.Rectangle(ClientRect);
  FCanvas.Pen.Style := psSolid;
  FCanvas.Font.Assign(Font);
  FCanvas.Font.Color := FColors.Foreground;
  W := FCanvas.TextWidth(Prompt);
  H := FCanvas.TextHeight('Wg');
  FCanvas.TextOut(Margin, Margin, Prompt);
  { The cursor is a block one character wide after the prompt }
  FCanvas.Brush.Color := FColors.Cursor;
  FCanvas.FillRect(Rect(Margin + W, Margin, Margin + W + FCanvas.TextWidth('W'),
    Margin + H));
  FCanvas.Brush.Color := FColors.Background;
end;

{ The terminal can send its signals while the control is being destroyed,
  when the events must not be invoked }

procedure TCustomTerminal.TerminalReady;
begin
  if not (csDestroying in ComponentState) then
    DoReady;
end;

procedure TCustomTerminal.TerminalExited;
begin
  if not (csDestroying in ComponentState) then
    DoTerminate;
end;

procedure TCustomTerminal.DoReady;
begin
  if Assigned(FOnReady) then
    FOnReady(Self);
end;

procedure TCustomTerminal.DoTerminate;
begin
  if Assigned(FOnTerminate) then
    FOnTerminate(Self);
end;

procedure TCustomTerminal.Restart;
begin
  if not HasTerminal then
    Exit;
  TWSTerminal.Restart(Self);
  { Restarting can replace the terminal, so its colors and font are set again }
  UpdateTerminal;
end;

end.
