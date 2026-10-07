(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified November 2015                              *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.forms.widget.txt> }
unit Codebot.Forms.Widget;

{$mode delphi}

interface

uses
  Classes, SysUtils, Graphics, Controls, Forms, ExtCtrls,
  Codebot.System,
  Codebot.Graphics,
  Codebot.Graphics.Types,
  Codebot.Forms.Floating,
  Codebot.Animation;

type
  { TEdgeSize identifies a corner of a widget which can be dragged to resize it }
  TEdgeSize = (esNW, esNE, esSE, esSW);
  { The set of corners which can be dragged to resize a widget }
  TEdgeSizable = set of TEdgeSize;

  { TClickBoxEvent is invoked when a click box of a widget is clicked }
  TClickBoxEvent = procedure(Sender: TObject; BoxIndex: Integer) of object;

{ TWidget is a floating form drawn with a surface which the user can drag
  to move and resize by its corners. Override Render to draw it. }

  TWidget = class(TFloatingForm)
  private
    FAspectRatio: Float;
    FEdgeSizable: TEdgeSizable;
    FHotQuad: Integer;
    FMaxHeight: Integer;
    FMaxWidth: Integer;
    FMinHeight: Integer;
    FMinWidth: Integer;
    FOnTick: TNotifyEvent;
    FSurface: ISurface;
    FDragged: Boolean;
    FSized: Boolean;
    FDragPoint: TPointI;
    FSizeQuad: Integer;
    FSizeBounds: TRectI;
    FTimer: TTimer;
    FGripOpacity: Float;
    FMoused: Boolean;
    FMouseOpacity: Float;
    FClickBoxes: TArrayList<TRectI>;
    FBoxIndex: Integer;
    FOnClickBox: TClickBoxEvent;
    {$ifdef windows}
    { The bitmap the widget is drawn into before it is shown }
    FLayer: IBitmap;
    { True while a redraw of the layer is queued }
    FLayerPending: Boolean;
    procedure RenderLayer;
    procedure UpdateLayer;
    procedure LayerUpdateAsync(Data: PtrInt);
    {$endif}
    procedure DoTimer(Sender: TObject);
    function GetAnimated: Boolean;
    procedure SetAnimated(Value: Boolean);
    procedure SetAspectRatio(Value: Float);
    procedure SetHotQuad(Value: Integer);
    procedure SetMoused(Value: Boolean);
    function MouseOutside: Boolean;
    procedure SetMaxHeight(Value: Integer);
    procedure SetMaxWidth(Value: Integer);
    procedure SetMinHeight(Value: Integer);
    procedure SetMinWidth(Value: Integer);
    function GetSizeRect(Quadrant: Integer): TRectI;
  protected
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure Paint; override;
    {doc off}
    function PerPixelAlpha: Boolean; override;
    {$ifdef windows}
    procedure Resize; override;
    procedure DoShow; override;
    {$endif}
    {doc on}
    { Called before Render }
    procedure BeforeRender; virtual;
    { Draw the widget on Surface }
    procedure Render; virtual;
    { Called after Render, by default drawing the resize grips }
    procedure AfterRender; virtual;
    { Called when a click box is clicked, invoking OnClickBox }
    procedure ClickBox(Index: Integer); virtual;
    { Color of the resize grips and outline, Sizing is true during a resize }
    function GripColor(Sizing: Boolean): TColorB; virtual;
    { The corner under the mouse or -1 if there is none }
    property HotQuad: Integer read FHotQuad write SetHotQuad;
    { OnClickBox is invoked when a click box is clicked }
    property OnClickBox: TClickBoxEvent read FOnClickBox write FOnClickBox;
  public
    { Create a new widget }
    constructor Create(AOwner: TComponent); override;
    {$ifdef windows}
    {doc off}
    destructor Destroy; override;
    procedure Invalidate; override;
    {doc on}
    {$endif}
    { Set the rectangles which respond to clicks instead of starting a drag }
    procedure ClickBoxes(Boxes: TArrayList<TRectI>);
    { Move the widget to the center of the screen }
    procedure Center;
    { When true a timer steps the Animator, repaints the widget while it is
      animating, and invokes OnTick }
    property Animated: Boolean read GetAnimated write SetAnimated;
    { The corners which can be dragged to resize the widget }
    property EdgeSizable: TEdgeSizable read FEdgeSizable write FEdgeSizable;
    { The surface used to draw the widget during Render }
    property Surface: ISurface read FSurface;
    { Dragged is true while the user is moving the widget }
    property Dragged: Boolean read FDragged;
    { Sized is true while the user is resizing the widget }
    property Sized: Boolean read FSized;
    { Moused is true while the mouse is over the widget }
    property Moused: Boolean read FMoused;
    { MouseOpacity fades from 0 to 1 when the mouse moves over the widget and
      back to 0 when it leaves. Use it to draw a hover effect in Render. }
    property MouseOpacity: Float read FMouseOpacity;
    { When greater than zero resizing keeps this width to height ratio }
    property AspectRatio: Float read FAspectRatio write SetAspectRatio;
    { Size limits used while resizing }
    property MinWidth: Integer read FMinWidth write SetMinWidth default 128;
    property MinHeight: Integer read FMinHeight write SetMinHeight default 128;
    property MaxWidth: Integer read FMaxWidth write SetMaxWidth default 2000;
    property MaxHeight: Integer read FMaxHeight write SetMaxHeight default 2000;
    { OnTick is invoked by the animation timer }
    property OnTick: TNotifyEvent read FOnTick write FOnTick;
  end;

implementation

{$ifdef windows}
uses
  Windows,
  Codebot.Graphics.Windows.InterfacedBitmap;

type
  { Gives access to the device context of a bitmap }
  TInterfacedBitmapAccess = class(TInterfacedBitmap);

  TLayerBlend = record
    BlendOp: Byte;
    BlendFlags: Byte;
    SourceConstantAlpha: Byte;
    AlphaFormat: Byte;
  end;

const
  LayerSrcOver = $00;
  LayerSrcAlpha = $01;
  LayerUpdateAlpha = $02;

function LayerUpdate(Wnd: HWND; DstDC: HDC; DstPoint: PPoint; Size: PSize;
  SrcDC: HDC; SrcPoint: PPoint; Key: COLORREF; var Blend: TLayerBlend;
  Flags: DWORD): BOOL; stdcall; external 'user32.dll' name 'UpdateLayeredWindow';
{$endif}

const
  GripSize = 24;

constructor TWidget.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 400;
  Height := 300;
  Center;
  FEdgeSizable := [esNW, esNE, esSE, esSW];
  FMinWidth := 128;
  FMinHeight := 128;
  FMaxWidth := 2000;
  FMaxHeight := 2000;
  FHotQuad := -1;
  FBoxIndex := -1;
  FSizeQuad := -1;
  FTimer := TTimer.Create(Self);
  FTimer.Interval := 30;
  FTimer.OnTimer := DoTimer;
end;

procedure TWidget.ClickBoxes(Boxes: TArrayList<TRectI>);
var
  R: TRectI;
begin
  FClickBoxes.Clear;
  for R in Boxes do
    FClickBoxes.Push(R);
end;

procedure TWidget.Center;
begin
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
end;

function Between(Value, A, B: Float): Boolean;
begin
  Result := (Value > A) and (Value < B)
end;

procedure TWidget.DoTimer(Sender: TObject);
begin
  Animator.Step;
  if Animator.Animated then
    Invalidate;
  { The mouse events track the mouse, but if a mouse leave was missed this
    returns the widget to its normal state once the mouse is outside }
  if (not FDragged) and (FMoused or (FHotQuad > -1)) and MouseOutside then
  begin
    SetHotQuad(-1);
    SetMoused(False);
  end;
  if Assigned(OnTick) then
    FOnTick(Self);
end;

function TWidget.MouseOutside: Boolean;
var
  R: TRectI;
begin
  R := BoundsRect;
  Result := not R.Contains(TPointI(Mouse.CursorPos));
end;

procedure TWidget.SetMoused(Value: Boolean);
begin
  if FMoused = Value then
    Exit;
  FMoused := Value;
  if FMoused then
    Animator.Animate(FMouseOpacity, 1)
  else
    Animator.Animate(FMouseOpacity, 0);
  Invalidate;
end;

procedure TWidget.MouseEnter;
begin
  inherited MouseEnter;
  SetMoused(True);
end;

procedure TWidget.MouseLeave;
begin
  inherited MouseLeave;
  { Moving onto a child control, such as a button, also sends a mouse leave,
    so only leave when the mouse is outside the widget. While dragging the
    mouse is captured and the widget follows it. }
  if FDragged or (not MouseOutside) then
    Exit;
  SetHotQuad(-1);
  SetMoused(False);
end;

function TWidget.GetAnimated: Boolean;
begin
  Result := FTimer.Enabled;
end;

procedure TWidget.SetAnimated(Value: Boolean);
begin
  FTimer.Enabled := Value;
end;

procedure TWidget.SetAspectRatio(Value: Float);
begin
  if FAspectRatio = Value then Exit;
  FAspectRatio := Value;
end;

procedure TWidget.SetHotQuad(Value: Integer);
begin
  if Value < 0 then
    Value := -1
  else if Value > 3 then
    Value := 3;
  if FHotQuad <> Value then
  begin
    FHotQuad := Value;
    case FHotQuad of
      0: Cursor := crSizeNW;
      1: Cursor := crSizeNE;
      2: Cursor := crSizeSE;
      3: Cursor := crSizeSW;
    else
      Cursor := crDefault;
    end;
    if FHotQuad > -1 then
      Animator.Animate(FGripOpacity, 1)
    else
      Animator.Animate(FGripOpacity, 0);
  end
end;

procedure TWidget.SetMaxHeight(Value: Integer);
begin
  if FMaxHeight = Value then Exit;
  FMaxHeight := Value;
end;

procedure TWidget.SetMaxWidth(Value: Integer);
begin
  if FMaxWidth = Value then Exit;
  FMaxWidth := Value;
end;

procedure TWidget.SetMinHeight(Value: Integer);
begin
  if FMinHeight = Value then Exit;
  FMinHeight := Value;
end;

procedure TWidget.SetMinWidth(Value: Integer);
begin
  if FMinWidth = Value then Exit;
  FMinWidth := Value;
end;

procedure TWidget.Loaded;
begin
  inherited Loaded;
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
end;

function TWidget.GetSizeRect(Quadrant: Integer): TRectI;
var
  Client: TRectI;
begin
  Result := TRectI.Create;
  case Quadrant of
    0: if FEdgeSizable * [esNW] = [] then Exit;
    1: if FEdgeSizable * [esNE] = [] then Exit;
    2: if FEdgeSizable * [esSE] = [] then Exit;
    3: if FEdgeSizable * [esSW] = [] then Exit;
  end;
  Client := ClientRect;
  Result := TRectI.Create(GripSize * 2, GripSize * 2);
  case Quadrant of
    0: Result.Center(Client.TopLeft);
    1: Result.Center(Client.TopRight);
    2: Result.Center(Client.BottomRight);
    3: Result.Center(Client.BottomLeft);
  end;
end;

procedure TWidget.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);
  SetMoused(True);
  if Button = mbLeft then
    for I := 0 to FClickBoxes.Length - 1 do
      if FClickBoxes[I].Contains(X, Y) then
      begin
        FBoxIndex := I;
        Exit;
      end;
  FDragged := Button = mbLeft;
  if FDragged then
  begin
    FDragPoint.X := X;
    FDragPoint.Y := Y;
    FSizeQuad := -1;
    for I := 0 to 3 do
    if GetSizeRect(I).Contains(FDragPoint) then
    begin
      FSizeQuad := I;
      Break;
    end;
    FSized := FSizeQuad > -1;
    FSizeBounds := BoundsRect;
    if FSizeQuad > -1 then
      Invalidate;
    if FSized then
      Cursor := crNone;
  end;
end;

function Max(A, B: Integer): Integer;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

procedure TWidget.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TPointI;
  R: TRectI;
  X1, Y1: Integer;
begin
  inherited MouseMove(Shift, X, Y);
  SetMoused(True);
  if FDragged then
  begin
    P := Mouse.CursorPos;
    R := FSizeBounds;
    case FSizeQuad of
      0:
        begin
          R.Left := P.X - FDragPoint.X;
          R.Top := P.Y - FDragPoint.Y;
          if R.Width < MinWidth then
            R.Left := R.Right - MinWidth;
          if R.Height < MinHeight then
            R.Top := R.Bottom - MinHeight;
          if FAspectRatio > 0 then
            if R.Width / R.Height <> FAspectRatio then
            begin
              X1 := Max(R.Width, R.Height);
              Y1 := Round(X1 * AspectRatio);
              R.Left := R.Right - X1;
              R.Top := R.Bottom - Y1;
            end;
          MoveSize(R);
        end;
      1:
        begin
          R.Right := R.Right + X - FDragPoint.X;
          R.Top := P.Y - FDragPoint.Y;
          if R.Width < MinWidth then
            R.Width := MinWidth;
          if R.Height < MinHeight then
            R.Top := R.Bottom - MinHeight;
          if FAspectRatio > 0 then
            if R.Width / R.Height <> FAspectRatio then
            begin
              X1 := Max(R.Width, R.Height);
              Y1 := Round(X1 * AspectRatio);
              R.Right := R.Left + X1;
              R.Top := R.Bottom - Y1;
            end;
          MoveSize(R);
        end;
      2:
        begin
          R.Right := R.Right + X - FDragPoint.X;
          R.Bottom := R.Bottom + Y - FDragPoint.Y;
          if R.Width < MinWidth then
            R.Width := MinWidth;
          if R.Height < MinHeight then
            R.Height := MinHeight;
          if FAspectRatio > 0 then
            if R.Width / R.Height <> FAspectRatio then
            begin
              X1 := Max(R.Width, R.Height);
              Y1 := Round(X1 * AspectRatio);
              R.Right := R.Left + X1;
              R.Bottom := R.Top + Y1;
            end;
          MoveSize(R);
        end;
      3:
        begin
          R.Left := P.X - FDragPoint.X;
          R.Bottom := R.Bottom + Y - FDragPoint.Y;
          if R.Width < MinWidth then
            R.Left := R.Right - MinWidth;
          if R.Height < MinHeight then
            R.Height := MinHeight;
          if FAspectRatio > 0 then
            if R.Width / R.Height <> FAspectRatio then
            begin
              X1 := Max(R.Width, R.Height);
              Y1 := Round(X1 * AspectRatio);
              R.Left := R.Right - X1;
              R.Bottom := R.Top + Y1;
            end;
          MoveSize(R);
        end;
    else
      Left := P.X - FDragPoint.X;
      Top := P.Y - FDragPoint.Y;
    end;
  end
  else if GetSizeRect(0).Contains(X, Y) then
    SetHotQuad(0)
  else if GetSizeRect(1).Contains(X, Y) then
    SetHotQuad(1)
  else if GetSizeRect(2).Contains(X, Y) then
    SetHotQuad(2)
  else if GetSizeRect(3).Contains(X, Y) then
    SetHotQuad(3)
  else
    SetHotQuad(-1);
end;

procedure TWidget.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  R: TRectI;
  I: Integer;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    FDragged := False;
    if (FBoxIndex > -1) and (FBoxIndex < FClickBoxes.Length) then
      if FClickBoxes[FBoxIndex].Contains(X, Y) then
      begin
        I := FBoxIndex;
        FBoxIndex := -1;
        ClickBox(I);
      end;
    FBoxIndex := -1;
    if FSizeQuad > -1 then
    begin
      Invalidate;
      R := BoundsRect;
      R.Inflate(-8, -8);
      case FSizeQuad of
        0: Mouse.CursorPos := R.TopLeft;
        1: Mouse.CursorPos := R.TopRight;
        2: Mouse.CursorPos := R.BottomRight;
        3: Mouse.CursorPos := R.BottomLeft;
      end;
      FSizeQuad := -1;
      FSized := False;
    end;
    Cursor := crDefault;
  end;
end;

procedure TWidget.ClickBox(Index: Integer);
begin
  if Assigned(FOnClickBox) then
    FOnClickBox(Self, Index);
end;

function TWidget.GripColor(Sizing: Boolean): TColorB;
begin
  if Sizing then
    Result := Blend(clHighlight, clBlack, 0.25)
  else
    Result := Blend(clHighlight, clWhite, 0.1);
end;

{$ifdef windows}
{ On Windows a window drawn in Paint cannot have transparent pixels. Instead
  the widget is drawn into a bitmap with an alpha channel, which is shown
  using UpdateLayeredWindow.

  Windows does not reliably send WM_PAINT to a window shown this way, so
  Invalidate queues a redraw of the layer instead of relying on Paint. Several
  invalidations before the redraw runs result in a single redraw. }

destructor TWidget.Destroy;
begin
  Application.RemoveAsyncCalls(Self);
  inherited Destroy;
end;

procedure TWidget.Invalidate;
begin
  inherited Invalidate;
  if FLayerPending or (csDestroying in ComponentState) then
    Exit;
  FLayerPending := True;
  Application.QueueAsyncCall(LayerUpdateAsync, 0);
end;

procedure TWidget.LayerUpdateAsync(Data: PtrInt);
begin
  FLayerPending := False;
  if HandleAllocated and Visible and not (csDestroying in ComponentState) then
    RenderLayer;
end;

procedure TWidget.Resize;
begin
  inherited Resize;
  Invalidate;
end;

procedure TWidget.DoShow;
begin
  inherited DoShow;
  Invalidate;
end;

procedure TWidget.Paint;
begin
  inherited Paint;
  RenderLayer;
end;

procedure TWidget.RenderLayer;
begin
  if (Width < 1) or (Height < 1) then
    Exit;
  if FLayer = nil then
    FLayer := NewBitmap(Width, Height)
  else
    FLayer.SetSize(Width, Height);
  FSurface := FLayer.Surface;
  try
    BeforeRender;
    Render;
    AfterRender;
  finally
    FSurface := nil;
  end;
  UpdateLayer;
end;

procedure TWidget.UpdateLayer;
var
  DC, ScreenDC: HDC;
  Size: TSize;
  Origin: TPoint;
  Blend: TLayerBlend;
begin
  { Reading the pixels finishes any drawing pending on the bitmap }
  if FLayer.Pixels = nil then
    Exit;
  DC := TInterfacedBitmapAccess(FLayer as TInterfacedBitmap).FBitmap.DC;
  { Child controls such as buttons are not drawn on a layered window, so draw
    them into the bitmap }
  PaintControls(DC, nil);
  Size.cx := FLayer.Width;
  Size.cy := FLayer.Height;
  Origin.X := 0;
  Origin.Y := 0;
  Blend.BlendOp := LayerSrcOver;
  Blend.BlendFlags := 0;
  Blend.SourceConstantAlpha := Opacity;
  Blend.AlphaFormat := LayerSrcAlpha;
  ScreenDC := GetDC(0);
  try
    { A nil destination point keeps the window where it is }
    LayerUpdate(Handle, ScreenDC, nil, @Size, DC, @Origin, 0, Blend,
      LayerUpdateAlpha);
  finally
    ReleaseDC(0, ScreenDC);
  end;
end;

function TWidget.PerPixelAlpha: Boolean;
begin
  Result := True;
end;

procedure TWidget.BeforeRender;
begin
  { Fully transparent pixels of a layered window do not receive the mouse, so
    clear to an almost transparent color. This keeps the whole widget,
    including the resize grips in its corners, available for dragging. }
  Surface.Clear(Rgba(clBlack, 1 / 255));
end;
{$else}
procedure TWidget.Paint;
begin
  inherited Paint;
  FSurface := NewSurface(Canvas);
  BeforeRender;
  Render;
  AfterRender;
  FSurface := nil;
end;

function TWidget.PerPixelAlpha: Boolean;
begin
  Result := inherited PerPixelAlpha;
end;

procedure TWidget.BeforeRender;
begin
  if Compositing then
    Surface.Clear(clTransparent)
  else
    Surface.Clear(clWhite);
end;
{$endif}

procedure TWidget.Render;
begin

end;

procedure TWidget.AfterRender;
var
  Alpha: Float;
  Color: TColorB;
begin
  if Sized then
    Alpha := 1
  else
    Alpha := FGripOpacity;
  if Alpha > 0 then
  begin
    Color := GripColor(FSized);
    Surface.StrokeRect(NewPen(Color.Fade(Alpha)), ClientRect);
    Surface.Ellipse(GetSizeRect(0));
    Surface.Ellipse(GetSizeRect(1));
    Surface.Ellipse(GetSizeRect(2));
    Surface.Ellipse(GetSizeRect(3));
    Surface.Fill(NewBrush(Color.Fade(0.75 * Alpha)));
  end;
end;

end.

