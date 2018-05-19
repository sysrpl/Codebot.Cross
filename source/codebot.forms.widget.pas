(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified November 2015                              *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.forms.widget.txt> }
unit Codebot.Forms.Widget;

{$i codebot.inc}

interface

{$if defined(linuxgtk)} // or defined(windows)}
uses
  Classes, SysUtils, Graphics, Controls, Forms, ExtCtrls,
  Codebot.System,
  Codebot.Graphics,
  Codebot.Graphics.Types,
  Codebot.Forms.Floating,
  Codebot.Animation;

{ TWidget }

type
  TEdgeSize = (esNW, esNE, esSE, esSW);
  TEdgeSizable = set of TEdgeSize;

  TClickBoxEvent = procedure(Sender: TObject; BoxIndex: Integer) of object;

  TWidget = class(TFloatingForm)
  private
    FAspectRatio: Float;
    FDraggable: Boolean;
    FEdgeSizable: TEdgeSizable;
    FSaveCursor: TCursor;
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
    FGripSize: Integer;
    FGripOpacity: Float;
    FClickBoxes: TArrayList<TRectI>;
    FBoxIndex: Integer;
    FOnClickBox: TClickBoxEvent;
    procedure DoTimer(Sender: TObject);
    function GetAnimated: Boolean;
    procedure SetAnimated(Value: Boolean);
    procedure SetAspectRatio(Value: Float);
    procedure SetGripSize(Value: Integer);
    procedure SetHotQuad(Value: Integer);
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
    procedure Paint; override;
    procedure BeforeRender; virtual;
    procedure Render; virtual;
    procedure AfterRender; virtual;
    procedure ClickBox(Index: Integer); virtual;
    property HotQuad: Integer read FHotQuad write SetHotQuad;
    property OnClickBox: TClickBoxEvent read FOnClickBox write FOnClickBox;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClickBoxes(Boxes: TArrayList<TRectI>);
    procedure Center;
    property Animated: Boolean read GetAnimated write SetAnimated;
    property EdgeSizable: TEdgeSizable read FEdgeSizable write FEdgeSizable;
    property Surface: ISurface read FSurface;
    property Dragged: Boolean read FDragged;
    property Sized: Boolean read FSized;
    property AspectRatio: Float read FAspectRatio write SetAspectRatio;
    property Draggable: Boolean read FDraggable write FDraggable default True;
    property MinWidth: Integer read FMinWidth write SetMinWidth default 128;
    property MinHeight: Integer read FMinHeight write SetMinHeight default 128;
    property MaxWidth: Integer read FMaxWidth write SetMaxWidth default 2000;
    property MaxHeight: Integer read FMaxHeight write SetMaxHeight default 2000;
    property GripSize: Integer read FGripSize write SetGripSize default 24;
    property OnTick: TNotifyEvent read FOnTick write FOnTick;
  end;
{$endif}

implementation

{$if defined(linuxgtk)} // or defined(windows)}
constructor TWidget.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 400;
  Height := 300;
  Center;
  FEdgeSizable := [esNW, esNE, esSE, esSW];
  FDraggable := True;
  FMinWidth := 128;
  FMinHeight := 128;
  FMaxWidth := 2000;
  FMaxHeight := 2000;
  FGripSize := 24;
  FHotQuad := -1;
  FBoxIndex := -1;
  FSizeQuad := -1;
  FTimer := TTimer.Create(nil);
  FTimer.Interval := 30;
  FTimer.OnTimer := DoTimer;
end;

destructor TWidget.Destroy;
begin
  FTimer.Enabled := False;
  FTimer.Free;
  inherited Destroy;
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
var
  R: TRectI;
  P: TPointI;
begin
  Animator.Step;
  if Animator.Animated then
    Invalidate
  else if (FHotQuad > -1) and (not FSized) then
  begin
    R := BoundsRect;
    P := Mouse.CursorPos;
    if not R.Contains(P) then
      SetHotQuad(-1);
  end;
  if Assigned(OnTick) then
    FOnTick(Self);
end;

function TWidget.GetAnimated: Boolean;
begin
  Result := FTimer.Enabled
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

procedure TWidget.SetGripSize(Value: Integer);
begin
  if FGripSize = Value then Exit;
  FGripSize := Value;
  Invalidate;
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
      Cursor := FSaveCursor;
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
  FSaveCursor := Cursor;
  if Button = mbLeft then
    for I := 0 to FClickBoxes.Length - 1 do
      if FClickBoxes[I].Contains(X, Y) then
      begin
        FBoxIndex := I;
        Exit;
      end;
  FDragged := (Button = mbLeft) and FDraggable;
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
    Cursor := FSaveCursor;
  end;
end;

procedure TWidget.ClickBox(Index: Integer);
begin
  if Assigned(FOnClickBox) then
    FOnClickBox(Self, Index);
end;

procedure TWidget.Paint;
begin
  FSurface := NewSurface(Canvas);
  BeforeRender;
  Render;
  AfterRender;
  FSurface := nil;
end;

procedure TWidget.BeforeRender;
begin
  if Compositing then
    Surface.Clear(clTransparent)
  else
    Surface.Clear(clWhite);
end;

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
    if FSized then
      Color := Blend(clHighlight, clBlack, 0.25)
    else
      Color := Blend(clHighlight, clWhite, 0.1);
    Surface.StrokeRect(NewPen(Color.Fade(Alpha)), ClientRect);
    Surface.Ellipse(GetSizeRect(0));
    Surface.Ellipse(GetSizeRect(1));
    Surface.Ellipse(GetSizeRect(2));
    Surface.Ellipse(GetSizeRect(3));
    Surface.Fill(NewBrush(Color.Fade(0.75 * Alpha)));
  end;
end;
{$endif}

end.

