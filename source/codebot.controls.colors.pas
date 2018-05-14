(********************************************************)
(*                                                      *)
(*  Codebot.Cross Pascal Library                        *)
(*  http://cross.codebot.org                            *)
(*  Modified March 2015                                 *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.graphics.types.txt> }
unit Codebot.Controls.Colors;

{$i codebot.inc}

interface

uses
  SysUtils, Classes, Graphics, Controls,
  Codebot.System,
  Codebot.Graphics,
  Codebot.Graphics.Types,
  Codebot.Controls;

  { TCustomColorControl }

type
  TCustomColorControl = class(TRenderGraphicControl)
  private
    FBitmap: IBitmap;
    FMousePos: TPointI;
    FOnChange: TNotifyEvent;
    procedure CheckChangeMouse(X, Y: Integer);
  protected
    function GetColorValue: TColorB; virtual; abstract;
    procedure SetColorValue(Value: TColorB); virtual; abstract;
    procedure Change; virtual;
    procedure ChangeMouse(X, Y: Integer); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Resize; override;
    property MousePos: TPointI read FMousePos write FMousePos;
    property ColorValue: TColorB read GetColorValue write SetColorValue;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

{ THueStyle }

  THueStyle = (hsLinear, hsRadial);

  TSaturationPicker = class;

{ THuePicker }

  THuePicker = class(TCustomColorControl)
  private
    FHue: Float;
    FSaturationPicker: TSaturationPicker;
    FStyle: THueStyle;
    procedure SetSaturationPicker(Value: TSaturationPicker);
    procedure SetHue(Value: Float);
    procedure SetStyle(Value: THueStyle);
  protected
    function GetColorValue: TColorB; override;
    procedure SetColorValue(Value: TColorB); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Change; override;
    procedure ChangeMouse(X, Y: Integer); override;
    procedure Render; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property SaturationPicker: TSaturationPicker read FSaturationPicker write SetSaturationPicker;
    property Hue: Float read FHue write SetHue;
    property Style: THueStyle read FStyle write SetStyle default hsRadial;
    property Align;
    property Anchors;
    property BorderSpacing;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property OnChange;
    property OnChangeBounds;
    property OnDragDrop;
    property OnDragOver;
    property OnDblClick;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPaint;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property ShowHint;
    property Visible;
  end;

  TSaturationStyle = (ssSaturate, ssDesaturate);

  { TSaturationPicker }

  TSaturationPicker = class(TCustomColorControl)
  private
    FHue: Float;
    FSaturation: Float;
    FLightness: Float;
    FStyle: TSaturationStyle;
    procedure SetHue(Value: Float);
    procedure SetSaturation(Value: Float);
    procedure SetLightness(Value: Float);
    procedure SetStyle(Value: TSaturationStyle);
  protected
    function GetColorValue: TColorB; override;
    procedure SetColorValue(Value: TColorB); override;
    procedure ChangeMouse(X, Y: Integer); override;
    procedure Render; override;
  public
    constructor Create(AOwner: TComponent); override;
    property ColorValue: TColorB read GetColorValue write SetColorValue;
  published
    property Hue: Float read FHue write SetHue;
    property Saturation: Float read FSaturation write SetSaturation;
    property Lightness: Float read FLightness write SetLightness;
    property Style: TSaturationStyle read FStyle write SetStyle;
    property Align;
    property Anchors;
    property BorderSpacing;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property OnChange;
    property OnChangeBounds;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPaint;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property ShowHint;
    property Visible;
  end;

{ TAlphaPicker }

  TAlphaPicker = class(TCustomColorControl)
  private
    FAlpha: Float;
    procedure SetAlpha(Value: Float);
  protected
    function GetColorValue: TColorB; override;
    procedure SetColorValue(Value: TColorB); override;
    procedure Render; override;
  public
    constructor Create(AOwner: TComponent); override;
    property Alpha: Float read FAlpha write SetAlpha;
  end;

implementation

{ TCustomColorControl }

procedure TCustomColorControl.Change;
begin
  Invalidate;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TCustomColorControl.ChangeMouse(X, Y: Integer);
begin
end;

procedure TCustomColorControl.CheckChangeMouse(X, Y: Integer);
begin
  if X < 0 then X := 0 else if X > Width - 1 then X := Width - 1;
  if Y < 0 then Y := 0 else if Y > Height - 1 then Y := Height - 1;
  if (FMousePos.X <> X) or (FMousePos.Y <> Y) then
  begin
    FMousePos.X := X;
    FMousePos.Y := Y;
    ChangeMouse(X, Y);
    Invalidate;
  end;
end;

procedure TCustomColorControl.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if MouseCapture then
    CheckChangeMouse(X, Y);
end;

procedure TCustomColorControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if MouseCapture then
    CheckChangeMouse(X, Y);
end;

procedure TCustomColorControl.Resize;
begin
  FBitmap := nil;
  inherited Resize;
end;

{ THuePicker }

constructor THuePicker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 200;
  Height := 200;
  FStyle := hsRadial;
end;

destructor THuePicker.Destroy;
begin
  SaturationPicker := nil;
  inherited Destroy;
end;

procedure THuePicker.ChangeMouse(X, Y: Integer);
var
  W, H, D: Integer;
begin
  if (Width < 1) or (Height < 1) then Exit;
  W := Width;
  H := Height;
  if FStyle = hsLinear then
    Hue := X / W
  else
  begin
    D := W + W + H + H;
    if (Y < X) and (Y < W - X) and (Y < H div 2) then
      Hue := X / D
    else if (W - X < H - Y) and (X > W div 2) then
      Hue := (W + Y) / D
    else if (H - Y <= X) and (H - Y <= W - X) then
      Hue := (W + W + H - X) / D
    else
      Hue := (W + W + H + H - Y) / D;
  end;
end;

function THuePicker.GetColorValue: TColorB;
begin
  Result := HueToColor(FHue);
end;

procedure THuePicker.SetColorValue(Value: TColorB);
begin
  Hue := ColorToHue(Value);
end;

procedure THuePicker.SetSaturationPicker(Value: TSaturationPicker);
begin
  if FSaturationPicker <> Value then
  begin
    if FSaturationPicker <> nil then
      FSaturationPicker.RemoveFreeNotification(Self);
    FSaturationPicker := Value;
    if FSaturationPicker <> nil then
      FSaturationPicker.FreeNotification(Self);
  end;
end;

procedure THuePicker.SetHue(Value: Float);
begin
  Value := Clamp(Value);
  if FHue <> Value then
  begin
    FHue := Value;
    Change;
  end;
end;

procedure THuePicker.SetStyle(Value: THueStyle);
begin
  if Value <>  FStyle then
  begin
    FStyle := Value;
    FBitmap := nil;
    Invalidate;
  end;
end;

procedure THuePicker.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FSaturationPicker) then
    FSaturationPicker := nil;
end;

procedure THuePicker.Change;
begin
  if FSaturationPicker <> nil then
    FSaturationPicker.Hue := Hue;
  inherited Change;
end;

function NewHueBrush(H: Float): IBrush;
begin
  H := H + 0.5;
  if H > 1 then
    H := H - 1;
  Result := NewBrush(Hue(H));
end;

procedure THuePicker.Render;
const
  ArrowSize = 6;
var
  S: ISurface;
  R: TRectI;
  W, H, X: Integer;
begin
  if FBitmap = nil then
    if FStyle = hsLinear then
      FBitmap := DrawHueLinear(Width, Height)
    else
      FBitmap := DrawHueRadial(Width, Height);
  if FBitmap.Empty then
    Exit;
  S := Surface;
  DrawBitmap(S, FBitmap, 0, 0);
  if FStyle = hsLinear then
  begin
    R := ClientRect;
    R.Width := 2;
    R.Offset(Round((FBitmap.Width - 1) * FHue), 0);
    S.FillRect(NewHueBrush(FHue), R);
  end
  else
  begin
    W := FBitmap.Width;
    H := FBitmap.Height;
    X := Round((W * 2 + H * 2) * Hue);
    if X < W then
    begin
      S.MoveTo(X - ArrowSize, 0);
      S.LineTo(X + ArrowSize, 0);
      S.LineTo(X, ArrowSize);
      S.LineTo(X - ArrowSize, 0);
    end
    else if X < W + H then
    begin
      X := X - W;
      S.MoveTo(W, X - ArrowSize);
      S.LineTo(W, X + ArrowSize);
      S.LineTo(W - ArrowSize, X);
      S.LineTo(W, X - ArrowSize);
    end
    else if X < W + W + H then
    begin
      X := X - W - H;
      S.MoveTo(W - X - ArrowSize, H);
      S.LineTo(W - X + ArrowSize, H);
      S.LineTo(W - X, H - ArrowSize);
      S.LineTo(W - X - ArrowSize, H);
    end
    else
    begin
      X := X - W - W - H;
      S.MoveTo(0, H - X - ArrowSize);
      S.LineTo(0, H - X + ArrowSize);
      S.LineTo(ArrowSize, H - X);
      S.LineTo(0, H - X - ArrowSize);
    end;
    S.Fill(NewHueBrush(FHue));
  end;
end;

{ TSaturationPicker }

constructor TSaturationPicker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 200;
  Height := 200;
end;

function ColorHue(H: Float): TColorB;
begin
  Result := Hue(H);
end;

procedure TSaturationPicker.ChangeMouse(X, Y: Integer);
var
  A1: Float;
begin
  if (Width < 1) or (Height < 1) then Exit;
  if FStyle = ssSaturate then
  begin
    if X <= 0 then
      Saturation := 0
    else if X >= Width - 1 then
      Saturation := 1
    else
      Saturation := X / Width;
    if Y <= 0 then
      Lightness := 0
    else if Y >= Height - 1 then
      Lightness := 1
    else
      Lightness := Y / Height;
  end
  else
  begin
    Saturation := X / Width;
    A1 := 1 - Y / Height;
    Lightness := (A1 * (1 - Saturation) + A1) / 2;
  end;
end;

procedure TSaturationPicker.SetHue(Value: Float);
begin
  Value := Clamp(Value);
  if FHue <> Value then
  begin
    FHue := Value;
    FBitmap := nil;
    Change;
  end;
end;

procedure TSaturationPicker.SetSaturation(Value: Float);
begin
  Value := Clamp(Value);
  if Value <> FSaturation then
  begin
    FSaturation := Value;
    Change;
  end;
end;

procedure TSaturationPicker.SetLightness(Value: Float);
begin
  Value := Clamp(Value);
  if Value <> FLightness then
  begin
    FLightness := Value;
    Change;
  end;
end;

procedure TSaturationPicker.SetStyle(Value: TSaturationStyle);
begin
  if Value <> FStyle then
  begin
    FStyle := Value;
    FBitmap := nil;
    Invalidate;
  end;
end;

function TSaturationPicker.GetColorValue: TColorB;
begin
  Result := THSL.Create(Hue, Saturation, Lightness);
end;

procedure TSaturationPicker.SetColorValue(Value: TColorB);
var
  HSL: THSL;
begin
  HSL := THSL(Value);
  if HSL.Hue <> FHue then
  begin
    FHue := HSL.Hue;
    FSaturation := HSL.Saturation;
    FLightness := HSL.Lightness;
    Change;
  end
  else if (HSL.Saturation <> FSaturation) or (HSL.Lightness <> FLightness) then
  begin
    FSaturation := HSL.Saturation;
    FLightness := HSL.Lightness;
    Change;
  end;
end;

function NewHuePen(H: Float): IPen;
begin
  H := H + 0.5;
  if H > 1 then
    H := H - 1;
  Result := NewPen(Hue(H), 3);
end;

procedure TSaturationPicker.Render;
const
  CircleSize = 6;
var
  X, Y: Integer;
  S: ISurface;
  R: TRectI;
begin
  if FBitmap = nil then
    if FStyle = ssSaturate then
      FBitmap := DrawSaturationBox(Width, Height, FHue)
    else
      FBitmap := DrawDesaturationBox(Width, Height, FHue);
  if FBitmap.Empty then
    Exit;
  S := Surface;
  DrawBitmap(S, FBitmap, 0, 0);
  if FStyle = ssSaturate then
  begin
    X := Round(Width * Saturation);
    Y := Round(Height * Lightness);
  end
  else
  begin
    X := MousePos.X;
    Y := MousePos.Y;
  end;
  R := TRectI.Create(CircleSize, CircleSize);
  R.Center(X, Y);
  S.Ellipse(R);
  S.Stroke(NewPen(ColorValue.Invert, 3));
end;

{ TAlphaPicker }

constructor TAlphaPicker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 200;
  Height := 24;
end;

procedure TAlphaPicker.SetAlpha(Value: Float);
begin
  if FAlpha = Value then Exit;
  FAlpha := Value;
end;

function TAlphaPicker.GetColorValue: TColorB;
begin
  Result := clTransparent;
end;

procedure TAlphaPicker.SetColorValue(Value: TColorB);
begin

end;

procedure TAlphaPicker.Render;
var
  //X, Y: Integer;
  S: ISurface;
  //R: TRectI;
begin
  if FBitmap = nil then
      FBitmap := NewBitmap(Width, Height)
    else
      FBitmap.SetSize(Width, Height);
  if FBitmap.Empty then
    Exit;
  S := Surface;
  DrawBitmap(S, FBitmap, 0, 0);
  S.Rectangle(ClientRect);//Brushes.Checker(clWhite, clBlack, 0, 10));
  S.Fill(Brushes.Checker(clWhite, clSilver, 0, 8));
  {if FStyle = ssSaturate then
  begin
    X := Round(Width * Saturation);
    Y := Round(Height * Lightness);
  end
  else
  begin
    X := MousePos.X;
    Y := MousePos.Y;
  end;
  R := TRectI.Create(CircleSize, CircleSize);
  R.Center(X, Y);
  S.Ellipse(R);
  S.Stroke(NewPen(ColorValue.Invert, 3));}
end;


end.
