(********************************************************)
(*                                                      *)
(*  Codebot.Cross Pascal Library                        *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.graphics.types.txt> }
unit Codebot.Controls.Colors;

{$i ../codebot/codebot.inc}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, LCLType, LMessages,
  Codebot.System,
  Codebot.Graphics,
  Codebot.Graphics.Types,
  Codebot.Controls,
  Codebot.Controls.Edits;

{ TCustomColorControl is the base class for controls which pick a color
  with the mouse }

type
  TCustomColorControl = class(TSurfaceGraphicControl)
  private
    FBitmap: IBitmap;
    FMousePos: TPointI;
    FTracking: Boolean;
    FOnChange: TNotifyEvent;
    procedure CheckChangeMouse(X, Y: Integer);
    procedure SetTracking(Value: Boolean);
    procedure UserInput(Sender: TObject; var Msg: TLMessage);
  protected
    { Return the selected color }
    function GetColorValue: TColorB; virtual; abstract;
    { Select a color }
    procedure SetColorValue(Value: TColorB); virtual; abstract;
    { Invoke OnChange and repaint }
    procedure Change; virtual;
    { Select the color under the mouse }
    procedure ChangeMouse(X, Y: Integer); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Resize; override;
    { The last mouse position used to pick a color }
    property MousePos: TPointI read FMousePos write FMousePos;
    { The selected color }
    property ColorValue: TColorB read GetColorValue write SetColorValue;
    { OnChange is invoked when the selected color changes }
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    destructor Destroy; override;
  end;

{ THueStyle determines if a hue picker is drawn as a bar or a wheel }

  THueStyle = (hsLinear, hsRadial);

  {doc off}
  TSaturationPicker = class;
  {doc on}

{ THuePicker selects a hue from a bar or color wheel }

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
    procedure Draw; override;
  public
    { Create a new hue picker }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { A saturation picker whose hue is kept in sync with this picker }
    property SaturationPicker: TSaturationPicker read FSaturationPicker write SetSaturationPicker;
    { The selected hue from 0 to 1 }
    property Hue: Float read FHue write SetHue;
    { Draw as a bar or a wheel }
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

{ TSaturationStyle determines how a saturation picker varies its colors }

  TSaturationStyle = (ssSaturate, ssDesaturate);

{ TSaturationPicker selects the saturation and lightness of a hue }

  TSaturationPicker = class(TCustomColorControl)
  private
    FHue: Single;
    FSaturation: Single;
    FLightness: Single;
    FStyle: TSaturationStyle;
    procedure SetHue(Value: Single);
    procedure SetSaturation(Value: Single);
    procedure SetLightness(Value: Single);
    procedure SetStyle(Value: TSaturationStyle);
  protected
    function GetColorValue: TColorB; override;
    procedure SetColorValue(Value: TColorB); override;
    procedure ChangeMouse(X, Y: Integer); override;
    procedure Draw; override;
  public
    { Create a new saturation picker }
    constructor Create(AOwner: TComponent); override;
    { The selected color }
    property ColorValue: TColorB read GetColorValue write SetColorValue;
  published
    { The hue from 0 to 1 }
    property Hue: Single read FHue write SetHue;
    { The selected saturation from 0 to 1 }
    property Saturation: Single read FSaturation write SetSaturation;
    { The selected lightness from 0 to 1 }
    property Lightness: Single read FLightness write SetLightness;
    { How the picker varies its colors }
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


{ TAlphaStyle determines if an alpha picker shows a gradient to pick from
  or only the color at its current alpha }

  TAlphaStyle = (asGradient, asSolid);

{ TAlphaPicker selects the transparency of a color. The color is drawn over a
  checkerboard fading from transparent on the left to opaque on the right.
  See also
  <link Overview.Codebot.Controls.Colors.TAlphaPicker, TAlphaPicker members> }

  TAlphaPicker = class(TCustomColorControl)
  private
    FBorder: Boolean;
    FCheckerSize: Integer;
    FColorAlpha: Float;
    FStyle: TAlphaStyle;
    procedure SetBorder(Value: Boolean);
    procedure SetCheckerSize(Value: Integer);
    procedure SetColorAlpha(Value: Float);
    procedure SetStyle(Value: TAlphaStyle);
    procedure CMColorChanged(var Message: TLMessage); message CM_COLORCHANGED;
  protected
    function GetColorValue: TColorB; override;
    procedure SetColorValue(Value: TColorB); override;
    procedure ChangeMouse(X, Y: Integer); override;
    procedure Draw; override;
  public
    { Create a new alpha picker }
    constructor Create(AOwner: TComponent); override;
    { The color combined with the selected alpha }
    property ColorValue;
  published
    { When true a border is drawn around the picker }
    property Border: Boolean read FBorder write SetBorder default False;
    { The color whose alpha is picked }
    property Color;
    { The size of the checkerboard squares }
    property CheckerSize: Integer read FCheckerSize write SetCheckerSize default 10;
    { The selected alpha from 0 for transparent to 1 for opaque }
    property ColorAlpha: Float read FColorAlpha write SetColorAlpha;
    { Show a gradient to pick from or only the color at its current alpha }
    property Style: TAlphaStyle read FStyle write SetStyle default asGradient;
    property Align;
    property Anchors;
    property BorderSpacing;
    property Constraints;
    property Enabled;
    property ParentShowHint;
    property ShowHint;
    property Visible;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
  end;

{ TAnglePicker is a round dial with a hand which the user drags to pick an
  angle in degrees, where 0 points up and angles increase clockwise
  See also
  <link Overview.Codebot.Controls.Colors.TAnglePicker, TAnglePicker members> }

  TAnglePicker = class(TSurfaceGraphicControl)
  private
    FAngle: Float;
    FStroke: Float;
    FOnChange: TNotifyEvent;
    procedure SetAngle(Value: Float);
    procedure SetStroke(Value: Float);
    procedure AngleFromPoint(X, Y: Integer);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Draw; override;
  public
    { Create a new angle picker }
    constructor Create(AOwner: TComponent); override;
  published
    { The selected angle in degrees from 0 to 360 }
    property Angle: Float read FAngle write SetAngle;
    { The width of the hand }
    property Stroke: Float read FStroke write SetStroke;
    { OnChange is invoked when the angle changes }
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Align;
    property Anchors;
    property BorderSpacing;
    property Constraints;
    property Enabled;
    property ParentShowHint;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
  end;

{ TColorSlideKind is the color component edited by a TColorSlideEdit }

  TColorSlideKind = (cskRed, cskGreen, cskBlue, cskHue,
    cskSaturation, cskLightness, cskAlpha);

{ TColorSlideEdit is a slide edit for one component of a color, with a color
  bar along its bottom which can be dragged to change the value. Position
  ranges from 0 to 255 for every kind. Use the Update methods to keep several
  color slide edits and pickers in sync without invoking OnValueChange.
  See also
  <link Overview.Codebot.Controls.Colors.TColorSlideEdit, TColorSlideEdit members> }

  TColorSlideEdit = class(TCustomSlideEdit)
  private
    FKind: TColorSlideKind;
    FColor: TColorB;
    FTrackPoint: TPointI;
    FTrackValue: Double;
    FTracking: Boolean;
    procedure SlideDrawBackground(Sender: TObject; Surface: ISurface;
      Rect: TRectI; State: TDrawState);
    procedure SetKind(Value: TColorSlideKind);
    function GetBarRect: TRectI;
    procedure Silent(const Value: Double);
  protected
    function GetButtonRect: TRectI; override;
    function GetEditRect: TRectI; override;
    function ExtraHeight: Integer; override;
    procedure DoValueChange; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Draw; override;
  public
    { Create a new color slide edit }
    constructor Create(AOwner: TComponent); override;
    { Set the alpha shown by an alpha slide edit }
    procedure UpdateAlpha(Alpha: Byte);
    { Set the color used to draw the bar, and the position of a red, green, or
      blue slide edit }
    procedure UpdateColor(Color: TColorB);
    { Set the hue used to draw the bar, and the position of a hue slide edit }
    procedure UpdateHue(Hue: Float);
    { Set the position of a saturation or lightness slide edit }
    procedure UpdateHSL(const HSL: THSL);
    { The color used to draw the bar }
    property RefColor: TColorB read FColor;
  published
    { The color component edited }
    property Kind: TColorSlideKind read FKind write SetKind default cskRed;
    property AutoHeight;
    property Position;
    property OnValueChange;
    property Align;
    property Anchors;
    property BorderSpacing;
    property Constraints;
    property Enabled;
    property Font;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    property TabOrder;
    property Visible;
    property OnEnter;
    property OnExit;
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

{ MouseCapture is not reliable with Gtk3, it is always false on a modal form,
  so while the left button is down the mouse is followed using the user input
  of the application. This allows a drag to continue outside of the control. }

procedure TCustomColorControl.SetTracking(Value: Boolean);
begin
  if FTracking = Value then Exit;
  FTracking := Value;
  if FTracking then
    Application.AddOnUserInputHandler(UserInput)
  else
    Application.RemoveOnUserInputHandler(UserInput);
end;

procedure TCustomColorControl.UserInput(Sender: TObject; var Msg: TLMessage);
var
  Mouse: TLMMouse absolute Msg;
  P: TPoint;
begin
  case Msg.Msg of
    LM_LBUTTONUP: SetTracking(False);
    LM_MOUSEMOVE:
      if Mouse.Keys and MK_LBUTTON = 0 then
        SetTracking(False)
      else if Sender is TControl then
      begin
        { The position is relative to the control which received the message }
        P := Point(Mouse.XPos, Mouse.YPos);
        P := ScreenToClient(TControl(Sender).ClientToScreen(P));
        CheckChangeMouse(P.X, P.Y);
      end;
  end;
end;

destructor TCustomColorControl.Destroy;
begin
  SetTracking(False);
  inherited Destroy;
end;

procedure TCustomColorControl.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    SetTracking(True);
    CheckChangeMouse(X, Y);
  end;
end;

procedure TCustomColorControl.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Button = mbLeft then
    SetTracking(False);
  inherited MouseUp(Button, Shift, X, Y);
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

procedure THuePicker.Draw;
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

procedure TSaturationPicker.SetHue(Value: Single);
begin
  Value := Clamp(Value);
  if FHue <> Value then
  begin
    FHue := Value;
    FBitmap := nil;
    Change;
  end;
end;

procedure TSaturationPicker.SetSaturation(Value: Single);
begin
  Value := Clamp(Value);
  if Value <> FSaturation then
  begin
    FSaturation := Value;
    Change;
  end;
end;

procedure TSaturationPicker.SetLightness(Value: Single);
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

procedure TSaturationPicker.Draw;
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


{ Color helpers }

function ColorMix(Fore, Back: TColorB; Percent: Float): TColorB;
begin
  Result := Back.Blend(Fore, Percent);
end;

procedure FillGradient(Surface: ISurface; const Rect: TRectI; A, B: TColorB);
var
  G: ILinearGradientBrush;
begin
  G := NewBrush(TPointF.Create(Rect.Left, 0), TPointF.Create(Rect.Right, 0));
  G.AddStop(A, 0);
  G.AddStop(B, 1);
  Surface.FillRect(G, Rect);
end;

procedure FillChecker(Surface: ISurface; const Rect: TRectI; Size: Integer);
begin
  Surface.FillRect(Brushes.Checker(clSilver, clWhite, 1, Size), Rect);
end;

{ Draw a vertical marker of a black line between two white lines }

procedure DrawMarker(Surface: ISurface; const Rect: TRectI; X: Integer);
var
  R: TRectI;
begin
  if X < Rect.Left + 1 then
    X := Rect.Left + 1
  else if X > Rect.Right - 2 then
    X := Rect.Right - 2;
  R := TRectI.Create(X - 1, Rect.Top, 3, Rect.Height);
  Surface.FillRect(NewBrush(clWhite), R);
  R := TRectI.Create(X, Rect.Top, 1, Rect.Height);
  Surface.FillRect(NewBrush(clBlack), R);
end;

{ TAlphaPicker }

constructor TAlphaPicker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCheckerSize := 10;
  FColorAlpha := 1;
  ParentColor := False;
  Color := clBlack;
  Width := 200;
  Height := 24;
end;

function TAlphaPicker.GetColorValue: TColorB;
begin
  Result := Color;
  Result.Alpha := Round(FColorAlpha * HiByte);
end;

procedure TAlphaPicker.SetColorValue(Value: TColorB);
begin
  ColorAlpha := Value.Alpha / HiByte;
  Value.Alpha := HiByte;
  Color := Value.Color;
end;

procedure TAlphaPicker.ChangeMouse(X, Y: Integer);
begin
  if (Width < 2) or (FStyle = asSolid) then
    Exit;
  ColorAlpha := X / (Width - 1);
end;

procedure TAlphaPicker.Draw;
var
  R: TRectI;
  C: TColorB;
begin
  R := ClientRect;
  FillChecker(Surface, R, FCheckerSize);
  C := Color;
  if FStyle = asGradient then
  begin
    FillGradient(Surface, R, C.Fade(0), C);
    DrawMarker(Surface, R, Round((Width - 1) * FColorAlpha));
  end
  else
    Surface.FillRect(NewBrush(ColorValue), R);
  if FBorder then
    StrokeRectColor(Surface, R, clBlack);
end;

procedure TAlphaPicker.SetBorder(Value: Boolean);
begin
  if FBorder = Value then Exit;
  FBorder := Value;
  Invalidate;
end;

procedure TAlphaPicker.SetCheckerSize(Value: Integer);
begin
  if Value < 2 then
    Value := 2
  else if Value > 20 then
    Value := 20;
  if FCheckerSize = Value then Exit;
  FCheckerSize := Value;
  Invalidate;
end;

procedure TAlphaPicker.SetColorAlpha(Value: Float);
begin
  Value := Clamp(Value);
  if FColorAlpha = Value then Exit;
  FColorAlpha := Value;
  Change;
end;

procedure TAlphaPicker.SetStyle(Value: TAlphaStyle);
begin
  if FStyle = Value then Exit;
  FStyle := Value;
  Invalidate;
end;

procedure TAlphaPicker.CMColorChanged(var Message: TLMessage);
begin
  inherited;
  Change;
end;

{ TAnglePicker }

{ Return the angle in radians of a point from the origin }

function PointAngle(X, Y: Float): Float;
begin
  if X > 0 then
    Result := ArcTan(Y / X)
  else if X < 0 then
    if Y >= 0 then
      Result := ArcTan(Y / X) + Pi
    else
      Result := ArcTan(Y / X) - Pi
  else if Y > 0 then
    Result := Pi / 2
  else if Y < 0 then
    Result := -Pi / 2
  else
    Result := 0;
end;

constructor TAnglePicker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 48;
  Height := 48;
  FStroke := 2;
end;

procedure TAnglePicker.AngleFromPoint(X, Y: Integer);
var
  A: Float;
begin
  { Zero points up and angles increase clockwise on the screen }
  A := RadToDeg(PointAngle(Height / 2 - Y, X - Width / 2));
  if A < 0 then
    A := A + 360;
  Angle := Round(A);
end;

procedure TAnglePicker.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if MouseCapture then
  begin
    AngleFromPoint(X, Y);
    Invalidate;
  end;
end;

procedure TAnglePicker.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if MouseCapture then
    AngleFromPoint(X, Y);
end;

procedure TAnglePicker.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  Invalidate;
end;

procedure TAnglePicker.Draw;
var
  Base, Window: TColorB;
  Size, Radius, S, C: Float;
  Center, Dir: TPointF;
  R: TRectF;
  G: ILinearGradientBrush;
  P: IPen;
begin
  Window := clWindow;
  if not Enabled then
    Base := cl3DDkShadow
  else if MouseCapture then
    Base := ColorMix(clHighlight, Window, 0.75)
  else
    Base := ColorMix(clHighlight, Window, 0.5);
  if Width > Height then
    Size := Height
  else
    Size := Width;
  if Size < 4 then
    Exit;
  Center := TPointF.Create(Width / 2, Height / 2);
  Radius := Size / 2 - 1;
  R := TRectF.Create(Center.X - Radius, Center.Y - Radius, Radius * 2, Radius * 2);
  { The dial is shaded from the back of the hand toward its tip }
  SinCos(0, S, C);
  Dir := TPointF.Create(S, -C);
  G := NewBrush(TPointF.Create(Center.X - Dir.X * Radius, Center.Y - Dir.Y * Radius),
    TPointF.Create(Center.X + Dir.X * Radius, Center.Y + Dir.Y * Radius));
  SinCos(DegToRad(FAngle), S, C);
  Dir := TPointF.Create(S, -C);
  G.AddStop(ColorMix(Base, Window, 0.3), 0);
  G.AddStop(Base, 1);
  Surface.Ellipse(R);
  Surface.Fill(G, True);
  Surface.Stroke(NewPen(Base));
  { Draw the hand with a small ring at the center }
  P := NewPen(ColorMix(Base, clBlack, 0.5), FStroke);
  R := TRectF.Create(Center.X - FStroke * 1.5, Center.Y - FStroke * 1.5,
    FStroke * 3, FStroke * 3);
  Surface.Ellipse(R);
  Surface.Stroke(P);
  Surface.MoveTo(Center.X + Dir.X * FStroke * 1.5, Center.Y + Dir.Y * FStroke * 1.5);
  Surface.LineTo(Center.X + Dir.X * (Size - FStroke) / 2,
    Center.Y + Dir.Y * (Size - FStroke) / 2);
  Surface.Stroke(P);
end;

procedure TAnglePicker.SetAngle(Value: Float);
begin
  if FAngle = Value then Exit;
  FAngle := Value;
  Invalidate;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TAnglePicker.SetStroke(Value: Float);
begin
  if Value < 0 then
    Value := 0
  else if Value > 50 then
    Value := 50;
  if FStroke = Value then Exit;
  FStroke := Value;
  Invalidate;
end;

{ TColorSlideEdit }

const
  ColorBarHeight = 6;

constructor TColorSlideEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColor := clBlack;
  Max := 255;
  Width := 52;
  OnDrawBackground := SlideDrawBackground;
end;

function TColorSlideEdit.GetBarRect: TRectI;
begin
  Result := ClientRect;
  Result.Inflate(-1, -1);
  Result.Top := Result.Bottom - ColorBarHeight;
end;

function TColorSlideEdit.GetButtonRect: TRectI;
begin
  Result := inherited GetButtonRect;
  Result.Height := Result.Height - ColorBarHeight - 1;
end;

function TColorSlideEdit.GetEditRect: TRectI;
begin
  { Leave room for the color bar and a one pixel gap above it }
  Result := inherited GetEditRect;
  Result.Height := Result.Height - ColorBarHeight - 1;
end;

function TColorSlideEdit.ExtraHeight: Integer;
begin
  { Room for the color bar and a one pixel gap above it }
  Result := ColorBarHeight + 1;
end;

procedure TColorSlideEdit.DoValueChange;
begin
  inherited DoValueChange;
  Invalidate;
end;

procedure TColorSlideEdit.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    FTrackPoint := TPointI.Create(X, Y);
    FTrackValue := Position;
    FTracking := GetBarRect.Contains(X, Y);
  end;
end;

procedure TColorSlideEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if FTracking then
    Position := FTrackValue + X - FTrackPoint.X;
end;

procedure TColorSlideEdit.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  FTracking := False;
end;

procedure TColorSlideEdit.Draw;
var
  R: TRectI;
  P: Float;
  HSL: THSL;
begin
  inherited Draw;
  if Max > Min then
    P := (Position - Min) / (Max - Min)
  else
    P := 0;
  R := GetBarRect;
  case FKind of
    cskRed, cskGreen, cskBlue:
      begin
        FillRectColor(Surface, R, clBlack);
        R.Width := Round(R.Width * P);
        case FKind of
          cskRed: FillRectColor(Surface, R, clRed);
          cskGreen: FillRectColor(Surface, R, clLime);
        else
          FillRectColor(Surface, R, clBlue);
        end;
      end;
    cskAlpha:
      begin
        FillChecker(Surface, R, 3);
        FillGradient(Surface, R, FColor.Fade(0), FColor);
      end;
    cskHue: FillRectColor(Surface, R, HueToColor(P));
    cskSaturation:
      begin
        HSL := THSL(FColor);
        FillGradient(Surface, R, THSL.Create(HSL.Hue, 0, 0.5), FColor);
      end;
    cskLightness:
      begin
        R.Width := R.Width div 2;
        FillGradient(Surface, R, clBlack, FColor);
        R.Left := R.Right;
        R.Right := GetBarRect.Right;
        FillGradient(Surface, R, FColor, clWhite);
      end;
  end;
  R := GetBarRect;
  DrawMarker(Surface, R, R.Left + Round((R.Width - 1) * P));
end;

procedure TColorSlideEdit.SlideDrawBackground(Sender: TObject;
  Surface: ISurface; Rect: TRectI; State: TDrawState);
var
  R: TRectI;
  HSL: THSL;
begin
  Rect.Inflate(0, -5);
  R := Rect;
  case FKind of
    cskRed: FillGradient(Surface, R, clBlack, clRed);
    cskGreen: FillGradient(Surface, R, clBlack, clLime);
    cskBlue: FillGradient(Surface, R, clBlack, clBlue);
    cskAlpha:
      begin
        FillChecker(Surface, R, 4);
        FillGradient(Surface, R, FColor.Fade(0), FColor);
      end;
    cskHue: DrawBitmap(Surface, DrawHueLinear(R.Width, R.Height), R.X, R.Y);
    cskSaturation:
      begin
        HSL := THSL(FColor);
        FillGradient(Surface, R, THSL.Create(HSL.Hue, 0, 0.5), FColor);
      end;
    cskLightness:
      begin
        R.Width := Rect.Width div 2;
        FillGradient(Surface, R, clBlack, FColor);
        R.Left := R.Right;
        R.Right := Rect.Right;
        FillGradient(Surface, R, FColor, clWhite);
      end;
  end;
end;

{ Set the position without invoking OnValueChange }

procedure TColorSlideEdit.Silent(const Value: Double);
var
  Event: TNotifyEvent;
begin
  Event := OnValueChange;
  try
    OnValueChange := nil;
    Position := Value;
  finally
    OnValueChange := Event;
  end;
end;

procedure TColorSlideEdit.UpdateAlpha(Alpha: Byte);
begin
  if FKind = cskAlpha then
  begin
    Silent(Alpha);
    Invalidate;
  end;
end;

procedure TColorSlideEdit.UpdateColor(Color: TColorB);
begin
  Color.Alpha := HiByte;
  if Color = FColor then
    Exit;
  FColor := Color;
  case FKind of
    cskRed: Silent(FColor.Red);
    cskGreen: Silent(FColor.Green);
    cskBlue: Silent(FColor.Blue);
  end;
  Invalidate;
end;

procedure TColorSlideEdit.UpdateHue(Hue: Float);
begin
  if FKind in [cskHue, cskSaturation, cskLightness] then
  begin
    FColor := THSL.Create(Hue, 1, 0.5);
    if FKind = cskHue then
      Silent(Round(Hue * Max));
    Invalidate;
  end;
end;

procedure TColorSlideEdit.UpdateHSL(const HSL: THSL);
begin
  case FKind of
    cskSaturation: Silent(Round(HSL.Saturation * Max));
    cskLightness: Silent(Round(HSL.Lightness * Max));
  else
    Exit;
  end;
  Invalidate;
end;

procedure TColorSlideEdit.SetKind(Value: TColorSlideKind);
var
  HSL: THSL;
begin
  if FKind = Value then Exit;
  FKind := Value;
  { Show the component of the reference color for the new kind }
  HSL := THSL(FColor);
  case FKind of
    cskRed: Silent(FColor.Red);
    cskGreen: Silent(FColor.Green);
    cskBlue: Silent(FColor.Blue);
    cskHue: Silent(Round(HSL.Hue * Max));
    cskSaturation: Silent(Round(HSL.Saturation * Max));
    cskLightness: Silent(Round(HSL.Lightness * Max));
  end;
  Invalidate;
end;

end.
