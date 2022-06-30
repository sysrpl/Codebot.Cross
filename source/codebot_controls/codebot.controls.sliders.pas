(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified March 2015                                 *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.controls.sliders.txt> }
unit Codebot.Controls.Sliders;

{$i codebot.inc}

interface

uses
  SysUtils, Classes, Graphics, Controls,
  LCLIntf, LCLType,
  Codebot.System,
  Codebot.Graphics,
  Codebot.Graphics.Types,
  Codebot.Controls;

type
  TSlideBarKind = (sbVertical, sbHorizontal);
  TFormatTextEvent = procedure(Sender: TObject; var Text: string) of object;

  { TCustomSlideBar }

  TCustomSlideBar = class(TRenderGraphicControl)
  private
    FChanged: Boolean;
    FKind: TSlideBarKind;
    FMin: Double;
    FMax: Double;
    FStep: Double;
    FTracking: Boolean;
    FPosition: Double;
    FAssociate: TControl;
    FMoving: Boolean;
    FOnChange: TNotifyEvent;
    FOnDrawBackground: TDrawStateEvent;
    FOnDrawThumb: TDrawStateEvent;
    FOnFormat: TFormatTextEvent;
    procedure SetAssociate(Value: TControl);
    procedure SetMax(const Value: Double);
    procedure SetMin(const Value: Double);
    procedure SetPosition(Value: Double);
    procedure SetKind(Value: TSlideBarKind);
  protected
    function GetGripRect: TRectI;
    function ThemeAware: Boolean; override;
    procedure Change; dynamic;
    procedure DoChange; dynamic;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure EnabledChanged; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseLeave; override;
    procedure Render; override;
    property Associate: TControl read FAssociate write SetAssociate;
    property Kind: TSlideBarKind read FKind write SetKind;
    property Min: Double read FMin write SetMin;
    property Max: Double read FMax write SetMax;
    property Step: Double read FStep write FStep;
    property Moving: Boolean read FMoving;
    property Tracking: Boolean read Ftracking write FTracking default True;
    property Position: Double read FPosition write SetPosition;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDrawBackground: TDrawStateEvent read FOnDrawBackground write FOnDrawBackground;
    property OnDrawThumb: TDrawStateEvent read FOnDrawThumb write FOnDrawThumb;
    property OnFormat: TFormatTextEvent read FOnFormat write FOnFormat;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TSlideBar = class(TCustomSlideBar)
  public
    property Moving;
  published
    property Align;
    property Anchors;
    property Associate;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Hint;
    property Kind;
    property Max;
    property Min;
    property ParentShowHint;
    property PopupMenu;
    property Position;
    property ShowHint;
    property Step;
    property ThemeName;
    property Tracking;
    property Visible;
    property OnClick;
    property OnChange;
    property OnChangeBounds;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawBackground;
    property OnDrawThumb;
    property OnShowHint;
    property OnEndDock;
    property OnEndDrag;
    property OnFormat;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

{ TCustomSlideBar }

constructor TCustomSlideBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csClickEvents, csReplicatable];
  Height := 100;
  Width := 50;
  FMax := 100;
  FMin := 0;
  FStep := 1;
  FTracking := True;
  if not Enabled then
    FDrawState := [dsDisabled];
end;

destructor TCustomSlideBar.Destroy;
begin
  FOnChange := nil;
  FOnDrawBackground := nil;
  FOnDrawThumb := nil;
  FOnFormat := nil;
  Associate := nil;
  inherited Destroy;
end;

function TCustomSlideBar.GetGripRect: TRectI;
var
  P: TPointI;
  I: Integer;
begin
  if FKind = sbVertical then
  begin
    P := Theme.MeasureThumbThin(toVertical);
    Result := TRectI.Create(P);
    if FMin = FMax then
      I := 0
    else
      I := Round((FMax - FMin - FPosition) / (FMax - FMin) * (Height - P.Y));
    Result.Offset(Width div 2 - P.X div 2, I);
  end
  else
  begin
    P := Theme.MeasureThumbThin(toHorizontal);
    Result := TRectI.Create(P);
    if FMin = FMax then
      I := 0
    else
      I := Round((FPosition - FMin) / (FMax - FMin) * (Width - P.X));
    Result.Offset(I, Height div 2 - P.Y div 2);
  end;
end;

function TCustomSlideBar.ThemeAware: Boolean;
begin
  Result := True;
end;

procedure TCustomSlideBar.Change;
begin
  FChanged := True;
  if FTracking then
    DoChange;
end;

procedure TCustomSlideBar.DoChange;
begin
  if FChanged then
    if Assigned(FOnChange) then FOnChange(Self);
  FChanged := False;
end;

procedure TCustomSlideBar.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FAssociate = AComponent) then
    Associate := nil;
end;

procedure TCustomSlideBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Range, Delta: Single;
begin
  { Work around a gtk bug }
  if FMoving and (GetKeyState(VK_LBUTTON) = 0) then
    MouseUp(mbLeft, Shift, X, Y);
  inherited MouseMove(Shift, X, Y);
  if GetGripRect.Contains(X, Y) then
    DrawState := DrawState + [dsHot]
  else
    DrawState := DrawState - [dsHot];
  if FKind = sbVertical then
  begin
    Range := Height;
    Delta := Y;
  end
  else
  begin
    Range := Width;
    Delta := X;
  end;
  if Range = 0 then
    Range := 0.000001;
  if (dsPressed in DrawState) and (FMax > FMin) then
    if FKind = sbVertical then
      Position := (Range - Delta) / (Range / (FMax - FMin)) + FMin
    else
      Position := (Delta / Range) * (FMax - FMin) + FMin;
end;

procedure TCustomSlideBar.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    FMoving := True;
    DrawState := DrawState + [dsPressed];
  end;
end;

procedure TCustomSlideBar.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if (Button = mbLeft) and FMoving then
  begin
    FMoving := False;
    DrawState := DrawState - [dsPressed];
    DoChange;
    Click;
  end;
end;

procedure TCustomSlideBar.Render;
var
  R: TRectI;
begin
  R := ClientRect;
  if Assigned(FOnDrawBackground) then
    FOnDrawBackground(Self, Surface, R, FDrawState)
  else if FKind = sbVertical then
    Theme.DrawSplit(R, toVertical)
  else
    Theme.DrawSplit(R, toHorizontal);
  R := GetGripRect;
  if Assigned(FOnDrawThumb) then
    FOnDrawThumb(Self, Surface, GetGripRect, FDrawState)
  else if FKind = sbVertical then
    Theme.DrawThumbThin(R, toVertical)
  else
    Theme.DrawThumbThin(R, toHorizontal);
end;

procedure TCustomSlideBar.SetKind(Value: TSlideBarKind);
var
  I: Integer;
begin
  if Value <> FKind then
  begin
    FKind := Value;
    if csLoading in ComponentState then Exit;
    I := Width;
    Width := Height;
    Height := I;
    Invalidate;
  end;
end;

procedure TCustomSlideBar.SetAssociate(Value: TControl);
var
  I: Double;
begin
  if FAssociate <> nil then
    FAssociate.RemoveFreeNotification(Self);
  FAssociate := Value;
  if FAssociate <> nil then
    FAssociate.FreeNotification(Self);
  if FAssociate <> nil then
  begin
    I := FPosition;
    FPosition := I - 1;
    Position := I;
  end;
end;

procedure TCustomSlideBar.SetMax(const Value: Double);
begin
  if Value <> FMax then
  begin
    if Value < FMin then
      FMax := FMin
    else
      FMax := Value;
    if FPosition > FMax then
      Position := FMax
    else
      Invalidate;
  end;
end;

procedure TCustomSlideBar.SetMin(const Value: Double);
begin
  if Value <> FMin then
  begin
    if FMax < Value then
      FMin := FMax
    else
      FMin := Value;
    if FPosition < FMin then
      Position := FMin
    else
      Invalidate;
  end;
end;

procedure TCustomSlideBar.SetPosition(Value: Double);
var
  S: string;
begin
  if Value < FMin then
    Value := FMin
  else if Value > FMax then
    Value := FMax
  else if FStep > 0 then
    Value := Divide(Value, FStep);
  if Value <> FPosition then
  begin
    FPosition := Value;
    Change;
    if FAssociate <> nil then
    begin
      S := FloatToStr(FPosition);
      if Assigned(FOnFormat) then
        FOnFormat(Self, S);
      TCustomSlideBar(FAssociate).Text := S;
    end;
    Invalidate;
  end;
end;

procedure TCustomSlideBar.EnabledChanged;
begin
  inherited;
  if Enabled then
    DrawState := DrawState - [dsDisabled]
  else
    DrawState := DrawState + [dsDisabled];
end;

procedure TCustomSlideBar.MouseLeave;
begin
  inherited;
  DrawState := DrawState - [dsHot];
end;


end.

