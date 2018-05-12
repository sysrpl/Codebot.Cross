(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified March 2015                                 *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.controls.highlighter.txt> }
unit Codebot.Controls.Highlighter;

{$i codebot.inc}

interface

uses
  Classes, SysUtils, Graphics, Controls, Forms,
  Codebot.System,
  Codebot.Graphics,
  Codebot.Graphics.Types,
  Codebot.Animation;

{ TControlHighlighter }

type
  TControlHighlighter = class(TComponent)
  private
    FOnRender: TDrawRectEvent;
    FSplash: ISplash;
    FTimer: TAnimationTimer;
    FControl: TControl;
    FColor: TColor;
    FOffset: Float;
    FOpacity: Byte;
    FRadius: Float;
    FThickness: Float;
    FVisible: Boolean;
    procedure SetColor(Value: TColor);
    procedure SetControl(Value: TControl);
    procedure SetOffset(Value: Float);
    procedure SetOpacity(Value: Byte);
    procedure SetRadius(Value: Float);
    procedure SetThickness(Value: Float);
    procedure SetVisible(Value: Boolean);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Animate(Sender: TObject);
    procedure Loaded; override;
    procedure Render(Surface: ISurface; Rect: TRectI); virtual;
    procedure Update;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Control: TControl read FControl write SetControl;
    property Color: TColor read FColor write SetColor default clRed;
    property Offset: Float read FOffset write SetOffset default 4;
    property Opacity: Byte read FOpacity write SetOpacity default  $7F;
    property Radius: Float read FRadius write SetRadius default 8;
    property Thickness: Float read FThickness write SetThickness default 4;
    property Visible: Boolean read FVisible write SetVisible;
    property OnRender: TDrawRectEvent read FOnRender write FOnRender;
  end;

implementation

constructor TControlHighlighter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSplash := NewSplash;
  FTimer := TAnimationTimer.Create(Self);
  FTimer.OnTimer := Animate;
  FColor := clRed;
  FThickness := 4;
  FOffset := 4;
  FRadius := 8;
  FOpacity := $7F;
end;

destructor TControlHighlighter.Destroy;
begin
  Control := nil;
  inherited Destroy;
end;

procedure TControlHighlighter.Animate(Sender: TObject);
begin
  Update;
end;

procedure TControlHighlighter.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = FControl then
    begin
      FControl := nil;
      Update;
    end;
end;

procedure TControlHighlighter.Loaded;
begin
  inherited Loaded;
  Update;
end;

procedure TControlHighlighter.Render(Surface: ISurface; Rect: TRectI);
var
  R: TRectF;
  C: TColorB;
  P: IPen;
begin
  if Assigned(FOnRender) then
    FOnRender(Self, Surface, Rect)
  else
  begin
    R := Rect;
    R.Inflate(FThickness / -2  - 1, FThickness / -2  - 1);
    Surface.RoundRectangle(R, FRadius);
    C := Color;
    C.Alpha := FOpacity;
    P := NewPen(C, FThickness);
    P.LinePattern := pnDash;
    P.LineCap := cpRound;
    P.LinePatternOffset := TimeQuery * 12;
    Surface.Stroke(P);
  end;
end;

procedure TControlHighlighter.Update;

  function IsVisible: Boolean;
  var
    C: TControl;
    F: TCustomForm;
  begin
    Result := False;
    if FOpacity = 0 then
      Exit;
    if FControl = nil then
      Exit;
    if not FVisible then
      Exit;
    C := FControl;
    if C.Parent = nil then
      Exit;
    F := nil;
    while C <> nil do
    begin
      if not C.Visible then
        Exit;
      if C is TCustomForm then
      begin
        F := C as TCustomForm;
        Break;
      end;
      C := C.Parent;
    end;
    if F = nil then
      Exit;
    if not F.Visible then
      Exit;
    Result := F.Active;
  end;

var
  S: ISurface;
  P: TPointI;
  I: Integer;
begin
  if csLoading in ComponentState then
    Exit;
  if not IsVisible then
  begin
    FSplash.Visible := False;
    Exit;
  end;
  FSplash.Bitmap.SetSize(Round(Control.Width + FThickness * 2 + FOffset * 2) + 2,
    Round(Control.Height + FThickness * 2 + FOffset * 2) + 2);
  S := FSplash.Bitmap.Surface;
  S.Clear(clTransparent);
  Render(S, FSplash.Bitmap.ClientRect);
  P := TPointI.Create(Control.Left, Control.Top);
  P := Control.Parent.ClientToScreen(P);
  I := Round(FThickness + FOffset + 1);
  P.Offset(-I, -I);
  FSplash.Move(P.X, P.Y);
  FSplash.Update;
  FSplash.Visible := True;
end;

procedure TControlHighlighter.SetControl(Value: TControl);
begin
  if FControl = Value then Exit;
  if FControl <> nil then
    FControl.RemoveFreeNotification(Self);
  FControl := Value;
  if FControl <> nil then
    FControl.FreeNotification(Self);
  FSplash.Visible := False;
  FTimer.Enabled := (FControl <> nil) and FVisible;
end;

procedure TControlHighlighter.SetOffset(Value: Float);
begin
  if Value < 1 then Value := 1;
  if FOffset = Value then Exit;
  FOffset := Value;
  Update;
end;

procedure TControlHighlighter.SetOpacity(Value: Byte);
begin
  if FOpacity = Value then Exit;
  FOpacity := Value;
  Update;
end;

procedure TControlHighlighter.SetRadius(Value: Float);
begin
  if Value < 0 then Value := 0;
  if FRadius = Value then Exit;
  FRadius := Value;
  Update;
end;

procedure TControlHighlighter.SetThickness(Value: Float);
begin
  if Value < 1 then Value := 1;
  if FThickness = Value then Exit;
  FThickness := Value;
  Update;
end;

procedure TControlHighlighter.SetVisible(Value: Boolean);
begin
  if FVisible = Value then Exit;
  FVisible := Value;
  FTimer.Enabled := (FControl <> nil) and FVisible;
  if not (FTimer.Enabled) then
    FSplash.Visible := False;
end;

procedure TControlHighlighter.SetColor(Value: TColor);
begin
  if FColor = Value then Exit;
  FColor := Value;
  Update;
end;

end.

