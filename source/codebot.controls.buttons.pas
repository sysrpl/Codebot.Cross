(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified March 2015                                 *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.buttons.txt> }
unit Codebot.Controls.Buttons;

{$i codebot.inc}

interface

uses
  Classes, SysUtils, Graphics, Controls,
  Codebot.System,
  Codebot.Controls,
  Codebot.Graphics,
  Codebot.Graphics.Types;

{ TCustomThinButton }

type
  TButtonKind = (bkButton, skDropDown, bkSplitter);

type
  TCustomThinButton = class(TRenderGraphicControl)
  private
    FKind: TButtonKind;
    FImages: TImageStrip;
    FImageIndex: Integer;
    FDown: Boolean;
    FOnDrawButton: TDrawStateEvent;
    procedure SetKind(Value: TButtonKind);
    procedure SetDown(Value: Boolean);
    procedure SetImages(Value: TImageStrip);
    procedure SetImageIndex(Value: Integer);
    procedure ImagesChanged(Sender: TObject);
  protected
    function ThemeAware: Boolean; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property Images: TImageStrip read FImages write SetImages;
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure Render; override;
    property Down: Boolean read FDown write SetDown;
    property Kind: TButtonKind read FKind write SetKind default bkButton;
    property OnDrawButton: TDrawStateEvent read FOnDrawButton write FOnDrawButton;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click;
  end;

{ TThinButton }

  TThinButton = class(TCustomThinButton)
  published
    property Action;
    property Align;
    property Anchors;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property Constraints;
    property Caption;
    property Color;
    property Down;
    property Enabled;
    property Images;
    property ImageIndex;
    property Kind;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDrawButton;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnChangeBounds;
    property ShowHint;
    property ParentBidiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
  end;

implementation

{ TCustomThinButton }

constructor TCustomThinButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csClickEvents];
  Width := 32;
  Height := 32;
  FImageIndex := -1;
end;

destructor TCustomThinButton.Destroy;
begin
  Images := nil;
  inherited Destroy;
end;

procedure TCustomThinButton.Click;
begin
  if Assigned(OnClick) then
    OnClick(Self);
end;

procedure TCustomThinButton.ImagesChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TCustomThinButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = FImages then
        Images := nil;
end;

procedure TCustomThinButton.SetImageIndex(Value: Integer);
begin
  if Value < 0 then
    Value := -1;
  if FImageIndex = Value then Exit;
  FImageIndex := Value;
  Invalidate;
end;

procedure TCustomThinButton.SetImages(Value: TImageStrip);
begin
  if FImages = Value then Exit;
  if FImages <> nil then
  begin
    FImages.RemoveFreeNotification(Self);
    FImages.OnChange.Remove(ImagesChanged);
  end;
  FImages := Value;
  if FImages <> nil then
  begin
    FImages.FreeNotification(Self);
    FImages.OnChange.Add(ImagesChanged);
  end;
end;

procedure TCustomThinButton.SetKind(Value: TButtonKind);
begin
  if FKind = Value then Exit;
  FKind := Value;
  FDown := False;
  if FKind = bkSplitter then
    Width := 16
  else
    Width := 32;
  DrawState := [];
  Invalidate;
end;

procedure TCustomThinButton.SetDown(Value: Boolean);
begin
  if FKind <> bkButton then
    Value := False;
  if FDown = Value then Exit;
  FDown := Value;
  if FDown then
    DrawState := [dsHot, dsPressed]
  else
    DrawState := [];
end;

function TCustomThinButton.ThemeAware: Boolean;
begin
  Result := True;
end;

procedure TCustomThinButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if FKind = bkSplitter then
    Exit;
  if not FDown then
    if Button = mbLeft then
      DrawState := DrawState + [dsPressed];
end;

procedure TCustomThinButton.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  R: TRectI;
begin
  inherited MouseMove(Shift, X, Y);
  if FDown then
    Exit;
  if FKind = bkSplitter then
    Exit;
  R := ClientRect;
  if R.Contains(X, Y) then
    DrawState := DrawState + [dsHot]
  else
    DrawState := DrawState - [dsHot];
end;

procedure TCustomThinButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FKind = bkSplitter then
    Exit;
  if not FDown then
    if Button = mbLeft then
      DrawState := DrawState - [dsPressed];
end;

procedure TCustomThinButton.MouseEnter;
begin
  inherited MouseEnter;
  if FKind = bkSplitter then
    Exit;
  if not FDown then
    DrawState := DrawState + [dsHot];
end;

procedure TCustomThinButton.MouseLeave;
begin
  inherited MouseLeave;
  if FKind = bkSplitter then
    Exit;
  if not FDown then
    DrawState := DrawState - [dsHot];
end;

procedure TCustomThinButton.Render;
var
  D: TDrawState;
  R: TRectI;
begin
  inherited Render;
  if csDesigning in ComponentState then
  begin
    if FDown then
      D := [dsHot, dsPressed]
    else
      D := [dsHot];
    Theme.Select(D);
  end;
  R := ClientRect;
  if FKind = bkSplitter then
  begin
    if csDesigning in ComponentState then
      Surface.StrokeRoundRect(NewPen(CurrentColor.Darken(0.05)), R, 3);
    R.Inflate(0, -2);
    Theme.DrawSplit(R, toVertical);
  end
  else if Assigned(FOnDrawButton) then
    FOnDrawButton(Self, Surface, R, DrawState)
  else
    Theme.DrawButtonThin(R);
  if FImages <> nil then
  begin
    FImages.Draw(Surface, FImageIndex,
      Width div 2 - FImages.Size div 2, Height div 2 - FImages.Size div 2,
      DrawState);
  end;
end;

end.

