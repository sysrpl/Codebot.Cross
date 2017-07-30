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
  Classes, SysUtils, Types, Graphics, Controls, LCLIntf,
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
    FShowCaption: Boolean;
    procedure SetKind(Value: TButtonKind);
    procedure SetDown(Value: Boolean);
    procedure SetImages(Value: TImageStrip);
    procedure SetImageIndex(Value: Integer);
    procedure ImagesChanged(Sender: TObject);
    procedure SetShowCaption(Value: Boolean);
  protected
    class function GetControlClassDefaultSize: TSize; override;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: Integer;
      WithThemeSpace: Boolean); override;
    procedure TextChanged; override;
    function ThemeAware: Boolean; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property Images: TImageStrip read FImages write SetImages;
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure Render; override;
    property Down: Boolean read FDown write SetDown;
    property Kind: TButtonKind read FKind write SetKind default bkButton;
    property OnDrawButton: TDrawStateEvent read FOnDrawButton write FOnDrawButton;
    property ShowCaption: Boolean read FShowCaption write SetShowCaption default False;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
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
    property Font;
    property Images;
    property ImageIndex;
    property Kind;
    property ShowCaption;
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

const
  DefThinSize = 24;

constructor TCustomThinButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csClickEvents, csSetCaption];
  FImageIndex := -1;
end;

destructor TCustomThinButton.Destroy;
begin
  Images := nil;
  inherited Destroy;
end;

class function TCustomThinButton.GetControlClassDefaultSize: TSize;
begin
  Result.cx := DefThinSize;
  Result.cy := DefThinSize;
end;

procedure TCustomThinButton.CalculatePreferredSize(var PreferredWidth, PreferredHeight: Integer;
  WithThemeSpace: Boolean);
var
  Size: TPointI;
  S: string;
  W: Integer;
begin
  if not AutoSize then
    Exit;
  PreferredWidth := DefThinSize;
  PreferredHeight := DefThinSize;
  W := 0;
  if FImages <> nil then
    W := FImages.Size;
  if W > PreferredWidth then
  begin
    PreferredWidth := FImages.Size + 8;
    PreferredHeight := PreferredWidth;
  end;
  S := Caption;
  if FShowCaption and (S <> '') then
  begin
    Size := TextSize(S);
    PreferredWidth := Size.X + W + 32;
    PreferredHeight := W + 8;
  end;
  if PreferredWidth < DefThinSize then
    PreferredWidth := DefThinSize;
  if PreferredHeight < DefThinSize then
    PreferredHeight := DefThinSize;
end;

procedure TCustomThinButton.ImagesChanged(Sender: TObject);
begin
  if AutoSize then
  begin
    InvalidatePreferredSize;
    AdjustSize;
  end;
  Invalidate;
end;

procedure TCustomThinButton.TextChanged;
begin
  if AutoSize and FShowCaption then
  begin
    InvalidatePreferredSize;
    AdjustSize;
  end;
  Invalidate;
end;

procedure TCustomThinButton.SetShowCaption(Value: Boolean);
begin
  if FShowCaption = Value then Exit;
  FShowCaption := Value;
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
  if FDown = Value then
    Exit;
  FDown := Value;
  if FDown then
    DrawState := DrawState + [dsPressed]
  else
    DrawState := DrawState - [dsPressed];
end;

function TCustomThinButton.ThemeAware: Boolean;
begin
  Result := True;
end;

procedure TCustomThinButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if FKind = bkSplitter then
    Exit;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCustomThinButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if FKind = bkSplitter then
    Exit;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TCustomThinButton.MouseEnter;
begin
  if FKind = bkSplitter then
    Exit;
  inherited MouseEnter;
end;

procedure TCustomThinButton.MouseLeave;
begin
  if FKind = bkSplitter then
    Exit;
  inherited MouseLeave;
end;

procedure TCustomThinButton.Render;
var
  Size: TPointF;
  D: TDrawState;
  R: TRectI;
  S: string;
  I: Integer;
begin
  inherited Render;
  R := ClientRect;
  if FKind = bkSplitter then
  begin
    if csDesigning in ComponentState then
      Surface.StrokeRoundRect(NewPen(CurrentColor.Darken(0.05)), R, 3);
    R.Inflate(0, -2);
    Theme.DrawSplit(R, toVertical);
    Exit;
  end;
  if (csDesigning in ComponentState) or FDown then
  begin
    if FDown then
      D := [dsPressed]
    else
      D := [dsHot];
    Theme.Select(D);
  end;
  if Assigned(FOnDrawButton) then
    FOnDrawButton(Self, Surface, R, DrawState)
  else
    Theme.DrawButtonThin(R);
  S := Caption;
  if not FShowCaption then
    S := '';
  if FImages <> nil then
  begin
    Size := Surface.TextSize(Theme.Font, S);
    if Size.X = 0 then
    begin
      FImages.Draw(Surface, FImageIndex,
        Width div 2 - FImages.Size div 2, Height div 2 - FImages.Size div 2,
        DrawState);
    end
    else
    begin
      I :=  Round(Width / 2 - (FImages.Size + Size.X + 8) / 2) + 2;
      FImages.Draw(Surface, FImageIndex, I, Height div 2 - FImages.Size div 2,
        DrawState);
      R.Left := I + FImages.Size;
      R.Inflate(-4, 0);
      Surface.TextOut(Theme.Font, S, R, drLeft);
    end;
  end
  else if (S <> '') and (Width > 13) then
  begin
    R.Inflate(-4, 0);
    Surface.TextOut(Theme.Font, S, R, drCenter);
  end;
end;

end.

