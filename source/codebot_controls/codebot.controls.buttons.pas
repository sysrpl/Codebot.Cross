(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified March 2015                                 *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.buttons.txt> }
unit Codebot.Controls.Buttons;

{$i ../codebot/codebot.inc}

interface

uses
  Classes, SysUtils, Types, Graphics, Controls, LCLIntf,
  Codebot.System,
  Codebot.Controls,
  Codebot.Graphics,
  Codebot.Graphics.Types;

{ TCustomThinButton }

type
  TButtonKind = (bkButton, bkDropDown, bkDialog, bkSpin, bkSplitter);

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
    procedure AreaClick(Area: Integer); override;
    procedure Click; override;
    function GetAreaCount: Integer; override;
    function GetAreaRect(Index: Integer): TRectI; override;
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
  {if not AutoSize then
    Exit;}
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

procedure TCustomThinButton.AreaClick(Area: Integer);
begin
  case Area of
    0: inherited Click;
  end;
end;

procedure TCustomThinButton.Click;
begin
end;

function TCustomThinButton.GetAreaCount: Integer;
begin
  case FKind of
    bkButton, bkSplitter: Result := 1;
    bkDropDown, bkDialog: Result := 2;
    bkSpin: Result := 3;
  end;
end;

function TCustomThinButton.GetAreaRect(Index: Integer): TRectI;
const
  SideWidth = 16;
begin
  case FKind of
    bkButton, bkSplitter:
      begin
        Result := ClientRect;
      end;
    bkDropDown, bkDialog:
      begin
        Result := ClientRect;
        if Index = 0 then
          Result.Right := Result.Right - SideWidth
        else
          Result.Left := Result.Right - SideWidth;
      end;
    bkSpin:
      begin
        Result := ClientRect;
        if Index = 0 then
          Result.Right := Result.Right - SideWidth
        else
        begin
          Result.Left := Result.Right - SideWidth;
          if Index = 1 then
            Result.Bottom := Result.Bottom div 2
          else
            Result.Top := Result.Bottom div 2;
        end;
      end;
  end;
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
  F: IFont;
  P: IPen;
  B: IBrush;
  I: Integer;
  C: TPointI;
begin
  inherited Render;
  R := GetAreaRect(0);
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
  end
  else
  begin
    D := GetAreaState(0);
    Theme.Select(D);
  end;
  R := ClientRect;
  if Assigned(FOnDrawButton) then
    FOnDrawButton(Self, Surface, R, DrawState)
  else
    Theme.DrawButtonThin(R);
  R := GetAreaRect(0);
  S := Caption;
  if not FShowCaption then
    S := '';
  F := NewFont(Font);
  if not ParentEnabled then
    F.Color := ColorToRGB(clGrayText);
  if FImages <> nil then
  begin
    Size := Surface.TextSize(F, S);
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
      Surface.TextOut(F, S, R, drLeft);
    end;
  end
  else if (S <> '') and (Width > 13) then
  begin
    //R.Inflate(-4, 0);
    Surface.TextOut(F, S, R, drCenter);
  end;
  if FKind = bkButton then
    Exit;
  if csDesigning in ComponentState then
  begin
    D := [dsHot];
    Theme.Select(D);
  end
  else
  begin
    D := GetAreaState(1);
    Theme.Select(D);
  end;
  R := GetAreaRect(1);
  P := NewPen(F.Color, 3);
  P.LineCap := cpButt;
  P.LineJoin := jnMiter;
  Theme.DrawButtonThin(R);
  if FKind = bkDropDown then
  begin
    C := R.MidPoint;
    Surface.MoveTo(C.X - 5, C.Y - 2.5);
    Surface.LineTo(C.X, C.Y + 1.5);
    Surface.LineTo(C.X + 5, C.Y - 2.5);
    Surface.Stroke(P);
    Exit;
  end
  else if FKind = bkDialog then
  begin
    B := NewBrush(F.Color);
    R := TRectI.Create(2, 2);
    R.Center(GetAreaRect(1).MidPoint);
    R.Offset(-4, 0);
    Surface.FillRect(B, R);
    R.Offset(4, 0);
    Surface.FillRect(B, R);
    R.Offset(4, 0);
    Surface.FillRect(B, R);
    Exit;
  end;
  C := R.MidPoint;
  Surface.MoveTo(C.X - 5, C.Y + 2.5);
  Surface.LineTo(C.X, C.Y - 1.5);
  Surface.LineTo(C.X + 5, C.Y + 2.5);
  Surface.Stroke(P);
  if csDesigning in ComponentState then
  begin
    D := [dsHot];
    Theme.Select(D);
  end
  else
  begin
    D := GetAreaState(2);
    Theme.Select(D);
  end;
  R := GetAreaRect(2);
  Theme.DrawButtonThin(R);
  C := R.MidPoint;
  Surface.MoveTo(C.X - 5, C.Y - 2.5);
  Surface.LineTo(C.X, C.Y + 1.5);
  Surface.LineTo(C.X + 5, C.Y - 2.5);
  Surface.Stroke(P);
end;

end.

