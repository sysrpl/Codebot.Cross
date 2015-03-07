(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://www.codebot.org                              *)
(*  Modified February 2015                              *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.controls.extras.txt> }
unit Codebot.Controls.Extras;

{$i codebot.inc}

interface

uses
  SysUtils, Classes, Graphics, Controls, ExtCtrls, Forms,
  Codebot.System,
  Codebot.Controls,
  Codebot.Graphics,
  Codebot.Graphics.Types;

{ TImageMode }

type
  TImageMode = (
    { Center the image in the client area and apply auto sizing if enabled }
    imCenter,
    { Center the image in the client area and shrink if it cannot fit }
    imFit,
    { Fill the client area without distortion }
    imFill,
    { Stretch the image to cover the entire client area }
    imStretch,
    { Repeat the image across the client area }
    imTile);

{ TRenderImage }

  TRenderImage = class(TRenderGraphicControl)
  private
    FImage: TSurfaceBitmap;
    FCopy: TSurfaceBitmap;
    FAngle: Float;
    FColorized: Boolean;
    FMode: TImageMode;
    FSaturation: Float;
    procedure ImageChange(Sender: TObject);
    procedure SetAngle(Value: Float);
    procedure SetColorized(Value: Boolean);
    procedure SetImage(Value: TSurfaceBitmap);
    procedure SetMode(Value: TImageMode);
    function GetOpacity: Byte;
    procedure SetOpacity(Value: Byte);
    procedure SetSaturation(Value: Float);
  protected
    procedure SetColor(Value: TColor); override;
    procedure GetPreferredSize(var PreferredWidth, PreferredHeight: integer;
      Raw: Boolean = False; WithThemeSpace: Boolean = True); override;
    procedure Render; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Image: TSurfaceBitmap read FImage write SetImage;
    property Angle: Float read FAngle write SetAngle;
    property Saturation: Float read FSaturation write SetSaturation;
    property Colorized: Boolean read FColorized write SetColorized;
    property Mode: TImageMode read FMode write SetMode;
    property Opacity: Byte read GetOpacity write SetOpacity;
    property Align;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property Constraints;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property OnChangeBounds;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnRender;
    property OnStartDrag;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
  end;

{ TRenderBox }

  TRenderBox = class(TRenderGraphicControl)
  protected
    procedure Render; override;
  published
    property OnRender;
    property Align;
    property Anchors;
    property BorderSpacing;
    property Constraints;
    property DragCursor;
    property DragMode;
    property Enabled;
    property OnChangeBounds;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDrag;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
  end;

  TProgressStatus = (psNone, psBusy, psReady, psInfo, psHelp, psWarn, psError, psCustom);
  TIconPosition = (icNear, icAbove, icFar, icBelow);

{ TIndeterminateProgress }

  TIndeterminateProgress = class(TRenderGraphicControl)
  private
    FTimer: TTimer;
    FStatus: TProgressStatus;
    FBusyImages: TImageStrip;
    FBusyIndex: Integer;
    FStatusImages: TImageStrip;
    FIconPosition: TIconPosition;
    procedure TimerExpired(Sender: TObject);
    procedure SetStatus(Value: TProgressStatus);
    procedure SetBusyImages(Value: TImageStrip);
    procedure SetStatusImages(Value: TImageStrip);
    procedure ImagesChange(Sender: TObject);
    function GetBusyDelay: Cardinal;
    procedure SetBusyDelay(Value: Cardinal);
    procedure SetIconPosition(Value: TIconPosition);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Render; override;
    procedure FontChanged(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Status: TProgressStatus read FStatus write SetStatus default psReady;
    property BusyImages: TImageStrip read FBusyImages write SetBusyImages;
    property StatusImages: TImageStrip read FStatusImages write SetStatusImages;
    property BusyDelay: Cardinal read GetBusyDelay write SetBusyDelay default 30;
    property IconPosition: TIconPosition read FIconPosition write SetIconPosition default icNear;
    property Align;
    property Anchors;
    property BidiMode;
    property BorderSpacing;
    property Caption;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Font;
    property ParentBidiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDrag;
  end;

{ TStepBubbles }

  TStepBubbles = class(TRenderGraphicControl)
  private
  end;

implementation

{ TRenderImage }

constructor TRenderImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImage := TSurfaceBitmap.Create;
  FImage.OnChange := ImageChange;
  FSaturation := 1;
end;

destructor TRenderImage.Destroy;
begin
  inherited Destroy;
  FImage.Free;
  FCopy.Free;
end;

procedure TRenderImage.Render;
var
  NeedsFit: Boolean;
  Bitmap: TSurfaceBitmap;
  Pen: IPen;
  Brush: IBrush;
  R: TRectI;
  S: Float;
  M: IMatrix;
begin
  inherited Render;
  if csDesigning in ComponentState then
  begin
    Pen := NewPen(clBlack);
    Pen.LinePattern := pnDash;
  end;
  if FImage.Empty then
  begin
    if csDesigning in ComponentState then
      Surface.StrokeRect(Pen, ClientRect);
    Exit;
  end;
  if FColorized  or (FSaturation < 1) then
  begin
    if FCopy = nil then
    begin
      FCopy := TSurfaceBitmap.Create;
      FCopy.Assign(FImage);
      if FColorized then
        FCopy.Colorize(Color)
      else
        FCopy.Desaturate(1 - FSaturation);
    end;
    Bitmap := FCopy;
  end
  else
    Bitmap := FImage;
  NeedsFit := FMode = imFit;
  if NeedsFit then
    if (Bitmap.Width > Width) or (Bitmap.Height > Height) then
      FMode := imFill
    else
      FMode := imCenter;
  M := NewMatrix;
  M.Translate(-Width / 2, -Height / 2);
  M.Rotate(DegToRad(Angle));
  M.Translate(Width / 2, Height / 2);
  case FMode of
    imCenter:
    begin
      Surface.Matrix := M;
      Bitmap.Draw(Surface, (Width - FImage.Width) div 2,
        (Height - Bitmap.Height) div 2);
    end;
    imFill:
    begin
      if Width / Height > Bitmap.Width / Bitmap.Height then
      begin
        R.Top := 0;
        R.Left := 0;
        R.Height := Height;
        R.Width := Round(Height * (Bitmap.Width / Bitmap.Height));
        R.X := (Width - R.Width) div 2;
      end
      else
      begin
        R.Top := 0;
        R.Left := 0;
        R.Width := Width;
        R.Height := Round(Width * (Bitmap.Height / Bitmap.Width));
        R.Y := (Height - R.Height) div 2;
      end;
      Surface.Matrix := M;
      Bitmap.Draw(Surface, Bitmap.ClientRect, R);
    end;
    imStretch:
    begin
      Bitmap.Draw(Surface, Bitmap.ClientRect, ClientRect);
    end;
    imTile:
    begin
      Brush := NewBrush(Bitmap.Bitmap);
      M := NewMatrix;
      {TODO: Fix brush matrix}
      {$ifdef windows}
      M.Rotate(DegToRad(Angle));
      M.Translate(Width / 2, Height / 2);
      {$else}
      M.Translate(Width / 2, Height / 2);
      M.Rotate(DegToRad(Angle));
      {$endif}
      Brush.Matrix := M;
      Brush.Opacity := Opacity;
      Surface.FillRect(Brush, ClientRect);
    end;
  end;
  if NeedsFit then
    FMode := imFit;
  if csDesigning in ComponentState then
    Surface.StrokeRect(Pen, ClientRect);
end;

procedure TRenderImage.ImageChange(Sender: TObject);
begin
  FCopy.Free;
  FCopy := nil;
  Invalidate;
end;

procedure TRenderImage.SetImage(Value: TSurfaceBitmap);
begin
  if FImage = Value then Exit;
  FImage.Assign(Value);
end;

procedure TRenderImage.SetAngle(Value: Float);
begin
  if FAngle = Value then Exit;
  FAngle := Value;
  Invalidate;
end;

procedure TRenderImage.SetColorized(Value: Boolean);
begin
  if FColorized = Value then Exit;
  FColorized := Value;
  FCopy.Free;
  FCopy := nil;
  Invalidate;
end;

procedure TRenderImage.SetMode(Value: TImageMode);
begin
  if FMode = Value then Exit;
  AutoSize := False;
  FMode := Value;
  Invalidate;
end;

function TRenderImage.GetOpacity: Byte;
begin
  Result := FImage.Opacity;
end;

procedure TRenderImage.SetOpacity(Value: Byte);
begin
  FImage.Opacity := Value;
  if FCopy <> nil then
    FCopy.Opacity := Value;
  Invalidate;
end;

procedure TRenderImage.SetSaturation(Value: Float);
begin
  Value := Clamp(Value);
  if FSaturation = Value then Exit;
  FSaturation := Value;
  FCopy.Free;
  FCopy := nil;
  Invalidate;
end;

procedure TRenderImage.SetColor(Value: TColor);
begin
  if Value = Color then Exit;
  inherited SetColor(Value);
  FCopy.Free;
  FCopy := nil;
  Invalidate;
end;

procedure TRenderImage.GetPreferredSize(var PreferredWidth,
  PreferredHeight: integer; Raw: Boolean; WithThemeSpace: Boolean);
begin
  if (not FImage.Empty) and (FMode = imCenter) then
  begin
    PreferredWidth := FImage.Width;
    PreferredHeight := FImage.Height;
  end;
end;

{ TRenderBox }

procedure TRenderBox.Render;
var
  Pen: IPen;
begin
  inherited Render;
  if csDesigning in ComponentState then
  begin
    Pen := NewPen(clBlack);
    Pen.LinePattern := pnDash;
    Surface.StrokeRect(Pen, ClientRect);
  end;
end;

{ TIndeterminateProgress }

{$R progress_icons.res}

var
  GlobalBusyImages: TImageStrip;
  GlobalStatusImages: TImageStrip;

constructor TIndeterminateProgress.Create(AOwner: TComponent);
var
  B: TSurfaceBitmap;
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csSetCaption];
  Width := 160;
  Height := 32;
  FStatus := psReady;
  FIconPosition := icNear;
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 20;
  FTimer.OnTimer := TimerExpired;
  if GlobalBusyImages = nil then
  begin
    B := TSurfaceBitmap.Create;
    B.LoadFromResourceName(HINSTANCE, 'progress_busy');
    GlobalBusyImages := TImageStrip.Create(Application);
    GlobalBusyImages.Add(B);
    B.Free;
  end;
  if GlobalStatusImages = nil then
  begin
    B := TSurfaceBitmap.Create;
    B.LoadFromResourceName(HINSTANCE, 'progress_status');
    GlobalStatusImages := TImageStrip.Create(Application);
    GlobalStatusImages.Add(B);
    B.Free;
  end;
end;

destructor TIndeterminateProgress.Destroy;
begin
  BusyImages := nil;
  StatusImages := nil;
  FTimer.Enabled := False;
  FTimer.Free;
  inherited Destroy;
end;

procedure TIndeterminateProgress.Render;
const
  Dir: array[TIconPosition] of TDirection =
   (drLeft, drCenter, drRight, drCenter);
  Margin = 4;
var
  Images: TImageStrip;
  Index: Integer;
  R: TRectI;
  F: IFont;
  S: string;
begin
  inherited Render;
  Images := nil;
  if Status = psBusy then
  begin
    Images := FBusyImages;
    if (Images = nil) or (Images.Count = 0) then
      Images := GlobalBusyImages;
    FBusyIndex := FBusyIndex mod Images.Count;
    Index := FBusyIndex;
  end
  else if Status > psBusy then
  begin
    Images := FStatusImages;
    if (Images = nil) or (Images.Count = 0) then
      Images := GlobalStatusImages;
    Index := Ord(Status) - Ord(psReady);
  end;
  R := ClientRect;
  S := Caption;
  F := NewFont(Font);
  if Images = nil then
    Surface.TextOut(F, S, R, Dir[FIconPosition])
  else
  begin
    case FIconPosition of
      icNear:
        begin
          Images.Draw(Surface, Index, 0,
            R.MidPoint.Y - Images.Size div 2);
          R.X := R.X + Images.Size + Margin;
          Surface.TextOut(F, S, R, drLeft);
        end;
      icAbove:
        begin
          Images.Draw(Surface, Index, R.MidPoint.X  - Images.Size div 2,
            R.MidPoint.Y - Images.Size);
          R.Y := R.MidPoint.Y + Margin;
          Surface.TextOut(F, S, R, drUp);
        end;
      icFar:
        begin
          Images.Draw(Surface, Index, R.Width - Images.Size,
            R.MidPoint.Y - Images.Size div 2);
          R.Right := R.Right - Images.Size - MArgin;
          Surface.TextOut(F, S, R, drRight);
        end;
      icBelow:
        begin
          Images.Draw(Surface, Index, R.MidPoint.X  - Images.Size div 2,
            R.MidPoint.Y + Images.Size);
          R.Bottom := R.MidPoint.Y - Margin;
          Surface.TextOut(F, S, R, drdown);
        end;
    end;
  end;
end;

procedure TIndeterminateProgress.TimerExpired(Sender: TObject);
begin
  Inc(FBusyIndex);
  Invalidate;
end;

procedure TIndeterminateProgress.SetStatus(Value: TProgressStatus);
begin
  if FStatus = Value then Exit;
  FStatus := Value;
  FTimer.Enabled := FStatus = psBusy;
  Invalidate;
end;

procedure TIndeterminateProgress.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = FBusyImages then
      FBusyImages := nil
    else if AComponent = FStatusImages then
      FStatusImages := nil;
end;

procedure TIndeterminateProgress.ImagesChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TIndeterminateProgress.SetBusyImages(Value: TImageStrip);
begin
  if FBusyImages = Value then Exit;
  if FBusyImages <> nil then
  begin
    FBusyImages.RemoveFreeNotification(Self);
    FBusyImages.OnChange.Remove(ImagesChange);
  end;
  FBusyImages := Value;
  if FBusyImages <> nil then
  begin
    FBusyImages.FreeNotification(Self);
    FBusyImages.OnChange.Add(ImagesChange);
  end;
end;

procedure TIndeterminateProgress.SetStatusImages(Value: TImageStrip);
begin
  if FStatusImages = Value then Exit;
  if FStatusImages <> nil then
  begin
    FStatusImages.RemoveFreeNotification(Self);
    FStatusImages.OnChange.Remove(ImagesChange);
  end;
  FStatusImages := Value;
  if FStatusImages <> nil then
  begin
    FStatusImages.FreeNotification(Self);
    FStatusImages.OnChange.Add(ImagesChange);
  end;
end;

function TIndeterminateProgress.GetBusyDelay: Cardinal;
begin
  Result := FTimer.Interval;
end;

procedure TIndeterminateProgress.SetBusyDelay(Value: Cardinal);
begin
  if Value < 10 then
    Value := 10
  else if Value > 1000 then
    Value := 1000;
  if Value = FTimer.Interval then Exit;
  FTimer.Interval := Value;
end;

procedure TIndeterminateProgress.SetIconPosition(Value: TIconPosition);
begin
  if FIconPosition = Value then Exit;
  FIconPosition := Value;
end;

procedure TIndeterminateProgress.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);
  Invalidate;
end;

end.

