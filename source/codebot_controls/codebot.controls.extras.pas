(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified March 2015                                 *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.controls.extras.txt> }
unit Codebot.Controls.Extras;

{$i ../codebot/codebot.inc}

interface

uses
  SysUtils, Classes, Graphics, Controls, ExtCtrls, Forms, LMessages,
  Codebot.System,
  Codebot.Controls,
  Codebot.Graphics,
  Codebot.Graphics.Types;

{ TImageMode determines how TDrawImage places its image }

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

{ TDrawImage displays an image which can be desaturated or colorized }

  TDrawImage = class(TSurfaceGraphicControl)
  private
    FImage: TSurfaceBitmap;
    FCopy: TSurfaceBitmap;
    FAngle: Float;
    FColorized: Boolean;
    FMode: TImageMode;
    FSaturation: Float;
    FSharedImage: TSurfaceBitmap;
    function GetComputeImage: TSurfaceBitmap;
    function GetRenderArea: TRectI;
    procedure ImageChange(Sender: TObject);
    procedure SetAngle(Value: Float);
    procedure SetColorized(Value: Boolean);
    procedure SetImage(Value: TSurfaceBitmap);
    procedure SetMode(Value: TImageMode);
    function GetOpacity: Byte;
    procedure SetOpacity(Value: Byte);
    procedure SetSaturation(Value: Float);
    procedure SetSharedImage(Value: TSurfaceBitmap);
  protected
    { The color used when Colorized is true }
    procedure SetColor(Value: TColor); override;
    procedure Draw; override;
    { SharedImage if it is assigned, otherwise Image }
    property ComputeImage: TSurfaceBitmap read GetComputeImage;
  public
    { Create a new draw image }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetPreferredSize(var PreferredWidth, PreferredHeight: integer;
      Raw: Boolean = False; WithThemeSpace: Boolean = True); override;
    { Discard the cached desaturated or colorized copy and repaint }
    procedure UpdateImage;
    { The area of the control covered by the image }
    property RenderArea: TRectI read GetRenderArea;
    { An image owned elsewhere which is drawn instead of Image when assigned }
    property SharedImage: TSurfaceBitmap read FSharedImage write SetSharedImage;
  published
    { The image to draw }
    property Image: TSurfaceBitmap read FImage write SetImage;
    { A rotation angle, currently unused when drawing }
    property Angle: Float read FAngle write SetAngle;
    { The image saturation from 0 for gray to 1 for full color }
    property Saturation: Float read FSaturation write SetSaturation;
    { When true the image is tinted using Color }
    property Colorized: Boolean read FColorized write SetColorized;
    { How the image is placed in the control }
    property Mode: TImageMode read FMode write SetMode;
    { The transparency of the image }
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
    property OnDraw;
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

{ TDrawBox is a graphic control which draws nothing on its own. Use OnDraw
  to draw on its surface. }

  TDrawBox = class(TSurfaceGraphicControl)
  protected
    procedure Draw; override;
  published
    property OnDraw;
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

{ TDrawPanel is a windowed control which draws nothing on its own. Use
  OnDraw to draw on its surface. }

  TDrawPanel = class(TSurfaceCustomControl)
  protected
    procedure Draw; override;
  public
    { Create a new draw panel }
    constructor Create(AOwner: TComponent); override;
  published
    property OnDraw;
    property Align;
    property Anchors;
    property BorderSpacing;
    property Constraints;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabStop;
    property Visible;
    property OnChangeBounds;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
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

  { TProgressStatus selects the icon shown by TIndeterminateProgress }
  TProgressStatus = (psNone, psBusy, psReady, psInfo, psHelp, psWarn, psError, psCustom);
  { TIconPosition is where an icon is placed relative to its text }
  TIconPosition = (icNear, icAbove, icFar, icBelow);

{ TIndeterminateProgress shows a status icon next to its caption, animating
  a busy icon while the status is psBusy }

  TIndeterminateProgress = class(TSurfaceGraphicControl)
  private
    FHelp: string;
    FTimer: TTimer;
    FStatus: TProgressStatus;
    FBusyImages: TImageStrip;
    FBusyIndex: Integer;
    FStatusImages: TImageStrip;
    FIconPosition: TIconPosition;
    procedure SetHelp(Value: string);
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
    procedure Draw; override;
    procedure FontChanged(Sender: TObject); override;
    procedure TextChanged; override;
  public
    { Create a new progress indicator }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { The current status }
    property Status: TProgressStatus read FStatus write SetStatus default psReady;
    { Animation frames shown while busy, or nil to use the defaults }
    property BusyImages: TImageStrip read FBusyImages write SetBusyImages;
    { Status icons starting with psReady, or nil to use the defaults }
    property StatusImages: TImageStrip read FStatusImages write SetStatusImages;
    { Milliseconds between busy animation frames }
    property BusyDelay: Cardinal read GetBusyDelay write SetBusyDelay default 30;
    { Where the icon is placed relative to the text }
    property IconPosition: TIconPosition read FIconPosition write SetIconPosition default icNear;
    { When not empty this text is shown with the help icon instead of the caption }
    property Help: string read FHelp write SetHelp;
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

{ TStepClickEvent is invoked when a step of a TStepBubbles is clicked }

  TStepClickEvent = procedure(Sender: TObject; StepIndex: Integer) of object;

{ TStepBubbles shows the steps of a process, such as the pages of a wizard,
  as a row of arrows. Each arrow has a numbered circle at its tail and
  points to the next step. The last step is drawn as a rounded bubble.
  Steps before and including StepIndex are highlighted. }

  TStepBubbles = class(TSurfaceGraphicControl)
  private
    FStepIndex: Integer;
    FHotStepIndex: Integer;
    FDownStepIndex: Integer;
    FSteps: TStrings;
    FStepRects: TButtonRects;
    FHotTrack: Boolean;
    FTransparent: Boolean;
    FOnStep: TNotifyEvent;
    FOnStepClick: TStepClickEvent;
    procedure StepsChange(Sender: TObject);
    procedure SetStepIndex(Value: Integer);
    procedure SetSteps(Value: TStrings);
    procedure SetTransparent(Value: Boolean);
  protected
    { Position the steps and return the size needed to draw them }
    function Layout(Target: ISurface): TPointI;
    { Return the step at a point or -1 if there is none }
    function StepFromPoint(X, Y: Integer): Integer;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: Integer;
      WithThemeSpace: Boolean); override;
    procedure FontChanged(Sender: TObject); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseLeave; override;
    procedure Draw; override;
  public
    { Create a new step bubbles control }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { The current step or -1 if no step has been reached }
    property StepIndex: Integer read FStepIndex write SetStepIndex default 0;
    { The caption of each step, one per line }
    property Steps: TStrings read FSteps write SetSteps;
    { When true the step under the mouse is highlighted }
    property HotTrack: Boolean read FHotTrack write FHotTrack default False;
    { When false the background is filled with Color }
    property Transparent: Boolean read FTransparent write SetTransparent default True;
    { OnStep is invoked when StepIndex changes }
    property OnStep: TNotifyEvent read FOnStep write FOnStep;
    { OnStepClick is invoked when a step is clicked }
    property OnStepClick: TStepClickEvent read FOnStepClick write FOnStepClick;
    property Align;
    property Anchors;
    property AutoSize default True;
    property BorderSpacing;
    property Color;
    property Constraints;
    property Cursor;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
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

implementation

{ TDrawImage }

constructor TDrawImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImage := TSurfaceBitmap.Create;
  FImage.OnChange := ImageChange;
  FSaturation := 1;
  Width := 256;
  Height := 256;
end;

destructor TDrawImage.Destroy;
begin
  inherited Destroy;
  FImage.Free;
  FCopy.Free;
end;

procedure TDrawImage.UpdateImage;
begin
  FCopy.Free;
  FCopy := nil;
  Invalidate;
end;

function TDrawImage.GetComputeImage: TSurfaceBitmap;
begin
  if FSharedImage <> nil then
    Result := FSharedImage
  else
    Result := FImage;
end;

function TDrawImage.GetRenderArea: TRectI;
var
  B: TSurfaceBitmap;
  M: TImageMode;
begin
  B := ComputeImage;
  M := FMode;
  if M = imFit then
    if (B.Width > Width) or (B.Height > Height) then
      M := imFill
    else
      M := imCenter;
  case M of
    imCenter:
    begin
      Result := B.ClientRect;
      Result.Offset((Width - B.Width) div 2, (Height - B.Height) div 2);
    end;
    imFill:
    if B.Empty then
    begin
      Result := B.ClientRect;
      Result.Offset(Width div 2, Height div 2);
    end
    else if Width / Height > B.Width / B.Height then
    begin
      Result.Top := 0;
      Result.Left := 0;
      Result.Height := Height;
      Result.Width := Round(Height * (B.Width / B.Height));
      Result.X := (Width - Result.Width) div 2;
    end
    else
    begin
      Result.Top := 0;
      Result.Left := 0;
      Result.Width := Width;
      Result.Height := Round(Width * (B.Height / B.Width));
      Result.Y := (Height - Result.Height) div 2;
    end;
  else
    Result := ClientRect;
  end;
end;

procedure TDrawImage.Draw;
var
  NeedsFit: Boolean;
  Bitmap: TSurfaceBitmap;
  Pen: IPen;
  Brush: IBrush;
  R: TRectI;
  M: IMatrix;
begin
  inherited Draw;
  if csDesigning in ComponentState then
  begin
    Pen := NewPen(clBlack);
    Pen.LinePattern := pnDash;
  end;
  if ComputeImage.Empty then
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
      FCopy.Assign(ComputeImage);
      if FColorized then
        FCopy.Colorize(Color)
      else
        FCopy.Desaturate(1 - FSaturation);
    end;
    Bitmap := FCopy;
  end
  else
    Bitmap := ComputeImage;
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
      Bitmap.Draw(Surface, (Width - ComputeImage.Width) div 2,
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

procedure TDrawImage.ImageChange(Sender: TObject);
begin
  FCopy.Free;
  FCopy := nil;
  Invalidate;
end;

procedure TDrawImage.SetImage(Value: TSurfaceBitmap);
begin
  if FImage = Value then Exit;
  FImage.Assign(Value);
end;

procedure TDrawImage.SetAngle(Value: Float);
begin
  if FAngle = Value then Exit;
  FAngle := Value;
  Invalidate;
end;

procedure TDrawImage.SetColorized(Value: Boolean);
begin
  if FColorized = Value then Exit;
  FColorized := Value;
  FCopy.Free;
  FCopy := nil;
  Invalidate;
end;

procedure TDrawImage.SetMode(Value: TImageMode);
begin
  if FMode = Value then Exit;
  AutoSize := False;
  FMode := Value;
  Invalidate;
end;

function TDrawImage.GetOpacity: Byte;
begin
  Result := ComputeImage.Opacity;
end;

procedure TDrawImage.SetOpacity(Value: Byte);
begin
  ComputeImage.Opacity := Value;
  if FCopy <> nil then
    FCopy.Opacity := Value;
  Invalidate;
end;

procedure TDrawImage.SetSaturation(Value: Float);
begin
  Value := Clamp(Value);
  if FSaturation = Value then Exit;
  FSaturation := Value;
  FCopy.Free;
  FCopy := nil;
  Invalidate;
end;

procedure TDrawImage.SetSharedImage(Value: TSurfaceBitmap);
begin
  FSharedImage := Value;
  UpdateImage;
end;

procedure TDrawImage.SetColor(Value: TColor);
begin
  if Value = Color then Exit;
  inherited SetColor(Value);
  FCopy.Free;
  FCopy := nil;
  Invalidate;
end;

procedure TDrawImage.GetPreferredSize(var PreferredWidth,
  PreferredHeight: integer; Raw: Boolean; WithThemeSpace: Boolean);
begin
  if (not FImage.Empty) and (FMode = imCenter) then
  begin
    PreferredWidth := ComputeImage.Width;
    PreferredHeight := ComputeImage.Height;
  end;
end;

{ TDrawBox }

procedure TDrawBox.Draw;
var
  Pen: IPen;
begin
  inherited Draw;
  if csDesigning in ComponentState then
  begin
    Pen := NewPen(clBlack);
    Pen.LinePattern := pnDash;
    Surface.StrokeRect(Pen, ClientRect);
  end;
end;

{ TDrawPanel }

procedure TDrawPanel.Draw;
var
  Pen: IPen;
begin
  inherited Draw;
  if csDesigning in ComponentState then
  begin
    Pen := NewPen(clBlack);
    Pen.LinePattern := pnDash;
    Surface.StrokeRect(Pen, ClientRect);
  end;
end;

constructor TDrawPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TabStop := False;
  ControlStyle := ControlStyle + [csAcceptsControls, csCaptureMouse,
    csClickEvents, csDoubleClicks, csReplicatable,
    csNoFocus, csParentBackground] - [csOpaque];
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
    GlobalBusyImages.Colorize(clWindowText);
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

procedure TIndeterminateProgress.Draw;
const
  Dir: array[TIconPosition] of TDirection =
    (drLeft, drCenter, drRight, drCenter);
  Margin = 4;
var
  ComputedStatus: TProgressStatus;
  Images: TImageStrip;
  Index: Integer;
  R: TRectI;
  F: IFont;
  S: string;
begin
  inherited Draw;
  Images := nil;
  ComputedStatus := Status;
  if FHelp <> '' then
    ComputedStatus := psHelp;
  if ComputedStatus = psBusy then
  begin
    Images := FBusyImages;
    if (Images = nil) or (Images.Count = 0) then
      Images := GlobalBusyImages;
    FBusyIndex := FBusyIndex mod Images.Count;
    Index := FBusyIndex;
  end
  else if ComputedStatus > psBusy then
  begin
    Images := FStatusImages;
    if (Images = nil) or (Images.Count = 0) then
      Images := GlobalStatusImages;
    Index := Ord(ComputedStatus) - Ord(psReady);
  end;
  R := ClientRect;
  S := Caption;
  if FHelp <> '' then
    S := FHelp;
  F := NewFont(Font);
  if Images = nil then
    Surface.TextOut(F, S, R, Dir[FIconPosition])
  else
  begin
    case FIconPosition of
      icNear:
        begin
          Images.Draw(Surface, Index, Margin,
            R.MidPoint.Y - Images.Size div 2);
          R.X := R.X + Images.Size + Margin + Margin;
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

procedure TIndeterminateProgress.SetHelp(Value: string);
begin
  if FHelp = Value then Exit;
  FHelp := Value;
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

procedure TIndeterminateProgress.TextChanged;
begin
  inherited TextChanged;
  Invalidate;
end;

{ TStepBubbles }

const
  StepArrowWidth = 24;
  StepOffset = 32;
  StepCenter = StepArrowWidth + 2;

{ Mix two colors where Percent is the amount of Fore }

function StepBlend(Fore, Back: TColor; Percent: Integer = 50): TColorB;
var
  F, B: TColorB;
begin
  F := Fore;
  B := Back;
  Result := B.Blend(F, Percent / 100);
end;

function StepBoldFont(Font: TFont): IFont;
begin
  Result := NewFont(Font);
  Result.Style := Result.Style + [fsBold];
  Result.Size := Result.Size * 1.5;
end;

{ Add a horizontal arrow from A to B with a body of Width and a head of twice
  Width to the current path }

procedure StepArrowPath(Surface: ISurface; const A, B: TPointF; Width: Float);
var
  W: Float;
begin
  W := Width / 2;
  Surface.MoveTo(A.X - W, A.Y - W);
  Surface.LineTo(B.X - W, B.Y - W);
  Surface.LineTo(B.X - W, B.Y - Width);
  Surface.LineTo(B.X + Width, B.Y);
  Surface.LineTo(B.X - W, B.Y + Width);
  Surface.LineTo(B.X - W, B.Y + W);
  Surface.LineTo(A.X - W, A.Y + W);
  Surface.Path.Close;
end;

{ Add a circle at A with a radius of Width to the current path }

procedure StepCirclePath(Surface: ISurface; const A: TPointF; Width: Float);
begin
  Surface.Ellipse(TRectF.Create(A.X - Width, A.Y - Width, Width * 2, Width * 2));
end;

{ Add a horizontal capsule from A to B with a height of Width to the current path }

procedure StepCapsulePath(Surface: ISurface; const A, B: TPointF; Width: Float);
var
  W: Float;
begin
  W := Width / 2;
  Surface.RoundRectangle(TRectF.Create(A.X - W, A.Y - W, B.X - A.X + Width, Width), W);
end;

{ Draw text centered on a point }

procedure StepText(Surface: ISurface; Font: IFont; const Text: string; const P: TPointF);
begin
  Surface.TextOut(Font, Text, TRectF.Create(P.X - 1000, P.Y - 100, 2000, 200), drCenter);
end;

{ Draw an arrow with a numbered circle at its tail. The shape is drawn in
  three layers to give an outlined look: an outer ring and fill in Back, a
  band in Fore, and an inner fill in Back. Each layer fills the arrow and
  circle separately so their outlines do not show where they overlap. }

procedure DrawBubbleArrow(Surface: ISurface; Font, Bold: IFont; const Caption: string;
  Step: Integer; const A, B: TPointF; Width: Float; Fore, Back: TColorB);
var
  Pen: IPen;
  Brush: IBrush;
begin
  Pen := NewPen(Back, 2.5);
  Brush := NewBrush(Back);
  StepArrowPath(Surface, A, B, Width);
  Surface.Stroke(Pen);
  StepCirclePath(Surface, A, Width);
  Surface.Stroke(Pen);
  StepArrowPath(Surface, A, B, Width);
  Surface.Fill(Brush);
  StepCirclePath(Surface, A, Width);
  Surface.Fill(Brush);
  Brush := NewBrush(Fore);
  StepArrowPath(Surface, A, B, Width - 2.5);
  Surface.Fill(Brush);
  StepCirclePath(Surface, A, Width - 2.5);
  Surface.Fill(Brush);
  Brush := NewBrush(Back);
  StepArrowPath(Surface, A, B, Width - 6);
  Surface.Fill(Brush);
  StepCirclePath(Surface, A, Width - 6);
  Surface.Fill(Brush);
  Font.Color := Fore;
  Bold.Color := Fore;
  StepText(Surface, Font, Caption, TPointF.Create((A.X + B.X) / 2, A.Y));
  StepText(Surface, Bold, IntToStr(Step + 1), A);
end;

{ Draw the last step as a capsule using the same layers as DrawBubbleArrow }

procedure DrawBubble(Surface: ISurface; Bold: IFont; const Caption: string;
  const A, B: TPointF; Width: Float; Fore, Back: TColorB);
begin
  StepCapsulePath(Surface, A, B, Width);
  Surface.Stroke(NewPen(Back, 2.5), True);
  Surface.Fill(NewBrush(Back));
  StepCapsulePath(Surface, A, B, Width - 5);
  Surface.Fill(NewBrush(Fore));
  StepCapsulePath(Surface, A, B, Width - 12);
  Surface.Fill(NewBrush(Back));
  Bold.Color := Fore;
  StepText(Surface, Bold, Caption, TPointF.Create((A.X + B.X) / 2, A.Y));
end;

constructor TStepBubbles.Create(AOwner: TComponent);
var
  S: TStringList;
begin
  inherited Create(AOwner);
  S := TStringList.Create;
  S.Add('Step one');
  S.Add('Step two');
  S.Add('Step three');
  S.Add('Done');
  S.OnChange := StepsChange;
  FSteps := S;
  FStepIndex := 0;
  FDownStepIndex := -1;
  FHotStepIndex := -1;
  FTransparent := True;
  FStepRects.Length := FSteps.Count;
  Width := 400;
  Height := StepArrowWidth * 2 + 4;
  AutoSize := True;
end;

destructor TStepBubbles.Destroy;
begin
  FSteps.Free;
  inherited Destroy;
end;

function TStepBubbles.Layout(Target: ISurface): TPointI;
var
  F, B: IFont;
  X, W: Float;
  R: TRectI;
  I, Last: Integer;
begin
  F := NewFont(Font);
  B := StepBoldFont(Font);
  Last := FSteps.Count - 1;
  FStepRects.Length := FSteps.Count;
  X := -StepArrowWidth / 2 + 2;
  for I := 0 to Last do
  begin
    X := X + StepArrowWidth + StepArrowWidth / 2;
    if I = Last then
      W := Target.TextSize(B, FSteps[I]).X
    else
      W := Target.TextSize(F, FSteps[I]).X;
    { The rect of a step spans its line from A to B }
    R.X := Round(X);
    R.Width := Round(X + W + StepOffset) - R.X;
    R.Y := StepCenter - StepArrowWidth div 2;
    R.Height := StepArrowWidth;
    if I = Last then
    begin
      R.X := Round(X - StepArrowWidth / 4);
      R.Width := Round(X + W - StepArrowWidth) - R.X;
      R.Y := StepCenter - StepArrowWidth;
      R.Height := StepArrowWidth * 2;
    end;
    FStepRects[I] := R;
    X := X + W + StepOffset;
  end;
  Result := TPointI.Create(StepArrowWidth * 2 + 4, StepArrowWidth * 2 + 4);
  if Last > -1 then
    Result.X := FStepRects[Last].Right + StepArrowWidth + 2;
end;

function TStepBubbles.StepFromPoint(X, Y: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := FStepRects.Lo to FStepRects.Hi do
    if FStepRects[I].Contains(X, Y) then
      Exit(I);
end;

procedure TStepBubbles.CalculatePreferredSize(var PreferredWidth, PreferredHeight: Integer;
  WithThemeSpace: Boolean);
var
  Bitmap: IBitmap;
  Size: TPointI;
begin
  { Measure text using a small bitmap since Surface is only valid in Draw }
  Bitmap := NewBitmap(1, 1);
  Size := Layout(Bitmap.Surface);
  PreferredWidth := Size.X;
  PreferredHeight := Size.Y;
end;

procedure TStepBubbles.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);
  InvalidatePreferredSize;
  AdjustSize;
  Invalidate;
end;

procedure TStepBubbles.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  I: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    I := StepFromPoint(X, Y);
    if FDownStepIndex <> I then
    begin
      FDownStepIndex := I;
      if FHotTrack then
        Invalidate;
    end;
  end;
end;

procedure TStepBubbles.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
begin
  inherited MouseMove(Shift, X, Y);
  I := StepFromPoint(X, Y);
  if FHotStepIndex <> I then
  begin
    FHotStepIndex := I;
    if FHotTrack then
      Invalidate;
  end;
end;

procedure TStepBubbles.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  I, J: Integer;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if (Button = mbLeft) and (FDownStepIndex > -1) then
  begin
    I := StepFromPoint(X, Y);
    J := FDownStepIndex;
    FDownStepIndex := -1;
    if FHotTrack then
      Invalidate;
    if (J = I) and Assigned(FOnStepClick) then
      FOnStepClick(Self, I);
  end;
end;

procedure TStepBubbles.MouseLeave;
begin
  inherited MouseLeave;
  if FHotStepIndex > -1 then
  begin
    FHotStepIndex := -1;
    if FHotTrack then
      Invalidate;
  end;
end;

procedure TStepBubbles.Draw;
var
  F, B: IFont;
  P0, P1: TPointF;
  Fore, Back: TColorB;
  R: TRectI;
  I: Integer;
begin
  if not FTransparent then
    FillRectColor(Surface, ClientRect, Color);
  Layout(Surface);
  F := NewFont(Font);
  B := StepBoldFont(Font);
  { Draw backwards so each arrow head lies on top of the next step }
  for I := FSteps.Count - 1 downto 0 do
  begin
    Fore := clHighlightText;
    { Past steps are a slightly darker tone of the highlight }
    Back := StepBlend(clHighlight, clBlack, 80);
    if I = FStepIndex then
      Back := StepBlend(clHighlight, clWindowText, 80);
    if FHotTrack then
    begin
      if I > FStepIndex then
        if I = FHotStepIndex then
          if I = FDownStepIndex then
            Back := StepBlend(cl3DDkShadow, clWindowText)
          else
            Back := clBtnShadow
        else
          Back := cl3DDkShadow
      else if I = FHotStepIndex then
        if I = FDownStepIndex then
          Back := StepBlend(clHighlight, clWindowText, 70)
        else
          Back := StepBlend(clHighlight, clHighlightText, 60);
    end
    else if I > FStepIndex then
      Back := cl3DDkShadow;
    if not Enabled then
      Back := Back.Desaturate(1);
    R := FStepRects[I];
    P0 := TPointF.Create(R.Left, StepCenter);
    P1 := TPointF.Create(R.Right, StepCenter);
    if I = FSteps.Count - 1 then
      DrawBubble(Surface, B, FSteps[I], P0, P1, StepArrowWidth * 2, Fore, Back)
    else
      DrawBubbleArrow(Surface, F, B, FSteps[I], I, P0, P1, StepArrowWidth, Fore, Back);
  end;
end;

procedure TStepBubbles.StepsChange(Sender: TObject);
begin
  FDownStepIndex := -1;
  FHotStepIndex := -1;
  StepIndex := FStepIndex;
  FStepRects.Length := FSteps.Count;
  InvalidatePreferredSize;
  AdjustSize;
  Invalidate;
end;

procedure TStepBubbles.SetStepIndex(Value: Integer);
begin
  if Value < -1 then
    Value := -1
  else if Value > FSteps.Count - 1 then
    Value := FSteps.Count - 1;
  if Value <> FStepIndex then
  begin
    FStepIndex := Value;
    if Assigned(FOnStep) then
      FOnStep(Self);
    Invalidate;
  end;
end;

procedure TStepBubbles.SetSteps(Value: TStrings);
begin
  FSteps.Assign(Value);
  StepIndex := -1;
end;

procedure TStepBubbles.SetTransparent(Value: Boolean);
begin
  if FTransparent = Value then Exit;
  FTransparent := Value;
  Invalidate;
end;

end.

