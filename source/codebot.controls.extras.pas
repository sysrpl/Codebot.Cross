(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified March 2015                                 *)
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

{ THeaderColumn }

type
  THeaderColumn = class(TCollectionItem)
  private
    FAlignment: TAlignment;
    FCaption: string;
    FFixed: Boolean;
    FMinWidth: Integer;
    FVisible: Boolean;
    FWidth: Integer;
    FOnResize: TNotifyDelegate;
    function GetVisibleIndex: Integer;
    procedure SetAlignment(Value: TAlignment);
    procedure SetCaption(Value: string);
    procedure SetFixed(Value: Boolean);
    procedure SetMinWidth(Value: Integer);
    procedure SetVisible(Value: Boolean);
    procedure SetWidth(Value: Integer);
    function GetOnResize: INotifyDelegate;
  protected
    procedure DoResize;
  public
    constructor Create(ACollection: TCollection); override;
    property OnResize: INotifyDelegate read GetOnResize;
    property VisibleIndex: Integer read GetVisibleIndex;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property Caption: string read FCaption write SetCaption;
    property Fixed: Boolean read FFixed write SetFixed default False;
    property MinWidth: Integer read FMinWidth write SetMinWidth default 10;
    property Visible: Boolean read FVisible write SetVisible default True;
    property Width: Integer read FWidth write SetWidth default 100;
  end;

  THeaderColumns = class(TNotifyCollection<THeaderColumn>)
  end;

{ THeaderColumnEvent }

  THeaderColumnEvent = procedure(Sender: TObject; Column: THeaderColumn) of object;

{ THeaderBar }

  THeaderBar = class(TRenderGraphicControl)
  private
    FColumns: THeaderColumns;
    FOnColumnClick: THeaderColumnEvent;
    FOnColumnResize: THeaderColumnEvent;
    FOnColumnSelect: THeaderColumnEvent;
    FScrollLeft: Integer;
    FSizeIndex: Integer;
    FDownIndex: Integer;
    FHotIndex: Integer;
    FHotTrack: Boolean;
    FPriorCursor: TCursor;
    FSelected: THeaderColumn;
    procedure HandleColumnResize(Sender: TObject);
    procedure HandleColumnUpdate(Sender: TObject; Item: TCollectionItem);
    procedure HandleColumnNotify(Sender: TObject; Item: TCollectionItem; Action: TCollectionNotification);
    function GetHotIndex: Integer;
    procedure SetColumns(Value: THeaderColumns);
    procedure SetHotTrack(Value: Boolean);
    procedure SetScrollLeft(Value: Integer);
    procedure SetSelected(Value: THeaderColumn);
  protected
    function ThemeAware: Boolean; override;
    procedure Render; override;
    procedure ColumnClick(Column: THeaderColumn); virtual;
    procedure ColumnResize(Column: THeaderColumn); virtual;
    procedure ColumnSelect(Column: THeaderColumn); virtual;
    procedure MouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetColumnRect(Index: Integer): TRectI;
    function GetSizingRect(Index: Integer): TRectI;
    property HotIndex: Integer read GetHotIndex;
    property ScrollLeft: Integer read FScrollLeft write SetScrollLeft;
    property Selected: THeaderColumn read FSelected write SetSelected;
  published
    property Columns: THeaderColumns read FColumns write SetColumns;
    property HotTrack: Boolean read FHotTrack write SetHotTrack default True;
    property Align;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property Constraints;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ThemeName;
    property ShowHint;
    property Visible;
    property OnColumnSelect: THeaderColumnEvent read FOnColumnSelect write FOnColumnSelect;
    property OnColumnClick: THeaderColumnEvent read FOnColumnClick write FOnColumnClick;
    property OnColumnResize: THeaderColumnEvent read FOnColumnResize write FOnColumnResize;
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
  end;

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
    procedure SetColor(Value: TColor); override;
    procedure GetPreferredSize(var PreferredWidth, PreferredHeight: integer;
      Raw: Boolean = False; WithThemeSpace: Boolean = True); override;
    procedure Render; override;
    property ComputeImage: TSurfaceBitmap read GetComputeImage;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateImage;
    property RenderArea: TRectI read GetRenderArea;
    property SharedImage: TSurfaceBitmap read FSharedImage write SetSharedImage;
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

{ TStepBubbles }

  TStepBubbles = class(TRenderGraphicControl)
  private
  end;

implementation

{ THeaderColumn }

constructor THeaderColumn.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FAlignment := taLeftJustify;
  FCaption := 'New Column';
  FMinWidth := 10;
  FVisible := True;
  FWidth := 100;
end;

procedure THeaderColumn.SetAlignment(Value: TAlignment);
begin
  if FAlignment = Value then Exit;
  FAlignment := Value;
  Changed(False);
end;

function THeaderColumn.GetVisibleIndex: Integer;
var
  C: THeaderColumns;
  I: Integer;
begin
  Result := -1;
  if not Visible then
    Exit;
  C := Collection as THeaderColumns;
  for I := 0 to C.Count - 1 do
  begin
    if C[I].Visible then
      Inc(Result);
    if C[I] = Self then
      Break;
  end;
end;

procedure THeaderColumn.SetCaption(Value: string);
begin
  if FCaption = Value then Exit;
  FCaption := Value;
  Changed(False);
end;

procedure THeaderColumn.SetFixed(Value: Boolean);
begin
  if FFixed = Value then Exit;
  FFixed := Value;
end;

procedure THeaderColumn.SetMinWidth(Value: Integer);
begin
  if Value < 10 then
    Value := 10;
  if FMinWidth = Value then Exit;
  FMinWidth := Value;
  if FWidth < FMinWidth then
    FWidth := FMinWidth;
  Changed(False);
end;

procedure THeaderColumn.SetVisible(Value: Boolean);
begin
  if FVisible = Value then Exit;
  FVisible := Value;
end;

procedure THeaderColumn.SetWidth(Value: Integer);
begin
  if Value < FMinWidth then
    Value := FMinWidth;
  if FWidth = Value then Exit;
  FWidth := Value;
  DoResize;
end;

procedure THeaderColumn.DoResize;
var
  Event: TNotifyEvent;
begin
  for Event in FOnResize do Event(Self);
end;

function THeaderColumn.GetOnResize: INotifyDelegate;
begin
  Result := FOnResize;
end;

{ THeaderBar }

constructor THeaderBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csClickEvents];
  Width := 160;
  Height := 24;
  FColumns := THeaderColumns.Create(Self);
  FColumns.OnItemNotify.Add(HandleColumnNotify);
  FColumns.OnItemUpdate.Add(HandleColumnUpdate);
  FSizeIndex := -1;
  FDownIndex := -1;
  FHotIndex := -1;
  FHotTrack := True;
end;

destructor THeaderBar.Destroy;
begin
  FColumns.Free;
  inherited Destroy;
end;

function THeaderBar.ThemeAware: Boolean;
begin
  Result := True;
end;

procedure THeaderBar.HandleColumnNotify(Sender: TObject; Item: TCollectionItem;
  Action: TCollectionNotification);
var
  C: THeaderColumn;
begin
  C := Item as THeaderColumn;
  if Action = cnAdded then
    C.OnResize.Add(HandleColumnResize)
  else if C = FSelected then
    Selected := nil;
  Invalidate;
end;

procedure THeaderBar.HandleColumnResize(Sender: TObject);
begin
  Invalidate;
  ColumnResize(Sender as THeaderColumn);
end;

procedure THeaderBar.HandleColumnUpdate(Sender: TObject; Item: TCollectionItem);
begin
  Invalidate;
end;

procedure THeaderBar.SetColumns(Value: THeaderColumns);
begin
  if FColumns = Value then Exit;
  FColumns.Assign(Value);
end;

function THeaderBar.GetHotIndex: Integer;
begin
  if FHotTrack then
    Result := FHotIndex
  else
    Result := -1;
end;

procedure THeaderBar.SetHotTrack(Value: Boolean);
begin
  if FHotTrack = Value then Exit;
  FHotTrack := Value;
end;

procedure THeaderBar.SetScrollLeft(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if FScrollLeft = Value then Exit;
  FScrollLeft := Value;
  Invalidate;
end;

procedure THeaderBar.SetSelected(Value: THeaderColumn);
begin
  if FSelected = Value then Exit;
  FSelected := Value;
  Invalidate;
  ColumnSelect(FSelected);
end;

function THeaderBar.GetColumnRect(Index: Integer): TRectI;
var
  I: Integer;
begin
  Result := ClientRect;
  Result.Width := 0;
  if (Index < 0) or (Index > FColumns.Count - 1) then
    Exit;
  for I := 0 to Index - 1 do
    if FColumns[I].Visible then
      Result.X := Result.X + FColumns[I].Width;
  if FColumns[Index].Visible then
    Result.Width := FColumns[Index].Width;
  Dec(Result.X, FScrollLeft);
end;

function THeaderBar.GetSizingRect(Index: Integer): TRectI;
const
  Size = 2;
begin
  Result := GetColumnRect(Index);
  if Result.Empty then
    Exit;
  Result.X := Result.Right - Size;
  Result.Width := Size * 2;
end;

procedure THeaderBar.Render;
const
  Margin = -4;
var
  Column: THeaderColumn;
  State: TDrawState;
  R: TRectI;
  F: IFont;
  I: Integer;
begin
  inherited Render;
  State := [dsBackground];
  Theme.Select(State);
  Theme.DrawHeaderBar(ClientRect);
  State := [];
  Theme.Select(State);
  F := NewFont(Font);
  F.Color := F.Color.Lighten(0.4);
  for I := 0 to FColumns.Count - 1 do
  begin
    Column := FColumns[I];
    if not Column.Visible then
      Continue;
    R := GetColumnRect(I);
    if FHotTrack and (FHotIndex = I) then
      State := [dsHot]
    else
      State := [];
    if Column = Selected then
      Include(State, dsSelected);
    if I = FDownIndex then
      Include(State, dsPressed);
    Theme.Select(State);
    Theme.DrawHeaderBar(R);
    R.Inflate(Margin, 0);
    Dec(R.Width);
    Surface.TextOut(F, Column.Caption, R, AlignDir[Column.Alignment]);
  end;
end;

procedure THeaderBar.ColumnClick(Column: THeaderColumn);
begin
  if Assigned(FOnColumnClick) then
    FOnColumnClick(Self, Column);
end;

procedure THeaderBar.ColumnResize(Column: THeaderColumn);
begin
  if Assigned(FOnColumnResize) then
    FOnColumnResize(Self, Column);
end;

procedure THeaderBar.ColumnSelect(Column: THeaderColumn);
begin
  if Assigned(FOnColumnSelect) then
    FOnColumnSelect(Self, Column);
end;

procedure THeaderBar.MouseLeave;
begin
  if FHotTrack and (FHotIndex > -1) then
    Invalidate;
  FHotIndex := -1;
  inherited MouseLeave;
end;

procedure THeaderBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  R: TRectI;
  I: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    FSizeIndex := -1;
    FDownIndex := -1;
    for I := 0 to FColumns.Count - 1 do
    begin
      R := GetSizingRect(I);
      if R.Contains(X, Y) then
      begin
        FSizeIndex := I;
        Exit;
      end;
    end;
    for I := 0 to FColumns.Count - 1 do
    begin
      R := GetColumnRect(I);
      if R.Contains(X, Y) then
      begin
        FDownIndex := I;
        Invalidate;
        Selected := FColumns[I];
        Exit;
      end;
    end;
  end;
end;

procedure THeaderBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  R: TRectI;
  I: Integer;
begin
  inherited MouseMove(Shift, X, Y);
  if FSizeIndex > -1 then
  begin
    R := GetColumnRect(FSizeIndex);
    FColumns[FSizeIndex].Width := X - R.X;
    Exit;
  end;
  if FDownIndex > -1 then
  begin
    Exit;
  end;
  for I := 0 to FColumns.Count - 1 do
  begin
    R := GetSizingRect(I);
    if R.Contains(X, Y) then
    begin
      if FHotIndex > -1 then
      begin
        FHotIndex := -1;
        Invalidate;
      end;
      if Cursor <> crHSplit then
      begin
        FPriorCursor := Cursor;
        Cursor := crHSplit;
      end;
      Exit;
    end;
  end;
  if Cursor = crHSplit then
    Cursor := FPriorCursor;
  if FHotTrack then
  begin
    for I := 0 to FColumns.Count - 1 do
    begin
      R := GetColumnRect(I);
      if R.Contains(X, Y) then
      begin
        if FHotIndex <> I then
          Invalidate;
        FHotIndex := I;
        Exit;
      end;
    end;
    if FHotIndex > -1 then
      Invalidate;
    FHotIndex := -1;
  end;
end;

procedure THeaderBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  R: TRectI;
  I: Integer;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    FSizeIndex := -1;
    if FDownIndex > FColumns.Count - 1 then
    begin
      FDownIndex := -1;
      Invalidate;
    end;
    if FDownIndex > -1 then
    begin
      I := FDownIndex;
      FDownIndex := -1;
      Invalidate;
      R := GetColumnRect(I);
      if R.Contains(X, Y) then
        ColumnClick(FColumns[I]);
    end;
  end;
end;

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

procedure TRenderImage.UpdateImage;
begin
  FCopy.Free;
  FCopy := nil;
  Invalidate;
end;

function TRenderImage.GetComputeImage: TSurfaceBitmap;
begin
  if FSharedImage <> nil then
    Result := FSharedImage
  else
    Result := FImage;
end;

function TRenderImage.GetRenderArea: TRectI;
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
  Result := ComputeImage.Opacity;
end;

procedure TRenderImage.SetOpacity(Value: Byte);
begin
  ComputeImage.Opacity := Value;
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

procedure TRenderImage.SetSharedImage(Value: TSurfaceBitmap);
begin
  FSharedImage := Value;
  UpdateImage;
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
    PreferredWidth := ComputeImage.Width;
    PreferredHeight := ComputeImage.Height;
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
  ComputedStatus: TProgressStatus;
  Images: TImageStrip;
  Index: Integer;
  R: TRectI;
  F: IFont;
  S: string;
begin
  inherited Render;
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

end.

