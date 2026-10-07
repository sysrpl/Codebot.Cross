(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified March 2015                                 *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.controls.containers.txt> }
unit Codebot.Controls.Containers;

{$i ../codebot/codebot.inc}

interface

uses
  Classes, SysUtils, Graphics, Controls, Forms, LCLType,
  Codebot.System,
  Codebot.Controls,
  Codebot.Graphics,
  Codebot.Graphics.Types;

{ TSplitter allows containers to be drag resized
  See also
  <link Overview.Codebot.Controls.Containers.TSplitter, TSplitter members> }

type
  TSplitter = class(TChangeNotifier)
  private
    FEnabled: Boolean;
    FMinSize: Integer;
    FMinRemain: Integer;
    FVisible: Boolean;
    FMargin: Integer;
    procedure SetEnabled(Value: Boolean);
    procedure SetMinSize(Value: Integer);
    procedure SetMinRemain(Value: Integer);
    procedure SetVisible(Value: Boolean);
    procedure SetMargin(Value: Integer);
  public
    { Create a new splitter }
    constructor Create;
    { Assign the object from a source }
    procedure Assign(Source: TPersistent); override;
  published
    { The control can be resized with the mouse }
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    { The minimum size the control can be resized to with the mouse }
    property MinSize: Integer read FMinSize write SetMinSize default 10;
    { The minimum space remaining in the parent control }
    property MinRemain: Integer read FMinRemain write SetMinRemain default 10;
    { When visible is true child controls are arranged to not occupy the splitter area }
    property Visible: Boolean read FVisible write SetVisible default True;
    { The size of the splitter area when visible }
    property Margin: Integer read FMargin write SetMargin default 4;
  end;

{ TPanelBackground determines the kind of background }

  TPanelBackground = (
    { Solid color }
    pbColor,
    { Gradient header }
    pbToolbar,
    { Brush pattern }
    pbPattern,
    { Image with clamped edges }
    pbImage);

{ TSizingPanel holds controls, pads edges, allows mouse resizing, and paints backgrounds
  See also
  <link Overview.Codebot.Controls.Containers.TSizingPanel, TSizingPanel members> }

  TSizingPanel = class(TSurfaceCustomControl)
  private
    FBackground: TPanelBackground;
    FImage: TSurfaceBitmap;
    FBorders: TEdges;
    FPadding: TEdgeOffset;
    FShadows: TEdges;
    FSplitter: TSplitter;
    FPriorCursor: TCursor;
    FDragging: Boolean;
    procedure ImageChanged(Sender: TObject);
    procedure PaddingChanged(Sender: TObject);
    procedure SetBackground(Value: TPanelBackground);
    procedure SetBorders(Value: TEdges);
    procedure SetImage(Value: TSurfaceBitmap);
    procedure SetPadding(Value: TEdgeOffset);
    procedure SetShadows(Value: TEdges);
    procedure SplitterChanged(Sender: TObject);
    function SplitterArea: TRectI;
    procedure SetSplitter(Value: TSplitter);
    procedure SplitterSized(Size: Integer);
  protected
    procedure Draw; override;
    procedure Resize; override;
    function GetLogicalClientRect: TRect; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    { Create a new sizing panel }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Arrange child controls in a single row using Codebot.Controls.ArrangeControls }
    procedure ArrangeControls;
  published
    { Background controls the background styling of a panel }
    property Background: TPanelBackground read FBackground write SetBackground default pbColor;
    { When background is set to pbImage this value is painted in the background and its
      edges are clamped }
    property Image: TSurfaceBitmap read FImage write SetImage;
    { Borders apply a single pixel border to an edge }
    property Borders: TEdges read FBorders write SetBorders;
    { Shadows apply a drop shadow to an edge }
    property Shadows: TEdges read FShadows write SetShadows;
    { Padding indents the placement of aligned child controls }
    property Padding: TEdgeOffset read FPadding write SetPadding;
    { Splitter allows the panel to be resized using the mouse }
    property Splitter: TSplitter read FSplitter write SetSplitter;
    property Align;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property BidiMode;
    property BorderWidth;
    property BorderStyle;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Color;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseDockManager default True;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDraw;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnGetDockCaption;
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
    property OnUnDock;
  end;

{ TCaptionBox is a panel with a caption area along its top.

  The caption area is painted using the system caption colors. It uses the
  active caption colors while a control inside the box has the input focus
  and the inactive caption colors otherwise. Clicking the caption area gives
  the input focus to the first control inside the box, if it has one.

  When ShowClose is true a close button is shown at the right of the caption
  area, which hides the box when clicked. The close button is an icon from
  the Material Design Icons font. }

  TCaptionBox = class(TCustomControl)
  private
    FActive: Boolean;
    FCaptionHeight: Integer;
    FShowClose: Boolean;
    FCloseHot: Boolean;
    FCloseDown: Boolean;
    FOnClose: TNotifyEvent;
    function CaptionRect: TRect;
    function CloseRect: TRect;
    function CloseHit(X, Y: Integer): Boolean;
    procedure FocusChild;
    procedure SetActive(Value: Boolean);
    procedure SetCaptionHeight(Value: Integer);
    procedure SetCloseState(Hot, Down: Boolean);
    procedure SetShowClose(Value: Boolean);
  protected
    procedure AdjustClientRect(var ARect: TRect); override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure FontChanged(Sender: TObject); override;
    procedure TextChanged; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseLeave; override;
    procedure Paint; override;
  public
    { Create a new caption box }
    constructor Create(AOwner: TComponent); override;
    { Hide the box and invoke OnClose }
    procedure Close;
    { Active is true while a control inside the box has the input focus }
    property Active: Boolean read FActive;
  published
    { The height of the caption area }
    property CaptionHeight: Integer read FCaptionHeight write SetCaptionHeight default 28;
    { When ShowClose is true a button which hides the box is shown in the caption area }
    property ShowClose: Boolean read FShowClose write SetShowClose default False;
    { OnClose is invoked after the box is hidden by its close button }
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property Align;
    property Anchors;
    property BorderSpacing;
    property Caption;
    property ChildSizing;
    property Color;
    property Constraints;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnEnter;
    property OnExit;
    property OnResize;
  end;

implementation

{ TSplitter }

constructor TSplitter.Create;
begin
  inherited Create;
  FVisible := True;
  FMinSize := 10;
  FMinRemain := 10;
  FMargin:= 4;
end;

procedure TSplitter.Assign(Source: TPersistent);
var
  S: TSplitter;
begin
  if Source is TSplitter then
  begin
    S := Source as TSplitter;
    FEnabled := S.Enabled;
    FMinSize := S.MinSize;
    FMinRemain := S.MinRemain;
    FVisible := S.Visible;
    FMargin := S.Margin;
    Change;
  end
  else
    inherited Assign(Source);
end;

procedure TSplitter.SetEnabled(Value: Boolean);
begin
  if FEnabled = Value then Exit;
  FEnabled := Value;
  Change;
end;

procedure TSplitter.SetMinRemain(Value: Integer);
begin
  if Value < 10 then
    Value := 10;
  if FMinRemain = Value then Exit;
  FMinRemain := Value;
  Change;
end;

procedure TSplitter.SetMinSize(Value: Integer);
begin
  if Value < 10 then
    Value := 10;
  if FMinSize = Value then Exit;
  FMinSize := Value;
  Change;
end;

procedure TSplitter.SetVisible(Value: Boolean);
begin
  if FVisible = Value then Exit;
  FVisible := Value;
  Change;
end;

procedure TSplitter.SetMargin(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if FMargin = Value then Exit;
  FMargin := Value;
  Change;
end;

{ TSizingPanel }

constructor TSizingPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImage := TSurfaceBitmap.Create;
  FImage.OnChange := ImageChanged;
  FPadding := TEdgeOffset.Create;
  FPadding.OnChange.Add(PaddingChanged);
  FSplitter := TSplitter.Create;
  FSplitter.OnChange.Add(SplitterChanged);
  ControlStyle := ControlStyle + [csAcceptsControls];
  Color := clBtnFace;
  Width := 160;
  Height := 160;
end;

destructor TSizingPanel.Destroy;
begin
  { The inherited destructor can still resize and realign the panel, so the
    fields are set to nil when freed }
  FreeAndNil(FSplitter);
  FreeAndNil(FPadding);
  FreeAndNil(FImage);
  inherited Destroy;
end;

procedure TSizingPanel.ArrangeControls;
begin
  Codebot.Controls.ArrangeControls(Self, ClientRect, Padding.Left);
end;

procedure TSizingPanel.SplitterSized(Size: Integer);
begin
  if Parent = nil then
    Exit;
  if FSplitter.Enabled then
  case Align of
    alTop, alBottom:
    begin
      if Parent.ClientHeight - Size < FSplitter.MinRemain then
        Size := Parent.ClientHeight - FSplitter.MinRemain;
      if Size < FSplitter.MinSize then
        Size := FSplitter.MinSize;
      Height := Size;
    end;
    alLeft, alRight:
    begin
      if Parent.ClientWidth - Size < FSplitter.MinRemain then
        Size := Parent.ClientWidth - FSplitter.MinRemain;
      if Size < FSplitter.MinSize then
        Size := FSplitter.MinSize;
      Width := Size;
    end;
  end;
end;

procedure TSizingPanel.SplitterChanged(Sender: TObject);
begin
  if FSplitter.Enabled then
  case Align of
    alTop, alBottom: SplitterSized(Height);
    alLeft, alRight: SplitterSized(Width);
  end;
end;

procedure TSizingPanel.PaddingChanged(Sender: TObject);
begin
  ReAlign;
  Invalidate;
end;

procedure TSizingPanel.ImageChanged(Sender: TObject);
begin
  if FBackground = pbImage then
    Invalidate;
end;

procedure TSizingPanel.SetBackground(Value: TPanelBackground);
begin
  if FBackground = Value then Exit;
  FBackground := Value;
  Invalidate;
end;

procedure TSizingPanel.SetBorders(Value: TEdges);
begin
  if FBorders = Value then Exit;
  FBorders := Value;
  ReAlign;
  Invalidate;
end;

procedure TSizingPanel.SetImage(Value: TSurfaceBitmap);
begin
  if FImage = Value then Exit;
  FImage.Assign(Value);
end;

procedure TSizingPanel.SetPadding(Value: TEdgeOffset);
begin
  if FPadding = Value then Exit;
  FPadding.Assign(Value);
end;

procedure TSizingPanel.SetShadows(Value: TEdges);
begin
  if FShadows = Value then Exit;
  FShadows := Value;
  Invalidate;
end;

procedure TSizingPanel.Resize;
begin
  inherited Resize;
  { Resize can be called by the inherited constructor before the splitter
    is created, or by the inherited destructor after it is freed }
  if FSplitter = nil then
    Exit;
  if FSplitter.Enabled then
  case Align of
    alTop, alBottom: SplitterSized(Height);
    alLeft, alRight: SplitterSized(Width);
  end;
end;

procedure TSizingPanel.Draw;
const
  Pad = 1;
var
  R: TRectI;
  A, B: TRectI;
begin
  R := ClientRect;
  if (FBackground = pbImage) and (not FImage.Empty) then
  begin
    Surface.FillRect(NewBrush(CurrentColor), R);
    FImage.Draw(Surface, 0, 0);
    if FImage.Width < R.Width then
    begin
      A := FImage.ClientRect;
      A.X := A.Width - 1;
      A.Width := 1;
      B := R;
      B.X := FImage.Width;
      B.Height := FImage.Height;
      FImage.Draw(Surface, A, B);
    end;
    if FImage.Height < R.Height then
    begin
      A := FImage.ClientRect;
      A.Y := A.Height - 1;
      A.Height := 1;
      B := R;
      B.Y := FImage.Height;
      B.Width := FImage.Width;
      FImage.Draw(Surface, A, B);
    end;
    if (FImage.Width < R.Width) or (FImage.Height < R.Height) then
    begin
      A := FImage.ClientRect;
      A.X := A.Width - 1;
      A.Y := A.Height - 1;
      A.Width := 1;
      A.Height := 1;
      B := R;
      B.Left := FImage.Width;
      B.Top := FImage.Height;
      FImage.Draw(Surface, A, B);
    end;
  end
  else if FBackground = pbToolbar then
  begin
    Surface.FillRect(NewBrush(CurrentColor), R);
    Theme.DrawHeader(Height);
  end
  else if FBackground = pbPattern then
    Surface.FillRect(Brushes.Transparent, R)
  else
    Surface.FillRect(NewBrush(CurrentColor), R);
  inherited Draw;
  R.Inflate(Pad, Pad);
  if BorderStyle = bsNone then
  begin
    if edLeft in FBorders then
      R.Left := R.Left + Pad;
    if edTop in FBorders then
      R.Top := R.Top + Pad;
    if edRight in FBorders then
      R.Right := R.Right - Pad;
    if edBottom in FBorders then
      R.Bottom := R.Bottom - Pad;
    Surface.StrokeRect(NewPen(CurrentColor.Darken(0.2)), R);
  end;
  R.Inflate(-Pad, -Pad);
  if edLeft in FShadows then
    DrawShadow(Surface, R, drLeft);
  if edTop in FShadows then
    DrawShadow(Surface, R, drUp);
  if edRight in FShadows then
    DrawShadow(Surface, R, drRight);
  if edBottom in FShadows then
    DrawShadow(Surface, R, drDown);
end;

procedure TSizingPanel.SetSplitter(Value: TSplitter);
begin
  if FSplitter = Value then Exit;
  FSplitter.Assign(Value);
end;

function TSizingPanel.SplitterArea: TRectI;
var
  R: TRectI;
begin
  R := ClientRect;
  if FSplitter.Enabled then
  case Align of
    alTop: R.Top := R.Bottom - FSplitter.Margin;
    alBottom: R.Bottom := R.Top + FSplitter.Margin;
    alLeft: R.Left := R.Right - FSplitter.Margin;
    alRight: R.Right := R.Left + FSplitter.Margin;
  end
  else
  begin
    R.Width := 0;
    R.Height := 0;
  end;
  Result := R;
end;

function TSizingPanel.GetLogicalClientRect: TRect;
var
  R: TRectI;
  M: Integer;
begin
  R := ClientRect;
  { The client rect is used while aligning, which can happen before the
    splitter and padding are created or after they are freed }
  if (FSplitter = nil) or (FPadding = nil) then
    Exit(R);
  M := FSplitter.Margin;
  if not FSplitter.Visible then
    M := 0;
  if FSplitter.Enabled then
  case Align of
    alTop: R.Bottom := R.Bottom - M;
    alBottom: R.Top := R.Top + M;
    alLeft: R.Right := R.Right - M;
    alRight: R.Left := R.Left + M;
  end;
  if FPadding.Left > R.X then
    R.X := FPadding.Left;
  if FPadding.Top > R.Y then
    R.Y := FPadding.Top;
  if R.Right > ClientWidth - FPadding.Right then
    R.Right := ClientWidth - FPadding.Right;
  if R.Bottom > ClientHeight - FPadding.Bottom then
    R.Bottom := ClientHeight - FPadding.Bottom;
  if BorderStyle = bsNone then
  begin
    if (R.Left = 0) and (edLeft in FBorders) then
      R.Left := 1;
    if (R.Top = 0) and (edTop in FBorders) then
      R.Top := 1;
    if (R.Right = Width) and (edRight in FBorders) then
      R.Right := R.Right - 1;
    if (R.Bottom = Height) and (edBottom in FBorders) then
      R.Bottom := R.Bottom - 1;
  end;
  Result := R;
end;

procedure TSizingPanel.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  R: TRectI;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    R := SplitterArea;
    FDragging := R.Contains(X, Y);
  end;
end;

procedure TSizingPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  R: TRectI;
begin
  inherited MouseMove(Shift, X, Y);
  R := SplitterArea;
  if (Cursor <> crHSplit) and (Cursor <> crVSplit) then
    FPriorCursor := Cursor;
  if R.Contains(X, Y) then
    if Align in [alLeft, alRight] then
      Cursor := crHSplit
    else
      Cursor := crVSplit
  else
    Cursor := FPriorCursor;
  if FDragging and FSplitter.Enabled  then
  begin
    case Align of
      alRight: X := ClientWidth - X;
      alBottom: Y := ClientHeight - Y;
    end;
    case Align of
      alTop, alBottom: SplitterSized(Y);
      alLeft, alRight: SplitterSized(X);
    end;
  end;
end;

procedure TSizingPanel.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Button = mbLeft then
    FDragging := False;
end;

{ TCaptionBox }

{ The close icon is close $F0156 from the Material Design Icons font encoded
  as utf8 }

const
  CloseFont = 'Material Design Icons';
  CloseGlyph = #$F3#$B0#$85#$96;
  CaptionMargin = 8;

constructor TCaptionBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls];
  FCaptionHeight := 28;
  Width := 200;
  Height := 150;
end;

procedure TCaptionBox.Close;
begin
  Hide;
  if Assigned(FOnClose) then
    FOnClose(Self);
end;

function TCaptionBox.CaptionRect: TRect;
begin
  Result := ClientRect;
  Result.Bottom := Result.Top + FCaptionHeight;
end;

function TCaptionBox.CloseRect: TRect;
begin
  Result := CaptionRect;
  Result.Left := Result.Right - FCaptionHeight;
end;

function TCaptionBox.CloseHit(X, Y: Integer): Boolean;
var
  R: TRect;
begin
  R := CloseRect;
  Result := FShowClose and (X >= R.Left) and (X < R.Right) and (Y >= R.Top) and
    (Y < R.Bottom);
end;

{ Child controls are placed below the caption area and inside the border }

procedure TCaptionBox.AdjustClientRect(var ARect: TRect);
begin
  inherited AdjustClientRect(ARect);
  Inc(ARect.Top, FCaptionHeight);
  Inc(ARect.Left);
  Dec(ARect.Right);
  Dec(ARect.Bottom);
end;

{ DoEnter and DoExit are called when the input focus moves into or out of the
  box, including when it moves to or from a control inside the box }

procedure TCaptionBox.DoEnter;
begin
  inherited DoEnter;
  SetActive(True);
end;

procedure TCaptionBox.DoExit;
begin
  inherited DoExit;
  SetActive(False);
end;

procedure TCaptionBox.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);
  Invalidate;
end;

procedure TCaptionBox.TextChanged;
begin
  inherited TextChanged;
  Invalidate;
end;

{ The focus is given to the first control in tab order which can take it }

procedure TCaptionBox.FocusChild;
var
  List: TFPList;
  Control: TWinControl;
  I: Integer;
begin
  List := TFPList.Create;
  try
    GetTabOrderList(List);
    for I := 0 to List.Count - 1 do
    begin
      Control := TWinControl(List[I]);
      if Control.CanFocus then
      begin
        Control.SetFocus;
        Break;
      end;
    end;
  finally
    List.Free;
  end;
end;

procedure TCaptionBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Button = mbLeft then
    if CloseHit(X, Y) then
      SetCloseState(True, True)
    else if (Y < FCaptionHeight) and (not FActive) then
      FocusChild;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCaptionBox.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  SetCloseState(CloseHit(X, Y), FCloseDown);
  inherited MouseMove(Shift, X, Y);
end;

procedure TCaptionBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Clicked: Boolean;
begin
  if Button = mbLeft then
  begin
    Clicked := FCloseDown and CloseHit(X, Y);
    SetCloseState(FCloseHot, False);
    if Clicked then
      Close;
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TCaptionBox.MouseLeave;
begin
  SetCloseState(False, FCloseDown);
  inherited MouseLeave;
end;

{ MixColors returns color A blended with an amount of color B from 0 to 255 }

function MixColors(A, B: TColor; Amount: Integer): TColor;
begin
  A := ColorToRGB(A);
  B := ColorToRGB(B);
  Result := RGBToColor(
    (Red(A) * (255 - Amount) + Red(B) * Amount) div 255,
    (Green(A) * (255 - Amount) + Green(B) * Amount) div 255,
    (Blue(A) * (255 - Amount) + Blue(B) * Amount) div 255);
end;

procedure TCaptionBox.Paint;
var
  Body, Back, Fore: TColor;
  Style: TTextStyle;
  R: TRect;
begin
  Body := GetRGBColorResolvingParent;
  if FActive then
  begin
    Back := clActiveCaption;
    Fore := clCaptionText;
  end
  else
  begin
    Back := clInactiveCaption;
    Fore := clInactiveCaptionText;
  end;
  { Some themes use the color of a window for a caption, most often for an
    inactive one. The caption area would then not be seen, so in that case
    its color is shifted towards the color of its text. }
  if ColorToRGB(Back) = Body then
    Back := MixColors(Back, Fore, 48);
  { The body and a border in the caption color }
  R := ClientRect;
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Body;
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Color := Back;
  Canvas.Rectangle(R);
  { The caption area }
  R := CaptionRect;
  Canvas.Brush.Color := Back;
  Canvas.FillRect(R);
  Style := Canvas.TextStyle;
  Style.SingleLine := True;
  Style.Clipping := True;
  Style.Layout := tlCenter;
  Canvas.Font := Font;
  Canvas.Font.Color := Fore;
  Canvas.Brush.Style := bsClear;
  Inc(R.Left, CaptionMargin);
  if FShowClose then
    R.Right := CloseRect.Left
  else
    Dec(R.Right, CaptionMargin);
  Style.Alignment := taLeftJustify;
  Style.EndEllipsis := True;
  Canvas.TextRect(R, R.Left, R.Top, Caption, Style);
  if not FShowClose then
    Exit;
  { The close button swaps the caption colors while it is pressed and is
    framed while the mouse is over it }
  R := CloseRect;
  Inc(R.Left, 3);
  Inc(R.Top, 3);
  Dec(R.Right, 3);
  Dec(R.Bottom, 3);
  if FCloseHot and FCloseDown then
  begin
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := Fore;
    Canvas.FillRect(R);
    Canvas.Font.Color := Back;
  end
  else if FCloseHot then
  begin
    Canvas.Pen.Color := Fore;
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle(R);
  end;
  Canvas.Brush.Style := bsClear;
  Canvas.Font.Name := CloseFont;
  Canvas.Font.Style := [];
  Canvas.Font.Height := -((R.Bottom - R.Top) * 3 div 4);
  Style.Alignment := taCenter;
  Style.EndEllipsis := False;
  Canvas.TextRect(R, R.Left, R.Top, CloseGlyph, Style);
end;

procedure TCaptionBox.SetActive(Value: Boolean);
begin
  if Value = FActive then
    Exit;
  FActive := Value;
  Invalidate;
end;

procedure TCaptionBox.SetCaptionHeight(Value: Integer);
begin
  if Value < 8 then
    Value := 8;
  if Value = FCaptionHeight then
    Exit;
  FCaptionHeight := Value;
  { Aligned child controls are arranged again below the resized caption area }
  DoAdjustClientRectChange;
  ReAlign;
  Invalidate;
end;

procedure TCaptionBox.SetCloseState(Hot, Down: Boolean);
begin
  if (Hot = FCloseHot) and (Down = FCloseDown) then
    Exit;
  FCloseHot := Hot;
  FCloseDown := Down;
  Invalidate;
end;

procedure TCaptionBox.SetShowClose(Value: Boolean);
begin
  if Value = FShowClose then
    Exit;
  FShowClose := Value;
  Invalidate;
end;

end.
