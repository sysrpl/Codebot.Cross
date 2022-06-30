(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified March 2015                                 *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.controls.containers.txt> }
unit Codebot.Controls.Containers;

{$i codebot.inc}

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
    { Create a new spliter }
    constructor Create;
    { Assign the object from a source }
    procedure Assign(Source: TPersistent); override;
  published
    { The control can be resized with the mouse }
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    { The minumum control can be resized with the mouse }
    property MinSize: Integer read FMinSize write SetMinSize default 10;
    { The minumum remaining in the parent control }
    property MinRemain: Integer read FMinRemain write SetMinRemain default 10;
    { When visible is true child controls are arrannged to not occupy the splitter area }
    property Visible: Boolean read FVisible write SetVisible default True;
    { The splitter area when visiible }
    property Margin: Integer read FMargin write SetMargin default 4;
  end;

{ TPanelBackgroundKind determiens the kind of background }

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

  TSizingPanel = class(TRenderCustomControl)
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
    procedure Render; override;
    procedure Resize; override;
    function GetLogicalClientRect: TRect; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ArrangeControls;
  published
    { Background controls the background styling of a panel }
    property Background: TPanelBackground read FBackground write SetBackground default pbColor;
    { When background is set to pbImage this value is painted in the background and its
      edges are clamped }
    property Image: TSurfaceBitmap read FImage write SetImage;
    { Borders apply single a pixel border to an edge }
    property Borders: TEdges read FBorders write SetBorders;
    { Shadows apply drop shadow to and edge }
    property Shadows: TEdges read FShadows write SetShadows;
    { Padding indents the placement of aligned child controls }
    property Padding: TEdgeOffset read FPadding write SetPadding;
    { Splitter alows the panel to be resized using the mouse }
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
    property OnRender;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
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
  FSplitter.Free;
  FPadding.Free;
  FImage.Free;
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
  if FSplitter.Enabled then
  case Align of
    alTop, alBottom: SplitterSized(Height);
    alLeft, alRight: SplitterSized(Width);
  end;
end;

procedure TSizingPanel.Render;
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
  inherited Render;
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


end.

