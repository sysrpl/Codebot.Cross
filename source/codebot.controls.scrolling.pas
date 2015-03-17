(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified March 2015                                 *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.controls.scrolling.txt> }
unit Codebot.Controls.Scrolling;

{$i codebot.inc}

interface

uses
  Classes, SysUtils, Graphics, Controls, Forms,
  LCLType, LCLIntf, LMessages,
  Codebot.System,
  Codebot.Graphics,
  Codebot.Graphics.Types,
  Codebot.Controls;

{ TControlHintWindow }

type
  TControlHintWindow = class(THintWindow)
  private
    FActive: Boolean;
    FControl: TControl;
    FPoint: TPointI;
    procedure SetActive(Value: Boolean);
    procedure SetControl(Value: TControl);
    procedure SetPoint(const Value: TPointI);
  public
    constructor Create(AOwner: TComponent); override;
    procedure ActivateHint(Rect: TRect; const AHint: string); override;
    property Active: Boolean read FActive write SetActive;
    property Control: TControl read FControl write SetControl;
    property Point: TPointI read FPoint write SetPoint;
  end;

  TScrollDir = (sdNone, sdUp, sdDown);

{ TScrollList }

  TScrollList = class(TRenderCustomControl)
  private
    FCount: Integer;
    FDownIndex: Integer;
    FHeaderSize: Integer;
    FHotTrack: Boolean;
    FHotIndex: Integer;
    FHintWindow: TControlHintWindow;
    FInsideRect: Boolean;
    FItemHeight: Integer;
    FItemIndex: Integer;
    FLocked: Boolean;
    FLockedIndex: Integer;
    FOnScrollLeft: TNotifyEvent;
    FScrollWidth: Integer;
    FScrollLeft: Integer;
    FTopIndex: Integer;
    FScrolling: Boolean;
    // TODO: Report or fix the MouseCapture bug
    // Bug with Gtk3 scrollbars causes MouseCapture to fail
    // Workaround uses private field to fix
    FMouseCapture: Boolean;
    FMouseDisabled: Boolean;
    FMultiSelect: Boolean;
    FSelectCount: Integer;
    FSelectItems: BoolArray;
    FShift: TShiftState;
    FShiftIndex: Integer;
    FOnSelectItem: TNotifyEvent;
    procedure SetCount(Value: Integer);
    procedure SetHeaderSize(Value: Integer);
    procedure SetHotTrack(Value: Boolean);
    procedure SetMouseDisabled(Value: Boolean);
    procedure SetMultiSelect(Value: Boolean);
    procedure SetItemHeight(Value: Integer);
    procedure SetScrollIndex(Value: Integer);
    procedure SetItemIndex(Value: Integer);
    procedure SetScrolling(Value: Boolean);
    function GetSelected(Index: Integer): Boolean;
    procedure SetScrollLeft(Value: Integer);
    procedure SetScrollWidth(Value: Integer);
    procedure SetSelected(Index: Integer; Value: Boolean);
    procedure SetTopIndex(Value: Integer);
    procedure WMCaptureChanged(var Message: TLMNoParams); message LM_CAPTURECHANGED;
    procedure WMGetDlgCode(var Message: TLMNoParams); message LM_GETDLGCODE;
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
    procedure WMTimer(var Message: TLMTimer); message LM_TIMER;
    procedure WMVScroll(var Message: TLMScroll); message LM_VSCROLL;
    procedure WMHScroll(var Message: TLMScroll); message LM_HSCROLL;
    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;
  protected
    procedure CreateHandle; override;
    procedure CaptureMove(X, Y: Integer); virtual;
    procedure KeyPress(var Key: Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
     X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
     X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave; override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure DoScrollLeft; virtual;
    procedure Render; override;
    procedure Resize; override;
    procedure DrawBackground; virtual;
    procedure DrawItem(Index: Integer; var Rect: TRectI; State: TDrawState); virtual;
    procedure InvalidateItem(Item: Integer);
    procedure UpdateScrollRange;
    procedure Scroll(Delta: Integer); virtual;
    procedure ScrollMove(Distance: Integer; Direction: TScrollDir); virtual;
    procedure SelectItem(PriorIndex: Integer; NewIndex: Integer; var CanSelect: Boolean); virtual;
    property DownIndex: Integer read FDownIndex;
    property Count: Integer read FCount write SetCount;
    property HintWindow: TControlHintWindow read FHintWindow;
    property HotIndex: Integer read FHotIndex;
    property HotTrack: Boolean read FHotTrack write SetHotTrack;
    property MouseDisabled: Boolean read FMouseDisabled write SetMouseDisabled;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect;
    property InsideRect: Boolean read FInsideRect write FInsideRect;
    property ItemHeight: Integer read FItemHeight write SetItemHeight default 16;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    property Locked: Boolean read FLocked write FLocked;
    property Scrolling: Boolean read FScrolling write SetScrolling;
    property Selected[Index: Integer]: Boolean read GetSelected write SetSelected;
    property SelectCount: Integer read FSelectCount;
    property TopIndex: Integer read FTopIndex write SetTopIndex;
    property OnSelectItem: TNotifyEvent read FOnSelectItem write FOnSelectItem;
    property OnScrollLeft: TNotifyEvent read FOnScrollLeft write FOnScrollLeft;
  public
    constructor Create(AOwner: TComponent); override;
    function ItemRect(Item: Integer): TRectI;
    function ItemAtPos(const Pos: TPointI; Existing: Boolean = False): Integer;
    procedure ScrollBy(DeltaX, DeltaY: Integer); override;
    procedure InsureItemVisible;
    procedure Select;
    procedure ScrollToSelection;
    property HeaderSize: Integer read FHeaderSize write SetHeaderSize;
    property ScrollWidth: Integer read FScrollWidth write SetScrollWidth;
    property ScrollLeft: Integer read FScrollLeft write SetScrollLeft;
  end;

{ TCustomDrawList }

  TCustomDrawList = class(TScrollList)
  private
    FAutoScroll: Boolean;
    FOnDrawBackground: TNotifyEvent;
    FOnDrawItem: TDrawIndexEvent;
    procedure SetAutoScroll(Value: Boolean);
  protected
    procedure Render; override;
    procedure DrawBackground; override;
    procedure DrawItem(Index: Integer; var Rect: TRectI;
      State: TDrawState); override;
    procedure Scroll(Delta: Integer); override;
    property AutoScroll: Boolean read FAutoScroll write SetAutoScroll;
    property OnDrawBackground: TNotifyEvent read FOnDrawBackground write FOnDrawBackground;
    property OnDrawItem: TDrawIndexEvent read FOnDrawItem write FOnDrawItem;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TDrawList }

  TDrawList = class(TCustomDrawList)
  public
    property Count;
    property MouseDisabled;
    property TopIndex;
    property ItemIndex;
    property Surface;
    property OnScrollLeft;
  published
    property Align;
    property Anchors;
    property BorderStyle;
    property Color;
    property DesktopFont;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property HotTrack;
    property ItemHeight;
    property MultiSelect;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property TabOrder;
    property TabStop;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawBackground;
    property OnDrawItem;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnSelectItem;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

{ TDrawTextList }

  TDrawTextList = class(TCustomDrawList)
  private
    FItems: TStrings;
    FAutoHeight: Boolean;
    procedure SetAutoHeight(Value: Boolean);
    procedure SetItems(Value: TStrings);
    procedure ItemsChange(Sender: TObject);
  protected
    procedure FontChanged(Sender: TObject); override;
    procedure DrawItem(Index: Integer; var Rect: TRectI;
      State: TDrawState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Surface;
    property Canvas;
    property MouseDisabled;
    property TopIndex;
    property ItemIndex;
    property SelectCount;
    property Selected;
  published
    property AutoHeight: Boolean read FAutoHeight write SetAutoHeight default True;
    property Items: TStrings read FItems write SetItems;
    property Align;
    property Anchors;
    property AutoScroll;
    property BorderStyle;
    property Color;
    property DesktopFont;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property HotTrack;
    property ItemHeight;
    property MultiSelect;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property TabOrder;
    property TabStop;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawBackground;
    property OnDrawItem;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnSelectItem;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

{ TControlHintWindow }

constructor TControlHintWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color := clWindow;
end;

procedure TControlHintWindow.ActivateHint(Rect: TRect; const AHint: string);
var
  R: TRectI;
begin
  FActive := True;
  R := Rect;
  if R.Empty then
  begin
    R := CalcHintRect(High(Integer), AHint, nil);
    if FControl <> nil then
      with FControl.ClientToScreen(FPoint) do
        R.Offset(X - 4, Y - 3)
    else
      R.Offset(FPoint);
  end;
  inherited ActivateHint(R, AHint);
end;

procedure TControlHintWindow.SetActive(Value: Boolean);
begin
  if Value <> FActive then
  begin
    FActive := Value;
    if FActive then
      ActivateHint(Rect(0, 0, 0, 0), Caption)
    else
      Hide;
  end;
end;

procedure TControlHintWindow.SetControl(Value: TControl);
begin
  FControl := Value;
  Active := False;
end;

procedure TControlHintWindow.SetPoint(const Value: TPointI);
begin
  if (Value.X <> FPoint.X) or (Value.Y <> FPoint.Y) then
  begin
    FPoint := Value;
    Active := False;
  end;
end;

{ TScrollList }

const
  ScrollTimer = $100;

constructor TScrollList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csDoubleClicks, csOpaque];
  BorderStyle := bsNone;
  FDownIndex := -1;
  FHotIndex := -1;
  FShiftIndex := -1;
  {FHintWindow := TControlHintWindow.Create(Self);
  FHintWindow.Control := Self;}
  FItemHeight := 15;
  FItemIndex := -1;
  ParentColor := False;
  ParentFont := True;
  Width := 100;
  Height := 200;
end;

procedure TScrollList.CreateHandle;
begin
  inherited CreateHandle;
  UpdateScrollRange;
end;

procedure TScrollList.WMCaptureChanged(var Message: TLMNoParams);
begin
  inherited;
  FScrolling := GetCapture = Handle;
end;

procedure TScrollList.WMGetDlgCode(var Message: TLMNoParams);
begin
  Message.Result := DLGC_WANTARROWS or DLGC_WANTCHARS;
end;

procedure TScrollList.WMSize(var Message: TLMSize);
begin
  inherited;
  UpdateScrollRange;
end;

procedure TScrollList.WMTimer(var Message: TLMTimer);
var
  Point: TPoint;
  ScrollDir: TScrollDir;
  Distance: Integer;
begin
  Message.Result := 0;
  if (Message.TimerID <> ScrollTimer) or FMouseDisabled then Exit;
  if FScrolling then
  begin
    Point.X := 0;
    Point.Y := 0;
    GetCursorPos(Point);
    LCLIntf.ScreenToClient(Handle, Point);
    ScrollDir := sdNone;
    Distance := 0;
    with Point do
      if Y < 0 then
      begin
        Distance := -Y div FItemHeight + 1;
        ScrollDir := sdUp;
      end
      else if Y > ClientHeight then
      begin
        Distance := (Y - ClientHeight) div FItemHeight + 1;
        ScrollDir := sdDown;
      end;
    if ScrollDir <> sdNone then
      ScrollMove(Distance, ScrollDir)
    else
    begin
      FScrolling := False;
      KillTimer(Handle, ScrollTimer);
    end;
  end
  else
    KillTimer(Handle, ScrollTimer);
end;

procedure TScrollList.WMVScroll(var Message: TLMScroll);
begin
  with Message do
    case ScrollCode of
      SB_BOTTOM: SetTopIndex(FCount - 1);
      SB_LINEDOWN: SetTopIndex(FTopIndex + 1);
      SB_LINEUP: SetTopIndex(FTopIndex - 1);
      SB_PAGEDOWN: SetTopIndex(FTopIndex + ClientHeight div FItemHeight);
      SB_PAGEUP: SetTopIndex(FTopIndex - ClientHeight div FItemHeight);
      SB_THUMBTRACK: SetTopIndex(Pos);
      SB_TOP: SetTopIndex(0);
    end;
end;

procedure TScrollList.WMHScroll(var Message: TLMScroll);
begin
  with Message do
    case ScrollCode of
      SB_BOTTOM: SetScrollLeft(FScrollWidth);
      SB_LINEDOWN: SetScrollLeft(FScrollLeft + 10);
      SB_LINEUP: SetScrollLeft(FScrollLeft - 10);
      SB_PAGEDOWN: SetScrollLeft(FScrollLeft + ClientWidth);
      SB_PAGEUP: SetScrollLeft(FScrollLeft - ClientWidth);
      SB_THUMBTRACK: SetScrollLeft(Pos);
      SB_TOP: SetScrollLeft(0);
    end;
end;

procedure TScrollList.WMSetFocus(var Message: TLMSetFocus);
begin
  inherited;
  Invalidate;
end;

procedure TScrollList.WMKillFocus(var Message: TLMKillFocus);
begin
  inherited;
  Invalidate;
end;

procedure TScrollList.CaptureMove(X, Y: Integer);
var
  I: Integer;
begin
  // Bug with Gtk3 scrollbars causes MouseCapture to fail
  if FMouseCapture then
  begin
    I := ItemAtPos(Point(X, Y));
    if I < 0 then Exit;
    if (Y < 0) or (Y > ClientHeight) then
      Scrolling := True
    else
      SetScrollIndex(I);
  end;
end;

procedure TScrollList.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  ScrollToSelection;
end;

procedure TScrollList.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  FShift := Shift;
  case Key of
    VK_HOME: ItemIndex := 0;
    VK_END: ItemIndex := Count - 1;
    VK_NEXT: SetScrollIndex(ItemIndex + ClientHeight div FItemHeight);
    VK_PRIOR: SetScrollIndex(ItemIndex - ClientHeight div FItemHeight);
    VK_UP: SetScrollIndex(ItemIndex - 1);
    VK_DOWN: SetScrollIndex(ItemIndex + 1);
  end;
  InsureItemVisible;
end;

procedure TScrollList.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);

  function CanFocus: Boolean;
  var
    W: TWinControl;
  begin
    W := Self;
    while W <> nil do
      if (not W.Visible) or (not W.Enabled) then
        Exit(False)
      else
        W := W.Parent;
    Result := True;
  end;

begin
  FShift := Shift;
  if FHintWindow <> nil then
    FHintWindow.Active := False;
  if Button = mbLeft then
  begin
    // Bug with Gtk3 scrollbars causes MouseCapture to fail
    FMouseCapture := True;
    CaptureMove(X, Y);
    if CanFocus then
      SetFocus;
    FDownIndex := ItemAtPos(Point(X, Y), True);
    if FDownIndex > -1 then
    begin
      if (FItemIndex > -1) and (FItemIndex <> FDownIndex) then
        InvalidateItem(FItemIndex);
      FItemIndex := FDownIndex;
      if HotTrack then
        FHotIndex := FDownIndex;
      InvalidateItem(FDownIndex);
    end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TScrollList.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  PriorIndex: Integer;
  P: TPoint;
  I: Integer;
begin
  if Button = mbLeft then
  begin
    FScrolling := False;
    KillTimer(Handle, ScrollTimer);
    if FDownIndex > -1 then
    begin
      PriorIndex := FDownIndex;
      FDownIndex := -1;
      if PriorIndex > -1 then
        InvalidateItem(PriorIndex);
    end;
    if FHotTrack then
    begin
      P := Point(X, Y);
      I := ItemAtPos(P, False);
      if I <> FHotIndex then
        InvalidateItem(FHotIndex);
      FHotIndex := I;
      if FHotIndex > -1 then
      begin
        InvalidateItem(FHotIndex);
        if not PtInRect(ItemRect(FHotIndex), P) then
          FHotIndex := -1;
      end;;
    end;
    InsureItemVisible;
    // Bug with Gtk3 scrollbars causes MouseCapture to fail
    FMouseCapture := False;
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TScrollList.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
begin
  inherited MouseMove(Shift, X, Y);
  { TODO:
  if FMultiSelect then
    FShift := KeyboardStateToShiftState - [ssCtrl];}
  if FHotTrack and (not FMouseDisabled) then
  begin
    I := ItemAtPos(Point(X, Y), True);
    if I <> FHotIndex then
    begin
      InvalidateItem(FHotIndex);
      FHotIndex := I;
      InvalidateItem(FHotIndex);
    end;
  end;
  if FMouseCapture and (not FMouseDisabled) then
    CaptureMove(X, Y);
end;

procedure TScrollList.MouseLeave;
var
  I: Integer;
begin
  inherited MouseLeave;
  if FHintWindow <> nil then
    FHintWindow.Active := False;
  if FHotIndex > -1 then
  begin
    I := FHotIndex;
    FHotIndex := -1;
    InvalidateItem(I);
  end;
end;

function TScrollList.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
const
  Last: TDateTime = 0;
  Delay = 1 / 24 / 60 / 60 / 50;
var
  N: TDateTime;
  I, Delta: Integer;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if Result then
    Exit;
  { TODO:
  if FMultiSelect then
    FShift := KeyboardStateToShiftState - [ssCtrl]; }
  N := Now;
  if N - Last < Delay then
    Exit;
  Last := N;
  if FHintWindow <> nil then
    HintWindow.Active := False;
  I := ItemIndex;
  if I < 0 then
    Exit;
  if WheelDelta > 0 then
    Delta := -1
    else
    Delta := 1;
  if I + Delta < 0 then
    Exit;
  ItemIndex := I + Delta;
  if ItemIndex = Count - 1 then
    SetScrollIndex(ItemIndex + 1);
  InsureItemVisible;
end;

procedure TScrollList.DoScrollLeft;
begin
  if Assigned(FOnScrollLeft) then
    FOnScrollLeft(Self);
end;

procedure TScrollList.Render;
var
  UpdateRect, R: TRectI;
  I: Integer;
begin
  DrawBackground;
  UpdateRect := Canvas.ClipRect;
  with UpdateRect do
  begin
    Left := 0;
    Right := ClientWidth;
    Top := (Top div FItemHeight) * FItemHeight;
    if Bottom mod FItemHeight > 0 then
      Bottom := Bottom + FItemHeight;
    Bottom := (Bottom div FItemHeight) * FItemHeight;
    for I := Top div FItemHeight to Bottom div FItemHeight do
    begin
      if I + FTopIndex > FCount - 1 then
        Break;
      Top := I * FItemHeight;
      Bottom := Top + FItemHeight;
      FDrawState := [];
      if Focused then
        Include(FDrawState, dsFocused);
      if FTopIndex + I = ItemIndex then
      begin
        if FMultiSelect then
          Include(FDrawState, dsDefaulted)
        else
          Include(FDrawState, dsSelected);
      end;
      if FMultiSelect and FSelectItems[FTopIndex + I] then
        Include(FDrawState, dsSelected);
      if FTopIndex + I = FHotIndex then
        Include(FDrawState, dsHot);
      if FTopIndex + I = FDownIndex then
        Include(FDrawState, dsPressed);
      R := UpdateRect;
      if FScrollWidth > 0 then
        R.Width := FScrollWidth;
      R.X := -FScrollLeft;
      R.Y := R.Y + FHeaderSize;
      DrawItem(FTopIndex + I, R, FDrawState);
    end;
    Top := Bottom;
  end;
end;

procedure TScrollList.Resize;
begin
  inherited Resize;
  if FScrollWidth - FScrollLeft  < ClientWidth then
  begin
    ScrollLeft := FScrollWidth - ClientWidth;
    Invalidate;
    UpdateScrollRange;
  end
end;

procedure TScrollList.DrawBackground;
begin
end;

procedure TScrollList.DrawItem(Index: Integer; var Rect: TRectI;
  State: TDrawState);
begin
end;

procedure TScrollList.InvalidateItem(Item: Integer);
var
  Rect: TRect;
begin
  if Item > -1 then
    if HandleAllocated then
      if DoubleBuffered then
        Invalidate
      else
      begin
        Rect := ItemRect(Item);
        InvalidateRect(Handle, @Rect, True);
      end;
end;

function TScrollList.ItemRect(Item: Integer): TRectI;
begin
  Result := TRectI.Create(0, FHeaderSize + (Item - FTopIndex) * FItemHeight,
    ClientWidth, FItemHeight);
end;

procedure TScrollList.ScrollBy(DeltaX, DeltaY: Integer);
var
  R: TRect;
begin
  if DoubleBuffered then
    Invalidate
  else if HandleAllocated then
  begin
    R := ClientRect;
    R.Top := FHeaderSize;
    ScrollWindow(Handle, DeltaX, DeltaY, @R, @R);
    inherited ScrollBy(DeltaX, DeltaY);
  end
end;

procedure TScrollList.Scroll(Delta: Integer);
begin
  ScrollBy(0, Delta);
end;

procedure TScrollList.ScrollMove(Distance: Integer; Direction: TScrollDir);
const
  Movement: array[TScrollDir] of Integer = (0, -1, 1);
begin
  if Distance > 0 then
    SetScrollIndex(ItemIndex + Distance * Movement[Direction]);
end;

procedure TScrollList.SelectItem(PriorIndex: Integer; NewIndex: Integer;
  var CanSelect: Boolean);
begin
  if CanSelect then
  begin
    FItemIndex := NewIndex;
    if Assigned(FOnSelectItem) then
      FOnSelectItem(Self);
  end;
end;

procedure TScrollList.Select;
begin
  if Assigned(FOnSelectItem) then
    FOnSelectItem(Self);
end;

procedure TScrollList.ScrollToSelection;
begin
  if FItemIndex < FTopIndex then
    SetTopIndex(FItemIndex)
  else if FItemIndex >= FTopIndex + (ClientHeight + 1) div FItemHeight  then
    SetTopIndex(FItemIndex - (ClientHeight - 1) div FItemHeight);
end;

procedure TScrollList.InsureItemVisible;
begin
  if (ItemIndex < 0) or (TopIndex >= ItemIndex) then
    Exit;
  if ItemRect(ItemIndex).Bottom > ClientHeight then
    TopIndex := TopIndex + 1;
end;

function TScrollList.ItemAtPos(const Pos: TPointI;
  Existing: Boolean = False): Integer;
begin
  Result := FTopIndex + (Pos.Y - FHeaderSize) div FItemHeight;
  if Result > FCount - 1 then
    if Existing then Result := -1 else Result := FCount - 1;
  if FInsideRect and ((Pos.X < 0) or (Pos.X > ClientWidth - 1)) then
    Result := -1;
  if FLocked and (Result <> FLockedIndex) then
    Result := -1;
end;

procedure TScrollList.UpdateScrollRange;
var
  ScrollInfo: TScrollInfo;
begin
  if HandleAllocated then
  begin
    with ScrollInfo do
    begin
      cbSize := SizeOf(TScrollInfo);
      fMask := SIF_PAGE or SIF_POS or SIF_RANGE;
      nMin := 0;
      nMax := FCount - 1;
      nPage := (ClientHeight - FHeaderSize) div FItemHeight;
      nPos := FTopIndex;
      SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
      if FCount - FTopIndex < Integer(nPage) then SetTopIndex(FCount -
        Integer(nPage));
    end;
    with ScrollInfo do
    begin
      cbSize := SizeOf(TScrollInfo);
      fMask := SIF_PAGE or SIF_POS or SIF_RANGE;
      nMin := 0;
      nMax := FScrollWidth;
      nPage := ClientWidth;
      nPos := FScrollLeft;
      SetScrollInfo(Handle, SB_Horz, ScrollInfo, True);
    end;
  end;
end;

procedure TScrollList.SetCount(Value: Integer);
begin
  if Value <> FCount then
  begin
    if Value < 0 then
      Value := 0;
    FCount := Value;
    FSelectItems := nil;
    if FMultiSelect and (FCount > 0) then
      FSelectItems.Length := FCount;
    FItemIndex := -1;
    FHotIndex := -1;
    if FCount > 0 then
      ItemIndex := 0;
    UpdateScrollRange;
    Invalidate;
  end;
end;

procedure TScrollList.SetHeaderSize(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if FHeaderSize = Value then Exit;
  FHeaderSize := Value;
  UpdateScrollRange;
  Invalidate;
end;

procedure TScrollList.SetItemHeight(Value: Integer);
const
  MinHeight = 5;
var
  WasVisible: Boolean;
begin
  if Value < MinHeight then
    Value := MinHeight;
  if Value <> FItemHeight then
  begin
    FItemHeight := Value;
    WasVisible := Visible;
    Visible := False;
    UpdateScrollRange;
    Visible := WasVisible;
    Invalidate;
  end;
end;

procedure TScrollList.SetHotTrack(Value: Boolean);
begin
  if Value <> FHotTrack then
  begin
    FHotTrack := Value;
    InvalidateItem(FHotIndex);
    FHotIndex := -1;
  end;
end;

procedure TScrollList.SetMouseDisabled(Value: Boolean);
begin
  if Value <> FMouseDisabled then
  begin
    FMouseDisabled := Value;
    InvalidateItem(FHotIndex);
    FHotIndex := -1;
    KillTimer(Handle, ScrollTimer);
  end;
end;

procedure TScrollList.SetMultiSelect(Value: Boolean);
begin
  if Value <> FMultiSelect then
  begin
    FMultiSelect := Value;
    FSelectItems := nil;
    if FMultiSelect and (FCount > 0) then
    begin
      FSelectItems.Length := FCount;
      if FItemIndex > -1 then
        FSelectItems[FItemIndex] := True;
    end
    else
      FSelectCount := 0;
    Invalidate;
    FHotIndex := -1;
    KillTimer(Handle, ScrollTimer);
  end;
end;

procedure TScrollList.SetScrollIndex(Value: Integer);
begin
  if Count = 0 then
    SetItemIndex(-1)
  else if Value > Count - 1 then
  begin
    SetItemIndex(Count - 1);
    SetTopIndex(FTopIndex + 1);
  end
  else if Value < 0 then
    SetItemIndex(0)
  else
    SetItemIndex(Value);
end;

procedure TScrollList.SetItemIndex(Value: Integer);
var
  PriorIndex: Integer;
  CanSelect: Boolean;
  WasSelected: Boolean;
  I: Integer;
begin
  if FLocked then
    if Value > -1 then
      Value := FLockedIndex;
  // FDownIndex := -1;
  if Value > Count - 1 then
    Value := Count - 1;
  if Value > -1 then
    FLockedIndex := Value;
  if Value <> FItemIndex then
  begin
    PriorIndex := FItemIndex;
    if not HandleAllocated then
    begin
      FItemIndex := Value;
      Exit;
    end;
    CanSelect := True;
    SelectItem(FItemIndex, Value, CanSelect);
    if CanSelect then
    begin
      if PriorIndex > -1 then
        InvalidateItem(PriorIndex);
      FItemIndex := Value;
      ScrollToSelection;
      if PriorIndex <> FItemIndex then
      begin
        InvalidateItem(FItemIndex);
        if FMultiSelect and (FItemIndex > -1) then
          if ssShift in FShift then
          begin
            if FShiftIndex > -1 then
              WasSelected := FSelectItems[FShiftIndex]
            else
              WasSelected := False;
            for I := FSelectItems.Lo to FSelectItems.Hi do
              FSelectItems[I] := False;
            FSelectCount := 0;
            if FShiftIndex > -1 then
            begin
              if FItemIndex < FShiftIndex then
                for I := FShiftIndex - 1 downto FItemIndex do
                begin
                  FSelectItems[I] := True;
                  Inc(FSelectCount);
                end
                else for I := FShiftIndex + 1 to FItemIndex do
                begin
                  FSelectItems[I] := True;
                  Inc(FSelectCount);
                end;
              if WasSelected then
              begin
                FSelectItems[FShiftIndex] := True;
                Inc(FSelectCount);
              end;
              Invalidate;
            end
            else
            begin
              FSelectItems[FItemIndex] := True;
              FSelectCount := 1;
              FShiftIndex := FItemIndex;
              Invalidate;
            end;
          end
          else if ssCtrl in FShift then
          begin
            if FSelectItems[FItemIndex] then
              Dec(FSelectCount);
            FSelectItems[FItemIndex] := not FSelectItems[FItemIndex];
            if FSelectItems[FItemIndex] then
              Inc(FSelectCount);
            if FSelectCount > 1 then
              Invalidate;
          end
          else if (PriorIndex > -1) and (FSelectCount = 1) and FSelectItems[FItemIndex] then
          begin
            FSelectItems[PriorIndex] := False;
            FSelectItems[FItemIndex] := True;
          end
          else
          begin
            for I := FSelectItems.Lo to FSelectItems.Hi do
              FSelectItems[I] := False;
            FSelectCount := 1;
            FSelectItems[FItemIndex] := True;
            Invalidate;
          end;
      end;
      if not (ssShift in FShift) then
        FShiftIndex := FItemIndex;
    end;
  end
  else if FMultiSelect and (FItemIndex > -1) and (ssCtrl in FShift) then
  begin
    if FSelectItems[FItemIndex] then
      Dec(FSelectCount);
    FSelectItems[FItemIndex] := not FSelectItems[FItemIndex];
    if FSelectItems[FItemIndex] then
      Inc(FSelectCount);
    if FSelectCount > 1 then
      Invalidate
    else
      InvalidateItem(FItemIndex);
  end;
  FShift := FShift - [ssCtrl];
end;

procedure TScrollList.SetScrolling(Value: Boolean);
begin
  if Value <> FScrolling then
  begin
    FScrolling := Value;
    if FScrolling then
      SetTimer(Handle, ScrollTimer, 60, nil);
  end;
end;

function TScrollList.GetSelected(Index: Integer): Boolean;
begin
  Result := False;
  if (not FMultiSelect) or (Index < 0) or (Index > FCount -1) then
    Exit;
  Result := FSelectItems[Index];
end;

procedure TScrollList.SetScrollLeft(Value: Integer);
var
  Delta: Integer;
begin
  if Value < 0 then
    Value := 0;
  if Value > FScrollWidth then
    Value := FScrollWidth;
  if FScrollLeft = Value then Exit;
  Delta := FScrollLeft - Value;
  FScrollLeft := Value;
  ScrollBy(Delta, 0);
  UpdateScrollRange;
  DoScrollLeft;
end;

procedure TScrollList.SetScrollWidth(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if FScrollWidth = Value then Exit;
  FScrollWidth := Value;
  if FScrollLeft > FScrollWidth then
    FScrollLeft := FScrollWidth
  else if FScrollWidth - FScrollLeft  < ClientWidth then
  begin
    ScrollLeft := FScrollWidth - ClientWidth;
    Invalidate;
    UpdateScrollRange;
  end
  else
  begin
    Invalidate;
    UpdateScrollRange;
  end;
end;

procedure TScrollList.SetSelected(Index: Integer; Value: Boolean);
begin
  if (not FMultiSelect) or (Index < 0) or (Index > FCount -1) then
    Exit;
  if FSelectItems[Index] <> Value then
  begin
    FShift := [];
    if FSelectItems[Index] then
      Dec(FSelectCount);
    FSelectItems[Index] := Value;
    if FSelectItems[Index] then
      Inc(FSelectCount);
    Invalidate;
  end;
end;

procedure TScrollList.SetTopIndex(Value: Integer);
var
  ScrollInfo: TScrollInfo;
  Delta: Integer;
  P: TPoint;
begin
  if Value > FCount - (ClientHeight - FHeaderSize) div FItemHeight then
    Value := FCount - (ClientHeight - FHeaderSize) div FItemHeight;
  if Value < 0 then
    Value := 0;
  if Value <> FTopIndex then
  begin
    Delta := (FTopIndex - Value) * FItemHeight;
    FTopIndex := Value;
    if FHotTrack then
      if FHotIndex > - 1 then
        InvalidateItem(FHotIndex);
    FHotIndex := -1;
    ScrollInfo.cbSize := Sizeof(TScrollInfo);
    ScrollInfo.fMask := SIF_POS;
    ScrollInfo.nPos := FTopIndex;
    SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
    Scroll(Delta);
    InvalidateItem(FItemIndex);
    if FHotTrack then
    begin
      P := ScreenToClient(Mouse.CursorPos);
      FHotIndex := ItemAtPos(P, False);
      if FHotIndex > -1 then
        if PtInRect(ItemRect(FHotIndex), P) then
          InvalidateItem(FHotIndex)
        else
          FHotIndex := -1;
    end;
  end;
end;

{ TCustomDrawList }

constructor TCustomDrawList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoScroll := True;
end;

procedure TCustomDrawList.Render;
begin
  DrawBackground;
  inherited Render;
end;

procedure TCustomDrawList.DrawBackground;
begin
  if Assigned(FOnDrawBackground) then
    FOnDrawBackground(Self)
  else
    FillRectColor(Surface, ClientRect, CurrentColor);
end;

procedure TCustomDrawList.DrawItem(Index: Integer; var Rect: TRectI;
  State: TDrawState);
begin
  if Assigned(FOnDrawItem) then
    FOnDrawItem(Self, Surface, Index, Rect, State);
end;

procedure TCustomDrawList.Scroll(Delta: Integer);
begin
  if FAutoScroll then
    inherited Scroll(Delta)
  else
    Invalidate;
end;

procedure TCustomDrawList.SetAutoScroll(Value: Boolean);
begin
  if Value <> FAutoScroll then
    FAutoScroll := Value;
end;

{ TDrawTextList }

constructor TDrawTextList.Create(AOwner: TComponent);
var
  S: TStringList;
begin
  inherited Create(AOwner);
  FAutoScroll := False;
  FAutoHeight := True;
  S := TStringList.Create;
  S.Add('item one');
  S.Add('item two');
  S.Add('item three');
  S.OnChange := ItemsChange;
  FItems := S;
  ItemHeight := TextHeight + 2;
  ItemsChange(S);
end;

destructor TDrawTextList.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TDrawTextList.SetAutoHeight(Value: Boolean);
begin
  if Value <> FAutoHeight then
  begin
    FAutoHeight := Value;
    if FAutoHeight then
      ItemHeight := TextHeight + 2;
  end;
end;

procedure TDrawTextList.DrawItem(Index: Integer; var Rect: TRectI;
  State: TDrawState);
begin
  if not HotTrack then
    Exclude(State, dsHot);
  if Assigned(OnDrawItem) then
    inherited DrawItem(Index, Rect, State)
  else
    DrawTextState(Surface, FItems[Index], Rect, State);
end;

procedure TDrawTextList.ItemsChange(Sender: TObject);
begin
  Count := FItems.Count;
  Invalidate;
end;

procedure TDrawTextList.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);
  if FAutoHeight then
    FItemHeight := TextHeight + 2;
end;

procedure TDrawTextList.SetItems(Value: TStrings);
begin
  FItems.Assign(Value);
  Invalidate;
end;

end.

