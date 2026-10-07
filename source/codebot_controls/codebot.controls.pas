(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified March 2015                                 *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.controls.txt> }
unit Codebot.Controls;

{$i ../codebot/codebot.inc}

interface

uses
  Classes, SysUtils, Types, Graphics, Controls, Forms, LCLType, LCLIntf, LCLProc,
  Codebot.System,
  Codebot.Graphics,
  Codebot.Graphics.Types;

type
  { TItemUpdateEvent is invoked when an item in a collection changes }
  TItemUpdateEvent = procedure(Sender: TObject; Item: TCollectionItem) of object;
  { Item update event publisher }
  TItemUpdateDelegate = TDelegate<TItemUpdateEvent>;
  { Item update event subscriber }
  IItemUpdateDelegate = IDelegate<TItemUpdateEvent>;

  { TItemNotifyEvent is invoked when an item is added to or removed from a
    collection }
  TItemNotifyEvent = procedure(Sender: TObject; Item: TCollectionItem; Action: TCollectionNotification) of object;
  { Item notify event publisher }
  TItemNotifyDelegate = TDelegate<TItemNotifyEvent>;
  { Item notify event subscriber }
  IItemNotifyDelegate = IDelegate<TItemNotifyEvent>;

{ TNotifyCollection\<T\> simplifies creating specialized persistent collections
  See also
  <link Overview.Codebot.Controls.TNotifyCollection, TNotifyCollection\<T\> members> }

  TNotifyCollection<T: TCollectionItem> = class(TCollection)
  private
    FOwner: TPersistent;
    FOnItemNotify: TItemNotifyDelegate;
    FOnItemUpdate: TItemUpdateDelegate;
    function GetItem(const Index: Integer): T;
    procedure SetItem(const Index: Integer; const Value: T);
    function GetOnItemNotify: IItemNotifyDelegate;
    function GetOnItemUpdate: IItemUpdateDelegate;
  protected
    function GetOwner: TPersistent; override;
    { Invoke OnItemNotify subscribers }
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    { Invoke OnItemUpdate subscribers }
    procedure Update(Item: TCollectionItem); override;
  public
    { Create a collection owned by a persistent object }
    constructor Create(AOwner: TPersistent); virtual;
    destructor Destroy; override;
    { Copy the items of another collection }
    procedure Assign(Source: TPersistent); override;
    { Add a new item }
    function Add: T;
    { The object which owns the collection }
    property Owner: TPersistent read FOwner;
    { Items indexed by an integer }
    property Items[const Index: Integer]: T read GetItem write SetItem; default;
    { Subscribe to notifications when items are added or removed }
    property OnItemNotify: IItemNotifyDelegate read GetOnItemNotify;
    { Subscribe to notifications when items change }
    property OnItemUpdate: IItemUpdateDelegate read GetOnItemUpdate;
  end;

{ TEdgeOffset represents a padding or margin on a control
  See also
  <link Overview.Codebot.Controls.TEdgeOffset, TEdgeOffset members> }

  TEdgeOffset = class(TChangeNotifier)
  private
    FBottom: Integer;
    FLeft: Integer;
    FRight: Integer;
    FTop: Integer;
    procedure SetBottom(Value: Integer);
    procedure SetLeft(Value: Integer);
    procedure SetRight(Value: Integer);
    procedure SetTop(Value: Integer);
  public
    { Copy padding }
    procedure Assign(Source: TPersistent); override;
  published
    { Space on the left }
    property Left: Integer read FLeft write SetLeft default 0;
    { Space on the top }
    property Top: Integer read FTop write SetTop default 0;
    { Space on the right }
    property Right: Integer read FRight write SetRight default 0;
    { Space on the bottom }
    property Bottom: Integer read FBottom write SetBottom default 0;
  end;

{ TEdge represents information about the border of a control }

  TEdge = (edLeft, edTop, edRight, edBottom);

{ TEdges represents information about multiple borders on a control }

  TEdges = set of TEdge;

{ ESurfaceAccessError }

{doc off}
  ESurfaceAccessError = class(Exception);
{doc on}

{doc off}
var
  MouseEnters: Integer;
  MouseLeaves: Integer;
{doc on}

{ TSurfaceGraphicControl is the base class for custom graphic controls
  which require an ISurface object
  See also
  <link Overview.Codebot.Controls.TSurfaceGraphicControl, TSurfaceGraphicControl members> }

type
  TSurfaceGraphicControl = class(TGraphicControl, IFloatPropertyNotify)
  private
    FSurface: ISurface;
    FThemeName: string;
    FAreaStates: TArrayList<TDrawState>;
    FAreaClicked: Integer;
    FOnDraw: TDrawEvent;
    FMousePoint: TPointI;
    FMouseDown: Boolean;
    FMouseTimer: Boolean;
    function InitAreas: Integer;
    function GetSurface: ISurface;
    procedure SetDrawState(Value: TDrawState);
    procedure SetThemeName(const Value: string);
    procedure MouseTimer(Enable: Boolean);
  protected
    { Allow controls direct access to draw state }
    FDrawState: TDrawState;
    { Areas are clickable regions of a control, such as the arrow of a drop
      down button. AreaClick is invoked when an area is clicked. }
    procedure AreaClick(Area: Integer); virtual;
    { Return the number of areas }
    function GetAreaCount: Integer; virtual;
    { Return the bounds of an area }
    function GetAreaRect(Index: Integer): TRectI; virtual;
    { Return the draw state of an area }
    function GetAreaState(Index: Integer): TDrawState;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    { Add an item to the draw state }
    procedure IncludeStateItem(Item: TDrawStateItem);  virtual;
    { Remove an item from the draw state }
    procedure ExcludeStateItem(Item: TDrawStateItem);  virtual;
    { Float property change notification }
    procedure PropChange(Prop: PFloat);  virtual;
    procedure SetParent(NewParent: TWinControl); override;
    { Create a default size }
    class function GetControlClassDefaultSize: TSize; override;
    { Update draw state when enabled is changed }
    procedure EnabledChanged; override;
    { Override ThemeAware and return true to subscribe to global theme changes }
    function ThemeAware: Boolean; virtual;
    { Invoked when the theme is changed }
    procedure ThemeChanged; virtual;
    { Paint is now final, so use Draw to access Surface }
    procedure Paint; override; final;
    { While Draw is executing Surface refers to a valid ISurface }
    procedure Draw; virtual;
    { Surface is only valid while Draw is executing }
    property Surface: ISurface read GetSurface;
    { Visual representation of the control. Is it pressed, hot, checked, etc }
    property DrawState: TDrawState read FDrawState write SetDrawState;
    { Theme name determines the styling for a control }
    property ThemeName: string read FThemeName write SetThemeName;
    { Draw event handler }
    property OnDraw: TDrawEvent read FOnDraw write FOnDraw;
    { The point where the mouse was pressed inside the control }
    property MousePoint: TPointI read FMousePoint;
  public
    { Create a new control }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TSurfaceCustomControl is the base class for custom windowed controls
  which require an ISurface object
  See also
  <link Overview.Codebot.Controls.TSurfaceCustomControl, TSurfaceCustomControl members> }

  TSurfaceCustomControl = class(TCustomControl, IFloatPropertyNotify)
  private
    FSurface: ISurface;
    FThemeName: string;
    FOnDraw: TDrawEvent;
    function GetSurface: ISurface;
    procedure SetDrawState(Value: TDrawState);
    procedure SetThemeName(const Value: string);
  protected
    { Allow controls direct access to draw state }
    FDrawState: TDrawState;
    { Float property change notification }
    procedure PropChange(Prop: PFloat);  virtual;
    { Override ThemeAware and return true to subscribe to global theme changes }
    function ThemeAware: Boolean; virtual;
    { Invoked when the theme is changed }
    procedure ThemeChanged; virtual;
    { Paint is now final, so use Draw to access Surface }
    procedure Paint; override; final;
    { While Draw is executing Surface refers to a valid ISurface }
    procedure Draw; virtual;
    { Surface is only valid while Draw is executing }
    property Surface: ISurface read GetSurface;
    { Visual representation of the control. Is it pressed, hot, checked, etc }
    property DrawState: TDrawState read FDrawState write SetDrawState;
    { Theme name determines the styling for a control }
    property ThemeName: string read FThemeName write SetThemeName;
    { Draw event handler }
    property OnDraw: TDrawEvent read FOnDraw write FOnDraw;
  public
    { Create a new control }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TSurfaceForm is the base class for custom forms controls which
  require an ISurface object
  See also
  <link Overview.Codebot.Controls.TSurfaceForm, TSurfaceForm members> }

  TSurfaceForm = class(TForm)
  private
    FSurface: ISurface;
    FThemeName: string;
    FOnRender: TDrawEvent;
    function GetSurface: ISurface;
    procedure SetDrawState(Value: TDrawState);
    procedure SetThemeName(const Value: string);
  protected
    { Allow controls direct access to draw state }
    FDrawState: TDrawState;
    { Override ThemeAware and return true to subscribe to global theme changes }
    function ThemeAware: Boolean; virtual;
    { Invoked when the theme is changed }
    procedure ThemeChanged; virtual;
    { Allow the form to be drawn at design time }
    procedure PaintWindow(DC: HDC); override;
    { Paint is now final, so use Draw to access Surface }
    procedure Paint; override; final;
    { While Draw is executing Surface refers to a valid ISurface }
    procedure Draw; virtual;
    { Surface is only valid while Draw is executing }
    property Surface: ISurface read GetSurface;
    { Visual representation of the control. Is it pressed, hot, checked, etc }
    property DrawState: TDrawState read FDrawState write SetDrawState;
  public
    { Create a new form without loading a form resource }
    constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;
    destructor Destroy; override;
  published
    { Draw event handler }
    property OnRender: TDrawEvent read FOnRender write FOnRender;
    { Theme name determines the styling for a control }
    property ThemeName: string read FThemeName write SetThemeName;
  end;

{ Arrange the controls of a container whose top left corner lies inside
  bounds in a single row from left to right, centered vertically in bounds.
  Labels are given extra space on both sides. }
procedure ArrangeControls(Container: TWinControl; Bounds: TRectI; Offset: Integer = 0);

implementation

{ Codebot.Platform.LCL is used so programs with these controls convert system
  colors using the LCL }

uses
  Codebot.Constants,
  Codebot.Platform.LCL;

constructor TNotifyCollection<T>.Create(AOwner: TPersistent);
begin
  inherited Create(T);
  FOwner := AOwner;
end;

destructor TNotifyCollection<T>.Destroy;
var
  EmptyNotify: TItemNotifyDelegate;
  EmptyUpdate: TItemUpdateDelegate;
begin
  FOnItemNotify := {%H-}EmptyNotify;
  FOnItemUpdate := {%H-}EmptyUpdate;
  BeginUpdate;
  Clear;
  EndUpdate;
  inherited Destroy;
end;

procedure TNotifyCollection<T>.Assign(Source: TPersistent);
var
  Collection: TCollection;
  Item: TCollectionItem;
  C: TComponent;
  I: Integer;
begin
  if Source = Self then
    Exit;
  if Source is TCollection then
  begin
    BeginUpdate;
    try
      Clear;
      Collection := Source as TCollection;
      for I := 0 to Collection.Count - 1 do
      begin
        Item := Add;
        Item.Assign(Collection.Items[I]);
      end;
    finally
      EndUpdate;
    end;
    if Owner is TComponent then
    begin
      C := Owner as TComponent;
      if csDesigning in C.ComponentState then
        OwnerFormDesignerModified(C);
    end;
  end
  else
    inherited Assign(Source);
end;

function TNotifyCollection<T>.Add: T;
var
  C: TComponent;
begin
  Result := T(inherited Add);
  if Owner is TComponent then
  begin
    C := Owner as TComponent;
    if csDesigning in C.ComponentState then
      OwnerFormDesignerModified(C);
  end;
end;

function TNotifyCollection<T>.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TNotifyCollection<T>.Notify(Item: TCollectionItem; Action: TCollectionNotification);
var
  Event: TItemNotifyEvent;
begin
  for Event in FOnItemNotify do
    Event(Self, Item, Action);
end;

procedure TNotifyCollection<T>.Update(Item: TCollectionItem);
var
  Event: TItemUpdateEvent;
begin
  for Event in FOnItemUpdate do
    Event(Self, Item);
end;

function TNotifyCollection<T>.GetItem(const Index: Integer): T;
begin
  Result := T(inherited GetItem(Index));
end;

procedure TNotifyCollection<T>.SetItem(const Index: Integer; const Value: T);
begin
  inherited SetItem(Index, Value);
end;

function TNotifyCollection<T>.GetOnItemNotify: IItemNotifyDelegate;
begin
  Result := FOnItemNotify;
end;

function TNotifyCollection<T>.GetOnItemUpdate: IItemUpdateDelegate;
begin
  Result := FOnItemUpdate;
end;

{ TEdgeOffset }

procedure TEdgeOffset.Assign(Source: TPersistent);
var
  E: TEdgeOffset;
begin
  if Source is TEdgeOffset then
  begin
    E := Source as TEdgeOffset;
    FLeft := E.Left;
    FTop := E.Top;
    FRight := E.Right;
    FBottom := E.Bottom;
    Change;
  end
  else
    inherited Assign(Source);
end;

procedure TEdgeOffset.SetLeft(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FLeft = Value then Exit;
  FLeft := Value;
end;

procedure TEdgeOffset.SetTop(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FTop = Value then Exit;
  FTop := Value;
  Change;
end;

procedure TEdgeOffset.SetRight(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FRight = Value then Exit;
  FRight := Value;
  Change;
end;

procedure TEdgeOffset.SetBottom(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FBottom = Value then Exit;
  FBottom := Value;
  Change;
end;

{ TSurfaceGraphicControl }

constructor TSurfaceGraphicControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAreaClicked := -1;
  Width := 160;
  Height := 80;
  ControlStyle := (ControlStyle +
    [csParentBackground, csClickEvents, csCaptureMouse]) - [csOpaque];
  if ThemeAware then
    ThemeNotifyAdd(ThemeChanged);
end;

destructor TSurfaceGraphicControl.Destroy;
begin
  MouseTimer(False);
  if ThemeAware then
    ThemeNotifyRemove(ThemeChanged);
  inherited Destroy;
end;

class function TSurfaceGraphicControl.GetControlClassDefaultSize: TSize;
begin
  Result.cx := 80;
  Result.cy := 80;
end;

procedure TSurfaceGraphicControl.SetParent(NewParent: TWinControl);
begin
  MouseTimer(False);
  inherited SetParent(NewParent);
end;

type
  PControl = ^TControl;

procedure ControlTimer(hWnd: HWND; uMsg: UINT; idEvent: UINT_PTR; dwTime: DWORD); stdcall;
var
  C: TSurfaceGraphicControl absolute idEvent;
  P: TPointI;
begin
  if C.FMouseDown then
    Exit;
  P := Mouse.CursorPos;
  P := C.ScreenToClient(P);
  if (P.X < 0) or (P.X >= C.Width) or (P.Y < 0) or (P.Y >= C.Height) then
  begin
    C.Perform(CM_MOUSELEAVE, 0, 0);
    if Application.MouseControl = C then
      PControl(@Application.MouseControl)^ := nil;
  end;
end;

procedure TSurfaceGraphicControl.MouseTimer(Enable: Boolean);
begin
  if Parent = nil then
    Exit;
  if Enable <> FMouseTimer then
  begin
    FMouseTimer := Enable;
    if FMouseTimer then
      SetTimer(Parent.Handle, UIntPtr(Self), 250, @ControlTimer)
    else
      KillTimer(Parent.Handle, UIntPtr(Self));
  end;
end;

function TSurfaceGraphicControl.InitAreas: Integer;
var
  I: Integer;
begin
  Result := GetAreaCount;
  I := Result;
  if FAreaStates.Length <> I then
  begin
    FAreaStates.Length :=  I;
    while I > 0 do
    begin
      Dec(I);
      FAreaStates[I] := [];
    end;
  end;
end;

procedure TSurfaceGraphicControl.AreaClick(Area: Integer);
begin

end;

function TSurfaceGraphicControl.GetAreaCount: Integer;
begin
  Result := 1;
end;

function TSurfaceGraphicControl.GetAreaRect(Index: Integer): TRectI;
begin
  Result := ClientRect;
end;

function TSurfaceGraphicControl.GetAreaState(Index: Integer): TDrawState;
begin
  Result := FAreaStates[Index];
end;

procedure TSurfaceGraphicControl.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  I: Integer;
begin
  FMouseDown := Button = mbLeft;
  if FMouseDown then
  begin
    FMousePoint.X := X;
    FMousePoint.Y := Y;
    DrawState := DrawState + [dsPressed, dsHot];
    FAreaClicked := -1;
    I := InitAreas;
    while I > 0 do
    begin
      Dec(I);
      if GetAreaRect(I).Contains(FMousePoint) then
        FAreaStates[I] := DrawState
      else
        FAreaStates[I] := [];
    end;
    MouseTimer(False);
  end;
  inherited MouseDown(Button, Shift, X, Y)
end;

procedure TSurfaceGraphicControl.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Hot: Boolean;
  I: Integer;
begin
  if Button = mbLeft then
  begin
    FMouseDown := False;
    Hot := (X > -1) and (X < Width) and (Y > -1) and (Y < Height);
    FAreaClicked := -1;
    I := InitAreas;
    while I > 0 do
    begin
      Dec(I);
      if GetAreaRect(I).Contains(X, Y) then
      begin
        if Hot and (dsPressed in FAreaStates[I]) then
          FAreaClicked := I;
        FAreaStates[I] := FAreaStates[I] -  [dsPressed] + [dsHot];
      end
      else
        FAreaStates[I] := FAreaStates[I] -  [dsPressed] - [dsHot];
    end;
    if Hot then
    begin
      DrawState := DrawState - [dsPressed]  + [dsHot];
    end
    else
    begin
      DrawState := (DrawState - [dsPressed, dsHot]);
      Perform(CM_MOUSELEAVE, 0, 0);
      PControl(@Application.MouseControl)^ := nil;
    end;
  end;
  if FAreaClicked > -1 then
  begin
    FAreaStates[FAreaClicked] := [];
    Invalidate;
    AreaClick(FAreaClicked);
    FAreaClicked := -1;
  end;
  inherited MouseUp(Button, Shift, X, Y)
end;

procedure TSurfaceGraphicControl.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  D: TDrawState;
  I: Integer;
begin
  if not FMouseDown then
  begin
    I := InitAreas;
    while I > 0 do
    begin
      Dec(I);
      D := FAreaStates[I];
      if GetAreaRect(I).Contains(X, Y) then
        FAreaStates[I] := FAreaStates[I] + [dsHot]
      else
        FAreaStates[I] := FAreaStates[I] - [dsHot];
      if D <> FAreaStates[I] then
        Invalidate;
    end;
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TSurfaceGraphicControl.MouseEnter;
var
  P: TPointI;
  I: Integer;
begin
  MouseTimer(True);
  P := ScreenToClient(Mouse.CursorPos);
  DrawState := DrawState + [dsHot];
  I := InitAreas;
  while I > 0 do
  begin
    Dec(I);
    if GetAreaRect(I).Contains(P) then
      FAreaStates[I] := FAreaStates[I] + [dsHot]
    else
      FAreaStates[I] := FAreaStates[I] - [dsHot];
  end;
  inherited MouseEnter;
end;

procedure TSurfaceGraphicControl.MouseLeave;
var
  I: Integer;
begin
  MouseTimer(False);
  DrawState := DrawState - [dsHot];
  InitAreas;
  I := GetAreaCount;
  while I > 0 do
  begin
    Dec(I);
    FAreaStates[I] := FAreaStates[I] - [dsHot];
  end;
  inherited MouseLeave;
end;

procedure TSurfaceGraphicControl.IncludeStateItem(Item: TDrawStateItem);
begin

end;

procedure TSurfaceGraphicControl.ExcludeStateItem(Item: TDrawStateItem);
begin

end;

procedure TSurfaceGraphicControl.PropChange(Prop: PFloat);
begin

end;

function TSurfaceGraphicControl.ThemeAware: Boolean;
begin
  Result := False;
end;

procedure TSurfaceGraphicControl.Draw;
var
  I: Integer;
begin
  I := GetAreaCount;
  if FAreaStates.Length <> I then
  begin
    FAreaStates.Length := I;
    while I > 0 do
    begin
      Dec(I);
      FAreaStates[I] := [];
    end;
  end;
  if Assigned(FOnDraw) then
    FOnDraw(Self, Surface);
end;

procedure TSurfaceGraphicControl.Paint;
begin
  FSurface := NewSurface(Canvas);
  if FSurface = nil then
    Exit;
  Theme.Select(Self, Surface, DrawState, Font);
  if ThemeAware then
    Theme.Select(FThemeName);
  try
    Draw;
  finally
    Theme.Deselect;
    FSurface.Flush;
    FSurface := nil;
  end;
end;

function TSurfaceGraphicControl.GetSurface: ISurface;
begin
  if (FSurface = nil) then
    raise ESurfaceAccessError.CreateFmt(SSurfaceAccess, [ClassName]);
  Result := FSurface;
end;

procedure TSurfaceGraphicControl.SetDrawState(Value: TDrawState);
var
  I: TDrawStateItem;
begin
  if FDrawState <> Value then
  begin
    for I := Low(I) to High(I) do
      if (I in Value) and (not (I in FDrawState)) then
        IncludeStateItem(I);
    for I := Low(I) to High(I) do
      if (not (I in Value)) and (I in FDrawState) then
        ExcludeStateItem(I);
    FDrawState := Value;
    Invalidate;
  end;
end;

procedure TSurfaceGraphicControl.SetThemeName(const Value: string);
var
  S: string;
begin
  if ThemeFind(Value) = nil then
    S:= ''
  else
    S := Value;
  if FThemeName <> S then
  begin
    FThemeName := S;
    ThemeChanged;
  end;
end;

procedure TSurfaceGraphicControl.ThemeChanged;
begin
  Invalidate;
end;

procedure TSurfaceGraphicControl.EnabledChanged;
begin
  inherited EnabledChanged;
  if Enabled then
    Exclude(FDrawState, dsDisabled)
  else
  begin
    Include(FDrawState, dsDisabled);
    Exclude(FDrawState, dsHot);
  end;
end;

{ TSurfaceCustomControl }

constructor TSurfaceCustomControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 160;
  Height := 80;
  ControlStyle := (ControlStyle + [csParentBackground, csAcceptsControls]) - [csOpaque];
  if ThemeAware then
    ThemeNotifyAdd(ThemeChanged);
end;

destructor TSurfaceCustomControl.Destroy;
begin
  if ThemeAware then
    ThemeNotifyRemove(ThemeChanged);
  inherited Destroy;
end;

procedure TSurfaceCustomControl.PropChange(Prop: PFloat);
begin

end;

function TSurfaceCustomControl.ThemeAware: Boolean;
begin
  Result := False;
end;

procedure TSurfaceCustomControl.Draw;
begin
  if Assigned(FOnDraw) then
    FOnDraw(Self, Surface);
end;

procedure TSurfaceCustomControl.Paint;
begin
  FSurface := NewSurface(Canvas);
  if FSurface = nil then
    Exit;
  Theme.Select(Self, Surface, DrawState, Font);
  if ThemeAware then
    Theme.Select(FThemeName);
  try
    Draw;
  finally
    Theme.Deselect;
    FSurface.Flush;
    FSurface := nil;
  end;
end;

function TSurfaceCustomControl.GetSurface: ISurface;
begin
  if (FSurface = nil) then
    raise ESurfaceAccessError.CreateFmt(SSurfaceAccess, [ClassName]);
  Result := FSurface;
end;

procedure TSurfaceCustomControl.SetDrawState(Value: TDrawState);
begin
  if FDrawState <> Value then
  begin
    FDrawState := Value;
    Invalidate;
  end;
end;

procedure TSurfaceCustomControl.SetThemeName(const Value: string);
var
  S: string;
begin
  if ThemeFind(Value) = nil then
    S:= ''
  else
    S := Value;
  if FThemeName <> S then
  begin
    FThemeName := S;
    ThemeChanged;
  end;
end;

procedure TSurfaceCustomControl.ThemeChanged;
begin
  Invalidate;
end;

{ TSurfaceForm }

constructor TSurfaceForm.CreateNew(AOwner: TComponent; Num: Integer = 0);
begin
  inherited CreateNew(AOwner, Num);
  if ThemeAware then
    ThemeNotifyAdd(ThemeChanged);
end;

destructor TSurfaceForm.Destroy;
begin
  if ThemeAware then
    ThemeNotifyRemove(ThemeChanged);
  inherited Destroy;
end;

function TSurfaceForm.ThemeAware: Boolean;
begin
  Result := True;
end;

procedure TSurfaceForm.Draw;
begin
  if Assigned(FOnRender) then
    FOnRender(Self, Surface);
end;

procedure TSurfaceForm.Paint;
begin
  FSurface := NewSurface(Canvas);
  if FSurface = nil then
    Exit;
  Theme.Select(Self, Surface, DrawState, Font);
  if ThemeAware then
    Theme.Select(FThemeName);
  try
    Draw;
  finally
    Theme.Deselect;
    FSurface.Flush;
    FSurface := nil;
  end;
end;

procedure TSurfaceForm.PaintWindow(DC: HDC);
begin
  Canvas.Handle := DC;
  try
    Paint;
    if Designer <> nil then Designer.PaintGrid;
  finally
    Canvas.Handle := 0;
  end;
end;

function TSurfaceForm.GetSurface: ISurface;
begin
  if (FSurface = nil) then
    raise ESurfaceAccessError.CreateFmt(SSurfaceAccess, [ClassName]);
  Result := FSurface;
end;

procedure TSurfaceForm.SetDrawState(Value: TDrawState);
begin
  if FDrawState <> Value then
  begin
    FDrawState := Value;
    Invalidate;
  end;
end;

procedure TSurfaceForm.SetThemeName(const Value: string);
var
  S: string;
begin
  if ThemeFind(Value) = nil then
    S:= ''
  else
    S := Value;
  if FThemeName <> S then
  begin
    FThemeName := S;
    ThemeChanged;
  end;
end;

procedure TSurfaceForm.ThemeChanged;
begin
  Invalidate;
end;

function CompareControls(constref A: TControl; constref B: TControl): Integer;
begin
  Result := A.Left - B.Left;
end;

procedure ArrangeControls(Container: TWinControl; Bounds: TRectI; Offset: Integer = 0);

  function IsLabel(C: TControl): Boolean;
  var
    S: string;
  begin
    S := C.ClassName;
    Result := S.EndsWith('Label');
  end;

  function IsSlider(C: TControl): Boolean;
  var
    S: string;
  begin
    S := C.ClassName;
    Result := S.EndsWith('SlideBar');
  end;

var
  Controls: TArrayList<TControl>;
  Control: TControl;
  X, Y: Integer;
  I: Integer;
begin
  for I := 0 to Container.ControlCount - 1 do
  begin
    Control := Container.Controls[I];
    if Bounds.Contains(Control.Left, Control.Top) then
    Controls.Push(Control);
  end;
  Controls.Sort(soAscend, CompareControls);
  X := Bounds.Left + Offset;
  Y := Bounds.Height;
  for Control in Controls do
  begin
    if IsLabel(Control) then
    begin
      X := X + 8;
      Control.Left := X;
      Control.Top := (Y - Control.Height) div 2 + Bounds.Top;
      X := X + Control.Width + 8;
    end
    else if IsSlider(Control) then
    begin
      Control.Left := X;
      Control.Top := (Y - Control.Height) div 2 + Bounds.Top;
      X := X + Control.Width;
    end
    else
    begin
      Control.Left := X;
      Control.Top := (Y - Control.Height) div 2 + Bounds.Top;
      X := X + Control.Width;
    end;
  end;
end;

end.

