(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified March 2015                                 *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.controls.txt> }
unit Codebot.Controls;

{$i codebot.inc}

interface

uses
  Classes, SysUtils, Types, Graphics, Controls, Forms, LCLType, LCLIntf, LCLProc,
  Codebot.System,
  Codebot.Graphics,
  Codebot.Graphics.Types;

type
  TItemUpdateEvent = procedure(Sender: TObject; Item: TCollectionItem) of object;
  TItemUpdateDelegate = TDelegate<TItemUpdateEvent>;
  IItemUpdateDelegate = IDelegate<TItemUpdateEvent>;

  TItemNotifyEvent = procedure(Sender: TObject; Item: TCollectionItem; Action: TCollectionNotification) of object;
  TItemNotifyDelegate = TDelegate<TItemNotifyEvent>;
  IItemNotifyDelegate = IDelegate<TItemNotifyEvent>;

{ TNotifyCollection\<T\> simplifies creating specialized persistent collections
  See also
  <link Overview.Codebot.Controls.TNotifyCollection\<T\>, TNotifyCollection\<T\> members> }

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
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function Add: T;
    property Owner: TPersistent read FOwner;
    property Items[const Index: Integer]: T read GetItem write SetItem; default;
    property OnItemNotify: IItemNotifyDelegate read GetOnItemNotify;
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

var
  MouseEnters: Integer;
  MouseLeaves: Integer;

{ TRenderGraphicControl is the base class for custom graphic controls
  which require an ISurface object
  See also
  <link Overview.Codebot.Controls.TRenderGraphicControl, TRenderGraphicControl members> }

type
  TRenderGraphicControl = class(TGraphicControl, IFloatPropertyNotify)
  private
    FSurface: ISurface;
    FThemeName: string;
    FOnRender: TDrawEvent;
    FMouseDown: Boolean;
    FMouseTimer: Boolean;
    function GetSurface: ISurface;
    procedure SetDrawState(Value: TDrawState);
    procedure SetThemeName(const Value: string);
    procedure MouseTimer(Enable: Boolean);
  protected
    { Allow controls direct access to draw state }
    FDrawState: TDrawState;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure IncludeStateItem(Item: TDrawStateItem);  virtual;
    procedure ExcludeStateItem(Item: TDrawStateItem);  virtual;
    procedure PropChange(Prop: PFloat);  virtual;
    procedure SetParent(NewParent: TWinControl); override;
    { Create a default size }
    class function GetControlClassDefaultSize: TSize; override;
    { Update draw state when enabled is changed }
    procedure EnabledChanged; override;
    { Override ThemeAware and return true to subscribe to glabal theme changes }
    function ThemeAware: Boolean; virtual;
    { Invoked when the theme is changed }
    procedure ThemeChanged; virtual;
    { Paint is now final, so use Render to access Surface }
    procedure Paint; override; final;
    { While Render is executing Surface refers to a valid ISurface }
    procedure Render; virtual;
    { Surface is only during while Draw is executing }
    property Surface: ISurface read GetSurface;
    { Visual representation of the control. Is it pressed, hot, checked, ect }
    property DrawState: TDrawState read FDrawState write SetDrawState;
    { Theme name determines the styling for a control }
    property ThemeName: string read FThemeName write SetThemeName;
    { Render event handler }
    property OnRender: TDrawEvent read FOnRender write FOnRender;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { True if the mouse is down inside the control }
    property IsMouseDown: Boolean read FMouseDown;
  end;

{ TRenderCustomControl is the base class for custom windowed controls
  which require an ISurface object
  See also
  <link Overview.Codebot.Controls.TRenderCustomControl, TRenderCustomControl members> }

  TRenderCustomControl = class(TCustomControl, IFloatPropertyNotify)
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
    { Float property change notification }
    procedure PropChange(Prop: PFloat);  virtual;
    { Override ThemeAware and return true to subscribe to glabal theme changes }
    function ThemeAware: Boolean; virtual;
    { Invoked when the theme is changed }
    procedure ThemeChanged; virtual;
    { Paint is now final, so use Render to access Surface }
    procedure Paint; override; final;
    { While Render is executing Surface refers to a valid ISurface }
    procedure Render; virtual;
    { Surface is only during while Draw is executing }
    property Surface: ISurface read GetSurface;
    { Visual representation of the control. Is it pressed, hot, checked, ect }
    property DrawState: TDrawState read FDrawState write SetDrawState;
    { Theme name determines the styling for a control }
    property ThemeName: string read FThemeName write SetThemeName;
    { Render event handler }
    property OnRender: TDrawEvent read FOnRender write FOnRender;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TRenderForm is the base class for custom forms controls which
  require an ISurface object
  See also
  <link Overview.Codebot.Controls.TRenderForm, TRenderForm members> }

  TRenderForm = class(TForm)
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
    { Override ThemeAware and return true to subscribe to glabal theme changes }
    function ThemeAware: Boolean; virtual;
    { Invoked when the theme is changed }
    procedure ThemeChanged; virtual;
    { Allow the form to be drawn at desing time }
    procedure PaintWindow(DC: HDC); override;
    { Paint is now final, so use Render to access Surface }
    procedure Paint; override; final;
    { While Render is executing Surface refers to a valid ISurface }
    procedure Render; virtual;
    { Surface is only during while Draw is executing }
    property Surface: ISurface read GetSurface;
    { Visual representation of the control. Is it pressed, hot, checked, ect }
    property DrawState: TDrawState read FDrawState write SetDrawState;
  public
    constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;
    destructor Destroy; override;
  published
    { Theme name determines the styling for a control }
    property OnRender: TDrawEvent read FOnRender write FOnRender;
    { Theme name determines the styling for a control }
    property ThemeName: string read FThemeName write SetThemeName;
  end;

procedure ArrangeControls(Container: TWinControl; Bounds: TRectI; Offset: Integer = 0);

implementation

uses
  Codebot.Constants;

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
  FOnItemNotify := EmptyNotify;
  FOnItemUpdate := EmptyUpdate;
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

{ TRenderGraphicControl }

constructor TRenderGraphicControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  with GetControlClassDefaultSize do
  begin
    Width := cx;
    Height := cy;
  end;
  ControlStyle := (ControlStyle +
    [csParentBackground, csClickEvents, csCaptureMouse]) - [csOpaque];
  if ThemeAware then
    ThemeNotifyAdd(ThemeChanged);
end;

destructor TRenderGraphicControl.Destroy;
begin
  MouseTimer(False);
  if ThemeAware then
    ThemeNotifyRemove(ThemeChanged);
  inherited Destroy;
end;

class function TRenderGraphicControl.GetControlClassDefaultSize: TSize;
begin
  Result.cx := 80;
  Result.cy := 80;
end;

procedure TRenderGraphicControl.SetParent(NewParent: TWinControl);
begin
  MouseTimer(False);
  inherited SetParent(NewParent);
end;

type
  PControl = ^TControl;

procedure ControlTimer(hWnd: HWND; uMsg: UINT; idEvent: UINT_PTR; dwTime: DWORD); stdcall;
var
  C: TRenderGraphicControl absolute idEvent;
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

procedure TRenderGraphicControl.MouseTimer(Enable: Boolean);
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

procedure TRenderGraphicControl.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  FMouseDown := Button = mbLeft;
  if FMouseDown then
  begin
    DrawState := DrawState + [dsPressed, dsHot];
    MouseTimer(False);
  end;
  inherited MouseDown(Button, Shift, X, Y)
end;

procedure TRenderGraphicControl.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Hot: Boolean;
begin
  if Button = mbLeft then
  begin
    FMouseDown := False;
    Hot := (X > -1) and (X < Width) and (Y > -1) and (Y < Height);
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
  inherited MouseUp(Button, Shift, X, Y)
end;

procedure TRenderGraphicControl.MouseEnter;
begin
  MouseTimer(True);
  DrawState := DrawState + [dsHot];
  inherited MouseEnter;
end;

procedure TRenderGraphicControl.MouseLeave;
begin
  MouseTimer(False);
  DrawState := DrawState - [dsHot];
  inherited MouseLeave;
end;

procedure TRenderGraphicControl.IncludeStateItem(Item: TDrawStateItem);
begin

end;

procedure TRenderGraphicControl.ExcludeStateItem(Item: TDrawStateItem);
begin

end;

procedure TRenderGraphicControl.PropChange(Prop: PFloat);
begin

end;

function TRenderGraphicControl.ThemeAware: Boolean;
begin
  Result := False;
end;

procedure TRenderGraphicControl.Render;
begin
  if Assigned(FOnRender) then
    FOnRender(Self, Surface);
end;

procedure TRenderGraphicControl.Paint;
begin
  FSurface := NewSurface(Canvas);
  if FSurface = nil then
    Exit;
  Theme.Select(Self, Surface, DrawState, Font);
  if ThemeAware then
    Theme.Select(FThemeName);
  try
    Render;
  finally
    Theme.Deselect;
    FSurface.Flush;
    FSurface := nil;
  end;
end;

function TRenderGraphicControl.GetSurface: ISurface;
begin
  if (FSurface = nil) then
    raise ESurfaceAccessError.CreateFmt(SSurfaceAccess, [ClassName]);
  Result := FSurface;
end;

procedure TRenderGraphicControl.SetDrawState(Value: TDrawState);
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

procedure TRenderGraphicControl.SetThemeName(const Value: string);
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

procedure TRenderGraphicControl.ThemeChanged;
begin
  Invalidate;
end;

procedure TRenderGraphicControl.EnabledChanged;
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

{ TRenderCustomControl }

constructor TRenderCustomControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := (ControlStyle + [csParentBackground]) - [csOpaque];
  if ThemeAware then
    ThemeNotifyAdd(ThemeChanged);
end;

destructor TRenderCustomControl.Destroy;
begin
  if ThemeAware then
    ThemeNotifyRemove(ThemeChanged);
  inherited Destroy;
end;

procedure TRenderCustomControl.PropChange(Prop: PFloat);
begin

end;

function TRenderCustomControl.ThemeAware: Boolean;
begin
  Result := False;
end;

procedure TRenderCustomControl.Render;
begin
  if Assigned(FOnRender) then
    FOnRender(Self, Surface);
end;

procedure TRenderCustomControl.Paint;
begin
  FSurface := NewSurface(Canvas);
  if FSurface = nil then
    Exit;
  Theme.Select(Self, Surface, DrawState, Font);
  if ThemeAware then
    Theme.Select(FThemeName);
  try
    Render;
  finally
    Theme.Deselect;
    FSurface.Flush;
    FSurface := nil;
  end;
end;

function TRenderCustomControl.GetSurface: ISurface;
begin
  if (FSurface = nil) then
    raise ESurfaceAccessError.CreateFmt(SSurfaceAccess, [ClassName]);
  Result := FSurface;
end;

procedure TRenderCustomControl.SetDrawState(Value: TDrawState);
begin
  if FDrawState <> Value then
  begin
    FDrawState := Value;
    Invalidate;
  end;
end;

procedure TRenderCustomControl.SetThemeName(const Value: string);
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

procedure TRenderCustomControl.ThemeChanged;
begin
  Invalidate;
end;

{ TRenderForm }

constructor TRenderForm.CreateNew(AOwner: TComponent; Num: Integer = 0);
begin
  inherited CreateNew(AOwner, Num);
  if ThemeAware then
    ThemeNotifyAdd(ThemeChanged);
end;

destructor TRenderForm.Destroy;
begin
  if ThemeAware then
    ThemeNotifyRemove(ThemeChanged);
  inherited Destroy;
end;

function TRenderForm.ThemeAware: Boolean;
begin
  Result := True;
end;

procedure TRenderForm.Render;
begin
  if Assigned(FOnRender) then
    FOnRender(Self, Surface);
end;

procedure TRenderForm.Paint;
begin
  FSurface := NewSurface(Canvas);
  if FSurface = nil then
    Exit;
  Theme.Select(Self, Surface, DrawState, Font);
  if ThemeAware then
    Theme.Select(FThemeName);
  try
    Render;
  finally
    Theme.Deselect;
    FSurface.Flush;
    FSurface := nil;
  end;
end;

procedure TRenderForm.PaintWindow(DC: HDC);
begin
  Canvas.Handle := DC;
  try
    Paint;
    if Designer <> nil then Designer.PaintGrid;
  finally
    Canvas.Handle := 0;
  end;
end;

function TRenderForm.GetSurface: ISurface;
begin
  if (FSurface = nil) then
    raise ESurfaceAccessError.CreateFmt(SSurfaceAccess, [ClassName]);
  Result := FSurface;
end;

procedure TRenderForm.SetDrawState(Value: TDrawState);
begin
  if FDrawState <> Value then
  begin
    FDrawState := Value;
    Invalidate;
  end;
end;

procedure TRenderForm.SetThemeName(const Value: string);
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

procedure TRenderForm.ThemeChanged;
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

