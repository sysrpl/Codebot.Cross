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
  Classes, SysUtils, Graphics, Controls, Forms, LCLType,
  Codebot.System,
  Codebot.Graphics,
  Codebot.Graphics.Types;

{ TEdgeOffset represents a padding or margin on a control
  See also
  <link Overview.Codebot.Controls.TEdgeOffset, TEdgeOffset members> }

type
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

{ TRenderGraphicControl is the base class for custom graphic controls
  which require an ISurface object
  See also
  <link Overview.Codebot.Controls.TRenderGraphicControl, TRenderGraphicControl members> }

  TRenderGraphicControl = class(TGraphicControl)
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
  end;

{ TRenderCustomControl is the base class for custom windowed controls
  which require an ISurface object
  See also
  <link Overview.Codebot.Controls.TRenderCustomControl, TRenderCustomControl members> }

  TRenderCustomControl = class(TCustomControl)
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
  Width := 100;
  Height := 100;
  ControlStyle := (ControlStyle + [csParentBackground]) - [csOpaque];
  if ThemeAware then
    ThemeNotifyAdd(ThemeChanged);
end;

destructor TRenderGraphicControl.Destroy;
begin
  if ThemeAware then
    ThemeNotifyRemove(ThemeChanged);
  inherited Destroy;
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
begin
  if FDrawState <> Value then
  begin
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
  Controls.Sort(CompareControls);
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

