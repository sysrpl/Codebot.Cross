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

{ ESurfaceAccessError }

type
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

{ TBorderContainer }

  TBorderContainer = class(TRenderCustomControl)
  protected
    procedure Render; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property BidiMode;
    property BorderWidth;
    property BorderStyle;
    property Caption;
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

uses
  Codebot.Constants;

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

{ TBorderContainer }

constructor TBorderContainer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls];
  BorderStyle := bsSingle;
  Color := clWindow;
  Width := 160;
  Height := 160;
end;

procedure TBorderContainer.Render;
begin
  inherited Render;
  if csDesigning in ComponentState then
    Surface.FillRect(Brushes.Checker, ClientRect);
end;

end.

