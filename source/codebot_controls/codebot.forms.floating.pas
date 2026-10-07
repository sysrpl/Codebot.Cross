(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified March 2019                                 *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.forms.floating.txt> }
unit Codebot.Forms.Floating;

{$i ../codebot/codebot.inc}

interface

uses
  Classes, SysUtils, Controls, Forms, LCLIntf, LCLType,
  Codebot.System,
  Codebot.Graphics.Types;

{ TFloatingForm is a borderless form which can be made transparent, faded,
  or set to let mouse input pass through it }

type
  TFloatingForm = class(TForm)
  private
    FInteractive: Boolean;
    FWindow: Pointer;
    FOpacity: Byte;
    FFaded: Boolean;
    FFadeTop: Integer;
    FFadeMoved: Boolean;
    function GetCompositing: Boolean;
    procedure SetFaded(Value: Boolean);
    procedure SetInteractive(Value: Boolean);
    procedure SetOpacity(Value: Byte);
  protected
    {doc off}
    procedure CreateHandle; override;
    procedure Loaded; override;
    procedure Paint; override;
    {doc on}
  public
    { Create a new floating form }
    constructor Create(AOwner: TComponent); override;
    { Move and resize the form in one step }
    procedure MoveSize(Rect: TRectI);
    { The overall transparency of the form }
    property Opacity: Byte read FOpacity write SetOpacity;
    { Compositing is true when the window manager supports transparency }
    property Compositing: Boolean read GetCompositing;
    { When false mouse input passes through the form to windows below }
    property Interactive: Boolean read FInteractive write SetInteractive;
    { When true the form fades out, or hides if fading is not supported }
    property Faded: Boolean read FFaded write SetFaded;
  end;

implementation

{$if defined(linux) and defined(lclgtk2)}
uses
  GLib2, Gdk2, Gtk2, Gtk2Def, Gtk2Extra, Gtk2Globals,
  Codebot.Interop.Linux.NetWM;

procedure gdk_window_input_shape_combine_mask(window: PGdkWindow;
  mask: PGdkBitmap; x, y: GInt); cdecl; external gdklib;
function gtk_widget_get_window(widget: PGtkWidget): PGdkWindow; cdecl; external gtklib;
function gdk_window_get_screen(window: PGdkWindow): PGdkScreen; cdecl; external gdklib;
function gdk_screen_is_composited(screen: PGdkScreen): gboolean; cdecl; external gdklib;

procedure FormScreenChanged(widget: PGtkWidget; old_screen: PGdkScreen;
  userdata: GPointer); cdecl;
var
  Screen: PGdkScreen;
  Colormap: PGdkColormap;
begin
  Screen := gtk_widget_get_screen(widget);
  Colormap := gdk_screen_get_rgba_colormap(Screen);
  gtk_widget_set_colormap(widget, Colormap);
end;

{ TFloatingForm }

constructor TFloatingForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BorderStyle := bsNone;
  FOpacity := $FF;
  FInteractive := True;
end;

procedure TFloatingForm.Loaded;
type
  PFormBorderStyle = ^TFormBorderStyle;
begin
  PFormBorderStyle(@BorderStyle)^ := bsNone;
  inherited Loaded;
  PFormBorderStyle(@BorderStyle)^ := bsNone;
end;

type
  PFormBorderStyle = ^TFormBorderStyle;

procedure TFloatingForm.CreateHandle;
var
  W: TGtkWindowType;
begin
  PFormBorderStyle(@BorderStyle)^ := bsNone;
  W := FormStyleMap[bsNone];
  FormStyleMap[bsNone] := GTK_WINDOW_POPUP;
  try
    inherited CreateHandle;
  finally
    FormStyleMap[bsNone] := W;
  end;
  if not (csDesigning in ComponentState) then
  begin
    FWindow := {%H-}Pointer(Handle);
    gtk_widget_set_app_paintable(PGtkWidget(FWindow), True);
    g_signal_connect(G_OBJECT(FWindow), 'screen-changed',
      G_CALLBACK(@FormScreenChanged), nil);
    FormScreenChanged(PGtkWidget(FWindow), nil, nil);
  end;
end;

procedure TFloatingForm.SetInteractive(Value: Boolean);
begin
  if FInteractive <> Value then
  begin
    FInteractive := Value;
    Invalidate;
  end;
end;

procedure TFloatingForm.Paint;
var
  Window: PGdkWindow;
  Mask: PGdkPixmap;
begin
  Window := GTK_WIDGET({%H-}Pointer(Handle)).window;
  if FInteractive then
    gdk_window_input_shape_combine_mask(Window, nil, 0, 0)
  else
  begin
    Mask := gdk_pixmap_new(nil, Width, Height, 1);
    gdk_window_input_shape_combine_mask(Window, nil, 0, 0);
    gdk_window_input_shape_combine_mask(Window, Mask, 0, 0);
    g_object_unref(Mask);
  end
end;

procedure TFloatingForm.MoveSize(Rect: TRectI);
var
  Window: PGdkWindow;
begin
  Window := GTK_WIDGET({%H-}Pointer(Handle)).window;
  gdk_window_move_resize(Window, Rect.Left, Rect.Top, Rect.Width, Rect.Height);
end;

procedure TFloatingForm.SetOpacity(Value: Byte);
begin
  if Value <> FOpacity then
  begin
    gtk_window_set_opacity(PGtkWindow(FWindow), Value / $FF);
    FOpacity := Value;
  end;
end;

function TFloatingForm.GetCompositing: Boolean;
var
  Screen: PGdkScreen;
begin
  Screen := gdk_window_get_screen(GTK_WIDGET({%H-}Pointer(Handle)).window);
  Result := gdk_screen_is_composited(screen);
end;

procedure FadeTimer(hWnd: HWND; uMsg: UINT; idEvent: UINT_PTR; dwTime: DWORD); stdcall;
var
  F: TFloatingForm absolute idEvent;
begin
  KillTimer(hWnd, UIntPtr(idEvent));
  F.FFadeTop := F.Top;
  F.FFadeMoved := True;
  F.Top := 30000;
end;

procedure TFloatingForm.SetFaded(Value: Boolean);
begin
  if FFaded <> Value then
  begin
    KillTimer(Handle, UIntPtr(Self));
    if FFadeMoved then
    begin
      FFadeMoved := False;
      Top := FFadeTop;
    end;
    FFaded := Value;
    if WindowManager.Compositing and (WindowManager.Name = 'Compiz') then
      if FFaded then
      begin
        Opacity := 0;
        SetTimer(Handle, UIntPtr(Self), 750, @FadeTimer);
      end
      else
        Opacity := $FF
    else
      Visible := not FFaded;
  end;
end;
{$elseif defined(linux) and defined(lclgtk3)}
uses
  LazGdk3, LazGtk3, Gtk3Widgets,
  Codebot.Interop.Linux.NetWM;

function floating_region_create: Pointer; cdecl;
  external 'libcairo.so.2' name 'cairo_region_create';
procedure floating_region_destroy(region: Pointer); cdecl;
  external 'libcairo.so.2' name 'cairo_region_destroy';
function floating_signal_connect(instance: Pointer; signal: PChar; handler: Pointer;
  data: Pointer; destroy_data: Pointer; flags: LongWord): LongWord; cdecl;
  external 'libgobject-2.0.so.0' name 'g_signal_connect_data';

procedure FormScreenChanged(widget: PGtkWidget; old_screen: PGdkScreen;
  userdata: Pointer); cdecl;
var
  Visual: PGdkVisual;
begin
  Visual := gdk_screen_get_rgba_visual(gtk_widget_get_screen(widget));
  if Visual <> nil then
    gtk_widget_set_visual(widget, Visual);
end;

{ TFloatingForm }

constructor TFloatingForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BorderStyle := bsNone;
  FOpacity := $FF;
  FInteractive := True;
end;

procedure TFloatingForm.Loaded;
type
  PFormBorderStyle = ^TFormBorderStyle;
begin
  PFormBorderStyle(@BorderStyle)^ := bsNone;
  inherited Loaded;
  PFormBorderStyle(@BorderStyle)^ := bsNone;
end;

type
  PFormBorderStyle = ^TFormBorderStyle;

function FormWindow(Form: TFloatingForm): PGdkWindow;
begin
  Result := nil;
  if Form.HandleAllocated then
    Result := gtk_widget_get_window(PGtkWidget(Form.FWindow));
end;

procedure TFloatingForm.CreateHandle;
begin
  PFormBorderStyle(@BorderStyle)^ := bsNone;
  { The gtk3 widgetset creates an undecorated popup window for borderless
    forms which do not take focus }
  ControlStyle := ControlStyle + [csNoFocus];
  inherited CreateHandle;
  if not (csDesigning in ComponentState) then
  begin
    FWindow := TGtk3Widget(Handle).Widget;
    gtk_widget_set_app_paintable(PGtkWidget(FWindow), True);
    floating_signal_connect(FWindow, 'screen-changed', @FormScreenChanged, nil, nil, 0);
    { The widgetset realizes the window while creating it, and a visual only
      takes effect before realization, so realize it again with an rgba visual }
    if gtk_widget_get_realized(PGtkWidget(FWindow)) then
    begin
      gtk_widget_unrealize(PGtkWidget(FWindow));
      FormScreenChanged(PGtkWidget(FWindow), nil, nil);
      gtk_widget_realize(PGtkWidget(FWindow));
      gdk_window_set_decorations(gtk_widget_get_window(PGtkWidget(FWindow)), []);
    end
    else
      FormScreenChanged(PGtkWidget(FWindow), nil, nil);
  end;
end;

procedure TFloatingForm.SetInteractive(Value: Boolean);
begin
  if FInteractive <> Value then
  begin
    FInteractive := Value;
    Invalidate;
  end;
end;

procedure TFloatingForm.Paint;
var
  Window: PGdkWindow;
  Region: Pointer;
begin
  Window := FormWindow(Self);
  if Window = nil then
    Exit;
  if FInteractive then
    gdk_window_input_shape_combine_region(Window, nil, 0, 0)
  else
  begin
    { An empty input region lets mouse clicks pass through the window }
    Region := floating_region_create;
    gdk_window_input_shape_combine_region(Window, Region, 0, 0);
    floating_region_destroy(Region);
  end;
end;

procedure TFloatingForm.MoveSize(Rect: TRectI);
begin
  { Go through the LCL so the gtk3 window and the form bounds stay in sync }
  SetBounds(Rect.Left, Rect.Top, Rect.Width, Rect.Height);
end;

procedure TFloatingForm.SetOpacity(Value: Byte);
begin
  if Value <> FOpacity then
  begin
    if FWindow <> nil then
      gtk_widget_set_opacity(PGtkWidget(FWindow), Value / $FF);
    FOpacity := Value;
  end;
end;

function TFloatingForm.GetCompositing: Boolean;
begin
  Result := (FWindow <> nil) and
    gdk_screen_is_composited(gtk_widget_get_screen(PGtkWidget(FWindow)));
end;

procedure FadeTimer(hWnd: HWND; uMsg: UINT; idEvent: UINT_PTR; dwTime: DWORD); stdcall;
var
  F: TFloatingForm absolute idEvent;
begin
  KillTimer(hWnd, UIntPtr(idEvent));
  F.FFadeTop := F.Top;
  F.FFadeMoved := True;
  F.Top := 30000;
end;

procedure TFloatingForm.SetFaded(Value: Boolean);
begin
  if FFaded <> Value then
  begin
    KillTimer(Handle, UIntPtr(Self));
    if FFadeMoved then
    begin
      FFadeMoved := False;
      Top := FFadeTop;
    end;
    FFaded := Value;
    if WindowManager.Compositing and (WindowManager.Name = 'Compiz') then
      if FFaded then
      begin
        Opacity := 0;
        SetTimer(Handle, UIntPtr(Self), 750, @FadeTimer);
      end
      else
        Opacity := $FF
    else
      Visible := not FFaded;
  end;
end;
{$else}
function TFloatingForm.GetCompositing: Boolean;
begin
  Result := False;
end;

procedure TFloatingForm.SetFaded(Value: Boolean);
begin

end;

procedure TFloatingForm.SetInteractive(Value: Boolean);
begin

end;

procedure TFloatingForm.SetOpacity(Value: Byte);
begin

end;

procedure TFloatingForm.CreateHandle;
begin
  inherited CreateHandle;
end;

procedure TFloatingForm.Loaded;
begin
  inherited Loaded;
end;

procedure TFloatingForm.Paint;
begin
  inherited Paint;
end;

constructor TFloatingForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TFloatingForm.MoveSize(Rect: TRectI);
begin

end;
{$endif}

end.
