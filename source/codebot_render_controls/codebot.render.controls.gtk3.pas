(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.render.controls.gtk3.txt> }
unit Codebot.Render.Controls.Gtk3;

{$i ../codebot_render/render.inc}

interface

{$ifdef gtk3gl}
uses
  Classes, SysUtils, Controls, LCLType, WSControls, WSLCLClasses;

type
  TWSOpenGLWindow = class(TWSWinControl)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND; override;
    { The native window of the control which the SDL window is placed
      inside, or 0 if there is none }
    class function NativeWindow(AWinControl: TWinControl): PtrUInt;
    { The native window of the form holding the control, which has the
      keyboard focus of the window system while the form is active }
    class function TopLevelWindow(AWinControl: TWinControl): PtrUInt;
  end;
{$endif}

implementation

{$ifdef gtk3gl}
uses
  Forms, LMessages, LazGLib2, LazGdk3, LazGtk3, Gtk3Int, Gtk3Procs, Gtk3Widgets;

function gdk_x11_window_get_xid(window: PGdkWindow): PtrUInt; cdecl; external LazGdk3_library;
procedure gtk_widget_set_double_buffered(widget: PGtkWidget; double_buffered: gboolean); cdecl; external LazGtk3_library;

{ TGtk3OpenGLWidget is a drawing area with its own native X window, which
  the SDL window is placed inside }

type
  TGtk3OpenGLWidget = class(TGtk3Widget)
  protected
    function CreateWidget(const Params: TCreateParams): PGtkWidget; override;
  public
    function GtkEventMouseMove(Sender: PGtkWidget; Event: PGdkEvent): Boolean; override; cdecl;
  end;

function TGtk3OpenGLWidget.CreateWidget(const Params: TCreateParams): PGtkWidget;
begin
  FHasPaint := True;
  Result := PGtkWidget(TGtkDrawingArea.new);
  gtk_widget_set_double_buffered(Result, False);
  { A drawing area cannot take the keyboard focus unless asked to, and only
    the focused widget receives key events. The LCL adds the key event masks
    when it initializes the widget. }
  Result^.set_can_focus(True);
end;

{ GtkEventMouseMove replaces the one inherited from TGtk3Widget, which drops
  a mouse move when the position inside the widget equals the position of the
  cursor on the screen. That is always the case when the widget is at the top
  left of the screen, as it is on a full screen form, so no moves arrived and
  nothing could be dragged.

  The widget uses motion hints, so the position is always asked for, which
  also tells gdk to send the next motion event. }

function TGtk3OpenGLWidget.GtkEventMouseMove(Sender: PGtkWidget; Event: PGdkEvent): Boolean; cdecl;
var
  Msg: TLMMouseMove;
  Seat: PGdkSeat;
  Device: PGdkDevice;
  Mask: TGdkModifierType;
  X, Y: gint;
begin
  Result := False;
  if Event^.motion.send_event = NO_PROPAGATION_TO_PARENT then
    Exit;
  if (LCLObject = nil) or (FWidget^.get_window = nil) then
    Exit;
  Seat := gdk_display_get_default_seat(gtk_widget_get_display(Sender));
  Device := gdk_seat_get_pointer(Seat);
  X := 0;
  Y := 0;
  Mask := Event^.motion.state;
  gdk_window_get_device_position(FWidget^.get_window, Device, @X, @Y, @Mask);
  FillChar(Msg{%H-}, SizeOf(Msg), #0);
  Msg.Msg := LM_MOUSEMOVE;
  Msg.XPos := SmallInt(X);
  Msg.YPos := SmallInt(Y);
  Msg.Keys := GdkModifierStateToLCL(Mask, False);
  NotifyApplicationUserInput(LCLObject, PLMessage(@Msg)^);
  if Widget^.get_parent <> nil then
    Event^.motion.send_event := NO_PROPAGATION_TO_PARENT;
  DeliverMessage(Msg, True);
end;

{ Nothing is rendered at design time, so the designer gets the same widget as
  any other windowed control. The OpenGL widget is only used at runtime. }

class function TWSOpenGLWindow.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND;
begin
  if csDesigning in AWinControl.ComponentState then
    Result := {%H-}TLCLHandle(TGtk3WinControlPanel.Create(AWinControl, AParams))
  else
    Result := {%H-}TLCLHandle(TGtk3OpenGLWidget.Create(AWinControl, AParams));
end;

class function TWSOpenGLWindow.NativeWindow(AWinControl: TWinControl): PtrUInt;
var
  Widget: PGtkWidget;
  Window: PGdkWindow;
begin
  Result := 0;
  { An X window is required, which is not available under wayland }
  if Gtk3WidgetSet.IsWayland then
    Exit;
  Widget := TGtk3Widget(AWinControl.Handle).Widget;
  if Widget = nil then
    Exit;
  if not Widget^.get_realized then
    Widget^.realize;
  Window := Widget^.get_window;
  if (Window = nil) or (not Window^.ensure_native) then
    Exit;
  { The native window was just created on the gtk display connection. Sync
    that connection so the window exists on the X server before SDL uses it
    from a different connection. }
  gdk_display_sync(gdk_window_get_display(Window));
  Result := gdk_x11_window_get_xid(Window);
end;

class function TWSOpenGLWindow.TopLevelWindow(AWinControl: TWinControl): PtrUInt;
var
  Widget: PGtkWidget;
  Window: PGdkWindow;
begin
  Result := 0;
  if Gtk3WidgetSet.IsWayland then
    Exit;
  Widget := TGtk3Widget(AWinControl.Handle).Widget;
  if Widget = nil then
    Exit;
  Widget := Widget^.get_toplevel;
  if Widget = nil then
    Exit;
  Window := Widget^.get_window;
  if Window <> nil then
    Result := gdk_x11_window_get_xid(Window);
end;
{$endif}

end.
