(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified July 2022                                  *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.render.controls.gtk2.txt> }
unit Codebot.Render.Controls.Gtk2;

{$i ../codebot_render/render.inc}

interface

{$ifdef gtk2gl}
uses
  Classes, SysUtils, Controls, LCLType, LCLIntf, WSControls, WSLCLClasses;

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

{$ifdef gtk2gl}
uses
  Gdk2, Gdk2x, Gtk2, Gtk2Int, Gtk2Def, Gtk2Globals, Gtk2Proc;

class function TWSOpenGLWindow.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND;
var
  Widget: PGtkWidget;
  Info: PWidgetInfo;
begin
  Widget := gtk_drawing_area_new;
  Info := GetOrCreateWidgetInfo(Widget);
  Info.LCLObject := AWinControl;
  Info.ClientWidget := Widget;
  gtk_widget_set_double_buffered(Widget, False);
  GTK_WIDGET_UNSET_FLAGS(Widget, GTK_NO_WINDOW);
  if AParams.Style and WS_VISIBLE = 0 then
    gtk_widget_hide(Widget)
  else
    gtk_widget_show(Widget);
  GTK2WidgetSet.SetCommonCallbacks(PGtkObject(Widget), AWinControl);
  Result := {%H-}TLCLIntfHandle(Widget);
end;

class function TWSOpenGLWindow.NativeWindow(AWinControl: TWinControl): PtrUInt;
var
  Widget: PGtkWidget;
begin
  Result := 0;
  Widget := {%H-}PGtkWidget(AWinControl.Handle);
  if Widget = nil then
    Exit;
  if Widget^.window = nil then
    gtk_widget_realize(Widget);
  if Widget^.window = nil then
    Exit;
  { Sync the gtk display connection so the window exists on the X server
    before SDL uses it from a different connection }
  gdk_flush;
  Result := GDK_WINDOW_XWINDOW(Widget^.window);
end;

class function TWSOpenGLWindow.TopLevelWindow(AWinControl: TWinControl): PtrUInt;
var
  Widget: PGtkWidget;
begin
  Result := 0;
  Widget := {%H-}PGtkWidget(AWinControl.Handle);
  if Widget = nil then
    Exit;
  Widget := gtk_widget_get_toplevel(Widget);
  if (Widget <> nil) and (Widget^.window <> nil) then
    Result := GDK_WINDOW_XWINDOW(Widget^.window);
end;
{$endif}

end.

