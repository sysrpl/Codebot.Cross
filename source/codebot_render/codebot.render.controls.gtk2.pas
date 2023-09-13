(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified July 2022                                  *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.render.controls.gtk2.txt> }
unit Codebot.Render.Controls.Gtk2;

{$i render.inc}

interface

{$ifdef gtk2gl}
uses
  Classes, SysUtils, Controls, LCLType, LCLIntf, WSControls, WSLCLClasses,
  Codebot.GLES;

type
  TWSOpenGLWindow = class(TWSWinControl)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND; override;
    class function CreateContext(AWinControl: TWinControl; const Params: TOpenGLParams): IOpenGLContext;
  end;
{$endif}

implementation

{$ifdef gtk2gl}
uses
  Gdk2x, Gtk2, Gtk2Int, Gtk2Def, Gtk2Globals, Gtk2Proc;

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

class function TWSOpenGLWindow.CreateContext(AWinControl: TWinControl; const Params: TOpenGLParams): IOpenGLContext;
var
  W: GLwindow;
begin
  W := GDK_WINDOW_XWINDOW({%H-}PGtkWidget(AWinControl.Handle)^.window);
  Result := OpenGLContextCreate(W, Params);
end;
{$endif}

end.

