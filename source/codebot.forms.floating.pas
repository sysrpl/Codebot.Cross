(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified November 2015                              *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.forms.floating.txt> }
unit Codebot.Forms.Floating;

{$i codebot.inc}

interface

uses
  Classes, SysUtils, Controls, Forms;

{ TFloatingForm }

type
  TFloatingForm = class(TForm)
  private
    FWindow: Pointer;
    FOpacity: Byte;
    procedure SetOpacity(Value: Byte);
  protected
    procedure CreateHandle; override;
    property Opacity: Byte read FOpacity write SetOpacity;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$if defined(linux) and defined(lclgtk2)}
uses
  GLib2, Gdk2, Gtk2, Gtk2Extra;

constructor TFloatingForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BorderStyle := bsNone;
  FOpacity := $FF;
end;

procedure gdk_window_input_shape_combine_mask (window: PGdkWindow;
  mask: PGdkBitmap; x, y: GInt); cdecl; external gdklib;
function gtk_widget_get_window(widget: PGtkWidget): PGdkWindow; cdecl; external gtklib;

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

procedure TFloatingForm.CreateHandle;
begin
  inherited CreateHandle;
  FWindow := Pointer(Handle);
  gtk_widget_set_app_paintable(PGtkWidget(FWindow), True);
  g_signal_connect(G_OBJECT(FWindow), 'screen-changed',
    G_CALLBACK(@FormScreenChanged), nil);
  FormScreenChanged(PGtkWidget(FWindow), nil, nil);
end;

procedure TFloatingForm.SetOpacity(Value: Byte);
begin
  if Value <> FOpacity then
  begin
    gtk_window_set_opacity(PGtkWindow(FWindow), Value / $FF);
    FOpacity := Value;
  end;
end;
{$endif}

end.
