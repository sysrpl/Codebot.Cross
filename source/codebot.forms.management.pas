(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified November 2015                              *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.forms.management.txt> }
unit Codebot.Forms.Management;

{$i codebot.inc}

interface

uses
  Classes, SysUtils, Controls, Forms;

procedure FormActivate(Form: TCustomForm);
function FormCurrent: TCustomForm;
function FormParent(Control: TControl): TCustomForm;

implementation

{$if defined(linux) and defined(lclgtk2)}
uses
  X, Gtk2, Gdk2, Gdk2X,
  Codebot.Interop.Linux.NetWM;

function XWindow(Control: TControl): TWindow;
var
  F: TCustomForm;
  W: PGdkWindow;
begin
  Result := 0;
  F := FormParent(Control);
  if F <> nil then
  begin
    W := GTK_WIDGET(PGtkWidget(F.Handle)).window;
    Result := gdk_x11_drawable_get_xid(W);
  end;
end;

procedure FormActivate(Form: TCustomForm);
begin
  WindowManager.Activate(XWindow(Form));
end;

function FormCurrent: TCustomForm;
var
  Window: TWindow;
  Form: TCustomForm;
  I: Integer;
begin
  Window := WindowManager.GetForegroundWindow;
  for I := 0 to Screen.FormCount - 1 do
  begin
    Form := Screen.Forms[I];
    if XWindow(Form) = Window then
      Exit(Form);
  end;
  Result:= nil;
end;
{$endif}

function FormParent(Control:TControl): TCustomForm;
var
  P: TWinControl;
begin
  Result := nil;
  if Control = nil then
    Exit;
  if Control is TWinControl then
    P := Control as TWinControl
  else
    P := Control.Parent;
  while P.Parent <> nil do
    P := P.Parent;
  if P is TCustomForm then
    Result := P as TCustomForm;
end;

end.
