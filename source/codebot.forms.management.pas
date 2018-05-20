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
  Classes, SysUtils, Graphics, Controls, Forms,
  Codebot.System;

{ FormManager }

{$if defined(linuxgtk) or defined(windows)}
type
  FormManager = record
  private
    class var {%H-}FDefaultFont: TFont;
    class function GetCurrent: TCustomForm; static;
    class function GetDefaultFont: TFont; static;
  public
    class procedure Activate(Form: TCustomForm); static;
    class function ParentForm(Control: TControl): TCustomForm; static;
    class property Current: TCustomForm read GetCurrent;
    class property DefaulFont: TFont read GetDefaultFont;
  end;
{$endif}

implementation

{$if defined(linuxgtk)}
uses
  X, GLib2, Gtk2, Gdk2, Gdk2X,
  Codebot.Interop.Linux.NetWM;

function XWindow(Control: TControl): TWindow;
var
  F: TCustomForm;
  W: PGdkWindow;
begin
  Result := 0;
  F := FormManager.ParentForm(Control);
  if F <> nil then
  begin
    W := GTK_WIDGET(PGtkWidget(F.Handle)).window;
    Result := gdk_x11_drawable_get_xid(W);
  end;
end;

class procedure FormManager.Activate(Form: TCustomForm);
begin
  WindowManager.Activate(XWindow(Form));
end;

class function FormManager.GetCurrent: TCustomForm;
var
  Window: TWindow;
  Form: TCustomForm;
  I: Integer;
begin
  Window := WindowManager.ForegroundWindow;
  for I := 0 to Screen.FormCount - 1 do
  begin
    Form := Screen.Forms[I];
    if XWindow(Form) = Window then
      Exit(Form);
  end;
  Result:= nil;
end;

class function FormManager.GetDefaultFont: Graphics.TFont;
var
  Items: StringArray;
  S: string;
  P: PChar;
begin
  Result := FDefaultFont;
  if Result <> nil then
    Exit;
  FDefaultFont := Graphics.TFont.Create;
  g_object_get(gtk_settings_get_default, 'gtk-font-name', [@P, nil]);
  S := P;
  g_free(P);
  Items := S.Split(' ');
  FDefaultFont.Size := StrToInt(Items.Pop);
  FDefaultFont.Name := Items.Join(' ');
  Result := FDefaultFont;
end;
{$endif}

{$if defined(windows)}
class function FormManager.GetCurrent: TCustomForm;
begin
  Result := nil;
end;

class function FormManager.GetDefaultFont: TFont;
begin
  Result := nil;
end;

class procedure FormManager.Activate(Form: TCustomForm);
begin
end;
{$endif}

class function FormManager.ParentForm(Control:TControl): TCustomForm;
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
