(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified November 2015                              *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.forms.management.txt> }
unit Codebot.Forms.Management;

{$i ../codebot/codebot.inc}

interface

uses
  Classes, SysUtils, Graphics, Controls, Forms,
  Codebot.System;

{ FormManager provides access to the active form and default font }

type
  FormManager = record
  private
    class var FDefaultFont: TFont;
    class function GetCurrent: TCustomForm; static;
    class function GetDefaultFont: TFont; static;
  public
    { Bring a form to the foreground and activate it }
    class procedure Activate(Form: TCustomForm); static;
    { Return the form containing a control or nil if there is none }
    class function ParentForm(Control: TControl): TCustomForm; static;
    { The form in the foreground or nil if it does not belong to this program }
    class property Current: TCustomForm read GetCurrent;
    { The default font used by forms }
    class property DefaulFont: TFont read GetDefaultFont;
  end;

implementation

{$if defined(linux) and defined(lclgtk2)}
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
    W := GTK_WIDGET({%H-}PGtkWidget(F.Handle)).window;
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
{$elseif defined(windows)}
uses
  Windows;

class procedure FormManager.Activate(Form: TCustomForm);
begin
  if (Form = nil) or (not Form.HandleAllocated) then
    Exit;
  if IsIconic(Form.Handle) then
    ShowWindow(Form.Handle, SW_RESTORE);
  SetForegroundWindow(Form.Handle);
end;

class function FormManager.GetCurrent: TCustomForm;
var
  Window: HWND;
  Form: TCustomForm;
  I: Integer;
begin
  Window := GetForegroundWindow;
  for I := 0 to Screen.CustomFormCount - 1 do
  begin
    Form := Screen.CustomForms[I];
    if Form.HandleAllocated and (Form.Handle = Window) then
      Exit(Form);
  end;
  Result := nil;
end;

class function FormManager.GetDefaultFont: Graphics.TFont;
var
  Metrics: TNonClientMetrics;
begin
  Result := FDefaultFont;
  if Result <> nil then
    Exit;
  FDefaultFont := Graphics.TFont.Create;
  { The message font is the font Windows uses for dialogs and messages }
  FillChar(Metrics, SizeOf(Metrics), 0);
  Metrics.cbSize := SizeOf(Metrics);
  if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, SizeOf(Metrics), @Metrics, 0) then
  begin
    FDefaultFont.Name := Metrics.lfMessageFont.lfFaceName;
    FDefaultFont.Size := Round(Abs(Metrics.lfMessageFont.lfHeight) * 72 /
      Screen.PixelsPerInch);
  end
  else
  begin
    FDefaultFont.Name := 'Segoe UI';
    FDefaultFont.Size := 9;
  end;
  Result := FDefaultFont;
end;
{$else}
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
