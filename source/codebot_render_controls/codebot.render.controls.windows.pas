(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.render.controls.windows.txt> }
unit Codebot.Render.Controls.Windows;

{$i ../codebot_render/render.inc}

interface

{$ifdef win32gl}
uses
  Windows, Classes, SysUtils, Controls, LCLType, WSControls, WSLCLClasses;

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

{$ifdef win32gl}
uses
  Win32Int, Win32WSControls;

class function TWSOpenGLWindow.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  PrepareCreateWindow(AWinControl, AParams, Params);
  with Params do
  begin
    pClassName := @ClsName[0];
    WindowTitle := StrCaption;
    { The SDL window placed inside is clipped from the surface of the control }
    Flags := Flags or WS_CLIPCHILDREN or WS_CLIPSIBLINGS;
  end;
  FinishCreateWindow(AWinControl, Params, False);
  Result := Params.Window;
end;

class function TWSOpenGLWindow.NativeWindow(AWinControl: TWinControl): PtrUInt;
begin
  Result := AWinControl.Handle;
end;

{ Windows moves the keyboard focus between windows itself }

class function TWSOpenGLWindow.TopLevelWindow(AWinControl: TWinControl): PtrUInt;
begin
  Result := 0;
end;
{$endif}

end.
