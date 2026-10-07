(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ Codebot.Render.Controls.Child creates an SDL window and places it inside
  the native window of an LCL control, where it fills the control. SDL
  creates the OpenGL context for the window and reads its keyboard and
  mouse, the same as it does for the window of an SDL application.

  SDL has no child windows of its own. The window is created hidden as a
  top level window without a border and is then given a new parent by the
  window system, X11 on Linux or Windows.

  The window system only sends keys to the window with the keyboard focus,
  which the LCL does not know the SDL window can have. ChildFocus gives the
  focus to the SDL window and ChildUnfocus gives it back to the form.

  After it is created the window is only changed through the window system
  and never through SDL, which learns of the changes from its events. That
  keeps SDL from being used by the main thread while the render thread reads
  its events. }

unit Codebot.Render.Controls.Child;

{$i ../codebot_render/render.inc}
{$packrecords c}

interface

uses
  Codebot.OpenGL,
  Codebot.Interop.SDL2;

{ Create an SDL window with an OpenGL pixel format inside a parent window
  and show it. If it cannot be created with multisampling it is created
  without, and Params is changed to match. The result is nil if it cannot
  be created. Call it on the main thread. }
function ChildCreate(Parent: PtrUInt; Width, Height: Integer;
  var Params: TOpenGLParams): PSDL_Window;
{ Destroy the window before its parent is destroyed }
procedure ChildDestroy(Window: PSDL_Window);
{ Resize the window to fill its parent }
procedure ChildResize(Window: PSDL_Window; Width, Height: Integer);
{ Return True if the window has the keyboard focus }
function ChildFocused(Window: PSDL_Window): Boolean;
{ Give the keyboard focus to the window if it can be seen }
procedure ChildFocus(Window: PSDL_Window);
{ Give the keyboard focus back to the top level window of the form if the
  window has it. It is left alone if another program took it. }
procedure ChildUnfocus(Window: PSDL_Window; TopLevel: PtrUInt);

implementation

uses
  {$ifdef windows}
  Windows,
  {$endif}
  {$ifdef unix}
  CTypes, X, XLib,
  {$endif}
  Codebot.OpenGL.SDL;

{ SDL_syswm.h, which is not in the interop unit }

const
  SDL_SYSWM_WINDOWS = 1;
  SDL_SYSWM_X11 = 2;

type
  TSysWMInfo = record
    version: TSDL_version;
    subsystem: LongInt;
    case Integer of
      0: (win: record
          window: PtrUInt;
          hdc: PtrUInt;
          hinstance: PtrUInt;
        end);
      1: (x11: record
          display: Pointer;
          window: PtrUInt;
        end);
      2: (dummy: array[0..63] of Byte);
  end;

function SDL_GetWindowWMInfo(window: PSDL_Window; var info: TSysWMInfo): SDL_Bool;
  cdecl; external 'SDL2';

function WindowInfo(Window: PSDL_Window; out Info: TSysWMInfo): Boolean;
begin
  FillChar(Info, SizeOf(Info), 0);
  Info.version.major := 2;
  Result := (Window <> nil) and SDL_GetWindowWMInfo(Window, Info);
end;

{$ifdef unix}
function NativeWindow(Window: PSDL_Window; out Display: PDisplay; out Handle: TWindow): Boolean;
var
  Info: TSysWMInfo;
begin
  Display := nil;
  Handle := 0;
  Result := WindowInfo(Window, Info) and (Info.subsystem = SDL_SYSWM_X11);
  if Result then
  begin
    Display := PDisplay(Info.x11.display);
    Handle := Info.x11.window;
    Result := (Display <> nil) and (Handle <> 0);
  end;
end;

function Embed(Window: PSDL_Window; Parent: PtrUInt; Width, Height: Integer): Boolean;
var
  D: PDisplay;
  W: TWindow;
begin
  Result := NativeWindow(Window, D, W);
  if not Result then
    Exit;
  XReparentWindow(D, W, Parent, 0, 0);
  XMoveResizeWindow(D, W, 0, 0, Width, Height);
  XMapWindow(D, W);
  XSync(D, 0);
end;

procedure ChildResize(Window: PSDL_Window; Width, Height: Integer);
var
  D: PDisplay;
  W: TWindow;
begin
  if (Width < 1) or (Height < 1) then
    Exit;
  if not NativeWindow(Window, D, W) then
    Exit;
  XMoveResizeWindow(D, W, 0, 0, Width, Height);
  XFlush(D);
end;

function ChildFocused(Window: PSDL_Window): Boolean;
var
  D: PDisplay;
  W, F: TWindow;
  R: cint;
begin
  Result := False;
  if not NativeWindow(Window, D, W) then
    Exit;
  F := 0;
  R := 0;
  XGetInputFocus(D, @F, @R);
  Result := F = W;
end;

{ Giving the focus to a window which cannot be seen is an X error, which
  would end the program }

procedure ChildFocus(Window: PSDL_Window);
var
  D: PDisplay;
  W: TWindow;
  A: TXWindowAttributes;
begin
  if not NativeWindow(Window, D, W) then
    Exit;
  if XGetWindowAttributes(D, W, @A) = 0 then
    Exit;
  if A.map_state <> IsViewable then
    Exit;
  XSetInputFocus(D, W, RevertToParent, CurrentTime);
  XFlush(D);
end;

procedure ChildUnfocus(Window: PSDL_Window; TopLevel: PtrUInt);
var
  D: PDisplay;
  W: TWindow;
  A: TXWindowAttributes;
begin
  if TopLevel = 0 then
    Exit;
  if not ChildFocused(Window) then
    Exit;
  if not NativeWindow(Window, D, W) then
    Exit;
  if XGetWindowAttributes(D, TopLevel, @A) = 0 then
    Exit;
  if A.map_state <> IsViewable then
    Exit;
  XSetInputFocus(D, TopLevel, RevertToParent, CurrentTime);
  XFlush(D);
end;
{$endif}

{$ifdef windows}
function NativeWindow(Window: PSDL_Window; out Handle: HWND): Boolean;
var
  Info: TSysWMInfo;
begin
  Handle := 0;
  Result := WindowInfo(Window, Info) and (Info.subsystem = SDL_SYSWM_WINDOWS);
  if Result then
  begin
    Handle := HWND(Info.win.window);
    Result := Handle <> 0;
  end;
end;

function Embed(Window: PSDL_Window; Parent: PtrUInt; Width, Height: Integer): Boolean;
var
  H: HWND;
begin
  Result := NativeWindow(Window, H);
  if not Result then
    Exit;
  SetWindowLongPtr(H, GWL_STYLE, PtrInt(WS_CHILD or WS_CLIPCHILDREN or WS_CLIPSIBLINGS));
  Windows.SetParent(H, HWND(Parent));
  SetWindowPos(H, 0, 0, 0, Width, Height, SWP_NOZORDER or SWP_NOACTIVATE or
    SWP_FRAMECHANGED);
  ShowWindow(H, SW_SHOWNA);
end;

procedure ChildResize(Window: PSDL_Window; Width, Height: Integer);
var
  H: HWND;
begin
  if (Width < 1) or (Height < 1) then
    Exit;
  if NativeWindow(Window, H) then
    SetWindowPos(H, 0, 0, 0, Width, Height, SWP_NOZORDER or SWP_NOACTIVATE);
end;

function ChildFocused(Window: PSDL_Window): Boolean;
var
  H: HWND;
begin
  Result := NativeWindow(Window, H) and (Windows.GetFocus = H);
end;

procedure ChildFocus(Window: PSDL_Window);
var
  H: HWND;
begin
  if NativeWindow(Window, H) and IsWindowVisible(H) then
    Windows.SetFocus(H);
end;

{ Windows moves the keyboard focus between windows itself }

procedure ChildUnfocus(Window: PSDL_Window; TopLevel: PtrUInt);
begin
end;
{$endif}

function ChildCreate(Parent: PtrUInt; Width, Height: Integer;
  var Params: TOpenGLParams): PSDL_Window;
const
  Flags = SDL_WINDOW_OPENGL or SDL_WINDOW_BORDERLESS or SDL_WINDOW_HIDDEN;
begin
  Result := nil;
  if Parent = 0 then
    Exit;
  if Width < 1 then
    Width := 1;
  if Height < 1 then
    Height := 1;
  {$ifdef unix}
  { The parent is an X window, so SDL must use X as well }
  SDL_SetHint('SDL_VIDEODRIVER', 'x11');
  {$endif}
  if SDL_InitSubSystem(SDL_INIT_VIDEO) < 0 then
    Exit;
  { SDL chooses the pixel format of a window when it is created }
  OpenGLSetAttributes(Params);
  Result := SDL_CreateWindow('', 0, 0, Width, Height, Flags);
  if (Result = nil) and Params.MultiSampling then
  begin
    Params.MultiSampling := False;
    OpenGLSetAttributes(Params);
    Result := SDL_CreateWindow('', 0, 0, Width, Height, Flags);
  end;
  if (Result <> nil) and (not Embed(Result, Parent, Width, Height)) then
  begin
    SDL_DestroyWindow(Result);
    Result := nil;
  end;
  if Result = nil then
  begin
    SDL_QuitSubSystem(SDL_INIT_VIDEO);
    Exit;
  end;
  { Text typed is sent as text events as well as key events }
  SDL_StartTextInput;
end;

procedure ChildDestroy(Window: PSDL_Window);
begin
  if Window = nil then
    Exit;
  SDL_DestroyWindow(Window);
  SDL_QuitSubSystem(SDL_INIT_VIDEO);
end;

end.
