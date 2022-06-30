(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified November 2015                              *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.interop.linux.netwm.txt> }
unit Codebot.Interop.Linux.NetWM;

{$i codebot.inc}

interface

{$ifdef linux}
uses
   SysUtils, X, XLib;

{ WindowManager is a static type implementing SOME of the
  NetWM protocol. It is a starter type. }

type
  WindowManager = record
  private
    class var FName: string;
    class var FDesktopEnvironment: string;
    class function GetForegroundWindow: TWindow; static;
    class function GetCompositing: Boolean; static;
    class function GetName: string; static;
    class function GetDesktopEnvironment: string; static;
    class function SetState(Window: TWindow; State: string;
      Active: Boolean; Toggle: Boolean = False): Boolean; static;
  public
    { Show or hide all the windows on your current workspace }
    class function ShowDesktop(Show: Boolean): Boolean; static;
    { Switch workspace, bring a window to the foreground, and give it input }
    class function Activate(Window: TWindow): Boolean; static;
    { Stick a window to the same place in all workspaces }
    class function Sticky(Window: TWindow; Stick: Boolean): Boolean; static;
    { Roll a window up to its title bar }
    class function Shaded(Window: TWindow; Shade: Boolean): Boolean; static;
    { Toggle window taking up the entire screen with no decorations }
    class function Fullscreen(Window: TWindow): Boolean; static;
    { Make window stay above all other windows, even when not active }
    class function Above(Window: TWindow; Topmost: Boolean): Boolean; static;
    { Asks the window manage to bring attention to the window }
    class function Attention(Window: TWindow): Boolean; static;
    { The foreground window is the window with input focus }
    class property ForegroundWindow: TWindow read GetForegroundWindow;
    { Compositing is true when the window manager renders windows to textures }
    class property Compositing: Boolean read GetCompositing;
    { The name of the window manager such as Compiz, KWin, Mutter, Metacity }
    class property Name: string read GetName;
    { The name of the desktop environment such as Unity, KDE, Xcfe }
    class property DesktopEnvironment: string read GetDesktopEnvironment;
  end;
{$endif}

implementation

{$ifdef linux}
function SendMessage(Display: PDisplay; Window: TWindow;
  Msg: PChar; Param0: LongWord = 0; Param1: LongWord = 0;
  Param2: LongWord = 0; Param3: LongWord = 0; Param4: LongWord = 0): Boolean; overload;
var
  Event: TXEvent;
  Mask: LongWord;
begin
  Mask := SubstructureRedirectMask or SubstructureNotifyMask;
  Event.xclient._type := ClientMessage;
  Event.xclient.serial := 0;
  Event.xclient.send_event := 1;
  Event.xclient.message_type := XInternAtom(Display, Msg, False);
  Event.xclient.window := Window;
  Event.xclient.format := 32;
  Event.xclient.data.l[0] := Param0;
  Event.xclient.data.l[1] := Param1;
  Event.xclient.data.l[2] := Param2;
  Event.xclient.data.l[3] := Param3;
  Event.xclient.data.l[4] := Param4;
  Result := XSendEvent(Display, DefaultRootWindow(Display), False, Mask, @Event) <> 0;
end;

function SendMessage(Msg: PChar; Param: LongWord): Boolean; overload;
var
  Display: PDisplay;
begin
  Display := XOpenDisplay(nil);
  Result := SendMessage(Display, DefaultRootWindow(Display), Msg, Param);
  XCloseDisplay(Display);
end;

{ WindowManager }

class function WindowManager.ShowDesktop(Show: Boolean): Boolean;
const
  BoolFlags: array[Boolean] of LongWord = (0, 1);
begin
  Result := SendMessage('_NET_SHOWING_DESKTOP', BoolFlags[Show]);
end;

class function WindowManager.GetForegroundWindow: TWindow;
var
  Display: PDisplay;
  Root: TWindow;
  Prop, Actual: TAtom;
  Dummy: LongWord;
  Data: PChar;
begin
  Result := 0;
  Display := XOpenDisplay(nil);
  try
    Root := DefaultRootWindow(Display);
    Prop := XInternAtom(Display, '_NET_ACTIVE_WINDOW', False);
    Data := nil;
    if (XGetWindowProperty(Display, Root, Prop, 0, not 0, False,
      AnyPropertyType, @Actual, @Dummy, @Dummy, @Dummy,
      @Data) = Success) and (Data <> nil) then
    begin
      Result := PWindow(Data)^;
      XFree(Data);
    end;
  finally
    XCloseDisplay(Display);
  end;
end;

class function WindowManager.GetName: string;
var
  Display: PDisplay;
  Root, Support: TWindow;
  Prop, Actual: TAtom;
  Dummy: LongWord;
  Data: PChar;
begin
  Result := FName;
  if Result <> '' then
    Exit;
  Display := XOpenDisplay(nil);
  try
    Root := DefaultRootWindow(Display);
    Prop := XInternAtom(Display, '_NET_SUPPORTING_WM_CHECK', False);
    Support := 0;
    Data := nil;
    if (XGetWindowProperty(Display, Root, Prop, 0, not 0, False,
      AnyPropertyType, @Actual, @Dummy, @Dummy, @Dummy,
      @Data) = Success) and (Data <> nil) then
    begin
      Support := PWindow(Data)^;
      XFree(Data);
      Prop := XInternAtom(Display, '_NET_WM_NAME', False);
      if (XGetWindowProperty(Display, Support, Prop, 0, not 0, False,
        AnyPropertyType, @Actual, @Dummy, @Dummy, @Dummy,
        @Data) = Success) and (Data <> nil) then
      begin
        Result := Data;
        XFree(Data);
      end;
    end;
  finally
    XCloseDisplay(Display);
  end;
  FName := Result;
end;

class function WindowManager.GetCompositing: Boolean;
var
  Display: PDisplay;
  Prop: TAtom;
begin
  Display := XOpenDisplay(nil);
  Prop := XInternAtom(Display,'_NET_WM_CM_S0', False);
  Result := XGetSelectionOwner(Display, Prop) <> Prop;
  XCloseDisplay(Display);
end;

class function WindowManager.GetDesktopEnvironment: string;
begin
  Result := FDesktopEnvironment;
  if Result <> '' then
    Exit;
  Result := GetEnvironmentVariable('XDG_CURRENT_DESKTOP');
  FDesktopEnvironment := Result;
end;

class function WindowManager.Activate(Window: TWindow): Boolean;
var
  Display: PDisplay;
begin
  if Window = 0 then
    Exit;
  Display := XOpenDisplay(nil);
  Result := SendMessage(Display, Window, '_NET_ACTIVE_WINDOW');
  XMapRaised(Display, Window);
  XCloseDisplay(Display);
end;

class function WindowManager.SetState(Window: TWindow; State: string; Active: Boolean;
  Toggle: Boolean = False): Boolean;
const
  _NET_WM_STATE_REMOVE       = 0;    { remove/unset property }
  _NET_WM_STATE_ADD          = 1;    { add/set property }
  _NET_WM_STATE_TOGGLE       = 2;    { toggle property }
  StateFlags: array[Boolean] of LongWord = (_NET_WM_STATE_REMOVE, _NET_WM_STATE_ADD);
var
  Display: PDisplay;
  S: string;
  P0, P1: LongWord;
begin
  if Window = 0 then
    Exit;
  Display := XOpenDisplay(nil);
  P0 := StateFlags[Active];
  if Toggle then
    P0 := _NET_WM_STATE_TOGGLE;
  S := '_NET_WM_STATE_' + State;
  P1 := XInternAtom(Display, PChar(S), False);
  Result := SendMessage(Display, Window, '_NET_WM_STATE', P0, P1);
  XCloseDisplay(Display);
end;

class function WindowManager.Sticky(Window: TWindow; Stick: Boolean): Boolean;
begin
  Result := SetState(Window, 'STICKY', Stick);
end;

class function WindowManager.Shaded(Window: TWindow; Shade: Boolean): Boolean;
begin
  Result := SetState(Window, 'SHADED', Shade);
end;

class function WindowManager.Fullscreen(Window: TWindow): Boolean;
begin
  Result := SetState(Window, 'FULLSCREEN', False, True);
end;

class function WindowManager.Above(Window: TWindow; Topmost: Boolean): Boolean;
begin
  Result := SetState(Window, 'ABOVE', Topmost);
end;

class function WindowManager.Attention(Window: TWindow): Boolean;
begin
  Result := SetState(Window, 'DEMANDS_ATTENTION', True);
end;
{$endif}

end.

