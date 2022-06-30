(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2015                               *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.input.mousequeue.txt> }
unit Codebot.Input.MouseMonitor;

{$i codebot.inc}

interface

uses
  Classes, SysUtils, Controls,
  LCLType, LCLIntf,
  Codebot.System;

type
  TMouseNotifyKind = (nkMove, nkButtonDown, nkButtonUp);

  TMouseNotifyEvent = procedure(Kind: TMouseNotifyKind;
    Button: TMouseButton; X, Y: Integer) of object;

  TMouseDelegate = TDelegate<TMouseNotifyEvent>;

{ TMouseMonitor }

  TMouseMonitor = class
  private
    FThread: TThread;
    FEvents: TMouseDelegate;
  public
    destructor Destroy; override;
    procedure Add(Notify: TMouseNotifyEvent);
    procedure Remove(Notify: TMouseNotifyEvent);
  end;

function MouseMonitor: TMouseMonitor;

implementation

type
  TMouseState = record
    Kind: TMouseNotifyKind;
    Button: TMouseButton;
    Position: TPoint;
    LeftDown: Boolean;
    MiddleDown: Boolean;
    RightDown: Boolean;
  end;

{ TMouseThread }

  TMouseThread = class(TThread)
  private
    FMonitor: TMouseMonitor;
    FState: TMouseState;
    procedure QueryMouse;
  protected
    procedure Execute; override;
  public
    constructor Create(Monitor: TMouseMonitor);
  end;

var
  InternalMouseMonitor: TMouseMonitor;

function MouseMonitor: TMouseMonitor;
begin
  if InternalMouseMonitor = nil then
    InternalMouseMonitor := TMouseMonitor.Create;
  Result := InternalMouseMonitor;
end;

{ TMouseThread }
{$ifdef windows}
const
  Pressed = $8000;
{$else}
const
  Pressed = $FFFFFFFF;
{$endif}

constructor TMouseThread.Create(Monitor: TMouseMonitor);
begin
  FMonitor := Monitor;
  FState.Position := Mouse.CursorPos;
  FState.LeftDown := GetKeyState(VK_LBUTTON) and Pressed <> 0;
  FState.MiddleDown := GetKeyState(VK_MBUTTON) and Pressed  <> 0;
  FState.RightDown := GetKeyState(VK_RBUTTON) and Pressed <> 0;
  FreeOnTerminate := True;
  inherited Create(False);
end;

procedure TMouseThread.QueryMouse;
var
  Change: TMouseState;
  Event: TMouseNotifyEvent;
begin
  if Terminated then
    Exit;
  Change.Position := Mouse.CursorPos;
  Change.Button := mbLeft;
  if (FState.Position.X <> Change.Position.X) or
    (FState.Position.Y <> Change.Position.Y) then
  begin
    Change.Kind := nkMove;
    for Event in FMonitor.FEvents do
      Event(Change.Kind, Change.Button, Change.Position.X, Change.Position.Y);
  end;
  Change.LeftDown := GetKeyState(VK_LBUTTON) and Pressed <> 0;
  if FState.LeftDown <> Change.LeftDown then
  begin
    if Change.LeftDown then
      Change.Kind := nkButtonDown
    else
      Change.Kind := nkButtonUp;
    Change.Button := mbLeft;
    for Event in FMonitor.FEvents do
      Event(Change.Kind, Change.Button, Change.Position.X, Change.Position.Y);
  end;
  Change.MiddleDown := GetKeyState(VK_MBUTTON) and Pressed <> 0;
  if FState.MiddleDown <> Change.MiddleDown then
  begin
    if Change.MiddleDown then
      Change.Kind := nkButtonDown
    else
      Change.Kind := nkButtonUp;
    Change.Button := mbMiddle;
    for Event in FMonitor.FEvents do
      Event(Change.Kind, Change.Button, Change.Position.X, Change.Position.Y);
  end;
  Change.RightDown := GetKeyState(VK_RBUTTON) and Pressed <> 0;
  if FState.RightDown <> Change.RightDown then
  begin
    if Change.RightDown then
      Change.Kind := nkButtonDown
    else
      Change.Kind := nkButtonUp;
    Change.Button := mbMiddle;
    for Event in FMonitor.FEvents do
      Event(Change.Kind, Change.Button, Change.Position.X, Change.Position.Y);
  end;
  FState := Change;
end;

procedure TMouseThread.Execute;
begin
  while not Terminated do
  begin
    Synchronize(QueryMouse);
    Sleep(10);
  end;
end;

{ TMouseMonitor }

destructor TMouseMonitor.Destroy;
begin
  if FThread <> nil then
    FThread.Terminate;
  inherited Destroy;
end;

procedure TMouseMonitor.Add(Notify: TMouseNotifyEvent);
begin
  FEvents.Add(Notify);
  if FThread = nil then
    FThread := TMouseThread.Create(Self);
end;

procedure TMouseMonitor.Remove(Notify: TMouseNotifyEvent);
begin
  FEvents.Remove(Notify);
  if FEvents.IsEmpty then
  begin
    if FThread <> nil then
      FThread.Terminate;
    FThread := nil;
  end;
end;

initialization
  InternalMouseMonitor := nil;
finalization
  InternalMouseMonitor.Free;
end.

