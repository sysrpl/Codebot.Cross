(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified March 2015                                 *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.debug.txt> }
unit Codebot.Debug;

{$i codebot.inc}

interface

uses
  Classes, SysUtils,
  Codebot.System,
  Codebot.Networking;

type
  TShutdownThread = class(TThread)
  public
    procedure Shutdown; virtual; abstract;
  end;

  TDebugEvent = procedure(const Text: string) of object;
  TDebugDelegate = TDelegate<TDebugEvent>;
  IDebugDelegate = IDelegate<TDebugEvent>;

{ TDebugServer }

  TDebugServer = class
  private
    FOnDebug: TDebugDelegate;
    FThread: TShutdownThread;
    function GetOnDebug: IDebugDelegate;
    function GetRunning: Boolean;
    procedure SetRunning(Value: Boolean);
  public
    destructor Destroy; override;
    property Running: Boolean read GetRunning write SetRunning;
    property OnDebug: IDebugDelegate read GetOnDebug;
  end;

procedure Bugout(const S: string); overload;
procedure Bugout(const S: string; Args: array of const); overload;
procedure Bugout(I: Integer); overload;
procedure Bugout(F: Float); overload;

implementation

const
  DebugPort = 6924;

{ TDebugServerThread }

type
  TDebugServerThread = class(TShutdownThread)
  private
    FServer: TDebugServer;
    FServerSocket: TSocket;
    FClientSocket: TSocket;
    FText: string;
  protected
    procedure DoDebug;
    procedure Execute; override;
  public
    constructor Create(Server: TDebugServer);
    procedure Shutdown; override;
  end;

constructor TDebugServerThread.Create(Server: TDebugServer);
begin
  FServer := Server;
  FServerSocket := TSocket.Create;
  FServerSocket.Blocking := True;
  FClientSocket := TSocket.Create;
  inherited Create(False);
end;

procedure TDebugServerThread.Shutdown;
begin
  Terminate;
  FServerSocket.Close;
  FClientSocket.Close;
  FServerSocket.Free;
  FClientSocket.Free;
  Free;
end;

procedure TDebugServerThread.DoDebug;
var
  Event: TDebugEvent;
begin
  if Terminated then
    Exit;
  for Event in FServer.FOnDebug do
    Event(FText);
end;

procedure TDebugServerThread.Execute;
begin
  if not FServerSocket.Listen(DebugPort) then
    Exit;
  while not Terminated do
    if FServerSocket.Accept(FClientSocket) then
    begin
      if Terminated then
        Exit;
      while FClientSocket.Read(FText) > 0 do
      begin
        if Terminated then
          Exit;
        Synchronize(DoDebug);
      end;
    end;
end;

{ TDebugServer }

destructor TDebugServer.Destroy;
begin
  Running := False;
  inherited Destroy;
end;

function TDebugServer.GetRunning: Boolean;
begin
  Result := FThread <> nil;
end;

procedure TDebugServer.SetRunning(Value: Boolean);
begin
  if Value = Running then Exit;
  if Value then
    FThread := TDebugServerThread.Create(Self)
  else
  begin
    FThread.Shutdown;
    FThread := nil;
  end;
end;

function TDebugServer.GetOnDebug: IDebugDelegate;
begin
  Result := FOnDebug;
end;

var
  BugoutClient: TSocket;

procedure Bugout(const S: string);
begin
  if S.Length < 1 then
    Exit;
  if BugoutClient = nil then
    BugoutClient := TSocket.Create;
  if not BugoutClient.Connected then
    BugoutClient.Connect('127.0.0.1', DebugPort);
  if BugoutClient.Connected then
    if BugoutClient.Write(S + #13#10) < 1 then
      BugoutClient.Close;
end;

procedure Bugout(const S: string; Args: array of const);
begin
  if S.Length < 1 then
    Exit;
  Bugout(Format(S, Args));
end;

procedure Bugout(I: Integer);
begin
  Bugout(IntToStr(I));
end;

procedure Bugout(F: Float);
begin
  Bugout(FloatToStr(F));
end;

initialization
  BugoutClient := nil;
finalization
  BugoutClient.Free;
end.

