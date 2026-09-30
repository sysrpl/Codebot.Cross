(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified November 2025                              *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.process.txt> }
unit Codebot.Process;

{$i codebot.inc}

interface

uses
  SysUtils,
  Classes,
  Codebot.System;

{ TExternalCommand provides a simplified way to run external programs
where you want to record their output and detect when they have completed }

type
  TLineReadEvent = procedure(Sender: TObject; const Line: string) of object;

  EExternalCommandException = class(Exception);

  TExternalCommand = class(TComponent)
	private
    FCommand: string;
    FArguments: TStrings;
    FBufferOutput: Boolean;
    FOutput: TStrings;
    FLine: string;
    FRunning: Boolean;
    FThread: TSimpleThread;
    FOnLineRead: TLineReadEvent;
    FOnComplete: TNotifyEvent;
    procedure SyncLineRead;
    procedure SyncComplete;
    procedure ThreadRun(Thread: TSimpleThread);
    procedure SetArguments(Value: TStrings);
	public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Run begins execution of the external program }
    procedure Run;
    { Kill aborts execution of the external program }
    procedure Kill;
    { Output contains all liens read from the program }
    property Output: TStrings read FOutput;
    { Running is true while the external program is executing }
    property Running: Boolean read FRunning;
	published
    { The file name of the external program to execute }
    property Command: string read FCommand write FCommand;
    { Any command line arguments provided to the above program }
    property Arguments: TStrings read FArguments write SetArguments;
    { When BufferOutput is true all lines read are stored in the output property }
    property BufferOutput: Boolean read FBufferOutput write FBufferOutput default True;
    { OnLineRead is fired each time a complete line is read from the running program }
    property OnLineRead: TLineReadEvent read FOnLineRead write FOnLineRead;
    { OnComplete is fire when the program completes }
    property OnComplete: TNotifyEvent read FOnComplete write FOnComplete;
  end;

procedure RunCommand(const Command: string; Output: TStrings = nil); overload;
procedure RunCommand(const Command: string; const Arg0: string; Output: TStrings = nil); overload;
procedure RunCommand(const Command: string; const Arg0, Arg1: string; Output: TStrings = nil); overload;
procedure RunCommand(const Command: string; const Arg0, Arg1, Arg2: string; Output: TStrings = nil); overload;

implementation

uses
	Process;

{ TExternalCommand }

constructor TExternalCommand.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
  FBufferOutput := True;
  FArguments := TStringList.Create;
  FOutput := TStringList.Create;
end;

destructor TExternalCommand.Destroy;
begin
  Kill;
  while FRunning do
  begin
  	Sleep(10);
    if FRunning then
	    PumpMessages;
  end;
  FOutput.Free;
	inherited Destroy;
end;

procedure TExternalCommand.ThreadRun(Thread: TSimpleThread);
var
  P: TProcess;
  C: Char;
begin
  P := TProcess.Create(nil);
  try
    P.Executable := FCommand;
    if FArguments.Count > 0 then
	    P.Parameters.Assign(FArguments);
    P.Options := [poUsePipes, poStderrToOutPut];
    P.Execute;
    while (not FThread.Terminated) and P.Running do
      while (not FThread.Terminated) and (P.Output.NumBytesAvailable > 0) do
      begin
        FLine := '';
        repeat
          C := Char(P.Output.ReadByte);
          if C = #13 then
          	Continue;
          if C = #10 then
        	  Break;
          FLine := FLine + C;
        until P.Output.NumBytesAvailable = 0;
        if not FThread.Terminated then
				  Thread.Synchronize(SyncLineRead);
      end;
    if FThread.Terminated then
	    P.Terminate(0);
    if not FThread.Terminated then
	    SyncComplete;
    FRunning := False;
  finally
    P.Free;
  end;
end;

procedure TExternalCommand.Run;
begin
	if FRunning then
  	raise EExternalCommandException.Create('External command is already running');
  if FCommand = '' then
		raise EExternalCommandException.Create('External command is not set');
  FRunning := True;
  FOutput.Clear;
  FThread := ThreadExecute(ThreadRun);
end;

procedure TExternalCommand.Kill;
begin
  if FRunning then
  	FThread.Terminate;
end;

procedure TExternalCommand.SyncLineRead;
begin
  if FBufferOutput then
  	FOutput.Add(FLine);
  if Assigned(FOnLineRead) then
  	FOnLineRead(Self, FLine);
end;

procedure TExternalCommand.SyncComplete;
begin
  FRunning := False;
  if Assigned(FOnComplete) then
  	FOnComplete(Self);
end;

procedure TExternalCommand.SetArguments(Value: TStrings);
begin
	FOutput.Assign(Value);
end;

{ RunCommand }

procedure RunCommand(const Command: string; Output: TStrings = nil);
begin
	RunCommand(Command, '', '', '', Output);
end;

procedure RunCommand(const Command: string; const Arg0: string; Output: TStrings = nil);
begin
	RunCommand(Command, Arg0, '', '', Output);
end;

procedure RunCommand(const Command: string; const Arg0, Arg1: string; Output: TStrings = nil);
begin
	RunCommand(Command, Arg0, Arg1, '', Output);
end;

procedure RunCommand(const Command: string; const Arg0, Arg1, Arg2: string; Output: TStrings = nil);
var
  P: TProcess;
  C: Char;
  S: string;
begin
  if Output <> nil then
    Output.Clear;
  P := TProcess.Create(nil);
  try
    P.Executable := Command;
    if Arg0 <> '' then
	    P.Parameters.Add(Arg0);
    if Arg1 <> '' then
	    P.Parameters.Add(Arg1);
    if Arg2 <> '' then
	    P.Parameters.Add(Arg2);
    P.Options := [poUsePipes, poStderrToOutPut];
    P.Execute;
    while P.Running do
      while P.Output.NumBytesAvailable > 0 do
      begin
        S := '';
        repeat
          C := Char(P.Output.ReadByte);
          if C = #13 then
          	Continue;
          if C = #10 then
        	  Break;
          S := S + C;
        until P.Output.NumBytesAvailable = 0;
        if Output <> nil then
          Output.Add(S);
      end;
  finally
    P.Free;
  end;
end;

end.
