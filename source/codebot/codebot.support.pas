(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified September 2023                             *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.support.txt> }
unit Codebot.Support;

{$i codebot.inc}

interface

uses
  Classes,
  Codebot.System;

{ IAsyncRunner\<T\> }

type
  IAsyncRunnerBase = interface
  ['{63E332A4-84A4-449F-B258-C2A8BB51403D}']
    { Tick ahead progress by a delta amount }
    procedure Tick(Delta: Int64);
  end;

  IAsyncRunner<T> = interface(IAsyncRunnerBase)
  ['{631018B8-D7D1-4C2A-928E-124500AFBA03}']
    { Nofity fires the update event if the status has changed }
    procedure Notify(Status: TAsyncStatus; Result: T);
  end;

{ TAsyncTaskRunner\<T\> }

  TAsyncTaskRunner<T> = class(TInterfacedObject, IAsyncTask, IAsyncRunnerBase,
    IAsyncRunner<T>)
  public
    type
      TNotifyComplete = procedure(Task: IAsyncTask; Result: T) of object;
  private
    FOnComplete: TNotifyComplete;
    FCancelled: Integer;
    FData: TObject;
    FOwnsObject: Boolean;
    FProgress: Int64;
    FStartTime: TDateTime;
    FStartQuery: Double;
    FDuration: Double;
    FStatus: TAsyncStatus;
    { IAsyncTask }
    function GetCancelled: Boolean;
    function GetData: TObject;
    function GetProgress: Int64;
    function GetStartTime: TDateTime;
    function GetDuration: Double;
    function GetStatus: TAsyncStatus;
    procedure Cancel;
    procedure Wait;
    { IAsyncRunnerBase }
    procedure Tick(Delta: Int64);
    { IAsyncRunner<T> }
    procedure Notify(Status: TAsyncStatus; Result: T);
  public
    constructor Create(OnComplete: TNotifyComplete; Data: TObject = nil; OwnsObject: Boolean = False); virtual;
    destructor Destroy; override;
  end;

{ TThreadRunner\<T\> }

  TThreadRunner<T> = class(TThread)
  public
    type
      TRunnerProc = procedure(var Params: T; Task: IAsyncTask);
  private
    FParams: T;
    FTask: IAsyncTask;
    FOnExecute: TRunnerProc;
    FOnComplete: TRunnerProc;
  protected
    procedure Complete;
    procedure Execute; override;
  public
    constructor Create(const Params: T; Task: IAsyncTask; OnExecute, OnComplete: TRunnerProc);
  end;

const
  BoolAsync: array[Boolean] of TAsyncStatus = (asyncFail, asyncSuccess);

implementation

{ TAsyncTaskRunner<T> }

constructor TAsyncTaskRunner<T>.Create(OnComplete: TNotifyComplete; Data: TObject = nil; OwnsObject: Boolean = False);
begin
  inherited Create;
  FOnComplete := OnComplete;
  FData := Data;
  FOwnsObject := OwnsObject;
  FStartTime := Now;
  FStartQuery := TimeQuery;
end;

destructor TAsyncTaskRunner<T>.Destroy;
begin
  if FOwnsObject then
    FData.Free;
  inherited Destroy;
end;

{ TAsyncTaskRunner<T>.IAsyncTask }

function TAsyncTaskRunner<T>.GetCancelled: Boolean;
begin
  Result := FCancelled > 0;
end;

function TAsyncTaskRunner<T>.GetData: TObject;
begin
  Result := FData;
end;

function TAsyncTaskRunner<T>.GetProgress: Int64;
begin
  Result := FProgress;
end;

function TAsyncTaskRunner<T>.GetStartTime: TDateTime;
begin
  Result := FStartTime;
end;

function TAsyncTaskRunner<T>.GetDuration: Double;
begin
  if FDuration = 0 then
    Result := TimeQuery -  FStartQuery
  else
    Result := FDuration;
end;

function TAsyncTaskRunner<T>.GetStatus: TAsyncStatus;
begin
  Result := FStatus;
end;

procedure TAsyncTaskRunner<T>.Cancel;
begin
  InterlockedIncrement(FCancelled);
end;

procedure TAsyncTaskRunner<T>.Wait;
begin
  while Assigned(PumpMessagesProc) do
  begin
    if GetStatus = asyncBusy then
      PumpMessages
    else
      Exit;
  end;
  raise EAsyncException.Create('PumpMessagesProc must be assigned in order to wait');
end;

{ IAsyncRunnerBase }

procedure TAsyncTaskRunner<T>.Tick(Delta: Int64);
begin
  FProgress := FProgress + Delta;
end;

{ TAsyncTaskRunner<T>.IAsyncTaskTask<T> }

procedure TAsyncTaskRunner<T>.Notify(Status: TAsyncStatus; Result: T);
var
  S: TAsyncStatus;
begin
  if FStatus <> asyncBusy then
    Exit;
  if FCancelled > 0 then
    S := asyncCanceled
  else
    S := Status;
  if S <> asyncBusy then
  begin
    FStatus := S;
    FDuration := TimeQuery - FStartQuery;
    if Assigned(FOnComplete) then
      FOnComplete(Self, Result);
  end;
end;

{ TThreadRunner<T> }

constructor TThreadRunner<T>.Create(const Params: T; Task: IAsyncTask; OnExecute, OnComplete: TRunnerProc);
begin
  FParams := Params;
  FTask := Task;
  FOnExecute := OnExecute;
  FOnComplete := OnComplete;
  inherited Create(False);
end;

procedure TThreadRunner<T>.Complete;
begin
  FOnComplete(FParams, FTask);
end;

procedure TThreadRunner<T>.Execute;
begin
  FreeOnTerminate := True;
  FOnExecute(FParams, FTask);
  Synchronize(Complete);
end;

end.

