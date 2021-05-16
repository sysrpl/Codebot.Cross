(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2015                               *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.unique.txt> }
unit Codebot.Unique;

{$i codebot.inc}

interface

uses
  SysUtils, Classes,
  Codebot.System,
  Codebot.Networking;

{ TMessageEvent }

type
  TMessageEvent = procedure(const Message: string) of object;

{ TUniqueInstance }

  TUniqueInstance = class
  private
    FPort: Word;
    FOriginal: Boolean;
    FSocket: TSocket;
    FThread: TSimpleThread;
    FMessage: string;
    FOnMessage: TMessageEvent;
    procedure ReceiveMessage;
    procedure Execute(Thread: TSimpleThread);
  public
    constructor Create(Key: Word);
    destructor Destroy; override;
    procedure SendMessage(const Message: string);
    property Original: Boolean read FOriginal;
    property OnMessage: TMessageEvent read FOnMessage write FOnMessage;
  end;

{ UniqueInstance should be called with a valid key at program startup }
function UniqueInstance(Key: Word = 0): TUniqueInstance; overload;
{ UniqueInstance overloaded to generate a Key by hashing a string }
function UniqueInstance(const Key: string): TUniqueInstance; overload;

implementation

function HashOf(const S: string): LongWord;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(S) do
    Result := ((Result shl 2) or (Result shr (SizeOf(Result) * 8 - 2))) xor Ord(S[I]);
end;

{ TUniqueInstance }

constructor TUniqueInstance.Create(Key: Word);
begin
  inherited Create;
  FPort := Key;
  FSocket := TSocket.Create;
  FSocket.Blocking := True;
  FOriginal := FSocket.Listen(FPort);
  if FOriginal then
    FThread := TSimpleThread.Create(Execute);
end;

destructor TUniqueInstance.Destroy;
begin
  if FThread <> nil then
    FThread.Terminate;
  FSocket.Free;
  inherited Destroy;
end;

procedure TUniqueInstance.ReceiveMessage;
begin
  if Assigned(FOnMessage) then
    FOnMessage(FMessage);
end;

procedure TUniqueInstance.Execute(Thread: TSimpleThread);
var
  Client: TSocket;
  S: string;
begin
  Client := TSocket.Create;
  try
    Client.Blocking := True;
    while not Thread.Terminated do
    begin
      if FSocket.Accept(Client) and (Client.Read(S) > 0) and (not Thread.Terminated) then
      begin
        FMessage := S;
        Thread.Synchronize(ReceiveMessage);
      end;
      Client.Close;
    end;
  finally
    Client.Free;
  end;
end;

procedure TUniqueInstance.SendMessage(const Message: string);
var
  S: TSocket;
begin
  S := TSocket.Create;
  try
    S.Blocking := True;
    if S.Connect('localhost', FPort) then
      S.Write(Message);
  finally
    S.Free;
  end;
end;

var
  InternalUniqueInstance: TObject;

function UniqueInstance(Key: Word = 0): TUniqueInstance;
begin
  if InternalUniqueInstance = nil then
    InternalUniqueInstance := TUniqueInstance.Create(Key);
  Result := TUniqueInstance(InternalUniqueInstance);
end;

function UniqueInstance(const Key: string): TUniqueInstance;
var
  L: LongWord;
begin
  L := HashOf(Key);
  L := L mod (High(Word) div 2 + High(Word) div 4) + High(Word) div 4;
  Result := UniqueInstance(L);
end;

initialization
  InternalUniqueInstance := nil;
finalization
  InternalUniqueInstance.Free;
end.

