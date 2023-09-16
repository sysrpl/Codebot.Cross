(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified March 2019                                 *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.networking.unix.txt> }
unit Codebot.Networking.Unix;

{$i codebot.inc}

interface

uses
  Codebot.System,
  Codebot.Interop.Sockets;

{ TUnixClientSocket }

type
  TUnixClientSocket = class(TObject)
  private
    FHandle: TSocketHandle;
    FFileName: string;
    function GetConnected: Boolean;
    procedure SetConnected(Value: Boolean);
    procedure SetFileName(Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Connect;
    procedure Disconnect;
    function Read(var Buffer; BufferSize: LongWord): Integer; overload;
    function Read(out Text: string; BufferSize: LongWord = $10000): Integer; overload;
    function Write(const S: string): Integer;
    property FileName: string read FFileName write SetFileName;
    property Connected: Boolean read GetConnected write SetConnected;
  end;

implementation

{ TUnixClientSocket }

constructor TUnixClientSocket.Create;
begin
  inherited Create;
  FHandle := INVALID_SOCKET;
end;

destructor TUnixClientSocket.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

procedure TUnixClientSocket.Connect;
begin
  if FHandle <> INVALID_SOCKET then
      Exit;
end;

procedure TUnixClientSocket.Disconnect;
var
  S: TSocketHandle;
begin
  if FHandle = INVALID_SOCKET then
      Exit;
  S := FHandle;
  FHandle := INVALID_SOCKET;
  close(S);
end;

function TUnixClientSocket.Read(var Buffer; BufferSize: LongWord): Integer;
begin
  if FHandle = INVALID_SOCKET then
    Exit(SOCKET_ERROR);
  Result := recv(FHandle, Buffer, BufferSize, 0);
  if Result = SOCKET_ERROR then
    Disconnect;
end;

function TUnixClientSocket.Read(out Text: string; BufferSize: LongWord = $10000): Integer;
begin
  Result := 0;
  Text := '';
  if BufferSize < 1 then
    Exit;
  SetLength(Text, BufferSize);
  Result := Read(Pointer(Text)^, BufferSize);
  if Result < 1 then
    SetLength(Text, 0)
  else
    SetLength(Text, Result);
end;

function TUnixClientSocket.Write(const S: string): Integer;
begin
  Result := 0;
end;

function TUnixClientSocket.GetConnected: Boolean;
begin
  Result := FHandle <> INVALID_SOCKET;
end;

procedure TUnixClientSocket.SetConnected(Value: Boolean);
begin
  if Value then
    Connect
  else
    Disconnect;
end;

procedure TUnixClientSocket.SetFileName(Value: string);
begin
  if FFileName = Value then Exit;
  FFileName := Value;
end;


end.
