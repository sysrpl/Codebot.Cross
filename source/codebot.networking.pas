(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified March 2015                                 *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.networking.txt> }
unit Codebot.Networking;

{$i codebot.inc}

interface

uses
  SysUtils,
  Codebot.System,
  Codebot.Interop.Sockets,
  Codebot.Interop.OpenSSL;

{ TAddressName resolves and converts a host name to an internet address
  See also
  <link Overview.Codebot.Networking.TAddressName, TAddressName members> }

type
  TAddressName = record
  private
    FAddress: LongWord;
    FHost: string;
    FLocation: string;
    FResolved: Boolean;
  public
    { Create an address name given a host to be resolved }
    class function Create(const Host: string): TAddressName; static; overload;
    { Create an address name given a 32 bit encoded address }
    class function Create(Address: LongWord): TAddressName; static; overload;
    { Create an address name given a 4 byte address }
    class function Create(A, B, C, D: Byte): TAddressName; static; overload;
    { Convert an address name to a string }
    class operator Implicit(const Value: TAddressName): string;
    { Convert a string to an address name }
    class operator Implicit(const Value: string): TAddressName;
    { Attempt to resolve a name into an address }
    function Resolve: Boolean;
    { The address resolved from the host }
    property Address: LongWord read FAddress;
    { The host name }
    property Host: string read FHost;
    { The address in string form }
    property Location: string read FLocation;
    { Resolved is true if a host name could be resolved }
    property Resolved: Boolean read FResolved;
  end;

{ TSocketState is used by the TSocket class
  See also
  <link Codebot.Networking.TSocket, TSocket class> }

  TSocketState = (
    { The socket is closed and not connected }
    ssClosed,
    { The socket is listening and acting as a server }
    ssServer,
    { The socket is a client and is connected to a server }
    ssClient,
    { The socket is an incomming connection accepted by a server socket }
    ssRemote);

{ TSocketKind is used by the TSocket class
  See also
  <link Codebot.Networking.TSocket, TSocket class> }

  TSocketKind = (
    { The socket uses transport control protocol }
    skTcp,
    { The socket uses user datagram protocol }
    skUdp);

{ TSocket provide a simple object oriented interface to network sockets
  See also
  <link Overview.Codebot.Networking.TSocket, TSocket members> }

  TSocket = class(TObject)
  private
    FAddress: TAddressName;
    FBlocking: Boolean;
    FPort: Word;
    FKind: TSocketKind;
    FHandle: TSocketHandle;
    FServer: TSocket;
    FState: TSocketState;
    FSecure: Boolean;
    FSSLContext: TSSLCtx;
    FSSLSocket: TSSL;
    FTimeout: LongWord;
    FTimer: Double;
    procedure SetBlocking(Value: Boolean);
    procedure TimerReset;
    function TimerExpired: Boolean;
    function DoRead(var Buffer; BufferSize: LongWord): Integer;
    function DoWrite(var Buffer; BufferSize: LongWord): Integer;
    function GetAddress: TAddressName;
    procedure SetSecure(Value: Boolean);
    function GetConnected: Boolean;
  public
    { Create a new socket }
    constructor Create; overload;
    { Create an incomming connection for a server socket }
    constructor Create(Server: TSocket); overload;
    destructor Destroy; override;
    { Close the socket }
    procedure Close;
    { Connect to an address converting the state to a client }
    function Connect(const Address: TAddressName; Port: Word): Boolean;
    { Listen on address and port converting the state to a server }
    function Listen(const Address: TAddressName; Port: Word): Boolean; overload;
    { Listen on port converting the state to a server }
    function Listen(Port: Word): Boolean; overload;
    { While listening wait to accept an incomming connection }
    function Accept(Socket: TSocket): Boolean;
    { Read from a client or remote socket to a buffer }
    function Read(var Buffer; BufferSize: LongWord): Integer; overload;
    { Read from a client or remote socket to text }
    function Read(out Text: string; BufferSize: LongWord = $10000): Integer; overload;
    { Write a buffer to a client or remote socket }
    function Write(var Buffer; BufferSize: LongWord): Integer; overload;
    { Write text to a client or remote socket }
    function Write(const Text: string): Integer; overload;
    { Write all bytes in a buffer to a client or remote socket }
    function WriteAll(var Buffer; BufferSize: LongWord): Boolean; overload;
    { Write all text to a client or remote socket }
    function WriteAll(const Text: string): Boolean; overload;
    { The address of socket }
    property Address: TAddressName read GetAddress;
    { When blocking is true, read an write operations wait }
    property Blocking: Boolean read FBlocking write SetBlocking;
    { The kind of the socket to create }
    property Kind: TSocketKind read FKind write FKind;
    { The port of the socket }
    property Port: Word read FPort;
    { The server socket from which a remote socket was accepted through }
    property Server: TSocket read FServer;
    { When secure is true socket communication will be routed through an SSL library }
    property Secure: Boolean read FSecure write SetSecure;
    { The underlying socket state }
    property State: TSocketState read FState;
    { Optional timeout period }
    property Timeout: LongWord read FTimeout write FTimeout;
    { Connected is true when a socket is valid and active }
    property Connected: Boolean read GetConnected;
  end;

{ TTransmitEvent can be reused to indicate progress of socket reading or writing.
  Size is the total expected number of bytes to be transmitted, and
  Transmitted is the actual number of bytes transmitted so far }

  TTransmitEvent = procedure(Sender: TObject; const Size, Transmitted: LargeWord) of object;

implementation

{ TAddressName }

class function TAddressName.Create(const Host: string): TAddressName;
begin
  Result.FAddress := 0;
  Result.FHost := Host;
  Result.FLocation := '';
  Result.FResolved := False;
end;

class function TAddressName.Create(Address: LongWord): TAddressName;
var
  Addr: TInAddr;
begin
  Addr.s_addr := Address;
  Result.FAddress := Addr.s_addr;
  Result.FHost := inet_ntoa(Addr);
  Result.FLocation := Result.FHost;
  Result.FResolved := True;
end;

class function TAddressName.Create(A, B, C, D: Byte): TAddressName;
var
  Addr: TInAddr;
begin
  SocketsInit;
  Addr.S_un_b.s_b1 := A;
  Addr.S_un_b.s_b2 := B;
  Addr.S_un_b.s_b3 := C;
  Addr.S_un_b.s_b4 := D;
  Result.FAddress := Addr.s_addr;
  Result.FHost := inet_ntoa(Addr);
  Result.FLocation := Result.FHost;
  Result.FResolved := True;
end;

class operator TAddressName.Implicit(const Value: TAddressName): string;
begin
  Result := Value.Host;
end;

class operator TAddressName.Implicit(const Value: string): TAddressName;
begin
  Result := TAddressName.Create(Value);
end;

function TAddressName.Resolve: Boolean;
var
  HostEnt: PHostEnt;
  Addr: PInAddr;
begin
  SocketsInit;
  if FResolved then
    Exit(True);
  if FHost = '' then
    Exit(False);
  HostEnt := gethostbyname(PAnsiChar(FHost));
  if HostEnt = nil then
    Exit(False);
  Addr := HostEnt.h_addr^;
  FAddress := Addr.S_addr;
  FLocation := inet_ntoa(Addr^);
  FResolved := True;
  Result := True;
end;

{ TSocket class }

constructor TSocket.Create;
const
  DefaultTimeout = 4000;
begin
  inherited Create;
  SocketsInit;
  FHandle := INVALID_SOCKET;
  FTimeout := DefaultTimeout;
end;

constructor TSocket.Create(Server: TSocket);
begin
  Create;
  FServer := Server;
end;

destructor TSocket.Destroy;
begin
  Close;
  inherited Destroy;
end;

procedure TSocket.Close;
var
  S: TSSL;
  C: TSSLCtx;
  H: TSocketHandle;
begin
  FState := ssClosed;
  if FHandle = INVALID_SOCKET then
    Exit;
  S := FSSLSocket;
  C := FSSLContext;
  H := FHandle;
  FSSLSocket := nil;
  FSSLContext := nil;
  FHandle := INVALID_SOCKET;
  if S <> nil then
  begin
    SSL_shutdown(S);
    SSL_free(S);
  end;
  if C <> nil then
    SSL_CTX_free(C);
  shutdown(H, SHUT_RDWR);
  Codebot.Interop.Sockets.close(H);
end;

procedure TSocket.TimerReset;
begin
  FTimer := 0;
end;

procedure TSocket.SetBlocking(Value: Boolean);
begin
  if FBlocking = Value then Exit;
  Close;
  FBlocking := Value;
end;

function TSocket.TimerExpired: Boolean;
begin
  if FTimeout = 0 then
    Result := True
  else if FTimer = 0 then
  begin
    FTimer := TimeQuery;
    Result := False;
  end
  else
    Result := TimeQuery - FTimer > FTimeout / 1000;
end;

function TSocket.Connect(const Address: TAddressName; Port: Word):  Boolean;
var
  Addr: TSockAddrIn;
  {$ifdef windows}
  Mode: LongWord;
  {$endif}
begin
  Close;
  if FSecure then
    if not OpenSSLInit then
      Exit(False);
  FAddress := Address;
  FPort := Port;
  if not FAddress.Resolve then
    Exit(False);
  if FPort = 0 then
    Exit(False);
  if FKind = skTcp then
    FHandle := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP)
  else
    FHandle := socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
  if FHandle = INVALID_SOCKET then
    Exit(False);
  Addr.sin_family := AF_INET;
  Addr.sin_addr.s_addr := FAddress.Address;
  Addr.sin_port := htons(FPort);
  if Codebot.Interop.Sockets.connect(FHandle, @Addr, SizeOf(Addr)) = SOCKET_ERROR then
  begin
    Close;
    Exit(False);
  end;
  FState := ssClient;
  if FSecure then
  begin
    FSSLContext := SSL_CTX_new(SSLv23_client_method);
    if FSSLContext = nil then
    begin
      Close;
      Exit(False);
    end;
    FSSLSocket := SSL_new(FSSLContext);
    if FSSLSocket = nil then
    begin
      Close;
      Exit(False);
    end;
    if SSL_set_fd(FSSLSocket, FHandle) <> 1 then
    begin
      Close;
      Exit(False);
    end;
    if SSL_connect(FSSLSocket) <> 1 then
    begin
      Close;
      Exit(False);
    end;
  end;
  if not FBlocking then
  begin
    {$ifdef windows}
    Mode := 1;
    ioctlsocket(FHandle, FIONBIO, Mode);
    {$else}
    fcntl(FHandle, F_SETFL, O_NONBLOCK);
    {$endif}
  end;
  Result := True;
end;

function TSocket.Listen(const Address: TAddressName; Port: Word): Boolean;
var
  Addr: TSockAddrIn;
begin
  Result := False;
  Close;
  FAddress := Address;
  FPort := Port;
  if FPort = 0 then
    Exit;
  if FSecure then
    Exit;
  if FKind = skTcp then
    FHandle := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP)
  else
    FHandle := socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
  if FHandle = INVALID_SOCKET then
    Exit;
  Addr.sin_family := AF_INET;
  if FAddress.Resolve then
    Addr.sin_addr.s_addr := FAddress.Address
  else
    Addr.sin_addr.s_addr := INADDR_ANY;
  Addr.sin_port := htons(FPort);
  if bind(FHandle, @Addr, SizeOf(Addr)) = SOCKET_ERROR then
  begin
    Close;
    Exit;
  end;
  if not FAddress.Resolved then
    FAddress := TAddressName.Create(Addr.sin_addr.s_addr);
  if Codebot.Interop.Sockets.listen(FHandle, SOMAXCONN) = SOCKET_ERROR then
  begin
    Close;
    Exit;
  end;
  FState := ssServer;
  Result := True;
end;

function TSocket.Listen(Port: Word): Boolean;
begin
  Result := Listen(TAddressName.Create(''), Port);
end;

function TSocket.Accept(Socket: TSocket): Boolean;
var
  Addr: TSockAddrIn;
  I: Integer;
  H: TSocketHandle;
  {$ifdef windows}
  Mode: LongWord;
  {$endif}
begin
  Result := False;
  if Socket = Self then
    Exit;
  Socket.Close;
  if FState <> ssServer then
    Exit;
  I := SizeOf(Addr);
  H := Codebot.Interop.Sockets.accept(FHandle, @Addr, I);
  if H = INVALID_SOCKET then
    Exit(False);
  Socket.FHandle := H;
  Socket.FAddress := TAddressName.Create(Addr.sin_addr.s_addr);
  Socket.FPort := ntohs(Addr.sin_port);
  Socket.FKind := FKind;
  Socket.FState := ssRemote;
  Socket.FServer := Self;
  Socket.FBlocking := FBlocking;
  if not FBlocking then
  begin
    {$ifdef windows}
    Mode := 1;
    ioctlsocket(Socket.FHandle, FIONBIO, Mode);
    {$else}
    fcntl(Socket.FHandle, F_SETFL, O_NONBLOCK);
    {$endif}
  end;
  Result := True;
end;

function TSocket.DoRead(var Buffer; BufferSize: LongWord): Integer;
var
  Bytes: LongInt;
begin
  if FState < ssClient then
    Exit(SOCKET_ERROR);
  if BufferSize < 1 then
    Exit(0);
  if FSecure then
    Bytes := SSL_read(FSSLSocket, @Buffer, BufferSize)
  else
    Bytes := recv(FHandle, Buffer, BufferSize, 0);
  if Bytes = 0 then
  begin
    Close;
    Exit(Bytes);
  end;
  Result := Bytes;
end;

function TSocket.Read(var Buffer; BufferSize: LongWord): Integer;
begin
  TimerReset;
  repeat
    Result := DoRead(Buffer, BufferSize);
    if (Result > -1) or FBlocking then
      Break;
    Sleep(1);
  until TimerExpired;
  if Result < 0 then
    Result := 0;
end;

function TSocket.Read(out Text: string; BufferSize: LongWord = $10000): Integer;
begin
  SetLength(Text, BufferSize);
  Result := Read(Pointer(Text)^, BufferSize);
  if Result < 1 then
    SetLength(Text, 0)
  else
    SetLength(Text, Result);
end;

function TSocket.DoWrite(var Buffer; BufferSize: LongWord): Integer;
var
  Bytes: LongInt;
begin
  if FState < ssClient then
    Exit(SOCKET_ERROR);
  if BufferSize < 1 then
    Exit(0);
  if FSecure then
    Bytes := SSL_write(FSSLSocket, @Buffer, BufferSize)
  else
    Bytes := send(FHandle, Buffer, BufferSize, MSG_NOSIGNAL);
  if Bytes < 1 then
  begin
    Close;
    Exit(SOCKET_ERROR);
  end;
  Result := Bytes;
end;

function TSocket.Write(var Buffer; BufferSize: LongWord): Integer;
begin
  TimerReset;
  repeat
    Result := DoWrite(Buffer, BufferSize);
    if (Result > -1) or FBlocking then
      Break;
    Sleep(1);
  until TimerExpired;
  if Result < 0 then
    Result := 0;
end;

function TSocket.Write(const Text: string): Integer;
begin
  Result := Write(Pointer(Text)^, Length(Text));
end;

function TSocket.WriteAll(var Buffer; BufferSize: LongWord): Boolean;
var
  B: PByte;
  I: Integer;
begin
  if BufferSize < 1 then
    Exit(Connected);
  B := @Buffer;
  while BufferSize > 0 do
  begin
    I := Write(B^, BufferSize);
    if I < 1 then
      Exit(False);
    BufferSize := BufferSize - I;
    Inc(B, I);
  end;
  Result := True;
end;

function TSocket.WriteAll(const Text: string): Boolean;
begin
  Result := WriteAll(Pointer(Text)^, Length(Text));
end;

function TSocket.GetAddress: TAddressName;
begin
  Result := FAddress;
end;

procedure TSocket.SetSecure(Value: Boolean);
begin
  if Value <> FSecure then
  begin
    Close;
    FSecure := Value;
  end;
end;

function TSocket.GetConnected: Boolean;
begin
  Result := FHandle <> INVALID_SOCKET;
end;

end.
