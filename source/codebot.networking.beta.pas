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
  SysUtils, Classes,
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
    { The socket is listening and acting as a local server }
    ssServer,
    { The socket is a client connection connected to a remote server }
    ssClient,
    { The socket is an remote connection accepted by a local server }
    ssRemote);


{ TTransmitProgress is used to tally bytes read or written
  See also
  <link Codebot.Networking.TTransmitProgress, TTransmitProgress members> }

  TTransmitData = record
    Expected: Longint;
    Actual: Longint;
  end;


  TTransmitCallback = procedure(Bytes: LargeInt; var Cancel: Boolean) of object;

{ TTransmitEvent can be reused to indicate progress of reading or writing }

  TTransmitEvent = procedure(Sender: TObject; const Expected, Actual: Longint;
    var Cancel: Boolean) of object;

{ Note, SSL certificate files can be generated using this terminal command:

  openssl req -x509 -newkey rsa:2048 -keyout key.pem -out cert.pem \
    -days 365 -nodes -subj '/CN=localhost' }

{ TSocket provides a simple object oriented interface to network sockets
  See also
  <link Overview.Codebot.Networking.TSocket, TSocket members> }

  TSocket = class(TObject)
  private
    FAddress: TAddressName;
    FBlocking: Boolean;
    FPort: Word;
    FHandle: TSocketHandle;
    FServer: TSocket;
    FState: TSocketState;
    FSecure: Boolean;
    FSSLCertificates: TSSLCtx;
    FSSLContext: TSSLCtx;
    FSSLSocket: TSSL;
    FTimeout: LongWord;
    FTimer: Double;
    FData: Pointer;
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
    { If you plan to create a secure socket server you need to load both a
      certificate file and key file before listening }
    function LoadCertificate(CertFile: string; KeyFile: string): Boolean;
    { release any resources related to server certificates }
    procedure UnloadCertificate;
    { Close the socket }
    procedure Close;
    { Connect to an address converting the state to a client }
    function Connect(const Address: TAddressName; Port: Word): Boolean;
    { Listen on address and port converting the state to a server }
    function Listen(const Address: TAddressName; Port: Word): Boolean; overload;
    { Listen on a port converting the socket to a local server }
    function Listen(Port: Word): Boolean; overload;
    { While listening you may accept an incomming connection }
    function Accept(Socket: TSocket): Boolean;
    { Read incoming data from a client or remote socket to a buffer }
    function Read(var Buffer; BufferSize: LongWord): Integer; overload;
    { Read incoming data from a client or remote socket to text }
    function Read(out Text: string; BufferSize: LongWord = $10000): Integer; overload;
    { Write outgoing data from a buffer to a client or remote socket }
    function Write(var Buffer; BufferSize: LongWord): Integer; overload;
    { Write outgoing data from text to a client or remote socket }
    function Write(const Text: string): Integer; overload;
    { Write all bytes in a buffer to a client or remote socket }
    function WriteAll(var Buffer; BufferSize: LongWord): Boolean; overload;
    { Write all text to a client or remote socket }
    function WriteAll(const Text: string): Boolean; overload;
    { The address of socket }
    property Address: TAddressName read GetAddress;
    { When blocking is true, read an write operations will wait for completion }
    property Blocking: Boolean read FBlocking write SetBlocking;
    { The port of the socket }
    property Port: Word read FPort;
    { The server socket from which a remote socket was accepted }
    property Server: TSocket read FServer;
    { When secure is true socket communication will be routed through an SSL library }
    property Secure: Boolean read FSecure write SetSecure;
    { The underlying socket state }
    property State: TSocketState read FState;
    { Optional timeout period }
    property Timeout: LongWord read FTimeout write FTimeout;
    { Connected is true when a socket is valid and active }
    property Connected: Boolean read GetConnected;
    { Data provides a extra bits of user definable information }
    property Data: Pointer read FData write FData;
  end;

{ THttpRequest can be used to send or receive http requests
  See also
  <link Overview.Codebot.Networking.THttpRequest, THttpRequest members> }

  THttpRequest = class
  private
    FPartialBody: string;
    FDone: Boolean;
    FVerb: string;
    FResource: string;
    FProtocol: string;
    FHeaders: INamedStrings;
    FBodyStream: TStream;
    FBodyText: string;
    FValid: Boolean;
    FCancelled: Boolean;
    FOnProgress: TTransmitEvent;
    function ReadHeader(Socket: TSocket): Boolean;
    function ReadBody(Socket: TSocket): Boolean;
    function SendHeader(Socket: TSocket): Boolean;
    function SendBody(Socket: TSocket): Boolean;
  public
    constructor Create;
    { Reset all read only properties to their defaults }
    procedure Reset;
    { Cancels the request, which you normally invoke during OnProgress }
    procedure Cancel;
    { Attempt to receive a request. Use this when you're the server. }
    function Receive(Socket: TSocket): Boolean;
    { Attempt to send a request. Use this when you're the client. }
    function Send(Socket: TSocket): Boolean;
    { Verb contains the method used by the client such as GET or POST }
    property Verb: string read FVerb;
    { Resource contains the resources and quest string,such as /index.htm }
    property Resource: string read FResource write FResource;
    { Protocol such as HTTP/1.1 }
    property Protocol: string read FProtocol write FProtocol;
    { Headers are the string value pairs }
    property Headers: INamedStrings read FHeaders;
    { When BodyStream is assigned it is used by send or receive }
    property BodyStream: TStream read FBodyStream write FBodyStream;
    { When BodyStream is unassigned BodyText is used by send or receive }
    property BodyText: string read FBodyText write FBodyText;
    { Valid holds the scuess or failure of the last send or receive }
    property Valid: Boolean read FValid;
    { OnProgress is invoked when body is being sent or received }
    property OnProgress: TTransmitEvent read FOnProgress write FOnProgress;
  end;


procedure SocketWrite(Socket: TSocket; Stream: TStream);


{ Attempt to locate the domain record }

function Whois(const Domain: string; out Response: string): Boolean;

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
  UnloadCertificate;
  inherited Destroy;
end;

function TSocket.LoadCertificate(CertFile: string; KeyFile: string): Boolean;
begin
  Close;
  FSecure := True;
  if OpenSSLInit then
  begin
    FSSLCertificates := SSL_CTX_new(SSLv23_server_method);
    if FSSLCertificates = nil then
      Exit(False);
    Result :=
      (SSL_CTX_use_certificate_file(FSSLCertificates, PChar(CertFile), SSL_FILETYPE_PEM) > 0) and
      (SSL_CTX_use_PrivateKey_file(FSSLCertificates, PChar(KeyFile), SSL_FILETYPE_PEM) > 0) and
      (SSL_CTX_check_private_key(FSSLCertificates) > 0);
    if not Result then
    begin
      SSL_CTX_free(FSSLCertificates);
      FSSLCertificates := nil;
    end;
  end
  else
    Result := False;
end;

procedure TSocket.UnloadCertificate;
begin
  if FSSLCertificates <> nil then
  begin
    Close;
    SSL_CTX_free(FSSLCertificates);
    FSSLCertificates := nil;
  end;
  FSecure := False;
end;

procedure TSocket.Close;
var
  H: TSocketHandle;
  S: TSSL;
  C: TSSLCtx;
begin
  FState := ssClosed;
  H := FHandle;
  S := FSSLSocket;
  C := FSSLContext;
  FHandle := INVALID_SOCKET;
  FSSLSocket := nil;
  FSSLContext := nil;
  if H <> INVALID_SOCKET then
  begin
    Codebot.Interop.Sockets.shutdown(H, SHUT_RDWR);
    Codebot.Interop.Sockets.close(H);
  end;
  if S <> nil then
  begin
    SSL_shutdown(S);
    SSL_free(S);
  end;
  if C <> nil then
    SSL_CTX_free(C);
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
  FHandle := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
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
    FSSLContext := SSL_CTX_new(SSLv3_client_method);
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
    if SSL_set_fd(FSSLSocket, FHandle) < 1 then
    begin
      Close;
      Exit(False);
    end;
    if SSL_connect(FSSLSocket) < 1 then
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
  if FSecure and (FSSLCertificates = nil) then
    Exit;
  FAddress := Address;
  FPort := Port;
  if FPort = 0 then
    Exit;
  FHandle := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
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
  Error: LongInt;
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
    Exit;
  Socket.FHandle := H;
  Socket.FAddress := TAddressName.Create(Addr.sin_addr.s_addr);
  Socket.FPort := ntohs(Addr.sin_port);
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
  Socket.FSecure := FSecure;
  if FSecure then
  begin
    Socket.FSSLSocket := SSL_new(FSSLCertificates);
    if Socket.FSSLSocket = nil then
    begin
      Socket.Close;
      Exit;
    end;
    if SSL_set_fd(Socket.FSSLSocket, Socket.FHandle) < 1 then
    begin
      Socket.Close;
      Exit;
    end;
    if FBlocking then
    begin
      if SSL_accept(Socket.FSSLSocket) < 1 then
      begin
        Socket.Close;
        Exit;
      end;
    end
    else
    repeat
      Error := SSL_accept(Socket.FSSLSocket);
      if Error > 0 then
        Break;
      if Error = 0 then
      begin
        Socket.Close;
        Exit;
      end;
      Error := SSL_get_error(Socket.FSSLSocket, Error);
      if (Error = SSL_ERROR_WANT_READ) or (Error = SSL_ERROR_WANT_WRITE) then
        Continue;
      Socket.Close;
      Exit;
    until False;
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
  Result := False;
  if FState < ssClient then
    Exit;
  if BufferSize < 1 then
    Exit;
  B := @Buffer;
  while BufferSize > 0 do
  begin
    I := Write(B^, BufferSize);
    if not Connected then
      Exit;
    if I < 1 then
      Continue;
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

{ THttpRequest }

constructor THttpRequest.Create;
begin
  inherited  Create;
  FHeaders := TNamedStringsIntf.Create;
end;

procedure THttpRequest.Reset;
begin
  FCancelled := False;
  FVerb := '';
  FResource := '';
  FProtocol := '';
  FPartialBody := '';
  FBodyText := '';
  FHeaders.Clear;
  FDone := False;
  FValid := False;
end;

procedure THttpRequest.Cancel;
begin
  FCancelled := True;
end;

function THttpRequest.ReadHeader(Socket: TSocket): Boolean;
const
  Endings: array[0..3] of string = (#13#10#13#10, #10#13#10#13, #10#10, #13#10);
var
  HeaderComplete: Boolean;
  Header, Ending, S: string;
  EndIndex, I, J: Integer;
  Lines, First: StringArray;
begin
  Result := False;
  Reset;
  HeaderComplete := False;
  Header := '';
  J := 0;
  repeat
    if not Socket.Connected then
      Break;
    I := Socket.Read(S);
    { We wait on read to give the user a chance to accept an ssl certificate }
    if I = 0 then
    begin
      Inc(J);
      Sleep(500);
      if J = 60 then
        Exit
      else
        Continue;
    end;
    if I > 0 then
    begin
      Header := Header + S;
      EndIndex := -1;
      for I := Low(Endings) to High(Endings) do
      begin
        EndIndex := Header.IndexOf(Endings[I]);
        if EndIndex > 0 then
        begin
          EndIndex := I;
          Break;
        end;
      end;
      if EndIndex > -1 then
      begin
        HeaderComplete := True;
        Ending := Endings[EndIndex];
        FPartialBody := Header.SecondOf(Ending);
        S := Header.FirstOf(Ending);
        Ending.Length := Ending.Length div 2;
        Lines := S.Split(Ending);
        if Lines.Length > 0 then
        begin
          S := Lines[0];
          First := S.Split(' ');
          if First.Length > 2 then
          begin
            FVerb := First[0];
            FResource := First[1];
            FProtocol := First[2];
            for I := 1 to Lines.Length - 1 do
            begin
              S := Lines[I];
              FHeaders.Add(S.FirstOf(':').Trim, S.SecondOf(':').Trim);
            end;
            FValid := True;
          end;
        end;
      end;
    end
    else
      HeaderComplete := True;
  until HeaderComplete;
  Result := FValid;
end;

function THttpRequest.ReadBody(Socket: TSocket): Boolean;
const
  BufferSize = $10000;
var
  OwnStream: Boolean;
  Stream: TStream;
  Buffer: Pointer;
  UndefinedLength: Boolean;
  ContentLength, ContentRead: LargeInt;
  I: LargeInt;
begin
  Result := False;
  if (not FValid) or FDone then
    Exit;
  FDone := True;
  if FVerb = 'GET' then
    Exit(True);
  OwnStream := BodyStream = nil;
  if OwnStream then
    Stream := TStringStream.Create(FPartialBody)
  else
  begin
    Stream := BodyStream;
    if FPartialBody.Length > 0 then
      Stream.Write(PChar(FPartialBody)^, FPartialBody.Length);
  end;
  Buffer := GetMem(BufferSize);
  FPartialBody := '';
  FBodyText := '';
  try
    ContentRead := 0;
    ContentLength := 0;
    I := StrToInt64Def(FHeaders.GetValue('Content-Length'), -1);
    UndefinedLength := I < 0;
    if UndefinedLength then
      ContentLength := High(ContentLength)
    else
      ContentLength := I;
    if (I <> 0) and Assigned(FOnProgress) then
      if UndefinedLength then
        FOnProgress(Self, 0, 0)
      else
        FOnProgress(Self, ContentLength, 0);
    if FCancelled then
      Exit;
    if I <> 0 then
    repeat
      if not Socket.Connected then
        Break;
      if UndefinedLength then
        I := Socket.Read(Buffer^, BufferSize)
      else if ContentLength - ContentRead >  BufferSize then
        I := Socket.Read(Buffer^, BufferSize)
      else
        I := Socket.Read(Buffer^, ContentLength - ContentRead);
      if I < 0 then
        Break;
      if I > 0 then
      begin
        Stream.Write(Buffer^, I);
        ContentRead := ContentRead + I;
        if Assigned(FOnProgress) then
          if UndefinedLength then
            FOnProgress(Self, 0, ContentRead)
          else
            FOnProgress(Self, ContentLength, ContentRead);
        if FCancelled then
          Exit;
      end;
    until ContentRead = ContentLength;
    Result := UndefinedLength or (ContentRead = ContentLength);
    if OwnStream then
      FBodyText := (Stream as TStringStream).DataString;
  finally
    FreeMem(Buffer);
    if OwnStream then
      Stream.Free;
  end;
end;

function THttpRequest.Receive(Socket: TSocket): Boolean;
begin
  Result := ReadHeader(Socket) and ReadBody(Socket);
end;

function THttpRequest.SendHeader(Socket: TSocket): Boolean;
const
  Ending = #13#10;
var
  Request, S: string;
  I: Integer;
begin
  FCancelled := False;
  Result := False;
  try
    if not Socket.Connected then
      Exit;
    if FVerb = '' then
      FVerb := 'GET';
    Request := FVerb;
    if FResource = '' then
      FResource := '/';
    Request :=  Request + ' ' + FResource;
    if FProtocol = '' then
      FProtocol := 'HTTP/1.1'
    Request :=  Request + ' ' + FProtocol + Ending;
    for I := 0 to FHeaders.Count - 1 do
    begin
      S := FHeaders.Names[I];
      Request := Request + S + ': ' FHeaders.ValueByIndex[I] + Ending;
    end;
    Request := Request + Ending;
    Result := Socket.WriteAll(Request);
  finally
    FValid := Result;
  end;
end;

function THttpRequest.SendBody(Socket: TSocket): Boolean;
const
  BufferSize = $10000;
var
  OwnStream: Boolean;
  Stream: TStream;
  Buffer: Pointer;
  ContentLength, ContentWrite: LargeInt;
  I: LargeInt;
begin
  Result := False;
  try
    if not Socket.Connected then
      Exit;
    if not FValid then
      Exit;
    if FVerb = 'GET' then
    begin
      Result := True;
      Exit;
    end;
    if FBodyStream = nil then
    begin
      Stream := TStringStream.Create(FBodyText);
      OwnStream := True;
    end
    else
    begin
      Stream := FBodyStream;
      OwnStream := False;
    end;
    try
      I := Stream.Size - Stream.Position < 0;
      if I < 1 then
        ContentLength := 0
      else
        ContentLength := I;
      I := StrToInt64Def(FHeaders['Content-Length'], 0);
      if I <> ContentLength then
        Exit;
      if ContentLength = 0 then
      begin
        Result := True;
        Exit;
      end;
      ContentWrite := 0;
      if Assigned(FOnProgress) then
        FOnProgress(Self, ContentLength, ContentWrite);
      if FCancelled then
        Exit;
      Buffer := GetMem();

    finally
      if OwnStream then
        Stream.Free;
    end;
  finally
    FValid := Result;
  end;
end;

function THttpRequest.Send(Socket: TSocket): Boolean;
begin
  Result := SendHeader(Socket) and SendBody(Socket);
end;

function Whois(const Domain: string; out Response: string): Boolean;
type
  TDomainList = array of string;

const
  Domains: TDomainList = nil;

  procedure Init;
  begin
    Domains := TDomainList.Create(
     'ac whois.nic.ac',
     'ae whois.aeda.net.ae',
     'aero whois.aero',
     'af whois.nic.af',
     'ag whois.nic.ag',
     'al whois.ripe.net',
     'am whois.amnic.net',
     'as whois.nic.as',
     'asia whois.nic.asia',
     'at whois.nic.at',
     'au whois.aunic.net',
     'ax whois.ax',
     'az whois.ripe.net',
     'ba whois.ripe.net',
     'be whois.dns.be',
     'bg whois.register.bg',
     'bi whois.nic.bi',
     'biz whois.neulevel.biz',
     'bj www.nic.bj',
     'br whois.nic.br',
     'br.com whois.centralnic.com',
     'bt whois.netnames.net',
     'by whois.cctld.by',
     'bz whois.belizenic.bz',
     'ca whois.cira.ca',
     'cat whois.cat',
     'cc whois.nic.cc',
     'cd whois.nic.cd',
     'ch whois.nic.ch',
     'ck whois.nic.ck',
     'cl whois.nic.cl',
     'cn whois.cnnic.net.cn',
     'cn.com whois.centralnic.com',
     'co whois.nic.co',
     'co.nl whois.co.nl',
     'com whois.verisign-grs.com',
     'coop whois.nic.coop',
     'cx whois.nic.cx',
     'cy whois.ripe.net',
     'cz whois.nic.cz',
     'de whois.denic.de',
     'dk whois.dk-hostmaster.dk',
     'dm whois.nic.cx',
     'dz whois.nic.dz',
     'edu whois.educause.net',
     'ee whois.tld.ee',
     'eg whois.ripe.net',
     'es whois.nic.es',
     'eu whois.eu',
     'eu.com whois.centralnic.com',
     'fi whois.ficora.fi',
     'fo whois.nic.fo',
     'fr whois.nic.fr',
     'gb whois.ripe.net',
     'gb.com whois.centralnic.com',
     'gb.net whois.centralnic.com',
     'qc.com whois.centralnic.com',
     'ge whois.ripe.net',
     'gl whois.nic.gl',
     'gm whois.ripe.net',
     'gov whois.nic.gov',
     'gr whois.ripe.net',
     'gs whois.nic.gs',
     'hk whois.hknic.net.hk',
     'hm whois.registry.hm',
     'hn whois2.afilias-grs.net',
     'hr whois.dns.hr',
     'hu whois.nic.hu',
     'hu.com whois.centralnic.com',
     'id whois.pandi.or.id',
     'ie whois.domainregistry.ie',
     'il whois.isoc.org.il',
     'in whois.inregistry.net',
     'info whois.afilias.info',
     'int whois.isi.edu',
     'io whois.nic.io',
     'iq vrx.net',
     'ir whois.nic.ir',
     'is whois.isnic.is',
     'it whois.nic.it',
     'je whois.je',
     'jobs jobswhois.verisign-grs.com',
     'jp whois.jprs.jp',
     'ke whois.kenic.or.ke',
     'kg whois.domain.kg',
     'kr whois.nic.or.kr',
     'la whois2.afilias-grs.net',
     'li whois.nic.li',
     'lt whois.domreg.lt',
     'lu whois.restena.lu',
     'lv whois.nic.lv',
     'ly whois.lydomains.com',
     'ma whois.iam.net.ma',
     'mc whois.ripe.net',
     'md whois.nic.md',
     'me whois.nic.me',
     'mil whois.nic.mil',
     'mk whois.ripe.net',
     'mobi whois.dotmobiregistry.net',
     'ms whois.nic.ms',
     'mt whois.ripe.net',
     'mu whois.nic.mu',
     'mx whois.nic.mx',
     'my whois.mynic.net.my',
     'name whois.nic.name',
     'net whois.verisign-grs.com',
     'news whois.rightside.co',
     'nf whois.nic.cx',
     'ng whois.nic.net.ng',
     'nl whois.domain-registry.nl',
     'no whois.norid.no',
     'no.com whois.centralnic.com',
     'nu whois.nic.nu',
     'nz whois.srs.net.nz',
     'org whois.pir.org',
     'pl whois.dns.pl',
     'pr whois.nic.pr',
     'pro whois.registrypro.pro',
     'pt whois.dns.pt',
     'pw whois.nic.pw',
     'ro whois.rotld.ro',
     'ru whois.tcinet.ru',
     'sa saudinic.net.sa',
     'sa.com whois.centralnic.com',
     'sb whois.nic.net.sb',
     'sc whois2.afilias-grs.net',
     'se whois.nic-se.se',
     'se.com whois.centralnic.com',
     'se.net whois.centralnic.com',
     'sg whois.nic.net.sg',
     'sh whois.nic.sh',
     'si whois.arnes.si',
     'sk whois.sk-nic.sk',
     'sm whois.nic.sm',
     'st whois.nic.st',
     'so whois.nic.so',
     'su whois.tcinet.ru',
     'tc whois.adamsnames.tc',
     'tel whois.nic.tel',
     'tf whois.nic.tf',
     'th whois.thnic.net',
     'tj whois.nic.tj',
     'tk whois.nic.tk',
     'tl whois.domains.tl',
     'tm whois.nic.tm',
     'tn whois.ati.tn',
     'to whois.tonic.to',
     'tp whois.domains.tl',
     'tr whois.nic.tr',
     'travel whois.nic.travel',
     'tw whois.twnic.net.tw',
     'tv whois.nic.tv',
     'tz whois.tznic.or.tz',
     'ua whois.ua',
     'uk whois.nic.uk',
     'uk.com whois.centralnic.com',
     'uk.net whois.centralnic.com',
     'ac.uk whois.ja.net',
     'gov.uk whois.ja.net',
     'us whois.nic.us',
     'us.com whois.centralnic.com',
     'uy nic.uy',
     'uy.com whois.centralnic.com',
     'uz whois.cctld.uz',
     'va whois.ripe.net',
     'vc whois2.afilias-grs.net',
     've whois.nic.ve',
     'vg whois.adamsnames.tc',
     'ws whois.website.ws',
     'xxx whois.nic.xxx',
     'yu whois.ripe.net',
     'za.com whois.centralnic.com');
  end;

  function FindServer: string;
  var
    Strings: StringArray;
    A, B: string;
    S: string;
  begin
    if Domains = nil then
      Init;
    Result := '';
    Strings := Domain.Trim.ToLower.Split('.');
    if Strings.Length < 2 then
      Exit;
    A := Strings.Pop;
    B := Strings.Pop;
    if A.IsEmpty or B.IsEmpty then
      Exit;
    for S in Domains do
      if S.BeginsWith(A + ' ') or S.BeginsWith(B + '.' + A + ' ') then
        Exit(S.SecondOf(' '));
  end;

const
  WhoisPort = 43;
var
  Socket: TSocket;
  S: string;
begin
  Response := '';
  Result := False;
  S := FindServer;
  if S.IsEmpty then
    Exit;
  Socket := TSocket.Create;
  try
    if Socket.Connect(S, WhoisPort) then
    begin
      S := 'domain ' + Domain.Trim.ToLower + #10;
      if Socket.Write(S) = S.Length then
        Result := (Socket.Read(Response) > S.Length) and (Response.IndexOf('Domain Name:') > 0);
    end;
  finally
    Socket.Free;
  end;
end;

end.

