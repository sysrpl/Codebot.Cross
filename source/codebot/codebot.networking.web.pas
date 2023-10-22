(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2023                               *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.networking.web.txt> }
unit Codebot.Networking.Web;

{$i codebot.inc}
{$ifdef linux}
  {.$define use_curl}
{$endif}

interface

uses
  Classes,
  SysUtils,
  Codebot.System,
  Codebot.Collections,
  Codebot.Networking
  {$ifdef use_curl}, LibCurl{$endif};

{ TUrl parses urls such as https://example.com:8080/resource and
  captures the component values
  See also
  <link Codebot.Networking.Web.TUrl, TUrl members> }

type
  TUrl = record
  private
    FProtocol: string;
    FPort: Word;
    FDomain: string;
    FResource: string;
    FSecure: Boolean;
    FValid: Boolean;
    function GetAsString: string;
  public
    { Convert a TUrl to a string }
    class operator Implicit(const Value: TUrl): string;
    { Convert s string to a TUrl }
    class operator Implicit(const Value: string): TUrl;
    { Create a TUrl given a string }
    class function Create(const S: string): TUrl; static;
    { The protocol portion of the url, for example HTTP }
    property Protocol: string read FProtocol;
    { The port portion of the url, for example 8080 }
    property Port: Word read FPort;
    { The domain portion of the url, for example www.google.com }
    property Domain: string read FDomain;
    { The resource portion of the url, for example /search/?query=hello }
    property Resource: string read FResource;
    { Flag indicating if SSL should be used }
    property Secure: Boolean read FSecure;
    { Flag indicating if a url is properly formatted }
    property Valid: Boolean read FValid;
    { Convert the url back to a string }
    property AsString: string read GetAsString;
  end;

{ THttpResponseHeader parses a buffer and find components of a
  valid http response header
  See also
  <link Codebot.Networking.Web.THttpResponseHeader, THttpResponseHeader members> }

  THttpResponseHeader = record
  public
    { Response code such as 200 }
    Code: Integer;
    { Response status such as OK }
    Status: string;
    { Response key values }
    Keys:  TNamedStrings;
    { Response raw header text }
    RawHeader: string;
    { When Valid is true a complete header was processed from extract }
    Valid: Boolean;
    { Clears all component values }
    procedure Clear;
    { Attempt to parse an incomming response buffer }
    function Extract(var Buffer: string): Boolean;
  end;

{ WebSendRequest performs a http 1.1 web request and supports chunked replies }

  TResponseStatus = (
    { Socket connect failed }
    rsNoConnect,
    { No response was received }
    rsNoResponse,
    { Status code was not 200 OK }
    rsError,
    { Status code was 200 OK }
    rsSuccess,
    { Task was cancelled }
    rsCancelled);

function WebSendRequest(Url: TUrl; const Request: string; Response: TStream;
  out Header: THttpResponseHeader; Task: IAsyncTask = nil): TResponseStatus; overload;
function WebSendRequest(Url: TUrl; const Request: string; out Response: string;
  out Header: THttpResponseHeader; Task: IAsyncTask = nil): TResponseStatus; overload;

function WebSendRequestStream(Url: TUrl; Request: TStream; Response: TStream;
  out Header: THttpResponseHeader; Task: IAsyncTask = nil): TResponseStatus; overload;
function WebSendRequestStream(Url: TUrl; Request: TStream;  out Response: string;
  out Header: THttpResponseHeader; Task: IAsyncTask = nil): TResponseStatus; overload;

{ THttpPost manages sending multipart http post requests. It is recommended that
  when you attach streams to the body you turn ownership over to this class.
  Invoking build will generate a request stream.

  Note:

  Adding only one nameless part to the body causes the post to suppress
  multipart sections }

type
  THttpPost = class
  private
    type
      TStringValues = TNamedValues<string>;

      TPostValue = class
      public
        Name: string;
        MimeType: string;
        Text: string;
        FileName: string;
        Stream: TStream;
        OwnsStream: Boolean;
        destructor Destroy; override;
      end;

      TPostValues = TObjectList<TPostValue>;
  private
    FHeaderValues: TStringValues;
    FPostValues: TPostValues;
    FRequest: TStream;
    procedure Reset;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    { Add a custom header }
    function AddHeader(const Name, Value: string): THttpPost;
    { Add a text to the post body }
    function AddText(const Name, Text: string): THttpPost; overload;
    function AddText(const Name, MimeType, Text: string): THttpPost; overload;
    { Add a file to the post body }
    function AddFile(const Name, FileName: string): THttpPost; overload;
    function AddFile(const Name, MimeType, FileName: string): THttpPost; overload;
    { Add a stream to the post body }
    function AddStream(const Name: string; Stream: TStream; OwnsStream: Boolean = True): THttpPost; overload;
    function AddStream(const Name, MimeType: string; Stream: TStream; OwnsStream: Boolean = True): THttpPost; overload;
    { Build the post resulting in a request stream }
    procedure Build(Url: TUrl);
    property Request: TStream read FRequest;
  end;

type
  TTransmistHeaderCompleteEvent = procedure (Sender: TObject; const Header: THttpResponseHeader) of object;

{ THttpClient implements the http 1.0 client protocol
  See also
  <link Codebot.Networking.Web.THttpClient, THttpClient members> }

  THttpClient = class
  private
    FBufferSize: Integer;
    FCancelled: Boolean;
    FCompleted: Boolean;
    FUserAgent: string;
    FResponseHeader: THttpResponseHeader;
    FResponseStream: TStream;
    FResponseText: TStringStream;
    FFOnCancel: TNotifyEvent;
    FOnHeaderComplete: TTransmistHeaderCompleteEvent;
    FOnComplete: TNotifyEvent;
    FOnProgress: TTransmitEvent;
    function GetCode: Integer;
    function GetStatus: string;
    function GetName(Index: Integer): string;
    function GetValue(Name: string): string;
    function GetNameCount: Integer;
    function GetResponseText: string;
    function Process(const Url: TUrl; const Request: string): Boolean;
  protected
    { Complete is invoked when Process is about to return true }
    procedure Complete; virtual;
    { Invoke the OnCancel event }
    procedure DoCancel; virtual;
    { Invoke the OnHeaderComplete event }
    procedure DoHeaderComplete; virtual;
    { Invoke the OnResponseComplete event }
    procedure DoComplete; virtual;
    { Invoke the OnProgress event }
    procedure DoProgress(const Size, Transmitted: LargeWord); virtual;
  public
    { Create an http client instance }
    constructor Create;
    destructor Destroy; override;
    { Clear the last response }
    procedure Clear;
    { Cancel an ongoing response, can be invoked automatically when an unxpected condition is encountered }
    procedure Cancel;
    { Request a copy of the response header }
    procedure CopyHeader(out Header: THttpResponseHeader);
    { Send an HTTP GET request }
    function Get(const Url: TUrl): Boolean; overload;
    { Send an HTTP GET request with custom headers }
    function Get(const Url: TUrl; const Headers: TNamedStrings): Boolean; overload;
    { Send an HTTP POST request with custom headers and content }
    function Post(const Url: TUrl; const Headers: TNamedStrings;
      const ContentType: string; const Content: string): Boolean;
    { Send an HTTP POST request with an arguments form body }
    function PostArgs(const Url: TUrl; const Args: TNamedStrings): Boolean;
    { Send an HTTP POST request with a json body }
    function PostJson(const Url: TUrl; const Json: string): Boolean;
    { Send an HTTP POST request with an xml body }
    function PostXml(const Url: TUrl; const Xml: string): Boolean;
    { Optional size in bytes of the response buffer }
    property BufferSize: Integer read FBufferSize write FBufferSize;
    { Holds true if the last request completed properly }
    property Completed: Boolean read FCompleted;
    { The user agent as seen by the server }
    property UserAgent: string read FUserAgent write FUserAgent;
    { The response code returned from the server }
    property Code: Integer read GetCode;
    { The response status returned from the server }
    property Status: string read GetStatus;
    { Response header names }
    property Names[Index: Integer]: string read GetName;
    { Response header values }
    property Values[Name: string]: string read GetValue;
    { Response header name count }
    property NameCount: Integer read GetNameCount;
    { Set ResponseStream to write the response body to a stream }
    property ResponseStream: TStream read FResponseStream write FResponseStream;
    { If ResponseStream is nil then the response body is stored in ResponseText instead }
    property ResponseText: string read GetResponseText;
    { OnCancel is invoked if the request is stoped before completion }
    property OnCancel: TNotifyEvent read FFOnCancel write FFOnCancel;
    { OnHeaderComplete is invoked after a complete response header is read }
    property OnHeaderComplete: TTransmistHeaderCompleteEvent read FOnHeaderComplete write FOnHeaderComplete;
    { OnProgress is invoked as after the request header is received while bytes are being read }
    property OnProgress: TTransmitEvent read FOnProgress write FOnProgress;
    { OnComplete is invoked after a response is read in its entirety }
    property OnComplete: TNotifyEvent read FOnComplete write FOnComplete;
  end;

const
  SDefaultUA = 'Mozilla/5.0 (compatible; WebKit/Chrome)';

{ Simplified HTTP GET with response output to a stream }
function WebGet(const Url: TUrl; Response: TStream; const UserAgent: string = SDefaultUA): Boolean; overload;
{ Simplified HTTP GET with response output to a string }
function WebGet(const Url: TUrl; out Response: string; const UserAgent: string = SDefaultUA): Boolean; overload;
{ Simplified HTTP POST with response output to a stream }
function WebPost(const Url: TUrl; Args: TNamedStrings; Response: TStream; const UserAgent: string = SDefaultUA): Boolean; overload;
{ Simplified HTTP POST with response output to a string }
function WebPost(const Url: TUrl; Args: TNamedStrings; out Response: string; const UserAgent: string = SDefaultUA): Boolean; overload;
{$ifdef use_curl}
{ Use curl to HTTP GET with response output to a stream }
function CurlGet(const Url: string; Response: TStream; const UserAgent: string = SDefaultUA): Boolean; overload;
{ Use curl to HTTP GET with response output to a string }
function CurlGet(const Url: string; out Response: string; const UserAgent: string = SDefaultUA): Boolean; overload;
{$endif}

const
  ContentNone  = '';
  ContentText  = 'text/plain';
  ContentHtml  = 'text/html';
  ContentArgs  = 'application/x-www-form-urlencoded';
  ContentJson  = 'application/json';
  ContentXml  = 'text/xml; charset=utf-8';

{ HttpResponseHeaderExtract attempts to parse buffer and find a
  valid http response header }
function HttpResponseHeaderExtract(var Buffer: string; out Header: string;
  out BreakStyle: string): Boolean;
{ HttpRequestGet creates an http get request given a url }
function HttpRequestGet(const Url: TUrl; const UserAgent: string = SDefaultUA): string;
{ HttpRequestPost creates an http post request given a url and arguments }
function HttpRequestPostArgs(const Url: TUrl; const Args: TNamedStrings; const UserAgent: string = SDefaultUA): string;
{ HttpRequestPostJson creates an http post request given a url and json string }
function HttpRequestPostJson(const Url: TUrl; const Json: string; const UserAgent: string = SDefaultUA): string;
{ HttpRequestPostJson creates an http post request given a url and json string }
function HttpRequestPostXml(const Url: TUrl; const Xml: string; const UserAgent: string = SDefaultUA): string;
{ UrlEncode escapes char sequences suitable for posting data }
function UrlEncode(const Value: string): string;
{ UrlDecode reverts previously escaped char sequences }
function UrlDecode(const Value: string): string;
{ ArgsEncode converts name value pairs to a string suitable for posting }
function ArgsEncode(const Args: TNamedStrings): string;
{ ArgsDecode converts a posted string back to name value pairs }
function ArgsDecode(const Args: string): TNamedStrings;
{ MimeType extracts a mime type given a file name }
function MimeType(const FileName: string): string;

implementation

uses
  Codebot.Support;

function ProtocolPort(const Protocol: string): Word;
var
  S: string;
begin
  S := Protocol.ToUpper;
  if S = 'FTP' then
    Result := 21
  else if S = 'HTTP' then
    Result := 80
  else if S = 'HTTPS' then
    Result := 443
  else
    Result := 0;
end;

function DomainValidate(const S: string): Boolean;
begin
  Result := S <> '';
end;

{ TUrl }

class operator TUrl.Implicit(const Value: TUrl): string;
begin
  Result := Value.FProtocol.ToLower + '://' + Value.FDomain;
  if Value.FPort <> ProtocolPort(Value.FProtocol) then
    Result := Result + ':' + IntToStr(Value.FPort);
  if Value.FResource <> '/' then
    Result := Result + Value.FResource;
end;

class operator TUrl.Implicit(const Value: string): TUrl;
begin
  Result := TUrl.Create(Value);
end;

class function TUrl.Create(const S: string): TUrl;
var
  U: string;
begin
  Result.FProtocol := 'HTTP';
  if S.IndexOf('://') > 0 then
  begin
    U := S.FirstOf('://');
    if U <> '' then
      Result.FProtocol := U.ToUpper;
    U := S.SecondOf('://');
  end
  else
    U := S;
  Result.FPort := ProtocolPort(Result.FProtocol);
  Result.FResource := '/' + U.SecondOf('/');
  U := U.FirstOf('/');
  Result.FDomain := U.FirstOf(':');
  U := U.SecondOf(':');
  if U <> '' then
    Result.FPort := StrToIntDef(U, Result.FPort);
  Result.FSecure := Result.FProtocol = 'HTTPS';
  Result.FValid := DomainValidate(Result.FDomain) and (Result.FPort > 0);
end;

function TUrl.GetAsString: string;
begin
  Result := Self;
end;

{ THttpResponseHeader }

procedure THttpResponseHeader.Clear;
begin
  Code := 0;
  Status := '';
  RawHeader := '';
  Valid := False;
  Keys.Clear;
end;

function THttpResponseHeader.Extract(var Buffer: string): Boolean;
var
  BreakStyle: string;
  Lines, Row: StringArray;
  I: Integer;
begin
  Result := False;
  if Valid then
    Exit;
  Valid := HttpResponseHeaderExtract(Buffer, RawHeader, BreakStyle);
  if Valid then
  begin
    Lines := RawHeader.Split(BreakStyle);
    for I := Lines.Lo to Lines.Hi do
      if I = 0 then
      begin
        Row := Lines[I].Words;
        if Row.Length > 1 then
          Code := StrToIntDef(Row[1], 0);
        if Row.Length > 2 then
          Status := Row[2];
      end
      else
        Keys.Add(Lines[I].FirstOf(':').Trim, Lines[I].SecondOf(':').Trim);
  end;
  Result := Valid;
end;

type
  TReadBuffer = class
    FSocket: TSocket;
    FBuffer: string;
    FChunked: Boolean;
    FContentLength: Int64;
    FIndex: Integer;
    FLength: Integer;
  public
    { TODO: Use ContentLength instead of assuming the header Connection: Close }
    procedure Reset(Socket: TSocket; Buffer: string; Chunked: Boolean; ContentLength: Int64 = -1);
    function Read(var Buffer; BufferSize: LongWord): Integer; overload;
    function Read(out Text: string): Integer; overload;
    property Chunked: Boolean read FChunked;
  end;

procedure TReadBuffer.Reset(Socket: TSocket; Buffer: string; Chunked: Boolean; ContentLength: Int64 = -1);
begin
  FSocket := Socket;
  FBuffer := Buffer;
  FChunked := Chunked;
  FContentLength := ContentLength;
  FIndex := 1;
  FLength := Length(FBuffer);
end;

function TReadBuffer.Read(var Buffer; BufferSize: LongWord): Integer;
var
  B: PByte;
begin
  if FLength > 0 then
  begin
    Result := 0;
    B := PByte(@Buffer);
    while (FLength > 0) and (BufferSize > 0) do
    begin
      B^ := Byte(FBuffer[FIndex]);
      Inc(FIndex);
      Dec(FLength);
      Dec(BufferSize);
      Inc(Result);
    end;
  end
  else
    Result := FSocket.Read(Buffer, BufferSize);
end;

function TReadBuffer.Read(out Text: string): Integer;
var
  P: PChar;
begin
  if FLength > 0 then
  begin
    P := PChar(FBuffer);
    Inc(P, FIndex - 1);
    Text := P^;
    Result := FLength;
    FLength := 0;
  end
  else
    Result := FSocket.Read(Text);
end;

function WebSendRequest(Url: TUrl; const Request: string; Response: TStream;
  out Header: THttpResponseHeader; Task: IAsyncTask = nil): TResponseStatus;
var
  S: TStream;
begin
  S := TStringStream.Create(Request);
  try
    Result := WebSendRequestStream(Url, S, Response, Header, Task);
  finally
    S.Free;
  end;
end;

function WebSendRequest(Url: TUrl; const Request: string; out Response: string;
  out Header: THttpResponseHeader; Task: IAsyncTask = nil): TResponseStatus;
var
  S: TStringStream;
begin
  S := TStringStream.Create;
  try
    Result := WebSendRequest(Url, Request, S, Header, Task);
    Response := S.DataString;
  finally
    S.Free;
  end;
end;

function WebSendRequestStream(Url: TUrl; Request: TStream; Response: TStream;
  out Header: THttpResponseHeader; Task: IAsyncTask = nil): TResponseStatus;

  procedure DoProgress(Delta: Int64);
  begin
    if (Delta > 0) and (Task <> nil) then
      (Task as IAsyncRunnerBase).Tick(Delta);
  end;

  function IsCancelled: Boolean;
  begin
    Result := (Task <> nil) and Task.Cancelled;
  end;

  function ReadChunk(S: TReadBuffer; Chunk: string): string;
  var
    Buffer: string;
    P: PChar;
    I, J: Integer;
  begin
    Result := '';
    if IsCancelled then
      Exit;
    Chunk := '$' + Chunk;
    I := StrToIntDef(Chunk, 0);
    if I = 0 then
      Exit;
    SetLength({%H-}Buffer, I);
    P := PChar(Buffer);
    while I > 0 do
    begin
      if IsCancelled then
        Exit;
      J := S.Read(P^, LongWord(I));
      if J = 0 then
        Exit;
      DoProgress(J);
      Inc(P, J);
      Dec(I, J);
    end;
    Result := Buffer;
  end;

var
  Buffer, Data, Encoding, Chunk: string;
  Socket: TSocket;
  ReadBuffer: TReadBuffer;
  Read: Boolean;
  C: Char;
begin
  Result := rsNoConnect;
  Header.Clear;
  Buffer := '';
  Socket := TSocket.Create;
  ReadBuffer := TReadBuffer.Create;
  try
    Socket.Secure := Url.Secure;
    if Socket.Connect(Url.Domain, Url.Port) then
    begin
      if IsCancelled then
        Exit;
      Result := rsNoResponse;
      if not Socket.WriteStream(Request, Task) then
        Exit;
      Read := False;
      while not Read do
        while Socket.Read(Data) > 0 do
        begin
          Read := True;
          Buffer := Buffer + Data;
          if Header.Extract(Buffer) then
          begin
            if Header.Code = 200 then
              Result := rsSuccess
            else
              Result := rsError;
            Encoding := Header.Keys.Values['Transfer-Encoding'];
            ReadBuffer.Reset(Socket, Buffer, StrContains(Encoding, 'chunked', True));
            if ReadBuffer.Chunked then
            repeat
              Chunk := '';
              if IsCancelled then
                Exit;
              while ReadBuffer.Read({%H-}C, 1) = 1 do
              begin
                if IsCancelled then
                  Exit;
                if (C = #10) and (Chunk <> '') then
                begin
                  Data := ReadChunk(ReadBuffer, Chunk);
                  if IsCancelled then
                    Exit;
                  Chunk := '';
                  if Length(Data) < 1 then
                    Break;
                  Response.Write(Pointer(Data)^, Length(Data));
                end
                else if C > ' ' then
                  Chunk := Chunk + C;
              end;
            until Chunk = ''
            else while ReadBuffer.Read(Data) > 0 do
            begin
              if IsCancelled then
                Exit;
              DoProgress(Length(Data));
              Response.Write(Pointer(Data)^, Length(Data));
            end;
            Break;
          end;
        end;
    end;
  finally
    ReadBuffer.Free;
    Socket.Free;
    if IsCancelled then
      Result := rsCancelled;
  end;
end;

function WebSendRequestStream(Url: TUrl; Request: TStream; out Response: string;
  out Header: THttpResponseHeader; Task: IAsyncTask = nil): TResponseStatus;
var
  S: TStringStream;
begin
  S := TStringStream.Create;
  try
    Result := WebSendRequestStream(Url, Request, S, Header, Task);
    Response := S.DataString;
  finally
    S.Free;
  end;
end;

{ THttpClient }

constructor THttpClient.Create;
begin
  inherited Create;
  FResponseText := TStringStream.Create('');
  Clear;
end;

destructor THttpClient.Destroy;
begin
  FResponseText.Free;
  inherited Destroy;
end;

procedure THttpClient.Clear;
begin
  FCompleted := False;
  FCancelled := True;
  FResponseHeader.Clear;
  FResponseText.Size := 0;
end;

procedure THttpClient.Complete;
begin
  if not FCompleted then
  begin
    FCompleted := True;
    FCancelled := True;
    DoComplete;
  end;
end;

procedure THttpClient.Cancel;
begin
  if not FCancelled then
  begin
    FCancelled := True;
    DoCancel;
  end;
end;

procedure THttpClient.CopyHeader(out Header: THttpResponseHeader);
begin
  Header := FResponseHeader;
end;

function THttpClient.GetCode: Integer;
begin
  Result := FResponseHeader.Code;
end;

function THttpClient.GetStatus: string;
begin
  Result := FResponseHeader.Status;
end;

function THttpClient.GetName(Index: Integer): string;
begin
  Result := FResponseHeader.Keys.Names[Index];
end;

function THttpClient.GetValue(Name: string): string;
begin
  Result := FResponseHeader.Keys.Values[Name];
end;

function THttpClient.GetNameCount: Integer;
begin
  Result := FResponseHeader.Keys.Count;
end;

function THttpClient.GetResponseText: string;
begin
  Result := FResponseText.DataString;
end;

procedure THttpClient.DoCancel;
begin
  if Assigned(FFOnCancel) then
    FFOnCancel(Self);
end;

procedure THttpClient.DoHeaderComplete;
begin
  if Assigned(FOnHeaderComplete) then
    FOnHeaderComplete(Self, FResponseHeader);
end;

procedure THttpClient.DoComplete;
begin
  if Assigned(FOnComplete) then
    FOnComplete(Self);
end;

procedure THttpClient.DoProgress(const Size, Transmitted: LargeWord);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, Size, Transmitted);
end;

function THttpClient.Process(const Url: TUrl; const Request: string): Boolean;

  function Stream: TStream;
  begin
    if FResponseStream <> nil then
      Result := FResponseStream
    else
      Result := FResponseText;
  end;

const
  BufferSizeMin = $1000;
  BufferSizeDef = $10000;
  BufferSizeMax = $100000;
var
  Socket: TSocket;
  Temp, S: string;
  ContentLength, ContentRead: LargeInt;
  Count: LongInt;
  Buffer: Pointer;
  BufferSize: Integer;
  I: Integer;
begin
  BufferSize := FBufferSize;
  if BufferSize < 1 then
    BufferSize := BufferSizeDef
  else if BufferSize < BufferSizeMin then
    BufferSize := BufferSizeMin
  else if BufferSize > BufferSizeMax then
    BufferSize := BufferSizeMax;
  Result := False;
  Clear;
  try
    FCancelled := False;
    if not Url.Valid then
      Exit;
    if Request.Length = 0 then
      Exit;
    Socket := TSocket.Create;
    try
      Socket.Secure := Url.Secure;
      Socket.Timeout := 4000;
      if not Socket.Connect(Url.Domain, Url.Port) then
        Exit;
      if not Socket.WriteText(Request) then
        Exit;
      Temp := '';
      repeat
        I := Socket.Read(S);
        if I < 1 then
          Exit;
        Temp := Temp + S;
      until FResponseHeader.Extract(Temp);
      DoHeaderComplete;
      S := FResponseHeader.Keys.Values['Content-Length'];
      if S <> '' then
      begin
        ContentLength := StrToInt64Def(S, 0);
        if ContentLength < 1 then
          Exit(True);
        if Temp.Length >= ContentLength then
        begin
          Stream.Write(Temp[1], ContentLength);
          Exit(True);
        end;
      end
      else
        ContentLength := High(ContentLength);
      ContentRead := Temp.Length;
      if ContentRead > 0 then
        Stream.Write(Temp[1], Temp.Length);
      Temp := '';
      GetMem(Buffer, BufferSize);
      try
        repeat
          Count := Socket.Read(Buffer^, BufferSize);
          if Count > 0 then
          begin
            if Count + ContentRead >= ContentLength then
              Count := ContentLength - ContentRead;
            if Stream.Write(Buffer^, Count) = Count then
            begin
              ContentRead := ContentRead + Count;
              DoProgress(ContentLength, ContentRead);
            end
            else
              Exit;
          end;
        until (FCancelled) or (Count < 1) or (ContentRead >= ContentLength);
        if FCancelled then
          Result := False
        else if S <> '' then
          Result := ContentRead >= ContentLength
        else
          Result := True;
      finally
        FreeMem(Buffer);
      end;
    finally
      Socket.Free;
    end;
  finally
    if Result then
      Complete
    else
      Cancel;
  end;
end;

function THttpClient.Get(const Url: TUrl): Boolean;
var
  S: string;
begin
  S := HttpRequestGet(Url, FUserAgent);
  Result := Process(Url, S);
end;

function THttpClient.Get(const Url: TUrl; const Headers: TNamedStrings): Boolean;
var
  Name, Value: string;
  S: string;
  I: Integer;
begin
  S := 'GET ' + Url.Resource + ' HTTP/1.0'#13#10 +
    'Host: ' + Url.Domain + #13#10;
  for I := 0 to Headers.Count - 1 do
  begin
    Name := Headers.Names[I];
    Value := Headers.ValueByIndex[I];
    S := S + Name + ': ' + Value + #13#10;
  end;
  if UserAgent <> '' then
    S := S + 'User-Agent: ' + UserAgent + #13#10;
  S := S + 'Connection: Close'#13#10#13#10;
  Result := Process(Url, S);
end;

function THttpClient.Post(const Url: TUrl; const Headers: TNamedStrings;
  const ContentType: string; const Content: string): Boolean;
var
  Name, Value: string;
  S: string;
  I: Integer;
begin
  S := 'POST ' + Url.Resource + ' HTTP/1.0'#13#10 +
    'Host: ' + Url.Domain + #13#10;
  for I := 0 to Headers.Count - 1 do
  begin
    Name := Headers.Names[I];
    Value := Headers.ValueByIndex[I];
    S := S + Name + ': ' + Value + #13#10;
  end;
  if Content.Length > 0 then
  begin
    S := S + 'Content-Type: ' + ContentType + #13#10;
    S := S + 'Content-Length: ' + IntToStr(Content.Length) + #13#10;
  end;
  if UserAgent <> '' then
    S := S + 'User-Agent: ' + UserAgent + #13#10;
  S := S + 'Connection: Close'#13#10#13#10;
  if Content.Length > 0 then
    S := S + Content;
  Result := Process(Url, S);
end;

function THttpClient.PostArgs(const Url: TUrl; const Args: TNamedStrings): Boolean;
var
  S: string;
begin
  S := HttpRequestPostArgs(Url, Args, FUserAgent);
  Result := Process(Url, S);
end;

function THttpClient.PostJson(const Url: TUrl; const Json: string): Boolean;
var
  S: string;
begin
  S := HttpRequestPostJson(Url, Json, FUserAgent);
  Result := Process(Url, S);
end;

function THttpClient.PostXml(const Url: TUrl; const Xml: string): Boolean;
var
  S: string;
begin
  S := HttpRequestPostXml(Url, Xml, FUserAgent);
  Result := Process(Url, S);
end;

function HttpResponseHeaderExtract(var Buffer: string; out Header: string; out BreakStyle: string): Boolean;
const
  Breaks: array[0..3] of string = (#10#10, #13#10#13#10, #13#13, #10#13#10#13);
var
  First, Index: Integer;
  I, J: Integer;
begin
  Result := False;
  Header := '';
  BreakStyle := '';
  First := -1;
  Index := -1;
  for I := Low(Breaks) to High(Breaks) do
  begin
    J := Buffer.IndexOf(Breaks[I]);
    if J < 1 then
      Continue;
    if (First < 0) or (J < First) then
    begin
      First := J;
      Index := I;
    end;
  end;
  if Index > -1 then
  begin
    Header := Buffer.FirstOf(Breaks[Index]);
    Buffer := Buffer.SecondOf(Breaks[Index]);
    BreakStyle := Breaks[Index];
    BreakStyle.Length := BreakStyle.Length div 2;
    Result := True;
  end;
end;

function HttpRequestGet(const Url: TUrl; const UserAgent: string = SDefaultUA): string;
begin
  if not Url.Valid then
    Exit('');
  Result :=
    'GET ' + Url.Resource + ' HTTP/1.0'#13#10 +
    'Host: ' + Url.Domain + #13#10;
  if UserAgent <> '' then
    Result := Result + 'User-Agent: ' + UserAgent + #13#10;
  Result := Result + 'Connection: Close'#13#10#13#10;
end;

function HttpRequestPostArgs(const Url: TUrl; const Args: TNamedStrings; const UserAgent: string = SDefaultUA): string;
var
  Content: string;
begin
  if not Url.Valid then
    Exit('');
  Content := ArgsEncode(Args);
  Result :=
    'POST ' + Url.Resource + ' HTTP/1.0'#13#10 +
    'Host: ' + Url.Domain + #13#10 +
    'Content-Length: ' + IntToStr(Content.Length) + #13#10 +
    'Content-Type: ' + ContentArgs + #13#10;
  if UserAgent <> '' then
    Result := Result + 'User-Agent: ' + UserAgent + #13#10;
  Result := Result + 'Connection: Close'#13#10#13#10 + Content;
end;

function HttpRequestPostJson(const Url: TUrl; const Json: string; const UserAgent: string = SDefaultUA): string;
begin
  if not Url.Valid then
    Exit('');
  Result :=
    'POST ' + Url.Resource + ' HTTP/1.0'#13#10 +
    'Host: ' + Url.Domain + #13#10 +
    'Content-Length: ' + IntToStr(Json.Length) + #13#10 +
    'Content-Type: ' + ContentJson + #13#10;
  if UserAgent <> '' then
    Result := Result + 'User-Agent: ' + UserAgent + #13#10;
  Result := Result + 'Connection: Close'#13#10#13#10 + Json;
end;

function HttpRequestPostXml(const Url: TUrl; const Xml: string; const UserAgent: string = SDefaultUA): string;
begin
  if not Url.Valid then
    Exit('');
  Result :=
    'POST ' + Url.Resource + ' HTTP/1.0'#13#10 +
    'Host: ' + Url.Domain + #13#10 +
    'Content-Length: ' + IntToStr(Xml.Length) + #13#10 +
    'Content-Type: ' + ContentXml + #13#10;
  if UserAgent <> '' then
    Result := Result + 'User-Agent: ' + UserAgent + #13#10;
  Result := Result + 'Connection: Close'#13#10#13#10 + Xml;
end;

function UrlEncode(const Value: string): string;
var
  C: Char;
  I: Integer;
begin
  Result := '';
  for I := 1 to Value.Length do
  begin
    C := Value[I];
    if C in ['-', '_', '.', '~', '0'..'9', 'A'..'Z', 'a'..'z'] then
      Result := Result + C
    else
      Result := Result + '%' + IntToHex(Ord(C), 2);
  end;
end;

function UrlDecode(const Value: string): string;
var
  C: Char;
  S: string;
  I, J: Integer;
begin
  Result := '';
  I := Value.Length  + 1;
  J := 1;
  while J < I do
  begin
    C := Value[J];
    if C = '%' then
    begin
      if J + 2 > I then
        Exit('');
      S := '$' + Value[J + 1] + Value[J + 2];
      C := Chr(StrToInt(S));
      Inc(J, 2);
    end;
    Result := Result + C;
    Inc(J);
  end;
end;

function ArgsEncode(const Args: TNamedStrings): string;
var
  N, V: string;
  I: Integer;
begin
  Result := '';
  for I := 0 to Args.Count - 1 do
  begin
    if Result <> '' then
      Result := Result + '&';
    N := Args.Names[I];
    V := Args.ValueByIndex[I];
    Result := Result + UrlEncode(N) + '=' + UrlEncode(V);
  end;
end;

function ArgsDecode(const Args: string): TNamedStrings;
var
  Pairs, NameValue: StringArray;
  S: string;
  N, V: string;
begin
  Result.Clear;
  Pairs := Args.Split('&');
  for S in Pairs do
  begin
    NameValue := S.Split('=');
    if NameValue.Length <> 2 then
    begin
      Result.Clear;
      Exit;
    end;
    N := UrlDecode(NameValue[0]);
    V := UrlDecode(NameValue[1]);
    if N <> '' then
      Result.Add(N, V);
  end;
end;

function MimeType(const FileName: string): string;
var
  S: string;
begin
  S := FileExtractExt(FileName).ToLower;
  if s = '.7z' then
    Exit('application/x-7z-compressed');
  if s = '.aac' then
    Exit('audio/aac');
  if s = '.avi' then
    Exit('video/avi');
  if s = '.bmp' then
    Exit('image/bmp');
  if s = '.css' then
    Exit('text/css');
  if s = '.csv' then
    Exit('text/csv');
  if s = '.doc' then
    Exit('application/msword');
  if s = '.ocx' then
    Exit('application/vnd.openxmlformats-officedocument.wordprocessingml.document');
  if s = '.gif' then
    Exit('image/gif');
  if s = '.htm' then
    Exit('text/html');
  if s = '.html' then
    Exit('text/html');
  if s = '.jpeg' then
    Exit('image/jpeg');
  if s = '.jpg' then
    Exit('image/jpeg');
  if s = '.js' then
    Exit('application/javascript');
  if s = '.json' then
    Exit('application/json');
  if s = '.mov' then
    Exit('video/quicktime');
  if s = '.m4a' then
    Exit('audio/mp4a');
  if s = '.mp3' then
    Exit('audio/mpeg');
  if s = '.m4v' then
    Exit('video/mp4');
  if s = '.mp4' then
    Exit('video/mp4');
  if s = '.mpeg' then
    Exit('video/mpeg');
  if s = '.mpg' then
    Exit('video/mpeg');
  if s = '.ogg' then
    Exit('audio/ogg');
  if s = '.ogv' then
    Exit('video/ogv');
  if s = '.pdf' then
    Exit('application/pdf');
  if s = '.png' then
    Exit('image/png');
  if s = '.ppt' then
    Exit('application/vnd.ms-powerpoint');
  if s = '.ptx' then
    Exit('application/vnd.openxmlformats-officedocument.presentationml.presentation');
  if s = '.qt' then
    Exit('video/quicktime');
  if s = '.svg' then
    Exit('image/svg');
  if s = '.swf' then
    Exit('application/x-shockwave-flash');
  if s = '.tif' then
    Exit('image/tiff');
  if s = '.tiff' then
    Exit('image/tiff');
  if s = '.ini' then
    Exit('text/plain');
  if s = '.cfg' then
    Exit('text/plain');
  if s = '.cs' then
    Exit('text/plain');
  if s = '.pas' then
    Exit('text/plain');
  if s = '.sh' then
    Exit('text/plain');
  if s = '.txt' then
    Exit('text/plain');
  if s = '.wav' then
    Exit('audio/x-wav');
  if s = '.wma' then
    Exit('audio/x-ms-wma');
  if s = '.wmv' then
    Exit('audio/x-ms-wmv');
  if s = '.xls' then
    Exit('application/vnd.ms-excel');
  if s = '.lsx' then
    Exit('application/vnd.openxmlformats-officedocument.spreadsheetml.sheet');
  if s = '.xml' then
    Exit('text/xml');
  if s = '.zip' then
    Exit('application/zip');
  Result := 'application/octet-stream';
end;

function WebGet(const Url: TUrl; Response: TStream; const UserAgent: string = SDefaultUA): Boolean;
var
  Request: THttpClient;
begin
  Request := THttpClient.Create;
  try
    Request.UserAgent := UserAgent;
    Request.ResponseStream := Response;
    Result := Request.Get(Url) and (Request.Code = 200);
  finally
    Request.Free;
  end;
end;

function WebGet(const Url: TUrl; out Response: string; const UserAgent: string = SDefaultUA): Boolean;
var
  Request: THttpClient;
begin
  Request := THttpClient.Create;
  try
    Request.UserAgent := UserAgent;
    Result := Request.Get(Url) and (Request.Code = 200);
    Response := Request.ResponseText;
  finally
    Request.Free;
  end;
end;

function WebPost(const Url: TUrl; Args: TNamedStrings; Response: TStream; const UserAgent: string = SDefaultUA): Boolean;
var
  Request: THttpClient;
begin
  Request := THttpClient.Create;
  try
    Request.UserAgent := UserAgent;
    Request.ResponseStream := Response;
    Result := Request.PostArgs(Url, Args) and (Request.Code = 200);
  finally
    Request.Free;
  end;
end;

function WebPost(const Url: TUrl; Args: TNamedStrings; out Response: string; const UserAgent: string = SDefaultUA): Boolean;
var
  Request: THttpClient;
begin
  Request := THttpClient.Create;
  try
    Request.UserAgent := UserAgent;
    Result := Request.PostArgs(Url, Args) and (Request.Code = 200);
    Response := Request.ResponseText;
  finally
    Request.Free;
  end;
end;

{$ifdef use_curl}
function CurlWriteStream(Ptr: PByte; MemberSize, MemberCount: UIntPtr; Response: TStream): UIntPtr; cdecl;
begin
  Result := MemberSize * MemberCount;
  Response.Write(Ptr^, Result);
end;

function CurlGet(const Url: string; Response: TStream; const UserAgent: string = SDefaultUA): Boolean;
var
  Curl: PCURL;
begin
  Result := False;
  if Url = '' then
    Exit;
  if Response = nil then
    Exit;
  Curl := curl_easy_init();
  if Curl = nil then
    Exit;
  try
    curl_easy_setopt(Curl, CURLOPT_URL, [PChar(Url)]);
    if UserAgent <> '' then
      curl_easy_setopt(Curl, CURLOPT_USERAGENT, [PChar(UserAgent)]);
    curl_easy_setopt(Curl, CURLOPT_WRITEFUNCTION, [@CurlWriteStream]);
    curl_easy_setopt(Curl, CURLOPT_WRITEDATA, [Pointer(Response)]);
    curl_easy_setopt(Curl, CURLOPT_SSL_VERIFYPEER, [0]);
    curl_easy_setopt(Curl, CURLOPT_SSL_VERIFYHOST, [0]);
    Result := curl_easy_perform(Curl) = CURLE_OK;
  finally
    curl_easy_cleanup(Curl);
  end;
end;

function CurlWriteString(Ptr: PChar; MemberSize, MemberCount: UIntPtr; var Response: string): UIntPtr; cdecl;
var
  S: string;
begin
  SetString(S, Ptr, MemberSize * MemberCount);
  Response := Response + S;
  Result := MemberSize * MemberCount;
end;

function CurlGet(const Url: string; out Response: string; const UserAgent: string = SDefaultUA): Boolean;
var
  Curl: PCURL;
begin
  Response := '';
  Result := False;
  if Url = '' then
    Exit;
  Curl := curl_easy_init();
  if Curl = nil then
    Exit;
  try
    curl_easy_setopt(Curl, CURLOPT_URL, [PChar(Url)]);
    if UserAgent <> '' then
      curl_easy_setopt(Curl, CURLOPT_USERAGENT, [PChar(UserAgent)]);
    curl_easy_setopt(Curl, CURLOPT_WRITEFUNCTION, [@CurlWriteString]);
    curl_easy_setopt(Curl, CURLOPT_WRITEDATA, [@Response]);
    curl_easy_setopt(Curl, CURLOPT_SSL_VERIFYPEER, [0]);
    curl_easy_setopt(Curl, CURLOPT_SSL_VERIFYHOST, [0]);
    Result := curl_easy_perform(Curl) = CURLE_OK;
  finally
    curl_easy_cleanup(Curl);
  end;
end;
{$endif}

const
  PostBoundary = 'post_boundary_C5D3DFF684C043DCA9715B823FBB15BD';
  PostBoundaryLine = '--' + PostBoundary + #13#10;

function PostCount(Post: THttpPost): Integer;
begin
  Result := Post.FPostValues.Count;
end;

function PostSection(Post: THttpPost; Index: Integer): string;
var
  Item: THttpPost.TPostValue;
  S: string;
begin
  Result := '';
  if Index < 0 then
    Exit;
  if Index >= PostCount(Post) then
    Exit;
  Item := Post.FPostValues[Index];
  Result :=
    PostBoundaryLine +
    'Content-Disposition: form-data; name="' + Item.Name + '"';
  if Item.Stream is TFileStream then
  begin
    S := FileExtractName(TFileStream(Item.Stream).FileName);
    Result := Result + '; filename= "' + S + '"';
  end
  else if (Item.FileName <> '') and (FileExists(Item.FileName)) then
  begin
    S := FileExtractName(Item.FileName);
    Result := Result + '; filename= "' + S + '"';
  end;
  Result := Result + #13#10 +
    'Content-Type: ' + Item.MimeType + #13#10 +
    #13#10;
end;

{ THttpPost.TPostValue }

destructor THttpPost.TPostValue.Destroy;
begin
  if OwnsStream then
    Stream.Free;
  inherited Destroy;
end;

{ THttpPost }

constructor THttpPost.Create;
begin
  inherited Create;
  FPostValues := TPostValues.Create(True);
end;

destructor THttpPost.Destroy;
begin
  Clear;
  FPostValues.Free;
  inherited Destroy;
end;

procedure THttpPost.Clear;
begin
  Reset;
  FPostValues.Clear;
end;

procedure THttpPost.Reset;
begin
  FRequest.Free;
  FRequest := nil;
end;

function THttpPost.AddHeader(const Name, Value: string): THttpPost;
begin
  Reset;
  FHeaderValues.Add(Name, Value);
  Result := Self;
end;

function THttpPost.AddText(const Name, Text: string): THttpPost; overload;
begin
  Result := AddText(Name, '', Text);
end;

function THttpPost.AddText(const Name, MimeType, Text: string): THttpPost; overload;
var
  Item: TPostValue;
  S: string;
begin
  Reset;
  S := Trim(MimeType);
  if S = '' then
    S := 'text/plain';
  Item := TPostValue.Create;
  Item.Name := Name;
  Item.MimeType := S;
  Item.Text := Text;
  FPostValues.Add(Item);
  Result := Self;
end;

function THttpPost.AddFile(const Name, FileName: string): THttpPost;
begin
  Result := AddFile(Name, '', FileName);
end;

function THttpPost.AddFile(const Name, MimeType, FileName: string): THttpPost;
var
  Item: TPostValue;
  S: string;
begin
  Reset;
  if not FileExists(FileName) then
    Exit;
  S := Trim(MimeType);
  if S = '' then
    S := Codebot.Networking.Web.MimeType(FileName);
  Item := TPostValue.Create;
  Item.Name := Name;
  Item.MimeType := S;
  Item.FileName := FileName;
  FPostValues.Add(Item);
  Result := Self;
end;

function THttpPost.AddStream(const Name: string; Stream: TStream;
  OwnsStream: Boolean = True): THttpPost; overload;
begin
  Result := AddStream(Name, '', Stream, OwnsStream);
end;

function THttpPost.AddStream(const Name, MimeType: string; Stream: TStream;
  OwnsStream: Boolean = True): THttpPost;
var
  Item: TPostValue;
  S: string;
begin
  Reset;
  S := Trim(MimeType);
  if S = '' then
    S := 'application/octet-stream';
  Item := TPostValue.Create;
  Item.Name := Name;
  Item.MimeType := S;
  Item.Stream := Stream;
  Item.OwnsStream := OwnsStream;
  FPostValues.Add(Item);
  Result := Self;
end;

procedure THttpPost.Build(Url: TUrl);
var
  Stream: TAggregateStream;
  Item: TPostValue;
  S: string;
  I: Integer;
begin
  if FRequest <> nil then
    Exit;
  S :=
    'POST ' + Url.Resource + ' HTTP/1.1'#13#10 +
    'Host: ' + Url.Domain + #13#10;
  for I := 0 to FHeaderValues.Count - 1 do
    S := S +
      FHeaderValues.Names[I] + ': ' + FHeaderValues.ValueByIndex[I] + #13#10;
  { If there are no body parts then we have zero content length }
  if FPostValues.Count = 0 then
  begin
    S := S +
      'Content-Length: 0'#13#10 +
      'Connection: Close'#13#10 +
      #13#10;
    FRequest := TStringStream.Create(S);
    Exit;
  end;
  { If there is one unmamed body part then we do not have a multipart post }
  if (FPostValues.Count = 1) and (FPostValues[0].Name = '') then
  begin
    Stream := TAggregateStream.Create;
    Item := FPostValues[0];
    S := S +
      'Content-Type: ' + Item.MimeType + #13#10;
    if Item.Stream <> nil then
      Stream.AddStream(Stream, False)
    else if (Item.FileName <> '') and (FileExists(Item.FileName)) then
      Stream.AddFile(Item.FileName)
    else if Item.FileName <> '' then
      Stream.AddText(Item.FileName)
    else
      Stream.AddText(Item.Text);
    S := S +
      'Content-Length: ' + IntToStr(Stream.Size) + #13#10 +
      'Connection: Close'#13#10 +
      #13#10;
    FRequest := TAggregateStream.Create;
    TAggregateStream(FRequest).AddText(S);
    TAggregateStream(FRequest).AddStream(Stream);
    Exit;
  end;
  { We have determined that this is a multipart post }
  S := S +
    'Content-Type: multipart/form-data; boundary=' + PostBoundary + #13#10;
  Stream := TAggregateStream.Create;
  for I := 0 to FPostValues.Count - 1 do
  begin
    Item := FPostValues[I];
    Stream.AddText(PostSection(Self, I));
    if Item.Stream <> nil then
      Stream.AddStream(Item.Stream, False)
    else if (Item.FileName <> '') and (FileExists(Item.FileName)) then
      Stream.AddFile(Item.FileName)
    else if Item.FileName <> '' then
      Stream.AddText(Item.FileName)
    else
      Stream.AddText(Item.Text);
    Stream.AddText(#13#10);
  end;
  Stream.AddText('--' + PostBoundary + '--');
  S := S +
    'Content-Length: ' + IntToStr(Stream.Size) + #13#10 +
    'Connection: Close'#13#10 +
    #13#10;
  FRequest := TAggregateStream.Create;
  TAggregateStream(FRequest).AddText(S);
  TAggregateStream(FRequest).AddStream(Stream);
end;

end.

