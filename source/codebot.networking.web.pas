(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified March 2015                                 *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.networking.web.txt> }
unit Codebot.Networking.Web;

{$i codebot.inc}

interface

uses
  Classes,
  SysUtils,
  Codebot.System,
  Codebot.Text,
  Codebot.Text.Xml,
  Codebot.Networking;

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
    { Reponse key values }
    Keys:  TNamedStrings;
    { Reponse raw header text }
    RawHeader: string;
    { When Valid is true a complete header was processed from extract }
    Valid: Boolean;
    { Reset clears all components and sets Valid to false }
    procedure Reset;
    { Attempt to parse an incomming response buffer }
    function Extract(var Buffer: string): Boolean;
  end;

{ THttpClient }

  THttpClient = class
  private
    FUserAgent: string;
    FCancelled: Boolean;
    FHeader: THttpResponseHeader;
    FOnProgress: TTransmitEvent;
    function GetCode: Integer;
    function GetStatus: string;
    function GetName(Index: Integer): string;
    function GetValue(Name: string): string;
    function GetKeyCount: Integer;
    function Process(const Url: TUrl; const Request: string; Stream: TStream;
      BufferSize: LongInt): Boolean;
  protected
    { Invoke the OnProgress event }
    procedure DoProgress(const Size, Transmitted: LargeWord); virtual;
  public
    { Cancel an ongoing response }
    procedure Cancel;
    { Send a request with custom verb and headers }
    function SendRequest(const Url: TUrl; Verb: string; const Headers: TNamedStrings;
      Stream: TStream; BufferSize: LongInt = $1000): Boolean;
    { Get and output to a stream }
    function Get(const Url: TUrl; Stream: TStream; BufferSize: LongInt = $1000): Boolean; overload;
    { Get and output to a string }
    function Get(const Url: TUrl; out Text: string): Boolean; overload;
    { Post name value pairs and output to a stream }
    function PostArgs(const Url: TUrl; const Args: TNamedStrings;
      Stream: TStream; BufferSize: LongInt = $1000): Boolean; overload;
    { Post name value pairs and output to a string }
    function PostArgs(const Url: TUrl; const Args: TNamedStrings;
      out Text: string): Boolean; overload;
    { Post json and output to a stream }
    function PostJson(const Url: TUrl; const Json: string;
      Stream: TStream; BufferSize: LongInt = $1000): Boolean; overload;
    { Post json and output to a string }
    function PostJson(const Url: TUrl; const Json: string;
      out Text: string): Boolean; overload;
    { The user agent as seen by the server }
    property UserAgent: string read FUserAgent write FUserAgent;
    { The response code returned from the server }
    property Code: Integer read GetCode;
    { The response status returned from the server }
    property Status: string read GetStatus;
    { Response haader names }
    property Names[Index: Integer]: string read GetName;
    { Response header values }
    property Values[Name: string]: string read GetValue;
    { Response header count }
    property KeyCount: Integer read GetKeyCount;
    { OnProgress can be used to provide feedback during responses, or to cancel an in progress response }
    property OnProgress: TTransmitEvent read FOnProgress write FOnProgress;
  end;

{ Simplified http get output to a stream }
function WebGet(const Url: TUrl; Stream: TStream; const UserAgent: string = ''): Boolean; overload;
{ Simplified http get output to a string }
function WebGet(const Url: TUrl; out Text: string; const UserAgent: string = ''): Boolean; overload;

{ HttpResponseHeaderExtract attempts to parse buffer and find a
  valid http response header }
function HttpResponseHeaderExtract(var Buffer: string; out Header: string;
  out BreakStyle: string): Boolean;
{ HttpRequestGet creates an http get request given a url }
function HttpRequestGet(const Url: TUrl; const UserAgent: string = ''): string;
{ HttpRequestPost creates an http post request given a url and arguments }
function HttpRequestPostArgs(const Url: TUrl; const Args: TNamedStrings; const UserAgent: string = ''): string;
{ HttpRequestPostJson creates an http post request given a url and json string }
function HttpRequestPostJson(const Url: TUrl; const Json: string; const UserAgent: string = ''): string;
{ UrlEncode escapes most char sequences suitable for posting data }
function UrlEncode(const Value: string): string;

implementation

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

{ THttpResponseHeader }

procedure THttpResponseHeader.Reset;
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

{ THttpClient }

procedure THttpClient.Cancel;
begin
  FCancelled := True;
end;

function THttpClient.GetCode: Integer;
begin
  Result := FHeader.Code;
end;

function THttpClient.GetStatus: string;
begin
  Result := FHeader.Status;
end;

function THttpClient.GetName(Index: Integer): string;
begin
  Result := FHeader.Keys.Names[Index];
end;

function THttpClient.GetValue(Name: string): string;
begin
  Result := FHeader.Keys.Values[Name];
end;

function THttpClient.GetKeyCount: Integer;
begin
  Result := FHeader.Keys.Count;
end;

procedure THttpClient.DoProgress(const Size, Transmitted: LargeWord);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, Size, Transmitted);
end;

function THttpClient.Process(const Url: TUrl; const Request: string; Stream: TStream;
  BufferSize: LongInt): Boolean;
var
  Socket: TSocket;
  Temp, S: string;
  ContentLength, ContentRead: LargeInt;
  Count: LongInt;
  Buffer: Pointer;
  I: Integer;
begin
  if BufferSize < 1000 then
    BufferSize := 1000;
  Result := False;
  Socket := TSocket.Create;
  try
    Socket.Secure := Url.Secure;
    Socket.Timeout := 4000;
    if not Socket.Connect(Url.Domain, Url.Port) then
      Exit;
    if not Socket.WriteAll(Request) then
      Exit;
    Temp := '';
    repeat
      I := Socket.Read(S);
      if I < 1 then
        Exit;
      Temp := Temp + S;
    until FHeader.Extract(Temp);
    S := FHeader.Keys.Values['Content-Length'];
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
          end;
        end;
      until (FCancelled) or (Count < 1) or (ContentRead = ContentLength);
      if FCancelled then
        Result := False
      else if S <> '' then
        Result := ContentRead = ContentLength
      else
        Result := True;
    finally
      FreeMem(Buffer);
    end;
  finally
    Socket.Free;
  end;
end;

function THttpClient.SendRequest(const Url: TUrl; Verb: string; const Headers: TNamedStrings;
  Stream: TStream; BufferSize: LongInt = $1000): Boolean;
var
  Name: string;
  S: string;
begin
  Result := False;
  FHeader.Reset;
  FCancelled := False;
  if not Url.Valid then
    Exit;
  if Stream = nil then
    Exit;
  S := Verb + ' ' + Url.Resource + ' HTTP/1.0'#13#10 +
    'Host: ' + Url.Domain + #13#10;
  for Name in Headers do
    S := S + Name + ': ' + Headers.Values[Name] + #13#10;
  if UserAgent <> '' then
  	S := S + 'User-Agent: ' + UserAgent + #13#10;
  S := S + 'Connection: Close'#13#10#13#10;
  Result := Process(Url, S, Stream, BufferSize);
end;

function THttpClient.Get(const Url: TUrl; Stream: TStream;
  BufferSize: LongInt = $1000): Boolean;
var
  S: string;
begin
  Result := False;
  FHeader.Reset;
  FCancelled := False;
  S := HttpRequestGet(Url, FUserAgent);
  if S = '' then
    Exit;
  if Stream = nil then
    Exit;
  Result := Process(Url, S, Stream, BufferSize);
end;

function THttpClient.Get(const Url: TUrl; out Text: string): Boolean;
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create('');
  try
    Result := Get(Url, Stream);
    if Result then
      Text := Stream.DataString
    else
      Text := '';
  finally
    Stream.Free;
  end;
end;

function THttpClient.PostArgs(const Url: TUrl; const Args: TNamedStrings;
  Stream: TStream; BufferSize: LongInt = $1000): Boolean;
var
  S: string;
begin
  Result := False;
  FHeader.Reset;
  FCancelled := False;
  S := HttpRequestPostArgs(Url, Args, FUserAgent);
  if S = '' then
    Exit;
  if Stream = nil then
    Exit;
  Result := Process(Url, S, Stream, BufferSize);
end;

function THttpClient.PostArgs(const Url: TUrl; const Args: TNamedStrings;
  out Text: string): Boolean;
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create('');
  try
    Result := PostArgs(Url, Args, Stream);
    if Result then
      Text := Stream.DataString
    else
      Text := '';
  finally
    Stream.Free;
  end;
end;

function THttpClient.PostJson(const Url: TUrl; const Json: string;
  Stream: TStream; BufferSize: LongInt = $1000): Boolean;
var
  S: string;
begin
  Result := False;
  FHeader.Reset;
  FCancelled := False;
  S := HttpRequestPostJson(Url, Json, FUserAgent);
  if S = '' then
    Exit;
  if Stream = nil then
    Exit;
  Result := Process(Url, S, Stream, BufferSize);
end;

function THttpClient.PostJson(const Url: TUrl; const Json: string;
  out Text: string): Boolean;
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create('');
  try
    Result := PostJson(Url, Json, Stream);
    if Result then
      Text := Stream.DataString
    else
      Text := '';
  finally
    Stream.Free;
  end;
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

function HttpRequestGet(const Url: TUrl; const UserAgent: string = ''): string;
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

function HttpRequestPostArgs(const Url: TUrl; const Args: TNamedStrings; const UserAgent: string = ''): string;
var
  S, Name: string;
  I: Integer;
begin
  if not Url.Valid then
    Exit('');
  S := '';
  for I := 0 to Args.Count - 1 do
  begin
    if S <> '' then
      S := S + '&';
    Name := Args.Names[I];
    S := S + UrlEncode(Name) + '=' + UrlEncode(Args.Values[Name])
  end;
  Result :=
    'POST ' + Url.Resource + ' HTTP/1.0'#13#10 +
    'Host: ' + Url.Domain + #13#10 +
    'Content-Length: ' + IntToStr(S.Length) + #13#10 +
    'Content-Type: application/x-www-form-urlencoded'#13#10;
  if UserAgent <> '' then
  	Result := Result + 'User-Agent: ' + UserAgent + #13#10;
  Result := Result + 'Connection: Close'#13#10#13#10 + S;
end;

function HttpRequestPostJson(const Url: TUrl; const Json: string; const UserAgent: string = ''): string;
begin
  if not Url.Valid then
    Exit('');
  Result :=
    'POST ' + Url.Resource + ' HTTP/1.0'#13#10 +
    'Host: ' + Url.Domain + #13#10 +
    'Content-Length: ' + IntToStr(Json.Length) + #13#10 +
    'Content-Type: application/json'#13#10;
  if UserAgent <> '' then
  	Result := Result + 'User-Agent: ' + UserAgent + #13#10;
  Result := Result + 'Connection: Close'#13#10#13#10 + Json;
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
    if C in ['-', '_', '0'..'9', 'A'..'Z', 'a'..'z'] then
      Result := Result + C
    else
      Result := Result + '%' + IntToHex(Ord(C), 2);
  end;
end;

function WebGet(const Url: TUrl; Stream: TStream; const UserAgent: string = ''): Boolean;
var
  Request: THttpClient;
begin
  Request := THttpClient.Create;
  try
    Request.UserAgent := UserAgent;
    Result := Request.Get(Url, Stream);
  finally
    Request.Free;
  end;
end;

function WebGet(const Url: TUrl; out Text: string; const UserAgent: string = ''): Boolean;
var
  Request: THttpClient;
begin
  Request := THttpClient.Create;
  try
    Request.UserAgent := UserAgent;
    Result := Request.Get(Url, Text);
  finally
    Request.Free;
  end;
end;

end.

