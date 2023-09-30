(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified September 2023                             *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.networking.storage.txt> }
unit Codebot.Networking.Storage;

{$i codebot.inc}

interface

uses
  Classes, SysUtils, DateUtils,
  Codebot.System,
  Codebot.Text,
  Codebot.Text.Xml,
  Codebot.Cryptography,
  Codebot.Networking,
  Codebot.Networking.Web;

type

{ TS3Config is used to specify different S3 compatible providers. These providers
  include Digital Ocean Spaces, IBM Web Storage, Wasabi, Google Cloud Storage,
  Azure Blob Storage, and MinIO among others.

  To define your own S3 provider simply inherit from this class and override
  the methods to return your endpoints, default region, and access key retrieval
  methods. }

  TS3ConfigBase = class
    { The default end point or one constructed from a region

      Examples:

      s3.amazonaws.com
      s3.us-east-2.amazonaws.com
      nyc3.digitaloceanspaces.com
      s3.wasabisys.com }
    function EndPoint(const Region: string = ''): string; virtual; abstract;
    { The default region }
    function Region: string; virtual; abstract;
  end;

{ TS3Config }

  TS3Config = class(TS3ConfigBase)
  public
    { Decrypts or load an access key id }
    function AccessId: string; virtual; abstract;
    { A function which decrypts or loads a secret access key }
    function SecretKey: string; virtual; abstract;
    { Port defaults to 443 but can be anything. Changing the default port is
      useful when using S3 services such as MinIO which can run on your local
      network using any port og your choosing.

      Note: SSL/TLS is always required to send requests regardless of port. }
    function Port: Word; virtual;
  end;

{ S3Configs provides some default S3 confiurations. These configurations are
  not exhaustive. Feel free to define you own.

  All methods conform to the TS3ConfigFactory prototype and can be used in the
  contructor of a TS3Client.

  Each configuration below depends on environment variables to store your S3
  credentials. The following is a list of those environment variable names
  you must have populated to use the these configurations.

    AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY
    DO_ACCESS_KEY_ID and DO_SECRET_ACCESS_KEY
    GOOG_ACCESS_KEY_ID and GOOG_SECRET_ACCESS_KEY
    IBM_ACCESS_KEY_ID and IBM_SECRET_ACCESS_KEY
    MS_ACCESS_KEY_ID and MS_SECRET_ACCESS_KEY
    WAS_ACCESS_KEY_ID and WAS_SECRET_ACCESS_KEY

  If you prefer an alternate method for managing access keys you can simply
  define your own functions which create TS3Config instances.

  Note:

  On Linux you can set these variables in your $HOME/.profile files using
  code like this.

    export AWS_ACCESS_KEY_ID = <your access key id>
    export AWS_SECRET_ACCESS_KEY = <secret access key> }

  S3Configs = record
    class function Amazon: TS3Config; static;
    {class function DigitalOcean: TS3Config; static;
    class function Google: TS3Config; static;
    class function IBM: TS3Config; static;
    class function Microsoft: TS3Config; static;
    class function Wasabi: TS3Config; static;}
  end;

{ TS3ConfigFactory is a prototype fo functions that return TS3Config instances.
  You pass this prototype to the TS3Client constructor which will then take
  ownership of TS3Config. }

  TS3ConfigFactory = function: TS3Config;

{ TS3Request includes the end point that will accept the request and a valid
  http and authorized request header }

  TS3Request = record
    { End point that will accept the request }
    EndPoint: string;
    { Http request header }
    Header: string;
  end;

{$region async}
{ To perform async commands use an async task. You can use the task with the
  SendAsync method. When send completes either through success, failure, or
  cancellation OnComplete will notify you that the task is done. }

  IAsyncDocTask = interface(IAsyncTask)
  ['{387A116F-9D1F-400D-8CD7-31A0E1769418}']
  end;

  IAsyncStreamTask = interface(IAsyncTask)
  ['{B915D4B9-4A84-400D-AF35-6E2CCE4B30CD}']
  end;

{ TNotifyComplete is used to notify of the result of an async task }

  TNotifyDocComplete = procedure(Task: IAsyncTask; Result: IDocument) of object;
  TNotifyStreamComplete = procedure(Task: IAsyncTask; Result: TStream) of object;

{ NewDocTask creates an async task with an xml document result. You may optionally
  supply the task with user data. If owns object is true then user data will be
  destroyed when the task is destroyed. }

function NewDocTask(OnComplete: TNotifyDocComplete; Data: TObject = nil; OwnsObject: Boolean = False): IAsyncDocTask;

{ NewStreamTask creates an async task using a stream object. The steam will be
  returned in the result of the completion event. The same rules apply to data
  as described above with the addition that you are responsible for managing the
  stream after completion. }

function NewStreamTask(OnComplete: TNotifyStreamComplete; Data: TObject = nil; OwnsObject: Boolean = False): IAsyncStreamTask;
{$endregion}

type

{ TS3Methods is used to generate S3 REST requests. It builds HTTP request headers
  using an S3 compatible configuration providers. There are many S3 compatible
  services and they each have their own end point and regions. When you sign
  up with any S3 service they will provide you with access keys.

  The headers generated by this class use the authorization version 4 signature
  scheme to verify you are the authorized user for their service account.

  Reference:

  https://docs.aws.amazon.com/AmazonS3/latest/API/sig-v4-header-based-auth.html

  Note:

  You will need either set to valid access key environment variables for your
  preferred S3 compatible service or define your own TS3ConfigFactory where
  you can implement your own TS3KeyStore functions for retrieving access keys.

  Bucket note:

  Some requests require both a bucket and a region. If no region is given then
  the default region for the configured S3 service will be used. This may
  cause the request to be denied, so you might want to first retrieve and then
  cache the bucket region using the GetBucketLocation request.

  Example usage:

  S3 := TS3Methods.Create(S3Config.Wasabi);
  ...
  Request := S3.ListBuckets;
  if S3.Send(Request, Document) then
    WriteLn(Document.Text); }

  TS3Methods = class
  private
    FConfig: TS3Config;
    FBuckets: TStrings;
    function GetConfig: TS3ConfigBase;
    function QueryBucket(const Bucket, Query: string): TS3Request;
  public
    constructor Create(Config: TS3ConfigFactory);
    destructor Destroy; override;
    { Reconfigure replaces the current config and erases bucket region memory }
    procedure Reconfigure(Config: TS3ConfigFactory);
    { Find a bucket's region directly and cache it in memory. If a bucket
      region is unknown this method will block while a query is dispatched. }
    function FindRegion(const Bucket: string): string;
    { Add a bucket's region to the memory cache manually }
    procedure AddRegion(const Bucket, Region: string);
    {$region requests}
    { Request to list all buckets }
    function ListBuckets: TS3Request;
    { Request to get bucket region. Due to legacy constraints a blank value
      returned from from this request should be interrupted as us-east-1. }
    function GetBucketLocation(const Bucket: string): TS3Request;
    { Request to get bucket access control list }
    function GetBucketAcl(const Bucket: string): TS3Request;
    { Request to get bucket CORS }
    function GetBucketCors(const Bucket: string): TS3Request;
    { Request to get bucket policy }
    function GetBucketPolicy(const Bucket: string): TS3Request;
    { Request to get bucket request payment }
    function GetBucketRequestPayment(const Bucket: string): TS3Request;
    { Request to get bucket tagging }
    function GetBucketTagging(const Bucket: string): TS3Request;
    { Request to get bucket versioning }
    function GetBucketVersioning(const Bucket: string): TS3Request;
    { Request to get bucket website }
    function GetBucketWebsite(const Bucket: string): TS3Request;
    { Request to list bucket objects }
    function ListObjects(const Bucket: string; NextToken: string = '';
      Prefix: string = ''; Delimiter: string = ''): TS3Request;
    {$endregion}
    {$region send}
    { Send a request to your S3 servers outputting the response to a XML
      document. Returns true if the response code is 200 OK.

      First overload can be used when querying or sending commands to S3
      Second overload can be used when receving files from S3 }
    function Send(const Request: TS3Request; out Response: IDocument): Boolean; overload;
    function Send(const Request: TS3Request; Stream: TStream): Boolean; overload;
    { SendAsync is identical to the method Send above but performed
      asynchronously.

      When sending an async request the task status will be success if the
      response code is 200 OK. The completion notification will receive either
      a response document or a stream as the result argument.

      Failure to provide a completion notification will cause then response
      document or stream to be discarded. When using the stream variant you
      might want to free the stream upon completion.

      Example usage:

      procedure ListingComplete(Task: IAsyncTask; Result: IDocument);
      begin
        if Task.Status = asyncSuccess then
          WriteLn(Result.Xml);
      end;

      Request := S3.ListObjects(Bucket);
      Task := NewDocTask(ListingComplete);
      S3.SendAsync(Request, Task); }
    procedure SendAsync(const Request: TS3Request; Task: IAsyncDocTask); overload;
    procedure SendAsync(const Request: TS3Request; Stream: TStream; Task: IAsyncStreamTask); overload;
    {$endregion}
    property Config: TS3ConfigBase read GetConfig;
  end;

implementation

uses
  Codebot.Support;

{
function DOPub: string; begin Result := GetEnvironmentVariable('DO_ACCESS_KEY_ID'); end;
function DOPriv: string; begin Result := GetEnvironmentVariable('DO_ACCESS_KEY'); end;

class function S3Configs.DigitalOcean: TS3Config;
begin
  Result := TS3Config.Create('nyc3.digitaloceanspaces.com', 'nyc3', DOPub, DOPriv);
end;

function GOOGPub: string; begin Result := GetEnvironmentVariable('GOOG_ACCESS_KEY_ID'); end;
function GOOGPriv: string; begin Result := GetEnvironmentVariable('GOOG_SECRET_ACCESS_KEY'); end;

class function S3Configs.Google: TS3Config;
begin
  Result := TS3Config.Create('storage.googleapis.com', 'us-east-1', GOOGPub, GOOGPriv);
end;

function IBMPub: string; begin Result := GetEnvironmentVariable('IBM_ACCESS_KEY_ID'); end;
function IBMPriv: string; begin Result := GetEnvironmentVariable('IBM_SECRET_ACCESS_KEY'); end;

class function S3Configs.IBM: TS3Config;
begin
  Result := TS3Config.Create('cloud-object-storage.appdomain.cloud', 'us-east', IBMPub, IBMPriv);
end;

function MSPub: string; begin Result := GetEnvironmentVariable('MS_ACCESS_KEY_ID'); end;
function MSPriv: string; begin Result := GetEnvironmentVariable('MS_SECRET_ACCESS_KEY'); end;

class function S3Configs.Microsoft: TS3Config;
begin
  Result := TS3Config.Create('blob.core.windows.net', 'unknown', MSPub, MSPriv);
end;

function WASPub: string; begin Result := GetEnvironmentVariable('WAS_ACCESS_KEY_ID'); end;
function WASPriv: string; begin Result := GetEnvironmentVariable('WAS_SECRET_ACCESS_KEY'); end;

class function S3Configs.Wasabi: TS3Config;
begin
  Result := TS3Config.Create('s3.wasabisys.com', 'us-east-1', WASPub, WASPriv);
end;}

{ THeaderPair }

type
  THeaderPair = class
    Name: string;
    Value: string;
  end;

{ THeaderPairs }

  THeaderPairs = class
  private
    FList: TList;
    FSorted: Boolean;
    function GetCount: Integer;
    function GetItem(Index: Integer): THeaderPair;
    procedure Sort;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Add(const Name, Value: string): THeaderPairs;
    function Names: string;
    property Items[Index: Integer]: THeaderPair read GetItem; default;
    property Count: Integer read GetCount;
  end;

{ THeaderPairs }

constructor THeaderPairs.Create;
begin
  inherited Create;
  FList := TList.Create;
  FSorted := True;
end;

destructor THeaderPairs.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

procedure THeaderPairs.Clear;
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
    TObject(FList[I]).Free;
  FList.Clear;
  FSorted := True;
end;

function HeaderCompare(Item1, Item2: Pointer): Integer;
var
  A: THeaderPair absolute Item1;
  B: THeaderPair absolute Item2;
begin
  Result := StrCompare(A.Name, B.Name, True);
end;

procedure THeaderPairs.Sort;
begin
  if FSorted then
    Exit;
  FSorted := True;
  if FList.Count > 1 then
    FList.Sort(HeaderCompare);
end;

function THeaderPairs.Add(const Name, Value: string): THeaderPairs;
var
  Item: THeaderPair;
begin
  Item := THeaderPair.Create;
  Item.Name := Trim(Name);
  Item.Value := Trim(Value);
  FList.Add(Item);
  FSorted := FList.Count < 2;
  Result := Self;
end;

function THeaderPairs.Names: string;
var
  I: Integer;
begin
  Result := '';
  Sort;
  if FList.Count < 1 then
    Exit;
  Result := LowerCase(THeaderPair(FList[0]).Name);
  for I := 1 to FList.Count - 1 do
    Result := Result + ';' + LowerCase(THeaderPair(FList[I]).Name)
end;

function THeaderPairs.GetCount: Integer;
begin
  Result := FList.Count;
end;

function THeaderPairs.GetItem(Index: Integer): THeaderPair;
begin
  Sort;
  Result := THeaderPair(FList[Index]);
end;

{ GenerateRequest does all the work of generating a valid AWS S3 request with
  proper version 4 authentication. }

const
  NoConnect = 'Could not connect';
  NoContent = 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855';
  Service = 's3';
  Version = 'aws4_request';

function NowUTC: TDateTime;
begin
  Result := LocalTimeToUniversal(Now);
end;

function NewHeaders(Name: string = ''; Value: string = ''): THeaderPairs;
begin
  Result := THeaderPairs.Create;
  if Name <> '' then
    if Value <> '' then
      Result.Add(Name, Value);
end;

function UriEncode(const S: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(S) do
    if S[I] in ['A'..'Z', 'a'..'z', '0'..'9', '-', '_', '.', '~'] then
      Result := Result + S[I]
    else
      Result := Result + '%' + IntToHex(Ord(S[I]), 2);
end;

{ Note: Query parameters should be UriEncode'd before they are sent to GenerateRequest }

function QueryEncode(Query: string): string;
var
  List: TStringList;
  S: string;
  I: Integer;
begin
  Result := '';
  if Query = '' then
    Exit;
  List := TStringList.Create;
  try
    for S in Query.Split('&') do
      List.Add(S);
    List.Sorted := True;
    S := List[0];
    Result := S.FirstOf('=') + '=' + S.SecondOf('=');
    for I := 1 to List.Count - 1 do
    begin
      S := List[I];
      Result := Result + '&' + S.FirstOf('=') + '=' + S.SecondOf('=');
    end;
  finally
    List.Free;
  end;
end;

function GenerateRequest(Config: TS3Config; const Region, Verb: string; Url: TUrl; Headers: THeaderPairs = nil): string;
var
  Date: TDateTime;
  DateShort: string;
  DateLong: string;
  Resource: string;
  Query: string;
  Request: string;
  CanonicalRequest: string;
  StringToSign: string;
  Signature: string;
  I: Integer;
begin
  if Headers = nil then
    Headers := NewHeaders;
  try
    Date := NowUTC;
    DateShort := FormatDateTime('yyyymmdd', Date);
    DateLong := DateShort + 'T' + FormatDateTime('hhnnss', Date) + 'Z';
    Resource := Url.Resource.FirstOf('?');
    Query := QueryEncode(Url.Resource.SecondOf('?'));
    Headers
      .Add('Host', Url.Domain)
      .Add('X-Amz-Content-Sha256', NoContent)
      .Add('X-Amz-Date', DateLong);
    CanonicalRequest :=
      Verb + #10 +
      Resource + #10 +
      Query +  #10;
    for I := 0 to Headers.Count - 1 do
      with Headers[I] do
        CanonicalRequest := CanonicalRequest + LowerCase(Name) + ':' + Value + #10;
    CanonicalRequest := CanonicalRequest + #10 +
      Headers.Names + #10 +
      NoContent;
    StringToSign :=
      'AWS4-HMAC-SHA256'#10 +
      DateLong + #10 +
      DateShort +
      '/' + Region +
      '/' + Service +
      '/' + Version + #10 +
      HashString(hashSHA256, CanonicalRequest).AsHex;
    Signature :=
      AuthString('AWS4' + Config.SecretKey, hashSHA256, DateShort)
      .AuthNext(hashSHA256, Region)
      .AuthNext(hashSHA256, Service)
      .AuthNext(hashSHA256, Version)
      .AuthNext(hashSHA256, StringToSign)
      .AsHex;
    Request :=
      Verb + ' ' + Url.Resource + ' HTTP/1.1'#10;
    for I := 0 to Headers.Count - 1 do
      with Headers[I] do
        Request := Request + Name + ': ' + Value + #10;
    Request := Request + 'Authorization: ' +
      'AWS4-HMAC-SHA256 Credential=' + Config.AccessId +
      '/' + DateShort +
      '/' + Region +
      '/' + Service +
      '/' + Version +
      ', SignedHeaders=' + Headers.Names +
      ', Signature=' + Signature + #10 +
      'Connection: Close'#10 +
        #10;
    Result := Request;
  finally
    Headers.Free;
  end;
end;

{ TS3Methods }

constructor TS3Methods.Create(Config: TS3ConfigFactory);
begin
  inherited Create;
  FConfig := Config;
  FBuckets := TStringList.Create;
end;

destructor TS3Methods.Destroy;
begin
  FBuckets.Free;
  FConfig.Free;
  inherited Destroy;
end;

function TS3Methods.GetConfig: TS3ConfigBase;
begin
  Result := FConfig;
end;

procedure TS3Methods.Reconfigure(Config: TS3ConfigFactory);
begin
  FBuckets.Free;
  FConfig.Free;
  FConfig := Config;
  FBuckets := TStringList.Create;
end;

{$region bucket read}
function TS3Methods.QueryBucket(const Bucket, Query: string): TS3Request;
var
  Region: string;
begin
  if Query = 'location' then
    Region := FConfig.Region
  else
    Region := FindRegion(Bucket);
  Result.EndPoint := FConfig.EndPoint(Region);
  Result.Header := GenerateRequest(FConfig, Region, 'GET', FConfig.EndPoint(Region) +
    '/' + Bucket +
    '?' + Query);
end;

function TS3Methods.FindRegion(const Bucket: string): string;
var
  Request: TS3Request;
  D: IDocument;
  I: Integer;
begin
  Result := '';
  I := FBuckets.IndexOf(UpperCase(Bucket));
  if I < 0 then
  begin
    Request.EndPoint := FConfig.EndPoint;
    Request.Header := GenerateRequest(FConfig, FConfig.Region, 'GET',
      FConfig.EndPoint + '/' + Bucket + '?location');
    if Send(Request, D) then
    begin
      Result := D.Root.Text;
      if Result = '' then
        Result := FConfig.Region;
      FBuckets.Add(UpperCase(Bucket));
      FBuckets.Add(Result);
    end;
  end
  else
    Result := FBuckets[I + 1];
  if Result = '' then
    Result := FConfig.Region;
end;

procedure TS3Methods.AddRegion(const Bucket, Region: string);
var
  I: Integer;
begin
  I := FBuckets.IndexOf(UpperCase(Bucket));
  if I < 0 then
  begin
    FBuckets.Add(UpperCase(Bucket));
    if Region <> '' then
      FBuckets.Add(Region)
    else
      FBuckets.Add(FConfig.Region);
  end;
end;

function TS3Methods.ListBuckets: TS3Request;
begin
  Result.EndPoint := FConfig.EndPoint;
  Result.Header := GenerateRequest(FConfig, FConfig.Region, 'GET', Result.EndPoint);
end;

function TS3Methods.GetBucketLocation(const Bucket: string): TS3Request;
begin
  Result := QueryBucket(Bucket, 'location');
end;

function TS3Methods.GetBucketAcl(const Bucket: string): TS3Request;
begin
  Result := QueryBucket(Bucket, 'acl');
end;

function TS3Methods.GetBucketCors(const Bucket: string): TS3Request;
begin
  Result := QueryBucket(Bucket, 'cors');
end;

function TS3Methods.GetBucketPolicy(const Bucket: string): TS3Request;
begin
  Result := QueryBucket(Bucket, 'policy');
end;

function TS3Methods.GetBucketRequestPayment(const Bucket: string): TS3Request;
begin
  Result := QueryBucket(Bucket, 'requestPayment');
end;

function TS3Methods.GetBucketTagging(const Bucket: string): TS3Request;
begin
  Result := QueryBucket(Bucket, 'tagging');
end;

function TS3Methods.GetBucketVersioning(const Bucket: string): TS3Request;
begin
  Result := QueryBucket(Bucket, 'versioning');
end;

function TS3Methods.GetBucketWebsite(const Bucket: string): TS3Request;
begin
  Result := QueryBucket(Bucket, 'website');
end;

function TS3Methods.ListObjects(const Bucket: string; NextToken: string = '';
  Prefix: string = ''; Delimiter: string = ''): TS3Request;
var
  Query: string;
begin
  Query := 'list-type=2&encoding-type=url';
  if Delimiter <> '' then
    Query := Query + '&delimiter=' + UriEncode(Delimiter);
  if Prefix <> '' then
    Query := Query + '&prefix=' + UriEncode(Prefix);
  if NextToken <> '' then
    Query := Query + '&continuation-token=' + UriEncode(NextToken);
  Result := QueryBucket(Bucket, Query);
end;
{$endregion}

{$region send}
function InternalSend(const Request: TS3Request; Port: Word; Stream: TStream; Task: IAsyncDocTask = nil): Boolean;

  procedure DoProgress(Delta: Int64);
  begin
    if (Delta > 0) and (Task <> nil) then
      (Task as IAsyncRunnerBase).Tick(Delta);
  end;

  function IsCancelled: Boolean;
  begin
    Result := (Task <> nil) and Task.Cancelled;
  end;

  function ReadChunk(S: TSocket; Chunk: string): string;
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
  Header: THttpResponseHeader;
  Buffer, Data, Encoding, Chunk: string;
  Socket: TSocket;
  C: Char;
begin
  Result := False;
  Header.Clear;
  Buffer := '';
  Socket := TSocket.Create;
  try
    Socket.Secure := True;
    if Socket.Connect(Request.EndPoint, Port) then
    begin
      if IsCancelled then
        Exit;
      Socket.Write(Request.Header);
      while Socket.Read(Data) > 0 do
      begin
        Buffer := Buffer + Data;
        if Header.Extract(Buffer) then
        begin
          if Buffer <> '' then
            raise Exception.Create('Chunked header buffer errror');
          Result := Header.Code = 200;
          Encoding := Header.Keys.Values['Transfer-Encoding'];
          if StrContains(Encoding, 'chunked', True) then
          repeat
            Chunk := '';
            if IsCancelled then
              Exit;
            while Socket.Read({%H-}C, 1) = 1 do
            begin
              if IsCancelled then
                Exit;
              if (C = #10) and (Chunk <> '') then
              begin
                Data := ReadChunk(Socket, Chunk);
                if IsCancelled then
                  Exit;
                Chunk := '';
                if Length(Data) < 1 then
                  Break;
                Stream.Write(Pointer(Data)^, Length(Data));
              end
              else if C > ' ' then
                Chunk := Chunk + C;
            end;
          until Chunk = ''
          else while Socket.Read(Data) > 0 do
          begin
            if IsCancelled then
              Exit;
            DoProgress(Length(Data));
            Stream.Write(Pointer(Data)^, Length(Data));
          end;
          Break;
        end;
      end;
    end
    else
      Buffer := NoConnect;
  finally
    Socket.Free;
    if IsCancelled then
      Result := False;
  end;
end;

procedure InternalDoc(Success: Boolean; Stream: TStringStream; out Response: IDocument);
const
  Xmlns = ' xmlns="http://s3.amazonaws.com/doc/2006-03-01/"';
var
  S: string;
begin
  Response := NewDocument;
  if Success then
  begin
    S := Stream.DataString;
    if S.BeginsWith('<?xml') then
      Response.Xml := S.ReplaceOne(Xmlns, '');
  end;
end;

function TS3Methods.Send(const Request: TS3Request; out Response: IDocument): Boolean;
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create;
  try
    Result := InternalSend(Request, FConfig.Port, Stream, nil);
    InternalDoc(Result, Stream, Response);
  finally
    Stream.Free;
  end;
end;

function TS3Methods.Send(const Request: TS3Request; Stream: TStream): Boolean;
begin
  Result := InternalSend(Request, FConfig.Port, Stream, nil);
end;

{ Async support }

type
  TAsyncParams<T> = record
    Request: TS3Request;
    Port: Word;
    Success: Boolean;
    Result: T;
  end;

  TAsyncDocParams = TAsyncParams<IDocument>;
  TAsyncStreamParams = TAsyncParams<TStream>;

  TAsyncDocTask = class(TAsyncTaskRunner<IDocument>, IAsyncDocTask) end;
  TAsyncStreamTask = class(TAsyncTaskRunner<TStream>, IAsyncStreamTask) end;

function NewDocTask(OnComplete: TNotifyDocComplete; Data: TObject = nil; OwnsObject: Boolean = False): IAsyncDocTask;
begin
  Result := TAsyncDocTask.Create(OnComplete, Data, OwnsObject);
end;

function NewStreamTask(OnComplete: TNotifyStreamComplete; Data: TObject = nil; OwnsObject: Boolean = False): IAsyncStreamTask;
begin
  Result := TAsyncStreamTask.Create(OnComplete, Data, OwnsObject);
end;

procedure DocExecute(var Params: TAsyncDocParams; Task: IAsyncTask);
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create;
  try
    Params.Success := InternalSend(Params.Request, Params.Port, Stream, Task as IAsyncDocTask);
    InternalDoc(Params.Success, Stream, Params.Result);
  finally
    Stream.Free;
  end;
end;

procedure DocComplete(var Params: TAsyncDocParams; Task: IAsyncTask);
var
  Runner: IAsyncRunner<IDocument>;
begin
  Runner := Task as IAsyncRunner<IDocument>;
  Runner.Notify(BoolAsync[Params.Success], Params.Result);
end;

procedure TS3Methods.SendAsync(const Request: TS3Request; Task: IAsyncDocTask);
var
  Params: TAsyncDocParams;
begin
  Params.Request := Request;
  Params.Port := FConfig.Port;
  TThreadRunner<TAsyncDocParams>.Create(Params, Task, DocExecute, DocComplete);
end;

procedure StreamExecute(var Params: TAsyncStreamParams; Task: IAsyncTask);
begin
  Params.Success := InternalSend(Params.Request, Params.Port, Params.Result, Task as IAsyncDocTask);
end;

procedure StreamComplete(var Params: TAsyncStreamParams; Task: IAsyncTask);
var
  Runner: IAsyncRunner<TStream>;
begin
  Runner := Task as IAsyncRunner<TStream>;
  Runner.Notify(BoolAsync[Params.Success], Params.Result);
end;

procedure TS3Methods.SendAsync(const Request: TS3Request; Stream: TStream; Task: IAsyncStreamTask);
var
  Params: TAsyncStreamParams;
begin
  Params.Request := Request;
  Params.Port := FConfig.Port;
  Params.Result := Stream;
  TThreadRunner<TAsyncStreamParams>.Create(Params, Task, StreamExecute, StreamComplete);
end;
{$endregion}

{ TS3Config }

function TS3Config.Port: Word;
begin
  Result := 443;
end;

{ TS3AmazonConfig }

type
  TS3AmazonConfig = class(TS3Config)
  public
    function EndPoint(const Region: string = ''): string; override;
    function Region: string; override;
    function AccessId: string; override;
    function SecretKey: string; override;
  end;

const
  DefAmazonRegion = 'us-east-1';
  DefAmazonDomain = 'amazonaws.com';
  DefAmazonEndPoint = 's3.' + DefAmazonDomain;

function TS3AmazonConfig.EndPoint(const Region: string): string;
begin
    if Region = '' then
      Result := DefAmazonEndPoint
    else if Region <> DefAmazonRegion then
      Result := 's3.' + Region + '.' + DefAmazonDomain
    else
      Result := DefAmazonEndPoint;
end;

function TS3AmazonConfig.Region: string;
begin
  Result := DefAmazonRegion;
end;

function TS3AmazonConfig.AccessId: string;
begin
  Result := GetEnvironmentVariable('AWS_ACCESS_KEY_ID');
end;

function TS3AmazonConfig.SecretKey: string;
begin
  Result := GetEnvironmentVariable('AWS_SECRET_ACCESS_KEY');
end;

{ S3Configs }

class function S3Configs.Amazon: TS3Config;
begin
  Result := TS3AmazonConfig.Create;
end;

end.
