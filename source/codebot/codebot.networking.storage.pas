(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2023                               *)
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
  Codebot.Text.Json,
  Codebot.Text.Xml,
  Codebot.Cryptography,
  Codebot.Networking,
  Codebot.Networking.Web;

{ ES3ConfigError is raised when an S3 provider name is not recognized }

type
  ES3ConfigError = class(Exception);

{ TS3Config holds the end points, regions, and credentials of an S3 compatible
  provider. Credentials are read from the environment variables named by the
  provider unless AccessId and SecretKey are set directly. }

  TS3Config = class
  private
    FNode: TJsonNode;
    FProvider: string;
    FHost: string;
    FDefaultHost: string;
    FPort: Word;
    FService: string;
    FDefaultRegion: string;
    FRegionVar: string;
    FAccessIdVar: string;
    FAccessId: string;
    FSecretKeyVar: string;
    FSecretKey: string;
    function GetAccessId: string;
    function GetSecretKey: string;
  public
    { Create a config for a named provider raising ES3ConfigError if the
      provider is not known }
    constructor Create(const AProvider: string);
    destructor Destroy; override;
    { The name of the provider }
    function Provider: string;
    { The host name for a region or the default host if region is empty.
      Providers with a single global host always return the default host. }
    function EndPoint(const Region: string = ''): string;
    { The port used to connect to the provider }
    function Port: Word;
    { The region used when none is given. Some providers name an environment
      variable which overrides this region. }
    function DefaultRegion: string;
    { Convert a GetBucketLocation result to a region. A blank location is the
      default region and the legacy location EU is eu-west-1. A location such
      as us-south-standard which begins with a known region becomes that
      region. Providers with a single global host always use the default
      region. }
    function LocationRegion(const Location: string): string;
    { Output the names and values of regions supported by the provider }
    procedure ListRegions(out Regions: TNamedStrings);
    { The access key id, read from the environment if not set }
    property AccessId: string read GetAccessId write FAccessId;
    { The secret access key, read from the environment if not set }
    property SecretKey: string read GetSecretKey write FSecretKey;
  end;

{ Built in providers

  TS3Config.Create accepts the provider names below. Unless AccessId and
  SecretKey are set directly, each provider reads its credentials from the
  environment variables listed with it.

    amazon        AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY
    wasabi        WAS_ACCESS_KEY_ID and WAS_SECRET_ACCESS_KEY
    digitalocean  DO_ACCESS_KEY_ID and DO_SECRET_ACCESS_KEY
    google        GOOG_ACCESS_KEY_ID and GOOG_SECRET_ACCESS_KEY
    ibm           IBM_ACCESS_KEY_ID and IBM_SECRET_ACCESS_KEY
    scaleway      SCW_ACCESS_KEY and SCW_SECRET_KEY
    ovh           OVH_ACCESS_KEY_ID and OVH_SECRET_ACCESS_KEY
    hetzner       HETZNER_ACCESS_KEY_ID and HETZNER_SECRET_ACCESS_KEY

  These providers support per object acls, so objects can be uploaded as
  private or public. Acl support on hetzner is not confirmed. Google uses
  HMAC keys and a single global host.

  Note:

  On Linux you can set these variables in your $HOME/.profile files using
  code like this.

    export AWS_ACCESS_KEY_ID=<your access key id>
    export AWS_SECRET_ACCESS_KEY=<secret access key> }

{ TS3Request includes the end point that will accept the request and a valid
  http and authorized request header }

  TS3Request = record
    { End point that will accept the request }
    EndPoint: string;
    { Http request header }
    Header: string;
  end;

{ TS3Access chooses who can read an object.

  accessDefault sends no acl, so the object follows the bucket's own rules.
  This works with every provider and is the safest choice.

  accessPrivate and accessPublic send the canned acl private or public-read.
  The built in providers support them, but Amazon buckets with acls disabled
  and Google buckets with uniform access reject them. A rejected acl fails
  the request with an error such as AccessControlListNotSupported. }

  TS3Access = (accessDefault, accessPrivate, accessPublic);

{ S3UrlDecode decodes the keys and prefixes in a ListObjects response. Those
  requests ask for url encoding so that any key can be represented in xml. }

function S3UrlDecode(const S: string): string;

{ S3ResponseHeader returns the value of a response header from a document
  returned by Send or SendAsync, or an empty string if the header is missing.
  Headers are kept when a response has no xml body, as with HeadObject.
  Names are not case sensitive, for example Content-Length or x-amz-meta-x. }

function S3ResponseHeader(Response: IDocument; const Name: string): string;

type

{$region async}
{ To perform async commands use an async task. You can use the task with the
  SendAsync method. When send completes either through success, failure, or
  cancellation OnComplete will notify you that the task is done. }

{ IAsyncDocTask is an async task which results in an xml document }

  IAsyncDocTask = interface(IAsyncTask)
  ['{387A116F-9D1F-400D-8CD7-31A0E1769418}']
  end;

{ IAsyncStreamTask is an async task which results in a stream }

  IAsyncStreamTask = interface(IAsyncTask)
  ['{B915D4B9-4A84-400D-AF35-6E2CCE4B30CD}']
  end;

{ TNotifyDocComplete and TNotifyStreamComplete are used to notify of the
  result of an async task }

  TNotifyDocComplete = procedure(Task: IAsyncTask; Result: IDocument) of object;
  TNotifyStreamComplete = procedure(Task: IAsyncTask; Result: TStream) of object;

{ NewDocTask creates an async task with an xml document result. You may optionally
  supply the task with user data. If owns object is true then user data will be
  destroyed when the task is destroyed. }

function NewDocTask(OnComplete: TNotifyDocComplete; Data: TObject = nil; OwnsObject: Boolean = False): IAsyncDocTask;

{ NewStreamTask creates an async task using a stream object. The stream will be
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

  You will need to either set valid access key environment variables for your
  preferred S3 compatible service or set Config.AccessId and Config.SecretKey
  directly.

  Bucket note:

  Some requests require both a bucket and a region. If no region is given then
  the default region for the configured S3 service will be used. This may
  cause the request to be denied, so you might want to first retrieve and then
  cache the bucket region using the GetBucketLocation request.

  Example usage:

  S3 := TS3Methods.Create('wasabi');
  ...
  Request := S3.ListBuckets;
  if S3.Send(Request, Document) then
    WriteLn(Document.Text); }

  TS3Methods = class
  private
    FConfig: TS3Config;
    FBuckets: TStrings;
    function GetConfig: TS3Config;
    function QueryBucket(const Bucket, Query: string): TS3Request;
  public
    { Create S3 methods using a named provider configuration }
    constructor Create(const AProvider: string);
    destructor Destroy; override;
    { Reconfigure replaces the current config and erases bucket region memory }
    procedure Reconfigure(const AProvider: string);
    { Find a bucket's region directly and cache it in memory. If a bucket
      region is unknown this method will block while a query is dispatched. }
    function FindRegion(const Bucket: string): string;
    { Add a bucket's region to the memory cache manually }
    procedure AddRegion(const Bucket, Region: string);
    {$region requests}
    { Request to list all buckets }
    function ListBuckets: TS3Request;
    { Request to get bucket region. Pass the result to Config.LocationRegion
      to convert it to a region. }
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
    {$region objects}
    { Request to download an object. Send the request with Send or SendAsync
      along with a stream to receive the object's contents. }
    function GetObject(const Bucket, Key: string): TS3Request;
    { Request an object's details without its contents. The response has no
      body, so read the details with S3ResponseHeader, for example
      Content-Length, Content-Type, Last-Modified, ETag, x-amz-storage-class,
      and user metadata named x-amz-meta-*. }
    function HeadObject(const Bucket, Key: string): TS3Request;
    { Request to upload an object of a given size. Send the request with
      SendObject or SendObjectAsync along with a stream of the object's
      contents. The access argument can upload the object as private or
      public. See TS3Access for which providers accept this. }
    function PutObject(const Bucket, Key: string; Size: Int64;
      const ContentType: string = ''; Access: TS3Access = accessDefault): TS3Request;
    { Request to delete an object }
    function DeleteObject(const Bucket, Key: string): TS3Request;
    { Request to get an object access control list }
    function GetObjectAcl(const Bucket, Key: string): TS3Request;
    { Request to make an existing object private or public. The value
      accessDefault is treated as accessPrivate. }
    function PutObjectAcl(const Bucket, Key: string; Access: TS3Access): TS3Request;
    {$endregion}
    {$region send}
    { Generate a presigned url for public access to an object }
    function Presign(const Bucket, Path: string; Expires: Integer = 0): string;
    { Send a request to your S3 servers outputting the response to a XML
      document. Returns true if the status code is a 2xx success code.

      First overload can be used when querying or sending commands to S3
      Second overload can be used when receiving files from S3 }
    function Send(const Request: TS3Request; out Response: IDocument): Boolean; overload;
    function Send(const Request: TS3Request; Stream: TStream): Boolean; overload;
    { SendAsync is identical to the method Send above but performed
      asynchronously.

      When sending an async request the task status will be success if the
      response status is 200 OK. The completion notification will receive either
      a response document or a stream as the result argument.

      Failure to provide a completion notification will cause the response
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
    { SendObject sends a PutObject request followed by the object contents read
      from body. The body is read from its current position and is not freed.
      SendObjectAsync is the same but performed asynchronously, and the body
      must remain valid until the task completes. }
    function SendObject(const Request: TS3Request; Body: TStream; out Response: IDocument): Boolean;
    procedure SendObjectAsync(const Request: TS3Request; Body: TStream; Task: IAsyncDocTask);
    {$endregion}
    { The current provider configuration }
    property Config: TS3Config read GetConfig;
  end;

implementation

uses
  Codebot.Support;

const
  DefPort = 443;

{ Built in list of S3 providers. Each provider gives:

    host           Region end points are [service.]region.host. When host is
                   empty the provider has a single global end point.
    defaultHost    End point used when no region is given. When empty the end
                   point of the default region is used.
    service        Optional prefix of region end points
    accessId       Environment variable holding the access key id
    secretKey      Environment variable holding the secret access key
    defaultRegion  Region used when none is given
    regionVar      Optional environment variable which overrides defaultRegion
    port           Optional port, 443 if not given
    regions        Names and values of the regions

  Non-ascii characters are written as json unicode escapes. Their backslash
  is concatenated separately to keep this source plain ascii. }

const
  ServiceList =
    '{' +
    '"amazon": {' +
      '"host": "amazonaws.com",' +
      '"defaultHost": "s3.amazonaws.com",' +
      '"service": "s3",' +
      '"accessId": "AWS_ACCESS_KEY_ID",' +
      '"secretKey": "AWS_SECRET_ACCESS_KEY",' +
      '"defaultRegion": "us-east-1",' +
      '"regions": [' +
        '{"name": "US East (N. Virginia)", "value": "us-east-1"},' +
        '{"name": "US East (Ohio)", "value": "us-east-2"},' +
        '{"name": "US West (N. California)", "value": "us-west-1"},' +
        '{"name": "US West (Oregon)", "value": "us-west-2"},' +
        '{"name": "Africa (Cape Town)", "value": "af-south-1"},' +
        '{"name": "Asia Pacific (Mumbai)", "value": "ap-south-1"},' +
        '{"name": "Asia Pacific (Hyderabad)", "value": "ap-south-2"},' +
        '{"name": "Asia Pacific (Singapore)", "value": "ap-southeast-1"},' +
        '{"name": "Asia Pacific (Jakarta)", "value": "ap-southeast-3"},' +
        '{"name": "Asia Pacific (Melbourne)", "value": "ap-southeast-4"},' +
        '{"name": "Asia Pacific (Tokyo)", "value": "ap-northeast-1"},' +
        '{"name": "Asia Pacific (Sydney)", "value": "ap-southeast-2"},' +
        '{"name": "Asia Pacific (Seoul)", "value": "ap-northeast-2"},' +
        '{"name": "Asia Pacific (Osaka)", "value": "ap-northeast-3"},' +
        '{"name": "Canada (Central)", "value": "ca-central-1"},' +
        '{"name": "China (Beijing)", "value": "cn-north-1"},' +
        '{"name": "China (Ningxia)", "value": "cn-northwest-1"},' +
        '{"name": "Europe (Frankfurt)", "value": "eu-central-1"},' +
        '{"name": "Europe (Ireland)", "value": "eu-west-1"},' +
        '{"name": "Europe (London)", "value": "eu-west-2"},' +
        '{"name": "Europe (Paris)", "value": "eu-west-3"},' +
        '{"name": "Europe (Milan)", "value": "eu-south-1"},' +
        '{"name": "Europe (Spain)", "value": "eu-south-2"},' +
        '{"name": "Europe (Zurich)", "value": "eu-central-2"},' +
        '{"name": "Europe (Stockholm)", "value": "eu-north-1"},' +
        '{"name": "South America (S' + '\' + 'u00e3o Paulo)", "value": "sa-east-1"},' +
        '{"name": "Middle East (Bahrain)", "value": "me-south-1"},' +
        '{"name": "Middle East (UAE)", "value": "me-central-1"},' +
        '{"name": "Israel (Tel Aviv)", "value": "il-central-1"}' +
      ']' +
    '},' +
    '"wasabi": {' +
      '"host": "wasabisys.com",' +
      '"defaultHost": "s3.wasabisys.com",' +
      '"service": "s3",' +
      '"accessId": "WAS_ACCESS_KEY_ID",' +
      '"secretKey": "WAS_SECRET_ACCESS_KEY",' +
      '"defaultRegion": "us-east-1",' +
      '"regions": [' +
        '{"name": "US East 1 (N. Virginia)", "value": "us-east-1"},' +
        '{"name": "US East 2 (N. Virginia)", "value": "us-east-2"},' +
        '{"name": "US Central 1 (Texas)", "value": "us-central-1"},' +
        '{"name": "US West 1 (Oregon)", "value": "us-west-1"},' +
        '{"name": "US West 2 (San Jose)", "value": "us-west-2"},' +
        '{"name": "CA Central 1 (Toronto)", "value": "ca-central-1"},' +
        '{"name": "EU Central 1 (Amsterdam)", "value": "eu-central-1"},' +
        '{"name": "EU Central 2 (Frankfurt)", "value": "eu-central-2"},' +
        '{"name": "EU West 1 (London)", "value": "eu-west-1"},' +
        '{"name": "EU West 2 (Paris)", "value": "eu-west-2"},' +
        '{"name": "EU West 3 (London)", "value": "eu-west-3"},' +
        '{"name": "EU South 1 (Milan)", "value": "eu-south-1"},' +
        '{"name": "AP Northeast 1 (Tokyo)", "value": "ap-northeast-1"},' +
        '{"name": "AP Northeast 2 (Osaka)", "value": "ap-northeast-2"},' +
        '{"name": "AP Southeast 1 (Singapore)", "value": "ap-southeast-1"},' +
        '{"name": "AP Southeast 2 (Sydney)", "value": "ap-southeast-2"}' +
      ']' +
    '},' +
    '"digitalocean": {' +
      '"host": "digitaloceanspaces.com",' +
      '"defaultHost": "nyc3.digitaloceanspaces.com",' +
      '"service": "",' +
      '"accessId": "DO_ACCESS_KEY_ID",' +
      '"secretKey": "DO_SECRET_ACCESS_KEY",' +
      '"defaultRegion": "nyc3",' +
      '"regions": [' +
        '{"name": "New York 3", "value": "nyc3"},' +
        '{"name": "San Francisco 2", "value": "sfo2"},' +
        '{"name": "San Francisco 3", "value": "sfo3"},' +
        '{"name": "Atlanta 1", "value": "atl1"},' +
        '{"name": "Toronto 1", "value": "tor1"},' +
        '{"name": "Amsterdam 3", "value": "ams3"},' +
        '{"name": "Frankfurt 1", "value": "fra1"},' +
        '{"name": "London 1", "value": "lon1"},' +
        '{"name": "Singapore 1", "value": "sgp1"},' +
        '{"name": "Bangalore 1", "value": "blr1"},' +
        '{"name": "Sydney 1", "value": "syd1"}' +
      ']' +
    '},' +
    '"google": {' +
      '"host": "",' +
      '"defaultHost": "storage.googleapis.com",' +
      '"service": "",' +
      '"accessId": "GOOG_ACCESS_KEY_ID",' +
      '"secretKey": "GOOG_SECRET_ACCESS_KEY",' +
      '"defaultRegion": "auto",' +
      '"regions": []' +
    '},' +
    '"ibm": {' +
      '"host": "cloud-object-storage.appdomain.cloud",' +
      '"defaultHost": "s3.us-south.cloud-object-storage.appdomain.cloud",' +
      '"service": "s3",' +
      '"accessId": "IBM_ACCESS_KEY_ID",' +
      '"secretKey": "IBM_SECRET_ACCESS_KEY",' +
      '"defaultRegion": "us-south",' +
      '"regions": [' +
        '{"name": "Dallas", "value": "us-south"},' +
        '{"name": "Washington DC", "value": "us-east"},' +
        '{"name": "Toronto", "value": "ca-tor"},' +
        '{"name": "S' + '\' + 'u00e3o Paulo", "value": "br-sao"},' +
        '{"name": "London", "value": "eu-gb"},' +
        '{"name": "Frankfurt", "value": "eu-de"},' +
        '{"name": "Madrid", "value": "eu-es"},' +
        '{"name": "Tokyo", "value": "jp-tok"},' +
        '{"name": "Osaka", "value": "jp-osa"},' +
        '{"name": "Sydney", "value": "au-syd"}' +
      ']' +
    '},' +
    '"scaleway": {' +
      '"host": "scw.cloud",' +
      '"defaultHost": "s3.fr-par.scw.cloud",' +
      '"service": "s3",' +
      '"accessId": "SCW_ACCESS_KEY",' +
      '"secretKey": "SCW_SECRET_KEY",' +
      '"defaultRegion": "fr-par",' +
      '"regions": [' +
        '{"name": "Paris", "value": "fr-par"},' +
        '{"name": "Amsterdam", "value": "nl-ams"},' +
        '{"name": "Warsaw", "value": "pl-waw"}' +
      ']' +
    '},' +
    '"ovh": {' +
      '"host": "io.cloud.ovh.net",' +
      '"defaultHost": "s3.gra.io.cloud.ovh.net",' +
      '"service": "s3",' +
      '"accessId": "OVH_ACCESS_KEY_ID",' +
      '"secretKey": "OVH_SECRET_ACCESS_KEY",' +
      '"defaultRegion": "gra",' +
      '"regions": [' +
        '{"name": "Gravelines", "value": "gra"},' +
        '{"name": "Roubaix", "value": "rbx"},' +
        '{"name": "Strasbourg", "value": "sbg"},' +
        '{"name": "Paris", "value": "eu-west-par"},' +
        '{"name": "Milan", "value": "eu-south-mil"},' +
        '{"name": "Frankfurt", "value": "de"},' +
        '{"name": "London", "value": "uk"},' +
        '{"name": "Warsaw", "value": "waw"},' +
        '{"name": "Beauharnois", "value": "bhs"},' +
        '{"name": "Toronto", "value": "ca-east-tor"},' +
        '{"name": "Singapore", "value": "sgp"},' +
        '{"name": "Sydney", "value": "ap-southeast-syd"},' +
        '{"name": "Mumbai", "value": "ap-south-mum"}' +
      ']' +
    '},' +
    '"hetzner": {' +
      '"host": "your-objectstorage.com",' +
      '"defaultHost": "fsn1.your-objectstorage.com",' +
      '"service": "",' +
      '"accessId": "HETZNER_ACCESS_KEY_ID",' +
      '"secretKey": "HETZNER_SECRET_ACCESS_KEY",' +
      '"defaultRegion": "fsn1",' +
      '"regions": [' +
        '{"name": "Falkenstein", "value": "fsn1"},' +
        '{"name": "Nuremberg", "value": "nbg1"},' +
        '{"name": "Helsinki", "value": "hel1"}' +
      ']' +
    '}' +
    '}';

var
  InternalServiceRegions: TObject;

function ServiceRegions: TJsonNode;
var
  N: TJsonNode;
begin
  if InternalServiceRegions = nil then
  begin
    N := TJsonNode.Create;
    N.Parse(ServiceList);
    InternalServiceRegions := N;
  end;
  Result := TJsonNode(InternalServiceRegions);
end;

constructor TS3Config.Create(const AProvider: string);
begin
  inherited Create;
  FProvider := AProvider;
  if ServiceRegions.Find(AProvider, FNode) then
  begin
    FHost := FNode.Force('host').AsString;
    FDefaultHost := FNode.Force('defaultHost').AsString;
    FService := FNode.Force('service').AsString;
    FPort := Round(FNode.Force('port').AsNumber);
    if FPort = 0 then
      FPort := DefPort;
    FDefaultRegion := FNode.Force('defaultRegion').AsString;
    FRegionVar := FNode.Force('regionVar').AsString;
    FAccessIdVar := FNode.Force('accessId').AsString;
    FSecretKeyVar := FNode.Force('secretKey').AsString;
  end
  else
  begin
    if FProvider = '' then
      FProvider := '(empty string)';
    raise ES3ConfigError.CreateFmt('Provider %s not found', [FProvider]);
  end;
end;

{ FNode belongs to the shared ServiceRegions tree and must not be freed here }

destructor TS3Config.Destroy;
begin
  inherited Destroy;
end;

function TS3Config.Provider: string;
begin
  Result := FProvider;
end;

function TS3Config.EndPoint(const Region: string = ''): string;
var
  S: string;
begin
  if FHost = '' then
    Exit(FDefaultHost);
  S := Region;
  if S = '' then
  begin
    if FDefaultHost <> '' then
      Exit(FDefaultHost);
    S := DefaultRegion;
  end;
  if FService <> '' then
    Result := FService + '.' + S + '.' + FHost
  else
    Result := S + '.' + FHost;
end;

function TS3Config.Port: Word;
begin
  Result := FPort;
end;

function TS3Config.DefaultRegion: string;
begin
  Result := '';
  if FRegionVar <> '' then
    Result := Trim(GetEnvironmentVariable(FRegionVar));
  if Result = '' then
    Result := FDefaultRegion;
end;

function TS3Config.LocationRegion(const Location: string): string;
var
  N: TJsonNode;
  S, Best: string;
begin
  Result := Trim(Location);
  if (FHost = '') or (Result = '') then
    Exit(DefaultRegion);
  if Result = 'EU' then
    Exit('eu-west-1');
  { Use the longest known region which the location begins with, for example
    us-south-standard becomes us-south }
  Best := '';
  for N in FNode.Force('regions').AsArray do
  begin
    S := N.Force('value').AsString;
    if S = Result then
      Exit;
    if (Length(S) > Length(Best)) and Result.BeginsWith(S + '-') then
      Best := S;
  end;
  if Best <> '' then
    Result := Best;
end;

procedure TS3Config.ListRegions(out Regions: TNamedStrings);
var
  N: TJsonNode;
begin
  for N in FNode.Force('regions').AsArray do
    Regions.Add(N.Force('name').AsString, N.Force('value').AsString);
end;

function TS3Config.GetAccessId: string;
begin
  if FAccessId = '' then
    FAccessId :=  GetEnvironmentVariable(FAccessIdVar);
  Result := FAccessId;
end;

function TS3Config.GetSecretKey: string;
begin
  if FSecretKey = '' then
    FSecretKey :=  GetEnvironmentVariable(FSecretKeyVar);
  Result := FSecretKey;
end;

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
  NoContent = 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855';
  UnsignedPayload = 'UNSIGNED-PAYLOAD';
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

function UrlEncode(const S: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(S) do
    if S[I] in ['A'..'Z', 'a'..'z', '0'..'9', '-', '_', '.', '~', '/'] then
      Result := Result + S[I]
    else
      Result := Result + '%' + IntToHex(Ord(S[I]), 2);
end;

function S3UrlDecode(const S: string): string;

  function HexValue(C: Char): Integer;
  begin
    case C of
      '0'..'9': Result := Ord(C) - Ord('0');
      'A'..'F': Result := Ord(C) - Ord('A') + 10;
      'a'..'f': Result := Ord(C) - Ord('a') + 10;
    else
      Result := -1;
    end;
  end;

var
  Hi, Lo, I, J: Integer;
begin
  SetLength(Result, Length(S));
  I := 1;
  J := 0;
  while I <= Length(S) do
  begin
    Inc(J);
    Result[J] := S[I];
    if S[I] = '+' then
      Result[J] := ' '
    else if (S[I] = '%') and (I + 2 <= Length(S)) then
    begin
      Hi := HexValue(S[I + 1]);
      Lo := HexValue(S[I + 2]);
      if (Hi > -1) and (Lo > -1) then
      begin
        Result[J] := Chr(Hi * 16 + Lo);
        Inc(I, 2);
      end;
    end;
    Inc(I);
  end;
  SetLength(Result, J);
end;

{ Note: Query parameters should be UriEncode'd before they are sent to GenerateRequest }

{ Signature version 4 requires query parameters sorted by name, then by value,
  in byte order }

function QueryCompare(List: TStringList; Index1, Index2: Integer): Integer;
var
  A, B: string;
begin
  A := List[Index1];
  B := List[Index2];
  Result := CompareStr(A.FirstOf('='), B.FirstOf('='));
  if Result = 0 then
    Result := CompareStr(A.SecondOf('='), B.SecondOf('='));
end;

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
    List.CustomSort(QueryCompare);
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

{ PayloadHash is the hex SHA256 of the request body, NoContent for an empty
  body, or UnsignedPayload when the body is not hashed }

function GenerateRequest(Config: TS3Config; const Region, Verb: string; Url: TUrl;
  Headers: THeaderPairs = nil; const PayloadHash: string = NoContent): string;
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
      .Add('X-Amz-Content-Sha256', PayloadHash)
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
      PayloadHash;
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
      Verb + ' ' + Url.Resource + ' HTTP/1.1'#13#10;
    for I := 0 to Headers.Count - 1 do
      with Headers[I] do
        Request := Request + Name + ': ' + Value + #13#10;
    Request := Request + 'Authorization: ' +
      'AWS4-HMAC-SHA256 Credential=' + Config.AccessId +
      '/' + DateShort +
      '/' + Region +
      '/' + Service +
      '/' + Version +
      ', SignedHeaders=' + Headers.Names +
      ', Signature=' + Signature + #13#10 +
      'Connection: Close'#13#10 +
        #13#10;
    Result := Request;
  finally
    Headers.Free;
  end;
end;

function GenerateUrl(Config: TS3Config; Url: TUrl; Region: string; Expires: Integer = 0): string;
const
  Week = 3600 * 24 * 7;
var
  Date: TDateTime;
  DateLong: string;
  DateShort: string;
  Host: string;
  Resource: string;
  Seconds: string;
  CanonicalRequest: string;
  StringToSign: string;
  Signature: string;
begin
  Date := NowUTC;
  DateShort := FormatDateTime('yyyymmdd', Date);
  DateLong := DateShort + 'T' + FormatDateTime('hhnnss', Date) + 'Z';
  Host := Url.Domain;
  Resource := Url.Resource;
  if Expires < 1 then
    Expires := Week;
  Seconds := IntToStr(Expires);
  CanonicalRequest :=
    'GET'#10 +
    Resource + #10 +
    'X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Credential=' + Config.AccessId +
    UriEncode('/' + DateShort + '/' + Region + '/s3/aws4_request') +
    '&X-Amz-Date=' + DateLong + '&X-Amz-Expires=' + Seconds + '&X-Amz-SignedHeaders=host'#10 +
    'host:' + Host + #10 +
    #10 +
    'host'#10 +
    'UNSIGNED-PAYLOAD';
  StringToSign :=
    'AWS4-HMAC-SHA256'#10 +
    DateLong + #10 +
    DateShort + '/' + Region + '/s3/aws4_request'#10 +
    HashString(hashSHA256, CanonicalRequest).AsHex;
  Signature :=
    AuthString('AWS4' + Config.SecretKey, hashSHA256, DateShort)
    .AuthNext(hashSHA256, Region)
    .AuthNext(hashSHA256, Service)
    .AuthNext(hashSHA256, Version)
    .AuthNext(hashSHA256, StringToSign)
    .AsHex;
  Result := 'https://' + Host;
  if Config.Port <> DefPort then
    Result := Result + ':' + IntToStr(Config.Port);
  Result := Result + Resource +
    '?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Credential=' + Config.AccessId +
    UriEncode('/' + DateShort + '/' + Region + '/s3/aws4_request') +
    '&X-Amz-Date=' + DateLong + '&X-Amz-Expires=' + Seconds + '&X-Amz-SignedHeaders=host' +
    '&X-Amz-Signature=' + Signature;
end;

function GenerateUrlSHA1(Config: TS3Config; Url: TUrl; Expires: Integer = 0): string;
var
  UnixTime: Int64;
  StringToSign: string;
  Signature: string;
begin
  if Expires < 1 then
    Expires := 3600;
  UnixTime := DateTimeToUnix(NowUTC) + Expires;
  StringToSign := 'GET'#10#10#10 + IntToStr(UnixTime) + #10 + UrlEncode(Url.Resource);
  Signature := AuthString(Config.SecretKey, hashSHA1, StringToSign).AsBase64;
  Result := Url.AsString + Format('?AWSAccessKeyId=%s&Expires=%s&Signature=%s',
    [Config.AccessId, IntToStr(UnixTime), UriEncode(Signature)]);
end;

{ TS3Methods }

{ Bucket regions are cached as case sensitive bucket=region pairs }

function NewBucketCache: TStrings;
var
  List: TStringList;
begin
  List := TStringList.Create;
  List.CaseSensitive := True;
  Result := List;
end;

constructor TS3Methods.Create(const AProvider: string);
begin
  inherited Create;
  FConfig := TS3Config.Create(AProvider);
  FBuckets := NewBucketCache;
end;

destructor TS3Methods.Destroy;
begin
  FBuckets.Free;
  FConfig.Free;
  inherited Destroy;
end;

function TS3Methods.GetConfig: TS3Config;
begin
  Result := FConfig;
end;

procedure TS3Methods.Reconfigure(const AProvider: string);
begin
  FBuckets.Free;
  FConfig.Free;
  FBuckets := NewBucketCache;
  FConfig := TS3Config.Create(AProvider);
end;

{$region bucket read}
function TS3Methods.QueryBucket(const Bucket, Query: string): TS3Request;
var
  Region: string;
begin
  if Query = 'location' then
    Region := FConfig.DefaultRegion
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
  I := FBuckets.IndexOfName(Bucket);
  if I < 0 then
  begin
    Request.EndPoint := FConfig.EndPoint;
    Request.Header := GenerateRequest(FConfig, FConfig.DefaultRegion, 'GET',
      FConfig.EndPoint + '/' + Bucket + '?location');
    if Send(Request, D) then
    begin
      Result := FConfig.LocationRegion(D.Root.Text);
      AddRegion(Bucket, Result);
    end;
  end
  else
    Result := FBuckets.ValueFromIndex[I];
  if Result = '' then
    Result := FConfig.DefaultRegion;
end;

procedure TS3Methods.AddRegion(const Bucket, Region: string);
begin
  FBuckets.Values[Bucket] := FConfig.LocationRegion(Region);
end;

function TS3Methods.ListBuckets: TS3Request;
begin
  Result.EndPoint := FConfig.EndPoint;
  Result.Header := GenerateRequest(FConfig, FConfig.DefaultRegion, 'GET', Result.EndPoint);
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

{$region objects}
const
  AccessAcl: array[TS3Access] of string = ('', 'private', 'public-read');

{ ObjectRequest builds a signed request for an object in a bucket. Headers is
  consumed by GenerateRequest. }

function ObjectRequest(S3: TS3Methods; const Verb, Bucket, Key, Query: string;
  Headers: THeaderPairs; const PayloadHash: string = NoContent): TS3Request;
var
  Region, Url: string;
begin
  Region := S3.FindRegion(Bucket);
  Result.EndPoint := S3.Config.EndPoint(Region);
  Url := Result.EndPoint + '/' + Bucket + '/' + UrlEncode(Key);
  if Query <> '' then
    Url := Url + '?' + Query;
  Result.Header := GenerateRequest(S3.Config, Region, Verb, Url, Headers, PayloadHash);
end;

function TS3Methods.GetObject(const Bucket, Key: string): TS3Request;
begin
  Result := ObjectRequest(Self, 'GET', Bucket, Key, '', nil);
end;

function TS3Methods.HeadObject(const Bucket, Key: string): TS3Request;
begin
  Result := ObjectRequest(Self, 'HEAD', Bucket, Key, '', nil);
end;

function TS3Methods.PutObject(const Bucket, Key: string; Size: Int64;
  const ContentType: string = ''; Access: TS3Access = accessDefault): TS3Request;
var
  Headers: THeaderPairs;
  S: string;
begin
  S := ContentType;
  if S = '' then
    S := 'application/octet-stream';
  Headers := NewHeaders
    .Add('Content-Length', IntToStr(Size))
    .Add('Content-Type', S);
  if Access <> accessDefault then
    Headers.Add('X-Amz-Acl', AccessAcl[Access]);
  { The body is not hashed so that large files are only read once. This is
    allowed because requests are always sent over https. }
  Result := ObjectRequest(Self, 'PUT', Bucket, Key, '', Headers, UnsignedPayload);
end;

function TS3Methods.DeleteObject(const Bucket, Key: string): TS3Request;
begin
  Result := ObjectRequest(Self, 'DELETE', Bucket, Key, '', nil);
end;

function TS3Methods.GetObjectAcl(const Bucket, Key: string): TS3Request;
begin
  Result := ObjectRequest(Self, 'GET', Bucket, Key, 'acl', nil);
end;

function TS3Methods.PutObjectAcl(const Bucket, Key: string; Access: TS3Access): TS3Request;
var
  Headers: THeaderPairs;
begin
  if Access = accessDefault then
    Access := accessPrivate;
  Headers := NewHeaders
    .Add('Content-Length', '0')
    .Add('X-Amz-Acl', AccessAcl[Access]);
  Result := ObjectRequest(Self, 'PUT', Bucket, Key, 'acl', Headers);
end;
{$endregion}

{$region send}
function TS3Methods.Presign(const Bucket, Path: string; Expires: Integer = 0): string;
var
  Region: string;
  Url: string;
begin
  Region := FindRegion(Bucket);
  Url := 'https://' + FConfig.EndPoint(Region);
  if FConfig.Port <> DefPort then
      Url := Url + ':' + IntToStr(FConfig.Port);
  Url := Url + '/' + Bucket + '/' + UrlEncode(Path);
  Result := GenerateUrl(FConfig, Url, Region, Expires);
end;

procedure ResponseCodes(Status: TResponseStatus; out Name: string; var Code: Integer);
const
  ResponseNames: array[TResponseStatus] of string = (
    'Could not connect',
    'No response recevied',
    'Error',
    'OK',
    'Cancelled');
begin
  Name :=  ResponseNames[Status];
  if Status <> rsSuccess then
    if Status = rsError then
    case Code of
      200: Name := 'OK';
      201: Name := 'Created';
      202: Name := 'Accepted';
      204: Name := 'No Content';
      301: Name := 'Moved Permanently';
      302: Name := 'Found';
      304: Name := 'Not Modified';
      400: Name := 'Bad Request';
      401: Name := 'Unauthorized';
      403: Name := 'Forbidden';
      404: Name := 'Not Found';
      500: Name := 'Internal Server Error';
      502: Name := 'Bad Gateway';
    end
    else
      Code := 0;
end;

{ InternalDoc converts a response to a document. When the response has no xml
  body its headers are stored as Headers/Header elements, each with a Name in
  lower case and a Value. }

function InternalDoc(const Body: string; Status: TResponseStatus;
  const Header: THttpResponseHeader): IDocument;
const
  Xmlns = ' xmlns="http://s3.amazonaws.com/doc/2006-03-01/"';
var
  Name: string;
  Code: Integer;
  Headers: INode;
  F: IFiler;
  I: Integer;
begin
  Result := NewDocument;
  if Body.BeginsWith('<?xml') then
    Result.Xml := Body.ReplaceOne(Xmlns, '')
  else
  begin
    if Status = rsSuccess then
      { Requests such as PutObject, DeleteObject, and HeadObject succeed with
        no body }
      Result.Force('Response')
    else
      Result.Force('Error');
    Headers := Result.Root.Force('Headers');
    for I := 0 to Header.Keys.Count - 1 do
    begin
      F := Headers.Nodes.Add('Header').Filer;
      F.WriteStr('Name', LowerCase(Header.Keys.Names[I]));
      F.WriteStr('Value', Header.Keys.ValueByIndex[I]);
    end;
  end;
  if Status <> rsSuccess then
  begin
    F := Result.Root.Force('ResponseStatus').Filer;
    Code := Header.Code;
    ResponseCodes(Status, Name, Code);
    F.WriteStr('Name', Name);
    F.WriteInt('Code', Code);
  end;
end;

function S3ResponseHeader(Response: IDocument; const Name: string): string;
var
  N: INode;
  F: IFiler;
  S: string;
begin
  Result := '';
  if (Response = nil) or (Response.Root = nil) then
    Exit;
  S := LowerCase(Name);
  for N in Response.Root.SelectList('Headers/Header') do
  begin
    F := N.Filer;
    if F.ReadStr('Name') = S then
      Exit(F.ReadStr('Value'));
  end;
end;

function TS3Methods.Send(const Request: TS3Request; out Response: IDocument): Boolean;
var
  ResponseHeader: THttpResponseHeader;
  Body: string;
  Status: TResponseStatus;
begin
  Status := WebSendRequest('https://' + Request.EndPoint + ':' + IntToStr(FConfig.Port),
    Request.Header, Body, ResponseHeader);
  Response := InternalDoc(Body, Status, ResponseHeader);
  Result := Status = rsSuccess;
end;

function TS3Methods.Send(const Request: TS3Request; Stream: TStream): Boolean;
var
  ResponseHeader: THttpResponseHeader;
  Status: TResponseStatus;
begin
  Status := WebSendRequest('https://' + Request.EndPoint + ':' + IntToStr(FConfig.Port),
    Request.Header, Stream, ResponseHeader);
  Result := Status = rsSuccess;
end;

{ TJoinStream reads a request header followed by a request body without
  copying the body into memory. It only supports reading, which is all a
  socket needs to send it. The body is not owned. }

type
  TJoinStream = class(TStream)
  private
    FHead: TStringStream;
    FTail: TStream;
  public
    constructor Create(const Head: string; Tail: TStream);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
  end;

constructor TJoinStream.Create(const Head: string; Tail: TStream);
begin
  inherited Create;
  FHead := TStringStream.Create(Head);
  FTail := Tail;
end;

destructor TJoinStream.Destroy;
begin
  FHead.Free;
  inherited Destroy;
end;

function TJoinStream.Read(var Buffer; Count: Longint): Longint;
var
  P: PByte;
begin
  Result := FHead.Read(Buffer, Count);
  if (Result < Count) and (FTail <> nil) then
  begin
    P := PByte(@Buffer);
    Inc(P, Result);
    Result := Result + FTail.Read(P^, Count - Result);
  end;
end;

{ SendBody sends a request header and body, outputting the response to a XML
  document. Task may be nil. }

function SendBody(const Request: TS3Request; Port: Word; Body: TStream;
  out Response: IDocument; Task: IAsyncTask = nil): Boolean;
var
  Stream: TStream;
  ResponseHeader: THttpResponseHeader;
  Text: string;
  Status: TResponseStatus;
begin
  Stream := TJoinStream.Create(Request.Header, Body);
  try
    Status := WebSendRequestStream('https://' + Request.EndPoint + ':' +
      IntToStr(Port), Stream, Text, ResponseHeader, Task);
  finally
    Stream.Free;
  end;
  Response := InternalDoc(Text, Status, ResponseHeader);
  Result := Status = rsSuccess;
end;

function TS3Methods.SendObject(const Request: TS3Request; Body: TStream;
  out Response: IDocument): Boolean;
begin
  Result := SendBody(Request, FConfig.Port, Body, Response);
end;

{ Async support }

type
  TAsyncParams<T> = record
    Request: TS3Request;
    Port: Word;
    Body: TStream;
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
  ResponseHeader: THttpResponseHeader;
  Body: string;
  Status: TResponseStatus;
begin
  if Params.Body <> nil then
  begin
    Params.Success := SendBody(Params.Request, Params.Port, Params.Body,
      Params.Result, Task);
    Exit;
  end;
  Status := WebSendRequest('https://' + Params.Request.EndPoint + ':' +
    IntToStr(Params.Port), Params.Request.Header, Body, ResponseHeader, Task);
  Params.Result := InternalDoc(Body, Status, ResponseHeader);
  Params.Success := Status = rsSuccess;
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
  Params.Body := nil;
  TThreadRunner<TAsyncDocParams>.Create(Params, Task, DocExecute, DocComplete);
end;

procedure TS3Methods.SendObjectAsync(const Request: TS3Request; Body: TStream;
  Task: IAsyncDocTask);
var
  Params: TAsyncDocParams;
begin
  Params.Request := Request;
  Params.Port := FConfig.Port;
  Params.Body := Body;
  TThreadRunner<TAsyncDocParams>.Create(Params, Task, DocExecute, DocComplete);
end;

procedure StreamExecute(var Params: TAsyncStreamParams; Task: IAsyncTask);
var
  ResponseHeader: THttpResponseHeader;
  Status: TResponseStatus;
begin
  Status := WebSendRequest('https://' + Params.Request.EndPoint + ':' +
    IntToStr(Params.Port), Params.Request.Header, Params.Result, ResponseHeader, Task);
  Params.Success := Status = rsSuccess;
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
  Params.Body := nil;
  Params.Result := Stream;
  TThreadRunner<TAsyncStreamParams>.Create(Params, Task, StreamExecute, StreamComplete);
end;
{$endregion}

initialization
  InternalServiceRegions := nil;
finalization
  InternalServiceRegions.Free;
end.

