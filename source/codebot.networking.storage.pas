(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified March 2015                                 *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.networking.storage.txt> }
unit Codebot.Networking.Storage;

{$i codebot.inc}

interface

uses
  Classes,
  SysUtils,
  Codebot.System,
  Codebot.Text,
  Codebot.Text.Xml,
  Codebot.Cryptography,
  Codebot.Networking,
  Codebot.Networking.Web;

{ TCloudVendor }

type
  TCloudVendor = (
    cloudAmazon,
    cloudGoogle,
    cloudMicrosoft
  );

{ TCloudStorage }

  TCloudStorage = class(TObject)
  private
    FVendor: TCloudVendor;
    FPrivateKey: string;
    FPublicKey: string;
    function ComputeSignature(const Verb, MD5, ContentType, Date, Resource: string): string;
    function GetRequestHeader(const Resource: string): string;
  public
    Header: THttpResponseHeader;
    constructor Create(Vendor: TCloudVendor; const PublicKey, PrivateKey: string);
    function List(const Resource: string): IDocument;
    function ListRaw(const Resource: string): string;
    function Fetch(const Resource: string; Stream: TStream): Boolean;
  end;

implementation

uses
  lazutf8sysutils;

const
  CloudHosts: array[TCloudVendor] of string = (
    's3.amazonaws.com',
    'storage.googleapis.com',
    'blob.core.windows.net'
  );

constructor TCloudStorage.Create(Vendor: TCloudVendor; const PublicKey, PrivateKey: string);
begin
  inherited Create;
  FVendor := Vendor;
  FPublicKey := PublicKey;
  FPrivateKey := PrivateKey;
end;

function TCloudStorage.ComputeSignature(const Verb, MD5, ContentType, Date, Resource: string): string;
var
  S: string;
begin
  S := Verb + #10 + MD5 + #10 + ContentType + #10 + Date + #10 + Resource.FirstOf('?');
  Result := 'Authorization: AWS ' + FPublicKey + ':' + AuthString(FPrivateKey, hashSHA1, S).Encode;
end;

function TCloudStorage.GetRequestHeader(const Resource: string): string;
var
  Date: string;
  Signature: string;
begin
  Date := NowUTC.ToString('GMT');
  Signature := ComputeSignature('GET', '', '', Date, Resource);
  Result := 'GET ' + Resource + ' HTTP/1.0'#10 +
    'Host: ' + CloudHosts[FVendor] + #10 +
    'Connection: Close' + #10 +
    'Date: ' + Date + #10 +
    Signature + #10#10;
end;

function TCloudStorage.List(const Resource: string): IDocument;
var
  Socket: TSocket;
  Body, Buffer: string;
begin
  Header.Clear;
  Body := '';
  Socket := TSocket.Create;
  try
    if Socket.Connect(CloudHosts[FVendor], 80) then
    begin
      Buffer := GetRequestHeader(Resource);
      Socket.WriteAll(Buffer);
      while Socket.Read(Buffer) > 0 do
      begin
        Body := Body + Buffer;
        if Header.Code = 0 then
          if not Header.Extract(Body) then
            Continue;
        if XmlValidate(Body) then
          Break;
      end;
    end;
  finally
    Socket.Free;
  end;
  if Header.Code = 0 then
    Body := '';
  Result := DocumentCreate;
  Result.Xml := Body.Replace(' xmlns="http://', ' X="');
end;

function TCloudStorage.ListRaw(const Resource: string): string;
var
  D: IDocument;
begin
  D := List(Resource);
  D.Beautify;
  Result := D.Xml;
end;

function TCloudStorage.Fetch(const Resource: string; Stream: TStream): Boolean;
begin
  Result := False;
end;

end.

