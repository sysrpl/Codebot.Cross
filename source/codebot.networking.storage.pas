(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified August 2019                                *)
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
    function GetRequestHeader(const Host, Resource: string): string;
  public
    Header: THttpResponseHeader;
    constructor Create(Vendor: TCloudVendor; const PublicKey, PrivateKey: string);
    function Request(const Host, Resource: string): string;
    function List(const Resource: string): IDocument;
    function ListRaw(const Resource: string): string;
    function Fetch(const {%H-}Resource: string; {%H-}Stream: TStream): Boolean;
  end;

function NowUTC: TDateTime;

implementation

uses
{$ifdef windows}
	Windows;

function NowUTC: TDateTime;
var
  T: TSystemTime;
begin
  Windows.GetSystemTime(T{%H-});
  Result := SystemTimeToDateTime(T);
end;

{$else}
	Unix, BaseUnix;

Procedure JulianToGregorian(Julian: LongInt; out Year, Month, Day: Word);
const
  D0 = 1461;
  D1 = 146097;
  D2 = 1721119;
var
  YYear, XYear, Temp, TempMonth: LongInt;
begin
  Temp := ((Julian-D2) shl 2) - 1;
  Julian := Temp div D1;
  XYear := (Temp mod D1) or 3;
  YYear := (XYear div D0);
  Temp := ((((XYear mod D0) +4 ) shr 2) * 5) - 3;
  Day := ((Temp mod 153)+5) div 5;
  TempMonth := Temp div 153;
  if TempMonth >= 10 then
  begin
    Inc(YYear);
    Dec(TempMonth, 12);
  end;
  Inc(TempMonth, 3);
  Month := TempMonth;
  Year := YYear + (Julian * 100);
end;

{ Transforms Epoch time into local time (hour, minute,seconds) }

procedure EpochToLocal(Epoch: LongInt; out Y, M, D, H, N, S: Word);
const
  EDIV = 86400;
  C1970 = 2440588;
var
  I: LongInt;
begin
  I := (Epoch div EDIV) + C1970;
  JulianToGregorian(I, Y, M, D);
  Epoch := Abs(Epoch mod 86400);
  H := Epoch div 3600;
  Epoch := Epoch mod 3600;
  N := Epoch div 60;
  S := Epoch mod 60;
end;

function NowUTC: TDateTime;
var
  T: TTimeVal;
  S: TSystemTime;
begin
  fpgettimeofday(@T, nil);
  EpochToLocal(T.tv_sec, S.Year, S.Month, S.Day, S.Hour, S.Minute, S.Second);
  S.MilliSecond:=T.tv_usec div 1000;
  Result := SystemTimeToDateTime(S);
end;
{$endif}

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

function TCloudStorage.GetRequestHeader(const Host, Resource: string): string;
var
  Date: string;
  Signature: string;
begin
  Date := NowUTC.ToString('GMT');
  Signature := ComputeSignature('GET', '', '', Date, Resource);
  Result := 'GET ' + Resource + ' HTTP/1.0'#10 +
    'Host: ' + Host + #10 +
    'Connection: Close' + #10 +
    'Date: ' + Date + #10 +
    Signature + #10#10;
end;

function TCloudStorage.Request(const Host, Resource: string): string;
var
  Socket: TSocket;
  Body, Buffer: string;
begin
  Header.Clear;
  Body := '';
  Socket := TSocket.Create;
  try
    Socket.Secure := True;
    if Socket.Connect(Host, 443) then
    begin
      Buffer := GetRequestHeader(Host, Resource);
      Socket.WriteAll(Buffer);
      while Socket.Read(Buffer) > 0 do
      begin
        Body := Body + Buffer;
        if Header.Code = 0 then
          if not Header.Extract(Body) then
            Continue;
      end;
    end;
  finally
    Socket.Free;
  end;
  if Header.Code = 0 then
    Body := '';
  Result := Body;
end;

function TCloudStorage.List(const Resource: string): IDocument;
begin
  Result := DocumentCreate;
  Result.Xml := Request(CloudHosts[FVendor], Resource).Replace(' xmlns="http://', ' X="');
end;

function TCloudStorage.ListRaw(const Resource: string): string;
begin
  Result := Request(CloudHosts[FVendor], Resource);
end;

function TCloudStorage.Fetch(const Resource: string; Stream: TStream): Boolean;
begin
  Result := False;
end;

end.
