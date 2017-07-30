(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified September 2013                             *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.cryptography.txt> }
unit Codebot.Cryptography;

{$i codebot.inc}

interface

uses
  { Free pascal units }
  SysUtils, Classes,
  { Codebot units }
  Codebot.System,
  Codebot.Interop.OpenSSL,
  Codebot.Text;

{$region hashing}
{ Hashing algorithm kinds }

type
  THashKind = (hashMD5, hashSHA1, hashSHA256, hashSHA512);

{ TDigest is a memory buffer }

  TDigest = TBuffer;

{ Convert a digest to a string }
function DigestToStr(const Digest: TDigest): string;

{ Compute the hash digest of a string }
function HashString(Kind: THashKind; const S: string): TDigest;
{ Compute the hash digest of a memory block }
function HashBuffer(Kind: THashKind; var Buffer; BufferSize: Cardinal): TDigest;
{ Compute the hash digest of a stream }
function HashStream(Kind: THashKind; Stream: TStream): TDigest;
{ Compute the hash digest of a file }
function HashFile(Kind: THashKind; const FileName: string): TDigest;

{ Compute the hmac digest of a string }
function AuthString(const Key: string; Kind: THashKind; const S: string): TDigest;
{ Compute the hmac digest of a memory block }
function AuthBuffer(const Key: string; Kind: THashKind; var Buffer; BufferSize: Cardinal): TDigest;
{ Compute the hmac digest of a stream }
function AuthStream(const Key: string; Kind: THashKind; Stream: TStream): TDigest;
{ Compute the hmac digest of a file }
function AuthFile(const Key: string; Kind: THashKind; const FileName: string): TDigest;
{$endregion}

{$region encryption}
{ Encrypt a string }
function Encrypt(const S: string): string;
{ Decrypt a string }
function Decrypt(const S: string): string;
{ Decrypt a string sequence }
function DecryptSequence(const S: string): string;
{$endregion}

implementation

{$region hashing}
function DigestToStr(const Digest: TDigest): string;
begin
  Result := HexEncode(Digest);
end;

type
  THashMethods = record
    Context: array[0..500] of Byte;
    Digest: TDigest;
    Init: function(var Context): LongBool; cdecl;
    Update: function(var Context; Data: Pointer; Size: Cardinal): LongBool; cdecl;
    Final: function(var Digest; var Context): LongBool; cdecl;
  end;

  TAuthMethod = record
    Digest: TDigest;
    Method:  TEVPMethod;
  end;

function GetHashMethods(Kind: THashKind; out Methods: THashMethods): Boolean;
begin
  OpenSSLInit(True);
  Result := True;
  case Kind of
    hashMD5:
      begin
        Methods.Digest := TBuffer.Create(SizeOf(TMD5Digest));
        Methods.Init := @MD5_Init;
        Methods.Update := @MD5_Update;
        Methods.Final := @MD5_Final;
      end;
    hashSHA1:
      begin
        Methods.Digest := TBuffer.Create(SizeOf(TSHA1Digest));
        Methods.Init := @SHA1_Init;
        Methods.Update := @SHA1_Update;
        Methods.Final := @SHA1_Final;
      end;
    hashSHA256:
      begin
        Methods.Digest := TBuffer.Create(SizeOf(TSHA256Digest));
        Methods.Init := @SHA256_Init;
        Methods.Update := @SHA256_Update;
        Methods.Final := @SHA256_Final;
      end;
    hashSHA512:
      begin
        Methods.Digest := TBuffer.Create(SizeOf(TSHA512Digest));
        Methods.Init := @SHA512_Init;
        Methods.Update := @SHA512_Update;
        Methods.Final := @SHA512_Final;
      end;
  else
    Methods.Digest := TBuffer.Create(0);
    Methods.Init := nil;
    Methods.Update := nil;
    Methods.Final := nil;
    Result := False;
  end;
end;

function GetAuthMethod(Kind: THashKind; out Method: TAuthMethod): Boolean;
begin
  OpenSSLInit(True);
  Result := True;
  case Kind of
    hashMD5:
      begin
        Method.Digest := TBuffer.Create(SizeOf(TMD5Digest));
        Method.Method := EVP_md5;
      end;
    hashSHA1:
      begin
        Method.Digest := TBuffer.Create(SizeOf(TSHA1Digest));
        Method.Method := EVP_sha1;
      end;
    hashSHA256:
      begin
        Method.Digest := TBuffer.Create(SizeOf(TSHA256Digest));
        Method.Method := EVP_sha256;
      end;
    hashSHA512:
      begin
        Method.Digest := TBuffer.Create(SizeOf(TSHA512Digest));
        Method.Method := EVP_sha512;
      end;
  else
    Method.Digest := TBuffer.Create(0);
    Method.Method := nil;
    Result := False;
  end;
end;

function HashString(Kind: THashKind; const S: string): TDigest;
begin
  Result := HashBuffer(Kind, PAnsiChar(S)^, Length(S));
end;

function HashBuffer(Kind: THashKind; var Buffer; BufferSize: Cardinal): TDigest;
var
  Methods: THashMethods;
begin
  if GetHashMethods(Kind, Methods) then
  begin
    Methods.Init(Methods.Context);
    Methods.Update(Methods.Context, @Buffer, BufferSize);
    if not Methods.Final(Methods.Digest.Data^, Methods.Context) then
      Methods.Digest := TDigest.Create(0);
  end;
  Result := Methods.Digest;
end;

function HashStream(Kind: THashKind; Stream: TStream): TDigest;
const
  BufferSize = $10000;
var
  Buffer: TBuffer;
  Bytes: LongInt;

  function ReadBuffer: Boolean;
  begin
    Bytes := Stream.Read(Buffer.Data^, BufferSize);
    Result := Bytes > 0;
  end;

var
  Methods: THashMethods;
begin
  if GetHashMethods(Kind, Methods) then
  begin
    Buffer := TBuffer.Create(BufferSize);
    Methods.Init(Methods.Context);
    while ReadBuffer do
      Methods.Update(Methods.Context, Buffer, Bytes);
    if not Methods.Final(Methods.Digest.Data^, Methods.Context) then
      Methods.Digest := TDigest.Create(0);
  end;
  Result := Methods.Digest;
end;

function HashFile(Kind: THashKind; const FileName: string): TDigest;
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    Result := HashStream(Kind, Stream);
  finally
    Stream.Free;
  end;
end;

function AuthString(const Key: string; Kind: THashKind; const S: string): TDigest;
begin
  Result := AuthBuffer(Key, Kind, Pointer(S)^, Length(S));
end;

function AuthBuffer(const Key: string; Kind: THashKind; var Buffer; BufferSize: Cardinal): TDigest;
var
  Method: TAuthMethod;
  Context: THMACCtx;
  I: LongWord;
begin
  if GetAuthMethod(Kind, Method) then
  begin
    HMAC_CTX_init(Context);
    try
      HMAC_Init_ex(Context, Pointer(Key), Length(Key), Method.Method, nil);
      HMAC_Update(Context, @Buffer, BufferSize);
      I := Method.Digest.Size;
      if not HMAC_Final(Context, Method.Digest, I) then
        Method.Digest := TDigest.Create(0);
    finally
      HMAC_CTX_cleanup(Context);
    end;
  end;
  Result := Method.Digest;
end;

function AuthStream(const Key: string; Kind: THashKind; Stream: TStream): TDigest;
const
  BufferSize = $10000;
var
  Buffer: TBuffer;
  Bytes: LongInt;

  function ReadBuffer: Boolean;
  begin
    Bytes := Stream.Read(Buffer.Data^, BufferSize);
    Result := Bytes > 0;
  end;

var
  Method: TAuthMethod;
  Context: THMACCtx;
  I: LongWord;
begin
  if GetAuthMethod(Kind, Method) then
  begin
    Buffer := TBuffer.Create(BufferSize);
    HMAC_CTX_init(Context);
    try
      HMAC_Init_ex(Context, Pointer(Key), Length(Key), Method.Method, nil);
      while ReadBuffer do
        HMAC_Update(Context, Buffer, BufferSize);
      I := Method.Digest.Size;
      if not HMAC_Final(Context, Method.Digest, I) then
        Method.Digest := TDigest.Create(0);
    finally
      HMAC_CTX_cleanup(Context);
    end;
  end;
  Result := Method.Digest;
end;

function AuthFile(const Key: string; Kind: THashKind; const FileName: string): TDigest;
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    Result := AuthStream(Key, Kind, Stream);
  finally
    Stream.Free;
  end;
end;
{$endregion}

{$region encryption}
function EndianLittleLong(L: LongWord): LongWord; inline;
begin
  {$ifdef endian_little}
  Result := L;
  {$else}
  Result := SwapEndian(L);
  {$endif}
end;

function EndianLittleWord(W: Word): Word; inline;
begin
  {$ifdef endian_little}
  Result := W;
  {$else}
  Result := SwapEndian(W);
  {$endif}
end;

function Encrypt(const S: string): string;
const
  Flags = #2;
var
  Buffer: PChar;
  Seed, R, I: Integer;
begin
  if Length(S) < 1 then
    Exit('');
  Result := S;
  UniqueString(Result);
  Buffer := PChar(Result);
  Randomize;
  Seed := Random(High(Word));
  R := Seed;
  for I := 1 to Length(Result) do
  begin
    R := (R * 17 + 2415235) and $FFFFFF;
    Buffer^ := Char(Ord(Buffer^) xor (R shr 10));
    Inc(Buffer);
  end;
  Result := 'XXXX' + Flags + 'XX' + Result;
  PLongWord(@Result[1])^ := EndianLittleLong(Length(Result) - 4);
  PWord(@Result[6])^ := EndianLittleWord(Seed);
end;

function Decrypt(const S: string): string;
var
  Flags: Byte;
  Buffer: PChar;
  R, I: Integer;
begin
  Result := '';
  if Length(S) < 8 then
    Exit;
  Flags := Byte(S[5]);
  R := EndianLittleWord(PWord(@S[6])^);
  I := EndianLittleLong(PCardinal(@S[1])^);
  Result := StrCopy(S, 8, I);
  if Flags and 2 <> 0 then
  begin
    Buffer := PChar(Result);
    for I := 1 to Length(result) do
    begin
      R := (R * 17 + 2415235) and $FFFFFF;
      Buffer^ := Chr(Ord(Buffer^) xor (R shr 10));
      Inc(Buffer);
    end;
  end;
end;

function DecryptSequence(const S: string): string;
var
  Buffer: string;
  I: Integer;
begin
  Result := '';
  Buffer := S;
  while Length(Buffer) > 4 do
  begin
    I := EndianLittleLong(PCardinal(Buffer)^) + 4;
    Result := Result + Decrypt(StrCopy(Buffer, 1, I));
    Delete(Buffer, 1, I);
  end;
end;
{$endregion}

end.

