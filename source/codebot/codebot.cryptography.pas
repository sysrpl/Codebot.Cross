(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified September 2023                             *)
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

{ TDigestHelper }

type
  TDigestHelper = record helper for TDigest
  public
    { Compute the next hmac digest of a string using the current digest as the key }
    function AuthNext(Kind: THashKind; const S: string): TDigest;
  end;

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
const
  BufferSize = 4096;

var
  HashInitialized: Boolean;
  HashMethods: array[THashKind] of TEVPMethod;

procedure Init;
begin
  if HashInitialized then
    Exit;
  InitCrypto(True);
  HashMethods[hashMD5] := EVP_md5;
  HashMethods[hashSHA1] := EVP_sha1;
  HashMethods[hashSHA256] := EVP_sha256;
  HashMethods[hashSHA512] := EVP_sha512;
  HashInitialized := True;
end;

function DigestToStr(const Digest: TDigest): string;
begin
  Result := HexEncode(Digest);
end;

function HashString(Kind: THashKind; const S: string): TDigest;
begin
  Result := HashBuffer(Kind, PChar(S)^, Cardinal(Length(S)));
end;

function HashBuffer(Kind: THashKind; var Buffer; BufferSize: Cardinal): TDigest;
var
  Ctx: TEVPMdCtx;
  Size: Cardinal;
begin
  Init;
  Result := TDigest.Create(EVP_MAX_MD_SIZE);
  Ctx := EVP_MD_CTX_new;
  try
    EVP_DigestInit_ex(Ctx, HashMethods[Kind]);
    EVP_DigestUpdate(Ctx, @Buffer, BufferSize);
    EVP_DigestFinal(Ctx, Result.Data, Size);
    Result.Size := LongInt(Size);
  finally
    EVP_MD_CTX_free(Ctx);
  end;
end;

function HashStream(Kind: THashKind; Stream: TStream): TDigest;
var
  Ctx: TEVPMdCtx;
  Size: Cardinal;
  Bytes: Pointer;
  BytesRead: LongInt;
begin
  Init;
  Result := TDigest.Create(EVP_MAX_MD_SIZE);
  Bytes := GetMem(BufferSize);
  Ctx := EVP_MD_CTX_new;
  try
    EVP_DigestInit_ex(Ctx, HashMethods[Kind]);
    BytesRead := Stream.Read(Bytes^, BufferSize);
    while BytesRead > 0 do
    begin
      EVP_DigestUpdate(Ctx, Bytes, Cardinal(BytesRead));
      BytesRead := Stream.Read(Bytes^, BufferSize);
    end;
    if EVP_DigestFinal(Ctx, Result.Data, Size) then
      Result.Size := LongInt(Size)
    else
      Result.Size := 0;
  finally
    EVP_MD_CTX_free(Ctx);
    FreeMem(Bytes);
  end;
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
  Result := AuthBuffer(Key, Kind, Pointer(S)^, Cardinal(Length(S)));
end;

function AuthBuffer(const Key: string; Kind: THashKind; var Buffer; BufferSize: Cardinal): TDigest;
var
  Ctx: THMACCtx;
  Size: Cardinal;
begin
  Init;
  Result := TDigest.Create(EVP_MAX_MD_SIZE);
  Ctx := HMAC_CTX_new;
  try
    HMAC_Init_ex(Ctx, Pointer(Key), Length(Key), HashMethods[Kind]);
    HMAC_Update(Ctx, @Buffer, BufferSize);
    if HMAC_Final(Ctx, Result.Data, Size) then
      Result.Size := LongInt(Size)
    else
      Result.Size := 0;
  finally
    HMAC_CTX_free(Ctx);
  end;
end;

function AuthStream(const Key: string; Kind: THashKind; Stream: TStream): TDigest;
var
  Ctx: THMACCtx;
  Size: Cardinal;
  Bytes: Pointer;
  BytesRead: LongInt;
begin
  Init;
  Result := TDigest.Create(EVP_MAX_MD_SIZE);
  Bytes := GetMem(BufferSize);
  Ctx := HMAC_CTX_new;
  try
    HMAC_Init_ex(Ctx, Pointer(Key), Length(Key), HashMethods[Kind]);
    BytesRead := Stream.Read(Bytes^, BufferSize);
    while BytesRead > 0 do
    begin
      HMAC_Update(Ctx, Bytes, Cardinal(BytesRead));
      BytesRead := Stream.Read(Bytes^, BufferSize);
    end;
    if HMAC_Final(Ctx, Result.Data, Size) then
      Result.Size := LongInt(Size)
    else
      Result.Size := 0;
  finally
    HMAC_CTX_free(Ctx);
    FreeMem(Bytes);
  end;
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

{ TDigestHelper }

function TDigestHelper.AuthNext(Kind: THashKind; const S: string): TDigest;
var
  Ctx: THMACCtx;
  Size: Cardinal;
begin
  Init;
  Result := TDigest.Create(EVP_MAX_MD_SIZE);
  Ctx := HMAC_CTX_new;
  try
    HMAC_Init_ex(Ctx, Self.Data, Self.Size, HashMethods[Kind]);
    HMAC_Update(Ctx, Pointer(S), Cardinal(Length(S)));
    if HMAC_Final(Ctx, Result.Data, Size) then
      Result.Size := LongInt(Size)
    else
      Result.Size := 0;
  finally
    HMAC_CTX_free(Ctx);
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

