(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified March 2015                                 *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.text.txt> }
unit Codebot.Text;

{$i codebot.inc}

interface

uses
  { Free pascal units }
  SysUtils, Classes,
  { Codebot units }
  Codebot.System;

{$region encoding}
{ The encoding methods can be hexadecimal or base64 [group encoding] }

type
  TEncodeMethod = (encodeHex, encodeBase64);

{doc off}
  IBuffer = interface(IInterface)
  ['{62C3AEC2-A51F-468C-9664-6027FF8722E6}']
    function GetData: Pointer;
    function GetSize: LongInt;
    procedure SetSize(Value: LongInt);
    property Data: Pointer read GetData;
    property Size: LongInt read GetSize write SetSize;
  end;
{doc on}

{ TBuffer is a managed a block of memory and is used when converting
  between text encodings and binary data [group memory]
  See also
  <link Overview.Codebot.Text.TBuffer, TBuffer members> }

  TBuffer = record
  private
    FBuffer: IBuffer;
    function GetData: Pointer;
    function GetSize: LongInt;
    procedure SetSize(Value: LongInt);
    function GetAsString: string;
  public
    { Allocate size number of bytes }
    class function Create(Size: LongInt): TBuffer; static;
    { Implicitly convert TBuffer to a memory address }
    class operator Implicit(const Value: TBuffer): Pointer;
    { Encode a buffer to a string using an encoding method }
    function Encode(Method: TEncodeMethod = encodeBase64): string;
    { Load buffer from a file }
    procedure LoadFromFile(const FileName: string);
    { Load buffer from a stream }
    procedure LoadFromStream(Stream: TStream);
    { Save buffer to a file }
    procedure SaveToFile(const FileName: string);
    { Save buffer to a stream }
    procedure SaveToStream(Stream: TStream);
    { The memory address where TBuffer stores data
      Remarks
      The memory will be valid until buffer goes out of scope or it is resized }
    property Data: Pointer read GetData;
    { The number of bytes allocated by buffer }
    property Size: LongInt read GetSize write SetSize;
    { If the buffer contains text, this is a shortcut to read back the text }
    property AsString: string read GetAsString;
  end;

{ TBufferStream can be used to convert a buffer to a stream [group stream]
  See also
  <link Overview.Codebot.Text.TBufferStream, TBufferStream members> }

  TBufferStream = class(TStream)
  private
    {doc off}
    FBuffer: IBuffer;
    FSize: Int64;
    FPosition: Int64;
  protected
    procedure SetSize(const NewSize: Int64); override;
    function GetSize: Int64; override;
  public
    {doc on}
    { Create a new buffer stream given a buffer }
    constructor Create(Buffer: TBuffer);
    function Read(var Buffer; Count: LongInt): LongInt; override;
    function Write(const Buffer; Count: LongInt): LongInt; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    procedure SaveToStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
  end;

{ Encode memory as a hexadecimal string [group encoding] }
function HexEncode(Buffer: Pointer; Size: LongInt): string; overload;
{ Encode a buffer as a hexadecimal string [group encoding] }
function HexEncode(const Buffer: TBuffer): string; overload;
{ Encode string data as a hexadecimal string [group encoding] }
function HexEncode(const S: string): string; overload;
{ Decode a hexadecimal string returing a buffer [group encoding] }
function HexDecode(const S: string): TBuffer;

{ Encode memory as a base64 string [group encoding] }
function Base64Encode(Buffer: Pointer; Size: LongInt): string; overload;
{ Encode a buffer as a base64 string [group encoding] }
function Base64Encode(const Buffer: TBuffer): string; overload;
{ Encode string data as a base64 string [group encoding] }
function Base64Encode(const S: string): string; overload;
{ Decode a base64 string returing a buffer [group encoding] }
function Base64Decode(const S: string): TBuffer;
{$endregion}

implementation

{$region encoding}
{ TBufferObject }

type
  TBufferObject = class(TInterfacedObject, IBuffer)
  private
    FData: Pointer;
    FSize: LongInt;
  public
    constructor Create(Size: LongInt);
    destructor Destroy; override;
    function GetData: Pointer;
    function GetSize: LongInt;
    procedure SetSize(Value: LongInt);
  end;

constructor TBufferObject.Create(Size: LongInt);
begin
  inherited Create;
  FSize := Size;
  if FSize > 0 then
    GetMem(FData, FSize)
  else
    FData := nil;
end;

destructor TBufferObject.Destroy;
begin
  if FData <> nil then
    FreeMem(FData);
  inherited Destroy;
end;

function TBufferObject.GetData: Pointer;
begin
  Result := FData;
end;

function TBufferObject.GetSize: LongInt;
begin
  Result := FSize;
end;

procedure TBufferObject.SetSize(Value: LongInt);
begin
  if Value <> FSize then
  begin
    FSize := Value;
    if FSize > 0 then
    begin
      if FData <> nil then
        ReallocMem(FData, FSize)
      else
        GetMem(FData, FSize);
    end
    else
    begin
      if FData <> nil then
        FreeMem(FData);
      FData := nil;
    end;
  end;
end;

{ TBuffer }

class function TBuffer.Create(Size: LongInt): TBuffer;
begin
  if Size > 0 then
    Result.FBuffer := TBufferObject.Create(Size)
  else
    Result.FBuffer := nil;
end;

class operator TBuffer.Implicit(const Value: TBuffer): Pointer;
begin
  if Value.FBuffer = nil then
    Result := nil
  else
    Result := Value.FBuffer.Data;
end;

function TBuffer.Encode(Method: TEncodeMethod = encodeBase64): string;
begin
  case Method of
    encodeHex: Result := HexEncode(Data, Size);
    encodeBase64: Result := Base64Encode(Data, Size);
  else
    Result := '';
  end;
end;

procedure TBuffer.LoadFromFile(const FileName: string);
var
  F: TStream;
begin
  F := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(F);
  finally
    F.Free;
  end;
end;

procedure TBuffer.LoadFromStream(Stream: TStream);
begin
  Size := Stream.Size - Stream.Position;
  Stream.Read(Data^, Size);
end;

procedure TBuffer.SaveToFile(const FileName: string);
var
  F: TStream;
begin
  F := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(F);
  finally
    F.Free;
  end;
end;

procedure TBuffer.SaveToStream(Stream: TStream);
begin
  Stream.Write(Data^, Size);
end;

function TBuffer.GetData: Pointer;
begin
  if FBuffer = nil then
    Result := nil
  else
    Result := FBuffer.Data;
end;

function TBuffer.GetSize: LongInt;
begin
  if FBuffer = nil then
    Result := 0
  else
    Result := FBuffer.Size;
end;

procedure TBuffer.SetSize(Value: LongInt);
begin
  if FBuffer = nil then
    FBuffer := TBufferObject.Create(Value);
  FBuffer.Size := Value;
end;

function TBuffer.GetAsString: string;
var
  I: Integer;
begin
  I := Size;
  if I < 1 then
    Exit('');
  SetLength(Result, I);
  Move(PChar(Data)[0], PChar(Result)[0], I);
end;

{ TBufferStream }

constructor TBufferStream.Create(Buffer: TBuffer);
begin
  inherited Create;
  FBuffer := Buffer.FBuffer;
  FSize := FBuffer.Size;
end;

function TBufferStream.Read(var Buffer; Count: LongInt): LongInt;
var
  MaxLen: LongInt;
begin
  if Count < 1 then
    Exit(0);
  MaxLen := FSize - FPosition;
  if MaxLen = 0 then
    Exit(0);
  Result := Count;
  if Result > MaxLen then
    Result := MaxLen;
  Move(PChar(FBuffer.Data)[FPosition], Buffer, Result);
  Inc(FPosition, Result);
end;

function TBufferStream.Write(const Buffer; Count: LongInt): LongInt;
begin
  if Count < 1 then
    Exit(0);
  Result := Count;
  if FPosition + Count > FSize then
  begin
    FSize := FPosition + Count;
    FBuffer.Size := FSize;
  end;
  Move(Buffer, PChar(FBuffer.Data)[FPosition], Count);
  Inc(FPosition, Count);
end;

function TBufferStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  case Origin of
    soBeginning: FPosition := Offset;
    soCurrent: Inc(FPosition, Offset);
    soEnd: FPosition := FSize - Offset;
  end;
  if FPosition > FSize then
    FPosition := FSize;
  Result := FPosition;
end;

function TBufferStream.GetSize: Int64;
begin
  Result := FSize;
end;

procedure TBufferStream.SetSize(const NewSize: Int64);
var
  S: Int64;
begin
  S := NewSize;
  if S < 1 then
    S := 0;
  if S <> FSize then
  begin
    FSize := S;
    FBuffer.Size := S;
  end;
end;

procedure TBufferStream.SaveToStream(Stream: TStream);
begin
  if FSize > 0 then Stream.WriteBuffer(FBuffer.Data^, FSize);
end;

procedure TBufferStream.SaveToFile(const FileName: string);
var
  S: TFileStream;
begin
  S := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(S);
  finally
    S.Free;
  end;
end;

{ Hex routines }

function HexEncode(Buffer: Pointer; Size: LongInt): string;
const
  Hex: PChar = '0123456789ABCDEF';
var
  B: PByte;
  C: PChar;
begin
  if Size = 0 then
    Exit('');
  SetLength(Result, Size shl 1);
  B := PByte(Buffer);
  C := PChar(Result);
  while Size > 0 do
  begin
    C^ := Hex[B^ shr $4];
    Inc(C);
    C^ := Hex[B^ and $F];
    Inc(C);
    Inc(B);
    Dec(Size);
  end;
end;

function HexEncode(const Buffer: TBuffer): string;
begin
  Result := HexEncode(Buffer.Data, Buffer.Size);
end;

function HexEncode(const S: string): string;
begin
  Result := HexEncode(Pointer(S), Length(S));
end;

function HexDecode(const S: string): TBuffer;
const
  Digit0 = Ord('0');
  DigitA = Ord('A');
var
  B: PByte;
  C: PChar;
  I: Integer;
begin
  I := Length(S);
  if Odd(I) or (I = 0) then
    Exit(TBuffer.Create(0));
  Result := TBuffer.Create(I shr 1);
  B := Result.Data;
  C := PChar(S);
  I := 0;
  repeat
    if C[I] in ['0'..'9'] then
      B^ := (Ord(C[I]) - Digit0) shl $4
    else if C[I] in ['A'..'F'] then
      B^ := (Ord(C[I]) - DigitA + $A) shl $4
    else
      Exit(TBuffer.Create(0));
    Inc(I);
    if C[I] in ['0'..'9'] then
      B^ := B^ or (Ord(C[I]) - Digit0)
    else if C[I] in ['A'..'F'] then
      B^ := B^ or (Ord(C[I]) - DigitA + $A)
    else
      Exit(TBuffer.Create(0));
    Inc(B);
    Inc(I);
  until C[I] = #0;
end;

{ Base64 routines }
const
  Base64: PChar = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';

function Base64EncodedSize(Size: LongInt): Cardinal;
begin
  Result := (Size div 3) shl 2;
  if (Size mod 3) > 0 then
    Inc(Result, 4);
end;

function Base64Encode(Buffer: Pointer; Size: LongInt): string;
const
  Fill: Char = '=';
var
  B: PByte;
  C: Byte;
  I: LongInt;
  J: LongInt;
begin
  SetLength(Result, Base64EncodedSize(Size));
  B := Buffer;
  I := 0;
  J := 0;
  while I < Size do
  begin
    C := (B[I] shr 2) and $3F;
    Inc(J);
    Result[J] := Base64[C];
    C := (B[I] shl 4) and $3f;
    Inc(I);
    if I < Size then
      C := C or ((B[I] shr 4) and $0F);
    Inc(J);
    Result[J] := Base64[C];
    if I < Size then
    begin
      C := (B[I] shl 2) and $3F;
      Inc(I);
      if I < Size then
        C := C or ((B[I] shr 6) and $03);
      Inc(J);
      Result[J] := Base64[C];
    end
    else
    begin
      Inc(I);
      Inc(J);
      Result[J] := Fill;
    end;
    if I < Size then
    begin
      C := B[I] and $3F;
      Inc(J);
      Result[J] := Base64[C];
    end
    else
    begin
      Inc(J);
      Result[J] := Fill;
    end;
    Inc(I);
  end;
end;

function Base64Encode(const Buffer: TBuffer): string;
begin
  Result := Base64Encode(Buffer, Buffer.Size);
end;

function Base64Encode(const S: string): string;
begin
  Result := Base64Encode(Pointer(S), Length(S));
end;

function Base64Decode(const S: string): TBuffer;

  procedure Zero(var Sextext, Index: LongInt); inline;
  begin
    Sextext := 0;
    Inc(Index);
  end;

  function Search(var Sextext, Index: LongInt): Boolean; inline;
  var
    C: Char;
    I: Integer;
  begin
    Sextext := 0;
    C := S[Index];
    Inc(Index);
    for I := 0 to 63 do
      if C = Base64[I] then
      begin
        Sextext := I;
        Exit(True);
      end;
    Result := False;
  end;

type
  TOutput = array[0..High(LongWord) div 4] of Byte;
  POutput = ^TOutput;
var
  Buffer: TBuffer;
  Output: POutput;
  InLen, OutLen, A, B, C, D, E, I, J: LongInt;
begin
  Result := TBuffer.Create(0);
  InLen := Length(S);
  if (InLen < 1) or (InLen mod 4 <> 0) then
    Exit;
  OutLen := InLen div 4 * 3;
  if S[InLen] = '=' then
    Dec(OutLen);
  if S[InLen - 1] = '=' then
    Dec(OutLen);
  if OutLen < 1 then
    Exit;
  Buffer := TBuffer.Create(OutLen);
  Output := Buffer.Data;
  A := 0;
  B := 0;
  C := 0;
  D := 0;
  J := 0;
  I := 1;
  Inc(InLen);
  while I < InLen do
  begin
    if S[I] = '=' then Zero(A, I) else if not Search(A, I) then Exit;
    if S[I] = '=' then Zero(B, I) else if not Search(B, I) then Exit;
    if S[I] = '=' then Zero(C, I) else if not Search(C, I) then Exit;
    if S[I] = '=' then Zero(D, I) else if not Search(D, I) then Exit;
    E := A shl 18 + B shl 12 + C shl 6 + D;
    if J >= OutLen then Break;
    Output^[J] := E shr 16 and $FF;
    Inc(J);
    if J >= OutLen then Break;
    Output^[J] := E shr 8 and $FF;
    Inc(J);
    if J >= OutLen then Break;
    Output^[J] := E and $FF;
    Inc(J);
  end;
  Result := Buffer;
end;
{$endregion}

end.

