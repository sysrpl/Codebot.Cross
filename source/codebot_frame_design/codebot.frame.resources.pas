(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ Codebot.Frame.Resources stores the properties of a converted control which
  cannot be written as code, such as images, in a resource file. Each
  resource holds one property in the binary form of a form file, which the
  control reads back using TReader.ReadProperty. }

{ <include docs/codebot.frame.resources.txt> }
unit Codebot.Frame.Resources;

{$mode delphi}

interface

uses
  Classes, SysUtils;

{ TPropertyResource is a property stored as a resource }

type
  TPropertyResource = record
    { The name of the resource, in uppercase }
    Name: string;
    { The name of the property followed by its value, in binary form }
    Data: TBytes;
  end;

  TPropertyResources = array of TPropertyResource;

{ Converts the text of a property from a form file, such as the data of a
  glyph, into its name followed by its value in binary form }
function PropertyToBinary(const Source: string): TBytes;
{ Writes Resources to the resource file FileName as raw data resources }
procedure WriteResourceFile(const FileName: string; const Resources: TPropertyResources);

implementation

function PropertyToBinary(const Source: string): TBytes;
var
  Input: TStringStream;
  Output: TMemoryStream;
  P: PByte;
  Offset: Integer;
begin
  { The property is placed in an object of its own and converted along with
    it, then the object header is removed, leaving the property }
  Input := TStringStream.Create('object Dummy: TPersistent'#10 + Source + #10'end'#10);
  Output := TMemoryStream.Create;
  try
    ObjectTextToBinary(Input, Output);
    P := Output.Memory;
    { The header is the signature followed by the class name and the object
      name, each a byte with its length followed by its characters }
    Offset := 4;
    Offset := Offset + 1 + P[Offset];
    Offset := Offset + 1 + P[Offset];
    SetLength(Result, Output.Size - Offset);
    if Length(Result) > 0 then
      Move(P[Offset], Result[0], Length(Result));
  finally
    Output.Free;
    Input.Free;
  end;
end;

procedure WriteResourceFile(const FileName: string; const Resources: TPropertyResources);
var
  Stream: TFileStream;

  procedure WriteDWord(Value: LongWord);
  begin
    Value := NtoLE(Value);
    Stream.WriteBuffer(Value, SizeOf(Value));
  end;

  procedure WriteWord(Value: Word);
  begin
    Value := NtoLE(Value);
    Stream.WriteBuffer(Value, SizeOf(Value));
  end;

  procedure Align;
  begin
    while Stream.Position mod 4 <> 0 do
      WriteWord(0);
  end;

  procedure WriteResource(const Resource: TPropertyResource);
  var
    HeaderSize, I: Integer;
  begin
    { The sizes, the type, and the name as UTF-16 with a terminating zero,
      aligned to four bytes, followed by four more values }
    HeaderSize := 8 + 4 + (Length(Resource.Name) + 1) * 2;
    if HeaderSize mod 4 <> 0 then
      Inc(HeaderSize, 2);
    Inc(HeaderSize, 16);
    WriteDWord(Length(Resource.Data));
    WriteDWord(HeaderSize);
    { The type RT_RCDATA is given by its number }
    WriteWord($FFFF);
    WriteWord(10);
    for I := 1 to Length(Resource.Name) do
      WriteWord(Ord(Resource.Name[I]));
    WriteWord(0);
    Align;
    { The data version, memory flags, language, version, and characteristics }
    WriteDWord(0);
    WriteWord($30);
    WriteWord(0);
    WriteDWord(0);
    WriteDWord(0);
    if Length(Resource.Data) > 0 then
      Stream.WriteBuffer(Resource.Data[0], Length(Resource.Data));
    Align;
  end;

var
  I: Integer;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    { A resource file starts with an empty resource }
    WriteDWord(0);
    WriteDWord(32);
    WriteWord($FFFF);
    WriteWord(0);
    WriteWord($FFFF);
    WriteWord(0);
    WriteDWord(0);
    WriteWord(0);
    WriteWord(0);
    WriteDWord(0);
    WriteDWord(0);
    for I := 0 to Length(Resources) - 1 do
      WriteResource(Resources[I]);
  finally
    Stream.Free;
  end;
end;

end.
