unit Codebot.Render.Assets;

{$i render.inc}

interface

uses
  SysUtils, Classes;

{ Assets are the files a program loads while it runs, such as fonts, images,
  and sounds. They are named by their path below the assets folder, with
  forward slashes, such as 'fonts/roboto.ttf'.

  An asset comes from one of two places:

    A file in the assets folder, which is searched for from the current
    directory and then from the folders above it

    A dat file, which holds every asset of a program in one file. It is kept
    beside the program and is named after it, such as cardgames.dat.

  A dat file is only used by a program which has set AssetKey, and only if
  the dat file is there. Without a key, or without a dat file, assets are
  files in the assets folder. When a dat file is used an asset which is not
  in it is still looked for in the assets folder.

  Every byte of a dat file is changed with xor by the four bytes of the key,
  so its contents can not be read without the key. See AssetXor.

  A program makes its own dat file when it is started with --build-dat. The
  hosts in Codebot.Render.Scenes handle that before they open a window.

  The layout of a dat file, before the xor, with numbers stored low byte
  first:

    Header   'CDAT', the version as 4 bytes, the number of assets as 4
             bytes, the size of the index as 4 bytes, and the offset of the
             index as 8 bytes
    Data     The bytes of each asset, one after the other
    Index    For each asset the length of its name as 2 bytes, its name in
             UTF-8, its offset as 8 bytes, and its size as 8 bytes

  Names are compared without regard to case, so a program finds the same
  assets on every system. }

type
  EAssetError = class(Exception);

const
  { The switch which makes a program build its dat file and end }
  AssetBuildSwitch = '--build-dat';

var
  { The key of the dat file of this program. Zero means there is no key, and
    then no dat file is used. Programs set this through the AssetKey property
    of their host before a scene is run. }
  AssetKey: LongWord = 0;
  { The name of the folder searched for assets which are files }
  AssetFolder: string = 'assets';

{ AssetXor changes Count bytes at Data with xor. Position is the offset in the
  dat file of the first byte, so any part of a file can be changed by itself.

  The bytes of a key such as $A1B2C3D4 are numbered one to four from the
  left, $A1 being the first. The bytes of the file use them in the order
  four, one, two, three, and then again from four. Doing this twice gives
  back the bytes which were there, so it is used both to write and to read. }

procedure AssetXor(Data: PByte; Count: SizeInt; Position: Int64; Key: LongWord);

{ The dat file of this program, which is beside the program and has its name }
function AssetPackFileName: string;
{ True if this program has a key and its dat file is being used }
function AssetPackActive: Boolean;
{ Give the bytes of an asset in the dat file. The memory is kept until the
  program ends and must not be freed. This is for loaders which keep using
  the memory they are given, such as fonts. }
function AssetPackData(const Name: string; out Data: Pointer; out Size: LongWord): Boolean;

{ Search for an asset which is a file in the assets folder, giving its path }
function AssetFindFile(const Name: string; out FileName: string): Boolean;
{ Search for the assets folder itself, giving its full path }
function AssetFindFolder(out Folder: string): Boolean;

{ True if an asset is in the dat file or is a file in the assets folder }
function AssetExists(const Name: string): Boolean;
{ Open an asset for reading, or return nil if there is no asset with the
  name. The stream must be freed. }
function AssetOpen(const Name: string): TStream;
{ Open an asset for reading, raising an EAssetError if it is not found. The
  stream must be freed. }
function AssetRequire(const Name: string): TStream;
{ Read an asset as text, raising an EAssetError if it is not found }
function AssetText(const Name: string): string;
{ Raise the EAssetError of an asset which was not found }
procedure AssetNotFound(const Name: string);

{ Build a dat file from every file in a folder and the folders inside it,
  returning the number of assets written. Files and folders whose names
  begin with a period are left out. }
function AssetPackBuild(const Folder, FileName: string; Key: LongWord): Integer;

{ AssetBuildRequested returns true if the program was started with
  --build-dat. The dat file is then built from the assets folder, a message
  is written, and ExitCode is set to zero if it worked and one if not. The
  caller must end the program without running it.

  A file name may follow the switch to say where the dat file is written.
  Without one it is written beside the program. With no key nothing is
  built and ExitCode is one. }

function AssetBuildRequested: Boolean;

implementation

resourcestring
  SAssetNotFound = 'Cannot locate asset with name ''%s''';
  SPackInvalid = 'The file ''%s'' is not a dat file made with the key of this program';
  SPackDuplicate = 'The assets ''%s'' and ''%s'' have the same name';
  SPackNoFolder = 'The folder ''%s'' was not found';
  SPackNoKey = 'No asset key is set, so no dat file was built';
  SPackWrote = 'Wrote %s holding %d assets';
  SPackTooLarge = 'The asset ''%s'' is too large';

const
  PackMagic: array[0..3] of Byte = (Ord('C'), Ord('D'), Ord('A'), Ord('T'));
  PackVersion = 1;
  PackHeaderSize = 24;
  { How many folders above the current directory are searched }
  SearchDepth = 9;

type
  TPackEntry = record
    { The name as it was stored and the name used to compare }
    Name: string;
    Key: string;
    Offset: Int64;
    Size: Int64;
    { The bytes of the asset once AssetPackData was asked for them }
    Data: Pointer;
    { The file an asset is read from while a dat file is built }
    Source: string;
  end;

  TPackEntries = array of TPackEntry;

  TPackState = (psUnknown, psNone, psOpen, psInvalid);

var
  PackLock: TRTLCriticalSection;
  PackState: TPackState = psUnknown;
  PackFile: TFileStream;
  PackEntries: TPackEntries;

procedure AssetXor(Data: PByte; Count: SizeInt; Position: Int64; Key: LongWord);
var
  K: array[0..3] of Byte;
  I: SizeInt;
  J: Integer;
begin
  K[0] := Byte(Key shr 24);
  K[1] := Byte(Key shr 16);
  K[2] := Byte(Key shr 8);
  K[3] := Byte(Key);
  { The first byte of the file uses the fourth byte of the key }
  J := Integer((Position + 3) and 3);
  for I := 0 to Count - 1 do
  begin
    Data^ := Data^ xor K[J];
    Inc(Data);
    J := (J + 1) and 3;
  end;
end;

{ Numbers are stored low byte first, whatever the computer }

procedure PutNumber(var P: PByte; Value: QWord; Bytes: Integer);
var
  I: Integer;
begin
  for I := 0 to Bytes - 1 do
  begin
    P^ := Byte(Value shr (I * 8));
    Inc(P);
  end;
end;

function GetNumber(var P: PByte; Bytes: Integer): QWord;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Bytes - 1 do
  begin
    Result := Result or (QWord(P^) shl (I * 8));
    Inc(P);
  end;
end;

{ The name of an asset as it is kept in a dat file: forward slashes, and
  nothing before the first folder or file }

function CleanName(const Name: string): string;
begin
  Result := StringReplace(Name, '\', '/', [rfReplaceAll]);
  while Copy(Result, 1, 2) = './' do
    Delete(Result, 1, 2);
  while Copy(Result, 1, 1) = '/' do
    Delete(Result, 1, 1);
end;

function CompareName(const Name: string): string;
begin
  Result := LowerCase(CleanName(Name));
end;

procedure SortEntries(var Entries: TPackEntries; L, R: Integer);
var
  I, J: Integer;
  Pivot: string;
  T: TPackEntry;
begin
  while L < R do
  begin
    I := L;
    J := R;
    Pivot := Entries[(L + R) div 2].Key;
    repeat
      while CompareStr(Entries[I].Key, Pivot) < 0 do
        Inc(I);
      while CompareStr(Entries[J].Key, Pivot) > 0 do
        Dec(J);
      if I <= J then
      begin
        T := Entries[I];
        Entries[I] := Entries[J];
        Entries[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      SortEntries(Entries, L, J);
    L := I;
  end;
end;

function AssetPackFileName: string;
begin
  Result := ChangeFileExt(ExpandFileName(ParamStr(0)), '.dat');
end;

{ Read the header and the index of the dat file. PackLock is held. }

procedure PackRead(const FileName: string);
var
  Header: array[0..PackHeaderSize - 1] of Byte;
  Index: array of Byte;
  P: PByte;
  Count, IndexSize: LongWord;
  IndexOffset: Int64;
  { The bytes of the index which have not been read yet }
  Left: Int64;
  Len: Integer;
  I: Integer;
begin
  PackFile := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    if PackFile.Read(Header, PackHeaderSize) <> PackHeaderSize then
      raise EAssetError.CreateFmt(SPackInvalid, [FileName]);
    AssetXor(@Header[0], PackHeaderSize, 0, AssetKey);
    if not CompareMem(@Header[0], @PackMagic[0], SizeOf(PackMagic)) then
      raise EAssetError.CreateFmt(SPackInvalid, [FileName]);
    P := @Header[4];
    if GetNumber(P, 4) <> PackVersion then
      raise EAssetError.CreateFmt(SPackInvalid, [FileName]);
    Count := LongWord(GetNumber(P, 4));
    IndexSize := LongWord(GetNumber(P, 4));
    IndexOffset := Int64(GetNumber(P, 8));
    if (IndexOffset < PackHeaderSize) or (IndexOffset + IndexSize > PackFile.Size) then
      raise EAssetError.CreateFmt(SPackInvalid, [FileName]);
    Index := nil;
    SetLength(Index, IndexSize + 1);
    PackFile.Position := IndexOffset;
    if PackFile.Read(Index[0], IndexSize) <> Integer(IndexSize) then
      raise EAssetError.CreateFmt(SPackInvalid, [FileName]);
    AssetXor(@Index[0], IndexSize, IndexOffset, AssetKey);
    P := @Index[0];
    Left := IndexSize;
    PackEntries := nil;
    SetLength(PackEntries, Count);
    for I := 0 to Integer(Count) - 1 do
    begin
      if Left < 2 then
        raise EAssetError.CreateFmt(SPackInvalid, [FileName]);
      Len := Integer(GetNumber(P, 2));
      Left := Left - 2;
      if Left < Len + 16 then
        raise EAssetError.CreateFmt(SPackInvalid, [FileName]);
      Left := Left - Len - 16;
      SetString(PackEntries[I].Name, PAnsiChar(P), Len);
      Inc(P, Len);
      PackEntries[I].Key := CompareName(PackEntries[I].Name);
      PackEntries[I].Offset := Int64(GetNumber(P, 8));
      PackEntries[I].Size := Int64(GetNumber(P, 8));
      PackEntries[I].Data := nil;
      PackEntries[I].Source := '';
      if (PackEntries[I].Offset < PackHeaderSize) or (PackEntries[I].Size < 0) or
        (PackEntries[I].Offset + PackEntries[I].Size > IndexOffset) then
        raise EAssetError.CreateFmt(SPackInvalid, [FileName]);
    end;
    { The index is sorted here, so finding a name does not rely on the order
      it was written in }
    if Count > 1 then
      SortEntries(PackEntries, 0, Integer(Count) - 1);
  except
    PackEntries := nil;
    FreeAndNil(PackFile);
    raise;
  end;
end;

{ PackOpen returns true if the dat file is in use, opening it the first time.
  A dat file which is there but can not be read raises an EAssetError each
  time, so the program stops with that message rather than going on to look
  for an assets folder it was not given. PackLock is held. }

function PackOpen: Boolean;
var
  FileName: string;
begin
  Result := False;
  if AssetKey = 0 then
    Exit;
  FileName := AssetPackFileName;
  if PackState = psUnknown then
    if not FileExists(FileName) then
      PackState := psNone
    else
    try
      PackRead(FileName);
      PackState := psOpen;
    except
      PackState := psInvalid;
    end;
  if PackState = psInvalid then
    raise EAssetError.CreateFmt(SPackInvalid, [FileName]);
  Result := PackState = psOpen;
end;

{ Find an asset in the index, which is sorted. PackLock is held. }

function PackFind(const Name: string): Integer;
var
  Key: string;
  L, H, M, C: Integer;
begin
  Key := CompareName(Name);
  L := 0;
  H := Length(PackEntries) - 1;
  while L <= H do
  begin
    M := (L + H) div 2;
    C := CompareStr(PackEntries[M].Key, Key);
    if C = 0 then
      Exit(M);
    if C < 0 then
      L := M + 1
    else
      H := M - 1;
  end;
  Result := -1;
end;

{ Read the bytes of an asset from the dat file. PackLock is held. }

procedure PackLoad(Index: Integer; Data: Pointer);
var
  E: ^TPackEntry;
begin
  E := @PackEntries[Index];
  if E.Size = 0 then
    Exit;
  PackFile.Position := E.Offset;
  PackFile.ReadBuffer(Data^, E.Size);
  AssetXor(Data, E.Size, E.Offset, AssetKey);
end;

function AssetPackActive: Boolean;
begin
  EnterCriticalSection(PackLock);
  try
    Result := PackOpen;
  finally
    LeaveCriticalSection(PackLock);
  end;
end;

function AssetPackData(const Name: string; out Data: Pointer; out Size: LongWord): Boolean;
var
  I: Integer;
begin
  Result := False;
  Data := nil;
  Size := 0;
  EnterCriticalSection(PackLock);
  try
    if not PackOpen then
      Exit;
    I := PackFind(Name);
    if I < 0 then
      Exit;
    if PackEntries[I].Size > High(LongWord) then
      raise EAssetError.CreateFmt(SPackTooLarge, [Name]);
    if (PackEntries[I].Data = nil) and (PackEntries[I].Size > 0) then
    begin
      GetMem(PackEntries[I].Data, PackEntries[I].Size);
      try
        PackLoad(I, PackEntries[I].Data);
      except
        FreeMem(PackEntries[I].Data);
        PackEntries[I].Data := nil;
        raise;
      end;
    end;
    Data := PackEntries[I].Data;
    Size := LongWord(PackEntries[I].Size);
    Result := True;
  finally
    LeaveCriticalSection(PackLock);
  end;
end;

{ Open an asset in the dat file as a stream holding a copy of its bytes, or
  return nil if the dat file is not in use or does not hold the asset }

function PackOpenStream(const Name: string): TStream;
var
  M: TMemoryStream;
  I: Integer;
begin
  Result := nil;
  EnterCriticalSection(PackLock);
  try
    if not PackOpen then
      Exit;
    I := PackFind(Name);
    if I < 0 then
      Exit;
    M := TMemoryStream.Create;
    try
      M.SetSize(PackEntries[I].Size);
      PackLoad(I, M.Memory);
      M.Position := 0;
    except
      M.Free;
      raise;
    end;
    Result := M;
  finally
    LeaveCriticalSection(PackLock);
  end;
end;

function PackContains(const Name: string): Boolean;
begin
  EnterCriticalSection(PackLock);
  try
    Result := PackOpen and (PackFind(Name) > -1);
  finally
    LeaveCriticalSection(PackLock);
  end;
end;

function AssetFindFile(const Name: string; out FileName: string): Boolean;
var
  S: string;
  I: Integer;
begin
  FileName := '';
  S := AssetFolder + DirectorySeparator + Name;
  for I := 0 to SearchDepth do
  begin
    if FileExists(S) then
    begin
      FileName := S;
      Exit(True);
    end;
    S := '..' + DirectorySeparator + S;
  end;
  Result := False;
end;

function AssetFindFolder(out Folder: string): Boolean;
var
  S: string;
  I: Integer;
begin
  Folder := '';
  S := AssetFolder;
  for I := 0 to SearchDepth do
  begin
    if DirectoryExists(S) then
    begin
      Folder := ExpandFileName(S);
      Exit(True);
    end;
    S := '..' + DirectorySeparator + S;
  end;
  Result := False;
end;

function AssetExists(const Name: string): Boolean;
var
  S: string;
begin
  Result := PackContains(Name) or AssetFindFile(Name, S);
end;

function AssetOpen(const Name: string): TStream;
var
  S: string;
begin
  Result := PackOpenStream(Name);
  if (Result = nil) and AssetFindFile(Name, S) then
    Result := TFileStream.Create(S, fmOpenRead or fmShareDenyWrite);
end;

procedure AssetNotFound(const Name: string);
begin
  raise EAssetError.CreateFmt(SAssetNotFound, [Name]);
end;

function AssetRequire(const Name: string): TStream;
begin
  Result := AssetOpen(Name);
  if Result = nil then
    AssetNotFound(Name);
end;

function AssetText(const Name: string): string;
var
  S: TStream;
begin
  S := AssetRequire(Name);
  try
    Result := '';
    SetLength(Result, S.Size);
    if S.Size > 0 then
      S.ReadBuffer(Result[1], S.Size);
  finally
    S.Free;
  end;
end;

{ Add the files of a folder, and of the folders inside it, to a list. Prefix
  is the name of the folder as it is kept in the dat file, ending with a
  slash, or nothing for the assets folder itself. }

procedure GatherFiles(const Folder, Prefix: string; var Entries: TPackEntries);
var
  Search: TSearchRec;
  Path: string;
  I: Integer;
begin
  if FindFirst(IncludeTrailingPathDelimiter(Folder) + AllFilesMask, faAnyFile, Search) <> 0 then
    Exit;
  try
    repeat
      if (Search.Name = '') or (Search.Name[1] = '.') then
        Continue;
      Path := IncludeTrailingPathDelimiter(Folder) + Search.Name;
      if Search.Attr and faDirectory <> 0 then
        GatherFiles(Path, Prefix + Search.Name + '/', Entries)
      else
      begin
        I := Length(Entries);
        SetLength(Entries, I + 1);
        Entries[I].Name := Prefix + Search.Name;
        Entries[I].Key := CompareName(Entries[I].Name);
        Entries[I].Offset := 0;
        Entries[I].Size := 0;
        Entries[I].Data := nil;
        Entries[I].Source := Path;
      end;
    until FindNext(Search) <> 0;
  finally
    FindClose(Search);
  end;
end;

function AssetPackBuild(const Folder, FileName: string; Key: LongWord): Integer;
const
  BufferSize = 64 * 1024;
var
  Entries: TPackEntries;
  Dest: TFileStream;
  Source: TFileStream;
  Buffer: array of Byte;
  Header: array[0..PackHeaderSize - 1] of Byte;
  Index: TMemoryStream;
  Item: array[0..15] of Byte;
  P: PByte;
  Position, IndexOffset: Int64;
  Count, I: Integer;
begin
  if not DirectoryExists(Folder) then
    raise EAssetError.CreateFmt(SPackNoFolder, [Folder]);
  Entries := nil;
  GatherFiles(Folder, '', Entries);
  if Length(Entries) > 1 then
    SortEntries(Entries, 0, Length(Entries) - 1);
  { Names which differ only by case would be the same asset }
  for I := 1 to Length(Entries) - 1 do
    if Entries[I].Key = Entries[I - 1].Key then
      raise EAssetError.CreateFmt(SPackDuplicate, [Entries[I - 1].Name, Entries[I].Name]);
  Buffer := nil;
  SetLength(Buffer, BufferSize);
  Index := nil;
  Dest := TFileStream.Create(FileName, fmCreate);
  try
    try
      { The header is written last, when the place of the index is known }
      FillChar(Header, SizeOf(Header), 0);
      Dest.WriteBuffer(Header, PackHeaderSize);
      Position := PackHeaderSize;
      for I := 0 to Length(Entries) - 1 do
      begin
        Entries[I].Offset := Position;
        Source := TFileStream.Create(Entries[I].Source, fmOpenRead or fmShareDenyWrite);
        try
          repeat
            Count := Source.Read(Buffer[0], BufferSize);
            if Count > 0 then
            begin
              AssetXor(@Buffer[0], Count, Position, Key);
              Dest.WriteBuffer(Buffer[0], Count);
              Position := Position + Count;
            end;
          until Count <= 0;
        finally
          Source.Free;
        end;
        Entries[I].Size := Position - Entries[I].Offset;
      end;
      IndexOffset := Position;
      Index := TMemoryStream.Create;
      for I := 0 to Length(Entries) - 1 do
      begin
        if Length(Entries[I].Name) > High(Word) then
          raise EAssetError.CreateFmt(SPackTooLarge, [Entries[I].Name]);
        P := @Item[0];
        PutNumber(P, Length(Entries[I].Name), 2);
        Index.WriteBuffer(Item, 2);
        Index.WriteBuffer(Entries[I].Name[1], Length(Entries[I].Name));
        P := @Item[0];
        PutNumber(P, QWord(Entries[I].Offset), 8);
        PutNumber(P, QWord(Entries[I].Size), 8);
        Index.WriteBuffer(Item, 16);
      end;
      if Index.Size > 0 then
      begin
        AssetXor(Index.Memory, Index.Size, IndexOffset, Key);
        Dest.WriteBuffer(Index.Memory^, Index.Size);
      end;
      Move(PackMagic, Header, SizeOf(PackMagic));
      P := @Header[4];
      PutNumber(P, PackVersion, 4);
      PutNumber(P, Length(Entries), 4);
      PutNumber(P, Index.Size, 4);
      PutNumber(P, QWord(IndexOffset), 8);
      AssetXor(@Header[0], PackHeaderSize, 0, Key);
      Dest.Position := 0;
      Dest.WriteBuffer(Header, PackHeaderSize);
    finally
      Index.Free;
      Dest.Free;
    end;
  except
    { A dat file which was not finished is not left behind }
    DeleteFile(FileName);
    raise;
  end;
  Result := Length(Entries);
end;

{ A program made for the Windows desktop has no console to write to, so
  there it says nothing and only its exit code tells what happened }

procedure Say(const S: string);
begin
  {$ifdef windows}
  if not IsConsole then
    Exit;
  {$endif}
  WriteLn(S);
end;

function AssetBuildRequested: Boolean;
var
  Folder, FileName: string;
  I, Found: Integer;
begin
  Found := 0;
  for I := 1 to ParamCount do
    if ParamStr(I) = AssetBuildSwitch then
    begin
      Found := I;
      Break;
    end;
  Result := Found > 0;
  if not Result then
    Exit;
  ExitCode := 1;
  if AssetKey = 0 then
  begin
    Say(SPackNoKey);
    Exit;
  end;
  if (Found < ParamCount) and (Copy(ParamStr(Found + 1), 1, 1) <> '-') then
    FileName := ExpandFileName(ParamStr(Found + 1))
  else
    FileName := AssetPackFileName;
  if not AssetFindFolder(Folder) then
  begin
    Say(Format(SPackNoFolder, [AssetFolder]));
    Exit;
  end;
  try
    I := AssetPackBuild(Folder, FileName, AssetKey);
    Say(Format(SPackWrote, [FileName, I]));
    ExitCode := 0;
  except
    on E: Exception do
      Say(E.Message);
  end;
end;

procedure PackFree;
var
  I: Integer;
begin
  for I := 0 to Length(PackEntries) - 1 do
    if PackEntries[I].Data <> nil then
      FreeMem(PackEntries[I].Data);
  PackEntries := nil;
  FreeAndNil(PackFile);
end;

initialization
  InitCriticalSection(PackLock);
finalization
  PackFree;
  DoneCriticalSection(PackLock);
end.
