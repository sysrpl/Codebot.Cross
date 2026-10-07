(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.render.truetype.txt> }
unit Codebot.Render.TrueType;

{ Codebot.Render.TrueType is a Pascal port of stb_truetype 1.26 by Sean
  Barrett, which is public domain or MIT licensed. The parts used by font
  rendering are ported: font loading, glyph lookup, metrics, kerning, glyph
  shapes for TrueType and CFF fonts, and the anti-aliased rasterizer. }

{$i render.inc}
{$pointermath on}
{$rangechecks off}
{$overflowchecks off}

interface

const
  STBTT_vmove = 1;
  STBTT_vline = 2;
  STBTT_vcurve = 3;
  STBTT_vcubic = 4;

  STBTT_PLATFORM_ID_UNICODE = 0;
  STBTT_PLATFORM_ID_MAC = 1;
  STBTT_PLATFORM_ID_ISO = 2;
  STBTT_PLATFORM_ID_MICROSOFT = 3;

  STBTT_MS_EID_SYMBOL = 0;
  STBTT_MS_EID_UNICODE_BMP = 1;
  STBTT_MS_EID_SHIFTJIS = 2;
  STBTT_MS_EID_UNICODE_FULL = 10;

type
  { Private structure used to parse font data }
  TStbttBuf = record
    Data: PByte;
    Cursor: Integer;
    Size: Integer;
  end;

  { TStbttFontInfo holds cached information about a font. It contains only
    value data, so it needs no cleanup, but the font data must remain valid
    while it is used. }
  PStbttFontInfo = ^TStbttFontInfo;
  TStbttFontInfo = record
    UserData: Pointer;
    { Pointer to the font file }
    Data: PByte;
    { Offset of start of font }
    FontStart: Integer;
    { Number of glyphs, needed for range checking }
    NumGlyphs: Integer;
    { Table locations as offset from start of the font }
    Loca, Head, Glyf, Hhea, Hmtx, Kern, Gpos, Svg: Integer;
    { A cmap mapping for our chosen character encoding }
    IndexMap: Integer;
    { Format needed to map from glyph index to glyph }
    IndexToLocFormat: Integer;
    { CFF font data }
    Cff: TStbttBuf;
    { The charstring index }
    CharStrings: TStbttBuf;
    { Global charstring subroutines index }
    GSubrs: TStbttBuf;
    { Private charstring subroutines index }
    Subrs: TStbttBuf;
    { Array of font dicts }
    FontDicts: TStbttBuf;
    { Map from glyph to fontdict }
    FdSelect: TStbttBuf;
  end;

  { TStbttVertex is a point in a glyph shape in unscaled font units }
  PStbttVertex = ^TStbttVertex;
  TStbttVertex = record
    X, Y, CX, CY, CX1, CY1: SmallInt;
    VType, Padding: Byte;
  end;

  { TStbttBitmap is a one channel bitmap rasterized into }
  TStbttBitmap = record
    W, H, Stride: Integer;
    Pixels: PByte;
  end;

{ Font loading }

{ Returns the offset of a font in a font file or collection, or -1 }
function stbtt_GetFontOffsetForIndex(Data: PByte; Index: Integer): Integer;
{ Returns the number of fonts in a font file or collection }
function stbtt_GetNumberOfFonts(Data: PByte): Integer;
{ Builds the cached information for a font at an offset into the font data }
function stbtt_InitFont(out Info: TStbttFontInfo; Data: PByte; FontStart: Integer): Boolean;

{ Character to glyph index conversion, returns 0 when a glyph is not found }
function stbtt_FindGlyphIndex(const Info: TStbttFontInfo; UnicodeCodepoint: Integer): Integer;

{ Metrics }

{ Computes a scale so that a font is Height pixels tall from ascent to descent }
function stbtt_ScaleForPixelHeight(const Info: TStbttFontInfo; Height: Single): Single;
{ Computes a scale so that the em size of a font is Pixels tall }
function stbtt_ScaleForMappingEmToPixels(const Info: TStbttFontInfo; Pixels: Single): Single;
{ Font vertical metrics in unscaled units, nil pointers are ignored }
procedure stbtt_GetFontVMetrics(const Info: TStbttFontInfo; Ascent, Descent, LineGap: PInteger);
{ The bounding box around all possible characters }
procedure stbtt_GetFontBoundingBox(const Info: TStbttFontInfo; out X0, Y0, X1, Y1: Integer);
{ Glyph horizontal metrics in unscaled units, nil pointers are ignored }
procedure stbtt_GetGlyphHMetrics(const Info: TStbttFontInfo; GlyphIndex: Integer; AdvanceWidth, LeftSideBearing: PInteger);
{ An additional amount to add to the advance value between two glyphs }
function stbtt_GetGlyphKernAdvance(const Info: TStbttFontInfo; G1, G2: Integer): Integer;
{ Gets the bounding box of the visible part of a glyph in unscaled units }
function stbtt_GetGlyphBox(const Info: TStbttFontInfo; GlyphIndex: Integer; X0, Y0, X1, Y1: PInteger): Boolean;

{ Glyph shapes }

{ Returns True if nothing is drawn for a glyph }
function stbtt_IsGlyphEmpty(const Info: TStbttFontInfo; GlyphIndex: Integer): Boolean;
{ Returns the number of vertices and the vertices of a glyph shape in
  unscaled units. Free the vertices with stbtt_FreeShape. }
function stbtt_GetGlyphShape(const Info: TStbttFontInfo; GlyphIndex: Integer; out Vertices: PStbttVertex): Integer;
{ Frees the vertices returned by stbtt_GetGlyphShape }
procedure stbtt_FreeShape(V: PStbttVertex);

{ Bitmap rendering }

{ Gets the bounding box of the bitmap centered around the glyph origin }
procedure stbtt_GetGlyphBitmapBox(const Info: TStbttFontInfo; Glyph: Integer;
  ScaleX, ScaleY: Single; IX0, IY0, IX1, IY1: PInteger);
{ The bounding box of the bitmap of a glyph at a scale and a subpixel shift }
procedure stbtt_GetGlyphBitmapBoxSubpixel(const Info: TStbttFontInfo; Glyph: Integer;
  ScaleX, ScaleY, ShiftX, ShiftY: Single; IX0, IY0, IX1, IY1: PInteger);
{ Rasterizes a shape with quadratic and cubic beziers into a bitmap }
procedure stbtt_Rasterize(var Bitmap: TStbttBitmap; FlatnessInPixels: Single; Vertices: PStbttVertex;
  NumVerts: Integer; ScaleX, ScaleY, ShiftX, ShiftY: Single; XOff, YOff: Integer; Invert: Boolean);
{ Renders a glyph into an output buffer you provide, clipped to its size }
procedure stbtt_MakeGlyphBitmap(const Info: TStbttFontInfo; Output: PByte; OutW, OutH, OutStride: Integer;
  ScaleX, ScaleY: Single; Glyph: Integer);
{ Draw the bitmap of a glyph at a scale and a subpixel shift }
procedure stbtt_MakeGlyphBitmapSubpixel(const Info: TStbttFontInfo; Output: PByte; OutW, OutH, OutStride: Integer;
  ScaleX, ScaleY, ShiftX, ShiftY: Single; Glyph: Integer);
{ Allocates and renders a glyph bitmap. Free it with stbtt_FreeBitmap. }
function stbtt_GetGlyphBitmapSubpixel(const Info: TStbttFontInfo; ScaleX, ScaleY, ShiftX, ShiftY: Single;
  Glyph: Integer; Width, Height, XOff, YOff: PInteger): PByte;
{ Frees a bitmap returned by stbtt_GetGlyphBitmapSubpixel }
procedure stbtt_FreeBitmap(Bitmap: PByte);

implementation

uses
  Math;

{ stbtt__buf helpers to parse data from file }

function stbtt__new_buf(P: Pointer; Size: PtrUInt): TStbttBuf;
begin
  Result.Data := P;
  Result.Size := Integer(Size);
  Result.Cursor := 0;
end;

function stbtt__buf_get8(var B: TStbttBuf): Byte;
begin
  if B.Cursor >= B.Size then
    Exit(0);
  Result := B.Data[B.Cursor];
  Inc(B.Cursor);
end;

function stbtt__buf_peek8(var B: TStbttBuf): Byte;
begin
  if B.Cursor >= B.Size then
    Exit(0);
  Result := B.Data[B.Cursor];
end;

procedure stbtt__buf_seek(var B: TStbttBuf; O: Integer);
begin
  if (O > B.Size) or (O < 0) then
    B.Cursor := B.Size
  else
    B.Cursor := O;
end;

procedure stbtt__buf_skip(var B: TStbttBuf; O: Integer);
begin
  stbtt__buf_seek(B, B.Cursor + O);
end;

function stbtt__buf_get(var B: TStbttBuf; N: Integer): UInt32;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to N - 1 do
    Result := (Result shl 8) or stbtt__buf_get8(B);
end;

function stbtt__buf_get16(var B: TStbttBuf): UInt32; inline;
begin
  Result := stbtt__buf_get(B, 2);
end;

function stbtt__buf_get32(var B: TStbttBuf): UInt32; inline;
begin
  Result := stbtt__buf_get(B, 4);
end;

function stbtt__buf_range(const B: TStbttBuf; O, S: Integer): TStbttBuf;
begin
  Result := stbtt__new_buf(nil, 0);
  if (O < 0) or (S < 0) or (O > B.Size) or (S > B.Size - O) then
    Exit;
  Result.Data := B.Data + O;
  Result.Size := S;
end;

function stbtt__cff_get_index(var B: TStbttBuf): TStbttBuf;
var
  Count, Start, OffSize: Integer;
begin
  Start := B.Cursor;
  Count := stbtt__buf_get16(B);
  if Count <> 0 then
  begin
    OffSize := stbtt__buf_get8(B);
    stbtt__buf_skip(B, OffSize * Count);
    stbtt__buf_skip(B, Integer(stbtt__buf_get(B, OffSize)) - 1);
  end;
  Result := stbtt__buf_range(B, Start, B.Cursor - Start);
end;

function stbtt__cff_int(var B: TStbttBuf): UInt32;
var
  B0: Integer;
begin
  B0 := stbtt__buf_get8(B);
  if (B0 >= 32) and (B0 <= 246) then
    Result := UInt32(B0 - 139)
  else if (B0 >= 247) and (B0 <= 250) then
    Result := UInt32((B0 - 247) * 256 + stbtt__buf_get8(B) + 108)
  else if (B0 >= 251) and (B0 <= 254) then
    Result := UInt32(-(B0 - 251) * 256 - stbtt__buf_get8(B) - 108)
  else if B0 = 28 then
    Result := stbtt__buf_get16(B)
  else if B0 = 29 then
    Result := stbtt__buf_get32(B)
  else
    Result := 0;
end;

procedure stbtt__cff_skip_operand(var B: TStbttBuf);
var
  V, B0: Integer;
begin
  B0 := stbtt__buf_peek8(B);
  if B0 = 30 then
  begin
    stbtt__buf_skip(B, 1);
    while B.Cursor < B.Size do
    begin
      V := stbtt__buf_get8(B);
      if ((V and $F) = $F) or ((V shr 4) = $F) then
        Break;
    end;
  end
  else
    stbtt__cff_int(B);
end;

function stbtt__dict_get(var B: TStbttBuf; Key: Integer): TStbttBuf;
var
  Start, Finish, Op: Integer;
begin
  stbtt__buf_seek(B, 0);
  while B.Cursor < B.Size do
  begin
    Start := B.Cursor;
    while stbtt__buf_peek8(B) >= 28 do
      stbtt__cff_skip_operand(B);
    Finish := B.Cursor;
    Op := stbtt__buf_get8(B);
    if Op = 12 then
      Op := stbtt__buf_get8(B) or $100;
    if Op = Key then
      Exit(stbtt__buf_range(B, Start, Finish - Start));
  end;
  Result := stbtt__buf_range(B, 0, 0);
end;

procedure stbtt__dict_get_ints(var B: TStbttBuf; Key, OutCount: Integer; Output: PUInt32);
var
  I: Integer;
  Operands: TStbttBuf;
begin
  Operands := stbtt__dict_get(B, Key);
  I := 0;
  while (I < OutCount) and (Operands.Cursor < Operands.Size) do
  begin
    Output[I] := stbtt__cff_int(Operands);
    Inc(I);
  end;
end;

function stbtt__cff_index_count(var B: TStbttBuf): Integer;
begin
  stbtt__buf_seek(B, 0);
  Result := stbtt__buf_get16(B);
end;

function stbtt__cff_index_get(B: TStbttBuf; I: Integer): TStbttBuf;
var
  Count, OffSize, Start, Finish: Integer;
begin
  stbtt__buf_seek(B, 0);
  Count := stbtt__buf_get16(B);
  OffSize := stbtt__buf_get8(B);
  stbtt__buf_skip(B, I * OffSize);
  Start := stbtt__buf_get(B, OffSize);
  Finish := stbtt__buf_get(B, OffSize);
  Result := stbtt__buf_range(B, 2 + (Count + 1) * OffSize + Start, Finish - Start);
end;

{ Accessors to parse data from file }

function ttBYTE(P: PByte): Byte; inline;
begin
  Result := P^;
end;

function ttCHAR(P: PByte): ShortInt; inline;
begin
  Result := ShortInt(P^);
end;

function ttUSHORT(P: PByte): Word; inline;
begin
  Result := Word((P[0] shl 8) or P[1]);
end;

function ttSHORT(P: PByte): SmallInt; inline;
begin
  Result := SmallInt(Word((P[0] shl 8) or P[1]));
end;

function ttULONG(P: PByte): UInt32; inline;
begin
  Result := (UInt32(P[0]) shl 24) or (UInt32(P[1]) shl 16) or (UInt32(P[2]) shl 8) or P[3];
end;

function ttLONG(P: PByte): Int32; inline;
begin
  Result := Int32(ttULONG(P));
end;

function stbtt_tag4(P: PByte; C0, C1, C2, C3: Byte): Boolean; inline;
begin
  Result := (P[0] = C0) and (P[1] = C1) and (P[2] = C2) and (P[3] = C3);
end;

function stbtt_tag(P: PByte; const Tag: string): Boolean; inline;
begin
  Result := stbtt_tag4(P, Ord(Tag[1]), Ord(Tag[2]), Ord(Tag[3]), Ord(Tag[4]));
end;

function stbtt__isfont(Font: PByte): Boolean;
begin
  Result :=
    stbtt_tag4(Font, Ord('1'), 0, 0, 0) or { TrueType 1 }
    stbtt_tag(Font, 'typ1') or { TrueType with type 1 font, not supported }
    stbtt_tag(Font, 'OTTO') or { OpenType with CFF }
    stbtt_tag4(Font, 0, 1, 0, 0) or { OpenType 1.0 }
    stbtt_tag(Font, 'true'); { Apple specification for TrueType fonts }
end;

function stbtt__find_table(Data: PByte; FontStart: UInt32; const Tag: string): UInt32;
var
  NumTables, I: Integer;
  TableDir, Loc: UInt32;
begin
  NumTables := ttUSHORT(Data + FontStart + 4);
  TableDir := FontStart + 12;
  for I := 0 to NumTables - 1 do
  begin
    Loc := TableDir + 16 * UInt32(I);
    if stbtt_tag(Data + Loc, Tag) then
      Exit(ttULONG(Data + Loc + 8));
  end;
  Result := 0;
end;

function stbtt_GetFontOffsetForIndex(Data: PByte; Index: Integer): Integer;
var
  N: Int32;
begin
  { If it's just a font, there's only one valid index }
  if stbtt__isfont(Data) then
  begin
    if Index = 0 then
      Exit(0);
    Exit(-1);
  end;
  { Check if it's a TTC }
  if stbtt_tag(Data, 'ttcf') then
    if (ttULONG(Data + 4) = $00010000) or (ttULONG(Data + 4) = $00020000) then
    begin
      N := ttLONG(Data + 8);
      if Index >= N then
        Exit(-1);
      Exit(Integer(ttULONG(Data + 12 + Index * 4)));
    end;
  Result := -1;
end;

function stbtt_GetNumberOfFonts(Data: PByte): Integer;
begin
  if stbtt__isfont(Data) then
    Exit(1);
  if stbtt_tag(Data, 'ttcf') then
    if (ttULONG(Data + 4) = $00010000) or (ttULONG(Data + 4) = $00020000) then
      Exit(ttLONG(Data + 8));
  Result := 0;
end;

function stbtt__get_subrs(Cff, FontDict: TStbttBuf): TStbttBuf;
var
  SubrsOff: UInt32;
  PrivateLoc: array[0..1] of UInt32;
  PDict: TStbttBuf;
begin
  SubrsOff := 0;
  PrivateLoc[0] := 0;
  PrivateLoc[1] := 0;
  stbtt__dict_get_ints(FontDict, 18, 2, @PrivateLoc[0]);
  if (PrivateLoc[1] = 0) or (PrivateLoc[0] = 0) then
    Exit(stbtt__new_buf(nil, 0));
  PDict := stbtt__buf_range(Cff, PrivateLoc[1], PrivateLoc[0]);
  stbtt__dict_get_ints(PDict, 19, 1, @SubrsOff);
  if SubrsOff = 0 then
    Exit(stbtt__new_buf(nil, 0));
  stbtt__buf_seek(Cff, PrivateLoc[1] + SubrsOff);
  Result := stbtt__cff_get_index(Cff);
end;

function stbtt_InitFont(out Info: TStbttFontInfo; Data: PByte; FontStart: Integer): Boolean;
var
  Cmap, T, CffTable, EncodingRecord: UInt32;
  I, NumTables: Integer;
  B, TopDict, TopDictIdx: TStbttBuf;
  CsType, CharStrings, FdArrayOff, FdSelectOff: UInt32;
begin
  FillChar(Info, SizeOf(Info), 0);
  Result := False;
  Info.Data := Data;
  Info.FontStart := FontStart;
  Info.Cff := stbtt__new_buf(nil, 0);

  Cmap := stbtt__find_table(Data, FontStart, 'cmap');       { required }
  Info.Loca := stbtt__find_table(Data, FontStart, 'loca'); { required }
  Info.Head := stbtt__find_table(Data, FontStart, 'head'); { required }
  Info.Glyf := stbtt__find_table(Data, FontStart, 'glyf'); { required }
  Info.Hhea := stbtt__find_table(Data, FontStart, 'hhea'); { required }
  Info.Hmtx := stbtt__find_table(Data, FontStart, 'hmtx'); { required }
  Info.Kern := stbtt__find_table(Data, FontStart, 'kern'); { not required }
  Info.Gpos := stbtt__find_table(Data, FontStart, 'GPOS'); { not required }

  if (Cmap = 0) or (Info.Head = 0) or (Info.Hhea = 0) or (Info.Hmtx = 0) then
    Exit;
  if Info.Glyf <> 0 then
  begin
    { Required for truetype }
    if Info.Loca = 0 then
      Exit;
  end
  else
  begin
    { Initialization for CFF / Type2 fonts (OTF) }
    CsType := 2;
    CharStrings := 0;
    FdArrayOff := 0;
    FdSelectOff := 0;
    CffTable := stbtt__find_table(Data, FontStart, 'CFF ');
    if CffTable = 0 then
      Exit;
    Info.FontDicts := stbtt__new_buf(nil, 0);
    Info.FdSelect := stbtt__new_buf(nil, 0);
    { This should use size from table (not 512MB) }
    Info.Cff := stbtt__new_buf(Data + CffTable, 512 * 1024 * 1024);
    B := Info.Cff;
    { Read the header }
    stbtt__buf_skip(B, 2);
    stbtt__buf_seek(B, stbtt__buf_get8(B)); { hdrsize }
    { The name INDEX could list multiple fonts, but we just use the first one }
    stbtt__cff_get_index(B); { name INDEX }
    TopDictIdx := stbtt__cff_get_index(B);
    TopDict := stbtt__cff_index_get(TopDictIdx, 0);
    stbtt__cff_get_index(B); { string INDEX }
    Info.GSubrs := stbtt__cff_get_index(B);
    stbtt__dict_get_ints(TopDict, 17, 1, @CharStrings);
    stbtt__dict_get_ints(TopDict, $100 or 6, 1, @CsType);
    stbtt__dict_get_ints(TopDict, $100 or 36, 1, @FdArrayOff);
    stbtt__dict_get_ints(TopDict, $100 or 37, 1, @FdSelectOff);
    Info.Subrs := stbtt__get_subrs(B, TopDict);
    { We only support Type 2 charstrings }
    if CsType <> 2 then
      Exit;
    if CharStrings = 0 then
      Exit;
    if FdArrayOff <> 0 then
    begin
      { Looks like a CID font }
      if FdSelectOff = 0 then
        Exit;
      stbtt__buf_seek(B, FdArrayOff);
      Info.FontDicts := stbtt__cff_get_index(B);
      Info.FdSelect := stbtt__buf_range(B, FdSelectOff, B.Size - Integer(FdSelectOff));
    end;
    stbtt__buf_seek(B, CharStrings);
    Info.CharStrings := stbtt__cff_get_index(B);
  end;

  T := stbtt__find_table(Data, FontStart, 'maxp');
  if T <> 0 then
    Info.NumGlyphs := ttUSHORT(Data + T + 4)
  else
    Info.NumGlyphs := $FFFF;

  Info.Svg := -1;

  { Find a cmap encoding table we understand now to avoid searching later }
  NumTables := ttUSHORT(Data + Cmap + 2);
  Info.IndexMap := 0;
  for I := 0 to NumTables - 1 do
  begin
    EncodingRecord := Cmap + 4 + 8 * UInt32(I);
    case ttUSHORT(Data + EncodingRecord) of
      STBTT_PLATFORM_ID_MICROSOFT:
        case ttUSHORT(Data + EncodingRecord + 2) of
          STBTT_MS_EID_UNICODE_BMP, STBTT_MS_EID_UNICODE_FULL:
            { MS/Unicode }
            Info.IndexMap := Cmap + ttULONG(Data + EncodingRecord + 4);
        end;
      STBTT_PLATFORM_ID_UNICODE:
        { Mac/iOS has these, all the encodingIDs are unicode }
        Info.IndexMap := Cmap + ttULONG(Data + EncodingRecord + 4);
    end;
  end;
  if Info.IndexMap = 0 then
    Exit;

  Info.IndexToLocFormat := ttUSHORT(Data + Info.Head + 50);
  Result := True;
end;

function stbtt_FindGlyphIndex(const Info: TStbttFontInfo; UnicodeCodepoint: Integer): Integer;
var
  Data: PByte;
  IndexMap: UInt32;
  Format: Word;
  Bytes: Int32;
  First, Count: UInt32;
  SegCount, SearchRange, EntrySelector, RangeShift, EndValue, Offset, Start, Item: Word;
  EndCount, Search, NGroups, StartChar, EndChar, StartGlyph: UInt32;
  Low, High, Mid: Int32;
begin
  Data := Info.Data;
  IndexMap := Info.IndexMap;
  Format := ttUSHORT(Data + IndexMap + 0);
  if Format = 0 then
  begin
    { Apple byte encoding }
    Bytes := ttUSHORT(Data + IndexMap + 2);
    if UnicodeCodepoint < Bytes - 6 then
      Exit(ttBYTE(Data + IndexMap + 6 + UnicodeCodepoint));
    Exit(0);
  end
  else if Format = 6 then
  begin
    First := ttUSHORT(Data + IndexMap + 6);
    Count := ttUSHORT(Data + IndexMap + 8);
    if (UInt32(UnicodeCodepoint) >= First) and (UInt32(UnicodeCodepoint) < First + Count) then
      Exit(ttUSHORT(Data + IndexMap + 10 + (UInt32(UnicodeCodepoint) - First) * 2));
    Exit(0);
  end
  else if Format = 2 then
  begin
    { High-byte mapping for japanese/chinese/korean is not supported }
    Exit(0);
  end
  else if Format = 4 then
  begin
    { Standard mapping for windows fonts: binary search collection of ranges }
    SegCount := ttUSHORT(Data + IndexMap + 6) shr 1;
    SearchRange := ttUSHORT(Data + IndexMap + 8) shr 1;
    EntrySelector := ttUSHORT(Data + IndexMap + 10);
    RangeShift := ttUSHORT(Data + IndexMap + 12) shr 1;
    { Do a binary search of the segments }
    EndCount := IndexMap + 14;
    Search := EndCount;
    if UnicodeCodepoint > $FFFF then
      Exit(0);
    { They lie from endCount .. endCount + segCount but searchRange is the
      nearest power of two }
    if UnicodeCodepoint >= ttUSHORT(Data + Search + RangeShift * 2) then
      Inc(Search, RangeShift * 2);
    { Now decrement to bias correctly to find smallest }
    Dec(Search, 2);
    while EntrySelector <> 0 do
    begin
      SearchRange := SearchRange shr 1;
      EndValue := ttUSHORT(Data + Search + SearchRange * 2);
      if UnicodeCodepoint > EndValue then
        Inc(Search, SearchRange * 2);
      Dec(EntrySelector);
    end;
    Inc(Search, 2);
    Item := Word((Search - EndCount) shr 1);
    Start := ttUSHORT(Data + IndexMap + 14 + SegCount * 2 + 2 + 2 * Item);
    if UnicodeCodepoint < Start then
      Exit(0);
    Offset := ttUSHORT(Data + IndexMap + 14 + SegCount * 6 + 2 + 2 * Item);
    if Offset = 0 then
      Exit(Word(UnicodeCodepoint + ttSHORT(Data + IndexMap + 14 + SegCount * 4 + 2 + 2 * Item)));
    Exit(ttUSHORT(Data + Offset + (UnicodeCodepoint - Start) * 2 + IndexMap + 14 + SegCount * 6 + 2 + 2 * Item));
  end
  else if (Format = 12) or (Format = 13) then
  begin
    NGroups := ttULONG(Data + IndexMap + 12);
    Low := 0;
    High := Int32(NGroups);
    { Binary search the right group }
    while Low < High do
    begin
      Mid := Low + ((High - Low) shr 1);
      StartChar := ttULONG(Data + IndexMap + 16 + UInt32(Mid) * 12);
      EndChar := ttULONG(Data + IndexMap + 16 + UInt32(Mid) * 12 + 4);
      if UInt32(UnicodeCodepoint) < StartChar then
        High := Mid
      else if UInt32(UnicodeCodepoint) > EndChar then
        Low := Mid + 1
      else
      begin
        StartGlyph := ttULONG(Data + IndexMap + 16 + UInt32(Mid) * 12 + 8);
        if Format = 12 then
          Exit(Integer(StartGlyph + UInt32(UnicodeCodepoint) - StartChar))
        else
          Exit(Integer(StartGlyph));
      end;
    end;
    Exit(0);
  end;
  Result := 0;
end;

procedure stbtt_setvertex(V: PStbttVertex; VType: Byte; X, Y, CX, CY: Int32); inline;
begin
  V.VType := VType;
  V.X := SmallInt(X);
  V.Y := SmallInt(Y);
  V.CX := SmallInt(CX);
  V.CY := SmallInt(CY);
end;

function stbtt__GetGlyfOffset(const Info: TStbttFontInfo; GlyphIndex: Integer): Integer;
var
  G1, G2: Integer;
begin
  if GlyphIndex >= Info.NumGlyphs then
    Exit(-1); { glyph index out of range }
  if Info.IndexToLocFormat >= 2 then
    Exit(-1); { unknown index->glyph map format }
  if Info.IndexToLocFormat = 0 then
  begin
    G1 := Info.Glyf + ttUSHORT(Info.Data + Info.Loca + GlyphIndex * 2) * 2;
    G2 := Info.Glyf + ttUSHORT(Info.Data + Info.Loca + GlyphIndex * 2 + 2) * 2;
  end
  else
  begin
    G1 := Info.Glyf + Integer(ttULONG(Info.Data + Info.Loca + GlyphIndex * 4));
    G2 := Info.Glyf + Integer(ttULONG(Info.Data + Info.Loca + GlyphIndex * 4 + 4));
  end;
  { If length is 0, return -1 }
  if G1 = G2 then
    Result := -1
  else
    Result := G1;
end;

function stbtt__GetGlyphInfoT2(const Info: TStbttFontInfo; GlyphIndex: Integer; X0, Y0, X1, Y1: PInteger): Integer; forward;

function stbtt_GetGlyphBox(const Info: TStbttFontInfo; GlyphIndex: Integer; X0, Y0, X1, Y1: PInteger): Boolean;
var
  G: Integer;
begin
  if Info.Cff.Size <> 0 then
    stbtt__GetGlyphInfoT2(Info, GlyphIndex, X0, Y0, X1, Y1)
  else
  begin
    G := stbtt__GetGlyfOffset(Info, GlyphIndex);
    if G < 0 then
      Exit(False);
    if X0 <> nil then
      X0^ := ttSHORT(Info.Data + G + 2);
    if Y0 <> nil then
      Y0^ := ttSHORT(Info.Data + G + 4);
    if X1 <> nil then
      X1^ := ttSHORT(Info.Data + G + 6);
    if Y1 <> nil then
      Y1^ := ttSHORT(Info.Data + G + 8);
  end;
  Result := True;
end;

function stbtt_IsGlyphEmpty(const Info: TStbttFontInfo; GlyphIndex: Integer): Boolean;
var
  G: Integer;
begin
  if Info.Cff.Size <> 0 then
    Exit(stbtt__GetGlyphInfoT2(Info, GlyphIndex, nil, nil, nil, nil) = 0);
  G := stbtt__GetGlyfOffset(Info, GlyphIndex);
  if G < 0 then
    Exit(True);
  Result := ttSHORT(Info.Data + G) = 0;
end;

function stbtt__close_shape(Vertices: PStbttVertex; NumVertices: Integer; WasOff, StartOff: Boolean;
  SX, SY, SCX, SCY, CX, CY: Int32): Integer;
begin
  if StartOff then
  begin
    if WasOff then
    begin
      stbtt_setvertex(@Vertices[NumVertices], STBTT_vcurve, SarLongint(CX + SCX, 1), SarLongint(CY + SCY, 1), CX, CY);
      Inc(NumVertices);
    end;
    stbtt_setvertex(@Vertices[NumVertices], STBTT_vcurve, SX, SY, SCX, SCY);
    Inc(NumVertices);
  end
  else
  begin
    if WasOff then
      stbtt_setvertex(@Vertices[NumVertices], STBTT_vcurve, SX, SY, CX, CY)
    else
      stbtt_setvertex(@Vertices[NumVertices], STBTT_vline, SX, SY, 0, 0);
    Inc(NumVertices);
  end;
  Result := NumVertices;
end;

function stbtt__GetGlyphShapeTT(const Info: TStbttFontInfo; GlyphIndex: Integer; out PVertices: PStbttVertex): Integer;
var
  NumberOfContours: SmallInt;
  EndPtsOfContours, Data, Points, Comp: PByte;
  Vertices, CompVerts, Tmp, V: PStbttVertex;
  NumVertices, G: Integer;
  Flags, FlagCount: Byte;
  Ins, I, J, M, N, NextMove, Off: Int32;
  WasOff, StartOff, More: Boolean;
  X, Y, CX, CY, SX, SY, SCX, SCY: Int32;
  DX, DY: SmallInt;
  CompFlags, GIdx: Word;
  CompNumVerts, K: Integer;
  Mtx: array[0..5] of Single;
  MS, NS: Single;
  VX, VY: SmallInt;
begin
  Data := Info.Data;
  Vertices := nil;
  NumVertices := 0;
  G := stbtt__GetGlyfOffset(Info, GlyphIndex);
  PVertices := nil;
  if G < 0 then
    Exit(0);
  NumberOfContours := ttSHORT(Data + G);
  if NumberOfContours > 0 then
  begin
    Flags := 0;
    J := 0;
    WasOff := False;
    StartOff := False;
    EndPtsOfContours := Data + G + 10;
    Ins := ttUSHORT(Data + G + 10 + NumberOfContours * 2);
    Points := Data + G + 10 + NumberOfContours * 2 + 2 + Ins;
    N := 1 + ttUSHORT(EndPtsOfContours + NumberOfContours * 2 - 2);
    { A loose bound on how many vertices we might need }
    M := N + 2 * NumberOfContours;
    Vertices := GetMem(M * SizeOf(TStbttVertex));
    if Vertices = nil then
      Exit(0);
    NextMove := 0;
    FlagCount := 0;
    { In first pass, we load uninterpreted data into the allocated array
      above, shifted to the end of the array so we won't overwrite it when
      we create our final data starting from the front }
    Off := M - N;
    { First load flags }
    for I := 0 to N - 1 do
    begin
      if FlagCount = 0 then
      begin
        Flags := Points^;
        Inc(Points);
        if Flags and 8 <> 0 then
        begin
          FlagCount := Points^;
          Inc(Points);
        end;
      end
      else
        Dec(FlagCount);
      Vertices[Off + I].VType := Flags;
    end;
    { Now load x coordinates }
    X := 0;
    for I := 0 to N - 1 do
    begin
      Flags := Vertices[Off + I].VType;
      if Flags and 2 <> 0 then
      begin
        DX := Points^;
        Inc(Points);
        if Flags and 16 <> 0 then
          Inc(X, DX)
        else
          Dec(X, DX);
      end
      else if Flags and 16 = 0 then
      begin
        X := X + SmallInt(Word(Points[0] * 256 + Points[1]));
        Inc(Points, 2);
      end;
      Vertices[Off + I].X := SmallInt(X);
    end;
    { Now load y coordinates }
    Y := 0;
    for I := 0 to N - 1 do
    begin
      Flags := Vertices[Off + I].VType;
      if Flags and 4 <> 0 then
      begin
        DY := Points^;
        Inc(Points);
        if Flags and 32 <> 0 then
          Inc(Y, DY)
        else
          Dec(Y, DY);
      end
      else if Flags and 32 = 0 then
      begin
        Y := Y + SmallInt(Word(Points[0] * 256 + Points[1]));
        Inc(Points, 2);
      end;
      Vertices[Off + I].Y := SmallInt(Y);
    end;
    { Now convert them to our format }
    NumVertices := 0;
    SX := 0; SY := 0; CX := 0; CY := 0; SCX := 0; SCY := 0;
    I := 0;
    while I < N do
    begin
      Flags := Vertices[Off + I].VType;
      X := Vertices[Off + I].X;
      Y := Vertices[Off + I].Y;
      if NextMove = I then
      begin
        if I <> 0 then
          NumVertices := stbtt__close_shape(Vertices, NumVertices, WasOff, StartOff, SX, SY, SCX, SCY, CX, CY);
        { Now start the new one }
        StartOff := Flags and 1 = 0;
        if StartOff then
        begin
          { If we start off with an off-curve point, then when we need to find
            a point on the curve where we can start, and we need to save some
            state for when we wraparound }
          SCX := X;
          SCY := Y;
          if Vertices[Off + I + 1].VType and 1 = 0 then
          begin
            { Next point is also a curve point, so interpolate an on-point curve }
            SX := SarLongint(X + Int32(Vertices[Off + I + 1].X), 1);
            SY := SarLongint(Y + Int32(Vertices[Off + I + 1].Y), 1);
          end
          else
          begin
            { Otherwise just use the next point as our start point }
            SX := Vertices[Off + I + 1].X;
            SY := Vertices[Off + I + 1].Y;
            { We're using point i+1 as the starting point, so skip it }
            Inc(I);
          end;
        end
        else
        begin
          SX := X;
          SY := Y;
        end;
        stbtt_setvertex(@Vertices[NumVertices], STBTT_vmove, SX, SY, 0, 0);
        Inc(NumVertices);
        WasOff := False;
        NextMove := 1 + ttUSHORT(EndPtsOfContours + J * 2);
        Inc(J);
      end
      else if Flags and 1 = 0 then
      begin
        { If it's a curve, two off-curve control points in a row means
          interpolate an on-curve midpoint }
        if WasOff then
        begin
          stbtt_setvertex(@Vertices[NumVertices], STBTT_vcurve, SarLongint(CX + X, 1), SarLongint(CY + Y, 1), CX, CY);
          Inc(NumVertices);
        end;
        CX := X;
        CY := Y;
        WasOff := True;
      end
      else
      begin
        if WasOff then
          stbtt_setvertex(@Vertices[NumVertices], STBTT_vcurve, X, Y, CX, CY)
        else
          stbtt_setvertex(@Vertices[NumVertices], STBTT_vline, X, Y, 0, 0);
        Inc(NumVertices);
        WasOff := False;
      end;
      Inc(I);
    end;
    NumVertices := stbtt__close_shape(Vertices, NumVertices, WasOff, StartOff, SX, SY, SCX, SCY, CX, CY);
  end
  else if NumberOfContours < 0 then
  begin
    { Compound shapes }
    More := True;
    Comp := Data + G + 10;
    NumVertices := 0;
    Vertices := nil;
    while More do
    begin
      CompNumVerts := 0;
      CompVerts := nil;
      Mtx[0] := 1; Mtx[1] := 0; Mtx[2] := 0; Mtx[3] := 1; Mtx[4] := 0; Mtx[5] := 0;
      CompFlags := Word(ttSHORT(Comp));
      Inc(Comp, 2);
      GIdx := Word(ttSHORT(Comp));
      Inc(Comp, 2);
      if CompFlags and 2 <> 0 then
      begin
        { XY values }
        if CompFlags and 1 <> 0 then
        begin
          { Shorts }
          Mtx[4] := ttSHORT(Comp); Inc(Comp, 2);
          Mtx[5] := ttSHORT(Comp); Inc(Comp, 2);
        end
        else
        begin
          Mtx[4] := ttCHAR(Comp); Inc(Comp);
          Mtx[5] := ttCHAR(Comp); Inc(Comp);
        end;
      end;
      { Matching points are not handled }
      if CompFlags and (1 shl 3) <> 0 then
      begin
        { WE_HAVE_A_SCALE }
        Mtx[0] := ttSHORT(Comp) / 16384.0;
        Mtx[3] := Mtx[0];
        Inc(Comp, 2);
        Mtx[1] := 0;
        Mtx[2] := 0;
      end
      else if CompFlags and (1 shl 6) <> 0 then
      begin
        { WE_HAVE_AN_X_AND_YSCALE }
        Mtx[0] := ttSHORT(Comp) / 16384.0; Inc(Comp, 2);
        Mtx[1] := 0;
        Mtx[2] := 0;
        Mtx[3] := ttSHORT(Comp) / 16384.0; Inc(Comp, 2);
      end
      else if CompFlags and (1 shl 7) <> 0 then
      begin
        { WE_HAVE_A_TWO_BY_TWO }
        Mtx[0] := ttSHORT(Comp) / 16384.0; Inc(Comp, 2);
        Mtx[1] := ttSHORT(Comp) / 16384.0; Inc(Comp, 2);
        Mtx[2] := ttSHORT(Comp) / 16384.0; Inc(Comp, 2);
        Mtx[3] := ttSHORT(Comp) / 16384.0; Inc(Comp, 2);
      end;
      { Find transformation scales }
      MS := Sqrt(Mtx[0] * Mtx[0] + Mtx[1] * Mtx[1]);
      NS := Sqrt(Mtx[2] * Mtx[2] + Mtx[3] * Mtx[3]);
      { Get indexed glyph }
      CompNumVerts := stbtt_GetGlyphShape(Info, GIdx, CompVerts);
      if CompNumVerts > 0 then
      begin
        { Transform vertices }
        for K := 0 to CompNumVerts - 1 do
        begin
          V := @CompVerts[K];
          VX := V.X;
          VY := V.Y;
          V.X := SmallInt(Trunc(MS * (Mtx[0] * VX + Mtx[2] * VY + Mtx[4])));
          V.Y := SmallInt(Trunc(NS * (Mtx[1] * VX + Mtx[3] * VY + Mtx[5])));
          VX := V.CX;
          VY := V.CY;
          V.CX := SmallInt(Trunc(MS * (Mtx[0] * VX + Mtx[2] * VY + Mtx[4])));
          V.CY := SmallInt(Trunc(NS * (Mtx[1] * VX + Mtx[3] * VY + Mtx[5])));
        end;
        { Append vertices }
        Tmp := GetMem((NumVertices + CompNumVerts) * SizeOf(TStbttVertex));
        if Tmp = nil then
        begin
          if Vertices <> nil then
            FreeMem(Vertices);
          if CompVerts <> nil then
            FreeMem(CompVerts);
          Exit(0);
        end;
        if NumVertices > 0 then
          Move(Vertices^, Tmp^, NumVertices * SizeOf(TStbttVertex));
        Move(CompVerts^, Tmp[NumVertices], CompNumVerts * SizeOf(TStbttVertex));
        if Vertices <> nil then
          FreeMem(Vertices);
        Vertices := Tmp;
        FreeMem(CompVerts);
        Inc(NumVertices, CompNumVerts);
      end;
      { More components? }
      More := CompFlags and (1 shl 5) <> 0;
    end;
  end;
  PVertices := Vertices;
  Result := NumVertices;
end;

{ CFF Type 2 charstring support }

type
  TStbttCsCtx = record
    Bounds: Boolean;
    Started: Boolean;
    FirstX, FirstY: Single;
    X, Y: Single;
    MinX, MaxX, MinY, MaxY: Int32;
    PVertices: PStbttVertex;
    NumVertices: Integer;
  end;

procedure stbtt__csctx_init(out C: TStbttCsCtx; Bounds: Boolean);
begin
  FillChar(C, SizeOf(C), 0);
  C.Bounds := Bounds;
end;

procedure stbtt__track_vertex(var C: TStbttCsCtx; X, Y: Int32);
begin
  if (X > C.MaxX) or (not C.Started) then
    C.MaxX := X;
  if (Y > C.MaxY) or (not C.Started) then
    C.MaxY := Y;
  if (X < C.MinX) or (not C.Started) then
    C.MinX := X;
  if (Y < C.MinY) or (not C.Started) then
    C.MinY := Y;
  C.Started := True;
end;

procedure stbtt__csctx_v(var C: TStbttCsCtx; VType: Byte; X, Y, CX, CY, CX1, CY1: Int32);
begin
  if C.Bounds then
  begin
    stbtt__track_vertex(C, X, Y);
    if VType = STBTT_vcubic then
    begin
      stbtt__track_vertex(C, CX, CY);
      stbtt__track_vertex(C, CX1, CY1);
    end;
  end
  else
  begin
    stbtt_setvertex(@C.PVertices[C.NumVertices], VType, X, Y, CX, CY);
    C.PVertices[C.NumVertices].CX1 := SmallInt(CX1);
    C.PVertices[C.NumVertices].CY1 := SmallInt(CY1);
  end;
  Inc(C.NumVertices);
end;

procedure stbtt__csctx_close_shape(var C: TStbttCsCtx);
begin
  if (C.FirstX <> C.X) or (C.FirstY <> C.Y) then
    stbtt__csctx_v(C, STBTT_vline, Trunc(C.FirstX), Trunc(C.FirstY), 0, 0, 0, 0);
end;

procedure stbtt__csctx_rmove_to(var C: TStbttCsCtx; DX, DY: Single);
begin
  stbtt__csctx_close_shape(C);
  C.X := C.X + DX;
  C.FirstX := C.X;
  C.Y := C.Y + DY;
  C.FirstY := C.Y;
  stbtt__csctx_v(C, STBTT_vmove, Trunc(C.X), Trunc(C.Y), 0, 0, 0, 0);
end;

procedure stbtt__csctx_rline_to(var C: TStbttCsCtx; DX, DY: Single);
begin
  C.X := C.X + DX;
  C.Y := C.Y + DY;
  stbtt__csctx_v(C, STBTT_vline, Trunc(C.X), Trunc(C.Y), 0, 0, 0, 0);
end;

procedure stbtt__csctx_rccurve_to(var C: TStbttCsCtx; DX1, DY1, DX2, DY2, DX3, DY3: Single);
var
  CX1, CY1, CX2, CY2: Single;
begin
  CX1 := C.X + DX1;
  CY1 := C.Y + DY1;
  CX2 := CX1 + DX2;
  CY2 := CY1 + DY2;
  C.X := CX2 + DX3;
  C.Y := CY2 + DY3;
  stbtt__csctx_v(C, STBTT_vcubic, Trunc(C.X), Trunc(C.Y), Trunc(CX1), Trunc(CY1), Trunc(CX2), Trunc(CY2));
end;

function stbtt__get_subr(Idx: TStbttBuf; N: Integer): TStbttBuf;
var
  Count, Bias: Integer;
begin
  Count := stbtt__cff_index_count(Idx);
  Bias := 107;
  if Count >= 33900 then
    Bias := 32768
  else if Count >= 1240 then
    Bias := 1131;
  Inc(N, Bias);
  if (N < 0) or (N >= Count) then
    Exit(stbtt__new_buf(nil, 0));
  Result := stbtt__cff_index_get(Idx, N);
end;

function stbtt__cid_get_glyph_subrs(const Info: TStbttFontInfo; GlyphIndex: Integer): TStbttBuf;
var
  FdSelect: TStbttBuf;
  NRanges, Start, Finish, V, Fmt, FdSelector, I: Integer;
begin
  FdSelect := Info.FdSelect;
  FdSelector := -1;
  stbtt__buf_seek(FdSelect, 0);
  Fmt := stbtt__buf_get8(FdSelect);
  if Fmt = 0 then
  begin
    stbtt__buf_skip(FdSelect, GlyphIndex);
    FdSelector := stbtt__buf_get8(FdSelect);
  end
  else if Fmt = 3 then
  begin
    NRanges := stbtt__buf_get16(FdSelect);
    Start := stbtt__buf_get16(FdSelect);
    for I := 0 to NRanges - 1 do
    begin
      V := stbtt__buf_get8(FdSelect);
      Finish := stbtt__buf_get16(FdSelect);
      if (GlyphIndex >= Start) and (GlyphIndex < Finish) then
      begin
        FdSelector := V;
        Break;
      end;
      Start := Finish;
    end;
  end;
  if FdSelector = -1 then
    Exit(stbtt__new_buf(nil, 0));
  Result := stbtt__get_subrs(Info.Cff, stbtt__cff_index_get(Info.FontDicts, FdSelector));
end;

function stbtt__run_charstring(const Info: TStbttFontInfo; GlyphIndex: Integer; var C: TStbttCsCtx): Boolean;
var
  InHeader, HasSubrs, ClearStack, Alternate: Boolean;
  MaskBits, SubrStackHeight, SP, V, I, B0, B1: Integer;
  S: array[0..47] of Single;
  SubrStack: array[0..9] of TStbttBuf;
  Subrs, B: TStbttBuf;
  F, DX1, DX2, DX3, DX4, DX5, DX6, DY1, DY2, DY3, DY4, DY5, DY6, DX, DY: Single;

  function Last: Single;
  begin
    if SP - I = 5 then
      Result := S[I + 4]
    else
      Result := 0;
  end;

begin
  Result := False;
  FillChar(S, SizeOf(S), 0);
  InHeader := True;
  MaskBits := 0;
  SubrStackHeight := 0;
  SP := 0;
  HasSubrs := False;
  Subrs := Info.Subrs;
  { This currently ignores the initial width value, which isn't needed if we
    have hmtx }
  B := stbtt__cff_index_get(Info.CharStrings, GlyphIndex);
  while B.Cursor < B.Size do
  begin
    I := 0;
    ClearStack := True;
    B0 := stbtt__buf_get8(B);
    case B0 of
      $13, $14:
        begin
          { hintmask, cntrmask }
          if InHeader then
            Inc(MaskBits, SP div 2); { implicit "vstem" }
          InHeader := False;
          stbtt__buf_skip(B, (MaskBits + 7) div 8);
        end;
      $01, $03, $12, $17:
        { hstem, vstem, hstemhm, vstemhm }
        Inc(MaskBits, SP div 2);
      $15:
        begin
          { rmoveto }
          InHeader := False;
          if SP < 2 then
            Exit;
          stbtt__csctx_rmove_to(C, S[SP - 2], S[SP - 1]);
        end;
      $04:
        begin
          { vmoveto }
          InHeader := False;
          if SP < 1 then
            Exit;
          stbtt__csctx_rmove_to(C, 0, S[SP - 1]);
        end;
      $16:
        begin
          { hmoveto }
          InHeader := False;
          if SP < 1 then
            Exit;
          stbtt__csctx_rmove_to(C, S[SP - 1], 0);
        end;
      $05:
        begin
          { rlineto }
          if SP < 2 then
            Exit;
          while I + 1 < SP do
          begin
            stbtt__csctx_rline_to(C, S[I], S[I + 1]);
            Inc(I, 2);
          end;
        end;
      $06, $07:
        begin
          { hlineto and vlineto alternate horizontal and vertical starting
            from a different place }
          if SP < 1 then
            Exit;
          Alternate := B0 = $07;
          while I < SP do
          begin
            if Alternate then
              stbtt__csctx_rline_to(C, 0, S[I])
            else
              stbtt__csctx_rline_to(C, S[I], 0);
            Inc(I);
            Alternate := not Alternate;
          end;
        end;
      $1E, $1F:
        begin
          { vhcurveto and hvcurveto alternate vertical and horizontal starting
            from a different place }
          if SP < 4 then
            Exit;
          Alternate := B0 = $1F;
          while I + 3 < SP do
          begin
            if Alternate then
              stbtt__csctx_rccurve_to(C, S[I], 0, S[I + 1], S[I + 2], Last, S[I + 3])
            else
              stbtt__csctx_rccurve_to(C, 0, S[I], S[I + 1], S[I + 2], S[I + 3], Last);
            Inc(I, 4);
            Alternate := not Alternate;
          end;
        end;
      $08:
        begin
          { rrcurveto }
          if SP < 6 then
            Exit;
          while I + 5 < SP do
          begin
            stbtt__csctx_rccurve_to(C, S[I], S[I + 1], S[I + 2], S[I + 3], S[I + 4], S[I + 5]);
            Inc(I, 6);
          end;
        end;
      $18:
        begin
          { rcurveline }
          if SP < 8 then
            Exit;
          while I + 5 < SP - 2 do
          begin
            stbtt__csctx_rccurve_to(C, S[I], S[I + 1], S[I + 2], S[I + 3], S[I + 4], S[I + 5]);
            Inc(I, 6);
          end;
          if I + 1 >= SP then
            Exit;
          stbtt__csctx_rline_to(C, S[I], S[I + 1]);
        end;
      $19:
        begin
          { rlinecurve }
          if SP < 8 then
            Exit;
          while I + 1 < SP - 6 do
          begin
            stbtt__csctx_rline_to(C, S[I], S[I + 1]);
            Inc(I, 2);
          end;
          if I + 5 >= SP then
            Exit;
          stbtt__csctx_rccurve_to(C, S[I], S[I + 1], S[I + 2], S[I + 3], S[I + 4], S[I + 5]);
        end;
      $1A, $1B:
        begin
          { vvcurveto, hhcurveto }
          if SP < 4 then
            Exit;
          F := 0;
          if SP and 1 <> 0 then
          begin
            F := S[I];
            Inc(I);
          end;
          while I + 3 < SP do
          begin
            if B0 = $1B then
              stbtt__csctx_rccurve_to(C, S[I], F, S[I + 1], S[I + 2], S[I + 3], 0)
            else
              stbtt__csctx_rccurve_to(C, F, S[I], S[I + 1], S[I + 2], 0, S[I + 3]);
            F := 0;
            Inc(I, 4);
          end;
        end;
      $0A, $1D:
        begin
          { callsubr, callgsubr }
          if (B0 = $0A) and (not HasSubrs) then
          begin
            if Info.FdSelect.Size <> 0 then
              Subrs := stbtt__cid_get_glyph_subrs(Info, GlyphIndex);
            HasSubrs := True;
          end;
          if SP < 1 then
            Exit;
          Dec(SP);
          V := Trunc(S[SP]);
          if SubrStackHeight >= 10 then
            Exit; { recursion limit }
          SubrStack[SubrStackHeight] := B;
          Inc(SubrStackHeight);
          if B0 = $0A then
            B := stbtt__get_subr(Subrs, V)
          else
            B := stbtt__get_subr(Info.GSubrs, V);
          if B.Size = 0 then
            Exit; { subr not found }
          B.Cursor := 0;
          ClearStack := False;
        end;
      $0B:
        begin
          { return }
          if SubrStackHeight <= 0 then
            Exit;
          Dec(SubrStackHeight);
          B := SubrStack[SubrStackHeight];
          ClearStack := False;
        end;
      $0E:
        begin
          { endchar }
          stbtt__csctx_close_shape(C);
          Exit(True);
        end;
      $0C:
        begin
          { Two-byte escape. These "flex" implementations ignore the flex-depth
            and resolution, and always draw beziers }
          B1 := stbtt__buf_get8(B);
          case B1 of
            $22:
              begin
                { hflex }
                if SP < 7 then
                  Exit;
                DX1 := S[0]; DX2 := S[1]; DY2 := S[2]; DX3 := S[3];
                DX4 := S[4]; DX5 := S[5]; DX6 := S[6];
                stbtt__csctx_rccurve_to(C, DX1, 0, DX2, DY2, DX3, 0);
                stbtt__csctx_rccurve_to(C, DX4, 0, DX5, -DY2, DX6, 0);
              end;
            $23:
              begin
                { flex }
                if SP < 13 then
                  Exit;
                DX1 := S[0]; DY1 := S[1]; DX2 := S[2]; DY2 := S[3];
                DX3 := S[4]; DY3 := S[5]; DX4 := S[6]; DY4 := S[7];
                DX5 := S[8]; DY5 := S[9]; DX6 := S[10]; DY6 := S[11];
                stbtt__csctx_rccurve_to(C, DX1, DY1, DX2, DY2, DX3, DY3);
                stbtt__csctx_rccurve_to(C, DX4, DY4, DX5, DY5, DX6, DY6);
              end;
            $24:
              begin
                { hflex1 }
                if SP < 9 then
                  Exit;
                DX1 := S[0]; DY1 := S[1]; DX2 := S[2]; DY2 := S[3];
                DX3 := S[4]; DX4 := S[5]; DX5 := S[6]; DY5 := S[7];
                DX6 := S[8];
                stbtt__csctx_rccurve_to(C, DX1, DY1, DX2, DY2, DX3, 0);
                stbtt__csctx_rccurve_to(C, DX4, 0, DX5, DY5, DX6, -(DY1 + DY2 + DY5));
              end;
            $25:
              begin
                { flex1 }
                if SP < 11 then
                  Exit;
                DX1 := S[0]; DY1 := S[1]; DX2 := S[2]; DY2 := S[3];
                DX3 := S[4]; DY3 := S[5]; DX4 := S[6]; DY4 := S[7];
                DX5 := S[8]; DY5 := S[9];
                DX6 := S[10];
                DY6 := S[10];
                DX := DX1 + DX2 + DX3 + DX4 + DX5;
                DY := DY1 + DY2 + DY3 + DY4 + DY5;
                if Abs(DX) > Abs(DY) then
                  DY6 := -DY
                else
                  DX6 := -DX;
                stbtt__csctx_rccurve_to(C, DX1, DY1, DX2, DY2, DX3, DY3);
                stbtt__csctx_rccurve_to(C, DX4, DY4, DX5, DY5, DX6, DY6);
              end;
          else
            Exit; { unimplemented }
          end;
        end;
    else
      if (B0 <> 255) and (B0 <> 28) and ((B0 < 32) or (B0 > 254)) then
        Exit; { reserved operator }
      { Push immediate }
      if B0 = 255 then
        F := Int32(stbtt__buf_get32(B)) / $10000
      else
      begin
        stbtt__buf_skip(B, -1);
        F := SmallInt(stbtt__cff_int(B));
      end;
      if SP >= 48 then
        Exit; { push stack overflow }
      S[SP] := F;
      Inc(SP);
      ClearStack := False;
    end;
    if ClearStack then
      SP := 0;
  end;
  { No endchar }
end;

function stbtt__GetGlyphShapeT2(const Info: TStbttFontInfo; GlyphIndex: Integer; out PVertices: PStbttVertex): Integer;
var
  CountCtx, OutputCtx: TStbttCsCtx;
begin
  { Runs the charstring twice, once to count and once to output (to avoid realloc) }
  stbtt__csctx_init(CountCtx, True);
  stbtt__csctx_init(OutputCtx, False);
  if stbtt__run_charstring(Info, GlyphIndex, CountCtx) then
  begin
    PVertices := GetMem(CountCtx.NumVertices * SizeOf(TStbttVertex));
    OutputCtx.PVertices := PVertices;
    if stbtt__run_charstring(Info, GlyphIndex, OutputCtx) then
      Exit(OutputCtx.NumVertices);
    FreeMem(PVertices);
  end;
  PVertices := nil;
  Result := 0;
end;

function stbtt__GetGlyphInfoT2(const Info: TStbttFontInfo; GlyphIndex: Integer; X0, Y0, X1, Y1: PInteger): Integer;
var
  C: TStbttCsCtx;
  R: Boolean;
begin
  stbtt__csctx_init(C, True);
  R := stbtt__run_charstring(Info, GlyphIndex, C);
  if X0 <> nil then
    if R then X0^ := C.MinX else X0^ := 0;
  if Y0 <> nil then
    if R then Y0^ := C.MinY else Y0^ := 0;
  if X1 <> nil then
    if R then X1^ := C.MaxX else X1^ := 0;
  if Y1 <> nil then
    if R then Y1^ := C.MaxY else Y1^ := 0;
  if R then
    Result := C.NumVertices
  else
    Result := 0;
end;

function stbtt_GetGlyphShape(const Info: TStbttFontInfo; GlyphIndex: Integer; out Vertices: PStbttVertex): Integer;
begin
  if Info.Cff.Size = 0 then
    Result := stbtt__GetGlyphShapeTT(Info, GlyphIndex, Vertices)
  else
    Result := stbtt__GetGlyphShapeT2(Info, GlyphIndex, Vertices);
end;

{ Metrics and kerning }

procedure stbtt_GetGlyphHMetrics(const Info: TStbttFontInfo; GlyphIndex: Integer; AdvanceWidth, LeftSideBearing: PInteger);
var
  NumOfLongHorMetrics: Word;
begin
  NumOfLongHorMetrics := ttUSHORT(Info.Data + Info.Hhea + 34);
  if GlyphIndex < NumOfLongHorMetrics then
  begin
    if AdvanceWidth <> nil then
      AdvanceWidth^ := ttSHORT(Info.Data + Info.Hmtx + 4 * GlyphIndex);
    if LeftSideBearing <> nil then
      LeftSideBearing^ := ttSHORT(Info.Data + Info.Hmtx + 4 * GlyphIndex + 2);
  end
  else
  begin
    if AdvanceWidth <> nil then
      AdvanceWidth^ := ttSHORT(Info.Data + Info.Hmtx + 4 * (NumOfLongHorMetrics - 1));
    if LeftSideBearing <> nil then
      LeftSideBearing^ := ttSHORT(Info.Data + Info.Hmtx + 4 * NumOfLongHorMetrics + 2 * (GlyphIndex - NumOfLongHorMetrics));
  end;
end;

function stbtt__GetGlyphKernInfoAdvance(const Info: TStbttFontInfo; Glyph1, Glyph2: Integer): Integer;
var
  Data: PByte;
  Needle, Straw: UInt32;
  L, R, M: Integer;
begin
  Data := Info.Data + Info.Kern;
  { We only look at the first table. It must be 'horizontal' and format 0 }
  if Info.Kern = 0 then
    Exit(0);
  if ttUSHORT(Data + 2) < 1 then
    Exit(0); { number of tables, need at least 1 }
  if ttUSHORT(Data + 8) <> 1 then
    Exit(0); { horizontal flag must be set in format }
  L := 0;
  R := ttUSHORT(Data + 10) - 1;
  Needle := (UInt32(Glyph1) shl 16) or UInt32(Glyph2);
  while L <= R do
  begin
    M := (L + R) shr 1;
    Straw := ttULONG(Data + 18 + (M * 6)); { note: unaligned read }
    if Needle < Straw then
      R := M - 1
    else if Needle > Straw then
      L := M + 1
    else
      Exit(ttSHORT(Data + 22 + (M * 6)));
  end;
  Result := 0;
end;

function stbtt__GetCoverageIndex(CoverageTable: PByte; Glyph: Integer): Int32;
var
  GlyphCount, RangeCount, StartCoverageIndex: Word;
  GlyphArray, RangeArray, RangeRecord: PByte;
  L, R, M, Straw, StrawStart, StrawEnd: Int32;
begin
  case ttUSHORT(CoverageTable) of
    1:
      begin
        GlyphCount := ttUSHORT(CoverageTable + 2);
        GlyphArray := CoverageTable + 4;
        { Binary search }
        L := 0;
        R := GlyphCount - 1;
        while L <= R do
        begin
          M := (L + R) shr 1;
          Straw := ttUSHORT(GlyphArray + 2 * M);
          if Glyph < Straw then
            R := M - 1
          else if Glyph > Straw then
            L := M + 1
          else
            Exit(M);
        end;
      end;
    2:
      begin
        RangeCount := ttUSHORT(CoverageTable + 2);
        RangeArray := CoverageTable + 4;
        { Binary search }
        L := 0;
        R := RangeCount - 1;
        while L <= R do
        begin
          M := (L + R) shr 1;
          RangeRecord := RangeArray + 6 * M;
          StrawStart := ttUSHORT(RangeRecord);
          StrawEnd := ttUSHORT(RangeRecord + 2);
          if Glyph < StrawStart then
            R := M - 1
          else if Glyph > StrawEnd then
            L := M + 1
          else
          begin
            StartCoverageIndex := ttUSHORT(RangeRecord + 4);
            Exit(StartCoverageIndex + Glyph - StrawStart);
          end;
        end;
      end;
  end;
  Result := -1;
end;

function stbtt__GetGlyphClass(ClassDefTable: PByte; Glyph: Integer): Int32;
var
  StartGlyphID, GlyphCount, ClassRangeCount: Word;
  ClassDef1ValueArray, ClassRangeRecords, ClassRangeRecord: PByte;
  L, R, M, StrawStart, StrawEnd: Int32;
begin
  case ttUSHORT(ClassDefTable) of
    1:
      begin
        StartGlyphID := ttUSHORT(ClassDefTable + 2);
        GlyphCount := ttUSHORT(ClassDefTable + 4);
        ClassDef1ValueArray := ClassDefTable + 6;
        if (Glyph >= StartGlyphID) and (Glyph < StartGlyphID + GlyphCount) then
          Exit(ttUSHORT(ClassDef1ValueArray + 2 * (Glyph - StartGlyphID)));
      end;
    2:
      begin
        ClassRangeCount := ttUSHORT(ClassDefTable + 2);
        ClassRangeRecords := ClassDefTable + 4;
        { Binary search }
        L := 0;
        R := ClassRangeCount - 1;
        while L <= R do
        begin
          M := (L + R) shr 1;
          ClassRangeRecord := ClassRangeRecords + 6 * M;
          StrawStart := ttUSHORT(ClassRangeRecord);
          StrawEnd := ttUSHORT(ClassRangeRecord + 2);
          if Glyph < StrawStart then
            R := M - 1
          else if Glyph > StrawEnd then
            L := M + 1
          else
            Exit(ttUSHORT(ClassRangeRecord + 4));
        end;
      end;
  end;
  Result := -1;
end;

function stbtt__GetGlyphGPOSInfoAdvance(const Info: TStbttFontInfo; Glyph1, Glyph2: Integer): Int32;
var
  LookupListOffset, LookupCount, LookupOffset, LookupType, SubTableCount: Word;
  SubtableOffset, PosFormat, CoverageOffset, ValueFormat1, ValueFormat2: Word;
  PairPosOffset, PairValueCount, SecondGlyph: Word;
  ClassDef1Offset, ClassDef2Offset, Class1Count, Class2Count: Word;
  Data, LookupList, LookupTable, SubTableOffsets, Table: PByte;
  PairValueTable, PairValueArray, PairValue, Class1Records, Class2Records: PByte;
  I, Sti, CoverageIndex, L, R, M, Straw: Int32;
  Glyph1Class, Glyph2Class: Integer;
const
  ValueRecordPairSizeInBytes = 2;
begin
  if Info.Gpos = 0 then
    Exit(0);
  Data := Info.Data + Info.Gpos;
  if ttUSHORT(Data + 0) <> 1 then
    Exit(0); { major version 1 }
  if ttUSHORT(Data + 2) <> 0 then
    Exit(0); { minor version 0 }
  LookupListOffset := ttUSHORT(Data + 8);
  LookupList := Data + LookupListOffset;
  LookupCount := ttUSHORT(LookupList);
  for I := 0 to LookupCount - 1 do
  begin
    LookupOffset := ttUSHORT(LookupList + 2 + 2 * I);
    LookupTable := LookupList + LookupOffset;
    LookupType := ttUSHORT(LookupTable);
    SubTableCount := ttUSHORT(LookupTable + 4);
    SubTableOffsets := LookupTable + 6;
    { Only pair adjustment positioning subtables are implemented }
    if LookupType <> 2 then
      Continue;
    for Sti := 0 to SubTableCount - 1 do
    begin
      SubtableOffset := ttUSHORT(SubTableOffsets + 2 * Sti);
      Table := LookupTable + SubtableOffset;
      PosFormat := ttUSHORT(Table);
      CoverageOffset := ttUSHORT(Table + 2);
      CoverageIndex := stbtt__GetCoverageIndex(Table + CoverageOffset, Glyph1);
      if CoverageIndex = -1 then
        Continue;
      case PosFormat of
        1:
          begin
            ValueFormat1 := ttUSHORT(Table + 4);
            ValueFormat2 := ttUSHORT(Table + 6);
            PairPosOffset := ttUSHORT(Table + 10 + 2 * CoverageIndex);
            PairValueTable := Table + PairPosOffset;
            PairValueCount := ttUSHORT(PairValueTable);
            PairValueArray := PairValueTable + 2;
            { Only these value formats are supported }
            if ValueFormat1 <> 4 then
              Exit(0);
            if ValueFormat2 <> 0 then
              Exit(0);
            R := PairValueCount - 1;
            L := 0;
            { Binary search }
            while L <= R do
            begin
              M := (L + R) shr 1;
              PairValue := PairValueArray + (2 + ValueRecordPairSizeInBytes) * M;
              SecondGlyph := ttUSHORT(PairValue);
              Straw := SecondGlyph;
              if Glyph2 < Straw then
                R := M - 1
              else if Glyph2 > Straw then
                L := M + 1
              else
                Exit(ttSHORT(PairValue + 2));
            end;
          end;
        2:
          begin
            ValueFormat1 := ttUSHORT(Table + 4);
            ValueFormat2 := ttUSHORT(Table + 6);
            ClassDef1Offset := ttUSHORT(Table + 8);
            ClassDef2Offset := ttUSHORT(Table + 10);
            Glyph1Class := stbtt__GetGlyphClass(Table + ClassDef1Offset, Glyph1);
            Glyph2Class := stbtt__GetGlyphClass(Table + ClassDef2Offset, Glyph2);
            Class1Count := ttUSHORT(Table + 12);
            Class2Count := ttUSHORT(Table + 14);
            { Only these value formats are supported }
            if ValueFormat1 <> 4 then
              Exit(0);
            if ValueFormat2 <> 0 then
              Exit(0);
            if (Glyph1Class >= 0) and (Glyph1Class < Class1Count) and
              (Glyph2Class >= 0) and (Glyph2Class < Class2Count) then
            begin
              Class1Records := Table + 16;
              Class2Records := Class1Records + 2 * (Glyph1Class * Class2Count);
              Exit(ttSHORT(Class2Records + 2 * Glyph2Class));
            end;
          end;
      end;
    end;
  end;
  Result := 0;
end;

function stbtt_GetGlyphKernAdvance(const Info: TStbttFontInfo; G1, G2: Integer): Integer;
begin
  Result := 0;
  if Info.Gpos <> 0 then
    Inc(Result, stbtt__GetGlyphGPOSInfoAdvance(Info, G1, G2))
  else if Info.Kern <> 0 then
    Inc(Result, stbtt__GetGlyphKernInfoAdvance(Info, G1, G2));
end;

procedure stbtt_GetFontVMetrics(const Info: TStbttFontInfo; Ascent, Descent, LineGap: PInteger);
begin
  if Ascent <> nil then
    Ascent^ := ttSHORT(Info.Data + Info.Hhea + 4);
  if Descent <> nil then
    Descent^ := ttSHORT(Info.Data + Info.Hhea + 6);
  if LineGap <> nil then
    LineGap^ := ttSHORT(Info.Data + Info.Hhea + 8);
end;

procedure stbtt_GetFontBoundingBox(const Info: TStbttFontInfo; out X0, Y0, X1, Y1: Integer);
begin
  X0 := ttSHORT(Info.Data + Info.Head + 36);
  Y0 := ttSHORT(Info.Data + Info.Head + 38);
  X1 := ttSHORT(Info.Data + Info.Head + 40);
  Y1 := ttSHORT(Info.Data + Info.Head + 42);
end;

function stbtt_ScaleForPixelHeight(const Info: TStbttFontInfo; Height: Single): Single;
var
  FHeight: Integer;
begin
  FHeight := ttSHORT(Info.Data + Info.Hhea + 4) - ttSHORT(Info.Data + Info.Hhea + 6);
  Result := Height / FHeight;
end;

function stbtt_ScaleForMappingEmToPixels(const Info: TStbttFontInfo; Pixels: Single): Single;
var
  UnitsPerEm: Integer;
begin
  UnitsPerEm := ttUSHORT(Info.Data + Info.Head + 18);
  Result := Pixels / UnitsPerEm;
end;

procedure stbtt_FreeShape(V: PStbttVertex);
begin
  if V <> nil then
    FreeMem(V);
end;

{ Antialiasing software rasterizer }

procedure stbtt_GetGlyphBitmapBoxSubpixel(const Info: TStbttFontInfo; Glyph: Integer;
  ScaleX, ScaleY, ShiftX, ShiftY: Single; IX0, IY0, IX1, IY1: PInteger);
var
  X0, Y0, X1, Y1: Integer;
begin
  X0 := 0;
  Y0 := 0;
  X1 := 0;
  Y1 := 0;
  if not stbtt_GetGlyphBox(Info, Glyph, @X0, @Y0, @X1, @Y1) then
  begin
    { For example the space character }
    if IX0 <> nil then IX0^ := 0;
    if IY0 <> nil then IY0^ := 0;
    if IX1 <> nil then IX1^ := 0;
    if IY1 <> nil then IY1^ := 0;
  end
  else
  begin
    { Move to integral bboxes (treating pixels as little squares, what pixels
      get touched) }
    if IX0 <> nil then IX0^ := Floor(X0 * ScaleX + ShiftX);
    if IY0 <> nil then IY0^ := Floor(-Y1 * ScaleY + ShiftY);
    if IX1 <> nil then IX1^ := Ceil(X1 * ScaleX + ShiftX);
    if IY1 <> nil then IY1^ := Ceil(-Y0 * ScaleY + ShiftY);
  end;
end;

procedure stbtt_GetGlyphBitmapBox(const Info: TStbttFontInfo; Glyph: Integer;
  ScaleX, ScaleY: Single; IX0, IY0, IX1, IY1: PInteger);
begin
  stbtt_GetGlyphBitmapBoxSubpixel(Info, Glyph, ScaleX, ScaleY, 0, 0, IX0, IY0, IX1, IY1);
end;

{ Rasterizer }

type
  PStbttHHeapChunk = ^TStbttHHeapChunk;
  TStbttHHeapChunk = record
    Next: PStbttHHeapChunk;
  end;

  TStbttHHeap = record
    Head: PStbttHHeapChunk;
    FirstFree: Pointer;
    NumRemainingInHeadChunk: Integer;
  end;

  PStbttEdge = ^TStbttEdge;
  TStbttEdge = record
    X0, Y0, X1, Y1: Single;
    Invert: Boolean;
  end;

  PPStbttActiveEdge = ^PStbttActiveEdge;
  PStbttActiveEdge = ^TStbttActiveEdge;
  TStbttActiveEdge = record
    Next: PStbttActiveEdge;
    FX, FDX, FDY: Single;
    Direction: Single;
    SY: Single;
    EY: Single;
  end;

  PStbttPoint = ^TStbttPoint;
  TStbttPoint = record
    X, Y: Single;
  end;

function stbtt__hheap_alloc(var HH: TStbttHHeap; Size: PtrUInt): Pointer;
var
  Count: Integer;
  C: PStbttHHeapChunk;
begin
  if HH.FirstFree <> nil then
  begin
    Result := HH.FirstFree;
    HH.FirstFree := PPointer(Result)^;
    Exit;
  end;
  if HH.NumRemainingInHeadChunk = 0 then
  begin
    if Size < 32 then
      Count := 2000
    else if Size < 128 then
      Count := 800
    else
      Count := 100;
    C := GetMem(SizeOf(TStbttHHeapChunk) + Size * PtrUInt(Count));
    if C = nil then
      Exit(nil);
    C.Next := HH.Head;
    HH.Head := C;
    HH.NumRemainingInHeadChunk := Count;
  end;
  Dec(HH.NumRemainingInHeadChunk);
  Result := PByte(HH.Head) + SizeOf(TStbttHHeapChunk) + Size * PtrUInt(HH.NumRemainingInHeadChunk);
end;

procedure stbtt__hheap_free(var HH: TStbttHHeap; P: Pointer);
begin
  PPointer(P)^ := HH.FirstFree;
  HH.FirstFree := P;
end;

procedure stbtt__hheap_cleanup(var HH: TStbttHHeap);
var
  C, N: PStbttHHeapChunk;
begin
  C := HH.Head;
  while C <> nil do
  begin
    N := C.Next;
    FreeMem(C);
    C := N;
  end;
end;

function stbtt__new_active(var HH: TStbttHHeap; E: PStbttEdge; OffX: Integer; StartPoint: Single): PStbttActiveEdge;
var
  Z: PStbttActiveEdge;
  DxDy: Single;
begin
  Z := stbtt__hheap_alloc(HH, SizeOf(TStbttActiveEdge));
  DxDy := (E.X1 - E.X0) / (E.Y1 - E.Y0);
  if Z = nil then
    Exit(nil);
  Z.FDX := DxDy;
  if DxDy <> 0 then
    Z.FDY := 1 / DxDy
  else
    Z.FDY := 0;
  Z.FX := E.X0 + DxDy * (StartPoint - E.Y0);
  Z.FX := Z.FX - OffX;
  if E.Invert then
    Z.Direction := 1
  else
    Z.Direction := -1;
  Z.SY := E.Y0;
  Z.EY := E.Y1;
  Z.Next := nil;
  Result := Z;
end;

{ The edge passed in here does not cross the vertical line at x or the
  vertical line at x+1 (it has already been clipped to those) }

procedure stbtt__handle_clipped_edge(Scanline: PSingle; X: Integer; E: PStbttActiveEdge; X0, Y0, X1, Y1: Single);
begin
  if Y0 = Y1 then
    Exit;
  if Y0 > E.EY then
    Exit;
  if Y1 < E.SY then
    Exit;
  if Y0 < E.SY then
  begin
    X0 := X0 + (X1 - X0) * (E.SY - Y0) / (Y1 - Y0);
    Y0 := E.SY;
  end;
  if Y1 > E.EY then
  begin
    X1 := X1 + (X1 - X0) * (E.EY - Y1) / (Y1 - Y0);
    Y1 := E.EY;
  end;
  if (X0 <= X) and (X1 <= X) then
    Scanline[X] := Scanline[X] + E.Direction * (Y1 - Y0)
  else if (X0 >= X + 1) and (X1 >= X + 1) then
    { Nothing to add }
  else
    { Coverage = 1 - average x position }
    Scanline[X] := Scanline[X] + E.Direction * (Y1 - Y0) * (1 - ((X0 - X) + (X1 - X)) / 2);
end;

procedure stbtt__fill_active_edges_new(Scanline, ScanlineFill: PSingle; Len: Integer; E: PStbttActiveEdge; YTop: Single);
var
  YBottom, X0, DX, XB, XTop, XBottom, SY0, SY1, DY, Height, T: Single;
  YCrossing, Step, Sign, Area, FX1, FX2, X3, FY0, FY1, FY2, Y3: Single;
  X, IX1, IX2: Integer;
begin
  YBottom := YTop + 1;
  while E <> nil do
  begin
    { Brute force every pixel, compute intersection points with top & bottom }
    if E.FDX = 0 then
    begin
      X0 := E.FX;
      if X0 < Len then
      begin
        if X0 >= 0 then
        begin
          stbtt__handle_clipped_edge(Scanline, Trunc(X0), E, X0, YTop, X0, YBottom);
          stbtt__handle_clipped_edge(ScanlineFill - 1, Trunc(X0) + 1, E, X0, YTop, X0, YBottom);
        end
        else
          stbtt__handle_clipped_edge(ScanlineFill - 1, 0, E, X0, YTop, X0, YBottom);
      end;
    end
    else
    begin
      X0 := E.FX;
      DX := E.FDX;
      XB := X0 + DX;
      DY := E.FDY;
      { Compute endpoints of line segment clipped to this scanline (if the
        line segment starts on this scanline. x0 is the intersection of the
        line with y_top, but that may be off the line segment }
      if E.SY > YTop then
      begin
        XTop := X0 + DX * (E.SY - YTop);
        SY0 := E.SY;
      end
      else
      begin
        XTop := X0;
        SY0 := YTop;
      end;
      if E.EY < YBottom then
      begin
        XBottom := X0 + DX * (E.EY - YTop);
        SY1 := E.EY;
      end
      else
      begin
        XBottom := XB;
        SY1 := YBottom;
      end;
      if (XTop >= 0) and (XBottom >= 0) and (XTop < Len) and (XBottom < Len) then
      begin
        { From here on, we don't have to range check x values }
        if Trunc(XTop) = Trunc(XBottom) then
        begin
          { Simple case, only spans one pixel }
          X := Trunc(XTop);
          Height := SY1 - SY0;
          Scanline[X] := Scanline[X] + E.Direction * (1 - ((XTop - X) + (XBottom - X)) / 2) * Height;
          { Everything right of this pixel is filled }
          ScanlineFill[X] := ScanlineFill[X] + E.Direction * Height;
        end
        else
        begin
          { Covers 2+ pixels }
          if XTop > XBottom then
          begin
            { Flip scanline vertically; signed area is the same }
            SY0 := YBottom - (SY0 - YTop);
            SY1 := YBottom - (SY1 - YTop);
            T := SY0; SY0 := SY1; SY1 := T;
            T := XBottom; XBottom := XTop; XTop := T;
            DX := -DX;
            DY := -DY;
            T := X0; X0 := XB; XB := T;
          end;
          IX1 := Trunc(XTop);
          IX2 := Trunc(XBottom);
          { Compute intersection with y axis at x1+1 }
          YCrossing := (IX1 + 1 - X0) * DY + YTop;
          Sign := E.Direction;
          { Area of the rectangle covered from y0..y_crossing }
          Area := Sign * (YCrossing - SY0);
          { Area of the triangle (x_top,y0), (x+1,y0), (x+1,y_crossing) }
          Scanline[IX1] := Scanline[IX1] + Area * (1 - ((XTop - IX1) + (IX1 + 1 - IX1)) / 2);
          Step := Sign * DY;
          for X := IX1 + 1 to IX2 - 1 do
          begin
            Scanline[X] := Scanline[X] + Area + Step / 2;
            Area := Area + Step;
          end;
          YCrossing := YCrossing + DY * (IX2 - (IX1 + 1));
          Scanline[IX2] := Scanline[IX2] + Area + Sign * (1 - ((IX2 - IX2) + (XBottom - IX2)) / 2) * (SY1 - YCrossing);
          ScanlineFill[IX2] := ScanlineFill[IX2] + Sign * (SY1 - SY0);
        end;
      end
      else
      begin
        { If edge goes outside of box we're drawing, we require clipping
          logic. Since this does not match the intended use of this library,
          we use a different, very slow brute force implementation }
        for X := 0 to Len - 1 do
        begin
          { There can be up to two intersections with the pixel. Any
            intersection with left or right edges can be handled by splitting
            into two (or three) regions. Intersections with top & bottom do not
            necessitate case-wise logic }
          FY0 := YTop;
          FX1 := X;
          FX2 := X + 1;
          X3 := XB;
          Y3 := YBottom;
          FY1 := (X - X0) / DX + YTop;
          FY2 := (X + 1 - X0) / DX + YTop;
          if (X0 < FX1) and (X3 > FX2) then
          begin
            { Three segments descending down-right }
            stbtt__handle_clipped_edge(Scanline, X, E, X0, FY0, FX1, FY1);
            stbtt__handle_clipped_edge(Scanline, X, E, FX1, FY1, FX2, FY2);
            stbtt__handle_clipped_edge(Scanline, X, E, FX2, FY2, X3, Y3);
          end
          else if (X3 < FX1) and (X0 > FX2) then
          begin
            { Three segments descending down-left }
            stbtt__handle_clipped_edge(Scanline, X, E, X0, FY0, FX2, FY2);
            stbtt__handle_clipped_edge(Scanline, X, E, FX2, FY2, FX1, FY1);
            stbtt__handle_clipped_edge(Scanline, X, E, FX1, FY1, X3, Y3);
          end
          else if (X0 < FX1) and (X3 > FX1) then
          begin
            { Two segments across x, down-right }
            stbtt__handle_clipped_edge(Scanline, X, E, X0, FY0, FX1, FY1);
            stbtt__handle_clipped_edge(Scanline, X, E, FX1, FY1, X3, Y3);
          end
          else if (X3 < FX1) and (X0 > FX1) then
          begin
            { Two segments across x, down-left }
            stbtt__handle_clipped_edge(Scanline, X, E, X0, FY0, FX1, FY1);
            stbtt__handle_clipped_edge(Scanline, X, E, FX1, FY1, X3, Y3);
          end
          else if (X0 < FX2) and (X3 > FX2) then
          begin
            { Two segments across x+1, down-right }
            stbtt__handle_clipped_edge(Scanline, X, E, X0, FY0, FX2, FY2);
            stbtt__handle_clipped_edge(Scanline, X, E, FX2, FY2, X3, Y3);
          end
          else if (X3 < FX2) and (X0 > FX2) then
          begin
            { Two segments across x+1, down-left }
            stbtt__handle_clipped_edge(Scanline, X, E, X0, FY0, FX2, FY2);
            stbtt__handle_clipped_edge(Scanline, X, E, FX2, FY2, X3, Y3);
          end
          else
            { One segment }
            stbtt__handle_clipped_edge(Scanline, X, E, X0, FY0, X3, Y3);
        end;
      end;
    end;
    E := E.Next;
  end;
end;

{ Directly anti-alias rasterize edges without supersampling }

procedure stbtt__rasterize_sorted_edges(var Bitmap: TStbttBitmap; E: PStbttEdge; N, OffX, OffY: Integer);
var
  HH: TStbttHHeap;
  Active, Z: PStbttActiveEdge;
  Step: PPStbttActiveEdge;
  Y, J, I, M: Integer;
  ScanlineData: array[0..128] of Single;
  Scanline, Scanline2: PSingle;
  ScanYTop, ScanYBottom, Sum, K: Single;
begin
  FillChar(HH, SizeOf(HH), 0);
  Active := nil;
  J := 0;
  if Bitmap.W > 64 then
    Scanline := GetMem((Bitmap.W * 2 + 1) * SizeOf(Single))
  else
    Scanline := @ScanlineData[0];
  Scanline2 := Scanline + Bitmap.W;
  Y := OffY;
  E[N].Y0 := (OffY + Bitmap.H) + 1;
  while J < Bitmap.H do
  begin
    { Find center of pixel for this scanline }
    ScanYTop := Y + 0.0;
    ScanYBottom := Y + 1.0;
    Step := @Active;
    FillChar(Scanline^, Bitmap.W * SizeOf(Single), 0);
    FillChar(Scanline2^, (Bitmap.W + 1) * SizeOf(Single), 0);
    { Update all active edges, remove all active edges that terminate before
      the top of this scanline }
    while Step^ <> nil do
    begin
      Z := Step^;
      if Z.EY <= ScanYTop then
      begin
        { Delete from list }
        Step^ := Z.Next;
        Z.Direction := 0;
        stbtt__hheap_free(HH, Z);
      end
      else
        { Advance through list }
        Step := @Step^.Next;
    end;
    { Insert all edges that start before the bottom of this scanline }
    while E.Y0 <= ScanYBottom do
    begin
      if E.Y0 <> E.Y1 then
      begin
        Z := stbtt__new_active(HH, E, OffX, ScanYTop);
        if Z <> nil then
        begin
          if (J = 0) and (OffY <> 0) then
            if Z.EY < ScanYTop then
              { This can happen due to subpixel positioning and some kind of
                fp rounding error }
              Z.EY := ScanYTop;
          { Insert at front }
          Z.Next := Active;
          Active := Z;
        end;
      end;
      Inc(E);
    end;
    { Now process all active edges }
    if Active <> nil then
      stbtt__fill_active_edges_new(Scanline, Scanline2 + 1, Bitmap.W, Active, ScanYTop);
    Sum := 0;
    for I := 0 to Bitmap.W - 1 do
    begin
      Sum := Sum + Scanline2[I];
      K := Scanline[I] + Sum;
      K := Abs(K) * 255 + 0.5;
      M := Trunc(K);
      if M > 255 then
        M := 255;
      Bitmap.Pixels[J * Bitmap.Stride + I] := Byte(M);
    end;
    { Advance all the edges }
    Step := @Active;
    while Step^ <> nil do
    begin
      Z := Step^;
      { Advance to position for current scanline }
      Z.FX := Z.FX + Z.FDX;
      Step := @Step^.Next;
    end;
    Inc(Y);
    Inc(J);
  end;
  stbtt__hheap_cleanup(HH);
  if Scanline <> @ScanlineData[0] then
    FreeMem(Scanline);
end;

procedure stbtt__sort_edges_ins_sort(P: PStbttEdge; N: Integer);
var
  I, J: Integer;
  T: TStbttEdge;
begin
  for I := 1 to N - 1 do
  begin
    T := P[I];
    J := I;
    while J > 0 do
    begin
      if not (T.Y0 < P[J - 1].Y0) then
        Break;
      P[J] := P[J - 1];
      Dec(J);
    end;
    if I <> J then
      P[J] := T;
  end;
end;

procedure stbtt__sort_edges_quicksort(P: PStbttEdge; N: Integer);
var
  T: TStbttEdge;
  C01, C12, C: Boolean;
  M, I, J, Z: Integer;
begin
  { Threshold for transitioning to insertion sort }
  while N > 12 do
  begin
    { Compute median of three }
    M := N shr 1;
    C01 := P[0].Y0 < P[M].Y0;
    C12 := P[M].Y0 < P[N - 1].Y0;
    { If 0 >= mid >= end, or 0 < mid < end, then use mid }
    if C01 <> C12 then
    begin
      { Otherwise, we'll need to swap something else to middle }
      C := P[0].Y0 < P[N - 1].Y0;
      if C = C12 then
        Z := 0
      else
        Z := N - 1;
      T := P[Z];
      P[Z] := P[M];
      P[M] := T;
    end;
    { Now p[m] is the median-of-three, swap it to the beginning so it won't
      move around }
    T := P[0];
    P[0] := P[M];
    P[M] := T;
    { Partition loop }
    I := 1;
    J := N - 1;
    while True do
    begin
      { Handling of equality is crucial here for sentinels & efficiency with
        duplicates }
      while P[I].Y0 < P[0].Y0 do
        Inc(I);
      while P[0].Y0 < P[J].Y0 do
        Dec(J);
      { Make sure we haven't crossed }
      if I >= J then
        Break;
      T := P[I];
      P[I] := P[J];
      P[J] := T;
      Inc(I);
      Dec(J);
    end;
    { Recurse on smaller side, iterate on larger }
    if J < (N - I) then
    begin
      stbtt__sort_edges_quicksort(P, J);
      P := P + I;
      N := N - I;
    end
    else
    begin
      stbtt__sort_edges_quicksort(P + I, N - I);
      N := J;
    end;
  end;
end;

procedure stbtt__sort_edges(P: PStbttEdge; N: Integer);
begin
  stbtt__sort_edges_quicksort(P, N);
  stbtt__sort_edges_ins_sort(P, N);
end;

procedure stbtt__rasterize(var Bitmap: TStbttBitmap; Pts: PStbttPoint; WCount: PInteger; Windings: Integer;
  ScaleX, ScaleY, ShiftX, ShiftY: Single; OffX, OffY: Integer; Invert: Boolean);
const
  VSubsample = 1;
var
  YScaleInv: Single;
  E: PStbttEdge;
  N, I, J, K, M, A, B: Integer;
  P: PStbttPoint;
  Swap: Boolean;
begin
  if Invert then
    YScaleInv := -ScaleY
  else
    YScaleInv := ScaleY;
  { Now we have to blow out the windings into explicit edge lists }
  N := 0;
  for I := 0 to Windings - 1 do
    Inc(N, WCount[I]);
  { Add an extra one as a sentinel }
  E := GetMem(SizeOf(TStbttEdge) * (N + 1));
  if E = nil then
    Exit;
  N := 0;
  M := 0;
  for I := 0 to Windings - 1 do
  begin
    P := Pts + M;
    Inc(M, WCount[I]);
    J := WCount[I] - 1;
    K := 0;
    while K < WCount[I] do
    begin
      A := K;
      B := J;
      { Skip the edge if horizontal }
      if P[J].Y <> P[K].Y then
      begin
        { Add edge from j to k to the list }
        E[N].Invert := False;
        if Invert then
          Swap := P[J].Y > P[K].Y
        else
          Swap := P[J].Y < P[K].Y;
        if Swap then
        begin
          E[N].Invert := True;
          A := J;
          B := K;
        end;
        E[N].X0 := P[A].X * ScaleX + ShiftX;
        E[N].Y0 := (P[A].Y * YScaleInv + ShiftY) * VSubsample;
        E[N].X1 := P[B].X * ScaleX + ShiftX;
        E[N].Y1 := (P[B].Y * YScaleInv + ShiftY) * VSubsample;
        Inc(N);
      end;
      J := K;
      Inc(K);
    end;
  end;
  { Now sort the edges by their highest point (should snap to integer, and
    then by x) }
  stbtt__sort_edges(E, N);
  { Now, traverse the scanlines and find the intersections on each scanline,
    use xor winding rule }
  stbtt__rasterize_sorted_edges(Bitmap, E, N, OffX, OffY);
  FreeMem(E);
end;

procedure stbtt__add_point(Points: PStbttPoint; N: Integer; X, Y: Single); inline;
begin
  { During first pass, it's unallocated }
  if Points = nil then
    Exit;
  Points[N].X := X;
  Points[N].Y := Y;
end;

{ Tessellate until threshold p is happy }

procedure stbtt__tesselate_curve(Points: PStbttPoint; var NumPoints: Integer;
  X0, Y0, X1, Y1, X2, Y2, ObjspaceFlatnessSquared: Single; N: Integer);
var
  MX, MY, DX, DY: Single;
begin
  { Midpoint }
  MX := (X0 + 2 * X1 + X2) / 4;
  MY := (Y0 + 2 * Y1 + Y2) / 4;
  { Versus directly drawn line }
  DX := (X0 + X2) / 2 - MX;
  DY := (Y0 + Y2) / 2 - MY;
  { 65536 segments on one curve better be enough }
  if N > 16 then
    Exit;
  { Half-pixel error allowed... need to be smaller if AA }
  if DX * DX + DY * DY > ObjspaceFlatnessSquared then
  begin
    stbtt__tesselate_curve(Points, NumPoints, X0, Y0, (X0 + X1) / 2, (Y0 + Y1) / 2, MX, MY, ObjspaceFlatnessSquared, N + 1);
    stbtt__tesselate_curve(Points, NumPoints, MX, MY, (X1 + X2) / 2, (Y1 + Y2) / 2, X2, Y2, ObjspaceFlatnessSquared, N + 1);
  end
  else
  begin
    stbtt__add_point(Points, NumPoints, X2, Y2);
    Inc(NumPoints);
  end;
end;

procedure stbtt__tesselate_cubic(Points: PStbttPoint; var NumPoints: Integer;
  X0, Y0, X1, Y1, X2, Y2, X3, Y3, ObjspaceFlatnessSquared: Single; N: Integer);
var
  DX0, DY0, DX1, DY1, DX2, DY2, DX, DY, LongLen, ShortLen, FlatnessSquared: Single;
  X01, Y01, X12, Y12, X23, Y23, XA, YA, XB, YB, MX, MY: Single;
begin
  { This "flatness" calculation is just made-up nonsense that seems to work
    well enough }
  DX0 := X1 - X0;
  DY0 := Y1 - Y0;
  DX1 := X2 - X1;
  DY1 := Y2 - Y1;
  DX2 := X3 - X2;
  DY2 := Y3 - Y2;
  DX := X3 - X0;
  DY := Y3 - Y0;
  LongLen := Sqrt(DX0 * DX0 + DY0 * DY0) + Sqrt(DX1 * DX1 + DY1 * DY1) + Sqrt(DX2 * DX2 + DY2 * DY2);
  ShortLen := Sqrt(DX * DX + DY * DY);
  FlatnessSquared := LongLen * LongLen - ShortLen * ShortLen;
  { 65536 segments on one curve better be enough }
  if N > 16 then
    Exit;
  if FlatnessSquared > ObjspaceFlatnessSquared then
  begin
    X01 := (X0 + X1) / 2;
    Y01 := (Y0 + Y1) / 2;
    X12 := (X1 + X2) / 2;
    Y12 := (Y1 + Y2) / 2;
    X23 := (X2 + X3) / 2;
    Y23 := (Y2 + Y3) / 2;
    XA := (X01 + X12) / 2;
    YA := (Y01 + Y12) / 2;
    XB := (X12 + X23) / 2;
    YB := (Y12 + Y23) / 2;
    MX := (XA + XB) / 2;
    MY := (YA + YB) / 2;
    stbtt__tesselate_cubic(Points, NumPoints, X0, Y0, X01, Y01, XA, YA, MX, MY, ObjspaceFlatnessSquared, N + 1);
    stbtt__tesselate_cubic(Points, NumPoints, MX, MY, XB, YB, X23, Y23, X3, Y3, ObjspaceFlatnessSquared, N + 1);
  end
  else
  begin
    stbtt__add_point(Points, NumPoints, X3, Y3);
    Inc(NumPoints);
  end;
end;

{ Returns the flattened points and the number of contours }

function stbtt_FlattenCurves(Vertices: PStbttVertex; NumVerts: Integer; ObjspaceFlatness: Single;
  out ContourLengths: PInteger; out NumContours: Integer): PStbttPoint;
var
  Points: PStbttPoint;
  NumPoints, I, N, Start, Pass: Integer;
  ObjspaceFlatnessSquared, X, Y: Single;
begin
  Points := nil;
  NumPoints := 0;
  ContourLengths := nil;
  ObjspaceFlatnessSquared := ObjspaceFlatness * ObjspaceFlatness;
  N := 0;
  Start := 0;
  { Count how many "moves" there are to get the contour count }
  for I := 0 to NumVerts - 1 do
    if Vertices[I].VType = STBTT_vmove then
      Inc(N);
  NumContours := N;
  if N = 0 then
    Exit(nil);
  ContourLengths := GetMem(SizeOf(Integer) * N);
  if ContourLengths = nil then
  begin
    NumContours := 0;
    Exit(nil);
  end;
  { Make two passes through the points so we don't need to realloc }
  for Pass := 0 to 1 do
  begin
    X := 0;
    Y := 0;
    if Pass = 1 then
    begin
      Points := GetMem(NumPoints * SizeOf(TStbttPoint));
      if Points = nil then
      begin
        FreeMem(ContourLengths);
        ContourLengths := nil;
        NumContours := 0;
        Exit(nil);
      end;
    end;
    NumPoints := 0;
    N := -1;
    for I := 0 to NumVerts - 1 do
      case Vertices[I].VType of
        STBTT_vmove:
          begin
            { Start the next contour }
            if N >= 0 then
              ContourLengths[N] := NumPoints - Start;
            Inc(N);
            Start := NumPoints;
            X := Vertices[I].X;
            Y := Vertices[I].Y;
            stbtt__add_point(Points, NumPoints, X, Y);
            Inc(NumPoints);
          end;
        STBTT_vline:
          begin
            X := Vertices[I].X;
            Y := Vertices[I].Y;
            stbtt__add_point(Points, NumPoints, X, Y);
            Inc(NumPoints);
          end;
        STBTT_vcurve:
          begin
            stbtt__tesselate_curve(Points, NumPoints, X, Y,
              Vertices[I].CX, Vertices[I].CY, Vertices[I].X, Vertices[I].Y,
              ObjspaceFlatnessSquared, 0);
            X := Vertices[I].X;
            Y := Vertices[I].Y;
          end;
        STBTT_vcubic:
          begin
            stbtt__tesselate_cubic(Points, NumPoints, X, Y,
              Vertices[I].CX, Vertices[I].CY, Vertices[I].CX1, Vertices[I].CY1,
              Vertices[I].X, Vertices[I].Y, ObjspaceFlatnessSquared, 0);
            X := Vertices[I].X;
            Y := Vertices[I].Y;
          end;
      end;
    ContourLengths[N] := NumPoints - Start;
  end;
  Result := Points;
end;

procedure stbtt_Rasterize(var Bitmap: TStbttBitmap; FlatnessInPixels: Single; Vertices: PStbttVertex;
  NumVerts: Integer; ScaleX, ScaleY, ShiftX, ShiftY: Single; XOff, YOff: Integer; Invert: Boolean);
var
  Scale: Single;
  WindingCount: Integer;
  WindingLengths: PInteger;
  Windings: PStbttPoint;
begin
  if ScaleX > ScaleY then
    Scale := ScaleY
  else
    Scale := ScaleX;
  Windings := stbtt_FlattenCurves(Vertices, NumVerts, FlatnessInPixels / Scale, WindingLengths, WindingCount);
  if Windings <> nil then
  begin
    stbtt__rasterize(Bitmap, Windings, WindingLengths, WindingCount, ScaleX, ScaleY, ShiftX, ShiftY, XOff, YOff, Invert);
    FreeMem(WindingLengths);
    FreeMem(Windings);
  end
  else if WindingLengths <> nil then
    FreeMem(WindingLengths);
end;

procedure stbtt_MakeGlyphBitmapSubpixel(const Info: TStbttFontInfo; Output: PByte; OutW, OutH, OutStride: Integer;
  ScaleX, ScaleY, ShiftX, ShiftY: Single; Glyph: Integer);
var
  IX0, IY0, NumVerts: Integer;
  Vertices: PStbttVertex;
  Gbm: TStbttBitmap;
begin
  NumVerts := stbtt_GetGlyphShape(Info, Glyph, Vertices);
  stbtt_GetGlyphBitmapBoxSubpixel(Info, Glyph, ScaleX, ScaleY, ShiftX, ShiftY, @IX0, @IY0, nil, nil);
  Gbm.Pixels := Output;
  Gbm.W := OutW;
  Gbm.H := OutH;
  Gbm.Stride := OutStride;
  if (Gbm.W <> 0) and (Gbm.H <> 0) then
    stbtt_Rasterize(Gbm, 0.35, Vertices, NumVerts, ScaleX, ScaleY, ShiftX, ShiftY, IX0, IY0, True);
  stbtt_FreeShape(Vertices);
end;

procedure stbtt_MakeGlyphBitmap(const Info: TStbttFontInfo; Output: PByte; OutW, OutH, OutStride: Integer;
  ScaleX, ScaleY: Single; Glyph: Integer);
begin
  stbtt_MakeGlyphBitmapSubpixel(Info, Output, OutW, OutH, OutStride, ScaleX, ScaleY, 0, 0, Glyph);
end;

function stbtt_GetGlyphBitmapSubpixel(const Info: TStbttFontInfo; ScaleX, ScaleY, ShiftX, ShiftY: Single;
  Glyph: Integer; Width, Height, XOff, YOff: PInteger): PByte;
var
  IX0, IY0, IX1, IY1, NumVerts: Integer;
  Gbm: TStbttBitmap;
  Vertices: PStbttVertex;
begin
  NumVerts := stbtt_GetGlyphShape(Info, Glyph, Vertices);
  if ScaleX = 0 then
    ScaleX := ScaleY;
  if ScaleY = 0 then
  begin
    if ScaleX = 0 then
    begin
      stbtt_FreeShape(Vertices);
      Exit(nil);
    end;
    ScaleY := ScaleX;
  end;
  stbtt_GetGlyphBitmapBoxSubpixel(Info, Glyph, ScaleX, ScaleY, ShiftX, ShiftY, @IX0, @IY0, @IX1, @IY1);
  { Now we get the size }
  Gbm.W := IX1 - IX0;
  Gbm.H := IY1 - IY0;
  Gbm.Pixels := nil;
  if Width <> nil then Width^ := Gbm.W;
  if Height <> nil then Height^ := Gbm.H;
  if XOff <> nil then XOff^ := IX0;
  if YOff <> nil then YOff^ := IY0;
  if (Gbm.W <> 0) and (Gbm.H <> 0) then
  begin
    Gbm.Pixels := GetMem(Gbm.W * Gbm.H);
    if Gbm.Pixels <> nil then
    begin
      Gbm.Stride := Gbm.W;
      stbtt_Rasterize(Gbm, 0.35, Vertices, NumVerts, ScaleX, ScaleY, ShiftX, ShiftY, IX0, IY0, True);
    end;
  end;
  stbtt_FreeShape(Vertices);
  Result := Gbm.Pixels;
end;

procedure stbtt_FreeBitmap(Bitmap: PByte);
begin
  if Bitmap <> nil then
    FreeMem(Bitmap);
end;

end.
