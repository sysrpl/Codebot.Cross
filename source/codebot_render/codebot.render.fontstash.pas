(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.render.fontstash.txt> }
unit Codebot.Render.FontStash;

{ Codebot.Render.FontStash is a Pascal port of fontstash by Mikko Mononen,
  which is zlib licensed. It caches glyphs rasterized by Codebot.Render.TrueType
  in a texture atlas and produces textured quads for drawing text. Strings are
  UTF-8 encoded. }

{$i render.inc}
{$pointermath on}
{$rangechecks off}
{$overflowchecks off}

interface

uses
  Codebot.Render.TrueType;

const
  FONS_INVALID = -1;

  { FONSflags }
  FONS_ZERO_TOPLEFT = 1;
  FONS_ZERO_BOTTOMLEFT = 2;

  { FONSalign horizontal align }
  FONS_ALIGN_LEFT = 1 shl 0; { default }
  FONS_ALIGN_CENTER = 1 shl 1;
  FONS_ALIGN_RIGHT = 1 shl 2;
  { FONSalign vertical align }
  FONS_ALIGN_TOP = 1 shl 3;
  FONS_ALIGN_MIDDLE = 1 shl 4;
  FONS_ALIGN_BOTTOM = 1 shl 5;
  FONS_ALIGN_BASELINE = 1 shl 6; { default }

  { FONSglyphBitmap }
  FONS_GLYPH_BITMAP_OPTIONAL = 1;
  FONS_GLYPH_BITMAP_REQUIRED = 2;

  { FONSerrorCode }
  { Font atlas is full }
  FONS_ATLAS_FULL = 1;
  { Scratch memory used to render glyphs is full }
  FONS_SCRATCH_FULL = 2;
  { Calls to fonsPushState has created too large stack }
  FONS_STATES_OVERFLOW = 3;
  { Trying to pop too many states fonsPopState }
  FONS_STATES_UNDERFLOW = 4;

  FONS_HASH_LUT_SIZE = 256;
  FONS_INIT_FONTS = 4;
  FONS_INIT_GLYPHS = 256;
  FONS_INIT_ATLAS_NODES = 256;
  FONS_VERTEX_COUNT = 1024;
  FONS_MAX_STATES = 20;
  FONS_MAX_FALLBACKS = 20;

type
  { Callbacks a renderer gives the font stash to create, resize, update, draw,
    and delete the texture of the atlas }
  TFonsRenderCreate = function(UserPtr: Pointer; Width, Height: Integer): Integer;
  TFonsRenderResize = function(UserPtr: Pointer; Width, Height: Integer): Integer;
  TFonsRenderUpdate = procedure(UserPtr: Pointer; Rect: PInteger; Data: PByte);
  TFonsRenderDraw = procedure(UserPtr: Pointer; Verts, TCoords: PSingle; Colors: PCardinal; NVerts: Integer);
  TFonsRenderDelete = procedure(UserPtr: Pointer);
  { A callback made when the atlas is full or a state stack overflows }
  TFonsErrorCallback = procedure(UserPtr: Pointer; Error, Val: Integer);

  { TFonsParams is the size of the atlas and the callbacks of the renderer }
  PFonsParams = ^TFonsParams;
  TFonsParams = record
    Width, Height: Integer;
    Flags: Byte;
    UserPtr: Pointer;
    RenderCreate: TFonsRenderCreate;
    RenderResize: TFonsRenderResize;
    RenderUpdate: TFonsRenderUpdate;
    RenderDraw: TFonsRenderDraw;
    RenderDelete: TFonsRenderDelete;
  end;

  { TFonsQuad is the rectangle and texture coordinates of one glyph }
  PFonsQuad = ^TFonsQuad;
  TFonsQuad = record
    X0, Y0, S0, T0: Single;
    X1, Y1, S1, T1: Single;
  end;

  { TFonsGlyph is a glyph stored in the atlas }
  PFonsGlyph = ^TFonsGlyph;
  TFonsGlyph = record
    Codepoint: Cardinal;
    Index: Integer;
    Next: Integer;
    Size, Blur: SmallInt;
    X0, Y0, X1, Y1: SmallInt;
    XAdv, XOff, YOff: SmallInt;
  end;

  { TFonsFont is a loaded font and its glyphs }
  PFonsFont = ^TFonsFont;
  TFonsFont = record
    Font: TStbttFontInfo;
    Name: string;
    Data: PByte;
    DataSize: Integer;
    FreeData: Boolean;
    Ascender: Single;
    Descender: Single;
    LineH: Single;
    Glyphs: PFonsGlyph;
    CGlyphs: Integer;
    NGlyphs: Integer;
    Lut: array[0..FONS_HASH_LUT_SIZE - 1] of Integer;
    Fallbacks: array[0..FONS_MAX_FALLBACKS - 1] of Integer;
    NFallbacks: Integer;
  end;
  PPFonsFont = ^PFonsFont;

  { TFonsTextIter steps through the glyphs of a string }
  PFonsTextIter = ^TFonsTextIter;
  TFonsTextIter = record
    X, Y, NextX, NextY, Scale, Spacing: Single;
    Codepoint: Cardinal;
    ISize, IBlur: SmallInt;
    Font: PFonsFont;
    PrevGlyphIndex: Integer;
    Str: PAnsiChar;
    Next: PAnsiChar;
    EndStr: PAnsiChar;
    Utf8State: Cardinal;
    BitmapOption: Integer;
  end;

  { TFonsState is the font, size, color, spacing, blur, and alignment in use }
  PFonsState = ^TFonsState;
  TFonsState = record
    Font: Integer;
    Align: Integer;
    Size: Single;
    Color: Cardinal;
    Blur: Single;
    Spacing: Single;
  end;

  { TFonsAtlasNode is a node of the skyline used to pack the atlas }
  PFonsAtlasNode = ^TFonsAtlasNode;
  TFonsAtlasNode = record
    X, Y, Width: SmallInt;
  end;

  { TFonsAtlas packs glyphs into the texture }
  PFonsAtlas = ^TFonsAtlas;
  TFonsAtlas = record
    Width, Height: Integer;
    Nodes: PFonsAtlasNode;
    NNodes: Integer;
    CNodes: Integer;
  end;

  { TFonsContext is a font stash, which holds fonts and the atlas of their
    glyphs }
  PFonsContext = ^TFonsContext;
  TFonsContext = record
    Params: TFonsParams;
    ITW, ITH: Single;
    TexData: PByte;
    DirtyRect: array[0..3] of Integer;
    Fonts: PPFonsFont;
    Atlas: PFonsAtlas;
    CFonts: Integer;
    NFonts: Integer;
    Verts: array[0..FONS_VERTEX_COUNT * 2 - 1] of Single;
    TCoords: array[0..FONS_VERTEX_COUNT * 2 - 1] of Single;
    Colors: array[0..FONS_VERTEX_COUNT - 1] of Cardinal;
    NVerts: Integer;
    States: array[0..FONS_MAX_STATES - 1] of TFonsState;
    NStates: Integer;
    HandleError: TFonsErrorCallback;
    ErrorUptr: Pointer;
  end;

{ Constructor and destructor }
function fonsCreateInternal(Params: PFonsParams): PFonsContext;
{ Free a font stash }
procedure fonsDeleteInternal(Stash: PFonsContext);

{ Set the callback made when an error occurs }
procedure fonsSetErrorCallback(Stash: PFonsContext; Callback: TFonsErrorCallback; UserPtr: Pointer);
{ Returns current atlas size }
procedure fonsGetAtlasSize(Stash: PFonsContext; Width, Height: PInteger);
{ Expands the atlas size }
function fonsExpandAtlas(Stash: PFonsContext; Width, Height: Integer): Integer;
{ Resets the whole stash }
function fonsResetAtlas(Stash: PFonsContext; Width, Height: Integer): Integer;

{ Add fonts }
function fonsAddFont(Stash: PFonsContext; const Name, Path: string; FontIndex: Integer): Integer;
{ Add a font from memory, returning its index }
function fonsAddFontMem(Stash: PFonsContext; const Name: string; Data: PByte; DataSize: Integer;
  FreeData: Boolean; FontIndex: Integer): Integer;
{ Find a font by name, returning its index }
function fonsGetFontByName(Stash: PFonsContext; const Name: string): Integer;
{ Add a font to use for glyphs which are missing from another font }
function fonsAddFallbackFont(Stash: PFonsContext; Base, Fallback: Integer): Integer;
{ Remove the fallback fonts of a font }
procedure fonsResetFallbackFont(Stash: PFonsContext; Base: Integer);

{ State handling }
procedure fonsPushState(Stash: PFonsContext);
{ Restore the state saved by fonsPushState }
procedure fonsPopState(Stash: PFonsContext);
{ Reset the state to its defaults }
procedure fonsClearState(Stash: PFonsContext);

{ State setting }
procedure fonsSetSize(Stash: PFonsContext; Size: Single);
{ Set the color, spacing, blur, alignment, and font of the state }
procedure fonsSetColor(Stash: PFonsContext; Color: Cardinal);
procedure fonsSetSpacing(Stash: PFonsContext; Spacing: Single);
procedure fonsSetBlur(Stash: PFonsContext; Blur: Single);
procedure fonsSetAlign(Stash: PFonsContext; Align: Integer);
procedure fonsSetFont(Stash: PFonsContext; Font: Integer);

{ Draw text, EndStr may be nil for a null terminated string }
function fonsDrawText(Stash: PFonsContext; X, Y: Single; Str, EndStr: PAnsiChar): Single;

{ Measure text }
function fonsTextBounds(Stash: PFonsContext; X, Y: Single; Str, EndStr: PAnsiChar; Bounds: PSingle): Single;
{ The top and bottom of a line of text at Y }
procedure fonsLineBounds(Stash: PFonsContext; Y: Single; MinY, MaxY: PSingle);
{ The ascender, descender, and line height of the font }
procedure fonsVertMetrics(Stash: PFonsContext; Ascender, Descender, LineH: PSingle);

{ Text iterator }
function fonsTextIterInit(Stash: PFonsContext; Iter: PFonsTextIter; X, Y: Single;
  Str, EndStr: PAnsiChar; BitmapOption: Integer): Integer;
{ Get the quad of the next glyph, returning 0 when the text ends }
function fonsTextIterNext(Stash: PFonsContext; Iter: PFonsTextIter; Quad: PFonsQuad): Integer;

{ Pull texture changes }
function fonsGetTextureData(Stash: PFonsContext; Width, Height: PInteger): PByte;
{ Returns 1 if part of the atlas texture changed, giving the rectangle to
  update }
function fonsValidateTexture(Stash: PFonsContext; Dirty: PInteger): Integer;

{ Draws the stash texture for debugging }
procedure fonsDrawDebug(Stash: PFonsContext; X, Y: Single);

implementation

uses
  SysUtils, Classes, Math;

const
  FONS_UTF8_ACCEPT = 0;
  FONS_UTF8_REJECT = 12;

function fons__hashint(A: Cardinal): Cardinal;
begin
  A := A + not (A shl 15);
  A := A xor (A shr 10);
  A := A + (A shl 3);
  A := A xor (A shr 6);
  A := A + not (A shl 11);
  A := A xor (A shr 16);
  Result := A;
end;

function fons__mini(A, B: Integer): Integer; inline;
begin
  if A < B then Result := A else Result := B;
end;

function fons__maxi(A, B: Integer): Integer; inline;
begin
  if A > B then Result := A else Result := B;
end;

{ Font implementation using Codebot.Render.TrueType }

function fons__tt_loadFont(var Font: TStbttFontInfo; Data: PByte; DataSize, FontIndex: Integer): Boolean;
var
  Offset: Integer;
begin
  Offset := stbtt_GetFontOffsetForIndex(Data, FontIndex);
  if Offset = -1 then
    Result := False
  else
    Result := stbtt_InitFont(Font, Data, Offset);
end;

procedure fons__tt_getFontVMetrics(const Font: TStbttFontInfo; Ascent, Descent, LineGap: PInteger);
begin
  stbtt_GetFontVMetrics(Font, Ascent, Descent, LineGap);
end;

function fons__tt_getPixelHeightScale(const Font: TStbttFontInfo; Size: Single): Single;
begin
  Result := stbtt_ScaleForMappingEmToPixels(Font, Size);
end;

function fons__tt_getGlyphIndex(const Font: TStbttFontInfo; Codepoint: Integer): Integer;
begin
  Result := stbtt_FindGlyphIndex(Font, Codepoint);
end;

procedure fons__tt_buildGlyphBitmap(const Font: TStbttFontInfo; Glyph: Integer; Size, Scale: Single;
  Advance, Lsb, X0, Y0, X1, Y1: PInteger);
begin
  stbtt_GetGlyphHMetrics(Font, Glyph, Advance, Lsb);
  stbtt_GetGlyphBitmapBox(Font, Glyph, Scale, Scale, X0, Y0, X1, Y1);
end;

procedure fons__tt_renderGlyphBitmap(const Font: TStbttFontInfo; Output: PByte; OutWidth, OutHeight, OutStride: Integer;
  ScaleX, ScaleY: Single; Glyph: Integer);
begin
  stbtt_MakeGlyphBitmap(Font, Output, OutWidth, OutHeight, OutStride, ScaleX, ScaleY, Glyph);
end;

function fons__tt_getGlyphKernAdvance(const Font: TStbttFontInfo; Glyph1, Glyph2: Integer): Integer;
begin
  Result := stbtt_GetGlyphKernAdvance(Font, Glyph1, Glyph2);
end;

{ UTF-8 decoder by Bjoern Hoehrmann, see http://bjoern.hoehrmann.de/utf-8/decoder/dfa/ }

const
  Utf8d: array[0..363] of Byte = (
    { The first part of the table maps bytes to character classes that to
      reduce the size of the transition table and create bitmasks }
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,
    7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
    8,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,  2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
    10,3,3,3,3,3,3,3,3,3,3,3,3,4,3,3, 11,6,6,6,5,8,8,8,8,8,8,8,8,8,8,8,
    { The second part is a transition table that maps a combination of a
      state of the automaton and a character class to a state }
    0,12,24,36,60,96,84,12,12,12,48,72, 12,12,12,12,12,12,12,12,12,12,12,12,
    12, 0,12,12,12,12,12, 0,12, 0,12,12, 12,24,12,12,12,12,12,24,12,24,12,12,
    12,12,12,12,12,12,12,24,12,12,12,12, 12,24,12,12,12,12,12,12,12,24,12,12,
    12,12,12,12,12,12,12,36,12,36,12,12, 12,36,12,12,12,12,12,36,12,36,12,12,
    12,36,12,12,12,12,12,12,12,12,12,12);

function fons__decutf8(var State, Codep: Cardinal; B: Cardinal): Cardinal;
var
  T: Cardinal;
begin
  T := Utf8d[B];
  if State <> FONS_UTF8_ACCEPT then
    Codep := (B and $3F) or (Codep shl 6)
  else
    Codep := ($FF shr T) and B;
  State := Utf8d[256 + State + T];
  Result := State;
end;

{ Atlas based on Skyline Bin Packer by Jukka Jylanki }

procedure fons__deleteAtlas(Atlas: PFonsAtlas);
begin
  if Atlas = nil then
    Exit;
  if Atlas.Nodes <> nil then
    FreeMem(Atlas.Nodes);
  FreeMem(Atlas);
end;

function fons__allocAtlas(W, H, NNodes: Integer): PFonsAtlas;
var
  Atlas: PFonsAtlas;
begin
  Atlas := AllocMem(SizeOf(TFonsAtlas));
  Atlas.Width := W;
  Atlas.Height := H;
  { Allocate space for skyline nodes }
  Atlas.Nodes := AllocMem(SizeOf(TFonsAtlasNode) * NNodes);
  Atlas.NNodes := 0;
  Atlas.CNodes := NNodes;
  { Init root node }
  Atlas.Nodes[0].X := 0;
  Atlas.Nodes[0].Y := 0;
  Atlas.Nodes[0].Width := SmallInt(W);
  Inc(Atlas.NNodes);
  Result := Atlas;
end;

function fons__atlasInsertNode(Atlas: PFonsAtlas; Idx, X, Y, W: Integer): Boolean;
var
  I: Integer;
begin
  { Insert node }
  if Atlas.NNodes + 1 > Atlas.CNodes then
  begin
    if Atlas.CNodes = 0 then
      Atlas.CNodes := 8
    else
      Atlas.CNodes := Atlas.CNodes * 2;
    ReallocMem(Atlas.Nodes, SizeOf(TFonsAtlasNode) * Atlas.CNodes);
    if Atlas.Nodes = nil then
      Exit(False);
  end;
  for I := Atlas.NNodes downto Idx + 1 do
    Atlas.Nodes[I] := Atlas.Nodes[I - 1];
  Atlas.Nodes[Idx].X := SmallInt(X);
  Atlas.Nodes[Idx].Y := SmallInt(Y);
  Atlas.Nodes[Idx].Width := SmallInt(W);
  Inc(Atlas.NNodes);
  Result := True;
end;

procedure fons__atlasRemoveNode(Atlas: PFonsAtlas; Idx: Integer);
var
  I: Integer;
begin
  if Atlas.NNodes = 0 then
    Exit;
  for I := Idx to Atlas.NNodes - 2 do
    Atlas.Nodes[I] := Atlas.Nodes[I + 1];
  Dec(Atlas.NNodes);
end;

procedure fons__atlasExpand(Atlas: PFonsAtlas; W, H: Integer);
begin
  { Insert node for empty space }
  if W > Atlas.Width then
    fons__atlasInsertNode(Atlas, Atlas.NNodes, Atlas.Width, 0, W - Atlas.Width);
  Atlas.Width := W;
  Atlas.Height := H;
end;

procedure fons__atlasReset(Atlas: PFonsAtlas; W, H: Integer);
begin
  Atlas.Width := W;
  Atlas.Height := H;
  Atlas.NNodes := 0;
  { Init root node }
  Atlas.Nodes[0].X := 0;
  Atlas.Nodes[0].Y := 0;
  Atlas.Nodes[0].Width := SmallInt(W);
  Inc(Atlas.NNodes);
end;

function fons__atlasAddSkylineLevel(Atlas: PFonsAtlas; Idx, X, Y, W, H: Integer): Boolean;
var
  I, Shrink: Integer;
begin
  { Insert new node }
  if not fons__atlasInsertNode(Atlas, Idx, X, Y + H, W) then
    Exit(False);
  { Delete skyline segments that fall under the shadow of the new segment }
  I := Idx + 1;
  while I < Atlas.NNodes do
  begin
    if Atlas.Nodes[I].X < Atlas.Nodes[I - 1].X + Atlas.Nodes[I - 1].Width then
    begin
      Shrink := Atlas.Nodes[I - 1].X + Atlas.Nodes[I - 1].Width - Atlas.Nodes[I].X;
      Atlas.Nodes[I].X := SmallInt(Atlas.Nodes[I].X + Shrink);
      Atlas.Nodes[I].Width := SmallInt(Atlas.Nodes[I].Width - Shrink);
      if Atlas.Nodes[I].Width <= 0 then
      begin
        fons__atlasRemoveNode(Atlas, I);
        Dec(I);
      end
      else
        Break;
    end
    else
      Break;
    Inc(I);
  end;
  { Merge same height skyline segments that are next to each other }
  I := 0;
  while I < Atlas.NNodes - 1 do
  begin
    if Atlas.Nodes[I].Y = Atlas.Nodes[I + 1].Y then
    begin
      Atlas.Nodes[I].Width := SmallInt(Atlas.Nodes[I].Width + Atlas.Nodes[I + 1].Width);
      fons__atlasRemoveNode(Atlas, I + 1);
      Dec(I);
    end;
    Inc(I);
  end;
  Result := True;
end;

{ Checks if there is enough space at the location of skyline span 'i', and
  return the max height of all skyline spans under that at that location,
  (think tetris block being dropped at that position). Or -1 if no space found }

function fons__atlasRectFits(Atlas: PFonsAtlas; I, W, H: Integer): Integer;
var
  X, Y, SpaceLeft: Integer;
begin
  X := Atlas.Nodes[I].X;
  Y := Atlas.Nodes[I].Y;
  if X + W > Atlas.Width then
    Exit(-1);
  SpaceLeft := W;
  while SpaceLeft > 0 do
  begin
    if I = Atlas.NNodes then
      Exit(-1);
    Y := fons__maxi(Y, Atlas.Nodes[I].Y);
    if Y + H > Atlas.Height then
      Exit(-1);
    Dec(SpaceLeft, Atlas.Nodes[I].Width);
    Inc(I);
  end;
  Result := Y;
end;

function fons__atlasAddRect(Atlas: PFonsAtlas; RW, RH: Integer; RX, RY: PInteger): Boolean;
var
  BestH, BestW, BestI, BestX, BestY, I, Y: Integer;
begin
  BestH := Atlas.Height;
  BestW := Atlas.Width;
  BestI := -1;
  BestX := -1;
  BestY := -1;
  { Bottom left fit heuristic }
  for I := 0 to Atlas.NNodes - 1 do
  begin
    Y := fons__atlasRectFits(Atlas, I, RW, RH);
    if Y <> -1 then
      if (Y + RH < BestH) or ((Y + RH = BestH) and (Atlas.Nodes[I].Width < BestW)) then
      begin
        BestI := I;
        BestW := Atlas.Nodes[I].Width;
        BestH := Y + RH;
        BestX := Atlas.Nodes[I].X;
        BestY := Y;
      end;
  end;
  if BestI = -1 then
    Exit(False);
  { Perform the actual packing }
  if not fons__atlasAddSkylineLevel(Atlas, BestI, BestX, BestY, RW, RH) then
    Exit(False);
  RX^ := BestX;
  RY^ := BestY;
  Result := True;
end;

procedure fons__addWhiteRect(Stash: PFonsContext; W, H: Integer);
var
  X, Y, GX, GY: Integer;
  Dst: PByte;
begin
  if not fons__atlasAddRect(Stash.Atlas, W, H, @GX, @GY) then
    Exit;
  { Rasterize }
  Dst := @Stash.TexData[GX + GY * Stash.Params.Width];
  for Y := 0 to H - 1 do
  begin
    for X := 0 to W - 1 do
      Dst[X] := $FF;
    Inc(Dst, Stash.Params.Width);
  end;
  Stash.DirtyRect[0] := fons__mini(Stash.DirtyRect[0], GX);
  Stash.DirtyRect[1] := fons__mini(Stash.DirtyRect[1], GY);
  Stash.DirtyRect[2] := fons__maxi(Stash.DirtyRect[2], GX + W);
  Stash.DirtyRect[3] := fons__maxi(Stash.DirtyRect[3], GY + H);
end;

function fonsCreateInternal(Params: PFonsParams): PFonsContext;
var
  Stash: PFonsContext;
begin
  { Allocate memory for the font stash }
  Stash := AllocMem(SizeOf(TFonsContext));
  Stash.Params := Params^;
  if Assigned(Stash.Params.RenderCreate) then
    if Stash.Params.RenderCreate(Stash.Params.UserPtr, Stash.Params.Width, Stash.Params.Height) = 0 then
    begin
      fonsDeleteInternal(Stash);
      Exit(nil);
    end;
  Stash.Atlas := fons__allocAtlas(Stash.Params.Width, Stash.Params.Height, FONS_INIT_ATLAS_NODES);
  { Allocate space for fonts }
  Stash.Fonts := AllocMem(SizeOf(PFonsFont) * FONS_INIT_FONTS);
  Stash.CFonts := FONS_INIT_FONTS;
  Stash.NFonts := 0;
  { Create texture for the cache }
  Stash.ITW := 1.0 / Stash.Params.Width;
  Stash.ITH := 1.0 / Stash.Params.Height;
  Stash.TexData := AllocMem(Stash.Params.Width * Stash.Params.Height);
  Stash.DirtyRect[0] := Stash.Params.Width;
  Stash.DirtyRect[1] := Stash.Params.Height;
  Stash.DirtyRect[2] := 0;
  Stash.DirtyRect[3] := 0;
  { Add white rect at 0,0 for debug drawing }
  fons__addWhiteRect(Stash, 2, 2);
  fonsPushState(Stash);
  fonsClearState(Stash);
  Result := Stash;
end;

function fons__getState(Stash: PFonsContext): PFonsState; inline;
begin
  Result := @Stash.States[Stash.NStates - 1];
end;

function fonsAddFallbackFont(Stash: PFonsContext; Base, Fallback: Integer): Integer;
var
  BaseFont: PFonsFont;
begin
  BaseFont := Stash.Fonts[Base];
  if BaseFont.NFallbacks < FONS_MAX_FALLBACKS then
  begin
    BaseFont.Fallbacks[BaseFont.NFallbacks] := Fallback;
    Inc(BaseFont.NFallbacks);
    Exit(1);
  end;
  Result := 0;
end;

procedure fonsResetFallbackFont(Stash: PFonsContext; Base: Integer);
var
  I: Integer;
  BaseFont: PFonsFont;
begin
  BaseFont := Stash.Fonts[Base];
  BaseFont.NFallbacks := 0;
  BaseFont.NGlyphs := 0;
  for I := 0 to FONS_HASH_LUT_SIZE - 1 do
    BaseFont.Lut[I] := -1;
end;

procedure fonsSetSize(Stash: PFonsContext; Size: Single);
begin
  fons__getState(Stash).Size := Size;
end;

procedure fonsSetColor(Stash: PFonsContext; Color: Cardinal);
begin
  fons__getState(Stash).Color := Color;
end;

procedure fonsSetSpacing(Stash: PFonsContext; Spacing: Single);
begin
  fons__getState(Stash).Spacing := Spacing;
end;

procedure fonsSetBlur(Stash: PFonsContext; Blur: Single);
begin
  fons__getState(Stash).Blur := Blur;
end;

procedure fonsSetAlign(Stash: PFonsContext; Align: Integer);
begin
  fons__getState(Stash).Align := Align;
end;

procedure fonsSetFont(Stash: PFonsContext; Font: Integer);
begin
  fons__getState(Stash).Font := Font;
end;

procedure fonsPushState(Stash: PFonsContext);
begin
  if Stash.NStates >= FONS_MAX_STATES then
  begin
    if Assigned(Stash.HandleError) then
      Stash.HandleError(Stash.ErrorUptr, FONS_STATES_OVERFLOW, 0);
    Exit;
  end;
  if Stash.NStates > 0 then
    Stash.States[Stash.NStates] := Stash.States[Stash.NStates - 1];
  Inc(Stash.NStates);
end;

procedure fonsPopState(Stash: PFonsContext);
begin
  if Stash.NStates <= 1 then
  begin
    if Assigned(Stash.HandleError) then
      Stash.HandleError(Stash.ErrorUptr, FONS_STATES_UNDERFLOW, 0);
    Exit;
  end;
  Dec(Stash.NStates);
end;

procedure fonsClearState(Stash: PFonsContext);
var
  State: PFonsState;
begin
  State := fons__getState(Stash);
  State.Size := 12.0;
  State.Color := $FFFFFFFF;
  State.Font := 0;
  State.Blur := 0;
  State.Spacing := 0;
  State.Align := FONS_ALIGN_LEFT or FONS_ALIGN_BASELINE;
end;

procedure fons__freeFont(Font: PFonsFont);
begin
  if Font = nil then
    Exit;
  if Font.Glyphs <> nil then
    FreeMem(Font.Glyphs);
  if Font.FreeData and (Font.Data <> nil) then
    FreeMem(Font.Data);
  Font.Name := '';
  FreeMem(Font);
end;

function fons__allocFont(Stash: PFonsContext): Integer;
var
  Font: PFonsFont;
begin
  if Stash.NFonts + 1 > Stash.CFonts then
  begin
    if Stash.CFonts = 0 then
      Stash.CFonts := 8
    else
      Stash.CFonts := Stash.CFonts * 2;
    ReallocMem(Stash.Fonts, SizeOf(PFonsFont) * Stash.CFonts);
    if Stash.Fonts = nil then
      Exit(-1);
  end;
  Font := AllocMem(SizeOf(TFonsFont));
  Font.Glyphs := GetMem(SizeOf(TFonsGlyph) * FONS_INIT_GLYPHS);
  Font.CGlyphs := FONS_INIT_GLYPHS;
  Font.NGlyphs := 0;
  Stash.Fonts[Stash.NFonts] := Font;
  Inc(Stash.NFonts);
  Result := Stash.NFonts - 1;
end;

function fonsAddFont(Stash: PFonsContext; const Name, Path: string; FontIndex: Integer): Integer;
var
  F: TFileStream;
  Data: PByte;
  DataSize: Integer;
begin
  Result := FONS_INVALID;
  if not FileExists(Path) then
    Exit;
  { Read in the font data }
  try
    F := TFileStream.Create(Path, fmOpenRead or fmShareDenyWrite);
  except
    Exit;
  end;
  try
    DataSize := F.Size;
    Data := GetMem(DataSize);
    if F.Read(Data^, DataSize) <> DataSize then
    begin
      FreeMem(Data);
      Exit;
    end;
  finally
    F.Free;
  end;
  Result := fonsAddFontMem(Stash, Name, Data, DataSize, True, FontIndex);
end;

function fonsAddFontMem(Stash: PFonsContext; const Name: string; Data: PByte; DataSize: Integer;
  FreeData: Boolean; FontIndex: Integer): Integer;
var
  I, Ascent, Descent, FH, LineGap, Idx: Integer;
  Font: PFonsFont;
begin
  Idx := fons__allocFont(Stash);
  if Idx = FONS_INVALID then
  begin
    if FreeData then
      FreeMem(Data);
    Exit(FONS_INVALID);
  end;
  Font := Stash.Fonts[Idx];
  Font.Name := Copy(Name, 1, 63);
  { Init hash lookup }
  for I := 0 to FONS_HASH_LUT_SIZE - 1 do
    Font.Lut[I] := -1;
  { Read in the font data }
  Font.DataSize := DataSize;
  Font.Data := Data;
  Font.FreeData := FreeData;
  { Init font }
  if not fons__tt_loadFont(Font.Font, Data, DataSize, FontIndex) then
  begin
    fons__freeFont(Font);
    Dec(Stash.NFonts);
    Exit(FONS_INVALID);
  end;
  { Store normalized line height. The real line height is got by multiplying
    the lineh by font size }
  fons__tt_getFontVMetrics(Font.Font, @Ascent, @Descent, @LineGap);
  Inc(Ascent, LineGap);
  FH := Ascent - Descent;
  Font.Ascender := Ascent / FH;
  Font.Descender := Descent / FH;
  Font.LineH := Font.Ascender - Font.Descender;
  Result := Idx;
end;

function fonsGetFontByName(Stash: PFonsContext; const Name: string): Integer;
var
  I: Integer;
begin
  for I := 0 to Stash.NFonts - 1 do
    if Stash.Fonts[I].Name = Name then
      Exit(I);
  Result := FONS_INVALID;
end;

function fons__allocGlyph(Font: PFonsFont): PFonsGlyph;
begin
  if Font.NGlyphs + 1 > Font.CGlyphs then
  begin
    if Font.CGlyphs = 0 then
      Font.CGlyphs := 8
    else
      Font.CGlyphs := Font.CGlyphs * 2;
    ReallocMem(Font.Glyphs, SizeOf(TFonsGlyph) * Font.CGlyphs);
    if Font.Glyphs = nil then
      Exit(nil);
  end;
  Inc(Font.NGlyphs);
  Result := @Font.Glyphs[Font.NGlyphs - 1];
end;

{ Based on Exponential blur, Jani Huhtanen, 2006 }

const
  APREC = 16;
  ZPREC = 7;

procedure fons__blurCols(Dst: PByte; W, H, DstStride, Alpha: Integer);
var
  X, Y, Z: Integer;
begin
  for Y := 0 to H - 1 do
  begin
    { Force zero border }
    Z := 0;
    for X := 1 to W - 1 do
    begin
      Z := Z + SarLongint(Alpha * ((Integer(Dst[X]) shl ZPREC) - Z), APREC);
      Dst[X] := Byte(SarLongint(Z, ZPREC));
    end;
    { Force zero border }
    Dst[W - 1] := 0;
    Z := 0;
    for X := W - 2 downto 0 do
    begin
      Z := Z + SarLongint(Alpha * ((Integer(Dst[X]) shl ZPREC) - Z), APREC);
      Dst[X] := Byte(SarLongint(Z, ZPREC));
    end;
    { Force zero border }
    Dst[0] := 0;
    Inc(Dst, DstStride);
  end;
end;

procedure fons__blurRows(Dst: PByte; W, H, DstStride, Alpha: Integer);
var
  X, Y, Z: Integer;
begin
  for X := 0 to W - 1 do
  begin
    { Force zero border }
    Z := 0;
    Y := DstStride;
    while Y < H * DstStride do
    begin
      Z := Z + SarLongint(Alpha * ((Integer(Dst[Y]) shl ZPREC) - Z), APREC);
      Dst[Y] := Byte(SarLongint(Z, ZPREC));
      Inc(Y, DstStride);
    end;
    { Force zero border }
    Dst[(H - 1) * DstStride] := 0;
    Z := 0;
    Y := (H - 2) * DstStride;
    while Y >= 0 do
    begin
      Z := Z + SarLongint(Alpha * ((Integer(Dst[Y]) shl ZPREC) - Z), APREC);
      Dst[Y] := Byte(SarLongint(Z, ZPREC));
      Dec(Y, DstStride);
    end;
    { Force zero border }
    Dst[0] := 0;
    Inc(Dst);
  end;
end;

procedure fons__blur(Dst: PByte; W, H, DstStride, Blur: Integer);
var
  Alpha: Integer;
  Sigma: Single;
begin
  if Blur < 1 then
    Exit;
  { Calculate the alpha such that 90% of the kernel is within the radius.
    (Kernel extends to infinity) }
  Sigma := Blur * 0.57735; { 1 / sqrt(3) }
  Alpha := Trunc((1 shl APREC) * (1.0 - Exp(-2.3 / (Sigma + 1.0))));
  fons__blurRows(Dst, W, H, DstStride, Alpha);
  fons__blurCols(Dst, W, H, DstStride, Alpha);
  fons__blurRows(Dst, W, H, DstStride, Alpha);
  fons__blurCols(Dst, W, H, DstStride, Alpha);
end;

function fons__getGlyph(Stash: PFonsContext; Font: PFonsFont; Codepoint: Cardinal;
  ISize, IBlur: SmallInt; BitmapOption: Integer): PFonsGlyph;
var
  I, G, Advance, Lsb, X0, Y0, X1, Y1, GW, GH, GX, GY, X, Y, Pad, FallbackIndex: Integer;
  Scale, Size: Single;
  Glyph: PFonsGlyph;
  H: Cardinal;
  Added: Boolean;
  BDst, Dst: PByte;
  RenderFont, FallbackFont: PFonsFont;
begin
  Glyph := nil;
  Size := ISize / 10.0;
  RenderFont := Font;
  if ISize < 2 then
    Exit(nil);
  if IBlur > 20 then
    IBlur := 20;
  Pad := IBlur + 2;
  { Find code point and size }
  H := fons__hashint(Codepoint) and (FONS_HASH_LUT_SIZE - 1);
  I := Font.Lut[H];
  while I <> -1 do
  begin
    if (Font.Glyphs[I].Codepoint = Codepoint) and (Font.Glyphs[I].Size = ISize) and (Font.Glyphs[I].Blur = IBlur) then
    begin
      Glyph := @Font.Glyphs[I];
      if (BitmapOption = FONS_GLYPH_BITMAP_OPTIONAL) or ((Glyph.X0 >= 0) and (Glyph.Y0 >= 0)) then
        Exit(Glyph);
      { At this point, glyph exists but the bitmap data is not yet created }
      Break;
    end;
    I := Font.Glyphs[I].Next;
  end;
  { Create a new glyph or rasterize bitmap data for a cached glyph }
  G := fons__tt_getGlyphIndex(Font.Font, Codepoint);
  { Try to find the glyph in fallback fonts }
  if G = 0 then
  begin
    for I := 0 to Font.NFallbacks - 1 do
    begin
      FallbackFont := Stash.Fonts[Font.Fallbacks[I]];
      FallbackIndex := fons__tt_getGlyphIndex(FallbackFont.Font, Codepoint);
      if FallbackIndex <> 0 then
      begin
        G := FallbackIndex;
        RenderFont := FallbackFont;
        Break;
      end;
    end;
    { It is possible that we did not find a fallback glyph. In that case the
      glyph index 'g' is 0, and we'll proceed below and cache empty glyph }
  end;
  Scale := fons__tt_getPixelHeightScale(RenderFont.Font, Size);
  fons__tt_buildGlyphBitmap(RenderFont.Font, G, Size, Scale, @Advance, @Lsb, @X0, @Y0, @X1, @Y1);
  GW := X1 - X0 + Pad * 2;
  GH := Y1 - Y0 + Pad * 2;
  { Determines the spot to draw glyph in the atlas }
  if BitmapOption = FONS_GLYPH_BITMAP_REQUIRED then
  begin
    { Find free spot for the rect in the atlas }
    Added := fons__atlasAddRect(Stash.Atlas, GW, GH, @GX, @GY);
    if (not Added) and Assigned(Stash.HandleError) then
    begin
      { Atlas is full, let the user to resize the atlas (or not), and try again }
      Stash.HandleError(Stash.ErrorUptr, FONS_ATLAS_FULL, 0);
      Added := fons__atlasAddRect(Stash.Atlas, GW, GH, @GX, @GY);
    end;
    if not Added then
      Exit(nil);
  end
  else
  begin
    { Negative coordinate indicates there is no bitmap data created }
    GX := -1;
    GY := -1;
  end;
  { Init glyph }
  if Glyph = nil then
  begin
    Glyph := fons__allocGlyph(Font);
    Glyph.Codepoint := Codepoint;
    Glyph.Size := ISize;
    Glyph.Blur := IBlur;
    { Insert char to hash lookup }
    Glyph.Next := Font.Lut[H];
    Font.Lut[H] := Font.NGlyphs - 1;
  end;
  Glyph.Index := G;
  Glyph.X0 := SmallInt(GX);
  Glyph.Y0 := SmallInt(GY);
  Glyph.X1 := SmallInt(Glyph.X0 + GW);
  Glyph.Y1 := SmallInt(Glyph.Y0 + GH);
  Glyph.XAdv := SmallInt(Trunc(Scale * Advance * 10.0));
  Glyph.XOff := SmallInt(X0 - Pad);
  Glyph.YOff := SmallInt(Y0 - Pad);
  if BitmapOption = FONS_GLYPH_BITMAP_OPTIONAL then
    Exit(Glyph);
  { Rasterize }
  Dst := @Stash.TexData[(Glyph.X0 + Pad) + (Glyph.Y0 + Pad) * Stash.Params.Width];
  fons__tt_renderGlyphBitmap(RenderFont.Font, Dst, GW - Pad * 2, GH - Pad * 2, Stash.Params.Width, Scale, Scale, G);
  { Make sure there is one pixel empty border }
  Dst := @Stash.TexData[Glyph.X0 + Glyph.Y0 * Stash.Params.Width];
  for Y := 0 to GH - 1 do
  begin
    Dst[Y * Stash.Params.Width] := 0;
    Dst[GW - 1 + Y * Stash.Params.Width] := 0;
  end;
  for X := 0 to GW - 1 do
  begin
    Dst[X] := 0;
    Dst[X + (GH - 1) * Stash.Params.Width] := 0;
  end;
  { Blur }
  if IBlur > 0 then
  begin
    BDst := @Stash.TexData[Glyph.X0 + Glyph.Y0 * Stash.Params.Width];
    fons__blur(BDst, GW, GH, Stash.Params.Width, IBlur);
  end;
  Stash.DirtyRect[0] := fons__mini(Stash.DirtyRect[0], Glyph.X0);
  Stash.DirtyRect[1] := fons__mini(Stash.DirtyRect[1], Glyph.Y0);
  Stash.DirtyRect[2] := fons__maxi(Stash.DirtyRect[2], Glyph.X1);
  Stash.DirtyRect[3] := fons__maxi(Stash.DirtyRect[3], Glyph.Y1);
  Result := Glyph;
end;

procedure fons__getQuad(Stash: PFonsContext; Font: PFonsFont; PrevGlyphIndex: Integer; Glyph: PFonsGlyph;
  Scale, Spacing: Single; var X, Y: Single; out Q: TFonsQuad);
var
  RX, RY, XOff, YOff, X0, Y0, X1, Y1, Adv: Single;
begin
  if PrevGlyphIndex <> -1 then
  begin
    Adv := fons__tt_getGlyphKernAdvance(Font.Font, PrevGlyphIndex, Glyph.Index) * Scale;
    X := X + Trunc(Adv + Spacing + 0.5);
  end;
  { Each glyph has 2px border to allow good interpolation, one pixel to
    prevent leaking, and one to allow good interpolation for rendering. Inset
    the texture region by one pixel for correct interpolation }
  XOff := SmallInt(Glyph.XOff + 1);
  YOff := SmallInt(Glyph.YOff + 1);
  X0 := Glyph.X0 + 1;
  Y0 := Glyph.Y0 + 1;
  X1 := Glyph.X1 - 1;
  Y1 := Glyph.Y1 - 1;
  if Stash.Params.Flags and FONS_ZERO_TOPLEFT <> 0 then
  begin
    RX := Floor(X + XOff);
    RY := Floor(Y + YOff);
    Q.X0 := RX;
    Q.Y0 := RY;
    Q.X1 := RX + X1 - X0;
    Q.Y1 := RY + Y1 - Y0;
    Q.S0 := X0 * Stash.ITW;
    Q.T0 := Y0 * Stash.ITH;
    Q.S1 := X1 * Stash.ITW;
    Q.T1 := Y1 * Stash.ITH;
  end
  else
  begin
    RX := Floor(X + XOff);
    RY := Floor(Y - YOff);
    Q.X0 := RX;
    Q.Y0 := RY;
    Q.X1 := RX + X1 - X0;
    Q.Y1 := RY - Y1 + Y0;
    Q.S0 := X0 * Stash.ITW;
    Q.T0 := Y0 * Stash.ITH;
    Q.S1 := X1 * Stash.ITW;
    Q.T1 := Y1 * Stash.ITH;
  end;
  X := X + Trunc(Glyph.XAdv / 10.0 + 0.5);
end;

procedure fons__flush(Stash: PFonsContext);
begin
  { Flush texture }
  if (Stash.DirtyRect[0] < Stash.DirtyRect[2]) and (Stash.DirtyRect[1] < Stash.DirtyRect[3]) then
  begin
    if Assigned(Stash.Params.RenderUpdate) then
      Stash.Params.RenderUpdate(Stash.Params.UserPtr, @Stash.DirtyRect[0], Stash.TexData);
    { Reset dirty rect }
    Stash.DirtyRect[0] := Stash.Params.Width;
    Stash.DirtyRect[1] := Stash.Params.Height;
    Stash.DirtyRect[2] := 0;
    Stash.DirtyRect[3] := 0;
  end;
  { Flush triangles }
  if Stash.NVerts > 0 then
  begin
    if Assigned(Stash.Params.RenderDraw) then
      Stash.Params.RenderDraw(Stash.Params.UserPtr, @Stash.Verts[0], @Stash.TCoords[0], @Stash.Colors[0], Stash.NVerts);
    Stash.NVerts := 0;
  end;
end;

procedure fons__vertex(Stash: PFonsContext; X, Y, S, T: Single; C: Cardinal); inline;
begin
  Stash.Verts[Stash.NVerts * 2 + 0] := X;
  Stash.Verts[Stash.NVerts * 2 + 1] := Y;
  Stash.TCoords[Stash.NVerts * 2 + 0] := S;
  Stash.TCoords[Stash.NVerts * 2 + 1] := T;
  Stash.Colors[Stash.NVerts] := C;
  Inc(Stash.NVerts);
end;

function fons__getVertAlign(Stash: PFonsContext; Font: PFonsFont; Align: Integer; ISize: SmallInt): Single;
begin
  if Stash.Params.Flags and FONS_ZERO_TOPLEFT <> 0 then
  begin
    if Align and FONS_ALIGN_TOP <> 0 then
      Exit(Font.Ascender * ISize / 10.0)
    else if Align and FONS_ALIGN_MIDDLE <> 0 then
      Exit((Font.Ascender + Font.Descender) / 2.0 * ISize / 10.0)
    else if Align and FONS_ALIGN_BASELINE <> 0 then
      Exit(0.0)
    else if Align and FONS_ALIGN_BOTTOM <> 0 then
      Exit(Font.Descender * ISize / 10.0);
  end
  else
  begin
    if Align and FONS_ALIGN_TOP <> 0 then
      Exit(-Font.Ascender * ISize / 10.0)
    else if Align and FONS_ALIGN_MIDDLE <> 0 then
      Exit(-(Font.Ascender + Font.Descender) / 2.0 * ISize / 10.0)
    else if Align and FONS_ALIGN_BASELINE <> 0 then
      Exit(0.0)
    else if Align and FONS_ALIGN_BOTTOM <> 0 then
      Exit(-Font.Descender * ISize / 10.0);
  end;
  Result := 0.0;
end;

function fonsDrawText(Stash: PFonsContext; X, Y: Single; Str, EndStr: PAnsiChar): Single;
var
  State: PFonsState;
  Codepoint, Utf8State: Cardinal;
  Glyph: PFonsGlyph;
  Q: TFonsQuad;
  PrevGlyphIndex: Integer;
  ISize, IBlur: SmallInt;
  Scale, Width: Single;
  Font: PFonsFont;
begin
  if Stash = nil then
    Exit(X);
  State := fons__getState(Stash);
  Codepoint := 0;
  Utf8State := 0;
  PrevGlyphIndex := -1;
  ISize := SmallInt(Trunc(State.Size * 10.0));
  IBlur := SmallInt(Trunc(State.Blur));
  if (State.Font < 0) or (State.Font >= Stash.NFonts) then
    Exit(X);
  Font := Stash.Fonts[State.Font];
  if Font.Data = nil then
    Exit(X);
  Scale := fons__tt_getPixelHeightScale(Font.Font, ISize / 10.0);
  if EndStr = nil then
    EndStr := Str + StrLen(Str);
  { Align horizontally }
  if State.Align and FONS_ALIGN_LEFT <> 0 then
    { Empty }
  else if State.Align and FONS_ALIGN_RIGHT <> 0 then
  begin
    Width := fonsTextBounds(Stash, X, Y, Str, EndStr, nil);
    X := X - Width;
  end
  else if State.Align and FONS_ALIGN_CENTER <> 0 then
  begin
    Width := fonsTextBounds(Stash, X, Y, Str, EndStr, nil);
    X := X - Width * 0.5;
  end;
  { Align vertically }
  Y := Y + fons__getVertAlign(Stash, Font, State.Align, ISize);
  while Str <> EndStr do
  begin
    if fons__decutf8(Utf8State, Codepoint, Byte(Str^)) <> 0 then
    begin
      Inc(Str);
      Continue;
    end;
    Glyph := fons__getGlyph(Stash, Font, Codepoint, ISize, IBlur, FONS_GLYPH_BITMAP_REQUIRED);
    if Glyph <> nil then
    begin
      fons__getQuad(Stash, Font, PrevGlyphIndex, Glyph, Scale, State.Spacing, X, Y, Q);
      if Stash.NVerts + 6 > FONS_VERTEX_COUNT then
        fons__flush(Stash);
      fons__vertex(Stash, Q.X0, Q.Y0, Q.S0, Q.T0, State.Color);
      fons__vertex(Stash, Q.X1, Q.Y1, Q.S1, Q.T1, State.Color);
      fons__vertex(Stash, Q.X1, Q.Y0, Q.S1, Q.T0, State.Color);
      fons__vertex(Stash, Q.X0, Q.Y0, Q.S0, Q.T0, State.Color);
      fons__vertex(Stash, Q.X0, Q.Y1, Q.S0, Q.T1, State.Color);
      fons__vertex(Stash, Q.X1, Q.Y1, Q.S1, Q.T1, State.Color);
    end;
    if Glyph <> nil then
      PrevGlyphIndex := Glyph.Index
    else
      PrevGlyphIndex := -1;
    Inc(Str);
  end;
  fons__flush(Stash);
  Result := X;
end;

function fonsTextIterInit(Stash: PFonsContext; Iter: PFonsTextIter; X, Y: Single;
  Str, EndStr: PAnsiChar; BitmapOption: Integer): Integer;
var
  State: PFonsState;
  Width: Single;
begin
  FillChar(Iter^, SizeOf(Iter^), 0);
  if Stash = nil then
    Exit(0);
  State := fons__getState(Stash);
  if (State.Font < 0) or (State.Font >= Stash.NFonts) then
    Exit(0);
  Iter.Font := Stash.Fonts[State.Font];
  if Iter.Font.Data = nil then
    Exit(0);
  Iter.ISize := SmallInt(Trunc(State.Size * 10.0));
  Iter.IBlur := SmallInt(Trunc(State.Blur));
  Iter.Scale := fons__tt_getPixelHeightScale(Iter.Font.Font, Iter.ISize / 10.0);
  { Align horizontally }
  if State.Align and FONS_ALIGN_LEFT <> 0 then
    { Empty }
  else if State.Align and FONS_ALIGN_RIGHT <> 0 then
  begin
    Width := fonsTextBounds(Stash, X, Y, Str, EndStr, nil);
    X := X - Width;
  end
  else if State.Align and FONS_ALIGN_CENTER <> 0 then
  begin
    Width := fonsTextBounds(Stash, X, Y, Str, EndStr, nil);
    X := X - Width * 0.5;
  end;
  { Align vertically }
  Y := Y + fons__getVertAlign(Stash, Iter.Font, State.Align, Iter.ISize);
  if EndStr = nil then
    EndStr := Str + StrLen(Str);
  Iter.X := X;
  Iter.NextX := X;
  Iter.Y := Y;
  Iter.NextY := Y;
  Iter.Spacing := State.Spacing;
  Iter.Str := Str;
  Iter.Next := Str;
  Iter.EndStr := EndStr;
  Iter.Codepoint := 0;
  Iter.PrevGlyphIndex := -1;
  Iter.BitmapOption := BitmapOption;
  Result := 1;
end;

function fonsTextIterNext(Stash: PFonsContext; Iter: PFonsTextIter; Quad: PFonsQuad): Integer;
var
  Glyph: PFonsGlyph;
  Str: PAnsiChar;
begin
  Glyph := nil;
  Str := Iter.Next;
  Iter.Str := Iter.Next;
  if Str = Iter.EndStr then
    Exit(0);
  while Str <> Iter.EndStr do
  begin
    if fons__decutf8(Iter.Utf8State, Iter.Codepoint, Byte(Str^)) <> 0 then
    begin
      Inc(Str);
      Continue;
    end;
    Inc(Str);
    { Get glyph and quad }
    Iter.X := Iter.NextX;
    Iter.Y := Iter.NextY;
    Glyph := fons__getGlyph(Stash, Iter.Font, Iter.Codepoint, Iter.ISize, Iter.IBlur, Iter.BitmapOption);
    { If the iterator was initialized with FONS_GLYPH_BITMAP_OPTIONAL, then
      the UV coordinates of the quad will be invalid }
    if Glyph <> nil then
      fons__getQuad(Stash, Iter.Font, Iter.PrevGlyphIndex, Glyph, Iter.Scale, Iter.Spacing, Iter.NextX, Iter.NextY, Quad^);
    if Glyph <> nil then
      Iter.PrevGlyphIndex := Glyph.Index
    else
      Iter.PrevGlyphIndex := -1;
    Break;
  end;
  Iter.Next := Str;
  Result := 1;
end;

procedure fonsDrawDebug(Stash: PFonsContext; X, Y: Single);
var
  I, W, H: Integer;
  U, V: Single;
  N: PFonsAtlasNode;
begin
  W := Stash.Params.Width;
  H := Stash.Params.Height;
  if W = 0 then U := 0 else U := 1.0 / W;
  if H = 0 then V := 0 else V := 1.0 / H;
  if Stash.NVerts + 6 + 6 > FONS_VERTEX_COUNT then
    fons__flush(Stash);
  { Draw background }
  fons__vertex(Stash, X + 0, Y + 0, U, V, $0FFFFFFF);
  fons__vertex(Stash, X + W, Y + H, U, V, $0FFFFFFF);
  fons__vertex(Stash, X + W, Y + 0, U, V, $0FFFFFFF);
  fons__vertex(Stash, X + 0, Y + 0, U, V, $0FFFFFFF);
  fons__vertex(Stash, X + 0, Y + H, U, V, $0FFFFFFF);
  fons__vertex(Stash, X + W, Y + H, U, V, $0FFFFFFF);
  { Draw texture }
  fons__vertex(Stash, X + 0, Y + 0, 0, 0, $FFFFFFFF);
  fons__vertex(Stash, X + W, Y + H, 1, 1, $FFFFFFFF);
  fons__vertex(Stash, X + W, Y + 0, 1, 0, $FFFFFFFF);
  fons__vertex(Stash, X + 0, Y + 0, 0, 0, $FFFFFFFF);
  fons__vertex(Stash, X + 0, Y + H, 0, 1, $FFFFFFFF);
  fons__vertex(Stash, X + W, Y + H, 1, 1, $FFFFFFFF);
  { Debug draw atlas }
  for I := 0 to Stash.Atlas.NNodes - 1 do
  begin
    N := @Stash.Atlas.Nodes[I];
    if Stash.NVerts + 6 > FONS_VERTEX_COUNT then
      fons__flush(Stash);
    fons__vertex(Stash, X + N.X + 0, Y + N.Y + 0, U, V, $C00000FF);
    fons__vertex(Stash, X + N.X + N.Width, Y + N.Y + 1, U, V, $C00000FF);
    fons__vertex(Stash, X + N.X + N.Width, Y + N.Y + 0, U, V, $C00000FF);
    fons__vertex(Stash, X + N.X + 0, Y + N.Y + 0, U, V, $C00000FF);
    fons__vertex(Stash, X + N.X + 0, Y + N.Y + 1, U, V, $C00000FF);
    fons__vertex(Stash, X + N.X + N.Width, Y + N.Y + 1, U, V, $C00000FF);
  end;
  fons__flush(Stash);
end;

function fonsTextBounds(Stash: PFonsContext; X, Y: Single; Str, EndStr: PAnsiChar; Bounds: PSingle): Single;
var
  State: PFonsState;
  Codepoint, Utf8State: Cardinal;
  Q: TFonsQuad;
  Glyph: PFonsGlyph;
  PrevGlyphIndex: Integer;
  ISize, IBlur: SmallInt;
  Scale, StartX, Advance, MinX, MinY, MaxX, MaxY: Single;
  Font: PFonsFont;
begin
  if Stash = nil then
    Exit(0);
  State := fons__getState(Stash);
  Codepoint := 0;
  Utf8State := 0;
  PrevGlyphIndex := -1;
  ISize := SmallInt(Trunc(State.Size * 10.0));
  IBlur := SmallInt(Trunc(State.Blur));
  if (State.Font < 0) or (State.Font >= Stash.NFonts) then
    Exit(0);
  Font := Stash.Fonts[State.Font];
  if Font.Data = nil then
    Exit(0);
  Scale := fons__tt_getPixelHeightScale(Font.Font, ISize / 10.0);
  { Align vertically }
  Y := Y + fons__getVertAlign(Stash, Font, State.Align, ISize);
  MinX := X;
  MaxX := X;
  MinY := Y;
  MaxY := Y;
  StartX := X;
  if EndStr = nil then
    EndStr := Str + StrLen(Str);
  while Str <> EndStr do
  begin
    if fons__decutf8(Utf8State, Codepoint, Byte(Str^)) <> 0 then
    begin
      Inc(Str);
      Continue;
    end;
    Glyph := fons__getGlyph(Stash, Font, Codepoint, ISize, IBlur, FONS_GLYPH_BITMAP_OPTIONAL);
    if Glyph <> nil then
    begin
      fons__getQuad(Stash, Font, PrevGlyphIndex, Glyph, Scale, State.Spacing, X, Y, Q);
      if Q.X0 < MinX then MinX := Q.X0;
      if Q.X1 > MaxX then MaxX := Q.X1;
      if Stash.Params.Flags and FONS_ZERO_TOPLEFT <> 0 then
      begin
        if Q.Y0 < MinY then MinY := Q.Y0;
        if Q.Y1 > MaxY then MaxY := Q.Y1;
      end
      else
      begin
        if Q.Y1 < MinY then MinY := Q.Y1;
        if Q.Y0 > MaxY then MaxY := Q.Y0;
      end;
    end;
    if Glyph <> nil then
      PrevGlyphIndex := Glyph.Index
    else
      PrevGlyphIndex := -1;
    Inc(Str);
  end;
  Advance := X - StartX;
  { Align horizontally }
  if State.Align and FONS_ALIGN_LEFT <> 0 then
    { Empty }
  else if State.Align and FONS_ALIGN_RIGHT <> 0 then
  begin
    MinX := MinX - Advance;
    MaxX := MaxX - Advance;
  end
  else if State.Align and FONS_ALIGN_CENTER <> 0 then
  begin
    MinX := MinX - Advance * 0.5;
    MaxX := MaxX - Advance * 0.5;
  end;
  if Bounds <> nil then
  begin
    Bounds[0] := MinX;
    Bounds[1] := MinY;
    Bounds[2] := MaxX;
    Bounds[3] := MaxY;
  end;
  Result := Advance;
end;

procedure fonsVertMetrics(Stash: PFonsContext; Ascender, Descender, LineH: PSingle);
var
  Font: PFonsFont;
  State: PFonsState;
  ISize: SmallInt;
begin
  if Stash = nil then
    Exit;
  State := fons__getState(Stash);
  if (State.Font < 0) or (State.Font >= Stash.NFonts) then
    Exit;
  Font := Stash.Fonts[State.Font];
  ISize := SmallInt(Trunc(State.Size * 10.0));
  if Font.Data = nil then
    Exit;
  if Ascender <> nil then
    Ascender^ := Font.Ascender * ISize / 10.0;
  if Descender <> nil then
    Descender^ := Font.Descender * ISize / 10.0;
  if LineH <> nil then
    LineH^ := Font.LineH * ISize / 10.0;
end;

procedure fonsLineBounds(Stash: PFonsContext; Y: Single; MinY, MaxY: PSingle);
var
  Font: PFonsFont;
  State: PFonsState;
  ISize: SmallInt;
begin
  if Stash = nil then
    Exit;
  State := fons__getState(Stash);
  if (State.Font < 0) or (State.Font >= Stash.NFonts) then
    Exit;
  Font := Stash.Fonts[State.Font];
  ISize := SmallInt(Trunc(State.Size * 10.0));
  if Font.Data = nil then
    Exit;
  Y := Y + fons__getVertAlign(Stash, Font, State.Align, ISize);
  if Stash.Params.Flags and FONS_ZERO_TOPLEFT <> 0 then
  begin
    MinY^ := Y - Font.Ascender * ISize / 10.0;
    MaxY^ := MinY^ + Font.LineH * ISize / 10.0;
  end
  else
  begin
    MaxY^ := Y + Font.Descender * ISize / 10.0;
    MinY^ := MaxY^ - Font.LineH * ISize / 10.0;
  end;
end;

function fonsGetTextureData(Stash: PFonsContext; Width, Height: PInteger): PByte;
begin
  if Width <> nil then
    Width^ := Stash.Params.Width;
  if Height <> nil then
    Height^ := Stash.Params.Height;
  Result := Stash.TexData;
end;

function fonsValidateTexture(Stash: PFonsContext; Dirty: PInteger): Integer;
begin
  if (Stash.DirtyRect[0] < Stash.DirtyRect[2]) and (Stash.DirtyRect[1] < Stash.DirtyRect[3]) then
  begin
    Dirty[0] := Stash.DirtyRect[0];
    Dirty[1] := Stash.DirtyRect[1];
    Dirty[2] := Stash.DirtyRect[2];
    Dirty[3] := Stash.DirtyRect[3];
    { Reset dirty rect }
    Stash.DirtyRect[0] := Stash.Params.Width;
    Stash.DirtyRect[1] := Stash.Params.Height;
    Stash.DirtyRect[2] := 0;
    Stash.DirtyRect[3] := 0;
    Exit(1);
  end;
  Result := 0;
end;

procedure fonsDeleteInternal(Stash: PFonsContext);
var
  I: Integer;
begin
  if Stash = nil then
    Exit;
  if Assigned(Stash.Params.RenderDelete) then
    Stash.Params.RenderDelete(Stash.Params.UserPtr);
  for I := 0 to Stash.NFonts - 1 do
    fons__freeFont(Stash.Fonts[I]);
  if Stash.Atlas <> nil then
    fons__deleteAtlas(Stash.Atlas);
  if Stash.Fonts <> nil then
    FreeMem(Stash.Fonts);
  if Stash.TexData <> nil then
    FreeMem(Stash.TexData);
  FreeMem(Stash);
end;

procedure fonsSetErrorCallback(Stash: PFonsContext; Callback: TFonsErrorCallback; UserPtr: Pointer);
begin
  if Stash = nil then
    Exit;
  Stash.HandleError := Callback;
  Stash.ErrorUptr := UserPtr;
end;

procedure fonsGetAtlasSize(Stash: PFonsContext; Width, Height: PInteger);
begin
  if Stash = nil then
    Exit;
  Width^ := Stash.Params.Width;
  Height^ := Stash.Params.Height;
end;

function fonsExpandAtlas(Stash: PFonsContext; Width, Height: Integer): Integer;
var
  I, MaxY: Integer;
  Data, Dst, Src: PByte;
begin
  MaxY := 0;
  if Stash = nil then
    Exit(0);
  Width := fons__maxi(Width, Stash.Params.Width);
  Height := fons__maxi(Height, Stash.Params.Height);
  if (Width = Stash.Params.Width) and (Height = Stash.Params.Height) then
    Exit(1);
  { Flush pending glyphs }
  fons__flush(Stash);
  { Create new texture }
  if Assigned(Stash.Params.RenderResize) then
    if Stash.Params.RenderResize(Stash.Params.UserPtr, Width, Height) = 0 then
      Exit(0);
  { Copy old texture data over }
  Data := GetMem(Width * Height);
  for I := 0 to Stash.Params.Height - 1 do
  begin
    Dst := @Data[I * Width];
    Src := @Stash.TexData[I * Stash.Params.Width];
    Move(Src^, Dst^, Stash.Params.Width);
    if Width > Stash.Params.Width then
      FillChar(Dst[Stash.Params.Width], Width - Stash.Params.Width, 0);
  end;
  if Height > Stash.Params.Height then
    FillChar(Data[Stash.Params.Height * Width], (Height - Stash.Params.Height) * Width, 0);
  FreeMem(Stash.TexData);
  Stash.TexData := Data;
  { Increase atlas size }
  fons__atlasExpand(Stash.Atlas, Width, Height);
  { Add existing data as dirty }
  for I := 0 to Stash.Atlas.NNodes - 1 do
    MaxY := fons__maxi(MaxY, Stash.Atlas.Nodes[I].Y);
  Stash.DirtyRect[0] := 0;
  Stash.DirtyRect[1] := 0;
  Stash.DirtyRect[2] := Stash.Params.Width;
  Stash.DirtyRect[3] := MaxY;
  Stash.Params.Width := Width;
  Stash.Params.Height := Height;
  Stash.ITW := 1.0 / Stash.Params.Width;
  Stash.ITH := 1.0 / Stash.Params.Height;
  Result := 1;
end;

function fonsResetAtlas(Stash: PFonsContext; Width, Height: Integer): Integer;
var
  I, J: Integer;
  Font: PFonsFont;
begin
  if Stash = nil then
    Exit(0);
  { Flush pending glyphs }
  fons__flush(Stash);
  { Create new texture }
  if Assigned(Stash.Params.RenderResize) then
    if Stash.Params.RenderResize(Stash.Params.UserPtr, Width, Height) = 0 then
      Exit(0);
  { Reset atlas }
  fons__atlasReset(Stash.Atlas, Width, Height);
  { Clear texture data }
  ReallocMem(Stash.TexData, Width * Height);
  if Stash.TexData = nil then
    Exit(0);
  FillChar(Stash.TexData^, Width * Height, 0);
  { Reset dirty rect }
  Stash.DirtyRect[0] := Width;
  Stash.DirtyRect[1] := Height;
  Stash.DirtyRect[2] := 0;
  Stash.DirtyRect[3] := 0;
  { Reset cached glyphs }
  for I := 0 to Stash.NFonts - 1 do
  begin
    Font := Stash.Fonts[I];
    Font.NGlyphs := 0;
    for J := 0 to FONS_HASH_LUT_SIZE - 1 do
      Font.Lut[J] := -1;
  end;
  Stash.Params.Width := Width;
  Stash.Params.Height := Height;
  Stash.ITW := 1.0 / Stash.Params.Width;
  Stash.ITH := 1.0 / Stash.Params.Height;
  { Add white rect at 0,0 for debug drawing }
  fons__addWhiteRect(Stash, 2, 2);
  Result := 1;
end;

end.
