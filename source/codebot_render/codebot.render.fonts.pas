unit Codebot.Render.Fonts;

{$i render.inc}
{$pointermath on}

interface

uses
  Classes,
  Codebot.System,
  Codebot.Geometry,
  Codebot.Render.Contexts,
  Codebot.Render.TrueType,
  Codebot.Render.Textures,
  Codebot.Render.Buffers;

{ TFontGlyph describes one glyph in the texture atlas of a font. Offsets are
  in font pixels from the pen position on the baseline, with y pointing down. }

type
  TFontGlyph = record
    Codepoint: Integer;
    Index: Integer;
    Advance: Float;
    X0, Y0, X1, Y1: Float;
    S0, T0, S1, T1: Float;
  end;
  { Pointer to a TFontGlyph }
  PFontGlyph = ^TFontGlyph;

{ TFont loads a TrueType font and renders its glyphs at a fixed pixel size
  into a texture atlas. The atlas contains the printable ASCII and Latin-1
  characters. Glyphs are white with coverage stored in alpha, so they can be
  tinted by vertex color. }

  TFont = class(TContextManagedObject)
  private
    FData: array of Byte;
    FInfo: TStbttFontInfo;
    FScale: Float;
    FSize: Float;
    FAscent: Float;
    FDescent: Float;
    FLineHeight: Float;
    FGlyphs: array of TFontGlyph;
    FFallback: PFontGlyph;
    FTexture: TTexture;
    procedure Bake;
  public
    { Create a font from a TrueType file rendered at a size in pixels }
    constructor Create(const FileName: string; Size: Float = 48);
    { Create a font from a stream containing a TrueType file }
    constructor CreateFromStream(Stream: TStream; Size: Float = 48);
    destructor Destroy; override;
    { Find the glyph for a unicode codepoint, returning a fallback glyph when
      the font atlas does not contain it }
    function FindGlyph(Codepoint: Integer): PFontGlyph;
    { The kerning adjustment in font pixels between two glyphs }
    function Kerning(A, B: PFontGlyph): Float;
    { Measure the width of the widest line and the height of all lines of
      text in font pixels }
    function Measure(const Text: string): TVec2;
    { Add the glyph quads for text to a buffer which was started with
      BeginBuffer(vertQuads). The text is laid out with y pointing up, X and
      Y being the top left of the first line, and one font pixel being Scale
      units. Line breaks start a new line. }
    procedure AddText(Buffer: TColorTexVertexBuffer; const Text: string;
      X, Y, Scale: Float; const Color: TVec4);
    { The pixel height the font was rendered at }
    property Size: Float read FSize;
    { The distance in font pixels from the baseline to the top of the font }
    property Ascent: Float read FAscent;
    { The distance in font pixels from the baseline to the bottom of the font }
    property Descent: Float read FDescent;
    { The distance in font pixels between baselines of lines of text }
    property LineHeight: Float read FLineHeight;
    { The texture atlas holding the font glyphs }
    property Texture: TTexture read FTexture;
  end;

{ TFontCollection holds fonts by name, loading them from the assets/fonts
  folder when they do not exist. A font named 'roboto' is loaded from
  'assets/fonts/roboto.ttf'. }

  TFontCollection = class(TContextCollection)
  private
    function GetFont(const AName: string): TFont;
    function GetDefaultFont: TFont;
  public
    constructor Create;
    { Return a font by name or locate and create the font from an asset }
    property Font[AName: string]: TFont read GetFont; default;
    { The font named 'roboto' provided with the library assets }
    property DefaultFont: TFont read GetDefaultFont;
  end;

{ TFontExtension adds the function Fonts to the current context }

  TFontExtension = class helper for TRenderContext
  public
    { Returns the font collection for the current context }
    function Fonts: TFontCollection;
  end;

{ TTextBlock draws text using a font. The vertices are rebuilt only when a
  property changes. Text is drawn in the current modelview space with y
  pointing up, X and Y being the top left of the first line. Use the
  TWorld.WorldToSpace function to position text in a 2D world. }

  TTextBlock = class(TContextManagedObject)
  private
    FFont: TFont;
    FText: string;
    FX: Float;
    FY: Float;
    FScale: Float;
    FColor: TVec4;
    FBuffer: TColorTexVertexBuffer;
    FChanged: Boolean;
    function GetFont: TFont;
    procedure SetFont(Value: TFont);
    procedure SetText(const Value: string);
    procedure SetX(Value: Float);
    procedure SetY(Value: Float);
    procedure SetScale(Value: Float);
    procedure SetColor(const Value: TVec4);
  public
    { Create a text block optionally using a font. If no font is given the
      default font is used. }
    constructor Create(Font: TFont = nil);
    destructor Destroy; override;
    { Measure the size of the text in modelview units }
    function Measure: TVec2;
    { Draw the text. If Text is not empty it replaces the Text property. }
    procedure Draw(const Text: string = '');
    { The font used to draw text }
    property Font: TFont read GetFont write SetFont;
    { The text to draw }
    property Text: string read FText write SetText;
    { The left of the text }
    property X: Float read FX write SetX;
    { The top of the text }
    property Y: Float read FY write SetY;
    { The size of one font pixel in modelview units }
    property Scale: Float read FScale write SetScale;
    { The color of the text which defaults to opaque white }
    property Color: TVec4 read FColor write SetColor;
  end;

implementation

uses
  SysUtils;

{ Decode the next UTF-8 codepoint advancing I, returning -1 for an invalid
  sequence }

function NextCodepoint(const S: string; var I: Integer): Integer;
var
  B, N, J: Integer;
begin
  B := Ord(S[I]);
  Inc(I);
  if B < $80 then
    Exit(B);
  if B and $E0 = $C0 then
  begin
    Result := B and $1F;
    N := 1;
  end
  else if B and $F0 = $E0 then
  begin
    Result := B and $0F;
    N := 2;
  end
  else if B and $F8 = $F0 then
  begin
    Result := B and $07;
    N := 3;
  end
  else
    Exit(-1);
  for J := 1 to N do
  begin
    if (I > Length(S)) or (Ord(S[I]) and $C0 <> $80) then
      Exit(-1);
    Result := (Result shl 6) or (Ord(S[I]) and $3F);
    Inc(I);
  end;
end;

{ TFont }

const
  { Characters rendered into the atlas }
  GlyphRanges: array[0..1, 0..1] of Integer = ((32, 126), (160, 255));
  { Empty pixels around each glyph to prevent bleeding when filtering }
  GlyphPadding = 4;

constructor TFont.Create(const FileName: string; Size: Float = 48);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    CreateFromStream(Stream, Size);
  finally
    Stream.Free;
  end;
end;

constructor TFont.CreateFromStream(Stream: TStream; Size: Float = 48);
begin
  inherited Create(Ctx.Fonts);
  if Size < 4 then
    Size := 4;
  FSize := Size;
  SetLength(FData, Stream.Size - Stream.Position);
  if Length(FData) > 0 then
    Stream.ReadBuffer(FData[0], Length(FData));
  { The font information refers to the font data, which is kept for kerning }
  if (Length(FData) = 0) or (not stbtt_InitFont(FInfo, @FData[0],
    stbtt_GetFontOffsetForIndex(@FData[0], 0))) then
    raise EContextError.Create('The font data could not be read');
  Bake;
end;

destructor TFont.Destroy;
begin
  FreeManaged(FTexture);
  inherited Destroy;
end;

procedure TFont.Bake;
type
  TPlace = record
    X, Y, W, H: Integer;
  end;
var
  Places: array of TPlace;
  Coverage, Pixels: array of Byte;
  AtlasWidth, AtlasHeight, PenX, PenY, RowHeight: Integer;
  Ascent, Descent, LineGap, Advance: Integer;
  X0, Y0, X1, Y1: Integer;
  Count, R, C, I, J, Index: Integer;
  Glyph: PFontGlyph;
begin
  FScale := stbtt_ScaleForPixelHeight(FInfo, FSize);
  stbtt_GetFontVMetrics(FInfo, @Ascent, @Descent, @LineGap);
  FAscent := Ascent * FScale;
  FDescent := -Descent * FScale;
  FLineHeight := (Ascent - Descent + LineGap) * FScale;
  { Gather the glyphs which exist in the font }
  Count := 0;
  for R := Low(GlyphRanges) to High(GlyphRanges) do
    Inc(Count, GlyphRanges[R, 1] - GlyphRanges[R, 0] + 1);
  SetLength(FGlyphs, Count);
  SetLength(Places, Count);
  Count := 0;
  for R := Low(GlyphRanges) to High(GlyphRanges) do
    for C := GlyphRanges[R, 0] to GlyphRanges[R, 1] do
    begin
      Index := stbtt_FindGlyphIndex(FInfo, C);
      if (Index = 0) and (C <> 32) then
        Continue;
      Glyph := @FGlyphs[Count];
      Glyph.Codepoint := C;
      Glyph.Index := Index;
      stbtt_GetGlyphHMetrics(FInfo, Index, @Advance, nil);
      Glyph.Advance := Advance * FScale;
      stbtt_GetGlyphBitmapBox(FInfo, Index, FScale, FScale, @X0, @Y0, @X1, @Y1);
      Glyph.X0 := X0;
      Glyph.Y0 := Y0;
      Glyph.X1 := X1;
      Glyph.Y1 := Y1;
      Places[Count].W := X1 - X0;
      Places[Count].H := Y1 - Y0;
      Inc(Count);
    end;
  SetLength(FGlyphs, Count);
  { Pack the glyphs into rows }
  if FSize > 64 then
    AtlasWidth := 1024
  else
    AtlasWidth := 512;
  PenX := GlyphPadding;
  PenY := GlyphPadding;
  RowHeight := 0;
  for I := 0 to Count - 1 do
  begin
    if PenX + Places[I].W + GlyphPadding > AtlasWidth then
    begin
      PenX := GlyphPadding;
      Inc(PenY, RowHeight + GlyphPadding);
      RowHeight := 0;
    end;
    Places[I].X := PenX;
    Places[I].Y := PenY;
    Inc(PenX, Places[I].W + GlyphPadding);
    if Places[I].H > RowHeight then
      RowHeight := Places[I].H;
  end;
  AtlasHeight := PenY + RowHeight + GlyphPadding;
  { Render the glyphs as coverage values }
  SetLength(Coverage, AtlasWidth * AtlasHeight);
  FillChar(Coverage[0], Length(Coverage), 0);
  for I := 0 to Count - 1 do
  begin
    Glyph := @FGlyphs[I];
    if (Places[I].W > 0) and (Places[I].H > 0) then
      stbtt_MakeGlyphBitmap(FInfo, @Coverage[Places[I].Y * AtlasWidth + Places[I].X],
        Places[I].W, Places[I].H, AtlasWidth, FScale, FScale, Glyph.Index);
    Glyph.S0 := Places[I].X / AtlasWidth;
    Glyph.T0 := Places[I].Y / AtlasHeight;
    Glyph.S1 := (Places[I].X + Places[I].W) / AtlasWidth;
    Glyph.T1 := (Places[I].Y + Places[I].H) / AtlasHeight;
  end;
  { Expand coverage to white pixels with coverage in alpha }
  SetLength(Pixels, Length(Coverage) * 4);
  J := 0;
  for I := 0 to Length(Coverage) - 1 do
  begin
    Pixels[J] := $FF;
    Pixels[J + 1] := $FF;
    Pixels[J + 2] := $FF;
    Pixels[J + 3] := Coverage[I];
    Inc(J, 4);
  end;
  FTexture := TTexture.Create;
  FTexture.MagFilter := tfLinear;
  FTexture.MinFilter := tfLinear;
  FTexture.LoadFromData(AtlasWidth, AtlasHeight, @Pixels[0]);
  FTexture.GenerateMipmaps;
  FFallback := FindGlyph(Ord('?'));
end;

function TFont.FindGlyph(Codepoint: Integer): PFontGlyph;
var
  Lo, Hi, Mid: Integer;
begin
  { Glyphs are stored in codepoint order }
  Lo := 0;
  Hi := Length(FGlyphs) - 1;
  while Lo <= Hi do
  begin
    Mid := (Lo + Hi) div 2;
    if FGlyphs[Mid].Codepoint = Codepoint then
      Exit(@FGlyphs[Mid])
    else if FGlyphs[Mid].Codepoint < Codepoint then
      Lo := Mid + 1
    else
      Hi := Mid - 1;
  end;
  Result := FFallback;
end;

function TFont.Kerning(A, B: PFontGlyph): Float;
begin
  if (A = nil) or (B = nil) then
    Result := 0
  else
    Result := stbtt_GetGlyphKernAdvance(FInfo, A.Index, B.Index) * FScale;
end;

function TFont.Measure(const Text: string): TVec2;
var
  Glyph, Prior: PFontGlyph;
  Width: Float;
  I, C: Integer;
begin
  Result := Vec2(0, 0);
  if Text = '' then
    Exit;
  Result.Y := FLineHeight;
  Width := 0;
  Prior := nil;
  I := 1;
  while I <= Length(Text) do
  begin
    C := NextCodepoint(Text, I);
    if C = 13 then
      Continue;
    if C = 10 then
    begin
      if Width > Result.X then
        Result.X := Width;
      Width := 0;
      Prior := nil;
      Result.Y := Result.Y + FLineHeight;
      Continue;
    end;
    Glyph := FindGlyph(C);
    if Glyph = nil then
      Continue;
    Width := Width + Kerning(Prior, Glyph) + Glyph.Advance;
    Prior := Glyph;
  end;
  if Width > Result.X then
    Result.X := Width;
end;

procedure TFont.AddText(Buffer: TColorTexVertexBuffer; const Text: string;
  X, Y, Scale: Float; const Color: TVec4);
var
  Glyph, Prior: PFontGlyph;
  PenX, BaseY, L, T, R, B: Float;
  I, C: Integer;
begin
  PenX := X;
  BaseY := Y - FAscent * Scale;
  Prior := nil;
  I := 1;
  while I <= Length(Text) do
  begin
    C := NextCodepoint(Text, I);
    if C = 13 then
      Continue;
    if C = 10 then
    begin
      PenX := X;
      BaseY := BaseY - FLineHeight * Scale;
      Prior := nil;
      Continue;
    end;
    Glyph := FindGlyph(C);
    if Glyph = nil then
      Continue;
    PenX := PenX + Kerning(Prior, Glyph) * Scale;
    if Glyph.X1 > Glyph.X0 then
    begin
      { Glyph offsets point down so they are subtracted going up }
      L := PenX + Glyph.X0 * Scale;
      R := PenX + Glyph.X1 * Scale;
      T := BaseY - Glyph.Y0 * Scale;
      B := BaseY - Glyph.Y1 * Scale;
      { Counter clockwise from the bottom left }
      Buffer.Add(Vec3(L, B, 0), Vec2(Glyph.S0, Glyph.T1), Color);
      Buffer.Add(Vec3(R, B, 0), Vec2(Glyph.S1, Glyph.T1), Color);
      Buffer.Add(Vec3(R, T, 0), Vec2(Glyph.S1, Glyph.T0), Color);
      Buffer.Add(Vec3(L, T, 0), Vec2(Glyph.S0, Glyph.T0), Color);
    end;
    PenX := PenX + Glyph.Advance * Scale;
    Prior := Glyph;
  end;
end;

{ TFontCollection }

const
  SFontCollection = 'fonts';
  SDefaultFont = 'roboto';

constructor TFontCollection.Create;
begin
  inherited Create(SFontCollection);
end;

function TFontCollection.GetFont(const AName: string): TFont;
var
  Item: TContextManagedObject;
  S: TStream;
begin
  Item := GetObject(AName);
  if (Item <> nil) and (Item is TFont) then
    Exit(TFont(Item));
  S := Ctx.GetAssetStream(PathCombine('fonts', AName + '.ttf'));
  try
    Result := TFont.CreateFromStream(S);
  finally
    S.Free;
  end;
  Result.Name := AName;
end;

function TFontCollection.GetDefaultFont: TFont;
begin
  Result := GetFont(SDefaultFont);
end;

{ TFontExtension }

function TFontExtension.Fonts: TFontCollection;
begin
  Result := TFontCollection(GetCollection(SFontCollection));
  if Result = nil then
    Result := TFontCollection.Create;
end;

{ TTextBlock }

constructor TTextBlock.Create(Font: TFont = nil);
begin
  inherited Create(nil);
  FFont := Font;
  FScale := 1;
  FColor := Vec4(1, 1, 1, 1);
  FChanged := True;
end;

destructor TTextBlock.Destroy;
begin
  FreeManaged(FBuffer);
  inherited Destroy;
end;

function TTextBlock.GetFont: TFont;
begin
  if FFont = nil then
    FFont := Ctx.Fonts.DefaultFont;
  Result := FFont;
end;

procedure TTextBlock.SetFont(Value: TFont);
begin
  if Value = FFont then Exit;
  FFont := Value;
  FChanged := True;
end;

procedure TTextBlock.SetText(const Value: string);
begin
  if Value = FText then Exit;
  FText := Value;
  FChanged := True;
end;

procedure TTextBlock.SetX(Value: Float);
begin
  if Value = FX then Exit;
  FX := Value;
  FChanged := True;
end;

procedure TTextBlock.SetY(Value: Float);
begin
  if Value = FY then Exit;
  FY := Value;
  FChanged := True;
end;

procedure TTextBlock.SetScale(Value: Float);
begin
  if Value = FScale then Exit;
  FScale := Value;
  FChanged := True;
end;

procedure TTextBlock.SetColor(const Value: TVec4);
begin
  if (Value.X = FColor.X) and (Value.Y = FColor.Y) and (Value.Z = FColor.Z) and
    (Value.W = FColor.W) then
    Exit;
  FColor := Value;
  FChanged := True;
end;

function TTextBlock.Measure: TVec2;
begin
  Result := GetFont.Measure(FText);
  Result.X := Result.X * FScale;
  Result.Y := Result.Y * FScale;
end;

procedure TTextBlock.Draw(const Text: string = '');
begin
  if Text <> '' then
    SetText(Text);
  if FText = '' then
    Exit;
  if FBuffer = nil then
    FBuffer := TColorTexVertexBuffer.Create;
  if FChanged then
  begin
    FBuffer.BeginBuffer(vertQuads, Length(FText) * 4);
    GetFont.AddText(FBuffer, FText, FX, FY, FScale, FColor);
    FBuffer.EndBuffer;
    FChanged := False;
  end;
  { Text quads may face away when the modelview is mirrored }
  Ctx.PushCulling(False);
  FFont.Texture.Push;
  FBuffer.Draw;
  FFont.Texture.Pop;
  Ctx.PopCulling;
end;

end.
