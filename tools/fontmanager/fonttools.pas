unit FontTools;

{$mode delphi}

interface

uses
  Classes, SysUtils, FreeTypeH,
  Codebot.System,
  Codebot.Text,
  Codebot.Geometry,
  Codebot.Graphics,
  Codebot.Graphics.Types;

{ TCharCode is the numeric value of a unicode character }

type
  TCharCode = LongWord;

{ TTextureData }

  TTextureData = record
    { The rectangle of the bitmap within a larger texture }
    Rect: TRectI;
    { The texture coordinates derived from rect above and the texture size }
    Coords: array[0..3] of TPointF;
  end;

{ TCharacterData }

  TCharacterData = record
    { The numeric value of the unicode character }
    CharCode: TCharCode;
    { Distance in x pixels to draw the next character }
    AdvanceX: Integer;
    { Distance in y pixels to draw the next character }
    AdvanceY: Integer;
    { Distance in x pixels from the baseline to place the bitmap }
    BearingX: Integer;
    { Distance in y pixels from the baseline to place the bitmap }
    BearingY: Integer;
    { Width in pixels of the bitmap }
    Width: Integer;
    { Height in pixels of the bitmap }
    Height: Integer;
    { An array of width X height bytes representing a grayscale bitmap }
    Bitmap: PByte;
    { Texture coordinate data useful for hardware rendering }
    TexData: TTextureData;
  end;

{ IFontStore }

  IFontStore = interface
  ['{5E90CB46-5680-4841-B99A-FD34AD5141B8}']
    { Load a font store from a file }
    procedure LoadFromFile(const FileName: string);
    { Save a font store to a file }
    procedure SaveToFile(const FileName: string);
    { Release the pixels if they are no longer needed }
    procedure ReleasePixels;
    { Test to determine if this store contains a unicode character }
    function Contains(CharCode: TCharCode): Boolean;
    { Retreive a unicode character from this store }
    function CharData(C: TCharCode; out Data: TCharacterData): Boolean;
    { The name of the font }
    property FontName: string read GetFontName;
    { The size of the font }
    property FontSize: Integer read GetFontSize;
    { The dpi of the font }
    property FontDpi: Integer read GetFontDpi;
    { The pixels of the texture }
    property Pixels: PByte read GetPixels;
    { The width of the texture }
    property Width: Integer read GetWidth;
    { The height of the texture }
    property Height: Integer read GetHeight;
    { The x distance in pixels of a space character }
    property Space: Integer read GetSpace;
    { The y distance in pixels of a newline character }
    property Newline: Integer read GetNewline;
  end;

(*type
  TCharacterData = record
    CharCode: TCharCode;
    AdvanceX: Integer;
    AdvanceY: Integer;
    BearingX: Integer;
    BearingY: Integer;
    Width: Integer;
    Height: Integer;
    Bitmap: PByte;
  end;

  TCharacterDataArray = TArrayList<TCharacterData>;

{ TFontDraw }

  TFontDraw = class
  private
    FLibrary: PFT_Library;
    FFace: PFT_Face;
    FDpi: Integer;
    FSize: Integer;
    FNewLine: Integer;
    FSpace: Integer;
    FChanged: Boolean;
    function GetNewLine: Integer;
    function GetSpace: Integer;
    procedure SetDpi(Value: Integer);
    procedure SetSize(Value: Integer);
    function Resize: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function OpenFont(const FileName: string): Boolean;
    procedure DrawText(Surface: ISurface; const S: string; X, Y: Integer);
    function MeasureText(const S: string): TPoint;
    function CharData(C: TCharCode; out Data: TCharacterData): Boolean;
    property Dpi: Integer read FDpi write SetDpi;
    property Size: Integer read FSize write SetSize;
    property NewLine: Integer read GetNewLine;
    property Space: Integer read GetSpace;
  end;


  IBitmapFont = interface
    ['{13D3B3AC-A724-4909-AB42-B5B470479510}']
    { The clear procedure frees the pixels buffer }
    procedure Clear;
    function Contains(CharCode: TCharCode): Boolean;
    property Pixels: PByte read GetGetPixels;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;

  end;

{ TFontStore }

  TFontStore = class
  private type
    TFontRange = record
      DontDraw: TFontDraw;
      FileName: string;
      RangeLow: Integer;
      RangeHigh: Integer;
    end;
    TFontRanges = TArrayList<TFontRange>;
  private
    FFontRanges: TFontRangess;
    FData: TCharacterDataArray;
    FBitmap: PByte;
    FBitmapBytes: Integer;
    FFontName: string;
    FDpi: Integer;
    FSize: Integer;
    FNewLine: Integer;
    FSpace: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure AddFont(const FileName: string; RangeLow, RangeHigh: Integer);
    procedure Build;
    function GetCharData(CharCode: TCharCode)

    procedure SaveToFile(const FileName: string);
    procedure LoadFromFile(const FileName: string);

    property FontName: string read FFontName write FFontName;
    property Dpi: Integer read FDpi write SetDpi;
    property Size: Integer read FSize write SetSize;
    property NewLine: Integer read FNewLine;
    property Space: Integer read FSpace;
  end;

  EFontError = Exception;*)

implementation

(*resourcestring
  SCouldNotOpenFreeType = 'Could not open free type library';

// The following are some examples of unicode characters

// Unicode number: U+00A2
// ¢ = 11000010 10100010

// Unicode number: U+03A3
// Σ = 11001110 10100011

// Unicode number: U+20AC
// € = 11100010 10000010 10101100

{ TFontDraw }

constructor TFontDraw.Create;
begin
  inherited Create;
  FDpi := 96;
  FSize := 12;
  if FT_Init_FreeType(FLibrary) <> 0 then
    raise EFontError.Create(SCouldNotOpenFreeType);
end;

destructor TFontDraw.Destroy;
begin
  if FFace <> nil then
    FT_Done_Face(FFace);
  FT_Done_FreeType(FLibrary);
  inherited Destroy;
end;

function TFontDraw.Open(const FileName: string): Boolean;
begin
  FChanged := True;
  Result := False;
  if FFace <> nil then
    FT_Done_Face(FFace);
  FFace := nil;
  if FileExists(FileName) then
    Result := FT_New_Face(FLibrary, PChar(FileName), 0, FFace) = 0;
end;

procedure TFontDraw.DrawText(Surface: ISurface; const S: string; X, Y: Integer);

  function CopyBitmap(W, H: Integer; P: PByte): IBitmap;
  var
    Pixel: PPixel;
    X, Y: Integer;
  begin
    Result := NewBitmap(W, H);
    Pixel := Result.Pixels;
    for X := 1 to W do
      for Y := 1 to H do
        begin
          Pixel.Red := 0;
          Pixel.Green := 0;
          Pixel.Blue := 0;
          Pixel.Alpha := P^;
          Inc(Pixel);
          Inc(P);
        end;
  end;

var
  P: PChar;
  C: TCharCode;
  D: TCharacterData;
  B: IBitmap;
  R: TRectI;
  X1, Y1: Integer;
begin
  if not Resize then
    Exit;
  X1 := X;
  Y1 := Y;
  P := PChar(S);
  repeat
    C := UnicodeToChar(P);
    if CharData(C, D) then
    begin
      B := CopyBitmap(D.Width, D.Height, D.Bitmap);
      R := B.ClientRect;
      R.Offset(X1 + D.BearingX, Y1 - D.BearingY);
      B.Surface.CopyTo(B.ClientRect, Surface, R);
      X1 := X1 + D.AdvanceX;
    end
    else if C = $20 then
      X1 := X1 + FSpace
    else if C = $0A then
    begin
      X1 := X;
      Y1 := Y1 + FNewline;
    end;
  until C = 0;
end;

function TFontDraw.MeasureText(const S: string): TPoint;
var
  P: PChar;
  C: TCharCode;
  X: Integer;
begin
  Result.X := 0;
  Result.Y := 0;
  if Resize then
    Exit;
  P := PChar(S);
  X := 0;
  Result.Y := NewLine;
  repeat
    C := UnicodeToChar(P);
    if C > $20 then
    begin
      FT_Load_Char(FFace, C, FT_LOAD_NO_BITMAP);
      X := X + FFace.glyph.advance.X shr 6;
      if X > Result.X then
        Result.X := X;
    end
    else if C = $20 then
    begin
      X := X + Space;
      if X > Result.X then
        Result.X := X;
    end
    else if C = $0A then
    begin
      X := 0;
      Result.Y := Result.Y + NewLine;
    end;
  until C = 0;
end;

function TFontDraw.CharData(C: TCharCode; out Data: TCharacterData): Boolean;
begin
  if C <= $20 then
    Exit(False);
  Result := Resize;
  if not Result then
    Exit;
  Result := FT_Load_Char(FFace, C, FT_LOAD_RENDER) = 0;
  if not Result then
    Exit;
  Data.BearingX := FFace.glyph.bitmap_left;
  Data.BearingY := FFace.glyph.bitmap_top;
  Data.AdvanceX := FFace.glyph.advance.X shr 6;
  Data.AdvanceY := FFace.glyph.advance.Y shr 6;
  Data.Width := FFace.glyph.bitmap.width;
  Data.Height := FFace.glyph.bitmap.rows;
  Data.Bitmap := FFace.glyph.bitmap.buffer;
end;

function TFontDraw.GetNewLine: Integer;
begin
  if Resize then
    Result := FNewLine
  else
    Result := 0;
end;

function TFontDraw.GetSpace: Integer;
begin
  if Resize then
    Result := FSpace
  else
    Result := 0;
end;

function TFontDraw.Resize: Boolean;
begin
  Result := FFace <> nil;
  if not Result then
    Exit;
  { From https://www.freetype.org/freetype2/docs/tutorial/example1.c
    use 50pt at 100dpi
    error = FT_Set_Char_Size(face, 50 * 64, 0, 100, 0 ); }
  if FChanged then
  begin
    Result := FT_Set_Char_Size(FFace, FSize shl 6, 0, FDpi, 0) = 0;
    FT_Load_Char(FFace, $20, FT_LOAD_NO_BITMAP);
    FNewLine := FFace.size.metrics.height shr 6;
    FSpace := FFace.glyph.advance.X shr 6;
  end;
  if Result then
    FChanged := False;
end;

procedure TFontDraw.SetDpi(Value: Integer);
begin
  if Value < 10 then
    Value := 10
  else if Value > 1000 then
    Value := 1000;
  if Value <> FDpi then
  begin
    FChanged := True;
    FDpi := Value;
  end;
end;

procedure TFontDraw.SetSize(Value: Integer);
begin
  if Value < 4 then
    Value := 4
  else if Value > 400 then
    Value := 400;
  if Value <> FSize then
  begin
    FChanged := True;
    FSize := Value;
  end;
end;*)

end.

