(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified September 2013                             *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.interop.windows.imagebitmap.txt> }
unit Codebot.Graphics.Windows.ImageBitmap;

{$i codebot.inc}

interface

{$ifdef windows}
uses
  Windows, ActiveX, ComObj, SysUtils, Classes, Graphics,
  Codebot.System,
  Codebot.Interop.Windows.ImageCodecs,
  Codebot.Interop.Windows.GdiPlus;

{ TImageBitmap }

type
  TImageBitmapFormat = type string;

const
  BmpFormat = 'bmp';
  GifFormat = 'gif';
  JpgFormat = 'jpeg';
  PngFormat = 'png';
  TifFormat = 'tiff';

var
  DefaultFormat: TImageBitmapFormat = PngFormat;

type
  TResizeQuality = (rzNormal, rzNearest, rzBicibic);

  TImageBitmap = class(TGraphic)
  private
    FFactory: IWICImagingFactory;
    FCanvas: TCanvas;
    FBitmap: TFastBitmap;
    FFormat: TImageBitmapFormat;
    FWidth: Integer;
    FHeight: Integer;
    FStride: Integer;
    FOpacity: Byte;
    FScaleX: Single;
    FScaleY: Single;
    FPixelDepth: TPixelDepth;
    function AllowBlit(out Func: TBlendFunction; Opacity: Byte): Boolean;
    function GetBitmap: TFastBitmap;
    function GetBits: Pointer;
    function GetBounds: TRect;
    function GetScanline(Row: Integer): Pointer;
    function GetSize: Integer;
    procedure SetImageBitmapFormat(const Value: TImageBitmapFormat);
    procedure SetPixelDepth(const Value: TPixelDepth);
    function GetHandle: THandle;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure HandleNeeded(AllowChange: Boolean = True);
    procedure DestroyHandle;
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
    function GetCanvas: TCanvas; virtual;
    function GetEmpty: Boolean; override;
    function GetTransparent: Boolean; override;
    procedure SetTransparent(Value: Boolean); override;
    function GetHeight: Integer; override;
    function GetWidth: Integer; override;
    procedure SetHeight(Value: Integer); override;
    procedure SetWidth(Value: Integer); override;
  public
    constructor Create; override; overload;
    constructor Create(Bitmap: TFastBitmap); overload;
    destructor Destroy; override;
    procedure DisableCodecs;
    procedure RequestBitmap(out Bitmap: TFastBitmap; Acquire: Boolean = False);
    procedure Assign(Source: TPersistent); override;
    procedure Blit(DC: HDC; const Rect: TRect; Opacity: Byte = $FF); overload;
    procedure Blit(DC: HDC; X, Y, Index: Integer; Opacity: Byte = $FF); overload;
    procedure Blit(DC: HDC; const Rect: TRect; const Borders: TRect; Opacity: Byte = $FF); overload;
    procedure Resize(AWidth, AHeight: Integer); overload;
    procedure Resize(Percent: Single); overload;
    procedure Load(Stream: TStream; const AFormat: TImageBitmapFormat);
    procedure Save(Stream: TStream; const AFormat: TImageBitmapFormat);
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromFile(const Filename: string); override;
    procedure SaveToFile(const Filename: string); override;
    property Format: TImageBitmapFormat read FFormat write SetImageBitmapFormat;
    property Bitmap: TFastBitmap read GetBitmap;
    property Bits: Pointer read GetBits;
    property Canvas: TCanvas read GetCanvas;
    property Bounds: TRect read GetBounds;
    property Opacity: Byte read FOpacity write FOpacity;
    property Handle: THandle read GetHandle;
    property PixelDepth: TPixelDepth read FPixelDepth write SetPixelDepth;
    property ScaleX: Single read FScaleX write FScaleX;
    property ScaleY: Single read FScaleY write FScaleY;
    property Scanline[Row: Integer]: Pointer read GetScanLine;
    property Stride: Integer read FStride;
    property Size: Integer read GetSize;
  end;

function ImageBitmapFromFile(const FileName: string): TImageBitmap;
function ImageBitmapFromStream(Stream: TStream; Format: TImageBitmapFormat): TImageBitmap;
function ImageBitmapFromResourceId(ResId: Integer; Format: TImageBitmapFormat): TImageBitmap;
function ImageFrameCount(Stream: TStream): Cardinal;

{ The following are extensible bitmap operations  .. removed for now }

(*
{ Color mixing operations }
procedure ImageSaturate(Bitmap: TImageBitmap; Color: TColor);
procedure ImageScreen(Bitmap: TImageBitmap; Color: TColor);
procedure ImageColorize(Bitmap: TImageBitmap; Color: TColor);
procedure ImageTransparent(Bitmap: TImageBitmap);
{ Convert the image to greyscale }
procedure ImageGrayscale(Bitmap: TImageBitmap);
*)
{$endif}

implementation

{$ifdef windows}
uses
  Codebot.Constants;

procedure InvalidOperation(Str: PResStringRec);
begin
  raise EInvalidGraphicOperation.CreateRes(Str);
end;

{ TImageBitmapCanvas }

type
  TImageBitmapCanvas = class(TCanvas)
  private
    FBitmap: TImageBitmap;
  protected
    procedure CreateHandle; override;
  public
    constructor Create(Bitmap: TImageBitmap);
  end;

constructor TImageBitmapCanvas.Create(Bitmap: TImageBitmap);
begin
  inherited Create;
  FBitmap := Bitmap;
end;

procedure TImageBitmapCanvas.CreateHandle;
begin
  FBitmap.HandleNeeded;
end;

{ TImageBitmap }

constructor TImageBitmap.Create;
begin
  inherited Create;
  if WinCodecsInit then
    CoCreateInstance(CLSID_WICImagingFactory, nil, CLSCTX_INPROC_SERVER,
      IID_IWICImagingFactory, FFactory);
  FFormat := DefaultFormat;
  FPixelDepth := pd32;
  FScaleX := 1;
  FScaleY := 1;
  FOpacity := $FF;
end;

constructor TImageBitmap.Create(Bitmap: TFastBitmap);
begin
  Create;
  FBitmap := Bitmap;
  FWidth := FBitmap.Width;
  FHeight := FBitmap.Height;
end;

destructor TImageBitmap.Destroy;
begin
  DestroyHandle;
  inherited Destroy;
end;

procedure TImageBitmap.DisableCodecs;
begin
  FFactory := nil;
end;

procedure TImageBitmap.HandleNeeded(AllowChange: Boolean = True);
begin
  if IsFastBitmap(FBitmap) then Exit;
  if (FWidth < 1) or (FHeight < 1) then
    InvalidOperation(@SInvalidGraphicSize);
  FBitmap := CreateFastBitmap(FWidth, -FHeight, FPixelDepth);
  if FCanvas <> nil then
    FCanvas.Handle := FBitmap.DC;
  FStride := ScanlineStride(FBitmap);
  if AllowChange then
    Changed(Self);
end;

procedure TImageBitmap.DestroyHandle;
begin
  if not IsFastBitmap(FBitmap) then Exit;
  if FCanvas <> nil then
    FCanvas.Handle := 0;
  DestroyFastBitmap(FBitmap);
  Changed(Self);
end;

procedure TImageBitmap.Assign(Source: TPersistent);
var
  Image: TImageBitmap absolute Source;
  Graphic: TGraphic absolute Source;
begin
  if Source is TImageBitmap then
  begin
    Height := Image.Height;
    Width := Image.Width;
    PixelDepth := Image.PixelDepth;
    Opacity := Image.Opacity;
    Format := Image.Format;
    ScaleX := Image.ScaleX;
    ScaleY := Image.ScaleY;
    if Image.Empty then
      DestroyHandle
    else
    begin
      HandleNeeded;
      Move(Image.FBitmap.Bits^, FBitmap.Bits^, FStride * FHeight);
      // alternate Canvas.Draw(0, 0, Image);
    end;
  end
  else if Source is TGraphic then
  begin
    DestroyHandle;
    Height := Graphic.Height;
    Width := Graphic.Width;
    PixelDepth := pd32;
    Opacity := $FF;
    Format := PngFormat;
    Canvas.Draw(0, 0, Graphic);
  end
  else
    inherited Assign(Source);
end;

procedure TImageBitmap.AssignTo(Dest: TPersistent);
var
  Bitmap: TBitmap absolute Dest;
begin
  if Dest is TImageBitmap then
    Dest.Assign(Self)
  else if Dest is TBitmap then
  begin
    Bitmap.Width := Width;
    Bitmap.Height := Height;
    Bitmap.PixelFormat := pf32bit;
    Bitmap.Canvas.Brush.Color := 0;
    Bitmap.Canvas.FillRect(Rect(0, 0, Width, Height));
    Bitmap.Canvas.Draw(0, 0, Self);
  end
  else
    inherited AssignTo(Dest);
end;

procedure TImageBitmap.RequestBitmap(out Bitmap: TFastBitmap; Acquire: Boolean = False);
begin
  HandleNeeded;
  Bitmap := FBitmap;
  if Acquire then
  begin
    if FCanvas <> nil then
      FCanvas.Handle := 0;
    FillChar(FBitmap, SizeOf(FBitmap), #0);
  end;
end;

procedure TImageBitmap.Draw(ACanvas: TCanvas; const Rect: TRect);
begin
  Blit(ACanvas.Handle, Rect);
end;

function TImageBitmap.AllowBlit(out Func: TBlendFunction; Opacity: Byte): Boolean;
var
  Alpha: Byte;
begin
  Result := False;
  if Empty then Exit;
  Alpha := Opacity;
  if Alpha = $FF then
    Alpha := FOpacity;
  if Alpha = 0 then
    Exit;
  Result := True;
  FillZero(Func, SizeOf(Func));
  Func.SourceConstantAlpha := Alpha;
  if FPixelDepth = pd32 then
    Func.AlphaFormat := AC_SRC_ALPHA;
end;

procedure TImageBitmap.Blit(DC: HDC; const Rect: TRect; Opacity: Byte = $FF);
var
  Func: TBlendFunction;
  W, H: Integer;
begin
  if AllowBlit(Func, Opacity) then
  begin
    W := Width;
    H := Height;
    if FScaleX <> 1 then
      W := Round(W * FScaleX);
    if FScaleY <> 1 then
      H := Round(H * FScaleY);
    AlphaBlend(DC, Rect.Left, Rect.Top, W, H,
      FBitmap.DC, 0, 0, FWidth, FHeight, Func);
  end;
end;

procedure TImageBitmap.Blit(DC: HDC; X, Y, Index: Integer; Opacity: Byte = $FF);
var
  Func: TBlendFunction;
  W, H: Integer;
begin
  if AllowBlit(Func, Opacity) then
  begin
    if Width > Height then
      W := FHeight
    else
      W := FWidth;
    H := W;
    if FScaleX <> 1 then
      W := Round(W * FScaleX);
    if FScaleY <> 1 then
      H := Round(H * FScaleY);
    { Support for both horizontal and vertical image strips }
    if Width > Height then
      AlphaBlend(DC, X, Y, W, H,
        FBitmap.DC, Index * FHeight, 0, FHeight, FHeight, Func)
    else
      AlphaBlend(DC, X, Y, W, H,
        FBitmap.DC, 0, Index * FWidth, FWidth, FWidth, Func);
  end;
end;

procedure TImageBitmap.Blit(DC: HDC; const Rect: TRect; const Borders: TRect; Opacity: Byte = $FF);
var
  Func: TBlendFunction;
begin
  if AllowBlit(Func, Opacity) then
    with Borders do
    begin
      AlphaBlend(DC, Rect.Left, Rect.Top, Left, Top,
        FBitmap.DC, 0, 0, Left, Top, Func);
      AlphaBlend(DC, Rect.Left + Left, Rect.Top, WidthOf(Rect) - (Left + Right), Top,
        FBitmap.DC, Left, 0, Width - (Left + Right), Top, Func);
      AlphaBlend(DC, Rect.Right - Right, Rect.Top, Right, Top,
        FBitmap.DC, Width - Right, 0, Right, Top, Func);

      AlphaBlend(DC, Rect.Left, Rect.Top + Top, Left, HeightOf(Rect) - (Top + Bottom),
        FBitmap.DC, 0, Top, Left, Height - (Top + Bottom), Func);
      AlphaBlend(DC, Rect.Left + Left, Rect.Top + Top, WidthOf(Rect) - (Left + Right), HeightOf(Rect) - (Top + Bottom),
        FBitmap.DC, Left, Top, Width - (Left + Right), Height - (Top + Bottom), Func);
      AlphaBlend(DC, Rect.Right - Right, Rect.Top + Top, Right, HeightOf(Rect) - (Top + Bottom),
        FBitmap.DC, Width - Right, Top, Right, Height - (Top + Bottom), Func);

      AlphaBlend(DC, Rect.Left, Rect.Bottom - Bottom, Left, Bottom,
        FBitmap.DC, 0, Height - Bottom, Left, Bottom, Func);
      AlphaBlend(DC, Rect.Left + Left, Rect.Bottom - Bottom, WidthOf(Rect) - (Left + Right), Bottom,
        FBitmap.DC, Left, Height - Bottom, Width - (Left + Right), Bottom, Func);
      AlphaBlend(DC, Rect.Right - Right, Rect.Bottom - Bottom, Right, Bottom,
        FBitmap.DC, Width - Right, Height - Bottom, Right, Bottom, Func);
    end;
end;

procedure TImageBitmap.Resize(AWidth, AHeight: Integer);
const
  Formats: array[Boolean] of DWORD =
    (PixelFormat24bppRGB, PixelFormat32bppARGB);
var
  B: TFastBitmap;
  G: IGdiGraphics;
  S: IGdiBitmap;
begin
  if Empty then
    Exit;
  if (AWidth = Width) and (AHeight = Height) then
    Exit;
  if (AWidth < 1) or (AHeight < 1) then
    InvalidOperation(@SInvalidGraphicSize);
  HandleNeeded(False);
  B := CreateFastBitmap(AWidth, -AHeight, FPixelDepth);
  try
    G := NewGdiGraphics(B.DC);
    G.CompositingMode := CompositingModeSourceOver;
    G.InterpolationMode := InterpolationModeHighQualityBicubic;
    G.SmoothingMode := SmoothingModeHighQuality;
    G.PixelOffsetMode := PixelOffsetModeHighQuality;
    S := NewGdiBitmap(Width, Height, ScanlineStride(FBitmap),
      Formats[FBitmap.Depth = pd32], FBitmap.Bits);
    G.DrawImage(S, 0, 0, B.Width, B.Height);
  finally
    DestroyFastBitmap(FBitmap);
    FBitmap := B;
    FCanvas.Handle := FBitmap.DC;
    FWidth := B.Width;
    FHeight := B.Height;
    FStride := ScanlineStride(B);
  end;
  Changed(Self);
end;

procedure TImageBitmap.Resize(Percent: Single);
var
  W, H: Integer;
begin
  if Empty then
    Exit;
  W := Round(Width * Percent);
  H := Round(Height * Percent);
  if W < 1 then
    W := 1;
  if H < 1 then
    H := 1;
  Resize(W, H);
end;

function IsGuidEqual(const A, B: TGUID): Boolean;
begin
  Result := CompareMem(@A, @B, SizeOf(A));
end;

type
  TRGBA = packed record
    Blue, Green, Red, Alpha: Byte;
  end;
  PRGBA = ^TRGBA;

procedure Premultiply(const Bitmap: TFastBitmap);
var
  Mix: Boolean;
  W, H: Integer;
  P: PRGBA;
  R: Single;
begin
  if IsFastBitmap(Bitmap) and (Bitmap.Depth = pd32) then
  begin
    Mix := False;
    for H := 0 to Bitmap.Height - 1 do
    begin
      P := Bitmap.Bits;
      Inc(P, H * Bitmap.Width);
      for W := 0 to Bitmap.Width - 1 do
      begin
        Mix := ((P.Blue + P.Green + P.Red > 0) and (P.Alpha = 0)) or
          (P.Blue > P.Alpha) or (P.Green > P.Alpha) or (P.Red > P.Alpha);
        if Mix then Break;
        Inc(P);
      end;
      if Mix then Break;
    end;
    if Mix then
      for H := 0 to Bitmap.Height - 1 do
      begin
        P := Bitmap.Bits;
        Inc(P, H * Bitmap.Width);
        for W := 0 to Bitmap.Width - 1 do
        begin
          if P.Alpha < $FF then
          begin
            R := P.Alpha / $FF;
            P.Blue := Round(P.Blue * R);
            P.Green := Round(P.Green * R);
            P.Red := Round(P.Red * R);
          end;
          Inc(P);
        end;
      end;
  end;
end;

type
  TSharedStream = class(TStream)
  private
    FStream: TStream;
    FStart: Int64;
  protected
    function GetSize: Int64; override;
    procedure SetSize(NewSize: Longint); override;
    procedure SetSize(const NewSize: Int64); override;
  public
    constructor Create(Stream: TStream);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

{ TSharedStream }

constructor TSharedStream.Create(Stream: TStream);
begin
  inherited Create;
  FStream := Stream;
  FStart := FStream.Position;
end;

function TSharedStream.GetSize: Int64;
begin
  Result := FStream.Size - FStart;
end;

procedure TSharedStream.SetSize(NewSize: Longint);
begin
  FStream.Size := NewSize + FStart;
end;

procedure TSharedStream.SetSize(const NewSize: Int64);
begin
  FStream.Size := NewSize + FStart;
end;

function TSharedStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := FStream.Read(Buffer, Count);
end;

function TSharedStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := FStream.Write(Buffer, Count);
end;

function TSharedStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  case Origin of
    soFromBeginning:
      begin
        if Offset < 0 then
          Offset := 0;
        Result := FStream.Seek(Offset + FStart, Origin) - FStart;
      end;
    soFromCurrent:
      begin
        if FStream.Position + Offset < FStart then
          Offset := FStart - FStream.Position;
        Result := FStream.Seek(Offset, Origin) - FStart;
      end;
    soFromEnd:
      begin
        if Position + Offset < FStart then
          Offset := FStart - Position;
        Result := FStream.Seek(Offset, Origin) - FStart;
      end;
  else
    Result := 0;
  end;
end;

function TSharedStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
var
  O: Int64;
begin
  O := Offset;
  case Origin of
    soBeginning:
      begin
        if O < 0 then
          O := 0;
        Result := FStream.Seek(O + FStart, Origin) - FStart;
      end;
    soCurrent:
      begin
        if FStream.Position + O < FStart then
          O := FStart - FStream.Position;
        Result := FStream.Seek(O, Origin) - FStart;
      end;
    soEnd:
      begin
        if FStream.Position + O < FStart then
          O := FStart - FStream.Position;
        Result := FStream.Seek(O, Origin) - FStart;
      end;
  else
    Result := 0{%H-};
  end;
end;

function ImageFrameCount(Stream: TStream): Cardinal;
var
  Share: TSharedStream;
  Adapter: IStream;
  Factory: IWICImagingFactory;
  BitmapDecoder: IWICBitmapDecoder;
begin
  if WinCodecsInit then
  begin
    Share := TSharedStream.Create(Stream);
    Adapter := TStreamAdapter.Create(Share, soOwned);
    OleCheck(CoCreateInstance(CLSID_WICImagingFactory, nil, CLSCTX_INPROC_SERVER,
      IID_IWICImagingFactory, Factory));
    OleCheck(Factory.CreateDecoderFromStream(Adapter, nil,
      WICDecodeMetadataCacheOnLoad, BitmapDecoder));
    OleCheck(BitmapDecoder.GetFrameCount(Result));
  end
  else
    Result := 0;
end;

procedure TImageBitmap.Load(Stream: TStream; const AFormat: TImageBitmapFormat);
var
  Adapter: IStream;

  procedure LoadWicBitmap;
  var
    BitmapDecoder: IWICBitmapDecoder;
    BitmapFrameDecode: IWICBitmapFrameDecode;
    Converter: IWICFormatConverter;
    Source: IWICBitmapSource;
    W, H: LongWord;
    G: TGUID;
  begin
    OleCheck(FFactory.CreateDecoderFromStream(Adapter, nil,
      WICDecodeMetadataCacheOnLoad, BitmapDecoder));
    OleCheck(BitmapDecoder.GetFrame(0, BitmapFrameDecode));
    OleCheck(BitmapFrameDecode.GetPixelFormat(G));
    if IsGuidEqual(G, GUID_WICPixelFormat32bppBGRA) then
      Source := BitmapFrameDecode
    else
    begin
      OleCheck(FFactory.CreateFormatConverter(Converter));
      OleCheck(Converter.Initialize(BitmapFrameDecode, GUID_WICPixelFormat32bppBGRA,
        WICBitmapDitherTypeNone, nil, 0, WICBitmapPaletteTypeCustom));
      Source := Converter;
    end;
    OleCheck(Source.GetSize(W, H));
    FWidth := W;
    FHeight := H;
    HandleNeeded(False);
    if IsFastBitmap(FBitmap) then
      OleCheck(Source.CopyPixels(nil, FStride, FStride * FHeight, FBitmap.Bits))
  end;

  procedure LoadGdiBitmap;
  var
    GdiBitmap: IGdiBitmap;
    Rect: TGdiRectI;
    Data: TBitmapData;
  begin
    GdiBitmap := NewGdiBitmap(Adapter);
    if GdiBitmap.LastStatus <> Ok then
      Exit;
    Rect.X := 0;
    Rect.Y := 0;
    Rect.Width := GdiBitmap.Width;
    Rect.Height := GdiBitmap.Height;
    FWidth := Rect.Width;
    FHeight := Rect.Height;
    HandleNeeded(False);
    if GdiBitmap.LockBits(Rect, ImageLockModeRead, PixelFormat32bppARGB, Data) <> Ok then
      InvalidOperation(@SCouldNotLockBits);
    Move(Data.Scan0^, FBitmap.Bits^, FStride *  FHeight);
    GdiBitmap.UnlockBits(Data);
  end;

var
  Share: TSharedStream;
begin
  DestroyHandle;
  FPixelDepth := pd32;
  FWidth := 0;
  FHeight := 0;
  FStride := 0;
  Format := LowerCase(AFormat);
  Share := TSharedStream.Create(Stream);
  Adapter := TStreamAdapter.Create(Share, soOwned);
  if FFactory <> nil then
    LoadWicBitmap
  else
    LoadGdiBitmap;
  if IsFastBitmap(FBitmap) then
    Premultiply(FBitmap)
  else
  begin
    FHeight := 0;
    FWidth := 0;
    FStride := 0;
  end;
  Changed(Self);
end;

{var
  MustCopy: Boolean;
  Memory: TMemoryStream;
begin
  DestroyHandle;
  FPixelDepth := pd32;
  FWidth := 0;
  FHeight := 0;
  FStride := 0;
  Format := LowerCase(AFormat);
  MustCopy := Stream.Position > 0;
  if MustCopy then
    Memory := TMemoryStream.Create
  else
    Memory := nil;
  try
    if MustCopy then
    begin
      Memory.CopyFrom(Stream, Stream.Size - Stream.Position);
      Adapter := TStreamAdapter.Create(Memory);
    end
    else
      Adapter := TStreamAdapter.Create(Stream);
    if FFactory <> nil then
      LoadWicBitmap
    else
      LoadGdiBitmap;
  finally
    Memory.Free;
  end;
  if IsFastBitmap(FBitmap) then
    Premultiply(FBitmap)
  else
  begin
    FHeight := 0;
    FWidth := 0;
    FStride := 0;
  end;
  Changed(Self);
end;}

procedure TImageBitmap.Save(Stream: TStream; const AFormat: TImageBitmapFormat);
var
  Adapter: IStream;

  procedure SaveWicBitmap;
  var
    PixelFormat: TGUID;
    SaveStream: IWICStream;
    BitmapInstance: IWICBitmap;
    BitmapSource: IWICBitmapSource;
    BitmapEncoder: IWICBitmapEncoder;
    BitmapFrameEncode: IWICBitmapFrameEncode;
    PropertyBag: IPropertyBag2;
    Converter: IWICFormatConverter;
    Palette: IWICPalette;
    S: WideString;
    G: TGUID;
  begin
    if PixelDepth = pd32 then
      PixelFormat := GUID_WICPixelFormat32bppBGRA
    else
      PixelFormat := GUID_WICPixelFormat24bppBGR;
    OleCheck(FFactory.CreateBitmapFromMemory(FWidth, FHeight,
      PixelFormat, ScanlineStride(FBitmap),
      ScanlineStride(FBitmap) * FHeight, Bits, BitmapInstance));
    S := {%H-}Format;
    OleCheck(WICMapShortNameToGuid(PWideChar(S), G));
    BitmapSource := BitmapInstance;
    OleCheck(FFactory.CreateEncoder(G, nil, BitmapEncoder));
    OleCheck(FFactory.CreateStream(SaveStream));
    OleCheck(SaveStream.InitializeFromIStream(Adapter));
    OleCheck(BitmapEncoder.Initialize(SaveStream, WICBitmapEncoderNoCache));
    OleCheck(BitmapEncoder.CreateNewFrame(BitmapFrameEncode, PropertyBag));
    OleCheck(BitmapFrameEncode.Initialize(PropertyBag));
    OleCheck(BitmapFrameEncode.SetSize(FWidth, FHeight));
    G := PixelFormat;
    OleCheck(BitmapFrameEncode.SetPixelFormat(G));
    if not IsGuidEqual(PixelFormat, G) then
    begin
      OleCheck(FFactory.CreateFormatConverter(Converter));
      if IsGuidEqual(GUID_WICPixelFormat8bppIndexed, G) then
      begin
        OleCheck(FFactory.CreatePalette(Palette));
        OleCheck(Palette.InitializeFromBitmap(BitmapSource, $100, False));
        OleCheck(BitmapFrameEncode.SetPalette(Palette));
        OleCheck(Converter.Initialize(BitmapSource, G,
          WICBitmapDitherTypeErrorDiffusion, Palette, 0,
          WICBitmapPaletteTypeMedianCut));
      end
      else
        OleCheck(Converter.Initialize(BitmapSource, G,
          WICBitmapDitherTypeNone, nil, 0, WICBitmapPaletteTypeCustom));
      BitmapSource := Converter;
    end;
    OleCheck(BitmapFrameEncode.WriteSource(BitmapSource, nil));
    OleCheck(BitmapFrameEncode.Commit);
    OleCheck(BitmapEncoder.Commit);
  end;

  procedure SaveGdiBitmap;
  const
    Formats: array[Boolean] of DWORD =
      (PixelFormat24bppRGB, PixelFormat32bppARGB);
  var
    B: IGdiBitmap;
    G: TGUID;
  begin
    B := NewGdiBitmap(FWidth, FHeight, ScanlineStride(FBitmap),
      Formats[PixelDepth = pd32], FBitmap.Bits);
    if GetEncoderClsid('image/' + Format, G) then
      B.Save(Adapter, G)
    else
      InvalidOperation(@SInvalidGraphicFormat);
  end;

begin
  if Empty then Exit;
  HandleNeeded(False);
  Adapter := TStreamAdapter.Create(Stream);
  FFormat := LowerCase(AFormat);
  if FFormat = 'ico' then
    FFormat := 'png';
  if FFactory <> nil then
    SaveWicBitmap
  else
    SaveGdiBitmap;
end;

    { Using LockBits
    if GdiBitmap.LockBits(Rect, ImageLockModeRead, , Data) <> Ok then
      InvalidOperation(@SCouldNotLockBits);
    Move(FBitmap.Bits^, Data.Scan0^,  * FHeight);
    GdiBitmap.UnlockBits(Data);
    }

procedure TImageBitmap.LoadFromStream(Stream: TStream);
begin
  Load(Stream, Format);
end;

procedure TImageBitmap.SaveToStream(Stream: TStream);
begin
  Save(Stream, Format);
end;

function ExtractFormat(const Filename: string): string;
begin
  Result := Copy(ExtractFileExt(LowerCase(Filename)), 2, MAX_PATH);
end;

procedure TImageBitmap.LoadFromFile(const Filename: string);
begin
  Format := ExtractFormat(Filename);
  inherited LoadfromFile(Filename);
end;

procedure TImageBitmap.SaveToFile(const Filename: string);
var
  F: string;
begin
  F := Format;
  try
    Format := ExtractFormat(Filename);
    inherited SaveToFile(Filename);
  finally
    if F <> '' then
      Format := F;
  end;
end;

function TImageBitmap.GetCanvas: TCanvas;
begin
  if FCanvas = nil then
  begin
    FCanvas := TImageBitmapCanvas.Create(Self);
    FCanvas.Handle := FBitmap.DC;
  end;
  Result := FCanvas;
end;

function TImageBitmap.GetBitmap: TFastBitmap;
begin
  RequestBitmap(Result);
end;

function TImageBitmap.GetBits: Pointer;
begin
  HandleNeeded;
  Result := FBitmap.Bits;
end;

function TImageBitmap.GetBounds: TRect;
begin
  Result := Rect(0, 0, FWidth, FHeight);
end;

function TImageBitmap.GetScanline(Row: Integer): Pointer;
var
  B: PByte absolute Result;
begin
  HandleNeeded;
  if (Row < 0) or (Row > FBitmap.Height - 1) then
    InvalidOperation(@SScanLine);
  Result := FBitmap.Bits;
  Inc(B, FStride * Row);
end;

function TImageBitmap.GetTransparent: Boolean;
begin
  Result := True;
end;

procedure TImageBitmap.SetTransparent(Value: Boolean);
begin
  Value := True;
end;

function TImageBitmap.GetEmpty: Boolean;
begin
  Result := (FWidth = 0) or (FHeight = 0);
end;

procedure TImageBitmap.SetHeight(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if Value <> FHeight then
  begin
    FHeight := Value;
    DestroyHandle;
  end;
end;

procedure TImageBitmap.SetWidth(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if Value <> FWidth then
  begin
    FWidth := Value;
    DestroyHandle;
  end;
end;

function TImageBitmap.GetHandle: THandle;
begin
  Result := 0;
  if Empty then Exit;
  HandleNeeded;
  Result := FBitmap.Handle;
end;

function TImageBitmap.GetHeight: Integer;
begin
  Result := FHeight;
end;

function TImageBitmap.GetWidth: Integer;
begin
  Result := FWidth;
end;

function TImageBitmap.GetSize: Integer;
begin
  if FWidth > FHeight then
    Result := FHeight
  else
    Result := FWidth;
end;

procedure TImageBitmap.SetImageBitmapFormat(const Value: TImageBitmapFormat);
var
  Success: Boolean;
  S: WideString;
  G: TGUID;
begin
  if Value <> FFormat then
  begin
    Success := False;
    S := LowerCase(Value);
    if FFactory <> nil then
    begin
      Success := WICMapShortNameToGuid(PWideChar(S), G) = S_OK;
      if Success then
        FFormat := S;
    end;
    if not Success then
      if S = 'png' then
        FFormat := PngFormat
      else if S = 'bmp' then
        FFormat := BmpFormat
      else if S = 'jpg' then
        FFormat := JpgFormat
      else if S = 'jpeg' then
        FFormat := JpgFormat
      else if S = 'gif' then
        FFormat := GifFormat
      else if S = 'tif' then
        FFormat := TifFormat
      else if S = 'tiff' then
        FFormat := TifFormat;
      { else use the current format }
  end;
end;

procedure TImageBitmap.SetPixelDepth(const Value: TPixelDepth);
begin
  if Value <> PixelDepth then
  begin
    FPixelDepth := Value;
    DestroyHandle;
  end;
end;

function ImageBitmapFromFile(const FileName: string): TImageBitmap;
begin
  Result := TImageBitmap.Create;
  Result.LoadFromFile(FileName);
end;

function ImageBitmapFromStream(Stream: TStream; Format: TImageBitmapFormat): TImageBitmap;
begin
  Result := TImageBitmap.Create;
  Result.Load(Stream, Format);
end;

function ImageBitmapFromResourceId(ResId: Integer; Format: TImageBitmapFormat): TImageBitmap;
begin
  Result := TImageBitmap.Create;
  Result.Format := Format;
  Result.LoadFromResourceID(HInstance, ResId);
end;

{ Extensible bitmap operations ... removed for now }

(*
procedure ImageSaturate(Bitmap: TImageBitmap; Color: TColor);
var
  B: TFastBitmap;
  Pixel: PRGBA;
  C: TRGBA;
  A, L: Single;
  I: Integer;
begin
  if Bitmap.Empty then
    Exit;
  Bitmap.RequestBitmap(B);
  Pixel := B.Bits;
  C := ColorToRGBA(Color);
  for I := 0 to B.Width * B.Height - 1 do
  begin
    L := (Pixel.Red + Pixel.Green + Pixel.Blue) / (3 * $FF);
    A := Pixel.Alpha / $FF;
    if L < 0.5 then
    begin
      Pixel.Red := Round(L * 2 * C.Red * A);
      Pixel.Green := Round(L * 2 * C.Green * A);
      Pixel.Blue := Round(L * 2 * C.Green * A);
    end
    else
    begin
      Pixel.Red := Round((((1 - L) / 0.5) * C.Red + ((L - 0.5) * 2) * $FF) * A);
      Pixel.Green := Round((((1 - L) / 0.5) * C.Green + ((L - 0.5) * 2) * $FF) * A);
      Pixel.Blue := Round((((1 - L) / 0.5) * C.Blue + ((L - 0.5) * 2) * $FF) * A);
    end;
    Inc(Pixel);
  end;
end;

procedure ImageScreen(Bitmap: TImageBitmap; Color: TColor);
var
  B: TFastBitmap;
  Pixel: PRGBA;
  C: TRGBA;
  A, L: Single;
  I: Integer;
begin
  if Bitmap.Empty then
    Exit;
  Bitmap.RequestBitmap(B);
  Pixel := B.Bits;
  C := ColorToRGBA(Color);
  for I := 0 to B.Width * B.Height - 1 do
  begin
    L := (Pixel.Red + Pixel.Green + Pixel.Blue) / (3 * $FF);
    A := Pixel.Alpha / $FF;
    Pixel.Red := Round(((1 - L) * C.Red + L * $FF) * A);
    Pixel.Green := Round(((1 - L) * C.Green + L * $FF) * A);
    Pixel.Blue := Round(((1 - L) * C.Blue + L * $FF) * A);
    Inc(Pixel);
  end;
end;

procedure ImageColorize(Bitmap: TImageBitmap; Color: TColor);
var
  B: TFastBitmap;
  Pixel: PRGBA;
  C: TRGBA;
  A: Single;
  I: Integer;
begin
  if Bitmap.Empty then
    Exit;
  Bitmap.RequestBitmap(B);
  Pixel := B.Bits;
  C := ColorToRGBA(Color);
  for I := 0 to B.Width * B.Height - 1 do
  begin
    if Pixel.Alpha = 0 then
    begin
      Inc(Pixel);
      Continue;
    end;
    if Pixel.Alpha = $FF then
    begin
      Pixel.Red := C.Red;
      Pixel.Green := C.Green;
      Pixel.Blue := C.Blue;
      Inc(Pixel);
      Continue;
    end;
    A := Pixel.Alpha / $FF;
    Pixel.Red := Round(C.Red * A);
    Pixel.Green := Round(C.Green * A);
    Pixel.Blue := Round(C.Blue * A);
    Inc(Pixel);
  end;
end;

procedure ImageTransparent(Bitmap: TImageBitmap);
var
  B: TFastBitmap;
  Pixel: PRGBA;
  X, Y: Integer;
begin
  if Bitmap.Empty then
    Exit;
  Bitmap.RequestBitmap(B);
  if IsFastBitmap(B) and (B.Depth = pd32) then
  begin
    Pixel := B.Bits;
    for X := 0 to B.Width - 1 do
      for Y := 0 to B.Height - 1 do
      begin
        Pixel.Alpha := Pixel.Red;
        Inc(Pixel);
      end;
  end;
end;

procedure ImageFade(Bitmap: TImageBitmap; Direction: TDirection);
var
  B: TFastBitmap;
  Pixel: PRGBA;
  X, Y: Integer;
  A: Single;
begin
  if Bitmap.Empty then
    Exit;
  Bitmap.RequestBitmap(B);
  Pixel := B.Bits;
  A := 1;
  for Y := 0 to B.Height - 1 do
    for X := 0 to B.Width - 1 do
    begin
      if Pixel.Alpha = 0 then
      begin
        Inc(Pixel);
        Continue;
      end;
      case Direction of
        drUp:
          begin
            if Y = 0 then
            begin
              Inc(Pixel);
              Continue;
            end;
            A := 1 - Y / B.Height;
          end;
        drDown:
          begin
            if Y = B.Height - 1 then
            begin
              Inc(Pixel);
              Continue;
            end;
            A := Y / B.Height;
          end;
        drLeft:
          begin
            if X = B.Width - 1 then
            begin
              Inc(Pixel);
              Continue;
            end;
            A := X / B.Width;
          end;
        drRight:
          begin
            if X = 0 then
            begin
              Inc(Pixel);
              Continue;
            end;
            A := 1 - X / B.Width;
          end;
      else
        Exit;
      end;
      Pixel.Red := Round(Pixel.Red * A);
      Pixel.Green := Round(Pixel.Green * A);
      Pixel.Blue := Round(Pixel.Blue * A);
      Pixel.Alpha := Round(Pixel.Alpha * A);
      Inc(Pixel);
    end;
end;

procedure ImageGrayscale(Bitmap: TImageBitmap);
var
  B: TFastBitmap;
  Pixel: PRGBA;
  P: PByte absolute Pixel;
  C: Integer;
  X, Y: Integer;
begin
  if Bitmap.Empty then
    Exit;
  Bitmap.RequestBitmap(B);
  if Bitmap.PixelDepth = pd24 then
    C := 3
  else
    C := 4;
  Pixel := B.Bits;
  for Y := 0 to B.Height - 1 do
    for X := 0 to B.Width - 1 do
    begin
      Pixel.Red := Round(0.3 * Pixel.Red + 0.6 * Pixel.Green + 0.1 * Pixel.Blue);
      Pixel.Blue := Pixel.Red;
      Pixel.Green := Pixel.Red;
      Inc(P, C);
    end;
end;
*)
{$endif}

end.

