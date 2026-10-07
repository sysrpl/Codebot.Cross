(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified September 2013                             *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.graphics.windows.imagebitmap.txt> }
unit Codebot.Graphics.Windows.ImageBitmap;

{$i ../codebot/codebot.inc}

interface

{$ifdef windows}
uses
  Windows, ActiveX, ComObj, SysUtils, Classes, Graphics,
  Codebot.System,
  Codebot.Interop.Windows.ImageCodecs;

const
  AC_SRC_OVER = $00;
  AC_SRC_ALPHA = $01;

type
  TBlendFunction = packed record
    BlendOp: Byte;
    BlendFlags: Byte;
    SourceConstantAlpha: Byte;
    AlphaFormat: Byte;
  end;

function AlphaBlend(dest: HDC; xoriginDest, yoriginDest, wDest, hDest: Integer;
  src: HDC; xoriginSrc, yoriginSrc, wSrc, hSrc: Integer;
  func: TBlendFunction): BOOL; stdcall;

function HeightOf(const Rect: TRect): Integer; inline;
function WidthOf(const Rect: TRect): Integer; inline;

{ TFastBitmap is a GDI device independent bitmap selected into its own
  device context }

type
  TPixelDepth = (pd24, pd32);

  TFastBitmap = record
    DC: HDC;
    Handle: HBITMAP;
    OldBitmap: HBITMAP;
    Bits: Pointer;
    Width: Integer;
    Height: Integer;
    Depth: TPixelDepth;
    procedure Create(Width, Height: Integer; Depth: TPixelDepth = pd24); overload;
    procedure Create(const Rect: TRect; Depth: TPixelDepth = pd24); overload;
    procedure Destroy;
    procedure Draw(DC: HDC; X, Y: Integer; Opacity: Byte = $FF); overload;
    procedure Draw(DC: HDC; const Rect: TRect; Opacity: Byte = $FF); overload;
    procedure Clear;
    function ClientRect: TRect;
    function IsEmpty: Boolean;
  end;
  PFastBitmap = ^TFastBitmap;

{ TFastBitmap routines }

{ Create a fast bitmap, use a negative height for a top down bitmap }
function CreateFastBitmap(Width, Height: Integer; Depth: TPixelDepth = pd24): TFastBitmap; overload;
function CreateFastBitmap(const Rect: TRect; Depth: TPixelDepth = pd24): TFastBitmap; overload;
procedure DestroyFastBitmap(var Bitmap: TFastBitmap);
procedure ClearFastBitmap(const Bitmap: TFastBitmap);
function IsEmptyFastBitmap(const Bitmap: TFastBitmap): Boolean;
function IsFastBitmap(const Bitmap: TFastBitmap): Boolean;
{ Return a resized copy of a bitmap using the Windows imaging component. Quality
  0 is nearest neighbor, 1 is linear, and 2 is bicubic. The result is empty if
  the bitmap could not be resized. }
function BitmapResize(Bitmap: TFastBitmap; Width, Height: Integer; Quality: Integer = 2): TFastBitmap;
{ The number of bytes in a row of pixels }
function ScanlineStride(const Bitmap: TFastBitmap): Integer;

{ Drawing routines }

procedure AlphaDraw(DC: HDC; X, Y: Integer; const Bitmap: TFastBitmap; Opacity: Byte = $FF); overload;
procedure AlphaDraw(DC: HDC; const Rect: TRect; const Bitmap: TFastBitmap; Opacity: Byte = $FF); overload;

{ TImageBitmapFormat is the name of an image file format }

type
  TImageBitmapFormat = type string;

{ Image format names }

const
  BmpFormat = 'bmp';
  GifFormat = 'gif';
  JpgFormat = 'jpeg';
  PngFormat = 'png';
  TifFormat = 'tiff';

{ The format used when saving if none is given }
var
  DefaultFormat: TImageBitmapFormat = PngFormat;

type
{ TImageBitmap is a 32 bit alpha blended graphic which loads and saves
  images using the Windows imaging component }

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

{ Create an image bitmap from a file }
function ImageBitmapFromFile(const FileName: string): TImageBitmap;
{ Create an image bitmap from a stream in a format }
function ImageBitmapFromStream(Stream: TStream; Format: TImageBitmapFormat): TImageBitmap;
{ Create an image bitmap from a resource in a format }
function ImageBitmapFromResourceId(ResId: Integer; Format: TImageBitmapFormat): TImageBitmap;
{ Return the number of frames in an image stream }
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

function AlphaBlend; external 'msimg32.dll';

{ Create a Windows imaging component factory. COM is initialized for the main
  thread by ComObj, but other threads must initialize it themselves. }

function CreateImagingFactory(out Factory: IWICImagingFactory): Boolean;
const
  CO_E_NOTINITIALIZED = HRESULT($800401F0);
var
  H: HRESULT;
begin
  Factory := nil;
  if not WinCodecsInit then
    Exit(False);
  H := CoCreateInstance(CLSID_WICImagingFactory, nil, CLSCTX_INPROC_SERVER,
    IID_IWICImagingFactory, Factory);
  if H = CO_E_NOTINITIALIZED then
  begin
    CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
    H := CoCreateInstance(CLSID_WICImagingFactory, nil, CLSCTX_INPROC_SERVER,
      IID_IWICImagingFactory, Factory);
  end;
  Result := (H = S_OK) and (Factory <> nil);
  if not Result then
    Factory := nil;
end;

function HeightOf(const Rect: TRect): Integer;
begin
  Result := Rect.Bottom - Rect.Top;
end;

function WidthOf(const Rect: TRect): Integer;
begin
  Result := Rect.Right - Rect.Left;
end;

{ TFastBitmap }

const
  Depths: array[TPixelDepth] of Integer = (24, 32);

procedure TFastBitmap.Create(Width, Height: Integer; Depth: TPixelDepth = pd24);
begin
  Self := CreateFastBitmap(Width, Height, Depth);
end;

procedure TFastBitmap.Create(const Rect: TRect; Depth: TPixelDepth = pd24);
begin
  Self := CreateFastBitmap(WidthOf(Rect), HeightOf(Rect), Depth);
end;

procedure TFastBitmap.Destroy;
begin
  DestroyFastBitmap(Self);
end;

procedure TFastBitmap.Draw(DC: HDC; X, Y: Integer; Opacity: Byte = $FF);
begin
  AlphaDraw(DC, X, Y, Self, Opacity);
end;

procedure TFastBitmap.Draw(DC: HDC; const Rect: TRect; Opacity: Byte = $FF);
begin
  AlphaDraw(DC, Rect, Self, Opacity);
end;

procedure TFastBitmap.Clear;
begin
  ClearFastBitmap(Self);
end;

function TFastBitmap.ClientRect: TRect;
begin
  Result.Left := 0;
  Result.Top := 0;
  Result.Right := Width;
  Result.Bottom := Height;
end;

function TFastBitmap.IsEmpty: Boolean;
begin
  Result := IsEmptyFastBitmap(Self);
end;

{ TFastBitmap routines }

function CreateFastBitmap(Width, Height: Integer; Depth: TPixelDepth = pd24): TFastBitmap;
var
  BitmapInfo: TBitmapInfo;
begin
  FillChar(Result, SizeOf(Result), #0);
  FillChar(BitmapInfo, SizeOf(BitmapInfo), #0);
  if (Width < 1) or (Height = 0) then
    Exit;
  Result.DC := CreateCompatibleDC(0);
  with BitmapInfo.bmiHeader do
  begin
    biSize := SizeOf(BitmapInfo.bmiHeader);
    biWidth := Width;
    biHeight := Height;
    biPlanes := 1;
    biBitCount := Depths[Depth];
    biCompression := BI_RGB;
  end;
  with Result do
    Handle := CreateDIBSection(DC, BitmapInfo, DIB_RGB_COLORS, Bits, 0, 0);
  Result.Width := Width;
  Result.Height := Height;
  if Result.Height < 0 then
    Result.Height := -Result.Height;
  Result.Depth := Depth;
  with Result do
    OldBitmap := SelectObject(DC, Handle);
end;

function CreateFastBitmap(const Rect: TRect; Depth: TPixelDepth = pd24): TFastBitmap;
begin
  Result := CreateFastBitmap(WidthOf(Rect), HeightOf(Rect), Depth);
end;

procedure DestroyFastBitmap(var Bitmap: TFastBitmap);
begin
  if Bitmap.DC <> 0 then
  begin
    SelectObject(Bitmap.DC, Bitmap.OldBitmap);
    DeleteObject(Bitmap.Handle);
    DeleteDC(Bitmap.DC);
    FillChar(Bitmap, SizeOf(Bitmap), #0);
  end;
end;

procedure ClearFastBitmap(const Bitmap: TFastBitmap);
begin
  if Bitmap.DC <> 0 then
    FillChar(Bitmap.Bits^, ScanlineStride(Bitmap) * Bitmap.Height, #0);
end;

function IsFastBitmap(const Bitmap: TFastBitmap): Boolean;
begin
  Result := Bitmap.DC <> 0;
end;

function IsEmptyFastBitmap(const Bitmap: TFastBitmap): Boolean;
begin
  Result := Bitmap.DC = 0;
end;

function BitmapResize(Bitmap: TFastBitmap; Width, Height: Integer; Quality: Integer = 2): TFastBitmap;
var
  Factory: IWICImagingFactory;
  PixelFormat: TGUID;
  Mode: WICBitmapInterpolationMode;
  Source: IWICBitmap;
  Scaler: IWICBitmapScaler;
  B: TFastBitmap;
begin
  FillChar(Result, SizeOf(Result), #0);
  if not IsFastBitmap(Bitmap) then
    Exit;
  if (Width < 1) or (Height < 1) then
    Exit;
  if not CreateImagingFactory(Factory) then
    Exit;
  { 32 bit bitmaps are stored with premultiplied alpha }
  if Bitmap.Depth = pd32 then
    PixelFormat := GUID_WICPixelFormat32bppPBGRA
  else
    PixelFormat := GUID_WICPixelFormat24bppBGR;
  case Quality of
    0: Mode := WICBitmapInterpolationModeNearestNeighbor;
    1: Mode := WICBitmapInterpolationModeLinear;
  else
    Mode := WICBitmapInterpolationModeCubic;
  end;
  if Factory.CreateBitmapFromMemory(Bitmap.Width, Bitmap.Height, PixelFormat,
    ScanlineStride(Bitmap), ScanlineStride(Bitmap) * Bitmap.Height,
    Bitmap.Bits, Source) <> S_OK then
    Exit;
  if Factory.CreateBitmapScaler(Scaler) <> S_OK then
    Exit;
  if Scaler.Initialize(Source, Width, Height, Mode) <> S_OK then
    Exit;
  B := CreateFastBitmap(Width, -Height, Bitmap.Depth);
  if Scaler.CopyPixels(nil, ScanlineStride(B), ScanlineStride(B) * Height,
    B.Bits) <> S_OK then
  begin
    DestroyFastBitmap(B);
    Exit;
  end;
  Result := B;
end;

function ScanlineStride(const Bitmap: TFastBitmap): Integer;
const
  Bit24Size = 3;
  Bit32Size = 4;
begin
  if Bitmap.Depth = pd24 then
    Result := Bitmap.Width * Bit24Size
  else
    Result := Bitmap.Width * Bit32Size;
  if Result mod SizeOf(DWORD) > 0 then
    Inc(Result, SizeOf(DWORD) - Result mod SizeOf(DWORD));
end;

{ Drawing routines }

procedure AlphaDraw(DC: HDC; X, Y: Integer; const Bitmap: TFastBitmap; Opacity: Byte = $FF);
var
  Func: TBlendFunction;
begin
  if IsFastBitmap(Bitmap) then
    if (Bitmap.Depth = pd32) and (Opacity > 0) then
    begin
      Func.BlendOp := AC_SRC_OVER;
      Func.BlendFlags := 0;
      Func.SourceConstantAlpha := Opacity;
      Func.AlphaFormat := AC_SRC_ALPHA;
      AlphaBlend(DC, X, Y, Bitmap.Width, Bitmap.Height,
        Bitmap.DC, 0, 0, Bitmap.Width, Bitmap.Height, Func);
    end
    else if Bitmap.Depth = pd24 then
      BitBlt(DC, X, Y, Bitmap.Width, Bitmap.Height, Bitmap.DC, 0, 0, SRCCOPY);
end;

procedure AlphaDraw(DC: HDC; const Rect: TRect; const Bitmap: TFastBitmap; Opacity: Byte = $FF);
var
  Func: TBlendFunction;
begin
  if IsFastBitmap(Bitmap) then
    if (Bitmap.Depth = pd32) and (Opacity > 0) then
    begin
      Func.BlendOp := AC_SRC_OVER;
      Func.BlendFlags := 0;
      Func.SourceConstantAlpha := Opacity;
      Func.AlphaFormat := AC_SRC_ALPHA;
      AlphaBlend(DC, Rect.Left, Rect.Top, WidthOf(Rect), HeightOf(Rect),
        Bitmap.DC, 0, 0, Bitmap.Width, Bitmap.Height, Func);
    end
    else if Bitmap.Depth = pd24 then
      StretchBlt(DC, Rect.Left, Rect.Top, WidthOf(Rect), HeightOf(Rect),
        Bitmap.DC, 0, 0, Bitmap.Width, Bitmap.Height, SRCCOPY);
end;

{ Set the alpha of every pixel to opaque if no pixel has an alpha value }

procedure MakeOpaque(const Bitmap: TFastBitmap);
const
  AlphaOffset = 3;
var
  P: PByte;
  I, Count: Integer;
begin
  if not IsFastBitmap(Bitmap) or (Bitmap.Depth <> pd32) then
    Exit;
  Count := Bitmap.Width * Bitmap.Height;
  P := Bitmap.Bits;
  Inc(P, AlphaOffset);
  for I := 0 to Count - 1 do
  begin
    if P^ <> 0 then
      Exit;
    Inc(P, 4);
  end;
  P := Bitmap.Bits;
  Inc(P, AlphaOffset);
  for I := 0 to Count - 1 do
  begin
    P^ := $FF;
    Inc(P, 4);
  end;
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
  CreateImagingFactory(FFactory);
  FFormat := DefaultFormat;
  FPixelDepth := pd32;
  FScaleX := 1;
  FScaleY := 1;
  FOpacity := $FF;
end;

constructor TImageBitmap.Create(Bitmap: TFastBitmap);
begin
  Create;
  if not IsFastBitmap(Bitmap) then
    Exit;
  FBitmap := Bitmap;
  FWidth := FBitmap.Width;
  FHeight := FBitmap.Height;
  FPixelDepth := FBitmap.Depth;
  FStride := ScanlineStride(FBitmap);
end;

destructor TImageBitmap.Destroy;
begin
  DestroyHandle;
  FCanvas.Free;
  inherited Destroy;
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
      { A source without a handle has no pixels yet, and a new handle is
        already cleared }
      if IsFastBitmap(Image.FBitmap) then
        Move(Image.FBitmap.Bits^, FBitmap.Bits^, FStride * FHeight);
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
    if Empty then
      Exit;
    Canvas.Draw(0, 0, Graphic);
    { GDI drawing leaves the alpha channel at zero, which would make the image
      invisible. Unless the graphic wrote alpha values, make it opaque. }
    MakeOpaque(FBitmap);
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
var
  Func: TBlendFunction;
begin
  { Stretch the image to fill the rect, as TCanvas.StretchDraw expects }
  if AllowBlit(Func, $FF) then
    AlphaBlend(ACanvas.Handle, Rect.Left, Rect.Top, WidthOf(Rect), HeightOf(Rect),
      FBitmap.DC, 0, 0, FWidth, FHeight, Func);
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
var
  B: TFastBitmap;
  PixelFormat: TGUID;
  Source: IWICBitmap;
  Scaler: IWICBitmapScaler;
begin
  if Empty then
    Exit;
  if (AWidth = Width) and (AHeight = Height) then
    Exit;
  if (AWidth < 1) or (AHeight < 1) then
    InvalidOperation(@SInvalidGraphicSize);
  if FFactory = nil then
    InvalidOperation(@SImagingUnavailable);
  HandleNeeded(False);
  { Loaded 32 bit images are stored with premultiplied alpha }
  if FBitmap.Depth = pd32 then
    PixelFormat := GUID_WICPixelFormat32bppPBGRA
  else
    PixelFormat := GUID_WICPixelFormat24bppBGR;
  B := CreateFastBitmap(AWidth, -AHeight, FBitmap.Depth);
  try
    { Scale the image with bicubic interpolation using the imaging component }
    OleCheck(FFactory.CreateBitmapFromMemory(FWidth, FHeight, PixelFormat,
      ScanlineStride(FBitmap), ScanlineStride(FBitmap) * FHeight,
      FBitmap.Bits, Source));
    OleCheck(FFactory.CreateBitmapScaler(Scaler));
    OleCheck(Scaler.Initialize(Source, AWidth, AHeight,
      WICBitmapInterpolationModeCubic));
    OleCheck(Scaler.CopyPixels(nil, ScanlineStride(B),
      ScanlineStride(B) * AHeight, B.Bits));
  except
    { Keep the original image if resizing failed }
    DestroyFastBitmap(B);
    raise;
  end;
  DestroyFastBitmap(FBitmap);
  FBitmap := B;
  if FCanvas <> nil then
    FCanvas.Handle := FBitmap.DC;
  FWidth := B.Width;
  FHeight := B.Height;
  FStride := ScanlineStride(B);
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
        if FStream.Size + Offset < FStart then
          Offset := FStart - FStream.Size;
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
        if FStream.Size + O < FStart then
          O := FStart - FStream.Size;
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
  if CreateImagingFactory(Factory) then
  begin
    Share := TSharedStream.Create(Stream);
    Adapter := TStreamAdapter.Create(Share, soOwned);
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
    { Images are stored with premultiplied alpha, which the converter computes }
    if IsGuidEqual(G, GUID_WICPixelFormat32bppPBGRA) then
      Source := BitmapFrameDecode
    else
    begin
      OleCheck(FFactory.CreateFormatConverter(Converter));
      OleCheck(Converter.Initialize(BitmapFrameDecode, GUID_WICPixelFormat32bppPBGRA,
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

var
  Share: TSharedStream;
begin
  if FFactory = nil then
    InvalidOperation(@SImagingUnavailable);
  DestroyHandle;
  FPixelDepth := pd32;
  FWidth := 0;
  FHeight := 0;
  FStride := 0;
  Format := LowerCase(AFormat);
  Share := TSharedStream.Create(Stream);
  Adapter := TStreamAdapter.Create(Share, soOwned);
  LoadWicBitmap;
  if not IsFastBitmap(FBitmap) then
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
    LoadWicBitmap;
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
    { 32 bit pixels are stored with premultiplied alpha, and the converter
      below changes them to the format the encoder wants }
    if PixelDepth = pd32 then
      PixelFormat := GUID_WICPixelFormat32bppPBGRA
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

begin
  if Empty then Exit;
  if FFactory = nil then
    InvalidOperation(@SImagingUnavailable);
  HandleNeeded(False);
  Adapter := TStreamAdapter.Create(Stream);
  FFormat := LowerCase(AFormat);
  if FFormat = 'ico' then
    FFormat := 'png';
  SaveWicBitmap;
end;

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
  { Image bitmaps are always transparent }
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
  try
    Result.LoadFromFile(FileName);
  except
    Result.Free;
    raise;
  end;
end;

function ImageBitmapFromStream(Stream: TStream; Format: TImageBitmapFormat): TImageBitmap;
begin
  Result := TImageBitmap.Create;
  try
    Result.Load(Stream, Format);
  except
    Result.Free;
    raise;
  end;
end;

function ImageBitmapFromResourceId(ResId: Integer; Format: TImageBitmapFormat): TImageBitmap;
begin
  Result := TImageBitmap.Create;
  try
    Result.Format := Format;
    Result.LoadFromResourceID(HInstance, ResId);
  except
    Result.Free;
    raise;
  end;
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

