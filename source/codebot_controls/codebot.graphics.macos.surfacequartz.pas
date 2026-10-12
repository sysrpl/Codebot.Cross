(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.graphics.macos.surfacequartz.txt> }
unit Codebot.Graphics.MacOS.SurfaceQuartz;

{$i ../codebot/codebot.inc}
{$ifdef darwin}
  {$modeswitch objectivec1}
  {$linkframework CoreGraphics}
  {$linkframework CoreText}
  {$linkframework ImageIO}
{$endif}

interface

{$ifdef darwin}
uses
  SysUtils, Classes, Graphics, Controls,
  Codebot.System,
  Codebot.Graphics.Types;

{ New object routines
  These create the quartz implementations of the drawing interfaces declared in
  Codebot.Graphics.Types. Use the New functions in Codebot.Graphics instead,
  which select the backend for the current platform.

  Drawing uses Core Graphics, text uses Core Text, and images are loaded and
  saved using ImageIO, all of which are part of macOS. }

function NewMatrixQuartz: IMatrix;
function NewPenQuartz(Brush: IBrush; Width: Float = 1): IPen; overload;
function NewPenQuartz(Color: TColorB; Width: Float = 1): IPen; overload;
function NewSolidBrushQuartz(Color: TColorB): ISolidBrush;
function NewBitmapBrushQuartz(Bitmap: IBitmap): IBitmapBrush;
function NewLinearGradientBrushQuartz(X1, Y1, X2, Y2: Float): ILinearGradientBrush; overload;
function NewLinearGradientBrushQuartz(const A, B: TPointF): ILinearGradientBrush; overload;
function NewRadialGradientBrushQuartz(const Rect: TRectF): IRadialGradientBrush;
function NewFontQuartz(const FontName: string; FontSize: Integer): IFont; overload;
function NewFontQuartz(Font: TFont = nil): IFont; overload;
function NewSurfaceQuartz(Canvas: TCanvas): ISurface; overload;
function NewSurfaceQuartz(Control: TWinControl): ISurface; overload;
function NewBitmapQuartz(Width, Height: Integer): IBitmap;
function NewSplashQuartz: ISplash;
{ Capture the screen into a bitmap. macOS only allows this after the user has
  granted the program screen recording permission. }
function NewScreenCaptureQuartz: IBitmap;
{$endif}

implementation

{$ifdef darwin}
uses
  DynLibs, MacOSAll, CocoaAll, CocoaGDIObjects;

const
  PixelDelta = 0.5;
  { Font sizes are in points at 96 pixels per inch, as they are in the LCL and
    the other backends }
  FontScale = 96 / 72;
  { Bitmap pixels are premultiplied and ordered blue, green, red, alpha in
    memory, which matches TColorB }
  BitmapInfo = kCGImageAlphaPremultipliedFirst or kCGBitmapByteOrder32Little;
  { The most gradient periods drawn when a gradient repeats or reflects }
  MaxPeriods = 512;

{ Math is not used by this unit because its Float type would hide the one in
  Codebot.System }

function CeilInt(const Value: Double): Integer;
begin
  Result := Trunc(Value);
  if Result < Value then
    Inc(Result);
end;

function FloorInt(const Value: Double): Integer;
begin
  Result := Trunc(Value);
  if Result > Value then
    Dec(Result);
end;

function ArcTangent(Y, X: Double): Double;
begin
  if X > 0 then
    Result := ArcTan(Y / X)
  else if X < 0 then
    if Y >= 0 then
      Result := ArcTan(Y / X) + Pi
    else
      Result := ArcTan(Y / X) - Pi
  else if Y > 0 then
    Result := Pi / 2
  else if Y < 0 then
    Result := -Pi / 2
  else
    Result := 0;
end;

function MatrixIdentity: CGAffineTransform;
begin
  Result := CGAffineTransformMake(1, 0, 0, 1, 0, 0);
end;

function ToRect(const R: TRectF): CGRect;
begin
  Result := CGRectMake(R.X, R.Y, R.Width, R.Height);
end;

var
  InternalColorSpace: CGColorSpaceRef;

function ColorSpace: CGColorSpaceRef;
begin
  if InternalColorSpace = nil then
    InternalColorSpace := CGColorSpaceCreateDeviceRGB;
  Result := InternalColorSpace;
end;

{ Create a context which draws on pixels using the default coordinates of
  Core Graphics, where the origin is at the bottom left }

function RawContext(Pixels: Pointer; Width, Height: Integer): CGContextRef;
begin
  Result := CGBitmapContextCreate(Pixels, Width, Height, 8, Width * SizeOf(TColorB),
    ColorSpace, BitmapInfo);
end;

function StrToCF(const S: string): CFStringRef;
const
  EncodingLatin1 = $0201;
begin
  Result := CFStringCreateWithBytes(nil, PChar(S), Length(S), kCFStringEncodingUTF8, False);
  { Text which is not valid utf8 is taken to be latin text }
  if Result = nil then
    Result := CFStringCreateWithBytes(nil, PChar(S), Length(S), EncodingLatin1, False);
end;

{ IMatrixQuartz }

type
  IMatrixQuartz = interface(IMatrix)
  ['{5B0B6D0F-6E0C-4B0E-9C41-6B3E5A3C7F21}']
    function GetMatrix: CGAffineTransform;
    procedure SetMatrix(const Value: CGAffineTransform);
    function GetChanged: Boolean;
    procedure SetChanged(Value: Boolean);
    property Matrix: CGAffineTransform read GetMatrix write SetMatrix;
    property Changed: Boolean read GetChanged write SetChanged;
  end;

{ TMatrixQuartz }

  TMatrixQuartz = class(TInterfacedObject, IMatrix, IMatrixQuartz)
  private
    FMatrix: CGAffineTransform;
    FChanged: Boolean;
  public
    constructor Create; overload;
    constructor Create(const M: CGAffineTransform); overload;
    function Clone: IMatrix;
    procedure Identity;
    procedure Multiply(M: IMatrix);
    procedure Translate(X, Y: Float);
    procedure Scale(X, Y: Float);
    procedure Rotate(Radians: Float);
    function Transform(Point: TPointF): TPointF;
    function GetMatrix: CGAffineTransform;
    procedure SetMatrix(const Value: CGAffineTransform);
    function GetChanged: Boolean;
    procedure SetChanged(Value: Boolean);
  end;

{ TPenQuartz }

  TPenQuartz = class(TInterfacedObject, IPen)
  private
    FBrush: IBrush;
    FColor: TColorB;
    FWidth: Float;
    FLinePattern: TLinePattern;
    FLinePatternOffset: Float;
    FLineCap: TLineCap;
    FLineJoin: TLineJoin;
    FMiterLimit: Float;
  public
    constructor Create(B: IBrush; W: Float); overload;
    constructor Create(C: TColorB; W: Float); overload;
    function GetBrush: IBrush;
    procedure SetBrush(Value: IBrush);
    function GetColor: TColorB;
    procedure SetColor(Value: TColorB);
    function GetWidth: Float;
    procedure SetWidth(Value: Float);
    function GetLinePattern: TLinePattern;
    procedure SetLinePattern(Value: TLinePattern);
    function GetLinePatternOffset: Float;
    procedure SetLinePatternOffset(Value: Float);
    function GetLineCap: TLineCap;
    procedure SetLineCap(Value: TLineCap);
    function GetLineJoin: TLineJoin;
    procedure SetLineJoin(Value: TLineJoin);
    function GetMiterLimit: Float;
    procedure SetMiterLimit(Value: Float);
  end;

{ TBrushQuartz }

  TBrushQuartz = class(TInterfacedObject, IBrush)
  private
    FOpacity: Byte;
  public
    constructor Create;
    function GetMatrix: IMatrix; virtual;
    procedure SetMatrix(Value: IMatrix); virtual;
    function GetOpacity: Byte;
    procedure SetOpacity(Value: Byte);
  end;

{ TSolidBrushQuartz }

  TSolidBrushQuartz = class(TBrushQuartz, ISolidBrush)
  private
    FColor: TColorB;
  public
    constructor Create(C: TColorB);
    function GetColor: TColorB;
    procedure SetColor(Value: TColorB);
  end;

{ TPatternBrushQuartz is a brush which fills the clipping area of a context
  with a pattern. The path being filled is made the clipping area before
  Draw is called, and the graphics state is restored afterwards. }

  TPatternBrushQuartz = class(TBrushQuartz)
  private
    FMatrix: IMatrix;
  protected
    function Transform: CGAffineTransform;
    procedure Draw(Context: CGContextRef); virtual; abstract;
  public
    function GetMatrix: IMatrix; override;
    procedure SetMatrix(Value: IMatrix); override;
  end;

{ TBitmapBrushQuartz }

  TBitmapBrushQuartz = class(TPatternBrushQuartz, IBitmapBrush)
  private
    FBitmap: IBitmap;
  protected
    procedure Draw(Context: CGContextRef); override;
  public
    constructor Create(B: IBitmap);
  end;

{ TGradientBrushQuartz }

  TGradientStop = record
    Color: TColorB;
    Offset: Float;
  end;

  TGradientBrushQuartz = class(TPatternBrushQuartz, IGradientBrush)
  private
    FStops: array of TGradientStop;
    FWrap: TGradientWrap;
  protected
    { Create a gradient made of the stops repeated for each period from
      First up to but not including Last }
    function GradientCreate(First, Last: Integer): CGGradientRef;
  public
    function GetWrap: TGradientWrap;
    procedure SetWrap(Value: TGradientWrap);
    procedure AddStop(Color: TColorB; Offset: Float);
  end;

{ TLinearGradientBrushQuartz }

  TLinearGradientBrushQuartz = class(TGradientBrushQuartz, ILinearGradientBrush)
  private
    FOrigin: TPointF;
    FAngle: Double;
    FDist: Double;
  protected
    procedure Draw(Context: CGContextRef); override;
  public
    constructor Create(X1, Y1, X2, Y2: Float);
  end;

{ TRadialGradientBrushQuartz }

  TRadialGradientBrushQuartz = class(TGradientBrushQuartz, IRadialGradientBrush)
  private
    FOrigin: TPointF;
    FRadius: Double;
    FScaleY: Double;
  protected
    procedure Draw(Context: CGContextRef); override;
  public
    constructor Create(const Rect: TRectF);
  end;

{ TFontQuartz }

  TFontQuartz = class(TInterfacedObject, IFont)
  private
    FFont: CTFontRef;
    FName: string;
    FColor: TColorB;
    FQuality: TFontQuality;
    FStyle: TFontStyles;
    FSize: Float;
    FKerning: Float;
    class function DefaultSize: Float;
    procedure FontRelease;
    function FontNeeded: CTFontRef;
  public
    constructor Create(Font: TFont); overload;
    constructor Create(const FontName: string; FontSize: Integer = 10); overload;
    destructor Destroy; override;
    function GetName: string;
    procedure SetName(const Value: string);
    function GetColor: TColorB;
    procedure SetColor(Value: TColorB);
    function GetQuality: TFontQuality;
    procedure SetQuality(Value: TFontQuality);
    function GetStyle: TFontStyles;
    procedure SetStyle(Value: TFontStyles);
    function GetSize: Float;
    procedure SetSize(Value: Float);
    function GetKerning: Float;
    procedure SetKerning(Value: Float);
  end;

{ TPathQuartz }

  TPathQuartz = class(TInterfacedObject, IPathData)
  private
    FPath: CGPathRef;
  public
    constructor Create(Path: CGPathRef);
    destructor Destroy; override;
  end;

  TSurfaceQuartz = class;

{ TSurfacePathQuartz }

  TSurfacePathQuartz = class(TInterfacedObject, IPath)
  private
    FSurface: TSurfaceQuartz;
    function HandleAvailable: Boolean;
  public
    constructor Create(Surface: TSurfaceQuartz);
    function Clone: IPathData;
    procedure Add;
    procedure Remove;
    procedure Close;
    procedure Join(Path: IPathData);
    procedure Clip;
    procedure Unclip;
  end;

{ TTextLayout is text broken into lines and measured }

  TTextLine = record
    Line: CTLineRef;
    Ascent: Double;
    Height: Double;
  end;

  TTextLayout = record
    Lines: array of TTextLine;
    Width: Double;
    Height: Double;
  end;

  TBitmapQuartz = class;

{ TSurfaceQuartz }

  TSurfaceQuartz = class(TInterfacedObject, ISurface)
  private
    FContext: CGContextRef;
    { When true the context is released with the surface }
    FOwned: Boolean;
    { The transform of the context when the surface was attached, which maps
      our coordinates with the origin at the top left to the device }
    FBase: CGAffineTransform;
    FMatrix: IMatrixQuartz;
    FPath: IPath;
    FPathQuartz: TSurfacePathQuartz;
    { The matrix saved by each call to Save }
    FStates: array of CGAffineTransform;
    { When true the next line or arc starts a new sub path }
    FNewSubPath: Boolean;
    procedure ApplyPen(P: IPen);
    procedure ArcPath(CX, CY, RX, RY, BeginAngle, EndAngle: Double);
    { Fill the current path using a brush and remove it }
    procedure FillPath(Brush: IBrush);
    function PathEmpty: Boolean;
    procedure Unclip;
    function LayoutCreate(Font: IFont; const Text: string; MaxWidth, MaxHeight: Double;
      Ellipsis: Boolean; out Layout: TTextLayout): Boolean;
    procedure LayoutFree(var Layout: TTextLayout);
  protected
    procedure Attach(Context: CGContextRef; Owned: Boolean);
    function HandleAvailable: Boolean; virtual;
    procedure HandleRelease;
  public
    constructor Create;
    destructor Destroy; override;
    function GetMatrix: IMatrix;
    procedure SetMatrix(Value: IMatrix);
    function GetPath: IPath;
    function GetHandle: Pointer;
    procedure Flush;
    procedure Clear(Color: TColorB);
    procedure CopyTo(const Source: TRectF; Surface: ISurface; const Dest: TRectF;
      Alpha: Byte = $FF; Quality: TResampleQuality = rqNormal);
    procedure Save;
    procedure Restore;
    procedure MoveTo(X, Y: Float);
    procedure LineTo(X, Y: Float);
    procedure ArcTo(const Rect: TRectF; BeginAngle, EndAngle: Float);
    procedure CurveTo(X, Y: Float; const C1, C2: TPointF);
    procedure Ellipse(const Rect: TRectF);
    procedure Rectangle(const Rect: TRectF);
    procedure RoundRectangle(const Rect: TRectF; Radius: Float);
    function TextSize(Font: IFont; const Text: string): TPointF;
    function TextHeight(Font: IFont; const Text: string; Width: Float): Float;
    procedure TextOut(Font: IFont; const Text: string; const Rect: TRectF;
      Direction: TDirection; Immediate: Boolean = True);
    procedure Stroke(Pen: IPen; Preserve: Boolean = False);
    procedure Fill(Brush: IBrush; Preserve: Boolean = False);
    procedure StrokeRect(Pen: IPen; const Rect: TRectF);
    procedure FillRect(Brush: IBrush; const Rect: TRectF);
    procedure StrokeRoundRect(Pen: IPen; const Rect: TRectF; Radius: Float);
    procedure FillRoundRect(Brush: IBrush; const Rect: TRectF; Radius: Float);
    property Matrix: IMatrix read GetMatrix write SetMatrix;
    property Path: IPath read GetPath;
  end;

{ TBitmapSurfaceQuartz }

  TBitmapSurfaceQuartz = class(TSurfaceQuartz)
  private
    FBitmap: TBitmapQuartz;
  protected
    function HandleAvailable: Boolean; override;
  public
    constructor Create(Bitmap: TBitmapQuartz);
  end;

{ TBitmapQuartz }

  TBitmapQuartz = class(TInterfacedObject, IBitmap)
  private
    FPixels: Pointer;
    FWidth: Integer;
    FHeight: Integer;
    FSurface: ISurface;
    FSurfaceQuartz: TBitmapSurfaceQuartz;
    FFormat: TImageFormat;
    procedure Flush;
    { Create an image which is a copy of the pixels }
    function ImageCreate: CGImageRef;
    { Replace the pixels with those of an image }
    procedure ImageLoad(Image: CGImageRef; Width, Height: Integer;
      Quality: CGInterpolationQuality);
    procedure Save(Stream: TStream; Format: TImageFormat);
  public
    destructor Destroy; override;
    function Clone: IBitmap;
    function GetEmpty: Boolean;
    function GetSurface: ISurface;
    function GetClientRect: TRectI;
    function GetFormat: TImageFormat;
    procedure SetFormat(Value: TImageFormat);
    function GetHeight: Integer;
    function GetWidth: Integer;
    function GetPixels: Pointer;
    procedure Clear;
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(Stream: TStream);
    function Resample(Width, Height: Integer; Quality: TResampleQuality = rqNormal): IBitmap;
    procedure SetSize(Width, Height: Integer);
  end;

{ TMatrixQuartz }

constructor TMatrixQuartz.Create;
begin
  inherited Create;
  FMatrix := MatrixIdentity;
  FChanged := False;
end;

constructor TMatrixQuartz.Create(const M: CGAffineTransform);
begin
  inherited Create;
  FMatrix := M;
  FChanged := True;
end;

function TMatrixQuartz.Clone: IMatrix;
begin
  Result := TMatrixQuartz.Create(FMatrix);
end;

procedure TMatrixQuartz.Identity;
begin
  FMatrix := MatrixIdentity;
  FChanged := True;
end;

{ As with the other backends each change is applied after the changes which
  came before it }

procedure TMatrixQuartz.Multiply(M: IMatrix);
var
  Q: IMatrixQuartz;
begin
  if Supports(M, IMatrixQuartz, Q) then
    FMatrix := CGAffineTransformConcat(FMatrix, Q.Matrix);
  FChanged := True;
end;

procedure TMatrixQuartz.Translate(X, Y: Float);
begin
  FMatrix := CGAffineTransformConcat(FMatrix, CGAffineTransformMakeTranslation(X, Y));
  FChanged := True;
end;

procedure TMatrixQuartz.Scale(X, Y: Float);
begin
  FMatrix := CGAffineTransformConcat(FMatrix, CGAffineTransformMakeScale(X, Y));
  FChanged := True;
end;

procedure TMatrixQuartz.Rotate(Radians: Float);
begin
  FMatrix := CGAffineTransformConcat(FMatrix, CGAffineTransformMakeRotation(Radians));
  FChanged := True;
end;

function TMatrixQuartz.Transform(Point: TPointF): TPointF;
begin
  Result.X := FMatrix.a * Point.X + FMatrix.c * Point.Y + FMatrix.tx;
  Result.Y := FMatrix.b * Point.X + FMatrix.d * Point.Y + FMatrix.ty;
end;

function TMatrixQuartz.GetMatrix: CGAffineTransform;
begin
  Result := FMatrix;
end;

procedure TMatrixQuartz.SetMatrix(const Value: CGAffineTransform);
begin
  FMatrix := Value;
  FChanged := True;
end;

function TMatrixQuartz.GetChanged: Boolean;
begin
  Result := FChanged;
end;

procedure TMatrixQuartz.SetChanged(Value: Boolean);
begin
  FChanged := Value;
end;

{ TPenQuartz }

constructor TPenQuartz.Create(B: IBrush; W: Float);
begin
  inherited Create;
  FBrush := B;
  FWidth := W;
  FMiterLimit := PenMiterLimitDefault;
end;

constructor TPenQuartz.Create(C: TColorB; W: Float);
begin
  inherited Create;
  FColor := C;
  FWidth := W;
  FMiterLimit := PenMiterLimitDefault;
end;

function TPenQuartz.GetBrush: IBrush;
begin
  Result := FBrush;
end;

procedure TPenQuartz.SetBrush(Value: IBrush);
begin
  FBrush := Value;
end;

function TPenQuartz.GetColor: TColorB;
begin
  Result := FColor;
end;

procedure TPenQuartz.SetColor(Value: TColorB);
begin
  FColor := Value;
end;

function TPenQuartz.GetWidth: Float;
begin
  Result := FWidth;
end;

procedure TPenQuartz.SetWidth(Value: Float);
begin
  FWidth := Value;
end;

function TPenQuartz.GetLinePattern: TLinePattern;
begin
  Result := FLinePattern;
end;

procedure TPenQuartz.SetLinePattern(Value: TLinePattern);
begin
  FLinePattern := Value;
end;

function TPenQuartz.GetLinePatternOffset: Float;
begin
  Result := FLinePatternOffset;
end;

procedure TPenQuartz.SetLinePatternOffset(Value: Float);
begin
  FLinePatternOffset := Value;
end;

function TPenQuartz.GetLineCap: TLineCap;
begin
  Result := FLineCap;
end;

procedure TPenQuartz.SetLineCap(Value: TLineCap);
begin
  FLineCap := Value;
end;

function TPenQuartz.GetLineJoin: TLineJoin;
begin
  Result := FLineJoin;
end;

procedure TPenQuartz.SetLineJoin(Value: TLineJoin);
begin
  FLineJoin := Value;
end;

function TPenQuartz.GetMiterLimit: Float;
begin
  Result := FMiterLimit;
end;

procedure TPenQuartz.SetMiterLimit(Value: Float);
begin
  if Value < 0 then
    Value := 0
  else if Value > 100 then
    Value := 100;
  FMiterLimit := Value;
end;

{ TBrushQuartz }

constructor TBrushQuartz.Create;
begin
  inherited Create;
  FOpacity := $FF;
end;

function TBrushQuartz.GetMatrix: IMatrix;
const
  DummyMatrix: IMatrix = nil;
begin
  if DummyMatrix = nil then
    DummyMatrix := NewMatrixQuartz;
  Result := DummyMatrix;
end;

procedure TBrushQuartz.SetMatrix(Value: IMatrix);
begin
end;

function TBrushQuartz.GetOpacity: Byte;
begin
  Result := FOpacity;
end;

procedure TBrushQuartz.SetOpacity(Value: Byte);
begin
  FOpacity := Value;
end;

{ TSolidBrushQuartz }

constructor TSolidBrushQuartz.Create(C: TColorB);
begin
  inherited Create;
  FColor := C;
end;

function TSolidBrushQuartz.GetColor: TColorB;
begin
  Result := FColor;
end;

procedure TSolidBrushQuartz.SetColor(Value: TColorB);
begin
  FColor := Value;
end;

{ TPatternBrushQuartz }

function TPatternBrushQuartz.Transform: CGAffineTransform;
var
  Q: IMatrixQuartz;
begin
  if Supports(FMatrix, IMatrixQuartz, Q) then
    Result := Q.Matrix
  else
    Result := MatrixIdentity;
end;

function TPatternBrushQuartz.GetMatrix: IMatrix;
begin
  if FMatrix = nil then
    FMatrix := NewMatrixQuartz;
  Result := FMatrix;
end;

procedure TPatternBrushQuartz.SetMatrix(Value: IMatrix);
var
  M: IMatrix;
begin
  { Clone the value first in case it is our own matrix }
  if Value <> nil then
    M := Value.Clone
  else
    M := nil;
  if FMatrix = nil then
    FMatrix := NewMatrixQuartz;
  FMatrix.Identity;
  if M <> nil then
    FMatrix.Multiply(M);
end;

{ TBitmapBrushQuartz }

constructor TBitmapBrushQuartz.Create(B: IBitmap);
begin
  inherited Create;
  FBitmap := B.Clone;
end;

procedure TBitmapBrushQuartz.Draw(Context: CGContextRef);
var
  Image: CGImageRef;
  W, H: Integer;
begin
  if (FBitmap = nil) or FBitmap.Empty then
    Exit;
  Image := (FBitmap as TBitmapQuartz).ImageCreate;
  if Image = nil then
    Exit;
  W := FBitmap.Width;
  H := FBitmap.Height;
  CGContextConcatCTM(Context, Transform);
  { Images are drawn bottom up, so the tile is turned over to be upright }
  CGContextConcatCTM(Context, CGAffineTransformMake(1, 0, 0, -1, 0, H));
  CGContextDrawTiledImage(Context, CGRectMake(0, 0, W, H), Image);
  CGImageRelease(Image);
end;

{ TGradientBrushQuartz }

function TGradientBrushQuartz.GradientCreate(First, Last: Integer): CGGradientRef;
var
  Colors, Offsets: array of CGFloat;
  Periods, Count, I, J, K, N: Integer;
  Reverse: Boolean;
  Stop: TGradientStop;
  Offset: Double;
begin
  Result := nil;
  Count := Length(FStops);
  if Count < 1 then
    Exit;
  Periods := Last - First;
  if Periods < 1 then
    Exit;
  SetLength(Colors, Periods * Count * 4);
  SetLength(Offsets, Periods * Count);
  N := 0;
  for I := 0 to Periods - 1 do
  begin
    { Every second period of a reflected gradient runs backwards }
    Reverse := (FWrap = gwReflect) and Odd(Abs(First + I));
    for J := 0 to Count - 1 do
    begin
      if Reverse then
        K := Count - 1 - J
      else
        K := J;
      Stop := FStops[K];
      Offset := Stop.Offset;
      if Reverse then
        Offset := 1 - Offset;
      Offsets[N] := (I + Offset) / Periods;
      Colors[N * 4] := Stop.Color.Red / $FF;
      Colors[N * 4 + 1] := Stop.Color.Green / $FF;
      Colors[N * 4 + 2] := Stop.Color.Blue / $FF;
      Colors[N * 4 + 3] := Stop.Color.Alpha / $FF;
      Inc(N);
    end;
  end;
  Result := CGGradientCreateWithColorComponents(ColorSpace, @Colors[0], @Offsets[0], N);
end;

function TGradientBrushQuartz.GetWrap: TGradientWrap;
begin
  Result := FWrap;
end;

procedure TGradientBrushQuartz.SetWrap(Value: TGradientWrap);
begin
  FWrap := Value;
end;

procedure TGradientBrushQuartz.AddStop(Color: TColorB; Offset: Float);
var
  I, J: Integer;
begin
  if Offset < 0 then
    Offset := 0
  else if Offset > 1 then
    Offset := 1;
  { Stops are kept in order of their offsets }
  I := Length(FStops);
  SetLength(FStops, I + 1);
  while (I > 0) and (FStops[I - 1].Offset > Offset) do
    Dec(I);
  for J := Length(FStops) - 1 downto I + 1 do
    FStops[J] := FStops[J - 1];
  FStops[I].Color := Color;
  FStops[I].Offset := Offset;
end;

{ TLinearGradientBrushQuartz }

constructor TLinearGradientBrushQuartz.Create(X1, Y1, X2, Y2: Float);
var
  X, Y: Double;
begin
  inherited Create;
  { The gradient runs along a horizontal line centered on the origin, which is
    rotated and moved into place when drawn. The brush matrix is applied to
    the gradient before it is rotated and moved. }
  FOrigin.X := (X1 + X2) / 2;
  FOrigin.Y := (Y1 + Y2) / 2;
  X := X2 - FOrigin.X;
  Y := Y2 - FOrigin.Y;
  FAngle := ArcTangent(Y, X);
  FDist := Sqrt(X * X + Y * Y);
end;

procedure TLinearGradientBrushQuartz.Draw(Context: CGContextRef);
var
  Gradient: CGGradientRef;
  Box: CGRect;
  Period: Double;
  First, Last: Integer;
begin
  if FDist = 0 then
    Exit;
  CGContextConcatCTM(Context, CGAffineTransformMakeTranslation(FOrigin.X, FOrigin.Y));
  CGContextConcatCTM(Context, CGAffineTransformMakeRotation(FAngle));
  CGContextConcatCTM(Context, Transform);
  First := 0;
  Last := 1;
  if FWrap <> gwClamp then
  begin
    { Find the periods of the gradient needed to cover the area being filled }
    Box := CGContextGetClipBoundingBox(Context);
    Period := FDist * 2;
    First := FloorInt((Box.origin.x + FDist) / Period);
    Last := CeilInt((Box.origin.x + Box.size.width + FDist) / Period);
    if Last <= First then
      Last := First + 1;
    if Last - First > MaxPeriods then
    begin
      First := 0;
      Last := 1;
    end;
  end;
  Gradient := GradientCreate(First, Last);
  if Gradient = nil then
    Exit;
  CGContextDrawLinearGradient(Context, Gradient,
    CGPointMake(-FDist + First * FDist * 2, 0),
    CGPointMake(-FDist + Last * FDist * 2, 0),
    kCGGradientDrawsBeforeStartLocation or kCGGradientDrawsAfterEndLocation);
  CGGradientRelease(Gradient);
end;

{ TRadialGradientBrushQuartz }

constructor TRadialGradientBrushQuartz.Create(const Rect: TRectF);
begin
  inherited Create;
  FOrigin := Rect.MidPoint;
  FRadius := Rect.Width / 2;
  if Rect.Width <> 0 then
    FScaleY := Rect.Height / Rect.Width;
end;

procedure TRadialGradientBrushQuartz.Draw(Context: CGContextRef);

  function Dist(X, Y: Double): Double;
  begin
    Result := Sqrt(X * X + Y * Y);
  end;

var
  Gradient: CGGradientRef;
  Box: CGRect;
  D, Far: Double;
  Last: Integer;
begin
  if FRadius <= 0 then
    Exit;
  CGContextConcatCTM(Context, CGAffineTransformMakeTranslation(FOrigin.X, FOrigin.Y));
  CGContextConcatCTM(Context, Transform);
  if FScaleY <> 0 then
    CGContextConcatCTM(Context, CGAffineTransformMakeScale(1, FScaleY));
  Last := 1;
  if FWrap <> gwClamp then
  begin
    { Find the periods of the gradient needed to reach the furthest corner of
      the area being filled }
    Box := CGContextGetClipBoundingBox(Context);
    Far := Dist(Box.origin.x, Box.origin.y);
    D := Dist(Box.origin.x + Box.size.width, Box.origin.y);
    if D > Far then
      Far := D;
    D := Dist(Box.origin.x, Box.origin.y + Box.size.height);
    if D > Far then
      Far := D;
    D := Dist(Box.origin.x + Box.size.width, Box.origin.y + Box.size.height);
    if D > Far then
      Far := D;
    Last := CeilInt(Far / FRadius);
    if (Last < 1) or (Last > MaxPeriods) then
      Last := 1;
  end;
  Gradient := GradientCreate(0, Last);
  if Gradient = nil then
    Exit;
  CGContextDrawRadialGradient(Context, Gradient, CGPointMake(0, 0), 0,
    CGPointMake(0, 0), FRadius * Last, kCGGradientDrawsAfterEndLocation);
  CGGradientRelease(Gradient);
end;

{ TFontQuartz }

class function TFontQuartz.DefaultSize: Float;
var
  F: CTFontRef;
begin
  Result := 10;
  F := CTFontCreateUIFontForLanguage(kCTFontSystemFontType, 0, nil);
  if F <> nil then
  begin
    Result := CTFontGetSize(F) / FontScale;
    CFRelease(F);
  end;
end;

constructor TFontQuartz.Create(Font: TFont);
begin
  inherited Create;
  FColor := clBlack;
  if Font = nil then
  begin
    FName := 'default';
    FSize := DefaultSize;
    Exit;
  end;
  FName := Font.Name;
  FQuality := Font.Quality;
  FColor := Font.Color;
  FStyle := Font.Style;
  if Font.Size > 4 then
    SetSize(Font.Size)
  else
    SetSize(DefaultSize);
end;

constructor TFontQuartz.Create(const FontName: string; FontSize: Integer = 10);
begin
  inherited Create;
  FColor := clBlack;
  FName := FontName;
  SetSize(FontSize);
end;

destructor TFontQuartz.Destroy;
begin
  FontRelease;
  inherited Destroy;
end;

procedure TFontQuartz.FontRelease;
begin
  if FFont <> nil then
    CFRelease(FFont);
  FFont := nil;
end;

function TFontQuartz.FontNeeded: CTFontRef;
var
  Name: CFStringRef;
  Traits: CTFontSymbolicTraits;
  F: CTFontRef;
  S: Double;
begin
  Result := FFont;
  if Result <> nil then
    Exit;
  S := FSize * FontScale;
  if (FName = '') or SameText(FName, 'default') then
    FFont := CTFontCreateUIFontForLanguage(kCTFontSystemFontType, S, nil)
  else
  begin
    Name := StrToCF(FName);
    if Name <> nil then
    begin
      FFont := CTFontCreateWithName(Name, S, nil);
      CFRelease(Name);
    end;
  end;
  if FFont = nil then
    FFont := CTFontCreateUIFontForLanguage(kCTFontSystemFontType, S, nil);
  Traits := 0;
  if fsBold in FStyle then
    Traits := Traits or kCTFontBoldTrait;
  if fsItalic in FStyle then
    Traits := Traits or kCTFontItalicTrait;
  if (Traits <> 0) and (FFont <> nil) then
  begin
    { This is nil when the font family has no such face, in which case the
      plain face is kept }
    F := CTFontCreateCopyWithSymbolicTraits(FFont, S, nil, Traits,
      kCTFontBoldTrait or kCTFontItalicTrait);
    if F <> nil then
    begin
      CFRelease(FFont);
      FFont := F;
    end;
  end;
  Result := FFont;
end;

function TFontQuartz.GetName: string;
begin
  Result := FName;
end;

procedure TFontQuartz.SetName(const Value: string);
begin
  if Value <> FName then
  begin
    FName := Value;
    FontRelease;
  end;
end;

function TFontQuartz.GetColor: TColorB;
begin
  Result := FColor;
end;

procedure TFontQuartz.SetColor(Value: TColorB);
begin
  FColor := Value;
end;

function TFontQuartz.GetQuality: TFontQuality;
begin
  Result := FQuality;
end;

procedure TFontQuartz.SetQuality(Value: TFontQuality);
begin
  FQuality := Value;
end;

function TFontQuartz.GetStyle: TFontStyles;
begin
  Result := FStyle;
end;

procedure TFontQuartz.SetStyle(Value: TFontStyles);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    FontRelease;
  end;
end;

function TFontQuartz.GetSize: Float;
begin
  Result := FSize;
end;

procedure TFontQuartz.SetSize(Value: Float);
const
  MinSize = 4;
begin
  if Value < MinSize then
    Value := MinSize;
  if Value <> FSize then
  begin
    FSize := Value;
    FontRelease;
  end;
end;

function TFontQuartz.GetKerning: Float;
begin
  Result := FKerning;
end;

procedure TFontQuartz.SetKerning(Value: Float);
begin
  FKerning := Value;
end;

{ TPathQuartz }

constructor TPathQuartz.Create(Path: CGPathRef);
begin
  inherited Create;
  FPath := Path;
end;

destructor TPathQuartz.Destroy;
begin
  if FPath <> nil then
    CGPathRelease(FPath);
  inherited Destroy;
end;

{ TSurfacePathQuartz }

constructor TSurfacePathQuartz.Create(Surface: TSurfaceQuartz);
begin
  inherited Create;
  FSurface := Surface;
end;

function TSurfacePathQuartz.HandleAvailable: Boolean;
begin
  Result := (FSurface <> nil) and FSurface.HandleAvailable;
end;

function TSurfacePathQuartz.Clone: IPathData;
begin
  if HandleAvailable and (not FSurface.PathEmpty) then
    Result := TPathQuartz.Create(CGContextCopyPath(FSurface.FContext))
  else
    Result := TPathQuartz.Create(nil);
end;

procedure TSurfacePathQuartz.Add;
begin
  if HandleAvailable then
    FSurface.FNewSubPath := True;
end;

procedure TSurfacePathQuartz.Remove;
begin
  if HandleAvailable then
  begin
    CGContextBeginPath(FSurface.FContext);
    FSurface.FNewSubPath := False;
  end;
end;

procedure TSurfacePathQuartz.Close;
begin
  if HandleAvailable and (not FSurface.PathEmpty) then
    CGContextClosePath(FSurface.FContext);
end;

procedure TSurfacePathQuartz.Join(Path: IPathData);
var
  P: TPathQuartz;
begin
  if HandleAvailable then
    if Path is TPathQuartz then
    begin
      P := Path as TPathQuartz;
      if P.FPath <> nil then
        CGContextAddPath(FSurface.FContext, P.FPath);
    end;
end;

procedure TSurfacePathQuartz.Clip;
begin
  if not HandleAvailable then
    Exit;
  { Clipping to nothing leaves nothing to draw on }
  if FSurface.PathEmpty then
    CGContextClipToRect(FSurface.FContext, CGRectMake(0, 0, 0, 0))
  else
    CGContextClip(FSurface.FContext);
  FSurface.FNewSubPath := False;
end;

procedure TSurfacePathQuartz.Unclip;
begin
  if HandleAvailable then
    FSurface.Unclip;
end;

{ TSurfaceQuartz }

constructor TSurfaceQuartz.Create;
begin
  inherited Create;
  FMatrix := TMatrixQuartz.Create;
  FBase := MatrixIdentity;
end;

destructor TSurfaceQuartz.Destroy;
begin
  HandleRelease;
  if FPathQuartz <> nil then
    FPathQuartz.FSurface := nil;
  inherited Destroy;
end;

{ The graphics state of the context is saved when it is attached and restored
  when it is released, leaving a context we do not own as we found it }

procedure TSurfaceQuartz.Attach(Context: CGContextRef; Owned: Boolean);
begin
  HandleRelease;
  FContext := Context;
  FOwned := Owned;
  if FContext = nil then
    Exit;
  CGContextSaveGState(FContext);
  FBase := CGContextGetCTM(FContext);
  CGContextSetBlendMode(FContext, kCGBlendModeNormal);
  CGContextSetAlpha(FContext, 1);
  CGContextSetShouldAntialias(FContext, 1);
  CGContextBeginPath(FContext);
  FNewSubPath := False;
  FMatrix.Changed := True;
end;

function TSurfaceQuartz.HandleAvailable: Boolean;
var
  M: CGAffineTransform;
begin
  Result := FContext <> nil;
  if Result and FMatrix.Changed then
  begin
    { Core Graphics can only add to the transform of a context, so it is taken
      back to the identity before our transform is added }
    M := CGContextGetCTM(FContext);
    CGContextConcatCTM(FContext, CGAffineTransformInvert(M));
    CGContextConcatCTM(FContext, CGAffineTransformConcat(FMatrix.Matrix, FBase));
    FMatrix.Changed := False;
  end;
end;

procedure TSurfaceQuartz.HandleRelease;
var
  I: Integer;
begin
  if FContext = nil then
    Exit;
  CGContextBeginPath(FContext);
  for I := 0 to Length(FStates) do
    CGContextRestoreGState(FContext);
  FStates := nil;
  if FOwned then
    CGContextRelease(FContext);
  FContext := nil;
end;

function TSurfaceQuartz.PathEmpty: Boolean;
begin
  Result := CGContextIsPathEmpty(FContext) <> 0;
end;

{ Core Graphics can only reduce the clipping area of a context. To remove it
  every saved state is restored, which brings back the clipping area the
  context was attached with, and then the states are saved again. }

procedure TSurfaceQuartz.Unclip;
var
  I: Integer;
begin
  for I := 0 to Length(FStates) do
    CGContextRestoreGState(FContext);
  for I := 0 to Length(FStates) do
    CGContextSaveGState(FContext);
  CGContextSetBlendMode(FContext, kCGBlendModeNormal);
  CGContextSetAlpha(FContext, 1);
  CGContextSetShouldAntialias(FContext, 1);
  FMatrix.Changed := True;
  HandleAvailable;
end;

procedure TSurfaceQuartz.ApplyPen(P: IPen);
const
  LineCaps: array[TLineCap] of CGLineCap = (kCGLineCapButt, kCGLineCapRound,
    kCGLineCapSquare);
  LineJoins: array[TLineJoin] of CGLineJoin = (kCGLineJoinMiter, kCGLineJoinRound,
    kCGLineJoinBevel);
var
  Dashes: array[0..3] of CGFloat;
  W: Double;
  I: Integer;
begin
  CGContextSetLineWidth(FContext, P.Width);
  CGContextSetLineCap(FContext, LineCaps[P.LineCap]);
  CGContextSetLineJoin(FContext, LineJoins[P.LineJoin]);
  CGContextSetMiterLimit(FContext, P.MiterLimit);
  W := P.Width;
  case P.LinePattern of
    pnDash:
      begin
        Dashes[0] := W * 2;
        Dashes[1] := W * 2;
        I := 2;
      end;
    pnDot:
      if P.LineCap = cpButt then
      begin
        Dashes[0] := W;
        Dashes[1] := W;
        I := 2;
      end
      else
      begin
        Dashes[0] := 0;
        Dashes[1] := W * 2;
        I := 2;
      end;
    pnDashDot:
      begin
        Dashes[0] := W * 2;
        Dashes[1] := W * 2;
        Dashes[2] := W;
        Dashes[3] := W * 2;
        I := 4;
      end;
  else
    I := 0;
  end;
  if (I > 0) and (W > 0) then
    CGContextSetLineDash(FContext, P.LinePatternOffset, @Dashes[0], I)
  else
    CGContextSetLineDash(FContext, 0, nil, 0);
end;

procedure TSurfaceQuartz.FillPath(Brush: IBrush);
var
  C: TColorB;
  P: TPatternBrushQuartz;
  A: Double;
begin
  FNewSubPath := False;
  if PathEmpty then
    Exit;
  if (Brush = nil) or (Brush.Opacity = 0) then
  begin
    CGContextBeginPath(FContext);
    Exit;
  end;
  A := Brush.Opacity / $FF;
  if Brush is TSolidBrushQuartz then
  begin
    C := (Brush as TSolidBrushQuartz).FColor;
    CGContextSetRGBFillColor(FContext, C.Red / $FF, C.Green / $FF, C.Blue / $FF,
      C.Alpha / $FF * A);
    CGContextFillPath(FContext);
  end
  else if Brush is TPatternBrushQuartz then
  begin
    P := Brush as TPatternBrushQuartz;
    CGContextSaveGState(FContext);
    CGContextClip(FContext);
    CGContextSetAlpha(FContext, A);
    P.Draw(FContext);
    CGContextRestoreGState(FContext);
  end
  else
  begin
    CGContextSetRGBFillColor(FContext, 0, 0, 0, A);
    CGContextFillPath(FContext);
  end;
end;

function TSurfaceQuartz.GetMatrix: IMatrix;
begin
  Result := FMatrix;
end;

procedure TSurfaceQuartz.SetMatrix(Value: IMatrix);
var
  Q: IMatrixQuartz;
begin
  if Supports(Value, IMatrixQuartz, Q) then
    FMatrix.Matrix := Q.Matrix
  else
    FMatrix.Identity;
end;

function TSurfaceQuartz.GetPath: IPath;
begin
  if FPath = nil then
  begin
    FPathQuartz := TSurfacePathQuartz.Create(Self);
    FPath := FPathQuartz;
  end;
  Result := FPath;
end;

function TSurfaceQuartz.GetHandle: Pointer;
begin
  HandleAvailable;
  Result := FContext;
end;

procedure TSurfaceQuartz.Flush;
begin
  if FContext <> nil then
    CGContextFlush(FContext);
end;

procedure TSurfaceQuartz.Clear(Color: TColorB);
begin
  if not HandleAvailable then
    Exit;
  CGContextSaveGState(FContext);
  CGContextSetBlendMode(FContext, kCGBlendModeCopy);
  CGContextSetRGBFillColor(FContext, Color.Red / $FF, Color.Green / $FF,
    Color.Blue / $FF, Color.Alpha / $FF);
  CGContextFillRect(FContext, CGContextGetClipBoundingBox(FContext));
  CGContextRestoreGState(FContext);
end;

procedure TSurfaceQuartz.CopyTo(const Source: TRectF; Surface: ISurface;
  const Dest: TRectF; Alpha: Byte = $FF; Quality: TResampleQuality = rqNormal);
const
  Resamples: array[TResampleQuality] of CGInterpolationQuality =
    (kCGInterpolationNone, kCGInterpolationMedium, kCGInterpolationHigh);
var
  DestSurface: TSurfaceQuartz;
  D: CGContextRef;
  Image: CGImageRef;
  SX, SY, X, Y, W, H: Double;
begin
  if not (Surface is TSurfaceQuartz) then
    Exit;
  DestSurface := Surface as TSurfaceQuartz;
  if not DestSurface.HandleAvailable then
    Exit;
  D := DestSurface.FContext;
  if Alpha = 0 then
  begin
    CGContextBeginPath(D);
    Exit;
  end;
  if Source.Empty or Dest.Empty then
    Exit;
  if not HandleAvailable then
    Exit;
  { Only a surface which draws on a bitmap can be copied from }
  Image := CGBitmapContextCreateImage(FContext);
  if Image = nil then
    Exit;
  SX := Dest.Width / Source.Width;
  SY := Dest.Height / Source.Height;
  W := CGImageGetWidth(Image) * SX;
  H := CGImageGetHeight(Image) * SY;
  X := Dest.X - Source.X * SX;
  Y := Dest.Y - Source.Y * SY;
  CGContextSaveGState(D);
  CGContextBeginPath(D);
  CGContextClipToRect(D, ToRect(Dest));
  CGContextSetAlpha(D, Alpha / $FF);
  CGContextSetInterpolationQuality(D, Resamples[Quality]);
  { Images are drawn bottom up, so the image is turned over to be upright }
  CGContextTranslateCTM(D, X, Y + H);
  CGContextScaleCTM(D, 1, -1);
  CGContextDrawImage(D, CGRectMake(0, 0, W, H), Image);
  CGContextRestoreGState(D);
  CGImageRelease(Image);
end;

procedure TSurfaceQuartz.Save;
var
  I: Integer;
begin
  if not HandleAvailable then
    Exit;
  CGContextSaveGState(FContext);
  I := Length(FStates);
  SetLength(FStates, I + 1);
  FStates[I] := FMatrix.Matrix;
end;

procedure TSurfaceQuartz.Restore;
var
  I: Integer;
begin
  if not HandleAvailable then
    Exit;
  I := Length(FStates);
  if I < 1 then
    Exit;
  CGContextRestoreGState(FContext);
  FMatrix.Matrix := FStates[I - 1];
  SetLength(FStates, I - 1);
end;

procedure TSurfaceQuartz.MoveTo(X, Y: Float);
begin
  if not HandleAvailable then
    Exit;
  CGContextMoveToPoint(FContext, X, Y);
  FNewSubPath := False;
end;

procedure TSurfaceQuartz.LineTo(X, Y: Float);
begin
  if not HandleAvailable then
    Exit;
  if FNewSubPath or PathEmpty then
    CGContextMoveToPoint(FContext, X, Y)
  else
    CGContextAddLineToPoint(FContext, X, Y);
  FNewSubPath := False;
end;

{ Add an arc of an ellipse to the path, with angles measured clockwise from the
  positive x axis. A line joins the arc to the current point unless a new sub
  path was started. }

procedure TSurfaceQuartz.ArcPath(CX, CY, RX, RY, BeginAngle, EndAngle: Double);
var
  X, Y: Double;
begin
  X := CX + RX * Cos(BeginAngle);
  Y := CY + RY * Sin(BeginAngle);
  if FNewSubPath or PathEmpty then
    CGContextMoveToPoint(FContext, X, Y)
  else
    CGContextAddLineToPoint(FContext, X, Y);
  FNewSubPath := False;
  { Path points are transformed as they are added, so the transform can be
    restored without changing the arc }
  CGContextSaveGState(FContext);
  CGContextTranslateCTM(FContext, CX, CY);
  CGContextScaleCTM(FContext, RX, RY);
  CGContextAddArc(FContext, 0, 0, 1, BeginAngle, EndAngle, 0);
  CGContextRestoreGState(FContext);
end;

procedure TSurfaceQuartz.ArcTo(const Rect: TRectF; BeginAngle, EndAngle: Float);
const
  AngleDelta = Pi / 2;
begin
  if not HandleAvailable then
    Exit;
  if Rect.Empty then
    Exit;
  ArcPath(Rect.X + Rect.Width / 2, Rect.Y + Rect.Height / 2, Rect.Width / 2,
    Rect.Height / 2, BeginAngle - AngleDelta, EndAngle - AngleDelta);
end;

procedure TSurfaceQuartz.CurveTo(X, Y: Float; const C1, C2: TPointF);
begin
  if not HandleAvailable then
    Exit;
  if FNewSubPath or PathEmpty then
    CGContextMoveToPoint(FContext, C1.X, C1.Y);
  FNewSubPath := False;
  CGContextAddCurveToPoint(FContext, C1.X, C1.Y, C2.X, C2.Y, X, Y);
end;

procedure TSurfaceQuartz.Ellipse(const Rect: TRectF);
begin
  if not HandleAvailable then
    Exit;
  if Rect.Empty then
    Exit;
  FNewSubPath := True;
  ArcPath(Rect.X + Rect.Width / 2, Rect.Y + Rect.Height / 2, Rect.Width / 2,
    Rect.Height / 2, 0, Pi * 2);
  CGContextClosePath(FContext);
end;

procedure TSurfaceQuartz.Rectangle(const Rect: TRectF);
begin
  if not HandleAvailable then
    Exit;
  CGContextAddRect(FContext, ToRect(Rect));
  FNewSubPath := False;
end;

procedure TSurfaceQuartz.RoundRectangle(const Rect: TRectF; Radius: Float);
const
  D = Pi / 180;
begin
  if Rect.Empty or (Radius <= 0) then
    Exit;
  if not HandleAvailable then
    Exit;
  if Rect.Width < Radius * 2 then
    Radius := Rect.Width / 2;
  if Rect.Height < Radius * 2 then
    Radius := Rect.Height / 2;
  FNewSubPath := True;
  ArcPath(Rect.X + Rect.Width - Radius, Rect.Y + Radius, Radius, Radius,
    -90 * D, 0);
  ArcPath(Rect.X + Rect.Width - Radius, Rect.Y + Rect.Height - Radius, Radius,
    Radius, 0, 90 * D);
  ArcPath(Rect.X + Radius, Rect.Y + Rect.Height - Radius, Radius, Radius,
    90 * D, 180 * D);
  ArcPath(Rect.X + Radius, Rect.Y + Radius, Radius, Radius, 180 * D, 270 * D);
  CGContextClosePath(FContext);
end;

{ Break text into lines no wider than MaxWidth and measure them. When
  MaxWidth is less than one lines are only broken where the text has line
  breaks. When Ellipsis is true lines which do not fit in MaxHeight are
  dropped, and the last line which does fit ends with an ellipsis. }

function TSurfaceQuartz.LayoutCreate(Font: IFont; const Text: string;
  MaxWidth, MaxHeight: Double; Ellipsis: Boolean; out Layout: TTextLayout): Boolean;
const
  Unlimited = 10000000;
var
  Str, Dots: CFStringRef;
  Attributes: CFMutableDictionaryRef;
  Attributed, AttributedDots: CFAttributedStringRef;
  Typesetter: CTTypesetterRef;
  Line, Rest, Token, Truncated: CTLineRef;
  Number: CFNumberRef;
  F: TFontQuartz;
  Kerning, W, Width: Double;
  Ascent, Descent, Leading: CGFloat;
  Start, Prior, Count, Len: CFIndex;
  I: Integer;
begin
  Result := False;
  Layout.Lines := nil;
  Layout.Width := 0;
  Layout.Height := 0;
  if Text = '' then
    Exit;
  if not (Font is TFontQuartz) then
    Exit;
  F := Font as TFontQuartz;
  if F.FontNeeded = nil then
    Exit;
  Str := StrToCF(Text);
  if Str = nil then
    Exit;
  Attributes := CFDictionaryCreateMutable(nil, 0, @kCFTypeDictionaryKeyCallBacks,
    @kCFTypeDictionaryValueCallBacks);
  CFDictionarySetValue(Attributes, kCTFontAttributeName, F.FontNeeded);
  { Text is filled with the fill color of the context }
  CFDictionarySetValue(Attributes, kCTForegroundColorFromContextAttributeName,
    kCFBooleanTrue);
  if F.FKerning <> 0 then
  begin
    Kerning := F.FKerning;
    Number := CFNumberCreate(nil, kCFNumberFloat64Type, @Kerning);
    CFDictionarySetValue(Attributes, kCTKernAttributeName, Number);
    CFRelease(Number);
  end;
  Attributed := CFAttributedStringCreate(nil, Str, Attributes);
  Typesetter := CTTypesetterCreateWithAttributedString(Attributed);
  if MaxWidth < 1 then
    Width := Unlimited
  else
    Width := MaxWidth;
  Len := CFStringGetLength(Str);
  Start := 0;
  Prior := 0;
  while Start < Len do
  begin
    Count := CTTypesetterSuggestLineBreak(Typesetter, Start, Width);
    if Count < 1 then
      Count := Len - Start;
    Line := CTTypesetterCreateLine(Typesetter, CFRangeMake(Start, Count));
    if Line = nil then
      Break;
    Ascent := 0;
    Descent := 0;
    Leading := 0;
    CTLineGetTypographicBounds(Line, @Ascent, @Descent, @Leading);
    I := Length(Layout.Lines);
    if Ellipsis and (MaxHeight > 0) and (I > 0) and
      (Layout.Height + CeilInt(Ascent + Descent + Leading) > MaxHeight) then
    begin
      { This line does not fit, so the line before it takes the rest of the
        text and is cut short with an ellipsis }
      CFRelease(Line);
      Rest := CTTypesetterCreateLine(Typesetter, CFRangeMake(Prior, Len - Prior));
      { The utf8 bytes of an ellipsis }
      Dots := StrToCF(#$E2#$80#$A6);
      AttributedDots := CFAttributedStringCreate(nil, Dots, Attributes);
      Token := CTLineCreateWithAttributedString(AttributedDots);
      Truncated := CTLineCreateTruncatedLine(Rest, Width, kCTLineTruncationEnd, Token);
      if Truncated <> nil then
      begin
        CFRelease(Layout.Lines[I - 1].Line);
        Layout.Lines[I - 1].Line := Truncated;
      end;
      CFRelease(Token);
      CFRelease(AttributedDots);
      CFRelease(Dots);
      CFRelease(Rest);
      Break;
    end;
    SetLength(Layout.Lines, I + 1);
    Layout.Lines[I].Line := Line;
    Layout.Lines[I].Ascent := Ascent;
    Layout.Lines[I].Height := CeilInt(Ascent + Descent + Leading);
    Layout.Height := Layout.Height + Layout.Lines[I].Height;
    Prior := Start;
    Start := Start + Count;
  end;
  for I := 0 to Length(Layout.Lines) - 1 do
  begin
    Line := Layout.Lines[I].Line;
    W := CTLineGetTypographicBounds(Line, nil, nil, nil) -
      CTLineGetTrailingWhitespaceWidth(Line);
    if W > Layout.Width then
      Layout.Width := W;
  end;
  CFRelease(Typesetter);
  CFRelease(Attributed);
  CFRelease(Attributes);
  CFRelease(Str);
  Result := Length(Layout.Lines) > 0;
end;

procedure TSurfaceQuartz.LayoutFree(var Layout: TTextLayout);
var
  I: Integer;
begin
  for I := 0 to Length(Layout.Lines) - 1 do
    CFRelease(Layout.Lines[I].Line);
  Layout.Lines := nil;
end;

function TSurfaceQuartz.TextSize(Font: IFont; const Text: string): TPointF;
var
  Layout: TTextLayout;
begin
  Result := TPointF.Create(0, 0);
  if LayoutCreate(Font, Text, 0, 0, False, Layout) then
  begin
    Result := TPointF.Create(CeilInt(Layout.Width), Layout.Height);
    LayoutFree(Layout);
  end;
end;

function TSurfaceQuartz.TextHeight(Font: IFont; const Text: string; Width: Float): Float;
var
  Layout: TTextLayout;
begin
  Result := 0;
  if Width < 1 then
    Exit;
  if LayoutCreate(Font, Text, Round(Width), 0, False, Layout) then
  begin
    Result := Layout.Height;
    LayoutFree(Layout);
  end;
end;

procedure TSurfaceQuartz.TextOut(Font: IFont; const Text: string; const Rect: TRectF;
  Direction: TDirection; Immediate: Boolean = True);
var
  Layout: TTextLayout;
  Line: CTLineRef;
  Runs: CFArrayRef;
  Run: CTRunRef;
  RunFont: CTFontRef;
  Glyphs: array of CGGlyph;
  Positions: array of CGPoint;
  GlyphPath: CGPathRef;
  T: CGAffineTransform;
  R: TRectI;
  C: TColorB;
  Align: Double;
  X, Y, Baseline: Double;
  Ellipsis, Draw: Boolean;
  H, I, J, K, N: Integer;
begin
  Draw := SurfaceOptions.ErrorCorrection or Immediate;
  if Draw then
    Path.Remove;
  if not HandleAvailable then
    Exit;
  if Text = '' then
    Exit;
  R := TRectI(Rect);
  if Rect.Empty then
    Exit;
  Path.Add;
  { Text which does not wrap is cut short with an ellipsis }
  Ellipsis := Direction in [drLeft, drUp, drRight, drDown, drCenter];
  if not LayoutCreate(Font, Text, R.Width, R.Height, Ellipsis, Layout) then
    Exit;
  { Horizontal alignment }
  case Direction of
    drLeft, drWrap, drFlow:
      Align := 0;
    drUp, drDown, drCenter, drFill:
      Align := 0.5;
  else
    Align := 1;
  end;
  { Placement }
  H := Round(Layout.Height);
  case Direction of
    drUp, drWrap, drFlow:
      Y := R.Y;
    drLeft, drRight, drCenter, drFill:
      Y := R.Y + (R.Height - H) div 2;
  else
    Y := Rect.Y + R.Height - H;
  end;
  if Draw then
  begin
    CGContextSaveGState(FContext);
    C := Font.Color;
    CGContextSetRGBFillColor(FContext, C.Red / $FF, C.Green / $FF, C.Blue / $FF,
      C.Alpha / $FF);
    if Font.Quality = fqNonAntialiased then
      CGContextSetShouldAntialias(FContext, 0)
    else
      CGContextSetShouldAntialias(FContext, 1);
    { Our coordinates have the origin at the top left, which would draw text
      upside down unless the text matrix is turned over }
    CGContextSetTextMatrix(FContext, CGAffineTransformMakeScale(1, -1));
  end;
  for I := 0 to Length(Layout.Lines) - 1 do
  begin
    Line := Layout.Lines[I].Line;
    X := R.X + Round(CTLineGetPenOffsetForFlush(Line, Align, R.Width));
    Baseline := Y + Round(Layout.Lines[I].Ascent);
    if Draw then
    begin
      CGContextSetTextPosition(FContext, X, Baseline);
      CTLineDraw(Line, FContext);
    end
    else
    begin
      { Add the outline of each glyph to the path }
      Runs := CTLineGetGlyphRuns(Line);
      for J := 0 to CFArrayGetCount(Runs) - 1 do
      begin
        Run := CTRunRef(CFArrayGetValueAtIndex(Runs, J));
        RunFont := CTFontRef(CFDictionaryGetValue(CTRunGetAttributes(Run),
          kCTFontAttributeName));
        N := CTRunGetGlyphCount(Run);
        if (N < 1) or (RunFont = nil) then
          Continue;
        SetLength(Glyphs, N);
        SetLength(Positions, N);
        CTRunGetGlyphs(Run, CFRangeMake(0, 0), @Glyphs[0]);
        CTRunGetPositions(Run, CFRangeMake(0, 0), @Positions[0]);
        for K := 0 to N - 1 do
        begin
          T := CGAffineTransformMake(1, 0, 0, -1, X + Positions[K].x,
            Baseline - Positions[K].y);
          GlyphPath := CTFontCreatePathForGlyph(RunFont, Glyphs[K], @T);
          if GlyphPath <> nil then
          begin
            CGContextAddPath(FContext, GlyphPath);
            CGPathRelease(GlyphPath);
          end;
        end;
      end;
    end;
    Y := Y + Layout.Lines[I].Height;
  end;
  if Draw then
    CGContextRestoreGState(FContext);
  FNewSubPath := False;
  LayoutFree(Layout);
end;

procedure TSurfaceQuartz.Stroke(Pen: IPen; Preserve: Boolean = False);
var
  P: CGPathRef;
  C: TColorB;
begin
  if not HandleAvailable then
    Exit;
  FNewSubPath := False;
  if PathEmpty then
    Exit;
  P := nil;
  if Preserve then
    P := CGContextCopyPath(FContext);
  ApplyPen(Pen);
  if Pen.Brush = nil then
  begin
    C := Pen.Color;
    CGContextSetRGBStrokeColor(FContext, C.Red / $FF, C.Green / $FF, C.Blue / $FF,
      C.Alpha / $FF);
    CGContextStrokePath(FContext);
  end
  else
  begin
    { A brush is stroked by filling the outline of the stroke }
    CGContextReplacePathWithStrokedPath(FContext);
    FillPath(Pen.Brush);
  end;
  if P <> nil then
  begin
    CGContextAddPath(FContext, P);
    CGPathRelease(P);
  end;
end;

procedure TSurfaceQuartz.Fill(Brush: IBrush; Preserve: Boolean = False);
var
  P: CGPathRef;
begin
  if not HandleAvailable then
    Exit;
  FNewSubPath := False;
  if PathEmpty then
    Exit;
  P := nil;
  if Preserve then
    P := CGContextCopyPath(FContext);
  FillPath(Brush);
  if P <> nil then
  begin
    CGContextAddPath(FContext, P);
    CGPathRelease(P);
  end;
end;

procedure TSurfaceQuartz.StrokeRect(Pen: IPen; const Rect: TRectF);
var
  Odd: Boolean;
  R: TRectI;
begin
  if not HandleAvailable then
    Exit;
  if Rect.Empty then
    Exit;
  Path.Remove;
  Odd := Round(Pen.Width) mod 2 = 1;
  if Odd then
    Matrix.Translate(PixelDelta, PixelDelta);
  R := TRectI(Rect);
  Dec(R.Width);
  Dec(R.Height);
  Rectangle(R);
  Stroke(Pen);
  if Odd then
    Matrix.Translate(-PixelDelta, -PixelDelta);
end;

procedure TSurfaceQuartz.FillRect(Brush: IBrush; const Rect: TRectF);
var
  R: TRectI;
begin
  if not HandleAvailable then
    Exit;
  if Rect.Empty then
    Exit;
  Path.Remove;
  R := TRectI(Rect);
  Rectangle(R);
  Fill(Brush);
end;

procedure TSurfaceQuartz.StrokeRoundRect(Pen: IPen; const Rect: TRectF; Radius: Float);
var
  Odd: Boolean;
  R: TRectI;
begin
  if not HandleAvailable then
    Exit;
  if Rect.Empty then
    Exit;
  Path.Remove;
  Odd := Round(Pen.Width) mod 2 = 1;
  if Odd then
    Matrix.Translate(PixelDelta, PixelDelta);
  R := TRectI(Rect);
  Dec(R.Width);
  Dec(R.Height);
  RoundRectangle(R, Radius);
  Stroke(Pen);
  if Odd then
    Matrix.Translate(-PixelDelta, -PixelDelta);
end;

procedure TSurfaceQuartz.FillRoundRect(Brush: IBrush; const Rect: TRectF; Radius: Float);
var
  R: TRectI;
begin
  if not HandleAvailable then
    Exit;
  if Rect.Empty then
    Exit;
  Path.Remove;
  R := TRectI(Rect);
  RoundRectangle(R, Radius);
  Fill(Brush);
end;

{ TBitmapSurfaceQuartz }

constructor TBitmapSurfaceQuartz.Create(Bitmap: TBitmapQuartz);
begin
  inherited Create;
  FBitmap := Bitmap;
end;

function TBitmapSurfaceQuartz.HandleAvailable: Boolean;
var
  C: CGContextRef;
begin
  if FContext = nil then
    if (FBitmap <> nil) and (not FBitmap.GetEmpty) then
    begin
      C := RawContext(FBitmap.FPixels, FBitmap.FWidth, FBitmap.FHeight);
      if C <> nil then
      begin
        { Move the origin from the bottom left to the top left }
        CGContextTranslateCTM(C, 0, FBitmap.FHeight);
        CGContextScaleCTM(C, 1, -1);
        Attach(C, True);
      end;
    end;
  Result := inherited HandleAvailable;
end;

{ TBitmapQuartz }

const
  { The uniform type identifiers of the image formats }
  ImageTypes: array[TImageFormat] of PChar = ('public.png', 'public.jpeg',
    'com.compuserve.gif', 'com.microsoft.bmp', 'com.microsoft.ico', 'public.tiff');

destructor TBitmapQuartz.Destroy;
begin
  if FSurfaceQuartz <> nil then
  begin
    FSurfaceQuartz.HandleRelease;
    FSurfaceQuartz.FBitmap := nil;
  end;
  if FPixels <> nil then
    FreeMem(FPixels);
  inherited Destroy;
end;

procedure TBitmapQuartz.Flush;
begin
  if FSurface <> nil then
    FSurface.Flush;
end;

function TBitmapQuartz.ImageCreate: CGImageRef;
var
  C: CGContextRef;
begin
  Result := nil;
  if GetEmpty then
    Exit;
  Flush;
  C := RawContext(FPixels, FWidth, FHeight);
  if C = nil then
    Exit;
  Result := CGBitmapContextCreateImage(C);
  CGContextRelease(C);
end;

procedure TBitmapQuartz.ImageLoad(Image: CGImageRef; Width, Height: Integer;
  Quality: CGInterpolationQuality);
var
  C: CGContextRef;
begin
  SetSize(Width, Height);
  if GetEmpty then
    Exit;
  Flush;
  C := RawContext(FPixels, FWidth, FHeight);
  if C = nil then
    Exit;
  CGContextSetBlendMode(C, kCGBlendModeCopy);
  CGContextSetInterpolationQuality(C, Quality);
  CGContextDrawImage(C, CGRectMake(0, 0, FWidth, FHeight), Image);
  CGContextRelease(C);
end;

function TBitmapQuartz.Clone: IBitmap;
var
  B: TBitmapQuartz;
begin
  B := TBitmapQuartz.Create;
  Result := B;
  B.FFormat := FFormat;
  if GetEmpty then
    Exit;
  Flush;
  B.SetSize(FWidth, FHeight);
  System.Move(FPixels^, B.FPixels^, FWidth * FHeight * SizeOf(TColorB));
end;

function TBitmapQuartz.GetEmpty: Boolean;
begin
  Result := FPixels = nil;
end;

function TBitmapQuartz.GetSurface: ISurface;
begin
  if FSurface = nil then
  begin
    FSurfaceQuartz := TBitmapSurfaceQuartz.Create(Self);
    FSurface := FSurfaceQuartz;
  end;
  Result := FSurface;
end;

function TBitmapQuartz.GetClientRect: TRectI;
begin
  Result := TRectI.Create(FWidth, FHeight);
end;

function TBitmapQuartz.GetFormat: TImageFormat;
begin
  Result := FFormat;
end;

procedure TBitmapQuartz.SetFormat(Value: TImageFormat);
begin
  FFormat := Value;
end;

function TBitmapQuartz.GetHeight: Integer;
begin
  Result := FHeight;
end;

function TBitmapQuartz.GetWidth: Integer;
begin
  Result := FWidth;
end;

function TBitmapQuartz.GetPixels: Pointer;
begin
  Flush;
  Result := FPixels;
end;

procedure TBitmapQuartz.Clear;
begin
  SetSize(0, 0);
end;

procedure TBitmapQuartz.SetSize(Width, Height: Integer);
begin
  if (Width < 1) or (Height < 1) then
  begin
    Width := 0;
    Height := 0;
  end;
  if (Width = FWidth) and (Height = FHeight) then
    Exit;
  if FSurfaceQuartz <> nil then
    FSurfaceQuartz.HandleRelease;
  if FPixels <> nil then
    FreeMem(FPixels);
  FPixels := nil;
  FWidth := Width;
  FHeight := Height;
  { New pixels are transparent }
  if Width > 0 then
    FPixels := AllocMem(Width * Height * SizeOf(TColorB));
end;

function TBitmapQuartz.Resample(Width, Height: Integer; Quality: TResampleQuality = rqNormal): IBitmap;
const
  Sampling: array[TResampleQuality] of CGInterpolationQuality =
    (kCGInterpolationNone, kCGInterpolationMedium, kCGInterpolationHigh);
var
  Image: CGImageRef;
  B: TBitmapQuartz;
begin
  Result := nil;
  if GetEmpty or (Width < 1) or (Height < 1) then
    Exit;
  Image := ImageCreate;
  if Image = nil then
    Exit;
  B := TBitmapQuartz.Create;
  Result := B;
  B.FFormat := FFormat;
  B.ImageLoad(Image, Width, Height, Sampling[Quality]);
  CGImageRelease(Image);
end;

procedure TBitmapQuartz.LoadFromFile(const FileName: string);
var
  Stream: TStream;
begin
  if not FileExists(FileName) then
    Exit;
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TBitmapQuartz.LoadFromStream(Stream: TStream);
var
  Buffer: Pointer;
  Size: Int64;
  Data: CFDataRef;
  Source: CGImageSourceRef;
  Image: CGImageRef;
  Kind: CFStringRef;
  F: TImageFormat;
begin
  Size := Stream.Size - Stream.Position;
  if Size < 1 then
    Exit;
  Buffer := GetMem(Size);
  try
    Stream.ReadBuffer(Buffer^, Size);
    Data := CFDataCreate(nil, Buffer, Size);
  finally
    FreeMem(Buffer);
  end;
  if Data = nil then
    Exit;
  Source := CGImageSourceCreateWithData(Data, nil);
  if Source <> nil then
  begin
    Image := CGImageSourceCreateImageAtIndex(Source, 0, nil);
    if Image <> nil then
    begin
      Kind := CGImageSourceGetType(Source);
      if Kind <> nil then
        for F := Low(TImageFormat) to High(TImageFormat) do
          if CFStringCompare(Kind, CFSTR(ImageTypes[F]), 0) = kCFCompareEqualTo then
            FFormat := F;
      ImageLoad(Image, CGImageGetWidth(Image), CGImageGetHeight(Image),
        kCGInterpolationNone);
      CGImageRelease(Image);
    end;
    CFRelease(Source);
  end;
  CFRelease(Data);
end;

procedure TBitmapQuartz.Save(Stream: TStream; Format: TImageFormat);
var
  Image: CGImageRef;
  Data: CFMutableDataRef;
  Dest: CGImageDestinationRef;
begin
  Image := ImageCreate;
  if Image = nil then
    Exit;
  Data := CFDataCreateMutable(nil, 0);
  Dest := CGImageDestinationCreateWithData(Data, CFSTR(ImageTypes[Format]), 1, nil);
  if Dest <> nil then
  begin
    CGImageDestinationAddImage(Dest, Image, nil);
    if CGImageDestinationFinalize(Dest) <> 0 then
      Stream.WriteBuffer(CFDataGetBytePtr(CFDataRef(Data))^,
        CFDataGetLength(CFDataRef(Data)));
    CFRelease(Dest);
  end;
  CFRelease(Data);
  CGImageRelease(Image);
end;

procedure TBitmapQuartz.SaveToFile(const FileName: string);
var
  Stream: TStream;
begin
  if GetEmpty then
    Exit;
  FFormat := StrToImageFormat(ExtractFileExt(FileName));
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    Save(Stream, FFormat);
  finally
    Stream.Free;
  end;
end;

procedure TBitmapQuartz.SaveToStream(Stream: TStream);
begin
  if GetEmpty then
    Exit;
  if not (FFormat in [fmBmp, fmJpeg, fmPng, fmTiff]) then
    FFormat := fmPng;
  Save(Stream, FFormat);
end;

{ TSplashQuartz is a window without a border which shows a bitmap, is see
  through wherever the bitmap is, and lets mouse clicks pass through it }

type
  TSplashQuartz = class(TInterfacedObject, ISplash)
  private
    FBitmap: IBitmap;
    { The window and its image view are kept as untyped objects and cast where
      they are used, because the compiler fails when it writes dwarf 3 debug
      information for anything declared as an Objective-C class }
    FWindow: id;
    FView: id;
    FOpacity: Byte;
    FVisible: Boolean;
    FX: Integer;
    FY: Integer;
    procedure Refresh;
  public
    constructor Create;
    destructor Destroy; override;
    function GetBitmap: IBitmap;
    function GetOpacity: Byte;
    procedure SetOpacity(Value: Byte);
    function GetVisible: Boolean;
    procedure SetVisible(Value: Boolean);
    function GetHandle: IntPtr;
    procedure Move(X, Y: Integer);
    procedure Update;
  end;

constructor TSplashQuartz.Create;
var
  R: NSRect;
begin
  inherited Create;
  FBitmap := TBitmapQuartz.Create;
  FOpacity := $FF;
  R := NSMakeRect(0, 0, 1, 1);
  FWindow := NSWindow.alloc.initWithContentRect_styleMask_backing_defer(R,
    NSBorderlessWindowMask, NSBackingStoreBuffered, False);
  NSWindow(FWindow).setReleasedWhenClosed(False);
  NSWindow(FWindow).setOpaque(False);
  NSWindow(FWindow).setBackgroundColor(NSColor.clearColor);
  NSWindow(FWindow).setHasShadow(False);
  NSWindow(FWindow).setIgnoresMouseEvents(True);
  { The window level constants in the Cocoa units are placeholders, so the
    level is asked for instead. The status level is above normal windows and
    the dock, which keeps the splash on top as it is on other platforms. }
  NSWindow(FWindow).setLevel(CGWindowLevelForKey(kCGStatusWindowLevelKey));
  FView := NSImageView.alloc.initWithFrame(R);
  NSImageView(FView).setImageScaling(NSImageScaleNone);
  NSWindow(FWindow).setContentView(FView);
end;

destructor TSplashQuartz.Destroy;
begin
  NSWindow(FWindow).orderOut(nil);
  NSWindow(FWindow).setContentView(nil);
  NSImageView(FView).release;
  NSWindow(FWindow).release;
  inherited Destroy;
end;

{ Show the bitmap in the window and place the window. The LCL measures from
  the top left of the main screen and Cocoa from its bottom left. }

procedure TSplashQuartz.Refresh;
var
  Image: CGImageRef;
  Picture: id;
  W, H: Integer;
  Top: Double;
begin
  if FBitmap.Empty then
    Exit;
  W := FBitmap.Width;
  H := FBitmap.Height;
  Image := (FBitmap as TBitmapQuartz).ImageCreate;
  if Image <> nil then
  begin
    Picture := NSImage.alloc.initWithCGImage_size(Image, NSMakeSize(W, H));
    NSImageView(FView).setImage(NSImage(Picture));
    NSImage(Picture).release;
    CGImageRelease(Image);
  end;
  Top := 0;
  if NSScreen.screens.count > 0 then
    Top := NSScreen(NSScreen.screens.objectAtIndex(0)).frame.size.height;
  NSWindow(FWindow).setFrame_display(NSMakeRect(FX, Top - FY - H, W, H), True);
end;

function TSplashQuartz.GetBitmap: IBitmap;
begin
  Result := FBitmap;
end;

function TSplashQuartz.GetOpacity: Byte;
begin
  Result := FOpacity;
end;

procedure TSplashQuartz.SetOpacity(Value: Byte);
begin
  if Value <> FOpacity then
  begin
    FOpacity := Value;
    NSWindow(FWindow).setAlphaValue(Value / $FF);
  end;
end;

function TSplashQuartz.GetVisible: Boolean;
begin
  Result := FVisible;
end;

procedure TSplashQuartz.SetVisible(Value: Boolean);
begin
  Value := Value and (not FBitmap.Empty);
  if Value <> FVisible then
  begin
    FVisible := Value;
    if FVisible then
    begin
      Refresh;
      NSWindow(FWindow).orderFrontRegardless;
    end
    else
      NSWindow(FWindow).orderOut(nil);
  end;
end;

function TSplashQuartz.GetHandle: IntPtr;
begin
  Result := IntPtr(Pointer(FWindow));
end;

procedure TSplashQuartz.Move(X, Y: Integer);
begin
  FX := X;
  FY := Y;
  if FVisible then
    Refresh;
end;

procedure TSplashQuartz.Update;
begin
  if FBitmap.Empty then
    SetVisible(False)
  else if FVisible then
    Refresh;
end;

{ Surface object creation routines }

function NewMatrixQuartz: IMatrix;
begin
  Result := TMatrixQuartz.Create;
end;

function NewPenQuartz(Brush: IBrush; Width: Float = 1): IPen;
begin
  Result := TPenQuartz.Create(Brush, Width);
end;

function NewPenQuartz(Color: TColorB; Width: Float = 1): IPen;
begin
  Result := TPenQuartz.Create(Color, Width);
end;

function NewSolidBrushQuartz(Color: TColorB): ISolidBrush;
begin
  Result := TSolidBrushQuartz.Create(Color);
end;

function NewBitmapBrushQuartz(Bitmap: IBitmap): IBitmapBrush;
begin
  Result := TBitmapBrushQuartz.Create(Bitmap);
end;

function NewLinearGradientBrushQuartz(X1, Y1, X2, Y2: Float): ILinearGradientBrush;
begin
  Result := TLinearGradientBrushQuartz.Create(X1, Y1, X2, Y2);
end;

function NewLinearGradientBrushQuartz(const A, B: TPointF): ILinearGradientBrush;
begin
  Result := TLinearGradientBrushQuartz.Create(A.X, A.Y, B.X, B.Y);
end;

function NewRadialGradientBrushQuartz(const Rect: TRectF): IRadialGradientBrush;
begin
  Result := TRadialGradientBrushQuartz.Create(Rect);
end;

function NewFontQuartz(const FontName: string; FontSize: Integer): IFont;
begin
  Result := TFontQuartz.Create(FontName, FontSize);
end;

function NewFontQuartz(Font: TFont = nil): IFont;
begin
  Result := TFontQuartz.Create(Font);
end;

{ The Cocoa widgetset gives a canvas a context with the origin at the top left
  and already moved to the origin of the control being painted, so the
  transform of the context is used as it is found }

function NewSurfaceQuartz(Canvas: TCanvas): ISurface;
var
  Obj: TObject;
  Context: CGContextRef;
  S: TSurfaceQuartz;
begin
  Result := nil;
  if Canvas = nil then
    Exit;
  Obj := TObject(Canvas.Handle);
  if Obj is TCocoaContext then
  begin
    Context := TCocoaContext(Obj).CGContext;
    if Context = nil then
      Exit;
    S := TSurfaceQuartz.Create;
    Result := S;
    S.Attach(Context, False);
  end;
end;

function NewSurfaceQuartz(Control: TWinControl): ISurface;
begin
  Result := nil;
  if Control is TCustomControl then
    Result := NewSurfaceQuartz(TCustomControl(Control).Canvas);
end;

function NewBitmapQuartz(Width, Height: Integer): IBitmap;
begin
  Result := TBitmapQuartz.Create;
  Result.SetSize(Width, Height);
end;

function NewSplashQuartz: ISplash;
begin
  Result := TSplashQuartz.Create;
end;

{ The function which captures a display is found when the program runs,
  because newer versions of macOS are removing it }

type
  TDisplayCreateImage = function(Display: CGDirectDisplayID): CGImageRef; cdecl;

function NewScreenCaptureQuartz: IBitmap;
const
  CoreGraphics = '/System/Library/Frameworks/CoreGraphics.framework/CoreGraphics';
var
  Module: TLibHandle;
  DisplayCreateImage: TDisplayCreateImage;
  Image: CGImageRef;
  B: TBitmapQuartz;
begin
  B := TBitmapQuartz.Create;
  Result := B;
  Module := DynLibs.LoadLibrary(CoreGraphics);
  if Module = NilHandle then
    Exit;
  DisplayCreateImage := TDisplayCreateImage(DynLibs.GetProcAddress(Module, 'CGDisplayCreateImage'));
  if not Assigned(DisplayCreateImage) then
    Exit;
  Image := DisplayCreateImage(CGMainDisplayID);
  if Image = nil then
    Exit;
  B.ImageLoad(Image, CGImageGetWidth(Image), CGImageGetHeight(Image),
    kCGInterpolationNone);
  CGImageRelease(Image);
end;
{$endif}

end.
