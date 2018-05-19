(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modied September 2013                               *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.graphics.windows.surfacedirect2d.txt> }
unit Codebot.Graphics.Windows.SurfaceD2D;

{$i codebot.inc}

interface

{$ifdef windows}
uses
  SysUtils, Classes, Graphics, Controls, ActiveX, Windows,
  Codebot.System,
  Codebot.Collections,
  Codebot.Graphics.Types,
  Codebot.Graphics.Windows.InterfacedBitmap,
  Codebot.Interop.Windows.Direct2D;

{ New object routines }
function LoadD2D: Boolean;

function NewMatrixD2D: IMatrix;
function NewPenD2D(Brush: IBrush; Width: Float = 1): IPen; overload;
function NewPenD2D(Color: TColorB; Width: Float = 1): IPen; overload;
function NewSolidBrushD2D(Color: TColorB): ISolidBrush;
function NewBitmapBrushD2D(Bitmap: IBitmap): IBitmapBrush;
function NewLinearGradientBrushD2D(X1, Y1, X2, Y2: Float): ILinearGradientBrush; overload;
function NewLinearGradientBrushD2D(const A, B: TPointF): ILinearGradientBrush; overload;
function NewRadialGradientBrushD2D(const Rect: TRectF): IRadialGradientBrush;
function NewFontD2D(Font: TFont): IFont;
function NewSurfaceD2D(Canvas: TCanvas): ISurface; overload;
function NewSurfaceD2D(Control: TWinControl): ISurface; overload;
function NewBitmapD2D(Width, Height: Integer): IBitmap;
{$endif}

implementation

{$ifdef windows}
function LoadD2D: Boolean;
begin
  if SurfaceOptions.HardwareRendering then
    Result := Direct2DInit
  else
    Result := False;
end;

{ Implementation types }

type
  TPenD2D = class;
  TBrushD2D = class;
  TPathD2D = class;
  TSurfacePathD2D = class;
  TSurfaceD2D = class;
  TBitmapD2D = class;

  TMatrixD2D = class(TInterfacedObject, IMatrix)
  private
    FMatrix: TD2DMatrix3X2F;
    FChanged: Boolean;
    FIsIdentity: Boolean;
  public
    constructor Create(const M: TD2DMatrix3X2F);
    function Clone: IMatrix;
    procedure Identity;
    procedure Multiply(M: IMatrix);
    procedure Translate(X, Y: Float);
    procedure Scale(X, Y: Float);
    procedure Rotate(Angle: Float);
    function Transform(Point: TPointF): TPointF;
  end;

{ TPenD2D }

  TPenD2D = class(TInterfacedObject, IPen)
  private
    FTarget: ID2D1RenderTarget;
    FID: Integer;
    FFill: ID2D1Brush;
    FStyle: ID2D1StrokeStyle;
    FBrush: IBrush;
    FColor: TColorB;
    FWidth: Float;
    FLinePattern: TLinePattern;
    FLinePatternOffset: Float;
    FLineCap: TLineCap;
    FLineJoin: TLineJoin;
    FMiterLimit: Float;
  protected
    function Acquire(Target: ID2D1RenderTarget; ID: Integer): Boolean;
    function CurrentBrush: ID2D1Brush;
    function HandleAvailable: Boolean;
  public
    constructor Create(B: IBrush; W: Float); overload;
    constructor Create(const C: TColorB; W: Float); overload;
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
    property Brush: IBrush read GetBrush write SetBrush;
    property Color: TColorB read GetColor write SetColor;
    property Width: Float read GetWidth write SetWidth;
    property LinePattern: TLinePattern read GetLinePattern write SetLinePattern;
    property LinePatternOffset: Float read GetLinePatternOffset write SetLinePatternOffset;
    property LineCap: TLineCap read GetLineCap write SetLineCap;
    property LineJoin: TLineJoin read GetLineJoin write SetLineJoin;
    property MiterLimit: Float read GetMiterLimit write SetMiterLimit;
  end;

{ TBrushD2D }

  TBrushD2D = class(TInterfacedObject, IBrush)
  private
    FTarget: ID2D1RenderTarget;
    FID: Integer;
    FBrush: ID2D1Brush;
    FMatrix: IMatrix;
    FOpacity: Byte;
  protected
    function Acquire(Target: ID2D1RenderTarget; ID: Integer): Boolean;
    function HandleAvailable: Boolean; virtual;
  public
    constructor Create;
    function GetMatrix: IMatrix;
    procedure SetMatrix(Value: IMatrix);
    function GetOpacity: Byte;
    procedure SetOpacity(Value: Byte);
    property Matrix: IMatrix read GetMatrix write SetMatrix;
    property Opacity: Byte read GetOpacity write SetOpacity;
  end;

{ TSolidBrushD2D }

  TSolidBrushD2D = class(TBrushD2D, ISolidBrush)
  private
    FColor: TColorB;
  protected
    function HandleAvailable: Boolean; override;
  public
    constructor Create(C: TColorB);
    function GetColor: TColorB;
    procedure SetColor(Value: TColorB);
    property Color: TColorB read GetColor write SetColor;
  end;

{ TGradientBrushD2D }

  TGradientStops = TArrayList<TD2D1GradientStop>;

  TGradientBrushD2D = class(TBrushD2D, IGradientBrush)
  private
    FCreated: Boolean;
    FMidPoint: TPointF;
    FWrap: TGradientWrap;
    FStops: TGradientStops;
  protected
    function HandleAvailable: Boolean; override;
  public
    constructor Create(MidPoint: TPointF);
    function GetWrap: TGradientWrap;
    procedure SetWrap(Value: TGradientWrap);
    procedure AddStop(Color: TColorB; Offset: Float);
    property Wrap: TGradientWrap read GetWrap write SetWrap;
  end;

{ TLinearGradientBrushD2D }

  TLinearGradientBrushD2D = class(TGradientBrushD2D, ILinearGradientBrush)
  private
    FA: TPointF;
    FB: TPointF;
  protected
    function HandleAvailable: Boolean; override;
  public
    constructor Create(const A, B: TPointF);
  end;

{ TRadialGradientBrushD2D }

  TRadialGradientBrushD2D = class(TGradientBrushD2D, IRadialGradientBrush)
  private
    FRect: TRectF;
  protected
    function HandleAvailable: Boolean; override;
  public
    constructor Create(const R: TRectF);
  end;

{ TBitmapBrushD2D }

  TBitmapBrushD2D = class(TBrushD2D, IBitmapBrush)
  private
    FBitmap: IBitmap;
  protected
    function HandleAvailable: Boolean; override;
  public
    constructor Create(B: IBitmap);
  end;

{ TFontD2D }

  TFontD2D = class(TInterfacedObject, IFont)
  private
    FFormat: IDWriteTextFormat;
    FName: string;
    FColor: TColorB;
    FQuality: TFontQuality;
    FStyle: TFontStyles;
    FSize: Float;
    function Format: IDWriteTextFormat;
  public
    constructor Create(F: TFont);
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
    property Name: string read GetName;
    property Color: TColorB read GetColor write SetColor;
    property Quality: TFontQuality read GetQuality write SetQuality;
    property Style: TFontStyles read GetStyle write SetStyle;
    property Size: Float read GetSize write SetSize;
  end;

{ TPathD2D }

  TPathD2D = class(TInterfacedObject, IPathData)
  private
    FPath: ID2D1Geometry;
  public
    constructor Create(Path: ID2D1Geometry);
  end;

{ TSurfacePathD2D }

  TSurfacePathD2D = class(TInterfacedObject, IPath)
  private
    FSurface: TSurfaceD2D;
    FClipStack: IList<ID2D1Geometry>;
    FClipHeight: Integer;
    FData: IList<ID2D1Geometry>;
    FFigure: ID2D1PathGeometry;
    FFigureSink: ID2D1GeometrySink;
    FFigureOrigin: TD2DPoint2f;
    FFigureOpened: Boolean;
    function ClipStack: IList<ID2D1Geometry>;
  protected
    function HandleAvailable: Boolean;
    procedure Open; overload;
    procedure Open(X, Y: Float); overload;
    procedure SaveClipStack;
    procedure RestoreClipStack;
  public
    constructor Create(C: TSurfaceD2D);
    function Clone: IPathData;
    procedure Add; overload;
    procedure Add(G: ID2D1Geometry); overload;
    procedure Remove;
    procedure Close;
    procedure Join(Path: IPathData);
    procedure Clip;
    procedure Unclip;
  end;

{ TSurfaceStateD2D }

  TSurfaceStateD2D = class
  public
    ClipStack: IList<ID2D1Geometry>;
    Data: IList<ID2D1Geometry>;
    Matrix: IMatrix;
    constructor Create(C: TSurfaceD2D);
    procedure Restore(C: TSurfaceD2D);
  end;

{ TSurfaceD2D }

  TSurfaceD2D = class(TInterfacedObject, ISurface)
  private
    FTarget: ID2D1RenderTarget;
    FID: Integer;
    FPath: IPath;
    FMatrix: IMatrix;
    FStateStack: IList<TSurfaceStateD2D>;
    FDrawing: Boolean;
    function Path: TSurfacePathD2D;
    function Matrix: TMatrixD2D;
    procedure Draw;
  protected
    procedure AcquireTarget(Target: ID2D1RenderTarget);
    function AcquireBrush(Brush: IBrush; out B: ID2D1Brush): Boolean;
    function AcquirePen(Pen: IPen; out B: ID2D1Brush; out S: ID2D1StrokeStyle): Boolean;
    function HandleAvailable: Boolean; virtual;
    procedure HandleRelease; virtual;
  public
    constructor Create(T: ID2D1RenderTarget);
    destructor Destroy; override;
    procedure ShareRelease; virtual;
    function GetMatrix: IMatrix;
    procedure SetMatrix(Value: IMatrix);
    function GetPath: IPath;
    function GetHandle: Pointer;
    procedure Flush; virtual;
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
    procedure FillOrStroke(Brush: IBrush; Pen: IPen; Preserve: Boolean);
    procedure Stroke(Pen: IPen; Preserve: Boolean = False);
    procedure Fill(Brush: IBrush; Preserve: Boolean = False);
    procedure StrokeRect(Pen: IPen; const Rect: TRectF);
    procedure FillRect(Brush: IBrush; const Rect: TRectF);
    procedure StrokeRoundRect(Pen: IPen; const Rect: TRectF; Radius: Float);
    procedure FillRoundRect(Brush: IBrush; const Rect: TRectF; Radius: Float);
  end;

{ ISharedBitmapSurface }

  ISharedBitmapTarget = interface
    ['{0959D148-21A8-4A20-9EAB-2503BB6C3A9F}']
    function ShareCreate(Target: ID2D1RenderTarget): ID2D1Bitmap;
    procedure ShareRelease;
  end;

{ TBitmapSurfaceD2D }

  TBitmapSurfaceD2D = class(TSurfaceD2D, ISharedBitmapTarget)
  private
    FBitmap: TBitmapD2D;
    FSurfaceBitmap: ID2D1Bitmap;
    FSharedTarget: ID2D1RenderTarget;
    FSharedBitmap: ID2D1Bitmap;
  protected
    function HandleAvailable: Boolean; override;
    procedure HandleRelease; override;
  public
    constructor Create(B: TBitmapD2D);
    function ShareCreate(Target: ID2D1RenderTarget): ID2D1Bitmap;
    procedure ShareRelease; override;
  end;

{ TWndSurfaceD2D }

  TWndSurfaceD2D = class(TSurfaceD2D)
  private
    FWnd: HWND;
    FRect: TRect;
    FFlushed: Boolean;
  protected
    function HandleAvailable: Boolean; override;
  public
    constructor Create(Wnd: HWND);
    procedure Flush; override;
  end;

{ TBitmapD2D }

  TBitmapD2D = class(TInterfacedBitmap)
  protected
    procedure HandleRelease; override;
  public
    destructor Destroy; override;
    procedure Clear; override;
    function GetSurface: ISurface; override;
    function GetPixels: PPixel; override;
  end;

{ Convert routines }

function Convert(const Value: TPointF): TD2D1Point2F; overload; inline;
begin
  Result.X := Value.X;
  Result.Y := Value.Y;
end;

function Convert(const Value: TD2D1Point2F): TPointF; overload; inline;
begin
  Result.X := Value.X;
  Result.Y := Value.Y;
end;

function Convert(const Value: TRectF): TD2D1RectF; overload; inline;
begin
  Result.Left := Value.Left;
  Result.Top := Value.Top;
  Result.Right := Value.Right;
  Result.Bottom := Value.Bottom;
end;

function Convert(const Value: TD2D1RectF): TRectF; overload; inline;
begin
  Result.Left := Value.Left;
  Result.Top := Value.Top;
  Result.Right := Value.Right;
  Result.Bottom := Value.Bottom;
end;

function Convert(const Value: TRectI): TD2D1RectU; overload; inline;
begin
  Result.Left := Value.Left;
  Result.Top := Value.Top;
  Result.Right := Value.Right;
  Result.Bottom := Value.Bottom;
end;

function Convert(const Value: TColorB): TD3DColorValue; overload; inline;
begin
  Result.B := Value.Blue / $FF;
  Result.G := Value.Green / $FF;
  Result.R := Value.Red / $FF;
  Result.A := Value.Alpha / $FF;
end;

function Convert(const Value: TD3DColorValue): TColorB; overload; inline;
begin
  Result.Blue := Round(Value.B * $FF);
  Result.Green := Round(Value.G * $FF);
  Result.Red := Round(Value.R * $FF);
  Result.Alpha := Round(Value.A * $FF);
end;

{ Matrix math }

const
  MatrixIdentity: TD2DMatrix3X2F = (
    m11: 1; m12: 0; m21: 0; m22: 1; m31: 0; m32: 0);

function MatrixMultiply(const A, B: TD2DMatrix3X2F): TD2DMatrix3X2F;
begin
  with Result do
  begin
    m11 := A.m11 * B.m11 + A.m12 * B.m21;
    m12 := A.m11 * B.m12 + A.m12 * B.m22;
    m21 := A.m21 * B.m11 + A.m22 * B.m21;
    m22 := A.m21 * B.m12 + A.m22 * B.m22;
    m31 := A.m31 * B.m11 + A.m32 * B.m21 + B.m31;
    m32 := A.m31 * B.m12 + A.m32 * B.m22 + B.m32;
  end;
end;

procedure MatrixTranslate(var M: TD2DMatrix3X2F; X, Y: Float);
var
  T: TD2DMatrix3X2F;
begin
  with T do
  begin
    m11 := 1;
    m12 := 0;
    m21 := 0;
    m22 := 1;
    m31 := X;
    m32 := Y;
  end;
  M := MatrixMultiply(M, T);
end;

procedure MatrixRotate(var M: TD2DMatrix3X2F; A: Single);
var
  C: TD2D1Point2F;
  R: TD2DMatrix3X2F;
begin
  C.x := 0;
  C.y := 0;
  D2D1MakeRotateMatrix(A / Pi * 180, C, @R);
  M := MatrixMultiply(M, R);
end;

procedure MatrixScale(var M: TD2DMatrix3X2F; X, Y: Float);
var
  S: TD2DMatrix3X2F;
begin
  with S do
  begin
    m11 := X;
    m12 := 0;
    m21 := 0;
    m22 := Y;
    m31 := 0;
    m32 := 0;
  end;
  M := MatrixMultiply(M, S);
end;

{ Direct2D helper routines }

var
  RenderFactoryInstance: ID2D1Factory;
  WriteFactoryInstance: IDWriteFactory;

function RenderFactory: ID2D1Factory;
begin
  if RenderFactoryInstance = nil then
    D2D1CreateFactory(D2D1_FACTORY_TYPE_SINGLE_THREADED, IID_ID2D1Factory, nil,
      RenderFactoryInstance);
  Result := RenderFactoryInstance;
end;

function WriteFactory: IDWriteFactory;
begin
  if WriteFactoryInstance = nil then
    DWriteCreateFactory(DWRITE_FACTORY_TYPE_SHARED, IDWriteFactory,
      WriteFactoryInstance);
  Result := WriteFactoryInstance;
end;

{ RenderFactory object creation routines }

function DefaultPixelFormat: TD2D1PixelFormat;
begin
  Result.format := DXGI_FORMAT_B8G8R8A8_UNORM;
  Result.alphaMode := D2D1_ALPHA_MODE_PREMULTIPLIED;
end;

function DefaultRenderTargetProperties: TD2D1RenderTargetProperties;
begin
  Result._type := D2D1_RENDER_TARGET_TYPE_DEFAULT;
  Result.pixelFormat := DefaultPixelFormat;
  Result.dpiX := 0;
  Result.dpiY := 0;
  { TODO: Review performance of D2D1_RENDER_TARGET_USAGE_GDI_COMPATIBLE }
  Result.usage := D2D1_RENDER_TARGET_USAGE_GDI_COMPATIBLE;
  Result.minLevel := D2D1_FEATURE_LEVEL_DEFAULT;
end;

function CreateDCTarget: ID2D1DCRenderTarget;
var
  Prop: TD2D1RenderTargetProperties;
begin
  Prop := DefaultRenderTargetProperties;
  RenderFactory.CreateDCRenderTarget(Prop, Result);
end;

function CreateWndTarget(Wnd: HWND; out Rect: TRect): ID2D1HwndRenderTarget;
const
  EmptyRect: TRect = ();
var
  Prop: TD2D1HwndRenderTargetProperties;
begin
  Rect := EmptyRect;
  Result := nil;
  if not IsWindow(Wnd) then
    Exit;
  GetClientRect(Wnd, Rect);
  Rect.Right := Rect.Right;
  Rect.Bottom := Rect.Bottom;
  Prop.hwnd := Wnd;
  Prop.pixelSize.width := Rect.Right - Rect.Left;
  Prop.pixelSize.height := Rect.Bottom - Rect.Top;
  Prop.presentOptions := D2D1_PRESENT_OPTIONS_NONE;
  RenderFactory.CreateHwndRenderTarget(DefaultRenderTargetProperties, Prop, Result);
end;

function CreateBitmap(Target: ID2D1RenderTarget; Width, Height: Integer; Bits: Pointer = nil): ID2D1Bitmap;
var
  Size: TD2DSizeU;
  Stride: Cardinal;
  Prop: TD2D1BitmapProperties;
begin
  Result:= nil;
  if (Width < 1) or (Height < 1) then
    Exit;
  Size.width := Width;
  Size.height := Height;
  Stride := Width * PixelSize;
  Prop.pixelFormat := DefaultPixelFormat;
  Prop.dpiX := 0;
  Prop.dpiY := 0;
  Target.CreateBitmap(Size, Bits, Stride, Prop, Result);
end;

function CreateSharedBitmap(Target: ID2D1RenderTarget; Bitmap: ID2D1Bitmap): ID2D1Bitmap;
var
  Prop: TD2D1BitmapProperties;
begin
  Prop.pixelFormat := DefaultPixelFormat;
  Prop.dpiX := 0;
  Prop.dpiY := 0;
  Target.CreateSharedBitmap(ID2D1Bitmap, Pointer(Bitmap), @Prop, Result);
end;

function CreatePenStyle(Pen: IPen): ID2D1StrokeStyle;
const
  Caps: array[TLineCap] of D2D1_CAP_STYLE =
    (D2D1_CAP_STYLE_FLAT, D2D1_CAP_STYLE_ROUND, D2D1_CAP_STYLE_SQUARE);
  Joins: array[TLineJoin] of D2D1_LINE_JOIN =
    (D2D1_LINE_JOIN_MITER, D2D1_LINE_JOIN_ROUND, D2D1_LINE_JOIN_BEVEL);
  Dashes: array[TLinePattern] of D2D1_DASH_STYLE =
    (D2D1_DASH_STYLE_SOLID, D2D1_DASH_STYLE_DASH,
    D2D1_DASH_STYLE_DOT, D2D1_DASH_STYLE_DASH_DOT);
var
  Prop: TD2D1StrokeStyleProperties;
begin
  Prop.startCap := Caps[Pen.LineCap];
  Prop.endCap := Prop.startCap;
  if (Pen.LinePattern = pnDot) and (Pen.LineCap = cpButt) then
    Prop.dashCap := D2D1_CAP_STYLE_SQUARE
  else
    Prop.dashCap := Prop.startCap;
  Prop.lineJoin := Joins[Pen.LineJoin];
  Prop.miterLimit := Pen.MiterLimit;
  Prop.dashStyle := Dashes[Pen.LinePattern];
  Prop.dashOffset := Pen.LinePatternOffset;
  RenderFactory.CreateStrokeStyle(Prop, nil, 0, Result);
end;

function CreateSolidBrush(Target: ID2D1RenderTarget;
  const C: TColorB; Opacity: Byte): ID2D1SolidColorBrush;
var
  Prop: TD2D1BrushProperties;
begin
  Prop.opacity := Opacity / $FF;
  Prop.transform := MatrixIdentity;
  Target.CreateSolidColorBrush(Convert(C), @Prop, Result);
end;

function CreateLinearGradientBrush(Target: ID2D1RenderTarget; const A, B: TPointF;
  Opacity: Byte; const Stops: TGradientStops; Wrap: TGradientWrap): ID2D1LinearGradientBrush;
const
  Wraps: array[TGradientWrap] of TD2D1ExtendMode =
    (D2D1_EXTEND_MODE_CLAMP, D2D1_EXTEND_MODE_WRAP, D2D1_EXTEND_MODE_MIRROR);
  EmptyStops: TD2D1GradientStop = ();
var
  LineProp: TD2D1LinearGradientBrushProperties;
  BrushProp: TD2D1BrushProperties;
  Collection: ID2D1GradientStopCollection;
  Gamma: TD2D1Gamma;
  S: Pointer;
  I: Integer;
begin
  LineProp.startPoint := Convert(A);
  LineProp.endPoint := Convert(B);
  BrushProp.opacity := Opacity / $FF;
  BrushProp.transform := MatrixIdentity;
  if Stops.IsEmpty then
  begin
    S := @EmptyStops;
    I := 1;
  end
  else
  begin
    S := Stops.Data;
    I := Stops.Length;
  end;
   { D2D1_GAMMA_1_0 is not supported by cairo }
  if SurfaceOptions.GammaCorrection then
    Gamma := D2D1_GAMMA_1_0
  else
    Gamma := D2D1_GAMMA_2_2;
  Target.CreateGradientStopCollection(S, I, Gamma, Wraps[Wrap], Collection);
  Target.CreateLinearGradientBrush(LineProp, @BrushProp, Collection, Result);
end;

function CreateRadialGradientBrush(Target: ID2D1RenderTarget; const Rect: TRectF;
  Opacity: Byte; const Stops: TGradientStops; Wrap: TGradientWrap): ID2D1RadialGradientBrush;
const
  Wraps: array[TGradientWrap] of TD2D1ExtendMode =
    (D2D1_EXTEND_MODE_CLAMP, D2D1_EXTEND_MODE_WRAP, D2D1_EXTEND_MODE_MIRROR);
  EmptyStops: TD2D1GradientStop = ();
  Offset: TD2D1Point2F = ();
var
  RadProp: TD2D1RadialGradientBrushProperties;
  BrushProp: TD2D1BrushProperties;
  Collection: ID2D1GradientStopCollection;
  Gamma: TD2D1Gamma;
  S: Pointer;
  I: Integer;
begin
  RadProp.center := Convert(Rect.MidPoint);
  RadProp.gradientOriginOffset := Offset;
  RadProp.radiusX := Rect.Width / 2;
  RadProp.radiusY := Rect.Height / 2;
  BrushProp.opacity := Opacity / $FF;
  BrushProp.transform := MatrixIdentity;
  if Stops.IsEmpty then
  begin
    S := @EmptyStops;
    I := 1;
  end
  else
  begin
    S := Stops.Data;
    I := Stops.Length;
  end;
  if SurfaceOptions.GammaCorrection then
    Gamma := D2D1_GAMMA_1_0
  else
    Gamma := D2D1_GAMMA_2_2;
  Target.CreateGradientStopCollection(S, I, Gamma, Wraps[Wrap], Collection);
  Target.CreateRadialGradientBrush(RadProp, @BrushProp, Collection, Result);
end;

function CreateBitmapBrush(Target: ID2D1RenderTarget; Bitmap: IBitmap; Opacity: Byte): ID2D1BitmapBrush;
var
  BitTarget: ID2D1Bitmap;
  BitProp: TD2D1BitmapBrushProperties;
  BrushProp: TD2D1BrushProperties;
begin
  if Bitmap.Empty then
    Exit(nil);
  BitTarget := CreateBitmap(Target, Bitmap.Width, Bitmap.Height, Bitmap.Pixels);
  BitProp.extendModeX := D2D1_EXTEND_MODE_WRAP;
  BitProp.extendModeY := D2D1_EXTEND_MODE_WRAP;
  BitProp.interpolationMode := D2D1_BITMAP_INTERPOLATION_MODE_LINEAR;
  BrushProp.opacity := Opacity / $FF;
  BrushProp.transform := MatrixIdentity;
  Target.CreateBitmapBrush(BitTarget, @BitProp, @BrushProp, Result);
end;

function CreateFigure: ID2D1PathGeometry;
begin
  RenderFactory.CreatePathGeometry(Result);
end;

function CreateEllispe(const Rect: TRectF): ID2D1EllipseGeometry;
var
  E: TD2D1Ellipse;
begin
  E.point := Convert(Rect.MidPoint);
  E.radiusX := Rect.Width / 2;
  E.radiusY := Rect.Height / 2;
  RenderFactory.CreateEllipseGeometry(E, Result);
end;

function CreateRectangle(const Rect: TRectF): ID2D1RectangleGeometry;
begin
  RenderFactory.CreateRectangleGeometry(Convert(Rect), Result);
end;

function CreateRoundRectangle(const Rect: TRectF; Radius: Float): ID2D1RoundedRectangleGeometry;
var
  R: TD2D1RoundedRect;
begin
  R.rect := Convert(Rect);
  if Rect.Width < Radius * 2 then
    Radius := Rect.Width / 2;
  if Rect.Height < Radius * 2 then
    Radius := Rect.Height / 2;
  R.radiusX := Radius;
  R.radiusY := Radius;
  RenderFactory.CreateRoundedRectangleGeometry(R, Result);
end;

function CreateTransformed(G: ID2D1Geometry; Matrix: IMatrix): ID2D1Geometry;
var
  M: TMatrixD2D;
  T: ID2D1TransformedGeometry;
begin
  if Matrix is TMatrixD2D then
  begin
    M := Matrix as TMatrixD2D;
    if M.FIsIdentity then
      Result := G
    else
    begin
      RenderFactory.CreateTransformedGeometry(G, M.FMatrix, T);
      Result := T;
    end;
  end
  else
    Result := G;
end;

function CreateGroup(G: PID2D1Geometry; Count: Integer): ID2D1GeometryGroup;
begin
  if Count < 1 then
    Count := 0;
  RenderFactory.CreateGeometryGroup(D2D1_FILL_MODE_WINDING, G, Count, Result);
end;

function CreateLayerParameters(G: ID2D1Geometry): TD2D1LayerParameters;
const
  InfiniteRect: TD2D1RectF =
    (left: -$400000; top: -$400000;
    right: $800000; bottom: $800000);
begin
  with Result do
  begin
    contentBounds := InfiniteRect;
    geometricMask := G;
    maskAntialiasMode := D2D1_ANTIALIAS_MODE_PER_PRIMITIVE;
    maskTransform := MatrixIdentity;
    opacity := 1;
    opacityBrush := nil;
    layerOptions := D2D1_LAYER_OPTIONS_NONE;
  end;
end;

{ WriteFactory object creation routines }

function CreateTextFormat(Font: IFont): IDWriteTextFormat;
const
  Factor = 72 / 96;
  Eng = 'en-us';
  Weight: array[Boolean] of DWRITE_FONT_WEIGHT =
    (DWRITE_FONT_WEIGHT_NORMAL, DWRITE_FONT_WEIGHT_BOLD);
  Style: array[Boolean] of DWRITE_FONT_WEIGHT =
    (DWRITE_FONT_STYLE_NORMAL, DWRITE_FONT_STYLE_ITALIC);
var
  Size: Float;
  Name: WideString;
begin
  Size := Font.Size * Factor;
  Name := Font.Name;
  if WriteFactory.CreateTextFormat(PWideChar(Name), nil, Weight[fsBold in Font.Style],
    Style[fsItalic in Font.Style], DWRITE_FONT_STRETCH_NORMAL, Size,
    Eng, Result) <> S_OK then
    Result := nil;
end;

function CreateTextLayout(Format: IDWriteTextFormat; const Text: string;
  Width, Height: Float): IDWriteTextLayout;
var
  S: WideString;
begin
  if Format = nil then
    Result := nil;
  S := Text;
  if WriteFactory.CreateTextLayout(PWideChar(S), Length(S), Format, Width, Height,
    Result) <> S_OK then
    Result := nil;
end;

function CreateGdiTextLayout(Format: IDWriteTextFormat; const Text: string;
  Width, Height: Float): IDWriteTextLayout;
var
  S: WideString;
begin
  if Format = nil then
    Result := nil;
  S := Text;
  if WriteFactory.CreateGdiCompatibleTextLayout(PWideChar(S), Length(S), Format,
    Width, Height, 1, nil, True, Result) <> S_OK then
    Result := nil;
end;

{ Implentation of ISurface related interfaces }

constructor TMatrixD2D.Create(const M: TD2DMatrix3X2F);
begin
  inherited Create;
  FMatrix := M;
  FChanged := True;
  FIsIdentity := CompareMem(@FMatrix, @MatrixIdentity, SizeOf(FMatrix));
end;

function TMatrixD2D.Clone: IMatrix;
begin
  Result := TMatrixD2D.Create(FMatrix);
end;

procedure TMatrixD2D.Identity;
begin
  FMatrix := MatrixIdentity;
  FChanged := True;
  FIsIdentity := True;
end;

procedure TMatrixD2D.Multiply(M: IMatrix);
begin
  if M is TMatrixD2D then
    FMatrix := MatrixMultiply(FMatrix, (M as TMatrixD2D).FMatrix);
  FChanged := True;
  FIsIdentity := False;
end;

procedure TMatrixD2D.Scale(X, Y: Float);
begin
  MatrixScale(FMatrix, X, Y);
  FChanged := True;
  FIsIdentity := False;
end;

procedure TMatrixD2D.Rotate(Angle: Float);
begin
  MatrixRotate(FMatrix, Angle);
  FChanged := True;
  FIsIdentity := False;
end;

procedure TMatrixD2D.Translate(X, Y: Float);
begin
  MatrixTranslate(FMatrix, X, Y);
  FChanged := True;
  FIsIdentity := False;
end;

function TMatrixD2D.Transform(Point: TPointF): TPointF;
begin
  Result.X := FMatrix.m11 * Point.X + FMatrix.m12 * Point.Y + FMatrix.m31;
  Result.Y := FMatrix.m21 * Point.X + FMatrix.m22 * Point.Y + FMatrix.m32;
end;

{ TPenD2D }

constructor TPenD2D.Create(B: IBrush; W: Float);
begin
  inherited Create;
  FBrush := B;
  FWidth := W;
  FColor := clBlack;
  FMiterLimit := PenMiterLimitDefault;
end;

constructor TPenD2D.Create(const C: TColorB; W: Float);
begin
  inherited Create;
  FColor := C;
  FWidth := W;
  FMiterLimit := PenMiterLimitDefault;
end;

function TPenD2D.Acquire(Target: ID2D1RenderTarget; ID: Integer): Boolean;
begin
  if Target = nil then
    Exit(False);
  Result := True;
  if FBrush is TBrushD2D then
    Result := (FBrush as TBrushD2D).Acquire(Target, ID);
  if not Result then
    Exit;
  if ID <> FID then
    FFill := nil;
  FTarget := Target;
  FID := ID;
  Result := HandleAvailable;
end;

function TPenD2D.CurrentBrush: ID2D1Brush;
begin
  if FFill <> nil then
    Result := FFill
  else if FBrush is TBrushD2D then
    Result := (FBrush as TBrushD2D).FBrush
  else
    Result := nil;
end;

function TPenD2D.HandleAvailable: Boolean;
begin
  if FStyle = nil then
    FStyle := CreatePenStyle(Self);
  Result := False;
  if FBrush is TBrushD2D then
    Result := (FBrush as TBrushD2D).HandleAvailable;
  if Result then
    FFill := nil
  else if FFill = nil then
    FFill := CreateSolidBrush(FTarget, Color, $FF);
  Result := CurrentBrush <> nil;
end;

function TPenD2D.GetBrush: IBrush;
begin
  Result := FBrush;
end;

procedure TPenD2D.SetBrush(Value: IBrush);
begin
  FBrush := Value;
end;

function TPenD2D.GetColor: TColorB;
begin
  Result := FColor;
end;

procedure TPenD2D.SetColor(Value: TColorB);
begin
  FColor := Value;
  FFill := nil;
end;

function TPenD2D.GetWidth: Float;
begin
  Result := FWidth;
end;

procedure TPenD2D.SetWidth(Value: Float);
begin
  FWidth := Value;
end;

function TPenD2D.GetLinePattern: TLinePattern;
begin
  Result := FLinePattern;
end;

procedure TPenD2D.SetLinePattern(Value: TLinePattern);
begin
  if Value <> FLinePattern then
  begin
    FLinePattern := Value;
    FStyle := nil;
  end;
end;

function TPenD2D.GetLinePatternOffset: Float;
begin
  Result := FLinePatternOffset;
end;

procedure TPenD2D.SetLinePatternOffset(Value: Float);
begin
  if Value <> FLinePatternOffset then
  begin
    FLinePatternOffset := Value;
    FStyle := nil;
  end;
end;

function TPenD2D.GetLineCap: TLineCap;
begin
  Result := FLineCap;
end;

procedure TPenD2D.SetLineCap(Value: TLineCap);
begin
  if Value <> FLineCap then
  begin
    FLineCap := Value;
    FStyle := nil;
  end;
end;

function TPenD2D.GetLineJoin: TLineJoin;
begin
  Result := FLineJoin;
end;

procedure TPenD2D.SetLineJoin(Value: TLineJoin);
begin
  if Value <> FLineJoin then
  begin
    FLineJoin := Value;
    FStyle := nil;
  end;
end;

function TPenD2D.GetMiterLimit: Float;
begin
  Result := FMiterLimit;
end;

procedure TPenD2D.SetMiterLimit(Value: Float);
begin
  if Value <> FMiterLimit then
  begin
    FMiterLimit := Value;
    FStyle := nil;
  end;
end;

{ TBrushD2D }

constructor TBrushD2D.Create;
begin
  inherited Create;
  FOpacity := $FF;
end;

function TBrushD2D.Acquire(Target: ID2D1RenderTarget; ID: Integer): Boolean;
begin
  if Target = nil then
    Exit(False);
  if ID <> FID then
  begin
    FBrush := nil;
    if FMatrix is TMatrixD2D then
      (FMatrix as  TMatrixD2D).FChanged := True;
  end;
  FTarget := Target;
  FID := ID;
  Result := HandleAvailable;
end;

function TBrushD2D.HandleAvailable: Boolean;
var
  M: IMatrix;
begin
  Result := FBrush <> nil;
  if not Result then
    Exit;
  M := nil;
  if (FMatrix is TMatrixD2D) and (FMatrix as TMatrixD2D).FChanged then
  begin
    M := FMatrix;
    (FMatrix as TMatrixD2D).FChanged := False;
  end;
  if M <> nil then
    FBrush.SetTransform((M as TMatrixD2D).FMatrix);
end;

function TBrushD2D.GetMatrix: IMatrix;
begin
  if FMatrix = nil then
    FMatrix := NewMatrixD2D;
  Result := FMatrix;
end;

procedure TBrushD2D.SetMatrix(Value: IMatrix);
begin
  if Value is TMatrixD2D then
  begin
    (Matrix as TMatrixD2D).FMatrix := (Value as TMatrixD2D).FMatrix;
    (Matrix as TMatrixD2D).FChanged := True;
  end
  else
    Matrix.Identity;
end;

function TBrushD2D.GetOpacity: Byte;
begin
  Result := FOpacity;
end;

procedure TBrushD2D.SetOpacity(Value: Byte);
begin
  FOpacity := Value;
  if FBrush <> nil then
    FBrush.SetOpacity(FOpacity / $FF);
end;

{ TSolidBrushD2D }

constructor TSolidBrushD2D.Create(C: TColorB);
begin
  inherited Create;
  FColor := C;
end;

function TSolidBrushD2D.HandleAvailable: Boolean;
begin
  if FTarget = nil then
    Exit(False);
  if FBrush = nil then
    FBrush := CreateSolidBrush(FTarget, Color, Opacity);
  Result := inherited HandleAvailable;
end;

function TSolidBrushD2D.GetColor: TColorB;
begin
  Result := FColor;
end;

procedure TSolidBrushD2D.SetColor(Value: TColorB);
begin
  FColor := Value;
  FBrush := nil;
end;

{ TGradientBrushD2D }

constructor TGradientBrushD2D.Create(MidPoint: TPointF);
begin
  inherited Create;
  FCreated := True;
  FMidPoint := MidPoint;
end;

function TGradientBrushD2D.HandleAvailable: Boolean;
var
  M: IMatrix;
begin
  Result := FBrush <> nil;
  if not Result then
    Exit;
  M  := nil;
  if (FMatrix is TMatrixD2D) and (FMatrix as TMatrixD2D).FChanged then
  begin
    (FMatrix as TMatrixD2D).FChanged := False;
    M := FMatrix.Clone;
    FCreated := True;
  end
  else if FCreated then
    M := NewMatrixD2D;
  if FCreated then
  begin
    M.Translate(FMidPoint.X, FMidPoint.Y);
    FBrush.SetTransform((M as TMatrixD2D).FMatrix);
    FCreated := False;
  end;
end;

function TGradientBrushD2D.GetWrap: TGradientWrap;
begin
  Result := FWrap;
end;

procedure TGradientBrushD2D.SetWrap(Value: TGradientWrap);
begin
  if Value <> FWrap then
  begin
    FBrush := nil;
    FWrap := Value;
  end;
end;

procedure TGradientBrushD2D.AddStop(Color: TColorB; Offset: Float);
var
  S: TD2D1GradientStop;
  I: Integer;
begin
  FBrush := nil;
  S.color := Convert(Color);
  S.position := Offset;
  FStops.Push(S);
end;

{ TLinearGradientBrushD2D }

constructor TLinearGradientBrushD2D.Create(const A, B: TPointF);
var
  M: TPointF;
begin
  M.X := (A.X + B.X) / 2;
  M.Y := (A.Y + B.Y) / 2;
  inherited Create(M);
  FA := A - M;
  FB := B - M;
end;

function TLinearGradientBrushD2D.HandleAvailable: Boolean;
begin
  if FTarget = nil then
    Exit(False);
  FCreated := FBrush = nil;
  if FCreated then
    FBrush := CreateLinearGradientBrush(FTarget, FA, FB, FOpacity, FStops, FWrap);
  Result := inherited HandleAvailable;
end;

{ TRadialGradientBrushD2D }

constructor TRadialGradientBrushD2D.Create(const R: TRectF);
begin
  inherited Create(R.MidPoint);
  FRect := R;
  FRect.Center(0, 0);
end;

function TRadialGradientBrushD2D.HandleAvailable: Boolean;
begin
  if FTarget = nil then
    Exit(False);
  FCreated := FBrush = nil;
  if FCreated then
    FBrush := CreateRadialGradientBrush(FTarget, FRect, FOpacity, FStops, FWrap);
  Result := inherited HandleAvailable;
end;

{ TFontD2D }

constructor TFontD2D.Create(F: TFont);
const
  Points = 72;
var
  LogFont: TLogFont;
begin
  inherited Create;
  FName := F.Name;
  FColor := F.Color;
  FQuality := F.Quality;
  FStyle := F.Style;
  GetObject(F.Handle, SizeOf(LogFont), @LogFont);
  if LogFont.lfHeight < 0 then
  begin
    FSize := -LogFont.lfHeight / Points;
    FSize := FSize  * Dpi;
  end
  else
    FSize := LogFont.lfHeight;
end;

function TFontD2D.Format: IDWriteTextFormat;
begin
  if FFormat = nil then
    FFormat := CreateTextFormat(Self);
  Result := FFormat;
end;

function TFontD2D.GetName: string;
begin
  Result := FName;
end;

procedure TFontD2D.SetName(const Value: string);
begin
  FName := Value;
end;

function TFontD2D.GetColor: TColorB;
begin
  Result := FColor;
end;

procedure TFontD2D.SetColor(Value: TColorB);
begin
  FColor := Value;
end;

function TFontD2D.GetQuality: TFontQuality;
begin
  Result := FQuality;
end;

procedure TFontD2D.SetQuality(Value: TFontQuality);
begin
  FQuality := Value;
end;

function TFontD2D.GetStyle: TFontStyles;
begin
  Result := FStyle;
end;

procedure TFontD2D.SetStyle(Value: TFontStyles);
begin
  FStyle := Value;
end;

function TFontD2D.GetSize: Float;
begin
  Result := FSize;
end;

procedure TFontD2D.SetSize(Value: Float);
const
  MinSize = 4;
begin
  if Value < MinSize then
    Value := MinSize;
  if Value <> FSize then
  begin
    FSize := Value;
    FFormat := nil;
  end;
end;

{ TBitmapBrushD2D }

constructor TBitmapBrushD2D.Create(B: IBitmap);
begin
  inherited Create;
  FBitmap := B.Clone;
end;

function TBitmapBrushD2D.HandleAvailable: Boolean;
begin
  if FTarget = nil then
    Exit(False);
  if FBrush = nil then
    FBrush := CreateBitmapBrush(FTarget, FBitmap, FOpacity);
  Result := inherited HandleAvailable;
end;

{ TSurfacePathD2D }

constructor TPathD2D.Create(Path: ID2D1Geometry);
begin
  inherited Create;
  FPath := Path;
end;

{ TSurfacePathD2D }

constructor TSurfacePathD2D.Create(C: TSurfaceD2D);
begin
  inherited Create;
  FSurface := C;
  FData := TInterfaces<ID2D1Geometry>.Create;
end;

function TSurfacePathD2D.HandleAvailable: Boolean;
begin
  Result := (FSurface <> nil) and (FSurface.FTarget <> nil);
end;

procedure TSurfacePathD2D.Open;
begin
  if not HandleAvailable then
    Exit;
  if not FFigureOpened then
    Open(FFigureOrigin.x, FFigureOrigin.y);
end;

procedure TSurfacePathD2D.Open(X, Y: Float);
begin
  if not HandleAvailable then
    Exit;
  if FFigure = nil then
  begin
    FFigure := CreateFigure;
    FFigure.Open(FFigureSink);
  end
  else if FFigureOpened then
    FFigureSink.EndFigure(D2D1_FIGURE_END_OPEN);
  FFigureOrigin.x := X;
  FFigureOrigin.y := Y;
  FFigureSink.BeginFigure(FFigureOrigin, D2D1_FIGURE_BEGIN_FILLED);
  FFigureOpened := True;
end;

function TSurfacePathD2D.Clone: IPathData;
var
  G: ID2D1Geometry;
  I: Integer;
begin
  Add;
  I := FData.Count;
  if I = 0 then
    G := nil
  else if I = 1 then
    G := FData.First
  else
    G := CreateGroup(FData.Data, I);
  Result := TPathD2D.Create(G);
end;

procedure TSurfacePathD2D.Add;
begin
  if not HandleAvailable then
    Exit;
  if FFigure <> nil then
  begin
    if FFigureOpened then
      FFigureSink.EndFigure(D2D1_FIGURE_END_OPEN);
    FFigureSink.Close;
    FData.Add(CreateTransformed(FFigure, FSurface.Matrix));
    FFigure := nil;
    FFigureSink := nil;
    FFigureOpened := False;
  end;
end;

procedure TSurfacePathD2D.Add(G: ID2D1Geometry);
begin
  if not HandleAvailable then
    Exit;
  Add;
  FData.Add(CreateTransformed(G, FSurface.Matrix));
end;

procedure TSurfacePathD2D.Remove;
begin
  if not HandleAvailable then
    Exit;
  Add;
  FData := TInterfaces<ID2D1Geometry>.Create;
end;

procedure TSurfacePathD2D.Close;
begin
  if not HandleAvailable then
    Exit;
  if FFigureOpened then
  begin
    FFigureSink.EndFigure(D2D1_FIGURE_END_CLOSED);
    FFigureOpened := False;
  end;
end;

procedure TSurfacePathD2D.Join(Path: IPathData);
var
  G: ID2D1Geometry;
begin
  G := (Path as TPathD2D).FPath;
  if G <> nil then
    Add(G);
end;

function TSurfacePathD2D.ClipStack: IList<ID2D1Geometry>;
begin
  if FClipStack = nil then
    FClipStack := TInterfaces<ID2D1Geometry>.Create;
  Result := FClipStack;
end;

procedure TSurfacePathD2D.SaveClipStack;
var
  I: Integer;
begin
  if not HandleAvailable then
    Exit;
  FSurface.Draw;
  while FClipHeight > 0 do
  begin
    FSurface.FTarget.PopLayer;
    Dec(FClipHeight);
  end;
end;

procedure TSurfacePathD2D.RestoreClipStack;
var
  Params: TD2D1LayerParameters;
  G: ID2D1Geometry;
  L: ID2D1Layer;
begin
  if not HandleAvailable then
    Exit;
  FSurface.Draw;
  if (FClipHeight > 0) or (FClipStack = nil) then
    Exit;
  for G in FClipStack do
  begin
    FSurface.FTarget.CreateLayer(nil, L);
    Params := CreateLayerParameters(G);
    FSurface.FTarget.PushLayer(Params, L);
    Inc(FClipHeight);
  end;
end;

procedure TSurfacePathD2D.Clip;
var
  Params: TD2D1LayerParameters;
  P: IPathData;
  G: ID2D1Geometry;
  L: ID2D1Layer;
begin
  if not HandleAvailable then
    Exit;
  FSurface.Draw;
  Add;
  P := Clone;
  G := (P as TPathD2D).FPath;
  if G = nil then
    Exit;
  FSurface.FTarget.CreateLayer(nil, L);
  Params := CreateLayerParameters(G);
  FSurface.FTarget.PushLayer(Params, L);
  ClipStack.Add(G);
  Inc(FClipHeight);
  Remove;
end;

procedure TSurfacePathD2D.Unclip;
begin
  if not HandleAvailable then
    Exit;
  SaveClipStack;
  FClipStack := nil;
  FClipHeight := 0;
end;

{ TSurfaceStateD2D }

constructor TSurfaceStateD2D.Create(C: TSurfaceD2D);
var
  Path: TSurfacePathD2D;
  G: ID2D1Geometry;
begin
  inherited Create;
  Path := C.Path;
  if Path.FClipStack <> nil then
  begin
    ClipStack := TInterfaces<ID2D1Geometry>.Create;
    for G in Path.FClipStack do
      ClipStack.Add(G);
  end
  else
    ClipStack := nil;
  Data := TInterfaces<ID2D1Geometry>.Create;
  for G in Path.FData do
    Data.Add(G);
  Matrix := C.GetMatrix.Clone;
end;

procedure TSurfaceStateD2D.Restore(C: TSurfaceD2D);
var
  Path: TSurfacePathD2D;
begin
  Path := C.Path;
  Path.SaveClipStack;
  Path.FClipStack := ClipStack;
  Path.FClipHeight := 0;
  Path.RestoreClipStack;
  Path.Add;
  Path.FData := Path.FData;
  C.SetMatrix(Matrix);
end;

{ TSurfaceD2D }

constructor TSurfaceD2D.Create(T: ID2D1RenderTarget);
begin
  inherited Create;
  AcquireTarget(T);
  FPath := TSurfacePathD2D.Create(Self);
  FMatrix := NewMatrixD2D;
end;

destructor TSurfaceD2D.Destroy;
begin
  HandleRelease;
  Path.FSurface := nil;
  inherited Destroy;
end;

procedure TSurfaceD2D.ShareRelease;
begin
end;

function TSurfaceD2D.Path: TSurfacePathD2D;
begin
  Result := FPath as TSurfacePathD2D;
end;

function TSurfaceD2D.Matrix: TMatrixD2D;
begin
  Result := FMatrix as TMatrixD2D;
end;

procedure TSurfaceD2D.AcquireTarget(Target: ID2D1RenderTarget);
const
  NextSurfaceID: Integer = 0;
begin
  FTarget := Target;
  FID := InterLockedIncrement(NextSurfaceID);
end;

function TSurfaceD2D.AcquireBrush(Brush: IBrush; out B: ID2D1Brush): Boolean;
var
  D: TBrushD2D;
begin
  B := nil;
  if not HandleAvailable then
    Exit(False);
  D := Brush as TBrushD2D;
  Result := D.Acquire(FTarget, FID);
  if Result then
    B := D.FBrush;
end;

function TSurfaceD2D.AcquirePen(Pen: IPen; out B: ID2D1Brush; out S: ID2D1StrokeStyle): Boolean;
var
  D: TPenD2D;
begin
  B := nil;
  S := nil;
  if not HandleAvailable then
    Exit(False);
  D := Pen as TPenD2D;
  Result := D.Acquire(FTarget, FID);
  if Result then
  begin
    B := D.CurrentBrush;
    S := D.FStyle;
  end;
end;

function TSurfaceD2D.HandleAvailable: Boolean;
begin
  Result := FTarget <> nil;
end;

procedure TSurfaceD2D.HandleRelease;
begin
  Flush;
  FTarget := nil;
  Path.FClipStack := nil;
  Path.FClipHeight := 0;
  FStateStack := nil;
end;

function TSurfaceD2D.GetMatrix: IMatrix;
begin
  Result := FMatrix;
end;

procedure TSurfaceD2D.SetMatrix(Value: IMatrix);
begin
  if Value is TMatrixD2D then
  begin
    Matrix.FMatrix := (Value as TMatrixD2D).FMatrix;
    Matrix.FChanged := True;
  end
  else
    FMatrix.Identity;
end;

function TSurfaceD2D.GetPath: IPath;
begin
  Result := FPath;
end;

function TSurfaceD2D.GetHandle: Pointer;
begin
  Result := Pointer(FTarget);
end;

procedure TSurfaceD2D.Draw;
begin
  if not FDrawing then
  begin
    FTarget.BeginDraw;
    FDrawing := True;
    Path.RestoreClipStack;
  end;
end;

procedure TSurfaceD2D.Flush;
begin
  if not HandleAvailable then
    Exit;
  if FDrawing then
  begin
    Path.SaveClipStack;
    FTarget.EndDraw;
    FTarget.Flush(nil, nil);
  end;
  FDrawing := False;
end;

procedure TSurfaceD2D.Clear(Color: TColorB);
begin
  ShareRelease;
  if not HandleAvailable then
    Exit;
  Draw;
  FTarget.Clear(Convert(Color));
end;

{ Some work is required to get CopyTo to work with Direc2D. Use sparingly }

procedure TSurfaceD2D.CopyTo(const Source: TRectF; Surface: ISurface;
  const Dest: TRectF; Alpha: Byte = $FF; Quality: TResampleQuality = rqNormal);

  function AdjustSource(var SrcRect: TRectI; var DstRect: TRectF): Boolean;
  var
    FloatSize: TD2D1SizeF;
    SurfaceSize: TD2D1SizeU;
    { Source and dest sizes }
    SrcSize, DstSize: TPointF;
    { Ratios of clipped sides }
    L, T, R, B: Float;
  begin
    Result := False;
    if SrcRect.Empty or DstRect.Empty then
      Exit;
    FTarget.GetSize(FloatSize);
    SurfaceSize.width := Round(FloatSize.width);
    SurfaceSize.height := Round(FloatSize.height);
    { Check if source is entirely outside the Surface }
    if (SurfaceSize.width < 1) or (SurfaceSize.height < 1) then
      Exit;
    if SrcRect.Right < 1 then
      Exit;
    if SrcRect.Bottom < 1 then
      Exit;
    if SrcRect.Left >= SurfaceSize.width then
      Exit;
    if SrcRect.Top >= SurfaceSize.height then
      Exit;
    { Check if source sides should be clipped }
    SrcSize := SrcRect.Size;
    DstSize := DstRect.Size;
    if SrcRect.X < 0 then
    begin
      L := DstRect.X + DstSize.X * (-SrcRect.X / SrcSize.X);
      SrcRect.Left := 0;
    end
    else
      L := DstRect.X;
    if SrcRect.Y < 0 then
    begin
      T := DstRect.Y + DstSize.Y * (-SrcRect.Y / SrcSize.Y);
      SrcRect.Top := 0;
    end
    else
      T := DstRect.Y;
    if SrcRect.Right > SurfaceSize.width then
    begin
      R := DstRect.Right - DstSize.X * ((SrcRect.Right - SurfaceSize.width) / SrcSize.X);
      SrcRect.Right := SurfaceSize.width;
    end
    else
      R := DstRect.Right;
    if SrcRect.Bottom > SurfaceSize.height then
    begin
      B := DstRect.Bottom - DstSize.Y * ((SrcRect.Bottom - SurfaceSize.height) / SrcSize.Y);
      SrcRect.Bottom := SurfaceSize.height;
    end
    else
      B := DstRect.Bottom;
    DstRect.Left := L;
    DstRect.Top := T;
    DstRect.Right := R;
    DstRect.Bottom := B;
    if SrcRect.Empty or DstRect.Empty then
      Exit;
    Result := True;
  end;

  function CopyFromTarget(C: TSurfaceD2D; B: ID2D1Bitmap;
    const R: TRectI): Boolean;
  var
    TopLeft: TD2D1Point2U;
    SrcRect: TD2D1RectU;
  begin
    C.Draw;
    C.Path.SaveClipStack;
    TopLeft.x := 0;
    TopLeft.y := 0;
    SrcRect := Convert(R);
    B.CopyFromRenderTarget(TopLeft, C.FTarget, SrcRect);
    Result := C.FTarget.EndDraw(nil, nil) = S_OK;
    C.FTarget.BeginDraw;
    C.Path.RestoreClipStack;
  end;

const
  Resamples: array[TResampleQuality] of LongInt =
    (D2D1_BITMAP_INTERPOLATION_MODE_NEAREST_NEIGHBOR,
    D2D1_BITMAP_INTERPOLATION_MODE_LINEAR,
    D2D1_BITMAP_INTERPOLATION_MODE_LINEAR);
var
  DstSurface: TSurfaceD2D;
  SrcRect: TRectI;
  DstRect: TRectF;
  Shared: Boolean;
  SrcBitmap, DstBitmap: ID2D1Bitmap;
  FinalRect, SourceRect: TD2D1RectF;
  M: TD2D1Matrix3x2F;
begin
  if not HandleAvailable then
    Exit;
  DstSurface := Surface as TSurfaceD2D;
  if not DstSurface.HandleAvailable then
    Exit;
  SrcRect := TRectI(Source);
  DstRect := Dest;
  { Adjust the source to fall within the Surface bounds }
  if not AdjustSource(SrcRect, DstRect) then
    Exit;
  DstBitmap := nil;
  Shared := Self is ISharedBitmapTarget;
  if Shared then
    DstBitmap := (Self as ISharedBitmapTarget).ShareCreate(DstSurface.FTarget)
  else
  begin
    SrcBitmap := CreateBitmap(FTarget, SrcRect.Width, SrcRect.Height);
    if not CopyFromTarget(Self, SrcBitmap, SrcRect) then
      Exit;
    if FTarget = DstSurface.FTarget then
      DstBitmap := SrcBitmap
    else
      DstBitmap := CreateSharedBitmap(DstSurface.FTarget, SrcBitmap);
  end;
  DstSurface.Draw;
  FinalRect := Convert(DstRect);
  DstSurface.FTarget.GetTransform(M);
  DstSurface.FTarget.SetTransform(DstSurface.Matrix.FMatrix);
  if Shared then
  begin
    SourceRect.left := Source.Left;
    SourceRect.top := Source.Top;
    SourceRect.right := Source.Right;
    SourceRect.bottom := Source.Bottom;
    DstSurface.FTarget.DrawBitmap(DstBitmap, @FinalRect, Alpha / $FF,
      Resamples[Quality], @SourceRect);
  end
  else
    DstSurface.FTarget.DrawBitmap(DstBitmap, @FinalRect, Alpha / $FF);
  DstSurface.FTarget.SetTransform(M);
end;

procedure TSurfaceD2D.Save;
begin
  if not HandleAvailable then
    Exit;
  if FStateStack = nil then
    FStateStack := TObjects<TSurfaceStateD2D>.Create(True);
  FStateStack.Add(TSurfaceStateD2D.Create(Self));
end;

procedure TSurfaceD2D.Restore;
var
  S: TSurfaceStateD2D;
begin
  if not HandleAvailable then
    Exit;
  if FStateStack = nil then
    Exit;
  S := FStateStack.Last;
  S.Restore(Self);
  FStateStack.Remove(S);
  if FStateStack.Count < 1 then
    FStateStack := nil;
end;

procedure TSurfaceD2D.MoveTo(X, Y: Float);
begin
  if not HandleAvailable then
    Exit;
  Path.Open(X, Y);
end;

procedure TSurfaceD2D.LineTo(X, Y: Float);
var
  L: TD2D1Point2F;
begin
  ShareRelease;
  if not HandleAvailable then
    Exit;
  L.x := X;
  L.y := Y;
  Path.Open;
  Path.FFigureSink.AddLine(L);
end;

procedure TSurfaceD2D.ArcTo(const Rect: TRectF; BeginAngle, EndAngle: Float);
const
  Sweep: array[Boolean] of TD2D1SweepDirection =
    (D2D1_SWEEP_DIRECTION_COUNTER_CLOCKWISE, D2D1_SWEEP_DIRECTION_CLOCKWISE);
  Size: array[Boolean] of TD2D1ArcSize =
    (D2D1_ARC_SIZE_SMALL, D2D1_ARC_SIZE_LARGE);
var
  P: TSurfacePathD2D;
  A: TD2D1ArcSegment;
  L: TD2D1Point2F;
  W, H: Float;
begin
  ShareRelease;
  if not HandleAvailable then
    Exit;
  P := Path;
  L.x := Sin(BeginAngle);
  L.y := -Cos(BeginAngle);
  W := Rect.Width / 2;
  H := Rect.Height / 2;
  with Rect.MidPoint do
  begin
    L.x := X + L.x * W;
    L.y := Y + L.y * H;
  end;
  if P.FFigureOpened then
    P.FFigureSink.AddLine(L)
  else
    P.Open(L.x, L.y);
  L.x := Sin(EndAngle);
  L.y := -Cos(EndAngle);
  with Rect.MidPoint do
  begin
    L.x := X + L.x * W;
    L.y := Y + L.y * H;
  end;
  A.point := L;
  A.size.width := W;
  A.size.height := H;
  A.rotationAngle := 0;
  A.sweepDirection := Sweep[EndAngle > BeginAngle];
  A.arcSize := Size[Abs(BeginAngle - EndAngle) > Pi];
  P.FFigureSink.AddArc(A);
end;

procedure TSurfaceD2D.CurveTo(X, Y: Float; const C1, C2: TPointF);
var
  B: TD2D1BezierSegment;
begin
  ShareRelease;
  if not HandleAvailable then
    Exit;
  B.point1 := Convert(C1);
  B.point2 := Convert(C2);
  B.point3.x := X;
  B.point3.y := Y;
  Path.Open;
  Path.FFigureSink.AddBezier(B);
end;

procedure TSurfaceD2D.Ellipse(const Rect: TRectF);
begin
  ShareRelease;
  if not HandleAvailable then
    Exit;
  Path.Add(CreateEllispe(Rect));
end;

procedure TSurfaceD2D.Rectangle(const Rect: TRectF);
begin
  ShareRelease;
  if not HandleAvailable then
    Exit;
  Path.Add(CreateRectangle(Rect));
end;

procedure TSurfaceD2D.RoundRectangle(const Rect: TRectF; Radius: Float);
begin
  ShareRelease;
  if not HandleAvailable then
    Exit;
  Path.Add(CreateRoundRectangle(Rect, Radius));
end;

const
  MaxTextSize = High(Integer);

function TSurfaceD2D.TextSize(Font: IFont; const Text: string): TPointF;
const
  EmptyPoint: TPointF = ();
var
  Layout: IDWriteTextLayout;
  M: TDWriteTextMetrics;
begin
  if Text = '' then
    Exit(EmptyPoint);
  Layout := CreateTextLayout((Font as TFontD2D).Format, Text, MaxTextSize, MaxTextSize);
  if Layout = nil then
    Exit(EmptyPoint);
  if Layout.GetMetrics(M) <> S_OK then
    Exit(EmptyPoint);
  Result.X := M.widthIncludingTrailingWhitespace;
  Result.Y := M.height;
end;

function TSurfaceD2D.TextHeight(Font: IFont; const Text: string; Width: Float): Float;
var
  Layout: IDWriteTextLayout;
  M: TDWriteTextMetrics;
begin
  if Width < 1 then
    Exit(0);
  Layout := CreateTextLayout((Font as TFontD2D).FFormat, Text, Width, MaxTextSize);
  if Layout = nil then
    Exit(0);
  if Layout.GetMetrics(M) <> S_OK then
    Exit(0);
  Result := M.height;
end;

{ TTextRenderer is used when ErrorCorrection is false. I currently don't know
  if it is possible to use clear type with text rendered to geometric paths.
  If anyone can figure out how drop me a note at sysrpl@gmail.com }

type
  TTextRenderer = class(TInterfacedObject, IDWritePixelSnapping, IDWriteTextRenderer)
  private
    FSurface: TSurfaceD2D;
    FMatrix: TDWriteMatrix;
    function TranslatedMatrix(X, Y: Float): TD2D1Matrix3x2F;
  public
    constructor Create(Surface: TSurfaceD2D);
    { IDWritePixelSnapping }
    function IsPixelSnappingDisabled(clientDrawingContext: Pointer;
      var isDisabled: BOOL): HResult; stdcall;
    function GetCurrentTransform(clientDrawingContext: Pointer;
      var transform: TDWriteMatrix): HResult; stdcall;
    function GetPixelsPerDip(clientDrawingContext: Pointer;
      var pixelsPerDip: Single): HResult; stdcall;
    { IDWriteTextRenderer }
    function DrawGlyphRun(clientDrawingContext: Pointer; baselineOriginX: Single;
      baselineOriginY: Single; measuringMode: TDWriteMeasuringMode;
      var glyphRun: TDwriteGlyphRun;
      var glyphRunDescription: TDwriteGlyphRunDescription;
      clientDrawingEffect: IUnknown): HResult; stdcall;
    function DrawUnderline(clientDrawingContext: Pointer; baselineOriginX: Single;
      baselineOriginY: Single; var underline: TDwriteUnderline;
      clientDrawingEffect: IUnknown): HResult; stdcall;
    function DrawStrikethrough(clientDrawingContext: Pointer;
      baselineOriginX: Single; baselineOriginY: Single;
      var strikethrough: TDwriteStrikethrough;
      clientDrawingEffect: IUnknown): HResult; stdcall;
    function DrawInlineObject(clientDrawingContext: Pointer; originX: Single;
      originY: Single; inlineObject: IDWriteInlineObject; isSideways: BOOL;
      isRightToLeft: BOOL; clientDrawingEffect: IUnknown): HResult; stdcall;
  end;

constructor TTextRenderer.Create(Surface: TSurfaceD2D);
begin
  inherited Create;
  FSurface := Surface;
  FMatrix := TDWriteMatrix(FSurface.Matrix.FMatrix);
end;

{ TTextRenderer.IDWritePixelSnapping }

function TTextRenderer.IsPixelSnappingDisabled(clientDrawingContext: Pointer;
  var isDisabled: BOOL): HResult;
begin
  isDisabled := False;
  Result := S_OK;
end;

function TTextRenderer.GetCurrentTransform(clientDrawingContext: Pointer;
  var transform: TDWriteMatrix): HResult;
begin
  transform := FMatrix;
  Result := S_OK;
end;

function TTextRenderer.GetPixelsPerDip(clientDrawingContext: Pointer;
  var pixelsPerDip: Single): HResult;
begin
  pixelsPerDip := 1;
  Result := S_OK;
end;

{ TTextRenderer.IDWriteTextRenderer }

function TTextRenderer.TranslatedMatrix(X, Y: Float): TD2D1Matrix3x2F;
var
  M: TD2D1Matrix3x2F;
begin
  M := MatrixIdentity;
  MatrixTranslate(M, X, Y);
  Result := MatrixMultiply(M, TD2D1Matrix3x2F(FMatrix));
end;

function TTextRenderer.DrawGlyphRun(clientDrawingContext: Pointer; baselineOriginX: Single;
  baselineOriginY: Single; measuringMode: TDWriteMeasuringMode;
  var glyphRun: TDWriteGlyphRun;
  var glyphRunDescription: TDWriteGlyphRunDescription;
  clientDrawingEffect: IUnknown): HResult;
var
  F: ID2D1PathGeometry;
  S: ID2D1GeometrySink;
  M: TD2D1Matrix3x2F;
  T: ID2D1TransformedGeometry;
begin
  F := CreateFigure;
  F.Open(S);
  glyphRun.fontFace.GetGlyphRunOutline(
    glyphRun.fontEmSize,
    glyphRun.glyphIndices,
    glyphRun.glyphAdvances,
    glyphRun.glyphOffsets,
    glyphRun.glyphCount,
    glyphRun.isSideways,
    glyphRun.bidiLevel <> 0,
    S);
  S.Close;
  M := TranslatedMatrix(baselineOriginX, baselineOriginY);
  RenderFactory.CreateTransformedGeometry(F, M, T);
  FSurface.Path.FData.Add(T);
  Result := S_OK;
end;

function TTextRenderer.DrawUnderline(clientDrawingContext: Pointer; baselineOriginX: Single;
  baselineOriginY: Single; var underline: TDWriteUnderline;
  clientDrawingEffect: IUnknown): HResult;
var
  R: TRectF;
  G: ID2D1RectangleGeometry;
  M: TD2D1Matrix3x2F;
  T: ID2D1TransformedGeometry;
begin
  R := TRectF.Create(underline.width, underline.thickness);
  R.Offset(0, underline.offset);
  G := CreateRectangle(R);
  M := TranslatedMatrix(baselineOriginX, baselineOriginY);
  RenderFactory.CreateTransformedGeometry(G, M, T);
  FSurface.Path.FData.Add(T);
  Result := S_OK;
end;

function TTextRenderer.DrawStrikethrough(clientDrawingContext: Pointer;
  baselineOriginX: Single; baselineOriginY: Single;
  var strikethrough: TDWriteStrikethrough;
  clientDrawingEffect: IUnknown): HResult;
var
  R: TRectF;
  G: ID2D1RectangleGeometry;
  M: TD2D1Matrix3x2F;
  T: ID2D1TransformedGeometry;
begin
  R := TRectF.Create(strikethrough.width, strikethrough.thickness);
  R.Offset(0, strikethrough.offset);
  G := CreateRectangle(R);
  M := TranslatedMatrix(baselineOriginX, baselineOriginY);
  RenderFactory.CreateTransformedGeometry(G, M, T);
  FSurface.Path.FData.Add(T);
  Result := S_OK;
end;

function TTextRenderer.DrawInlineObject(clientDrawingContext: Pointer; originX: Single;
  originY: Single; inlineObject: IDWriteInlineObject; isSideways: BOOL;
  isRightToLeft: BOOL; clientDrawingEffect: IUnknown): HResult;
var
  R: IDWriteTextRenderer;
begin
  R := Self;
  inlineObject.Draw(clientDrawingContext, R, originX, originY, isSideways,
    isRightToLeft, clientDrawingEffect);
  Result := S_OK;
end;

{ TODO: Review all the options for text quality settings, especially concerning
  matrix operations and clear type + pixel snapping }

procedure TSurfaceD2D.TextOut(Font: IFont; const Text: string; const Rect: TRectF;
  Direction: TDirection; Immediate: Boolean = True);
const
  TrimChar: TDWriteTrimming = (granularity: DWRITE_TRIMMING_GRANULARITY_CHARACTER);
  RenderingModes: array[TFontQuality] of DWORD = (DWRITE_RENDERING_MODE_DEFAULT,
    DWRITE_RENDERING_MODE_ALIASED, DWRITE_RENDERING_MODE_CLEARTYPE_GDI_CLASSIC, DWRITE_RENDERING_MODE_ALIASED,
    DWRITE_RENDERING_MODE_CLEARTYPE_GDI_CLASSIC, DWRITE_RENDERING_MODE_CLEARTYPE_GDI_CLASSIC,
    DWRITE_RENDERING_MODE_CLEARTYPE_NATURAL);
  AntialiasModes: array[TFontQuality] of DWORD = (D2D1_TEXT_ANTIALIAS_MODE_DEFAULT,
    D2D1_TEXT_ANTIALIAS_MODE_ALIASED, D2D1_TEXT_ANTIALIAS_MODE_GRAYSCALE,
    D2D1_TEXT_ANTIALIAS_MODE_ALIASED, D2D1_TEXT_ANTIALIAS_MODE_GRAYSCALE,
    D2D1_TEXT_ANTIALIAS_MODE_CLEARTYPE, D2D1_TEXT_ANTIALIAS_MODE_CLEARTYPE);
var
  FontObj: TFontD2D;
  Layout: IDWriteTextLayout;
  Range: TDWriteTextRange;
  Ellipse: IDWriteInlineObject;
  Params1: IDWriteRenderingParams;
  Params2: IDWriteRenderingParams;
  Brush: ID2D1Brush;
  Renderer: IDWriteTextRenderer;
  M: TD2D1Matrix3x2F;
begin
  ShareRelease;
  if not HandleAvailable then
    Exit;
  if SurfaceOptions.ErrorCorrection or Immediate then
    Path.Remove;
  if Rect.Empty or (Text = '') then
    Exit;
  Draw;
  Path.Add;
  FontObj := Font as TFontD2D;
  { It's hard to tell if CreateGdiTextLayout makes any difference }
  Layout := CreateGdiTextLayout(FontObj.Format, Text, Rect.Width, Rect.Height);
  if Direction in [drLeft..drCenter] then
    Layout.SetWordWrapping(DWRITE_WORD_WRAPPING_NO_WRAP)
  else
    Layout.SetWordWrapping(DWRITE_WORD_WRAPPING_WRAP);
  case Direction of
    drLeft, drWrap, drFlow:
      Layout.SetTextAlignment(DWRITE_TEXT_ALIGNMENT_LEADING);
    drRight:
      Layout.SetTextAlignment(DWRITE_TEXT_ALIGNMENT_TRAILING);
  else
    Layout.SetTextAlignment(DWRITE_TEXT_ALIGNMENT_CENTER);
  end;
  case Direction of
    drLeft, drRight, drCenter, drFill:
      Layout.SetParagraphAlignment(DWRITE_PARAGRAPH_ALIGNMENT_CENTER);
    drDown:
      Layout.SetParagraphAlignment(DWRITE_PARAGRAPH_ALIGNMENT_FAR);
  else
    Layout.SetParagraphAlignment(DWRITE_PARAGRAPH_ALIGNMENT_NEAR);
  end;
  Range.StartPosition := 0;
  Range.Length := Length(Text);
  if fsBold in FontObj.Style then
    Layout.SetFontWeight(DWRITE_FONT_WEIGHT_BOLD, Range)
  else
    Layout.SetFontWeight(DWRITE_FONT_WEIGHT_NORMAL, Range);
  if fsItalic in FontObj.Style then
    Layout.SetFontStyle(DWRITE_FONT_STYLE_ITALIC, Range)
  else
    Layout.SetFontStyle(DWRITE_FONT_STYLE_NORMAL, Range);
  if fsStrikeOut in FontObj.Style then
    Layout.SetStrikethrough(True, Range);
  if fsUnderline in FontObj.Style then
    Layout.SetUnderline(True, Range);
  WriteFactory.CreateEllipsisTrimmingSign(Layout, Ellipse);
  Layout.SetTrimming(TrimChar, Ellipse);
  WriteFactory.CreateRenderingParams(Params1);
  WriteFactory.CreateCustomRenderingParams(Params1.GetGamma,
    Params1.GetEnhancedContrast, 1, Params1.GetPixelGeometry,
    RenderingModes[Font.Quality],
    Params2);
  FTarget.SetTextRenderingParams(Params2);
  FTarget.SetTextAntialiasMode(AntialiasModes[Font.Quality]);
  { ErrorCorrection looks better at untransformed small sizes, but doesn't
    actually build a geometric path. It's probably useful when you just want
    "standard" Windows clear type text. Right now it looks pretty awful when
    text is rotated or scaled unevenly }
  if SurfaceOptions.ErrorCorrection or Immediate then
  begin
    Brush := CreateSolidBrush(FTarget, Font.Color, $FF);
    FTarget.GetTransform(M);
    FTarget.SetTransform(Matrix.FMatrix);
    FTarget.DrawTextLayout(Convert(Rect.TopLeft), Layout, Brush,
      D2D1_DRAW_TEXT_OPTIONS_CLIP);
    FTarget.SetTransform(M);
  end
  else
  { Looks "okay" and is best for animated or transformed text }
  begin
    Renderer := TTextRenderer.Create(Self);
    Layout.Draw(nil, Renderer, Rect.X, Rect.Top);
  end;
end;

procedure ApplyMatrix(Brush: ID2D1Brush; Matrix: IMatrix; out State: TD2D1Matrix3x2F);
var
  M: TD2D1Matrix3x2F;
begin
  State := MatrixIdentity;
  if Brush = nil then
    Exit;
  M := (Matrix as TMatrixD2D).FMatrix;
  Brush.GetTransform(State);
  M := MatrixMultiply(State, M);
  Brush.SetTransform(M);
end;

procedure RestoreMatrix(Brush: ID2D1Brush; State: TD2D1Matrix3x2F);
begin
  if Brush = nil then
    Exit;
  Brush.SetTransform(State);
end;

function PenWidth(Matrix: IMatrix; Width: Float): Float;
const
  A: TPointF = (X: 1; Y : 0);
  B: TPointF = (X: 0; Y : 0);
begin
  Result := Matrix.Transform(A).Dist(Matrix.Transform(B));
  Result := Abs(Result * Width);
end;

procedure TSurfaceD2D.FillOrStroke(Brush: IBrush; Pen: IPen; Preserve: Boolean);
var
  Acquired: Boolean;
  State: TD2D1Matrix3x2F;
  P: TSurfacePathD2D;
  B: ID2D1Brush;
  S: ID2D1StrokeStyle;
  G: ID2D1Geometry;
  I: Integer;
begin
  ShareRelease;
  B := nil;
  S := nil;
  if Brush <> nil then
  begin
    Acquired := AcquireBrush(Brush, B);
    if Acquired then
      ApplyMatrix(B, GetMatrix, State);
  end
  else
  begin
    Acquired := AcquirePen(Pen, B, S);
    if Acquired then
      ApplyMatrix(B, GetMatrix, State);
  end;
  if not Acquired then
    Exit;
  Draw;
  P := Path;
  P.Add;
  I := P.FData.Count;
  if I = 0 then
    Exit;
  if I = 1 then
    G := P.FData.First
  else
    G := CreateGroup(P.FData.Data, I);
  if Brush <> nil then
    FTarget.FillGeometry(G, B)
  else
    FTarget.DrawGeometry(G, B, PenWidth(GetMatrix, Pen.Width), S);
  if not Preserve then
    P.Remove;
  if B <> nil then
    RestoreMatrix(B, State);
end;

procedure TSurfaceD2D.Stroke(Pen: IPen; Preserve: Boolean = False);
begin
  FillOrStroke(nil, Pen, Preserve);
end;

procedure TSurfaceD2D.Fill(Brush: IBrush; Preserve: Boolean = False);
begin
  FillOrStroke(Brush, nil, Preserve);
end;

function AlignRect(Pen: IPen; const Rect: TRectF): TRectF;
var
  I: Integer;
begin
  Result := Rect;
  I := Round(Pen.Width);
  if I and 1 = 1 then
  begin
    Result.Offset(0.5, 0.5);
    Result.Width := Result.Width - 1;
    Result.Height := Result.Height - 1;
  end;
end;

procedure TSurfaceD2D.StrokeRect(Pen: IPen; const Rect: TRectF);
begin
  Path.Remove;
  Rectangle(AlignRect(Pen, Rect));
  Stroke(Pen);
end;

procedure TSurfaceD2D.FillRect(Brush: IBrush; const Rect: TRectF);
begin
  Path.Remove;
  Rectangle(Rect);
  Fill(Brush);
end;

procedure TSurfaceD2D.StrokeRoundRect(Pen: IPen; const Rect: TRectF; Radius: Float);
begin
  Path.Remove;
  RoundRectangle(AlignRect(Pen, Rect), Radius);
  Stroke(Pen);
end;

procedure TSurfaceD2D.FillRoundRect(Brush: IBrush; const Rect: TRectF; Radius: Float);
begin
  Path.Remove;
  RoundRectangle(Rect, Radius);
  Fill(Brush);
end;

{ TBitmapSurfaceD2D }

constructor TBitmapSurfaceD2D.Create(B: TBitmapD2D);
begin
  inherited Create(nil);
  FBitmap := B;
end;

procedure TBitmapSurfaceD2D.HandleRelease;
begin
  ShareRelease;
  inherited HandleRelease;
end;

function TBitmapSurfaceD2D.HandleAvailable: Boolean;
var
  T: ID2D1DCRenderTarget;
begin
  Result := inherited HandleAvailable;
  if not Result then
    if (FBitmap <> nil) and FBitmap.HandleAvailable then
    begin
      T := CreateDCTarget;
      T.BindDC(FBitmap.FBitmap.DC, FBitmap.ClientRect);
      AcquireTarget(T);
      FSharedTarget := nil;
      FSurfaceBitmap := nil;
      Result := True;
    end;
end;

function TBitmapSurfaceD2D.ShareCreate(Target: ID2D1RenderTarget): ID2D1Bitmap;
var
  B: ID2D1Bitmap;
begin
  Result := nil;
  if not HandleAvailable then
    Exit;
  if FSurfaceBitmap = nil then
  begin
    FSurfaceBitmap := CreateBitmap(FTarget, FBitmap.Width, FBitmap.Height, FBitmap.Pixels);
    FSharedTarget := Target;
    FSharedBitmap := CreateSharedBitmap(FSharedTarget, FSurfaceBitmap);
  end
  else if FSharedTarget <> Target then
  begin
    FSharedTarget := Target;
    FSharedBitmap := CreateSharedBitmap(FSharedTarget, FSurfaceBitmap);
  end;
  Result := FSharedBitmap;
end;

procedure TBitmapSurfaceD2D.ShareRelease;
begin
  FSurfaceBitmap := nil;
  FSharedTarget := nil;
  FSharedBitmap := nil;
end;

{ TWndSurfaceD2D }

constructor TWndSurfaceD2D.Create(Wnd: HWND);
begin
  inherited Create(nil);
  FWnd := Wnd;
end;

function TWndSurfaceD2D.HandleAvailable: Boolean;
var
  R: TRect;
begin
  if FTarget = nil then
    AcquireTarget(CreateWndTarget(FWnd, FRect))
  else if FFlushed and IsWindow(FWnd) then
  begin
    GetClientRect(FWnd, R);
    { We must recreate the target if the client size has changed }
    if (R.Left <> FRect.Left) or (R.Top <> FRect.Top) or
      (R.Right <> FRect.Right) or (R.Bottom <> FRect.Bottom) then
      AcquireTarget(CreateWndTarget(FWnd, FRect));
    FFlushed := False;
  end;
  Result := inherited HandleAvailable;
end;

procedure TWndSurfaceD2D.Flush;
var
  P: TSurfacePathD2D;
begin
  FFlushed := True;
  { Clear all state when flushed; matching the persistent cairo version.
    Plus it makes good sense }
  P := Path;
  P.SaveClipStack;
  P.Unclip;
  P.Add;
  P.FData := TInterfaces<ID2D1Geometry>.Create;
  P.FClipStack := nil;
  P.FClipHeight := 0;
  FStateStack := nil;
  Matrix.Identity;
  inherited Flush;
end;

{ TBitmapD2D }

destructor TBitmapD2D.Destroy;
begin
  HandleRelease;
  if FSurface is TBitmapSurfaceD2D then
    (FSurface as TBitmapSurfaceD2D).FBitmap := nil;
  inherited Destroy;
end;

procedure TBitmapD2D.HandleRelease;
begin
  if FSurface is TBitmapSurfaceD2D then
    (FSurface as TBitmapSurfaceD2D).HandleRelease;
  inherited HandleRelease;
end;

procedure TBitmapD2D.Clear;
begin
  if FSurface is ISharedBitmapTarget then
    (FSurface as ISharedBitmapTarget).ShareRelease;
  inherited Clear;
end;

function TBitmapD2D.GetSurface: ISurface;
begin
  if FSurface = nil then
    FSurface := TBitmapSurfaceD2D.Create(Self);
  Result := FSurface;
end;

function TBitmapD2D.GetPixels: PPixel;
begin
  if FSurface is ISharedBitmapTarget then
    (FSurface as ISharedBitmapTarget).ShareRelease;
  Result := inherited GetPixels;
end;

{ New object routines }

function NewMatrixD2D: IMatrix;
begin
  Result := TMatrixD2D.Create(MatrixIdentity);
end;

function NewPenD2D(Brush: IBrush; Width: Float = 1): IPen;
begin
  Result := TPenD2D.Create(Brush, Width);
end;

function NewPenD2D(Color: TColorB; Width: Float = 1): IPen;
begin
  Result := TPenD2D.Create(Color, Width);
end;

function NewSolidBrushD2D(Color: TColorB): ISolidBrush;
begin
  Result := TSolidBrushD2D.Create(Color);
end;

function NewBitmapBrushD2D(Bitmap: IBitmap): IBitmapBrush;
begin
  Result := TBitmapBrushD2D.Create(Bitmap);
end;

function NewLinearGradientBrushD2D(X1, Y1, X2, Y2: Float): ILinearGradientBrush;
begin
  Result := TLinearGradientBrushD2D.Create(TPointF.Create(X1, Y1),
    TPointF.Create(X2, Y2));
end;

function NewLinearGradientBrushD2D(const A, B: TPointF): ILinearGradientBrush;
begin
  Result := TLinearGradientBrushD2D.Create(A, B);
end;

function NewRadialGradientBrushD2D(const Rect: TRectF): IRadialGradientBrush;
begin
  Result := TRadialGradientBrushD2D.Create(Rect);
end;

function NewFontD2D(Font: TFont): IFont;
begin
  Result := TFontD2D.Create(Font);
end;

var
  ScreenDC: HDC;

function NewSurfaceD2D(Canvas: TCanvas): ISurface;
var
  T: ID2D1DCRenderTarget;
  R: TRect;
begin
  T := CreateDCTarget;
  if Canvas = nil then
  begin
    if ScreenDC = 0 then
      ScreenDC := GetDC(0);
    GetWindowRect(GetDesktopWindow, R);
    T.BindDC(ScreenDC, TRectI.Create(R.Right - R.Left, R.Bottom - R.Top));
  end
  else
    T.BindDC(Canvas.Handle, TRectI.Create(Canvas.Width, Canvas.Height));
  Result := TSurfaceD2D.Create(T);
end;

function NewSurfaceD2D(Control: TWinControl): ISurface;
begin
  Result := TWndSurfaceD2D.Create(Control.Handle);
end;

function NewBitmapD2D(Width, Height: Integer): IBitmap;
begin
  Result := TBitmapD2D.Create;
  Result.SetSize(Width, Height);
end;
{$endif}

end.

