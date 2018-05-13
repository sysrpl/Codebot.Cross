(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified September 2013                             *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.graphics.windows.Surfacecairo.txt> }
unit Codebot.Graphics.Linux.SurfaceCairo;

{$i codebot.inc}

interface

{$ifdef linuxgtk}
uses
  SysUtils, Classes, Graphics, Controls,
  Codebot.System,
  Codebot.Graphics.Types,
  Codebot.Forms.Management;

{ New object routines }

function NewMatrixCairo: IMatrix;
function NewPenCairo(Brush: IBrush; Width: Float = 1): IPen; overload;
function NewPenCairo(Color: TColorB; Width: Float = 1): IPen; overload;
function NewSolidBrushCairo(Color: TColorB): ISolidBrush;
function NewBitmapBrushCairo(Bitmap: IBitmap): IBitmapBrush;
function NewLinearGradientBrushCairo(X1, Y1, X2, Y2: Float): ILinearGradientBrush; overload;
function NewLinearGradientBrushCairo(const A, B: TPointF): ILinearGradientBrush; overload;
function NewRadialGradientBrushCairo(const Rect: TRectF): IRadialGradientBrush;
function NewFontCairo(const FontName: string; FontSize: Integer): IFont; overload;
function NewFontCairo(Font: TFont = nil): IFont; overload;
function NewSurfaceCairo(Canvas: TCanvas): ISurface; overload;
function NewSurfaceCairo(Control: TWinControl): ISurface; overload;
function NewBitmapCairo(BitmapBuffer: Pointer): IBitmap; overload;
function NewBitmapCairo(Width, Height: Integer): IBitmap; overload;
function NewSplashCairo: ISplash;
{$endif}

implementation

{$ifdef linuxgtk}
uses
  glib2, gdk2, gtk2, gtk2def, gtk2proc, gtk2int, gdk2pixbuf, gtk2extra,
  cairo, pango, pangocairo;

const
  Delta = 0.5;

{ Friendly cairo types }

type
  PCairoSurface = Pcairo_surface_t;
  PCairo = Pcairo_t;
  PCairoPattern = Pcairo_pattern_t;
  PCairoPath = Pcairo_path_t;
  TCairoLineCap = cairo_line_cap_t;
  TCairoLineJoin = cairo_line_join_t;
  TCairoMatrix = cairo_matrix_t;
  PCairoMatrix = Pcairo_matrix_t;
  { TODO: Research why TCairoFontOptions is not used }
  // TCairoFontOptions = cairo_font_options_t;
  PCairoFontOptions = Pcairo_font_options_t;
  TCairoAntiAlias = cairo_antialias_t;
  TCairoFilter = cairo_filter_t;

{ Extra gdk types and routines }

  GdkPixbufModulePattern = GSize;
  PGdkPixbufModulePattern = ^GdkPixbufModulePattern;

  GdkPixbufFormat = record
    name: PGChar;
    signature: PGdkPixbufModulePattern;
    domain: PGChar;
    description: PGChar;
    mime_types: PPGChar;
    extensions: PPGChar;
    flags: GUInt32;
    disabled: GBoolean;
    license: PGChar;
  end;
  PGdkPixbufFormat = ^GdkPixbufFormat;

  GdkPixbufSaveFunc = function(buffer: PGChar; count: GSize; error: PPGError;
    data: GPointer): GBoolean; cdecl;

function gdk_pixbuf_loader_get_format(loader: PGdkPixbufLoader): PGdkPixbufFormat; cdecl; external gdkpixbuflib;
function gdk_pixbuf_save_to_callback(pixbuf: PGdkPixbuf; save_func: GdkPixbufSaveFunc;
  data: gpointer; _type: PGChar; error: PPGError; empty: Pointer): GBoolean; cdecl; external gdkpixbuflib;

{ Extra cairo routines }

procedure gdk_cairo_reset_clip(cr: Pcairo_t; drawable: PGdkDrawable); cdecl; external gdklib;

{ Extra pango routines }

function pango_version_string: PChar; cdecl; external pangolib;
procedure pango_layout_set_height(layout: PPangoLayout; height: LongInt); cdecl; external pangolib;
function pango_layout_get_height(layout: PPangoLayout): LongInt; cdecl; external pangolib;
procedure pango_layout_set_ellipsize(layout: PPangoLayout;
  ellipsize: TPangoEllipsizeMode); cdecl; external pangolib;
function pango_layout_get_ellipsize(layout: PPangoLayout): TPangoEllipsizeMode; cdecl; external pangolib;
function pango_layout_is_ellipsized(layout: PPangoLayout): GBoolean; cdecl; external pangolib;

type
  TCairoMatrixHelper = record helper for TCairoMatrix
  public
    procedure Identity;
    function Multiply(M: TCairoMatrix): TCairoMatrix;
    procedure Translate(X, Y: Float);
    procedure Scale(X, Y: Float);
    procedure Rotate(Radians: Float);
  end;

procedure TCairoMatrixHelper.Identity;
begin
  cairo_matrix_init_identity(@Self);
end;

function TCairoMatrixHelper.Multiply(M: TCairoMatrix): TCairoMatrix;
begin
  cairo_matrix_multiply(@Result, @Self, @M);
end;

procedure TCairoMatrixHelper.Translate(X, Y: Float);
var
  M: TCairoMatrix;
begin
  M.Identity;
  cairo_matrix_translate(@M, X, Y);
  Self := Multiply(M);
end;

procedure TCairoMatrixHelper.Scale(X, Y: Float);
var
  M: TCairoMatrix;
begin
  M.Identity;
  cairo_matrix_scale(@M, X, Y);
  Self := Multiply(M);
end;

procedure TCairoMatrixHelper.Rotate(Radians: Float);
var
  M: TCairoMatrix;
begin
  M.Identity;
  cairo_matrix_rotate(@M, Radians);
  Self := Multiply(M);
end;

{ IMatrixCairo }

type
  IMatrixCairo = interface(IMatrix)
  ['{9FD40980-43A8-4C51-AE7E-9CCB10E40EFF}']
    function GetMatrix: TCairoMatrix;
    procedure SetMatrix(const Value: TCairoMatrix);
    function GetInverse: TCairoMatrix;
    procedure SetInverse(const Value: TCairoMatrix);
    function GetChanged: Boolean;
    procedure SetChanged(Value: Boolean);
    function GetAtMatrix: PCairoMatrix;
    function GetAtInverse: PCairoMatrix;
    property Matrix: TCairoMatrix read GetMatrix write SetMatrix;
    property Inverse: TCairoMatrix read GetInverse write SetInverse;
    property AtMatrix: PCairoMatrix read GetAtMatrix;
    property AtInverse: PCairoMatrix read GetAtInverse;
    property Changed: Boolean read GetChanged write SetChanged;
  end;

  { TMatrixCairo }

  TMatrixCairo = class(TInterfacedObject, IMatrix, IMatrixCairo)
  private
    FMatrix: TCairoMatrix;
    FInverse: TCairoMatrix;
    FChanged: Boolean;
    FX: Float;
    FY: Float;
  public
    constructor Create; overload;
    constructor Create(const M, I: TCairoMatrix); overload;
    constructor Create(X, Y: Float); overload;
    function Clone: IMatrix; virtual;
    procedure Identity; virtual;
    procedure Multiply(M: IMatrix);
    procedure Translate(X, Y: Float);
    procedure Scale(X, Y: Float);
    procedure Rotate(Radians: Float);
    function Transform(Point: TPointF): TPointF;
    function GetMatrix: TCairoMatrix;
    procedure SetMatrix(const Value: TCairoMatrix);
    function GetInverse: TCairoMatrix;
    procedure SetInverse(const Value: TCairoMatrix);
    function GetAtMatrix: PCairoMatrix;
    function GetAtInverse: PCairoMatrix;
    function GetChanged: Boolean;
    procedure SetChanged(Value: Boolean);
  end;

{ TMatrixMultiplyCairo }

  TMatrixMultiplyCairo = class(TMatrixCairo)
  private
    FMultiply: TCairoMatrix;
    FMultiplyInverse: TCairoMatrix;
  public
    constructor Create(const M, I: TCairoMatrix);
    function Clone: IMatrix; override;
    procedure Identity; override;
  end;

{ TPenCairo }

  TPenCairo = class(TInterfacedObject, IPen)
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
    property Brush: IBrush read GetBrush write SetBrush;
    property Color: TColorB read GetColor write SetColor;
    property Width: Float read GetWidth write SetWidth;
    property LinePattern: TLinePattern read GetLinePattern write SetLinePattern;
    property LinePatternOffset: Float read GetLinePatternOffset write SetLinePatternOffset;
    property LineCap: TLineCap read GetLineCap write SetLineCap;
    property LineJoin: TLineJoin read GetLineJoin write SetLineJoin;
    property MiterLimit: Float read GetMiterLimit write SetMiterLimit;
  end;

{ TBrushCairo }

  TBrushCairo = class(TInterfacedObject, IBrush)
  private
    FOpacity: Byte;
  public
    constructor Create;
    function GetMatrix: IMatrix; virtual;
    procedure SetMatrix(Value: IMatrix); virtual;
    function GetOpacity: Byte;
    procedure SetOpacity(Value: Byte);
    property Matrix: IMatrix read GetMatrix write SetMatrix;
    property Opacity: Byte read GetOpacity write SetOpacity;
  end;

{ TSolidBrushCairo }

  TSolidBrushCairo = class(TBrushCairo, ISolidBrush)
  private
    FColor: TColorB;
  public
    constructor Create(C: TColorB);
    function GetColor: TColorB;
    procedure SetColor(Value: TColorB);
    property Color: TColorB read GetColor write SetColor;
  end;

{ TPatternBrush }

  TPatternBrush = class(TBrushCairo)
  private
    FPattern: PCairoPattern;
    FMatrix: IMatrix;
  protected
    function HandleNeeded: Boolean; virtual;
    procedure HandleRelease;
  public
    constructor Create(P: PCairoPattern);
    destructor Destroy; override;
    function GetMatrix: IMatrix; override;
    procedure SetMatrix(Value: IMatrix); override;
  end;

{ TBitmapBrushCairo }

  TBitmapBrushCairo = class(TPatternBrush, IBitmapBrush)
  private
    FBitmap: IBitmap;
  protected
    function HandleNeeded: Boolean; override;
  public
    constructor Create(B: IBitmap);
  end;

{ TGradientBrushCairo }

  TGradientBrushCairo = class(TPatternBrush, IGradientBrush)
  private
    FWrap: TGradientWrap;
  public
    constructor Create(Pattern: PCairoPattern; M: IMatrix);
    function GetWrap: TGradientWrap;
    procedure SetWrap(Value: TGradientWrap);
    procedure AddStop(Color: TColorB; Offset: Float);
    property Wrap: TGradientWrap read GetWrap write SetWrap;
  end;

{ TLinearGradientBrushCairo }

  TLinearGradientBrushCairo = class(TGradientBrushCairo, ILinearGradientBrush)
  private
    FOrigin: TPointF;
    FAngle: Float;
  protected
    function HandleNeeded: Boolean; override;
  public
    constructor Create(X1, Y1, X2, Y2: Float);
  end;

{ TRadialGradientBrushCairo }

  TRadialGradientBrushCairo = class(TGradientBrushCairo, IRadialGradientBrush)
  private
    FOrigin: TPointF;
    FScaleY: Float;
  protected
    function HandleNeeded: Boolean; override;
  public
    constructor Create(const Rect: TRectF);
  end;

{ TFontCairo }

  TFontCairo = class(TInterfacedObject, IFont)
  private
    FDesc: PPangoFontDescription;
    FName: string;
    FColor: TColorB;
    FQuality: TFontQuality;
    FStyle: TFontStyles;
    FSize: Float;
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
    property Name: string read GetName write SetName;
    property Color: TColorB read GetColor write SetColor;
    property Quality: TFontQuality read GetQuality write SetQuality;
    property Style: TFontStyles read GetStyle write SetStyle;
    property Size: Float read GetSize write SetSize;
  end;
{ TPathCairo }

  TPathCairo = class(TInterfacedObject, IPathData)
  private
    FPath: PCairoPath;
  public
    constructor Create(Path: PCairoPath);
    destructor Destroy; override;
  end;

{ TSurfacePathCairo }

  TSurfacePathCairo = class(TInterfacedObject, IPath)
  private
    FCairo: PCairo;
    function HandleAvailable: Boolean;
  public
    constructor Create(Cairo: PCairo);
    function Clone: IPathData;
    procedure Add;
    procedure Remove;
    procedure Close;
    procedure Join(Path: IPathData);
    procedure Clip;
    procedure Unclip; virtual;
  end;

{ TSurfacePathClipCairo }

  TSurfacePathClipCairo = class(TSurfacePathCairo)
  private
    FDrawable: PGdkDrawable;
    FClip: TRectI;
  public
    constructor Create(Cairo: PCairo; Drawable: PGdkDrawable; Clip: TRectI);
    procedure Unclip; override;
  end;

  TBitmapCairo = class;

{ TSurfaceCairo }

  TSurfaceCairo = class(TInterfacedObject, ISurface)
  private
    FCairo: PCairo;
    FPath: IPath;
    FPathCairo: TSurfacePathCairo;
    FMatrix: IMatrixCairo;
    FLayout: PPangoLayout;
    procedure SetSource(const C: TColorB); overload;
    procedure SetSource(P: IPen); overload;
    procedure SetSource(B: IBrush); overload;
    procedure SetFont(F: IFont);
  protected
    function LayoutAvailable: Boolean;
    function HandleAvailable: Boolean; virtual;
    procedure HandleRelease;
  public
    constructor Create(C: PCairo = nil);
    destructor Destroy; override;
    function GetMatrix: IMatrix;
    procedure SetMatrix(Value: IMatrix);
    function GetPath: IPath;
    function GetHandle: Pointer;
    procedure Flush; virtual;
    procedure Clear; overload;
    procedure Clear(Color: TColorB); overload;
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

{ TClipSurfaceCairo }

  TClipSurfaceCairo = class(TSurfaceCairo)
  { TODO: Acquire brush for pattern brushes setting origins to clip }
  { TODO: Consider reworking clipping multiply matrix to use translate only }
  public
    constructor Create(Drawable: PGdkDrawable; const Clip: TRectI);
  end;

{ TControlSurfaceCairo }

  TControlSurfaceCairo = class(TSurfaceCairo)
  private
    FControl: TWinControl;
    FDrawable: PGdkDrawable;
  protected
    function HandleAvailable: Boolean; override;
  public
    constructor Create(Control: TWinControl);
    procedure Flush; override;
  end;

{ TBitmapSurfaceCairo }

  TBitmapSurfaceCairo = class(TSurfaceCairo)
  private
    FBitmap: TBitmapCairo;
    FDirty: Boolean;
  protected
    function HandleAvailable: Boolean; override;
  public
    constructor Create(Bitmap: TBitmapCairo);
  end;

{ TBitmapCairo }

  TBitmapCairo = class(TInterfacedObject, IBitmap)
  private
    FBuffer: PGdkPixbuf;
    FSurface: ISurface;
    FSurfaceCairo: TBitmapSurfaceCairo;
    FFormat: TImageFormat;
    procedure Flush;
    procedure FlipPixels;
    procedure Premultiply;
    procedure SetBuffer(Value: PGdkPixbuf);
  public
    constructor Create(B: PGdkPixbuf = nil);
    destructor Destroy; override;
    function Clone: IBitmap;
    function GetEmpty: Boolean;
    function GetSurface: ISurface;
    function GetClientRect: TRectI;
    function GetFormat: TImageFormat;
    procedure SetFormat(Value: TImageFormat);
    function GetHeight: Integer;
    function GetWidth: Integer;
    function GetPixels: PPixel;
    procedure Clear;
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(Stream: TStream);
    function Resample(Width, Height: Integer; Quality: TResampleQuality = rqNormal): IBitmap;
    procedure SetSize(Width, Height: Integer);
    property Empty: Boolean read GetEmpty;
    property Surface: ISurface read GetSurface;
    property ClientRect: TRectI read GetClientRect;
    property Format: TImageFormat read GetFormat write SetFormat;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property Pixels: PPixel read GetPixels;
  end;

function MatrixIdentity: TCairoMatrix;
begin
  cairo_matrix_init_identity(@Result);
end;

{ TMatrixCairo }

constructor TMatrixCairo.Create;
begin
  inherited Create;
  Identity;
  FChanged := False;
end;

constructor TMatrixCairo.Create(const M, I: TCairoMatrix);
begin
  inherited Create;
  FMatrix := M;
  FInverse := I;
  FChanged := True;
end;

constructor TMatrixCairo.Create(X, Y: Float);
begin
  inherited Create;
  FX := X;
  FY := Y;
  Identity;
end;

function TMatrixCairo.Clone: IMatrix;
begin
  Result := TMatrixCairo.Create(FMatrix, FInverse);
end;

procedure TMatrixCairo.Identity;
begin
  FMatrix.Identity;
  FInverse.Identity;
  Translate(FX, FY);
  FChanged := True;
end;

procedure TMatrixCairo.Multiply(M: IMatrix);
var
  MC: IMatrixCairo;
begin
  if M is IMatrixCairo then
  begin
    MC := M as IMatrixCairo;
    FMatrix := FMatrix.Multiply(MC.Matrix);
    FInverse := FInverse.Multiply(MC.Inverse);
  end;
  FChanged := True;
end;

procedure TMatrixCairo.Scale(X, Y: Float);
begin
  FMatrix.Scale(X, Y);
  if X > 0 then
    X := 1 / X;
  if Y > 0 then
    Y := 1 / Y;
  FInverse.Scale(X, Y);
  FChanged := True;
end;

procedure TMatrixCairo.Rotate(Radians: Float);
begin
  FMatrix.Rotate(Radians);
  FInverse.Rotate(-Radians);
  FChanged := True;
end;

procedure TMatrixCairo.Translate(X, Y: Float);
begin
  FMatrix.Translate(X, Y);
  FInverse.Translate(-X, -Y);
  FChanged := True;
end;

function TMatrixCairo.Transform(Point: TPointF): TPointF;
begin
  Result.X := FMatrix.xx * Point.X + FMatrix.xy * Point.Y + FMatrix.x0;
  Result.Y := FMatrix.yx * Point.X + FMatrix.yy * Point.Y + FMatrix.y0
end;

function TMatrixCairo.GetMatrix: TCairoMatrix;
begin
  Result := FMatrix;
end;

procedure TMatrixCairo.SetMatrix(const Value: TCairoMatrix);
begin
  FMatrix := Value;
  FChanged := True;
end;

function TMatrixCairo.GetInverse: TCairoMatrix;
begin
  Result := FInverse;
end;

procedure TMatrixCairo.SetInverse(const Value: TCairoMatrix);
begin
  FInverse := Value;
  FChanged := True;
end;

function TMatrixCairo.GetAtMatrix: PCairoMatrix;
begin
  Result := @FMatrix;
end;

function TMatrixCairo.GetAtInverse: PCairoMatrix;
begin
  Result := @FInverse;
end;

function TMatrixCairo.GetChanged: Boolean;
begin
  Result := FChanged;
end;

procedure TMatrixCairo.SetChanged(Value: Boolean);
begin
  FChanged := Value;
end;

{ TMatrixMultiplyCairo }

constructor TMatrixMultiplyCairo.Create(const M, I: TCairoMatrix);
begin
  inherited Create(M, I);
  FMultiply := M;
  FMultiplyInverse := I;
  FChanged := True;
end;

function TMatrixMultiplyCairo.Clone: IMatrix;
var
  M: TMatrixMultiplyCairo;
begin
  Result := TMatrixMultiplyCairo.Create(FMultiply, FMultiplyInverse);
  M := Result as TMatrixMultiplyCairo;
  M.FMatrix := FMatrix;
  M.FInverse := FInverse;
end;

procedure TMatrixMultiplyCairo.Identity;
begin
  FMatrix := FMultiply;
  FInverse := FMultiplyInverse;
  FChanged := True;
end;

{ TPenCairo }

constructor TPenCairo.Create(B: IBrush; W: Float);
begin
  inherited Create;
  FBrush := B;
  FWidth := W;
  FMiterLimit := PenMiterLimitDefault;
end;

constructor TPenCairo.Create(C: TColorB; W: Float);
begin
  inherited Create;
  FColor := C;
  FWidth := W;
  FMiterLimit := PenMiterLimitDefault;
end;

function TPenCairo.GetBrush: IBrush;
begin
  Result := FBrush;
end;

procedure TPenCairo.SetBrush(Value: IBrush);
begin
  FBrush := Value;
end;

function TPenCairo.GetColor: TColorB;
begin
  Result := FColor;
end;

procedure TPenCairo.SetColor(Value: TColorB);
begin
  FColor := Value;
end;

function TPenCairo.GetWidth: Float;
begin
  Result := FWidth;
end;

procedure TPenCairo.SetWidth(Value: Float);
begin
  FWidth := Value;
end;

function TPenCairo.GetLinePattern: TLinePattern;
begin
  Result := FLinePattern;
end;

procedure TPenCairo.SetLinePattern(Value: TLinePattern);
begin
  FLinePattern:= Value;
end;

function TPenCairo.GetLinePatternOffset: Float;
begin
  Result := FLinePatternOffset;
end;

procedure TPenCairo.SetLinePatternOffset(Value: Float);
begin
  FLinePatternOffset := Value;
end;

function TPenCairo.GetLineCap: TLineCap;
begin
  Result := FLineCap;
end;

procedure TPenCairo.SetLineCap(Value: TLineCap);
begin
  FLineCap := Value;
end;

function TPenCairo.GetLineJoin: TLineJoin;
begin
  Result := FLineJoin;
end;

procedure TPenCairo.SetLineJoin(Value: TLineJoin);
begin
  FLineJoin := Value;
end;

function TPenCairo.GetMiterLimit: Float;
begin
  Result := FMiterLimit;
end;

procedure TPenCairo.SetMiterLimit(Value: Float);
begin
  if Value < 0 then
    Value := 0
  else if Value > 100 then
    Value := 100;
  FMiterLimit := Value;
end;

{ TBrushCairo }

constructor TBrushCairo.Create;
begin
  inherited Create;
  FOpacity := $FF;
end;

function TBrushCairo.GetMatrix: IMatrix;
const
  DummyMatrix: IMatrix = nil;
begin
  if DummyMatrix = nil then
    DummyMatrix := NewMatrixCairo;
  Result := DummyMatrix;
end;

procedure TBrushCairo.SetMatrix(Value: IMatrix);
begin
end;

function TBrushCairo.GetOpacity: Byte;
begin
  Result := FOpacity;
end;

procedure TBrushCairo.SetOpacity(Value: Byte);
begin
  FOpacity := Value;
end;

{ TSolidBrushCairo }

constructor TSolidBrushCairo.Create(C: TColorB);
begin
  inherited Create;
  FColor := C;
end;

function TSolidBrushCairo.GetColor: TColorB;
begin
  Result := FColor;
end;

procedure TSolidBrushCairo.SetColor(Value: TColorB);
begin
  FColor := Value;
end;

{ TPatternBrush }

constructor TPatternBrush.Create(P: PCairoPattern);
begin
  inherited Create;
  FPattern := P;
end;

destructor TPatternBrush.Destroy;
begin
  HandleRelease;
  inherited Destroy;
end;

function TPatternBrush.HandleNeeded: Boolean;
var
  Mat: TMatrixCairo;
begin
  Result := FPattern <> nil;
  if not Result then
    Exit;
  if FMatrix is TMatrixCairo then
  begin
    Mat := FMatrix as TMatrixCairo;
    if Mat.FChanged then
      cairo_pattern_set_matrix(FPattern, @Mat.FInverse);
    Mat.FChanged := False;
  end;
end;

procedure TPatternBrush.HandleRelease;
begin
  if FPattern <> nil then
    cairo_pattern_destroy(FPattern);
  FPattern := nil;
end;

function TPatternBrush.GetMatrix: IMatrix;
begin
  if FMatrix = nil then
    FMatrix := NewMatrixCairo;
  Result := FMatrix;
end;

procedure TPatternBrush.SetMatrix(Value: IMatrix);
begin
  if FMatrix = nil then
    FMatrix := NewMatrixCairo;
  FMatrix.Identity;
  FMatrix.Multiply(Value);
end;

{ TBitmapBrushCairo }

constructor TBitmapBrushCairo.Create(B: IBitmap);
begin
  inherited Create(nil);
  FBitmap := B.Clone;
end;

function TBitmapBrushCairo.HandleNeeded: Boolean;
var
  Surface: TSurfaceCairo;
  CairoSurface: PCairoSurface;
begin
  Result := False;
  if (FBitmap = nil) or (FBitmap.Empty) then
    Exit;
  if FPattern = nil then
  begin
    Surface := FBitmap.Surface as TSurfaceCairo;
    if not Surface.HandleAvailable then
      Exit;
    CairoSurface := cairo_get_target(Surface.FCairo);
    FPattern := cairo_pattern_create_for_surface(CairoSurface);
  end;
  cairo_pattern_set_extend(FPattern, CAIRO_EXTEND_REPEAT);
  Result := inherited HandleNeeded;
end;

{ TGradientBrushCairo }

constructor TGradientBrushCairo.Create(Pattern: PCairoPattern; M: IMatrix);
begin
  inherited Create(Pattern);
  FMatrix := M;
  Wrap := gwClamp;
end;

function TGradientBrushCairo.GetWrap: TGradientWrap;
begin
  Result := FWrap;
end;

procedure TGradientBrushCairo.SetWrap(Value: TGradientWrap);
begin
  FWrap := Value;
  case Value of
    gwRepeat: cairo_pattern_set_extend(FPattern, CAIRO_EXTEND_REPEAT);
    gwReflect: cairo_pattern_set_extend(FPattern, CAIRO_EXTEND_REFLECT);
  else
    cairo_pattern_set_extend(FPattern, CAIRO_EXTEND_PAD);
  end;
end;

procedure TGradientBrushCairo.AddStop(Color: TColorB; Offset: Float);
begin
  cairo_pattern_add_color_stop_rgba(FPattern, Offset, Color.Red / $FF,
    Color.Green / $FF, Color.Blue / $FF, Color.Alpha / $FF);
end;

{ TLinearGradientBrushCairo }

constructor TLinearGradientBrushCairo.Create(X1, Y1, X2, Y2: Float);
const
  VertAngle = Pi / 2;
var
  Point: TPointF;
  Dist: Float;
  Pattern: PCairoPattern;
  M: IMatrix;
begin
  FOrigin.X := (X1 + X2) / 2;
  FOrigin.Y := (Y1 + Y2) / 2;
  Point.X := X1 - FOrigin.X;
  Point.Y := Y1 - FOrigin.Y;
  if Point.X = 0 then
    if Point.Y < 0 then
      FAngle := VertAngle
    else
      FAngle := -VertAngle
  else if Point.Y = 0 then
    if Point.X < 0 then
      FAngle := 0
    else
      FAngle := Pi
  else
    FAngle := ArcTan(Point.Y / Point.X);
  Dist := Sqrt(Point.X * Point.X + Point.Y * Point.Y);
  Pattern := cairo_pattern_create_linear(-Dist, 0, Dist, 0);
  M := NewMatrixCairo;
  (M as TMatrixCairo).FChanged := True;
  inherited Create(Pattern, M);
end;

function TLinearGradientBrushCairo.HandleNeeded: Boolean;
var
  Mat: TMatrixCairo;
  M: TCairoMatrix;
begin
  Result := FPattern <> nil;
  if not Result then
    Exit;
  if FMatrix is TMatrixCairo then
  begin
    Mat := FMatrix as TMatrixCairo;
    if Mat.FChanged then
    begin
      M := Mat.FInverse;
      cairo_matrix_rotate(@M, -FAngle);
      cairo_matrix_translate(@M, -FOrigin.X, -FOrigin.Y);
      cairo_pattern_set_matrix(FPattern, @M);
    end;
    Mat.FChanged := False;
  end;
end;

{ TRadialGradientBrushCairo }

constructor TRadialGradientBrushCairo.Create(const Rect: TRectF);
var
  Pattern: PCairoPattern;
  M: IMatrix;
begin
  FOrigin := Rect.MidPoint;
  if Rect.Width <> 0 then
    FScaleY := Rect.Height / Rect.Width;
  Pattern := cairo_pattern_create_radial(0, 0, 1, 1, 1, Rect.Width / 2);
  M := NewMatrixCairo;
  (M as TMatrixCairo).FChanged := True;
  inherited Create(Pattern, M);
end;

function TRadialGradientBrushCairo.HandleNeeded: Boolean;
var
  Mat: TMatrixCairo;
  S, M: TCairoMatrix;
begin
  Result := FPattern <> nil;
  if not Result then
    Exit;
  if FMatrix is TMatrixCairo then
  begin
    Mat := FMatrix as TMatrixCairo;
    if Mat.FChanged then
    begin
      S := MatrixIdentity;
      if FScaleY <> 0 then
        cairo_matrix_scale(@S, 1, 1 / FScaleY);
      cairo_matrix_multiply(@M, @Mat.FInverse, @S);
      cairo_matrix_translate(@M, -FOrigin.X, -FOrigin.Y);
      cairo_pattern_set_matrix(FPattern, @M);
    end;
    Mat.FChanged := False;
  end;
end;

{ TFontCairo }

constructor TFontCairo.Create(Font: TFont);
begin
  inherited Create;
  FDesc := pango_font_description_new;
  if Font = nil then
    Font := FormManager.DefaulFont;
  if Font.Name <> 'default' then
    SetName(Font.Name)
  else
    SetName(FormManager.DefaulFont.Name);
  if Font.Size > 4 then
    SetSize(Font.Size)
  else
    SetSize(FormManager.DefaulFont.Size);
  Quality := Font.Quality;
  Color := Font.Color;
  Style := Font.Style;
end;

constructor TFontCairo.Create(const FontName: string; FontSize: Integer = 10);
begin
  inherited Create;
  FDesc := pango_font_description_new;
  FColor := clBlack;
  SetName(FontName);
  SetSize(FontSize);
end;

destructor TFontCairo.Destroy;
begin
  pango_font_description_free(FDesc);
  inherited Destroy;
end;

function TFontCairo.GetName: string;
begin
  Result := FName;
end;

procedure TFontCairo.SetName(const Value: string);
begin
  if Value <> FName then
  begin
    FName := Value;
    pango_font_description_set_family(FDesc, PChar(Value));
  end;
end;

function TFontCairo.GetColor: TColorB;
begin
  Result := FColor;
end;

procedure TFontCairo.SetColor(Value: TColorB);
begin
  FColor := Value;
end;

function TFontCairo.GetQuality: TFontQuality;
begin
  Result := FQuality;
end;

procedure TFontCairo.SetQuality(Value: TFontQuality);
begin
  FQuality := Value;
end;

function TFontCairo.GetStyle: TFontStyles;
begin
  Result := FStyle;
end;

procedure TFontCairo.SetStyle(Value: TFontStyles);
const
  Weights: array[Boolean] of TPangoWeight =
    (PANGO_WEIGHT_NORMAL, PANGO_WEIGHT_BOLD);
  Styles: array[Boolean] of TPangoStyle =
    (PANGO_STYLE_NORMAL, PANGO_STYLE_ITALIC);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    pango_font_description_set_weight(FDesc, Weights[fsBold in FStyle]);
    pango_font_description_set_style(FDesc, Styles[fsItalic in FStyle]);
  end;
end;

function TFontCairo.GetSize: Float;
begin
  Result := FSize;
end;

procedure TFontCairo.SetSize(Value: Float);
const
  MinSize = 4;
begin
  if Value < MinSize then
    Value := MinSize;
  if Value <> FSize then
  begin
    FSize := Value;
    pango_font_description_set_size(FDesc, Round(FSize * PANGO_SCALE));
  end;
end;

{ TPathCairo }

constructor TPathCairo.Create(Path: PCairoPath);
begin
  inherited Create;
  FPath := Path;
end;

destructor TPathCairo.Destroy;
begin
  if FPath <> nil then
    cairo_path_destroy(FPath);
  inherited Destroy;
end;

{ TSurfacePathCairo }

constructor TSurfacePathCairo.Create(Cairo: PCairo);
begin
  inherited Create;
  FCairo := Cairo;
end;

function TSurfacePathCairo.HandleAvailable: Boolean;
begin
  Result := FCairo <> nil;
end;

function TSurfacePathCairo.Clone: IPathData;
begin
  if HandleAvailable then
    Result := TPathCairo.Create(cairo_copy_path(FCairo))
  else
    Result := TPathCairo.Create(nil);
end;

procedure TSurfacePathCairo.Add;
begin
  if HandleAvailable then
    cairo_new_sub_path(FCairo);
end;

procedure TSurfacePathCairo.Remove;
begin
  if HandleAvailable then
    cairo_new_path(FCairo);
end;

procedure TSurfacePathCairo.Close;
begin
  if HandleAvailable then
    cairo_close_path(FCairo);
end;

procedure TSurfacePathCairo.Join(Path: IPathData);
var
  P: TPathCairo;
begin
  if HandleAvailable then
    if Path is TPathCairo then
    begin
      P := Path as TPathCairo;
      if P.FPath <> nil then
        cairo_append_path(FCairo, P.FPath);
    end;
end;

procedure TSurfacePathCairo.Clip;
begin
  if HandleAvailable then
    cairo_clip(FCairo);
end;

procedure TSurfacePathCairo.Unclip;
begin
  if HandleAvailable then
    cairo_reset_clip(FCairo);
end;

{ TSurfacePathClipCairo }

constructor TSurfacePathClipCairo.Create(Cairo: PCairo; Drawable: PGdkDrawable; Clip: TRectI);
begin
  inherited Create(Cairo);
  FDrawable := Drawable;
  FClip := Clip;
  Unclip;
end;

procedure TSurfacePathClipCairo.Unclip;
var
  OldMat: TCairoMatrix;
  NewMat: TCairoMatrix;
begin
  inherited Unclip;
  if not HandleAvailable then
    Exit;
  cairo_get_matrix(FCairo, @OldMat);
  NewMat.Identity;
  cairo_set_matrix(FCairo, @NewMat);
  gdk_cairo_reset_clip(FCairo, FDrawable);
  cairo_new_path(FCairo);
  NewMat.Translate(FClip.X, FClip.Y);
  cairo_set_matrix(FCairo, @NewMat);
  cairo_rectangle(FCairo, 0, 0, FClip.Width, FClip.Height);
  cairo_clip(FCairo);
  cairo_set_matrix(FCairo, @OldMat);
end;

{ TSurfaceCairo }

constructor TSurfaceCairo.Create(C: PCairo = nil);
begin
  inherited Create;
  FCairo := C;
  FMatrix := TMatrixCairo.Create;
end;

destructor TSurfaceCairo.Destroy;
begin
  HandleRelease;
  inherited Destroy;
end;

function TSurfaceCairo.HandleAvailable: Boolean;
begin
  Result := FCairo <> nil;
  if Result then
  begin
    if FMatrix.Changed then
    begin
      cairo_set_matrix(FCairo, FMatrix.AtMatrix);
      FMatrix.Changed := False;
    end;
    if FPathCairo <> nil then
      FPathCairo.FCairo := FCairo;
  end;
end;

procedure TSurfaceCairo.HandleRelease;
begin
  if FLayout <> nil then
    g_object_unref(FLayout);
  FLayout := nil;
  if FPathCairo <> nil then
    FPathCairo.FCairo := nil;
  if FCairo <> nil then
    cairo_destroy(FCairo);
  FCairo := nil;
end;

function TSurfaceCairo.LayoutAvailable: Boolean;
begin
  Result := HandleAvailable;
  if not Result then Exit;
  if FLayout = nil then
    FLayout := pango_cairo_create_layout(FCairo);
end;

procedure TSurfaceCairo.SetSource(const C: TColorB);
begin
  if not HandleAvailable then Exit;
  cairo_set_source_rgba(FCairo, C.Red / $FF, C.Green / $FF,
    C.Blue / $FF, C.Alpha / $FF);
end;

procedure TSurfaceCairo.SetSource(P: IPen);
const
  LineCaps: array[TLineCap] of TCairoLineCap = (CAIRO_LINE_CAP_BUTT,
    CAIRO_LINE_CAP_ROUND, CAIRO_LINE_CAP_SQUARE);
  LineJoins: array[TLineJoin] of TCairoLineJoin = (CAIRO_LINE_JOIN_MITER,
    CAIRO_LINE_JOIN_ROUND, CAIRO_LINE_JOIN_BEVEL);
var
  Dashes: array[0..4] of Double;
  W: Double;
  I: Integer;
begin
  if not HandleAvailable then Exit;
  if P.Brush = nil then
    SetSource(P.Color)
  else
    SetSource(P.Brush);
  cairo_set_line_width(FCairo, P.Width);
  cairo_set_line_cap(FCairo, LineCaps[P.LineCap]);
  cairo_set_line_join(FCairo, LineJoins[P.LineJoin]);
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
  cairo_set_dash(FCairo, @Dashes, I, P.LinePatternOffset);
end;

procedure TSurfaceCairo.SetSource(B: IBrush);
var
  S: TSolidBrushCairo;
  P: TPatternBrush;
begin
  if not HandleAvailable then Exit;
  if B.Opacity = 0 then
    cairo_set_source_rgba(FCairo, 0, 0, 0, 0)
  else if B is TSolidBrushCairo then
  begin
    S := B as TSolidBrushCairo;
    SetSource(S.Color);
  end
  else if B is TPatternBrush then
  begin
    P := B as TPatternBrush;
    if P.HandleNeeded then
      cairo_set_source(FCairo, P.FPattern)
    else
      SetSource(clBlack);
  end
  else
    cairo_set_source_rgba(FCairo, 0, 0, 0, 1);
end;

function TSurfaceCairo.GetMatrix: IMatrix;
begin
  Result := FMatrix;
end;

procedure TSurfaceCairo.SetMatrix(Value: IMatrix);
var
  M: IMatrixCairo;
begin
  FMatrix.Identity;
  if Value is TMatrixCairo then
  begin
    M := (Value.Clone as IMatrixCairo);
    if M = FMatrix then
      Exit;
    M.Multiply(FMatrix);
    FMatrix.Matrix := M.Matrix;
    FMatrix.Inverse := M.Inverse;
  end;
end;

function TSurfaceCairo.GetPath: IPath;
begin
  if FPath = nil then
  begin
    FPath := TSurfacePathCairo.Create(FCairo);
    FPathCairo := FPath as TSurfacePathCairo;
  end;
  Result := FPath;
end;

function TSurfaceCairo.GetHandle: Pointer;
begin
  Result := FCairo;
end;

procedure TSurfaceCairo.Flush;
var
  S: PCairoSurface;
begin
  if FCairo <> nil then
  begin
    S := cairo_get_target(FCairo);
    cairo_surface_flush(S);
  end;
end;

procedure TSurfaceCairo.Clear;
begin
  if not HandleAvailable then Exit;
  cairo_save(FCairo);
  cairo_set_operator(FCairo, CAIRO_OPERATOR_CLEAR);
  cairo_paint(FCairo);
  cairo_restore(FCairo);
end;

procedure TSurfaceCairo.Clear(Color: TColorB);
begin
  if not HandleAvailable then Exit;
  cairo_save(FCairo);
  cairo_set_operator(FCairo, CAIRO_OPERATOR_SOURCE);
  SetSource(Color);
  cairo_paint(FCairo);
  cairo_restore(FCairo);
end;

procedure TSurfaceCairo.CopyTo(const Source: TRectF; Surface: ISurface;
  const Dest: TRectF; Alpha: Byte = $FF; Quality: TResampleQuality = rqNormal);
const
  Resamples: array[TResampleQuality] of TCairoFilter =
    (CAIRO_FILTER_FAST, CAIRO_FILTER_GOOD, CAIRO_FILTER_BILINEAR);
var
  DestSurface: TSurfaceCairo;
  CairoSurface: PCairoSurface;
  Pattern: PCairoPattern;
  Matrix: TCairoMatrix;
begin
  if not (Surface is TSurfaceCairo) then
    Exit;
  DestSurface :=  Surface as TSurfaceCairo;
  if not DestSurface.HandleAvailable then
    Exit;
  if Alpha = 0 then
  begin
    cairo_new_path(DestSurface.FCairo);
    Exit;
  end;
  if Source.Empty or Dest.Empty then
    Exit;
  if not HandleAvailable then
    Exit;
  CairoSurface := cairo_get_target(FCairo);
  Pattern := cairo_pattern_create_for_surface(CairoSurface);
  cairo_save(DestSurface.FCairo);
  cairo_new_path(DestSurface.FCairo);
  cairo_matrix_init_identity(@Matrix);
  cairo_matrix_translate(@Matrix, Source.X, Source.Y);
  cairo_matrix_scale(@Matrix, Source.Width / Dest.Width,
    Source.Height / Dest.Height);
  cairo_matrix_translate(@Matrix, -Dest.X, -Dest.Y);
  cairo_pattern_set_matrix(Pattern, @Matrix);
  cairo_rectangle(DestSurface.FCairo, Dest.X, Dest.Y, Dest.Width, Dest.Height);
  cairo_set_source(DestSurface.FCairo, Pattern);
  cairo_pattern_set_filter(Pattern, Resamples[Quality]);
  if Alpha < $FF then
  begin
    cairo_clip(DestSurface.FCairo);
    cairo_paint_with_alpha(DestSurface.FCairo, Alpha / $FF);
  end
  else
    cairo_fill(DestSurface.FCairo);
  cairo_set_source_rgba(DestSurface.FCairo, 0, 0, 0, 0);
  cairo_pattern_destroy(Pattern);
  cairo_restore(DestSurface.FCairo);
end;

procedure TSurfaceCairo.Save;
begin
  if HandleAvailable then
    cairo_save(FCairo);
end;

procedure TSurfaceCairo.Restore;
begin
  if HandleAvailable then
    cairo_restore(FCairo);
end;

procedure TSurfaceCairo.MoveTo(X, Y: Float);
begin
  if HandleAvailable then
    cairo_move_to(FCairo, X, Y);
end;

procedure TSurfaceCairo.LineTo(X, Y: Float);
begin
  if HandleAvailable then
    cairo_line_to(FCairo, X, Y);
end;

procedure TSurfaceCairo.ArcTo(const Rect: TRectF; BeginAngle, EndAngle: Float);
const
  AngleDelta = Pi / 2;
begin
  if not HandleAvailable then
    Exit;
  if Rect.Empty then
    Exit;
  cairo_save(FCairo);
  cairo_translate(FCairo, Rect.X + Rect.Width / 2,
    Rect.Y + Rect.Height / 2);
  cairo_scale(FCairo, Rect.Width / 2, Rect.Height / 2);
  cairo_arc(FCairo, 0, 0, 1, BeginAngle - AngleDelta, EndAngle - AngleDelta);
  cairo_restore(FCairo);
end;

procedure TSurfaceCairo.CurveTo(X, Y: Float; const C1, C2: TPointF);
begin
  if HandleAvailable then
    cairo_curve_to(FCairo, C1.X, C1.Y, C2.X, C2.Y, X, Y);
end;

procedure TSurfaceCairo.Ellipse(const Rect: TRectF);
begin
  if not HandleAvailable then
    Exit;
  if Rect.Empty then
    Exit;
  cairo_save(FCairo);
  cairo_new_sub_path(FCairo);
  cairo_translate(FCairo, Rect.X + Rect.Width / 2,
    Rect.Y + Rect.Height / 2);
  cairo_scale(FCairo, Rect.Width / 2, Rect.Height / 2);
  cairo_arc(FCairo, 0, 0, 1, 0, PI * 2);
  cairo_close_path(FCairo);
  cairo_restore(FCairo);
end;

procedure TSurfaceCairo.Rectangle(const Rect: TRectF);
begin
  if not HandleAvailable then
    Exit;
  cairo_new_sub_path(FCairo);
  cairo_rectangle(FCairo, Rect.X, Rect.Y, Rect.Width, Rect.Height);
  cairo_close_path(FCairo);
end;

procedure TSurfaceCairo.RoundRectangle(const Rect: TRectF; Radius: Float);
var
  D: Double;
begin
  if Rect.Empty or (Radius <= 0) then
    Exit;
  if Rect.Empty then
    Exit;
  if HandleAvailable then
  begin
    if Rect.Width < Radius * 2 then
      Radius := Rect.Width / 2;
    if Rect.Height < Radius * 2 then
      Radius := Rect.Height / 2;
    D := Radius * 2;
    D := PI / 180;
    cairo_new_sub_path(FCairo);
    with Rect do
    begin
      cairo_arc(FCairo, X + Width - Radius, Y + Radius,
        Radius, -90 * D, 0);
      cairo_arc(FCairo, X + Width - Radius, Y + Height - Radius,
        Radius, 0, 90 * D);
      cairo_arc(FCairo, X + Radius, Y + Height - Radius,
        Radius, 90 * D, 180 * D);
      cairo_arc(FCairo, X + Radius, Y + Radius,
        Radius, 180 * D, 270 * D);
    end;
    cairo_close_path(FCairo);
  end;
end;

procedure TSurfaceCairo.SetFont(F: IFont);
var
  C: TFontCairo;
  D: PPangoFontDescription;
begin
  C := F as TFontCairo;
  D := C.FDesc;
  pango_layout_set_font_description(FLayout, D);
end;

function TSurfaceCairo.TextSize(Font: IFont; const Text: string): TPointF;
var
  W, H: LongInt;
begin
  Result := TPointF.Create(0, 0);
  if not LayoutAvailable then
    Exit;
  if Text = '' then
    Exit;
  SetFont(Font);
  pango_layout_set_text(FLayout, PAnsiChar(Text), -1);
  pango_layout_set_ellipsize(FLayout, PANGO_ELLIPSIZE_NONE);
  pango_layout_set_width(FLayout, -1);
  pango_layout_set_height(FLayout, -1);
  pango_layout_get_pixel_size(FLayout, @W, @H);
  Result := TPointF.Create(W, H);
end;

function TSurfaceCairo.TextHeight(Font: IFont; const Text: string; Width: Float): Float;
var
  W, H: LongInt;
begin
  Result := 0;
  if not LayoutAvailable then
    Exit;
  if Text = '' then
    Exit;
  if Width < 1 then
    Exit;
  SetFont(Font);
  pango_layout_set_text(FLayout, PAnsiChar(Text), -1);
  pango_layout_set_ellipsize(FLayout, PANGO_ELLIPSIZE_NONE);
  pango_layout_set_width(FLayout, Round(Width) * PANGO_SCALE);
  pango_layout_set_height(FLayout, -1);
  pango_layout_get_pixel_size(FLayout, @W, @H);
  Result := H;
end;

procedure TSurfaceCairo.TextOut(Font: IFont; const Text: string; const Rect: TRectF;
  Direction: TDirection; Immediate: Boolean = True);
const
  FontOptions: array[TFontQuality] of TCairoAntiAlias = (
    CAIRO_ANTIALIAS_DEFAULT, CAIRO_ANTIALIAS_FAST, CAIRO_ANTIALIAS_GOOD,
    CAIRO_ANTIALIAS_NONE, CAIRO_ANTIALIAS_GRAY, CAIRO_ANTIALIAS_SUBPIXEL,
    CAIRO_ANTIALIAS_SUBPIXEL);
var
  R: TRectI;
  W, H: LongInt;
  M: TCairoMatrix;
  C: TColorF;
  Options: PCairoFontOptions;
begin
  if SurfaceOptions.ErrorCorrection or Immediate then
    Path.Remove;
  if not LayoutAvailable then
    Exit;
  if Text = '' then
    Exit;
  R := TRectI(Rect);
  if Rect.Empty then
    Exit;
  Path.Add;
  SetFont(Font);
  pango_layout_set_text(FLayout, PAnsiChar(Text), -1);
  pango_layout_set_width(FLayout, R.Width * PANGO_SCALE);
  pango_layout_set_height(FLayout, R.Height * PANGO_SCALE);
  { Horizontal alignment }
  case Direction of
    drLeft, drWrap, drFlow:
      pango_layout_set_alignment(FLayout, PANGO_ALIGN_LEFT);
    drUp, drDown, drCenter, drFill:
      pango_layout_set_alignment(FLayout, PANGO_ALIGN_CENTER);
  else
    pango_layout_set_alignment(FLayout, PANGO_ALIGN_RIGHT);
  end;
  { Ellipses }
  case Direction of
    drLeft, drUp, drRight, drDown, drCenter:
      begin
        pango_layout_set_height(FLayout, -1);
        pango_layout_set_ellipsize(FLayout, PANGO_ELLIPSIZE_END);
      end;
  else
    pango_layout_set_height(FLayout, High(Word));
    pango_layout_set_ellipsize(FLayout, PANGO_ELLIPSIZE_NONE);
  end;
  pango_cairo_update_layout(FCairo, FLayout);
  pango_layout_get_pixel_size(FLayout, @W, @H);
  cairo_get_matrix(FCairo, @M);
  { Placement }
  case Direction of
    drUp, drWrap, drFlow:
      cairo_translate(FCairo, R.X + Delta, R.Y + Delta);
    drLeft, drRight, drCenter, drFill:
      cairo_translate(FCairo, R.X + Delta, R.Y + (R.Height - H) div 2 + Delta);
  else
    cairo_translate(FCairo, R.X + Delta, Rect.Y + R.Height - H + Delta);
  end;
  pango_cairo_update_layout(FCairo, FLayout);
  Options := cairo_font_options_create;
  cairo_font_options_set_antialias(Options, FontOptions[Font.Quality]);
  cairo_font_options_set_hint_style(Options, CAIRO_HINT_STYLE_SLIGHT);
  cairo_set_font_options(FCairo, Options);
  pango_cairo_context_set_font_options(pango_layout_get_context(FLayout),
    Options);
  cairo_move_to(FCairo, 0, 0);
  if SurfaceOptions.ErrorCorrection or Immediate then
  begin
    C := TColorF(Font.Color);
    cairo_set_source_rgba(FCairo, C.Red, C.Green, C.Blue, C.Alpha);
    pango_cairo_show_layout(FCairo, FLayout);
  end
  else
    pango_cairo_layout_path(FCairo, FLayout);
  cairo_set_matrix(FCairo, @M);
  cairo_font_options_destroy(Options);
end;

procedure TSurfaceCairo.Stroke(Pen: IPen; Preserve: Boolean = False);
begin
  if not HandleAvailable then Exit;
  SetSource(Pen);
  if Preserve then
    cairo_stroke_preserve(FCairo)
  else
    cairo_stroke(FCairo);
end;

procedure TSurfaceCairo.Fill(Brush: IBrush; Preserve: Boolean = False);
begin
  if not HandleAvailable then Exit;
  SetSource(Brush);
  if Brush.Opacity = 0 then
  begin
    if not Preserve then
      cairo_new_path(FCairo);
  end
  else if Brush.Opacity < $FF then
  begin
    cairo_save(FCairo);
    if Preserve then
      cairo_clip_preserve(FCairo)
    else
      cairo_clip(FCairo);
    cairo_paint_with_alpha(FCairo, Brush.Opacity / $FF);
    cairo_restore(FCairo);
  end
  else if Preserve then
    cairo_fill_preserve(FCairo)
  else
    cairo_fill(FCairo);
end;

procedure TSurfaceCairo.StrokeRect(Pen: IPen; const Rect: TRectF);
var
  Odd: Boolean;
  R: TRectI;
begin
  if not HandleAvailable then
    Exit;
  if Rect.Empty then
    Exit;
  cairo_new_path(FCairo);
  Odd := Round(Pen.Width) mod 2 = 1;
  if Odd then
    Matrix.Translate(Delta, Delta);
  R := TRectI(Rect);
  Dec(R.Width);
  Dec(R.Height);
  Rectangle(R);
  Stroke(Pen);
  if Odd then
    Matrix.Translate(-Delta, -Delta);
end;

procedure TSurfaceCairo.FillRect(Brush: IBrush; const Rect: TRectF);
var
  R: TRectI;
begin
  if not HandleAvailable then
    Exit;
  if Rect.Empty then
    Exit;
  cairo_new_path(FCairo);
  R := TRectI(Rect);
  Rectangle(R);
  Fill(Brush);
end;

procedure TSurfaceCairo.StrokeRoundRect(Pen: IPen; const Rect: TRectF; Radius: Float);
var
  Odd: Boolean;
  R: TRectI;
begin
  if not HandleAvailable then
    Exit;
  if Rect.Empty then
    Exit;
  cairo_new_path(FCairo);
  Odd := Round(Pen.Width) mod 2 = 1;
  if Odd then
    Matrix.Translate(Delta, Delta);
  R := TRectI(Rect);
  Dec(R.Width);
  Dec(R.Height);
  RoundRectangle(R, Radius);
  Stroke(Pen);
  if Odd then
    Matrix.Translate(-Delta, -Delta);
end;

procedure TSurfaceCairo.FillRoundRect(Brush: IBrush; const Rect: TRectF; Radius: Float);
var
  R: TRectI;
begin
  if not HandleAvailable then
    Exit;
  if Rect.Empty then
    Exit;
  cairo_new_path(FCairo);
  R := TRectI(Rect);
  RoundRectangle(R, Radius);
  Fill(Brush);
end;

{ TClipSurfaceCairo }

constructor TClipSurfaceCairo.Create(Drawable: PGdkDrawable; const Clip: TRectI);
begin
  inherited Create;
  FCairo := gdk_cairo_create(Drawable);
  FMatrix := TMatrixCairo.Create(Clip.X, Clip.Y);
  FPath := TSurfacePathClipCairo.Create(FCairo, Drawable, Clip);
  FPathCairo := FPath as TSurfacePathCairo;
end;

{ TControlSurfaceCairo }

constructor TControlSurfaceCairo.Create(Control: TWinControl);
begin
  inherited Create;
  FControl := Control;
end;

function TControlSurfaceCairo.HandleAvailable: Boolean;
var
  Widget: PGtkWidget;
  Region: PGdkRegion;
  { R: TGdkRectangle; alternate method, see below }
begin
  if FCairo = nil then
  begin
    if FDrawable = nil then
    begin
      Widget := GTK_WIDGET(Pointer(FControl.Handle));
      Widget := GetFixedWidget(Widget);
      FDrawable := GetControlWindow(Widget);
    end;
    Region := gdk_drawable_get_clip_region(FDrawable);
    gdk_window_begin_paint_region(FDrawable, Region);
    gdk_region_destroy(Region);
    { Alternate method:
    R.x := 0;
    R.y := 0;
    R.width := FControl.ClientWidth;
    R.height := FControl.ClientHeight;
    gdk_window_begin_paint_rect(FDrawable, @R);}
    FCairo := gdk_cairo_create(FDrawable);
    FMatrix.Identity;
  end;
  Result := inherited HandleAvailable;
end;

procedure TControlSurfaceCairo.Flush;
begin
  inherited Flush;
  if FCairo = nil then
    Exit;
  cairo_destroy(FCairo);
  FCairo := nil;
  gdk_window_end_paint(FDrawable);
  { Without the call below everything is slow }
  gdk_flush;
end;

{ TBitmapSurfaceCairo }

constructor TBitmapSurfaceCairo.Create(Bitmap: TBitmapCairo);
begin
  inherited Create;
  FBitmap := Bitmap;
end;

function TBitmapSurfaceCairo.HandleAvailable: Boolean;
var
  W, H: Integer;
  B: PByte;
  S: PCairoSurface;
begin
  if FCairo = nil then
    if (FBitmap <> nil) and (not FBitmap.Empty) then
    begin
      W := FBitmap.Width;
      H := FBitmap.Height;
      B := PByte(FBitmap.Pixels);
      S := cairo_image_surface_create_for_data(B, CAIRO_FORMAT_ARGB32,
        W, H, W * SizeOf(TColorB));
      FCairo := cairo_create(S);
      cairo_surface_destroy(S);
       FDirty := False;
    end;
  if FCairo = nil then
    FDirty := False;
  if FDirty then
  begin
    S := cairo_get_target(FCairo);
    cairo_surface_mark_dirty(S);
    FDirty := False;
  end;
  Result := inherited HandleAvailable;
end;

{ TBitmapCairo }

constructor TBitmapCairo.Create(B: PGdkPixbuf = nil);
begin
  inherited Create;
  SetBuffer(B);
end;

destructor TBitmapCairo.Destroy;
begin
  if FSurfaceCairo <> nil then
  begin
    FSurfaceCairo.HandleRelease;
    FSurfaceCairo.FBitmap := nil;
  end;
  SetBuffer(nil);
  inherited Destroy;
end;

procedure TBitmapCairo.Flush;
begin
  if FSurface <> nil then
    FSurface.Flush;
end;

procedure TBitmapCairo.FlipPixels;
var
  P: PBGRA;
  B: Byte;
  I: Integer;
begin
  if Empty then
    Exit;
  Flush;
  P := Pixels;
  I := Width * Height;
  while I > 0 do
  begin
    B := P.Red;
    P.Red := P.Blue;
    P.Blue := B;
    Inc(P);
    Dec(I);
  end;
end;

procedure TBitmapCairo.Premultiply;
var
  Found: Boolean;
  Ratio: Single;
  A, B: PBGRA;
  I, J: Integer;
begin
  if Empty then
    Exit;
  if not SurfaceOptions.UsePremultiply then
    Exit;
  Found := False;
  A := Pointer(gdk_pixbuf_get_pixels(FBuffer));
  B := A;
  I := Width * Height;
  J := I;
  while J > 0 do
  begin
    Found := (B.Blue > B.Alpha) or (B.Green > B.Alpha) or (B.Red > B.Alpha);
    if Found then
      Break;
    Inc(B);
    Dec(J);
  end;
  if not Found then
    Exit;
  B := A;
  J := I;
  while J > 0 do
  begin
    Ratio := B.Alpha / $FF;
    B.Blue := Round(B.Blue * Ratio);
    B.Green := Round(B.Green * Ratio);
    B.Red := Round(B.Red * Ratio);
    Inc(B);
    Dec(J);
  end;
end;

procedure TBitmapCairo.SetBuffer(Value: PGdkPixbuf);
begin
  if Value = FBuffer then
    Exit;
  if FBuffer <> nil then
    g_object_unref(FBuffer);
  FBuffer := Value;
  if FSurfaceCairo <> nil then
    FSurfaceCairo.HandleRelease;
end;

function TBitmapCairo.Clone: IBitmap;
begin
  if FBuffer = nil then
    Result := TBitmapCairo.Create
  else
    Result := TBitmapCairo.Create(gdk_pixbuf_copy(FBuffer));
  (Result as TBitmapCairo).FFormat := FFormat;
end;

function TBitmapCairo.GetEmpty: Boolean;
begin
  Result := FBuffer = nil;
end;

function TBitmapCairo.GetSurface: ISurface;
begin
  if FSurface = nil then
  begin
    FSurface := TBitmapSurfaceCairo.Create(Self);
    FSurfaceCairo := FSurface as TBitmapSurfaceCairo;
  end;
  Result := FSurface;
end;

function TBitmapCairo.GetClientRect: TRectI;
begin
  Result := TRectI.Create(Width, Height);
end;

function TBitmapCairo.GetFormat: TImageFormat;
begin
  Result := FFormat;
end;

procedure TBitmapCairo.SetFormat(Value: TImageFormat);
begin
  FFormat := Value;
end;

function TBitmapCairo.GetHeight: Integer;
begin
  if FBuffer = nil then
    Result := 0
  else
    Result := gdk_pixbuf_get_height(FBuffer);
end;

function TBitmapCairo.GetWidth: Integer;
begin
  if FBuffer = nil then
    Result := 0
  else
    Result := gdk_pixbuf_get_width(FBuffer);
end;

function TBitmapCairo.GetPixels: PPixel;
begin
  if FBuffer = nil then
    Result := nil
  else
  begin
    Flush;
    Result := Pointer(gdk_pixbuf_get_pixels(FBuffer));
    if FSurfaceCairo <> nil then
      FSurfaceCairo.FDirty := True;
  end;
end;

procedure TBitmapCairo.Clear;
begin
  SetBuffer(nil);
end;

function TBitmapCairo.Resample(Width, Height: Integer; Quality: TResampleQuality = rqNormal): IBitmap;
const
  Sampling: array[TResampleQuality] of TGdkInterpType =
    (GDK_INTERP_NEAREST, GDK_INTERP_BILINEAR, GDK_INTERP_HYPER);
var
  B: PGdkPixbuf;
begin
  if Empty then
    Exit(nil);
  Flush;
  B := gdk_pixbuf_scale_simple(FBuffer, Width, Height, Sampling[Quality]);
  if B = nil then
    Exit(nil);
  Result := TBitmapCairo.Create(B);
end;

procedure TBitmapCairo.SetSize(Width, Height: Integer);
var
  NewPixels: Boolean;
  W, H: Integer;
  P: PPixel;
  B: PGdkPixbuf;
begin
  NewPixels := False;
  if (Width < 1) or (Height < 1) then
    B := nil
  else if FBuffer = nil then
  begin
    B := gdk_pixbuf_new(GDK_COLORSPACE_RGB, True, 8, Width, Height);
    NewPixels := True;
  end
  else if (Width <> GetWidth) or (Height <> GetHeight) then
  begin
    B := gdk_pixbuf_new(GDK_COLORSPACE_RGB, True, 8, Width, Height);
    NewPixels := True;
  end
  else
    Exit;
  SetBuffer(B);
  if NewPixels then
  begin
    P := Pixels;
    for W := 1 to Width do
      for H := 1 to Height do
      begin
        P^ := clTransparent;
        Inc(P);
      end;
  end;
end;

procedure TBitmapCairo.LoadFromFile(const FileName: string);
var
  B, C: PGdkPixbuf;
begin
  B := gdk_pixbuf_new_from_file(PAnsiChar(FileName), nil);
  if B <> nil then
  begin
    FFormat := StrToImageFormat(ExtractFileExt(FileName));
    if gdk_pixbuf_get_has_alpha(B) then
      SetBuffer(B)
    else
    begin
      C := gdk_pixbuf_add_alpha(B, False, 0, 0, 0);
      g_object_unref(B);
      SetBuffer(C);
    end;
    FlipPixels;
    Premultiply;
  end;
end;

procedure TBitmapCairo.LoadFromStream(Stream: TStream);
const
  DataSize = 1024 * 16;
var
  Loader: PGdkPixbufLoader;
  Data: PByte;
  B, C: PGdkPixbuf;
  F: PGdkPixbufFormat;
  I: LongInt;
begin
  Loader := gdk_pixbuf_loader_new;
  Data := GetMem(DataSize);
  try
    repeat
      I := Stream.Read(Data^, DataSize);
      if not gdk_pixbuf_loader_write(Loader, Data, I, nil) then
        Exit;
    until I < DataSize;
    if not gdk_pixbuf_loader_close(Loader, nil) then
      Exit;
    B := gdk_pixbuf_loader_get_pixbuf(Loader);
    if B <> nil then
    begin
      g_object_ref(B);
      if gdk_pixbuf_get_has_alpha(B) then
        SetBuffer(B)
      else
      begin
        C := gdk_pixbuf_add_alpha(B, False, 0, 0, 0);
        g_object_unref(B);
        SetBuffer(C);
      end;
      F := gdk_pixbuf_loader_get_format(Loader);
      if F <> nil then
        FFormat := StrToImageFormat(F.name);
      FlipPixels;
      Premultiply;
    end;
  finally
    FreeMem(Data);
    g_object_unref(Loader);
  end;
end;

procedure TBitmapCairo.SaveToFile(const FileName: string);
var
  S: string;
begin
  if not Empty then
  begin
    S := ExtractFileExt(FileName);
    FFormat := StrToImageFormat(S);
    S := ImageFormatToStr(FFormat);
    FlipPixels;
    gdk_pixbuf_save(FBuffer, PChar(FileName), PChar(S), nil);
    FlipPixels;
  end;
end;

function SaveCallback(buffer: PGChar; count: GSize; error: PPGError;
  data: GPointer): GBoolean; cdecl;
var
  Stream: TStream absolute data;
begin
  Stream.Write(buffer^, count);
  Result := True;
end;

procedure TBitmapCairo.SaveToStream(Stream: TStream);
var
  S: string;
begin
  if not Empty then
  begin
    { For some unknow reason this WriteLn causes the IDE to realize property data }
    if not (FFormat in [fmBmp, fmJpeg, fmPng, fmTiff]) then
      FFormat := fmPng;
    S := ImageFormatToStr(FFormat);
    FlipPixels;
    gdk_pixbuf_save_to_callback(FBuffer, SaveCallback, Stream, PChar(S), nil, nil);
    FlipPixels;
  end;
end;

{ Surface object creation routines }

function NewMatrixCairo: IMatrix;
begin
  Result := TMatrixCairo.Create;
end;

function NewPenCairo(Brush: IBrush; Width: Float = 1): IPen;
begin
  Result := TPenCairo.Create(Brush, Width);
end;

function NewPenCairo(Color: TColorB; Width: Float = 1): IPen;
begin
  Result := TPenCairo.Create(Color, Width);
end;

function NewSolidBrushCairo(Color: TColorB): ISolidBrush;
begin
  Result := TSolidBrushCairo.Create(Color);
end;

function NewLinearGradientBrushCairo(X1, Y1, X2, Y2: Float): ILinearGradientBrush;
begin
  Result := TLinearGradientBrushCairo.Create(X1, Y1, X2, Y2);
end;

function NewLinearGradientBrushCairo(const A, B: TPointF): ILinearGradientBrush;
begin
  Result := TLinearGradientBrushCairo.Create(A.X, A.Y, B.X, B.Y);
end;

function NewRadialGradientBrushCairo(const Rect: TRectF): IRadialGradientBrush;
begin
  Result := TRadialGradientBrushCairo.Create(Rect);
end;

function NewBitmapBrushCairo(Bitmap: IBitmap): IBitmapBrush;
begin
  Result := TBitmapBrushCairo.Create(Bitmap);
end;

function NewFontCairo(const FontName: string; FontSize: Integer): IFont;
begin
  Result := TFontCairo.Create(FontName, FontSize);
end;

function NewFontCairo(Font: TFont = nil): IFont;
begin
  Result := TFontCairo.Create(Font);
end;

function CanvasToDrawable(Canvas: TCanvas; out Rect: TRectI): Pointer;
var
  Root: PGdkWindow;
  C: TControl;
  R: TGdkRectangle;
begin
  Result := nil;
  Rect := TRectI.Create;
  if Canvas = nil then
  begin
    Root := gdk_get_default_root_window;
    gdk_drawable_get_size(PGdkDrawable(Root), @Rect.Width, @Rect.Height);
    Result := Root;
  end
  else if TObject(Canvas.Handle) is TGtkDeviceContext then
  begin
    if Canvas is TControlCanvas then
    begin
      C := TControlCanvas(Canvas).Control;
      if C is TGraphicControl then
        Rect := C.BoundsRect
      else
        Rect := C.ClientRect;
    end
    else
    begin
      R := TGtkDeviceContext(Canvas.Handle).ClipRect;
      Rect.Width := R.width;
      Rect.Height := R.height;
    end;
    Result := TGtkDeviceContext(Canvas.Handle).Drawable;
  end;
end;

function NewSurfaceCairo(Canvas: TCanvas): ISurface;
var
  D: PGDKDrawable;
  R: TRectI;
begin
  Result := nil;
  D := CanvasToDrawable(Canvas, R);
  if D = nil then
    Exit;
  Result := TClipSurfaceCairo.Create(D, R);
end;

function NewSurfaceCairo(Control: TWinControl): ISurface;
begin
  Result := TControlSurfaceCairo.Create(Control);
end;

function NewBitmapCairo(BitmapBuffer: Pointer): IBitmap;
begin
  Result := TBitmapCairo.Create(PGdkPixmap(BitmapBuffer));
end;

function NewBitmapCairo(Width, Height: Integer): IBitmap;
begin
  Result := TBitmapCairo.Create(nil);
  Result.SetSize(Width, Height);
end;

{ TSplashCairo }

type
  TSplashCairo = class(TInterfacedObject, ISplash)
    private
    FClipped: Boolean;
    FBitmap: IBitmap;
    FWidget: PGtkWidget;
    FOpacity: Byte;
    FVisible: Boolean;
    FSize: TPointI;
    procedure Resize;
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

procedure gdk_window_input_shape_combine_mask (window: PGdkWindow;
  mask: PGdkBitmap; x, y: GInt); cdecl; external gdklib;
function gtk_widget_get_window(widget: PGtkWidget): PGdkWindow; cdecl; external gtklib;

procedure SplashScreenChanged(widget: PGtkWidget; old_screen: PGdkScreen;
    userdata: GPointer); cdecl;
var
  Screen: PGdkScreen;
  Colormap: PGdkColormap;
begin
  Screen := gtk_widget_get_screen(widget);
  Colormap := gdk_screen_get_rgba_colormap(Screen);
    gtk_widget_set_colormap(widget, Colormap);
end;

procedure Clip(Splash: TSplashCairo; Widget: PGtkWidget);
var
  Window: PGdkWindow;
  Bitmap: PGdkBitmap;
  Bits: Integer;
begin
  if Splash.FClipped then
    Exit;
  Splash.FClipped := True;
  Window := gtk_widget_get_window(Widget);
  Bits := 0;
  Bitmap := gdk_bitmap_create_from_data(Widget.window, Pointer(@Bits), 1, 1);
  gdk_window_input_shape_combine_mask(Window, Bitmap, 0, 0);
  g_object_unref(Bitmap);
end;

procedure SplashExpose(widget: PGtkWidget; event: PGdkEventExpose;
    userdata: GPointer); cdecl;
var
  Splash: TSplashCairo absolute userdata;
  BitmapSurface: TBitmapSurfaceCairo;
  Surface: PCairoSurface;
  Pattern: PCairoPattern;
  Dest: PCairo;
begin
  Clip(Splash, widget);
  BitmapSurface := Splash.FBitmap.Surface as TBitmapSurfaceCairo;
  if BitmapSurface.HandleAvailable then
  begin
    Surface := cairo_get_target(BitmapSurface.FCairo);
    Pattern := cairo_pattern_create_for_surface(Surface);
    Dest := gdk_cairo_create(widget.window);
    cairo_set_operator(Dest, CAIRO_OPERATOR_SOURCE);
    cairo_set_source(Dest, Pattern);
    cairo_paint(Dest);
    cairo_destroy(Dest);
    cairo_pattern_destroy(Pattern);
  end;
end;

constructor TSplashCairo.Create;
begin
  inherited Create;
  FBitmap := TBitmapCairo.Create;
  FOpacity := $FF;
  FWidget := gtk_window_new(GTK_WINDOW_POPUP);
  // gtk_window_set_accept_focus(PGtkWindow(FWidget), True);
  gtk_window_set_type_hint(PGtkWindow(FWidget), GDK_WINDOW_TYPE_HINT_SPLASHSCREEN);
  gtk_widget_set_app_paintable(FWidget, True);
  g_signal_connect(G_OBJECT(FWidget), 'expose-event',
    G_CALLBACK(@SplashExpose), Pointer(Self));
  g_signal_connect(G_OBJECT(FWidget), 'screen-changed',
    G_CALLBACK(@SplashScreenChanged), nil);
  SplashScreenChanged(FWidget, nil, nil);
end;

destructor TSplashCairo.Destroy;
begin
  gtk_widget_destroy(FWidget);
  inherited Destroy;
end;

procedure TSplashCairo.Resize;
begin
  if (FBitmap.Width <> FSize.X) or (FBitmap.Height <> FSize.Y) then
  begin
    gtk_window_resize(GTK_WINDOW(FWidget), FBitmap.Width, FBitmap.Height);
    FSize.X := FBitmap.Width;
    FSize.Y := FBitmap.Height;
  end;
end;

function TSplashCairo.GetBitmap: IBitmap;
begin
    Result := FBitmap;
end;

function TSplashCairo.GetOpacity: Byte;
begin
  Result := FOpacity;
end;

procedure TSplashCairo.SetOpacity(Value: Byte);
begin
  if Value <> FOpacity then
  begin
    gtk_window_set_opacity(GTK_WINDOW(FWidget), Value / $FF);
    FOpacity := Value;
  end;
end;

function TSplashCairo.GetVisible: Boolean;
begin
  Result := FVisible;
end;

procedure TSplashCairo.SetVisible(Value: Boolean);
begin
  Value := Value and (not FBitmap.Empty);
  if Value <> FVisible then
  begin
    FVisible := Value;
    if FVisible then
    begin
      Resize;
      gtk_widget_show_all(FWidget);
    end
    else
      gtk_widget_hide_all(FWidget);
  end;
end;

function TSplashCairo.GetHandle: IntPtr;
begin
  Result := IntPtr(FWidget);
end;

procedure TSplashCairo.Move(X, Y: Integer);
begin
  gtk_window_move(GTK_WINDOW(FWidget), X, Y);
end;

procedure TSplashCairo.Update;
begin
  if FBitmap.Empty then
    SetVisible(False)
  else if FVisible then
  begin
    Resize;
    gtk_widget_queue_draw(FWidget);
  end;
end;

function NewSplashCairo: ISplash;
begin
  Result := TSplashCairo.Create;
end;
{$endif}

end.

