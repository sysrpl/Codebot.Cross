(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified September 2013                             *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.graphics.windows.Surfacegdiplus.txt> }
unit Codebot.Graphics.Windows.SurfaceGdiPlus;

{$i codebot.inc}

interface

{$ifdef windows}
uses
  SysUtils, Classes, Graphics, Controls, Windows,
  Codebot.System,
  Codebot.Collections,
  Codebot.Graphics.Types,
  Codebot.Graphics.Windows.InterfacedBitmap,
  Codebot.Interop.Windows.GdiPlus;

{ New object routines }

function NewMatrixGdi: IMatrix;
function NewPenGdi(Brush: IBrush; Width: Float = 1): IPen; overload;
function NewPenGdi(Color: TColorB; Width: Float = 1): IPen; overload;
function NewSolidBrushGdi(Color: TColorB): ISolidBrush;
function NewBitmapBrushGdi(Bitmap: IBitmap): IBitmapBrush;
function NewLinearGradientBrushGdi(X1, Y1, X2, Y2: Float): ILinearGradientBrush; overload;
function NewLinearGradientBrushGdi(const A, B: TPointF): ILinearGradientBrush; overload;
function NewRadialGradientBrushGdi(const Rect: TRectF): IRadialGradientBrush;
function NewFontGdi(Font: TFont): IFont;
function NewSurfaceGdi(Canvas: TCanvas): ISurface; overload;
function NewSurfaceGdi(Control: TWinControl): ISurface; overload;
function NewBitmapGdi(Width, Height: Integer): IBitmap;
{$endif}

implementation

{$ifdef windows}
{ Implementation types }
const
  PointsPerInch = 72;
  DeviceIndependentPixels = 96;

type
  TMatrixGdi = class;
  TPenGdi = class;
  TBrushGdi = class;
  TSolidBrushGdi = class;
  TRadialGradientBrushGdi = class;
  TFontGdi = class;
  TPathGdi = class;
  TSurfacePathGdi = class;
  TSurfaceGdi = class;
  TBitmapGdi = class;

{ TMatrixGdi }

  TMatrixGdi = class(TInterfacedObject, IMatrix)
  private
    FMatrix: IGdiMatrix;
    FChanged: Boolean;
    FIsIdentity: Boolean;
  public
    constructor Create(M: IGdiMatrix = nil);
    function Clone: IMatrix;
    procedure Identity;
    procedure Multiply(M: IMatrix);
    procedure Translate(X, Y: Float);
    procedure Scale(X, Y: Float);
    procedure Rotate(Angle: Float);
    function Transform(Point: TPointF): TPointF;
  end;

{ TPenGdi }

  TPenGdi = class(TInterfacedObject, IPen)
  private
    FPen: IGdiPen;
    FBrush: IBrush;
    FColor: TColorB;
  protected
    function HandleAvailable: Boolean;
  public
    constructor Create(Color: TColorB; Width: Float);
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

{ TBrushGdi }

  TBrushGdi = class(TInterfacedObject, IBrush)
  private
    FBrush: IGdiBrush;
    FMatrix: IMatrix;
    FOpacity: Byte;
    function Matrix: TMatrixGdi;
  protected
    function HandleAvailable: Boolean; virtual;
  public
    constructor Create;
    function GetMatrix: IMatrix;
    procedure SetMatrix(Value: IMatrix);
    function GetOpacity: Byte;
    procedure SetOpacity(Value: Byte); virtual;
  end;

{ TSolidBrushGdi }

  TSolidBrushGdi = class(TBrushGdi, ISolidBrush)
  private
    FColor: TColorB;
  public
    constructor Create(Color: TColorB);
    function GetColor: TColorB;
    procedure SetColor(Value: TColorB);
    procedure SetOpacity(Value: Byte); override;
  end;

{ TGradientStop }

  TGradientStop = record
    Color: TColorB;
    Offset: Float;
  end;

  TGradientStops = TArrayList<TGradientStop>;
  TGradientColors = TArrayList<TGdiArgb>;
  TGradientOffsets = TArrayList<Float>;

{ TGradientBrushGdi }

  TGradientBrushGdi = class(TBrushGdi, IGradientBrush)
  private
    FOrigin: TPointF;
    FStops: TGradientStops;
    FWrap: TGradientWrap;
    FReverse: Boolean;
  protected
    function HandleAvailable: Boolean; override;
  public
    constructor Create(const Origin: TPointF);
    procedure SetOpacity(Value: Byte); override;
    function GetWrap: TGradientWrap;
    procedure SetWrap(Value: TGradientWrap);
    procedure AddStop(Color: TColorB; Offset: Float);
  end;

{ TLinearGradientBrushGdi }

  TLinearGradientBrushGdi = class(TGradientBrushGdi, ILinearGradientBrush)
  private
    FPoint: TPointF;
    FDist: Float;
    FGradient: IGdiLinearGradientBrush;
  protected
    function HandleAvailable: Boolean; override;
  public
    constructor Create(const A, B: TPointF);
  end;

{ TRadialGradientBrushGdi }

  TRadialGradientBrushGdi = class(TGradientBrushGdi, IRadialGradientBrush)
  private
    FRect: TRectF;
    FGradient: IGdiPathGradientBrush;
  protected
    function HandleAvailable: Boolean; override;
  public
    constructor Create(const Rect: TRectF);
  end;

{ TBitmapBrushGdi }

  TBitmapBrushGdi = class(TBrushGdi, IBitmapBrush)
  private
    FBitmap: IBitmap;
    FTextureBrush: IGdiTextureBrush;
  protected
    function HandleAvailable: Boolean; override;
  public
    constructor Create(Bitmap: IBitmap);
    procedure SetOpacity(Value: Byte); override;
 end;

{ TFontGdi }

  TFontGdi = class(TInterfacedObject, IFont)
  private
    FFont: IGdiFont;
    FFontObject: TFont;
    FColor: TColorB;
    FQuality: TFontQuality;
    function Font: IGdiFont;
  public
    constructor Create(F: TFont);
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
    property Name: string read GetName;
    property Color: TColorB read GetColor write SetColor;
    property Quality: TFontQuality read GetQuality write SetQuality;
    property Style: TFontStyles read GetStyle write SetStyle;
    property Size: Float read GetSize write SetSize;
  end;

{ TPathGdi }

  TPathGdi =  class(TInterfacedObject, IPathData)
  private
    FPath: IGdiGraphicsPath;
  public
    constructor Create(Path: IGdiGraphicsPath);
  end;

{ TSurfacePathGdi }

  TSurfacePathGdi = class(TInterfacedObject, IPath)
  private
    FSurface: TSurfaceGdi;
    FClip: IGdiGraphicsPath;
    FData: IGdiGraphicsPath;
    FFigure: IGdiGraphicsPath;
    FOrigin: TPointF;
    FClosed: Boolean;
  protected
    function HandleAvailable: Boolean;
    procedure HandleRelease;
  public
    constructor Create(Surface: TSurfaceGdi);
    function Clone: IPathData;
    procedure Add; overload;
    procedure Add(P: IGdiGraphicsPath); overload;
    procedure Open(X, Y: Float); overload;
    procedure Open; overload;
    procedure Remove;
    procedure Close;
    procedure Join(Path: IPathData);
    procedure Clip;
    procedure Unclip;
  end;

{ TSurfaceStateGdi }

  TSurfaceStateGdi = class
  public
    Origin: TPointF;
    Clip: IGdiRegion;
    Data: IGdiGraphicsPath;
    Matrix: IMatrix;
    constructor Create(C: TSurfaceGdi);
    procedure Restore(C: TSurfaceGdi);
  end;

{ TODO: Consider breaking TSurfaceGdi into a few more classes. One thing to note
  is that the Gdi implementation is totally unsuitable for animation and as such
  users shouldn't use a persitent version of it i.e. NewSurface(SomeControl) }

{ TSurfaceGdi }

  TSurfaceGdi = class(TInterfacedObject, ISurface)
  private
    FGraphics: IGdiGraphics;
    FPath: IPath;
    FMatrix: IMatrix;
    FStateStack: IList<TSurfaceStateGdi>;
    { Double buffering feature enabled by SurfaceOptions.SoftwareBuffering }
    FBuffer: TFastBitmap;
    FWnd: HWND;
    FDest: HDC;
    function Path: TSurfacePathGdi;
    function Matrix: TMatrixGdi;
  protected
    procedure InitializeDefaults;
    function HandleAvailable: Boolean; virtual;
    procedure HandleRelease;
    function SourceMap: PFastBitmap; virtual;
  public
    constructor Create(G: IGdiGraphics); overload;
    constructor Create(Width, Height: Integer; Wnd: HWND; Dest: HDC); overload;
    destructor Destroy; override;
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

{ TBitmapSurfaceGdi }

  TBitmapSurfaceGdi = class(TSurfaceGdi)
  private
    FBitmap: TBitmapGdi;
  protected
    function HandleAvailable: Boolean; override;
    function SourceMap: PFastBitmap; override;
  public
    constructor Create(B: TBitmapGdi);
  end;

{ TWndSurfaceD2D }

  TWndSurfaceGdi = class(TSurfaceGdi)
  private
    FRect: TRect;
    FFlushed: Boolean;
  protected
    function HandleAvailable: Boolean; override;
  public
    constructor Create(Wnd: HWND);
    procedure Flush; override;
  end;

{ TBitmapGdi }

  TBitmapGdi = class(TInterfacedBitmap)
  public
    destructor Destroy; override;
    procedure HandleRelease; override;
    function GetSurface: ISurface; override;
  end;

{ TMatrixGdi }

constructor TMatrixGdi.Create(M: IGdiMatrix = nil);
begin
  inherited Create;
  FMatrix := M;
  if FMatrix = nil then
  begin
    FMatrix := NewGdiMatrix;
    FChanged := False;
    FIsIdentity := True;
  end
  else
  begin
    FChanged := True;
    FIsIdentity := False;
  end;
end;

function TMatrixGdi.Clone: IMatrix;
begin
  if FIsIdentity then
    Result := TMatrixGdi.Create(nil)
  else
    Result := TMatrixGdi.Create(FMatrix.Clone);
end;

procedure TMatrixGdi.Identity;
begin
  if not FIsIdentity then
  begin
    FMatrix.Reset;
    FChanged  := True;
    FIsIdentity := True;
  end;
end;

procedure TMatrixGdi.Multiply(M: IMatrix);
begin
  FMatrix.Multiply((M as TMatrixGdi).FMatrix, MatrixOrderAppend);
  FChanged  := True;
  FIsIdentity := False;
end;

procedure TMatrixGdi.Translate(X, Y: Float);
begin
  FMatrix.Translate(X, Y, MatrixOrderAppend);
  FChanged  := True;
  FIsIdentity := False;
end;

procedure TMatrixGdi.Scale(X, Y: Float);
begin
  FMatrix.Scale(X, Y, MatrixOrderAppend);
  FChanged  := True;
  FIsIdentity := False;
end;

procedure TMatrixGdi.Rotate(Angle: Float);
begin
  FMatrix.Rotate(Angle / Pi * 180, MatrixOrderAppend);
  FChanged  := True;
  FIsIdentity := False;
end;

function TMatrixGdi.Transform(Point: TPointF): TPointF;
var
  P: PGdiPointF;
begin
  P := @Result;
  P.X := Point.X;
  P.Y := Point.Y;
  FMatrix.TransformPoints(P, 1);
end;

{ TPenGdi }

constructor TPenGdi.Create(Color: TColorB; Width: Float);
begin
  inherited Create;
  FColor := Color;
  FPen := NewGdiPen(TGdiArgb(Color), Width);
  FPen.MiterLimit := PenMiterLimitDefault;
end;

function TPenGdi.HandleAvailable: Boolean;
begin
  if FBrush is TBrushGdi then
    Result := (FBrush as TBrushGdi).HandleAvailable
  else
    Result := True;
end;

function TPenGdi.GetBrush: IBrush;
begin
  Result := FBrush;
end;

procedure TPenGdi.SetBrush(Value: IBrush);
var
  B: TBrushGdi;
  P: IGdiPen;
begin
  FBrush := Value;
  if FBrush is TBrushGdi then
  begin
    B := FBrush as TBrushGdi;
    if B.HandleAvailable then
      FPen.SetBrush(B.FBrush);
  end
  else
  begin
    FBrush := nil;
    P := NewGdiPen(TGdiArgb(FColor), Width);
    P.EndCap := FPen.EndCap;
    P.StartCap := FPen.StartCap;
    P.DashCap := FPen.DashCap;
    P.DashOffset := FPen.DashOffset;
    P.DashStyle := FPen.DashStyle;
    P.LineJoin := FPen.LineJoin;
    P.MiterLimit := FPen.MiterLimit;
    FPen := P;
  end;
end;

function TPenGdi.GetColor: TColorB;
begin
  Result := FColor;
end;

procedure TPenGdi.SetColor(Value: TColorB);
begin
  FColor := Value;
  FPen.SetColor(TGdiArgb(FColor));
end;

function TPenGdi.GetWidth: Float;
begin
  Result := FPen.Width;
end;

procedure TPenGdi.SetWidth(Value: Float);
begin
  FPen.Width := Value;
end;

function TPenGdi.GetLinePattern: TLinePattern;
const
  Styles: array[TGdiDashStyle] of TLinePattern = (
    pnSolid, pnDash, pnDot, pnDashDot, pnSolid, pnSolid);
begin
  Result := Styles[FPen.DashStyle];
end;

procedure TPenGdi.SetLinePattern(Value: TLinePattern);
const
  Styles: array[TLinePattern] of TGdiDashStyle = (
    DashStyleSolid, DashStyleDash, DashStyleDot, DashStyleDashDot);
begin
  FPen.DashStyle := Styles[Value];
end;

function TPenGdi.GetLinePatternOffset: Float;
begin
  Result := FPen.DashOffset;
end;

procedure TPenGdi.SetLinePatternOffset(Value: Float);
begin
  FPen.DashOffset := Value;
end;

function TPenGdi.GetLineCap: TLineCap;
const
  Caps: array[0..2] of TLineCap  = (cpButt, cpSquare, cpRound);
begin
  Result := Caps[Ord(FPen.GetEndCap)];
end;

procedure TPenGdi.SetLineCap(Value: TLineCap);
const
  LineCaps: array[TLineCap] of TGdiLineCap = (LineCapFlat, LineCapRound,
    LineCapSquare);
  DashCaps: array[TLineCap] of TGdiDashCap = (DashCapFlat, DashCapRound,
    DashCapFlat);
var
  C: TGdiLineCap;
begin
  C := LineCaps[Value];
  FPen.EndCap := C;
  FPen.StartCap := C;
  FPen.DashCap := DashCaps[Value];
end;

function TPenGdi.GetLineJoin: TLineJoin;
const
  Joins: array[TGdiLineJoin] of TLineJoin = (jnMiter, jnBevel, jnRound,
    jnRound);
begin
  Result := Joins[FPen.LineJoin];
end;

procedure TPenGdi.SetLineJoin(Value: TLineJoin);
const
  Joins: array[TLineJoin] of TGdiLineJoin = (LineJoinMiter, LineJoinRound,
    LineJoinBevel);
begin
  FPen.LineJoin := Joins[Value];
end;

function TPenGdi.GetMiterLimit: Float;
begin
  Result := FPen.MiterLimit;
end;

procedure TPenGdi.SetMiterLimit(Value: Float);
begin
  FPen.MiterLimit := Value;
end;

{ TBrushGdi }

constructor TBrushGdi.Create;
begin
  inherited Create;
  FOpacity := $FF;
end;

function TBrushGdi.HandleAvailable: Boolean;
begin
  Result := FOpacity > 0;
end;

function TBrushGdi.Matrix: TMatrixGdi;
begin
  Result := GetMatrix as TMatrixGdi;
end;

function TBrushGdi.GetMatrix: IMatrix;
begin
  if FMatrix = nil then
    FMatrix := NewMatrixGdi;
  Result := FMatrix;
end;

procedure TBrushGdi.SetMatrix(Value: IMatrix);
var
  M: TMatrixGdi;
begin
  M := Value as TMatrixGdi;
  Matrix.FMatrix := M.FMatrix.Clone;
  Matrix.FIsIdentity := M.FIsIdentity;
  Matrix.FChanged := True;
end;

function TBrushGdi.GetOpacity: Byte;
begin
  Result := FOpacity;
end;

procedure TBrushGdi.SetOpacity(Value: Byte);
begin
  FOpacity := Value;
end;

{ TSolidBrushGdi }

constructor TSolidBrushGdi.Create(Color: TColorB);
begin
  inherited Create;
  FColor := Color;
  FBrush := NewGdiSolidBrush(TGdiArgb(FColor));
end;

function TSolidBrushGdi.GetColor: TColorB;
begin
  Result := FColor;
end;

procedure TSolidBrushGdi.SetColor(Value: TColorB);
begin
  FColor := Value;
  SetOpacity(FOpacity);
end;

procedure TSolidBrushGdi.SetOpacity(Value: Byte);
var
  C: TColorB;
begin
  C := FColor;
  C := C.Fade(Value / $FF);
  (FBrush as IGdiSolidBrush).SetColor(TGdiArgb(C));
end;

{ Gdi+ specific gradient helper routines }

function StopsColors(const Stops: TGradientStops; Opacity: Byte): TGradientColors;
var
  F: Single;
  I: Integer;
begin
  F := Opacity / $FF;
  Result.Length := Stops.Length + 2;
  Result.First := TGdiArgb(Stops[0].Color.Fade(F));
  for I := 0 to Stops.Length - 1 do
    Result[I + 1] := TGdiArgb(Stops[I].Color.Fade(F));
  Result.Last := Result[Result.Length - 2];
end;

function StopsOffsets(const Stops: TGradientStops): TGradientOffsets;
var
  I: Integer;
begin
  Result.Length := Stops.Length + 2;
  Result.First := 0;
  for I := 0 to Stops.Length - 1 do
    Result[I + 1] := Stops[I].Offset;
  Result.Last := 1;
end;

procedure StopsScale(var Offsets: TGradientOffsets; Scale: Float);
var
  I: Integer;
begin
  for I := 0 to Offsets.Length - 1 do
    Offsets[I] := (Offsets[I] - 0.5) * Scale + 0.5;
  Offsets.First := 0;
  Offsets.Last := 1;
end;

constructor TGradientBrushGdi.Create(const Origin: TPointF);
begin
  inherited Create;
  FOrigin := Origin;
  FWrap := gwClamp;
end;

function TGradientBrushGdi.HandleAvailable: Boolean;
begin
   Result := (not FStops.IsEmpty) and (FOpacity > 0);
  if not Result then
    Exit;
  if FBrush = nil then
    FBrush := NewGdiSolidBrush(TGdiArgb(FStops[0].Color.Fade(FOpacity / $FF)));
end;

procedure TGradientBrushGdi.SetOpacity(Value: Byte);
begin
  inherited SetOpacity(Value);
  FBrush := nil;
end;

function TGradientBrushGdi.GetWrap: TGradientWrap;
begin
  Result := FWrap;
end;

procedure TGradientBrushGdi.SetWrap(Value: TGradientWrap);
begin
  FWrap := Value;
  FBrush := nil;
end;

procedure TGradientBrushGdi.AddStop(Color: TColorB; Offset: Float);
var
  S: TGradientStop;
begin
  S.Color := Color;
  if FReverse then
    S.Offset := 1 - Offset
  else
    S.Offset := Offset;
  FStops.Push(S);
  FBrush := nil;
end;

{ TLinearGradientBrushGdi }

constructor TLinearGradientBrushGdi.Create(const A, B: TPointF);
begin
  inherited Create(TPointF.Create((A.X + B.X) / 2, (A.Y + B.Y) / 2));
  FPoint := A - FOrigin;
  { FDist has to do with a matrix problem with Gdi+ linear gradients }
  FDist := Sqrt(FPoint.X * FPoint.X + FPoint.Y * FPoint.Y);
  if FPoint.X < 0 then
    FDist := -FDist;
end;

function GradientStopCompare(constref A, B: TGradientStop): Integer;
begin
  if A.Offset < B.Offset then
    Result := -1
  else if A.Offset > B.Offset then
    Result := 1
  else
    Result := 0;
end;

function TLinearGradientBrushGdi.HandleAvailable: Boolean;

  procedure Clamp(var Point: TPointF; var Scale: Float);
  const
    ClampBounds = 5000;
  begin
    Scale := Sqrt(Point.X * Point.X + Point.Y * Point.Y);
    if Scale = 0 then
    begin
      Scale := 1;
      Exit;
    end;
    Scale := ClampBounds / Scale;
    Point.X := Point.X * Scale;
    Point.Y := Point.Y * Scale;
    Scale := 1 / Scale;
  end;

const
  Modes: array[TGradientWrap] of TGdiWrapMode = (WrapModeTileFlipXY,
    WrapModeTile, WrapModeTileFlipXY);
var
  Colors: TGradientColors;
  Offsets: TGradientOffsets;
  A: TPointF;
  S: Float;
  M: IGdiMatrix;
begin
  if (FStops.Length < 2) or (FOpacity = 0) then
    Exit(inherited HandleAvailable);
  if FBrush = nil then
  begin
    FStops.Sort(soAscend, GradientStopCompare);
    Colors := StopsColors(FStops, FOpacity);
    Offsets := StopsOffsets(FStops);
    A.X := FDist;
    A.Y := 0;
    S := 1;
    { Gdi+ does not support clamp on linear gradients, this is a work around }
    if FWrap = gwClamp then
    begin
      Clamp(A, S);
      StopsScale(Offsets, S);
    end;
    FGradient := NewGdiLinearGradientBrush(TGdiPointF(A), TGdiPointF(-A), 0, 0);
    FGradient.SetInterpolationColors(Colors.Data, Offsets.Data, Colors.Length);
    FGradient.SetLinearColors(Colors.First, Colors.Last);
    FGradient.SetWrapMode(Modes[FWrap]);
    { Gamma correction is not supported by cairo }
    if SurfaceOptions.GammaCorrection then
       FGradient.SetGammaCorrection(True);
    FBrush := FGradient;
    Matrix.FChanged := True;
  end;
  if Matrix.FChanged then
  begin
    M := NewGdiMatrix;
    M.Translate(FOrigin.X, FOrigin.Y);
    M.Multiply(Matrix.FMatrix, MatrixOrderPrepend);
    { Gdi+ actually uses a matrix to store its angle, we fix that here }
    if FPoint.X = 0 then
      if FPoint.Y > 0 then
        M.Rotate(-90)
      else
        M.Rotate(90)
    else if FPoint.Y <> 0 then
      M.Rotate(ArcTan(FPoint.Y / FPoint.X) / Pi * 180);
    FGradient.SetTransform(M);
    Matrix.FChanged := False;
  end;
  Result := True;
end;

{ TRadialGradientBrushGdi }

constructor TRadialGradientBrushGdi.Create(const Rect: TRectF);
begin
  inherited Create(Rect.MidPoint);
  FRect := Rect;
  FRect.Center(0, 0);
  FReverse := True;
end;


function TRadialGradientBrushGdi.HandleAvailable: Boolean;

  procedure Clamp(var Rect: TRectF; var Scale: Float);
  const
    ClampBounds = 5000;
  var
    Point: TPointF;
  begin
    Point := Rect.TopLeft;
    Scale := Sqrt(Point.X * Point.X + Point.Y * Point.Y);
    if Scale = 0 then
    begin
      Scale := 1;
      Exit;
    end;
    Scale := ClampBounds / Scale;
    Point.X := Abs(Point.X) * Scale * 2;
    Point.Y := Abs(Point.Y) * Scale * 2;
    Rect := TRectF.Create(Point.X, Point.Y);
    Rect.Center(0, 0);
    Scale := 1 / Scale;
  end;

const
  CenterPoint: TGdiPointF = ();
var
  Rect: TRectF;
  Scale: Float;
  Path: IGdiGraphicsPath;
  Colors: TGradientColors;
  Offsets: TGradientOffsets;
  M: IGdiMatrix;
  I: Integer;
begin
  if (FStops.Length < 2) or (FOpacity = 0) then
    Exit(inherited HandleAvailable);
  if FBrush = nil then
  begin
    Rect := FRect;
    Scale := 1;
    { Gdi+ doesn't support radial gradients, we're stuck using a fake clamp mode only :( }
    Clamp(Rect, Scale);
    Path := NewGdiGraphicsPath;
    Path.AddEllipse(TGdiRectF(Rect));
    FStops.Sort(soAscend, GradientStopCompare);
    Colors := StopsColors(FStops, FOpacity);
    Offsets := StopsOffsets(FStops);
    I := Offsets.Length;
    while I > 0 do
    begin
      Dec(I);
      Offsets[I] := (Offsets[I] - 1) * Scale + 1;
    end;
    Offsets.First := 0;
    Offsets.Last := 1;
    FGradient := NewGdiPathGradientBrush(Path);
    FGradient.SetCenterPoint(CenterPoint);
    FGradient.SetInterpolationColors(Colors.Data, Offsets.Data, Colors.Length);
    if SurfaceOptions.GammaCorrection then
       FGradient.SetGammaCorrection(True);
    FBrush := FGradient;
    Matrix.FChanged := True;
  end;
  if Matrix.FChanged then
  begin
    M := NewGdiMatrix;
    M.Translate(FOrigin.X, FOrigin.Y);
    M.Multiply(Matrix.FMatrix, MatrixOrderPrepend);
    FGradient.SetTransform(M);
    Matrix.FChanged := False;
  end;
  Result := True;
end;

{ TBitmapBrushGdi }

constructor TBitmapBrushGdi.Create(Bitmap: IBitmap);
begin
  inherited Create;
  FBitmap := Bitmap.Clone;
end;

function TBitmapBrushGdi.HandleAvailable: Boolean;
var
  M: IGdiMatrix;
begin
  Result := (not FBitmap.Empty) and (FOpacity > 0);
  if not Result then
    Exit;
  if FBrush = nil then
  begin
    FTextureBrush := NewGdiTextureBrush(FBitmap.Width, FBitmap.Height,
      FBitmap.Pixels, FOpacity);
    FBrush := FTextureBrush;
    Matrix.FChanged := True;
  end;
  if Matrix.FChanged then
  begin
    M := Matrix.FMatrix.Clone;
    M.Translate(0.5, 0.5);
    FTextureBrush.SetTransform(M);
    Matrix.FChanged := False;
  end;
  Result := True;
end;

procedure TBitmapBrushGdi.SetOpacity(Value: Byte);
begin
  inherited SetOpacity(Value);
  FTextureBrush := nil;
  FBrush := nil;
end;

{ TFontGdi }

constructor TFontGdi.Create(F: TFont);
begin
  inherited Create;
  FFontObject := TFont.Create;
  FFontObject.Assign(F);
  FColor := F.Color;
  FQuality := F.Quality;
end;

destructor TFontGdi.Destroy;
begin
  FFontObject.Free;
  inherited Destroy;
end;

function TFontGdi.Font: IGdiFont;
const
  Bytes: array[Boolean] of Byte = (0, 1);
var
  DC: HDC;
begin
  if FFont = nil then
  begin
    DC := GetDC(0);
    FFont := NewGdiFont(DC, FFontObject.Handle);
    ReleaseDC(0, DC);
  end;
  Result := FFont;
end;

function TFontGdi.GetName: string;
begin
  Result := FFontObject.Name;
end;

procedure TFontGdi.SetName(const Value: string);
begin
  FFontObject.Name := Value;
end;

function TFontGdi.GetColor: TColorB;
begin
  Result := FColor;
end;

procedure TFontGdi.SetColor(Value: TColorB);
begin
  FColor := Value;
end;

function TFontGdi.GetQuality: TFontQuality;
begin
  Result := FQuality;
end;

procedure TFontGdi.SetQuality(Value: TFontQuality);
begin
  FQuality := Value;
end;

function TFontGdi.GetStyle: TFontStyles;
begin
  Result := FFontObject.Style;
end;

procedure TFontGdi.SetStyle(Value: TFontStyles);
begin
  if Value <> FFontObject.Style then
  begin
    FFontObject.Style := Value;
    FFont := nil;
  end;
end;

function TFontGdi.GetSize: Float;
begin
  Result := FFontObject.Height;
end;

procedure TFontGdi.SetSize(Value: Float);
begin
  if Value < 0 then
    Value := 0;
  if Value <> FFontObject.Height then
  begin
    FFontObject.Height := Round(Value);
    FFont := nil;
  end;
end;

{ TPathGdi }

constructor TPathGdi.Create(Path: IGdiGraphicsPath);
begin
  inherited Create;
  FPath := Path;
end;

{ TSurfacePathGdi }

constructor TSurfacePathGdi.Create(Surface: TSurfaceGdi);
begin
  inherited Create;
  FSurface := Surface;
end;

function TSurfacePathGdi.HandleAvailable: Boolean;
begin
  Result := (FSurface <> nil) and (FSurface.HandleAvailable);
end;

procedure TSurfacePathGdi.HandleRelease;
begin
  FData := nil;
   FFigure := nil;
  FOrigin.X := 0;
  FOrigin.Y := 0;
  FClosed := False;
end;

function TSurfacePathGdi.Clone: IPathData;
begin
  Result := TPathGdi.Create(FData);
end;

procedure TSurfacePathGdi.Add;
begin
  if not HandleAvailable then
    Exit;
  FClosed := False;
  if FFigure <> nil then
  begin
    if FData = nil then
      FData := NewGdiGraphicsPath;
    if not FSurface.Matrix.FIsIdentity then
      FFigure.Transform(FSurface.Matrix.FMatrix);
    FData.AddPath(FFigure, False);
    FFigure := nil;
  end;
end;

procedure TSurfacePathGdi.Add(P: IGdiGraphicsPath);
begin
  if not HandleAvailable then
    Exit;
  Add;
  if FData = nil then
    FData := NewGdiGraphicsPath;
  if not FSurface.Matrix.FIsIdentity then
    P.Transform(FSurface.Matrix.FMatrix);
  FData.AddPath(P, False);
end;

procedure TSurfacePathGdi.Remove;
begin
  if not HandleAvailable then
    Exit;
  FData := nil;
  FFigure := nil;
end;

procedure TSurfacePathGdi.Open(X, Y: Float);
begin
  if not HandleAvailable then
    Exit;
  if not FClosed then
    Add;
  FOrigin.X := X;
  FOrigin.Y := Y;
end;

procedure TSurfacePathGdi.Open;
begin
  if not HandleAvailable then
    Exit;
  if FFigure = nil then
    FFigure := NewGdiGraphicsPath;
end;

procedure TSurfacePathGdi.Close;
begin
  if not HandleAvailable then
    Exit;
  if FFigure <> nil then
  begin
    FFigure.CloseFigure;
    FClosed := True;
  end;
end;

procedure TSurfacePathGdi.Join(Path: IPathData);
var
  P: TPathGdi;
begin
  if not HandleAvailable then
    Exit;
  Add;
  P := Path as TPathGdi;
  if P.FPath <> nil then
  begin
    if FData = nil then
      FData := NewGdiGraphicsPath;
    { TODO: Consider transforming P based on cairo implementation }
    FData.AddPath(P.FPath, False);
  end;
end;

procedure TSurfacePathGdi.Clip;
begin
  if not HandleAvailable then
    Exit;
  Add;
  if FData <> nil then
    FSurface.FGraphics.SetClip(FData, CombineModeIntersect);
  Remove;
end;

procedure TSurfacePathGdi.Unclip;
begin
  if HandleAvailable then
    FSurface.FGraphics.ResetClip;
end;

{ TSurfaceStateGdi }

constructor TSurfaceStateGdi.Create(C: TSurfaceGdi);
begin
  inherited Create;
  C.Path.Add;
  Origin := C.Path.FOrigin;
  Clip := NewGdiRegion;
  C.FGraphics.GetClip(Clip);
  if C.Path.FData <> nil then
    Data := C.Path.FData.Clone;
  Matrix := C.Matrix.Clone;
end;

procedure TSurfaceStateGdi.Restore(C: TSurfaceGdi);
begin
  C.Path.Add;
  C.Path.FOrigin := Origin;
  C.FGraphics.SetClip(Clip);
  C.Path.FData := Data;
  C.SetMatrix(Matrix);
  inherited Destroy;
end;

{ TSurfaceGdi }

procedure TSurfaceGdi.InitializeDefaults;
var
  M: IGdiMatrix;
begin
  if FGraphics <> nil then
  begin
    M := NewGdiMatrix;
    M.Translate(-0.5, -0.5);
    FGraphics.Transform := M;
    FGraphics.SmoothingMode := SmoothingModeHighQuality;
    FGraphics.InterpolationMode := InterpolationModeHighQuality;
  end;
end;

constructor TSurfaceGdi.Create(G: IGdiGraphics);
begin
  inherited Create;
  FGraphics := G;
  FPath := TSurfacePathGdi.Create(Self);
  FMatrix := NewMatrixGdi;
  InitializeDefaults;
end;

constructor TSurfaceGdi.Create(Width, Height: Integer; Wnd: HWND; Dest: HDC);
begin
  FBuffer := CreateFastBitmap(Width, Height, pd32);
  FWnd := Wnd;
  FDest := Dest;
  Create(NewGdiGraphics(FBuffer.DC));
end;

destructor TSurfaceGdi.Destroy;
begin
  Path.FSurface := nil;
  Flush;
  DestroyFastBitmap(FBuffer);
  if (FWnd <> 0) and (FDest <> 0) then
    ReleaseDC(FWnd, FDest);
  inherited Destroy;
end;

function TSurfaceGdi.Path: TSurfacePathGdi;
begin
  Result := FPath as TSurfacePathGdi;
end;

function TSurfaceGdi.Matrix: TMatrixGdi;
begin
  Result := FMatrix as TMatrixGdi;
end;

function TSurfaceGdi.HandleAvailable: Boolean;
begin
  Result := FGraphics <> nil;
end;

procedure TSurfaceGdi.HandleRelease;
begin
  FGraphics := nil;
end;

function TSurfaceGdi.SourceMap: PFastBitmap;
begin
  Result := @FBuffer;
end;

function TSurfaceGdi.GetMatrix: IMatrix;
begin
  Result := FMatrix;
end;

procedure TSurfaceGdi.SetMatrix(Value: IMatrix);
var
  M: TMatrixGdi;
begin
  M := Value as TMatrixGdi;
  Matrix.FMatrix := M.FMatrix.Clone;
  Matrix.FIsIdentity := M.FIsIdentity;
end;

function TSurfaceGdi.GetPath: IPath;
begin
  Result := FPath;
end;

function TSurfaceGdi.GetHandle: Pointer;
begin
  Result := Pointer(FDest);
end;

procedure TSurfaceGdi.Flush;
begin
  if not HandleAvailable then
    Exit;
  FGraphics.Flush(FlushIntentionSync);
  if FDest <> 0 then
    AlphaDraw(FDest, 0, 0, FBuffer);
end;

procedure TSurfaceGdi.Clear(Color: TColorB);
begin
  if HandleAvailable then
    FGraphics.Clear(TGdiArgb(Color));
end;

procedure TSurfaceGdi.CopyTo(const Source: TRectF; Surface: ISurface;
  const Dest: TRectF; Alpha: Byte = $FF; Quality: TResampleQuality = rqNormal);

  function OpacityTransform: IGdiImageAttributes;
  var
    T: TColorTransform;
  begin
    if Alpha = $FF then
      Exit(nil);
    T := NewGdiColorTransform;
    T.Opacity := Alpha / $FF;
    Result := NewGdiImageAttributes(T);
  end;

const
  Resamples: array[TResampleQuality] of TInterpolationMode =
    (InterpolationModeNearestNeighbor,
    InterpolationModeHighQualityBilinear,
    InterpolationModeHighQualityBicubic);
var
  DstSurface: TSurfaceGdi;
  Bitmap: IGdiBitmap;
  A, B: IGdiMatrix;
  G: IGdiGraphics;
  S, D: TRectI;
begin
  if Alpha = 0 then
    Exit;
  if not HandleAvailable then
    Exit;
  DstSurface := Surface as TSurfaceGdi;
  if not DstSurface.HandleAvailable then
    Exit;
  FGraphics.Flush;
  { For now we only allow bitmap or buffered Surface sources }
  if IsFastBitmap(SourceMap^) then
    Bitmap := NewGdiBitmap(SourceMap^)
  else
    { Drawing directly to a window, we have no source buffer ... exit }
    Exit;
  S := TRectI(Source);
  D := TRectI(Dest);
  A := DstSurface.FGraphics.Transform;
  B := DstSurface.Matrix.FMatrix.Clone;
  G := DstSurface.FGraphics;
  G.Transform := B;
  G.InterpolationMode := Resamples[Quality];
  G.DrawImage(Bitmap, TGdiRectI(D),
    S.X, S.Y, S.Width, S.Height, UnitPixel, OpacityTransform);
  G.Transform := A;
end;

procedure TSurfaceGdi.Save;
begin
  if not HandleAvailable then
    Exit;
  if FStateStack = nil then
    FStateStack := TObjects<TSurfaceStateGdi>.Create(True);
  FStateStack.Add(TSurfaceStateGdi.Create(Self));
end;

procedure TSurfaceGdi.Restore;
var
  S: TSurfaceStateGdi;
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

procedure TSurfaceGdi.MoveTo(X, Y: Float);
begin
  if not HandleAvailable then
    Exit;
  Path.Open(X, Y);
end;

procedure TSurfaceGdi.LineTo(X, Y: Float);
var
  P: TSurfacePathGdi;
begin
  if not HandleAvailable then
    Exit;
  P := Path;
  P.Open;
  P.FFigure.AddLine(P.FOrigin.X, P.FOrigin.Y, X, Y);
  P.FOrigin.X := X;
  P.FOrigin.Y := Y;
end;

function PointRotate(const P: TPointF; Angle: Float): TPointF;
begin
  Result.X := (P.X * Cos(Angle)) - (P.Y * Sin(Angle));
  Result.Y := (P.X * Sin(Angle)) + (P.Y * Cos(Angle));
end;

procedure TSurfaceGdi.ArcTo(const Rect: TRectF; BeginAngle, EndAngle: Float);
const
  AngleAdjust = Pi / 2;
var
  P: TSurfacePathGdi;
begin
  if not HandleAvailable then
    Exit;
  BeginAngle :=  BeginAngle - AngleAdjust;
  EndAngle :=  EndAngle - AngleAdjust;
  P := Path;
  P.Open;
  P.FFigure.AddArc(TGdiRectF(Rect), BeginAngle / Pi * 180,
    (EndAngle - BeginAngle) / Pi * 180);
  P.FFigure.GetLastPoint(TGdiPointF(P.FOrigin));
end;

procedure TSurfaceGdi.CurveTo(X, Y: Float; const C1, C2: TPointF);
var
  EndPoint: TPointF;
  P: TSurfacePathGdi;
begin
  if not HandleAvailable then
    Exit;
  P := Path;
  P.Open;
  EndPoint.X := X;
  EndPoint.Y := Y;
  P.FFigure.AddBezier(TGdiPointF(P.FOrigin), TGdiPointF(C1),
    TGdiPointF(C2), TGdiPointF(EndPoint));
  P.FOrigin := EndPoint;
end;

procedure TSurfaceGdi.Ellipse(const Rect: TRectF);
var
  P: IGdiGraphicsPath;
begin
  if not HandleAvailable then
    Exit;
  P := NewGdiGraphicsPath;
  P.AddEllipse(TGdiRectF(Rect));
  Path.Add(P);
end;

procedure TSurfaceGdi.Rectangle(const Rect: TRectF);
var
  P: IGdiGraphicsPath;
begin
  if not HandleAvailable then
    Exit;
  P := NewGdiGraphicsPath;
  P.AddRectangle(TGdiRectF(Rect));
  Path.Add(P);
end;

procedure TSurfaceGdi.RoundRectangle(const Rect: TRectF; Radius: Float);
const
  Angle = 90;
var
  P: TSurfacePathGdi;
  X, Y: Float;
  R: TRectF;
  A: Float;
begin
  if not HandleAvailable then
    Exit;
  if Rect.Width < Radius * 2 then
    Radius := Rect.Width / 2;
  if Rect.Height < Radius * 2 then
    Radius := Rect.Height / 2;
  P := Path;
  X := Rect.MidPoint.X;
  Y := Rect.Top;
  R := TRectF.Create(Radius * 2, Radius * 2);
  R.Offset(Rect.Right - Radius * 2, Rect.Top);
  P.Open(X, Y);
  P.Open;
  P.FFigure.AddLine(X, Y, Rect.Right - Radius, Y);
  A := -90;
  P.FFigure.AddArc(TGdiRectF(R), A, Angle);
  R.Offset(0, Rect.Height - R.Height);
  A := A + Angle;
  P.FFigure.AddArc(TGdiRectF(R), A, Angle);
  R.Offset(R.Width - Rect.Width, 0);
  A := A + Angle;
  P.FFigure.AddArc(TGdiRectF(R), A, Angle);
  R.Offset(0, R.Height - Rect.Height);
  A := A + Angle;
  P.FFigure.AddArc(TGdiRectF(R), A, Angle);
  P.Close;
end;

{ TODO: Implement ICanavs.TextXxx routines on Gdi }

function TSurfaceGdi.TextSize(Font: IFont; const Text: string): TPointF;
const
  MeasureOrigin: TGdiPointF = ();
var
  F: TFontGdi;
  B: TGdiRectF;
begin
  Result := TPointF(MeasureOrigin);
  if not HandleAvailable then
    Exit;
  F := Font as TFontGdi;
  FGraphics.SetPageUnit(UnitPixel);
  FGraphics.MeasureString(Text, F.Font, MeasureOrigin, B);
  Result := TPointF.Create(B.Width, B.Height);
end;

function TSurfaceGdi.TextHeight(Font: IFont; const Text: string; Width: Float): Float;
var
  F: TFontGdi;
  L: TGdiRectF;
  B: TGdiRectF;
begin
  Result := 0;
  if not HandleAvailable then
    Exit;
  F := Font as TFontGdi;
  FGraphics.SetPageUnit(UnitPixel);
  L.X := 0;
  L.Y := 0;
  L.Width := Width;
  L.Height := High(Integer);
  FGraphics.MeasureString(Text, F.Font, L, B);
  Result := B.Height;
end;

{ See notes in CrossSurfaceD2D.pas TSurfaceD2D.TextOut. Most everything written
  there applies here as well }

procedure TSurfaceGdi.TextOut(Font: IFont; const Text: string; const Rect: TRectF;
  Direction: TDirection; Immediate: Boolean = True);
const
  TextHints: array[TFontQuality] of TTextRenderingHint = (
    TextRenderingHintSystemDefault, TextRenderingHintSingleBitPerPixelGridFit,
      TextRenderingHintAntiAliasGridFit, TextRenderingHintSingleBitPerPixelGridFit,
      TextRenderingHintAntiAliasGridFit, TextRenderingHintClearTypeGridFit,
      TextRenderingHintClearTypeGridFit);
var
  F: TFontGdi;
  StringFormat: IGdiStringFormat;
  HAlign, VAlign: TStringAlignment;
  Wrap: Boolean;
  M: IGdiMatrix;
  G: IGdiGraphicsPath;
  A: IGdiFontFamily;
  S: Integer;
  E: Float;
begin
  if not HandleAvailable then
    Exit;
  if SurfaceOptions.ErrorCorrection or Immediate then
    Path.Remove;
  if Rect.Empty or (Text = '') then
    Exit;
  Path.Add;
  F := Font as TFontGdi;
  FGraphics.SetPageUnit(UnitPixel);
  case Direction of
    drLeft, drWrap, drFlow:
      HAlign := StringAlignmentNear;
    drRight:
      HAlign := StringAlignmentFar;
  else
    HAlign := StringAlignmentCenter;
  end;
  case Direction of
    drLeft, drRight, drCenter, drFill:
      VAlign := StringAlignmentCenter;
    drDown:
      VAlign := StringAlignmentFar;
  else
    VAlign := StringAlignmentNear;
  end;
  Wrap := not (Direction in [drLeft..drCenter]);
  StringFormat := NewGdiStringFormat(HAlign, VAlign, Wrap);
  FGraphics.TextRenderingHint := TextHints[Font.Quality];
  if SurfaceOptions.ErrorCorrection or Immediate then
  begin
    M := FGraphics.GetTransform;
    FGraphics.SetTransform(Matrix.FMatrix);
    FGraphics.DrawString(Text, F.Font, TGdiRectF(Rect), StringFormat,
      NewGdiSolidBrush(TGdiArgb(F.FColor)));
    FGraphics.SetTransform(M);
  end
  else
  begin
    G := NewGdiGraphicsPath;
    A := NewGdiFontFamily(F.Font);
    S := 0;
    if fsBold in F.FFontObject.Style then
      S := S or FontStyleBold;
    if fsItalic in F.FFontObject.Style then
      S := S or FontStyleItalic;
    if fsUnderline in F.FFontObject.Style then
      S := S or FontStyleUnderline;
    if fsStrikeOut in F.FFontObject.Style then
      S := S or FontStyleStrikeout;
    E := F.FFontObject.Height / DeviceIndependentPixels * PointsPerInch;
    G.AddString(Text, Length(Text), A, S, E, TGdiRectF(Rect), StringFormat);
    Path.Add(G);
  end;
end;

procedure ApplyMatrix(Brush: IGdiBrush; Matrix: IMatrix; out State: IGdiMatrix);
var
  M: IGdiMatrix;
begin
  State := NewGdiMatrix;
  if Brush = nil then
    Exit;
  M := (Matrix as TMatrixGdi).FMatrix.Clone;
  State := Brush.GetTransform;
  M.Multiply(State);
  Brush.SetTransform(M);
end;

procedure RestoreMatrix(Brush: IGdiBrush; State: IGdiMatrix);
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

procedure TSurfaceGdi.FillOrStroke(Brush: IBrush; Pen: IPen; Preserve: Boolean);
var
  State: IGdiMatrix;
  P: TSurfacePathGdi;
  W: Float;
begin
  if not HandleAvailable then
    Exit;
  P := Path;
  P.Add;
  if P.FData = nil then
    Exit;
  if (Brush is TBrushGdi) and (Brush as TBrushGdi).HandleAvailable then
  begin
    ApplyMatrix((Brush as TBrushGdi).FBrush, GetMatrix, State);
    FGraphics.FillPath((Brush as TBrushGdi).FBrush, P.FData);
    RestoreMatrix((Brush as TBrushGdi).FBrush, State);
  end
  else if (Pen is TPenGdi) and (Pen as TPenGdi).HandleAvailable then
  begin
    W := Pen.Width;
    Pen.Width := PenWidth(GetMatrix, W);
    if Pen.Brush <> nil then
    begin
      ApplyMatrix((Pen.Brush as TBrushGdi).FBrush, GetMatrix, State);
      FGraphics.DrawPath((Pen as TPenGdi).FPen, P.FData);
      RestoreMatrix((Pen.Brush as TBrushGdi).FBrush, State);
    end
    else
      FGraphics.DrawPath((Pen as TPenGdi).FPen, P.FData);
    Pen.Width := W;
  end;
  if not Preserve then
    P.Remove;
end;

procedure TSurfaceGdi.Stroke(Pen: IPen; Preserve: Boolean = False);
begin
  FillOrStroke(nil, Pen, Preserve);
end;

procedure TSurfaceGdi.Fill(Brush: IBrush; Preserve: Boolean = False);
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

procedure TSurfaceGdi.StrokeRect(Pen: IPen; const Rect: TRectF);
begin
  Path.Remove;
  Rectangle(AlignRect(Pen, Rect));
  Stroke(Pen);
end;

procedure TSurfaceGdi.FillRect(Brush: IBrush; const Rect: TRectF);
begin
  Path.Remove;
  Rectangle(Rect);
  Fill(Brush);
end;

procedure TSurfaceGdi.StrokeRoundRect(Pen: IPen; const Rect: TRectF; Radius: Float);
begin
  Path.Remove;
  RoundRectangle(AlignRect(Pen, Rect), Radius);
  Stroke(Pen);
end;

procedure TSurfaceGdi.FillRoundRect(Brush: IBrush; const Rect: TRectF; Radius: Float);
begin
  Path.Remove;
  RoundRectangle(Rect, Radius);
  Fill(Brush);
end;

{ TBitmapSurfaceGdi }

constructor TBitmapSurfaceGdi.Create(B: TBitmapGdi);
begin
  inherited Create(nil);
  FBitmap := B
end;

function TBitmapSurfaceGdi.HandleAvailable: Boolean;
begin
  Result := inherited HandleAvailable;
  if not Result then
    if (FBitmap <> nil) and FBitmap.HandleAvailable then
    begin
      FGraphics := NewGdiGraphics(FBitmap.FBitmap.DC);
      InitializeDefaults;
      Result := True;
    end;
end;

function TBitmapSurfaceGdi.SourceMap: PFastBitmap;
begin
  if FBitmap <> nil then
    Result := @FBitmap.FBitmap
  else
    Result := inherited SourceMap;
end;

{ TWndSurfaceGdi }

constructor TWndSurfaceGdi.Create(Wnd: HWND);
begin
  inherited Create(nil);
  FWnd := Wnd;
end;

function TWndSurfaceGdi.HandleAvailable: Boolean;

  procedure CreateGraphics;
  var
    R: TRect;
  begin
    if SurfaceOptions.SoftwareBuffering then
    begin
      GetClientRect(FWnd, R);
      if (WidthOf(R) > 0) and (HeightOf(R) > 0) then
      begin
        FDest := GetDC(FWnd);
        FBuffer := CreateFastBitmap(WidthOf(R), HeightOf(R), pd32);
        FGraphics := NewGdiGraphics(FBuffer.DC);
      end;
    end
    else
      FGraphics := NewGdiGraphics(FWnd, False);
    InitializeDefaults;
  end;

var
  R: TRect;
begin
  if FGraphics = nil then
  begin
    if not IsWindow(FWnd) then
      Exit(False);
    CreateGraphics;
  end
  else if FFlushed then
  begin
    if not IsWindow(FWnd) then
      Exit(False);
    FFlushed := False;
    GetClientRect(FWnd, R);
    { We must recreate the target if the client size has changed }
    if (R.Left <> FRect.Left) or (R.Top <> FRect.Top) or
      (R.Right <> FRect.Right) or (R.Bottom <> FRect.Bottom) then
    begin
      FGraphics := nil;
      if FDest <> 0 then
        ReleaseDC(FWnd, FDest);
      FDest := 0;
      DestroyFastBitmap(FBuffer);
      CreateGraphics;
    end;
  end;
  Result := inherited HandleAvailable;
end;

procedure TWndSurfaceGdi.Flush;
begin
  inherited Flush;
  FFlushed := True;
  Path.Unclip;
  Path.FClip := nil;
  Path.FData := nil;
  FStateStack := nil;
  InitializeDefaults;
end;

{ TBitmapGdi }

destructor TBitmapGdi.Destroy;
begin
  HandleRelease;
  if FSurface <> nil then
    (FSurface as TBitmapSurfaceGdi).FBitmap := nil;
  inherited Destroy;
end;

procedure TBitmapGdi.HandleRelease;
begin
  if FSurface <> nil then
    (FSurface as TBitmapSurfaceGdi).HandleRelease;
  inherited HandleRelease;
end;

function TBitmapGdi.GetSurface: ISurface;
begin
  if FSurface = nil then
    FSurface := TBitmapSurfaceGdi.Create(Self);
  Result := FSurface;
end;

{ New object routines }

function NewMatrixGdi: IMatrix;
begin
  Result := TMatrixGdi.Create;
end;

function NewPenGdi(Brush: IBrush; Width: Float = 1): IPen;
begin
  Result := TPenGdi.Create(clBlack, Width);
  Result.Brush := Brush;
end;

function NewPenGdi(Color: TColorB; Width: Float = 1): IPen;
begin
  Result := TPenGdi.Create(Color, Width);
end;

function NewSolidBrushGdi(Color: TColorB): ISolidBrush;
begin
  Result := TSolidBrushGdi.Create(Color);
end;

function NewBitmapBrushGdi(Bitmap: IBitmap): IBitmapBrush;
begin
  Result := TBitmapBrushGdi.Create(Bitmap);
end;

function NewLinearGradientBrushGdi(X1, Y1, X2, Y2: Float): ILinearGradientBrush;
begin
  Result := TLinearGradientBrushGdi.Create(TPointF.Create(X1, Y1),
    TPointF.Create(X2, Y2));
end;

function NewLinearGradientBrushGdi(const A, B: TPointF): ILinearGradientBrush;
begin
  Result := TLinearGradientBrushGdi.Create(A, B);
end;

function NewRadialGradientBrushGdi(const Rect: TRectF): IRadialGradientBrush;
begin
  Result := TRadialGradientBrushGdi.Create(Rect);
end;

function NewFontGdi(Font: TFont): IFont;
begin
  Result := TFontGdi.Create(Font);
end;

function NewSurfaceGdi(Canvas: TCanvas): ISurface;
var
  G: IGdiGraphics;
begin
  if Canvas = nil then
    G := NewGdiGraphics(GetDesktopWindow)
  else
    G := NewGdiGraphics(Canvas.Handle);
  Result := TSurfaceGdi.Create(G);
end;

function NewSurfaceGdi(Control: TWinControl): ISurface;
begin
  Result := TWndSurfaceGdi.Create(Control.Handle);
end;

function NewBitmapGdi(Width, Height: Integer): IBitmap;
begin
  Result := TBitmapGdi.Create;
  Result.SetSize(Width, Height);
end;
{$endif}

end.

