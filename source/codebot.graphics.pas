(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified March 2015                                 *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.graphics.txt> }
unit Codebot.Graphics;

{$i codebot.inc}

interface

uses
  Classes, SysUtils, Graphics, Controls,
  Codebot.System,
  Codebot.Graphics.Types,
  Codebot.Animation;

{ Create a new matrix }
function NewMatrix: IMatrix;
{ Create a new pen using a brush as the color }
function NewPen(Brush: IBrush; Width: Float = 1): IPen; overload;
{ Create a new solid color pen }
function NewPen(Color: TColorB; Width: Float = 1): IPen; overload;
{ Create a new solid color brush }
function NewBrush(Color: TColorB): ISolidBrush; overload;
{ Create a new bitmap pattern brush }
function NewBrush(Bitmap: IBitmap): IBitmapBrush; overload;
{ Create a new linear gradient brush using four coordinates for endpoints }
function NewBrush(X1, Y1, X2, Y2: Float): ILinearGradientBrush; overload;
{ Create a new linear gradient brush using two points for endpoints }
function NewBrush(const A, B: TPointF): ILinearGradientBrush; overload;
{ Create a new radial gradient brush bounded by a rect }
function NewBrush(const Rect: TRectF): IRadialGradientBrush; overload;
{ Create a new font given a name and size }
function NewFont(const FontName: string; FontSize: Integer = 10): IFont; overload;
{ Create a new font by copying a regular font object, or use the system default }
function NewFont(Font: TFont = nil): IFont; overload;
{ Create a new surface using a regular canvas object }
function NewSurface(Canvas: TCanvas): ISurface; overload;
{ Create a new surface using a window }
function NewSurface(Control: TWinControl): ISurface; overload;
{ Create a new bitmap of width and height size }
function NewBitmap(Width: Integer = 0; Height: Integer = 0): IBitmap;
{ Create a new splash screen }
function NewSplash: ISplash;

{ TSurfaceBitmap is a TGraphic representation of an IBitmap
  See also
  <link Overview.Codebot.Graphics.TSurfaceBitmap, TSurfaceBitmap members> }

type
  TSurfaceBitmap = class(TGraphic)
  private
    FBitmap: IBitmap;
    FOpacity: Byte;
    FWidth: Integer;
    FHeight: Integer;
    procedure SetOpacity(Value: Byte);
    procedure UpdateBitmap(B: IBitmap);
    function GetClientRect: TRectI;
    procedure HandleRelease;
    procedure HandleNeeded;
    procedure SetBitmap(Value: IBitmap);
    function GetSurface: ISurface;
    function GetFormat: TImageFormat;
    function GetFrames(Index: Integer): TRectI;
    function GetFrameCount: Integer;
    procedure SetFormat(Value: TImageFormat);
    function GetPixels: PPixel;
  protected
    {doc off}
    function GetEmpty: Boolean; override;
    function GetMimeType: string; override;
    procedure ReadData(Stream: TStream); override;
    procedure WriteData(Stream: TStream); override;
    procedure DefineProperties(Filer: TFiler); override;
    function GetTransparent: Boolean; override;
    procedure SetTransparent(Value: Boolean); override;
    function GetWidth: Integer; override;
    procedure SetWidth(Value: Integer); override;
    function GetHeight: Integer; override;
    procedure SetHeight(Value: Integer); override;
    procedure Draw(Canvas: TCanvas; const Rect: TRect); override; overload;
    {doc on}
  public
    constructor Create; override;
    {doc off}
    class function GetFileExtensions: string; override;
    class function IsStreamFormatSupported(Stream: TStream): Boolean; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    {doc on}
    { Set the underlying bitmap referenc directly }
    procedure CopyReference(Bitmap: IBitmap);
    { Draw the image on a canvas at x and y, optionaly picking a frame as the source }
    procedure Draw(Surface: ISurface; X, Y: Integer; FrameIndex: Integer = -1); overload;
    { Draw the image on a canvas stretching it from the source to dest rectangles }
    procedure Draw(Surface: ISurface; const Source, Dest: TRectF); overload;
    { Load a bitmap from a file }
    procedure LoadFromFile(const Filename: string); override;
    { Load a bitmap to a stream }
    procedure LoadFromStream(Stream: TStream); override;
    { Save a bitmap from a stream }
    procedure SaveToFile(const Filename: string); override;
    { Save a bitmap to a stream }
    procedure SaveToStream(Stream: TStream); override;
    { Output the mime types to a TStrings }
    procedure GetSupportedSourceMimeTypes(List: TStrings); override;
    { Convert the image to shades of a color }
    procedure Colorize(Color: TColorB);
    { Convert the image to grayscale when 1 }
    procedure Desaturate(Percent: Float);
    { Convert the image to white when 1 }
    procedure Lighten(Percent: Float);
    { Convert the image to black when 1 }
    procedure Darken(Percent: Float);
    { Alter a destination bitmap to width and height copy of pixels }
    procedure ResizeTo(Width, Height: Integer; Dest: TSurfaceBitmap);
    { Alter a destination bitmap to a copy of rect pixels }
    procedure ClipTo(const Rect: TRectI; Dest: TSurfaceBitmap);
    { Resample the image to width and height an optional quality }
    procedure Resample(Width, Height: Integer; Quality: TResampleQuality = rqNormal);
    { Resize the image erasing its contants and canvas }
    procedure SetSize(Width, Height: Integer);
    { Access to the underlying bitmap }
    property Bitmap: IBitmap read FBitmap write SetBitmap;
    { Access to the underlying bitmap's canvas }
    property Surface: ISurface read GetSurface;
    { Boundary rect of the image }
    property ClientRect: TRectI read GetClientRect;
    { Frames provides access to sub image locations when using TSurfaceBitmap as an image strip }
    property Frames[Index: Integer]: TRectI read GetFrames;
    { FramesCount is the number of frames available when using TSurfaceBitmap as an image strip }
    property FrameCount: Integer read GetFrameCount;
    { Direct access to image pixels }
    property Pixels: PPixel read GetPixels;
    { Image format used when loading or saving }
    property Format: TImageFormat read GetFormat write SetFormat;
    { Opacity controls the transparency of the image when drawing }
    property Opacity: Byte read FOpacity write SetOpacity;
  end;

{ TImageStrip is a series of images of the same height and width
  See also
  <link Overview.Codebot.Graphics.TImageStrip, TImageStrip members> }

  TImageStrip = class(TComponent)
  private
    FBitmap: TSurfaceBitmap;
    FUpdateRef: Integer;
    FOnChange: TNotifyDelegate;
    procedure SwapBitmap(B: TSurfaceBitmap);
    function GetCount: Integer;
    function GetSize: Integer;
    function GetOnChange: INotifyDelegate;
    procedure BitmapChanged(Sender: TObject);
  protected
    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);
    procedure DefineProperties(Filer: TFiler); override;
    { Notify component subscribers of changes }
    procedure Change; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure EndUpdate;
    { Clone the underlying bitmap object }
    procedure CloneTo(out Bitmap: IBitmap);
    { Copy the underlying bitmap object }
    procedure CopyTo(Bitmap: IBitmap);
    { Use the lanczos resampling method }
    procedure Resample(Size: Integer);
    { Scale the images in the center while leaving the original size }
    procedure Scale(Size: Integer);
    { Load a series of images }
    procedure LoadRange(const Files: TStrings);
    { Load an image list from a file }
    procedure LoadFromFile(const Filename: string);
    { Load an image list from a stream }
    procedure LoadFromStream(Stream: TStream);
    { Save an image list to a stream }
    procedure SaveToFile(const Filename: string);
    { Save an image list to a stream }
    procedure SaveToStream(Stream: TStream);
    procedure Draw(Surface: ISurface; Index: Integer;
      X, Y: Integer); overload;
    procedure Draw(Surface: ISurface; Index: Integer;
      X, Y: Integer; State: TDrawState); overload;
    procedure Draw(Surface: ISurface; Index: Integer;
      const Rect: TRectI; Opacity: Byte = $FF; Saturation: Float = 1.0); overload;
    procedure Add(Image: IBitmap); overload;
    procedure Add(Image: TSurfaceBitmap); overload;
    procedure Insert(Image: TSurfaceBitmap; Index: Integer);
    procedure Remove(Index: Integer);
    procedure Move(OldIndex, NewIndex: Integer);
    procedure Clear;
    property Count: Integer read GetCount;
    property Size: Integer read GetSize;
    { Allow component subscribers to add or remove change notification }
    property OnChange: INotifyDelegate read GetOnChange;
  end;

{ Drawing events }

  TNotifyIndexEvent = procedure (Sender: TObject; Index: Integer) of object;
  TDrawEvent = procedure (Sender: TObject; Surface: ISurface) of object;
  TDrawRectEvent = procedure (Sender: TObject; Surface: ISurface; Rect: TRectI) of object;
  TDrawStateEvent = procedure (Sender: TObject; Surface: ISurface; Rect: TRectI; State: TDrawState) of object;
  TDrawIndexEvent = procedure (Sender: TObject; Surface: ISurface; Index: Integer; Rect: TRectI; State: TDrawState) of object;
  TMeasureIndexEvent = procedure (Sender: TObject; Surface: ISurface; Index: Integer; out Size: Integer) of object;
  TDrawIndexDefaultEvent = procedure (Sender: TObject; Surface: ISurface; Index: Integer; Rect: TRectI; State: TDrawState; var DefaultDraw: Boolean) of object;
  TDrawStage = (dsPreProcess, dsProcessing, dsPostProcess);
  TDrawStageEvent = procedure (Sender: TObject; Surface: ISurface; Rect: TRectF; Stage: TDrawStage) of object;
  TCalculateRectEvent = procedure (Sender: TObject; var Rect: TRectI) of object;

{ Drawing routines which operate independent of a control }

procedure FillRectColor(Surface: ISurface; const Rect: TRectI; Color: TColorB; Radius: Float = 0);
procedure StrokeRectColor(Surface: ISurface; const Rect: TRectI; Color: TColorB; Radius: Float = 0);
procedure FillRectState(Surface: ISurface; const Rect: TRectI; State: TDrawState);
procedure FillRectSelected(Surface: ISurface; const Rect: TRectI; Radius: Float = 0);
function DrawDummyBitmap(Width, Height: Integer): IBitmap;
function DrawHueLinear(Width, Height: Integer): IBitmap;
function DrawHueRadial(Width, Height: Integer): IBitmap;
function DrawSaturationBox(Width, Height: Integer; Hue: Float): IBitmap;
function DrawDesaturationBox(Width, Height: Integer; Hue: Float): IBitmap;
procedure DrawShadow(Surface: ISurface; const Rect: TRectI; Direction: TDirection); overload;
function DrawShadow(Image: IBitmap; Darkness: Float): IBitmap; overload;
procedure DrawBitmap(Surface: ISurface; Bitmap: IBitmap; X, Y: Float; Alpha: Byte = $FF);

{ Bitmap resampling functions }

type
  TFilterFunction = function(Value: Single): Single;

  TSamplingFilter = (sfNearest, sfLinear, sfCosine, sfHermite, sfQuadratic,
    sfGaussian, sfSpline, sfLanczos, sfMitchell, sfCatmullRom);

{ Default resampling filter used for bicubic resizing }

const
  DefaultCubicFilter = sfCatmullRom;

{ Built-in filter functions }

var
  SamplingFilterFunctions : array[TSamplingFilter] of TFilterFunction;

{ Default radii of built-in filter functions }

  SamplingFilterRadii: array[TSamplingFilter] of Float;

{ Resamples rectangle in source image to rectangle in destination image
  with resampling. You can use custom sampling function and filter radius.

  Set WrapEdges to True for seamlessly tileable images }

procedure ResampleBitmap(Src: IBitmap; SrcX, SrcY, SrcWidth, SrcHeight: LongInt;
  Dst: IBitmap; DstX, DstY, DstWidth, DstHeight: LongInt;
  Filter: TFilterFunction; Radius: Single; WrapEdges: Boolean); overload;
function ResampleBitmap(Bitmap: IBitmap; Width, Height: Integer;
  Filter: TSamplingFilter; WrapEdges: Boolean = False): IBitmap; overload;

{ Perform a gaussian blur on a bitmap }

procedure BlurBitmap(Bits: PPixel; W, H: Integer; const Radius: Double); overload;
function BlurBitmap(Bitmap: IBitmap; const Radius: Double): IBitmap; overload;

{ Draw an easing function as a graph }

procedure DrawEasing(Surface: ISurface; Font: IFont; Rect: TRectF;
  Easing: TEasing; Reverse: Boolean; Time: Float);

{ Brushes creates a series of bitmap batterns }

const
  DefPenWidth = 1;
  DefBrushSize = 9;

type
  Brushes = record
    class function Transparent: IBrush; overload; static;
    class function Dither(Foreground, Background: TColorB; PenWidth: Float = DefPenWidth; BrushSize: Integer = DefBrushSize): IBrush; overload; static;
    class function Checker(Foreground, Background: TColorB; PenWidth: Float = DefPenWidth; BrushSize: Integer = DefBrushSize): IBrush; overload; static;
    class function Beam(Foreground, Background: TColorB; PenWidth: Float = DefPenWidth; BrushSize: Integer = DefBrushSize): IBrush; overload; static;
    class function Cross(Foreground, Background: TColorB; PenWidth: Float = DefPenWidth; BrushSize: Integer = DefBrushSize): IBrush; overload; static;
    class function ZigZag(Foreground, Background: TColorB; PenWidth: Float = DefPenWidth; BrushSize: Integer = DefBrushSize): IBrush; overload; static;
    class function Brick(Foreground, Background: TColorB; PenWidth: Float = DefPenWidth; BrushSize: Integer = DefBrushSize): IBrush; overload; static;
    class function Circles(Foreground, Background: TColorB; PenWidth: Float = DefPenWidth; BrushSize: Integer = DefBrushSize): IBrush; overload; static;
    class function Squares(Foreground, Background: TColorB; PenWidth: Float = DefPenWidth; BrushSize: Integer = DefBrushSize): IBrush; overload; static;
    class function Clovers(Foreground, Background: TColorB; PenWidth: Float = DefPenWidth; BrushSize: Integer = DefBrushSize): IBrush; overload; static;
    class function Tooth(Foreground, Background: TColorB; PenWidth: Float = DefPenWidth; BrushSize: Integer = DefBrushSize): IBrush; overload; static;
    class function FloorTile(Foreground, Background: TColorB; PenWidth: Float = DefPenWidth; BrushSize: Integer = DefBrushSize): IBrush; overload; static;
    class function SnakeSkin(Foreground, Background: TColorB; PenWidth: Float = DefPenWidth; BrushSize: Integer = DefBrushSize): IBrush; overload; static;
    class function Pipes(Foreground, Background: TColorB; PenWidth: Float = DefPenWidth; BrushSize: Integer = DefBrushSize): IBrush; overload; static;
    //class function Pipes(Foreground, Background: TColorB; PenWidth: Float = DefPenWidth; BrushSize: Integer = DefBrushSize): IBrush; overload; static;
  end;

  TBrushStyle = function(Foreground, Background: TColorB; PenWidth: Float = DefPenWidth; BrushSize: Integer = DefBrushSize): IBrush;

function StrToBrush(Name: string; Foreground, Background: TColorB; PenWidth: Float = DefPenWidth; BrushSize: Integer = DefBrushSize): IBrush;
procedure RegisterBrushStyle(Name: string; BrushStyle: TBrushStyle);
function EnumBrushStyles: TNamedEnumerable;

const
  DefaulHeaderHeight = 96;
  DefaultFooterHeight = 41;

{ TDrawControlHelper }

type
  TDrawControlHelper = class helper for TControl
  private
    function GetCurrentColor: TColorB;
    function GetParentCurrentColor: TColorB;
  public
    function TextHeight: Integer;
    function TextSize(const Text: string): TPointI;
    procedure DrawDummyBlock(Surface: ISurface; const Rect: TRectI; State: TDrawState);
    procedure DrawBitmap(Surface: ISurface; Bitmap: IBitmap; X, Y: Integer; Alpha: Byte = $FF);
    procedure DrawCaption(Surface: ISurface; const Caption: string; const Rect: TRectI; Enabled: Boolean = True);
    procedure DrawText(Surface: ISurface; const Text: string; const Rect: TRectI; Direction: TDirection);
    procedure DrawTextState(Surface: ISurface; const Text: string; const Rect: TRectI; State: TDrawState; Radius: Float = 0);
    procedure DrawRectState(Surface: ISurface; const Rect: TRectI; State: TDrawState; Radius: Float = 0);
    property CurrentColor: TColorB read GetCurrentColor;
    property ParentCurrentColor: TColorB read GetParentCurrentColor;
  end;

{ TTheme }

  TThemeOrientation = (toVertical, toHorizontal);

  TTheme = class
  private
    class function GetSelected: Boolean; static;
  public
    class var Control: TControl;
    class var Surface: ISurface;
    class var Font: IFont;
    class var State: TDrawState;
    class procedure Select(Control: TControl; Surface: ISurface; State: TDrawState; Font: TFont); overload;
    class procedure Select(State: TDrawState); overload;
    class procedure Select(ThemeName: string); overload;
    class procedure Deselect;
    class function Name: string;
    class procedure DrawButton(const Rect: TRectI); virtual; abstract;
    class procedure DrawButtonThin(const Rect: TRectI); virtual; abstract;
    class procedure DrawSplit(const Rect: TRectI; Orientation: TThemeOrientation); virtual; abstract;
    class function MeasureThumbThin(Orientation: TThemeOrientation): TPointI; virtual; abstract;
    class procedure DrawThumbThin(const Rect: TRectI; Orientation: TThemeOrientation); virtual; abstract;
    class function MeasureBorder: TPointI; virtual; abstract;
    class procedure DrawBorder(const Rect: TRectI); virtual; abstract;
    class function MeasureEditBorder: TPointI; virtual; abstract;
    class procedure DrawEditBorder(const Rect: TRectI); virtual; abstract;
    class procedure DrawHeaderColumn(const Rect: TRectI; Sort: TSortingOrder = soNone); virtual; abstract;
    class procedure DrawHeader(Height: Integer = DefaulHeaderHeight); virtual; abstract;
    class procedure DrawHeaderShadow(Top: Integer = 0); virtual; abstract;
    class procedure DrawFooter(Height: Integer = DefaultFooterHeight); virtual; abstract;
    class procedure DrawFooterGrip; virtual; abstract;
    class property Selected: Boolean read GetSelected;
  end;

{ TThemeClass }

  TThemeClass = class of TTheme;

{ TThemeClassArray }

  TThemeClassArray = TArrayList<TThemeClass>;

{doc off}
{ TDefaultTheme }

  TDefaultTheme = class(TTheme)
    class procedure DrawButton(const Rect: TRectI); override;
    class procedure DrawButtonThin(const Rect: TRectI); override;
    class procedure DrawSplit(const Rect: TRectI; Orientation: TThemeOrientation); override;
    class function MeasureThumbThin(Orientation: TThemeOrientation): TPointI; override;
    class procedure DrawThumbThin(const Rect: TRectI; Orientation: TThemeOrientation); override;
    class function MeasureBorder: TPointI; override;
    class procedure DrawBorder(const Rect: TRectI); override;
    class function MeasureEditBorder: TPointI; override;
    class procedure DrawEditBorder(const Rect: TRectI); override;
    class procedure DrawHeaderColumn(const Rect: TRectI; Sort: TSortingOrder = soNone); override;
    class procedure DrawHeader(Height: Integer = DefaulHeaderHeight); override;
    class procedure DrawHeaderShadow(Top: Integer = 0); override;
    class procedure DrawFooter(Height: Integer = DefaultFooterHeight); override;
    class procedure DrawFooterGrip; override;
  end;

{ TRedmondTheme }

  TRedmondTheme = class(TTheme)
    class procedure DrawButton(const Rect: TRectI); override;
    class procedure DrawButtonThin(const Rect: TRectI); override;
    class procedure DrawSplit(const Rect: TRectI; Orientation: TThemeOrientation); override;
    class function MeasureThumbThin(Orientation: TThemeOrientation): TPointI; override;
    class procedure DrawThumbThin(const Rect: TRectI; Orientation: TThemeOrientation); override;
    class function MeasureBorder: TPointI; override;
    class procedure DrawBorder(const Rect: TRectI); override;
    class function MeasureEditBorder: TPointI; override;
    class procedure DrawEditBorder(const Rect: TRectI); override;
    class procedure DrawHeaderColumn(const Rect: TRectI; Sort: TSortingOrder = soNone); override;
    class procedure DrawHeader(Height: Integer = DefaulHeaderHeight); override;
    class procedure DrawHeaderShadow( Top: Integer = 0); override;
    class procedure DrawFooter(Height: Integer = DefaultFooterHeight); override;
    class procedure DrawFooterGrip; override;
  end;
{doc on}

function Theme: TThemeClass;
procedure ThemeRegisiter(ThemeClass: TThemeClass);
function ThemeFind(const ThemeName: string): TThemeClass;
procedure ThemeChange(const ThemeName: string);
procedure ThemeNotifyAdd(Event: TMethodEvent);
procedure ThemeNotifyRemove(Event: TMethodEvent);
procedure ThemeNames(Strings: TStrings);

implementation

{$ifdef linuxgtk}
uses
  Codebot.Graphics.Linux.SurfaceCairo;

function NewMatrix: IMatrix;
begin
  Result := NewMatrixCairo;
end;

function NewPen(Brush: IBrush; Width: Float): IPen;
begin
  Result := NewPenCairo(Brush, Width);
end;

function NewPen(Color: TBGRA; Width: Float): IPen;
begin
  Result := NewPenCairo(Color, Width);
end;

function NewBrush(Color: TBGRA): ISolidBrush;
begin
  Result := NewSolidBrushCairo(Color);
end;

function NewBrush(Bitmap: IBitmap): IBitmapBrush;
begin
  Result := NewBitmapBrushCairo(Bitmap);
end;

function NewBrush(X1, Y1, X2, Y2: Float): ILinearGradientBrush;
begin
  Result := NewLinearGradientBrushCairo(X1, Y1, X2, Y2);
end;

function NewBrush(const A, B: TPointF): ILinearGradientBrush;
begin
  Result := NewLinearGradientBrushCairo(A, B);
end;

function NewBrush(const Rect: TRectF): IRadialGradientBrush;
begin
  Result := NewRadialGradientBrushCairo(Rect);
end;

function NewFont(const FontName: string; FontSize: Integer = 10): IFont;
begin
  Result := NewFontCairo(FontName, FontSize);
end;

function NewFont(Font: TFont = nil): IFont;
begin
  Result := NewFontCairo(Font);
end;

function NewSurface(Canvas: TCanvas): ISurface;
begin
  Result := NewSurfaceCairo(Canvas);
end;

function NewSurface(Control: TWinControl): ISurface;
begin
  Result := NewSurfaceCairo(Control);
end;

function NewBitmap(Width: Integer = 0; Height: Integer = 0): IBitmap;
begin
  Result := NewBitmapCairo(Width, Height);
end;

function NewSplash: ISplash;
begin
  Result := NewSplashCairo;
end;
{$endif}

{$ifdef linuxqt}
function NewMatrix: IMatrix;
begin
  Result := nil;
end;

function NewPen(Brush: IBrush; Width: Float): IPen;
begin
  Result := nil;
end;

function NewPen(Color: TBGRA; Width: Float): IPen;
begin
  Result := nil;
end;

function NewBrush(Color: TBGRA): ISolidBrush;
begin
  Result := nil;
end;

function NewBrush(Bitmap: IBitmap): IBitmapBrush;
begin
  Result := nil;
end;

function NewBrush(X1, Y1, X2, Y2: Float): ILinearGradientBrush;
begin
  Result := nil;
end;

function NewBrush(const A, B: TPointF): ILinearGradientBrush;
begin
  Result := nil;
end;

function NewBrush(const Rect: TRectF): IRadialGradientBrush;
begin
  Result := nil;
end;

function NewFont(const FontName: string; FontSize: Integer = 10): IFont;
begin
  Result := nil;
end;

function NewFont(Font: TFont = nil): IFont;
begin
  Result := nil;
end;

function NewSurface(Canvas: TCanvas): ISurface;
begin
  Result := nil;
end;

function NewSurface(Control: TWinControl): ISurface;
begin
  Result := nil;
end;

function NewBitmap(Width: Integer = 0; Height: Integer = 0): IBitmap;
begin
  Result := nil;
end;

function NewSplash: ISplash;
begin
  Result := nil;
end;
{$endif}

{$ifdef windows}
uses
  Codebot.Graphics.Windows.InterfacedBitmap,
  Codebot.Graphics.Windows.SurfaceD2D,
  Codebot.Graphics.Windows.SurfaceGdiPlus;

function NewMatrix: IMatrix;
begin
  if LoadD2D then
    Result := NewMatrixD2D
  else
    Result := NewMatrixGdi;
end;

function NewPen(Brush: IBrush; Width: Float): IPen;
begin
  if LoadD2D then
    Result := NewPenD2D(Brush, Width)
  else
    Result := NewPenGdi(Brush, Width);
end;

function NewPen(Color: TBGRA; Width: Float): IPen;
begin
  if LoadD2D then
    Result := NewPenD2D(Color, Width)
  else
    Result := NewPenGdi(Color, Width);
end;

function NewBrush(Color: TBGRA): ISolidBrush;
begin
  if LoadD2D then
    Result := NewSolidBrushD2D(Color)
  else
    Result := NewSolidBrushGdi(Color);
end;

function NewBrush(Bitmap: IBitmap): IBitmapBrush;
begin
  if LoadD2D then
    Result := NewBitmapBrushD2D(Bitmap)
  else
    Result := NewBitmapBrushGdi(Bitmap);
end;

function NewBrush(X1, Y1, X2, Y2: Float): ILinearGradientBrush;
begin
  if LoadD2D then
    Result := NewLinearGradientBrushD2D(X1, Y1, X2, Y2)
  else
    Result := NewLinearGradientBrushGdi(X1, Y1, X2, Y2);
end;

function NewBrush(const A, B: TPointF): ILinearGradientBrush;
begin
  if LoadD2D then
    Result := NewLinearGradientBrushD2D(A, B)
  else
    Result := NewLinearGradientBrushGdi(A, B);
end;

function NewBrush(const Rect: TRectF): IRadialGradientBrush;
begin
  if LoadD2D then
    Result := NewRadialGradientBrushD2D(Rect)
  else
    Result := NewRadialGradientBrushGdi(Rect);
end;

function NewFont(Font: TFont): IFont;
begin
  if LoadD2D then
    Result := NewFontD2D(Font)
  else
    Result := NewFontGdi(Font);
end;

function NewFont(const FontName: string; FontSize: Integer = 10): IFont;
var
  F: TFont;
begin
  F := TFont.Create;
  try
    F.Name := FontName;
    F.Size := FontSize;
    Result := NewFont(F);
  finally
    F.Free;
  end;
end;

function NewSurface(Canvas: TCanvas): ISurface;
begin
  if LoadD2D then
    Result := NewSurfaceD2D(Canvas)
  else
    Result := NewSurfaceGdi(Canvas);
end;

function NewSurface(Control: TWinControl): ISurface;
begin
  if LoadD2D then
    Result := NewSurfaceD2D(Control)
  else
    Result := NewSurfaceGdi(Control);
end;

function NewBitmap(Width: Integer = 0; Height: Integer = 0): IBitmap;
begin
  if LoadD2D then
    Result := NewBitmapD2D(Width, Height)
  else
    Result := NewBitmapGdi(Width, Height);
end;

function NewBitmapD2DStub: IBitmap;
begin
  Result := NewBitmapD2D(0, 0);
end;

function NewBitmapGdiStub: IBitmap;
begin
  Result := NewBitmapGdi(0, 0);
end;

function NewSplash: ISplash;
begin
  if LoadD2D then
    NewBitmapProc := NewBitmapD2DStub
  else
    NewBitmapProc := NewBitmapGdiStub;
  Result := NewSplashWin;
end;
{$endif}

{ TSurfaceBitmap }

constructor TSurfaceBitmap.Create;
begin
  inherited Create;
  FBitmap := NewBitmap(0, 0);
  FOpacity := High(Byte);
end;

procedure TSurfaceBitmap.CopyReference(Bitmap: IBitmap);
begin
  UpdateBitmap(Bitmap);
  FWidth := Bitmap.Width;
  FHeight := Bitmap.Height;
end;

procedure TSurfaceBitmap.UpdateBitmap(B: IBitmap);
var
  F: TImageFormat;
begin
  if B <> FBitmap then
  begin
    F := FBitmap.Format;
    FBitmap := B;
    FBitmap.Format := F;
    Modified := True;
  end;
end;

procedure TSurfaceBitmap.SetOpacity(Value: Byte);
begin
  if FOpacity = Value then Exit;
  FOpacity := Value;
  Modified := True;
end;

procedure TSurfaceBitmap.HandleRelease;
begin
  if not FBitmap.Empty then
    UpdateBitmap(NewBitmap(0, 0));
end;

function TSurfaceBitmap.GetClientRect: TRectI;
begin
  Result := TRectI.Create(Width, Height);
end;

procedure TSurfaceBitmap.HandleNeeded;
begin
  if FBitmap.Empty and (FWidth > 0) and (FHeight > 0) then
    UpdateBitmap(NewBitmap(FWidth, FHeight));
end;

procedure TSurfaceBitmap.SetBitmap(Value: IBitmap);
begin
  if Value = FBitmap then
    Exit;
  if Value = nil then
  begin
    FBitmap := NewBitmap;
    FWidth := 0;
    FHeight := 0;
  end
  else
  begin
    FBitmap := Value.Clone;
    FWidth := FBitmap.Width;
    FHeight := FBitmap.Height;
  end;
end;

function TSurfaceBitmap.GetSurface: ISurface;
begin
  HandleNeeded;
  Result := FBitmap.Surface;
end;

function TSurfaceBitmap.GetFormat: TImageFormat;
begin
  Result := FBitmap.Format;
end;

function TSurfaceBitmap.GetFrames(Index: Integer): TRectI;
begin
  Result := TRectI.Create;
  if Empty then
    Exit;
  Result := TRectI.Create(FWidth, FHeight);
  if Width > Height then
  begin
    Result.Width := Height;
    Result.Offset(Index * Height, 0);
  end
  else
  begin
    Result.Height := Width;
    Result.Offset(0, Index * Width);
  end;
end;

function TSurfaceBitmap.GetFrameCount: Integer;
begin
  Result := 0;
  if Empty then
    Exit;
  if Width > Height then
    Result := Width div Height
  else
    Result := Height div Width;
end;

procedure TSurfaceBitmap.SetFormat(Value: TImageFormat);
begin
  FBitmap.Format := Value;
end;

function TSurfaceBitmap.GetPixels: PPixel;
begin
  HandleNeeded;
  Result := FBitmap.Pixels;
end;

function TSurfaceBitmap.GetEmpty: Boolean;
begin
  Result := (Width < 1) or (Height < 1);
end;

function TSurfaceBitmap.GetMimeType: string;
begin
  Result := ImageFormatToMimeType(FBitmap.Format);
end;

procedure TSurfaceBitmap.ReadData(Stream: TStream);
begin
  LoadFromStream(Stream);
end;

procedure TSurfaceBitmap.WriteData(Stream: TStream);
begin
  if Format = fmIco then
    Format := fmPng;
  SaveToStream(Stream);
end;

procedure TSurfaceBitmap.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('Data', ReadData, WriteData, not Empty);
end;

function TSurfaceBitmap.GetTransparent: Boolean;
begin
  Result := True;
end;

procedure TSurfaceBitmap.SetTransparent(Value: Boolean);
begin
end;

function TSurfaceBitmap.GetWidth: Integer;
begin
  if FBitmap.Empty then
    Result := FWidth
  else
    Result := Bitmap.Width;
end;

procedure TSurfaceBitmap.SetWidth(Value: Integer);
begin
  if Value < 1 then
    Value := 0;
  if Value <> Width then
  begin
    FWidth := Value;
    HandleRelease;
  end;
end;

function TSurfaceBitmap.GetHeight: Integer;
begin
  if FBitmap.Empty then
    Result := FHeight
  else
    Result := Bitmap.Height;
end;

procedure TSurfaceBitmap.SetHeight(Value: Integer);
begin
  if Value < 1 then
    Value := 0;
  if Value <> Height then
  begin
    FHeight := Value;
    HandleRelease;
  end;
end;

procedure TSurfaceBitmap.Draw(Canvas: TCanvas; const Rect: TRect);
var
  Surface: ISurface;
begin
  if Empty then
    Exit;
  HandleNeeded;
  Surface := NewSurface(Canvas);
  if Surface <> nil then
    FBitmap.Surface.CopyTo(FBitmap.ClientRect, Surface, Rect, Opacity);
end;

procedure TSurfaceBitmap.Draw(Surface: ISurface; X, Y: Integer; FrameIndex: Integer = -1);
var
  Source, Dest: TRectI;
begin
  if Empty or (Surface = nil) then
    Exit;
  HandleNeeded;
  if FrameIndex < 0 then
  begin
    Source := FBitmap.ClientRect;
    Dest := Source;
  end
  else
  begin
    Source := Frames[FrameIndex];
    Dest := TRectI.Create(Source.Width, Source.Height);
  end;
  Dest.Offset(X, Y);
  FBitmap.Surface.CopyTo(Source, Surface, Dest, FOpacity);
end;

procedure TSurfaceBitmap.Draw(Surface: ISurface; const Source, Dest: TRectF);
begin
  if Empty or (Surface = nil) then
    Exit;
  HandleNeeded;
  FBitmap.Surface.CopyTo(Source, Surface, Dest, FOpacity);
end;

class function TSurfaceBitmap.GetFileExtensions: string;
begin
  Result := inherited GetFileExtensions;
end;

class function TSurfaceBitmap.IsStreamFormatSupported(Stream: TStream): Boolean;
begin
  Result := inherited IsStreamFormatSupported(Stream);
end;

procedure TSurfaceBitmap.Assign(Source: TPersistent);
var
  Image: TSurfaceBitmap;
  Graphic: TGraphic;
  Stream: TStream;
begin
  if Source = Self then
    Exit;
  if Source is TSurfaceBitmap then
  begin
    Image := Source as TSurfaceBitmap;
    FBitmap := Image.Bitmap.Clone;
    FOpacity := Image.Opacity;
    FWidth := FBitmap.Width;
    FHeight := FBitmap.Height;
    Modified := True;
  end
  else if Source is TGraphic then
  begin
    Graphic := Source as TGraphic;
    Stream := TMemoryStream.Create;
    try
      Graphic.SaveToStream(Stream);
      Stream.Position := 0;
      LoadFromStream(Stream);
      Modified := True;
    finally
      Stream.Free;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TSurfaceBitmap.Clear;
begin
  FBitmap.SetSize(0, 0);
  FWidth := 0;
  FHeight := 0;
  Modified := True;
end;

procedure TSurfaceBitmap.LoadFromFile(const Filename: string);
begin
  FBitmap.LoadFromFile(FileName);
  FWidth := FBitmap.Width;
  FHeight := FBitmap.Height;
  Modified := True;
end;

procedure TSurfaceBitmap.SaveToFile(const Filename: string);
begin
  FBitmap.SaveToFile(FileName);
end;

procedure TSurfaceBitmap.LoadFromStream(Stream: TStream);
begin
  if Stream.Size = 0 then
    Exit;
  FBitmap.LoadFromStream(Stream);
  FWidth := FBitmap.Width;
  FHeight := FBitmap.Height;
  Modified := True;
end;

procedure TSurfaceBitmap.SaveToStream(Stream: TStream);
begin
  FBitmap.SaveToStream(Stream);
end;

procedure TSurfaceBitmap.GetSupportedSourceMimeTypes(List: TStrings);
var
  M: TImageFormat;
begin
  for M := Low(M) to High(M) do
    List.Add(ImageFormatToMimeType(M));
end;

procedure TSurfaceBitmap.Colorize(Color: TColorB);
var
  Shades: array[Low(Byte)..High(Byte)] of TColorB;
  C: TColorB;
  P: PPixel;
  M: Float;
  I: Integer;
begin
  if Empty then
    Exit;
  HandleNeeded;
  for I := Low(Shades) to High(Shades) do
  begin
    if I > $7F then
      Shades[I] := Color.Lighten((I - $7F) / $7F)
    else
      Shades[I] := Color.Darken(1 - I / $7F);
  end;
  P := FBitmap.Pixels;
  for I := 1 to FBitmap.Width * FBitmap.Height do
  begin
    C := P^;
    P^ := Shades[C.Desaturate(1).Red];
    P.Alpha := C.Alpha;
    M := C.Alpha / $FF;
    P.Red := Round(P.Red * M);
    P.Green := Round(P.Green * M);
    P.Blue := Round(P.Blue * M);
    Inc(P);
  end;
end;

procedure TSurfaceBitmap.Desaturate(Percent: Float);
var
  P: PPixel;
  M: Float;
  I: Integer;
begin
  if Empty then
    Exit;
  HandleNeeded;
  Percent := Clamp(Percent);
  if Percent = 0 then
    Exit;
  P := FBitmap.Pixels;
  for I := 1 to FBitmap.Width * FBitmap.Height do
  begin
    P^ := P.Desaturate(Percent);
    M := P.Alpha / $FF;
    P.Red := Round(P.Red * M);
    P.Green := Round(P.Green * M);
    P.Blue := Round(P.Blue * M);
    Inc(P);
  end;
end;

procedure TSurfaceBitmap.Lighten(Percent: Float);
var
  P: PPixel;
  I: Integer;
begin
  if Empty then
    Exit;
  HandleNeeded;
  Percent := Clamp(Percent);
  if Percent = 0 then
    Exit;
  P := FBitmap.Pixels;
  for I := 1 to FBitmap.Width * FBitmap.Height do
  begin
    P^ := P.Lighten(Percent);
    Inc(P);
  end;
end;

procedure TSurfaceBitmap.Darken(Percent: Float);
var
  P: PPixel;
  I: Integer;
begin
  if Empty then
    Exit;
  HandleNeeded;
  Percent := Clamp(Percent);
  if Percent = 0 then
    Exit;
  P := FBitmap.Pixels;
  for I := 1 to FBitmap.Width * FBitmap.Height do
  begin
    P^ := P.Darken(Percent);
    Inc(P);
  end;
end;

procedure TSurfaceBitmap.ResizeTo(Width, Height: Integer; Dest: TSurfaceBitmap);
begin
  Dest.SetSize(Width, Height);
  if Dest.Empty then Exit;
  Draw(Dest.Surface, 0, 0);
end;

procedure TSurfaceBitmap.ClipTo(const Rect: TRectI; Dest: TSurfaceBitmap);
var
  R: TRectI;
begin
  Dest.SetSize(Rect.Width, Rect.Height);
  if Dest.Empty then Exit;
  R := Rect;
  R.X := 0;
  R.Y := 0;
  Draw(Dest.Surface, Rect, R);
end;

procedure TSurfaceBitmap.Resample(Width, Height: Integer;
  Quality: TResampleQuality);
begin
  if Empty then
    Exit;
  if Width < 1 then
    Width := 0;
  if Height < 1 then
    Height := 0;
  if (Width < 1) or (Height  < 1) then
  begin
    SetSize(Width, Height);
    Exit;
  end;
  if (Width = Self.Width) or (Height = Self.Height) then
    Exit;
  HandleNeeded;
  UpdateBitmap(FBitmap.Resample(Width, Height, Quality));
  FWidth := Width;
  FHeight := Height;
  Modified := True;
end;

procedure TSurfaceBitmap.SetSize(Width, Height: Integer);
begin
  if Width < 1 then
    Width := 0;
  if Height < 0 then
    Height := 0;
  if (Width <> Self.Width) or (Height <> Self.Height) then
  begin
    FWidth := Width;
    FHeight := Height;
    HandleRelease;
  end;
end;

{ TImageStrip }

constructor TImageStrip.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SwapBitmap(TSurfaceBitmap.Create);
end;

destructor TImageStrip.Destroy;
begin
  FBitmap.Free;
  inherited Destroy;
end;

procedure TImageStrip.BeginUpdate;
begin
  Inc(FUpdateRef);
end;

procedure TImageStrip.EndUpdate;
begin
  Dec(FUpdateRef);
  Change;
end;

procedure TImageStrip.SwapBitmap(B: TSurfaceBitmap);
begin
  FBitmap.Free;
  FBitmap := B;
  FBitmap.OnChange := BitmapChanged;
  Change;
end;

procedure TImageStrip.BitmapChanged(Sender: TObject);
begin
  Change;
end;

procedure TImageStrip.LoadFromFile(const Filename: string);
begin
  FBitmap.LoadFromFile(Filename);
  Change;
end;

procedure TImageStrip.LoadFromStream(Stream: TStream);
begin
  FBitmap.LoadFromStream(Stream);
  Change;
end;

procedure TImageStrip.SaveToFile(const Filename: string);
begin
  FBitmap.SaveToFile(Filename);
end;

procedure TImageStrip.SaveToStream(Stream: TStream);
begin
  FBitmap.SaveToStream(Stream);
end;

procedure TImageStrip.ReadData(Stream: TStream);
begin
  FBitmap.LoadFromStream(Stream);
end;

procedure TImageStrip.WriteData(Stream: TStream);
begin
  FBitmap.SaveToStream(Stream);
end;

procedure TImageStrip.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('Data', ReadData, WriteData, not FBitmap.Empty);
end;

procedure TImageStrip.Change;
var
  Event: TNotifyEvent;
begin
  if FUpdateRef < 1 then
    for Event in FOnChange do
      Event(Self);
end;

function TImageStrip.GetOnChange: INotifyDelegate;
begin
  Result := FOnChange;
end;

function TImageStrip.GetCount: Integer;
begin
  Result := FBitmap.FrameCount;
end;

function TImageStrip.GetSize: Integer;
begin
  Result := FBitmap.Frames[0].Height;
end;

procedure TImageStrip.CloneTo(out Bitmap: IBitmap);
begin
  if FBitmap.Empty  then
    Bitmap := NewBitmap
  else
    Bitmap := FBitmap.Bitmap.Clone;
end;

procedure TImageStrip.CopyTo(Bitmap: IBitmap);
var
  B: IBitmap;
  R: TRectI;
begin
  Bitmap.Clear;
  if not FBitmap.Empty  then
  begin
    B := FBitmap.Bitmap;
    R := B.ClientRect;
    Bitmap.SetSize(R.Width, R.Height);
    B.Surface.CopyTo(R, Bitmap.Surface, R);
  end;
end;

procedure TImageStrip.Resample(Size: Integer);
begin
  if FBitmap.Empty then
    Exit;
  if Size < 1 then Exit;
  if Size > 1024 then Exit;
  FBitmap.Bitmap := ResampleBitmap(FBitmap.Bitmap, Size * Count, Size, sfLanczos);
end;

procedure TImageStrip.Scale(Size: Integer);
var
  Source, Dest, Final: IBitmap;
  Current, Offset: Integer;
  I: Integer;
begin
  if FBitmap.Empty then
    Exit;
  if Size < 1 then Exit;
  if Size > 1024 then Exit;
  Current := Self.Size;
  if Size = Current then Exit;
  Offset := (Current - Size) div 2;
  Final := NewBitmap(Count * Current, Current);
  Source := NewBitmap(Current, Current);
  for I := 0 to Count - 1 do
  begin
    Source.Surface.Clear(clTransparent);
    Draw(Source.Surface, I, 0, 0);
    Dest := ResampleBitmap(Source, Size, Size, sfLanczos);
    DrawBitmap(Final.Surface, Dest, I * Current + Offset, Offset);
  end;
  Clear;
  Add(Final);
end;

procedure TImageStrip.LoadRange(const Files: TStrings);
var
  B, T: TSurfaceBitmap;
  S, D: TRectI;
  I, J: Integer;
begin
  if Files.Count < 1 then
    Exit;
  if Files.Count = 1 then
  begin
    B := TSurfaceBitmap.Create;
    try
      B.LoadFromFile(Files[0]);
      Add(B);
    finally
      B.Free;
    end;
    Exit;
  end;
  BeginUpdate;
  try
    I := 0;
    if Count = 0 then
    begin
      LoadFromFile(Files[0]);
      Inc(I);
      if Files.Count = 1 then
        Exit;
    end;
    B := TSurfaceBitmap.Create;
    T := TSurfaceBitmap.Create;
    try
      FBitmap.ResizeTo(FBitmap.Width + (Files.Count - I) * Size,
        FBitmap.Height, B);
      S := TRectI.Create(Size, Size);
      D := S;
      D.X := Count * S.Width;
      for J := I to Files.Count - 1 do
      begin
        T.LoadFromFile(Files[J]);
        T.Surface.CopyTo(S, B.Surface, D);
        D.X := D.X + S.Width;
      end;
      SwapBitmap(B);
      B := nil;
    finally
      T.Free;
      B.Free;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TImageStrip.Add(Image: TSurfaceBitmap);
var
  Count: Integer;
  Size: Integer;
  S, D: TRectI;
  B: TSurfaceBitmap;
  I: Integer;
begin
  if Image.Empty then
    Exit;
  Count := FBitmap.FrameCount;
  if FBitmap.Empty then
    Size := Image.Frames[0].Height
  else
    Size := FBitmap.Frames[0].Height;
  B := TSurfaceBitmap.Create;
  FBitmap.ResizeTo((Count + Image.FrameCount) * Size, Size, B);
  for I := 0 to Image.FrameCount - 1 do
  begin
    S := Image.Frames[I];
    S.Width := Size;
    S.Height := Size;
    D := TRectI.Create(Size, Size);
    D.X := (Count + I) * Size;
    Image.Draw(B.Surface, S, D);
  end;
  SwapBitmap(B);
end;

procedure TImageStrip.Add(Image: IBitmap);
var
  B: TSurfaceBitmap;
begin
  B := TSurfaceBitmap.Create;
  try
    B.CopyReference(Image);
    Add(B);
  finally
    B.Free;
  end;
end;

procedure TImageStrip.Insert(Image: TSurfaceBitmap; Index: Integer);
var
  B: TSurfaceBitmap;
  S, D: TRectI;
  I, J: Integer;
begin
  if Image.Empty then
    Exit;
  if FBitmap.Empty then
  begin
    Add(Image);
    Exit;
  end;
  if Index > Count - 1 then
  begin
    Add(Image);
    Exit;
  end;
  if Index < 0 then
    Index := 0;
  B := TSurfaceBitmap.Create;
  B.Width := (Count + 1) * Size;
  B.Height := Size;
  S := TRectI.Create(Size, Size);
  J := 0;
  for I := 0 to Count - 1 do
  begin
    if I = Index then
    begin
      D := S;
      D.X := I * Size;
      Image.Draw(B.Surface, S, D);
      Inc(J);
    end;
    FBitmap.Draw(B.Surface, J * Size, 0, I);
    Inc(J);
  end;
  SwapBitmap(B);
end;

procedure TImageStrip.Remove(Index: Integer);
var
  B: TSurfaceBitmap;
  I, J: Integer;
begin
  if (Index < 0) or (Index > Count - 1) then
    Exit;
  if Count = 1 then
    Clear
  else
  begin
    B := TSurfaceBitmap.Create;
    B.Width := (Count - 1) * Size;
    B.Height := Size;
    J := 0;
    for I := 0 to Count - 1 do
    begin
      if I = Index then
        Continue;
      FBitmap.Draw(B.Surface, J * Size, 0, I);
      Inc(J);
    end;
    SwapBitmap(B);
  end;
end;

procedure TImageStrip.Move(OldIndex, NewIndex: Integer);
var
  RefFrom, RefTo: PInteger;
  B: TSurfaceBitmap;
  Positions: IntArray;
  I: Integer;
begin
  if (OldIndex < 0) or (OldIndex > Count - 1) then
    Exit;
  if (NewIndex < 0) or (NewIndex > Count - 1) then
    Exit;
  B := TSurfaceBitmap.Create;
  B.Width := Count * Size;
  B.Height := Size;
  Positions.Length := Count;
  for I := 0 to Count - 1 do
    Positions[I] := I;
  I := Positions[OldIndex];
  if NewIndex > OldIndex then
  begin
    RefFrom := @Positions.Items[OldIndex + 1];
    RefTo := @Positions.Items[OldIndex];
    System.Move(RefFrom^, RefTo^, (NewIndex - OldIndex) * SizeOf(Integer));
  end
  else
  begin
    RefFrom := @Positions.Items[NewIndex];
    RefTo := @Positions.Items[NewIndex + 1];
    System.Move(RefFrom^, RefTo^, (OldIndex - NewIndex) * SizeOf(Integer));
  end;
  Positions[NewIndex] := I;
  for I := 0 to Count - 1 do
    FBitmap.Draw(B.Surface, I * Size, 0, Positions[I]);
  SwapBitmap(B);
end;

procedure TImageStrip.Clear;
begin
  if FBitmap.Empty then
    Exit;
  FBitmap.Clear;
end;

procedure TImageStrip.Draw(Surface: ISurface; Index: Integer; X,
  Y: Integer);
begin
  if (Index < 0) or (Index > Count - 1) then
    Exit;
  FBitmap.Draw(Surface, X, Y, Index);
end;

procedure TImageStrip.Draw(Surface: ISurface; Index: Integer; X,
  Y: Integer; State: TDrawState);
var
  B: TSurfaceBitmap;
begin
  if (Index < 0) or (Index > Count - 1) then
    Exit;
  if dsDisabled in State then
  begin
    B := TSurfaceBitmap.Create;
    B.SetSize(Size, Size);
    FBitmap.Draw(B.Surface, 0, 0, Index);
    B.Desaturate(1);
    B.Opacity := $70;
    B.Draw(Surface, X, Y);
    B.Free;
  end
  else if [dsHot, dsPressed] * State = [dsHot, dsPressed] then
  begin
    B := TSurfaceBitmap.Create;
    B.SetSize(Size, Size);
    FBitmap.Draw(B.Surface, 0, 0, Index);
    B.Darken(0.2);
    B.Draw(Surface, X, Y);
    B.Free;
  end
  else
    FBitmap.Draw(Surface, X, Y, Index);
end;

procedure TImageStrip.Draw(Surface: ISurface; Index: Integer;
  const Rect: TRectI; Opacity: Byte = $FF; Saturation: Float = 1.0);
var
  B: TSurfaceBitmap;
begin
  if (Index < 0) or (Index > Count - 1) then
    Exit;
  if Opacity = 0 then
    Exit;
  Saturation := Clamp(Saturation);
  if Saturation < 1 then
  begin
    B := TSurfaceBitmap.Create;
    B.SetSize(Size, Size);
    FBitmap.Draw(B.Surface, 0, 0, Index);
    B.Desaturate(1 - Saturation);
    B.Opacity := Opacity;
    B.Draw(Surface, B.ClientRect, Rect);
    B.Free;
  end
  else
  begin
    FBitmap.Opacity := Opacity;
    FBitmap.Draw(Surface, FBitmap.Frames[Index], Rect);
    FBitmap.Opacity := $FF;
  end;
end;

procedure TImageStrip.Assign(Source: TPersistent);
begin
  if Source = Self then
    Exit;
  if Source is TImageStrip then
    FBitmap.Assign((Source as TImageStrip).FBitmap)
  else
    FBitmap.Assign(Source);
end;

procedure FillRectColor(Surface: ISurface; const Rect: TRectI; Color: TColorB; Radius: Float = 0);
begin
  if Radius < 1 then
    Surface.FillRect(NewBrush(Color), Rect)
  else
    Surface.FillRoundRect(NewBrush(Color), Rect, Radius);
end;

procedure StrokeRectColor(Surface: ISurface; const Rect: TRectI; Color: TColorB; Radius: Float = 0);
begin
  if Radius < 1 then
    Surface.StrokeRect(NewPen(Color), Rect)
  else
    Surface.StrokeRoundRect(NewPen(Color), Rect, Radius);
end;

procedure FillRectState(Surface: ISurface; const Rect: TRectI; State: TDrawState);
var
  C: TColorB;
begin
  if dsSelected in State then
  begin
    C := clHighlight;
    Surface.FillRect(NewBrush(C.Blend(clWindow, 0.75)), Rect);
    Surface.StrokeRect(NewPen(C.Blend(clWindow, 0.25)), Rect);
  end
  else
    Surface.FillRect(NewBrush(clWindow), Rect);
end;

procedure FillRectSelected(Surface: ISurface; const Rect: TRectI; Radius: Float = 0);
var
  C: TColorB;
  G: IGradientBrush;
begin
  C := clHighlight;
  G := NewBrush(0, Rect.Top, 0, Rect.Bottom);
  G.AddStop(C.Fade(0.4), 0);
  G.AddStop(C.Fade(0.0), 1);
  Surface.FillRoundRect(G, Rect, Radius);
  Surface.StrokeRoundRect(NewPen(C.Fade(0.75)), Rect, Radius);
end;

function DrawDummyBitmap(Width, Height: Integer): IBitmap;
var
  S: ISurface;
begin
  Result := NewBitmap(Width, Height);
  if Result.Empty then
    Exit;
  S := Result.Surface;
  S.FillRect(NewBrush(clRed), Result.ClientRect);
end;

function DrawHueLinear(Width, Height: Integer): IBitmap;
var
  S: ISurface;
  P: IPen;
  I: Integer;
begin
  Result := NewBitmap(Width, Height);
  if Result.Empty then
    Exit;
  S := Result.Surface;
  P := NewPen(clRed, 2);
  for I := 0 to Width - 1 do
  begin
    S.MoveTo(I, 0);
    S.LineTo(I, Height);
    P.Color := Hue(I / Width);
    S.Stroke(P);
  end;
end;

function DrawHueRadial(Width, Height: Integer): IBitmap;
var
  Size: TPointI;
  Total: Integer;
  Mid: TPointI;
  S: ISurface;
  P: IPen;
  I, J: Integer;
begin
  Result := NewBitmap(Width, Height);
  if Result.Empty then
    Exit;
  Size := TPointI.Create(Width, Height);
  Total := (Size.X + Size.Y) * 2;
  Mid := TPointI.Create(Size.X div 2, Size.Y div 2);
  S := Result.Surface;
  P := NewPen(clRed, 2);
  I := 0;
  for J := 0 to Size.X - 1 do
  begin
    S.MoveTo(Mid.X, Mid.Y);
    S.LineTo(J, 0);
    Inc(I);
    P.Color := Hue(I / Total);
    S.Stroke(P);
  end;
  for J := 0 to Size.Y - 1 do
  begin
    S.MoveTo(Mid.X, Mid.Y);
    S.LineTo(Size.X, J);
    Inc(I);
    P.Color := Hue(I / Total);
    S.Stroke(P);
  end;
  for J := 0 to Size.X - 1 do
  begin
    S.MoveTo(Mid.X, Mid.Y);
    S.LineTo(Size.X - J, Size.Y);
    Inc(I);
    P.Color := Hue(I / Total);
    S.Stroke(P);
  end;
  for J := 0 to Size.Y - 1 do
  begin
    S.MoveTo(Mid.X, Mid.Y);
    S.LineTo(0, Size.Y - J);
    Inc(I);
    P.Color := Hue(I / Total);
    S.Stroke(P);
  end;
end;

function DrawSaturationBox(Width, Height: Integer; Hue: Float): IBitmap;
var
  P: PPixel;
  X, Y: Integer;
begin
  Result := NewBitmap(Width, Height);
  if Result.Empty then
    Exit;
  P := Result.Pixels;
  for Y := 1 to Height do
    for X := 1 to Width do
    begin
      P^ :=  THSL.Create(Hue, X / Width, Y / Height);
      Inc(P);
    end;
end;

function DrawDesaturationBox(Width, Height: Integer; Hue: Float): IBitmap;
var
  P: PPixel;
  X, Y, W, H: Integer;
  A1: Float;
  HSL: THSL;
begin
  Result := NewBitmap(Width, Height);
  if Result.Empty then
    Exit;
  W := Width;
  H := Height;
  P := Result.Pixels;
  for Y := H downto 1 do
    for X := 1 to W do
    begin
      HSL.Hue := Hue;
      HSL.Saturation := X / W;
      A1 := Y / H;
      HSL.Lightness := (A1 * (1 - HSL.Saturation) + A1) / 2;
      P^ := HSL;
      Inc(P);
    end;
end;

procedure DrawShadow(Surface: ISurface; const Rect: TRectI; Direction: TDirection);
const
  Margin = 8;
var
  R: TRectI;
  C: TColorB;
  B: IBrush;
begin
  R := Rect;
  C := clBlack;
  C.Alpha := 5;
  B := NewBrush(C);
  case Direction of
    drLeft:
    begin
      R.Right := R.Left + Margin;
      while R.Width > 1 do
      begin
        Surface.FillRect(B, R);
        Dec(R.Width);
      end;
    end;
    drUp:
    begin
      R.Bottom := R.Top + Margin;
      while R.Height > 1 do
      begin
        Surface.FillRect(B, R);
        Dec(R.Height);
      end;
    end;
    drRight:
    begin
      R.Left := R.Right - Margin;
      while R.Width > 1 do
      begin
        Surface.FillRect(B, R);
        R.Left := R.Left + 1;
      end;
    end;
    drDown:
    begin
      R.Top := R.Bottom - Margin;
      while R.Height > 1 do
      begin
        Surface.FillRect(B, R);
        R.Top := R.Top + 1;
      end;
    end;
  end;
end;

function DrawShadow(Image: IBitmap; Darkness: Float): IBitmap;
var
  P: PPixel;
  X, Y: Integer;
begin
  Darkness := Clamp(Darkness);
  Result := Image.Clone;
  P := Result.Pixels;
  for X := 1 to Result.Width do
    for Y := 1 to Result.Height do
    begin
      P.Red := 0;
      P.Green := 0;
      P.Blue := 0;
      P.Alpha := Round(P.Alpha * Darkness);
      Inc(P);
    end;
end;

procedure DrawBitmap(Surface: ISurface; Bitmap: IBitmap; X, Y: Float; Alpha: Byte = $FF);
var
  S, D: TRectF;
begin
  S := Bitmap.ClientRect;
  D := S;
  D.Offset(X, Y);
  Bitmap.Surface.CopyTo(S, Surface, D, Alpha);
end;

{ Type of custom sampling function}

type
  TPointRec = record
    Pos: LongInt;
    Weight: Single;
  end;

  TCluster = array of TPointRec;
  TMappingTable = array of TCluster;

var
  FullEdge: Boolean = True;

function ClampInt(Number: LongInt; Min, Max: LongInt): LongInt;
begin
  Result := Number;
  if Result < Min then
    Result := Min
  else if Result > Max then
    Result := Max;
end;

{ The following resampling code is modified and extended code from Graphics32
  library by Alex A. Denisov }

function BuildMappingTable(DstLow, DstHigh, SrcLow, SrcHigh, SrcImageWidth: LongInt; Filter: TFilterFunction; Radius: Single; WrapEdges: Boolean): TMappingTable;
var
  I, J, K, N: LongInt;
  Left, Right, SrcWidth, DstWidth: LongInt;
  Weight, Scale, Center, Count: Single;
begin
  Result := nil;
  K := 0;
  SrcWidth := SrcHigh - SrcLow;
  DstWidth := DstHigh - DstLow;
  if SrcWidth = 1 then
  begin
    SetLength(Result, DstWidth);
    for I := 0 to DstWidth - 1 do
    begin
      SetLength(Result[I], 1);
      Result[I][0].Pos := 0;
      Result[I][0].Weight := 1.0;
    end;
    Exit;
  end
  else if (SrcWidth = 0) or (DstWidth = 0) then
    Exit;
  if FullEdge then
    Scale := DstWidth / SrcWidth
  else
    Scale := (DstWidth - 1) / (SrcWidth - 1);
  SetLength(Result, DstWidth);
  if Scale = 0.0 then
  begin
    Assert(Length(Result) = 1);
    SetLength(Result[0], 1);
    Result[0][0].Pos := (SrcLow + SrcHigh) div 2;
    Result[0][0].Weight := 1.0;
  end
  else if Scale < 1.0 then
  begin
    Radius := Radius / Scale;
    for I := 0 to DstWidth - 1 do
    begin
      if FullEdge then
        Center := SrcLow - 0.5 + (I + 0.5) / Scale
      else
        Center := SrcLow + I / Scale;
      Left := Round(Floor(Center - Radius));
      Right := Round(Ceil(Center + Radius));
      Count := -1.0;
      for J := Left to Right do
      begin
        Weight := Filter((Center - J) * Scale) * Scale;
        if Weight <> 0.0 then
        begin
          Count := Count + Weight;
          K := Length(Result[I]);
          SetLength(Result[I], K + 1);
          Result[I][K].Pos := ClampInt(J, SrcLow, SrcHigh - 1);
          Result[I][K].Weight := Weight;
        end;
      end;
      if Length(Result[I]) = 0 then
      begin
        SetLength(Result[I], 1);
        Result[I][0].Pos := Round(Floor(Center));
        Result[I][0].Weight := 1.0;
      end
      else if Count <> 0.0 then
        Result[I][K div 2].Weight := Result[I][K div 2].Weight - Count;
    end;
  end
  else // if Scale > 1.0 then
  begin
    // Super-sampling - scales from smaller to bigger
    Scale := 1.0 / Scale;
    for I := 0 to DstWidth - 1 do
    begin
      if FullEdge then
        Center := SrcLow - 0.5 + (I + 0.5) * Scale
      else
        Center := SrcLow + I * Scale;
      Left := Round(Floor(Center - Radius));
      Right := Round(Ceil(Center + Radius));
      Count := -1.0;
      for J := Left to Right do
      begin
        Weight := Filter(Center - J);
        if Weight <> 0.0 then
        begin
          Count := Count + Weight;
          K := Length(Result[I]);
          SetLength(Result[I], K + 1);
          if WrapEdges then
          begin
            if J < 0 then
              N := SrcImageWidth + J
            else if J >= SrcImageWidth then
              N := J - SrcImageWidth
            else
              N := ClampInt(J, SrcLow, SrcHigh - 1);
          end
          else
            N := ClampInt(J, SrcLow, SrcHigh - 1);
          Result[I][K].Pos := N;
          Result[I][K].Weight := Weight;
        end;
      end;
      if Count <> 0.0 then
        Result[I][K div 2].Weight := Result[I][K div 2].Weight - Count;
    end;
  end;
end;

procedure FindExtremes(const Map: TMappingTable; out  MinPos, MaxPos: LongInt);
var
  I, J: LongInt;
begin
  MinPos := 0;
  MaxPos := 0;
  if Length(Map) > 0 then
  begin
    MinPos := Map[0][0].Pos;
    MaxPos := MinPos;
    for I := 0 to Length(Map) - 1 do
      for J := 0 to Length(Map[I]) - 1 do
      begin
        if MinPos > Map[I][J].Pos then
          MinPos := Map[I][J].Pos;
        if MaxPos < Map[I][J].Pos then
          MaxPos := Map[I][J].Pos;
      end;
  end;
end;

{ Filter function for nearest filtering. Also known as box filter }

function FilterNearest(Value: Single): Single;
begin
  if (Value > -0.5) and (Value <= 0.5) then
    Result := 1
  else
    Result := 0;
end;

{ Filter function for linear filtering. Also known as triangle or Bartlett filter }

function FilterLinear(Value: Single): Single;
begin
  if Value < 0.0 then
    Value := -Value;
  if Value < 1.0 then
    Result := 1.0 - Value
  else
    Result := 0.0;
end;

{ Cosine filter }

function FilterCosine(Value: Single): Single;
begin
  Result := 0;
  if Abs(Value) < 1 then
    Result := (Cos(Value * Pi) + 1) / 2;
end;

{ Hermite filter }

function FilterHermite(Value: Single): Single;
begin
  if Value < 0.0 then
    Value := -Value;
  if Value < 1 then
    Result := (2 * Value - 3) * Sqr(Value) + 1
  else
    Result := 0;
end;

{ Quadratic filter. Also known as Bell }

function FilterQuadratic(Value: Single): Single;
begin
  if Value < 0.0 then
    Value := -Value;
  if Value < 0.5 then
    Result := 0.75 - Sqr(Value)
  else
  if Value < 1.5 then
  begin
    Value := Value - 1.5;
    Result := 0.5 * Sqr(Value);
  end
  else
    Result := 0.0;
end;

{ Gaussian filter }

function FilterGaussian(Value: Single): Single;
begin
  Result := Exp(-2.0 * Sqr(Value)) * Sqrt(2.0 / Pi);
end;

{ 4th order (cubic) b-spline filter }

function FilterSpline(Value: Single): Single;
var
  Temp: Single;
begin
  if Value < 0.0 then
    Value := -Value;
  if Value < 1.0 then
  begin
    Temp := Sqr(Value);
    Result := 0.5 * Temp * Value - Temp + 2.0 / 3.0;
  end
  else
  if Value < 2.0 then
  begin
    Value := 2.0 - Value;
    Result := Sqr(Value) * Value / 6.0;
  end
  else
    Result := 0.0;
end;

{ Lanczos-windowed sinc filter }

function FilterLanczos(Value: Single): Single;

  function SinC(Value: Single): Single;
  begin
    if Value <> 0.0 then
    begin
      Value := Value * Pi;
      Result := Sin(Value) / Value;
    end
    else
      Result := 1.0;
  end;

begin
  if Value < 0.0 then
    Value := -Value;
  if Value < 3.0 then
    Result := SinC(Value) * SinC(Value / 3.0)
  else
    Result := 0.0;
end;

{ Micthell cubic filter }

function FilterMitchell(Value: Single): Single;
const
  B = 1.0 / 3.0;
  C = 1.0 / 3.0;
var
  Temp: Single;
begin
  if Value < 0.0 then
    Value := -Value;
  Temp := Sqr(Value);
  if Value < 1.0 then
  begin
    Value := (((12.0 - 9.0 * B - 6.0 * C) * (Value * Temp)) +
      ((-18.0 + 12.0 * B + 6.0 * C) * Temp) +
      (6.0 - 2.0 * B));
    Result := Value / 6.0;
  end
  else
  if Value < 2.0 then
  begin
    Value := (((-B - 6.0 * C) * (Value * Temp)) +
      ((6.0 * B + 30.0 * C) * Temp) +
      ((-12.0 * B - 48.0 * C) * Value) +
      (8.0 * B + 24.0 * C));
    Result := Value / 6.0;
  end
  else
    Result := 0.0;
end;

{ CatmullRom spline filter }

function FilterCatmullRom(Value: Single): Single;
begin
  if Value < 0.0 then
    Value := -Value;
  if Value < 1.0 then
    Result := 0.5 * (2.0 + Sqr(Value) * (-5.0 + 3.0 * Value))
  else
  if Value < 2.0 then
    Result := 0.5 * (4.0 + Value * (-8.0 + Value * (5.0 - Value)))
  else
    Result := 0.0;
end;

var
  Init: Boolean;

procedure InitResample;
begin
  if Init then
    Exit;
  Init := True;
  SamplingFilterFunctions[sfNearest] := FilterNearest;
  SamplingFilterFunctions[sfLinear] := FilterLinear;
  SamplingFilterFunctions[sfCosine] := FilterCosine;
  SamplingFilterFunctions[sfHermite] := FilterHermite;
  SamplingFilterFunctions[sfQuadratic] := FilterQuadratic;
  SamplingFilterFunctions[sfGaussian] := FilterGaussian;
  SamplingFilterFunctions[sfSpline] := FilterSpline;
  SamplingFilterFunctions[sfLanczos] := FilterLanczos;
  SamplingFilterFunctions[sfMitchell] := FilterMitchell;
  SamplingFilterFunctions[sfCatmullRom] := FilterCatmullRom;
  SamplingFilterRadii[sfNearest] := 1.0;
  SamplingFilterRadii[sfLinear] := 1.0;
  SamplingFilterRadii[sfCosine] := 1.0;
  SamplingFilterRadii[sfHermite] := 1.0;
  SamplingFilterRadii[sfQuadratic] := 1.5;
  SamplingFilterRadii[sfGaussian] := 1.25;
  SamplingFilterRadii[sfSpline] := 2.0;
  SamplingFilterRadii[sfLanczos] := 3.0;
  SamplingFilterRadii[sfMitchell] := 2.0;
  SamplingFilterRadii[sfCatmullRom] := 2.0;
end;

procedure ResampleBitmap(Src: IBitmap; SrcX, SrcY, SrcWidth, SrcHeight: LongInt;
  Dst: IBitmap; DstX, DstY, DstWidth, DstHeight: LongInt;
  Filter: TFilterFunction; Radius: Single; WrapEdges: Boolean);
type
  TBufferItem = record
    A, R, G, B: Integer;
  end;
  TByteArray = array[0..High(LongWord) div 4] of Byte;
  PByteArray = ^TByteArray;

var
  MapX, MapY: TMappingTable;
  MinX, MaxX: Integer;
  LineBufferInt: array of TBufferItem;
  ClusterX, ClusterY: TCluster;
  Speed, Weight, AccumA, AccumR, AccumG, AccumB: Integer;
  SrcColor: TPixel;
  Pixels: PPixel;
  SrcPixels: array of PByteArray;
  DstPixels: array of PByteArray;
  I, J, X, Y: Integer;
begin
  InitResample;
  if (Src.Width < 2) or (Src.Height < 2) or (Dst.Width < 2) or (Dst.Height < 2) then Exit;
  MapX := BuildMappingTable(DstX, DstX + DstWidth , SrcX, SrcX + SrcWidth , Src.Width , Filter, Radius, WrapEdges);
  MapY := BuildMappingTable(DstY, DstY + DstHeight, SrcY, SrcY + SrcHeight, Src.Height, Filter, Radius, WrapEdges);
  ClusterX := nil;
  ClusterY := nil;
  SetLength(SrcPixels, Src.Height);
  Pixels := Src.Pixels;
  for I := 0 to Src.Height - 1 do
  begin
    SrcPixels[I] := PByteArray(Pixels);
    Inc(Pixels, Src.Width);
  end;
  SetLength(DstPixels, Dst.Height);
  Pixels := Dst.Pixels;
  for I := 0 to Dst.Height - 1 do
  begin
    DstPixels[I] := PByteArray(Pixels);
    Inc(Pixels, Dst.Width);
  end;
  FindExtremes(MapX, MinX, MaxX);
  SetLength(LineBufferInt, MaxX - MinX + 1);
  for J := 0 to DstHeight - 1 do
  begin
    ClusterY := MapY[J];
    for X := MinX to MaxX do
    begin
      AccumA := 0;
      AccumR := 0;
      AccumG := 0;
      AccumB := 0;
      for Y := 0 to Length(ClusterY) - 1 do
      begin
        Weight := Round(256 * ClusterY[Y].Weight);
        Speed := X * 4;
        AccumB := AccumB + SrcPixels[ClusterY[Y].Pos][Speed] * Weight;
        AccumG := AccumG + SrcPixels[ClusterY[Y].Pos][Speed + 1] * Weight;
        AccumR := AccumR + SrcPixels[ClusterY[Y].Pos][Speed + 2] * Weight;
        AccumA := AccumA + SrcPixels[ClusterY[Y].Pos][Speed + 3] * Weight;
      end;
      with LineBufferInt[X - MinX] do
      begin
        A := AccumA;
        R := AccumR;
        G := AccumG;
        B := AccumB;
      end;
    end;
    for I := 0 to DstWidth - 1 do
    begin
      ClusterX := MapX[I];
      AccumA := 0;
      AccumR := 0;
      AccumG := 0;
      AccumB := 0;
      for X := 0 to Length(ClusterX) - 1 do
      begin
        Weight := Round(256 * ClusterX[X].Weight);
        with LineBufferInt[ClusterX[X].Pos - MinX] do
        begin
          AccumB := AccumB + B * Weight;
          AccumG := AccumG + G * Weight;
          AccumR := AccumR + R * Weight;
          AccumA := AccumA + A * Weight;
        end;
      end;
      SrcColor.Blue := ClampInt(AccumB, 0, $00FF0000) shr 16;
      SrcColor.Green := ClampInt(AccumG, 0, $00FF0000) shr 16;
      SrcColor.Red := ClampInt(AccumR, 0, $00FF0000) shr 16;
      SrcColor.Alpha := ClampInt(AccumA, 0, $00FF0000) shr 16;
      PLongWord(@DstPixels[J]^[(I + DstX) * 4])^ := PLongWord(@SrcColor)^;
    end;
  end;
end;

function ResampleBitmap(Bitmap: IBitmap; Width, Height: Integer; Filter: TSamplingFilter; WrapEdges: Boolean = False): IBitmap;
begin
  InitResample;
  Result := NewBitmap(Width, Height);
  if Bitmap.Empty or Result.Empty then
    Exit;
  ResampleBitmap(Bitmap, 0, 0, Bitmap.Width, Bitmap.Height,
    Result, 0, 0, Result.Width, Result.Height,
    SamplingFilterFunctions[Filter], SamplingFilterRadii[Filter], WrapEdges);
end;

const
  MaxKernelSize = 100;

type
  TKernelSize = 1..MaxKernelSize;

  TKernel = record
    Size: TKernelSize;
    Weights: array[-MaxKernelSize..MaxKernelSize] of Single;
  end;

  PRow = ^TRow;
  TRow = array [0..1000000] of TPixel;

  PPRows = ^TPRows;
  TPRows = array [0..1000000] of PRow;

procedure MakeGaussianKernel(out K: TKernel; Radius: Double;
  MaxData, Granularity: Double);
var
  Temp, Delta: Double;
  KernelSize: TKernelSize;
  I: Integer;
begin
  for I := Low(K.Weights) to High(K.Weights) do
  begin
    Temp := I / Radius;
    K.Weights[I] := exp(-Temp * Temp / 2);
  end;
  Temp := 0;
  for I := Low(K.Weights) to High(K.Weights) do
    Temp := Temp + K.Weights[I];
  for I := Low(K.Weights) to High(K.Weights) do
    K.Weights[I] := K.Weights[I] / Temp;
  KernelSize := MaxKernelSize;
  Delta := Granularity / (2*MaxData);
  Temp := 0;
  while (Temp < Delta) and (KernelSize > 1) do
  begin
    Temp := Temp + 2 * K.Weights[KernelSize];
    Dec(KernelSize);
  end;
  K.Size := KernelSize;
  Temp := 0;
  for I := -K.Size to K.Size do
    Temp := Temp + K.Weights[I];
  for I := -K.Size to K.Size do
    K.Weights[I] := K.Weights[I] / Temp;
end;

function TrimInt(Lower, Upper, I: Integer): Integer;
begin
  if (I <= Upper) and (I >= Lower) then
    Result := I
  else if I > Upper then
    Result := Upper
  else
    Result := Lower;
end;

function TrimReal(Lower, Upper: Integer; D: Double): Integer;
begin
  if (D < Upper) and (D >= Lower) then
    Result := Trunc(D)
  else if D > Upper then
    Result := Upper
  else
    Result := Lower;
end;

procedure BlurRow(var Row: array of TPixel; K: TKernel; P: PRow);
var
  R, G, B, A: Double;
  W: Double;
  I, J: Integer;
begin
  for I := 0 to High(Row) do
  begin
    B := 0;
    G := 0;
    R := 0;
    A := 0;
    for J := - K.Size to K.Size do
    begin
      W := K.Weights[J];
      with Row[TrimInt(0, High(Row), I - J)] do
      begin
        B := B + W * Blue;
        G := G + W * Green;
        R := R + W * Red;
        A := A + W * Alpha;
      end;
    end;
    with P[I] do
    begin
      Blue := TrimReal(0, 255, B);
      Green := TrimReal(0, 255, G);
      Red := TrimReal(0, 255, R);
      Alpha := TrimReal(0, 255, A);
    end;
  end;
  Move(P[0], Row[0], (High(Row) + 1) * SizeOf(TPixel));
end;

procedure BlurBitmap(Bits: PPixel; W, H: Integer; const Radius: Double);
var
  Row, Col: Integer;
  Rows: PPRows;
  K: TKernel;
  ACol, P: PRow;
begin
  if Radius < 0.1 then Exit;
  if (W < 2) or (H < 2) then Exit;
  MakeGaussianKernel(K, Radius, 255, 1);
  GetMem(Rows, H * SizeOf(PRow));
  GetMem(ACol, H * SizeOf(TPixel));
  for Row := 0 to H - 1 do
  begin
    Rows[Row] := Pointer(Bits);
    Inc(Bits, W);
  end;
  P := AllocMem(W * SizeOf(TPixel));
  for Row := 0 to H - 1 do
    BlurRow(Slice(Rows[Row]^, W), K, P);
  ReAllocMem(P, H * SizeOf(TPixel));
  for Col := 0 to W - 1 do
  begin
    for Row := 0 to H - 1 do
      ACol[Row] := Rows[Row][Col];
    BlurRow(Slice(ACol^, H), K, P);
    for Row := 0 to H - 1 do
      Rows[Row][Col] := ACol[Row];
  end;
  FreeMem(Rows);
  FreeMem(ACol);
  ReAllocMem(P, 0);
end;

function BlurBitmap(Bitmap: IBitmap; const Radius: Double): IBitmap;
var
  I: Integer;
begin
  if Bitmap.Empty or (Radius < 0.1) then
    Exit(Bitmap.Clone);
  I := Round(Radius);
  Result := NewBitmap(Bitmap.Width + I * 2, Bitmap.Height + I * 2);
  DrawBitmap(Result.Surface, Bitmap, I, I);
  BlurBitmap(Result.Pixels, Result.Width, Result.Height, Radius);
end;

procedure DrawEasing(Surface: ISurface; Font: IFont; Rect: TRectF;
  Easing: TEasing; Reverse: Boolean; Time: Float);
var
  P: IPen;
  R: TRectF;
  C: TColorB;
  X, Y: Float;
  I, J: Integer;
begin
  { Add the axis to the path }
  R := Rect;
  Surface.MoveTo(R.Left, R.Top);
  Surface.LineTo(R.Left, R.Bottom);
  Surface.LineTo(R.Right, R.Bottom);
  { And stroke with a gray pen }
  P := NewPen(clGray);
  Surface.Stroke(P);
  { Find the current point in the easing }
  X := Interpolate(@TEasingDefaults.Linear, Time, R.Left, R.Right);
  Y := Interpolate(Easing, Time, R.Bottom, R.Top, Reverse);
  { And add a dashed red line from one acis to the easing delta }
  Surface.MoveTo(X, R.Bottom);
  Surface.LineTo(X, Y);
  P.Color := clRed;
  P.LinePattern := pnDash;
  { Stroke the line }
  Surface.Stroke(P);
  { Add an 8x8 circle dot to the easing point }
  R := TRectF.Create(8, 8);
  R.Center(X - 0.5, Y - 0.5);
  Surface.Ellipse(R);
  { Add fill the dot with a red brush }
  Surface.Fill(NewBrush(clRed));
  { Draw the easing line }
  R := Rect;
  Surface.MoveTo(R.Left, R.Bottom);
  J := Round(R.Width);
  for I := 1 to J do
  begin
    X := R.Left + I;
    Y := Interpolate(Easing, I / J, R.Bottom, R.Top, Reverse);
    Surface.LineTo(X, Y);
  end;
  Surface.LineTo(R.Right, R.Top);
  { And stroke it with a 2 pixel thick black pen }
  P := NewPen(clBlack, 2);
  P.LineJoin := jnRound;
  Surface.Stroke(P);
  { label the axis }
  Y := Surface.TextSize(Font, 'Wg').Y;
  R.Top := R.Top - Y;
  C := Theme.Font.Color;
  Font.Color := clGray;
  { The left axis is the delta, or change over time }
  Surface.TextOut(Font, 'delta', R, drWrap);
  R := Rect;
  R.Top := R.Bottom;
  R.Height := Y;
  { The bottom axis is time }
  Surface.TextOut(Font, 'time', R, drRight);
  Theme.Font.Color := C;
end;

type
  TBrushNames = TNamedValues<TBrushStyle>;

var
  BrushNames: TBrushNames;

procedure BrushesRegisterDefaults;
begin
  if BrushNames.Count > 0 then
    Exit;
  BrushNames.Add('Dither', @Brushes.Dither);
  BrushNames.Add('Checker', @Brushes.Checker);
  BrushNames.Add('Beam', @Brushes.Beam);
  BrushNames.Add('Cross', @Brushes.Cross);
  BrushNames.Add('Zig Zag', @Brushes.ZigZag);
  BrushNames.Add('Brick', @Brushes.Brick);
  BrushNames.Add('Circles', @Brushes.Circles);
  BrushNames.Add('Squares', @Brushes.Squares);
  BrushNames.Add('Clovers', @Brushes.Clovers);
  BrushNames.Add('Tooth', @Brushes.Tooth);
  BrushNames.Add('Floor Tile', @Brushes.FloorTile);
  BrushNames.Add('Snake Skin', @Brushes.SnakeSkin);
  BrushNames.Add('Pipes', @Brushes.Pipes);
end;

function StrToBrush(Name: string; Foreground, Background: TColorB; PenWidth: Float = DefPenWidth; BrushSize: Integer = DefBrushSize): IBrush;
var
  B: TBrushStyle;
begin
  BrushesRegisterDefaults;
  B := BrushNames[Name];
  if @B <> nil then
    Result := B(Foreground, Background, PenWidth, BrushSize)
  else
    Result := newBrush(clTransparent);
end;

procedure RegisterBrushStyle(Name: string; BrushStyle: TBrushStyle);
begin
  BrushNames.Add(Name, BrushStyle);
end;

function EnumBrushStyles: TNamedEnumerable;
begin
  BrushesRegisterDefaults;
  Result.Enumerator := BrushNames.GetEnumerator;
end;

{ Brushes }

class function Brushes.Transparent: IBrush;
begin
  Result := Brushes.Checker(clSilver, clWhite, 10);
end;

class function Brushes.Checker(Foreground, Background: TColorB; PenWidth: Float = DefPenWidth; BrushSize: Integer = DefBrushSize): IBrush;
var
  Bitmap: IBitmap;
  Brush: IBrush;
  R: TRectI;
begin
  if BrushSize < 1 then
    BrushSize := 1;
  Bitmap := NewBitmap(BrushSize * 2, BrushSize * 2);
  Brush := NewBrush(Foreground);
  R := TRectI.Create(BrushSize, BrushSize);
  Bitmap.Surface.FillRect(Brush, R);
  R.Offset(BrushSize, BrushSize);
  Bitmap.Surface.FillRect(Brush, R);
  Brush := NewBrush(Background);
  R.Offset(0, -BrushSize);
  Bitmap.Surface.FillRect(Brush, R);
  R.Offset(-BrushSize, BrushSize);
  Bitmap.Surface.FillRect(Brush, R);
  Result := NewBrush(Bitmap);
end;

class function Brushes.Dither(Foreground, Background: TColorB; PenWidth: Float = DefPenWidth; BrushSize: Integer = DefBrushSize): IBrush;
var
  Bitmap: IBitmap;
  Brush: IBrush;
  R: TRectF;
  S: ISurface;
  F: Float;
begin
  if BrushSize < 2 then
    BrushSize := 2;
  Bitmap := NewBitmap(BrushSize div 2, BrushSize div 2);
  Brush := NewBrush(Background);
  R := Bitmap.ClientRect;
  S := Bitmap.Surface;
  S.FillRect(Brush, R);
  Brush := NewBrush(Foreground);
  R.Width := 1;
  R.Height := 1;
  F := BrushSize / 8;
  R.Center(F + 0.5, F + 0.5);
  S.FillRect(Brush, R);
  R.Center(F * 3 + 0.5, F + 0.5);
  S.FillRect(Brush, R);
  R.Center(0.5, F * 3 + 0.5);
  S.FillRect(Brush, R);
  R.Center(F * 2 + 0.5, F * 3 + 0.5);
  S.FillRect(Brush, R);
  Result := NewBrush(Bitmap);
end;

class function Brushes.Beam(Foreground, Background: TColorB; PenWidth: Float = DefPenWidth; BrushSize: Integer = DefBrushSize): IBrush;
var
  Bitmap: IBitmap;
  Brush: IBrush;
  Pen: IPen;
  R: TRectI;
  S: ISurface;
begin
  if BrushSize < 1 then
    BrushSize := 1;
  Bitmap := NewBitmap(BrushSize, BrushSize);
  Brush := NewBrush(Background);
  Pen := NewPen(Foreground, PenWidth);
  R := TRectI.Create(BrushSize, BrushSize);
  S := Bitmap.Surface;
  S.FillRect(Brush, R);
  S.MoveTo(R.Left - 1, (R.Top + R.Bottom) / 2);
  S.LineTo(R.Right + 1, (R.Top + R.Bottom) / 2);
  S.Stroke(Pen);
  Result := NewBrush(Bitmap);
end;

class function Brushes.Cross(Foreground, Background: TColorB; PenWidth: Float = DefPenWidth; BrushSize: Integer = DefBrushSize): IBrush;
var
  Bitmap: IBitmap;
  Brush: IBrush;
  Pen: IPen;
  R: TRectI;
  S: ISurface;
begin
  if BrushSize < 1 then
    BrushSize := 1;
  Bitmap := NewBitmap(BrushSize, BrushSize);
  Brush := NewBrush(Background);
  Pen := NewPen(Foreground, PenWidth);
  R := TRectI.Create(BrushSize, BrushSize);
  S := Bitmap.Surface;
  S.FillRect(Brush, R);
  S.MoveTo(R.Left - 1, (R.Top + R.Bottom) / 2);
  S.LineTo(R.Right + 1, (R.Top + R.Bottom) / 2);
  S.MoveTo((R.Left + R.Right) / 2, R.Top - 1);
  S.LineTo((R.Left + R.Right) / 2, R.Bottom + 1);
  S.Stroke(Pen);
  Result := NewBrush(Bitmap);
end;

class function Brushes.ZigZag(Foreground, Background: TColorB; PenWidth: Float = DefPenWidth; BrushSize: Integer = DefBrushSize): IBrush;
var
  Bitmap: IBitmap;
  Brush: IBrush;
  Pen: IPen;
  R: TRectF;
  S: ISurface;
begin
  if BrushSize < 1 then
    BrushSize := 1;
  Bitmap := NewBitmap(BrushSize * 2, BrushSize * 2);
  Brush := NewBrush(Background);
  Pen := NewPen(Foreground, PenWidth);
  R := Bitmap.ClientRect;
  S := Bitmap.Surface;
  S.FillRect(Brush, R);
  S.MoveTo(R.Left - PenWidth, R.Bottom + PenWidth);
  with R.MidPoint do
    S.LineTo(X, Y);
  S.LineTo(R.Right + PenWidth, R.Bottom + PenWidth);
  S.MoveTo(R.Left - PenWidth, R.Top + PenWidth);
  S.LineTo(R.Left + PenWidth, R.Top - PenWidth);
  S.MoveTo(R.Right - PenWidth, R.Top - PenWidth);
  S.LineTo(R.Right + PenWidth, R.Top + PenWidth);
  S.Stroke(Pen);
  Result := NewBrush(Bitmap);
end;

class function Brushes.Brick(Foreground, Background: TColorB; PenWidth: Float = DefPenWidth; BrushSize: Integer = DefBrushSize): IBrush;
var
  Bitmap: IBitmap;
  Brush: IBrush;
  Pen: IPen;
  R: TRectF;
  S: ISurface;
  F: Float;
begin
  if BrushSize < 1 then
    BrushSize := 1;
  Bitmap := NewBitmap(BrushSize * 2, BrushSize * 2);
  Brush := NewBrush(Background);
  Pen := NewPen(Foreground, PenWidth);
  R := Bitmap.ClientRect;
  S := Bitmap.Surface;
  S.FillRect(Brush, R);
  F := BrushSize / 2;
  S.MoveTo(R.Left - 1, R.Top + F);
  S.LineTo(R.Right + 1, R.Top + F);
  S.MoveTo(R.Left - 1, R.Bottom - F);
  S.LineTo(R.Right + 1, R.Bottom - F);
  S.MoveTo(R.Left + F, R.Top + F);
  S.LineTo(R.Left + F, R.Bottom - F);
  S.MoveTo(R.Right - F, R.Top - 1);
  S.LineTo(R.Right - F, R.Top + F);
  S.MoveTo(R.Right - F, R.Bottom + 1);
  S.LineTo(R.Right - F, R.Bottom - F);
  S.Stroke(Pen);
  Result := NewBrush(Bitmap);
end;

class function Brushes.Circles(Foreground, Background: TColorB; PenWidth: Float = DefPenWidth; BrushSize: Integer = DefBrushSize): IBrush;
var
  Bitmap: IBitmap;
  Brush: IBrush;
  Pen: IPen;
  R: TRectF;
  S: ISurface;
begin
  if BrushSize < 1 then
    BrushSize := 1;
  Bitmap := NewBitmap(BrushSize * 2, BrushSize * 2);
  Brush := NewBrush(Background);
  Pen := NewPen(Foreground, PenWidth);
  R := Bitmap.ClientRect;
  S := Bitmap.Surface;
  S.FillRect(Brush, R);
  R.Width := R.Width / 2;
  R.Height := R.Height / 2;
  R.Center(R.BottomRight);
  S.Ellipse(R);
  S.Stroke(Pen);
  Result := NewBrush(Bitmap);
end;

class function Brushes.Squares(Foreground, Background: TColorB; PenWidth: Float = DefPenWidth; BrushSize: Integer = DefBrushSize): IBrush;
var
  Bitmap: IBitmap;
  Brush: IBrush;
  Pen: IPen;
  R: TRectF;
  S: ISurface;
begin
  if BrushSize < 1 then
    BrushSize := 1;
  Bitmap := NewBitmap(BrushSize * 2, BrushSize * 2);
  Brush := NewBrush(Background);
  Pen := NewPen(Foreground, PenWidth);
  R := Bitmap.ClientRect;
  S := Bitmap.Surface;
  S.FillRect(Brush, R);
  R.Width := R.Width / 2;
  R.Height := R.Height / 2;
  R.Center(R.BottomRight);
  S.Rectangle(R);
  S.Stroke(Pen);
  Result := NewBrush(Bitmap);
end;

class function Brushes.Tooth(Foreground, Background: TColorB; PenWidth: Float = DefPenWidth; BrushSize: Integer = DefBrushSize): IBrush;
var
  Bitmap: IBitmap;
  Brush: IBrush;
  Pen: IPen;
  R: TRectF;
  S: ISurface;
  F: Float;
begin
  if BrushSize < 1 then
    BrushSize := 1;
  Bitmap := NewBitmap(BrushSize * 2, BrushSize * 2);
  Brush := NewBrush(Background);
  Pen := NewPen(Foreground, PenWidth);
  R := Bitmap.ClientRect;
  S := Bitmap.Surface;
  S.FillRect(Brush, R);
  F := R.Width / 4;
  S.MoveTo(R.Left - 1, R.Top + F);
  S.LineTo(R.Left + F, R.Top + F);
  S.LineTo(R.Left + F, R.Bottom - F);
  S.LineTo(R.Right - F, R.Bottom - F);
  S.LineTo(R.Right - F, R.Top + F);
  S.LineTo(R.Right + 1, R.Top + F);
  S.Stroke(Pen);
  Result := NewBrush(Bitmap);
end;

class function Brushes.FloorTile(Foreground, Background: TColorB; PenWidth: Float = DefPenWidth; BrushSize: Integer = DefBrushSize): IBrush;
var
  Bitmap: IBitmap;
  Brush: IBrush;
  Pen: IPen;
  R: TRectF;
  S: ISurface;
  F: Float;
begin
  if BrushSize < 1 then
    BrushSize := 1;
  Bitmap := NewBitmap(BrushSize * 2, BrushSize * 2);
  Brush := NewBrush(Background);
  Pen := NewPen(Foreground, PenWidth);
  R := Bitmap.ClientRect;
  S := Bitmap.Surface;
  S.FillRect(Brush, R);
  F := R.Width / 3;
  S.MoveTo(R.Left - 1, R.Top - 1);
  S.LineTo(R.Left + F, R.Top + F);
  S.LineTo(R.Left + F, R.Bottom - F);
  S.LineTo(R.Left - 1, R.Bottom + 1);
  S.MoveTo(R.Right + 1, R.Top - 1);
  S.LineTo(R.Right - F, R.Top + F);
  S.LineTo(R.Right - F, R.Bottom - F);
  S.LineTo(R.Right + 1, R.Bottom + 1);

  S.MoveTo(R.Left + F, R.Top + F);
  S.LineTo(R.Right - F, R.Top + F);
  S.MoveTo(R.Left + F, R.Bottom - F);
  S.LineTo(R.Right - F, R.Bottom - F);

  S.Stroke(Pen);
  Result := NewBrush(Bitmap);
end;

class function Brushes.SnakeSkin(Foreground, Background: TColorB; PenWidth: Float = DefPenWidth; BrushSize: Integer = DefBrushSize): IBrush;
var
  Bitmap: IBitmap;
  Brush: IBrush;
  Pen: IPen;
  R: TRectF;
  S: ISurface;
  FX, FY: Float;
begin
  if BrushSize < 1 then
    BrushSize := 1;
  Bitmap := NewBitmap(BrushSize * 2, BrushSize * 2);
  Brush := NewBrush(Background);
  Pen := NewPen(Foreground, PenWidth);
  R := Bitmap.ClientRect;
  S := Bitmap.Surface;
  S.FillRect(Brush, R);
  FX := R.Width / 3;
  FY := R.Height / 2;
  S.MoveTo(R.Left - 1, R.Top - 1);
  S.LineTo(R.Left + FX, R.Top + FY);
  S.LineTo(R.Left - 1, R.Bottom + 1);
  S.MoveTo(R.Right + 1, R.Top - 1);
  S.LineTo(R.Right - FX, R.Top + FX);
  S.LineTo(R.Right + 1, R.Bottom + 1);
  S.MoveTo(R.Left + FX, R.Top + FY);
  S.LineTo(R.Right - FX, R.Top + FX);
  S.Stroke(Pen);
  Result := NewBrush(Bitmap);
end;

class function Brushes.Pipes(Foreground, Background: TColorB; PenWidth: Float = DefPenWidth; BrushSize: Integer = DefBrushSize): IBrush;
var
  Bitmap: IBitmap;
  Brush: IBrush;
  Pen: IPen;
  R: TRectF;
  S: ISurface;
  F: Float;
begin
  if BrushSize < 1 then
    BrushSize := 1;
  Bitmap := NewBitmap(BrushSize * 2, BrushSize * 2);
  Brush := NewBrush(Background);
  Pen := NewPen(Foreground, PenWidth);
  R := Bitmap.ClientRect;
  S := Bitmap.Surface;
  S.FillRect(Brush, R);
  F := R.Width / 4;
  S.MoveTo(R.Left + F, R.Top - 1);
  S.LineTo(R.Left + F, R.Bottom + 1);
  S.MoveTo(R.Right - F, R.Top - 1);
  S.LineTo(R.Right - F, R.Bottom + 1);
  R.Center(R.MidPoint);
  R.Inflate(-F, -F);
  with R.MidRight do S.MoveTo(X, Y);
  S.ArcTo(R, Pi * 0.5, Pi * 1.5);
  R.Center(0, 0);
  with R.MidRight do S.MoveTo(X, Y);
  S.ArcTo(R, Pi * 0.5, Pi * 1.5);
  R.Center(BrushSize * 2, 0);
  with R.MidRight do S.MoveTo(X, Y);
  S.ArcTo(R, Pi * 0.5, Pi * 1.5);
  S.Stroke(Pen);
  Result := NewBrush(Bitmap);
end;

class function Brushes.Clovers(Foreground, Background: TColorB; PenWidth: Float = DefPenWidth; BrushSize: Integer = DefBrushSize): IBrush;
var
  Bitmap: IBitmap;
  Brush: IBrush;
  Pen: IPen;
  R: TRectF;
  S: ISurface;
  F: Float;
begin
  if BrushSize < 1 then
    BrushSize := 1;
  Bitmap := NewBitmap(BrushSize * 2, BrushSize * 2);
  Brush := NewBrush(Background);
  Pen := NewPen(Foreground, PenWidth);
  R := Bitmap.ClientRect;
  S := Bitmap.Surface;
  S.FillRect(Brush, R);
  F := R.Width / 3;
  R.Inflate(-F, -F);
  R.Center(R.MidTop);
  with R.MidLeft do S.MoveTo(X, Y);
  S.ArcTo(R, Pi * 1.5, Pi * 2.5);
  R.Center(R.BottomRight);
  S.ArcTo(R, 0, Pi);
  R.Center(R.BottomLeft);
  S.ArcTo(R, Pi * 0.5, Pi * 1.5);
  R.Center(R.TopLeft);
  S.ArcTo(R, Pi, Pi * 2);
  S.Stroke(Pen);
  Result := NewBrush(Bitmap);
end;

{ TDrawControlHelper }

type
  TFontDetails = record
    Name: string;
    Style: TFontStyles;
    Height: Integer;
    Size: TPointI;
    function Same(Font: TFont): Boolean;
  end;

function TFontDetails.Same(Font: TFont): Boolean;
begin
  Result := (Name = Font.Name)
    and (Style = Font.Style)
    and (Height = Font.Height);
  if not Result then
  begin
    Name := Font.Name;
    Style := Font.Style;
    Height := Font.Height;
  end;
end;

var
  FontDetails: TFontDetails;
  FontBitmap: TBitmap;

function TDrawControlHelper.GetCurrentColor: TColorB;
begin
  Result := Self.GetRGBColorResolvingParent;
end;

function TDrawControlHelper.GetParentCurrentColor: TColorB;
begin
  if Parent <> nil then
    Result := Self.Parent.GetRGBColorResolvingParent
  else
    Result := clTransparent;
end;

function TDrawControlHelper.TextHeight: Integer;
begin
  if FontBitmap = nil then
  begin
    FontBitmap := TBitmap.Create;
    FontBitmap.Width := 2;
    FontBitmap.Height := 2;
  end;
  if not FontDetails.Same(Font) then
  begin
    FontBitmap.Canvas.Font := Font;
    FontDetails.Size.Y := FontBitmap.Canvas.TextHeight('Wg');
  end;
  Result := FontDetails.Size.Y;
end;

function TDrawControlHelper.TextSize(const Text: string): TPointI;
begin
  TextHeight;
  FontDetails.Size.X := FontBitmap.Canvas.TextWidth(Text);
  Result := FontDetails.Size;
end;

procedure TDrawControlHelper.DrawDummyBlock(Surface: ISurface; const Rect: TRectI; State: TDrawState);

    function Brush(P: Float): IBrush;
    var
      C: TColorB;
    begin
      C := clBlack;
      Result := NewBrush(C.Lighten(P));
    end;

    function Pen(P: Float): IPen;
    var
      C: TColorB;
    begin
      C := clBlack;
      Result := NewPen(C.Lighten(P));
    end;

const
  DisableBrush = 0.5;
  DisablePen = 0.4;
  PressedBrush = 0.7;
  PressedPen = 0.6;
  HotBrush = 0.9;
  HotPen = 0.7;
  NormalBrush = 0.85;
  NormalPen = 0.7;
begin
  if dsDisabled in State then
  begin
    Surface.FillRect(Brush(DisableBrush), Rect);
    Surface.StrokeRect(Pen(DisablePen), Rect);
  end
  else if dsHot in State then
    if dsPressed in State then
    begin
      Surface.FillRect(Brush(PressedBrush), Rect);
      Surface.StrokeRect(Pen(PressedPen), Rect);
    end
    else
    begin
      Surface.FillRect(Brush(HotBrush), Rect);
      Surface.StrokeRect(Pen(HotPen), Rect);
    end
  else
  begin
    Surface.FillRect(Brush(NormalBrush), Rect);
    Surface.StrokeRect(Pen(NormalPen), Rect);
  end;
end;

procedure TDrawControlHelper.DrawBitmap(Surface: ISurface; Bitmap: IBitmap; X, Y: Integer; Alpha: Byte = $FF);
var
  R: TRectI;
begin
  if Bitmap.Empty then
    Exit;
  R := Bitmap.ClientRect;
  R.Offset(X, Y);
  Bitmap.Surface.CopyTo(Bitmap.ClientRect, Surface, R, Alpha);
end;

procedure TDrawControlHelper.DrawCaption(Surface: ISurface; const Caption: string; const Rect: TRectI; Enabled: Boolean = True);
var
  F: IFont;
  C: TColorB;
  R: TRectI;
begin
  F := NewFont(Self.Font);
  C := Font.Color;
  R := Rect;
  if not Enabled then
  begin
    R.Offset(1, 1);
    F.Color := clWhite;
  end;
  Surface.TextOut(F, Caption, R, drLeft);
  if not Enabled then
  begin
    R.Offset(-1, -1);
    if THSL(C).Lightness > 0.5 then
      F.Color := C.Darken(0.5).Desaturate(0.5)
    else
      F.Color := C.Lighten(0.5).Desaturate(0.5);
    Surface.TextOut(F, Caption, R, drLeft);
  end
end;

procedure TDrawControlHelper.DrawText(Surface: ISurface; const Text: string; const Rect: TRectI; Direction: TDirection);
begin
  Surface.TextOut(NewFont(Self.Font), Text, Rect, Direction);
end;

procedure TDrawControlHelper.DrawTextState(Surface: ISurface; const Text: string; const Rect: TRectI; State: TDrawState; Radius: Float = 0);
const
  TextMargin = -6;
var
  R: TRectI;
begin
  DrawRectState(Surface, Rect, State, Radius);
  R := Rect;
  R.Inflate(TextMargin, 0);
  Surface.TextOut(NewFont(Self.Font), Text, R, drLeft);
end;

procedure TDrawControlHelper.DrawRectState(Surface: ISurface; const Rect: TRectI; State: TDrawState; Radius: Float = 0);
var
  C: TColorB;
  G: IGradientBrush;
  B: IBrush;
  P: IPen;
begin
  B := NewBrush(CurrentColor);
  Surface.FillRect(B, Rect);
  if dsSelected in State then
  begin
    C := clHighlight;
    if dsFocused in State then
    begin
      G := NewBrush(0, Rect.Top, 0, Rect.Bottom);
      G.AddStop(C.Fade(0.1), 0);
      G.AddStop(C.Fade(0.5), 0.8);
      if Radius > 1 then
        Surface.FillRoundRect(G, Rect, Radius)
      else
        Surface.FillRect(G, Rect);
      P := NewPen(C.Fade(0.8));
      if Radius > 1 then
        Surface.StrokeRoundRect(P, Rect, Radius)
      else
        Surface.StrokeRect(P, Rect);
    end
    else
    begin
      B := NewBrush(C.Fade(0.2));
      if Radius > 1 then
        Surface.FillRoundRect(B, Rect, Radius)
      else
        Surface.FillRect(B, Rect);
      P := NewPen(C.Fade(0.4));
      if Radius > 1 then
        Surface.StrokeRoundRect(P, Rect, Radius)
      else
        Surface.StrokeRect(P, Rect);
    end;
  end
  else
  if dsHot in State then
  begin
    C := clHighlight;
    P := NewPen(C.Fade(0.5));
    if Radius > 1 then
      Surface.StrokeRoundRect(P, Rect, Radius)
    else
      Surface.StrokeRect(P, Rect);
  end;
end;

var
  OriginalTheme: TThemeClass = TDefaultTheme;
  InternalTheme: TThemeClass = TDefaultTheme;
  InternalThemes: TThemeClassArray;
  InternalThemeNotify: TMethodDelegate;

function Theme: TThemeClass;
begin
  Result := InternalTheme;
end;

procedure ThemeRegisiter(ThemeClass: TThemeClass);
begin
  InternalThemes.Push(ThemeClass);
end;

function ThemeFind(const ThemeName: string): TThemeClass;
var
  T: TThemeClass;
  S: string;
begin
  for T in InternalThemes do
  begin
    S := T.Name;
    if S = ThemeName then
      Exit(T);
  end;
  Result := nil;
end;

procedure ThemeChange(const ThemeName: string);
var
  T: TThemeClass;
  M: TMethodEvent;
begin
  T := ThemeFind(ThemeName);
  if T = nil then
    Exit;
  InternalTheme := T;
  OriginalTheme := T;
  if T <> InternalTheme then
    for M in InternalThemeNotify do
      M;
end;

procedure ThemeNotifyAdd(Event: TMethodEvent);
begin
  InternalThemeNotify.Add(Event);
end;

procedure ThemeNotifyRemove(Event: TMethodEvent);
begin
  InternalThemeNotify.Remove(Event);
end;

procedure ThemeNames(Strings: TStrings);
var
  T: TThemeClass;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    for T in InternalThemes do
      Strings.Add(T.Name);
  finally
    Strings.EndUpdate;
  end;
end;

{ TTheme }

class function TTheme.Name: string;
var
  S: string;
  I: Integer;
begin
  S := ClassName;
  S := S.Replace('Theme', '');
  Result := '';
  for I := 2 to S.Length do
  begin
    if S[I] in ['A'..'Z'] then
      Result := Result + ' ';
    Result := Result + S[I];
  end;
  Result := Result.Trim;
end;

class procedure TTheme.Select(Control: TControl; Surface: ISurface;
  State: TDrawState; Font: TFont);
begin
  Self.Control := Control;
  Self.Surface := Surface;
  Self.State := State;
  Self.Font := NewFont(Font);
end;

class procedure TTheme.Select(State: TDrawState);
begin
  Self.State := State;
end;

class procedure TTheme.Select(ThemeName: string);
var
  T: TThemeClass;
begin
  T := ThemeFind(ThemeName);
  if T <> nil then
  begin
    OriginalTheme := InternalTheme;
    InternalTheme := T;
  end;
end;

class procedure TTheme.Deselect;
begin
  Control := nil;
  Surface := nil;
  Font := nil;
  State := [];
  InternalTheme := OriginalTheme;
end;

class function TTheme.GetSelected: Boolean;
begin
  Result :=  (Control <> nil) and (Surface <> nil) and (Font <> nil);
end;

{ TDefaultTheme }

class procedure TDefaultTheme.DrawButton(const Rect: TRectI);
begin

end;

class procedure TDefaultTheme.DrawButtonThin(const Rect: TRectI);
const
  Radius = 3;
var
  R: TRectI;
  C: TColorB;
  G: IGradientBrush;
begin
  R := Rect;
  C := Control.CurrentColor;
  if dsPressed in State then
  begin
    G := NewBrush(0, 0, 0, R.Height);
    G.AddStop(C.Fade(0.6).Darken(0.5), 0);
    G.AddStop(C.Fade(0).Darken(0.5), 1);
    Surface.FillRoundRect(G, Rect, 3);
    Surface.StrokeRoundRect(NewPen(C.Fade(0.6).Darken(0.6)), Rect, Radius);
  end
  else if dsHot in State then
  begin
    G := NewBrush(R.Left, R.Top, R.Left, R.Bottom);
    C := Control.CurrentColor;
    G.AddStop(C.Lighten(0.4), 0);
    G.AddStop(C.Darken(0.1), 1);
    R.Inflate(-1, -1);
    Surface.FillRect(G, R);
    Surface.StrokeRect(NewPen(clWhite), R);
    R.Inflate(1, 1);
    Surface.StrokeRoundRect(NewPen(clBtnShadow), Rect, Radius);
  end;
end;

class procedure TDefaultTheme.DrawSplit(const Rect: TRectI; Orientation: TThemeOrientation);
var
  R: TRectI;
begin
  R := Rect;
  if Orientation = toHorizontal then
  begin
    R.Top := R.MidPoint.Y - 1;
    R.Height := 1;
  end
  else
  begin
    R.Left := R.MidPoint.X - 1;
    R.Width := 1;
  end;
  Surface.FillRect(NewBrush(Control.CurrentColor.Darken(0.2)), R);
end;

class function TDefaultTheme.MeasureThumbThin(Orientation: TThemeOrientation): TPointI;
begin
  if Orientation = toHorizontal then
    Result := TPointI.Create(8, 16)
  else
    Result := TPointI.Create(16, 8);
end;

class procedure TDefaultTheme.DrawThumbThin(const Rect: TRectI; Orientation: TThemeOrientation);
const
  Radius = 3;
var
  G: IGradientBrush;
  C: TColorB;
  R: TRectI;
begin
  R := Rect;
  if Orientation = toVertical then
    G := NewBrush(R.Left, R.Top, R.Right, R.Top)
  else
    G := NewBrush(R.Left, R.Top, R.Left, R.Bottom);
  C := Control.CurrentColor;
  G.AddStop(C.Lighten(0.5), 0);
  G.AddStop(C.Darken(0.2), 1);
  if [dsHot, dsPressed] * State <> [] then
  begin
    C := clWhite;
    C := C.Blend(clHighlight, 0.75);
    R.Inflate(-1, -1);
    Surface.FillRoundRect(NewBrush(C), R, Radius);
    if Orientation = toVertical then
      R.Inflate(-3, 0)
    else
      R.Inflate(0, -3);
    Surface.FillRect(G, R);
    Surface.StrokeRect(NewPen(clWhite), R);
  end
  else
  begin
    Surface.FillRoundRect(G, Rect, Radius);
    R := Rect;
    R.Inflate(-1, -1);
    Surface.StrokeRoundRect(NewPen(clWhite), R, Radius);
  end;
  Surface.StrokeRoundRect(NewPen(clBtnShadow), Rect, Radius);
end;

class function TDefaultTheme.MeasureBorder: TPointI;
begin
  Result := TPointI.Create(1, 1);
end;

class procedure TDefaultTheme.DrawBorder(const Rect: TRectI);
begin

end;

class function TDefaultTheme.MeasureEditBorder: TPointI;
begin
  Result := TPointI.Create(2, 2);
end;

class procedure TDefaultTheme.DrawEditBorder(const Rect: TRectI);
begin

end;

class procedure TDefaultTheme.DrawHeaderColumn(const Rect: TRectI; Sort: TSortingOrder = soNone);
const
  SortSize = 3;
var
  R: TRectI;
  B: IGradientBrush;
  H: THSL;
  C: TColorB;
  I: Integer;
begin
  R := Rect;
  B := NewBrush(0, R.Height, 0, 0);
  if dsPressed in State then
  begin
    C := Control.CurrentColor;
    B.AddStop(C, 0);
    B.AddStop(C, 0.25);
    B.AddStop(C.Darken(0.1), 1);
  end
  else if dsSelected in State then
  begin
    C := clHighlight;
    H := THSL(C);
    H.Lightness := 0.925;
    C := H;
    B.AddStop(C, 0.4);
    C := C.Lighten(0.6);
    B.AddStop(C, 0.5);
  end
  else
  begin
    C := Control.CurrentColor;
    B.AddStop(C.Fade(0.8).Darken(0.1), 0);
    B.AddStop(C, 0.5);
    B.AddStop(C.Lighten(0.8), 0.75);
  end;
  Surface.FillRect(B, R);
  if dsBackground in State then
    Exit;
  R.Inflate(-1, -1);
  R.Bottom := Rect.Bottom + 1;
  StrokeRectColor(Surface, R, clWhite);
  R := Rect;
  R.Inflate(0, 5);
  R.Left := -5;
  C := clBtnShadow;
  C := C.Lighten(0.5);
  StrokeRectColor(Surface, R, C);
  C := clBtnShadow;
  C := C.Lighten(0.4);
  if dsPressed in State then
  begin
    C := Control.CurrentColor;
    C := C.Darken(0.5);
    R.Left := Rect.Left - 1;
    StrokeRectColor(Surface, R, C);
  end
  else if dsSelected in State then
  begin
    C := clHighlight;
    C := C.Lighten(0.5);
    R := Rect;
    R.Left := R.Left - 1;
    StrokeRectColor(Surface, R, C);
    C := clHighlight;
    C := C.Lighten(0.25);
  end;
  if dsHot in State then
  begin
    R := Rect;
    C := clHighlight;
    C := C.Invert;
    R.Inflate(-2, 0);
    R.Top := R.Bottom - 3;
    FillRectColor(Surface, R, C.Lighten(0.4));
  end;
  R := Rect;
  Inc(R.Y);
  if Sort = soAscend then
  begin
    I := R.MidPoint.X;
    Surface.MoveTo(I, R.Y);
    Surface.LineTo(I + SortSize, R.Y + SortSize);
    Surface.LineTo(I - SortSize, R.Y + SortSize);
    Surface.Path.Close;
    Surface.Fill(NewBrush(C.Lighten(0.6)), True);
    Surface.Stroke(NewPen(C));
  end
  else if Sort = soDescend then
  begin
    I := R.MidPoint.X;
    Surface.MoveTo(I, R.Y + SortSize);
    Surface.LineTo(I - SortSize, R.Y);
    Surface.LineTo(I + SortSize, R.Y);
    Surface.Path.Close;
    Surface.Fill(NewBrush(C.Lighten(0.6)), True);
    Surface.Stroke(NewPen(C));
  end;
end;

class procedure TDefaultTheme.DrawHeader(Height: Integer = DefaulHeaderHeight);
var
  R: TRectI;
  B: IGradientBrush;
  C: TColorB;
begin
  R := Control.ClientRect;
  R.Height := Height;
  B := NewBrush(0, 0, 0, R.Height);
  C := Control.CurrentColor;
  B.AddStop(C.Fade(0.8).Darken(0.1), 0);
  B.AddStop(C.Fade(0.8), 0.5);
  B.AddStop(C.Fade(0.8).Lighten(0.3), 1);
  Surface.FillRect(B, R);
end;

class procedure TDefaultTheme.DrawHeaderShadow(Top: Integer);
var
  R: TRectI;
  B: IBrush;
  C: TColorB;
begin
  C := clBlack;
  C.Alpha := 5;
  B := NewBrush(C);
  R := Control.ClientRect;
  R.Top := Top;
  R.Height := 8;
  while R.Height > 1 do
  begin
    Surface.FillRect(B, R);
    Dec(R.Height);
  end;
end;

class procedure TDefaultTheme.DrawFooter(Height: Integer = DefaultFooterHeight);
var
  R: TRectI;
  B: IGradientBrush;
begin
  R := Control.ClientRect;
  R.Top := R.Bottom - Height;
  R.Height := Height;
  R.Offset(0, Height div -2);
  B := NewBrush(R);
  B.AddStop(clBlack, 0);
  B.AddStop(clTransparent, 1);
  R.Offset(0, Height div 2);
  B.Opacity := $40;
  Surface.FillRect(B, R);
end;

class procedure TDefaultTheme.DrawFooterGrip;
var
  Light, Dark: IBrush;
  Rect: TRectI;
  R: TRectI;
begin
  Light := NewBrush(Control.CurrentColor.Lighten(0.85));
  Dark := NewBrush(Control.CurrentColor.Darken(0.25));
  Rect := Control.ClientRect;
  R := TRectI.Create(2, 2);
  R.X := Rect.Right - 3;
  R.Y := Rect.Bottom - 9;
  Surface.FillRect(Light, R);
  R.Offset(-1, -1);
  Surface.FillRect(Dark, R);
  R.Offset(-2, 4);
  Surface.FillRect(Light, R);
  R.Offset(-1, -1);
  Surface.FillRect(Dark, R);
  R.Offset(-2, 4);
  Surface.FillRect(Light, R);
  R.Offset(-1, -1);
  Surface.FillRect(Dark, R);
  R.Offset(4, 1);
  Surface.FillRect(Light, R);
  R.Offset(-1, -1);
  Surface.FillRect(Dark, R);
  R.Offset(4, 1);
  Surface.FillRect(Light, R);
  R.Offset(-1, -1);
  Surface.FillRect(Dark, R);
  R.Offset(1, -2);
  Surface.FillRect(Light, R);
  R.Offset(-1, -1);
  Surface.FillRect(Dark, R);
end;

{ TRedmondTheme }

class procedure TRedmondTheme.DrawButton(const Rect: TRectI);
begin

end;

class procedure TRedmondTheme.DrawButtonThin(const Rect: TRectI);
var
  R: TRectI;
begin
  R := Rect;
  Surface.StrokeRect(NewPen(cl3DDkShadow), R);
  R.Width := R.Width - 1;
  R.Height := R.Height - 1;
  Surface.StrokeRect(NewPen(clBtnShadow), R);
  R.Width := R.Width - 1;
  R.Height := R.Height - 1;
  R.Offset(1, 1);
  Surface.FillRect(NewBrush(clBtnFace), R);
  Surface.StrokeRect(NewPen(clBtnHighlight), R);
end;

class procedure TRedmondTheme.DrawSplit(const Rect: TRectI; Orientation: TThemeOrientation);
var
  R: TRectI;
begin
  R := Rect;
  if Orientation = toHorizontal then
  begin
    R.Top := R.MidPoint.Y - 1;
    R.Height := 1;
  end
  else
  begin
    R.Left := R.MidPoint.X - 1;
    R.Width := 1;
  end;
  Surface.FillRect(NewBrush(clBtnShadow), R);
  if Orientation = toHorizontal then
    R.Offset(0, 1)
  else
    R.Offset(1, 0);
  Surface.FillRect(NewBrush(clBtnHighlight), R);
end;

class function TRedmondTheme.MeasureThumbThin(Orientation: TThemeOrientation): TPointI;
begin
  if Orientation = toHorizontal then
    Result := TPointI.Create(8, 16)
  else
    Result := TPointI.Create(16, 8);
end;

class procedure TRedmondTheme.DrawThumbThin(const Rect: TRectI; Orientation: TThemeOrientation);
var
  R: TRectI;
begin
  R := Rect;
  Surface.StrokeRect(NewPen(cl3DDkShadow), R);
  R.Width := R.Width - 1;
  R.Height := R.Height - 1;
  Surface.StrokeRect(NewPen(clBtnShadow), R);
  R.Width := R.Width - 1;
  R.Height := R.Height - 1;
  R.Offset(1, 1);
  Surface.FillRect(NewBrush(clBtnFace), R);
  Surface.StrokeRect(NewPen(clBtnHighlight), R);
end;

class function TRedmondTheme.MeasureBorder: TPointI;
begin
  Result := TPointI.Create(2, 2);
end;

class procedure TRedmondTheme.DrawBorder(const Rect: TRectI);
begin
end;

class function TRedmondTheme.MeasureEditBorder: TPointI;
begin
  Result := TPointI.Create(2, 2);
end;

class procedure TRedmondTheme.DrawEditBorder(const Rect: TRectI);
begin
end;

class procedure TRedmondTheme.DrawHeaderColumn(const Rect: TRectI; Sort: TSortingOrder = soNone);
begin

end;

class procedure TRedmondTheme.DrawHeader(Height: Integer = DefaulHeaderHeight);
begin

end;

class procedure TRedmondTheme.DrawHeaderShadow(Top: Integer = 0);
begin

end;

class procedure TRedmondTheme.DrawFooter(Height: Integer = DefaultFooterHeight);
begin

end;

class procedure TRedmondTheme.DrawFooterGrip;
begin

end;

initialization
  ThemeRegisiter(TDefaultTheme);
  ThemeRegisiter(TRedmondTheme);
  FontBitmap := nil;
finalization
  FontBitmap.Free;
end.

