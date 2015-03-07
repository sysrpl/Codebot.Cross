(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://www.codebot.org                              *)
(*  Modified February 2015                              *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.graphics.txt> }
unit Codebot.Graphics;

{$i codebot.inc}

interface

uses
  Classes, SysUtils, Graphics, Controls, Themes,
  Codebot.System,
  Codebot.Graphics.Types;

{ Create a new matrix }
function NewMatrix: IMatrix;
{ Create a new pen using a brush as the color }
function NewPen(Brush: IBrush; Width: Float = 1): IPen; overload;
{ Create a new solid color pen }
function NewPen(Color: TBGRA; Width: Float = 1): IPen; overload;
{ Create a new solid color brush }
function NewBrush(Color: TBGRA): ISolidBrush; overload;
{ Create a new bitmap pattern brush }
function NewBrush(Bitmap: IBitmap): IBitmapBrush; overload;
{ Create a new linear gradient brush using four coordinates for endpoints }
function NewBrush(X1, Y1, X2, Y2: Float): ILinearGradientBrush; overload;
{ Create a new linear gradient brush using two points for endpoints }
function NewBrush(const A, B: TPointF): ILinearGradientBrush; overload;
{ Create a new radial gradient brush bounded by a rect }
function NewBrush(const Rect: TRectF): IRadialGradientBrush; overload;
{ Create a new font by copying a regular font object }
function NewFont(Font: TFont): IFont;
{ Create a new canvas using a regular canvas object }
function NewSurface(Canvas: TCanvas): ISurface; overload;
{ Create a new canvas using a window }
function NewSurface(Control: TWinControl): ISurface; overload;
{ Create a new bitmap of width and height size }
function NewBitmap(Width, Height: Integer): IBitmap;
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
    property Bitmap: IBitmap read FBitmap;
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
      const Rect: TRectI; Opacity: Byte; Saturation: Float); overload;
    procedure Add(Image: TSurfaceBitmap);
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

{ Drawing routines which operate indepentent of a control }

procedure FillRectColor(Surface: ISurface; const Rect: TRectI; Color: TColorB; Radius: Float = 0);
procedure StrokeRectColor(Surface: ISurface; const Rect: TRectI; Color: TColorB; Radius: Float = 0);
function DrawDummyBitmap(Width, Height: Integer): IBitmap;
function DrawHueLinear(Width, Height: Integer): IBitmap;
function DrawHueRadial(Width, Height: Integer): IBitmap;
function DrawSaturationBox(Width, Height: Integer; Hue: Float): IBitmap;
function DrawDesaturationBox(Width, Height: Integer; Hue: Float): IBitmap;

{ Brushes creates a series of bitmap batterns }

type
  Brushes = record
    class function Checker(Size: Integer = 10): IBrush; overload; static;
    class function Checker(Foreground, Background: TColorB; Size: Integer = 10): IBrush; overload; static;
  end;

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
    procedure DrawBitmap(Surface: ISurface; Bitmap: IBitmap; X, Y: Integer);
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
  protected
    class var Control: TControl;
    class var Surface: ISurface;
    class var State: TDrawState;
    class var Font: TFont;
  public
    class procedure Select(Control: TControl; Surface: ISurface; State: TDrawState; Font: TFont); overload;
    class procedure Select(State: TDrawState); overload;
    class procedure Select(ThemeName: string); overload;
    class procedure Deselect;
    class function Name: string;
    class procedure DrawSplit(const Rect: TRectI; Orientation: TThemeOrientation); virtual; abstract;
    class function MeasureThumbThin(Orientation: TThemeOrientation): TPointI; virtual; abstract;
    class procedure DrawThumbThin(const Rect: TRectI; Orientation: TThemeOrientation); virtual; abstract;
    class function MeasureBorder: TPointI; virtual; abstract;
    class procedure DrawBorder(const Rect: TRectI); virtual; abstract;
    class function MeasureEditBorder: TPointI; virtual; abstract;
    class procedure DrawEditBorder(const Rect: TRectI); virtual; abstract;
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
    class procedure DrawSplit(const Rect: TRectI; Orientation: TThemeOrientation); override;
    class function MeasureThumbThin(Orientation: TThemeOrientation): TPointI; override;
    class procedure DrawThumbThin(const Rect: TRectI; Orientation: TThemeOrientation); override;
    class function MeasureBorder: TPointI; override;
    class procedure DrawBorder(const Rect: TRectI); override;
    class function MeasureEditBorder: TPointI; override;
    class procedure DrawEditBorder(const Rect: TRectI); override;
    class procedure DrawHeader(Height: Integer = DefaulHeaderHeight); override;
    class procedure DrawHeaderShadow(Top: Integer = 0); override;
    class procedure DrawFooter(Height: Integer = DefaultFooterHeight); override;
    class procedure DrawFooterGrip; override;
  end;

{ TRedmondTheme }

  TRedmondTheme = class(TTheme)
    class procedure DrawSplit(const Rect: TRectI; Orientation: TThemeOrientation); override;
    class function MeasureThumbThin(Orientation: TThemeOrientation): TPointI; override;
    class procedure DrawThumbThin(const Rect: TRectI; Orientation: TThemeOrientation); override;
    class function MeasureBorder: TPointI; override;
    class procedure DrawBorder(const Rect: TRectI); override;
    class function MeasureEditBorder: TPointI; override;
    class procedure DrawEditBorder(const Rect: TRectI); override;
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

{$ifdef linux}
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

function NewFont(Font: TFont): IFont;
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

function NewBitmap(Width, Height: Integer): IBitmap;
begin
  Result := NewBitmapCairo(Width, Height);
end;

function NewSplash: ISplash;
begin
  Result := NewSplashCairo;
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

function NewBitmap(Width, Height: Integer): IBitmap;
begin
  if LoadD2D then
    Result := NewBitmapD2D(Width, Height)
  else
    Result := NewBitmapGdi(Width, Height);
end;

function NewBitmapWin: IBitmap;
begin
  Result := NewBitmapD2D(0, 0);
end;

function NewSplash: ISplash;
begin
  if not Assigned(NewBitmapProc) then
    NewBitmapProc := NewBitmapWin;
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
  if FWidth > FHeight then
  begin
    Result.Width := FHeight;
    Result.Offset(Index * FHeight, 0);
  end
  else
  begin
    Result.Height := FWidth;
    Result.Offset(0, Index * FWidth);
  end;
end;

function TSurfaceBitmap.GetFrameCount: Integer;
begin
  Result := 0;
  if Empty then
    Exit;
  if FWidth > FHeight then
    Result := FWidth div FHeight
  else
    Result := FHeight div FWidth;
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
  Result := (FWidth < 1) or (FHeight < 1);
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
  Value := True;
end;

function TSurfaceBitmap.GetWidth: Integer;
begin
	Result := FWidth;
end;

procedure TSurfaceBitmap.SetWidth(Value: Integer);
begin
  if Value < 1 then
    Value := 0;
  if Value <> FWidth then
  begin
    FWidth := Value;
    HandleRelease;
  end;
end;

function TSurfaceBitmap.GetHeight: Integer;
begin
	Result := FHeight;
end;

procedure TSurfaceBitmap.SetHeight(Value: Integer);
begin
  if Value < 1 then
    Value := 0;
  if Value <> FHeight then
  begin
    FHeight := Value;
    HandleRelease;
  end;
end;

procedure TSurfaceBitmap.Draw(Canvas: TCanvas; const Rect: TRect);
var
  S: ISurface;
begin
  if Empty then
    Exit;
  HandleNeeded;
  S := NewSurface(Canvas);
  if S <> nil then
    FBitmap.Surface.CopyTo(FBitmap.ClientRect, FBitmap.Surface, Rect, FOpacity);
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
    Shades[I] := Color.Lighten(I / High(Byte));
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
  if (Width = FWidth) or (Height = FHeight) then
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
  if (Width <> FWidth) or (Height <> FHeight) then
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
    B.Desaturate(0.75);
    B.Opacity := $A0;
    B.Draw(Surface, X, Y);
    B.Free;
  end
  else if dsHot in State then
  begin
    B := TSurfaceBitmap.Create;
    B.SetSize(Size, Size);
    FBitmap.Draw(B.Surface, 0, 0, Index);
    if dsPressed in State then
      B.Darken(0.25)
    else
      B.Lighten(0.25);
    B.Draw(Surface, X, Y);
    B.Free;
  end
  else
    FBitmap.Draw(Surface, X, Y, Index);
end;

procedure TImageStrip.Draw(Surface: ISurface; Index: Integer;
  const Rect: TRectI; Opacity: Byte; Saturation: Float);
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


{ Brushes }

class function Brushes.Checker(Size: Integer = 10): IBrush;
begin
  Result := Brushes.Checker(clSilver, clWhite, Size);
end;

class function Brushes.Checker(Foreground, Background: TColorB; Size: Integer = 10): IBrush;
var
  Bitmap: IBitmap;
  Brush: IBrush;
  R: TRectI;
begin
  if Size < 1 then
    Size := 1;
  Bitmap := NewBitmap(Size * 2, Size * 2);
  Brush := NewBrush(Foreground);
  R := TRectI.Create(Size, Size);
  Bitmap.Surface.FillRect(Brush, R);
  R.Offset(Size, Size);
  Bitmap.Surface.FillRect(Brush, R);
  Brush := NewBrush(Background);
  R.Offset(0, -Size);
  Bitmap.Surface.FillRect(Brush, R);
  R.Offset(-Size, Size);
  Bitmap.Surface.FillRect(Brush, R);
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

procedure TDrawControlHelper.DrawBitmap(Surface: ISurface; Bitmap: IBitmap; X, Y: Integer);
var
  R: TRectI;
begin
  if Bitmap.Empty then
    Exit;
  R := Bitmap.ClientRect;
  R.Offset(X, Y);
  Bitmap.Surface.CopyTo(Bitmap.ClientRect, Surface, R);
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
  Self.Font := Font;
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
  State := [];
  Font := nil;
  InternalTheme := OriginalTheme;
end;

class function TTheme.GetSelected: Boolean;
begin
  Result :=  (Control <> nil) and (Surface <> nil) and (Font <> nil);
end;

{ TDefaultTheme }

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
var
  R: TRectI;
begin
  R := Rect;
  if [dsHot, dsPressed] * State <> [] then
  begin
    Surface.FillRoundRect(NewBrush(clHighlight), R, 4);
    if Orientation = toVertical then
	    R.Inflate(-4, 0)
    else
	    R.Inflate(0, -4);
    Surface.FillRect(NewBrush(clBtnFace), R);
  end
  else
    Surface.FillRoundRect(NewBrush(clBtnFace), Rect, 4);
  Surface.StrokeRoundRect(NewPen(clBtnShadow), Rect, 4);
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
  B.AddStop(C, 0.5);
  B.AddStop(C.Lighten(0.8), 1);
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
  { Toolbar button
  Surface.StrokeRect(NewPen(clBtnShadow), R);
  R.Width := R.Width - 1;
  R.Height := R.Height - 1;
  Surface.StrokeRect(NewPen(clBtnHighlight), R);
  R.Width := R.Width - 1;
  R.Height := R.Height - 1;
  R.Offset(1, 1);
  Surface.FillRect(NewBrush(clBtnFace), R);}
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

