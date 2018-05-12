(********************************************************)
(*                                                      *)
(*  Codebot.Cross Pascal Library                        *)
(*  http://cross.codebot.org                            *)
(*  Modified March 2015                                 *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.graphics.types.txt> }
unit Codebot.Graphics.Types;

{$i codebot.inc}

interface

uses
  SysUtils, Classes, Graphics, Math,
  Codebot.System;

{ TDirection has several applications. When drawing text it has
  the following meaning:

  drLeft: Single line text aligned left center
  drUp: Single line aligned top center
  drRight: Single line aligned right center
  drBottom: Single line aligned bottom center
  drCenter: Single line aligned absolute center
  drFill: Wrapping lines of text aligned center
  drWrap: Wrapping lines of text starting at the top left (like a text editor)  }

type
  TDirection = (drLeft, drUp, drRight, drDown, drCenter, drFill, drWrap, drFlow);
  TDirections = set of TDirection;

{ AlignDir converts ebtween TAlignment and TDirection }

const
  AlignDir: array[TAlignment] of TDirection = (drLeft, drRight, drCenter);

{ TDrawStateItem }

type
  TDrawStateItem = (dsDisabled, dsPressed, dsSelected, dsHot, dsFocused, dsChecked,
    dsExpanded, dsDefaulted, dsThin, dsFlat, dsBackground, dsCustom);
  TDrawState = set of TDrawStateItem;

{ TPointI }

  TPointI = record
  public
    X, Y: Integer;
    class operator Implicit(const Value: TPointI): TPoint;
    class operator Implicit(const Value: TPoint): TPointI;
    class operator Negative(const A: TPointI): TPointI;
    class operator Equal(const A, B: TPointI): Boolean;
    class operator NotEqual(const A, B: TPointI): Boolean;
    class operator Add(const A, B: TPointI): TPointI;
    class operator Subtract(const A, B: TPointI): TPointI;
    function Equals(const Value: TPointI): Boolean;
    class function Create: TPointI; overload; static;
    class function Create(X, Y: Integer): TPointI; overload; static;
    function Angle(const P: TPointI): Float;
    function Dist(const P: TPointI): Float;
    function Mid(const P: TPointI): TPointI;
    procedure Offset(X, Y: Integer); overload;
    procedure Offset(const P: TPointI); overload;
    function Move(X, Y: Integer): TPointI; overload;
    function Move(const P: TPointI): TPointI; overload;
  end;
  PPointI = ^TPointI;

{ TRectI }

  TRectI = record
  private
    function GetEmpty: Boolean;
    procedure SetTop(Value: Integer);
    procedure SetLeft(Value: Integer);
    function GetRight: Integer;
    procedure SetRight(Value: Integer);
    function GetBottom: Integer;
    procedure SetBottom(Value: Integer);
    function GetSize: TPointI;
    function GetTopLeft: TPointI;
    function GetBottomLeft: TPointI;
    function GetBottomRight: TPointI;
    function GetTopRight: TPointI;
    function GetMidPoint: TPointI;
  public
    X, Y, Width, Height: Integer;
    class operator Implicit(const Value: TRectI): TRect;
    class operator Implicit(const Value: TRect): TRectI;
    class operator Equal(const A, B: TRectI): Boolean;
    class operator NotEqual(const A, B: TRectI): Boolean;
    class function Create: TRectI; overload; static;
    class function Create(Size: TPointI): TRectI; overload; static;
    class function Create(W, H: Integer): TRectI; overload; static;
    class function Create(X, Y, W, H: Integer): TRectI; overload; static;
    function Equals(const Value: TRectI): Boolean;
    function Contains(X, Y: Integer): Boolean; overload;
    function Contains(const P: TPointI): Boolean; overload;
    procedure Center(X, Y: Integer); overload;
    procedure Center(const P: TPointI); overload;
    procedure Inflate(X, Y: Integer); overload;
    procedure Inflate(const P: TPointI); overload;
    procedure Offset(X, Y: Integer); overload;
    procedure Offset(const P: TPointI); overload;
    property Empty: Boolean read GetEmpty;
    property Left: Integer read X write SetLeft;
    property Top: Integer read Y write SetTop;
    property Right: Integer read GetRight write SetRight;
    property Bottom: Integer read GetBottom write SetBottom;
    property Size: TPointI read GetSize;
    property TopLeft: TPointI read GetTopLeft;
    property BottomLeft: TPointI read GetBottomLeft;
    property BottomRight: TPointI read GetBottomRight;
    property TopRight: TPointI read GetTopRight;
    property MidPoint: TPointI read GetMidPoint;
  end;
  PRectI = ^TRectI;

{ TPointF }

  TPointF = record
  public
    X, Y: Float;
    class operator Implicit(const Value: TPointI): TPointF;
    class operator Implicit(const Value: TPoint): TPointF;
    class operator Explicit(const Value: TPointF): TPointI;
    class operator Explicit(const Value: TPointF): TPoint;
    class operator Negative(const A: TPointF): TPointF;
    class operator Equal(const A, B: TPointF): Boolean;
    class operator NotEqual(const A, B: TPointF): Boolean;
    class operator Add(const A, B: TPointF): TPointF;
    class operator Subtract(const A, B: TPointF): TPointF;
    class operator Multiply(const A: TPointF; B: Float): TPointF;
    function Equals(const Value: TPointF): Boolean;
    class function Create: TPointF; overload; static;
    class function Create(X, Y: Float): TPointF; overload; static;
    procedure Offset(X, Y: Float); overload;
    procedure Offset(const P: TPointF); overload;
    function Move(X, Y: Float): TPointF; overload;
    function Move(const P: TPointF): TPointF; overload;
    function Angle(const P: TPointF): Float;
    function Dist(const P: TPointF): Float;
    function Mid(const P: TPointF): TPointF;
    function Extend(const P: TPointF; Dist: Float): TPointF;
    function Rotate(const P: TPointF; Angle: Float): TPointF;
  end;
  PPointF = ^TPointF;

{ TRectF }

  TRectF = record
  private
    function GetEmpty: Boolean;
    procedure SetTop(Value: Float);
    procedure SetLeft(Value: Float);
    function GetRight: Float;
    procedure SetRight(Value: Float);
    function GetBottom: Float;
    procedure SetBottom(Value: Float);
    function GetSize: TPointF;
    function GetTopLeft: TPointF;
    function GetTopRight: TPointF;
    function GetBottomLeft: TPointF;
    function GetBottomRight: TPointF;
    function GetMidPoint: TPointF;
    function GetMidLeft: TPointF;
    function GetMidTop: TPointF;
    function GetMidRight: TPointF;
    function GetMidBottom: TPointF;
  public
    X, Y, Width, Height: Float;
    class operator Implicit(const Value: TRectI): TRectF;
    class operator Implicit(const Value: TRect): TRectF;
    class operator Explicit(const Value: TRectF): TRectI;
    class operator Explicit(const Value: TRectF): TRect;
    class operator Equal(const A, B: TRectF): Boolean;
    class operator NotEqual(const A, B: TRectF): Boolean;
    class operator Multiply(const A: TRectF; B: Float): TRectF;
    class function Create: TRectF; overload; static;
    class function Create(Size: TPointF): TRectF; overload; static;
    class function Create(W, H: Float): TRectF; overload; static;
    class function Create(X, Y, W, H: Float): TRectF; overload; static;
    function Equals(const Value: TRectF): Boolean;
    function Contains(X, Y: Float): Boolean; overload;
    function Contains(const P: TPointF): Boolean; overload;
    procedure Center(X, Y: Float); overload;
    procedure Center(const P: TPointF); overload;
    procedure Center(const R: TRectF); overload;
    procedure Inflate(X, Y: Float); overload;
    procedure Inflate(const P: TPointF); overload;
    procedure Offset(X, Y: Float); overload;
    procedure Offset(const P: TPointF); overload;
    property Empty: Boolean read GetEmpty;
    property Left: Float read X write SetLeft;
    property Top: Float read Y write SetTop;
    property Right: Float read GetRight write SetRight;
    property Bottom: Float read GetBottom write SetBottom;
    property Size: TPointF read GetSize;
    property TopLeft: TPointF read GetTopLeft;
    property TopRight: TPointF read GetTopRight;
    property BottomLeft: TPointF read GetBottomLeft;
    property BottomRight: TPointF read GetBottomRight;
    property MidPoint: TPointF read GetMidPoint;
    property MidLeft: TPointF read GetMidLeft;
    property MidTop: TPointF read GetMidTop;
    property MidRight: TPointF read GetMidRight;
    property MidBottom: TPointF read GetMidBottom;
  end;
  PRectF = ^TRectF;

  TMatrixOrder = (moPrepend, moAppend);

{ TMatrix2x3 }

  TMatrix2x3 = record
  public
    M11, M12, M21, M22, M31, M32: Float;
    Order: TMatrixOrder;
    class operator Equal(const A, B: TMatrix2x3): Boolean;
    class operator Negative(const A: TMatrix2x3): TMatrix2x3;
    class operator Multiply(const A, B: TMatrix2x3): TMatrix2x3;
    class operator Multiply(const A: TMatrix2x3; const B: TPointF): TPointF;
    class function Create: TMatrix2x3; static;
    function CanInvert: Boolean;
    procedure Identity;
    procedure Invert;
    procedure Rotate(Angle: Float);
    procedure RotateAt(Angle: Float; X, Y: Float); overload;
    procedure RotateAt(Angle: Float; const P: TPointF); overload;
    procedure Scale(SX, SY: Float);
    procedure ScaleAt(SX, SY, X, Y: Float); overload;
    procedure ScaleAt(SX, SY: Float; const P: TPointF); overload;
    procedure Skew(AngleX, AngleY: Float); overload;
    procedure Translate(X, Y: Float); overload;
    procedure Translate(const P: TPointF); overload;
    function Multiply(const M: TMatrix2x3): TMatrix2x3; overload;
    function Multiply(const P: TPointF): TPointF; overload;
  end;
  PMatrix2x3 = ^TMatrix2x3;

{ TColorAlpha }

  TColorAlpha = type LongWord;

{ TColorAlphaHelper }

  TColorAlphaHelper = record helper for TColorAlpha
  private
    function GetAlpha: Byte;
    function GetBlue: Byte;
    function GetGreen: Byte;
    function GetRed: Byte;
    procedure SetAlpha(Value: Byte);
    procedure SetBlue(Value: Byte);
    procedure SetGreen(Value: Byte);
    procedure SetRed(Value: Byte);
  public
    property Blue: Byte read GetBlue write SetBlue;
    property Green: Byte read GetGreen write SetGreen;
    property Red: Byte read GetRed write SetRed;
    property Alpha: Byte read GetAlpha write SetAlpha;
  end;

{ THSL }

  THSL = record
  public
    Hue, Saturation, Lightness: Float;
    class operator Implicit(Value: Float): THSL;
    class operator Implicit(const Value: THSL): Float;
    class function Create(H, S, L: Float): THSL; static;
  end;
  PHSL = ^THSL;

{ TColorB }

  TColorB = packed record
  public
    Blue, Green, Red, Alpha: Byte;
    class operator Implicit(Value: TColorB): TColorAlpha;
    class operator Implicit(Value: TColorAlpha): TColorB;
    class operator Implicit(const Value: THSL): TColorB;
    class operator Explicit(Value: TColorB): THSL;
    class operator Implicit(Value: TColor): TColorB;
    class operator Explicit(Value: TColorB): TColor;
    class operator Negative(A: TColorB): TColorB;
    class operator Positive(A: TColorB): TColorB;
    class operator Equal(A: TColorB; B: TColorB): Boolean;
    class operator NotEqual(A: TColorB; B: TColorB): Boolean;
    class function Create(B, G, R: Byte; A: Byte = $FF): TColorB; static;
    function Invert: TColorB;
    function Blend(Value: TColorB; Percent: Float): TColorB;
    function Desaturate(Percent: Float): TColorB;
    function Darken(Percent: Float): TColorB;
    function Lighten(Percent: Float): TColorB;
    function Fade(Percent: Float): TColorB;
    function Color: TColor;
  end;
  PColorB = ^TColorB;

  TBGRA = TColorB;
  PBGRA = PColorB;
  TPixel = TColorB;
  PPixel = PColorB;

  TColorHelper = record helper for TColor
    function Blend(Value: TColorB; Percent: Float): TColorB;
  end;

{ TColorF }

  TColorF = record
  public
    Blue, Green, Red, Alpha: Float;
    class operator Implicit(const Value: THSL): TColorF;
    class operator Explicit(const Value: TColorF): THSL;
    class operator Implicit(Value: TColorB): TColorF;
    class operator Explicit(const Value: TColorF): TColorB;
    class operator Implicit(Value: TColor): TColorF;
    class operator Explicit(const Value: TColorF): TColor;
    class function Create(B, G, R: Float; A: Byte = 1): TColorF; static;
    function Blend(const Value: TColorF; Percent: Float): TColorF;
    function Desaturate(Percent: Float): TColorF;
    function Fade(Percent: Float): TColorF;
    function Color: TColor;
  end;
  PColorF = ^TColorF;

const
  PixelSize = SizeOf(TPixel);

  clTransparent: TColorB = (Blue: 0; Green: 0; Red: 0; Alpha: 0);
  clAliceBlue = TColor($FFF8F0);
  clAntiqueWhite = TColor($D7EBFA);
  clAquamarine = TColor($D4FF7F);
  clAzure = TColor($FFFFF0);
  clBeige = TColor($DCF5F5);
  clBisque = TColor($C4E4FF);
  clBlanchedAlmond = TColor($CDEBFF);
  clBlueViolet = TColor($E22B8A);
  clBrown = TColor($2A2AA5);
  clBurlywood = TColor($87B8DE);
  clCadetBlue = TColor($A09E5F);
  clChartreuse = TColor($00FF7F);
  clChocolate = TColor($1E69D2);
  clCoral = TColor($507FFF);
  clCornFlowerBlue = TColor($ED9564);
  clCornSilk = TColor($DCF8FF);
  clCrimson = TColor($3C14DC);
  clCyan = TColor($FFFF00);
  clDarkBlue = TColor($8B0000);
  clDarkCyan = TColor($8B8B00);
  clDarkGoldenrod = TColor($0B86B8);
  clDarkGray = TColor($A9A9A9);
  clDarkGreen = TColor($006400);
  clDarkKhaki = TColor($6BB7BD);
  clDarkMagenta = TColor($8B008B);
  clDarkOliveGreen = TColor($2F6B55);
  clDarkOrange = TColor($008CFF);
  clDarkOrchid = TColor($CC3299);
  clDarkRed = TColor($00008B);
  clDarkSalmon = TColor($7A96E9);
  clDarkSeaGreen = TColor($8BBC8F);
  clDarkSlateBlue = TColor($8B3D48);
  clDarkSlateGray = TColor($4F4F2F);
  clDarkTurquoise = TColor($D1CE00);
  clDarkViolet = TColor($D30094);
  clDeepPink = TColor($9314FF);
  clDeepSkyBlue = TColor($FFBF00);
  clDimGray = TColor($696969);
  clDodgerBlue = TColor($FF901E);
  clFireBrick = TColor($2222B2);
  clFloralWhite = TColor($F0FAFF);
  clForestGreen = TColor($228B22);
  clGainsboro = TColor($DCDCDC);
  clGhostWhite = TColor($FFF8F8);
  clGold = TColor($00D7FF);
  clGoldenrod = TColor($20A5DA);
  clGreenYellow = TColor($2FFFAD);
  clHoneydew = TColor($F0FFF0);
  clHotPink = TColor($B469FF);
  clIndianRed = TColor($5C5CCD);
  clIndigo = TColor($82004B);
  clIvory = TColor($F0FFFF);
  clKhaki = TColor($8CE6F0);
  clLavender = TColor($FAE6E6);
  clLavenderBlush = TColor($F5F0FF);
  clLawnGreen = TColor($00FC7C);
  clLemonChiffon = TColor($CDFAFF);
  clLightBlue = TColor($E6D8AD);
  clLightCoral = TColor($8080F0);
  clLightCyan = TColor($FFFFE0);
  clLightGoldenrodYellow = TColor($D2FAFA);
  clLightGreen = TColor($90EE90);
  clLightGray = TColor($D3D3D3);
  clLightPink = TColor($C1B6FF);
  clLightSalmon = TColor($7AA0FF);
  clLightSeaGreen = TColor($AAB220);
  clLightSkyBlue = TColor($FACE87);
  clLightSlateGray = TColor($998877);
  clLightSteelBlue = TColor($DEC4B0);
  clLightYellow = TColor($E0FFFF);
  clLimeGreen = TColor($32CD32);
  clLinen = TColor($E6F0FA);
  clMagenta = TColor($FF00FF);
  clMediumAquamarine = TColor($AACD66);
  clMediumBlue = TColor($CD0000);
  clMediumOrchid = TColor($D355BA);
  clMediumPurple = TColor($DB7093);
  clMediumSeaGreen = TColor($71B33C);
  clMediumSlateBlue = TColor($EE687B);
  clMediumSpringGreen = TColor($9AFA00);
  clMediumTurquoise = TColor($CCD148);
  clMediumVioletRed = TColor($8515C7);
  clMidnightBlue = TColor($701919);
  clMintCream = TColor($FAFFF5);
  clMistyRose = TColor($E1E4FF);
  clMoccasin = TColor($B5E4FF);
  clNavajoWhite = TColor($ADDEFF);
  clOldLace = TColor($E6F5FD);
  clOliveDrab = TColor($238E6B);
  clOrange = TColor($00A5FF);
  clOrangeRed = TColor($0045FF);
  clOrchid = TColor($D670DA);
  clPaleGoldenrod = TColor($AAE8EE);
  clPaleGreen = TColor($98FB98);
  clPaleTurquoise = TColor($EEEEAF);
  clPaleVioletRed = TColor($9370DB);
  clPapayaWhip = TColor($D5EFFF);
  clPeachPuff = TColor($B9DAFF);
  clPeru = TColor($3F85CD);
  clPink = TColor($CBC0FF);
  clPlum = TColor($DDA0DD);
  clPowderBlue = TColor($E6E0B0);
  clRosyBrown = TColor($8F8FBC);
  clRoyalBlue = TColor($E16941);
  clSaddleBrown = TColor($13458B);
  clSalmon = TColor($7280FA);
  clSandyBrown = TColor($60A4F4);
  clSeaGreen = TColor($578B2E);
  clSeaShell = TColor($EEF5FF);
  clSienna = TColor($2D52A0);
  clSkyBlue = TColor($EBCE87);
  clSlateBlue = TColor($CD5A6A);
  clSlateGray = TColor($908070);
  clSnow = TColor($FAFAFF);
  clSpringGreen = TColor($7FFF00);
  clSteelBlue = TColor($B48246);
  clTan = TColor($8CB4D2);
  clThistle = TColor($D8BFD8);
  clTomato = TColor($4763FF);
  clTurquoise = TColor($D0E040);
  clViolet = TColor($EE82EE);
  clWheat = TColor($B3DEF5);
  clWhiteSmoke = TColor($F5F5F5);
  clYellowGreen = TColor($32CD9A);

{ Color routines }

function StrToColor(S: string): TColorB;
function ColorToStr(C: TColorB): string;
procedure RegisterColorName(Color: TColorB; Name: string);

function HueToColor(H: Float): TColorB;
function ColorToHue(C: TColorB): Float;

function Hue(H: Float): TColorB;
function HueInvert(H: Float): TColorB;
function Blend(A, B: TColorB; Percent: Float): TColorB;
function Fade(Color: TColorB; Percent: Float): TColorB;
function Darken(Color: TColorB; Percent: Float): TColorB;
function Lighten(Color: TColorB; Percent: Float): TColorB;
function Rgba(R, G, B: Byte; A: Float): TColorB; overload;
function Rgba(Color: TColor; A: Float): TColorB; overload;

{ Miscellaneous routines }

function Divide(const Quotient, Divisor: Extended): Extended;
function Remainder(const Quotient, Divisor: Extended): Extended;
function Clamp(Percent: Float): Float;
function DegToRad(D: Float): Float;
function RadToDeg(R: Float): Float;

{doc off}
type
  TMatrix = TMatrix2x3;
  PMatrix = PMatrix2x3;

{ Forward types }

type
  IMatrix = interface;
  IPen = interface;
  IBrush = interface;
  ISolidBrush = interface;
  IGradientBrush = interface;
  IFont = interface;
  IPathData = interface;
  IPath = interface;
  ISurface = interface;
  IBitmap = interface;

{ IMatrix }

  IMatrix = interface(ICloneable<IMatrix>)
  ['{2918AB0D-E288-4E5C-8FDE-67776EC7CFAD}']
    procedure Identity;
    procedure Multiply(M: IMatrix);
    procedure Translate(X, Y: Float);
    procedure Scale(X, Y: Float);
    procedure Rotate(Radians: Float);
    function Transform(Point: TPointF): TPointF;
  end;

{ IPen }

  TLinePattern = (pnSolid, pnDash, pnDot, pnDashDot);
  TLineCap = (cpButt, cpRound, cpSquare);
  TLineJoin = (jnMiter, jnRound, jnBevel);

  IPen = interface
  ['{F702E75F-684F-4D02-9A51-682A154DD9D5}']
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

{ IBrush }

  IBrush = interface
  ['{77B25395-F891-4526-A941-77C200C3F08F}']
    function GetMatrix: IMatrix;
    procedure SetMatrix(Value: IMatrix);
    function GetOpacity: Byte;
    procedure SetOpacity(Value: Byte);
    property Matrix: IMatrix read GetMatrix write SetMatrix;
    property Opacity: Byte read GetOpacity write SetOpacity;
  end;

{ ISolidBrush }

  ISolidBrush = interface(IBrush)
  ['{8F520DF0-C9F7-4C5E-8954-9A62179BB84F}']
    function GetColor: TColorB;
    procedure SetColor(Value: TColorB);
    property Color: TColorB read GetColor write SetColor;
  end;

{ IBitmapBrush }

  IBitmapBrush = interface(IBrush)
  ['{3199CC5B-5B41-4E91-BDD0-17FAB1E91ABA}']
  end;

{ IGradientBrush }

  TGradientWrap = (gwClamp, gwRepeat, gwReflect);

  IGradientBrush = interface(IBrush)
  ['{B3870AD1-4A48-4A0A-AC39-13DD43501E63}']
    function GetWrap: TGradientWrap;
    procedure SetWrap(Value: TGradientWrap);
    procedure AddStop(Color: TColorB; Offset: Float);
    property Wrap: TGradientWrap read GetWrap write SetWrap;
  end;

{ ILinearGradientBrush }

  ILinearGradientBrush = interface(IGradientBrush)
  ['{EBEC24AC-6BE7-44D1-BA4F-F11B8A206DEA}']
  end;

{ IRadialGradientBrush }

  IRadialGradientBrush = interface(IGradientBrush)
  ['{A8B230B8-4CD4-4C51-878A-D1448077F0C3}']
  end;

{ IFont }

  IFont = interface
  ['{9ACA722A-6DDE-4C15-89E0-47DD1B86B981}']
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

{ IPathData can be obtained by cloning a path }

  IPathData = interface
  ['{C2887F8D-D729-427D-90C2-F9B081697CAB}']
  end;

{ IPath is created by surface drawing commands. Path data can be obtain by
  cloning this object. }

  IPath = interface(ICloneable<IPathData>)
  ['{E2E7EC9A-74BA-4614-8809-7D828E30DD51}']
    { Leave the current path opened and add a new sub path }
    procedure Add;
    { Remove the current path and make the previous sub path current }
    procedure Remove;
    { Close the current path and begin a new empty sub path }
    procedure Close;
    { Join previous path data to the end of the sub paths }
    procedure Join(Path: IPathData);
    { Clip all rendering by the path and remove all path data }
    procedure Clip;
    { Remove any existing clipping paths }
    procedure Unclip;
  end;

{ ISurface is the main interface used to draw high quality fast vector graphics }

  TResampleQuality = (rqLowest, rqNormal, rqBest);

  ISurface = interface
  ['{6C23D3BC-6D74-4EDD-B0B9-EB55BF655E80}']
    function GetMatrix: IMatrix;
    procedure SetMatrix(Value: IMatrix);
    function GetPath: IPath;
    function GetHandle: Pointer;
    { Wait for drawing operations to complete }
    procedure Flush;
    { Fill the entire surface with a color }
    procedure Clear(Color: TColorB);
    { Copy an area from one surface to another optionally alpha blending
      the result }
    procedure CopyTo(const Source: TRectF; Surface: ISurface; const Dest: TRectF;
      Alpha: Byte = $FF; Quality: TResampleQuality = rqNormal);
    { Push the current path, matrix, and clipping area onto a stack }
    procedure Save;
    { Pop the path, matrix, and clipping area from a stack }
    procedure Restore;
    { Move to a new position creating a new sub path }
    procedure MoveTo(X, Y: Float);
    { Line to a new position adding to the current path }
    procedure LineTo(X, Y: Float);
    { Arc to a new position adding to the current path }
    procedure ArcTo(const Rect: TRectF; BeginAngle, EndAngle: Float);
    { Bezier curve to a new position adding to the current path }
    procedure CurveTo(X, Y: Float; const C1, C2: TPointF);
    { Ellipse as a new sub path }
    procedure Ellipse(const Rect: TRectF);
    { Rectangle as a new sub path }
    procedure Rectangle(const Rect: TRectF);
    { Round rectangle as a new sub path }
    procedure RoundRectangle(const Rect: TRectF; Radius: Float);
    { TextSize returns the number of pixels needed to fit a single
      line of text }
    function TextSize(Font: IFont; const Text: string): TPointF;
    { TextHeight returns the number of vertical pixels needed to
      fit text given a wrappable width }
    function TextHeight(Font: IFont; const Text: string; Width: Float): Float;
    { TextOut writes text. When immediate is true no path is created. }
    procedure TextOut(Font: IFont; const Text: string; const Rect: TRectF;
      Direction: TDirection; Immediate: Boolean = True);
    { Stroke the current path optionally preserving it }
    procedure Stroke(Pen: IPen; Preserve: Boolean = False);
    { Fill the current path optionally preserving it }
    procedure Fill(Brush: IBrush; Preserve: Boolean = False);
    { Stroke a rect aligning its pixels to the pen width  }
    procedure StrokeRect(Pen: IPen; const Rect: TRectF);
    { Fill a rect aligning its pixels evenly within the rect }
    procedure FillRect(Brush: IBrush; const Rect: TRectF);
    { Stroke a round rect aligning its pixels to the pen width  }
    procedure StrokeRoundRect(Pen: IPen; const Rect: TRectF; Radius: Float);
    { Fill a round rect aligning its pixels evenly within the rect }
    procedure FillRoundRect(Brush: IBrush; const Rect: TRectF; Radius: Float);
    { Matrix is used to rotate, scale, and translate rendering }
    property Matrix: IMatrix read GetMatrix write SetMatrix;
    { The current path which can be stroked or filled }
    property Path: IPath read GetPath;
    { Handle }
    property Handle: Pointer read GetHandle;
  end;

{ IBitmap can load and save images as well as allow ISurface drawing in memory }

  TImageFormat = (fmPng, fmJpeg, fmGif, fmBmp, fmIco, fmTiff);

  IBitmap = interface(ICloneable<IBitmap>)
  ['{DB935633-A218-4181-96A2-B0808697150F}']
    function GetEmpty: Boolean;
    function GetSurface: ISurface;
    function GetClientRect: TRectI;
    function GetFormat: TImageFormat;
    procedure SetFormat(Value: TImageFormat);
    function GetHeight: Integer;
    function GetWidth: Integer;
    function GetPixels: PPixel;
    procedure Clear;
    function Resample(Width, Height: Integer; Quality: TResampleQuality = rqNormal): IBitmap;
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(Stream: TStream);
    procedure SetSize(Width, Height: Integer);
    property Empty: Boolean read GetEmpty;
    property Surface: ISurface read GetSurface;
    property ClientRect: TRectI read GetClientRect;
    property Format: TImageFormat read GetFormat write SetFormat;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property Pixels: PPixel read GetPixels;
  end;

{ ISplash is a floating window whose shape is defined by a bitmap }

  ISplash = interface
  ['{291570E9-3567-4C10-8899-CDA04979060F}']
    function GetBitmap: IBitmap;
    function GetOpacity: Byte;
    procedure SetOpacity(Value: Byte);
    function GetVisible: Boolean;
    procedure SetVisible(Value: Boolean);
    function GetHandle: IntPtr;
    { Move the window to x and y }
    procedure Move(X, Y: Integer);
    { Update the window when you're done drawing on bitmap }
    procedure Update;
    { Bitmap is the image surface which defines the window shape }
    property Bitmap: IBitmap read GetBitmap;
    { Opacity controls the overall transparency of the window }
    property Opacity: Byte read GetOpacity write SetOpacity;
    { Visible shows or hide the window }
    property Visible: Boolean read GetVisible write SetVisible;
    { Handle is the udnerlying operating system window handle }
    property Handle: IntPtr read GetHandle;
  end;

const
  PenMiterLimitDefault = 10;

function StrToImageFormat(S: string): TImageFormat;
function ImageFormatToStr(F: TImageFormat): string;
function ImageFormatToMimeType(F: TImageFormat): string;

type
  TSurfaceOptions = record
    { Use hardware rendering when possbile }
    HardwareRendering: Boolean;
    { Use double buffering if hardware rendering is not supported }
    SoftwareBuffering: Boolean;
    { Correct small render errors with possible degraded performance or loss of
      features. Enabling this causes text to fill the current path immediately,
      which in Windows has the effect of nicer text when unscaled or rotated. }
    ErrorCorrection: Boolean;
    { Use gamma corrected gradients on supported back ends }
    GammaCorrection: Boolean;
    { Use premultiplication when loading or saving images }
    UsePremultiply: Boolean;
  end;

var
  SurfaceOptions: TSurfaceOptions = (
    HardwareRendering: True;
    SoftwareBuffering: False;
    ErrorCorrection: False;
    GammaCorrection: False;
    UsePremultiply: True;
  );

implementation

const
  HiByte = High(Byte);

{ TPointI }

class operator TPointI.Implicit(const Value: TPointI): TPoint;
var
  R: TPoint absolute Value;
begin
  Result := R;
end;

class operator TPointI.Implicit(const Value: TPoint): TPointI;
var
  R: TPointI absolute Value;
begin
  Result := R;
end;

class operator TPointI.Negative(const A: TPointI): TPointI;
begin
  Result.X := -A.X;
  Result.Y := -A.Y;
end;

class operator TPointI.Equal(const A, B: TPointI): Boolean;
begin
  Result := A.Equals(B);
end;

class operator TPointI.NotEqual(const A, B: TPointI): Boolean;
begin
  Result := not A.Equals(B);
end;

class operator TPointI.Add(const A, B: TPointI): TPointI;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
end;

class operator TPointI.Subtract(const A, B: TPointI): TPointI;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
end;

class function TPointI.Create: TPointI;
begin
  Result.X := 0;
  Result.Y := 0;
end;

class function TPointI.Create(X, Y: Integer): TPointI;
begin
  Result.X := X;
  Result.Y := Y;
end;

function TPointI.Equals(const Value: TPointI): Boolean;
begin
  Result := (X = Value.X) and (Y = Value.Y);
end;

function TPointI.Angle(const P: TPointI): Float;
begin
  Result := TPointF(Self).Angle(P);
end;

function TPointI.Dist(const P: TPointI): Float;
begin
  Result := TPointF(Self).Dist(P);
end;

function TPointI.Mid(const P: TPointI): TPointI;
begin
  Result := TPointI(TPointF(Self).Mid(P));
end;

procedure TPointI.Offset(X, Y: Integer);
begin
  Inc(Self.X, X);
  Inc(Self.Y, Y);
end;

procedure TPointI.Offset(const P: TPointI);
begin
  Inc(X, P.X);
  Inc(Y, P.Y);
end;

function TPointI.Move(X, Y: Integer): TPointI;
begin
  Result.X := Self.X + X;
  Result.Y := Self.Y + Y;
end;

function TPointI.Move(const P: TPointI): TPointI;
begin
  Result.X := X + P.X;
  Result.Y := Y + P.Y;
end;

{ TRectI }

class operator TRectI.Implicit(const Value: TRectI): TRect;
begin
  Result.Left := Value.X;
  Result.Top := Value.Y;
  Result.Right := Value.X + Value.Width;
  Result.Bottom := Value.Y + Value.Height;
end;

class operator TRectI.Implicit(const Value: TRect): TRectI;
begin
  Result.X := Value.Left;
  Result.Y := Value.Top;
  Result.Width := Value.Right - Value.Left;
  Result.Height := Value.Bottom - Value.Top;
end;

class operator TRectI.Equal(const A, B: TRectI): Boolean;
begin
  Result := A.Equals(B);
end;

class operator TRectI.NotEqual(const A, B: TRectI): Boolean;
begin
  Result := not A.Equals(B);
end;

class function TRectI.Create: TRectI;
begin
  Result.X := 0;
  Result.Y := 0;
  Result.Width := 0;
  Result.Height := 0;
end;

class function TRectI.Create(Size: TPointI): TRectI;
begin
  Result.X := 0;
  Result.Y := 0;
  Result.Width := Size.X;
  Result.Height := Size.Y;
end;

class function TRectI.Create(W, H: Integer): TRectI;
begin
  Result.X := 0;
  Result.Y := 0;
  Result.Width := W;
  Result.Height := H;
end;

class function TRectI.Create(X, Y, W, H: Integer): TRectI;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Width := W;
  Result.Height := H;
end;

function TRectI.Equals(const Value: TRectI): Boolean;
begin
  Result := (X = Value.X) and (Y = Value.Y) and
    (Width = Value.Width) and (Width = Value.Width);
end;

function TRectI.Contains(X, Y: Integer): Boolean;
begin
  Result := Contains(TPointI.Create(X, Y));
end;

function TRectI.Contains(const P: TPointI): Boolean;
begin
  if Empty then
    Exit(False);
  Result := (X <= P.X) and (X + Width > P.X) and
    (Y <= P.Y) and (Y + Height > P.Y);
end;

procedure TRectI.Center(X, Y: Integer);
begin
  Self.X := X - Width shr 1;
  Self.Y := Y - Height shr 1;
end;

procedure TRectI.Center(const P: TPointI);
begin
  X := P.X - Width shr 1;
  Y := P.Y - Height shr 1;
end;

procedure TRectI.Inflate(X, Y: Integer);
begin
  Dec(Self.X, X);
  Inc(Width, X shl 1);
  Dec(Self.Y, Y);
  Inc(Height, Y shl 1);
end;

procedure TRectI.Inflate(const P: TPointI);
begin
  Dec(X, P.X);
  Inc(Width, P.X shl 1);
  Dec(Y, P.Y);
  Inc(Height, P.Y shl 1);
end;

procedure TRectI.Offset(X, Y: Integer);
begin
  Inc(Self.X, X);
  Inc(Self.Y, Y);
end;

procedure TRectI.Offset(const P: TPointI);
begin
  Inc(X, P.X);
  Inc(Y, P.Y);
end;

function TRectI.GetEmpty: Boolean;
begin
  Result := (Width < 1) or (Height < 1);
end;

procedure TRectI.SetLeft(Value: Integer);
var
  I: Integer;
begin
  I := X + Width;
  X := Value;
  Width := I - X;
end;

procedure TRectI.SetTop(Value: Integer);
var
  I: Integer;
begin
  I := Y + Height;
  Y := Value;
  Height := I - Y;
end;

function TRectI.GetRight: Integer;
begin
  Result := X + Width;
end;

procedure TRectI.SetRight(Value: Integer);
begin
  Width := Value - X;
end;

function TRectI.GetBottom: Integer;
begin
  Result := Y + Height;
end;

procedure TRectI.SetBottom(Value: Integer);
begin
  Height := Value - Y;
end;

function TRectI.GetSize: TPointI;
begin
  Result := TPointI.Create(Width, Height);
end;

function TRectI.GetTopLeft: TPointI;
begin
  Result := TPointI.Create(X, Y);
end;

function TRectI.GetBottomLeft: TPointI;
begin
  Result := TPointI.Create(X, Y + Height);
end;

function TRectI.GetBottomRight: TPointI;
begin
  Result := TPointI.Create(X + Width, Y + Height);
end;

function TRectI.GetTopRight: TPointI;
begin
  Result := TPointI.Create(X + Width, Y);
end;

function TRectI.GetMidPoint: TPointI;
begin
  Result := TPointI.Create(X + Width div 2, Y + Height div 2);
end;

{ TPointF }

class operator TPointF.Implicit(const Value: TPointI): TPointF;
begin
  Result.X := Value.X;
  Result.Y := Value.Y;
end;

class operator TPointF.Implicit(const Value: TPoint): TPointF;
begin
  Result.X := Value.X;
  Result.Y := Value.Y;
end;

class operator TPointF.Explicit(const Value: TPointF): TPointI;
begin
  Result.X := Round(Value.X);
  Result.Y := Round(Value.Y);
end;

class operator TPointF.Explicit(const Value: TPointF): TPoint;
begin
  Result.X := Round(Value.X);
  Result.Y := Round(Value.Y);
end;

class operator TPointF.Negative(const A: TPointF): TPointF;
begin
  Result.X := -A.X;
  Result.Y := -A.Y;
end;

class operator TPointF.Equal(const A, B: TPointF): Boolean;
begin
  Result := A.Equals(B);
end;

class operator TPointF.NotEqual(const A, B: TPointF): Boolean;
begin
  Result := not A.Equals(B);
end;

class operator TPointF.Add(const A, B: TPointF): TPointF;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
end;

class operator TPointF.Subtract(const A, B: TPointF): TPointF;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
end;

class operator TPointF.Multiply(const A: TPointF; B: Float): TPointF;
begin
  Result.X := A.X * B;
  Result.Y := A.Y * B;
end;

class function TPointF.Create: TPointF;
begin
  Result.X := 0;
  Result.Y := 0;
end;

class function TPointF.Create(X, Y: Float): TPointF;
begin
  Result.X := X;
  Result.Y := Y;
end;

function TPointF.Equals(const Value: TPointF): Boolean;
begin
  Result := (X = Value.X) and (Y = Value.Y);
end;

procedure TPointF.Offset(X, Y: Float);
begin
  Self.X :=  Self.X + X;
  Self.Y :=  Self.Y + Y;
end;

procedure TPointF.Offset(const P: TPointF);
begin
  X :=  X + P.X;
  Y :=  Y + P.Y;
end;

function TPointF.Move(X, Y: Float): TPointF;
begin
  Result.X := Self.X + X;
  Result.Y := Self.Y + Y;
end;

function TPointF.Move(const P: TPointF): TPointF;
begin
  Result.X := X + P.X;
  Result.Y := Y + P.Y;
end;

function TPointF.Angle(const P: TPointF): Float;
var
  X, Y: Float;
begin
  X := Self.X - P.X;
  Y := Self.Y - P.Y;
  if X = 0 then
    if Y < 0 then
      Exit(Pi)
    else
      Exit(0);
  Result := Arctan(Y / X) + Pi / 2;
  if X > 0 then
    Result := Result + Pi;
end;

function TPointF.Dist(const P: TPointF): Float;
var
  X, Y: Float;
begin
  X := Self.X - P.X;
  Y := Self.Y - P.Y;
  Result := Sqrt(X * X + Y * Y);
end;

function TPointF.Mid(const P: TPointF): TPointF;
begin
  Result.X := (X + P.X) / 2;
  Result.Y := (Y + P.Y) / 2;
end;

function TPointF.Extend(const P: TPointF; Dist: Float): TPointF;
var
  X, Y, R: Float;
begin
  X := Self.X - P.X;
  Y := Self.Y - P.Y;
  R := Sqrt(X * X + Y * Y);
  if R = 0 then
    Exit(Self);
  R := 1 / R;
  Result.X := Self.X - X * R * Dist;
  Result.Y := Self.Y - Y * R * Dist;
end;

function TPointF.Rotate(const P: TPointF; Angle: Float): TPointF;
var
  S, C: Float;
  X, Y: Float;
begin
  SinCos(Angle, S, C);
  X := Self.Y * S - Self.X * C + Self.X;
  Y := -Self.X * S - Self.Y * C + Self.Y;
  Result.X := P.X * C - P.Y * S + X;
  Result.Y := P.X * S + P.Y * C + Y;
end;

{ TRectF }

class operator TRectF.Implicit(const Value: TRectI): TRectF;
begin
  Result.X := Value.X;
  Result.Y := Value.Y;
  Result.Width := Value.Width;
  Result.Height := Value.Height;
end;

class operator TRectF.Implicit(const Value: TRect): TRectF;
begin
  Result.X := Value.Left;
  Result.Y := Value.Top;
  Result.Width := Value.Right - Value.Left;
  Result.Height := Value.Bottom - Value.Top;
end;

class operator TRectF.Explicit(const Value: TRectF): TRectI;
begin
  Result.X := Round(Value.X);
  Result.Y := Round(Value.Y);
  Result.Width := Round(Value.Width);
  Result.Height := Round(Value.Height);
end;

class operator TRectF.Explicit(const Value: TRectF): TRect;
begin
  Result.Left := Round(Value.X);
  Result.Top := Round(Value.Y);
  Result.Right := Result.Left + Round(Value.Width);
  Result.Bottom := Result.Top + Round(Value.Height);
end;

class operator TRectF.Equal(const A, B: TRectF): Boolean;
begin
  Result := A.Equals(B);
end;

class operator TRectF.NotEqual(const A, B: TRectF): Boolean;
begin
  Result := not A.Equals(B);
end;

class operator TRectF.Multiply(const A: TRectF; B: Float): TRectF;
begin
  Result.X := A.X * B;
  Result.Y := A.Y * B;
  Result.Width := A.Width * B;
  Result.Height := A.Height * B;
end;

class function TRectF.Create: TRectF;
begin
  Result.X := 0;
  Result.Y := 0;
  Result.Width := 0;
  Result.Height := 0;
end;

class function TRectF.Create(Size: TPointF): TRectF;
begin
  Result.X := 0;
  Result.Y := 0;
  Result.Width := Size.X;
  Result.Height := Size.Y;
end;

class function TRectF.Create(W, H: Float): TRectF;
begin
  Result.X := 0;
  Result.Y := 0;
  Result.Width := W;
  Result.Height := H;
end;

class function TRectF.Create(X, Y, W, H: Float): TRectF;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Width := W;
  Result.Height := H;
end;

function TRectF.Equals(const Value: TRectF): Boolean;
begin
  Result := (X = Value.X) and (Y = Value.Y) and
    (Width = Value.Width) and (Width = Value.Width);
end;

function TRectF.Contains(X, Y: Float): Boolean;
begin
  Result := Contains(TPointF.Create(X, Y));
end;

function TRectF.Contains(const P: TPointF): Boolean;
begin
  if Empty then
    Exit(False);
  Result := (X <= P.X) and (X + Width > P.X) and
    (Y <= P.Y) and (Y + Height > P.Y);
end;

procedure TRectF.Center(X, Y: Float);
begin
  Self.X := X - Width / 2;
  Self.Y := Y - Height / 2;
end;

procedure TRectF.Center(const P: TPointF);
begin
  X := P.X - Width / 2;
  Y := P.Y - Height / 2;
end;

procedure TRectF.Center(const R: TRectF);
begin
  Center(R.MidPoint)
end;

procedure TRectF.Inflate(X, Y: Float);
begin
  Self.X := Self.X - X;
  Self.Y := Self.Y - Y;
  Width := Width + X * 2;
  Height := Height + Y * 2;
end;

procedure TRectF.Inflate(const P: TPointF);
begin
  X := X - P.X;
  Y := Y - P.Y;
  Width := Width + P.X * 2;
  Height := Height + P.Y * 2;
end;

procedure TRectF.Offset(X, Y: Float);
begin
  Self.X := Self.X + X;
  Self.Y := Self.Y + Y;
end;

procedure TRectF.Offset(const P: TPointF);
begin
  X := X + P.X;
  Y := Y + P.Y;
end;

function TRectF.GetEmpty: Boolean;
begin
  Result := (Width < 1) or (Height < 1);
end;

procedure TRectF.SetLeft(Value: Float);
var
  I: Float;
begin
  I := X + Width;
  X := Value;
  Width := I - X;
end;

procedure TRectF.SetTop(Value: Float);
var
  I: Float;
begin
  I := Y + Height;
  Y := Value;
  Height := I - Y;
end;

function TRectF.GetRight: Float;
begin
  Result := X + Width;
end;

procedure TRectF.SetRight(Value: Float);
begin
  Width := Value - X;
end;

function TRectF.GetBottom: Float;
begin
  Result := Y + Height;
end;

procedure TRectF.SetBottom(Value: Float);
begin
  Height := Value - Y;
end;

function TRectF.GetSize: TPointF;
begin
  Result := TPointF.Create(Width, Height);
end;

function TRectF.GetTopLeft: TPointF;
begin
  Result := TPointF.Create(X, Y);
end;

function TRectF.GetTopRight: TPointF;
begin
  Result := TPointF.Create(X + Width, Y);
end;

function TRectF.GetBottomLeft: TPointF;
begin
  Result := TPointF.Create(X, Y + Height);
end;

function TRectF.GetBottomRight: TPointF;
begin
  Result := TPointF.Create(X + Width, Y + Height);
end;

function TRectF.GetMidPoint: TPointF;
begin
  Result := TPointF.Create(X + Width / 2, Y + Height / 2);
end;

function TRectF.GetMidLeft: TPointF;
begin
  Result := TPointF.Create(X, Y + Height / 2);
end;

function TRectF.GetMidTop: TPointF;
begin
  Result := TPointF.Create(X + Width / 2, Y);
end;

function TRectF.GetMidRight: TPointF;
begin
  Result := TPointF.Create(X + Width, Y + Height / 2);
end;

function TRectF.GetMidBottom: TPointF;
begin
  Result := TPointF.Create(X + Width / 2, Y + Height);
end;

{ TMatrix2x3 }

class operator TMatrix2x3.Equal(const A, B: TMatrix2x3): Boolean;
begin
  Result := CompareMem(@A, @B, SizeOf(TMatrix2x3));
end;

class operator TMatrix2x3.Negative(const A: TMatrix2x3): TMatrix2x3;
var
  F: Float;
begin
  if (A.M21 = 0) and (A.M12 = 0) then
  begin
    Result := A;
    Result.M31 := -Result.M31;
    Result.M32 := -Result.M32;
    if Result.M11 <> 1 then
    begin
      if Result.M11 = 0 then
        Exit(A);
      Result.M11 := 1 / Result.M11;
      Result.M31 := Result.M31 * Result.M11;
    end;
    if Result.M22 <> 1 then
    begin
      if Result.M22 = 0 then
        Exit(A);
      Result.M22 := 1 / Result.M22;
      Result.M32 := Result.M32 * Result.M22;
    end;
    Exit;
  end;
  F := A.M11 * A.M22 - A.M12 * A.M21;
  if F * F <= 0 then
    Exit(A);
  F := 1 / F;
  Result.M11 := A.M22 * F;
  Result.M12 := -A.M12 * F;
  Result.M21 := -A.M21 * F;
  Result.M22 := A.M11 * F;
  Result.M31 := (A.M21 * A.M32 - A.M22 * A.M31) * F;
  Result.M32 := (A.M12 * A.M31 - A.M11 * A.M32) * F;
  Result.Order := A.Order;
end;

class operator TMatrix2x3.Multiply(const A, B: TMatrix2x3): TMatrix2x3;
begin
  Result.M11 := A.M11 * B.M11 + A.M12 * B.M21;
  Result.M12 := A.M11 * B.M12 + A.M12 * B.M22;
  Result.M21 := A.M21 * B.M11 + A.M22 * B.M21;
  Result.M22 := A.M21 * B.M12 + A.M22 * B.M22;
  Result.M31 := A.M31 * B.M11 + A.M32 * B.M21 + B.M31;
  Result.M32 := A.M31 * B.M12 + A.M32 * B.M22 + B.M32;
  Result.Order := A.Order;
end;

class operator TMatrix2x3.Multiply(const A: TMatrix2x3; const B: TPointF): TPointF;
begin
  Result.X := A.M11 * B.X + A.M21 * B.Y + A.M31;
  Result.Y := A.M12 * B.X + A.M22 * B.Y + A.M32;
end;

class function TMatrix2x3.Create: TMatrix2x3;
const
  M: TMatrix2x3 = (M11: 1; M12: 0; M21: 0; M22: 1; M31: 0; M32: 0;
    Order: moPrepend);
begin
  Result := M;
end;

function TMatrix2x3.CanInvert: Boolean;
var
  F: Float;
begin
  if (M21 = 0) and (M12 = 0) then
    Exit((M11 <> 0) and (M22 <> 0));
  F := M11 * M22 - M12 * M21;
  Result := F * F > 0;
end;

procedure TMatrix2x3.Identity;
const
  M: TMatrix2x3 = (M11: 1; M12: 0; M21: 0; M22: 1; M31: 0; M32: 0);
var
  O: TMatrixOrder;
begin
  O := Order;;
  Self := M;
  Self.Order := O;
end;

procedure TMatrix2x3.Invert;
begin
  Self := -Self;
end;

procedure TMatrix2x3.Rotate(Angle: Float);
var
  S, C: Float;
var
  M: TMatrix2x3;
  O: TMatrixOrder;
begin
  SinCos(Angle, S, C);
  M.Identity;
  M.M11 := C;
  M.M12 := S;
  M.M21 := -S;
  M.M22 := C;
  O := Order;
  if Order = moPrepend then
    Self := M * Self
  else
    Self := Self * M;
  Order  := O;
end;

procedure TMatrix2x3.RotateAt(Angle: Float; X, Y: Float);
begin
  if Order = moPrepend then
  begin
    Translate(X, Y);
    Rotate(Angle);
    Translate(-X, -Y);
  end
  else
  begin
    Translate(-X, -Y);
    Rotate(Angle);
    Translate(X, Y);
  end;
end;

procedure TMatrix2x3.RotateAt(Angle: Float; const P: TPointF);
begin
  RotateAt(Angle, P.X, P.Y);
end;

procedure TMatrix2x3.Scale(SX, SY: Float);
var
  M: TMatrix2x3;
  O: TMatrixOrder;
begin
  M.Identity;
  M.M11 := SX;
  M.M22 := SY;
  O := Order;
  if Order = moPrepend then
    Self := M * Self
  else
    Self := Self * M;
  Order := O;
end;

procedure TMatrix2x3.ScaleAt(SX, SY, X, Y: Float);
begin
  if Order = moPrepend then
  begin
    Translate(X, Y);
    Scale(SX, SY);
    Translate(-X, -Y);
  end
  else
  begin
    Translate(-X, -Y);
    Scale(SX, SY);
    Translate(X, Y);
  end;
end;

procedure TMatrix2x3.ScaleAt(SX, SY: Float; const P: TPointF);
begin
  ScaleAt(SX, SY, P.X, P.Y);
end;

procedure TMatrix2x3.Skew(AngleX, AngleY: Float);
var
  M: TMatrix2x3;
  O: TMatrixOrder;
begin
  M.Identity;
  M.M12 := Tan(AngleY);
  M.M21 := Tan(AngleX);
  O := Order;
  if Order = moPrepend then
    Self := M * Self
  else
    Self := Self * M;
  Order := O;
end;

procedure TMatrix2x3.Translate(X, Y: Float);
var
  M: TMatrix2x3;
  O: TMatrixOrder;
begin
  M.Identity;
  M.M31 := X;
  M.M32 := Y;
  O := Order;
  if Order = moPrepend then
    Self := M * Self
  else
    Self := Self * M;
  Order := O;
end;

procedure TMatrix2x3.Translate(const P: TPointF);
begin
  Translate(P.X, P.Y);
end;

function TMatrix2x3.Multiply(const M: TMatrix2x3): TMatrix2x3;
begin
  Result := Self * M;
end;

function TMatrix2x3.Multiply(const P: TPointF): TPointF;
begin
  Result := Self * P;
end;

{ TColorAlphaHelper }

function TColorAlphaHelper.GetAlpha: Byte;
begin
  Result := PColorB(@Self).Alpha;
end;

function TColorAlphaHelper.GetBlue: Byte;
begin
  Result := PColorB(@Self).Blue;
end;

function TColorAlphaHelper.GetGreen: Byte;
begin
  Result := PColorB(@Self).Green;
end;

function TColorAlphaHelper.GetRed: Byte;
begin
  Result := PColorB(@Self).Red;
end;

procedure TColorAlphaHelper.SetAlpha(Value: Byte);
begin
  PColorB(@Self).Alpha := Value;
end;

procedure TColorAlphaHelper.SetBlue(Value: Byte);
begin
  PColorB(@Self).Blue := Value;
end;

procedure TColorAlphaHelper.SetGreen(Value: Byte);
begin
  PColorB(@Self).Green := Value;
end;

procedure TColorAlphaHelper.SetRed(Value: Byte);
begin
  PColorB(@Self).Red := Value;
end;

{ THSL }

class function THSL.Create(H, S, L: Float): THSL;
begin
  Result.Hue := H;
  Result.Saturation := S;
  Result.Lightness := L;
end;

class operator THSL.Implicit(Value: Float): THSL;
begin
  Result.Hue := Remainder(Value, 1);
  if Result.Hue < 0 then
    Result.Hue := 1 - Result.Hue;
  Result.Lightness := 0.5;
  Result.Saturation := 1;
end;

class operator THSL.Implicit(const Value: THSL): Float;
begin
  Result := Value.Hue;
end;

{ TColorB }

type
  TRGBAShort = packed record
    R, G, B, A: Byte;
  end;

class operator TColorB.Implicit(Value: TColorB): TColorAlpha;
var
  B: TColorB absolute Result;
begin
  B.Blue := Value.Blue;
  B.Green := Value.Green;
  B.Red := Value.Red;
  B.Alpha := Value.Alpha;
end;

class operator TColorB.Implicit(Value: TColorAlpha): TColorB;
begin
  Result.Blue := Value.Blue;
  Result.Green := Value.Green;
  Result.Red := Value.Red;
  Result.Alpha := Value.Alpha;
end;

class operator TColorB.Implicit(const Value: THSL): TColorB;
const
  OneOverThree = 1 / 3;
var
  M1, M2: Float;

  function HueToValue(Hue: Float) : Float;
  begin
    if Hue < 0 then
      Hue := Hue + 1
    else if Hue > 1 then
      Hue := Hue - 1;
    if 6 * Hue < 1 then
      Result := M1 + (M2 - M1) * Hue * 6
    else if 2 * Hue < 1 then
      Result := M2
    else if 3 * Hue < 2 then
      Result := M1 + (M2 - M1) * (2 / 3 - Hue) * 6
    else
    Result := M1;
  end;

var
  H, S, L, R, G, B: Float;
begin
  H := Value.Hue;
  if H < 0 then
    H := H + Ceil(H)
  else if H > 1 then
    H := H - Floor(H);
  S := Clamp(Value.Saturation);
  L := Clamp(Value.Lightness);
  if S = 0 then
  begin
    R := L;
    G := L;
    B := L
  end
  else
  begin
    if L <= 0.5 then
      M2 := L * (1 + S)
    else
      M2 := L + S - L * S;
    M1 := 2 * L - M2;
    R := HueToValue(H + OneOverThree);
    G := HueToValue(H);
    B := HueToValue(H - OneOverThree)
  end;
  Result.Red := FloatToByte(R);
  Result.Green := FloatToByte(G);
  Result.Blue := FloatToByte(B);
  Result.Alpha := HiByte;
end;

{var
  M1, M2: Float;
  H, S, L, R, G, B: Float;

  function HueToColor(Hue: Float): Float;
  var
    V: Double;
  begin
    Hue := Hue - Floor(Hue);
    if 6 * Hue < 1 then
      V := M1 + (M2 - M1) * Hue * 6
    else if 2 * Hue < 1 then
      V := M2
    else if 3 * Hue < 2 then
      V := M1 + (M2 - M1) * (2 / 3 - Hue) * 6
    else
      V := M1;
    Result := V;
  end;

begin
  H := Remainder(Value.Hue, 1);
  if H < 0 then
    H := 1 - H;
  S := Clamp(Value.Saturation);
  L := Clamp(Value.Lightness);
  if S = 0 then
  begin
    R := L;
    G := L;
    B := L;
  end
  else
  begin
    if L <= 0.5 then
      M2 := L * (1 + S)
    else
      M2 := L + S - L * S;
    M1 := 2 * L - M2;
    R := HueToColor(H + OneOverThree);
    G := HueToColor(H);
    B := HueToColor(H - OneOverThree)
  end;
  Result.Red := FloatToByte(R);
  Result.Green := FloatToByte(G);
  Result.Blue := FloatToByte(B);
  Result.Alpha := HiByte;
end;}

class operator TColorB.Explicit(Value: TColorB): THSL;
var
  B, R, G, D, CMax, CMin: Single;
begin
  B := Value.Blue / HiByte;
  G := Value.Green / HiByte;
  R := Value.Red / HiByte;
  CMax := Max(R, Max(G, B));
  CMin := Min(R, Min(G, B));
  Result.Lightness := (CMax + CMin) / 2;
  if CMax = CMin then
  begin
    Result.Hue := 0;
    Result.Saturation := 0
  end
  else
  begin
    D := CMax - CMin;
    if Result.Lightness < 0.5 then Result.Saturation := D / (CMax + CMin)
    else Result.Saturation := D / (2 - CMax - CMin);
    if R = CMax then Result.Hue := (G - B) / D
    else
      if G = CMax then Result.Hue  := 2 + (B - R) / D
      else Result.Hue := 4 + (R - G) / D;
    Result.Hue := Result.Hue / 6;
    if Result.Hue < 0 then Result.Hue := Result.Hue + 1
  end;
end;

class operator TColorB.Implicit(Value: TColor): TColorB;
var
  C: TColor;
  S: TRGBAShort absolute C;
begin
  C := ColorToRGB(Value);
  Result.Red := S.R;
  Result.Green := S.G;
  Result.Blue := S.B;
  Result.Alpha := HiByte;
end;

class operator TColorB.Explicit(Value: TColorB): TColor;
var
  S: TRGBAShort;
begin
  S.R := Value.Red;
  S.G := Value.Green;
  S.B := Value.Blue;
  S.A := 0;
  Result := TColor(S);
end;

class operator TColorB.Negative(A: TColorB): TColorB;
begin
  Result.Red := High(Byte) - A.Red;
  Result.Green := High(Byte) - A.Green;
  Result.Blue := High(Byte) - A.Blue;
  Result.Alpha := A.Alpha;
end;

class operator TColorB.Positive(A: TColorB): TColorB;
begin
  Result := A;
end;

class operator TColorB.Equal(A: TColorB; B: TColorB): Boolean;
begin
  Result := PInteger(@A)^ = PInteger(@B)^;
end;

class operator TColorB.NotEqual(A: TColorB; B: TColorB): Boolean;
begin
  Result := PInteger(@A)^ <> PInteger(@B)^;
end;

class function TColorB.Create(B, G, R: Byte; A: Byte = $FF): TColorB;
begin
  Result.Blue := B;
  Result.Green := G;
  Result.Red := R;
  Result.Alpha := A;
end;

function TColorB.Invert: TColorB;
begin
  Result.Blue := $FF - Blue;
  Result.Green := $FF - Green;
  Result.Red := $FF - Red;
  Result.Alpha := Alpha;
end;

function MixB(V1: Byte; P1: Float; V2: Byte; P2: Float): Byte; overload;
begin
  Result := Round(V1 * P1 + V2 * P2);
end;

function MixF(V1, P1, V2, P2: Float): Float; overload;
begin
  Result := V1 * P1 + V2 * P2;
end;

function TColorB.Blend(Value: TColorB; Percent: Float): TColorB;
var
  Compliment: Float;
begin
  if Percent = 0 then
    Exit(Self);
  if Percent = 1 then
    Exit(Value);
  Compliment := 1 - Percent;
  Result.Red := MixB(Value.Red, Percent, Red, Compliment);
  Result.Green := MixB(Value.Green, Percent, Green, Compliment);
  Result.Blue := MixB(Value.Blue, Percent, Blue, Compliment);
  Result.Alpha := MixB(Value.Alpha, Percent, Alpha, Compliment);
end;

function TColorB.Desaturate(Percent: Float): TColorB;
var
  Gray: Byte;
  Compliment: Float;
begin
  if Percent = 0 then
    Exit(Self);
  Gray := Round(Red * 0.2126 + Green * 0.7152 + Blue * 0.0722);
  Result.Alpha := Alpha;
  if Percent = 1 then
  begin
    Result.Blue := Gray;
    Result.Green := Gray;
    Result.Red := Gray;
  end
  else
  begin
    Compliment := 1 - Percent;
    Result.Blue := MixB(Gray, Percent, Blue, Compliment);
    Result.Green := MixB(Gray, Percent, Green, Compliment);
    Result.Red := MixB(Gray, Percent, Red, Compliment);
  end;
end;

function TColorB.Darken(Percent: Float): TColorB;
var
  Compliment: Float;
begin
  if Percent <= 0.005 then
    Exit(Self);
  Result.Alpha := Alpha;
  if Percent >= 0.995 then
  begin
    Result.Blue := 0;
    Result.Green := 0;
    Result.Red := 0;
  end
  else
  begin
    Compliment := 1 - Percent;
    Result.Blue := MixB(0, Percent, Blue, Compliment);
    Result.Green := MixB(0, Percent, Green, Compliment);
    Result.Red := MixB(0, Percent, Red, Compliment);
  end;
end;

function TColorB.Lighten(Percent: Float): TColorB;
var
  Compliment: Float;
begin
  if Percent <= 0.005 then
    Exit(Self);
  Result.Alpha := Alpha;
  if Alpha = 0 then
  begin
    Result.Blue := 0;
    Result.Green := 0;
    Result.Red := 0;
  end
  else if (Alpha = $FF) and (Percent >= 0.995) then
  begin
    Result.Blue := HiByte;
    Result.Green := HiByte;
    Result.Red := HiByte;
  end
  else if Alpha = $FF then
  begin
    Compliment := 1 - Percent;
    Result.Blue := MixB(HiByte, Percent, Blue, Compliment);
    Result.Green := MixB(HiByte, Percent, Green, Compliment);
    Result.Red := MixB(HiByte, Percent, Red, Compliment);
  end
  else
  begin
    Compliment := 1 - Percent;
    Result.Blue := MixB(HiByte, Percent, Blue, Compliment);
    Result.Green := MixB(HiByte, Percent, Green, Compliment);
    Result.Red := MixB(HiByte, Percent, Red, Compliment);
    Compliment := Alpha / $FF;
    Result.Blue := Round(Result.Blue * Compliment);
    Result.Green := Round(Result.Green * Compliment);
    Result.Red := Round(Result.Red * Compliment);
  end;
end;

function TColorB.Fade(Percent: Float): TColorB;
begin
  Result := Self;
  Percent := Clamp(Percent);
  if Percent = 1 then
    Exit(Self);
  Result.Alpha := Round(Result.Alpha * Percent);
end;

function TColorB.Color: TColor;
begin
  Result := TColor(Self);
end;

{ TColorHelper }

function TColorHelper.Blend(Value: TColorB; Percent: Float): TColorB;
begin
  Percent := Clamp(Percent);
  Result := TColorB(Self).Blend(Value, Percent);
end;

{ TColorF }

function GetColorF(const C: TColorB): TColorF;
begin
  Result.Blue := C.Blue / HiByte;
  Result.Green := C.Green / HiByte;
  Result.Red := C.Red / HiByte;
  Result.Alpha := C.Alpha / HiByte;
end;

function GetColorB(const C: TColorF): TColorB;
begin
  Result.Blue := Round(Clamp(C.Blue) * HiByte);
  Result.Green := Round(Clamp(C.Green) * HiByte);
  Result.Red := Round(Clamp(C.Red) * HiByte);
  Result.Alpha := Round(Clamp(C.Alpha) * HiByte);
end;

class operator TColorF.Implicit(const Value: THSL): TColorF;
begin
  Result := GetColorF(TColorB(Value));
end;

class operator TColorF.Explicit(const Value: TColorF): THSL;
begin
  Result := THSL(GetColorB(Value));
end;

class operator TColorF.Implicit(Value: TColorB): TColorF;
begin
  Result := GetColorF(Value);
end;

class operator TColorF.Explicit(const Value: TColorF): TColorB;
begin
  Result := GetColorB(Value);
end;

class operator TColorF.Implicit(Value: TColor): TColorF;
begin
  Result := GetColorF(TColorB(Value));
end;

class operator TColorF.Explicit(const Value: TColorF): TColor;
begin
  Result := TColor(GetColorB(Value));
end;

class function TColorF.Create(B, G, R: Float; A: Byte = 1): TColorF;
begin
  Result.Blue := B;
  Result.Green := G;
  Result.Red := R;
  Result.Alpha := A;
end;

function TColorF.Blend(const Value: TColorF; Percent: Float): TColorF;
var
  Compliment: Float;
begin
  Percent := Clamp(Percent);
  if Percent < 0.0001 then
    Exit(Self);
  if Percent > 0.9999 then
    Exit(Value);
  Compliment := 1 - Percent;
  Result.Blue := MixF(Blue, Compliment, Value.Blue, Percent);
  Result.Green := MixF(Green, Compliment, Value.Green, Percent);
  Result.Red := MixF(Red, Compliment, Value.Red, Percent);
  Result.Alpha := MixF(Alpha, Compliment, Value.Alpha, Percent);
end;

function TColorF.Desaturate(Percent: Float): TColorF;
var
  Gray: Float;
  Compliment: Float;
begin
  Percent := Clamp(Percent);
  if Percent < 0.0001 then
    Exit(Self);
  Gray := Red * 0.2126 + Green * 0.7152 + Blue * 0.0722;
  Result.Alpha := Alpha;
  if Percent > 0.9999 then
  begin
    Result.Blue := Gray;
    Result.Green := Gray;
    Result.Red := Gray;
  end
  else
  begin
    Compliment := 1 - Percent;
    Result.Blue := MixF(Gray, Percent, Blue, Compliment);
    Result.Green := MixF(Gray, Percent, Green, Compliment);
    Result.Red := MixF(Gray, Percent, Red, Compliment);
  end;
end;

function TColorF.Fade(Percent: Float): TColorF;
begin
  Result := Self;
  Result.Alpha := Clamp(Percent);
end;

function TColorF.Color: TColor;
begin
  Result := TColor(GetColorB(Self));
end;

{ Color routines }

type
  TColorName = record
    Color: TColorB;
    Name: string;
  end;
  TColorNames = TArrayList<TColorName>;

var
  ColorNames: TColorNames;

procedure InitColorNames;
var
  I: Integer;

  procedure Add(Color: TColorB; Name: string);
  begin
    ColorNames.Items[I].Color := Color;
    ColorNames.Items[I].Name := Name;
    Inc(I);
  end;

begin
  if ColorNames.Length > 0 then
    Exit;
  ColorNames.Length := 200;
  I := 0;
  Add(clTransparent, 'Transparent');
  Add(clWhite, 'White');
  Add(clBlack, 'Black');
  Add(clLtGray, 'Light Gray');
  Add(clMedGray, 'Medium Gray');
  Add(clDkGray, 'Dark Gray');
  Add(clMaroon, 'Maroon');
  Add(clGreen, 'Green');
  Add(clOlive, 'Olive');
  Add(clNavy, 'Navy');
  Add(clPurple, 'Purple');
  Add(clTeal, 'Teal');
  Add(clGray, 'Gray');
  Add(clSilver, 'Silver');
  Add(clRed, 'Red');
  Add(clLime, 'Lime');
  Add(clYellow, 'Yellow');
  Add(clBlue, 'Blue');
  Add(clFuchsia, 'Fuchsia');
  Add(clAqua, 'Aqua');
  Add(clMoneyGreen, 'Money Green');
  Add(clSkyBlue, 'Sky Blue');
  Add(clCream, 'Cream');
  Add(clAliceBlue, 'Alice Blue');
  Add(clAntiqueWhite, 'Antique White');
  Add(clAquamarine, 'Aquamarine');
  Add(clAzure, 'Azure');
  Add(clBeige, 'Beige');
  Add(clBisque, 'Bisque');
  Add(clBlanchedAlmond, 'Blanched Almond');
  Add(clBlueViolet, 'Blue Violet');
  Add(clBrown, 'Brown');
  Add(clBurlywood, 'Burlywood');
  Add(clCadetBlue, 'Cadet Blue');
  Add(clChartreuse, 'Chartreuse');
  Add(clChocolate, 'Chocolate');
  Add(clCoral, 'Coral');
  Add(clCornFlowerBlue, 'Corn Flower Blue');
  Add(clCornSilk, 'Corn Silk');
  Add(clCrimson, 'Crimson');
  Add(clCyan, 'Cyan');
  Add(clDarkBlue, 'Dark Blue');
  Add(clDarkCyan, 'Dark Cyan');
  Add(clDarkGoldenrod, 'Dark Goldenrod');
  Add(clDarkGreen, 'Dark Green');
  Add(clDarkKhaki, 'Dark Khaki');
  Add(clDarkMagenta, 'Dark Magenta');
  Add(clDarkOliveGreen, 'Dark Olive Green');
  Add(clDarkOrange, 'Dark Orange');
  Add(clDarkOrchid, 'Dark Orchid');
  Add(clDarkRed, 'Dark Red');
  Add(clDarkSalmon, 'Dark Salmon');
  Add(clDarkseaGreen, 'Darksea Green');
  Add(clDarkslateBlue, 'Darkslate Blue');
  Add(clDarkSlateGray, 'DarkSlate Gray');
  Add(clDarkTurquoise, 'Dark Turquoise');
  Add(clDarkViolet, 'Dark Violet');
  Add(clDeepPink, 'Deep Pink');
  Add(clDeepSkyBlue, 'Deep Sky Blue');
  Add(clDimGray, 'Dim Gray');
  Add(clDodgerBlue, 'Dodger Blue');
  Add(clFireBrick, 'Fire Brick');
  Add(clFloralWhite, 'Floral White');
  Add(clForestGreen, 'Forest Green');
  Add(clGainsboro, 'Gainsboro');
  Add(clGhostWhite, 'Ghost White');
  Add(clGold, 'Gold');
  Add(clGoldenrod, 'Goldenrod');
  Add(clGreenYellow, 'Green Yellow');
  Add(clHoneydew, 'Honeydew');
  Add(clHotPink, 'Hot Pink');
  Add(clIndianRed, 'Indian Red');
  Add(clIndigo, 'Indigo');
  Add(clIvory, 'Ivory');
  Add(clKhaki, 'Khaki');
  Add(clLavender, 'Lavender');
  Add(clLavenderBlush, 'Lavender Blush');
  Add(clLawnGreen, 'Lawn Green');
  Add(clLemonChiffon, 'Lemon Chiffon');
  Add(clLightBlue, 'Light Blue');
  Add(clLightCoral, 'Light Coral');
  Add(clLightCyan, 'Light Cyan');
  Add(clLightGoldenrodYellow, 'Light Goldenrod Yellow');
  Add(clLightGreen, 'LightGreen');
  Add(clLightGray, 'Light Gray');
  Add(clLightPink, 'Light Pink');
  Add(clLightSalmon, 'Light Salmon');
  Add(clLightSeaGreen, 'Light Sea Green');
  Add(clLightSkyBlue, 'Light Sky Blue');
  Add(clLightSlateGray, 'Light Slate Gray');
  Add(clLightSteelBlue, 'Light Steel Blue');
  Add(clLightYellow, 'Light Yellow');
  Add(clLimeGreen, 'Lime Green');
  Add(clLinen, 'Linen');
  Add(clMagenta, 'Magenta');
  Add(clMediumAquamarine, 'Medium Aquamarine');
  Add(clMediumBlue, 'Medium Blue');
  Add(clMediumOrchid, 'Medium Orchid');
  Add(clMediumPurple, 'Medium Purple');
  Add(clMediumSeaGreen, 'Medium Sea Green');
  Add(clMediumSlateBlue, 'Medium Slate Blue');
  Add(clMediumSpringGreen, 'Medium Spring Green');
  Add(clMediumTurquoise, 'Medium Turquoise');
  Add(clMediumVioletRed, 'Medium Violet Red');
  Add(clMidnightBlue, 'Midnight Blue');
  Add(clMintCream, 'Mint Cream');
  Add(clMistyRose, 'Misty Rose');
  Add(clMoccasin, 'Moccasin');
  Add(clNavajoWhite, 'Navajo White');
  Add(clOldLace, 'Old Lace');
  Add(clOliveDrab, 'Olive Drab');
  Add(clOrange, 'Orange');
  Add(clOrangeRed, 'Orange Red');
  Add(clOrchid, 'Orchid');
  Add(clPaleGoldenrod, 'Pale Goldenrod');
  Add(clPaleGreen, 'Pale Green');
  Add(clPaleTurquoise, 'Pale Turquoise');
  Add(clPalevioletRed, 'Paleviolet Red');
  Add(clPapayaWhip, 'Papaya Whip');
  Add(clPeachPuff, 'Peach Puff');
  Add(clPeru, 'Peru');
  Add(clPink, 'Pink');
  Add(clPlum, 'Plum');
  Add(clPowderBlue, 'Powder Blue');
  Add(clRosyBrown, 'Rosy Brown');
  Add(clRoyalBlue, 'Royal Blue');
  Add(clSaddleBrown, 'Saddle Brown');
  Add(clSalmon, 'Salmon');
  Add(clSandyBrown, 'Sandy Brown');
  Add(clSeaGreen, 'Sea Green');
  Add(clSeaShell, 'Sea Shell');
  Add(clSienna, 'Sienna');
  Add(clSkyBlue, 'Sky Blue');
  Add(clSlateBlue, 'Slate Blue');
  Add(clSlateGray, 'Slate Gray');
  Add(clSnow, 'Snow');
  Add(clSpringGreen, 'Spring Green');
  Add(clSteelBlue, 'Steel Blue');
  Add(clTan, 'Tan');
  Add(clThistle, 'Thistle');
  Add(clTomato, 'Tomato');
  Add(clTurquoise, 'Turquoise');
  Add(clViolet, 'Violet');
  Add(clWheat, 'Wheat');
  Add(clWhiteSmoke, 'White Smoke');
  Add(clYellowGreen, 'Yellow Green');
end;

function StrToColor(S: string): TColorB;
var
  N: string;
  I: Integer;
begin
  InitColorNames;
  Result := clTransparent;
  S := S.ToUpper;
  for I := ColorNames.Lo to ColorNames.Hi do
  begin
    N := ColorNames[I].Name;
    if N = '' then
      Exit;
    if N.ToUpper = S then
      Exit(ColorNames[I].Color);
  end;
end;

function ColorToStr(C: TColorB): string;
var
  N: string;
  I: Integer;
begin
  InitColorNames;
  Result := '';
  for I := ColorNames.Lo to ColorNames.Hi do
  begin
    N := ColorNames[I].Name;
    if N = '' then
      Exit;
    if ColorNames[I].Color = C then
      Exit(N);
  end;
end;

procedure RegisterColorName(Color: TColorB; Name: string);
var
  ColorName: TColorName;
  S: string;
  N: string;
  I: Integer;
begin
  Name := Name.Trim;
  S := Name.ToUpper;
  if S.Length = 0 then
    Exit;
  InitColorNames;
  if ColorNames.Last.Name <> '' then
    ColorNames.Length := ColorNames.Length + 50;
  for I := ColorNames.Lo to ColorNames.Hi do
  begin
    N := ColorNames[I].Name;
    if N = '' then
    begin
      ColorName.Color := Color;
      ColorName.Name := Name;
      ColorNames[I] := ColorName;
      Exit;
    end;
    if ColorNames[I].Color = Color then
      Exit;
    if ColorNames[I].Name.ToUpper = S then
      Exit;
  end;
end;

function HueToColor(H: Float): TColorB;
begin
  Result := THSL(H);
end;

function ColorToHue(C: TColorB): Float;
begin
  Result := THSL(C).Hue;
end;

function Hue(H: Float): TColorB;
begin
  Result := THSL(H);
end;

function HueInvert(H: Float): TColorB;
begin
  H := H + 0.5;
  if H > 1 then
    H := H - 1;
  Result := THSL(H);
end;

function Blend(A, B: TColorB; Percent: Float): TColorB;
begin
  Result := A.Blend(B, Percent);
end;

function Fade(Color: TColorB; Percent: Float): TColorB;
begin
  Result := Color.Fade(Percent);
end;

function Darken(Color: TColorB; Percent: Float): TColorB;
begin
  Result := Color.Darken(Clamp(Percent));
end;

function Lighten(Color: TColorB; Percent: Float): TColorB;
begin
  Result := Color.Lighten(Clamp(Percent));
end;

function Rgba(R, G, B: Byte; A: Float): TColorB;
begin
  Result.Red := R;
  Result.Green := R;
  Result.Blue := R;
  Result.Alpha := Round(Clamp(A) * $FF);
end;

function Rgba(Color:TColor; A: Float): TColorB;
begin
  Result := Color;
  Result.Alpha := Round(Clamp(A) * $FF);
end;

{ Miscellaneous routines }

function Divide(const Quotient, Divisor: Extended): Extended;
begin
  if Divisor = 0 then
    Result := 0
  else
    Result := Trunc(Quotient / Divisor) * Divisor;
end;

function Remainder(const Quotient, Divisor: Extended): Extended;
begin
  if Divisor = 0 then
    Result := 0
  else
    Result := Quotient - Divisor * Trunc(Quotient / Divisor);
end;

function Clamp(Percent: Float): Float;
begin
  if Percent < 0 then
    Result := 0
  else if Percent > 1 then
    Result := 1
  else
    Result := Percent;
end;

function DegToRad(D: Float): Float;
begin
  Result := D / 180 * Pi;
end;

function RadToDeg(R: Float): Float;
begin
  Result := R * 180 / Pi;
end;

function StrToImageFormat(S: string): TImageFormat;
begin
  S := S.ToLower;
  Result := fmPng;
  if S.EndsWith('png') then
    Result := fmPng
  else if S.EndsWith('jpg') then
    Result := fmJpeg
  else if S.EndsWith('jpeg') then
    Result := fmJpeg
  else if S.EndsWith('gif') then
    Result := fmGif
  else if S.EndsWith('bmp') then
    Result := fmBmp
  else if S.EndsWith('ico') then
    Result := fmIco
  else if S.EndsWith('tif') then
    Result := fmTiff
  else if S.EndsWith('tiff') then
    Result := fmTiff;
end;

function ImageFormatToStr(F: TImageFormat): string;
const
  Formats: array[TImageFormat] of string =
    ('png', 'jpeg', 'gif', 'bmp', 'ico', 'tiff');
begin
  Result := Formats[F];
end;

function ImageFormatToMimeType(F: TImageFormat): string;
begin
  Result := 'image/' + ImageFormatToStr(F);
end;

end.

