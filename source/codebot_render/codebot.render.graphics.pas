(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.render.graphics.txt> }
unit Codebot.Render.Graphics;

{ Codebot.Render.Graphics provides vector graphics through an ICanvas
  interface which renders using Codebot.Render.NanoVG. A canvas draws paths,
  images, sprites and text with pens, brushes and fonts. It was adapted from
  the Tiny.Graphics unit of Tiny Sim.

  A canvas must be created and used while an OpenGL context is current. }

{$i render.inc}

interface

uses
  Codebot.System,
  Codebot.Graphics.Types,
  Codebot.Geometry;

{ Counts of the objects created and destroyed, which can be used to find leaks }
var
  MatrixPushPop: Integer;
  MatrixCreated: Integer;
  MatrixDestroyed: Integer;
  FontsCreated: Integer;
  FontsDestroyed: Integer;
  BitmapsCreated: Integer;
  BitmapsDestroyed: Integer;

{ Forward declarations }

type
  IMatrix = interface;
  IBitmap = interface;
  IFont = interface;
  IPen = interface;
  IBrush = interface;
  ISolidBrush = interface;
  IGradientBrush = interface;
  ILinearGradientBrush = interface;
  IRadialGradientBrush = interface;
  IBoxGradientBrush = interface;
  IBitmapBrush = interface;
  ISprite = interface;
  ITextWriter = interface;
  ICanvas = interface;

{ Enumerations used by the interfaces above }

  { TLineJoin is how lines are joined at corners }
  TLineJoin = (joinMiter, joinBevel, joinRound);
  { TLineCap is how the ends of lines are drawn }
  TLineCap = (capButt, capSquare, capRound);
  { TFontAlign is where text is placed across from its position }
  TFontAlign = (fontLeft, fontCenter, fontRight);
  { TFontLayout is where text is placed up and down from its position }
  TFontLayout = (fontTop, fontMiddle, fontBaseline, fontBottom);
  { TWinding is the direction a path is wound, which makes it solid or a hole }
  TWinding = (windCCW, windCW);

{ TFillRule decides which areas are inside a path with several sub-paths.
  fillWinding fills each sub-path as solid unless Winding marks it as a hole,
  fillNonZero makes sub-paths drawn in the opposite direction holes, and
  fillEvenOdd makes areas where sub-paths overlap an even number of times
  holes. }

  TFillRule = (fillWinding, fillNonZero, fillEvenOdd);

{ TBlendMode is how drawing is combined with what is already drawn }

  TBlendMode = (blendAlpha, blendAdditive, blendSubtractive,
    blendLighten, blendDarken, blendInvert, blendNegative);

{ IMatrix is a 2D transform, created with NewMatrix }

  IMatrix = interface
  ['{2F82AE30-10EB-40B5-87DD-C9124319B5CB}']
    { Set the six values of the matrix }
    procedure Copy(A, B, C, D, E, F: Float); overload;
    { Copy the values of another matrix }
    procedure Copy(M: IMatrix); overload;
    { Reset to the identity matrix }
    procedure Identity;
    { Returns the inverse as a new matrix }
    function Inverse: IMatrix;
    { Move by an offset }
    procedure Translate(X, Y: Float);
    { Rotate by an angle }
    procedure Rotate(Angle: Float);
    { Rotate by an angle about a point }
    procedure RotateAt(Angle, X, Y: Float);
    { Scale along each axis }
    procedure Scale(SX, SY: Float);
    { Scale along each axis about a point }
    procedure ScaleAt(SX, SY, X, Y: Float);
    { Skew along the x axis }
    procedure SkewX(X: Float);
    { Skew along the y axis }
    procedure SkewY(Y: Float);
    { Combine with another matrix }
    procedure Transform(M: IMatrix);
    { Creates new matrix C using the formula:
      C = A * B
      A is this matrix, B is the matrix passed below, and C is the result }
    function Multiply(M: IMatrix): IMatrix; overload;
    { Creates a new point C using the formula:
      C = A * B
      A is this matrix, B is the point passed below, and C is the result }
    function Multiply(const P: TPointF): TPointF; overload;
    { Preserve the matrix state by pushing it onto a stack }
    procedure Push;
    { Restore the matrix state by popping it from a stack }
    procedure Pop;
  end;

{ IBitmap are fixed size bitmap instances can be created using any of the
  Canvas.LoadBitmap methods. They differ from IRenderBitmap in that they cannot
  be the target of canvas rendering and cannot be resized.

  Note: Bitmaps and share the same namespace as render bitmaps. }

  IBitmap = interface
  ['{2E0D937A-B9F9-4573-8E22-407BDBA2C587}']
    {$region property access methods}
    function GetName: string;
    function GetClientRect: TRectF;
    function GetWidth: LongWord;
    function GetHeight: LongWord;
    {$endregion}
    { Name is defined when you load a bitmap from a canvas }
    property Name: string read GetName;
    { A convenient rectangle exactly fiting the bitmap }
    property ClientRect: TRectF read GetClientRect;
    { The fixed width and height of the bitmap }
    property Width: LongWord read GetWidth;
    property Height: LongWord read GetHeight;
  end;

{ IRenderBitmap is a special bitmap which can be drawn to using canvas
  commands. When bound to a canvas commands such as LineTo, Circle, and Fill
  create graphics on the bitmap. When unbound the render bitmap can be used
  as with commands such as DrawImage or as a property of a bitmap brush.

  Note: A render bitmap can be created using the Canvas.NewBitmap method and
  share the same namespace as fixed bitmaps. }

  IRenderBitmap = interface(IBitmap)
  ['{C8850303-01D8-4BFD-AA68-20DBF90C430B}']
    { Bind makes the render bitmap the current target for canvas drawing }
    procedure Bind;
    { Unbind restores the back buffer as the target for canvas drawing }
    procedure Unbind;
    { If unbound resize the drawable area of this render bitmap }
    procedure Resize(W, H: LongWord);
  end;

{ IFont instances can be created by using Canvas.LoadFont methods. }

  IFont = interface
  ['{2DF60E11-7BEB-486E-B7DC-3714ED310B90}']
    {$region property access methods}
    function GetName: string;
    function GetColor: TColorF;
    procedure SetColor(const Value: TColorF);
    function GetSize: Float;
    procedure SetSize(Value: Float);
    function GetHeight: Float;
    procedure SetHeight(Value: Float);
    function GetAlign: TFontAlign;
    procedure SetAlign(const Value: TFontAlign);
    function GetLayout: TFontLayout;
    procedure SetLayout(const Value: TFontLayout);
    function GetBlur: Float;
    procedure SetBlur(Value: Float);
    function GetLetterSpacing: Float;
    procedure SetLetterSpacing(Value: Float);
    function GetLineSpacing: Float;
    procedure SetLineSpacing(Value: Float);
    {$endregion}
    { Name of the font }
    property Name: string read GetName;
    { Only solid color fonts are supported }
    property Color: TColorF read GetColor write SetColor;
    { Font size in pixels }
    property Size: Float read GetSize write SetSize;
    { Font height in points }
    property Height: Float read GetHeight write SetHeight;
    { Horizontal alignment of text rendered with this font }
    property Align: TFontAlign read GetAlign write SetAlign;
    { Vertical layout of text rendered with this font }
    property Layout: TFontLayout read GetLayout write SetLayout;
    { Font can be blurred which might be useful for soft font shadows }
    property Blur: Float read GetBlur write SetBlur;
    { Additive value used to modify letter spacing }
    property LetterSpacing: Float read GetLetterSpacing write SetLetterSpacing;
    { Multiplicative factor used to modify line spacing }
    property LineSpacing: Float read GetLineSpacing write SetLineSpacing;
  end;

{ IPen instances are used to stroke canvas paths.
  Note: Pens can reference a brush for more complex coloring options. }

  IPen = interface
  ['{3D67CC14-17DC-4653-B97A-02E00A16D431}']
    {$region property access methods}
    function GetColor: TColorF;
    procedure SetColor(const Value: TColorF);
    function GetBrush: IBrush;
    procedure SetBrush(Value: IBrush);
    function GetWidth: Float;
    procedure SetWidth(Value: Float);
    function GetMiterLimit: Float;
    procedure SetMiterLimit(Value: Float);
    function GetLineCap: TLineCap;
    procedure SetLineCap(const Value: TLineCap);
    function GetLineJoin: TLineJoin;
    procedure SetLineJoin(const Value: TLineJoin);
    {$endregion}
    { The color of the stroke }
    property Color: TColorF read GetColor write SetColor;
    { A brush to stroke with in place of the color }
    property Brush: IBrush read GetBrush write SetBrush;
    { The width of the stroke }
    property Width: Float read GetWidth write SetWidth;
    { How far a miter join can reach before it is cut off }
    property MiterLimit: Float read GetMiterLimit write SetMiterLimit;
    { How the ends of lines are drawn }
    property LineCap: TLineCap read GetLineCap write SetLineCap;
    { How lines are joined at corners }
    property LineJoin: TLineJoin read GetLineJoin write SetLineJoin;
  end;

{ IBrush is the base interface for the various brush types to follow }

  IBrush = interface
  ['{B7F6A0C7-EE64-4B97-96AF-74A5A5362486}']
  end;

{ ISolidBrush is a simple solid color brush }

  ISolidBrush = interface(IBrush)
  ['{35443F16-1FCD-490B-B2D0-5C17D6621953}']
    {$region property access methods}
    function GetColor: TColorF;
    procedure SetColor(const Value: TColorF);
    {$endregion}
    { The color of the brush }
    property Color: TColorF read GetColor write SetColor;
  end;

{ IGradientStop is used by gradient brushes }

  IGradientStop = interface
  ['{A44D2101-F2B4-4AC2-B346-CB2B7857CB13}']
    {$region property access methods}
    function GetOffset: Float;
    procedure SetOffset(Value: Float);
    function GetColor: TColorF;
    procedure SetColor(const Value: TColorF);
    {$endregion}
    { Where the stop is along the gradient, from 0 to 1 }
    property Offset: Float read GetOffset write SetOffset;
    { The color at the stop }
    property Color: TColorF read GetColor write SetColor;
  end;

{ IGradientBrush is the base interface for gradient brushes }

  IGradientBrush = interface(IBrush)
  ['{2EAF4D99-EFAC-47C9-AB45-C6C49ACAAF98}']
    {$region property access methods}
    function GetNearStop: IGradientStop;
    function GetFarStop: IGradientStop;
    {$endregion}
    { The stop where the gradient begins }
    property NearStop: IGradientStop read GetNearStop;
    { The stop where the gradient ends }
    property FarStop: IGradientStop read GetFarStop;
  end;

{ ILinearGradientBrush blends from the near stop at point A to the far stop at
  point B }

  ILinearGradientBrush = interface(IGradientBrush)
  ['{88232BE7-7B51-4E50-B71C-7EC2AD28AAA2}']
    {$region property access methods}
    function GetA: TPointF;
    procedure SetA(const Value: TPointF);
    function GetB: TPointF;
    procedure SetB(const Value: TPointF);
    {$endregion}
    { The point where the gradient begins }
    property A: TPointF read GetA write SetA;
    { The point where the gradient ends }
    property B: TPointF read GetB write SetB;
  end;

{ IRadialGradientBrush blends from the near stop at the center of a rectangle
  to the far stop at its edge }

  IRadialGradientBrush = interface(IGradientBrush)
  ['{79C44874-B697-4C02-8E29-5D257828A590}']
    {$region property access methods}
    function GetRect: TRectF;
    procedure SetRect(const Value: TRectF);
    {$endregion}
    { The rectangle the gradient fills }
    property Rect: TRectF read GetRect write SetRect;
  end;

{ IBoxGradientBrush is a feathered rounded rectangle useful for drop shadows
  and highlights. The near stop color is used inside the rectangle and the
  far stop color outside of it. The stop offsets are not used. }

  IBoxGradientBrush = interface(IGradientBrush)
  ['{6A3D2C1E-5B47-4F0A-9C8D-2E1F7A4B9C30}']
    {$region property access methods}
    function GetRect: TRectF;
    procedure SetRect(const Value: TRectF);
    function GetRadius: Float;
    procedure SetRadius(Value: Float);
    function GetFeather: Float;
    procedure SetFeather(Value: Float);
    {$endregion}
    { The rectangle defining the gradient }
    property Rect: TRectF read GetRect write SetRect;
    { The corner radius of the rectangle }
    property Radius: Float read GetRadius write SetRadius;
    { How blurry the border of the rectangle is }
    property Feather: Float read GetFeather write SetFeather;
  end;

{ IBitmapBrush references a bitmap as a repeatable fill pattern. The pattern
  can be scaled along two axes, translated, rotated, and alpha blended.

  Note: GLES2 backend requires patterns to be a power of two or they will not
  wrap. }

  IBitmapBrush = interface(IBrush)
  ['{8C1F4E62-3A9B-4D7E-B05C-91D2E6F3A7B4}']
    {$region property access methods}
    function GetBitmap: IBitmap;
    procedure SetBitmap(Value: IBitmap);
    function GetAngle: Float;
    procedure SetAngle(Value: Float);
    function GetOffset: TPointF;
    procedure SetOffset(const Value: TPointF);
    function GetScale: TPointF;
    procedure SetScale(const Value: TPointF);
    function GetOpacity: Float;
    procedure SetOpacity(Value: Float);
    {$endregion}
    { The bitmap repeated as the pattern }
    property Bitmap: IBitmap read GetBitmap write SetBitmap;
    { The angle the pattern is rotated by }
    property Angle: Float read GetAngle write SetAngle;
    { The distance the pattern is moved }
    property Offset: TPointF read GetOffset write SetOffset;
    { The scale of the pattern along each axis }
    property Scale: TPointF read GetScale write SetScale;
    { The opacity of the pattern from 0 to 1 }
    property Opacity: Float read GetOpacity write SetOpacity;
  end;

{ ISprite draws one cell of a sprite sheet with a position, rotation, and
  scale }

  ISprite = interface
  ['{4B6882E6-F483-4D43-87EC-5F85C39FB228}']
    {$region property access methods}
    function GetBitmap: IBitmap;
    procedure SetBitmap(Value: IBitmap);
    function GetCols: Integer;
    procedure SetCols(Value: Integer);
    function GetRows: Integer;
    procedure SetRows(Value: Integer);
    function GetCell: Integer;
    procedure SetCell(Value: Integer);
    function GetPivot: TVec2;
    procedure SetPivot(Value: TVec2);
    function GetPosition: TVec2;
    procedure SetPosition(Value: TVec2);
    function GetRotation: Single;
    procedure SetRotation(Value: Single);
    function GetScale: TVec2;
    procedure SetScale(Value: TVec2);
    function GetOpacity: Single;
    procedure SetOpacity(Value: Single);
    {$endregion}
    { The sprite sheet }
    property Bitmap: IBitmap read GetBitmap write SetBitmap;
    { The number of columns left and right in the sprite sheet }
    property Cols: Integer read GetCols write SetCols;
    { The number of rows up and down in the sprite sheet }
    property Rows: Integer read GetRows write SetRows;
    { A zero based idnex of the current cell. Numbering starts at the top left,
      continuing right, then moving down to the next row. The total number of
      cells equals cols * rows. }
    property Cell: Integer read GetCell write SetCell;
    { A two compoent vector in the range of 0..1 that specifcies
      the position, rotation, and scale pivot. }
    property Pivot: TVec2 read GetPivot write SetPivot;
    { Position of the sprite in pixels }
    property Position: TVec2 read GetPosition write SetPosition;
    { Rotation Angle of the sprite in radians  }
    property Rotation: Single read GetRotation write SetRotation;
    { Scale of the sprite in both the x and y }
    property Scale: TVec2 read GetScale write SetScale;
    { Opacity of the sprite  in the range of 0..1 }
    property Opacity: Single read GetOpacity write SetOpacity;
  end;

{ ITextWriter simplifies writing multi line text with formatting }

  { TTextLayout is where a text writer places text on its page }
  TTextLayout = (
    { Top left }
    textLeft,
    { Top center }
    textTop,
    { Top right }
    textRight,
    { Bottom center }
    textBottom,
    { Center left }
    textMiddleLeft,
    { Centered from all margins }
    textMiddle,
    { Centered from all margins }
    textMiddleRight,
    { Centered from all margins }
    textMemo);

  ITextWriter = interface
  ['{CF8628E0-A660-4102-B4C5-01150CED28F3}']
    function GetShadow: Boolean;
    procedure SetShadow(Value: Boolean);
    { Reset the starting point where writing begins }
    procedure Paper(const Rect: TRectF);
    { Move the margin left by an amount }
    procedure Indent(Margin: Single);
    { Revert the paper back to its initial size as if nothing has been written }
    procedure NewPage;
    { Move down one row based on the font height }
    procedure NewLine;
    { Write text at the current line using a layout }
    procedure Write(const S: string; Layout: TTextLayout = textLeft);
    { Write text offset by a column amount from the left }
    procedure WriteColumn(const S: string; Column: Single; Layout: TTextLayout = textLeft);
    { Write text and start a new line }
    procedure WriteLine(const S: string);
    { Write text at the current line using a layout }
    procedure WriteAt(const S: string; X, Y: Single);
    { When shadow is true a black shadow appears under text making it easier to read in some scenes }
    property Shadow: Boolean read GetShadow write SetShadow;
  end;

{ IResourceStore is associated with a canvas, and allows for the creation and
  management of bitmap and font resources. Resoruces are tracked by name, and
  subsequent requests using the same name return an existing resource rather
  than loading the same resource mutliple times. }

  IResourceStore = interface
  ['{DEFA1507-0E3F-49C9-8E39-73F12A52560C}']
    { Create a text writer given a font }
    function NewTextWriter(Font: IFont): ITextWriter; overload;
    { Create a text writer given a font and a page }
    function NewTextWriter(Font: IFont; Page: TRectF; Margin: TVec2): ITextWriter; overload;
    { Create a new render bitmap, checking the store first if a render bitmap
      with a matching name already exists. }
    function NewBitmap(const Name: string; Width, Height: LongWord): IRenderBitmap;
    { Check the store if a image exists using name as a the key.
      If name is not found then nil is returned. }
    function LoadBitmap(const Name: string): IBitmap; overload;
    { Check the store if an image already exists. If not found load a new jpg, png,
      psd, tga, pic or gif image from a file or memory and associated it with the
      store using name as a key. When Mipmaps is true a new image is given
      mipmaps, so it stays smooth when drawn much smaller than its size. An
      image already in the store is returned as it was loaded. }
    function LoadBitmap(const Name: string; FileName: string;
      Mipmaps: Boolean = False): IBitmap; overload;
    { Load the image from memory }
    function LoadBitmap(const Name: string; Memory: Pointer; Size: LongWord;
      Mipmaps: Boolean = False): IBitmap; overload;
    { Load the image from a resource of the program }
    function LoadBitmapResource(const Name, ResName: string;
      Mipmaps: Boolean = False): IBitmap;
    { Release space used by a bitmap and dispose of the underlying resources. }
    procedure DisposeBitmap(Bitmap: IBitmap);
    { Check the store if a font exists using name as a the key.
      If name is not found then nil is returned. }
    function LoadFont(const Name: string): IFont; overload;
    { Check the store if a font already exists. If not found load a new font from a file
      or memory and associated it with the store using name as a key }
    function LoadFont(const Name: string; FileName: string): IFont; overload;
    { Load the font from memory }
    function LoadFont(const Name: string; Memory: Pointer; Size: LongWord): IFont; overload;
    { Load the font from a resource of the program }
    function LoadFontResource(const Name, ResName: string): IFont;
    { Note: It is an intentional design that fonts cannot be disposed }
  end;

// TODO: test object management

{ ICanvas provides the main interface for generating vector graphics in this
  graphics unit. Canvas can be used to draw paths, images, and text. It has
  global properties for matrix transforms, blending, and alpha blending (aka
  Opacity), and methods for clipping and clearing content. }

  ICanvas = interface(IResourceStore)
  ['{352DAE63-6D1C-40E0-955E-87C27020CCE0}']
    {$region property access methods}
    function GetBlendMode: TBlendMode;
    procedure SetBlendMode(Value: TBlendMode);
    function GetOpacity: Float;
    procedure SetOpacity(Value: Float);
    function GetMatrix: IMatrix;
    procedure SetMatrix(Value: IMatrix);
    function GetWinding: TWinding;
    procedure SetWinding(const Value: TWinding);
    function GetFillRule: TFillRule;
    procedure SetFillRule(const Value: TFillRule);
    {$endregion}
    { Push all canvas state to the stack }
    procedure Push;
    { Pop canvas state from the stack }
    procedure Pop;
    { Clip drawing to a rectangle placed with the current matrix. Calls
      intersect with the clipping already in effect, and Pop restores the
      clipping in effect at Push, so clips nest. An empty rect removes the
      clipping until the next Pop. }
    procedure Clip(const Rect: TRectF); overload;
    { Shortcut to remove the clipping }
    procedure Clip; overload;
    { Clear all pixels in the current render or back buffer target. A render
      bitmap can be used with the canvas using its Bind and Unbind methods. }
    procedure Clear;
    { Measure a Float line of text }
    function MeasureText(Font: IFont; const Text: string): TPointF;
    { Measure how far a line of text moves the place where text is drawn }
    function MeasureAdvance(Font: IFont; const Text: string): Float;
    { Measure a the height of multiple lines of text given a width }
    function MeasureMemo(Font: IFont; const Text: string; Width: Float): Float;
    { Render a Float line of text given a coordinate }
    procedure DrawText(Font: IFont; const Text: string; X, Y: Float);
    { Render a multiple lines of text given a width }
    procedure DrawTextMemo(Font: IFont; const Text: string; X, Y, Width: Float);
    { Raster image drawing }
    procedure DrawImage(Image: IBitmap; X, Y: Float; Opacity: Float = 1; Angle: Float = 0); overload;
    { Draw part of an image stretched to fill a rectangle }
    procedure DrawImage(Image: IBitmap; Source, Dest: TRectF; Opacity: Float = 1; Angle: Float = 0); overload;
   { Draw a sprite }
  	procedure DrawSprite(Sprite: ISprite);
	  { Discards the existing path and begins a new one }
    procedure BeginPath;
    { Closing the path creates a line from the last point to the first point in
      the path. It does not begin a new path. }
    procedure ClosePath;
    { These methods add geometry to the current path }
    procedure MoveTo(X, Y: Float);
    { Add a line to a point }
    procedure LineTo(X, Y: Float);
    { Add an arc which rounds the corner between two lines }
    procedure ArcTo(X1, Y1, X2, Y2, Radius: Float);
    { Add a cubic bezier curve }
    procedure BezierTo(CX1, CY1, CX2, CY2, X, Y: Float);
    { Add a quadratic bezier curve }
    procedure QuadTo(CX, CY, X, Y: Float);
    { Add an arc of a circle between two angles }
    procedure Arc(CX, CY, Radius, A0, A1: Float; Clockwise: Boolean);
    { Add a circle }
    procedure Circle(X, Y, Radius: Float); overload;
    { Add an ellipse inside a rectangle }
    procedure Ellipse(const R: TRectF); overload;
    procedure Ellipse(X, Y, Width, Height: Float); overload;
    { Add a rectangle }
    procedure Rect(const R: TRectF); overload;
    procedure Rect(X, Y, Width, Height: Float); overload;
    { Add a rectangle with rounded corners }
    procedure RoundRect(const R: TRectF; Radius: Float); overload;
    procedure RoundRect(X, Y, Width, Height: Float; Radius: Float); overload;
    { Add a polygon from an array of points }
    procedure Polygon(P: PPointF; Count: Integer; Closed: Boolean = True);
    { Radii are TL top left, TR top right, BR bottom right, and BL bottom left }
    procedure RoundRectVarying(const R: TRectF; TL, TR, BR, BL: Float);
    { When preserve is false the current path is discarded and a new path begins }
    procedure Fill(Brush: IBrush; Preserve: Boolean = False); overload;
    procedure Fill(Color: TColorF; Preserve: Boolean = False); overload;
    { Stroke the current path with a pen or a color }
    procedure Stroke(Pen: IPen; Preserve: Boolean = False); overload;
    procedure Stroke(Color: TColorF; Width: Float = 1; Preserve: Boolean = False); overload;
    { Convenience methods that begin, paint, then discard a path }
    procedure FillCircle(Brush: IBrush; X, Y, Radius: Float);
    procedure StrokeCircle(Pen: IPen; X, Y, Radius: Float);
    procedure FillRect(Brush: IBrush; const R: TRectF);
    procedure StrokeRect(Pen: IPen; const R: TRectF);
    procedure FillRoundRect(Brush: IBrush; const R: TRectF; Radius: Float);
    procedure StrokeRoundRect(Pen: IPen; const R: TRectF; Radius: Float);
    { Global blending mode can be controlled using this property }
    property BlendMode: TBlendMode read GetBlendMode write SetBlendMode;
    { Global rendering opacity can be controlled using this property }
    property Opacity: Float read GetOpacity write SetOpacity;
    { Global rendering can be transformed using this property }
    property Matrix: IMatrix read GetMatrix write SetMatrix;
    { Counter clockwise creates solid shapes and clockwise creates shapes with holes. }
    property Winding: TWinding read GetWinding write SetWinding;
    { The rule used by Fill to find holes in paths, fillWinding by default }
    property FillRule: TFillRule read GetFillRule write SetFillRule;
  end;

{ IBackBuffer can be used to toggle canvas rendering or flip the buffers. }

  IBackBuffer = interface
  ['{65B38056-5457-44B0-B10D-06F2AE95E73A}']
    { Begin canvas operations and suspend OpenGL calls }
    procedure BeginFrame;
    { End all canvas operations and resume OpenGL calls }
    procedure EndFrame;
    { Switch the front an back buffers while setting the width and height }
    procedure Flip(W, H: Integer);
  end;

{ Clamp a value to the range 0 to 1 }
function Clamp(A: Float): Float;

{ Routines to create objects used by canvas methods }

function NewColorB(R, G, B: Byte; A: Byte = $FF): TColorF;
function NewColorF(R, G, B: Float; A: Float = 1): TColorF;
function NewHSL(H, S, L: Float; A: Float = 1): TColorF;
function NewPointF(X, Y: Float): TPointF;
function NewRectF(Width, Height: Float): TRectF; overload;
function NewRectF(X, Y, Width, Height: Float): TRectF; overload;

function NewMatrix: IMatrix;
function NewPen: IPen; overload;
function NewPen(const Color: TColorF; Width: Float = 1): IPen; overload;
function NewBrush(const Color: TColorF): ISolidBrush; overload;
function NewBrush(const A, B: TPointF): ILinearGradientBrush; overload;
function NewBrush(const Rect: TRectF): IRadialGradientBrush; overload;
function NewBrush(const Rect: TRectF; Radius, Feather: Float): IBoxGradientBrush; overload;
function NewBrush(Bitmap: IBitmap): IBitmapBrush; overload;

function NewSprite: ISprite;

function NewCanvas: ICanvas;

{ Useful constants }

const
  PointZero: TPointF = (X: 0; Y: 0);
  RectEmpty: TRectF = (X: 0; Y: 0; Width: 0; Height: 0);

implementation

uses
  SysUtils, Classes,
  Codebot.Collections,
  Codebot.OpenGL,
  Codebot.Render.NanoVG;

const
  StackSize = 100;
{$ifdef windows}
  { The resource type of raw data, which the system unit declares on other
    systems and the Windows unit declares on Windows }
  RT_RCDATA = PChar(10);
{$endif}

{ Named objects and lists of named objects used to track resources }

type
  INamed = interface
  ['{B9A5411D-561B-43F6-B5E0-0C690EB3D4DB}']
    function GetName: string;
    procedure SetName(const Value: string);
    property Name: string read GetName write SetName;
  end;

  TNamedList<T: IInterface> = class(TList<T>)
  public
    procedure AddName(const Name: string; Item: T);
    procedure RemoveName(const Name: string);
    function FindName(const Name: string): T;
    procedure Remove(Item: T);
  end;

procedure TNamedList<T>.AddName(const Name: string; Item: T);
begin
  (Item as INamed).Name := Name;
  Add(Item);
end;

procedure TNamedList<T>.RemoveName(const Name: string);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if (Item[I] as INamed).Name = Name then
    begin
      Delete(I);
      Exit;
    end;
end;

function TNamedList<T>.FindName(const Name: string): T;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if (Item[I] as INamed).Name = Name then
      Exit(Item[I]);
  Result := Default(T);
end;

procedure TNamedList<T>.Remove(Item: T);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if PPointer(Direct[I])^ = PPointer(@Item)^ then
    begin
      Delete(I);
      Exit;
    end;
end;

{ Expands $(NAME) environment variables in a string }

function StrExpand(const S: string): string;
var
  I, J: Integer;
begin
  Result := S;
  I := Pos('$(', Result);
  while I > 0 do
  begin
    J := Pos(')', Result, I + 2);
    if J = 0 then
      Break;
    Result := Copy(Result, 1, I - 1) +
      GetEnvironmentVariable(Copy(Result, I + 2, J - I - 2)) +
      Copy(Result, J + 1, Length(Result));
    I := Pos('$(', Result, I);
  end;
end;

{ Color helpers }

function ToNVG(const C: TColorF): TNVGcolor; inline;
begin
  Result := nvgRGBAf(C.Red, C.Green, C.Blue, C.Alpha);
end;

function SameColor(const A, B: TColorF): Boolean; inline;
begin
  Result := (A.Red = B.Red) and (A.Green = B.Green) and (A.Blue = B.Blue) and (A.Alpha = B.Alpha);
end;

function MixPoint(const A, B: TPointF; Percent: Float): TPointF; inline;
begin
  Result.X := A.X + (B.X - A.X) * Percent;
  Result.Y := A.Y + (B.Y - A.Y) * Percent;
end;

{ Note: Repeat X and Y do not work with the GLESv2 path }

const
  NVG_IMAGE_DEFAULT = NVG_IMAGE_REPEATX or NVG_IMAGE_REPEATY;

type
  TCanvas = class;

  TGraphicsObject = class(TInterfacedObject)
    Changed: Boolean;
    function IsChanged: Boolean; virtual;
  end;

  TMatrix = class(TGraphicsObject, IMatrix)
  public
    Data: TNVGxform;
    Stack: IMatrix;
    constructor Create;
    destructor Destroy; override;
    procedure Copy(A, B, C, D, E, F: Float); overload;
    procedure Copy(M: IMatrix); overload;
    procedure Identity;
    function Inverse: IMatrix;
    procedure Translate(X, Y: Float);
    procedure Rotate(Angle: Float);
    procedure RotateAt(Angle, X, Y: Float);
    procedure Scale(SX, SY: Float);
    procedure ScaleAt(SX, SY, X, Y: Float);
    procedure SkewX(X: Float);
    procedure SkewY(Y: Float);
    procedure Transform(M: IMatrix);
    function Multiply(M: IMatrix): IMatrix; overload;
    function Multiply(const P: TPointF): TPointF; overload;
    procedure Push;
    procedure Pop;
  end;

  TBitmap = class(TGraphicsObject, IBitmap, INamed)
  public
    Id: Integer;
    Name: string;
    Width: LongWord;
    Height: LongWord;
    function GetName: string;
    procedure SetName(const Value: string);
    function GetClientRect: TRectF;
    function GetWidth: LongWord;
    function GetHeight: LongWord;
  end;

  TRenderBitmap = class(TGraphicsObject, IBitmap, IRenderBitmap, INamed)
  public
    Id: PNVGLUframebuffer;
    Name: string;
    Width: LongWord;
    Height: LongWord;
    Canvas: TCanvas;
    { The viewport and frame state saved by Bind and restored by Unbind }
    Viewport: array[0..3] of GLint;
    WasDrawing: Boolean;
    function GetName: string;
    procedure SetName(const Value: string);
    function GetClientRect: TRectF;
    function GetWidth: LongWord;
    function GetHeight: LongWord;
    procedure Bind;
    procedure Unbind;
    procedure Resize(W, H: LongWord);
  end;

  TFont = class(TGraphicsObject, IFont, INamed)
  public
    Id: Integer;
    Name: string;
    Color: TColorF;
    Size: Float;
    Align: TFontAlign;
    Layout: TFontLayout;
    Blur: Float;
    LetterSpacing: Float;
    LineSpacing: Float;
    constructor Create;
    destructor Destroy; override;
    function GetName: string;
    procedure SetName(const Value: string);
    function GetColor: TColorF;
    procedure SetColor(const Value: TColorF);
    function GetSize: Float;
    procedure SetSize(Value: Float);
    function GetHeight: Float;
    procedure SetHeight(Value: Float);
    function GetAlign: TFontAlign;
    procedure SetAlign(const Value: TFontAlign);
    function GetLayout: TFontLayout;
    procedure SetLayout(const Value: TFontLayout);
    function GetBlur: Float;
    procedure SetBlur(Value: Float);
    function GetLetterSpacing: Float;
    procedure SetLetterSpacing(Value: Float);
    function GetLineSpacing: Float;
    procedure SetLineSpacing(Value: Float);
  end;

  TPen = class(TGraphicsObject, IPen)
  public
    Color: TColorF;
    Brush: IBrush;
    Width: Float;
    MiterLimit: Float;
    LineCap: TLineCap;
    LineJoin: TLineJoin;
    constructor Create;
    function IsChanged: Boolean; override;
    function GetColor: TColorF;
    procedure SetColor(const Value: TColorF);
    function GetBrush: IBrush;
    procedure SetBrush(Value: IBrush);
    function GetWidth: Float;
    procedure SetWidth(Value: Float);
    function GetMiterLimit: Float;
    procedure SetMiterLimit(Value: Float);
    function GetLineCap: TLineCap;
    procedure SetLineCap(const Value: TLineCap);
    function GetLineJoin: TLineJoin;
    procedure SetLineJoin(const Value: TLineJoin);
  end;

  TBrush = class(TGraphicsObject, IBrush)
  end;

  TSolidBrush = class(TBrush, ISolidBrush)
  public
    Color: TColorF;
    function GetColor: TColorF;
    procedure SetColor(const Value: TColorF);
  end;

  TPaintBrush = class(TBrush)
  public
    Paint: TNVGpaint;
    procedure Build(Ctx: PNVGcontext); virtual; abstract;
  end;

  TGradientStop = class(TGraphicsObject, IGradientStop)
  public
    Offset: Float;
    Color: TColorF;
    constructor Create;
    function GetOffset: Float;
    procedure SetOffset(Value: Float);
    function GetColor: TColorF;
    procedure SetColor(const Value: TColorF);
  end;

  TGradientBrush = class(TPaintBrush, IGradientBrush)
  public
    NearStop: IGradientStop;
    FarStop: IGradientStop;
    constructor Create;
    function IsChanged: Boolean; override;
    function GetNearStop: IGradientStop;
    function GetFarStop: IGradientStop;
  end;

  TLinearGradientBrush = class(TGradientBrush, ILinearGradientBrush)
  public
    A: TPointF;
    B: TPointF;
    procedure Build(Ctx: PNVGcontext); override;
    function GetA: TPointF;
    procedure SetA(const Value: TPointF);
    function GetB: TPointF;
    procedure SetB(const Value: TPointF);
  end;

  TRadialGradientBrush = class(TGradientBrush, IRadialGradientBrush)
  public
    Rect: TRectF;
    procedure Build(Ctx: PNVGcontext); override;
    function GetRect: TRectF;
    procedure SetRect(const Value: TRectF);
  end;

  TBoxGradientBrush = class(TGradientBrush, IBoxGradientBrush)
  public
    Rect: TRectF;
    Radius: Float;
    Feather: Float;
    procedure Build(Ctx: PNVGcontext); override;
    function GetRect: TRectF;
    procedure SetRect(const Value: TRectF);
    function GetRadius: Float;
    procedure SetRadius(Value: Float);
    function GetFeather: Float;
    procedure SetFeather(Value: Float);
  end;

  TBitmapBrush = class(TPaintBrush, IBitmapBrush)
  public
    Bitmap: IBitmap;
    Angle: Float;
    Offset: TPointF;
    Scale: TPointF;
    Opacity: Float;
    constructor Create;
    procedure Build(Ctx: PNVGcontext); override;
    function GetBitmap: IBitmap;
    procedure SetBitmap(Value: IBitmap);
    function GetAngle: Float;
    procedure SetAngle(Value: Float);
    function GetOffset: TPointF;
    procedure SetOffset(const Value: TPointF);
    function GetScale: TPointF;
    procedure SetScale(const Value: TPointF);
    function GetOpacity: Float;
    procedure SetOpacity(Value: Float);
  end;

  TSprite = class(TGraphicsObject, ISprite)
  public
    Bitmap: IBitmap;
    Cols: Integer;
    Rows: Integer;
    Cell: Integer;
    Pivot: TVec2;
    Position: TVec2;
    Rotation: Single;
    Scale: TVec2;
    Opacity: Single;
    constructor Create;
    function GetBitmap: IBitmap;
    procedure SetBitmap(Value: IBitmap);
    function GetCols: Integer;
    procedure SetCols(Value: Integer);
    function GetRows: Integer;
    procedure SetRows(Value: Integer);
    function GetCell: Integer;
    procedure SetCell(Value: Integer);
    function GetPivot: TVec2;
    procedure SetPivot(Value: TVec2);
    function GetPosition: TVec2;
    procedure SetPosition(Value: TVec2);
    function GetRotation: Single;
    procedure SetRotation(Value: Single);
    function GetScale: TVec2;
    procedure SetScale(Value: TVec2);
    function GetOpacity: Single;
    procedure SetOpacity(Value: Single);
  end;

  TTextWriter = class(TGraphicsObject, ITextWriter)
  public
    Font: IFont;
    Canvas: ICanvas;
    Area: TRectF;
    Page: TRectF;
    RowHeight: Single;
    Shadow: Boolean;
    constructor Create(C: ICanvas; F: IFont);
    function GetShadow: Boolean;
    procedure SetShadow(Value: Boolean);
    procedure Paper(const Rect: TRectF);
    procedure Indent(Margin: Single);
    procedure NewPage;
    procedure NewLine;
    procedure Write(const S: string; Layout: TTextLayout = textLeft);
    procedure WriteAt(const S: string; X, Y: Single);
    procedure WriteColumn(const S: string; Column: Single; Layout: TTextLayout = textLeft);
    procedure WriteLine(const S: string);
  end;

  { TCanvasClip is a rectangle passed to Clip and the transform it was placed
    with. The clips in effect are kept so that Pop can put the scissor back
    exactly, including clips from outer pushes. }
  TCanvasClip = record
    Rect: TRectF;
    Transform: TNVGxform;
  end;

  TCanvasStack = class
    BlendMode: TBlendMode;
    ClipStart: Integer;
    ClipCount: Integer;
    Opacity: Float;
    Winding: TWinding;
    FillRule: TFillRule;
    Stack: TCanvasStack;
  end;

  TBitmaps = class(TNamedList<IBitmap>) end;
  TRenderBitmaps = class(TNamedList<IRenderBitmap>) end;
  TFonts = class(TNamedList<IFont>) end;

  TCanvas = class(TInterfacedObject, IResourceStore, ICanvas, IBackBuffer)
  public
    Ctx: PNVGcontext;
    Stack: TCanvasStack;
    Width, Height: Integer;
    FrontFacing: Boolean;
    Bitmaps: TBitmaps;
    RenderBitmaps: TRenderBitmaps;
    Fonts: TFonts;
    RenderBitmap: IRenderBitmap;
    SolidBrush: ISolidBrush;
    SolidPen: IPen;
    LastPen: IPen;
    LastBrush: IBrush;
    LastFont: IFont;
    { Clips from ClipStart to ClipCount - 1 make up the scissor in effect }
    Clips: array of TCanvasClip;
    ClipStart: Integer;
    ClipCount: Integer;
    BlendMode: TBlendMode;
    Opacity: Float;
    Winding: TWinding;
    FillRule: TFillRule;
    Matrix: IMatrix;
    M: TMatrix;
    constructor Create;
    destructor Destroy; override;
    procedure CheckMatrix;
    procedure ApplyClips;
    procedure SelectObject(Pen: IPen); overload;
    procedure SelectObject(Brush: IBrush); overload;
    procedure SelectObject(Font: IFont); overload;
    { IBackBuffer }
    procedure BeginFrame;
    procedure EndFrame;
    procedure Flip(W, H: Integer);
    { IResourceStore }
    function NewTextWriter(Font: IFont): ITextWriter; overload;
    function NewTextWriter(Font: IFont; Page: TRectF; Margin: TVec2): ITextWriter; overload;
    function NewBitmap(const Name: string; Width, Height: LongWord): IRenderBitmap; overload;
    function NewBitmap(Id: Integer; const Name: string): IBitmap; overload;
    procedure DisposeBitmap(Bitmap: IBitmap);
    function LoadBitmap(const Name: string): IBitmap; overload;
    function LoadBitmap(const Name: string; FileName: string;
      Mipmaps: Boolean = False): IBitmap; overload;
    function LoadBitmap(const Name: string; Memory: Pointer; Size: LongWord;
      Mipmaps: Boolean = False): IBitmap; overload;
    function LoadBitmapResource(const Name, ResName: string;
      Mipmaps: Boolean = False): IBitmap;
    function CloneFont(Font: IFont): IFont;
    function NewFont(Id: Integer; const Name: string): IFont;
    function LoadFont(const Name: string): IFont; overload;
    function LoadFont(const Name: string; FileName: string): IFont; overload;
    function LoadFont(const Name: string; Memory: Pointer; Size: LongWord): IFont; overload;
    function LoadFontResource(const Name, ResName: string): IFont;
    { ICanvas }
    procedure Push;
    procedure Pop;
    procedure Clip(const Rect: TRectF); overload;
    procedure Clip; overload;
    procedure Clear;
    function MeasureText(Font: IFont; const Text: string): TPointF;
    function MeasureAdvance(Font: IFont; const Text: string): Float;
    function MeasureMemo(Font: IFont; const Text: string; Width: Float): Float;
    procedure DrawText(Font: IFont; const Text: string; X, Y: Float);
    procedure DrawTextMemo(Font: IFont; const Text: string; X, Y, Width: Float);
    procedure DrawImage(Image: IBitmap; X, Y: Float; Opacity: Float = 1; Angle: Float = 0); overload;
    procedure DrawImage(Image: IBitmap; Source, Dest: TRectF; Opacity: Float = 1; Angle: Float = 0); overload;
    procedure DrawSprite(Sprite: ISprite);
    procedure BeginPath;
    procedure MoveTo(X, Y: Float);
    procedure LineTo(X, Y: Float);
    procedure ArcTo(X1, Y1, X2, Y2, Radius: Float);
    procedure BezierTo(CX1, CY1, CX2, CY2, X, Y: Float);
    procedure QuadTo(CX, CY, X, Y: Float);
    procedure Arc(CX, CY, Radius, A0, A1: Float; Clockwise: Boolean);
    procedure Circle(X, Y, Radius: Float);
    procedure Ellipse(const R: TRectF); overload;
    procedure Ellipse(X, Y, Width, Height: Float); overload;
    procedure Rect(const R: TRectF); overload;
    procedure Rect(X, Y, Width, Height: Float); overload;
    procedure RoundRect(const R: TRectF; Radius: Float); overload;
    procedure RoundRect(X, Y, Width, Height: Float; Radius: Float); overload;
    procedure RoundRectVarying(const R: TRectF; TL, TR, BR, BL: Float);
    procedure Polygon(P: PPointF; Count: Integer; Closed: Boolean = True);
    procedure ClosePath;
    procedure Fill(Brush: IBrush; Preserve: Boolean = False); overload;
    procedure Fill(Color: TColorF; Preserve: Boolean = False); overload;
    procedure Stroke(Pen: IPen; Preserve: Boolean = False); overload;
    procedure Stroke(Color: TColorF; Width: Float = 1; Preserve: Boolean = False); overload;
    procedure FillCircle(Brush: IBrush; X, Y, Radius: Float);
    procedure StrokeCircle(Pen: IPen; X, Y, Radius: Float);
    procedure FillRect(Brush: IBrush; const R: TRectF);
    procedure StrokeRect(Pen: IPen; const R: TRectF);
    procedure FillRoundRect(Brush: IBrush; const R: TRectF; Radius: Float);
    procedure StrokeRoundRect(Pen: IPen; const R: TRectF; Radius: Float);
    function GetGontext: Pointer;
    function GetBlendMode: TBlendMode;
    procedure SetBlendMode(Value: TBlendMode);
    function GetOpacity: Float;
    procedure SetOpacity(Value: Float);
    function GetMatrix: IMatrix;
    procedure SetMatrix(Value: IMatrix);
    function GetWinding: TWinding;
    procedure SetWinding(const Value: TWinding);
    function GetFillRule: TFillRule;
    procedure SetFillRule(const Value: TFillRule);
  end;

function IsBound(Bitmap: IBitmap): Boolean;
begin
  if Bitmap is IRenderBitmap then
    Result := (Bitmap as TRenderBitmap).Canvas.RenderBitmap = Bitmap as IRenderBitmap
  else
    Result := False;
end;

{ TGraphicsObject }

function TGraphicsObject.IsChanged: Boolean;
begin
  Result := Changed;
  Changed := False;
end;

{ TMatrix }

constructor TMatrix.Create;
begin
  inherited Create;
  Identity;
  Inc(MatrixCreated);
end;

destructor TMatrix.Destroy;
begin
  Inc(MatrixDestroyed);
  inherited Destroy;
end;

procedure TMatrix.Copy(A, B, C, D, E, F: Float);
begin
  Data[0] := A; Data[1] := B; Data[2] := C;
  Data[3] := D; Data[4] := E; Data[5] := F;
  Changed := True;
end;

procedure TMatrix.Copy(M: IMatrix);
var
  A: TMatrix;
begin
  A := M as TMatrix;
  if A = Self then
    Exit;
  Data := A.Data;
  Changed := True;
end;

procedure TMatrix.Identity;
begin
  nvgTransformIdentity(Data);
  Changed := True;
end;

function TMatrix.Inverse: IMatrix;
var
  M: TMatrix;
begin
  Result := NewMatrix;
  M := Result as TMatrix;
  nvgTransformInverse(M.Data, Data);
end;

procedure TMatrix.Translate(X, Y: Float);
var
  M: TNVGxform;
begin
  nvgTransformTranslate(M, X, Y);
  nvgTransformMultiply(Data, M);
  Changed := True;
end;

procedure TMatrix.Rotate(Angle: Float);
var
  M: TNVGxform;
begin
  nvgTransformRotate(M, Angle);
  nvgTransformMultiply(Data, M);
  Changed := True;
end;

procedure TMatrix.RotateAt(Angle, X, Y: Float);
begin
  Translate(-X, -Y);
  Rotate(Angle);
  Translate(X, Y);
end;

procedure TMatrix.Scale(SX, SY: Float);
var
  M: TNVGxform;
begin
  nvgTransformScale(M, SX, SY);
  nvgTransformMultiply(Data, M);
  Changed := True;
end;

procedure TMatrix.ScaleAt(SX, SY, X, Y: Float);
begin
  Translate(-X, -Y);
  Scale(SX, SY);
  Translate(X, Y);
end;

procedure TMatrix.SkewX(X: Float);
var
  M: TNVGxform;
begin
  nvgTransformSkewX(M, X);
  nvgTransformMultiply(Data, M);
  Changed := True;
end;

procedure TMatrix.SkewY(Y: Float);
var
  M: TNVGxform;
begin
  nvgTransformSkewY(M, Y);
  nvgTransformMultiply(Data, M);
  Changed := True;
end;

procedure TMatrix.Transform(M: IMatrix);
var
  Mat: TMatrix;
begin
  Mat := M as TMatrix;
  nvgTransformMultiply(Data, Mat.Data);
  Changed := True;
end;

function TMatrix.Multiply(M: IMatrix): IMatrix;
var
  B: TMatrix;
  C: TMatrix;
begin
  B := M as TMatrix;
  Result := NewMatrix;
  C := Result as TMatrix;
  C.Data := Data;
  nvgTransformMultiply(C.Data, B.Data);
end;

function TMatrix.Multiply(const P: TPointF): TPointF;
begin
  nvgTransformPoint(Result.X, Result.Y, Data, P.X, P.Y);
end;

procedure TMatrix.Push;
var
  S: IMatrix;
  M: TMatrix;
begin
  Inc(MatrixPushPop);
  S := NewMatrix;
  M := S as TMatrix;
  M.Data := Data;
  M.Stack := Stack;
  Stack := S;
end;

procedure TMatrix.Pop;
var
  S: IMatrix;
  M: TMatrix;
begin
  Dec(MatrixPushPop);
  S := Stack;
  if S = nil then
    Exit;
  M := S as TMatrix;
  Data := M.Data;
  Stack := M.Stack;
  Changed := True;
end;

{ TBitmap }

function TBitmap.GetName: string;
begin
  Result := Name;
end;

procedure TBitmap.SetName(const Value: string);
begin
  if Name = '' then
    Name := Value;
end;

function TBitmap.GetClientRect: TRectF;
begin
  Result := {%H-}NewRectF(Width, Height);
end;

function TBitmap.GetWidth: LongWord;
begin
  Result := Width;
end;

function TBitmap.GetHeight: LongWord;
begin
  Result := Height;
end;

{ TRenderBitmap }

function TRenderBitmap.GetName: string;
begin
  Result := Name;
end;

procedure TRenderBitmap.SetName(const Value: string);
begin
  if Name = '' then
    Name := Value;
end;

function TRenderBitmap.GetClientRect: TRectF;
begin
  Result := {%H-}NewRectF(Width, Height);
end;

function TRenderBitmap.GetWidth: LongWord;
begin
  Result := Width;
end;

function TRenderBitmap.GetHeight: LongWord;
begin
  Result := Height;
end;

procedure TRenderBitmap.Bind;
var
  C: TCanvas;
begin
  if IsBound(Self) then
    Exit;
  C := Canvas;
  if IsChanged then
  begin
    nvgluDeleteFramebuffer(Id);
    Id := nvgluCreateFramebuffer(C.Ctx, Width, Height, NVG_IMAGE_DEFAULT);
  end;
  { A bitmap can be bound inside or outside of a canvas frame. The frame and
    viewport are restored by Unbind. The device pixel ratio is always 1, the
    same as canvas frames use. }
  glGetIntegerv(GL_VIEWPORT, @Viewport[0]);
  WasDrawing := C.FrontFacing;
  if WasDrawing then
    nvgEndFrame(C.Ctx);
  nvgluBindFramebuffer(Id);
  glViewport(0, 0, Width, Height);
  nvgBeginFrame(C.Ctx, Width, Height, 1);
  Canvas.Push;
  { The new frame has no scissor, and clips of the back buffer do not apply
    to the bitmap. Unbind pops them back. }
  C.ClipStart := C.ClipCount;
  C.LastPen := nil;
  C.LastBrush := nil;
  C.LastFont := nil;
  C.RenderBitmap := Self;
end;

procedure TRenderBitmap.Unbind;
var
  Ctx: PNVGcontext;
  C: TCanvas;
begin
  if not IsBound(Self) then
    Exit;
  Ctx := Canvas.Ctx;
  nvgEndFrame(Ctx);
  nvgluBindFramebuffer(nil);
  glViewport(Viewport[0], Viewport[1], Viewport[2], Viewport[3]);
  if WasDrawing then
  begin
    nvgBeginFrame(Ctx, Canvas.Width, Canvas.Height, 1);
    nvgBeginPath(Ctx);
  end;
  Canvas.Pop;
  C := Canvas as TCanvas;
  C.LastPen := nil;
  C.LastBrush := nil;
  C.LastFont := nil;
  C.RenderBitmap := nil;
end;

procedure TRenderBitmap.Resize(W, H: LongWord);
begin
  if IsBound(Self) then
    Exit;
  if (W = Width) and (H = Height) then
    Exit;
  Width := W;
  Height := H;
  Changed := True;
end;

{ TFont }

constructor TFont.Create;
begin
  inherited Create;
  Inc(FontsCreated);
  Color := ColorWhite;
  Size := 20;
  LetterSpacing := 0;
  LineSpacing := 1;
end;

destructor TFont.Destroy;
begin
  Inc(FontsDestroyed);
  inherited Destroy;
end;

function TFont.GetName: string;
begin
  Result := Name;
end;

procedure TFont.SetName(const Value: string);
begin
  if Name = '' then
    Name := Value;
end;

function TFont.GetColor: TColorF;
begin
  Result := Color;
end;

procedure TFont.SetColor(const Value: TColorF);
begin
  if SameColor(Value, Color) then Exit;
  Color := Value;
  Changed := True;
end;

function TFont.GetSize: Float;
begin
  Result := Size;
end;

procedure TFont.SetSize(Value: Float);
begin
  if Value < 0 then Value := 0;
  if Value = Size then Exit;
  Size := Value;
  Changed := True;
end;

function TFont.GetHeight: Float;
begin
  Result := Size / 96 * 72;
end;

procedure TFont.SetHeight(Value: Float);
begin
  Value := Value / 72 * 96;
  if Value = Size then Exit;
  Size := Value;
  Changed := True;
end;

function TFont.GetAlign: TFontAlign;
begin
  Result := Align;
end;

procedure TFont.SetAlign(const Value: TFontAlign);
begin
  if Value = Align then Exit;
  Align := Value;
  Changed := True;
end;

function TFont.GetLayout: TFontLayout;
begin
  Result := Layout;
end;

procedure TFont.SetLayout(const Value: TFontLayout);
begin
  if Value = Layout then Exit;
  Layout := Value;
  Changed := True;
end;

function TFont.GetBlur: Float;
begin
  Result := Blur;
end;

procedure TFont.SetBlur(Value: Float);
begin
  if Value < 0 then Value := 0;
  if Value = Blur then Exit;
  Blur := Value;
  Changed := True;
end;

function TFont.GetLetterSpacing: Float;
begin
  Result := LetterSpacing;
end;

procedure TFont.SetLetterSpacing(Value: Float);
begin
  if Value = LetterSpacing then Exit;
  LetterSpacing := Value;
  Changed := True;
end;

function TFont.GetLineSpacing: Float;
begin
  Result := LineSpacing;
end;

procedure TFont.SetLineSpacing(Value: Float);
begin
  if Value = LineSpacing then Exit;
  LineSpacing := Value;
  Changed := True;
end;

{ TPen }

constructor TPen.Create;
begin
  inherited Create;
  Color := ColorBlack;
  Width := 1;
  MiterLimit := 10;
end;

function TPen.IsChanged: Boolean;
begin
  Result := inherited IsChanged;
  if (Brush <> nil) and (Brush as TBrush).IsChanged then
    Result := True;
end;

function TPen.GetColor: TColorF;
begin
  Result := Color;
end;

procedure TPen.SetColor(const Value: TColorF);
begin
  if SameColor(Value, Color) then Exit;
  Color := Value;
  Changed := True;
end;

function TPen.GetBrush: IBrush;
begin
  Result := Brush;
end;

procedure TPen.SetBrush(Value: IBrush);
begin
  if Value = Brush then Exit;
  Brush := Value;
  Changed := True;
end;

function TPen.GetWidth: Float;
begin
  Result := Width;
end;

procedure TPen.SetWidth(Value: Float);
begin
  if Value = Width then Exit;
  Width := Value;
  Changed := True;
end;

function TPen.GetMiterLimit: Float;
begin
  Result := MiterLimit;
end;

procedure TPen.SetMiterLimit(Value: Float);
begin
  if Value = MiterLimit then Exit;
  MiterLimit := Value;
  Changed := True;
end;

function TPen.GetLineCap: TLineCap;
begin
  Result := LineCap;
end;

procedure TPen.SetLineCap(const Value: TLineCap);
begin
  if Value = LineCap then Exit;
  LineCap := Value;
  Changed := True;
end;

function TPen.GetLineJoin: TLineJoin;
begin
  Result := LineJoin;
end;

procedure TPen.SetLineJoin(const Value: TLineJoin);
begin
  if Value = LineJoin then Exit;
  LineJoin := Value;
  Changed := True;
end;

{ TBrush }

function TSolidBrush.GetColor: TColorF;
begin
  Result := Color;
end;

procedure TSolidBrush.SetColor(const Value: TColorF);
begin
  if SameColor(Value, Color) then Exit;
  Color := Value;
  Changed := True;
end;

{ TGradientStop }

constructor TGradientStop.Create;
begin
  inherited Create;
  Color := ColorBlack;
  Changed := True;
end;

function TGradientStop.GetOffset: Float;
begin
  Result := Offset;
end;

procedure TGradientStop.SetOffset(Value: Float);
begin
  if Value = Offset then Exit;
  Offset := Value;
  Changed := True;
end;

function TGradientStop.GetColor: TColorF;
begin
  Result := Color;
end;

procedure TGradientStop.SetColor(const Value: TColorF);
begin
  if SameColor(Value, Color) then Exit;
  Color := Value;
  Changed := True;
end;

{ TGradientBrush }

constructor TGradientBrush.Create;
begin
  inherited Create;
  NearStop := TGradientStop.Create;
  FarStop := TGradientStop.Create;
  FarStop.Offset := 1;
  FarStop.Color := ColorWhite;
end;

function TGradientBrush.IsChanged: Boolean;
begin
  Result := inherited IsChanged;
  if (NearStop as TGradientStop).IsChanged then
    Result := True;
  if (FarStop as TGradientStop).IsChanged then
    Result := True;
end;

function TGradientBrush.GetNearStop: IGradientStop;
begin
  Result := NearStop;
end;

function TGradientBrush.GetFarStop: IGradientStop;
begin
  Result := FarStop;
end;

{ TLinearGradientBrush }

procedure TLinearGradientBrush.Build(Ctx: PNVGcontext);
var
  C, D: TPointF;
begin
  C := MixPoint(A, B, NearStop.Offset);
  D := MixPoint(A, B, FarStop.Offset);
  Paint := nvgLinearGradient(Ctx, C.X, C.Y, D.X, D.Y, ToNVG(NearStop.Color), ToNVG(FarStop.Color));
end;

function TLinearGradientBrush.GetA: TPointF;
begin
  Result := A;
end;

procedure TLinearGradientBrush.SetA(const Value: TPointF);
begin
  A := Value;
  Changed := True;
end;

function TLinearGradientBrush.GetB: TPointF;
begin
  Result := B;
end;

procedure TLinearGradientBrush.SetB(const Value: TPointF);
begin
  B := Value;
  Changed := True;
end;

{ TRadialGradientBrush }

procedure TRadialGradientBrush.Build(Ctx: PNVGcontext);
var
  R: Float;
begin
  R := Rect.Width;
  if Rect.Height < R then
    R := Rect.Height;
  Paint := nvgRadialGradient(Ctx, Rect.X + Rect.Width / 2, Rect.Y + Rect.Height / 2,
    R * NearStop.Offset, R * FarStop.Offset, ToNVG(NearStop.Color), ToNVG(FarStop.Color));
end;

function TRadialGradientBrush.GetRect: TRectF;
begin
  Result := Rect;
end;

procedure TRadialGradientBrush.SetRect(const Value: TRectF);
begin
  Rect := Value;
  Changed := True;
end;

{ TBoxGradientBrush }

procedure TBoxGradientBrush.Build(Ctx: PNVGcontext);
begin
  Paint := nvgBoxGradient(Ctx, Rect.X, Rect.Y, Rect.Width, Rect.Height, Radius, Feather,
    ToNVG(NearStop.Color), ToNVG(FarStop.Color));
end;

function TBoxGradientBrush.GetRect: TRectF;
begin
  Result := Rect;
end;

procedure TBoxGradientBrush.SetRect(const Value: TRectF);
begin
  Rect := Value;
  Changed := True;
end;

function TBoxGradientBrush.GetRadius: Float;
begin
  Result := Radius;
end;

procedure TBoxGradientBrush.SetRadius(Value: Float);
begin
  if Value < 0 then Value := 0;
  if Value = Radius then Exit;
  Radius := Value;
  Changed := True;
end;

function TBoxGradientBrush.GetFeather: Float;
begin
  Result := Feather;
end;

procedure TBoxGradientBrush.SetFeather(Value: Float);
begin
  if Value < 0 then Value := 0;
  if Value = Feather then Exit;
  Feather := Value;
  Changed := True;
end;

{ TBitmapBrush }

constructor TBitmapBrush.Create;
begin
  inherited Create;
  Scale := {%H-}NewPointF(1, 1);
  Opacity := 1;
end;

function GetBitmapId(Bitmap: IBitmap): Integer;
var
  A: TBitmap;
  B: TRenderBitmap;
begin
  if Bitmap is TBitmap then
  begin
    A := Bitmap as TBitmap;
    Result := A.Id;
  end
  else
  begin
    B := Bitmap as TRenderBitmap;
    Result := B.Id.Image;
  end;
end;

procedure TBitmapBrush.Build(Ctx: PNVGcontext);
var
  Id: Integer;
begin
  Id := GetBitmapId(Bitmap);
  if (Id < 0) or IsBound(Bitmap) then
  begin
    Paint := nvgLinearGradient(Ctx, 0, 0, 10, 10, nvgRGB(0, 0, 0), nvgRGB(0, 0, 0));
    Changed := True;
  end
  else
    Paint := nvgImagePattern(Ctx, Offset.X, Offset.Y, Bitmap.Width * Scale.X,
      Bitmap.Height * Scale.Y, Angle, Id, Opacity);
end;

function TBitmapBrush.GetBitmap: IBitmap;
begin
  Result := Bitmap;
end;

procedure TBitmapBrush.SetBitmap(Value: IBitmap);
begin
  if Value = Bitmap then Exit;
  Bitmap := Value;
  Changed := True;
end;

function TBitmapBrush.GetAngle: Float;
begin
  Result := Angle;
end;

procedure TBitmapBrush.SetAngle(Value: Float);
begin
  if Value = Angle then Exit;
  Angle := Value;
  Changed := True;
end;

function TBitmapBrush.GetOffset: TPointF;
begin
  Result := Offset;
end;

procedure TBitmapBrush.SetOffset(const Value: TPointF);
begin
  Offset := Value;
  Changed := True;
end;

function TBitmapBrush.GetScale: TPointF;
begin
  Result := Scale;
end;

procedure TBitmapBrush.SetScale(const Value: TPointF);
begin
  Scale := Value;
  Changed := True;
end;

function TBitmapBrush.GetOpacity: Float;
begin
  Result := Opacity;
end;

procedure TBitmapBrush.SetOpacity(Value: Float);
begin
  if Value = Opacity then Exit;
  Opacity := Value;
  Changed := True;
end;

{ TSprite }

constructor TSprite.Create;
begin
  inherited Create;
  Cols := 1;
  Rows := 1;
  Pivot := Vec(0.5, 0.5);
  Scale := Vec(1, 1);
  Opacity := 1;
end;

function TSprite.GetBitmap: IBitmap;
begin
  Result := Bitmap;
end;

procedure TSprite.SetBitmap(Value: IBitmap);
begin
  if Value = Bitmap then Exit;
  Bitmap := Value;
end;

function TSprite.GetCols: Integer;
begin
  Result := Cols;
end;

procedure TSprite.SetCols(Value: Integer);
begin
  if Value < 0 then Value := 1;
  Cols := Value;
end;

function TSprite.GetRows: Integer;
begin
  Result := Rows;
end;

procedure TSprite.SetRows(Value: Integer);
begin
  if Value < 0 then Value := 1;
  Rows := Value;
end;

function TSprite.GetCell: Integer;
begin
  Result := Cell;
end;

procedure TSprite.SetCell(Value: Integer);
begin
  if Value < 0 then Value := 0;
  Cell := Value;
end;

function TSprite.GetPivot: TVec2;
begin
  Result := Pivot;
end;

procedure TSprite.SetPivot(Value: TVec2);
begin
  Value.x := Value.x;
  Value.y := Value.y;
  Pivot := Value;
end;

function TSprite.GetPosition: TVec2;
begin
  Result := Position;
end;

procedure TSprite.SetPosition(Value: TVec2);
begin
  Position := Value;
end;

function TSprite.GetRotation: Single;
begin
  Result := Rotation;
end;

procedure TSprite.SetRotation(Value: Single);
begin
  Rotation := Value;
end;

function TSprite.GetScale: TVec2;
begin
  Result := Scale;
end;

procedure TSprite.SetScale(Value: TVec2);
begin
  Scale := Value;
end;

function TSprite.GetOpacity: Single;
begin
  Result := Opacity;
end;

procedure TSprite.SetOpacity(Value: Single);
begin
  Value := Clamp(Value);
  Opacity := Value;
end;

{ TTextWriter }

constructor TTextWriter.Create(C: ICanvas; F: IFont);
begin
  Canvas := C;
  Font := F;
  Area := NewRectF(5000, 5000);
  RowHeight := Canvas.MeasureText(Font, 'Wg').Y * 1.25;
end;

function TTextWriter.GetShadow: Boolean;
begin
  Result := Shadow;
end;

procedure TTextWriter.SetShadow(Value: Boolean);
begin
  Shadow := Value;
end;

procedure TTextWriter.Paper(const Rect: TRectF);
begin
  Area := Rect;
  Page := Rect;
end;

procedure TTextWriter.Indent(Margin: Single);
begin
  Area.X := Area.X + Margin;
  Area.Width := Page.Width - Margin;
end;

procedure TTextWriter.NewPage;
begin
  Area := Page;
end;

procedure TTextWriter.NewLine;
begin
  Area.Height := Area.Height - RowHeight;
  Area.Y := Area.Y + RowHeight;
end;

procedure TTextWriter.Write(const S: string; Layout: TTextLayout = textLeft);

  procedure DrawShadow(Font: IFont; S: string; X, Y: Single);
  var
    C: TColorF;
  begin
    if Shadow then
    begin
      C := Font.Color;
      Font.Color := ColorBlack;
      Canvas.DrawText(Font, S, X + 1, Y + 1);
      Font.Color := C;
    end;
    Canvas.DrawText(Font, S, X, Y);
  end;

  procedure DrawShadowMemo(Font: IFont; S: string; X, Y, W: Single);
  var
    C: TColorF;
  begin
    if Shadow then
    begin
      C := Font.Color;
      Font.Color := ColorBlack;
      Canvas.DrawTextMemo(Font, S, X + 1, Y + 1, W);
      Font.Color := C;
    end;
    Canvas.DrawTextMemo(Font, S, X, Y, W);
  end;

var
  V: TVec2;
begin
  if Area.Height <= 0 then
    Exit;
  case Layout of
    textLeft:
      DrawShadow(Font, S, Area.X, Area.Y);
    textTop:
      begin
        V := Canvas.MeasureText(Font, S);
        V.X := Area.X + (Area.Width - V.X) / 2;
        DrawShadow(Font, S, V.X, Area.Y);
      end;
    textRight:
      begin
        V := Canvas.MeasureText(Font, S);
        V.X := Area.Right - V.X;
        DrawShadow(Font, S, V.X, Area.Y);
      end;
    textMiddleLeft:
      begin
        DrawShadow(Font, S, Area.X, Area.Y + (Area.Height - RowHeight) / 2);
      end;
    textBottom:
      begin
        V := Canvas.MeasureText(Font, S);
        V.X := Area.X + (Area.Width - V.X) / 2;
        DrawShadow(Font, S, V.X, Area.Bottom - V.Y);
      end;
    textMemo:
      begin
        DrawShadowMemo(Font, S, Area.X, Area.Y, Area.Width);
        Area.Y := Area.Y + Canvas.MeasureMemo(Font, S, Area.Width);
        Area.Height := Page.Bottom - Area.Y;
      end
  else
  end;
end;

procedure TTextWriter.WriteAt(const S: string; X, Y: Single);
begin
  Canvas.DrawText(Font, S, X, Y);
end;

procedure TTextWriter.WriteColumn(const S: string; Column: Single; Layout: TTextLayout = textLeft);
var
  A: TRectF;
begin
  A := Area;
  Area.X := A.X + A.Width * Column;
  Area.Width := A.Width - Area.X;
  Write(S, Layout);
  Area := A;
end;

procedure TTextWriter.WriteLine(const S: string);
begin
  Write(S);
  NewLine;
end;

{ TCanvas }

constructor TCanvas.Create;
begin
  inherited Create;
  { The NanoVG backend follows the OpenGL version selected in render.inc }
  Ctx := nvgCreateGL(NVG_ANTIALIAS or NVG_STENCIL_STROKES);
  Opacity := 1;
  Matrix := NewMatrix;
  M := Matrix as TMatrix;
  nvgPathWinding(Ctx, NVG_CCW);
  SolidBrush := NewBrush(ColorBlack);
  SolidPen := NewPen(ColorBlack, 1);
  Bitmaps := TBitmaps.Create;
  RenderBitmaps := TRenderBitmaps.Create;
  Fonts := TFonts.Create;
end;

destructor TCanvas.Destroy;
var
  A: IBitmap;
  B: IRenderBitmap;
begin
  LastBrush := nil;
  LastPen := nil;
  LastFont := nil;
  SolidBrush := nil;
  SolidPen := nil;
  if RenderBitmap <> nil then
    RenderBitmap.Unbind;
  RenderBitmap := nil;
  for A in Bitmaps do
    nvgDeleteImage(Ctx, (A as TBitmap).Id);
  for B in RenderBitmaps do
    nvgluDeleteFramebuffer((B as TRenderBitmap).Id);
  Bitmaps.Free;
  RenderBitmaps.Free;
  Fonts.Free;
  nvgDeleteGL(Ctx);
  inherited Destroy;
end;

procedure TCanvas.CheckMatrix;
begin
  if M.Changed then
  begin
    nvgResetTransform(Ctx);
    nvgTransform(Ctx, M.Data[0], M.Data[1], M.Data[2], M.Data[3], M.Data[4], M.Data[5]);
    M.Changed := False;
  end;
end;

{ Rebuild the scissor from the clips in effect, each with the transform it
  was placed with. The canvas matrix is applied again before the next draw. }
procedure TCanvas.ApplyClips;
var
  T: TNVGxform;
  R: TRectF;
  I: Integer;
begin
  nvgResetScissor(Ctx);
  if ClipCount = ClipStart then
    Exit;
  for I := ClipStart to ClipCount - 1 do
  begin
    T := Clips[I].Transform;
    R := Clips[I].Rect;
    nvgResetTransform(Ctx);
    nvgTransform(Ctx, T[0], T[1], T[2], T[3], T[4], T[5]);
    if I = ClipStart then
      nvgScissor(Ctx, R.X, R.Y, R.Width, R.Height)
    else
      nvgIntersectScissor(Ctx, R.X, R.Y, R.Width, R.Height);
  end;
  M.Changed := True;
end;

procedure TCanvas.SelectObject(Brush: IBrush);
var
  HasChanged: Boolean;
  Paint: TPaintBrush;
  B: TBrush;
begin
  HasChanged := Brush <> LastBrush;
  LastBrush := Brush;
  B := Brush as TBrush;
  if B.IsChanged then
    HasChanged := True;
  if not HasChanged then
    Exit;
  if B is TSolidBrush then
    nvgFillColor(Ctx, ToNVG((B as TSolidBrush).Color))
  else
  begin
    Paint := B as TPaintBrush;
    Paint.Build(Ctx);
    nvgFillPaint(Ctx, Paint.Paint);
  end;
end;

procedure TCanvas.SelectObject(Pen: IPen);
const
  Caps: array[TLineCap] of Integer = (NVG_BUTT, NVG_SQUARE, NVG_ROUND);
  Joins: array[TLineJoin] of Integer = (NVG_MITER, NVG_BEVEL, NVG_ROUND);
var
  HasChanged: Boolean;
  Paint: TPaintBrush;
  P: TPen;
begin
  HasChanged := Pen <> LastPen;
  LastPen := Pen;
  P := Pen as TPen;
  if P.IsChanged then
    HasChanged := True;
  if not HasChanged then
    Exit;
  nvgStrokeWidth(Ctx, P.Width);
  nvgLineCap(Ctx, Caps[P.LineCap]);
  nvgLineJoin(Ctx, Joins[P.LineJoin]);
  nvgMiterLimit(Ctx, P.MiterLimit);
  if P.Brush = nil then
    nvgStrokeColor(Ctx, ToNVG(P.Color))
  else if P.Brush is TSolidBrush then
    nvgStrokeColor(Ctx, ToNVG((P.Brush as TSolidBrush).Color))
  else
  begin
    Paint := P.Brush as TPaintBrush;
    Paint.Build(Ctx);
    nvgStrokePaint(Ctx, Paint.Paint);
  end;
end;

procedure TCanvas.SelectObject(Font: IFont);
const
  Aligns: array[TFontAlign] of Integer = (NVG_ALIGN_LEFT, NVG_ALIGN_CENTER,
    NVG_ALIGN_RIGHT);
  Layouts: array[TFontLayout] of Integer = (NVG_ALIGN_TOP, NVG_ALIGN_MIDDLE,
    NVG_ALIGN_BOTTOM, NVG_ALIGN_BASELINE);
var
  HasChanged: Boolean;
  F: TFont;
begin
  HasChanged := LastFont <> Font;
  LastFont := Font;
  F := Font as TFont;
  if F.IsChanged then
    HasChanged := True;
  if not HasChanged then
    Exit;
  nvgFontFaceId(Ctx, F.Id);
  nvgFontSize(Ctx, F.Size);
  nvgFontBlur(Ctx, F.Blur);
  nvgTextLetterSpacing(Ctx, F.LetterSpacing);
  nvgTextLineHeight(Ctx, F.LineSpacing);
  nvgTextAlign(Ctx, Aligns[F.Align] or Layouts[F.Layout]);
end;

{ IBackBuffer }

procedure TCanvas.BeginFrame;
begin
	nvgBeginFrame(Ctx, Width, Height, 1);
  nvgBeginPath(Ctx);
end;

procedure TCanvas.EndFrame;
begin
  nvgEndFrame(Ctx);
end;

procedure TCanvas.Flip(W, H: Integer);
begin
  if FrontFacing then
  begin
    if RenderBitmap <> nil then
      RenderBitmap.Unbind;
    nvgEndFrame(Ctx);
    LastPen := nil;
    LastBrush := nil;
    LastFont := nil;
    Opacity := 1;
    BlendMode := blendAlpha;
    ClipStart := 0;
    ClipCount := 0;
    M.Identity;
    M.Changed := False;
  end
  else
  begin
    nvgBeginFrame(Ctx, W, H, 1);
    nvgBeginPath(Ctx);
  end;
  FrontFacing := not FrontFacing;
  Width := W;
  Height := H;
end;

{ IResourceStore }

function AdjustPathDelimiter(const S: string): string;
var
  I: Integer;
begin
  Result := S;
  {$ifdef linux}
  for I := 1 to Length(Result) do
    if Result[I] = '\' then Result[I] := '/';
  {$else}
  for I := 1 to Length(S) do
    if Result[I] = '/' then Result[I] := '\';
  {$endif}
end;

function TCanvas.NewTextWriter(Font: IFont): ITextWriter;
begin
  Result := TTextWriter.Create(Self, Font);
end;

function TCanvas.NewTextWriter(Font: IFont; Page: TRectF; Margin: TVec2): ITextWriter;
begin
  Result := TTextWriter.Create(Self, Font);
  Page.Inflate(-Margin.X, -Margin.Y);
  Result.Paper(Page);
end;

function TCanvas.NewBitmap(const Name: string; Width, Height: LongWord): IRenderBitmap;
var
  Bitmap: IBitmap;
  R: TRenderBitmap;
begin
  Result := nil;
  if Name = '' then
    Exit;
  Bitmap := LoadBitmap(Name);
  if Bitmap <> nil then
  begin
    if Bitmap is IRenderBitmap then
    begin
      Result := Bitmap as IRenderBitmap;
      Result.Resize(Width, Height);
    end;
    Exit;
  end;
  Result := TRenderBitmap.Create;
  R := Result as TRenderBitmap;
  R.Id := nvgluCreateFramebuffer(Ctx, Width, Height, NVG_IMAGE_DEFAULT);
  R.Width := Width;
  R.Height := Height;
  R.Canvas := Self;
  R.Changed := False;
  RenderBitmaps.AddName(Name, Result);
end;

function TCanvas.NewBitmap(Id: Integer; const Name: string): IBitmap;
var
  R: TBitmap;
  W, H: Integer;
begin
  Result := nil;
  if Name = '' then
    Exit;
  Result := TBitmap.Create;
  R := Result as TBitmap;
  R.Id := Id;
  W := 0;
  H := 0;
  nvgImageSize(Ctx, R.Id, @W, @H);
  R.Width := W;
  R.Height := H;
  Bitmaps.AddName(Name, Result);
end;

function TCanvas.LoadBitmap(const Name: string): IBitmap;
var
  A: IBitmap;
  B: IRenderBitmap;
begin
  Result := nil;
  if Name = '' then
    Exit;
  A := Bitmaps.FindName(Name);
  if A <> nil then
    Exit(A);
  B := RenderBitmaps.FindName(Name);
  if B <> nil then
    Result := B as IBitmap;
end;

{ The flags of a new image, with mipmaps if they are wanted }

function BitmapFlags(Mipmaps: Boolean): Integer;
begin
  Result := NVG_IMAGE_DEFAULT;
  if Mipmaps then
    Result := Result or NVG_IMAGE_GENERATE_MIPMAPS;
end;

function TCanvas.LoadBitmap(const Name: string; FileName: string;
  Mipmaps: Boolean = False): IBitmap; overload;
var
  S: string;
begin
  FileName := StrExpand(FileName);
  Result := nil;
  if Name = '' then
    Exit;
  Result := LoadBitmap(Name);
  if Result = nil then
  begin
    S := AdjustPathDelimiter(FileName);
    Result := NewBitmap(nvgCreateImage(Ctx, S, BitmapFlags(Mipmaps)), Name);
  end;
end;

function TCanvas.LoadBitmap(const Name: string; Memory: Pointer; Size: LongWord;
  Mipmaps: Boolean = False): IBitmap; overload;
begin
  Result := nil;
  if Name = '' then
    Exit;
  Result := LoadBitmap(Name);
  if Result = nil then
    Result := NewBitmap(nvgCreateImageMem(Ctx, BitmapFlags(Mipmaps), Memory, Size), Name);
end;

function TCanvas.LoadBitmapResource(const Name, ResName: string;
  Mipmaps: Boolean = False): IBitmap;
var
  S: TResourceStream;
begin
  Result := nil;
  if Name = '' then
    Exit;
  Result := LoadBitmap(Name);
  if Result = nil then
	begin
		S := TResourceStream.Create(HINSTANCE, ResName, RT_RCDATA);
    try
      Result := NewBitmap(nvgCreateImageMem(Ctx, BitmapFlags(Mipmaps), S.Memory, S.Size), Name);
    finally
      S.Free;
    end;
  end;
end;

procedure TCanvas.DisposeBitmap(Bitmap: IBitmap);
begin
  if Bitmap is IRenderBitmap then
  begin
    nvgluDeleteFramebuffer((Bitmap as TRenderBitmap).Id);
    RenderBitmaps.Remove(Bitmap as IRenderBitmap)
  end
  else
  begin
    nvgDeleteImage(Ctx, (Bitmap as TBitmap).Id);
    Bitmaps.Remove(Bitmap);
  end;
end;

function TCanvas.CloneFont(Font: IFont): IFont;
var
  A, B: TFont;
begin
  Result := TFont.Create;
  A := Result as TFont;
  B := Font as TFont;
  A.Id := B.Id;
  A.Name := B.Name;
  A.Size := B.Size;
  A.Align := B.Align;
  A.Layout := B.Layout;
  A.Blur := B.Blur;
  A.LetterSpacing := B.LetterSpacing;
  A.LineSpacing := B.LineSpacing;
end;

function TCanvas.NewFont(Id: Integer; const Name: string): IFont;
var
  F: TFont;
begin
  Result := TFont.Create;
  F := Result as TFont;
  F.Id := Id;
  Fonts.AddName(Name, Result);
  Result := CloneFont(Result);
end;

function TCanvas.LoadFont(const Name: string): IFont; overload;
var
  F: IFont;
  S: string;
begin
  Result := nil;
  if Name = '' then
    Exit;
  F := Fonts.FindName(Name);
  if F = nil then
  begin
    S := AdjustPathDelimiter(Name);
    F := NewFont(nvgCreateFont(Ctx, Name, S), Name);
  end;
  Result := CloneFont(F);
end;

function TCanvas.LoadFont(const Name: string; FileName: string): IFont; overload;
var
  F: IFont;
  S: string;
begin
  FileName := StrExpand(FileName);
  Result := nil;
  if Name = '' then
    Exit;
  F := Fonts.FindName(Name);
  if F = nil then
  begin
    S := AdjustPathDelimiter(FileName);
    F := NewFont(nvgCreateFont(Ctx, Name, S), Name);
  end;
  Result := CloneFont(F);
end;

function TCanvas.LoadFont(const Name: string; Memory: Pointer; Size: LongWord): IFont; overload;
var
  F: IFont;
begin
  Result := nil;
  if Name = '' then
    Exit;
  F := Fonts.FindName(Name);
  if F = nil then
    F := NewFont(nvgCreateFontMem(Ctx, Name, Memory, Size, False), Name);
  Result := CloneFont(F);
end;

function TCanvas.LoadFontResource(const Name, ResName: string): IFont;
var
  F: IFont;
  S: TResourceStream;
begin
  Result := nil;
  if Name = '' then
    Exit;
  F := Fonts.FindName(Name);
  if F = nil then
	begin
		S := TResourceStream.Create(HINSTANCE, ResName, RT_RCDATA);
    try
      F := NewFont(nvgCreateFontMem(Ctx, Name, S.Memory, S.Size, False), Name);
    finally
      S.Free;
    end;
  end;
  Result := CloneFont(F);
end;

{ ICanvas }

procedure TCanvas.Push;
var
  S: TCanvasStack;
begin
  Matrix.Push;
  S := TCanvasStack.Create;
  S.BlendMode := BlendMode;
  S.ClipStart := ClipStart;
  S.ClipCount := ClipCount;
  S.Opacity := Opacity;
  S.Winding := Winding;
  S.FillRule := FillRule;
  S.Stack := Stack;
  Stack := S;
  BeginPath;
end;

procedure TCanvas.Pop;
var
  C: ICanvas;
  S: TCanvasStack;
begin
  if Stack = nil then
    Exit;
  Matrix.Pop;
  C := Self;
  S := Stack;
  C.BlendMode := S.BlendMode;
  { The scissor is only rebuilt when clipping changed since the push }
  if (ClipStart <> S.ClipStart) or (ClipCount <> S.ClipCount) then
  begin
    ClipStart := S.ClipStart;
    ClipCount := S.ClipCount;
    ApplyClips;
  end;
  C.Opacity := S.Opacity;
  C.Winding := S.Winding;
  C.FillRule := S.FillRule;
  Stack := S.Stack;
  S.Free;
  BeginPath;
end;

procedure TCanvas.Clip(const Rect: TRectF); overload;
begin
  { The scissor is placed with the current transform, so a changed matrix
    is applied first }
  if Rect.Empty then
  begin
    Clip;
    Exit;
  end;
  CheckMatrix;
  if ClipCount = ClipStart then
    nvgScissor(Ctx, Rect.X, Rect.Y, Rect.Width, Rect.Height)
  else
    nvgIntersectScissor(Ctx, Rect.X, Rect.Y, Rect.Width, Rect.Height);
  if ClipCount = Length(Clips) then
    SetLength(Clips, ClipCount * 2 + 4);
  Clips[ClipCount].Rect := Rect;
  Clips[ClipCount].Transform := M.Data;
  Inc(ClipCount);
end;

procedure TCanvas.Clip; overload;
begin
  nvgResetScissor(Ctx);
  { Clips added since the last push are dropped. Clips from outer pushes are
    kept for Pop to restore, but no longer take effect. }
  if Stack <> nil then
    ClipCount := Stack.ClipCount
  else
    ClipCount := 0;
  ClipStart := ClipCount;
end;

procedure TCanvas.Clear;
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT);
end;

function TCanvas.MeasureText(Font: IFont; const Text: string): TPointF;
var
  Bounds: array[0..4] of Float;
  S: string;
  I: Integer;
begin
  Result := PointZero;
  if Text = '' then
    Exit;
  CheckMatrix;
  { Needed for some reason font changed isn't being handled correctly. I will
    track down the issue later }
  LastFont := nil;
  SelectObject(Font);
  S := Text;
  for I := 1 to Length(S) do
    if S[I] = ' ' then
      S[I] := '[';
  nvgTextBounds(Ctx, 0, 0, PChar(S), nil, @Bounds);
  Result.X := Bounds[2] - Bounds[0];
  Result.Y := Bounds[3] - Bounds[1];
end;

{ MeasureAdvance returns how far the pen moves when Text is drawn, including
  leading and trailing spaces, which is where the next character would start }

function TCanvas.MeasureAdvance(Font: IFont; const Text: string): Float;
begin
  Result := 0;
  if Text = '' then
    Exit;
  CheckMatrix;
  LastFont := nil;
  SelectObject(Font);
  Result := nvgTextBounds(Ctx, 0, 0, PChar(Text), nil, nil);
end;

function TCanvas.MeasureMemo(Font: IFont; const Text: string; Width: Float): Float;
var
  Bounds: array[0..4] of Float;
begin
  Result := 0;
  if Text = '' then
    Exit;
  CheckMatrix;
  { Needed for some reason font changed isn't being handled correctly. I will
    track down the issue later }
  LastFont := nil;
  SelectObject(Font);
  nvgTextBoxBounds(Ctx, 0, 0, Width, PChar(Text), nil, @Bounds);
  Result := Bounds[3] - Bounds[1];
end;

procedure TCanvas.DrawText(Font: IFont; const Text: string; X, Y: Float);
begin
  if Text = '' then
    Exit;
  CheckMatrix;
  SelectObject(Font);
  nvgFillColor(Ctx,  ToNVG(Font.Color));
  nvgText(Ctx, X, Y, PChar(Text), nil);
  nvgBeginPath(Ctx);
  LastBrush := nil;
  LastPen := nil;
end;

procedure TCanvas.DrawTextMemo(Font: IFont; const Text: string; X, Y, Width: Float);
begin
  if Text = '' then
    Exit;
  CheckMatrix;
  SelectObject(Font);
  nvgFillColor(Ctx, ToNVG(Font.Color));
  nvgTextBox(Ctx, X, Y, Width, PChar(Text), nil);
  nvgBeginPath(Ctx);
  LastBrush := nil;
  LastPen := nil;
end;

procedure TCanvas.DrawImage(Image: IBitmap; X, Y: Float; Opacity: Float = 1; Angle: Float = 0);
var
  Paint: TNVGpaint;
  Id: Integer;
begin
  Id := GetBitmapId(Image);
  if (Id < 0) or IsBound(Image) then Exit;
  CheckMatrix;
  nvgBeginPath(Ctx);
  nvgRect(Ctx, X, Y, Image.Width, Image.Height);
  Paint := nvgImagePattern(Ctx, X, Y, Image.Width, Image.Height, Angle, Id, Opacity);
  nvgFillPaint(Ctx, Paint);
  nvgFill(Ctx);
  nvgBeginPath(Ctx);
  LastBrush := nil;
end;

procedure TCanvas.DrawImage(Image: IBitmap; Source, Dest: TRectF; Opacity: Float = 1; Angle: Float = 0);
var
  Paint: TNVGpaint;
  SX, SY: Float;
  FlipX, FlipY: Integer;
  Id: Integer;
begin
  if Source.Empty or Dest.Empty then
    Exit;
  Id := GetBitmapId(Image);
  if (Id < 0) or IsBound(Image) then Exit;
  CheckMatrix;
  nvgBeginPath(Ctx);
  if Dest.Width < 0 then FlipX := -1 else FlipX := 1;
  if Dest.Height < 0 then FlipY := -1 else FlipY := 1;
  Dest.Width := Dest.Width * FlipX;
  Dest.Height :=Dest.Height * FlipY;
  nvgRect(Ctx, Dest.X, Dest.Y, Dest.Width, Dest.Height);
  SX := Dest.Width / Source.Width;
  SY := Dest.Height / Source.Height;
  Paint := nvgImagePattern(Ctx, Dest.X - Source.X * SX, Dest.Y - Source.Y * SY,
    Image.Width * SX * FlipX, Image.Height * SY * FlipY, Angle, Id, Opacity);
  nvgFillPaint(Ctx, Paint);
  nvgFill(Ctx);
  nvgBeginPath(Ctx);
  LastBrush := nil;
end;

procedure TCanvas.DrawSprite(Sprite: ISprite);
var
  W, H: Integer;
  P, F: TVec2;
  S, D: TRectF;
begin
  if Sprite.Bitmap = nil then
    Exit;
  if Sprite.Opacity = 0 then
    Exit;
  W := Sprite.Bitmap.Width;
  H := Sprite.Bitmap.Height;
  S := Sprite.Bitmap.ClientRect;
  S.width := S.width / Sprite.Cols;
  S.height := S.height / Sprite.Rows;
  F := Sprite.Scale;
  P.x := S.width * Sprite.Pivot.x * Abs(F.x);
  P.y := S.height * Sprite.Pivot.y * Abs(F.y);
  D := S;
  if Sprite.Cell mod (Sprite.Cols + 1) > 0 then
  S.x := Sprite.Cell mod Sprite.Cols;
  S.x := S.x * W / Sprite.Cols;
  if Sprite.Cell >= Sprite.Cols then
  begin
    S.y := Sprite.Cell div Sprite.Cols;
    S.y := S.y * H / Sprite.Rows;
  end;
  D.x := Sprite.Position.x - P.x;
  D.y := Sprite.Position.y - P.y;
  D.width := Abs(F.x) * D.width;
  D.height := Abs(F.y) * D.height;
  if Sprite.Rotation <> 0 then
  begin
    Matrix.Push;
    if F.x < 0 then
      if F.y < 0 then
        Matrix.ScaleAt(-1, -1, D.x + P.x, D.y + P.y)
      else
        Matrix.ScaleAt(-1, 1, D.x + P.x, D.y + P.y)
    else if F.y < 0 then
      Matrix.ScaleAt(1, -1, D.x + P.x, D.y + P.y);
    Matrix.RotateAt(Sprite.Rotation, D.x + P.x, D.y + P.y);
    DrawImage(Sprite.Bitmap, S, D, Sprite.Opacity);
    Matrix.Pop;
  end
  else if (F.x < 0) or (F.y < 0) then
  begin
    Matrix.Push;
    if F.x < 0 then
      if F.y < 0 then
        Matrix.ScaleAt(-1, -1, D.x + P.x, D.y + P.y)
      else
        Matrix.ScaleAt(-1, 1, D.x + P.x, D.y + P.y)
    else
      Matrix.ScaleAt(1, -1, D.x + P.x, D.y + P.y);
    DrawImage(Sprite.Bitmap, S, D, Sprite.Opacity);
    Matrix.Pop;
  end
  else
    DrawImage(Sprite.Bitmap, S, D, Sprite.Opacity);
end;

procedure TCanvas.BeginPath;
begin
  nvgBeginPath(Ctx);
end;

procedure TCanvas.MoveTo(X, Y: Float);
begin
  CheckMatrix;
  nvgMoveTo(Ctx, X, Y);
end;

procedure TCanvas.LineTo(X, Y: Float);
begin
  CheckMatrix;
  nvgLineTo(Ctx, X, Y);
end;

procedure TCanvas.ArcTo(X1, Y1, X2, Y2, Radius: Float);
begin
  CheckMatrix;
  nvgArcTo(Ctx, X1, Y1, X2, Y2, Radius);
end;

procedure TCanvas.BezierTo(CX1, CY1, CX2, CY2, X, Y: Float);
begin
  CheckMatrix;
  nvgBezierTo(Ctx, CX1, CY1, CX2, CY2, X, Y);
end;

procedure TCanvas.QuadTo(CX, CY, X, Y: Float);
begin
  CheckMatrix;
  nvgQuadTo(Ctx, CX, CY, X, Y);
end;

procedure TCanvas.Arc(CX, CY, Radius, A0, A1: Float; Clockwise: Boolean);
const
  Dir: array[Boolean] of Integer = (NVG_CCW, NVG_CW);
begin
  CheckMatrix;
  nvgArc(Ctx, CX, CY, Radius, A0, A1, Dir[Clockwise]);
end;

procedure TCanvas.Circle(X, Y, Radius: Float);
begin
  CheckMatrix;
  nvgCircle(Ctx, X, Y, Radius);
end;

procedure TCanvas.Ellipse(const R: TRectF);
begin
  CheckMatrix;
  nvgEllipse(Ctx, R.X + R.Width / 2, R.Y + R.Height / 2,
    R.Width / 2, R.Height / 2);
end;

procedure TCanvas.Ellipse(X, Y, Width, Height: Float);
begin
  CheckMatrix;
  nvgEllipse(Ctx, X + Width / 2, Y + Height / 2, Width / 2, Height / 2);
end;

procedure TCanvas.Rect(const R: TRectF);
begin
  CheckMatrix;
  nvgRect(Ctx, R.X, R.Y, R.Width, R.Height);
end;

procedure TCanvas.Rect(X, Y, Width, Height: Float);
begin
  CheckMatrix;
  nvgRect(Ctx, X, Y, Width, Height);
end;

procedure TCanvas.RoundRect(const R: TRectF; Radius: Float);
begin
  CheckMatrix;
  nvgRoundedRect(Ctx, R.X, R.Y, R.Width, R.Height, Radius);
end;

procedure TCanvas.RoundRect(X, Y, Width, Height: Float; Radius: Float);
begin
  CheckMatrix;
  nvgRoundedRect(Ctx, X, Y, Width, Height, Radius);
end;

procedure TCanvas.RoundRectVarying(const R: TRectF; TL, TR, BR, BL: Float);
begin
  CheckMatrix;
  if TL < 0 then TL := 0;
  if TR < 0 then TR := 0;
  if BL < 0 then BL := 0;
  if BR < 0 then BR := 0;
  nvgRoundedRectVarying(Ctx, R.X, R.Y, R.Width, R.Height, TL, TR, BR, BL);
end;

procedure TCanvas.Polygon(P: PPointF; Count: Integer; Closed: Boolean = True);
var
  I: Integer;
begin
  if Count < 2 then
    Exit;
  CheckMatrix;
  nvgMoveTo(Ctx, P.X, P.Y);
  Inc(P);
  for I := 2 to Count do
  begin
    nvgLineTo(Ctx, P.X, P.Y);
    Inc(P);
  end;
  if Closed then
    nvgClosePath(Ctx);
end;

procedure TCanvas.ClosePath;
begin
  nvgClosePath(Ctx);
end;

procedure TCanvas.Fill(Brush: IBrush; Preserve: Boolean = False);
const
  Rules: array[TFillRule] of Integer = (NVG_FILL_WINDING, NVG_FILL_NONZERO, NVG_FILL_EVENODD);
begin
  SelectObject(Brush);
  { nvgBeginFrame resets the nanovg state, so the rule is set for every fill }
  nvgFillRule(Ctx, Rules[FillRule]);
  nvgFill(Ctx);
  if not Preserve then
    nvgBeginPath(Ctx);
end;

procedure TCanvas.Fill(Color: TColorF; Preserve: Boolean = False); overload;
begin
  (SolidBrush as TBrush).Changed := True;
  SolidBrush.Color := Color;
  Fill(SolidBrush, Preserve);
end;

procedure TCanvas.Stroke(Pen: IPen; Preserve: Boolean = False); overload;
begin
  SelectObject(Pen);
  nvgStroke(Ctx);
  if not Preserve then
    nvgBeginPath(Ctx);
end;

procedure TCanvas.Stroke(Color: TColorF; Width: Float = 1; Preserve: Boolean = False); overload;
begin
  SolidPen.Color := Color;
  SolidPen.Width := Width;;
  Stroke(SolidPen, Preserve);
end;

procedure TCanvas.FillCircle(Brush: IBrush; X, Y, Radius: Float);
begin
  BeginPath;
  Circle(X, Y, Radius);
  Fill(Brush);
end;

procedure TCanvas.StrokeCircle(Pen: IPen; X, Y, Radius: Float);
begin
  BeginPath;
  Circle(X, Y, Radius);
  Stroke(Pen);
end;

procedure TCanvas.FillRect(Brush: IBrush; const R: TRectF);
begin
  BeginPath;
  Rect(R);
  Fill(Brush);
end;

procedure TCanvas.StrokeRect(Pen: IPen; const R: TRectF);
begin
  BeginPath;
  Rect(R);
  Stroke(Pen);
end;

procedure TCanvas.FillRoundRect(Brush: IBrush; const R: TRectF; Radius: Float);
begin
  BeginPath;
  RoundRect(R, Radius);
  Fill(Brush);
end;

procedure TCanvas.StrokeRoundRect(Pen: IPen; const R: TRectF; Radius: Float);
begin
  BeginPath;
  RoundRect(R, Radius);
  Stroke(Pen);
end;

function TCanvas.GetGontext: Pointer;
begin
  Result := Ctx;
end;

function TCanvas.GetBlendMode: TBlendMode;
begin
  Result := BlendMode;
end;

procedure TCanvas.SetBlendMode(Value: TBlendMode);
begin
  BlendMode := Value;
  case BlendMode of
    blendAlpha: nvgGlobalCompositeBlendFunc(Ctx, NVG_ONE, NVG_ONE_MINUS_SRC_ALPHA);
    blendAdditive: nvgGlobalCompositeBlendFunc(Ctx, NVG_ONE, NVG_ONE);
    blendSubtractive: nvgGlobalCompositeBlendFunc(Ctx, NVG_ONE_MINUS_SRC_COLOR, NVG_ONE_MINUS_SRC_COLOR);
    blendLighten: nvgGlobalCompositeBlendFunc(Ctx, NVG_SRC_COLOR, NVG_ONE);
    blendDarken: nvgGlobalCompositeBlendFunc(Ctx, NVG_ONE_MINUS_SRC_COLOR, NVG_SRC_COLOR);
    blendInvert: nvgGlobalCompositeBlendFunc(Ctx, NVG_ZERO, NVG_ONE_MINUS_SRC_COLOR);
    blendNegative: nvgGlobalCompositeBlendFunc(Ctx, NVG_ONE_MINUS_DST_COLOR, NVG_ZERO);
  end;
end;

function TCanvas.GetOpacity: Float;
begin
  Result := Opacity;
end;

procedure TCanvas.SetOpacity(Value: Float);
begin
  Opacity := {%H-}Clamp(Value);
  nvgGlobalAlpha(Ctx, Opacity);
end;

function TCanvas.GetMatrix: IMatrix;
begin
  Result := Matrix;
end;

procedure TCanvas.SetMatrix(Value: IMatrix);
var
  A: TMatrix;
begin
  if Value = Matrix then Exit;
  A := Value as TMatrix;
  M.Data := A.Data;
  M.Changed := True;
end;

function TCanvas.GetWinding: TWinding;
begin
  Result := Winding;
end;

procedure TCanvas.SetWinding(const Value: TWinding);
const
  W: array[TWinding] of Integer = (NVG_CCW, NVG_CW);
begin
  if Value = Winding then Exit;
  Winding := Value;
  nvgPathWinding(Ctx, W[Winding]);
end;

function TCanvas.GetFillRule: TFillRule;
begin
  Result := FillRule;
end;

procedure TCanvas.SetFillRule(const Value: TFillRule);
begin
  FillRule := Value;
end;

{ Creation functions }

function Clamp(A: Float): Float;
begin
  if A < 0 then Result := 0 else if A > 1 then Result := 1 else Result := A;
end;

function NewColorB(R, G, B: Byte; A: Byte = $FF): TColorF;
begin
  Result.Red := R / $FF;
  Result.Green := G / $FF;
  Result.Blue := B / $FF;
  Result.Alpha := A / $FF;
end;

function NewColorF(R, G, B: Float; A: Float = 1): TColorF;
begin
  Result.Alpha := Clamp(A);
  Result.Red := Clamp(R);
  Result.Green := Clamp(G);
  Result.Blue := Clamp(B);
end;

function NewHSL(H, S, L: Float; A: Float = 1): TColorF;
var
  C: TNVGcolor;
begin
  C := nvgHSL(H, S, L);
  Result.Red := C.R;
  Result.Green := C.G;
  Result.Blue := C.B;
  Result.Alpha := Clamp(A);
end;

function NewPointF(X, Y: Float): TPointF;
begin
  Result.X := X; Result.Y := Y;
end;

function NewRectF(Width, Height: Float): TRectF;
begin
  Result.X := 0; Result.Y := 0; Result.Width := Width; Result.Height := Height;
end;

function NewRectF(X, Y, Width, Height: Float): TRectF;
begin
  Result.X := X; Result.Y := Y; Result.Width := Width; Result.Height := Height;
end;

type
  TMatrixStack = TList<IMatrix>;
  TPenStack = TList<IPen>;
  TSolidBrushStack = TList<ISolidBrush>;

var
  MatrixStack: TMatrixStack;
  PenStack: TPenStack;
  SolidBrushStack: TSolidBrushStack;

function NewMatrix: IMatrix;
var
  M: TMatrix;
  I: Integer;
begin
  if MatrixStack = nil then
  begin
    MatrixStack := TMatrixStack.Create;
    MatrixStack.Capacity := StackSize;
    for I := 0 to StackSize - 1 do
      MatrixStack.Add(TMatrix.Create);
  end;
  for I := 0 to StackSize - 1 do
  begin
    M := MatrixStack.Item[I] as TMatrix;
    if M.RefCount = 2 then
    begin
      M.Identity;
      Exit(M);
    end;
  end;
  Result := TMatrix.Create;
end;

function NewPen: IPen;
var
  P: TPen;
  I: Integer;
begin
  if PenStack = nil then
  begin
    PenStack := TPenStack.Create;
    PenStack.Capacity := StackSize;
    for I := 0 to StackSize - 1 do
      PenStack.Add(TPen.Create);
  end;
  for I := 0 to StackSize - 1 do
  begin
    P := PenStack.Item[I] as TPen;
    if P.RefCount = 2 then
    begin
      P.Color := ColorBlack;
      P.Width := 1;
      P.LineCap := capButt;
      P.LineJoin := joinMiter;
      P.MiterLimit := 10;
      P.Brush := nil;
      Exit(P);
    end;
  end;
  Result := TPen.Create;
end;

function NewPen(const Color: TColorF; Width: Float = 1): IPen;
begin
  Result := NewPen;
  Result.Color := Color;
  Result.Width := Width;
end;

function NewBrush(const Color: TColorF): ISolidBrush;
var
  B: TSolidBrush;
  I: Integer;
begin
  if SolidBrushStack = nil then
  begin
    SolidBrushStack := TSolidBrushStack.Create;
    SolidBrushStack.Capacity := StackSize;
    for I := 0 to StackSize - 1 do
      SolidBrushStack.Add(TSolidBrush.Create);
  end;
  for I := 0 to StackSize - 1 do
  begin
    B := SolidBrushStack.Item[I] as TSolidBrush;
    if B.RefCount = 2 then
    begin
      // Strange that this was ColorBlack
      B.Color := Color;
      Exit(B);
    end;
  end;
  Result := TSolidBrush.Create;
  Result.Color := Color;
end;

function NewBrush(const A, B: TPointF): ILinearGradientBrush;
var
  G: TLinearGradientBrush;
begin
  Result := TLinearGradientBrush.Create;
  G := Result as TLinearGradientBrush;
  G.A := A;
  G.B := B;
end;

function NewBrush(const Rect: TRectF): IRadialGradientBrush;
var
  B: TRadialGradientBrush;
begin
  Result := TRadialGradientBrush.Create;
  B := Result as TRadialGradientBrush;
  B.Rect := Rect;
end;

function NewBrush(const Rect: TRectF; Radius, Feather: Float): IBoxGradientBrush;
var
  B: TBoxGradientBrush;
begin
  Result := TBoxGradientBrush.Create;
  B := Result as TBoxGradientBrush;
  B.Rect := Rect;
  B.Radius := Radius;
  B.Feather := Feather;
end;

function NewBrush(Bitmap: IBitmap): IBitmapBrush;
var
  B: TBitmapBrush;
begin
  Result := TBitmapBrush.Create;
  B := Result as TBitmapBrush;
  B.Bitmap := Bitmap;
end;

function NewSprite: ISprite;
begin
  Result := TSprite.Create;
end;

function NewCanvas: ICanvas;
begin
  Result := TCanvas.Create;
end;

finalization
  MatrixStack.Free;
  PenStack.Free;
  SolidBrushStack.Free;
end.

