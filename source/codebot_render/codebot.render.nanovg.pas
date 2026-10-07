(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.render.nanovg.txt> }
unit Codebot.Render.NanoVG;

{ Codebot.Render.NanoVG is a Pascal port of NanoVG by Mikko Mononen, which is
  zlib licensed. NanoVG is an antialiased 2D vector drawing library rendered
  with OpenGL. It includes the OpenGL backend from nanovg_gl.h, which uses
  Codebot.OpenGL and works with every desktop and embedded version selected in
  render.inc. Text is rendered with Codebot.Render.FontStash and images are
  loaded using NewBitmapData from Codebot.Platform.

  Strings are UTF-8 encoded. Functions taking Str and EndStr accept a pointer
  to the start of the text and an optional pointer one past its last byte, or
  nil for a null terminated string.

  Typical usage inside TGraphicsBox.OnRender:

    nvgBeginFrame(Vg, Width, Height, 1);
    nvgBeginPath(Vg);
    nvgRect(Vg, 100, 100, 120, 30);
    nvgFillColor(Vg, nvgRGBA(255, 192, 0, 255));
    nvgFill(Vg);
    nvgEndFrame(Vg); }

{$i render.inc}
{$pointermath on}
{$rangechecks off}
{$overflowchecks off}

interface

uses
  Codebot.OpenGL,
  Codebot.Platform,
  Codebot.Graphics.Types,
  Codebot.Render.FontStash;

const
  NVG_PI = 3.14159265358979323846264338327;

  { NVGwinding }
  { Winding for solid shapes }
  NVG_CCW = 1;
  { Winding for holes }
  NVG_CW = 2;

  { NVGsolidity }
  NVG_SOLID = 1; { CCW }
  NVG_HOLE = 2; { CW }

  { NVGfillRule }
  { Each sub-path is solid or a hole as set by nvgPathWinding, solid by default }
  NVG_FILL_WINDING = 0;
  { Sub-paths keep the direction they were drawn in and fill using the nonzero rule }
  NVG_FILL_NONZERO = 1;
  { Sub-paths fill using the even-odd rule regardless of direction }
  NVG_FILL_EVENODD = 2;

  { NVGlineCap }
  NVG_BUTT = 0;
  NVG_ROUND = 1;
  NVG_SQUARE = 2;
  NVG_BEVEL = 3;
  NVG_MITER = 4;

  { NVGalign horizontal align }
  { Default, align text horizontally to left }
  NVG_ALIGN_LEFT = 1 shl 0;
  { Align text horizontally to center }
  NVG_ALIGN_CENTER = 1 shl 1;
  { Align text horizontally to right }
  NVG_ALIGN_RIGHT = 1 shl 2;
  { NVGalign vertical align }
  { Align text vertically to top }
  NVG_ALIGN_TOP = 1 shl 3;
  { Align text vertically to middle }
  NVG_ALIGN_MIDDLE = 1 shl 4;
  { Align text vertically to bottom }
  NVG_ALIGN_BOTTOM = 1 shl 5;
  { Default, align text vertically to baseline }
  NVG_ALIGN_BASELINE = 1 shl 6;

  { NVGblendFactor }
  NVG_ZERO = 1 shl 0;
  NVG_ONE = 1 shl 1;
  NVG_SRC_COLOR = 1 shl 2;
  NVG_ONE_MINUS_SRC_COLOR = 1 shl 3;
  NVG_DST_COLOR = 1 shl 4;
  NVG_ONE_MINUS_DST_COLOR = 1 shl 5;
  NVG_SRC_ALPHA = 1 shl 6;
  NVG_ONE_MINUS_SRC_ALPHA = 1 shl 7;
  NVG_DST_ALPHA = 1 shl 8;
  NVG_ONE_MINUS_DST_ALPHA = 1 shl 9;
  NVG_SRC_ALPHA_SATURATE = 1 shl 10;

  { NVGcompositeOperation }
  NVG_SOURCE_OVER = 0;
  NVG_SOURCE_IN = 1;
  NVG_SOURCE_OUT = 2;
  NVG_ATOP = 3;
  NVG_DESTINATION_OVER = 4;
  NVG_DESTINATION_IN = 5;
  NVG_DESTINATION_OUT = 6;
  NVG_DESTINATION_ATOP = 7;
  NVG_LIGHTER = 8;
  NVG_COPY = 9;
  NVG_XOR = 10;

  { NVGimageFlags }
  { Generate mipmaps during creation of the image }
  NVG_IMAGE_GENERATE_MIPMAPS = 1 shl 0;
  { Repeat image in X direction }
  NVG_IMAGE_REPEATX = 1 shl 1;
  { Repeat image in Y direction }
  NVG_IMAGE_REPEATY = 1 shl 2;
  { Flips (inverses) image in Y direction when rendered }
  NVG_IMAGE_FLIPY = 1 shl 3;
  { Image data has premultiplied alpha }
  NVG_IMAGE_PREMULTIPLIED = 1 shl 4;
  { Image interpolation is Nearest instead Linear }
  NVG_IMAGE_NEAREST = 1 shl 5;

  { NVGcreateFlags for nvgCreateGL }
  { Flag indicating if geometry based anti-aliasing is used (may not be
    needed when using MSAA) }
  NVG_ANTIALIAS = 1 shl 0;
  { Flag indicating if strokes should be drawn using stencil buffer. The
    rendering will be a little slower, but path overlaps (i.e.
    self-intersecting or sharp turns) will be drawn just once }
  NVG_STENCIL_STROKES = 1 shl 1;
  { Flag indicating that additional debug checks are done }
  NVG_DEBUG = 1 shl 2;

  { NVGimageFlagsGL }
  { Do not delete GL texture handle }
  NVG_IMAGE_NODELETE = 1 shl 16;

  { NVGtexture }
  NVG_TEXTURE_ALPHA = $01;
  NVG_TEXTURE_RGBA = $02;

  NVG_MAX_STATES = 32;
  NVG_MAX_FONTIMAGES = 4;

type
  { TNVGxform is a 2x3 affine transform [a b c d e f] where the matrix is
      [a c e]
      [b d f]
      [0 0 1] }
  PNVGxform = ^TNVGxform;
  { TNVGxform holds the six values of a 2D transform }
  TNVGxform = array[0..5] of Single;

  { TNVGcolor is a color as red, green, blue, and alpha from 0 to 1 }
  PNVGcolor = ^TNVGcolor;
  TNVGcolor = record
    case Integer of
      0: (RGBA: array[0..3] of Single);
      1: (R, G, B, A: Single);
  end;

  { TNVGpaint is a gradient or image pattern used to fill or stroke }
  PNVGpaint = ^TNVGpaint;
  TNVGpaint = record
    XForm: TNVGxform;
    Extent: array[0..1] of Single;
    Radius: Single;
    Feather: Single;
    InnerColor: TNVGcolor;
    OuterColor: TNVGcolor;
    Image: Integer;
  end;

  { TNVGcompositeOperationState holds the blend factors used when drawing }
  TNVGcompositeOperationState = record
    SrcRGB: Integer;
    DstRGB: Integer;
    SrcAlpha: Integer;
    DstAlpha: Integer;
  end;

  { TNVGglyphPosition is where one glyph of a line of text is }
  PNVGglyphPosition = ^TNVGglyphPosition;
  TNVGglyphPosition = record
    { Position of the glyph in the input string }
    Str: PAnsiChar;
    { The x-coordinate of the logical glyph position }
    X: Single;
    { The bounds of the glyph shape }
    MinX, MaxX: Single;
  end;

  { TNVGtextRow is one row of text broken to a width }
  PNVGtextRow = ^TNVGtextRow;
  TNVGtextRow = record
    { Pointer to the input text where the row starts }
    Start: PAnsiChar;
    { Pointer to the input text where the row ends (one past the last character) }
    EndStr: PAnsiChar;
    { Pointer to the beginning of the next row }
    Next: PAnsiChar;
    { Logical width of the row }
    Width: Single;
    { Actual bounds of the row. Logical with and bounds can differ because of
      kerning and some parts over extending }
    MinX, MaxX: Single;
  end;

  { TNVGscissor is the rectangle drawing is clipped to }
  PNVGscissor = ^TNVGscissor;
  TNVGscissor = record
    XForm: TNVGxform;
    Extent: array[0..1] of Single;
  end;

  { TNVGvertex is a position and a texture coordinate }
  PNVGvertex = ^TNVGvertex;
  TNVGvertex = record
    X, Y, U, V: Single;
  end;

  { TNVGpath is the fill and stroke vertices of one sub-path }
  PNVGpath = ^TNVGpath;
  TNVGpath = record
    First: Integer;
    Count: Integer;
    Closed: Byte;
    NBevel: Integer;
    Fill: PNVGvertex;
    NFill: Integer;
    Stroke: PNVGvertex;
    NStroke: Integer;
    Winding: Integer;
    Convex: Integer;
  end;

  { Render backend interface used by nvgCreateInternal }
  TNVGrenderCreate = function(UPtr: Pointer): Integer;
  { Callbacks a renderer gives NanoVG to manage textures and to draw }
  TNVGrenderCreateTexture = function(UPtr: Pointer; TexType, W, H, ImageFlags: Integer; Data: PByte): Integer;
  TNVGrenderDeleteTexture = function(UPtr: Pointer; Image: Integer): Integer;
  TNVGrenderUpdateTexture = function(UPtr: Pointer; Image, X, Y, W, H: Integer; Data: PByte): Integer;
  TNVGrenderGetTextureSize = function(UPtr: Pointer; Image: Integer; W, H: PInteger): Integer;
  TNVGrenderViewport = procedure(UPtr: Pointer; Width, Height, DevicePixelRatio: Single);
  TNVGrenderCancel = procedure(UPtr: Pointer);
  TNVGrenderFlush = procedure(UPtr: Pointer);
  TNVGrenderFill = procedure(UPtr: Pointer; Paint: PNVGpaint; CompositeOperation: TNVGcompositeOperationState;
    Scissor: PNVGscissor; Fringe: Single; Bounds: PSingle; Paths: PNVGpath; NPaths: Integer; FillRule: Integer);
  TNVGrenderStroke = procedure(UPtr: Pointer; Paint: PNVGpaint; CompositeOperation: TNVGcompositeOperationState;
    Scissor: PNVGscissor; Fringe, StrokeWidth: Single; Paths: PNVGpath; NPaths: Integer);
  TNVGrenderTriangles = procedure(UPtr: Pointer; Paint: PNVGpaint; CompositeOperation: TNVGcompositeOperationState;
    Scissor: PNVGscissor; Verts: PNVGvertex; NVerts: Integer; Fringe: Single);
  TNVGrenderDelete = procedure(UPtr: Pointer);

  { TNVGparams holds the callbacks of the renderer }
  PNVGparams = ^TNVGparams;
  TNVGparams = record
    UserPtr: Pointer;
    EdgeAntiAlias: Integer;
    RenderCreate: TNVGrenderCreate;
    RenderCreateTexture: TNVGrenderCreateTexture;
    RenderDeleteTexture: TNVGrenderDeleteTexture;
    RenderUpdateTexture: TNVGrenderUpdateTexture;
    RenderGetTextureSize: TNVGrenderGetTextureSize;
    RenderViewport: TNVGrenderViewport;
    RenderCancel: TNVGrenderCancel;
    RenderFlush: TNVGrenderFlush;
    RenderFill: TNVGrenderFill;
    RenderStroke: TNVGrenderStroke;
    RenderTriangles: TNVGrenderTriangles;
    RenderDelete: TNVGrenderDelete;
  end;

  { Internal state, use the nvg functions to work with a context }

  PNVGstate = ^TNVGstate;
  { TNVGstate is the drawing state saved by nvgSave and restored by nvgRestore }
  TNVGstate = record
    CompositeOperation: TNVGcompositeOperationState;
    ShapeAntiAlias: Integer;
    Fill: TNVGpaint;
    Stroke: TNVGpaint;
    StrokeWidth: Single;
    MiterLimit: Single;
    LineJoin: Integer;
    LineCap: Integer;
    Alpha: Single;
    XForm: TNVGxform;
    Scissor: TNVGscissor;
    FillRule: Integer;
    FontSize: Single;
    LetterSpacing: Single;
    LineHeight: Single;
    FontBlur: Single;
    TextAlign: Integer;
    FontId: Integer;
  end;

  { TNVGpoint is a point of a flattened path }
  PNVGpoint = ^TNVGpoint;
  TNVGpoint = record
    X, Y: Single;
    DX, DY: Single;
    Len: Single;
    DMX, DMY: Single;
    Flags: Byte;
  end;

  { TNVGpathCache holds the flattened paths and their vertices }
  PNVGpathCache = ^TNVGpathCache;
  TNVGpathCache = record
    Points: PNVGpoint;
    NPoints: Integer;
    CPoints: Integer;
    Paths: PNVGpath;
    NPaths: Integer;
    CPaths: Integer;
    Verts: PNVGvertex;
    NVerts: Integer;
    CVerts: Integer;
    Bounds: array[0..3] of Single;
  end;

  { TNVGcontext is a NanoVG drawing context }
  PNVGcontext = ^TNVGcontext;
  TNVGcontext = record
    Params: TNVGparams;
    Commands: PSingle;
    CCommands: Integer;
    NCommands: Integer;
    CommandX, CommandY: Single;
    States: array[0..NVG_MAX_STATES - 1] of TNVGstate;
    NStates: Integer;
    Cache: PNVGpathCache;
    TessTol: Single;
    DistTol: Single;
    FringeWidth: Single;
    DevicePxRatio: Single;
    Fs: PFonsContext;
    FontImages: array[0..NVG_MAX_FONTIMAGES - 1] of Integer;
    FontImageIdx: Integer;
    DrawCallCount: Integer;
    FillTriCount: Integer;
    StrokeTriCount: Integer;
    TextTriCount: Integer;
  end;

{ OpenGL backend

  Creates a NanoVG context rendered with the OpenGL version selected in
  render.inc. The OpenGL context must be current when creating, using and
  deleting the NanoVG context. Flags should be a combination of NVG_ANTIALIAS,
  NVG_STENCIL_STROKES and NVG_DEBUG. }

function nvgCreateGL(Flags: Integer): PNVGcontext;
{ Free a context created for OpenGL }
procedure nvgDeleteGL(Ctx: PNVGcontext);
{ Creates an image from an existing OpenGL texture handle }
function nvglCreateImageFromHandle(Ctx: PNVGcontext; TextureId: GLuint; W, H, ImageFlags: Integer): Integer;
{ Returns the OpenGL texture handle of an image }
function nvglImageHandle(Ctx: PNVGcontext; Image: Integer): GLuint;

{ Framebuffer utilities from nanovg_gl_utils.h

  A framebuffer can be bound to render into an image instead of the window.
  Its Image field is a NanoVG image which can be painted with nvgImagePattern
  after the framebuffer is unbound by binding nil. }

type
  PNVGLUframebuffer = ^TNVGLUframebuffer;
  { TNVGLUframebuffer is a framebuffer which is drawn to and then used as an
    image }
  TNVGLUframebuffer = record
    Ctx: PNVGcontext;
    Fbo: GLuint;
    Rbo: GLuint;
    Texture: GLuint;
    Image: Integer;
  end;

{ Creates a framebuffer to render to, returning nil if it could not be created }
function nvgluCreateFramebuffer(Ctx: PNVGcontext; W, H, ImageFlags: Integer): PNVGLUframebuffer;
{ Binds a framebuffer, or the framebuffer which was bound first if nil }
procedure nvgluBindFramebuffer(Fb: PNVGLUframebuffer);
{ Deletes a framebuffer and its image }
procedure nvgluDeleteFramebuffer(Fb: PNVGLUframebuffer);

{ Frames

  Begin drawing a new frame. Calls to nanovg drawing API should be wrapped in
  nvgBeginFrame and nvgEndFrame. nvgBeginFrame defines the size of the window
  to render to in relation currently set viewport (i.e. glViewport on GL
  backends). Device pixel ration allows to control the rendering on Hi-DPI
  devices. For example, GLFW returns two dimension for an opened window:
  window size and frame buffer size. In that case you would set windowWidth
  and windowHeight to the window size devicePixelRatio to:
  frameBufferWidth / windowWidth. }

procedure nvgBeginFrame(Ctx: PNVGcontext; WindowWidth, WindowHeight, DevicePixelRatio: Single);
{ Cancels drawing the current frame }
procedure nvgCancelFrame(Ctx: PNVGcontext);
{ Ends drawing flushing remaining render state }
procedure nvgEndFrame(Ctx: PNVGcontext);

{ Composite operation

  The composite operations in NanoVG are modeled after HTML Canvas API, and
  the blend func is based on OpenGL (see corresponding manuals for more info).
  The colors in the blending state have premultiplied alpha. }

{ Sets the composite operation. The op parameter should be one of NVG_SOURCE_OVER and the rest }
procedure nvgGlobalCompositeOperation(Ctx: PNVGcontext; Op: Integer);
{ Sets the composite operation with custom pixel arithmetic }
procedure nvgGlobalCompositeBlendFunc(Ctx: PNVGcontext; SFactor, DFactor: Integer);
{ Sets the composite operation with custom pixel arithmetic for RGB and alpha components separately }
procedure nvgGlobalCompositeBlendFuncSeparate(Ctx: PNVGcontext; SrcRGB, DstRGB, SrcAlpha, DstAlpha: Integer);

{ Color utils

  Colors in NanoVG are stored as unsigned ints in ABGR format. }

{ Returns a color value from red, green, blue values. Alpha will be set to 255 (1.0) }
function nvgRGB(R, G, B: Byte): TNVGcolor;
{ Returns a color value from red, green, blue values. Alpha will be set to 1.0 }
function nvgRGBf(R, G, B: Single): TNVGcolor;
{ Returns a color value from red, green, blue and alpha values }
function nvgRGBA(R, G, B, A: Byte): TNVGcolor;
{ Returns a color value from red, green, blue and alpha values }
function nvgRGBAf(R, G, B, A: Single): TNVGcolor;
{ Linearly interpolates from color c0 to c1, and returns resulting color value }
function nvgLerpRGBA(C0, C1: TNVGcolor; U: Single): TNVGcolor;
{ Sets transparency of a color value }
function nvgTransRGBA(C0: TNVGcolor; A: Byte): TNVGcolor;
{ Sets transparency of a color value }
function nvgTransRGBAf(C0: TNVGcolor; A: Single): TNVGcolor;
{ Returns color value specified by hue, saturation and lightness. HSL values
  are all in range [0..1], alpha will be set to 255 }
function nvgHSL(H, S, L: Single): TNVGcolor;
{ Returns color value specified by hue, saturation and lightness and alpha.
  HSL values are all in range [0..1], alpha in range [0..255] }
function nvgHSLA(H, S, L: Single; A: Byte): TNVGcolor;
{ Converts a Codebot color to a NanoVG color }
function nvgColor(Color: TColorB): TNVGcolor;

{ State Handling

  NanoVG contains state which represents how paths will be rendered. The
  state contains transform, fill and stroke styles, text and font styles, and
  scissor clipping. }

{ Pushes and saves the current render state into a state stack. A matching
  nvgRestore must be used to restore the state }
procedure nvgSave(Ctx: PNVGcontext);
{ Pops and restores current render state }
procedure nvgRestore(Ctx: PNVGcontext);
{ Resets current render state to default values. Does not affect the render state stack }
procedure nvgReset(Ctx: PNVGcontext);

{ Render styles

  Fill and stroke render style can be either a solid color or a paint which
  is a gradient or a pattern. Solid color is simply defined as a color value,
  different kinds of paints can be created using nvgLinearGradient,
  nvgBoxGradient, nvgRadialGradient and nvgImagePattern.

  Current render style can be saved and restored using nvgSave and nvgRestore. }

{ Sets whether to draw antialias for nvgStroke and nvgFill. It's enabled by default }
procedure nvgShapeAntiAlias(Ctx: PNVGcontext; Enabled: Integer);
{ Sets how nvgFill decides which areas are inside a path with several
  sub-paths, can be one of NVG_FILL_WINDING (default), NVG_FILL_NONZERO or
  NVG_FILL_EVENODD }
procedure nvgFillRule(Ctx: PNVGcontext; Rule: Integer);
{ Sets current stroke style to a solid color }
procedure nvgStrokeColor(Ctx: PNVGcontext; Color: TNVGcolor);
{ Sets current stroke style to a paint, which can be a one of the gradients or a pattern }
procedure nvgStrokePaint(Ctx: PNVGcontext; Paint: TNVGpaint);
{ Sets current fill style to a solid color }
procedure nvgFillColor(Ctx: PNVGcontext; Color: TNVGcolor);
{ Sets current fill style to a paint, which can be a one of the gradients or a pattern }
procedure nvgFillPaint(Ctx: PNVGcontext; Paint: TNVGpaint);
{ Sets the miter limit of the stroke style. Miter limit controls when a
  sharp corner is beveled }
procedure nvgMiterLimit(Ctx: PNVGcontext; Limit: Single);
{ Sets the stroke width of the stroke style }
procedure nvgStrokeWidth(Ctx: PNVGcontext; Size: Single);
{ Sets how the end of the line (cap) is drawn, can be one of: NVG_BUTT
  (default), NVG_ROUND, NVG_SQUARE }
procedure nvgLineCap(Ctx: PNVGcontext; Cap: Integer);
{ Sets how sharp path corners are drawn. Can be one of NVG_MITER (default),
  NVG_ROUND, NVG_BEVEL }
procedure nvgLineJoin(Ctx: PNVGcontext; Join: Integer);
{ Sets the transparency applied to all rendered shapes. Already transparent
  paths will get proportionally more transparent as well }
procedure nvgGlobalAlpha(Ctx: PNVGcontext; Alpha: Single);

{ Transforms

  The paths, gradients, patterns and scissor region are transformed by an
  transformation matrix at the time when they are passed to the API. The
  current transformation matrix is a affine matrix:
    [sx kx tx]
    [ky sy ty]
    [ 0  0  1]
  Where: sx,sy define scaling, kx,ky skewing, and tx,ty translation. The last
  row is assumed to be 0,0,1 and is not stored.

  Apart from nvgResetTransform, each transformation function first creates
  specific transformation matrix and pre-multiplies the current
  transformation by it.

  Current coordinate system (transformation) can be saved and restored using
  nvgSave and nvgRestore. }

{ Resets current transform to a identity matrix }
procedure nvgResetTransform(Ctx: PNVGcontext);
{ Premultiplies current coordinate system by specified matrix }
procedure nvgTransform(Ctx: PNVGcontext; A, B, C, D, E, F: Single);
{ Translates current coordinate system }
procedure nvgTranslate(Ctx: PNVGcontext; X, Y: Single);
{ Rotates current coordinate system. Angle is specified in radians }
procedure nvgRotate(Ctx: PNVGcontext; Angle: Single);
{ Skews the current coordinate system along X axis. Angle is specified in radians }
procedure nvgSkewX(Ctx: PNVGcontext; Angle: Single);
{ Skews the current coordinate system along Y axis. Angle is specified in radians }
procedure nvgSkewY(Ctx: PNVGcontext; Angle: Single);
{ Scales the current coordinate system }
procedure nvgScale(Ctx: PNVGcontext; X, Y: Single);
{ Stores the top part (a-f) of the current transformation matrix }
procedure nvgCurrentTransform(Ctx: PNVGcontext; out XForm: TNVGxform);

{ The following functions can be used to make calculations on 2x3
  transformation matrices }

{ Sets the transform to identity matrix }
procedure nvgTransformIdentity(out Dst: TNVGxform);
{ Sets the transform to translation matrix matrix }
procedure nvgTransformTranslate(out Dst: TNVGxform; TX, TY: Single);
{ Sets the transform to scale matrix }
procedure nvgTransformScale(out Dst: TNVGxform; SX, SY: Single);
{ Sets the transform to rotate matrix. Angle is specified in radians }
procedure nvgTransformRotate(out Dst: TNVGxform; A: Single);
{ Sets the transform to skew-x matrix. Angle is specified in radians }
procedure nvgTransformSkewX(out Dst: TNVGxform; A: Single);
{ Sets the transform to skew-y matrix. Angle is specified in radians }
procedure nvgTransformSkewY(out Dst: TNVGxform; A: Single);
{ Sets the transform to the result of multiplication of two transforms, of A = A*B }
procedure nvgTransformMultiply(var Dst: TNVGxform; const Src: TNVGxform);
{ Sets the transform to the result of multiplication of two transforms, of A = B*A }
procedure nvgTransformPremultiply(var Dst: TNVGxform; const Src: TNVGxform);
{ Sets the destination to inverse of specified transform. Returns 1 if the
  inverse could be calculated, else 0 }
function nvgTransformInverse(out Dst: TNVGxform; const Src: TNVGxform): Integer;
{ Transform a point by given transform }
procedure nvgTransformPoint(out DstX, DstY: Single; const XForm: TNVGxform; SrcX, SrcY: Single);

{ Converts degrees to radians and vice versa }
function nvgDegToRad(Deg: Single): Single;
{ Convert radians to degrees }
function nvgRadToDeg(Rad: Single): Single;

{ Images

  NanoVG allows you to load jpg, png, psd, tga, pic and gif files to be used
  for rendering. In addition you can upload your own image. The image loading
  is provided by NewBitmapData from Codebot.Platform. }

{ Creates image by loading it from the disk from specified file name.
  Returns handle to the image }
function nvgCreateImage(Ctx: PNVGcontext; const FileName: string; ImageFlags: Integer): Integer;
{ Creates image by loading it from the specified chunk of memory.
  Returns handle to the image }
function nvgCreateImageMem(Ctx: PNVGcontext; ImageFlags: Integer; Data: PByte; NData: Integer): Integer;
{ Creates image from a Codebot bitmap. Returns handle to the image }
function nvgCreateImageBitmap(Ctx: PNVGcontext; Bitmap: IBitmapData; ImageFlags: Integer): Integer;
{ Creates image from specified image data. Returns handle to the image }
function nvgCreateImageRGBA(Ctx: PNVGcontext; W, H, ImageFlags: Integer; Data: PByte): Integer;
{ Updates image data specified by image handle }
procedure nvgUpdateImage(Ctx: PNVGcontext; Image: Integer; Data: PByte);
{ Returns the dimensions of a created image }
procedure nvgImageSize(Ctx: PNVGcontext; Image: Integer; W, H: PInteger);
{ Deletes created image }
procedure nvgDeleteImage(Ctx: PNVGcontext; Image: Integer);

{ Paints

  NanoVG supports four types of paints: linear gradient, box gradient, radial
  gradient and image pattern. These can be used as paints for strokes and
  fills. }

{ Creates and returns a linear gradient. Parameters (sx,sy)-(ex,ey) specify
  the start and end coordinates of the linear gradient, icol specifies the
  start color and ocol the end color }
function nvgLinearGradient(Ctx: PNVGcontext; SX, SY, EX, EY: Single; ICol, OCol: TNVGcolor): TNVGpaint;
{ Creates and returns a box gradient. Box gradient is a feathered rounded
  rectangle, it is useful for rendering drop shadows or highlights for boxes.
  Parameters (x,y) define the top-left corner of the rectangle, (w,h) define
  the size of the rectangle, r defines the corner radius, and f feather.
  Feather defines how blurry the border of the rectangle is }
function nvgBoxGradient(Ctx: PNVGcontext; X, Y, W, H, R, F: Single; ICol, OCol: TNVGcolor): TNVGpaint;
{ Creates and returns a radial gradient. Parameters (cx,cy) specify the
  center, inr and outr specify the inner and outer radius of the gradient,
  icol specifies the start color and ocol the end color }
function nvgRadialGradient(Ctx: PNVGcontext; CX, CY, InR, OutR: Single; ICol, OCol: TNVGcolor): TNVGpaint;
{ Creates and returns an image pattern. Parameters (ox,oy) specify the left-top
  location of the image pattern, (ex,ey) the size of one image, angle rotation
  around the top-left corner, image is handle to the image to render }
function nvgImagePattern(Ctx: PNVGcontext; OX, OY, EX, EY, Angle: Single; Image: Integer; Alpha: Single): TNVGpaint;

{ Scissoring

  Scissoring allows you to clip the rendering into a rectangle. This is
  useful for various user interface cases like rendering a text edit or a
  timeline. }

{ Sets the current scissor rectangle. The scissor rectangle is transformed by
  the current transform }
procedure nvgScissor(Ctx: PNVGcontext; X, Y, W, H: Single);
{ Intersects current scissor rectangle with the specified rectangle. The
  scissor rectangle is transformed by the current transform. Note: in case
  the rotation of previous scissor rect differs from the current one, the
  intersection will be done between the specified rectangle and the previous
  scissor rectangle transformed in the current transform space. The resulting
  shape is always rectangle }
procedure nvgIntersectScissor(Ctx: PNVGcontext; X, Y, W, H: Single);
{ Reset and disables scissoring }
procedure nvgResetScissor(Ctx: PNVGcontext);

{ Paths

  Drawing a new shape starts with nvgBeginPath, it clears all the currently
  defined paths. Then you define one or more paths and sub-paths which
  describe the shape. The are functions to draw common shapes like rectangles
  and circles, and lower level step-by-step functions, which allow to define
  a path curve by curve.

  By default every sub-path is filled as a solid shape, and holes have to be
  marked with nvgPathWinding(NVG_HOLE). This is useful especially for the
  common shapes, which are drawn CCW. With nvgFillRule you can instead use the
  nonzero rule, where sub-paths drawn in the opposite direction make holes, or
  the even-odd rule, where overlapping sub-paths make holes.

  Finally you can fill the path using current fill style by calling nvgFill,
  and stroke it with current stroke style by calling nvgStroke.

  The curve segments and sub-paths are transformed by the current transform. }

{ Clears the current path and sub-paths }
procedure nvgBeginPath(Ctx: PNVGcontext);
{ Starts new sub-path with specified point as first point }
procedure nvgMoveTo(Ctx: PNVGcontext; X, Y: Single);
{ Adds line segment from the last point in the path to the specified point }
procedure nvgLineTo(Ctx: PNVGcontext; X, Y: Single);
{ Adds cubic bezier segment from last point in the path via two control
  points to the specified point }
procedure nvgBezierTo(Ctx: PNVGcontext; C1X, C1Y, C2X, C2Y, X, Y: Single);
{ Adds quadratic bezier segment from last point in the path via a control
  point to the specified point }
procedure nvgQuadTo(Ctx: PNVGcontext; CX, CY, X, Y: Single);
{ Adds an arc segment at the corner defined by the last path point, and two
  specified points }
procedure nvgArcTo(Ctx: PNVGcontext; X1, Y1, X2, Y2, Radius: Single);
{ Closes current sub-path with a line segment }
procedure nvgClosePath(Ctx: PNVGcontext);
{ Sets the current sub-path winding, see NVG_CCW, NVG_CW, NVG_SOLID and NVG_HOLE }
procedure nvgPathWinding(Ctx: PNVGcontext; Dir: Integer);
{ Creates new circle arc shaped sub-path. The arc center is at cx,cy, the
  arc radius is r, and the arc is drawn from angle a0 to a1, and swept in
  direction dir (NVG_CCW, or NVG_CW). Angles are specified in radians }
procedure nvgArc(Ctx: PNVGcontext; CX, CY, R, A0, A1: Single; Dir: Integer);
{ Creates new rectangle shaped sub-path }
procedure nvgRect(Ctx: PNVGcontext; X, Y, W, H: Single);
{ Creates new rounded rectangle shaped sub-path }
procedure nvgRoundedRect(Ctx: PNVGcontext; X, Y, W, H, R: Single);
{ Creates new rounded rectangle shaped sub-path with varying radii for each corner }
procedure nvgRoundedRectVarying(Ctx: PNVGcontext; X, Y, W, H, RadTopLeft, RadTopRight,
  RadBottomRight, RadBottomLeft: Single);
{ Creates new ellipse shaped sub-path }
procedure nvgEllipse(Ctx: PNVGcontext; CX, CY, RX, RY: Single);
{ Creates new circle shaped sub-path }
procedure nvgCircle(Ctx: PNVGcontext; CX, CY, R: Single);
{ Fills the current path with current fill style }
procedure nvgFill(Ctx: PNVGcontext);
{ Fills the current path with current stroke style }
procedure nvgStroke(Ctx: PNVGcontext);

{ Text

  NanoVG allows you to load .ttf files and use the font to render text.

  The appearance of the text can be defined by setting the current text
  style and by specifying the fill color. Common text and font settings such
  as font size, letter spacing and text align are supported. Font blur allows
  you to create simple text effects such as drop shadows.

  At render time the font face can be set based on the font handles or name.

  Font measure functions return values in local space, the calculations are
  carried in the same resolution as the final rendering. This is done because
  the text glyph positions are snapped to the nearest pixels sharp rendering.

  The local space means that values are not rotated or scale as per the
  current transformation. For example if you set font size to 12, which
  would mean that line height is 16, then regardless of the current scaling
  and rotation, the returned line height is always 16. Some measures may vary
  because of the scaling since aforementioned pixel snapping.

  While this may sound a little odd, the setup allows you to always render
  the same way regardless of scaling. I.e. following works regardless of
  scaling:

    nvgTextBounds(Vg, X, Y, 'Text me.', nil, @Bounds);
    nvgBeginPath(Vg);
    nvgRect(Vg, Bounds[0], Bounds[1], Bounds[2] - Bounds[0], Bounds[3] - Bounds[1]);
    nvgFill(Vg);

  Note: currently only solid color fill is supported for text. }

{ Creates font by loading it from the disk from specified file name.
  Returns handle to the font }
function nvgCreateFont(Ctx: PNVGcontext; const Name, FileName: string): Integer;
{ FontIndex specifies which font face to load from a .ttf/.ttc file }
function nvgCreateFontAtIndex(Ctx: PNVGcontext; const Name, FileName: string; FontIndex: Integer): Integer;
{ Creates font by loading it from the specified memory chunk. Returns handle
  to the font. The memory must remain valid while the font is used. When
  FreeData is True it is freed with FreeMem when the context is deleted. }
function nvgCreateFontMem(Ctx: PNVGcontext; const Name: string; Data: PByte; NData: Integer; FreeData: Boolean): Integer;
{ FontIndex specifies which font face to load from a .ttf/.ttc file }
function nvgCreateFontMemAtIndex(Ctx: PNVGcontext; const Name: string; Data: PByte; NData: Integer;
  FreeData: Boolean; FontIndex: Integer): Integer;
{ Finds a loaded font of specified name, and returns handle to it, or -1 if
  the font is not found }
function nvgFindFont(Ctx: PNVGcontext; const Name: string): Integer;
{ Adds a fallback font by handle }
function nvgAddFallbackFontId(Ctx: PNVGcontext; BaseFont, FallbackFont: Integer): Integer;
{ Adds a fallback font by name }
function nvgAddFallbackFont(Ctx: PNVGcontext; const BaseFont, FallbackFont: string): Integer;
{ Resets fallback fonts by handle }
procedure nvgResetFallbackFontsId(Ctx: PNVGcontext; BaseFont: Integer);
{ Resets fallback fonts by name }
procedure nvgResetFallbackFonts(Ctx: PNVGcontext; const BaseFont: string);
{ Sets the font size of current text style }
procedure nvgFontSize(Ctx: PNVGcontext; Size: Single);
{ Sets the blur of current text style }
procedure nvgFontBlur(Ctx: PNVGcontext; Blur: Single);
{ Sets the letter spacing of current text style }
procedure nvgTextLetterSpacing(Ctx: PNVGcontext; Spacing: Single);
{ Sets the proportional line height of current text style. The line height
  is specified as multiple of font size }
procedure nvgTextLineHeight(Ctx: PNVGcontext; LineHeight: Single);
{ Sets the text align of current text style, see NVG_ALIGN_LEFT and the rest }
procedure nvgTextAlign(Ctx: PNVGcontext; Align: Integer);
{ Sets the font face based on specified id of current text style }
procedure nvgFontFaceId(Ctx: PNVGcontext; Font: Integer);
{ Sets the font face based on specified name of current text style }
procedure nvgFontFace(Ctx: PNVGcontext; const Font: string);
{ Draws text string at specified location. If EndStr is specified only the
  sub-string up to the end is drawn }
function nvgText(Ctx: PNVGcontext; X, Y: Single; Str, EndStr: PAnsiChar): Single; overload;
function nvgText(Ctx: PNVGcontext; X, Y: Single; const S: string): Single; overload;
{ Draws multi-line text string at specified location wrapped at the
  specified width. If EndStr is specified only the sub-string up to the end
  is drawn. White space is stripped at the beginning of the rows, the text is
  split at word boundaries or when new-line characters are encountered.
  Words longer than the max width are slit at nearest character (i.e. no
  hyphenation) }
procedure nvgTextBox(Ctx: PNVGcontext; X, Y, BreakRowWidth: Single; Str, EndStr: PAnsiChar); overload;
procedure nvgTextBox(Ctx: PNVGcontext; X, Y, BreakRowWidth: Single; const S: string); overload;
{ Measures the specified text string. Parameter bounds should be a pointer to
  float[4], if the bounding box of the text should be returned. The bounds
  value are [xmin,ymin, xmax,ymax]. Returns the horizontal advance of the
  measured text (i.e. where the next character should drawn). Measured values
  are returned in local coordinate space }
function nvgTextBounds(Ctx: PNVGcontext; X, Y: Single; Str, EndStr: PAnsiChar; Bounds: PSingle): Single; overload;
function nvgTextBounds(Ctx: PNVGcontext; X, Y: Single; const S: string; Bounds: PSingle): Single; overload;
{ Measures the specified multi-text string. Parameter bounds should be a
  pointer to float[4], if the bounding box of the text should be returned.
  The bounds value are [xmin,ymin, xmax,ymax]. Measured values are returned
  in local coordinate space }
procedure nvgTextBoxBounds(Ctx: PNVGcontext; X, Y, BreakRowWidth: Single; Str, EndStr: PAnsiChar; Bounds: PSingle); overload;
procedure nvgTextBoxBounds(Ctx: PNVGcontext; X, Y, BreakRowWidth: Single; const S: string; Bounds: PSingle); overload;
{ Calculates the glyph x positions of the specified text. If EndStr is
  specified only the sub-string will be used. Measured values are returned in
  local coordinate space }
function nvgTextGlyphPositions(Ctx: PNVGcontext; X, Y: Single; Str, EndStr: PAnsiChar;
  Positions: PNVGglyphPosition; MaxPositions: Integer): Integer;
{ Returns the vertical metrics based on the current text style. Measured
  values are returned in local coordinate space }
procedure nvgTextMetrics(Ctx: PNVGcontext; Ascender, Descender, LineH: PSingle);
{ Breaks the specified text into lines. If EndStr is specified only the
  sub-string will be used. White space is stripped at the beginning of the
  rows, the text is split at word boundaries or when new-line characters are
  encountered. Words longer than the max width are slit at nearest character
  (i.e. no hyphenation) }
function nvgTextBreakLines(Ctx: PNVGcontext; Str, EndStr: PAnsiChar; BreakRowWidth: Single;
  Rows: PNVGtextRow; MaxRows: Integer): Integer;

{ Internal render API }

function nvgCreateInternal(Params: PNVGparams): PNVGcontext;
{ Free a context }
procedure nvgDeleteInternal(Ctx: PNVGcontext);
{ The renderer callbacks of a context }
function nvgInternalParams(Ctx: PNVGcontext): PNVGparams;
{ Debug function to dump cached path data }
procedure nvgDebugDumpPathCache(Ctx: PNVGcontext);

implementation

uses
  SysUtils, Classes, Math;

const
  NVG_INIT_FONTIMAGE_SIZE = 512;
  NVG_MAX_FONTIMAGE_SIZE = 2048;

  NVG_INIT_COMMANDS_SIZE = 256;
  NVG_INIT_POINTS_SIZE = 128;
  NVG_INIT_PATHS_SIZE = 16;
  NVG_INIT_VERTS_SIZE = 256;

  { Length proportional to radius of a cubic bezier handle for 90deg arcs }
  NVG_KAPPA90 = 0.5522847493;

  { NVGcommands }
  NVG_MOVETO = 0;
  NVG_LINETO = 1;
  NVG_BEZIERTO = 2;
  NVG_CLOSE = 3;
  NVG_WINDING = 4;

  { NVGpointFlags }
  NVG_PT_CORNER = $01;
  NVG_PT_LEFT = $02;
  NVG_PT_BEVEL = $04;
  NVG_PR_INNERBEVEL = $08;

function nvg__sqrtf(A: Single): Single; inline;
begin
  Result := Sqrt(A);
end;

function nvg__modf(A, B: Single): Single; inline;
begin
  Result := FMod(A, B);
end;

function nvg__sinf(A: Single): Single; inline;
begin
  Result := Sin(A);
end;

function nvg__cosf(A: Single): Single; inline;
begin
  Result := Cos(A);
end;

function nvg__tanf(A: Single): Single; inline;
begin
  Result := Tan(A);
end;

function nvg__atan2f(A, B: Single): Single; inline;
begin
  Result := ArcTan2(A, B);
end;

function nvg__acosf(A: Single): Single; inline;
begin
  Result := ArcCos(A);
end;

function nvg__mini(A, B: Integer): Integer; inline;
begin
  if A < B then Result := A else Result := B;
end;

function nvg__maxi(A, B: Integer): Integer; inline;
begin
  if A > B then Result := A else Result := B;
end;

function nvg__clampi(A, Mn, Mx: Integer): Integer; inline;
begin
  if A < Mn then Result := Mn else if A > Mx then Result := Mx else Result := A;
end;

function nvg__minf(A, B: Single): Single; inline;
begin
  if A < B then Result := A else Result := B;
end;

function nvg__maxf(A, B: Single): Single; inline;
begin
  if A > B then Result := A else Result := B;
end;

function nvg__absf(A: Single): Single; inline;
begin
  if A >= 0 then Result := A else Result := -A;
end;

function nvg__signf(A: Single): Single; inline;
begin
  if A >= 0 then Result := 1 else Result := -1;
end;

function nvg__clampf(A, Mn, Mx: Single): Single; inline;
begin
  if A < Mn then Result := Mn else if A > Mx then Result := Mx else Result := A;
end;

function nvg__cross(DX0, DY0, DX1, DY1: Single): Single; inline;
begin
  Result := DX1 * DY0 - DX0 * DY1;
end;

function nvg__normalize(var X, Y: Single): Single;
var
  D, ID: Single;
begin
  D := nvg__sqrtf(X * X + Y * Y);
  if D > 1e-6 then
  begin
    ID := 1.0 / D;
    X := X * ID;
    Y := Y * ID;
  end;
  Result := D;
end;

procedure nvg__deletePathCache(C: PNVGpathCache);
begin
  if C = nil then
    Exit;
  if C.Points <> nil then
    FreeMem(C.Points);
  if C.Paths <> nil then
    FreeMem(C.Paths);
  if C.Verts <> nil then
    FreeMem(C.Verts);
  FreeMem(C);
end;

function nvg__allocPathCache: PNVGpathCache;
var
  C: PNVGpathCache;
begin
  C := AllocMem(SizeOf(TNVGpathCache));
  C.Points := GetMem(SizeOf(TNVGpoint) * NVG_INIT_POINTS_SIZE);
  C.NPoints := 0;
  C.CPoints := NVG_INIT_POINTS_SIZE;
  C.Paths := GetMem(SizeOf(TNVGpath) * NVG_INIT_PATHS_SIZE);
  C.NPaths := 0;
  C.CPaths := NVG_INIT_PATHS_SIZE;
  C.Verts := GetMem(SizeOf(TNVGvertex) * NVG_INIT_VERTS_SIZE);
  C.NVerts := 0;
  C.CVerts := NVG_INIT_VERTS_SIZE;
  Result := C;
end;

procedure nvg__setDevicePixelRatio(Ctx: PNVGcontext; Ratio: Single);
begin
  Ctx.TessTol := 0.25 / Ratio;
  Ctx.DistTol := 0.01 / Ratio;
  Ctx.FringeWidth := 1.0 / Ratio;
  Ctx.DevicePxRatio := Ratio;
end;

function nvg__compositeOperationState(Op: Integer): TNVGcompositeOperationState;
var
  SFactor, DFactor: Integer;
begin
  case Op of
    NVG_SOURCE_OVER:
      begin
        SFactor := NVG_ONE;
        DFactor := NVG_ONE_MINUS_SRC_ALPHA;
      end;
    NVG_SOURCE_IN:
      begin
        SFactor := NVG_DST_ALPHA;
        DFactor := NVG_ZERO;
      end;
    NVG_SOURCE_OUT:
      begin
        SFactor := NVG_ONE_MINUS_DST_ALPHA;
        DFactor := NVG_ZERO;
      end;
    NVG_ATOP:
      begin
        SFactor := NVG_DST_ALPHA;
        DFactor := NVG_ONE_MINUS_SRC_ALPHA;
      end;
    NVG_DESTINATION_OVER:
      begin
        SFactor := NVG_ONE_MINUS_DST_ALPHA;
        DFactor := NVG_ONE;
      end;
    NVG_DESTINATION_IN:
      begin
        SFactor := NVG_ZERO;
        DFactor := NVG_SRC_ALPHA;
      end;
    NVG_DESTINATION_OUT:
      begin
        SFactor := NVG_ZERO;
        DFactor := NVG_ONE_MINUS_SRC_ALPHA;
      end;
    NVG_DESTINATION_ATOP:
      begin
        SFactor := NVG_ONE_MINUS_DST_ALPHA;
        DFactor := NVG_SRC_ALPHA;
      end;
    NVG_LIGHTER:
      begin
        SFactor := NVG_ONE;
        DFactor := NVG_ONE;
      end;
    NVG_COPY:
      begin
        SFactor := NVG_ONE;
        DFactor := NVG_ZERO;
      end;
    NVG_XOR:
      begin
        SFactor := NVG_ONE_MINUS_DST_ALPHA;
        DFactor := NVG_ONE_MINUS_SRC_ALPHA;
      end;
  else
    SFactor := NVG_ONE;
    DFactor := NVG_ZERO;
  end;
  Result.SrcRGB := SFactor;
  Result.DstRGB := DFactor;
  Result.SrcAlpha := SFactor;
  Result.DstAlpha := DFactor;
end;

function nvg__getState(Ctx: PNVGcontext): PNVGstate; inline;
begin
  Result := @Ctx.States[Ctx.NStates - 1];
end;

function nvgCreateInternal(Params: PNVGparams): PNVGcontext;
var
  FontParams: TFonsParams;
  Ctx: PNVGcontext;
  I: Integer;
begin
  Ctx := AllocMem(SizeOf(TNVGcontext));
  Ctx.Params := Params^;
  for I := 0 to NVG_MAX_FONTIMAGES - 1 do
    Ctx.FontImages[I] := 0;
  Ctx.Commands := GetMem(SizeOf(Single) * NVG_INIT_COMMANDS_SIZE);
  Ctx.NCommands := 0;
  Ctx.CCommands := NVG_INIT_COMMANDS_SIZE;
  Ctx.Cache := nvg__allocPathCache;
  nvgSave(Ctx);
  nvgReset(Ctx);
  nvg__setDevicePixelRatio(Ctx, 1.0);
  if Ctx.Params.RenderCreate(Ctx.Params.UserPtr) = 0 then
  begin
    nvgDeleteInternal(Ctx);
    Exit(nil);
  end;
  { Init font rendering }
  FillChar(FontParams, SizeOf(FontParams), 0);
  FontParams.Width := NVG_INIT_FONTIMAGE_SIZE;
  FontParams.Height := NVG_INIT_FONTIMAGE_SIZE;
  FontParams.Flags := FONS_ZERO_TOPLEFT;
  Ctx.Fs := fonsCreateInternal(@FontParams);
  if Ctx.Fs = nil then
  begin
    nvgDeleteInternal(Ctx);
    Exit(nil);
  end;
  { Create font texture }
  Ctx.FontImages[0] := Ctx.Params.RenderCreateTexture(Ctx.Params.UserPtr, NVG_TEXTURE_ALPHA,
    FontParams.Width, FontParams.Height, 0, nil);
  if Ctx.FontImages[0] = 0 then
  begin
    nvgDeleteInternal(Ctx);
    Exit(nil);
  end;
  Ctx.FontImageIdx := 0;
  Result := Ctx;
end;

function nvgInternalParams(Ctx: PNVGcontext): PNVGparams;
begin
  Result := @Ctx.Params;
end;

procedure nvgDeleteInternal(Ctx: PNVGcontext);
var
  I: Integer;
begin
  if Ctx = nil then
    Exit;
  if Ctx.Commands <> nil then
    FreeMem(Ctx.Commands);
  if Ctx.Cache <> nil then
    nvg__deletePathCache(Ctx.Cache);
  if Ctx.Fs <> nil then
    fonsDeleteInternal(Ctx.Fs);
  for I := 0 to NVG_MAX_FONTIMAGES - 1 do
    if Ctx.FontImages[I] <> 0 then
    begin
      nvgDeleteImage(Ctx, Ctx.FontImages[I]);
      Ctx.FontImages[I] := 0;
    end;
  if Assigned(Ctx.Params.RenderDelete) then
    Ctx.Params.RenderDelete(Ctx.Params.UserPtr);
  FreeMem(Ctx);
end;

procedure nvgBeginFrame(Ctx: PNVGcontext; WindowWidth, WindowHeight, DevicePixelRatio: Single);
begin
  Ctx.NStates := 0;
  nvgSave(Ctx);
  nvgReset(Ctx);
  nvg__setDevicePixelRatio(Ctx, DevicePixelRatio);
  Ctx.Params.RenderViewport(Ctx.Params.UserPtr, WindowWidth, WindowHeight, DevicePixelRatio);
  Ctx.DrawCallCount := 0;
  Ctx.FillTriCount := 0;
  Ctx.StrokeTriCount := 0;
  Ctx.TextTriCount := 0;
end;

procedure nvgCancelFrame(Ctx: PNVGcontext);
begin
  Ctx.Params.RenderCancel(Ctx.Params.UserPtr);
end;

procedure nvgEndFrame(Ctx: PNVGcontext);
var
  FontImage, I, J, IW, IH, NW, NH, Image: Integer;
begin
  Ctx.Params.RenderFlush(Ctx.Params.UserPtr);
  if Ctx.FontImageIdx <> 0 then
  begin
    FontImage := Ctx.FontImages[Ctx.FontImageIdx];
    Ctx.FontImages[Ctx.FontImageIdx] := 0;
    { Delete images that smaller than current one }
    if FontImage = 0 then
      Exit;
    nvgImageSize(Ctx, FontImage, @IW, @IH);
    J := 0;
    for I := 0 to Ctx.FontImageIdx - 1 do
      if Ctx.FontImages[I] <> 0 then
      begin
        Image := Ctx.FontImages[I];
        Ctx.FontImages[I] := 0;
        nvgImageSize(Ctx, Image, @NW, @NH);
        if (NW < IW) or (NH < IH) then
          nvgDeleteImage(Ctx, Image)
        else
        begin
          Ctx.FontImages[J] := Image;
          Inc(J);
        end;
      end;
    { Make current font image to first }
    Ctx.FontImages[J] := Ctx.FontImages[0];
    Ctx.FontImages[0] := FontImage;
    Ctx.FontImageIdx := 0;
  end;
end;

function nvgRGB(R, G, B: Byte): TNVGcolor;
begin
  Result := nvgRGBA(R, G, B, 255);
end;

function nvgRGBf(R, G, B: Single): TNVGcolor;
begin
  Result := nvgRGBAf(R, G, B, 1.0);
end;

function nvgRGBA(R, G, B, A: Byte): TNVGcolor;
begin
  Result.R := R / 255.0;
  Result.G := G / 255.0;
  Result.B := B / 255.0;
  Result.A := A / 255.0;
end;

function nvgRGBAf(R, G, B, A: Single): TNVGcolor;
begin
  Result.R := R;
  Result.G := G;
  Result.B := B;
  Result.A := A;
end;

function nvgTransRGBA(C0: TNVGcolor; A: Byte): TNVGcolor;
begin
  Result := C0;
  Result.A := A / 255.0;
end;

function nvgTransRGBAf(C0: TNVGcolor; A: Single): TNVGcolor;
begin
  Result := C0;
  Result.A := A;
end;

function nvgLerpRGBA(C0, C1: TNVGcolor; U: Single): TNVGcolor;
var
  I: Integer;
  OneMinU: Single;
begin
  U := nvg__clampf(U, 0.0, 1.0);
  OneMinU := 1.0 - U;
  for I := 0 to 3 do
    Result.RGBA[I] := C0.RGBA[I] * OneMinU + C1.RGBA[I] * U;
end;

function nvgHSL(H, S, L: Single): TNVGcolor;
begin
  Result := nvgHSLA(H, S, L, 255);
end;

function nvg__hue(H, M1, M2: Single): Single;
begin
  if H < 0 then
    H := H + 1;
  if H > 1 then
    H := H - 1;
  if H < 1.0 / 6.0 then
    Result := M1 + (M2 - M1) * H * 6.0
  else if H < 3.0 / 6.0 then
    Result := M2
  else if H < 4.0 / 6.0 then
    Result := M1 + (M2 - M1) * (2.0 / 3.0 - H) * 6.0
  else
    Result := M1;
end;

function nvgHSLA(H, S, L: Single; A: Byte): TNVGcolor;
var
  M1, M2: Single;
begin
  H := nvg__modf(H, 1.0);
  if H < 0.0 then
    H := H + 1.0;
  S := nvg__clampf(S, 0.0, 1.0);
  L := nvg__clampf(L, 0.0, 1.0);
  if L <= 0.5 then
    M2 := L * (1 + S)
  else
    M2 := L + S - L * S;
  M1 := 2 * L - M2;
  Result.R := nvg__clampf(nvg__hue(H + 1.0 / 3.0, M1, M2), 0.0, 1.0);
  Result.G := nvg__clampf(nvg__hue(H, M1, M2), 0.0, 1.0);
  Result.B := nvg__clampf(nvg__hue(H - 1.0 / 3.0, M1, M2), 0.0, 1.0);
  Result.A := A / 255.0;
end;

function nvgColor(Color: TColorB): TNVGcolor;
begin
  Result := nvgRGBA(Color.Red, Color.Green, Color.Blue, Color.Alpha);
end;

procedure nvgTransformIdentity(out Dst: TNVGxform);
begin
  Dst[0] := 1.0; Dst[1] := 0.0;
  Dst[2] := 0.0; Dst[3] := 1.0;
  Dst[4] := 0.0; Dst[5] := 0.0;
end;

procedure nvgTransformTranslate(out Dst: TNVGxform; TX, TY: Single);
begin
  Dst[0] := 1.0; Dst[1] := 0.0;
  Dst[2] := 0.0; Dst[3] := 1.0;
  Dst[4] := TX; Dst[5] := TY;
end;

procedure nvgTransformScale(out Dst: TNVGxform; SX, SY: Single);
begin
  Dst[0] := SX; Dst[1] := 0.0;
  Dst[2] := 0.0; Dst[3] := SY;
  Dst[4] := 0.0; Dst[5] := 0.0;
end;

procedure nvgTransformRotate(out Dst: TNVGxform; A: Single);
var
  CS, SN: Single;
begin
  CS := nvg__cosf(A);
  SN := nvg__sinf(A);
  Dst[0] := CS; Dst[1] := SN;
  Dst[2] := -SN; Dst[3] := CS;
  Dst[4] := 0.0; Dst[5] := 0.0;
end;

procedure nvgTransformSkewX(out Dst: TNVGxform; A: Single);
begin
  Dst[0] := 1.0; Dst[1] := 0.0;
  Dst[2] := nvg__tanf(A); Dst[3] := 1.0;
  Dst[4] := 0.0; Dst[5] := 0.0;
end;

procedure nvgTransformSkewY(out Dst: TNVGxform; A: Single);
begin
  Dst[0] := 1.0; Dst[1] := nvg__tanf(A);
  Dst[2] := 0.0; Dst[3] := 1.0;
  Dst[4] := 0.0; Dst[5] := 0.0;
end;

procedure nvgTransformMultiply(var Dst: TNVGxform; const Src: TNVGxform);
var
  T0, T2, T4: Single;
begin
  T0 := Dst[0] * Src[0] + Dst[1] * Src[2];
  T2 := Dst[2] * Src[0] + Dst[3] * Src[2];
  T4 := Dst[4] * Src[0] + Dst[5] * Src[2] + Src[4];
  Dst[1] := Dst[0] * Src[1] + Dst[1] * Src[3];
  Dst[3] := Dst[2] * Src[1] + Dst[3] * Src[3];
  Dst[5] := Dst[4] * Src[1] + Dst[5] * Src[3] + Src[5];
  Dst[0] := T0;
  Dst[2] := T2;
  Dst[4] := T4;
end;

procedure nvgTransformPremultiply(var Dst: TNVGxform; const Src: TNVGxform);
var
  S2: TNVGxform;
begin
  S2 := Src;
  nvgTransformMultiply(S2, Dst);
  Dst := S2;
end;

function nvgTransformInverse(out Dst: TNVGxform; const Src: TNVGxform): Integer;
var
  T0, T1, T2, T3, T4, T5, InvDet, Det: Double;
begin
  { The determinant and inverse are calculated in double precision }
  T0 := Src[0]; T1 := Src[1]; T2 := Src[2];
  T3 := Src[3]; T4 := Src[4]; T5 := Src[5];
  Det := T0 * T3 - T2 * T1;
  if (Det > -1e-6) and (Det < 1e-6) then
  begin
    nvgTransformIdentity(Dst);
    Exit(0);
  end;
  InvDet := 1.0 / Det;
  Dst[0] := T3 * InvDet;
  Dst[2] := -T2 * InvDet;
  Dst[4] := (T2 * T5 - T3 * T4) * InvDet;
  Dst[1] := -T1 * InvDet;
  Dst[3] := T0 * InvDet;
  Dst[5] := (T1 * T4 - T0 * T5) * InvDet;
  Result := 1;
end;

procedure nvgTransformPoint(out DstX, DstY: Single; const XForm: TNVGxform; SrcX, SrcY: Single);
begin
  DstX := SrcX * XForm[0] + SrcY * XForm[2] + XForm[4];
  DstY := SrcX * XForm[1] + SrcY * XForm[3] + XForm[5];
end;

function nvgDegToRad(Deg: Single): Single;
begin
  Result := Deg / 180.0 * NVG_PI;
end;

function nvgRadToDeg(Rad: Single): Single;
begin
  Result := Rad / NVG_PI * 180.0;
end;

procedure nvg__setPaintColor(var P: TNVGpaint; Color: TNVGcolor);
begin
  FillChar(P, SizeOf(P), 0);
  nvgTransformIdentity(P.XForm);
  P.Radius := 0.0;
  P.Feather := 1.0;
  P.InnerColor := Color;
  P.OuterColor := Color;
end;

{ State handling }

procedure nvgSave(Ctx: PNVGcontext);
begin
  if Ctx.NStates >= NVG_MAX_STATES then
    Exit;
  if Ctx.NStates > 0 then
    Ctx.States[Ctx.NStates] := Ctx.States[Ctx.NStates - 1];
  Inc(Ctx.NStates);
end;

procedure nvgRestore(Ctx: PNVGcontext);
begin
  if Ctx.NStates <= 1 then
    Exit;
  Dec(Ctx.NStates);
end;

procedure nvgReset(Ctx: PNVGcontext);
var
  State: PNVGstate;
begin
  State := nvg__getState(Ctx);
  FillChar(State^, SizeOf(State^), 0);
  nvg__setPaintColor(State.Fill, nvgRGBA(255, 255, 255, 255));
  nvg__setPaintColor(State.Stroke, nvgRGBA(0, 0, 0, 255));
  State.CompositeOperation := nvg__compositeOperationState(NVG_SOURCE_OVER);
  State.ShapeAntiAlias := 1;
  State.StrokeWidth := 1.0;
  State.MiterLimit := 10.0;
  State.LineCap := NVG_BUTT;
  State.LineJoin := NVG_MITER;
  State.Alpha := 1.0;
  nvgTransformIdentity(State.XForm);
  State.Scissor.Extent[0] := -1.0;
  State.Scissor.Extent[1] := -1.0;
  State.FillRule := NVG_FILL_WINDING;
  State.FontSize := 16.0;
  State.LetterSpacing := 0.0;
  State.LineHeight := 1.0;
  State.FontBlur := 0.0;
  State.TextAlign := NVG_ALIGN_LEFT or NVG_ALIGN_BASELINE;
  State.FontId := 0;
end;

{ State setting }

procedure nvgShapeAntiAlias(Ctx: PNVGcontext; Enabled: Integer);
begin
  nvg__getState(Ctx).ShapeAntiAlias := Enabled;
end;

procedure nvgFillRule(Ctx: PNVGcontext; Rule: Integer);
begin
  nvg__getState(Ctx).FillRule := Rule;
end;

procedure nvgStrokeWidth(Ctx: PNVGcontext; Size: Single);
begin
  nvg__getState(Ctx).StrokeWidth := Size;
end;

procedure nvgMiterLimit(Ctx: PNVGcontext; Limit: Single);
begin
  nvg__getState(Ctx).MiterLimit := Limit;
end;

procedure nvgLineCap(Ctx: PNVGcontext; Cap: Integer);
begin
  nvg__getState(Ctx).LineCap := Cap;
end;

procedure nvgLineJoin(Ctx: PNVGcontext; Join: Integer);
begin
  nvg__getState(Ctx).LineJoin := Join;
end;

procedure nvgGlobalAlpha(Ctx: PNVGcontext; Alpha: Single);
begin
  nvg__getState(Ctx).Alpha := Alpha;
end;

procedure nvgTransform(Ctx: PNVGcontext; A, B, C, D, E, F: Single);
var
  T: TNVGxform;
begin
  T[0] := A; T[1] := B; T[2] := C; T[3] := D; T[4] := E; T[5] := F;
  nvgTransformPremultiply(nvg__getState(Ctx).XForm, T);
end;

procedure nvgResetTransform(Ctx: PNVGcontext);
begin
  nvgTransformIdentity(nvg__getState(Ctx).XForm);
end;

procedure nvgTranslate(Ctx: PNVGcontext; X, Y: Single);
var
  T: TNVGxform;
begin
  nvgTransformTranslate(T, X, Y);
  nvgTransformPremultiply(nvg__getState(Ctx).XForm, T);
end;

procedure nvgRotate(Ctx: PNVGcontext; Angle: Single);
var
  T: TNVGxform;
begin
  nvgTransformRotate(T, Angle);
  nvgTransformPremultiply(nvg__getState(Ctx).XForm, T);
end;

procedure nvgSkewX(Ctx: PNVGcontext; Angle: Single);
var
  T: TNVGxform;
begin
  nvgTransformSkewX(T, Angle);
  nvgTransformPremultiply(nvg__getState(Ctx).XForm, T);
end;

procedure nvgSkewY(Ctx: PNVGcontext; Angle: Single);
var
  T: TNVGxform;
begin
  nvgTransformSkewY(T, Angle);
  nvgTransformPremultiply(nvg__getState(Ctx).XForm, T);
end;

procedure nvgScale(Ctx: PNVGcontext; X, Y: Single);
var
  T: TNVGxform;
begin
  nvgTransformScale(T, X, Y);
  nvgTransformPremultiply(nvg__getState(Ctx).XForm, T);
end;

procedure nvgCurrentTransform(Ctx: PNVGcontext; out XForm: TNVGxform);
begin
  XForm := nvg__getState(Ctx).XForm;
end;

procedure nvgStrokeColor(Ctx: PNVGcontext; Color: TNVGcolor);
begin
  nvg__setPaintColor(nvg__getState(Ctx).Stroke, Color);
end;

procedure nvgStrokePaint(Ctx: PNVGcontext; Paint: TNVGpaint);
var
  State: PNVGstate;
begin
  State := nvg__getState(Ctx);
  State.Stroke := Paint;
  nvgTransformMultiply(State.Stroke.XForm, State.XForm);
end;

procedure nvgFillColor(Ctx: PNVGcontext; Color: TNVGcolor);
begin
  nvg__setPaintColor(nvg__getState(Ctx).Fill, Color);
end;

procedure nvgFillPaint(Ctx: PNVGcontext; Paint: TNVGpaint);
var
  State: PNVGstate;
begin
  State := nvg__getState(Ctx);
  State.Fill := Paint;
  nvgTransformMultiply(State.Fill.XForm, State.XForm);
end;

{ Images }

function nvgCreateImageBitmap(Ctx: PNVGcontext; Bitmap: IBitmapData; ImageFlags: Integer): Integer;
var
  W, H, I: Integer;
  Src: PColorB;
  Data, Dst: PByte;
begin
  Result := 0;
  if (Bitmap = nil) or (Bitmap.Width < 1) or (Bitmap.Height < 1) then
    Exit;
  W := Bitmap.Width;
  H := Bitmap.Height;
  Src := Bitmap.Pixels;
  if Src = nil then
    Exit;
  { Codebot bitmaps store premultiplied pixels with named color channels, which
    are copied in RGBA order }
  Data := GetMem(W * H * 4);
  try
    Dst := Data;
    for I := 0 to W * H - 1 do
    begin
      Dst[0] := Src.Red;
      Dst[1] := Src.Green;
      Dst[2] := Src.Blue;
      Dst[3] := Src.Alpha;
      Inc(Dst, 4);
      Inc(Src);
    end;
    Result := nvgCreateImageRGBA(Ctx, W, H, ImageFlags or NVG_IMAGE_PREMULTIPLIED, Data);
  finally
    FreeMem(Data);
  end;
end;

function nvgCreateImage(Ctx: PNVGcontext; const FileName: string; ImageFlags: Integer): Integer;
var
  Bitmap: IBitmapData;
begin
  Result := 0;
  if not FileExists(FileName) then
    Exit;
  Bitmap := NewBitmapData;
  try
    Bitmap.LoadFromFile(FileName);
  except
    Exit;
  end;
  Result := nvgCreateImageBitmap(Ctx, Bitmap, ImageFlags);
end;

function nvgCreateImageMem(Ctx: PNVGcontext; ImageFlags: Integer; Data: PByte; NData: Integer): Integer;
var
  Bitmap: IBitmapData;
  Stream: TMemoryStream;
begin
  Result := 0;
  if (Data = nil) or (NData < 1) then
    Exit;
  Bitmap := NewBitmapData;
  Stream := TMemoryStream.Create;
  try
    Stream.Write(Data^, NData);
    Stream.Position := 0;
    try
      Bitmap.LoadFromStream(Stream);
    except
      Exit;
    end;
  finally
    Stream.Free;
  end;
  Result := nvgCreateImageBitmap(Ctx, Bitmap, ImageFlags);
end;

function nvgCreateImageRGBA(Ctx: PNVGcontext; W, H, ImageFlags: Integer; Data: PByte): Integer;
begin
  Result := Ctx.Params.RenderCreateTexture(Ctx.Params.UserPtr, NVG_TEXTURE_RGBA, W, H, ImageFlags, Data);
end;

procedure nvgUpdateImage(Ctx: PNVGcontext; Image: Integer; Data: PByte);
var
  W, H: Integer;
begin
  Ctx.Params.RenderGetTextureSize(Ctx.Params.UserPtr, Image, @W, @H);
  Ctx.Params.RenderUpdateTexture(Ctx.Params.UserPtr, Image, 0, 0, W, H, Data);
end;

procedure nvgImageSize(Ctx: PNVGcontext; Image: Integer; W, H: PInteger);
begin
  Ctx.Params.RenderGetTextureSize(Ctx.Params.UserPtr, Image, W, H);
end;

procedure nvgDeleteImage(Ctx: PNVGcontext; Image: Integer);
begin
  Ctx.Params.RenderDeleteTexture(Ctx.Params.UserPtr, Image);
end;

{ Paints }

function nvgLinearGradient(Ctx: PNVGcontext; SX, SY, EX, EY: Single; ICol, OCol: TNVGcolor): TNVGpaint;
const
  Large = 1e5;
var
  DX, DY, D: Single;
begin
  FillChar(Result, SizeOf(Result), 0);
  { Calculate transform aligned to the line }
  DX := EX - SX;
  DY := EY - SY;
  D := Sqrt(DX * DX + DY * DY);
  if D > 0.0001 then
  begin
    DX := DX / D;
    DY := DY / D;
  end
  else
  begin
    DX := 0;
    DY := 1;
  end;
  Result.XForm[0] := DY; Result.XForm[1] := -DX;
  Result.XForm[2] := DX; Result.XForm[3] := DY;
  Result.XForm[4] := SX - DX * Large; Result.XForm[5] := SY - DY * Large;
  Result.Extent[0] := Large;
  Result.Extent[1] := Large + D * 0.5;
  Result.Radius := 0.0;
  Result.Feather := nvg__maxf(1.0, D);
  Result.InnerColor := ICol;
  Result.OuterColor := OCol;
end;

function nvgRadialGradient(Ctx: PNVGcontext; CX, CY, InR, OutR: Single; ICol, OCol: TNVGcolor): TNVGpaint;
var
  R, F: Single;
begin
  R := (InR + OutR) * 0.5;
  F := OutR - InR;
  FillChar(Result, SizeOf(Result), 0);
  nvgTransformIdentity(Result.XForm);
  Result.XForm[4] := CX;
  Result.XForm[5] := CY;
  Result.Extent[0] := R;
  Result.Extent[1] := R;
  Result.Radius := R;
  Result.Feather := nvg__maxf(1.0, F);
  Result.InnerColor := ICol;
  Result.OuterColor := OCol;
end;

function nvgBoxGradient(Ctx: PNVGcontext; X, Y, W, H, R, F: Single; ICol, OCol: TNVGcolor): TNVGpaint;
begin
  FillChar(Result, SizeOf(Result), 0);
  nvgTransformIdentity(Result.XForm);
  Result.XForm[4] := X + W * 0.5;
  Result.XForm[5] := Y + H * 0.5;
  Result.Extent[0] := W * 0.5;
  Result.Extent[1] := H * 0.5;
  Result.Radius := R;
  Result.Feather := nvg__maxf(1.0, F);
  Result.InnerColor := ICol;
  Result.OuterColor := OCol;
end;

function nvgImagePattern(Ctx: PNVGcontext; OX, OY, EX, EY, Angle: Single; Image: Integer; Alpha: Single): TNVGpaint;
begin
  FillChar(Result, SizeOf(Result), 0);
  nvgTransformRotate(Result.XForm, Angle);
  Result.XForm[4] := OX;
  Result.XForm[5] := OY;
  Result.Extent[0] := EX;
  Result.Extent[1] := EY;
  Result.Image := Image;
  Result.InnerColor := nvgRGBAf(1, 1, 1, Alpha);
  Result.OuterColor := Result.InnerColor;
end;

{ Scissoring }

procedure nvgScissor(Ctx: PNVGcontext; X, Y, W, H: Single);
var
  State: PNVGstate;
begin
  State := nvg__getState(Ctx);
  W := nvg__maxf(0.0, W);
  H := nvg__maxf(0.0, H);
  nvgTransformIdentity(State.Scissor.XForm);
  State.Scissor.XForm[4] := X + W * 0.5;
  State.Scissor.XForm[5] := Y + H * 0.5;
  nvgTransformMultiply(State.Scissor.XForm, State.XForm);
  State.Scissor.Extent[0] := W * 0.5;
  State.Scissor.Extent[1] := H * 0.5;
end;

procedure nvg__isectRects(Dst: PSingle; AX, AY, AW, AH, BX, BY, BW, BH: Single);
var
  MinX, MinY, MaxX, MaxY: Single;
begin
  MinX := nvg__maxf(AX, BX);
  MinY := nvg__maxf(AY, BY);
  MaxX := nvg__minf(AX + AW, BX + BW);
  MaxY := nvg__minf(AY + AH, BY + BH);
  Dst[0] := MinX;
  Dst[1] := MinY;
  Dst[2] := nvg__maxf(0.0, MaxX - MinX);
  Dst[3] := nvg__maxf(0.0, MaxY - MinY);
end;

procedure nvgIntersectScissor(Ctx: PNVGcontext; X, Y, W, H: Single);
var
  State: PNVGstate;
  PXForm, InvXForm: TNVGxform;
  Rect: array[0..3] of Single;
  EX, EY, TEX, TEY: Single;
begin
  State := nvg__getState(Ctx);
  { If no previous scissor has been set, set the scissor as current scissor }
  if State.Scissor.Extent[0] < 0 then
  begin
    nvgScissor(Ctx, X, Y, W, H);
    Exit;
  end;
  { Transform the current scissor rect into current transform space. If there
    is difference in rotation, this will be approximation }
  PXForm := State.Scissor.XForm;
  EX := State.Scissor.Extent[0];
  EY := State.Scissor.Extent[1];
  nvgTransformInverse(InvXForm, State.XForm);
  nvgTransformMultiply(PXForm, InvXForm);
  TEX := EX * nvg__absf(PXForm[0]) + EY * nvg__absf(PXForm[2]);
  TEY := EX * nvg__absf(PXForm[1]) + EY * nvg__absf(PXForm[3]);
  { Intersect rects }
  nvg__isectRects(@Rect[0], PXForm[4] - TEX, PXForm[5] - TEY, TEX * 2, TEY * 2, X, Y, W, H);
  nvgScissor(Ctx, Rect[0], Rect[1], Rect[2], Rect[3]);
end;

procedure nvgResetScissor(Ctx: PNVGcontext);
var
  State: PNVGstate;
begin
  State := nvg__getState(Ctx);
  FillChar(State.Scissor.XForm, SizeOf(State.Scissor.XForm), 0);
  State.Scissor.Extent[0] := -1.0;
  State.Scissor.Extent[1] := -1.0;
end;

{ Global composite operation }

procedure nvgGlobalCompositeOperation(Ctx: PNVGcontext; Op: Integer);
begin
  nvg__getState(Ctx).CompositeOperation := nvg__compositeOperationState(Op);
end;

procedure nvgGlobalCompositeBlendFunc(Ctx: PNVGcontext; SFactor, DFactor: Integer);
begin
  nvgGlobalCompositeBlendFuncSeparate(Ctx, SFactor, DFactor, SFactor, DFactor);
end;

procedure nvgGlobalCompositeBlendFuncSeparate(Ctx: PNVGcontext; SrcRGB, DstRGB, SrcAlpha, DstAlpha: Integer);
var
  Op: TNVGcompositeOperationState;
begin
  Op.SrcRGB := SrcRGB;
  Op.DstRGB := DstRGB;
  Op.SrcAlpha := SrcAlpha;
  Op.DstAlpha := DstAlpha;
  nvg__getState(Ctx).CompositeOperation := Op;
end;

function nvg__ptEquals(X1, Y1, X2, Y2, Tol: Single): Boolean; inline;
var
  DX, DY: Single;
begin
  DX := X2 - X1;
  DY := Y2 - Y1;
  Result := DX * DX + DY * DY < Tol * Tol;
end;

function nvg__distPtSeg(X, Y, PX, PY, QX, QY: Single): Single;
var
  PQX, PQY, DX, DY, D, T: Single;
begin
  PQX := QX - PX;
  PQY := QY - PY;
  DX := X - PX;
  DY := Y - PY;
  D := PQX * PQX + PQY * PQY;
  T := PQX * DX + PQY * DY;
  if D > 0 then
    T := T / D;
  if T < 0 then
    T := 0
  else if T > 1 then
    T := 1;
  DX := PX + T * PQX - X;
  DY := PY + T * PQY - Y;
  Result := DX * DX + DY * DY;
end;

procedure nvg__appendCommands(Ctx: PNVGcontext; Vals: PSingle; NVals: Integer);
var
  State: PNVGstate;
  I, CCommands, Cmd: Integer;
begin
  State := nvg__getState(Ctx);
  if Ctx.NCommands + NVals > Ctx.CCommands then
  begin
    CCommands := Ctx.NCommands + NVals + Ctx.CCommands div 2;
    ReallocMem(Ctx.Commands, SizeOf(Single) * CCommands);
    Ctx.CCommands := CCommands;
  end;
  if (Trunc(Vals[0]) <> NVG_CLOSE) and (Trunc(Vals[0]) <> NVG_WINDING) then
  begin
    Ctx.CommandX := Vals[NVals - 2];
    Ctx.CommandY := Vals[NVals - 1];
  end;
  { Transform commands }
  I := 0;
  while I < NVals do
  begin
    Cmd := Trunc(Vals[I]);
    case Cmd of
      NVG_MOVETO, NVG_LINETO:
        begin
          nvgTransformPoint(Vals[I + 1], Vals[I + 2], State.XForm, Vals[I + 1], Vals[I + 2]);
          Inc(I, 3);
        end;
      NVG_BEZIERTO:
        begin
          nvgTransformPoint(Vals[I + 1], Vals[I + 2], State.XForm, Vals[I + 1], Vals[I + 2]);
          nvgTransformPoint(Vals[I + 3], Vals[I + 4], State.XForm, Vals[I + 3], Vals[I + 4]);
          nvgTransformPoint(Vals[I + 5], Vals[I + 6], State.XForm, Vals[I + 5], Vals[I + 6]);
          Inc(I, 7);
        end;
      NVG_CLOSE:
        Inc(I);
      NVG_WINDING:
        Inc(I, 2);
    else
      Inc(I);
    end;
  end;
  Move(Vals^, Ctx.Commands[Ctx.NCommands], NVals * SizeOf(Single));
  Inc(Ctx.NCommands, NVals);
end;

procedure nvg__clearPathCache(Ctx: PNVGcontext);
begin
  Ctx.Cache.NPoints := 0;
  Ctx.Cache.NPaths := 0;
end;

function nvg__lastPath(Ctx: PNVGcontext): PNVGpath;
begin
  if Ctx.Cache.NPaths > 0 then
    Result := @Ctx.Cache.Paths[Ctx.Cache.NPaths - 1]
  else
    Result := nil;
end;

procedure nvg__addPath(Ctx: PNVGcontext);
var
  Path: PNVGpath;
  CPaths: Integer;
begin
  if Ctx.Cache.NPaths + 1 > Ctx.Cache.CPaths then
  begin
    CPaths := Ctx.Cache.NPaths + 1 + Ctx.Cache.CPaths div 2;
    ReallocMem(Ctx.Cache.Paths, SizeOf(TNVGpath) * CPaths);
    Ctx.Cache.CPaths := CPaths;
  end;
  Path := @Ctx.Cache.Paths[Ctx.Cache.NPaths];
  FillChar(Path^, SizeOf(Path^), 0);
  Path.First := Ctx.Cache.NPoints;
  Path.Winding := NVG_CCW;
  Inc(Ctx.Cache.NPaths);
end;

function nvg__lastPoint(Ctx: PNVGcontext): PNVGpoint;
begin
  if Ctx.Cache.NPoints > 0 then
    Result := @Ctx.Cache.Points[Ctx.Cache.NPoints - 1]
  else
    Result := nil;
end;

procedure nvg__addPoint(Ctx: PNVGcontext; X, Y: Single; Flags: Integer);
var
  Path: PNVGpath;
  Pt: PNVGpoint;
  CPoints: Integer;
begin
  Path := nvg__lastPath(Ctx);
  if Path = nil then
    Exit;
  if (Path.Count > 0) and (Ctx.Cache.NPoints > 0) then
  begin
    Pt := nvg__lastPoint(Ctx);
    if nvg__ptEquals(Pt.X, Pt.Y, X, Y, Ctx.DistTol) then
    begin
      Pt.Flags := Pt.Flags or Flags;
      Exit;
    end;
  end;
  if Ctx.Cache.NPoints + 1 > Ctx.Cache.CPoints then
  begin
    CPoints := Ctx.Cache.NPoints + 1 + Ctx.Cache.CPoints div 2;
    ReallocMem(Ctx.Cache.Points, SizeOf(TNVGpoint) * CPoints);
    Ctx.Cache.CPoints := CPoints;
  end;
  Pt := @Ctx.Cache.Points[Ctx.Cache.NPoints];
  FillChar(Pt^, SizeOf(Pt^), 0);
  Pt.X := X;
  Pt.Y := Y;
  Pt.Flags := Byte(Flags);
  Inc(Ctx.Cache.NPoints);
  Inc(Path.Count);
end;

procedure nvg__closePath(Ctx: PNVGcontext);
var
  Path: PNVGpath;
begin
  Path := nvg__lastPath(Ctx);
  if Path = nil then
    Exit;
  Path.Closed := 1;
end;

procedure nvg__pathWinding(Ctx: PNVGcontext; Winding: Integer);
var
  Path: PNVGpath;
begin
  Path := nvg__lastPath(Ctx);
  if Path = nil then
    Exit;
  Path.Winding := Winding;
end;

function nvg__getAverageScale(const T: TNVGxform): Single;
var
  SX, SY: Single;
begin
  SX := Sqrt(T[0] * T[0] + T[2] * T[2]);
  SY := Sqrt(T[1] * T[1] + T[3] * T[3]);
  Result := (SX + SY) * 0.5;
end;

function nvg__allocTempVerts(Ctx: PNVGcontext; NVerts: Integer): PNVGvertex;
var
  CVerts: Integer;
begin
  if NVerts > Ctx.Cache.CVerts then
  begin
    { Round up to prevent allocations when things change just slightly }
    CVerts := (NVerts + $FF) and (not $FF);
    ReallocMem(Ctx.Cache.Verts, SizeOf(TNVGvertex) * CVerts);
    Ctx.Cache.CVerts := CVerts;
  end;
  Result := Ctx.Cache.Verts;
end;

function nvg__triarea2(AX, AY, BX, BY, CX, CY: Single): Single; inline;
var
  ABX, ABY, ACX, ACY: Single;
begin
  ABX := BX - AX;
  ABY := BY - AY;
  ACX := CX - AX;
  ACY := CY - AY;
  Result := ACX * ABY - ABX * ACY;
end;

function nvg__polyArea(Pts: PNVGpoint; NPts: Integer): Single;
var
  I: Integer;
  A, B, C: PNVGpoint;
  Area: Single;
begin
  Area := 0;
  for I := 2 to NPts - 1 do
  begin
    A := @Pts[0];
    B := @Pts[I - 1];
    C := @Pts[I];
    Area := Area + nvg__triarea2(A.X, A.Y, B.X, B.Y, C.X, C.Y);
  end;
  Result := Area * 0.5;
end;

procedure nvg__polyReverse(Pts: PNVGpoint; NPts: Integer);
var
  Tmp: TNVGpoint;
  I, J: Integer;
begin
  I := 0;
  J := NPts - 1;
  while I < J do
  begin
    Tmp := Pts[I];
    Pts[I] := Pts[J];
    Pts[J] := Tmp;
    Inc(I);
    Dec(J);
  end;
end;

procedure nvg__vset(Vtx: PNVGvertex; X, Y, U, V: Single); inline;
begin
  Vtx.X := X;
  Vtx.Y := Y;
  Vtx.U := U;
  Vtx.V := V;
end;

procedure nvg__tesselateBezier(Ctx: PNVGcontext; X1, Y1, X2, Y2, X3, Y3, X4, Y4: Single; Level, VType: Integer);
var
  X12, Y12, X23, Y23, X34, Y34, X123, Y123, X234, Y234, X1234, Y1234: Single;
  DX, DY, D2, D3: Single;
begin
  if Level > 10 then
    Exit;
  X12 := (X1 + X2) * 0.5;
  Y12 := (Y1 + Y2) * 0.5;
  X23 := (X2 + X3) * 0.5;
  Y23 := (Y2 + Y3) * 0.5;
  X34 := (X3 + X4) * 0.5;
  Y34 := (Y3 + Y4) * 0.5;
  X123 := (X12 + X23) * 0.5;
  Y123 := (Y12 + Y23) * 0.5;
  DX := X4 - X1;
  DY := Y4 - Y1;
  D2 := nvg__absf((X2 - X4) * DY - (Y2 - Y4) * DX);
  D3 := nvg__absf((X3 - X4) * DY - (Y3 - Y4) * DX);
  if (D2 + D3) * (D2 + D3) < Ctx.TessTol * (DX * DX + DY * DY) then
  begin
    nvg__addPoint(Ctx, X4, Y4, VType);
    Exit;
  end;
  X234 := (X23 + X34) * 0.5;
  Y234 := (Y23 + Y34) * 0.5;
  X1234 := (X123 + X234) * 0.5;
  Y1234 := (Y123 + Y234) * 0.5;
  nvg__tesselateBezier(Ctx, X1, Y1, X12, Y12, X123, Y123, X1234, Y1234, Level + 1, 0);
  nvg__tesselateBezier(Ctx, X1234, Y1234, X234, Y234, X34, Y34, X4, Y4, Level + 1, VType);
end;

function nvg__pointInPoly(Pts: PNVGpoint; NPts: Integer; X, Y: Single): Boolean;
var
  I, J: Integer;
begin
  Result := False;
  J := NPts - 1;
  for I := 0 to NPts - 1 do
  begin
    if ((Pts[I].Y > Y) <> (Pts[J].Y > Y)) and
      (X < (Pts[J].X - Pts[I].X) * (Y - Pts[I].Y) / (Pts[J].Y - Pts[I].Y) + Pts[I].X) then
      Result := not Result;
    J := I;
  end;
end;

{ nvg__orientPaths sets the direction of each flattened sub-path. The fill
  draws solid sub-paths (positive area) and holes (negative area) into the
  stencil, and the anti-aliased fringe is drawn on the outside of solid
  sub-paths and on the inside of holes. }

procedure nvg__orientPaths(Ctx: PNVGcontext; Rule: Integer);
var
  Cache: PNVGpathCache;
  Path: PNVGpath;
  Pts, Other: PNVGpoint;
  Areas: array of Single;
  Largest, Depth, I, J: Integer;
  Biggest: Single;
begin
  Cache := Ctx.Cache;
  if Rule = NVG_FILL_WINDING then
  begin
    { Each sub-path takes the winding given by nvgPathWinding }
    for J := 0 to Cache.NPaths - 1 do
    begin
      Path := @Cache.Paths[J];
      if Path.Count < 3 then
        Continue;
      Pts := @Cache.Points[Path.First];
      Biggest := nvg__polyArea(Pts, Path.Count);
      if (Path.Winding = NVG_CCW) and (Biggest < 0.0) then
        nvg__polyReverse(Pts, Path.Count);
      if (Path.Winding = NVG_CW) and (Biggest > 0.0) then
        nvg__polyReverse(Pts, Path.Count);
    end;
    Exit;
  end;
  SetLength(Areas, Cache.NPaths);
  Largest := -1;
  Biggest := 0;
  for J := 0 to Cache.NPaths - 1 do
  begin
    Path := @Cache.Paths[J];
    Areas[J] := 0;
    if Path.Count < 3 then
      Continue;
    Areas[J] := nvg__polyArea(@Cache.Points[Path.First], Path.Count);
    if nvg__absf(Areas[J]) > Biggest then
    begin
      Biggest := nvg__absf(Areas[J]);
      Largest := J;
    end;
  end;
  if Largest < 0 then
    Exit;
  if Rule = NVG_FILL_NONZERO then
  begin
    { The nonzero fill only depends on the directions of the sub-paths
      relative to each other, so reversing all of them gives the same result.
      They are reversed when needed to make the largest sub-path solid. }
    if Areas[Largest] < 0 then
      for J := 0 to Cache.NPaths - 1 do
      begin
        Path := @Cache.Paths[J];
        if Path.Count > 2 then
          nvg__polyReverse(@Cache.Points[Path.First], Path.Count);
      end;
  end
  else
  begin
    { The even-odd fill does not depend on direction at all. A sub-path
      inside an even number of other sub-paths is made solid and the rest
      are made holes, so the fringes fall outside the filled area. }
    for J := 0 to Cache.NPaths - 1 do
    begin
      Path := @Cache.Paths[J];
      if Path.Count < 3 then
        Continue;
      Pts := @Cache.Points[Path.First];
      Depth := 0;
      for I := 0 to Cache.NPaths - 1 do
      begin
        if (I = J) or (Cache.Paths[I].Count < 3) then
          Continue;
        Other := @Cache.Points[Cache.Paths[I].First];
        if nvg__pointInPoly(Other, Cache.Paths[I].Count, Pts[0].X, Pts[0].Y) then
          Inc(Depth);
      end;
      if Odd(Depth) = (Areas[J] > 0) then
        nvg__polyReverse(Pts, Path.Count);
    end;
  end;
end;

procedure nvg__flattenPaths(Ctx: PNVGcontext);
var
  Cache: PNVGpathCache;
  Last, P0, P1, Pts: PNVGpoint;
  Path: PNVGpath;
  I, J, Cmd: Integer;
  CP1, CP2, P: PSingle;
begin
  Cache := Ctx.Cache;
  if Cache.NPaths > 0 then
    Exit;
  { Flatten }
  I := 0;
  while I < Ctx.NCommands do
  begin
    Cmd := Trunc(Ctx.Commands[I]);
    case Cmd of
      NVG_MOVETO:
        begin
          nvg__addPath(Ctx);
          P := @Ctx.Commands[I + 1];
          nvg__addPoint(Ctx, P[0], P[1], NVG_PT_CORNER);
          Inc(I, 3);
        end;
      NVG_LINETO:
        begin
          P := @Ctx.Commands[I + 1];
          nvg__addPoint(Ctx, P[0], P[1], NVG_PT_CORNER);
          Inc(I, 3);
        end;
      NVG_BEZIERTO:
        begin
          Last := nvg__lastPoint(Ctx);
          if Last <> nil then
          begin
            CP1 := @Ctx.Commands[I + 1];
            CP2 := @Ctx.Commands[I + 3];
            P := @Ctx.Commands[I + 5];
            nvg__tesselateBezier(Ctx, Last.X, Last.Y, CP1[0], CP1[1], CP2[0], CP2[1], P[0], P[1], 0, NVG_PT_CORNER);
          end;
          Inc(I, 7);
        end;
      NVG_CLOSE:
        begin
          nvg__closePath(Ctx);
          Inc(I);
        end;
      NVG_WINDING:
        begin
          nvg__pathWinding(Ctx, Trunc(Ctx.Commands[I + 1]));
          Inc(I, 2);
        end;
    else
      Inc(I);
    end;
  end;
  Cache.Bounds[0] := 1e6;
  Cache.Bounds[1] := 1e6;
  Cache.Bounds[2] := -1e6;
  Cache.Bounds[3] := -1e6;
  { If the first and last points are the same, remove the last, mark as
    closed path }
  for J := 0 to Cache.NPaths - 1 do
  begin
    Path := @Cache.Paths[J];
    Pts := @Cache.Points[Path.First];
    P0 := @Pts[Path.Count - 1];
    P1 := @Pts[0];
    if nvg__ptEquals(P0.X, P0.Y, P1.X, P1.Y, Ctx.DistTol) then
    begin
      Dec(Path.Count);
      Path.Closed := 1;
    end;
  end;
  { Enforce winding }
  nvg__orientPaths(Ctx, nvg__getState(Ctx).FillRule);
  { Calculate the direction and length of line segments }
  for J := 0 to Cache.NPaths - 1 do
  begin
    Path := @Cache.Paths[J];
    Pts := @Cache.Points[Path.First];
    P0 := @Pts[Path.Count - 1];
    P1 := @Pts[0];
    for I := 0 to Path.Count - 1 do
    begin
      { Calculate segment direction and length }
      P0.DX := P1.X - P0.X;
      P0.DY := P1.Y - P0.Y;
      P0.Len := nvg__normalize(P0.DX, P0.DY);
      { Update bounds }
      Cache.Bounds[0] := nvg__minf(Cache.Bounds[0], P0.X);
      Cache.Bounds[1] := nvg__minf(Cache.Bounds[1], P0.Y);
      Cache.Bounds[2] := nvg__maxf(Cache.Bounds[2], P0.X);
      Cache.Bounds[3] := nvg__maxf(Cache.Bounds[3], P0.Y);
      { Advance }
      P0 := P1;
      Inc(P1);
    end;
  end;
end;

function nvg__curveDivs(R, Arc, Tol: Single): Integer;
var
  DA: Single;
begin
  DA := ArcCos(R / (R + Tol)) * 2.0;
  Result := nvg__maxi(2, Ceil(Arc / DA));
end;

procedure nvg__chooseBevel(Bevel: Boolean; P0, P1: PNVGpoint; W: Single; out X0, Y0, X1, Y1: Single);
begin
  if Bevel then
  begin
    X0 := P1.X + P0.DY * W;
    Y0 := P1.Y - P0.DX * W;
    X1 := P1.X + P1.DY * W;
    Y1 := P1.Y - P1.DX * W;
  end
  else
  begin
    X0 := P1.X + P1.DMX * W;
    Y0 := P1.Y + P1.DMY * W;
    X1 := P1.X + P1.DMX * W;
    Y1 := P1.Y + P1.DMY * W;
  end;
end;

function nvg__roundJoin(Dst: PNVGvertex; P0, P1: PNVGpoint; LW, RW, LU, RU: Single; NCap: Integer;
  Fringe: Single): PNVGvertex;
var
  I, N: Integer;
  DLX0, DLY0, DLX1, DLY1, LX0, LY0, LX1, LY1, RX0, RY0, RX1, RY1, A0, A1, U, A, RX, RY, LX, LY: Single;
begin
  DLX0 := P0.DY;
  DLY0 := -P0.DX;
  DLX1 := P1.DY;
  DLY1 := -P1.DX;
  if P1.Flags and NVG_PT_LEFT <> 0 then
  begin
    nvg__chooseBevel(P1.Flags and NVG_PR_INNERBEVEL <> 0, P0, P1, LW, LX0, LY0, LX1, LY1);
    A0 := ArcTan2(-DLY0, -DLX0);
    A1 := ArcTan2(-DLY1, -DLX1);
    if A1 > A0 then
      A1 := A1 - NVG_PI * 2;
    nvg__vset(Dst, LX0, LY0, LU, 1); Inc(Dst);
    nvg__vset(Dst, P1.X - DLX0 * RW, P1.Y - DLY0 * RW, RU, 1); Inc(Dst);
    N := nvg__clampi(Ceil(((A0 - A1) / NVG_PI) * NCap), 2, NCap);
    for I := 0 to N - 1 do
    begin
      U := I / (N - 1);
      A := A0 + U * (A1 - A0);
      RX := P1.X + Cos(A) * RW;
      RY := P1.Y + Sin(A) * RW;
      nvg__vset(Dst, P1.X, P1.Y, 0.5, 1); Inc(Dst);
      nvg__vset(Dst, RX, RY, RU, 1); Inc(Dst);
    end;
    nvg__vset(Dst, LX1, LY1, LU, 1); Inc(Dst);
    nvg__vset(Dst, P1.X - DLX1 * RW, P1.Y - DLY1 * RW, RU, 1); Inc(Dst);
  end
  else
  begin
    nvg__chooseBevel(P1.Flags and NVG_PR_INNERBEVEL <> 0, P0, P1, -RW, RX0, RY0, RX1, RY1);
    A0 := ArcTan2(DLY0, DLX0);
    A1 := ArcTan2(DLY1, DLX1);
    if A1 < A0 then
      A1 := A1 + NVG_PI * 2;
    nvg__vset(Dst, P1.X + DLX0 * RW, P1.Y + DLY0 * RW, LU, 1); Inc(Dst);
    nvg__vset(Dst, RX0, RY0, RU, 1); Inc(Dst);
    N := nvg__clampi(Ceil(((A1 - A0) / NVG_PI) * NCap), 2, NCap);
    for I := 0 to N - 1 do
    begin
      U := I / (N - 1);
      A := A0 + U * (A1 - A0);
      LX := P1.X + Cos(A) * LW;
      LY := P1.Y + Sin(A) * LW;
      nvg__vset(Dst, LX, LY, LU, 1); Inc(Dst);
      nvg__vset(Dst, P1.X, P1.Y, 0.5, 1); Inc(Dst);
    end;
    nvg__vset(Dst, P1.X + DLX1 * RW, P1.Y + DLY1 * RW, LU, 1); Inc(Dst);
    nvg__vset(Dst, RX1, RY1, RU, 1); Inc(Dst);
  end;
  Result := Dst;
end;

function nvg__bevelJoin(Dst: PNVGvertex; P0, P1: PNVGpoint; LW, RW, LU, RU, Fringe: Single): PNVGvertex;
var
  RX0, RY0, RX1, RY1, LX0, LY0, LX1, LY1, DLX0, DLY0, DLX1, DLY1: Single;
begin
  DLX0 := P0.DY;
  DLY0 := -P0.DX;
  DLX1 := P1.DY;
  DLY1 := -P1.DX;
  if P1.Flags and NVG_PT_LEFT <> 0 then
  begin
    nvg__chooseBevel(P1.Flags and NVG_PR_INNERBEVEL <> 0, P0, P1, LW, LX0, LY0, LX1, LY1);
    nvg__vset(Dst, LX0, LY0, LU, 1); Inc(Dst);
    nvg__vset(Dst, P1.X - DLX0 * RW, P1.Y - DLY0 * RW, RU, 1); Inc(Dst);
    if P1.Flags and NVG_PT_BEVEL <> 0 then
    begin
      nvg__vset(Dst, LX0, LY0, LU, 1); Inc(Dst);
      nvg__vset(Dst, P1.X - DLX0 * RW, P1.Y - DLY0 * RW, RU, 1); Inc(Dst);
      nvg__vset(Dst, LX1, LY1, LU, 1); Inc(Dst);
      nvg__vset(Dst, P1.X - DLX1 * RW, P1.Y - DLY1 * RW, RU, 1); Inc(Dst);
    end
    else
    begin
      RX0 := P1.X - P1.DMX * RW;
      RY0 := P1.Y - P1.DMY * RW;
      nvg__vset(Dst, P1.X, P1.Y, 0.5, 1); Inc(Dst);
      nvg__vset(Dst, P1.X - DLX0 * RW, P1.Y - DLY0 * RW, RU, 1); Inc(Dst);
      nvg__vset(Dst, RX0, RY0, RU, 1); Inc(Dst);
      nvg__vset(Dst, RX0, RY0, RU, 1); Inc(Dst);
      nvg__vset(Dst, P1.X, P1.Y, 0.5, 1); Inc(Dst);
      nvg__vset(Dst, P1.X - DLX1 * RW, P1.Y - DLY1 * RW, RU, 1); Inc(Dst);
    end;
    nvg__vset(Dst, LX1, LY1, LU, 1); Inc(Dst);
    nvg__vset(Dst, P1.X - DLX1 * RW, P1.Y - DLY1 * RW, RU, 1); Inc(Dst);
  end
  else
  begin
    nvg__chooseBevel(P1.Flags and NVG_PR_INNERBEVEL <> 0, P0, P1, -RW, RX0, RY0, RX1, RY1);
    nvg__vset(Dst, P1.X + DLX0 * LW, P1.Y + DLY0 * LW, LU, 1); Inc(Dst);
    nvg__vset(Dst, RX0, RY0, RU, 1); Inc(Dst);
    if P1.Flags and NVG_PT_BEVEL <> 0 then
    begin
      nvg__vset(Dst, P1.X + DLX0 * LW, P1.Y + DLY0 * LW, LU, 1); Inc(Dst);
      nvg__vset(Dst, RX0, RY0, RU, 1); Inc(Dst);
      nvg__vset(Dst, P1.X + DLX1 * LW, P1.Y + DLY1 * LW, LU, 1); Inc(Dst);
      nvg__vset(Dst, RX1, RY1, RU, 1); Inc(Dst);
    end
    else
    begin
      LX0 := P1.X + P1.DMX * LW;
      LY0 := P1.Y + P1.DMY * LW;
      nvg__vset(Dst, P1.X + DLX0 * LW, P1.Y + DLY0 * LW, LU, 1); Inc(Dst);
      nvg__vset(Dst, P1.X, P1.Y, 0.5, 1); Inc(Dst);
      nvg__vset(Dst, LX0, LY0, LU, 1); Inc(Dst);
      nvg__vset(Dst, LX0, LY0, LU, 1); Inc(Dst);
      nvg__vset(Dst, P1.X + DLX1 * LW, P1.Y + DLY1 * LW, LU, 1); Inc(Dst);
      nvg__vset(Dst, P1.X, P1.Y, 0.5, 1); Inc(Dst);
    end;
    nvg__vset(Dst, P1.X + DLX1 * LW, P1.Y + DLY1 * LW, LU, 1); Inc(Dst);
    nvg__vset(Dst, RX1, RY1, RU, 1); Inc(Dst);
  end;
  Result := Dst;
end;

function nvg__buttCapStart(Dst: PNVGvertex; P: PNVGpoint; DX, DY, W, D, AA, U0, U1: Single): PNVGvertex;
var
  PX, PY, DLX, DLY: Single;
begin
  PX := P.X - DX * D;
  PY := P.Y - DY * D;
  DLX := DY;
  DLY := -DX;
  nvg__vset(Dst, PX + DLX * W - DX * AA, PY + DLY * W - DY * AA, U0, 0); Inc(Dst);
  nvg__vset(Dst, PX - DLX * W - DX * AA, PY - DLY * W - DY * AA, U1, 0); Inc(Dst);
  nvg__vset(Dst, PX + DLX * W, PY + DLY * W, U0, 1); Inc(Dst);
  nvg__vset(Dst, PX - DLX * W, PY - DLY * W, U1, 1); Inc(Dst);
  Result := Dst;
end;

function nvg__buttCapEnd(Dst: PNVGvertex; P: PNVGpoint; DX, DY, W, D, AA, U0, U1: Single): PNVGvertex;
var
  PX, PY, DLX, DLY: Single;
begin
  PX := P.X + DX * D;
  PY := P.Y + DY * D;
  DLX := DY;
  DLY := -DX;
  nvg__vset(Dst, PX + DLX * W, PY + DLY * W, U0, 1); Inc(Dst);
  nvg__vset(Dst, PX - DLX * W, PY - DLY * W, U1, 1); Inc(Dst);
  nvg__vset(Dst, PX + DLX * W + DX * AA, PY + DLY * W + DY * AA, U0, 0); Inc(Dst);
  nvg__vset(Dst, PX - DLX * W + DX * AA, PY - DLY * W + DY * AA, U1, 0); Inc(Dst);
  Result := Dst;
end;

function nvg__roundCapStart(Dst: PNVGvertex; P: PNVGpoint; DX, DY, W: Single; NCap: Integer;
  AA, U0, U1: Single): PNVGvertex;
var
  I: Integer;
  PX, PY, DLX, DLY, A, AX, AY: Single;
begin
  PX := P.X;
  PY := P.Y;
  DLX := DY;
  DLY := -DX;
  for I := 0 to NCap - 1 do
  begin
    A := I / (NCap - 1) * NVG_PI;
    AX := Cos(A) * W;
    AY := Sin(A) * W;
    nvg__vset(Dst, PX - DLX * AX - DX * AY, PY - DLY * AX - DY * AY, U0, 1); Inc(Dst);
    nvg__vset(Dst, PX, PY, 0.5, 1); Inc(Dst);
  end;
  nvg__vset(Dst, PX + DLX * W, PY + DLY * W, U0, 1); Inc(Dst);
  nvg__vset(Dst, PX - DLX * W, PY - DLY * W, U1, 1); Inc(Dst);
  Result := Dst;
end;

function nvg__roundCapEnd(Dst: PNVGvertex; P: PNVGpoint; DX, DY, W: Single; NCap: Integer;
  AA, U0, U1: Single): PNVGvertex;
var
  I: Integer;
  PX, PY, DLX, DLY, A, AX, AY: Single;
begin
  PX := P.X;
  PY := P.Y;
  DLX := DY;
  DLY := -DX;
  nvg__vset(Dst, PX + DLX * W, PY + DLY * W, U0, 1); Inc(Dst);
  nvg__vset(Dst, PX - DLX * W, PY - DLY * W, U1, 1); Inc(Dst);
  for I := 0 to NCap - 1 do
  begin
    A := I / (NCap - 1) * NVG_PI;
    AX := Cos(A) * W;
    AY := Sin(A) * W;
    nvg__vset(Dst, PX, PY, 0.5, 1); Inc(Dst);
    nvg__vset(Dst, PX - DLX * AX + DX * AY, PY - DLY * AX + DY * AY, U0, 1); Inc(Dst);
  end;
  Result := Dst;
end;

procedure nvg__calculateJoins(Ctx: PNVGcontext; W: Single; LineJoin: Integer; MiterLimit: Single);
var
  Cache: PNVGpathCache;
  I, J, NLeft: Integer;
  IW, DLX0, DLY0, DLX1, DLY1, DMR2, Cross, Limit, Scale: Single;
  Path: PNVGpath;
  Pts, P0, P1: PNVGpoint;
begin
  Cache := Ctx.Cache;
  IW := 0.0;
  if W > 0.0 then
    IW := 1.0 / W;
  { Calculate which joins needs extra vertices to append, and gather vertex count }
  for I := 0 to Cache.NPaths - 1 do
  begin
    Path := @Cache.Paths[I];
    Pts := @Cache.Points[Path.First];
    P0 := @Pts[Path.Count - 1];
    P1 := @Pts[0];
    NLeft := 0;
    Path.NBevel := 0;
    for J := 0 to Path.Count - 1 do
    begin
      DLX0 := P0.DY;
      DLY0 := -P0.DX;
      DLX1 := P1.DY;
      DLY1 := -P1.DX;
      { Calculate extrusions }
      P1.DMX := (DLX0 + DLX1) * 0.5;
      P1.DMY := (DLY0 + DLY1) * 0.5;
      DMR2 := P1.DMX * P1.DMX + P1.DMY * P1.DMY;
      if DMR2 > 0.000001 then
      begin
        Scale := 1.0 / DMR2;
        if Scale > 600.0 then
          Scale := 600.0;
        P1.DMX := P1.DMX * Scale;
        P1.DMY := P1.DMY * Scale;
      end;
      { Clear flags, but keep the corner }
      if P1.Flags and NVG_PT_CORNER <> 0 then
        P1.Flags := NVG_PT_CORNER
      else
        P1.Flags := 0;
      { Keep track of left turns }
      Cross := P1.DX * P0.DY - P0.DX * P1.DY;
      if Cross > 0.0 then
      begin
        Inc(NLeft);
        P1.Flags := P1.Flags or NVG_PT_LEFT;
      end;
      { Calculate if we should use bevel or miter for inner join }
      Limit := nvg__maxf(1.01, nvg__minf(P0.Len, P1.Len) * IW);
      if (DMR2 * Limit * Limit) < 1.0 then
        P1.Flags := P1.Flags or NVG_PR_INNERBEVEL;
      { Check to see if the corner needs to be beveled }
      if P1.Flags and NVG_PT_CORNER <> 0 then
        if ((DMR2 * MiterLimit * MiterLimit) < 1.0) or (LineJoin = NVG_BEVEL) or (LineJoin = NVG_ROUND) then
          P1.Flags := P1.Flags or NVG_PT_BEVEL;
      if (P1.Flags and (NVG_PT_BEVEL or NVG_PR_INNERBEVEL)) <> 0 then
        Inc(Path.NBevel);
      P0 := P1;
      Inc(P1);
    end;
    if NLeft = Path.Count then
      Path.Convex := 1
    else
      Path.Convex := 0;
  end;
end;

function nvg__expandStroke(Ctx: PNVGcontext; W, Fringe: Single; LineCap, LineJoin: Integer; MiterLimit: Single): Boolean;
var
  Cache: PNVGpathCache;
  Verts, Dst: PNVGvertex;
  CVerts, I, J, S, E, NCap: Integer;
  AA, U0, U1, DX, DY: Single;
  Path: PNVGpath;
  Pts, P0, P1: PNVGpoint;
  Loop: Boolean;
begin
  Cache := Ctx.Cache;
  AA := Fringe;
  U0 := 0.0;
  U1 := 1.0;
  { Calculate divisions per half circle }
  NCap := nvg__curveDivs(W, NVG_PI, Ctx.TessTol);
  W := W + AA * 0.5;
  { Disable the gradient used for antialiasing when antialiasing is not used }
  if AA = 0.0 then
  begin
    U0 := 0.5;
    U1 := 0.5;
  end;
  nvg__calculateJoins(Ctx, W, LineJoin, MiterLimit);
  { Calculate max vertex usage }
  CVerts := 0;
  for I := 0 to Cache.NPaths - 1 do
  begin
    Path := @Cache.Paths[I];
    Loop := Path.Closed <> 0;
    if LineJoin = NVG_ROUND then
      Inc(CVerts, (Path.Count + Path.NBevel * (NCap + 2) + 1) * 2) { plus one for loop }
    else
      Inc(CVerts, (Path.Count + Path.NBevel * 5 + 1) * 2); { plus one for loop }
    if not Loop then
    begin
      { Space for caps }
      if LineCap = NVG_ROUND then
        Inc(CVerts, (NCap * 2 + 2) * 2)
      else
        Inc(CVerts, (3 + 3) * 2);
    end;
  end;
  Verts := nvg__allocTempVerts(Ctx, CVerts);
  if Verts = nil then
    Exit(False);
  for I := 0 to Cache.NPaths - 1 do
  begin
    Path := @Cache.Paths[I];
    Pts := @Cache.Points[Path.First];
    Path.Fill := nil;
    Path.NFill := 0;
    { Calculate fringe or stroke }
    Loop := Path.Closed <> 0;
    Dst := Verts;
    Path.Stroke := Dst;
    if Loop then
    begin
      { Looping }
      P0 := @Pts[Path.Count - 1];
      P1 := @Pts[0];
      S := 0;
      E := Path.Count;
    end
    else
    begin
      { Add cap }
      P0 := @Pts[0];
      P1 := @Pts[1];
      S := 1;
      E := Path.Count - 1;
    end;
    if not Loop then
    begin
      { Add cap }
      DX := P1.X - P0.X;
      DY := P1.Y - P0.Y;
      nvg__normalize(DX, DY);
      if LineCap = NVG_BUTT then
        Dst := nvg__buttCapStart(Dst, P0, DX, DY, W, -AA * 0.5, AA, U0, U1)
      else if (LineCap = NVG_BUTT) or (LineCap = NVG_SQUARE) then
        Dst := nvg__buttCapStart(Dst, P0, DX, DY, W, W - AA, AA, U0, U1)
      else if LineCap = NVG_ROUND then
        Dst := nvg__roundCapStart(Dst, P0, DX, DY, W, NCap, AA, U0, U1);
    end;
    for J := S to E - 1 do
    begin
      if (P1.Flags and (NVG_PT_BEVEL or NVG_PR_INNERBEVEL)) <> 0 then
      begin
        if LineJoin = NVG_ROUND then
          Dst := nvg__roundJoin(Dst, P0, P1, W, W, U0, U1, NCap, AA)
        else
          Dst := nvg__bevelJoin(Dst, P0, P1, W, W, U0, U1, AA);
      end
      else
      begin
        nvg__vset(Dst, P1.X + (P1.DMX * W), P1.Y + (P1.DMY * W), U0, 1); Inc(Dst);
        nvg__vset(Dst, P1.X - (P1.DMX * W), P1.Y - (P1.DMY * W), U1, 1); Inc(Dst);
      end;
      P0 := P1;
      Inc(P1);
    end;
    if Loop then
    begin
      { Loop it }
      nvg__vset(Dst, Verts[0].X, Verts[0].Y, U0, 1); Inc(Dst);
      nvg__vset(Dst, Verts[1].X, Verts[1].Y, U1, 1); Inc(Dst);
    end
    else
    begin
      { Add cap }
      DX := P1.X - P0.X;
      DY := P1.Y - P0.Y;
      nvg__normalize(DX, DY);
      if LineCap = NVG_BUTT then
        Dst := nvg__buttCapEnd(Dst, P1, DX, DY, W, -AA * 0.5, AA, U0, U1)
      else if (LineCap = NVG_BUTT) or (LineCap = NVG_SQUARE) then
        Dst := nvg__buttCapEnd(Dst, P1, DX, DY, W, W - AA, AA, U0, U1)
      else if LineCap = NVG_ROUND then
        Dst := nvg__roundCapEnd(Dst, P1, DX, DY, W, NCap, AA, U0, U1);
    end;
    Path.NStroke := Dst - Verts;
    Verts := Dst;
  end;
  Result := True;
end;

function nvg__expandFill(Ctx: PNVGcontext; W: Single; LineJoin: Integer; MiterLimit: Single): Boolean;
var
  Cache: PNVGpathCache;
  Verts, Dst: PNVGvertex;
  CVerts, I, J: Integer;
  Convex, Fringe: Boolean;
  AA, RW, LW, WOff, RU, LU, DLX0, DLY0, DLX1, DLY1, LX, LY, LX0, LY0, LX1, LY1: Single;
  Path: PNVGpath;
  Pts, P0, P1: PNVGpoint;
begin
  Cache := Ctx.Cache;
  AA := Ctx.FringeWidth;
  Fringe := W > 0.0;
  nvg__calculateJoins(Ctx, W, LineJoin, MiterLimit);
  { Calculate max vertex usage }
  CVerts := 0;
  for I := 0 to Cache.NPaths - 1 do
  begin
    Path := @Cache.Paths[I];
    Inc(CVerts, Path.Count + Path.NBevel + 1);
    if Fringe then
      Inc(CVerts, (Path.Count + Path.NBevel * 5 + 1) * 2); { plus one for loop }
  end;
  Verts := nvg__allocTempVerts(Ctx, CVerts);
  if Verts = nil then
    Exit(False);
  Convex := (Cache.NPaths = 1) and (Cache.Paths[0].Convex <> 0);
  for I := 0 to Cache.NPaths - 1 do
  begin
    Path := @Cache.Paths[I];
    Pts := @Cache.Points[Path.First];
    { Calculate shape vertices }
    WOff := 0.5 * AA;
    Dst := Verts;
    Path.Fill := Dst;
    if Fringe then
    begin
      { Looping }
      P0 := @Pts[Path.Count - 1];
      P1 := @Pts[0];
      for J := 0 to Path.Count - 1 do
      begin
        if P1.Flags and NVG_PT_BEVEL <> 0 then
        begin
          DLX0 := P0.DY;
          DLY0 := -P0.DX;
          DLX1 := P1.DY;
          DLY1 := -P1.DX;
          if P1.Flags and NVG_PT_LEFT <> 0 then
          begin
            LX := P1.X + P1.DMX * WOff;
            LY := P1.Y + P1.DMY * WOff;
            nvg__vset(Dst, LX, LY, 0.5, 1); Inc(Dst);
          end
          else
          begin
            LX0 := P1.X + DLX0 * WOff;
            LY0 := P1.Y + DLY0 * WOff;
            LX1 := P1.X + DLX1 * WOff;
            LY1 := P1.Y + DLY1 * WOff;
            nvg__vset(Dst, LX0, LY0, 0.5, 1); Inc(Dst);
            nvg__vset(Dst, LX1, LY1, 0.5, 1); Inc(Dst);
          end;
        end
        else
        begin
          nvg__vset(Dst, P1.X + (P1.DMX * WOff), P1.Y + (P1.DMY * WOff), 0.5, 1); Inc(Dst);
        end;
        P0 := P1;
        Inc(P1);
      end;
    end
    else
    begin
      for J := 0 to Path.Count - 1 do
      begin
        nvg__vset(Dst, Pts[J].X, Pts[J].Y, 0.5, 1);
        Inc(Dst);
      end;
    end;
    Path.NFill := Dst - Verts;
    Verts := Dst;
    { Calculate fringe }
    if Fringe then
    begin
      LW := W + WOff;
      RW := W - WOff;
      LU := 0;
      RU := 1;
      Dst := Verts;
      Path.Stroke := Dst;
      { Create only half a fringe for convex shapes so that the shape can be
        rendered without stenciling }
      if Convex then
      begin
        { This should generate the same vertex as fill inset above }
        LW := WOff;
        { Set outline fade at middle }
        LU := 0.5;
      end;
      { Looping }
      P0 := @Pts[Path.Count - 1];
      P1 := @Pts[0];
      for J := 0 to Path.Count - 1 do
      begin
        if (P1.Flags and (NVG_PT_BEVEL or NVG_PR_INNERBEVEL)) <> 0 then
          Dst := nvg__bevelJoin(Dst, P0, P1, LW, RW, LU, RU, Ctx.FringeWidth)
        else
        begin
          nvg__vset(Dst, P1.X + (P1.DMX * LW), P1.Y + (P1.DMY * LW), LU, 1); Inc(Dst);
          nvg__vset(Dst, P1.X - (P1.DMX * RW), P1.Y - (P1.DMY * RW), RU, 1); Inc(Dst);
        end;
        P0 := P1;
        Inc(P1);
      end;
      { Loop it }
      nvg__vset(Dst, Verts[0].X, Verts[0].Y, LU, 1); Inc(Dst);
      nvg__vset(Dst, Verts[1].X, Verts[1].Y, RU, 1); Inc(Dst);
      Path.NStroke := Dst - Verts;
      Verts := Dst;
    end
    else
    begin
      Path.Stroke := nil;
      Path.NStroke := 0;
    end;
  end;
  Result := True;
end;

{ Draw }

procedure nvgBeginPath(Ctx: PNVGcontext);
begin
  Ctx.NCommands := 0;
  nvg__clearPathCache(Ctx);
end;

procedure nvgMoveTo(Ctx: PNVGcontext; X, Y: Single);
var
  Vals: array[0..2] of Single;
begin
  Vals[0] := NVG_MOVETO;
  Vals[1] := X;
  Vals[2] := Y;
  nvg__appendCommands(Ctx, @Vals[0], Length(Vals));
end;

procedure nvgLineTo(Ctx: PNVGcontext; X, Y: Single);
var
  Vals: array[0..2] of Single;
begin
  Vals[0] := NVG_LINETO;
  Vals[1] := X;
  Vals[2] := Y;
  nvg__appendCommands(Ctx, @Vals[0], Length(Vals));
end;

procedure nvgBezierTo(Ctx: PNVGcontext; C1X, C1Y, C2X, C2Y, X, Y: Single);
var
  Vals: array[0..6] of Single;
begin
  Vals[0] := NVG_BEZIERTO;
  Vals[1] := C1X;
  Vals[2] := C1Y;
  Vals[3] := C2X;
  Vals[4] := C2Y;
  Vals[5] := X;
  Vals[6] := Y;
  nvg__appendCommands(Ctx, @Vals[0], Length(Vals));
end;

procedure nvgQuadTo(Ctx: PNVGcontext; CX, CY, X, Y: Single);
var
  X0, Y0: Single;
  Vals: array[0..6] of Single;
begin
  X0 := Ctx.CommandX;
  Y0 := Ctx.CommandY;
  Vals[0] := NVG_BEZIERTO;
  Vals[1] := X0 + 2.0 / 3.0 * (CX - X0);
  Vals[2] := Y0 + 2.0 / 3.0 * (CY - Y0);
  Vals[3] := X + 2.0 / 3.0 * (CX - X);
  Vals[4] := Y + 2.0 / 3.0 * (CY - Y);
  Vals[5] := X;
  Vals[6] := Y;
  nvg__appendCommands(Ctx, @Vals[0], Length(Vals));
end;

procedure nvgArcTo(Ctx: PNVGcontext; X1, Y1, X2, Y2, Radius: Single);
var
  X0, Y0, DX0, DY0, DX1, DY1, A, D, CX, CY, A0, A1: Single;
  Dir: Integer;
begin
  X0 := Ctx.CommandX;
  Y0 := Ctx.CommandY;
  if Ctx.NCommands = 0 then
    Exit;
  { Handle degenerate cases }
  if nvg__ptEquals(X0, Y0, X1, Y1, Ctx.DistTol) or
    nvg__ptEquals(X1, Y1, X2, Y2, Ctx.DistTol) or
    (nvg__distPtSeg(X1, Y1, X0, Y0, X2, Y2) < Ctx.DistTol * Ctx.DistTol) or
    (Radius < Ctx.DistTol) then
  begin
    nvgLineTo(Ctx, X1, Y1);
    Exit;
  end;
  { Calculate tangential circle to lines (x0,y0)-(x1,y1) and (x1,y1)-(x2,y2) }
  DX0 := X0 - X1;
  DY0 := Y0 - Y1;
  DX1 := X2 - X1;
  DY1 := Y2 - Y1;
  nvg__normalize(DX0, DY0);
  nvg__normalize(DX1, DY1);
  A := nvg__acosf(DX0 * DX1 + DY0 * DY1);
  D := Radius / nvg__tanf(A / 2.0);
  if D > 10000.0 then
  begin
    nvgLineTo(Ctx, X1, Y1);
    Exit;
  end;
  if nvg__cross(DX0, DY0, DX1, DY1) > 0.0 then
  begin
    CX := X1 + DX0 * D + DY0 * Radius;
    CY := Y1 + DY0 * D + -DX0 * Radius;
    A0 := nvg__atan2f(DX0, -DY0);
    A1 := nvg__atan2f(-DX1, DY1);
    Dir := NVG_CW;
  end
  else
  begin
    CX := X1 + DX0 * D + -DY0 * Radius;
    CY := Y1 + DY0 * D + DX0 * Radius;
    A0 := nvg__atan2f(-DX0, DY0);
    A1 := nvg__atan2f(DX1, -DY1);
    Dir := NVG_CCW;
  end;
  nvgArc(Ctx, CX, CY, Radius, A0, A1, Dir);
end;

procedure nvgClosePath(Ctx: PNVGcontext);
var
  Vals: array[0..0] of Single;
begin
  Vals[0] := NVG_CLOSE;
  nvg__appendCommands(Ctx, @Vals[0], Length(Vals));
end;

procedure nvgPathWinding(Ctx: PNVGcontext; Dir: Integer);
var
  Vals: array[0..1] of Single;
begin
  Vals[0] := NVG_WINDING;
  Vals[1] := Dir;
  nvg__appendCommands(Ctx, @Vals[0], Length(Vals));
end;

procedure nvgArc(Ctx: PNVGcontext; CX, CY, R, A0, A1: Single; Dir: Integer);
var
  A, DA, HDA, Kappa, DX, DY, X, Y, TanX, TanY, PX, PY, PTanX, PTanY: Single;
  Vals: array[0..3 + 5 * 7 + 100 - 1] of Single;
  I, NDivs, NVals, MoveCmd: Integer;
begin
  PX := 0; PY := 0; PTanX := 0; PTanY := 0;
  if Ctx.NCommands > 0 then
    MoveCmd := NVG_LINETO
  else
    MoveCmd := NVG_MOVETO;
  { Clamp angles }
  DA := A1 - A0;
  if Dir = NVG_CW then
  begin
    if nvg__absf(DA) >= NVG_PI * 2 then
      DA := NVG_PI * 2
    else
      while DA < 0.0 do
        DA := DA + NVG_PI * 2;
  end
  else
  begin
    if nvg__absf(DA) >= NVG_PI * 2 then
      DA := -NVG_PI * 2
    else
      while DA > 0.0 do
        DA := DA - NVG_PI * 2;
  end;
  { Split arc into max 90 degree segments }
  NDivs := nvg__maxi(1, nvg__mini(Trunc(nvg__absf(DA) / (NVG_PI * 0.5) + 0.5), 5));
  HDA := (DA / NDivs) / 2.0;
  Kappa := nvg__absf(4.0 / 3.0 * (1.0 - nvg__cosf(HDA)) / nvg__sinf(HDA));
  if Dir = NVG_CCW then
    Kappa := -Kappa;
  NVals := 0;
  for I := 0 to NDivs do
  begin
    A := A0 + DA * (I / NDivs);
    DX := nvg__cosf(A);
    DY := nvg__sinf(A);
    X := CX + DX * R;
    Y := CY + DY * R;
    TanX := -DY * R * Kappa;
    TanY := DX * R * Kappa;
    if I = 0 then
    begin
      Vals[NVals] := MoveCmd; Inc(NVals);
      Vals[NVals] := X; Inc(NVals);
      Vals[NVals] := Y; Inc(NVals);
    end
    else
    begin
      Vals[NVals] := NVG_BEZIERTO; Inc(NVals);
      Vals[NVals] := PX + PTanX; Inc(NVals);
      Vals[NVals] := PY + PTanY; Inc(NVals);
      Vals[NVals] := X - TanX; Inc(NVals);
      Vals[NVals] := Y - TanY; Inc(NVals);
      Vals[NVals] := X; Inc(NVals);
      Vals[NVals] := Y; Inc(NVals);
    end;
    PX := X;
    PY := Y;
    PTanX := TanX;
    PTanY := TanY;
  end;
  nvg__appendCommands(Ctx, @Vals[0], NVals);
end;

procedure nvgRect(Ctx: PNVGcontext; X, Y, W, H: Single);
var
  Vals: array[0..12] of Single;
begin
  Vals[0] := NVG_MOVETO; Vals[1] := X; Vals[2] := Y;
  Vals[3] := NVG_LINETO; Vals[4] := X; Vals[5] := Y + H;
  Vals[6] := NVG_LINETO; Vals[7] := X + W; Vals[8] := Y + H;
  Vals[9] := NVG_LINETO; Vals[10] := X + W; Vals[11] := Y;
  Vals[12] := NVG_CLOSE;
  nvg__appendCommands(Ctx, @Vals[0], Length(Vals));
end;

procedure nvgRoundedRect(Ctx: PNVGcontext; X, Y, W, H, R: Single);
begin
  nvgRoundedRectVarying(Ctx, X, Y, W, H, R, R, R, R);
end;

procedure nvgRoundedRectVarying(Ctx: PNVGcontext; X, Y, W, H, RadTopLeft, RadTopRight,
  RadBottomRight, RadBottomLeft: Single);
var
  HalfW, HalfH, RxBL, RyBL, RxBR, RyBR, RxTR, RyTR, RxTL, RyTL, K: Single;
  Vals: array[0..44] of Single;
begin
  if (RadTopLeft < 0.1) and (RadTopRight < 0.1) and (RadBottomRight < 0.1) and (RadBottomLeft < 0.1) then
  begin
    nvgRect(Ctx, X, Y, W, H);
    Exit;
  end;
  HalfW := nvg__absf(W) * 0.5;
  HalfH := nvg__absf(H) * 0.5;
  RxBL := nvg__minf(RadBottomLeft, HalfW) * nvg__signf(W);
  RyBL := nvg__minf(RadBottomLeft, HalfH) * nvg__signf(H);
  RxBR := nvg__minf(RadBottomRight, HalfW) * nvg__signf(W);
  RyBR := nvg__minf(RadBottomRight, HalfH) * nvg__signf(H);
  RxTR := nvg__minf(RadTopRight, HalfW) * nvg__signf(W);
  RyTR := nvg__minf(RadTopRight, HalfH) * nvg__signf(H);
  RxTL := nvg__minf(RadTopLeft, HalfW) * nvg__signf(W);
  RyTL := nvg__minf(RadTopLeft, HalfH) * nvg__signf(H);
  K := 1 - NVG_KAPPA90;
  Vals[0] := NVG_MOVETO; Vals[1] := X; Vals[2] := Y + RyTL;
  Vals[3] := NVG_LINETO; Vals[4] := X; Vals[5] := Y + H - RyBL;
  Vals[6] := NVG_BEZIERTO; Vals[7] := X; Vals[8] := Y + H - RyBL * K; Vals[9] := X + RxBL * K;
    Vals[10] := Y + H; Vals[11] := X + RxBL; Vals[12] := Y + H;
  Vals[13] := NVG_LINETO; Vals[14] := X + W - RxBR; Vals[15] := Y + H;
  Vals[16] := NVG_BEZIERTO; Vals[17] := X + W - RxBR * K; Vals[18] := Y + H; Vals[19] := X + W;
    Vals[20] := Y + H - RyBR * K; Vals[21] := X + W; Vals[22] := Y + H - RyBR;
  Vals[23] := NVG_LINETO; Vals[24] := X + W; Vals[25] := Y + RyTR;
  Vals[26] := NVG_BEZIERTO; Vals[27] := X + W; Vals[28] := Y + RyTR * K; Vals[29] := X + W - RxTR * K;
    Vals[30] := Y; Vals[31] := X + W - RxTR; Vals[32] := Y;
  Vals[33] := NVG_LINETO; Vals[34] := X + RxTL; Vals[35] := Y;
  Vals[36] := NVG_BEZIERTO; Vals[37] := X + RxTL * K; Vals[38] := Y; Vals[39] := X;
    Vals[40] := Y + RyTL * K; Vals[41] := X; Vals[42] := Y + RyTL;
  Vals[43] := NVG_CLOSE;
  nvg__appendCommands(Ctx, @Vals[0], 44);
end;

procedure nvgEllipse(Ctx: PNVGcontext; CX, CY, RX, RY: Single);
var
  Vals: array[0..31] of Single;
begin
  Vals[0] := NVG_MOVETO; Vals[1] := CX - RX; Vals[2] := CY;
  Vals[3] := NVG_BEZIERTO; Vals[4] := CX - RX; Vals[5] := CY + RY * NVG_KAPPA90;
    Vals[6] := CX - RX * NVG_KAPPA90; Vals[7] := CY + RY; Vals[8] := CX; Vals[9] := CY + RY;
  Vals[10] := NVG_BEZIERTO; Vals[11] := CX + RX * NVG_KAPPA90; Vals[12] := CY + RY;
    Vals[13] := CX + RX; Vals[14] := CY + RY * NVG_KAPPA90; Vals[15] := CX + RX; Vals[16] := CY;
  Vals[17] := NVG_BEZIERTO; Vals[18] := CX + RX; Vals[19] := CY - RY * NVG_KAPPA90;
    Vals[20] := CX + RX * NVG_KAPPA90; Vals[21] := CY - RY; Vals[22] := CX; Vals[23] := CY - RY;
  Vals[24] := NVG_BEZIERTO; Vals[25] := CX - RX * NVG_KAPPA90; Vals[26] := CY - RY;
    Vals[27] := CX - RX; Vals[28] := CY - RY * NVG_KAPPA90; Vals[29] := CX - RX; Vals[30] := CY;
  Vals[31] := NVG_CLOSE;
  nvg__appendCommands(Ctx, @Vals[0], Length(Vals));
end;

procedure nvgCircle(Ctx: PNVGcontext; CX, CY, R: Single);
begin
  nvgEllipse(Ctx, CX, CY, R, R);
end;

procedure nvgDebugDumpPathCache(Ctx: PNVGcontext);
var
  Path: PNVGpath;
  I, J: Integer;
begin
  WriteLn('Dumping ', Ctx.Cache.NPaths, ' cached paths');
  for I := 0 to Ctx.Cache.NPaths - 1 do
  begin
    Path := @Ctx.Cache.Paths[I];
    WriteLn(' - Path ', I);
    if Path.NFill <> 0 then
    begin
      WriteLn('   - fill: ', Path.NFill);
      for J := 0 to Path.NFill - 1 do
        WriteLn(Path.Fill[J].X:0:6, #9, Path.Fill[J].Y:0:6);
    end;
    if Path.NStroke <> 0 then
    begin
      WriteLn('   - stroke: ', Path.NStroke);
      for J := 0 to Path.NStroke - 1 do
        WriteLn(Path.Stroke[J].X:0:6, #9, Path.Stroke[J].Y:0:6);
    end;
  end;
end;

procedure nvgFill(Ctx: PNVGcontext);
var
  State: PNVGstate;
  Path: PNVGpath;
  FillPaint: TNVGpaint;
  I: Integer;
begin
  State := nvg__getState(Ctx);
  FillPaint := State.Fill;
  nvg__flattenPaths(Ctx);
  if (Ctx.Params.EdgeAntiAlias <> 0) and (State.ShapeAntiAlias <> 0) then
    nvg__expandFill(Ctx, Ctx.FringeWidth, NVG_MITER, 2.4)
  else
    nvg__expandFill(Ctx, 0.0, NVG_MITER, 2.4);
  { Apply global alpha }
  FillPaint.InnerColor.A := FillPaint.InnerColor.A * State.Alpha;
  FillPaint.OuterColor.A := FillPaint.OuterColor.A * State.Alpha;
  Ctx.Params.RenderFill(Ctx.Params.UserPtr, @FillPaint, State.CompositeOperation, @State.Scissor,
    Ctx.FringeWidth, @Ctx.Cache.Bounds[0], Ctx.Cache.Paths, Ctx.Cache.NPaths, State.FillRule);
  { Count triangles }
  for I := 0 to Ctx.Cache.NPaths - 1 do
  begin
    Path := @Ctx.Cache.Paths[I];
    Inc(Ctx.FillTriCount, Path.NFill - 2);
    Inc(Ctx.FillTriCount, Path.NStroke - 2);
    Inc(Ctx.DrawCallCount, 2);
  end;
end;

procedure nvgStroke(Ctx: PNVGcontext);
var
  State: PNVGstate;
  Scale, StrokeWidth, Alpha: Single;
  StrokePaint: TNVGpaint;
  Path: PNVGpath;
  I: Integer;
begin
  State := nvg__getState(Ctx);
  Scale := nvg__getAverageScale(State.XForm);
  StrokeWidth := nvg__clampf(State.StrokeWidth * Scale, 0.0, 200.0);
  StrokePaint := State.Stroke;
  if StrokeWidth < Ctx.FringeWidth then
  begin
    { If the stroke width is less than pixel size, use alpha to emulate
      coverage. Since coverage is area, scale by alpha*alpha }
    Alpha := nvg__clampf(StrokeWidth / Ctx.FringeWidth, 0.0, 1.0);
    StrokePaint.InnerColor.A := StrokePaint.InnerColor.A * Alpha * Alpha;
    StrokePaint.OuterColor.A := StrokePaint.OuterColor.A * Alpha * Alpha;
    StrokeWidth := Ctx.FringeWidth;
  end;
  { Apply global alpha }
  StrokePaint.InnerColor.A := StrokePaint.InnerColor.A * State.Alpha;
  StrokePaint.OuterColor.A := StrokePaint.OuterColor.A * State.Alpha;
  nvg__flattenPaths(Ctx);
  if (Ctx.Params.EdgeAntiAlias <> 0) and (State.ShapeAntiAlias <> 0) then
    nvg__expandStroke(Ctx, StrokeWidth * 0.5, Ctx.FringeWidth, State.LineCap, State.LineJoin, State.MiterLimit)
  else
    nvg__expandStroke(Ctx, StrokeWidth * 0.5, 0.0, State.LineCap, State.LineJoin, State.MiterLimit);
  Ctx.Params.RenderStroke(Ctx.Params.UserPtr, @StrokePaint, State.CompositeOperation, @State.Scissor,
    Ctx.FringeWidth, StrokeWidth, Ctx.Cache.Paths, Ctx.Cache.NPaths);
  { Count triangles }
  for I := 0 to Ctx.Cache.NPaths - 1 do
  begin
    Path := @Ctx.Cache.Paths[I];
    Inc(Ctx.StrokeTriCount, Path.NStroke - 2);
    Inc(Ctx.DrawCallCount);
  end;
end;

{ Add fonts }

function nvgCreateFont(Ctx: PNVGcontext; const Name, FileName: string): Integer;
begin
  Result := fonsAddFont(Ctx.Fs, Name, FileName, 0);
end;

function nvgCreateFontAtIndex(Ctx: PNVGcontext; const Name, FileName: string; FontIndex: Integer): Integer;
begin
  Result := fonsAddFont(Ctx.Fs, Name, FileName, FontIndex);
end;

function nvgCreateFontMem(Ctx: PNVGcontext; const Name: string; Data: PByte; NData: Integer; FreeData: Boolean): Integer;
begin
  Result := fonsAddFontMem(Ctx.Fs, Name, Data, NData, FreeData, 0);
end;

function nvgCreateFontMemAtIndex(Ctx: PNVGcontext; const Name: string; Data: PByte; NData: Integer;
  FreeData: Boolean; FontIndex: Integer): Integer;
begin
  Result := fonsAddFontMem(Ctx.Fs, Name, Data, NData, FreeData, FontIndex);
end;

function nvgFindFont(Ctx: PNVGcontext; const Name: string): Integer;
begin
  Result := fonsGetFontByName(Ctx.Fs, Name);
end;

function nvgAddFallbackFontId(Ctx: PNVGcontext; BaseFont, FallbackFont: Integer): Integer;
begin
  if (BaseFont = -1) or (FallbackFont = -1) then
    Exit(0);
  Result := fonsAddFallbackFont(Ctx.Fs, BaseFont, FallbackFont);
end;

function nvgAddFallbackFont(Ctx: PNVGcontext; const BaseFont, FallbackFont: string): Integer;
begin
  Result := nvgAddFallbackFontId(Ctx, nvgFindFont(Ctx, BaseFont), nvgFindFont(Ctx, FallbackFont));
end;

procedure nvgResetFallbackFontsId(Ctx: PNVGcontext; BaseFont: Integer);
begin
  fonsResetFallbackFont(Ctx.Fs, BaseFont);
end;

procedure nvgResetFallbackFonts(Ctx: PNVGcontext; const BaseFont: string);
begin
  nvgResetFallbackFontsId(Ctx, nvgFindFont(Ctx, BaseFont));
end;

{ State setting }

procedure nvgFontSize(Ctx: PNVGcontext; Size: Single);
begin
  nvg__getState(Ctx).FontSize := Size;
end;

procedure nvgFontBlur(Ctx: PNVGcontext; Blur: Single);
begin
  nvg__getState(Ctx).FontBlur := Blur;
end;

procedure nvgTextLetterSpacing(Ctx: PNVGcontext; Spacing: Single);
begin
  nvg__getState(Ctx).LetterSpacing := Spacing;
end;

procedure nvgTextLineHeight(Ctx: PNVGcontext; LineHeight: Single);
begin
  nvg__getState(Ctx).LineHeight := LineHeight;
end;

procedure nvgTextAlign(Ctx: PNVGcontext; Align: Integer);
begin
  nvg__getState(Ctx).TextAlign := Align;
end;

procedure nvgFontFaceId(Ctx: PNVGcontext; Font: Integer);
begin
  nvg__getState(Ctx).FontId := Font;
end;

procedure nvgFontFace(Ctx: PNVGcontext; const Font: string);
begin
  nvg__getState(Ctx).FontId := fonsGetFontByName(Ctx.Fs, Font);
end;

function nvg__quantize(A, D: Single): Single; inline;
begin
  Result := Trunc(A / D + 0.5) * D;
end;

function nvg__getFontScale(State: PNVGstate): Single;
begin
  Result := nvg__minf(nvg__quantize(nvg__getAverageScale(State.XForm), 0.01), 4.0);
end;

procedure nvg__flushTextTexture(Ctx: PNVGcontext);
var
  Dirty: array[0..3] of Integer;
  FontImage, IW, IH, X, Y, W, H: Integer;
  Data: PByte;
begin
  if fonsValidateTexture(Ctx.Fs, @Dirty[0]) <> 0 then
  begin
    FontImage := Ctx.FontImages[Ctx.FontImageIdx];
    { Update texture }
    if FontImage <> 0 then
    begin
      Data := fonsGetTextureData(Ctx.Fs, @IW, @IH);
      X := Dirty[0];
      Y := Dirty[1];
      W := Dirty[2] - Dirty[0];
      H := Dirty[3] - Dirty[1];
      Ctx.Params.RenderUpdateTexture(Ctx.Params.UserPtr, FontImage, X, Y, W, H, Data);
    end;
  end;
end;

function nvg__allocTextAtlas(Ctx: PNVGcontext): Boolean;
var
  IW, IH: Integer;
begin
  nvg__flushTextTexture(Ctx);
  if Ctx.FontImageIdx >= NVG_MAX_FONTIMAGES - 1 then
    Exit(False);
  { If next fontImage already have a texture }
  if Ctx.FontImages[Ctx.FontImageIdx + 1] <> 0 then
    nvgImageSize(Ctx, Ctx.FontImages[Ctx.FontImageIdx + 1], @IW, @IH)
  else
  begin
    { Calculate the new font image size and create it }
    nvgImageSize(Ctx, Ctx.FontImages[Ctx.FontImageIdx], @IW, @IH);
    if IW > IH then
      IH := IH * 2
    else
      IW := IW * 2;
    if (IW > NVG_MAX_FONTIMAGE_SIZE) or (IH > NVG_MAX_FONTIMAGE_SIZE) then
    begin
      IW := NVG_MAX_FONTIMAGE_SIZE;
      IH := NVG_MAX_FONTIMAGE_SIZE;
    end;
    Ctx.FontImages[Ctx.FontImageIdx + 1] := Ctx.Params.RenderCreateTexture(Ctx.Params.UserPtr,
      NVG_TEXTURE_ALPHA, IW, IH, 0, nil);
  end;
  Inc(Ctx.FontImageIdx);
  fonsResetAtlas(Ctx.Fs, IW, IH);
  Result := True;
end;

procedure nvg__renderText(Ctx: PNVGcontext; Verts: PNVGvertex; NVerts: Integer);
var
  State: PNVGstate;
  Paint: TNVGpaint;
begin
  State := nvg__getState(Ctx);
  Paint := State.Fill;
  { Render triangles }
  Paint.Image := Ctx.FontImages[Ctx.FontImageIdx];
  { Apply global alpha }
  Paint.InnerColor.A := Paint.InnerColor.A * State.Alpha;
  Paint.OuterColor.A := Paint.OuterColor.A * State.Alpha;
  Ctx.Params.RenderTriangles(Ctx.Params.UserPtr, @Paint, State.CompositeOperation, @State.Scissor,
    Verts, NVerts, Ctx.FringeWidth);
  Inc(Ctx.DrawCallCount);
  Inc(Ctx.TextTriCount, NVerts div 3);
end;

function nvg__isTransformFlipped(const XForm: TNVGxform): Boolean;
var
  Det: Single;
begin
  Det := XForm[0] * XForm[3] - XForm[2] * XForm[1];
  Result := Det < 0;
end;

procedure nvg__setTextState(Ctx: PNVGcontext; State: PNVGstate; Scale: Single);
begin
  fonsSetSize(Ctx.Fs, State.FontSize * Scale);
  fonsSetSpacing(Ctx.Fs, State.LetterSpacing * Scale);
  fonsSetBlur(Ctx.Fs, State.FontBlur * Scale);
  fonsSetAlign(Ctx.Fs, State.TextAlign);
  fonsSetFont(Ctx.Fs, State.FontId);
end;

function nvgText(Ctx: PNVGcontext; X, Y: Single; Str, EndStr: PAnsiChar): Single;
var
  State: PNVGstate;
  Iter, PrevIter: TFonsTextIter;
  Q: TFonsQuad;
  Verts: PNVGvertex;
  Scale, InvScale, Tmp: Single;
  CVerts, NVerts: Integer;
  IsFlipped: Boolean;
  C: array[0..7] of Single;
begin
  State := nvg__getState(Ctx);
  Scale := nvg__getFontScale(State) * Ctx.DevicePxRatio;
  InvScale := 1.0 / Scale;
  NVerts := 0;
  IsFlipped := nvg__isTransformFlipped(State.XForm);
  if Str = nil then
    Exit(X);
  if EndStr = nil then
    EndStr := Str + StrLen(Str);
  if State.FontId = FONS_INVALID then
    Exit(X);
  nvg__setTextState(Ctx, State, Scale);
  { Conservative estimate }
  CVerts := nvg__maxi(2, EndStr - Str) * 6;
  Verts := nvg__allocTempVerts(Ctx, CVerts);
  if Verts = nil then
    Exit(X);
  fonsTextIterInit(Ctx.Fs, @Iter, X * Scale, Y * Scale, Str, EndStr, FONS_GLYPH_BITMAP_REQUIRED);
  PrevIter := Iter;
  while fonsTextIterNext(Ctx.Fs, @Iter, @Q) <> 0 do
  begin
    { Can not retrieve glyph? }
    if Iter.PrevGlyphIndex = -1 then
    begin
      if NVerts <> 0 then
      begin
        nvg__renderText(Ctx, Verts, NVerts);
        NVerts := 0;
      end;
      if not nvg__allocTextAtlas(Ctx) then
        Break; { no memory }
      Iter := PrevIter;
      { Try again }
      fonsTextIterNext(Ctx.Fs, @Iter, @Q);
      { Still can not find glyph? }
      if Iter.PrevGlyphIndex = -1 then
        Break;
    end;
    PrevIter := Iter;
    if IsFlipped then
    begin
      Tmp := Q.Y0; Q.Y0 := Q.Y1; Q.Y1 := Tmp;
      Tmp := Q.T0; Q.T0 := Q.T1; Q.T1 := Tmp;
    end;
    { Transform corners }
    nvgTransformPoint(C[0], C[1], State.XForm, Q.X0 * InvScale, Q.Y0 * InvScale);
    nvgTransformPoint(C[2], C[3], State.XForm, Q.X1 * InvScale, Q.Y0 * InvScale);
    nvgTransformPoint(C[4], C[5], State.XForm, Q.X1 * InvScale, Q.Y1 * InvScale);
    nvgTransformPoint(C[6], C[7], State.XForm, Q.X0 * InvScale, Q.Y1 * InvScale);
    { Create triangles }
    if NVerts + 6 <= CVerts then
    begin
      nvg__vset(@Verts[NVerts], C[0], C[1], Q.S0, Q.T0); Inc(NVerts);
      nvg__vset(@Verts[NVerts], C[4], C[5], Q.S1, Q.T1); Inc(NVerts);
      nvg__vset(@Verts[NVerts], C[2], C[3], Q.S1, Q.T0); Inc(NVerts);
      nvg__vset(@Verts[NVerts], C[0], C[1], Q.S0, Q.T0); Inc(NVerts);
      nvg__vset(@Verts[NVerts], C[6], C[7], Q.S0, Q.T1); Inc(NVerts);
      nvg__vset(@Verts[NVerts], C[4], C[5], Q.S1, Q.T1); Inc(NVerts);
    end;
  end;
  nvg__flushTextTexture(Ctx);
  nvg__renderText(Ctx, Verts, NVerts);
  Result := Iter.NextX / Scale;
end;

function nvgText(Ctx: PNVGcontext; X, Y: Single; const S: string): Single;
begin
  if S = '' then
    Exit(X);
  Result := nvgText(Ctx, X, Y, PAnsiChar(S), PAnsiChar(S) + Length(S));
end;

procedure nvgTextBox(Ctx: PNVGcontext; X, Y, BreakRowWidth: Single; Str, EndStr: PAnsiChar);
var
  State: PNVGstate;
  Rows: array[0..1] of TNVGtextRow;
  NRows, I, OldAlign, HAlign, VAlign: Integer;
  LineH: Single;
  Row: PNVGtextRow;
begin
  State := nvg__getState(Ctx);
  OldAlign := State.TextAlign;
  HAlign := State.TextAlign and (NVG_ALIGN_LEFT or NVG_ALIGN_CENTER or NVG_ALIGN_RIGHT);
  VAlign := State.TextAlign and (NVG_ALIGN_TOP or NVG_ALIGN_MIDDLE or NVG_ALIGN_BOTTOM or NVG_ALIGN_BASELINE);
  LineH := 0;
  if State.FontId = FONS_INVALID then
    Exit;
  nvgTextMetrics(Ctx, nil, nil, @LineH);
  State.TextAlign := NVG_ALIGN_LEFT or VAlign;
  NRows := nvgTextBreakLines(Ctx, Str, EndStr, BreakRowWidth, @Rows[0], 2);
  while NRows <> 0 do
  begin
    for I := 0 to NRows - 1 do
    begin
      Row := @Rows[I];
      if HAlign and NVG_ALIGN_LEFT <> 0 then
        nvgText(Ctx, X, Y, Row.Start, Row.EndStr)
      else if HAlign and NVG_ALIGN_CENTER <> 0 then
        nvgText(Ctx, X + BreakRowWidth * 0.5 - Row.Width * 0.5, Y, Row.Start, Row.EndStr)
      else if HAlign and NVG_ALIGN_RIGHT <> 0 then
        nvgText(Ctx, X + BreakRowWidth - Row.Width, Y, Row.Start, Row.EndStr);
      Y := Y + LineH * State.LineHeight;
    end;
    Str := Rows[NRows - 1].Next;
    NRows := nvgTextBreakLines(Ctx, Str, EndStr, BreakRowWidth, @Rows[0], 2);
  end;
  State.TextAlign := OldAlign;
end;

procedure nvgTextBox(Ctx: PNVGcontext; X, Y, BreakRowWidth: Single; const S: string);
begin
  if S = '' then
    Exit;
  nvgTextBox(Ctx, X, Y, BreakRowWidth, PAnsiChar(S), PAnsiChar(S) + Length(S));
end;

function nvgTextGlyphPositions(Ctx: PNVGcontext; X, Y: Single; Str, EndStr: PAnsiChar;
  Positions: PNVGglyphPosition; MaxPositions: Integer): Integer;
var
  State: PNVGstate;
  Scale, InvScale: Single;
  Iter, PrevIter: TFonsTextIter;
  Q: TFonsQuad;
  NPos: Integer;
begin
  State := nvg__getState(Ctx);
  Scale := nvg__getFontScale(State) * Ctx.DevicePxRatio;
  InvScale := 1.0 / Scale;
  NPos := 0;
  if State.FontId = FONS_INVALID then
    Exit(0);
  if Str = nil then
    Exit(0);
  if EndStr = nil then
    EndStr := Str + StrLen(Str);
  if Str = EndStr then
    Exit(0);
  nvg__setTextState(Ctx, State, Scale);
  fonsTextIterInit(Ctx.Fs, @Iter, X * Scale, Y * Scale, Str, EndStr, FONS_GLYPH_BITMAP_OPTIONAL);
  PrevIter := Iter;
  while fonsTextIterNext(Ctx.Fs, @Iter, @Q) <> 0 do
  begin
    { Can not retrieve glyph? }
    if (Iter.PrevGlyphIndex < 0) and nvg__allocTextAtlas(Ctx) then
    begin
      Iter := PrevIter;
      { Try again }
      fonsTextIterNext(Ctx.Fs, @Iter, @Q);
    end;
    PrevIter := Iter;
    Positions[NPos].Str := Iter.Str;
    Positions[NPos].X := Iter.X * InvScale;
    Positions[NPos].MinX := nvg__minf(Iter.X, Q.X0) * InvScale;
    Positions[NPos].MaxX := nvg__maxf(Iter.NextX, Q.X1) * InvScale;
    Inc(NPos);
    if NPos >= MaxPositions then
      Break;
  end;
  Result := NPos;
end;

const
  { NVGcodepointType }
  NVG_SPACE = 0;
  NVG_NEWLINE = 1;
  NVG_CHAR = 2;
  NVG_CJK_CHAR = 3;

function nvgTextBreakLines(Ctx: PNVGcontext; Str, EndStr: PAnsiChar; BreakRowWidth: Single;
  Rows: PNVGtextRow; MaxRows: Integer): Integer;
var
  State: PNVGstate;
  Scale, InvScale: Single;
  Iter, PrevIter: TFonsTextIter;
  Q: TFonsQuad;
  NRows: Integer;
  RowStartX, RowWidth, RowMinX, RowMaxX, WordStartX, WordMinX, BreakWidth, BreakMaxX, NextWidth: Single;
  RowStart, RowEnd, WordStart, BreakEnd: PAnsiChar;
  CharType, PType: Integer;
  PCodepoint, Cp: Cardinal;
begin
  State := nvg__getState(Ctx);
  Scale := nvg__getFontScale(State) * Ctx.DevicePxRatio;
  InvScale := 1.0 / Scale;
  NRows := 0;
  RowStartX := 0;
  RowWidth := 0;
  RowMinX := 0;
  RowMaxX := 0;
  RowStart := nil;
  RowEnd := nil;
  WordStart := nil;
  WordStartX := 0;
  WordMinX := 0;
  BreakEnd := nil;
  BreakWidth := 0;
  BreakMaxX := 0;
  CharType := NVG_SPACE;
  PType := NVG_SPACE;
  PCodepoint := 0;
  if MaxRows = 0 then
    Exit(0);
  if State.FontId = FONS_INVALID then
    Exit(0);
  if Str = nil then
    Exit(0);
  if EndStr = nil then
    EndStr := Str + StrLen(Str);
  if Str = EndStr then
    Exit(0);
  nvg__setTextState(Ctx, State, Scale);
  BreakRowWidth := BreakRowWidth * Scale;
  fonsTextIterInit(Ctx.Fs, @Iter, 0, 0, Str, EndStr, FONS_GLYPH_BITMAP_OPTIONAL);
  PrevIter := Iter;
  while fonsTextIterNext(Ctx.Fs, @Iter, @Q) <> 0 do
  begin
    { Can not retrieve glyph? }
    if (Iter.PrevGlyphIndex < 0) and nvg__allocTextAtlas(Ctx) then
    begin
      Iter := PrevIter;
      { Try again }
      fonsTextIterNext(Ctx.Fs, @Iter, @Q);
    end;
    PrevIter := Iter;
    Cp := Iter.Codepoint;
    case Cp of
      9, 11, 12, 32, $00A0:
        { \t \v \f space NBSP }
        CharType := NVG_SPACE;
      10:
        { \n }
        if PCodepoint = 13 then
          CharType := NVG_SPACE
        else
          CharType := NVG_NEWLINE;
      13:
        { \r }
        if PCodepoint = 10 then
          CharType := NVG_SPACE
        else
          CharType := NVG_NEWLINE;
      $0085:
        { NEL }
        CharType := NVG_NEWLINE;
    else
      if ((Cp >= $4E00) and (Cp <= $9FFF)) or
        ((Cp >= $3000) and (Cp <= $30FF)) or
        ((Cp >= $FF00) and (Cp <= $FFEF)) or
        ((Cp >= $1100) and (Cp <= $11FF)) or
        ((Cp >= $3130) and (Cp <= $318F)) or
        ((Cp >= $AC00) and (Cp <= $D7AF)) then
        CharType := NVG_CJK_CHAR
      else
        CharType := NVG_CHAR;
    end;
    if CharType = NVG_NEWLINE then
    begin
      { Always handle new lines }
      if RowStart <> nil then
        Rows[NRows].Start := RowStart
      else
        Rows[NRows].Start := Iter.Str;
      if RowEnd <> nil then
        Rows[NRows].EndStr := RowEnd
      else
        Rows[NRows].EndStr := Iter.Str;
      Rows[NRows].Width := RowWidth * InvScale;
      Rows[NRows].MinX := RowMinX * InvScale;
      Rows[NRows].MaxX := RowMaxX * InvScale;
      Rows[NRows].Next := Iter.Next;
      Inc(NRows);
      if NRows >= MaxRows then
        Exit(NRows);
      { Set null break point }
      BreakEnd := RowStart;
      BreakWidth := 0.0;
      BreakMaxX := 0.0;
      { Indicate to skip the white space at the beginning of the row }
      RowStart := nil;
      RowEnd := nil;
      RowWidth := 0;
      RowMinX := 0;
      RowMaxX := 0;
    end
    else
    begin
      if RowStart = nil then
      begin
        { Skip white space until the beginning of the line }
        if (CharType = NVG_CHAR) or (CharType = NVG_CJK_CHAR) then
        begin
          { The current char is the row so far }
          RowStartX := Iter.X;
          RowStart := Iter.Str;
          RowEnd := Iter.Next;
          RowWidth := Iter.NextX - RowStartX;
          RowMinX := Q.X0 - RowStartX;
          RowMaxX := Q.X1 - RowStartX;
          WordStart := Iter.Str;
          WordStartX := Iter.X;
          WordMinX := Q.X0 - RowStartX;
          { Set null break point }
          BreakEnd := RowStart;
          BreakWidth := 0.0;
          BreakMaxX := 0.0;
        end;
      end
      else
      begin
        NextWidth := Iter.NextX - RowStartX;
        { Track last non-white space character }
        if (CharType = NVG_CHAR) or (CharType = NVG_CJK_CHAR) then
        begin
          RowEnd := Iter.Next;
          RowWidth := Iter.NextX - RowStartX;
          RowMaxX := Q.X1 - RowStartX;
        end;
        { Track last end of a word }
        if (((PType = NVG_CHAR) or (PType = NVG_CJK_CHAR)) and (CharType = NVG_SPACE)) or (CharType = NVG_CJK_CHAR) then
        begin
          BreakEnd := Iter.Str;
          BreakWidth := RowWidth;
          BreakMaxX := RowMaxX;
        end;
        { Track last beginning of a word }
        if ((PType = NVG_SPACE) and ((CharType = NVG_CHAR) or (CharType = NVG_CJK_CHAR))) or (CharType = NVG_CJK_CHAR) then
        begin
          WordStart := Iter.Str;
          WordStartX := Iter.X;
          WordMinX := Q.X0;
        end;
        { Break to new line when a character is beyond break width }
        if ((CharType = NVG_CHAR) or (CharType = NVG_CJK_CHAR)) and (NextWidth > BreakRowWidth) then
        begin
          { The run length is too long, need to break to new line }
          if BreakEnd = RowStart then
          begin
            { The current word is longer than the row length, just break it
              from here }
            Rows[NRows].Start := RowStart;
            Rows[NRows].EndStr := Iter.Str;
            Rows[NRows].Width := RowWidth * InvScale;
            Rows[NRows].MinX := RowMinX * InvScale;
            Rows[NRows].MaxX := RowMaxX * InvScale;
            Rows[NRows].Next := Iter.Str;
            Inc(NRows);
            if NRows >= MaxRows then
              Exit(NRows);
            RowStartX := Iter.X;
            RowStart := Iter.Str;
            RowEnd := Iter.Next;
            RowWidth := Iter.NextX - RowStartX;
            RowMinX := Q.X0 - RowStartX;
            RowMaxX := Q.X1 - RowStartX;
            WordStart := Iter.Str;
            WordStartX := Iter.X;
            WordMinX := Q.X0 - RowStartX;
          end
          else
          begin
            { Break the line from the end of the last word, and start new
              line from the beginning of the new }
            Rows[NRows].Start := RowStart;
            Rows[NRows].EndStr := BreakEnd;
            Rows[NRows].Width := BreakWidth * InvScale;
            Rows[NRows].MinX := RowMinX * InvScale;
            Rows[NRows].MaxX := BreakMaxX * InvScale;
            Rows[NRows].Next := WordStart;
            Inc(NRows);
            if NRows >= MaxRows then
              Exit(NRows);
            { Update row }
            RowStartX := WordStartX;
            RowStart := WordStart;
            RowEnd := Iter.Next;
            RowWidth := Iter.NextX - RowStartX;
            RowMinX := WordMinX - RowStartX;
            RowMaxX := Q.X1 - RowStartX;
          end;
          { Set null break point }
          BreakEnd := RowStart;
          BreakWidth := 0.0;
          BreakMaxX := 0.0;
        end;
      end;
    end;
    PCodepoint := Iter.Codepoint;
    PType := CharType;
  end;
  { Break the line from the end of the last word, and start new line from the
    beginning of the new }
  if RowStart <> nil then
  begin
    Rows[NRows].Start := RowStart;
    Rows[NRows].EndStr := RowEnd;
    Rows[NRows].Width := RowWidth * InvScale;
    Rows[NRows].MinX := RowMinX * InvScale;
    Rows[NRows].MaxX := RowMaxX * InvScale;
    Rows[NRows].Next := EndStr;
    Inc(NRows);
  end;
  Result := NRows;
end;

function nvgTextBounds(Ctx: PNVGcontext; X, Y: Single; Str, EndStr: PAnsiChar; Bounds: PSingle): Single;
var
  State: PNVGstate;
  Scale, InvScale, Width: Single;
begin
  State := nvg__getState(Ctx);
  Scale := nvg__getFontScale(State) * Ctx.DevicePxRatio;
  InvScale := 1.0 / Scale;
  if State.FontId = FONS_INVALID then
    Exit(0);
  if Str = nil then
    Exit(0);
  nvg__setTextState(Ctx, State, Scale);
  Width := fonsTextBounds(Ctx.Fs, X * Scale, Y * Scale, Str, EndStr, Bounds);
  if Bounds <> nil then
  begin
    { Use line bounds for height }
    fonsLineBounds(Ctx.Fs, Y * Scale, @Bounds[1], @Bounds[3]);
    Bounds[0] := Bounds[0] * InvScale;
    Bounds[1] := Bounds[1] * InvScale;
    Bounds[2] := Bounds[2] * InvScale;
    Bounds[3] := Bounds[3] * InvScale;
  end;
  Result := Width * InvScale;
end;

function nvgTextBounds(Ctx: PNVGcontext; X, Y: Single; const S: string; Bounds: PSingle): Single;
begin
  Result := nvgTextBounds(Ctx, X, Y, PAnsiChar(S), PAnsiChar(S) + Length(S), Bounds);
end;

procedure nvgTextBoxBounds(Ctx: PNVGcontext; X, Y, BreakRowWidth: Single; Str, EndStr: PAnsiChar; Bounds: PSingle);
var
  State: PNVGstate;
  Rows: array[0..1] of TNVGtextRow;
  Scale, InvScale, LineH, RMinY, RMaxY, MinX, MinY, MaxX, MaxY, RMinX, RMaxX, DX: Single;
  NRows, I, OldAlign, HAlign, VAlign: Integer;
  Row: PNVGtextRow;
begin
  State := nvg__getState(Ctx);
  Scale := nvg__getFontScale(State) * Ctx.DevicePxRatio;
  InvScale := 1.0 / Scale;
  OldAlign := State.TextAlign;
  HAlign := State.TextAlign and (NVG_ALIGN_LEFT or NVG_ALIGN_CENTER or NVG_ALIGN_RIGHT);
  VAlign := State.TextAlign and (NVG_ALIGN_TOP or NVG_ALIGN_MIDDLE or NVG_ALIGN_BOTTOM or NVG_ALIGN_BASELINE);
  LineH := 0;
  RMinY := 0;
  RMaxY := 0;
  if State.FontId = FONS_INVALID then
  begin
    if Bounds <> nil then
    begin
      Bounds[0] := 0;
      Bounds[1] := 0;
      Bounds[2] := 0;
      Bounds[3] := 0;
    end;
    Exit;
  end;
  nvgTextMetrics(Ctx, nil, nil, @LineH);
  State.TextAlign := NVG_ALIGN_LEFT or VAlign;
  MinX := X;
  MaxX := X;
  MinY := Y;
  MaxY := Y;
  nvg__setTextState(Ctx, State, Scale);
  fonsLineBounds(Ctx.Fs, 0, @RMinY, @RMaxY);
  RMinY := RMinY * InvScale;
  RMaxY := RMaxY * InvScale;
  NRows := nvgTextBreakLines(Ctx, Str, EndStr, BreakRowWidth, @Rows[0], 2);
  while NRows <> 0 do
  begin
    for I := 0 to NRows - 1 do
    begin
      Row := @Rows[I];
      DX := 0;
      { Horizontal bounds }
      if HAlign and NVG_ALIGN_LEFT <> 0 then
        DX := 0
      else if HAlign and NVG_ALIGN_CENTER <> 0 then
        DX := BreakRowWidth * 0.5 - Row.Width * 0.5
      else if HAlign and NVG_ALIGN_RIGHT <> 0 then
        DX := BreakRowWidth - Row.Width;
      RMinX := X + Row.MinX + DX;
      RMaxX := X + Row.MaxX + DX;
      MinX := nvg__minf(MinX, RMinX);
      MaxX := nvg__maxf(MaxX, RMaxX);
      { Vertical bounds }
      MinY := nvg__minf(MinY, Y + RMinY);
      MaxY := nvg__maxf(MaxY, Y + RMaxY);
      Y := Y + LineH * State.LineHeight;
    end;
    Str := Rows[NRows - 1].Next;
    NRows := nvgTextBreakLines(Ctx, Str, EndStr, BreakRowWidth, @Rows[0], 2);
  end;
  State.TextAlign := OldAlign;
  if Bounds <> nil then
  begin
    Bounds[0] := MinX;
    Bounds[1] := MinY;
    Bounds[2] := MaxX;
    Bounds[3] := MaxY;
  end;
end;

procedure nvgTextBoxBounds(Ctx: PNVGcontext; X, Y, BreakRowWidth: Single; const S: string; Bounds: PSingle);
begin
  nvgTextBoxBounds(Ctx, X, Y, BreakRowWidth, PAnsiChar(S), PAnsiChar(S) + Length(S), Bounds);
end;

procedure nvgTextMetrics(Ctx: PNVGcontext; Ascender, Descender, LineH: PSingle);
var
  State: PNVGstate;
  Scale, InvScale: Single;
begin
  State := nvg__getState(Ctx);
  Scale := nvg__getFontScale(State) * Ctx.DevicePxRatio;
  InvScale := 1.0 / Scale;
  if State.FontId = FONS_INVALID then
    Exit;
  nvg__setTextState(Ctx, State, Scale);
  fonsVertMetrics(Ctx.Fs, Ascender, Descender, LineH);
  if Ascender <> nil then
    Ascender^ := Ascender^ * InvScale;
  if Descender <> nil then
    Descender^ := Descender^ * InvScale;
  if LineH <> nil then
    LineH^ := LineH^ * InvScale;
end;

{ OpenGL backend ported from nanovg_gl.h

  The backend follows the OpenGL version selected in render.inc. Desktop
  versions use the GL3 path with a vertex array object, OpenGL ES 3 uses the
  GLES3 path, and OpenGL ES 2 uses the GLES2 path. All paths store fragment
  uniforms in a uniform array. }

{$if defined(glesapi) and not defined(gles30)}
  {$define nvg_gles2}
{$endif}
{$ifndef glesapi}
  {$define nvg_gl3}
{$endif}

const
  { GLNVGuniformLoc }
  GLNVG_LOC_VIEWSIZE = 0;
  GLNVG_LOC_TEX = 1;
  GLNVG_LOC_FRAG = 2;
  GLNVG_MAX_LOCS = 3;

  { GLNVGshaderType }
  NSVG_SHADER_FILLGRAD = 0;
  NSVG_SHADER_FILLIMG = 1;
  NSVG_SHADER_SIMPLE = 2;
  NSVG_SHADER_IMG = 3;

  { GLNVGcallType }
  GLNVG_NONE = 0;
  GLNVG_FILL = 1;
  GLNVG_CONVEXFILL = 2;
  GLNVG_STROKE = 3;
  GLNVG_TRIANGLES = 4;

  { Note: after modifying layout or size of uniform array, don't forget to also
    update the fragment shader source }
  NANOVG_GL_UNIFORMARRAY_SIZE = 11;

type
  TGLNVGshader = record
    Prog: GLuint;
    Frag: GLuint;
    Vert: GLuint;
    Loc: array[0..GLNVG_MAX_LOCS - 1] of GLint;
  end;

  PGLNVGtexture = ^TGLNVGtexture;
  TGLNVGtexture = record
    Id: Integer;
    Tex: GLuint;
    Width, Height: Integer;
    TexType: Integer;
    Flags: Integer;
  end;

  PGLNVGblend = ^TGLNVGblend;
  TGLNVGblend = record
    SrcRGB: GLenum;
    DstRGB: GLenum;
    SrcAlpha: GLenum;
    DstAlpha: GLenum;
  end;

  PGLNVGcall = ^TGLNVGcall;
  TGLNVGcall = record
    CallType: Integer;
    Image: Integer;
    PathOffset: Integer;
    PathCount: Integer;
    TriangleOffset: Integer;
    TriangleCount: Integer;
    UniformOffset: Integer;
    BlendFunc: TGLNVGblend;
    FillRule: Integer;
  end;

  PGLNVGpath = ^TGLNVGpath;
  TGLNVGpath = record
    FillOffset: Integer;
    FillCount: Integer;
    StrokeOffset: Integer;
    StrokeCount: Integer;
  end;

  PGLNVGfragUniforms = ^TGLNVGfragUniforms;
  TGLNVGfragUniforms = record
    case Integer of
      0: (
        { Matrices are actually 3 vec4s }
        ScissorMat: array[0..11] of Single;
        PaintMat: array[0..11] of Single;
        InnerCol: TNVGcolor;
        OuterCol: TNVGcolor;
        ScissorExt: array[0..1] of Single;
        ScissorScale: array[0..1] of Single;
        Extent: array[0..1] of Single;
        Radius: Single;
        Feather: Single;
        StrokeMult: Single;
        StrokeThr: Single;
        TexType: Single;
        UniformType: Single);
      1: (
        UniformArray: array[0..NANOVG_GL_UNIFORMARRAY_SIZE - 1, 0..3] of Single);
  end;

  PGLNVGcontext = ^TGLNVGcontext;
  TGLNVGcontext = record
    Shader: TGLNVGshader;
    Textures: PGLNVGtexture;
    View: array[0..1] of Single;
    NTextures: Integer;
    CTextures: Integer;
    TextureId: Integer;
    VertBuf: GLuint;
    VertArr: GLuint;
    FragSize: Integer;
    Flags: Integer;
    { Per frame buffers }
    Calls: PGLNVGcall;
    CCalls: Integer;
    NCalls: Integer;
    Paths: PGLNVGpath;
    CPaths: Integer;
    NPaths: Integer;
    Verts: PNVGvertex;
    CVerts: Integer;
    NVerts: Integer;
    Uniforms: PByte;
    CUniforms: Integer;
    NUniforms: Integer;
    { Cached state }
    BoundTexture: GLuint;
    StencilMask: GLuint;
    StencilFunc: GLenum;
    StencilFuncRef: GLint;
    StencilFuncMask: GLuint;
    BlendFunc: TGLNVGblend;
    DummyTex: Integer;
  end;

procedure glnvg__log(const S: string);
begin
  if IsConsole then
    WriteLn(StdErr, S);
end;

function glnvg__maxi(A, B: Integer): Integer; inline;
begin
  if A > B then Result := A else Result := B;
end;

{$ifdef nvg_gles2}
function glnvg__nearestPow2(Num: Cardinal): Cardinal;
var
  N: Cardinal;
begin
  if Num > 0 then
    N := Num - 1
  else
    N := 0;
  N := N or (N shr 1);
  N := N or (N shr 2);
  N := N or (N shr 4);
  N := N or (N shr 8);
  N := N or (N shr 16);
  Inc(N);
  Result := N;
end;
{$endif}

procedure glnvg__bindTexture(GL: PGLNVGcontext; Tex: GLuint);
begin
  if GL.BoundTexture <> Tex then
  begin
    GL.BoundTexture := Tex;
    glBindTexture(GL_TEXTURE_2D, Tex);
  end;
end;

procedure glnvg__stencilMask(GL: PGLNVGcontext; Mask: GLuint);
begin
  if GL.StencilMask <> Mask then
  begin
    GL.StencilMask := Mask;
    glStencilMask(Mask);
  end;
end;

procedure glnvg__stencilFunc(GL: PGLNVGcontext; Func: GLenum; Ref: GLint; Mask: GLuint);
begin
  if (GL.StencilFunc <> Func) or (GL.StencilFuncRef <> Ref) or (GL.StencilFuncMask <> Mask) then
  begin
    GL.StencilFunc := Func;
    GL.StencilFuncRef := Ref;
    GL.StencilFuncMask := Mask;
    glStencilFunc(Func, Ref, Mask);
  end;
end;

procedure glnvg__blendFuncSeparate(GL: PGLNVGcontext; const Blend: TGLNVGblend);
begin
  if (GL.BlendFunc.SrcRGB <> Blend.SrcRGB) or (GL.BlendFunc.DstRGB <> Blend.DstRGB) or
    (GL.BlendFunc.SrcAlpha <> Blend.SrcAlpha) or (GL.BlendFunc.DstAlpha <> Blend.DstAlpha) then
  begin
    GL.BlendFunc := Blend;
    glBlendFuncSeparate(Blend.SrcRGB, Blend.DstRGB, Blend.SrcAlpha, Blend.DstAlpha);
  end;
end;

function glnvg__allocTexture(GL: PGLNVGcontext): PGLNVGtexture;
var
  Tex: PGLNVGtexture;
  I, CTextures: Integer;
begin
  Tex := nil;
  for I := 0 to GL.NTextures - 1 do
    if GL.Textures[I].Id = 0 then
    begin
      Tex := @GL.Textures[I];
      Break;
    end;
  if Tex = nil then
  begin
    if GL.NTextures + 1 > GL.CTextures then
    begin
      { 1.5x Overallocate }
      CTextures := glnvg__maxi(GL.NTextures + 1, 4) + GL.CTextures div 2;
      ReallocMem(GL.Textures, SizeOf(TGLNVGtexture) * CTextures);
      GL.CTextures := CTextures;
    end;
    Tex := @GL.Textures[GL.NTextures];
    Inc(GL.NTextures);
  end;
  FillChar(Tex^, SizeOf(Tex^), 0);
  Inc(GL.TextureId);
  Tex.Id := GL.TextureId;
  Result := Tex;
end;

function glnvg__findTexture(GL: PGLNVGcontext; Id: Integer): PGLNVGtexture;
var
  I: Integer;
begin
  for I := 0 to GL.NTextures - 1 do
    if GL.Textures[I].Id = Id then
      Exit(@GL.Textures[I]);
  Result := nil;
end;

function glnvg__deleteTexture(GL: PGLNVGcontext; Id: Integer): Integer;
var
  I: Integer;
begin
  for I := 0 to GL.NTextures - 1 do
    if GL.Textures[I].Id = Id then
    begin
      if (GL.Textures[I].Tex <> 0) and ((GL.Textures[I].Flags and NVG_IMAGE_NODELETE) = 0) then
        glDeleteTextures(1, @GL.Textures[I].Tex);
      FillChar(GL.Textures[I], SizeOf(GL.Textures[I]), 0);
      Exit(1);
    end;
  Result := 0;
end;

procedure glnvg__dumpShaderError(Shader: GLuint; const Name, ShaderType: string);
var
  Str: array[0..512] of AnsiChar;
  Len: GLsizei;
begin
  Len := 0;
  glGetShaderInfoLog(Shader, 512, @Len, @Str[0]);
  if Len > 512 then
    Len := 512;
  Str[Len] := #0;
  glnvg__log('Shader ' + Name + '/' + ShaderType + ' error:' + LineEnding + string(PAnsiChar(@Str[0])));
end;

procedure glnvg__dumpProgramError(Prog: GLuint; const Name: string);
var
  Str: array[0..512] of AnsiChar;
  Len: GLsizei;
begin
  Len := 0;
  glGetProgramInfoLog(Prog, 512, @Len, @Str[0]);
  if Len > 512 then
    Len := 512;
  Str[Len] := #0;
  glnvg__log('Program ' + Name + ' error:' + LineEnding + string(PAnsiChar(@Str[0])));
end;

procedure glnvg__checkError(GL: PGLNVGcontext; const Str: string);
var
  Err: GLenum;
begin
  if (GL.Flags and NVG_DEBUG) = 0 then
    Exit;
  Err := glGetError;
  if Err <> GL_NO_ERROR then
    glnvg__log('Error ' + IntToHex(Err, 8) + ' after ' + Str);
end;

function glnvg__createShader(out Shader: TGLNVGshader; const Name: string; Header, Opts, VShader,
  FShader: PAnsiChar): Boolean;
var
  Status: GLint;
  Prog, Vert, Frag: GLuint;
  Str: array[0..2] of PAnsiChar;
begin
  Result := False;
  Str[0] := Header;
  if Opts <> nil then
    Str[1] := Opts
  else
    Str[1] := '';
  FillChar(Shader, SizeOf(Shader), 0);
  Prog := glCreateProgram;
  Vert := glCreateShader(GL_VERTEX_SHADER);
  Frag := glCreateShader(GL_FRAGMENT_SHADER);
  Str[2] := VShader;
  glShaderSource(Vert, 3, @Str[0], nil);
  Str[2] := FShader;
  glShaderSource(Frag, 3, @Str[0], nil);
  glCompileShader(Vert);
  glGetShaderiv(Vert, GL_COMPILE_STATUS, @Status);
  if Status <> GL_TRUE then
  begin
    glnvg__dumpShaderError(Vert, Name, 'vert');
    Exit;
  end;
  glCompileShader(Frag);
  glGetShaderiv(Frag, GL_COMPILE_STATUS, @Status);
  if Status <> GL_TRUE then
  begin
    glnvg__dumpShaderError(Frag, Name, 'frag');
    Exit;
  end;
  glAttachShader(Prog, Vert);
  glAttachShader(Prog, Frag);
  glBindAttribLocation(Prog, 0, 'vertex');
  glBindAttribLocation(Prog, 1, 'tcoord');
  glLinkProgram(Prog);
  glGetProgramiv(Prog, GL_LINK_STATUS, @Status);
  if Status <> GL_TRUE then
  begin
    glnvg__dumpProgramError(Prog, Name);
    Exit;
  end;
  Shader.Prog := Prog;
  Shader.Vert := Vert;
  Shader.Frag := Frag;
  Result := True;
end;

procedure glnvg__deleteShader(var Shader: TGLNVGshader);
begin
  if Shader.Prog <> 0 then
    glDeleteProgram(Shader.Prog);
  if Shader.Vert <> 0 then
    glDeleteShader(Shader.Vert);
  if Shader.Frag <> 0 then
    glDeleteShader(Shader.Frag);
end;

procedure glnvg__getUniforms(var Shader: TGLNVGshader);
begin
  Shader.Loc[GLNVG_LOC_VIEWSIZE] := glGetUniformLocation(Shader.Prog, 'viewSize');
  Shader.Loc[GLNVG_LOC_TEX] := glGetUniformLocation(Shader.Prog, 'tex');
  Shader.Loc[GLNVG_LOC_FRAG] := glGetUniformLocation(Shader.Prog, 'frag');
end;

function glnvg__renderCreateTexture(UPtr: Pointer; TexType, W, H, ImageFlags: Integer; Data: PByte): Integer; forward;

const
  ShaderHeader: PAnsiChar =
{$if defined(nvg_gles2)}
    '#version 100'#10 +
    '#define NANOVG_GL2 1'#10 +
{$elseif defined(glesapi)}
    '#version 300 es'#10 +
    '#define NANOVG_GL3 1'#10 +
{$elseif defined(gl32)}
    '#version 150 core'#10 +
    '#define NANOVG_GL3 1'#10 +
{$elseif defined(gl31)}
    '#version 140'#10 +
    '#define NANOVG_GL3 1'#10 +
{$else}
    '#version 130'#10 +
    '#define NANOVG_GL3 1'#10 +
{$endif}
    '#define UNIFORMARRAY_SIZE 11'#10 +
    #10;

  FillVertShader: PAnsiChar =
    '#ifdef NANOVG_GL3'#10 +
    '	uniform vec2 viewSize;'#10 +
    '	in vec2 vertex;'#10 +
    '	in vec2 tcoord;'#10 +
    '	out vec2 ftcoord;'#10 +
    '	out vec2 fpos;'#10 +
    '#else'#10 +
    '	uniform vec2 viewSize;'#10 +
    '	attribute vec2 vertex;'#10 +
    '	attribute vec2 tcoord;'#10 +
    '	varying vec2 ftcoord;'#10 +
    '	varying vec2 fpos;'#10 +
    '#endif'#10 +
    'void main(void) {'#10 +
    '	ftcoord = tcoord;'#10 +
    '	fpos = vertex;'#10 +
    '	gl_Position = vec4(2.0*vertex.x/viewSize.x - 1.0, 1.0 - 2.0*vertex.y/viewSize.y, 0, 1);'#10 +
    '}'#10;

  FillFragShader: PAnsiChar =
    '#ifdef GL_ES'#10 +
    '#if defined(GL_FRAGMENT_PRECISION_HIGH) || defined(NANOVG_GL3)'#10 +
    ' precision highp float;'#10 +
    '#else'#10 +
    ' precision mediump float;'#10 +
    '#endif'#10 +
    '#endif'#10 +
    '#ifdef NANOVG_GL3'#10 +
    '	uniform vec4 frag[UNIFORMARRAY_SIZE];'#10 +
    '	uniform sampler2D tex;'#10 +
    '	in vec2 ftcoord;'#10 +
    '	in vec2 fpos;'#10 +
    '	out vec4 outColor;'#10 +
    '#else'#10 +
    '	uniform vec4 frag[UNIFORMARRAY_SIZE];'#10 +
    '	uniform sampler2D tex;'#10 +
    '	varying vec2 ftcoord;'#10 +
    '	varying vec2 fpos;'#10 +
    '#endif'#10 +
    '	#define scissorMat mat3(frag[0].xyz, frag[1].xyz, frag[2].xyz)'#10 +
    '	#define paintMat mat3(frag[3].xyz, frag[4].xyz, frag[5].xyz)'#10 +
    '	#define innerCol frag[6]'#10 +
    '	#define outerCol frag[7]'#10 +
    '	#define scissorExt frag[8].xy'#10 +
    '	#define scissorScale frag[8].zw'#10 +
    '	#define extent frag[9].xy'#10 +
    '	#define radius frag[9].z'#10 +
    '	#define feather frag[9].w'#10 +
    '	#define strokeMult frag[10].x'#10 +
    '	#define strokeThr frag[10].y'#10 +
    '	#define texType int(frag[10].z)'#10 +
    '	#define type int(frag[10].w)'#10 +
    #10 +
    'float sdroundrect(vec2 pt, vec2 ext, float rad) {'#10 +
    '	vec2 ext2 = ext - vec2(rad,rad);'#10 +
    '	vec2 d = abs(pt) - ext2;'#10 +
    '	return min(max(d.x,d.y),0.0) + length(max(d,0.0)) - rad;'#10 +
    '}'#10 +
    #10 +
    '// Scissoring'#10 +
    'float scissorMask(vec2 p) {'#10 +
    '	vec2 sc = (abs((scissorMat * vec3(p,1.0)).xy) - scissorExt);'#10 +
    '	sc = vec2(0.5,0.5) - sc * scissorScale;'#10 +
    '	return clamp(sc.x,0.0,1.0) * clamp(sc.y,0.0,1.0);'#10 +
    '}'#10 +
    '#ifdef EDGE_AA'#10 +
    '// Stroke - from [0..1] to clipped pyramid, where the slope is 1px.'#10 +
    'float strokeMask() {'#10 +
    '	return min(1.0, (1.0-abs(ftcoord.x*2.0-1.0))*strokeMult) * min(1.0, ftcoord.y);'#10 +
    '}'#10 +
    '#endif'#10 +
    #10 +
    'void main(void) {'#10 +
    '   vec4 result;'#10 +
    '	float scissor = scissorMask(fpos);'#10 +
    '#ifdef EDGE_AA'#10 +
    '	float strokeAlpha = strokeMask();'#10 +
    '	if (strokeAlpha < strokeThr) discard;'#10 +
    '#else'#10 +
    '	float strokeAlpha = 1.0;'#10 +
    '#endif'#10 +
    '	if (type == 0) {			// Gradient'#10 +
    '		// Calculate gradient color using box gradient'#10 +
    '		vec2 pt = (paintMat * vec3(fpos,1.0)).xy;'#10 +
    '		float d = clamp((sdroundrect(pt, extent, radius) + feather*0.5) / feather, 0.0, 1.0);'#10 +
    '		vec4 color = mix(innerCol,outerCol,d);'#10 +
    '		// Combine alpha'#10 +
    '		color *= strokeAlpha * scissor;'#10 +
    '		result = color;'#10 +
    '	} else if (type == 1) {		// Image'#10 +
    '		// Calculate color fron texture'#10 +
    '		vec2 pt = (paintMat * vec3(fpos,1.0)).xy / extent;'#10 +
    '#ifdef NANOVG_GL3'#10 +
    '		vec4 color = texture(tex, pt);'#10 +
    '#else'#10 +
    '		vec4 color = texture2D(tex, pt);'#10 +
    '#endif'#10 +
    '		if (texType == 1) color = vec4(color.xyz*color.w,color.w);' +
    '		if (texType == 2) color = vec4(color.x);' +
    '		// Apply color tint and alpha.'#10 +
    '		color *= innerCol;'#10 +
    '		// Combine alpha'#10 +
    '		color *= strokeAlpha * scissor;'#10 +
    '		result = color;'#10 +
    '	} else if (type == 2) {		// Stencil fill'#10 +
    '		result = vec4(1,1,1,1);'#10 +
    '	} else if (type == 3) {		// Textured tris'#10 +
    '#ifdef NANOVG_GL3'#10 +
    '		vec4 color = texture(tex, ftcoord);'#10 +
    '#else'#10 +
    '		vec4 color = texture2D(tex, ftcoord);'#10 +
    '#endif'#10 +
    '		if (texType == 1) color = vec4(color.xyz*color.w,color.w);' +
    '		if (texType == 2) color = vec4(color.x);' +
    '		color *= scissor;'#10 +
    '		result = color * innerCol;'#10 +
    '	}'#10 +
    '#ifdef NANOVG_GL3'#10 +
    '	outColor = result;'#10 +
    '#else'#10 +
    '	gl_FragColor = result;'#10 +
    '#endif'#10 +
    '}'#10;

function glnvg__renderCreate(UPtr: Pointer): Integer;
var
  GL: PGLNVGcontext;
  Align: Integer;
begin
  GL := UPtr;
  Align := 4;
  glnvg__checkError(GL, 'init');
  if GL.Flags and NVG_ANTIALIAS <> 0 then
  begin
    if not glnvg__createShader(GL.Shader, 'shader', ShaderHeader, '#define EDGE_AA 1'#10, FillVertShader, FillFragShader) then
      Exit(0);
  end
  else if not glnvg__createShader(GL.Shader, 'shader', ShaderHeader, nil, FillVertShader, FillFragShader) then
    Exit(0);
  glnvg__checkError(GL, 'uniform locations');
  glnvg__getUniforms(GL.Shader);
  { Create dynamic vertex array }
  {$ifdef nvg_gl3}
  glGenVertexArrays(1, @GL.VertArr);
  {$endif}
  glGenBuffers(1, @GL.VertBuf);
  GL.FragSize := SizeOf(TGLNVGfragUniforms) + Align - SizeOf(TGLNVGfragUniforms) mod Align;
  { Some platforms does not allow to have samples to unset textures. Create
    empty one which is bound when there's no texture specified }
  GL.DummyTex := glnvg__renderCreateTexture(GL, NVG_TEXTURE_ALPHA, 1, 1, 0, nil);
  glnvg__checkError(GL, 'create done');
  glFinish;
  Result := 1;
end;

function glnvg__renderCreateTexture(UPtr: Pointer; TexType, W, H, ImageFlags: Integer; Data: PByte): Integer;
var
  GL: PGLNVGcontext;
  Tex: PGLNVGtexture;
begin
  GL := UPtr;
  Tex := glnvg__allocTexture(GL);
  if Tex = nil then
    Exit(0);
  {$ifdef nvg_gles2}
  { Check for non-power of 2 }
  if (glnvg__nearestPow2(W) <> Cardinal(W)) or (glnvg__nearestPow2(H) <> Cardinal(H)) then
  begin
    { No repeat }
    if ((ImageFlags and NVG_IMAGE_REPEATX) <> 0) or ((ImageFlags and NVG_IMAGE_REPEATY) <> 0) then
    begin
      glnvg__log(Format('Repeat X/Y is not supported for non power-of-two textures (%d x %d)', [W, H]));
      ImageFlags := ImageFlags and not (NVG_IMAGE_REPEATX or NVG_IMAGE_REPEATY);
    end;
    { No mips }
    if (ImageFlags and NVG_IMAGE_GENERATE_MIPMAPS) <> 0 then
    begin
      glnvg__log(Format('Mip-maps is not support for non power-of-two textures (%d x %d)', [W, H]));
      ImageFlags := ImageFlags and not NVG_IMAGE_GENERATE_MIPMAPS;
    end;
  end;
  {$endif}
  glGenTextures(1, @Tex.Tex);
  Tex.Width := W;
  Tex.Height := H;
  Tex.TexType := TexType;
  Tex.Flags := ImageFlags;
  glnvg__bindTexture(GL, Tex.Tex);
  glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
  {$ifndef nvg_gles2}
  glPixelStorei(GL_UNPACK_ROW_LENGTH, Tex.Width);
  glPixelStorei(GL_UNPACK_SKIP_PIXELS, 0);
  glPixelStorei(GL_UNPACK_SKIP_ROWS, 0);
  {$endif}
  if TexType = NVG_TEXTURE_RGBA then
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, W, H, 0, GL_RGBA, GL_UNSIGNED_BYTE, Data)
  else
    {$if defined(nvg_gles2)}
    glTexImage2D(GL_TEXTURE_2D, 0, GL_LUMINANCE, W, H, 0, GL_LUMINANCE, GL_UNSIGNED_BYTE, Data);
    {$elseif defined(glesapi)}
    glTexImage2D(GL_TEXTURE_2D, 0, GL_R8, W, H, 0, GL_RED, GL_UNSIGNED_BYTE, Data);
    {$else}
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RED, W, H, 0, GL_RED, GL_UNSIGNED_BYTE, Data);
    {$endif}
  if ImageFlags and NVG_IMAGE_GENERATE_MIPMAPS <> 0 then
  begin
    if ImageFlags and NVG_IMAGE_NEAREST <> 0 then
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST_MIPMAP_NEAREST)
    else
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
  end
  else
  begin
    if ImageFlags and NVG_IMAGE_NEAREST <> 0 then
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST)
    else
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  end;
  if ImageFlags and NVG_IMAGE_NEAREST <> 0 then
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST)
  else
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  if ImageFlags and NVG_IMAGE_REPEATX <> 0 then
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT)
  else
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  if ImageFlags and NVG_IMAGE_REPEATY <> 0 then
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT)
  else
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  glPixelStorei(GL_UNPACK_ALIGNMENT, 4);
  {$ifndef nvg_gles2}
  glPixelStorei(GL_UNPACK_ROW_LENGTH, 0);
  glPixelStorei(GL_UNPACK_SKIP_PIXELS, 0);
  glPixelStorei(GL_UNPACK_SKIP_ROWS, 0);
  {$endif}
  { The new way to build mipmaps on GLES and GL3 }
  if ImageFlags and NVG_IMAGE_GENERATE_MIPMAPS <> 0 then
    glGenerateMipmap(GL_TEXTURE_2D);
  glnvg__checkError(GL, 'create tex');
  glnvg__bindTexture(GL, 0);
  Result := Tex.Id;
end;

function glnvg__renderDeleteTexture(UPtr: Pointer; Image: Integer): Integer;
begin
  Result := glnvg__deleteTexture(PGLNVGcontext(UPtr), Image);
end;

function glnvg__renderUpdateTexture(UPtr: Pointer; Image, X, Y, W, H: Integer; Data: PByte): Integer;
var
  GL: PGLNVGcontext;
  Tex: PGLNVGtexture;
begin
  GL := UPtr;
  Tex := glnvg__findTexture(GL, Image);
  if Tex = nil then
    Exit(0);
  glnvg__bindTexture(GL, Tex.Tex);
  glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
  {$ifndef nvg_gles2}
  glPixelStorei(GL_UNPACK_ROW_LENGTH, Tex.Width);
  glPixelStorei(GL_UNPACK_SKIP_PIXELS, X);
  glPixelStorei(GL_UNPACK_SKIP_ROWS, Y);
  {$else}
  { No support for all of skip, need to update a whole row at a time }
  if Tex.TexType = NVG_TEXTURE_RGBA then
    Inc(Data, Y * Tex.Width * 4)
  else
    Inc(Data, Y * Tex.Width);
  X := 0;
  W := Tex.Width;
  {$endif}
  if Tex.TexType = NVG_TEXTURE_RGBA then
    glTexSubImage2D(GL_TEXTURE_2D, 0, X, Y, W, H, GL_RGBA, GL_UNSIGNED_BYTE, Data)
  else
    {$ifdef nvg_gles2}
    glTexSubImage2D(GL_TEXTURE_2D, 0, X, Y, W, H, GL_LUMINANCE, GL_UNSIGNED_BYTE, Data);
    {$else}
    glTexSubImage2D(GL_TEXTURE_2D, 0, X, Y, W, H, GL_RED, GL_UNSIGNED_BYTE, Data);
    {$endif}
  glPixelStorei(GL_UNPACK_ALIGNMENT, 4);
  {$ifndef nvg_gles2}
  glPixelStorei(GL_UNPACK_ROW_LENGTH, 0);
  glPixelStorei(GL_UNPACK_SKIP_PIXELS, 0);
  glPixelStorei(GL_UNPACK_SKIP_ROWS, 0);
  {$endif}
  glnvg__bindTexture(GL, 0);
  Result := 1;
end;

function glnvg__renderGetTextureSize(UPtr: Pointer; Image: Integer; W, H: PInteger): Integer;
var
  Tex: PGLNVGtexture;
begin
  Tex := glnvg__findTexture(PGLNVGcontext(UPtr), Image);
  if Tex = nil then
    Exit(0);
  W^ := Tex.Width;
  H^ := Tex.Height;
  Result := 1;
end;

procedure glnvg__xformToMat3x4(M3: PSingle; const T: TNVGxform);
begin
  M3[0] := T[0];
  M3[1] := T[1];
  M3[2] := 0.0;
  M3[3] := 0.0;
  M3[4] := T[2];
  M3[5] := T[3];
  M3[6] := 0.0;
  M3[7] := 0.0;
  M3[8] := T[4];
  M3[9] := T[5];
  M3[10] := 1.0;
  M3[11] := 0.0;
end;

function glnvg__premulColor(C: TNVGcolor): TNVGcolor;
begin
  C.R := C.R * C.A;
  C.G := C.G * C.A;
  C.B := C.B * C.A;
  Result := C;
end;

function glnvg__convertPaint(GL: PGLNVGcontext; Frag: PGLNVGfragUniforms; Paint: PNVGpaint;
  Scissor: PNVGscissor; Width, Fringe, StrokeThr: Single): Boolean;
var
  Tex: PGLNVGtexture;
  InvXForm, M1, M2: TNVGxform;
begin
  FillChar(Frag^, SizeOf(Frag^), 0);
  Frag.InnerCol := glnvg__premulColor(Paint.InnerColor);
  Frag.OuterCol := glnvg__premulColor(Paint.OuterColor);
  if (Scissor.Extent[0] < -0.5) or (Scissor.Extent[1] < -0.5) then
  begin
    FillChar(Frag.ScissorMat, SizeOf(Frag.ScissorMat), 0);
    Frag.ScissorExt[0] := 1.0;
    Frag.ScissorExt[1] := 1.0;
    Frag.ScissorScale[0] := 1.0;
    Frag.ScissorScale[1] := 1.0;
  end
  else
  begin
    nvgTransformInverse(InvXForm, Scissor.XForm);
    glnvg__xformToMat3x4(@Frag.ScissorMat[0], InvXForm);
    Frag.ScissorExt[0] := Scissor.Extent[0];
    Frag.ScissorExt[1] := Scissor.Extent[1];
    Frag.ScissorScale[0] := Sqrt(Scissor.XForm[0] * Scissor.XForm[0] + Scissor.XForm[2] * Scissor.XForm[2]) / Fringe;
    Frag.ScissorScale[1] := Sqrt(Scissor.XForm[1] * Scissor.XForm[1] + Scissor.XForm[3] * Scissor.XForm[3]) / Fringe;
  end;
  Frag.Extent[0] := Paint.Extent[0];
  Frag.Extent[1] := Paint.Extent[1];
  Frag.StrokeMult := (Width * 0.5 + Fringe * 0.5) / Fringe;
  Frag.StrokeThr := StrokeThr;
  if Paint.Image <> 0 then
  begin
    Tex := glnvg__findTexture(GL, Paint.Image);
    if Tex = nil then
      Exit(False);
    if (Tex.Flags and NVG_IMAGE_FLIPY) <> 0 then
    begin
      nvgTransformTranslate(M1, 0.0, Frag.Extent[1] * 0.5);
      nvgTransformMultiply(M1, Paint.XForm);
      nvgTransformScale(M2, 1.0, -1.0);
      nvgTransformMultiply(M2, M1);
      nvgTransformTranslate(M1, 0.0, -Frag.Extent[1] * 0.5);
      nvgTransformMultiply(M1, M2);
      nvgTransformInverse(InvXForm, M1);
    end
    else
      nvgTransformInverse(InvXForm, Paint.XForm);
    Frag.UniformType := NSVG_SHADER_FILLIMG;
    if Tex.TexType = NVG_TEXTURE_RGBA then
    begin
      if Tex.Flags and NVG_IMAGE_PREMULTIPLIED <> 0 then
        Frag.TexType := 0.0
      else
        Frag.TexType := 1.0;
    end
    else
      Frag.TexType := 2.0;
  end
  else
  begin
    Frag.UniformType := NSVG_SHADER_FILLGRAD;
    Frag.Radius := Paint.Radius;
    Frag.Feather := Paint.Feather;
    nvgTransformInverse(InvXForm, Paint.XForm);
  end;
  glnvg__xformToMat3x4(@Frag.PaintMat[0], InvXForm);
  Result := True;
end;

function nvg__fragUniformPtr(GL: PGLNVGcontext; I: Integer): PGLNVGfragUniforms; inline;
begin
  Result := PGLNVGfragUniforms(@GL.Uniforms[I]);
end;

procedure glnvg__setUniforms(GL: PGLNVGcontext; UniformOffset, Image: Integer);
var
  Tex: PGLNVGtexture;
  Frag: PGLNVGfragUniforms;
begin
  Tex := nil;
  Frag := nvg__fragUniformPtr(GL, UniformOffset);
  glUniform4fv(GL.Shader.Loc[GLNVG_LOC_FRAG], NANOVG_GL_UNIFORMARRAY_SIZE, @Frag.UniformArray[0, 0]);
  if Image <> 0 then
    Tex := glnvg__findTexture(GL, Image);
  { If no image is set, use empty texture }
  if Tex = nil then
    Tex := glnvg__findTexture(GL, GL.DummyTex);
  if Tex <> nil then
    glnvg__bindTexture(GL, Tex.Tex)
  else
    glnvg__bindTexture(GL, 0);
  glnvg__checkError(GL, 'tex paint tex');
end;

procedure glnvg__renderViewport(UPtr: Pointer; Width, Height, DevicePixelRatio: Single);
var
  GL: PGLNVGcontext;
begin
  GL := UPtr;
  GL.View[0] := Width;
  GL.View[1] := Height;
end;

procedure glnvg__fill(GL: PGLNVGcontext; Call: PGLNVGcall);
var
  Paths: PGLNVGpath;
  I, NPaths: Integer;
begin
  Paths := @GL.Paths[Call.PathOffset];
  NPaths := Call.PathCount;
  { Draw shapes }
  glEnable(GL_STENCIL_TEST);
  glnvg__stencilMask(GL, $FF);
  glnvg__stencilFunc(GL, GL_ALWAYS, 0, $FF);
  glColorMask(GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE);
  { Set bindpoint for solid loc }
  glnvg__setUniforms(GL, Call.UniformOffset, 0);
  glnvg__checkError(GL, 'fill simple');
  if Call.FillRule = NVG_FILL_EVENODD then
  begin
    { Even-odd flips the lowest stencil bit for every covering triangle, so
      it ends up set where an odd number of sub-paths overlap }
    glnvg__stencilMask(GL, $01);
    glStencilOp(GL_KEEP, GL_KEEP, GL_INVERT);
  end
  else
  begin
    { Nonzero counts up for solid triangles and down for holes }
    glStencilOpSeparate(GL_FRONT, GL_KEEP, GL_KEEP, GL_INCR_WRAP);
    glStencilOpSeparate(GL_BACK, GL_KEEP, GL_KEEP, GL_DECR_WRAP);
  end;
  glDisable(GL_CULL_FACE);
  for I := 0 to NPaths - 1 do
    glDrawArrays(GL_TRIANGLE_FAN, Paths[I].FillOffset, Paths[I].FillCount);
  glEnable(GL_CULL_FACE);
  glnvg__stencilMask(GL, $FF);
  { Draw anti-aliased pixels }
  glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);
  glnvg__setUniforms(GL, Call.UniformOffset + GL.FragSize, Call.Image);
  glnvg__checkError(GL, 'fill fill');
  if GL.Flags and NVG_ANTIALIAS <> 0 then
  begin
    glnvg__stencilFunc(GL, GL_EQUAL, $00, $FF);
    glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
    { Draw fringes }
    for I := 0 to NPaths - 1 do
      glDrawArrays(GL_TRIANGLE_STRIP, Paths[I].StrokeOffset, Paths[I].StrokeCount);
  end;
  { Draw fill }
  glnvg__stencilFunc(GL, GL_NOTEQUAL, $0, $FF);
  glStencilOp(GL_ZERO, GL_ZERO, GL_ZERO);
  glDrawArrays(GL_TRIANGLE_STRIP, Call.TriangleOffset, Call.TriangleCount);
  glDisable(GL_STENCIL_TEST);
end;

procedure glnvg__convexFill(GL: PGLNVGcontext; Call: PGLNVGcall);
var
  Paths: PGLNVGpath;
  I, NPaths: Integer;
begin
  Paths := @GL.Paths[Call.PathOffset];
  NPaths := Call.PathCount;
  glnvg__setUniforms(GL, Call.UniformOffset, Call.Image);
  glnvg__checkError(GL, 'convex fill');
  for I := 0 to NPaths - 1 do
  begin
    glDrawArrays(GL_TRIANGLE_FAN, Paths[I].FillOffset, Paths[I].FillCount);
    { Draw fringes }
    if Paths[I].StrokeCount > 0 then
      glDrawArrays(GL_TRIANGLE_STRIP, Paths[I].StrokeOffset, Paths[I].StrokeCount);
  end;
end;

procedure glnvg__stroke(GL: PGLNVGcontext; Call: PGLNVGcall);
var
  Paths: PGLNVGpath;
  I, NPaths: Integer;
begin
  Paths := @GL.Paths[Call.PathOffset];
  NPaths := Call.PathCount;
  if GL.Flags and NVG_STENCIL_STROKES <> 0 then
  begin
    glEnable(GL_STENCIL_TEST);
    glnvg__stencilMask(GL, $FF);
    { Fill the stroke base without overlap }
    glnvg__stencilFunc(GL, GL_EQUAL, $0, $FF);
    glStencilOp(GL_KEEP, GL_KEEP, GL_INCR);
    glnvg__setUniforms(GL, Call.UniformOffset + GL.FragSize, Call.Image);
    glnvg__checkError(GL, 'stroke fill 0');
    for I := 0 to NPaths - 1 do
      glDrawArrays(GL_TRIANGLE_STRIP, Paths[I].StrokeOffset, Paths[I].StrokeCount);
    { Draw anti-aliased pixels }
    glnvg__setUniforms(GL, Call.UniformOffset, Call.Image);
    glnvg__stencilFunc(GL, GL_EQUAL, $00, $FF);
    glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
    for I := 0 to NPaths - 1 do
      glDrawArrays(GL_TRIANGLE_STRIP, Paths[I].StrokeOffset, Paths[I].StrokeCount);
    { Clear stencil buffer }
    glColorMask(GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE);
    glnvg__stencilFunc(GL, GL_ALWAYS, $0, $FF);
    glStencilOp(GL_ZERO, GL_ZERO, GL_ZERO);
    glnvg__checkError(GL, 'stroke fill 1');
    for I := 0 to NPaths - 1 do
      glDrawArrays(GL_TRIANGLE_STRIP, Paths[I].StrokeOffset, Paths[I].StrokeCount);
    glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);
    glDisable(GL_STENCIL_TEST);
  end
  else
  begin
    glnvg__setUniforms(GL, Call.UniformOffset, Call.Image);
    glnvg__checkError(GL, 'stroke fill');
    { Draw Strokes }
    for I := 0 to NPaths - 1 do
      glDrawArrays(GL_TRIANGLE_STRIP, Paths[I].StrokeOffset, Paths[I].StrokeCount);
  end;
end;

procedure glnvg__triangles(GL: PGLNVGcontext; Call: PGLNVGcall);
begin
  glnvg__setUniforms(GL, Call.UniformOffset, Call.Image);
  glnvg__checkError(GL, 'triangles fill');
  glDrawArrays(GL_TRIANGLES, Call.TriangleOffset, Call.TriangleCount);
end;

procedure glnvg__renderCancel(UPtr: Pointer);
var
  GL: PGLNVGcontext;
begin
  GL := UPtr;
  GL.NVerts := 0;
  GL.NPaths := 0;
  GL.NCalls := 0;
  GL.NUniforms := 0;
end;

function glnvg_convertBlendFuncFactor(Factor: Integer): GLenum;
begin
  case Factor of
    NVG_ZERO: Result := GL_ZERO;
    NVG_ONE: Result := GL_ONE;
    NVG_SRC_COLOR: Result := GL_SRC_COLOR;
    NVG_ONE_MINUS_SRC_COLOR: Result := GL_ONE_MINUS_SRC_COLOR;
    NVG_DST_COLOR: Result := GL_DST_COLOR;
    NVG_ONE_MINUS_DST_COLOR: Result := GL_ONE_MINUS_DST_COLOR;
    NVG_SRC_ALPHA: Result := GL_SRC_ALPHA;
    NVG_ONE_MINUS_SRC_ALPHA: Result := GL_ONE_MINUS_SRC_ALPHA;
    NVG_DST_ALPHA: Result := GL_DST_ALPHA;
    NVG_ONE_MINUS_DST_ALPHA: Result := GL_ONE_MINUS_DST_ALPHA;
    NVG_SRC_ALPHA_SATURATE: Result := GL_SRC_ALPHA_SATURATE;
  else
    Result := GL_INVALID_ENUM;
  end;
end;

function glnvg__blendCompositeOperation(Op: TNVGcompositeOperationState): TGLNVGblend;
begin
  Result.SrcRGB := glnvg_convertBlendFuncFactor(Op.SrcRGB);
  Result.DstRGB := glnvg_convertBlendFuncFactor(Op.DstRGB);
  Result.SrcAlpha := glnvg_convertBlendFuncFactor(Op.SrcAlpha);
  Result.DstAlpha := glnvg_convertBlendFuncFactor(Op.DstAlpha);
  if (Result.SrcRGB = GL_INVALID_ENUM) or (Result.DstRGB = GL_INVALID_ENUM) or
    (Result.SrcAlpha = GL_INVALID_ENUM) or (Result.DstAlpha = GL_INVALID_ENUM) then
  begin
    Result.SrcRGB := GL_ONE;
    Result.DstRGB := GL_ONE_MINUS_SRC_ALPHA;
    Result.SrcAlpha := GL_ONE;
    Result.DstAlpha := GL_ONE_MINUS_SRC_ALPHA;
  end;
end;

procedure glnvg__renderFlush(UPtr: Pointer);
var
  GL: PGLNVGcontext;
  Call: PGLNVGcall;
  I: Integer;
  {$ifdef nvg_gl3}
  PriorVertArr: GLint;
  {$endif}
begin
  GL := UPtr;
  if GL.NCalls > 0 then
  begin
    { Setup require GL state }
    glUseProgram(GL.Shader.Prog);
    glEnable(GL_CULL_FACE);
    glCullFace(GL_BACK);
    glFrontFace(GL_CCW);
    glEnable(GL_BLEND);
    glDisable(GL_DEPTH_TEST);
    glDisable(GL_SCISSOR_TEST);
    glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);
    glStencilMask($FFFFFFFF);
    glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
    glStencilFunc(GL_ALWAYS, 0, $FFFFFFFF);
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, 0);
    GL.BoundTexture := 0;
    GL.StencilMask := $FFFFFFFF;
    GL.StencilFunc := GL_ALWAYS;
    GL.StencilFuncRef := 0;
    GL.StencilFuncMask := $FFFFFFFF;
    GL.BlendFunc.SrcRGB := GL_INVALID_ENUM;
    GL.BlendFunc.SrcAlpha := GL_INVALID_ENUM;
    GL.BlendFunc.DstRGB := GL_INVALID_ENUM;
    GL.BlendFunc.DstAlpha := GL_INVALID_ENUM;
    { Upload vertex data }
    {$ifdef nvg_gl3}
    { Remember the vertex array of the caller, which core profiles require to
      draw, so it can be restored after rendering }
    PriorVertArr := 0;
    glGetIntegerv(GL_VERTEX_ARRAY_BINDING, @PriorVertArr);
    glBindVertexArray(GL.VertArr);
    {$endif}
    glBindBuffer(GL_ARRAY_BUFFER, GL.VertBuf);
    glBufferData(GL_ARRAY_BUFFER, GL.NVerts * SizeOf(TNVGvertex), GL.Verts, GL_STREAM_DRAW);
    glEnableVertexAttribArray(0);
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, SizeOf(TNVGvertex), Pointer(0));
    glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, SizeOf(TNVGvertex), Pointer(2 * SizeOf(Single)));
    { Set view and texture just once per frame }
    glUniform1i(GL.Shader.Loc[GLNVG_LOC_TEX], 0);
    glUniform2fv(GL.Shader.Loc[GLNVG_LOC_VIEWSIZE], 1, @GL.View[0]);
    for I := 0 to GL.NCalls - 1 do
    begin
      Call := @GL.Calls[I];
      glnvg__blendFuncSeparate(GL, Call.BlendFunc);
      case Call.CallType of
        GLNVG_FILL: glnvg__fill(GL, Call);
        GLNVG_CONVEXFILL: glnvg__convexFill(GL, Call);
        GLNVG_STROKE: glnvg__stroke(GL, Call);
        GLNVG_TRIANGLES: glnvg__triangles(GL, Call);
      end;
    end;
    glDisableVertexAttribArray(0);
    glDisableVertexAttribArray(1);
    {$ifdef nvg_gl3}
    glBindVertexArray(PriorVertArr);
    {$endif}
    glDisable(GL_CULL_FACE);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glUseProgram(0);
    glnvg__bindTexture(GL, 0);
  end;
  { Reset calls }
  GL.NVerts := 0;
  GL.NPaths := 0;
  GL.NCalls := 0;
  GL.NUniforms := 0;
end;

function glnvg__maxVertCount(Paths: PNVGpath; NPaths: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to NPaths - 1 do
  begin
    Inc(Result, Paths[I].NFill);
    Inc(Result, Paths[I].NStroke);
  end;
end;

function glnvg__allocCall(GL: PGLNVGcontext): PGLNVGcall;
var
  CCalls: Integer;
begin
  if GL.NCalls + 1 > GL.CCalls then
  begin
    { 1.5x Overallocate }
    CCalls := glnvg__maxi(GL.NCalls + 1, 128) + GL.CCalls div 2;
    ReallocMem(GL.Calls, SizeOf(TGLNVGcall) * CCalls);
    GL.CCalls := CCalls;
  end;
  Result := @GL.Calls[GL.NCalls];
  Inc(GL.NCalls);
  FillChar(Result^, SizeOf(TGLNVGcall), 0);
end;

function glnvg__allocPaths(GL: PGLNVGcontext; N: Integer): Integer;
var
  CPaths: Integer;
begin
  if GL.NPaths + N > GL.CPaths then
  begin
    { 1.5x Overallocate }
    CPaths := glnvg__maxi(GL.NPaths + N, 128) + GL.CPaths div 2;
    ReallocMem(GL.Paths, SizeOf(TGLNVGpath) * CPaths);
    GL.CPaths := CPaths;
  end;
  Result := GL.NPaths;
  Inc(GL.NPaths, N);
end;

function glnvg__allocVerts(GL: PGLNVGcontext; N: Integer): Integer;
var
  CVerts: Integer;
begin
  if GL.NVerts + N > GL.CVerts then
  begin
    { 1.5x Overallocate }
    CVerts := glnvg__maxi(GL.NVerts + N, 4096) + GL.CVerts div 2;
    ReallocMem(GL.Verts, SizeOf(TNVGvertex) * CVerts);
    GL.CVerts := CVerts;
  end;
  Result := GL.NVerts;
  Inc(GL.NVerts, N);
end;

function glnvg__allocFragUniforms(GL: PGLNVGcontext; N: Integer): Integer;
var
  StructSize, CUniforms: Integer;
begin
  StructSize := GL.FragSize;
  if GL.NUniforms + N > GL.CUniforms then
  begin
    { 1.5x Overallocate }
    CUniforms := glnvg__maxi(GL.NUniforms + N, 128) + GL.CUniforms div 2;
    ReallocMem(GL.Uniforms, StructSize * CUniforms);
    GL.CUniforms := CUniforms;
  end;
  Result := GL.NUniforms * StructSize;
  Inc(GL.NUniforms, N);
end;

procedure glnvg__vset(Vtx: PNVGvertex; X, Y, U, V: Single); inline;
begin
  Vtx.X := X;
  Vtx.Y := Y;
  Vtx.U := U;
  Vtx.V := V;
end;

procedure glnvg__renderFill(UPtr: Pointer; Paint: PNVGpaint; CompositeOperation: TNVGcompositeOperationState;
  Scissor: PNVGscissor; Fringe: Single; Bounds: PSingle; Paths: PNVGpath; NPaths: Integer; FillRule: Integer);
var
  GL: PGLNVGcontext;
  Call: PGLNVGcall;
  Quad: PNVGvertex;
  Frag: PGLNVGfragUniforms;
  I, MaxVerts, Offset: Integer;
  Copy: PGLNVGpath;
  Path: PNVGpath;
begin
  GL := UPtr;
  Call := glnvg__allocCall(GL);
  if Call = nil then
    Exit;
  Call.CallType := GLNVG_FILL;
  Call.FillRule := FillRule;
  Call.TriangleCount := 4;
  Call.PathOffset := glnvg__allocPaths(GL, NPaths);
  Call.PathCount := NPaths;
  Call.Image := Paint.Image;
  Call.BlendFunc := glnvg__blendCompositeOperation(CompositeOperation);
  if (NPaths = 1) and (Paths[0].Convex <> 0) then
  begin
    Call.CallType := GLNVG_CONVEXFILL;
    { Bounding box fill quad not needed for convex fill }
    Call.TriangleCount := 0;
  end;
  { Allocate vertices for all the paths }
  MaxVerts := glnvg__maxVertCount(Paths, NPaths) + Call.TriangleCount;
  Offset := glnvg__allocVerts(GL, MaxVerts);
  for I := 0 to NPaths - 1 do
  begin
    Copy := @GL.Paths[Call.PathOffset + I];
    Path := @Paths[I];
    FillChar(Copy^, SizeOf(TGLNVGpath), 0);
    if Path.NFill > 0 then
    begin
      Copy.FillOffset := Offset;
      Copy.FillCount := Path.NFill;
      Move(Path.Fill^, GL.Verts[Offset], SizeOf(TNVGvertex) * Path.NFill);
      Inc(Offset, Path.NFill);
    end;
    if Path.NStroke > 0 then
    begin
      Copy.StrokeOffset := Offset;
      Copy.StrokeCount := Path.NStroke;
      Move(Path.Stroke^, GL.Verts[Offset], SizeOf(TNVGvertex) * Path.NStroke);
      Inc(Offset, Path.NStroke);
    end;
  end;
  { Setup uniforms for draw calls }
  if Call.CallType = GLNVG_FILL then
  begin
    { Quad }
    Call.TriangleOffset := Offset;
    Quad := @GL.Verts[Call.TriangleOffset];
    glnvg__vset(@Quad[0], Bounds[2], Bounds[3], 0.5, 1.0);
    glnvg__vset(@Quad[1], Bounds[2], Bounds[1], 0.5, 1.0);
    glnvg__vset(@Quad[2], Bounds[0], Bounds[3], 0.5, 1.0);
    glnvg__vset(@Quad[3], Bounds[0], Bounds[1], 0.5, 1.0);
    Call.UniformOffset := glnvg__allocFragUniforms(GL, 2);
    { Simple shader for stencil }
    Frag := nvg__fragUniformPtr(GL, Call.UniformOffset);
    FillChar(Frag^, SizeOf(Frag^), 0);
    Frag.StrokeThr := -1.0;
    Frag.UniformType := NSVG_SHADER_SIMPLE;
    { Fill shader }
    glnvg__convertPaint(GL, nvg__fragUniformPtr(GL, Call.UniformOffset + GL.FragSize), Paint, Scissor, Fringe, Fringe, -1.0);
  end
  else
  begin
    Call.UniformOffset := glnvg__allocFragUniforms(GL, 1);
    { Fill shader }
    glnvg__convertPaint(GL, nvg__fragUniformPtr(GL, Call.UniformOffset), Paint, Scissor, Fringe, Fringe, -1.0);
  end;
end;

procedure glnvg__renderStroke(UPtr: Pointer; Paint: PNVGpaint; CompositeOperation: TNVGcompositeOperationState;
  Scissor: PNVGscissor; Fringe, StrokeWidth: Single; Paths: PNVGpath; NPaths: Integer);
var
  GL: PGLNVGcontext;
  Call: PGLNVGcall;
  I, MaxVerts, Offset: Integer;
  Copy: PGLNVGpath;
  Path: PNVGpath;
begin
  GL := UPtr;
  Call := glnvg__allocCall(GL);
  if Call = nil then
    Exit;
  Call.CallType := GLNVG_STROKE;
  Call.PathOffset := glnvg__allocPaths(GL, NPaths);
  Call.PathCount := NPaths;
  Call.Image := Paint.Image;
  Call.BlendFunc := glnvg__blendCompositeOperation(CompositeOperation);
  { Allocate vertices for all the paths }
  MaxVerts := glnvg__maxVertCount(Paths, NPaths);
  Offset := glnvg__allocVerts(GL, MaxVerts);
  for I := 0 to NPaths - 1 do
  begin
    Copy := @GL.Paths[Call.PathOffset + I];
    Path := @Paths[I];
    FillChar(Copy^, SizeOf(TGLNVGpath), 0);
    if Path.NStroke <> 0 then
    begin
      Copy.StrokeOffset := Offset;
      Copy.StrokeCount := Path.NStroke;
      Move(Path.Stroke^, GL.Verts[Offset], SizeOf(TNVGvertex) * Path.NStroke);
      Inc(Offset, Path.NStroke);
    end;
  end;
  if GL.Flags and NVG_STENCIL_STROKES <> 0 then
  begin
    { Fill shader }
    Call.UniformOffset := glnvg__allocFragUniforms(GL, 2);
    glnvg__convertPaint(GL, nvg__fragUniformPtr(GL, Call.UniformOffset), Paint, Scissor, StrokeWidth, Fringe, -1.0);
    glnvg__convertPaint(GL, nvg__fragUniformPtr(GL, Call.UniformOffset + GL.FragSize), Paint, Scissor,
      StrokeWidth, Fringe, 1.0 - 0.5 / 255.0);
  end
  else
  begin
    { Fill shader }
    Call.UniformOffset := glnvg__allocFragUniforms(GL, 1);
    glnvg__convertPaint(GL, nvg__fragUniformPtr(GL, Call.UniformOffset), Paint, Scissor, StrokeWidth, Fringe, -1.0);
  end;
end;

procedure glnvg__renderTriangles(UPtr: Pointer; Paint: PNVGpaint; CompositeOperation: TNVGcompositeOperationState;
  Scissor: PNVGscissor; Verts: PNVGvertex; NVerts: Integer; Fringe: Single);
var
  GL: PGLNVGcontext;
  Call: PGLNVGcall;
  Frag: PGLNVGfragUniforms;
begin
  GL := UPtr;
  Call := glnvg__allocCall(GL);
  if Call = nil then
    Exit;
  Call.CallType := GLNVG_TRIANGLES;
  Call.Image := Paint.Image;
  Call.BlendFunc := glnvg__blendCompositeOperation(CompositeOperation);
  { Allocate vertices for all the paths }
  Call.TriangleOffset := glnvg__allocVerts(GL, NVerts);
  Call.TriangleCount := NVerts;
  Move(Verts^, GL.Verts[Call.TriangleOffset], SizeOf(TNVGvertex) * NVerts);
  { Fill shader }
  Call.UniformOffset := glnvg__allocFragUniforms(GL, 1);
  Frag := nvg__fragUniformPtr(GL, Call.UniformOffset);
  glnvg__convertPaint(GL, Frag, Paint, Scissor, 1.0, Fringe, -1.0);
  Frag.UniformType := NSVG_SHADER_IMG;
end;

procedure glnvg__renderDelete(UPtr: Pointer);
var
  GL: PGLNVGcontext;
  I: Integer;
begin
  GL := UPtr;
  if GL = nil then
    Exit;
  glnvg__deleteShader(GL.Shader);
  {$ifdef nvg_gl3}
  if GL.VertArr <> 0 then
    glDeleteVertexArrays(1, @GL.VertArr);
  {$endif}
  if GL.VertBuf <> 0 then
    glDeleteBuffers(1, @GL.VertBuf);
  for I := 0 to GL.NTextures - 1 do
    if (GL.Textures[I].Tex <> 0) and ((GL.Textures[I].Flags and NVG_IMAGE_NODELETE) = 0) then
      glDeleteTextures(1, @GL.Textures[I].Tex);
  FreeMem(GL.Textures);
  FreeMem(GL.Paths);
  FreeMem(GL.Verts);
  FreeMem(GL.Uniforms);
  FreeMem(GL.Calls);
  FreeMem(GL);
end;

function nvgCreateGL(Flags: Integer): PNVGcontext;
var
  Params: TNVGparams;
  GL: PGLNVGcontext;
begin
  GL := AllocMem(SizeOf(TGLNVGcontext));
  FillChar(Params, SizeOf(Params), 0);
  Params.RenderCreate := glnvg__renderCreate;
  Params.RenderCreateTexture := glnvg__renderCreateTexture;
  Params.RenderDeleteTexture := glnvg__renderDeleteTexture;
  Params.RenderUpdateTexture := glnvg__renderUpdateTexture;
  Params.RenderGetTextureSize := glnvg__renderGetTextureSize;
  Params.RenderViewport := glnvg__renderViewport;
  Params.RenderCancel := glnvg__renderCancel;
  Params.RenderFlush := glnvg__renderFlush;
  Params.RenderFill := glnvg__renderFill;
  Params.RenderStroke := glnvg__renderStroke;
  Params.RenderTriangles := glnvg__renderTriangles;
  Params.RenderDelete := glnvg__renderDelete;
  Params.UserPtr := GL;
  if Flags and NVG_ANTIALIAS <> 0 then
    Params.EdgeAntiAlias := 1
  else
    Params.EdgeAntiAlias := 0;
  GL.Flags := Flags;
  { The GL context is freed by nvgDeleteInternal when creation fails }
  Result := nvgCreateInternal(@Params);
end;

procedure nvgDeleteGL(Ctx: PNVGcontext);
begin
  nvgDeleteInternal(Ctx);
end;

function nvglCreateImageFromHandle(Ctx: PNVGcontext; TextureId: GLuint; W, H, ImageFlags: Integer): Integer;
var
  GL: PGLNVGcontext;
  Tex: PGLNVGtexture;
begin
  GL := nvgInternalParams(Ctx).UserPtr;
  Tex := glnvg__allocTexture(GL);
  if Tex = nil then
    Exit(0);
  Tex.TexType := NVG_TEXTURE_RGBA;
  Tex.Tex := TextureId;
  Tex.Flags := ImageFlags;
  Tex.Width := W;
  Tex.Height := H;
  Result := Tex.Id;
end;

function nvglImageHandle(Ctx: PNVGcontext; Image: Integer): GLuint;
var
  GL: PGLNVGcontext;
  Tex: PGLNVGtexture;
begin
  GL := nvgInternalParams(Ctx).UserPtr;
  Tex := glnvg__findTexture(GL, Image);
  if Tex = nil then
    Exit(0);
  Result := Tex.Tex;
end;

{ Framebuffer utilities }

var
  DefaultFBO: GLint = -1;

function nvgluCreateFramebuffer(Ctx: PNVGcontext; W, H, ImageFlags: Integer): PNVGLUframebuffer;
var
  PriorFBO, PriorRBO: GLint;
  Fb: PNVGLUframebuffer;

  procedure Restore;
  begin
    glBindFramebuffer(GL_FRAMEBUFFER, PriorFBO);
    glBindRenderbuffer(GL_RENDERBUFFER, PriorRBO);
  end;

begin
  PriorFBO := 0;
  PriorRBO := 0;
  glGetIntegerv(GL_FRAMEBUFFER_BINDING, @PriorFBO);
  glGetIntegerv(GL_RENDERBUFFER_BINDING, @PriorRBO);
  Fb := AllocMem(SizeOf(TNVGLUframebuffer));
  Fb.Image := nvgCreateImageRGBA(Ctx, W, H, ImageFlags or NVG_IMAGE_FLIPY or NVG_IMAGE_PREMULTIPLIED, nil);
  Fb.Texture := nvglImageHandle(Ctx, Fb.Image);
  Fb.Ctx := Ctx;
  { Frame buffer object }
  glGenFramebuffers(1, @Fb.Fbo);
  glBindFramebuffer(GL_FRAMEBUFFER, Fb.Fbo);
  { Render buffer object }
  glGenRenderbuffers(1, @Fb.Rbo);
  glBindRenderbuffer(GL_RENDERBUFFER, Fb.Rbo);
  glRenderbufferStorage(GL_RENDERBUFFER, GL_STENCIL_INDEX8, W, H);
  { Combine all }
  glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, Fb.Texture, 0);
  glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_STENCIL_ATTACHMENT, GL_RENDERBUFFER, Fb.Rbo);
  if glCheckFramebufferStatus(GL_FRAMEBUFFER) <> GL_FRAMEBUFFER_COMPLETE then
  begin
    {$ifndef nvg_gles2}
    { If GL_STENCIL_INDEX8 is not supported, try GL_DEPTH24_STENCIL8 as a
      fallback. Some graphics cards require a depth buffer along with a stencil }
    glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH24_STENCIL8, W, H);
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, Fb.Texture, 0);
    glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_STENCIL_ATTACHMENT, GL_RENDERBUFFER, Fb.Rbo);
    if glCheckFramebufferStatus(GL_FRAMEBUFFER) <> GL_FRAMEBUFFER_COMPLETE then
    {$endif}
    begin
      Restore;
      nvgluDeleteFramebuffer(Fb);
      Exit(nil);
    end;
  end;
  Restore;
  Result := Fb;
end;

procedure nvgluBindFramebuffer(Fb: PNVGLUframebuffer);
begin
  if DefaultFBO = -1 then
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, @DefaultFBO);
  if Fb <> nil then
    glBindFramebuffer(GL_FRAMEBUFFER, Fb.Fbo)
  else
    glBindFramebuffer(GL_FRAMEBUFFER, DefaultFBO);
end;

procedure nvgluDeleteFramebuffer(Fb: PNVGLUframebuffer);
begin
  if Fb = nil then
    Exit;
  if Fb.Fbo <> 0 then
    glDeleteFramebuffers(1, @Fb.Fbo);
  if Fb.Rbo <> 0 then
    glDeleteRenderbuffers(1, @Fb.Rbo);
  if Fb.Image >= 0 then
    nvgDeleteImage(Fb.Ctx, Fb.Image);
  FreeMem(Fb);
end;

end.
