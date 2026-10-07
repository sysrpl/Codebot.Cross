unit Codebot.Render.Buffers;

{$i render.inc}

interface

uses
  Codebot.System,
  Codebot.Geometry,
  Codebot.Render.Contexts;

type
{$define glframebuffer}
{$ifdef glframebuffer}
{$region texture buffer}
{ TTextureBuffer is used to render to a texture }
  TTextureBuffer = class(TContextManagedObject)
  private
    FFrameBuffer: Integer;
    FDepthBuffer: Integer;
    FTexture: Integer;
    FWidth: Integer;
    FHeight: Integer;
  public
    { Create a new texture buffer using a specific size in pixels }
    constructor Create(Width, Height: Integer);
    destructor Destroy; override;
    { Change the size of the buffer }
    procedure Resize(Width, Height: Integer);
    { Start recording to a texture }
    procedure StartRecording;
    { Stop recording leaving texture with the rendered pixels }
    procedure StopRecording;
    { Texture is the location where the render is recorded }
    property Texture: Integer read FTexture;
    { The width of the texture in pixels }
    property Width: Integer read FWidth;
    { The height of the Integer in pixels }
    property Height: Integer read FHeight;
  end;
{$endregion}

{$region shadow buffer}
{ TShadowBuffer records depth into a square depth texture for shadow mapping.
  The texture compares depths when sampled, so shaders read it using a
  sampler2DShadow which returns how lit a point is from 0 to 1 with filtered
  edges. Points outside the texture are lit. }

  TShadowBuffer = class(TContextManagedObject)
  private
    FFrameBuffer: Integer;
    FTexture: Integer;
    FSize: Integer;
    FPriorFrameBuffer: Integer;
  public
    { Create a shadow buffer using a size in pixels }
    constructor Create(Size: Integer = 2048);
    destructor Destroy; override;
    { Start recording depth. Nothing is drawn to color, and depths are offset
      slightly to prevent surfaces from shadowing themselves. }
    procedure StartRecording;
    { Stop recording, restoring the prior frame buffer and viewport }
    procedure StopRecording;
    { The depth texture }
    property Texture: Integer read FTexture;
    { The width and height of the depth texture in pixels }
    property Size: Integer read FSize;
  end;
{$endregion}
{$endif}

{$region vertex buffers}
{ TBaseBuffer is the base class for both static and dynamic buffers }

  TBaseBuffer = class(TContextManagedObject)
  protected
    { Changed is called whenever the contents of the buffer change }
    procedure Changed; virtual;
  public
    { Create a buffer with room for N items }
    constructor Create(N: Integer = 0); virtual;
  end;

  { The class of a buffer }
  TBufferClass = class of TBaseBuffer;

{ TDataBuffer\<T\> is a class for incrementally adding large amounts of
  growing data }

  TDataBuffer<T> = class(TBaseBuffer)
  private
    FBuffer: TArrayList<T>;
    FCount: Integer;
    FLength: Integer;
    procedure Grow(N: Integer);
    function GetData(Index: Integer): Pointer;
    function GetItem(Index: Integer): T;
    procedure SetItem(Index: Integer; Value: T);
  protected
    procedure Added(N: Integer); virtual;
  public
    { Create a new dynamic buffer optionally allocating room for a N number
      of future items }
    constructor Create(N: Integer = 0); override;
    { Remove any extra data allocated by the previous grow }
    procedure Pack;
    { Create a copy of the data buffer }
    function Clone: TObject; virtual;
    { Add a range of items to the buffer }
    procedure AddRange(const Range: array of T);
    { Add a single item to the buffer }
    procedure AddItem(const Item: T);
    { Clear the buffer optionally allocating room for a N number
    of future items }
    procedure Clear(N: Integer = 0);
    { Pointer to the data at a specified index }
    property Data[Index: Integer]: Pointer read GetData; default;
    { Item at specified index }
    property Item[Index: Integer]: T read GetItem write SetItem;
    { The number of items in the buffer }
    property Count: Integer read FCount;
  end;

{ TVertMode described how vertex arrays are sequenced by by draw buffers }

    TVertMode = (
      vertPoints,
      vertLines,
      vertLineLoop,
      vertLineStrip,
      vertTriangles,
      vertTriangleStrip,
      vertTriangleFan,
      vertQuads);

    WordArray = TArrayList<Word>;

{ TDrawingBuffer\<T\> is the abstract base class for drawing vertex arrays.
  Vertices are kept in memory and uploaded to an OpenGL buffer object the
  next time the buffer is drawn after a change. Each drawing buffer owns its
  buffer objects and, when the API supports them, a vertex array object. }

  TDrawingBuffer<T> = class(TDataBuffer<T>)
  private type
    TBufferMark = record
      Mode: TVertMode;
      Start: Integer;
      Length: Integer;
    end;
    TBufferMarkers = TArrayList<TBufferMark>;
  private var
    FMark: TBufferMark;
    FMarkers: TBufferMarkers;
    FProg: Integer;
    FArrayBuffer: Integer;
    FIndexBuffer: Integer;
    FVertexArray: Integer;
    FUploaded: Boolean;
  private
    function GetMarkCount: Integer;
    procedure DrawQuads(Start: Integer; Length: Integer);
    procedure BeginDraw;
    procedure EndDraw;
  protected
    procedure Changed; override;
    procedure Added(N: Integer); override;
    { CountAttributes returns the number of vertex attributes in T }
    function CountAttributes: Integer; virtual; abstract;
    { BindAttributes describes the fields of T as offsets into the bound
      array buffer using glVertexAttribPointer }
    procedure BindAttributes; virtual; abstract;
  public
    { Create a new data buffer optionally allocating room for a N number
      of future vertices }
    constructor Create(N: Integer = 0); override;
    destructor Destroy; override;
    { Set the shader program associated with draw commands }
    procedure SetProgram(Prog: Integer); overload;
    { Set the shader program by name associated with draw commands }
    procedure SetProgram(const ProgName: string); overload;
    { Adds the V T field above to the buffer }
    procedure Add(constref V: T); overload;
    { Clone additional drawing information }
    function Clone: TObject; override;
    { Begin buffering new vertex data using a specified mode }
    procedure BeginBuffer(Mode: TVertMode; Count: Integer = 0);
    { Delineate a new vertex mode type without drawing }
    procedure MarkBuffer(Mode: TVertMode);
    { End buffering and draw everything }
    procedure EndBuffer(DrawBuffer: Boolean = False);
    { Draw everything }
    procedure Draw; overload;
    { Draw items at the marked index }
    procedure Draw(Mark: Integer); overload;
    { Draw from the buffer a starting at an index using a specific mode }
    procedure Draw(Mode: TVertMode; Start: Integer; Length: Integer = 0); overload;
    { Draw from the buffer using a list of vertex indices using a specific mode }
    procedure Draw(Mode: TVertMode; Indices: WordArray); overload;
    { The number of delineated sections in the buffer }
    property MarkCount: Integer read GetMarkCount;
  end;
{$endregion}

{$region vertex types}
type
  { TFlatVertex is a 2D position }
  TFlatVertex = record
    Vertex: TVec2;
  end;

  { TVertex is a 3D position }
  TVertex = record
    Vertex: TVec3;
  end;

  { TColorVertex is a position and a color }
  TColorVertex = record
    Vertex: TVec3;
    Color: TVec4;
  end;

  { TColorTexVertex is a position, a texture coordinate, and a color }
  TColorTexVertex = record
    Vertex: TVec3;
    TexCoord: TVec2;
    Color: TVec4;
  end;

  { TLitColorVertex is a position, a color, and a normal for lighting }
  TLitColorVertex = record
    Vertex: TVec3;
    Color: TVec3;
    Normal: TVec3;
  end;

  { TTexVertex is a position and a texture coordinate }
  TTexVertex = record
    Vertex: TVec3;
    TexCoord: TVec2;
  end;

  { TLitTexVertex is a position, a texture coordinate, and a normal for
    lighting }
  TLitTexVertex = record
    Vertex: TVec3;
    TexCoord: TVec2;
    Normal: TVec3;
  end;

  { TSkinVertex is a lit textured vertex moved by up to four bones. Bones holds
    bone indices stored as floats and Weights holds their weights. }
  TSkinVertex = record
    Vertex: TVec3;
    Normal: TVec3;
    TexCoord: TVec2;
    Bones: TVec4;
    Weights: TVec4;
  end;
{$endregion}

{$region specilized data buffers}
  { TFlatVertexBuffer is a buffer of 2D positions }

  TFlatVertexBuffer = class(TDrawingBuffer<TFlatVertex>)
  protected
    function CountAttributes: Integer; override;
    procedure BindAttributes; override;
  public
    { Add a vertex, returning the buffer so calls can be chained }
    function Add(const V: TVec2): TFlatVertexBuffer; overload;
    function Add(X, Y: Float): TFlatVertexBuffer; overload;
  end;

  { TVertexBuffer is a buffer of 3D positions }

  TVertexBuffer = class(TDrawingBuffer<TVertex>)
  protected
    function CountAttributes: Integer; override;
    procedure BindAttributes; override;
  public
    { Add a vertex, returning the buffer so calls can be chained }
    function Add(const V: TVec3): TVertexBuffer; overload;
    function Add(X, Y, Z: Float): TVertexBuffer; overload;
  end;

  { TColorVertexBuffer is a buffer of colored vertices }

  TColorVertexBuffer = class(TDrawingBuffer<TColorVertex>)
  protected
    function CountAttributes: Integer; override;
    procedure BindAttributes; override;
  public
    { Add a vertex, returning the buffer so calls can be chained }
    function Add(const V: TVec3; const C: TVec4): TColorVertexBuffer; overload;
    function Add(X, Y, Z, R, G, B, A: Float): TColorVertexBuffer; overload;
  end;

  { TTexVertexBuffer is a buffer of textured vertices }

  TTexVertexBuffer = class(TDrawingBuffer<TTexVertex>)
  protected
    function CountAttributes: Integer; override;
    procedure BindAttributes; override;
  public
    { Add a vertex, returning the buffer so calls can be chained }
    function Add(const V: TVec3; const P: TVec2): TTexVertexBuffer; overload;
    function Add(X, Y, Z, PX, PY: Float): TTexVertexBuffer; overload;
  end;

  { TColorTexVertexBuffer is a buffer of colored and textured vertices }

  TColorTexVertexBuffer = class(TDrawingBuffer<TColorTexVertex>)
  protected
    function CountAttributes: Integer; override;
    procedure BindAttributes; override;
  public
    { Add a vertex, returning the buffer so calls can be chained }
    function Add(const V: TVec3; const Tex: TVec2; Color: TVec4): TColorTexVertexBuffer;
      overload;
    function Add(X, Y, Z, U, V, R, G, B, A: Float): TColorTexVertexBuffer; overload;
  end;

  { TSkinVertexBuffer is a buffer of vertices moved by bones }

  TSkinVertexBuffer = class(TDrawingBuffer<TSkinVertex>)
  protected
    function CountAttributes: Integer; override;
    procedure BindAttributes; override;
  public
    { Add a vertex, returning the buffer so calls can be chained }
    function Add(const V, N: TVec3; const Tex: TVec2; const Bones, Weights: TVec4): TSkinVertexBuffer; overload;
  end;
{$endregion}

implementation

uses
  Codebot.Render.Shaders,
  Codebot.OpenGL;

{ Vertex array objects are part of desktop OpenGL 3.0 and OpenGL ES 3.0 }

{$if defined(gl30) or defined(gles30)}
  {$define glvertexarray}
{$endif}

{$ifdef glframebuffer}
{$region texture buffer}
{ TTextureBuffer }

constructor TTextureBuffer.Create(Width, Height: Integer);
begin
  inherited Create(nil);
  glGenFramebuffers(1, @FFrameBuffer);
  glGenRenderbuffers(1, @FDepthBuffer);
  glGenTextures(1, @FTexture);
  Ctx.PushTexture(FTexture);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  Ctx.PopTexture;
  Resize(Width, Height);
end;

procedure TTextureBuffer.Resize(Width, Height: Integer);
begin
  if Width < 1 then
    Width := 1;
  if Height < 1 then
    Height := 1;
  if (FWidth = Width) and (FHeight = Height) then
    Exit;
  FWidth := Width;
  FHeight := Height;
  Ctx.PushTexture(FTexture);
  glBindFramebuffer(GL_FRAMEBUFFER, FFrameBuffer);
  glBindRenderbuffer(GL_RENDERBUFFER, FDepthBuffer);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, FWidth, FHeight, 0, GL_RGBA,
    GL_UNSIGNED_BYTE, nil);
  glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0,
    GL_TEXTURE_2D, FTexture, 0);
  glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH_COMPONENT16, FWidth, FHeight);
  glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT,
    GL_RENDERBUFFER, FDepthBuffer);
  glBindRenderbuffer(GL_RENDERBUFFER, 0);
  glBindFramebuffer(GL_FRAMEBUFFER, 0);
  Ctx.PopTexture;
end;

destructor TTextureBuffer.Destroy;
begin
  glBindRenderbuffer(GL_RENDERBUFFER, 0);
  glBindFramebuffer(GL_FRAMEBUFFER, 0);
  glDeleteRenderbuffers(1, @FDepthBuffer);
  glDeleteFramebuffers(1, @FFrameBuffer);
  glDeleteTextures(1, @FTexture);
  inherited Destroy;
end;

procedure TTextureBuffer.StartRecording;
begin
  glBindFramebuffer(GL_FRAMEBUFFER, FFrameBuffer);
  Ctx.PushViewport(0, 0, FWidth, FHeight);
  Ctx.Clear;
  Ctx.Identity;
end;

procedure TTextureBuffer.StopRecording;
begin
  glBindFramebuffer(GL_FRAMEBUFFER, 0);
  Ctx.PopViewport;
  Ctx.Identity;
end;
{$endregion}

{$region shadow buffer}
{ TShadowBuffer }

constructor TShadowBuffer.Create(Size: Integer = 2048);
begin
  inherited Create(nil);
  if Size < 16 then
    Size := 16;
  FSize := Size;
  glGenTextures(1, @FTexture);
  Ctx.PushTexture(FTexture);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT24, FSize, FSize, 0,
    GL_DEPTH_COMPONENT, GL_UNSIGNED_INT, nil);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_MODE, GL_COMPARE_REF_TO_TEXTURE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_FUNC, GL_LEQUAL);
  Ctx.PopTexture;
  glGenFramebuffers(1, @FFrameBuffer);
  glGetIntegerv(GL_FRAMEBUFFER_BINDING, @FPriorFrameBuffer);
  glBindFramebuffer(GL_FRAMEBUFFER, FFrameBuffer);
  glFramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_TEXTURE_2D, FTexture, 0);
  {$ifndef glesapi}
  { Desktop OpenGL requires that a frame buffer without color draws nothing }
  glDrawBuffer(GL_NONE);
  glReadBuffer(GL_NONE);
  {$endif}
  if glCheckFramebufferStatus(GL_FRAMEBUFFER) <> GL_FRAMEBUFFER_COMPLETE then
  begin
    glBindFramebuffer(GL_FRAMEBUFFER, FPriorFrameBuffer);
    raise EContextError.Create('The shadow buffer could not be created');
  end;
  glBindFramebuffer(GL_FRAMEBUFFER, FPriorFrameBuffer);
end;

destructor TShadowBuffer.Destroy;
begin
  glDeleteFramebuffers(1, @FFrameBuffer);
  glDeleteTextures(1, @FTexture);
  inherited Destroy;
end;

procedure TShadowBuffer.StartRecording;
begin
  glGetIntegerv(GL_FRAMEBUFFER_BINDING, @FPriorFrameBuffer);
  glBindFramebuffer(GL_FRAMEBUFFER, FFrameBuffer);
  Ctx.PushViewport(0, 0, FSize, FSize);
  glClear(GL_DEPTH_BUFFER_BIT);
  glEnable(GL_POLYGON_OFFSET_FILL);
  glPolygonOffset(2, 4);
end;

procedure TShadowBuffer.StopRecording;
begin
  glDisable(GL_POLYGON_OFFSET_FILL);
  glBindFramebuffer(GL_FRAMEBUFFER, FPriorFrameBuffer);
  Ctx.PopViewport;
end;
{$endregion}
{$endif}

{$region vertex buffers}
{ TBaseBuffer }

constructor TBaseBuffer.Create(N: Integer = 0);
begin
  inherited Create(nil);
end;

procedure TBaseBuffer.Changed;
begin
end;

{ TDataBuffer<T> }

constructor TDataBuffer<T>.Create(N: Integer = 0);
begin
  inherited Create;
  Clear(N);
end;

procedure TDataBuffer<T>.Pack;
begin
  if FCount < FLength then
  begin
    FLength := FCount;
    FBuffer.Length := FLength;
  end;
end;

function TDataBuffer<T>.Clone: TObject;
var
  Copy: TDataBuffer<T>;
begin
  Copy := TBufferClass(ClassType).Create as TDataBuffer<T>;
  if FCount = 0 then
    Exit(Copy);
  Copy.FCount := FCount;
  Copy.FLength := FCount;
  FBuffer.CopyFast(Copy.FBuffer, FCount);
  Result := Copy;
end;

procedure TDataBuffer<T>.Added(N: Integer);
begin
end;

procedure TDataBuffer<T>.Grow(N: Integer);
const
  MaxGrowSize = 50000;
var
  C: Integer;
begin
  if N < 1 then
    Exit;
  if N < 16 then
    N := 16;
  if N + FCount > FLength then
    if FLength = 0 then
    begin
      FLength := N;
      FBuffer.Length := N;
    end
    else
    begin
      if FLength > MaxGrowSize then
      begin
        if N < MaxGrowSize then
          C := MaxGrowSize
        else
          C := N;
        C := FLength + C;
      end
      else
      begin
        C := FLength + MaxGrowSize;
        C := FLength * 2;
        FLength := FLength + N;
        while C < FLength do
          C := C * 2;
      end;
      FLength := C;
      FBuffer.Length := C;
    end;
end;

procedure TDataBuffer<T>.Clear(N: Integer = 0);
begin
  Changed;
  FCount := 0;
  if N = 0 then
  begin
    FLength := 0;
    FBuffer.Length := 0;
  end
  else if N > FBuffer.Length then
    Grow(N - FBuffer.Length);
  Added(0);
end;

procedure TDataBuffer<T>.AddRange(const Range: array of T);
var
  I, J: Integer;
begin
  I := Length(Range);
  if I < 1 then
    Exit;
  Grow(I);
  for J := 0 to I - 1 do
    FBuffer.Items[FCount + J] := Range[J];
  Inc(FCount, I);
  Changed;
  Added(I);
end;

procedure TDataBuffer<T>.AddItem(const Item: T);
begin
  Grow(1);
  FBuffer.Items[FCount] := Item;
  Inc(FCount);
  Changed;
  Added(1);
end;

function TDataBuffer<T>.GetData(Index: Integer): Pointer;
begin
  Result := @FBuffer.Items[Index];
end;

function TDataBuffer<T>.GetItem(Index: Integer): T;
begin
  Result := FBuffer.Items[Index];
end;

procedure TDataBuffer<T>.SetItem(Index: Integer; Value: T);
begin
  FBuffer.Items[Index] := Value;
  Changed;
end;

{ TDrawingBuffer<T> }

constructor TDrawingBuffer<T>.Create(N: Integer = 0);
begin
  inherited Create(N);
end;

destructor TDrawingBuffer<T>.Destroy;
begin
  {$ifdef glvertexarray}
  if FVertexArray <> 0 then
    glDeleteVertexArrays(1, @FVertexArray);
  {$endif}
  if FIndexBuffer <> 0 then
    glDeleteBuffers(1, @FIndexBuffer);
  if FArrayBuffer <> 0 then
    glDeleteBuffers(1, @FArrayBuffer);
  inherited Destroy;
end;

procedure TDrawingBuffer<T>.SetProgram(Prog: Integer);
begin
  FProg := Prog;
end;

procedure TDrawingBuffer<T>.SetProgram(const ProgName: string);
begin
  FProg := Ctx.Shaders.Prog[ProgName].Handle;
end;

procedure TDrawingBuffer<T>.Add(constref V: T);
begin
  AddItem(V);
end;

function TDrawingBuffer<T>.Clone: TObject;
var
  Copy: TDrawingBuffer<T>;
begin
  Copy := TDrawingBuffer<T>(inherited Clone);
  if FCount = 0 then
    Exit(Copy);
  Copy.FMark := FMark;
  FMarkers.CopyFast(Copy.FMarkers, 0);
  Result := Copy;
end;

procedure TDrawingBuffer<T>.Changed;
begin
  FUploaded := False;
end;

{ BeginDraw binds the vertex data, uploading it if it has changed, and
  activates the shader program }

procedure TDrawingBuffer<T>.BeginDraw;
var
  I: Integer;
  S: string;
begin
  if FArrayBuffer = 0 then
  begin
    glGenBuffers(1, @FArrayBuffer);
    {$ifdef glvertexarray}
    { The vertex array records the attribute layout once }
    glGenVertexArrays(1, @FVertexArray);
    glBindVertexArray(FVertexArray);
    glBindBuffer(GL_ARRAY_BUFFER, FArrayBuffer);
    for I := 0 to CountAttributes - 1 do
      glEnableVertexAttribArray(I);
    BindAttributes;
    {$endif}
  end;
  {$ifdef glvertexarray}
  glBindVertexArray(FVertexArray);
  {$endif}
  glBindBuffer(GL_ARRAY_BUFFER, FArrayBuffer);
  if not FUploaded then
  begin
    glBufferData(GL_ARRAY_BUFFER, FCount * SizeOf(T), @FBuffer.Items[0],
      GL_DYNAMIC_DRAW);
    FUploaded := True;
  end;
  {$ifndef glvertexarray}
  for I := 0 to CountAttributes - 1 do
    glEnableVertexAttribArray(I);
  BindAttributes;
  {$endif}
  if FProg = 0 then
  begin
    S := ClassName;
    S := S.ToLower.Copy(2);
    FProg := Ctx.Shaders[S].Handle;
  end;
  if FProg > -1 then
  begin
    Ctx.PushProgram(FProg);
    Ctx.SetProgramMatrix;
  end;
end;

procedure TDrawingBuffer<T>.EndDraw;
{$ifndef glvertexarray}
var
  I: Integer;
{$endif}
begin
  if FProg > -1 then
    Ctx.PopProgram;
  {$ifdef glvertexarray}
  glBindVertexArray(0);
  {$else}
  for I := 0 to CountAttributes - 1 do
    glDisableVertexAttribArray(I);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
  {$endif}
  glBindBuffer(GL_ARRAY_BUFFER, 0);
end;

procedure TDrawingBuffer<T>.Added(N: Integer);
begin
  if N = 0 then
  begin
    FMark.Start := 0;
    FMark.Length := 0;
    FMarkers.Clear;
  end
  else
    Inc(FMark.Length, N);
end;

procedure TDrawingBuffer<T>.BeginBuffer(Mode: TVertMode; Count: Integer = 0);
begin
  Clear(Count);
  FMark.Mode := Mode;
end;

procedure TDrawingBuffer<T>.MarkBuffer(Mode: TVertMode);
begin
  if FMark.Length > 0 then
  begin
    FMarkers.Push(FMark);
    FMark.Start := FMark.Start + FMark.Length;
    FMark.Length := 0;
  end;
  FMark.Mode := Mode;
end;

procedure TDrawingBuffer<T>.EndBuffer(DrawBuffer: Boolean = False);
begin
  if FMark.Length > 0 then
    MarkBuffer(FMark.Mode);
  if DrawBuffer then
    Draw;
end;

procedure TDrawingBuffer<T>.DrawQuads(Start: Integer; Length: Integer);
var
  QuadCount: word;
  Indices: WordArray;
  Max, I, J: word;
begin
  QuadCount := Length div 4;
  if QuadCount < 1 then
    Exit;
  Indices.Length := QuadCount * 6;
  Max := Start + QuadCount * 4;
  I := Start;
  J := 0;
  while I < Max do
  begin
    Indices.Items[J] := I;
    Indices.Items[J + 1] := I + 1;
    Indices.Items[J + 2] := I + 2;
    Indices.Items[J + 3] := I;
    Indices.Items[J + 4] := I + 2;
    Indices.Items[J + 5] := I + 3;
    Inc(I, 4);
    Inc(J, 6);
  end;
  Draw(vertTriangles, Indices);
end;

procedure TDrawingBuffer<T>.Draw;
var
  I: Integer;
begin
  if FMark.Length > 0 then
    MarkBuffer(FMark.Mode);
  for I := FMarkers.Lo to FMarkers.Hi do
    Draw(I);
end;

procedure TDrawingBuffer<T>.Draw(Mark: Integer);
var
  M: TBufferMark;
  I: Integer;
begin
  if Mark < 0 then
    Exit;
  if FMark.Length > 0 then
    MarkBuffer(FMark.Mode);
  I := FMarkers.Length;
  if Mark < I then
  begin
    M := FMarkers[Mark];
    Draw(M.Mode, M.Start, M.Length);
  end
  else if (Mark = FMarkers.Length) and (FMark.Length > 0) then
    Draw(FMark.Mode, FMark.Start, FMark.Length);
end;

procedure TDrawingBuffer<T>.Draw(Mode: TVertMode; Start: Integer; Length: Integer = 0);
begin
  if Start < 0 then
    Exit;
  if Length < 1 then
    Length := FCount - Start;
  if Length < 1 then
    Exit;
  if Start + Length > Count then
    Exit;
  if Mode = vertQuads then
  begin
    DrawQuads(Start, Length);
    Exit;
  end;
  BeginDraw;
  glDrawArrays(GLenum(Mode), Start, Length);
  EndDraw;
end;

procedure TDrawingBuffer<T>.Draw(Mode: TVertMode; Indices: WordArray);
begin
  if FCount < 1 then
    Exit;
  if Indices.IsEmpty then
    Exit;
  if Mode = vertQuads then
    Mode := vertTriangles;
  BeginDraw;
  { Indices are streamed through an element buffer, which the vertex array
    records while it is bound }
  if FIndexBuffer = 0 then
    glGenBuffers(1, @FIndexBuffer);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, FIndexBuffer);
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, Indices.Length * SizeOf(Word),
    @Indices.Items[0], GL_STREAM_DRAW);
  glDrawElements(GLenum(Mode), Indices.Length, GL_UNSIGNED_SHORT, nil);
  EndDraw;
end;

function TDrawingBuffer<T>.GetMarkCount: Integer;
begin
  Result := FMarkers.Length;
  if FMark.Length > 0 then
    Inc(Result);
end;

{$endregion}

{$region specilized data buffers}
{ Offset returns the position of a field within a vertex record as an offset
  into the bound array buffer }

function Offset(Vertex, Field: Pointer): Pointer; inline;
begin
  Result := Pointer(PtrUInt(Field) - PtrUInt(Vertex));
end;

{ TFlatVertex }

function TFlatVertexBuffer.CountAttributes: Integer;
begin
  Result := 1;
end;

procedure TFlatVertexBuffer.BindAttributes;
var
  V: TFlatVertex;
begin
  glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, SizeOf(V), Offset(@V, @V.Vertex));
end;

function TFlatVertexBuffer.Add(X, Y: Float): TFlatVertexBuffer;
var
  V: TFlatVertex;
begin
  V.Vertex.X := X;
  V.Vertex.Y := Y;
  AddItem(V);
  Result := Self;
end;

function TFlatVertexBuffer.Add(const V: TVec2): TFlatVertexBuffer;
var
  Item: TFlatVertex;
begin
  Item.Vertex := V;
  AddItem(Item);
  Result := Self;
end;

{ TVertexBuffer }

function TVertexBuffer.CountAttributes: Integer;
begin
  Result := 1;
end;

procedure TVertexBuffer.BindAttributes;
var
  V: TVertex;
begin
  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, SizeOf(V), Offset(@V, @V.Vertex));
end;

function TVertexBuffer.Add(X, Y, Z: Float): TVertexBuffer;
var
  V: TVertex;
begin
  V.Vertex.X := X;
  V.Vertex.Y := Y;
  V.Vertex.Z := Z;
  AddItem(V);
  Result := Self;
end;

function TVertexBuffer.Add(const V: TVec3): TVertexBuffer;
var
  Item: TVertex;
begin
  Item.Vertex := V;
  AddItem(Item);
  Result := Self;
end;

{ TColorVertexBuffer }

function TColorVertexBuffer.CountAttributes: Integer;
begin
  Result := 2;
end;

procedure TColorVertexBuffer.BindAttributes;
var
  V: TColorVertex;
begin
  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, SizeOf(V), Offset(@V, @V.Vertex));
  glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, SizeOf(V), Offset(@V, @V.Color));
end;

function TColorVertexBuffer.Add(const V: TVec3; const C: TVec4): TColorVertexBuffer;
var
  Item: TColorVertex;
begin
  Item.Vertex := V;
  Item.Color := C;
  AddItem(Item);
  Result := Self;
end;

function TColorVertexBuffer.Add(X, Y, Z, R, G, B, A: Float): TColorVertexBuffer;
var
  Item: TColorVertex;
begin
  Item.Vertex := Vec3(X, Y, Z);
  Item.Color := Vec4(R, G, B, A);
  AddItem(Item);
  Result := Self;
end;

{ TTexVertexBuffer }

function TTexVertexBuffer.CountAttributes: Integer;
begin
  Result := 2;
end;

procedure TTexVertexBuffer.BindAttributes;
var
  V: TTexVertex;
begin
  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, SizeOf(V), Offset(@V, @V.Vertex));
  glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, SizeOf(V), Offset(@V, @V.TexCoord));
end;

function TTexVertexBuffer.Add(const V: TVec3; const P: TVec2): TTexVertexBuffer;
var
  Item: TTexVertex;
begin
  Item.Vertex := V;
  Item.TexCoord := P;
  AddItem(Item);
  Result := Self;
end;

function TTexVertexBuffer.Add(X, Y, Z, PX, PY: Float): TTexVertexBuffer;
var
  Item: TTexVertex;
begin
  Item.Vertex := Vec3(X, Y, Z);
  Item.TexCoord := Vec2(PX, PY);
  AddItem(Item);
  Result := Self;
end;

{ TColorTexVertexBuffer }

function TColorTexVertexBuffer.CountAttributes: Integer;
begin
  Result := 3;
end;

procedure TColorTexVertexBuffer.BindAttributes;
var
  V: TColorTexVertex;
begin
  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, SizeOf(V), Offset(@V, @V.Vertex));
  glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, SizeOf(V), Offset(@V, @V.TexCoord));
  glVertexAttribPointer(2, 4, GL_FLOAT, GL_FALSE, SizeOf(V), Offset(@V, @V.Color));
end;

function TColorTexVertexBuffer.Add(const V: TVec3; const Tex: TVec2;
  Color: TVec4): TColorTexVertexBuffer;
var
  Item: TColorTexVertex;
begin
  Item.Vertex := V;
  Item.TexCoord := Tex;
  Item.Color := Color;
  AddItem(Item);
  Result := Self;
end;

function TColorTexVertexBuffer.Add(X, Y, Z, U, V, R, G, B, A: Float):
TColorTexVertexBuffer;
var
  Item: TColorTexVertex;
begin
  Item.Vertex := Vec3(X, Y, Z);
  Item.TexCoord := Vec2(U, V);
  Item.Color := Vec4(R, G, B, A);
  AddItem(Item);
  Result := Self;
end;

{ TSkinVertexBuffer }

function TSkinVertexBuffer.CountAttributes: Integer;
begin
  Result := 5;
end;

procedure TSkinVertexBuffer.BindAttributes;
var
  V: TSkinVertex;
begin
  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, SizeOf(V), Offset(@V, @V.Vertex));
  glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, SizeOf(V), Offset(@V, @V.Normal));
  glVertexAttribPointer(2, 2, GL_FLOAT, GL_FALSE, SizeOf(V), Offset(@V, @V.TexCoord));
  glVertexAttribPointer(3, 4, GL_FLOAT, GL_FALSE, SizeOf(V), Offset(@V, @V.Bones));
  glVertexAttribPointer(4, 4, GL_FLOAT, GL_FALSE, SizeOf(V), Offset(@V, @V.Weights));
end;

function TSkinVertexBuffer.Add(const V, N: TVec3; const Tex: TVec2; const Bones, Weights: TVec4): TSkinVertexBuffer;
var
  Item: TSkinVertex;
begin
  Item.Vertex := V;
  Item.Normal := N;
  Item.TexCoord := Tex;
  Item.Bones := Bones;
  Item.Weights := Weights;
  AddItem(Item);
  Result := Self;
end;
{$endregion}

end.

