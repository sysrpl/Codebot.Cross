unit Codebot.Render.Contexts;

{$i render.inc}

interface

uses
  SysUtils, Classes,
  Codebot.System,
  Codebot.Platform,
  Codebot.Graphics.Types,
  Codebot.OpenGL,
  Codebot.Geometry;

type
  EContextError = class(Exception);
  EContextAssetError = class(EContextError);
  EContextCollectionError = class(EContextError);
  EOpenGLError = class(Exception);

  TContextCollection = class;

{ TContextManagedObject provides a way to manage the lifetime of objects such as shaders,
  textures, and vertex buffers. If no collection is given to the constructor create then
  the object will be maintained by Ctx.Objects.

  Managed objects require a render context, so they must be created on a thread
  with a current render context, such as in the OnRenderStart, OnRender, or
  OnRenderStop events of TGraphicsBox. Any objects not freed by the user are
  freed when the render context is destroyed. A managed object which owns other
  managed objects should free them using FreeManaged in its destructor. These
  are the managed objects defined by the library:

  Codebot.Render.Shaders
    TVertexShader, TFragmentShader compile a single shader stage from source
    TShaderProgram links a vertex and fragment shader into a program. It can be
      created from source, files, or assets, and is made current with Push/Pop.
    Ctx.Shaders is a TShaderCollection which finds programs by name, loading
      name.vert and name.frag from the assets/shaders folder when they do not
      exist. Sources without a #version directive are given one matching the
      API selected in render.inc.

  Codebot.Render.Textures
    TTexture is a 2D texture loaded from a bitmap, stream, file, or RGBA pixel
      data with filter, wrap, and mipmap settings. It is bound to a texture
      slot with Push/Pop.
    Ctx.Textures is a TTextureCollection which finds textures by name, loading
      them from the assets/textures folder when they do not exist

  Codebot.Render.Buffers
    TFlatVertexBuffer, TVertexBuffer, TColorVertexBuffer, TTexVertexBuffer,
      TColorTexVertexBuffer, and TSkinVertexBuffer hold vertices in memory and
      upload them to an OpenGL buffer when drawn after a change. Their fields
      are vertex attributes 0, 1, 2, and so on in order. Without SetProgram
      they draw using a program from
      Ctx.Shaders named after the class, such as 'texvertexbuffer'.
    TTextureBuffer renders to a texture through a framebuffer with a depth buffer
      between StartRecording and StopRecording

  Codebot.Render.World
    TWorld maps a 2D virtual resolution onto a 3D perspective. It is returned by
      Ctx.World and optionally provides a camera, a skybox, and a ground grid.
    TCamera holds a position and direction applied to the modelview matrix
    TSkybox draws an inward facing textured cube around the camera. Its texture
      must be loaded by the user.

  Codebot.Render.Fonts
    TFont renders the glyphs of a TrueType font at a pixel size into a texture
      atlas and lays out text as quads in a TColorTexVertexBuffer
    Ctx.Fonts is a TFontCollection which finds fonts by name, loading name.ttf
      from the assets/fonts folder when they do not exist. Its DefaultFont is
      the roboto font provided with the library assets.
    TTextBlock draws a string with a font, position, scale, and color,
      rebuilding its vertices only when they change }

  TContextManagedObject = class(IInterface)
  private
    FName: string;
    FCollection: TContextCollection;
    FNext: TContextManagedObject;
    procedure SetName(const Value: string);
  protected
    function QueryInterface(constref Iid: TGuid; out Obj): LongInt; apicall;
    function _AddRef: LongInt; apicall;
    function _Release: LongInt; apicall;
    { FreeManaged frees a managed object owned by this object and sets it to
      nil. While the render context is being destroyed it only sets it to nil,
      as the render context frees every managed object itself. Use it in place
      of Free in destructors. }
    procedure FreeManaged(var Obj);
  public
    { Create a new item managing its lifetime with a collection and name. If no
      collection is given it will be maintained by Ctx.Objects. }
    constructor Create(Collection: TContextCollection; const Name: string = '');
    { Destroy automatically removes the item from its collection }
    destructor Destroy; override;
    { Setting the name adds to or removes the item from a collection }
    property Name: string read FName write SetName;
  end;

{ Other units may define TContextCollection extensions to provide managed
  access to objects such as shaders, textures, effects, and so on.

  Class helpers can add to a context using functions such as:

    function TShaderExtention.Shaders: TShaderCollection;
    function TTextureExtention.Textures: TTextureCollection; }

  TContextCollection = class
  private
    FName: string;
    FNextCollection: TContextCollection;
    FNext: TContextManagedObject;
  protected
    function GetObject(const Name: string): TContextManagedObject;
    property Objects[AName: string]: TContextManagedObject read GetObject;
  public
    { Collection name must not be blank and must be unique }
    constructor Create(const Name: string);
    destructor Destroy; override;
    { The read only name of the collection }
    property Name: string read FName;
  end;

{ TManagedObjectCollection is used by a context as the default collection for
  managed objects which are created without a collection }

  TManagedObjectCollection = class(TContextCollection)
  public
    property Objects; default;
  end;

{ The TRenderContext class provides an interface to all rendering in this library }

  TRenderContext = class
  private type
    TTextureItem = record
      Texture: Integer;
      Slot: Integer;
      Previous: Integer;
    end;
    TTextureStack = TStack<TTextureItem>;
    TMatrixStack = TStack<TMatrix>;
    TViewportStack = TStack<TRectI>;
    TBoolStack = TStack<Boolean>;
    TIntStack = TStack<Integer>;
  private var
    FAssetFolder: string;
    FCull: Boolean;
    FCullStack: TBoolStack;
    FDepthTest: Boolean;
    FDepthTestStack: TBoolStack;
    FDepthWriting: Boolean;
    FDepthWritingStack: TBoolStack;
    FProgramStack: TIntStack;
    FProgramCount: TIntStack;
    FProgramChange: Boolean;
    FViewport: TRectI;
    FViewportStack: TViewportStack;
    FCollection: TContextCollection;
    FObjects: TManagedObjectCollection;
    FTextureStack: TTextureStack;
    FModelviewStack: TMatrixStack;
    FModelviewCurrent: TMatrix;
    FProjectionStack: TMatrixStack;
    FProjectionCurrent: TMatrix;
    FMatrixChange: Boolean;
    FWorld: TContextManagedObject;
    FDestroying: Boolean;
  private
    { Add a collection or raise an EContextCollectionError exeption if the
      name is blank or already exists }
    procedure AddCollection(Collection: TContextCollection);
  public
    constructor Create;
    destructor Destroy; override;
    {$region general context methods and rendering options}
    { Set the color to use when cleared }
    procedure SetClearColor(R, G, B, A: Float);
    { Clear the color and depth buffer bits }
    procedure Clear;
    { Change rendering ability to remove back facing polygons (default to true) }
    procedure PushCulling(Cull: Boolean);
    { Restore previous setting to remove back facing polygons }
    procedure PopCulling;
    { Change rendering ability to bypass depth buffer testing (default to true) }
    procedure PushDepthTesting(DepthTest: Boolean);
    { Restore previous setting to depth buffer testing }
    procedure PopDepthTesting;
    { Change rendering ability to write to the depth buffer (default to true) }
    procedure PushDepthWriting(DepthWriting: Boolean);
    { Restore previous ability to write to the depth buffer }
    procedure PopDepthWriting;
    { Put the OpenGL state back to what the context keeps track of: blending
      on with the straight alpha blend function, and culling, depth testing,
      and depth writing as they were last pushed. It is for code which draws
      with OpenGL outside of the context and leaves this state changed, as
      the canvas does. }
    procedure RestoreState;
    {$endregion}
    {$region viewports}
    { Get the current viewport }
    function GetViewport: TRectI;
    { Set the current viewport erasing the viewport stack }
    procedure SetViewport(X, Y, W, H: Integer);
    { Set the current viewport and pushing the prior one to the stack }
    procedure PushViewport(X, Y, W, H: Integer);
    { Restore the prior viewport from the stack }
    procedure PopViewport;
    { Get the world for this context }
    function GetWorld: TContextManagedObject;
    { Set the world for this context }
    procedure SetWorld(Value: TContextManagedObject);
    { Save the current viewport contents to a bitmap }
    procedure SaveToBitmap(Bitmap: IBitmapData);
    { Save the current viewport contents to a bitmap stream }
    procedure SaveToStream(Stream: TStream);
    { Save the current viewport contents to a bitmap file }
    procedure SaveToFile(const FileName: string);
    {$endregion}
    {$region assets and collections}
    { Search for an asset stream first using a resource name then using
      GetAssetFile.  }
    function GetAssetStream(const Name: string): TStream;
    { Search upwards for an asset returning the valid filename or raise
      an EContextAssetError exception }
    function GetAssetFile(const FileName: string): string;
    { Search upwards for an asset the same way as GetAssetFile, returning
      False instead of raising an exception if it cannot be found }
    function FindAssetFile(const FileName: string; out Path: string): Boolean;
    { Set the asset folder name, which defaults to 'assets' }
    procedure SetAssetFolder(const Folder: string);
    { Returns a collection by name }
    function GetCollection(const Name: string): TContextCollection;
    { Objects refers to managed objects without a specialized collection  }
    function Objects: TManagedObjectCollection;
    {$endregion}
    {$region shader program stack}
    { Returns the current program }
    function GetProgram: Integer;
    { Add the program to the stack and activates it }
    procedure PushProgram(Prog: Integer);
    { Removes a program from the stack and potentially deactivates it }
    procedure PopProgram;
    { Set the current program's modelview and perspective uniforms }
    procedure SetProgramMatrix;
    { Get the location of unform for the specified program }
    function GetUniform(Prog: Integer; Name: string; out Location: Integer): Boolean; overload;
    { Get the location of unform for the current program }
    function GetUniform(const Name: string; out Location: Integer): Boolean; overload;
    { Overload to set program uniforms by name }
    procedure SetUniform(Location: Integer; const B: Boolean); overload;
    procedure SetUniform(const Name: string; const B: Boolean); overload;
    procedure SetUniform(Location: Integer; const I: Integer); overload;
    procedure SetUniform(const Name: string; const I: Integer); overload;
    procedure SetUniform(Location: Integer; const X: Float); overload;
    procedure SetUniform(const Name: string; const X: Float); overload;
    procedure SetUniform(Location: Integer; const A: TArray<Float>); overload;
    procedure SetUniform(const Name: string; const A: TArray<Float>); overload;
    procedure SetUniform(Location: Integer; const X, Y: Float); overload;
    procedure SetUniform(const Name: string; const X, Y: Float); overload;
    procedure SetUniform(Location: Integer; const X, Y, Z: Float); overload;
    procedure SetUniform(const Name: string; const X, Y, Z: Float); overload;
    procedure SetUniform(Location: Integer; const X, Y, Z, W: Float); overload;
    procedure SetUniform(const Name: string; const X, Y, Z, W: Float); overload;
    procedure SetUniform(Location: Integer; const V: TVec2); overload;
    procedure SetUniform(const Name: string; const V: TVec2); overload;
    procedure SetUniform(Location: Integer; const V: TVec3); overload;
    procedure SetUniform(const Name: string; const V: TVec3); overload;
    procedure SetUniform(Location: Integer; const V: TVec4); overload;
    procedure SetUniform(const Name: string; const V: TVec4); overload;
    procedure SetUniform(Location: Integer; const M: TMatrix); overload;
    procedure SetUniform(const Name: string; const M: TMatrix); overload;
    {$endregion}
    {$region texture stacks}
    { Activate a texture unit (slot to avoid reserved word) which can be any number 0-9 }
    procedure SetTextureSlot(Slot: Integer);
    { Retrieve the number of the active texture unit }
    function GetTextureSlot: Integer;
    { Retrieve the texture bound to the active texture unit }
    function GetTexture: Integer;
    { Add the texture to the stack and bind it to a unit }
    procedure PushTexture(Texture: Integer; Slot: Integer = 0);
    { Removes a texture from the stack and potentially activates new texture and unit }
    procedure PopTexture;
    {$endregion}
    {$region matrix stacks}
    { Replaces the current modelview matrix }
    procedure SetModelview(constref M: TMatrix);
    { Returns the current modelview matrix }
    function GetModelview: TMatrix;
    { Adds a new modelview matrix on to the stack }
    procedure PushModelview(const M: TMatrix);
    { Removes the most recent modelview matrix from the stack }
    procedure PopModelview;
    { Replaces the current model view matrix with a look at matrix }
    procedure LookAt(Eye, Center, Up: TVec3);
    { Replaces the current model view matrix with an identity matrix }
    procedure Identity;
    { Transform the current model view matrix with a matrix }
    procedure Transform(constref T: TMatrix);
    { Translate the current model view matrix }
    procedure Translate(X, Y, Z: Float);
    { Rotate the current model view matrix }
    procedure Rotate(X, Y, Z: Float; Order: TRotationOrder = roZXY);
    { Scale the current model view matrix }
    procedure Scale(X, Y, Z: Float);
    { Replace the current projection matrix }
    procedure SetProjection(constref M: TMatrix);
    { Returns the current projection matrix }
    function GetProjection: TMatrix;
    { Adds a new projection matrix to the stack }
    procedure PushProjection(const M: TMatrix);
    { Removes the most recent projection matrix from the stack }
    procedure PopProjection;
    { Replaces the current pespective matrix with a perspective matrix }
    procedure Perspective(FoV, AspectRatio, NearPlane, FarPlane: Float);
    { Replaces the current pespective matrix with a frustum matrix }
    procedure Frustum(Left, Right, Top, Bottom, NearPlane, FarPlane: Float);
    {$endregion}
  end;

{ Ctx returns the render context of the calling thread or throws EContextError
  if there is none. A render context becomes the context of the thread which
  creates it, and stops being its context when destroyed. TGraphicsBox creates
  a render context on its render thread before OnRenderStart and destroys it
  after OnRenderStop. }

function Ctx: TRenderContext;

{ RestoreContextState calls RestoreState of the render context of the current
  thread, and does nothing when there is none. The canvas calls it when it
  ends a frame. }

procedure RestoreContextState;

resourcestring
  SNoOpenGL = 'The OpenGL library could not be loaded';
  SNoContext = 'No context is available';
  SAssetNotFound = 'Cannot locate asset with name ''%s''';
  SAssetNotUnderstood = 'Cannot understand asset with name ''%s''';
  SNoCollectionName = 'Cannot add unnammed collections';
  SDuplicateCollectionName = 'An collection or item with name ''%s'' already exist';


implementation

{ Each render thread has its own render context }

threadvar
  InternalContext: TObject;

function Ctx: TRenderContext;
begin
  if InternalContext = nil then
    raise EContextError.Create(SNoContext);
  Result := TRenderContext(InternalContext);
end;

procedure RestoreContextState;
begin
  if InternalContext <> nil then
    TRenderContext(InternalContext).RestoreState;
end;

{ TContextManagedObject }

function TContextManagedObject.QueryInterface(constref Iid: TGuid; out Obj): LongInt;
begin
  if GetInterface(Iid, Obj) then
    Result := S_OK
  else
    Result := LongInt(E_NOINTERFACE);
end;

function TContextManagedObject._AddRef: LongInt;
begin
  Result := 1;
end;

function TContextManagedObject._Release: LongInt;
begin
  Result := 1;
end;

constructor TContextManagedObject.Create(Collection: TContextCollection; const Name: string = '');
begin
  inherited Create;
  if Collection = nil then
    Collection := Ctx.Objects;
  FCollection := Collection;
  SetName(Name);
end;

destructor TContextManagedObject.Destroy;
var
  P: ^TContextManagedObject;
begin
  { Unlink from the collection }
  P := @FCollection.FNext;
  while P^ <> nil do
    if P^ = Self then
    begin
      P^ := FNext;
      Break;
    end
    else
      P := @P^.FNext;
  inherited Destroy;
end;

procedure TContextManagedObject.FreeManaged(var Obj);
var
  Item: TObject;
begin
  Item := TObject(Obj);
  TObject(Obj) := nil;
  if Item = nil then
    Exit;
  if (InternalContext <> nil) and TRenderContext(InternalContext).FDestroying then
    Exit;
  Item.Free;
end;

procedure TContextManagedObject.SetName(const Value: string);
var
  C: TContextManagedObject;
begin
  if Value = FName then
    Exit;
  C := FCollection.FNext;
  if Value <> '' then
    while C <> nil do
    begin
      if (C <> Self) and (C.FName = Value) then
        raise EContextCollectionError.CreateFmt(SDuplicateCollectionName, [Name]);
      C := C.FNext;
    end;
  FName := Value;
end;

{ TContextCollection }

constructor TContextCollection.Create(const Name: string);
begin
  inherited Create;
  FName := Name;
  Ctx.AddCollection(Self);
end;

destructor TContextCollection.Destroy;
begin
  { Each item unlinks itself when freed }
  while FNext <> nil do
    FNext.Free;
  inherited Destroy;
end;

function TContextCollection.GetObject(const Name: string): TContextManagedObject;
var
  C: TContextManagedObject;
begin
  if Name = '' then
    Exit(nil);
  C := FNext;
  while C <> nil do
    if C.Name = Name then
      Exit(C)
    else
      C := C.FNext;
  Result := nil;
end;

{ TRenderContext }

constructor TRenderContext.Create;
const
  StackSize = 100;
begin
  inherited Create;
  if not OpenGLInfo.IsValid then
    raise EContextError.Create(SNoOpenGL);
  InternalContext := Self;
  FAssetFolder := 'assets';
  FProjectionCurrent.Identity;
  FModelviewCurrent.Identity;
  FMatrixChange := True;
  glEnable(GL_BLEND);
  FCull := True;
  glEnable(GL_CULL_FACE);
  FDepthTest := True;
  glEnable(GL_DEPTH_TEST);
  FDepthWriting := True;
  glDepthMask(GL_TRUE);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  FCullStack := TBoolStack.Create(StackSize);
  FDepthTestStack := TBoolStack.Create(StackSize);
  FDepthWritingStack := TBoolStack.Create(StackSize);
  FProgramStack := TIntStack.Create(StackSize);
  FProgramCount := TIntStack.Create(StackSize);
  FViewportStack := TViewportStack.Create(StackSize);
  FTextureStack := TTextureStack.Create(StackSize);
  FModelviewStack := TMatrixStack.Create(StackSize);
  FProjectionStack := TMatrixStack.Create(StackSize);
end;

destructor TRenderContext.Destroy;
var
  C, N: TContextCollection;
begin
  { Ctx remains valid while managed objects are freed }
  FDestroying := True;
  C := FCollection;
  FCollection := nil;
  while C <> nil do
  begin
    N := C.FNextCollection;
    C.Free;
    C := N;
  end;
  if InternalContext = Self then
    InternalContext := nil;
  inherited Destroy;
end;

procedure TRenderContext.AddCollection(Collection: TContextCollection);
var
  C: TContextCollection;
begin
  if Collection.Name = '' then
    raise EContextCollectionError.Create(SNoCollectionName);
  C := FCollection;
  if C = nil then
  begin
    FCollection := Collection;
    Exit;
  end;
  while C.FNextCollection <> nil do
  begin
    if C.Name = Collection.Name then
      raise EContextCollectionError.CreateFmt(SDuplicateCollectionName, [C.Name]);
    C := C.FNextCollection;
  end;
  C.FNextCollection := Collection;
end;

{$region general context methods}
procedure TRenderContext.SetClearColor(R, G, B, A: Float);
begin
  glClearColor(R, G, B, A);
end;

procedure TRenderContext.Clear;
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
end;

procedure TRenderContext.PushCulling(Cull: Boolean);
begin
  FCullStack.Push(FCull);
  FCull := Cull;
  if FCull then
    glEnable(GL_CULL_FACE)
  else
    glDisable(GL_CULL_FACE);
end;

procedure TRenderContext.PopCulling;
begin
  if FCullStack.Index < 0 then
    Exit;
  FCull := FCullStack.Pop;
  if FCull then
    glEnable(GL_CULL_FACE)
  else
    glDisable(GL_CULL_FACE);
end;

procedure TRenderContext.PushDepthTesting(DepthTest: Boolean);
begin
  FDepthTestStack.Push(FDepthTest);
  FDepthTest := DepthTest;
  if FDepthTest then
    glEnable(GL_DEPTH_TEST)
  else
    glDisable(GL_DEPTH_TEST);
end;

procedure TRenderContext.PopDepthTesting;
begin
  if FDepthTestStack.Index < 0 then
    Exit;
  FDepthTest := FDepthTestStack.Pop;
  if FDepthTest then
    glEnable(GL_DEPTH_TEST)
  else
    glDisable(GL_DEPTH_TEST);
end;

procedure TRenderContext.PushDepthWriting(DepthWriting: Boolean);
begin
  FDepthWritingStack.Push(FDepthWriting);
  FDepthWriting := DepthWriting;
  if FDepthWriting then
    glDepthMask(GL_TRUE)
  else
    glDepthMask(GL_FALSE);
end;

procedure TRenderContext.PopDepthWriting;
begin
  if FDepthWritingStack.Index < 0 then
    Exit;
  FDepthWriting := FDepthWritingStack.Pop;
  if FDepthWriting then
    glDepthMask(GL_TRUE)
  else
    glDepthMask(GL_FALSE);
end;

procedure TRenderContext.RestoreState;
begin
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  if FCull then
    glEnable(GL_CULL_FACE)
  else
    glDisable(GL_CULL_FACE);
  if FDepthTest then
    glEnable(GL_DEPTH_TEST)
  else
    glDisable(GL_DEPTH_TEST);
  if FDepthWriting then
    glDepthMask(GL_TRUE)
  else
    glDepthMask(GL_FALSE);
end;

function TRenderContext.GetViewport: TRectI;
begin
  Result := FViewport;
end;

procedure TRenderContext.SetViewport(X, Y, W, H: Integer);
begin
  FViewport := TRectI.Create(X, Y, W, H);
  glViewport(X, Y, W, H);
end;

procedure TRenderContext.PushViewport(X, Y, W, H: Integer);
begin
  FViewportStack.Push(FViewport);
  FViewport := TRectI.Create(X, Y, W, H);
  glViewport(X, Y, W, H);
end;

procedure TRenderContext.PopViewport;
begin
  if FViewportStack.Index < 0 then
    Exit;
  FViewport := FViewportStack.Pop;
  glViewport(FViewport.X, FViewport.Y, FViewport.Width, FViewport.Height);
end;

function TRenderContext.GetWorld: TContextManagedObject;
begin
  Result := FWorld;
end;

procedure TRenderContext.SetWorld(Value: TContextManagedObject);
begin
  FWorld := Value;
end;

{ OpenGL returns rows of RGBA bottom to top while bitmaps store rows of
  premultiplied BGRA top to bottom, so the rows are flipped and the pixels are
  converted as they are copied. This is the reverse of TTexture.LoadFromBitmap. }

procedure TRenderContext.SaveToBitmap(Bitmap: IBitmapData);
var
  Data: array of Byte;
  W, H, A, X, Y: Integer;
  Source: PByte;
  Dest: PPixel;
begin
  W := FViewport.Width;
  H := FViewport.Height;
  Bitmap.SetSize(W, H);
  if (W < 1) or (H < 1) then
    Exit;
  SetLength(Data, W * H * 4);
  glPixelStorei(GL_PACK_ALIGNMENT, 4);
  glReadPixels(FViewport.X, FViewport.Y, W, H, GL_RGBA, GL_UNSIGNED_BYTE,
    @Data[0]);
  Dest := Bitmap.Pixels;
  for Y := H - 1 downto 0 do
  begin
    Source := @Data[Y * W * 4];
    for X := 0 to W - 1 do
    begin
      A := Source[3];
      Dest.Red := (Source[0] * A + $7F) div $FF;
      Dest.Green := (Source[1] * A + $7F) div $FF;
      Dest.Blue := (Source[2] * A + $7F) div $FF;
      Dest.Alpha := A;
      Inc(Source, 4);
      Inc(Dest);
    end;
  end;
end;

{ A new bitmap saves to a stream as a png }

procedure TRenderContext.SaveToStream(Stream: TStream);
var
  B: IBitmapData;
begin
  B := NewBitmapData;
  SaveToBitmap(B);
  B.SaveToStream(Stream);
end;

procedure TRenderContext.SaveToFile(const FileName: string);
var
  B: IBitmapData;
begin
  B := NewBitmapData;
  SaveToBitmap(B);
  B.SaveToFile(FileName);
end;
{$endregion}

{$region assets and collections}
function TRenderContext.GetAssetStream(const Name: string): TStream;
var
  S: string;
begin
  if ResLoadData(Name, Result) then
    Exit;
  S := GetAssetFile(Name);
  Result := TFileStream.Create(S, fmOpenRead);
end;

function TRenderContext.FindAssetFile(const FileName: string; out Path: string): Boolean;
var
  S: string;
  I: Integer;
begin
  Path := '';
  S := PathCombine(FAssetFolder, FileName);
  for I := 0 to 9 do
  begin
    if FileExists(S) then
    begin
      Path := S;
      Exit(True);
    end;
    S := PathCombine('..', S);
  end;
  Result := False;
end;

function TRenderContext.GetAssetFile(const FileName: string): string;
begin
  if not FindAssetFile(FileName, Result) then
    raise EContextAssetError.CreateFmt(SAssetNotFound, [FileName]);
end;

procedure TRenderContext.SetAssetFolder(const Folder: string);
begin
  FAssetFolder := Folder;
end;

function TRenderContext.GetCollection(const Name: string): TContextCollection;
var
  C: TContextCollection;
begin
  C := FCollection;
  while C <> nil do
    if C.Name = Name then
      Exit(C)
    else
      C := C.FNextCollection;
  Result := nil;
end;

const
  SManagedObjectCollection = 'objects';

function TRenderContext.Objects: TManagedObjectCollection;
begin
  if FObjects = nil then
    FObjects := TManagedObjectCollection.Create(SManagedObjectCollection);
  REsult := FObjects;
end;

{$endregion}

{$region program shader stack}
function TRenderContext.GetProgram: Integer;
begin
  glGetIntegerv(GL_CURRENT_PROGRAM, @Result);
end;

procedure TRenderContext.PushProgram(Prog: Integer);
begin
  if (FProgramStack.IsEmpty) or (Prog <> FProgramStack.Last) then
  begin
    glUseProgram(Prog);
    FProgramStack.Push(Prog);
    FProgramCount.Push(1);
    FProgramChange := True;
  end
  else
    FProgramCount.Last := FProgramCount.Last + 1;
end;

procedure TRenderContext.PopProgram;
begin
  if FProgramStack.IsEmpty then
    Exit;
  FProgramCount.Last := FProgramCount.Last - 1;
  if FProgramCount.Last < 1 then
  begin
    FProgramCount.Pop;
    FProgramStack.Pop;
    if FProgramStack.IsEmpty then
      glUseProgram(0)
    else
      glUseProgram(FProgramStack.Last);
    FProgramChange := True;
  end;
end;

function TRenderContext.GetUniform(Prog: Integer; Name: string; out Location: Integer): Boolean;
begin
  Location := glGetUniformLocation(Prog, PChar(Name));
  Result := (Location > -1) and (Location < GL_INVALID_ENUM);
end;

function TRenderContext.GetUniform(const Name: string; out Location: Integer): Boolean;
var
  I: Integer;
begin
  I := GetProgram;
  if I < 1 then
  begin
    Location := -1;
    Exit(False);
  end;
  Location := glGetUniformLocation(I, PChar(Name));
  Result := (Location > -1) and (Location < GL_INVALID_ENUM);
end;

procedure TRenderContext.SetUniform(Location: Integer; const B: Boolean);
begin
  if B then
    SetUniform(Location, 1)
  else
    SetUniform(Location, 0);
end;

procedure TRenderContext.SetUniform(const Name: string; const B: Boolean);
begin
  if B then
    SetUniform(Name, 1)
  else
    SetUniform(Name, 0);
end;

procedure TRenderContext.SetUniform(Location: Integer; const I: Integer);
begin
  glUniform1i(Location, I);
end;

procedure TRenderContext.SetUniform(const Name: string; const I: Integer);
var
  L: Integer;
begin
  if GetUniform(Name, L) then
    glUniform1i(L, I);
end;

procedure TRenderContext.SetUniform(Location: Integer; const X: Float);
begin
  glUniform1f(Location, X);
end;

procedure TRenderContext.SetUniform(const Name: string; const X: Float);
var
  L: Integer;
begin
  if GetUniform(Name, L) then
    glUniform1f(L, X);
end;

procedure TRenderContext.SetUniform(Location: Integer; const A: TArray<Float>);
begin
  glUniform1fv(Location, Length(A), @A[0]);
end;

procedure TRenderContext.SetUniform(const Name: string; const A: TArray<Float>);
var
  L: Integer;
begin
  if GetUniform(Name, L) then
    glUniform1fv(L, Length(A), @A[0]);
end;

procedure TRenderContext.SetUniform(Location: Integer; const X, Y: Float);
begin
  glUniform2f(Location, X, Y);
end;

procedure TRenderContext.SetUniform(const Name: string; const X, Y: Float);
var
  L: Integer;
begin
  if GetUniform(Name, L) then
    glUniform2f(L, X, Y);
end;

procedure TRenderContext.SetUniform(Location: Integer; const X, Y, Z: Float);
begin
  glUniform3f(Location, X, Y, Z);
end;

procedure TRenderContext.SetUniform(const Name: string; const X, Y, Z: Float);
var
  L: Integer;
begin
  if GetUniform(Name, L) then
    glUniform3f(L, X, Y, Z);
end;

procedure TRenderContext.SetUniform(Location: Integer; const X, Y, Z, W: Float);
begin
  glUniform4f(Location, X, Y, Z, W);
end;

procedure TRenderContext.SetUniform(const Name: string; const X, Y, Z, W: Float);
var
  L: Integer;
begin
  if GetUniform(Name, L) then
    glUniform4f(L, X, Y, Z, W);
end;

procedure TRenderContext.SetUniform(Location: Integer; const V: TVec2);
begin
  SetUniform(Location, V.X, V.Y);
end;

procedure TRenderContext.SetUniform(const Name: string; const V: TVec2); overload;
begin
  SetUniform(Name, V.X, V.Y);
end;

procedure TRenderContext.SetUniform(Location: Integer; const V: TVec3); overload;
begin
  SetUniform(Location, V.X, V.Y, V.Z);
end;

procedure TRenderContext.SetUniform(const Name: string; const V: TVec3); overload;
begin
  SetUniform(Name, V.X, V.Y, V.Z);
end;

procedure TRenderContext.SetUniform(Location: Integer; const V: TVec4); overload;
begin
  SetUniform(Location, V.X, V.Y, V.Z, V.W);
end;

procedure TRenderContext.SetUniform(const Name: string; const V: TVec4); overload;
begin
  SetUniform(Name, V.X, V.Y, V.Z, V.W);
end;

procedure TRenderContext.SetUniform(Location: Integer; const M: TMatrix); overload;
begin
  glUniformMatrix4fv(Location, 1, GL_FALSE, @M);
end;

procedure TRenderContext.SetUniform(const Name: string; const M: TMatrix); overload;
var
  L: Integer;
begin
  if GetUniform(Name, L) then
    glUniformMatrix4fv(L, 1, GL_FALSE, @M);
end;
{$endregion}

{$region textures}
function TRenderContext.GetTextureSlot: Integer;
begin
  glGetIntegerv(GL_ACTIVE_TEXTURE, @Result);
end;

procedure TRenderContext.SetTextureSlot(Slot: Integer);
begin
  glActiveTexture(GL_TEXTURE0 + Slot);
end;

function TRenderContext.GetTexture: Integer;
begin
  glGetIntegerv(GL_TEXTURE_BINDING_2D, @Result);
end;

procedure TRenderContext.PushTexture(Texture: Integer; Slot: Integer = 0);
var
  Item: TTextureItem;
begin
  Item.Texture := Texture;
  Item.Slot := Slot;
  glActiveTexture(GL_TEXTURE0 + Slot);
  { Remember the texture bound to the slot so it can be restored }
  glGetIntegerv(GL_TEXTURE_BINDING_2D, @Item.Previous);
  FTextureStack.Push(Item);
  glBindTexture(GL_TEXTURE_2D, Texture);
end;

procedure TRenderContext.PopTexture;
var
  Item: TTextureItem;
begin
  if FTextureStack.IsEmpty then
    Exit;
  Item := FTextureStack.Pop;
  glActiveTexture(GL_TEXTURE0 + Item.Slot);
  glBindTexture(GL_TEXTURE_2D, Item.Previous);
  { Leave the first slot active as is expected by code which binds textures
    without a slot }
  if Item.Slot <> 0 then
    glActiveTexture(GL_TEXTURE0);
end;
{$endregion}

{$region matrix stacks}
procedure TRenderContext.SetModelview(constref M: TMatrix);
begin
  FModelviewCurrent := M;
  FMatrixChange := True;
end;

function TRenderContext.GetModelview: TMatrix;
begin
  Result := FModelviewCurrent;
end;

procedure TRenderContext.PushModelview(const M: TMatrix);
begin
  FModelviewCurrent := M;
  FModelviewStack.Push(M);
  FMatrixChange := True;
end;

procedure TRenderContext.PopModelview;
begin
  if FModelviewStack.IsEmpty then
    Exit;
  FModelviewStack.Pop;
  FMatrixChange := True;
end;

procedure TRenderContext.LookAt(Eye, Center, Up: TVec3);
begin
  FModelviewCurrent.LookAt(Eye, Center, Up);
  FMatrixChange := True;
end;

procedure TRenderContext.Identity;
begin
  FModelviewCurrent.Identity;
  FMatrixChange := True;
end;

procedure TRenderContext.Transform(constref T: TMatrix);
begin
  FModelviewCurrent := FModelviewCurrent * T;
  FMatrixChange := True;
end;

procedure TRenderContext.Translate(X, Y, Z: Float);
begin
  FModelviewCurrent.Translate(X, Y, Z);
  FMatrixChange := True;
end;

procedure TRenderContext.Rotate(X, Y, Z: Float; Order: TRotationOrder = roZXY);
begin
  FModelviewCurrent.Rotate(X, Y, Z, Order);
  FMatrixChange := True;
end;

procedure TRenderContext.Scale(X, Y, Z: Float);
begin
  FModelviewCurrent.Scale(X, Y, Z);
  FMatrixChange := True;
end;

procedure TRenderContext.SetProjection(constref M: TMatrix);
begin
  FProjectionCurrent := M;
  FMatrixChange := True;
end;

function TRenderContext.GetProjection: TMatrix;
begin
  Result := FProjectionCurrent;
end;

procedure TRenderContext.PushProjection(const M: TMatrix);
begin
  FProjectionCurrent := M;
  FProjectionStack.Push(M);
  FMatrixChange := True;
end;

procedure TRenderContext.PopProjection;
begin
  if FProjectionStack.IsEmpty then
    Exit;
  FProjectionCurrent := FProjectionStack.Pop;
  FMatrixChange := True;
end;

procedure TRenderContext.Perspective(FoV, AspectRatio, NearPlane, FarPlane: Float);
begin
  FProjectionCurrent.Perspective(FoV, AspectRatio, NearPlane, FarPlane);
  FMatrixChange := True;
end;

procedure TRenderContext.Frustum(Left, Right, Top, Bottom, NearPlane, FarPlane: Float);
begin
  FProjectionCurrent.Frustum(Left, Right, Top, Bottom, NearPlane, FarPlane);
  FMatrixChange := True;
end;

procedure TRenderContext.SetProgramMatrix;
begin
  if FProgramChange or FMatrixChange then
  begin
    SetUniform('projection', FProjectionCurrent);
    SetUniform('modelview', FModelviewCurrent);
    FProgramChange := False;
    FMatrixChange := False;
  end;
end;
{$endregion}

end.

