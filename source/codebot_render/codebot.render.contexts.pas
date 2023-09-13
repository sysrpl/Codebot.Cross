unit Codebot.Render.Contexts;

{$i render.inc}

interface

uses
  SysUtils, Classes,
  Codebot.System,
  Codebot.Graphics,
  Codebot.Graphics.Types,
  Codebot.GLES,
  Codebot.Geometry;

type
  EContextError = class(Exception);
  EContextAssetError = class(EContextError);
  EContextCollectionError = class(EContextError);
  EOpenGLError = class(Exception);

  TContextCollection = class;

{ TContextManagedObject provides a way to manage the lifetime of objects such as shaders,
  textures, and vertex buffers. If no collection is given to the constructor create then
  the object will be maintained by Ctx.Objects. }

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

{ The TContext class provides an interface to all rendering in this library }

  TContext = class
  private type
    TTextureItem = record
      Texture: Integer;
      Slot: Integer;
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
  private
    { Add a collection or raise an EContextCollectionError exeption if the
      name is blank or already exists }
    procedure AddCollection(Collection: TContextCollection);
  public
    constructor Create;
    destructor Destroy; override;
    {$region general context methods and rendering options}
    { Make the context current or not current }
    procedure MakeCurrent(Current: Boolean);
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
    procedure SaveToBitmap(Bitmap: IBitmap);
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

{ Ctx returns the current TContext or throws EContextError if there is none }

function Ctx: TContext;

resourcestring
  SNoOpenGL = 'The OpenGL library could not be loaded';
  SNoContext = 'No context is available';
  SAssetNotFound = 'Cannot locate asset with name ''%s''';
  SAssetNotUnderstood = 'Cannot understand asset with name ''%s''';
  SNoCollectionName = 'Cannot add unnammed collections';
  SDuplicateCollectionName = 'An collection or item with name ''%s'' already exist';


implementation

var
  InternalContext: TObject;

function Ctx: TContext;
begin
  if InternalContext = nil then
    raise EContextError.Create(SNoContext);
  Result := TContext(InternalContext);
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
  C, N: TContextManagedObject;
begin
  C := FCollection.FNext;
  if C = nil then
    Exit;
  N := nil;
  while C <> Self do
  begin
    N := C;
    C := N.FNext;
  end;
  if N = nil then
    FCollection.FNext := FNext
  else
    N.FNext := FNext;
  inherited Destroy;
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
var
  C, N: TContextManagedObject;
begin
  C := FNext;
  FNext := nil;
  while C <> nil do
  begin
    N := C.FNext;
    C.Free;
    C := N;
  end;
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

{ TContext }

constructor TContext.Create;
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

destructor TContext.Destroy;
var
  C, N: TContextCollection;
begin
  InternalContext := nil;
  C := FCollection;
  while C <> nil do
  begin
    N := C.FNextCollection;
    C.Free;
    C := N;
  end;
  inherited Destroy;
end;

procedure TContext.AddCollection(Collection: TContextCollection);
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
procedure TContext.MakeCurrent(Current: Boolean);
begin
  if Current then
    InternalContext := Self
  else
    InternalContext := nil;
end;

procedure TContext.SetClearColor(R, G, B, A: Float);
begin
  glClearColor(R, G, B, A);
end;

procedure TContext.Clear;
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
end;

procedure TContext.PushCulling(Cull: Boolean);
begin
  FCullStack.Push(FCull);
  FCull := Cull;
  if FCull then
    glEnable(GL_CULL_FACE)
  else
    glDisable(GL_CULL_FACE);
end;

procedure TContext.PopCulling;
begin
  if FCullStack.Index < 0 then
    Exit;
  FCull := FCullStack.Pop;
  if FCull then
    glEnable(GL_CULL_FACE)
  else
    glDisable(GL_CULL_FACE);
end;

procedure TContext.PushDepthTesting(DepthTest: Boolean);
begin
  FDepthTestStack.Push(FDepthTest);
  FDepthTest := DepthTest;
  if FDepthTest then
    glEnable(GL_DEPTH_TEST)
  else
    glDisable(GL_DEPTH_TEST);
end;

procedure TContext.PopDepthTesting;
begin
  if FDepthTestStack.Index < 0 then
    Exit;
  FDepthTest := FDepthTestStack.Pop;
  if FDepthTest then
    glEnable(GL_DEPTH_TEST)
  else
    glDisable(GL_DEPTH_TEST);
end;

procedure TContext.PushDepthWriting(DepthWriting: Boolean);
begin
  FDepthWritingStack.Push(FDepthWriting);
  FDepthWriting := DepthWriting;
  if FDepthWriting then
    glDepthMask(GL_TRUE)
  else
    glDepthMask(GL_FALSE);
end;

procedure TContext.PopDepthWriting;
begin
  if FDepthWritingStack.Index < 0 then
    Exit;
  FDepthWriting := FDepthWritingStack.Pop;
  if FDepthWriting then
    glDepthMask(GL_TRUE)
  else
    glDepthMask(GL_FALSE);
end;

function TContext.GetViewport: TRectI;
begin
  Result := FViewport;
end;

procedure TContext.SetViewport(X, Y, W, H: Integer);
begin
  FViewport := TRectI.Create(X, Y, W, H);
  glViewport(X, Y, W, H);
end;

procedure TContext.PushViewport(X, Y, W, H: Integer);
begin
  FViewportStack.Push(FViewport);
  FViewport := TRectI.Create(X, Y, W, H);
  glViewport(X, Y, W, H);
end;

procedure TContext.PopViewport;
begin
  if FViewportStack.Index < 0 then
    Exit;
  FViewport := FViewportStack.Pop;
  glViewport(FViewport.X, FViewport.Y, FViewport.Width, FViewport.Height);
end;

function TContext.GetWorld: TContextManagedObject;
begin
  Result := FWorld;
end;

procedure TContext.SetWorld(Value: TContextManagedObject);
begin
  FWorld := Value;
end;

procedure TContext.SaveToBitmap(Bitmap: IBitmap);
begin
  Bitmap.SetSize(FViewport.Width, FViewport.Height);
  glReadPixels(FViewport.X, FViewport.Y, FViewport.Width, FViewport.Height,
    GL_UNSIGNED_BYTE, GL_RGBA, Bitmap.Pixels);
end;

procedure TContext.SaveToStream(Stream: TStream);
var
  B: IBitmap;
begin
  B := NewBitmap;
  SaveToBitmap(B);
  B.Format := fmPng;
  B.SaveToStream(Stream);
end;

procedure TContext.SaveToFile(const FileName: string);
var
  B: IBitmap;
begin
  B := NewBitmap;
  SaveToBitmap(B);
  B.SaveToFile(FileName);
end;
{$endregion}

{$region assets and collections}
function TContext.GetAssetStream(const Name: string): TStream;
var
  S: string;
begin
  if ResLoadData(Name, Result) then
    Exit;
  S := GetAssetFile(Name);
  Result := TFileStream.Create(S, fmOpenRead);
end;

function TContext.GetAssetFile(const FileName: string): string;
var
  S: string;
  I: Integer;
begin
  Result := '';
  S := PathCombine(FAssetFolder, FileName);
  I := 0;
  while I < 10 do
    if FileExists(S) then
      Exit(S)
    else
    begin
      Inc(I);
      S := PathCombine('..', S);
    end;
  raise EContextAssetError.CreateFmt(SAssetNotFound, [FileName]);
end;

procedure TContext.SetAssetFolder(const Folder: string);
begin
  FAssetFolder := Folder;
end;

function TContext.GetCollection(const Name: string): TContextCollection;
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

function TContext.Objects: TManagedObjectCollection;
begin
  if FObjects = nil then
    FObjects := TManagedObjectCollection.Create(SManagedObjectCollection);
  REsult := FObjects;
end;

{$endregion}

{$region program shader stack}
function TContext.GetProgram: Integer;
begin
  glGetIntegerv(GL_CURRENT_PROGRAM, @Result);
end;

procedure TContext.PushProgram(Prog: Integer);
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

procedure TContext.PopProgram;
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

function TContext.GetUniform(Prog: Integer; Name: string; out Location: Integer): Boolean;
begin
  Location := glGetUniformLocation(Prog, PChar(Name));
  Result := (Location > -1) and (Location < GL_INVALID_ENUM);
end;

function TContext.GetUniform(const Name: string; out Location: Integer): Boolean;
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

procedure TContext.SetUniform(Location: Integer; const B: Boolean);
begin
  if B then
    SetUniform(Location, 1)
  else
    SetUniform(Location, 0);
end;

procedure TContext.SetUniform(const Name: string; const B: Boolean);
begin
  if B then
    SetUniform(Name, 1)
  else
    SetUniform(Name, 0);
end;

procedure TContext.SetUniform(Location: Integer; const I: Integer);
begin
  glUniform1i(Location, I);
end;

procedure TContext.SetUniform(const Name: string; const I: Integer);
var
  L: Integer;
begin
  if GetUniform(Name, L) then
    glUniform1i(L, I);
end;

procedure TContext.SetUniform(Location: Integer; const X: Float);
begin
  glUniform1f(Location, X);
end;

procedure TContext.SetUniform(const Name: string; const X: Float);
var
  L: Integer;
begin
  if GetUniform(Name, L) then
    glUniform1f(L, X);
end;

procedure TContext.SetUniform(Location: Integer; const A: TArray<Float>);
begin
  glUniform1fv(Location, Length(A), @A[0]);
end;

procedure TContext.SetUniform(const Name: string; const A: TArray<Float>);
var
  L: Integer;
begin
  if GetUniform(Name, L) then
    glUniform1fv(L, Length(A), @A[0]);
end;

procedure TContext.SetUniform(Location: Integer; const X, Y: Float);
begin
  glUniform2f(Location, X, Y);
end;

procedure TContext.SetUniform(const Name: string; const X, Y: Float);
var
  L: Integer;
begin
  if GetUniform(Name, L) then
    glUniform2f(L, X, Y);
end;

procedure TContext.SetUniform(Location: Integer; const X, Y, Z: Float);
begin
  glUniform3f(Location, X, Y, Z);
end;

procedure TContext.SetUniform(const Name: string; const X, Y, Z: Float);
var
  L: Integer;
begin
  if GetUniform(Name, L) then
    glUniform3f(L, X, Y, Z);
end;

procedure TContext.SetUniform(Location: Integer; const X, Y, Z, W: Float);
begin
  glUniform4f(Location, X, Y, Z, W);
end;

procedure TContext.SetUniform(const Name: string; const X, Y, Z, W: Float);
var
  L: Integer;
begin
  if GetUniform(Name, L) then
    glUniform4f(L, X, Y, Z, W);
end;

procedure TContext.SetUniform(Location: Integer; const V: TVec2);
begin
  SetUniform(Location, V.X, V.Y);
end;

procedure TContext.SetUniform(const Name: string; const V: TVec2); overload;
begin
  SetUniform(Name, V.X, V.Y);
end;

procedure TContext.SetUniform(Location: Integer; const V: TVec3); overload;
begin
  SetUniform(Location, V.X, V.Y, V.Z);
end;

procedure TContext.SetUniform(const Name: string; const V: TVec3); overload;
begin
  SetUniform(Name, V.X, V.Y, V.Z);
end;

procedure TContext.SetUniform(Location: Integer; const V: TVec4); overload;
begin
  SetUniform(Location, V.X, V.Y, V.Z, V.W);
end;

procedure TContext.SetUniform(const Name: string; const V: TVec4); overload;
begin
  SetUniform(Name, V.X, V.Y, V.Z, V.W);
end;

procedure TContext.SetUniform(Location: Integer; const M: TMatrix); overload;
begin
  glUniformMatrix4fv(Location, 1, GL_FALSE, @M);
end;

procedure TContext.SetUniform(const Name: string; const M: TMatrix); overload;
var
  L: Integer;
begin
  if GetUniform(Name, L) then
    glUniformMatrix4fv(L, 1, GL_FALSE, @M);
end;
{$endregion}

{$region textures}
function TContext.GetTextureSlot: Integer;
begin
  glGetIntegerv(GL_ACTIVE_TEXTURE, @Result);
end;

procedure TContext.SetTextureSlot(Slot: Integer);
begin
  glActiveTexture(GL_TEXTURE0 + Slot);
end;

function TContext.GetTexture: Integer;
begin
  glGetIntegerv(GL_TEXTURE_BINDING_2D, @Result);
end;

procedure TContext.PushTexture(Texture: Integer; Slot: Integer = 0);
var
  Item: TTextureItem;
begin
  Item.Texture := Texture;
  Item.Slot := Slot;
  FTextureStack.Push(Item);
  glActiveTexture(GL_TEXTURE0 + Slot);
  glBindTexture(GL_TEXTURE_2D, Texture);
end;

procedure TContext.PopTexture;
var
  Item: TTextureItem;
begin
  if FTextureStack.IsEmpty then
    Exit;
  Item := FTextureStack.Pop;
  glActiveTexture(GL_TEXTURE0 + Item.Slot);
  glBindTexture(GL_TEXTURE_2D, Item.Texture);
end;
{$endregion}

{$region matrix stacks}
procedure TContext.SetModelview(constref M: TMatrix);
begin
  FModelviewCurrent := M;
  FMatrixChange := True;
end;

function TContext.GetModelview: TMatrix;
begin
  Result := FModelviewCurrent;
end;

procedure TContext.PushModelview(const M: TMatrix);
begin
  FModelviewCurrent := M;
  FModelviewStack.Push(M);
  FMatrixChange := True;
end;

procedure TContext.PopModelview;
begin
  if FModelviewStack.IsEmpty then
    Exit;
  FModelviewStack.Pop;
  FMatrixChange := True;
end;

procedure TContext.LookAt(Eye, Center, Up: TVec3);
begin
  FModelviewCurrent.LookAt(Eye, Center, Up);
  FMatrixChange := True;
end;

procedure TContext.Identity;
begin
  FModelviewCurrent.Identity;
  FMatrixChange := True;
end;

procedure TContext.Transform(constref T: TMatrix);
begin
  FModelviewCurrent := FModelviewCurrent * T;
  FMatrixChange := True;
end;

procedure TContext.Translate(X, Y, Z: Float);
begin
  FModelviewCurrent.Translate(X, Y, Z);
  FMatrixChange := True;
end;

procedure TContext.Rotate(X, Y, Z: Float; Order: TRotationOrder = roZXY);
begin
  FModelviewCurrent.Rotate(X, Y, Z, Order);
  FMatrixChange := True;
end;

procedure TContext.Scale(X, Y, Z: Float);
begin
  FModelviewCurrent.Scale(X, Y, Z);
  FMatrixChange := True;
end;

procedure TContext.SetProjection(constref M: TMatrix);
begin
  FProjectionCurrent := M;
  FMatrixChange := True;
end;

function TContext.GetProjection: TMatrix;
begin
  Result := FProjectionCurrent;
end;

procedure TContext.PushProjection(const M: TMatrix);
begin
  FProjectionCurrent := M;
  FProjectionStack.Push(M);
  FMatrixChange := True;
end;

procedure TContext.PopProjection;
begin
  if FProjectionStack.IsEmpty then
    Exit;
  FProjectionCurrent := FProjectionStack.Pop;
  FMatrixChange := True;
end;

procedure TContext.Perspective(FoV, AspectRatio, NearPlane, FarPlane: Float);
begin
  FProjectionCurrent.Perspective(FoV, AspectRatio, NearPlane, FarPlane);
  FMatrixChange := True;
end;

procedure TContext.Frustum(Left, Right, Top, Bottom, NearPlane, FarPlane: Float);
begin
  FProjectionCurrent.Frustum(Left, Right, Top, Bottom, NearPlane, FarPlane);
  FMatrixChange := True;
end;

procedure TContext.SetProgramMatrix;
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

