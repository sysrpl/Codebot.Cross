unit Codebot.Render.Contexts;

{$mode delphi}

interface

uses
  SysUtils, Classes,
  Codebot.System,
  Codebot.Graphics,
  Codebot.Graphics.Types,
  Codebot.Geometry;

type
  EContextError = class(Exception);
  EContextAssetError = class(EContextError);
  EContextCollectionError = class(EContextError);
  TContextCollection = class;

{ TContextItem provides a way to manmge the lifetime of objects such as shaders,
  textures, and vertex buffers. When an item has a name it is managed by an
  associated collection. If there is no name then you must manage the lifetime
  of the item. }

  TContextItem = class
  private
    FName: string;
    FCollection: TContextCollection;
    FNext: TContextItem;
    procedure SetName(const Value: string);
  public
    { Create a new item optionally managing its lifetime with a collection and name }
    constructor Create(Collection: TContextCollection; const Name: string = '');
    { Destroy automatically removes the item from its collection }
    destructor Destroy; override;
    { Setting the name adds to or removes the item from a collection }
    property Name: string read FName write SetName;
  end;

{ Other units may define TContextCollection extensions to provide managed
  access to objects such as shaders, textures, vertex buffers, and so on.

  Class helpers can add to a context using functions such as:

    function TShaderExtention.Shaders: TShaderCollection;
    function TTextureExtention.Textures: TTextureCollection; }

  TContextCollection = class
  private
    FName: string;
    FNextCollection: TContextCollection;
    FNext: TContextItem;
  protected
    function GetItem(const Name: string): TContextItem;
  public
    { Collection name must not be blank and must be unique }
    constructor Create(const Name: string);
    destructor Destroy; override;
    { The read only name of the collection }
    property Name: string read FName;
  end;

{ The TContext class provides an interface to all rendering in this library }

  TContext = class
  private type
    TTextureItem = record
      Texture: Integer;
      Slot: Integer;
    end;
    TTextureStack = TArrayList<TTextureItem>;
    TMatrixStack = TArrayList<TMatrix4x4>;
    TViewportStack = TArrayList<TRectI>;
  private var
    FAssetFolder: string;
    FProgramStack: IntArray;
    FProgramCount: IntArray;
    FProgramChange: Boolean;
    FViewport: TRectI;
    FViewportStack: TViewportStack;
    FCollection: TContextCollection;
    FTextureStack: TTextureStack;
    FModelviewStack: TMatrixStack;
    FModelviewCurrent: TMatrix4x4;
    FProjectionStack: TMatrixStack;
    FProjectionCurrent: TMatrix4x4;
    FMatrixChange: Boolean;
  private
    { Add a collection or raise an EContextCollectionError exeption if the
      name is blank or already exists }
    procedure AddCollection(Collection: TContextCollection);
  public
    constructor Create;
    destructor Destroy; override;
    {$region general context methods}
    { Make the context current or not current }
    procedure MakeCurrent(Current: Boolean);
    { Set the color to use when cleared }
    procedure SetClearColor(R, G, B, A: Float);
    { Clear the color and depth buffer bits }
    procedure Clear;
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
    { Save the current viewport contents to a bitmap }
    procedure SaveToBitmap(Bitmap: IBitmap);
    { Save the current viewport contents to a bitmap stream }
    procedure SaveToStream(Stream: TStream);
    { Save the current viewport contents to a bitmap file }
    procedure SaveToFile(const FileName: string);
    {$endregion}
    {$region assets and collections}
    { Search upwards for an asset returning the valid filename or raise
      an EContextAssetError exception }
    function GetAssetFile(const FileName: string): string;
    { Set the asset folder name, which defaults to 'assets' }
    procedure SetAssetFolder(const Folder: string);
    { Returns a collection by name }
    function GetCollection(const Name: string): TContextCollection;
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
    procedure SetUniform(Location: Integer; const M: TMatrix4x4); overload;
    procedure SetUniform(const Name: string; const M: TMatrix4x4); overload;
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
    { Returns the current modelview matrix }
    function GetModelview: TMatrix4x4;
    { Adds a new modelview matrix on to the stack }
    procedure PushModelview(const M: TMatrix4x4);
    { Removes the most recent modelview matrix from the stack }
    procedure PopModelview;
    { Replaces the current model view matrix with a look at matrix }
    procedure LookAt(Eye, Center, Up: TVec3);
    { Replaces the current model view matrix with an identity matrix }
    procedure Identity;
    { Translate the current model view matrix }
    procedure Translate(X, Y, Z: Float);
    { Rotate the current model view matrix }
    procedure Rotate(X, Y, Z: Float; Order: TRotationOrder = roZXY);
    { Scale the current model view matrix }
    procedure Scale(X, Y, Z: Float);
    { Returns the current projection matrix }
    function GetProjection: TMatrix4x4;
    { Adds a new projection matrix to the stack }
    procedure PushProjection(const M: TMatrix4x4);
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

implementation

uses
  Codebot.GLES;

resourcestring
  SNoOpenGL = 'The OpenGL library could not be loaded';
  SNoContext = 'No context is available';
  SAssetNotFound = 'Cannot locate asset with name ''%s''';
  SNoCollection = 'Cannot set name of type ''%s'' without a parent collection';
  SNoCollectionName = 'Cannot add unnammed collections';
  SDuplicateCollectionName = 'An collection or item with name ''%s'' already exist';

var
  InternalContext: TObject;

function Ctx: TContext;
begin
  if InternalContext = nil then
    raise EContextError.Create(SNoContext);
  Result := TContext(InternalContext);
end;

{ TContextItem }

constructor TContextItem.Create(Collection: TContextCollection; const Name: string = '');
begin
  inherited Create;
  FCollection := Collection;
  SetName(Name);
end;

destructor TContextItem.Destroy;
var
  C, N: TContextItem;
begin
  if FName <> '' then
  begin
    C := FCollection.FNext;
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
  end;
  inherited Destroy;
end;

procedure TContextItem.SetName(const Value: string);
var
  C: TContextItem;
begin
  if Value = FName then
    Exit;
  if FCollection = nil then
    raise EContextCollectionError.CreateFmt(SNoCollection, [ClassName]);
  C := FCollection.FNext;
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
  C, N: TContextItem;
begin
  C := FNext;
  while C <> nil do
  begin
    N := C.FNext;
    C.FName := '';
    C.Free;
    C := N;
  end;
  inherited Destroy;
end;

function TContextCollection.GetItem(const Name: string): TContextItem;
var
  C: TContextItem;
begin
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
begin
  inherited Create;
  if not LoadOpenGLES then
    raise EContextError.Create(SNoOpenGL);
  InternalContext := Self;
  FAssetFolder := 'assets';
  FModelviewCurrent.Identity;
  FProjectionCurrent.Identity;
  FMatrixChange := True;
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
  if FViewportStack.Length < 1 then
    Exit;
  FViewport := FViewportStack.Pop;
  glViewport(FViewport.X, FViewport.Y, FViewport.Width, FViewport.Height);
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
  F: Boolean;
begin
  B := NewBitmap;
  SaveToBitmap(B);
  B.Format := fmPng;
  F := SurfaceOptions.AllowPixelFlip;
  SurfaceOptions.AllowPixelFlip := False;
  B.SaveToStream(Stream);
  SurfaceOptions.AllowPixelFlip := F;
end;

procedure TContext.SaveToFile(const FileName: string);
var
  B: IBitmap;
  F: Boolean;
begin
  B := NewBitmap;
  SaveToBitmap(B);
  F := SurfaceOptions.AllowPixelFlip;
  SurfaceOptions.AllowPixelFlip := False;
  B.SaveToFile(FileName);
  SurfaceOptions.AllowPixelFlip := F;
end;

{$endregion}

{$region assets and collections}
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

procedure TContext.SetUniform(Location: Integer; const M: TMatrix4x4); overload;
begin
  glUniformMatrix4fv(Location, 1, GL_FALSE, @M);
end;

procedure TContext.SetUniform(const Name: string; const M: TMatrix4x4); overload;
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
  FTextureStack.Pop;
  if FTextureStack.IsEmpty then
    Exit;
  Item := FTextureStack.Last;
  glActiveTexture(GL_TEXTURE0 + Item.Slot);
  glBindTexture(GL_TEXTURE_2D, Item.Texture);
end;
{$endregion}

{$region matrix stacks}
function TContext.GetModelview: TMatrix4x4;
begin
  Result := FModelviewCurrent;
end;

procedure TContext.PushModelview(const M: TMatrix4x4);
begin
  FModelviewCurrent := M;
  FModelviewStack.Push(M);
  FMatrixChange := True;
end;

procedure TContext.PopModelview;
begin
  FModelviewStack.Pop;
  FMatrixChange := True;
end;

procedure TContext.LookAt(Eye, Center, Up: TVec3);
begin
  FModelviewCurrent.LookAt(Eye, Center, Up);
  FModelviewStack.Last := FModelviewCurrent;
  FMatrixChange := True;
end;

procedure TContext.Identity;
begin
  FModelviewCurrent.Identity;
  FModelviewStack.Last := FModelviewCurrent;
  FMatrixChange := True;
end;

procedure TContext.Translate(X, Y, Z: Float);
begin
  FModelviewCurrent.Translate(X, Y, Z);
  FModelviewStack.Last := FModelviewCurrent;
  FMatrixChange := True;
end;

procedure TContext.Rotate(X, Y, Z: Float; Order: TRotationOrder = roZXY);
begin
  FModelviewCurrent.Rotate(X, Y, Z, Order);
  FModelviewStack.Last := FModelviewCurrent;
  FMatrixChange := True;
end;

procedure TContext.Scale(X, Y, Z: Float);
begin
  FModelviewCurrent.Scale(X, Y, Z);
  FModelviewStack.Last := FModelviewCurrent;
  FMatrixChange := True;
end;

function TContext.GetProjection: TMatrix4x4;
begin
  Result := FProjectionCurrent;
end;

procedure TContext.PushProjection(const M: TMatrix4x4);
begin
  FProjectionCurrent := M;
  FProjectionStack.Push(M);
  FMatrixChange := True;
end;

procedure TContext.PopProjection;
begin
  FProjectionStack.Pop;
  FMatrixChange := True;
end;

procedure TContext.Perspective(FoV, AspectRatio, NearPlane, FarPlane: Float);
begin
  FProjectionCurrent.Perspective(FoV, AspectRatio, NearPlane, FarPlane);
  FProjectionStack.Last := FProjectionCurrent;
  FMatrixChange := True;
end;

procedure TContext.Frustum(Left, Right, Top, Bottom, NearPlane, FarPlane: Float);
begin
  FProjectionCurrent.Frustum(Left, Right, Top, Bottom, NearPlane, FarPlane);
  FProjectionStack.Last := FProjectionCurrent;
end;

procedure TContext.SetProgramMatrix;
begin
  if FProgramChange or FMatrixChange then
  begin
    SetUniform('modelview', FModelviewCurrent);
    SetUniform('projection', FProjectionCurrent);
    FProgramChange := False;
    FMatrixChange := False;
  end;
end;
{$endregion}

end.

