unit Codebot.Render.World;

{$i codebot.inc}

interface

uses
  Codebot.System,
  Codebot.Animation,
  Codebot.Geometry,
  Codebot.Graphics,
  Codebot.Graphics.Types,
  Codebot.Render.Contexts,
  Codebot.Render.Textures,
  Codebot.Render.Buffers;

{ TCamera allows you to position a perspective and move within 3D space. A
  camera is provided by TWorld. }

type
  TCamera = class(TContextManagedObject, IPropertyResolver)
  private
    FDirection: TVec3Prop;
    FPosition: TVec3Prop;
    procedure GetMatrix(out Matrix: TMatrix4x4);
    procedure Move(const Normal: TVec3; Distance: Float);
    procedure SetDirection(const Value: TVec3Prop);
    procedure SetPosition(const Value: TVec3Prop);
    function Resolve(const Name: string; out Prop: TVectorProperty): Boolean;
  public
    { Camera is create a TWorld }
    constructor Create;
    destructor Destroy; override;
    { Apply the camera orientation to the current view matrix }
    procedure ApplyDirection;
    { Apply the camera vantage point to the current view matrix }
    procedure ApplyPosition;
    { Move forward a distance along the direction }
    procedure MoveForward(Distance: Float);
    { Move backward a distance along the direction }
    procedure MoveBackward(Distance: Float);
    { Move left a distance perpedicular to the direction }
    procedure MoveLeft(Distance: Float);
    { Move right a distance perpedicular to the direction }
    procedure MoveRight(Distance: Float);
    { The heading, pitch, and roll orientation of the camera }
    property Direction: TVec3Prop read FDirection write SetDirection;
    { The vantage point of the camera }
    property Position: TVec3Prop read FPosition write SetPosition;
  end;

{ TSkybox is a texture mapped an inward facing cube use to simulate the background
  of 3D space. A skybox is provided by TWorld. }

  TSkybox = class(TContextManagedObject)
  private
    FBoxBuffer: TTexVertexBuffer;
    FTexture: TTexture;
  public
    { Skybox is create a TWorld }
    constructor Create;
    destructor Destroy; override;
    { Draw the skybox centered on zero }
    procedure Draw(Time: Double = 0); virtual;
    { The skybox texture }
    property Texture: TTexture read FTexture;
  end;

{ TWorld provides a 2D world in a 3D space. It also gives you an optional
  camera and skybox }

  TWorld = class(TContextManagedObject)
  private
    FCamera: TCamera;
    FSkybox: TSkybox;
    FWidth: Integer;
    FHeight: Integer;
    FDepth: Float;
    FFieldOfView: Float;
    FNearPlane: Float;
    FFarPlane: Float;
    FAspect: Float;
    FScale: TVec2;
    FOffset: TVec2;
    FTangent: Float;
    FHalfWidth: Float;
    FHalfHeight: Float;
    FRatio: Float;
    FGrid: TTexVertexBuffer;
    FGridTexture: TTexture;
    function GetCamera: TCamera;
    function GetSkybox: TSkybox;
  public
    { World is created for you by the TWorldExtension helper below }
    constructor Create;
    { Define the world virtual resolution and perspective. USeful defaults are
      provided representing a 1080p virtual resolution for your 2D world. }
    procedure Define(Width: Integer = 1920; Height: Integer = 1080; Depth: Float = -10;
      FieldOfView: Float = 60; NearPlane: Float = 0.5; FarPlane: Float = 100);
    { Update the perspective and apply camera and skybox if they exist }
    procedure Update(Time: Double = 0);
    { Convert a 3D space coordinate to a 2D world coordinate }
    function SpaceToWorld(X, Y, Z: Float): TVec2; overload;
    function SpaceToWorld(const V: TVec3): TVec2; overload;
    { Convert a 2D worldcoordinate to a 3D space coordinate }
    function WorldToSpace(X, Y: Float): TVec3; overload;
    function WorldToSpace(const V: TVec2): TVec3; overload;
    { Convert a space distance to a 2d world distance }
    function DepthToWorld(Depth: Float): Float;
    { Convert a 2d world distance to a space distance  }
    function WorldToDepth(Depth: Float): Float;
    { Draw a simple grid on a ground plane }
    procedure DrawGrid;
    { The virtual resolution width of world in a 2D plane }
    property Width: Integer read FWidth;
    { The virtual resolution height of world in a 2D plane }
    property Height: Integer read FHeight;
    { The distance from the origin of the world in a 2D plane }
    property Depth: Float read FDepth;
    { The field of view for the 3d space }
    property FieldOfView: Float read FFieldOfView;
    { The optional camera }
    property Camera: TCamera read GetCamera;
    { The optional skybox }
    property Skybox: TSkybox read GetSkybox;
  end;

{ TWorldExtension adds the function World to the current context }

  TWorldExtension = class helper for TContext
  public
    { Returns the world for the current context }
    function World: TWorld;
  end;

implementation

constructor TCamera.Create;
begin
  inherited Create(nil);
  FDirection.Link;
  FPosition.Link;
end;

destructor TCamera.Destroy;
begin
  FDirection.Unlink;
  FPosition.Unlink;
  inherited Destroy;
end;

procedure TCamera.ApplyDirection;
var
  V: TVec3;
begin
  V := FDirection;
  Ctx.Rotate(V.Pitch, V.Heading, V.Roll, roXYZ);
end;

procedure TCamera.ApplyPosition;
var
  V: TVec3;
begin
  V := FPosition;
  Ctx.Translate(-V.X, -V.Y, -V.Z);
end;

procedure TCamera.GetMatrix(out Matrix: TMatrix4x4);
var
  V: TVec3;
begin
  V := FDirection;
  Matrix := StockMatrix;
  Matrix.Rotate(-V.Pitch, -V.Heading, -V.Roll, roYXZ);
end;

procedure TCamera.Move(const Normal: TVec3; Distance: Float);
var
  M: TMatrix4x4;
  V, B: TVec3;
begin
  GetMatrix(M);
  V := Position;
  B := M * Normal;
  Position := V + (B * Distance);
end;

procedure TCamera.MoveForward(Distance: Float);
begin
  Move(Vec3(0, 0, -1), Distance);
end;

procedure TCamera.MoveBackward(Distance: Float);
begin
  Move(Vec3(0, 0, 1), Distance);
end;

procedure TCamera.MoveLeft(Distance: Float);
begin
  Move(Vec3(-1, 0, 0), Distance);
end;

procedure TCamera.MoveRight(Distance: Float);
begin
  Move(Vec3(1, 0, 0), Distance);
end;

procedure TCamera.SetDirection(const Value: TVec3Prop);
begin
  FDirection.Value := Value;
end;

procedure TCamera.SetPosition(const Value: TVec3Prop);
begin
  FPosition.Value := Value;
end;

function TCamera.Resolve(const Name: string; out Prop: TVectorProperty): Boolean;
begin
  Result := VectorPropertyEmpty(Prop);
  if StrEquals(Name, 'Direction') then
    Prop.Vec3Prop := Direction
  else if StrEquals(Name, 'Position') then
    Prop.Vec3Prop := Position
  else
    Exit;
  Result := True;
end;

{ TSkybox }

constructor TSkybox.Create;
var
  Index: Integer;

  procedure Vert(X, Y, Z, S, T: Float);
  var
    V: TTexVertex;
  begin
    V.Vertex := Vec3(X, Y, Z);
    V.TexCoord := Vec2(S / 2, T / 3);
    { There might be an issue with seams showing on some systems }
    if Byte(Index mod 4) in [0..1] then
      V.TexCoord.S := V.TexCoord.S
    else
      V.TexCoord.S := V.TexCoord.S;
    if Byte(Index mod 4) in [0, 3] then
      V.TexCoord.T := V.TexCoord.T
    else
      V.TexCoord.T := V.TexCoord.T;
    FBoxBuffer.Add(V);
    Inc(Index);
  end;

begin
  inherited Create(nil);
  FBoxBuffer := TTexVertexBuffer.Create;
  FBoxBuffer.BeginBuffer(vertQuads, 6);
  Index := 0;
  { Front }
  Vert(-2, 2, -2, 1, 1);
  Vert(-2, -2, -2, 1, 2);
  Vert(2, -2, -2, 2, 2);
  Vert(2, 2, -2, 2, 1);
  { Right }
  Vert(2, 2, -2, 0, 0);
  Vert(2, -2, -2, 0, 1);
  Vert(2, -2, 2, 1, 1);
  Vert(2, 2, 2, 1, 0);
  { Back }
  Vert(2, 2, 2, 0, 2);
  Vert(2, -2, 2, 0, 3);
  Vert(-2, -2, 2, 1, 3);
  Vert(-2, 2, 2, 1, 2);
  { Left }
  Vert(-2, 2, 2, 0, 1);
  Vert(-2, -2, 2, 0, 2);
  Vert(-2, -2, -2, 1, 2);
  Vert(-2, 2, -2, 1, 1);
  { Top }
  Vert(-2, 2, 2, 1, 0);
  Vert(-2, 2, -2, 1, 1);
  Vert(2, 2, -2, 2, 1);
  Vert(2, 2, 2, 2, 0);
  { Bottom }
  Vert(-2, -2, -2, 1, 2);
  Vert(-2, -2, 2, 1, 3);
  Vert(2, -2, 2, 2, 3);
  Vert(2, -2, -2, 2, 2);
  FBoxBuffer.EndBuffer(False);
  FTexture := TTexture.Create;
  FTexture.Wrap := False;
end;

destructor TSkybox.Destroy;
begin
  FBoxBuffer.Free;
  FTexture.Free;
  inherited Destroy;
end;

procedure TSkybox.Draw(Time: Double = 0);
const
  SpinFactor = 0.5;
begin
  if FTexture.Width < 1 then
    Exit;
  if Time <> 0 then
    Ctx.Rotate(0, Time * SpinFactor, 0);
  Ctx.PushDepthWriting(False);
  FTexture.Push;
  FBoxBuffer.Draw;
  FTexture.Pop;
  Ctx.PopDepthWriting;
  if Time <> 0 then
    Ctx.Rotate(0, Time * -SpinFactor, 0);
end;

{ TWorld }

constructor TWorld.Create;
begin
  inherited Create(nil);
  Define;
end;

procedure TWorld.Define(Width: Integer = 1920; Height: Integer = 1080; Depth: Float = -10;
  FieldOfView: Float = 60; NearPlane: Float = 0.5; FarPlane: Float = 100);
begin
  FWidth := Width;
  FHeight := Height;
  FDepth := Depth;
  FFieldOfView := FieldOfView;
  FNearPlane := NearPlane;
  FFarPlane := FarPlane;
end;

procedure TWorld.Update(Time: Double = 0);
var
  V: TRectI;
  A: Float;
begin
  Ctx.Clear;
  V := Ctx.GetViewport;
  FAspect := V.Width / V.Height;
  FOffset.X := 0;
  FOffset.Y := 0;
  A := FWidth / FHeight;
  if A > FAspect then
  begin
    FScale.X := V.Width / FWidth;
    FScale.Y := FScale.X;
    FOffset.Y := ((V.Height / FScale.Y) - Height) / 2;
  end
  else if A < FAspect then
  begin
    FScale.Y := V.Height / FHeight;
    FScale.X := FScale.Y;
    FOffset.X := ((V.Width / FScale.X) - Width) / 2;
  end
  else
  begin
    FScale.X := V.Width / FWidth;
    FScale.Y := FScale.X;
  end;
  FTangent := Tan(FFieldOfView / 360 * Pi);
  FHalfWidth := V.Width / 2;
  FHalfHeight := V.Height / 2;
  FRatio := (Abs(FDepth) * FTangent) / FHalfHeight;
  Ctx.Perspective(FFieldOfView, FAspect, FNearPlane, FFarPlane);
  Ctx.Identity;
  if FCamera <> nil then
  begin
    FCamera.ApplyDirection;
    if FSkybox <> nil then
      FSkybox.Draw(Time);
    FCamera.ApplyPosition;
  end
  else if FSkybox <> nil then
    FSkybox.Draw(Time);
end;

function TWorld.WorldToSpace(X, Y: Float): TVec3;
begin
  X := (X + FOffset.X) * FScale.X;
  Y := (Y + FOffset.Y) * FScale.Y;
  Result := Vec3(X - FHalfWidth, FHalfHeight - Y, FDepth);
  Result.X := Result.X * FRatio;
  Result.Y := Result.Y * FRatio;
end;

function TWorld.WorldToSpace(const V: TVec2): TVec3;
begin
  Result := WorldToSpace(V.X, V.Y);
end;

function TWorld.SpaceToWorld(X, Y, Z: Float): TVec2;
begin
  if (Z < 0) and (FTangent > 0) then
  begin
    Result.X := (X / Z / FTangent) * FHalfHeight;
    Result.X := FHalfWidth - Result.X;
    Result.Y := (Y / Z / FTangent) * FHalfHeight;
    Result.Y := FHalfHeight + Result.Y;
    { ?

    Result.X := (Result.X - FOffset.X) / FScale.X;
    Result.Y := (Result.Y - FOffset.Y) / FScale.Y; }
  end
  else
  begin
    Result.X := 0;
    Result.Y := 0;
  end;
end;

function TWorld.SpaceToWorld(const V: TVec3): TVec2;
begin
  Result := SpaceToWorld(V.X, V.Y, V.Z);
end;

function TWorld.DepthToWorld(Depth: Float): Float;
begin
  Result := Depth / WorldToSpace(Width / 2 + 1, 0).X;
end;

function TWorld.WorldToDepth(Depth: Float): Float;
begin
  Result := Depth * WorldToSpace(Width / 2 + 1, 0).X;
end;

procedure TWorld.DrawGrid;

  function GenerateBitmap: IBitmap;
  const
    TexSize = 128;
  var
    R: TRectI;
    G: IGradientBrush;
    C: TColorB;
    P: PPixel;
    I: Integer;
  begin
    R := TRectI.Create(TexSize, TexSize);
    Result := NewBitmap(R.Width, R.Height);
    G := NewBrush(R.TopLeft, R.BottomLeft);
    C := $FFFFFF;
    C.Alpha := 0;
    G.AddStop(C, 0);
    C.Alpha := $A0;
    G.AddStop(C, 0.2);
    G.AddStop(C, 0.8);
    C.Alpha := 0;
    G.AddStop(C, 1);
    Result.Surface.FillRect(G, R);
    P := Result.Pixels;
    for I := 1 to R.Width * R.Height do
    begin
      P.Red := $FF;
      P.Green := $FF;
      P.Blue := $FF;
      Inc(P);
    end;
  end;

const
  Ground = -6;
  GridSize = 16;
  GridHalf = GridSize div 2;
  GridSpace = 2;
  LineWidth = 0.1;
var
  V: TTexVertex;
  I: Integer;
begin
  if FGrid = nil then
  begin
    FGrid := TTexVertexBuffer.Create;
    FGrid.BeginBuffer(vertQuads, GridSize * GridSize);
    for I := -GridHalf to GridHalf do
    begin
      V.Vertex := Vec3(-GridHalf * GridSpace, Ground, I * GridSpace - LineWidth);
      V.TexCoord := Vec2(0, 0);
      FGrid.Add(V);
      V.Vertex := Vec3(-GridHalf * GridSpace, Ground, I * GridSpace + LineWidth);
      V.TexCoord := Vec2(0, 1);
      FGrid.Add(V);
      V.Vertex := Vec3(GridHalf * GridSpace, Ground, I * GridSpace + LineWidth);
      V.TexCoord := Vec2(1, 1);
      FGrid.Add(V);
      V.Vertex := Vec3(GridHalf * GridSpace, Ground, I * GridSpace - LineWidth);
      V.TexCoord := Vec2(1, 0);
      FGrid.Add(V);
    end;
    for I := -GridHalf to GridHalf do
    begin
      V.Vertex := Vec3(I * GridSpace + LineWidth, Ground, -GridHalf * GridSpace);
      V.TexCoord := Vec2(0, 0);
      FGrid.Add(V);
      V.Vertex := Vec3(I * GridSpace - LineWidth, Ground, -GridHalf * GridSpace);
      V.TexCoord := Vec2(0, 1);
      FGrid.Add(V);
      V.Vertex := Vec3(I * GridSpace - LineWidth, Ground, GridHalf * GridSpace);
      V.TexCoord := Vec2(1, 1);
      FGrid.Add(V);
      V.Vertex := Vec3(I * GridSpace + LineWidth, Ground, GridHalf * GridSpace);
      V.TexCoord := Vec2(1, 0);
      FGrid.Add(V);
    end;
    FGrid.EndBuffer(False);
    FGridTexture := TTexture.Create;
    FGridTexture.LoadFromBitmap(GenerateBitmap);
    FGridTexture.GenerateMipmaps;
  end;
  Ctx.PushDepthTesting(False);
  Ctx.PushCulling(False);
  FGridTexture.Push;
  FGrid.Draw;
  FGridTexture.Pop;
  Ctx.PopCulling;
  Ctx.PopDepthTesting;
end;

function TWorld.GetCamera: TCamera;
begin
  if FCamera = nil then
    FCamera := TCamera.Create;
  Result := FCamera;
end;

function TWorld.GetSkybox: TSkybox;
begin
  if FSkybox = nil then
    FSkybox := TSkybox.Create;
  Result := FSkybox;
end;

{ TWorldExtension }

function TWorldExtension.World: TWorld;
begin
  Result := TWorld(GetWorld);
  if Result = nil then
  begin
    Result := TWorld.Create;
    SetWorld(Result);
  end;
end;

end.

