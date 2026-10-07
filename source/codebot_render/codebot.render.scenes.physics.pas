(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

unit Codebot.Render.Scenes.Physics;

{$i render.inc}

interface

uses
  SyncObjs,
  Codebot.System,
  Codebot.Graphics.Types,
  Codebot.Geometry,
  Codebot.Render.Graphics,
  Codebot.Hardware,
  Codebot.Render.Scenes,
  Codebot.Render.Scenes.Widgets,
  Codebot.Interop.Chipmunk2D,
  Codebot.Physics;

{ Physics is simulated in studio coordinates, a fixed area which is scaled to
  fill the scene. This keeps the physics the same at any scene size. }

const
  StudioWidth = 1920;
  StudioHeight = 1080;
  { The time in seconds of each physics step }
  PhysicsStep = Double(1 / 150);
  { The most a dragged body is accelerated toward the mouse in studio units
    per second squared }
  GrabAcceleration = 50000;

{ TPhysicsScene runs a Chipmunk2D space which is stepped every PhysicsStep
  seconds by the step thread of the TGraphicsBox it runs in. The scene
  controller starts the step thread after the scene is created, so the space
  can be built in Initialize without locking it.

  The space is shared by the step thread and the render thread. Outside of
  Simulate, wrap any code which uses the space in Lock and Unlock.
  DrawPhysics and the mouse handlers do this for you.

  Simulate, collision events, and post step callbacks run on the step thread
  and must not access LCL controls directly. If one of them raises an
  exception the step thread stops and the box reports it through its Failed
  property and OnFailed event.

  When GrabBodies is true dynamic bodies can be dragged with the left mouse
  button. }

type
  TPhysicsScene = class(TWidgetScene)
  private
    FSpace: TSpace;
    FLock: TCriticalSection;
    FPaused: Boolean;
    FGrab: TBody;
    FGrabJoint: TJoint;
    FGrabBodies: Boolean;
    FGrabCentered: Boolean;
    FBrush: ISolidBrush;
    FPen: IPen;
    procedure CreateSpace;
  protected
    { Simulate is called on the step thread with the space locked. Override
      it to apply forces or game logic, and call inherited to step the space. }
    procedure Simulate(DeltaTime: Double); virtual;
    { Convert a scene point to a studio point. Override this together with
      ScaleToStudio to map the studio to the scene in another way. }
    function PointToStudio(X, Y: Float): TVec2; virtual;
    { Set the canvas matrix to draw in studio coordinates }
    procedure ScaleToStudio; virtual;
    { The nearest shape within a distance of a studio point or a nil shape }
    function ShapeNearPoint(X, Y, Distance: Float): TShape;
    { Add static side and ground walls around the studio }
    procedure GenerateStudioWalls;
    { Return true to disable default drawing }
    function DrawCustomBody(Body: TBody): Boolean; virtual;
    function DrawCustomShape(Shape: TShape): Boolean; virtual;
    function DrawCustomJoint(Joint: TJoint): Boolean; virtual;
    { Draw the physics shapes included in the category mask in studio
      coordinates with the space locked. Drawing happens in its own canvas
      frame, so do not call this between your own canvas frame calls. }
    procedure DrawPhysics(Categories: TBitmask = $FFFFFFFF);
    property Space: TSpace read FSpace;
    property GrabJoint: TJoint read FGrabJoint;
    property GrabBodies: Boolean read FGrabBodies write FGrabBodies;
    property GrabCentered: Boolean read FGrabCentered write FGrabCentered;
  public
    procedure Initialize; override;
    procedure Finalize; override;
    { The scene is stepped every PhysicsStep seconds }
    function StepInterval: Double; override;
    { Step locks the space and calls Simulate unless the scene is paused }
    procedure Step(DeltaTime: Double); override;
    procedure DoMouseDown(var Args: TSceneMouseArgs); override;
    procedure DoMouseMove(var Args: TSceneMouseArgs); override;
    procedure DoMouseUp(var Args: TSceneMouseArgs); override;
    { Free the space and everything in it and create a new empty space. Read
      the Space property again afterwards, as copies of it are no longer
      valid. }
    procedure ResetSpace;
    { Release the joint used to drag a body. Call this before freeing bodies
      yourself, because freeing a body also frees the joints connected to it. }
    procedure ReleaseGrab;
    { Lock the space to use it outside of the step thread }
    procedure Lock;
    { Unlock the space after Lock }
    procedure Unlock;
    { When paused the space is not stepped }
    property Paused: Boolean read FPaused write FPaused;
  end;

implementation

{ TPhysicsScene }

procedure TPhysicsScene.Initialize;
begin
  inherited Initialize;
  FLock := TCriticalSection.Create;
  FBrush := NewBrush(NewColorB(0, 0, 0));
  FPen := NewPen;
  FPen.Width := 4;
  CreateSpace;
end;

{ The space holds a kinematic body which is pulled by the mouse, and a pivot joint
  connects it to the body being dragged }

procedure TPhysicsScene.CreateSpace;
begin
  FSpace := NewSpace;
  FGrabJoint.Ref := nil;
  FGrab := FSpace.NewKinematicBody;
  with FGrab.NewCircle(20) do
  begin
    Friction := 0;
    Elasticity := 0;
    Category := 0;
    CollisionType := collideGrab;
  end;
end;

procedure TPhysicsScene.Finalize;
begin
  FSpace.Free;
  FLock.Free;
  FLock := nil;
  FBrush := nil;
  FPen := nil;
  inherited Finalize;
end;

procedure TPhysicsScene.ResetSpace;
begin
  Lock;
  try
    { The grab joint is freed with the space }
    FSpace.Free;
    CreateSpace;
  finally
    Unlock;
  end;
end;

procedure TPhysicsScene.ReleaseGrab;
begin
  Lock;
  try
    FGrabJoint.Free;
  finally
    Unlock;
  end;
end;

procedure TPhysicsScene.Lock;
begin
  FLock.Enter;
end;

procedure TPhysicsScene.Unlock;
begin
  FLock.Leave;
end;

function TPhysicsScene.StepInterval: Double;
begin
  Result := PhysicsStep;
end;

procedure TPhysicsScene.Step(DeltaTime: Double);
begin
  if FPaused then
    Exit;
  Lock;
  try
    Simulate(DeltaTime);
  finally
    Unlock;
  end;
end;

procedure TPhysicsScene.Simulate(DeltaTime: Double);
begin
  FSpace.Step(DeltaTime);
end;

function TPhysicsScene.PointToStudio(X, Y: Float): TVec2;
begin
  if (Width = 0) or (Height = 0) then
    Exit(Vec2(X, Y));
  Result.X := X * StudioWidth / Width;
  Result.Y := Y * StudioHeight / Height;
end;

procedure TPhysicsScene.ScaleToStudio;
begin
  Canvas.Matrix.Identity;
  Canvas.Matrix.Scale(Width / StudioWidth, Height / StudioHeight);
end;

function TPhysicsScene.ShapeNearPoint(X, Y, Distance: Float): TShape;
var
  Info: cpPointQueryInfoStruct;
begin
  Result := FSpace.PointQueryNearest(Vec2(X, Y), Distance, FilterAll, Info);
end;

procedure TPhysicsScene.DoMouseDown(var Args: TSceneMouseArgs);
var
  Shape: TShape;
  Info: cpPointQueryInfoStruct;
  P: TVec2;
begin
  inherited DoMouseDown(Args);
  if Args.Handled then
    Exit;
  if not FGrabBodies then
    Exit;
  if Args.Button <> buttonLeft then
    Exit;
  Lock;
  try
    FGrabJoint.Free;
    P := PointToStudio(Args.X, Args.Y);
    Shape := FSpace.PointQueryNearest(P, 100, FilterAll, Info);
    if Shape.IsNil then
      Exit;
    if Shape.Body.Kind <> bodyDynamic then
      Exit;
    FGrab.Position := P;
    { The joint is anchored where the body was grabbed. When the mouse is
      inside the shape that is the mouse point, and when it is just outside
      it is the nearest point on the shape. }
    if FGrabCentered then
      P := Shape.Body.BodyToWorld(Shape.CenterOfGravity)
    else if Info.distance > 0 then
      P := Info.point;
    P := Shape.Body.WorldToBody(P);
    { A pivot joint pulls the grabbed point to the mouse without the bounce
      of a spring. Its force is limited by the mass of the body so a drag
      cannot fling it, and it corrects 15% of the distance every 1/60th of a
      second so it follows the mouse smoothly. }
    FGrabJoint := FSpace.NewPivot(Shape.Body, FGrab, P, VectZero);
    FGrabJoint.MaxForce := Shape.Body.Mass * GrabAcceleration;
    FGrabJoint.ErrorBias := Exp(60 * Ln(1 - 0.15));
  finally
    Unlock;
  end;
  Args.Handled := True;
end;

procedure TPhysicsScene.DoMouseMove(var Args: TSceneMouseArgs);
begin
  inherited DoMouseMove(Args);
  if Args.Handled then
    Exit;
  if not FGrabBodies then
    Exit;
  Lock;
  try
    if FGrabJoint.IsNil then
      Exit;
    FGrab.Position := PointToStudio(Args.X, Args.Y);
  finally
    Unlock;
  end;
  Args.Handled := True;
end;

procedure TPhysicsScene.DoMouseUp(var Args: TSceneMouseArgs);
begin
  inherited DoMouseUp(Args);
  if Args.Handled then
    Exit;
  if Args.Button <> buttonLeft then
    Exit;
  Lock;
  try
    if FGrabJoint.IsNil then
      Exit;
    FGrabJoint.Free;
  finally
    Unlock;
  end;
  Args.Handled := True;
end;

procedure TPhysicsScene.GenerateStudioWalls;
const
  WallHeight = -500000;
  WallThick = 20;

  procedure AddWall(const A, B: TVec2);
  begin
    with FSpace.Ground.NewSegment(A, B, WallThick) do
    begin
      Friction := 0.7;
      Elasticity := 0.5;
    end;
  end;

begin
  Lock;
  try
    AddWall(Vec2(0, WallHeight), Vec2(0, StudioHeight));
    AddWall(Vec2(0, StudioHeight), Vec2(StudioWidth, StudioHeight));
    AddWall(Vec2(StudioWidth, StudioHeight), Vec2(StudioWidth, WallHeight));
  finally
    Unlock;
  end;
end;

function TPhysicsScene.DrawCustomBody(Body: TBody): Boolean;
begin
  Result := False;
end;

function TPhysicsScene.DrawCustomShape(Shape: TShape): Boolean;
begin
  Result := False;
end;

function TPhysicsScene.DrawCustomJoint(Joint: TJoint): Boolean;
begin
  Result := False;
end;

{ A point to the side of the line from A to B at a percent of the way from A
  to B, used to draw the head of the grab arrow }

function NormalAtMix(const A, B: TVec2; Percent, Scale: Float): TVec2;
var
  N: TVec2;
  D: Float;
begin
  N := A - B;
  D := N.Distance + 0.000001;
  N := N / D * Scale;
  Result.X := N.Y + A.X * (1 - Percent) + B.X * Percent;
  Result.Y := -N.X + A.Y * (1 - Percent) + B.Y * Percent;
end;

procedure TPhysicsScene.DrawPhysics(Categories: TBitmask = $FFFFFFFF);
var
  Sleeping, LightBlue: TColorF;

  procedure DrawPoint(const P: TVec2);
  const
    PointSize = 8;
  begin
    Canvas.Circle(P.X, P.Y, PointSize);
    Canvas.Fill(FBrush);
  end;

  procedure DrawCircle(C: TCircle);
  var
    Body: TBody;
    A, B: TVec2;
  begin
    Body := C.Base.Body;
    A := Body.BodyToWorld(C.Offset);
    B := C.Offset;
    B.Y := B.Y - C.Radius;
    B := Body.BodyToWorld(B);
    if Body.IsSleeping then
      FBrush.Color := Sleeping
    else if Body.Kind = bodyDynamic then
      FBrush.Color := NewColorB(100, 180, 240)
    else
      FBrush.Color := LightBlue;
    Canvas.Circle(A.X, A.Y, C.Radius);
    Canvas.Fill(FBrush);
    if Body.Kind <> bodyDynamic then
      Exit;
    { A line from the center shows the rotation of the circle }
    FPen.Color := NewColorB(60, 130, 200);
    FPen.Width := 4;
    FPen.LineCap := capButt;
    Canvas.MoveTo(A.X, A.Y);
    Canvas.LineTo(B.X, B.Y);
    Canvas.Stroke(FPen);
  end;

  procedure DrawSegment(S: TSegment);
  var
    Body: TBody;
    A, B: TVec2;
  begin
    Body := S.Base.Body;
    A := Body.BodyToWorld(S.A);
    B := Body.BodyToWorld(S.B);
    if Body.IsSleeping then
      FPen.Color := Sleeping
    else
      FPen.Color := LightBlue;
    FPen.Width := S.Border * 2 + 1;
    FPen.LineCap := capRound;
    Canvas.MoveTo(A.X, A.Y);
    Canvas.LineTo(B.X, B.Y);
    Canvas.Stroke(FPen);
  end;

  procedure DrawPolygon(P: TPolygon);
  var
    Body: TBody;
    V: TVec2;
    I: Integer;
  begin
    if P.VertCount < 3 then
      Exit;
    Body := P.Base.Body;
    V := Body.BodyToWorld(P.Vert[0]);
    Canvas.MoveTo(V.X, V.Y);
    for I := 1 to P.VertCount - 1 do
    begin
      V := Body.BodyToWorld(P.Vert[I]);
      Canvas.LineTo(V.X, V.Y);
    end;
    Canvas.ClosePath;
    if Body.IsSleeping then
      FBrush.Color := Sleeping
    else if Body.Kind = bodyDynamic then
      FBrush.Color := NewColorB(140, 230, 200)
    else
      FBrush.Color := NewColorB(230, 230, 40);
    Canvas.Fill(FBrush);
  end;

  procedure DrawShape(Shape: TShape);
  begin
    case Shape.Kind of
      shapeCircle: DrawCircle(Shape.AsCircle);
      shapeSegment: DrawSegment(Shape.AsSegment);
      shapePolygon: DrawPolygon(Shape.AsPolygon);
    end;
  end;

  procedure DrawGroundSegment(S: TSegment);
  var
    A, B: TVec2;
  begin
    A := S.A;
    B := S.B;
    FPen.Color := NewColorB(60, 230, 60);
    FPen.Width := S.Border * 2 + 1;
    FPen.LineCap := capRound;
    Canvas.MoveTo(A.X, A.Y);
    Canvas.LineTo(B.X, B.Y);
    Canvas.Stroke(FPen);
  end;

  procedure DrawGrab(J: TJoint);
  var
    A, B, P: TVec2;
  begin
    A := J.A.BodyToWorld(J.AsPivot.PinA);
    B := J.B.BodyToWorld(J.AsPivot.PinB);
    Canvas.MoveTo(A.X, A.Y);
    P := NormalAtMix(A, B, 0.8, 5);
    Canvas.LineTo(P.X, P.Y);
    P := NormalAtMix(A, B, 0.8, 15);
    Canvas.LineTo(P.X, P.Y);
    Canvas.LineTo(B.X, B.Y);
    P := NormalAtMix(A, B, 0.8, -15);
    Canvas.LineTo(P.X, P.Y);
    P := NormalAtMix(A, B, 0.8, -5);
    Canvas.LineTo(P.X, P.Y);
    Canvas.ClosePath;
    FBrush.Color := NewColorB(50, 150, 30);
    Canvas.Fill(FBrush);
  end;

  procedure DrawSpring(J: TDampedSpringJoint);
  var
    A, B: TVec2;
  begin
    A := J.Base.A.BodyToWorld(J.PinA);
    B := J.Base.B.BodyToWorld(J.PinB);
    FBrush.Color := NewColorB(255, 0, 0);
    DrawPoint(A);
    DrawPoint(B);
    FPen.Color := NewColorB(255, 0, 0);
    FPen.Width := 3;
    FPen.LineCap := capRound;
    Canvas.MoveTo(A.X, A.Y);
    Canvas.LineTo(B.X, B.Y);
    Canvas.Stroke(FPen);
  end;

var
  Buffer: IBackBuffer;
  Shape: TShape;
  Body: TBody;
  Joint: TJoint;
begin
  if Canvas = nil then
    Exit;
  Sleeping := NewColorB(150, 150, 150);
  LightBlue := NewColorB($AD, $D8, $E6);
  Buffer := Canvas as IBackBuffer;
  Lock;
  try
    Buffer.Flip(Width, Height);
    try
      ScaleToStudio;
      { The ground is drawn first. Its segments are drawn as green walls. }
      if not DrawCustomBody(FSpace.Ground) then
        for Shape in FSpace.Ground.Shapes do
          if DrawCustomShape(Shape) then
            Continue
          else if Shape.Category and Categories = 0 then
            Continue
          else if Shape.Kind = shapeSegment then
            DrawGroundSegment(Shape.AsSegment)
          else
            DrawShape(Shape);
      for Body in FSpace.Bodies do
        if DrawCustomBody(Body) then
          Continue
        else for Shape in Body.Shapes do
          if DrawCustomShape(Shape) then
            Continue
          else if Shape.Category and Categories <> 0 then
            DrawShape(Shape);
      { Pin joints are not drawn }
      for Joint in FSpace.Joints do
        if DrawCustomJoint(Joint) then
          Continue
        else if Joint.IsGrab then
          DrawGrab(Joint)
        else if Joint.Kind = jointDampedSpring then
          DrawSpring(Joint.AsDampedSpring);
    finally
      Buffer.Flip(Width, Height);
    end;
  finally
    Unlock;
  end;
end;

end.
