(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ Codebot.Interop.Chipmunk2D is a Pascal port of the Chipmunk2D physics library
  version 7.0.3 by Scott Lembcke and Howling Moon Software.

  Copyright (c) 2013 Scott Lembcke and Howling Moon Software

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.

  About this unit

  This unit links the Chipmunk2D C library statically, which must be built at
  single precision. Run Chipmunk2D/build-single.sh to build libchipmunk2d.a
  before compiling. InitChipmunk2D returns False if the library linked was not
  built at single precision.

  The functions, types, and fields keep their Chipmunk names so that the
  Chipmunk documentation and examples apply. The differences are:

    cpFloat is single precision, the same as Chipmunk built with
      CP_USE_DOUBLES set to 0
    cpVect is TVec2, cpTransform is TMatrix2x3, and cpMat2x2 is TMatrix2x2,
      all from Codebot.Geometry, which have the same memory layout as the
      Chipmunk types
    A type such as cpBody is a pointer to cpBodyStruct, so a C parameter
      written cpBody *body is written body: cpBody
    cpBool is Boolean
    Callbacks are passed to C and must be declared cdecl
    Floating point exceptions are masked while a Chipmunk function runs,
      including while it calls your callbacks, because Chipmunk relies on
      IEEE arithmetic such as the infinite mass of a static body
    A failed hard assertion in the C library aborts the program
    cpHastySpace, the multithreaded solver, is not included

  Codebot.Chipmunk2D.Native is a Pascal port of the same library which needs
  no C library. It is not part of the package. }
unit Codebot.Interop.Chipmunk2D;

{$i render.inc}
{$pointermath on}
{ The hashing in Chipmunk relies on integer overflow wrapping around }
{$q-}
{$r-}

interface

uses
  Math,
  Codebot.System,
  Codebot.Geometry;

{ Basic types, from chipmunk_types.h }

const
  CP_VERSION_MAJOR = 7;
  CP_VERSION_MINOR = 0;
  CP_VERSION_RELEASE = 3;
  cpVersionString = '7.0.3';

type
  cpFloat = Float;
  { Pointer to a cpFloat }
  PcpFloat = ^cpFloat;
  cpVect = TVec2;
  { Pointer to a cpVect }
  PcpVect = ^cpVect;
  cpTransform = TMatrix2x3;
  cpMat2x2 = TMatrix2x2;
  cpBool = Boolean;
  cpHashValue = PtrUInt;
  cpCollisionID = LongWord;
  cpDataPointer = Pointer;
  cpCollisionType = PtrUInt;
  cpGroup = PtrUInt;
  cpBitmask = LongWord;
  cpTimestamp = LongWord;

const
  cpTrue = True;
  cpFalse = False;
  CP_PI = 3.14159265358979323846264338327950288;
  { The smallest normalized single precision number. Typed constants keep the
    arithmetic they appear in at single precision, as it is in Chipmunk. }
  CPFLOAT_MIN = cpFloat(1.17549435082228750797e-38);
  CP_NO_GROUP = cpGroup(0);
  CP_ALL_CATEGORIES = cpBitmask($FFFFFFFF);
  CP_WILDCARD_COLLISION_TYPE = not cpCollisionType(0);
  CP_MAX_CONTACTS_PER_ARBITER = 2;
  CP_POLY_SHAPE_INLINE_ALLOC = 6;
  CP_BUFFER_BYTES = 32 * 1024;

function CP_INFINITY: cpFloat; inline;

function cpfmax(a, b: cpFloat): cpFloat; inline;
function cpfmin(a, b: cpFloat): cpFloat; inline;
function cpfabs(f: cpFloat): cpFloat; inline;
function cpfclamp(f, min, max: cpFloat): cpFloat; inline;
function cpfclamp01(f: cpFloat): cpFloat; inline;
function cpflerp(f1, f2, t: cpFloat): cpFloat; inline;
function cpflerpconst(f1, f2, d: cpFloat): cpFloat; inline;

{ Vectors, from cpVect.h }

const
  cpvzero: cpVect = (X: 0; Y: 0);

function cpv(x, y: cpFloat): cpVect; inline;
function cpveql(const v1, v2: cpVect): cpBool; inline;
function cpvadd(const v1, v2: cpVect): cpVect; inline;
function cpvsub(const v1, v2: cpVect): cpVect; inline;
function cpvneg(const v: cpVect): cpVect; inline;
function cpvmult(const v: cpVect; s: cpFloat): cpVect; inline;
function cpvdot(const v1, v2: cpVect): cpFloat; inline;
function cpvcross(const v1, v2: cpVect): cpFloat; inline;
function cpvperp(const v: cpVect): cpVect; inline;
function cpvrperp(const v: cpVect): cpVect; inline;
function cpvproject(const v1, v2: cpVect): cpVect; inline;
function cpvforangle(a: cpFloat): cpVect; inline;
function cpvtoangle(const v: cpVect): cpFloat; inline;
function cpvrotate(const v1, v2: cpVect): cpVect; inline;
function cpvunrotate(const v1, v2: cpVect): cpVect; inline;
function cpvlengthsq(const v: cpVect): cpFloat; inline;
function cpvlength(const v: cpVect): cpFloat; inline;
function cpvlerp(const v1, v2: cpVect; t: cpFloat): cpVect; inline;
function cpvnormalize(const v: cpVect): cpVect; inline;
function cpvslerp(const v1, v2: cpVect; t: cpFloat): cpVect;
function cpvslerpconst(const v1, v2: cpVect; a: cpFloat): cpVect;
function cpvclamp(const v: cpVect; len: cpFloat): cpVect; inline;
function cpvlerpconst(const v1, v2: cpVect; d: cpFloat): cpVect; inline;
function cpvdist(const v1, v2: cpVect): cpFloat; inline;
function cpvdistsq(const v1, v2: cpVect): cpFloat; inline;
function cpvnear(const v1, v2: cpVect; dist: cpFloat): cpBool; inline;
function cpMat2x2New(a, b, c, d: cpFloat): cpMat2x2; inline;
function cpMat2x2Transform(const m: cpMat2x2; const v: cpVect): cpVect; inline;

{ Bounding boxes, from cpBB.h }

type
  { An axis aligned bounding box of left, bottom, right, and top }
  cpBB = record
    l, b, r, t: cpFloat;
  end;
  { Pointer to a cpBB }
  PcpBB = ^cpBB;

function cpBBNew(l, b, r, t: cpFloat): cpBB; inline;
function cpBBNewForExtents(const c: cpVect; hw, hh: cpFloat): cpBB; inline;
function cpBBNewForCircle(const p: cpVect; r: cpFloat): cpBB; inline;
function cpBBIntersects(const a, b: cpBB): cpBool; inline;
function cpBBContainsBB(const bb, other: cpBB): cpBool; inline;
function cpBBContainsVect(const bb: cpBB; const v: cpVect): cpBool; inline;
function cpBBMerge(const a, b: cpBB): cpBB; inline;
function cpBBExpand(const bb: cpBB; const v: cpVect): cpBB; inline;
function cpBBCenter(const bb: cpBB): cpVect; inline;
function cpBBArea(const bb: cpBB): cpFloat; inline;
function cpBBMergedArea(const a, b: cpBB): cpFloat; inline;
function cpBBSegmentQuery(const bb: cpBB; const a, b: cpVect): cpFloat;
function cpBBIntersectsSegment(const bb: cpBB; const a, b: cpVect): cpBool;
function cpBBClampVect(const bb: cpBB; const v: cpVect): cpVect;
function cpBBWrapVect(const bb: cpBB; const v: cpVect): cpVect;
function cpBBOffset(const bb: cpBB; const v: cpVect): cpBB; inline;

{ Transforms, from cpTransform.h }

const
  cpTransformIdentity: cpTransform = (A: 1; B: 0; C: 0; D: 1; TX: 0; TY: 0);

function cpTransformNew(a, b, c, d, tx, ty: cpFloat): cpTransform; inline;
function cpTransformNewTranspose(a, c, tx, b, d, ty: cpFloat): cpTransform; inline;
function cpTransformInverse(const t: cpTransform): cpTransform;
function cpTransformMult(const t1, t2: cpTransform): cpTransform;
function cpTransformPoint(const t: cpTransform; const p: cpVect): cpVect; inline;
function cpTransformVect(const t: cpTransform; const v: cpVect): cpVect; inline;
function cpTransformbBB(const t: cpTransform; const bb: cpBB): cpBB;
function cpTransformTranslate(const translate: cpVect): cpTransform;
function cpTransformScale(scaleX, scaleY: cpFloat): cpTransform;
function cpTransformRotate(radians: cpFloat): cpTransform;
function cpTransformRigid(const translate: cpVect; radians: cpFloat): cpTransform;
function cpTransformRigidInverse(const t: cpTransform): cpTransform;
function cpTransformWrap(const outer, inner: cpTransform): cpTransform;
function cpTransformWrapInverse(const outer, inner: cpTransform): cpTransform;
function cpTransformOrtho(const bb: cpBB): cpTransform;
function cpTransformBoneScale(const v0, v1: cpVect): cpTransform;
function cpTransformAxialScale(const axis, pivot: cpVect; scale: cpFloat): cpTransform;

{ Object types. Each type is a pointer to a record with the same name ending
  in Struct. The records are from chipmunk_structs.h. }

type
  cpArray = ^cpArrayStruct;
  cpHashSet = ^cpHashSetStruct;
  cpBody = ^cpBodyStruct;
  cpShape = ^cpShapeStruct;
  cpCircleShape = ^cpCircleShapeStruct;
  cpSegmentShape = ^cpSegmentShapeStruct;
  cpPolyShape = ^cpPolyShapeStruct;
  cpConstraint = ^cpConstraintStruct;
  cpPinJoint = ^cpPinJointStruct;
  cpSlideJoint = ^cpSlideJointStruct;
  cpPivotJoint = ^cpPivotJointStruct;
  cpGrooveJoint = ^cpGrooveJointStruct;
  cpDampedSpring = ^cpDampedSpringStruct;
  cpDampedRotarySpring = ^cpDampedRotarySpringStruct;
  cpRotaryLimitJoint = ^cpRotaryLimitJointStruct;
  cpRatchetJoint = ^cpRatchetJointStruct;
  cpGearJoint = ^cpGearJointStruct;
  cpSimpleMotor = ^cpSimpleMotorStruct;
  cpCollisionHandler = ^cpCollisionHandlerStruct;
  cpContactPointSet = ^cpContactPointSetStruct;
  cpArbiter = ^cpArbiterStruct;
  cpSpace = ^cpSpaceStruct;
  cpSpatialIndex = ^cpSpatialIndexStruct;
  cpSpatialIndexClass = ^cpSpatialIndexClassStruct;
  cpContact = ^cpContactStruct;
  cpShapeClass = ^cpShapeClassStruct;
  cpConstraintClass = ^cpConstraintClassStruct;
  cpPointQueryInfo = ^cpPointQueryInfoStruct;
  cpSegmentQueryInfo = ^cpSegmentQueryInfoStruct;
  cpSplittingPlane = ^cpSplittingPlaneStruct;
  cpPostStepCallback = ^cpPostStepCallbackStruct;
  cpHashSetBin = ^cpHashSetBinStruct;

  { Pointer to a cpShape }
  PcpShape = ^cpShape;
  { Pointer to a cpBody }
  PcpBody = ^cpBody;

{ Spatial index, from cpSpatialIndex.h }

  { Returns the bounding box of an object in a spatial index }
  cpSpatialIndexBBFunc = function(obj: Pointer): cpBB; cdecl;
  cpSpatialIndexIteratorFunc = procedure(obj: Pointer; data: Pointer); cdecl;
  cpSpatialIndexQueryFunc = function(obj1, obj2: Pointer; id: cpCollisionID; data: Pointer): cpCollisionID; cdecl;
  cpSpatialIndexSegmentQueryFunc = function(obj1, obj2: Pointer; data: Pointer): cpFloat; cdecl;
  cpBBTreeVelocityFunc = function(obj: Pointer): cpVect; cdecl;

  cpSpatialIndexDestroyImpl = procedure(index: cpSpatialIndex); cdecl;
  cpSpatialIndexCountImpl = function(index: cpSpatialIndex): Integer; cdecl;
  cpSpatialIndexEachImpl = procedure(index: cpSpatialIndex; func: cpSpatialIndexIteratorFunc; data: Pointer); cdecl;
  cpSpatialIndexContainsImpl = function(index: cpSpatialIndex; obj: Pointer; hashid: cpHashValue): cpBool; cdecl;
  cpSpatialIndexInsertImpl = procedure(index: cpSpatialIndex; obj: Pointer; hashid: cpHashValue); cdecl;
  cpSpatialIndexRemoveImpl = procedure(index: cpSpatialIndex; obj: Pointer; hashid: cpHashValue); cdecl;
  cpSpatialIndexReindexImpl = procedure(index: cpSpatialIndex); cdecl;
  cpSpatialIndexReindexObjectImpl = procedure(index: cpSpatialIndex; obj: Pointer; hashid: cpHashValue); cdecl;
  cpSpatialIndexReindexQueryImpl = procedure(index: cpSpatialIndex; func: cpSpatialIndexQueryFunc; data: Pointer); cdecl;
  cpSpatialIndexQueryImpl = procedure(index: cpSpatialIndex; obj: Pointer; bb: cpBB; func: cpSpatialIndexQueryFunc; data: Pointer); cdecl;
  cpSpatialIndexSegmentQueryImpl = procedure(index: cpSpatialIndex; obj: Pointer; a, b: cpVect; t_exit: cpFloat;
    func: cpSpatialIndexSegmentQueryFunc; data: Pointer); cdecl;

  cpSpatialIndexClassStruct = record
    destroy: cpSpatialIndexDestroyImpl;
    count: cpSpatialIndexCountImpl;
    each: cpSpatialIndexEachImpl;
    contains: cpSpatialIndexContainsImpl;
    insert: cpSpatialIndexInsertImpl;
    remove: cpSpatialIndexRemoveImpl;
    reindex: cpSpatialIndexReindexImpl;
    reindexObject: cpSpatialIndexReindexObjectImpl;
    reindexQuery: cpSpatialIndexReindexQueryImpl;
    query: cpSpatialIndexQueryImpl;
    segmentQuery: cpSpatialIndexSegmentQueryImpl;
  end;

  cpSpatialIndexStruct = record
    klass: cpSpatialIndexClass;
    bbfunc: cpSpatialIndexBBFunc;
    staticIndex, dynamicIndex: cpSpatialIndex;
  end;

{ Callback types }

  cpBodyVelocityFunc = procedure(body: cpBody; gravity: cpVect; damping, dt: cpFloat); cdecl;
  cpBodyPositionFunc = procedure(body: cpBody; dt: cpFloat); cdecl;
  cpBodyShapeIteratorFunc = procedure(body: cpBody; shape: cpShape; data: Pointer); cdecl;
  cpBodyConstraintIteratorFunc = procedure(body: cpBody; constraint: cpConstraint; data: Pointer); cdecl;
  cpBodyArbiterIteratorFunc = procedure(body: cpBody; arbiter: cpArbiter; data: Pointer); cdecl;

  cpConstraintPreSolveFunc = procedure(constraint: cpConstraint; space: cpSpace); cdecl;
  cpConstraintPostSolveFunc = procedure(constraint: cpConstraint; space: cpSpace); cdecl;
  cpDampedSpringForceFunc = function(spring: cpConstraint; dist: cpFloat): cpFloat; cdecl;
  cpDampedRotarySpringTorqueFunc = function(spring: cpConstraint; relativeAngle: cpFloat): cpFloat; cdecl;

  cpCollisionBeginFunc = function(arb: cpArbiter; space: cpSpace; userData: cpDataPointer): cpBool; cdecl;
  cpCollisionPreSolveFunc = function(arb: cpArbiter; space: cpSpace; userData: cpDataPointer): cpBool; cdecl;
  cpCollisionPostSolveFunc = procedure(arb: cpArbiter; space: cpSpace; userData: cpDataPointer); cdecl;
  cpCollisionSeparateFunc = procedure(arb: cpArbiter; space: cpSpace; userData: cpDataPointer); cdecl;

  cpPostStepFunc = procedure(space: cpSpace; key: Pointer; data: Pointer); cdecl;
  cpSpacePointQueryFunc = procedure(shape: cpShape; point: cpVect; distance: cpFloat; gradient: cpVect; data: Pointer); cdecl;
  cpSpaceSegmentQueryFunc = procedure(shape: cpShape; point, normal: cpVect; alpha: cpFloat; data: Pointer); cdecl;
  cpSpaceBBQueryFunc = procedure(shape: cpShape; data: Pointer); cdecl;
  cpSpaceShapeQueryFunc = procedure(shape: cpShape; points: cpContactPointSet; data: Pointer); cdecl;
  cpSpaceBodyIteratorFunc = procedure(body: cpBody; data: Pointer); cdecl;
  cpSpaceShapeIteratorFunc = procedure(shape: cpShape; data: Pointer); cdecl;
  cpSpaceConstraintIteratorFunc = procedure(constraint: cpConstraint; data: Pointer); cdecl;
  cpSpaceArbiterApplyImpulseFunc = procedure(arb: cpArbiter); cdecl;

{ Containers }

  cpArrayStruct = record
    num, max: Integer;
    arr: PPointer;
  end;

  cpHashSetEqlFunc = function(ptr, elt: Pointer): cpBool; cdecl;
  cpHashSetTransFunc = function(ptr, data: Pointer): Pointer; cdecl;
  cpHashSetIteratorFunc = procedure(elt, data: Pointer); cdecl;
  cpHashSetFilterFunc = function(elt, data: Pointer): cpBool; cdecl;

  cpHashSetBinStruct = record
    elt: Pointer;
    hash: cpHashValue;
    next: cpHashSetBin;
  end;
  { Pointer to a cpHashSetBin }
  PcpHashSetBin = ^cpHashSetBin;

  cpHashSetStruct = record
    entries, size: LongWord;
    eql: cpHashSetEqlFunc;
    default_value: Pointer;
    table: PcpHashSetBin;
    pooledBins: cpHashSetBin;
    allocatedBuffers: cpArray;
  end;

{ Bodies }

  cpBodyType = (CP_BODY_TYPE_DYNAMIC, CP_BODY_TYPE_KINEMATIC, CP_BODY_TYPE_STATIC);

  cpBodySleeping = record
    root: cpBody;
    next: cpBody;
    idleTime: cpFloat;
  end;

  cpBodyStruct = record
    velocity_func: cpBodyVelocityFunc;
    position_func: cpBodyPositionFunc;
    { Mass and its inverse }
    m: cpFloat;
    m_inv: cpFloat;
    { Moment of inertia and its inverse }
    i: cpFloat;
    i_inv: cpFloat;
    { Center of gravity, position, velocity, and force }
    cog: cpVect;
    p: cpVect;
    v: cpVect;
    f: cpVect;
    { Angle, angular velocity, and torque in radians }
    a: cpFloat;
    w: cpFloat;
    t: cpFloat;
    transform: cpTransform;
    userData: cpDataPointer;
    v_bias: cpVect;
    w_bias: cpFloat;
    space: cpSpace;
    shapeList: cpShape;
    arbiterList: cpArbiter;
    constraintList: cpConstraint;
    sleeping: cpBodySleeping;
  end;

{ Arbiters }

  cpArbiterState = (
    { Arbiter is active and its the first collision }
    CP_ARBITER_STATE_FIRST_COLLISION,
    { Arbiter is active and its not the first collision }
    CP_ARBITER_STATE_NORMAL,
    { Collision has been explicitly ignored, either by returning false from
      a begin collision handler or calling cpArbiterIgnore }
    CP_ARBITER_STATE_IGNORE,
    { Collison is no longer active, a space will cache an arbiter for up to
      cpSpace.collisionPersistence more steps }
    CP_ARBITER_STATE_CACHED,
    { Collison arbiter is invalid because one of the shapes was removed }
    CP_ARBITER_STATE_INVALIDATED);

  cpArbiterThread = record
    next, prev: cpArbiter;
  end;
  { Pointer to a cpArbiterThread }
  PcpArbiterThread = ^cpArbiterThread;

  cpContactStruct = record
    r1, r2: cpVect;
    nMass, tMass: cpFloat;
    bounce: cpFloat;
    jnAcc, jtAcc, jBias: cpFloat;
    bias: cpFloat;
    hash: cpHashValue;
  end;

  cpCollisionInfo = record
    a, b: cpShape;
    id: cpCollisionID;
    n: cpVect;
    count: Integer;
    arr: cpContact;
  end;
  { Pointer to a cpCollisionInfo }
  PcpCollisionInfo = ^cpCollisionInfo;

  cpCollisionHandlerStruct = record
    typeA: cpCollisionType;
    typeB: cpCollisionType;
    beginFunc: cpCollisionBeginFunc;
    preSolveFunc: cpCollisionPreSolveFunc;
    postSolveFunc: cpCollisionPostSolveFunc;
    separateFunc: cpCollisionSeparateFunc;
    userData: cpDataPointer;
  end;

  cpArbiterStruct = record
    e: cpFloat;
    u: cpFloat;
    surface_vr: cpVect;
    data: cpDataPointer;
    a, b: cpShape;
    body_a, body_b: cpBody;
    thread_a, thread_b: cpArbiterThread;
    count: Integer;
    contacts: cpContact;
    n: cpVect;
    handler, handlerA, handlerB: cpCollisionHandler;
    swapped: cpBool;
    stamp: cpTimestamp;
    state: cpArbiterState;
  end;

  cpContactPoint = record
    { The position of the contact on the surface of each shape }
    pointA, pointB: cpVect;
    { Penetration distance of the two shapes, negative when overlapping }
    distance: cpFloat;
  end;

  cpContactPointSetStruct = record
    { The number of contact points in the set }
    count: Integer;
    { The normal of the collision }
    normal: cpVect;
    points: array[0..CP_MAX_CONTACTS_PER_ARBITER - 1] of cpContactPoint;
  end;

{ Shapes }

  cpPointQueryInfoStruct = record
    { The nearest shape, nil if no shape was within range }
    shape: cpShape;
    { The closest point on the surface of the shape in world coordinates }
    point: cpVect;
    { The distance to the point, negative if the point is inside the shape }
    distance: cpFloat;
    { The gradient of the signed distance function }
    gradient: cpVect;
  end;

  cpSegmentQueryInfoStruct = record
    { The shape that was hit, or nil if no collision occured }
    shape: cpShape;
    { The point of impact }
    point: cpVect;
    { The normal of the surface hit }
    normal: cpVect;
    { The normalized distance along the query segment in the range 0 to 1 }
    alpha: cpFloat;
  end;

  cpShapeFilter = record
    { Two objects with the same non zero group do not collide }
    group: cpGroup;
    { A bitmask of the categories this object belongs to }
    categories: cpBitmask;
    { A bitmask of the categories this object collides with }
    mask: cpBitmask;
  end;

  cpShapeMassInfo = record
    m: cpFloat;
    i: cpFloat;
    cog: cpVect;
    area: cpFloat;
  end;

  cpShapeType = (CP_CIRCLE_SHAPE, CP_SEGMENT_SHAPE, CP_POLY_SHAPE, CP_NUM_SHAPES);

  cpShapeCacheDataImpl = function(shape: cpShape; transform: cpTransform): cpBB; cdecl;
  cpShapeDestroyImpl = procedure(shape: cpShape); cdecl;
  cpShapePointQueryImpl = procedure(shape: cpShape; p: cpVect; info: cpPointQueryInfo); cdecl;
  cpShapeSegmentQueryImpl = procedure(shape: cpShape; a, b: cpVect; radius: cpFloat; info: cpSegmentQueryInfo); cdecl;

  cpShapeClassStruct = record
    kind: cpShapeType;
    cacheData: cpShapeCacheDataImpl;
    destroy: cpShapeDestroyImpl;
    pointQuery: cpShapePointQueryImpl;
    segmentQuery: cpShapeSegmentQueryImpl;
  end;

  cpShapeStruct = record
    klass: cpShapeClass;
    space: cpSpace;
    body: cpBody;
    massInfo: cpShapeMassInfo;
    bb: cpBB;
    sensor: cpBool;
    e: cpFloat;
    u: cpFloat;
    surfaceV: cpVect;
    userData: cpDataPointer;
    collisionType: cpCollisionType;
    filter: cpShapeFilter;
    next: cpShape;
    prev: cpShape;
    hashid: cpHashValue;
  end;

  cpCircleShapeStruct = record
    shape: cpShapeStruct;
    c, tc: cpVect;
    r: cpFloat;
  end;

  cpSegmentShapeStruct = record
    shape: cpShapeStruct;
    a, b, n: cpVect;
    ta, tb, tn: cpVect;
    r: cpFloat;
    a_tangent, b_tangent: cpVect;
  end;

  cpSplittingPlaneStruct = record
    v0, n: cpVect;
  end;

  cpPolyShapeStruct = record
    shape: cpShapeStruct;
    r: cpFloat;
    count: Integer;
    { The untransformed planes are appended at the end of the transformed planes }
    planes: cpSplittingPlane;
    { Allocate a small number of splitting planes internally for simple poly }
    _planes: array[0..2 * CP_POLY_SHAPE_INLINE_ALLOC - 1] of cpSplittingPlaneStruct;
  end;

{ Constraints }

  cpConstraintPreStepImpl = procedure(constraint: cpConstraint; dt: cpFloat); cdecl;
  cpConstraintApplyCachedImpulseImpl = procedure(constraint: cpConstraint; dt_coef: cpFloat); cdecl;
  cpConstraintApplyImpulseImpl = procedure(constraint: cpConstraint; dt: cpFloat); cdecl;
  cpConstraintGetImpulseImpl = function(constraint: cpConstraint): cpFloat; cdecl;

  cpConstraintClassStruct = record
    preStep: cpConstraintPreStepImpl;
    applyCachedImpulse: cpConstraintApplyCachedImpulseImpl;
    applyImpulse: cpConstraintApplyImpulseImpl;
    getImpulse: cpConstraintGetImpulseImpl;
  end;

  cpConstraintStruct = record
    klass: cpConstraintClass;
    space: cpSpace;
    a, b: cpBody;
    next_a, next_b: cpConstraint;
    maxForce: cpFloat;
    errorBias: cpFloat;
    maxBias: cpFloat;
    collideBodies: cpBool;
    preSolve: cpConstraintPreSolveFunc;
    postSolve: cpConstraintPostSolveFunc;
    userData: cpDataPointer;
  end;

  cpPinJointStruct = record
    constraint: cpConstraintStruct;
    anchorA, anchorB: cpVect;
    dist: cpFloat;
    r1, r2: cpVect;
    n: cpVect;
    nMass: cpFloat;
    jnAcc: cpFloat;
    bias: cpFloat;
  end;

  cpSlideJointStruct = record
    constraint: cpConstraintStruct;
    anchorA, anchorB: cpVect;
    min, max: cpFloat;
    r1, r2: cpVect;
    n: cpVect;
    nMass: cpFloat;
    jnAcc: cpFloat;
    bias: cpFloat;
  end;

  cpPivotJointStruct = record
    constraint: cpConstraintStruct;
    anchorA, anchorB: cpVect;
    r1, r2: cpVect;
    k: cpMat2x2;
    jAcc: cpVect;
    bias: cpVect;
  end;

  cpGrooveJointStruct = record
    constraint: cpConstraintStruct;
    grv_n, grv_a, grv_b: cpVect;
    anchorB: cpVect;
    grv_tn: cpVect;
    clamp: cpFloat;
    r1, r2: cpVect;
    k: cpMat2x2;
    jAcc: cpVect;
    bias: cpVect;
  end;

  cpDampedSpringStruct = record
    constraint: cpConstraintStruct;
    anchorA, anchorB: cpVect;
    restLength: cpFloat;
    stiffness: cpFloat;
    damping: cpFloat;
    springForceFunc: cpDampedSpringForceFunc;
    target_vrn: cpFloat;
    v_coef: cpFloat;
    r1, r2: cpVect;
    nMass: cpFloat;
    n: cpVect;
    jAcc: cpFloat;
  end;

  cpDampedRotarySpringStruct = record
    constraint: cpConstraintStruct;
    restAngle: cpFloat;
    stiffness: cpFloat;
    damping: cpFloat;
    springTorqueFunc: cpDampedRotarySpringTorqueFunc;
    target_wrn: cpFloat;
    w_coef: cpFloat;
    iSum: cpFloat;
    jAcc: cpFloat;
  end;

  cpRotaryLimitJointStruct = record
    constraint: cpConstraintStruct;
    min, max: cpFloat;
    iSum: cpFloat;
    bias: cpFloat;
    jAcc: cpFloat;
  end;

  cpRatchetJointStruct = record
    constraint: cpConstraintStruct;
    angle, phase, ratchet: cpFloat;
    iSum: cpFloat;
    bias: cpFloat;
    jAcc: cpFloat;
  end;

  cpGearJointStruct = record
    constraint: cpConstraintStruct;
    phase, ratio: cpFloat;
    ratio_inv: cpFloat;
    iSum: cpFloat;
    bias: cpFloat;
    jAcc: cpFloat;
  end;

  cpSimpleMotorStruct = record
    constraint: cpConstraintStruct;
    rate: cpFloat;
    iSum: cpFloat;
    jAcc: cpFloat;
  end;

{ Spaces }

  cpSpaceStruct = record
    iterations: Integer;
    gravity: cpVect;
    damping: cpFloat;
    idleSpeedThreshold: cpFloat;
    sleepTimeThreshold: cpFloat;
    collisionSlop: cpFloat;
    collisionBias: cpFloat;
    collisionPersistence: cpTimestamp;
    userData: cpDataPointer;
    stamp: cpTimestamp;
    curr_dt: cpFloat;
    dynamicBodies: cpArray;
    staticBodies: cpArray;
    rousedBodies: cpArray;
    sleepingComponents: cpArray;
    shapeIDCounter: cpHashValue;
    staticShapes: cpSpatialIndex;
    dynamicShapes: cpSpatialIndex;
    constraints: cpArray;
    arbiters: cpArray;
    { A linked ring of contact buffers, which is private to the space }
    contactBuffersHead: Pointer;
    cachedArbiters: cpHashSet;
    pooledArbiters: cpArray;
    allocatedBuffers: cpArray;
    locked: Integer;
    usesWildcards: cpBool;
    collisionHandlers: cpHashSet;
    defaultHandler: cpCollisionHandlerStruct;
    skipPostStep: cpBool;
    postStepCallbacks: cpArray;
    staticBody: cpBody;
    _staticBody: cpBodyStruct;
  end;

  cpPostStepCallbackStruct = record
    func: cpPostStepFunc;
    key: Pointer;
    data: Pointer;
  end;

{ Debug drawing }

  cpSpaceDebugColor = record
    r, g, b, a: Single;
  end;

  cpSpaceDebugDrawCircleImpl = procedure(pos: cpVect; angle, radius: cpFloat;
    outlineColor, fillColor: cpSpaceDebugColor; data: cpDataPointer); cdecl;
  cpSpaceDebugDrawSegmentImpl = procedure(a, b: cpVect; color: cpSpaceDebugColor; data: cpDataPointer); cdecl;
  cpSpaceDebugDrawFatSegmentImpl = procedure(a, b: cpVect; radius: cpFloat;
    outlineColor, fillColor: cpSpaceDebugColor; data: cpDataPointer); cdecl;
  cpSpaceDebugDrawPolygonImpl = procedure(count: Integer; verts: PcpVect; radius: cpFloat;
    outlineColor, fillColor: cpSpaceDebugColor; data: cpDataPointer); cdecl;
  cpSpaceDebugDrawDotImpl = procedure(size: cpFloat; pos: cpVect; color: cpSpaceDebugColor; data: cpDataPointer); cdecl;
  cpSpaceDebugDrawColorForShapeImpl = function(shape: cpShape; data: cpDataPointer): cpSpaceDebugColor; cdecl;

  cpSpaceDebugDrawFlags = LongWord;

  cpSpaceDebugDrawOptions = record
    drawCircle: cpSpaceDebugDrawCircleImpl;
    drawSegment: cpSpaceDebugDrawSegmentImpl;
    drawFatSegment: cpSpaceDebugDrawFatSegmentImpl;
    drawPolygon: cpSpaceDebugDrawPolygonImpl;
    drawDot: cpSpaceDebugDrawDotImpl;
    flags: cpSpaceDebugDrawFlags;
    shapeOutlineColor: cpSpaceDebugColor;
    colorForShape: cpSpaceDebugDrawColorForShapeImpl;
    constraintColor: cpSpaceDebugColor;
    collisionPointColor: cpSpaceDebugColor;
    data: cpDataPointer;
  end;
  { Pointer to a cpSpaceDebugDrawOptions }
  PcpSpaceDebugDrawOptions = ^cpSpaceDebugDrawOptions;

{ Automatic geometry, from cpMarch.h and cpPolyline.h }

  cpMarchSampleFunc = function(point: cpVect; data: Pointer): cpFloat; cdecl;
  cpMarchSegmentFunc = procedure(v0, v1: cpVect; data: Pointer); cdecl;

  { A polyline is a count and capacity followed by that many vertices }
  cpPolyline = ^cpPolylineStruct;
  cpPolylineStruct = record
    count, capacity: Integer;
    verts: array[0..0] of cpVect;
  end;
  { Pointer to a cpPolyline }
  PcpPolyline = ^cpPolyline;

  cpPolylineSet = ^cpPolylineSetStruct;
  cpPolylineSetStruct = record
    count, capacity: Integer;
    lines: PcpPolyline;
  end;

const
  CP_SPACE_DEBUG_DRAW_SHAPES = 1 shl 0;
  CP_SPACE_DEBUG_DRAW_CONSTRAINTS = 1 shl 1;
  CP_SPACE_DEBUG_DRAW_COLLISION_POINTS = 1 shl 2;

  CP_SHAPE_FILTER_ALL: cpShapeFilter = (group: CP_NO_GROUP; categories: CP_ALL_CATEGORIES;
    mask: CP_ALL_CATEGORIES);
  CP_SHAPE_FILTER_NONE: cpShapeFilter = (group: CP_NO_GROUP; categories: 0; mask: 0);

{ Misc, from chipmunk.h }

{ Calculate the moment of inertia for a circle. r1 and r2 are the inner and
  outer radii. A solid circle has an inner radius of 0. }
function cpMomentForCircle(m, r1, r2: cpFloat; offset: cpVect): cpFloat;
{ Calculate area of a hollow circle }
function cpAreaForCircle(r1, r2: cpFloat): cpFloat;
{ Calculate the moment of inertia for a line segment. Beveling radius is not
  supported. }
function cpMomentForSegment(m: cpFloat; a, b: cpVect; radius: cpFloat): cpFloat;
{ Calculate the area of a fattened (capsule shaped) line segment }
function cpAreaForSegment(a, b: cpVect; radius: cpFloat): cpFloat;
{ Calculate the moment of inertia for a solid polygon shape assuming it's
  center of gravity is at it's centroid. The offset is added to each vertex. }
function cpMomentForPoly(m: cpFloat; count: Integer; verts: PcpVect; offset: cpVect; radius: cpFloat): cpFloat;
{ Calculate the signed area of a polygon. A clockwise winding gives positive
  area, the opposite of what you would expect. }
function cpAreaForPoly(count: Integer; verts: PcpVect; radius: cpFloat): cpFloat;
{ Calculate the natural centroid of a polygon }
function cpCentroidForPoly(count: Integer; verts: PcpVect): cpVect;
{ Calculate the moment of inertia for a solid box }
function cpMomentForBox(m, width, height: cpFloat): cpFloat;
function cpMomentForBox2(m: cpFloat; box: cpBB): cpFloat;
{ Calculate the convex hull of a given set of points. Returns the count of
  points in the hull. result must be an array with room for count points,
  and may be the same array as verts. first is an optional pointer to an
  integer to store where the first vertex in the hull came from. tol is the
  allowed amount to shrink the hull when simplifying it. }
function cpConvexHull(count: Integer; verts, result_: PcpVect; first: PInteger; tol: cpFloat): Integer;
{ Returns the closest point on the line segment ab, to the point p }
function cpClosetPointOnSegment(const p, a, b: cpVect): cpVect;

{ Arbiters, from cpArbiter.h }

function cpArbiterGetRestitution(arb: cpArbiter): cpFloat;
procedure cpArbiterSetRestitution(arb: cpArbiter; restitution: cpFloat);
function cpArbiterGetFriction(arb: cpArbiter): cpFloat;
procedure cpArbiterSetFriction(arb: cpArbiter; friction: cpFloat);
function cpArbiterGetSurfaceVelocity(arb: cpArbiter): cpVect;
procedure cpArbiterSetSurfaceVelocity(arb: cpArbiter; vr: cpVect);
function cpArbiterGetUserData(arb: cpArbiter): cpDataPointer;
procedure cpArbiterSetUserData(arb: cpArbiter; userData: cpDataPointer);
{ Calculate the total impulse including the friction that was applied by this
  arbiter. Only use this in a post solve or post step callback. }
function cpArbiterTotalImpulse(arb: cpArbiter): cpVect;
{ Calculate the amount of energy lost in a collision including friction }
function cpArbiterTotalKE(arb: cpArbiter): cpFloat;
{ Mark a collision pair to be ignored until the two objects separate }
function cpArbiterIgnore(arb: cpArbiter): cpBool;
{ Return the colliding shapes in the order their collision types were given
  when the handler was added }
procedure cpArbiterGetShapes(arb: cpArbiter; out a, b: cpShape);
procedure cpArbiterGetBodies(arb: cpArbiter; out a, b: cpBody);
function cpArbiterGetContactPointSet(arb: cpArbiter): cpContactPointSetStruct;
procedure cpArbiterSetContactPointSet(arb: cpArbiter; set_: cpContactPointSet);
{ Returns true if this is the first step a pair of objects started colliding }
function cpArbiterIsFirstContact(arb: cpArbiter): cpBool;
{ Returns true if the separate callback is due to a shape being removed }
function cpArbiterIsRemoval(arb: cpArbiter): cpBool;
function cpArbiterGetCount(arb: cpArbiter): Integer;
function cpArbiterGetNormal(arb: cpArbiter): cpVect;
function cpArbiterGetPointA(arb: cpArbiter; i: Integer): cpVect;
function cpArbiterGetPointB(arb: cpArbiter; i: Integer): cpVect;
function cpArbiterGetDepth(arb: cpArbiter; i: Integer): cpFloat;
function cpArbiterCallWildcardBeginA(arb: cpArbiter; space: cpSpace): cpBool;
function cpArbiterCallWildcardBeginB(arb: cpArbiter; space: cpSpace): cpBool;
function cpArbiterCallWildcardPreSolveA(arb: cpArbiter; space: cpSpace): cpBool;
function cpArbiterCallWildcardPreSolveB(arb: cpArbiter; space: cpSpace): cpBool;
procedure cpArbiterCallWildcardPostSolveA(arb: cpArbiter; space: cpSpace);
procedure cpArbiterCallWildcardPostSolveB(arb: cpArbiter; space: cpSpace);
procedure cpArbiterCallWildcardSeparateA(arb: cpArbiter; space: cpSpace);
procedure cpArbiterCallWildcardSeparateB(arb: cpArbiter; space: cpSpace);

{ Bodies, from cpBody.h }

function cpBodyAlloc: cpBody;
function cpBodyInit(body: cpBody; mass, moment: cpFloat): cpBody;
{ Allocate and initialize a dynamic body }
function cpBodyNew(mass, moment: cpFloat): cpBody;
function cpBodyNewKinematic: cpBody;
function cpBodyNewStatic: cpBody;
procedure cpBodyDestroy(body: cpBody);
procedure cpBodyFree(body: cpBody);
{ Wake up a sleeping or idle body }
procedure cpBodyActivate(body: cpBody);
{ Wake up any sleeping or idle bodies touching a static body }
procedure cpBodyActivateStatic(body: cpBody; filter: cpShape);
{ Force a body to fall asleep immediately }
procedure cpBodySleep(body: cpBody);
procedure cpBodySleepWithGroup(body: cpBody; group: cpBody);
function cpBodyIsSleeping(body: cpBody): cpBool;
function cpBodyGetType(body: cpBody): cpBodyType;
procedure cpBodySetType(body: cpBody; kind: cpBodyType);
function cpBodyGetSpace(body: cpBody): cpSpace;
function cpBodyGetMass(body: cpBody): cpFloat;
procedure cpBodySetMass(body: cpBody; m: cpFloat);
function cpBodyGetMoment(body: cpBody): cpFloat;
procedure cpBodySetMoment(body: cpBody; i: cpFloat);
function cpBodyGetPosition(body: cpBody): cpVect;
procedure cpBodySetPosition(body: cpBody; pos: cpVect);
function cpBodyGetCenterOfGravity(body: cpBody): cpVect;
procedure cpBodySetCenterOfGravity(body: cpBody; cog: cpVect);
function cpBodyGetVelocity(body: cpBody): cpVect;
procedure cpBodySetVelocity(body: cpBody; velocity: cpVect);
function cpBodyGetForce(body: cpBody): cpVect;
procedure cpBodySetForce(body: cpBody; force: cpVect);
function cpBodyGetAngle(body: cpBody): cpFloat;
procedure cpBodySetAngle(body: cpBody; a: cpFloat);
function cpBodyGetAngularVelocity(body: cpBody): cpFloat;
procedure cpBodySetAngularVelocity(body: cpBody; angularVelocity: cpFloat);
function cpBodyGetTorque(body: cpBody): cpFloat;
procedure cpBodySetTorque(body: cpBody; torque: cpFloat);
{ Get the rotation vector of the body, the x basis vector of its transform }
function cpBodyGetRotation(body: cpBody): cpVect;
function cpBodyGetUserData(body: cpBody): cpDataPointer;
procedure cpBodySetUserData(body: cpBody; userData: cpDataPointer);
procedure cpBodySetVelocityUpdateFunc(body: cpBody; velocityFunc: cpBodyVelocityFunc);
procedure cpBodySetPositionUpdateFunc(body: cpBody; positionFunc: cpBodyPositionFunc);
{ Default velocity integration function }
procedure cpBodyUpdateVelocity(body: cpBody; gravity: cpVect; damping, dt: cpFloat); cdecl;
{ Default position integration function }
procedure cpBodyUpdatePosition(body: cpBody; dt: cpFloat); cdecl;
function cpBodyLocalToWorld(body: cpBody; const point: cpVect): cpVect;
function cpBodyWorldToLocal(body: cpBody; const point: cpVect): cpVect;
procedure cpBodyApplyForceAtWorldPoint(body: cpBody; force, point: cpVect);
procedure cpBodyApplyForceAtLocalPoint(body: cpBody; force, point: cpVect);
procedure cpBodyApplyImpulseAtWorldPoint(body: cpBody; impulse, point: cpVect);
procedure cpBodyApplyImpulseAtLocalPoint(body: cpBody; impulse, point: cpVect);
function cpBodyGetVelocityAtWorldPoint(body: cpBody; point: cpVect): cpVect;
function cpBodyGetVelocityAtLocalPoint(body: cpBody; point: cpVect): cpVect;
function cpBodyKineticEnergy(body: cpBody): cpFloat;
procedure cpBodyEachShape(body: cpBody; func: cpBodyShapeIteratorFunc; data: Pointer);
procedure cpBodyEachConstraint(body: cpBody; func: cpBodyConstraintIteratorFunc; data: Pointer);
procedure cpBodyEachArbiter(body: cpBody; func: cpBodyArbiterIteratorFunc; data: Pointer);

{ Shapes, from cpShape.h, cpPolyShape.h, and chipmunk_unsafe.h }

function cpShapeFilterNew(group: cpGroup; categories, mask: cpBitmask): cpShapeFilter;
procedure cpShapeDestroy(shape: cpShape);
procedure cpShapeFree(shape: cpShape);
{ Update, cache and return the bounding box of a shape based on the body it's
  attached to }
function cpShapeCacheBB(shape: cpShape): cpBB;
{ Update, cache and return the bounding box of a shape with an explicit
  transformation }
function cpShapeUpdate(shape: cpShape; transform: cpTransform): cpBB;
{ Perform a nearest point query. It finds the closest point on the surface of
  shape to a specific point. The value returned is the distance between the
  points. A negative distance means the point is inside the shape. }
function cpShapePointQuery(shape: cpShape; p: cpVect; info: cpPointQueryInfo): cpFloat;
{ Perform a segment query against a shape. info must be a pointer to a valid
  cpSegmentQueryInfoStruct. }
function cpShapeSegmentQuery(shape: cpShape; a, b: cpVect; radius: cpFloat; info: cpSegmentQueryInfo): cpBool;
{ Return contact information about two shapes }
function cpShapesCollide(a, b: cpShape): cpContactPointSetStruct;
function cpShapeGetSpace(shape: cpShape): cpSpace;
function cpShapeGetBody(shape: cpShape): cpBody;
procedure cpShapeSetBody(shape: cpShape; body: cpBody);
function cpShapeGetMass(shape: cpShape): cpFloat;
procedure cpShapeSetMass(shape: cpShape; mass: cpFloat);
function cpShapeGetDensity(shape: cpShape): cpFloat;
procedure cpShapeSetDensity(shape: cpShape; density: cpFloat);
function cpShapeGetMoment(shape: cpShape): cpFloat;
function cpShapeGetArea(shape: cpShape): cpFloat;
function cpShapeGetCenterOfGravity(shape: cpShape): cpVect;
function cpShapeGetBB(shape: cpShape): cpBB;
function cpShapeGetSensor(shape: cpShape): cpBool;
procedure cpShapeSetSensor(shape: cpShape; sensor: cpBool);
function cpShapeGetElasticity(shape: cpShape): cpFloat;
procedure cpShapeSetElasticity(shape: cpShape; elasticity: cpFloat);
function cpShapeGetFriction(shape: cpShape): cpFloat;
procedure cpShapeSetFriction(shape: cpShape; friction: cpFloat);
function cpShapeGetSurfaceVelocity(shape: cpShape): cpVect;
procedure cpShapeSetSurfaceVelocity(shape: cpShape; surfaceVelocity: cpVect);
function cpShapeGetUserData(shape: cpShape): cpDataPointer;
procedure cpShapeSetUserData(shape: cpShape; userData: cpDataPointer);
function cpShapeGetCollisionType(shape: cpShape): cpCollisionType;
procedure cpShapeSetCollisionType(shape: cpShape; collisionType: cpCollisionType);
function cpShapeGetFilter(shape: cpShape): cpShapeFilter;
procedure cpShapeSetFilter(shape: cpShape; filter: cpShapeFilter);

function cpCircleShapeAlloc: cpCircleShape;
function cpCircleShapeInit(circle: cpCircleShape; body: cpBody; radius: cpFloat; offset: cpVect): cpCircleShape;
function cpCircleShapeNew(body: cpBody; radius: cpFloat; offset: cpVect): cpShape;
function cpCircleShapeGetOffset(shape: cpShape): cpVect;
function cpCircleShapeGetRadius(shape: cpShape): cpFloat;
procedure cpCircleShapeSetRadius(shape: cpShape; radius: cpFloat);
procedure cpCircleShapeSetOffset(shape: cpShape; offset: cpVect);

function cpSegmentShapeAlloc: cpSegmentShape;
function cpSegmentShapeInit(seg: cpSegmentShape; body: cpBody; a, b: cpVect; radius: cpFloat): cpSegmentShape;
function cpSegmentShapeNew(body: cpBody; a, b: cpVect; radius: cpFloat): cpShape;
{ Let Chipmunk know about the geometry of adjacent segments to avoid
  colliding with endcaps }
procedure cpSegmentShapeSetNeighbors(shape: cpShape; prev, next: cpVect);
function cpSegmentShapeGetA(shape: cpShape): cpVect;
function cpSegmentShapeGetB(shape: cpShape): cpVect;
function cpSegmentShapeGetNormal(shape: cpShape): cpVect;
function cpSegmentShapeGetRadius(shape: cpShape): cpFloat;
procedure cpSegmentShapeSetEndpoints(shape: cpShape; a, b: cpVect);
procedure cpSegmentShapeSetRadius(shape: cpShape; radius: cpFloat);

function cpPolyShapeAlloc: cpPolyShape;
{ Initialize a polygon shape with rounded corners. A convex hull will be
  created from the vertexes. }
function cpPolyShapeInit(poly: cpPolyShape; body: cpBody; count: Integer; verts: PcpVect;
  transform: cpTransform; radius: cpFloat): cpPolyShape;
{ Initialize a polygon shape with rounded corners. The vertexes must be
  convex with a counter-clockwise winding. }
function cpPolyShapeInitRaw(poly: cpPolyShape; body: cpBody; count: Integer; verts: PcpVect;
  radius: cpFloat): cpPolyShape;
function cpPolyShapeNew(body: cpBody; count: Integer; verts: PcpVect; transform: cpTransform;
  radius: cpFloat): cpShape;
function cpPolyShapeNewRaw(body: cpBody; count: Integer; verts: PcpVect; radius: cpFloat): cpShape;
function cpBoxShapeInit(poly: cpPolyShape; body: cpBody; width, height, radius: cpFloat): cpPolyShape;
function cpBoxShapeInit2(poly: cpPolyShape; body: cpBody; box: cpBB; radius: cpFloat): cpPolyShape;
function cpBoxShapeNew(body: cpBody; width, height, radius: cpFloat): cpShape;
function cpBoxShapeNew2(body: cpBody; box: cpBB; radius: cpFloat): cpShape;
function cpPolyShapeGetCount(shape: cpShape): Integer;
function cpPolyShapeGetVert(shape: cpShape; index: Integer): cpVect;
function cpPolyShapeGetRadius(shape: cpShape): cpFloat;
procedure cpPolyShapeSetVerts(shape: cpShape; count: Integer; verts: PcpVect; transform: cpTransform);
procedure cpPolyShapeSetVertsRaw(shape: cpShape; count: Integer; verts: PcpVect);
procedure cpPolyShapeSetRadius(shape: cpShape; radius: cpFloat);

{ Constraints, from cpConstraint.h and the joint headers }

procedure cpConstraintDestroy(constraint: cpConstraint);
procedure cpConstraintFree(constraint: cpConstraint);
function cpConstraintGetSpace(constraint: cpConstraint): cpSpace;
function cpConstraintGetBodyA(constraint: cpConstraint): cpBody;
function cpConstraintGetBodyB(constraint: cpConstraint): cpBody;
function cpConstraintGetMaxForce(constraint: cpConstraint): cpFloat;
procedure cpConstraintSetMaxForce(constraint: cpConstraint; maxForce: cpFloat);
function cpConstraintGetErrorBias(constraint: cpConstraint): cpFloat;
procedure cpConstraintSetErrorBias(constraint: cpConstraint; errorBias: cpFloat);
function cpConstraintGetMaxBias(constraint: cpConstraint): cpFloat;
procedure cpConstraintSetMaxBias(constraint: cpConstraint; maxBias: cpFloat);
function cpConstraintGetCollideBodies(constraint: cpConstraint): cpBool;
procedure cpConstraintSetCollideBodies(constraint: cpConstraint; collideBodies: cpBool);
function cpConstraintGetPreSolveFunc(constraint: cpConstraint): cpConstraintPreSolveFunc;
procedure cpConstraintSetPreSolveFunc(constraint: cpConstraint; preSolveFunc: cpConstraintPreSolveFunc);
function cpConstraintGetPostSolveFunc(constraint: cpConstraint): cpConstraintPostSolveFunc;
procedure cpConstraintSetPostSolveFunc(constraint: cpConstraint; postSolveFunc: cpConstraintPostSolveFunc);
function cpConstraintGetUserData(constraint: cpConstraint): cpDataPointer;
procedure cpConstraintSetUserData(constraint: cpConstraint; userData: cpDataPointer);
{ Get the last impulse applied by this constraint }
function cpConstraintGetImpulse(constraint: cpConstraint): cpFloat;

function cpConstraintIsPinJoint(constraint: cpConstraint): cpBool;
function cpPinJointAlloc: cpPinJoint;
function cpPinJointInit(joint: cpPinJoint; a, b: cpBody; anchorA, anchorB: cpVect): cpPinJoint;
function cpPinJointNew(a, b: cpBody; anchorA, anchorB: cpVect): cpConstraint;
function cpPinJointGetAnchorA(constraint: cpConstraint): cpVect;
procedure cpPinJointSetAnchorA(constraint: cpConstraint; anchorA: cpVect);
function cpPinJointGetAnchorB(constraint: cpConstraint): cpVect;
procedure cpPinJointSetAnchorB(constraint: cpConstraint; anchorB: cpVect);
function cpPinJointGetDist(constraint: cpConstraint): cpFloat;
procedure cpPinJointSetDist(constraint: cpConstraint; dist: cpFloat);

function cpConstraintIsSlideJoint(constraint: cpConstraint): cpBool;
function cpSlideJointAlloc: cpSlideJoint;
function cpSlideJointInit(joint: cpSlideJoint; a, b: cpBody; anchorA, anchorB: cpVect; min, max: cpFloat): cpSlideJoint;
function cpSlideJointNew(a, b: cpBody; anchorA, anchorB: cpVect; min, max: cpFloat): cpConstraint;
function cpSlideJointGetAnchorA(constraint: cpConstraint): cpVect;
procedure cpSlideJointSetAnchorA(constraint: cpConstraint; anchorA: cpVect);
function cpSlideJointGetAnchorB(constraint: cpConstraint): cpVect;
procedure cpSlideJointSetAnchorB(constraint: cpConstraint; anchorB: cpVect);
function cpSlideJointGetMin(constraint: cpConstraint): cpFloat;
procedure cpSlideJointSetMin(constraint: cpConstraint; min: cpFloat);
function cpSlideJointGetMax(constraint: cpConstraint): cpFloat;
procedure cpSlideJointSetMax(constraint: cpConstraint; max: cpFloat);

function cpConstraintIsPivotJoint(constraint: cpConstraint): cpBool;
function cpPivotJointAlloc: cpPivotJoint;
function cpPivotJointInit(joint: cpPivotJoint; a, b: cpBody; anchorA, anchorB: cpVect): cpPivotJoint;
function cpPivotJointNew(a, b: cpBody; pivot: cpVect): cpConstraint;
function cpPivotJointNew2(a, b: cpBody; anchorA, anchorB: cpVect): cpConstraint;
function cpPivotJointGetAnchorA(constraint: cpConstraint): cpVect;
procedure cpPivotJointSetAnchorA(constraint: cpConstraint; anchorA: cpVect);
function cpPivotJointGetAnchorB(constraint: cpConstraint): cpVect;
procedure cpPivotJointSetAnchorB(constraint: cpConstraint; anchorB: cpVect);

function cpConstraintIsGrooveJoint(constraint: cpConstraint): cpBool;
function cpGrooveJointAlloc: cpGrooveJoint;
function cpGrooveJointInit(joint: cpGrooveJoint; a, b: cpBody; groove_a, groove_b, anchorB: cpVect): cpGrooveJoint;
function cpGrooveJointNew(a, b: cpBody; groove_a, groove_b, anchorB: cpVect): cpConstraint;
function cpGrooveJointGetGrooveA(constraint: cpConstraint): cpVect;
procedure cpGrooveJointSetGrooveA(constraint: cpConstraint; grooveA: cpVect);
function cpGrooveJointGetGrooveB(constraint: cpConstraint): cpVect;
procedure cpGrooveJointSetGrooveB(constraint: cpConstraint; grooveB: cpVect);
function cpGrooveJointGetAnchorB(constraint: cpConstraint): cpVect;
procedure cpGrooveJointSetAnchorB(constraint: cpConstraint; anchorB: cpVect);

function cpConstraintIsDampedSpring(constraint: cpConstraint): cpBool;
function cpDampedSpringAlloc: cpDampedSpring;
function cpDampedSpringInit(joint: cpDampedSpring; a, b: cpBody; anchorA, anchorB: cpVect;
  restLength, stiffness, damping: cpFloat): cpDampedSpring;
function cpDampedSpringNew(a, b: cpBody; anchorA, anchorB: cpVect;
  restLength, stiffness, damping: cpFloat): cpConstraint;
function cpDampedSpringGetAnchorA(constraint: cpConstraint): cpVect;
procedure cpDampedSpringSetAnchorA(constraint: cpConstraint; anchorA: cpVect);
function cpDampedSpringGetAnchorB(constraint: cpConstraint): cpVect;
procedure cpDampedSpringSetAnchorB(constraint: cpConstraint; anchorB: cpVect);
function cpDampedSpringGetRestLength(constraint: cpConstraint): cpFloat;
procedure cpDampedSpringSetRestLength(constraint: cpConstraint; restLength: cpFloat);
function cpDampedSpringGetStiffness(constraint: cpConstraint): cpFloat;
procedure cpDampedSpringSetStiffness(constraint: cpConstraint; stiffness: cpFloat);
function cpDampedSpringGetDamping(constraint: cpConstraint): cpFloat;
procedure cpDampedSpringSetDamping(constraint: cpConstraint; damping: cpFloat);
function cpDampedSpringGetSpringForceFunc(constraint: cpConstraint): cpDampedSpringForceFunc;
procedure cpDampedSpringSetSpringForceFunc(constraint: cpConstraint; springForceFunc: cpDampedSpringForceFunc);

function cpConstraintIsDampedRotarySpring(constraint: cpConstraint): cpBool;
function cpDampedRotarySpringAlloc: cpDampedRotarySpring;
function cpDampedRotarySpringInit(joint: cpDampedRotarySpring; a, b: cpBody;
  restAngle, stiffness, damping: cpFloat): cpDampedRotarySpring;
function cpDampedRotarySpringNew(a, b: cpBody; restAngle, stiffness, damping: cpFloat): cpConstraint;
function cpDampedRotarySpringGetRestAngle(constraint: cpConstraint): cpFloat;
procedure cpDampedRotarySpringSetRestAngle(constraint: cpConstraint; restAngle: cpFloat);
function cpDampedRotarySpringGetStiffness(constraint: cpConstraint): cpFloat;
procedure cpDampedRotarySpringSetStiffness(constraint: cpConstraint; stiffness: cpFloat);
function cpDampedRotarySpringGetDamping(constraint: cpConstraint): cpFloat;
procedure cpDampedRotarySpringSetDamping(constraint: cpConstraint; damping: cpFloat);
function cpDampedRotarySpringGetSpringTorqueFunc(constraint: cpConstraint): cpDampedRotarySpringTorqueFunc;
procedure cpDampedRotarySpringSetSpringTorqueFunc(constraint: cpConstraint;
  springTorqueFunc: cpDampedRotarySpringTorqueFunc);

function cpConstraintIsRotaryLimitJoint(constraint: cpConstraint): cpBool;
function cpRotaryLimitJointAlloc: cpRotaryLimitJoint;
function cpRotaryLimitJointInit(joint: cpRotaryLimitJoint; a, b: cpBody; min, max: cpFloat): cpRotaryLimitJoint;
function cpRotaryLimitJointNew(a, b: cpBody; min, max: cpFloat): cpConstraint;
function cpRotaryLimitJointGetMin(constraint: cpConstraint): cpFloat;
procedure cpRotaryLimitJointSetMin(constraint: cpConstraint; min: cpFloat);
function cpRotaryLimitJointGetMax(constraint: cpConstraint): cpFloat;
procedure cpRotaryLimitJointSetMax(constraint: cpConstraint; max: cpFloat);

function cpConstraintIsRatchetJoint(constraint: cpConstraint): cpBool;
function cpRatchetJointAlloc: cpRatchetJoint;
function cpRatchetJointInit(joint: cpRatchetJoint; a, b: cpBody; phase, ratchet: cpFloat): cpRatchetJoint;
function cpRatchetJointNew(a, b: cpBody; phase, ratchet: cpFloat): cpConstraint;
function cpRatchetJointGetAngle(constraint: cpConstraint): cpFloat;
procedure cpRatchetJointSetAngle(constraint: cpConstraint; angle: cpFloat);
function cpRatchetJointGetPhase(constraint: cpConstraint): cpFloat;
procedure cpRatchetJointSetPhase(constraint: cpConstraint; phase: cpFloat);
function cpRatchetJointGetRatchet(constraint: cpConstraint): cpFloat;
procedure cpRatchetJointSetRatchet(constraint: cpConstraint; ratchet: cpFloat);

function cpConstraintIsGearJoint(constraint: cpConstraint): cpBool;
function cpGearJointAlloc: cpGearJoint;
function cpGearJointInit(joint: cpGearJoint; a, b: cpBody; phase, ratio: cpFloat): cpGearJoint;
function cpGearJointNew(a, b: cpBody; phase, ratio: cpFloat): cpConstraint;
function cpGearJointGetPhase(constraint: cpConstraint): cpFloat;
procedure cpGearJointSetPhase(constraint: cpConstraint; phase: cpFloat);
function cpGearJointGetRatio(constraint: cpConstraint): cpFloat;
procedure cpGearJointSetRatio(constraint: cpConstraint; ratio: cpFloat);

function cpConstraintIsSimpleMotor(constraint: cpConstraint): cpBool;
function cpSimpleMotorAlloc: cpSimpleMotor;
function cpSimpleMotorInit(joint: cpSimpleMotor; a, b: cpBody; rate: cpFloat): cpSimpleMotor;
function cpSimpleMotorNew(a, b: cpBody; rate: cpFloat): cpConstraint;
function cpSimpleMotorGetRate(constraint: cpConstraint): cpFloat;
procedure cpSimpleMotorSetRate(constraint: cpConstraint; rate: cpFloat);

{ Spatial indexes, from cpSpatialIndex.h }

{ Allocate and initialize a spatial hash }
function cpSpaceHashNew(celldim: cpFloat; cells: Integer; bbfunc: cpSpatialIndexBBFunc;
  staticIndex: cpSpatialIndex): cpSpatialIndex;
{ Change the cell dimensions and table size of the spatial hash to tune it }
procedure cpSpaceHashResize(hash: cpSpatialIndex; celldim: cpFloat; numcells: Integer);
{ Allocate and initialize a bounding box tree }
function cpBBTreeNew(bbfunc: cpSpatialIndexBBFunc; staticIndex: cpSpatialIndex): cpSpatialIndex;
{ Perform a static top down optimization of the tree }
procedure cpBBTreeOptimize(index: cpSpatialIndex);
{ Set the velocity function for the bounding box tree to enable temporal coherence }
procedure cpBBTreeSetVelocityFunc(index: cpSpatialIndex; func: cpBBTreeVelocityFunc);
{ Allocate and initialize a 1D sort and sweep broadphase }
function cpSweep1DNew(bbfunc: cpSpatialIndexBBFunc; staticIndex: cpSpatialIndex): cpSpatialIndex;
procedure cpSpatialIndexFree(index: cpSpatialIndex);
{ Collide the objects in dynamicIndex against the objects in staticIndex }
procedure cpSpatialIndexCollideStatic(dynamicIndex, staticIndex: cpSpatialIndex;
  func: cpSpatialIndexQueryFunc; data: Pointer);
procedure cpSpatialIndexDestroy(index: cpSpatialIndex);
function cpSpatialIndexCount(index: cpSpatialIndex): Integer;
procedure cpSpatialIndexEach(index: cpSpatialIndex; func: cpSpatialIndexIteratorFunc; data: Pointer);
function cpSpatialIndexContains(index: cpSpatialIndex; obj: Pointer; hashid: cpHashValue): cpBool;
procedure cpSpatialIndexInsert(index: cpSpatialIndex; obj: Pointer; hashid: cpHashValue);
procedure cpSpatialIndexRemove(index: cpSpatialIndex; obj: Pointer; hashid: cpHashValue);
procedure cpSpatialIndexReindex(index: cpSpatialIndex);
procedure cpSpatialIndexReindexObject(index: cpSpatialIndex; obj: Pointer; hashid: cpHashValue);
procedure cpSpatialIndexQuery(index: cpSpatialIndex; obj: Pointer; bb: cpBB;
  func: cpSpatialIndexQueryFunc; data: Pointer);
procedure cpSpatialIndexSegmentQuery(index: cpSpatialIndex; obj: Pointer; a, b: cpVect; t_exit: cpFloat;
  func: cpSpatialIndexSegmentQueryFunc; data: Pointer);
procedure cpSpatialIndexReindexQuery(index: cpSpatialIndex; func: cpSpatialIndexQueryFunc; data: Pointer);

{ Spaces, from cpSpace.h }

function cpSpaceAlloc: cpSpace;
function cpSpaceInit(space: cpSpace): cpSpace;
function cpSpaceNew: cpSpace;
procedure cpSpaceDestroy(space: cpSpace);
procedure cpSpaceFree(space: cpSpace);
{ Number of iterations to use in the impulse solver to solve contacts and
  other constraints }
function cpSpaceGetIterations(space: cpSpace): Integer;
procedure cpSpaceSetIterations(space: cpSpace; iterations: Integer);
{ Gravity to pass to rigid bodies when integrating velocity }
function cpSpaceGetGravity(space: cpSpace): cpVect;
procedure cpSpaceSetGravity(space: cpSpace; gravity: cpVect);
{ Damping rate expressed as the fraction of velocity bodies retain each
  second. A value of 0.9 would mean that each body's velocity will drop 10%
  per second. The default value is 1.0, meaning no damping is applied. }
function cpSpaceGetDamping(space: cpSpace): cpFloat;
procedure cpSpaceSetDamping(space: cpSpace; damping: cpFloat);
{ Speed threshold for a body to be considered idle }
function cpSpaceGetIdleSpeedThreshold(space: cpSpace): cpFloat;
procedure cpSpaceSetIdleSpeedThreshold(space: cpSpace; idleSpeedThreshold: cpFloat);
{ Time a group of bodies must remain idle in order to fall asleep. Enabling
  sleeping also implicitly enables the the contact graph. The default value
  of infinity disables the sleeping algorithm. }
function cpSpaceGetSleepTimeThreshold(space: cpSpace): cpFloat;
procedure cpSpaceSetSleepTimeThreshold(space: cpSpace; sleepTimeThreshold: cpFloat);
{ Amount of encouraged penetration between colliding shapes }
function cpSpaceGetCollisionSlop(space: cpSpace): cpFloat;
procedure cpSpaceSetCollisionSlop(space: cpSpace; collisionSlop: cpFloat);
{ Determines how fast overlapping shapes are pushed apart }
function cpSpaceGetCollisionBias(space: cpSpace): cpFloat;
procedure cpSpaceSetCollisionBias(space: cpSpace; collisionBias: cpFloat);
{ Number of frames that contact information should persist }
function cpSpaceGetCollisionPersistence(space: cpSpace): cpTimestamp;
procedure cpSpaceSetCollisionPersistence(space: cpSpace; collisionPersistence: cpTimestamp);
function cpSpaceGetUserData(space: cpSpace): cpDataPointer;
procedure cpSpaceSetUserData(space: cpSpace; userData: cpDataPointer);
{ The space provided static body for a given space }
function cpSpaceGetStaticBody(space: cpSpace): cpBody;
{ Returns the current, or most recent, time step used with the given space }
function cpSpaceGetCurrentTimeStep(space: cpSpace): cpFloat;
{ Returns true from inside a callback when objects cannot be added or removed }
function cpSpaceIsLocked(space: cpSpace): cpBool;
{ Create or return the existing collision handler that is called for all
  collisions that are not handled by a more specific collision handler }
function cpSpaceAddDefaultCollisionHandler(space: cpSpace): cpCollisionHandler;
{ Create or return the existing collision handler for the specified pair of
  collision types }
function cpSpaceAddCollisionHandler(space: cpSpace; a, b: cpCollisionType): cpCollisionHandler;
{ Create or return the existing wildcard collision handler for the specified type }
function cpSpaceAddWildcardHandler(space: cpSpace; kind: cpCollisionType): cpCollisionHandler;
function cpSpaceAddShape(space: cpSpace; shape: cpShape): cpShape;
function cpSpaceAddBody(space: cpSpace; body: cpBody): cpBody;
function cpSpaceAddConstraint(space: cpSpace; constraint: cpConstraint): cpConstraint;
procedure cpSpaceRemoveShape(space: cpSpace; shape: cpShape);
procedure cpSpaceRemoveBody(space: cpSpace; body: cpBody);
procedure cpSpaceRemoveConstraint(space: cpSpace; constraint: cpConstraint);
function cpSpaceContainsShape(space: cpSpace; shape: cpShape): cpBool;
function cpSpaceContainsBody(space: cpSpace; body: cpBody): cpBool;
function cpSpaceContainsConstraint(space: cpSpace; constraint: cpConstraint): cpBool;
{ Schedule a post step callback to be called when cpSpaceStep finishes. You
  can only register one callback per unique value for key. Returns true only
  if key has never been scheduled before. }
function cpSpaceAddPostStepCallback(space: cpSpace; func: cpPostStepFunc; key, data: Pointer): cpBool;
{ Query the space at a point and call func for each shape found }
procedure cpSpacePointQuery(space: cpSpace; point: cpVect; maxDistance: cpFloat; filter: cpShapeFilter;
  func: cpSpacePointQueryFunc; data: Pointer);
{ Query the space at a point and return the nearest shape found, or nil }
function cpSpacePointQueryNearest(space: cpSpace; point: cpVect; maxDistance: cpFloat; filter: cpShapeFilter;
  info: cpPointQueryInfo): cpShape;
{ Perform a directed line segment query (like a raycast) against the space
  calling func for each shape intersected }
procedure cpSpaceSegmentQuery(space: cpSpace; start, finish: cpVect; radius: cpFloat; filter: cpShapeFilter;
  func: cpSpaceSegmentQueryFunc; data: Pointer);
{ Perform a directed line segment query against the space and return the
  first shape hit, or nil }
function cpSpaceSegmentQueryFirst(space: cpSpace; start, finish: cpVect; radius: cpFloat; filter: cpShapeFilter;
  info: cpSegmentQueryInfo): cpShape;
{ Perform a fast rectangle query on the space calling func for each shape
  found. Only the bounding boxes of the shapes are checked. }
procedure cpSpaceBBQuery(space: cpSpace; bb: cpBB; filter: cpShapeFilter; func: cpSpaceBBQueryFunc; data: Pointer);
{ Query a space for any shapes overlapping the given shape and call func for
  each shape found }
function cpSpaceShapeQuery(space: cpSpace; shape: cpShape; func: cpSpaceShapeQueryFunc; data: Pointer): cpBool;
procedure cpSpaceEachBody(space: cpSpace; func: cpSpaceBodyIteratorFunc; data: Pointer);
procedure cpSpaceEachShape(space: cpSpace; func: cpSpaceShapeIteratorFunc; data: Pointer);
procedure cpSpaceEachConstraint(space: cpSpace; func: cpSpaceConstraintIteratorFunc; data: Pointer);
{ Update the collision detection info for the static shapes in the space }
procedure cpSpaceReindexStatic(space: cpSpace);
{ Update the collision detection data for a specific shape in the space }
procedure cpSpaceReindexShape(space: cpSpace; shape: cpShape);
{ Update the collision detection data for all shapes attached to a body }
procedure cpSpaceReindexShapesForBody(space: cpSpace; body: cpBody);
{ Switch the space to use a spatial hash as its spatial index }
procedure cpSpaceUseSpatialHash(space: cpSpace; dim: cpFloat; count: Integer);
{ Step the space forward in time by dt }
procedure cpSpaceStep(space: cpSpace; dt: cpFloat);
{ Debug draw the current state of the space using the supplied drawing options }
procedure cpSpaceDebugDraw(space: cpSpace; options: PcpSpaceDebugDrawOptions);

{ Automatic geometry, from cpMarch.h and cpPolyline.h }

{ Trace an anti-aliased contour of an image along a particular threshold.
  The given number of samples will be taken and spread across the bounding
  box area using the sampling function and context. The segment function
  will be called for each segment detected that lies along the density
  contour for threshold. }
procedure cpMarchSoft(bb: cpBB; x_samples, y_samples: LongWord; threshold: cpFloat;
  segment: cpMarchSegmentFunc; segment_data: Pointer; sample: cpMarchSampleFunc; sample_data: Pointer);
{ Trace an aliased curve of an image along a particular threshold }
procedure cpMarchHard(bb: cpBB; x_samples, y_samples: LongWord; threshold: cpFloat;
  segment: cpMarchSegmentFunc; segment_data: Pointer; sample: cpMarchSampleFunc; sample_data: Pointer);

procedure cpPolylineFree(line: cpPolyline);
{ Returns true if the first vertex is equal to the last }
function cpPolylineIsClosed(line: cpPolyline): cpBool;
{ Returns a copy of a polyline simplified by using the Douglas-Peucker
  algorithm. This works very well on smooth or gently curved shapes, but not
  well on straight edged or angular shapes. }
function cpPolylineSimplifyCurves(line: cpPolyline; tol: cpFloat): cpPolyline;
{ Returns a copy of a polyline simplified by discarding "flat" vertexes.
  This works well on straight edged or angular shapes, not as well on smooth
  shapes. }
function cpPolylineSimplifyVertexes(line: cpPolyline; tol: cpFloat): cpPolyline;
{ Get the convex hull of a polyline as a looped polyline }
function cpPolylineToConvexHull(line: cpPolyline; tol: cpFloat): cpPolyline;
function cpPolylineSetAlloc: cpPolylineSet;
function cpPolylineSetInit(set_: cpPolylineSet): cpPolylineSet;
function cpPolylineSetNew: cpPolylineSet;
procedure cpPolylineSetDestroy(set_: cpPolylineSet; freePolylines: cpBool);
procedure cpPolylineSetFree(set_: cpPolylineSet; freePolylines: cpBool);
{ Add a line segment to a polyline set. A segment will either start a new
  polyline, join two others, or add to or loop an existing polyline. This is
  mostly intended to be used as a callback directly from cpMarchSoft or
  cpMarchHard. }
procedure cpPolylineSetCollectSegment(v0, v1: cpVect; lines: Pointer); cdecl;
{ Get an approximate convex decomposition from a polyline. Returns a
  cpPolylineSet of convex hulls that match the original shape to within tol.
  If the input is a self intersecting polygon, the output might end up
  overly simplified. }
function cpPolylineConvexDecomposition(line: cpPolyline; tol: cpFloat): cpPolylineSet;

{ Library checking }

{ Return True if the linked Chipmunk2D library was built at single precision.
  It is safe to call more than once. }
function InitChipmunk2D(ThrowExceptions: Boolean = False): Boolean;

implementation

uses
  SysUtils, Codebot.Core;

{ The static library built by Chipmunk2D/build-single.sh. The codebot_render
  package adds the folder it is written to to the library path. }

const
  libchipmunk2d = 'libchipmunk2d.a';

{$linklib libchipmunk2d.a}
{$ifdef unix}
  {$linklib m}
  {$linklib c}
{$endif}
{ On Windows the C runtime comes from these MinGW-w64 libraries, which are
  copied next to libchipmunk2d.a. The math functions are in mingwex, which
  reports errors through mingw32, the C library functions are imported from
  the Universal C Runtime by ucrt, and gcc has the stack probe. }
{$ifdef windows}
  {$linklib libmingwex.a}
  {$linklib libmingw32.a}
  {$linklib libucrt.a}
  {$linklib libgcc.a}
{$endif}

{ Floating point exceptions are masked while Chipmunk code runs }

const
  cpAllExceptions = [exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision];

function cpFloatEnter: TFPUExceptionMask; inline;
begin
  Result := GetExceptionMask;
  if Result <> cpAllExceptions then
    SetExceptionMask(cpAllExceptions);
end;

procedure cpFloatLeave(const mask: TFPUExceptionMask); inline;
begin
  if mask <> cpAllExceptions then
  begin
    ClearExceptions(False);
    SetExceptionMask(mask);
  end;
end;

{ Basic types }

function CP_INFINITY: cpFloat;
begin
  Result := Infinity;
end;

function cpfmax(a, b: cpFloat): cpFloat;
begin
  if a > b then Result := a else Result := b;
end;

function cpfmin(a, b: cpFloat): cpFloat;
begin
  if a < b then Result := a else Result := b;
end;

function cpfabs(f: cpFloat): cpFloat;
begin
  if f < 0 then Result := -f else Result := f;
end;

function cpfclamp(f, min, max: cpFloat): cpFloat;
begin
  Result := cpfmin(cpfmax(f, min), max);
end;

function cpfclamp01(f: cpFloat): cpFloat;
begin
  Result := cpfmax(0, cpfmin(f, 1));
end;

function cpflerp(f1, f2, t: cpFloat): cpFloat;
begin
  Result := f1 * (1 - t) + f2 * t;
end;

function cpflerpconst(f1, f2, d: cpFloat): cpFloat;
begin
  Result := f1 + cpfclamp(f2 - f1, -d, d);
end;

function cpfmod(a, b: cpFloat): cpFloat;
begin
  Result := a - b * Trunc(a / b);
end;

function cpfsqrt(f: cpFloat): cpFloat; inline;
begin
  Result := Sqrt(f);
end;

{ Vectors }

function cpv(x, y: cpFloat): cpVect;
begin
  Result.X := x;
  Result.Y := y;
end;

function cpveql(const v1, v2: cpVect): cpBool;
begin
  Result := (v1.X = v2.X) and (v1.Y = v2.Y);
end;

function cpvadd(const v1, v2: cpVect): cpVect;
begin
  Result.X := v1.X + v2.X;
  Result.Y := v1.Y + v2.Y;
end;

function cpvsub(const v1, v2: cpVect): cpVect;
begin
  Result.X := v1.X - v2.X;
  Result.Y := v1.Y - v2.Y;
end;

function cpvneg(const v: cpVect): cpVect;
begin
  Result.X := -v.X;
  Result.Y := -v.Y;
end;

function cpvmult(const v: cpVect; s: cpFloat): cpVect;
begin
  Result.X := v.X * s;
  Result.Y := v.Y * s;
end;

function cpvdot(const v1, v2: cpVect): cpFloat;
begin
  Result := v1.X * v2.X + v1.Y * v2.Y;
end;

function cpvcross(const v1, v2: cpVect): cpFloat;
begin
  Result := v1.X * v2.Y - v1.Y * v2.X;
end;

function cpvperp(const v: cpVect): cpVect;
begin
  Result.X := -v.Y;
  Result.Y := v.X;
end;

function cpvrperp(const v: cpVect): cpVect;
begin
  Result.X := v.Y;
  Result.Y := -v.X;
end;

function cpvproject(const v1, v2: cpVect): cpVect;
var
  f: cpFloat;
begin
  f := (v1.X * v2.X + v1.Y * v2.Y) / (v2.X * v2.X + v2.Y * v2.Y);
  Result.X := v2.X * f;
  Result.Y := v2.Y * f;
end;

function cpvforangle(a: cpFloat): cpVect;
begin
  Result.X := Cos(a);
  Result.Y := Sin(a);
end;

function cpvtoangle(const v: cpVect): cpFloat;
begin
  Result := ArcTan2(v.Y, v.X);
end;

function cpvrotate(const v1, v2: cpVect): cpVect;
begin
  Result.X := v1.X * v2.X - v1.Y * v2.Y;
  Result.Y := v1.X * v2.Y + v1.Y * v2.X;
end;

function cpvunrotate(const v1, v2: cpVect): cpVect;
begin
  Result.X := v1.X * v2.X + v1.Y * v2.Y;
  Result.Y := v1.Y * v2.X - v1.X * v2.Y;
end;

function cpvlengthsq(const v: cpVect): cpFloat;
begin
  Result := v.X * v.X + v.Y * v.Y;
end;

function cpvlength(const v: cpVect): cpFloat;
begin
  Result := Sqrt(v.X * v.X + v.Y * v.Y);
end;

function cpvlerp(const v1, v2: cpVect; t: cpFloat): cpVect;
begin
  Result.X := v1.X * (1 - t) + v2.X * t;
  Result.Y := v1.Y * (1 - t) + v2.Y * t;
end;

function cpvnormalize(const v: cpVect): cpVect;
var
  f: cpFloat;
begin
  f := 1 / (Sqrt(v.X * v.X + v.Y * v.Y) + CPFLOAT_MIN);
  Result.X := v.X * f;
  Result.Y := v.Y * f;
end;

function cpvslerp(const v1, v2: cpVect; t: cpFloat): cpVect;
var
  dot, omega, denom: cpFloat;
begin
  dot := cpvdot(cpvnormalize(v1), cpvnormalize(v2));
  omega := ArcCos(cpfclamp(dot, -1, 1));
  if omega < 1e-3 then
    Result := cpvlerp(v1, v2, t)
  else
  begin
    denom := 1 / Sin(omega);
    Result := cpvadd(cpvmult(v1, Sin((1 - t) * omega) * denom), cpvmult(v2, Sin(t * omega) * denom));
  end;
end;

function cpvslerpconst(const v1, v2: cpVect; a: cpFloat): cpVect;
var
  dot, omega: cpFloat;
begin
  dot := cpvdot(cpvnormalize(v1), cpvnormalize(v2));
  omega := ArcCos(cpfclamp(dot, -1, 1));
  Result := cpvslerp(v1, v2, cpfmin(a, omega) / omega);
end;

function cpvclamp(const v: cpVect; len: cpFloat): cpVect;
var
  f: cpFloat;
begin
  f := v.X * v.X + v.Y * v.Y;
  if f > len * len then
  begin
    f := 1 / (Sqrt(f) + CPFLOAT_MIN);
    Result.X := v.X * f * len;
    Result.Y := v.Y * f * len;
  end
  else
    Result := v;
end;

function cpvlerpconst(const v1, v2: cpVect; d: cpFloat): cpVect;
var
  delta: cpVect;
begin
  delta.X := v2.X - v1.X;
  delta.Y := v2.Y - v1.Y;
  delta := cpvclamp(delta, d);
  Result.X := v1.X + delta.X;
  Result.Y := v1.Y + delta.Y;
end;

function cpvdist(const v1, v2: cpVect): cpFloat;
begin
  Result := Sqrt((v1.X - v2.X) * (v1.X - v2.X) + (v1.Y - v2.Y) * (v1.Y - v2.Y));
end;

function cpvdistsq(const v1, v2: cpVect): cpFloat;
begin
  Result := (v1.X - v2.X) * (v1.X - v2.X) + (v1.Y - v2.Y) * (v1.Y - v2.Y);
end;

function cpvnear(const v1, v2: cpVect; dist: cpFloat): cpBool;
begin
  Result := (v1.X - v2.X) * (v1.X - v2.X) + (v1.Y - v2.Y) * (v1.Y - v2.Y) < dist * dist;
end;

function cpMat2x2New(a, b, c, d: cpFloat): cpMat2x2;
begin
  Result.A := a;
  Result.B := b;
  Result.C := c;
  Result.D := d;
end;

function cpMat2x2Transform(const m: cpMat2x2; const v: cpVect): cpVect;
begin
  Result.X := v.X * m.A + v.Y * m.B;
  Result.Y := v.X * m.C + v.Y * m.D;
end;

{ Bounding boxes }

function cpBBNew(l, b, r, t: cpFloat): cpBB;
begin
  Result.l := l;
  Result.b := b;
  Result.r := r;
  Result.t := t;
end;

function cpBBNewForExtents(const c: cpVect; hw, hh: cpFloat): cpBB;
begin
  Result.l := c.X - hw;
  Result.b := c.Y - hh;
  Result.r := c.X + hw;
  Result.t := c.Y + hh;
end;

function cpBBNewForCircle(const p: cpVect; r: cpFloat): cpBB;
begin
  Result.l := p.X - r;
  Result.b := p.Y - r;
  Result.r := p.X + r;
  Result.t := p.Y + r;
end;

function cpBBIntersects(const a, b: cpBB): cpBool;
begin
  Result := (a.l <= b.r) and (b.l <= a.r) and (a.b <= b.t) and (b.b <= a.t);
end;

function cpBBContainsBB(const bb, other: cpBB): cpBool;
begin
  Result := (bb.l <= other.l) and (bb.r >= other.r) and (bb.b <= other.b) and (bb.t >= other.t);
end;

function cpBBContainsVect(const bb: cpBB; const v: cpVect): cpBool;
begin
  Result := (bb.l <= v.X) and (bb.r >= v.X) and (bb.b <= v.Y) and (bb.t >= v.Y);
end;

function cpBBMerge(const a, b: cpBB): cpBB;
begin
  if a.l < b.l then Result.l := a.l else Result.l := b.l;
  if a.b < b.b then Result.b := a.b else Result.b := b.b;
  if a.r > b.r then Result.r := a.r else Result.r := b.r;
  if a.t > b.t then Result.t := a.t else Result.t := b.t;
end;

function cpBBExpand(const bb: cpBB; const v: cpVect): cpBB;
begin
  if bb.l < v.X then Result.l := bb.l else Result.l := v.X;
  if bb.b < v.Y then Result.b := bb.b else Result.b := v.Y;
  if bb.r > v.X then Result.r := bb.r else Result.r := v.X;
  if bb.t > v.Y then Result.t := bb.t else Result.t := v.Y;
end;

function cpBBCenter(const bb: cpBB): cpVect;
begin
  Result.X := bb.l * 0.5 + bb.r * 0.5;
  Result.Y := bb.b * 0.5 + bb.t * 0.5;
end;

function cpBBArea(const bb: cpBB): cpFloat;
begin
  Result := (bb.r - bb.l) * (bb.t - bb.b);
end;

function cpBBMergedArea(const a, b: cpBB): cpFloat;
begin
  Result := (cpfmax(a.r, b.r) - cpfmin(a.l, b.l)) * (cpfmax(a.t, b.t) - cpfmin(a.b, b.b));
end;

function cpBBSegmentQuery(const bb: cpBB; const a, b: cpVect): cpFloat;
var
  delta: cpVect;
  tmin, tmax, t1, t2: cpFloat;
begin
  delta := cpvsub(b, a);
  tmin := -CP_INFINITY;
  tmax := CP_INFINITY;
  if delta.X = 0 then
  begin
    if (a.X < bb.l) or (bb.r < a.X) then
      Exit(CP_INFINITY);
  end
  else
  begin
    t1 := (bb.l - a.X) / delta.X;
    t2 := (bb.r - a.X) / delta.X;
    tmin := cpfmax(tmin, cpfmin(t1, t2));
    tmax := cpfmin(tmax, cpfmax(t1, t2));
  end;
  if delta.Y = 0 then
  begin
    if (a.Y < bb.b) or (bb.t < a.Y) then
      Exit(CP_INFINITY);
  end
  else
  begin
    t1 := (bb.b - a.Y) / delta.Y;
    t2 := (bb.t - a.Y) / delta.Y;
    tmin := cpfmax(tmin, cpfmin(t1, t2));
    tmax := cpfmin(tmax, cpfmax(t1, t2));
  end;
  if (tmin <= tmax) and (0 <= tmax) and (tmin <= 1) then
    Result := cpfmax(tmin, 0)
  else
    Result := CP_INFINITY;
end;

function cpBBIntersectsSegment(const bb: cpBB; const a, b: cpVect): cpBool;
begin
  Result := cpBBSegmentQuery(bb, a, b) <> CP_INFINITY;
end;

function cpBBClampVect(const bb: cpBB; const v: cpVect): cpVect;
begin
  Result := cpv(cpfclamp(v.X, bb.l, bb.r), cpfclamp(v.Y, bb.b, bb.t));
end;

function cpBBWrapVect(const bb: cpBB; const v: cpVect): cpVect;
var
  dx, modx, x, dy, mody, y: cpFloat;
begin
  dx := cpfabs(bb.r - bb.l);
  modx := cpfmod(v.X - bb.l, dx);
  if modx > 0 then x := modx else x := modx + dx;
  dy := cpfabs(bb.t - bb.b);
  mody := cpfmod(v.Y - bb.b, dy);
  if mody > 0 then y := mody else y := mody + dy;
  Result := cpv(x + bb.l, y + bb.b);
end;

function cpBBOffset(const bb: cpBB; const v: cpVect): cpBB;
begin
  Result.l := bb.l + v.X;
  Result.b := bb.b + v.Y;
  Result.r := bb.r + v.X;
  Result.t := bb.t + v.Y;
end;

{ Transforms }

function cpTransformNew(a, b, c, d, tx, ty: cpFloat): cpTransform;
begin
  Result.A := a;
  Result.B := b;
  Result.C := c;
  Result.D := d;
  Result.TX := tx;
  Result.TY := ty;
end;

function cpTransformNewTranspose(a, c, tx, b, d, ty: cpFloat): cpTransform;
begin
  Result.A := a;
  Result.B := b;
  Result.C := c;
  Result.D := d;
  Result.TX := tx;
  Result.TY := ty;
end;

function cpTransformInverse(const t: cpTransform): cpTransform;
var
  inv_det: cpFloat;
begin
  inv_det := 1 / (t.A * t.D - t.C * t.B);
  Result := cpTransformNewTranspose(
    t.D * inv_det, -t.C * inv_det, (t.C * t.TY - t.TX * t.D) * inv_det,
    -t.B * inv_det, t.A * inv_det, (t.TX * t.B - t.A * t.TY) * inv_det);
end;

function cpTransformMult(const t1, t2: cpTransform): cpTransform;
begin
  Result := cpTransformNewTranspose(
    t1.A * t2.A + t1.C * t2.B, t1.A * t2.C + t1.C * t2.D, t1.A * t2.TX + t1.C * t2.TY + t1.TX,
    t1.B * t2.A + t1.D * t2.B, t1.B * t2.C + t1.D * t2.D, t1.B * t2.TX + t1.D * t2.TY + t1.TY);
end;

function cpTransformPoint(const t: cpTransform; const p: cpVect): cpVect;
begin
  Result.X := t.A * p.X + t.C * p.Y + t.TX;
  Result.Y := t.B * p.X + t.D * p.Y + t.TY;
end;

function cpTransformVect(const t: cpTransform; const v: cpVect): cpVect;
begin
  Result.X := t.A * v.X + t.C * v.Y;
  Result.Y := t.B * v.X + t.D * v.Y;
end;

function cpTransformbBB(const t: cpTransform; const bb: cpBB): cpBB;
var
  center: cpVect;
  hw, hh, a, b, d, e, hw_max, hh_max: cpFloat;
begin
  center := cpBBCenter(bb);
  hw := (bb.r - bb.l) * 0.5;
  hh := (bb.t - bb.b) * 0.5;
  a := t.A * hw;
  b := t.C * hh;
  d := t.B * hw;
  e := t.D * hh;
  hw_max := cpfmax(cpfabs(a + b), cpfabs(a - b));
  hh_max := cpfmax(cpfabs(d + e), cpfabs(d - e));
  Result := cpBBNewForExtents(cpTransformPoint(t, center), hw_max, hh_max);
end;

function cpTransformTranslate(const translate: cpVect): cpTransform;
begin
  Result := cpTransformNewTranspose(1, 0, translate.X, 0, 1, translate.Y);
end;

function cpTransformScale(scaleX, scaleY: cpFloat): cpTransform;
begin
  Result := cpTransformNewTranspose(scaleX, 0, 0, 0, scaleY, 0);
end;

function cpTransformRotate(radians: cpFloat): cpTransform;
var
  rot: cpVect;
begin
  rot := cpvforangle(radians);
  Result := cpTransformNewTranspose(rot.X, -rot.Y, 0, rot.Y, rot.X, 0);
end;

function cpTransformRigid(const translate: cpVect; radians: cpFloat): cpTransform;
var
  rot: cpVect;
begin
  rot := cpvforangle(radians);
  Result := cpTransformNewTranspose(rot.X, -rot.Y, translate.X, rot.Y, rot.X, translate.Y);
end;

function cpTransformRigidInverse(const t: cpTransform): cpTransform;
begin
  Result := cpTransformNewTranspose(
    t.D, -t.C, t.C * t.TY - t.TX * t.D,
    -t.B, t.A, t.TX * t.B - t.A * t.TY);
end;

function cpTransformWrap(const outer, inner: cpTransform): cpTransform;
begin
  Result := cpTransformMult(cpTransformInverse(outer), cpTransformMult(inner, outer));
end;

function cpTransformWrapInverse(const outer, inner: cpTransform): cpTransform;
begin
  Result := cpTransformMult(outer, cpTransformMult(inner, cpTransformInverse(outer)));
end;

function cpTransformOrtho(const bb: cpBB): cpTransform;
begin
  Result := cpTransformNewTranspose(
    2 / (bb.r - bb.l), 0, -(bb.r + bb.l) / (bb.r - bb.l),
    0, 2 / (bb.t - bb.b), -(bb.t + bb.b) / (bb.t - bb.b));
end;

function cpTransformBoneScale(const v0, v1: cpVect): cpTransform;
var
  d: cpVect;
begin
  d := cpvsub(v1, v0);
  Result := cpTransformNewTranspose(d.X, -d.Y, v0.X, d.Y, d.X, v0.Y);
end;

function cpTransformAxialScale(const axis, pivot: cpVect; scale: cpFloat): cpTransform;
var
  a, b: cpFloat;
begin
  a := axis.X * axis.Y * (scale - 1);
  b := cpvdot(axis, pivot) * (1 - scale);
  Result := cpTransformNewTranspose(
    scale * axis.X * axis.X + axis.Y * axis.Y, a, axis.X * b,
    a, axis.X * axis.X + scale * axis.Y * axis.Y, axis.Y * b);
end;

{ Inline functions from chipmunk.h and cpShape.h }

function cpClosetPointOnSegment(const p, a, b: cpVect): cpVect;
var
  delta: cpVect;
  lensq, t: cpFloat;
begin
  delta := cpvsub(a, b);
  lensq := cpvlengthsq(delta);
  if lensq = 0 then
    t := 1
  else
    t := cpfclamp01(cpvdot(delta, cpvsub(p, b)) / lensq);
  Result := cpvadd(b, cpvmult(delta, t));
end;

function cpShapeFilterNew(group: cpGroup; categories, mask: cpBitmask): cpShapeFilter;
begin
  Result.group := group;
  Result.categories := categories;
  Result.mask := mask;
end;

{ Spatial indexes, inline functions from cpSpatialIndex.h }

procedure cpSpatialIndexDestroy(index: cpSpatialIndex);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  try
    if index.klass <> nil then
      index.klass.destroy(index);
  finally
    cpFloatLeave(mask);
  end;
end;

function cpSpatialIndexCount(index: cpSpatialIndex): Integer;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  try
    Result := index.klass.count(index);
  finally
    cpFloatLeave(mask);
  end;
end;

procedure cpSpatialIndexEach(index: cpSpatialIndex; func: cpSpatialIndexIteratorFunc; data: Pointer);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  try
    index.klass.each(index, func, data);
  finally
    cpFloatLeave(mask);
  end;
end;

function cpSpatialIndexContains(index: cpSpatialIndex; obj: Pointer; hashid: cpHashValue): cpBool;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  try
    Result := index.klass.contains(index, obj, hashid);
  finally
    cpFloatLeave(mask);
  end;
end;

procedure cpSpatialIndexInsert(index: cpSpatialIndex; obj: Pointer; hashid: cpHashValue);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  try
    index.klass.insert(index, obj, hashid);
  finally
    cpFloatLeave(mask);
  end;
end;

procedure cpSpatialIndexRemove(index: cpSpatialIndex; obj: Pointer; hashid: cpHashValue);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  try
    index.klass.remove(index, obj, hashid);
  finally
    cpFloatLeave(mask);
  end;
end;

procedure cpSpatialIndexReindex(index: cpSpatialIndex);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  try
    index.klass.reindex(index);
  finally
    cpFloatLeave(mask);
  end;
end;

procedure cpSpatialIndexReindexObject(index: cpSpatialIndex; obj: Pointer; hashid: cpHashValue);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  try
    index.klass.reindexObject(index, obj, hashid);
  finally
    cpFloatLeave(mask);
  end;
end;

procedure cpSpatialIndexQuery(index: cpSpatialIndex; obj: Pointer; bb: cpBB;
  func: cpSpatialIndexQueryFunc; data: Pointer);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  try
    index.klass.query(index, obj, bb, func, data);
  finally
    cpFloatLeave(mask);
  end;
end;

procedure cpSpatialIndexSegmentQuery(index: cpSpatialIndex; obj: Pointer; a, b: cpVect; t_exit: cpFloat;
  func: cpSpatialIndexSegmentQueryFunc; data: Pointer);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  try
    index.klass.segmentQuery(index, obj, a, b, t_exit, func, data);
  finally
    cpFloatLeave(mask);
  end;
end;

procedure cpSpatialIndexReindexQuery(index: cpSpatialIndex; func: cpSpatialIndexQueryFunc; data: Pointer);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  try
    index.klass.reindexQuery(index, func, data);
  finally
    cpFloatLeave(mask);
  end;
end;

{ Library functions }

type
  { The C unsigned long, which is 32 bits on Windows }
  cpULong = {$ifdef windows}LongWord{$else}PtrUInt{$endif};

function _cpMomentForCircle(m, r1, r2: cpFloat; offset: cpVect): cpFloat; cdecl; external name 'cpMomentForCircle';
function _cpAreaForCircle(r1, r2: cpFloat): cpFloat; cdecl; external name 'cpAreaForCircle';
function _cpMomentForSegment(m: cpFloat; a, b: cpVect; radius: cpFloat): cpFloat; cdecl; external name 'cpMomentForSegment';
function _cpAreaForSegment(a, b: cpVect; radius: cpFloat): cpFloat; cdecl; external name 'cpAreaForSegment';
function _cpMomentForPoly(m: cpFloat; count: Integer; verts: PcpVect; offset: cpVect; radius: cpFloat): cpFloat; cdecl; external name 'cpMomentForPoly';
function _cpAreaForPoly(count: Integer; verts: PcpVect; radius: cpFloat): cpFloat; cdecl; external name 'cpAreaForPoly';
function _cpCentroidForPoly(count: Integer; verts: PcpVect): cpVect; cdecl; external name 'cpCentroidForPoly';
function _cpMomentForBox(m, width, height: cpFloat): cpFloat; cdecl; external name 'cpMomentForBox';
function _cpMomentForBox2(m: cpFloat; box: cpBB): cpFloat; cdecl; external name 'cpMomentForBox2';
function _cpConvexHull(count: Integer; verts, result_: PcpVect; first: PInteger; tol: cpFloat): Integer; cdecl; external name 'cpConvexHull';
function _cpArbiterGetRestitution(arb: cpArbiter): cpFloat; cdecl; external name 'cpArbiterGetRestitution';
procedure _cpArbiterSetRestitution(arb: cpArbiter; restitution: cpFloat); cdecl; external name 'cpArbiterSetRestitution';
function _cpArbiterGetFriction(arb: cpArbiter): cpFloat; cdecl; external name 'cpArbiterGetFriction';
procedure _cpArbiterSetFriction(arb: cpArbiter; friction: cpFloat); cdecl; external name 'cpArbiterSetFriction';
function _cpArbiterGetSurfaceVelocity(arb: cpArbiter): cpVect; cdecl; external name 'cpArbiterGetSurfaceVelocity';
procedure _cpArbiterSetSurfaceVelocity(arb: cpArbiter; vr: cpVect); cdecl; external name 'cpArbiterSetSurfaceVelocity';
function _cpArbiterGetUserData(arb: cpArbiter): cpDataPointer; cdecl; external name 'cpArbiterGetUserData';
procedure _cpArbiterSetUserData(arb: cpArbiter; userData: cpDataPointer); cdecl; external name 'cpArbiterSetUserData';
function _cpArbiterTotalImpulse(arb: cpArbiter): cpVect; cdecl; external name 'cpArbiterTotalImpulse';
function _cpArbiterTotalKE(arb: cpArbiter): cpFloat; cdecl; external name 'cpArbiterTotalKE';
function _cpArbiterIgnore(arb: cpArbiter): cpBool; cdecl; external name 'cpArbiterIgnore';
procedure _cpArbiterGetShapes(arb: cpArbiter; out a, b: cpShape); cdecl; external name 'cpArbiterGetShapes';
procedure _cpArbiterGetBodies(arb: cpArbiter; out a, b: cpBody); cdecl; external name 'cpArbiterGetBodies';
function _cpArbiterGetContactPointSet(arb: cpArbiter): cpContactPointSetStruct; cdecl; external name 'cpArbiterGetContactPointSet';
procedure _cpArbiterSetContactPointSet(arb: cpArbiter; set_: cpContactPointSet); cdecl; external name 'cpArbiterSetContactPointSet';
function _cpArbiterIsFirstContact(arb: cpArbiter): cpBool; cdecl; external name 'cpArbiterIsFirstContact';
function _cpArbiterIsRemoval(arb: cpArbiter): cpBool; cdecl; external name 'cpArbiterIsRemoval';
function _cpArbiterGetCount(arb: cpArbiter): Integer; cdecl; external name 'cpArbiterGetCount';
function _cpArbiterGetNormal(arb: cpArbiter): cpVect; cdecl; external name 'cpArbiterGetNormal';
function _cpArbiterGetPointA(arb: cpArbiter; i: Integer): cpVect; cdecl; external name 'cpArbiterGetPointA';
function _cpArbiterGetPointB(arb: cpArbiter; i: Integer): cpVect; cdecl; external name 'cpArbiterGetPointB';
function _cpArbiterGetDepth(arb: cpArbiter; i: Integer): cpFloat; cdecl; external name 'cpArbiterGetDepth';
function _cpArbiterCallWildcardBeginA(arb: cpArbiter; space: cpSpace): cpBool; cdecl; external name 'cpArbiterCallWildcardBeginA';
function _cpArbiterCallWildcardBeginB(arb: cpArbiter; space: cpSpace): cpBool; cdecl; external name 'cpArbiterCallWildcardBeginB';
function _cpArbiterCallWildcardPreSolveA(arb: cpArbiter; space: cpSpace): cpBool; cdecl; external name 'cpArbiterCallWildcardPreSolveA';
function _cpArbiterCallWildcardPreSolveB(arb: cpArbiter; space: cpSpace): cpBool; cdecl; external name 'cpArbiterCallWildcardPreSolveB';
procedure _cpArbiterCallWildcardPostSolveA(arb: cpArbiter; space: cpSpace); cdecl; external name 'cpArbiterCallWildcardPostSolveA';
procedure _cpArbiterCallWildcardPostSolveB(arb: cpArbiter; space: cpSpace); cdecl; external name 'cpArbiterCallWildcardPostSolveB';
procedure _cpArbiterCallWildcardSeparateA(arb: cpArbiter; space: cpSpace); cdecl; external name 'cpArbiterCallWildcardSeparateA';
procedure _cpArbiterCallWildcardSeparateB(arb: cpArbiter; space: cpSpace); cdecl; external name 'cpArbiterCallWildcardSeparateB';
function _cpBodyAlloc: cpBody; cdecl; external name 'cpBodyAlloc';
function _cpBodyInit(body: cpBody; mass, moment: cpFloat): cpBody; cdecl; external name 'cpBodyInit';
function _cpBodyNew(mass, moment: cpFloat): cpBody; cdecl; external name 'cpBodyNew';
function _cpBodyNewKinematic: cpBody; cdecl; external name 'cpBodyNewKinematic';
function _cpBodyNewStatic: cpBody; cdecl; external name 'cpBodyNewStatic';
procedure _cpBodyDestroy(body: cpBody); cdecl; external name 'cpBodyDestroy';
procedure _cpBodyFree(body: cpBody); cdecl; external name 'cpBodyFree';
procedure _cpBodyActivate(body: cpBody); cdecl; external name 'cpBodyActivate';
procedure _cpBodyActivateStatic(body: cpBody; filter: cpShape); cdecl; external name 'cpBodyActivateStatic';
procedure _cpBodySleep(body: cpBody); cdecl; external name 'cpBodySleep';
procedure _cpBodySleepWithGroup(body: cpBody; group: cpBody); cdecl; external name 'cpBodySleepWithGroup';
function _cpBodyIsSleeping(body: cpBody): cpBool; cdecl; external name 'cpBodyIsSleeping';
function _cpBodyGetType(body: cpBody): cpBodyType; cdecl; external name 'cpBodyGetType';
procedure _cpBodySetType(body: cpBody; kind: cpBodyType); cdecl; external name 'cpBodySetType';
function _cpBodyGetSpace(body: cpBody): cpSpace; cdecl; external name 'cpBodyGetSpace';
function _cpBodyGetMass(body: cpBody): cpFloat; cdecl; external name 'cpBodyGetMass';
procedure _cpBodySetMass(body: cpBody; m: cpFloat); cdecl; external name 'cpBodySetMass';
function _cpBodyGetMoment(body: cpBody): cpFloat; cdecl; external name 'cpBodyGetMoment';
procedure _cpBodySetMoment(body: cpBody; i: cpFloat); cdecl; external name 'cpBodySetMoment';
function _cpBodyGetPosition(body: cpBody): cpVect; cdecl; external name 'cpBodyGetPosition';
procedure _cpBodySetPosition(body: cpBody; pos: cpVect); cdecl; external name 'cpBodySetPosition';
function _cpBodyGetCenterOfGravity(body: cpBody): cpVect; cdecl; external name 'cpBodyGetCenterOfGravity';
procedure _cpBodySetCenterOfGravity(body: cpBody; cog: cpVect); cdecl; external name 'cpBodySetCenterOfGravity';
function _cpBodyGetVelocity(body: cpBody): cpVect; cdecl; external name 'cpBodyGetVelocity';
procedure _cpBodySetVelocity(body: cpBody; velocity: cpVect); cdecl; external name 'cpBodySetVelocity';
function _cpBodyGetForce(body: cpBody): cpVect; cdecl; external name 'cpBodyGetForce';
procedure _cpBodySetForce(body: cpBody; force: cpVect); cdecl; external name 'cpBodySetForce';
function _cpBodyGetAngle(body: cpBody): cpFloat; cdecl; external name 'cpBodyGetAngle';
procedure _cpBodySetAngle(body: cpBody; a: cpFloat); cdecl; external name 'cpBodySetAngle';
function _cpBodyGetAngularVelocity(body: cpBody): cpFloat; cdecl; external name 'cpBodyGetAngularVelocity';
procedure _cpBodySetAngularVelocity(body: cpBody; angularVelocity: cpFloat); cdecl; external name 'cpBodySetAngularVelocity';
function _cpBodyGetTorque(body: cpBody): cpFloat; cdecl; external name 'cpBodyGetTorque';
procedure _cpBodySetTorque(body: cpBody; torque: cpFloat); cdecl; external name 'cpBodySetTorque';
function _cpBodyGetRotation(body: cpBody): cpVect; cdecl; external name 'cpBodyGetRotation';
function _cpBodyGetUserData(body: cpBody): cpDataPointer; cdecl; external name 'cpBodyGetUserData';
procedure _cpBodySetUserData(body: cpBody; userData: cpDataPointer); cdecl; external name 'cpBodySetUserData';
procedure _cpBodySetVelocityUpdateFunc(body: cpBody; velocityFunc: cpBodyVelocityFunc); cdecl; external name 'cpBodySetVelocityUpdateFunc';
procedure _cpBodySetPositionUpdateFunc(body: cpBody; positionFunc: cpBodyPositionFunc); cdecl; external name 'cpBodySetPositionUpdateFunc';
procedure _cpBodyUpdateVelocity(body: cpBody; gravity: cpVect; damping, dt: cpFloat); cdecl; external name 'cpBodyUpdateVelocity';
procedure _cpBodyUpdatePosition(body: cpBody; dt: cpFloat); cdecl; external name 'cpBodyUpdatePosition';
function _cpBodyLocalToWorld(body: cpBody; point: cpVect): cpVect; cdecl; external name 'cpBodyLocalToWorld';
function _cpBodyWorldToLocal(body: cpBody; point: cpVect): cpVect; cdecl; external name 'cpBodyWorldToLocal';
procedure _cpBodyApplyForceAtWorldPoint(body: cpBody; force, point: cpVect); cdecl; external name 'cpBodyApplyForceAtWorldPoint';
procedure _cpBodyApplyForceAtLocalPoint(body: cpBody; force, point: cpVect); cdecl; external name 'cpBodyApplyForceAtLocalPoint';
procedure _cpBodyApplyImpulseAtWorldPoint(body: cpBody; impulse, point: cpVect); cdecl; external name 'cpBodyApplyImpulseAtWorldPoint';
procedure _cpBodyApplyImpulseAtLocalPoint(body: cpBody; impulse, point: cpVect); cdecl; external name 'cpBodyApplyImpulseAtLocalPoint';
function _cpBodyGetVelocityAtWorldPoint(body: cpBody; point: cpVect): cpVect; cdecl; external name 'cpBodyGetVelocityAtWorldPoint';
function _cpBodyGetVelocityAtLocalPoint(body: cpBody; point: cpVect): cpVect; cdecl; external name 'cpBodyGetVelocityAtLocalPoint';
function _cpBodyKineticEnergy(body: cpBody): cpFloat; cdecl; external name 'cpBodyKineticEnergy';
procedure _cpBodyEachShape(body: cpBody; func: cpBodyShapeIteratorFunc; data: Pointer); cdecl; external name 'cpBodyEachShape';
procedure _cpBodyEachConstraint(body: cpBody; func: cpBodyConstraintIteratorFunc; data: Pointer); cdecl; external name 'cpBodyEachConstraint';
procedure _cpBodyEachArbiter(body: cpBody; func: cpBodyArbiterIteratorFunc; data: Pointer); cdecl; external name 'cpBodyEachArbiter';
procedure _cpShapeDestroy(shape: cpShape); cdecl; external name 'cpShapeDestroy';
procedure _cpShapeFree(shape: cpShape); cdecl; external name 'cpShapeFree';
function _cpShapeCacheBB(shape: cpShape): cpBB; cdecl; external name 'cpShapeCacheBB';
function _cpShapeUpdate(shape: cpShape; transform: cpTransform): cpBB; cdecl; external name 'cpShapeUpdate';
function _cpShapePointQuery(shape: cpShape; p: cpVect; info: cpPointQueryInfo): cpFloat; cdecl; external name 'cpShapePointQuery';
function _cpShapeSegmentQuery(shape: cpShape; a, b: cpVect; radius: cpFloat; info: cpSegmentQueryInfo): cpBool; cdecl; external name 'cpShapeSegmentQuery';
function _cpShapesCollide(a, b: cpShape): cpContactPointSetStruct; cdecl; external name 'cpShapesCollide';
function _cpShapeGetSpace(shape: cpShape): cpSpace; cdecl; external name 'cpShapeGetSpace';
function _cpShapeGetBody(shape: cpShape): cpBody; cdecl; external name 'cpShapeGetBody';
procedure _cpShapeSetBody(shape: cpShape; body: cpBody); cdecl; external name 'cpShapeSetBody';
function _cpShapeGetMass(shape: cpShape): cpFloat; cdecl; external name 'cpShapeGetMass';
procedure _cpShapeSetMass(shape: cpShape; mass: cpFloat); cdecl; external name 'cpShapeSetMass';
function _cpShapeGetDensity(shape: cpShape): cpFloat; cdecl; external name 'cpShapeGetDensity';
procedure _cpShapeSetDensity(shape: cpShape; density: cpFloat); cdecl; external name 'cpShapeSetDensity';
function _cpShapeGetMoment(shape: cpShape): cpFloat; cdecl; external name 'cpShapeGetMoment';
function _cpShapeGetArea(shape: cpShape): cpFloat; cdecl; external name 'cpShapeGetArea';
function _cpShapeGetCenterOfGravity(shape: cpShape): cpVect; cdecl; external name 'cpShapeGetCenterOfGravity';
function _cpShapeGetBB(shape: cpShape): cpBB; cdecl; external name 'cpShapeGetBB';
function _cpShapeGetSensor(shape: cpShape): cpBool; cdecl; external name 'cpShapeGetSensor';
procedure _cpShapeSetSensor(shape: cpShape; sensor: cpBool); cdecl; external name 'cpShapeSetSensor';
function _cpShapeGetElasticity(shape: cpShape): cpFloat; cdecl; external name 'cpShapeGetElasticity';
procedure _cpShapeSetElasticity(shape: cpShape; elasticity: cpFloat); cdecl; external name 'cpShapeSetElasticity';
function _cpShapeGetFriction(shape: cpShape): cpFloat; cdecl; external name 'cpShapeGetFriction';
procedure _cpShapeSetFriction(shape: cpShape; friction: cpFloat); cdecl; external name 'cpShapeSetFriction';
function _cpShapeGetSurfaceVelocity(shape: cpShape): cpVect; cdecl; external name 'cpShapeGetSurfaceVelocity';
procedure _cpShapeSetSurfaceVelocity(shape: cpShape; surfaceVelocity: cpVect); cdecl; external name 'cpShapeSetSurfaceVelocity';
function _cpShapeGetUserData(shape: cpShape): cpDataPointer; cdecl; external name 'cpShapeGetUserData';
procedure _cpShapeSetUserData(shape: cpShape; userData: cpDataPointer); cdecl; external name 'cpShapeSetUserData';
function _cpShapeGetCollisionType(shape: cpShape): cpCollisionType; cdecl; external name 'cpShapeGetCollisionType';
procedure _cpShapeSetCollisionType(shape: cpShape; collisionType: cpCollisionType); cdecl; external name 'cpShapeSetCollisionType';
function _cpShapeGetFilter(shape: cpShape): cpShapeFilter; cdecl; external name 'cpShapeGetFilter';
procedure _cpShapeSetFilter(shape: cpShape; filter: cpShapeFilter); cdecl; external name 'cpShapeSetFilter';
function _cpCircleShapeAlloc: cpCircleShape; cdecl; external name 'cpCircleShapeAlloc';
function _cpCircleShapeInit(circle: cpCircleShape; body: cpBody; radius: cpFloat; offset: cpVect): cpCircleShape; cdecl; external name 'cpCircleShapeInit';
function _cpCircleShapeNew(body: cpBody; radius: cpFloat; offset: cpVect): cpShape; cdecl; external name 'cpCircleShapeNew';
function _cpCircleShapeGetOffset(shape: cpShape): cpVect; cdecl; external name 'cpCircleShapeGetOffset';
function _cpCircleShapeGetRadius(shape: cpShape): cpFloat; cdecl; external name 'cpCircleShapeGetRadius';
procedure _cpCircleShapeSetRadius(shape: cpShape; radius: cpFloat); cdecl; external name 'cpCircleShapeSetRadius';
procedure _cpCircleShapeSetOffset(shape: cpShape; offset: cpVect); cdecl; external name 'cpCircleShapeSetOffset';
function _cpSegmentShapeAlloc: cpSegmentShape; cdecl; external name 'cpSegmentShapeAlloc';
function _cpSegmentShapeInit(seg: cpSegmentShape; body: cpBody; a, b: cpVect; radius: cpFloat): cpSegmentShape; cdecl; external name 'cpSegmentShapeInit';
function _cpSegmentShapeNew(body: cpBody; a, b: cpVect; radius: cpFloat): cpShape; cdecl; external name 'cpSegmentShapeNew';
procedure _cpSegmentShapeSetNeighbors(shape: cpShape; prev, next: cpVect); cdecl; external name 'cpSegmentShapeSetNeighbors';
function _cpSegmentShapeGetA(shape: cpShape): cpVect; cdecl; external name 'cpSegmentShapeGetA';
function _cpSegmentShapeGetB(shape: cpShape): cpVect; cdecl; external name 'cpSegmentShapeGetB';
function _cpSegmentShapeGetNormal(shape: cpShape): cpVect; cdecl; external name 'cpSegmentShapeGetNormal';
function _cpSegmentShapeGetRadius(shape: cpShape): cpFloat; cdecl; external name 'cpSegmentShapeGetRadius';
procedure _cpSegmentShapeSetEndpoints(shape: cpShape; a, b: cpVect); cdecl; external name 'cpSegmentShapeSetEndpoints';
procedure _cpSegmentShapeSetRadius(shape: cpShape; radius: cpFloat); cdecl; external name 'cpSegmentShapeSetRadius';
function _cpPolyShapeAlloc: cpPolyShape; cdecl; external name 'cpPolyShapeAlloc';
function _cpPolyShapeInit(poly: cpPolyShape; body: cpBody; count: Integer; verts: PcpVect;
  transform: cpTransform; radius: cpFloat): cpPolyShape; cdecl; external name 'cpPolyShapeInit';
function _cpPolyShapeInitRaw(poly: cpPolyShape; body: cpBody; count: Integer; verts: PcpVect;
  radius: cpFloat): cpPolyShape; cdecl; external name 'cpPolyShapeInitRaw';
function _cpPolyShapeNew(body: cpBody; count: Integer; verts: PcpVect; transform: cpTransform;
  radius: cpFloat): cpShape; cdecl; external name 'cpPolyShapeNew';
function _cpPolyShapeNewRaw(body: cpBody; count: Integer; verts: PcpVect; radius: cpFloat): cpShape; cdecl; external name 'cpPolyShapeNewRaw';
function _cpBoxShapeInit(poly: cpPolyShape; body: cpBody; width, height, radius: cpFloat): cpPolyShape; cdecl; external name 'cpBoxShapeInit';
function _cpBoxShapeInit2(poly: cpPolyShape; body: cpBody; box: cpBB; radius: cpFloat): cpPolyShape; cdecl; external name 'cpBoxShapeInit2';
function _cpBoxShapeNew(body: cpBody; width, height, radius: cpFloat): cpShape; cdecl; external name 'cpBoxShapeNew';
function _cpBoxShapeNew2(body: cpBody; box: cpBB; radius: cpFloat): cpShape; cdecl; external name 'cpBoxShapeNew2';
function _cpPolyShapeGetCount(shape: cpShape): Integer; cdecl; external name 'cpPolyShapeGetCount';
function _cpPolyShapeGetVert(shape: cpShape; index: Integer): cpVect; cdecl; external name 'cpPolyShapeGetVert';
function _cpPolyShapeGetRadius(shape: cpShape): cpFloat; cdecl; external name 'cpPolyShapeGetRadius';
procedure _cpPolyShapeSetVerts(shape: cpShape; count: Integer; verts: PcpVect; transform: cpTransform); cdecl; external name 'cpPolyShapeSetVerts';
procedure _cpPolyShapeSetVertsRaw(shape: cpShape; count: Integer; verts: PcpVect); cdecl; external name 'cpPolyShapeSetVertsRaw';
procedure _cpPolyShapeSetRadius(shape: cpShape; radius: cpFloat); cdecl; external name 'cpPolyShapeSetRadius';
procedure _cpConstraintDestroy(constraint: cpConstraint); cdecl; external name 'cpConstraintDestroy';
procedure _cpConstraintFree(constraint: cpConstraint); cdecl; external name 'cpConstraintFree';
function _cpConstraintGetSpace(constraint: cpConstraint): cpSpace; cdecl; external name 'cpConstraintGetSpace';
function _cpConstraintGetBodyA(constraint: cpConstraint): cpBody; cdecl; external name 'cpConstraintGetBodyA';
function _cpConstraintGetBodyB(constraint: cpConstraint): cpBody; cdecl; external name 'cpConstraintGetBodyB';
function _cpConstraintGetMaxForce(constraint: cpConstraint): cpFloat; cdecl; external name 'cpConstraintGetMaxForce';
procedure _cpConstraintSetMaxForce(constraint: cpConstraint; maxForce: cpFloat); cdecl; external name 'cpConstraintSetMaxForce';
function _cpConstraintGetErrorBias(constraint: cpConstraint): cpFloat; cdecl; external name 'cpConstraintGetErrorBias';
procedure _cpConstraintSetErrorBias(constraint: cpConstraint; errorBias: cpFloat); cdecl; external name 'cpConstraintSetErrorBias';
function _cpConstraintGetMaxBias(constraint: cpConstraint): cpFloat; cdecl; external name 'cpConstraintGetMaxBias';
procedure _cpConstraintSetMaxBias(constraint: cpConstraint; maxBias: cpFloat); cdecl; external name 'cpConstraintSetMaxBias';
function _cpConstraintGetCollideBodies(constraint: cpConstraint): cpBool; cdecl; external name 'cpConstraintGetCollideBodies';
procedure _cpConstraintSetCollideBodies(constraint: cpConstraint; collideBodies: cpBool); cdecl; external name 'cpConstraintSetCollideBodies';
function _cpConstraintGetPreSolveFunc(constraint: cpConstraint): cpConstraintPreSolveFunc; cdecl; external name 'cpConstraintGetPreSolveFunc';
procedure _cpConstraintSetPreSolveFunc(constraint: cpConstraint; preSolveFunc: cpConstraintPreSolveFunc); cdecl; external name 'cpConstraintSetPreSolveFunc';
function _cpConstraintGetPostSolveFunc(constraint: cpConstraint): cpConstraintPostSolveFunc; cdecl; external name 'cpConstraintGetPostSolveFunc';
procedure _cpConstraintSetPostSolveFunc(constraint: cpConstraint; postSolveFunc: cpConstraintPostSolveFunc); cdecl; external name 'cpConstraintSetPostSolveFunc';
function _cpConstraintGetUserData(constraint: cpConstraint): cpDataPointer; cdecl; external name 'cpConstraintGetUserData';
procedure _cpConstraintSetUserData(constraint: cpConstraint; userData: cpDataPointer); cdecl; external name 'cpConstraintSetUserData';
function _cpConstraintGetImpulse(constraint: cpConstraint): cpFloat; cdecl; external name 'cpConstraintGetImpulse';
function _cpConstraintIsPinJoint(constraint: cpConstraint): cpBool; cdecl; external name 'cpConstraintIsPinJoint';
function _cpPinJointAlloc: cpPinJoint; cdecl; external name 'cpPinJointAlloc';
function _cpPinJointInit(joint: cpPinJoint; a, b: cpBody; anchorA, anchorB: cpVect): cpPinJoint; cdecl; external name 'cpPinJointInit';
function _cpPinJointNew(a, b: cpBody; anchorA, anchorB: cpVect): cpConstraint; cdecl; external name 'cpPinJointNew';
function _cpPinJointGetAnchorA(constraint: cpConstraint): cpVect; cdecl; external name 'cpPinJointGetAnchorA';
procedure _cpPinJointSetAnchorA(constraint: cpConstraint; anchorA: cpVect); cdecl; external name 'cpPinJointSetAnchorA';
function _cpPinJointGetAnchorB(constraint: cpConstraint): cpVect; cdecl; external name 'cpPinJointGetAnchorB';
procedure _cpPinJointSetAnchorB(constraint: cpConstraint; anchorB: cpVect); cdecl; external name 'cpPinJointSetAnchorB';
function _cpPinJointGetDist(constraint: cpConstraint): cpFloat; cdecl; external name 'cpPinJointGetDist';
procedure _cpPinJointSetDist(constraint: cpConstraint; dist: cpFloat); cdecl; external name 'cpPinJointSetDist';
function _cpConstraintIsSlideJoint(constraint: cpConstraint): cpBool; cdecl; external name 'cpConstraintIsSlideJoint';
function _cpSlideJointAlloc: cpSlideJoint; cdecl; external name 'cpSlideJointAlloc';
function _cpSlideJointInit(joint: cpSlideJoint; a, b: cpBody; anchorA, anchorB: cpVect; min, max: cpFloat): cpSlideJoint; cdecl; external name 'cpSlideJointInit';
function _cpSlideJointNew(a, b: cpBody; anchorA, anchorB: cpVect; min, max: cpFloat): cpConstraint; cdecl; external name 'cpSlideJointNew';
function _cpSlideJointGetAnchorA(constraint: cpConstraint): cpVect; cdecl; external name 'cpSlideJointGetAnchorA';
procedure _cpSlideJointSetAnchorA(constraint: cpConstraint; anchorA: cpVect); cdecl; external name 'cpSlideJointSetAnchorA';
function _cpSlideJointGetAnchorB(constraint: cpConstraint): cpVect; cdecl; external name 'cpSlideJointGetAnchorB';
procedure _cpSlideJointSetAnchorB(constraint: cpConstraint; anchorB: cpVect); cdecl; external name 'cpSlideJointSetAnchorB';
function _cpSlideJointGetMin(constraint: cpConstraint): cpFloat; cdecl; external name 'cpSlideJointGetMin';
procedure _cpSlideJointSetMin(constraint: cpConstraint; min: cpFloat); cdecl; external name 'cpSlideJointSetMin';
function _cpSlideJointGetMax(constraint: cpConstraint): cpFloat; cdecl; external name 'cpSlideJointGetMax';
procedure _cpSlideJointSetMax(constraint: cpConstraint; max: cpFloat); cdecl; external name 'cpSlideJointSetMax';
function _cpConstraintIsPivotJoint(constraint: cpConstraint): cpBool; cdecl; external name 'cpConstraintIsPivotJoint';
function _cpPivotJointAlloc: cpPivotJoint; cdecl; external name 'cpPivotJointAlloc';
function _cpPivotJointInit(joint: cpPivotJoint; a, b: cpBody; anchorA, anchorB: cpVect): cpPivotJoint; cdecl; external name 'cpPivotJointInit';
function _cpPivotJointNew(a, b: cpBody; pivot: cpVect): cpConstraint; cdecl; external name 'cpPivotJointNew';
function _cpPivotJointNew2(a, b: cpBody; anchorA, anchorB: cpVect): cpConstraint; cdecl; external name 'cpPivotJointNew2';
function _cpPivotJointGetAnchorA(constraint: cpConstraint): cpVect; cdecl; external name 'cpPivotJointGetAnchorA';
procedure _cpPivotJointSetAnchorA(constraint: cpConstraint; anchorA: cpVect); cdecl; external name 'cpPivotJointSetAnchorA';
function _cpPivotJointGetAnchorB(constraint: cpConstraint): cpVect; cdecl; external name 'cpPivotJointGetAnchorB';
procedure _cpPivotJointSetAnchorB(constraint: cpConstraint; anchorB: cpVect); cdecl; external name 'cpPivotJointSetAnchorB';
function _cpConstraintIsGrooveJoint(constraint: cpConstraint): cpBool; cdecl; external name 'cpConstraintIsGrooveJoint';
function _cpGrooveJointAlloc: cpGrooveJoint; cdecl; external name 'cpGrooveJointAlloc';
function _cpGrooveJointInit(joint: cpGrooveJoint; a, b: cpBody; groove_a, groove_b, anchorB: cpVect): cpGrooveJoint; cdecl; external name 'cpGrooveJointInit';
function _cpGrooveJointNew(a, b: cpBody; groove_a, groove_b, anchorB: cpVect): cpConstraint; cdecl; external name 'cpGrooveJointNew';
function _cpGrooveJointGetGrooveA(constraint: cpConstraint): cpVect; cdecl; external name 'cpGrooveJointGetGrooveA';
procedure _cpGrooveJointSetGrooveA(constraint: cpConstraint; grooveA: cpVect); cdecl; external name 'cpGrooveJointSetGrooveA';
function _cpGrooveJointGetGrooveB(constraint: cpConstraint): cpVect; cdecl; external name 'cpGrooveJointGetGrooveB';
procedure _cpGrooveJointSetGrooveB(constraint: cpConstraint; grooveB: cpVect); cdecl; external name 'cpGrooveJointSetGrooveB';
function _cpGrooveJointGetAnchorB(constraint: cpConstraint): cpVect; cdecl; external name 'cpGrooveJointGetAnchorB';
procedure _cpGrooveJointSetAnchorB(constraint: cpConstraint; anchorB: cpVect); cdecl; external name 'cpGrooveJointSetAnchorB';
function _cpConstraintIsDampedSpring(constraint: cpConstraint): cpBool; cdecl; external name 'cpConstraintIsDampedSpring';
function _cpDampedSpringAlloc: cpDampedSpring; cdecl; external name 'cpDampedSpringAlloc';
function _cpDampedSpringInit(joint: cpDampedSpring; a, b: cpBody; anchorA, anchorB: cpVect;
  restLength, stiffness, damping: cpFloat): cpDampedSpring; cdecl; external name 'cpDampedSpringInit';
function _cpDampedSpringNew(a, b: cpBody; anchorA, anchorB: cpVect;
  restLength, stiffness, damping: cpFloat): cpConstraint; cdecl; external name 'cpDampedSpringNew';
function _cpDampedSpringGetAnchorA(constraint: cpConstraint): cpVect; cdecl; external name 'cpDampedSpringGetAnchorA';
procedure _cpDampedSpringSetAnchorA(constraint: cpConstraint; anchorA: cpVect); cdecl; external name 'cpDampedSpringSetAnchorA';
function _cpDampedSpringGetAnchorB(constraint: cpConstraint): cpVect; cdecl; external name 'cpDampedSpringGetAnchorB';
procedure _cpDampedSpringSetAnchorB(constraint: cpConstraint; anchorB: cpVect); cdecl; external name 'cpDampedSpringSetAnchorB';
function _cpDampedSpringGetRestLength(constraint: cpConstraint): cpFloat; cdecl; external name 'cpDampedSpringGetRestLength';
procedure _cpDampedSpringSetRestLength(constraint: cpConstraint; restLength: cpFloat); cdecl; external name 'cpDampedSpringSetRestLength';
function _cpDampedSpringGetStiffness(constraint: cpConstraint): cpFloat; cdecl; external name 'cpDampedSpringGetStiffness';
procedure _cpDampedSpringSetStiffness(constraint: cpConstraint; stiffness: cpFloat); cdecl; external name 'cpDampedSpringSetStiffness';
function _cpDampedSpringGetDamping(constraint: cpConstraint): cpFloat; cdecl; external name 'cpDampedSpringGetDamping';
procedure _cpDampedSpringSetDamping(constraint: cpConstraint; damping: cpFloat); cdecl; external name 'cpDampedSpringSetDamping';
function _cpDampedSpringGetSpringForceFunc(constraint: cpConstraint): cpDampedSpringForceFunc; cdecl; external name 'cpDampedSpringGetSpringForceFunc';
procedure _cpDampedSpringSetSpringForceFunc(constraint: cpConstraint; springForceFunc: cpDampedSpringForceFunc); cdecl; external name 'cpDampedSpringSetSpringForceFunc';
function _cpConstraintIsDampedRotarySpring(constraint: cpConstraint): cpBool; cdecl; external name 'cpConstraintIsDampedRotarySpring';
function _cpDampedRotarySpringAlloc: cpDampedRotarySpring; cdecl; external name 'cpDampedRotarySpringAlloc';
function _cpDampedRotarySpringInit(joint: cpDampedRotarySpring; a, b: cpBody;
  restAngle, stiffness, damping: cpFloat): cpDampedRotarySpring; cdecl; external name 'cpDampedRotarySpringInit';
function _cpDampedRotarySpringNew(a, b: cpBody; restAngle, stiffness, damping: cpFloat): cpConstraint; cdecl; external name 'cpDampedRotarySpringNew';
function _cpDampedRotarySpringGetRestAngle(constraint: cpConstraint): cpFloat; cdecl; external name 'cpDampedRotarySpringGetRestAngle';
procedure _cpDampedRotarySpringSetRestAngle(constraint: cpConstraint; restAngle: cpFloat); cdecl; external name 'cpDampedRotarySpringSetRestAngle';
function _cpDampedRotarySpringGetStiffness(constraint: cpConstraint): cpFloat; cdecl; external name 'cpDampedRotarySpringGetStiffness';
procedure _cpDampedRotarySpringSetStiffness(constraint: cpConstraint; stiffness: cpFloat); cdecl; external name 'cpDampedRotarySpringSetStiffness';
function _cpDampedRotarySpringGetDamping(constraint: cpConstraint): cpFloat; cdecl; external name 'cpDampedRotarySpringGetDamping';
procedure _cpDampedRotarySpringSetDamping(constraint: cpConstraint; damping: cpFloat); cdecl; external name 'cpDampedRotarySpringSetDamping';
function _cpDampedRotarySpringGetSpringTorqueFunc(constraint: cpConstraint): cpDampedRotarySpringTorqueFunc; cdecl; external name 'cpDampedRotarySpringGetSpringTorqueFunc';
procedure _cpDampedRotarySpringSetSpringTorqueFunc(constraint: cpConstraint;
  springTorqueFunc: cpDampedRotarySpringTorqueFunc); cdecl; external name 'cpDampedRotarySpringSetSpringTorqueFunc';
function _cpConstraintIsRotaryLimitJoint(constraint: cpConstraint): cpBool; cdecl; external name 'cpConstraintIsRotaryLimitJoint';
function _cpRotaryLimitJointAlloc: cpRotaryLimitJoint; cdecl; external name 'cpRotaryLimitJointAlloc';
function _cpRotaryLimitJointInit(joint: cpRotaryLimitJoint; a, b: cpBody; min, max: cpFloat): cpRotaryLimitJoint; cdecl; external name 'cpRotaryLimitJointInit';
function _cpRotaryLimitJointNew(a, b: cpBody; min, max: cpFloat): cpConstraint; cdecl; external name 'cpRotaryLimitJointNew';
function _cpRotaryLimitJointGetMin(constraint: cpConstraint): cpFloat; cdecl; external name 'cpRotaryLimitJointGetMin';
procedure _cpRotaryLimitJointSetMin(constraint: cpConstraint; min: cpFloat); cdecl; external name 'cpRotaryLimitJointSetMin';
function _cpRotaryLimitJointGetMax(constraint: cpConstraint): cpFloat; cdecl; external name 'cpRotaryLimitJointGetMax';
procedure _cpRotaryLimitJointSetMax(constraint: cpConstraint; max: cpFloat); cdecl; external name 'cpRotaryLimitJointSetMax';
function _cpConstraintIsRatchetJoint(constraint: cpConstraint): cpBool; cdecl; external name 'cpConstraintIsRatchetJoint';
function _cpRatchetJointAlloc: cpRatchetJoint; cdecl; external name 'cpRatchetJointAlloc';
function _cpRatchetJointInit(joint: cpRatchetJoint; a, b: cpBody; phase, ratchet: cpFloat): cpRatchetJoint; cdecl; external name 'cpRatchetJointInit';
function _cpRatchetJointNew(a, b: cpBody; phase, ratchet: cpFloat): cpConstraint; cdecl; external name 'cpRatchetJointNew';
function _cpRatchetJointGetAngle(constraint: cpConstraint): cpFloat; cdecl; external name 'cpRatchetJointGetAngle';
procedure _cpRatchetJointSetAngle(constraint: cpConstraint; angle: cpFloat); cdecl; external name 'cpRatchetJointSetAngle';
function _cpRatchetJointGetPhase(constraint: cpConstraint): cpFloat; cdecl; external name 'cpRatchetJointGetPhase';
procedure _cpRatchetJointSetPhase(constraint: cpConstraint; phase: cpFloat); cdecl; external name 'cpRatchetJointSetPhase';
function _cpRatchetJointGetRatchet(constraint: cpConstraint): cpFloat; cdecl; external name 'cpRatchetJointGetRatchet';
procedure _cpRatchetJointSetRatchet(constraint: cpConstraint; ratchet: cpFloat); cdecl; external name 'cpRatchetJointSetRatchet';
function _cpConstraintIsGearJoint(constraint: cpConstraint): cpBool; cdecl; external name 'cpConstraintIsGearJoint';
function _cpGearJointAlloc: cpGearJoint; cdecl; external name 'cpGearJointAlloc';
function _cpGearJointInit(joint: cpGearJoint; a, b: cpBody; phase, ratio: cpFloat): cpGearJoint; cdecl; external name 'cpGearJointInit';
function _cpGearJointNew(a, b: cpBody; phase, ratio: cpFloat): cpConstraint; cdecl; external name 'cpGearJointNew';
function _cpGearJointGetPhase(constraint: cpConstraint): cpFloat; cdecl; external name 'cpGearJointGetPhase';
procedure _cpGearJointSetPhase(constraint: cpConstraint; phase: cpFloat); cdecl; external name 'cpGearJointSetPhase';
function _cpGearJointGetRatio(constraint: cpConstraint): cpFloat; cdecl; external name 'cpGearJointGetRatio';
procedure _cpGearJointSetRatio(constraint: cpConstraint; ratio: cpFloat); cdecl; external name 'cpGearJointSetRatio';
function _cpConstraintIsSimpleMotor(constraint: cpConstraint): cpBool; cdecl; external name 'cpConstraintIsSimpleMotor';
function _cpSimpleMotorAlloc: cpSimpleMotor; cdecl; external name 'cpSimpleMotorAlloc';
function _cpSimpleMotorInit(joint: cpSimpleMotor; a, b: cpBody; rate: cpFloat): cpSimpleMotor; cdecl; external name 'cpSimpleMotorInit';
function _cpSimpleMotorNew(a, b: cpBody; rate: cpFloat): cpConstraint; cdecl; external name 'cpSimpleMotorNew';
function _cpSimpleMotorGetRate(constraint: cpConstraint): cpFloat; cdecl; external name 'cpSimpleMotorGetRate';
procedure _cpSimpleMotorSetRate(constraint: cpConstraint; rate: cpFloat); cdecl; external name 'cpSimpleMotorSetRate';
function _cpSpaceHashNew(celldim: cpFloat; cells: Integer; bbfunc: cpSpatialIndexBBFunc;
  staticIndex: cpSpatialIndex): cpSpatialIndex; cdecl; external name 'cpSpaceHashNew';
procedure _cpSpaceHashResize(hash: cpSpatialIndex; celldim: cpFloat; numcells: Integer); cdecl; external name 'cpSpaceHashResize';
function _cpBBTreeNew(bbfunc: cpSpatialIndexBBFunc; staticIndex: cpSpatialIndex): cpSpatialIndex; cdecl; external name 'cpBBTreeNew';
procedure _cpBBTreeOptimize(index: cpSpatialIndex); cdecl; external name 'cpBBTreeOptimize';
procedure _cpBBTreeSetVelocityFunc(index: cpSpatialIndex; func: cpBBTreeVelocityFunc); cdecl; external name 'cpBBTreeSetVelocityFunc';
function _cpSweep1DNew(bbfunc: cpSpatialIndexBBFunc; staticIndex: cpSpatialIndex): cpSpatialIndex; cdecl; external name 'cpSweep1DNew';
procedure _cpSpatialIndexFree(index: cpSpatialIndex); cdecl; external name 'cpSpatialIndexFree';
procedure _cpSpatialIndexCollideStatic(dynamicIndex, staticIndex: cpSpatialIndex;
  func: cpSpatialIndexQueryFunc; data: Pointer); cdecl; external name 'cpSpatialIndexCollideStatic';
function _cpSpaceAlloc: cpSpace; cdecl; external name 'cpSpaceAlloc';
function _cpSpaceInit(space: cpSpace): cpSpace; cdecl; external name 'cpSpaceInit';
function _cpSpaceNew: cpSpace; cdecl; external name 'cpSpaceNew';
procedure _cpSpaceDestroy(space: cpSpace); cdecl; external name 'cpSpaceDestroy';
procedure _cpSpaceFree(space: cpSpace); cdecl; external name 'cpSpaceFree';
function _cpSpaceGetIterations(space: cpSpace): Integer; cdecl; external name 'cpSpaceGetIterations';
procedure _cpSpaceSetIterations(space: cpSpace; iterations: Integer); cdecl; external name 'cpSpaceSetIterations';
function _cpSpaceGetGravity(space: cpSpace): cpVect; cdecl; external name 'cpSpaceGetGravity';
procedure _cpSpaceSetGravity(space: cpSpace; gravity: cpVect); cdecl; external name 'cpSpaceSetGravity';
function _cpSpaceGetDamping(space: cpSpace): cpFloat; cdecl; external name 'cpSpaceGetDamping';
procedure _cpSpaceSetDamping(space: cpSpace; damping: cpFloat); cdecl; external name 'cpSpaceSetDamping';
function _cpSpaceGetIdleSpeedThreshold(space: cpSpace): cpFloat; cdecl; external name 'cpSpaceGetIdleSpeedThreshold';
procedure _cpSpaceSetIdleSpeedThreshold(space: cpSpace; idleSpeedThreshold: cpFloat); cdecl; external name 'cpSpaceSetIdleSpeedThreshold';
function _cpSpaceGetSleepTimeThreshold(space: cpSpace): cpFloat; cdecl; external name 'cpSpaceGetSleepTimeThreshold';
procedure _cpSpaceSetSleepTimeThreshold(space: cpSpace; sleepTimeThreshold: cpFloat); cdecl; external name 'cpSpaceSetSleepTimeThreshold';
function _cpSpaceGetCollisionSlop(space: cpSpace): cpFloat; cdecl; external name 'cpSpaceGetCollisionSlop';
procedure _cpSpaceSetCollisionSlop(space: cpSpace; collisionSlop: cpFloat); cdecl; external name 'cpSpaceSetCollisionSlop';
function _cpSpaceGetCollisionBias(space: cpSpace): cpFloat; cdecl; external name 'cpSpaceGetCollisionBias';
procedure _cpSpaceSetCollisionBias(space: cpSpace; collisionBias: cpFloat); cdecl; external name 'cpSpaceSetCollisionBias';
function _cpSpaceGetCollisionPersistence(space: cpSpace): cpTimestamp; cdecl; external name 'cpSpaceGetCollisionPersistence';
procedure _cpSpaceSetCollisionPersistence(space: cpSpace; collisionPersistence: cpTimestamp); cdecl; external name 'cpSpaceSetCollisionPersistence';
function _cpSpaceGetUserData(space: cpSpace): cpDataPointer; cdecl; external name 'cpSpaceGetUserData';
procedure _cpSpaceSetUserData(space: cpSpace; userData: cpDataPointer); cdecl; external name 'cpSpaceSetUserData';
function _cpSpaceGetStaticBody(space: cpSpace): cpBody; cdecl; external name 'cpSpaceGetStaticBody';
function _cpSpaceGetCurrentTimeStep(space: cpSpace): cpFloat; cdecl; external name 'cpSpaceGetCurrentTimeStep';
function _cpSpaceIsLocked(space: cpSpace): cpBool; cdecl; external name 'cpSpaceIsLocked';
function _cpSpaceAddDefaultCollisionHandler(space: cpSpace): cpCollisionHandler; cdecl; external name 'cpSpaceAddDefaultCollisionHandler';
function _cpSpaceAddCollisionHandler(space: cpSpace; a, b: cpCollisionType): cpCollisionHandler; cdecl; external name 'cpSpaceAddCollisionHandler';
function _cpSpaceAddWildcardHandler(space: cpSpace; kind: cpCollisionType): cpCollisionHandler; cdecl; external name 'cpSpaceAddWildcardHandler';
function _cpSpaceAddShape(space: cpSpace; shape: cpShape): cpShape; cdecl; external name 'cpSpaceAddShape';
function _cpSpaceAddBody(space: cpSpace; body: cpBody): cpBody; cdecl; external name 'cpSpaceAddBody';
function _cpSpaceAddConstraint(space: cpSpace; constraint: cpConstraint): cpConstraint; cdecl; external name 'cpSpaceAddConstraint';
procedure _cpSpaceRemoveShape(space: cpSpace; shape: cpShape); cdecl; external name 'cpSpaceRemoveShape';
procedure _cpSpaceRemoveBody(space: cpSpace; body: cpBody); cdecl; external name 'cpSpaceRemoveBody';
procedure _cpSpaceRemoveConstraint(space: cpSpace; constraint: cpConstraint); cdecl; external name 'cpSpaceRemoveConstraint';
function _cpSpaceContainsShape(space: cpSpace; shape: cpShape): cpBool; cdecl; external name 'cpSpaceContainsShape';
function _cpSpaceContainsBody(space: cpSpace; body: cpBody): cpBool; cdecl; external name 'cpSpaceContainsBody';
function _cpSpaceContainsConstraint(space: cpSpace; constraint: cpConstraint): cpBool; cdecl; external name 'cpSpaceContainsConstraint';
function _cpSpaceAddPostStepCallback(space: cpSpace; func: cpPostStepFunc; key, data: Pointer): cpBool; cdecl; external name 'cpSpaceAddPostStepCallback';
procedure _cpSpacePointQuery(space: cpSpace; point: cpVect; maxDistance: cpFloat; filter: cpShapeFilter;
  func: cpSpacePointQueryFunc; data: Pointer); cdecl; external name 'cpSpacePointQuery';
function _cpSpacePointQueryNearest(space: cpSpace; point: cpVect; maxDistance: cpFloat; filter: cpShapeFilter;
  info: cpPointQueryInfo): cpShape; cdecl; external name 'cpSpacePointQueryNearest';
procedure _cpSpaceSegmentQuery(space: cpSpace; start, finish: cpVect; radius: cpFloat; filter: cpShapeFilter;
  func: cpSpaceSegmentQueryFunc; data: Pointer); cdecl; external name 'cpSpaceSegmentQuery';
function _cpSpaceSegmentQueryFirst(space: cpSpace; start, finish: cpVect; radius: cpFloat; filter: cpShapeFilter;
  info: cpSegmentQueryInfo): cpShape; cdecl; external name 'cpSpaceSegmentQueryFirst';
procedure _cpSpaceBBQuery(space: cpSpace; bb: cpBB; filter: cpShapeFilter; func: cpSpaceBBQueryFunc; data: Pointer); cdecl; external name 'cpSpaceBBQuery';
function _cpSpaceShapeQuery(space: cpSpace; shape: cpShape; func: cpSpaceShapeQueryFunc; data: Pointer): cpBool; cdecl; external name 'cpSpaceShapeQuery';
procedure _cpSpaceEachBody(space: cpSpace; func: cpSpaceBodyIteratorFunc; data: Pointer); cdecl; external name 'cpSpaceEachBody';
procedure _cpSpaceEachShape(space: cpSpace; func: cpSpaceShapeIteratorFunc; data: Pointer); cdecl; external name 'cpSpaceEachShape';
procedure _cpSpaceEachConstraint(space: cpSpace; func: cpSpaceConstraintIteratorFunc; data: Pointer); cdecl; external name 'cpSpaceEachConstraint';
procedure _cpSpaceReindexStatic(space: cpSpace); cdecl; external name 'cpSpaceReindexStatic';
procedure _cpSpaceReindexShape(space: cpSpace; shape: cpShape); cdecl; external name 'cpSpaceReindexShape';
procedure _cpSpaceReindexShapesForBody(space: cpSpace; body: cpBody); cdecl; external name 'cpSpaceReindexShapesForBody';
procedure _cpSpaceUseSpatialHash(space: cpSpace; dim: cpFloat; count: Integer); cdecl; external name 'cpSpaceUseSpatialHash';
procedure _cpSpaceStep(space: cpSpace; dt: cpFloat); cdecl; external name 'cpSpaceStep';
procedure _cpSpaceDebugDraw(space: cpSpace; options: PcpSpaceDebugDrawOptions); cdecl; external name 'cpSpaceDebugDraw';
procedure _cpMarchSoft(bb: cpBB; x_samples, y_samples: cpULong; threshold: cpFloat;
  segment: cpMarchSegmentFunc; segment_data: Pointer; sample: cpMarchSampleFunc; sample_data: Pointer); cdecl; external name 'cpMarchSoft';
procedure _cpMarchHard(bb: cpBB; x_samples, y_samples: cpULong; threshold: cpFloat;
  segment: cpMarchSegmentFunc; segment_data: Pointer; sample: cpMarchSampleFunc; sample_data: Pointer); cdecl; external name 'cpMarchHard';
procedure _cpPolylineFree(line: cpPolyline); cdecl; external name 'cpPolylineFree';
function _cpPolylineIsClosed(line: cpPolyline): cpBool; cdecl; external name 'cpPolylineIsClosed';
function _cpPolylineSimplifyCurves(line: cpPolyline; tol: cpFloat): cpPolyline; cdecl; external name 'cpPolylineSimplifyCurves';
function _cpPolylineSimplifyVertexes(line: cpPolyline; tol: cpFloat): cpPolyline; cdecl; external name 'cpPolylineSimplifyVertexes';
function _cpPolylineToConvexHull(line: cpPolyline; tol: cpFloat): cpPolyline; cdecl; external name 'cpPolylineToConvexHull';
function _cpPolylineSetAlloc: cpPolylineSet; cdecl; external name 'cpPolylineSetAlloc';
function _cpPolylineSetInit(set_: cpPolylineSet): cpPolylineSet; cdecl; external name 'cpPolylineSetInit';
function _cpPolylineSetNew: cpPolylineSet; cdecl; external name 'cpPolylineSetNew';
procedure _cpPolylineSetDestroy(set_: cpPolylineSet; freePolylines: cpBool); cdecl; external name 'cpPolylineSetDestroy';
procedure _cpPolylineSetFree(set_: cpPolylineSet; freePolylines: cpBool); cdecl; external name 'cpPolylineSetFree';
procedure _cpPolylineSetCollectSegment(v0, v1: cpVect; lines: Pointer); cdecl; external name 'cpPolylineSetCollectSegment';
function _cpPolylineConvexDecomposition(line: cpPolyline; tol: cpFloat): cpPolylineSet; cdecl; external name 'cpPolylineConvexDecomposition';

{ Each Chipmunk function masks floating point exceptions and calls the library }

function cpMomentForCircle(m, r1, r2: cpFloat; offset: cpVect): cpFloat;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpMomentForCircle(m, r1, r2, offset);
  cpFloatLeave(mask);
end;

function cpAreaForCircle(r1, r2: cpFloat): cpFloat;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpAreaForCircle(r1, r2);
  cpFloatLeave(mask);
end;

function cpMomentForSegment(m: cpFloat; a, b: cpVect; radius: cpFloat): cpFloat;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpMomentForSegment(m, a, b, radius);
  cpFloatLeave(mask);
end;

function cpAreaForSegment(a, b: cpVect; radius: cpFloat): cpFloat;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpAreaForSegment(a, b, radius);
  cpFloatLeave(mask);
end;

function cpMomentForPoly(m: cpFloat; count: Integer; verts: PcpVect; offset: cpVect; radius: cpFloat): cpFloat;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpMomentForPoly(m, count, verts, offset, radius);
  cpFloatLeave(mask);
end;

function cpAreaForPoly(count: Integer; verts: PcpVect; radius: cpFloat): cpFloat;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpAreaForPoly(count, verts, radius);
  cpFloatLeave(mask);
end;

function cpCentroidForPoly(count: Integer; verts: PcpVect): cpVect;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpCentroidForPoly(count, verts);
  cpFloatLeave(mask);
end;

function cpMomentForBox(m, width, height: cpFloat): cpFloat;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpMomentForBox(m, width, height);
  cpFloatLeave(mask);
end;

function cpMomentForBox2(m: cpFloat; box: cpBB): cpFloat;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpMomentForBox2(m, box);
  cpFloatLeave(mask);
end;

function cpConvexHull(count: Integer; verts, result_: PcpVect; first: PInteger; tol: cpFloat): Integer;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpConvexHull(count, verts, result_, first, tol);
  cpFloatLeave(mask);
end;

function cpArbiterGetRestitution(arb: cpArbiter): cpFloat;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpArbiterGetRestitution(arb);
  cpFloatLeave(mask);
end;

procedure cpArbiterSetRestitution(arb: cpArbiter; restitution: cpFloat);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpArbiterSetRestitution(arb, restitution);
  cpFloatLeave(mask);
end;

function cpArbiterGetFriction(arb: cpArbiter): cpFloat;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpArbiterGetFriction(arb);
  cpFloatLeave(mask);
end;

procedure cpArbiterSetFriction(arb: cpArbiter; friction: cpFloat);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpArbiterSetFriction(arb, friction);
  cpFloatLeave(mask);
end;

function cpArbiterGetSurfaceVelocity(arb: cpArbiter): cpVect;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpArbiterGetSurfaceVelocity(arb);
  cpFloatLeave(mask);
end;

procedure cpArbiterSetSurfaceVelocity(arb: cpArbiter; vr: cpVect);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpArbiterSetSurfaceVelocity(arb, vr);
  cpFloatLeave(mask);
end;

function cpArbiterGetUserData(arb: cpArbiter): cpDataPointer;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpArbiterGetUserData(arb);
  cpFloatLeave(mask);
end;

procedure cpArbiterSetUserData(arb: cpArbiter; userData: cpDataPointer);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpArbiterSetUserData(arb, userData);
  cpFloatLeave(mask);
end;

function cpArbiterTotalImpulse(arb: cpArbiter): cpVect;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpArbiterTotalImpulse(arb);
  cpFloatLeave(mask);
end;

function cpArbiterTotalKE(arb: cpArbiter): cpFloat;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpArbiterTotalKE(arb);
  cpFloatLeave(mask);
end;

function cpArbiterIgnore(arb: cpArbiter): cpBool;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpArbiterIgnore(arb);
  cpFloatLeave(mask);
end;

procedure cpArbiterGetShapes(arb: cpArbiter; out a, b: cpShape);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpArbiterGetShapes(arb, a, b);
  cpFloatLeave(mask);
end;

procedure cpArbiterGetBodies(arb: cpArbiter; out a, b: cpBody);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpArbiterGetBodies(arb, a, b);
  cpFloatLeave(mask);
end;

function cpArbiterGetContactPointSet(arb: cpArbiter): cpContactPointSetStruct;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpArbiterGetContactPointSet(arb);
  cpFloatLeave(mask);
end;

procedure cpArbiterSetContactPointSet(arb: cpArbiter; set_: cpContactPointSet);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpArbiterSetContactPointSet(arb, set_);
  cpFloatLeave(mask);
end;

function cpArbiterIsFirstContact(arb: cpArbiter): cpBool;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpArbiterIsFirstContact(arb);
  cpFloatLeave(mask);
end;

function cpArbiterIsRemoval(arb: cpArbiter): cpBool;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpArbiterIsRemoval(arb);
  cpFloatLeave(mask);
end;

function cpArbiterGetCount(arb: cpArbiter): Integer;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpArbiterGetCount(arb);
  cpFloatLeave(mask);
end;

function cpArbiterGetNormal(arb: cpArbiter): cpVect;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpArbiterGetNormal(arb);
  cpFloatLeave(mask);
end;

function cpArbiterGetPointA(arb: cpArbiter; i: Integer): cpVect;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpArbiterGetPointA(arb, i);
  cpFloatLeave(mask);
end;

function cpArbiterGetPointB(arb: cpArbiter; i: Integer): cpVect;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpArbiterGetPointB(arb, i);
  cpFloatLeave(mask);
end;

function cpArbiterGetDepth(arb: cpArbiter; i: Integer): cpFloat;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpArbiterGetDepth(arb, i);
  cpFloatLeave(mask);
end;

function cpArbiterCallWildcardBeginA(arb: cpArbiter; space: cpSpace): cpBool;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpArbiterCallWildcardBeginA(arb, space);
  cpFloatLeave(mask);
end;

function cpArbiterCallWildcardBeginB(arb: cpArbiter; space: cpSpace): cpBool;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpArbiterCallWildcardBeginB(arb, space);
  cpFloatLeave(mask);
end;

function cpArbiterCallWildcardPreSolveA(arb: cpArbiter; space: cpSpace): cpBool;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpArbiterCallWildcardPreSolveA(arb, space);
  cpFloatLeave(mask);
end;

function cpArbiterCallWildcardPreSolveB(arb: cpArbiter; space: cpSpace): cpBool;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpArbiterCallWildcardPreSolveB(arb, space);
  cpFloatLeave(mask);
end;

procedure cpArbiterCallWildcardPostSolveA(arb: cpArbiter; space: cpSpace);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpArbiterCallWildcardPostSolveA(arb, space);
  cpFloatLeave(mask);
end;

procedure cpArbiterCallWildcardPostSolveB(arb: cpArbiter; space: cpSpace);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpArbiterCallWildcardPostSolveB(arb, space);
  cpFloatLeave(mask);
end;

procedure cpArbiterCallWildcardSeparateA(arb: cpArbiter; space: cpSpace);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpArbiterCallWildcardSeparateA(arb, space);
  cpFloatLeave(mask);
end;

procedure cpArbiterCallWildcardSeparateB(arb: cpArbiter; space: cpSpace);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpArbiterCallWildcardSeparateB(arb, space);
  cpFloatLeave(mask);
end;

function cpBodyAlloc: cpBody;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpBodyAlloc;
  cpFloatLeave(mask);
end;

function cpBodyInit(body: cpBody; mass, moment: cpFloat): cpBody;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpBodyInit(body, mass, moment);
  cpFloatLeave(mask);
end;

function cpBodyNew(mass, moment: cpFloat): cpBody;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpBodyNew(mass, moment);
  cpFloatLeave(mask);
end;

function cpBodyNewKinematic: cpBody;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpBodyNewKinematic;
  cpFloatLeave(mask);
end;

function cpBodyNewStatic: cpBody;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpBodyNewStatic;
  cpFloatLeave(mask);
end;

procedure cpBodyDestroy(body: cpBody);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpBodyDestroy(body);
  cpFloatLeave(mask);
end;

procedure cpBodyFree(body: cpBody);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpBodyFree(body);
  cpFloatLeave(mask);
end;

procedure cpBodyActivate(body: cpBody);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpBodyActivate(body);
  cpFloatLeave(mask);
end;

procedure cpBodyActivateStatic(body: cpBody; filter: cpShape);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpBodyActivateStatic(body, filter);
  cpFloatLeave(mask);
end;

procedure cpBodySleep(body: cpBody);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpBodySleep(body);
  cpFloatLeave(mask);
end;

procedure cpBodySleepWithGroup(body: cpBody; group: cpBody);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpBodySleepWithGroup(body, group);
  cpFloatLeave(mask);
end;

function cpBodyIsSleeping(body: cpBody): cpBool;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpBodyIsSleeping(body);
  cpFloatLeave(mask);
end;

function cpBodyGetType(body: cpBody): cpBodyType;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpBodyGetType(body);
  cpFloatLeave(mask);
end;

procedure cpBodySetType(body: cpBody; kind: cpBodyType);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  try
    _cpBodySetType(body, kind);
  finally
    cpFloatLeave(mask);
  end;
end;

function cpBodyGetSpace(body: cpBody): cpSpace;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpBodyGetSpace(body);
  cpFloatLeave(mask);
end;

function cpBodyGetMass(body: cpBody): cpFloat;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpBodyGetMass(body);
  cpFloatLeave(mask);
end;

procedure cpBodySetMass(body: cpBody; m: cpFloat);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpBodySetMass(body, m);
  cpFloatLeave(mask);
end;

function cpBodyGetMoment(body: cpBody): cpFloat;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpBodyGetMoment(body);
  cpFloatLeave(mask);
end;

procedure cpBodySetMoment(body: cpBody; i: cpFloat);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpBodySetMoment(body, i);
  cpFloatLeave(mask);
end;

function cpBodyGetPosition(body: cpBody): cpVect;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpBodyGetPosition(body);
  cpFloatLeave(mask);
end;

procedure cpBodySetPosition(body: cpBody; pos: cpVect);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpBodySetPosition(body, pos);
  cpFloatLeave(mask);
end;

function cpBodyGetCenterOfGravity(body: cpBody): cpVect;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpBodyGetCenterOfGravity(body);
  cpFloatLeave(mask);
end;

procedure cpBodySetCenterOfGravity(body: cpBody; cog: cpVect);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpBodySetCenterOfGravity(body, cog);
  cpFloatLeave(mask);
end;

function cpBodyGetVelocity(body: cpBody): cpVect;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpBodyGetVelocity(body);
  cpFloatLeave(mask);
end;

procedure cpBodySetVelocity(body: cpBody; velocity: cpVect);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpBodySetVelocity(body, velocity);
  cpFloatLeave(mask);
end;

function cpBodyGetForce(body: cpBody): cpVect;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpBodyGetForce(body);
  cpFloatLeave(mask);
end;

procedure cpBodySetForce(body: cpBody; force: cpVect);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpBodySetForce(body, force);
  cpFloatLeave(mask);
end;

function cpBodyGetAngle(body: cpBody): cpFloat;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpBodyGetAngle(body);
  cpFloatLeave(mask);
end;

procedure cpBodySetAngle(body: cpBody; a: cpFloat);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpBodySetAngle(body, a);
  cpFloatLeave(mask);
end;

function cpBodyGetAngularVelocity(body: cpBody): cpFloat;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpBodyGetAngularVelocity(body);
  cpFloatLeave(mask);
end;

procedure cpBodySetAngularVelocity(body: cpBody; angularVelocity: cpFloat);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpBodySetAngularVelocity(body, angularVelocity);
  cpFloatLeave(mask);
end;

function cpBodyGetTorque(body: cpBody): cpFloat;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpBodyGetTorque(body);
  cpFloatLeave(mask);
end;

procedure cpBodySetTorque(body: cpBody; torque: cpFloat);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpBodySetTorque(body, torque);
  cpFloatLeave(mask);
end;

function cpBodyGetRotation(body: cpBody): cpVect;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpBodyGetRotation(body);
  cpFloatLeave(mask);
end;

function cpBodyGetUserData(body: cpBody): cpDataPointer;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpBodyGetUserData(body);
  cpFloatLeave(mask);
end;

procedure cpBodySetUserData(body: cpBody; userData: cpDataPointer);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpBodySetUserData(body, userData);
  cpFloatLeave(mask);
end;

procedure cpBodySetVelocityUpdateFunc(body: cpBody; velocityFunc: cpBodyVelocityFunc);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  try
    _cpBodySetVelocityUpdateFunc(body, velocityFunc);
  finally
    cpFloatLeave(mask);
  end;
end;

procedure cpBodySetPositionUpdateFunc(body: cpBody; positionFunc: cpBodyPositionFunc);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  try
    _cpBodySetPositionUpdateFunc(body, positionFunc);
  finally
    cpFloatLeave(mask);
  end;
end;

procedure cpBodyUpdateVelocity(body: cpBody; gravity: cpVect; damping, dt: cpFloat); cdecl;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpBodyUpdateVelocity(body, gravity, damping, dt);
  cpFloatLeave(mask);
end;

procedure cpBodyUpdatePosition(body: cpBody; dt: cpFloat); cdecl;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpBodyUpdatePosition(body, dt);
  cpFloatLeave(mask);
end;

function cpBodyLocalToWorld(body: cpBody; const point: cpVect): cpVect;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpBodyLocalToWorld(body, point);
  cpFloatLeave(mask);
end;

function cpBodyWorldToLocal(body: cpBody; const point: cpVect): cpVect;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpBodyWorldToLocal(body, point);
  cpFloatLeave(mask);
end;

procedure cpBodyApplyForceAtWorldPoint(body: cpBody; force, point: cpVect);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpBodyApplyForceAtWorldPoint(body, force, point);
  cpFloatLeave(mask);
end;

procedure cpBodyApplyForceAtLocalPoint(body: cpBody; force, point: cpVect);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpBodyApplyForceAtLocalPoint(body, force, point);
  cpFloatLeave(mask);
end;

procedure cpBodyApplyImpulseAtWorldPoint(body: cpBody; impulse, point: cpVect);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpBodyApplyImpulseAtWorldPoint(body, impulse, point);
  cpFloatLeave(mask);
end;

procedure cpBodyApplyImpulseAtLocalPoint(body: cpBody; impulse, point: cpVect);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpBodyApplyImpulseAtLocalPoint(body, impulse, point);
  cpFloatLeave(mask);
end;

function cpBodyGetVelocityAtWorldPoint(body: cpBody; point: cpVect): cpVect;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpBodyGetVelocityAtWorldPoint(body, point);
  cpFloatLeave(mask);
end;

function cpBodyGetVelocityAtLocalPoint(body: cpBody; point: cpVect): cpVect;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpBodyGetVelocityAtLocalPoint(body, point);
  cpFloatLeave(mask);
end;

function cpBodyKineticEnergy(body: cpBody): cpFloat;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpBodyKineticEnergy(body);
  cpFloatLeave(mask);
end;

procedure cpBodyEachShape(body: cpBody; func: cpBodyShapeIteratorFunc; data: Pointer);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  try
    _cpBodyEachShape(body, func, data);
  finally
    cpFloatLeave(mask);
  end;
end;

procedure cpBodyEachConstraint(body: cpBody; func: cpBodyConstraintIteratorFunc; data: Pointer);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  try
    _cpBodyEachConstraint(body, func, data);
  finally
    cpFloatLeave(mask);
  end;
end;

procedure cpBodyEachArbiter(body: cpBody; func: cpBodyArbiterIteratorFunc; data: Pointer);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  try
    _cpBodyEachArbiter(body, func, data);
  finally
    cpFloatLeave(mask);
  end;
end;

procedure cpShapeDestroy(shape: cpShape);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpShapeDestroy(shape);
  cpFloatLeave(mask);
end;

procedure cpShapeFree(shape: cpShape);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpShapeFree(shape);
  cpFloatLeave(mask);
end;

function cpShapeCacheBB(shape: cpShape): cpBB;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpShapeCacheBB(shape);
  cpFloatLeave(mask);
end;

function cpShapeUpdate(shape: cpShape; transform: cpTransform): cpBB;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpShapeUpdate(shape, transform);
  cpFloatLeave(mask);
end;

function cpShapePointQuery(shape: cpShape; p: cpVect; info: cpPointQueryInfo): cpFloat;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpShapePointQuery(shape, p, info);
  cpFloatLeave(mask);
end;

function cpShapeSegmentQuery(shape: cpShape; a, b: cpVect; radius: cpFloat; info: cpSegmentQueryInfo): cpBool;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpShapeSegmentQuery(shape, a, b, radius, info);
  cpFloatLeave(mask);
end;

function cpShapesCollide(a, b: cpShape): cpContactPointSetStruct;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpShapesCollide(a, b);
  cpFloatLeave(mask);
end;

function cpShapeGetSpace(shape: cpShape): cpSpace;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpShapeGetSpace(shape);
  cpFloatLeave(mask);
end;

function cpShapeGetBody(shape: cpShape): cpBody;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpShapeGetBody(shape);
  cpFloatLeave(mask);
end;

procedure cpShapeSetBody(shape: cpShape; body: cpBody);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpShapeSetBody(shape, body);
  cpFloatLeave(mask);
end;

function cpShapeGetMass(shape: cpShape): cpFloat;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpShapeGetMass(shape);
  cpFloatLeave(mask);
end;

procedure cpShapeSetMass(shape: cpShape; mass: cpFloat);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpShapeSetMass(shape, mass);
  cpFloatLeave(mask);
end;

function cpShapeGetDensity(shape: cpShape): cpFloat;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpShapeGetDensity(shape);
  cpFloatLeave(mask);
end;

procedure cpShapeSetDensity(shape: cpShape; density: cpFloat);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpShapeSetDensity(shape, density);
  cpFloatLeave(mask);
end;

function cpShapeGetMoment(shape: cpShape): cpFloat;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpShapeGetMoment(shape);
  cpFloatLeave(mask);
end;

function cpShapeGetArea(shape: cpShape): cpFloat;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpShapeGetArea(shape);
  cpFloatLeave(mask);
end;

function cpShapeGetCenterOfGravity(shape: cpShape): cpVect;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpShapeGetCenterOfGravity(shape);
  cpFloatLeave(mask);
end;

function cpShapeGetBB(shape: cpShape): cpBB;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpShapeGetBB(shape);
  cpFloatLeave(mask);
end;

function cpShapeGetSensor(shape: cpShape): cpBool;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpShapeGetSensor(shape);
  cpFloatLeave(mask);
end;

procedure cpShapeSetSensor(shape: cpShape; sensor: cpBool);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpShapeSetSensor(shape, sensor);
  cpFloatLeave(mask);
end;

function cpShapeGetElasticity(shape: cpShape): cpFloat;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpShapeGetElasticity(shape);
  cpFloatLeave(mask);
end;

procedure cpShapeSetElasticity(shape: cpShape; elasticity: cpFloat);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpShapeSetElasticity(shape, elasticity);
  cpFloatLeave(mask);
end;

function cpShapeGetFriction(shape: cpShape): cpFloat;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpShapeGetFriction(shape);
  cpFloatLeave(mask);
end;

procedure cpShapeSetFriction(shape: cpShape; friction: cpFloat);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpShapeSetFriction(shape, friction);
  cpFloatLeave(mask);
end;

function cpShapeGetSurfaceVelocity(shape: cpShape): cpVect;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpShapeGetSurfaceVelocity(shape);
  cpFloatLeave(mask);
end;

procedure cpShapeSetSurfaceVelocity(shape: cpShape; surfaceVelocity: cpVect);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpShapeSetSurfaceVelocity(shape, surfaceVelocity);
  cpFloatLeave(mask);
end;

function cpShapeGetUserData(shape: cpShape): cpDataPointer;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpShapeGetUserData(shape);
  cpFloatLeave(mask);
end;

procedure cpShapeSetUserData(shape: cpShape; userData: cpDataPointer);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpShapeSetUserData(shape, userData);
  cpFloatLeave(mask);
end;

function cpShapeGetCollisionType(shape: cpShape): cpCollisionType;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpShapeGetCollisionType(shape);
  cpFloatLeave(mask);
end;

procedure cpShapeSetCollisionType(shape: cpShape; collisionType: cpCollisionType);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpShapeSetCollisionType(shape, collisionType);
  cpFloatLeave(mask);
end;

function cpShapeGetFilter(shape: cpShape): cpShapeFilter;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpShapeGetFilter(shape);
  cpFloatLeave(mask);
end;

procedure cpShapeSetFilter(shape: cpShape; filter: cpShapeFilter);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpShapeSetFilter(shape, filter);
  cpFloatLeave(mask);
end;

function cpCircleShapeAlloc: cpCircleShape;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpCircleShapeAlloc;
  cpFloatLeave(mask);
end;

function cpCircleShapeInit(circle: cpCircleShape; body: cpBody; radius: cpFloat; offset: cpVect): cpCircleShape;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpCircleShapeInit(circle, body, radius, offset);
  cpFloatLeave(mask);
end;

function cpCircleShapeNew(body: cpBody; radius: cpFloat; offset: cpVect): cpShape;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpCircleShapeNew(body, radius, offset);
  cpFloatLeave(mask);
end;

function cpCircleShapeGetOffset(shape: cpShape): cpVect;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpCircleShapeGetOffset(shape);
  cpFloatLeave(mask);
end;

function cpCircleShapeGetRadius(shape: cpShape): cpFloat;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpCircleShapeGetRadius(shape);
  cpFloatLeave(mask);
end;

procedure cpCircleShapeSetRadius(shape: cpShape; radius: cpFloat);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpCircleShapeSetRadius(shape, radius);
  cpFloatLeave(mask);
end;

procedure cpCircleShapeSetOffset(shape: cpShape; offset: cpVect);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpCircleShapeSetOffset(shape, offset);
  cpFloatLeave(mask);
end;

function cpSegmentShapeAlloc: cpSegmentShape;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpSegmentShapeAlloc;
  cpFloatLeave(mask);
end;

function cpSegmentShapeInit(seg: cpSegmentShape; body: cpBody; a, b: cpVect; radius: cpFloat): cpSegmentShape;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpSegmentShapeInit(seg, body, a, b, radius);
  cpFloatLeave(mask);
end;

function cpSegmentShapeNew(body: cpBody; a, b: cpVect; radius: cpFloat): cpShape;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpSegmentShapeNew(body, a, b, radius);
  cpFloatLeave(mask);
end;

procedure cpSegmentShapeSetNeighbors(shape: cpShape; prev, next: cpVect);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpSegmentShapeSetNeighbors(shape, prev, next);
  cpFloatLeave(mask);
end;

function cpSegmentShapeGetA(shape: cpShape): cpVect;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpSegmentShapeGetA(shape);
  cpFloatLeave(mask);
end;

function cpSegmentShapeGetB(shape: cpShape): cpVect;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpSegmentShapeGetB(shape);
  cpFloatLeave(mask);
end;

function cpSegmentShapeGetNormal(shape: cpShape): cpVect;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpSegmentShapeGetNormal(shape);
  cpFloatLeave(mask);
end;

function cpSegmentShapeGetRadius(shape: cpShape): cpFloat;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpSegmentShapeGetRadius(shape);
  cpFloatLeave(mask);
end;

procedure cpSegmentShapeSetEndpoints(shape: cpShape; a, b: cpVect);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpSegmentShapeSetEndpoints(shape, a, b);
  cpFloatLeave(mask);
end;

procedure cpSegmentShapeSetRadius(shape: cpShape; radius: cpFloat);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpSegmentShapeSetRadius(shape, radius);
  cpFloatLeave(mask);
end;

function cpPolyShapeAlloc: cpPolyShape;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpPolyShapeAlloc;
  cpFloatLeave(mask);
end;

function cpPolyShapeInit(poly: cpPolyShape; body: cpBody; count: Integer; verts: PcpVect;
  transform: cpTransform; radius: cpFloat): cpPolyShape;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpPolyShapeInit(poly, body, count, verts, transform, radius);
  cpFloatLeave(mask);
end;

function cpPolyShapeInitRaw(poly: cpPolyShape; body: cpBody; count: Integer; verts: PcpVect;
  radius: cpFloat): cpPolyShape;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpPolyShapeInitRaw(poly, body, count, verts, radius);
  cpFloatLeave(mask);
end;

function cpPolyShapeNew(body: cpBody; count: Integer; verts: PcpVect; transform: cpTransform;
  radius: cpFloat): cpShape;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpPolyShapeNew(body, count, verts, transform, radius);
  cpFloatLeave(mask);
end;

function cpPolyShapeNewRaw(body: cpBody; count: Integer; verts: PcpVect; radius: cpFloat): cpShape;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpPolyShapeNewRaw(body, count, verts, radius);
  cpFloatLeave(mask);
end;

function cpBoxShapeInit(poly: cpPolyShape; body: cpBody; width, height, radius: cpFloat): cpPolyShape;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpBoxShapeInit(poly, body, width, height, radius);
  cpFloatLeave(mask);
end;

function cpBoxShapeInit2(poly: cpPolyShape; body: cpBody; box: cpBB; radius: cpFloat): cpPolyShape;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpBoxShapeInit2(poly, body, box, radius);
  cpFloatLeave(mask);
end;

function cpBoxShapeNew(body: cpBody; width, height, radius: cpFloat): cpShape;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpBoxShapeNew(body, width, height, radius);
  cpFloatLeave(mask);
end;

function cpBoxShapeNew2(body: cpBody; box: cpBB; radius: cpFloat): cpShape;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpBoxShapeNew2(body, box, radius);
  cpFloatLeave(mask);
end;

function cpPolyShapeGetCount(shape: cpShape): Integer;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpPolyShapeGetCount(shape);
  cpFloatLeave(mask);
end;

function cpPolyShapeGetVert(shape: cpShape; index: Integer): cpVect;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpPolyShapeGetVert(shape, index);
  cpFloatLeave(mask);
end;

function cpPolyShapeGetRadius(shape: cpShape): cpFloat;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpPolyShapeGetRadius(shape);
  cpFloatLeave(mask);
end;

procedure cpPolyShapeSetVerts(shape: cpShape; count: Integer; verts: PcpVect; transform: cpTransform);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpPolyShapeSetVerts(shape, count, verts, transform);
  cpFloatLeave(mask);
end;

procedure cpPolyShapeSetVertsRaw(shape: cpShape; count: Integer; verts: PcpVect);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpPolyShapeSetVertsRaw(shape, count, verts);
  cpFloatLeave(mask);
end;

procedure cpPolyShapeSetRadius(shape: cpShape; radius: cpFloat);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpPolyShapeSetRadius(shape, radius);
  cpFloatLeave(mask);
end;

procedure cpConstraintDestroy(constraint: cpConstraint);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpConstraintDestroy(constraint);
  cpFloatLeave(mask);
end;

procedure cpConstraintFree(constraint: cpConstraint);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpConstraintFree(constraint);
  cpFloatLeave(mask);
end;

function cpConstraintGetSpace(constraint: cpConstraint): cpSpace;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpConstraintGetSpace(constraint);
  cpFloatLeave(mask);
end;

function cpConstraintGetBodyA(constraint: cpConstraint): cpBody;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpConstraintGetBodyA(constraint);
  cpFloatLeave(mask);
end;

function cpConstraintGetBodyB(constraint: cpConstraint): cpBody;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpConstraintGetBodyB(constraint);
  cpFloatLeave(mask);
end;

function cpConstraintGetMaxForce(constraint: cpConstraint): cpFloat;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpConstraintGetMaxForce(constraint);
  cpFloatLeave(mask);
end;

procedure cpConstraintSetMaxForce(constraint: cpConstraint; maxForce: cpFloat);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpConstraintSetMaxForce(constraint, maxForce);
  cpFloatLeave(mask);
end;

function cpConstraintGetErrorBias(constraint: cpConstraint): cpFloat;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpConstraintGetErrorBias(constraint);
  cpFloatLeave(mask);
end;

procedure cpConstraintSetErrorBias(constraint: cpConstraint; errorBias: cpFloat);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpConstraintSetErrorBias(constraint, errorBias);
  cpFloatLeave(mask);
end;

function cpConstraintGetMaxBias(constraint: cpConstraint): cpFloat;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpConstraintGetMaxBias(constraint);
  cpFloatLeave(mask);
end;

procedure cpConstraintSetMaxBias(constraint: cpConstraint; maxBias: cpFloat);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpConstraintSetMaxBias(constraint, maxBias);
  cpFloatLeave(mask);
end;

function cpConstraintGetCollideBodies(constraint: cpConstraint): cpBool;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpConstraintGetCollideBodies(constraint);
  cpFloatLeave(mask);
end;

procedure cpConstraintSetCollideBodies(constraint: cpConstraint; collideBodies: cpBool);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpConstraintSetCollideBodies(constraint, collideBodies);
  cpFloatLeave(mask);
end;

function cpConstraintGetPreSolveFunc(constraint: cpConstraint): cpConstraintPreSolveFunc;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpConstraintGetPreSolveFunc(constraint);
  cpFloatLeave(mask);
end;

procedure cpConstraintSetPreSolveFunc(constraint: cpConstraint; preSolveFunc: cpConstraintPreSolveFunc);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  try
    _cpConstraintSetPreSolveFunc(constraint, preSolveFunc);
  finally
    cpFloatLeave(mask);
  end;
end;

function cpConstraintGetPostSolveFunc(constraint: cpConstraint): cpConstraintPostSolveFunc;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpConstraintGetPostSolveFunc(constraint);
  cpFloatLeave(mask);
end;

procedure cpConstraintSetPostSolveFunc(constraint: cpConstraint; postSolveFunc: cpConstraintPostSolveFunc);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  try
    _cpConstraintSetPostSolveFunc(constraint, postSolveFunc);
  finally
    cpFloatLeave(mask);
  end;
end;

function cpConstraintGetUserData(constraint: cpConstraint): cpDataPointer;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpConstraintGetUserData(constraint);
  cpFloatLeave(mask);
end;

procedure cpConstraintSetUserData(constraint: cpConstraint; userData: cpDataPointer);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpConstraintSetUserData(constraint, userData);
  cpFloatLeave(mask);
end;

function cpConstraintGetImpulse(constraint: cpConstraint): cpFloat;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpConstraintGetImpulse(constraint);
  cpFloatLeave(mask);
end;

function cpConstraintIsPinJoint(constraint: cpConstraint): cpBool;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpConstraintIsPinJoint(constraint);
  cpFloatLeave(mask);
end;

function cpPinJointAlloc: cpPinJoint;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpPinJointAlloc;
  cpFloatLeave(mask);
end;

function cpPinJointInit(joint: cpPinJoint; a, b: cpBody; anchorA, anchorB: cpVect): cpPinJoint;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpPinJointInit(joint, a, b, anchorA, anchorB);
  cpFloatLeave(mask);
end;

function cpPinJointNew(a, b: cpBody; anchorA, anchorB: cpVect): cpConstraint;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpPinJointNew(a, b, anchorA, anchorB);
  cpFloatLeave(mask);
end;

function cpPinJointGetAnchorA(constraint: cpConstraint): cpVect;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpPinJointGetAnchorA(constraint);
  cpFloatLeave(mask);
end;

procedure cpPinJointSetAnchorA(constraint: cpConstraint; anchorA: cpVect);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpPinJointSetAnchorA(constraint, anchorA);
  cpFloatLeave(mask);
end;

function cpPinJointGetAnchorB(constraint: cpConstraint): cpVect;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpPinJointGetAnchorB(constraint);
  cpFloatLeave(mask);
end;

procedure cpPinJointSetAnchorB(constraint: cpConstraint; anchorB: cpVect);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpPinJointSetAnchorB(constraint, anchorB);
  cpFloatLeave(mask);
end;

function cpPinJointGetDist(constraint: cpConstraint): cpFloat;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpPinJointGetDist(constraint);
  cpFloatLeave(mask);
end;

procedure cpPinJointSetDist(constraint: cpConstraint; dist: cpFloat);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpPinJointSetDist(constraint, dist);
  cpFloatLeave(mask);
end;

function cpConstraintIsSlideJoint(constraint: cpConstraint): cpBool;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpConstraintIsSlideJoint(constraint);
  cpFloatLeave(mask);
end;

function cpSlideJointAlloc: cpSlideJoint;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpSlideJointAlloc;
  cpFloatLeave(mask);
end;

function cpSlideJointInit(joint: cpSlideJoint; a, b: cpBody; anchorA, anchorB: cpVect; min, max: cpFloat): cpSlideJoint;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpSlideJointInit(joint, a, b, anchorA, anchorB, min, max);
  cpFloatLeave(mask);
end;

function cpSlideJointNew(a, b: cpBody; anchorA, anchorB: cpVect; min, max: cpFloat): cpConstraint;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpSlideJointNew(a, b, anchorA, anchorB, min, max);
  cpFloatLeave(mask);
end;

function cpSlideJointGetAnchorA(constraint: cpConstraint): cpVect;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpSlideJointGetAnchorA(constraint);
  cpFloatLeave(mask);
end;

procedure cpSlideJointSetAnchorA(constraint: cpConstraint; anchorA: cpVect);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpSlideJointSetAnchorA(constraint, anchorA);
  cpFloatLeave(mask);
end;

function cpSlideJointGetAnchorB(constraint: cpConstraint): cpVect;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpSlideJointGetAnchorB(constraint);
  cpFloatLeave(mask);
end;

procedure cpSlideJointSetAnchorB(constraint: cpConstraint; anchorB: cpVect);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpSlideJointSetAnchorB(constraint, anchorB);
  cpFloatLeave(mask);
end;

function cpSlideJointGetMin(constraint: cpConstraint): cpFloat;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpSlideJointGetMin(constraint);
  cpFloatLeave(mask);
end;

procedure cpSlideJointSetMin(constraint: cpConstraint; min: cpFloat);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpSlideJointSetMin(constraint, min);
  cpFloatLeave(mask);
end;

function cpSlideJointGetMax(constraint: cpConstraint): cpFloat;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpSlideJointGetMax(constraint);
  cpFloatLeave(mask);
end;

procedure cpSlideJointSetMax(constraint: cpConstraint; max: cpFloat);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpSlideJointSetMax(constraint, max);
  cpFloatLeave(mask);
end;

function cpConstraintIsPivotJoint(constraint: cpConstraint): cpBool;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpConstraintIsPivotJoint(constraint);
  cpFloatLeave(mask);
end;

function cpPivotJointAlloc: cpPivotJoint;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpPivotJointAlloc;
  cpFloatLeave(mask);
end;

function cpPivotJointInit(joint: cpPivotJoint; a, b: cpBody; anchorA, anchorB: cpVect): cpPivotJoint;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpPivotJointInit(joint, a, b, anchorA, anchorB);
  cpFloatLeave(mask);
end;

function cpPivotJointNew(a, b: cpBody; pivot: cpVect): cpConstraint;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpPivotJointNew(a, b, pivot);
  cpFloatLeave(mask);
end;

function cpPivotJointNew2(a, b: cpBody; anchorA, anchorB: cpVect): cpConstraint;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpPivotJointNew2(a, b, anchorA, anchorB);
  cpFloatLeave(mask);
end;

function cpPivotJointGetAnchorA(constraint: cpConstraint): cpVect;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpPivotJointGetAnchorA(constraint);
  cpFloatLeave(mask);
end;

procedure cpPivotJointSetAnchorA(constraint: cpConstraint; anchorA: cpVect);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpPivotJointSetAnchorA(constraint, anchorA);
  cpFloatLeave(mask);
end;

function cpPivotJointGetAnchorB(constraint: cpConstraint): cpVect;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpPivotJointGetAnchorB(constraint);
  cpFloatLeave(mask);
end;

procedure cpPivotJointSetAnchorB(constraint: cpConstraint; anchorB: cpVect);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpPivotJointSetAnchorB(constraint, anchorB);
  cpFloatLeave(mask);
end;

function cpConstraintIsGrooveJoint(constraint: cpConstraint): cpBool;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpConstraintIsGrooveJoint(constraint);
  cpFloatLeave(mask);
end;

function cpGrooveJointAlloc: cpGrooveJoint;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpGrooveJointAlloc;
  cpFloatLeave(mask);
end;

function cpGrooveJointInit(joint: cpGrooveJoint; a, b: cpBody; groove_a, groove_b, anchorB: cpVect): cpGrooveJoint;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpGrooveJointInit(joint, a, b, groove_a, groove_b, anchorB);
  cpFloatLeave(mask);
end;

function cpGrooveJointNew(a, b: cpBody; groove_a, groove_b, anchorB: cpVect): cpConstraint;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpGrooveJointNew(a, b, groove_a, groove_b, anchorB);
  cpFloatLeave(mask);
end;

function cpGrooveJointGetGrooveA(constraint: cpConstraint): cpVect;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpGrooveJointGetGrooveA(constraint);
  cpFloatLeave(mask);
end;

procedure cpGrooveJointSetGrooveA(constraint: cpConstraint; grooveA: cpVect);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpGrooveJointSetGrooveA(constraint, grooveA);
  cpFloatLeave(mask);
end;

function cpGrooveJointGetGrooveB(constraint: cpConstraint): cpVect;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpGrooveJointGetGrooveB(constraint);
  cpFloatLeave(mask);
end;

procedure cpGrooveJointSetGrooveB(constraint: cpConstraint; grooveB: cpVect);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpGrooveJointSetGrooveB(constraint, grooveB);
  cpFloatLeave(mask);
end;

function cpGrooveJointGetAnchorB(constraint: cpConstraint): cpVect;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpGrooveJointGetAnchorB(constraint);
  cpFloatLeave(mask);
end;

procedure cpGrooveJointSetAnchorB(constraint: cpConstraint; anchorB: cpVect);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpGrooveJointSetAnchorB(constraint, anchorB);
  cpFloatLeave(mask);
end;

function cpConstraintIsDampedSpring(constraint: cpConstraint): cpBool;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpConstraintIsDampedSpring(constraint);
  cpFloatLeave(mask);
end;

function cpDampedSpringAlloc: cpDampedSpring;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpDampedSpringAlloc;
  cpFloatLeave(mask);
end;

function cpDampedSpringInit(joint: cpDampedSpring; a, b: cpBody; anchorA, anchorB: cpVect;
  restLength, stiffness, damping: cpFloat): cpDampedSpring;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpDampedSpringInit(joint, a, b, anchorA, anchorB, restLength, stiffness, damping);
  cpFloatLeave(mask);
end;

function cpDampedSpringNew(a, b: cpBody; anchorA, anchorB: cpVect;
  restLength, stiffness, damping: cpFloat): cpConstraint;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpDampedSpringNew(a, b, anchorA, anchorB, restLength, stiffness, damping);
  cpFloatLeave(mask);
end;

function cpDampedSpringGetAnchorA(constraint: cpConstraint): cpVect;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpDampedSpringGetAnchorA(constraint);
  cpFloatLeave(mask);
end;

procedure cpDampedSpringSetAnchorA(constraint: cpConstraint; anchorA: cpVect);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpDampedSpringSetAnchorA(constraint, anchorA);
  cpFloatLeave(mask);
end;

function cpDampedSpringGetAnchorB(constraint: cpConstraint): cpVect;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpDampedSpringGetAnchorB(constraint);
  cpFloatLeave(mask);
end;

procedure cpDampedSpringSetAnchorB(constraint: cpConstraint; anchorB: cpVect);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpDampedSpringSetAnchorB(constraint, anchorB);
  cpFloatLeave(mask);
end;

function cpDampedSpringGetRestLength(constraint: cpConstraint): cpFloat;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpDampedSpringGetRestLength(constraint);
  cpFloatLeave(mask);
end;

procedure cpDampedSpringSetRestLength(constraint: cpConstraint; restLength: cpFloat);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpDampedSpringSetRestLength(constraint, restLength);
  cpFloatLeave(mask);
end;

function cpDampedSpringGetStiffness(constraint: cpConstraint): cpFloat;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpDampedSpringGetStiffness(constraint);
  cpFloatLeave(mask);
end;

procedure cpDampedSpringSetStiffness(constraint: cpConstraint; stiffness: cpFloat);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpDampedSpringSetStiffness(constraint, stiffness);
  cpFloatLeave(mask);
end;

function cpDampedSpringGetDamping(constraint: cpConstraint): cpFloat;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpDampedSpringGetDamping(constraint);
  cpFloatLeave(mask);
end;

procedure cpDampedSpringSetDamping(constraint: cpConstraint; damping: cpFloat);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpDampedSpringSetDamping(constraint, damping);
  cpFloatLeave(mask);
end;

function cpDampedSpringGetSpringForceFunc(constraint: cpConstraint): cpDampedSpringForceFunc;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpDampedSpringGetSpringForceFunc(constraint);
  cpFloatLeave(mask);
end;

procedure cpDampedSpringSetSpringForceFunc(constraint: cpConstraint; springForceFunc: cpDampedSpringForceFunc);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  try
    _cpDampedSpringSetSpringForceFunc(constraint, springForceFunc);
  finally
    cpFloatLeave(mask);
  end;
end;

function cpConstraintIsDampedRotarySpring(constraint: cpConstraint): cpBool;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpConstraintIsDampedRotarySpring(constraint);
  cpFloatLeave(mask);
end;

function cpDampedRotarySpringAlloc: cpDampedRotarySpring;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpDampedRotarySpringAlloc;
  cpFloatLeave(mask);
end;

function cpDampedRotarySpringInit(joint: cpDampedRotarySpring; a, b: cpBody;
  restAngle, stiffness, damping: cpFloat): cpDampedRotarySpring;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpDampedRotarySpringInit(joint, a, b, restAngle, stiffness, damping);
  cpFloatLeave(mask);
end;

function cpDampedRotarySpringNew(a, b: cpBody; restAngle, stiffness, damping: cpFloat): cpConstraint;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpDampedRotarySpringNew(a, b, restAngle, stiffness, damping);
  cpFloatLeave(mask);
end;

function cpDampedRotarySpringGetRestAngle(constraint: cpConstraint): cpFloat;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpDampedRotarySpringGetRestAngle(constraint);
  cpFloatLeave(mask);
end;

procedure cpDampedRotarySpringSetRestAngle(constraint: cpConstraint; restAngle: cpFloat);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpDampedRotarySpringSetRestAngle(constraint, restAngle);
  cpFloatLeave(mask);
end;

function cpDampedRotarySpringGetStiffness(constraint: cpConstraint): cpFloat;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpDampedRotarySpringGetStiffness(constraint);
  cpFloatLeave(mask);
end;

procedure cpDampedRotarySpringSetStiffness(constraint: cpConstraint; stiffness: cpFloat);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpDampedRotarySpringSetStiffness(constraint, stiffness);
  cpFloatLeave(mask);
end;

function cpDampedRotarySpringGetDamping(constraint: cpConstraint): cpFloat;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpDampedRotarySpringGetDamping(constraint);
  cpFloatLeave(mask);
end;

procedure cpDampedRotarySpringSetDamping(constraint: cpConstraint; damping: cpFloat);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpDampedRotarySpringSetDamping(constraint, damping);
  cpFloatLeave(mask);
end;

function cpDampedRotarySpringGetSpringTorqueFunc(constraint: cpConstraint): cpDampedRotarySpringTorqueFunc;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpDampedRotarySpringGetSpringTorqueFunc(constraint);
  cpFloatLeave(mask);
end;

procedure cpDampedRotarySpringSetSpringTorqueFunc(constraint: cpConstraint;
  springTorqueFunc: cpDampedRotarySpringTorqueFunc);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  try
    _cpDampedRotarySpringSetSpringTorqueFunc(constraint, springTorqueFunc);
  finally
    cpFloatLeave(mask);
  end;
end;

function cpConstraintIsRotaryLimitJoint(constraint: cpConstraint): cpBool;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpConstraintIsRotaryLimitJoint(constraint);
  cpFloatLeave(mask);
end;

function cpRotaryLimitJointAlloc: cpRotaryLimitJoint;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpRotaryLimitJointAlloc;
  cpFloatLeave(mask);
end;

function cpRotaryLimitJointInit(joint: cpRotaryLimitJoint; a, b: cpBody; min, max: cpFloat): cpRotaryLimitJoint;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpRotaryLimitJointInit(joint, a, b, min, max);
  cpFloatLeave(mask);
end;

function cpRotaryLimitJointNew(a, b: cpBody; min, max: cpFloat): cpConstraint;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpRotaryLimitJointNew(a, b, min, max);
  cpFloatLeave(mask);
end;

function cpRotaryLimitJointGetMin(constraint: cpConstraint): cpFloat;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpRotaryLimitJointGetMin(constraint);
  cpFloatLeave(mask);
end;

procedure cpRotaryLimitJointSetMin(constraint: cpConstraint; min: cpFloat);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpRotaryLimitJointSetMin(constraint, min);
  cpFloatLeave(mask);
end;

function cpRotaryLimitJointGetMax(constraint: cpConstraint): cpFloat;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpRotaryLimitJointGetMax(constraint);
  cpFloatLeave(mask);
end;

procedure cpRotaryLimitJointSetMax(constraint: cpConstraint; max: cpFloat);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpRotaryLimitJointSetMax(constraint, max);
  cpFloatLeave(mask);
end;

function cpConstraintIsRatchetJoint(constraint: cpConstraint): cpBool;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpConstraintIsRatchetJoint(constraint);
  cpFloatLeave(mask);
end;

function cpRatchetJointAlloc: cpRatchetJoint;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpRatchetJointAlloc;
  cpFloatLeave(mask);
end;

function cpRatchetJointInit(joint: cpRatchetJoint; a, b: cpBody; phase, ratchet: cpFloat): cpRatchetJoint;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpRatchetJointInit(joint, a, b, phase, ratchet);
  cpFloatLeave(mask);
end;

function cpRatchetJointNew(a, b: cpBody; phase, ratchet: cpFloat): cpConstraint;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpRatchetJointNew(a, b, phase, ratchet);
  cpFloatLeave(mask);
end;

function cpRatchetJointGetAngle(constraint: cpConstraint): cpFloat;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpRatchetJointGetAngle(constraint);
  cpFloatLeave(mask);
end;

procedure cpRatchetJointSetAngle(constraint: cpConstraint; angle: cpFloat);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpRatchetJointSetAngle(constraint, angle);
  cpFloatLeave(mask);
end;

function cpRatchetJointGetPhase(constraint: cpConstraint): cpFloat;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpRatchetJointGetPhase(constraint);
  cpFloatLeave(mask);
end;

procedure cpRatchetJointSetPhase(constraint: cpConstraint; phase: cpFloat);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpRatchetJointSetPhase(constraint, phase);
  cpFloatLeave(mask);
end;

function cpRatchetJointGetRatchet(constraint: cpConstraint): cpFloat;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpRatchetJointGetRatchet(constraint);
  cpFloatLeave(mask);
end;

procedure cpRatchetJointSetRatchet(constraint: cpConstraint; ratchet: cpFloat);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpRatchetJointSetRatchet(constraint, ratchet);
  cpFloatLeave(mask);
end;

function cpConstraintIsGearJoint(constraint: cpConstraint): cpBool;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpConstraintIsGearJoint(constraint);
  cpFloatLeave(mask);
end;

function cpGearJointAlloc: cpGearJoint;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpGearJointAlloc;
  cpFloatLeave(mask);
end;

function cpGearJointInit(joint: cpGearJoint; a, b: cpBody; phase, ratio: cpFloat): cpGearJoint;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpGearJointInit(joint, a, b, phase, ratio);
  cpFloatLeave(mask);
end;

function cpGearJointNew(a, b: cpBody; phase, ratio: cpFloat): cpConstraint;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpGearJointNew(a, b, phase, ratio);
  cpFloatLeave(mask);
end;

function cpGearJointGetPhase(constraint: cpConstraint): cpFloat;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpGearJointGetPhase(constraint);
  cpFloatLeave(mask);
end;

procedure cpGearJointSetPhase(constraint: cpConstraint; phase: cpFloat);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpGearJointSetPhase(constraint, phase);
  cpFloatLeave(mask);
end;

function cpGearJointGetRatio(constraint: cpConstraint): cpFloat;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpGearJointGetRatio(constraint);
  cpFloatLeave(mask);
end;

procedure cpGearJointSetRatio(constraint: cpConstraint; ratio: cpFloat);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpGearJointSetRatio(constraint, ratio);
  cpFloatLeave(mask);
end;

function cpConstraintIsSimpleMotor(constraint: cpConstraint): cpBool;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpConstraintIsSimpleMotor(constraint);
  cpFloatLeave(mask);
end;

function cpSimpleMotorAlloc: cpSimpleMotor;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpSimpleMotorAlloc;
  cpFloatLeave(mask);
end;

function cpSimpleMotorInit(joint: cpSimpleMotor; a, b: cpBody; rate: cpFloat): cpSimpleMotor;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpSimpleMotorInit(joint, a, b, rate);
  cpFloatLeave(mask);
end;

function cpSimpleMotorNew(a, b: cpBody; rate: cpFloat): cpConstraint;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpSimpleMotorNew(a, b, rate);
  cpFloatLeave(mask);
end;

function cpSimpleMotorGetRate(constraint: cpConstraint): cpFloat;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpSimpleMotorGetRate(constraint);
  cpFloatLeave(mask);
end;

procedure cpSimpleMotorSetRate(constraint: cpConstraint; rate: cpFloat);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpSimpleMotorSetRate(constraint, rate);
  cpFloatLeave(mask);
end;

function cpSpaceHashNew(celldim: cpFloat; cells: Integer; bbfunc: cpSpatialIndexBBFunc;
  staticIndex: cpSpatialIndex): cpSpatialIndex;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  try
    Result := _cpSpaceHashNew(celldim, cells, bbfunc, staticIndex);
  finally
    cpFloatLeave(mask);
  end;
end;

procedure cpSpaceHashResize(hash: cpSpatialIndex; celldim: cpFloat; numcells: Integer);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpSpaceHashResize(hash, celldim, numcells);
  cpFloatLeave(mask);
end;

function cpBBTreeNew(bbfunc: cpSpatialIndexBBFunc; staticIndex: cpSpatialIndex): cpSpatialIndex;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  try
    Result := _cpBBTreeNew(bbfunc, staticIndex);
  finally
    cpFloatLeave(mask);
  end;
end;

procedure cpBBTreeOptimize(index: cpSpatialIndex);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpBBTreeOptimize(index);
  cpFloatLeave(mask);
end;

procedure cpBBTreeSetVelocityFunc(index: cpSpatialIndex; func: cpBBTreeVelocityFunc);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  try
    _cpBBTreeSetVelocityFunc(index, func);
  finally
    cpFloatLeave(mask);
  end;
end;

function cpSweep1DNew(bbfunc: cpSpatialIndexBBFunc; staticIndex: cpSpatialIndex): cpSpatialIndex;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  try
    Result := _cpSweep1DNew(bbfunc, staticIndex);
  finally
    cpFloatLeave(mask);
  end;
end;

procedure cpSpatialIndexFree(index: cpSpatialIndex);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  try
    _cpSpatialIndexFree(index);
  finally
    cpFloatLeave(mask);
  end;
end;

procedure cpSpatialIndexCollideStatic(dynamicIndex, staticIndex: cpSpatialIndex;
  func: cpSpatialIndexQueryFunc; data: Pointer);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  try
    _cpSpatialIndexCollideStatic(dynamicIndex, staticIndex, func, data);
  finally
    cpFloatLeave(mask);
  end;
end;

function cpSpaceAlloc: cpSpace;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpSpaceAlloc;
  cpFloatLeave(mask);
end;

function cpSpaceInit(space: cpSpace): cpSpace;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpSpaceInit(space);
  cpFloatLeave(mask);
end;

function cpSpaceNew: cpSpace;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpSpaceNew;
  cpFloatLeave(mask);
end;

procedure cpSpaceDestroy(space: cpSpace);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  try
    _cpSpaceDestroy(space);
  finally
    cpFloatLeave(mask);
  end;
end;

procedure cpSpaceFree(space: cpSpace);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  try
    _cpSpaceFree(space);
  finally
    cpFloatLeave(mask);
  end;
end;

function cpSpaceGetIterations(space: cpSpace): Integer;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpSpaceGetIterations(space);
  cpFloatLeave(mask);
end;

procedure cpSpaceSetIterations(space: cpSpace; iterations: Integer);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpSpaceSetIterations(space, iterations);
  cpFloatLeave(mask);
end;

function cpSpaceGetGravity(space: cpSpace): cpVect;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpSpaceGetGravity(space);
  cpFloatLeave(mask);
end;

procedure cpSpaceSetGravity(space: cpSpace; gravity: cpVect);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpSpaceSetGravity(space, gravity);
  cpFloatLeave(mask);
end;

function cpSpaceGetDamping(space: cpSpace): cpFloat;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpSpaceGetDamping(space);
  cpFloatLeave(mask);
end;

procedure cpSpaceSetDamping(space: cpSpace; damping: cpFloat);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpSpaceSetDamping(space, damping);
  cpFloatLeave(mask);
end;

function cpSpaceGetIdleSpeedThreshold(space: cpSpace): cpFloat;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpSpaceGetIdleSpeedThreshold(space);
  cpFloatLeave(mask);
end;

procedure cpSpaceSetIdleSpeedThreshold(space: cpSpace; idleSpeedThreshold: cpFloat);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpSpaceSetIdleSpeedThreshold(space, idleSpeedThreshold);
  cpFloatLeave(mask);
end;

function cpSpaceGetSleepTimeThreshold(space: cpSpace): cpFloat;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpSpaceGetSleepTimeThreshold(space);
  cpFloatLeave(mask);
end;

procedure cpSpaceSetSleepTimeThreshold(space: cpSpace; sleepTimeThreshold: cpFloat);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpSpaceSetSleepTimeThreshold(space, sleepTimeThreshold);
  cpFloatLeave(mask);
end;

function cpSpaceGetCollisionSlop(space: cpSpace): cpFloat;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpSpaceGetCollisionSlop(space);
  cpFloatLeave(mask);
end;

procedure cpSpaceSetCollisionSlop(space: cpSpace; collisionSlop: cpFloat);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpSpaceSetCollisionSlop(space, collisionSlop);
  cpFloatLeave(mask);
end;

function cpSpaceGetCollisionBias(space: cpSpace): cpFloat;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpSpaceGetCollisionBias(space);
  cpFloatLeave(mask);
end;

procedure cpSpaceSetCollisionBias(space: cpSpace; collisionBias: cpFloat);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpSpaceSetCollisionBias(space, collisionBias);
  cpFloatLeave(mask);
end;

function cpSpaceGetCollisionPersistence(space: cpSpace): cpTimestamp;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpSpaceGetCollisionPersistence(space);
  cpFloatLeave(mask);
end;

procedure cpSpaceSetCollisionPersistence(space: cpSpace; collisionPersistence: cpTimestamp);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpSpaceSetCollisionPersistence(space, collisionPersistence);
  cpFloatLeave(mask);
end;

function cpSpaceGetUserData(space: cpSpace): cpDataPointer;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpSpaceGetUserData(space);
  cpFloatLeave(mask);
end;

procedure cpSpaceSetUserData(space: cpSpace; userData: cpDataPointer);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpSpaceSetUserData(space, userData);
  cpFloatLeave(mask);
end;

function cpSpaceGetStaticBody(space: cpSpace): cpBody;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpSpaceGetStaticBody(space);
  cpFloatLeave(mask);
end;

function cpSpaceGetCurrentTimeStep(space: cpSpace): cpFloat;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpSpaceGetCurrentTimeStep(space);
  cpFloatLeave(mask);
end;

function cpSpaceIsLocked(space: cpSpace): cpBool;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpSpaceIsLocked(space);
  cpFloatLeave(mask);
end;

function cpSpaceAddDefaultCollisionHandler(space: cpSpace): cpCollisionHandler;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpSpaceAddDefaultCollisionHandler(space);
  cpFloatLeave(mask);
end;

function cpSpaceAddCollisionHandler(space: cpSpace; a, b: cpCollisionType): cpCollisionHandler;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpSpaceAddCollisionHandler(space, a, b);
  cpFloatLeave(mask);
end;

function cpSpaceAddWildcardHandler(space: cpSpace; kind: cpCollisionType): cpCollisionHandler;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpSpaceAddWildcardHandler(space, kind);
  cpFloatLeave(mask);
end;

function cpSpaceAddShape(space: cpSpace; shape: cpShape): cpShape;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpSpaceAddShape(space, shape);
  cpFloatLeave(mask);
end;

function cpSpaceAddBody(space: cpSpace; body: cpBody): cpBody;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpSpaceAddBody(space, body);
  cpFloatLeave(mask);
end;

function cpSpaceAddConstraint(space: cpSpace; constraint: cpConstraint): cpConstraint;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpSpaceAddConstraint(space, constraint);
  cpFloatLeave(mask);
end;

procedure cpSpaceRemoveShape(space: cpSpace; shape: cpShape);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  try
    _cpSpaceRemoveShape(space, shape);
  finally
    cpFloatLeave(mask);
  end;
end;

procedure cpSpaceRemoveBody(space: cpSpace; body: cpBody);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  try
    _cpSpaceRemoveBody(space, body);
  finally
    cpFloatLeave(mask);
  end;
end;

procedure cpSpaceRemoveConstraint(space: cpSpace; constraint: cpConstraint);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  try
    _cpSpaceRemoveConstraint(space, constraint);
  finally
    cpFloatLeave(mask);
  end;
end;

function cpSpaceContainsShape(space: cpSpace; shape: cpShape): cpBool;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpSpaceContainsShape(space, shape);
  cpFloatLeave(mask);
end;

function cpSpaceContainsBody(space: cpSpace; body: cpBody): cpBool;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpSpaceContainsBody(space, body);
  cpFloatLeave(mask);
end;

function cpSpaceContainsConstraint(space: cpSpace; constraint: cpConstraint): cpBool;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpSpaceContainsConstraint(space, constraint);
  cpFloatLeave(mask);
end;

function cpSpaceAddPostStepCallback(space: cpSpace; func: cpPostStepFunc; key, data: Pointer): cpBool;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  try
    Result := _cpSpaceAddPostStepCallback(space, func, key, data);
  finally
    cpFloatLeave(mask);
  end;
end;

procedure cpSpacePointQuery(space: cpSpace; point: cpVect; maxDistance: cpFloat; filter: cpShapeFilter;
  func: cpSpacePointQueryFunc; data: Pointer);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  try
    _cpSpacePointQuery(space, point, maxDistance, filter, func, data);
  finally
    cpFloatLeave(mask);
  end;
end;

function cpSpacePointQueryNearest(space: cpSpace; point: cpVect; maxDistance: cpFloat; filter: cpShapeFilter;
  info: cpPointQueryInfo): cpShape;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpSpacePointQueryNearest(space, point, maxDistance, filter, info);
  cpFloatLeave(mask);
end;

procedure cpSpaceSegmentQuery(space: cpSpace; start, finish: cpVect; radius: cpFloat; filter: cpShapeFilter;
  func: cpSpaceSegmentQueryFunc; data: Pointer);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  try
    _cpSpaceSegmentQuery(space, start, finish, radius, filter, func, data);
  finally
    cpFloatLeave(mask);
  end;
end;

function cpSpaceSegmentQueryFirst(space: cpSpace; start, finish: cpVect; radius: cpFloat; filter: cpShapeFilter;
  info: cpSegmentQueryInfo): cpShape;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpSpaceSegmentQueryFirst(space, start, finish, radius, filter, info);
  cpFloatLeave(mask);
end;

procedure cpSpaceBBQuery(space: cpSpace; bb: cpBB; filter: cpShapeFilter; func: cpSpaceBBQueryFunc; data: Pointer);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  try
    _cpSpaceBBQuery(space, bb, filter, func, data);
  finally
    cpFloatLeave(mask);
  end;
end;

function cpSpaceShapeQuery(space: cpSpace; shape: cpShape; func: cpSpaceShapeQueryFunc; data: Pointer): cpBool;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  try
    Result := _cpSpaceShapeQuery(space, shape, func, data);
  finally
    cpFloatLeave(mask);
  end;
end;

procedure cpSpaceEachBody(space: cpSpace; func: cpSpaceBodyIteratorFunc; data: Pointer);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  try
    _cpSpaceEachBody(space, func, data);
  finally
    cpFloatLeave(mask);
  end;
end;

procedure cpSpaceEachShape(space: cpSpace; func: cpSpaceShapeIteratorFunc; data: Pointer);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  try
    _cpSpaceEachShape(space, func, data);
  finally
    cpFloatLeave(mask);
  end;
end;

procedure cpSpaceEachConstraint(space: cpSpace; func: cpSpaceConstraintIteratorFunc; data: Pointer);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  try
    _cpSpaceEachConstraint(space, func, data);
  finally
    cpFloatLeave(mask);
  end;
end;

procedure cpSpaceReindexStatic(space: cpSpace);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpSpaceReindexStatic(space);
  cpFloatLeave(mask);
end;

procedure cpSpaceReindexShape(space: cpSpace; shape: cpShape);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpSpaceReindexShape(space, shape);
  cpFloatLeave(mask);
end;

procedure cpSpaceReindexShapesForBody(space: cpSpace; body: cpBody);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpSpaceReindexShapesForBody(space, body);
  cpFloatLeave(mask);
end;

procedure cpSpaceUseSpatialHash(space: cpSpace; dim: cpFloat; count: Integer);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpSpaceUseSpatialHash(space, dim, count);
  cpFloatLeave(mask);
end;

procedure cpSpaceStep(space: cpSpace; dt: cpFloat);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  try
    _cpSpaceStep(space, dt);
  finally
    cpFloatLeave(mask);
  end;
end;

procedure cpSpaceDebugDraw(space: cpSpace; options: PcpSpaceDebugDrawOptions);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  try
    _cpSpaceDebugDraw(space, options);
  finally
    cpFloatLeave(mask);
  end;
end;

procedure cpMarchSoft(bb: cpBB; x_samples, y_samples: LongWord; threshold: cpFloat;
  segment: cpMarchSegmentFunc; segment_data: Pointer; sample: cpMarchSampleFunc; sample_data: Pointer);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  try
    _cpMarchSoft(bb, x_samples, y_samples, threshold, segment, segment_data, sample, sample_data);
  finally
    cpFloatLeave(mask);
  end;
end;

procedure cpMarchHard(bb: cpBB; x_samples, y_samples: LongWord; threshold: cpFloat;
  segment: cpMarchSegmentFunc; segment_data: Pointer; sample: cpMarchSampleFunc; sample_data: Pointer);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  try
    _cpMarchHard(bb, x_samples, y_samples, threshold, segment, segment_data, sample, sample_data);
  finally
    cpFloatLeave(mask);
  end;
end;

procedure cpPolylineFree(line: cpPolyline);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpPolylineFree(line);
  cpFloatLeave(mask);
end;

function cpPolylineIsClosed(line: cpPolyline): cpBool;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpPolylineIsClosed(line);
  cpFloatLeave(mask);
end;

function cpPolylineSimplifyCurves(line: cpPolyline; tol: cpFloat): cpPolyline;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpPolylineSimplifyCurves(line, tol);
  cpFloatLeave(mask);
end;

function cpPolylineSimplifyVertexes(line: cpPolyline; tol: cpFloat): cpPolyline;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpPolylineSimplifyVertexes(line, tol);
  cpFloatLeave(mask);
end;

function cpPolylineToConvexHull(line: cpPolyline; tol: cpFloat): cpPolyline;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpPolylineToConvexHull(line, tol);
  cpFloatLeave(mask);
end;

function cpPolylineSetAlloc: cpPolylineSet;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpPolylineSetAlloc;
  cpFloatLeave(mask);
end;

function cpPolylineSetInit(set_: cpPolylineSet): cpPolylineSet;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpPolylineSetInit(set_);
  cpFloatLeave(mask);
end;

function cpPolylineSetNew: cpPolylineSet;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpPolylineSetNew;
  cpFloatLeave(mask);
end;

procedure cpPolylineSetDestroy(set_: cpPolylineSet; freePolylines: cpBool);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpPolylineSetDestroy(set_, freePolylines);
  cpFloatLeave(mask);
end;

procedure cpPolylineSetFree(set_: cpPolylineSet; freePolylines: cpBool);
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpPolylineSetFree(set_, freePolylines);
  cpFloatLeave(mask);
end;

procedure cpPolylineSetCollectSegment(v0, v1: cpVect; lines: Pointer); cdecl;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  _cpPolylineSetCollectSegment(v0, v1, lines);
  cpFloatLeave(mask);
end;

function cpPolylineConvexDecomposition(line: cpPolyline; tol: cpFloat): cpPolylineSet;
var
  mask: TFPUExceptionMask;
begin
  mask := cpFloatEnter;
  Result := _cpPolylineConvexDecomposition(line, tol);
  cpFloatLeave(mask);
end;
{ Library checking }

var
  CheckedChipmunk2D: Boolean;
  InitializedChipmunk2D: Boolean;

function InitChipmunk2D(ThrowExceptions: Boolean = False): Boolean;
var
  mask: TFPUExceptionMask;
begin
  if not CheckedChipmunk2D then
  begin
    CheckedChipmunk2D := True;
    { A library built at double precision reads Single arguments as part of
      a Double, so a known result shows which precision it was built at }
    mask := cpFloatEnter;
    InitializedChipmunk2D := Abs(_cpMomentForCircle(2, 0, 3, cpvzero) - 9) < 0.001;
    cpFloatLeave(mask);
  end;
  Result := InitializedChipmunk2D;
  if (not Result) and ThrowExceptions and (@LibraryExceptProc <> nil) then
    LibraryExceptProc(libchipmunk2d, 'cpMomentForCircle (the library is not single precision)');
end;

end.
