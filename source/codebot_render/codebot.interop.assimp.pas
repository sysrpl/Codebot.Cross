(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ Codebot.Interop.Assimp declares the C API of the Open Asset Import Library,
  assimp 5.4, which loads 3D model formats into a common scene structure. The
  library is loaded when InitAssimp is called.

  Records match the C structures of a default assimp build. Define
  ASSIMP_DOUBLE_PRECISION if the library was built with double precision. Pointer
  fields which refer to arrays use array types so they can be indexed directly,
  for example Scene.mMeshes[I].mVertices[J]. Strings in TAiString are UTF-8 and
  can be read using the AiStr function.

  Functions which exist in every assimp 5 release are required by InitAssimp.
  Math helpers and functions added in later releases are optional and are nil
  if the loaded library does not export them. }
unit Codebot.Interop.Assimp;

{$i render.inc}
{$packrecords c}
{$pointermath on}

interface

uses
  Codebot.Core;

{ Basic types }

type
{$ifdef ASSIMP_DOUBLE_PRECISION}
  TAiReal = Double;
{$else}
  { TAiReal is the floating point type used by the library }
  TAiReal = Single;
{$endif}
  { Pointer to a TAiReal }
  PAiReal = ^TAiReal;
  { TAiBool is a C boolean, which is zero for false }
  TAiBool = Integer;
  { TAiReturn is the result code of a function }
  TAiReturn = Integer;

const
  AI_FALSE = 0;
  AI_TRUE = 1;

  aiReturn_SUCCESS = 0;
  aiReturn_FAILURE = -1;
  aiReturn_OUTOFMEMORY = -3;

  AI_MAXLEN = 1024;
  AI_MAX_FACE_INDICES = $7fff;
  AI_MAX_BONE_WEIGHTS = $7fffffff;
  AI_MAX_VERTICES = $7fffffff;
  AI_MAX_FACES = $7fffffff;
  AI_MAX_NUMBER_OF_COLOR_SETS = 8;
  AI_MAX_NUMBER_OF_TEXTURECOORDS = 8;
  HINTMAXTEXTURELEN = 9;
  AI_EMBEDDED_TEXNAME_PREFIX = '*';
  AI_DEFAULT_MATERIAL_NAME = 'DefaultMaterial';

{ aiOrigin }

type
  TAiOrigin = Integer;

const
  aiOrigin_SET = 0;
  aiOrigin_CUR = 1;
  aiOrigin_END = 2;

{ aiDefaultLogStream }

type
  TAiDefaultLogStream = Integer;

const
  aiDefaultLogStream_FILE = 1;
  aiDefaultLogStream_STDOUT = 2;
  aiDefaultLogStream_STDERR = 4;
  aiDefaultLogStream_DEBUGGER = 8;

{ aiPrimitiveType }

const
  aiPrimitiveType_POINT = $1;
  aiPrimitiveType_LINE = $2;
  aiPrimitiveType_TRIANGLE = $4;
  aiPrimitiveType_POLYGON = $8;
  aiPrimitiveType_NGONEncodingFlag = $10;

{ aiMorphingMethod }

type
  TAiMorphingMethod = Integer;

const
  aiMorphingMethod_UNKNOWN = 0;
  aiMorphingMethod_VERTEX_BLEND = 1;
  aiMorphingMethod_MORPH_NORMALIZED = 2;
  aiMorphingMethod_MORPH_RELATIVE = 3;

{ aiTextureOp }

type
  TAiTextureOp = Integer;
  { Pointer to a TAiTextureOp }
  PAiTextureOp = ^TAiTextureOp;

const
  aiTextureOp_Multiply = 0;
  aiTextureOp_Add = 1;
  aiTextureOp_Subtract = 2;
  aiTextureOp_Divide = 3;
  aiTextureOp_SmoothAdd = 4;
  aiTextureOp_SignedAdd = 5;

{ aiTextureMapMode }

type
  TAiTextureMapMode = Integer;
  { Pointer to a TAiTextureMapMode }
  PAiTextureMapMode = ^TAiTextureMapMode;

const
  aiTextureMapMode_Wrap = 0;
  aiTextureMapMode_Clamp = 1;
  aiTextureMapMode_Decal = 3;
  aiTextureMapMode_Mirror = 2;

{ aiTextureMapping }

type
  TAiTextureMapping = Integer;
  { Pointer to a TAiTextureMapping }
  PAiTextureMapping = ^TAiTextureMapping;

const
  aiTextureMapping_UV = 0;
  aiTextureMapping_SPHERE = 1;
  aiTextureMapping_CYLINDER = 2;
  aiTextureMapping_BOX = 3;
  aiTextureMapping_PLANE = 4;
  aiTextureMapping_OTHER = 5;

{ aiTextureType }

type
  TAiTextureType = Integer;

const
  aiTextureType_NONE = 0;
  aiTextureType_DIFFUSE = 1;
  aiTextureType_SPECULAR = 2;
  aiTextureType_AMBIENT = 3;
  aiTextureType_EMISSIVE = 4;
  aiTextureType_HEIGHT = 5;
  aiTextureType_NORMALS = 6;
  aiTextureType_SHININESS = 7;
  aiTextureType_OPACITY = 8;
  aiTextureType_DISPLACEMENT = 9;
  aiTextureType_LIGHTMAP = 10;
  aiTextureType_REFLECTION = 11;
  aiTextureType_BASE_COLOR = 12;
  aiTextureType_NORMAL_CAMERA = 13;
  aiTextureType_EMISSION_COLOR = 14;
  aiTextureType_METALNESS = 15;
  aiTextureType_DIFFUSE_ROUGHNESS = 16;
  aiTextureType_AMBIENT_OCCLUSION = 17;
  aiTextureType_UNKNOWN = 18;
  aiTextureType_SHEEN = 19;
  aiTextureType_CLEARCOAT = 20;
  aiTextureType_TRANSMISSION = 21;
  aiTextureType_MAYA_BASE = 22;
  aiTextureType_MAYA_SPECULAR = 23;
  aiTextureType_MAYA_SPECULAR_COLOR = 24;
  aiTextureType_MAYA_SPECULAR_ROUGHNESS = 25;
  aiTextureType_ANISOTROPY = 26;
  aiTextureType_GLTF_METALLIC_ROUGHNESS = 27;
  AI_TEXTURE_TYPE_MAX = aiTextureType_GLTF_METALLIC_ROUGHNESS;

{ aiShadingMode }

const
  aiShadingMode_Flat = 1;
  aiShadingMode_Gouraud = 2;
  aiShadingMode_Phong = 3;
  aiShadingMode_Blinn = 4;
  aiShadingMode_Toon = 5;
  aiShadingMode_OrenNayar = 6;
  aiShadingMode_Minnaert = 7;
  aiShadingMode_CookTorrance = 8;
  aiShadingMode_NoShading = 9;
  aiShadingMode_Unlit = aiShadingMode_NoShading;
  aiShadingMode_Fresnel = 10;
  aiShadingMode_PBR_BRDF = 11;

{ aiTextureFlags }

const
  aiTextureFlags_Invert = 1;
  aiTextureFlags_UseAlpha = 2;
  aiTextureFlags_IgnoreAlpha = 4;

{ aiBlendMode }

const
  aiBlendMode_Default = 0;
  aiBlendMode_Additive = 1;

{ aiPropertyTypeInfo }

type
  TAiPropertyTypeInfo = Integer;

const
  aiPTI_Float = 1;
  aiPTI_Double = 2;
  aiPTI_String = 3;
  aiPTI_Integer = 4;
  aiPTI_Buffer = 5;

{ aiAnimInterpolation }

type
  TAiAnimInterpolation = Integer;

const
  aiAnimInterpolation_Step = 0;
  aiAnimInterpolation_Linear = 1;
  aiAnimInterpolation_Spherical_Linear = 2;
  aiAnimInterpolation_Cubic_Spline = 3;

{ aiAnimBehaviour }

type
  TAiAnimBehaviour = Integer;

const
  aiAnimBehaviour_DEFAULT = 0;
  aiAnimBehaviour_CONSTANT = 1;
  aiAnimBehaviour_LINEAR = 2;
  aiAnimBehaviour_REPEAT = 3;

{ aiLightSourceType }

type
  TAiLightSourceType = Integer;

const
  aiLightSource_UNDEFINED = 0;
  aiLightSource_DIRECTIONAL = 1;
  aiLightSource_POINT = 2;
  aiLightSource_SPOT = 3;
  aiLightSource_AMBIENT = 4;
  aiLightSource_AREA = 5;

{ aiMetadataType }

type
  TAiMetadataType = Integer;

const
  AI_BOOL = 0;
  AI_INT32 = 1;
  AI_UINT64 = 2;
  AI_FLOAT = 3;
  AI_DOUBLE = 4;
  AI_AISTRING = 5;
  AI_AIVECTOR3D = 6;
  AI_AIMETADATA = 7;
  AI_INT64 = 8;
  AI_UINT32 = 9;
  AI_META_MAX = 10;

{ aiImporterFlags }

const
  aiImporterFlags_SupportTextFlavour = $1;
  aiImporterFlags_SupportBinaryFlavour = $2;
  aiImporterFlags_SupportCompressedFlavour = $4;
  aiImporterFlags_LimitedSupport = $8;
  aiImporterFlags_Experimental = $10;

{ Scene flags }

const
  AI_SCENE_FLAGS_INCOMPLETE = $1;
  AI_SCENE_FLAGS_VALIDATED = $2;
  AI_SCENE_FLAGS_VALIDATION_WARNING = $4;
  AI_SCENE_FLAGS_NON_VERBOSE_FORMAT = $8;
  AI_SCENE_FLAGS_TERRAIN = $10;
  AI_SCENE_FLAGS_ALLOW_SHARED = $20;

{ Compile flags returned by aiGetCompileFlags }

const
  ASSIMP_CFLAGS_SHARED = $1;
  ASSIMP_CFLAGS_STLPORT = $2;
  ASSIMP_CFLAGS_DEBUG = $4;
  ASSIMP_CFLAGS_NOBOOST = $8;
  ASSIMP_CFLAGS_SINGLETHREADED = $10;
  ASSIMP_CFLAGS_DOUBLE_SUPPORT = $20;

{ aiPostProcessSteps flags passed to the import functions }

const
  aiProcess_CalcTangentSpace = $1;
  aiProcess_JoinIdenticalVertices = $2;
  aiProcess_MakeLeftHanded = $4;
  aiProcess_Triangulate = $8;
  aiProcess_RemoveComponent = $10;
  aiProcess_GenNormals = $20;
  aiProcess_GenSmoothNormals = $40;
  aiProcess_SplitLargeMeshes = $80;
  aiProcess_PreTransformVertices = $100;
  aiProcess_LimitBoneWeights = $200;
  aiProcess_ValidateDataStructure = $400;
  aiProcess_ImproveCacheLocality = $800;
  aiProcess_RemoveRedundantMaterials = $1000;
  aiProcess_FixInfacingNormals = $2000;
  aiProcess_PopulateArmatureData = $4000;
  aiProcess_SortByPType = $8000;
  aiProcess_FindDegenerates = $10000;
  aiProcess_FindInvalidData = $20000;
  aiProcess_GenUVCoords = $40000;
  aiProcess_TransformUVCoords = $80000;
  aiProcess_FindInstances = $100000;
  aiProcess_OptimizeMeshes = $200000;
  aiProcess_OptimizeGraph = $400000;
  aiProcess_FlipUVs = $800000;
  aiProcess_FlipWindingOrder = $1000000;
  aiProcess_SplitByBoneCount = $2000000;
  aiProcess_Debone = $4000000;
  aiProcess_GlobalScale = $8000000;
  aiProcess_EmbedTextures = $10000000;
  aiProcess_ForceGenNormals = $20000000;
  aiProcess_DropNormals = $40000000;
  aiProcess_GenBoundingBoxes = $80000000;

  aiProcess_ConvertToLeftHanded = aiProcess_MakeLeftHanded or
    aiProcess_FlipUVs or aiProcess_FlipWindingOrder;
  aiProcessPreset_TargetRealtime_Fast = aiProcess_CalcTangentSpace or
    aiProcess_GenNormals or aiProcess_JoinIdenticalVertices or
    aiProcess_Triangulate or aiProcess_GenUVCoords or aiProcess_SortByPType;
  aiProcessPreset_TargetRealtime_Quality = aiProcess_CalcTangentSpace or
    aiProcess_GenSmoothNormals or aiProcess_JoinIdenticalVertices or
    aiProcess_ImproveCacheLocality or aiProcess_LimitBoneWeights or
    aiProcess_RemoveRedundantMaterials or aiProcess_SplitLargeMeshes or
    aiProcess_Triangulate or aiProcess_GenUVCoords or aiProcess_SortByPType or
    aiProcess_FindDegenerates or aiProcess_FindInvalidData;
  aiProcessPreset_TargetRealtime_MaxQuality = aiProcessPreset_TargetRealtime_Quality or
    aiProcess_FindInstances or aiProcess_ValidateDataStructure or
    aiProcess_OptimizeMeshes;

{ Material keys. Each key is used with a texture type and index which are
  both 0 except for the texture keys, which take a texture type and the index
  of the texture of that type. }

const
  AI_MATKEY_NAME = '?mat.name';
  AI_MATKEY_TWOSIDED = '$mat.twosided';
  AI_MATKEY_SHADING_MODEL = '$mat.shadingm';
  AI_MATKEY_ENABLE_WIREFRAME = '$mat.wireframe';
  AI_MATKEY_BLEND_FUNC = '$mat.blend';
  AI_MATKEY_OPACITY = '$mat.opacity';
  AI_MATKEY_TRANSPARENCYFACTOR = '$mat.transparencyfactor';
  AI_MATKEY_BUMPSCALING = '$mat.bumpscaling';
  AI_MATKEY_SHININESS = '$mat.shininess';
  AI_MATKEY_REFLECTIVITY = '$mat.reflectivity';
  AI_MATKEY_SHININESS_STRENGTH = '$mat.shinpercent';
  AI_MATKEY_REFRACTI = '$mat.refracti';
  AI_MATKEY_COLOR_DIFFUSE = '$clr.diffuse';
  AI_MATKEY_COLOR_AMBIENT = '$clr.ambient';
  AI_MATKEY_COLOR_SPECULAR = '$clr.specular';
  AI_MATKEY_COLOR_EMISSIVE = '$clr.emissive';
  AI_MATKEY_COLOR_TRANSPARENT = '$clr.transparent';
  AI_MATKEY_COLOR_REFLECTIVE = '$clr.reflective';
  AI_MATKEY_GLOBAL_BACKGROUND_IMAGE = '?bg.global';
  AI_MATKEY_GLOBAL_SHADERLANG = '?sh.lang';
  AI_MATKEY_SHADER_VERTEX = '?sh.vs';
  AI_MATKEY_SHADER_FRAGMENT = '?sh.fs';
  AI_MATKEY_SHADER_GEO = '?sh.gs';
  AI_MATKEY_SHADER_TESSELATION = '?sh.ts';
  AI_MATKEY_SHADER_PRIMITIVE = '?sh.ps';
  AI_MATKEY_SHADER_COMPUTE = '?sh.cs';
  AI_MATKEY_USE_COLOR_MAP = '$mat.useColorMap';
  AI_MATKEY_BASE_COLOR = '$clr.base';
  AI_MATKEY_USE_METALLIC_MAP = '$mat.useMetallicMap';
  AI_MATKEY_METALLIC_FACTOR = '$mat.metallicFactor';
  AI_MATKEY_USE_ROUGHNESS_MAP = '$mat.useRoughnessMap';
  AI_MATKEY_ROUGHNESS_FACTOR = '$mat.roughnessFactor';
  AI_MATKEY_ANISOTROPY_FACTOR = '$mat.anisotropyFactor';
  AI_MATKEY_SPECULAR_FACTOR = '$mat.specularFactor';
  AI_MATKEY_GLOSSINESS_FACTOR = '$mat.glossinessFactor';
  AI_MATKEY_SHEEN_COLOR_FACTOR = '$clr.sheen.factor';
  AI_MATKEY_SHEEN_ROUGHNESS_FACTOR = '$mat.sheen.roughnessFactor';
  AI_MATKEY_CLEARCOAT_FACTOR = '$mat.clearcoat.factor';
  AI_MATKEY_CLEARCOAT_ROUGHNESS_FACTOR = '$mat.clearcoat.roughnessFactor';
  AI_MATKEY_TRANSMISSION_FACTOR = '$mat.transmission.factor';
  AI_MATKEY_VOLUME_THICKNESS_FACTOR = '$mat.volume.thicknessFactor';
  AI_MATKEY_VOLUME_ATTENUATION_DISTANCE = '$mat.volume.attenuationDistance';
  AI_MATKEY_VOLUME_ATTENUATION_COLOR = '$mat.volume.attenuationColor';
  AI_MATKEY_USE_EMISSIVE_MAP = '$mat.useEmissiveMap';
  AI_MATKEY_EMISSIVE_INTENSITY = '$mat.emissiveIntensity';
  AI_MATKEY_USE_AO_MAP = '$mat.useAOMap';
  AI_MATKEY_ANISOTROPY_ROTATION = '$mat.anisotropyRotation';
  { Texture keys }
  AI_MATKEY_TEXTURE = '$tex.file';
  AI_MATKEY_UVWSRC = '$tex.uvwsrc';
  AI_MATKEY_TEXOP = '$tex.op';
  AI_MATKEY_MAPPING = '$tex.mapping';
  AI_MATKEY_TEXBLEND = '$tex.blend';
  AI_MATKEY_MAPPINGMODE_U = '$tex.mapmodeu';
  AI_MATKEY_MAPPINGMODE_V = '$tex.mapmodev';
  AI_MATKEY_TEXMAP_AXIS = '$tex.mapaxis';
  AI_MATKEY_UVTRANSFORM = '$tex.uvtrafo';
  AI_MATKEY_TEXFLAGS = '$tex.flags';

{ Structures }

type
  PAiVector2D = ^TAiVector2D;
  { TAiVector2D is a 2D vector }
  TAiVector2D = record
    x, y: TAiReal;
  end;

  { Pointer to a TAiVector3D }
  PAiVector3D = ^TAiVector3D;
  { TAiVector3D is a 3D vector }
  TAiVector3D = record
    x, y, z: TAiReal;
  end;
  { An array of TAiVector3D, for indexing a pointer to many of them }
  TAiVector3DArray = array[0..MaxInt div SizeOf(TAiVector3D) - 1] of TAiVector3D;
  { Pointer to a TAiVector3DArray }
  PAiVector3DArray = ^TAiVector3DArray;

  { Pointer to a TAiColor3D }
  PAiColor3D = ^TAiColor3D;
  { TAiColor3D is a color as red, green, and blue }
  TAiColor3D = record
    r, g, b: Single;
  end;

  { Pointer to a TAiColor4D }
  PAiColor4D = ^TAiColor4D;
  { TAiColor4D is a color as red, green, blue, and alpha }
  TAiColor4D = record
    r, g, b, a: Single;
  end;
  { An array of TAiColor4D, for indexing a pointer to many of them }
  TAiColor4DArray = array[0..MaxInt div SizeOf(TAiColor4D) - 1] of TAiColor4D;
  { Pointer to a TAiColor4DArray }
  PAiColor4DArray = ^TAiColor4DArray;

  { Pointer to a TAiQuaternion }
  PAiQuaternion = ^TAiQuaternion;
  { TAiQuaternion is a rotation as a quaternion }
  TAiQuaternion = record
    w, x, y, z: TAiReal;
  end;

  { Pointer to a TAiMatrix3x3 }
  PAiMatrix3x3 = ^TAiMatrix3x3;
  { TAiMatrix3x3 is a 3 by 3 matrix }
  TAiMatrix3x3 = record
    a1, a2, a3: TAiReal;
    b1, b2, b3: TAiReal;
    c1, c2, c3: TAiReal;
  end;

  { Row major, unlike the column major matrices used by OpenGL }
  PAiMatrix4x4 = ^TAiMatrix4x4;
  { TAiMatrix4x4 is a 4 by 4 matrix }
  TAiMatrix4x4 = record
    a1, a2, a3, a4: TAiReal;
    b1, b2, b3, b4: TAiReal;
    c1, c2, c3, c4: TAiReal;
    d1, d2, d3, d4: TAiReal;
  end;

  { Pointer to a TAiPlane }
  PAiPlane = ^TAiPlane;
  { TAiPlane is a plane }
  TAiPlane = record
    a, b, c, d: TAiReal;
  end;

  { Pointer to a TAiRay }
  PAiRay = ^TAiRay;
  { TAiRay is a ray with a position and a direction }
  TAiRay = record
    pos, dir: TAiVector3D;
  end;

  { Pointer to a TAiAABB }
  PAiAABB = ^TAiAABB;
  { TAiAABB is an axis aligned bounding box }
  TAiAABB = record
    mMin: TAiVector3D;
    mMax: TAiVector3D;
  end;

  { A UTF-8 string of length bytes which is also null terminated }
  PAiString = ^TAiString;
  { TAiString is a string with its length, as the library stores them }
  TAiString = record
    length: LongWord;
    data: array[0..AI_MAXLEN - 1] of AnsiChar;
  end;
  { An array of TAiString, for indexing a pointer to many of them }
  TAiStringArray = array[0..MaxInt div SizeOf(TAiString) - 1] of TAiString;
  { Pointer to a TAiStringArray }
  PAiStringArray = ^TAiStringArray;
  { An array of PAiString, for indexing a pointer to many of them }
  TAiStringPtrArray = array[0..MaxInt div SizeOf(Pointer) - 1] of PAiString;
  { Pointer to a TAiStringPtrArray }
  PAiStringPtrArray = ^TAiStringPtrArray;

  { Pointer to a TAiMemoryInfo }
  PAiMemoryInfo = ^TAiMemoryInfo;
  { TAiMemoryInfo is the memory used by the parts of a scene }
  TAiMemoryInfo = record
    textures: LongWord;
    materials: LongWord;
    meshes: LongWord;
    nodes: LongWord;
    animations: LongWord;
    cameras: LongWord;
    lights: LongWord;
    total: LongWord;
  end;

  { Pointer to a TAiBuffer }
  PAiBuffer = ^TAiBuffer;
  { TAiBuffer is a block of memory }
  TAiBuffer = record
    data: PAnsiChar;
    end_: PAnsiChar;
  end;

  { An array of LongWord, for indexing a pointer to many of them }
  TLongWordArray = array[0..MaxInt div SizeOf(LongWord) - 1] of LongWord;
  { Pointer to a TLongWordArray }
  PLongWordArray = ^TLongWordArray;
  { An array of Double, for indexing a pointer to many of them }
  TDoubleArray = array[0..MaxInt div SizeOf(Double) - 1] of Double;
  { Pointer to a TDoubleArray }
  PDoubleArray = ^TDoubleArray;

{ Metadata }

  PAiMetadata = ^TAiMetadata;

  { Pointer to a TAiMetadataEntry }
  PAiMetadataEntry = ^TAiMetadataEntry;
  { TAiMetadataEntry is one value of the metadata of a node }
  TAiMetadataEntry = record
    mType: TAiMetadataType;
    mData: Pointer;
  end;
  { An array of TAiMetadataEntry, for indexing a pointer to many of them }
  TAiMetadataEntryArray = array[0..MaxInt div SizeOf(TAiMetadataEntry) - 1] of TAiMetadataEntry;
  { Pointer to a TAiMetadataEntryArray }
  PAiMetadataEntryArray = ^TAiMetadataEntryArray;

  { TAiMetadata is named values attached to a node or a scene }
  TAiMetadata = record
    mNumProperties: LongWord;
    mKeys: PAiStringArray;
    mValues: PAiMetadataEntryArray;
  end;

{ Scene nodes }

  PAiNode = ^TAiNode;
  { An array of PAiNode, for indexing a pointer to many of them }
  TAiNodePtrArray = array[0..MaxInt div SizeOf(Pointer) - 1] of PAiNode;
  { Pointer to a TAiNodePtrArray }
  PAiNodePtrArray = ^TAiNodePtrArray;

  { TAiNode is a node of the scene hierarchy, with a transform, meshes, and
    child nodes }
  TAiNode = record
    mName: TAiString;
    mTransformation: TAiMatrix4x4;
    mParent: PAiNode;
    mNumChildren: LongWord;
    mChildren: PAiNodePtrArray;
    mNumMeshes: LongWord;
    mMeshes: PLongWordArray;
    mMetaData: PAiMetadata;
  end;

{ Meshes }

  PAiFace = ^TAiFace;
  { TAiFace is a face of a mesh, as indices into its vertices }
  TAiFace = record
    mNumIndices: LongWord;
    mIndices: PLongWordArray;
  end;
  { An array of TAiFace, for indexing a pointer to many of them }
  TAiFaceArray = array[0..MaxInt div SizeOf(TAiFace) - 1] of TAiFace;
  { Pointer to a TAiFaceArray }
  PAiFaceArray = ^TAiFaceArray;

  { Pointer to a TAiVertexWeight }
  PAiVertexWeight = ^TAiVertexWeight;
  { TAiVertexWeight is how much a bone moves one vertex }
  TAiVertexWeight = record
    mVertexId: LongWord;
    mWeight: TAiReal;
  end;
  { An array of TAiVertexWeight, for indexing a pointer to many of them }
  TAiVertexWeightArray = array[0..MaxInt div SizeOf(TAiVertexWeight) - 1] of TAiVertexWeight;
  { Pointer to a TAiVertexWeightArray }
  PAiVertexWeightArray = ^TAiVertexWeightArray;

  { Pointer to a TAiBone }
  PAiBone = ^TAiBone;
  { TAiBone is a bone of a mesh and the vertices it moves }
  TAiBone = record
    mName: TAiString;
    mNumWeights: LongWord;
  {$ifndef ASSIMP_BUILD_NO_ARMATUREPOPULATE_PROCESS}
    mArmature: PAiNode;
    mNode: PAiNode;
  {$endif}
    mWeights: PAiVertexWeightArray;
    mOffsetMatrix: TAiMatrix4x4;
  end;
  { An array of PAiBone, for indexing a pointer to many of them }
  TAiBonePtrArray = array[0..MaxInt div SizeOf(Pointer) - 1] of PAiBone;
  { Pointer to a TAiBonePtrArray }
  PAiBonePtrArray = ^TAiBonePtrArray;

  { TAiColorSets is the color sets of a mesh }
  TAiColorSets = array[0..AI_MAX_NUMBER_OF_COLOR_SETS - 1] of PAiColor4DArray;
  { TAiTextureCoordSets is the texture coordinate sets of a mesh }
  TAiTextureCoordSets = array[0..AI_MAX_NUMBER_OF_TEXTURECOORDS - 1] of PAiVector3DArray;

  { Pointer to a TAiAnimMesh }
  PAiAnimMesh = ^TAiAnimMesh;
  { TAiAnimMesh is a replacement for the vertices of a mesh, used for morph
    animation }
  TAiAnimMesh = record
    mName: TAiString;
    mVertices: PAiVector3DArray;
    mNormals: PAiVector3DArray;
    mTangents: PAiVector3DArray;
    mBitangents: PAiVector3DArray;
    mColors: TAiColorSets;
    mTextureCoords: TAiTextureCoordSets;
    mNumVertices: LongWord;
    mWeight: Single;
  end;
  { An array of PAiAnimMesh, for indexing a pointer to many of them }
  TAiAnimMeshPtrArray = array[0..MaxInt div SizeOf(Pointer) - 1] of PAiAnimMesh;
  { Pointer to a TAiAnimMeshPtrArray }
  PAiAnimMeshPtrArray = ^TAiAnimMeshPtrArray;

  { Pointer to a TAiMesh }
  PAiMesh = ^TAiMesh;
  { TAiMesh is a mesh of vertices, faces, and bones with one material }
  TAiMesh = record
    mPrimitiveTypes: LongWord;
    mNumVertices: LongWord;
    mNumFaces: LongWord;
    mVertices: PAiVector3DArray;
    mNormals: PAiVector3DArray;
    mTangents: PAiVector3DArray;
    mBitangents: PAiVector3DArray;
    mColors: TAiColorSets;
    mTextureCoords: TAiTextureCoordSets;
    mNumUVComponents: array[0..AI_MAX_NUMBER_OF_TEXTURECOORDS - 1] of LongWord;
    mFaces: PAiFaceArray;
    mNumBones: LongWord;
    mBones: PAiBonePtrArray;
    mMaterialIndex: LongWord;
    mName: TAiString;
    mNumAnimMeshes: LongWord;
    mAnimMeshes: PAiAnimMeshPtrArray;
    mMethod: TAiMorphingMethod;
    mAABB: TAiAABB;
    mTextureCoordsNames: PAiStringPtrArray;
  end;
  { An array of PAiMesh, for indexing a pointer to many of them }
  TAiMeshPtrArray = array[0..MaxInt div SizeOf(Pointer) - 1] of PAiMesh;
  { Pointer to a TAiMeshPtrArray }
  PAiMeshPtrArray = ^TAiMeshPtrArray;

  { Pointer to a TAiSkeletonBone }
  PAiSkeletonBone = ^TAiSkeletonBone;
  { TAiSkeletonBone is a bone of a skeleton }
  TAiSkeletonBone = record
    mParent: Integer;
  {$ifndef ASSIMP_BUILD_NO_ARMATUREPOPULATE_PROCESS}
    mArmature: PAiNode;
    mNode: PAiNode;
  {$endif}
    mNumnWeights: LongWord;
    mMeshId: PAiMesh;
    mWeights: PAiVertexWeightArray;
    mOffsetMatrix: TAiMatrix4x4;
    mLocalMatrix: TAiMatrix4x4;
  end;
  { An array of PAiSkeletonBone, for indexing a pointer to many of them }
  TAiSkeletonBonePtrArray = array[0..MaxInt div SizeOf(Pointer) - 1] of PAiSkeletonBone;
  { Pointer to a TAiSkeletonBonePtrArray }
  PAiSkeletonBonePtrArray = ^TAiSkeletonBonePtrArray;

  { Pointer to a TAiSkeleton }
  PAiSkeleton = ^TAiSkeleton;
  { TAiSkeleton is a skeleton of bones }
  TAiSkeleton = record
    mName: TAiString;
    mNumBones: LongWord;
    mBones: PAiSkeletonBonePtrArray;
  end;
  { An array of PAiSkeleton, for indexing a pointer to many of them }
  TAiSkeletonPtrArray = array[0..MaxInt div SizeOf(Pointer) - 1] of PAiSkeleton;
  { Pointer to a TAiSkeletonPtrArray }
  PAiSkeletonPtrArray = ^TAiSkeletonPtrArray;

{ Textures }

  PAiTexel = ^TAiTexel;
  { TAiTexel is a pixel of an embedded texture }
  TAiTexel = packed record
    b, g, r, a: Byte;
  end;
  { An array of TAiTexel, for indexing a pointer to many of them }
  TAiTexelArray = array[0..MaxInt div SizeOf(TAiTexel) - 1] of TAiTexel;
  { Pointer to a TAiTexelArray }
  PAiTexelArray = ^TAiTexelArray;

  { When mHeight is 0 the texture is compressed, mWidth is its size in bytes,
    pcData holds the file data, and achFormatHint holds its extension }
  PAiTexture = ^TAiTexture;
  { TAiTexture is a texture embedded in the model file }
  TAiTexture = record
    mWidth: LongWord;
    mHeight: LongWord;
    achFormatHint: array[0..HINTMAXTEXTURELEN - 1] of AnsiChar;
    pcData: PAiTexelArray;
    mFilename: TAiString;
  end;
  { An array of PAiTexture, for indexing a pointer to many of them }
  TAiTexturePtrArray = array[0..MaxInt div SizeOf(Pointer) - 1] of PAiTexture;
  { Pointer to a TAiTexturePtrArray }
  PAiTexturePtrArray = ^TAiTexturePtrArray;

{ Materials }

  PAiUVTransform = ^TAiUVTransform;
  { TAiUVTransform is a transform of texture coordinates }
  TAiUVTransform = record
    mTranslation: TAiVector2D;
    mScaling: TAiVector2D;
    mRotation: TAiReal;
  end;

  { Pointer to a TAiMaterialProperty }
  PAiMaterialProperty = ^TAiMaterialProperty;
  { TAiMaterialProperty is one property of a material }
  TAiMaterialProperty = record
    mKey: TAiString;
    mSemantic: LongWord;
    mIndex: LongWord;
    mDataLength: LongWord;
    mType: TAiPropertyTypeInfo;
    mData: PAnsiChar;
  end;
  { An array of PAiMaterialProperty, for indexing a pointer to many of them }
  TAiMaterialPropertyPtrArray = array[0..MaxInt div SizeOf(Pointer) - 1] of PAiMaterialProperty;
  { Pointer to a TAiMaterialPropertyPtrArray }
  PAiMaterialPropertyPtrArray = ^TAiMaterialPropertyPtrArray;

  { Pointer to a TAiMaterial }
  PAiMaterial = ^TAiMaterial;
  { TAiMaterial is a material, which is a list of properties }
  TAiMaterial = record
    mProperties: PAiMaterialPropertyPtrArray;
    mNumProperties: LongWord;
    mNumAllocated: LongWord;
  end;
  { An array of PAiMaterial, for indexing a pointer to many of them }
  TAiMaterialPtrArray = array[0..MaxInt div SizeOf(Pointer) - 1] of PAiMaterial;
  { Pointer to a TAiMaterialPtrArray }
  PAiMaterialPtrArray = ^TAiMaterialPtrArray;

{ Animations }

  { Animation keys gained mInterpolation in assimp 5.4. With older libraries
    it is not valid, and quaternion keys are smaller, so use aiRotationKey to
    read rotation keys rather than indexing mRotationKeys. }
  PAiVectorKey = ^TAiVectorKey;
  { TAiVectorKey is a position or scale at a time in an animation }
  TAiVectorKey = record
    mTime: Double;
    mValue: TAiVector3D;
    mInterpolation: TAiAnimInterpolation;
  end;
  { An array of TAiVectorKey, for indexing a pointer to many of them }
  TAiVectorKeyArray = array[0..MaxInt div SizeOf(TAiVectorKey) - 1] of TAiVectorKey;
  { Pointer to a TAiVectorKeyArray }
  PAiVectorKeyArray = ^TAiVectorKeyArray;

  { Pointer to a TAiQuatKey }
  PAiQuatKey = ^TAiQuatKey;
  { TAiQuatKey is a rotation at a time in an animation }
  TAiQuatKey = record
    mTime: Double;
    mValue: TAiQuaternion;
    mInterpolation: TAiAnimInterpolation;
  end;
  { An array of TAiQuatKey, for indexing a pointer to many of them }
  TAiQuatKeyArray = array[0..MaxInt div SizeOf(TAiQuatKey) - 1] of TAiQuatKey;
  { Pointer to a TAiQuatKeyArray }
  PAiQuatKeyArray = ^TAiQuatKeyArray;

  { Pointer to a TAiMeshKey }
  PAiMeshKey = ^TAiMeshKey;
  { TAiMeshKey is the mesh to show at a time in an animation }
  TAiMeshKey = record
    mTime: Double;
    mValue: LongWord;
  end;
  { An array of TAiMeshKey, for indexing a pointer to many of them }
  TAiMeshKeyArray = array[0..MaxInt div SizeOf(TAiMeshKey) - 1] of TAiMeshKey;
  { Pointer to a TAiMeshKeyArray }
  PAiMeshKeyArray = ^TAiMeshKeyArray;

  { Pointer to a TAiMeshMorphKey }
  PAiMeshMorphKey = ^TAiMeshMorphKey;
  { TAiMeshMorphKey is morph weights at a time in an animation }
  TAiMeshMorphKey = record
    mTime: Double;
    mValues: PLongWordArray;
    mWeights: PDoubleArray;
    mNumValuesAndWeights: LongWord;
  end;
  { An array of TAiMeshMorphKey, for indexing a pointer to many of them }
  TAiMeshMorphKeyArray = array[0..MaxInt div SizeOf(TAiMeshMorphKey) - 1] of TAiMeshMorphKey;
  { Pointer to a TAiMeshMorphKeyArray }
  PAiMeshMorphKeyArray = ^TAiMeshMorphKeyArray;

  { Pointer to a TAiNodeAnim }
  PAiNodeAnim = ^TAiNodeAnim;
  { TAiNodeAnim is the animation of one node }
  TAiNodeAnim = record
    mNodeName: TAiString;
    mNumPositionKeys: LongWord;
    mPositionKeys: PAiVectorKeyArray;
    mNumRotationKeys: LongWord;
    mRotationKeys: PAiQuatKeyArray;
    mNumScalingKeys: LongWord;
    mScalingKeys: PAiVectorKeyArray;
    mPreState: TAiAnimBehaviour;
    mPostState: TAiAnimBehaviour;
  end;
  { An array of PAiNodeAnim, for indexing a pointer to many of them }
  TAiNodeAnimPtrArray = array[0..MaxInt div SizeOf(Pointer) - 1] of PAiNodeAnim;
  { Pointer to a TAiNodeAnimPtrArray }
  PAiNodeAnimPtrArray = ^TAiNodeAnimPtrArray;

  { Pointer to a TAiMeshAnim }
  PAiMeshAnim = ^TAiMeshAnim;
  { TAiMeshAnim is the animation of one mesh by replacing it }
  TAiMeshAnim = record
    mName: TAiString;
    mNumKeys: LongWord;
    mKeys: PAiMeshKeyArray;
  end;
  { An array of PAiMeshAnim, for indexing a pointer to many of them }
  TAiMeshAnimPtrArray = array[0..MaxInt div SizeOf(Pointer) - 1] of PAiMeshAnim;
  { Pointer to a TAiMeshAnimPtrArray }
  PAiMeshAnimPtrArray = ^TAiMeshAnimPtrArray;

  { Pointer to a TAiMeshMorphAnim }
  PAiMeshMorphAnim = ^TAiMeshMorphAnim;
  { TAiMeshMorphAnim is the animation of one mesh by morphing }
  TAiMeshMorphAnim = record
    mName: TAiString;
    mNumKeys: LongWord;
    mKeys: PAiMeshMorphKeyArray;
  end;
  { An array of PAiMeshMorphAnim, for indexing a pointer to many of them }
  TAiMeshMorphAnimPtrArray = array[0..MaxInt div SizeOf(Pointer) - 1] of PAiMeshMorphAnim;
  { Pointer to a TAiMeshMorphAnimPtrArray }
  PAiMeshMorphAnimPtrArray = ^TAiMeshMorphAnimPtrArray;

  { Pointer to a TAiAnimation }
  PAiAnimation = ^TAiAnimation;
  { TAiAnimation is an animation, with a channel for each node it moves }
  TAiAnimation = record
    mName: TAiString;
    mDuration: Double;
    mTicksPerSecond: Double;
    mNumChannels: LongWord;
    mChannels: PAiNodeAnimPtrArray;
    mNumMeshChannels: LongWord;
    mMeshChannels: PAiMeshAnimPtrArray;
    mNumMorphMeshChannels: LongWord;
    mMorphMeshChannels: PAiMeshMorphAnimPtrArray;
  end;
  { An array of PAiAnimation, for indexing a pointer to many of them }
  TAiAnimationPtrArray = array[0..MaxInt div SizeOf(Pointer) - 1] of PAiAnimation;
  { Pointer to a TAiAnimationPtrArray }
  PAiAnimationPtrArray = ^TAiAnimationPtrArray;

{ Cameras and lights }

  PAiCamera = ^TAiCamera;
  { TAiCamera is a camera in the scene }
  TAiCamera = record
    mName: TAiString;
    mPosition: TAiVector3D;
    mUp: TAiVector3D;
    mLookAt: TAiVector3D;
    mHorizontalFOV: Single;
    mClipPlaneNear: Single;
    mClipPlaneFar: Single;
    mAspect: Single;
    mOrthographicWidth: Single;
  end;
  { An array of PAiCamera, for indexing a pointer to many of them }
  TAiCameraPtrArray = array[0..MaxInt div SizeOf(Pointer) - 1] of PAiCamera;
  { Pointer to a TAiCameraPtrArray }
  PAiCameraPtrArray = ^TAiCameraPtrArray;

  { Pointer to a TAiLight }
  PAiLight = ^TAiLight;
  { TAiLight is a light in the scene }
  TAiLight = record
    mName: TAiString;
    mType: TAiLightSourceType;
    mPosition: TAiVector3D;
    mDirection: TAiVector3D;
    mUp: TAiVector3D;
    mAttenuationConstant: Single;
    mAttenuationLinear: Single;
    mAttenuationQuadratic: Single;
    mColorDiffuse: TAiColor3D;
    mColorSpecular: TAiColor3D;
    mColorAmbient: TAiColor3D;
    mAngleInnerCone: Single;
    mAngleOuterCone: Single;
    mSize: TAiVector2D;
  end;
  { An array of PAiLight, for indexing a pointer to many of them }
  TAiLightPtrArray = array[0..MaxInt div SizeOf(Pointer) - 1] of PAiLight;
  { Pointer to a TAiLightPtrArray }
  PAiLightPtrArray = ^TAiLightPtrArray;

{ Scenes }

  PAiScene = ^TAiScene;
  { TAiScene is the root of everything imported from a model file }
  TAiScene = record
    mFlags: LongWord;
    mRootNode: PAiNode;
    mNumMeshes: LongWord;
    mMeshes: PAiMeshPtrArray;
    mNumMaterials: LongWord;
    mMaterials: PAiMaterialPtrArray;
    mNumAnimations: LongWord;
    mAnimations: PAiAnimationPtrArray;
    mNumTextures: LongWord;
    mTextures: PAiTexturePtrArray;
    mNumLights: LongWord;
    mLights: PAiLightPtrArray;
    mNumCameras: LongWord;
    mCameras: PAiCameraPtrArray;
    mMetaData: PAiMetadata;
    mName: TAiString;
    mNumSkeletons: LongWord;
    mSkeletons: PAiSkeletonPtrArray;
    mPrivate: PAnsiChar;
  end;

{ Importer descriptions }

  PAiImporterDesc = ^TAiImporterDesc;
  { TAiImporterDesc is a description of the importer for a file format }
  TAiImporterDesc = record
    mName: PAnsiChar;
    mAuthor: PAnsiChar;
    mMaintainer: PAnsiChar;
    mComments: PAnsiChar;
    mFlags: LongWord;
    mMinMajor: LongWord;
    mMinMinor: LongWord;
    mMaxMajor: LongWord;
    mMaxMinor: LongWord;
    mFileExtensions: PAnsiChar;
  end;

{ Logging }

  TAiLogStreamCallback = procedure(msg: PAnsiChar; user: PAnsiChar); cdecl;

  { Pointer to a TAiLogStream }
  PAiLogStream = ^TAiLogStream;
  { TAiLogStream is a callback which receives log messages }
  TAiLogStream = record
    callback: TAiLogStreamCallback;
    user: PAnsiChar;
  end;

{ Import properties are an opaque handle created by aiCreatePropertyStore }

  PAiPropertyStore = ^TAiPropertyStore;
  { TAiPropertyStore is an opaque store of import properties }
  TAiPropertyStore = record
    sentinel: AnsiChar;
  end;

{ Custom file systems }

  PAiFileIO = ^TAiFileIO;
  { Pointer to a TAiFile }
  PAiFile = ^TAiFile;

  { TAiFileWriteProc is one of the callbacks for reading and writing files
    with your own code, as are the types which follow }
  TAiFileWriteProc = function(f: PAiFile; buffer: PAnsiChar; size, count: NativeUInt): NativeUInt; cdecl;
  TAiFileReadProc = function(f: PAiFile; buffer: PAnsiChar; size, count: NativeUInt): NativeUInt; cdecl;
  TAiFileTellProc = function(f: PAiFile): NativeUInt; cdecl;
  TAiFileFlushProc = procedure(f: PAiFile); cdecl;
  TAiFileSeek = function(f: PAiFile; offset: NativeUInt; origin: TAiOrigin): TAiReturn; cdecl;
  TAiFileOpenProc = function(io: PAiFileIO; fileName, mode: PAnsiChar): PAiFile; cdecl;
  TAiFileCloseProc = procedure(io: PAiFileIO; f: PAiFile); cdecl;

  { TAiFileIO is the callbacks which open and close files with your own code }
  TAiFileIO = record
    OpenProc: TAiFileOpenProc;
    CloseProc: TAiFileCloseProc;
    UserData: PAnsiChar;
  end;

  { TAiFile is the callbacks which read and write one open file }
  TAiFile = record
    ReadProc: TAiFileReadProc;
    WriteProc: TAiFileWriteProc;
    TellProc: TAiFileTellProc;
    FileSizeProc: TAiFileTellProc;
    SeekProc: TAiFileSeek;
    FlushProc: TAiFileFlushProc;
    UserData: PAnsiChar;
  end;

{ Functions }

var
  aiImportFile: function(pFile: PAnsiChar; pFlags: LongWord): PAiScene; cdecl;
  aiImportFileEx: function(pFile: PAnsiChar; pFlags: LongWord; pFS: PAiFileIO): PAiScene; cdecl;
  aiImportFileExWithProperties: function(pFile: PAnsiChar; pFlags: LongWord; pFS: PAiFileIO; pProps: PAiPropertyStore): PAiScene; cdecl;
  aiImportFileFromMemory: function(pBuffer: Pointer; pLength: LongWord; pFlags: LongWord; pHint: PAnsiChar): PAiScene; cdecl;
  aiImportFileFromMemoryWithProperties: function(pBuffer: Pointer; pLength: LongWord; pFlags: LongWord; pHint: PAnsiChar; pProps: PAiPropertyStore): PAiScene; cdecl;
  aiApplyPostProcessing: function(pScene: PAiScene; pFlags: LongWord): PAiScene; cdecl;
  aiGetPredefinedLogStream: function(pStreams: TAiDefaultLogStream; file_: PAnsiChar): TAiLogStream; cdecl;
  aiAttachLogStream: procedure(stream: PAiLogStream); cdecl;
  aiEnableVerboseLogging: procedure(d: TAiBool); cdecl;
  aiDetachLogStream: function(stream: PAiLogStream): TAiReturn; cdecl;
  aiDetachAllLogStreams: procedure; cdecl;
  aiReleaseImport: procedure(pScene: PAiScene); cdecl;
  aiGetErrorString: function: PAnsiChar; cdecl;
  aiIsExtensionSupported: function(szExtension: PAnsiChar): TAiBool; cdecl;
  aiGetExtensionList: procedure(szOut: PAiString); cdecl;
  aiGetMemoryRequirements: procedure(pIn: PAiScene; info: PAiMemoryInfo); cdecl;
  aiGetEmbeddedTexture: function(pIn: PAiScene; filename: PAnsiChar): PAiTexture; cdecl;
  aiCreatePropertyStore: function: PAiPropertyStore; cdecl;
  aiReleasePropertyStore: procedure(p: PAiPropertyStore); cdecl;
  aiSetImportPropertyInteger: procedure(store: PAiPropertyStore; szName: PAnsiChar; value: Integer); cdecl;
  aiSetImportPropertyFloat: procedure(store: PAiPropertyStore; szName: PAnsiChar; value: TAiReal); cdecl;
  aiSetImportPropertyString: procedure(store: PAiPropertyStore; szName: PAnsiChar; st: PAiString); cdecl;
  aiSetImportPropertyMatrix: procedure(store: PAiPropertyStore; szName: PAnsiChar; mat: PAiMatrix4x4); cdecl;
  aiGetImportFormatCount: function: NativeUInt; cdecl;
  aiGetImportFormatDescription: function(pIndex: NativeUInt): PAiImporterDesc; cdecl;
  aiGetImporterDesc: function(extension: PAnsiChar): PAiImporterDesc; cdecl;
  aiTextureTypeToString: function(in_: TAiTextureType): PAnsiChar; cdecl;
  aiGetMaterialProperty: function(pMat: PAiMaterial; pKey: PAnsiChar; type_, index: LongWord; out pPropOut: PAiMaterialProperty): TAiReturn; cdecl;
  aiGetMaterialFloatArray: function(pMat: PAiMaterial; pKey: PAnsiChar; type_, index: LongWord; pOut: PAiReal; pMax: PLongWord): TAiReturn; cdecl;
  aiGetMaterialIntegerArray: function(pMat: PAiMaterial; pKey: PAnsiChar; type_, index: LongWord; pOut: PInteger; pMax: PLongWord): TAiReturn; cdecl;
  aiGetMaterialColor: function(pMat: PAiMaterial; pKey: PAnsiChar; type_, index: LongWord; pOut: PAiColor4D): TAiReturn; cdecl;
  aiGetMaterialUVTransform: function(pMat: PAiMaterial; pKey: PAnsiChar; type_, index: LongWord; pOut: PAiUVTransform): TAiReturn; cdecl;
  aiGetMaterialString: function(pMat: PAiMaterial; pKey: PAnsiChar; type_, index: LongWord; pOut: PAiString): TAiReturn; cdecl;
  aiGetMaterialTextureCount: function(pMat: PAiMaterial; type_: TAiTextureType): LongWord; cdecl;
  aiGetMaterialTexture: function(mat: PAiMaterial; type_: TAiTextureType; index: LongWord; path: PAiString; mapping: PAiTextureMapping; uvindex: PLongWord; blend: PAiReal; op: PAiTextureOp; mapmode: PAiTextureMapMode; flags: PLongWord): TAiReturn; cdecl;
  aiGetLegalString: function: PAnsiChar; cdecl;
  aiGetVersionPatch: function: LongWord; cdecl;
  aiGetVersionMinor: function: LongWord; cdecl;
  aiGetVersionMajor: function: LongWord; cdecl;
  aiGetVersionRevision: function: LongWord; cdecl;
  aiGetBranchName: function: PAnsiChar; cdecl;
  aiGetCompileFlags: function: LongWord; cdecl;
  aiCreateQuaternionFromMatrix: procedure(quat: PAiQuaternion; mat: PAiMatrix3x3); cdecl;
  aiDecomposeMatrix: procedure(mat: PAiMatrix4x4; scaling: PAiVector3D; rotation: PAiQuaternion; position: PAiVector3D); cdecl;
  aiTransposeMatrix4: procedure(mat: PAiMatrix4x4); cdecl;
  aiTransposeMatrix3: procedure(mat: PAiMatrix3x3); cdecl;
  aiTransformVecByMatrix3: procedure(vec: PAiVector3D; mat: PAiMatrix3x3); cdecl;
  aiTransformVecByMatrix4: procedure(vec: PAiVector3D; mat: PAiMatrix4x4); cdecl;
  aiMultiplyMatrix4: procedure(dst: PAiMatrix4x4; src: PAiMatrix4x4); cdecl;
  aiMultiplyMatrix3: procedure(dst: PAiMatrix3x3; src: PAiMatrix3x3); cdecl;
  aiIdentityMatrix3: procedure(mat: PAiMatrix3x3); cdecl;
  aiIdentityMatrix4: procedure(mat: PAiMatrix4x4); cdecl;
  aiVector2AreEqual: function(a, b: PAiVector2D): Integer; cdecl;
  aiVector2AreEqualEpsilon: function(a, b: PAiVector2D; epsilon: Single): Integer; cdecl;
  aiVector2Add: procedure(dst: PAiVector2D; src: PAiVector2D); cdecl;
  aiVector2Subtract: procedure(dst: PAiVector2D; src: PAiVector2D); cdecl;
  aiVector2Scale: procedure(dst: PAiVector2D; s: Single); cdecl;
  aiVector2SymMul: procedure(dst: PAiVector2D; other: PAiVector2D); cdecl;
  aiVector2DivideByScalar: procedure(dst: PAiVector2D; s: Single); cdecl;
  aiVector2DivideByVector: procedure(dst: PAiVector2D; v: PAiVector2D); cdecl;
  aiVector2Length: function(v: PAiVector2D): TAiReal; cdecl;
  aiVector2SquareLength: function(v: PAiVector2D): TAiReal; cdecl;
  aiVector2Negate: procedure(dst: PAiVector2D); cdecl;
  aiVector2DotProduct: function(a, b: PAiVector2D): TAiReal; cdecl;
  aiVector2Normalize: procedure(v: PAiVector2D); cdecl;
  aiVector3AreEqual: function(a, b: PAiVector3D): Integer; cdecl;
  aiVector3AreEqualEpsilon: function(a, b: PAiVector3D; epsilon: Single): Integer; cdecl;
  aiVector3LessThan: function(a, b: PAiVector3D): Integer; cdecl;
  aiVector3Add: procedure(dst: PAiVector3D; src: PAiVector3D); cdecl;
  aiVector3Subtract: procedure(dst: PAiVector3D; src: PAiVector3D); cdecl;
  aiVector3Scale: procedure(dst: PAiVector3D; s: Single); cdecl;
  aiVector3SymMul: procedure(dst: PAiVector3D; other: PAiVector3D); cdecl;
  aiVector3DivideByScalar: procedure(dst: PAiVector3D; s: Single); cdecl;
  aiVector3DivideByVector: procedure(dst: PAiVector3D; v: PAiVector3D); cdecl;
  aiVector3Length: function(v: PAiVector3D): TAiReal; cdecl;
  aiVector3SquareLength: function(v: PAiVector3D): TAiReal; cdecl;
  aiVector3Negate: procedure(dst: PAiVector3D); cdecl;
  aiVector3DotProduct: function(a, b: PAiVector3D): TAiReal; cdecl;
  aiVector3CrossProduct: procedure(dst: PAiVector3D; a, b: PAiVector3D); cdecl;
  aiVector3Normalize: procedure(v: PAiVector3D); cdecl;
  aiVector3NormalizeSafe: procedure(v: PAiVector3D); cdecl;
  aiVector3RotateByQuaternion: procedure(v: PAiVector3D; q: PAiQuaternion); cdecl;
  aiMatrix3FromMatrix4: procedure(dst: PAiMatrix3x3; mat: PAiMatrix4x4); cdecl;
  aiMatrix3FromQuaternion: procedure(mat: PAiMatrix3x3; q: PAiQuaternion); cdecl;
  aiMatrix3AreEqual: function(a, b: PAiMatrix3x3): Integer; cdecl;
  aiMatrix3AreEqualEpsilon: function(a, b: PAiMatrix3x3; epsilon: Single): Integer; cdecl;
  aiMatrix3Inverse: procedure(mat: PAiMatrix3x3); cdecl;
  aiMatrix3Determinant: function(mat: PAiMatrix3x3): TAiReal; cdecl;
  aiMatrix3RotationZ: procedure(mat: PAiMatrix3x3; angle: Single); cdecl;
  aiMatrix3FromRotationAroundAxis: procedure(mat: PAiMatrix3x3; axis: PAiVector3D; angle: Single); cdecl;
  aiMatrix3Translation: procedure(mat: PAiMatrix3x3; translation: PAiVector2D); cdecl;
  aiMatrix3FromTo: procedure(mat: PAiMatrix3x3; from, to_: PAiVector3D); cdecl;
  aiMatrix4FromMatrix3: procedure(dst: PAiMatrix4x4; mat: PAiMatrix3x3); cdecl;
  aiMatrix4FromScalingQuaternionPosition: procedure(mat: PAiMatrix4x4; scaling: PAiVector3D; rotation: PAiQuaternion; position: PAiVector3D); cdecl;
  aiMatrix4Add: procedure(dst: PAiMatrix4x4; src: PAiMatrix4x4); cdecl;
  aiMatrix4AreEqual: function(a, b: PAiMatrix4x4): Integer; cdecl;
  aiMatrix4AreEqualEpsilon: function(a, b: PAiMatrix4x4; epsilon: Single): Integer; cdecl;
  aiMatrix4Inverse: procedure(mat: PAiMatrix4x4); cdecl;
  aiMatrix4Determinant: function(mat: PAiMatrix4x4): TAiReal; cdecl;
  aiMatrix4IsIdentity: function(mat: PAiMatrix4x4): Integer; cdecl;
  aiMatrix4DecomposeIntoScalingEulerAnglesPosition: procedure(mat: PAiMatrix4x4; scaling, rotation, position: PAiVector3D); cdecl;
  aiMatrix4DecomposeIntoScalingAxisAnglePosition: procedure(mat: PAiMatrix4x4; scaling, axis: PAiVector3D; angle: PAiReal; position: PAiVector3D); cdecl;
  aiMatrix4DecomposeNoScaling: procedure(mat: PAiMatrix4x4; rotation: PAiQuaternion; position: PAiVector3D); cdecl;
  aiMatrix4FromEulerAngles: procedure(mat: PAiMatrix4x4; x, y, z: Single); cdecl;
  aiMatrix4RotationX: procedure(mat: PAiMatrix4x4; angle: Single); cdecl;
  aiMatrix4RotationY: procedure(mat: PAiMatrix4x4; angle: Single); cdecl;
  aiMatrix4RotationZ: procedure(mat: PAiMatrix4x4; angle: Single); cdecl;
  aiMatrix4FromRotationAroundAxis: procedure(mat: PAiMatrix4x4; axis: PAiVector3D; angle: Single); cdecl;
  aiMatrix4Translation: procedure(mat: PAiMatrix4x4; translation: PAiVector3D); cdecl;
  aiMatrix4Scaling: procedure(mat: PAiMatrix4x4; scaling: PAiVector3D); cdecl;
  aiMatrix4FromTo: procedure(mat: PAiMatrix4x4; from, to_: PAiVector3D); cdecl;
  aiQuaternionFromEulerAngles: procedure(q: PAiQuaternion; x, y, z: Single); cdecl;
  aiQuaternionFromAxisAngle: procedure(q: PAiQuaternion; axis: PAiVector3D; angle: Single); cdecl;
  aiQuaternionFromNormalizedQuaternion: procedure(q: PAiQuaternion; normalized: PAiVector3D); cdecl;
  aiQuaternionAreEqual: function(a, b: PAiQuaternion): Integer; cdecl;
  aiQuaternionAreEqualEpsilon: function(a, b: PAiQuaternion; epsilon: Single): Integer; cdecl;
  aiQuaternionNormalize: procedure(q: PAiQuaternion); cdecl;
  aiQuaternionConjugate: procedure(q: PAiQuaternion); cdecl;
  aiQuaternionMultiply: procedure(dst: PAiQuaternion; q: PAiQuaternion); cdecl;
  aiQuaternionInterpolate: procedure(dst: PAiQuaternion; start, end_: PAiQuaternion; factor: Single); cdecl;

{ Read a TAiString as a string }
function AiStr(const S: TAiString): string;

{ Returns the rotation key at an index of a channel using the key size of the
  loaded library version }
function aiRotationKey(Channel: PAiNodeAnim; Index: LongWord): PAiQuatKey;

{ Returns True when animation keys of the loaded library have mInterpolation }
function aiHasKeyInterpolation: Boolean;

{ Helpers matching the aiGetMaterialFloat and aiGetMaterialInteger macros }
function aiGetMaterialFloat(pMat: PAiMaterial; pKey: PAnsiChar; type_, index: LongWord;
  out Value: TAiReal): TAiReturn;
function aiGetMaterialInteger(pMat: PAiMaterial; pKey: PAnsiChar; type_, index: LongWord;
  out Value: Integer): TAiReturn;

const
{$ifdef windows}
  libassimp = 'assimp-vc143-mt.dll';
  libassimpalt = 'libassimp-5.dll';
{$endif}
{$ifdef linux}
  libassimp = 'libassimp.so.5';
  libassimpalt = 'libassimp.so';
{$endif}
{$ifdef darwin}
  libassimp = 'libassimp.5.dylib';
  libassimpalt = 'libassimp.dylib';
{$endif}

{ Load the assimp library returning True if the required functions were found.
  It is safe to call more than once. }
function InitAssimp(ThrowExceptions: Boolean = False): Boolean;

implementation

function AiStr(const S: TAiString): string;
var
  Len: LongWord;
begin
  Len := S.length;
  if Len > AI_MAXLEN - 1 then
    Len := AI_MAXLEN - 1;
  SetString(Result, PAnsiChar(@S.data[0]), Len);
end;

function aiGetMaterialFloat(pMat: PAiMaterial; pKey: PAnsiChar; type_, index: LongWord;
  out Value: TAiReal): TAiReturn;
begin
  Value := 0;
  Result := aiGetMaterialFloatArray(pMat, pKey, type_, index, @Value, nil);
end;

function aiGetMaterialInteger(pMat: PAiMaterial; pKey: PAnsiChar; type_, index: LongWord;
  out Value: Integer): TAiReturn;
begin
  Value := 0;
  Result := aiGetMaterialIntegerArray(pMat, pKey, type_, index, @Value, nil);
end;

var
  LoadedAssimp: Boolean;
  InitializedAssimp: Boolean;
  { Before assimp 5.4 quaternion keys had no mInterpolation field }
  QuatKeySize: Integer = SizeOf(TAiQuatKey);
  KeyInterpolation: Boolean = True;

function aiRotationKey(Channel: PAiNodeAnim; Index: LongWord): PAiQuatKey;
begin
  Result := PAiQuatKey(PByte(Channel.mRotationKeys) + PtrUInt(Index) * PtrUInt(QuatKeySize));
end;

function aiHasKeyInterpolation: Boolean;
begin
  Result := KeyInterpolation;
end;

function InitAssimp(ThrowExceptions: Boolean = False): Boolean;
var
  FailedModuleName: string;
  FailedProcName: string;
  Module: HModule;

  procedure CheckExceptions;
  begin
    if (not InitializedAssimp) and ThrowExceptions then
      LibraryExceptProc(FailedModuleName, FailedProcName);
  end;

  function TryLoad(const ProcName: string; var Proc: Pointer): Boolean;
  begin
    FailedProcName := ProcName;
    Proc := LibraryGetProc(Module, ProcName);
    Result := Proc <> nil;
    if not Result then
      CheckExceptions;
  end;

  procedure LoadOptional(const ProcName: string; var Proc: Pointer);
  begin
    Proc := LibraryGetProc(Module, ProcName);
  end;

begin
  ThrowExceptions := ThrowExceptions and (@LibraryExceptProc <> nil);
  if LoadedAssimp then
  begin
    CheckExceptions;
    Exit(InitializedAssimp);
  end;
  LoadedAssimp := True;
  Result := False;
  FailedModuleName := libassimp;
  FailedProcName := '';
  Module := LibraryLoad(libassimp, libassimpalt);
  if Module = ModuleNil then
  begin
    CheckExceptions;
    Exit;
  end;
  Result :=
    TryLoad('aiImportFile', @aiImportFile) and
    TryLoad('aiImportFileEx', @aiImportFileEx) and
    TryLoad('aiImportFileExWithProperties', @aiImportFileExWithProperties) and
    TryLoad('aiImportFileFromMemory', @aiImportFileFromMemory) and
    TryLoad('aiImportFileFromMemoryWithProperties', @aiImportFileFromMemoryWithProperties) and
    TryLoad('aiApplyPostProcessing', @aiApplyPostProcessing) and
    TryLoad('aiGetPredefinedLogStream', @aiGetPredefinedLogStream) and
    TryLoad('aiAttachLogStream', @aiAttachLogStream) and
    TryLoad('aiEnableVerboseLogging', @aiEnableVerboseLogging) and
    TryLoad('aiDetachLogStream', @aiDetachLogStream) and
    TryLoad('aiDetachAllLogStreams', @aiDetachAllLogStreams) and
    TryLoad('aiReleaseImport', @aiReleaseImport) and
    TryLoad('aiGetErrorString', @aiGetErrorString) and
    TryLoad('aiIsExtensionSupported', @aiIsExtensionSupported) and
    TryLoad('aiGetExtensionList', @aiGetExtensionList) and
    TryLoad('aiGetMemoryRequirements', @aiGetMemoryRequirements) and
    TryLoad('aiCreatePropertyStore', @aiCreatePropertyStore) and
    TryLoad('aiReleasePropertyStore', @aiReleasePropertyStore) and
    TryLoad('aiSetImportPropertyInteger', @aiSetImportPropertyInteger) and
    TryLoad('aiSetImportPropertyFloat', @aiSetImportPropertyFloat) and
    TryLoad('aiSetImportPropertyString', @aiSetImportPropertyString) and
    TryLoad('aiSetImportPropertyMatrix', @aiSetImportPropertyMatrix) and
    TryLoad('aiGetImportFormatCount', @aiGetImportFormatCount) and
    TryLoad('aiGetImportFormatDescription', @aiGetImportFormatDescription) and
    TryLoad('aiGetMaterialProperty', @aiGetMaterialProperty) and
    TryLoad('aiGetMaterialFloatArray', @aiGetMaterialFloatArray) and
    TryLoad('aiGetMaterialIntegerArray', @aiGetMaterialIntegerArray) and
    TryLoad('aiGetMaterialColor', @aiGetMaterialColor) and
    TryLoad('aiGetMaterialUVTransform', @aiGetMaterialUVTransform) and
    TryLoad('aiGetMaterialString', @aiGetMaterialString) and
    TryLoad('aiGetMaterialTextureCount', @aiGetMaterialTextureCount) and
    TryLoad('aiGetMaterialTexture', @aiGetMaterialTexture) and
    TryLoad('aiGetLegalString', @aiGetLegalString) and
    TryLoad('aiGetVersionMinor', @aiGetVersionMinor) and
    TryLoad('aiGetVersionMajor', @aiGetVersionMajor) and
    TryLoad('aiGetVersionRevision', @aiGetVersionRevision) and
    TryLoad('aiGetCompileFlags', @aiGetCompileFlags);
  InitializedAssimp := Result;
  if not Result then
    Exit;
  if (aiGetVersionMajor < 5) or ((aiGetVersionMajor = 5) and (aiGetVersionMinor < 4)) then
  begin
    KeyInterpolation := False;
    QuatKeySize := (SizeOf(Double) + SizeOf(TAiQuaternion) + SizeOf(Double) - 1) and
      not (SizeOf(Double) - 1);
  end;
  LoadOptional('aiGetEmbeddedTexture', @aiGetEmbeddedTexture);
  LoadOptional('aiGetImporterDesc', @aiGetImporterDesc);
  LoadOptional('aiTextureTypeToString', @aiTextureTypeToString);
  LoadOptional('aiGetVersionPatch', @aiGetVersionPatch);
  LoadOptional('aiGetBranchName', @aiGetBranchName);
  LoadOptional('aiCreateQuaternionFromMatrix', @aiCreateQuaternionFromMatrix);
  LoadOptional('aiDecomposeMatrix', @aiDecomposeMatrix);
  LoadOptional('aiTransposeMatrix4', @aiTransposeMatrix4);
  LoadOptional('aiTransposeMatrix3', @aiTransposeMatrix3);
  LoadOptional('aiTransformVecByMatrix3', @aiTransformVecByMatrix3);
  LoadOptional('aiTransformVecByMatrix4', @aiTransformVecByMatrix4);
  LoadOptional('aiMultiplyMatrix4', @aiMultiplyMatrix4);
  LoadOptional('aiMultiplyMatrix3', @aiMultiplyMatrix3);
  LoadOptional('aiIdentityMatrix3', @aiIdentityMatrix3);
  LoadOptional('aiIdentityMatrix4', @aiIdentityMatrix4);
  LoadOptional('aiVector2AreEqual', @aiVector2AreEqual);
  LoadOptional('aiVector2AreEqualEpsilon', @aiVector2AreEqualEpsilon);
  LoadOptional('aiVector2Add', @aiVector2Add);
  LoadOptional('aiVector2Subtract', @aiVector2Subtract);
  LoadOptional('aiVector2Scale', @aiVector2Scale);
  LoadOptional('aiVector2SymMul', @aiVector2SymMul);
  LoadOptional('aiVector2DivideByScalar', @aiVector2DivideByScalar);
  LoadOptional('aiVector2DivideByVector', @aiVector2DivideByVector);
  LoadOptional('aiVector2Length', @aiVector2Length);
  LoadOptional('aiVector2SquareLength', @aiVector2SquareLength);
  LoadOptional('aiVector2Negate', @aiVector2Negate);
  LoadOptional('aiVector2DotProduct', @aiVector2DotProduct);
  LoadOptional('aiVector2Normalize', @aiVector2Normalize);
  LoadOptional('aiVector3AreEqual', @aiVector3AreEqual);
  LoadOptional('aiVector3AreEqualEpsilon', @aiVector3AreEqualEpsilon);
  LoadOptional('aiVector3LessThan', @aiVector3LessThan);
  LoadOptional('aiVector3Add', @aiVector3Add);
  LoadOptional('aiVector3Subtract', @aiVector3Subtract);
  LoadOptional('aiVector3Scale', @aiVector3Scale);
  LoadOptional('aiVector3SymMul', @aiVector3SymMul);
  LoadOptional('aiVector3DivideByScalar', @aiVector3DivideByScalar);
  LoadOptional('aiVector3DivideByVector', @aiVector3DivideByVector);
  LoadOptional('aiVector3Length', @aiVector3Length);
  LoadOptional('aiVector3SquareLength', @aiVector3SquareLength);
  LoadOptional('aiVector3Negate', @aiVector3Negate);
  LoadOptional('aiVector3DotProduct', @aiVector3DotProduct);
  LoadOptional('aiVector3CrossProduct', @aiVector3CrossProduct);
  LoadOptional('aiVector3Normalize', @aiVector3Normalize);
  LoadOptional('aiVector3NormalizeSafe', @aiVector3NormalizeSafe);
  LoadOptional('aiVector3RotateByQuaternion', @aiVector3RotateByQuaternion);
  LoadOptional('aiMatrix3FromMatrix4', @aiMatrix3FromMatrix4);
  LoadOptional('aiMatrix3FromQuaternion', @aiMatrix3FromQuaternion);
  LoadOptional('aiMatrix3AreEqual', @aiMatrix3AreEqual);
  LoadOptional('aiMatrix3AreEqualEpsilon', @aiMatrix3AreEqualEpsilon);
  LoadOptional('aiMatrix3Inverse', @aiMatrix3Inverse);
  LoadOptional('aiMatrix3Determinant', @aiMatrix3Determinant);
  LoadOptional('aiMatrix3RotationZ', @aiMatrix3RotationZ);
  LoadOptional('aiMatrix3FromRotationAroundAxis', @aiMatrix3FromRotationAroundAxis);
  LoadOptional('aiMatrix3Translation', @aiMatrix3Translation);
  LoadOptional('aiMatrix3FromTo', @aiMatrix3FromTo);
  LoadOptional('aiMatrix4FromMatrix3', @aiMatrix4FromMatrix3);
  LoadOptional('aiMatrix4FromScalingQuaternionPosition', @aiMatrix4FromScalingQuaternionPosition);
  LoadOptional('aiMatrix4Add', @aiMatrix4Add);
  LoadOptional('aiMatrix4AreEqual', @aiMatrix4AreEqual);
  LoadOptional('aiMatrix4AreEqualEpsilon', @aiMatrix4AreEqualEpsilon);
  LoadOptional('aiMatrix4Inverse', @aiMatrix4Inverse);
  LoadOptional('aiMatrix4Determinant', @aiMatrix4Determinant);
  LoadOptional('aiMatrix4IsIdentity', @aiMatrix4IsIdentity);
  LoadOptional('aiMatrix4DecomposeIntoScalingEulerAnglesPosition', @aiMatrix4DecomposeIntoScalingEulerAnglesPosition);
  LoadOptional('aiMatrix4DecomposeIntoScalingAxisAnglePosition', @aiMatrix4DecomposeIntoScalingAxisAnglePosition);
  LoadOptional('aiMatrix4DecomposeNoScaling', @aiMatrix4DecomposeNoScaling);
  LoadOptional('aiMatrix4FromEulerAngles', @aiMatrix4FromEulerAngles);
  LoadOptional('aiMatrix4RotationX', @aiMatrix4RotationX);
  LoadOptional('aiMatrix4RotationY', @aiMatrix4RotationY);
  LoadOptional('aiMatrix4RotationZ', @aiMatrix4RotationZ);
  LoadOptional('aiMatrix4FromRotationAroundAxis', @aiMatrix4FromRotationAroundAxis);
  LoadOptional('aiMatrix4Translation', @aiMatrix4Translation);
  LoadOptional('aiMatrix4Scaling', @aiMatrix4Scaling);
  LoadOptional('aiMatrix4FromTo', @aiMatrix4FromTo);
  LoadOptional('aiQuaternionFromEulerAngles', @aiQuaternionFromEulerAngles);
  LoadOptional('aiQuaternionFromAxisAngle', @aiQuaternionFromAxisAngle);
  LoadOptional('aiQuaternionFromNormalizedQuaternion', @aiQuaternionFromNormalizedQuaternion);
  LoadOptional('aiQuaternionAreEqual', @aiQuaternionAreEqual);
  LoadOptional('aiQuaternionAreEqualEpsilon', @aiQuaternionAreEqualEpsilon);
  LoadOptional('aiQuaternionNormalize', @aiQuaternionNormalize);
  LoadOptional('aiQuaternionConjugate', @aiQuaternionConjugate);
  LoadOptional('aiQuaternionMultiply', @aiQuaternionMultiply);
  LoadOptional('aiQuaternionInterpolate', @aiQuaternionInterpolate);
end;

end.
