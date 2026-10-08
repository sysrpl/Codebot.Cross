(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.opengl.txt> }
unit Codebot.OpenGL;

{ Codebot.OpenGL contains the full OpenGL 3.0 through 4.6 and OpenGL ES 2.0
  through 3.2 api, along with the context and information interfaces used to
  render. The version is selected by a define in render.inc and only the
  functions belonging to that version are declared and loaded.

  See render.inc for the list of version defines. }

{$i render.inc}

{ All gl* procedures and functions in this unit require a valid and current
  OpenGL context in order to be used }

interface

uses
  Codebot.System;

type
  GLbitfield = UInt32;
  GLboolean = Byte;
  GLbyte = Int8;
  GLchar = Char;
  GLdouble = Double;
  GLenum = UInt32;
  GLfixed = Int32;
  GLfloat = Single;
  GLhalf = UInt16;
  GLint = Int32;
  GLint64 = Int64;
  GLintptr = IntPtr;
  GLshort = Int16;
  GLsizei = Int32;
  GLsizeiptr = IntPtr;
  GLsync = Pointer;
  GLubyte = UInt8;
  GLuint = UInt32;
  GLuint64 = UInt64;
  GLushort = UInt16;

  { Pointers to the OpenGL types }
  PGLbitfield = ^GLbitfield;
  PGLboolean = ^GLboolean;
  PGLbyte = ^GLbyte;
  PGLchar = PChar;
  PGLdouble = PDouble;
  PGLenum = ^GLenum;
  PGLfixed = ^GLfixed;
  PGLfloat = PSingle;
  PGLhalf = ^GLhalf;
  PGLint = ^GLint;
  PGLint64 = ^GLint64;
  PGLintptr = ^GLintptr;
  PGLshort = ^GLshort;
  PGLsizei = ^GLsizei;
  PGLsizeiptr = ^GLsizeiptr;
  PGLsync = ^GLsync;
  PGLubyte = ^GLubyte;
  PGLuint = ^GLuint;
  PGLuint64 = ^GLuint64;
  PGLushort = ^GLushort;
  PPGLchar = ^PGLchar;

{ GLwindow represents an HWND on windows or a XWindow on linux }
  GLwindow = UIntPtr;
{ GLcontext represents an opengl context }
  GLcontext = Pointer;

{ GLDEBUGPROC is the callback used by glDebugMessageCallback }

  GLDEBUGPROC = procedure(source: GLenum; type_: GLenum; id: GLuint; severity: GLenum;
    length: GLsizei; message: PGLchar; userParam: Pointer); apicall;

{ The version of the api selected in render.inc }

const
{$if defined(gles32)}
  OpenGLMajor = 3;
  OpenGLMinor = 2;
{$elseif defined(gles31)}
  OpenGLMajor = 3;
  OpenGLMinor = 1;
{$elseif defined(gles30)}
  OpenGLMajor = 3;
  OpenGLMinor = 0;
{$elseif defined(gles20)}
  OpenGLMajor = 2;
  OpenGLMinor = 0;
{$elseif defined(gl46)}
  OpenGLMajor = 4;
  OpenGLMinor = 6;
{$elseif defined(gl45)}
  OpenGLMajor = 4;
  OpenGLMinor = 5;
{$elseif defined(gl44)}
  OpenGLMajor = 4;
  OpenGLMinor = 4;
{$elseif defined(gl43)}
  OpenGLMajor = 4;
  OpenGLMinor = 3;
{$elseif defined(gl42)}
  OpenGLMajor = 4;
  OpenGLMinor = 2;
{$elseif defined(gl41)}
  OpenGLMajor = 4;
  OpenGLMinor = 1;
{$elseif defined(gl40)}
  OpenGLMajor = 4;
  OpenGLMinor = 0;
{$elseif defined(gl33)}
  OpenGLMajor = 3;
  OpenGLMinor = 3;
{$elseif defined(gl32)}
  OpenGLMajor = 3;
  OpenGLMinor = 2;
{$elseif defined(gl31)}
  OpenGLMajor = 3;
  OpenGLMinor = 1;
{$else}
  OpenGLMajor = 3;
  OpenGLMinor = 0;
{$endif}
  { OpenGLEmbedded is True when an OpenGL ES version was selected }
  OpenGLEmbedded = {$ifdef glesapi}True{$else}False{$endif};
  { OpenGLCompatibility is True when compatibility profile functions are included }
  OpenGLCompatibility = {$ifdef glcompat}True{$else}False{$endif};
  { OpenGLApiName is the selected api in readable form such as 'OpenGL 3.3' }
  OpenGLApiName = {$ifdef glesapi}'OpenGL ES '{$else}'OpenGL '{$endif} +
    Chr(Ord('0') + OpenGLMajor) + '.' + Chr(Ord('0') + OpenGLMinor);
  { OpenGLFallback is True when contexts fall back to OpenGL 3.3 if the
    selected version is not supported, which glfallback in render.inc turns on }
  OpenGLFallback = {$ifdef glfallback}True{$else}False{$endif};
  OpenGLFallbackMajor = 3;
  OpenGLFallbackMinor = 3;

{ OpenGLContextMajor and OpenGLContextMinor are the version contexts are
  created with. They begin as the selected version and are changed to the
  fallback version by the platform unit if the selected version is not
  supported. Functions belonging to versions above the context version are
  then nil and must not be called. }

var
  OpenGLContextMajor: Integer = OpenGLMajor;
  OpenGLContextMinor: Integer = OpenGLMinor;

{$ifdef glesapi}
{ OpenGL ES 2.0 }

{$region gles20}
const
  GL_DEPTH_BUFFER_BIT = $00000100;
  GL_STENCIL_BUFFER_BIT = $00000400;
  GL_COLOR_BUFFER_BIT = $00004000;
  GL_FALSE = 0;
  GL_TRUE = 1;
  GL_POINTS = $0000;
  GL_LINES = $0001;
  GL_LINE_LOOP = $0002;
  GL_LINE_STRIP = $0003;
  GL_TRIANGLES = $0004;
  GL_TRIANGLE_STRIP = $0005;
  GL_TRIANGLE_FAN = $0006;
  GL_ZERO = 0;
  GL_ONE = 1;
  GL_SRC_COLOR = $0300;
  GL_ONE_MINUS_SRC_COLOR = $0301;
  GL_SRC_ALPHA = $0302;
  GL_ONE_MINUS_SRC_ALPHA = $0303;
  GL_DST_ALPHA = $0304;
  GL_ONE_MINUS_DST_ALPHA = $0305;
  GL_DST_COLOR = $0306;
  GL_ONE_MINUS_DST_COLOR = $0307;
  GL_SRC_ALPHA_SATURATE = $0308;
  GL_FUNC_ADD = $8006;
  GL_BLEND_EQUATION = $8009;
  GL_BLEND_EQUATION_RGB = $8009;
  GL_BLEND_EQUATION_ALPHA = $883D;
  GL_FUNC_SUBTRACT = $800A;
  GL_FUNC_REVERSE_SUBTRACT = $800B;
  GL_BLEND_DST_RGB = $80C8;
  GL_BLEND_SRC_RGB = $80C9;
  GL_BLEND_DST_ALPHA = $80CA;
  GL_BLEND_SRC_ALPHA = $80CB;
  GL_CONSTANT_COLOR = $8001;
  GL_ONE_MINUS_CONSTANT_COLOR = $8002;
  GL_CONSTANT_ALPHA = $8003;
  GL_ONE_MINUS_CONSTANT_ALPHA = $8004;
  GL_BLEND_COLOR = $8005;
  GL_ARRAY_BUFFER = $8892;
  GL_ELEMENT_ARRAY_BUFFER = $8893;
  GL_ARRAY_BUFFER_BINDING = $8894;
  GL_ELEMENT_ARRAY_BUFFER_BINDING = $8895;
  GL_STREAM_DRAW = $88E0;
  GL_STATIC_DRAW = $88E4;
  GL_DYNAMIC_DRAW = $88E8;
  GL_BUFFER_SIZE = $8764;
  GL_BUFFER_USAGE = $8765;
  GL_CURRENT_VERTEX_ATTRIB = $8626;
  GL_FRONT = $0404;
  GL_BACK = $0405;
  GL_FRONT_AND_BACK = $0408;
  GL_TEXTURE_2D = $0DE1;
  GL_CULL_FACE = $0B44;
  GL_BLEND = $0BE2;
  GL_DITHER = $0BD0;
  GL_STENCIL_TEST = $0B90;
  GL_DEPTH_TEST = $0B71;
  GL_SCISSOR_TEST = $0C11;
  GL_POLYGON_OFFSET_FILL = $8037;
  GL_SAMPLE_ALPHA_TO_COVERAGE = $809E;
  GL_SAMPLE_COVERAGE = $80A0;
  GL_NO_ERROR = 0;
  GL_INVALID_ENUM = $0500;
  GL_INVALID_VALUE = $0501;
  GL_INVALID_OPERATION = $0502;
  GL_OUT_OF_MEMORY = $0505;
  GL_CW = $0900;
  GL_CCW = $0901;
  GL_LINE_WIDTH = $0B21;
  GL_ALIASED_POINT_SIZE_RANGE = $846D;
  GL_ALIASED_LINE_WIDTH_RANGE = $846E;
  GL_CULL_FACE_MODE = $0B45;
  GL_FRONT_FACE = $0B46;
  GL_DEPTH_RANGE = $0B70;
  GL_DEPTH_WRITEMASK = $0B72;
  GL_DEPTH_CLEAR_VALUE = $0B73;
  GL_DEPTH_FUNC = $0B74;
  GL_STENCIL_CLEAR_VALUE = $0B91;
  GL_STENCIL_FUNC = $0B92;
  GL_STENCIL_FAIL = $0B94;
  GL_STENCIL_PASS_DEPTH_FAIL = $0B95;
  GL_STENCIL_PASS_DEPTH_PASS = $0B96;
  GL_STENCIL_REF = $0B97;
  GL_STENCIL_VALUE_MASK = $0B93;
  GL_STENCIL_WRITEMASK = $0B98;
  GL_STENCIL_BACK_FUNC = $8800;
  GL_STENCIL_BACK_FAIL = $8801;
  GL_STENCIL_BACK_PASS_DEPTH_FAIL = $8802;
  GL_STENCIL_BACK_PASS_DEPTH_PASS = $8803;
  GL_STENCIL_BACK_REF = $8CA3;
  GL_STENCIL_BACK_VALUE_MASK = $8CA4;
  GL_STENCIL_BACK_WRITEMASK = $8CA5;
  GL_VIEWPORT = $0BA2;
  GL_SCISSOR_BOX = $0C10;
  GL_COLOR_CLEAR_VALUE = $0C22;
  GL_COLOR_WRITEMASK = $0C23;
  GL_UNPACK_ALIGNMENT = $0CF5;
  GL_PACK_ALIGNMENT = $0D05;
  GL_MAX_TEXTURE_SIZE = $0D33;
  GL_MAX_VIEWPORT_DIMS = $0D3A;
  GL_SUBPIXEL_BITS = $0D50;
  GL_RED_BITS = $0D52;
  GL_GREEN_BITS = $0D53;
  GL_BLUE_BITS = $0D54;
  GL_ALPHA_BITS = $0D55;
  GL_DEPTH_BITS = $0D56;
  GL_STENCIL_BITS = $0D57;
  GL_POLYGON_OFFSET_UNITS = $2A00;
  GL_POLYGON_OFFSET_FACTOR = $8038;
  GL_TEXTURE_BINDING_2D = $8069;
  GL_SAMPLE_BUFFERS = $80A8;
  GL_SAMPLES = $80A9;
  GL_SAMPLE_COVERAGE_VALUE = $80AA;
  GL_SAMPLE_COVERAGE_INVERT = $80AB;
  GL_NUM_COMPRESSED_TEXTURE_FORMATS = $86A2;
  GL_COMPRESSED_TEXTURE_FORMATS = $86A3;
  GL_DONT_CARE = $1100;
  GL_FASTEST = $1101;
  GL_NICEST = $1102;
  GL_GENERATE_MIPMAP_HINT = $8192;
  GL_BYTE = $1400;
  GL_UNSIGNED_BYTE = $1401;
  GL_SHORT = $1402;
  GL_UNSIGNED_SHORT = $1403;
  GL_INT = $1404;
  GL_UNSIGNED_INT = $1405;
  GL_FLOAT = $1406;
  GL_FIXED = $140C;
  GL_DEPTH_COMPONENT = $1902;
  GL_ALPHA = $1906;
  GL_RGB = $1907;
  GL_RGBA = $1908;
  GL_LUMINANCE = $1909;
  GL_LUMINANCE_ALPHA = $190A;
  GL_UNSIGNED_SHORT_4_4_4_4 = $8033;
  GL_UNSIGNED_SHORT_5_5_5_1 = $8034;
  GL_UNSIGNED_SHORT_5_6_5 = $8363;
  GL_FRAGMENT_SHADER = $8B30;
  GL_VERTEX_SHADER = $8B31;
  GL_MAX_VERTEX_ATTRIBS = $8869;
  GL_MAX_VERTEX_UNIFORM_VECTORS = $8DFB;
  GL_MAX_VARYING_VECTORS = $8DFC;
  GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS = $8B4D;
  GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS = $8B4C;
  GL_MAX_TEXTURE_IMAGE_UNITS = $8872;
  GL_MAX_FRAGMENT_UNIFORM_VECTORS = $8DFD;
  GL_SHADER_TYPE = $8B4F;
  GL_DELETE_STATUS = $8B80;
  GL_LINK_STATUS = $8B82;
  GL_VALIDATE_STATUS = $8B83;
  GL_ATTACHED_SHADERS = $8B85;
  GL_ACTIVE_UNIFORMS = $8B86;
  GL_ACTIVE_UNIFORM_MAX_LENGTH = $8B87;
  GL_ACTIVE_ATTRIBUTES = $8B89;
  GL_ACTIVE_ATTRIBUTE_MAX_LENGTH = $8B8A;
  GL_SHADING_LANGUAGE_VERSION = $8B8C;
  GL_CURRENT_PROGRAM = $8B8D;
  GL_NEVER = $0200;
  GL_LESS = $0201;
  GL_EQUAL = $0202;
  GL_LEQUAL = $0203;
  GL_GREATER = $0204;
  GL_NOTEQUAL = $0205;
  GL_GEQUAL = $0206;
  GL_ALWAYS = $0207;
  GL_KEEP = $1E00;
  GL_REPLACE = $1E01;
  GL_INCR = $1E02;
  GL_DECR = $1E03;
  GL_INVERT = $150A;
  GL_INCR_WRAP = $8507;
  GL_DECR_WRAP = $8508;
  GL_VENDOR = $1F00;
  GL_RENDERER = $1F01;
  GL_VERSION = $1F02;
  GL_EXTENSIONS = $1F03;
  GL_NEAREST = $2600;
  GL_LINEAR = $2601;
  GL_NEAREST_MIPMAP_NEAREST = $2700;
  GL_LINEAR_MIPMAP_NEAREST = $2701;
  GL_NEAREST_MIPMAP_LINEAR = $2702;
  GL_LINEAR_MIPMAP_LINEAR = $2703;
  GL_TEXTURE_MAG_FILTER = $2800;
  GL_TEXTURE_MIN_FILTER = $2801;
  GL_TEXTURE_WRAP_S = $2802;
  GL_TEXTURE_WRAP_T = $2803;
  GL_TEXTURE = $1702;
  GL_TEXTURE_CUBE_MAP = $8513;
  GL_TEXTURE_BINDING_CUBE_MAP = $8514;
  GL_TEXTURE_CUBE_MAP_POSITIVE_X = $8515;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_X = $8516;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Y = $8517;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Y = $8518;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Z = $8519;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Z = $851A;
  GL_MAX_CUBE_MAP_TEXTURE_SIZE = $851C;
  GL_TEXTURE0 = $84C0;
  GL_TEXTURE1 = $84C1;
  GL_TEXTURE2 = $84C2;
  GL_TEXTURE3 = $84C3;
  GL_TEXTURE4 = $84C4;
  GL_TEXTURE5 = $84C5;
  GL_TEXTURE6 = $84C6;
  GL_TEXTURE7 = $84C7;
  GL_TEXTURE8 = $84C8;
  GL_TEXTURE9 = $84C9;
  GL_TEXTURE10 = $84CA;
  GL_TEXTURE11 = $84CB;
  GL_TEXTURE12 = $84CC;
  GL_TEXTURE13 = $84CD;
  GL_TEXTURE14 = $84CE;
  GL_TEXTURE15 = $84CF;
  GL_TEXTURE16 = $84D0;
  GL_TEXTURE17 = $84D1;
  GL_TEXTURE18 = $84D2;
  GL_TEXTURE19 = $84D3;
  GL_TEXTURE20 = $84D4;
  GL_TEXTURE21 = $84D5;
  GL_TEXTURE22 = $84D6;
  GL_TEXTURE23 = $84D7;
  GL_TEXTURE24 = $84D8;
  GL_TEXTURE25 = $84D9;
  GL_TEXTURE26 = $84DA;
  GL_TEXTURE27 = $84DB;
  GL_TEXTURE28 = $84DC;
  GL_TEXTURE29 = $84DD;
  GL_TEXTURE30 = $84DE;
  GL_TEXTURE31 = $84DF;
  GL_ACTIVE_TEXTURE = $84E0;
  GL_REPEAT = $2901;
  GL_CLAMP_TO_EDGE = $812F;
  GL_MIRRORED_REPEAT = $8370;
  GL_FLOAT_VEC2 = $8B50;
  GL_FLOAT_VEC3 = $8B51;
  GL_FLOAT_VEC4 = $8B52;
  GL_INT_VEC2 = $8B53;
  GL_INT_VEC3 = $8B54;
  GL_INT_VEC4 = $8B55;
  GL_BOOL = $8B56;
  GL_BOOL_VEC2 = $8B57;
  GL_BOOL_VEC3 = $8B58;
  GL_BOOL_VEC4 = $8B59;
  GL_FLOAT_MAT2 = $8B5A;
  GL_FLOAT_MAT3 = $8B5B;
  GL_FLOAT_MAT4 = $8B5C;
  GL_SAMPLER_2D = $8B5E;
  GL_SAMPLER_CUBE = $8B60;
  GL_VERTEX_ATTRIB_ARRAY_ENABLED = $8622;
  GL_VERTEX_ATTRIB_ARRAY_SIZE = $8623;
  GL_VERTEX_ATTRIB_ARRAY_STRIDE = $8624;
  GL_VERTEX_ATTRIB_ARRAY_TYPE = $8625;
  GL_VERTEX_ATTRIB_ARRAY_NORMALIZED = $886A;
  GL_VERTEX_ATTRIB_ARRAY_POINTER = $8645;
  GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING = $889F;
  GL_IMPLEMENTATION_COLOR_READ_TYPE = $8B9A;
  GL_IMPLEMENTATION_COLOR_READ_FORMAT = $8B9B;
  GL_COMPILE_STATUS = $8B81;
  GL_INFO_LOG_LENGTH = $8B84;
  GL_SHADER_SOURCE_LENGTH = $8B88;
  GL_SHADER_COMPILER = $8DFA;
  GL_SHADER_BINARY_FORMATS = $8DF8;
  GL_NUM_SHADER_BINARY_FORMATS = $8DF9;
  GL_LOW_FLOAT = $8DF0;
  GL_MEDIUM_FLOAT = $8DF1;
  GL_HIGH_FLOAT = $8DF2;
  GL_LOW_INT = $8DF3;
  GL_MEDIUM_INT = $8DF4;
  GL_HIGH_INT = $8DF5;
  GL_FRAMEBUFFER = $8D40;
  GL_RENDERBUFFER = $8D41;
  GL_RGBA4 = $8056;
  GL_RGB5_A1 = $8057;
  GL_RGB565 = $8D62;
  GL_DEPTH_COMPONENT16 = $81A5;
  GL_STENCIL_INDEX8 = $8D48;
  GL_RENDERBUFFER_WIDTH = $8D42;
  GL_RENDERBUFFER_HEIGHT = $8D43;
  GL_RENDERBUFFER_INTERNAL_FORMAT = $8D44;
  GL_RENDERBUFFER_RED_SIZE = $8D50;
  GL_RENDERBUFFER_GREEN_SIZE = $8D51;
  GL_RENDERBUFFER_BLUE_SIZE = $8D52;
  GL_RENDERBUFFER_ALPHA_SIZE = $8D53;
  GL_RENDERBUFFER_DEPTH_SIZE = $8D54;
  GL_RENDERBUFFER_STENCIL_SIZE = $8D55;
  GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE = $8CD0;
  GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME = $8CD1;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL = $8CD2;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE = $8CD3;
  GL_COLOR_ATTACHMENT0 = $8CE0;
  GL_DEPTH_ATTACHMENT = $8D00;
  GL_STENCIL_ATTACHMENT = $8D20;
  GL_NONE = 0;
  GL_FRAMEBUFFER_COMPLETE = $8CD5;
  GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT = $8CD6;
  GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT = $8CD7;
  GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS = $8CD9;
  GL_FRAMEBUFFER_UNSUPPORTED = $8CDD;
  GL_FRAMEBUFFER_BINDING = $8CA6;
  GL_RENDERBUFFER_BINDING = $8CA7;
  GL_MAX_RENDERBUFFER_SIZE = $84E8;
  GL_INVALID_FRAMEBUFFER_OPERATION = $0506;

var
  glActiveTexture: procedure(texture: GLenum); apicall;
  glAttachShader: procedure(program_: GLuint; shader: GLuint); apicall;
  glBindAttribLocation: procedure(program_: GLuint; index: GLuint; name: PGLchar); apicall;
  glBindBuffer: procedure(target: GLenum; buffer: GLuint); apicall;
  glBindFramebuffer: procedure(target: GLenum; framebuffer: GLuint); apicall;
  glBindRenderbuffer: procedure(target: GLenum; renderbuffer: GLuint); apicall;
  glBindTexture: procedure(target: GLenum; texture: GLuint); apicall;
  glBlendColor: procedure(red: GLfloat; green: GLfloat; blue: GLfloat; alpha: GLfloat); apicall;
  glBlendEquation: procedure(mode: GLenum); apicall;
  glBlendEquationSeparate: procedure(modeRGB: GLenum; modeAlpha: GLenum); apicall;
  glBlendFunc: procedure(sfactor: GLenum; dfactor: GLenum); apicall;
  glBlendFuncSeparate: procedure(sfactorRGB: GLenum; dfactorRGB: GLenum; sfactorAlpha: GLenum; dfactorAlpha: GLenum); apicall;
  glBufferData: procedure(target: GLenum; size: GLsizeiptr; data: Pointer; usage: GLenum); apicall;
  glBufferSubData: procedure(target: GLenum; offset: GLintptr; size: GLsizeiptr; data: Pointer); apicall;
  glCheckFramebufferStatus: function(target: GLenum): GLenum; apicall;
  glClear: procedure(mask: GLbitfield); apicall;
  glClearColor: procedure(red: GLfloat; green: GLfloat; blue: GLfloat; alpha: GLfloat); apicall;
  glClearDepthf: procedure(d: GLfloat); apicall;
  glClearStencil: procedure(s: GLint); apicall;
  glColorMask: procedure(red: GLboolean; green: GLboolean; blue: GLboolean; alpha: GLboolean); apicall;
  glCompileShader: procedure(shader: GLuint); apicall;
  glCompressedTexImage2D: procedure(target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; height: GLsizei; border: GLint; imageSize: GLsizei; data: Pointer); apicall;
  glCompressedTexSubImage2D: procedure(target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; width: GLsizei; height: GLsizei; format: GLenum; imageSize: GLsizei; data: Pointer); apicall;
  glCopyTexImage2D: procedure(target: GLenum; level: GLint; internalformat: GLenum; x: GLint; y: GLint; width: GLsizei; height: GLsizei; border: GLint); apicall;
  glCopyTexSubImage2D: procedure(target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; x: GLint; y: GLint; width: GLsizei; height: GLsizei); apicall;
  glCreateProgram: function: GLuint; apicall;
  glCreateShader: function(type_: GLenum): GLuint; apicall;
  glCullFace: procedure(mode: GLenum); apicall;
  glDeleteBuffers: procedure(n: GLsizei; buffers: PGLuint); apicall;
  glDeleteFramebuffers: procedure(n: GLsizei; framebuffers: PGLuint); apicall;
  glDeleteProgram: procedure(program_: GLuint); apicall;
  glDeleteRenderbuffers: procedure(n: GLsizei; renderbuffers: PGLuint); apicall;
  glDeleteShader: procedure(shader: GLuint); apicall;
  glDeleteTextures: procedure(n: GLsizei; textures: PGLuint); apicall;
  glDepthFunc: procedure(func: GLenum); apicall;
  glDepthMask: procedure(flag: GLboolean); apicall;
  glDepthRangef: procedure(n: GLfloat; f: GLfloat); apicall;
  glDetachShader: procedure(program_: GLuint; shader: GLuint); apicall;
  glDisable: procedure(cap: GLenum); apicall;
  glDisableVertexAttribArray: procedure(index: GLuint); apicall;
  glDrawArrays: procedure(mode: GLenum; first: GLint; count: GLsizei); apicall;
  glDrawElements: procedure(mode: GLenum; count: GLsizei; type_: GLenum; indices: Pointer); apicall;
  glEnable: procedure(cap: GLenum); apicall;
  glEnableVertexAttribArray: procedure(index: GLuint); apicall;
  glFinish: procedure; apicall;
  glFlush: procedure; apicall;
  glFramebufferRenderbuffer: procedure(target: GLenum; attachment: GLenum; renderbuffertarget: GLenum; renderbuffer: GLuint); apicall;
  glFramebufferTexture2D: procedure(target: GLenum; attachment: GLenum; textarget: GLenum; texture: GLuint; level: GLint); apicall;
  glFrontFace: procedure(mode: GLenum); apicall;
  glGenBuffers: procedure(n: GLsizei; buffers: PGLuint); apicall;
  glGenerateMipmap: procedure(target: GLenum); apicall;
  glGenFramebuffers: procedure(n: GLsizei; framebuffers: PGLuint); apicall;
  glGenRenderbuffers: procedure(n: GLsizei; renderbuffers: PGLuint); apicall;
  glGenTextures: procedure(n: GLsizei; textures: PGLuint); apicall;
  glGetActiveAttrib: procedure(program_: GLuint; index: GLuint; bufSize: GLsizei; length: PGLsizei; size: PGLint; type_: PGLenum; name: PGLchar); apicall;
  glGetActiveUniform: procedure(program_: GLuint; index: GLuint; bufSize: GLsizei; length: PGLsizei; size: PGLint; type_: PGLenum; name: PGLchar); apicall;
  glGetAttachedShaders: procedure(program_: GLuint; maxCount: GLsizei; count: PGLsizei; shaders: PGLuint); apicall;
  glGetAttribLocation: function(program_: GLuint; name: PGLchar): GLint; apicall;
  glGetBooleanv: procedure(pname: GLenum; data: PGLboolean); apicall;
  glGetBufferParameteriv: procedure(target: GLenum; pname: GLenum; params: PGLint); apicall;
  glGetError: function: GLenum; apicall;
  glGetFloatv: procedure(pname: GLenum; data: PGLfloat); apicall;
  glGetFramebufferAttachmentParameteriv: procedure(target: GLenum; attachment: GLenum; pname: GLenum; params: PGLint); apicall;
  glGetIntegerv: procedure(pname: GLenum; data: PGLint); apicall;
  glGetProgramiv: procedure(program_: GLuint; pname: GLenum; params: PGLint); apicall;
  glGetProgramInfoLog: procedure(program_: GLuint; bufSize: GLsizei; length: PGLsizei; infoLog: PGLchar); apicall;
  glGetRenderbufferParameteriv: procedure(target: GLenum; pname: GLenum; params: PGLint); apicall;
  glGetShaderiv: procedure(shader: GLuint; pname: GLenum; params: PGLint); apicall;
  glGetShaderInfoLog: procedure(shader: GLuint; bufSize: GLsizei; length: PGLsizei; infoLog: PGLchar); apicall;
  glGetShaderPrecisionFormat: procedure(shadertype: GLenum; precisiontype: GLenum; range: PGLint; precision: PGLint); apicall;
  glGetShaderSource: procedure(shader: GLuint; bufSize: GLsizei; length: PGLsizei; source: PGLchar); apicall;
  glGetString: function(name: GLenum): PGLubyte; apicall;
  glGetTexParameterfv: procedure(target: GLenum; pname: GLenum; params: PGLfloat); apicall;
  glGetTexParameteriv: procedure(target: GLenum; pname: GLenum; params: PGLint); apicall;
  glGetUniformfv: procedure(program_: GLuint; location: GLint; params: PGLfloat); apicall;
  glGetUniformiv: procedure(program_: GLuint; location: GLint; params: PGLint); apicall;
  glGetUniformLocation: function(program_: GLuint; name: PGLchar): GLint; apicall;
  glGetVertexAttribfv: procedure(index: GLuint; pname: GLenum; params: PGLfloat); apicall;
  glGetVertexAttribiv: procedure(index: GLuint; pname: GLenum; params: PGLint); apicall;
  glGetVertexAttribPointerv: procedure(index: GLuint; pname: GLenum; pointer: PPointer); apicall;
  glHint: procedure(target: GLenum; mode: GLenum); apicall;
  glIsBuffer: function(buffer: GLuint): GLboolean; apicall;
  glIsEnabled: function(cap: GLenum): GLboolean; apicall;
  glIsFramebuffer: function(framebuffer: GLuint): GLboolean; apicall;
  glIsProgram: function(program_: GLuint): GLboolean; apicall;
  glIsRenderbuffer: function(renderbuffer: GLuint): GLboolean; apicall;
  glIsShader: function(shader: GLuint): GLboolean; apicall;
  glIsTexture: function(texture: GLuint): GLboolean; apicall;
  glLineWidth: procedure(width: GLfloat); apicall;
  glLinkProgram: procedure(program_: GLuint); apicall;
  glPixelStorei: procedure(pname: GLenum; param: GLint); apicall;
  glPolygonOffset: procedure(factor: GLfloat; units: GLfloat); apicall;
  glReadPixels: procedure(x: GLint; y: GLint; width: GLsizei; height: GLsizei; format: GLenum; type_: GLenum; pixels: Pointer); apicall;
  glReleaseShaderCompiler: procedure; apicall;
  glRenderbufferStorage: procedure(target: GLenum; internalformat: GLenum; width: GLsizei; height: GLsizei); apicall;
  glSampleCoverage: procedure(value: GLfloat; invert: GLboolean); apicall;
  glScissor: procedure(x: GLint; y: GLint; width: GLsizei; height: GLsizei); apicall;
  glShaderBinary: procedure(count: GLsizei; shaders: PGLuint; binaryFormat: GLenum; binary: Pointer; length: GLsizei); apicall;
  glShaderSource: procedure(shader: GLuint; count: GLsizei; string_: PPGLchar; length: PGLint); apicall;
  glStencilFunc: procedure(func: GLenum; ref: GLint; mask: GLuint); apicall;
  glStencilFuncSeparate: procedure(face: GLenum; func: GLenum; ref: GLint; mask: GLuint); apicall;
  glStencilMask: procedure(mask: GLuint); apicall;
  glStencilMaskSeparate: procedure(face: GLenum; mask: GLuint); apicall;
  glStencilOp: procedure(fail: GLenum; zfail: GLenum; zpass: GLenum); apicall;
  glStencilOpSeparate: procedure(face: GLenum; sfail: GLenum; dpfail: GLenum; dppass: GLenum); apicall;
  glTexImage2D: procedure(target: GLenum; level: GLint; internalformat: GLint; width: GLsizei; height: GLsizei; border: GLint; format: GLenum; type_: GLenum; pixels: Pointer); apicall;
  glTexParameterf: procedure(target: GLenum; pname: GLenum; param: GLfloat); apicall;
  glTexParameterfv: procedure(target: GLenum; pname: GLenum; params: PGLfloat); apicall;
  glTexParameteri: procedure(target: GLenum; pname: GLenum; param: GLint); apicall;
  glTexParameteriv: procedure(target: GLenum; pname: GLenum; params: PGLint); apicall;
  glTexSubImage2D: procedure(target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; width: GLsizei; height: GLsizei; format: GLenum; type_: GLenum; pixels: Pointer); apicall;
  glUniform1f: procedure(location: GLint; v0: GLfloat); apicall;
  glUniform1fv: procedure(location: GLint; count: GLsizei; value: PGLfloat); apicall;
  glUniform1i: procedure(location: GLint; v0: GLint); apicall;
  glUniform1iv: procedure(location: GLint; count: GLsizei; value: PGLint); apicall;
  glUniform2f: procedure(location: GLint; v0: GLfloat; v1: GLfloat); apicall;
  glUniform2fv: procedure(location: GLint; count: GLsizei; value: PGLfloat); apicall;
  glUniform2i: procedure(location: GLint; v0: GLint; v1: GLint); apicall;
  glUniform2iv: procedure(location: GLint; count: GLsizei; value: PGLint); apicall;
  glUniform3f: procedure(location: GLint; v0: GLfloat; v1: GLfloat; v2: GLfloat); apicall;
  glUniform3fv: procedure(location: GLint; count: GLsizei; value: PGLfloat); apicall;
  glUniform3i: procedure(location: GLint; v0: GLint; v1: GLint; v2: GLint); apicall;
  glUniform3iv: procedure(location: GLint; count: GLsizei; value: PGLint); apicall;
  glUniform4f: procedure(location: GLint; v0: GLfloat; v1: GLfloat; v2: GLfloat; v3: GLfloat); apicall;
  glUniform4fv: procedure(location: GLint; count: GLsizei; value: PGLfloat); apicall;
  glUniform4i: procedure(location: GLint; v0: GLint; v1: GLint; v2: GLint; v3: GLint); apicall;
  glUniform4iv: procedure(location: GLint; count: GLsizei; value: PGLint); apicall;
  glUniformMatrix2fv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); apicall;
  glUniformMatrix3fv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); apicall;
  glUniformMatrix4fv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); apicall;
  glUseProgram: procedure(program_: GLuint); apicall;
  glValidateProgram: procedure(program_: GLuint); apicall;
  glVertexAttrib1f: procedure(index: GLuint; x: GLfloat); apicall;
  glVertexAttrib1fv: procedure(index: GLuint; v: PGLfloat); apicall;
  glVertexAttrib2f: procedure(index: GLuint; x: GLfloat; y: GLfloat); apicall;
  glVertexAttrib2fv: procedure(index: GLuint; v: PGLfloat); apicall;
  glVertexAttrib3f: procedure(index: GLuint; x: GLfloat; y: GLfloat; z: GLfloat); apicall;
  glVertexAttrib3fv: procedure(index: GLuint; v: PGLfloat); apicall;
  glVertexAttrib4f: procedure(index: GLuint; x: GLfloat; y: GLfloat; z: GLfloat; w: GLfloat); apicall;
  glVertexAttrib4fv: procedure(index: GLuint; v: PGLfloat); apicall;
  glVertexAttribPointer: procedure(index: GLuint; size: GLint; type_: GLenum; normalized: GLboolean; stride: GLsizei; pointer: Pointer); apicall;
  glViewport: procedure(x: GLint; y: GLint; width: GLsizei; height: GLsizei); apicall;
{$endregion}

{ OpenGL ES 3.0 }

{$region gles30}
{$ifdef gles30}
const
  GL_READ_BUFFER = $0C02;
  GL_UNPACK_ROW_LENGTH = $0CF2;
  GL_UNPACK_SKIP_ROWS = $0CF3;
  GL_UNPACK_SKIP_PIXELS = $0CF4;
  GL_PACK_ROW_LENGTH = $0D02;
  GL_PACK_SKIP_ROWS = $0D03;
  GL_PACK_SKIP_PIXELS = $0D04;
  GL_COLOR = $1800;
  GL_DEPTH = $1801;
  GL_STENCIL = $1802;
  GL_RED = $1903;
  GL_RGB8 = $8051;
  GL_RGBA8 = $8058;
  GL_RGB10_A2 = $8059;
  GL_TEXTURE_BINDING_3D = $806A;
  GL_UNPACK_SKIP_IMAGES = $806D;
  GL_UNPACK_IMAGE_HEIGHT = $806E;
  GL_TEXTURE_3D = $806F;
  GL_TEXTURE_WRAP_R = $8072;
  GL_MAX_3D_TEXTURE_SIZE = $8073;
  GL_UNSIGNED_INT_2_10_10_10_REV = $8368;
  GL_MAX_ELEMENTS_VERTICES = $80E8;
  GL_MAX_ELEMENTS_INDICES = $80E9;
  GL_TEXTURE_MIN_LOD = $813A;
  GL_TEXTURE_MAX_LOD = $813B;
  GL_TEXTURE_BASE_LEVEL = $813C;
  GL_TEXTURE_MAX_LEVEL = $813D;
  GL_MIN = $8007;
  GL_MAX = $8008;
  GL_DEPTH_COMPONENT24 = $81A6;
  GL_MAX_TEXTURE_LOD_BIAS = $84FD;
  GL_TEXTURE_COMPARE_MODE = $884C;
  GL_TEXTURE_COMPARE_FUNC = $884D;
  GL_CURRENT_QUERY = $8865;
  GL_QUERY_RESULT = $8866;
  GL_QUERY_RESULT_AVAILABLE = $8867;
  GL_BUFFER_MAPPED = $88BC;
  GL_BUFFER_MAP_POINTER = $88BD;
  GL_STREAM_READ = $88E1;
  GL_STREAM_COPY = $88E2;
  GL_STATIC_READ = $88E5;
  GL_STATIC_COPY = $88E6;
  GL_DYNAMIC_READ = $88E9;
  GL_DYNAMIC_COPY = $88EA;
  GL_MAX_DRAW_BUFFERS = $8824;
  GL_DRAW_BUFFER0 = $8825;
  GL_DRAW_BUFFER1 = $8826;
  GL_DRAW_BUFFER2 = $8827;
  GL_DRAW_BUFFER3 = $8828;
  GL_DRAW_BUFFER4 = $8829;
  GL_DRAW_BUFFER5 = $882A;
  GL_DRAW_BUFFER6 = $882B;
  GL_DRAW_BUFFER7 = $882C;
  GL_DRAW_BUFFER8 = $882D;
  GL_DRAW_BUFFER9 = $882E;
  GL_DRAW_BUFFER10 = $882F;
  GL_DRAW_BUFFER11 = $8830;
  GL_DRAW_BUFFER12 = $8831;
  GL_DRAW_BUFFER13 = $8832;
  GL_DRAW_BUFFER14 = $8833;
  GL_DRAW_BUFFER15 = $8834;
  GL_MAX_FRAGMENT_UNIFORM_COMPONENTS = $8B49;
  GL_MAX_VERTEX_UNIFORM_COMPONENTS = $8B4A;
  GL_SAMPLER_3D = $8B5F;
  GL_SAMPLER_2D_SHADOW = $8B62;
  GL_FRAGMENT_SHADER_DERIVATIVE_HINT = $8B8B;
  GL_PIXEL_PACK_BUFFER = $88EB;
  GL_PIXEL_UNPACK_BUFFER = $88EC;
  GL_PIXEL_PACK_BUFFER_BINDING = $88ED;
  GL_PIXEL_UNPACK_BUFFER_BINDING = $88EF;
  GL_FLOAT_MAT2x3 = $8B65;
  GL_FLOAT_MAT2x4 = $8B66;
  GL_FLOAT_MAT3x2 = $8B67;
  GL_FLOAT_MAT3x4 = $8B68;
  GL_FLOAT_MAT4x2 = $8B69;
  GL_FLOAT_MAT4x3 = $8B6A;
  GL_SRGB = $8C40;
  GL_SRGB8 = $8C41;
  GL_SRGB8_ALPHA8 = $8C43;
  GL_COMPARE_REF_TO_TEXTURE = $884E;
  GL_MAJOR_VERSION = $821B;
  GL_MINOR_VERSION = $821C;
  GL_NUM_EXTENSIONS = $821D;
  GL_RGBA32F = $8814;
  GL_RGB32F = $8815;
  GL_RGBA16F = $881A;
  GL_RGB16F = $881B;
  GL_VERTEX_ATTRIB_ARRAY_INTEGER = $88FD;
  GL_MAX_ARRAY_TEXTURE_LAYERS = $88FF;
  GL_MIN_PROGRAM_TEXEL_OFFSET = $8904;
  GL_MAX_PROGRAM_TEXEL_OFFSET = $8905;
  GL_MAX_VARYING_COMPONENTS = $8B4B;
  GL_TEXTURE_2D_ARRAY = $8C1A;
  GL_TEXTURE_BINDING_2D_ARRAY = $8C1D;
  GL_R11F_G11F_B10F = $8C3A;
  GL_UNSIGNED_INT_10F_11F_11F_REV = $8C3B;
  GL_RGB9_E5 = $8C3D;
  GL_UNSIGNED_INT_5_9_9_9_REV = $8C3E;
  GL_TRANSFORM_FEEDBACK_VARYING_MAX_LENGTH = $8C76;
  GL_TRANSFORM_FEEDBACK_BUFFER_MODE = $8C7F;
  GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS = $8C80;
  GL_TRANSFORM_FEEDBACK_VARYINGS = $8C83;
  GL_TRANSFORM_FEEDBACK_BUFFER_START = $8C84;
  GL_TRANSFORM_FEEDBACK_BUFFER_SIZE = $8C85;
  GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN = $8C88;
  GL_RASTERIZER_DISCARD = $8C89;
  GL_MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS = $8C8A;
  GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS = $8C8B;
  GL_INTERLEAVED_ATTRIBS = $8C8C;
  GL_SEPARATE_ATTRIBS = $8C8D;
  GL_TRANSFORM_FEEDBACK_BUFFER = $8C8E;
  GL_TRANSFORM_FEEDBACK_BUFFER_BINDING = $8C8F;
  GL_RGBA32UI = $8D70;
  GL_RGB32UI = $8D71;
  GL_RGBA16UI = $8D76;
  GL_RGB16UI = $8D77;
  GL_RGBA8UI = $8D7C;
  GL_RGB8UI = $8D7D;
  GL_RGBA32I = $8D82;
  GL_RGB32I = $8D83;
  GL_RGBA16I = $8D88;
  GL_RGB16I = $8D89;
  GL_RGBA8I = $8D8E;
  GL_RGB8I = $8D8F;
  GL_RED_INTEGER = $8D94;
  GL_RGB_INTEGER = $8D98;
  GL_RGBA_INTEGER = $8D99;
  GL_SAMPLER_2D_ARRAY = $8DC1;
  GL_SAMPLER_2D_ARRAY_SHADOW = $8DC4;
  GL_SAMPLER_CUBE_SHADOW = $8DC5;
  GL_UNSIGNED_INT_VEC2 = $8DC6;
  GL_UNSIGNED_INT_VEC3 = $8DC7;
  GL_UNSIGNED_INT_VEC4 = $8DC8;
  GL_INT_SAMPLER_2D = $8DCA;
  GL_INT_SAMPLER_3D = $8DCB;
  GL_INT_SAMPLER_CUBE = $8DCC;
  GL_INT_SAMPLER_2D_ARRAY = $8DCF;
  GL_UNSIGNED_INT_SAMPLER_2D = $8DD2;
  GL_UNSIGNED_INT_SAMPLER_3D = $8DD3;
  GL_UNSIGNED_INT_SAMPLER_CUBE = $8DD4;
  GL_UNSIGNED_INT_SAMPLER_2D_ARRAY = $8DD7;
  GL_BUFFER_ACCESS_FLAGS = $911F;
  GL_BUFFER_MAP_LENGTH = $9120;
  GL_BUFFER_MAP_OFFSET = $9121;
  GL_DEPTH_COMPONENT32F = $8CAC;
  GL_DEPTH32F_STENCIL8 = $8CAD;
  GL_FLOAT_32_UNSIGNED_INT_24_8_REV = $8DAD;
  GL_FRAMEBUFFER_ATTACHMENT_COLOR_ENCODING = $8210;
  GL_FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE = $8211;
  GL_FRAMEBUFFER_ATTACHMENT_RED_SIZE = $8212;
  GL_FRAMEBUFFER_ATTACHMENT_GREEN_SIZE = $8213;
  GL_FRAMEBUFFER_ATTACHMENT_BLUE_SIZE = $8214;
  GL_FRAMEBUFFER_ATTACHMENT_ALPHA_SIZE = $8215;
  GL_FRAMEBUFFER_ATTACHMENT_DEPTH_SIZE = $8216;
  GL_FRAMEBUFFER_ATTACHMENT_STENCIL_SIZE = $8217;
  GL_FRAMEBUFFER_DEFAULT = $8218;
  GL_FRAMEBUFFER_UNDEFINED = $8219;
  GL_DEPTH_STENCIL_ATTACHMENT = $821A;
  GL_DEPTH_STENCIL = $84F9;
  GL_UNSIGNED_INT_24_8 = $84FA;
  GL_DEPTH24_STENCIL8 = $88F0;
  GL_UNSIGNED_NORMALIZED = $8C17;
  GL_DRAW_FRAMEBUFFER_BINDING = $8CA6;
  GL_READ_FRAMEBUFFER = $8CA8;
  GL_DRAW_FRAMEBUFFER = $8CA9;
  GL_READ_FRAMEBUFFER_BINDING = $8CAA;
  GL_RENDERBUFFER_SAMPLES = $8CAB;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER = $8CD4;
  GL_MAX_COLOR_ATTACHMENTS = $8CDF;
  GL_COLOR_ATTACHMENT1 = $8CE1;
  GL_COLOR_ATTACHMENT2 = $8CE2;
  GL_COLOR_ATTACHMENT3 = $8CE3;
  GL_COLOR_ATTACHMENT4 = $8CE4;
  GL_COLOR_ATTACHMENT5 = $8CE5;
  GL_COLOR_ATTACHMENT6 = $8CE6;
  GL_COLOR_ATTACHMENT7 = $8CE7;
  GL_COLOR_ATTACHMENT8 = $8CE8;
  GL_COLOR_ATTACHMENT9 = $8CE9;
  GL_COLOR_ATTACHMENT10 = $8CEA;
  GL_COLOR_ATTACHMENT11 = $8CEB;
  GL_COLOR_ATTACHMENT12 = $8CEC;
  GL_COLOR_ATTACHMENT13 = $8CED;
  GL_COLOR_ATTACHMENT14 = $8CEE;
  GL_COLOR_ATTACHMENT15 = $8CEF;
  GL_COLOR_ATTACHMENT16 = $8CF0;
  GL_COLOR_ATTACHMENT17 = $8CF1;
  GL_COLOR_ATTACHMENT18 = $8CF2;
  GL_COLOR_ATTACHMENT19 = $8CF3;
  GL_COLOR_ATTACHMENT20 = $8CF4;
  GL_COLOR_ATTACHMENT21 = $8CF5;
  GL_COLOR_ATTACHMENT22 = $8CF6;
  GL_COLOR_ATTACHMENT23 = $8CF7;
  GL_COLOR_ATTACHMENT24 = $8CF8;
  GL_COLOR_ATTACHMENT25 = $8CF9;
  GL_COLOR_ATTACHMENT26 = $8CFA;
  GL_COLOR_ATTACHMENT27 = $8CFB;
  GL_COLOR_ATTACHMENT28 = $8CFC;
  GL_COLOR_ATTACHMENT29 = $8CFD;
  GL_COLOR_ATTACHMENT30 = $8CFE;
  GL_COLOR_ATTACHMENT31 = $8CFF;
  GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE = $8D56;
  GL_MAX_SAMPLES = $8D57;
  GL_HALF_FLOAT = $140B;
  GL_MAP_READ_BIT = $0001;
  GL_MAP_WRITE_BIT = $0002;
  GL_MAP_INVALIDATE_RANGE_BIT = $0004;
  GL_MAP_INVALIDATE_BUFFER_BIT = $0008;
  GL_MAP_FLUSH_EXPLICIT_BIT = $0010;
  GL_MAP_UNSYNCHRONIZED_BIT = $0020;
  GL_RG = $8227;
  GL_RG_INTEGER = $8228;
  GL_R8 = $8229;
  GL_RG8 = $822B;
  GL_R16F = $822D;
  GL_R32F = $822E;
  GL_RG16F = $822F;
  GL_RG32F = $8230;
  GL_R8I = $8231;
  GL_R8UI = $8232;
  GL_R16I = $8233;
  GL_R16UI = $8234;
  GL_R32I = $8235;
  GL_R32UI = $8236;
  GL_RG8I = $8237;
  GL_RG8UI = $8238;
  GL_RG16I = $8239;
  GL_RG16UI = $823A;
  GL_RG32I = $823B;
  GL_RG32UI = $823C;
  GL_VERTEX_ARRAY_BINDING = $85B5;
  GL_R8_SNORM = $8F94;
  GL_RG8_SNORM = $8F95;
  GL_RGB8_SNORM = $8F96;
  GL_RGBA8_SNORM = $8F97;
  GL_SIGNED_NORMALIZED = $8F9C;
  GL_PRIMITIVE_RESTART_FIXED_INDEX = $8D69;
  GL_COPY_READ_BUFFER = $8F36;
  GL_COPY_WRITE_BUFFER = $8F37;
  GL_COPY_READ_BUFFER_BINDING = $8F36;
  GL_COPY_WRITE_BUFFER_BINDING = $8F37;
  GL_UNIFORM_BUFFER = $8A11;
  GL_UNIFORM_BUFFER_BINDING = $8A28;
  GL_UNIFORM_BUFFER_START = $8A29;
  GL_UNIFORM_BUFFER_SIZE = $8A2A;
  GL_MAX_VERTEX_UNIFORM_BLOCKS = $8A2B;
  GL_MAX_FRAGMENT_UNIFORM_BLOCKS = $8A2D;
  GL_MAX_COMBINED_UNIFORM_BLOCKS = $8A2E;
  GL_MAX_UNIFORM_BUFFER_BINDINGS = $8A2F;
  GL_MAX_UNIFORM_BLOCK_SIZE = $8A30;
  GL_MAX_COMBINED_VERTEX_UNIFORM_COMPONENTS = $8A31;
  GL_MAX_COMBINED_FRAGMENT_UNIFORM_COMPONENTS = $8A33;
  GL_UNIFORM_BUFFER_OFFSET_ALIGNMENT = $8A34;
  GL_ACTIVE_UNIFORM_BLOCK_MAX_NAME_LENGTH = $8A35;
  GL_ACTIVE_UNIFORM_BLOCKS = $8A36;
  GL_UNIFORM_TYPE = $8A37;
  GL_UNIFORM_SIZE = $8A38;
  GL_UNIFORM_NAME_LENGTH = $8A39;
  GL_UNIFORM_BLOCK_INDEX = $8A3A;
  GL_UNIFORM_OFFSET = $8A3B;
  GL_UNIFORM_ARRAY_STRIDE = $8A3C;
  GL_UNIFORM_MATRIX_STRIDE = $8A3D;
  GL_UNIFORM_IS_ROW_MAJOR = $8A3E;
  GL_UNIFORM_BLOCK_BINDING = $8A3F;
  GL_UNIFORM_BLOCK_DATA_SIZE = $8A40;
  GL_UNIFORM_BLOCK_NAME_LENGTH = $8A41;
  GL_UNIFORM_BLOCK_ACTIVE_UNIFORMS = $8A42;
  GL_UNIFORM_BLOCK_ACTIVE_UNIFORM_INDICES = $8A43;
  GL_UNIFORM_BLOCK_REFERENCED_BY_VERTEX_SHADER = $8A44;
  GL_UNIFORM_BLOCK_REFERENCED_BY_FRAGMENT_SHADER = $8A46;
  GL_INVALID_INDEX = $FFFFFFFF;
  GL_MAX_VERTEX_OUTPUT_COMPONENTS = $9122;
  GL_MAX_FRAGMENT_INPUT_COMPONENTS = $9125;
  GL_MAX_SERVER_WAIT_TIMEOUT = $9111;
  GL_OBJECT_TYPE = $9112;
  GL_SYNC_CONDITION = $9113;
  GL_SYNC_STATUS = $9114;
  GL_SYNC_FLAGS = $9115;
  GL_SYNC_FENCE = $9116;
  GL_SYNC_GPU_COMMANDS_COMPLETE = $9117;
  GL_UNSIGNALED = $9118;
  GL_SIGNALED = $9119;
  GL_ALREADY_SIGNALED = $911A;
  GL_TIMEOUT_EXPIRED = $911B;
  GL_CONDITION_SATISFIED = $911C;
  GL_WAIT_FAILED = $911D;
  GL_SYNC_FLUSH_COMMANDS_BIT = $00000001;
  GL_TIMEOUT_IGNORED = GLuint64($FFFFFFFFFFFFFFFF);
  GL_VERTEX_ATTRIB_ARRAY_DIVISOR = $88FE;
  GL_ANY_SAMPLES_PASSED = $8C2F;
  GL_ANY_SAMPLES_PASSED_CONSERVATIVE = $8D6A;
  GL_SAMPLER_BINDING = $8919;
  GL_RGB10_A2UI = $906F;
  GL_TEXTURE_SWIZZLE_R = $8E42;
  GL_TEXTURE_SWIZZLE_G = $8E43;
  GL_TEXTURE_SWIZZLE_B = $8E44;
  GL_TEXTURE_SWIZZLE_A = $8E45;
  GL_GREEN = $1904;
  GL_BLUE = $1905;
  GL_INT_2_10_10_10_REV = $8D9F;
  GL_TRANSFORM_FEEDBACK = $8E22;
  GL_TRANSFORM_FEEDBACK_PAUSED = $8E23;
  GL_TRANSFORM_FEEDBACK_ACTIVE = $8E24;
  GL_TRANSFORM_FEEDBACK_BINDING = $8E25;
  GL_PROGRAM_BINARY_RETRIEVABLE_HINT = $8257;
  GL_PROGRAM_BINARY_LENGTH = $8741;
  GL_NUM_PROGRAM_BINARY_FORMATS = $87FE;
  GL_PROGRAM_BINARY_FORMATS = $87FF;
  GL_COMPRESSED_R11_EAC = $9270;
  GL_COMPRESSED_SIGNED_R11_EAC = $9271;
  GL_COMPRESSED_RG11_EAC = $9272;
  GL_COMPRESSED_SIGNED_RG11_EAC = $9273;
  GL_COMPRESSED_RGB8_ETC2 = $9274;
  GL_COMPRESSED_SRGB8_ETC2 = $9275;
  GL_COMPRESSED_RGB8_PUNCHTHROUGH_ALPHA1_ETC2 = $9276;
  GL_COMPRESSED_SRGB8_PUNCHTHROUGH_ALPHA1_ETC2 = $9277;
  GL_COMPRESSED_RGBA8_ETC2_EAC = $9278;
  GL_COMPRESSED_SRGB8_ALPHA8_ETC2_EAC = $9279;
  GL_TEXTURE_IMMUTABLE_FORMAT = $912F;
  GL_MAX_ELEMENT_INDEX = $8D6B;
  GL_NUM_SAMPLE_COUNTS = $9380;
  GL_TEXTURE_IMMUTABLE_LEVELS = $82DF;

var
  glReadBuffer: procedure(src: GLenum); apicall;
  glDrawRangeElements: procedure(mode: GLenum; start: GLuint; end_: GLuint; count: GLsizei; type_: GLenum; indices: Pointer); apicall;
  glTexImage3D: procedure(target: GLenum; level: GLint; internalformat: GLint; width: GLsizei; height: GLsizei; depth: GLsizei; border: GLint; format: GLenum; type_: GLenum; pixels: Pointer); apicall;
  glTexSubImage3D: procedure(target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; zoffset: GLint; width: GLsizei; height: GLsizei; depth: GLsizei; format: GLenum; type_: GLenum; pixels: Pointer); apicall;
  glCopyTexSubImage3D: procedure(target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; zoffset: GLint; x: GLint; y: GLint; width: GLsizei; height: GLsizei); apicall;
  glCompressedTexImage3D: procedure(target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; height: GLsizei; depth: GLsizei; border: GLint; imageSize: GLsizei; data: Pointer); apicall;
  glCompressedTexSubImage3D: procedure(target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; zoffset: GLint; width: GLsizei; height: GLsizei; depth: GLsizei; format: GLenum; imageSize: GLsizei; data: Pointer); apicall;
  glGenQueries: procedure(n: GLsizei; ids: PGLuint); apicall;
  glDeleteQueries: procedure(n: GLsizei; ids: PGLuint); apicall;
  glIsQuery: function(id: GLuint): GLboolean; apicall;
  glBeginQuery: procedure(target: GLenum; id: GLuint); apicall;
  glEndQuery: procedure(target: GLenum); apicall;
  glGetQueryiv: procedure(target: GLenum; pname: GLenum; params: PGLint); apicall;
  glGetQueryObjectuiv: procedure(id: GLuint; pname: GLenum; params: PGLuint); apicall;
  glUnmapBuffer: function(target: GLenum): GLboolean; apicall;
  glGetBufferPointerv: procedure(target: GLenum; pname: GLenum; params: PPointer); apicall;
  glDrawBuffers: procedure(n: GLsizei; bufs: PGLenum); apicall;
  glUniformMatrix2x3fv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); apicall;
  glUniformMatrix3x2fv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); apicall;
  glUniformMatrix2x4fv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); apicall;
  glUniformMatrix4x2fv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); apicall;
  glUniformMatrix3x4fv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); apicall;
  glUniformMatrix4x3fv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); apicall;
  glBlitFramebuffer: procedure(srcX0: GLint; srcY0: GLint; srcX1: GLint; srcY1: GLint; dstX0: GLint; dstY0: GLint; dstX1: GLint; dstY1: GLint; mask: GLbitfield; filter: GLenum); apicall;
  glRenderbufferStorageMultisample: procedure(target: GLenum; samples: GLsizei; internalformat: GLenum; width: GLsizei; height: GLsizei); apicall;
  glFramebufferTextureLayer: procedure(target: GLenum; attachment: GLenum; texture: GLuint; level: GLint; layer: GLint); apicall;
  glMapBufferRange: function(target: GLenum; offset: GLintptr; length: GLsizeiptr; access: GLbitfield): Pointer; apicall;
  glFlushMappedBufferRange: procedure(target: GLenum; offset: GLintptr; length: GLsizeiptr); apicall;
  glBindVertexArray: procedure(array_: GLuint); apicall;
  glDeleteVertexArrays: procedure(n: GLsizei; arrays: PGLuint); apicall;
  glGenVertexArrays: procedure(n: GLsizei; arrays: PGLuint); apicall;
  glIsVertexArray: function(array_: GLuint): GLboolean; apicall;
  glGetIntegeri_v: procedure(target: GLenum; index: GLuint; data: PGLint); apicall;
  glBeginTransformFeedback: procedure(primitiveMode: GLenum); apicall;
  glEndTransformFeedback: procedure; apicall;
  glBindBufferRange: procedure(target: GLenum; index: GLuint; buffer: GLuint; offset: GLintptr; size: GLsizeiptr); apicall;
  glBindBufferBase: procedure(target: GLenum; index: GLuint; buffer: GLuint); apicall;
  glTransformFeedbackVaryings: procedure(program_: GLuint; count: GLsizei; varyings: PPGLchar; bufferMode: GLenum); apicall;
  glGetTransformFeedbackVarying: procedure(program_: GLuint; index: GLuint; bufSize: GLsizei; length: PGLsizei; size: PGLsizei; type_: PGLenum; name: PGLchar); apicall;
  glVertexAttribIPointer: procedure(index: GLuint; size: GLint; type_: GLenum; stride: GLsizei; pointer: Pointer); apicall;
  glGetVertexAttribIiv: procedure(index: GLuint; pname: GLenum; params: PGLint); apicall;
  glGetVertexAttribIuiv: procedure(index: GLuint; pname: GLenum; params: PGLuint); apicall;
  glVertexAttribI4i: procedure(index: GLuint; x: GLint; y: GLint; z: GLint; w: GLint); apicall;
  glVertexAttribI4ui: procedure(index: GLuint; x: GLuint; y: GLuint; z: GLuint; w: GLuint); apicall;
  glVertexAttribI4iv: procedure(index: GLuint; v: PGLint); apicall;
  glVertexAttribI4uiv: procedure(index: GLuint; v: PGLuint); apicall;
  glGetUniformuiv: procedure(program_: GLuint; location: GLint; params: PGLuint); apicall;
  glGetFragDataLocation: function(program_: GLuint; name: PGLchar): GLint; apicall;
  glUniform1ui: procedure(location: GLint; v0: GLuint); apicall;
  glUniform2ui: procedure(location: GLint; v0: GLuint; v1: GLuint); apicall;
  glUniform3ui: procedure(location: GLint; v0: GLuint; v1: GLuint; v2: GLuint); apicall;
  glUniform4ui: procedure(location: GLint; v0: GLuint; v1: GLuint; v2: GLuint; v3: GLuint); apicall;
  glUniform1uiv: procedure(location: GLint; count: GLsizei; value: PGLuint); apicall;
  glUniform2uiv: procedure(location: GLint; count: GLsizei; value: PGLuint); apicall;
  glUniform3uiv: procedure(location: GLint; count: GLsizei; value: PGLuint); apicall;
  glUniform4uiv: procedure(location: GLint; count: GLsizei; value: PGLuint); apicall;
  glClearBufferiv: procedure(buffer: GLenum; drawbuffer: GLint; value: PGLint); apicall;
  glClearBufferuiv: procedure(buffer: GLenum; drawbuffer: GLint; value: PGLuint); apicall;
  glClearBufferfv: procedure(buffer: GLenum; drawbuffer: GLint; value: PGLfloat); apicall;
  glClearBufferfi: procedure(buffer: GLenum; drawbuffer: GLint; depth: GLfloat; stencil: GLint); apicall;
  glGetStringi: function(name: GLenum; index: GLuint): PGLubyte; apicall;
  glCopyBufferSubData: procedure(readTarget: GLenum; writeTarget: GLenum; readOffset: GLintptr; writeOffset: GLintptr; size: GLsizeiptr); apicall;
  glGetUniformIndices: procedure(program_: GLuint; uniformCount: GLsizei; uniformNames: PPGLchar; uniformIndices: PGLuint); apicall;
  glGetActiveUniformsiv: procedure(program_: GLuint; uniformCount: GLsizei; uniformIndices: PGLuint; pname: GLenum; params: PGLint); apicall;
  glGetUniformBlockIndex: function(program_: GLuint; uniformBlockName: PGLchar): GLuint; apicall;
  glGetActiveUniformBlockiv: procedure(program_: GLuint; uniformBlockIndex: GLuint; pname: GLenum; params: PGLint); apicall;
  glGetActiveUniformBlockName: procedure(program_: GLuint; uniformBlockIndex: GLuint; bufSize: GLsizei; length: PGLsizei; uniformBlockName: PGLchar); apicall;
  glUniformBlockBinding: procedure(program_: GLuint; uniformBlockIndex: GLuint; uniformBlockBinding: GLuint); apicall;
  glDrawArraysInstanced: procedure(mode: GLenum; first: GLint; count: GLsizei; instancecount: GLsizei); apicall;
  glDrawElementsInstanced: procedure(mode: GLenum; count: GLsizei; type_: GLenum; indices: Pointer; instancecount: GLsizei); apicall;
  glFenceSync: function(condition: GLenum; flags: GLbitfield): GLsync; apicall;
  glIsSync: function(sync: GLsync): GLboolean; apicall;
  glDeleteSync: procedure(sync: GLsync); apicall;
  glClientWaitSync: function(sync: GLsync; flags: GLbitfield; timeout: GLuint64): GLenum; apicall;
  glWaitSync: procedure(sync: GLsync; flags: GLbitfield; timeout: GLuint64); apicall;
  glGetInteger64v: procedure(pname: GLenum; data: PGLint64); apicall;
  glGetSynciv: procedure(sync: GLsync; pname: GLenum; count: GLsizei; length: PGLsizei; values: PGLint); apicall;
  glGetInteger64i_v: procedure(target: GLenum; index: GLuint; data: PGLint64); apicall;
  glGetBufferParameteri64v: procedure(target: GLenum; pname: GLenum; params: PGLint64); apicall;
  glGenSamplers: procedure(count: GLsizei; samplers: PGLuint); apicall;
  glDeleteSamplers: procedure(count: GLsizei; samplers: PGLuint); apicall;
  glIsSampler: function(sampler: GLuint): GLboolean; apicall;
  glBindSampler: procedure(unit_: GLuint; sampler: GLuint); apicall;
  glSamplerParameteri: procedure(sampler: GLuint; pname: GLenum; param: GLint); apicall;
  glSamplerParameteriv: procedure(sampler: GLuint; pname: GLenum; param: PGLint); apicall;
  glSamplerParameterf: procedure(sampler: GLuint; pname: GLenum; param: GLfloat); apicall;
  glSamplerParameterfv: procedure(sampler: GLuint; pname: GLenum; param: PGLfloat); apicall;
  glGetSamplerParameteriv: procedure(sampler: GLuint; pname: GLenum; params: PGLint); apicall;
  glGetSamplerParameterfv: procedure(sampler: GLuint; pname: GLenum; params: PGLfloat); apicall;
  glVertexAttribDivisor: procedure(index: GLuint; divisor: GLuint); apicall;
  glBindTransformFeedback: procedure(target: GLenum; id: GLuint); apicall;
  glDeleteTransformFeedbacks: procedure(n: GLsizei; ids: PGLuint); apicall;
  glGenTransformFeedbacks: procedure(n: GLsizei; ids: PGLuint); apicall;
  glIsTransformFeedback: function(id: GLuint): GLboolean; apicall;
  glPauseTransformFeedback: procedure; apicall;
  glResumeTransformFeedback: procedure; apicall;
  glGetProgramBinary: procedure(program_: GLuint; bufSize: GLsizei; length: PGLsizei; binaryFormat: PGLenum; binary: Pointer); apicall;
  glProgramBinary: procedure(program_: GLuint; binaryFormat: GLenum; binary: Pointer; length: GLsizei); apicall;
  glProgramParameteri: procedure(program_: GLuint; pname: GLenum; value: GLint); apicall;
  glInvalidateFramebuffer: procedure(target: GLenum; numAttachments: GLsizei; attachments: PGLenum); apicall;
  glInvalidateSubFramebuffer: procedure(target: GLenum; numAttachments: GLsizei; attachments: PGLenum; x: GLint; y: GLint; width: GLsizei; height: GLsizei); apicall;
  glTexStorage2D: procedure(target: GLenum; levels: GLsizei; internalformat: GLenum; width: GLsizei; height: GLsizei); apicall;
  glTexStorage3D: procedure(target: GLenum; levels: GLsizei; internalformat: GLenum; width: GLsizei; height: GLsizei; depth: GLsizei); apicall;
  glGetInternalformativ: procedure(target: GLenum; internalformat: GLenum; pname: GLenum; count: GLsizei; params: PGLint); apicall;
{$endif}
{$endregion}

{ OpenGL ES 3.1 }

{$region gles31}
{$ifdef gles31}
const
  GL_COMPUTE_SHADER = $91B9;
  GL_MAX_COMPUTE_UNIFORM_BLOCKS = $91BB;
  GL_MAX_COMPUTE_TEXTURE_IMAGE_UNITS = $91BC;
  GL_MAX_COMPUTE_IMAGE_UNIFORMS = $91BD;
  GL_MAX_COMPUTE_SHARED_MEMORY_SIZE = $8262;
  GL_MAX_COMPUTE_UNIFORM_COMPONENTS = $8263;
  GL_MAX_COMPUTE_ATOMIC_COUNTER_BUFFERS = $8264;
  GL_MAX_COMPUTE_ATOMIC_COUNTERS = $8265;
  GL_MAX_COMBINED_COMPUTE_UNIFORM_COMPONENTS = $8266;
  GL_MAX_COMPUTE_WORK_GROUP_INVOCATIONS = $90EB;
  GL_MAX_COMPUTE_WORK_GROUP_COUNT = $91BE;
  GL_MAX_COMPUTE_WORK_GROUP_SIZE = $91BF;
  GL_COMPUTE_WORK_GROUP_SIZE = $8267;
  GL_DISPATCH_INDIRECT_BUFFER = $90EE;
  GL_DISPATCH_INDIRECT_BUFFER_BINDING = $90EF;
  GL_COMPUTE_SHADER_BIT = $00000020;
  GL_DRAW_INDIRECT_BUFFER = $8F3F;
  GL_DRAW_INDIRECT_BUFFER_BINDING = $8F43;
  GL_MAX_UNIFORM_LOCATIONS = $826E;
  GL_FRAMEBUFFER_DEFAULT_WIDTH = $9310;
  GL_FRAMEBUFFER_DEFAULT_HEIGHT = $9311;
  GL_FRAMEBUFFER_DEFAULT_SAMPLES = $9313;
  GL_FRAMEBUFFER_DEFAULT_FIXED_SAMPLE_LOCATIONS = $9314;
  GL_MAX_FRAMEBUFFER_WIDTH = $9315;
  GL_MAX_FRAMEBUFFER_HEIGHT = $9316;
  GL_MAX_FRAMEBUFFER_SAMPLES = $9318;
  GL_UNIFORM = $92E1;
  GL_UNIFORM_BLOCK = $92E2;
  GL_PROGRAM_INPUT = $92E3;
  GL_PROGRAM_OUTPUT = $92E4;
  GL_BUFFER_VARIABLE = $92E5;
  GL_SHADER_STORAGE_BLOCK = $92E6;
  GL_ATOMIC_COUNTER_BUFFER = $92C0;
  GL_TRANSFORM_FEEDBACK_VARYING = $92F4;
  GL_ACTIVE_RESOURCES = $92F5;
  GL_MAX_NAME_LENGTH = $92F6;
  GL_MAX_NUM_ACTIVE_VARIABLES = $92F7;
  GL_NAME_LENGTH = $92F9;
  GL_TYPE = $92FA;
  GL_ARRAY_SIZE = $92FB;
  GL_OFFSET = $92FC;
  GL_BLOCK_INDEX = $92FD;
  GL_ARRAY_STRIDE = $92FE;
  GL_MATRIX_STRIDE = $92FF;
  GL_IS_ROW_MAJOR = $9300;
  GL_ATOMIC_COUNTER_BUFFER_INDEX = $9301;
  GL_BUFFER_BINDING = $9302;
  GL_BUFFER_DATA_SIZE = $9303;
  GL_NUM_ACTIVE_VARIABLES = $9304;
  GL_ACTIVE_VARIABLES = $9305;
  GL_REFERENCED_BY_VERTEX_SHADER = $9306;
  GL_REFERENCED_BY_FRAGMENT_SHADER = $930A;
  GL_REFERENCED_BY_COMPUTE_SHADER = $930B;
  GL_TOP_LEVEL_ARRAY_SIZE = $930C;
  GL_TOP_LEVEL_ARRAY_STRIDE = $930D;
  GL_LOCATION = $930E;
  GL_VERTEX_SHADER_BIT = $00000001;
  GL_FRAGMENT_SHADER_BIT = $00000002;
  GL_ALL_SHADER_BITS = $FFFFFFFF;
  GL_PROGRAM_SEPARABLE = $8258;
  GL_ACTIVE_PROGRAM = $8259;
  GL_PROGRAM_PIPELINE_BINDING = $825A;
  GL_ATOMIC_COUNTER_BUFFER_BINDING = $92C1;
  GL_ATOMIC_COUNTER_BUFFER_START = $92C2;
  GL_ATOMIC_COUNTER_BUFFER_SIZE = $92C3;
  GL_MAX_VERTEX_ATOMIC_COUNTER_BUFFERS = $92CC;
  GL_MAX_FRAGMENT_ATOMIC_COUNTER_BUFFERS = $92D0;
  GL_MAX_COMBINED_ATOMIC_COUNTER_BUFFERS = $92D1;
  GL_MAX_VERTEX_ATOMIC_COUNTERS = $92D2;
  GL_MAX_FRAGMENT_ATOMIC_COUNTERS = $92D6;
  GL_MAX_COMBINED_ATOMIC_COUNTERS = $92D7;
  GL_MAX_ATOMIC_COUNTER_BUFFER_SIZE = $92D8;
  GL_MAX_ATOMIC_COUNTER_BUFFER_BINDINGS = $92DC;
  GL_ACTIVE_ATOMIC_COUNTER_BUFFERS = $92D9;
  GL_UNSIGNED_INT_ATOMIC_COUNTER = $92DB;
  GL_MAX_IMAGE_UNITS = $8F38;
  GL_MAX_VERTEX_IMAGE_UNIFORMS = $90CA;
  GL_MAX_FRAGMENT_IMAGE_UNIFORMS = $90CE;
  GL_MAX_COMBINED_IMAGE_UNIFORMS = $90CF;
  GL_IMAGE_BINDING_NAME = $8F3A;
  GL_IMAGE_BINDING_LEVEL = $8F3B;
  GL_IMAGE_BINDING_LAYERED = $8F3C;
  GL_IMAGE_BINDING_LAYER = $8F3D;
  GL_IMAGE_BINDING_ACCESS = $8F3E;
  GL_IMAGE_BINDING_FORMAT = $906E;
  GL_VERTEX_ATTRIB_ARRAY_BARRIER_BIT = $00000001;
  GL_ELEMENT_ARRAY_BARRIER_BIT = $00000002;
  GL_UNIFORM_BARRIER_BIT = $00000004;
  GL_TEXTURE_FETCH_BARRIER_BIT = $00000008;
  GL_SHADER_IMAGE_ACCESS_BARRIER_BIT = $00000020;
  GL_COMMAND_BARRIER_BIT = $00000040;
  GL_PIXEL_BUFFER_BARRIER_BIT = $00000080;
  GL_TEXTURE_UPDATE_BARRIER_BIT = $00000100;
  GL_BUFFER_UPDATE_BARRIER_BIT = $00000200;
  GL_FRAMEBUFFER_BARRIER_BIT = $00000400;
  GL_TRANSFORM_FEEDBACK_BARRIER_BIT = $00000800;
  GL_ATOMIC_COUNTER_BARRIER_BIT = $00001000;
  GL_ALL_BARRIER_BITS = $FFFFFFFF;
  GL_IMAGE_2D = $904D;
  GL_IMAGE_3D = $904E;
  GL_IMAGE_CUBE = $9050;
  GL_IMAGE_2D_ARRAY = $9053;
  GL_INT_IMAGE_2D = $9058;
  GL_INT_IMAGE_3D = $9059;
  GL_INT_IMAGE_CUBE = $905B;
  GL_INT_IMAGE_2D_ARRAY = $905E;
  GL_UNSIGNED_INT_IMAGE_2D = $9063;
  GL_UNSIGNED_INT_IMAGE_3D = $9064;
  GL_UNSIGNED_INT_IMAGE_CUBE = $9066;
  GL_UNSIGNED_INT_IMAGE_2D_ARRAY = $9069;
  GL_IMAGE_FORMAT_COMPATIBILITY_TYPE = $90C7;
  GL_IMAGE_FORMAT_COMPATIBILITY_BY_SIZE = $90C8;
  GL_IMAGE_FORMAT_COMPATIBILITY_BY_CLASS = $90C9;
  GL_READ_ONLY = $88B8;
  GL_WRITE_ONLY = $88B9;
  GL_READ_WRITE = $88BA;
  GL_SHADER_STORAGE_BUFFER = $90D2;
  GL_SHADER_STORAGE_BUFFER_BINDING = $90D3;
  GL_SHADER_STORAGE_BUFFER_START = $90D4;
  GL_SHADER_STORAGE_BUFFER_SIZE = $90D5;
  GL_MAX_VERTEX_SHADER_STORAGE_BLOCKS = $90D6;
  GL_MAX_FRAGMENT_SHADER_STORAGE_BLOCKS = $90DA;
  GL_MAX_COMPUTE_SHADER_STORAGE_BLOCKS = $90DB;
  GL_MAX_COMBINED_SHADER_STORAGE_BLOCKS = $90DC;
  GL_MAX_SHADER_STORAGE_BUFFER_BINDINGS = $90DD;
  GL_MAX_SHADER_STORAGE_BLOCK_SIZE = $90DE;
  GL_SHADER_STORAGE_BUFFER_OFFSET_ALIGNMENT = $90DF;
  GL_SHADER_STORAGE_BARRIER_BIT = $00002000;
  GL_MAX_COMBINED_SHADER_OUTPUT_RESOURCES = $8F39;
  GL_DEPTH_STENCIL_TEXTURE_MODE = $90EA;
  GL_STENCIL_INDEX = $1901;
  GL_MIN_PROGRAM_TEXTURE_GATHER_OFFSET = $8E5E;
  GL_MAX_PROGRAM_TEXTURE_GATHER_OFFSET = $8E5F;
  GL_SAMPLE_POSITION = $8E50;
  GL_SAMPLE_MASK = $8E51;
  GL_SAMPLE_MASK_VALUE = $8E52;
  GL_TEXTURE_2D_MULTISAMPLE = $9100;
  GL_MAX_SAMPLE_MASK_WORDS = $8E59;
  GL_MAX_COLOR_TEXTURE_SAMPLES = $910E;
  GL_MAX_DEPTH_TEXTURE_SAMPLES = $910F;
  GL_MAX_INTEGER_SAMPLES = $9110;
  GL_TEXTURE_BINDING_2D_MULTISAMPLE = $9104;
  GL_TEXTURE_SAMPLES = $9106;
  GL_TEXTURE_FIXED_SAMPLE_LOCATIONS = $9107;
  GL_TEXTURE_WIDTH = $1000;
  GL_TEXTURE_HEIGHT = $1001;
  GL_TEXTURE_DEPTH = $8071;
  GL_TEXTURE_INTERNAL_FORMAT = $1003;
  GL_TEXTURE_RED_SIZE = $805C;
  GL_TEXTURE_GREEN_SIZE = $805D;
  GL_TEXTURE_BLUE_SIZE = $805E;
  GL_TEXTURE_ALPHA_SIZE = $805F;
  GL_TEXTURE_DEPTH_SIZE = $884A;
  GL_TEXTURE_STENCIL_SIZE = $88F1;
  GL_TEXTURE_SHARED_SIZE = $8C3F;
  GL_TEXTURE_RED_TYPE = $8C10;
  GL_TEXTURE_GREEN_TYPE = $8C11;
  GL_TEXTURE_BLUE_TYPE = $8C12;
  GL_TEXTURE_ALPHA_TYPE = $8C13;
  GL_TEXTURE_DEPTH_TYPE = $8C16;
  GL_TEXTURE_COMPRESSED = $86A1;
  GL_SAMPLER_2D_MULTISAMPLE = $9108;
  GL_INT_SAMPLER_2D_MULTISAMPLE = $9109;
  GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE = $910A;
  GL_VERTEX_ATTRIB_BINDING = $82D4;
  GL_VERTEX_ATTRIB_RELATIVE_OFFSET = $82D5;
  GL_VERTEX_BINDING_DIVISOR = $82D6;
  GL_VERTEX_BINDING_OFFSET = $82D7;
  GL_VERTEX_BINDING_STRIDE = $82D8;
  GL_VERTEX_BINDING_BUFFER = $8F4F;
  GL_MAX_VERTEX_ATTRIB_RELATIVE_OFFSET = $82D9;
  GL_MAX_VERTEX_ATTRIB_BINDINGS = $82DA;
  GL_MAX_VERTEX_ATTRIB_STRIDE = $82E5;

var
  glDispatchCompute: procedure(num_groups_x: GLuint; num_groups_y: GLuint; num_groups_z: GLuint); apicall;
  glDispatchComputeIndirect: procedure(indirect: GLintptr); apicall;
  glDrawArraysIndirect: procedure(mode: GLenum; indirect: Pointer); apicall;
  glDrawElementsIndirect: procedure(mode: GLenum; type_: GLenum; indirect: Pointer); apicall;
  glFramebufferParameteri: procedure(target: GLenum; pname: GLenum; param: GLint); apicall;
  glGetFramebufferParameteriv: procedure(target: GLenum; pname: GLenum; params: PGLint); apicall;
  glGetProgramInterfaceiv: procedure(program_: GLuint; programInterface: GLenum; pname: GLenum; params: PGLint); apicall;
  glGetProgramResourceIndex: function(program_: GLuint; programInterface: GLenum; name: PGLchar): GLuint; apicall;
  glGetProgramResourceName: procedure(program_: GLuint; programInterface: GLenum; index: GLuint; bufSize: GLsizei; length: PGLsizei; name: PGLchar); apicall;
  glGetProgramResourceiv: procedure(program_: GLuint; programInterface: GLenum; index: GLuint; propCount: GLsizei; props: PGLenum; count: GLsizei; length: PGLsizei; params: PGLint); apicall;
  glGetProgramResourceLocation: function(program_: GLuint; programInterface: GLenum; name: PGLchar): GLint; apicall;
  glUseProgramStages: procedure(pipeline: GLuint; stages: GLbitfield; program_: GLuint); apicall;
  glActiveShaderProgram: procedure(pipeline: GLuint; program_: GLuint); apicall;
  glCreateShaderProgramv: function(type_: GLenum; count: GLsizei; strings: PPGLchar): GLuint; apicall;
  glBindProgramPipeline: procedure(pipeline: GLuint); apicall;
  glDeleteProgramPipelines: procedure(n: GLsizei; pipelines: PGLuint); apicall;
  glGenProgramPipelines: procedure(n: GLsizei; pipelines: PGLuint); apicall;
  glIsProgramPipeline: function(pipeline: GLuint): GLboolean; apicall;
  glGetProgramPipelineiv: procedure(pipeline: GLuint; pname: GLenum; params: PGLint); apicall;
  glProgramUniform1i: procedure(program_: GLuint; location: GLint; v0: GLint); apicall;
  glProgramUniform2i: procedure(program_: GLuint; location: GLint; v0: GLint; v1: GLint); apicall;
  glProgramUniform3i: procedure(program_: GLuint; location: GLint; v0: GLint; v1: GLint; v2: GLint); apicall;
  glProgramUniform4i: procedure(program_: GLuint; location: GLint; v0: GLint; v1: GLint; v2: GLint; v3: GLint); apicall;
  glProgramUniform1ui: procedure(program_: GLuint; location: GLint; v0: GLuint); apicall;
  glProgramUniform2ui: procedure(program_: GLuint; location: GLint; v0: GLuint; v1: GLuint); apicall;
  glProgramUniform3ui: procedure(program_: GLuint; location: GLint; v0: GLuint; v1: GLuint; v2: GLuint); apicall;
  glProgramUniform4ui: procedure(program_: GLuint; location: GLint; v0: GLuint; v1: GLuint; v2: GLuint; v3: GLuint); apicall;
  glProgramUniform1f: procedure(program_: GLuint; location: GLint; v0: GLfloat); apicall;
  glProgramUniform2f: procedure(program_: GLuint; location: GLint; v0: GLfloat; v1: GLfloat); apicall;
  glProgramUniform3f: procedure(program_: GLuint; location: GLint; v0: GLfloat; v1: GLfloat; v2: GLfloat); apicall;
  glProgramUniform4f: procedure(program_: GLuint; location: GLint; v0: GLfloat; v1: GLfloat; v2: GLfloat; v3: GLfloat); apicall;
  glProgramUniform1iv: procedure(program_: GLuint; location: GLint; count: GLsizei; value: PGLint); apicall;
  glProgramUniform2iv: procedure(program_: GLuint; location: GLint; count: GLsizei; value: PGLint); apicall;
  glProgramUniform3iv: procedure(program_: GLuint; location: GLint; count: GLsizei; value: PGLint); apicall;
  glProgramUniform4iv: procedure(program_: GLuint; location: GLint; count: GLsizei; value: PGLint); apicall;
  glProgramUniform1uiv: procedure(program_: GLuint; location: GLint; count: GLsizei; value: PGLuint); apicall;
  glProgramUniform2uiv: procedure(program_: GLuint; location: GLint; count: GLsizei; value: PGLuint); apicall;
  glProgramUniform3uiv: procedure(program_: GLuint; location: GLint; count: GLsizei; value: PGLuint); apicall;
  glProgramUniform4uiv: procedure(program_: GLuint; location: GLint; count: GLsizei; value: PGLuint); apicall;
  glProgramUniform1fv: procedure(program_: GLuint; location: GLint; count: GLsizei; value: PGLfloat); apicall;
  glProgramUniform2fv: procedure(program_: GLuint; location: GLint; count: GLsizei; value: PGLfloat); apicall;
  glProgramUniform3fv: procedure(program_: GLuint; location: GLint; count: GLsizei; value: PGLfloat); apicall;
  glProgramUniform4fv: procedure(program_: GLuint; location: GLint; count: GLsizei; value: PGLfloat); apicall;
  glProgramUniformMatrix2fv: procedure(program_: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); apicall;
  glProgramUniformMatrix3fv: procedure(program_: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); apicall;
  glProgramUniformMatrix4fv: procedure(program_: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); apicall;
  glProgramUniformMatrix2x3fv: procedure(program_: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); apicall;
  glProgramUniformMatrix3x2fv: procedure(program_: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); apicall;
  glProgramUniformMatrix2x4fv: procedure(program_: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); apicall;
  glProgramUniformMatrix4x2fv: procedure(program_: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); apicall;
  glProgramUniformMatrix3x4fv: procedure(program_: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); apicall;
  glProgramUniformMatrix4x3fv: procedure(program_: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); apicall;
  glValidateProgramPipeline: procedure(pipeline: GLuint); apicall;
  glGetProgramPipelineInfoLog: procedure(pipeline: GLuint; bufSize: GLsizei; length: PGLsizei; infoLog: PGLchar); apicall;
  glBindImageTexture: procedure(unit_: GLuint; texture: GLuint; level: GLint; layered: GLboolean; layer: GLint; access: GLenum; format: GLenum); apicall;
  glGetBooleani_v: procedure(target: GLenum; index: GLuint; data: PGLboolean); apicall;
  glMemoryBarrier: procedure(barriers: GLbitfield); apicall;
  glMemoryBarrierByRegion: procedure(barriers: GLbitfield); apicall;
  glTexStorage2DMultisample: procedure(target: GLenum; samples: GLsizei; internalformat: GLenum; width: GLsizei; height: GLsizei; fixedsamplelocations: GLboolean); apicall;
  glGetMultisamplefv: procedure(pname: GLenum; index: GLuint; val: PGLfloat); apicall;
  glSampleMaski: procedure(maskNumber: GLuint; mask: GLbitfield); apicall;
  glGetTexLevelParameteriv: procedure(target: GLenum; level: GLint; pname: GLenum; params: PGLint); apicall;
  glGetTexLevelParameterfv: procedure(target: GLenum; level: GLint; pname: GLenum; params: PGLfloat); apicall;
  glBindVertexBuffer: procedure(bindingindex: GLuint; buffer: GLuint; offset: GLintptr; stride: GLsizei); apicall;
  glVertexAttribFormat: procedure(attribindex: GLuint; size: GLint; type_: GLenum; normalized: GLboolean; relativeoffset: GLuint); apicall;
  glVertexAttribIFormat: procedure(attribindex: GLuint; size: GLint; type_: GLenum; relativeoffset: GLuint); apicall;
  glVertexAttribBinding: procedure(attribindex: GLuint; bindingindex: GLuint); apicall;
  glVertexBindingDivisor: procedure(bindingindex: GLuint; divisor: GLuint); apicall;
{$endif}
{$endregion}

{ OpenGL ES 3.2 }

{$region gles32}
{$ifdef gles32}
const
  GL_MULTISAMPLE_LINE_WIDTH_RANGE = $9381;
  GL_MULTISAMPLE_LINE_WIDTH_GRANULARITY = $9382;
  GL_MULTIPLY = $9294;
  GL_SCREEN = $9295;
  GL_OVERLAY = $9296;
  GL_DARKEN = $9297;
  GL_LIGHTEN = $9298;
  GL_COLORDODGE = $9299;
  GL_COLORBURN = $929A;
  GL_HARDLIGHT = $929B;
  GL_SOFTLIGHT = $929C;
  GL_DIFFERENCE = $929E;
  GL_EXCLUSION = $92A0;
  GL_HSL_HUE = $92AD;
  GL_HSL_SATURATION = $92AE;
  GL_HSL_COLOR = $92AF;
  GL_HSL_LUMINOSITY = $92B0;
  GL_DEBUG_OUTPUT_SYNCHRONOUS = $8242;
  GL_DEBUG_NEXT_LOGGED_MESSAGE_LENGTH = $8243;
  GL_DEBUG_CALLBACK_FUNCTION = $8244;
  GL_DEBUG_CALLBACK_USER_PARAM = $8245;
  GL_DEBUG_SOURCE_API = $8246;
  GL_DEBUG_SOURCE_WINDOW_SYSTEM = $8247;
  GL_DEBUG_SOURCE_SHADER_COMPILER = $8248;
  GL_DEBUG_SOURCE_THIRD_PARTY = $8249;
  GL_DEBUG_SOURCE_APPLICATION = $824A;
  GL_DEBUG_SOURCE_OTHER = $824B;
  GL_DEBUG_TYPE_ERROR = $824C;
  GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR = $824D;
  GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR = $824E;
  GL_DEBUG_TYPE_PORTABILITY = $824F;
  GL_DEBUG_TYPE_PERFORMANCE = $8250;
  GL_DEBUG_TYPE_OTHER = $8251;
  GL_DEBUG_TYPE_MARKER = $8268;
  GL_DEBUG_TYPE_PUSH_GROUP = $8269;
  GL_DEBUG_TYPE_POP_GROUP = $826A;
  GL_DEBUG_SEVERITY_NOTIFICATION = $826B;
  GL_MAX_DEBUG_GROUP_STACK_DEPTH = $826C;
  GL_DEBUG_GROUP_STACK_DEPTH = $826D;
  GL_BUFFER = $82E0;
  GL_SHADER = $82E1;
  GL_PROGRAM = $82E2;
  GL_VERTEX_ARRAY = $8074;
  GL_QUERY = $82E3;
  GL_PROGRAM_PIPELINE = $82E4;
  GL_SAMPLER = $82E6;
  GL_MAX_LABEL_LENGTH = $82E8;
  GL_MAX_DEBUG_MESSAGE_LENGTH = $9143;
  GL_MAX_DEBUG_LOGGED_MESSAGES = $9144;
  GL_DEBUG_LOGGED_MESSAGES = $9145;
  GL_DEBUG_SEVERITY_HIGH = $9146;
  GL_DEBUG_SEVERITY_MEDIUM = $9147;
  GL_DEBUG_SEVERITY_LOW = $9148;
  GL_DEBUG_OUTPUT = $92E0;
  GL_CONTEXT_FLAG_DEBUG_BIT = $00000002;
  GL_STACK_OVERFLOW = $0503;
  GL_STACK_UNDERFLOW = $0504;
  GL_GEOMETRY_SHADER = $8DD9;
  GL_GEOMETRY_SHADER_BIT = $00000004;
  GL_GEOMETRY_VERTICES_OUT = $8916;
  GL_GEOMETRY_INPUT_TYPE = $8917;
  GL_GEOMETRY_OUTPUT_TYPE = $8918;
  GL_GEOMETRY_SHADER_INVOCATIONS = $887F;
  GL_LAYER_PROVOKING_VERTEX = $825E;
  GL_LINES_ADJACENCY = $000A;
  GL_LINE_STRIP_ADJACENCY = $000B;
  GL_TRIANGLES_ADJACENCY = $000C;
  GL_TRIANGLE_STRIP_ADJACENCY = $000D;
  GL_MAX_GEOMETRY_UNIFORM_COMPONENTS = $8DDF;
  GL_MAX_GEOMETRY_UNIFORM_BLOCKS = $8A2C;
  GL_MAX_COMBINED_GEOMETRY_UNIFORM_COMPONENTS = $8A32;
  GL_MAX_GEOMETRY_INPUT_COMPONENTS = $9123;
  GL_MAX_GEOMETRY_OUTPUT_COMPONENTS = $9124;
  GL_MAX_GEOMETRY_OUTPUT_VERTICES = $8DE0;
  GL_MAX_GEOMETRY_TOTAL_OUTPUT_COMPONENTS = $8DE1;
  GL_MAX_GEOMETRY_SHADER_INVOCATIONS = $8E5A;
  GL_MAX_GEOMETRY_TEXTURE_IMAGE_UNITS = $8C29;
  GL_MAX_GEOMETRY_ATOMIC_COUNTER_BUFFERS = $92CF;
  GL_MAX_GEOMETRY_ATOMIC_COUNTERS = $92D5;
  GL_MAX_GEOMETRY_IMAGE_UNIFORMS = $90CD;
  GL_MAX_GEOMETRY_SHADER_STORAGE_BLOCKS = $90D7;
  GL_FIRST_VERTEX_CONVENTION = $8E4D;
  GL_LAST_VERTEX_CONVENTION = $8E4E;
  GL_UNDEFINED_VERTEX = $8260;
  GL_PRIMITIVES_GENERATED = $8C87;
  GL_FRAMEBUFFER_DEFAULT_LAYERS = $9312;
  GL_MAX_FRAMEBUFFER_LAYERS = $9317;
  GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS = $8DA8;
  GL_FRAMEBUFFER_ATTACHMENT_LAYERED = $8DA7;
  GL_REFERENCED_BY_GEOMETRY_SHADER = $9309;
  GL_PRIMITIVE_BOUNDING_BOX = $92BE;
  GL_CONTEXT_FLAG_ROBUST_ACCESS_BIT = $00000004;
  GL_CONTEXT_FLAGS = $821E;
  GL_LOSE_CONTEXT_ON_RESET = $8252;
  GL_GUILTY_CONTEXT_RESET = $8253;
  GL_INNOCENT_CONTEXT_RESET = $8254;
  GL_UNKNOWN_CONTEXT_RESET = $8255;
  GL_RESET_NOTIFICATION_STRATEGY = $8256;
  GL_NO_RESET_NOTIFICATION = $8261;
  GL_CONTEXT_LOST = $0507;
  GL_SAMPLE_SHADING = $8C36;
  GL_MIN_SAMPLE_SHADING_VALUE = $8C37;
  GL_MIN_FRAGMENT_INTERPOLATION_OFFSET = $8E5B;
  GL_MAX_FRAGMENT_INTERPOLATION_OFFSET = $8E5C;
  GL_FRAGMENT_INTERPOLATION_OFFSET_BITS = $8E5D;
  GL_PATCHES = $000E;
  GL_PATCH_VERTICES = $8E72;
  GL_TESS_CONTROL_OUTPUT_VERTICES = $8E75;
  GL_TESS_GEN_MODE = $8E76;
  GL_TESS_GEN_SPACING = $8E77;
  GL_TESS_GEN_VERTEX_ORDER = $8E78;
  GL_TESS_GEN_POINT_MODE = $8E79;
  GL_ISOLINES = $8E7A;
  GL_QUADS = $0007;
  GL_FRACTIONAL_ODD = $8E7B;
  GL_FRACTIONAL_EVEN = $8E7C;
  GL_MAX_PATCH_VERTICES = $8E7D;
  GL_MAX_TESS_GEN_LEVEL = $8E7E;
  GL_MAX_TESS_CONTROL_UNIFORM_COMPONENTS = $8E7F;
  GL_MAX_TESS_EVALUATION_UNIFORM_COMPONENTS = $8E80;
  GL_MAX_TESS_CONTROL_TEXTURE_IMAGE_UNITS = $8E81;
  GL_MAX_TESS_EVALUATION_TEXTURE_IMAGE_UNITS = $8E82;
  GL_MAX_TESS_CONTROL_OUTPUT_COMPONENTS = $8E83;
  GL_MAX_TESS_PATCH_COMPONENTS = $8E84;
  GL_MAX_TESS_CONTROL_TOTAL_OUTPUT_COMPONENTS = $8E85;
  GL_MAX_TESS_EVALUATION_OUTPUT_COMPONENTS = $8E86;
  GL_MAX_TESS_CONTROL_UNIFORM_BLOCKS = $8E89;
  GL_MAX_TESS_EVALUATION_UNIFORM_BLOCKS = $8E8A;
  GL_MAX_TESS_CONTROL_INPUT_COMPONENTS = $886C;
  GL_MAX_TESS_EVALUATION_INPUT_COMPONENTS = $886D;
  GL_MAX_COMBINED_TESS_CONTROL_UNIFORM_COMPONENTS = $8E1E;
  GL_MAX_COMBINED_TESS_EVALUATION_UNIFORM_COMPONENTS = $8E1F;
  GL_MAX_TESS_CONTROL_ATOMIC_COUNTER_BUFFERS = $92CD;
  GL_MAX_TESS_EVALUATION_ATOMIC_COUNTER_BUFFERS = $92CE;
  GL_MAX_TESS_CONTROL_ATOMIC_COUNTERS = $92D3;
  GL_MAX_TESS_EVALUATION_ATOMIC_COUNTERS = $92D4;
  GL_MAX_TESS_CONTROL_IMAGE_UNIFORMS = $90CB;
  GL_MAX_TESS_EVALUATION_IMAGE_UNIFORMS = $90CC;
  GL_MAX_TESS_CONTROL_SHADER_STORAGE_BLOCKS = $90D8;
  GL_MAX_TESS_EVALUATION_SHADER_STORAGE_BLOCKS = $90D9;
  GL_PRIMITIVE_RESTART_FOR_PATCHES_SUPPORTED = $8221;
  GL_IS_PER_PATCH = $92E7;
  GL_REFERENCED_BY_TESS_CONTROL_SHADER = $9307;
  GL_REFERENCED_BY_TESS_EVALUATION_SHADER = $9308;
  GL_TESS_CONTROL_SHADER = $8E88;
  GL_TESS_EVALUATION_SHADER = $8E87;
  GL_TESS_CONTROL_SHADER_BIT = $00000008;
  GL_TESS_EVALUATION_SHADER_BIT = $00000010;
  GL_TEXTURE_BORDER_COLOR = $1004;
  GL_CLAMP_TO_BORDER = $812D;
  GL_TEXTURE_BUFFER = $8C2A;
  GL_TEXTURE_BUFFER_BINDING = $8C2A;
  GL_MAX_TEXTURE_BUFFER_SIZE = $8C2B;
  GL_TEXTURE_BINDING_BUFFER = $8C2C;
  GL_TEXTURE_BUFFER_DATA_STORE_BINDING = $8C2D;
  GL_TEXTURE_BUFFER_OFFSET_ALIGNMENT = $919F;
  GL_SAMPLER_BUFFER = $8DC2;
  GL_INT_SAMPLER_BUFFER = $8DD0;
  GL_UNSIGNED_INT_SAMPLER_BUFFER = $8DD8;
  GL_IMAGE_BUFFER = $9051;
  GL_INT_IMAGE_BUFFER = $905C;
  GL_UNSIGNED_INT_IMAGE_BUFFER = $9067;
  GL_TEXTURE_BUFFER_OFFSET = $919D;
  GL_TEXTURE_BUFFER_SIZE = $919E;
  GL_COMPRESSED_RGBA_ASTC_4x4 = $93B0;
  GL_COMPRESSED_RGBA_ASTC_5x4 = $93B1;
  GL_COMPRESSED_RGBA_ASTC_5x5 = $93B2;
  GL_COMPRESSED_RGBA_ASTC_6x5 = $93B3;
  GL_COMPRESSED_RGBA_ASTC_6x6 = $93B4;
  GL_COMPRESSED_RGBA_ASTC_8x5 = $93B5;
  GL_COMPRESSED_RGBA_ASTC_8x6 = $93B6;
  GL_COMPRESSED_RGBA_ASTC_8x8 = $93B7;
  GL_COMPRESSED_RGBA_ASTC_10x5 = $93B8;
  GL_COMPRESSED_RGBA_ASTC_10x6 = $93B9;
  GL_COMPRESSED_RGBA_ASTC_10x8 = $93BA;
  GL_COMPRESSED_RGBA_ASTC_10x10 = $93BB;
  GL_COMPRESSED_RGBA_ASTC_12x10 = $93BC;
  GL_COMPRESSED_RGBA_ASTC_12x12 = $93BD;
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_4x4 = $93D0;
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x4 = $93D1;
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x5 = $93D2;
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x5 = $93D3;
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x6 = $93D4;
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x5 = $93D5;
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x6 = $93D6;
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x8 = $93D7;
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x5 = $93D8;
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x6 = $93D9;
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x8 = $93DA;
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x10 = $93DB;
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_12x10 = $93DC;
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_12x12 = $93DD;
  GL_TEXTURE_CUBE_MAP_ARRAY = $9009;
  GL_TEXTURE_BINDING_CUBE_MAP_ARRAY = $900A;
  GL_SAMPLER_CUBE_MAP_ARRAY = $900C;
  GL_SAMPLER_CUBE_MAP_ARRAY_SHADOW = $900D;
  GL_INT_SAMPLER_CUBE_MAP_ARRAY = $900E;
  GL_UNSIGNED_INT_SAMPLER_CUBE_MAP_ARRAY = $900F;
  GL_IMAGE_CUBE_MAP_ARRAY = $9054;
  GL_INT_IMAGE_CUBE_MAP_ARRAY = $905F;
  GL_UNSIGNED_INT_IMAGE_CUBE_MAP_ARRAY = $906A;
  GL_TEXTURE_2D_MULTISAMPLE_ARRAY = $9102;
  GL_TEXTURE_BINDING_2D_MULTISAMPLE_ARRAY = $9105;
  GL_SAMPLER_2D_MULTISAMPLE_ARRAY = $910B;
  GL_INT_SAMPLER_2D_MULTISAMPLE_ARRAY = $910C;
  GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE_ARRAY = $910D;

var
  glBlendBarrier: procedure; apicall;
  glCopyImageSubData: procedure(srcName: GLuint; srcTarget: GLenum; srcLevel: GLint; srcX: GLint; srcY: GLint; srcZ: GLint; dstName: GLuint; dstTarget: GLenum; dstLevel: GLint; dstX: GLint; dstY: GLint; dstZ: GLint; srcWidth: GLsizei; srcHeight: GLsizei; srcDepth: GLsizei); apicall;
  glDebugMessageControl: procedure(source: GLenum; type_: GLenum; severity: GLenum; count: GLsizei; ids: PGLuint; enabled: GLboolean); apicall;
  glDebugMessageInsert: procedure(source: GLenum; type_: GLenum; id: GLuint; severity: GLenum; length: GLsizei; buf: PGLchar); apicall;
  glDebugMessageCallback: procedure(callback: GLDEBUGPROC; userParam: Pointer); apicall;
  glGetDebugMessageLog: function(count: GLuint; bufSize: GLsizei; sources: PGLenum; types: PGLenum; ids: PGLuint; severities: PGLenum; lengths: PGLsizei; messageLog: PGLchar): GLuint; apicall;
  glPushDebugGroup: procedure(source: GLenum; id: GLuint; length: GLsizei; message: PGLchar); apicall;
  glPopDebugGroup: procedure; apicall;
  glObjectLabel: procedure(identifier: GLenum; name: GLuint; length: GLsizei; label_: PGLchar); apicall;
  glGetObjectLabel: procedure(identifier: GLenum; name: GLuint; bufSize: GLsizei; length: PGLsizei; label_: PGLchar); apicall;
  glObjectPtrLabel: procedure(ptr: Pointer; length: GLsizei; label_: PGLchar); apicall;
  glGetObjectPtrLabel: procedure(ptr: Pointer; bufSize: GLsizei; length: PGLsizei; label_: PGLchar); apicall;
  glGetPointerv: procedure(pname: GLenum; params: PPointer); apicall;
  glEnablei: procedure(target: GLenum; index: GLuint); apicall;
  glDisablei: procedure(target: GLenum; index: GLuint); apicall;
  glBlendEquationi: procedure(buf: GLuint; mode: GLenum); apicall;
  glBlendEquationSeparatei: procedure(buf: GLuint; modeRGB: GLenum; modeAlpha: GLenum); apicall;
  glBlendFunci: procedure(buf: GLuint; src: GLenum; dst: GLenum); apicall;
  glBlendFuncSeparatei: procedure(buf: GLuint; srcRGB: GLenum; dstRGB: GLenum; srcAlpha: GLenum; dstAlpha: GLenum); apicall;
  glColorMaski: procedure(index: GLuint; r: GLboolean; g: GLboolean; b: GLboolean; a: GLboolean); apicall;
  glIsEnabledi: function(target: GLenum; index: GLuint): GLboolean; apicall;
  glDrawElementsBaseVertex: procedure(mode: GLenum; count: GLsizei; type_: GLenum; indices: Pointer; basevertex: GLint); apicall;
  glDrawRangeElementsBaseVertex: procedure(mode: GLenum; start: GLuint; end_: GLuint; count: GLsizei; type_: GLenum; indices: Pointer; basevertex: GLint); apicall;
  glDrawElementsInstancedBaseVertex: procedure(mode: GLenum; count: GLsizei; type_: GLenum; indices: Pointer; instancecount: GLsizei; basevertex: GLint); apicall;
  glFramebufferTexture: procedure(target: GLenum; attachment: GLenum; texture: GLuint; level: GLint); apicall;
  glPrimitiveBoundingBox: procedure(minX: GLfloat; minY: GLfloat; minZ: GLfloat; minW: GLfloat; maxX: GLfloat; maxY: GLfloat; maxZ: GLfloat; maxW: GLfloat); apicall;
  glGetGraphicsResetStatus: function: GLenum; apicall;
  glReadnPixels: procedure(x: GLint; y: GLint; width: GLsizei; height: GLsizei; format: GLenum; type_: GLenum; bufSize: GLsizei; data: Pointer); apicall;
  glGetnUniformfv: procedure(program_: GLuint; location: GLint; bufSize: GLsizei; params: PGLfloat); apicall;
  glGetnUniformiv: procedure(program_: GLuint; location: GLint; bufSize: GLsizei; params: PGLint); apicall;
  glGetnUniformuiv: procedure(program_: GLuint; location: GLint; bufSize: GLsizei; params: PGLuint); apicall;
  glMinSampleShading: procedure(value: GLfloat); apicall;
  glPatchParameteri: procedure(pname: GLenum; value: GLint); apicall;
  glTexParameterIiv: procedure(target: GLenum; pname: GLenum; params: PGLint); apicall;
  glTexParameterIuiv: procedure(target: GLenum; pname: GLenum; params: PGLuint); apicall;
  glGetTexParameterIiv: procedure(target: GLenum; pname: GLenum; params: PGLint); apicall;
  glGetTexParameterIuiv: procedure(target: GLenum; pname: GLenum; params: PGLuint); apicall;
  glSamplerParameterIiv: procedure(sampler: GLuint; pname: GLenum; param: PGLint); apicall;
  glSamplerParameterIuiv: procedure(sampler: GLuint; pname: GLenum; param: PGLuint); apicall;
  glGetSamplerParameterIiv: procedure(sampler: GLuint; pname: GLenum; params: PGLint); apicall;
  glGetSamplerParameterIuiv: procedure(sampler: GLuint; pname: GLenum; params: PGLuint); apicall;
  glTexBuffer: procedure(target: GLenum; internalformat: GLenum; buffer: GLuint); apicall;
  glTexBufferRange: procedure(target: GLenum; internalformat: GLenum; buffer: GLuint; offset: GLintptr; size: GLsizeiptr); apicall;
  glTexStorage3DMultisample: procedure(target: GLenum; samples: GLsizei; internalformat: GLenum; width: GLsizei; height: GLsizei; depth: GLsizei; fixedsamplelocations: GLboolean); apicall;
{$endif}
{$endregion}
{$else}
{ OpenGL 1.0 }

{$region gl10}
const
  GL_DEPTH_BUFFER_BIT = $00000100;
  GL_STENCIL_BUFFER_BIT = $00000400;
  GL_COLOR_BUFFER_BIT = $00004000;
  GL_FALSE = 0;
  GL_TRUE = 1;
  GL_POINTS = $0000;
  GL_LINES = $0001;
  GL_LINE_LOOP = $0002;
  GL_LINE_STRIP = $0003;
  GL_TRIANGLES = $0004;
  GL_TRIANGLE_STRIP = $0005;
  GL_TRIANGLE_FAN = $0006;
  GL_NEVER = $0200;
  GL_LESS = $0201;
  GL_EQUAL = $0202;
  GL_LEQUAL = $0203;
  GL_GREATER = $0204;
  GL_NOTEQUAL = $0205;
  GL_GEQUAL = $0206;
  GL_ALWAYS = $0207;
  GL_ZERO = 0;
  GL_ONE = 1;
  GL_SRC_COLOR = $0300;
  GL_ONE_MINUS_SRC_COLOR = $0301;
  GL_SRC_ALPHA = $0302;
  GL_ONE_MINUS_SRC_ALPHA = $0303;
  GL_DST_ALPHA = $0304;
  GL_ONE_MINUS_DST_ALPHA = $0305;
  GL_DST_COLOR = $0306;
  GL_ONE_MINUS_DST_COLOR = $0307;
  GL_SRC_ALPHA_SATURATE = $0308;
  GL_NONE = 0;
  GL_FRONT_LEFT = $0400;
  GL_FRONT_RIGHT = $0401;
  GL_BACK_LEFT = $0402;
  GL_BACK_RIGHT = $0403;
  GL_FRONT = $0404;
  GL_BACK = $0405;
  GL_LEFT = $0406;
  GL_RIGHT = $0407;
  GL_FRONT_AND_BACK = $0408;
  GL_NO_ERROR = 0;
  GL_INVALID_ENUM = $0500;
  GL_INVALID_VALUE = $0501;
  GL_INVALID_OPERATION = $0502;
  GL_OUT_OF_MEMORY = $0505;
  GL_CW = $0900;
  GL_CCW = $0901;
  GL_POINT_SIZE = $0B11;
  GL_POINT_SIZE_RANGE = $0B12;
  GL_POINT_SIZE_GRANULARITY = $0B13;
  GL_LINE_SMOOTH = $0B20;
  GL_LINE_WIDTH = $0B21;
  GL_LINE_WIDTH_RANGE = $0B22;
  GL_LINE_WIDTH_GRANULARITY = $0B23;
  GL_POLYGON_MODE = $0B40;
  GL_POLYGON_SMOOTH = $0B41;
  GL_CULL_FACE = $0B44;
  GL_CULL_FACE_MODE = $0B45;
  GL_FRONT_FACE = $0B46;
  GL_DEPTH_RANGE = $0B70;
  GL_DEPTH_TEST = $0B71;
  GL_DEPTH_WRITEMASK = $0B72;
  GL_DEPTH_CLEAR_VALUE = $0B73;
  GL_DEPTH_FUNC = $0B74;
  GL_STENCIL_TEST = $0B90;
  GL_STENCIL_CLEAR_VALUE = $0B91;
  GL_STENCIL_FUNC = $0B92;
  GL_STENCIL_VALUE_MASK = $0B93;
  GL_STENCIL_FAIL = $0B94;
  GL_STENCIL_PASS_DEPTH_FAIL = $0B95;
  GL_STENCIL_PASS_DEPTH_PASS = $0B96;
  GL_STENCIL_REF = $0B97;
  GL_STENCIL_WRITEMASK = $0B98;
  GL_VIEWPORT = $0BA2;
  GL_DITHER = $0BD0;
  GL_BLEND_DST = $0BE0;
  GL_BLEND_SRC = $0BE1;
  GL_BLEND = $0BE2;
  GL_LOGIC_OP_MODE = $0BF0;
  GL_DRAW_BUFFER = $0C01;
  GL_READ_BUFFER = $0C02;
  GL_SCISSOR_BOX = $0C10;
  GL_SCISSOR_TEST = $0C11;
  GL_COLOR_CLEAR_VALUE = $0C22;
  GL_COLOR_WRITEMASK = $0C23;
  GL_DOUBLEBUFFER = $0C32;
  GL_STEREO = $0C33;
  GL_LINE_SMOOTH_HINT = $0C52;
  GL_POLYGON_SMOOTH_HINT = $0C53;
  GL_UNPACK_SWAP_BYTES = $0CF0;
  GL_UNPACK_LSB_FIRST = $0CF1;
  GL_UNPACK_ROW_LENGTH = $0CF2;
  GL_UNPACK_SKIP_ROWS = $0CF3;
  GL_UNPACK_SKIP_PIXELS = $0CF4;
  GL_UNPACK_ALIGNMENT = $0CF5;
  GL_PACK_SWAP_BYTES = $0D00;
  GL_PACK_LSB_FIRST = $0D01;
  GL_PACK_ROW_LENGTH = $0D02;
  GL_PACK_SKIP_ROWS = $0D03;
  GL_PACK_SKIP_PIXELS = $0D04;
  GL_PACK_ALIGNMENT = $0D05;
  GL_MAX_TEXTURE_SIZE = $0D33;
  GL_MAX_VIEWPORT_DIMS = $0D3A;
  GL_SUBPIXEL_BITS = $0D50;
  GL_TEXTURE_1D = $0DE0;
  GL_TEXTURE_2D = $0DE1;
  GL_TEXTURE_WIDTH = $1000;
  GL_TEXTURE_HEIGHT = $1001;
  GL_TEXTURE_BORDER_COLOR = $1004;
  GL_DONT_CARE = $1100;
  GL_FASTEST = $1101;
  GL_NICEST = $1102;
  GL_BYTE = $1400;
  GL_UNSIGNED_BYTE = $1401;
  GL_SHORT = $1402;
  GL_UNSIGNED_SHORT = $1403;
  GL_INT = $1404;
  GL_UNSIGNED_INT = $1405;
  GL_FLOAT = $1406;
  GL_CLEAR = $1500;
  GL_AND = $1501;
  GL_AND_REVERSE = $1502;
  GL_COPY = $1503;
  GL_AND_INVERTED = $1504;
  GL_NOOP = $1505;
  GL_XOR = $1506;
  GL_OR = $1507;
  GL_NOR = $1508;
  GL_EQUIV = $1509;
  GL_INVERT = $150A;
  GL_OR_REVERSE = $150B;
  GL_COPY_INVERTED = $150C;
  GL_OR_INVERTED = $150D;
  GL_NAND = $150E;
  GL_SET = $150F;
  GL_TEXTURE = $1702;
  GL_COLOR = $1800;
  GL_DEPTH = $1801;
  GL_STENCIL = $1802;
  GL_STENCIL_INDEX = $1901;
  GL_DEPTH_COMPONENT = $1902;
  GL_RED = $1903;
  GL_GREEN = $1904;
  GL_BLUE = $1905;
  GL_ALPHA = $1906;
  GL_RGB = $1907;
  GL_RGBA = $1908;
  GL_POINT = $1B00;
  GL_LINE = $1B01;
  GL_FILL = $1B02;
  GL_KEEP = $1E00;
  GL_REPLACE = $1E01;
  GL_INCR = $1E02;
  GL_DECR = $1E03;
  GL_VENDOR = $1F00;
  GL_RENDERER = $1F01;
  GL_VERSION = $1F02;
  GL_EXTENSIONS = $1F03;
  GL_NEAREST = $2600;
  GL_LINEAR = $2601;
  GL_NEAREST_MIPMAP_NEAREST = $2700;
  GL_LINEAR_MIPMAP_NEAREST = $2701;
  GL_NEAREST_MIPMAP_LINEAR = $2702;
  GL_LINEAR_MIPMAP_LINEAR = $2703;
  GL_TEXTURE_MAG_FILTER = $2800;
  GL_TEXTURE_MIN_FILTER = $2801;
  GL_TEXTURE_WRAP_S = $2802;
  GL_TEXTURE_WRAP_T = $2803;
  GL_REPEAT = $2901;

var
  glCullFace: procedure(mode: GLenum); apicall;
  glFrontFace: procedure(mode: GLenum); apicall;
  glHint: procedure(target: GLenum; mode: GLenum); apicall;
  glLineWidth: procedure(width: GLfloat); apicall;
  glPointSize: procedure(size: GLfloat); apicall;
  glPolygonMode: procedure(face: GLenum; mode: GLenum); apicall;
  glScissor: procedure(x: GLint; y: GLint; width: GLsizei; height: GLsizei); apicall;
  glTexParameterf: procedure(target: GLenum; pname: GLenum; param: GLfloat); apicall;
  glTexParameterfv: procedure(target: GLenum; pname: GLenum; params: PGLfloat); apicall;
  glTexParameteri: procedure(target: GLenum; pname: GLenum; param: GLint); apicall;
  glTexParameteriv: procedure(target: GLenum; pname: GLenum; params: PGLint); apicall;
  glTexImage1D: procedure(target: GLenum; level: GLint; internalformat: GLint; width: GLsizei; border: GLint; format: GLenum; type_: GLenum; pixels: Pointer); apicall;
  glTexImage2D: procedure(target: GLenum; level: GLint; internalformat: GLint; width: GLsizei; height: GLsizei; border: GLint; format: GLenum; type_: GLenum; pixels: Pointer); apicall;
  glDrawBuffer: procedure(buf: GLenum); apicall;
  glClear: procedure(mask: GLbitfield); apicall;
  glClearColor: procedure(red: GLfloat; green: GLfloat; blue: GLfloat; alpha: GLfloat); apicall;
  glClearStencil: procedure(s: GLint); apicall;
  glClearDepth: procedure(depth: GLdouble); apicall;
  glStencilMask: procedure(mask: GLuint); apicall;
  glColorMask: procedure(red: GLboolean; green: GLboolean; blue: GLboolean; alpha: GLboolean); apicall;
  glDepthMask: procedure(flag: GLboolean); apicall;
  glDisable: procedure(cap: GLenum); apicall;
  glEnable: procedure(cap: GLenum); apicall;
  glFinish: procedure; apicall;
  glFlush: procedure; apicall;
  glBlendFunc: procedure(sfactor: GLenum; dfactor: GLenum); apicall;
  glLogicOp: procedure(opcode: GLenum); apicall;
  glStencilFunc: procedure(func: GLenum; ref: GLint; mask: GLuint); apicall;
  glStencilOp: procedure(fail: GLenum; zfail: GLenum; zpass: GLenum); apicall;
  glDepthFunc: procedure(func: GLenum); apicall;
  glPixelStoref: procedure(pname: GLenum; param: GLfloat); apicall;
  glPixelStorei: procedure(pname: GLenum; param: GLint); apicall;
  glReadBuffer: procedure(src: GLenum); apicall;
  glReadPixels: procedure(x: GLint; y: GLint; width: GLsizei; height: GLsizei; format: GLenum; type_: GLenum; pixels: Pointer); apicall;
  glGetBooleanv: procedure(pname: GLenum; data: PGLboolean); apicall;
  glGetDoublev: procedure(pname: GLenum; data: PGLdouble); apicall;
  glGetError: function: GLenum; apicall;
  glGetFloatv: procedure(pname: GLenum; data: PGLfloat); apicall;
  glGetIntegerv: procedure(pname: GLenum; data: PGLint); apicall;
  glGetString: function(name: GLenum): PGLubyte; apicall;
  glGetTexImage: procedure(target: GLenum; level: GLint; format: GLenum; type_: GLenum; pixels: Pointer); apicall;
  glGetTexParameterfv: procedure(target: GLenum; pname: GLenum; params: PGLfloat); apicall;
  glGetTexParameteriv: procedure(target: GLenum; pname: GLenum; params: PGLint); apicall;
  glGetTexLevelParameterfv: procedure(target: GLenum; level: GLint; pname: GLenum; params: PGLfloat); apicall;
  glGetTexLevelParameteriv: procedure(target: GLenum; level: GLint; pname: GLenum; params: PGLint); apicall;
  glIsEnabled: function(cap: GLenum): GLboolean; apicall;
  glDepthRange: procedure(n: GLdouble; f: GLdouble); apicall;
  glViewport: procedure(x: GLint; y: GLint; width: GLsizei; height: GLsizei); apicall;
{$endregion}

{ OpenGL 1.0 compatibility profile }

{$region gl10 compatibility}
{$ifdef glcompat}
const
  GL_CURRENT_BIT = $00000001;
  GL_POINT_BIT = $00000002;
  GL_LINE_BIT = $00000004;
  GL_POLYGON_BIT = $00000008;
  GL_POLYGON_STIPPLE_BIT = $00000010;
  GL_PIXEL_MODE_BIT = $00000020;
  GL_LIGHTING_BIT = $00000040;
  GL_FOG_BIT = $00000080;
  GL_ACCUM_BUFFER_BIT = $00000200;
  GL_VIEWPORT_BIT = $00000800;
  GL_TRANSFORM_BIT = $00001000;
  GL_ENABLE_BIT = $00002000;
  GL_HINT_BIT = $00008000;
  GL_EVAL_BIT = $00010000;
  GL_LIST_BIT = $00020000;
  GL_TEXTURE_BIT = $00040000;
  GL_SCISSOR_BIT = $00080000;
  GL_ALL_ATTRIB_BITS = $FFFFFFFF;
  GL_QUAD_STRIP = $0008;
  GL_QUADS = $0007;
  GL_POLYGON = $0009;
  GL_ACCUM = $0100;
  GL_LOAD = $0101;
  GL_RETURN = $0102;
  GL_MULT = $0103;
  GL_ADD = $0104;
  GL_STACK_OVERFLOW = $0503;
  GL_STACK_UNDERFLOW = $0504;
  GL_AUX0 = $0409;
  GL_AUX1 = $040A;
  GL_AUX2 = $040B;
  GL_AUX3 = $040C;
  GL_2D = $0600;
  GL_3D = $0601;
  GL_3D_COLOR = $0602;
  GL_3D_COLOR_TEXTURE = $0603;
  GL_4D_COLOR_TEXTURE = $0604;
  GL_PASS_THROUGH_TOKEN = $0700;
  GL_POINT_TOKEN = $0701;
  GL_LINE_TOKEN = $0702;
  GL_POLYGON_TOKEN = $0703;
  GL_BITMAP_TOKEN = $0704;
  GL_DRAW_PIXEL_TOKEN = $0705;
  GL_COPY_PIXEL_TOKEN = $0706;
  GL_LINE_RESET_TOKEN = $0707;
  GL_EXP = $0800;
  GL_EXP2 = $0801;
  GL_COEFF = $0A00;
  GL_ORDER = $0A01;
  GL_DOMAIN = $0A02;
  GL_PIXEL_MAP_I_TO_I = $0C70;
  GL_PIXEL_MAP_S_TO_S = $0C71;
  GL_PIXEL_MAP_I_TO_R = $0C72;
  GL_PIXEL_MAP_I_TO_G = $0C73;
  GL_PIXEL_MAP_I_TO_B = $0C74;
  GL_PIXEL_MAP_I_TO_A = $0C75;
  GL_PIXEL_MAP_R_TO_R = $0C76;
  GL_PIXEL_MAP_G_TO_G = $0C77;
  GL_PIXEL_MAP_B_TO_B = $0C78;
  GL_PIXEL_MAP_A_TO_A = $0C79;
  GL_CURRENT_COLOR = $0B00;
  GL_CURRENT_INDEX = $0B01;
  GL_CURRENT_NORMAL = $0B02;
  GL_CURRENT_TEXTURE_COORDS = $0B03;
  GL_CURRENT_RASTER_COLOR = $0B04;
  GL_CURRENT_RASTER_INDEX = $0B05;
  GL_CURRENT_RASTER_TEXTURE_COORDS = $0B06;
  GL_CURRENT_RASTER_POSITION = $0B07;
  GL_CURRENT_RASTER_POSITION_VALID = $0B08;
  GL_CURRENT_RASTER_DISTANCE = $0B09;
  GL_POINT_SMOOTH = $0B10;
  GL_LINE_STIPPLE = $0B24;
  GL_LINE_STIPPLE_PATTERN = $0B25;
  GL_LINE_STIPPLE_REPEAT = $0B26;
  GL_LIST_MODE = $0B30;
  GL_MAX_LIST_NESTING = $0B31;
  GL_LIST_BASE = $0B32;
  GL_LIST_INDEX = $0B33;
  GL_POLYGON_STIPPLE = $0B42;
  GL_EDGE_FLAG = $0B43;
  GL_LIGHTING = $0B50;
  GL_LIGHT_MODEL_LOCAL_VIEWER = $0B51;
  GL_LIGHT_MODEL_TWO_SIDE = $0B52;
  GL_LIGHT_MODEL_AMBIENT = $0B53;
  GL_SHADE_MODEL = $0B54;
  GL_COLOR_MATERIAL_FACE = $0B55;
  GL_COLOR_MATERIAL_PARAMETER = $0B56;
  GL_COLOR_MATERIAL = $0B57;
  GL_FOG = $0B60;
  GL_FOG_INDEX = $0B61;
  GL_FOG_DENSITY = $0B62;
  GL_FOG_START = $0B63;
  GL_FOG_END = $0B64;
  GL_FOG_MODE = $0B65;
  GL_FOG_COLOR = $0B66;
  GL_ACCUM_CLEAR_VALUE = $0B80;
  GL_MATRIX_MODE = $0BA0;
  GL_NORMALIZE = $0BA1;
  GL_MODELVIEW_STACK_DEPTH = $0BA3;
  GL_PROJECTION_STACK_DEPTH = $0BA4;
  GL_TEXTURE_STACK_DEPTH = $0BA5;
  GL_MODELVIEW_MATRIX = $0BA6;
  GL_PROJECTION_MATRIX = $0BA7;
  GL_TEXTURE_MATRIX = $0BA8;
  GL_ATTRIB_STACK_DEPTH = $0BB0;
  GL_ALPHA_TEST = $0BC0;
  GL_ALPHA_TEST_FUNC = $0BC1;
  GL_ALPHA_TEST_REF = $0BC2;
  GL_LOGIC_OP = $0BF1;
  GL_AUX_BUFFERS = $0C00;
  GL_INDEX_CLEAR_VALUE = $0C20;
  GL_INDEX_WRITEMASK = $0C21;
  GL_INDEX_MODE = $0C30;
  GL_RGBA_MODE = $0C31;
  GL_RENDER_MODE = $0C40;
  GL_PERSPECTIVE_CORRECTION_HINT = $0C50;
  GL_POINT_SMOOTH_HINT = $0C51;
  GL_FOG_HINT = $0C54;
  GL_TEXTURE_GEN_S = $0C60;
  GL_TEXTURE_GEN_T = $0C61;
  GL_TEXTURE_GEN_R = $0C62;
  GL_TEXTURE_GEN_Q = $0C63;
  GL_PIXEL_MAP_I_TO_I_SIZE = $0CB0;
  GL_PIXEL_MAP_S_TO_S_SIZE = $0CB1;
  GL_PIXEL_MAP_I_TO_R_SIZE = $0CB2;
  GL_PIXEL_MAP_I_TO_G_SIZE = $0CB3;
  GL_PIXEL_MAP_I_TO_B_SIZE = $0CB4;
  GL_PIXEL_MAP_I_TO_A_SIZE = $0CB5;
  GL_PIXEL_MAP_R_TO_R_SIZE = $0CB6;
  GL_PIXEL_MAP_G_TO_G_SIZE = $0CB7;
  GL_PIXEL_MAP_B_TO_B_SIZE = $0CB8;
  GL_PIXEL_MAP_A_TO_A_SIZE = $0CB9;
  GL_MAP_COLOR = $0D10;
  GL_MAP_STENCIL = $0D11;
  GL_INDEX_SHIFT = $0D12;
  GL_INDEX_OFFSET = $0D13;
  GL_RED_SCALE = $0D14;
  GL_RED_BIAS = $0D15;
  GL_ZOOM_X = $0D16;
  GL_ZOOM_Y = $0D17;
  GL_GREEN_SCALE = $0D18;
  GL_GREEN_BIAS = $0D19;
  GL_BLUE_SCALE = $0D1A;
  GL_BLUE_BIAS = $0D1B;
  GL_ALPHA_SCALE = $0D1C;
  GL_ALPHA_BIAS = $0D1D;
  GL_DEPTH_SCALE = $0D1E;
  GL_DEPTH_BIAS = $0D1F;
  GL_MAX_EVAL_ORDER = $0D30;
  GL_MAX_LIGHTS = $0D31;
  GL_MAX_CLIP_PLANES = $0D32;
  GL_MAX_PIXEL_MAP_TABLE = $0D34;
  GL_MAX_ATTRIB_STACK_DEPTH = $0D35;
  GL_MAX_MODELVIEW_STACK_DEPTH = $0D36;
  GL_MAX_NAME_STACK_DEPTH = $0D37;
  GL_MAX_PROJECTION_STACK_DEPTH = $0D38;
  GL_MAX_TEXTURE_STACK_DEPTH = $0D39;
  GL_INDEX_BITS = $0D51;
  GL_RED_BITS = $0D52;
  GL_GREEN_BITS = $0D53;
  GL_BLUE_BITS = $0D54;
  GL_ALPHA_BITS = $0D55;
  GL_DEPTH_BITS = $0D56;
  GL_STENCIL_BITS = $0D57;
  GL_ACCUM_RED_BITS = $0D58;
  GL_ACCUM_GREEN_BITS = $0D59;
  GL_ACCUM_BLUE_BITS = $0D5A;
  GL_ACCUM_ALPHA_BITS = $0D5B;
  GL_NAME_STACK_DEPTH = $0D70;
  GL_AUTO_NORMAL = $0D80;
  GL_MAP1_COLOR_4 = $0D90;
  GL_MAP1_INDEX = $0D91;
  GL_MAP1_NORMAL = $0D92;
  GL_MAP1_TEXTURE_COORD_1 = $0D93;
  GL_MAP1_TEXTURE_COORD_2 = $0D94;
  GL_MAP1_TEXTURE_COORD_3 = $0D95;
  GL_MAP1_TEXTURE_COORD_4 = $0D96;
  GL_MAP1_VERTEX_3 = $0D97;
  GL_MAP1_VERTEX_4 = $0D98;
  GL_MAP2_COLOR_4 = $0DB0;
  GL_MAP2_INDEX = $0DB1;
  GL_MAP2_NORMAL = $0DB2;
  GL_MAP2_TEXTURE_COORD_1 = $0DB3;
  GL_MAP2_TEXTURE_COORD_2 = $0DB4;
  GL_MAP2_TEXTURE_COORD_3 = $0DB5;
  GL_MAP2_TEXTURE_COORD_4 = $0DB6;
  GL_MAP2_VERTEX_3 = $0DB7;
  GL_MAP2_VERTEX_4 = $0DB8;
  GL_MAP1_GRID_DOMAIN = $0DD0;
  GL_MAP1_GRID_SEGMENTS = $0DD1;
  GL_MAP2_GRID_DOMAIN = $0DD2;
  GL_MAP2_GRID_SEGMENTS = $0DD3;
  GL_TEXTURE_COMPONENTS = $1003;
  GL_TEXTURE_BORDER = $1005;
  GL_AMBIENT = $1200;
  GL_DIFFUSE = $1201;
  GL_SPECULAR = $1202;
  GL_POSITION = $1203;
  GL_SPOT_DIRECTION = $1204;
  GL_SPOT_EXPONENT = $1205;
  GL_SPOT_CUTOFF = $1206;
  GL_CONSTANT_ATTENUATION = $1207;
  GL_LINEAR_ATTENUATION = $1208;
  GL_QUADRATIC_ATTENUATION = $1209;
  GL_COMPILE = $1300;
  GL_COMPILE_AND_EXECUTE = $1301;
  GL_2_BYTES = $1407;
  GL_3_BYTES = $1408;
  GL_4_BYTES = $1409;
  GL_EMISSION = $1600;
  GL_SHININESS = $1601;
  GL_AMBIENT_AND_DIFFUSE = $1602;
  GL_COLOR_INDEXES = $1603;
  GL_MODELVIEW = $1700;
  GL_PROJECTION = $1701;
  GL_COLOR_INDEX = $1900;
  GL_LUMINANCE = $1909;
  GL_LUMINANCE_ALPHA = $190A;
  GL_BITMAP = $1A00;
  GL_RENDER = $1C00;
  GL_FEEDBACK = $1C01;
  GL_SELECT = $1C02;
  GL_FLAT = $1D00;
  GL_SMOOTH = $1D01;
  GL_S = $2000;
  GL_T = $2001;
  GL_R = $2002;
  GL_Q = $2003;
  GL_MODULATE = $2100;
  GL_DECAL = $2101;
  GL_TEXTURE_ENV_MODE = $2200;
  GL_TEXTURE_ENV_COLOR = $2201;
  GL_TEXTURE_ENV = $2300;
  GL_EYE_LINEAR = $2400;
  GL_OBJECT_LINEAR = $2401;
  GL_SPHERE_MAP = $2402;
  GL_TEXTURE_GEN_MODE = $2500;
  GL_OBJECT_PLANE = $2501;
  GL_EYE_PLANE = $2502;
  GL_CLAMP = $2900;
  GL_CLIP_PLANE0 = $3000;
  GL_CLIP_PLANE1 = $3001;
  GL_CLIP_PLANE2 = $3002;
  GL_CLIP_PLANE3 = $3003;
  GL_CLIP_PLANE4 = $3004;
  GL_CLIP_PLANE5 = $3005;
  GL_LIGHT0 = $4000;
  GL_LIGHT1 = $4001;
  GL_LIGHT2 = $4002;
  GL_LIGHT3 = $4003;
  GL_LIGHT4 = $4004;
  GL_LIGHT5 = $4005;
  GL_LIGHT6 = $4006;
  GL_LIGHT7 = $4007;

var
  glNewList: procedure(list: GLuint; mode: GLenum); apicall;
  glEndList: procedure; apicall;
  glCallList: procedure(list: GLuint); apicall;
  glCallLists: procedure(n: GLsizei; type_: GLenum; lists: Pointer); apicall;
  glDeleteLists: procedure(list: GLuint; range: GLsizei); apicall;
  glGenLists: function(range: GLsizei): GLuint; apicall;
  glListBase: procedure(base: GLuint); apicall;
  glBegin: procedure(mode: GLenum); apicall;
  glBitmap: procedure(width: GLsizei; height: GLsizei; xorig: GLfloat; yorig: GLfloat; xmove: GLfloat; ymove: GLfloat; bitmap: PGLubyte); apicall;
  glColor3b: procedure(red: GLbyte; green: GLbyte; blue: GLbyte); apicall;
  glColor3bv: procedure(v: PGLbyte); apicall;
  glColor3d: procedure(red: GLdouble; green: GLdouble; blue: GLdouble); apicall;
  glColor3dv: procedure(v: PGLdouble); apicall;
  glColor3f: procedure(red: GLfloat; green: GLfloat; blue: GLfloat); apicall;
  glColor3fv: procedure(v: PGLfloat); apicall;
  glColor3i: procedure(red: GLint; green: GLint; blue: GLint); apicall;
  glColor3iv: procedure(v: PGLint); apicall;
  glColor3s: procedure(red: GLshort; green: GLshort; blue: GLshort); apicall;
  glColor3sv: procedure(v: PGLshort); apicall;
  glColor3ub: procedure(red: GLubyte; green: GLubyte; blue: GLubyte); apicall;
  glColor3ubv: procedure(v: PGLubyte); apicall;
  glColor3ui: procedure(red: GLuint; green: GLuint; blue: GLuint); apicall;
  glColor3uiv: procedure(v: PGLuint); apicall;
  glColor3us: procedure(red: GLushort; green: GLushort; blue: GLushort); apicall;
  glColor3usv: procedure(v: PGLushort); apicall;
  glColor4b: procedure(red: GLbyte; green: GLbyte; blue: GLbyte; alpha: GLbyte); apicall;
  glColor4bv: procedure(v: PGLbyte); apicall;
  glColor4d: procedure(red: GLdouble; green: GLdouble; blue: GLdouble; alpha: GLdouble); apicall;
  glColor4dv: procedure(v: PGLdouble); apicall;
  glColor4f: procedure(red: GLfloat; green: GLfloat; blue: GLfloat; alpha: GLfloat); apicall;
  glColor4fv: procedure(v: PGLfloat); apicall;
  glColor4i: procedure(red: GLint; green: GLint; blue: GLint; alpha: GLint); apicall;
  glColor4iv: procedure(v: PGLint); apicall;
  glColor4s: procedure(red: GLshort; green: GLshort; blue: GLshort; alpha: GLshort); apicall;
  glColor4sv: procedure(v: PGLshort); apicall;
  glColor4ub: procedure(red: GLubyte; green: GLubyte; blue: GLubyte; alpha: GLubyte); apicall;
  glColor4ubv: procedure(v: PGLubyte); apicall;
  glColor4ui: procedure(red: GLuint; green: GLuint; blue: GLuint; alpha: GLuint); apicall;
  glColor4uiv: procedure(v: PGLuint); apicall;
  glColor4us: procedure(red: GLushort; green: GLushort; blue: GLushort; alpha: GLushort); apicall;
  glColor4usv: procedure(v: PGLushort); apicall;
  glEdgeFlag: procedure(flag: GLboolean); apicall;
  glEdgeFlagv: procedure(flag: PGLboolean); apicall;
  glEnd: procedure; apicall;
  glIndexd: procedure(c: GLdouble); apicall;
  glIndexdv: procedure(c: PGLdouble); apicall;
  glIndexf: procedure(c: GLfloat); apicall;
  glIndexfv: procedure(c: PGLfloat); apicall;
  glIndexi: procedure(c: GLint); apicall;
  glIndexiv: procedure(c: PGLint); apicall;
  glIndexs: procedure(c: GLshort); apicall;
  glIndexsv: procedure(c: PGLshort); apicall;
  glNormal3b: procedure(nx: GLbyte; ny: GLbyte; nz: GLbyte); apicall;
  glNormal3bv: procedure(v: PGLbyte); apicall;
  glNormal3d: procedure(nx: GLdouble; ny: GLdouble; nz: GLdouble); apicall;
  glNormal3dv: procedure(v: PGLdouble); apicall;
  glNormal3f: procedure(nx: GLfloat; ny: GLfloat; nz: GLfloat); apicall;
  glNormal3fv: procedure(v: PGLfloat); apicall;
  glNormal3i: procedure(nx: GLint; ny: GLint; nz: GLint); apicall;
  glNormal3iv: procedure(v: PGLint); apicall;
  glNormal3s: procedure(nx: GLshort; ny: GLshort; nz: GLshort); apicall;
  glNormal3sv: procedure(v: PGLshort); apicall;
  glRasterPos2d: procedure(x: GLdouble; y: GLdouble); apicall;
  glRasterPos2dv: procedure(v: PGLdouble); apicall;
  glRasterPos2f: procedure(x: GLfloat; y: GLfloat); apicall;
  glRasterPos2fv: procedure(v: PGLfloat); apicall;
  glRasterPos2i: procedure(x: GLint; y: GLint); apicall;
  glRasterPos2iv: procedure(v: PGLint); apicall;
  glRasterPos2s: procedure(x: GLshort; y: GLshort); apicall;
  glRasterPos2sv: procedure(v: PGLshort); apicall;
  glRasterPos3d: procedure(x: GLdouble; y: GLdouble; z: GLdouble); apicall;
  glRasterPos3dv: procedure(v: PGLdouble); apicall;
  glRasterPos3f: procedure(x: GLfloat; y: GLfloat; z: GLfloat); apicall;
  glRasterPos3fv: procedure(v: PGLfloat); apicall;
  glRasterPos3i: procedure(x: GLint; y: GLint; z: GLint); apicall;
  glRasterPos3iv: procedure(v: PGLint); apicall;
  glRasterPos3s: procedure(x: GLshort; y: GLshort; z: GLshort); apicall;
  glRasterPos3sv: procedure(v: PGLshort); apicall;
  glRasterPos4d: procedure(x: GLdouble; y: GLdouble; z: GLdouble; w: GLdouble); apicall;
  glRasterPos4dv: procedure(v: PGLdouble); apicall;
  glRasterPos4f: procedure(x: GLfloat; y: GLfloat; z: GLfloat; w: GLfloat); apicall;
  glRasterPos4fv: procedure(v: PGLfloat); apicall;
  glRasterPos4i: procedure(x: GLint; y: GLint; z: GLint; w: GLint); apicall;
  glRasterPos4iv: procedure(v: PGLint); apicall;
  glRasterPos4s: procedure(x: GLshort; y: GLshort; z: GLshort; w: GLshort); apicall;
  glRasterPos4sv: procedure(v: PGLshort); apicall;
  glRectd: procedure(x1: GLdouble; y1: GLdouble; x2: GLdouble; y2: GLdouble); apicall;
  glRectdv: procedure(v1: PGLdouble; v2: PGLdouble); apicall;
  glRectf: procedure(x1: GLfloat; y1: GLfloat; x2: GLfloat; y2: GLfloat); apicall;
  glRectfv: procedure(v1: PGLfloat; v2: PGLfloat); apicall;
  glRecti: procedure(x1: GLint; y1: GLint; x2: GLint; y2: GLint); apicall;
  glRectiv: procedure(v1: PGLint; v2: PGLint); apicall;
  glRects: procedure(x1: GLshort; y1: GLshort; x2: GLshort; y2: GLshort); apicall;
  glRectsv: procedure(v1: PGLshort; v2: PGLshort); apicall;
  glTexCoord1d: procedure(s: GLdouble); apicall;
  glTexCoord1dv: procedure(v: PGLdouble); apicall;
  glTexCoord1f: procedure(s: GLfloat); apicall;
  glTexCoord1fv: procedure(v: PGLfloat); apicall;
  glTexCoord1i: procedure(s: GLint); apicall;
  glTexCoord1iv: procedure(v: PGLint); apicall;
  glTexCoord1s: procedure(s: GLshort); apicall;
  glTexCoord1sv: procedure(v: PGLshort); apicall;
  glTexCoord2d: procedure(s: GLdouble; t: GLdouble); apicall;
  glTexCoord2dv: procedure(v: PGLdouble); apicall;
  glTexCoord2f: procedure(s: GLfloat; t: GLfloat); apicall;
  glTexCoord2fv: procedure(v: PGLfloat); apicall;
  glTexCoord2i: procedure(s: GLint; t: GLint); apicall;
  glTexCoord2iv: procedure(v: PGLint); apicall;
  glTexCoord2s: procedure(s: GLshort; t: GLshort); apicall;
  glTexCoord2sv: procedure(v: PGLshort); apicall;
  glTexCoord3d: procedure(s: GLdouble; t: GLdouble; r: GLdouble); apicall;
  glTexCoord3dv: procedure(v: PGLdouble); apicall;
  glTexCoord3f: procedure(s: GLfloat; t: GLfloat; r: GLfloat); apicall;
  glTexCoord3fv: procedure(v: PGLfloat); apicall;
  glTexCoord3i: procedure(s: GLint; t: GLint; r: GLint); apicall;
  glTexCoord3iv: procedure(v: PGLint); apicall;
  glTexCoord3s: procedure(s: GLshort; t: GLshort; r: GLshort); apicall;
  glTexCoord3sv: procedure(v: PGLshort); apicall;
  glTexCoord4d: procedure(s: GLdouble; t: GLdouble; r: GLdouble; q: GLdouble); apicall;
  glTexCoord4dv: procedure(v: PGLdouble); apicall;
  glTexCoord4f: procedure(s: GLfloat; t: GLfloat; r: GLfloat; q: GLfloat); apicall;
  glTexCoord4fv: procedure(v: PGLfloat); apicall;
  glTexCoord4i: procedure(s: GLint; t: GLint; r: GLint; q: GLint); apicall;
  glTexCoord4iv: procedure(v: PGLint); apicall;
  glTexCoord4s: procedure(s: GLshort; t: GLshort; r: GLshort; q: GLshort); apicall;
  glTexCoord4sv: procedure(v: PGLshort); apicall;
  glVertex2d: procedure(x: GLdouble; y: GLdouble); apicall;
  glVertex2dv: procedure(v: PGLdouble); apicall;
  glVertex2f: procedure(x: GLfloat; y: GLfloat); apicall;
  glVertex2fv: procedure(v: PGLfloat); apicall;
  glVertex2i: procedure(x: GLint; y: GLint); apicall;
  glVertex2iv: procedure(v: PGLint); apicall;
  glVertex2s: procedure(x: GLshort; y: GLshort); apicall;
  glVertex2sv: procedure(v: PGLshort); apicall;
  glVertex3d: procedure(x: GLdouble; y: GLdouble; z: GLdouble); apicall;
  glVertex3dv: procedure(v: PGLdouble); apicall;
  glVertex3f: procedure(x: GLfloat; y: GLfloat; z: GLfloat); apicall;
  glVertex3fv: procedure(v: PGLfloat); apicall;
  glVertex3i: procedure(x: GLint; y: GLint; z: GLint); apicall;
  glVertex3iv: procedure(v: PGLint); apicall;
  glVertex3s: procedure(x: GLshort; y: GLshort; z: GLshort); apicall;
  glVertex3sv: procedure(v: PGLshort); apicall;
  glVertex4d: procedure(x: GLdouble; y: GLdouble; z: GLdouble; w: GLdouble); apicall;
  glVertex4dv: procedure(v: PGLdouble); apicall;
  glVertex4f: procedure(x: GLfloat; y: GLfloat; z: GLfloat; w: GLfloat); apicall;
  glVertex4fv: procedure(v: PGLfloat); apicall;
  glVertex4i: procedure(x: GLint; y: GLint; z: GLint; w: GLint); apicall;
  glVertex4iv: procedure(v: PGLint); apicall;
  glVertex4s: procedure(x: GLshort; y: GLshort; z: GLshort; w: GLshort); apicall;
  glVertex4sv: procedure(v: PGLshort); apicall;
  glClipPlane: procedure(plane: GLenum; equation: PGLdouble); apicall;
  glColorMaterial: procedure(face: GLenum; mode: GLenum); apicall;
  glFogf: procedure(pname: GLenum; param: GLfloat); apicall;
  glFogfv: procedure(pname: GLenum; params: PGLfloat); apicall;
  glFogi: procedure(pname: GLenum; param: GLint); apicall;
  glFogiv: procedure(pname: GLenum; params: PGLint); apicall;
  glLightf: procedure(light: GLenum; pname: GLenum; param: GLfloat); apicall;
  glLightfv: procedure(light: GLenum; pname: GLenum; params: PGLfloat); apicall;
  glLighti: procedure(light: GLenum; pname: GLenum; param: GLint); apicall;
  glLightiv: procedure(light: GLenum; pname: GLenum; params: PGLint); apicall;
  glLightModelf: procedure(pname: GLenum; param: GLfloat); apicall;
  glLightModelfv: procedure(pname: GLenum; params: PGLfloat); apicall;
  glLightModeli: procedure(pname: GLenum; param: GLint); apicall;
  glLightModeliv: procedure(pname: GLenum; params: PGLint); apicall;
  glLineStipple: procedure(factor: GLint; pattern: GLushort); apicall;
  glMaterialf: procedure(face: GLenum; pname: GLenum; param: GLfloat); apicall;
  glMaterialfv: procedure(face: GLenum; pname: GLenum; params: PGLfloat); apicall;
  glMateriali: procedure(face: GLenum; pname: GLenum; param: GLint); apicall;
  glMaterialiv: procedure(face: GLenum; pname: GLenum; params: PGLint); apicall;
  glPolygonStipple: procedure(mask: PGLubyte); apicall;
  glShadeModel: procedure(mode: GLenum); apicall;
  glTexEnvf: procedure(target: GLenum; pname: GLenum; param: GLfloat); apicall;
  glTexEnvfv: procedure(target: GLenum; pname: GLenum; params: PGLfloat); apicall;
  glTexEnvi: procedure(target: GLenum; pname: GLenum; param: GLint); apicall;
  glTexEnviv: procedure(target: GLenum; pname: GLenum; params: PGLint); apicall;
  glTexGend: procedure(coord: GLenum; pname: GLenum; param: GLdouble); apicall;
  glTexGendv: procedure(coord: GLenum; pname: GLenum; params: PGLdouble); apicall;
  glTexGenf: procedure(coord: GLenum; pname: GLenum; param: GLfloat); apicall;
  glTexGenfv: procedure(coord: GLenum; pname: GLenum; params: PGLfloat); apicall;
  glTexGeni: procedure(coord: GLenum; pname: GLenum; param: GLint); apicall;
  glTexGeniv: procedure(coord: GLenum; pname: GLenum; params: PGLint); apicall;
  glFeedbackBuffer: procedure(size: GLsizei; type_: GLenum; buffer: PGLfloat); apicall;
  glSelectBuffer: procedure(size: GLsizei; buffer: PGLuint); apicall;
  glRenderMode: function(mode: GLenum): GLint; apicall;
  glInitNames: procedure; apicall;
  glLoadName: procedure(name: GLuint); apicall;
  glPassThrough: procedure(token: GLfloat); apicall;
  glPopName: procedure; apicall;
  glPushName: procedure(name: GLuint); apicall;
  glClearAccum: procedure(red: GLfloat; green: GLfloat; blue: GLfloat; alpha: GLfloat); apicall;
  glClearIndex: procedure(c: GLfloat); apicall;
  glIndexMask: procedure(mask: GLuint); apicall;
  glAccum: procedure(op: GLenum; value: GLfloat); apicall;
  glPopAttrib: procedure; apicall;
  glPushAttrib: procedure(mask: GLbitfield); apicall;
  glMap1d: procedure(target: GLenum; u1: GLdouble; u2: GLdouble; stride: GLint; order: GLint; points: PGLdouble); apicall;
  glMap1f: procedure(target: GLenum; u1: GLfloat; u2: GLfloat; stride: GLint; order: GLint; points: PGLfloat); apicall;
  glMap2d: procedure(target: GLenum; u1: GLdouble; u2: GLdouble; ustride: GLint; uorder: GLint; v1: GLdouble; v2: GLdouble; vstride: GLint; vorder: GLint; points: PGLdouble); apicall;
  glMap2f: procedure(target: GLenum; u1: GLfloat; u2: GLfloat; ustride: GLint; uorder: GLint; v1: GLfloat; v2: GLfloat; vstride: GLint; vorder: GLint; points: PGLfloat); apicall;
  glMapGrid1d: procedure(un: GLint; u1: GLdouble; u2: GLdouble); apicall;
  glMapGrid1f: procedure(un: GLint; u1: GLfloat; u2: GLfloat); apicall;
  glMapGrid2d: procedure(un: GLint; u1: GLdouble; u2: GLdouble; vn: GLint; v1: GLdouble; v2: GLdouble); apicall;
  glMapGrid2f: procedure(un: GLint; u1: GLfloat; u2: GLfloat; vn: GLint; v1: GLfloat; v2: GLfloat); apicall;
  glEvalCoord1d: procedure(u: GLdouble); apicall;
  glEvalCoord1dv: procedure(u: PGLdouble); apicall;
  glEvalCoord1f: procedure(u: GLfloat); apicall;
  glEvalCoord1fv: procedure(u: PGLfloat); apicall;
  glEvalCoord2d: procedure(u: GLdouble; v: GLdouble); apicall;
  glEvalCoord2dv: procedure(u: PGLdouble); apicall;
  glEvalCoord2f: procedure(u: GLfloat; v: GLfloat); apicall;
  glEvalCoord2fv: procedure(u: PGLfloat); apicall;
  glEvalMesh1: procedure(mode: GLenum; i1: GLint; i2: GLint); apicall;
  glEvalPoint1: procedure(i: GLint); apicall;
  glEvalMesh2: procedure(mode: GLenum; i1: GLint; i2: GLint; j1: GLint; j2: GLint); apicall;
  glEvalPoint2: procedure(i: GLint; j: GLint); apicall;
  glAlphaFunc: procedure(func: GLenum; ref: GLfloat); apicall;
  glPixelZoom: procedure(xfactor: GLfloat; yfactor: GLfloat); apicall;
  glPixelTransferf: procedure(pname: GLenum; param: GLfloat); apicall;
  glPixelTransferi: procedure(pname: GLenum; param: GLint); apicall;
  glPixelMapfv: procedure(map: GLenum; mapsize: GLsizei; values: PGLfloat); apicall;
  glPixelMapuiv: procedure(map: GLenum; mapsize: GLsizei; values: PGLuint); apicall;
  glPixelMapusv: procedure(map: GLenum; mapsize: GLsizei; values: PGLushort); apicall;
  glCopyPixels: procedure(x: GLint; y: GLint; width: GLsizei; height: GLsizei; type_: GLenum); apicall;
  glDrawPixels: procedure(width: GLsizei; height: GLsizei; format: GLenum; type_: GLenum; pixels: Pointer); apicall;
  glGetClipPlane: procedure(plane: GLenum; equation: PGLdouble); apicall;
  glGetLightfv: procedure(light: GLenum; pname: GLenum; params: PGLfloat); apicall;
  glGetLightiv: procedure(light: GLenum; pname: GLenum; params: PGLint); apicall;
  glGetMapdv: procedure(target: GLenum; query: GLenum; v: PGLdouble); apicall;
  glGetMapfv: procedure(target: GLenum; query: GLenum; v: PGLfloat); apicall;
  glGetMapiv: procedure(target: GLenum; query: GLenum; v: PGLint); apicall;
  glGetMaterialfv: procedure(face: GLenum; pname: GLenum; params: PGLfloat); apicall;
  glGetMaterialiv: procedure(face: GLenum; pname: GLenum; params: PGLint); apicall;
  glGetPixelMapfv: procedure(map: GLenum; values: PGLfloat); apicall;
  glGetPixelMapuiv: procedure(map: GLenum; values: PGLuint); apicall;
  glGetPixelMapusv: procedure(map: GLenum; values: PGLushort); apicall;
  glGetPolygonStipple: procedure(mask: PGLubyte); apicall;
  glGetTexEnvfv: procedure(target: GLenum; pname: GLenum; params: PGLfloat); apicall;
  glGetTexEnviv: procedure(target: GLenum; pname: GLenum; params: PGLint); apicall;
  glGetTexGendv: procedure(coord: GLenum; pname: GLenum; params: PGLdouble); apicall;
  glGetTexGenfv: procedure(coord: GLenum; pname: GLenum; params: PGLfloat); apicall;
  glGetTexGeniv: procedure(coord: GLenum; pname: GLenum; params: PGLint); apicall;
  glIsList: function(list: GLuint): GLboolean; apicall;
  glFrustum: procedure(left: GLdouble; right: GLdouble; bottom: GLdouble; top: GLdouble; zNear: GLdouble; zFar: GLdouble); apicall;
  glLoadIdentity: procedure; apicall;
  glLoadMatrixf: procedure(m: PGLfloat); apicall;
  glLoadMatrixd: procedure(m: PGLdouble); apicall;
  glMatrixMode: procedure(mode: GLenum); apicall;
  glMultMatrixf: procedure(m: PGLfloat); apicall;
  glMultMatrixd: procedure(m: PGLdouble); apicall;
  glOrtho: procedure(left: GLdouble; right: GLdouble; bottom: GLdouble; top: GLdouble; zNear: GLdouble; zFar: GLdouble); apicall;
  glPopMatrix: procedure; apicall;
  glPushMatrix: procedure; apicall;
  glRotated: procedure(angle: GLdouble; x: GLdouble; y: GLdouble; z: GLdouble); apicall;
  glRotatef: procedure(angle: GLfloat; x: GLfloat; y: GLfloat; z: GLfloat); apicall;
  glScaled: procedure(x: GLdouble; y: GLdouble; z: GLdouble); apicall;
  glScalef: procedure(x: GLfloat; y: GLfloat; z: GLfloat); apicall;
  glTranslated: procedure(x: GLdouble; y: GLdouble; z: GLdouble); apicall;
  glTranslatef: procedure(x: GLfloat; y: GLfloat; z: GLfloat); apicall;
{$endif}
{$endregion}

{ OpenGL 1.1 }

{$region gl11}
const
  GL_COLOR_LOGIC_OP = $0BF2;
  GL_POLYGON_OFFSET_UNITS = $2A00;
  GL_POLYGON_OFFSET_POINT = $2A01;
  GL_POLYGON_OFFSET_LINE = $2A02;
  GL_POLYGON_OFFSET_FILL = $8037;
  GL_POLYGON_OFFSET_FACTOR = $8038;
  GL_TEXTURE_BINDING_1D = $8068;
  GL_TEXTURE_BINDING_2D = $8069;
  GL_TEXTURE_INTERNAL_FORMAT = $1003;
  GL_TEXTURE_RED_SIZE = $805C;
  GL_TEXTURE_GREEN_SIZE = $805D;
  GL_TEXTURE_BLUE_SIZE = $805E;
  GL_TEXTURE_ALPHA_SIZE = $805F;
  GL_DOUBLE = $140A;
  GL_PROXY_TEXTURE_1D = $8063;
  GL_PROXY_TEXTURE_2D = $8064;
  GL_R3_G3_B2 = $2A10;
  GL_RGB4 = $804F;
  GL_RGB5 = $8050;
  GL_RGB8 = $8051;
  GL_RGB10 = $8052;
  GL_RGB12 = $8053;
  GL_RGB16 = $8054;
  GL_RGBA2 = $8055;
  GL_RGBA4 = $8056;
  GL_RGB5_A1 = $8057;
  GL_RGBA8 = $8058;
  GL_RGB10_A2 = $8059;
  GL_RGBA12 = $805A;
  GL_RGBA16 = $805B;

var
  glDrawArrays: procedure(mode: GLenum; first: GLint; count: GLsizei); apicall;
  glDrawElements: procedure(mode: GLenum; count: GLsizei; type_: GLenum; indices: Pointer); apicall;
  glPolygonOffset: procedure(factor: GLfloat; units: GLfloat); apicall;
  glCopyTexImage1D: procedure(target: GLenum; level: GLint; internalformat: GLenum; x: GLint; y: GLint; width: GLsizei; border: GLint); apicall;
  glCopyTexImage2D: procedure(target: GLenum; level: GLint; internalformat: GLenum; x: GLint; y: GLint; width: GLsizei; height: GLsizei; border: GLint); apicall;
  glCopyTexSubImage1D: procedure(target: GLenum; level: GLint; xoffset: GLint; x: GLint; y: GLint; width: GLsizei); apicall;
  glCopyTexSubImage2D: procedure(target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; x: GLint; y: GLint; width: GLsizei; height: GLsizei); apicall;
  glTexSubImage1D: procedure(target: GLenum; level: GLint; xoffset: GLint; width: GLsizei; format: GLenum; type_: GLenum; pixels: Pointer); apicall;
  glTexSubImage2D: procedure(target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; width: GLsizei; height: GLsizei; format: GLenum; type_: GLenum; pixels: Pointer); apicall;
  glBindTexture: procedure(target: GLenum; texture: GLuint); apicall;
  glDeleteTextures: procedure(n: GLsizei; textures: PGLuint); apicall;
  glGenTextures: procedure(n: GLsizei; textures: PGLuint); apicall;
  glIsTexture: function(texture: GLuint): GLboolean; apicall;
{$endregion}

{ OpenGL 1.1 compatibility profile }

{$region gl11 compatibility}
{$ifdef glcompat}
const
  GL_CLIENT_PIXEL_STORE_BIT = $00000001;
  GL_CLIENT_VERTEX_ARRAY_BIT = $00000002;
  GL_CLIENT_ALL_ATTRIB_BITS = $FFFFFFFF;
  GL_VERTEX_ARRAY_POINTER = $808E;
  GL_NORMAL_ARRAY_POINTER = $808F;
  GL_COLOR_ARRAY_POINTER = $8090;
  GL_INDEX_ARRAY_POINTER = $8091;
  GL_TEXTURE_COORD_ARRAY_POINTER = $8092;
  GL_EDGE_FLAG_ARRAY_POINTER = $8093;
  GL_FEEDBACK_BUFFER_POINTER = $0DF0;
  GL_SELECTION_BUFFER_POINTER = $0DF3;
  GL_CLIENT_ATTRIB_STACK_DEPTH = $0BB1;
  GL_INDEX_LOGIC_OP = $0BF1;
  GL_MAX_CLIENT_ATTRIB_STACK_DEPTH = $0D3B;
  GL_FEEDBACK_BUFFER_SIZE = $0DF1;
  GL_FEEDBACK_BUFFER_TYPE = $0DF2;
  GL_SELECTION_BUFFER_SIZE = $0DF4;
  GL_VERTEX_ARRAY = $8074;
  GL_NORMAL_ARRAY = $8075;
  GL_COLOR_ARRAY = $8076;
  GL_INDEX_ARRAY = $8077;
  GL_TEXTURE_COORD_ARRAY = $8078;
  GL_EDGE_FLAG_ARRAY = $8079;
  GL_VERTEX_ARRAY_SIZE = $807A;
  GL_VERTEX_ARRAY_TYPE = $807B;
  GL_VERTEX_ARRAY_STRIDE = $807C;
  GL_NORMAL_ARRAY_TYPE = $807E;
  GL_NORMAL_ARRAY_STRIDE = $807F;
  GL_COLOR_ARRAY_SIZE = $8081;
  GL_COLOR_ARRAY_TYPE = $8082;
  GL_COLOR_ARRAY_STRIDE = $8083;
  GL_INDEX_ARRAY_TYPE = $8085;
  GL_INDEX_ARRAY_STRIDE = $8086;
  GL_TEXTURE_COORD_ARRAY_SIZE = $8088;
  GL_TEXTURE_COORD_ARRAY_TYPE = $8089;
  GL_TEXTURE_COORD_ARRAY_STRIDE = $808A;
  GL_EDGE_FLAG_ARRAY_STRIDE = $808C;
  GL_TEXTURE_LUMINANCE_SIZE = $8060;
  GL_TEXTURE_INTENSITY_SIZE = $8061;
  GL_TEXTURE_PRIORITY = $8066;
  GL_TEXTURE_RESIDENT = $8067;
  GL_ALPHA4 = $803B;
  GL_ALPHA8 = $803C;
  GL_ALPHA12 = $803D;
  GL_ALPHA16 = $803E;
  GL_LUMINANCE4 = $803F;
  GL_LUMINANCE8 = $8040;
  GL_LUMINANCE12 = $8041;
  GL_LUMINANCE16 = $8042;
  GL_LUMINANCE4_ALPHA4 = $8043;
  GL_LUMINANCE6_ALPHA2 = $8044;
  GL_LUMINANCE8_ALPHA8 = $8045;
  GL_LUMINANCE12_ALPHA4 = $8046;
  GL_LUMINANCE12_ALPHA12 = $8047;
  GL_LUMINANCE16_ALPHA16 = $8048;
  GL_INTENSITY = $8049;
  GL_INTENSITY4 = $804A;
  GL_INTENSITY8 = $804B;
  GL_INTENSITY12 = $804C;
  GL_INTENSITY16 = $804D;
  GL_V2F = $2A20;
  GL_V3F = $2A21;
  GL_C4UB_V2F = $2A22;
  GL_C4UB_V3F = $2A23;
  GL_C3F_V3F = $2A24;
  GL_N3F_V3F = $2A25;
  GL_C4F_N3F_V3F = $2A26;
  GL_T2F_V3F = $2A27;
  GL_T4F_V4F = $2A28;
  GL_T2F_C4UB_V3F = $2A29;
  GL_T2F_C3F_V3F = $2A2A;
  GL_T2F_N3F_V3F = $2A2B;
  GL_T2F_C4F_N3F_V3F = $2A2C;
  GL_T4F_C4F_N3F_V4F = $2A2D;

var
  glArrayElement: procedure(i: GLint); apicall;
  glColorPointer: procedure(size: GLint; type_: GLenum; stride: GLsizei; pointer: Pointer); apicall;
  glDisableClientState: procedure(array_: GLenum); apicall;
  glEdgeFlagPointer: procedure(stride: GLsizei; pointer: Pointer); apicall;
  glEnableClientState: procedure(array_: GLenum); apicall;
  glIndexPointer: procedure(type_: GLenum; stride: GLsizei; pointer: Pointer); apicall;
  glGetPointerv: procedure(pname: GLenum; params: PPointer); apicall;
  glInterleavedArrays: procedure(format: GLenum; stride: GLsizei; pointer: Pointer); apicall;
  glNormalPointer: procedure(type_: GLenum; stride: GLsizei; pointer: Pointer); apicall;
  glTexCoordPointer: procedure(size: GLint; type_: GLenum; stride: GLsizei; pointer: Pointer); apicall;
  glVertexPointer: procedure(size: GLint; type_: GLenum; stride: GLsizei; pointer: Pointer); apicall;
  glAreTexturesResident: function(n: GLsizei; textures: PGLuint; residences: PGLboolean): GLboolean; apicall;
  glPrioritizeTextures: procedure(n: GLsizei; textures: PGLuint; priorities: PGLfloat); apicall;
  glIndexub: procedure(c: GLubyte); apicall;
  glIndexubv: procedure(c: PGLubyte); apicall;
  glPopClientAttrib: procedure; apicall;
  glPushClientAttrib: procedure(mask: GLbitfield); apicall;
{$endif}
{$endregion}

{ OpenGL 1.2 }

{$region gl12}
const
  GL_UNSIGNED_BYTE_3_3_2 = $8032;
  GL_UNSIGNED_SHORT_4_4_4_4 = $8033;
  GL_UNSIGNED_SHORT_5_5_5_1 = $8034;
  GL_UNSIGNED_INT_8_8_8_8 = $8035;
  GL_UNSIGNED_INT_10_10_10_2 = $8036;
  GL_TEXTURE_BINDING_3D = $806A;
  GL_PACK_SKIP_IMAGES = $806B;
  GL_PACK_IMAGE_HEIGHT = $806C;
  GL_UNPACK_SKIP_IMAGES = $806D;
  GL_UNPACK_IMAGE_HEIGHT = $806E;
  GL_TEXTURE_3D = $806F;
  GL_PROXY_TEXTURE_3D = $8070;
  GL_TEXTURE_DEPTH = $8071;
  GL_TEXTURE_WRAP_R = $8072;
  GL_MAX_3D_TEXTURE_SIZE = $8073;
  GL_UNSIGNED_BYTE_2_3_3_REV = $8362;
  GL_UNSIGNED_SHORT_5_6_5 = $8363;
  GL_UNSIGNED_SHORT_5_6_5_REV = $8364;
  GL_UNSIGNED_SHORT_4_4_4_4_REV = $8365;
  GL_UNSIGNED_SHORT_1_5_5_5_REV = $8366;
  GL_UNSIGNED_INT_8_8_8_8_REV = $8367;
  GL_UNSIGNED_INT_2_10_10_10_REV = $8368;
  GL_BGR = $80E0;
  GL_BGRA = $80E1;
  GL_MAX_ELEMENTS_VERTICES = $80E8;
  GL_MAX_ELEMENTS_INDICES = $80E9;
  GL_CLAMP_TO_EDGE = $812F;
  GL_TEXTURE_MIN_LOD = $813A;
  GL_TEXTURE_MAX_LOD = $813B;
  GL_TEXTURE_BASE_LEVEL = $813C;
  GL_TEXTURE_MAX_LEVEL = $813D;
  GL_SMOOTH_POINT_SIZE_RANGE = $0B12;
  GL_SMOOTH_POINT_SIZE_GRANULARITY = $0B13;
  GL_SMOOTH_LINE_WIDTH_RANGE = $0B22;
  GL_SMOOTH_LINE_WIDTH_GRANULARITY = $0B23;
  GL_ALIASED_LINE_WIDTH_RANGE = $846E;

var
  glDrawRangeElements: procedure(mode: GLenum; start: GLuint; end_: GLuint; count: GLsizei; type_: GLenum; indices: Pointer); apicall;
  glTexImage3D: procedure(target: GLenum; level: GLint; internalformat: GLint; width: GLsizei; height: GLsizei; depth: GLsizei; border: GLint; format: GLenum; type_: GLenum; pixels: Pointer); apicall;
  glTexSubImage3D: procedure(target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; zoffset: GLint; width: GLsizei; height: GLsizei; depth: GLsizei; format: GLenum; type_: GLenum; pixels: Pointer); apicall;
  glCopyTexSubImage3D: procedure(target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; zoffset: GLint; x: GLint; y: GLint; width: GLsizei; height: GLsizei); apicall;
{$endregion}

{ OpenGL 1.2 compatibility profile }

{$region gl12 compatibility}
{$ifdef glcompat}
const
  GL_RESCALE_NORMAL = $803A;
  GL_LIGHT_MODEL_COLOR_CONTROL = $81F8;
  GL_SINGLE_COLOR = $81F9;
  GL_SEPARATE_SPECULAR_COLOR = $81FA;
  GL_ALIASED_POINT_SIZE_RANGE = $846D;
{$endif}
{$endregion}

{ OpenGL 1.3 }

{$region gl13}
const
  GL_TEXTURE0 = $84C0;
  GL_TEXTURE1 = $84C1;
  GL_TEXTURE2 = $84C2;
  GL_TEXTURE3 = $84C3;
  GL_TEXTURE4 = $84C4;
  GL_TEXTURE5 = $84C5;
  GL_TEXTURE6 = $84C6;
  GL_TEXTURE7 = $84C7;
  GL_TEXTURE8 = $84C8;
  GL_TEXTURE9 = $84C9;
  GL_TEXTURE10 = $84CA;
  GL_TEXTURE11 = $84CB;
  GL_TEXTURE12 = $84CC;
  GL_TEXTURE13 = $84CD;
  GL_TEXTURE14 = $84CE;
  GL_TEXTURE15 = $84CF;
  GL_TEXTURE16 = $84D0;
  GL_TEXTURE17 = $84D1;
  GL_TEXTURE18 = $84D2;
  GL_TEXTURE19 = $84D3;
  GL_TEXTURE20 = $84D4;
  GL_TEXTURE21 = $84D5;
  GL_TEXTURE22 = $84D6;
  GL_TEXTURE23 = $84D7;
  GL_TEXTURE24 = $84D8;
  GL_TEXTURE25 = $84D9;
  GL_TEXTURE26 = $84DA;
  GL_TEXTURE27 = $84DB;
  GL_TEXTURE28 = $84DC;
  GL_TEXTURE29 = $84DD;
  GL_TEXTURE30 = $84DE;
  GL_TEXTURE31 = $84DF;
  GL_ACTIVE_TEXTURE = $84E0;
  GL_MULTISAMPLE = $809D;
  GL_SAMPLE_ALPHA_TO_COVERAGE = $809E;
  GL_SAMPLE_ALPHA_TO_ONE = $809F;
  GL_SAMPLE_COVERAGE = $80A0;
  GL_SAMPLE_BUFFERS = $80A8;
  GL_SAMPLES = $80A9;
  GL_SAMPLE_COVERAGE_VALUE = $80AA;
  GL_SAMPLE_COVERAGE_INVERT = $80AB;
  GL_TEXTURE_CUBE_MAP = $8513;
  GL_TEXTURE_BINDING_CUBE_MAP = $8514;
  GL_TEXTURE_CUBE_MAP_POSITIVE_X = $8515;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_X = $8516;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Y = $8517;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Y = $8518;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Z = $8519;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Z = $851A;
  GL_PROXY_TEXTURE_CUBE_MAP = $851B;
  GL_MAX_CUBE_MAP_TEXTURE_SIZE = $851C;
  GL_COMPRESSED_RGB = $84ED;
  GL_COMPRESSED_RGBA = $84EE;
  GL_TEXTURE_COMPRESSION_HINT = $84EF;
  GL_TEXTURE_COMPRESSED_IMAGE_SIZE = $86A0;
  GL_TEXTURE_COMPRESSED = $86A1;
  GL_NUM_COMPRESSED_TEXTURE_FORMATS = $86A2;
  GL_COMPRESSED_TEXTURE_FORMATS = $86A3;
  GL_CLAMP_TO_BORDER = $812D;

var
  glActiveTexture: procedure(texture: GLenum); apicall;
  glSampleCoverage: procedure(value: GLfloat; invert: GLboolean); apicall;
  glCompressedTexImage3D: procedure(target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; height: GLsizei; depth: GLsizei; border: GLint; imageSize: GLsizei; data: Pointer); apicall;
  glCompressedTexImage2D: procedure(target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; height: GLsizei; border: GLint; imageSize: GLsizei; data: Pointer); apicall;
  glCompressedTexImage1D: procedure(target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; border: GLint; imageSize: GLsizei; data: Pointer); apicall;
  glCompressedTexSubImage3D: procedure(target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; zoffset: GLint; width: GLsizei; height: GLsizei; depth: GLsizei; format: GLenum; imageSize: GLsizei; data: Pointer); apicall;
  glCompressedTexSubImage2D: procedure(target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; width: GLsizei; height: GLsizei; format: GLenum; imageSize: GLsizei; data: Pointer); apicall;
  glCompressedTexSubImage1D: procedure(target: GLenum; level: GLint; xoffset: GLint; width: GLsizei; format: GLenum; imageSize: GLsizei; data: Pointer); apicall;
  glGetCompressedTexImage: procedure(target: GLenum; level: GLint; img: Pointer); apicall;
{$endregion}

{ OpenGL 1.3 compatibility profile }

{$region gl13 compatibility}
{$ifdef glcompat}
const
  GL_CLIENT_ACTIVE_TEXTURE = $84E1;
  GL_MAX_TEXTURE_UNITS = $84E2;
  GL_TRANSPOSE_MODELVIEW_MATRIX = $84E3;
  GL_TRANSPOSE_PROJECTION_MATRIX = $84E4;
  GL_TRANSPOSE_TEXTURE_MATRIX = $84E5;
  GL_TRANSPOSE_COLOR_MATRIX = $84E6;
  GL_MULTISAMPLE_BIT = $20000000;
  GL_NORMAL_MAP = $8511;
  GL_REFLECTION_MAP = $8512;
  GL_COMPRESSED_ALPHA = $84E9;
  GL_COMPRESSED_LUMINANCE = $84EA;
  GL_COMPRESSED_LUMINANCE_ALPHA = $84EB;
  GL_COMPRESSED_INTENSITY = $84EC;
  GL_COMBINE = $8570;
  GL_COMBINE_RGB = $8571;
  GL_COMBINE_ALPHA = $8572;
  GL_SOURCE0_RGB = $8580;
  GL_SOURCE1_RGB = $8581;
  GL_SOURCE2_RGB = $8582;
  GL_SOURCE0_ALPHA = $8588;
  GL_SOURCE1_ALPHA = $8589;
  GL_SOURCE2_ALPHA = $858A;
  GL_OPERAND0_RGB = $8590;
  GL_OPERAND1_RGB = $8591;
  GL_OPERAND2_RGB = $8592;
  GL_OPERAND0_ALPHA = $8598;
  GL_OPERAND1_ALPHA = $8599;
  GL_OPERAND2_ALPHA = $859A;
  GL_RGB_SCALE = $8573;
  GL_ADD_SIGNED = $8574;
  GL_INTERPOLATE = $8575;
  GL_SUBTRACT = $84E7;
  GL_CONSTANT = $8576;
  GL_PRIMARY_COLOR = $8577;
  GL_PREVIOUS = $8578;
  GL_DOT3_RGB = $86AE;
  GL_DOT3_RGBA = $86AF;

var
  glClientActiveTexture: procedure(texture: GLenum); apicall;
  glMultiTexCoord1d: procedure(target: GLenum; s: GLdouble); apicall;
  glMultiTexCoord1dv: procedure(target: GLenum; v: PGLdouble); apicall;
  glMultiTexCoord1f: procedure(target: GLenum; s: GLfloat); apicall;
  glMultiTexCoord1fv: procedure(target: GLenum; v: PGLfloat); apicall;
  glMultiTexCoord1i: procedure(target: GLenum; s: GLint); apicall;
  glMultiTexCoord1iv: procedure(target: GLenum; v: PGLint); apicall;
  glMultiTexCoord1s: procedure(target: GLenum; s: GLshort); apicall;
  glMultiTexCoord1sv: procedure(target: GLenum; v: PGLshort); apicall;
  glMultiTexCoord2d: procedure(target: GLenum; s: GLdouble; t: GLdouble); apicall;
  glMultiTexCoord2dv: procedure(target: GLenum; v: PGLdouble); apicall;
  glMultiTexCoord2f: procedure(target: GLenum; s: GLfloat; t: GLfloat); apicall;
  glMultiTexCoord2fv: procedure(target: GLenum; v: PGLfloat); apicall;
  glMultiTexCoord2i: procedure(target: GLenum; s: GLint; t: GLint); apicall;
  glMultiTexCoord2iv: procedure(target: GLenum; v: PGLint); apicall;
  glMultiTexCoord2s: procedure(target: GLenum; s: GLshort; t: GLshort); apicall;
  glMultiTexCoord2sv: procedure(target: GLenum; v: PGLshort); apicall;
  glMultiTexCoord3d: procedure(target: GLenum; s: GLdouble; t: GLdouble; r: GLdouble); apicall;
  glMultiTexCoord3dv: procedure(target: GLenum; v: PGLdouble); apicall;
  glMultiTexCoord3f: procedure(target: GLenum; s: GLfloat; t: GLfloat; r: GLfloat); apicall;
  glMultiTexCoord3fv: procedure(target: GLenum; v: PGLfloat); apicall;
  glMultiTexCoord3i: procedure(target: GLenum; s: GLint; t: GLint; r: GLint); apicall;
  glMultiTexCoord3iv: procedure(target: GLenum; v: PGLint); apicall;
  glMultiTexCoord3s: procedure(target: GLenum; s: GLshort; t: GLshort; r: GLshort); apicall;
  glMultiTexCoord3sv: procedure(target: GLenum; v: PGLshort); apicall;
  glMultiTexCoord4d: procedure(target: GLenum; s: GLdouble; t: GLdouble; r: GLdouble; q: GLdouble); apicall;
  glMultiTexCoord4dv: procedure(target: GLenum; v: PGLdouble); apicall;
  glMultiTexCoord4f: procedure(target: GLenum; s: GLfloat; t: GLfloat; r: GLfloat; q: GLfloat); apicall;
  glMultiTexCoord4fv: procedure(target: GLenum; v: PGLfloat); apicall;
  glMultiTexCoord4i: procedure(target: GLenum; s: GLint; t: GLint; r: GLint; q: GLint); apicall;
  glMultiTexCoord4iv: procedure(target: GLenum; v: PGLint); apicall;
  glMultiTexCoord4s: procedure(target: GLenum; s: GLshort; t: GLshort; r: GLshort; q: GLshort); apicall;
  glMultiTexCoord4sv: procedure(target: GLenum; v: PGLshort); apicall;
  glLoadTransposeMatrixf: procedure(m: PGLfloat); apicall;
  glLoadTransposeMatrixd: procedure(m: PGLdouble); apicall;
  glMultTransposeMatrixf: procedure(m: PGLfloat); apicall;
  glMultTransposeMatrixd: procedure(m: PGLdouble); apicall;
{$endif}
{$endregion}

{ OpenGL 1.4 }

{$region gl14}
const
  GL_BLEND_DST_RGB = $80C8;
  GL_BLEND_SRC_RGB = $80C9;
  GL_BLEND_DST_ALPHA = $80CA;
  GL_BLEND_SRC_ALPHA = $80CB;
  GL_POINT_FADE_THRESHOLD_SIZE = $8128;
  GL_DEPTH_COMPONENT16 = $81A5;
  GL_DEPTH_COMPONENT24 = $81A6;
  GL_DEPTH_COMPONENT32 = $81A7;
  GL_MIRRORED_REPEAT = $8370;
  GL_MAX_TEXTURE_LOD_BIAS = $84FD;
  GL_TEXTURE_LOD_BIAS = $8501;
  GL_INCR_WRAP = $8507;
  GL_DECR_WRAP = $8508;
  GL_TEXTURE_DEPTH_SIZE = $884A;
  GL_TEXTURE_COMPARE_MODE = $884C;
  GL_TEXTURE_COMPARE_FUNC = $884D;
  GL_BLEND_COLOR = $8005;
  GL_BLEND_EQUATION = $8009;
  GL_CONSTANT_COLOR = $8001;
  GL_ONE_MINUS_CONSTANT_COLOR = $8002;
  GL_CONSTANT_ALPHA = $8003;
  GL_ONE_MINUS_CONSTANT_ALPHA = $8004;
  GL_FUNC_ADD = $8006;
  GL_FUNC_REVERSE_SUBTRACT = $800B;
  GL_FUNC_SUBTRACT = $800A;
  GL_MIN = $8007;
  GL_MAX = $8008;

var
  glBlendFuncSeparate: procedure(sfactorRGB: GLenum; dfactorRGB: GLenum; sfactorAlpha: GLenum; dfactorAlpha: GLenum); apicall;
  glMultiDrawArrays: procedure(mode: GLenum; first: PGLint; count: PGLsizei; drawcount: GLsizei); apicall;
  glMultiDrawElements: procedure(mode: GLenum; count: PGLsizei; type_: GLenum; indices: PPointer; drawcount: GLsizei); apicall;
  glPointParameterf: procedure(pname: GLenum; param: GLfloat); apicall;
  glPointParameterfv: procedure(pname: GLenum; params: PGLfloat); apicall;
  glPointParameteri: procedure(pname: GLenum; param: GLint); apicall;
  glPointParameteriv: procedure(pname: GLenum; params: PGLint); apicall;
  glBlendColor: procedure(red: GLfloat; green: GLfloat; blue: GLfloat; alpha: GLfloat); apicall;
  glBlendEquation: procedure(mode: GLenum); apicall;
{$endregion}

{ OpenGL 1.4 compatibility profile }

{$region gl14 compatibility}
{$ifdef glcompat}
const
  GL_POINT_SIZE_MIN = $8126;
  GL_POINT_SIZE_MAX = $8127;
  GL_POINT_DISTANCE_ATTENUATION = $8129;
  GL_GENERATE_MIPMAP = $8191;
  GL_GENERATE_MIPMAP_HINT = $8192;
  GL_FOG_COORDINATE_SOURCE = $8450;
  GL_FOG_COORDINATE = $8451;
  GL_FRAGMENT_DEPTH = $8452;
  GL_CURRENT_FOG_COORDINATE = $8453;
  GL_FOG_COORDINATE_ARRAY_TYPE = $8454;
  GL_FOG_COORDINATE_ARRAY_STRIDE = $8455;
  GL_FOG_COORDINATE_ARRAY_POINTER = $8456;
  GL_FOG_COORDINATE_ARRAY = $8457;
  GL_COLOR_SUM = $8458;
  GL_CURRENT_SECONDARY_COLOR = $8459;
  GL_SECONDARY_COLOR_ARRAY_SIZE = $845A;
  GL_SECONDARY_COLOR_ARRAY_TYPE = $845B;
  GL_SECONDARY_COLOR_ARRAY_STRIDE = $845C;
  GL_SECONDARY_COLOR_ARRAY_POINTER = $845D;
  GL_SECONDARY_COLOR_ARRAY = $845E;
  GL_TEXTURE_FILTER_CONTROL = $8500;
  GL_DEPTH_TEXTURE_MODE = $884B;
  GL_COMPARE_R_TO_TEXTURE = $884E;

var
  glFogCoordf: procedure(coord: GLfloat); apicall;
  glFogCoordfv: procedure(coord: PGLfloat); apicall;
  glFogCoordd: procedure(coord: GLdouble); apicall;
  glFogCoorddv: procedure(coord: PGLdouble); apicall;
  glFogCoordPointer: procedure(type_: GLenum; stride: GLsizei; pointer: Pointer); apicall;
  glSecondaryColor3b: procedure(red: GLbyte; green: GLbyte; blue: GLbyte); apicall;
  glSecondaryColor3bv: procedure(v: PGLbyte); apicall;
  glSecondaryColor3d: procedure(red: GLdouble; green: GLdouble; blue: GLdouble); apicall;
  glSecondaryColor3dv: procedure(v: PGLdouble); apicall;
  glSecondaryColor3f: procedure(red: GLfloat; green: GLfloat; blue: GLfloat); apicall;
  glSecondaryColor3fv: procedure(v: PGLfloat); apicall;
  glSecondaryColor3i: procedure(red: GLint; green: GLint; blue: GLint); apicall;
  glSecondaryColor3iv: procedure(v: PGLint); apicall;
  glSecondaryColor3s: procedure(red: GLshort; green: GLshort; blue: GLshort); apicall;
  glSecondaryColor3sv: procedure(v: PGLshort); apicall;
  glSecondaryColor3ub: procedure(red: GLubyte; green: GLubyte; blue: GLubyte); apicall;
  glSecondaryColor3ubv: procedure(v: PGLubyte); apicall;
  glSecondaryColor3ui: procedure(red: GLuint; green: GLuint; blue: GLuint); apicall;
  glSecondaryColor3uiv: procedure(v: PGLuint); apicall;
  glSecondaryColor3us: procedure(red: GLushort; green: GLushort; blue: GLushort); apicall;
  glSecondaryColor3usv: procedure(v: PGLushort); apicall;
  glSecondaryColorPointer: procedure(size: GLint; type_: GLenum; stride: GLsizei; pointer: Pointer); apicall;
  glWindowPos2d: procedure(x: GLdouble; y: GLdouble); apicall;
  glWindowPos2dv: procedure(v: PGLdouble); apicall;
  glWindowPos2f: procedure(x: GLfloat; y: GLfloat); apicall;
  glWindowPos2fv: procedure(v: PGLfloat); apicall;
  glWindowPos2i: procedure(x: GLint; y: GLint); apicall;
  glWindowPos2iv: procedure(v: PGLint); apicall;
  glWindowPos2s: procedure(x: GLshort; y: GLshort); apicall;
  glWindowPos2sv: procedure(v: PGLshort); apicall;
  glWindowPos3d: procedure(x: GLdouble; y: GLdouble; z: GLdouble); apicall;
  glWindowPos3dv: procedure(v: PGLdouble); apicall;
  glWindowPos3f: procedure(x: GLfloat; y: GLfloat; z: GLfloat); apicall;
  glWindowPos3fv: procedure(v: PGLfloat); apicall;
  glWindowPos3i: procedure(x: GLint; y: GLint; z: GLint); apicall;
  glWindowPos3iv: procedure(v: PGLint); apicall;
  glWindowPos3s: procedure(x: GLshort; y: GLshort; z: GLshort); apicall;
  glWindowPos3sv: procedure(v: PGLshort); apicall;
{$endif}
{$endregion}

{ OpenGL 1.5 }

{$region gl15}
const
  GL_BUFFER_SIZE = $8764;
  GL_BUFFER_USAGE = $8765;
  GL_QUERY_COUNTER_BITS = $8864;
  GL_CURRENT_QUERY = $8865;
  GL_QUERY_RESULT = $8866;
  GL_QUERY_RESULT_AVAILABLE = $8867;
  GL_ARRAY_BUFFER = $8892;
  GL_ELEMENT_ARRAY_BUFFER = $8893;
  GL_ARRAY_BUFFER_BINDING = $8894;
  GL_ELEMENT_ARRAY_BUFFER_BINDING = $8895;
  GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING = $889F;
  GL_READ_ONLY = $88B8;
  GL_WRITE_ONLY = $88B9;
  GL_READ_WRITE = $88BA;
  GL_BUFFER_ACCESS = $88BB;
  GL_BUFFER_MAPPED = $88BC;
  GL_BUFFER_MAP_POINTER = $88BD;
  GL_STREAM_DRAW = $88E0;
  GL_STREAM_READ = $88E1;
  GL_STREAM_COPY = $88E2;
  GL_STATIC_DRAW = $88E4;
  GL_STATIC_READ = $88E5;
  GL_STATIC_COPY = $88E6;
  GL_DYNAMIC_DRAW = $88E8;
  GL_DYNAMIC_READ = $88E9;
  GL_DYNAMIC_COPY = $88EA;
  GL_SAMPLES_PASSED = $8914;
  GL_SRC1_ALPHA = $8589;

var
  glGenQueries: procedure(n: GLsizei; ids: PGLuint); apicall;
  glDeleteQueries: procedure(n: GLsizei; ids: PGLuint); apicall;
  glIsQuery: function(id: GLuint): GLboolean; apicall;
  glBeginQuery: procedure(target: GLenum; id: GLuint); apicall;
  glEndQuery: procedure(target: GLenum); apicall;
  glGetQueryiv: procedure(target: GLenum; pname: GLenum; params: PGLint); apicall;
  glGetQueryObjectiv: procedure(id: GLuint; pname: GLenum; params: PGLint); apicall;
  glGetQueryObjectuiv: procedure(id: GLuint; pname: GLenum; params: PGLuint); apicall;
  glBindBuffer: procedure(target: GLenum; buffer: GLuint); apicall;
  glDeleteBuffers: procedure(n: GLsizei; buffers: PGLuint); apicall;
  glGenBuffers: procedure(n: GLsizei; buffers: PGLuint); apicall;
  glIsBuffer: function(buffer: GLuint): GLboolean; apicall;
  glBufferData: procedure(target: GLenum; size: GLsizeiptr; data: Pointer; usage: GLenum); apicall;
  glBufferSubData: procedure(target: GLenum; offset: GLintptr; size: GLsizeiptr; data: Pointer); apicall;
  glGetBufferSubData: procedure(target: GLenum; offset: GLintptr; size: GLsizeiptr; data: Pointer); apicall;
  glMapBuffer: function(target: GLenum; access: GLenum): Pointer; apicall;
  glUnmapBuffer: function(target: GLenum): GLboolean; apicall;
  glGetBufferParameteriv: procedure(target: GLenum; pname: GLenum; params: PGLint); apicall;
  glGetBufferPointerv: procedure(target: GLenum; pname: GLenum; params: PPointer); apicall;
{$endregion}

{ OpenGL 1.5 compatibility profile }

{$region gl15 compatibility}
{$ifdef glcompat}
const
  GL_VERTEX_ARRAY_BUFFER_BINDING = $8896;
  GL_NORMAL_ARRAY_BUFFER_BINDING = $8897;
  GL_COLOR_ARRAY_BUFFER_BINDING = $8898;
  GL_INDEX_ARRAY_BUFFER_BINDING = $8899;
  GL_TEXTURE_COORD_ARRAY_BUFFER_BINDING = $889A;
  GL_EDGE_FLAG_ARRAY_BUFFER_BINDING = $889B;
  GL_SECONDARY_COLOR_ARRAY_BUFFER_BINDING = $889C;
  GL_FOG_COORDINATE_ARRAY_BUFFER_BINDING = $889D;
  GL_WEIGHT_ARRAY_BUFFER_BINDING = $889E;
  GL_FOG_COORD_SRC = $8450;
  GL_FOG_COORD = $8451;
  GL_CURRENT_FOG_COORD = $8453;
  GL_FOG_COORD_ARRAY_TYPE = $8454;
  GL_FOG_COORD_ARRAY_STRIDE = $8455;
  GL_FOG_COORD_ARRAY_POINTER = $8456;
  GL_FOG_COORD_ARRAY = $8457;
  GL_FOG_COORD_ARRAY_BUFFER_BINDING = $889D;
  GL_SRC0_RGB = $8580;
  GL_SRC1_RGB = $8581;
  GL_SRC2_RGB = $8582;
  GL_SRC0_ALPHA = $8588;
  GL_SRC2_ALPHA = $858A;
{$endif}
{$endregion}

{ OpenGL 2.0 }

{$region gl20}
const
  GL_BLEND_EQUATION_RGB = $8009;
  GL_VERTEX_ATTRIB_ARRAY_ENABLED = $8622;
  GL_VERTEX_ATTRIB_ARRAY_SIZE = $8623;
  GL_VERTEX_ATTRIB_ARRAY_STRIDE = $8624;
  GL_VERTEX_ATTRIB_ARRAY_TYPE = $8625;
  GL_CURRENT_VERTEX_ATTRIB = $8626;
  GL_VERTEX_PROGRAM_POINT_SIZE = $8642;
  GL_VERTEX_ATTRIB_ARRAY_POINTER = $8645;
  GL_STENCIL_BACK_FUNC = $8800;
  GL_STENCIL_BACK_FAIL = $8801;
  GL_STENCIL_BACK_PASS_DEPTH_FAIL = $8802;
  GL_STENCIL_BACK_PASS_DEPTH_PASS = $8803;
  GL_MAX_DRAW_BUFFERS = $8824;
  GL_DRAW_BUFFER0 = $8825;
  GL_DRAW_BUFFER1 = $8826;
  GL_DRAW_BUFFER2 = $8827;
  GL_DRAW_BUFFER3 = $8828;
  GL_DRAW_BUFFER4 = $8829;
  GL_DRAW_BUFFER5 = $882A;
  GL_DRAW_BUFFER6 = $882B;
  GL_DRAW_BUFFER7 = $882C;
  GL_DRAW_BUFFER8 = $882D;
  GL_DRAW_BUFFER9 = $882E;
  GL_DRAW_BUFFER10 = $882F;
  GL_DRAW_BUFFER11 = $8830;
  GL_DRAW_BUFFER12 = $8831;
  GL_DRAW_BUFFER13 = $8832;
  GL_DRAW_BUFFER14 = $8833;
  GL_DRAW_BUFFER15 = $8834;
  GL_BLEND_EQUATION_ALPHA = $883D;
  GL_MAX_VERTEX_ATTRIBS = $8869;
  GL_VERTEX_ATTRIB_ARRAY_NORMALIZED = $886A;
  GL_MAX_TEXTURE_IMAGE_UNITS = $8872;
  GL_FRAGMENT_SHADER = $8B30;
  GL_VERTEX_SHADER = $8B31;
  GL_MAX_FRAGMENT_UNIFORM_COMPONENTS = $8B49;
  GL_MAX_VERTEX_UNIFORM_COMPONENTS = $8B4A;
  GL_MAX_VARYING_FLOATS = $8B4B;
  GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS = $8B4C;
  GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS = $8B4D;
  GL_SHADER_TYPE = $8B4F;
  GL_FLOAT_VEC2 = $8B50;
  GL_FLOAT_VEC3 = $8B51;
  GL_FLOAT_VEC4 = $8B52;
  GL_INT_VEC2 = $8B53;
  GL_INT_VEC3 = $8B54;
  GL_INT_VEC4 = $8B55;
  GL_BOOL = $8B56;
  GL_BOOL_VEC2 = $8B57;
  GL_BOOL_VEC3 = $8B58;
  GL_BOOL_VEC4 = $8B59;
  GL_FLOAT_MAT2 = $8B5A;
  GL_FLOAT_MAT3 = $8B5B;
  GL_FLOAT_MAT4 = $8B5C;
  GL_SAMPLER_1D = $8B5D;
  GL_SAMPLER_2D = $8B5E;
  GL_SAMPLER_3D = $8B5F;
  GL_SAMPLER_CUBE = $8B60;
  GL_SAMPLER_1D_SHADOW = $8B61;
  GL_SAMPLER_2D_SHADOW = $8B62;
  GL_DELETE_STATUS = $8B80;
  GL_COMPILE_STATUS = $8B81;
  GL_LINK_STATUS = $8B82;
  GL_VALIDATE_STATUS = $8B83;
  GL_INFO_LOG_LENGTH = $8B84;
  GL_ATTACHED_SHADERS = $8B85;
  GL_ACTIVE_UNIFORMS = $8B86;
  GL_ACTIVE_UNIFORM_MAX_LENGTH = $8B87;
  GL_SHADER_SOURCE_LENGTH = $8B88;
  GL_ACTIVE_ATTRIBUTES = $8B89;
  GL_ACTIVE_ATTRIBUTE_MAX_LENGTH = $8B8A;
  GL_FRAGMENT_SHADER_DERIVATIVE_HINT = $8B8B;
  GL_SHADING_LANGUAGE_VERSION = $8B8C;
  GL_CURRENT_PROGRAM = $8B8D;
  GL_POINT_SPRITE_COORD_ORIGIN = $8CA0;
  GL_LOWER_LEFT = $8CA1;
  GL_UPPER_LEFT = $8CA2;
  GL_STENCIL_BACK_REF = $8CA3;
  GL_STENCIL_BACK_VALUE_MASK = $8CA4;
  GL_STENCIL_BACK_WRITEMASK = $8CA5;

var
  glBlendEquationSeparate: procedure(modeRGB: GLenum; modeAlpha: GLenum); apicall;
  glDrawBuffers: procedure(n: GLsizei; bufs: PGLenum); apicall;
  glStencilOpSeparate: procedure(face: GLenum; sfail: GLenum; dpfail: GLenum; dppass: GLenum); apicall;
  glStencilFuncSeparate: procedure(face: GLenum; func: GLenum; ref: GLint; mask: GLuint); apicall;
  glStencilMaskSeparate: procedure(face: GLenum; mask: GLuint); apicall;
  glAttachShader: procedure(program_: GLuint; shader: GLuint); apicall;
  glBindAttribLocation: procedure(program_: GLuint; index: GLuint; name: PGLchar); apicall;
  glCompileShader: procedure(shader: GLuint); apicall;
  glCreateProgram: function: GLuint; apicall;
  glCreateShader: function(type_: GLenum): GLuint; apicall;
  glDeleteProgram: procedure(program_: GLuint); apicall;
  glDeleteShader: procedure(shader: GLuint); apicall;
  glDetachShader: procedure(program_: GLuint; shader: GLuint); apicall;
  glDisableVertexAttribArray: procedure(index: GLuint); apicall;
  glEnableVertexAttribArray: procedure(index: GLuint); apicall;
  glGetActiveAttrib: procedure(program_: GLuint; index: GLuint; bufSize: GLsizei; length: PGLsizei; size: PGLint; type_: PGLenum; name: PGLchar); apicall;
  glGetActiveUniform: procedure(program_: GLuint; index: GLuint; bufSize: GLsizei; length: PGLsizei; size: PGLint; type_: PGLenum; name: PGLchar); apicall;
  glGetAttachedShaders: procedure(program_: GLuint; maxCount: GLsizei; count: PGLsizei; shaders: PGLuint); apicall;
  glGetAttribLocation: function(program_: GLuint; name: PGLchar): GLint; apicall;
  glGetProgramiv: procedure(program_: GLuint; pname: GLenum; params: PGLint); apicall;
  glGetProgramInfoLog: procedure(program_: GLuint; bufSize: GLsizei; length: PGLsizei; infoLog: PGLchar); apicall;
  glGetShaderiv: procedure(shader: GLuint; pname: GLenum; params: PGLint); apicall;
  glGetShaderInfoLog: procedure(shader: GLuint; bufSize: GLsizei; length: PGLsizei; infoLog: PGLchar); apicall;
  glGetShaderSource: procedure(shader: GLuint; bufSize: GLsizei; length: PGLsizei; source: PGLchar); apicall;
  glGetUniformLocation: function(program_: GLuint; name: PGLchar): GLint; apicall;
  glGetUniformfv: procedure(program_: GLuint; location: GLint; params: PGLfloat); apicall;
  glGetUniformiv: procedure(program_: GLuint; location: GLint; params: PGLint); apicall;
  glGetVertexAttribdv: procedure(index: GLuint; pname: GLenum; params: PGLdouble); apicall;
  glGetVertexAttribfv: procedure(index: GLuint; pname: GLenum; params: PGLfloat); apicall;
  glGetVertexAttribiv: procedure(index: GLuint; pname: GLenum; params: PGLint); apicall;
  glGetVertexAttribPointerv: procedure(index: GLuint; pname: GLenum; pointer: PPointer); apicall;
  glIsProgram: function(program_: GLuint): GLboolean; apicall;
  glIsShader: function(shader: GLuint): GLboolean; apicall;
  glLinkProgram: procedure(program_: GLuint); apicall;
  glShaderSource: procedure(shader: GLuint; count: GLsizei; string_: PPGLchar; length: PGLint); apicall;
  glUseProgram: procedure(program_: GLuint); apicall;
  glUniform1f: procedure(location: GLint; v0: GLfloat); apicall;
  glUniform2f: procedure(location: GLint; v0: GLfloat; v1: GLfloat); apicall;
  glUniform3f: procedure(location: GLint; v0: GLfloat; v1: GLfloat; v2: GLfloat); apicall;
  glUniform4f: procedure(location: GLint; v0: GLfloat; v1: GLfloat; v2: GLfloat; v3: GLfloat); apicall;
  glUniform1i: procedure(location: GLint; v0: GLint); apicall;
  glUniform2i: procedure(location: GLint; v0: GLint; v1: GLint); apicall;
  glUniform3i: procedure(location: GLint; v0: GLint; v1: GLint; v2: GLint); apicall;
  glUniform4i: procedure(location: GLint; v0: GLint; v1: GLint; v2: GLint; v3: GLint); apicall;
  glUniform1fv: procedure(location: GLint; count: GLsizei; value: PGLfloat); apicall;
  glUniform2fv: procedure(location: GLint; count: GLsizei; value: PGLfloat); apicall;
  glUniform3fv: procedure(location: GLint; count: GLsizei; value: PGLfloat); apicall;
  glUniform4fv: procedure(location: GLint; count: GLsizei; value: PGLfloat); apicall;
  glUniform1iv: procedure(location: GLint; count: GLsizei; value: PGLint); apicall;
  glUniform2iv: procedure(location: GLint; count: GLsizei; value: PGLint); apicall;
  glUniform3iv: procedure(location: GLint; count: GLsizei; value: PGLint); apicall;
  glUniform4iv: procedure(location: GLint; count: GLsizei; value: PGLint); apicall;
  glUniformMatrix2fv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); apicall;
  glUniformMatrix3fv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); apicall;
  glUniformMatrix4fv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); apicall;
  glValidateProgram: procedure(program_: GLuint); apicall;
  glVertexAttrib1d: procedure(index: GLuint; x: GLdouble); apicall;
  glVertexAttrib1dv: procedure(index: GLuint; v: PGLdouble); apicall;
  glVertexAttrib1f: procedure(index: GLuint; x: GLfloat); apicall;
  glVertexAttrib1fv: procedure(index: GLuint; v: PGLfloat); apicall;
  glVertexAttrib1s: procedure(index: GLuint; x: GLshort); apicall;
  glVertexAttrib1sv: procedure(index: GLuint; v: PGLshort); apicall;
  glVertexAttrib2d: procedure(index: GLuint; x: GLdouble; y: GLdouble); apicall;
  glVertexAttrib2dv: procedure(index: GLuint; v: PGLdouble); apicall;
  glVertexAttrib2f: procedure(index: GLuint; x: GLfloat; y: GLfloat); apicall;
  glVertexAttrib2fv: procedure(index: GLuint; v: PGLfloat); apicall;
  glVertexAttrib2s: procedure(index: GLuint; x: GLshort; y: GLshort); apicall;
  glVertexAttrib2sv: procedure(index: GLuint; v: PGLshort); apicall;
  glVertexAttrib3d: procedure(index: GLuint; x: GLdouble; y: GLdouble; z: GLdouble); apicall;
  glVertexAttrib3dv: procedure(index: GLuint; v: PGLdouble); apicall;
  glVertexAttrib3f: procedure(index: GLuint; x: GLfloat; y: GLfloat; z: GLfloat); apicall;
  glVertexAttrib3fv: procedure(index: GLuint; v: PGLfloat); apicall;
  glVertexAttrib3s: procedure(index: GLuint; x: GLshort; y: GLshort; z: GLshort); apicall;
  glVertexAttrib3sv: procedure(index: GLuint; v: PGLshort); apicall;
  glVertexAttrib4Nbv: procedure(index: GLuint; v: PGLbyte); apicall;
  glVertexAttrib4Niv: procedure(index: GLuint; v: PGLint); apicall;
  glVertexAttrib4Nsv: procedure(index: GLuint; v: PGLshort); apicall;
  glVertexAttrib4Nub: procedure(index: GLuint; x: GLubyte; y: GLubyte; z: GLubyte; w: GLubyte); apicall;
  glVertexAttrib4Nubv: procedure(index: GLuint; v: PGLubyte); apicall;
  glVertexAttrib4Nuiv: procedure(index: GLuint; v: PGLuint); apicall;
  glVertexAttrib4Nusv: procedure(index: GLuint; v: PGLushort); apicall;
  glVertexAttrib4bv: procedure(index: GLuint; v: PGLbyte); apicall;
  glVertexAttrib4d: procedure(index: GLuint; x: GLdouble; y: GLdouble; z: GLdouble; w: GLdouble); apicall;
  glVertexAttrib4dv: procedure(index: GLuint; v: PGLdouble); apicall;
  glVertexAttrib4f: procedure(index: GLuint; x: GLfloat; y: GLfloat; z: GLfloat; w: GLfloat); apicall;
  glVertexAttrib4fv: procedure(index: GLuint; v: PGLfloat); apicall;
  glVertexAttrib4iv: procedure(index: GLuint; v: PGLint); apicall;
  glVertexAttrib4s: procedure(index: GLuint; x: GLshort; y: GLshort; z: GLshort; w: GLshort); apicall;
  glVertexAttrib4sv: procedure(index: GLuint; v: PGLshort); apicall;
  glVertexAttrib4ubv: procedure(index: GLuint; v: PGLubyte); apicall;
  glVertexAttrib4uiv: procedure(index: GLuint; v: PGLuint); apicall;
  glVertexAttrib4usv: procedure(index: GLuint; v: PGLushort); apicall;
  glVertexAttribPointer: procedure(index: GLuint; size: GLint; type_: GLenum; normalized: GLboolean; stride: GLsizei; pointer: Pointer); apicall;
{$endregion}

{ OpenGL 2.0 compatibility profile }

{$region gl20 compatibility}
{$ifdef glcompat}
const
  GL_VERTEX_PROGRAM_TWO_SIDE = $8643;
  GL_POINT_SPRITE = $8861;
  GL_COORD_REPLACE = $8862;
  GL_MAX_TEXTURE_COORDS = $8871;
{$endif}
{$endregion}

{ OpenGL 2.1 }

{$region gl21}
const
  GL_PIXEL_PACK_BUFFER = $88EB;
  GL_PIXEL_UNPACK_BUFFER = $88EC;
  GL_PIXEL_PACK_BUFFER_BINDING = $88ED;
  GL_PIXEL_UNPACK_BUFFER_BINDING = $88EF;
  GL_FLOAT_MAT2x3 = $8B65;
  GL_FLOAT_MAT2x4 = $8B66;
  GL_FLOAT_MAT3x2 = $8B67;
  GL_FLOAT_MAT3x4 = $8B68;
  GL_FLOAT_MAT4x2 = $8B69;
  GL_FLOAT_MAT4x3 = $8B6A;
  GL_SRGB = $8C40;
  GL_SRGB8 = $8C41;
  GL_SRGB_ALPHA = $8C42;
  GL_SRGB8_ALPHA8 = $8C43;
  GL_COMPRESSED_SRGB = $8C48;
  GL_COMPRESSED_SRGB_ALPHA = $8C49;

var
  glUniformMatrix2x3fv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); apicall;
  glUniformMatrix3x2fv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); apicall;
  glUniformMatrix2x4fv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); apicall;
  glUniformMatrix4x2fv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); apicall;
  glUniformMatrix3x4fv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); apicall;
  glUniformMatrix4x3fv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); apicall;
{$endregion}

{ OpenGL 2.1 compatibility profile }

{$region gl21 compatibility}
{$ifdef glcompat}
const
  GL_CURRENT_RASTER_SECONDARY_COLOR = $845F;
  GL_SLUMINANCE_ALPHA = $8C44;
  GL_SLUMINANCE8_ALPHA8 = $8C45;
  GL_SLUMINANCE = $8C46;
  GL_SLUMINANCE8 = $8C47;
  GL_COMPRESSED_SLUMINANCE = $8C4A;
  GL_COMPRESSED_SLUMINANCE_ALPHA = $8C4B;
{$endif}
{$endregion}

{ OpenGL 3.0 }

{$region gl30}
{$ifdef gl30}
const
  GL_COMPARE_REF_TO_TEXTURE = $884E;
  GL_CLIP_DISTANCE0 = $3000;
  GL_CLIP_DISTANCE1 = $3001;
  GL_CLIP_DISTANCE2 = $3002;
  GL_CLIP_DISTANCE3 = $3003;
  GL_CLIP_DISTANCE4 = $3004;
  GL_CLIP_DISTANCE5 = $3005;
  GL_CLIP_DISTANCE6 = $3006;
  GL_CLIP_DISTANCE7 = $3007;
  GL_MAX_CLIP_DISTANCES = $0D32;
  GL_MAJOR_VERSION = $821B;
  GL_MINOR_VERSION = $821C;
  GL_NUM_EXTENSIONS = $821D;
  GL_CONTEXT_FLAGS = $821E;
  GL_COMPRESSED_RED = $8225;
  GL_COMPRESSED_RG = $8226;
  GL_CONTEXT_FLAG_FORWARD_COMPATIBLE_BIT = $00000001;
  GL_RGBA32F = $8814;
  GL_RGB32F = $8815;
  GL_RGBA16F = $881A;
  GL_RGB16F = $881B;
  GL_VERTEX_ATTRIB_ARRAY_INTEGER = $88FD;
  GL_MAX_ARRAY_TEXTURE_LAYERS = $88FF;
  GL_MIN_PROGRAM_TEXEL_OFFSET = $8904;
  GL_MAX_PROGRAM_TEXEL_OFFSET = $8905;
  GL_CLAMP_READ_COLOR = $891C;
  GL_FIXED_ONLY = $891D;
  GL_MAX_VARYING_COMPONENTS = $8B4B;
  GL_TEXTURE_1D_ARRAY = $8C18;
  GL_PROXY_TEXTURE_1D_ARRAY = $8C19;
  GL_TEXTURE_2D_ARRAY = $8C1A;
  GL_PROXY_TEXTURE_2D_ARRAY = $8C1B;
  GL_TEXTURE_BINDING_1D_ARRAY = $8C1C;
  GL_TEXTURE_BINDING_2D_ARRAY = $8C1D;
  GL_R11F_G11F_B10F = $8C3A;
  GL_UNSIGNED_INT_10F_11F_11F_REV = $8C3B;
  GL_RGB9_E5 = $8C3D;
  GL_UNSIGNED_INT_5_9_9_9_REV = $8C3E;
  GL_TEXTURE_SHARED_SIZE = $8C3F;
  GL_TRANSFORM_FEEDBACK_VARYING_MAX_LENGTH = $8C76;
  GL_TRANSFORM_FEEDBACK_BUFFER_MODE = $8C7F;
  GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS = $8C80;
  GL_TRANSFORM_FEEDBACK_VARYINGS = $8C83;
  GL_TRANSFORM_FEEDBACK_BUFFER_START = $8C84;
  GL_TRANSFORM_FEEDBACK_BUFFER_SIZE = $8C85;
  GL_PRIMITIVES_GENERATED = $8C87;
  GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN = $8C88;
  GL_RASTERIZER_DISCARD = $8C89;
  GL_MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS = $8C8A;
  GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS = $8C8B;
  GL_INTERLEAVED_ATTRIBS = $8C8C;
  GL_SEPARATE_ATTRIBS = $8C8D;
  GL_TRANSFORM_FEEDBACK_BUFFER = $8C8E;
  GL_TRANSFORM_FEEDBACK_BUFFER_BINDING = $8C8F;
  GL_RGBA32UI = $8D70;
  GL_RGB32UI = $8D71;
  GL_RGBA16UI = $8D76;
  GL_RGB16UI = $8D77;
  GL_RGBA8UI = $8D7C;
  GL_RGB8UI = $8D7D;
  GL_RGBA32I = $8D82;
  GL_RGB32I = $8D83;
  GL_RGBA16I = $8D88;
  GL_RGB16I = $8D89;
  GL_RGBA8I = $8D8E;
  GL_RGB8I = $8D8F;
  GL_RED_INTEGER = $8D94;
  GL_GREEN_INTEGER = $8D95;
  GL_BLUE_INTEGER = $8D96;
  GL_RGB_INTEGER = $8D98;
  GL_RGBA_INTEGER = $8D99;
  GL_BGR_INTEGER = $8D9A;
  GL_BGRA_INTEGER = $8D9B;
  GL_SAMPLER_1D_ARRAY = $8DC0;
  GL_SAMPLER_2D_ARRAY = $8DC1;
  GL_SAMPLER_1D_ARRAY_SHADOW = $8DC3;
  GL_SAMPLER_2D_ARRAY_SHADOW = $8DC4;
  GL_SAMPLER_CUBE_SHADOW = $8DC5;
  GL_UNSIGNED_INT_VEC2 = $8DC6;
  GL_UNSIGNED_INT_VEC3 = $8DC7;
  GL_UNSIGNED_INT_VEC4 = $8DC8;
  GL_INT_SAMPLER_1D = $8DC9;
  GL_INT_SAMPLER_2D = $8DCA;
  GL_INT_SAMPLER_3D = $8DCB;
  GL_INT_SAMPLER_CUBE = $8DCC;
  GL_INT_SAMPLER_1D_ARRAY = $8DCE;
  GL_INT_SAMPLER_2D_ARRAY = $8DCF;
  GL_UNSIGNED_INT_SAMPLER_1D = $8DD1;
  GL_UNSIGNED_INT_SAMPLER_2D = $8DD2;
  GL_UNSIGNED_INT_SAMPLER_3D = $8DD3;
  GL_UNSIGNED_INT_SAMPLER_CUBE = $8DD4;
  GL_UNSIGNED_INT_SAMPLER_1D_ARRAY = $8DD6;
  GL_UNSIGNED_INT_SAMPLER_2D_ARRAY = $8DD7;
  GL_QUERY_WAIT = $8E13;
  GL_QUERY_NO_WAIT = $8E14;
  GL_QUERY_BY_REGION_WAIT = $8E15;
  GL_QUERY_BY_REGION_NO_WAIT = $8E16;
  GL_BUFFER_ACCESS_FLAGS = $911F;
  GL_BUFFER_MAP_LENGTH = $9120;
  GL_BUFFER_MAP_OFFSET = $9121;
  GL_DEPTH_COMPONENT32F = $8CAC;
  GL_DEPTH32F_STENCIL8 = $8CAD;
  GL_FLOAT_32_UNSIGNED_INT_24_8_REV = $8DAD;
  GL_INVALID_FRAMEBUFFER_OPERATION = $0506;
  GL_FRAMEBUFFER_ATTACHMENT_COLOR_ENCODING = $8210;
  GL_FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE = $8211;
  GL_FRAMEBUFFER_ATTACHMENT_RED_SIZE = $8212;
  GL_FRAMEBUFFER_ATTACHMENT_GREEN_SIZE = $8213;
  GL_FRAMEBUFFER_ATTACHMENT_BLUE_SIZE = $8214;
  GL_FRAMEBUFFER_ATTACHMENT_ALPHA_SIZE = $8215;
  GL_FRAMEBUFFER_ATTACHMENT_DEPTH_SIZE = $8216;
  GL_FRAMEBUFFER_ATTACHMENT_STENCIL_SIZE = $8217;
  GL_FRAMEBUFFER_DEFAULT = $8218;
  GL_FRAMEBUFFER_UNDEFINED = $8219;
  GL_DEPTH_STENCIL_ATTACHMENT = $821A;
  GL_MAX_RENDERBUFFER_SIZE = $84E8;
  GL_DEPTH_STENCIL = $84F9;
  GL_UNSIGNED_INT_24_8 = $84FA;
  GL_DEPTH24_STENCIL8 = $88F0;
  GL_TEXTURE_STENCIL_SIZE = $88F1;
  GL_TEXTURE_RED_TYPE = $8C10;
  GL_TEXTURE_GREEN_TYPE = $8C11;
  GL_TEXTURE_BLUE_TYPE = $8C12;
  GL_TEXTURE_ALPHA_TYPE = $8C13;
  GL_TEXTURE_DEPTH_TYPE = $8C16;
  GL_UNSIGNED_NORMALIZED = $8C17;
  GL_FRAMEBUFFER_BINDING = $8CA6;
  GL_DRAW_FRAMEBUFFER_BINDING = $8CA6;
  GL_RENDERBUFFER_BINDING = $8CA7;
  GL_READ_FRAMEBUFFER = $8CA8;
  GL_DRAW_FRAMEBUFFER = $8CA9;
  GL_READ_FRAMEBUFFER_BINDING = $8CAA;
  GL_RENDERBUFFER_SAMPLES = $8CAB;
  GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE = $8CD0;
  GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME = $8CD1;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL = $8CD2;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE = $8CD3;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER = $8CD4;
  GL_FRAMEBUFFER_COMPLETE = $8CD5;
  GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT = $8CD6;
  GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT = $8CD7;
  GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER = $8CDB;
  GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER = $8CDC;
  GL_FRAMEBUFFER_UNSUPPORTED = $8CDD;
  GL_MAX_COLOR_ATTACHMENTS = $8CDF;
  GL_COLOR_ATTACHMENT0 = $8CE0;
  GL_COLOR_ATTACHMENT1 = $8CE1;
  GL_COLOR_ATTACHMENT2 = $8CE2;
  GL_COLOR_ATTACHMENT3 = $8CE3;
  GL_COLOR_ATTACHMENT4 = $8CE4;
  GL_COLOR_ATTACHMENT5 = $8CE5;
  GL_COLOR_ATTACHMENT6 = $8CE6;
  GL_COLOR_ATTACHMENT7 = $8CE7;
  GL_COLOR_ATTACHMENT8 = $8CE8;
  GL_COLOR_ATTACHMENT9 = $8CE9;
  GL_COLOR_ATTACHMENT10 = $8CEA;
  GL_COLOR_ATTACHMENT11 = $8CEB;
  GL_COLOR_ATTACHMENT12 = $8CEC;
  GL_COLOR_ATTACHMENT13 = $8CED;
  GL_COLOR_ATTACHMENT14 = $8CEE;
  GL_COLOR_ATTACHMENT15 = $8CEF;
  GL_COLOR_ATTACHMENT16 = $8CF0;
  GL_COLOR_ATTACHMENT17 = $8CF1;
  GL_COLOR_ATTACHMENT18 = $8CF2;
  GL_COLOR_ATTACHMENT19 = $8CF3;
  GL_COLOR_ATTACHMENT20 = $8CF4;
  GL_COLOR_ATTACHMENT21 = $8CF5;
  GL_COLOR_ATTACHMENT22 = $8CF6;
  GL_COLOR_ATTACHMENT23 = $8CF7;
  GL_COLOR_ATTACHMENT24 = $8CF8;
  GL_COLOR_ATTACHMENT25 = $8CF9;
  GL_COLOR_ATTACHMENT26 = $8CFA;
  GL_COLOR_ATTACHMENT27 = $8CFB;
  GL_COLOR_ATTACHMENT28 = $8CFC;
  GL_COLOR_ATTACHMENT29 = $8CFD;
  GL_COLOR_ATTACHMENT30 = $8CFE;
  GL_COLOR_ATTACHMENT31 = $8CFF;
  GL_DEPTH_ATTACHMENT = $8D00;
  GL_STENCIL_ATTACHMENT = $8D20;
  GL_FRAMEBUFFER = $8D40;
  GL_RENDERBUFFER = $8D41;
  GL_RENDERBUFFER_WIDTH = $8D42;
  GL_RENDERBUFFER_HEIGHT = $8D43;
  GL_RENDERBUFFER_INTERNAL_FORMAT = $8D44;
  GL_STENCIL_INDEX1 = $8D46;
  GL_STENCIL_INDEX4 = $8D47;
  GL_STENCIL_INDEX8 = $8D48;
  GL_STENCIL_INDEX16 = $8D49;
  GL_RENDERBUFFER_RED_SIZE = $8D50;
  GL_RENDERBUFFER_GREEN_SIZE = $8D51;
  GL_RENDERBUFFER_BLUE_SIZE = $8D52;
  GL_RENDERBUFFER_ALPHA_SIZE = $8D53;
  GL_RENDERBUFFER_DEPTH_SIZE = $8D54;
  GL_RENDERBUFFER_STENCIL_SIZE = $8D55;
  GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE = $8D56;
  GL_MAX_SAMPLES = $8D57;
  GL_FRAMEBUFFER_SRGB = $8DB9;
  GL_HALF_FLOAT = $140B;
  GL_MAP_READ_BIT = $0001;
  GL_MAP_WRITE_BIT = $0002;
  GL_MAP_INVALIDATE_RANGE_BIT = $0004;
  GL_MAP_INVALIDATE_BUFFER_BIT = $0008;
  GL_MAP_FLUSH_EXPLICIT_BIT = $0010;
  GL_MAP_UNSYNCHRONIZED_BIT = $0020;
  GL_COMPRESSED_RED_RGTC1 = $8DBB;
  GL_COMPRESSED_SIGNED_RED_RGTC1 = $8DBC;
  GL_COMPRESSED_RG_RGTC2 = $8DBD;
  GL_COMPRESSED_SIGNED_RG_RGTC2 = $8DBE;
  GL_RG = $8227;
  GL_RG_INTEGER = $8228;
  GL_R8 = $8229;
  GL_R16 = $822A;
  GL_RG8 = $822B;
  GL_RG16 = $822C;
  GL_R16F = $822D;
  GL_R32F = $822E;
  GL_RG16F = $822F;
  GL_RG32F = $8230;
  GL_R8I = $8231;
  GL_R8UI = $8232;
  GL_R16I = $8233;
  GL_R16UI = $8234;
  GL_R32I = $8235;
  GL_R32UI = $8236;
  GL_RG8I = $8237;
  GL_RG8UI = $8238;
  GL_RG16I = $8239;
  GL_RG16UI = $823A;
  GL_RG32I = $823B;
  GL_RG32UI = $823C;
  GL_VERTEX_ARRAY_BINDING = $85B5;

var
  glColorMaski: procedure(index: GLuint; r: GLboolean; g: GLboolean; b: GLboolean; a: GLboolean); apicall;
  glGetBooleani_v: procedure(target: GLenum; index: GLuint; data: PGLboolean); apicall;
  glGetIntegeri_v: procedure(target: GLenum; index: GLuint; data: PGLint); apicall;
  glEnablei: procedure(target: GLenum; index: GLuint); apicall;
  glDisablei: procedure(target: GLenum; index: GLuint); apicall;
  glIsEnabledi: function(target: GLenum; index: GLuint): GLboolean; apicall;
  glBeginTransformFeedback: procedure(primitiveMode: GLenum); apicall;
  glEndTransformFeedback: procedure; apicall;
  glBindBufferRange: procedure(target: GLenum; index: GLuint; buffer: GLuint; offset: GLintptr; size: GLsizeiptr); apicall;
  glBindBufferBase: procedure(target: GLenum; index: GLuint; buffer: GLuint); apicall;
  glTransformFeedbackVaryings: procedure(program_: GLuint; count: GLsizei; varyings: PPGLchar; bufferMode: GLenum); apicall;
  glGetTransformFeedbackVarying: procedure(program_: GLuint; index: GLuint; bufSize: GLsizei; length: PGLsizei; size: PGLsizei; type_: PGLenum; name: PGLchar); apicall;
  glClampColor: procedure(target: GLenum; clamp: GLenum); apicall;
  glBeginConditionalRender: procedure(id: GLuint; mode: GLenum); apicall;
  glEndConditionalRender: procedure; apicall;
  glVertexAttribIPointer: procedure(index: GLuint; size: GLint; type_: GLenum; stride: GLsizei; pointer: Pointer); apicall;
  glGetVertexAttribIiv: procedure(index: GLuint; pname: GLenum; params: PGLint); apicall;
  glGetVertexAttribIuiv: procedure(index: GLuint; pname: GLenum; params: PGLuint); apicall;
  glVertexAttribI1i: procedure(index: GLuint; x: GLint); apicall;
  glVertexAttribI2i: procedure(index: GLuint; x: GLint; y: GLint); apicall;
  glVertexAttribI3i: procedure(index: GLuint; x: GLint; y: GLint; z: GLint); apicall;
  glVertexAttribI4i: procedure(index: GLuint; x: GLint; y: GLint; z: GLint; w: GLint); apicall;
  glVertexAttribI1ui: procedure(index: GLuint; x: GLuint); apicall;
  glVertexAttribI2ui: procedure(index: GLuint; x: GLuint; y: GLuint); apicall;
  glVertexAttribI3ui: procedure(index: GLuint; x: GLuint; y: GLuint; z: GLuint); apicall;
  glVertexAttribI4ui: procedure(index: GLuint; x: GLuint; y: GLuint; z: GLuint; w: GLuint); apicall;
  glVertexAttribI1iv: procedure(index: GLuint; v: PGLint); apicall;
  glVertexAttribI2iv: procedure(index: GLuint; v: PGLint); apicall;
  glVertexAttribI3iv: procedure(index: GLuint; v: PGLint); apicall;
  glVertexAttribI4iv: procedure(index: GLuint; v: PGLint); apicall;
  glVertexAttribI1uiv: procedure(index: GLuint; v: PGLuint); apicall;
  glVertexAttribI2uiv: procedure(index: GLuint; v: PGLuint); apicall;
  glVertexAttribI3uiv: procedure(index: GLuint; v: PGLuint); apicall;
  glVertexAttribI4uiv: procedure(index: GLuint; v: PGLuint); apicall;
  glVertexAttribI4bv: procedure(index: GLuint; v: PGLbyte); apicall;
  glVertexAttribI4sv: procedure(index: GLuint; v: PGLshort); apicall;
  glVertexAttribI4ubv: procedure(index: GLuint; v: PGLubyte); apicall;
  glVertexAttribI4usv: procedure(index: GLuint; v: PGLushort); apicall;
  glGetUniformuiv: procedure(program_: GLuint; location: GLint; params: PGLuint); apicall;
  glBindFragDataLocation: procedure(program_: GLuint; color: GLuint; name: PGLchar); apicall;
  glGetFragDataLocation: function(program_: GLuint; name: PGLchar): GLint; apicall;
  glUniform1ui: procedure(location: GLint; v0: GLuint); apicall;
  glUniform2ui: procedure(location: GLint; v0: GLuint; v1: GLuint); apicall;
  glUniform3ui: procedure(location: GLint; v0: GLuint; v1: GLuint; v2: GLuint); apicall;
  glUniform4ui: procedure(location: GLint; v0: GLuint; v1: GLuint; v2: GLuint; v3: GLuint); apicall;
  glUniform1uiv: procedure(location: GLint; count: GLsizei; value: PGLuint); apicall;
  glUniform2uiv: procedure(location: GLint; count: GLsizei; value: PGLuint); apicall;
  glUniform3uiv: procedure(location: GLint; count: GLsizei; value: PGLuint); apicall;
  glUniform4uiv: procedure(location: GLint; count: GLsizei; value: PGLuint); apicall;
  glTexParameterIiv: procedure(target: GLenum; pname: GLenum; params: PGLint); apicall;
  glTexParameterIuiv: procedure(target: GLenum; pname: GLenum; params: PGLuint); apicall;
  glGetTexParameterIiv: procedure(target: GLenum; pname: GLenum; params: PGLint); apicall;
  glGetTexParameterIuiv: procedure(target: GLenum; pname: GLenum; params: PGLuint); apicall;
  glClearBufferiv: procedure(buffer: GLenum; drawbuffer: GLint; value: PGLint); apicall;
  glClearBufferuiv: procedure(buffer: GLenum; drawbuffer: GLint; value: PGLuint); apicall;
  glClearBufferfv: procedure(buffer: GLenum; drawbuffer: GLint; value: PGLfloat); apicall;
  glClearBufferfi: procedure(buffer: GLenum; drawbuffer: GLint; depth: GLfloat; stencil: GLint); apicall;
  glGetStringi: function(name: GLenum; index: GLuint): PGLubyte; apicall;
  glIsRenderbuffer: function(renderbuffer: GLuint): GLboolean; apicall;
  glBindRenderbuffer: procedure(target: GLenum; renderbuffer: GLuint); apicall;
  glDeleteRenderbuffers: procedure(n: GLsizei; renderbuffers: PGLuint); apicall;
  glGenRenderbuffers: procedure(n: GLsizei; renderbuffers: PGLuint); apicall;
  glRenderbufferStorage: procedure(target: GLenum; internalformat: GLenum; width: GLsizei; height: GLsizei); apicall;
  glGetRenderbufferParameteriv: procedure(target: GLenum; pname: GLenum; params: PGLint); apicall;
  glIsFramebuffer: function(framebuffer: GLuint): GLboolean; apicall;
  glBindFramebuffer: procedure(target: GLenum; framebuffer: GLuint); apicall;
  glDeleteFramebuffers: procedure(n: GLsizei; framebuffers: PGLuint); apicall;
  glGenFramebuffers: procedure(n: GLsizei; framebuffers: PGLuint); apicall;
  glCheckFramebufferStatus: function(target: GLenum): GLenum; apicall;
  glFramebufferTexture1D: procedure(target: GLenum; attachment: GLenum; textarget: GLenum; texture: GLuint; level: GLint); apicall;
  glFramebufferTexture2D: procedure(target: GLenum; attachment: GLenum; textarget: GLenum; texture: GLuint; level: GLint); apicall;
  glFramebufferTexture3D: procedure(target: GLenum; attachment: GLenum; textarget: GLenum; texture: GLuint; level: GLint; zoffset: GLint); apicall;
  glFramebufferRenderbuffer: procedure(target: GLenum; attachment: GLenum; renderbuffertarget: GLenum; renderbuffer: GLuint); apicall;
  glGetFramebufferAttachmentParameteriv: procedure(target: GLenum; attachment: GLenum; pname: GLenum; params: PGLint); apicall;
  glGenerateMipmap: procedure(target: GLenum); apicall;
  glBlitFramebuffer: procedure(srcX0: GLint; srcY0: GLint; srcX1: GLint; srcY1: GLint; dstX0: GLint; dstY0: GLint; dstX1: GLint; dstY1: GLint; mask: GLbitfield; filter: GLenum); apicall;
  glRenderbufferStorageMultisample: procedure(target: GLenum; samples: GLsizei; internalformat: GLenum; width: GLsizei; height: GLsizei); apicall;
  glFramebufferTextureLayer: procedure(target: GLenum; attachment: GLenum; texture: GLuint; level: GLint; layer: GLint); apicall;
  glMapBufferRange: function(target: GLenum; offset: GLintptr; length: GLsizeiptr; access: GLbitfield): Pointer; apicall;
  glFlushMappedBufferRange: procedure(target: GLenum; offset: GLintptr; length: GLsizeiptr); apicall;
  glBindVertexArray: procedure(array_: GLuint); apicall;
  glDeleteVertexArrays: procedure(n: GLsizei; arrays: PGLuint); apicall;
  glGenVertexArrays: procedure(n: GLsizei; arrays: PGLuint); apicall;
  glIsVertexArray: function(array_: GLuint): GLboolean; apicall;
{$endif}
{$endregion}

{ OpenGL 3.0 compatibility profile }

{$region gl30 compatibility}
{$if defined(gl30) and defined(glcompat)}
const
  GL_CLAMP_VERTEX_COLOR = $891A;
  GL_CLAMP_FRAGMENT_COLOR = $891B;
  GL_ALPHA_INTEGER = $8D97;
  GL_INDEX = $8222;
  GL_TEXTURE_LUMINANCE_TYPE = $8C14;
  GL_TEXTURE_INTENSITY_TYPE = $8C15;
{$endif}
{$endregion}

{ OpenGL 3.1 }

{$region gl31}
{$ifdef gl31}
const
  GL_SAMPLER_2D_RECT = $8B63;
  GL_SAMPLER_2D_RECT_SHADOW = $8B64;
  GL_SAMPLER_BUFFER = $8DC2;
  GL_INT_SAMPLER_2D_RECT = $8DCD;
  GL_INT_SAMPLER_BUFFER = $8DD0;
  GL_UNSIGNED_INT_SAMPLER_2D_RECT = $8DD5;
  GL_UNSIGNED_INT_SAMPLER_BUFFER = $8DD8;
  GL_TEXTURE_BUFFER = $8C2A;
  GL_MAX_TEXTURE_BUFFER_SIZE = $8C2B;
  GL_TEXTURE_BINDING_BUFFER = $8C2C;
  GL_TEXTURE_BUFFER_DATA_STORE_BINDING = $8C2D;
  GL_TEXTURE_RECTANGLE = $84F5;
  GL_TEXTURE_BINDING_RECTANGLE = $84F6;
  GL_PROXY_TEXTURE_RECTANGLE = $84F7;
  GL_MAX_RECTANGLE_TEXTURE_SIZE = $84F8;
  GL_R8_SNORM = $8F94;
  GL_RG8_SNORM = $8F95;
  GL_RGB8_SNORM = $8F96;
  GL_RGBA8_SNORM = $8F97;
  GL_R16_SNORM = $8F98;
  GL_RG16_SNORM = $8F99;
  GL_RGB16_SNORM = $8F9A;
  GL_RGBA16_SNORM = $8F9B;
  GL_SIGNED_NORMALIZED = $8F9C;
  GL_PRIMITIVE_RESTART = $8F9D;
  GL_PRIMITIVE_RESTART_INDEX = $8F9E;
  GL_COPY_READ_BUFFER = $8F36;
  GL_COPY_WRITE_BUFFER = $8F37;
  GL_UNIFORM_BUFFER = $8A11;
  GL_UNIFORM_BUFFER_BINDING = $8A28;
  GL_UNIFORM_BUFFER_START = $8A29;
  GL_UNIFORM_BUFFER_SIZE = $8A2A;
  GL_MAX_VERTEX_UNIFORM_BLOCKS = $8A2B;
  GL_MAX_GEOMETRY_UNIFORM_BLOCKS = $8A2C;
  GL_MAX_FRAGMENT_UNIFORM_BLOCKS = $8A2D;
  GL_MAX_COMBINED_UNIFORM_BLOCKS = $8A2E;
  GL_MAX_UNIFORM_BUFFER_BINDINGS = $8A2F;
  GL_MAX_UNIFORM_BLOCK_SIZE = $8A30;
  GL_MAX_COMBINED_VERTEX_UNIFORM_COMPONENTS = $8A31;
  GL_MAX_COMBINED_GEOMETRY_UNIFORM_COMPONENTS = $8A32;
  GL_MAX_COMBINED_FRAGMENT_UNIFORM_COMPONENTS = $8A33;
  GL_UNIFORM_BUFFER_OFFSET_ALIGNMENT = $8A34;
  GL_ACTIVE_UNIFORM_BLOCK_MAX_NAME_LENGTH = $8A35;
  GL_ACTIVE_UNIFORM_BLOCKS = $8A36;
  GL_UNIFORM_TYPE = $8A37;
  GL_UNIFORM_SIZE = $8A38;
  GL_UNIFORM_NAME_LENGTH = $8A39;
  GL_UNIFORM_BLOCK_INDEX = $8A3A;
  GL_UNIFORM_OFFSET = $8A3B;
  GL_UNIFORM_ARRAY_STRIDE = $8A3C;
  GL_UNIFORM_MATRIX_STRIDE = $8A3D;
  GL_UNIFORM_IS_ROW_MAJOR = $8A3E;
  GL_UNIFORM_BLOCK_BINDING = $8A3F;
  GL_UNIFORM_BLOCK_DATA_SIZE = $8A40;
  GL_UNIFORM_BLOCK_NAME_LENGTH = $8A41;
  GL_UNIFORM_BLOCK_ACTIVE_UNIFORMS = $8A42;
  GL_UNIFORM_BLOCK_ACTIVE_UNIFORM_INDICES = $8A43;
  GL_UNIFORM_BLOCK_REFERENCED_BY_VERTEX_SHADER = $8A44;
  GL_UNIFORM_BLOCK_REFERENCED_BY_GEOMETRY_SHADER = $8A45;
  GL_UNIFORM_BLOCK_REFERENCED_BY_FRAGMENT_SHADER = $8A46;
  GL_INVALID_INDEX = $FFFFFFFF;

var
  glDrawArraysInstanced: procedure(mode: GLenum; first: GLint; count: GLsizei; instancecount: GLsizei); apicall;
  glDrawElementsInstanced: procedure(mode: GLenum; count: GLsizei; type_: GLenum; indices: Pointer; instancecount: GLsizei); apicall;
  glTexBuffer: procedure(target: GLenum; internalformat: GLenum; buffer: GLuint); apicall;
  glPrimitiveRestartIndex: procedure(index: GLuint); apicall;
  glCopyBufferSubData: procedure(readTarget: GLenum; writeTarget: GLenum; readOffset: GLintptr; writeOffset: GLintptr; size: GLsizeiptr); apicall;
  glGetUniformIndices: procedure(program_: GLuint; uniformCount: GLsizei; uniformNames: PPGLchar; uniformIndices: PGLuint); apicall;
  glGetActiveUniformsiv: procedure(program_: GLuint; uniformCount: GLsizei; uniformIndices: PGLuint; pname: GLenum; params: PGLint); apicall;
  glGetActiveUniformName: procedure(program_: GLuint; uniformIndex: GLuint; bufSize: GLsizei; length: PGLsizei; uniformName: PGLchar); apicall;
  glGetUniformBlockIndex: function(program_: GLuint; uniformBlockName: PGLchar): GLuint; apicall;
  glGetActiveUniformBlockiv: procedure(program_: GLuint; uniformBlockIndex: GLuint; pname: GLenum; params: PGLint); apicall;
  glGetActiveUniformBlockName: procedure(program_: GLuint; uniformBlockIndex: GLuint; bufSize: GLsizei; length: PGLsizei; uniformBlockName: PGLchar); apicall;
  glUniformBlockBinding: procedure(program_: GLuint; uniformBlockIndex: GLuint; uniformBlockBinding: GLuint); apicall;
{$endif}
{$endregion}

{ OpenGL 3.2 }

{$region gl32}
{$ifdef gl32}
const
  GL_CONTEXT_CORE_PROFILE_BIT = $00000001;
  GL_CONTEXT_COMPATIBILITY_PROFILE_BIT = $00000002;
  GL_LINES_ADJACENCY = $000A;
  GL_LINE_STRIP_ADJACENCY = $000B;
  GL_TRIANGLES_ADJACENCY = $000C;
  GL_TRIANGLE_STRIP_ADJACENCY = $000D;
  GL_PROGRAM_POINT_SIZE = $8642;
  GL_MAX_GEOMETRY_TEXTURE_IMAGE_UNITS = $8C29;
  GL_FRAMEBUFFER_ATTACHMENT_LAYERED = $8DA7;
  GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS = $8DA8;
  GL_GEOMETRY_SHADER = $8DD9;
  GL_GEOMETRY_VERTICES_OUT = $8916;
  GL_GEOMETRY_INPUT_TYPE = $8917;
  GL_GEOMETRY_OUTPUT_TYPE = $8918;
  GL_MAX_GEOMETRY_UNIFORM_COMPONENTS = $8DDF;
  GL_MAX_GEOMETRY_OUTPUT_VERTICES = $8DE0;
  GL_MAX_GEOMETRY_TOTAL_OUTPUT_COMPONENTS = $8DE1;
  GL_MAX_VERTEX_OUTPUT_COMPONENTS = $9122;
  GL_MAX_GEOMETRY_INPUT_COMPONENTS = $9123;
  GL_MAX_GEOMETRY_OUTPUT_COMPONENTS = $9124;
  GL_MAX_FRAGMENT_INPUT_COMPONENTS = $9125;
  GL_CONTEXT_PROFILE_MASK = $9126;
  GL_DEPTH_CLAMP = $864F;
  GL_QUADS_FOLLOW_PROVOKING_VERTEX_CONVENTION = $8E4C;
  GL_FIRST_VERTEX_CONVENTION = $8E4D;
  GL_LAST_VERTEX_CONVENTION = $8E4E;
  GL_PROVOKING_VERTEX = $8E4F;
  GL_TEXTURE_CUBE_MAP_SEAMLESS = $884F;
  GL_MAX_SERVER_WAIT_TIMEOUT = $9111;
  GL_OBJECT_TYPE = $9112;
  GL_SYNC_CONDITION = $9113;
  GL_SYNC_STATUS = $9114;
  GL_SYNC_FLAGS = $9115;
  GL_SYNC_FENCE = $9116;
  GL_SYNC_GPU_COMMANDS_COMPLETE = $9117;
  GL_UNSIGNALED = $9118;
  GL_SIGNALED = $9119;
  GL_ALREADY_SIGNALED = $911A;
  GL_TIMEOUT_EXPIRED = $911B;
  GL_CONDITION_SATISFIED = $911C;
  GL_WAIT_FAILED = $911D;
  GL_TIMEOUT_IGNORED = GLuint64($FFFFFFFFFFFFFFFF);
  GL_SYNC_FLUSH_COMMANDS_BIT = $00000001;
  GL_SAMPLE_POSITION = $8E50;
  GL_SAMPLE_MASK = $8E51;
  GL_SAMPLE_MASK_VALUE = $8E52;
  GL_MAX_SAMPLE_MASK_WORDS = $8E59;
  GL_TEXTURE_2D_MULTISAMPLE = $9100;
  GL_PROXY_TEXTURE_2D_MULTISAMPLE = $9101;
  GL_TEXTURE_2D_MULTISAMPLE_ARRAY = $9102;
  GL_PROXY_TEXTURE_2D_MULTISAMPLE_ARRAY = $9103;
  GL_TEXTURE_BINDING_2D_MULTISAMPLE = $9104;
  GL_TEXTURE_BINDING_2D_MULTISAMPLE_ARRAY = $9105;
  GL_TEXTURE_SAMPLES = $9106;
  GL_TEXTURE_FIXED_SAMPLE_LOCATIONS = $9107;
  GL_SAMPLER_2D_MULTISAMPLE = $9108;
  GL_INT_SAMPLER_2D_MULTISAMPLE = $9109;
  GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE = $910A;
  GL_SAMPLER_2D_MULTISAMPLE_ARRAY = $910B;
  GL_INT_SAMPLER_2D_MULTISAMPLE_ARRAY = $910C;
  GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE_ARRAY = $910D;
  GL_MAX_COLOR_TEXTURE_SAMPLES = $910E;
  GL_MAX_DEPTH_TEXTURE_SAMPLES = $910F;
  GL_MAX_INTEGER_SAMPLES = $9110;

var
  glDrawElementsBaseVertex: procedure(mode: GLenum; count: GLsizei; type_: GLenum; indices: Pointer; basevertex: GLint); apicall;
  glDrawRangeElementsBaseVertex: procedure(mode: GLenum; start: GLuint; end_: GLuint; count: GLsizei; type_: GLenum; indices: Pointer; basevertex: GLint); apicall;
  glDrawElementsInstancedBaseVertex: procedure(mode: GLenum; count: GLsizei; type_: GLenum; indices: Pointer; instancecount: GLsizei; basevertex: GLint); apicall;
  glMultiDrawElementsBaseVertex: procedure(mode: GLenum; count: PGLsizei; type_: GLenum; indices: PPointer; drawcount: GLsizei; basevertex: PGLint); apicall;
  glProvokingVertex: procedure(mode: GLenum); apicall;
  glFenceSync: function(condition: GLenum; flags: GLbitfield): GLsync; apicall;
  glIsSync: function(sync: GLsync): GLboolean; apicall;
  glDeleteSync: procedure(sync: GLsync); apicall;
  glClientWaitSync: function(sync: GLsync; flags: GLbitfield; timeout: GLuint64): GLenum; apicall;
  glWaitSync: procedure(sync: GLsync; flags: GLbitfield; timeout: GLuint64); apicall;
  glGetInteger64v: procedure(pname: GLenum; data: PGLint64); apicall;
  glGetSynciv: procedure(sync: GLsync; pname: GLenum; count: GLsizei; length: PGLsizei; values: PGLint); apicall;
  glGetInteger64i_v: procedure(target: GLenum; index: GLuint; data: PGLint64); apicall;
  glGetBufferParameteri64v: procedure(target: GLenum; pname: GLenum; params: PGLint64); apicall;
  glFramebufferTexture: procedure(target: GLenum; attachment: GLenum; texture: GLuint; level: GLint); apicall;
  glTexImage2DMultisample: procedure(target: GLenum; samples: GLsizei; internalformat: GLenum; width: GLsizei; height: GLsizei; fixedsamplelocations: GLboolean); apicall;
  glTexImage3DMultisample: procedure(target: GLenum; samples: GLsizei; internalformat: GLenum; width: GLsizei; height: GLsizei; depth: GLsizei; fixedsamplelocations: GLboolean); apicall;
  glGetMultisamplefv: procedure(pname: GLenum; index: GLuint; val: PGLfloat); apicall;
  glSampleMaski: procedure(maskNumber: GLuint; mask: GLbitfield); apicall;
{$endif}
{$endregion}

{ OpenGL 3.3 }

{$region gl33}
{$ifdef gl33}
const
  GL_VERTEX_ATTRIB_ARRAY_DIVISOR = $88FE;
  GL_SRC1_COLOR = $88F9;
  GL_ONE_MINUS_SRC1_COLOR = $88FA;
  GL_ONE_MINUS_SRC1_ALPHA = $88FB;
  GL_MAX_DUAL_SOURCE_DRAW_BUFFERS = $88FC;
  GL_ANY_SAMPLES_PASSED = $8C2F;
  GL_SAMPLER_BINDING = $8919;
  GL_RGB10_A2UI = $906F;
  GL_TEXTURE_SWIZZLE_R = $8E42;
  GL_TEXTURE_SWIZZLE_G = $8E43;
  GL_TEXTURE_SWIZZLE_B = $8E44;
  GL_TEXTURE_SWIZZLE_A = $8E45;
  GL_TEXTURE_SWIZZLE_RGBA = $8E46;
  GL_TIME_ELAPSED = $88BF;
  GL_TIMESTAMP = $8E28;
  GL_INT_2_10_10_10_REV = $8D9F;

var
  glBindFragDataLocationIndexed: procedure(program_: GLuint; colorNumber: GLuint; index: GLuint; name: PGLchar); apicall;
  glGetFragDataIndex: function(program_: GLuint; name: PGLchar): GLint; apicall;
  glGenSamplers: procedure(count: GLsizei; samplers: PGLuint); apicall;
  glDeleteSamplers: procedure(count: GLsizei; samplers: PGLuint); apicall;
  glIsSampler: function(sampler: GLuint): GLboolean; apicall;
  glBindSampler: procedure(unit_: GLuint; sampler: GLuint); apicall;
  glSamplerParameteri: procedure(sampler: GLuint; pname: GLenum; param: GLint); apicall;
  glSamplerParameteriv: procedure(sampler: GLuint; pname: GLenum; param: PGLint); apicall;
  glSamplerParameterf: procedure(sampler: GLuint; pname: GLenum; param: GLfloat); apicall;
  glSamplerParameterfv: procedure(sampler: GLuint; pname: GLenum; param: PGLfloat); apicall;
  glSamplerParameterIiv: procedure(sampler: GLuint; pname: GLenum; param: PGLint); apicall;
  glSamplerParameterIuiv: procedure(sampler: GLuint; pname: GLenum; param: PGLuint); apicall;
  glGetSamplerParameteriv: procedure(sampler: GLuint; pname: GLenum; params: PGLint); apicall;
  glGetSamplerParameterIiv: procedure(sampler: GLuint; pname: GLenum; params: PGLint); apicall;
  glGetSamplerParameterfv: procedure(sampler: GLuint; pname: GLenum; params: PGLfloat); apicall;
  glGetSamplerParameterIuiv: procedure(sampler: GLuint; pname: GLenum; params: PGLuint); apicall;
  glQueryCounter: procedure(id: GLuint; target: GLenum); apicall;
  glGetQueryObjecti64v: procedure(id: GLuint; pname: GLenum; params: PGLint64); apicall;
  glGetQueryObjectui64v: procedure(id: GLuint; pname: GLenum; params: PGLuint64); apicall;
  glVertexAttribDivisor: procedure(index: GLuint; divisor: GLuint); apicall;
  glVertexAttribP1ui: procedure(index: GLuint; type_: GLenum; normalized: GLboolean; value: GLuint); apicall;
  glVertexAttribP1uiv: procedure(index: GLuint; type_: GLenum; normalized: GLboolean; value: PGLuint); apicall;
  glVertexAttribP2ui: procedure(index: GLuint; type_: GLenum; normalized: GLboolean; value: GLuint); apicall;
  glVertexAttribP2uiv: procedure(index: GLuint; type_: GLenum; normalized: GLboolean; value: PGLuint); apicall;
  glVertexAttribP3ui: procedure(index: GLuint; type_: GLenum; normalized: GLboolean; value: GLuint); apicall;
  glVertexAttribP3uiv: procedure(index: GLuint; type_: GLenum; normalized: GLboolean; value: PGLuint); apicall;
  glVertexAttribP4ui: procedure(index: GLuint; type_: GLenum; normalized: GLboolean; value: GLuint); apicall;
  glVertexAttribP4uiv: procedure(index: GLuint; type_: GLenum; normalized: GLboolean; value: PGLuint); apicall;
{$endif}
{$endregion}

{ OpenGL 3.3 compatibility profile }

{$region gl33 compatibility}
{$if defined(gl33) and defined(glcompat)}
var
  glVertexP2ui: procedure(type_: GLenum; value: GLuint); apicall;
  glVertexP2uiv: procedure(type_: GLenum; value: PGLuint); apicall;
  glVertexP3ui: procedure(type_: GLenum; value: GLuint); apicall;
  glVertexP3uiv: procedure(type_: GLenum; value: PGLuint); apicall;
  glVertexP4ui: procedure(type_: GLenum; value: GLuint); apicall;
  glVertexP4uiv: procedure(type_: GLenum; value: PGLuint); apicall;
  glTexCoordP1ui: procedure(type_: GLenum; coords: GLuint); apicall;
  glTexCoordP1uiv: procedure(type_: GLenum; coords: PGLuint); apicall;
  glTexCoordP2ui: procedure(type_: GLenum; coords: GLuint); apicall;
  glTexCoordP2uiv: procedure(type_: GLenum; coords: PGLuint); apicall;
  glTexCoordP3ui: procedure(type_: GLenum; coords: GLuint); apicall;
  glTexCoordP3uiv: procedure(type_: GLenum; coords: PGLuint); apicall;
  glTexCoordP4ui: procedure(type_: GLenum; coords: GLuint); apicall;
  glTexCoordP4uiv: procedure(type_: GLenum; coords: PGLuint); apicall;
  glMultiTexCoordP1ui: procedure(texture: GLenum; type_: GLenum; coords: GLuint); apicall;
  glMultiTexCoordP1uiv: procedure(texture: GLenum; type_: GLenum; coords: PGLuint); apicall;
  glMultiTexCoordP2ui: procedure(texture: GLenum; type_: GLenum; coords: GLuint); apicall;
  glMultiTexCoordP2uiv: procedure(texture: GLenum; type_: GLenum; coords: PGLuint); apicall;
  glMultiTexCoordP3ui: procedure(texture: GLenum; type_: GLenum; coords: GLuint); apicall;
  glMultiTexCoordP3uiv: procedure(texture: GLenum; type_: GLenum; coords: PGLuint); apicall;
  glMultiTexCoordP4ui: procedure(texture: GLenum; type_: GLenum; coords: GLuint); apicall;
  glMultiTexCoordP4uiv: procedure(texture: GLenum; type_: GLenum; coords: PGLuint); apicall;
  glNormalP3ui: procedure(type_: GLenum; coords: GLuint); apicall;
  glNormalP3uiv: procedure(type_: GLenum; coords: PGLuint); apicall;
  glColorP3ui: procedure(type_: GLenum; color: GLuint); apicall;
  glColorP3uiv: procedure(type_: GLenum; color: PGLuint); apicall;
  glColorP4ui: procedure(type_: GLenum; color: GLuint); apicall;
  glColorP4uiv: procedure(type_: GLenum; color: PGLuint); apicall;
  glSecondaryColorP3ui: procedure(type_: GLenum; color: GLuint); apicall;
  glSecondaryColorP3uiv: procedure(type_: GLenum; color: PGLuint); apicall;
{$endif}
{$endregion}

{ OpenGL 4.0 }

{$region gl40}
{$ifdef gl40}
const
  GL_SAMPLE_SHADING = $8C36;
  GL_MIN_SAMPLE_SHADING_VALUE = $8C37;
  GL_MIN_PROGRAM_TEXTURE_GATHER_OFFSET = $8E5E;
  GL_MAX_PROGRAM_TEXTURE_GATHER_OFFSET = $8E5F;
  GL_TEXTURE_CUBE_MAP_ARRAY = $9009;
  GL_TEXTURE_BINDING_CUBE_MAP_ARRAY = $900A;
  GL_PROXY_TEXTURE_CUBE_MAP_ARRAY = $900B;
  GL_SAMPLER_CUBE_MAP_ARRAY = $900C;
  GL_SAMPLER_CUBE_MAP_ARRAY_SHADOW = $900D;
  GL_INT_SAMPLER_CUBE_MAP_ARRAY = $900E;
  GL_UNSIGNED_INT_SAMPLER_CUBE_MAP_ARRAY = $900F;
  GL_DRAW_INDIRECT_BUFFER = $8F3F;
  GL_DRAW_INDIRECT_BUFFER_BINDING = $8F43;
  GL_GEOMETRY_SHADER_INVOCATIONS = $887F;
  GL_MAX_GEOMETRY_SHADER_INVOCATIONS = $8E5A;
  GL_MIN_FRAGMENT_INTERPOLATION_OFFSET = $8E5B;
  GL_MAX_FRAGMENT_INTERPOLATION_OFFSET = $8E5C;
  GL_FRAGMENT_INTERPOLATION_OFFSET_BITS = $8E5D;
  GL_MAX_VERTEX_STREAMS = $8E71;
  GL_DOUBLE_VEC2 = $8FFC;
  GL_DOUBLE_VEC3 = $8FFD;
  GL_DOUBLE_VEC4 = $8FFE;
  GL_DOUBLE_MAT2 = $8F46;
  GL_DOUBLE_MAT3 = $8F47;
  GL_DOUBLE_MAT4 = $8F48;
  GL_DOUBLE_MAT2x3 = $8F49;
  GL_DOUBLE_MAT2x4 = $8F4A;
  GL_DOUBLE_MAT3x2 = $8F4B;
  GL_DOUBLE_MAT3x4 = $8F4C;
  GL_DOUBLE_MAT4x2 = $8F4D;
  GL_DOUBLE_MAT4x3 = $8F4E;
  GL_ACTIVE_SUBROUTINES = $8DE5;
  GL_ACTIVE_SUBROUTINE_UNIFORMS = $8DE6;
  GL_ACTIVE_SUBROUTINE_UNIFORM_LOCATIONS = $8E47;
  GL_ACTIVE_SUBROUTINE_MAX_LENGTH = $8E48;
  GL_ACTIVE_SUBROUTINE_UNIFORM_MAX_LENGTH = $8E49;
  GL_MAX_SUBROUTINES = $8DE7;
  GL_MAX_SUBROUTINE_UNIFORM_LOCATIONS = $8DE8;
  GL_NUM_COMPATIBLE_SUBROUTINES = $8E4A;
  GL_COMPATIBLE_SUBROUTINES = $8E4B;
  GL_PATCHES = $000E;
  GL_PATCH_VERTICES = $8E72;
  GL_PATCH_DEFAULT_INNER_LEVEL = $8E73;
  GL_PATCH_DEFAULT_OUTER_LEVEL = $8E74;
  GL_TESS_CONTROL_OUTPUT_VERTICES = $8E75;
  GL_TESS_GEN_MODE = $8E76;
  GL_TESS_GEN_SPACING = $8E77;
  GL_TESS_GEN_VERTEX_ORDER = $8E78;
  GL_TESS_GEN_POINT_MODE = $8E79;
  GL_ISOLINES = $8E7A;
  GL_FRACTIONAL_ODD = $8E7B;
  GL_FRACTIONAL_EVEN = $8E7C;
  GL_MAX_PATCH_VERTICES = $8E7D;
  GL_MAX_TESS_GEN_LEVEL = $8E7E;
  GL_MAX_TESS_CONTROL_UNIFORM_COMPONENTS = $8E7F;
  GL_MAX_TESS_EVALUATION_UNIFORM_COMPONENTS = $8E80;
  GL_MAX_TESS_CONTROL_TEXTURE_IMAGE_UNITS = $8E81;
  GL_MAX_TESS_EVALUATION_TEXTURE_IMAGE_UNITS = $8E82;
  GL_MAX_TESS_CONTROL_OUTPUT_COMPONENTS = $8E83;
  GL_MAX_TESS_PATCH_COMPONENTS = $8E84;
  GL_MAX_TESS_CONTROL_TOTAL_OUTPUT_COMPONENTS = $8E85;
  GL_MAX_TESS_EVALUATION_OUTPUT_COMPONENTS = $8E86;
  GL_MAX_TESS_CONTROL_UNIFORM_BLOCKS = $8E89;
  GL_MAX_TESS_EVALUATION_UNIFORM_BLOCKS = $8E8A;
  GL_MAX_TESS_CONTROL_INPUT_COMPONENTS = $886C;
  GL_MAX_TESS_EVALUATION_INPUT_COMPONENTS = $886D;
  GL_MAX_COMBINED_TESS_CONTROL_UNIFORM_COMPONENTS = $8E1E;
  GL_MAX_COMBINED_TESS_EVALUATION_UNIFORM_COMPONENTS = $8E1F;
  GL_UNIFORM_BLOCK_REFERENCED_BY_TESS_CONTROL_SHADER = $84F0;
  GL_UNIFORM_BLOCK_REFERENCED_BY_TESS_EVALUATION_SHADER = $84F1;
  GL_TESS_EVALUATION_SHADER = $8E87;
  GL_TESS_CONTROL_SHADER = $8E88;
  GL_TRANSFORM_FEEDBACK = $8E22;
  GL_TRANSFORM_FEEDBACK_BUFFER_PAUSED = $8E23;
  GL_TRANSFORM_FEEDBACK_BUFFER_ACTIVE = $8E24;
  GL_TRANSFORM_FEEDBACK_BINDING = $8E25;
  GL_MAX_TRANSFORM_FEEDBACK_BUFFERS = $8E70;

var
  glMinSampleShading: procedure(value: GLfloat); apicall;
  glBlendEquationi: procedure(buf: GLuint; mode: GLenum); apicall;
  glBlendEquationSeparatei: procedure(buf: GLuint; modeRGB: GLenum; modeAlpha: GLenum); apicall;
  glBlendFunci: procedure(buf: GLuint; src: GLenum; dst: GLenum); apicall;
  glBlendFuncSeparatei: procedure(buf: GLuint; srcRGB: GLenum; dstRGB: GLenum; srcAlpha: GLenum; dstAlpha: GLenum); apicall;
  glDrawArraysIndirect: procedure(mode: GLenum; indirect: Pointer); apicall;
  glDrawElementsIndirect: procedure(mode: GLenum; type_: GLenum; indirect: Pointer); apicall;
  glUniform1d: procedure(location: GLint; x: GLdouble); apicall;
  glUniform2d: procedure(location: GLint; x: GLdouble; y: GLdouble); apicall;
  glUniform3d: procedure(location: GLint; x: GLdouble; y: GLdouble; z: GLdouble); apicall;
  glUniform4d: procedure(location: GLint; x: GLdouble; y: GLdouble; z: GLdouble; w: GLdouble); apicall;
  glUniform1dv: procedure(location: GLint; count: GLsizei; value: PGLdouble); apicall;
  glUniform2dv: procedure(location: GLint; count: GLsizei; value: PGLdouble); apicall;
  glUniform3dv: procedure(location: GLint; count: GLsizei; value: PGLdouble); apicall;
  glUniform4dv: procedure(location: GLint; count: GLsizei; value: PGLdouble); apicall;
  glUniformMatrix2dv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLdouble); apicall;
  glUniformMatrix3dv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLdouble); apicall;
  glUniformMatrix4dv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLdouble); apicall;
  glUniformMatrix2x3dv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLdouble); apicall;
  glUniformMatrix2x4dv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLdouble); apicall;
  glUniformMatrix3x2dv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLdouble); apicall;
  glUniformMatrix3x4dv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLdouble); apicall;
  glUniformMatrix4x2dv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLdouble); apicall;
  glUniformMatrix4x3dv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLdouble); apicall;
  glGetUniformdv: procedure(program_: GLuint; location: GLint; params: PGLdouble); apicall;
  glGetSubroutineUniformLocation: function(program_: GLuint; shadertype: GLenum; name: PGLchar): GLint; apicall;
  glGetSubroutineIndex: function(program_: GLuint; shadertype: GLenum; name: PGLchar): GLuint; apicall;
  glGetActiveSubroutineUniformiv: procedure(program_: GLuint; shadertype: GLenum; index: GLuint; pname: GLenum; values: PGLint); apicall;
  glGetActiveSubroutineUniformName: procedure(program_: GLuint; shadertype: GLenum; index: GLuint; bufSize: GLsizei; length: PGLsizei; name: PGLchar); apicall;
  glGetActiveSubroutineName: procedure(program_: GLuint; shadertype: GLenum; index: GLuint; bufSize: GLsizei; length: PGLsizei; name: PGLchar); apicall;
  glUniformSubroutinesuiv: procedure(shadertype: GLenum; count: GLsizei; indices: PGLuint); apicall;
  glGetUniformSubroutineuiv: procedure(shadertype: GLenum; location: GLint; params: PGLuint); apicall;
  glGetProgramStageiv: procedure(program_: GLuint; shadertype: GLenum; pname: GLenum; values: PGLint); apicall;
  glPatchParameteri: procedure(pname: GLenum; value: GLint); apicall;
  glPatchParameterfv: procedure(pname: GLenum; values: PGLfloat); apicall;
  glBindTransformFeedback: procedure(target: GLenum; id: GLuint); apicall;
  glDeleteTransformFeedbacks: procedure(n: GLsizei; ids: PGLuint); apicall;
  glGenTransformFeedbacks: procedure(n: GLsizei; ids: PGLuint); apicall;
  glIsTransformFeedback: function(id: GLuint): GLboolean; apicall;
  glPauseTransformFeedback: procedure; apicall;
  glResumeTransformFeedback: procedure; apicall;
  glDrawTransformFeedback: procedure(mode: GLenum; id: GLuint); apicall;
  glDrawTransformFeedbackStream: procedure(mode: GLenum; id: GLuint; stream: GLuint); apicall;
  glBeginQueryIndexed: procedure(target: GLenum; index: GLuint; id: GLuint); apicall;
  glEndQueryIndexed: procedure(target: GLenum; index: GLuint); apicall;
  glGetQueryIndexediv: procedure(target: GLenum; index: GLuint; pname: GLenum; params: PGLint); apicall;
{$endif}
{$endregion}

{ OpenGL 4.1 }

{$region gl41}
{$ifdef gl41}
const
  GL_FIXED = $140C;
  GL_IMPLEMENTATION_COLOR_READ_TYPE = $8B9A;
  GL_IMPLEMENTATION_COLOR_READ_FORMAT = $8B9B;
  GL_LOW_FLOAT = $8DF0;
  GL_MEDIUM_FLOAT = $8DF1;
  GL_HIGH_FLOAT = $8DF2;
  GL_LOW_INT = $8DF3;
  GL_MEDIUM_INT = $8DF4;
  GL_HIGH_INT = $8DF5;
  GL_SHADER_COMPILER = $8DFA;
  GL_SHADER_BINARY_FORMATS = $8DF8;
  GL_NUM_SHADER_BINARY_FORMATS = $8DF9;
  GL_MAX_VERTEX_UNIFORM_VECTORS = $8DFB;
  GL_MAX_VARYING_VECTORS = $8DFC;
  GL_MAX_FRAGMENT_UNIFORM_VECTORS = $8DFD;
  GL_RGB565 = $8D62;
  GL_PROGRAM_BINARY_RETRIEVABLE_HINT = $8257;
  GL_PROGRAM_BINARY_LENGTH = $8741;
  GL_NUM_PROGRAM_BINARY_FORMATS = $87FE;
  GL_PROGRAM_BINARY_FORMATS = $87FF;
  GL_VERTEX_SHADER_BIT = $00000001;
  GL_FRAGMENT_SHADER_BIT = $00000002;
  GL_GEOMETRY_SHADER_BIT = $00000004;
  GL_TESS_CONTROL_SHADER_BIT = $00000008;
  GL_TESS_EVALUATION_SHADER_BIT = $00000010;
  GL_ALL_SHADER_BITS = $FFFFFFFF;
  GL_PROGRAM_SEPARABLE = $8258;
  GL_ACTIVE_PROGRAM = $8259;
  GL_PROGRAM_PIPELINE_BINDING = $825A;
  GL_MAX_VIEWPORTS = $825B;
  GL_VIEWPORT_SUBPIXEL_BITS = $825C;
  GL_VIEWPORT_BOUNDS_RANGE = $825D;
  GL_LAYER_PROVOKING_VERTEX = $825E;
  GL_VIEWPORT_INDEX_PROVOKING_VERTEX = $825F;
  GL_UNDEFINED_VERTEX = $8260;

var
  glReleaseShaderCompiler: procedure; apicall;
  glShaderBinary: procedure(count: GLsizei; shaders: PGLuint; binaryFormat: GLenum; binary: Pointer; length: GLsizei); apicall;
  glGetShaderPrecisionFormat: procedure(shadertype: GLenum; precisiontype: GLenum; range: PGLint; precision: PGLint); apicall;
  glDepthRangef: procedure(n: GLfloat; f: GLfloat); apicall;
  glClearDepthf: procedure(d: GLfloat); apicall;
  glGetProgramBinary: procedure(program_: GLuint; bufSize: GLsizei; length: PGLsizei; binaryFormat: PGLenum; binary: Pointer); apicall;
  glProgramBinary: procedure(program_: GLuint; binaryFormat: GLenum; binary: Pointer; length: GLsizei); apicall;
  glProgramParameteri: procedure(program_: GLuint; pname: GLenum; value: GLint); apicall;
  glUseProgramStages: procedure(pipeline: GLuint; stages: GLbitfield; program_: GLuint); apicall;
  glActiveShaderProgram: procedure(pipeline: GLuint; program_: GLuint); apicall;
  glCreateShaderProgramv: function(type_: GLenum; count: GLsizei; strings: PPGLchar): GLuint; apicall;
  glBindProgramPipeline: procedure(pipeline: GLuint); apicall;
  glDeleteProgramPipelines: procedure(n: GLsizei; pipelines: PGLuint); apicall;
  glGenProgramPipelines: procedure(n: GLsizei; pipelines: PGLuint); apicall;
  glIsProgramPipeline: function(pipeline: GLuint): GLboolean; apicall;
  glGetProgramPipelineiv: procedure(pipeline: GLuint; pname: GLenum; params: PGLint); apicall;
  glProgramUniform1i: procedure(program_: GLuint; location: GLint; v0: GLint); apicall;
  glProgramUniform1iv: procedure(program_: GLuint; location: GLint; count: GLsizei; value: PGLint); apicall;
  glProgramUniform1f: procedure(program_: GLuint; location: GLint; v0: GLfloat); apicall;
  glProgramUniform1fv: procedure(program_: GLuint; location: GLint; count: GLsizei; value: PGLfloat); apicall;
  glProgramUniform1d: procedure(program_: GLuint; location: GLint; v0: GLdouble); apicall;
  glProgramUniform1dv: procedure(program_: GLuint; location: GLint; count: GLsizei; value: PGLdouble); apicall;
  glProgramUniform1ui: procedure(program_: GLuint; location: GLint; v0: GLuint); apicall;
  glProgramUniform1uiv: procedure(program_: GLuint; location: GLint; count: GLsizei; value: PGLuint); apicall;
  glProgramUniform2i: procedure(program_: GLuint; location: GLint; v0: GLint; v1: GLint); apicall;
  glProgramUniform2iv: procedure(program_: GLuint; location: GLint; count: GLsizei; value: PGLint); apicall;
  glProgramUniform2f: procedure(program_: GLuint; location: GLint; v0: GLfloat; v1: GLfloat); apicall;
  glProgramUniform2fv: procedure(program_: GLuint; location: GLint; count: GLsizei; value: PGLfloat); apicall;
  glProgramUniform2d: procedure(program_: GLuint; location: GLint; v0: GLdouble; v1: GLdouble); apicall;
  glProgramUniform2dv: procedure(program_: GLuint; location: GLint; count: GLsizei; value: PGLdouble); apicall;
  glProgramUniform2ui: procedure(program_: GLuint; location: GLint; v0: GLuint; v1: GLuint); apicall;
  glProgramUniform2uiv: procedure(program_: GLuint; location: GLint; count: GLsizei; value: PGLuint); apicall;
  glProgramUniform3i: procedure(program_: GLuint; location: GLint; v0: GLint; v1: GLint; v2: GLint); apicall;
  glProgramUniform3iv: procedure(program_: GLuint; location: GLint; count: GLsizei; value: PGLint); apicall;
  glProgramUniform3f: procedure(program_: GLuint; location: GLint; v0: GLfloat; v1: GLfloat; v2: GLfloat); apicall;
  glProgramUniform3fv: procedure(program_: GLuint; location: GLint; count: GLsizei; value: PGLfloat); apicall;
  glProgramUniform3d: procedure(program_: GLuint; location: GLint; v0: GLdouble; v1: GLdouble; v2: GLdouble); apicall;
  glProgramUniform3dv: procedure(program_: GLuint; location: GLint; count: GLsizei; value: PGLdouble); apicall;
  glProgramUniform3ui: procedure(program_: GLuint; location: GLint; v0: GLuint; v1: GLuint; v2: GLuint); apicall;
  glProgramUniform3uiv: procedure(program_: GLuint; location: GLint; count: GLsizei; value: PGLuint); apicall;
  glProgramUniform4i: procedure(program_: GLuint; location: GLint; v0: GLint; v1: GLint; v2: GLint; v3: GLint); apicall;
  glProgramUniform4iv: procedure(program_: GLuint; location: GLint; count: GLsizei; value: PGLint); apicall;
  glProgramUniform4f: procedure(program_: GLuint; location: GLint; v0: GLfloat; v1: GLfloat; v2: GLfloat; v3: GLfloat); apicall;
  glProgramUniform4fv: procedure(program_: GLuint; location: GLint; count: GLsizei; value: PGLfloat); apicall;
  glProgramUniform4d: procedure(program_: GLuint; location: GLint; v0: GLdouble; v1: GLdouble; v2: GLdouble; v3: GLdouble); apicall;
  glProgramUniform4dv: procedure(program_: GLuint; location: GLint; count: GLsizei; value: PGLdouble); apicall;
  glProgramUniform4ui: procedure(program_: GLuint; location: GLint; v0: GLuint; v1: GLuint; v2: GLuint; v3: GLuint); apicall;
  glProgramUniform4uiv: procedure(program_: GLuint; location: GLint; count: GLsizei; value: PGLuint); apicall;
  glProgramUniformMatrix2fv: procedure(program_: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); apicall;
  glProgramUniformMatrix3fv: procedure(program_: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); apicall;
  glProgramUniformMatrix4fv: procedure(program_: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); apicall;
  glProgramUniformMatrix2dv: procedure(program_: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; value: PGLdouble); apicall;
  glProgramUniformMatrix3dv: procedure(program_: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; value: PGLdouble); apicall;
  glProgramUniformMatrix4dv: procedure(program_: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; value: PGLdouble); apicall;
  glProgramUniformMatrix2x3fv: procedure(program_: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); apicall;
  glProgramUniformMatrix3x2fv: procedure(program_: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); apicall;
  glProgramUniformMatrix2x4fv: procedure(program_: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); apicall;
  glProgramUniformMatrix4x2fv: procedure(program_: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); apicall;
  glProgramUniformMatrix3x4fv: procedure(program_: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); apicall;
  glProgramUniformMatrix4x3fv: procedure(program_: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); apicall;
  glProgramUniformMatrix2x3dv: procedure(program_: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; value: PGLdouble); apicall;
  glProgramUniformMatrix3x2dv: procedure(program_: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; value: PGLdouble); apicall;
  glProgramUniformMatrix2x4dv: procedure(program_: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; value: PGLdouble); apicall;
  glProgramUniformMatrix4x2dv: procedure(program_: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; value: PGLdouble); apicall;
  glProgramUniformMatrix3x4dv: procedure(program_: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; value: PGLdouble); apicall;
  glProgramUniformMatrix4x3dv: procedure(program_: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; value: PGLdouble); apicall;
  glValidateProgramPipeline: procedure(pipeline: GLuint); apicall;
  glGetProgramPipelineInfoLog: procedure(pipeline: GLuint; bufSize: GLsizei; length: PGLsizei; infoLog: PGLchar); apicall;
  glVertexAttribL1d: procedure(index: GLuint; x: GLdouble); apicall;
  glVertexAttribL2d: procedure(index: GLuint; x: GLdouble; y: GLdouble); apicall;
  glVertexAttribL3d: procedure(index: GLuint; x: GLdouble; y: GLdouble; z: GLdouble); apicall;
  glVertexAttribL4d: procedure(index: GLuint; x: GLdouble; y: GLdouble; z: GLdouble; w: GLdouble); apicall;
  glVertexAttribL1dv: procedure(index: GLuint; v: PGLdouble); apicall;
  glVertexAttribL2dv: procedure(index: GLuint; v: PGLdouble); apicall;
  glVertexAttribL3dv: procedure(index: GLuint; v: PGLdouble); apicall;
  glVertexAttribL4dv: procedure(index: GLuint; v: PGLdouble); apicall;
  glVertexAttribLPointer: procedure(index: GLuint; size: GLint; type_: GLenum; stride: GLsizei; pointer: Pointer); apicall;
  glGetVertexAttribLdv: procedure(index: GLuint; pname: GLenum; params: PGLdouble); apicall;
  glViewportArrayv: procedure(first: GLuint; count: GLsizei; v: PGLfloat); apicall;
  glViewportIndexedf: procedure(index: GLuint; x: GLfloat; y: GLfloat; w: GLfloat; h: GLfloat); apicall;
  glViewportIndexedfv: procedure(index: GLuint; v: PGLfloat); apicall;
  glScissorArrayv: procedure(first: GLuint; count: GLsizei; v: PGLint); apicall;
  glScissorIndexed: procedure(index: GLuint; left: GLint; bottom: GLint; width: GLsizei; height: GLsizei); apicall;
  glScissorIndexedv: procedure(index: GLuint; v: PGLint); apicall;
  glDepthRangeArrayv: procedure(first: GLuint; count: GLsizei; v: PGLdouble); apicall;
  glDepthRangeIndexed: procedure(index: GLuint; n: GLdouble; f: GLdouble); apicall;
  glGetFloati_v: procedure(target: GLenum; index: GLuint; data: PGLfloat); apicall;
  glGetDoublei_v: procedure(target: GLenum; index: GLuint; data: PGLdouble); apicall;
{$endif}
{$endregion}

{ OpenGL 4.2 }

{$region gl42}
{$ifdef gl42}
const
  GL_COPY_READ_BUFFER_BINDING = $8F36;
  GL_COPY_WRITE_BUFFER_BINDING = $8F37;
  GL_TRANSFORM_FEEDBACK_ACTIVE = $8E24;
  GL_TRANSFORM_FEEDBACK_PAUSED = $8E23;
  GL_UNPACK_COMPRESSED_BLOCK_WIDTH = $9127;
  GL_UNPACK_COMPRESSED_BLOCK_HEIGHT = $9128;
  GL_UNPACK_COMPRESSED_BLOCK_DEPTH = $9129;
  GL_UNPACK_COMPRESSED_BLOCK_SIZE = $912A;
  GL_PACK_COMPRESSED_BLOCK_WIDTH = $912B;
  GL_PACK_COMPRESSED_BLOCK_HEIGHT = $912C;
  GL_PACK_COMPRESSED_BLOCK_DEPTH = $912D;
  GL_PACK_COMPRESSED_BLOCK_SIZE = $912E;
  GL_NUM_SAMPLE_COUNTS = $9380;
  GL_MIN_MAP_BUFFER_ALIGNMENT = $90BC;
  GL_ATOMIC_COUNTER_BUFFER = $92C0;
  GL_ATOMIC_COUNTER_BUFFER_BINDING = $92C1;
  GL_ATOMIC_COUNTER_BUFFER_START = $92C2;
  GL_ATOMIC_COUNTER_BUFFER_SIZE = $92C3;
  GL_ATOMIC_COUNTER_BUFFER_DATA_SIZE = $92C4;
  GL_ATOMIC_COUNTER_BUFFER_ACTIVE_ATOMIC_COUNTERS = $92C5;
  GL_ATOMIC_COUNTER_BUFFER_ACTIVE_ATOMIC_COUNTER_INDICES = $92C6;
  GL_ATOMIC_COUNTER_BUFFER_REFERENCED_BY_VERTEX_SHADER = $92C7;
  GL_ATOMIC_COUNTER_BUFFER_REFERENCED_BY_TESS_CONTROL_SHADER = $92C8;
  GL_ATOMIC_COUNTER_BUFFER_REFERENCED_BY_TESS_EVALUATION_SHADER = $92C9;
  GL_ATOMIC_COUNTER_BUFFER_REFERENCED_BY_GEOMETRY_SHADER = $92CA;
  GL_ATOMIC_COUNTER_BUFFER_REFERENCED_BY_FRAGMENT_SHADER = $92CB;
  GL_MAX_VERTEX_ATOMIC_COUNTER_BUFFERS = $92CC;
  GL_MAX_TESS_CONTROL_ATOMIC_COUNTER_BUFFERS = $92CD;
  GL_MAX_TESS_EVALUATION_ATOMIC_COUNTER_BUFFERS = $92CE;
  GL_MAX_GEOMETRY_ATOMIC_COUNTER_BUFFERS = $92CF;
  GL_MAX_FRAGMENT_ATOMIC_COUNTER_BUFFERS = $92D0;
  GL_MAX_COMBINED_ATOMIC_COUNTER_BUFFERS = $92D1;
  GL_MAX_VERTEX_ATOMIC_COUNTERS = $92D2;
  GL_MAX_TESS_CONTROL_ATOMIC_COUNTERS = $92D3;
  GL_MAX_TESS_EVALUATION_ATOMIC_COUNTERS = $92D4;
  GL_MAX_GEOMETRY_ATOMIC_COUNTERS = $92D5;
  GL_MAX_FRAGMENT_ATOMIC_COUNTERS = $92D6;
  GL_MAX_COMBINED_ATOMIC_COUNTERS = $92D7;
  GL_MAX_ATOMIC_COUNTER_BUFFER_SIZE = $92D8;
  GL_MAX_ATOMIC_COUNTER_BUFFER_BINDINGS = $92DC;
  GL_ACTIVE_ATOMIC_COUNTER_BUFFERS = $92D9;
  GL_UNIFORM_ATOMIC_COUNTER_BUFFER_INDEX = $92DA;
  GL_UNSIGNED_INT_ATOMIC_COUNTER = $92DB;
  GL_VERTEX_ATTRIB_ARRAY_BARRIER_BIT = $00000001;
  GL_ELEMENT_ARRAY_BARRIER_BIT = $00000002;
  GL_UNIFORM_BARRIER_BIT = $00000004;
  GL_TEXTURE_FETCH_BARRIER_BIT = $00000008;
  GL_SHADER_IMAGE_ACCESS_BARRIER_BIT = $00000020;
  GL_COMMAND_BARRIER_BIT = $00000040;
  GL_PIXEL_BUFFER_BARRIER_BIT = $00000080;
  GL_TEXTURE_UPDATE_BARRIER_BIT = $00000100;
  GL_BUFFER_UPDATE_BARRIER_BIT = $00000200;
  GL_FRAMEBUFFER_BARRIER_BIT = $00000400;
  GL_TRANSFORM_FEEDBACK_BARRIER_BIT = $00000800;
  GL_ATOMIC_COUNTER_BARRIER_BIT = $00001000;
  GL_ALL_BARRIER_BITS = $FFFFFFFF;
  GL_MAX_IMAGE_UNITS = $8F38;
  GL_MAX_COMBINED_IMAGE_UNITS_AND_FRAGMENT_OUTPUTS = $8F39;
  GL_IMAGE_BINDING_NAME = $8F3A;
  GL_IMAGE_BINDING_LEVEL = $8F3B;
  GL_IMAGE_BINDING_LAYERED = $8F3C;
  GL_IMAGE_BINDING_LAYER = $8F3D;
  GL_IMAGE_BINDING_ACCESS = $8F3E;
  GL_IMAGE_1D = $904C;
  GL_IMAGE_2D = $904D;
  GL_IMAGE_3D = $904E;
  GL_IMAGE_2D_RECT = $904F;
  GL_IMAGE_CUBE = $9050;
  GL_IMAGE_BUFFER = $9051;
  GL_IMAGE_1D_ARRAY = $9052;
  GL_IMAGE_2D_ARRAY = $9053;
  GL_IMAGE_CUBE_MAP_ARRAY = $9054;
  GL_IMAGE_2D_MULTISAMPLE = $9055;
  GL_IMAGE_2D_MULTISAMPLE_ARRAY = $9056;
  GL_INT_IMAGE_1D = $9057;
  GL_INT_IMAGE_2D = $9058;
  GL_INT_IMAGE_3D = $9059;
  GL_INT_IMAGE_2D_RECT = $905A;
  GL_INT_IMAGE_CUBE = $905B;
  GL_INT_IMAGE_BUFFER = $905C;
  GL_INT_IMAGE_1D_ARRAY = $905D;
  GL_INT_IMAGE_2D_ARRAY = $905E;
  GL_INT_IMAGE_CUBE_MAP_ARRAY = $905F;
  GL_INT_IMAGE_2D_MULTISAMPLE = $9060;
  GL_INT_IMAGE_2D_MULTISAMPLE_ARRAY = $9061;
  GL_UNSIGNED_INT_IMAGE_1D = $9062;
  GL_UNSIGNED_INT_IMAGE_2D = $9063;
  GL_UNSIGNED_INT_IMAGE_3D = $9064;
  GL_UNSIGNED_INT_IMAGE_2D_RECT = $9065;
  GL_UNSIGNED_INT_IMAGE_CUBE = $9066;
  GL_UNSIGNED_INT_IMAGE_BUFFER = $9067;
  GL_UNSIGNED_INT_IMAGE_1D_ARRAY = $9068;
  GL_UNSIGNED_INT_IMAGE_2D_ARRAY = $9069;
  GL_UNSIGNED_INT_IMAGE_CUBE_MAP_ARRAY = $906A;
  GL_UNSIGNED_INT_IMAGE_2D_MULTISAMPLE = $906B;
  GL_UNSIGNED_INT_IMAGE_2D_MULTISAMPLE_ARRAY = $906C;
  GL_MAX_IMAGE_SAMPLES = $906D;
  GL_IMAGE_BINDING_FORMAT = $906E;
  GL_IMAGE_FORMAT_COMPATIBILITY_TYPE = $90C7;
  GL_IMAGE_FORMAT_COMPATIBILITY_BY_SIZE = $90C8;
  GL_IMAGE_FORMAT_COMPATIBILITY_BY_CLASS = $90C9;
  GL_MAX_VERTEX_IMAGE_UNIFORMS = $90CA;
  GL_MAX_TESS_CONTROL_IMAGE_UNIFORMS = $90CB;
  GL_MAX_TESS_EVALUATION_IMAGE_UNIFORMS = $90CC;
  GL_MAX_GEOMETRY_IMAGE_UNIFORMS = $90CD;
  GL_MAX_FRAGMENT_IMAGE_UNIFORMS = $90CE;
  GL_MAX_COMBINED_IMAGE_UNIFORMS = $90CF;
  GL_COMPRESSED_RGBA_BPTC_UNORM = $8E8C;
  GL_COMPRESSED_SRGB_ALPHA_BPTC_UNORM = $8E8D;
  GL_COMPRESSED_RGB_BPTC_SIGNED_FLOAT = $8E8E;
  GL_COMPRESSED_RGB_BPTC_UNSIGNED_FLOAT = $8E8F;
  GL_TEXTURE_IMMUTABLE_FORMAT = $912F;

var
  glDrawArraysInstancedBaseInstance: procedure(mode: GLenum; first: GLint; count: GLsizei; instancecount: GLsizei; baseinstance: GLuint); apicall;
  glDrawElementsInstancedBaseInstance: procedure(mode: GLenum; count: GLsizei; type_: GLenum; indices: Pointer; instancecount: GLsizei; baseinstance: GLuint); apicall;
  glDrawElementsInstancedBaseVertexBaseInstance: procedure(mode: GLenum; count: GLsizei; type_: GLenum; indices: Pointer; instancecount: GLsizei; basevertex: GLint; baseinstance: GLuint); apicall;
  glGetInternalformativ: procedure(target: GLenum; internalformat: GLenum; pname: GLenum; count: GLsizei; params: PGLint); apicall;
  glGetActiveAtomicCounterBufferiv: procedure(program_: GLuint; bufferIndex: GLuint; pname: GLenum; params: PGLint); apicall;
  glBindImageTexture: procedure(unit_: GLuint; texture: GLuint; level: GLint; layered: GLboolean; layer: GLint; access: GLenum; format: GLenum); apicall;
  glMemoryBarrier: procedure(barriers: GLbitfield); apicall;
  glTexStorage1D: procedure(target: GLenum; levels: GLsizei; internalformat: GLenum; width: GLsizei); apicall;
  glTexStorage2D: procedure(target: GLenum; levels: GLsizei; internalformat: GLenum; width: GLsizei; height: GLsizei); apicall;
  glTexStorage3D: procedure(target: GLenum; levels: GLsizei; internalformat: GLenum; width: GLsizei; height: GLsizei; depth: GLsizei); apicall;
  glDrawTransformFeedbackInstanced: procedure(mode: GLenum; id: GLuint; instancecount: GLsizei); apicall;
  glDrawTransformFeedbackStreamInstanced: procedure(mode: GLenum; id: GLuint; stream: GLuint; instancecount: GLsizei); apicall;
{$endif}
{$endregion}

{ OpenGL 4.3 }

{$region gl43}
{$ifdef gl43}
const
  GL_NUM_SHADING_LANGUAGE_VERSIONS = $82E9;
  GL_VERTEX_ATTRIB_ARRAY_LONG = $874E;
  GL_COMPRESSED_RGB8_ETC2 = $9274;
  GL_COMPRESSED_SRGB8_ETC2 = $9275;
  GL_COMPRESSED_RGB8_PUNCHTHROUGH_ALPHA1_ETC2 = $9276;
  GL_COMPRESSED_SRGB8_PUNCHTHROUGH_ALPHA1_ETC2 = $9277;
  GL_COMPRESSED_RGBA8_ETC2_EAC = $9278;
  GL_COMPRESSED_SRGB8_ALPHA8_ETC2_EAC = $9279;
  GL_COMPRESSED_R11_EAC = $9270;
  GL_COMPRESSED_SIGNED_R11_EAC = $9271;
  GL_COMPRESSED_RG11_EAC = $9272;
  GL_COMPRESSED_SIGNED_RG11_EAC = $9273;
  GL_PRIMITIVE_RESTART_FIXED_INDEX = $8D69;
  GL_ANY_SAMPLES_PASSED_CONSERVATIVE = $8D6A;
  GL_MAX_ELEMENT_INDEX = $8D6B;
  GL_COMPUTE_SHADER = $91B9;
  GL_MAX_COMPUTE_UNIFORM_BLOCKS = $91BB;
  GL_MAX_COMPUTE_TEXTURE_IMAGE_UNITS = $91BC;
  GL_MAX_COMPUTE_IMAGE_UNIFORMS = $91BD;
  GL_MAX_COMPUTE_SHARED_MEMORY_SIZE = $8262;
  GL_MAX_COMPUTE_UNIFORM_COMPONENTS = $8263;
  GL_MAX_COMPUTE_ATOMIC_COUNTER_BUFFERS = $8264;
  GL_MAX_COMPUTE_ATOMIC_COUNTERS = $8265;
  GL_MAX_COMBINED_COMPUTE_UNIFORM_COMPONENTS = $8266;
  GL_MAX_COMPUTE_WORK_GROUP_INVOCATIONS = $90EB;
  GL_MAX_COMPUTE_WORK_GROUP_COUNT = $91BE;
  GL_MAX_COMPUTE_WORK_GROUP_SIZE = $91BF;
  GL_COMPUTE_WORK_GROUP_SIZE = $8267;
  GL_UNIFORM_BLOCK_REFERENCED_BY_COMPUTE_SHADER = $90EC;
  GL_ATOMIC_COUNTER_BUFFER_REFERENCED_BY_COMPUTE_SHADER = $90ED;
  GL_DISPATCH_INDIRECT_BUFFER = $90EE;
  GL_DISPATCH_INDIRECT_BUFFER_BINDING = $90EF;
  GL_COMPUTE_SHADER_BIT = $00000020;
  GL_DEBUG_OUTPUT_SYNCHRONOUS = $8242;
  GL_DEBUG_NEXT_LOGGED_MESSAGE_LENGTH = $8243;
  GL_DEBUG_CALLBACK_FUNCTION = $8244;
  GL_DEBUG_CALLBACK_USER_PARAM = $8245;
  GL_DEBUG_SOURCE_API = $8246;
  GL_DEBUG_SOURCE_WINDOW_SYSTEM = $8247;
  GL_DEBUG_SOURCE_SHADER_COMPILER = $8248;
  GL_DEBUG_SOURCE_THIRD_PARTY = $8249;
  GL_DEBUG_SOURCE_APPLICATION = $824A;
  GL_DEBUG_SOURCE_OTHER = $824B;
  GL_DEBUG_TYPE_ERROR = $824C;
  GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR = $824D;
  GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR = $824E;
  GL_DEBUG_TYPE_PORTABILITY = $824F;
  GL_DEBUG_TYPE_PERFORMANCE = $8250;
  GL_DEBUG_TYPE_OTHER = $8251;
  GL_MAX_DEBUG_MESSAGE_LENGTH = $9143;
  GL_MAX_DEBUG_LOGGED_MESSAGES = $9144;
  GL_DEBUG_LOGGED_MESSAGES = $9145;
  GL_DEBUG_SEVERITY_HIGH = $9146;
  GL_DEBUG_SEVERITY_MEDIUM = $9147;
  GL_DEBUG_SEVERITY_LOW = $9148;
  GL_DEBUG_TYPE_MARKER = $8268;
  GL_DEBUG_TYPE_PUSH_GROUP = $8269;
  GL_DEBUG_TYPE_POP_GROUP = $826A;
  GL_DEBUG_SEVERITY_NOTIFICATION = $826B;
  GL_MAX_DEBUG_GROUP_STACK_DEPTH = $826C;
  GL_DEBUG_GROUP_STACK_DEPTH = $826D;
  GL_BUFFER = $82E0;
  GL_SHADER = $82E1;
  GL_PROGRAM = $82E2;
  GL_QUERY = $82E3;
  GL_PROGRAM_PIPELINE = $82E4;
  GL_SAMPLER = $82E6;
  GL_MAX_LABEL_LENGTH = $82E8;
  GL_DEBUG_OUTPUT = $92E0;
  GL_CONTEXT_FLAG_DEBUG_BIT = $00000002;
  GL_MAX_UNIFORM_LOCATIONS = $826E;
  GL_FRAMEBUFFER_DEFAULT_WIDTH = $9310;
  GL_FRAMEBUFFER_DEFAULT_HEIGHT = $9311;
  GL_FRAMEBUFFER_DEFAULT_LAYERS = $9312;
  GL_FRAMEBUFFER_DEFAULT_SAMPLES = $9313;
  GL_FRAMEBUFFER_DEFAULT_FIXED_SAMPLE_LOCATIONS = $9314;
  GL_MAX_FRAMEBUFFER_WIDTH = $9315;
  GL_MAX_FRAMEBUFFER_HEIGHT = $9316;
  GL_MAX_FRAMEBUFFER_LAYERS = $9317;
  GL_MAX_FRAMEBUFFER_SAMPLES = $9318;
  GL_INTERNALFORMAT_SUPPORTED = $826F;
  GL_INTERNALFORMAT_PREFERRED = $8270;
  GL_INTERNALFORMAT_RED_SIZE = $8271;
  GL_INTERNALFORMAT_GREEN_SIZE = $8272;
  GL_INTERNALFORMAT_BLUE_SIZE = $8273;
  GL_INTERNALFORMAT_ALPHA_SIZE = $8274;
  GL_INTERNALFORMAT_DEPTH_SIZE = $8275;
  GL_INTERNALFORMAT_STENCIL_SIZE = $8276;
  GL_INTERNALFORMAT_SHARED_SIZE = $8277;
  GL_INTERNALFORMAT_RED_TYPE = $8278;
  GL_INTERNALFORMAT_GREEN_TYPE = $8279;
  GL_INTERNALFORMAT_BLUE_TYPE = $827A;
  GL_INTERNALFORMAT_ALPHA_TYPE = $827B;
  GL_INTERNALFORMAT_DEPTH_TYPE = $827C;
  GL_INTERNALFORMAT_STENCIL_TYPE = $827D;
  GL_MAX_WIDTH = $827E;
  GL_MAX_HEIGHT = $827F;
  GL_MAX_DEPTH = $8280;
  GL_MAX_LAYERS = $8281;
  GL_MAX_COMBINED_DIMENSIONS = $8282;
  GL_COLOR_COMPONENTS = $8283;
  GL_DEPTH_COMPONENTS = $8284;
  GL_STENCIL_COMPONENTS = $8285;
  GL_COLOR_RENDERABLE = $8286;
  GL_DEPTH_RENDERABLE = $8287;
  GL_STENCIL_RENDERABLE = $8288;
  GL_FRAMEBUFFER_RENDERABLE = $8289;
  GL_FRAMEBUFFER_RENDERABLE_LAYERED = $828A;
  GL_FRAMEBUFFER_BLEND = $828B;
  GL_READ_PIXELS = $828C;
  GL_READ_PIXELS_FORMAT = $828D;
  GL_READ_PIXELS_TYPE = $828E;
  GL_TEXTURE_IMAGE_FORMAT = $828F;
  GL_TEXTURE_IMAGE_TYPE = $8290;
  GL_GET_TEXTURE_IMAGE_FORMAT = $8291;
  GL_GET_TEXTURE_IMAGE_TYPE = $8292;
  GL_MIPMAP = $8293;
  GL_MANUAL_GENERATE_MIPMAP = $8294;
  GL_AUTO_GENERATE_MIPMAP = $8295;
  GL_COLOR_ENCODING = $8296;
  GL_SRGB_READ = $8297;
  GL_SRGB_WRITE = $8298;
  GL_FILTER = $829A;
  GL_VERTEX_TEXTURE = $829B;
  GL_TESS_CONTROL_TEXTURE = $829C;
  GL_TESS_EVALUATION_TEXTURE = $829D;
  GL_GEOMETRY_TEXTURE = $829E;
  GL_FRAGMENT_TEXTURE = $829F;
  GL_COMPUTE_TEXTURE = $82A0;
  GL_TEXTURE_SHADOW = $82A1;
  GL_TEXTURE_GATHER = $82A2;
  GL_TEXTURE_GATHER_SHADOW = $82A3;
  GL_SHADER_IMAGE_LOAD = $82A4;
  GL_SHADER_IMAGE_STORE = $82A5;
  GL_SHADER_IMAGE_ATOMIC = $82A6;
  GL_IMAGE_TEXEL_SIZE = $82A7;
  GL_IMAGE_COMPATIBILITY_CLASS = $82A8;
  GL_IMAGE_PIXEL_FORMAT = $82A9;
  GL_IMAGE_PIXEL_TYPE = $82AA;
  GL_SIMULTANEOUS_TEXTURE_AND_DEPTH_TEST = $82AC;
  GL_SIMULTANEOUS_TEXTURE_AND_STENCIL_TEST = $82AD;
  GL_SIMULTANEOUS_TEXTURE_AND_DEPTH_WRITE = $82AE;
  GL_SIMULTANEOUS_TEXTURE_AND_STENCIL_WRITE = $82AF;
  GL_TEXTURE_COMPRESSED_BLOCK_WIDTH = $82B1;
  GL_TEXTURE_COMPRESSED_BLOCK_HEIGHT = $82B2;
  GL_TEXTURE_COMPRESSED_BLOCK_SIZE = $82B3;
  GL_CLEAR_BUFFER = $82B4;
  GL_TEXTURE_VIEW = $82B5;
  GL_VIEW_COMPATIBILITY_CLASS = $82B6;
  GL_FULL_SUPPORT = $82B7;
  GL_CAVEAT_SUPPORT = $82B8;
  GL_IMAGE_CLASS_4_X_32 = $82B9;
  GL_IMAGE_CLASS_2_X_32 = $82BA;
  GL_IMAGE_CLASS_1_X_32 = $82BB;
  GL_IMAGE_CLASS_4_X_16 = $82BC;
  GL_IMAGE_CLASS_2_X_16 = $82BD;
  GL_IMAGE_CLASS_1_X_16 = $82BE;
  GL_IMAGE_CLASS_4_X_8 = $82BF;
  GL_IMAGE_CLASS_2_X_8 = $82C0;
  GL_IMAGE_CLASS_1_X_8 = $82C1;
  GL_IMAGE_CLASS_11_11_10 = $82C2;
  GL_IMAGE_CLASS_10_10_10_2 = $82C3;
  GL_VIEW_CLASS_128_BITS = $82C4;
  GL_VIEW_CLASS_96_BITS = $82C5;
  GL_VIEW_CLASS_64_BITS = $82C6;
  GL_VIEW_CLASS_48_BITS = $82C7;
  GL_VIEW_CLASS_32_BITS = $82C8;
  GL_VIEW_CLASS_24_BITS = $82C9;
  GL_VIEW_CLASS_16_BITS = $82CA;
  GL_VIEW_CLASS_8_BITS = $82CB;
  GL_VIEW_CLASS_S3TC_DXT1_RGB = $82CC;
  GL_VIEW_CLASS_S3TC_DXT1_RGBA = $82CD;
  GL_VIEW_CLASS_S3TC_DXT3_RGBA = $82CE;
  GL_VIEW_CLASS_S3TC_DXT5_RGBA = $82CF;
  GL_VIEW_CLASS_RGTC1_RED = $82D0;
  GL_VIEW_CLASS_RGTC2_RG = $82D1;
  GL_VIEW_CLASS_BPTC_UNORM = $82D2;
  GL_VIEW_CLASS_BPTC_FLOAT = $82D3;
  GL_UNIFORM = $92E1;
  GL_UNIFORM_BLOCK = $92E2;
  GL_PROGRAM_INPUT = $92E3;
  GL_PROGRAM_OUTPUT = $92E4;
  GL_BUFFER_VARIABLE = $92E5;
  GL_SHADER_STORAGE_BLOCK = $92E6;
  GL_VERTEX_SUBROUTINE = $92E8;
  GL_TESS_CONTROL_SUBROUTINE = $92E9;
  GL_TESS_EVALUATION_SUBROUTINE = $92EA;
  GL_GEOMETRY_SUBROUTINE = $92EB;
  GL_FRAGMENT_SUBROUTINE = $92EC;
  GL_COMPUTE_SUBROUTINE = $92ED;
  GL_VERTEX_SUBROUTINE_UNIFORM = $92EE;
  GL_TESS_CONTROL_SUBROUTINE_UNIFORM = $92EF;
  GL_TESS_EVALUATION_SUBROUTINE_UNIFORM = $92F0;
  GL_GEOMETRY_SUBROUTINE_UNIFORM = $92F1;
  GL_FRAGMENT_SUBROUTINE_UNIFORM = $92F2;
  GL_COMPUTE_SUBROUTINE_UNIFORM = $92F3;
  GL_TRANSFORM_FEEDBACK_VARYING = $92F4;
  GL_ACTIVE_RESOURCES = $92F5;
  GL_MAX_NAME_LENGTH = $92F6;
  GL_MAX_NUM_ACTIVE_VARIABLES = $92F7;
  GL_MAX_NUM_COMPATIBLE_SUBROUTINES = $92F8;
  GL_NAME_LENGTH = $92F9;
  GL_TYPE = $92FA;
  GL_ARRAY_SIZE = $92FB;
  GL_OFFSET = $92FC;
  GL_BLOCK_INDEX = $92FD;
  GL_ARRAY_STRIDE = $92FE;
  GL_MATRIX_STRIDE = $92FF;
  GL_IS_ROW_MAJOR = $9300;
  GL_ATOMIC_COUNTER_BUFFER_INDEX = $9301;
  GL_BUFFER_BINDING = $9302;
  GL_BUFFER_DATA_SIZE = $9303;
  GL_NUM_ACTIVE_VARIABLES = $9304;
  GL_ACTIVE_VARIABLES = $9305;
  GL_REFERENCED_BY_VERTEX_SHADER = $9306;
  GL_REFERENCED_BY_TESS_CONTROL_SHADER = $9307;
  GL_REFERENCED_BY_TESS_EVALUATION_SHADER = $9308;
  GL_REFERENCED_BY_GEOMETRY_SHADER = $9309;
  GL_REFERENCED_BY_FRAGMENT_SHADER = $930A;
  GL_REFERENCED_BY_COMPUTE_SHADER = $930B;
  GL_TOP_LEVEL_ARRAY_SIZE = $930C;
  GL_TOP_LEVEL_ARRAY_STRIDE = $930D;
  GL_LOCATION = $930E;
  GL_LOCATION_INDEX = $930F;
  GL_IS_PER_PATCH = $92E7;
  GL_SHADER_STORAGE_BUFFER = $90D2;
  GL_SHADER_STORAGE_BUFFER_BINDING = $90D3;
  GL_SHADER_STORAGE_BUFFER_START = $90D4;
  GL_SHADER_STORAGE_BUFFER_SIZE = $90D5;
  GL_MAX_VERTEX_SHADER_STORAGE_BLOCKS = $90D6;
  GL_MAX_GEOMETRY_SHADER_STORAGE_BLOCKS = $90D7;
  GL_MAX_TESS_CONTROL_SHADER_STORAGE_BLOCKS = $90D8;
  GL_MAX_TESS_EVALUATION_SHADER_STORAGE_BLOCKS = $90D9;
  GL_MAX_FRAGMENT_SHADER_STORAGE_BLOCKS = $90DA;
  GL_MAX_COMPUTE_SHADER_STORAGE_BLOCKS = $90DB;
  GL_MAX_COMBINED_SHADER_STORAGE_BLOCKS = $90DC;
  GL_MAX_SHADER_STORAGE_BUFFER_BINDINGS = $90DD;
  GL_MAX_SHADER_STORAGE_BLOCK_SIZE = $90DE;
  GL_SHADER_STORAGE_BUFFER_OFFSET_ALIGNMENT = $90DF;
  GL_SHADER_STORAGE_BARRIER_BIT = $00002000;
  GL_MAX_COMBINED_SHADER_OUTPUT_RESOURCES = $8F39;
  GL_DEPTH_STENCIL_TEXTURE_MODE = $90EA;
  GL_TEXTURE_BUFFER_OFFSET = $919D;
  GL_TEXTURE_BUFFER_SIZE = $919E;
  GL_TEXTURE_BUFFER_OFFSET_ALIGNMENT = $919F;
  GL_TEXTURE_VIEW_MIN_LEVEL = $82DB;
  GL_TEXTURE_VIEW_NUM_LEVELS = $82DC;
  GL_TEXTURE_VIEW_MIN_LAYER = $82DD;
  GL_TEXTURE_VIEW_NUM_LAYERS = $82DE;
  GL_TEXTURE_IMMUTABLE_LEVELS = $82DF;
  GL_VERTEX_ATTRIB_BINDING = $82D4;
  GL_VERTEX_ATTRIB_RELATIVE_OFFSET = $82D5;
  GL_VERTEX_BINDING_DIVISOR = $82D6;
  GL_VERTEX_BINDING_OFFSET = $82D7;
  GL_VERTEX_BINDING_STRIDE = $82D8;
  GL_MAX_VERTEX_ATTRIB_RELATIVE_OFFSET = $82D9;
  GL_MAX_VERTEX_ATTRIB_BINDINGS = $82DA;
  GL_VERTEX_BINDING_BUFFER = $8F4F;

var
  glClearBufferData: procedure(target: GLenum; internalformat: GLenum; format: GLenum; type_: GLenum; data: Pointer); apicall;
  glClearBufferSubData: procedure(target: GLenum; internalformat: GLenum; offset: GLintptr; size: GLsizeiptr; format: GLenum; type_: GLenum; data: Pointer); apicall;
  glDispatchCompute: procedure(num_groups_x: GLuint; num_groups_y: GLuint; num_groups_z: GLuint); apicall;
  glDispatchComputeIndirect: procedure(indirect: GLintptr); apicall;
  glCopyImageSubData: procedure(srcName: GLuint; srcTarget: GLenum; srcLevel: GLint; srcX: GLint; srcY: GLint; srcZ: GLint; dstName: GLuint; dstTarget: GLenum; dstLevel: GLint; dstX: GLint; dstY: GLint; dstZ: GLint; srcWidth: GLsizei; srcHeight: GLsizei; srcDepth: GLsizei); apicall;
  glFramebufferParameteri: procedure(target: GLenum; pname: GLenum; param: GLint); apicall;
  glGetFramebufferParameteriv: procedure(target: GLenum; pname: GLenum; params: PGLint); apicall;
  glGetInternalformati64v: procedure(target: GLenum; internalformat: GLenum; pname: GLenum; count: GLsizei; params: PGLint64); apicall;
  glInvalidateTexSubImage: procedure(texture: GLuint; level: GLint; xoffset: GLint; yoffset: GLint; zoffset: GLint; width: GLsizei; height: GLsizei; depth: GLsizei); apicall;
  glInvalidateTexImage: procedure(texture: GLuint; level: GLint); apicall;
  glInvalidateBufferSubData: procedure(buffer: GLuint; offset: GLintptr; length: GLsizeiptr); apicall;
  glInvalidateBufferData: procedure(buffer: GLuint); apicall;
  glInvalidateFramebuffer: procedure(target: GLenum; numAttachments: GLsizei; attachments: PGLenum); apicall;
  glInvalidateSubFramebuffer: procedure(target: GLenum; numAttachments: GLsizei; attachments: PGLenum; x: GLint; y: GLint; width: GLsizei; height: GLsizei); apicall;
  glMultiDrawArraysIndirect: procedure(mode: GLenum; indirect: Pointer; drawcount: GLsizei; stride: GLsizei); apicall;
  glMultiDrawElementsIndirect: procedure(mode: GLenum; type_: GLenum; indirect: Pointer; drawcount: GLsizei; stride: GLsizei); apicall;
  glGetProgramInterfaceiv: procedure(program_: GLuint; programInterface: GLenum; pname: GLenum; params: PGLint); apicall;
  glGetProgramResourceIndex: function(program_: GLuint; programInterface: GLenum; name: PGLchar): GLuint; apicall;
  glGetProgramResourceName: procedure(program_: GLuint; programInterface: GLenum; index: GLuint; bufSize: GLsizei; length: PGLsizei; name: PGLchar); apicall;
  glGetProgramResourceiv: procedure(program_: GLuint; programInterface: GLenum; index: GLuint; propCount: GLsizei; props: PGLenum; count: GLsizei; length: PGLsizei; params: PGLint); apicall;
  glGetProgramResourceLocation: function(program_: GLuint; programInterface: GLenum; name: PGLchar): GLint; apicall;
  glGetProgramResourceLocationIndex: function(program_: GLuint; programInterface: GLenum; name: PGLchar): GLint; apicall;
  glShaderStorageBlockBinding: procedure(program_: GLuint; storageBlockIndex: GLuint; storageBlockBinding: GLuint); apicall;
  glTexBufferRange: procedure(target: GLenum; internalformat: GLenum; buffer: GLuint; offset: GLintptr; size: GLsizeiptr); apicall;
  glTexStorage2DMultisample: procedure(target: GLenum; samples: GLsizei; internalformat: GLenum; width: GLsizei; height: GLsizei; fixedsamplelocations: GLboolean); apicall;
  glTexStorage3DMultisample: procedure(target: GLenum; samples: GLsizei; internalformat: GLenum; width: GLsizei; height: GLsizei; depth: GLsizei; fixedsamplelocations: GLboolean); apicall;
  glTextureView: procedure(texture: GLuint; target: GLenum; origtexture: GLuint; internalformat: GLenum; minlevel: GLuint; numlevels: GLuint; minlayer: GLuint; numlayers: GLuint); apicall;
  glBindVertexBuffer: procedure(bindingindex: GLuint; buffer: GLuint; offset: GLintptr; stride: GLsizei); apicall;
  glVertexAttribFormat: procedure(attribindex: GLuint; size: GLint; type_: GLenum; normalized: GLboolean; relativeoffset: GLuint); apicall;
  glVertexAttribIFormat: procedure(attribindex: GLuint; size: GLint; type_: GLenum; relativeoffset: GLuint); apicall;
  glVertexAttribLFormat: procedure(attribindex: GLuint; size: GLint; type_: GLenum; relativeoffset: GLuint); apicall;
  glVertexAttribBinding: procedure(attribindex: GLuint; bindingindex: GLuint); apicall;
  glVertexBindingDivisor: procedure(bindingindex: GLuint; divisor: GLuint); apicall;
  glDebugMessageControl: procedure(source: GLenum; type_: GLenum; severity: GLenum; count: GLsizei; ids: PGLuint; enabled: GLboolean); apicall;
  glDebugMessageInsert: procedure(source: GLenum; type_: GLenum; id: GLuint; severity: GLenum; length: GLsizei; buf: PGLchar); apicall;
  glDebugMessageCallback: procedure(callback: GLDEBUGPROC; userParam: Pointer); apicall;
  glGetDebugMessageLog: function(count: GLuint; bufSize: GLsizei; sources: PGLenum; types: PGLenum; ids: PGLuint; severities: PGLenum; lengths: PGLsizei; messageLog: PGLchar): GLuint; apicall;
  glPushDebugGroup: procedure(source: GLenum; id: GLuint; length: GLsizei; message: PGLchar); apicall;
  glPopDebugGroup: procedure; apicall;
  glObjectLabel: procedure(identifier: GLenum; name: GLuint; length: GLsizei; label_: PGLchar); apicall;
  glGetObjectLabel: procedure(identifier: GLenum; name: GLuint; bufSize: GLsizei; length: PGLsizei; label_: PGLchar); apicall;
  glObjectPtrLabel: procedure(ptr: Pointer; length: GLsizei; label_: PGLchar); apicall;
  glGetObjectPtrLabel: procedure(ptr: Pointer; bufSize: GLsizei; length: PGLsizei; label_: PGLchar); apicall;
{$endif}
{$endregion}

{ OpenGL 4.3 compatibility profile }

{$region gl43 compatibility}
{$if defined(gl43) and defined(glcompat)}
const
  GL_DISPLAY_LIST = $82E7;
{$endif}
{$endregion}

{ OpenGL 4.4 }

{$region gl44}
{$ifdef gl44}
const
  GL_MAX_VERTEX_ATTRIB_STRIDE = $82E5;
  GL_PRIMITIVE_RESTART_FOR_PATCHES_SUPPORTED = $8221;
  GL_TEXTURE_BUFFER_BINDING = $8C2A;
  GL_MAP_PERSISTENT_BIT = $0040;
  GL_MAP_COHERENT_BIT = $0080;
  GL_DYNAMIC_STORAGE_BIT = $0100;
  GL_CLIENT_STORAGE_BIT = $0200;
  GL_CLIENT_MAPPED_BUFFER_BARRIER_BIT = $00004000;
  GL_BUFFER_IMMUTABLE_STORAGE = $821F;
  GL_BUFFER_STORAGE_FLAGS = $8220;
  GL_CLEAR_TEXTURE = $9365;
  GL_LOCATION_COMPONENT = $934A;
  GL_TRANSFORM_FEEDBACK_BUFFER_INDEX = $934B;
  GL_TRANSFORM_FEEDBACK_BUFFER_STRIDE = $934C;
  GL_QUERY_BUFFER = $9192;
  GL_QUERY_BUFFER_BARRIER_BIT = $00008000;
  GL_QUERY_BUFFER_BINDING = $9193;
  GL_QUERY_RESULT_NO_WAIT = $9194;
  GL_MIRROR_CLAMP_TO_EDGE = $8743;

var
  glBufferStorage: procedure(target: GLenum; size: GLsizeiptr; data: Pointer; flags: GLbitfield); apicall;
  glClearTexImage: procedure(texture: GLuint; level: GLint; format: GLenum; type_: GLenum; data: Pointer); apicall;
  glClearTexSubImage: procedure(texture: GLuint; level: GLint; xoffset: GLint; yoffset: GLint; zoffset: GLint; width: GLsizei; height: GLsizei; depth: GLsizei; format: GLenum; type_: GLenum; data: Pointer); apicall;
  glBindBuffersBase: procedure(target: GLenum; first: GLuint; count: GLsizei; buffers: PGLuint); apicall;
  glBindBuffersRange: procedure(target: GLenum; first: GLuint; count: GLsizei; buffers: PGLuint; offsets: PGLintptr; sizes: PGLsizeiptr); apicall;
  glBindTextures: procedure(first: GLuint; count: GLsizei; textures: PGLuint); apicall;
  glBindSamplers: procedure(first: GLuint; count: GLsizei; samplers: PGLuint); apicall;
  glBindImageTextures: procedure(first: GLuint; count: GLsizei; textures: PGLuint); apicall;
  glBindVertexBuffers: procedure(first: GLuint; count: GLsizei; buffers: PGLuint; offsets: PGLintptr; strides: PGLsizei); apicall;
{$endif}
{$endregion}

{ OpenGL 4.5 }

{$region gl45}
{$ifdef gl45}
const
  GL_CONTEXT_LOST = $0507;
  GL_NEGATIVE_ONE_TO_ONE = $935E;
  GL_ZERO_TO_ONE = $935F;
  GL_CLIP_ORIGIN = $935C;
  GL_CLIP_DEPTH_MODE = $935D;
  GL_QUERY_WAIT_INVERTED = $8E17;
  GL_QUERY_NO_WAIT_INVERTED = $8E18;
  GL_QUERY_BY_REGION_WAIT_INVERTED = $8E19;
  GL_QUERY_BY_REGION_NO_WAIT_INVERTED = $8E1A;
  GL_MAX_CULL_DISTANCES = $82F9;
  GL_MAX_COMBINED_CLIP_AND_CULL_DISTANCES = $82FA;
  GL_TEXTURE_TARGET = $1006;
  GL_QUERY_TARGET = $82EA;
  GL_GUILTY_CONTEXT_RESET = $8253;
  GL_INNOCENT_CONTEXT_RESET = $8254;
  GL_UNKNOWN_CONTEXT_RESET = $8255;
  GL_RESET_NOTIFICATION_STRATEGY = $8256;
  GL_LOSE_CONTEXT_ON_RESET = $8252;
  GL_NO_RESET_NOTIFICATION = $8261;
  GL_CONTEXT_FLAG_ROBUST_ACCESS_BIT = $00000004;
  GL_CONTEXT_RELEASE_BEHAVIOR = $82FB;
  GL_CONTEXT_RELEASE_BEHAVIOR_FLUSH = $82FC;

var
  glClipControl: procedure(origin: GLenum; depth: GLenum); apicall;
  glCreateTransformFeedbacks: procedure(n: GLsizei; ids: PGLuint); apicall;
  glTransformFeedbackBufferBase: procedure(xfb: GLuint; index: GLuint; buffer: GLuint); apicall;
  glTransformFeedbackBufferRange: procedure(xfb: GLuint; index: GLuint; buffer: GLuint; offset: GLintptr; size: GLsizeiptr); apicall;
  glGetTransformFeedbackiv: procedure(xfb: GLuint; pname: GLenum; param: PGLint); apicall;
  glGetTransformFeedbacki_v: procedure(xfb: GLuint; pname: GLenum; index: GLuint; param: PGLint); apicall;
  glGetTransformFeedbacki64_v: procedure(xfb: GLuint; pname: GLenum; index: GLuint; param: PGLint64); apicall;
  glCreateBuffers: procedure(n: GLsizei; buffers: PGLuint); apicall;
  glNamedBufferStorage: procedure(buffer: GLuint; size: GLsizeiptr; data: Pointer; flags: GLbitfield); apicall;
  glNamedBufferData: procedure(buffer: GLuint; size: GLsizeiptr; data: Pointer; usage: GLenum); apicall;
  glNamedBufferSubData: procedure(buffer: GLuint; offset: GLintptr; size: GLsizeiptr; data: Pointer); apicall;
  glCopyNamedBufferSubData: procedure(readBuffer: GLuint; writeBuffer: GLuint; readOffset: GLintptr; writeOffset: GLintptr; size: GLsizeiptr); apicall;
  glClearNamedBufferData: procedure(buffer: GLuint; internalformat: GLenum; format: GLenum; type_: GLenum; data: Pointer); apicall;
  glClearNamedBufferSubData: procedure(buffer: GLuint; internalformat: GLenum; offset: GLintptr; size: GLsizeiptr; format: GLenum; type_: GLenum; data: Pointer); apicall;
  glMapNamedBuffer: function(buffer: GLuint; access: GLenum): Pointer; apicall;
  glMapNamedBufferRange: function(buffer: GLuint; offset: GLintptr; length: GLsizeiptr; access: GLbitfield): Pointer; apicall;
  glUnmapNamedBuffer: function(buffer: GLuint): GLboolean; apicall;
  glFlushMappedNamedBufferRange: procedure(buffer: GLuint; offset: GLintptr; length: GLsizeiptr); apicall;
  glGetNamedBufferParameteriv: procedure(buffer: GLuint; pname: GLenum; params: PGLint); apicall;
  glGetNamedBufferParameteri64v: procedure(buffer: GLuint; pname: GLenum; params: PGLint64); apicall;
  glGetNamedBufferPointerv: procedure(buffer: GLuint; pname: GLenum; params: PPointer); apicall;
  glGetNamedBufferSubData: procedure(buffer: GLuint; offset: GLintptr; size: GLsizeiptr; data: Pointer); apicall;
  glCreateFramebuffers: procedure(n: GLsizei; framebuffers: PGLuint); apicall;
  glNamedFramebufferRenderbuffer: procedure(framebuffer: GLuint; attachment: GLenum; renderbuffertarget: GLenum; renderbuffer: GLuint); apicall;
  glNamedFramebufferParameteri: procedure(framebuffer: GLuint; pname: GLenum; param: GLint); apicall;
  glNamedFramebufferTexture: procedure(framebuffer: GLuint; attachment: GLenum; texture: GLuint; level: GLint); apicall;
  glNamedFramebufferTextureLayer: procedure(framebuffer: GLuint; attachment: GLenum; texture: GLuint; level: GLint; layer: GLint); apicall;
  glNamedFramebufferDrawBuffer: procedure(framebuffer: GLuint; buf: GLenum); apicall;
  glNamedFramebufferDrawBuffers: procedure(framebuffer: GLuint; n: GLsizei; bufs: PGLenum); apicall;
  glNamedFramebufferReadBuffer: procedure(framebuffer: GLuint; src: GLenum); apicall;
  glInvalidateNamedFramebufferData: procedure(framebuffer: GLuint; numAttachments: GLsizei; attachments: PGLenum); apicall;
  glInvalidateNamedFramebufferSubData: procedure(framebuffer: GLuint; numAttachments: GLsizei; attachments: PGLenum; x: GLint; y: GLint; width: GLsizei; height: GLsizei); apicall;
  glClearNamedFramebufferiv: procedure(framebuffer: GLuint; buffer: GLenum; drawbuffer: GLint; value: PGLint); apicall;
  glClearNamedFramebufferuiv: procedure(framebuffer: GLuint; buffer: GLenum; drawbuffer: GLint; value: PGLuint); apicall;
  glClearNamedFramebufferfv: procedure(framebuffer: GLuint; buffer: GLenum; drawbuffer: GLint; value: PGLfloat); apicall;
  glClearNamedFramebufferfi: procedure(framebuffer: GLuint; buffer: GLenum; drawbuffer: GLint; depth: GLfloat; stencil: GLint); apicall;
  glBlitNamedFramebuffer: procedure(readFramebuffer: GLuint; drawFramebuffer: GLuint; srcX0: GLint; srcY0: GLint; srcX1: GLint; srcY1: GLint; dstX0: GLint; dstY0: GLint; dstX1: GLint; dstY1: GLint; mask: GLbitfield; filter: GLenum); apicall;
  glCheckNamedFramebufferStatus: function(framebuffer: GLuint; target: GLenum): GLenum; apicall;
  glGetNamedFramebufferParameteriv: procedure(framebuffer: GLuint; pname: GLenum; param: PGLint); apicall;
  glGetNamedFramebufferAttachmentParameteriv: procedure(framebuffer: GLuint; attachment: GLenum; pname: GLenum; params: PGLint); apicall;
  glCreateRenderbuffers: procedure(n: GLsizei; renderbuffers: PGLuint); apicall;
  glNamedRenderbufferStorage: procedure(renderbuffer: GLuint; internalformat: GLenum; width: GLsizei; height: GLsizei); apicall;
  glNamedRenderbufferStorageMultisample: procedure(renderbuffer: GLuint; samples: GLsizei; internalformat: GLenum; width: GLsizei; height: GLsizei); apicall;
  glGetNamedRenderbufferParameteriv: procedure(renderbuffer: GLuint; pname: GLenum; params: PGLint); apicall;
  glCreateTextures: procedure(target: GLenum; n: GLsizei; textures: PGLuint); apicall;
  glTextureBuffer: procedure(texture: GLuint; internalformat: GLenum; buffer: GLuint); apicall;
  glTextureBufferRange: procedure(texture: GLuint; internalformat: GLenum; buffer: GLuint; offset: GLintptr; size: GLsizeiptr); apicall;
  glTextureStorage1D: procedure(texture: GLuint; levels: GLsizei; internalformat: GLenum; width: GLsizei); apicall;
  glTextureStorage2D: procedure(texture: GLuint; levels: GLsizei; internalformat: GLenum; width: GLsizei; height: GLsizei); apicall;
  glTextureStorage3D: procedure(texture: GLuint; levels: GLsizei; internalformat: GLenum; width: GLsizei; height: GLsizei; depth: GLsizei); apicall;
  glTextureStorage2DMultisample: procedure(texture: GLuint; samples: GLsizei; internalformat: GLenum; width: GLsizei; height: GLsizei; fixedsamplelocations: GLboolean); apicall;
  glTextureStorage3DMultisample: procedure(texture: GLuint; samples: GLsizei; internalformat: GLenum; width: GLsizei; height: GLsizei; depth: GLsizei; fixedsamplelocations: GLboolean); apicall;
  glTextureSubImage1D: procedure(texture: GLuint; level: GLint; xoffset: GLint; width: GLsizei; format: GLenum; type_: GLenum; pixels: Pointer); apicall;
  glTextureSubImage2D: procedure(texture: GLuint; level: GLint; xoffset: GLint; yoffset: GLint; width: GLsizei; height: GLsizei; format: GLenum; type_: GLenum; pixels: Pointer); apicall;
  glTextureSubImage3D: procedure(texture: GLuint; level: GLint; xoffset: GLint; yoffset: GLint; zoffset: GLint; width: GLsizei; height: GLsizei; depth: GLsizei; format: GLenum; type_: GLenum; pixels: Pointer); apicall;
  glCompressedTextureSubImage1D: procedure(texture: GLuint; level: GLint; xoffset: GLint; width: GLsizei; format: GLenum; imageSize: GLsizei; data: Pointer); apicall;
  glCompressedTextureSubImage2D: procedure(texture: GLuint; level: GLint; xoffset: GLint; yoffset: GLint; width: GLsizei; height: GLsizei; format: GLenum; imageSize: GLsizei; data: Pointer); apicall;
  glCompressedTextureSubImage3D: procedure(texture: GLuint; level: GLint; xoffset: GLint; yoffset: GLint; zoffset: GLint; width: GLsizei; height: GLsizei; depth: GLsizei; format: GLenum; imageSize: GLsizei; data: Pointer); apicall;
  glCopyTextureSubImage1D: procedure(texture: GLuint; level: GLint; xoffset: GLint; x: GLint; y: GLint; width: GLsizei); apicall;
  glCopyTextureSubImage2D: procedure(texture: GLuint; level: GLint; xoffset: GLint; yoffset: GLint; x: GLint; y: GLint; width: GLsizei; height: GLsizei); apicall;
  glCopyTextureSubImage3D: procedure(texture: GLuint; level: GLint; xoffset: GLint; yoffset: GLint; zoffset: GLint; x: GLint; y: GLint; width: GLsizei; height: GLsizei); apicall;
  glTextureParameterf: procedure(texture: GLuint; pname: GLenum; param: GLfloat); apicall;
  glTextureParameterfv: procedure(texture: GLuint; pname: GLenum; param: PGLfloat); apicall;
  glTextureParameteri: procedure(texture: GLuint; pname: GLenum; param: GLint); apicall;
  glTextureParameterIiv: procedure(texture: GLuint; pname: GLenum; params: PGLint); apicall;
  glTextureParameterIuiv: procedure(texture: GLuint; pname: GLenum; params: PGLuint); apicall;
  glTextureParameteriv: procedure(texture: GLuint; pname: GLenum; param: PGLint); apicall;
  glGenerateTextureMipmap: procedure(texture: GLuint); apicall;
  glBindTextureUnit: procedure(unit_: GLuint; texture: GLuint); apicall;
  glGetTextureImage: procedure(texture: GLuint; level: GLint; format: GLenum; type_: GLenum; bufSize: GLsizei; pixels: Pointer); apicall;
  glGetCompressedTextureImage: procedure(texture: GLuint; level: GLint; bufSize: GLsizei; pixels: Pointer); apicall;
  glGetTextureLevelParameterfv: procedure(texture: GLuint; level: GLint; pname: GLenum; params: PGLfloat); apicall;
  glGetTextureLevelParameteriv: procedure(texture: GLuint; level: GLint; pname: GLenum; params: PGLint); apicall;
  glGetTextureParameterfv: procedure(texture: GLuint; pname: GLenum; params: PGLfloat); apicall;
  glGetTextureParameterIiv: procedure(texture: GLuint; pname: GLenum; params: PGLint); apicall;
  glGetTextureParameterIuiv: procedure(texture: GLuint; pname: GLenum; params: PGLuint); apicall;
  glGetTextureParameteriv: procedure(texture: GLuint; pname: GLenum; params: PGLint); apicall;
  glCreateVertexArrays: procedure(n: GLsizei; arrays: PGLuint); apicall;
  glDisableVertexArrayAttrib: procedure(vaobj: GLuint; index: GLuint); apicall;
  glEnableVertexArrayAttrib: procedure(vaobj: GLuint; index: GLuint); apicall;
  glVertexArrayElementBuffer: procedure(vaobj: GLuint; buffer: GLuint); apicall;
  glVertexArrayVertexBuffer: procedure(vaobj: GLuint; bindingindex: GLuint; buffer: GLuint; offset: GLintptr; stride: GLsizei); apicall;
  glVertexArrayVertexBuffers: procedure(vaobj: GLuint; first: GLuint; count: GLsizei; buffers: PGLuint; offsets: PGLintptr; strides: PGLsizei); apicall;
  glVertexArrayAttribBinding: procedure(vaobj: GLuint; attribindex: GLuint; bindingindex: GLuint); apicall;
  glVertexArrayAttribFormat: procedure(vaobj: GLuint; attribindex: GLuint; size: GLint; type_: GLenum; normalized: GLboolean; relativeoffset: GLuint); apicall;
  glVertexArrayAttribIFormat: procedure(vaobj: GLuint; attribindex: GLuint; size: GLint; type_: GLenum; relativeoffset: GLuint); apicall;
  glVertexArrayAttribLFormat: procedure(vaobj: GLuint; attribindex: GLuint; size: GLint; type_: GLenum; relativeoffset: GLuint); apicall;
  glVertexArrayBindingDivisor: procedure(vaobj: GLuint; bindingindex: GLuint; divisor: GLuint); apicall;
  glGetVertexArrayiv: procedure(vaobj: GLuint; pname: GLenum; param: PGLint); apicall;
  glGetVertexArrayIndexediv: procedure(vaobj: GLuint; index: GLuint; pname: GLenum; param: PGLint); apicall;
  glGetVertexArrayIndexed64iv: procedure(vaobj: GLuint; index: GLuint; pname: GLenum; param: PGLint64); apicall;
  glCreateSamplers: procedure(n: GLsizei; samplers: PGLuint); apicall;
  glCreateProgramPipelines: procedure(n: GLsizei; pipelines: PGLuint); apicall;
  glCreateQueries: procedure(target: GLenum; n: GLsizei; ids: PGLuint); apicall;
  glGetQueryBufferObjecti64v: procedure(id: GLuint; buffer: GLuint; pname: GLenum; offset: GLintptr); apicall;
  glGetQueryBufferObjectiv: procedure(id: GLuint; buffer: GLuint; pname: GLenum; offset: GLintptr); apicall;
  glGetQueryBufferObjectui64v: procedure(id: GLuint; buffer: GLuint; pname: GLenum; offset: GLintptr); apicall;
  glGetQueryBufferObjectuiv: procedure(id: GLuint; buffer: GLuint; pname: GLenum; offset: GLintptr); apicall;
  glMemoryBarrierByRegion: procedure(barriers: GLbitfield); apicall;
  glGetTextureSubImage: procedure(texture: GLuint; level: GLint; xoffset: GLint; yoffset: GLint; zoffset: GLint; width: GLsizei; height: GLsizei; depth: GLsizei; format: GLenum; type_: GLenum; bufSize: GLsizei; pixels: Pointer); apicall;
  glGetCompressedTextureSubImage: procedure(texture: GLuint; level: GLint; xoffset: GLint; yoffset: GLint; zoffset: GLint; width: GLsizei; height: GLsizei; depth: GLsizei; bufSize: GLsizei; pixels: Pointer); apicall;
  glGetGraphicsResetStatus: function: GLenum; apicall;
  glGetnCompressedTexImage: procedure(target: GLenum; lod: GLint; bufSize: GLsizei; pixels: Pointer); apicall;
  glGetnTexImage: procedure(target: GLenum; level: GLint; format: GLenum; type_: GLenum; bufSize: GLsizei; pixels: Pointer); apicall;
  glGetnUniformdv: procedure(program_: GLuint; location: GLint; bufSize: GLsizei; params: PGLdouble); apicall;
  glGetnUniformfv: procedure(program_: GLuint; location: GLint; bufSize: GLsizei; params: PGLfloat); apicall;
  glGetnUniformiv: procedure(program_: GLuint; location: GLint; bufSize: GLsizei; params: PGLint); apicall;
  glGetnUniformuiv: procedure(program_: GLuint; location: GLint; bufSize: GLsizei; params: PGLuint); apicall;
  glReadnPixels: procedure(x: GLint; y: GLint; width: GLsizei; height: GLsizei; format: GLenum; type_: GLenum; bufSize: GLsizei; data: Pointer); apicall;
  glTextureBarrier: procedure; apicall;
{$endif}
{$endregion}

{ OpenGL 4.5 compatibility profile }

{$region gl45 compatibility}
{$if defined(gl45) and defined(glcompat)}
const
  GL_COLOR_TABLE = $80D0;
  GL_POST_CONVOLUTION_COLOR_TABLE = $80D1;
  GL_POST_COLOR_MATRIX_COLOR_TABLE = $80D2;
  GL_PROXY_COLOR_TABLE = $80D3;
  GL_PROXY_POST_CONVOLUTION_COLOR_TABLE = $80D4;
  GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE = $80D5;
  GL_CONVOLUTION_1D = $8010;
  GL_CONVOLUTION_2D = $8011;
  GL_SEPARABLE_2D = $8012;
  GL_HISTOGRAM = $8024;
  GL_PROXY_HISTOGRAM = $8025;
  GL_MINMAX = $802E;

var
  glGetnMapdv: procedure(target: GLenum; query: GLenum; bufSize: GLsizei; v: PGLdouble); apicall;
  glGetnMapfv: procedure(target: GLenum; query: GLenum; bufSize: GLsizei; v: PGLfloat); apicall;
  glGetnMapiv: procedure(target: GLenum; query: GLenum; bufSize: GLsizei; v: PGLint); apicall;
  glGetnPixelMapfv: procedure(map: GLenum; bufSize: GLsizei; values: PGLfloat); apicall;
  glGetnPixelMapuiv: procedure(map: GLenum; bufSize: GLsizei; values: PGLuint); apicall;
  glGetnPixelMapusv: procedure(map: GLenum; bufSize: GLsizei; values: PGLushort); apicall;
  glGetnPolygonStipple: procedure(bufSize: GLsizei; pattern: PGLubyte); apicall;
  glGetnColorTable: procedure(target: GLenum; format: GLenum; type_: GLenum; bufSize: GLsizei; table: Pointer); apicall;
  glGetnConvolutionFilter: procedure(target: GLenum; format: GLenum; type_: GLenum; bufSize: GLsizei; image: Pointer); apicall;
  glGetnSeparableFilter: procedure(target: GLenum; format: GLenum; type_: GLenum; rowBufSize: GLsizei; row: Pointer; columnBufSize: GLsizei; column: Pointer; span: Pointer); apicall;
  glGetnHistogram: procedure(target: GLenum; reset: GLboolean; format: GLenum; type_: GLenum; bufSize: GLsizei; values: Pointer); apicall;
  glGetnMinmax: procedure(target: GLenum; reset: GLboolean; format: GLenum; type_: GLenum; bufSize: GLsizei; values: Pointer); apicall;
{$endif}
{$endregion}

{ OpenGL 4.6 }

{$region gl46}
{$ifdef gl46}
const
  GL_SHADER_BINARY_FORMAT_SPIR_V = $9551;
  GL_SPIR_V_BINARY = $9552;
  GL_PARAMETER_BUFFER = $80EE;
  GL_PARAMETER_BUFFER_BINDING = $80EF;
  GL_CONTEXT_FLAG_NO_ERROR_BIT = $00000008;
  GL_VERTICES_SUBMITTED = $82EE;
  GL_PRIMITIVES_SUBMITTED = $82EF;
  GL_VERTEX_SHADER_INVOCATIONS = $82F0;
  GL_TESS_CONTROL_SHADER_PATCHES = $82F1;
  GL_TESS_EVALUATION_SHADER_INVOCATIONS = $82F2;
  GL_GEOMETRY_SHADER_PRIMITIVES_EMITTED = $82F3;
  GL_FRAGMENT_SHADER_INVOCATIONS = $82F4;
  GL_COMPUTE_SHADER_INVOCATIONS = $82F5;
  GL_CLIPPING_INPUT_PRIMITIVES = $82F6;
  GL_CLIPPING_OUTPUT_PRIMITIVES = $82F7;
  GL_POLYGON_OFFSET_CLAMP = $8E1B;
  GL_SPIR_V_EXTENSIONS = $9553;
  GL_NUM_SPIR_V_EXTENSIONS = $9554;
  GL_TEXTURE_MAX_ANISOTROPY = $84FE;
  GL_MAX_TEXTURE_MAX_ANISOTROPY = $84FF;
  GL_TRANSFORM_FEEDBACK_OVERFLOW = $82EC;
  GL_TRANSFORM_FEEDBACK_STREAM_OVERFLOW = $82ED;

var
  glSpecializeShader: procedure(shader: GLuint; pEntryPoint: PGLchar; numSpecializationConstants: GLuint; pConstantIndex: PGLuint; pConstantValue: PGLuint); apicall;
  glMultiDrawArraysIndirectCount: procedure(mode: GLenum; indirect: Pointer; drawcount: GLintptr; maxdrawcount: GLsizei; stride: GLsizei); apicall;
  glMultiDrawElementsIndirectCount: procedure(mode: GLenum; type_: GLenum; indirect: Pointer; drawcount: GLintptr; maxdrawcount: GLsizei; stride: GLsizei); apicall;
  glPolygonOffsetClamp: procedure(factor: GLfloat; units: GLfloat; clamp: GLfloat); apicall;
{$endif}
{$endregion}
{$endif}

{ TOpenGLGetProc returns the address of an OpenGL function by name or nil if
  the function could not be found }

type
  TOpenGLGetProc = function(Name: PChar): Pointer;

{ OpenGLLoadFunctions loads every function belonging to the version selected
  in render.inc. An OpenGL context must be created and made current before
  calling this function. The result is True if every function was found and
  the current context version is at least the selected version.

  The overload without arguments uses glXGetProcAddress on linux and
  wglGetProcAddress on windows. Use the GetProc overload to load functions
  using another library such as SDL or GLFW. }

function OpenGLLoadFunctions: Boolean; overload;
function OpenGLLoadFunctions(GetProc: TOpenGLGetProc): Boolean; overload;

{ OpenGLGetProc is the function OpenGLLoadFunctions last used to find the
  OpenGL functions, or nil if it has not been called. A library which draws
  with the same context, such as libmpv, can be given it to find them. }

var
  OpenGLGetProc: TOpenGLGetProc;

{ IOpenGLContext provides access to OpenGL rendering and can be obtained by
  using the OpenGLContextCreate function. }

type
  IOpenGLContext = interface
  ['{8F60FCCA-2D15-42E1-9141-C2EB34CCC321}']
    function GetCanRender: Boolean;
    procedure SetCanRender(const Value: Boolean);
    function GetCurrent: Boolean;
    procedure SetCurrent(const Value: Boolean);
    function GetVSync: Boolean;
    procedure SetVSync(const Value: Boolean);
    { Calling GetSize returns the size in pixels of the rendering area }
    procedure GetSize(out Width, Height: Integer);
    { Calling Flip switched the fore and back rendering buffers }
    procedure Flip;
    { MakeCurrent is another way to set the context current to True or False }
    procedure MakeCurrent(Value: Boolean);
    { Lock exclusive access for the calling thread }
    procedure Lock;
    { Unlock exclusive access for the calling thread }
    procedure Unlock;
    { CanRender is set to True when a context is ready and is set
      set to False when starting up or shutting down }
    property CanRender: Boolean read GetCanRender write SetCanRender;
    { Current can be used to control if a context current for a thread }
    property Current: Boolean read GetCurrent write SetCurrent;
    { When VSync is True calls to Flip wait for vertical sync before returning }
    property VSync: Boolean read GetVSync write SetVSync;
  end;

{ TOpenGLParams is used to create an IOpenGLContext. A description of each field
  is provided below. Most of the parameters cannot be altered once a context is
  created. }

  TOpenGLParams = record
    { The number of bits for a depth buffer, defaults to 24 }
    Depth: Byte;
    { The number of bits for a stencil buffer, defaults to 8 }
    Stencil: Byte;
    { Optionally use multisampling for smoothing, defaults to True }
    MultiSampling: Boolean;
    { Optionally the number of multisamples (1, 2, 4, 8, 16), defaults to 4  }
    MultiSamples: Byte;
    { Create TOpenGLParams with the default options }
    class function Create: TOpenGLParams; static;
  end;

{ OpenGLContextCreate returns an OpenGL context given a window handle and a set
  of opengl parameters. If a conxtext could not be created, either due to an
  invalid window handle or unsupported parameter options, then nil is returned.

  For OpenGLContextCreate to return a valid context the OpenGLInfo.IsValid must
  be return a value of True. See the note below for more details.

  The context requests the version selected in render.inc. Desktop versions
  3.2 and above request a core profile, or a compatibility profile when
  glcompat is defined. Embedded versions request an OpenGL ES profile and
  fall back to a desktop context providing the ES compatibility functions,
  except on the Raspberry Pi which has no fallbacks.

  Desktop contexts have a vertex array object bound when first made current,
  so code written for OpenGL ES also works with core profiles. }

function OpenGLContextCreate(Window: GLwindow; const Params: TOpenGLParams): IOpenGLContext;

{ OpenGLContextCurrent returns the current context for the calling thread
  or nil if there is no current context }

function OpenGLContextCurrent: IOpenGLContext;

{ IOpenGLInfo provides information about opengl support on your platform and
  hardware. If the version selected in render.inc is not supported, then
  IsValid will return False, and it is unsafe to call any opengl functions.

  See the notes on the OpenGLInfo for the current state of platform support. }

type
  IOpenGLInfo = interface
  ['{6713F1F2-8642-4734-ABFF-C84614DB3E8A}']
    { IsValid returns True if your hardware supports the selected version }
    function IsValid: Boolean;
    { The actual opengl major version number }
    function Major: Integer;
    { The actual opengl number version number }
    function Minor: Integer;
    { The actual opengl major and minor in string form }
    function MajorMinor: string;
    { The name of the hardware model and driver being used }
    function Renderer: string;
    { The name of the hardware vendor }
    function Vendor: string;
    { The opengl version in string form }
    function Version: string;
    { The supported extensions }
    function Extensions: string;
  end;

{ OpenGLLoad simply calls OpenGLInfo and returns the state of IsValid }

function OpenGLLoad: Boolean;

{ OpenGLInfo returns an IOpenGLInfo interface with more information about
  your hardware. Some platforms and widgetset are not supported. In those
  cases the IOpenGLInfo.IsValid property will return False.

  Current supported platforms are:

  Any platform SDL2 supports using Codebot.OpenGL.SDL. Without that unit
  IsValid is False. }

function OpenGLInfo: IOpenGLInfo;

{ The platform unit assigns these routines in its initialization section.
  Codebot.OpenGL.SDL provides them for SDL windows on any platform, both the
  window of an SDL application and the SDL window a TGraphicsBox places
  inside an LCL form. They are kept out of this unit's uses clauses to avoid a circular unit
  reference. }

var
  OpenGLPlatformInfo: function: IOpenGLInfo;
  OpenGLPlatformContextCreate: function(Window: GLwindow; const Params: TOpenGLParams): IOpenGLContext;
  OpenGLPlatformContextCurrent: function: IOpenGLContext;

implementation

{$ifdef linux}
const
  LibName = 'libGL.so.1';
  ProcAddressName = 'glXGetProcAddressARB';
{$endif}
{$ifdef windows}
const
  LibName = 'opengl32.dll';
  ProcAddressName = 'wglGetProcAddress';
{$endif}

var
  Lib: HModule;
  ProcAddress: function(Name: PChar): Pointer; apicall;

function DefaultGetProc(Name: PChar): Pointer;
begin
  Result := nil;
  if Lib = 0 then
  begin
    Lib := LibraryLoad(LibName);
    if Lib = 0 then
      Exit;
    ProcAddress := LibraryGetProc(Lib, ProcAddressName);
  end;
  if @ProcAddress <> nil then
    Result := ProcAddress(Name);
  {$ifdef windows}
  { wglGetProcAddress may return small values instead of nil on failure }
  if (UIntPtr(Result) < 4) or (IntPtr(Result) = -1) then
    Result := nil;
  {$endif}
  { OpenGL 1.1 functions on windows are only exported by the library }
  if Result = nil then
    Result := LibraryGetProc(Lib, Name);
end;

function OpenGLLoadFunctions: Boolean;
begin
  Result := OpenGLLoadFunctions(DefaultGetProc);
end;

{ Compare the version of the current context to the selected version }

function VersionCheck: Boolean;
const
  Embedded = 'OpenGL ES';
var
  S: string;
  Major, Minor, I: Integer;
begin
  Result := False;
  if @glGetString = nil then
    Exit;
  S := PChar(glGetString(GL_VERSION));
  if S = '' then
    Exit;
  if S.BeginsWith(Embedded) <> OpenGLEmbedded then
  begin
    { A desktop context can provide embedded functions through the
      ES compatibility extensions, but not the reverse. The Raspberry Pi
      has no fallbacks, so it requires an OpenGL ES context. }
    {$ifdef raspberrypi}
    Result := False;
    {$else}
    Result := OpenGLEmbedded;
    {$endif}
    Exit;
  end;
  I := 1;
  while (I <= Length(S)) and (not (S[I] in ['0'..'9'])) do
    Inc(I);
  Major := 0;
  while (I <= Length(S)) and (S[I] in ['0'..'9']) do
  begin
    Major := Major * 10 + Ord(S[I]) - Ord('0');
    Inc(I);
  end;
  if (I > Length(S)) or (S[I] <> '.') then
    Exit;
  Inc(I);
  Minor := 0;
  while (I <= Length(S)) and (S[I] in ['0'..'9']) do
  begin
    Minor := Minor * 10 + Ord(S[I]) - Ord('0');
    Inc(I);
  end;
  Result := (Major > OpenGLContextMajor) or
    ((Major = OpenGLContextMajor) and (Minor >= OpenGLContextMinor));
end;

function OpenGLLoadFunctions(GetProc: TOpenGLGetProc): Boolean;
var
  Loaded: Boolean;
  { Functions belonging to versions above the context version are loaded if
    they are found, but are not required }
  Required: Boolean;

  function ContextHas(Major, Minor: Integer): Boolean;
  begin
    Result := (OpenGLContextMajor > Major) or
      ((OpenGLContextMajor = Major) and (OpenGLContextMinor >= Minor));
  end;

  { Some drivers provide a few functions only under the name of the ARB
    extension they came from, such as glGetnTexImageARB on Intel drivers,
    so that name is tried when the core name is not found }

  procedure Load(var Proc; Name: PChar);
  var
    S: string;
  begin
    Pointer(Proc) := GetProc(Name);
    if Pointer(Proc) = nil then
    begin
      S := Name + 'ARB';
      Pointer(Proc) := GetProc(PChar(S));
    end;
    if (Pointer(Proc) = nil) and Required then
      Loaded := False;
  end;

begin
  OpenGLGetProc := GetProc;
  Loaded := True;
  Required := True;
  {$ifdef glesapi}
  { OpenGL ES 2.0 }
  Load(glActiveTexture, 'glActiveTexture');
  Load(glAttachShader, 'glAttachShader');
  Load(glBindAttribLocation, 'glBindAttribLocation');
  Load(glBindBuffer, 'glBindBuffer');
  Load(glBindFramebuffer, 'glBindFramebuffer');
  Load(glBindRenderbuffer, 'glBindRenderbuffer');
  Load(glBindTexture, 'glBindTexture');
  Load(glBlendColor, 'glBlendColor');
  Load(glBlendEquation, 'glBlendEquation');
  Load(glBlendEquationSeparate, 'glBlendEquationSeparate');
  Load(glBlendFunc, 'glBlendFunc');
  Load(glBlendFuncSeparate, 'glBlendFuncSeparate');
  Load(glBufferData, 'glBufferData');
  Load(glBufferSubData, 'glBufferSubData');
  Load(glCheckFramebufferStatus, 'glCheckFramebufferStatus');
  Load(glClear, 'glClear');
  Load(glClearColor, 'glClearColor');
  Load(glClearDepthf, 'glClearDepthf');
  Load(glClearStencil, 'glClearStencil');
  Load(glColorMask, 'glColorMask');
  Load(glCompileShader, 'glCompileShader');
  Load(glCompressedTexImage2D, 'glCompressedTexImage2D');
  Load(glCompressedTexSubImage2D, 'glCompressedTexSubImage2D');
  Load(glCopyTexImage2D, 'glCopyTexImage2D');
  Load(glCopyTexSubImage2D, 'glCopyTexSubImage2D');
  Load(glCreateProgram, 'glCreateProgram');
  Load(glCreateShader, 'glCreateShader');
  Load(glCullFace, 'glCullFace');
  Load(glDeleteBuffers, 'glDeleteBuffers');
  Load(glDeleteFramebuffers, 'glDeleteFramebuffers');
  Load(glDeleteProgram, 'glDeleteProgram');
  Load(glDeleteRenderbuffers, 'glDeleteRenderbuffers');
  Load(glDeleteShader, 'glDeleteShader');
  Load(glDeleteTextures, 'glDeleteTextures');
  Load(glDepthFunc, 'glDepthFunc');
  Load(glDepthMask, 'glDepthMask');
  Load(glDepthRangef, 'glDepthRangef');
  Load(glDetachShader, 'glDetachShader');
  Load(glDisable, 'glDisable');
  Load(glDisableVertexAttribArray, 'glDisableVertexAttribArray');
  Load(glDrawArrays, 'glDrawArrays');
  Load(glDrawElements, 'glDrawElements');
  Load(glEnable, 'glEnable');
  Load(glEnableVertexAttribArray, 'glEnableVertexAttribArray');
  Load(glFinish, 'glFinish');
  Load(glFlush, 'glFlush');
  Load(glFramebufferRenderbuffer, 'glFramebufferRenderbuffer');
  Load(glFramebufferTexture2D, 'glFramebufferTexture2D');
  Load(glFrontFace, 'glFrontFace');
  Load(glGenBuffers, 'glGenBuffers');
  Load(glGenerateMipmap, 'glGenerateMipmap');
  Load(glGenFramebuffers, 'glGenFramebuffers');
  Load(glGenRenderbuffers, 'glGenRenderbuffers');
  Load(glGenTextures, 'glGenTextures');
  Load(glGetActiveAttrib, 'glGetActiveAttrib');
  Load(glGetActiveUniform, 'glGetActiveUniform');
  Load(glGetAttachedShaders, 'glGetAttachedShaders');
  Load(glGetAttribLocation, 'glGetAttribLocation');
  Load(glGetBooleanv, 'glGetBooleanv');
  Load(glGetBufferParameteriv, 'glGetBufferParameteriv');
  Load(glGetError, 'glGetError');
  Load(glGetFloatv, 'glGetFloatv');
  Load(glGetFramebufferAttachmentParameteriv, 'glGetFramebufferAttachmentParameteriv');
  Load(glGetIntegerv, 'glGetIntegerv');
  Load(glGetProgramiv, 'glGetProgramiv');
  Load(glGetProgramInfoLog, 'glGetProgramInfoLog');
  Load(glGetRenderbufferParameteriv, 'glGetRenderbufferParameteriv');
  Load(glGetShaderiv, 'glGetShaderiv');
  Load(glGetShaderInfoLog, 'glGetShaderInfoLog');
  Load(glGetShaderPrecisionFormat, 'glGetShaderPrecisionFormat');
  Load(glGetShaderSource, 'glGetShaderSource');
  Load(glGetString, 'glGetString');
  Load(glGetTexParameterfv, 'glGetTexParameterfv');
  Load(glGetTexParameteriv, 'glGetTexParameteriv');
  Load(glGetUniformfv, 'glGetUniformfv');
  Load(glGetUniformiv, 'glGetUniformiv');
  Load(glGetUniformLocation, 'glGetUniformLocation');
  Load(glGetVertexAttribfv, 'glGetVertexAttribfv');
  Load(glGetVertexAttribiv, 'glGetVertexAttribiv');
  Load(glGetVertexAttribPointerv, 'glGetVertexAttribPointerv');
  Load(glHint, 'glHint');
  Load(glIsBuffer, 'glIsBuffer');
  Load(glIsEnabled, 'glIsEnabled');
  Load(glIsFramebuffer, 'glIsFramebuffer');
  Load(glIsProgram, 'glIsProgram');
  Load(glIsRenderbuffer, 'glIsRenderbuffer');
  Load(glIsShader, 'glIsShader');
  Load(glIsTexture, 'glIsTexture');
  Load(glLineWidth, 'glLineWidth');
  Load(glLinkProgram, 'glLinkProgram');
  Load(glPixelStorei, 'glPixelStorei');
  Load(glPolygonOffset, 'glPolygonOffset');
  Load(glReadPixels, 'glReadPixels');
  Load(glReleaseShaderCompiler, 'glReleaseShaderCompiler');
  Load(glRenderbufferStorage, 'glRenderbufferStorage');
  Load(glSampleCoverage, 'glSampleCoverage');
  Load(glScissor, 'glScissor');
  Load(glShaderBinary, 'glShaderBinary');
  Load(glShaderSource, 'glShaderSource');
  Load(glStencilFunc, 'glStencilFunc');
  Load(glStencilFuncSeparate, 'glStencilFuncSeparate');
  Load(glStencilMask, 'glStencilMask');
  Load(glStencilMaskSeparate, 'glStencilMaskSeparate');
  Load(glStencilOp, 'glStencilOp');
  Load(glStencilOpSeparate, 'glStencilOpSeparate');
  Load(glTexImage2D, 'glTexImage2D');
  Load(glTexParameterf, 'glTexParameterf');
  Load(glTexParameterfv, 'glTexParameterfv');
  Load(glTexParameteri, 'glTexParameteri');
  Load(glTexParameteriv, 'glTexParameteriv');
  Load(glTexSubImage2D, 'glTexSubImage2D');
  Load(glUniform1f, 'glUniform1f');
  Load(glUniform1fv, 'glUniform1fv');
  Load(glUniform1i, 'glUniform1i');
  Load(glUniform1iv, 'glUniform1iv');
  Load(glUniform2f, 'glUniform2f');
  Load(glUniform2fv, 'glUniform2fv');
  Load(glUniform2i, 'glUniform2i');
  Load(glUniform2iv, 'glUniform2iv');
  Load(glUniform3f, 'glUniform3f');
  Load(glUniform3fv, 'glUniform3fv');
  Load(glUniform3i, 'glUniform3i');
  Load(glUniform3iv, 'glUniform3iv');
  Load(glUniform4f, 'glUniform4f');
  Load(glUniform4fv, 'glUniform4fv');
  Load(glUniform4i, 'glUniform4i');
  Load(glUniform4iv, 'glUniform4iv');
  Load(glUniformMatrix2fv, 'glUniformMatrix2fv');
  Load(glUniformMatrix3fv, 'glUniformMatrix3fv');
  Load(glUniformMatrix4fv, 'glUniformMatrix4fv');
  Load(glUseProgram, 'glUseProgram');
  Load(glValidateProgram, 'glValidateProgram');
  Load(glVertexAttrib1f, 'glVertexAttrib1f');
  Load(glVertexAttrib1fv, 'glVertexAttrib1fv');
  Load(glVertexAttrib2f, 'glVertexAttrib2f');
  Load(glVertexAttrib2fv, 'glVertexAttrib2fv');
  Load(glVertexAttrib3f, 'glVertexAttrib3f');
  Load(glVertexAttrib3fv, 'glVertexAttrib3fv');
  Load(glVertexAttrib4f, 'glVertexAttrib4f');
  Load(glVertexAttrib4fv, 'glVertexAttrib4fv');
  Load(glVertexAttribPointer, 'glVertexAttribPointer');
  Load(glViewport, 'glViewport');
  { OpenGL ES 3.0 }
  {$ifdef gles30}
  Load(glReadBuffer, 'glReadBuffer');
  Load(glDrawRangeElements, 'glDrawRangeElements');
  Load(glTexImage3D, 'glTexImage3D');
  Load(glTexSubImage3D, 'glTexSubImage3D');
  Load(glCopyTexSubImage3D, 'glCopyTexSubImage3D');
  Load(glCompressedTexImage3D, 'glCompressedTexImage3D');
  Load(glCompressedTexSubImage3D, 'glCompressedTexSubImage3D');
  Load(glGenQueries, 'glGenQueries');
  Load(glDeleteQueries, 'glDeleteQueries');
  Load(glIsQuery, 'glIsQuery');
  Load(glBeginQuery, 'glBeginQuery');
  Load(glEndQuery, 'glEndQuery');
  Load(glGetQueryiv, 'glGetQueryiv');
  Load(glGetQueryObjectuiv, 'glGetQueryObjectuiv');
  Load(glUnmapBuffer, 'glUnmapBuffer');
  Load(glGetBufferPointerv, 'glGetBufferPointerv');
  Load(glDrawBuffers, 'glDrawBuffers');
  Load(glUniformMatrix2x3fv, 'glUniformMatrix2x3fv');
  Load(glUniformMatrix3x2fv, 'glUniformMatrix3x2fv');
  Load(glUniformMatrix2x4fv, 'glUniformMatrix2x4fv');
  Load(glUniformMatrix4x2fv, 'glUniformMatrix4x2fv');
  Load(glUniformMatrix3x4fv, 'glUniformMatrix3x4fv');
  Load(glUniformMatrix4x3fv, 'glUniformMatrix4x3fv');
  Load(glBlitFramebuffer, 'glBlitFramebuffer');
  Load(glRenderbufferStorageMultisample, 'glRenderbufferStorageMultisample');
  Load(glFramebufferTextureLayer, 'glFramebufferTextureLayer');
  Load(glMapBufferRange, 'glMapBufferRange');
  Load(glFlushMappedBufferRange, 'glFlushMappedBufferRange');
  Load(glBindVertexArray, 'glBindVertexArray');
  Load(glDeleteVertexArrays, 'glDeleteVertexArrays');
  Load(glGenVertexArrays, 'glGenVertexArrays');
  Load(glIsVertexArray, 'glIsVertexArray');
  Load(glGetIntegeri_v, 'glGetIntegeri_v');
  Load(glBeginTransformFeedback, 'glBeginTransformFeedback');
  Load(glEndTransformFeedback, 'glEndTransformFeedback');
  Load(glBindBufferRange, 'glBindBufferRange');
  Load(glBindBufferBase, 'glBindBufferBase');
  Load(glTransformFeedbackVaryings, 'glTransformFeedbackVaryings');
  Load(glGetTransformFeedbackVarying, 'glGetTransformFeedbackVarying');
  Load(glVertexAttribIPointer, 'glVertexAttribIPointer');
  Load(glGetVertexAttribIiv, 'glGetVertexAttribIiv');
  Load(glGetVertexAttribIuiv, 'glGetVertexAttribIuiv');
  Load(glVertexAttribI4i, 'glVertexAttribI4i');
  Load(glVertexAttribI4ui, 'glVertexAttribI4ui');
  Load(glVertexAttribI4iv, 'glVertexAttribI4iv');
  Load(glVertexAttribI4uiv, 'glVertexAttribI4uiv');
  Load(glGetUniformuiv, 'glGetUniformuiv');
  Load(glGetFragDataLocation, 'glGetFragDataLocation');
  Load(glUniform1ui, 'glUniform1ui');
  Load(glUniform2ui, 'glUniform2ui');
  Load(glUniform3ui, 'glUniform3ui');
  Load(glUniform4ui, 'glUniform4ui');
  Load(glUniform1uiv, 'glUniform1uiv');
  Load(glUniform2uiv, 'glUniform2uiv');
  Load(glUniform3uiv, 'glUniform3uiv');
  Load(glUniform4uiv, 'glUniform4uiv');
  Load(glClearBufferiv, 'glClearBufferiv');
  Load(glClearBufferuiv, 'glClearBufferuiv');
  Load(glClearBufferfv, 'glClearBufferfv');
  Load(glClearBufferfi, 'glClearBufferfi');
  Load(glGetStringi, 'glGetStringi');
  Load(glCopyBufferSubData, 'glCopyBufferSubData');
  Load(glGetUniformIndices, 'glGetUniformIndices');
  Load(glGetActiveUniformsiv, 'glGetActiveUniformsiv');
  Load(glGetUniformBlockIndex, 'glGetUniformBlockIndex');
  Load(glGetActiveUniformBlockiv, 'glGetActiveUniformBlockiv');
  Load(glGetActiveUniformBlockName, 'glGetActiveUniformBlockName');
  Load(glUniformBlockBinding, 'glUniformBlockBinding');
  Load(glDrawArraysInstanced, 'glDrawArraysInstanced');
  Load(glDrawElementsInstanced, 'glDrawElementsInstanced');
  Load(glFenceSync, 'glFenceSync');
  Load(glIsSync, 'glIsSync');
  Load(glDeleteSync, 'glDeleteSync');
  Load(glClientWaitSync, 'glClientWaitSync');
  Load(glWaitSync, 'glWaitSync');
  Load(glGetInteger64v, 'glGetInteger64v');
  Load(glGetSynciv, 'glGetSynciv');
  Load(glGetInteger64i_v, 'glGetInteger64i_v');
  Load(glGetBufferParameteri64v, 'glGetBufferParameteri64v');
  Load(glGenSamplers, 'glGenSamplers');
  Load(glDeleteSamplers, 'glDeleteSamplers');
  Load(glIsSampler, 'glIsSampler');
  Load(glBindSampler, 'glBindSampler');
  Load(glSamplerParameteri, 'glSamplerParameteri');
  Load(glSamplerParameteriv, 'glSamplerParameteriv');
  Load(glSamplerParameterf, 'glSamplerParameterf');
  Load(glSamplerParameterfv, 'glSamplerParameterfv');
  Load(glGetSamplerParameteriv, 'glGetSamplerParameteriv');
  Load(glGetSamplerParameterfv, 'glGetSamplerParameterfv');
  Load(glVertexAttribDivisor, 'glVertexAttribDivisor');
  Load(glBindTransformFeedback, 'glBindTransformFeedback');
  Load(glDeleteTransformFeedbacks, 'glDeleteTransformFeedbacks');
  Load(glGenTransformFeedbacks, 'glGenTransformFeedbacks');
  Load(glIsTransformFeedback, 'glIsTransformFeedback');
  Load(glPauseTransformFeedback, 'glPauseTransformFeedback');
  Load(glResumeTransformFeedback, 'glResumeTransformFeedback');
  Load(glGetProgramBinary, 'glGetProgramBinary');
  Load(glProgramBinary, 'glProgramBinary');
  Load(glProgramParameteri, 'glProgramParameteri');
  Load(glInvalidateFramebuffer, 'glInvalidateFramebuffer');
  Load(glInvalidateSubFramebuffer, 'glInvalidateSubFramebuffer');
  Load(glTexStorage2D, 'glTexStorage2D');
  Load(glTexStorage3D, 'glTexStorage3D');
  Load(glGetInternalformativ, 'glGetInternalformativ');
  {$endif}
  { OpenGL ES 3.1 }
  {$ifdef gles31}
  Load(glDispatchCompute, 'glDispatchCompute');
  Load(glDispatchComputeIndirect, 'glDispatchComputeIndirect');
  Load(glDrawArraysIndirect, 'glDrawArraysIndirect');
  Load(glDrawElementsIndirect, 'glDrawElementsIndirect');
  Load(glFramebufferParameteri, 'glFramebufferParameteri');
  Load(glGetFramebufferParameteriv, 'glGetFramebufferParameteriv');
  Load(glGetProgramInterfaceiv, 'glGetProgramInterfaceiv');
  Load(glGetProgramResourceIndex, 'glGetProgramResourceIndex');
  Load(glGetProgramResourceName, 'glGetProgramResourceName');
  Load(glGetProgramResourceiv, 'glGetProgramResourceiv');
  Load(glGetProgramResourceLocation, 'glGetProgramResourceLocation');
  Load(glUseProgramStages, 'glUseProgramStages');
  Load(glActiveShaderProgram, 'glActiveShaderProgram');
  Load(glCreateShaderProgramv, 'glCreateShaderProgramv');
  Load(glBindProgramPipeline, 'glBindProgramPipeline');
  Load(glDeleteProgramPipelines, 'glDeleteProgramPipelines');
  Load(glGenProgramPipelines, 'glGenProgramPipelines');
  Load(glIsProgramPipeline, 'glIsProgramPipeline');
  Load(glGetProgramPipelineiv, 'glGetProgramPipelineiv');
  Load(glProgramUniform1i, 'glProgramUniform1i');
  Load(glProgramUniform2i, 'glProgramUniform2i');
  Load(glProgramUniform3i, 'glProgramUniform3i');
  Load(glProgramUniform4i, 'glProgramUniform4i');
  Load(glProgramUniform1ui, 'glProgramUniform1ui');
  Load(glProgramUniform2ui, 'glProgramUniform2ui');
  Load(glProgramUniform3ui, 'glProgramUniform3ui');
  Load(glProgramUniform4ui, 'glProgramUniform4ui');
  Load(glProgramUniform1f, 'glProgramUniform1f');
  Load(glProgramUniform2f, 'glProgramUniform2f');
  Load(glProgramUniform3f, 'glProgramUniform3f');
  Load(glProgramUniform4f, 'glProgramUniform4f');
  Load(glProgramUniform1iv, 'glProgramUniform1iv');
  Load(glProgramUniform2iv, 'glProgramUniform2iv');
  Load(glProgramUniform3iv, 'glProgramUniform3iv');
  Load(glProgramUniform4iv, 'glProgramUniform4iv');
  Load(glProgramUniform1uiv, 'glProgramUniform1uiv');
  Load(glProgramUniform2uiv, 'glProgramUniform2uiv');
  Load(glProgramUniform3uiv, 'glProgramUniform3uiv');
  Load(glProgramUniform4uiv, 'glProgramUniform4uiv');
  Load(glProgramUniform1fv, 'glProgramUniform1fv');
  Load(glProgramUniform2fv, 'glProgramUniform2fv');
  Load(glProgramUniform3fv, 'glProgramUniform3fv');
  Load(glProgramUniform4fv, 'glProgramUniform4fv');
  Load(glProgramUniformMatrix2fv, 'glProgramUniformMatrix2fv');
  Load(glProgramUniformMatrix3fv, 'glProgramUniformMatrix3fv');
  Load(glProgramUniformMatrix4fv, 'glProgramUniformMatrix4fv');
  Load(glProgramUniformMatrix2x3fv, 'glProgramUniformMatrix2x3fv');
  Load(glProgramUniformMatrix3x2fv, 'glProgramUniformMatrix3x2fv');
  Load(glProgramUniformMatrix2x4fv, 'glProgramUniformMatrix2x4fv');
  Load(glProgramUniformMatrix4x2fv, 'glProgramUniformMatrix4x2fv');
  Load(glProgramUniformMatrix3x4fv, 'glProgramUniformMatrix3x4fv');
  Load(glProgramUniformMatrix4x3fv, 'glProgramUniformMatrix4x3fv');
  Load(glValidateProgramPipeline, 'glValidateProgramPipeline');
  Load(glGetProgramPipelineInfoLog, 'glGetProgramPipelineInfoLog');
  Load(glBindImageTexture, 'glBindImageTexture');
  Load(glGetBooleani_v, 'glGetBooleani_v');
  Load(glMemoryBarrier, 'glMemoryBarrier');
  Load(glMemoryBarrierByRegion, 'glMemoryBarrierByRegion');
  Load(glTexStorage2DMultisample, 'glTexStorage2DMultisample');
  Load(glGetMultisamplefv, 'glGetMultisamplefv');
  Load(glSampleMaski, 'glSampleMaski');
  Load(glGetTexLevelParameteriv, 'glGetTexLevelParameteriv');
  Load(glGetTexLevelParameterfv, 'glGetTexLevelParameterfv');
  Load(glBindVertexBuffer, 'glBindVertexBuffer');
  Load(glVertexAttribFormat, 'glVertexAttribFormat');
  Load(glVertexAttribIFormat, 'glVertexAttribIFormat');
  Load(glVertexAttribBinding, 'glVertexAttribBinding');
  Load(glVertexBindingDivisor, 'glVertexBindingDivisor');
  {$endif}
  { OpenGL ES 3.2 }
  {$ifdef gles32}
  Load(glBlendBarrier, 'glBlendBarrier');
  Load(glCopyImageSubData, 'glCopyImageSubData');
  Load(glDebugMessageControl, 'glDebugMessageControl');
  Load(glDebugMessageInsert, 'glDebugMessageInsert');
  Load(glDebugMessageCallback, 'glDebugMessageCallback');
  Load(glGetDebugMessageLog, 'glGetDebugMessageLog');
  Load(glPushDebugGroup, 'glPushDebugGroup');
  Load(glPopDebugGroup, 'glPopDebugGroup');
  Load(glObjectLabel, 'glObjectLabel');
  Load(glGetObjectLabel, 'glGetObjectLabel');
  Load(glObjectPtrLabel, 'glObjectPtrLabel');
  Load(glGetObjectPtrLabel, 'glGetObjectPtrLabel');
  Load(glGetPointerv, 'glGetPointerv');
  Load(glEnablei, 'glEnablei');
  Load(glDisablei, 'glDisablei');
  Load(glBlendEquationi, 'glBlendEquationi');
  Load(glBlendEquationSeparatei, 'glBlendEquationSeparatei');
  Load(glBlendFunci, 'glBlendFunci');
  Load(glBlendFuncSeparatei, 'glBlendFuncSeparatei');
  Load(glColorMaski, 'glColorMaski');
  Load(glIsEnabledi, 'glIsEnabledi');
  Load(glDrawElementsBaseVertex, 'glDrawElementsBaseVertex');
  Load(glDrawRangeElementsBaseVertex, 'glDrawRangeElementsBaseVertex');
  Load(glDrawElementsInstancedBaseVertex, 'glDrawElementsInstancedBaseVertex');
  Load(glFramebufferTexture, 'glFramebufferTexture');
  Load(glPrimitiveBoundingBox, 'glPrimitiveBoundingBox');
  Load(glGetGraphicsResetStatus, 'glGetGraphicsResetStatus');
  Load(glReadnPixels, 'glReadnPixels');
  Load(glGetnUniformfv, 'glGetnUniformfv');
  Load(glGetnUniformiv, 'glGetnUniformiv');
  Load(glGetnUniformuiv, 'glGetnUniformuiv');
  Load(glMinSampleShading, 'glMinSampleShading');
  Load(glPatchParameteri, 'glPatchParameteri');
  Load(glTexParameterIiv, 'glTexParameterIiv');
  Load(glTexParameterIuiv, 'glTexParameterIuiv');
  Load(glGetTexParameterIiv, 'glGetTexParameterIiv');
  Load(glGetTexParameterIuiv, 'glGetTexParameterIuiv');
  Load(glSamplerParameterIiv, 'glSamplerParameterIiv');
  Load(glSamplerParameterIuiv, 'glSamplerParameterIuiv');
  Load(glGetSamplerParameterIiv, 'glGetSamplerParameterIiv');
  Load(glGetSamplerParameterIuiv, 'glGetSamplerParameterIuiv');
  Load(glTexBuffer, 'glTexBuffer');
  Load(glTexBufferRange, 'glTexBufferRange');
  Load(glTexStorage3DMultisample, 'glTexStorage3DMultisample');
  {$endif}
  {$else}
  { OpenGL 1.0 }
  Load(glCullFace, 'glCullFace');
  Load(glFrontFace, 'glFrontFace');
  Load(glHint, 'glHint');
  Load(glLineWidth, 'glLineWidth');
  Load(glPointSize, 'glPointSize');
  Load(glPolygonMode, 'glPolygonMode');
  Load(glScissor, 'glScissor');
  Load(glTexParameterf, 'glTexParameterf');
  Load(glTexParameterfv, 'glTexParameterfv');
  Load(glTexParameteri, 'glTexParameteri');
  Load(glTexParameteriv, 'glTexParameteriv');
  Load(glTexImage1D, 'glTexImage1D');
  Load(glTexImage2D, 'glTexImage2D');
  Load(glDrawBuffer, 'glDrawBuffer');
  Load(glClear, 'glClear');
  Load(glClearColor, 'glClearColor');
  Load(glClearStencil, 'glClearStencil');
  Load(glClearDepth, 'glClearDepth');
  Load(glStencilMask, 'glStencilMask');
  Load(glColorMask, 'glColorMask');
  Load(glDepthMask, 'glDepthMask');
  Load(glDisable, 'glDisable');
  Load(glEnable, 'glEnable');
  Load(glFinish, 'glFinish');
  Load(glFlush, 'glFlush');
  Load(glBlendFunc, 'glBlendFunc');
  Load(glLogicOp, 'glLogicOp');
  Load(glStencilFunc, 'glStencilFunc');
  Load(glStencilOp, 'glStencilOp');
  Load(glDepthFunc, 'glDepthFunc');
  Load(glPixelStoref, 'glPixelStoref');
  Load(glPixelStorei, 'glPixelStorei');
  Load(glReadBuffer, 'glReadBuffer');
  Load(glReadPixels, 'glReadPixels');
  Load(glGetBooleanv, 'glGetBooleanv');
  Load(glGetDoublev, 'glGetDoublev');
  Load(glGetError, 'glGetError');
  Load(glGetFloatv, 'glGetFloatv');
  Load(glGetIntegerv, 'glGetIntegerv');
  Load(glGetString, 'glGetString');
  Load(glGetTexImage, 'glGetTexImage');
  Load(glGetTexParameterfv, 'glGetTexParameterfv');
  Load(glGetTexParameteriv, 'glGetTexParameteriv');
  Load(glGetTexLevelParameterfv, 'glGetTexLevelParameterfv');
  Load(glGetTexLevelParameteriv, 'glGetTexLevelParameteriv');
  Load(glIsEnabled, 'glIsEnabled');
  Load(glDepthRange, 'glDepthRange');
  Load(glViewport, 'glViewport');
  { OpenGL 1.0 compatibility profile }
  {$ifdef glcompat}
  Load(glNewList, 'glNewList');
  Load(glEndList, 'glEndList');
  Load(glCallList, 'glCallList');
  Load(glCallLists, 'glCallLists');
  Load(glDeleteLists, 'glDeleteLists');
  Load(glGenLists, 'glGenLists');
  Load(glListBase, 'glListBase');
  Load(glBegin, 'glBegin');
  Load(glBitmap, 'glBitmap');
  Load(glColor3b, 'glColor3b');
  Load(glColor3bv, 'glColor3bv');
  Load(glColor3d, 'glColor3d');
  Load(glColor3dv, 'glColor3dv');
  Load(glColor3f, 'glColor3f');
  Load(glColor3fv, 'glColor3fv');
  Load(glColor3i, 'glColor3i');
  Load(glColor3iv, 'glColor3iv');
  Load(glColor3s, 'glColor3s');
  Load(glColor3sv, 'glColor3sv');
  Load(glColor3ub, 'glColor3ub');
  Load(glColor3ubv, 'glColor3ubv');
  Load(glColor3ui, 'glColor3ui');
  Load(glColor3uiv, 'glColor3uiv');
  Load(glColor3us, 'glColor3us');
  Load(glColor3usv, 'glColor3usv');
  Load(glColor4b, 'glColor4b');
  Load(glColor4bv, 'glColor4bv');
  Load(glColor4d, 'glColor4d');
  Load(glColor4dv, 'glColor4dv');
  Load(glColor4f, 'glColor4f');
  Load(glColor4fv, 'glColor4fv');
  Load(glColor4i, 'glColor4i');
  Load(glColor4iv, 'glColor4iv');
  Load(glColor4s, 'glColor4s');
  Load(glColor4sv, 'glColor4sv');
  Load(glColor4ub, 'glColor4ub');
  Load(glColor4ubv, 'glColor4ubv');
  Load(glColor4ui, 'glColor4ui');
  Load(glColor4uiv, 'glColor4uiv');
  Load(glColor4us, 'glColor4us');
  Load(glColor4usv, 'glColor4usv');
  Load(glEdgeFlag, 'glEdgeFlag');
  Load(glEdgeFlagv, 'glEdgeFlagv');
  Load(glEnd, 'glEnd');
  Load(glIndexd, 'glIndexd');
  Load(glIndexdv, 'glIndexdv');
  Load(glIndexf, 'glIndexf');
  Load(glIndexfv, 'glIndexfv');
  Load(glIndexi, 'glIndexi');
  Load(glIndexiv, 'glIndexiv');
  Load(glIndexs, 'glIndexs');
  Load(glIndexsv, 'glIndexsv');
  Load(glNormal3b, 'glNormal3b');
  Load(glNormal3bv, 'glNormal3bv');
  Load(glNormal3d, 'glNormal3d');
  Load(glNormal3dv, 'glNormal3dv');
  Load(glNormal3f, 'glNormal3f');
  Load(glNormal3fv, 'glNormal3fv');
  Load(glNormal3i, 'glNormal3i');
  Load(glNormal3iv, 'glNormal3iv');
  Load(glNormal3s, 'glNormal3s');
  Load(glNormal3sv, 'glNormal3sv');
  Load(glRasterPos2d, 'glRasterPos2d');
  Load(glRasterPos2dv, 'glRasterPos2dv');
  Load(glRasterPos2f, 'glRasterPos2f');
  Load(glRasterPos2fv, 'glRasterPos2fv');
  Load(glRasterPos2i, 'glRasterPos2i');
  Load(glRasterPos2iv, 'glRasterPos2iv');
  Load(glRasterPos2s, 'glRasterPos2s');
  Load(glRasterPos2sv, 'glRasterPos2sv');
  Load(glRasterPos3d, 'glRasterPos3d');
  Load(glRasterPos3dv, 'glRasterPos3dv');
  Load(glRasterPos3f, 'glRasterPos3f');
  Load(glRasterPos3fv, 'glRasterPos3fv');
  Load(glRasterPos3i, 'glRasterPos3i');
  Load(glRasterPos3iv, 'glRasterPos3iv');
  Load(glRasterPos3s, 'glRasterPos3s');
  Load(glRasterPos3sv, 'glRasterPos3sv');
  Load(glRasterPos4d, 'glRasterPos4d');
  Load(glRasterPos4dv, 'glRasterPos4dv');
  Load(glRasterPos4f, 'glRasterPos4f');
  Load(glRasterPos4fv, 'glRasterPos4fv');
  Load(glRasterPos4i, 'glRasterPos4i');
  Load(glRasterPos4iv, 'glRasterPos4iv');
  Load(glRasterPos4s, 'glRasterPos4s');
  Load(glRasterPos4sv, 'glRasterPos4sv');
  Load(glRectd, 'glRectd');
  Load(glRectdv, 'glRectdv');
  Load(glRectf, 'glRectf');
  Load(glRectfv, 'glRectfv');
  Load(glRecti, 'glRecti');
  Load(glRectiv, 'glRectiv');
  Load(glRects, 'glRects');
  Load(glRectsv, 'glRectsv');
  Load(glTexCoord1d, 'glTexCoord1d');
  Load(glTexCoord1dv, 'glTexCoord1dv');
  Load(glTexCoord1f, 'glTexCoord1f');
  Load(glTexCoord1fv, 'glTexCoord1fv');
  Load(glTexCoord1i, 'glTexCoord1i');
  Load(glTexCoord1iv, 'glTexCoord1iv');
  Load(glTexCoord1s, 'glTexCoord1s');
  Load(glTexCoord1sv, 'glTexCoord1sv');
  Load(glTexCoord2d, 'glTexCoord2d');
  Load(glTexCoord2dv, 'glTexCoord2dv');
  Load(glTexCoord2f, 'glTexCoord2f');
  Load(glTexCoord2fv, 'glTexCoord2fv');
  Load(glTexCoord2i, 'glTexCoord2i');
  Load(glTexCoord2iv, 'glTexCoord2iv');
  Load(glTexCoord2s, 'glTexCoord2s');
  Load(glTexCoord2sv, 'glTexCoord2sv');
  Load(glTexCoord3d, 'glTexCoord3d');
  Load(glTexCoord3dv, 'glTexCoord3dv');
  Load(glTexCoord3f, 'glTexCoord3f');
  Load(glTexCoord3fv, 'glTexCoord3fv');
  Load(glTexCoord3i, 'glTexCoord3i');
  Load(glTexCoord3iv, 'glTexCoord3iv');
  Load(glTexCoord3s, 'glTexCoord3s');
  Load(glTexCoord3sv, 'glTexCoord3sv');
  Load(glTexCoord4d, 'glTexCoord4d');
  Load(glTexCoord4dv, 'glTexCoord4dv');
  Load(glTexCoord4f, 'glTexCoord4f');
  Load(glTexCoord4fv, 'glTexCoord4fv');
  Load(glTexCoord4i, 'glTexCoord4i');
  Load(glTexCoord4iv, 'glTexCoord4iv');
  Load(glTexCoord4s, 'glTexCoord4s');
  Load(glTexCoord4sv, 'glTexCoord4sv');
  Load(glVertex2d, 'glVertex2d');
  Load(glVertex2dv, 'glVertex2dv');
  Load(glVertex2f, 'glVertex2f');
  Load(glVertex2fv, 'glVertex2fv');
  Load(glVertex2i, 'glVertex2i');
  Load(glVertex2iv, 'glVertex2iv');
  Load(glVertex2s, 'glVertex2s');
  Load(glVertex2sv, 'glVertex2sv');
  Load(glVertex3d, 'glVertex3d');
  Load(glVertex3dv, 'glVertex3dv');
  Load(glVertex3f, 'glVertex3f');
  Load(glVertex3fv, 'glVertex3fv');
  Load(glVertex3i, 'glVertex3i');
  Load(glVertex3iv, 'glVertex3iv');
  Load(glVertex3s, 'glVertex3s');
  Load(glVertex3sv, 'glVertex3sv');
  Load(glVertex4d, 'glVertex4d');
  Load(glVertex4dv, 'glVertex4dv');
  Load(glVertex4f, 'glVertex4f');
  Load(glVertex4fv, 'glVertex4fv');
  Load(glVertex4i, 'glVertex4i');
  Load(glVertex4iv, 'glVertex4iv');
  Load(glVertex4s, 'glVertex4s');
  Load(glVertex4sv, 'glVertex4sv');
  Load(glClipPlane, 'glClipPlane');
  Load(glColorMaterial, 'glColorMaterial');
  Load(glFogf, 'glFogf');
  Load(glFogfv, 'glFogfv');
  Load(glFogi, 'glFogi');
  Load(glFogiv, 'glFogiv');
  Load(glLightf, 'glLightf');
  Load(glLightfv, 'glLightfv');
  Load(glLighti, 'glLighti');
  Load(glLightiv, 'glLightiv');
  Load(glLightModelf, 'glLightModelf');
  Load(glLightModelfv, 'glLightModelfv');
  Load(glLightModeli, 'glLightModeli');
  Load(glLightModeliv, 'glLightModeliv');
  Load(glLineStipple, 'glLineStipple');
  Load(glMaterialf, 'glMaterialf');
  Load(glMaterialfv, 'glMaterialfv');
  Load(glMateriali, 'glMateriali');
  Load(glMaterialiv, 'glMaterialiv');
  Load(glPolygonStipple, 'glPolygonStipple');
  Load(glShadeModel, 'glShadeModel');
  Load(glTexEnvf, 'glTexEnvf');
  Load(glTexEnvfv, 'glTexEnvfv');
  Load(glTexEnvi, 'glTexEnvi');
  Load(glTexEnviv, 'glTexEnviv');
  Load(glTexGend, 'glTexGend');
  Load(glTexGendv, 'glTexGendv');
  Load(glTexGenf, 'glTexGenf');
  Load(glTexGenfv, 'glTexGenfv');
  Load(glTexGeni, 'glTexGeni');
  Load(glTexGeniv, 'glTexGeniv');
  Load(glFeedbackBuffer, 'glFeedbackBuffer');
  Load(glSelectBuffer, 'glSelectBuffer');
  Load(glRenderMode, 'glRenderMode');
  Load(glInitNames, 'glInitNames');
  Load(glLoadName, 'glLoadName');
  Load(glPassThrough, 'glPassThrough');
  Load(glPopName, 'glPopName');
  Load(glPushName, 'glPushName');
  Load(glClearAccum, 'glClearAccum');
  Load(glClearIndex, 'glClearIndex');
  Load(glIndexMask, 'glIndexMask');
  Load(glAccum, 'glAccum');
  Load(glPopAttrib, 'glPopAttrib');
  Load(glPushAttrib, 'glPushAttrib');
  Load(glMap1d, 'glMap1d');
  Load(glMap1f, 'glMap1f');
  Load(glMap2d, 'glMap2d');
  Load(glMap2f, 'glMap2f');
  Load(glMapGrid1d, 'glMapGrid1d');
  Load(glMapGrid1f, 'glMapGrid1f');
  Load(glMapGrid2d, 'glMapGrid2d');
  Load(glMapGrid2f, 'glMapGrid2f');
  Load(glEvalCoord1d, 'glEvalCoord1d');
  Load(glEvalCoord1dv, 'glEvalCoord1dv');
  Load(glEvalCoord1f, 'glEvalCoord1f');
  Load(glEvalCoord1fv, 'glEvalCoord1fv');
  Load(glEvalCoord2d, 'glEvalCoord2d');
  Load(glEvalCoord2dv, 'glEvalCoord2dv');
  Load(glEvalCoord2f, 'glEvalCoord2f');
  Load(glEvalCoord2fv, 'glEvalCoord2fv');
  Load(glEvalMesh1, 'glEvalMesh1');
  Load(glEvalPoint1, 'glEvalPoint1');
  Load(glEvalMesh2, 'glEvalMesh2');
  Load(glEvalPoint2, 'glEvalPoint2');
  Load(glAlphaFunc, 'glAlphaFunc');
  Load(glPixelZoom, 'glPixelZoom');
  Load(glPixelTransferf, 'glPixelTransferf');
  Load(glPixelTransferi, 'glPixelTransferi');
  Load(glPixelMapfv, 'glPixelMapfv');
  Load(glPixelMapuiv, 'glPixelMapuiv');
  Load(glPixelMapusv, 'glPixelMapusv');
  Load(glCopyPixels, 'glCopyPixels');
  Load(glDrawPixels, 'glDrawPixels');
  Load(glGetClipPlane, 'glGetClipPlane');
  Load(glGetLightfv, 'glGetLightfv');
  Load(glGetLightiv, 'glGetLightiv');
  Load(glGetMapdv, 'glGetMapdv');
  Load(glGetMapfv, 'glGetMapfv');
  Load(glGetMapiv, 'glGetMapiv');
  Load(glGetMaterialfv, 'glGetMaterialfv');
  Load(glGetMaterialiv, 'glGetMaterialiv');
  Load(glGetPixelMapfv, 'glGetPixelMapfv');
  Load(glGetPixelMapuiv, 'glGetPixelMapuiv');
  Load(glGetPixelMapusv, 'glGetPixelMapusv');
  Load(glGetPolygonStipple, 'glGetPolygonStipple');
  Load(glGetTexEnvfv, 'glGetTexEnvfv');
  Load(glGetTexEnviv, 'glGetTexEnviv');
  Load(glGetTexGendv, 'glGetTexGendv');
  Load(glGetTexGenfv, 'glGetTexGenfv');
  Load(glGetTexGeniv, 'glGetTexGeniv');
  Load(glIsList, 'glIsList');
  Load(glFrustum, 'glFrustum');
  Load(glLoadIdentity, 'glLoadIdentity');
  Load(glLoadMatrixf, 'glLoadMatrixf');
  Load(glLoadMatrixd, 'glLoadMatrixd');
  Load(glMatrixMode, 'glMatrixMode');
  Load(glMultMatrixf, 'glMultMatrixf');
  Load(glMultMatrixd, 'glMultMatrixd');
  Load(glOrtho, 'glOrtho');
  Load(glPopMatrix, 'glPopMatrix');
  Load(glPushMatrix, 'glPushMatrix');
  Load(glRotated, 'glRotated');
  Load(glRotatef, 'glRotatef');
  Load(glScaled, 'glScaled');
  Load(glScalef, 'glScalef');
  Load(glTranslated, 'glTranslated');
  Load(glTranslatef, 'glTranslatef');
  {$endif}
  { OpenGL 1.1 }
  Load(glDrawArrays, 'glDrawArrays');
  Load(glDrawElements, 'glDrawElements');
  Load(glPolygonOffset, 'glPolygonOffset');
  Load(glCopyTexImage1D, 'glCopyTexImage1D');
  Load(glCopyTexImage2D, 'glCopyTexImage2D');
  Load(glCopyTexSubImage1D, 'glCopyTexSubImage1D');
  Load(glCopyTexSubImage2D, 'glCopyTexSubImage2D');
  Load(glTexSubImage1D, 'glTexSubImage1D');
  Load(glTexSubImage2D, 'glTexSubImage2D');
  Load(glBindTexture, 'glBindTexture');
  Load(glDeleteTextures, 'glDeleteTextures');
  Load(glGenTextures, 'glGenTextures');
  Load(glIsTexture, 'glIsTexture');
  { OpenGL 1.1 compatibility profile }
  {$ifdef glcompat}
  Load(glArrayElement, 'glArrayElement');
  Load(glColorPointer, 'glColorPointer');
  Load(glDisableClientState, 'glDisableClientState');
  Load(glEdgeFlagPointer, 'glEdgeFlagPointer');
  Load(glEnableClientState, 'glEnableClientState');
  Load(glIndexPointer, 'glIndexPointer');
  Load(glGetPointerv, 'glGetPointerv');
  Load(glInterleavedArrays, 'glInterleavedArrays');
  Load(glNormalPointer, 'glNormalPointer');
  Load(glTexCoordPointer, 'glTexCoordPointer');
  Load(glVertexPointer, 'glVertexPointer');
  Load(glAreTexturesResident, 'glAreTexturesResident');
  Load(glPrioritizeTextures, 'glPrioritizeTextures');
  Load(glIndexub, 'glIndexub');
  Load(glIndexubv, 'glIndexubv');
  Load(glPopClientAttrib, 'glPopClientAttrib');
  Load(glPushClientAttrib, 'glPushClientAttrib');
  {$endif}
  { OpenGL 1.2 }
  Load(glDrawRangeElements, 'glDrawRangeElements');
  Load(glTexImage3D, 'glTexImage3D');
  Load(glTexSubImage3D, 'glTexSubImage3D');
  Load(glCopyTexSubImage3D, 'glCopyTexSubImage3D');
  { OpenGL 1.3 }
  Load(glActiveTexture, 'glActiveTexture');
  Load(glSampleCoverage, 'glSampleCoverage');
  Load(glCompressedTexImage3D, 'glCompressedTexImage3D');
  Load(glCompressedTexImage2D, 'glCompressedTexImage2D');
  Load(glCompressedTexImage1D, 'glCompressedTexImage1D');
  Load(glCompressedTexSubImage3D, 'glCompressedTexSubImage3D');
  Load(glCompressedTexSubImage2D, 'glCompressedTexSubImage2D');
  Load(glCompressedTexSubImage1D, 'glCompressedTexSubImage1D');
  Load(glGetCompressedTexImage, 'glGetCompressedTexImage');
  { OpenGL 1.3 compatibility profile }
  {$ifdef glcompat}
  Load(glClientActiveTexture, 'glClientActiveTexture');
  Load(glMultiTexCoord1d, 'glMultiTexCoord1d');
  Load(glMultiTexCoord1dv, 'glMultiTexCoord1dv');
  Load(glMultiTexCoord1f, 'glMultiTexCoord1f');
  Load(glMultiTexCoord1fv, 'glMultiTexCoord1fv');
  Load(glMultiTexCoord1i, 'glMultiTexCoord1i');
  Load(glMultiTexCoord1iv, 'glMultiTexCoord1iv');
  Load(glMultiTexCoord1s, 'glMultiTexCoord1s');
  Load(glMultiTexCoord1sv, 'glMultiTexCoord1sv');
  Load(glMultiTexCoord2d, 'glMultiTexCoord2d');
  Load(glMultiTexCoord2dv, 'glMultiTexCoord2dv');
  Load(glMultiTexCoord2f, 'glMultiTexCoord2f');
  Load(glMultiTexCoord2fv, 'glMultiTexCoord2fv');
  Load(glMultiTexCoord2i, 'glMultiTexCoord2i');
  Load(glMultiTexCoord2iv, 'glMultiTexCoord2iv');
  Load(glMultiTexCoord2s, 'glMultiTexCoord2s');
  Load(glMultiTexCoord2sv, 'glMultiTexCoord2sv');
  Load(glMultiTexCoord3d, 'glMultiTexCoord3d');
  Load(glMultiTexCoord3dv, 'glMultiTexCoord3dv');
  Load(glMultiTexCoord3f, 'glMultiTexCoord3f');
  Load(glMultiTexCoord3fv, 'glMultiTexCoord3fv');
  Load(glMultiTexCoord3i, 'glMultiTexCoord3i');
  Load(glMultiTexCoord3iv, 'glMultiTexCoord3iv');
  Load(glMultiTexCoord3s, 'glMultiTexCoord3s');
  Load(glMultiTexCoord3sv, 'glMultiTexCoord3sv');
  Load(glMultiTexCoord4d, 'glMultiTexCoord4d');
  Load(glMultiTexCoord4dv, 'glMultiTexCoord4dv');
  Load(glMultiTexCoord4f, 'glMultiTexCoord4f');
  Load(glMultiTexCoord4fv, 'glMultiTexCoord4fv');
  Load(glMultiTexCoord4i, 'glMultiTexCoord4i');
  Load(glMultiTexCoord4iv, 'glMultiTexCoord4iv');
  Load(glMultiTexCoord4s, 'glMultiTexCoord4s');
  Load(glMultiTexCoord4sv, 'glMultiTexCoord4sv');
  Load(glLoadTransposeMatrixf, 'glLoadTransposeMatrixf');
  Load(glLoadTransposeMatrixd, 'glLoadTransposeMatrixd');
  Load(glMultTransposeMatrixf, 'glMultTransposeMatrixf');
  Load(glMultTransposeMatrixd, 'glMultTransposeMatrixd');
  {$endif}
  { OpenGL 1.4 }
  Load(glBlendFuncSeparate, 'glBlendFuncSeparate');
  Load(glMultiDrawArrays, 'glMultiDrawArrays');
  Load(glMultiDrawElements, 'glMultiDrawElements');
  Load(glPointParameterf, 'glPointParameterf');
  Load(glPointParameterfv, 'glPointParameterfv');
  Load(glPointParameteri, 'glPointParameteri');
  Load(glPointParameteriv, 'glPointParameteriv');
  Load(glBlendColor, 'glBlendColor');
  Load(glBlendEquation, 'glBlendEquation');
  { OpenGL 1.4 compatibility profile }
  {$ifdef glcompat}
  Load(glFogCoordf, 'glFogCoordf');
  Load(glFogCoordfv, 'glFogCoordfv');
  Load(glFogCoordd, 'glFogCoordd');
  Load(glFogCoorddv, 'glFogCoorddv');
  Load(glFogCoordPointer, 'glFogCoordPointer');
  Load(glSecondaryColor3b, 'glSecondaryColor3b');
  Load(glSecondaryColor3bv, 'glSecondaryColor3bv');
  Load(glSecondaryColor3d, 'glSecondaryColor3d');
  Load(glSecondaryColor3dv, 'glSecondaryColor3dv');
  Load(glSecondaryColor3f, 'glSecondaryColor3f');
  Load(glSecondaryColor3fv, 'glSecondaryColor3fv');
  Load(glSecondaryColor3i, 'glSecondaryColor3i');
  Load(glSecondaryColor3iv, 'glSecondaryColor3iv');
  Load(glSecondaryColor3s, 'glSecondaryColor3s');
  Load(glSecondaryColor3sv, 'glSecondaryColor3sv');
  Load(glSecondaryColor3ub, 'glSecondaryColor3ub');
  Load(glSecondaryColor3ubv, 'glSecondaryColor3ubv');
  Load(glSecondaryColor3ui, 'glSecondaryColor3ui');
  Load(glSecondaryColor3uiv, 'glSecondaryColor3uiv');
  Load(glSecondaryColor3us, 'glSecondaryColor3us');
  Load(glSecondaryColor3usv, 'glSecondaryColor3usv');
  Load(glSecondaryColorPointer, 'glSecondaryColorPointer');
  Load(glWindowPos2d, 'glWindowPos2d');
  Load(glWindowPos2dv, 'glWindowPos2dv');
  Load(glWindowPos2f, 'glWindowPos2f');
  Load(glWindowPos2fv, 'glWindowPos2fv');
  Load(glWindowPos2i, 'glWindowPos2i');
  Load(glWindowPos2iv, 'glWindowPos2iv');
  Load(glWindowPos2s, 'glWindowPos2s');
  Load(glWindowPos2sv, 'glWindowPos2sv');
  Load(glWindowPos3d, 'glWindowPos3d');
  Load(glWindowPos3dv, 'glWindowPos3dv');
  Load(glWindowPos3f, 'glWindowPos3f');
  Load(glWindowPos3fv, 'glWindowPos3fv');
  Load(glWindowPos3i, 'glWindowPos3i');
  Load(glWindowPos3iv, 'glWindowPos3iv');
  Load(glWindowPos3s, 'glWindowPos3s');
  Load(glWindowPos3sv, 'glWindowPos3sv');
  {$endif}
  { OpenGL 1.5 }
  Load(glGenQueries, 'glGenQueries');
  Load(glDeleteQueries, 'glDeleteQueries');
  Load(glIsQuery, 'glIsQuery');
  Load(glBeginQuery, 'glBeginQuery');
  Load(glEndQuery, 'glEndQuery');
  Load(glGetQueryiv, 'glGetQueryiv');
  Load(glGetQueryObjectiv, 'glGetQueryObjectiv');
  Load(glGetQueryObjectuiv, 'glGetQueryObjectuiv');
  Load(glBindBuffer, 'glBindBuffer');
  Load(glDeleteBuffers, 'glDeleteBuffers');
  Load(glGenBuffers, 'glGenBuffers');
  Load(glIsBuffer, 'glIsBuffer');
  Load(glBufferData, 'glBufferData');
  Load(glBufferSubData, 'glBufferSubData');
  Load(glGetBufferSubData, 'glGetBufferSubData');
  Load(glMapBuffer, 'glMapBuffer');
  Load(glUnmapBuffer, 'glUnmapBuffer');
  Load(glGetBufferParameteriv, 'glGetBufferParameteriv');
  Load(glGetBufferPointerv, 'glGetBufferPointerv');
  { OpenGL 2.0 }
  Load(glBlendEquationSeparate, 'glBlendEquationSeparate');
  Load(glDrawBuffers, 'glDrawBuffers');
  Load(glStencilOpSeparate, 'glStencilOpSeparate');
  Load(glStencilFuncSeparate, 'glStencilFuncSeparate');
  Load(glStencilMaskSeparate, 'glStencilMaskSeparate');
  Load(glAttachShader, 'glAttachShader');
  Load(glBindAttribLocation, 'glBindAttribLocation');
  Load(glCompileShader, 'glCompileShader');
  Load(glCreateProgram, 'glCreateProgram');
  Load(glCreateShader, 'glCreateShader');
  Load(glDeleteProgram, 'glDeleteProgram');
  Load(glDeleteShader, 'glDeleteShader');
  Load(glDetachShader, 'glDetachShader');
  Load(glDisableVertexAttribArray, 'glDisableVertexAttribArray');
  Load(glEnableVertexAttribArray, 'glEnableVertexAttribArray');
  Load(glGetActiveAttrib, 'glGetActiveAttrib');
  Load(glGetActiveUniform, 'glGetActiveUniform');
  Load(glGetAttachedShaders, 'glGetAttachedShaders');
  Load(glGetAttribLocation, 'glGetAttribLocation');
  Load(glGetProgramiv, 'glGetProgramiv');
  Load(glGetProgramInfoLog, 'glGetProgramInfoLog');
  Load(glGetShaderiv, 'glGetShaderiv');
  Load(glGetShaderInfoLog, 'glGetShaderInfoLog');
  Load(glGetShaderSource, 'glGetShaderSource');
  Load(glGetUniformLocation, 'glGetUniformLocation');
  Load(glGetUniformfv, 'glGetUniformfv');
  Load(glGetUniformiv, 'glGetUniformiv');
  Load(glGetVertexAttribdv, 'glGetVertexAttribdv');
  Load(glGetVertexAttribfv, 'glGetVertexAttribfv');
  Load(glGetVertexAttribiv, 'glGetVertexAttribiv');
  Load(glGetVertexAttribPointerv, 'glGetVertexAttribPointerv');
  Load(glIsProgram, 'glIsProgram');
  Load(glIsShader, 'glIsShader');
  Load(glLinkProgram, 'glLinkProgram');
  Load(glShaderSource, 'glShaderSource');
  Load(glUseProgram, 'glUseProgram');
  Load(glUniform1f, 'glUniform1f');
  Load(glUniform2f, 'glUniform2f');
  Load(glUniform3f, 'glUniform3f');
  Load(glUniform4f, 'glUniform4f');
  Load(glUniform1i, 'glUniform1i');
  Load(glUniform2i, 'glUniform2i');
  Load(glUniform3i, 'glUniform3i');
  Load(glUniform4i, 'glUniform4i');
  Load(glUniform1fv, 'glUniform1fv');
  Load(glUniform2fv, 'glUniform2fv');
  Load(glUniform3fv, 'glUniform3fv');
  Load(glUniform4fv, 'glUniform4fv');
  Load(glUniform1iv, 'glUniform1iv');
  Load(glUniform2iv, 'glUniform2iv');
  Load(glUniform3iv, 'glUniform3iv');
  Load(glUniform4iv, 'glUniform4iv');
  Load(glUniformMatrix2fv, 'glUniformMatrix2fv');
  Load(glUniformMatrix3fv, 'glUniformMatrix3fv');
  Load(glUniformMatrix4fv, 'glUniformMatrix4fv');
  Load(glValidateProgram, 'glValidateProgram');
  Load(glVertexAttrib1d, 'glVertexAttrib1d');
  Load(glVertexAttrib1dv, 'glVertexAttrib1dv');
  Load(glVertexAttrib1f, 'glVertexAttrib1f');
  Load(glVertexAttrib1fv, 'glVertexAttrib1fv');
  Load(glVertexAttrib1s, 'glVertexAttrib1s');
  Load(glVertexAttrib1sv, 'glVertexAttrib1sv');
  Load(glVertexAttrib2d, 'glVertexAttrib2d');
  Load(glVertexAttrib2dv, 'glVertexAttrib2dv');
  Load(glVertexAttrib2f, 'glVertexAttrib2f');
  Load(glVertexAttrib2fv, 'glVertexAttrib2fv');
  Load(glVertexAttrib2s, 'glVertexAttrib2s');
  Load(glVertexAttrib2sv, 'glVertexAttrib2sv');
  Load(glVertexAttrib3d, 'glVertexAttrib3d');
  Load(glVertexAttrib3dv, 'glVertexAttrib3dv');
  Load(glVertexAttrib3f, 'glVertexAttrib3f');
  Load(glVertexAttrib3fv, 'glVertexAttrib3fv');
  Load(glVertexAttrib3s, 'glVertexAttrib3s');
  Load(glVertexAttrib3sv, 'glVertexAttrib3sv');
  Load(glVertexAttrib4Nbv, 'glVertexAttrib4Nbv');
  Load(glVertexAttrib4Niv, 'glVertexAttrib4Niv');
  Load(glVertexAttrib4Nsv, 'glVertexAttrib4Nsv');
  Load(glVertexAttrib4Nub, 'glVertexAttrib4Nub');
  Load(glVertexAttrib4Nubv, 'glVertexAttrib4Nubv');
  Load(glVertexAttrib4Nuiv, 'glVertexAttrib4Nuiv');
  Load(glVertexAttrib4Nusv, 'glVertexAttrib4Nusv');
  Load(glVertexAttrib4bv, 'glVertexAttrib4bv');
  Load(glVertexAttrib4d, 'glVertexAttrib4d');
  Load(glVertexAttrib4dv, 'glVertexAttrib4dv');
  Load(glVertexAttrib4f, 'glVertexAttrib4f');
  Load(glVertexAttrib4fv, 'glVertexAttrib4fv');
  Load(glVertexAttrib4iv, 'glVertexAttrib4iv');
  Load(glVertexAttrib4s, 'glVertexAttrib4s');
  Load(glVertexAttrib4sv, 'glVertexAttrib4sv');
  Load(glVertexAttrib4ubv, 'glVertexAttrib4ubv');
  Load(glVertexAttrib4uiv, 'glVertexAttrib4uiv');
  Load(glVertexAttrib4usv, 'glVertexAttrib4usv');
  Load(glVertexAttribPointer, 'glVertexAttribPointer');
  { OpenGL 2.1 }
  Load(glUniformMatrix2x3fv, 'glUniformMatrix2x3fv');
  Load(glUniformMatrix3x2fv, 'glUniformMatrix3x2fv');
  Load(glUniformMatrix2x4fv, 'glUniformMatrix2x4fv');
  Load(glUniformMatrix4x2fv, 'glUniformMatrix4x2fv');
  Load(glUniformMatrix3x4fv, 'glUniformMatrix3x4fv');
  Load(glUniformMatrix4x3fv, 'glUniformMatrix4x3fv');
  { OpenGL 3.0 }
  {$ifdef gl30}
  Load(glColorMaski, 'glColorMaski');
  Load(glGetBooleani_v, 'glGetBooleani_v');
  Load(glGetIntegeri_v, 'glGetIntegeri_v');
  Load(glEnablei, 'glEnablei');
  Load(glDisablei, 'glDisablei');
  Load(glIsEnabledi, 'glIsEnabledi');
  Load(glBeginTransformFeedback, 'glBeginTransformFeedback');
  Load(glEndTransformFeedback, 'glEndTransformFeedback');
  Load(glBindBufferRange, 'glBindBufferRange');
  Load(glBindBufferBase, 'glBindBufferBase');
  Load(glTransformFeedbackVaryings, 'glTransformFeedbackVaryings');
  Load(glGetTransformFeedbackVarying, 'glGetTransformFeedbackVarying');
  Load(glClampColor, 'glClampColor');
  Load(glBeginConditionalRender, 'glBeginConditionalRender');
  Load(glEndConditionalRender, 'glEndConditionalRender');
  Load(glVertexAttribIPointer, 'glVertexAttribIPointer');
  Load(glGetVertexAttribIiv, 'glGetVertexAttribIiv');
  Load(glGetVertexAttribIuiv, 'glGetVertexAttribIuiv');
  Load(glVertexAttribI1i, 'glVertexAttribI1i');
  Load(glVertexAttribI2i, 'glVertexAttribI2i');
  Load(glVertexAttribI3i, 'glVertexAttribI3i');
  Load(glVertexAttribI4i, 'glVertexAttribI4i');
  Load(glVertexAttribI1ui, 'glVertexAttribI1ui');
  Load(glVertexAttribI2ui, 'glVertexAttribI2ui');
  Load(glVertexAttribI3ui, 'glVertexAttribI3ui');
  Load(glVertexAttribI4ui, 'glVertexAttribI4ui');
  Load(glVertexAttribI1iv, 'glVertexAttribI1iv');
  Load(glVertexAttribI2iv, 'glVertexAttribI2iv');
  Load(glVertexAttribI3iv, 'glVertexAttribI3iv');
  Load(glVertexAttribI4iv, 'glVertexAttribI4iv');
  Load(glVertexAttribI1uiv, 'glVertexAttribI1uiv');
  Load(glVertexAttribI2uiv, 'glVertexAttribI2uiv');
  Load(glVertexAttribI3uiv, 'glVertexAttribI3uiv');
  Load(glVertexAttribI4uiv, 'glVertexAttribI4uiv');
  Load(glVertexAttribI4bv, 'glVertexAttribI4bv');
  Load(glVertexAttribI4sv, 'glVertexAttribI4sv');
  Load(glVertexAttribI4ubv, 'glVertexAttribI4ubv');
  Load(glVertexAttribI4usv, 'glVertexAttribI4usv');
  Load(glGetUniformuiv, 'glGetUniformuiv');
  Load(glBindFragDataLocation, 'glBindFragDataLocation');
  Load(glGetFragDataLocation, 'glGetFragDataLocation');
  Load(glUniform1ui, 'glUniform1ui');
  Load(glUniform2ui, 'glUniform2ui');
  Load(glUniform3ui, 'glUniform3ui');
  Load(glUniform4ui, 'glUniform4ui');
  Load(glUniform1uiv, 'glUniform1uiv');
  Load(glUniform2uiv, 'glUniform2uiv');
  Load(glUniform3uiv, 'glUniform3uiv');
  Load(glUniform4uiv, 'glUniform4uiv');
  Load(glTexParameterIiv, 'glTexParameterIiv');
  Load(glTexParameterIuiv, 'glTexParameterIuiv');
  Load(glGetTexParameterIiv, 'glGetTexParameterIiv');
  Load(glGetTexParameterIuiv, 'glGetTexParameterIuiv');
  Load(glClearBufferiv, 'glClearBufferiv');
  Load(glClearBufferuiv, 'glClearBufferuiv');
  Load(glClearBufferfv, 'glClearBufferfv');
  Load(glClearBufferfi, 'glClearBufferfi');
  Load(glGetStringi, 'glGetStringi');
  Load(glIsRenderbuffer, 'glIsRenderbuffer');
  Load(glBindRenderbuffer, 'glBindRenderbuffer');
  Load(glDeleteRenderbuffers, 'glDeleteRenderbuffers');
  Load(glGenRenderbuffers, 'glGenRenderbuffers');
  Load(glRenderbufferStorage, 'glRenderbufferStorage');
  Load(glGetRenderbufferParameteriv, 'glGetRenderbufferParameteriv');
  Load(glIsFramebuffer, 'glIsFramebuffer');
  Load(glBindFramebuffer, 'glBindFramebuffer');
  Load(glDeleteFramebuffers, 'glDeleteFramebuffers');
  Load(glGenFramebuffers, 'glGenFramebuffers');
  Load(glCheckFramebufferStatus, 'glCheckFramebufferStatus');
  Load(glFramebufferTexture1D, 'glFramebufferTexture1D');
  Load(glFramebufferTexture2D, 'glFramebufferTexture2D');
  Load(glFramebufferTexture3D, 'glFramebufferTexture3D');
  Load(glFramebufferRenderbuffer, 'glFramebufferRenderbuffer');
  Load(glGetFramebufferAttachmentParameteriv, 'glGetFramebufferAttachmentParameteriv');
  Load(glGenerateMipmap, 'glGenerateMipmap');
  Load(glBlitFramebuffer, 'glBlitFramebuffer');
  Load(glRenderbufferStorageMultisample, 'glRenderbufferStorageMultisample');
  Load(glFramebufferTextureLayer, 'glFramebufferTextureLayer');
  Load(glMapBufferRange, 'glMapBufferRange');
  Load(glFlushMappedBufferRange, 'glFlushMappedBufferRange');
  Load(glBindVertexArray, 'glBindVertexArray');
  Load(glDeleteVertexArrays, 'glDeleteVertexArrays');
  Load(glGenVertexArrays, 'glGenVertexArrays');
  Load(glIsVertexArray, 'glIsVertexArray');
  {$endif}
  { OpenGL 3.1 }
  {$ifdef gl31}
  Load(glDrawArraysInstanced, 'glDrawArraysInstanced');
  Load(glDrawElementsInstanced, 'glDrawElementsInstanced');
  Load(glTexBuffer, 'glTexBuffer');
  Load(glPrimitiveRestartIndex, 'glPrimitiveRestartIndex');
  Load(glCopyBufferSubData, 'glCopyBufferSubData');
  Load(glGetUniformIndices, 'glGetUniformIndices');
  Load(glGetActiveUniformsiv, 'glGetActiveUniformsiv');
  Load(glGetActiveUniformName, 'glGetActiveUniformName');
  Load(glGetUniformBlockIndex, 'glGetUniformBlockIndex');
  Load(glGetActiveUniformBlockiv, 'glGetActiveUniformBlockiv');
  Load(glGetActiveUniformBlockName, 'glGetActiveUniformBlockName');
  Load(glUniformBlockBinding, 'glUniformBlockBinding');
  {$endif}
  { OpenGL 3.2 }
  {$ifdef gl32}
  Load(glDrawElementsBaseVertex, 'glDrawElementsBaseVertex');
  Load(glDrawRangeElementsBaseVertex, 'glDrawRangeElementsBaseVertex');
  Load(glDrawElementsInstancedBaseVertex, 'glDrawElementsInstancedBaseVertex');
  Load(glMultiDrawElementsBaseVertex, 'glMultiDrawElementsBaseVertex');
  Load(glProvokingVertex, 'glProvokingVertex');
  Load(glFenceSync, 'glFenceSync');
  Load(glIsSync, 'glIsSync');
  Load(glDeleteSync, 'glDeleteSync');
  Load(glClientWaitSync, 'glClientWaitSync');
  Load(glWaitSync, 'glWaitSync');
  Load(glGetInteger64v, 'glGetInteger64v');
  Load(glGetSynciv, 'glGetSynciv');
  Load(glGetInteger64i_v, 'glGetInteger64i_v');
  Load(glGetBufferParameteri64v, 'glGetBufferParameteri64v');
  Load(glFramebufferTexture, 'glFramebufferTexture');
  Load(glTexImage2DMultisample, 'glTexImage2DMultisample');
  Load(glTexImage3DMultisample, 'glTexImage3DMultisample');
  Load(glGetMultisamplefv, 'glGetMultisamplefv');
  Load(glSampleMaski, 'glSampleMaski');
  {$endif}
  { OpenGL 3.3 }
  {$ifdef gl33}
  Load(glBindFragDataLocationIndexed, 'glBindFragDataLocationIndexed');
  Load(glGetFragDataIndex, 'glGetFragDataIndex');
  Load(glGenSamplers, 'glGenSamplers');
  Load(glDeleteSamplers, 'glDeleteSamplers');
  Load(glIsSampler, 'glIsSampler');
  Load(glBindSampler, 'glBindSampler');
  Load(glSamplerParameteri, 'glSamplerParameteri');
  Load(glSamplerParameteriv, 'glSamplerParameteriv');
  Load(glSamplerParameterf, 'glSamplerParameterf');
  Load(glSamplerParameterfv, 'glSamplerParameterfv');
  Load(glSamplerParameterIiv, 'glSamplerParameterIiv');
  Load(glSamplerParameterIuiv, 'glSamplerParameterIuiv');
  Load(glGetSamplerParameteriv, 'glGetSamplerParameteriv');
  Load(glGetSamplerParameterIiv, 'glGetSamplerParameterIiv');
  Load(glGetSamplerParameterfv, 'glGetSamplerParameterfv');
  Load(glGetSamplerParameterIuiv, 'glGetSamplerParameterIuiv');
  Load(glQueryCounter, 'glQueryCounter');
  Load(glGetQueryObjecti64v, 'glGetQueryObjecti64v');
  Load(glGetQueryObjectui64v, 'glGetQueryObjectui64v');
  Load(glVertexAttribDivisor, 'glVertexAttribDivisor');
  Load(glVertexAttribP1ui, 'glVertexAttribP1ui');
  Load(glVertexAttribP1uiv, 'glVertexAttribP1uiv');
  Load(glVertexAttribP2ui, 'glVertexAttribP2ui');
  Load(glVertexAttribP2uiv, 'glVertexAttribP2uiv');
  Load(glVertexAttribP3ui, 'glVertexAttribP3ui');
  Load(glVertexAttribP3uiv, 'glVertexAttribP3uiv');
  Load(glVertexAttribP4ui, 'glVertexAttribP4ui');
  Load(glVertexAttribP4uiv, 'glVertexAttribP4uiv');
  {$endif}
  { OpenGL 3.3 compatibility profile }
  {$if defined(gl33) and defined(glcompat)}
  Load(glVertexP2ui, 'glVertexP2ui');
  Load(glVertexP2uiv, 'glVertexP2uiv');
  Load(glVertexP3ui, 'glVertexP3ui');
  Load(glVertexP3uiv, 'glVertexP3uiv');
  Load(glVertexP4ui, 'glVertexP4ui');
  Load(glVertexP4uiv, 'glVertexP4uiv');
  Load(glTexCoordP1ui, 'glTexCoordP1ui');
  Load(glTexCoordP1uiv, 'glTexCoordP1uiv');
  Load(glTexCoordP2ui, 'glTexCoordP2ui');
  Load(glTexCoordP2uiv, 'glTexCoordP2uiv');
  Load(glTexCoordP3ui, 'glTexCoordP3ui');
  Load(glTexCoordP3uiv, 'glTexCoordP3uiv');
  Load(glTexCoordP4ui, 'glTexCoordP4ui');
  Load(glTexCoordP4uiv, 'glTexCoordP4uiv');
  Load(glMultiTexCoordP1ui, 'glMultiTexCoordP1ui');
  Load(glMultiTexCoordP1uiv, 'glMultiTexCoordP1uiv');
  Load(glMultiTexCoordP2ui, 'glMultiTexCoordP2ui');
  Load(glMultiTexCoordP2uiv, 'glMultiTexCoordP2uiv');
  Load(glMultiTexCoordP3ui, 'glMultiTexCoordP3ui');
  Load(glMultiTexCoordP3uiv, 'glMultiTexCoordP3uiv');
  Load(glMultiTexCoordP4ui, 'glMultiTexCoordP4ui');
  Load(glMultiTexCoordP4uiv, 'glMultiTexCoordP4uiv');
  Load(glNormalP3ui, 'glNormalP3ui');
  Load(glNormalP3uiv, 'glNormalP3uiv');
  Load(glColorP3ui, 'glColorP3ui');
  Load(glColorP3uiv, 'glColorP3uiv');
  Load(glColorP4ui, 'glColorP4ui');
  Load(glColorP4uiv, 'glColorP4uiv');
  Load(glSecondaryColorP3ui, 'glSecondaryColorP3ui');
  Load(glSecondaryColorP3uiv, 'glSecondaryColorP3uiv');
  {$endif}
  { OpenGL 4.0 }
  {$ifdef gl40}
  Required := ContextHas(4, 0);
  Load(glMinSampleShading, 'glMinSampleShading');
  Load(glBlendEquationi, 'glBlendEquationi');
  Load(glBlendEquationSeparatei, 'glBlendEquationSeparatei');
  Load(glBlendFunci, 'glBlendFunci');
  Load(glBlendFuncSeparatei, 'glBlendFuncSeparatei');
  Load(glDrawArraysIndirect, 'glDrawArraysIndirect');
  Load(glDrawElementsIndirect, 'glDrawElementsIndirect');
  Load(glUniform1d, 'glUniform1d');
  Load(glUniform2d, 'glUniform2d');
  Load(glUniform3d, 'glUniform3d');
  Load(glUniform4d, 'glUniform4d');
  Load(glUniform1dv, 'glUniform1dv');
  Load(glUniform2dv, 'glUniform2dv');
  Load(glUniform3dv, 'glUniform3dv');
  Load(glUniform4dv, 'glUniform4dv');
  Load(glUniformMatrix2dv, 'glUniformMatrix2dv');
  Load(glUniformMatrix3dv, 'glUniformMatrix3dv');
  Load(glUniformMatrix4dv, 'glUniformMatrix4dv');
  Load(glUniformMatrix2x3dv, 'glUniformMatrix2x3dv');
  Load(glUniformMatrix2x4dv, 'glUniformMatrix2x4dv');
  Load(glUniformMatrix3x2dv, 'glUniformMatrix3x2dv');
  Load(glUniformMatrix3x4dv, 'glUniformMatrix3x4dv');
  Load(glUniformMatrix4x2dv, 'glUniformMatrix4x2dv');
  Load(glUniformMatrix4x3dv, 'glUniformMatrix4x3dv');
  Load(glGetUniformdv, 'glGetUniformdv');
  Load(glGetSubroutineUniformLocation, 'glGetSubroutineUniformLocation');
  Load(glGetSubroutineIndex, 'glGetSubroutineIndex');
  Load(glGetActiveSubroutineUniformiv, 'glGetActiveSubroutineUniformiv');
  Load(glGetActiveSubroutineUniformName, 'glGetActiveSubroutineUniformName');
  Load(glGetActiveSubroutineName, 'glGetActiveSubroutineName');
  Load(glUniformSubroutinesuiv, 'glUniformSubroutinesuiv');
  Load(glGetUniformSubroutineuiv, 'glGetUniformSubroutineuiv');
  Load(glGetProgramStageiv, 'glGetProgramStageiv');
  Load(glPatchParameteri, 'glPatchParameteri');
  Load(glPatchParameterfv, 'glPatchParameterfv');
  Load(glBindTransformFeedback, 'glBindTransformFeedback');
  Load(glDeleteTransformFeedbacks, 'glDeleteTransformFeedbacks');
  Load(glGenTransformFeedbacks, 'glGenTransformFeedbacks');
  Load(glIsTransformFeedback, 'glIsTransformFeedback');
  Load(glPauseTransformFeedback, 'glPauseTransformFeedback');
  Load(glResumeTransformFeedback, 'glResumeTransformFeedback');
  Load(glDrawTransformFeedback, 'glDrawTransformFeedback');
  Load(glDrawTransformFeedbackStream, 'glDrawTransformFeedbackStream');
  Load(glBeginQueryIndexed, 'glBeginQueryIndexed');
  Load(glEndQueryIndexed, 'glEndQueryIndexed');
  Load(glGetQueryIndexediv, 'glGetQueryIndexediv');
  {$endif}
  { OpenGL 4.1 }
  {$ifdef gl41}
  Required := ContextHas(4, 1);
  Load(glReleaseShaderCompiler, 'glReleaseShaderCompiler');
  Load(glShaderBinary, 'glShaderBinary');
  Load(glGetShaderPrecisionFormat, 'glGetShaderPrecisionFormat');
  Load(glDepthRangef, 'glDepthRangef');
  Load(glClearDepthf, 'glClearDepthf');
  Load(glGetProgramBinary, 'glGetProgramBinary');
  Load(glProgramBinary, 'glProgramBinary');
  Load(glProgramParameteri, 'glProgramParameteri');
  Load(glUseProgramStages, 'glUseProgramStages');
  Load(glActiveShaderProgram, 'glActiveShaderProgram');
  Load(glCreateShaderProgramv, 'glCreateShaderProgramv');
  Load(glBindProgramPipeline, 'glBindProgramPipeline');
  Load(glDeleteProgramPipelines, 'glDeleteProgramPipelines');
  Load(glGenProgramPipelines, 'glGenProgramPipelines');
  Load(glIsProgramPipeline, 'glIsProgramPipeline');
  Load(glGetProgramPipelineiv, 'glGetProgramPipelineiv');
  Load(glProgramUniform1i, 'glProgramUniform1i');
  Load(glProgramUniform1iv, 'glProgramUniform1iv');
  Load(glProgramUniform1f, 'glProgramUniform1f');
  Load(glProgramUniform1fv, 'glProgramUniform1fv');
  Load(glProgramUniform1d, 'glProgramUniform1d');
  Load(glProgramUniform1dv, 'glProgramUniform1dv');
  Load(glProgramUniform1ui, 'glProgramUniform1ui');
  Load(glProgramUniform1uiv, 'glProgramUniform1uiv');
  Load(glProgramUniform2i, 'glProgramUniform2i');
  Load(glProgramUniform2iv, 'glProgramUniform2iv');
  Load(glProgramUniform2f, 'glProgramUniform2f');
  Load(glProgramUniform2fv, 'glProgramUniform2fv');
  Load(glProgramUniform2d, 'glProgramUniform2d');
  Load(glProgramUniform2dv, 'glProgramUniform2dv');
  Load(glProgramUniform2ui, 'glProgramUniform2ui');
  Load(glProgramUniform2uiv, 'glProgramUniform2uiv');
  Load(glProgramUniform3i, 'glProgramUniform3i');
  Load(glProgramUniform3iv, 'glProgramUniform3iv');
  Load(glProgramUniform3f, 'glProgramUniform3f');
  Load(glProgramUniform3fv, 'glProgramUniform3fv');
  Load(glProgramUniform3d, 'glProgramUniform3d');
  Load(glProgramUniform3dv, 'glProgramUniform3dv');
  Load(glProgramUniform3ui, 'glProgramUniform3ui');
  Load(glProgramUniform3uiv, 'glProgramUniform3uiv');
  Load(glProgramUniform4i, 'glProgramUniform4i');
  Load(glProgramUniform4iv, 'glProgramUniform4iv');
  Load(glProgramUniform4f, 'glProgramUniform4f');
  Load(glProgramUniform4fv, 'glProgramUniform4fv');
  Load(glProgramUniform4d, 'glProgramUniform4d');
  Load(glProgramUniform4dv, 'glProgramUniform4dv');
  Load(glProgramUniform4ui, 'glProgramUniform4ui');
  Load(glProgramUniform4uiv, 'glProgramUniform4uiv');
  Load(glProgramUniformMatrix2fv, 'glProgramUniformMatrix2fv');
  Load(glProgramUniformMatrix3fv, 'glProgramUniformMatrix3fv');
  Load(glProgramUniformMatrix4fv, 'glProgramUniformMatrix4fv');
  Load(glProgramUniformMatrix2dv, 'glProgramUniformMatrix2dv');
  Load(glProgramUniformMatrix3dv, 'glProgramUniformMatrix3dv');
  Load(glProgramUniformMatrix4dv, 'glProgramUniformMatrix4dv');
  Load(glProgramUniformMatrix2x3fv, 'glProgramUniformMatrix2x3fv');
  Load(glProgramUniformMatrix3x2fv, 'glProgramUniformMatrix3x2fv');
  Load(glProgramUniformMatrix2x4fv, 'glProgramUniformMatrix2x4fv');
  Load(glProgramUniformMatrix4x2fv, 'glProgramUniformMatrix4x2fv');
  Load(glProgramUniformMatrix3x4fv, 'glProgramUniformMatrix3x4fv');
  Load(glProgramUniformMatrix4x3fv, 'glProgramUniformMatrix4x3fv');
  Load(glProgramUniformMatrix2x3dv, 'glProgramUniformMatrix2x3dv');
  Load(glProgramUniformMatrix3x2dv, 'glProgramUniformMatrix3x2dv');
  Load(glProgramUniformMatrix2x4dv, 'glProgramUniformMatrix2x4dv');
  Load(glProgramUniformMatrix4x2dv, 'glProgramUniformMatrix4x2dv');
  Load(glProgramUniformMatrix3x4dv, 'glProgramUniformMatrix3x4dv');
  Load(glProgramUniformMatrix4x3dv, 'glProgramUniformMatrix4x3dv');
  Load(glValidateProgramPipeline, 'glValidateProgramPipeline');
  Load(glGetProgramPipelineInfoLog, 'glGetProgramPipelineInfoLog');
  Load(glVertexAttribL1d, 'glVertexAttribL1d');
  Load(glVertexAttribL2d, 'glVertexAttribL2d');
  Load(glVertexAttribL3d, 'glVertexAttribL3d');
  Load(glVertexAttribL4d, 'glVertexAttribL4d');
  Load(glVertexAttribL1dv, 'glVertexAttribL1dv');
  Load(glVertexAttribL2dv, 'glVertexAttribL2dv');
  Load(glVertexAttribL3dv, 'glVertexAttribL3dv');
  Load(glVertexAttribL4dv, 'glVertexAttribL4dv');
  Load(glVertexAttribLPointer, 'glVertexAttribLPointer');
  Load(glGetVertexAttribLdv, 'glGetVertexAttribLdv');
  Load(glViewportArrayv, 'glViewportArrayv');
  Load(glViewportIndexedf, 'glViewportIndexedf');
  Load(glViewportIndexedfv, 'glViewportIndexedfv');
  Load(glScissorArrayv, 'glScissorArrayv');
  Load(glScissorIndexed, 'glScissorIndexed');
  Load(glScissorIndexedv, 'glScissorIndexedv');
  Load(glDepthRangeArrayv, 'glDepthRangeArrayv');
  Load(glDepthRangeIndexed, 'glDepthRangeIndexed');
  Load(glGetFloati_v, 'glGetFloati_v');
  Load(glGetDoublei_v, 'glGetDoublei_v');
  {$endif}
  { OpenGL 4.2 }
  {$ifdef gl42}
  Required := ContextHas(4, 2);
  Load(glDrawArraysInstancedBaseInstance, 'glDrawArraysInstancedBaseInstance');
  Load(glDrawElementsInstancedBaseInstance, 'glDrawElementsInstancedBaseInstance');
  Load(glDrawElementsInstancedBaseVertexBaseInstance, 'glDrawElementsInstancedBaseVertexBaseInstance');
  Load(glGetInternalformativ, 'glGetInternalformativ');
  Load(glGetActiveAtomicCounterBufferiv, 'glGetActiveAtomicCounterBufferiv');
  Load(glBindImageTexture, 'glBindImageTexture');
  Load(glMemoryBarrier, 'glMemoryBarrier');
  Load(glTexStorage1D, 'glTexStorage1D');
  Load(glTexStorage2D, 'glTexStorage2D');
  Load(glTexStorage3D, 'glTexStorage3D');
  Load(glDrawTransformFeedbackInstanced, 'glDrawTransformFeedbackInstanced');
  Load(glDrawTransformFeedbackStreamInstanced, 'glDrawTransformFeedbackStreamInstanced');
  {$endif}
  { OpenGL 4.3 }
  {$ifdef gl43}
  Required := ContextHas(4, 3);
  Load(glClearBufferData, 'glClearBufferData');
  Load(glClearBufferSubData, 'glClearBufferSubData');
  Load(glDispatchCompute, 'glDispatchCompute');
  Load(glDispatchComputeIndirect, 'glDispatchComputeIndirect');
  Load(glCopyImageSubData, 'glCopyImageSubData');
  Load(glFramebufferParameteri, 'glFramebufferParameteri');
  Load(glGetFramebufferParameteriv, 'glGetFramebufferParameteriv');
  Load(glGetInternalformati64v, 'glGetInternalformati64v');
  Load(glInvalidateTexSubImage, 'glInvalidateTexSubImage');
  Load(glInvalidateTexImage, 'glInvalidateTexImage');
  Load(glInvalidateBufferSubData, 'glInvalidateBufferSubData');
  Load(glInvalidateBufferData, 'glInvalidateBufferData');
  Load(glInvalidateFramebuffer, 'glInvalidateFramebuffer');
  Load(glInvalidateSubFramebuffer, 'glInvalidateSubFramebuffer');
  Load(glMultiDrawArraysIndirect, 'glMultiDrawArraysIndirect');
  Load(glMultiDrawElementsIndirect, 'glMultiDrawElementsIndirect');
  Load(glGetProgramInterfaceiv, 'glGetProgramInterfaceiv');
  Load(glGetProgramResourceIndex, 'glGetProgramResourceIndex');
  Load(glGetProgramResourceName, 'glGetProgramResourceName');
  Load(glGetProgramResourceiv, 'glGetProgramResourceiv');
  Load(glGetProgramResourceLocation, 'glGetProgramResourceLocation');
  Load(glGetProgramResourceLocationIndex, 'glGetProgramResourceLocationIndex');
  Load(glShaderStorageBlockBinding, 'glShaderStorageBlockBinding');
  Load(glTexBufferRange, 'glTexBufferRange');
  Load(glTexStorage2DMultisample, 'glTexStorage2DMultisample');
  Load(glTexStorage3DMultisample, 'glTexStorage3DMultisample');
  Load(glTextureView, 'glTextureView');
  Load(glBindVertexBuffer, 'glBindVertexBuffer');
  Load(glVertexAttribFormat, 'glVertexAttribFormat');
  Load(glVertexAttribIFormat, 'glVertexAttribIFormat');
  Load(glVertexAttribLFormat, 'glVertexAttribLFormat');
  Load(glVertexAttribBinding, 'glVertexAttribBinding');
  Load(glVertexBindingDivisor, 'glVertexBindingDivisor');
  Load(glDebugMessageControl, 'glDebugMessageControl');
  Load(glDebugMessageInsert, 'glDebugMessageInsert');
  Load(glDebugMessageCallback, 'glDebugMessageCallback');
  Load(glGetDebugMessageLog, 'glGetDebugMessageLog');
  Load(glPushDebugGroup, 'glPushDebugGroup');
  Load(glPopDebugGroup, 'glPopDebugGroup');
  Load(glObjectLabel, 'glObjectLabel');
  Load(glGetObjectLabel, 'glGetObjectLabel');
  Load(glObjectPtrLabel, 'glObjectPtrLabel');
  Load(glGetObjectPtrLabel, 'glGetObjectPtrLabel');
  {$endif}
  { OpenGL 4.4 }
  {$ifdef gl44}
  Required := ContextHas(4, 4);
  Load(glBufferStorage, 'glBufferStorage');
  Load(glClearTexImage, 'glClearTexImage');
  Load(glClearTexSubImage, 'glClearTexSubImage');
  Load(glBindBuffersBase, 'glBindBuffersBase');
  Load(glBindBuffersRange, 'glBindBuffersRange');
  Load(glBindTextures, 'glBindTextures');
  Load(glBindSamplers, 'glBindSamplers');
  Load(glBindImageTextures, 'glBindImageTextures');
  Load(glBindVertexBuffers, 'glBindVertexBuffers');
  {$endif}
  { OpenGL 4.5 }
  {$ifdef gl45}
  Required := ContextHas(4, 5);
  Load(glClipControl, 'glClipControl');
  Load(glCreateTransformFeedbacks, 'glCreateTransformFeedbacks');
  Load(glTransformFeedbackBufferBase, 'glTransformFeedbackBufferBase');
  Load(glTransformFeedbackBufferRange, 'glTransformFeedbackBufferRange');
  Load(glGetTransformFeedbackiv, 'glGetTransformFeedbackiv');
  Load(glGetTransformFeedbacki_v, 'glGetTransformFeedbacki_v');
  Load(glGetTransformFeedbacki64_v, 'glGetTransformFeedbacki64_v');
  Load(glCreateBuffers, 'glCreateBuffers');
  Load(glNamedBufferStorage, 'glNamedBufferStorage');
  Load(glNamedBufferData, 'glNamedBufferData');
  Load(glNamedBufferSubData, 'glNamedBufferSubData');
  Load(glCopyNamedBufferSubData, 'glCopyNamedBufferSubData');
  Load(glClearNamedBufferData, 'glClearNamedBufferData');
  Load(glClearNamedBufferSubData, 'glClearNamedBufferSubData');
  Load(glMapNamedBuffer, 'glMapNamedBuffer');
  Load(glMapNamedBufferRange, 'glMapNamedBufferRange');
  Load(glUnmapNamedBuffer, 'glUnmapNamedBuffer');
  Load(glFlushMappedNamedBufferRange, 'glFlushMappedNamedBufferRange');
  Load(glGetNamedBufferParameteriv, 'glGetNamedBufferParameteriv');
  Load(glGetNamedBufferParameteri64v, 'glGetNamedBufferParameteri64v');
  Load(glGetNamedBufferPointerv, 'glGetNamedBufferPointerv');
  Load(glGetNamedBufferSubData, 'glGetNamedBufferSubData');
  Load(glCreateFramebuffers, 'glCreateFramebuffers');
  Load(glNamedFramebufferRenderbuffer, 'glNamedFramebufferRenderbuffer');
  Load(glNamedFramebufferParameteri, 'glNamedFramebufferParameteri');
  Load(glNamedFramebufferTexture, 'glNamedFramebufferTexture');
  Load(glNamedFramebufferTextureLayer, 'glNamedFramebufferTextureLayer');
  Load(glNamedFramebufferDrawBuffer, 'glNamedFramebufferDrawBuffer');
  Load(glNamedFramebufferDrawBuffers, 'glNamedFramebufferDrawBuffers');
  Load(glNamedFramebufferReadBuffer, 'glNamedFramebufferReadBuffer');
  Load(glInvalidateNamedFramebufferData, 'glInvalidateNamedFramebufferData');
  Load(glInvalidateNamedFramebufferSubData, 'glInvalidateNamedFramebufferSubData');
  Load(glClearNamedFramebufferiv, 'glClearNamedFramebufferiv');
  Load(glClearNamedFramebufferuiv, 'glClearNamedFramebufferuiv');
  Load(glClearNamedFramebufferfv, 'glClearNamedFramebufferfv');
  Load(glClearNamedFramebufferfi, 'glClearNamedFramebufferfi');
  Load(glBlitNamedFramebuffer, 'glBlitNamedFramebuffer');
  Load(glCheckNamedFramebufferStatus, 'glCheckNamedFramebufferStatus');
  Load(glGetNamedFramebufferParameteriv, 'glGetNamedFramebufferParameteriv');
  Load(glGetNamedFramebufferAttachmentParameteriv, 'glGetNamedFramebufferAttachmentParameteriv');
  Load(glCreateRenderbuffers, 'glCreateRenderbuffers');
  Load(glNamedRenderbufferStorage, 'glNamedRenderbufferStorage');
  Load(glNamedRenderbufferStorageMultisample, 'glNamedRenderbufferStorageMultisample');
  Load(glGetNamedRenderbufferParameteriv, 'glGetNamedRenderbufferParameteriv');
  Load(glCreateTextures, 'glCreateTextures');
  Load(glTextureBuffer, 'glTextureBuffer');
  Load(glTextureBufferRange, 'glTextureBufferRange');
  Load(glTextureStorage1D, 'glTextureStorage1D');
  Load(glTextureStorage2D, 'glTextureStorage2D');
  Load(glTextureStorage3D, 'glTextureStorage3D');
  Load(glTextureStorage2DMultisample, 'glTextureStorage2DMultisample');
  Load(glTextureStorage3DMultisample, 'glTextureStorage3DMultisample');
  Load(glTextureSubImage1D, 'glTextureSubImage1D');
  Load(glTextureSubImage2D, 'glTextureSubImage2D');
  Load(glTextureSubImage3D, 'glTextureSubImage3D');
  Load(glCompressedTextureSubImage1D, 'glCompressedTextureSubImage1D');
  Load(glCompressedTextureSubImage2D, 'glCompressedTextureSubImage2D');
  Load(glCompressedTextureSubImage3D, 'glCompressedTextureSubImage3D');
  Load(glCopyTextureSubImage1D, 'glCopyTextureSubImage1D');
  Load(glCopyTextureSubImage2D, 'glCopyTextureSubImage2D');
  Load(glCopyTextureSubImage3D, 'glCopyTextureSubImage3D');
  Load(glTextureParameterf, 'glTextureParameterf');
  Load(glTextureParameterfv, 'glTextureParameterfv');
  Load(glTextureParameteri, 'glTextureParameteri');
  Load(glTextureParameterIiv, 'glTextureParameterIiv');
  Load(glTextureParameterIuiv, 'glTextureParameterIuiv');
  Load(glTextureParameteriv, 'glTextureParameteriv');
  Load(glGenerateTextureMipmap, 'glGenerateTextureMipmap');
  Load(glBindTextureUnit, 'glBindTextureUnit');
  Load(glGetTextureImage, 'glGetTextureImage');
  Load(glGetCompressedTextureImage, 'glGetCompressedTextureImage');
  Load(glGetTextureLevelParameterfv, 'glGetTextureLevelParameterfv');
  Load(glGetTextureLevelParameteriv, 'glGetTextureLevelParameteriv');
  Load(glGetTextureParameterfv, 'glGetTextureParameterfv');
  Load(glGetTextureParameterIiv, 'glGetTextureParameterIiv');
  Load(glGetTextureParameterIuiv, 'glGetTextureParameterIuiv');
  Load(glGetTextureParameteriv, 'glGetTextureParameteriv');
  Load(glCreateVertexArrays, 'glCreateVertexArrays');
  Load(glDisableVertexArrayAttrib, 'glDisableVertexArrayAttrib');
  Load(glEnableVertexArrayAttrib, 'glEnableVertexArrayAttrib');
  Load(glVertexArrayElementBuffer, 'glVertexArrayElementBuffer');
  Load(glVertexArrayVertexBuffer, 'glVertexArrayVertexBuffer');
  Load(glVertexArrayVertexBuffers, 'glVertexArrayVertexBuffers');
  Load(glVertexArrayAttribBinding, 'glVertexArrayAttribBinding');
  Load(glVertexArrayAttribFormat, 'glVertexArrayAttribFormat');
  Load(glVertexArrayAttribIFormat, 'glVertexArrayAttribIFormat');
  Load(glVertexArrayAttribLFormat, 'glVertexArrayAttribLFormat');
  Load(glVertexArrayBindingDivisor, 'glVertexArrayBindingDivisor');
  Load(glGetVertexArrayiv, 'glGetVertexArrayiv');
  Load(glGetVertexArrayIndexediv, 'glGetVertexArrayIndexediv');
  Load(glGetVertexArrayIndexed64iv, 'glGetVertexArrayIndexed64iv');
  Load(glCreateSamplers, 'glCreateSamplers');
  Load(glCreateProgramPipelines, 'glCreateProgramPipelines');
  Load(glCreateQueries, 'glCreateQueries');
  Load(glGetQueryBufferObjecti64v, 'glGetQueryBufferObjecti64v');
  Load(glGetQueryBufferObjectiv, 'glGetQueryBufferObjectiv');
  Load(glGetQueryBufferObjectui64v, 'glGetQueryBufferObjectui64v');
  Load(glGetQueryBufferObjectuiv, 'glGetQueryBufferObjectuiv');
  Load(glMemoryBarrierByRegion, 'glMemoryBarrierByRegion');
  Load(glGetTextureSubImage, 'glGetTextureSubImage');
  Load(glGetCompressedTextureSubImage, 'glGetCompressedTextureSubImage');
  Load(glGetGraphicsResetStatus, 'glGetGraphicsResetStatus');
  Load(glGetnCompressedTexImage, 'glGetnCompressedTexImage');
  Load(glGetnTexImage, 'glGetnTexImage');
  Load(glGetnUniformdv, 'glGetnUniformdv');
  Load(glGetnUniformfv, 'glGetnUniformfv');
  Load(glGetnUniformiv, 'glGetnUniformiv');
  Load(glGetnUniformuiv, 'glGetnUniformuiv');
  Load(glReadnPixels, 'glReadnPixels');
  Load(glTextureBarrier, 'glTextureBarrier');
  {$endif}
  { OpenGL 4.5 compatibility profile }
  {$if defined(gl45) and defined(glcompat)}
  Required := ContextHas(4, 5);
  Load(glGetnMapdv, 'glGetnMapdv');
  Load(glGetnMapfv, 'glGetnMapfv');
  Load(glGetnMapiv, 'glGetnMapiv');
  Load(glGetnPixelMapfv, 'glGetnPixelMapfv');
  Load(glGetnPixelMapuiv, 'glGetnPixelMapuiv');
  Load(glGetnPixelMapusv, 'glGetnPixelMapusv');
  Load(glGetnPolygonStipple, 'glGetnPolygonStipple');
  Load(glGetnColorTable, 'glGetnColorTable');
  Load(glGetnConvolutionFilter, 'glGetnConvolutionFilter');
  Load(glGetnSeparableFilter, 'glGetnSeparableFilter');
  Load(glGetnHistogram, 'glGetnHistogram');
  Load(glGetnMinmax, 'glGetnMinmax');
  {$endif}
  { OpenGL 4.6 }
  {$ifdef gl46}
  Required := ContextHas(4, 6);
  Load(glSpecializeShader, 'glSpecializeShader');
  Load(glMultiDrawArraysIndirectCount, 'glMultiDrawArraysIndirectCount');
  Load(glMultiDrawElementsIndirectCount, 'glMultiDrawElementsIndirectCount');
  Load(glPolygonOffsetClamp, 'glPolygonOffsetClamp');
  {$endif}
  {$endif}
  Result := Loaded and VersionCheck;
end;

{ TNullOpenGLInfo is returned when no platform unit has been loaded }

type
  TNullOpenGLInfo = class(TInterfacedObject, IOpenGLInfo)
  public
    function IsValid: Boolean;
    function Major: Integer;
    function Minor: Integer;
    function MajorMinor: string;
    function Renderer: string;
    function Vendor: string;
    function Version: string;
    function Extensions: string;
  end;

function TNullOpenGLInfo.IsValid: Boolean;
begin
  Result := False;
end;

function TNullOpenGLInfo.Major: Integer;
begin
  Result := 0;
end;

function TNullOpenGLInfo.Minor: Integer;
begin
  Result := 0;
end;

function TNullOpenGLInfo.MajorMinor: string;
begin
  Result := '';
end;

function TNullOpenGLInfo.Renderer: string;
begin
  Result := '';
end;

function TNullOpenGLInfo.Vendor: string;
begin
  Result := '';
end;

function TNullOpenGLInfo.Version: string;
begin
  Result := '';
end;

function TNullOpenGLInfo.Extensions: string;
begin
  Result := '';
end;

var
  NullInfo: IOpenGLInfo;

function OpenGLInfo: IOpenGLInfo;
begin
  if Assigned(OpenGLPlatformInfo) then
    Result := OpenGLPlatformInfo
  else
  begin
    if NullInfo = nil then
      NullInfo := TNullOpenGLInfo.Create;
    Result := NullInfo;
  end;
end;

function OpenGLContextCreate(Window: GLwindow; const Params: TOpenGLParams): IOpenGLContext;
begin
  if Assigned(OpenGLPlatformContextCreate) then
    Result := OpenGLPlatformContextCreate(Window, Params)
  else
    Result := nil;
end;

function OpenGLContextCurrent: IOpenGLContext;
begin
  if Assigned(OpenGLPlatformContextCurrent) then
    Result := OpenGLPlatformContextCurrent
  else
    Result := nil;
end;

class function TOpenGLParams.Create: TOpenGLParams;
begin
  Result.Depth := 24;
  Result.Stencil := 8;
  Result.MultiSampling := True;
  Result.MultiSamples := 4;
end;

function OpenGLLoad: Boolean;
begin
  Result := OpenGLInfo.IsValid;
end;

end.
