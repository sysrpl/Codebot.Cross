(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified July 2022                                  *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.gles.txt> }
unit Codebot.GLES;

{ Reduce GLES driver version requirements by adding gles2 to your
  build defines }

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
  GLclampd = Double;
  GLclampf = Single;
  GLclampx = Int32;
  GLdouble = Double;
  GLenum = UInt32;
  GLfixed = Int32;
  GLfloat = Single;
  GLhalf = UInt16;
  GLint = Int32;
  GLint64 = Int64;
  GLintptr = Int32;
  GLshort = Int16;
  GLsizei = Int32;
  GLsizeiptr = IntPtr;
  GLsync = Pointer;
  GLubyte = UInt8;
  GLuint = UInt32;
  GLuint64 = UInt64;
  GLushort = UInt16;
  GLvoid = Pointer;

  PGLbitfield = ^GLbitfield;
  PGLboolean = ^GLboolean;
  PGLbyte = PByte;
  PGLchar = PChar;
  PGLclampd = ^GLclampd;
  PGLclampf = ^GLclampf;
  PGLclampx = ^GLclampx;
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
  PGLvoid = ^GLvoid;
  PPGLchar = ^PGLchar;

{ GLwindow represents an HWND on windows or a XWindow on linux }
  GLwindow = UIntPtr;
{ GLcontext represents an opengl context }
  GLcontext = Pointer;

{ GLES 2 is required as a minimum }

{$region gles2}
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
  GL_MAJOR_VERSION = $821B;
  GL_MINOR_VERSION = $821C;
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
  glGetString: function(name: GLenum): PGLchar; apicall;
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

{ GLES 3 is becomes the minimum requirement if gles3 is defined in the
  include file. See the note at the top of this file. }

{$region gles3}
{$ifdef gles3}
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
  GL_INVALID_INDEX = UInt32($FFFFFFFF);
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
  GL_TIMEOUT_IGNORED = UInt64($FFFFFFFFFFFFFFFF);
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
  glEndTransformFeedback: procedure(); apicall;
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
  glPauseTransformFeedback: procedure(); apicall;
  glResumeTransformFeedback: procedure(); apicall;
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

  Note: An OpenGL version is not requested in this design. Instead your system
  will be queried for the maximum available in compatibility mode. The legacy
  fixed function pipeline will not be loaded, but an GLES 2 or 3 compatible
  version will be requested.

  For GLES 2 this means either your compatibility version is 2.1 or greater or
  ES2 compatiblity is found in your extension strings.

  For GLES 3 this means either your compatibility version is 3.4 or greater or
  ES3 compatiblity is found in your extension strings.

  If the above conditions are not met based on your compiler defines, then
  OpenGLContextCreate will fail and return nil. }

function OpenGLContextCreate(Window: GLwindow; const Params: TOpenGLParams): IOpenGLContext;

{ OpenGLContextCurrent returns the current context for the calling thread
  or nil if there is no current context }

function OpenGLContextCurrent: IOpenGLContext;

{ IOpenGLInfo provides information about opengl support on your platform and
  hardware. If the minimum of GLES 2 or 3 is not satisfied, then IsValid
  will return False, and it is unsafe to call any opengl functions.

  See the notes on the OpenGLInfo for the current state of platform support. }

type
  IOpenGLInfo = interface
  ['{6713F1F2-8642-4734-ABFF-C84614DB3E8A}']
    { IsValid returns True if your hardware supports the minimum GLES version }
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

  Linux (Gtk2 Gtk3) and Microsoft Windows }

function OpenGLInfo: IOpenGLInfo;

implementation

uses
{$ifdef linux}
  Codebot.GLES.Linux;
{$endif}
{$ifdef windows}
  Codebot.GLES.Windows;
{$endif}

function OpenGLInfo: IOpenGLInfo;
begin
  Result := OpenGLInfoPrivate;
end;

function OpenGLContextCreate(Window: GLwindow; const Params: TOpenGLParams): IOpenGLContext;
begin
  Result := OpenGLContextCreatePrivate(Window, Params);
end;

function OpenGLContextCurrent: IOpenGLContext;
begin
  Result := OpenGLContextCurrentPrivate;
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

