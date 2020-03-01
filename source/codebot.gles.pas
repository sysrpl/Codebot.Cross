unit Codebot.GLES;

{$i codebot.inc}

interface

type
  GLbitfield = uint32;
  GLboolean = byte;
  GLbyte = int8;
  GLchar = char;
  GLcharARB = byte;
  GLclampd = double;
  GLclampf = single;
  GLclampx = int32;
  GLdouble = double;
  GLenum = uint32;
  GLfixed = int32;
  GLfloat = single;
  GLhalf = uint16;
  GLhalfARB = uint16;
  GLhalfNV = uint16;
  GLhandleARB = uint32;
  GLint = int32;
  GLint64 = int64;
  GLint64EXT = int64;
  GLintptr = int32;
  GLintptrARB = int32;
  GLshort = int16;
  GLsizei = int32;
  GLsizeiptr = int32;
  GLsizeiptrARB = int32;
  GLsync = pointer;
  GLubyte = uint8;
  GLuint = uint32;
  GLuint64 = uint64;
  GLuint64EXT = uint64;
  GLushort = uint16;
  GLvdpauSurfaceNV = int32;
  GLvoid = pointer;

  PGLbitfield = ^uint32;
  PGLboolean = ^byte;
  PGLbyte = ^int8;
  PGLchar = ^char;
  PGLcharARB = ^byte;
  PGLclampd = ^double;
  PGLclampf = ^single;
  PGLclampx = ^int32;
  PGLdouble = ^double;
  PGLeglClientBufferEXT = ^pointer;
  PGLeglImageOES = ^pointer;
  PGLenum = ^uint32;
  PGLfixed = ^int32;
  PGLfloat = ^single;
  PGLhalf = ^uint16;
  PGLhalfARB = ^uint16;
  PGLhalfNV = ^uint16;
  PGLhandleARB = ^uint32;
  PGLint = ^int32;
  PGLint64 = ^int64;
  PGLint64EXT = ^int64;
  PGLintptr = ^int32;
  PGLintptrARB = ^int32;
  PGLshort = ^int16;
  PGLsizei = ^int32;
  PGLsizeiptr = ^int32;
  PGLsizeiptrARB = ^int32;
  PGLsync = ^pointer;
  PGLubyte = ^uint8;
  PGLuint = ^uint32;
  PGLuint64 = ^uint64;
  PGLuint64EXT = ^uint64;
  PGLushort = ^uint16;
  PGLvdpauSurfaceNV = ^int32;
  PGLvoid = ^pointer;
  P_cl_context = ^pointer;
  P_cl_event = ^pointer;
  PPGLchar = ^PGLchar;
  PPGLcharARB = ^PGLcharARB;
  PPGLboolean = ^PGLboolean;

const
  GL_FALSE = 0;
  GL_INVALID_INDEX = uint32($FFFFFFFF);
  GL_NONE = 0;
  GL_NONE_OES = 0;
  GL_NO_ERROR = 0;
  GL_ONE = 1;
  GL_TIMEOUT_IGNORED = uint64($FFFFFFFFFFFFFFFF);
  GL_TIMEOUT_IGNORED_APPLE = uint64($FFFFFFFFFFFFFFFF);
  GL_TRUE = 1;
  GL_VERSION_ES_CL_1_0 = 1;
  GL_VERSION_ES_CL_1_1 = 1;
  GL_VERSION_ES_CM_1_1 = 1;
  GL_ZERO = 0;

  GL_DEPTH_BUFFER_BIT = $00000100;
  GL_STENCIL_BUFFER_BIT = $00000400;
  GL_COLOR_BUFFER_BIT = $00004000;
  GL_POINTS = $0000;
  GL_LINES = $0001;
  GL_LINE_LOOP = $0002;
  GL_LINE_STRIP = $0003;
  GL_TRIANGLES = $0004;
  GL_TRIANGLE_STRIP = $0005;
  GL_TRIANGLE_FAN = $0006;
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
  glActiveTexture: procedure (texture: GLenum); apicall;
  glAttachShader: procedure (program_: GLuint; shader: GLuint); apicall;
  glBindAttribLocation: procedure (program_: GLuint; index: GLuint; name: PGLchar); apicall;
  glBindBuffer: procedure (target: GLenum; buffer: GLuint); apicall;
  glBindFramebuffer: procedure (target: GLenum; framebuffer: GLuint); apicall;
  glBindRenderbuffer: procedure (target: GLenum; renderbuffer: GLuint); apicall;
  glBindTexture: procedure (target: GLenum; texture: GLuint); apicall;
  glBlendColor: procedure (red: GLfloat; green: GLfloat; blue: GLfloat; alpha: GLfloat); apicall;
  glBlendEquation: procedure (mode: GLenum); apicall;
  glBlendEquationSeparate: procedure (modeRGB: GLenum; modeAlpha: GLenum); apicall;
  glBlendFunc: procedure (sfactor: GLenum; dfactor: GLenum); apicall;
  glBlendFuncSeparate: procedure (sfactorRGB: GLenum; dfactorRGB: GLenum; sfactorAlpha: GLenum; dfactorAlpha: GLenum); apicall;
  glBufferData: procedure (target: GLenum; size: GLsizeiptr; data: Pointer; usage: GLenum); apicall;
  glBufferSubData: procedure (target: GLenum; offset: GLintptr; size: GLsizeiptr; data: Pointer); apicall;
  glCheckFramebufferStatus: function (target: GLenum): GLenum; apicall;
  glClear: procedure (mask: GLbitfield); apicall;
  glClearColor: procedure (red: GLfloat; green: GLfloat; blue: GLfloat; alpha: GLfloat); apicall;
  glClearDepthf: procedure (d: GLfloat); apicall;
  glClearStencil: procedure (s: GLint); apicall;
  glColorMask: procedure (red: GLboolean; green: GLboolean; blue: GLboolean; alpha: GLboolean); apicall;
  glCompileShader: procedure (shader: GLuint); apicall;
  glCompressedTexImage2D: procedure (target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; height: GLsizei; border: GLint; imageSize: GLsizei; data: Pointer); apicall;
  glCompressedTexSubImage2D: procedure (target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; width: GLsizei; height: GLsizei; format: GLenum; imageSize: GLsizei; data: Pointer); apicall;
  glCopyTexImage2D: procedure (target: GLenum; level: GLint; internalformat: GLenum; x: GLint; y: GLint; width: GLsizei; height: GLsizei; border: GLint); apicall;
  glCopyTexSubImage2D: procedure (target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; x: GLint; y: GLint; width: GLsizei; height: GLsizei); apicall;
  glCreateProgram: function (): GLuint; apicall;
  glCreateShader: function (type_: GLenum): GLuint; apicall;
  glCullFace: procedure (mode: GLenum); apicall;
  glDeleteBuffers: procedure (n: GLsizei; buffers: PGLuint); apicall;
  glDeleteFramebuffers: procedure (n: GLsizei; framebuffers: PGLuint); apicall;
  glDeleteProgram: procedure (program_: GLuint); apicall;
  glDeleteRenderbuffers: procedure (n: GLsizei; renderbuffers: PGLuint); apicall;
  glDeleteShader: procedure (shader: GLuint); apicall;
  glDeleteTextures: procedure (n: GLsizei; textures: PGLuint); apicall;
  glDepthFunc: procedure (func: GLenum); apicall;
  glDepthMask: procedure (flag: GLboolean); apicall;
  glDepthRangef: procedure (n: GLfloat; f: GLfloat); apicall;
  glDetachShader: procedure (program_: GLuint; shader: GLuint); apicall;
  glDisable: procedure (cap: GLenum); apicall;
  glDisableVertexAttribArray: procedure (index: GLuint); apicall;
  glDrawArrays: procedure (mode: GLenum; first: GLint; count: GLsizei); apicall;
  glDrawElements: procedure (mode: GLenum; count: GLsizei; type_: GLenum; indices: Pointer); apicall;
  glEnable: procedure (cap: GLenum); apicall;
  glEnableVertexAttribArray: procedure (index: GLuint); apicall;
  glFinish: procedure (); apicall;
  glFlush: procedure (); apicall;
  glFramebufferRenderbuffer: procedure (target: GLenum; attachment: GLenum; renderbuffertarget: GLenum; renderbuffer: GLuint); apicall;
  glFramebufferTexture2D: procedure (target: GLenum; attachment: GLenum; textarget: GLenum; texture: GLuint; level: GLint); apicall;
  glFrontFace: procedure (mode: GLenum); apicall;
  glGenBuffers: procedure (n: GLsizei; buffers: PGLuint); apicall;
  glGenerateMipmap: procedure (target: GLenum); apicall;
  glGenFramebuffers: procedure (n: GLsizei; framebuffers: PGLuint); apicall;
  glGenRenderbuffers: procedure (n: GLsizei; renderbuffers: PGLuint); apicall;
  glGenTextures: procedure (n: GLsizei; textures: PGLuint); apicall;
  glGetActiveAttrib: procedure (program_: GLuint; index: GLuint; bufSize: GLsizei; length: PGLsizei; size: PGLint; type_: PGLenum; name: PGLchar); apicall;
  glGetActiveUniform: procedure (program_: GLuint; index: GLuint; bufSize: GLsizei; length: PGLsizei; size: PGLint; type_: PGLenum; name: PGLchar); apicall;
  glGetAttachedShaders: procedure (program_: GLuint; maxCount: GLsizei; count: PGLsizei; shaders: PGLuint); apicall;
  glGetAttribLocation: function (program_: GLuint; name: PGLchar): GLint; apicall;
  glGetBooleanv: procedure (pname: GLenum; data: PGLboolean); apicall;
  glGetBufferParameteriv: procedure (target: GLenum; pname: GLenum; params: PGLint); apicall;
  glGetError: function (): GLenum; apicall;
  glGetFloatv: procedure (pname: GLenum; data: PGLfloat); apicall;
  glGetFramebufferAttachmentParameteriv: procedure (target: GLenum; attachment: GLenum; pname: GLenum; params: PGLint); apicall;
  glGetIntegerv: procedure (pname: GLenum; data: PGLint); apicall;
  glGetProgramiv: procedure (program_: GLuint; pname: GLenum; params: PGLint); apicall;
  glGetProgramInfoLog: procedure (program_: GLuint; bufSize: GLsizei; length: PGLsizei; infoLog: PGLchar); apicall;
  glGetRenderbufferParameteriv: procedure (target: GLenum; pname: GLenum; params: PGLint); apicall;
  glGetShaderiv: procedure (shader: GLuint; pname: GLenum; params: PGLint); apicall;
  glGetShaderInfoLog: procedure (shader: GLuint; bufSize: GLsizei; length: PGLsizei; infoLog: PGLchar); apicall;
  glGetShaderPrecisionFormat: procedure (shadertype: GLenum; precisiontype: GLenum; range: PGLint; precision: PGLint); apicall;
  glGetShaderSource: procedure (shader: GLuint; bufSize: GLsizei; length: PGLsizei; source: PGLchar); apicall;
  glGetString: function (name: GLenum): PGLubyte; apicall;
  glGetTexParameterfv: procedure (target: GLenum; pname: GLenum; params: PGLfloat); apicall;
  glGetTexParameteriv: procedure (target: GLenum; pname: GLenum; params: PGLint); apicall;
  glGetUniformfv: procedure (program_: GLuint; location: GLint; params: PGLfloat); apicall;
  glGetUniformiv: procedure (program_: GLuint; location: GLint; params: PGLint); apicall;
  glGetUniformLocation: function (program_: GLuint; name: PGLchar): GLint; apicall;
  glGetVertexAttribfv: procedure (index: GLuint; pname: GLenum; params: PGLfloat); apicall;
  glGetVertexAttribiv: procedure (index: GLuint; pname: GLenum; params: PGLint); apicall;
  glGetVertexAttribPointerv: procedure (index: GLuint; pname: GLenum; pointer: PPointer); apicall;
  glHint: procedure (target: GLenum; mode: GLenum); apicall;
  glIsBuffer: function (buffer: GLuint): GLboolean; apicall;
  glIsEnabled: function (cap: GLenum): GLboolean; apicall;
  glIsFramebuffer: function (framebuffer: GLuint): GLboolean; apicall;
  glIsProgram: function (program_: GLuint): GLboolean; apicall;
  glIsRenderbuffer: function (renderbuffer: GLuint): GLboolean; apicall;
  glIsShader: function (shader: GLuint): GLboolean; apicall;
  glIsTexture: function (texture: GLuint): GLboolean; apicall;
  glLineWidth: procedure (width: GLfloat); apicall;
  glLinkProgram: procedure (program_: GLuint); apicall;
  glPixelStorei: procedure (pname: GLenum; param: GLint); apicall;
  glPolygonOffset: procedure (factor: GLfloat; units: GLfloat); apicall;
  glReadPixels: procedure (x: GLint; y: GLint; width: GLsizei; height: GLsizei; format: GLenum; type_: GLenum; pixels: Pointer); apicall;
  glReleaseShaderCompiler: procedure (); apicall;
  glRenderbufferStorage: procedure (target: GLenum; internalformat: GLenum; width: GLsizei; height: GLsizei); apicall;
  glSampleCoverage: procedure (value: GLfloat; invert: GLboolean); apicall;
  glScissor: procedure (x: GLint; y: GLint; width: GLsizei; height: GLsizei); apicall;
  glShaderBinary: procedure (count: GLsizei; shaders: PGLuint; binaryformat: GLenum; binary: Pointer; length: GLsizei); apicall;
  glShaderSource: procedure (shader: GLuint; count: GLsizei; string_: PPGLchar; length: PGLint); apicall;
  glStencilFunc: procedure (func: GLenum; ref: GLint; mask: GLuint); apicall;
  glStencilFuncSeparate: procedure (face: GLenum; func: GLenum; ref: GLint; mask: GLuint); apicall;
  glStencilMask: procedure (mask: GLuint); apicall;
  glStencilMaskSeparate: procedure (face: GLenum; mask: GLuint); apicall;
  glStencilOp: procedure (fail: GLenum; zfail: GLenum; zpass: GLenum); apicall;
  glStencilOpSeparate: procedure (face: GLenum; sfail: GLenum; dpfail: GLenum; dppass: GLenum); apicall;
  glTexImage2D: procedure (target: GLenum; level: GLint; internalformat: GLint; width: GLsizei; height: GLsizei; border: GLint; format: GLenum; type_: GLenum; pixels: Pointer); apicall;
  glTexParameterf: procedure (target: GLenum; pname: GLenum; param: GLfloat); apicall;
  glTexParameterfv: procedure (target: GLenum; pname: GLenum; params: PGLfloat); apicall;
  glTexParameteri: procedure (target: GLenum; pname: GLenum; param: GLint); apicall;
  glTexParameteriv: procedure (target: GLenum; pname: GLenum; params: PGLint); apicall;
  glTexSubImage2D: procedure (target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; width: GLsizei; height: GLsizei; format: GLenum; type_: GLenum; pixels: Pointer); apicall;
  glUniform1f: procedure (location: GLint; v0: GLfloat); apicall;
  glUniform1fv: procedure (location: GLint; count: GLsizei; value: PGLfloat); apicall;
  glUniform1i: procedure (location: GLint; v0: GLint); apicall;
  glUniform1iv: procedure (location: GLint; count: GLsizei; value: PGLint); apicall;
  glUniform2f: procedure (location: GLint; v0: GLfloat; v1: GLfloat); apicall;
  glUniform2fv: procedure (location: GLint; count: GLsizei; value: PGLfloat); apicall;
  glUniform2i: procedure (location: GLint; v0: GLint; v1: GLint); apicall;
  glUniform2iv: procedure (location: GLint; count: GLsizei; value: PGLint); apicall;
  glUniform3f: procedure (location: GLint; v0: GLfloat; v1: GLfloat; v2: GLfloat); apicall;
  glUniform3fv: procedure (location: GLint; count: GLsizei; value: PGLfloat); apicall;
  glUniform3i: procedure (location: GLint; v0: GLint; v1: GLint; v2: GLint); apicall;
  glUniform3iv: procedure (location: GLint; count: GLsizei; value: PGLint); apicall;
  glUniform4f: procedure (location: GLint; v0: GLfloat; v1: GLfloat; v2: GLfloat; v3: GLfloat); apicall;
  glUniform4fv: procedure (location: GLint; count: GLsizei; value: PGLfloat); apicall;
  glUniform4i: procedure (location: GLint; v0: GLint; v1: GLint; v2: GLint; v3: GLint); apicall;
  glUniform4iv: procedure (location: GLint; count: GLsizei; value: PGLint); apicall;
  glUniformMatrix2fv: procedure (location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); apicall;
  glUniformMatrix3fv: procedure (location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); apicall;
  glUniformMatrix4fv: procedure (location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); apicall;
  glUseProgram: procedure (program_: GLuint); apicall;
  glValidateProgram: procedure (program_: GLuint); apicall;
  glVertexAttrib1f: procedure (index: GLuint; x: GLfloat); apicall;
  glVertexAttrib1fv: procedure (index: GLuint; v: PGLfloat); apicall;
  glVertexAttrib2f: procedure (index: GLuint; x: GLfloat; y: GLfloat); apicall;
  glVertexAttrib2fv: procedure (index: GLuint; v: PGLfloat); apicall;
  glVertexAttrib3f: procedure (index: GLuint; x: GLfloat; y: GLfloat; z: GLfloat); apicall;
  glVertexAttrib3fv: procedure (index: GLuint; v: PGLfloat); apicall;
  glVertexAttrib4f: procedure (index: GLuint; x: GLfloat; y: GLfloat; z: GLfloat; w: GLfloat); apicall;
  glVertexAttrib4fv: procedure (index: GLuint; v: PGLfloat); apicall;
  glVertexAttribPointer: procedure (index: GLuint; size: GLint; type_: GLenum; normalized: GLboolean; stride: GLsizei; pointer: Pointer); apicall;
  glViewport: procedure (x: GLint; y: GLint; width: GLsizei; height: GLsizei); apicall;

function LoadOpenGLES: Boolean;

implementation

var
  Loaded: Boolean;
  LoadedSuccess: Boolean;

function LoadOpenGLES: Boolean;
const
  LibName =
  {$if defined(windows)}
  'opengl32.dll'
  {$elseif defined(darwin)}
  '/System/Library/Frameworks/OpenGL.framework/Libraries/libGL.dylib'
  {$else}
  'libGL.so.1';
  {$endif}
var
  LibHandle: TLibHandle;

  function Load(const ProcName: string; out Proc: Pointer): Boolean;
  begin
    Proc := GetProcAddress(LibHandle, ProcName);
    Result := Proc <> nil;
  end;

begin
  Result := LoadedSuccess;
  if Loaded then
    Exit;
  Loaded := True;
  LibHandle := LoadLibrary(LibName);
  if LibHandle = 0 then
    Exit;
  LoadedSuccess := Load('glActiveTexture', @glActiveTexture) and
    Load('glAttachShader', @glAttachShader) and
    Load('glBindAttribLocation', @glBindAttribLocation) and
    Load('glBindBuffer', @glBindBuffer) and
    Load('glBindFramebuffer', @glBindFramebuffer) and
    Load('glBindRenderbuffer', @glBindRenderbuffer) and
    Load('glBindTexture', @glBindTexture) and
    Load('glBlendColor', @glBlendColor) and
    Load('glBlendEquation', @glBlendEquation) and
    Load('glBlendEquationSeparate', @glBlendEquationSeparate) and
    Load('glBlendFunc', @glBlendFunc) and
    Load('glBlendFuncSeparate', @glBlendFuncSeparate) and
    Load('glBufferData', @glBufferData) and
    Load('glBufferSubData', @glBufferSubData) and
    Load('glCheckFramebufferStatus', @glCheckFramebufferStatus) and
    Load('glClear', @glClear) and
    Load('glClearColor', @glClearColor) and
    Load('glClearDepthf', @glClearDepthf) and
    Load('glClearStencil', @glClearStencil) and
    Load('glColorMask', @glColorMask) and
    Load('glCompileShader', @glCompileShader) and
    Load('glCompressedTexImage2D', @glCompressedTexImage2D) and
    Load('glCompressedTexSubImage2D', @glCompressedTexSubImage2D) and
    Load('glCopyTexImage2D', @glCopyTexImage2D) and
    Load('glCopyTexSubImage2D', @glCopyTexSubImage2D) and
    Load('glCreateProgram', @glCreateProgram) and
    Load('glCreateShader', @glCreateShader) and
    Load('glCullFace', @glCullFace) and
    Load('glDeleteBuffers', @glDeleteBuffers) and
    Load('glDeleteFramebuffers', @glDeleteFramebuffers) and
    Load('glDeleteProgram', @glDeleteProgram) and
    Load('glDeleteRenderbuffers', @glDeleteRenderbuffers) and
    Load('glDeleteShader', @glDeleteShader) and
    Load('glDeleteTextures', @glDeleteTextures) and
    Load('glDepthFunc', @glDepthFunc) and
    Load('glDepthMask', @glDepthMask) and
    Load('glDepthRangef', @glDepthRangef) and
    Load('glDetachShader', @glDetachShader) and
    Load('glDisable', @glDisable) and
    Load('glDisableVertexAttribArray', @glDisableVertexAttribArray) and
    Load('glDrawArrays', @glDrawArrays) and
    Load('glDrawElements', @glDrawElements) and
    Load('glEnable', @glEnable) and
    Load('glEnableVertexAttribArray', @glEnableVertexAttribArray) and
    Load('glFinish', @glFinish) and
    Load('glFlush', @glFlush) and
    Load('glFramebufferRenderbuffer', @glFramebufferRenderbuffer) and
    Load('glFramebufferTexture2D', @glFramebufferTexture2D) and
    Load('glFrontFace', @glFrontFace) and
    Load('glGenBuffers', @glGenBuffers) and
    Load('glGenerateMipmap', @glGenerateMipmap) and
    Load('glGenFramebuffers', @glGenFramebuffers) and
    Load('glGenRenderbuffers', @glGenRenderbuffers) and
    Load('glGenTextures', @glGenTextures) and
    Load('glGetActiveAttrib', @glGetActiveAttrib) and
    Load('glGetActiveUniform', @glGetActiveUniform) and
    Load('glGetAttachedShaders', @glGetAttachedShaders) and
    Load('glGetAttribLocation', @glGetAttribLocation) and
    Load('glGetBooleanv', @glGetBooleanv) and
    Load('glGetBufferParameteriv', @glGetBufferParameteriv) and
    Load('glGetError', @glGetError) and
    Load('glGetFloatv', @glGetFloatv) and
    Load('glGetFramebufferAttachmentParameteriv', @glGetFramebufferAttachmentParameteriv) and
    Load('glGetIntegerv', @glGetIntegerv) and
    Load('glGetProgramiv', @glGetProgramiv) and
    Load('glGetProgramInfoLog', @glGetProgramInfoLog) and
    Load('glGetRenderbufferParameteriv', @glGetRenderbufferParameteriv) and
    Load('glGetShaderiv', @glGetShaderiv) and
    Load('glGetShaderInfoLog', @glGetShaderInfoLog) and
    Load('glGetShaderPrecisionFormat', @glGetShaderPrecisionFormat) and
    Load('glGetShaderSource', @glGetShaderSource) and
    Load('glGetString', @glGetString) and
    Load('glGetTexParameterfv', @glGetTexParameterfv) and
    Load('glGetTexParameteriv', @glGetTexParameteriv) and
    Load('glGetUniformfv', @glGetUniformfv) and
    Load('glGetUniformiv', @glGetUniformiv) and
    Load('glGetUniformLocation', @glGetUniformLocation) and
    Load('glGetVertexAttribfv', @glGetVertexAttribfv) and
    Load('glGetVertexAttribiv', @glGetVertexAttribiv) and
    Load('glGetVertexAttribPointerv', @glGetVertexAttribPointerv) and
    Load('glHint', @glHint) and
    Load('glIsBuffer', @glIsBuffer) and
    Load('glIsEnabled', @glIsEnabled) and
    Load('glIsFramebuffer', @glIsFramebuffer) and
    Load('glIsProgram', @glIsProgram) and
    Load('glIsRenderbuffer', @glIsRenderbuffer) and
    Load('glIsShader', @glIsShader) and
    Load('glIsTexture', @glIsTexture) and
    Load('glLineWidth', @glLineWidth) and
    Load('glLinkProgram', @glLinkProgram) and
    Load('glPixelStorei', @glPixelStorei) and
    Load('glPolygonOffset', @glPolygonOffset) and
    Load('glReadPixels', @glReadPixels) and
    Load('glReleaseShaderCompiler', @glReleaseShaderCompiler) and
    Load('glRenderbufferStorage', @glRenderbufferStorage) and
    Load('glSampleCoverage', @glSampleCoverage) and
    Load('glScissor', @glScissor) and
    Load('glShaderBinary', @glShaderBinary) and
    Load('glShaderSource', @glShaderSource) and
    Load('glStencilFunc', @glStencilFunc) and
    Load('glStencilFuncSeparate', @glStencilFuncSeparate) and
    Load('glStencilMask', @glStencilMask) and
    Load('glStencilMaskSeparate', @glStencilMaskSeparate) and
    Load('glStencilOp', @glStencilOp) and
    Load('glStencilOpSeparate', @glStencilOpSeparate) and
    Load('glTexImage2D', @glTexImage2D) and
    Load('glTexParameterf', @glTexParameterf) and
    Load('glTexParameterfv', @glTexParameterfv) and
    Load('glTexParameteri', @glTexParameteri) and
    Load('glTexParameteriv', @glTexParameteriv) and
    Load('glTexSubImage2D', @glTexSubImage2D) and
    Load('glUniform1f', @glUniform1f) and
    Load('glUniform1fv', @glUniform1fv) and
    Load('glUniform1i', @glUniform1i) and
    Load('glUniform1iv', @glUniform1iv) and
    Load('glUniform2f', @glUniform2f) and
    Load('glUniform2fv', @glUniform2fv) and
    Load('glUniform2i', @glUniform2i) and
    Load('glUniform2iv', @glUniform2iv) and
    Load('glUniform3f', @glUniform3f) and
    Load('glUniform3fv', @glUniform3fv) and
    Load('glUniform3i', @glUniform3i) and
    Load('glUniform3iv', @glUniform3iv) and
    Load('glUniform4f', @glUniform4f) and
    Load('glUniform4fv', @glUniform4fv) and
    Load('glUniform4i', @glUniform4i) and
    Load('glUniform4iv', @glUniform4iv) and
    Load('glUniformMatrix2fv', @glUniformMatrix2fv) and
    Load('glUniformMatrix3fv', @glUniformMatrix3fv) and
    Load('glUniformMatrix4fv', @glUniformMatrix4fv) and
    Load('glUseProgram', @glUseProgram) and
    Load('glValidateProgram', @glValidateProgram) and
    Load('glVertexAttrib1f', @glVertexAttrib1f) and
    Load('glVertexAttrib1fv', @glVertexAttrib1fv) and
    Load('glVertexAttrib2f', @glVertexAttrib2f) and
    Load('glVertexAttrib2fv', @glVertexAttrib2fv) and
    Load('glVertexAttrib3f', @glVertexAttrib3f) and
    Load('glVertexAttrib3fv', @glVertexAttrib3fv) and
    Load('glVertexAttrib4f', @glVertexAttrib4f) and
    Load('glVertexAttrib4fv', @glVertexAttrib4fv) and
    Load('glVertexAttribPointer', @glVertexAttribPointer) and
    Load('glViewport', @glViewport);
  Result := LoadedSuccess;
end;

end.
