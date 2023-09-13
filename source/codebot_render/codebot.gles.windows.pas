(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified July 2022                                  *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.gles.windows.txt> }
unit Codebot.GLES.Windows;

{$i render.inc}

interface

{$ifdef windows}
uses
  Codebot.System,
  Codebot.GLES;

function OpenGLInfoPrivate: IOpenGLInfo;
function OpenGLContextCreatePrivate(Window: GLwindow; const Params: TOpenGLParams): IOpenGLContext;
function OpenGLContextCurrentPrivate: IOpenGLContext;
{$endif}

implementation

{$ifdef windows}
uses
  Windows;

threadvar
  CurrentContext: IOpenGLContext;

{ WGL_EXT_EXTENSIONS_STRING }

var
  wglGetExtensionsString: function(DC: HDC): PChar; stdcall;

{ WGL_ARB_PIXEL_FORMAT }

var
  wglChoosePixelFormat: function(hdc: HDC; piAttribIList: PGLint;
    pfAttribFList: PGLfloat; nMaxFormats: GLuint; var iFormat: GLint;
    var nNumFormats: GLuint): BOOL; stdcall;

{ Accepted in the <piAttributes> parameter array of
  wglGetPixelFormatAttribivARB, and wglGetPixelFormatAttribfvARB, and
  as a type in the <piAttribIList> and <pfAttribFList> parameter
  arrays of wglChoosePixelFormatARB: }

const
  WGL_NUMBER_PIXEL_FORMATS                                     = $2000;
  WGL_DRAW_TO_WINDOW                                           = $2001;
  WGL_DRAW_TO_BITMAP                                           = $2002;
  WGL_ACCELERATION                                             = $2003;
  WGL_NEED_PALETTE                                             = $2004;
  WGL_NEED_SYSTEM_PALETTE                                      = $2005;
  WGL_SWAP_LAYER_BUFFERS                                       = $2006;
  WGL_SWAP_METHOD                                              = $2007;
  WGL_NUMBER_OVERLAYS                                          = $2008;
  WGL_NUMBER_UNDERLAYS                                         = $2009;
  WGL_TRANSPARENT                                              = $200A;
  WGL_TRANSPARENT_RED_VALUE                                    = $2037;
  WGL_TRANSPARENT_GREEN_VALUE                                  = $2038;
  WGL_TRANSPARENT_BLUE_VALUE                                   = $2039;
  WGL_TRANSPARENT_ALPHA_VALUE                                  = $203A;
  WGL_TRANSPARENT_INDEX_VALUE                                  = $203B;
  WGL_SHARE_DEPTH                                              = $200C;
  WGL_SHARE_STENCIL                                            = $200D;
  WGL_SHARE_ACCUM                                              = $200E;
  WGL_SUPPORT_GDI                                              = $200F;
  WGL_SUPPORT_OPENGL                                           = $2010;
  WGL_DOUBLE_BUFFER                                            = $2011;
  WGL_STEREO                                                   = $2012;
  WGL_PIXEL_TYPE                                               = $2013;
  WGL_COLOR_BITS                                               = $2014;
  WGL_RED_BITS                                                 = $2015;
  WGL_RED_SHIFT                                                = $2016;
  WGL_GREEN_BITS                                               = $2017;
  WGL_GREEN_SHIFT                                              = $2018;
  WGL_BLUE_BITS                                                = $2019;
  WGL_BLUE_SHIFT                                               = $201A;
  WGL_ALPHA_BITS                                               = $201B;
  WGL_ALPHA_SHIFT                                              = $201C;
  WGL_ACCUM_BITS                                               = $201D;
  WGL_ACCUM_RED_BITS                                           = $201E;
  WGL_ACCUM_GREEN_BITS                                         = $201F;
  WGL_ACCUM_BLUE_BITS                                          = $2020;
  WGL_ACCUM_ALPHA_BITS                                         = $2021;
  WGL_DEPTH_BITS                                               = $2022;
  WGL_STENCIL_BITS                                             = $2023;
  WGL_AUX_BUFFERS                                              = $2024;

{ Accepted as a value in the <piAttribIList> and <pfAttribFList>
  parameter arrays of wglChoosePixelFormatARB, and returned in the
  <piValues> parameter array of wglGetPixelFormatAttribivARB, and the
  <pfValues> parameter array of wglGetPixelFormatAttribfvARB: }

  WGL_NO_ACCELERATION                                          = $2025;
  WGL_GENERIC_ACCELERATION                                     = $2026;
  WGL_FULL_ACCELERATION                                        = $2027;

  WGL_SWAP_EXCHANGE                                            = $2028;
  WGL_SWAP_COPY                                                = $2029;
  WGL_SWAP_UNDEFINED                                           = $202A;

  WGL_TYPE_RGBA                                                = $202B;
  WGL_TYPE_COLORINDEX                                          = $202C;

{ WGL_ARB_MULTISAMPLE }

var
  {%H-}WGL_ARB_MULTISAMPLE: Boolean;

{ Accepted by the <piAttributes> parameter of
  wglGetPixelFormatAttribivEXT, wglGetPixelFormatAttribfvEXT, and
  the <piAttribIList> and <pfAttribIList> of wglChoosePixelFormatEXT: }

const
  WGL_SAMPLE_BUFFERS                                           = $2041;
  WGL_SAMPLES                                                  = $2042;

var
  WGL_EXT_SWAP_CONTROL: Boolean;

{ WGL_EXT_SWAP_CONTROL }

var
  wglSwapIntervalEXT: function(interval: GLint): GLboolean; stdcall;
  wglGetSwapIntervalEXT: function: GLint; stdcall;

type
  TOpenGLInfo = class(TInterfacedObject, IOpenGLInfo)
  public
    FIsValid: Boolean;
    FMajor: Integer;
    FMinor: Integer;
    FMajorMinor: string;
    FRenderer: string;
    FVendor: string;
    FVersion: string;
    FExtensions: string;
    function IsValid: Boolean;
    function Major: Integer;
    function Minor: Integer;
    function MajorMinor: string;
    function Renderer: string;
    function Vendor: string;
    function Version: string;
    function Extensions: string;
  end;

function TOpenGLInfo.IsValid: Boolean;
begin
  Result := FIsValid;
end;

function TOpenGLInfo.Major: Integer;
begin
  Result := FMajor;
end;

function TOpenGLInfo.Minor: Integer;
begin
  Result := FMinor;
end;

function TOpenGLInfo.MajorMinor: string;
begin
  Result := FMajorMinor;
end;

function TOpenGLInfo.Renderer: string;
begin
  Result := FRenderer;
end;

function TOpenGLInfo.Vendor: string;
begin
  Result := FVendor;
end;

function TOpenGLInfo.Version: string;
begin
  Result := FVersion;
end;

function TOpenGLInfo.Extensions: string;
begin
  Result := FExtensions;
end;

var
  Info: IOpenGLInfo;

function OpenGLInfoPrivate: IOpenGLInfo;

	function Load(const Name: string; out Proc: Pointer): Boolean;
  begin
    Proc := wglGetProcAddress(PChar(Name));
    Result := Proc <> nil;
  end;

const
  OrdZero = Ord('0');
var
  Obj: TOpenGLInfo;
  WindowClass: string;
  WindowName: string;
  WndClass: TWndClass;
  Wnd: HWND;
  Descriptor: TPixelFormatDescriptor;
  DC: HDC;
  RC: HGLRC;
  Format: GLint;
  A, B: string;
begin
  if Info <> nil then
  	Exit(Info);
	Info := TOpenGLInfo.Create;
  Result := Info;
  Obj := Result as TOpenGLInfo;
  WindowClass := 'OpenGLInfoPrivateClass';
  WindowName := 'OpenGLInfoPrivateWindow';
  with WndClass do
  begin
    FillChar(WndClass{%H-}, SizeOf(TWndClass), #0);
    style := CS_OWNDC or CS_DBLCLKS or CS_HREDRAW or CS_VREDRAW;
    lpfnWndProc := @DefWindowProc;
    lpszClassName := PChar(WindowClass);
    hInstance := System.HInstance;
  end;
  Wnd := CreateWindowEx(0, PChar(WindowClass), PChar(WindowName), WS_POPUP,
    0, 0, $FF, $FF, 0, 0, 0, nil);
  DC := GetDC(Wnd);
  FillChar(Descriptor{%H-}, SizeOf(TPixelFormatDescriptor), #0);
  with Descriptor do
  begin
    nSize := SizeOf(TPixelFormatDescriptor);
    nVersion := 1;
    dwFlags := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
    iPixelType := PFD_TYPE_RGBA;
    cColorBits := 24;
    cRedBits := 8;
    cGreenBits := 8;
    cBlueBits := 8;
    cAlphaBits := 8;
    cDepthBits := 24;
    cStencilBits := 8;
    iLayerType := PFD_MAIN_PLANE;
  end;
  Format := ChoosePixelFormat(DC, @Descriptor);
  if SetPixelFormat(DC, Format, @Descriptor) then
  begin
    RC := wglCreateContext(DC);
    if wglMakeCurrent(DC, RC) then
    begin
      Obj.FIsValid :=
      	Load('glGetString', @glGetString) and
				Load('glGetIntegerv', @glGetIntegerv) and
        Load('wglGetExtensionsStringARB', @wglGetExtensionsString) and
	      Load('wglChoosePixelFormatARB', @wglChoosePixelFormat);
      if Obj.FIsValid then
      begin
        Obj.FRenderer := glGetString(GL_RENDERER);
        Obj.FVendor := glGetString(GL_VENDOR);
        Obj.FVersion := glGetString(GL_VERSION);
        A := glGetString(GL_EXTENSIONS);
        B := wglGetExtensionsString(DC);
        Obj.FExtensions := A.Trim + ' ' + B.Trim;
        WGL_ARB_MULTISAMPLE := B.IndexOf('WGL_ARB_multisample') > -1;
        WGL_EXT_SWAP_CONTROL := B.IndexOf('WGL_EXT_swap_control') > -1;
        WGL_EXT_SWAP_CONTROL := WGL_EXT_SWAP_CONTROL and
	      	Load('wglSwapIntervalEXT', @wglSwapIntervalEXT);
  	      Load('wglGetSwapIntervalEXT', @wglGetSwapIntervalEXT);
        glGetIntegerv(GL_MAJOR_VERSION, @Obj.FMajor);
        glGetIntegerv(GL_MINOR_VERSION, @Obj.FMinor);
        if Obj.FVersion.Length > 2 then
        begin
          if (Obj.FMajor = 0) and (Obj.FVersion[1] in ['0'..'9']) then
            Obj.FMajor := Ord(Obj.FVersion[1]) -  OrdZero;
          if (Obj.FMinor = 0) and (Obj.FVersion[3] in ['0'..'9']) then
            Obj.FMinor := Ord(Obj.FVersion[3]) - OrdZero;
        end;
        Obj.FMajorMinor := Chr(Obj.FMajor + OrdZero) + '.' + Chr(Obj.FMinor + OrdZero);
        Obj.FIsValid := (Obj.FMajorMinor > '2.0') or (Obj.FExtensions.IndexOf('ES2_compatibility') > -1);
        Obj.FIsValid := Obj.FIsValid and
          Load('glActiveTexture', @glActiveTexture) and
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
        {$ifdef gles3}
        Obj.FIsValid := Obj.FIsValid and ((Obj.FMajorMinor > '3.3') or (Obj.FExtensions.IndexOf('ES3_compatibility') > -1));
        Obj.FIsValid := Obj.FIsValid and
          Load('glReadBuffer', @glReadBuffer) and
          Load('glDrawRangeElements', @glDrawRangeElements) and
          Load('glTexImage3D', @glTexImage3D) and
          Load('glTexSubImage3D', @glTexSubImage3D) and
          Load('glCopyTexSubImage3D', @glCopyTexSubImage3D) and
          Load('glCompressedTexImage3D', @glCompressedTexImage3D) and
          Load('glCompressedTexSubImage3D', @glCompressedTexSubImage3D) and
          Load('glGenQueries', @glGenQueries) and
          Load('glDeleteQueries', @glDeleteQueries) and
          Load('glIsQuery', @glIsQuery) and
          Load('glBeginQuery', @glBeginQuery) and
          Load('glEndQuery', @glEndQuery) and
          Load('glGetQueryiv', @glGetQueryiv) and
          Load('glGetQueryObjectuiv', @glGetQueryObjectuiv) and
          Load('glUnmapBuffer', @glUnmapBuffer) and
          Load('glGetBufferPointerv', @glGetBufferPointerv) and
          Load('glDrawBuffers', @glDrawBuffers) and
          Load('glUniformMatrix2x3fv', @glUniformMatrix2x3fv) and
          Load('glUniformMatrix3x2fv', @glUniformMatrix3x2fv) and
          Load('glUniformMatrix2x4fv', @glUniformMatrix2x4fv) and
          Load('glUniformMatrix4x2fv', @glUniformMatrix4x2fv) and
          Load('glUniformMatrix3x4fv', @glUniformMatrix3x4fv) and
          Load('glUniformMatrix4x3fv', @glUniformMatrix4x3fv) and
          Load('glBlitFramebuffer', @glBlitFramebuffer) and
          Load('glRenderbufferStorageMultisample', @glRenderbufferStorageMultisample) and
          Load('glFramebufferTextureLayer', @glFramebufferTextureLayer) and
          Load('glMapBufferRange', @glMapBufferRange) and
          Load('glFlushMappedBufferRange', @glFlushMappedBufferRange) and
          Load('glBindVertexArray', @glBindVertexArray) and
          Load('glDeleteVertexArrays', @glDeleteVertexArrays) and
          Load('glGenVertexArrays', @glGenVertexArrays) and
          Load('glIsVertexArray', @glIsVertexArray) and
          Load('glGetIntegeri_v', @glGetIntegeri_v) and
          Load('glBeginTransformFeedback', @glBeginTransformFeedback) and
          Load('glEndTransformFeedback', @glEndTransformFeedback) and
          Load('glBindBufferRange', @glBindBufferRange) and
          Load('glBindBufferBase', @glBindBufferBase) and
          Load('glTransformFeedbackVaryings', @glTransformFeedbackVaryings) and
          Load('glGetTransformFeedbackVarying', @glGetTransformFeedbackVarying) and
          Load('glVertexAttribIPointer', @glVertexAttribIPointer) and
          Load('glGetVertexAttribIiv', @glGetVertexAttribIiv) and
          Load('glGetVertexAttribIuiv', @glGetVertexAttribIuiv) and
          Load('glVertexAttribI4i', @glVertexAttribI4i) and
          Load('glVertexAttribI4ui', @glVertexAttribI4ui) and
          Load('glVertexAttribI4iv', @glVertexAttribI4iv) and
          Load('glVertexAttribI4uiv', @glVertexAttribI4uiv) and
          Load('glGetUniformuiv', @glGetUniformuiv) and
          Load('glGetFragDataLocation', @glGetFragDataLocation) and
          Load('glUniform1ui', @glUniform1ui) and
          Load('glUniform2ui', @glUniform2ui) and
          Load('glUniform3ui', @glUniform3ui) and
          Load('glUniform4ui', @glUniform4ui) and
          Load('glUniform1uiv', @glUniform1uiv) and
          Load('glUniform2uiv', @glUniform2uiv) and
          Load('glUniform3uiv', @glUniform3uiv) and
          Load('glUniform4uiv', @glUniform4uiv) and
          Load('glClearBufferiv', @glClearBufferiv) and
          Load('glClearBufferuiv', @glClearBufferuiv) and
          Load('glClearBufferfv', @glClearBufferfv) and
          Load('glClearBufferfi', @glClearBufferfi) and
          Load('glGetStringi', @glGetStringi) and
          Load('glCopyBufferSubData', @glCopyBufferSubData) and
          Load('glGetUniformIndices', @glGetUniformIndices) and
          Load('glGetActiveUniformsiv', @glGetActiveUniformsiv) and
          Load('glGetUniformBlockIndex', @glGetUniformBlockIndex) and
          Load('glGetActiveUniformBlockiv', @glGetActiveUniformBlockiv) and
          Load('glGetActiveUniformBlockName', @glGetActiveUniformBlockName) and
          Load('glUniformBlockBinding', @glUniformBlockBinding) and
          Load('glDrawArraysInstanced', @glDrawArraysInstanced) and
          Load('glDrawElementsInstanced', @glDrawElementsInstanced) and
          Load('glFenceSync', @glFenceSync) and
          Load('glIsSync', @glIsSync) and
          Load('glDeleteSync', @glDeleteSync) and
          Load('glClientWaitSync', @glClientWaitSync) and
          Load('glWaitSync', @glWaitSync) and
          Load('glGetInteger64v', @glGetInteger64v) and
          Load('glGetSynciv', @glGetSynciv) and
          Load('glGetInteger64i_v', @glGetInteger64i_v) and
          Load('glGetBufferParameteri64v', @glGetBufferParameteri64v) and
          Load('glGenSamplers', @glGenSamplers) and
          Load('glDeleteSamplers', @glDeleteSamplers) and
          Load('glIsSampler', @glIsSampler) and
          Load('glBindSampler', @glBindSampler) and
          Load('glSamplerParameteri', @glSamplerParameteri) and
          Load('glSamplerParameteriv', @glSamplerParameteriv) and
          Load('glSamplerParameterf', @glSamplerParameterf) and
          Load('glSamplerParameterfv', @glSamplerParameterfv) and
          Load('glGetSamplerParameteriv', @glGetSamplerParameteriv) and
          Load('glGetSamplerParameterfv', @glGetSamplerParameterfv) and
          Load('glVertexAttribDivisor', @glVertexAttribDivisor) and
          Load('glBindTransformFeedback', @glBindTransformFeedback) and
          Load('glDeleteTransformFeedbacks', @glDeleteTransformFeedbacks) and
          Load('glGenTransformFeedbacks', @glGenTransformFeedbacks) and
          Load('glIsTransformFeedback', @glIsTransformFeedback) and
          Load('glPauseTransformFeedback', @glPauseTransformFeedback) and
          Load('glResumeTransformFeedback', @glResumeTransformFeedback) and
          Load('glGetProgramBinary', @glGetProgramBinary) and
          Load('glProgramBinary', @glProgramBinary) and
          Load('glProgramParameteri', @glProgramParameteri) and
          Load('glInvalidateFramebuffer', @glInvalidateFramebuffer) and
          Load('glInvalidateSubFramebuffer', @glInvalidateSubFramebuffer) and
          Load('glTexStorage2D', @glTexStorage2D) and
          Load('glTexStorage3D', @glTexStorage3D) and
          Load('glGetInternalformativ', @glGetInternalformativ);
        {$endif}
      end;
      wglMakeCurrent(0, 0);
    end;
    wglDeleteContext(RC);
  end;
  ReleaseDC(Wnd, DC);
  DestroyWindow(Wnd);
end;

type
  TOpenGLContext = class(TInterfacedObject, IOpenGLContext)
  private
    FCanRender: Boolean;
    FContext: HGLRC;
    FWindow: HWND;
    FDevice: HDC;
    FVSync: Boolean;
    FMutex: IMutex;
  public
    constructor Create(Context: HGLRC; Window: HWND; Device: HDC);
    destructor Destroy; override;
    function GetCanRender: Boolean;
    procedure SetCanRender(const Value: Boolean);
    function GetCurrent: Boolean;
    procedure SetCurrent(const Value: Boolean);
    function GetVSync: Boolean;
    procedure SetVSync(const Value: Boolean);
    procedure GetSize(out Width, Height: Integer);
    procedure Flip;
    procedure MakeCurrent(Value: Boolean);
    procedure Lock;
    procedure Unlock;
  end;

constructor TOpenGLContext.Create(Context: HGLRC; Window: HWND; Device: HDC);
begin
  inherited Create;
  FMutex := MutexCreate;
  FContext := Context;
  FWindow := Window;
  FDevice := Device;
  FVSync := True;
end;

destructor TOpenGLContext.Destroy;
begin
  SetCurrent(False);
  wglDeleteContext(FContext);
  ReleaseDC(FWindow, FDevice);
  FMutex := nil;
  inherited Destroy;
end;

function TOpenGLContext.GetCanRender: Boolean;
begin
  Result := FCanRender;
end;

procedure TOpenGLContext.SetCanRender(const Value: Boolean);
begin
  FCanRender := Value;
end;

function TOpenGLContext.GetCurrent: Boolean;
begin
  Result := wglGetCurrentContext = FContext;
end;

procedure TOpenGLContext.SetCurrent(const Value: Boolean);
begin
  Lock;
  try
    if Value = GetCurrent then
      Exit;
    if Value then
    begin
      wglMakeCurrent(FDevice, FContext);
      CurrentContext := Self;
      if WGL_EXT_SWAP_CONTROL then
        if FVSync then
          wglSwapIntervalEXT(-1)
        else
          wglSwapIntervalEXT(0);
    end
    else
    begin
      wglMakeCurrent(0, 0);
      CurrentContext := nil;
    end;
  finally
    Unlock;
  end;
end;

function TOpenGLContext.GetVSync: Boolean;
begin
  Result := FVSync;
end;

procedure TOpenGLContext.SetVSync(const Value: Boolean);
begin
  Lock;
  try
    if Value = FVSync then
      Exit;
    FVSync := Value;
    if GetCurrent and WGL_EXT_SWAP_CONTROL then
        if FVSync then
          wglSwapIntervalEXT(-1)
        else
          wglSwapIntervalEXT(0);
  finally
    Unlock;
  end;
end;

procedure TOpenGLContext.GetSize(out Width, Height: Integer);
var
  R: TRect;
begin
  Lock;
  try
    Windows.GetWindowRect(FWindow, R{%H-});
    Width := R.Right - R.Left;
    Height := R.Bottom - R.Top;
  finally
    Unlock;
  end;
end;

procedure TOpenGLContext.Flip;
begin
  SwapBuffers(FDevice);
end;

procedure TOpenGLContext.MakeCurrent(Value: Boolean);
begin
  SetCurrent(Value);
end;

procedure TOpenGLContext.Lock;
begin
  FMutex.Lock;
end;

procedure TOpenGLContext.Unlock;
begin
  FMutex.Unlock;
end;

function OpenGLContextCreatePrivate(Window: GLwindow; const Params: TOpenGLParams): IOpenGLContext;
var
  Wnd: HWND;
  DC: HDC;
  RC: HGLRC;
  Attrib: IntArray;
  Format, NumFormat: GLint;
  Multi: Boolean;
begin
	Result := nil;
  if not OpenGLInfoPrivate.IsValid then
  	Exit;
  Wnd := HWND(Window);
  if Wnd = 0 then
  	Exit;
  DC := GetDC(Wnd);
  RC := 0;
  Attrib.Push(WGL_DRAW_TO_WINDOW); Attrib.Push(1);
  Attrib.Push(WGL_SUPPORT_OPENGL); Attrib.Push(1);
  Attrib.Push(WGL_DOUBLE_BUFFER); Attrib.Push(1);
  Attrib.Push(WGL_ACCELERATION); Attrib.Push(WGL_FULL_ACCELERATION);
  Attrib.Push(WGL_PIXEL_TYPE); Attrib.Push(WGL_TYPE_RGBA);
  Attrib.Push(WGL_COLOR_BITS); Attrib.Push(24);
  Attrib.Push(WGL_RED_BITS); Attrib.Push(8);
  Attrib.Push(WGL_GREEN_BITS); Attrib.Push(8);
  Attrib.Push(WGL_BLUE_BITS); Attrib.Push(8);
  Attrib.Push(WGL_ALPHA_BITS); Attrib.Push(8);
  Attrib.Push(WGL_DEPTH_BITS); Attrib.Push(Params.Depth);
  Attrib.Push(WGL_STENCIL_BITS); Attrib.Push(Params.Stencil);
  Multi := Params.MultiSampling and (Params.MultiSamples > 1);
  if Multi then
  begin
    Attrib.Push(WGL_SAMPLE_BUFFERS); Attrib.Push(1);
    Attrib.Push(WGL_SAMPLES); Attrib.Push(Params.MultiSamples);
  end;
  Attrib.Push(0);
  if wglChoosePixelFormat(DC, @Attrib.Items[0], nil, 1, Format{%H-}, NumFormat{%H-}) and SetPixelFormat(DC, Format, nil) then
  begin
    RC := wglCreateContext(DC);
    if RC <> 0 then
    	Result := TOpenGLContext.Create(RC, Wnd, DC);
  end;
  if (RC = 0) and Multi then
  begin
    Attrib.Pop;
    Attrib.Pop;
    Attrib.Pop;
    Attrib.Push(0);
    if wglChoosePixelFormat(DC, @Attrib.Items[0], nil, 1, Format, NumFormat) and SetPixelFormat(DC, Format, nil) then
    begin
      SetPixelFormat(DC, Format, nil);
      RC := wglCreateContext(DC);
      if RC <> 0 then
      	Result := TOpenGLContext.Create(RC, Wnd, DC);
    end
  end;
  if Result = nil then
  	if DC <> 0 then
    	ReleaseDC(Wnd, DC);
end;

function OpenGLContextCurrentPrivate: IOpenGLContext;
begin
  Result := CurrentContext;
end;
{$endif}

end.

