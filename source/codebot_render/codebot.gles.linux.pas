(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified July 2022                                  *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.gles.linux.txt> }
unit Codebot.GLES.Linux;

{$i render.inc}

interface

{$ifdef linux}
uses
  Codebot.System,
  Codebot.GLES;

function OpenGLInfoPrivate: IOpenGLInfo;
function OpenGLContextCreatePrivate(Window: GLwindow; const Params: TOpenGLParams): IOpenGLContext;
function OpenGLContextCurrentPrivate: IOpenGLContext;
{$endif}

implementation

{$ifdef linux}
uses
  X, XLib;

threadvar
  CurrentContext: IOpenGLContext;

const
  GLX_RGBA                              = 4;
  GLX_DOUBLEBUFFER                      = 5;
  GLX_RED_SIZE                          = 8;
  GLX_GREEN_SIZE                        = 9;
  GLX_BLUE_SIZE                         = 10;
  GLX_ALPHA_SIZE                        = 11;
  GLX_DEPTH_SIZE                        = 12;
  GLX_STENCIL_SIZE                      = 13;
  GLX_DRAWABLE_TYPE                     = $8010;
  GLX_RGBA_BIT                          = 1;
  GLX_RENDER_TYPE                       = $8011;
  GLX_WINDOW_BIT                        = 1;

  GLX_SAMPLE_BUFFERS                    = $186A0;
  GLX_SAMPLES                           = $186A1;

  GLX_CONTEXT_MAJOR_VERSION_ARB         = $2091;
  GLX_CONTEXT_MINOR_VERSION_ARB         = $2092;

type
  PXVisualInfo = Pointer;
  TGLXFBConfig = Pointer;
  PGLXFBConfig = ^TGLXFBConfig;

var
  glXGetProcAddress: function(name: PChar): Pointer; cdecl;
  glXChooseVisual: function(display: PDisplay; screen: Integer; attributes: PInteger): PXVisualInfo; cdecl;
  glXChooseFBConfig: function(display: PDisplay; screen: Integer; attributes: PInteger; out numElements: Integer): PGLXFBConfig; cdecl;
  glXCreateContext: function(display: PDisplay; visual: PXVisualInfo; share: GLcontext; direct: LongBool): GLcontext; cdecl;
  glXCreateContextAttribsARB: function(display: PDisplay; config: TGLXFBConfig; share: GLcontext; direct: LongBool; attributes: PInteger): GLcontext; cdecl;
  glXMakeCurrent: function(display: PDisplay; drawable: TWindow; ctx: GLcontext): LongBool; cdecl;
  glXGetCurrentContext: function: GLcontext; cdecl;
  glXQueryExtensionsString: function(display: PDisplay; screen: Integer): PChar; cdecl;
  glXSwapBuffers: procedure(display: PDisplay; drawable: TWindow); cdecl;
  glXDestroyContext: procedure(display: PDisplay; context: GLcontext); cdecl;
  glXSwapIntervalEXT: function(display: PDisplay; drawable: TWindow; interval: Integer): Integer; cdecl;

{ TOpenGLInfo }

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
  Display: PDisplay;
  Screen: Integer;

procedure Init;
const
  LibName = 'libGL.so.1';
var
  Obj: TOpenGLInfo;
  Lib: HModule;

  function LoadDirect(Name: string; out Proc: Pointer): Boolean;
  begin
    Proc := LibraryGetProc(Lib, Name);
    Result := Proc <> nil;
  end;

  function LoadIndirect(Name: string; out Proc: Pointer): Boolean;
  begin
    Proc := glXGetProcAddress(PChar(Name));
    Result := Proc <> nil;
  end;

const
  OrdZero = Ord('0');
var
  Window: TWindow;
  Attributes: array of Integer;
  Config: PGLXFBConfig;
  Context: GLcontext;
  N: Integer;
begin
  if Info <> nil then
    Exit;
  Info := TOpenGLInfo.Create;
  Obj := Info as TOpenGLInfo;
  Lib := LibraryLoad(LibName);
  if Lib = 0 then
    Exit;
  if LoadDirect('glXGetProcAddress', @glXGetProcAddress) and
    LoadDirect('glXChooseFBConfig', @glXChooseFBConfig) and
    LoadDirect('glXCreateContextAttribsARB', @glXCreateContextAttribsARB) and
    LoadDirect('glXMakeCurrent', @glXMakeCurrent) and
    LoadDirect('glXGetCurrentContext', @glXGetCurrentContext) and
    LoadDirect('glXQueryExtensionsString', @glXQueryExtensionsString) and
    LoadDirect('glXSwapBuffers', @glXSwapBuffers) and
    LoadDirect('glXDestroyContext', @glXDestroyContext) then
  begin
    Display := XOpenDisplay(nil);
    if Display = nil then
      Exit;
    Screen := DefaultScreen(Display);
    Window := XCreateSimpleWindow(Display, DefaultRootWindow(Display), 10, 10, 100, 100, 0, 0, 0);
    if Window <> 0 then
    try
      Attributes := [
        GLX_RENDER_TYPE, GLX_RGBA_BIT,
        GLX_DRAWABLE_TYPE, GLX_WINDOW_BIT,
        GLX_DOUBLEBUFFER, 1,
        GLX_RED_SIZE, 8,
        GLX_GREEN_SIZE, 8,
        GLX_BLUE_SIZE, 8,
        GLX_ALPHA_SIZE, 8,
        0];
      Config := glXChooseFBConfig(Display, Screen, @Attributes[0], N);
      if Config = nil then
        Exit;
      try
        Attributes := [
          GLX_CONTEXT_MAJOR_VERSION_ARB, 2,
          GLX_CONTEXT_MINOR_VERSION_ARB, 1,
          0];
        Context := glXCreateContextAttribsARB(Display, Config^, nil, True, @Attributes[0]);
        if Context = nil then
          Exit;
        try
          if glXMakeCurrent(Display, Window, Context) then
          try
            if LoadIndirect('glGetString', @glGetString) and
               LoadIndirect('glGetIntegerv', @glGetIntegerv) then
            begin
              Obj.FRenderer := glGetString(GL_RENDERER);
              Obj.FVendor := glGetString(GL_VENDOR);
              Obj.FVersion := glGetString(GL_VERSION);
              Obj.FExtensions := glGetString(GL_EXTENSIONS);
              Obj.FExtensions := Obj.FExtensions + ' ' + glXQueryExtensionsString(Display, Screen);
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
              if Obj.FExtensions.IndexOf('GLX_EXT_swap_control') > 1 then
                LoadIndirect('glXSwapIntervalEXT', @glXSwapIntervalEXT);
              Obj.FIsValid := (Obj.FMajorMinor > '2.0') or (Obj.FExtensions.IndexOf('ES2_compatibility') > -1);
              Obj.FIsValid := Obj.FIsValid and
                LoadIndirect('glActiveTexture', @glActiveTexture) and
                LoadIndirect('glAttachShader', @glAttachShader) and
                LoadIndirect('glBindAttribLocation', @glBindAttribLocation) and
                LoadIndirect('glBindBuffer', @glBindBuffer) and
                LoadIndirect('glBindFramebuffer', @glBindFramebuffer) and
                LoadIndirect('glBindRenderbuffer', @glBindRenderbuffer) and
                LoadIndirect('glBindTexture', @glBindTexture) and
                LoadIndirect('glBlendColor', @glBlendColor) and
                LoadIndirect('glBlendEquation', @glBlendEquation) and
                LoadIndirect('glBlendEquationSeparate', @glBlendEquationSeparate) and
                LoadIndirect('glBlendFunc', @glBlendFunc) and
                LoadIndirect('glBlendFuncSeparate', @glBlendFuncSeparate) and
                LoadIndirect('glBufferData', @glBufferData) and
                LoadIndirect('glBufferSubData', @glBufferSubData) and
                LoadIndirect('glCheckFramebufferStatus', @glCheckFramebufferStatus) and
                LoadIndirect('glClear', @glClear) and
                LoadIndirect('glClearColor', @glClearColor) and
                LoadIndirect('glClearDepthf', @glClearDepthf) and
                LoadIndirect('glClearStencil', @glClearStencil) and
                LoadIndirect('glColorMask', @glColorMask) and
                LoadIndirect('glCompileShader', @glCompileShader) and
                LoadIndirect('glCompressedTexImage2D', @glCompressedTexImage2D) and
                LoadIndirect('glCompressedTexSubImage2D', @glCompressedTexSubImage2D) and
                LoadIndirect('glCopyTexImage2D', @glCopyTexImage2D) and
                LoadIndirect('glCopyTexSubImage2D', @glCopyTexSubImage2D) and
                LoadIndirect('glCreateProgram', @glCreateProgram) and
                LoadIndirect('glCreateShader', @glCreateShader) and
                LoadIndirect('glCullFace', @glCullFace) and
                LoadIndirect('glDeleteBuffers', @glDeleteBuffers) and
                LoadIndirect('glDeleteFramebuffers', @glDeleteFramebuffers) and
                LoadIndirect('glDeleteProgram', @glDeleteProgram) and
                LoadIndirect('glDeleteRenderbuffers', @glDeleteRenderbuffers) and
                LoadIndirect('glDeleteShader', @glDeleteShader) and
                LoadIndirect('glDeleteTextures', @glDeleteTextures) and
                LoadIndirect('glDepthFunc', @glDepthFunc) and
                LoadIndirect('glDepthMask', @glDepthMask) and
                LoadIndirect('glDepthRangef', @glDepthRangef) and
                LoadIndirect('glDetachShader', @glDetachShader) and
                LoadIndirect('glDisable', @glDisable) and
                LoadIndirect('glDisableVertexAttribArray', @glDisableVertexAttribArray) and
                LoadIndirect('glDrawArrays', @glDrawArrays) and
                LoadIndirect('glDrawElements', @glDrawElements) and
                LoadIndirect('glEnable', @glEnable) and
                LoadIndirect('glEnableVertexAttribArray', @glEnableVertexAttribArray) and
                LoadIndirect('glFinish', @glFinish) and
                LoadIndirect('glFlush', @glFlush) and
                LoadIndirect('glFramebufferRenderbuffer', @glFramebufferRenderbuffer) and
                LoadIndirect('glFramebufferTexture2D', @glFramebufferTexture2D) and
                LoadIndirect('glFrontFace', @glFrontFace) and
                LoadIndirect('glGenBuffers', @glGenBuffers) and
                LoadIndirect('glGenerateMipmap', @glGenerateMipmap) and
                LoadIndirect('glGenFramebuffers', @glGenFramebuffers) and
                LoadIndirect('glGenRenderbuffers', @glGenRenderbuffers) and
                LoadIndirect('glGenTextures', @glGenTextures) and
                LoadIndirect('glGetActiveAttrib', @glGetActiveAttrib) and
                LoadIndirect('glGetActiveUniform', @glGetActiveUniform) and
                LoadIndirect('glGetAttachedShaders', @glGetAttachedShaders) and
                LoadIndirect('glGetAttribLocation', @glGetAttribLocation) and
                LoadIndirect('glGetBooleanv', @glGetBooleanv) and
                LoadIndirect('glGetBufferParameteriv', @glGetBufferParameteriv) and
                LoadIndirect('glGetError', @glGetError) and
                LoadIndirect('glGetFloatv', @glGetFloatv) and
                LoadIndirect('glGetFramebufferAttachmentParameteriv', @glGetFramebufferAttachmentParameteriv) and
                LoadIndirect('glGetIntegerv', @glGetIntegerv) and
                LoadIndirect('glGetProgramiv', @glGetProgramiv) and
                LoadIndirect('glGetProgramInfoLog', @glGetProgramInfoLog) and
                LoadIndirect('glGetRenderbufferParameteriv', @glGetRenderbufferParameteriv) and
                LoadIndirect('glGetShaderiv', @glGetShaderiv) and
                LoadIndirect('glGetShaderInfoLog', @glGetShaderInfoLog) and
                LoadIndirect('glGetShaderPrecisionFormat', @glGetShaderPrecisionFormat) and
                LoadIndirect('glGetShaderSource', @glGetShaderSource) and
                LoadIndirect('glGetString', @glGetString) and
                LoadIndirect('glGetTexParameterfv', @glGetTexParameterfv) and
                LoadIndirect('glGetTexParameteriv', @glGetTexParameteriv) and
                LoadIndirect('glGetUniformfv', @glGetUniformfv) and
                LoadIndirect('glGetUniformiv', @glGetUniformiv) and
                LoadIndirect('glGetUniformLocation', @glGetUniformLocation) and
                LoadIndirect('glGetVertexAttribfv', @glGetVertexAttribfv) and
                LoadIndirect('glGetVertexAttribiv', @glGetVertexAttribiv) and
                LoadIndirect('glGetVertexAttribPointerv', @glGetVertexAttribPointerv) and
                LoadIndirect('glHint', @glHint) and
                LoadIndirect('glIsBuffer', @glIsBuffer) and
                LoadIndirect('glIsEnabled', @glIsEnabled) and
                LoadIndirect('glIsFramebuffer', @glIsFramebuffer) and
                LoadIndirect('glIsProgram', @glIsProgram) and
                LoadIndirect('glIsRenderbuffer', @glIsRenderbuffer) and
                LoadIndirect('glIsShader', @glIsShader) and
                LoadIndirect('glIsTexture', @glIsTexture) and
                LoadIndirect('glLineWidth', @glLineWidth) and
                LoadIndirect('glLinkProgram', @glLinkProgram) and
                LoadIndirect('glPixelStorei', @glPixelStorei) and
                LoadIndirect('glPolygonOffset', @glPolygonOffset) and
                LoadIndirect('glReadPixels', @glReadPixels) and
                LoadIndirect('glReleaseShaderCompiler', @glReleaseShaderCompiler) and
                LoadIndirect('glRenderbufferStorage', @glRenderbufferStorage) and
                LoadIndirect('glSampleCoverage', @glSampleCoverage) and
                LoadIndirect('glScissor', @glScissor) and
                LoadIndirect('glShaderBinary', @glShaderBinary) and
                LoadIndirect('glShaderSource', @glShaderSource) and
                LoadIndirect('glStencilFunc', @glStencilFunc) and
                LoadIndirect('glStencilFuncSeparate', @glStencilFuncSeparate) and
                LoadIndirect('glStencilMask', @glStencilMask) and
                LoadIndirect('glStencilMaskSeparate', @glStencilMaskSeparate) and
                LoadIndirect('glStencilOp', @glStencilOp) and
                LoadIndirect('glStencilOpSeparate', @glStencilOpSeparate) and
                LoadIndirect('glTexImage2D', @glTexImage2D) and
                LoadIndirect('glTexParameterf', @glTexParameterf) and
                LoadIndirect('glTexParameterfv', @glTexParameterfv) and
                LoadIndirect('glTexParameteri', @glTexParameteri) and
                LoadIndirect('glTexParameteriv', @glTexParameteriv) and
                LoadIndirect('glTexSubImage2D', @glTexSubImage2D) and
                LoadIndirect('glUniform1f', @glUniform1f) and
                LoadIndirect('glUniform1fv', @glUniform1fv) and
                LoadIndirect('glUniform1i', @glUniform1i) and
                LoadIndirect('glUniform1iv', @glUniform1iv) and
                LoadIndirect('glUniform2f', @glUniform2f) and
                LoadIndirect('glUniform2fv', @glUniform2fv) and
                LoadIndirect('glUniform2i', @glUniform2i) and
                LoadIndirect('glUniform2iv', @glUniform2iv) and
                LoadIndirect('glUniform3f', @glUniform3f) and
                LoadIndirect('glUniform3fv', @glUniform3fv) and
                LoadIndirect('glUniform3i', @glUniform3i) and
                LoadIndirect('glUniform3iv', @glUniform3iv) and
                LoadIndirect('glUniform4f', @glUniform4f) and
                LoadIndirect('glUniform4fv', @glUniform4fv) and
                LoadIndirect('glUniform4i', @glUniform4i) and
                LoadIndirect('glUniform4iv', @glUniform4iv) and
                LoadIndirect('glUniformMatrix2fv', @glUniformMatrix2fv) and
                LoadIndirect('glUniformMatrix3fv', @glUniformMatrix3fv) and
                LoadIndirect('glUniformMatrix4fv', @glUniformMatrix4fv) and
                LoadIndirect('glUseProgram', @glUseProgram) and
                LoadIndirect('glValidateProgram', @glValidateProgram) and
                LoadIndirect('glVertexAttrib1f', @glVertexAttrib1f) and
                LoadIndirect('glVertexAttrib1fv', @glVertexAttrib1fv) and
                LoadIndirect('glVertexAttrib2f', @glVertexAttrib2f) and
                LoadIndirect('glVertexAttrib2fv', @glVertexAttrib2fv) and
                LoadIndirect('glVertexAttrib3f', @glVertexAttrib3f) and
                LoadIndirect('glVertexAttrib3fv', @glVertexAttrib3fv) and
                LoadIndirect('glVertexAttrib4f', @glVertexAttrib4f) and
                LoadIndirect('glVertexAttrib4fv', @glVertexAttrib4fv) and
                LoadIndirect('glVertexAttribPointer', @glVertexAttribPointer) and
                LoadIndirect('glViewport', @glViewport);
              {$ifdef gles3}
              Obj.FIsValid := Obj.FIsValid and ((Obj.FMajorMinor > '3.3') or (Obj.FExtensions.IndexOf('ES3_compatibility') > -1));
              Obj.FIsValid := Obj.FIsValid and
                LoadIndirect('glReadBuffer', @glReadBuffer) and
                LoadIndirect('glDrawRangeElements', @glDrawRangeElements) and
                LoadIndirect('glTexImage3D', @glTexImage3D) and
                LoadIndirect('glTexSubImage3D', @glTexSubImage3D) and
                LoadIndirect('glCopyTexSubImage3D', @glCopyTexSubImage3D) and
                LoadIndirect('glCompressedTexImage3D', @glCompressedTexImage3D) and
                LoadIndirect('glCompressedTexSubImage3D', @glCompressedTexSubImage3D) and
                LoadIndirect('glGenQueries', @glGenQueries) and
                LoadIndirect('glDeleteQueries', @glDeleteQueries) and
                LoadIndirect('glIsQuery', @glIsQuery) and
                LoadIndirect('glBeginQuery', @glBeginQuery) and
                LoadIndirect('glEndQuery', @glEndQuery) and
                LoadIndirect('glGetQueryiv', @glGetQueryiv) and
                LoadIndirect('glGetQueryObjectuiv', @glGetQueryObjectuiv) and
                LoadIndirect('glUnmapBuffer', @glUnmapBuffer) and
                LoadIndirect('glGetBufferPointerv', @glGetBufferPointerv) and
                LoadIndirect('glDrawBuffers', @glDrawBuffers) and
                LoadIndirect('glUniformMatrix2x3fv', @glUniformMatrix2x3fv) and
                LoadIndirect('glUniformMatrix3x2fv', @glUniformMatrix3x2fv) and
                LoadIndirect('glUniformMatrix2x4fv', @glUniformMatrix2x4fv) and
                LoadIndirect('glUniformMatrix4x2fv', @glUniformMatrix4x2fv) and
                LoadIndirect('glUniformMatrix3x4fv', @glUniformMatrix3x4fv) and
                LoadIndirect('glUniformMatrix4x3fv', @glUniformMatrix4x3fv) and
                LoadIndirect('glBlitFramebuffer', @glBlitFramebuffer) and
                LoadIndirect('glRenderbufferStorageMultisample', @glRenderbufferStorageMultisample) and
                LoadIndirect('glFramebufferTextureLayer', @glFramebufferTextureLayer) and
                LoadIndirect('glMapBufferRange', @glMapBufferRange) and
                LoadIndirect('glFlushMappedBufferRange', @glFlushMappedBufferRange) and
                LoadIndirect('glBindVertexArray', @glBindVertexArray) and
                LoadIndirect('glDeleteVertexArrays', @glDeleteVertexArrays) and
                LoadIndirect('glGenVertexArrays', @glGenVertexArrays) and
                LoadIndirect('glIsVertexArray', @glIsVertexArray) and
                LoadIndirect('glGetIntegeri_v', @glGetIntegeri_v) and
                LoadIndirect('glBeginTransformFeedback', @glBeginTransformFeedback) and
                LoadIndirect('glEndTransformFeedback', @glEndTransformFeedback) and
                LoadIndirect('glBindBufferRange', @glBindBufferRange) and
                LoadIndirect('glBindBufferBase', @glBindBufferBase) and
                LoadIndirect('glTransformFeedbackVaryings', @glTransformFeedbackVaryings) and
                LoadIndirect('glGetTransformFeedbackVarying', @glGetTransformFeedbackVarying) and
                LoadIndirect('glVertexAttribIPointer', @glVertexAttribIPointer) and
                LoadIndirect('glGetVertexAttribIiv', @glGetVertexAttribIiv) and
                LoadIndirect('glGetVertexAttribIuiv', @glGetVertexAttribIuiv) and
                LoadIndirect('glVertexAttribI4i', @glVertexAttribI4i) and
                LoadIndirect('glVertexAttribI4ui', @glVertexAttribI4ui) and
                LoadIndirect('glVertexAttribI4iv', @glVertexAttribI4iv) and
                LoadIndirect('glVertexAttribI4uiv', @glVertexAttribI4uiv) and
                LoadIndirect('glGetUniformuiv', @glGetUniformuiv) and
                LoadIndirect('glGetFragDataLocation', @glGetFragDataLocation) and
                LoadIndirect('glUniform1ui', @glUniform1ui) and
                LoadIndirect('glUniform2ui', @glUniform2ui) and
                LoadIndirect('glUniform3ui', @glUniform3ui) and
                LoadIndirect('glUniform4ui', @glUniform4ui) and
                LoadIndirect('glUniform1uiv', @glUniform1uiv) and
                LoadIndirect('glUniform2uiv', @glUniform2uiv) and
                LoadIndirect('glUniform3uiv', @glUniform3uiv) and
                LoadIndirect('glUniform4uiv', @glUniform4uiv) and
                LoadIndirect('glClearBufferiv', @glClearBufferiv) and
                LoadIndirect('glClearBufferuiv', @glClearBufferuiv) and
                LoadIndirect('glClearBufferfv', @glClearBufferfv) and
                LoadIndirect('glClearBufferfi', @glClearBufferfi) and
                LoadIndirect('glGetStringi', @glGetStringi) and
                LoadIndirect('glCopyBufferSubData', @glCopyBufferSubData) and
                LoadIndirect('glGetUniformIndices', @glGetUniformIndices) and
                LoadIndirect('glGetActiveUniformsiv', @glGetActiveUniformsiv) and
                LoadIndirect('glGetUniformBlockIndex', @glGetUniformBlockIndex) and
                LoadIndirect('glGetActiveUniformBlockiv', @glGetActiveUniformBlockiv) and
                LoadIndirect('glGetActiveUniformBlockName', @glGetActiveUniformBlockName) and
                LoadIndirect('glUniformBlockBinding', @glUniformBlockBinding) and
                LoadIndirect('glDrawArraysInstanced', @glDrawArraysInstanced) and
                LoadIndirect('glDrawElementsInstanced', @glDrawElementsInstanced) and
                LoadIndirect('glFenceSync', @glFenceSync) and
                LoadIndirect('glIsSync', @glIsSync) and
                LoadIndirect('glDeleteSync', @glDeleteSync) and
                LoadIndirect('glClientWaitSync', @glClientWaitSync) and
                LoadIndirect('glWaitSync', @glWaitSync) and
                LoadIndirect('glGetInteger64v', @glGetInteger64v) and
                LoadIndirect('glGetSynciv', @glGetSynciv) and
                LoadIndirect('glGetInteger64i_v', @glGetInteger64i_v) and
                LoadIndirect('glGetBufferParameteri64v', @glGetBufferParameteri64v) and
                LoadIndirect('glGenSamplers', @glGenSamplers) and
                LoadIndirect('glDeleteSamplers', @glDeleteSamplers) and
                LoadIndirect('glIsSampler', @glIsSampler) and
                LoadIndirect('glBindSampler', @glBindSampler) and
                LoadIndirect('glSamplerParameteri', @glSamplerParameteri) and
                LoadIndirect('glSamplerParameteriv', @glSamplerParameteriv) and
                LoadIndirect('glSamplerParameterf', @glSamplerParameterf) and
                LoadIndirect('glSamplerParameterfv', @glSamplerParameterfv) and
                LoadIndirect('glGetSamplerParameteriv', @glGetSamplerParameteriv) and
                LoadIndirect('glGetSamplerParameterfv', @glGetSamplerParameterfv) and
                LoadIndirect('glVertexAttribDivisor', @glVertexAttribDivisor) and
                LoadIndirect('glBindTransformFeedback', @glBindTransformFeedback) and
                LoadIndirect('glDeleteTransformFeedbacks', @glDeleteTransformFeedbacks) and
                LoadIndirect('glGenTransformFeedbacks', @glGenTransformFeedbacks) and
                LoadIndirect('glIsTransformFeedback', @glIsTransformFeedback) and
                LoadIndirect('glPauseTransformFeedback', @glPauseTransformFeedback) and
                LoadIndirect('glResumeTransformFeedback', @glResumeTransformFeedback) and
                LoadIndirect('glGetProgramBinary', @glGetProgramBinary) and
                LoadIndirect('glProgramBinary', @glProgramBinary) and
                LoadIndirect('glProgramParameteri', @glProgramParameteri) and
                LoadIndirect('glInvalidateFramebuffer', @glInvalidateFramebuffer) and
                LoadIndirect('glInvalidateSubFramebuffer', @glInvalidateSubFramebuffer) and
                LoadIndirect('glTexStorage2D', @glTexStorage2D) and
                LoadIndirect('glTexStorage3D', @glTexStorage3D) and
                LoadIndirect('glGetInternalformativ', @glGetInternalformativ);
              {$endif}
            end;
          finally
            glXMakeCurrent(Display, 0, nil);
          end;
        finally
          glXDestroyContext(Display, Context);
        end;
      finally
        XFree(Config);
      end;
    finally
      XDestroyWindow(Display, Window);
    end;
  end;
end;

function OpenGLInfoPrivate: IOpenGLInfo;
begin
  Init;
  Result := Info;
end;

type
  TOpenGLContext = class(TInterfacedObject, IOpenGLContext)
  private
    FContext: GLcontext;
    FCanRender: Boolean;
    FWindow: TWindow;
    FVSync: Boolean;
    FMutex: IMutex;
  public
    constructor Create(Context: GLcontext; Window: TWindow);
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

constructor TOpenGLContext.Create(Context: GLcontext; Window: TWindow);
begin
  inherited Create;
  FMutex := MutexCreate;
  FContext := Context;
  FWindow := Window;
  FVSync := True;
end;

destructor TOpenGLContext.Destroy;
begin
  SetCurrent(False);
  glXDestroyContext(Display, FContext);
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
  Result := glXGetCurrentContext = FContext;
end;

procedure TOpenGLContext.SetCurrent(const Value: Boolean);
begin
  Lock;
  try
    if Value = GetCurrent then
      Exit;
    if Value then
    begin
      glXMakeCurrent(Display, FWindow, FContext);
      CurrentContext := Self;
      if Assigned(glXSwapIntervalEXT) then
        if FVSync then
          glXSwapIntervalEXT(Display, FWindow, -1)
        else
          glXSwapIntervalEXT(Display, FWindow, 0);
    end
    else
    begin
      glXMakeCurrent(Display, 0, nil);
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
    if GetCurrent and Assigned(glXSwapIntervalEXT) then
      if FVSync then
        glXSwapIntervalEXT(Display, FWindow, -1)
      else
        glXSwapIntervalEXT(Display, FWindow, 0);
  finally
    Unlock;
  end;
end;

procedure TOpenGLContext.GetSize(out Width, Height: Integer);
var
  R: TWindow;
  X, Y: Integer;
  W, H, B, D: LongWord;
begin
  Lock;
  try
    XGetGeometry(Display, FWindow, @R, @X, @Y, @W, @H, @B, @D);
    Width := W;
    Height := H;
  finally
    Unlock;
  end;
end;

procedure TOpenGLContext.Flip;
begin
  glXSwapBuffers(Display, FWindow);
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
  Attributes: IntArray;
  Config: PGLXFBConfig;
  Context: GLcontext;
  Multi: Boolean;
  N: Integer;
begin
  Result := nil;
  if not OpenGLInfoPrivate.IsValid then
    Exit;
  Attributes := [
    GLX_RENDER_TYPE, GLX_RGBA_BIT,
    GLX_DRAWABLE_TYPE, GLX_WINDOW_BIT,
    GLX_DOUBLEBUFFER, 1,
    GLX_RED_SIZE, 8,
    GLX_GREEN_SIZE, 8,
    GLX_BLUE_SIZE, 8,
    GLX_ALPHA_SIZE, 8];
  if Params.Depth > 0 then
  begin
    Attributes.Push(GLX_DEPTH_SIZE);
    Attributes.Push(Params.Depth);
  end;
  if Params.Stencil > 0 then
  begin
    Attributes.Push(GLX_STENCIL_SIZE);
    Attributes.Push(Params.Stencil);
  end;
  Multi := Params.MultiSampling and (Params.MultiSamples > 1);
  if Multi then
  begin
    Attributes.Push(GLX_SAMPLE_BUFFERS);
    Attributes.Push(1);
    Attributes.Push(GLX_SAMPLES);
    Attributes.Push(Params.MultiSamples);
  end;
  Attributes.Push(0);
  Config := glXChooseFBConfig(Display, Screen, @Attributes.Items[0], N);
  if (Config = nil) and Multi then
  begin
    Attributes.Pop;
    Attributes.Pop;
    Attributes.Pop;
    Attributes.Push(0);
    Config := glXChooseFBConfig(Display, Screen, @Attributes.Items[0], N);
  end;
  if Config = nil then
    Exit;
  try
    Attributes := [
      GLX_CONTEXT_MAJOR_VERSION_ARB, 2,
      GLX_CONTEXT_MINOR_VERSION_ARB, 1,
      0];
    Context := glXCreateContextAttribsARB(Display, Config^, nil, True, @Attributes.Items[0]);
    if Context = nil then
      Exit;
    if glXMakeCurrent(Display, Window, Context) then
    begin
      glXMakeCurrent(Display, 0, nil);
      Result := TOpenGLContext.Create(Context, Window);
    end
    else
      glXDestroyContext(Display, Context);
  finally
    if Config <> nil then
      XFree(Config);
  end;
end;

function OpenGLContextCurrentPrivate: IOpenGLContext;
begin
  Result := CurrentContext;
end;
{$endif}

end.

