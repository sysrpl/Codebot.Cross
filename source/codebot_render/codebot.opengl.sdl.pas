(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ Codebot.OpenGL.SDL provides the Codebot.OpenGL platform routines using SDL2,
  so it works the same on every platform SDL supports. Every OpenGL context
  is created by it, for the window of an SDL application and for the SDL
  window a TGraphicsBox places inside an LCL form.

  OpenGLContextCreate takes an SDL window cast to GLwindow. The window must
  have been created with SDL_WINDOW_OPENGL after calling OpenGLSetAttributes,
  because SDL chooses the pixel format of a window when it is created.

  OpenGLInfo creates a hidden window and context to find out what the
  hardware supports. SDL video is started for it if it is not running. }
unit Codebot.OpenGL.SDL;

{$i render.inc}

interface

uses
  Codebot.OpenGL;

{ OpenGLSetAttributes sets the SDL attributes for the version selected in
  render.inc and the buffer options in Params. Call it before creating a
  window for OpenGLContextCreate. }

procedure OpenGLSetAttributes(const Params: TOpenGLParams);

implementation

uses
  SysUtils,
  Codebot.System,
  Codebot.Interop.SDL2;

{ Functions missing from the interop unit }

function SDL_GL_GetCurrentContext: PSDL_GLContext; cdecl; external 'SDL2';
function SDL_GL_GetCurrentWindow: PSDL_Window; cdecl; external 'SDL2';
procedure SDL_GL_GetDrawableSize(window: PSDL_Window; out w, h: LongInt); cdecl; external 'SDL2';

procedure OpenGLSetAttributes(const Params: TOpenGLParams);
var
  Multi: Boolean;
begin
  SDL_GL_SetAttribute(SDL_GL_RED_SIZE, 8);
  SDL_GL_SetAttribute(SDL_GL_GREEN_SIZE, 8);
  SDL_GL_SetAttribute(SDL_GL_BLUE_SIZE, 8);
  SDL_GL_SetAttribute(SDL_GL_ALPHA_SIZE, 8);
  SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
  SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE, Params.Depth);
  SDL_GL_SetAttribute(SDL_GL_STENCIL_SIZE, Params.Stencil);
  Multi := Params.MultiSampling and (Params.MultiSamples > 1);
  if Multi then
  begin
    SDL_GL_SetAttribute(SDL_GL_MULTISAMPLEBUFFERS, 1);
    SDL_GL_SetAttribute(SDL_GL_MULTISAMPLESAMPLES, Params.MultiSamples);
  end
  else
  begin
    SDL_GL_SetAttribute(SDL_GL_MULTISAMPLEBUFFERS, 0);
    SDL_GL_SetAttribute(SDL_GL_MULTISAMPLESAMPLES, 0);
  end;
  { Request the version selected in render.inc }
  SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, OpenGLMajor);
  SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, OpenGLMinor);
  {$if defined(glesapi)}
  SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_ES);
  {$elseif defined(glcompat)}
  SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_COMPATIBILITY);
  {$else}
  SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_CORE);
  {$endif}
end;

function GetProc(Name: PChar): Pointer;
begin
  Result := SDL_GL_GetProcAddress(Name);
end;

{ TOpenGLInfo }

type
  TOpenGLInfo = class(TInterfacedObject, IOpenGLInfo)
  private
    FIsValid: Boolean;
    FMajor: Integer;
    FMinor: Integer;
    FMajorMinor: string;
    FRenderer: string;
    FVendor: string;
    FVersion: string;
    FExtensions: string;
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
  CurrentContext: IOpenGLContext;

{ Read the version numbers from a version string such as '4.6 (Core Profile)'
  or 'OpenGL ES 3.2 Mesa' }

procedure ParseVersion(const S: string; out Major, Minor: Integer);
var
  I: Integer;
begin
  Major := 0;
  Minor := 0;
  I := 1;
  while (I <= Length(S)) and (not (S[I] in ['0'..'9'])) do
    Inc(I);
  while (I <= Length(S)) and (S[I] in ['0'..'9']) do
  begin
    Major := Major * 10 + Ord(S[I]) - Ord('0');
    Inc(I);
  end;
  if (I > Length(S)) or (S[I] <> '.') then
    Exit;
  Inc(I);
  while (I <= Length(S)) and (S[I] in ['0'..'9']) do
  begin
    Minor := Minor * 10 + Ord(S[I]) - Ord('0');
    Inc(I);
  end;
end;

{ Core profiles do not support glGetString(GL_EXTENSIONS) }

function ReadExtensions: string;
{$if defined(glesapi) and not defined(gles30)}
begin
  Result := PChar(glGetString(GL_EXTENSIONS));
end;
{$else}
var
  Count, I: GLint;
begin
  Result := '';
  Count := 0;
  glGetIntegerv(GL_NUM_EXTENSIONS, @Count);
  for I := 0 to Count - 1 do
    if I = 0 then
      Result := PChar(glGetStringi(GL_EXTENSIONS, I))
    else
      Result := Result + ' ' + PChar(glGetStringi(GL_EXTENSIONS, I));
end;
{$endif}

{ Init loads the OpenGL functions using a hidden window and context. The
  context current on the calling thread before is made current again. }

procedure Init;
var
  Obj: TOpenGLInfo;
  Started: Boolean;
  PriorWindow: PSDL_Window;
  PriorContext: PSDL_GLContext;
  Window: PSDL_Window;
  Context: PSDL_GLContext;
  Params: TOpenGLParams;
begin
  if Info <> nil then
    Exit;
  Info := TOpenGLInfo.Create;
  Obj := Info as TOpenGLInfo;
  Started := SDL_WasInit(SDL_INIT_VIDEO) = 0;
  if Started and (SDL_InitSubSystem(SDL_INIT_VIDEO) < 0) then
    Exit;
  try
    PriorWindow := SDL_GL_GetCurrentWindow;
    PriorContext := SDL_GL_GetCurrentContext;
    Params := TOpenGLParams.Create;
    Params.MultiSampling := False;
    OpenGLSetAttributes(Params);
    Window := SDL_CreateWindow('', 0, 0, 16, 16, SDL_WINDOW_OPENGL or SDL_WINDOW_HIDDEN);
    if Window = nil then
      Exit;
    try
      Context := SDL_GL_CreateContext(Window);
      if Context = nil then
        Exit;
      try
        if SDL_GL_MakeCurrent(Window, Context) = 0 then
        try
          { Load every function belonging to the version selected in render.inc }
          Obj.FIsValid := OpenGLLoadFunctions(@GetProc);
          if Assigned(glGetString) then
          begin
            Obj.FRenderer := PChar(glGetString(GL_RENDERER));
            Obj.FVendor := PChar(glGetString(GL_VENDOR));
            Obj.FVersion := PChar(glGetString(GL_VERSION));
            ParseVersion(Obj.FVersion, Obj.FMajor, Obj.FMinor);
            Obj.FMajorMinor := IntToStr(Obj.FMajor) + '.' + IntToStr(Obj.FMinor);
            if Obj.FIsValid then
              Obj.FExtensions := ReadExtensions;
          end;
        finally
          SDL_GL_MakeCurrent(PriorWindow, PriorContext);
        end;
      finally
        SDL_GL_DeleteContext(Context);
      end;
    finally
      SDL_DestroyWindow(Window);
    end;
  finally
    if Started then
      SDL_QuitSubSystem(SDL_INIT_VIDEO);
  end;
end;

function OpenGLInfoPrivate: IOpenGLInfo;
begin
  Init;
  Result := Info;
end;

{ TOpenGLContext }

type
  TOpenGLContext = class(TInterfacedObject, IOpenGLContext)
  private
    FContext: PSDL_GLContext;
    FWindow: PSDL_Window;
    FCanRender: Boolean;
    FVSync: Boolean;
    FVertexArray: GLuint;
    FMutex: IMutex;
  public
    constructor Create(Context: PSDL_GLContext; Window: PSDL_Window);
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

constructor TOpenGLContext.Create(Context: PSDL_GLContext; Window: PSDL_Window);
begin
  inherited Create;
  FMutex := MutexCreate;
  FContext := Context;
  FWindow := Window;
  FVSync := True;
  FCanRender := True;
end;

destructor TOpenGLContext.Destroy;
begin
  SetCurrent(False);
  SDL_GL_DeleteContext(FContext);
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
  Result := SDL_GL_GetCurrentContext = FContext;
end;

procedure TOpenGLContext.SetCurrent(const Value: Boolean);
begin
  Lock;
  try
    if Value = GetCurrent then
      Exit;
    if Value then
    begin
      SDL_GL_MakeCurrent(FWindow, FContext);
      CurrentContext := Self;
      SDL_GL_SetSwapInterval(Ord(FVSync));
      {$ifndef glesapi}
      { Core profiles require a bound vertex array object to draw }
      if FVertexArray = 0 then
      begin
        glGenVertexArrays(1, @FVertexArray);
        glBindVertexArray(FVertexArray);
      end;
      {$endif}
    end
    else
    begin
      SDL_GL_MakeCurrent(FWindow, nil);
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
    if GetCurrent then
      SDL_GL_SetSwapInterval(Ord(FVSync));
  finally
    Unlock;
  end;
end;

procedure TOpenGLContext.GetSize(out Width, Height: Integer);
begin
  SDL_GL_GetDrawableSize(FWindow, Width, Height);
end;

procedure TOpenGLContext.Flip;
begin
  SDL_GL_SwapWindow(FWindow);
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

{ The context is created and released, so it can be made current on any thread }

function OpenGLContextCreatePrivate(Window: GLwindow; const Params: TOpenGLParams): IOpenGLContext;
var
  W: PSDL_Window;
  Context: PSDL_GLContext;
begin
  Result := nil;
  if not OpenGLInfoPrivate.IsValid then
    Exit;
  W := PSDL_Window(Window);
  if W = nil then
    Exit;
  OpenGLSetAttributes(Params);
  Context := SDL_GL_CreateContext(W);
  if Context = nil then
    Exit;
  SDL_GL_MakeCurrent(W, nil);
  Result := TOpenGLContext.Create(Context, W);
end;

function OpenGLContextCurrentPrivate: IOpenGLContext;
begin
  Result := CurrentContext;
end;

initialization
  OpenGLPlatformInfo := @OpenGLInfoPrivate;
  OpenGLPlatformContextCreate := @OpenGLContextCreatePrivate;
  OpenGLPlatformContextCurrent := @OpenGLContextCurrentPrivate;
end.
