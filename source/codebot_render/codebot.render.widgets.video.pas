unit Codebot.Render.Widgets.Video;

{$i render.inc}

interface

{$ifdef videowidget}
uses
  SysUtils,
  Classes,
  CTypes,
  Codebot.System,
  Codebot.Graphics.Types,
  Codebot.OpenGL,
  Codebot.Interop.MPV,
  Codebot.Render.Graphics,
  Codebot.Render.Widgets,
  Codebot.Render.Widgets.Themes;

(* This unit is empty unless videowidget is defined in render.inc. Defining it
  links libmpv, which plays the video.

  TVideoWidget is a rectangle which plays a video. Set FileName, which can
  also be a url, and call Play. The video is scaled to fit the widget and
  keeps its shape, with black bars where it does not fill the widget.

  Pause holds the video where it is and Play continues it. Stop unloads the
  video, and Play then starts it from the beginning. Position is where the
  video is in seconds and can be set to seek, and Duration is how long the
  video is in seconds. Both are zero until the video has been loaded.

  When the video reaches its end OnPlayComplete fires. If AutoRestart is true
  the video then plays again from the beginning. Otherwise it stops with its
  last picture showing, and Play starts it again.

  The video is drawn by libmpv with OpenGL into a framebuffer, which is a
  render bitmap of the canvas, and the bitmap is drawn where the widget is.
  A new picture is taken from the player when the widget is painted, and its
  events are read then too, so OnPlayComplete does not fire and AutoRestart
  does not restart while the widget is hidden. Sound keeps playing.

  The sound of the video is played by libmpv through its own audio output,
  not through the audio of Codebot.Hardware. Volume is how loud it is from 0
  to 1, and Muted silences it without changing Volume. Both can be set
  before a video is loaded. HasAudio tells if the video which was loaded has
  any sound.

  Effect changes how the video looks. It is the source of a GLSL fragment
  shader which draws the video in one pass, using nothing but the picture of
  the video, and it is empty for no effect. The shader is written the way
  Shadertoy shaders are:

    void mainImage(out vec4 fragColor, in vec2 fragCoord)
    {
        vec2 uv = fragCoord / iResolution.xy;
        fragColor = vec4(texture2D(iChannel0, uv).bgr, 1.0);
    }

  iChannel0 is the picture of the video, iResolution is the size of the
  widget in pixels, and iTime is the time of the scene in seconds. fragCoord
  is in pixels from the bottom left, so y runs upwards. Either texture2D or
  texture can be used. A shader with its own main in place of mainImage can
  be used too, writing to gl_FragColor and reading gl_FragCoord. The version
  and the declarations of the inputs are added to the source, so leave them
  out. The alpha which is written is ignored.

  The shader is compiled the next time the widget is painted. If it does not
  compile the video is drawn without an effect and EffectError holds what
  the compiler said. With an effect the video is drawn every frame, so
  effects which use iTime move while the video is paused.

  The player is made the first time it is needed. If it can not be made the
  widget draws a black rectangle and Available is false. *)

type
  { TVideoState is whether a video is stopped, playing, or paused }
  TVideoState = (videoStopped, videoPlaying, videoPaused);

{ TVideoWidget plays a video, as described above }

  TVideoWidget = class(TCustomWidget)
  private
    FFileName: string;
    FAutoRestart: Boolean;
    FState: TVideoState;
    FPlayer: Pmpv_handle;
    FRender: Pmpv_render_context;
    FBitmap: IRenderBitmap;
    FFailed: Boolean;
    FLoaded: Boolean;
    FEnded: Boolean;
    FPendingLoad: Boolean;
    FHasFrame: Boolean;
    FRedraw: Boolean;
    { Set to 1 by the player from its own threads when it has a new picture }
    FUpdate: LongInt;
    FOnPlayComplete: TNotifyEvent;
    FVolume: Float;
    FMuted: Boolean;
    FEffect: string;
    FEffectError: string;
    FEffectChanged: Boolean;
    { The shader of the effect, and the framebuffer and texture the video
      is drawn to for the shader to read }
    FProgram: GLuint;
    FVertexArray: GLuint;
    FSourceFbo: GLuint;
    FSourceTexture: GLuint;
    FSourceWidth: Integer;
    FSourceHeight: Integer;
    FLocResolution: GLint;
    FLocTime: GLint;
    FLocChannel: GLint;
    procedure SetEffect(const Value: string);
    procedure CompileEffect;
    procedure ReleaseEffect;
    function EnsureSource(W, H: Integer): Boolean;
    procedure ReleaseSource;
    procedure RenderVideo(Fbo: GLuint; W, H: Integer);
    procedure DrawEffect(W, H: Integer);
    procedure SetFileName(const Value: string);
    function GetPosition: Double;
    procedure SetPosition(const Value: Double);
    function GetDuration: Double;
    function GetHasAudio: Boolean;
    procedure SetVolume(Value: Float);
    procedure SetMuted(Value: Boolean);
    procedure ApplyVolume;
    function GetAvailable: Boolean;
    function CreatePlayer: Boolean;
    procedure CreateRender;
    procedure SetPause(Value: Boolean);
    procedure LoadFile;
    procedure ProcessEvents;
    procedure PlayComplete;
    procedure RenderFrame(W, H: Integer; Video: Boolean);
  protected
    procedure Paint(Stage: TPaintStage); override;
  public
    constructor Create(Parent: TWidget; const Name: string = ''); override;
    destructor Destroy; override;
    { Play the video, or continue it if it is paused }
    procedure Play;
    { Hold the video where it is }
    procedure Pause;
    { Unload the video }
    procedure Stop;
    { The file or url of the video. Changing it while a video is playing
      plays the new video. }
    property FileName: string read FFileName write SetFileName;
    { Where the video is in seconds }
    property Position: Double read GetPosition write SetPosition;
    { How long the video is in seconds, or zero if it is not known }
    property Duration: Double read GetDuration;
    { Whether the video is stopped, playing, or paused }
    property State: TVideoState read FState;
    { When true the video plays again from the beginning when it ends }
    property AutoRestart: Boolean read FAutoRestart write FAutoRestart;
    { True if the video has sound. It is false until the video has been
      loaded, which is shortly after Play. }
    property HasAudio: Boolean read GetHasAudio;
    { How loud the sound of the video is, from 0 for silent to 1 for its
      normal loudness, which is the default }
    property Volume: Float read FVolume write SetVolume;
    { When true the sound of the video is silenced, leaving Volume as it is }
    property Muted: Boolean read FMuted write SetMuted;
    { The source of a fragment shader which draws the video, or empty for no
      effect }
    property Effect: string read FEffect write SetEffect;
    { What the compiler said if Effect did not compile, or empty }
    property EffectError: string read FEffectError;
    { False if the player could not be made }
    property Available: Boolean read GetAvailable;
    { OnPlayComplete fires when the video reaches its end }
    property OnPlayComplete: TNotifyEvent read FOnPlayComplete write FOnPlayComplete;
  end;
{$endif}

implementation

{$ifdef videowidget}

{ libmpv needs numbers to be read and written the same way everywhere, and
  will not start otherwise. The LCL widgetsets change this for the desktop. }

{$ifdef unix}
const
  {$ifdef linux}
  LC_NUMERIC = 1;
  {$else}
  LC_NUMERIC = 4;
  {$endif}

function setlocale(category: cint; locale: PChar): PChar; cdecl; external 'c';
{$endif}

const
  { A render bitmap is drawn by the canvas from the bottom up, as OpenGL
    draws, so the player is asked to draw the same way }
  FlipY = 1;

  { The parts added around the source of an effect }
  {$ifdef glesapi}
  EffectVersion = '#version 300 es'#10'precision highp float;'#10;
  {$else}
  EffectVersion = '#version 330 core'#10;
  {$endif}
  { One triangle which covers the framebuffer, made from the number of each
    vertex so that no vertex data is needed }
  EffectVertex = EffectVersion +
    'void main()'#10 +
    '{'#10 +
    '  vec2 p = vec2(float((gl_VertexID << 1) & 2), float(gl_VertexID & 2));'#10 +
    '  gl_Position = vec4(p * 2.0 - 1.0, 0.0, 1.0);'#10 +
    '}'#10;
  EffectHead = EffectVersion +
    'uniform vec3 iResolution;'#10 +
    'uniform float iTime;'#10 +
    'uniform sampler2D iChannel0;'#10 +
    'out vec4 effectColor;'#10 +
    '#define texture2D texture'#10;
  { Added when the source has mainImage }
  EffectTail = #10 +
    'void main()'#10 +
    '{'#10 +
    '  vec4 effectResult = vec4(0.0, 0.0, 0.0, 1.0);'#10 +
    '  mainImage(effectResult, gl_FragCoord.xy);'#10 +
    '  effectColor = vec4(effectResult.rgb, 1.0);'#10 +
    '}'#10;

var
  VideoCount: Integer;

{ Compile a shader, returning 0 and what the compiler said if it fails }

function CompileShader(Kind: GLenum; const Source: string; out Error: string): GLuint;
var
  P: PChar;
  Status: GLint;
  Len: GLsizei;
begin
  Error := '';
  Result := glCreateShader(Kind);
  if Result = 0 then
  begin
    Error := 'A shader could not be created';
    Exit;
  end;
  P := PChar(Source);
  glShaderSource(Result, 1, @P, nil);
  glCompileShader(Result);
  Status := 0;
  glGetShaderiv(Result, GL_COMPILE_STATUS, @Status);
  if Status <> 0 then
    Exit;
  SetLength(Error, 4096);
  Len := 0;
  glGetShaderInfoLog(Result, Length(Error), @Len, PChar(Error));
  SetLength(Error, Len);
  if Error = '' then
    Error := 'The shader did not compile';
  glDeleteShader(Result);
  Result := 0;
end;

{ The player finds the OpenGL functions the same way Codebot.OpenGL did }

function VideoGetProc(Ctx: Pointer; Name: PChar): Pointer; cdecl;
begin
  if Assigned(OpenGLGetProc) then
    Result := OpenGLGetProc(Name)
  else
    Result := nil;
end;

{ Called by the player from its own threads when it has a new picture }

procedure VideoUpdate(Ctx: Pointer); cdecl;
begin
  InterlockedExchange(TVideoWidget(Ctx).FUpdate, 1);
end;

{ TVideoWidget }

constructor TVideoWidget.Create(Parent: TWidget; const Name: string = '');
begin
  inherited Create(Parent, Name);
  Width := 320;
  Height := 180;
  FVolume := 1;
end;

destructor TVideoWidget.Destroy;
begin
  { The render context is freed first, while the OpenGL context is current }
  if FRender <> nil then
  begin
    mpv_render_context_set_update_callback(FRender, nil, nil);
    mpv_render_context_free(FRender);
    FRender := nil;
  end;
  if FPlayer <> nil then
  begin
    mpv_terminate_destroy(FPlayer);
    FPlayer := nil;
  end;
  ReleaseEffect;
  ReleaseSource;
  if FVertexArray <> 0 then
  begin
    glDeleteVertexArrays(1, @FVertexArray);
    FVertexArray := 0;
  end;
  try
    if (FBitmap <> nil) and (Computed.Theme is TCanvasTheme) then
      TCanvasTheme(Computed.Theme).Canvas.DisposeBitmap(FBitmap);
  except
    { The canvas may already be gone when the scene is closing }
  end;
  FBitmap := nil;
  inherited Destroy;
end;

function TVideoWidget.GetAvailable: Boolean;
begin
  Result := not FFailed;
end;

{ CreatePlayer makes the player, which does not need OpenGL. Its picture goes
  to the render context made by CreateRender, and it keeps the last picture
  of a video open when the video ends. }

function TVideoWidget.CreatePlayer: Boolean;
begin
  if FPlayer <> nil then
    Exit(True);
  if FFailed then
    Exit(False);
  {$ifdef unix}
  setlocale(LC_NUMERIC, 'C');
  {$endif}
  FPlayer := mpv_create;
  if FPlayer = nil then
  begin
    FFailed := True;
    Exit(False);
  end;
  mpv_set_option_string(FPlayer, 'vo', 'libmpv');
  mpv_set_option_string(FPlayer, 'idle', 'yes');
  mpv_set_option_string(FPlayer, 'keep-open', 'yes');
  mpv_set_option_string(FPlayer, 'config', 'no');
  mpv_set_option_string(FPlayer, 'terminal', 'no');
  mpv_set_option_string(FPlayer, 'osc', 'no');
  mpv_set_option_string(FPlayer, 'input-default-bindings', 'no');
  mpv_set_option_string(FPlayer, 'input-vo-keyboard', 'no');
  { Use the DRM hardware decoder where there is one, such as HEVC on the Pi 5.
    The decoded frames are copied to memory and uploaded as textures, as
    sharing the decoder's buffers with OpenGL fails at times on the Pi. }
  mpv_set_option_string(FPlayer, 'hwdec', 'drm-copy');
  { Cheaper scaling and no dithering, suited to small or embedded GPUs }
  mpv_set_option_string(FPlayer, 'profile', 'fast');
  if mpv_initialize(FPlayer) < 0 then
  begin
    mpv_terminate_destroy(FPlayer);
    FPlayer := nil;
    FFailed := True;
    Exit(False);
  end;
  { With keep-open the end of a video is told by this property }
  mpv_observe_property(FPlayer, 0, 'eof-reached', MPV_FORMAT_FLAG);
  ApplyVolume;
  Result := True;
end;

{ CreateRender makes the render context, which draws the video with OpenGL.
  It is called when the widget is painted, when the OpenGL context is
  current. A video is not loaded before the render context is made, since the
  player has nowhere to draw it. }

procedure TVideoWidget.CreateRender;
var
  Init: Tmpv_opengl_init_params;
  Params: array[0..2] of Tmpv_render_param;
begin
  if (FRender <> nil) or FFailed or (FPlayer = nil) then
    Exit;
  Init.get_proc_address := VideoGetProc;
  Init.get_proc_address_ctx := nil;
  Params[0].type_ := MPV_RENDER_PARAM_API_TYPE;
  Params[0].data := PChar(MPV_RENDER_API_TYPE_OPENGL);
  Params[1].type_ := MPV_RENDER_PARAM_OPENGL_INIT_PARAMS;
  Params[1].data := @Init;
  Params[2].type_ := MPV_RENDER_PARAM_INVALID;
  Params[2].data := nil;
  if mpv_render_context_create(FRender, FPlayer, @Params[0]) < 0 then
  begin
    FRender := nil;
    FFailed := True;
    Exit;
  end;
  mpv_render_context_set_update_callback(FRender, VideoUpdate, Self);
  FRedraw := True;
end;

procedure TVideoWidget.SetPause(Value: Boolean);
var
  Flag: cint;
begin
  if FPlayer = nil then
    Exit;
  Flag := Ord(Value);
  mpv_set_property(FPlayer, 'pause', MPV_FORMAT_FLAG, @Flag);
end;

procedure TVideoWidget.LoadFile;
var
  Args: array[0..2] of PChar;
begin
  FPendingLoad := False;
  if (FPlayer = nil) or (FFileName = '') then
    Exit;
  Args[0] := 'loadfile';
  Args[1] := PChar(FFileName);
  Args[2] := nil;
  FLoaded := mpv_command(FPlayer, @Args[0]) >= 0;
  FEnded := False;
  if FLoaded then
    SetPause(FState = videoPaused)
  else
    FState := videoStopped;
end;

procedure TVideoWidget.Play;
var
  Args: array[0..3] of PChar;
begin
  if FFileName = '' then
    Exit;
  if not CreatePlayer then
    Exit;
  FState := videoPlaying;
  if not FLoaded then
  begin
    if FRender = nil then
      FPendingLoad := True
    else
      LoadFile;
    Exit;
  end;
  if FEnded then
  begin
    { Play the video again from the beginning }
    FEnded := False;
    Args[0] := 'seek';
    Args[1] := '0';
    Args[2] := 'absolute';
    Args[3] := nil;
    mpv_command(FPlayer, @Args[0]);
  end;
  SetPause(False);
end;

procedure TVideoWidget.Pause;
begin
  if FState <> videoPlaying then
    Exit;
  FState := videoPaused;
  if FLoaded then
    SetPause(True);
end;

procedure TVideoWidget.Stop;
var
  Args: array[0..1] of PChar;
begin
  FState := videoStopped;
  FPendingLoad := False;
  FEnded := False;
  if FLoaded and (FPlayer <> nil) then
  begin
    Args[0] := 'stop';
    Args[1] := nil;
    mpv_command(FPlayer, @Args[0]);
  end;
  FLoaded := False;
end;

procedure TVideoWidget.SetFileName(const Value: string);
var
  Prior: TVideoState;
begin
  if Value = FFileName then
    Exit;
  Prior := FState;
  Stop;
  FFileName := Value;
  if (Prior = videoStopped) or (FFileName = '') then
    Exit;
  Play;
  if Prior = videoPaused then
    Pause;
end;

function TVideoWidget.GetPosition: Double;
var
  D: cdouble;
begin
  Result := 0;
  if (FPlayer = nil) or (not FLoaded) then
    Exit;
  D := 0;
  if mpv_get_property(FPlayer, 'time-pos', MPV_FORMAT_DOUBLE, @D) >= 0 then
    Result := D;
end;

procedure TVideoWidget.SetPosition(const Value: Double);
var
  D: cdouble;
begin
  if (FPlayer = nil) or (not FLoaded) then
    Exit;
  D := Value;
  if D < 0 then
    D := 0;
  mpv_set_property(FPlayer, 'time-pos', MPV_FORMAT_DOUBLE, @D);
  { A video which had ended is left paused where it was moved to }
  if FEnded then
  begin
    FEnded := False;
    FState := videoPaused;
  end;
end;

function TVideoWidget.GetDuration: Double;
var
  D: cdouble;
begin
  Result := 0;
  if (FPlayer = nil) or (not FLoaded) then
    Exit;
  D := 0;
  if mpv_get_property(FPlayer, 'duration', MPV_FORMAT_DOUBLE, @D) >= 0 then
    Result := D;
end;

{ The video has sound if one of its tracks is an audio track }

function TVideoWidget.GetHasAudio: Boolean;
var
  Count: cint64;
  S: PChar;
  I: Integer;
begin
  Result := False;
  if (FPlayer = nil) or (not FLoaded) then
    Exit;
  Count := 0;
  if mpv_get_property(FPlayer, 'track-list/count', MPV_FORMAT_INT64, @Count) < 0 then
    Exit;
  for I := 0 to Integer(Count) - 1 do
  begin
    S := mpv_get_property_string(FPlayer, PChar('track-list/' + IntToStr(I) + '/type'));
    if S = nil then
      Continue;
    Result := StrComp(S, 'audio') = 0;
    mpv_free(S);
    if Result then
      Exit;
  end;
end;

procedure TVideoWidget.SetVolume(Value: Float);
begin
  if Value < 0 then
    Value := 0
  else if Value > 1 then
    Value := 1;
  FVolume := Value;
  ApplyVolume;
end;

procedure TVideoWidget.SetMuted(Value: Boolean);
begin
  FMuted := Value;
  ApplyVolume;
end;

{ The volume and mute are kept by the widget and given to the player when it
  is made, as it is not made until it is needed. The player takes a volume
  from 0 to 100. }

procedure TVideoWidget.ApplyVolume;
var
  D: cdouble;
  Flag: cint;
begin
  if FPlayer = nil then
    Exit;
  D := FVolume * 100;
  mpv_set_property(FPlayer, 'volume', MPV_FORMAT_DOUBLE, @D);
  Flag := Ord(FMuted);
  mpv_set_property(FPlayer, 'mute', MPV_FORMAT_FLAG, @Flag);
end;

{ ProcessEvents reads what the player has to tell since the last time }

procedure TVideoWidget.ProcessEvents;
var
  Event: Pmpv_event;
  Prop: Pmpv_event_property;
  EndFile: Pmpv_event_end_file;
  Complete: Boolean;
begin
  if FPlayer = nil then
    Exit;
  Complete := False;
  repeat
    Event := mpv_wait_event(FPlayer, 0);
    if (Event = nil) or (Event^.event_id = MPV_EVENT_NONE) then
      Break;
    case Event^.event_id of
      MPV_EVENT_PROPERTY_CHANGE:
        begin
          Prop := Event^.data;
          if (Prop <> nil) and (Prop^.format = MPV_FORMAT_FLAG) and (Prop^.data <> nil) and
            (StrComp(Prop^.name, 'eof-reached') = 0) and (pcint(Prop^.data)^ <> 0) then
            Complete := True;
        end;
      MPV_EVENT_END_FILE:
        begin
          { A file which could not be played }
          EndFile := Event^.data;
          if (EndFile <> nil) and (EndFile^.reason = MPV_END_FILE_REASON_ERROR) then
          begin
            FLoaded := False;
            FEnded := False;
            FState := videoStopped;
          end;
        end;
    end;
  until False;
  if Complete and FLoaded and (FState = videoPlaying) then
    PlayComplete;
end;

{ The player has paused on the last picture of the video }

procedure TVideoWidget.PlayComplete;
begin
  FEnded := True;
  if FAutoRestart then
    Play
  else
    FState := videoStopped;
  if Assigned(FOnPlayComplete) then
    FOnPlayComplete(Self);
end;

procedure TVideoWidget.SetEffect(const Value: string);
begin
  if Value = FEffect then
    Exit;
  FEffect := Value;
  FEffectError := '';
  FEffectChanged := True;
end;

procedure TVideoWidget.ReleaseEffect;
begin
  if FProgram <> 0 then
    glDeleteProgram(FProgram);
  FProgram := 0;
end;

{ CompileEffect makes the shader program of the effect. It is called when the
  widget is painted, when the OpenGL context is current. }

procedure TVideoWidget.CompileEffect;
var
  Source: string;
  Vert, Frag: GLuint;
  Status: GLint;
  Len: GLsizei;
begin
  ReleaseEffect;
  FEffectError := '';
  if Trim(FEffect) = '' then
    Exit;
  { #line makes the compiler count lines from the start of the effect }
  if Pos('mainImage', FEffect) > 0 then
    Source := EffectHead + '#line 1'#10 + FEffect + EffectTail
  else
    Source := EffectHead + '#define gl_FragColor effectColor'#10'#line 1'#10 + FEffect + #10;
  Frag := CompileShader(GL_FRAGMENT_SHADER, Source, FEffectError);
  if Frag = 0 then
    Exit;
  Vert := CompileShader(GL_VERTEX_SHADER, EffectVertex, FEffectError);
  if Vert = 0 then
  begin
    glDeleteShader(Frag);
    Exit;
  end;
  FProgram := glCreateProgram;
  glAttachShader(FProgram, Vert);
  glAttachShader(FProgram, Frag);
  glLinkProgram(FProgram);
  { The shaders are kept by the program until it is deleted }
  glDeleteShader(Vert);
  glDeleteShader(Frag);
  Status := 0;
  glGetProgramiv(FProgram, GL_LINK_STATUS, @Status);
  if Status = 0 then
  begin
    SetLength(FEffectError, 4096);
    Len := 0;
    glGetProgramInfoLog(FProgram, Length(FEffectError), @Len, PChar(FEffectError));
    SetLength(FEffectError, Len);
    if FEffectError = '' then
      FEffectError := 'The shader did not link';
    ReleaseEffect;
    Exit;
  end;
  FLocResolution := glGetUniformLocation(FProgram, 'iResolution');
  FLocTime := glGetUniformLocation(FProgram, 'iTime');
  FLocChannel := glGetUniformLocation(FProgram, 'iChannel0');
  if FVertexArray = 0 then
    glGenVertexArrays(1, @FVertexArray);
end;

procedure TVideoWidget.ReleaseSource;
begin
  if FSourceFbo <> 0 then
    glDeleteFramebuffers(1, @FSourceFbo);
  if FSourceTexture <> 0 then
    glDeleteTextures(1, @FSourceTexture);
  FSourceFbo := 0;
  FSourceTexture := 0;
  FSourceWidth := 0;
  FSourceHeight := 0;
end;

{ EnsureSource makes the framebuffer and texture the video is drawn to when
  there is an effect, returning true if they are new and have no picture.
  The framebuffer is left current. }

function TVideoWidget.EnsureSource(W, H: Integer): Boolean;
begin
  Result := False;
  if (FSourceFbo <> 0) and (FSourceWidth = W) and (FSourceHeight = H) then
    Exit;
  ReleaseSource;
  glGenTextures(1, @FSourceTexture);
  glBindTexture(GL_TEXTURE_2D, FSourceTexture);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, W, H, 0, GL_RGBA, GL_UNSIGNED_BYTE, nil);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  glBindTexture(GL_TEXTURE_2D, 0);
  glGenFramebuffers(1, @FSourceFbo);
  glBindFramebuffer(GL_FRAMEBUFFER, FSourceFbo);
  glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D,
    FSourceTexture, 0);
  FSourceWidth := W;
  FSourceHeight := H;
  Result := True;
end;

{ RenderVideo has the player draw its current picture into a framebuffer.
  The player expects the OpenGL state to be the defaults and leaves it that
  way, other than the framebuffer and the viewport. }

procedure TVideoWidget.RenderVideo(Fbo: GLuint; W, H: Integer);
var
  Target: Tmpv_opengl_fbo;
  Params: array[0..3] of Tmpv_render_param;
  Flip, Block: cint;
begin
  Target.fbo := Fbo;
  Target.w := W;
  Target.h := H;
  Target.internal_format := 0;
  Flip := FlipY;
  { Do not wait for the time the picture is meant to be shown }
  Block := 0;
  Params[0].type_ := MPV_RENDER_PARAM_OPENGL_FBO;
  Params[0].data := @Target;
  Params[1].type_ := MPV_RENDER_PARAM_FLIP_Y;
  Params[1].data := @Flip;
  Params[2].type_ := MPV_RENDER_PARAM_BLOCK_FOR_TARGET_TIME;
  Params[2].data := @Block;
  Params[3].type_ := MPV_RENDER_PARAM_INVALID;
  Params[3].data := nil;
  mpv_render_context_render(FRender, @Params[0]);
end;

{ DrawEffect draws the picture of the video through the shader of the effect
  into the framebuffer which is current, and leaves no program, vertex array,
  or texture in use }

procedure TVideoWidget.DrawEffect(W, H: Integer);
var
  T: Double;
begin
  T := 0;
  if Main <> nil then
    T := Main.Time;
  glUseProgram(FProgram);
  glUniform3f(FLocResolution, W, H, 1);
  glUniform1f(FLocTime, T);
  glUniform1i(FLocChannel, 0);
  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D, FSourceTexture);
  glBindVertexArray(FVertexArray);
  glDrawArrays(GL_TRIANGLES, 0, 3);
  glBindVertexArray(0);
  glBindTexture(GL_TEXTURE_2D, 0);
  glUseProgram(0);
end;

{ RenderFrame draws into the bitmap. Without an effect the player draws its
  picture there. With an effect the player draws its picture into a texture,
  if Video is true, and the shader of the effect draws the texture into the
  bitmap.

  Binding the bitmap ends the canvas frame, makes the framebuffer of the
  bitmap current, and sets the viewport. Unbinding puts all of that back and
  begins the canvas frame again. The canvas sets the OpenGL state it needs
  each time it draws. }

procedure TVideoWidget.RenderFrame(W, H: Integer; Video: Boolean);
var
  Fbo: GLint;
begin
  FBitmap.Bind;
  try
    Fbo := 0;
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, @Fbo);
    glDisable(GL_BLEND);
    glDisable(GL_CULL_FACE);
    glDisable(GL_DEPTH_TEST);
    glDisable(GL_SCISSOR_TEST);
    glDisable(GL_STENCIL_TEST);
    if FProgram <> 0 then
    begin
      if EnsureSource(W, H) then
        Video := True;
      if Video then
        RenderVideo(FSourceFbo, W, H);
      glBindFramebuffer(GL_FRAMEBUFFER, Fbo);
      glViewport(0, 0, W, H);
      DrawEffect(W, H);
    end
    else if Video then
      RenderVideo(Fbo, W, H);
  finally
    FBitmap.Unbind;
  end;
  FHasFrame := True;
  FRedraw := False;
end;

procedure TVideoWidget.Paint(Stage: TPaintStage);
var
  Canvas: ICanvas;
  R: TRectF;
  W, H: Integer;
  Update: Boolean;
begin
  if Stage <> prePaint then
    Exit;
  if not (Computed.Theme is TCanvasTheme) then
    Exit;
  Canvas := TCanvasTheme(Computed.Theme).Canvas;
  if Canvas = nil then
    Exit;
  R := Computed.Bounds.Round;
  W := Round(R.Width);
  H := Round(R.Height);
  if (W < 1) or (H < 1) then
    Exit;
  if FPlayer <> nil then
  begin
    CreateRender;
    if FPendingLoad and (FRender <> nil) then
      LoadFile;
    ProcessEvents;
  end;
  if FEffectChanged then
  begin
    { The video is drawn again, with or without the new effect }
    FEffectChanged := False;
    CompileEffect;
    FRedraw := True;
  end;
  if FRender <> nil then
  begin
    { The bitmap is the size of the widget, and is drawn again when the size
      changes since that makes a new framebuffer }
    if FBitmap = nil then
    begin
      Inc(VideoCount);
      FBitmap := Canvas.NewBitmap('videowidget' + IntToStr(VideoCount), W, H);
      FRedraw := True;
    end
    else if (FBitmap.Width <> LongWord(W)) or (FBitmap.Height <> LongWord(H)) then
    begin
      FBitmap.Resize(W, H);
      FRedraw := True;
    end;
    Update := False;
    if InterlockedExchange(FUpdate, 0) <> 0 then
      Update := (mpv_render_context_update(FRender) and MPV_RENDER_UPDATE_FRAME) <> 0;
    { An effect is drawn every frame, as it can change with time }
    if (FBitmap <> nil) and (Update or FRedraw or (FProgram <> 0)) then
      RenderFrame(W, H, Update or FRedraw);
  end;
  if FHasFrame and (FBitmap <> nil) then
    Canvas.DrawImage(FBitmap, FBitmap.ClientRect, R)
  else
  begin
    Canvas.Rect(R);
    Canvas.Fill(ARGB($FF000000));
  end;
end;
{$endif}

end.
