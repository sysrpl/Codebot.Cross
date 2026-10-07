(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ Codebot.Interop.MPV declares the part of the C API of the libmpv library
  used to play video: the client API from mpv/client.h and the render API
  from mpv/render.h and mpv/render_gl.h.

  The unit is empty unless videowidget is defined in render.inc. When it is
  defined libmpv is linked when a program using this unit is built, which
  needs the libmpv development package to be installed. }

unit Codebot.Interop.MPV;

{$i render.inc}
{$packrecords c}

interface

{$ifdef videowidget}
uses
  CTypes;

{$ifdef windows}
  {$define libmpv := external 'libmpv-2.dll'}
{$else}
  {$define libmpv := external 'mpv'}
{$endif}

{ client.h }

type
  Tmpv_handle = record end;
  { Pointer to a player }
  Pmpv_handle = ^Tmpv_handle;

const
  MPV_ERROR_SUCCESS = 0;
  MPV_ERROR_EVENT_QUEUE_FULL = -1;
  MPV_ERROR_NOMEM = -2;
  MPV_ERROR_UNINITIALIZED = -3;
  MPV_ERROR_INVALID_PARAMETER = -4;
  MPV_ERROR_OPTION_NOT_FOUND = -5;
  MPV_ERROR_OPTION_FORMAT = -6;
  MPV_ERROR_OPTION_ERROR = -7;
  MPV_ERROR_PROPERTY_NOT_FOUND = -8;
  MPV_ERROR_PROPERTY_FORMAT = -9;
  MPV_ERROR_PROPERTY_UNAVAILABLE = -10;
  MPV_ERROR_PROPERTY_ERROR = -11;
  MPV_ERROR_COMMAND = -12;
  MPV_ERROR_LOADING_FAILED = -13;
  MPV_ERROR_AO_INIT_FAILED = -14;
  MPV_ERROR_VO_INIT_FAILED = -15;
  MPV_ERROR_NOTHING_TO_PLAY = -16;
  MPV_ERROR_UNKNOWN_FORMAT = -17;
  MPV_ERROR_UNSUPPORTED = -18;
  MPV_ERROR_NOT_IMPLEMENTED = -19;
  MPV_ERROR_GENERIC = -20;

  { mpv_format }
  MPV_FORMAT_NONE = 0;
  MPV_FORMAT_STRING = 1;
  MPV_FORMAT_OSD_STRING = 2;
  { A flag is a cint which is 0 or 1 }
  MPV_FORMAT_FLAG = 3;
  MPV_FORMAT_INT64 = 4;
  MPV_FORMAT_DOUBLE = 5;
  MPV_FORMAT_NODE = 6;
  MPV_FORMAT_NODE_ARRAY = 7;
  MPV_FORMAT_NODE_MAP = 8;
  MPV_FORMAT_BYTE_ARRAY = 9;

  { mpv_event_id }
  MPV_EVENT_NONE = 0;
  MPV_EVENT_SHUTDOWN = 1;
  MPV_EVENT_LOG_MESSAGE = 2;
  MPV_EVENT_GET_PROPERTY_REPLY = 3;
  MPV_EVENT_SET_PROPERTY_REPLY = 4;
  MPV_EVENT_COMMAND_REPLY = 5;
  MPV_EVENT_START_FILE = 6;
  MPV_EVENT_END_FILE = 7;
  MPV_EVENT_FILE_LOADED = 8;
  MPV_EVENT_IDLE = 11;
  MPV_EVENT_TICK = 14;
  MPV_EVENT_CLIENT_MESSAGE = 16;
  MPV_EVENT_VIDEO_RECONFIG = 17;
  MPV_EVENT_AUDIO_RECONFIG = 18;
  MPV_EVENT_SEEK = 20;
  MPV_EVENT_PLAYBACK_RESTART = 21;
  MPV_EVENT_PROPERTY_CHANGE = 22;
  MPV_EVENT_QUEUE_OVERFLOW = 24;
  MPV_EVENT_HOOK = 25;

  { mpv_end_file_reason }
  MPV_END_FILE_REASON_EOF = 0;
  MPV_END_FILE_REASON_STOP = 2;
  MPV_END_FILE_REASON_QUIT = 3;
  MPV_END_FILE_REASON_ERROR = 4;
  MPV_END_FILE_REASON_REDIRECT = 5;

type
  { The data of an MPV_EVENT_PROPERTY_CHANGE event }
  Tmpv_event_property = record
    name: PChar;
    format: cint;
    data: Pointer;
  end;
  { Pointer to a Tmpv_event_property }
  Pmpv_event_property = ^Tmpv_event_property;

  { The data of an MPV_EVENT_END_FILE event }
  Tmpv_event_end_file = record
    reason: cint;
    error: cint;
    playlist_entry_id: cint64;
    playlist_insert_id: cint64;
    playlist_insert_num_entries: cint;
  end;
  { Pointer to a Tmpv_event_end_file }
  Pmpv_event_end_file = ^Tmpv_event_end_file;

  { Tmpv_event is an event from the player }
  Tmpv_event = record
    event_id: cint;
    error: cint;
    reply_userdata: cuint64;
    data: Pointer;
  end;
  { Pointer to a Tmpv_event }
  Pmpv_event = ^Tmpv_event;

  { A callback made when the player has events to read }
  Tmpv_wakeup_fn = procedure(d: Pointer); cdecl;

{ The version of the client API }
function mpv_client_api_version: culong; cdecl; libmpv;
{ The text of an error code }
function mpv_error_string(error: cint): PChar; cdecl; libmpv;
{ Free memory returned by the player }
procedure mpv_free(data: Pointer); cdecl; libmpv;
{ mpv_create returns nil if the LC_NUMERIC locale is not "C" }
function mpv_create: Pmpv_handle; cdecl; libmpv;
{ Start a player after its options have been set }
function mpv_initialize(ctx: Pmpv_handle): cint; cdecl; libmpv;
{ Destroy a handle to the player }
procedure mpv_destroy(ctx: Pmpv_handle); cdecl; libmpv;
{ Stop the player and destroy it }
procedure mpv_terminate_destroy(ctx: Pmpv_handle); cdecl; libmpv;
{ Set an option before the player is started }
function mpv_set_option(ctx: Pmpv_handle; name: PChar; format: cint; data: Pointer): cint; cdecl; libmpv;
function mpv_set_option_string(ctx: Pmpv_handle; name, data: PChar): cint; cdecl; libmpv;
{ The arguments of a command are an array of strings which ends with nil }
function mpv_command(ctx: Pmpv_handle; args: PPChar): cint; cdecl; libmpv;
{ Run a command given as one string }
function mpv_command_string(ctx: Pmpv_handle; args: PChar): cint; cdecl; libmpv;
{ Run a command without waiting for it }
function mpv_command_async(ctx: Pmpv_handle; reply_userdata: cuint64; args: PPChar): cint; cdecl; libmpv;
{ Set a property }
function mpv_set_property(ctx: Pmpv_handle; name: PChar; format: cint; data: Pointer): cint; cdecl; libmpv;
function mpv_set_property_string(ctx: Pmpv_handle; name, data: PChar): cint; cdecl; libmpv;
function mpv_set_property_async(ctx: Pmpv_handle; reply_userdata: cuint64; name: PChar;
  format: cint; data: Pointer): cint; cdecl; libmpv;
{ Get a property }
function mpv_get_property(ctx: Pmpv_handle; name: PChar; format: cint; data: Pointer): cint; cdecl; libmpv;
{ The string returned must be freed with mpv_free }
function mpv_get_property_string(ctx: Pmpv_handle; name: PChar): PChar; cdecl; libmpv;
{ Ask for an event each time a property changes }
function mpv_observe_property(mpv: Pmpv_handle; reply_userdata: cuint64; name: PChar;
  format: cint): cint; cdecl; libmpv;
{ Stop watching the properties registered with a value }
function mpv_unobserve_property(mpv: Pmpv_handle; registered_reply_userdata: cuint64): cint; cdecl; libmpv;
{ The name of an event }
function mpv_event_name(event: cint): PChar; cdecl; libmpv;
{ Ask for log messages as events }
function mpv_request_log_messages(ctx: Pmpv_handle; min_level: PChar): cint; cdecl; libmpv;
{ Never returns nil. With a timeout of 0 an event of MPV_EVENT_NONE is
  returned when no event is waiting. }
function mpv_wait_event(ctx: Pmpv_handle; timeout: cdouble): Pmpv_event; cdecl; libmpv;
{ Make mpv_wait_event return at once }
procedure mpv_wakeup(ctx: Pmpv_handle); cdecl; libmpv;
{ Set a callback made when there are events to read }
procedure mpv_set_wakeup_callback(ctx: Pmpv_handle; cb: Tmpv_wakeup_fn; d: Pointer); cdecl; libmpv;

{ render.h }

type
  Tmpv_render_context = record end;
  { Pointer to a render context }
  Pmpv_render_context = ^Tmpv_render_context;

const
  { mpv_render_param_type }
  MPV_RENDER_PARAM_INVALID = 0;
  { PChar }
  MPV_RENDER_PARAM_API_TYPE = 1;
  { Pmpv_opengl_init_params }
  MPV_RENDER_PARAM_OPENGL_INIT_PARAMS = 2;
  { Pmpv_opengl_fbo }
  MPV_RENDER_PARAM_OPENGL_FBO = 3;
  { pcint }
  MPV_RENDER_PARAM_FLIP_Y = 4;
  MPV_RENDER_PARAM_DEPTH = 5;
  MPV_RENDER_PARAM_ICC_PROFILE = 6;
  MPV_RENDER_PARAM_AMBIENT_LIGHT = 7;
  MPV_RENDER_PARAM_X11_DISPLAY = 8;
  MPV_RENDER_PARAM_WL_DISPLAY = 9;
  MPV_RENDER_PARAM_ADVANCED_CONTROL = 10;
  MPV_RENDER_PARAM_NEXT_FRAME_INFO = 11;
  { pcint }
  MPV_RENDER_PARAM_BLOCK_FOR_TARGET_TIME = 12;
  MPV_RENDER_PARAM_SKIP_RENDERING = 13;

  MPV_RENDER_API_TYPE_OPENGL = 'opengl';
  MPV_RENDER_API_TYPE_SW = 'sw';

  { mpv_render_update_flag }
  MPV_RENDER_UPDATE_FRAME = 1;

type
  { An array of parameters ends with one of the type MPV_RENDER_PARAM_INVALID }
  Tmpv_render_param = record
    type_: cint;
    data: Pointer;
  end;
  { Pointer to a Tmpv_render_param }
  Pmpv_render_param = ^Tmpv_render_param;

  { Called from any thread when mpv_render_context_update should be called }
  Tmpv_render_update_fn = procedure(cb_ctx: Pointer); cdecl;

{ Create a render context for a player }
function mpv_render_context_create(out res: Pmpv_render_context; mpv: Pmpv_handle;
  params: Pmpv_render_param): cint; cdecl; libmpv;
{ Set a parameter of the render context }
function mpv_render_context_set_parameter(ctx: Pmpv_render_context;
  param: Tmpv_render_param): cint; cdecl; libmpv;
{ Set a callback made when a new frame is ready }
procedure mpv_render_context_set_update_callback(ctx: Pmpv_render_context;
  callback: Tmpv_render_update_fn; callback_ctx: Pointer); cdecl; libmpv;
{ Find out if a new frame should be drawn }
function mpv_render_context_update(ctx: Pmpv_render_context): cuint64; cdecl; libmpv;
{ Draw the current frame of the video }
function mpv_render_context_render(ctx: Pmpv_render_context; params: Pmpv_render_param): cint; cdecl; libmpv;
{ Tell the player a frame was shown }
procedure mpv_render_context_report_swap(ctx: Pmpv_render_context); cdecl; libmpv;
{ The OpenGL context must be current. Call it before mpv_terminate_destroy. }
procedure mpv_render_context_free(ctx: Pmpv_render_context); cdecl; libmpv;

{ render_gl.h }

type
  Tmpv_opengl_get_proc_address = function(ctx: Pointer; name: PChar): Pointer; cdecl;

  { Tmpv_opengl_init_params gives the player a way to find OpenGL functions }
  Tmpv_opengl_init_params = record
    get_proc_address: Tmpv_opengl_get_proc_address;
    get_proc_address_ctx: Pointer;
  end;
  { Pointer to a Tmpv_opengl_init_params }
  Pmpv_opengl_init_params = ^Tmpv_opengl_init_params;

  { The framebuffer to render to, where 0 is the default framebuffer, and
    its size. An internal format of 0 is unknown. }
  Tmpv_opengl_fbo = record
    fbo: cint;
    w: cint;
    h: cint;
    internal_format: cint;
  end;
  { Pointer to a Tmpv_opengl_fbo }
  Pmpv_opengl_fbo = ^Tmpv_opengl_fbo;
{$endif}

implementation

end.
