(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ Codebot.Interop.Xmp declares the part of the C API of the libxmp library
  version 4.6.0 used to play tracker music.

  The library is linked statically from libcodebotaudio.a, which is built
  by audio/build.sh. It is built without the depackers and ProWizard
  loaders, so compressed modules are not supported. }

unit Codebot.Interop.Xmp;

{$i render.inc}
{$packrecords c}

interface

uses
  CTypes;

const
  { The size of a module name and type }
  XMP_NAME_SIZE = 64;
  { The most channels a module can have }
  XMP_MAX_CHANNELS = 64;

  { Sample format flags for xmp_start_player. A format of 0 is signed 16 bit
    stereo. }
  XMP_FORMAT_8BIT = 1 shl 0;
  XMP_FORMAT_UNSIGNED = 1 shl 1;
  XMP_FORMAT_MONO = 1 shl 2;

  { Player parameters for xmp_set_player }
  XMP_PLAYER_AMP = 0;
  XMP_PLAYER_MIX = 1;
  XMP_PLAYER_INTERP = 2;
  XMP_PLAYER_DSP = 3;
  XMP_PLAYER_FLAGS = 4;
  XMP_PLAYER_CFLAGS = 5;
  XMP_PLAYER_SMPCTL = 6;
  XMP_PLAYER_VOLUME = 7;

  { A value of XMP_PLAYER_SMPCTL which loads a module without its samples.
    Set it before the module is loaded. }
  XMP_SMPCTL_SKIP = 1 shl 0;

  { Results are 0 on success or one of these negated }
  XMP_END = 1;
  XMP_ERROR_INTERNAL = 2;
  XMP_ERROR_FORMAT = 3;
  XMP_ERROR_LOAD = 4;
  XMP_ERROR_DEPACK = 5;
  XMP_ERROR_SYSTEM = 6;
  XMP_ERROR_INVALID = 7;
  XMP_ERROR_STATE = 8;

type
  { TXmpContext is a handle to a player }
  TXmpContext = Pointer;

  { The title and format of a module which passed a test }
  TXmpTestInfo = record
    name: array[0..XMP_NAME_SIZE - 1] of Char;
    type_: array[0..XMP_NAME_SIZE - 1] of Char;
  end;

  { TXmpEvent is one note event of a pattern }
  TXmpEvent = record
    note: Byte;
    ins: Byte;
    vol: Byte;
    fxt: Byte;
    fxp: Byte;
    f2t: Byte;
    f2p: Byte;
    flag: Byte;
  end;

  { TXmpChannelInfo is the state of one channel while playing }
  TXmpChannelInfo = record
    period: cuint;
    position: cuint;
    pitchbend: cshort;
    note: Byte;
    instrument: Byte;
    sample: Byte;
    volume: Byte;
    pan: Byte;
    reserved: Byte;
    event: TXmpEvent;
  end;

  { The state of the player after the last frame it played }
  TXmpFrameInfo = record
    pos: cint;
    pattern: cint;
    row: cint;
    num_rows: cint;
    frame: cint;
    speed: cint;
    bpm: cint;
    { The current time in milliseconds }
    time: cint;
    { The estimated length in milliseconds }
    total_time: cint;
    { The length of the frame in microseconds }
    frame_time: cint;
    buffer: Pointer;
    buffer_size: cint;
    total_size: cint;
    volume: cint;
    loop_count: cint;
    virt_channels: cint;
    virt_used: cint;
    sequence: cint;
    channel_info: array[0..XMP_MAX_CHANNELS - 1] of TXmpChannelInfo;
  end;
  { Pointer to a TXmpFrameInfo }
  PXmpFrameInfo = ^TXmpFrameInfo;

{ Create a player }
function xmp_create_context: TXmpContext; cdecl; external;
{ Free a player }
procedure xmp_free_context(c: TXmpContext); cdecl; external;
{ Return 0 if the memory holds a module in a supported format }
function xmp_test_module_from_memory(mem: Pointer; size: clong;
  out info: TXmpTestInfo): cint; cdecl; external;
{ Load a module from memory }
function xmp_load_module_from_memory(c: TXmpContext; mem: Pointer; size: clong): cint; cdecl; external;
{ Unload the module }
procedure xmp_release_module(c: TXmpContext); cdecl; external;
{ Start playing the loaded module at a sample rate and sample format }
function xmp_start_player(c: TXmpContext; rate, format: cint): cint; cdecl; external;
{ Stop playing }
procedure xmp_end_player(c: TXmpContext); cdecl; external;
{ Fill a buffer of size bytes with samples. With a loop of 0 the module
  repeats without end, and otherwise the result is -XMP_END once it has
  played that many times. }
function xmp_play_buffer(c: TXmpContext; buffer: Pointer; size, loop: cint): cint; cdecl; external;
{ Get information about the frame which was just played }
procedure xmp_get_frame_info(c: TXmpContext; info: PXmpFrameInfo); cdecl; external;
{ Seek to the pattern position nearest a time in milliseconds }
function xmp_seek_time(c: TXmpContext; time: cint): cint; cdecl; external;
{ Set a parameter of the player }
function xmp_set_player(c: TXmpContext; param, val: cint): cint; cdecl; external;

implementation

uses
  { On Windows the C runtime is linked by Codebot.Interop.MinGW }
  Codebot.Interop.MinGW;

{$linklib libcodebotaudio.a}
{$ifdef unix}
  {$linklib m}
  {$linklib c}
{$endif}

end.
