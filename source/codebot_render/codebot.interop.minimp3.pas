(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ Codebot.Interop.MiniMp3 declares the part of the C API of the minimp3
  library used to decode mp3 audio.

  The library is linked statically from libcodebotaudio.a, which is built
  by audio/build.sh. }

unit Codebot.Interop.MiniMp3;

{$i render.inc}
{$packrecords c}

interface

uses
  CTypes;

type
  { TMp3Sample is one 16 bit sample }
  TMp3Sample = cint16;
  { Pointer to a TMp3Sample }
  PMp3Sample = ^TMp3Sample;

const
  { The most samples per channel a frame can hold }
  MINIMP3_SAMPLES_PER_FRAME = 1152;
  { The bytes needed to hold a frame of stereo samples }
  MINIMP3_BYTES_PER_FRAME = MINIMP3_SAMPLES_PER_FRAME * SizeOf(TMp3Sample) * 2;

type
  { Describes the frame last decoded }
  TMp3DecFrameInfo = record
    { The bytes to move forward in the data after decoding the frame }
    frame_bytes: cint;
    frame_offset: cint;
    { 1 for mono and 2 for stereo }
    channels: cint;
    { The sample rate }
    hz: cint;
    layer: cint;
    bitrate_kbps: cint;
  end;
  { Pointer to a TMp3DecFrameInfo }
  PMp3DecFrameInfo = ^TMp3DecFrameInfo;

  { The state of a decoder }
  TMp3Dec = record
    mdct_overlap: array[0..1, 0..9 * 32 - 1] of cfloat;
    qmf_state: array[0..15 * 2 * 32 - 1] of cfloat;
    reserv: cint;
    free_format_bytes: cint;
    header: array[0..3] of Byte;
    reserv_buf: array[0..510] of Byte;
  end;
  { Pointer to a TMp3Dec }
  PMp3Dec = ^TMp3Dec;

{ Prepare a decoder before decoding or after seeking }
procedure mp3dec_init(dec: PMp3Dec); cdecl; external;
{ Decode one frame from data and return the samples per channel written to
  pcm, which must hold MINIMP3_BYTES_PER_FRAME. Move data forward by
  info.frame_bytes after each call. A result of 0 with info.frame_bytes
  above 0 is data which was skipped, and with info.frame_bytes of 0 there
  is no more audio. }
function mp3dec_decode_frame(dec: PMp3Dec; data: Pointer; dataSize: cint;
  pcm: PMp3Sample; out info: TMp3DecFrameInfo): cint; cdecl; external;

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
