(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ Codebot.Interop.Vorbis declares the part of the C API of the vorbisfile
  library used to decode ogg vorbis audio.

  The library is linked statically from libcodebotaudio.a, which is built
  by audio/build.sh. }

unit Codebot.Interop.Vorbis;

{$i render.inc}
{$packrecords c}

interface

uses
  CTypes;

type
  { An OggVorbis_File, which is made by ov_create }
  TOggVorbisFile = Pointer;

  { Describes an open vorbis file }
  TVorbisInfo = record
    version: cint;
    { 1 for mono and 2 for stereo }
    channels: cint;
    { The sample rate }
    rate: clong;
    bitrate_upper: clong;
    bitrate_nominal: clong;
    bitrate_lower: clong;
    bitrate_window: clong;
    codec_setup: Pointer;
  end;
  { Pointer to a TVorbisInfo }
  PVorbisInfo = ^TVorbisInfo;

  { The functions ov_open_callbacks reads the file with. The functions to
    seek and tell may be nil if the data cannot be seeked, and the function
    to close may be nil. }
  TOVCallbacks = record
    read_func: function(mem: Pointer; size, count: csize_t; datasource: Pointer): csize_t; cdecl;
    seek_func: function(datasource: Pointer; offset: cint64; whence: cint): cint; cdecl;
    close_func: function(datasource: Pointer): cint; cdecl;
    tell_func: function(datasource: Pointer): clong; cdecl;
  end;

{ Allocate the memory of an OggVorbis_File. This is part of this unit. }
function ov_create: TOggVorbisFile;
{ Free the memory of an OggVorbis_File after ov_clear. This is part of this unit. }
procedure ov_destroy(vf: TOggVorbisFile);
{ Open a vorbis file which is read using callbacks, where datasource is
  passed to each of them. The result is 0 on success. }
function ov_open_callbacks(datasource: Pointer; vf: TOggVorbisFile; initial: PChar;
  ibytes: clong; callbacks: TOVCallbacks): cint; cdecl; external;
{ Close a vorbis file }
function ov_clear(vf: TOggVorbisFile): cint; cdecl; external;
{ Use a bitstream of -1 for the current one }
function ov_info(vf: TOggVorbisFile; bitstream: cint): PVorbisInfo; cdecl; external;
{ The length in seconds, where a bitstream of -1 is the whole file }
function ov_time_total(vf: TOggVorbisFile; bitstream: cint): cdouble; cdecl; external;
{ Decode up to memsize bytes of samples and return the bytes written, which
  may be fewer than asked for, or 0 at the end of the file. For signed 16
  bit samples in the byte order of x86 use 0 for endian, 2 for size and 1
  for signed. }
function ov_read(vf: TOggVorbisFile; mem: Pointer; memsize, endian, size, signed: cint;
  bitstream: pcint): clong; cdecl; external;
{ Seek to the page nearest a position in samples, which is faster and less
  exact than seeking to the sample }
function ov_pcm_seek_page(vf: TOggVorbisFile; pos: cint64): cint; cdecl; external;
{ The current position in samples }
function ov_pcm_tell(vf: TOggVorbisFile): cint64; cdecl; external;

implementation

uses
  { On Windows the C runtime is linked by Codebot.Interop.MinGW }
  Codebot.Interop.MinGW;

{$linklib libcodebotaudio.a}
{$ifdef unix}
  {$linklib m}
  {$linklib c}
{$endif}

const
  { An OggVorbis_File is 944 bytes on 64 bit Linux and smaller elsewhere }
  OggVorbisFileSize = 1024;

function ov_create: TOggVorbisFile;
begin
  Result := AllocMem(OggVorbisFileSize);
end;

procedure ov_destroy(vf: TOggVorbisFile);
begin
  FreeMem(vf);
end;

end.
