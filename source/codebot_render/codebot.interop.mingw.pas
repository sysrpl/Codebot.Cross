(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ Codebot.Interop.MinGW links the static libraries built with MinGW-w64,
  which are libchipmunk2d.a and libcodebotaudio.a, followed by the C runtime
  they use.

  The internal linker of Free Pascal searches each static library once, in
  the order it first meets them, so every library using the C runtime must
  come before it. Where the libraries of a unit are placed depends on the
  order the units are loaded, which differs between a program compiled from
  source and one using the units of a package. This unit therefore links
  every MinGW-w64 library itself, ahead of the C runtime, and each unit
  linking one of them uses this unit in its implementation section. A library
  listed again by its own unit is ignored by the linker. A library which is
  linked but not called adds nothing to the program.

  A MinGW-w64 library added later is linked here as well, before the C
  runtime.

  The C runtime is copied next to the static libraries. The math functions
  are in mingwex, which reports errors through mingw32, the C library
  functions are imported from the Universal C Runtime by ucrt, gcc has the
  stack probe, and kernel32 imports the Windows functions the libraries call.

  On other systems this unit links nothing. }

unit Codebot.Interop.MinGW;

{$i render.inc}

interface

implementation

{$ifdef windows}
  { The libraries built with MinGW-w64 }
  {$linklib libchipmunk2d.a}
  {$linklib libcodebotaudio.a}
  { The C runtime they use }
  {$linklib libmingwex.a}
  {$linklib libmingw32.a}
  {$linklib libucrt.a}
  {$linklib libgcc.a}
  {$linklib libkernel32.a}
{$endif}

end.
