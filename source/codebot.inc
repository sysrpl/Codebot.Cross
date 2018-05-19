{$ifndef fpc}
'This library requires the free pascal compiler'
{$endif}
{$if fpc_fullversion < 30000}
'This library requires the free pascal 3 or greater'
{$endif}

{$mode delphi}

{$z4}
{$macro on}

{$ifdef windows}
  {$define apicall := stdcall}
{$else}
  {$define apicall := cdecl}
{$endif}

{$if defined(linux) and defined(lclgtk2)}
  {$define linuxgtk}
{$endif}

{$if defined(linux) and defined(lclqt)}
  {$define linuxqt}
{$endif}

{$WARN 4055 off : Conversion between ordinals and pointers is not portable}
{$WARN 5024 off : Parameter "$1" not used}

