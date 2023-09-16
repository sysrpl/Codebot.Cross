{$mode delphi}
{$modeswitch multihelpers}

{$z4}
{$macro on}

{$WARN 3177 off : Some fields coming after "$1" were not initialized}
{$WARN 5024 off : Parameter "$1" not used}
{$WARN 5094 off : Function result variable of a managed type does not seem to be initialized}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
{$WARN 6060 off : }

{$ifdef linux}
  {$define apicall := cdecl}
  {$ifdef lclgtk2}
    {$define platformok}
  {$endif}
  {$ifdef lclgtk3}
    {$define platformok}
  {$endif}
{$endif}

{$ifdef windows}
  {$define apicall := stdcall}
  {$ifdef lclwin32}
    {$define platformok}
  {$endif}
{$endif}

{$ifndef platformok}
'This library requires windows win32, linux gtk2, or linux gtk3 widgetsets'
{$endif}
{$ifndef fpc}
'This library requires the free pascal compiler'
{$endif}
{$if fpc_fullversion < 30000}
'This library requires the free pascal 3 or greater'
{$endif}


