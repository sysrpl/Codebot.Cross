{$i ../codebot/codebot.inc}

{$ifdef linux}
  {$ifdef lclgtk2}
    {$define gtk2gl}
  {$endif}
  {$ifdef lclgtk3}
    {$define gtk3gl}
  {$endif}
{$else ifdef windows}
  {$ifdef lclwin32}
    {$define win32gl}
  {$endif}
{$endif}

{ Reduce GLES driver version requirements to GLES2 by adding gles2 to your
  build defines or by removing the '.' infront of the directive below }

{.$define gles2}

{$ifndef gles2}
  { Require GLES3 driver support to run your program }
  {$define gles3}
	{$define glapi := 'ES3'}
{$else}
  { Require GLES2 driver support to run your program }
	{$define glapi := 'ES2'}
{$endif}
