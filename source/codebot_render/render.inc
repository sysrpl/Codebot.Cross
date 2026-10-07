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

{ Add videowidget to your build defines, or remove the '.' infront of the
  directive below, to include TVideoWidget in Codebot.Render.Widgets.Video. It
  plays video using libmpv, which is declared in Codebot.Interop.MPV and is
  then linked to every program using codebot_render. The libmpv development
  package must be installed to build, and libmpv to run. Without the define
  both units are empty and libmpv is not needed. }

{$define videowidget}

{ Select the OpenGL version used by Codebot.OpenGL by adding one of these to
  your build defines or by removing the '.' infront of one directive below.

  Desktop OpenGL versions:

    gl30 gl31 gl32 gl33 gl40 gl41 gl42 gl43 gl44 gl45 gl46

  Embedded OpenGL ES versions:

    gles20 gles30 gles31 gles32

  Only the functions belonging to the selected version and earlier versions
  are declared and loaded. If no version is selected gl33 is used. }

{.$define gl30}
{.$define gl31}
{.$define gl32}
{.$define gl33}
{.$define gl40}
{.$define gl41}
{.$define gl42}
{.$define gl43}
{.$define gl44}
{.$define gl45}
{$define gl46}
{.$define gles20}
{.$define gles30}
{.$define gles31}
{.$define gles32}

{ Add glcompat to your build defines to include the compatibility profile
  functions removed from the core profile in OpenGL 3.2 and to request a
  compatibility profile context. Compatibility functions are always included
  for versions before OpenGL 3.2. }

{.$define glcompat}

{$if defined(gl20) or defined(gl21)}
  {$error OpenGL 2 is no longer supported, select gl30 or a later version}
{$endif}

{$if defined(gles20) or defined(gles30) or defined(gles31) or defined(gles32)}
  {$define glesapi}
  {$if defined(gl30) or defined(gl31) or defined(gl32) or defined(gl33) or
    defined(gl40) or defined(gl41) or defined(gl42) or defined(gl43) or
    defined(gl44) or defined(gl45) or defined(gl46)}
    {$error Select either a desktop OpenGL version or an OpenGL ES version, but not both}
  {$endif}
{$elseif not (defined(gl30) or defined(gl31) or defined(gl32) or defined(gl33) or
  defined(gl40) or defined(gl41) or defined(gl42) or defined(gl43) or
  defined(gl44) or defined(gl45) or defined(gl46))}
  {$define gl33}
{$endif}

{ Each version implies all earlier versions }

{$ifdef gl46}{$define gl45}{$endif}
{$ifdef gl45}{$define gl44}{$endif}
{$ifdef gl44}{$define gl43}{$endif}
{$ifdef gl43}{$define gl42}{$endif}
{$ifdef gl42}{$define gl41}{$endif}
{$ifdef gl41}{$define gl40}{$endif}
{$ifdef gl40}{$define gl33}{$endif}
{$ifdef gl33}{$define gl32}{$endif}
{$ifdef gl32}{$define gl31}{$endif}
{$ifdef gl31}{$define gl30}{$endif}

{$ifdef gles32}{$define gles31}{$endif}
{$ifdef gles31}{$define gles30}{$endif}
{$ifdef gles30}{$define gles20}{$endif}

{$ifdef glesapi}
  {$undef glcompat}
{$else}
  {$ifndef gl32}
    {$define glcompat}
  {$endif}
{$endif}
