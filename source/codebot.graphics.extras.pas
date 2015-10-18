(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2015                               *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.graphics.extras.txt> }
unit Codebot.Graphics.Extras;

{$i codebot.inc}

interface

uses
  Classes, SysUtils, Graphics, Controls, Forms, LCLIntf, LCLType,
  Codebot.System;

{ Load a cursor from a resource by name and associate it with a cursor id }
procedure LoadCursor(const ResName: string; Cursor: TCursor);

{ Copy the screen pixels into a bitmap }
procedure CaptureScreen(Dest: TBitmap);

implementation

procedure LoadCursor(const ResName: string; Cursor: TCursor);
var
  C: TCursorImage;
begin
  C := TCursorImage.Create;
  try
    C.LoadFromResourceName(HINSTANCE, ResName);
    Screen.Cursors[Cursor] := C.ReleaseHandle;
  finally
    C.Free;
  end;
end;

procedure CaptureScreen(Dest: TBitmap);
var
  DC: HDC;
begin
  DC := GetDC(0);
  Dest.LoadFromDevice(DC);
  ReleaseDC(0, DC);
end;

end.

