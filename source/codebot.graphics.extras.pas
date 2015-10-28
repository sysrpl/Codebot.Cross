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
  Codebot.System,
  Codebot.Graphics.Types;

{ Load a cursor from a resource by name and associate it with a cursor id }
procedure LoadCursor(const ResName: string; Cursor: TCursor);

{ Copy the screen pixels into a bitmap }
procedure CaptureScreen(Dest: TBitmap);

procedure CaptureScreenRect(Rect: TRectI; Dest: TBitmap);

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

procedure CaptureScreenRect(Rect: TRectI; Dest: TBitmap);
var
  DC: HDC;
begin
  if Rect.Empty then
    Exit;
  DC := GetDC(0);
  if (Dest.Width <> Rect.Width) or (Dest.Height <> Rect.Height) then
    Dest.SetSize(Rect.Width, Rect.Height);
  BitBlt(HDC(Dest.Canvas.Handle), 0, 0, Rect.Width, Rect.Height,
    DC, Rect.Left, Rect.Top, SRCCOPY);
  ReleaseDC(0, DC);
end;

end.
