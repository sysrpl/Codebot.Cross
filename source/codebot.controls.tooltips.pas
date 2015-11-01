(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2015                               *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.controls.tooltips.txt> }
unit Codebot.Controls.Tooltips;

{$mode delphi}

interface

uses
  Classes, SysUtils, Graphics, Controls, Forms,
  Codebot.System,
  Codebot.Graphics,
  Codebot.Graphics.Types;

var
  UseTipify: Boolean;

procedure Tipify(Control: TControl);

implementation

var
  Tip: ISplash;
  TipFont: IFont;

procedure Tipify(Control: TControl);
var
  Surface: ISurface;
  Shape: TRectF;
  Rect: TRectI;
  P: IPen;
  G: IGradientBrush;
  Size, Point: TPointI;
  S: string;
begin

  if Tip = nil then
  begin
    Tip := NewSplash;
    Tip.Bitmap.SetSize(100, 100);
    TipFont := NewFont(Screen.HintFont);
    TipFont.Color := TColor($0D4848);
  end;
  if Control = nil then
  begin
    Tip.Visible := False;
    Exit;
  end;
  S := Control.Hint;
  S := S.Trim;
  if S.Length < 1 then
  begin
    Tip.Visible := False;
    Exit;
  end;
  Size := TPointI(Tip.Bitmap.Surface.TextSize(TipFont, S));
  Tip.Bitmap.SetSize(Size.X + 30, Size.Y + 40);
  Surface := Tip.Bitmap.Surface;
  Surface.Clear(clTransparent);
  Rect := Tip.Bitmap.ClientRect;
  Shape := Rect;
  Shape.Inflate(-8, -8);
  Shape.Bottom := Shape.Bottom - 16;
  Surface.RoundRectangle(Shape, 6);
  Surface.Path.Add;
  Surface.MoveTo(Shape.MidPoint.X, Shape.Bottom + 12);
  Surface.LineTo(Shape.MidPoint.X - 16, Shape.Bottom - 4);
  Surface.LineTo(Shape.MidPoint.X + 16, Shape.Bottom - 4);
  Surface.Path.Close;
  P := NewPen(TColorB(clBlack).Fade(0.05), 10);
  while P.Width > 1 do
  begin
    Surface.Stroke(P, True);
    P.Width := P.Width - 2;
  end;
  Surface.Stroke(NewPen(TColor($259494), 2), True);
  G := NewBrush(Shape.TopLeft, Shape.BottomLeft);
  G.AddStop(TColor($D5F2F2), 0);
  G.AddStop(TColor($92E9E9), 0.4);
  Surface.Fill(G);
  Surface.TextOut(TipFont, S, Shape, drCenter);
  Point := TPointI.Create(Control.ClientWidth div 2, -Size.Y - 4);
  Point := Control.ClientToScreen(Point);
  Rect := Tip.Bitmap.ClientRect;
  Rect.Center(Point);
  Tip.Move(Rect.X,Rect.Y);
  Tip.Update;
  Tip.Visible := True;
end;

end.

