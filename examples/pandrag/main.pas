unit Main;

{$mode delphi}

interface

uses
	Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Types
  Codebot.System,
  Codebot.Graphics,
  Codebot.Graphics.Types;

{ TForm1 }

type
	TForm1 = class(TForm)
		procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer);
		procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
		procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer);
		procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
			WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
		procedure FormPaint(Sender: TObject);
	private
		{ private declarations }
	public
		{ public declarations }
	end;

var
	Form1: TForm1;

implementation

{$R *.lfm}

var
  Pan: TPointF;
  Zoom: Float = 1;

  Drag: Boolean;
  DragPoint: TPointF;

procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Drag := Button = mbLeft;
  if Drag then
  begin
    DragPoint.X := X;
    DragPoint.Y := Y;
  end;
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if Drag then
  begin
    Pan.X := Pan.X + X - DragPoint.X;
    Pan.Y := Pan.Y + Y - DragPoint.Y;
    DragPoint.X := X;
    DragPoint.Y := Y;
    Invalidate;
  end;
end;

procedure TForm1.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    Drag := False;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  Z: Float;
begin
  if WheelDelta < 0 then
    Z := Zoom + 0.25
  else
    Z := Zoom - 0.25;
  if Z < 0.25 then
    Z := 0.25;
  if Z <> Zoom then
  begin
    Zoom := Z;
    Invalidate;
  end;
end;

procedure TForm1.FormPaint(Sender: TObject);
const
  Help = 'Pan using the left mouse button, zoom using the mouse wheel';
var
  S: ISurface;
  R: TRectF;
  B: IBrush;
begin
  { Create a surface }
  S := NewSurface(Canvas);
  { Zoom our surface }
  S.Matrix.Scale(Zoom, Zoom);
  { Pan our surface }
  S.Matrix.Translate(Pan.X, Pan.Y);
  { Fill it with white }
  S.Clear(clWhite);
  { Write out some instructions }
  R := ClientRect;
  { Give the text a margin of 8 }
  R.Inflate(-8, -8);
  S.TextOut(NewFont(Font), Help, R, drWrap);
  { Draw some shapes }
  R := TRectF.Create(50, 50, 100, 100);
  { A rectangle }
  S.Rectangle(R);
  { Stroke in red and preserve the path for a fill pattern }
  S.Stroke(NewPen(clRed, 4), True);
  { Normal brush pattern scalling can cause fuzzy patterns }
  B := Brushes.Brick(clRed, clTransparent);
  S.Fill(B);
  { A circle }
  R := TRectF.Create(175, 50, 100, 100);
  S.Ellipse(R);
  { Stroke in green and preserve the path for a fill pattern }
  S.Stroke(NewPen(clGreen, 4), True);
  { We can correct fuzzy pattern by growing the pattern size with the zoom }
  B := Brushes.ZigZag(clGreen, clTransparent, Zoom, Round(DefBrushSize * Zoom));
  { And rescaling the brush }
  B.Matrix.Scale(1 / Zoom, 1 / Zoom);
  S.Fill(B);
end;

end.

