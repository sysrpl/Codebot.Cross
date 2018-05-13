unit Main;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, LCLIntf,
  LCLType, Buttons,
  Codebot.System,
  Codebot.Graphics,
  Codebot.Graphics.Types,
  Codebot.Geometry,
  Codebot.Forms.Widget,
  Codebot.Animation,
  Codebot.Controls.Buttons;

{ TClockWidget }

type
  TClockWidget = class(TWidget)
    Images: TImageStrip;
    CloseButton: TThinButton;
    procedure FormClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
  private
    procedure ClockTick(Sender: TObject);
  protected
    FBitmap: IBitmap;
    FClose: IBitmap;
    FMoused: Boolean;
    FMouseOpacity: Float;
    procedure Render; override;
  end;

var
  ClockWidget: TClockWidget;

implementation

{$R *.lfm}

{ TClockWidget }

{ Since we are using vector graphics, we can scale the widget
  size using a scaling factor }

var
  Factor: Float;
  { For our example Size always equals Round(Factor * 256) }
  Size: Integer;

procedure DrawClock(Bitmap: IBitmap);
const
  { Define our colors }
  clClockFace: TColorB = (Blue: 255; Green: 248; Red: 248; Alpha: 255);
  clLens: TColorB = (Blue: 255; Green: 220; Red: 220; Alpha: 255);
  clMinuteHand: TColorB = (Blue: 80; Green: 72; Red: 72; Alpha: 255);
  clSecondHand: TColorB = (Blue: 32; Green: 32; Red: 168; Alpha: 255);
  clMicroHand: TColorB = (Blue: 220; Green: 100; Red: 100; Alpha: 255);
  clShadowHand: TColorB = (Blue: 0; Green: 0; Red: 0; Alpha: 50);

  { Draw the ticks around the clock face }

  procedure DrawTicks(Surface: ISurface);
  var
    M: TMatrix4x4;
    A, B, C: TVec3;
    P: IPen;
    I: Integer;
  begin
    A := Vec(Size / 2, 35 * Factor, 0);
    B := Vec(Size / 2, 43 * Factor, 0);
    C := Vec(Size / 2, Size / 2, 0);
    { Use a matrix from Codebot.Geometry to rotate two points
      around the clock face }
    M := StockMatrix;
    { 12 hours in 360 degrees }
    M.RotateAt(0, 0, 360 / 12 , C);
    for I := 0 to 11 do
    begin
      Surface.MoveTo(A.X, A.Y);
      Surface.LineTo(B.X, B.Y);
      A := M * A;
      B := M * B;
    end;
    { Use a fat pen for the hour ticks }
    P := NewPen(clGray, 3.5 * Factor);
    P.Color := P.Color.Darken(0.1);
    P.LineCap := cpRound;
    { Stroke the hours }
    Surface.Stroke(P);
    M := StockMatrix;
    { 60 minutes in 360 degrees }
    M.RotateAt(0, 0, 360 / 60 , C);
    A := Vec(Size / 2, 35 * Factor, 0);
    B := Vec(Size / 2, 37 * Factor, 0);
    for I := 0 to 59 do
    begin
      if I mod 5 > 0 then
      begin
        Surface.MoveTo(A.X, A.Y);
        Surface.LineTo(B.X, B.Y);
      end;
      A := M * A;
      B := M * B;
    end;
    { Use a thin pen }
    P.Width := Factor;
    { And stroke the minute ticks }
    Surface.Stroke(P);
    { The micro second clock is offset to the side }
    A := Vec(Size / 2 + 50 * Factor, Size / 2 - 10 * Factor, 0);
    B := Vec(Size / 2 + 50 * Factor, Size / 2 - 8 * Factor, 0);
    C := Vec(Size / 2 + 50 * Factor, Size / 2, 0);
    M := StockMatrix;
    { Draw 12 ticks for the micro seconds }
    M.RotateAt(0, 0, 360 / 12 , C);
    for I := 0 to 11 do
    begin
      Surface.MoveTo(A.X, A.Y);
      Surface.LineTo(B.X, B.Y);
      A := M * A;
      B := M * B;
    end;
    { Use a thin pen }
    P.Width := Factor;
    { And stroke the micro second ticks }
    Surface.Stroke(P);
  end;

  { Draw the microsecond hand }

  procedure DrawMicros(Surface: ISurface; Micro: Double);
  var
    X, Y: Float;
  begin
    { Clear the previous transform (not really needed, but just in case we
      change something later) }
    Surface.Matrix.Identity;
    X := Size / 2 + 50 * Factor;
    Y := Size / 2;
    { To rotate hands, we center the rotation on an x and y coordinate }
    Surface.Matrix.Translate(-X, -Y);
    { Rotate the micro second hand }
    Surface.Matrix.Rotate(Micro / 12 * PI * 2);
    { Move back to the original position  }
    Surface.Matrix.Translate(X, Y);
    { Add a microsecond hand to the page }
    Surface.MoveTo(X + 0.25 * Factor, Y + -6 * Factor);
    Surface.LineTo(X + Factor, Y + 2 * Factor);
    Surface.LineTo(X + -Factor, Y + 2 * Factor);
    Surface.LineTo(X - 0.25 * Factor, Y + -6 * Factor);
    Surface.Path.Close;
    { Close and fill using color clMicroHand }
    Surface.Fill(NewBrush(clMicroHand));
    { Reset the transform }
    Surface.Matrix.Identity;
  end;

  procedure DrawHours(Surface: ISurface; Hour: Double);
  var
    X: Float;
    I: Integer;
  begin
    { Draw larger hands two times, once for a shadow, then over it again
      with the actual hand }
    for I := 1 downto 0 do
    begin
      Surface.Matrix.Identity;
      Surface.Matrix.Translate(-Size / 2, -Size / 2);
      { Rotate the hour hand }
      Surface.Matrix.Rotate(Hour / 12 * PI * 2);
      Surface.Matrix.Translate(Size / 2 - I / 2 * Factor, Size / 2 + I * 2 * Factor);
      { Create the hour hand path }
      X := Size / 2;
      Surface.MoveTo(X + 3 * Factor, 70 * Factor);
      Surface.LineTo(X + 3 * Factor, X + 15 * Factor);
      Surface.LineTo(X - 3 * Factor, X + 15 * Factor);
      Surface.LineTo(X - 3 * Factor, 70 * Factor);
      Surface.Path.Close;
      { and fill the hand or the shadow, depending on the pass }
      if I = 0 then
        Surface.Fill(NewBrush(clMinuteHand))
      else
        Surface.Fill(NewBrush(clShadowHand));
      Surface.Matrix.Identity;
    end;
  end;

  procedure DrawMinutes(Surface: ISurface; Minute: Double);
  var
    X: Float;
    I: Integer;
  begin
    { Same code as the hour hand, but with minor adjustments }
    for I := 1 downto 0 do
    begin
      Surface.Matrix.Identity;
      Surface.Matrix.Translate(-Size / 2, -Size / 2);
      Surface.Matrix.Rotate(Minute / 60 * PI * 2);
      Surface.Matrix.Translate(Size / 2 - I / 2 * Factor, Size / 2 + I * 2 * Factor);
      X := Size / 2;
      Surface.MoveTo(X + 2 * Factor, 50 * Factor);
      Surface.LineTo(X + 2 * Factor, X + 22 * Factor);
      Surface.LineTo(X - 2 * Factor, X + 22 * Factor);
      Surface.LineTo(X - 2 * Factor, 50 * Factor);
      Surface.Path.Close;
      if I = 0 then
        Surface.Fill(NewBrush(clMinuteHand))
      else
        Surface.Fill(NewBrush(clShadowHand));
      Surface.Matrix.Identity;
    end;
  end;

  procedure DrawSeconds(Surface: ISurface; Second: Double);
  var
    A, B, C: TPointF;
    R: TRectF;
    Color: TColorB;
    I: Integer;
  begin
    { Mostly the same code as the hour hand }
    for I := 1 downto 0 do
    begin
      Surface.Matrix.Identity;
      Surface.Matrix.Translate(-Size / 2, -Size / 2);
      Surface.Matrix.Rotate(Second / 60 * PI * 2);
      Surface.Matrix.Translate(Size / 2 - I / 2 * Factor, Size / 2 + I * 2 * Factor);
      { But this time we define a clipping path }
      Surface.MoveTo(0, 0);
      Surface.LineTo(0, Size);
      Surface.LineTo(Size, Size);
      Surface.LineTo(Size, 0);
      Surface.Path.Close;
      R := TRectF.Create(10 * Factor, 10 * Factor);
      R.Center(Size / 2, Size / 2);
      R.Inflate(-2 * Factor, -2 * Factor);
      { Because the second hand has a neat hole in it }
      Surface.Ellipse(R);
      R := TRectF.Create(4 * Factor, 4 * Factor);
      R.Center(Size / 2, 60 * Factor);
      Surface.Ellipse(R);
      { And clip the hole out }
      Surface.Path.Clip;
      { A, B, and C are points on the minute hand when it's positioned at zero }
      A := TPointF.Create(Size / 2, 35 * Factor);
      B := TPointF.Create(Size / 2 + 1.5 * Factor,
        Size / 2 + 25 * Factor);
      C := TPointF.Create(Size / 2 - 1.5 * Factor,
        Size / 2 + 25 * Factor);
      Surface.MoveTo(A.X - 0.25 * Factor, A.Y);
      Surface.LineTo(A.X + 0.25 * Factor, A.Y);
      Surface.LineTo(B.X, B.Y - 30 * Factor);
      Surface.LineTo(B.X, B.Y);
      Surface.LineTo(C.X, C.Y);
      Surface.LineTo(C.X, C.Y - 30 * Factor);
      R := TRectF.Create(10 * Factor, 10 * Factor);
      R.Center(Size / 2, Size / 2);
      Surface.Ellipse(R);
      R := TRectF.Create(6 * Factor, 6 * Factor);
      R.Center(Size / 2, 60 * Factor);
      { Another hole }
      Surface.Ellipse(R);
      { Color with the hand, or the shadow, depending on the pass }
      if I = 0 then
        Color := clSecondHand
      else
        Color := clShadowHand;
      { Fill the path }
      Surface.Fill(NewBrush(Color));
      { Unclip }
      Surface.Path.Unclip;
      { And reset the matrix }
      Surface.Matrix.Identity;
    end;
  end;

  { Draw a light reflection above and below the center of the clock face }

  procedure DrawLens(Surface: ISurface);
  var
    R: TRectF;
    C: TColorB;
    G: IGradientBrush;
  begin
    R := TRectF.Create(Size, Size);
    R.Left := -Size * 1.25;
    R.Top := R.Left;
    R.Offset(0, 25 * Factor);
    { Use a big gradient }
    G := NewBrush(R);
    { With an off blue white color }
    C := clLens;
    G.AddStop(C.Fade(0), 0);
    { Fade in the reflection here }
    G.AddStop(C.Fade(0), 0.88);
    { Sharply }
    G.AddStop(C.Fade(0.15), 0.885);
    G.AddStop(C.Fade(0), 1);
    { Repeat for the top reflections, with different values }
    R := TRectF.Create(Size, Size);
    R.Inflate(-23 * Factor, -23 * Factor);
    Surface.Ellipse(R);
    Surface.Fill(G);
    R := TRectF.Create(Size, Size);
    R.Left := -Size * 0.25;
    R.Top := R.Left;
    R.Right := R.Right + Size * 1.25;
    R.Bottom := R.Right;
    R.Offset(0, 25 * -Factor);
    G := NewBrush(R);
    G.AddStop(C.Fade(0), 0);
    G.AddStop(C.Fade(0), 0.75);
    G.AddStop(C.Fade(0.1), 0.755);
    G.AddStop(C.Fade(0), 0.79);
    R := TRectF.Create(Size, Size);
    R.Inflate(-23 * Factor, -23 * Factor);
    Surface.Ellipse(R);
    Surface.Fill(G);
  end;

var
  Time: Double;
  Hour, Minute, Second, Micro: Double;
  Surface: ISurface;
  R: TRectF;
  C: TColorB;
  G: IGradientBrush;
begin
  Surface := Bitmap.Surface;
  { Erase the last clock }
  Surface.Clear(clTransparent);
  { Draw the border ring }
  R := Bitmap.ClientRect;
  R.Inflate(-2 * Factor, -2 * Factor);
  C := clClockFace;
  G := NewBrush(R.TopRight, R.BottomLeft);
  G.AddStop(C.Darken(0.4), 0);
  G.AddStop(C.Fade(0), 0.75);
  Surface.Ellipse(R);
  Surface.Fill(G);
  { Draw the big ring }
  R := Bitmap.ClientRect;
  R.Top := R.Top - 200 * Factor;
  R.Right := R.Right + 200 * Factor;
  G := NewBrush(R);
  G.AddStop(C, 0);
  G.AddStop(C.Darken(0.8), 1);
  R := Bitmap.ClientRect;
  R.Inflate(-4 * Factor, -4 * Factor);
  Surface.Ellipse(R);
  Surface.Fill(G);
  { Draw the inner ring  }
  R := Bitmap.ClientRect;
  R.Left := R.Left - 150 * Factor;
  R.Bottom := R.Bottom + 150 * Factor;
  G := NewBrush(R);
  G.AddStop(C, 0);
  G.AddStop(C.Darken(0.8), 1);
  R := Bitmap.ClientRect;
  R.Inflate(-23 * Factor, -23 * Factor);
  Surface.Ellipse(R);
  Surface.Fill(G);
  { And finally a solid color for the clock face}
  R := Bitmap.ClientRect;
  R.Inflate(-28 * Factor, -28 * Factor);
  Surface.Ellipse(R);
  Surface.Fill(NewBrush(C));
  { Draw the tick marks on top of the clock face }
  DrawTicks(Surface);
  { Extract the hours, minutes, seconds, and micro seconds }
  Time := Frac(Now);
  Hour := Remainder(Time * 24, 12);
  Minute := Remainder(Time * 24 * 60, 60);
  Second := Remainder(Time * 24 * 60 * 60, 60);
  Micro := (Second - Trunc(Second)) * 12;
  { In order, micros, hours, minutes, then seconds }
  DrawMicros(Surface, Micro);
  DrawHours(Surface, Hour);
  DrawMinutes(Surface, Minute);
  DrawSeconds(Surface, Second);
  { Draw a shadow creeping across the clock face }
  C := clBlack;
  R := Bitmap.ClientRect;
  R.Left := R.Left - 24.5 * Factor;
  R.Bottom := R.Bottom + 24.5 * Factor;
  G := NewBrush(R);
  G.AddStop(C.Fade(0.12), 0);
  G.AddStop(C.Fade(0.16), 0.35);
  G.AddStop(C.Fade(0.2), 0.45);
  G.AddStop(C.Fade(0.6), 1);
  R := Bitmap.ClientRect;
  R.Inflate(-28 * Factor, -28 * Factor);
  Surface.Ellipse(R);
  Surface.Fill(G);
  { And finally draw the clock lense }
  DrawLens(Surface);
end;

procedure TClockWidget.FormClick(Sender: TObject);
var
  P: TPointI;
begin
  P := Mouse.CursorPos;
  P.Offset(-Left, -Top);
  if (P.Y < FClose.Height) and (P.X > Width - FClose.Width) then
    Close;
end;

procedure TClockWidget.FormCreate(Sender: TObject);
//var
  //Stream: TStream;
begin
  EdgeSizable := [esNW, esSE, esSW];
  Width := 220;
  Height := Width;
  AspectRatio := 1;
  OnTick := ClockTick;
  FBitmap := NewBitmap(Size, Size);
  FClose := NewBitmap;
  //Stream := TResourceStream.Create(HInstance, 'CLOSE', RT_RCDATA);
  try
    // FClose.LoadFromStream(Stream);
  finally
    //Stream.Free;
  end;
end;

procedure TClockWidget.ClockTick(Sender: TObject);
begin
  if not Dragged then
    Invalidate
  else if Sized then
    Invalidate;
end;

procedure TClockWidget.Render;
var
  PriorMoused: Boolean;
  Alpha: Float;
  P: TPointI;
  B: IGradientBrush;
  R: TRectI;
begin
  R := BoundsRect;
  P := Mouse.CursorPos;
  PriorMoused := FMoused;
  FMoused := R.Contains(P);
  if FMoused <> PriorMoused then
    if FMoused then
      Animator.Animate(FMouseOpacity, 1)
    else
      Animator.Animate(FMouseOpacity, 0);
  Alpha := FMouseOpacity;
  if Sized then
  begin
    R := ClientRect;
    B := NewBrush(R.TopLeft, R.BottomLeft);
    B.AddStop(Rgba(clHighlight, 0.3), 0);
    B.AddStop(Rgba(clHighlight, 0.2), 0.66);
    B.AddStop(Rgba(clHighlight, 0.1), 1);
    Surface.FillRect(B ,R);
    R := FClose.ClientRect;
    R.X := Width - R.Width - 4;
    R.Y := 4;
    FClose.Surface.CopyTo(FClose.ClientRect, Surface, R, $FF);
  end
  else if FMoused or Animator.Animated then
  begin
    R := ClientRect;
    B := NewBrush(R.TopLeft, R.BottomLeft);
    B.AddStop(TColorB($A7B2B6).Fade(Alpha), 0);
    B.AddStop(TColorB($D8E9EC).Fade(Alpha), 0.4);
    B.AddStop(TColorB($A7B2B6).Fade(Alpha), 1);
    Surface.RoundRectangle(R, 8);
    Surface.Fill(B, True);
    Surface.Stroke(NewPen(TColorB($9D9E9E).Fade(Alpha), 1));
    CloseButton.Visible := True;
  end;
  if Sized then
  begin
    R := ClientRect;
    R.Inflate(-4, -4);
    Surface.Ellipse(R);
    Surface.Fill(NewBrush(Rgba(clHighlight, 0.5)), True);
  end
  else
  begin
    Size := Width;
    Factor := Width / 256;
    FBitmap.SetSize(Size, Size);
    DrawClock(FBitmap);
    FBitmap.Surface.CopyTo(FBitmap.ClientRect, Surface, FBitmap.ClientRect);
    R := FClose.ClientRect;
    R.X := Width - R.Width - 4;
    R.Y := 4;
    if Alpha = 0 then
      CloseButton.Visible := False
    else
      CloseButton.Visible := True;
  end;
end;

procedure TClockWidget.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

end.

