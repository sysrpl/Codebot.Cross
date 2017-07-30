  unit Main;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls,
  Codebot.System,
  Codebot.Animation,
  Codebot.Graphics,
  Codebot.Graphics.Types,
  Codebot.Controls.Scrolling,
  Codebot.Controls.Containers,
  Codebot.Controls.Sliders;

{ TForm1 }

type
  TForm1 = class(TForm)
    SpeedLabel: TLabel;
    ReverseBox: TCheckBox;
    DrawList: TDrawList;
    SizingPanel: TSizingPanel;
    SlideBar: TSlideBar;
    FrameRateLabel: TLabel;
    procedure DrawListDrawBackground(Sender: TObject; Surface: ISurface;
      Rect: TRectI);
    procedure DrawListDrawItem(Sender: TObject; Surface: ISurface;
      Index: Integer; Rect: TRectI; State: TDrawState);
    procedure DrawListSelectItem(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure SizingPanelRender(Sender: TObject; Surface: ISurface);
    procedure SlideBarChange(Sender: TObject);
  private
    FTimer: TAnimationTimer;
    FSecond: Integer;
    FFrame: Integer;
    procedure AnimatedTimer(Sender: TObject);
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ We are going to allow for variable speed playback }

var
  TimeFactor: Double;
  TimeLast: Double;
  TimeNow: Double;

{ Normally you never use step functions for animations, but
  in this case we don't want time to rewind or skip ahead
  if time factor changes. So in this case we step time forward
  only when a timer ticks. }

procedure TimeStep;
var
  T: Double;
begin
  if TimeLast = 0 then
    TimeLast := TimeQuery;
  T := TimeQuery;
  TimeNow := TimeNow + (T - TimeLast) * TimeFactor;
  TimeLast := T;
end;

var
  { The currently selected easing }
  CurrentEasing: TEasing;
  { The most basic of easings, linear }
  Linear: TEasing;
  { Our images }
  Food: TArrayList<IBitmap>;

const
  { Our image sources }
  FoodNames = 'apples,bananas,cherries,doughnuts,eggs,fish,grapes';

procedure TForm1.FormCreate(Sender: TObject);
var
  B: IBitmap;
  S: string;
begin
  Width := SizingPanel.Left + SizingPanel.Width + 8;
  Height := SlideBar.Top + SlideBar.Height + 8;
  FTimer := TAnimationTimer.Create(Self);
  FTimer.OnTimer := AnimatedTimer;
  FTimer.Enabled := True;
  { Default time to flow at a 1 to 1 rate }
  TimeFactor := 1;
  { Retrieve the linear easing function by name }
  Linear := Easings['Linear'];
  { Make the default current easing linear }
  CurrentEasing := Linear;
  { Our draw list will hold as many easings as are registered }
  DrawList.Count := Easings.Count;
  { Display 4 easings at a time }
  DrawList.Height := DrawList.ItemHeight * 4 + 4;;
  DrawList.ItemIndex := 0;
  SizingPanel.Height := DrawList.Height;
  { Load our food images }
  for S in FoodNames.Split(',') do
  begin
    B := NewBitmap;
    B.LoadFromFile(S + '.jpg');
    Food.Push(B);
  end;
end;

procedure TForm1.DrawListSelectItem(Sender: TObject);
begin
  { When you select an item in the draw list, change the current easing }
  CurrentEasing := Easings.Items[DrawList.ItemIndex].Value;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  FTimer.Enabled := False;
end;

{ We are going to default the animation sequence to 2 seconds,
  with a 0.5 second pause at both ends. This can be sped up or
  slowed down using time factor }

function AnimatedTime: Double;
begin
  Result := Remainder(TimeNow, 2);
  if Result <= 0.5 then
    Result := 0
  else if Result >= 1.5 then
    Result := 1
  else
    Result := Result - 0.5;
end;

{ This is where we draw the photos }

procedure TForm1.SizingPanelRender(Sender: TObject; Surface: ISurface);
const
  { Space the photos 150 pixels apart }
  Space = 150;

  { Draw an individual photo }
  procedure DrawPhoto(CurIndex, Index: Integer; Time: Float; Point: TPointF; Offset: Float);
  var
    Factor: Float;
    S, D: TRectF;
  begin
    { Corrent the index to fall within the Food array bounds }
    Index := (Index + Food.Length) mod Food.Length;
    { Rhis is the default photo size }
    Factor := 0.5;
    { If the index is current, make it bigger }
    if Index = CurIndex then
      Factor := Interpolate(CurrentEasing, Time, Factor, 0.7, ReverseBox.Checked)
    { Else if the index was current, make it smaller }
    else if Index = (CurIndex + Food.Length - 1) mod Food.Length then
      Factor := Interpolate(CurrentEasing, Time, 0.7, Factor, ReverseBox.Checked);
    { Get the source rectangle }
    S := Food[Index].ClientRect;
    { And the dest rectangle }
    D := S;
    { Scale the dest rectangle by the factor }
    D.Width := D.Width * Factor;
    D.Height := D.Height * Factor;
    { Center the photo at point }
    D.Center(Point);
    { Scroll the photo based on time and the easing }
    D.Y := Offset + Interpolate(CurrentEasing, Time, D.Y - Space, D.Y, ReverseBox.Checked);
    { Draw a white border }
    D.Inflate(10, 10);
    Surface.Rectangle(D);
    Surface.Fill(NewBrush(clWhite));
    { Draw the photo }
    D.Inflate(-10, -10);
    Food[Index].Surface.CopyTo(S, Surface, D);
  end;

var
  Time: Float;
  Index: Integer;
  R: TRectI;
  G: IGradientBrush;
  I: Integer;
begin
  { Capture the frames per second here }
  I := Trunc(TimeQuery);
  if I > FSecond then
  begin
    FSecond := I;
    FrameRateLabel.Caption := 'FPS: ' + IntToStr(FFrame);
    FFrame := 1;
  end
  else
    Inc(FFrame);
  { Fill the rect with a black to gray gradient }
  R := SizingPanel.ClientRect;
  G := NewBrush(0, R.Top, 0, R.Bottom);
  G.AddStop(clBlack, 0);
  G.AddStop(clGray, 1);
  Surface.FillRect(G, R);
  { Get the animation timing }
  Time := AnimatedTime;
  { Get the current photo index based on TimeNow }
  Index := Trunc(TimeNow / 2) mod Food.Length;
  { And draw all the photos that might currently be visible }
  for I := -2 to 2 do
    DrawPhoto(Index, Index - I, Time, R.MidPoint, Space * I);
end;

{ Draw a background with a dashed set of time lines }

procedure TForm1.DrawListDrawBackground(Sender: TObject; Surface: ISurface;
  Rect: TRectI);
const
  Align = 0.5;
var
  Time: Float;
  R: TRectF;
  P: IPen;
begin
  { Fill with the current control color }
  FillRectColor(Surface, Rect, DrawList.CurrentColor);
  R := Rect;
  { Create a silver pen }
  P := NewPen(clSilver);
  R.X := R.MidPoint.X + 10;
  R.Width := Rect.Width - R.X - 30 - Align;
  { Add the left time boundary to the path }
  Surface.MoveTo(R.Left, R.Top);
  Surface.LineTo(R.Left, R.Bottom);
  { Add the left right time boundary to the path }
  Surface.MoveTo(R.Right, R.Top);
  Surface.LineTo(R.Right, R.Bottom);
  { Stroke the time boundary }
  Surface.Stroke(P);
  { Switch to a red dashed pen }
  P.Color := clRed;
  P.LinePattern := pnDash;
  Time := AnimatedTime;
  { We'll draw the red dotten line as the actual time }
  R.Left := Interpolate(Linear, Time, R.Left, R.Right);
  Surface.MoveTo(R.Left, R.Top);
  Surface.LineTo(R.Left, R.Bottom);
  { And stroke the path with the red pen }
  Surface.Stroke(P);
end;

{ This method draws the items in the TDrawList }

procedure TForm1.DrawListDrawItem(Sender: TObject; Surface: ISurface;
  Index: Integer; Rect: TRectI; State: TDrawState);
var
  KeyValue: TEasingKeyValue;
  Time: Double;
  R: TRectF;
  A, B, C: TPointF;
begin
  { Retrieve the KeyValue from to Easings dicntionary }
  KeyValue := Easings.Items[Index];
  { If the item is selected, but it a nice gradiant outline and fill }
  if dsSelected in State then
    FillRectSelected(Surface, Rect, 5);
  { Retrieve the current animation time }
  Time := AnimatedTime;
  { Define the rectangle where the easing should be drawn }
  R := Rect;
  R.Inflate(-10, -30);
  R.Offset(0, -5);
  R.Right := R.Left + 100;
  R.Bottom := R.Bottom - 2;
  { And give it to our Codebot.Graphics.DrawEasing procedure }
  DrawEasing(Surface, Theme.Font, R, KeyValue.Value, ReverseBox.Checked, Time);
  R.Top := R.Bottom + 5;
  R.Bottom := Rect.Bottom;
  { Draw the name beneath the easing }
  Surface.TextOut(Theme.Font, KeyValue.Key, R, drCenter);
  { Create a block to show the easing motion }
  R := TRectF.Create(20, 20);
  { Create two points A and B }
  A := Rect.MidPoint;
  A.Offset(10, 0);
  B := A;
  B.X := Rect.Right - 30;
  C.Y := A.Y;
  { And interpolate A and B to create C, using the X axis }
  C.X := Interpolate(KeyValue.Value, Time, A.X, B.X, ReverseBox.Checked);
  { Center our block on C }
  R.Center(C);
  { And fill it with black }
  Surface.FillRect(NewBrush(clBlack), R);
end;

procedure TForm1.SlideBarChange(Sender: TObject);
begin
  { When the slider changes, update the caption }
  SpeedLabel.Caption := 'Speed: %f'.Format([SlideBar.Position]);
  { And the time factor }
  TimeFactor := SlideBar.Position;
end;

procedure TForm1.AnimatedTimer(Sender: TObject);
begin
  { Advance time, but don't reverse it }
  TimeStep;
  { Redraw our controls }
  DrawList.Invalidate;
  SizingPanel.Invalidate;
end;

end.

