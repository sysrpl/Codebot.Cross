unit main;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Docker.Spacing,
  Codebot.System,
  Codebot.Input.MouseMonitor,
  Codebot.Controls.Buttons,
  Codebot.Graphics,
  Codebot.Graphics.Types;

{ TForm1 }

type
  TForm1 = class(TForm)
    LargeImages: TImageStrip;
    SmallImages: TImageStrip;
    ButtonImages: TImageStrip;
    ToggleButton: TThinButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
    procedure ToggleButtonClick(Sender: TObject);
    procedure ToggleButtonMouseEnter(Sender: TObject);
    procedure ToggleButtonMouseLeave(Sender: TObject);
    procedure ToggleButtonMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    FBackground: IBitmap;
    FDesktopDisplay: Boolean;
    FDocker: TDocker;
    FMouseCoord: Integer;
    FSplash: ISplash;
    FArea: TRectI;
    procedure SetDesktopDisplay(Value: Boolean);
    procedure UpdateSplash(X, Y: Integer);
    procedure MouseNotify(Kind: TMouseNotifyKind;
      Button: TMouseButton; X, Y: Integer);
  public
    property DesktopDisplay: Boolean read FDesktopDisplay write SetDesktopDisplay;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
const
  Names = 'Home,Folder,Save,Calculator,Search,Options,Stop,Help,Pictures,Print,Control Panel,Recycle Bin';
begin
  ClientWidth := 836;
  ClientHeight := 196;
  FBackground := NewBitmap;
  FBackground.LoadFromFile('flower.jpg');
  FDocker := TDocker.Create(Names);
  FDocker.Count := 12;
  FDocker.SmallSize := 48;
  FDocker.Stretch := FDocker.SmallSize * 2;
  FDocker.LargeSize := LargeImages.Size;
  { Copy the larger images }
  SmallImages.Assign(LargeImages);
  { Resample the small images using the best possible filtering }
  SmallImages.Resample(FDocker.SmallSize);
  FSplash := NewSplash;
  FArea := ClientRect;
  FArea.Center(Screen.Width div 2, Screen.Height - FArea.Height div 2 - 20);
  FSplash.Bitmap.SetSize(FArea.Width, FArea.Height);
  FSplash.Move(FArea.X, FArea.Y);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  MouseMonitor.Remove(MouseNotify);
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if FDesktopDisplay then Exit;
  if X <> FMouseCoord then
  begin
    FMouseCoord := X;
    Invalidate;
  end;
end;

{ Draw a docker unto a surface given a mouse location, area to center it upon,
  font to draw text, and small and large images. Images should probably have
  been resampled using IBitmap.Resample with bicubic filtering for best appearance }

procedure DrawDocker(Surface: ISurface; Docker: TDocker; MouseCoord: Integer;
  Area: TRectI; Font: TFont; SmallImages, LargeImages: TImageStrip);
var
  DockerItems: TDockerItems;
  DockerIndex: Integer;
  R: TRectF;
  F: IFont;
  P: IPen;
  I: Integer;
begin
  { Calculate the docker items based on a mouse coord and area.
    Note: A docker can be arranged horizontal or vertical, with text above
    below, left, or to the right. In this procedure we assume horizonal
    with text above. Writing to those other alignments is trivial given this
    example }
  DockerIndex := Docker.MouseMove(MouseCoord, Area.Width, DockerItems);
  { For each docker item  }
  for I := 0 to Length(DockerItems) - 1 do
  begin
    { Get the rectangle }
    R := TRectF.Create(DockerItems[I].Size, DockerItems[I].Size);
    R.X := DockerItems[I].Offset;
    R.Y := Area.Height - R.Height - 24;
    { If it's selected, draw a highlight }
    if I = DockerIndex then
      FillRectSelected(Surface, TRectI(R), 8);
    { If it's smallest use the small resampled image strip }
    if R.Width = Docker.SmallSize then
      SmallImages.Draw(Surface, I, TRectI(R))
    { All other sizes are streched using the larger image strip }
    else
      LargeImages.Draw(Surface, I, TRectI(R));
    { If the item is selected }
    if I = DockerIndex then
    begin
      R.Bottom := R.Top - 4;
      R.Top := 0;
      { Prepare the font in white }
      F := NewFont(Font);
      F.Size := 24;
      F.Style := [fsBold];
      F.Color := clWhite;
      { But first create a path }
      Surface.TextOut(F, Docker.Name[I], R, drDown, False);
      P := NewPen(clBlack, 6);
      { And stroke the path over and over again with a
        dark nearly transparent pen }
      P.Color := P.Color.Fade(0.25);
      while P.Width > 0 do
      begin
        Surface.Stroke(P, True);
        P.Width := P.Width - 2;
      end;
      { And finally write out the text in white }
      Surface.TextOut(F, Docker.Name[I], R, drDown);
    end;
  end;
end;

procedure TForm1.FormPaint(Sender: TObject);
var
  Surface: ISurface;
begin
  Surface := NewSurface(Canvas);
  FBackground.Surface.CopyTo(FBackground.ClientRect, Surface, FBackground.ClientRect);
  if FDesktopDisplay then Exit;
  DrawDocker(Surface, FDocker, FMouseCoord, ClientRect, Font, SmallImages, LargeImages);
end;

procedure TForm1.ToggleButtonClick(Sender: TObject);
begin
  DesktopDisplay := not DesktopDisplay;
end;

procedure TForm1.ToggleButtonMouseEnter(Sender: TObject);
begin
  ToggleButton.Font.Color := clBlack;
end;

procedure TForm1.ToggleButtonMouseLeave(Sender: TObject);
begin
  ToggleButton.Font.Color := clWhite;
end;

procedure TForm1.ToggleButtonMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if FDesktopDisplay then Exit;
  X := X + ToggleButton.Left;
  if X <> FMouseCoord then
  begin
    FMouseCoord := X;
    Invalidate;
  end;
end;

procedure TForm1.UpdateSplash(X, Y: Integer);
const
  FadeDistance = 150;
var
  Opacity, OpacityX, OpacityY: Float;
begin
  FSplash.Bitmap.Surface.Clear(clTransparent);
  OpacityX := 1;
  if X < -FadeDistance then
    OpacityX := 0
  else if X < 0 then
    OpacityX := 1 - X / -FadeDistance
  else if X > FArea.Width + FadeDistance  then
    OpacityX := 0
  else if X > FArea.Width then
    OpacityX := 1 - (X - FArea.Width) / FadeDistance;
  OpacityY := 1;
  if Y < -FadeDistance then
    OpacityY := 0
  else if Y < 0 then
    OpacityY := 1 - Y / -FadeDistance
  else if Y > FArea.Height + FadeDistance  then
    OpacityY := 0
  else if Y > FArea.Height then
    OpacityY := 1 - (Y - FArea.Height) / FadeDistance;
  if OpacityX < OpacityY then
    Opacity := OpacityX
  else
    Opacity := OpacityY;
  if (FSplash.Opacity = 0) and (Opacity = 0) then
    Exit;
  DrawDocker(FSplash.Bitmap.Surface, FDocker, X, FArea, Font, SmallImages, LargeImages);
  FSplash.Update;
  FSplash.Opacity := Round(255 * Opacity);
end;

procedure TForm1.MouseNotify(Kind: TMouseNotifyKind; Button: TMouseButton; X,
  Y: Integer);
begin
  if Kind = nkMove then
    UpdateSplash(X - FArea.X, Y - FArea.Y);
end;

procedure TForm1.SetDesktopDisplay(Value: Boolean);
begin
  if FDesktopDisplay = Value then Exit;
  FDesktopDisplay := Value;
  if FDesktopDisplay then
  begin
    ToggleButton.ImageIndex := 1;
    ToggleButton.Caption := 'Display in window';
  end
  else
  begin
    ToggleButton.ImageIndex := 0;
    ToggleButton.Caption := 'Display on desktop';
  end;
  UpdateSplash(-500, 0);
  FSplash.Visible := FDesktopDisplay;
  if FDesktopDisplay then
    MouseMonitor.Add(MouseNotify)
  else
    MouseMonitor.Remove(MouseNotify);
  Invalidate;
end;

end.

