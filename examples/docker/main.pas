unit main;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  Docker.Spacing,
  Codebot.System,
  Codebot.Graphics,
  Codebot.Graphics.Types;

{ TForm1 }

type
  TForm1 = class(TForm)
    LargeImages: TImageStrip;
    SmallImages: TImageStrip;
    procedure FormCreate(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
  private
    { private declarations }
  public
    FBackground: IBitmap;
    FDocker: TDocker;
    FMouseCoord: Integer;
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
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
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
      P.Color := P.Color.Fade(0.1);
      while P.Width > 0 do
      begin
        Surface.Stroke(P, True);
        P.Width := P.Width - 2;
      end;
      { Which ends up creating a dark blurry shadow effect }
      P.Width := 1.5;
      P.Color := clBlack;
      P.Color := P.Color.Fade(0.5);
      { One last slightly darker text outline }
      Surface.Stroke(P);
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
  DrawDocker(Surface, FDocker, FMouseCoord, ClientRect, Font, SmallImages, LargeImages);
end;

end.

