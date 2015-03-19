{ Cross Codeot: Torment demonstration

  This program demonstrates how to use the
  TDetailList control and mix in ISurface graphics
  in the DrawItem event handler.

  The TDetailList control is a virtual list control
  with optional column headers. It looks and functions the
  same on all platforms. }

unit Main;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, ExtCtrls,
  { Project units }
  Downloads,
  { Codebot units }
  Codebot.System,
  Codebot.Graphics,
  Codebot.Graphics.Types,
  Codebot.Controls.Scrolling;

{ TDetailsForm }

type
  TDetailsForm = class(TForm)
    DetailsList: TDetailsList;
    Images: TImageStrip;
    Timer: TTimer;
    procedure DetailsListColumnClick(Sender: TObject; Column: THeaderColumn);
    procedure DetailsListColumnSelect(Sender: TObject; Column: THeaderColumn);
    procedure DetailsListDrawItem(Sender: TObject; Surface: ISurface;
      Index: Integer; Rect: TRectI; State: TDrawState);
    procedure FormCreate(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FDownloads: TFileDownloadList;
    FGreenPen: IPen;
    FGreenBrush: IBrush;
    procedure Sort(Column: THeaderColumn);
  end;

var
  DetailsForm: TDetailsForm;

implementation

{$R *.lfm}

{ TDetailsForm }

procedure TDetailsForm.FormCreate(Sender: TObject);
var
  Names: StringArray;
  Widths: IntArray;
  Aligns: IntArray;
  Column: THeaderColumn;
  I: Integer;
begin
  { Codebot.System defines TArrayList }
  FDownloads.Length := 3000;
  { FDownloads is a TArrayList<IFileDownload> }
  for I := FDownloads.Lo to FDownloads.Hi do
    FDownloads.Items[I] := NewDownload;
  DetailsList.Columns.BeginUpdate;
  try
    { Codebot.System adds many helper methods to the string type }
    Names := FieldNames.Split(',');
    { Split string methods convert a string into a TArrayList<T> }
    Widths := FieldWidths.SplitInt(',');
    Aligns := FieldAligns.SplitInt(',');
    for I := Names.Lo to Names.Hi do
    begin
      Column := DetailsList.Columns.Add;
      Column.Caption := Names[I];
      Column.Width := Widths[I];
      Column.Alignment := TAlignment(Aligns[I]);
    end;
  finally
    DetailsList.Columns.EndUpdate;
  end;
  { Details list is virtual, just set its Count and handle DrawItem }
  DetailsList.Count := FDownloads.Length;
  { Surface object like IPen and IBrush are managed for you }
  FGreenPen := NewPen($3B8300);
  { The built in refernce counting will these object for you }
  FGreenBrush := NewBrush($3B8300);
end;

procedure TDetailsForm.TimerTimer(Sender: TObject);
var
  I: Integer;
begin
  { For this example we simulate torrent activity }
  for I := FDownloads.Lo to FDownloads.Hi do
    FDownloads.Items[I].Step;
  Sort(DetailsList.SelectedColumn);
  DetailsList.Invalidate;
end;

procedure TDetailsForm.DetailsListDrawItem(Sender: TObject; Surface: ISurface;
  Index: Integer; Rect: TRectI; State: TDrawState);

  function MimeToIndex(Mime: string): Integer;
  begin
    { Codebot.System adds many helper methods to string }
    if Mime.Contains('text') then
      Result := 0
    else if Mime.Contains('mp3') then
      Result := 1
    else if Mime.Contains('zip') then
      Result := 2
    else
      Result := 3;
  end;

  procedure DrawProgress(Surface: ISurface; Rect: TRectI; Progress: Float);
  var
    C: TColorB;
    R: TRectI;
  begin
    R := Rect;
    { TRectI has many time saving methods }
    R.Inflate(0, -3);
    { TColorB can be converted from a TColor }
    C := clSilver;
    { TColorB has many methods to adjust the color }
    C := C.Lighten(0.5);
    { The Codebot.Graphics unit has many convient drawing functions }
    FillRectColor(Surface, R, C);
    R.Width := Round(R.Width * Progress);
    if R.Width > 0 then
    begin
      C := clGreen;
      FillRectColor(Surface, R, C.Lighten(0.75));
      StrokeRectColor(Surface, R, C);
    end;
  end;

  procedure DrawHealth(Surface: ISurface; Rect: TRectI; Health: Integer);
  var
    R: TRectI;
    I: integer;
  begin
    { Save matrix, paths, clipping, and more using Surface.Save }
    Surface.Save;
    { Cllip areas by drwing paths }
    Surface.Rectangle(Rect);
    { Then calling Path.Clip }
    Surface.Path.Clip;
    { TRectI is like a super TRect }
    R := TRectI.Create(5, 18);
    R.Center(Rect.MidPoint);
    R.Offset(14, 0);
    for I := 4 downto 0 do
    begin
      if Health > I then
        Surface.FillRect(FGreenBrush, R)
      else
        Surface.StrokeRect(FGreenPen, R);
      R.Offset(-7, 0);
      R.Top := R.Top + 3;
    end;
    { Restore state using Surface.Restore }
    Surface.Restore;
  end;

  procedure DrawRating(Surface: ISurface; Rect: TRectI; Rating: Integer);
  var
    R: TRectI;
    I, J: integer;
  begin
    { Again we can save the Surface state using the Save method }
    Surface.Save;
    Surface.Rectangle(Rect);
    Surface.Path.Clip;
    { TRectI and other record types have constructors }
    R := TRectI.Create(Images.Size, Images.Size);
    { TRectI.Center centers a rect over a point }
    R.Center(Rect.MidPoint);
    R.Offset(-34, 0);
    for I := 1 to 5 do
    begin
      J := I * 2 - 1;
      if Rating < J then
        { TIImageStrip can draw a series of icons to a surface }
        Images.Draw(Surface, 4, R.X, R.Y)
      else if Rating = J then
        Images.Draw(Surface, 5, R.X, R.Y)
      else
        Images.Draw(Surface, 6, R.X, R.Y);
      R.Offset(Images.Size + 1, 0);
    end;
    Surface.Restore;
  end;

const
  Margin = -4;
var
  Column: THeaderColumn;
  Download: IFileDownload;
  R: TRectI;
  S: string;
  I: Integer;
begin
  { Most all drawing function involve a TDrawState
    See Codebot.Graphics.Types.TDrawState for details }
  if dsSelected in State then
    FillRectState(Surface, Rect, State);
  { Draw each column in this row }
  for I := 0 to DetailsList.Columns.Count - 1 do
  begin
    { Mind you, you can reorder TDetail.List.Coulmns }
    Column := DetailsList.Columns[I];
    { Skip the column if its not visible }
    if not Column.Visible then
      Continue;
    { Retreive the current IFileDownload }
    Download := FDownloads[Index];
    { Retreive rectangle for this column }
    R := DetailsList.GetColumnRect(I, Rect);
    { We want a small amrgin to the left and right when we draw text }
    R.Inflate(Margin, 0);
    R.Width := R.Width + Margin;
    S := '';
    { We draw different things based on the column tag }
    case Column.Tag of
      tagName:
      begin
        Images.Draw(Surface, MimeToIndex(Download.Kind), R.Left - 2,
          R.Top + (R.Height - Images.Size) div 2);
        R.Left := R.Left + R.Height;
        S := Download.Name;
      end;
      tagProgress:
      begin
        DrawProgress(Surface, R, Download.Progress / Download.Size);
        if Download.Size = Download.Progress then
          S := 'Completed'
        else
          S := '%.1f'.Format([Download.Progress / Download.Size * 100]) + '%';
      end;
      tagEta: S := EtaToStr(Download.Eta);
      tagSize: S := BytesToStr(Download.Size);
      tagHealth: DrawHealth(Surface, R, Download.Health);
      tagKind: S := Download.Kind;
      tagUp: S := BytesToStr(Download.UploadSpeed) + '/S';
      tagDown: S := BytesToStr(Download.DownloadSpeed) + '/S';
      tagRating: DrawRating(Surface, R, Download.Rating);
      tagSeeds: S := IntToStr(Download.Seeds);
      tagPeers: S := IntToStr(Download.Peers);
    end;
    { If the column is to small avoid trying to fit in text }
    if R.Width > 10 then
      { Since TDrawList is a themed control, Theme.Font contains a copy of its IFont }
      Surface.TextOut(Theme.Font, S, R, AlignDir[Column.Alignment]);
  end;
end;

procedure TDetailsForm.Sort(Column: THeaderColumn);
begin
  if Column <> nil then
    { TArrayList<T> can sort itself if you give it an TComparer<T> }
    FDownloads.Sort(Column.Sort, DownloadCompare(Column.Tag));
end;

procedure TDetailsForm.DetailsListColumnSelect(Sender: TObject;
  Column: THeaderColumn);
begin
  { When a column is selected default the sort order to ascending }
  Column.Sort := soAscend;
  Sort(Column);
end;

procedure TDetailsForm.DetailsListColumnClick(Sender: TObject; Column: THeaderColumn);
begin
  { When a column is clicked reverse the sort order }
  if Column.Sort = soDescend then
    Column.Sort := soAscend
  else
    Column.Sort := soDescend;
  Sort(Column);
end;

end.

