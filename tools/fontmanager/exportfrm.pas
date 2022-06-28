unit ExportFrm;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ComCtrls, LCLType, ExtCtrls,
  Codebot.System, Codebot.Graphics, Codebot.Graphics.Types, Codebot.Text,
  Codebot.Text.Json, MaterialFont;

{ TExportForm }

type
  TExportForm = class(TForm)
    PreviewLabel: TLabel;
    SaveButton: TButton;
    CloseButton: TButton;
    BrowseButton: TSpeedButton;
    FolderEdit: TEdit;
    FileEdit: TEdit;
    FolderLabel: TLabel;
    FileLabel: TLabel;
    SizeEdit: TEdit;
    SizeLabel: TLabel;
    SizeSpinner: TUpDown;
    InfoLabel: TLabel;
    BrowseDialog: TSelectDirectoryDialog;
    InfoTimer: TTimer;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SizeSpinnerChanging(Sender: TObject; var AllowChange: Boolean);
    procedure SizeEditExit(Sender: TObject);
    procedure SizeEditKeyPress(Sender: TObject; var Key: char);
    procedure BrowseButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure BrowseButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure InfoTimerTimer(Sender: TObject);
  private
    FCaption: string;
    FGlyphIndex: Integer;
    FGlyphName: string;
    FGlyphCodePoint: string;
    FGlyphSize: Integer;
    procedure LoadState;
    procedure SaveState;
    procedure SetGlyphIndex(Value: Integer);
    procedure SetInfo(const Info: string);
    procedure UpdateGlyph;
  public

  end;

procedure ShowExport(GlyphIndex: Integer);

implementation

{$R *.lfm}

procedure ShowExport(GlyphIndex: Integer);
var
  F: TExportForm;
begin
  F := TExportForm.Create(nil);
  try
    F.SetGlyphIndex(GlyphIndex);
    F.ShowModal;
  finally
    F.Free;
  end;
end;

{ TExportForm }

procedure TExportForm.FormCreate(Sender: TObject);
begin
  FCaption := Caption;
  ClientWidth := CloseButton.Left + CloseButton.Width + 8;
  ClientHeight := CloseButton.Top + CloseButton.Height + 8;
  LoadState;
end;

procedure TExportForm.LoadState;
var
  N: TJsonNode;
  S: string;
begin
  S := ConfigAppFile(False);
  if FileExists(S) then
  begin
    N := TJsonNode.Create;
    try
      if N.TryParse(FileReadStr(S)) then
      begin
        N.AsObject;
        FolderEdit.Text := Trim(N.Force('folder').AsString);
        if not DirExists(FolderEdit.Text) then
          FolderEdit.Text := GetUserDir;
        FileEdit.Text := Trim(N.Force('filename').AsString);
        SizeSpinner.Position := Round(N.Force('size').AsNumber);
        FGlyphSize := SizeSpinner.Position;
        Exit;
      end;
    finally
      N.Free;
    end;
  end;
  FolderEdit.Text := GetUserDir;
  FileEdit.Text := 'exported-icon';
  FGlyphSize := 64;
  SizeSpinner.Position := FGlyphSize;
end;

procedure TExportForm.SaveState;
var
  N: TJsonNode;
begin
  N := TJsonNode.Create;
  try
    N.AsObject;
    N.Add('folder', Trim(FolderEdit.Text));
    N.Add('filename', Trim(FileEdit.Text));
    N.Add('size', SizeSpinner.Position);
    N.SaveToFile(ConfigAppFile(False, True));
  finally
    N.Free;
  end;
end;

procedure TExportForm.UpdateGlyph;
const
  MinCenter = 100;
  Margin = 8;
var
  M: Integer;
  S: string;
begin
  M := (FGlyphSize + Margin * 2 + 2) div 2;
  if M < MinCenter then
    M := MinCenter;
  S := FGlyphName + #10 + IntToStr(FGlyphSize) + 'px';
  InfoLabel.Caption := S;
  PreviewLabel.Left := Margin;
  PreviewLabel.Width := FGlyphSize + 2;
  PreviewLabel.Height := FGlyphSize + 2;
  PreviewLabel.Top := (M - PreviewLabel.Height) div 2;
  if PreviewLabel.Top < Margin then
  begin
    PreviewLabel.Top := Margin;
    M := PreviewLabel.Height + Margin * 2;
  end;
  PreviewLabel.Font.Size :=  PreviewLabel.Scale96ToForm(FGlyphSize - Round((FGlyphSize) / 96 * 34));
  InfoLabel.Left := PreviewLabel.Left + PreviewLabel.Width + Margin;
  InfoLabel.Top := (M - InfoLabel.Height) div 2;
  InfoLabel.Width := ClientWidth - InfoLabel.Left - Margin;
  FolderLabel.Top := M;
  FolderEdit.Top := FolderLabel.Top + FolderLabel.Height + Margin;
  BrowseButton.Top := FolderEdit.Top + (FolderEdit.Height - BrowseButton.Height) div 2;
  FileLabel.Top := FolderEdit.Top + FolderEdit.Height + Margin * 2;
  FileEdit.Top := FileLabel.Top + FileLabel.Height + Margin;
  SizeLabel.Top := FileLabel.Top;
  SizeEdit.Top := FileEdit.Top;
  SaveButton.Top := SizeEdit.Top + SizeEdit.Height + Margin * 2;
  CloseButton.Top := SaveButton.Top;
  ClientHeight := CloseButton.Top + CloseButton.Height + Margin;
  Invalidate;
end;

procedure TExportForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TExportForm.FormPaint(Sender: TObject);
var
  S: ISurface;
  P: IPen;
  F: IFont;
  R: TRectF;
begin
  S := NewSurface(Canvas);
  R := PreviewLabel.BoundsRect;
  R.Offset(0.5, 0.5);
  S.Rectangle(R);
  P := NewPen(clWindowText);
  P.LinePattern := pnDash;
  S.Stroke(P);
  R.Inflate(500, 500);
  F := NewFont(PreviewLabel.Font);
  F.Color := clWindowText;
  S.TextOut(F, PreviewLabel.Caption, R, drCenter);
end;

procedure TExportForm.FormShow(Sender: TObject);
begin
  Mouse.Capture := Self.Handle;
  UpdateGlyph;
  FileEdit.SetFocus;
end;

procedure TExportForm.SizeSpinnerChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  FGlyphSize := SizeSpinner.Position;
  UpdateGlyph;
end;

procedure TExportForm.SizeEditExit(Sender: TObject);
begin
  SizeSpinner.Position := StrToIntDef(Trim(SizeEdit.Text), SizeSpinner.Position);
  FGlyphSize := SizeSpinner.Position;
  UpdateGlyph;
end;

procedure TExportForm.SizeEditKeyPress(Sender: TObject; var Key: char);
begin
  if Key = ^M then
  begin
    SizeSpinner.Position := StrToIntDef(Trim(SizeEdit.Text), SizeSpinner.Position);
    FGlyphSize := SizeSpinner.Position;
    UpdateGlyph;
  end;
end;

procedure TExportForm.BrowseButtonClick(Sender: TObject);
begin
  BrowseDialog.FileName := FolderEdit.Text;
  if BrowseDialog.Execute then
    FolderEdit.Text := BrowseDialog.FileName;
end;

procedure TExportForm.SaveButtonClick(Sender: TObject);
var
  B: IBitmap;
  R: TRectI;
  D, F: string;
begin
  D := Trim(FolderEdit.Text);
  F := Trim(FileEdit.Text);
  if (D = '') or (not DirExists(D)) then
  begin
    FolderEdit.SetFocus;
    SetInfo('Invalid Folder');
    Exit;
  end;
  if F = '' then
  begin
    FileEdit.SetFocus;
    SetInfo('Invalid Filename');
    Exit;
  end;
  F := FileChangeExt(F, '.png');
  B := NewBitmap(FGlyphSize, FGlyphSize);
  R := B.ClientRect;
  R.Inflate(500, 500);
  B.Surface.TextOut(NewFont(PreviewLabel.Font), PreviewLabel.Caption, B.ClientRect, drCenter);
  try
    B.SaveToFile(PathCombine(D, F));
    SaveState;
    SetInfo('Saved');
  except
    FileEdit.SetFocus;
    SetInfo('Unable To Save');
  end;
end;

procedure TExportForm.BrowseButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
end;

procedure TExportForm.InfoTimerTimer(Sender: TObject);
begin
  InfoTimer.Enabled := False;
  Caption := FCaption;
end;

procedure TExportForm.SetGlyphIndex(Value: Integer);
var
  NeedsUp: Boolean;
  S, T: string;
  I: Integer;
begin
  FGlyphIndex := Value;
  S := MaterialGlyphs[FGlyphIndex];
  S :=  S.Split(' ')[1];
  PreviewLabel.Caption := UnicodeToStr(StrToInt(S));
  S := MaterialGlyphs[FGlyphIndex];
  S := S.Split(' ')[0];
  T := UpCase(S[1]);
  NeedsUp := False;
  for I := 2 to Length(S) do
  begin
    if S[I] = '-' then
    begin
      NeedsUp := True;
      Continue;
    end;
    if NeedsUp then
      T := T + ' ' + UpCase(S[I])
    else
      T := T + S[I];
    NeedsUp := False;
  end;
  S := MaterialGlyphs[FGlyphIndex];
  S := S.Split(' ')[1];
  S[1] := ' ';
  FGlyphName := T;
  FGlyphCodePoint := '\u' + S.Trim;
  UpdateGlyph;
end;

procedure TExportForm.SetInfo(const Info: string);
begin
  Caption := Info;
  InfoTimer.Enabled := False;
  InfoTimer.Enabled := True;
end;

procedure TExportForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
end;

end.

