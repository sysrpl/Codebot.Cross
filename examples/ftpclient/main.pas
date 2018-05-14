unit Main;

{$mode delphi}
{$WARN 5024 off : Parameter "$1" not used}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  LCLType,
  Codebot.System,
  Codebot.Graphics,
  Codebot.Graphics.Types,
  Codebot.Networking.Ftp,
  Codebot.Controls.Scrolling;

{ TClientForm }

type
  TFileList = TArrayList<TRemoteFindData>;

  TClientForm = class(TForm)
    ConnectButton: TButton;
    HostEdit: TEdit;
    FileList: TDrawList;
    CommandHistory: TMemo;
    FileImages: TImageStrip;
    PreviewMemo: TMemo;
    PreviewImage: TImage;
    PreviewPanel: TPanel;
    UserEdit: TEdit;
    HostLabel: TLabel;
    PasswordEdit: TEdit;
    UserLabel: TLabel;
    PathLabel: TLabel;
    Toolbar: TPanel;
    PasswordLabel: TLabel;
    procedure ConnectButtonClick(Sender: TObject);
    procedure FileListDblClick(Sender: TObject);
    procedure FileListDrawItem(Sender: TObject; Surface: ISurface;
      Index: Integer; Rect: TRectI; State: TDrawState);
    procedure FileListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FileListKeyPress(Sender: TObject; var Key: char);
    procedure FileListSelectItem(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FClient: TFtpClient;
    FFiles: TFileList;
    FPath: string;
    FDownloadFile: string;
    procedure ClientCommand(Sender: TObject; const Text: string);
    procedure ClientResponse(Sender: TObject; const Text: string);
    procedure SimpleDownload(Thread: TSimpleThread);
  end;

var
  ClientForm: TClientForm;

implementation

{$R *.lfm}

{ TClientForm }

procedure TClientForm.FormCreate(Sender: TObject);
begin
  Color := clWindow;
  Font.Color := clWindowText;
  FClient := TFtpClient.Create;
  FClient.OnCommand := ClientCommand;
  FClient.OnResponse := ClientResponse;
end;

procedure TClientForm.FormShow(Sender: TObject);
begin
  HostEdit.SetFocus;
  OnShow := nil;
end;

procedure TClientForm.SimpleDownload(Thread: TSimpleThread);
var
  Client: TFtpClient;
  S: string;
begin
  S := FDownloadFile;
  Client := TFtpClient.Create;
  try
    Client.Host := FClient.Host;
    Client.UserName := FClient.UserName;
    Client.Password := FClient.Password;
    if Client.Connect then
      Client.FileGet(S, FileExtractName(S));
  finally
    Client.Free;
  end;
end;

function CompareData(constref A, B: TRemoteFindData): Integer;
begin
  if (fsaDirectory in A.Attributes) and (fsaDirectory in B.Attributes) then
  begin
    if A.Name = '..' then
      Exit(-1);
    if B.Name = '..' then
      Exit(1);
    Result := StrCompare(A.Name, B.Name, True);
  end
  else if fsaDirectory in A.Attributes then
    Exit(-1)
  else if fsaDirectory in B.Attributes then
    Exit(1)
  else
    Result := StrCompare(A.Name, B.Name, True);
end;

procedure TClientForm.ConnectButtonClick(Sender: TObject);
var
  S: string;

  procedure ConnectionChange;
  var
    B: Boolean;
  begin
    B := FClient.Connected;
    Caption := S;
    if B then
    begin
      FileList.Count := FFiles.Length;
      ConnectButton.Caption := 'Disconnect';
    end
    else
      FileList.Count := 0;
    B := not B;
    HostLabel.Visible := B;
    HostEdit.Visible := B;
    UserLabel.Visible := B;
    UserEdit.Visible := B;
    PasswordLabel.Visible := B;
    PasswordEdit.Visible := B;
    PathLabel.Visible := not B;
  end;

var
  H, U, P: string;
  D: TRemoteFindData;
begin
  if ConnectButton.Caption = 'Connect' then
  begin
    CommandHistory.Clear;
    FClient.Disconnect;
    try
      H := Trim(HostEdit.Text);
      U := Trim(UserEdit.Text);
      P := Trim(PasswordEdit.Text);
      FClient.Host := H;
      if (U = '') or (U.ToLower = 'anonymous') then
      begin
        U := 'anonymous';
        P := 'user@email.com';
      end;
      FClient.UserName := U;
      FClient.Password := P;
      if FClient.Connect then
      begin
        FFiles.Clear;
        FPath := '/';
        PathLabel.Caption := FClient.Host + FPath;
        if FClient.FindFirst(FPath, D) then
        repeat
          if D.Name = '.' then
            Continue;
          if D.Name = '..' then
            Continue;
          FFiles.Push(D);
        until not FClient.FindNext(D);
        FFiles.Sort(soAscend, CompareData);
        S := 'Ftp Client';
      end
      else
        S := 'Ftp Client: Could not connect';
    except
      on E: Exception do
        S := 'Ftp Client: Error ' + E.Message;
    end;
    ConnectionChange;
  end
  else if ConnectButton.Caption = 'Disconnect' then
  begin
    FClient.Disconnect;
    FileList.Count := 0;
    FFiles.Clear;
    HostLabel.Visible := True;
    HostEdit.Visible := True;
    UserLabel.Visible := True;
    UserEdit.Visible := True;
    PasswordLabel.Visible := True;
    PasswordEdit.Visible := True;
    HostLabel.Left := 10;
    HostEdit.Left := HostLabel.Left + HostLabel.Width + 10;
    UserLabel.Left := HostEdit.Left + HostEdit.Width + 10;
    UserEdit.Left := UserLabel.Left + UserLabel.Width + 10;
    PasswordLabel.Left := UserEdit.Left + UserEdit.Width + 10;
    PasswordEdit.Left := PasswordLabel.Left + PasswordLabel.Width + 10;
    PathLabel.Visible := False;
    PreviewPanel.Visible := False;
    ConnectButton.Caption := 'Connect';
  end
  else if ConnectButton.Caption = 'Download' then
  begin
    FDownloadFile := FPath + FFiles[FileList.ItemIndex].Name;
    TSimpleThread.Create(SimpleDownload);
    CommandHistory.Lines.Insert(0, '> GET ' + FDownloadFile.Quote);
  end;
end;

procedure TClientForm.FileListDblClick(Sender: TObject);
var
  Stream: TStream;
  D: TRemoteFindData;
  S: string;
  I: Integer;
begin
  PreviewPanel.Visible := False;
  I := FileList.ItemIndex;
  if I < 0 then
    Exit;
  S := FFiles[I].Name;
  if fsaDirectory in FFiles[I].Attributes then
  begin
    if S = '.'  then
      Exit;
    if S = '..'  then
    begin
      FPath := UriExcludeDelimiter(FPath);
      if FPath.MatchCount('/') < 2 then
        FPath := '/'
      else
      begin
        FPath := UriExtractPath(FPath);
        FPath := UriIncludeDelimiter(FPath);
      end;
    end
    else
      FPath := UriCombine(FPath, S, True);
    FFiles.Clear;
    if FClient.FindFirst(FPath, D) then
    repeat
      if D.Name = '.' then
        Continue;
      if D.Name = '..' then
        Continue;
      FFiles.Push(D);
    until not FClient.FindNext(D);
    if FPath <> '/' then
    begin
      D.Attributes := [fsaDirectory];
      D.Name := '..';
      D.Path := FPath;
      FFiles.Push(D);
    end;
    FFiles.Sort(soAscend, CompareData);
    FileList.Count := FFiles.Length;
    FileList.ItemIndex := 0;
    FileList.ScrollToSelection;
    FileList.Invalidate;
    PathLabel.Caption := UriCombine(FClient.Host, FPath);
  end
  else if FFiles[I].Size < 1024 * 1024 then
      if '.cva.xml.asc.txt.htm.html'.IndexOf(FileExtractExt(S)) > 0 then
      begin
        Stream := TStringStream.Create;
        try
          FClient.StreamGet(FPath + S, Stream);
          PreviewMemo.Text := TStringStream(Stream).DataString;
          PreviewMemo.SelStart := 0;
          PreviewMemo.Visible := True;
          PreviewPanel.Visible := True;
          PreviewImage.Visible := False;
        finally
          Stream.Free;
        end;
      end
      else if '.png.bmp.jpg.jpeg.gif'.IndexOf(FileExtractExt(S)) > 0 then
      begin
        Stream := TMemoryStream.Create;
        try
          FClient.StreamGet(FPath + S, Stream);
          Stream.Seek(0, 0);
          PreviewImage.Visible := True;
          PreviewImage.Picture.LoadFromStream(Stream);
          PreviewPanel.Visible := True;
          PreviewMemo.Visible := False;
        finally
          Stream.Free;
        end;
      end;
end;

procedure TClientForm.FileListDrawItem(Sender: TObject; Surface: ISurface;
  Index: Integer; Rect: TRectI; State: TDrawState);

  function FileNameToImage(const FileName: string): Integer;
  const
    KnownExts = '.asc.txt.cva;.pdf;.png.jpg.jpeg.bmp.ico.gif.tif;' +
      '.zip.7z.rar.tar.gz;.html.htm.xml;.js.json;.h.c;.exe;.wav.mp3.ogg;' +
      '.psd;.avi.mp4.mov.wmv;.iso.bin;.rtf;.doc.docx;.css;.py;.torrent;.ppt;.xls';
    KnownIndex: array[0..18] of Integer = (7, 32, 8, 6, 26, 25, 24, 10, 11, 19, 12, 18,
      22, 15, 29, 27, 17, 30, 31);
  var
    Exts: StringArray;
    S: string;
    I: Integer;
  begin
    Exts := KnownExts.Split(';');
    S := FileExtractExt(FileName);
    for I := Exts.Lo to Exts.Hi do
      if Exts.Item[I].IndexOf(S) > 0 then
        Exit(KnownIndex[I]);
    Result := 5;
  end;

var
  R: TRectI;
  S: string;
  I: Integer;
begin
  if fsaDirectory in FFiles[Index].Attributes then
  begin
    R := Rect;
    if FFiles[Index].Name = '..' then
      I := 0
    else if dsSelected in State then
      I := 4
    else
      I := 3;
    R.Left := R.Height;
    if FFiles[Index].Name = '..' then
      S := '.. [up one level]'
    else
      S := FFiles[Index].Name;
    DrawTextState(Surface, S, R, State);
    FileImages.Draw(Surface, I, 4, R.Top + (R.Height - FileImages.Size) div 2);
  end
  else
  begin
    S := FFiles[Index].Name;
    R := Rect;
    R.Left := R.Height;
    DrawTextState(Surface, '', R, State);
    R.Left := R.Left + 4;
    R.Right := R.Right - 250;
    DrawText(Surface, S, R, drLeft);
    R := Rect;
    R.Right := R.Right - 150;
    DrawText(Surface, IntToStorage(FFiles[Index].Size), R, drRight);
    R := Rect;
    R.Right := R.Right - 10;
    DrawText(Surface, FormatDateTime('yyyy/mm/dd', FFiles[Index].Date), R, drRight);
    R := Rect;
    FileImages.Draw(Surface, FileNameToImage(S), 4, R.Top + (R.Height - FileImages.Size) div 2);
  end;
end;

procedure TClientForm.FileListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if FileList.Count = 0 then
    Exit;
  case Key of
    VK_BACK:
      if FFiles[0].Name = '..' then
      begin
        FileList.ItemIndex := 0;
        FileList.OnDblClick(FileList);
      end;
    VK_RETURN:
      FileList.OnDblClick(FileList);
  end;
end;

procedure TClientForm.FileListKeyPress(Sender: TObject; var Key: Char);
var
  A: Char;
  I: Integer;
begin
  if FileList.Count = 0 then
    Exit;
  A := UpCase(Key);
  for I := FileList.ItemIndex + 1 to FileList.Count - 1 do
    if UpCase(FFiles[I].Name[1]) = A then
    begin
      FileList.ItemIndex := I;
      FileList.ScrollToSelection;
      Exit;
    end;
  for I := 0 to FileList.Count - 1 do
    if UpCase(FFiles[I].Name[1]) = A then
    begin
      FileList.ItemIndex := I;
      FileList.ScrollToSelection;
      Exit;
    end;
end;

procedure TClientForm.FileListSelectItem(Sender: TObject);
var
  I: Integer;
begin
  I := FileList.ItemIndex;
  if I < 0 then
    if FClient.Connected then
      ConnectButton.Caption := 'Disconnect'
    else
      ConnectButton.Caption := 'Connect'
  else if fsaDirectory in FFiles[I].Attributes then
    ConnectButton.Caption := 'Disconnect'
  else
    ConnectButton.Caption := 'Download';
end;

procedure TClientForm.ClientCommand(Sender: TObject; const Text: string);
begin
  CommandHistory.Lines.Insert(0, '> ' + Trim(Text));
  CommandHistory.SelStart := 0;
end;

procedure TClientForm.ClientResponse(Sender: TObject; const Text: string);
begin
  CommandHistory.Lines.Insert(0, Trim(Text));
  CommandHistory.SelStart := 0;
end;

end.

