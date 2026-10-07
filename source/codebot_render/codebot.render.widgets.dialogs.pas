unit Codebot.Render.Widgets.Dialogs;

{$i render.inc}

interface

uses
  SysUtils,
  Classes,
  Codebot.System,
  Codebot.Platform,
  Codebot.Graphics.Types,
  Codebot.Render.Graphics,
  Codebot.Hardware,
  Codebot.Render.Scenes,
  Codebot.Render.Widgets,
  Codebot.Render.Widgets.Themes;

{ This unit holds file and picture dialogs made from widgets. They implement
  IFileDialog and IPictureDialog from Codebot.Platform without using the LCL,
  SDL, or any other backend, so they work wherever a widget scene does. The
  SDL backend uses them, since SDL has no file dialogs of its own.

  A dialog is a modal window shown in WidgetDialogHost. It has an edit with
  the folder being shown beside buttons for the folder above it and the home
  folder, a list of the folders and files, an edit for the file name, a drop
  down list of the filters, and buttons to accept or cancel. A picture dialog
  shows the selected image beside the list.

  The list has columns for the name, size, type, and date modified, under a
  header. Clicking a header cell sorts the list by that column, and clicking
  it again reverses the order. Folders stay above files either way. Dragging
  the right edge of a header cell resizes its column. The name column takes
  the room the others leave until a column has been resized.

  The dialog opens showing InitialDir. If that is empty it shows the folder
  of FileName, and if that has no folder, the current folder.

  Double click a folder or press Enter to open it, and Backspace to go up.
  Typing a folder or a mask such as *.txt into the name and pressing Enter
  shows that folder or only those files. When MultiSelect is true Control
  and Shift select more than one file. A save dialog asks before a file is
  replaced. The window is resized by the grip in its corner.

  The scene keeps running while a dialog is open, as IDialog describes. If
  there is no host when Execute is called the dialog closes without being
  accepted. }

{ TWidgetFileDialog is a file dialog made from widgets }

type
  TWidgetFileDialog = class(TCustomFileDialog)
  private
    procedure Done(const Files: StringArray);
  protected
    procedure Show; override;
  end;

{ TWidgetPictureDialog is a picture dialog made from widgets, which shows the
  selected image beside the list }

  TWidgetPictureDialog = class(TCustomPictureDialog)
  private
    procedure Done(const Files: StringArray);
  protected
    procedure Show; override;
  end;

{ WidgetDialogHost is the main widget the dialogs are shown in. TWidgetScene
  sets it to its main widget when that is created, and clears it when the
  scene is finalized. }

var
  WidgetDialogHost: TMainWidget;

{ Create a file dialog made from widgets }
function NewWidgetFileDialog(Kind: TFileDialogKind): IFileDialog;
{ Create a picture dialog made from widgets }
function NewWidgetPictureDialog(Kind: TFileDialogKind): IPictureDialog;

implementation

const
  { Material design icons for the buttons and the list }
  GlyphUp = #$F3#$B0#$81#$9D;
  GlyphHome = #$F3#$B0#$8B#$9C;
  GlyphFolder = #$F3#$B0#$89#$8B;
  GlyphFile = #$F3#$B0#$88#$A4;
  GlyphImage = #$F3#$B0#$88#$9F;
  { A second press on the same row within this time opens it }
  DoubleClickTime = 0.4;
  { The size of the list when the dialog opens }
  ListWidth = 560;
  ListHeight = 300;
  PreviewWidth = 220;
  { The columns of the list }
  ColumnName = 0;
  ColumnSize = 1;
  ColumnType = 2;
  ColumnDate = 3;
  { The widths of the columns when the dialog opens. The name column takes
    the room which is left, but is never narrower than NameWidth. }
  NameWidth = 160;
  SizeWidth = 80;
  TypeWidth = 90;
  DateWidth = 130;
  IconWidth = 22;
  LabelWidth = 50;
  FilterWidth = 280;
  ImageExts = '.png.jpg.jpeg.bmp.gif.tga.tif.tiff.webp.';
  { The extensions of the other types named in the type column }
  VideoExts = '.mp4.mkv.avi.mov.webm.wmv.flv.m4v.mpg.mpeg.';
  AudioExts = '.mp3.ogg.wav.flac.m4a.aac.opus.wma.mod.xm.s3m.it.';
  TextExts = '.txt.md.log.ini.cfg.conf.json.xml.yaml.yml.csv.';
  DocumentExts = '.pdf.doc.docx.odt.rtf.xls.xlsx.ods.ppt.pptx.odp.';
  ArchiveExts = '.zip.tar.gz.bz2.xz.7z.rar.tgz.';
  SourceExts = '.pas.pp.inc.lpr.dpr.c.h.cpp.hpp.js.ts.py.sh.html.css.glsl.';
  FontExts = '.ttf.otf.woff.woff2.';
  {$ifdef windows}
  RootLength = 3;
  {$else}
  RootLength = 1;
  {$endif}

{ TWidgetAccess reaches the key events every widget has but not all publish }

type
  TWidgetAccess = class(TWidget);

{ TFilePreview draws the image of a file scaled to fit inside of it. The
  image is loaded the next time the preview is painted. }

  TFilePreview = class(TCustomWidget)
  private
    FFileName: string;
    FBitmap: IBitmap;
    FChanged: Boolean;
    procedure Release(Canvas: ICanvas);
  protected
    procedure Paint(Stage: TPaintStage); override;
  public
    destructor Destroy; override;
    procedure SetFile(const FileName: string);
  end;

{ TFileItem is a folder or file in the list }

  TFileItem = record
    Name: string;
    Folder: Boolean;
    Size: Int64;
    Modified: TDateTime;
    { What the type column shows }
    Kind: string;
    Selected: Boolean;
  end;

  { The files to accept come from the list or from the name which was typed,
    whichever was used last }
  TFilePick = (pickEdit, pickGrid);

  TFileDialogDone = procedure(const Files: StringArray) of object;

{ TFileDialogWindow builds the window of a dialog and runs it. It frees
  itself and the window when the window closes, after which it calls OnDone
  with the files chosen, or with no files if the dialog was cancelled. }

  TFileDialogWindow = class
  private
    FMain: TMainWidget;
    FDialog: TCustomFileDialog;
    FOnDone: TFileDialogDone;
    FKind: TFileDialogKind;
    FMulti: Boolean;
    FWindow: TWindow;
    FPathEdit: TEdit;
    FGrid: TScrollGrid;
    FPreview: TFilePreview;
    FNameEdit: TEdit;
    FFilterBox: TSpinBox;
    FDir: string;
    FItems: array of TFileItem;
    FFilterNames: StringArray;
    FFilterMasks: StringArray;
    FMasks: StringArray;
    FHidden: Boolean;
    FLoading: Boolean;
    FPick: TFilePick;
    FAnchor: Integer;
    FClickRow: Integer;
    FClickTime: Double;
    FOpenRow: Integer;
    FMouseDown: Boolean;
    FMouseShift: TShiftKeys;
    { The column the list is sorted by, and whether the order is reversed }
    FSortColumn: Integer;
    FSortDescending: Boolean;
    { True once a column has been resized by dragging its header cell }
    FColumnsSized: Boolean;
    FFiles: StringArray;
    FPending: StringArray;
    procedure Hook(Widget: TWidget);
    procedure ParseFilter;
    procedure SetFilter(Index: Integer);
    function MatchMasks(const Name: string): Boolean;
    function FullName(const Name: string): string;
    function ExpandName(const Name: string): string;
    function CompareItems(const A, B: TFileItem): Integer;
    procedure SortItems(L, R: Integer);
    procedure Resort;
    procedure Load;
    procedure Navigate(const Dir: string);
    procedure Up;
    procedure Layout;
    procedure UpdateColumns;
    procedure SelectOnly(Row: Integer);
    procedure SelectRange(A, B: Integer);
    procedure SelectionChanged;
    procedure OpenRow(Row: Integer);
    procedure Accept;
    procedure Finish(const Files: StringArray);
    procedure ModalDone(Sender: TObject; ModalResult: TModalResult);
    procedure ConfirmResult(Sender: TObject; ModalResult: TModalResult);
    procedure WidgetKeyUp(Sender: TObject; var Args: TSceneKeyArgs);
    procedure WindowResize(Sender: TObject);
    procedure UpClick(Sender: TObject);
    procedure HomeClick(Sender: TObject);
    procedure AcceptClick(Sender: TObject);
    procedure NameChange(Sender: TObject);
    procedure FilterChange(Sender: TObject);
    procedure HiddenChange(Sender: TObject);
    procedure GridChange(Sender: TObject);
    procedure GridClick(Sender: TObject);
    procedure GridMouseDown(Sender: TObject; var Args: TSceneMouseArgs);
    procedure GridMouseUp(Sender: TObject; var Args: TSceneMouseArgs);
    procedure GridHeaderClick(Sender: TObject; Col: Integer);
    procedure GridColResize(Sender: TObject; Col: Integer);
    procedure GridDrawCell(Sender: TObject; Surface: ICanvas; Row, Col: Integer;
      const Rect: TRectF);
  public
    constructor Create(Main: TMainWidget; Dialog: TCustomFileDialog;
      Preview: Boolean; OnDone: TFileDialogDone);
  end;

{ Returns true if a name matches a mask of characters, ? for any one
  character, and * for any number of characters. Both are lower case. }

function MatchMask(const S, M: string): Boolean;
var
  I, J, StarI, StarJ: Integer;
begin
  if (M = '') or (M = '*') or (M = '*.*') then
    Exit(True);
  I := 1;
  J := 1;
  StarI := 0;
  StarJ := 0;
  while I <= Length(S) do
    if (J <= Length(M)) and ((M[J] = '?') or (M[J] = S[I])) then
    begin
      Inc(I);
      Inc(J);
    end
    else if (J <= Length(M)) and (M[J] = '*') then
    begin
      StarJ := J;
      StarI := I;
      Inc(J);
    end
    else if StarJ > 0 then
    begin
      { Let the last star take one more character and try again }
      J := StarJ + 1;
      Inc(StarI);
      I := StarI;
    end
    else
      Exit(False);
  while (J <= Length(M)) and (M[J] = '*') do
    Inc(J);
  Result := J > Length(M);
end;

{ Replace a leading ~ with the home folder of the user }

function ExpandHome(const Path: string): string;
begin
  Result := Path;
  if (Result <> '') and (Result[1] = '~') then
    Result := ExcludeTrailingPathDelimiter(GetUserDir) + Copy(Result, 2, Length(Result));
end;

{ The full path of a folder without a delimiter at its end, unless it is the
  root. An empty folder is the current folder. }

function ExpandDir(const Dir: string): string;
begin
  Result := Trim(Dir);
  if Result = '' then
    Result := GetCurrentDir;
  Result := ExpandFileName(ExpandHome(Result));
  while (Length(Result) > RootLength) and (Result[Length(Result)] in AllowDirectorySeparators) do
    SetLength(Result, Length(Result) - 1);
end;

function IsImage(const Name: string): Boolean;
var
  S: string;
begin
  S := LowerCase(ExtractFileExt(Name));
  Result := (S <> '') and (Pos(S + '.', ImageExts) > 0);
end;

{ The type of a file named by its extension, for the type column }

function KindText(const Name: string): string;
var
  S: string;

  function Among(const Exts: string): Boolean;
  begin
    Result := Pos(S + '.', Exts) > 0;
  end;

begin
  S := LowerCase(ExtractFileExt(Name));
  if (S = '') or (S = '.') then
    Result := 'File'
  else if Among(ImageExts) then
    Result := 'Image'
  else if Among(VideoExts) then
    Result := 'Video'
  else if Among(AudioExts) then
    Result := 'Audio'
  else if Among(TextExts) then
    Result := 'Text'
  else if Among(DocumentExts) then
    Result := 'Document'
  else if Among(ArchiveExts) then
    Result := 'Archive'
  else if Among(SourceExts) then
    Result := 'Source'
  else if Among(FontExts) then
    Result := 'Font'
  else
    Result := UpperCase(Copy(S, 2, Length(S))) + ' file';
end;

function SizeText(Size: Int64): string;
begin
  if Size < 1024 then
    Result := IntToStr(Size) + ' B'
  else if Size < 1024 * 1024 then
    Result := Format('%.1f KB', [Size / 1024])
  else if Size < 1024 * 1024 * 1024 then
    Result := Format('%.1f MB', [Size / (1024 * 1024)])
  else
    Result := Format('%.1f GB', [Size / (1024 * 1024 * 1024)]);
end;

{ TFilePreview }

destructor TFilePreview.Destroy;
begin
  try
    if (FBitmap <> nil) and (Computed.Theme is TCanvasTheme) then
      Release(TCanvasTheme(Computed.Theme).Canvas);
  except
    { The canvas may already be gone when the scene is closing }
  end;
  FBitmap := nil;
  inherited Destroy;
end;

procedure TFilePreview.Release(Canvas: ICanvas);
begin
  if (FBitmap <> nil) and (Canvas <> nil) then
    Canvas.DisposeBitmap(FBitmap);
  FBitmap := nil;
end;

procedure TFilePreview.SetFile(const FileName: string);
begin
  if FileName = FFileName then
    Exit;
  FFileName := FileName;
  FChanged := True;
end;

var
  PreviewCount: Integer;

procedure TFilePreview.Paint(Stage: TPaintStage);
var
  Canvas: ICanvas;
  R, D: TRectF;
  C: TColorF;
  S: Float;
begin
  if Stage <> prePaint then
    Exit;
  if not (Computed.Theme is TCanvasTheme) then
    Exit;
  Canvas := TCanvasTheme(Computed.Theme).Canvas;
  if Canvas = nil then
    Exit;
  if FChanged then
  begin
    FChanged := False;
    Release(Canvas);
    if FFileName <> '' then
    try
      { Bitmaps are kept by name, so each image is given a name of its own }
      Inc(PreviewCount);
      FBitmap := Canvas.LoadBitmap('filepreview' + IntToStr(PreviewCount), FFileName);
    except
      FBitmap := nil;
    end;
  end;
  R := Computed.Bounds.Round;
  C := Color(colorBorder);
  C.Alpha := C.Alpha * 0.6;
  Canvas.Rect(R);
  Canvas.Stroke(C);
  if (FBitmap = nil) or (FBitmap.Width < 1) or (FBitmap.Height < 1) then
    Exit;
  R.Inflate(-4, -4);
  if (R.Width < 1) or (R.Height < 1) then
    Exit;
  { Scale the image down to fit, but do not make a small image larger }
  S := R.Width / FBitmap.Width;
  if R.Height / FBitmap.Height < S then
    S := R.Height / FBitmap.Height;
  if S > 1 then
    S := 1;
  D := NewRectF(0, 0, FBitmap.Width * S, FBitmap.Height * S);
  D.X := R.X + (R.Width - D.Width) / 2;
  D.Y := R.Y + (R.Height - D.Height) / 2;
  Canvas.DrawImage(FBitmap, FBitmap.ClientRect, D);
end;

{ TFileDialogWindow }

constructor TFileDialogWindow.Create(Main: TMainWidget; Dialog: TCustomFileDialog;
  Preview: Boolean; OnDone: TFileDialogDone);
var
  AcceptButton, CancelButton: TPushButton;
  UpButton, HomeButton: TGlyphButton;
  HiddenBox: TCheckBox;
  S: string;
begin
  inherited Create;
  FMain := Main;
  FDialog := Dialog;
  FOnDone := OnDone;
  FKind := Dialog.GetKind;
  FMulti := Dialog.GetMultiSelect and (FKind = fdOpen);
  FAnchor := -1;
  FClickRow := -1;
  FOpenRow := -1;
  FLoading := True;
  ParseFilter;
  with FMain.Add<TWindow>(FWindow) do
  begin
    S := Dialog.GetTitle;
    if S = '' then
      if FKind = fdSave then
        S := 'Save File'
      else
        S := 'Open File';
    Text := S;
    OnResize := WindowResize;
    { The folder being shown }
    with This.Add<THBox> do
    begin
      Margin := 0;
      with This.Add<TGlyphButton>(UpButton) do
      begin
        Align := alignCenter;
        Text := GlyphUp;
        Hint := 'Go to the folder above this one';
        OnClick := UpClick;
      end;
      with This.Add<TGlyphButton>(HomeButton) do
      begin
        Align := alignCenter;
        Text := GlyphHome;
        Hint := 'Go to your home folder';
        OnClick := HomeClick;
      end;
      with This.Add<TEdit>(FPathEdit) do
      begin
        Align := alignCenter;
        Width := ListWidth;
        Hint := 'Type a folder and press Enter to show it';
      end;
    end;
    { The folders and files, with a preview beside them in a picture dialog }
    with This.Add<THBox> do
    begin
      Margin := 0;
      with This.Add<TScrollGrid>(FGrid) do
      begin
        if Preview then
          Width := ListWidth - PreviewWidth
        else
          Width := ListWidth;
        Height := ListHeight;
        ColCount := 4;
        ColTitles[ColumnName] := 'Name';
        ColTitles[ColumnSize] := 'Size';
        ColTitles[ColumnType] := 'Type';
        ColTitles[ColumnDate] := 'Date Modified';
        ColWidths[ColumnName] := NameWidth;
        ColWidths[ColumnSize] := SizeWidth;
        ColWidths[ColumnType] := TypeWidth;
        ColWidths[ColumnDate] := DateWidth;
        ColAligns[ColumnSize] := alignFar;
        HeaderRow := True;
        ColSizing := True;
        SortCol := ColumnName;
        OnDrawCell := GridDrawCell;
        OnChange := GridChange;
        OnClick := GridClick;
        OnMouseDown := GridMouseDown;
        OnMouseUp := GridMouseUp;
        OnHeaderClick := GridHeaderClick;
        OnColResize := GridColResize;
      end;
      if Preview then
        with This.Add<TFilePreview>(FPreview) do
        begin
          Width := PreviewWidth;
          Height := ListHeight;
        end;
    end;
    with This.Add<THBox> do
    begin
      Margin := 0;
      with This.Add<TLabel> do
      begin
        Align := alignCenter;
        Text := 'Name:';
        Width := LabelWidth;
      end;
      with This.Add<TEdit>(FNameEdit) do
      begin
        Align := alignCenter;
        Width := ListWidth;
        OnChange := NameChange;
      end;
    end;
    with This.Add<THBox> do
    begin
      Margin := 0;
      with This.Add<TLabel> do
      begin
        Align := alignCenter;
        Text := 'Type:';
        Width := LabelWidth;
      end;
      with This.Add<TSpinBox>(FFilterBox) do
      begin
        Align := alignCenter;
        Width := FilterWidth;
        Kind := spinDropDown;
        Hint := 'Choose which files are shown';
        OnChange := FilterChange;
      end;
      with This.Add<TCheckBox>(HiddenBox) do
      begin
        Align := alignCenter;
        Text := 'Hidden files';
        OnChange := HiddenChange;
      end;
    end;
    with This.Add<THBox> do
    begin
      Align := alignFar;
      Margin := 0;
      with This.Add<TPushButton>(AcceptButton) do
      begin
        if FKind = fdSave then
          Text := 'Save'
        else
          Text := 'Open';
        OnClick := AcceptClick;
      end;
      with This.Add<TPushButton>(CancelButton) do
      begin
        Text := 'Cancel';
        ModalResult := modalCancel;
      end;
    end;
  end;
  Hook(UpButton);
  Hook(HomeButton);
  Hook(FPathEdit);
  Hook(FGrid);
  Hook(FNameEdit);
  Hook(FFilterBox);
  Hook(HiddenBox);
  Hook(AcceptButton);
  Hook(CancelButton);
  FGrid.RowHeight := Round(FGrid.Computed.Theme.CalcTextHeight) + 8;
  FFilterBox.Items := FFilterNames;
  SetFilter(Dialog.GetFilterIndex - 1);
  { Start in the initial folder, or the folder of the file name, or the
    current folder }
  S := Dialog.GetFileName;
  FDir := Dialog.GetInitialDir;
  if (FDir = '') and (ExtractFilePath(S) <> '') then
    FDir := ExtractFilePath(S);
  FDir := ExpandDir(FDir);
  if not DirectoryExists(FDir) then
    FDir := ExpandDir('');
  FPathEdit.Text := FDir;
  FNameEdit.Text := ExtractFileName(S);
  FWindow.SizeWidget := FGrid;
  FWindow.Sizeable := True;
  { The edits are as wide as the list, which is known after packing }
  FWindow.Pack;
  Layout;
  FWindow.Pack;
  FWindow.X := Round((FMain.Width - FWindow.Width) / 2);
  FWindow.Y := Round((FMain.Height - FWindow.Height) / 2);
  FLoading := False;
  Load;
  FPick := pickEdit;
  FWindow.ShowModal(ModalDone);
  if FKind = fdSave then
    FNameEdit.Activate
  else
    FGrid.Activate;
end;

{ Every widget which can have the input focus passes its keys to WidgetKeyUp }

procedure TFileDialogWindow.Hook(Widget: TWidget);
begin
  TWidgetAccess(Widget).OnKeyUp := WidgetKeyUp;
end;

{ The filter is pairs of a description and masks separated by bars }

procedure TFileDialogWindow.ParseFilter;
var
  Parts: StringArray;
  Name, Mask: string;
  I: Integer;
begin
  Parts := StrSplit(FDialog.GetFilter, '|');
  I := 0;
  while I < Parts.Length do
  begin
    Name := Trim(Parts[I]);
    if I + 1 < Parts.Length then
      Mask := Trim(Parts[I + 1])
    else
      Mask := '*';
    if Mask = '' then
      Mask := '*';
    if Name = '' then
      Name := Mask;
    FFilterNames.Push(Name);
    FFilterMasks.Push(Mask);
    Inc(I, 2);
  end;
  if FFilterNames.Length = 0 then
  begin
    FFilterNames.Push('All files');
    FFilterMasks.Push('*');
  end;
end;

procedure TFileDialogWindow.SetFilter(Index: Integer);
var
  WasLoading: Boolean;
begin
  if Index < 0 then
    Index := 0;
  if Index > FFilterMasks.Length - 1 then
    Index := FFilterMasks.Length - 1;
  FMasks := StrSplit(LowerCase(FFilterMasks[Index]), ';');
  WasLoading := FLoading;
  FLoading := True;
  try
    FFilterBox.ItemIndex := Index;
  finally
    FLoading := WasLoading;
  end;
end;

function TFileDialogWindow.MatchMasks(const Name: string): Boolean;
var
  S: string;
  I: Integer;
begin
  if FMasks.Length = 0 then
    Exit(True);
  S := LowerCase(Name);
  for I := 0 to FMasks.Length - 1 do
    if MatchMask(S, Trim(FMasks[I])) then
      Exit(True);
  Result := False;
end;

function TFileDialogWindow.FullName(const Name: string): string;
begin
  Result := IncludeTrailingPathDelimiter(FDir) + Name;
end;

{ The full path of a name which was typed. A name which is not a full path
  is inside of the folder being shown. }

function TFileDialogWindow.ExpandName(const Name: string): string;
begin
  Result := ExpandHome(Name);
  if not ((Result[1] in AllowDirectorySeparators) or
    ((Length(Result) > 1) and (Result[2] = ':'))) then
    Result := FullName(Result);
  Result := ExpandFileName(Result);
end;

{ Folders come before files whichever way the list is sorted. Folders have
  no size, so they are sorted by name when the list is sorted by size. Items
  which are the same in the sort column are sorted by name. }

function TFileDialogWindow.CompareItems(const A, B: TFileItem): Integer;
begin
  if A.Folder <> B.Folder then
  begin
    if A.Folder then
      Result := -1
    else
      Result := 1;
    Exit;
  end;
  Result := 0;
  case FSortColumn of
    ColumnSize:
      if not A.Folder then
        if A.Size < B.Size then
          Result := -1
        else if A.Size > B.Size then
          Result := 1;
    ColumnType:
      Result := CompareText(A.Kind, B.Kind);
    ColumnDate:
      if A.Modified < B.Modified then
        Result := -1
      else if A.Modified > B.Modified then
        Result := 1;
  end;
  if Result = 0 then
    Result := CompareText(A.Name, B.Name);
  if FSortDescending then
    Result := -Result;
end;

procedure TFileDialogWindow.SortItems(L, R: Integer);

  function Before(const A, B: TFileItem): Boolean;
  begin
    Result := CompareItems(A, B) < 0;
  end;

var
  I, J: Integer;
  P, T: TFileItem;
begin
  if L >= R then
    Exit;
  I := L;
  J := R;
  P := FItems[(L + R) div 2];
  repeat
    while Before(FItems[I], P) do
      Inc(I);
    while Before(P, FItems[J]) do
      Dec(J);
    if I <= J then
    begin
      T := FItems[I];
      FItems[I] := FItems[J];
      FItems[J] := T;
      Inc(I);
      Dec(J);
    end;
  until I > J;
  SortItems(L, J);
  SortItems(I, R);
end;

{ Resort sorts the list again after the sort column or order changes. The
  rows which are selected stay selected, as that is kept in the items. The
  row of the grid and the row a range is selected from are found again by
  name, and the row of the grid is scrolled into view. }

procedure TFileDialogWindow.Resort;
var
  RowName, AnchorName: string;
  HasRow, HasAnchor: Boolean;
  Row, I: Integer;
begin
  Row := FGrid.Row;
  HasRow := (Row > -1) and (Row <= High(FItems));
  if HasRow then
    RowName := FItems[Row].Name;
  HasAnchor := (FAnchor > -1) and (FAnchor <= High(FItems));
  if HasAnchor then
    AnchorName := FItems[FAnchor].Name;
  SortItems(0, High(FItems));
  Row := -1;
  FAnchor := -1;
  FClickRow := -1;
  FOpenRow := -1;
  for I := 0 to High(FItems) do
  begin
    if HasRow and (FItems[I].Name = RowName) then
      Row := I;
    if HasAnchor and (FItems[I].Name = AnchorName) then
      FAnchor := I;
  end;
  FLoading := True;
  try
    if Row > -1 then
    begin
      FGrid.Select(0, Row);
      FGrid.ScrollToCell(0, Row);
    end
    else
      FGrid.Select(-1, -1);
  finally
    FLoading := False;
  end;
end;

{ Load reads the folder being shown into the list }

procedure TFileDialogWindow.Load;
var
  Search: TSearchRec;
  Folder: Boolean;
  Count: Integer;
begin
  Count := 0;
  SetLength(FItems, 0);
  if SysUtils.FindFirst(FullName(AllFilesMask), SysUtils.faAnyFile, Search) = 0 then
  try
    repeat
      if (Search.Name = '') or (Search.Name = '.') or (Search.Name = '..') then
        Continue;
      if (not FHidden) and (Search.Name[1] = '.') then
        Continue;
      Folder := (Search.Attr and SysUtils.faDirectory) <> 0;
      { A link to a folder is shown as a folder }
      if (not Folder) and ((Search.Attr and SysUtils.faSymLink) <> 0) then
        Folder := DirectoryExists(FullName(Search.Name));
      if (not Folder) and (not MatchMasks(Search.Name)) then
        Continue;
      if Count = Length(FItems) then
        SetLength(FItems, Count * 2 + 64);
      FItems[Count].Name := Search.Name;
      FItems[Count].Folder := Folder;
      FItems[Count].Size := Search.Size;
      if Folder then
        FItems[Count].Kind := 'Folder'
      else
        FItems[Count].Kind := KindText(Search.Name);
      FItems[Count].Selected := False;
      try
        FItems[Count].Modified := FileDateToDateTime(Search.Time);
      except
        FItems[Count].Modified := 0;
      end;
      Inc(Count);
    until SysUtils.FindNext(Search) <> 0;
  finally
    SysUtils.FindClose(Search);
  end;
  SetLength(FItems, Count);
  SortItems(0, Count - 1);
  FAnchor := -1;
  FClickRow := -1;
  FOpenRow := -1;
  FLoading := True;
  try
    FGrid.Select(-1, -1);
    FGrid.RowCount := Count;
    if Count > 0 then
      FGrid.ScrollToCell(0, 0);
  finally
    FLoading := False;
  end;
  UpdateColumns;
  if FPreview <> nil then
    FPreview.SetFile('');
end;

procedure TFileDialogWindow.Navigate(const Dir: string);
var
  S: string;
begin
  S := ExpandDir(Dir);
  if DirectoryExists(S) then
  begin
    FDir := S;
    Load;
    { The name which was typed is used next, not what was selected before }
    FPick := pickEdit;
  end;
  FPathEdit.Text := FDir;
end;

{ Up shows the folder above the one being shown, and selects the folder which
  was left }

procedure TFileDialogWindow.Up;
var
  Old, Name: string;
  I: Integer;
begin
  Old := FDir;
  Navigate(ExtractFileDir(FDir));
  if FDir = Old then
    Exit;
  Name := ExtractFileName(Old);
  for I := 0 to High(FItems) do
    if FItems[I].Folder and (FItems[I].Name = Name) then
    begin
      SelectOnly(I);
      FAnchor := I;
      FLoading := True;
      try
        FGrid.Select(0, I);
      finally
        FLoading := False;
      end;
      FPick := pickGrid;
      Break;
    end;
end;

{ Layout makes the edits reach as far across the window as the list does. A
  box places its widgets a margin apart, and all of these have the same
  margin. }

procedure TFileDialogWindow.Layout;
var
  Total, W: Float;
begin
  Total := FGrid.Margin * 2 + FGrid.Width;
  if FPreview <> nil then
  begin
    FPreview.Height := FGrid.Height;
    Total := Total + FPreview.Width + FPreview.Margin;
  end;
  W := Total - FPathEdit.X - FPathEdit.Margin;
  if W < 50 then
    W := 50;
  FPathEdit.Width := W;
  W := Total - FNameEdit.X - FNameEdit.Margin;
  if W < 50 then
    W := 50;
  FNameEdit.Width := W;
  UpdateColumns;
end;

{ The name column is as wide as the room the other columns leave in the grid,
  until a column has been resized. It is made narrow first so the room is
  measured without a horizontal scroll bar. }

procedure TFileDialogWindow.UpdateColumns;
var
  W: Float;
begin
  if FColumnsSized then
    Exit;
  FGrid.ColWidths[ColumnName] := 4;
  W := Trunc(FGrid.CellArea.Width) - FGrid.ColWidths[ColumnSize] -
    FGrid.ColWidths[ColumnType] - FGrid.ColWidths[ColumnDate];
  if W < NameWidth then
    W := NameWidth;
  FGrid.ColWidths[ColumnName] := W;
end;

procedure TFileDialogWindow.SelectOnly(Row: Integer);
var
  I: Integer;
begin
  for I := 0 to High(FItems) do
    FItems[I].Selected := I = Row;
end;

procedure TFileDialogWindow.SelectRange(A, B: Integer);
var
  I: Integer;
begin
  if A > B then
  begin
    I := A;
    A := B;
    B := I;
  end;
  for I := 0 to High(FItems) do
    FItems[I].Selected := (I >= A) and (I <= B);
end;

{ SelectionChanged puts the selected files in the name edit, and shows the
  selected file in the preview }

procedure TFileDialogWindow.SelectionChanged;
var
  Names, Last: string;
  Count, I: Integer;
begin
  Names := '';
  Last := '';
  Count := 0;
  for I := 0 to High(FItems) do
    if FItems[I].Selected and (not FItems[I].Folder) then
    begin
      Inc(Count);
      Last := FItems[I].Name;
      if Names <> '' then
        Names := Names + ' ';
      Names := Names + '"' + Last + '"';
    end;
  if Count = 1 then
    FNameEdit.Text := Last
  else if Count > 1 then
    FNameEdit.Text := Names;
  FPick := pickGrid;
  if FPreview <> nil then
    if (Count = 1) and IsImage(Last) then
      FPreview.SetFile(FullName(Last))
    else
      FPreview.SetFile('');
end;

{ OpenRow shows a folder or accepts a file }

procedure TFileDialogWindow.OpenRow(Row: Integer);
begin
  if (Row < 0) or (Row > High(FItems)) then
    Exit;
  if FItems[Row].Folder then
    Navigate(FullName(FItems[Row].Name))
  else
  begin
    SelectOnly(Row);
    FPick := pickGrid;
    { Accepting can free the window, so nothing may follow this }
    Accept;
  end;
end;

{ Accept closes the dialog with the files chosen. When there is something
  else to do first, such as showing a folder, the dialog stays open.
  Accepting can free the window, so nothing may follow a call to it. }

procedure TFileDialogWindow.Accept;
var
  Files: StringArray;
  S, P, Ext: string;
  FolderRow, I: Integer;
begin
  Files.Clear;
  if FPick = pickGrid then
  begin
    FolderRow := -1;
    for I := 0 to High(FItems) do
      if FItems[I].Selected then
        if FItems[I].Folder then
        begin
          if FolderRow < 0 then
            FolderRow := I;
        end
        else
          Files.Push(FullName(FItems[I].Name));
    if (Files.Length = 0) and (FolderRow > -1) then
    begin
      Navigate(FullName(FItems[FolderRow].Name));
      Exit;
    end;
  end;
  if Files.Length = 0 then
  begin
    S := Trim(FNameEdit.Text);
    if S = '' then
      Exit;
    { A mask shows only the files which match it }
    if (Pos('*', S) > 0) or (Pos('?', S) > 0) then
    begin
      FMasks := StrSplit(LowerCase(S), ';');
      Load;
      Exit;
    end;
    P := ExpandName(S);
    if DirectoryExists(P) then
    begin
      Navigate(P);
      Exit;
    end;
    Ext := FDialog.GetDefaultExt;
    if (Ext <> '') and (ExtractFileExt(P) = '') then
      if (FKind = fdSave) or (not SysUtils.FileExists(P)) then
      begin
        if Ext[1] <> '.' then
          Ext := '.' + Ext;
        P := P + Ext;
      end;
    if FKind = fdOpen then
    begin
      if not SysUtils.FileExists(P) then
      begin
        FMain.MessageBox(Format('The file "%s" was not found.', [ExtractFileName(P)]));
        Exit;
      end;
    end
    else if not DirectoryExists(ExtractFileDir(P)) then
    begin
      FMain.MessageBox(Format('The folder "%s" does not exist.', [ExtractFileDir(P)]));
      Exit;
    end;
    Files.Push(P);
  end;
  if (FKind = fdSave) and SysUtils.FileExists(Files[0]) then
  begin
    FPending := Files;
    FMain.MessageConfirm(Format('The file "%s" already exists. Do you want to replace it?',
      [ExtractFileName(Files[0])]), ConfirmResult);
    Exit;
  end;
  Finish(Files);
end;

procedure TFileDialogWindow.ConfirmResult(Sender: TObject; ModalResult: TModalResult);
begin
  if ModalResult = modalYes then
    Finish(FPending);
end;

{ Finish ends the modal window, which calls ModalDone and frees everything }

procedure TFileDialogWindow.Finish(const Files: StringArray);
begin
  FFiles := Files;
  FDialog.SetFilterIndex(FFilterBox.ItemIndex + 1);
  FWindow.ModalResult := modalOk;
end;

{ ModalDone is called when the window closes for any reason. The window and
  this object are freed before the dialog is told, since the dialog may open
  another window in its OnClose event. }

procedure TFileDialogWindow.ModalDone(Sender: TObject; ModalResult: TModalResult);
var
  Done: TFileDialogDone;
  Files: StringArray;
begin
  if ModalResult = modalOk then
    Files := FFiles
  else
    Files.Clear;
  Done := FOnDone;
  FWindow.Free;
  Free;
  Done(Files);
end;

{ Keys are acted on when they are released, so that a key which closes the
  dialog is not still down for whatever has the input focus afterwards }

procedure TFileDialogWindow.WidgetKeyUp(Sender: TObject; var Args: TSceneKeyArgs);
begin
  case Args.Key of
    VK_ESCAPE:
      begin
        Args.Handled := True;
        FWindow.Hide;
      end;
    VK_RETURN:
      if Sender = FPathEdit then
      begin
        Args.Handled := True;
        Navigate(FPathEdit.Text);
      end
      else if Sender = FNameEdit then
      begin
        Args.Handled := True;
        FPick := pickEdit;
        Accept;
      end
      else if Sender = FGrid then
      begin
        Args.Handled := True;
        FPick := pickGrid;
        Accept;
      end;
    VK_BACK:
      if Sender = FGrid then
      begin
        Args.Handled := True;
        Up;
      end;
  end;
end;

procedure TFileDialogWindow.WindowResize(Sender: TObject);
begin
  Layout;
end;

procedure TFileDialogWindow.UpClick(Sender: TObject);
begin
  Up;
end;

procedure TFileDialogWindow.HomeClick(Sender: TObject);
begin
  Navigate(GetUserDir);
end;

procedure TFileDialogWindow.AcceptClick(Sender: TObject);
begin
  Accept;
end;

procedure TFileDialogWindow.NameChange(Sender: TObject);
begin
  if not FLoading then
    FPick := pickEdit;
end;

procedure TFileDialogWindow.FilterChange(Sender: TObject);
begin
  if FLoading then
    Exit;
  SetFilter(FFilterBox.ItemIndex);
  Load;
end;

procedure TFileDialogWindow.HiddenChange(Sender: TObject);
begin
  FHidden := (Sender as TCheckBox).Checked;
  if not FLoading then
    Load;
end;

{ The grid selects one row. Which rows are selected is kept in the items, so
  more than one can be. A press of the mouse is handled by GridMouseDown,
  which is called before the grid selects the row, so GridChange only has to
  handle the mouse being dragged to another row and the keyboard. }

procedure TFileDialogWindow.GridChange(Sender: TObject);
var
  Row: Integer;
begin
  if FLoading then
    Exit;
  Row := FGrid.Row;
  if (Row < 0) or (Row > High(FItems)) then
    Exit;
  if FMouseDown then
  begin
    if Row = FClickRow then
      Exit;
    { Dragging to another row is not a double click }
    FOpenRow := -1;
    FClickRow := -1;
    if FMulti and (FMouseShift * [skCtrl, skShift] <> []) then
      Exit;
    SelectOnly(Row);
    FAnchor := Row;
  end
  else if FMulti and (FAnchor > -1) and IsKeyDown(VK_SHIFT) then
    SelectRange(FAnchor, Row)
  else
  begin
    SelectOnly(Row);
    FAnchor := Row;
  end;
  SelectionChanged;
end;

procedure TFileDialogWindow.GridMouseDown(Sender: TObject; var Args: TSceneMouseArgs);
var
  Col, Row: Integer;
begin
  if Args.Button <> buttonLeft then
    Exit;
  FOpenRow := -1;
  if (not FGrid.CellFromPoint(Args.X, Args.Y, Col, Row)) or (Row > High(FItems)) then
  begin
    FClickRow := -1;
    Exit;
  end;
  FMouseDown := True;
  FMouseShift := Args.Shift;
  { The row is opened by the click which follows the second press }
  if (Row = FClickRow) and (FMain.Time - FClickTime < DoubleClickTime) and
    (Args.Shift * [skCtrl, skShift] = []) then
    FOpenRow := Row;
  FClickRow := Row;
  FClickTime := FMain.Time;
  if FMulti and (skCtrl in Args.Shift) then
  begin
    FItems[Row].Selected := not FItems[Row].Selected;
    FAnchor := Row;
  end
  else if FMulti and (skShift in Args.Shift) and (FAnchor > -1) then
    SelectRange(FAnchor, Row)
  else
  begin
    SelectOnly(Row);
    FAnchor := Row;
  end;
  SelectionChanged;
end;

procedure TFileDialogWindow.GridMouseUp(Sender: TObject; var Args: TSceneMouseArgs);
begin
  FMouseDown := False;
end;

{ Clicking the header cell of the sort column reverses the order. Clicking
  another sorts by that column from first to last. }

procedure TFileDialogWindow.GridHeaderClick(Sender: TObject; Col: Integer);
begin
  if Col = FSortColumn then
    FSortDescending := not FSortDescending
  else
  begin
    FSortColumn := Col;
    FSortDescending := False;
  end;
  FGrid.SortCol := FSortColumn;
  FGrid.SortDescending := FSortDescending;
  Resort;
end;

{ Once a column has been resized the columns keep the widths they are given }

procedure TFileDialogWindow.GridColResize(Sender: TObject; Col: Integer);
begin
  FColumnsSized := True;
end;

{ The click follows the mouse being released, and is the last thing done with
  the grid by the main widget, so the window can be freed from here }

procedure TFileDialogWindow.GridClick(Sender: TObject);
var
  Row: Integer;
begin
  Row := FOpenRow;
  FOpenRow := -1;
  if Row < 0 then
    Exit;
  FClickRow := -1;
  OpenRow(Row);
end;

{ A row is a cell in each column: an icon and the name, the size, the type,
  and the date modified. A selected row is filled cell by cell. Drawing is
  clipped to the cell by the theme. The fonts belong to the theme and are
  shared with the other widgets, so they are put back as they were. }

procedure TFileDialogWindow.GridDrawCell(Sender: TObject; Surface: ICanvas;
  Row, Col: Integer; const Rect: TRectF);
var
  Theme: TCanvasTheme;
  Font, Glyph: IFont;
  FontAlign, GlyphAlign: TFontAlign;
  FontLayout, GlyphLayout: TFontLayout;
  FontColor, GlyphColor, C, H: TColorF;
  GlyphSize, Y: Float;
  S: string;
begin
  if (Row < 0) or (Row > High(FItems)) then
    Exit;
  if not (FGrid.Computed.Theme is TCanvasTheme) then
    Exit;
  Theme := TCanvasTheme(FGrid.Computed.Theme);
  Font := Theme.Font;
  Glyph := Theme.Glyph;
  if (Font = nil) or (Glyph = nil) then
    Exit;
  C := FGrid.TextColor;
  if FItems[Row].Selected then
  begin
    Surface.Rect(Rect);
    Surface.Fill(FGrid.SelectColor);
    C := FGrid.SelectTextColor;
  end
  else if (Row = FGrid.HotRow) and (wsHot in FGrid.State) then
  begin
    H := FGrid.SelectColor;
    H.Alpha := H.Alpha * 0.3;
    Surface.Rect(Rect);
    Surface.Fill(H);
  end;
  FontAlign := Font.Align;
  FontLayout := Font.Layout;
  FontColor := Font.Color;
  GlyphAlign := Glyph.Align;
  GlyphLayout := Glyph.Layout;
  GlyphColor := Glyph.Color;
  GlyphSize := Glyph.Size;
  try
    Y := Rect.Y + Rect.Height / 2;
    Font.Align := fontLeft;
    Font.Layout := fontMiddle;
    Font.Color := C;
    case Col of
      ColumnName:
        begin
          if FItems[Row].Folder then
            S := GlyphFolder
          else if IsImage(FItems[Row].Name) then
            S := GlyphImage
          else
            S := GlyphFile;
          Glyph.Size := Rect.Height * 0.75;
          Glyph.Align := fontCenter;
          Glyph.Layout := fontMiddle;
          Glyph.Color := C;
          Surface.DrawText(Glyph, S, Rect.X + 4 + IconWidth / 2, Y);
          Surface.DrawText(Font, FItems[Row].Name, Rect.X + IconWidth + 10, Y);
        end;
      ColumnSize:
        if not FItems[Row].Folder then
        begin
          Font.Align := fontRight;
          Surface.DrawText(Font, SizeText(FItems[Row].Size), Rect.X + Rect.Width - 6, Y);
        end;
      ColumnType:
        Surface.DrawText(Font, FItems[Row].Kind, Rect.X + 6, Y);
      ColumnDate:
        if FItems[Row].Modified > 0 then
          Surface.DrawText(Font, FormatDateTime('yyyy-mm-dd hh:nn', FItems[Row].Modified),
            Rect.X + 6, Y);
    end;
  finally
    Font.Align := FontAlign;
    Font.Layout := FontLayout;
    Font.Color := FontColor;
    Glyph.Align := GlyphAlign;
    Glyph.Layout := GlyphLayout;
    Glyph.Color := GlyphColor;
    Glyph.Size := GlyphSize;
  end;
end;

{ TWidgetFileDialog }

procedure TWidgetFileDialog.Show;
begin
  if WidgetDialogHost = nil then
    inherited Show
  else
    TFileDialogWindow.Create(WidgetDialogHost, Self, False, Done);
end;

procedure TWidgetFileDialog.Done(const Files: StringArray);
begin
  CloseFiles(Files);
end;

{ TWidgetPictureDialog }

procedure TWidgetPictureDialog.Show;
begin
  if WidgetDialogHost = nil then
    inherited Show
  else
    TFileDialogWindow.Create(WidgetDialogHost, Self, True, Done);
end;

procedure TWidgetPictureDialog.Done(const Files: StringArray);
begin
  CloseFiles(Files);
end;

function NewWidgetFileDialog(Kind: TFileDialogKind): IFileDialog;
begin
  Result := TWidgetFileDialog.Create(Kind);
end;

function NewWidgetPictureDialog(Kind: TFileDialogKind): IPictureDialog;
begin
  Result := TWidgetPictureDialog.Create(Kind);
end;

end.
