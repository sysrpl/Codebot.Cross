(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ Codebot.Platform.LCL provides the Codebot.Platform interfaces using the LCL.
  Bitmaps are created by Codebot.Graphics, which uses Cairo on Linux and
  Direct2D on Windows. Dialogs are the standard LCL dialogs. It takes
  the place of Codebot.Platform.SDL in programs which use the LCL.

  Scenes run on the render thread of a TGraphicsBox while the LCL may only be
  used on the main thread. The clipboard and window reads wait for the main
  thread, while window changes and dialogs are queued to it.

  The platform routines are assigned when this unit is initialized. If a
  program also uses Codebot.Platform.SDL, the unit initialized last decides
  which backend is used. }

unit Codebot.Platform.LCL;

{$i ../codebot/codebot.inc}

interface

uses
  Controls,
  Codebot.Platform;

{ Create an empty LCL bitmap }
function NewBitmapDataLCL: IBitmapData;

{ Create a window for the form holding a control }
function NewWindowLCL(Control: TWinControl): IWindow;

implementation

{ SyncObjs is listed after LCLType, which declares a different TCriticalSection }

uses
  SysUtils, Classes, Graphics, Forms, Dialogs, ExtDlgs, Clipbrd,
  LCLIntf, LCLType,
  Codebot.System,
  Codebot.Graphics,
  SyncObjs;

function IsMainThread: Boolean;
begin
  Result := GetCurrentThreadId = MainThreadID;
end;

{ Run a method on the main thread and wait for it to finish }

procedure RunMain(Method: TThreadMethod);
begin
  if IsMainThread then
    Method
  else
    TThread.Synchronize(nil, Method);
end;

{ Run a method on the main thread without waiting }

procedure QueueMain(Method: TThreadMethod);
begin
  if IsMainThread then
    Method
  else
    TThread.Queue(nil, Method);
end;

{ Bitmaps }

function NewBitmapDataLCL: IBitmapData;
begin
  Result := NewBitmap;
end;

{ TClipboardLCL }

type
  TClipboardLCL = class(TInterfacedObject, IClipboard)
  private
    FLock: TCriticalSection;
    FText: string;
    FHasText: Boolean;
    procedure ReadText;
    procedure WriteText;
  public
    constructor Create;
    destructor Destroy; override;
    function GetText: string;
    procedure SetText(const Value: string);
    function HasText: Boolean;
  end;

constructor TClipboardLCL.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
end;

destructor TClipboardLCL.Destroy;
begin
  FLock.Free;
  inherited Destroy;
end;

procedure TClipboardLCL.ReadText;
begin
  FText := Clipboard.AsText;
  FHasText := Clipboard.HasFormat(CF_TEXT);
end;

procedure TClipboardLCL.WriteText;
begin
  Clipboard.AsText := FText;
end;

{ The lock keeps two threads from using FText at the same time }

function TClipboardLCL.GetText: string;
begin
  FLock.Enter;
  try
    RunMain(ReadText);
    Result := FText;
  finally
    FLock.Leave;
  end;
end;

procedure TClipboardLCL.SetText(const Value: string);
begin
  FLock.Enter;
  try
    FText := Value;
    RunMain(WriteText);
  finally
    FLock.Leave;
  end;
end;

function TClipboardLCL.HasText: Boolean;
begin
  FLock.Enter;
  try
    RunMain(ReadText);
    Result := FHasText;
  finally
    FLock.Leave;
  end;
end;

{ TWindowLCL }

type
  TWindowLCL = class(TInterfacedObject, IWindow)
  private
    FLock: TCriticalSection;
    FControl: TWinControl;
    FTitle: string;
    FFullscreen: Boolean;
    FScale: Single;
    function Form: TCustomForm;
    procedure ReadState;
    procedure WriteTitle;
    procedure WriteFullscreen;
    procedure CloseForm;
  public
    constructor Create(Control: TWinControl);
    destructor Destroy; override;
    function GetTitle: string;
    procedure SetTitle(const Value: string);
    function GetFullscreen: Boolean;
    procedure SetFullscreen(Value: Boolean);
    function GetScale: Single;
    procedure Close;
  end;

constructor TWindowLCL.Create(Control: TWinControl);
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FControl := Control;
  FScale := 1;
end;

destructor TWindowLCL.Destroy;
begin
  { Changes still queued to the main thread are dropped }
  TThread.RemoveQueuedEvents(nil, WriteTitle);
  TThread.RemoveQueuedEvents(nil, WriteFullscreen);
  TThread.RemoveQueuedEvents(nil, CloseForm);
  FLock.Free;
  inherited Destroy;
end;

function TWindowLCL.Form: TCustomForm;
begin
  Result := GetParentForm(FControl);
end;

procedure TWindowLCL.ReadState;
var
  F: TCustomForm;
begin
  F := Form;
  if F <> nil then
  begin
    FTitle := F.Caption;
    FFullscreen := F.WindowState = wsFullScreen;
  end;
  FScale := FControl.GetCanvasScaleFactor;
  if FScale <= 0 then
    FScale := 1;
end;

procedure TWindowLCL.WriteTitle;
var
  F: TCustomForm;
begin
  F := Form;
  if F <> nil then
    F.Caption := FTitle;
end;

procedure TWindowLCL.WriteFullscreen;
var
  F: TCustomForm;
begin
  F := Form;
  if F = nil then
    Exit;
  if FFullscreen then
    F.WindowState := wsFullScreen
  else
    F.WindowState := wsNormal;
end;

procedure TWindowLCL.CloseForm;
var
  F: TCustomForm;
begin
  F := Form;
  if F <> nil then
    F.Close;
end;

function TWindowLCL.GetTitle: string;
begin
  FLock.Enter;
  try
    RunMain(ReadState);
    Result := FTitle;
  finally
    FLock.Leave;
  end;
end;

procedure TWindowLCL.SetTitle(const Value: string);
begin
  FLock.Enter;
  try
    FTitle := Value;
  finally
    FLock.Leave;
  end;
  QueueMain(WriteTitle);
end;

function TWindowLCL.GetFullscreen: Boolean;
begin
  FLock.Enter;
  try
    RunMain(ReadState);
    Result := FFullscreen;
  finally
    FLock.Leave;
  end;
end;

procedure TWindowLCL.SetFullscreen(Value: Boolean);
begin
  FLock.Enter;
  try
    FFullscreen := Value;
  finally
    FLock.Leave;
  end;
  QueueMain(WriteFullscreen);
end;

function TWindowLCL.GetScale: Single;
begin
  FLock.Enter;
  try
    RunMain(ReadState);
    Result := FScale;
  finally
    FLock.Leave;
  end;
end;

{ Closing the form may stop the render thread, so it is queued and not waited
  for }

procedure TWindowLCL.Close;
begin
  if IsMainThread then
    CloseForm
  else
    TThread.Queue(nil, CloseForm);
end;

function NewWindowLCL(Control: TWinControl): IWindow;
begin
  Result := TWindowLCL.Create(Control);
end;

{ Dialogs are shown on the main thread. When executed from another thread
  they are queued, and OnClose is delivered by PlatformDispatch. }

type
  TMessageDialogLCL = class(TCustomMessageDialog)
  private
    procedure ShowMain;
  protected
    procedure Show; override;
  end;

procedure TMessageDialogLCL.Show;
begin
  QueueMain(ShowMain);
end;

procedure TMessageDialogLCL.ShowMain;
const
  DialogTypes: array[TMessageKind] of TMsgDlgType =
    (mtInformation, mtWarning, mtError);
  { Buttons are given modal results which do not match the standard ones }
  ButtonResult = 100;
var
  Args: array of TVarRec;
  R: TModalResult;
  I: Integer;
begin
  if FButtons.Length = 0 then
  begin
    MessageDlg(GetTitle, FMessage, DialogTypes[FKind], [mbOK], 0);
    CloseButton(0);
    Exit;
  end;
  SetLength(Args, FButtons.Length * 2);
  for I := 0 to FButtons.Length - 1 do
  begin
    Args[I * 2].VType := vtInteger;
    Args[I * 2].VInteger := ButtonResult + I;
    Args[I * 2 + 1].VType := vtAnsiString;
    Args[I * 2 + 1].VAnsiString := Pointer(FButtons.Items[I]);
  end;
  R := QuestionDlg(GetTitle, FMessage, DialogTypes[FKind], Args, 0);
  if (R >= ButtonResult) and (R < ButtonResult + FButtons.Length) then
    CloseButton(R - ButtonResult)
  else
    CloseButton(-1);
end;

type
  TFileDialogLCL = class(TCustomFileDialog)
  private
    procedure ShowMain;
  protected
    function CreateDialog: TOpenDialog; virtual;
    procedure Show; override;
  end;

  TPictureDialogLCL = class(TCustomPictureDialog)
  private
    procedure ShowMain;
  protected
    procedure Show; override;
  end;

{ Copy the properties of a file dialog to an LCL dialog, run it, and return
  the files chosen }

function RunFileDialog(Source: TCustomFileDialog; Dialog: TOpenDialog): StringArray;
var
  I: Integer;
begin
  Result.Clear;
  Dialog.Title := Source.GetTitle;
  Dialog.FileName := Source.GetFileName;
  Dialog.Filter := Source.GetFilter;
  Dialog.FilterIndex := Source.GetFilterIndex;
  Dialog.InitialDir := Source.GetInitialDir;
  Dialog.DefaultExt := Source.GetDefaultExt;
  if Source.GetKind = fdSave then
    Dialog.Options := Dialog.Options + [ofOverwritePrompt]
  else
    Dialog.Options := Dialog.Options + [ofFileMustExist];
  if Source.GetMultiSelect then
    Dialog.Options := Dialog.Options + [ofAllowMultiSelect];
  if not Dialog.Execute then
    Exit;
  Source.SetFilterIndex(Dialog.FilterIndex);
  if Dialog.Files.Count > 0 then
    for I := 0 to Dialog.Files.Count - 1 do
      Result.Push(Dialog.Files[I])
  else if Dialog.FileName <> '' then
    Result.Push(Dialog.FileName);
end;

function TFileDialogLCL.CreateDialog: TOpenDialog;
begin
  if FKind = fdSave then
    Result := TSaveDialog.Create(nil)
  else
    Result := TOpenDialog.Create(nil);
end;

procedure TFileDialogLCL.Show;
begin
  QueueMain(ShowMain);
end;

procedure TFileDialogLCL.ShowMain;
var
  Dialog: TOpenDialog;
  Files: StringArray;
begin
  Dialog := CreateDialog;
  try
    Files := RunFileDialog(Self, Dialog);
  finally
    Dialog.Free;
  end;
  CloseFiles(Files);
end;

procedure TPictureDialogLCL.Show;
begin
  QueueMain(ShowMain);
end;

procedure TPictureDialogLCL.ShowMain;
var
  Dialog: TOpenDialog;
  Files: StringArray;
begin
  if FKind = fdSave then
    Dialog := TSavePictureDialog.Create(nil)
  else
    Dialog := TOpenPictureDialog.Create(nil);
  try
    Files := RunFileDialog(Self, Dialog);
  finally
    Dialog.Free;
  end;
  CloseFiles(Files);
end;

function NewMessageDialogLCL: IMessageDialog;
begin
  Result := TMessageDialogLCL.Create;
end;

function NewFileDialogLCL(Kind: TFileDialogKind): IFileDialog;
begin
  Result := TFileDialogLCL.Create(Kind);
end;

function NewPictureDialogLCL(Kind: TFileDialogKind): IPictureDialog;
begin
  Result := TPictureDialogLCL.Create(Kind);
end;

{ System colors are converted by the widgetset }

var
  ThemeColorsLoaded: Boolean;

function SystemColorToRGBLCL(Color: LongInt): LongInt;
begin
  { The gtk3 widgetset only reads the theme highlight and caption colors when
    clForm is first requested, so request it once before any other color }
  if not ThemeColorsLoaded then
  begin
    ThemeColorsLoaded := True;
    ColorToRGB(clForm);
  end;
  Result := ColorToRGB(TColor(Color));
end;

initialization
  NewBitmapData := NewBitmapDataLCL;
  NewMessageDialog := NewMessageDialogLCL;
  NewFileDialog := NewFileDialogLCL;
  NewPictureDialog := NewPictureDialogLCL;
  PlatformClipboard := TClipboardLCL.Create;
  SystemColorToRGB := SystemColorToRGBLCL;
end.
