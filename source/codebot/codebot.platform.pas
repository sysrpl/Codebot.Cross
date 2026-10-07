(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ Codebot.Platform defines interfaces for the platform details that scenes and
  applications need: bitmaps, the keyboard, the clipboard, windows, dialogs,
  and system colors. This unit does not depend on the LCL, SDL, or any other
  backend.

  The routines and services below are assigned when the unit implementing a
  backend is initialized: Codebot.Platform.LCL in codebot_controls or
  Codebot.Platform.SDL in codebot_render_sdl. Until then they hold defaults
  which are safe to call: bitmaps hold pixels but cannot load or save, no keys
  are down, the clipboard is kept in memory, and dialogs close as soon as they
  are shown. }

unit Codebot.Platform;

{$i codebot.inc}

interface

uses
  SysUtils, Classes,
  Codebot.System;

{ Virtual key codes used by key events sent to scenes and by Codebot.Hardware. They
  match the codes used by the LCL. }

const
  VK_LBUTTON    = 1;
  VK_RBUTTON    = 2;
  VK_CANCEL     = 3;
  VK_MBUTTON    = 4;
  VK_XBUTTON1   = 5;
  VK_XBUTTON2   = 6;
  VK_BACK       = 8;
  VK_TAB        = 9;
  VK_CLEAR      = 12;
  VK_RETURN     = 13;
  VK_SHIFT      = 16;
  VK_CONTROL    = 17;
  VK_MENU       = 18;
  VK_PAUSE      = 19;
  VK_CAPITAL    = 20;
  VK_KANA       = 21;
  VK_HANGUL     = 21;
  VK_JUNJA      = 23;
  VK_FINAL      = 24;
  VK_HANJA      = 25;
  VK_KANJI      = 25;
  VK_ESCAPE     = 27;
  VK_CONVERT    = 28;
  VK_NONCONVERT = 29;
  VK_ACCEPT     = 30;
  VK_MODECHANGE = 31;
  VK_SPACE      = 32;
  VK_PRIOR      = 33;
  VK_NEXT       = 34;
  VK_END        = 35;
  VK_HOME       = 36;
  VK_LEFT       = 37;
  VK_UP         = 38;
  VK_RIGHT      = 39;
  VK_DOWN       = 40;
  VK_SELECT     = 41;
  VK_PRINT      = 42;
  VK_EXECUTE    = 43;
  VK_SNAPSHOT   = 44;
  VK_INSERT     = 45;
  VK_DELETE     = 46;
  VK_HELP       = 47;
  VK_0          = $30;
  VK_1          = $31;
  VK_2          = $32;
  VK_3          = $33;
  VK_4          = $34;
  VK_5          = $35;
  VK_6          = $36;
  VK_7          = $37;
  VK_8          = $38;
  VK_9          = $39;
  VK_A          = $41;
  VK_B          = $42;
  VK_C          = $43;
  VK_D          = $44;
  VK_E          = $45;
  VK_F          = $46;
  VK_G          = $47;
  VK_H          = $48;
  VK_I          = $49;
  VK_J          = $4A;
  VK_K          = $4B;
  VK_L          = $4C;
  VK_M          = $4D;
  VK_N          = $4E;
  VK_O          = $4F;
  VK_P          = $50;
  VK_Q          = $51;
  VK_R          = $52;
  VK_S          = $53;
  VK_T          = $54;
  VK_U          = $55;
  VK_V          = $56;
  VK_W          = $57;
  VK_X          = $58;
  VK_Y          = $59;
  VK_Z          = $5A;
  VK_LWIN       = $5B;
  VK_RWIN       = $5C;
  VK_APPS       = $5D;
  VK_SLEEP      = $5F;
  VK_NUMPAD0    = 96;
  VK_NUMPAD1    = 97;
  VK_NUMPAD2    = 98;
  VK_NUMPAD3    = 99;
  VK_NUMPAD4    = 100;
  VK_NUMPAD5    = 101;
  VK_NUMPAD6    = 102;
  VK_NUMPAD7    = 103;
  VK_NUMPAD8    = 104;
  VK_NUMPAD9    = 105;
  VK_MULTIPLY   = 106;
  VK_ADD        = 107;
  VK_SEPARATOR  = 108;
  VK_SUBTRACT   = 109;
  VK_DECIMAL    = 110;
  VK_DIVIDE     = 111;
  VK_F1         = 112;
  VK_F2         = 113;
  VK_F3         = 114;
  VK_F4         = 115;
  VK_F5         = 116;
  VK_F6         = 117;
  VK_F7         = 118;
  VK_F8         = 119;
  VK_F9         = 120;
  VK_F10        = 121;
  VK_F11        = 122;
  VK_F12        = 123;
  VK_NUMLOCK    = $90;
  VK_SCROLL     = $91;
  VK_LSHIFT     = $A0;
  VK_RSHIFT     = $A1;
  VK_LCONTROL   = $A2;
  VK_RCONTROL   = $A3;
  VK_LMENU      = $A4;
  VK_RMENU      = $A5;
  VK_OEM_1      = $BA;
  VK_OEM_PLUS   = $BB;
  VK_OEM_COMMA  = $BC;
  VK_OEM_MINUS  = $BD;
  VK_OEM_PERIOD = $BE;
  VK_OEM_2      = $BF;
  VK_OEM_3      = $C0;
  VK_OEM_4      = $DB;
  VK_OEM_5      = $DC;
  VK_OEM_6      = $DD;
  VK_OEM_7      = $DE;

{ IBitmapData holds an image as pixels in memory and can load and save it.
  SetSize resizes the image, after which the contents of Pixels are undefined.
  Pixels points to Width * Height pixels of 4 bytes each, stored in rows from
  top to bottom. Each pixel is blue, green, red, and alpha with the colors
  premultiplied by alpha. SaveToStream writes a png unless the bitmap was
  loaded from another format. }

type
  IBitmapData = interface
  ['{6E2B3F0A-9C41-4D7E-8A5B-1F3D2C7E9B40}']
    {doc off}
    function GetWidth: Integer;
    function GetHeight: Integer;
    function GetPixels: Pointer;
    {doc on}
    { Resize the image leaving the pixels undefined }
    procedure SetSize(Width, Height: Integer);
    { Load the image from a file }
    procedure LoadFromFile(const FileName: string);
    { Load the image from a stream }
    procedure LoadFromStream(Stream: TStream);
    { Save the image to a file }
    procedure SaveToFile(const FileName: string);
    { Save the image to a stream }
    procedure SaveToStream(Stream: TStream);
    { The width of the image in pixels }
    property Width: Integer read GetWidth;
    { The height of the image in pixels }
    property Height: Integer read GetHeight;
    { The address of the first pixel }
    property Pixels: Pointer read GetPixels;
  end;

{ IClipboard reads and writes text on the system clipboard. It can be called
  from any thread. }

  IClipboard = interface
  ['{4C1D8E73-6A2B-4F90-B5E3-2D7C9A1F8B04}']
    {doc off}
    function GetText: string;
    procedure SetText(const Value: string);
    {doc on}
    { Returns true if the clipboard holds text }
    function HasText: Boolean;
    { The text on the clipboard }
    property Text: string read GetText write SetText;
  end;

{ IWindow is the window showing a scene, given to scenes by their host. On the
  LCL it is the form holding the control the scene renders in, and on SDL it
  is the SDL window. It can be called from the thread running the scene.

  Scale is the ratio of pixels to logical units, such as 2 on a high
  resolution display. Close closes the window, which stops the scene. The
  mouse cursor over the scene is Mouse.Cursor in Codebot.Hardware. }

  IWindow = interface
  ['{9D3F2A86-5B7E-4E1C-A8D4-3F6B2C7E1A95}']
    {doc off}
    function GetTitle: string;
    procedure SetTitle(const Value: string);
    function GetFullscreen: Boolean;
    procedure SetFullscreen(Value: Boolean);
    function GetScale: Single;
    {doc on}
    { Close the window stopping the scene }
    procedure Close;
    { The window caption }
    property Title: string read GetTitle write SetTitle;
    { Fullscreen is true when the window covers the whole screen }
    property Fullscreen: Boolean read GetFullscreen write SetFullscreen;
    { The ratio of pixels to logical units }
    property Scale: Single read GetScale;
  end;

{ IDialog is a dialog the user closes. Execute shows the dialog and returns
  immediately, so the scene keeps running while the dialog is open. When the
  dialog closes Accepted tells whether the user chose to accept it and OnClose
  is called on the thread which called Execute.

  If Execute was called from a thread other than the main thread, OnClose is
  called the next time that thread calls PlatformDispatch. Scene hosts call
  PlatformDispatch once a frame on the thread running their scenes. Calling
  Execute while the dialog is showing does nothing. A dialog keeps itself
  alive while it is showing. }

  {doc off}
  IDialog = interface;
  {doc on}

  { TDialogCloseEvent is invoked when a dialog closes }
  TDialogCloseEvent = procedure(Dialog: IDialog) of object;

  IDialog = interface
  ['{1E7B4C92-8D3A-4F6E-9B15-6C2A8E4D7F30}']
    {doc off}
    function GetTitle: string;
    procedure SetTitle(const Value: string);
    function GetAccepted: Boolean;
    function GetShowing: Boolean;
    {doc on}
    { Show the dialog and return immediately }
    procedure Execute(OnClose: TDialogCloseEvent);
    { The dialog caption }
    property Title: string read GetTitle write SetTitle;
    { Accepted is true if the user accepted the dialog when it closed }
    property Accepted: Boolean read GetAccepted;
    { Showing is true while the dialog is open }
    property Showing: Boolean read GetShowing;
  end;

{ IMessageDialog shows a message with buttons. If no buttons are given it has
  an OK button. ButtonIndex is the index of the button chosen, or -1 if the
  dialog was closed without choosing one. Accepted is True when a button was
  chosen. }

  { TMessageKind determines the icon shown by a message dialog }
  TMessageKind = (mkInformation, mkWarning, mkError);

  IMessageDialog = interface(IDialog)
  ['{6F2A9D14-3C7B-4E85-A1D6-8B3E5F2C9A47}']
    {doc off}
    function GetMessage: string;
    procedure SetMessage(const Value: string);
    function GetKind: TMessageKind;
    procedure SetKind(Value: TMessageKind);
    function GetButtons: StringArray;
    procedure SetButtons(const Value: StringArray);
    function GetButtonIndex: Integer;
    {doc on}
    { The message text }
    property Message: string read GetMessage write SetMessage;
    { The kind of message }
    property Kind: TMessageKind read GetKind write SetKind;
    { The captions of the buttons }
    property Buttons: StringArray read GetButtons write SetButtons;
    { The index of the button chosen or -1 }
    property ButtonIndex: Integer read GetButtonIndex;
  end;

{ IFileDialog chooses a file to open or save. Accepted is True when a file was
  chosen. FileName is the chosen file, and Files holds every chosen file when
  MultiSelect is True.

  Filter lists the kinds of files shown as pairs of a description and a mask
  separated by bars, such as 'Text files|*.txt|All files|*'. FilterIndex
  selects a pair starting with 1. DefaultExt is added to a file name typed
  without an extension. A save dialog asks before overwriting a file. }

  { TFileDialogKind determines if a file dialog opens or saves files }
  TFileDialogKind = (fdOpen, fdSave);

  IFileDialog = interface(IDialog)
  ['{8A4C1E63-7D2F-4B9A-9C58-1E6D3B7A2F05}']
    {doc off}
    function GetKind: TFileDialogKind;
    procedure SetKind(Value: TFileDialogKind);
    function GetFileName: string;
    procedure SetFileName(const Value: string);
    function GetFiles: StringArray;
    function GetFilter: string;
    procedure SetFilter(const Value: string);
    function GetFilterIndex: Integer;
    procedure SetFilterIndex(Value: Integer);
    function GetInitialDir: string;
    procedure SetInitialDir(const Value: string);
    function GetDefaultExt: string;
    procedure SetDefaultExt(const Value: string);
    function GetMultiSelect: Boolean;
    procedure SetMultiSelect(Value: Boolean);
    {doc on}
    { Open or save }
    property Kind: TFileDialogKind read GetKind write SetKind;
    { The chosen file }
    property FileName: string read GetFileName write SetFileName;
    { Every chosen file when MultiSelect is true }
    property Files: StringArray read GetFiles;
    { Pairs of descriptions and masks separated by bars }
    property Filter: string read GetFilter write SetFilter;
    { The selected filter pair starting with 1 }
    property FilterIndex: Integer read GetFilterIndex write SetFilterIndex;
    { The directory shown when the dialog opens }
    property InitialDir: string read GetInitialDir write SetInitialDir;
    { The extension added to a file name typed without one }
    property DefaultExt: string read GetDefaultExt write SetDefaultExt;
    { When true more than one file can be chosen }
    property MultiSelect: Boolean read GetMultiSelect write SetMultiSelect;
  end;

{ IPictureDialog is a file dialog for images which can show a preview of the
  selected image. Its filter starts with the image formats. LoadBitmap loads
  the chosen file using NewBitmapData, or returns nil if no file was chosen. }

  IPictureDialog = interface(IFileDialog)
  ['{3B9E6F28-4A1D-4C73-B8E2-9F5A1C6D3E81}']
    { Load the chosen file or return nil if no file was chosen }
    function LoadBitmap: IBitmapData;
  end;

{ TCustomDialog implements IDialog for the backends. A backend overrides Show
  to open the dialog and calls Close when the user closes it, from any thread.
  Show is called by Execute on the thread which called Execute. The dialogs
  below close without being shown and are the defaults until a backend is
  initialized. }

  TCustomDialog = class(TInterfacedObject, IDialog)
  private
    FTitle: string;
    FAccepted: Boolean;
    FShowing: Boolean;
    FOnClose: TDialogCloseEvent;
    FThread: TThreadID;
    FSelf: IDialog;
  protected
    { Show the dialog. By default it closes without accepting. }
    procedure Show; virtual;
    { Close the dialog and deliver OnClose }
    procedure Close(Accepted: Boolean);
  public
    {doc off}
    function GetTitle: string;
    procedure SetTitle(const Value: string);
    function GetAccepted: Boolean;
    function GetShowing: Boolean;
    {doc on}
    { Show the dialog by calling Show }
    procedure Execute(OnClose: TDialogCloseEvent);
  end;

{ TCustomMessageDialog implements IMessageDialog for the backends }

  TCustomMessageDialog = class(TCustomDialog, IMessageDialog)
  protected
    FMessage: string;
    FKind: TMessageKind;
    FButtons: StringArray;
    FButtonIndex: Integer;
    { Close the dialog with the index of the button chosen, or -1 }
    procedure CloseButton(Index: Integer);
  public
    {doc off}
    function GetMessage: string;
    procedure SetMessage(const Value: string);
    function GetKind: TMessageKind;
    procedure SetKind(Value: TMessageKind);
    function GetButtons: StringArray;
    procedure SetButtons(const Value: StringArray);
    function GetButtonIndex: Integer;
    {doc on}
  end;

{ TCustomFileDialog implements IFileDialog for the backends }

  TCustomFileDialog = class(TCustomDialog, IFileDialog)
  protected
    FKind: TFileDialogKind;
    FFileName: string;
    FFiles: StringArray;
    FFilter: string;
    FFilterIndex: Integer;
    FInitialDir: string;
    FDefaultExt: string;
    FMultiSelect: Boolean;
    { Close the dialog with the files chosen, accepting it if there are any }
    procedure CloseFiles(const Files: StringArray);
  public
    { Create an open or save dialog }
    constructor Create(Kind: TFileDialogKind); virtual;
    {doc off}
    function GetKind: TFileDialogKind;
    procedure SetKind(Value: TFileDialogKind);
    function GetFileName: string;
    procedure SetFileName(const Value: string);
    function GetFiles: StringArray;
    function GetFilter: string;
    procedure SetFilter(const Value: string);
    function GetFilterIndex: Integer;
    procedure SetFilterIndex(Value: Integer);
    function GetInitialDir: string;
    procedure SetInitialDir(const Value: string);
    function GetDefaultExt: string;
    procedure SetDefaultExt(const Value: string);
    function GetMultiSelect: Boolean;
    procedure SetMultiSelect(Value: Boolean);
    {doc on}
  end;

{ TCustomPictureDialog implements IPictureDialog for the backends }

  TCustomPictureDialog = class(TCustomFileDialog, IPictureDialog)
  public
    { Create an open or save dialog with PictureFilter as its filter }
    constructor Create(Kind: TFileDialogKind); override;
    { Load the chosen file or return nil if no file was chosen }
    function LoadBitmap: IBitmapData;
  end;

const
  { The default filter used by picture dialogs }
  PictureFilter = 'Images|*.png;*.jpg;*.jpeg;*.bmp;*.gif;*.tga;*.tif;*.tiff;*.webp|All files|*';

{ PlatformDispatch delivers the OnClose events of dialogs executed on the
  calling thread which closed on another thread }

procedure PlatformDispatch;

{ Platform routines and services assigned by the backend }

var
  { Create an empty bitmap }
  NewBitmapData: function: IBitmapData;
  { Create a message dialog }
  NewMessageDialog: function: IMessageDialog;
  { Create a file dialog to open or save files }
  NewFileDialog: function(Kind: TFileDialogKind): IFileDialog;
  { Create a picture dialog to open or save images }
  NewPictureDialog: function(Kind: TFileDialogKind): IPictureDialog;
  { The system clipboard }
  PlatformClipboard: IClipboard;
  { Convert a system color, such as the LCL clBtnFace, to a color with only red,
    green and blue. Colors which are not system colors are returned unchanged. }
  SystemColorToRGB: function(Color: LongInt): LongInt;

implementation

uses
  SyncObjs;

{ Dialogs closed on another thread wait here until their thread dispatches }

type
  TPendingClose = record
    Dialog: IDialog;
    OnClose: TDialogCloseEvent;
    Thread: TThreadID;
  end;

var
  PendingLock: TCriticalSection;
  Pending: array of TPendingClose;

procedure PlatformDispatch;
var
  Thread: TThreadID;
  Ready: array of TPendingClose;
  I, J: Integer;
begin
  if PendingLock = nil then
    Exit;
  Thread := GetCurrentThreadId;
  Ready := nil;
  PendingLock.Enter;
  try
    J := 0;
    for I := 0 to High(Pending) do
      if Pending[I].Thread = Thread then
      begin
        SetLength(Ready, Length(Ready) + 1);
        Ready[High(Ready)] := Pending[I];
      end
      else
      begin
        Pending[J] := Pending[I];
        Inc(J);
      end;
    SetLength(Pending, J);
  finally
    PendingLock.Leave;
  end;
  for I := 0 to High(Ready) do
    Ready[I].OnClose(Ready[I].Dialog);
end;

{ TCustomDialog }

function TCustomDialog.GetTitle: string;
begin
  Result := FTitle;
end;

procedure TCustomDialog.SetTitle(const Value: string);
begin
  FTitle := Value;
end;

function TCustomDialog.GetAccepted: Boolean;
begin
  Result := FAccepted;
end;

function TCustomDialog.GetShowing: Boolean;
begin
  Result := FShowing;
end;

procedure TCustomDialog.Execute(OnClose: TDialogCloseEvent);
begin
  if FShowing then
    Exit;
  FShowing := True;
  FAccepted := False;
  FOnClose := OnClose;
  FThread := GetCurrentThreadId;
  { The dialog holds a reference to itself until it closes }
  FSelf := Self;
  Show;
end;

procedure TCustomDialog.Show;
begin
  Close(False);
end;

procedure TCustomDialog.Close(Accepted: Boolean);
var
  Dialog: IDialog;
  Item: TPendingClose;
begin
  if not FShowing then
    Exit;
  Dialog := FSelf;
  FSelf := nil;
  FAccepted := Accepted;
  FShowing := False;
  if not Assigned(FOnClose) then
    Exit;
  if GetCurrentThreadId = FThread then
    FOnClose(Dialog)
  else
  begin
    Item.Dialog := Dialog;
    Item.OnClose := FOnClose;
    Item.Thread := FThread;
    PendingLock.Enter;
    try
      SetLength(Pending, Length(Pending) + 1);
      Pending[High(Pending)] := Item;
    finally
      PendingLock.Leave;
    end;
  end;
end;

{ TCustomMessageDialog }

procedure TCustomMessageDialog.CloseButton(Index: Integer);
begin
  FButtonIndex := Index;
  Close(Index > -1);
end;

function TCustomMessageDialog.GetMessage: string;
begin
  Result := FMessage;
end;

procedure TCustomMessageDialog.SetMessage(const Value: string);
begin
  FMessage := Value;
end;

function TCustomMessageDialog.GetKind: TMessageKind;
begin
  Result := FKind;
end;

procedure TCustomMessageDialog.SetKind(Value: TMessageKind);
begin
  FKind := Value;
end;

function TCustomMessageDialog.GetButtons: StringArray;
begin
  Result := FButtons;
end;

procedure TCustomMessageDialog.SetButtons(const Value: StringArray);
begin
  FButtons := Value;
end;

function TCustomMessageDialog.GetButtonIndex: Integer;
begin
  Result := FButtonIndex;
end;

{ TCustomFileDialog }

constructor TCustomFileDialog.Create(Kind: TFileDialogKind);
begin
  inherited Create;
  FKind := Kind;
  FFilter := 'All files|*';
  FFilterIndex := 1;
end;

procedure TCustomFileDialog.CloseFiles(const Files: StringArray);
begin
  FFiles := Files;
  if FFiles.Length > 0 then
    FFileName := FFiles[0];
  Close(FFiles.Length > 0);
end;

function TCustomFileDialog.GetKind: TFileDialogKind;
begin
  Result := FKind;
end;

procedure TCustomFileDialog.SetKind(Value: TFileDialogKind);
begin
  FKind := Value;
end;

function TCustomFileDialog.GetFileName: string;
begin
  Result := FFileName;
end;

procedure TCustomFileDialog.SetFileName(const Value: string);
begin
  FFileName := Value;
end;

function TCustomFileDialog.GetFiles: StringArray;
begin
  Result := FFiles;
end;

function TCustomFileDialog.GetFilter: string;
begin
  Result := FFilter;
end;

procedure TCustomFileDialog.SetFilter(const Value: string);
begin
  FFilter := Value;
end;

function TCustomFileDialog.GetFilterIndex: Integer;
begin
  Result := FFilterIndex;
end;

procedure TCustomFileDialog.SetFilterIndex(Value: Integer);
begin
  FFilterIndex := Value;
end;

function TCustomFileDialog.GetInitialDir: string;
begin
  Result := FInitialDir;
end;

procedure TCustomFileDialog.SetInitialDir(const Value: string);
begin
  FInitialDir := Value;
end;

function TCustomFileDialog.GetDefaultExt: string;
begin
  Result := FDefaultExt;
end;

procedure TCustomFileDialog.SetDefaultExt(const Value: string);
begin
  FDefaultExt := Value;
end;

function TCustomFileDialog.GetMultiSelect: Boolean;
begin
  Result := FMultiSelect;
end;

procedure TCustomFileDialog.SetMultiSelect(Value: Boolean);
begin
  FMultiSelect := Value;
end;

{ TCustomPictureDialog }

constructor TCustomPictureDialog.Create(Kind: TFileDialogKind);
begin
  inherited Create(Kind);
  FFilter := PictureFilter;
end;

function TCustomPictureDialog.LoadBitmap: IBitmapData;
begin
  if FFileName = '' then
    Exit(nil);
  Result := NewBitmapData;
  Result.LoadFromFile(FFileName);
end;

{ Defaults used until a backend is initialized }

type
  TDefaultBitmapData = class(TInterfacedObject, IBitmapData)
  private
    FWidth: Integer;
    FHeight: Integer;
    FPixels: TBytes;
    procedure NoBackend;
  public
    function GetWidth: Integer;
    function GetHeight: Integer;
    function GetPixels: Pointer;
    procedure SetSize(Width, Height: Integer);
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(Stream: TStream);
  end;

  TDefaultClipboard = class(TInterfacedObject, IClipboard)
  private
    FText: string;
  public
    function GetText: string;
    procedure SetText(const Value: string);
    function HasText: Boolean;
  end;

procedure TDefaultBitmapData.NoBackend;
begin
  raise EInOutError.Create('Bitmaps cannot be loaded or saved without ' +
    'Codebot.Platform.LCL or Codebot.Platform.SDL');
end;

function TDefaultBitmapData.GetWidth: Integer;
begin
  Result := FWidth;
end;

function TDefaultBitmapData.GetHeight: Integer;
begin
  Result := FHeight;
end;

function TDefaultBitmapData.GetPixels: Pointer;
begin
  if Length(FPixels) = 0 then
    Result := nil
  else
    Result := @FPixels[0];
end;

procedure TDefaultBitmapData.SetSize(Width, Height: Integer);
begin
  if (Width < 1) or (Height < 1) then
  begin
    Width := 0;
    Height := 0;
  end;
  FWidth := Width;
  FHeight := Height;
  FPixels := nil;
  SetLength(FPixels, Width * Height * 4);
end;

procedure TDefaultBitmapData.LoadFromFile(const FileName: string);
begin
  NoBackend;
end;

procedure TDefaultBitmapData.LoadFromStream(Stream: TStream);
begin
  NoBackend;
end;

procedure TDefaultBitmapData.SaveToFile(const FileName: string);
begin
  NoBackend;
end;

procedure TDefaultBitmapData.SaveToStream(Stream: TStream);
begin
  NoBackend;
end;

function TDefaultClipboard.GetText: string;
begin
  PendingLock.Enter;
  try
    Result := FText;
  finally
    PendingLock.Leave;
  end;
end;

procedure TDefaultClipboard.SetText(const Value: string);
begin
  PendingLock.Enter;
  try
    FText := Value;
  finally
    PendingLock.Leave;
  end;
end;

function TDefaultClipboard.HasText: Boolean;
begin
  Result := GetText <> '';
end;

function DefaultBitmapData: IBitmapData;
begin
  Result := TDefaultBitmapData.Create;
end;

function DefaultMessageDialog: IMessageDialog;
begin
  Result := TCustomMessageDialog.Create;
end;

function DefaultFileDialog(Kind: TFileDialogKind): IFileDialog;
begin
  Result := TCustomFileDialog.Create(Kind);
end;

function DefaultPictureDialog(Kind: TFileDialogKind): IPictureDialog;
begin
  Result := TCustomPictureDialog.Create(Kind);
end;

{ The default system colors are those of Windows, indexed by the low byte of
  the system color }

function DefaultSystemColorToRGB(Color: LongInt): LongInt;
const
  SystemColors: array[0..31] of LongInt = (
    $C8C8C8, $000000, $D1B499, $DBCDBF, $F0F0F0, $FFFFFF, $646464, $000000,
    $000000, $000000, $B4B4B4, $FCF7F4, $ABABAB, $D77800, $FFFFFF, $F0F0F0,
    $A0A0A0, $6D6D6D, $000000, $000000, $FFFFFF, $696969, $E3E3E3, $000000,
    $E1FFFF, $B4B4B4, $CC6600, $EAD1B9, $F2E4D7, $FF9933, $F0F0F0, $F0F0F0);
var
  I: Integer;
begin
  if Color >= 0 then
    Exit(Color);
  I := Color and $FF;
  if I <= High(SystemColors) then
    Result := SystemColors[I]
  else
    Result := $F0F0F0;
end;

initialization
  PendingLock := TCriticalSection.Create;
  NewBitmapData := DefaultBitmapData;
  NewMessageDialog := DefaultMessageDialog;
  NewFileDialog := DefaultFileDialog;
  NewPictureDialog := DefaultPictureDialog;
  PlatformClipboard := TDefaultClipboard.Create;
  SystemColorToRGB := DefaultSystemColorToRGB;
finalization
  PlatformClipboard := nil;
  Pending := nil;
  FreeAndNil(PendingLock);
end.
