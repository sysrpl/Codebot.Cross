(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.windows.shell.tools.txt> }
unit Codebot.Windows.Shell.Tools;

{$i ../codebot/codebot.inc}

interface

{$ifdef windows}
uses
  Windows, Messages, ActiveX, ShlObj, SysUtils, Classes, LMessages,
  Codebot.System,
  Codebot.Interop.Windows.Shell,
  Codebot.Windows.Explorer;

const
  { Sent by the shell to a TShellNotify window when something changes }
  WM_SHELLCHANGE = WM_USER + $54;

{ TShellFileOption is an option of a TShellFileOperation }

type
  TShellFileOption = (foAllowUndo, foNoConfirmation, foNoConfirmMakeFolder,
    foNoErrorUI, foSilent, foRenameOnCollision, foFilesOnly, foKeepNewerFile,
    foPreserveExtensions, foShowElevationPrompt, foEarlyFailure);

  TShellFileOptions = set of TShellFileOption;

{ TShellFileOperation copies, moves, renames, deletes, and creates files and
  folders using Windows, which shows its progress dialog, asks about
  conflicts, and records operations so they can be undone in Windows
  Explorer. Operations are added and then performed together by Execute, or
  performed at once by Copy, Move, Rename, Delete, and NewFolder. Paths are
  file system paths, shell parsing names, or known folders such as
  shell:Downloads }

  TShellFileOperation = class(TComponent)
  private
    FAborted: Boolean;
    FItems: TList;
    FOptions: TShellFileOptions;
    procedure AddItem(Kind: Integer; const Source, Folder, Name: string);
    function GetCount: Integer;
  public
    { Create a new shell file operation }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Remove the operations added }
    procedure Clear;
    { Add copying a file or folder into a folder, optionally with a new name }
    procedure AddCopy(const Source, Folder: string; const NewName: string = '');
    { Add moving a file or folder into a folder, optionally with a new name }
    procedure AddMove(const Source, Folder: string; const NewName: string = '');
    { Add renaming a file or folder }
    procedure AddRename(const Source, NewName: string);
    { Add deleting a file or folder. With the foAllowUndo option it is moved
      to the recycle bin }
    procedure AddDelete(const Source: string);
    { Add creating a folder inside a folder }
    procedure AddNewFolder(const Folder, Name: string);
    { Add creating an empty file inside a folder }
    procedure AddNewFile(const Folder, Name: string);
    { Perform the operations added and then remove them. Returns false if
      the user cancelled or an operation failed. Raises an exception if a
      path is not found }
    function Execute: Boolean;
    { Copy files or folders into a folder }
    function Copy(const Source, Folder: string): Boolean; overload;
    function Copy(Source: TStrings; const Folder: string): Boolean; overload;
    { Move files or folders into a folder }
    function Move(const Source, Folder: string): Boolean; overload;
    function Move(Source: TStrings; const Folder: string): Boolean; overload;
    { Rename a file or folder }
    function Rename(const Source, NewName: string): Boolean;
    { Delete files or folders }
    function Delete(const Source: string): Boolean; overload;
    function Delete(Source: TStrings): Boolean; overload;
    { Create a folder inside a folder }
    function NewFolder(const Folder, Name: string): Boolean;
    { The user cancelled an operation during the last Execute }
    property Aborted: Boolean read FAborted;
    { The number of operations added }
    property Count: Integer read GetCount;
  published
    { The options of the operations }
    property Options: TShellFileOptions read FOptions write FOptions default [foAllowUndo];
  end;

{ TShellChange is a kind of change reported by a TShellNotify }

  TShellChange = (scCreate, scDelete, scRename, scCreateFolder, scDeleteFolder,
    scRenameFolder, scAttributes, scUpdateFolder, scUpdateItem, scDriveAdd,
    scDriveRemove, scMediaInsert, scMediaRemove, scFreeSpace, scAssociations);

  TShellChanges = set of TShellChange;

{ TShellChangeEvent reports a change. NewPath is the new path of a renamed
  item and is empty otherwise }

  TShellChangeEvent = procedure(Sender: TObject; Change: TShellChange;
    const Path, NewPath: string) of object;

const
  AllShellChanges = [Low(TShellChange)..High(TShellChange)];

{ TShellNotify reports the changes Windows makes to files, folders, and
  drives, such as items being created, deleted, or renamed in a folder. It
  receives the same notifications Windows Explorer uses to refresh its
  windows }

type
  TShellNotify = class(TComponent)
  private
    FActive: Boolean;
    FChanges: TShellChanges;
    FIDList: PItemIDList;
    FPath: string;
    FRecursive: Boolean;
    FRegistration: ULONG;
    FWindow: HWND;
    FOnChange: TShellChangeEvent;
    procedure Start;
    procedure Stop;
    procedure Restart;
    procedure WindowProc(var Message: TLMessage);
    procedure SetActive(Value: Boolean);
    procedure SetChanges(Value: TShellChanges);
    procedure SetPath(const Value: string);
    procedure SetRecursive(Value: Boolean);
  protected
    procedure Loaded; override;
  public
    { Create a new shell notify }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Changes are reported while Active is true }
    property Active: Boolean read FActive write SetActive default False;
    { The kinds of changes reported }
    property Changes: TShellChanges read FChanges write SetChanges default AllShellChanges;
    { The folder watched. When empty the desktop is watched, which with
      Recursive reports changes anywhere }
    property Path: string read FPath write SetPath;
    { Report changes in the subfolders of Path too }
    property Recursive: Boolean read FRecursive write SetRecursive default True;
    { Invoked when something changes }
    property OnChange: TShellChangeEvent read FOnChange write FOnChange;
  end;

{ TShellLinkShow is the window state a shortcut starts its program with }

  TShellLinkShow = (lsNormal, lsMaximized, lsMinimized);

{ TShellLink reads and writes Windows shortcuts, the .lnk files used by the
  desktop and the start menu. Set the properties and call Save, or call Load
  to read a shortcut into the properties }

  TShellLink = class(TComponent)
  private
    FArguments: string;
    FDescription: string;
    FFileName: string;
    FHotKey: Word;
    FIconIndex: Integer;
    FIconLocation: string;
    FShow: TShellLinkShow;
    FTarget: string;
    FWorkingDirectory: string;
    procedure ReadLink(Link: IShellLinkW);
  public
    { Clear the properties except FileName }
    procedure Clear;
    { Read the shortcut named by FileName }
    procedure Load; overload;
    { Read a shortcut and set FileName }
    procedure Load(const FileName: string); overload;
    { Write the shortcut named by FileName }
    procedure Save; overload;
    { Write a shortcut and set FileName }
    procedure Save(const FileName: string); overload;
    { Find the target of a shortcut whose target was moved or renamed, saving
      the shortcut if it was found. When ShowUI is true Windows may ask the
      user to find it. Returns false if the target was not found }
    function Resolve(ShowUI: Boolean = False): Boolean;
  published
    { The command line arguments passed to the target }
    property Arguments: string read FArguments write FArguments;
    { The comment shown as the tooltip of the shortcut }
    property Description: string read FDescription write FDescription;
    { The .lnk file of the shortcut }
    property FileName: string read FFileName write FFileName;
    { The keyboard shortcut which opens the target, with the virtual key in
      the low byte and HOTKEYF_ modifiers in the high byte }
    property HotKey: Word read FHotKey write FHotKey default 0;
    { The index of the icon in IconLocation }
    property IconIndex: Integer read FIconIndex write FIconIndex default 0;
    { The file holding the icon. When empty the icon of the target is used }
    property IconLocation: string read FIconLocation write FIconLocation;
    { The window state the target starts with }
    property Show: TShellLinkShow read FShow write FShow default lsNormal;
    { The file, folder, or shell item the shortcut opens }
    property Target: string read FTarget write FTarget;
    { The folder the target starts in }
    property WorkingDirectory: string read FWorkingDirectory write FWorkingDirectory;
  end;
{$endif}

implementation

{$ifdef windows}
uses
  ComObj, LCLIntf;

{ Shell helpers }

function PathToItem(const Path: string): IShellItem;
var
  pidl: PItemIDList;
begin
  Result := nil;
  pidl := ShellPathToIDList(Path);
  if pidl = nil then
    raise Exception.CreateFmt('"%s" was not found', [Path]);
  try
    OleCheck(SHCreateItemFromIDList(pidl, IShellItem, Result));
  finally
    CoTaskMemFree(pidl);
  end;
end;

{ TShellFileOperation }

const
  OperationCopy = 0;
  OperationMove = 1;
  OperationRename = 2;
  OperationDelete = 3;
  OperationNewFolder = 4;
  OperationNewFile = 5;

  OptionFlags: array[TShellFileOption] of DWORD = (FOF_ALLOWUNDO,
    FOF_NOCONFIRMATION, FOF_NOCONFIRMMKDIR, FOF_NOERRORUI, FOF_SILENT,
    FOF_RENAMEONCOLLISION, FOF_FILESONLY, FOFX_KEEPNEWERFILE,
    FOFX_PRESERVEFILEEXTENSIONS, FOFX_SHOWELEVATIONPROMPT, FOFX_EARLYFAILURE);

type
  TOperationItem = class
  public
    Kind: Integer;
    Source: string;
    Folder: string;
    Name: string;
  end;

constructor TShellFileOperation.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TList.Create;
  FOptions := [foAllowUndo];
end;

destructor TShellFileOperation.Destroy;
begin
  Clear;
  FItems.Free;
  inherited Destroy;
end;

procedure TShellFileOperation.Clear;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    TObject(FItems[I]).Free;
  FItems.Clear;
end;

function TShellFileOperation.GetCount: Integer;
begin
  Result := FItems.Count;
end;

procedure TShellFileOperation.AddItem(Kind: Integer; const Source, Folder,
  Name: string);
var
  Item: TOperationItem;
begin
  Item := TOperationItem.Create;
  Item.Kind := Kind;
  Item.Source := Source;
  Item.Folder := Folder;
  Item.Name := Name;
  FItems.Add(Item);
end;

procedure TShellFileOperation.AddCopy(const Source, Folder: string;
  const NewName: string = '');
begin
  AddItem(OperationCopy, Source, Folder, NewName);
end;

procedure TShellFileOperation.AddMove(const Source, Folder: string;
  const NewName: string = '');
begin
  AddItem(OperationMove, Source, Folder, NewName);
end;

procedure TShellFileOperation.AddRename(const Source, NewName: string);
begin
  AddItem(OperationRename, Source, '', NewName);
end;

procedure TShellFileOperation.AddDelete(const Source: string);
begin
  AddItem(OperationDelete, Source, '', '');
end;

procedure TShellFileOperation.AddNewFolder(const Folder, Name: string);
begin
  AddItem(OperationNewFolder, '', Folder, Name);
end;

procedure TShellFileOperation.AddNewFile(const Folder, Name: string);
begin
  AddItem(OperationNewFile, '', Folder, Name);
end;

function TShellFileOperation.Execute: Boolean;
var
  Operation: IFileOperation;
  Option: TShellFileOption;
  Item: TOperationItem;
  Source, Folder: IShellItem;
  Flags: DWORD;
  S: WideString;
  NamePtr: PWideChar;
  Any: BOOL;
  I: Integer;
begin
  Result := True;
  FAborted := False;
  if FItems.Count = 0 then
    Exit;
  try
    OleCheck(CoCreateInstance(CLSID_FileOperation, nil, CLSCTX_ALL,
      IFileOperation, Operation));
    Flags := 0;
    for Option := Low(Option) to High(Option) do
      if Option in FOptions then
        Flags := Flags or OptionFlags[Option];
    OleCheck(Operation.SetOperationFlags(Flags));
    Operation.SetOwnerWindow(Windows.GetActiveWindow);
    for I := 0 to FItems.Count - 1 do
    begin
      Item := TOperationItem(FItems[I]);
      S := StrToWide(Item.Name);
      if S = '' then
        NamePtr := nil
      else
        NamePtr := PWideChar(S);
      Source := nil;
      Folder := nil;
      if Item.Source <> '' then
        Source := PathToItem(Item.Source);
      if Item.Folder <> '' then
        Folder := PathToItem(Item.Folder);
      case Item.Kind of
        OperationCopy:
          OleCheck(Operation.CopyItem(Source, Folder, NamePtr, nil));
        OperationMove:
          OleCheck(Operation.MoveItem(Source, Folder, NamePtr, nil));
        OperationRename:
          OleCheck(Operation.RenameItem(Source, NamePtr, nil));
        OperationDelete:
          OleCheck(Operation.DeleteItem(Source, nil));
        OperationNewFolder:
          OleCheck(Operation.NewItem(Folder, FILE_ATTRIBUTE_DIRECTORY, NamePtr,
            nil, nil));
        OperationNewFile:
          OleCheck(Operation.NewItem(Folder, FILE_ATTRIBUTE_NORMAL, NamePtr,
            nil, nil));
      end;
    end;
    { Cancelling or failing is reported by the result rather than raised }
    Result := Operation.PerformOperations = S_OK;
    Any := False;
    if Operation.GetAnyOperationsAborted(Any) = S_OK then
      FAborted := Any;
    Result := Result and not FAborted;
  finally
    Clear;
  end;
end;

function TShellFileOperation.Copy(const Source, Folder: string): Boolean;
begin
  Clear;
  AddCopy(Source, Folder);
  Result := Execute;
end;

function TShellFileOperation.Copy(Source: TStrings; const Folder: string): Boolean;
var
  I: Integer;
begin
  Clear;
  for I := 0 to Source.Count - 1 do
    AddCopy(Source[I], Folder);
  Result := Execute;
end;

function TShellFileOperation.Move(const Source, Folder: string): Boolean;
begin
  Clear;
  AddMove(Source, Folder);
  Result := Execute;
end;

function TShellFileOperation.Move(Source: TStrings; const Folder: string): Boolean;
var
  I: Integer;
begin
  Clear;
  for I := 0 to Source.Count - 1 do
    AddMove(Source[I], Folder);
  Result := Execute;
end;

function TShellFileOperation.Rename(const Source, NewName: string): Boolean;
begin
  Clear;
  AddRename(Source, NewName);
  Result := Execute;
end;

function TShellFileOperation.Delete(const Source: string): Boolean;
begin
  Clear;
  AddDelete(Source);
  Result := Execute;
end;

function TShellFileOperation.Delete(Source: TStrings): Boolean;
var
  I: Integer;
begin
  Clear;
  for I := 0 to Source.Count - 1 do
    AddDelete(Source[I]);
  Result := Execute;
end;

function TShellFileOperation.NewFolder(const Folder, Name: string): Boolean;
begin
  Clear;
  AddNewFolder(Folder, Name);
  Result := Execute;
end;

{ TShellNotify }

const
  ChangeEvents: array[TShellChange] of DWORD = (SHCNE_CREATE, SHCNE_DELETE,
    SHCNE_RENAMEITEM, SHCNE_MKDIR, SHCNE_RMDIR, SHCNE_RENAMEFOLDER,
    SHCNE_ATTRIBUTES, SHCNE_UPDATEDIR, SHCNE_UPDATEITEM, SHCNE_DRIVEADD,
    SHCNE_DRIVEREMOVED, SHCNE_MEDIAINSERTED, SHCNE_MEDIAREMOVED,
    SHCNE_FREESPACE, SHCNE_ASSOCCHANGED);

constructor TShellNotify.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FChanges := AllShellChanges;
  FRecursive := True;
end;

destructor TShellNotify.Destroy;
begin
  Stop;
  inherited Destroy;
end;

procedure TShellNotify.Loaded;
begin
  inherited Loaded;
  Start;
end;

procedure TShellNotify.Start;
var
  Entry: SHChangeNotifyEntry;
  Events: DWORD;
  Change: TShellChange;
begin
  if (not FActive) or (FRegistration <> 0) or
    (ComponentState * [csDesigning, csLoading] <> []) then
    Exit;
  FIDList := ShellPathToIDList(FPath);
  if FIDList = nil then
    Exit;
  FWindow := LCLIntf.AllocateHWnd(WindowProc);
  Events := 0;
  for Change := Low(Change) to High(Change) do
    if Change in FChanges then
      Events := Events or ChangeEvents[Change];
  Entry.pidl := FIDList;
  Entry.fRecursive := FRecursive;
  FRegistration := SHChangeNotifyRegister(FWindow, SHCNRF_InterruptLevel or
    SHCNRF_ShellLevel or SHCNRF_NewDelivery, LONG(Events), WM_SHELLCHANGE,
    1, @Entry);
  if FRegistration = 0 then
    Stop;
end;

procedure TShellNotify.Stop;
begin
  if FRegistration <> 0 then
    SHChangeNotifyDeregister(FRegistration);
  FRegistration := 0;
  if FWindow <> 0 then
    LCLIntf.DeallocateHWnd(FWindow);
  FWindow := 0;
  CoTaskMemFree(FIDList);
  FIDList := nil;
end;

procedure TShellNotify.Restart;
begin
  Stop;
  Start;
end;

procedure TShellNotify.WindowProc(var Message: TLMessage);
var
  Lock: THandle;
  Items: PIDListPair;
  Event: LONG;
  Change, Found: TShellChange;
  ItemPath, NewItemPath: string;
  Known: Boolean;
begin
  if Message.Msg <> WM_SHELLCHANGE then
  begin
    Message.Result := Windows.DefWindowProc(FWindow, Message.Msg,
      Message.WParam, Message.LParam);
    Exit;
  end;
  Items := nil;
  Event := 0;
  Lock := SHChangeNotification_Lock(THandle(Message.WParam),
    DWORD(Message.LParam), Items, Event);
  if Lock = 0 then
    Exit;
  { The paths are read before the event is invoked so the lock is held
    briefly }
  try
    ItemPath := '';
    NewItemPath := '';
    if Items <> nil then
    begin
      if Items[0] <> nil then
        ItemPath := ShellIDListToPath(Items[0]);
      if Items[1] <> nil then
        NewItemPath := ShellIDListToPath(Items[1]);
    end;
  finally
    SHChangeNotification_Unlock(Lock);
  end;
  Known := False;
  Found := scCreate;
  for Change := Low(Change) to High(Change) do
    if (DWORD(Event) and not SHCNE_INTERRUPT) and ChangeEvents[Change] <> 0 then
    begin
      Found := Change;
      Known := True;
      Break;
    end;
  if Known and (Found in FChanges) and Assigned(FOnChange) then
    FOnChange(Self, Found, ItemPath, NewItemPath);
end;

procedure TShellNotify.SetActive(Value: Boolean);
begin
  if Value = FActive then
    Exit;
  FActive := Value;
  if FActive then
    Start
  else
    Stop;
end;

procedure TShellNotify.SetChanges(Value: TShellChanges);
begin
  if Value = FChanges then
    Exit;
  FChanges := Value;
  Restart;
end;

procedure TShellNotify.SetPath(const Value: string);
begin
  if Value = FPath then
    Exit;
  FPath := Value;
  Restart;
end;

procedure TShellNotify.SetRecursive(Value: Boolean);
begin
  if Value = FRecursive then
    Exit;
  FRecursive := Value;
  Restart;
end;

{ TShellLink }

const
  ShowCommands: array[TShellLinkShow] of Integer = (SW_SHOWNORMAL,
    SW_SHOWMAXIMIZED, SW_SHOWMINNOACTIVE);
  ResolveNoUI = $1;
  ResolveUpdate = $4;

function CreateLink: IShellLinkW;
begin
  OleCheck(CoCreateInstance(CLSID_ShellLink, nil, CLSCTX_INPROC_SERVER,
    IShellLinkW, Result));
end;

procedure LoadLink(Link: IShellLinkW; const FileName: string);
var
  W: WideString;
begin
  W := StrToWide(FileName);
  OleCheck((Link as IPersistFile).Load(PWideChar(W), STGM_READ));
end;

procedure TShellLink.Clear;
begin
  FArguments := '';
  FDescription := '';
  FHotKey := 0;
  FIconIndex := 0;
  FIconLocation := '';
  FShow := lsNormal;
  FTarget := '';
  FWorkingDirectory := '';
end;

procedure TShellLink.ReadLink(Link: IShellLinkW);
var
  Buffer: array[0..1023] of WideChar;
  pidl: PItemIDList;
  Command, Index: LongInt;
  Key: Word;
  S: TShellLinkShow;

  function Text: string;
  begin
    Result := WideToStr(WideString(PWideChar(@Buffer[0])));
  end;

begin
  Clear;
  { Targets which are not files, such as Control Panel, have no path }
  FillChar(Buffer, SizeOf(Buffer), #0);
  if Link.GetPath(@Buffer[0], Length(Buffer), nil, 0) = S_OK then
    FTarget := Text;
  if FTarget = '' then
  begin
    pidl := nil;
    if (Link.GetIDList(@pidl) = S_OK) and (pidl <> nil) then
    begin
      FTarget := ShellIDListToPath(pidl);
      CoTaskMemFree(pidl);
    end;
  end;
  FillChar(Buffer, SizeOf(Buffer), #0);
  if Link.GetArguments(@Buffer[0], Length(Buffer)) = S_OK then
    FArguments := Text;
  FillChar(Buffer, SizeOf(Buffer), #0);
  if Link.GetDescription(@Buffer[0], Length(Buffer)) = S_OK then
    FDescription := Text;
  FillChar(Buffer, SizeOf(Buffer), #0);
  if Link.GetWorkingDirectory(@Buffer[0], Length(Buffer)) = S_OK then
    FWorkingDirectory := Text;
  FillChar(Buffer, SizeOf(Buffer), #0);
  Index := 0;
  if Link.GetIconLocation(@Buffer[0], Length(Buffer), @Index) = S_OK then
  begin
    FIconLocation := Text;
    FIconIndex := Index;
  end;
  Key := 0;
  if Link.GetHotkey(@Key) = S_OK then
    FHotKey := Key;
  Command := SW_SHOWNORMAL;
  if Link.GetShowCmd(@Command) = S_OK then
    for S := Low(S) to High(S) do
      if ShowCommands[S] = Command then
        FShow := S;
end;

procedure TShellLink.Load;
var
  Link: IShellLinkW;
begin
  Link := CreateLink;
  LoadLink(Link, FFileName);
  ReadLink(Link);
end;

procedure TShellLink.Load(const FileName: string);
begin
  FFileName := FileName;
  Load;
end;

procedure TShellLink.Save;
var
  Link: IShellLinkW;
  pidl: PItemIDList;
  W: WideString;
begin
  Link := CreateLink;
  { File system targets are stored by path so they can be found again if
    they move, and other targets by item list }
  if (Length(FTarget) > 2) and ((FTarget[2] = ':') or
    ((FTarget[1] = '\') and (FTarget[2] = '\'))) then
  begin
    W := StrToWide(FTarget);
    OleCheck(Link.SetPath(PWideChar(W)));
  end
  else
  begin
    pidl := ShellPathToIDList(FTarget);
    if pidl = nil then
      raise Exception.CreateFmt('"%s" was not found', [FTarget]);
    try
      OleCheck(Link.SetIDList(pidl));
    finally
      CoTaskMemFree(pidl);
    end;
  end;
  W := StrToWide(FArguments);
  Link.SetArguments(PWideChar(W));
  W := StrToWide(FDescription);
  Link.SetDescription(PWideChar(W));
  W := StrToWide(FWorkingDirectory);
  Link.SetWorkingDirectory(PWideChar(W));
  if FIconLocation <> '' then
  begin
    W := StrToWide(FIconLocation);
    Link.SetIconLocation(PWideChar(W), FIconIndex);
  end;
  Link.SetHotkey(FHotKey);
  Link.SetShowCmd(ShowCommands[FShow]);
  W := StrToWide(FFileName);
  OleCheck((Link as IPersistFile).Save(PWideChar(W), True));
end;

procedure TShellLink.Save(const FileName: string);
begin
  FFileName := FileName;
  Save;
end;

function TShellLink.Resolve(ShowUI: Boolean = False): Boolean;
var
  Link: IShellLinkW;
  Persist: IPersistFile;
  Flags: DWORD;
  W: WideString;
begin
  Link := CreateLink;
  LoadLink(Link, FFileName);
  if ShowUI then
    Flags := ResolveUpdate
  else
    Flags := ResolveUpdate or ResolveNoUI;
  Result := Link.Resolve(Windows.GetActiveWindow, Flags) = S_OK;
  if not Result then
    Exit;
  Persist := Link as IPersistFile;
  if Persist.IsDirty = S_OK then
  begin
    W := StrToWide(FFileName);
    Persist.Save(PWideChar(W), True);
  end;
  ReadLink(Link);
end;
{$endif}

end.
