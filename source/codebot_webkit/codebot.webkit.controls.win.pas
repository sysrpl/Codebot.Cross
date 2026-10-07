(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.webkit.controls.win.txt> }
unit Codebot.WebKit.Controls.Win;

{$i webkit.inc}

interface

{$ifdef windows}
uses
  Classes, SysUtils, Controls, LCLType, WSLCLClasses, Win32WSControls;

{ The values passed to IWebBrowserEvents match those of WebKitGTK, which are
  declared in Codebot.Interop.WebKit. They are declared here as well so that
  Codebot.WebKit.Controls works the same on both platforms. }

const
  WEBKIT_LOAD_STARTED = 0;
  WEBKIT_LOAD_REDIRECTED = 1;
  WEBKIT_LOAD_COMMITTED = 2;
  WEBKIT_LOAD_FINISHED = 3;

  WEBKIT_HIT_TEST_RESULT_CONTEXT_DOCUMENT = 1 shl 1;
  WEBKIT_HIT_TEST_RESULT_CONTEXT_LINK = 1 shl 2;
  WEBKIT_HIT_TEST_RESULT_CONTEXT_IMAGE = 1 shl 3;
  WEBKIT_HIT_TEST_RESULT_CONTEXT_MEDIA = 1 shl 4;
  WEBKIT_HIT_TEST_RESULT_CONTEXT_EDITABLE = 1 shl 5;
  WEBKIT_HIT_TEST_RESULT_CONTEXT_SCROLLBAR = 1 shl 6;
  WEBKIT_HIT_TEST_RESULT_CONTEXT_SELECTION = 1 shl 7;

  WEBKIT_SCRIPT_DIALOG_ALERT = 0;
  WEBKIT_SCRIPT_DIALOG_CONFIRM = 1;
  WEBKIT_SCRIPT_DIALOG_PROMPT = 2;

{ IWebBrowserEvents and IWebInspectorEvents are the same as those declared in
  Codebot.WebKit.Controls.Gtk3, and are described there }

type
  IWebBrowserEvents = interface
  ['{6E1C2B9A-4D57-4F0E-9A3B-7C5D21E8F4A6}']
    procedure ViewLoadChange(LoadEvent: Integer);
    procedure ViewError(const Uri: string; ErrorCode: Integer; const ErrorMessage: string;
      var Handled: Boolean);
    procedure ViewProgress(Progress: Integer);
    procedure ViewLocationChange(const Uri: string);
    procedure ViewTitleChange(const Title: string);
    procedure ViewNavigate(const Uri: string; var Allow: Boolean);
    procedure ViewHitTest(Context: LongWord; const Link, Media: string);
    procedure ViewContextMenu(X, Y: Integer; Context: LongWord; const Link, Media: string;
      var Handled: Boolean);
    procedure ViewScriptDialog(Dialog: Integer; const Message: string;
      var Input: string; var Accepted: Boolean);
    procedure ViewDownloadStart(Download: Pointer; const Uri: string;
      var FileName: string; var Allow: Boolean);
    procedure ViewDownloadProgress(Download: Pointer; Progress: Integer);
    procedure ViewDownloadFinish(Download: Pointer; Failed: Boolean;
      const ErrorMessage: string);
    procedure ViewInspectorRequest(var Hosted: Boolean);
  end;

  IWebInspectorEvents = interface
  ['{B27F4C1D-8E36-4A92-B5D0-3F6A9C0E7D18}']
    procedure ViewInspectorClosed;
  end;

{ TWSWebBrowser hosts a Microsoft Edge WebView2 web view in a browser control.
  The web view is created in the background after the window of the control,
  so changes made before it is ready are kept and applied once it is. The
  routines other than CreateHandle do nothing when the control has no web
  view, which is the case at design time or when WebView2 is not available. }

  TWSWebBrowser = class(TWin32WSWinControl)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class procedure ShowHide(const AWinControl: TWinControl); override;
    { Resize the web view to the client area of the control }
    class procedure UpdateBounds(AWinControl: TWinControl);
    { Give the input focus to the web view }
    class procedure FocusView(AWinControl: TWinControl);
    class procedure Load(AWinControl: TWinControl; const Uri: string);
    class procedure LoadHtml(AWinControl: TWinControl; const Html: string);
    class procedure Stop(AWinControl: TWinControl);
    class procedure Reload(AWinControl: TWinControl);
    class function GetLoading(AWinControl: TWinControl): Boolean;
    class procedure ExecuteScript(AWinControl: TWinControl; const Script: string);
    class procedure BackOrForward(AWinControl: TWinControl; Steps: Integer);
    class function BackOrForwardExists(AWinControl: TWinControl; Steps: Integer): Boolean;
    class procedure SetEditable(AWinControl: TWinControl; Value: Boolean);
    class procedure SetZoomFactor(AWinControl: TWinControl; Value: Double);
    class procedure SetZoomTextOnly(AWinControl: TWinControl; Value: Boolean);
    class procedure SetDeveloperTools(AWinControl: TWinControl; Value: Boolean);
    class procedure ShowInspector(AWinControl: TWinControl; Show: Boolean);
    class procedure Print(AWinControl: TWinControl);
    class procedure CaptureToClipboard(AWinControl: TWinControl);
    class function HistoryItem(AWinControl: TWinControl; Steps: Integer;
      out Title, Uri: string): Boolean;
    class procedure Download(AWinControl: TWinControl; const Uri: string);
    class procedure CancelDownload(Download: Pointer);
    class function DownloadFolder: string;
  end;

{ TWSWebInspector hosts the developer tools of a browser control.

  WebView2 can only show its developer tools in a window of their own, so the
  inspector control has a web view of its own which shows the developer tools
  page of the browser. That page is served by the debugging server of WebView2,
  which listens on a local port that only programs on the same computer can
  reach. The server is only started when a program creates an inspector
  control, and when it is not running Open shows the developer tools in a
  window of their own instead. }

  TWSWebInspector = class(TWin32WSWinControl)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class procedure ShowHide(const AWinControl: TWinControl); override;
    { Resize the web view to the client area of the control }
    class procedure UpdateBounds(AWinControl: TWinControl);
    class procedure Open(AWinControl, ABrowser: TWinControl);
    class procedure Close(AWinControl, ABrowser: TWinControl);
  end;

{ InspectorControlCreated is called when an inspector control is created, and
  starts the debugging server with the web views created after it }
procedure InspectorControlCreated;

{ InitWebKit returns true when WebView2Loader.dll can be loaded and the
  WebView2 runtime is installed. It has the name of the WebKitGTK routine so
  that Codebot.WebKit.Controls works the same on both platforms. }
function InitWebKit(ThrowExceptions: Boolean = False): Boolean;
{$endif}

implementation

{$ifdef windows}
uses
  Windows, ActiveX, WinSock2, Graphics, Clipbrd,
  Codebot.Interop.WebView2;

function InitWebKit(ThrowExceptions: Boolean = False): Boolean;
begin
  Result := InitWebView2(ThrowExceptions);
end;

{ String conversion between the UTF-8 strings of the LCL and the UTF-16
  strings of WebView2 }

function ToWide(const S: string): UnicodeString;
begin
  Result := UTF8Decode(S);
end;

{ A javascript string literal holding S }

function ScriptString(const S: string): string;
var
  C: Char;
begin
  Result := '"';
  for C in S do
    case C of
      '"': Result := Result + '\"';
      '\': Result := Result + '\\';
      #10: Result := Result + '\n';
      #13: Result := Result + '\r';
      #0..#9, #11, #12, #14..#31: Result := Result + '\u' + IntToHex(Ord(C), 4);
    else
      Result := Result + C;
    end;
  Result := Result + '"';
end;

type
  TWebViewHost = class;

{ TWebViewHandler receives the events of one web view. WebView2 holds a
  reference to it, and so it can be called after its host is destroyed, in
  which case Host is nil and the event is ignored. }

  TWebViewHandler = class(TInterfacedObject,
    ICoreWebView2CreateCoreWebView2ControllerCompletedHandler,
    ICoreWebView2NavigationStartingEventHandler,
    ICoreWebView2ContentLoadingEventHandler,
    ICoreWebView2SourceChangedEventHandler,
    ICoreWebView2NavigationCompletedEventHandler,
    ICoreWebView2ScriptDialogOpeningEventHandler,
    ICoreWebView2DocumentTitleChangedEventHandler,
    ICoreWebView2NewWindowRequestedEventHandler,
    ICoreWebView2DownloadStartingEventHandler,
    ICoreWebView2ContextMenuRequestedEventHandler,
    ICoreWebView2StatusBarTextChangedEventHandler)
  private
    function ControllerCompleted(ErrorCode: HRESULT; Controller: ICoreWebView2Controller): HRESULT; stdcall;
    function NavigationStarting(Sender: ICoreWebView2; Args: ICoreWebView2NavigationStartingEventArgs): HRESULT; stdcall;
    function ContentLoading(Sender: ICoreWebView2; Args: Pointer): HRESULT; stdcall;
    function SourceChanged(Sender: ICoreWebView2; Args: Pointer): HRESULT; stdcall;
    function NavigationCompleted(Sender: ICoreWebView2; Args: ICoreWebView2NavigationCompletedEventArgs): HRESULT; stdcall;
    function ScriptDialogOpening(Sender: ICoreWebView2; Args: ICoreWebView2ScriptDialogOpeningEventArgs): HRESULT; stdcall;
    function DocumentTitleChanged(Sender: ICoreWebView2; Args: IUnknown): HRESULT; stdcall;
    function NewWindowRequested(Sender: ICoreWebView2; Args: ICoreWebView2NewWindowRequestedEventArgs): HRESULT; stdcall;
    function DownloadStarting(Sender: ICoreWebView2; Args: ICoreWebView2DownloadStartingEventArgs): HRESULT; stdcall;
    function ContextMenuRequested(Sender: ICoreWebView2; Args: ICoreWebView2ContextMenuRequestedEventArgs): HRESULT; stdcall;
    function StatusBarTextChanged(Sender: ICoreWebView2; Args: IUnknown): HRESULT; stdcall;
    function ICoreWebView2CreateCoreWebView2ControllerCompletedHandler.Invoke = ControllerCompleted;
    function ICoreWebView2NavigationStartingEventHandler.Invoke = NavigationStarting;
    function ICoreWebView2ContentLoadingEventHandler.Invoke = ContentLoading;
    function ICoreWebView2SourceChangedEventHandler.Invoke = SourceChanged;
    function ICoreWebView2NavigationCompletedEventHandler.Invoke = NavigationCompleted;
    function ICoreWebView2ScriptDialogOpeningEventHandler.Invoke = ScriptDialogOpening;
    function ICoreWebView2DocumentTitleChangedEventHandler.Invoke = DocumentTitleChanged;
    function ICoreWebView2NewWindowRequestedEventHandler.Invoke = NewWindowRequested;
    function ICoreWebView2DownloadStartingEventHandler.Invoke = DownloadStarting;
    function ICoreWebView2ContextMenuRequestedEventHandler.Invoke = ContextMenuRequested;
    function ICoreWebView2StatusBarTextChangedEventHandler.Invoke = StatusBarTextChanged;
  public
    Host: TWebViewHost;
  end;

{ TWebViewHost holds the web view of one browser control. The state set
  before the web view is ready is kept and applied when it is created. }

  TPendingLoad = (plNone, plUri, plHtml);

  TWebViewHost = class
  private
    FControl: TWinControl;
    FWindow: HWND;
    FHandler: TWebViewHandler;
    FHandlerRef: IUnknown;
    FController: ICoreWebView2Controller;
    FView: ICoreWebView2;
    FSettings: ICoreWebView2Settings;
    FLoading: Boolean;
    FPending: TPendingLoad;
    FPendingText: string;
    FEditable: Boolean;
    FDeveloperTools: Boolean;
    FZoomFactor: Double;
    { The host of the inspector control showing the developer tools of this
      web view, or nil if there is none }
    FInspector: TWebViewHost;
    procedure ApplyEditable;
  public
    { Window is the new window of the control, which the control does not
      hold yet while its handle is being created }
    constructor Create(Control: TWinControl; Window: HWND);
    destructor Destroy; override;
    function GetEvents(out Events: IWebBrowserEvents): Boolean;
    function Ready: Boolean;
    { Ask the environment for a web view }
    procedure Start;
    procedure ViewCreated(Controller: ICoreWebView2Controller);
    procedure UpdateBounds;
    procedure UpdateVisible;
    procedure Load(const Uri: string);
    procedure LoadHtml(const Html: string);
    procedure ExecuteScript(const Script: string);
    procedure SetEditable(Value: Boolean);
    procedure SetZoomFactor(Value: Double);
    procedure SetDeveloperTools(Value: Boolean);
    { Show the developer tools of this web view in its inspector host }
    procedure ConnectInspector;
    function Location: string;
    property View: ICoreWebView2 read FView;
    property Controller: ICoreWebView2Controller read FController;
  end;

{ TEnvironmentHandler receives the WebView2 environment, which every web view
  of the program shares }

  TEnvironmentHandler = class(TInterfacedObject,
    ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler)
  public
    function Invoke(ErrorCode: HRESULT; Environment: ICoreWebView2Environment): HRESULT; stdcall;
  end;

{ TEnvironmentOptions passes extra arguments to the browser process, which
  is used to start the debugging server }

  TEnvironmentOptions = class(TInterfacedObject, ICoreWebView2EnvironmentOptions)
  private
    FArguments: string;
    FLanguage: string;
    FTargetVersion: string;
    FAllowSingleSignOn: LongBool;
  public
    constructor Create(const Arguments: string);
    function get_AdditionalBrowserArguments(out Value: PWideChar): HRESULT; stdcall;
    function put_AdditionalBrowserArguments(Value: PWideChar): HRESULT; stdcall;
    function get_Language(out Value: PWideChar): HRESULT; stdcall;
    function put_Language(Value: PWideChar): HRESULT; stdcall;
    function get_TargetCompatibleBrowserVersion(out Value: PWideChar): HRESULT; stdcall;
    function put_TargetCompatibleBrowserVersion(Value: PWideChar): HRESULT; stdcall;
    function get_AllowSingleSignOnUsingOSPrimaryAccount(out Allow: LongBool): HRESULT; stdcall;
    function put_AllowSingleSignOnUsingOSPrimaryAccount(Allow: LongBool): HRESULT; stdcall;
  end;

{ TTargetHandler receives the DevTools target of a web view, and loads the
  developer tools page for that target in the inspector host }

  TTargetHandler = class(TInterfacedObject, ICoreWebView2CallDevToolsProtocolMethodCompletedHandler)
  private
    FHost: TWebViewHost;
  public
    constructor Create(Host: TWebViewHost);
    function Invoke(ErrorCode: HRESULT; ResultJson: PWideChar): HRESULT; stdcall;
  end;

{ TScriptHandler receives the result of a script, which is not needed }

  TScriptHandler = class(TInterfacedObject, ICoreWebView2ExecuteScriptCompletedHandler)
  public
    function Invoke(ErrorCode: HRESULT; ResultJson: PWideChar): HRESULT; stdcall;
  end;

{ TCaptureHandler places a captured image of a web view on the clipboard }

  TCaptureHandler = class(TInterfacedObject, ICoreWebView2CapturePreviewCompletedHandler)
  private
    FData: TMemoryStream;
    FStream: IStream;
  public
    constructor Create;
    destructor Destroy; override;
    function Invoke(ErrorCode: HRESULT): HRESULT; stdcall;
    property Stream: IStream read FStream;
  end;

{ TDownloadHandler follows one download. The download is identified by its
  operation, which is kept in the Downloads list until the download ends so
  that it can be cancelled. }

  TDownloadHandler = class(TInterfacedObject,
    ICoreWebView2BytesReceivedChangedEventHandler,
    ICoreWebView2StateChangedEventHandler)
  private
    FOperation: ICoreWebView2DownloadOperation;
    FControl: TWinControl;
    function GetEvents(out Events: IWebBrowserEvents): Boolean;
    function BytesReceivedChanged(Sender: ICoreWebView2DownloadOperation; Args: IUnknown): HRESULT; stdcall;
    function StateChanged(Sender: ICoreWebView2DownloadOperation; Args: IUnknown): HRESULT; stdcall;
    function ICoreWebView2BytesReceivedChangedEventHandler.Invoke = BytesReceivedChanged;
    function ICoreWebView2StateChangedEventHandler.Invoke = StateChanged;
  public
    constructor Create(Operation: ICoreWebView2DownloadOperation; Control: TWinControl);
    procedure Finish;
    property Operation: ICoreWebView2DownloadOperation read FOperation;
  end;

var
  Environment: ICoreWebView2Environment;
  EnvironmentRequested: Boolean;
  EnvironmentFailed: Boolean;
  { Every existing host, and the hosts waiting for the environment }
  Hosts: TList;
  WaitingHosts: TList;
  { The handlers of the downloads in progress, and the references which keep
    them alive, at the same indexes }
  Downloads: TList;
  DownloadRefs: TInterfaceList;
  { True once an inspector control is created }
  InspectorsUsed: Boolean;
  { The port of the debugging server, or zero when it is not running }
  DebugPort: Word;

procedure InspectorControlCreated;
begin
  InspectorsUsed := True;
end;

const
  HostProperty = 'CodebotWebViewHost';

function HostOf(AWinControl: TWinControl): TWebViewHost;
begin
  Result := nil;
  if (AWinControl <> nil) and AWinControl.HandleAllocated then
    Result := TWebViewHost(GetProp(AWinControl.Handle, HostProperty));
end;

function ReadyHostOf(AWinControl: TWinControl): TWebViewHost;
begin
  Result := HostOf(AWinControl);
  if (Result <> nil) and not Result.Ready then
    Result := nil;
end;

{ The folder WebView2 keeps its data in, such as its cache and cookies. By
  default it is beside the program, which might not be writable, so a folder
  for the program in the local application data folder is used instead. }

function UserDataFolder: string;
begin
  Result := SysUtils.GetEnvironmentVariable('LOCALAPPDATA');
  if Result = '' then
    Result := GetTempDir;
  Result := IncludeTrailingPathDelimiter(Result) +
    ChangeFileExt(ExtractFileName(ParamStr(0)), '') + PathDelim + 'WebView2';
end;

{ A port on the local computer which is free, or zero if none was found. The
  system picks the port, which is then released so the browser can use it. }

function FreePort: Word;
var
  Data: TWSAData;
  S: TSocket;
  Address: TSockAddrIn;
  Size: Integer;
begin
  Result := 0;
  if WSAStartup(MAKEWORD(2, 2), Data) <> 0 then
    Exit;
  try
    S := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    if S = INVALID_SOCKET then
      Exit;
    try
      FillChar(Address, SizeOf(Address), 0);
      Address.sin_family := AF_INET;
      Address.sin_addr.S_addr := htonl(INADDR_LOOPBACK);
      Address.sin_port := 0;
      Size := SizeOf(Address);
      if (bind(S, @Address, Size) = 0) and (getsockname(S, PSockAddr(@Address)^, Size) = 0) then
        Result := ntohs(Address.sin_port);
    finally
      closesocket(S);
    end;
  finally
    WSACleanup;
  end;
end;

{ The address of the debugging server }

function DebugAddress: string;
begin
  Result := '127.0.0.1:' + IntToStr(DebugPort);
end;

procedure RequestEnvironment;
var
  Folder: UnicodeString;
  Options: ICoreWebView2EnvironmentOptions;
begin
  if EnvironmentRequested then
    Exit;
  EnvironmentRequested := True;
  { WebView2 needs COM in single threaded apartment mode, which is already
    the case when the LCL or ComObj initialized it }
  CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
  Folder := ToWide(UserDataFolder);
  { The debugging server only listens on the local computer. The developer
    tools page it serves is allowed to connect back to it. }
  Options := nil;
  if InspectorsUsed then
  begin
    DebugPort := FreePort;
    if DebugPort <> 0 then
      Options := TEnvironmentOptions.Create(
        '--remote-debugging-port=' + IntToStr(DebugPort) +
        ' --remote-allow-origins=http://' + DebugAddress);
  end;
  if Failed(CreateCoreWebView2EnvironmentWithOptions(nil, PWideChar(Folder), Options,
    TEnvironmentHandler.Create)) then
  begin
    EnvironmentFailed := True;
    DebugPort := 0;
  end;
end;

{ A copy of S which the caller frees with CoTaskMemFree }

function AllocString(const S: string): PWideChar;
var
  W: UnicodeString;
  Size: PtrUInt;
begin
  W := ToWide(S);
  Size := (Length(W) + 1) * SizeOf(WideChar);
  Result := CoTaskMemAlloc(Size);
  if Result <> nil then
    Move(PWideChar(W)^, Result^, Size);
end;

{ TEnvironmentOptions }

constructor TEnvironmentOptions.Create(const Arguments: string);
begin
  inherited Create;
  FArguments := Arguments;
  FTargetVersion := WebView2TargetVersion;
end;

function TEnvironmentOptions.get_AdditionalBrowserArguments(out Value: PWideChar): HRESULT; stdcall;
begin
  Value := AllocString(FArguments);
  Result := S_OK;
end;

function TEnvironmentOptions.put_AdditionalBrowserArguments(Value: PWideChar): HRESULT; stdcall;
begin
  FArguments := UTF8Encode(UnicodeString(Value));
  Result := S_OK;
end;

function TEnvironmentOptions.get_Language(out Value: PWideChar): HRESULT; stdcall;
begin
  Value := AllocString(FLanguage);
  Result := S_OK;
end;

function TEnvironmentOptions.put_Language(Value: PWideChar): HRESULT; stdcall;
begin
  FLanguage := UTF8Encode(UnicodeString(Value));
  Result := S_OK;
end;

function TEnvironmentOptions.get_TargetCompatibleBrowserVersion(out Value: PWideChar): HRESULT; stdcall;
begin
  Value := AllocString(FTargetVersion);
  Result := S_OK;
end;

function TEnvironmentOptions.put_TargetCompatibleBrowserVersion(Value: PWideChar): HRESULT; stdcall;
begin
  FTargetVersion := UTF8Encode(UnicodeString(Value));
  Result := S_OK;
end;

function TEnvironmentOptions.get_AllowSingleSignOnUsingOSPrimaryAccount(out Allow: LongBool): HRESULT; stdcall;
begin
  Allow := FAllowSingleSignOn;
  Result := S_OK;
end;

function TEnvironmentOptions.put_AllowSingleSignOnUsingOSPrimaryAccount(Allow: LongBool): HRESULT; stdcall;
begin
  FAllowSingleSignOn := Allow;
  Result := S_OK;
end;

{ TTargetHandler }

constructor TTargetHandler.Create(Host: TWebViewHost);
begin
  inherited Create;
  FHost := Host;
end;

{ The result is json holding targetInfo, which holds the targetId }

function TTargetHandler.Invoke(ErrorCode: HRESULT; ResultJson: PWideChar): HRESULT; stdcall;
const
  Key = '"targetId":"';
var
  Json, Id: string;
  I: Integer;
begin
  Result := S_OK;
  { The host may have been destroyed while waiting }
  if Failed(ErrorCode) or (ResultJson = nil) or (Hosts.IndexOf(FHost) < 0) or
    (FHost.FInspector = nil) or (DebugPort = 0) then
    Exit;
  Json := UTF8Encode(UnicodeString(ResultJson));
  I := Pos(Key, Json);
  if I = 0 then
    Exit;
  Id := Copy(Json, I + Length(Key), Length(Json));
  I := Pos('"', Id);
  if I = 0 then
    Exit;
  Id := Copy(Id, 1, I - 1);
  { The devtools_app page is the developer tools alone. The inspector page
    also shows a picture of the page, which is meant for remote devices. }
  FHost.FInspector.Load('http://' + DebugAddress + '/devtools/devtools_app.html?ws=' +
    DebugAddress + '/devtools/page/' + Id);
end;

{ TEnvironmentHandler }

function TEnvironmentHandler.Invoke(ErrorCode: HRESULT; Environment: ICoreWebView2Environment): HRESULT; stdcall;
var
  Hosts: TList;
  I: Integer;
begin
  Result := S_OK;
  if Succeeded(ErrorCode) and (Environment <> nil) then
    Codebot.WebKit.Controls.Win.Environment := Environment
  else
    EnvironmentFailed := True;
  { Hosts may be added or removed while starting, so a copy is used }
  Hosts := TList.Create;
  try
    Hosts.Assign(WaitingHosts);
    WaitingHosts.Clear;
    if not EnvironmentFailed then
      for I := 0 to Hosts.Count - 1 do
        TWebViewHost(Hosts[I]).Start;
  finally
    Hosts.Free;
  end;
end;

{ TScriptHandler }

function TScriptHandler.Invoke(ErrorCode: HRESULT; ResultJson: PWideChar): HRESULT; stdcall;
begin
  Result := S_OK;
end;

{ TCaptureHandler }

constructor TCaptureHandler.Create;
begin
  inherited Create;
  FData := TMemoryStream.Create;
  FStream := TStreamAdapter.Create(FData, soReference);
end;

destructor TCaptureHandler.Destroy;
begin
  FStream := nil;
  FData.Free;
  inherited Destroy;
end;

function TCaptureHandler.Invoke(ErrorCode: HRESULT): HRESULT; stdcall;
var
  Image: TPortableNetworkGraphic;
begin
  Result := S_OK;
  if Failed(ErrorCode) or (FData.Size = 0) then
    Exit;
  Image := TPortableNetworkGraphic.Create;
  try
    FData.Position := 0;
    Image.LoadFromStream(FData);
    Clipboard.Assign(Image);
  finally
    Image.Free;
  end;
end;

{ TDownloadHandler }

constructor TDownloadHandler.Create(Operation: ICoreWebView2DownloadOperation; Control: TWinControl);
begin
  inherited Create;
  FOperation := Operation;
  FControl := Control;
end;

{ The control of a download is looked for among the existing hosts, as the
  control may have been destroyed while the download continued }

function TDownloadHandler.GetEvents(out Events: IWebBrowserEvents): Boolean;
var
  I: Integer;
begin
  Events := nil;
  for I := 0 to Hosts.Count - 1 do
    if TWebViewHost(Hosts[I]).FControl = FControl then
      Exit(TWebViewHost(Hosts[I]).GetEvents(Events));
  Result := False;
end;

function DownloadProgress(Operation: ICoreWebView2DownloadOperation): Integer;
var
  Total, Received: Int64;
begin
  Result := 0;
  Total := 0;
  Received := 0;
  Operation.get_TotalBytesToReceive(Total);
  Operation.get_BytesReceived(Received);
  if Total > 0 then
    Result := Round(Received / Total * 100);
  if Result > 100 then
    Result := 100;
end;

function TDownloadHandler.BytesReceivedChanged(Sender: ICoreWebView2DownloadOperation; Args: IUnknown): HRESULT; stdcall;
var
  Events: IWebBrowserEvents;
begin
  Result := S_OK;
  if GetEvents(Events) then
    Events.ViewDownloadProgress(Pointer(FOperation), DownloadProgress(FOperation));
end;

function InterruptMessage(Reason: LongInt): string;
begin
  case Reason of
    COREWEBVIEW2_DOWNLOAD_INTERRUPT_REASON_USER_CANCELED: Result := 'The download was cancelled';
    COREWEBVIEW2_DOWNLOAD_INTERRUPT_REASON_FILE_ACCESS_DENIED: Result := 'Access to the file was denied';
    COREWEBVIEW2_DOWNLOAD_INTERRUPT_REASON_FILE_NO_SPACE: Result := 'There is not enough disk space';
    COREWEBVIEW2_DOWNLOAD_INTERRUPT_REASON_FILE_BLOCKED_BY_POLICY: Result := 'The download was blocked by a policy';
    COREWEBVIEW2_DOWNLOAD_INTERRUPT_REASON_FILE_MALICIOUS: Result := 'The file was found to be malicious';
    COREWEBVIEW2_DOWNLOAD_INTERRUPT_REASON_NETWORK_FAILED,
    COREWEBVIEW2_DOWNLOAD_INTERRUPT_REASON_NETWORK_DISCONNECTED: Result := 'The network connection failed';
    COREWEBVIEW2_DOWNLOAD_INTERRUPT_REASON_NETWORK_TIMEOUT: Result := 'The network connection timed out';
    COREWEBVIEW2_DOWNLOAD_INTERRUPT_REASON_SERVER_FAILED,
    COREWEBVIEW2_DOWNLOAD_INTERRUPT_REASON_SERVER_BAD_CONTENT,
    COREWEBVIEW2_DOWNLOAD_INTERRUPT_REASON_SERVER_UNEXPECTED_RESPONSE: Result := 'The server failed';
  else
    Result := 'The download failed with reason ' + IntToStr(Reason);
  end;
end;

{ As with WebKitGTK, a download which fails is reported as failed and then as
  finished }

function TDownloadHandler.StateChanged(Sender: ICoreWebView2DownloadOperation; Args: IUnknown): HRESULT; stdcall;
var
  Events: IWebBrowserEvents;
  State, Reason: LongInt;
begin
  Result := S_OK;
  State := COREWEBVIEW2_DOWNLOAD_STATE_IN_PROGRESS;
  FOperation.get_State(State);
  if State = COREWEBVIEW2_DOWNLOAD_STATE_IN_PROGRESS then
    Exit;
  if GetEvents(Events) then
  begin
    if State = COREWEBVIEW2_DOWNLOAD_STATE_INTERRUPTED then
    begin
      Reason := 0;
      FOperation.get_InterruptReason(Reason);
      Events.ViewDownloadFinish(Pointer(FOperation), True, InterruptMessage(Reason));
    end;
    Events.ViewDownloadFinish(Pointer(FOperation), False, '');
  end;
  Finish;
end;

{ The download has ended, so it is removed from the lists. Removing the
  reference may free the handler, so it is done last. }

procedure TDownloadHandler.Finish;
var
  I: Integer;
begin
  I := Downloads.IndexOf(Self);
  if I < 0 then
    Exit;
  Downloads.Delete(I);
  DownloadRefs.Delete(I);
end;

{ TWebViewHandler }

function TWebViewHandler.ControllerCompleted(ErrorCode: HRESULT; Controller: ICoreWebView2Controller): HRESULT; stdcall;
begin
  Result := S_OK;
  if Failed(ErrorCode) or (Controller = nil) then
    Exit;
  { The control may have been destroyed while the web view was created }
  if Host = nil then
    Controller.Close
  else
    Host.ViewCreated(Controller);
end;

function TWebViewHandler.NavigationStarting(Sender: ICoreWebView2; Args: ICoreWebView2NavigationStartingEventArgs): HRESULT; stdcall;
var
  Events: IWebBrowserEvents;
  P: PWideChar;
  Redirected: LongBool;
  Allow: Boolean;
begin
  Result := S_OK;
  if (Host = nil) or not Host.GetEvents(Events) then
    Exit;
  P := nil;
  Args.get_Uri(P);
  Allow := True;
  Events.ViewNavigate(TakeString(P), Allow);
  if not Allow then
  begin
    Args.put_Cancel(True);
    Exit;
  end;
  Host.FLoading := True;
  Redirected := False;
  Args.get_IsRedirected(Redirected);
  if Redirected then
    Events.ViewLoadChange(WEBKIT_LOAD_REDIRECTED)
  else
    Events.ViewLoadChange(WEBKIT_LOAD_STARTED);
  { WebView2 does not report the progress of a load, so it advances at each
    stage of the load }
  Events.ViewProgress(10);
end;

function TWebViewHandler.ContentLoading(Sender: ICoreWebView2; Args: Pointer): HRESULT; stdcall;
var
  Events: IWebBrowserEvents;
begin
  Result := S_OK;
  if (Host = nil) or not Host.GetEvents(Events) then
    Exit;
  Events.ViewLoadChange(WEBKIT_LOAD_COMMITTED);
  Events.ViewProgress(50);
end;

function TWebViewHandler.SourceChanged(Sender: ICoreWebView2; Args: Pointer): HRESULT; stdcall;
var
  Events: IWebBrowserEvents;
begin
  Result := S_OK;
  if (Host <> nil) and Host.GetEvents(Events) then
    Events.ViewLocationChange(Host.Location);
end;

function ErrorMessage(Status: LongInt): string;
begin
  case Status of
    COREWEBVIEW2_WEB_ERROR_STATUS_CERTIFICATE_COMMON_NAME_IS_INCORRECT,
    COREWEBVIEW2_WEB_ERROR_STATUS_CERTIFICATE_EXPIRED,
    COREWEBVIEW2_WEB_ERROR_STATUS_CLIENT_CERTIFICATE_CONTAINS_ERRORS,
    COREWEBVIEW2_WEB_ERROR_STATUS_CERTIFICATE_REVOKED,
    COREWEBVIEW2_WEB_ERROR_STATUS_CERTIFICATE_IS_INVALID: Result := 'The certificate of the site is not valid';
    COREWEBVIEW2_WEB_ERROR_STATUS_SERVER_UNREACHABLE: Result := 'The server could not be reached';
    COREWEBVIEW2_WEB_ERROR_STATUS_TIMEOUT: Result := 'The connection timed out';
    COREWEBVIEW2_WEB_ERROR_STATUS_ERROR_HTTP_INVALID_SERVER_RESPONSE: Result := 'The server sent an invalid response';
    COREWEBVIEW2_WEB_ERROR_STATUS_CONNECTION_ABORTED: Result := 'The connection was aborted';
    COREWEBVIEW2_WEB_ERROR_STATUS_CONNECTION_RESET: Result := 'The connection was reset';
    COREWEBVIEW2_WEB_ERROR_STATUS_DISCONNECTED: Result := 'The network connection was lost';
    COREWEBVIEW2_WEB_ERROR_STATUS_CANNOT_CONNECT: Result := 'A connection could not be made';
    COREWEBVIEW2_WEB_ERROR_STATUS_HOST_NAME_NOT_RESOLVED: Result := 'The host name could not be resolved';
    COREWEBVIEW2_WEB_ERROR_STATUS_OPERATION_CANCELED: Result := 'The load was cancelled';
    COREWEBVIEW2_WEB_ERROR_STATUS_REDIRECT_FAILED: Result := 'The redirect failed';
    COREWEBVIEW2_WEB_ERROR_STATUS_VALID_AUTHENTICATION_CREDENTIALS_REQUIRED: Result := 'Authentication is required';
    COREWEBVIEW2_WEB_ERROR_STATUS_VALID_PROXY_AUTHENTICATION_REQUIRED: Result := 'Proxy authentication is required';
  else
    Result := 'The load failed';
  end;
end;

{ As with WebKitGTK, a load which fails is reported as an error and then as
  finished }

function TWebViewHandler.NavigationCompleted(Sender: ICoreWebView2; Args: ICoreWebView2NavigationCompletedEventArgs): HRESULT; stdcall;
var
  Events: IWebBrowserEvents;
  Success: LongBool;
  Status: LongInt;
  Handled: Boolean;
begin
  Result := S_OK;
  if Host = nil then
    Exit;
  Host.FLoading := False;
  if not Host.GetEvents(Events) then
    Exit;
  Success := True;
  Args.get_IsSuccess(Success);
  if not Success then
  begin
    Status := COREWEBVIEW2_WEB_ERROR_STATUS_UNKNOWN;
    Args.get_WebErrorStatus(Status);
    Handled := False;
    Events.ViewError(Host.Location, Status, ErrorMessage(Status), Handled);
  end
  else
    { Edit mode belongs to a document, so it is set again for each one }
    Host.ApplyEditable;
  Events.ViewProgress(100);
  Events.ViewLoadChange(WEBKIT_LOAD_FINISHED);
end;

{ The dialog shown when leaving a page with unsaved changes is accepted,
  which lets the page be left }

function TWebViewHandler.ScriptDialogOpening(Sender: ICoreWebView2; Args: ICoreWebView2ScriptDialogOpeningEventArgs): HRESULT; stdcall;
var
  Events: IWebBrowserEvents;
  Kind: LongInt;
  P: PWideChar;
  Message, Input: string;
  Accepted: Boolean;
  W: UnicodeString;
begin
  Result := S_OK;
  Kind := COREWEBVIEW2_SCRIPT_DIALOG_KIND_ALERT;
  Args.get_Kind(Kind);
  if (Kind = COREWEBVIEW2_SCRIPT_DIALOG_KIND_BEFOREUNLOAD) or
    (Host = nil) or not Host.GetEvents(Events) then
  begin
    Args.Accept;
    Exit;
  end;
  P := nil;
  Args.get_Message(P);
  Message := TakeString(P);
  Input := '';
  if Kind = COREWEBVIEW2_SCRIPT_DIALOG_KIND_PROMPT then
  begin
    P := nil;
    Args.get_DefaultText(P);
    Input := TakeString(P);
  end;
  Accepted := False;
  { The dialog kinds of WebView2 and WebKitGTK have the same values }
  Events.ViewScriptDialog(Kind, Message, Input, Accepted);
  case Kind of
    COREWEBVIEW2_SCRIPT_DIALOG_KIND_ALERT:
      Args.Accept;
    COREWEBVIEW2_SCRIPT_DIALOG_KIND_CONFIRM:
      if Accepted then
        Args.Accept;
    COREWEBVIEW2_SCRIPT_DIALOG_KIND_PROMPT:
      if Accepted then
      begin
        W := ToWide(Input);
        Args.put_ResultText(PWideChar(W));
        Args.Accept;
      end;
  end;
end;

function TWebViewHandler.DocumentTitleChanged(Sender: ICoreWebView2; Args: IUnknown): HRESULT; stdcall;
var
  Events: IWebBrowserEvents;
  P: PWideChar;
begin
  Result := S_OK;
  if (Host = nil) or (Host.View = nil) or not Host.GetEvents(Events) then
    Exit;
  P := nil;
  Host.View.get_DocumentTitle(P);
  Events.ViewTitleChange(TakeString(P));
end;

{ As with WebKitGTK, a navigation which asks for a new window is loaded in the
  web view instead when it is allowed }

function TWebViewHandler.NewWindowRequested(Sender: ICoreWebView2; Args: ICoreWebView2NewWindowRequestedEventArgs): HRESULT; stdcall;
var
  Events: IWebBrowserEvents;
  P: PWideChar;
  Uri: string;
  Allow: Boolean;
begin
  Result := S_OK;
  Args.put_Handled(True);
  if (Host = nil) or not Host.GetEvents(Events) then
    Exit;
  P := nil;
  Args.get_Uri(P);
  Uri := TakeString(P);
  Allow := True;
  Events.ViewNavigate(Uri, Allow);
  if Allow then
    Host.Load(Uri);
end;

{ The file of a download is named by the browser control. The download bar
  of WebView2 is not shown, as the browser control reports downloads itself. }

function TWebViewHandler.DownloadStarting(Sender: ICoreWebView2; Args: ICoreWebView2DownloadStartingEventArgs): HRESULT; stdcall;
var
  Events: IWebBrowserEvents;
  Operation: ICoreWebView2DownloadOperation;
  Handler: TDownloadHandler;
  Token: EventRegistrationToken;
  P: PWideChar;
  Uri, FileName: string;
  Allow: Boolean;
  W: UnicodeString;
begin
  Result := S_OK;
  if (Host = nil) or not Host.GetEvents(Events) then
    Exit;
  Operation := nil;
  if Failed(Args.get_DownloadOperation(Operation)) or (Operation = nil) then
    Exit;
  P := nil;
  Operation.get_Uri(P);
  Uri := TakeString(P);
  P := nil;
  Args.get_ResultFilePath(P);
  FileName := ExtractFileName(TakeString(P));
  Allow := True;
  Events.ViewDownloadStart(Pointer(Operation), Uri, FileName, Allow);
  if (not Allow) or (FileName = '') then
  begin
    Args.put_Cancel(True);
    Exit;
  end;
  W := ToWide(FileName);
  Args.put_ResultFilePath(PWideChar(W));
  Args.put_Handled(True);
  { The lists hold a reference to the handler until the download ends }
  Handler := TDownloadHandler.Create(Operation, Host.FControl);
  Downloads.Add(Handler);
  DownloadRefs.Add(Handler as IUnknown);
  Operation.add_BytesReceivedChanged(Handler, Token);
  Operation.add_StateChanged(Handler, Token);
end;

function TWebViewHandler.ContextMenuRequested(Sender: ICoreWebView2; Args: ICoreWebView2ContextMenuRequestedEventArgs): HRESULT; stdcall;
var
  Events: IWebBrowserEvents;
  Target: ICoreWebView2ContextMenuTarget;
  Location: TPoint;
  Kind: LongInt;
  Flag: LongBool;
  P: PWideChar;
  Context: LongWord;
  Link, Media: string;
  Handled: Boolean;
begin
  Result := S_OK;
  if (Host = nil) or not Host.GetEvents(Events) then
    Exit;
  Context := WEBKIT_HIT_TEST_RESULT_CONTEXT_DOCUMENT;
  Link := '';
  Media := '';
  Target := nil;
  if Succeeded(Args.get_ContextMenuTarget(Target)) and (Target <> nil) then
  begin
    Kind := COREWEBVIEW2_CONTEXT_MENU_TARGET_KIND_PAGE;
    Target.get_Kind(Kind);
    case Kind of
      COREWEBVIEW2_CONTEXT_MENU_TARGET_KIND_IMAGE:
        Context := Context or WEBKIT_HIT_TEST_RESULT_CONTEXT_IMAGE;
      COREWEBVIEW2_CONTEXT_MENU_TARGET_KIND_AUDIO,
      COREWEBVIEW2_CONTEXT_MENU_TARGET_KIND_VIDEO:
        Context := Context or WEBKIT_HIT_TEST_RESULT_CONTEXT_MEDIA;
      COREWEBVIEW2_CONTEXT_MENU_TARGET_KIND_SELECTED_TEXT:
        Context := Context or WEBKIT_HIT_TEST_RESULT_CONTEXT_SELECTION;
    end;
    Flag := False;
    Target.get_IsEditable(Flag);
    if Flag then
      Context := Context or WEBKIT_HIT_TEST_RESULT_CONTEXT_EDITABLE;
    Flag := False;
    Target.get_HasLinkUri(Flag);
    if Flag then
    begin
      P := nil;
      Target.get_LinkUri(P);
      Link := TakeString(P);
      Context := Context or WEBKIT_HIT_TEST_RESULT_CONTEXT_LINK;
    end;
    Flag := False;
    Target.get_HasSourceUri(Flag);
    if Flag then
    begin
      P := nil;
      Target.get_SourceUri(P);
      Media := TakeString(P);
    end;
  end;
  Location.X := -1;
  Location.Y := -1;
  Args.get_Location(Location);
  Handled := False;
  Events.ViewContextMenu(Location.X, Location.Y, Context, Link, Media, Handled);
  if Handled then
    Args.put_Handled(True);
end;

{ The status bar text is the uri of the link under the mouse, and is empty
  when the mouse leaves the link }

function TWebViewHandler.StatusBarTextChanged(Sender: ICoreWebView2; Args: IUnknown): HRESULT; stdcall;
var
  Events: IWebBrowserEvents;
  View: ICoreWebView2_12;
  P: PWideChar;
  Link: string;
begin
  Result := S_OK;
  if (Host = nil) or not Host.GetEvents(Events) then
    Exit;
  if not Supports(Sender, ICoreWebView2_12, View) then
    Exit;
  P := nil;
  View.get_StatusBarText(P);
  Link := TakeString(P);
  if Link <> '' then
    Events.ViewHitTest(WEBKIT_HIT_TEST_RESULT_CONTEXT_DOCUMENT or
      WEBKIT_HIT_TEST_RESULT_CONTEXT_LINK, Link, '')
  else
    Events.ViewHitTest(WEBKIT_HIT_TEST_RESULT_CONTEXT_DOCUMENT, '', '');
end;

{ TWebViewHost }

constructor TWebViewHost.Create(Control: TWinControl; Window: HWND);
begin
  inherited Create;
  FControl := Control;
  FWindow := Window;
  FZoomFactor := 1;
  FHandler := TWebViewHandler.Create;
  FHandler.Host := Self;
  FHandlerRef := FHandler;
  SetProp(FWindow, HostProperty, THandle(Self));
  Hosts.Add(Self);
end;

destructor TWebViewHost.Destroy;
var
  I: Integer;
begin
  { Break the links between a browser and its inspector }
  for I := 0 to Hosts.Count - 1 do
    if TWebViewHost(Hosts[I]).FInspector = Self then
      TWebViewHost(Hosts[I]).FInspector := nil;
  Hosts.Remove(Self);
  WaitingHosts.Remove(Self);
  RemoveProp(FWindow, HostProperty);
  FHandler.Host := nil;
  if FController <> nil then
    FController.Close;
  FSettings := nil;
  FView := nil;
  FController := nil;
  FHandlerRef := nil;
  inherited Destroy;
end;

{ The web view can send events while its control is being destroyed, at
  which point the control might not want them }

function TWebViewHost.GetEvents(out Events: IWebBrowserEvents): Boolean;
begin
  Events := nil;
  Result := (FControl <> nil) and not (csDestroying in FControl.ComponentState) and
    Supports(FControl, IWebBrowserEvents, Events);
end;

function TWebViewHost.Ready: Boolean;
begin
  Result := FView <> nil;
end;

procedure TWebViewHost.Start;
begin
  if Environment <> nil then
    Environment.CreateCoreWebView2Controller(FWindow, FHandler)
  else if not EnvironmentFailed then
  begin
    if WaitingHosts.IndexOf(Self) < 0 then
      WaitingHosts.Add(Self);
    RequestEnvironment;
  end;
end;

procedure TWebViewHost.ViewCreated(Controller: ICoreWebView2Controller);
var
  View4: ICoreWebView2_4;
  View11: ICoreWebView2_11;
  View12: ICoreWebView2_12;
  Token: EventRegistrationToken;
begin
  FController := Controller;
  if Failed(FController.get_CoreWebView2(FView)) or (FView = nil) then
  begin
    FView := nil;
    Exit;
  end;
  if Succeeded(FView.get_Settings(FSettings)) and (FSettings <> nil) then
  begin
    { Script dialogs are shown by the browser control, and the link under the
      mouse is reported by it rather than shown in a status bubble }
    FSettings.put_AreDefaultScriptDialogsEnabled(False);
    FSettings.put_IsStatusBarEnabled(False);
    FSettings.put_AreDevToolsEnabled(FDeveloperTools);
  end;
  FView.add_NavigationStarting(FHandler, Token);
  FView.add_ContentLoading(FHandler, Token);
  FView.add_SourceChanged(FHandler, Token);
  FView.add_NavigationCompleted(FHandler, Token);
  FView.add_ScriptDialogOpening(FHandler, Token);
  FView.add_DocumentTitleChanged(FHandler, Token);
  FView.add_NewWindowRequested(FHandler, Token);
  { Newer events are used when the installed runtime has them }
  if Supports(FView, ICoreWebView2_4, View4) then
    View4.add_DownloadStarting(FHandler, Token);
  if Supports(FView, ICoreWebView2_11, View11) then
    View11.add_ContextMenuRequested(FHandler, Token);
  if Supports(FView, ICoreWebView2_12, View12) then
    View12.add_StatusBarTextChanged(FHandler, Token);
  FController.put_ZoomFactor(FZoomFactor);
  UpdateBounds;
  UpdateVisible;
  case FPending of
    plUri: FView.Navigate(PWideChar(ToWide(FPendingText)));
    plHtml: FView.NavigateToString(PWideChar(ToWide(FPendingText)));
  end;
  FPending := plNone;
  FPendingText := '';
  if FInspector <> nil then
    ConnectInspector;
end;

procedure TWebViewHost.ConnectInspector;
begin
  if (FView = nil) or (FInspector = nil) then
    Exit;
  SetDeveloperTools(True);
  FView.CallDevToolsProtocolMethod('Target.getTargetInfo', '{}', TTargetHandler.Create(Self));
end;

{ The client area of the control is used rather than that of its window. When
  a form is maximized the LCL moves its controls together and resizes their
  windows afterwards, so the window still has its old size at this point. }

procedure TWebViewHost.UpdateBounds;
var
  R: TRect;
begin
  if FController = nil then
    Exit;
  R := FControl.ClientRect;
  FController.put_Bounds(R);
end;

procedure TWebViewHost.UpdateVisible;
begin
  if FController <> nil then
    FController.put_IsVisible(FControl.HandleObjectShouldBeVisible);
end;

procedure TWebViewHost.Load(const Uri: string);
var
  W: UnicodeString;
begin
  if FView = nil then
  begin
    FPending := plUri;
    FPendingText := Uri;
    Exit;
  end;
  W := ToWide(Uri);
  FView.Navigate(PWideChar(W));
end;

procedure TWebViewHost.LoadHtml(const Html: string);
var
  W: UnicodeString;
begin
  if FView = nil then
  begin
    FPending := plHtml;
    FPendingText := Html;
    Exit;
  end;
  W := ToWide(Html);
  FView.NavigateToString(PWideChar(W));
end;

procedure TWebViewHost.ExecuteScript(const Script: string);
var
  W: UnicodeString;
begin
  if FView = nil then
    Exit;
  W := ToWide(Script);
  FView.ExecuteScript(PWideChar(W), TScriptHandler.Create);
end;

{ WebView2 has no edit mode of its own, so the design mode of the document is
  used }

procedure TWebViewHost.ApplyEditable;
begin
  if FEditable then
    ExecuteScript('document.designMode = "on";')
  else
    ExecuteScript('document.designMode = "off";');
end;

procedure TWebViewHost.SetEditable(Value: Boolean);
begin
  FEditable := Value;
  if FView <> nil then
    ApplyEditable;
end;

procedure TWebViewHost.SetZoomFactor(Value: Double);
begin
  FZoomFactor := Value;
  if FController <> nil then
    FController.put_ZoomFactor(Value);
end;

procedure TWebViewHost.SetDeveloperTools(Value: Boolean);
begin
  FDeveloperTools := Value;
  if FSettings <> nil then
    FSettings.put_AreDevToolsEnabled(Value);
end;

{ Html loaded from a string is shown at about:blank on WebKitGTK, which the
  browser control relies on, so a data uri holding the html is reported the
  same way }

function TWebViewHost.Location: string;
var
  P: PWideChar;
begin
  Result := '';
  if FView = nil then
    Exit;
  P := nil;
  FView.get_Source(P);
  Result := TakeString(P);
  if Pos('data:text/html', Result) = 1 then
    Result := 'about:blank';
end;

{ TWSWebBrowser }

class function TWSWebBrowser.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND;
var
  Host: TWebViewHost;
begin
  Result := inherited CreateHandle(AWinControl, AParams);
  if (Result = 0) or (csDesigning in AWinControl.ComponentState) or (not InitWebKit) then
    Exit;
  Host := TWebViewHost.Create(AWinControl, Result);
  Host.Start;
end;

class procedure TWSWebBrowser.DestroyHandle(const AWinControl: TWinControl);
begin
  HostOf(AWinControl).Free;
  inherited DestroyHandle(AWinControl);
end;

class procedure TWSWebBrowser.ShowHide(const AWinControl: TWinControl);
var
  Host: TWebViewHost;
begin
  inherited ShowHide(AWinControl);
  Host := HostOf(AWinControl);
  if Host <> nil then
    Host.UpdateVisible;
end;

class procedure TWSWebBrowser.UpdateBounds(AWinControl: TWinControl);
var
  Host: TWebViewHost;
begin
  Host := HostOf(AWinControl);
  if Host <> nil then
    Host.UpdateBounds;
end;

class procedure TWSWebBrowser.FocusView(AWinControl: TWinControl);
var
  Host: TWebViewHost;
begin
  Host := ReadyHostOf(AWinControl);
  if Host <> nil then
    Host.Controller.MoveFocus(COREWEBVIEW2_MOVE_FOCUS_REASON_PROGRAMMATIC);
end;

class procedure TWSWebBrowser.Load(AWinControl: TWinControl; const Uri: string);
var
  Host: TWebViewHost;
begin
  Host := HostOf(AWinControl);
  if Host <> nil then
    Host.Load(Uri);
end;

class procedure TWSWebBrowser.LoadHtml(AWinControl: TWinControl; const Html: string);
var
  Host: TWebViewHost;
begin
  Host := HostOf(AWinControl);
  if Host <> nil then
    Host.LoadHtml(Html);
end;

class procedure TWSWebBrowser.Stop(AWinControl: TWinControl);
var
  Host: TWebViewHost;
begin
  Host := ReadyHostOf(AWinControl);
  if Host <> nil then
    Host.View.Stop;
end;

class procedure TWSWebBrowser.Reload(AWinControl: TWinControl);
var
  Host: TWebViewHost;
begin
  Host := ReadyHostOf(AWinControl);
  if Host <> nil then
    Host.View.Reload;
end;

class function TWSWebBrowser.GetLoading(AWinControl: TWinControl): Boolean;
var
  Host: TWebViewHost;
begin
  Host := ReadyHostOf(AWinControl);
  Result := (Host <> nil) and Host.FLoading;
end;

class procedure TWSWebBrowser.ExecuteScript(AWinControl: TWinControl; const Script: string);
var
  Host: TWebViewHost;
begin
  Host := ReadyHostOf(AWinControl);
  if Host <> nil then
    Host.ExecuteScript(Script);
end;

{ WebView2 has no history list, so only one step back or forward is possible }

class procedure TWSWebBrowser.BackOrForward(AWinControl: TWinControl; Steps: Integer);
var
  Host: TWebViewHost;
begin
  Host := ReadyHostOf(AWinControl);
  if Host = nil then
    Exit;
  if Steps = -1 then
    Host.View.GoBack
  else if Steps = 1 then
    Host.View.GoForward;
end;

class function TWSWebBrowser.BackOrForwardExists(AWinControl: TWinControl; Steps: Integer): Boolean;
var
  Host: TWebViewHost;
  Value: LongBool;
begin
  Result := False;
  Host := ReadyHostOf(AWinControl);
  if Host = nil then
    Exit;
  Value := False;
  if Steps = -1 then
    Host.View.get_CanGoBack(Value)
  else if Steps = 1 then
    Host.View.get_CanGoForward(Value);
  Result := Value;
end;

class procedure TWSWebBrowser.SetEditable(AWinControl: TWinControl; Value: Boolean);
var
  Host: TWebViewHost;
begin
  Host := HostOf(AWinControl);
  if Host <> nil then
    Host.SetEditable(Value);
end;

class procedure TWSWebBrowser.SetZoomFactor(AWinControl: TWinControl; Value: Double);
var
  Host: TWebViewHost;
begin
  Host := HostOf(AWinControl);
  if Host <> nil then
    Host.SetZoomFactor(Value);
end;

{ WebView2 always zooms the whole page }

class procedure TWSWebBrowser.SetZoomTextOnly(AWinControl: TWinControl; Value: Boolean);
begin
end;

class procedure TWSWebBrowser.SetDeveloperTools(AWinControl: TWinControl; Value: Boolean);
var
  Host: TWebViewHost;
begin
  Host := HostOf(AWinControl);
  if Host <> nil then
    Host.SetDeveloperTools(Value);
end;

{ The developer tools open in a window of their own, which WebView2 provides
  no way to close. This is used when there is no inspector control. }

class procedure TWSWebBrowser.ShowInspector(AWinControl: TWinControl; Show: Boolean);
var
  Host: TWebViewHost;
begin
  Host := ReadyHostOf(AWinControl);
  if (Host = nil) or (not Show) then
    Exit;
  Host.SetDeveloperTools(True);
  Host.View.OpenDevToolsWindow;
end;

{ The print dialog of the browser is used when the runtime has it, and the
  print function of the page otherwise }

class procedure TWSWebBrowser.Print(AWinControl: TWinControl);
var
  Host: TWebViewHost;
  View: ICoreWebView2_16;
begin
  Host := ReadyHostOf(AWinControl);
  if Host = nil then
    Exit;
  if Supports(Host.View, ICoreWebView2_16, View) then
    View.ShowPrintUI(COREWEBVIEW2_PRINT_DIALOG_KIND_BROWSER)
  else
    Host.ExecuteScript('window.print();');
end;

class procedure TWSWebBrowser.CaptureToClipboard(AWinControl: TWinControl);
var
  Host: TWebViewHost;
  Handler: TCaptureHandler;
begin
  Host := ReadyHostOf(AWinControl);
  if Host = nil then
    Exit;
  Handler := TCaptureHandler.Create;
  Host.View.CapturePreview(COREWEBVIEW2_CAPTURE_PREVIEW_IMAGE_FORMAT_PNG,
    Handler.Stream, Handler);
end;

{ WebView2 has no history list to read items from }

class function TWSWebBrowser.HistoryItem(AWinControl: TWinControl; Steps: Integer;
  out Title, Uri: string): Boolean;
begin
  Title := '';
  Uri := '';
  Result := False;
end;

{ WebView2 has no way to download a uri directly, so a link to it marked for
  download is clicked. A uri from another site may be shown rather than
  downloaded, which is decided by the web view. }

class procedure TWSWebBrowser.Download(AWinControl: TWinControl; const Uri: string);
var
  Host: TWebViewHost;
begin
  Host := ReadyHostOf(AWinControl);
  if Host <> nil then
    Host.ExecuteScript(
      'var a = document.createElement("a");' +
      'a.href = ' + ScriptString(Uri) + ';' +
      'a.download = "";' +
      'document.body.appendChild(a);' +
      'a.click();' +
      'a.remove();');
end;

class procedure TWSWebBrowser.CancelDownload(Download: Pointer);
var
  I: Integer;
  Handler: TDownloadHandler;
begin
  if (Download = nil) or (Downloads = nil) then
    Exit;
  for I := 0 to Downloads.Count - 1 do
  begin
    Handler := TDownloadHandler(Downloads[I]);
    if Pointer(Handler.Operation) = Download then
    begin
      Handler.Operation.Cancel;
      Exit;
    end;
  end;
end;

{ The download folder of the user, which is empty if there is none }

function SHGetKnownFolderPath(const FolderId: TGUID; Flags: DWORD; Token: THandle;
  out Path: PWideChar): HRESULT; stdcall; external 'shell32.dll';

class function TWSWebBrowser.DownloadFolder: string;
const
  FOLDERID_Downloads: TGUID = '{374DE290-123F-4565-9164-39C4925E467B}';
var
  P: PWideChar;
begin
  Result := '';
  P := nil;
  if Succeeded(SHGetKnownFolderPath(FOLDERID_Downloads, 0, 0, P)) then
    Result := TakeString(P)
  else if P <> nil then
    CoTaskMemFree(P);
end;

{ TWSWebInspector }

class function TWSWebInspector.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND;
var
  Host: TWebViewHost;
begin
  Result := inherited CreateHandle(AWinControl, AParams);
  if (Result = 0) or (csDesigning in AWinControl.ComponentState) or (not InitWebKit) then
    Exit;
  Host := TWebViewHost.Create(AWinControl, Result);
  Host.Start;
end;

class procedure TWSWebInspector.DestroyHandle(const AWinControl: TWinControl);
begin
  HostOf(AWinControl).Free;
  inherited DestroyHandle(AWinControl);
end;

class procedure TWSWebInspector.ShowHide(const AWinControl: TWinControl);
var
  Host: TWebViewHost;
begin
  inherited ShowHide(AWinControl);
  Host := HostOf(AWinControl);
  if Host <> nil then
    Host.UpdateVisible;
end;

class procedure TWSWebInspector.UpdateBounds(AWinControl: TWinControl);
var
  Host: TWebViewHost;
begin
  Host := HostOf(AWinControl);
  if Host <> nil then
    Host.UpdateBounds;
end;

{ The developer tools are shown in the inspector control when the debugging
  server is running, and in a window of their own otherwise. Either web view
  may still be starting, in which case they are connected once it is ready. }

class procedure TWSWebInspector.Open(AWinControl, ABrowser: TWinControl);
var
  Inspector, Browser: TWebViewHost;
begin
  Inspector := HostOf(AWinControl);
  Browser := HostOf(ABrowser);
  if Browser = nil then
    Exit;
  if (Inspector = nil) or (DebugPort = 0) then
  begin
    TWSWebBrowser.ShowInspector(ABrowser, True);
    Exit;
  end;
  Browser.FInspector := Inspector;
  Browser.ConnectInspector;
end;

class procedure TWSWebInspector.Close(AWinControl, ABrowser: TWinControl);
var
  Inspector, Browser: TWebViewHost;
begin
  Inspector := HostOf(AWinControl);
  Browser := HostOf(ABrowser);
  if (Browser <> nil) and (Browser.FInspector = Inspector) then
    Browser.FInspector := nil;
  if Inspector <> nil then
    Inspector.Load('about:blank');
end;

initialization
  Hosts := TList.Create;
  WaitingHosts := TList.Create;
  Downloads := TList.Create;
  DownloadRefs := TInterfaceList.Create;
finalization
  { Downloads still in progress hold a reference which is released here }
  while Downloads.Count > 0 do
    TDownloadHandler(Downloads[Downloads.Count - 1]).Finish;
  FreeAndNil(DownloadRefs);
  FreeAndNil(Downloads);
  FreeAndNil(WaitingHosts);
  FreeAndNil(Hosts);
  Environment := nil;
{$endif}
end.
