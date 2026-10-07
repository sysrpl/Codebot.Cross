(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ Codebot.Interop.WebView2 declares the part of the Microsoft Edge WebView2 API
  used by this package. The interfaces are generated from WebView2.h of the
  Microsoft.Web.WebView2 NuGet package, version 1.0.4258.31, keeping the order
  of every method. Interfaces this package does not use appear as Pointer.

  WebView2Loader.dll is loaded when InitWebView2 is called rather than linked
  when a program is built, so a program can start and report that WebView2 is
  not available. The DLL must be placed next to the program. }
unit Codebot.Interop.WebView2;

{$i webkit.inc}

interface

{$ifdef windows}
uses
  Windows, ActiveX,
  Codebot.Core;

const
  { The browser version the interfaces were generated for, which is
    CORE_WEBVIEW_TARGET_PRODUCT_VERSION of WebView2EnvironmentOptions.h }
  WebView2TargetVersion = '154.0.4258.31';

  { COREWEBVIEW2_SCRIPT_DIALOG_KIND }
  COREWEBVIEW2_SCRIPT_DIALOG_KIND_ALERT = 0;
  COREWEBVIEW2_SCRIPT_DIALOG_KIND_CONFIRM = 1;
  COREWEBVIEW2_SCRIPT_DIALOG_KIND_PROMPT = 2;
  COREWEBVIEW2_SCRIPT_DIALOG_KIND_BEFOREUNLOAD = 3;

  { COREWEBVIEW2_DOWNLOAD_STATE }
  COREWEBVIEW2_DOWNLOAD_STATE_IN_PROGRESS = 0;
  COREWEBVIEW2_DOWNLOAD_STATE_INTERRUPTED = 1;
  COREWEBVIEW2_DOWNLOAD_STATE_COMPLETED = 2;

  { COREWEBVIEW2_CONTEXT_MENU_TARGET_KIND }
  COREWEBVIEW2_CONTEXT_MENU_TARGET_KIND_PAGE = 0;
  COREWEBVIEW2_CONTEXT_MENU_TARGET_KIND_IMAGE = 1;
  COREWEBVIEW2_CONTEXT_MENU_TARGET_KIND_SELECTED_TEXT = 2;
  COREWEBVIEW2_CONTEXT_MENU_TARGET_KIND_AUDIO = 3;
  COREWEBVIEW2_CONTEXT_MENU_TARGET_KIND_VIDEO = 4;

  { COREWEBVIEW2_CAPTURE_PREVIEW_IMAGE_FORMAT }
  COREWEBVIEW2_CAPTURE_PREVIEW_IMAGE_FORMAT_PNG = 0;
  COREWEBVIEW2_CAPTURE_PREVIEW_IMAGE_FORMAT_JPEG = 1;

  { COREWEBVIEW2_PRINT_DIALOG_KIND }
  COREWEBVIEW2_PRINT_DIALOG_KIND_BROWSER = 0;
  COREWEBVIEW2_PRINT_DIALOG_KIND_SYSTEM = 1;

  { COREWEBVIEW2_MOVE_FOCUS_REASON }
  COREWEBVIEW2_MOVE_FOCUS_REASON_PROGRAMMATIC = 0;
  COREWEBVIEW2_MOVE_FOCUS_REASON_NEXT = 1;
  COREWEBVIEW2_MOVE_FOCUS_REASON_PREVIOUS = 2;

  { COREWEBVIEW2_WEB_ERROR_STATUS }
  COREWEBVIEW2_WEB_ERROR_STATUS_UNKNOWN = 0;
  COREWEBVIEW2_WEB_ERROR_STATUS_CERTIFICATE_COMMON_NAME_IS_INCORRECT = 1;
  COREWEBVIEW2_WEB_ERROR_STATUS_CERTIFICATE_EXPIRED = 2;
  COREWEBVIEW2_WEB_ERROR_STATUS_CLIENT_CERTIFICATE_CONTAINS_ERRORS = 3;
  COREWEBVIEW2_WEB_ERROR_STATUS_CERTIFICATE_REVOKED = 4;
  COREWEBVIEW2_WEB_ERROR_STATUS_CERTIFICATE_IS_INVALID = 5;
  COREWEBVIEW2_WEB_ERROR_STATUS_SERVER_UNREACHABLE = 6;
  COREWEBVIEW2_WEB_ERROR_STATUS_TIMEOUT = 7;
  COREWEBVIEW2_WEB_ERROR_STATUS_ERROR_HTTP_INVALID_SERVER_RESPONSE = 8;
  COREWEBVIEW2_WEB_ERROR_STATUS_CONNECTION_ABORTED = 9;
  COREWEBVIEW2_WEB_ERROR_STATUS_CONNECTION_RESET = 10;
  COREWEBVIEW2_WEB_ERROR_STATUS_DISCONNECTED = 11;
  COREWEBVIEW2_WEB_ERROR_STATUS_CANNOT_CONNECT = 12;
  COREWEBVIEW2_WEB_ERROR_STATUS_HOST_NAME_NOT_RESOLVED = 13;
  COREWEBVIEW2_WEB_ERROR_STATUS_OPERATION_CANCELED = 14;
  COREWEBVIEW2_WEB_ERROR_STATUS_REDIRECT_FAILED = 15;
  COREWEBVIEW2_WEB_ERROR_STATUS_UNEXPECTED_ERROR = 16;
  COREWEBVIEW2_WEB_ERROR_STATUS_VALID_AUTHENTICATION_CREDENTIALS_REQUIRED = 17;
  COREWEBVIEW2_WEB_ERROR_STATUS_VALID_PROXY_AUTHENTICATION_REQUIRED = 18;

  { COREWEBVIEW2_DOWNLOAD_INTERRUPT_REASON }
  COREWEBVIEW2_DOWNLOAD_INTERRUPT_REASON_NONE = 0;
  COREWEBVIEW2_DOWNLOAD_INTERRUPT_REASON_FILE_FAILED = 1;
  COREWEBVIEW2_DOWNLOAD_INTERRUPT_REASON_FILE_ACCESS_DENIED = 2;
  COREWEBVIEW2_DOWNLOAD_INTERRUPT_REASON_FILE_NO_SPACE = 3;
  COREWEBVIEW2_DOWNLOAD_INTERRUPT_REASON_FILE_NAME_TOO_LONG = 4;
  COREWEBVIEW2_DOWNLOAD_INTERRUPT_REASON_FILE_TOO_LARGE = 5;
  COREWEBVIEW2_DOWNLOAD_INTERRUPT_REASON_FILE_MALICIOUS = 6;
  COREWEBVIEW2_DOWNLOAD_INTERRUPT_REASON_FILE_TRANSIENT_ERROR = 7;
  COREWEBVIEW2_DOWNLOAD_INTERRUPT_REASON_FILE_BLOCKED_BY_POLICY = 8;
  COREWEBVIEW2_DOWNLOAD_INTERRUPT_REASON_FILE_SECURITY_CHECK_FAILED = 9;
  COREWEBVIEW2_DOWNLOAD_INTERRUPT_REASON_FILE_TOO_SHORT = 10;
  COREWEBVIEW2_DOWNLOAD_INTERRUPT_REASON_FILE_HASH_MISMATCH = 11;
  COREWEBVIEW2_DOWNLOAD_INTERRUPT_REASON_NETWORK_FAILED = 12;
  COREWEBVIEW2_DOWNLOAD_INTERRUPT_REASON_NETWORK_TIMEOUT = 13;
  COREWEBVIEW2_DOWNLOAD_INTERRUPT_REASON_NETWORK_DISCONNECTED = 14;
  COREWEBVIEW2_DOWNLOAD_INTERRUPT_REASON_NETWORK_SERVER_DOWN = 15;
  COREWEBVIEW2_DOWNLOAD_INTERRUPT_REASON_NETWORK_INVALID_REQUEST = 16;
  COREWEBVIEW2_DOWNLOAD_INTERRUPT_REASON_SERVER_FAILED = 17;
  COREWEBVIEW2_DOWNLOAD_INTERRUPT_REASON_SERVER_NO_RANGE = 18;
  COREWEBVIEW2_DOWNLOAD_INTERRUPT_REASON_SERVER_BAD_CONTENT = 19;
  COREWEBVIEW2_DOWNLOAD_INTERRUPT_REASON_SERVER_UNAUTHORIZED = 20;
  COREWEBVIEW2_DOWNLOAD_INTERRUPT_REASON_SERVER_CERTIFICATE_PROBLEM = 21;
  COREWEBVIEW2_DOWNLOAD_INTERRUPT_REASON_SERVER_FORBIDDEN = 22;
  COREWEBVIEW2_DOWNLOAD_INTERRUPT_REASON_SERVER_UNEXPECTED_RESPONSE = 23;
  COREWEBVIEW2_DOWNLOAD_INTERRUPT_REASON_SERVER_CONTENT_LENGTH_MISMATCH = 24;
  COREWEBVIEW2_DOWNLOAD_INTERRUPT_REASON_SERVER_CROSS_ORIGIN_REDIRECT = 25;
  COREWEBVIEW2_DOWNLOAD_INTERRUPT_REASON_USER_CANCELED = 26;
  COREWEBVIEW2_DOWNLOAD_INTERRUPT_REASON_USER_SHUTDOWN = 27;
  COREWEBVIEW2_DOWNLOAD_INTERRUPT_REASON_USER_PAUSED = 28;
  COREWEBVIEW2_DOWNLOAD_INTERRUPT_REASON_DOWNLOAD_PROCESS_CRASHED = 29;

type
  EventRegistrationToken = record
    Value: Int64;
  end;
  PEventRegistrationToken = ^EventRegistrationToken;

  ICoreWebView2EnvironmentOptions = interface;
  ICoreWebView2Environment = interface;
  ICoreWebView2Controller = interface;
  ICoreWebView2Settings = interface;
  ICoreWebView2 = interface;
  ICoreWebView2_2 = interface;
  ICoreWebView2_3 = interface;
  ICoreWebView2_4 = interface;
  ICoreWebView2_5 = interface;
  ICoreWebView2_6 = interface;
  ICoreWebView2_7 = interface;
  ICoreWebView2_8 = interface;
  ICoreWebView2_9 = interface;
  ICoreWebView2_10 = interface;
  ICoreWebView2_11 = interface;
  ICoreWebView2_12 = interface;
  ICoreWebView2_13 = interface;
  ICoreWebView2_14 = interface;
  ICoreWebView2_15 = interface;
  ICoreWebView2_16 = interface;
  ICoreWebView2Deferral = interface;
  ICoreWebView2NavigationStartingEventArgs = interface;
  ICoreWebView2NavigationCompletedEventArgs = interface;
  ICoreWebView2ScriptDialogOpeningEventArgs = interface;
  ICoreWebView2NewWindowRequestedEventArgs = interface;
  ICoreWebView2DownloadOperation = interface;
  ICoreWebView2DownloadStartingEventArgs = interface;
  ICoreWebView2ContextMenuTarget = interface;
  ICoreWebView2ContextMenuRequestedEventArgs = interface;
  ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler = interface;
  ICoreWebView2CreateCoreWebView2ControllerCompletedHandler = interface;
  ICoreWebView2NavigationStartingEventHandler = interface;
  ICoreWebView2ContentLoadingEventHandler = interface;
  ICoreWebView2SourceChangedEventHandler = interface;
  ICoreWebView2HistoryChangedEventHandler = interface;
  ICoreWebView2NavigationCompletedEventHandler = interface;
  ICoreWebView2ScriptDialogOpeningEventHandler = interface;
  ICoreWebView2DocumentTitleChangedEventHandler = interface;
  ICoreWebView2NewWindowRequestedEventHandler = interface;
  ICoreWebView2ExecuteScriptCompletedHandler = interface;
  ICoreWebView2CapturePreviewCompletedHandler = interface;
  ICoreWebView2CallDevToolsProtocolMethodCompletedHandler = interface;
  ICoreWebView2DownloadStartingEventHandler = interface;
  ICoreWebView2BytesReceivedChangedEventHandler = interface;
  ICoreWebView2StateChangedEventHandler = interface;
  ICoreWebView2ContextMenuRequestedEventHandler = interface;
  ICoreWebView2StatusBarTextChangedEventHandler = interface;

{ ICoreWebView2EnvironmentOptions }

  ICoreWebView2EnvironmentOptions = interface(IUnknown)
  ['{2FDE08A8-1E9A-4766-8C05-95A9CEB9D1C5}']
    function get_AdditionalBrowserArguments(out value: PWideChar): HRESULT; stdcall;
    function put_AdditionalBrowserArguments(value: PWideChar): HRESULT; stdcall;
    function get_Language(out value: PWideChar): HRESULT; stdcall;
    function put_Language(value: PWideChar): HRESULT; stdcall;
    function get_TargetCompatibleBrowserVersion(out value: PWideChar): HRESULT; stdcall;
    function put_TargetCompatibleBrowserVersion(value: PWideChar): HRESULT; stdcall;
    function get_AllowSingleSignOnUsingOSPrimaryAccount(out allow: LongBool): HRESULT; stdcall;
    function put_AllowSingleSignOnUsingOSPrimaryAccount(allow: LongBool): HRESULT; stdcall;
  end;

{ ICoreWebView2Environment }

  ICoreWebView2Environment = interface(IUnknown)
  ['{B96D755E-0319-4E92-A296-23436F46A1FC}']
    function CreateCoreWebView2Controller(parentWindow: HWND; handler: ICoreWebView2CreateCoreWebView2ControllerCompletedHandler): HRESULT; stdcall;
    function CreateWebResourceResponse(content: IStream; statusCode: LongInt; reasonPhrase: PWideChar; headers: PWideChar; out response: Pointer): HRESULT; stdcall;
    function get_BrowserVersionString(out versionInfo: PWideChar): HRESULT; stdcall;
    function add_NewBrowserVersionAvailable(eventHandler: Pointer; out token: EventRegistrationToken): HRESULT; stdcall;
    function remove_NewBrowserVersionAvailable(token: EventRegistrationToken): HRESULT; stdcall;
  end;

{ ICoreWebView2Controller }

  ICoreWebView2Controller = interface(IUnknown)
  ['{4D00C0D1-9434-4EB6-8078-8697A560334F}']
    function get_IsVisible(out isVisible: LongBool): HRESULT; stdcall;
    function put_IsVisible(isVisible: LongBool): HRESULT; stdcall;
    function get_Bounds(out bounds: TRect): HRESULT; stdcall;
    function put_Bounds(bounds: TRect): HRESULT; stdcall;
    function get_ZoomFactor(out zoomFactor: Double): HRESULT; stdcall;
    function put_ZoomFactor(zoomFactor: Double): HRESULT; stdcall;
    function add_ZoomFactorChanged(eventHandler: Pointer; out token: EventRegistrationToken): HRESULT; stdcall;
    function remove_ZoomFactorChanged(token: EventRegistrationToken): HRESULT; stdcall;
    function SetBoundsAndZoomFactor(bounds: TRect; zoomFactor: Double): HRESULT; stdcall;
    function MoveFocus(reason: LongInt): HRESULT; stdcall;
    function add_MoveFocusRequested(eventHandler: Pointer; out token: EventRegistrationToken): HRESULT; stdcall;
    function remove_MoveFocusRequested(token: EventRegistrationToken): HRESULT; stdcall;
    function add_GotFocus(eventHandler: Pointer; out token: EventRegistrationToken): HRESULT; stdcall;
    function remove_GotFocus(token: EventRegistrationToken): HRESULT; stdcall;
    function add_LostFocus(eventHandler: Pointer; out token: EventRegistrationToken): HRESULT; stdcall;
    function remove_LostFocus(token: EventRegistrationToken): HRESULT; stdcall;
    function add_AcceleratorKeyPressed(eventHandler: Pointer; out token: EventRegistrationToken): HRESULT; stdcall;
    function remove_AcceleratorKeyPressed(token: EventRegistrationToken): HRESULT; stdcall;
    function get_ParentWindow(out parentWindow: HWND): HRESULT; stdcall;
    function put_ParentWindow(parentWindow: HWND): HRESULT; stdcall;
    function NotifyParentWindowPositionChanged: HRESULT; stdcall;
    function Close: HRESULT; stdcall;
    function get_CoreWebView2(out coreWebView2: ICoreWebView2): HRESULT; stdcall;
  end;

{ ICoreWebView2Settings }

  ICoreWebView2Settings = interface(IUnknown)
  ['{E562E4F0-D7FA-43AC-8D71-C05150499F00}']
    function get_IsScriptEnabled(out isScriptEnabled: LongBool): HRESULT; stdcall;
    function put_IsScriptEnabled(isScriptEnabled: LongBool): HRESULT; stdcall;
    function get_IsWebMessageEnabled(out isWebMessageEnabled: LongBool): HRESULT; stdcall;
    function put_IsWebMessageEnabled(isWebMessageEnabled: LongBool): HRESULT; stdcall;
    function get_AreDefaultScriptDialogsEnabled(out areDefaultScriptDialogsEnabled: LongBool): HRESULT; stdcall;
    function put_AreDefaultScriptDialogsEnabled(areDefaultScriptDialogsEnabled: LongBool): HRESULT; stdcall;
    function get_IsStatusBarEnabled(out isStatusBarEnabled: LongBool): HRESULT; stdcall;
    function put_IsStatusBarEnabled(isStatusBarEnabled: LongBool): HRESULT; stdcall;
    function get_AreDevToolsEnabled(out areDevToolsEnabled: LongBool): HRESULT; stdcall;
    function put_AreDevToolsEnabled(areDevToolsEnabled: LongBool): HRESULT; stdcall;
    function get_AreDefaultContextMenusEnabled(out enabled: LongBool): HRESULT; stdcall;
    function put_AreDefaultContextMenusEnabled(enabled: LongBool): HRESULT; stdcall;
    function get_AreHostObjectsAllowed(out allowed: LongBool): HRESULT; stdcall;
    function put_AreHostObjectsAllowed(allowed: LongBool): HRESULT; stdcall;
    function get_IsZoomControlEnabled(out enabled: LongBool): HRESULT; stdcall;
    function put_IsZoomControlEnabled(enabled: LongBool): HRESULT; stdcall;
    function get_IsBuiltInErrorPageEnabled(out enabled: LongBool): HRESULT; stdcall;
    function put_IsBuiltInErrorPageEnabled(enabled: LongBool): HRESULT; stdcall;
  end;

{ ICoreWebView2 }

  ICoreWebView2 = interface(IUnknown)
  ['{76ECEACB-0462-4D94-AC83-423A6793775E}']
    function get_Settings(out settings: ICoreWebView2Settings): HRESULT; stdcall;
    function get_Source(out uri: PWideChar): HRESULT; stdcall;
    function Navigate(uri: PWideChar): HRESULT; stdcall;
    function NavigateToString(htmlContent: PWideChar): HRESULT; stdcall;
    function add_NavigationStarting(eventHandler: ICoreWebView2NavigationStartingEventHandler; out token: EventRegistrationToken): HRESULT; stdcall;
    function remove_NavigationStarting(token: EventRegistrationToken): HRESULT; stdcall;
    function add_ContentLoading(eventHandler: ICoreWebView2ContentLoadingEventHandler; out token: EventRegistrationToken): HRESULT; stdcall;
    function remove_ContentLoading(token: EventRegistrationToken): HRESULT; stdcall;
    function add_SourceChanged(eventHandler: ICoreWebView2SourceChangedEventHandler; out token: EventRegistrationToken): HRESULT; stdcall;
    function remove_SourceChanged(token: EventRegistrationToken): HRESULT; stdcall;
    function add_HistoryChanged(eventHandler: ICoreWebView2HistoryChangedEventHandler; out token: EventRegistrationToken): HRESULT; stdcall;
    function remove_HistoryChanged(token: EventRegistrationToken): HRESULT; stdcall;
    function add_NavigationCompleted(eventHandler: ICoreWebView2NavigationCompletedEventHandler; out token: EventRegistrationToken): HRESULT; stdcall;
    function remove_NavigationCompleted(token: EventRegistrationToken): HRESULT; stdcall;
    function add_FrameNavigationStarting(eventHandler: ICoreWebView2NavigationStartingEventHandler; out token: EventRegistrationToken): HRESULT; stdcall;
    function remove_FrameNavigationStarting(token: EventRegistrationToken): HRESULT; stdcall;
    function add_FrameNavigationCompleted(eventHandler: ICoreWebView2NavigationCompletedEventHandler; out token: EventRegistrationToken): HRESULT; stdcall;
    function remove_FrameNavigationCompleted(token: EventRegistrationToken): HRESULT; stdcall;
    function add_ScriptDialogOpening(eventHandler: ICoreWebView2ScriptDialogOpeningEventHandler; out token: EventRegistrationToken): HRESULT; stdcall;
    function remove_ScriptDialogOpening(token: EventRegistrationToken): HRESULT; stdcall;
    function add_PermissionRequested(eventHandler: Pointer; out token: EventRegistrationToken): HRESULT; stdcall;
    function remove_PermissionRequested(token: EventRegistrationToken): HRESULT; stdcall;
    function add_ProcessFailed(eventHandler: Pointer; out token: EventRegistrationToken): HRESULT; stdcall;
    function remove_ProcessFailed(token: EventRegistrationToken): HRESULT; stdcall;
    function AddScriptToExecuteOnDocumentCreated(javaScript: PWideChar; handler: Pointer): HRESULT; stdcall;
    function RemoveScriptToExecuteOnDocumentCreated(id: PWideChar): HRESULT; stdcall;
    function ExecuteScript(javaScript: PWideChar; handler: ICoreWebView2ExecuteScriptCompletedHandler): HRESULT; stdcall;
    function CapturePreview(imageFormat: LongInt; imageStream: IStream; handler: ICoreWebView2CapturePreviewCompletedHandler): HRESULT; stdcall;
    function Reload: HRESULT; stdcall;
    function PostWebMessageAsJson(webMessageAsJson: PWideChar): HRESULT; stdcall;
    function PostWebMessageAsString(webMessageAsString: PWideChar): HRESULT; stdcall;
    function add_WebMessageReceived(handler: Pointer; out token: EventRegistrationToken): HRESULT; stdcall;
    function remove_WebMessageReceived(token: EventRegistrationToken): HRESULT; stdcall;
    function CallDevToolsProtocolMethod(methodName: PWideChar; parametersAsJson: PWideChar; handler: ICoreWebView2CallDevToolsProtocolMethodCompletedHandler): HRESULT; stdcall;
    function get_BrowserProcessId(out value: LongWord): HRESULT; stdcall;
    function get_CanGoBack(out canGoBack: LongBool): HRESULT; stdcall;
    function get_CanGoForward(out canGoForward: LongBool): HRESULT; stdcall;
    function GoBack: HRESULT; stdcall;
    function GoForward: HRESULT; stdcall;
    function GetDevToolsProtocolEventReceiver(eventName: PWideChar; out receiver: Pointer): HRESULT; stdcall;
    function Stop: HRESULT; stdcall;
    function add_NewWindowRequested(eventHandler: ICoreWebView2NewWindowRequestedEventHandler; out token: EventRegistrationToken): HRESULT; stdcall;
    function remove_NewWindowRequested(token: EventRegistrationToken): HRESULT; stdcall;
    function add_DocumentTitleChanged(eventHandler: ICoreWebView2DocumentTitleChangedEventHandler; out token: EventRegistrationToken): HRESULT; stdcall;
    function remove_DocumentTitleChanged(token: EventRegistrationToken): HRESULT; stdcall;
    function get_DocumentTitle(out title: PWideChar): HRESULT; stdcall;
    function AddHostObjectToScript(name: PWideChar; object_: POleVariant): HRESULT; stdcall;
    function RemoveHostObjectFromScript(name: PWideChar): HRESULT; stdcall;
    function OpenDevToolsWindow: HRESULT; stdcall;
    function add_ContainsFullScreenElementChanged(eventHandler: Pointer; out token: EventRegistrationToken): HRESULT; stdcall;
    function remove_ContainsFullScreenElementChanged(token: EventRegistrationToken): HRESULT; stdcall;
    function get_ContainsFullScreenElement(out containsFullScreenElement: LongBool): HRESULT; stdcall;
    function add_WebResourceRequested(eventHandler: Pointer; out token: EventRegistrationToken): HRESULT; stdcall;
    function remove_WebResourceRequested(token: EventRegistrationToken): HRESULT; stdcall;
    function AddWebResourceRequestedFilter(uri: PWideChar; resourceContext: LongInt): HRESULT; stdcall;
    function RemoveWebResourceRequestedFilter(uri: PWideChar; resourceContext: LongInt): HRESULT; stdcall;
    function add_WindowCloseRequested(eventHandler: Pointer; out token: EventRegistrationToken): HRESULT; stdcall;
    function remove_WindowCloseRequested(token: EventRegistrationToken): HRESULT; stdcall;
  end;

{ ICoreWebView2_2 }

  ICoreWebView2_2 = interface(ICoreWebView2)
  ['{9E8F0CF8-E670-4B5E-B2BC-73E061E3184C}']
    function add_WebResourceResponseReceived(eventHandler: Pointer; out token: EventRegistrationToken): HRESULT; stdcall;
    function remove_WebResourceResponseReceived(token: EventRegistrationToken): HRESULT; stdcall;
    function NavigateWithWebResourceRequest(request: Pointer): HRESULT; stdcall;
    function add_DOMContentLoaded(eventHandler: Pointer; out token: EventRegistrationToken): HRESULT; stdcall;
    function remove_DOMContentLoaded(token: EventRegistrationToken): HRESULT; stdcall;
    function get_CookieManager(out cookieManager: Pointer): HRESULT; stdcall;
    function get_Environment(out environment: ICoreWebView2Environment): HRESULT; stdcall;
  end;

{ ICoreWebView2_3 }

  ICoreWebView2_3 = interface(ICoreWebView2_2)
  ['{A0D6DF20-3B92-416D-AA0C-437A9C727857}']
    function TrySuspend(handler: Pointer): HRESULT; stdcall;
    function Resume: HRESULT; stdcall;
    function get_IsSuspended(out isSuspended: LongBool): HRESULT; stdcall;
    function SetVirtualHostNameToFolderMapping(hostName: PWideChar; folderPath: PWideChar; accessKind: LongInt): HRESULT; stdcall;
    function ClearVirtualHostNameToFolderMapping(hostName: PWideChar): HRESULT; stdcall;
  end;

{ ICoreWebView2_4 }

  ICoreWebView2_4 = interface(ICoreWebView2_3)
  ['{20D02D59-6DF2-42DC-BD06-F98A694B1302}']
    function add_FrameCreated(eventHandler: Pointer; out token: EventRegistrationToken): HRESULT; stdcall;
    function remove_FrameCreated(token: EventRegistrationToken): HRESULT; stdcall;
    function add_DownloadStarting(eventHandler: ICoreWebView2DownloadStartingEventHandler; out token: EventRegistrationToken): HRESULT; stdcall;
    function remove_DownloadStarting(token: EventRegistrationToken): HRESULT; stdcall;
  end;

{ ICoreWebView2_5 }

  ICoreWebView2_5 = interface(ICoreWebView2_4)
  ['{BEDB11B8-D63C-11EB-B8BC-0242AC130003}']
    function add_ClientCertificateRequested(eventHandler: Pointer; out token: EventRegistrationToken): HRESULT; stdcall;
    function remove_ClientCertificateRequested(token: EventRegistrationToken): HRESULT; stdcall;
  end;

{ ICoreWebView2_6 }

  ICoreWebView2_6 = interface(ICoreWebView2_5)
  ['{499AADAC-D92C-4589-8A75-111BFC167795}']
    function OpenTaskManagerWindow: HRESULT; stdcall;
  end;

{ ICoreWebView2_7 }

  ICoreWebView2_7 = interface(ICoreWebView2_6)
  ['{79C24D83-09A3-45AE-9418-487F32A58740}']
    function PrintToPdf(ResultFilePath: PWideChar; printSettings: Pointer; handler: Pointer): HRESULT; stdcall;
  end;

{ ICoreWebView2_8 }

  ICoreWebView2_8 = interface(ICoreWebView2_7)
  ['{E9632730-6E1E-43AB-B7B8-7B2C9E62E094}']
    function add_IsMutedChanged(eventHandler: Pointer; out token: EventRegistrationToken): HRESULT; stdcall;
    function remove_IsMutedChanged(token: EventRegistrationToken): HRESULT; stdcall;
    function get_IsMuted(out value: LongBool): HRESULT; stdcall;
    function put_IsMuted(value: LongBool): HRESULT; stdcall;
    function add_IsDocumentPlayingAudioChanged(eventHandler: Pointer; out token: EventRegistrationToken): HRESULT; stdcall;
    function remove_IsDocumentPlayingAudioChanged(token: EventRegistrationToken): HRESULT; stdcall;
    function get_IsDocumentPlayingAudio(out value: LongBool): HRESULT; stdcall;
  end;

{ ICoreWebView2_9 }

  ICoreWebView2_9 = interface(ICoreWebView2_8)
  ['{4D7B2EAB-9FDC-468D-B998-A9260B5ED651}']
    function add_IsDefaultDownloadDialogOpenChanged(handler: Pointer; out token: EventRegistrationToken): HRESULT; stdcall;
    function remove_IsDefaultDownloadDialogOpenChanged(token: EventRegistrationToken): HRESULT; stdcall;
    function get_IsDefaultDownloadDialogOpen(out value: LongBool): HRESULT; stdcall;
    function OpenDefaultDownloadDialog: HRESULT; stdcall;
    function CloseDefaultDownloadDialog: HRESULT; stdcall;
    function get_DefaultDownloadDialogCornerAlignment(out value: LongInt): HRESULT; stdcall;
    function put_DefaultDownloadDialogCornerAlignment(value: LongInt): HRESULT; stdcall;
    function get_DefaultDownloadDialogMargin(out value: TPoint): HRESULT; stdcall;
    function put_DefaultDownloadDialogMargin(value: TPoint): HRESULT; stdcall;
  end;

{ ICoreWebView2_10 }

  ICoreWebView2_10 = interface(ICoreWebView2_9)
  ['{B1690564-6F5A-4983-8E48-31D1143FECDB}']
    function add_BasicAuthenticationRequested(eventHandler: Pointer; out token: EventRegistrationToken): HRESULT; stdcall;
    function remove_BasicAuthenticationRequested(token: EventRegistrationToken): HRESULT; stdcall;
  end;

{ ICoreWebView2_11 }

  ICoreWebView2_11 = interface(ICoreWebView2_10)
  ['{0BE78E56-C193-4051-B943-23B460C08BDB}']
    function CallDevToolsProtocolMethodForSession(sessionId: PWideChar; methodName: PWideChar; parametersAsJson: PWideChar; handler: ICoreWebView2CallDevToolsProtocolMethodCompletedHandler): HRESULT; stdcall;
    function add_ContextMenuRequested(eventHandler: ICoreWebView2ContextMenuRequestedEventHandler; out token: EventRegistrationToken): HRESULT; stdcall;
    function remove_ContextMenuRequested(token: EventRegistrationToken): HRESULT; stdcall;
  end;

{ ICoreWebView2_12 }

  ICoreWebView2_12 = interface(ICoreWebView2_11)
  ['{35D69927-BCFA-4566-9349-6B3E0D154CAC}']
    function add_StatusBarTextChanged(eventHandler: ICoreWebView2StatusBarTextChangedEventHandler; out token: EventRegistrationToken): HRESULT; stdcall;
    function remove_StatusBarTextChanged(token: EventRegistrationToken): HRESULT; stdcall;
    function get_StatusBarText(out value: PWideChar): HRESULT; stdcall;
  end;

{ ICoreWebView2_13 }

  ICoreWebView2_13 = interface(ICoreWebView2_12)
  ['{F75F09A8-667E-4983-88D6-C8773F315E84}']
    function get_Profile(out value: Pointer): HRESULT; stdcall;
  end;

{ ICoreWebView2_14 }

  ICoreWebView2_14 = interface(ICoreWebView2_13)
  ['{6DAA4F10-4A90-4753-8898-77C5DF534165}']
    function add_ServerCertificateErrorDetected(eventHandler: Pointer; out token: EventRegistrationToken): HRESULT; stdcall;
    function remove_ServerCertificateErrorDetected(token: EventRegistrationToken): HRESULT; stdcall;
    function ClearServerCertificateErrorActions(handler: Pointer): HRESULT; stdcall;
  end;

{ ICoreWebView2_15 }

  ICoreWebView2_15 = interface(ICoreWebView2_14)
  ['{517B2D1D-7DAE-4A66-A4F4-10352FFB9518}']
    function add_FaviconChanged(eventHandler: Pointer; out token: EventRegistrationToken): HRESULT; stdcall;
    function remove_FaviconChanged(token: EventRegistrationToken): HRESULT; stdcall;
    function get_FaviconUri(out value: PWideChar): HRESULT; stdcall;
    function GetFavicon(format: LongInt; completedHandler: Pointer): HRESULT; stdcall;
  end;

{ ICoreWebView2_16 }

  ICoreWebView2_16 = interface(ICoreWebView2_15)
  ['{0EB34DC9-9F91-41E1-8639-95CD5943906B}']
    function Print(printSettings: Pointer; handler: Pointer): HRESULT; stdcall;
    function ShowPrintUI(printDialogKind: LongInt): HRESULT; stdcall;
    function PrintToPdfStream(printSettings: Pointer; handler: Pointer): HRESULT; stdcall;
  end;

{ ICoreWebView2Deferral }

  ICoreWebView2Deferral = interface(IUnknown)
  ['{C10E7F7B-B585-46F0-A623-8BEFBF3E4EE0}']
    function Complete: HRESULT; stdcall;
  end;

{ ICoreWebView2NavigationStartingEventArgs }

  ICoreWebView2NavigationStartingEventArgs = interface(IUnknown)
  ['{5B495469-E119-438A-9B18-7604F25F2E49}']
    function get_Uri(out uri: PWideChar): HRESULT; stdcall;
    function get_IsUserInitiated(out isUserInitiated: LongBool): HRESULT; stdcall;
    function get_IsRedirected(out isRedirected: LongBool): HRESULT; stdcall;
    function get_RequestHeaders(out requestHeaders: Pointer): HRESULT; stdcall;
    function get_Cancel(out cancel: LongBool): HRESULT; stdcall;
    function put_Cancel(cancel: LongBool): HRESULT; stdcall;
    function get_NavigationId(out navigationId: QWord): HRESULT; stdcall;
  end;

{ ICoreWebView2NavigationCompletedEventArgs }

  ICoreWebView2NavigationCompletedEventArgs = interface(IUnknown)
  ['{30D68B7D-20D9-4752-A9CA-EC8448FBB5C1}']
    function get_IsSuccess(out isSuccess: LongBool): HRESULT; stdcall;
    function get_WebErrorStatus(out webErrorStatus: LongInt): HRESULT; stdcall;
    function get_NavigationId(out navigationId: QWord): HRESULT; stdcall;
  end;

{ ICoreWebView2ScriptDialogOpeningEventArgs }

  ICoreWebView2ScriptDialogOpeningEventArgs = interface(IUnknown)
  ['{7390BB70-ABE0-4843-9529-F143B31B03D6}']
    function get_Uri(out uri: PWideChar): HRESULT; stdcall;
    function get_Kind(out kind: LongInt): HRESULT; stdcall;
    function get_Message(out message_: PWideChar): HRESULT; stdcall;
    function Accept: HRESULT; stdcall;
    function get_DefaultText(out defaultText: PWideChar): HRESULT; stdcall;
    function get_ResultText(out resultText: PWideChar): HRESULT; stdcall;
    function put_ResultText(resultText: PWideChar): HRESULT; stdcall;
    function GetDeferral(out deferral: ICoreWebView2Deferral): HRESULT; stdcall;
  end;

{ ICoreWebView2NewWindowRequestedEventArgs }

  ICoreWebView2NewWindowRequestedEventArgs = interface(IUnknown)
  ['{34ACB11C-FC37-4418-9132-F9C21D1EAFB9}']
    function get_Uri(out uri: PWideChar): HRESULT; stdcall;
    function put_NewWindow(newWindow: ICoreWebView2): HRESULT; stdcall;
    function get_NewWindow(out newWindow: ICoreWebView2): HRESULT; stdcall;
    function put_Handled(handled: LongBool): HRESULT; stdcall;
    function get_Handled(out handled: LongBool): HRESULT; stdcall;
    function get_IsUserInitiated(out isUserInitiated: LongBool): HRESULT; stdcall;
    function GetDeferral(out deferral: ICoreWebView2Deferral): HRESULT; stdcall;
    function get_WindowFeatures(out value: Pointer): HRESULT; stdcall;
  end;

{ ICoreWebView2DownloadOperation }

  ICoreWebView2DownloadOperation = interface(IUnknown)
  ['{3D6B6CF2-AFE1-44C7-A995-C65117714336}']
    function add_BytesReceivedChanged(eventHandler: ICoreWebView2BytesReceivedChangedEventHandler; out token: EventRegistrationToken): HRESULT; stdcall;
    function remove_BytesReceivedChanged(token: EventRegistrationToken): HRESULT; stdcall;
    function add_EstimatedEndTimeChanged(eventHandler: Pointer; out token: EventRegistrationToken): HRESULT; stdcall;
    function remove_EstimatedEndTimeChanged(token: EventRegistrationToken): HRESULT; stdcall;
    function add_StateChanged(eventHandler: ICoreWebView2StateChangedEventHandler; out token: EventRegistrationToken): HRESULT; stdcall;
    function remove_StateChanged(token: EventRegistrationToken): HRESULT; stdcall;
    function get_Uri(out uri: PWideChar): HRESULT; stdcall;
    function get_ContentDisposition(out contentDisposition: PWideChar): HRESULT; stdcall;
    function get_MimeType(out mimeType: PWideChar): HRESULT; stdcall;
    function get_TotalBytesToReceive(out totalBytesToReceive: Int64): HRESULT; stdcall;
    function get_BytesReceived(out bytesReceived: Int64): HRESULT; stdcall;
    function get_EstimatedEndTime(out estimatedEndTime: PWideChar): HRESULT; stdcall;
    function get_ResultFilePath(out resultFilePath: PWideChar): HRESULT; stdcall;
    function get_State(out downloadState: LongInt): HRESULT; stdcall;
    function get_InterruptReason(out interruptReason: LongInt): HRESULT; stdcall;
    function Cancel: HRESULT; stdcall;
    function Pause: HRESULT; stdcall;
    function Resume: HRESULT; stdcall;
    function get_CanResume(out canResume: LongBool): HRESULT; stdcall;
  end;

{ ICoreWebView2DownloadStartingEventArgs }

  ICoreWebView2DownloadStartingEventArgs = interface(IUnknown)
  ['{E99BBE21-43E9-4544-A732-282764EAFA60}']
    function get_DownloadOperation(out downloadOperation: ICoreWebView2DownloadOperation): HRESULT; stdcall;
    function get_Cancel(out cancel: LongBool): HRESULT; stdcall;
    function put_Cancel(cancel: LongBool): HRESULT; stdcall;
    function get_ResultFilePath(out resultFilePath: PWideChar): HRESULT; stdcall;
    function put_ResultFilePath(resultFilePath: PWideChar): HRESULT; stdcall;
    function get_Handled(out handled: LongBool): HRESULT; stdcall;
    function put_Handled(handled: LongBool): HRESULT; stdcall;
    function GetDeferral(out deferral: ICoreWebView2Deferral): HRESULT; stdcall;
  end;

{ ICoreWebView2ContextMenuTarget }

  ICoreWebView2ContextMenuTarget = interface(IUnknown)
  ['{B8611D99-EED6-4F3F-902C-A198502AD472}']
    function get_Kind(out value: LongInt): HRESULT; stdcall;
    function get_IsEditable(out value: LongBool): HRESULT; stdcall;
    function get_IsRequestedForMainFrame(out value: LongBool): HRESULT; stdcall;
    function get_PageUri(out value: PWideChar): HRESULT; stdcall;
    function get_FrameUri(out value: PWideChar): HRESULT; stdcall;
    function get_HasLinkUri(out value: LongBool): HRESULT; stdcall;
    function get_LinkUri(out value: PWideChar): HRESULT; stdcall;
    function get_HasLinkText(out value: LongBool): HRESULT; stdcall;
    function get_LinkText(out value: PWideChar): HRESULT; stdcall;
    function get_HasSourceUri(out value: LongBool): HRESULT; stdcall;
    function get_SourceUri(out value: PWideChar): HRESULT; stdcall;
    function get_HasSelection(out value: LongBool): HRESULT; stdcall;
    function get_SelectionText(out value: PWideChar): HRESULT; stdcall;
  end;

{ ICoreWebView2ContextMenuRequestedEventArgs }

  ICoreWebView2ContextMenuRequestedEventArgs = interface(IUnknown)
  ['{A1D309EE-C03F-11EB-8529-0242AC130003}']
    function get_MenuItems(out value: Pointer): HRESULT; stdcall;
    function get_ContextMenuTarget(out value: ICoreWebView2ContextMenuTarget): HRESULT; stdcall;
    function get_Location(out value: TPoint): HRESULT; stdcall;
    function put_SelectedCommandId(value: LongInt): HRESULT; stdcall;
    function get_SelectedCommandId(out value: LongInt): HRESULT; stdcall;
    function put_Handled(value: LongBool): HRESULT; stdcall;
    function get_Handled(out value: LongBool): HRESULT; stdcall;
    function GetDeferral(out deferral: ICoreWebView2Deferral): HRESULT; stdcall;
  end;

{ ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler }

  ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler = interface(IUnknown)
  ['{4E8A3389-C9D8-4BD2-B6B5-124FEE6CC14D}']
    function Invoke(errorCode: HRESULT; result_: ICoreWebView2Environment): HRESULT; stdcall;
  end;

{ ICoreWebView2CreateCoreWebView2ControllerCompletedHandler }

  ICoreWebView2CreateCoreWebView2ControllerCompletedHandler = interface(IUnknown)
  ['{6C4819F3-C9B7-4260-8127-C9F5BDE7F68C}']
    function Invoke(errorCode: HRESULT; result_: ICoreWebView2Controller): HRESULT; stdcall;
  end;

{ ICoreWebView2NavigationStartingEventHandler }

  ICoreWebView2NavigationStartingEventHandler = interface(IUnknown)
  ['{9ADBE429-F36D-432B-9DDC-F8881FBD76E3}']
    function Invoke(sender: ICoreWebView2; args: ICoreWebView2NavigationStartingEventArgs): HRESULT; stdcall;
  end;

{ ICoreWebView2ContentLoadingEventHandler }

  ICoreWebView2ContentLoadingEventHandler = interface(IUnknown)
  ['{364471E7-F2BE-4910-BDBA-D72077D51C4B}']
    function Invoke(sender: ICoreWebView2; args: Pointer): HRESULT; stdcall;
  end;

{ ICoreWebView2SourceChangedEventHandler }

  ICoreWebView2SourceChangedEventHandler = interface(IUnknown)
  ['{3C067F9F-5388-4772-8B48-79F7EF1AB37C}']
    function Invoke(sender: ICoreWebView2; args: Pointer): HRESULT; stdcall;
  end;

{ ICoreWebView2HistoryChangedEventHandler }

  ICoreWebView2HistoryChangedEventHandler = interface(IUnknown)
  ['{C79A420C-EFD9-4058-9295-3E8B4BCAB645}']
    function Invoke(sender: ICoreWebView2; args: IUnknown): HRESULT; stdcall;
  end;

{ ICoreWebView2NavigationCompletedEventHandler }

  ICoreWebView2NavigationCompletedEventHandler = interface(IUnknown)
  ['{D33A35BF-1C49-4F98-93AB-006E0533FE1C}']
    function Invoke(sender: ICoreWebView2; args: ICoreWebView2NavigationCompletedEventArgs): HRESULT; stdcall;
  end;

{ ICoreWebView2ScriptDialogOpeningEventHandler }

  ICoreWebView2ScriptDialogOpeningEventHandler = interface(IUnknown)
  ['{EF381BF9-AFA8-4E37-91C4-8AC48524BDFB}']
    function Invoke(sender: ICoreWebView2; args: ICoreWebView2ScriptDialogOpeningEventArgs): HRESULT; stdcall;
  end;

{ ICoreWebView2DocumentTitleChangedEventHandler }

  ICoreWebView2DocumentTitleChangedEventHandler = interface(IUnknown)
  ['{F5F2B923-953E-4042-9F95-F3A118E1AFD4}']
    function Invoke(sender: ICoreWebView2; args: IUnknown): HRESULT; stdcall;
  end;

{ ICoreWebView2NewWindowRequestedEventHandler }

  ICoreWebView2NewWindowRequestedEventHandler = interface(IUnknown)
  ['{D4C185FE-C81C-4989-97AF-2D3FA7AB5651}']
    function Invoke(sender: ICoreWebView2; args: ICoreWebView2NewWindowRequestedEventArgs): HRESULT; stdcall;
  end;

{ ICoreWebView2ExecuteScriptCompletedHandler }

  ICoreWebView2ExecuteScriptCompletedHandler = interface(IUnknown)
  ['{49511172-CC67-4BCA-9923-137112F4C4CC}']
    function Invoke(errorCode: HRESULT; result_: PWideChar): HRESULT; stdcall;
  end;

{ ICoreWebView2CapturePreviewCompletedHandler }

  ICoreWebView2CapturePreviewCompletedHandler = interface(IUnknown)
  ['{697E05E9-3D8F-45FA-96F4-8FFE1EDEDAF5}']
    function Invoke(errorCode: HRESULT): HRESULT; stdcall;
  end;

{ ICoreWebView2CallDevToolsProtocolMethodCompletedHandler }

  ICoreWebView2CallDevToolsProtocolMethodCompletedHandler = interface(IUnknown)
  ['{5C4889F0-5EF6-4C5A-952C-D8F1B92D0574}']
    function Invoke(errorCode: HRESULT; result_: PWideChar): HRESULT; stdcall;
  end;

{ ICoreWebView2DownloadStartingEventHandler }

  ICoreWebView2DownloadStartingEventHandler = interface(IUnknown)
  ['{EFEDC989-C396-41CA-83F7-07F845A55724}']
    function Invoke(sender: ICoreWebView2; args: ICoreWebView2DownloadStartingEventArgs): HRESULT; stdcall;
  end;

{ ICoreWebView2BytesReceivedChangedEventHandler }

  ICoreWebView2BytesReceivedChangedEventHandler = interface(IUnknown)
  ['{828E8AB6-D94C-4264-9CEF-5217170D6251}']
    function Invoke(sender: ICoreWebView2DownloadOperation; args: IUnknown): HRESULT; stdcall;
  end;

{ ICoreWebView2StateChangedEventHandler }

  ICoreWebView2StateChangedEventHandler = interface(IUnknown)
  ['{81336594-7EDE-4BA9-BF71-ACF0A95B58DD}']
    function Invoke(sender: ICoreWebView2DownloadOperation; args: IUnknown): HRESULT; stdcall;
  end;

{ ICoreWebView2ContextMenuRequestedEventHandler }

  ICoreWebView2ContextMenuRequestedEventHandler = interface(IUnknown)
  ['{04D3FE1D-AB87-42FB-A898-DA241D35B63C}']
    function Invoke(sender: ICoreWebView2; args: ICoreWebView2ContextMenuRequestedEventArgs): HRESULT; stdcall;
  end;

{ ICoreWebView2StatusBarTextChangedEventHandler }

  ICoreWebView2StatusBarTextChangedEventHandler = interface(IUnknown)
  ['{A5E3B0D0-10DF-4156-BFAD-3B43867ACAC6}']
    function Invoke(sender: ICoreWebView2; args: IUnknown): HRESULT; stdcall;
  end;

{ Load WebView2Loader.dll and check that the WebView2 runtime is installed }
function InitWebView2(ThrowExceptions: Boolean = False): Boolean;

{ The version of the installed WebView2 runtime, or an empty string }
function WebView2Version: string;

var
  CreateCoreWebView2EnvironmentWithOptions: function(BrowserExecutableFolder,
    UserDataFolder: PWideChar; EnvironmentOptions: ICoreWebView2EnvironmentOptions;
    EnvironmentCreatedHandler: ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler): HRESULT; stdcall;
  GetAvailableCoreWebView2BrowserVersionString: function(BrowserExecutableFolder: PWideChar;
    out VersionInfo: PWideChar): HRESULT; stdcall;

{ Convert a string allocated by WebView2 and free it }
function TakeString(P: PWideChar): string;
{$endif}

implementation

{$ifdef windows}
const
  LoaderLibrary = 'WebView2Loader.dll';

var
  Loaded: Boolean;
  Initialized: Boolean;
  FailedModuleName: string;
  FailedProcName: string;

function TakeString(P: PWideChar): string;
begin
  if P = nil then
    Exit('');
  Result := UTF8Encode(WideString(P));
  CoTaskMemFree(P);
end;

function WebView2Version: string;
var
  P: PWideChar;
begin
  Result := '';
  if not Assigned(GetAvailableCoreWebView2BrowserVersionString) then
    Exit;
  P := nil;
  if GetAvailableCoreWebView2BrowserVersionString(nil, P) = S_OK then
    Result := TakeString(P);
end;

function InitWebView2(ThrowExceptions: Boolean = False): Boolean;
var
  Module: HModule;

  procedure CheckExceptions;
  begin
    if (not Initialized) and ThrowExceptions then
      LibraryExceptProc(FailedModuleName, FailedProcName);
  end;

  function TryLoad(const ProcName: string; var Proc: Pointer): Boolean;
  begin
    FailedProcName := ProcName;
    Proc := LibraryGetProc(Module, ProcName);
    Result := Proc <> nil;
    if not Result then
      CheckExceptions;
  end;

begin
  ThrowExceptions := ThrowExceptions and Assigned(LibraryExceptProc);
  if Loaded then
  begin
    CheckExceptions;
    Exit(Initialized);
  end;
  Loaded := True;
  Result := False;
  FailedModuleName := LoaderLibrary;
  FailedProcName := '';
  Module := LibraryLoad(LoaderLibrary);
  if Module = ModuleNil then
  begin
    CheckExceptions;
    Exit;
  end;
  if not (TryLoad('CreateCoreWebView2EnvironmentWithOptions', @CreateCoreWebView2EnvironmentWithOptions) and
    TryLoad('GetAvailableCoreWebView2BrowserVersionString', @GetAvailableCoreWebView2BrowserVersionString)) then
    Exit;
  { The loader is present but the runtime it loads might not be installed }
  FailedModuleName := 'Microsoft Edge WebView2 Runtime';
  FailedProcName := '';
  Initialized := WebView2Version <> '';
  CheckExceptions;
  Result := Initialized;
end;
{$endif}

end.