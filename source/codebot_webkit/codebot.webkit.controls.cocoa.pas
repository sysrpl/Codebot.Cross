(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.webkit.controls.cocoa.txt> }
unit Codebot.WebKit.Controls.Cocoa;

{$i webkit.inc}
{$ifdef lclcocoa}
  {$modeswitch objectivec2}
  {$modeswitch cblocks}
{$endif}

interface

{$ifdef lclcocoa}
uses
  Classes, SysUtils, Controls, LCLType, WSLCLClasses, CocoaWSPrivate;

{ The values passed to IWebBrowserEvents match those of WebKitGTK, which are
  declared in Codebot.Interop.WebKit. They are declared here as well so that
  Codebot.WebKit.Controls works the same on every platform. }

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

{ TWSWebBrowser hosts a WKWebView in a browser control. The web view is added
  to the view the LCL creates for the control, and is resized with it. The
  routines other than CreateHandle do nothing when the control has no web
  view, which is the case at design time or before macOS 12. }

  TWSWebBrowser = class(TCocoaWSWinControl)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
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

{ TWSWebInspector hosts the inspector of a browser control.

  WebKit has no public way to show its inspector inside another view, so the
  web view of the inspector is taken from the window or the browser it is
  shown in and placed in the inspector control. It is moved back to the
  control if the user docks or undocks it, and the inspector control is told
  when the user closes it. }

  TWSWebInspector = class(TCocoaWSWinControl)
  published
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class procedure Open(AWinControl, ABrowser: TWinControl);
    class procedure Close(AWinControl, ABrowser: TWinControl);
  end;

{ InitWebKit returns true on macOS 12 Monterey or later. It has the name of the
  WebKitGTK routine so that Codebot.WebKit.Controls works the same on every
  platform. }
function InitWebKit(ThrowExceptions: Boolean = False): Boolean;
{$endif}

implementation

{$ifdef lclcocoa}
uses
  CocoaAll, CocoaPrivate, CocoaWindows,
  Codebot.Interop.WKWebView;

const
  { The name scripts in a page use to post messages to the browser control }
  MessageName = 'codebot';

  { The hit test script reports the element under the mouse, and the element
    a context menu was asked for, in the WEBKIT_HIT_TEST_RESULT values }
  HitTestScript =
    '(function() {' +
    '  var last = "";' +
    '  function post(info) {' +
    '    window.webkit.messageHandlers.' + MessageName + '.postMessage(info);' +
    '  }' +
    '  function read(target, kind) {' +
    '    var context = 2, link = "", media = "", node = target;' +
    '    if (node && node.nodeType !== 1) node = node.parentElement;' +
    '    if (node) {' +
    '      var a = node.closest("a[href]");' +
    '      if (a) { context |= 4; link = a.href; }' +
    '      if (node instanceof HTMLImageElement) { context |= 8; media = node.currentSrc || node.src; }' +
    '      else if (node instanceof HTMLMediaElement) { context |= 16; media = node.currentSrc || node.src; }' +
    '      if (node.isContentEditable || node instanceof HTMLTextAreaElement ||' +
    '        (node instanceof HTMLInputElement &&' +
    '        !/^(button|submit|reset|checkbox|radio|file|image|color|range|hidden)$/.test(node.type)))' +
    '        context |= 32;' +
    '      var s = window.getSelection();' +
    '      if (s && !s.isCollapsed && s.containsNode(node, true)) context |= 128;' +
    '    }' +
    '    return { kind: kind, context: context, link: link, media: media };' +
    '  }' +
    '  document.addEventListener("mouseover", function(e) {' +
    '    var info = read(e.target, "hit");' +
    '    var key = info.context + " " + info.link + " " + info.media;' +
    '    if (key !== last) { last = key; post(info); }' +
    '  }, true);' +
    '  document.addEventListener("mouseout", function(e) {' +
    '    if (e.relatedTarget || last === "2  ") return;' +
    '    last = "2  ";' +
    '    post({ kind: "hit", context: 2, link: "", media: "" });' +
    '  }, true);' +
    '  document.addEventListener("contextmenu", function(e) {' +
    '    post(read(e.target, "menu"));' +
    '  }, true);' +
    '})();';

  { The inspector is checked this often in seconds }
  InspectorInterval = 0.25;

function InitWebKit(ThrowExceptions: Boolean = False): Boolean;
var
  Version: NSOperatingSystemVersion;
begin
  Version.majorVersion := 12;
  Version.minorVersion := 0;
  Version.patchVersion := 0;
  Result := NSProcessInfo.processInfo.isOperatingSystemAtLeastVersion(Version) and
    (NSClassFromString(NSSTR('WKWebView')) <> nil);
  if (not Result) and ThrowExceptions then
    raise Exception.Create('The WebKit framework requires macOS 12 or later');
end;

{ The text of an html page holding S }

function HtmlText(const S: string): string;
var
  C: Char;
begin
  Result := '';
  for C in S do
    case C of
      '&': Result := Result + '&amp;';
      '<': Result := Result + '&lt;';
      '>': Result := Result + '&gt;';
      '"': Result := Result + '&quot;';
    else
      Result := Result + C;
    end;
end;

{ WebKitGTK shows a page in place of a page which failed to load. WebKit on
  macOS leaves the web view as it was, so a similar page is shown instead. }

function ErrorPage(const Uri, Message: string): string;
begin
  Result :=
    '<!DOCTYPE html><html><head><meta charset="utf-8"><title>' + HtmlText(Message) +
    '</title><style>body { font: 14px -apple-system, sans-serif; color: #444; margin: 15% 10%; }' +
    'h1 { font-size: 20px; color: #222; } p { word-break: break-all; }</style></head><body>' +
    '<h1>' + HtmlText(Message) + '</h1><p>' + HtmlText(Uri) + '</p></body></html>';
end;

type
  TWebViewHost = class;

{ TCocoaWebView is the web view of a browser control.

  It reports the view of the control as its content view, which the LCL uses
  to decide which control has the input focus. The LCL then treats the web
  view and the control as one, and is told when the web view takes or loses
  the focus. It reports no callback, so keys go to the web view without the
  LCL acting on them, such as tab moving to the next control. }

  TCocoaWebView = objcclass(WKWebView)
  public
    Host: TWebViewHost;
    function lclContentView: NSView; override;
    function lclGetTarget: TObject; override;
    procedure willOpenMenu_withEvent(menu: NSMenu; event: NSEvent); override;
  end;

{ TCocoaWebViewDelegate receives the events of one web view. It implements the
  methods of WKNavigationDelegate, WKUIDelegate, and WKScriptMessageHandler
  without adopting them, as WebKit only checks whether a delegate responds to
  each method. Adopting a protocol makes FPC 3.2 emit protocol data in a section
  Apple's assembler reports as deprecated. The message names must match the
  protocols exactly, as the compiler no longer checks them. }

  TCocoaWebViewDelegate = objcclass(NSObject)
  public
    Host: TWebViewHost;
    { WKNavigationDelegateProtocol }
    procedure webView_decidePolicyForNavigationAction_decisionHandler(webView: WKWebView;
      navigationAction: WKNavigationAction; decisionHandler: TWKPolicyBlock); message 'webView:decidePolicyForNavigationAction:decisionHandler:';
    procedure webView_decidePolicyForNavigationResponse_decisionHandler(webView: WKWebView;
      navigationResponse: WKNavigationResponse; decisionHandler: TWKPolicyBlock); message 'webView:decidePolicyForNavigationResponse:decisionHandler:';
    procedure webView_didStartProvisionalNavigation(webView: WKWebView;
      navigation: WKNavigation); message 'webView:didStartProvisionalNavigation:';
    procedure webView_didReceiveServerRedirectForProvisionalNavigation(webView: WKWebView;
      navigation: WKNavigation); message 'webView:didReceiveServerRedirectForProvisionalNavigation:';
    procedure webView_didFailProvisionalNavigation_withError(webView: WKWebView;
      navigation: WKNavigation; error: NSError); message 'webView:didFailProvisionalNavigation:withError:';
    procedure webView_didCommitNavigation(webView: WKWebView;
      navigation: WKNavigation); message 'webView:didCommitNavigation:';
    procedure webView_didFinishNavigation(webView: WKWebView;
      navigation: WKNavigation); message 'webView:didFinishNavigation:';
    procedure webView_didFailNavigation_withError(webView: WKWebView;
      navigation: WKNavigation; error: NSError); message 'webView:didFailNavigation:withError:';
    procedure webView_navigationAction_didBecomeDownload(webView: WKWebView;
      navigationAction: WKNavigationAction; download: WKDownload); message 'webView:navigationAction:didBecomeDownload:';
    procedure webView_navigationResponse_didBecomeDownload(webView: WKWebView;
      navigationResponse: WKNavigationResponse; download: WKDownload); message 'webView:navigationResponse:didBecomeDownload:';
    { WKUIDelegateProtocol }
    function webView_createWebViewWithConfiguration_forNavigationAction_windowFeatures(webView: WKWebView;
      configuration: WKWebViewConfiguration; navigationAction: WKNavigationAction;
      windowFeatures: WKWindowFeatures): WKWebView; message 'webView:createWebViewWithConfiguration:forNavigationAction:windowFeatures:';
    procedure webView_runJavaScriptAlertPanelWithMessage_initiatedByFrame_completionHandler(webView: WKWebView;
      message_: NSString; frame: WKFrameInfo; completionHandler: TWKVoidBlock); message 'webView:runJavaScriptAlertPanelWithMessage:initiatedByFrame:completionHandler:';
    procedure webView_runJavaScriptConfirmPanelWithMessage_initiatedByFrame_completionHandler(webView: WKWebView;
      message_: NSString; frame: WKFrameInfo; completionHandler: TWKBoolBlock); message 'webView:runJavaScriptConfirmPanelWithMessage:initiatedByFrame:completionHandler:';
    procedure webView_runJavaScriptTextInputPanelWithPrompt_defaultText_initiatedByFrame_completionHandler(webView: WKWebView;
      prompt: NSString; defaultText: NSString; frame: WKFrameInfo; completionHandler: TWKStringBlock); message 'webView:runJavaScriptTextInputPanelWithPrompt:defaultText:initiatedByFrame:completionHandler:';
    { WKScriptMessageHandlerProtocol }
    procedure userContentController_didReceiveScriptMessage(userContentController: id;
      message_: WKScriptMessage); message 'userContentController:didReceiveScriptMessage:';
    { The title, location, and progress of the web view are observed }
    procedure observeValueForKeyPath_ofObject_change_context(keyPath: NSString;
      object_: id; change: NSDictionary; context: pointer); override;
    { Called by a timer to keep the inspector in its inspector control }
    procedure checkInspector(timer: NSTimer); message 'checkInspector:';
  end;

{ TCocoaDownloadDelegate follows every download. A download continues after
  the browser control which started it is destroyed, in which case it has
  nothing to notify. It implements the methods of WKDownloadDelegate without
  adopting it, for the same reason as TCocoaWebViewDelegate. }

  TCocoaDownloadDelegate = objcclass(NSObject)
  public
    procedure download_decideDestinationUsingResponse_suggestedFilename_completionHandler(download: WKDownload;
      response: NSURLResponse; suggestedFilename: NSString; completionHandler: TWKURLBlock); message 'download:decideDestinationUsingResponse:suggestedFilename:completionHandler:';
    procedure downloadDidFinish(download: WKDownload); message 'downloadDidFinish:';
    procedure download_didFailWithError_resumeData(download: WKDownload; error: NSError;
      resumeData: NSData); message 'download:didFailWithError:resumeData:';
    { The progress of a download is observed }
    procedure observeValueForKeyPath_ofObject_change_context(keyPath: NSString;
      object_: id; change: NSDictionary; context: pointer); override;
    procedure reportProgress(download: WKDownload); message 'reportProgress:';
  end;

{ TWebViewHost holds the web view of one browser control and the state of the
  control which the web view has no property for }

  TWebViewHost = class
  private
    FControl: TWinControl;
    FView: TCocoaWebView;
    FDelegate: TCocoaWebViewDelegate;
    FTimer: NSTimer;
    FEditable: Boolean;
    FZoomFactor: Double;
    FZoomTextOnly: Boolean;
    { The element a context menu was last asked for }
    FMenuContext: LongWord;
    FMenuLink: string;
    FMenuMedia: string;
    { True while an error page is loaded for FErrorUri, during which the load
      is not reported. Alternate html has no navigation, and html loaded as a
      string has FErrorNavigation. }
    FErrorLoading: Boolean;
    FErrorUri: string;
    FErrorNavigation: WKNavigation;
    { The inspector control showing the inspector of this web view, the view
      of that control the inspector is placed in, and the view of the
      inspector }
    FInspectorControl: TWinControl;
    FInspectorHost: NSView;
    FInspectorItem: NSView;
    { True once a hosted inspector has been seen, after which it not being
      visible means the user closed it }
    FInspectorShown: Boolean;
    { True once the browser control was asked to host the inspector }
    FInspectorRequested: Boolean;
    function Inspector: id;
    function InspectorView: NSView;
    procedure HostInspector;
    procedure RemoveInspectorItem;
    procedure ReleaseInspector;
    procedure ShowError(const Uri, Message: string);
    procedure EndError;
    function IsErrorNavigation(Navigation: WKNavigation): Boolean;
  public
    constructor Create(Control: TWinControl; Parent: NSView);
    destructor Destroy; override;
    function GetEvents(out Events: IWebBrowserEvents): Boolean;
    procedure Load(const Uri: string);
    procedure LoadHtml(const Html: string);
    procedure ExecuteScript(const Script: string);
    procedure ApplyEditable;
    procedure ApplyZoom;
    procedure SetDeveloperTools(Value: Boolean);
    procedure LoadFailed(Navigation: WKNavigation; Error: NSError);
    procedure Denied(const Uri: string);
    procedure CheckInspector;
    procedure OpenInspector(Control: TWinControl; Host: NSView);
    procedure CloseInspector;
    property View: TCocoaWebView read FView;
  end;

var
  { Every existing host }
  Hosts: TList;
  { The downloads in progress, which are retained until they end }
  Downloads: NSMutableArray;
  DownloadDelegate: TCocoaDownloadDelegate;

function HostOf(AWinControl: TWinControl): TWebViewHost;
var
  I: Integer;
begin
  if Hosts <> nil then
    for I := 0 to Hosts.Count - 1 do
    begin
      Result := TWebViewHost(Hosts[I]);
      if Result.FControl = AWinControl then
        Exit;
    end;
  Result := nil;
end;

function HostOfView(View: WKWebView): TWebViewHost;
var
  I: Integer;
begin
  if (Hosts <> nil) and (View <> nil) then
    for I := 0 to Hosts.Count - 1 do
    begin
      Result := TWebViewHost(Hosts[I]);
      if Result.FView = View then
        Exit;
    end;
  Result := nil;
end;

{ The view of a control which other views are placed in }

function ContentView(AWinControl: TWinControl): NSView;
begin
  Result := nil;
  if AWinControl.HandleAllocated then
    Result := NSObject(AWinControl.Handle).lclContentView;
end;

function UrlText(Url: NSURL): string;
begin
  if Url = nil then
    Result := ''
  else
    Result := FromNSString(Url.absoluteString);
end;

{ Downloads }

function DownloadEvents(Download: WKDownload; out Events: IWebBrowserEvents): Boolean;
var
  Host: TWebViewHost;
begin
  Events := nil;
  Host := HostOfView(Download.webView);
  Result := (Host <> nil) and Host.GetEvents(Events);
end;

{ A download is followed from when it starts until it ends }

procedure DownloadBegin(Download: WKDownload);
begin
  if (Download = nil) or Downloads.containsObject(Download) then
    Exit;
  Downloads.addObject(Download);
  Download.setDelegate(DownloadDelegate);
  Download.progress.addObserver_forKeyPath_options_context(DownloadDelegate,
    NSSTR('fractionCompleted'), 0, Download);
end;

procedure DownloadEnd(Download: WKDownload);
begin
  if not Downloads.containsObject(Download) then
    Exit;
  Download.progress.removeObserver_forKeyPath(DownloadDelegate, NSSTR('fractionCompleted'));
  Download.setDelegate(nil);
  Downloads.removeObject(Download);
end;

procedure DownloadStarted(Download: id); cdecl;
begin
  DownloadBegin(WKDownload(Download));
end;

{ TCocoaDownloadDelegate }

procedure TCocoaDownloadDelegate.download_decideDestinationUsingResponse_suggestedFilename_completionHandler(
  download: WKDownload; response: NSURLResponse; suggestedFilename: NSString;
  completionHandler: TWKURLBlock);
var
  Events: IWebBrowserEvents;
  FileName: string;
  Allow: Boolean;
begin
  if not DownloadEvents(download, Events) then
  begin
    completionHandler(nil);
    Exit;
  end;
  FileName := FromNSString(suggestedFilename);
  Allow := True;
  Events.ViewDownloadStart(download, UrlText(download.originalRequest.URL), FileName, Allow);
  if Allow and (FileName <> '') then
    completionHandler(NSURL.fileURLWithPath(ToNSString(FileName)))
  else
    completionHandler(nil);
end;

procedure TCocoaDownloadDelegate.downloadDidFinish(download: WKDownload);
var
  Events: IWebBrowserEvents;
begin
  if DownloadEvents(download, Events) then
    Events.ViewDownloadFinish(download, False, '');
  DownloadEnd(download);
end;

{ As with WebKitGTK, a download which fails is reported as failed and then as
  finished }

procedure TCocoaDownloadDelegate.download_didFailWithError_resumeData(download: WKDownload;
  error: NSError; resumeData: NSData);
var
  Events: IWebBrowserEvents;
  Message: string;
begin
  if DownloadEvents(download, Events) then
  begin
    if error.code = NSURLErrorCancelled then
      Message := 'The download was cancelled'
    else
      Message := FromNSString(error.localizedDescription);
    Events.ViewDownloadFinish(download, True, Message);
    Events.ViewDownloadFinish(download, False, '');
  end;
  DownloadEnd(download);
end;

{ The progress of a download may change on another thread }

procedure TCocoaDownloadDelegate.observeValueForKeyPath_ofObject_change_context(
  keyPath: NSString; object_: id; change: NSDictionary; context: pointer);
begin
  if NSThread.classIsMainThread then
    reportProgress(WKDownload(context))
  else
    performSelectorOnMainThread_withObject_waitUntilDone(objcselector('reportProgress:'),
      WKDownload(context), False);
end;

procedure TCocoaDownloadDelegate.reportProgress(download: WKDownload);
var
  Events: IWebBrowserEvents;
  Progress: Integer;
begin
  if not Downloads.containsObject(download) then
    Exit;
  if not DownloadEvents(download, Events) then
    Exit;
  Progress := Round(download.progress.fractionCompleted * 100);
  if Progress < 0 then
    Progress := 0
  else if Progress > 100 then
    Progress := 100;
  Events.ViewDownloadProgress(download, Progress);
end;

{ TCocoaWebView }

function TCocoaWebView.lclContentView: NSView;
begin
  if superview <> nil then
    Result := superview
  else
    Result := Self;
end;

function TCocoaWebView.lclGetTarget: TObject;
begin
  if superview <> nil then
    Result := superview.lclGetTarget
  else
    Result := nil;
end;

{ The context menu is reported for the element the page last reported a
  context menu for. X and Y are -1 when the menu was opened with the keyboard,
  as with WebKitGTK. }

procedure TCocoaWebView.willOpenMenu_withEvent(menu: NSMenu; event: NSEvent);
var
  Events: IWebBrowserEvents;
  Point: NSPoint;
  X, Y: Integer;
  Handled: Boolean;
begin
  inherited willOpenMenu_withEvent(menu, event);
  if (Host = nil) or not Host.GetEvents(Events) then
    Exit;
  X := -1;
  Y := -1;
  if event <> nil then
    case event.type_ of
      NSLeftMouseDown, NSLeftMouseUp, NSRightMouseDown, NSRightMouseUp,
      NSOtherMouseDown, NSOtherMouseUp:
        begin
          Point := convertPoint_fromView(event.locationInWindow, nil);
          X := Round(Point.x);
          if isFlipped then
            Y := Round(Point.y)
          else
            Y := Round(bounds.size.height - Point.y);
        end;
    end;
  Handled := False;
  Events.ViewContextMenu(X, Y, Host.FMenuContext, Host.FMenuLink, Host.FMenuMedia, Handled);
  if Handled then
    menu.removeAllItems;
end;

{ TCocoaWebViewDelegate }

{ A navigation which asks for a new window, such as a link with a target of
  _blank, would do nothing as the control only has the one web view. As with
  WebKitGTK, when it is allowed it is loaded in the web view instead. }

procedure TCocoaWebViewDelegate.webView_decidePolicyForNavigationAction_decisionHandler(
  webView: WKWebView; navigationAction: WKNavigationAction; decisionHandler: TWKPolicyBlock);
var
  Events: IWebBrowserEvents;
  Uri: string;
  Allow: Boolean;
begin
  if navigationAction.shouldPerformDownload then
  begin
    decisionHandler(WKNavigationActionPolicyDownload);
    Exit;
  end;
  if (Host <> nil) and Host.FErrorLoading and
    (UrlText(navigationAction.request.URL) = Host.FErrorUri) then
  begin
    decisionHandler(WKNavigationActionPolicyAllow);
    Exit;
  end;
  if (Host = nil) or not Host.GetEvents(Events) then
  begin
    decisionHandler(WKNavigationActionPolicyAllow);
    Exit;
  end;
  Uri := UrlText(navigationAction.request.URL);
  Allow := True;
  Events.ViewNavigate(Uri, Allow);
  if not Allow then
  begin
    decisionHandler(WKNavigationActionPolicyCancel);
    { WebKitGTK reports a denied navigation of the page as an error, which
      WebKit on macOS does not }
    if (navigationAction.targetFrame = nil) or navigationAction.targetFrame.isMainFrame then
      Host.Denied(Uri);
  end
  else if navigationAction.targetFrame = nil then
  begin
    decisionHandler(WKNavigationActionPolicyCancel);
    webView.loadRequest(navigationAction.request);
  end
  else
    decisionHandler(WKNavigationActionPolicyAllow);
end;

{ A response the web view cannot show is downloaded instead }

procedure TCocoaWebViewDelegate.webView_decidePolicyForNavigationResponse_decisionHandler(
  webView: WKWebView; navigationResponse: WKNavigationResponse; decisionHandler: TWKPolicyBlock);
begin
  if navigationResponse.canShowMIMEType then
    decisionHandler(WKNavigationResponsePolicyAllow)
  else
    decisionHandler(WKNavigationResponsePolicyDownload);
end;

procedure TCocoaWebViewDelegate.webView_didStartProvisionalNavigation(webView: WKWebView;
  navigation: WKNavigation);
var
  Events: IWebBrowserEvents;
begin
  if (Host = nil) or Host.IsErrorNavigation(navigation) then
    Exit;
  if Host.GetEvents(Events) then
    Events.ViewLoadChange(WEBKIT_LOAD_STARTED);
end;

procedure TCocoaWebViewDelegate.webView_didReceiveServerRedirectForProvisionalNavigation(
  webView: WKWebView; navigation: WKNavigation);
var
  Events: IWebBrowserEvents;
begin
  if (Host = nil) or Host.IsErrorNavigation(navigation) then
    Exit;
  if Host.GetEvents(Events) then
    Events.ViewLoadChange(WEBKIT_LOAD_REDIRECTED);
end;

procedure TCocoaWebViewDelegate.webView_didFailProvisionalNavigation_withError(
  webView: WKWebView; navigation: WKNavigation; error: NSError);
begin
  if Host <> nil then
    Host.LoadFailed(navigation, error);
end;

procedure TCocoaWebViewDelegate.webView_didCommitNavigation(webView: WKWebView;
  navigation: WKNavigation);
var
  Events: IWebBrowserEvents;
begin
  if (Host = nil) or Host.IsErrorNavigation(navigation) then
    Exit;
  if Host.GetEvents(Events) then
    Events.ViewLoadChange(WEBKIT_LOAD_COMMITTED);
end;

procedure TCocoaWebViewDelegate.webView_didFinishNavigation(webView: WKWebView;
  navigation: WKNavigation);
var
  Events: IWebBrowserEvents;
begin
  if Host = nil then
    Exit;
  if Host.IsErrorNavigation(navigation) then
  begin
    Host.EndError;
    Exit;
  end;
  { Edit mode belongs to a document when the web view has no edit mode of its
    own, so it is set again for each one }
  Host.ApplyEditable;
  if Host.GetEvents(Events) then
    Events.ViewLoadChange(WEBKIT_LOAD_FINISHED);
end;

procedure TCocoaWebViewDelegate.webView_didFailNavigation_withError(webView: WKWebView;
  navigation: WKNavigation; error: NSError);
begin
  if Host <> nil then
    Host.LoadFailed(navigation, error);
end;

procedure TCocoaWebViewDelegate.webView_navigationAction_didBecomeDownload(webView: WKWebView;
  navigationAction: WKNavigationAction; download: WKDownload);
begin
  DownloadBegin(download);
end;

procedure TCocoaWebViewDelegate.webView_navigationResponse_didBecomeDownload(webView: WKWebView;
  navigationResponse: WKNavigationResponse; download: WKDownload);
begin
  DownloadBegin(download);
end;

{ Windows are not created, as the navigation of a new window is loaded in
  the web view when the navigation is decided }

function TCocoaWebViewDelegate.webView_createWebViewWithConfiguration_forNavigationAction_windowFeatures(
  webView: WKWebView; configuration: WKWebViewConfiguration; navigationAction: WKNavigationAction;
  windowFeatures: WKWindowFeatures): WKWebView;
begin
  Result := nil;
end;

procedure TCocoaWebViewDelegate.webView_runJavaScriptAlertPanelWithMessage_initiatedByFrame_completionHandler(
  webView: WKWebView; message_: NSString; frame: WKFrameInfo; completionHandler: TWKVoidBlock);
var
  Events: IWebBrowserEvents;
  Input: string;
  Accepted: Boolean;
begin
  if (Host <> nil) and Host.GetEvents(Events) then
  begin
    Input := '';
    Accepted := False;
    Events.ViewScriptDialog(WEBKIT_SCRIPT_DIALOG_ALERT, FromNSString(message_), Input, Accepted);
  end;
  completionHandler();
end;

procedure TCocoaWebViewDelegate.webView_runJavaScriptConfirmPanelWithMessage_initiatedByFrame_completionHandler(
  webView: WKWebView; message_: NSString; frame: WKFrameInfo; completionHandler: TWKBoolBlock);
var
  Events: IWebBrowserEvents;
  Input: string;
  Accepted: Boolean;
begin
  Accepted := False;
  if (Host <> nil) and Host.GetEvents(Events) then
  begin
    Input := '';
    Events.ViewScriptDialog(WEBKIT_SCRIPT_DIALOG_CONFIRM, FromNSString(message_), Input, Accepted);
  end;
  completionHandler(Accepted);
end;

procedure TCocoaWebViewDelegate.webView_runJavaScriptTextInputPanelWithPrompt_defaultText_initiatedByFrame_completionHandler(
  webView: WKWebView; prompt: NSString; defaultText: NSString; frame: WKFrameInfo;
  completionHandler: TWKStringBlock);
var
  Events: IWebBrowserEvents;
  Input: string;
  Accepted: Boolean;
begin
  Accepted := False;
  Input := FromNSString(defaultText);
  if (Host <> nil) and Host.GetEvents(Events) then
    Events.ViewScriptDialog(WEBKIT_SCRIPT_DIALOG_PROMPT, FromNSString(prompt), Input, Accepted);
  if Accepted then
    completionHandler(ToNSString(Input))
  else
    completionHandler(nil);
end;

{ The hit test script posts a dictionary holding kind, context, link, and
  media }

procedure TCocoaWebViewDelegate.userContentController_didReceiveScriptMessage(
  userContentController: id; message_: WKScriptMessage);
var
  Events: IWebBrowserEvents;
  Body: NSDictionary;
  Kind, Link, Media: string;
  Number: NSNumber;
  Context: LongWord;
begin
  if (Host = nil) or (not message_.name.isEqualToString(NSSTR(MessageName))) then
    Exit;
  if not NSObject(message_.body).isKindOfClass(NSDictionary) then
    Exit;
  Body := NSDictionary(message_.body);
  Kind := FromNSString(NSString(Body.objectForKey(NSSTR('kind'))));
  Link := FromNSString(NSString(Body.objectForKey(NSSTR('link'))));
  Media := FromNSString(NSString(Body.objectForKey(NSSTR('media'))));
  Number := NSNumber(Body.objectForKey(NSSTR('context')));
  if Number <> nil then
    Context := Number.unsignedIntValue
  else
    Context := WEBKIT_HIT_TEST_RESULT_CONTEXT_DOCUMENT;
  if Kind = 'menu' then
  begin
    Host.FMenuContext := Context;
    Host.FMenuLink := Link;
    Host.FMenuMedia := Media;
  end
  else if Host.GetEvents(Events) then
    Events.ViewHitTest(Context, Link, Media);
end;

procedure TCocoaWebViewDelegate.observeValueForKeyPath_ofObject_change_context(
  keyPath: NSString; object_: id; change: NSDictionary; context: pointer);
var
  Events: IWebBrowserEvents;
  View: WKWebView;
begin
  if (Host = nil) or not Host.GetEvents(Events) then
    Exit;
  View := Host.View;
  if keyPath.isEqualToString(NSSTR('URL')) then
    Events.ViewLocationChange(UrlText(View.URL))
  else if keyPath.isEqualToString(NSSTR('title')) then
    Events.ViewTitleChange(FromNSString(View.title))
  { The progress of an error page is not reported }
  else if keyPath.isEqualToString(NSSTR('estimatedProgress')) and not Host.FErrorLoading then
    Events.ViewProgress(Round(View.estimatedProgress * 100));
end;

procedure TCocoaWebViewDelegate.checkInspector(timer: NSTimer);
begin
  if Host <> nil then
    Host.CheckInspector;
end;

{ TWebViewHost }

constructor TWebViewHost.Create(Control: TWinControl; Parent: NSView);
var
  Config: WKWebViewConfiguration;
  Script: WKUserScript;
begin
  inherited Create;
  FControl := Control;
  FZoomFactor := 1;
  FMenuContext := WEBKIT_HIT_TEST_RESULT_CONTEXT_DOCUMENT;
  FDelegate := TCocoaWebViewDelegate.alloc.init;
  FDelegate.Host := Self;
  Config := WKWebViewConfiguration(WKWebViewConfiguration.alloc.init);
  Script := WKUserScript(WKUserScript.alloc).initWithSource_injectionTime_forMainFrameOnly(
    NSSTR(HitTestScript), WKUserScriptInjectionTimeAtDocumentStart, True);
  Config.userContentController.addUserScript(Script);
  Script.release;
  Config.userContentController.addScriptMessageHandler_name(FDelegate, NSSTR(MessageName));
  FView := TCocoaWebView(TCocoaWebView.alloc.initWithFrame_configuration(Parent.bounds, Config));
  Config.release;
  FView.Host := Self;
  FView.setNavigationDelegate(FDelegate);
  FView.setUIDelegate(FDelegate);
  FView.addObserver_forKeyPath_options_context(FDelegate, NSSTR('URL'), 0, nil);
  FView.addObserver_forKeyPath_options_context(FDelegate, NSSTR('title'), 0, nil);
  FView.addObserver_forKeyPath_options_context(FDelegate, NSSTR('estimatedProgress'), 0, nil);
  Parent.addSubview(FView);
  { The LCL sets the mask of a view added to a control, so it is set after }
  FView.setAutoresizingMask(NSViewWidthSizable or NSViewHeightSizable);
  FTimer := NSTimer.scheduledTimerWithTimeInterval_target_selector_userInfo_repeats(
    InspectorInterval, FDelegate, objcselector('checkInspector:'), nil, True);
  FTimer.setTolerance(InspectorInterval / 2);
  Hosts.Add(Self);
end;

{ The script message handler holds the delegate and the timer holds the
  delegate, so both are let go of here }

destructor TWebViewHost.Destroy;
begin
  Hosts.Remove(Self);
  CloseInspector;
  FTimer.invalidate;
  FTimer := nil;
  FView.removeObserver_forKeyPath(FDelegate, NSSTR('URL'));
  FView.removeObserver_forKeyPath(FDelegate, NSSTR('title'));
  FView.removeObserver_forKeyPath(FDelegate, NSSTR('estimatedProgress'));
  FView.configuration.userContentController.removeScriptMessageHandlerForName(NSSTR(MessageName));
  FView.setNavigationDelegate(nil);
  FView.setUIDelegate(nil);
  FView.stopLoading;
  FView.Host := nil;
  FDelegate.Host := nil;
  FView.removeFromSuperview;
  FView.release;
  FView := nil;
  FDelegate.release;
  FDelegate := nil;
  EndError;
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

{ A file can only be loaded by naming the files the web view may read, which
  is every file as it is with WebKitGTK. A uri which is not valid, such as one
  with spaces, is encoded. }

procedure TWebViewHost.Load(const Uri: string);
var
  S: NSString;
  Url: NSURL;
begin
  S := ToNSString(Uri);
  Url := NSURL.URLWithString(S);
  if Url = nil then
    Url := NSURL.URLWithString(S.stringByAddingPercentEncodingWithAllowedCharacters(
      NSCharacterSet.URLFragmentAllowedCharacterSet));
  if Url = nil then
    Exit;
  if Url.isFileURL then
    FView.loadFileURL_allowingReadAccessToURL(Url, NSURL.fileURLWithPath(NSSTR('/')))
  else
    FView.loadRequest(NSURLRequest.requestWithURL(Url));
end;

{ As with WebKitGTK, html loaded from a string is shown at about:blank }

procedure TWebViewHost.LoadHtml(const Html: string);
begin
  FView.loadHTMLString_baseURL(ToNSString(Html), NSURL.URLWithString(NSSTR('about:blank')));
end;

procedure TWebViewHost.ExecuteScript(const Script: string);
begin
  FView.evaluateJavaScript_completionHandler(ToNSString(Script), nil);
end;

{ The private edit mode of the web view is used when it has one, and the
  design mode of the document otherwise }

procedure TWebViewHost.ApplyEditable;
begin
  if MsgSendBool(FView, '_setEditable:', FEditable) then
    Exit;
  if FEditable then
    ExecuteScript('document.designMode = "on";')
  else
    ExecuteScript('document.designMode = "off";');
end;

{ Text alone is zoomed by the private text zoom of the web view. When it has
  none the whole page is zoomed. }

procedure TWebViewHost.ApplyZoom;
begin
  if FZoomTextOnly and MsgSendDouble(FView, '_setTextZoomFactor:', FZoomFactor) then
    FView.setPageZoom(1)
  else
  begin
    MsgSendDouble(FView, '_setTextZoomFactor:', 1);
    FView.setPageZoom(FZoomFactor);
  end;
end;

{ Developer extras add Inspect Element to the context menu and allow the
  inspector to be shown. Inspectable allows Safari to inspect the web view,
  and only exists on macOS 13.3 or later. }

procedure TWebViewHost.SetDeveloperTools(Value: Boolean);
begin
  FView.configuration.preferences.setValue_forKey(NSNumber.numberWithBool(Value),
    NSSTR('developerExtrasEnabled'));
  MsgSendBool(FView, 'setInspectable:', Value);
end;

function TWebViewHost.IsErrorNavigation(Navigation: WKNavigation): Boolean;
begin
  Result := FErrorLoading and ((Navigation = nil) or (Navigation = FErrorNavigation));
end;

procedure TWebViewHost.EndError;
begin
  FErrorLoading := False;
  FErrorUri := '';
  if FErrorNavigation <> nil then
    FErrorNavigation.release;
  FErrorNavigation := nil;
end;

{ The error page is loaded as alternate html for the uri which failed, as
  WebKitGTK does, which places the uri in the history. Html loaded as a
  string is not placed in the history, and is used when the web view has no
  alternate html. }

procedure TWebViewHost.ShowError(const Uri, Message: string);
var
  Url: NSURL;
  Html: NSString;
begin
  Url := NSURL.URLWithString(ToNSString(Uri));
  if Url = nil then
    Url := NSURL.URLWithString(NSSTR('about:blank'));
  EndError;
  FErrorLoading := True;
  FErrorUri := UrlText(Url);
  Html := ToNSString(ErrorPage(Uri, Message));
  if not MsgSendIds(FView, '_loadAlternateHTMLString:baseURL:forUnreachableURL:', Html, Url, Url) then
  begin
    FErrorNavigation := FView.loadHTMLString_baseURL(Html, Url);
    if FErrorNavigation <> nil then
      FErrorNavigation.retain;
  end;
end;

{ As with WebKitGTK, a navigation of the page which was denied is reported as
  an error and then as finished }

procedure TWebViewHost.Denied(const Uri: string);
var
  Events: IWebBrowserEvents;
  Handled: Boolean;
begin
  if not GetEvents(Events) then
    Exit;
  Handled := False;
  Events.ViewError(Uri, WebKitErrorFrameLoadInterruptedByPolicyChange,
    'Frame load interrupted', Handled);
  Events.ViewLoadChange(WEBKIT_LOAD_FINISHED);
end;

{ As with WebKitGTK, a load which fails is reported as an error and then as
  finished, and an error page is shown unless the error was handled. A load
  which was stopped, replaced, or turned into a download has no error page. }

procedure TWebViewHost.LoadFailed(Navigation: WKNavigation; Error: NSError);
var
  Events: IWebBrowserEvents;
  Uri, Message: string;
  Handled, Interrupted: Boolean;
begin
  if IsErrorNavigation(Navigation) then
  begin
    EndError;
    Exit;
  end;
  if not GetEvents(Events) then
    Exit;
  Uri := FromNSString(NSString(Error.userInfo.objectForKey(NSSTR('NSErrorFailingURLStringKey'))));
  if Uri = '' then
    Uri := UrlText(FView.URL);
  Message := FromNSString(Error.localizedDescription);
  Interrupted := (Error.code = NSURLErrorCancelled) or
    (Error.domain.isEqualToString(NSSTR('WebKitErrorDomain')) and
    (Error.code = WebKitErrorFrameLoadInterruptedByPolicyChange));
  Handled := False;
  Events.ViewError(Uri, Error.code, Message, Handled);
  Events.ViewLoadChange(WEBKIT_LOAD_FINISHED);
  if (not Handled) and (not Interrupted) then
    ShowError(Uri, Message);
end;

{ Inspector }

function TWebViewHost.Inspector: id;
begin
  if not MsgSendId(FView, '_inspector', Result) then
    Result := nil;
end;

function TWebViewHost.InspectorView: NSView;
var
  Value: id;
begin
  Result := nil;
  if MsgSendId(Inspector, 'inspectorWebView', Value) then
    Result := NSView(Value);
end;

{ The inspector is shown attached to the web view, which places it beside
  the web view, or in a window of its own. When attached it is detached
  first, which stops WebKit resizing the web view to make room for it. The
  window it leaves behind is hidden.

  The view of the inspector is kept, as WebKit lets go of it when the
  inspector closes and it would otherwise be left in the inspector control. }

procedure TWebViewHost.HostInspector;
var
  Item: NSView;
  Window: NSWindow;
begin
  Item := InspectorView;
  if (Item = nil) or (FInspectorHost = nil) then
    Exit;
  if Item.superview = FInspectorHost then
    Exit;
  if (Item.superview <> nil) and (Item.superview = FView.superview) then
  begin
    MsgSend(Inspector, 'detach');
    Item := InspectorView;
    if Item = nil then
      Exit;
  end;
  RemoveInspectorItem;
  FInspectorItem := Item;
  FInspectorItem.retain;
  Window := Item.window;
  Item.removeFromSuperview;
  Item.setFrame(FInspectorHost.bounds);
  FInspectorHost.addSubview(Item);
  Item.setAutoresizingMask(NSViewWidthSizable or NSViewHeightSizable);
  if (Window <> nil) and (Window <> FInspectorHost.window) and (Window <> FView.window) then
    Window.orderOut(nil);
  if FView.superview <> nil then
    FView.setFrame(FView.superview.bounds);
end;

procedure TWebViewHost.RemoveInspectorItem;
begin
  if FInspectorItem = nil then
    Exit;
  if (FInspectorHost <> nil) and (FInspectorItem.superview = FInspectorHost) then
    FInspectorItem.removeFromSuperview;
  FInspectorItem.release;
  FInspectorItem := nil;
end;

procedure TWebViewHost.ReleaseInspector;
begin
  RemoveInspectorItem;
  FInspectorHost := nil;
  FInspectorControl := nil;
  FInspectorShown := False;
end;

{ CheckInspector is called by a timer, as WebKit has no public way to report
  the inspector being shown or closed. A hosted inspector which the user
  closes is released, and the inspector control is told. An inspector shown
  with no host, such as from Inspect Element, asks the browser control if an
  inspector control will host it. }

procedure TWebViewHost.CheckInspector;
var
  Events: IWebBrowserEvents;
  InspectorEvents: IWebInspectorEvents;
  Control: TWinControl;
  Visible, Hosted: Boolean;
begin
  if not MsgReadBool(Inspector, 'isVisible', Visible) then
    Exit;
  if FInspectorHost <> nil then
  begin
    if Visible then
    begin
      FInspectorShown := True;
      HostInspector;
    end
    else if FInspectorShown then
    begin
      Control := FInspectorControl;
      ReleaseInspector;
      if (Control <> nil) and Supports(Control, IWebInspectorEvents, InspectorEvents) then
        InspectorEvents.ViewInspectorClosed;
    end;
  end
  else if Visible then
  begin
    if FInspectorRequested then
      Exit;
    FInspectorRequested := True;
    Hosted := False;
    if GetEvents(Events) then
      Events.ViewInspectorRequest(Hosted);
  end
  else
    FInspectorRequested := False;
end;

procedure TWebViewHost.OpenInspector(Control: TWinControl; Host: NSView);
var
  Visible: Boolean;
begin
  if (FInspectorHost <> nil) and (FInspectorHost <> Host) then
    CloseInspector;
  FInspectorControl := Control;
  FInspectorHost := Host;
  SetDeveloperTools(True);
  MsgReadBool(Inspector, 'isVisible', Visible);
  if Visible then
  begin
    FInspectorShown := True;
    HostInspector;
  end
  else
  begin
    FInspectorShown := False;
    MsgSend(Inspector, 'show');
    CheckInspector;
  end;
end;

procedure TWebViewHost.CloseInspector;
begin
  if FInspectorHost = nil then
    Exit;
  MsgSend(Inspector, 'close');
  ReleaseInspector;
end;

{ TWSWebBrowser }

class function TWSWebBrowser.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle;
var
  Parent: NSView;
begin
  Result := inherited CreateHandle(AWinControl, AParams);
  if (Result = 0) or (csDesigning in AWinControl.ComponentState) or (not InitWebKit) then
    Exit;
  Parent := NSObject(Result).lclContentView;
  if Parent <> nil then
    TWebViewHost.Create(AWinControl, Parent);
end;

class procedure TWSWebBrowser.DestroyHandle(const AWinControl: TWinControl);
begin
  HostOf(AWinControl).Free;
  inherited DestroyHandle(AWinControl);
end;

class procedure TWSWebBrowser.FocusView(AWinControl: TWinControl);
var
  Host: TWebViewHost;
  Window: NSWindow;
begin
  Host := HostOf(AWinControl);
  if Host = nil then
    Exit;
  Window := Host.View.window;
  if (Window = nil) or (Window.firstResponder = Host.View) then
    Exit;
  if Window.isKindOfClass(TCocoaWindow) then
    TCocoaWindow(Window).makeFirstResponderFromLCL(Host.View)
  else
    Window.makeFirstResponder(Host.View);
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
  Host := HostOf(AWinControl);
  if Host <> nil then
    Host.View.stopLoading;
end;

class procedure TWSWebBrowser.Reload(AWinControl: TWinControl);
var
  Host: TWebViewHost;
begin
  Host := HostOf(AWinControl);
  if Host <> nil then
    Host.View.reload;
end;

class function TWSWebBrowser.GetLoading(AWinControl: TWinControl): Boolean;
var
  Host: TWebViewHost;
begin
  Host := HostOf(AWinControl);
  Result := (Host <> nil) and Host.View.isLoading;
end;

class procedure TWSWebBrowser.ExecuteScript(AWinControl: TWinControl; const Script: string);
var
  Host: TWebViewHost;
begin
  Host := HostOf(AWinControl);
  if Host <> nil then
    Host.ExecuteScript(Script);
end;

class procedure TWSWebBrowser.BackOrForward(AWinControl: TWinControl; Steps: Integer);
var
  Host: TWebViewHost;
  Item: WKBackForwardListItem;
begin
  Host := HostOf(AWinControl);
  if Host = nil then
    Exit;
  Item := Host.View.backForwardList.itemAtIndex(Steps);
  if Item <> nil then
    Host.View.goToBackForwardListItem(Item);
end;

class function TWSWebBrowser.BackOrForwardExists(AWinControl: TWinControl; Steps: Integer): Boolean;
var
  Host: TWebViewHost;
begin
  Host := HostOf(AWinControl);
  Result := (Host <> nil) and (Host.View.backForwardList.itemAtIndex(Steps) <> nil);
end;

class procedure TWSWebBrowser.SetEditable(AWinControl: TWinControl; Value: Boolean);
var
  Host: TWebViewHost;
begin
  Host := HostOf(AWinControl);
  if Host = nil then
    Exit;
  Host.FEditable := Value;
  Host.ApplyEditable;
end;

class procedure TWSWebBrowser.SetZoomFactor(AWinControl: TWinControl; Value: Double);
var
  Host: TWebViewHost;
begin
  Host := HostOf(AWinControl);
  if Host = nil then
    Exit;
  Host.FZoomFactor := Value;
  Host.ApplyZoom;
end;

class procedure TWSWebBrowser.SetZoomTextOnly(AWinControl: TWinControl; Value: Boolean);
var
  Host: TWebViewHost;
begin
  Host := HostOf(AWinControl);
  if Host = nil then
    Exit;
  Host.FZoomTextOnly := Value;
  Host.ApplyZoom;
end;

class procedure TWSWebBrowser.SetDeveloperTools(AWinControl: TWinControl; Value: Boolean);
var
  Host: TWebViewHost;
begin
  Host := HostOf(AWinControl);
  if Host <> nil then
    Host.SetDeveloperTools(Value);
end;

class procedure TWSWebBrowser.ShowInspector(AWinControl: TWinControl; Show: Boolean);
var
  Host: TWebViewHost;
begin
  Host := HostOf(AWinControl);
  if Host = nil then
    Exit;
  if Show then
  begin
    Host.SetDeveloperTools(True);
    MsgSend(Host.Inspector, 'show');
  end
  else
    MsgSend(Host.Inspector, 'close');
end;

{ The print dialog is a sheet of the window holding the web view. The view of
  the print operation is given the size of the web view, without which the
  pages are blank. }

class procedure TWSWebBrowser.Print(AWinControl: TWinControl);
var
  Host: TWebViewHost;
  Operation: NSPrintOperation;
begin
  Host := HostOf(AWinControl);
  if Host = nil then
    Exit;
  Operation := Host.View.printOperationWithPrintInfo(NSPrintInfo.sharedPrintInfo);
  if Operation = nil then
    Exit;
  Operation.setShowsPrintPanel(True);
  Operation.setShowsProgressPanel(True);
  Operation.view.setFrame(Host.View.bounds);
  if Host.View.window <> nil then
    Operation.runOperationModalForWindow_delegate_didRunSelector_contextInfo(Host.View.window,
      nil, nil, nil)
  else
    Operation.runOperation;
end;

{ SnapshotReady is called when a snapshot of a web view is ready, and places
  it on the clipboard as an image }

procedure SnapshotReady(Image: NSImage; Error: NSError); cdecl;
var
  Board: NSPasteboard;
begin
  if Image = nil then
    Exit;
  Board := NSPasteboard.generalPasteboard;
  Board.clearContents;
  Board.writeObjects(NSArray.arrayWithObject(Image));
end;

class procedure TWSWebBrowser.CaptureToClipboard(AWinControl: TWinControl);
var
  Host: TWebViewHost;
begin
  Host := HostOf(AWinControl);
  if Host <> nil then
    Host.View.takeSnapshotWithConfiguration_completionHandler(nil, SnapshotReady);
end;

class function TWSWebBrowser.HistoryItem(AWinControl: TWinControl; Steps: Integer;
  out Title, Uri: string): Boolean;
var
  Host: TWebViewHost;
  Item: WKBackForwardListItem;
begin
  Title := '';
  Uri := '';
  Result := False;
  Host := HostOf(AWinControl);
  if Host = nil then
    Exit;
  Item := Host.View.backForwardList.itemAtIndex(Steps);
  if Item = nil then
    Exit;
  Title := FromNSString(Item.title);
  Uri := UrlText(Item.URL);
  Result := True;
end;

class procedure TWSWebBrowser.Download(AWinControl: TWinControl; const Uri: string);
var
  Host: TWebViewHost;
  Url: NSURL;
begin
  Host := HostOf(AWinControl);
  if Host = nil then
    Exit;
  Url := NSURL.URLWithString(ToNSString(Uri));
  if Url <> nil then
    Host.View.startDownloadUsingRequest_completionHandler(NSURLRequest.requestWithURL(Url),
      DownloadStarted);
end;

class procedure TWSWebBrowser.CancelDownload(Download: Pointer);
begin
  if (Download <> nil) and (Downloads <> nil) and Downloads.containsObject(id(Download)) then
    WKDownload(Download).cancel(nil);
end;

{ The download folder of the user, which is empty if there is none }

class function TWSWebBrowser.DownloadFolder: string;
var
  Paths: NSArray;
begin
  Result := '';
  Paths := NSSearchPathForDirectoriesInDomains(NSDownloadsDirectory, NSUserDomainMask, True);
  if (Paths <> nil) and (Paths.count > 0) then
    Result := FromNSString(NSString(Paths.objectAtIndex(0)));
end;

{ TWSWebInspector }

class procedure TWSWebInspector.DestroyHandle(const AWinControl: TWinControl);
var
  I: Integer;
begin
  if Hosts <> nil then
    for I := 0 to Hosts.Count - 1 do
      if TWebViewHost(Hosts[I]).FInspectorControl = AWinControl then
        TWebViewHost(Hosts[I]).CloseInspector;
  inherited DestroyHandle(AWinControl);
end;

class procedure TWSWebInspector.Open(AWinControl, ABrowser: TWinControl);
var
  Browser: TWebViewHost;
  Host: NSView;
begin
  Browser := HostOf(ABrowser);
  if Browser = nil then
    Exit;
  Host := ContentView(AWinControl);
  if (Host = nil) or (csDesigning in AWinControl.ComponentState) then
  begin
    TWSWebBrowser.ShowInspector(ABrowser, True);
    Exit;
  end;
  Browser.OpenInspector(AWinControl, Host);
end;

class procedure TWSWebInspector.Close(AWinControl, ABrowser: TWinControl);
var
  Browser: TWebViewHost;
begin
  Browser := HostOf(ABrowser);
  if (Browser <> nil) and (Browser.FInspectorControl = AWinControl) then
    Browser.CloseInspector;
end;

initialization
  Hosts := TList.Create;
  Downloads := NSMutableArray.alloc.init;
  DownloadDelegate := TCocoaDownloadDelegate.alloc.init;
finalization
  FreeAndNil(Hosts);
  Downloads.release;
  Downloads := nil;
  DownloadDelegate.release;
  DownloadDelegate := nil;
{$endif}
end.
