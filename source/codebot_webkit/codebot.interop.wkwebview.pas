(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ Codebot.Interop.WKWebView declares the part of the WebKit framework of macOS
  used by this package. The WebKit unit of FPC only completes WKWebView itself,
  and leaves out its configuration, delegates, and newer methods, so the
  classes are declared here instead. That unit is not used, as its classes
  have the same names.

  WebKit is part of macOS and is linked when a program is built. The package
  requires macOS 12 Monterey or later. A few private methods of WebKit are
  called by name, and only when the object responds to them. }
{ <include docs/codebot.interop.wkwebview.txt> }
unit Codebot.Interop.WKWebView;

{$i webkit.inc}
{$ifdef lclcocoa}
  {$modeswitch objectivec2}
  {$modeswitch cblocks}
  {$linkframework WebKit}
{$endif}

interface

{$ifdef lclcocoa}
uses
  CocoaAll;

const
  { WKNavigationActionPolicy }
  WKNavigationActionPolicyCancel = 0;
  WKNavigationActionPolicyAllow = 1;
  WKNavigationActionPolicyDownload = 2;

  { WKNavigationResponsePolicy }
  WKNavigationResponsePolicyCancel = 0;
  WKNavigationResponsePolicyAllow = 1;
  WKNavigationResponsePolicyDownload = 2;

  { WKUserScriptInjectionTime }
  WKUserScriptInjectionTimeAtDocumentStart = 0;
  WKUserScriptInjectionTimeAtDocumentEnd = 1;

  { NSURLErrorCancelled is reported when a load is stopped or replaced by
    another load }
  NSURLErrorCancelled = -999;

  { WebKitErrorFrameLoadInterruptedByPolicyChange is reported when a
    navigation is denied or becomes a download }
  WebKitErrorFrameLoadInterruptedByPolicyChange = 102;

{ Completion handlers passed to the delegates by WebKit, and to WebKit by this
  package. A class declared in this unit cannot be used as a parameter of a
  block, which stops FPC 3.2 with an internal error, so id is used instead. }

type
  TWKVoidBlock = reference to procedure; cdecl; cblock;
  TWKBoolBlock = reference to procedure(Value: ObjCBOOL); cdecl; cblock;
  TWKStringBlock = reference to procedure(Value: NSString); cdecl; cblock;
  TWKPolicyBlock = reference to procedure(Policy: NSInteger); cdecl; cblock;
  TWKURLBlock = reference to procedure(Url: NSURL); cdecl; cblock;
  TWKScriptBlock = reference to procedure(Value: id; Error: NSError); cdecl; cblock;
  TWKImageBlock = reference to procedure(Image: NSImage; Error: NSError); cdecl; cblock;
  TWKDownloadBlock = reference to procedure(Download: id); cdecl; cblock;

  WKPreferences = objcclass external (NSObject)
  end;

  WKScriptMessage = objcclass external (NSObject)
  public
    function body: id; message 'body';
    function name: NSString; message 'name';
  end;

  WKScriptMessageHandlerProtocol = objcprotocol external name 'WKScriptMessageHandler' (NSObjectProtocol)
  required
    procedure userContentController_didReceiveScriptMessage(userContentController: id;
      message_: WKScriptMessage); message 'userContentController:didReceiveScriptMessage:';
  end;

  WKUserScript = objcclass external (NSObject)
  public
    function initWithSource_injectionTime_forMainFrameOnly(source: NSString;
      injectionTime: NSInteger; forMainFrameOnly: ObjCBOOL): id; message 'initWithSource:injectionTime:forMainFrameOnly:';
  end;

  WKUserContentController = objcclass external (NSObject)
  public
    procedure addUserScript(userScript: WKUserScript); message 'addUserScript:';
    procedure addScriptMessageHandler_name(scriptMessageHandler: id;
      name: NSString); message 'addScriptMessageHandler:name:';
    procedure removeScriptMessageHandlerForName(name: NSString); message 'removeScriptMessageHandlerForName:';
  end;

  WKWebViewConfiguration = objcclass external (NSObject)
  public
    function preferences: WKPreferences; message 'preferences';
    function userContentController: WKUserContentController; message 'userContentController';
  end;

  WKBackForwardListItem = objcclass external (NSObject)
  public
    function URL: NSURL; message 'URL';
    function title: NSString; message 'title';
  end;

  WKBackForwardList = objcclass external (NSObject)
  public
    function itemAtIndex(index: NSInteger): WKBackForwardListItem; message 'itemAtIndex:';
  end;

  WKFrameInfo = objcclass external (NSObject)
  public
    function isMainFrame: ObjCBOOL; message 'isMainFrame';
  end;

  WKNavigation = objcclass external (NSObject)
  end;

  WKNavigationAction = objcclass external (NSObject)
  public
    function targetFrame: WKFrameInfo; message 'targetFrame';
    function request: NSURLRequest; message 'request';
    function shouldPerformDownload: ObjCBOOL; message 'shouldPerformDownload';
  end;

  WKNavigationResponse = objcclass external (NSObject)
  public
    function isForMainFrame: ObjCBOOL; message 'isForMainFrame';
    function response: NSURLResponse; message 'response';
    function canShowMIMEType: ObjCBOOL; message 'canShowMIMEType';
  end;

  WKWindowFeatures = objcclass external (NSObject)
  end;

  WKWebView = objcclass external (NSView)
  public
    function initWithFrame_configuration(frame: NSRect; configuration: WKWebViewConfiguration): id; message 'initWithFrame:configuration:';
    function configuration: WKWebViewConfiguration; message 'configuration';
    procedure setNavigationDelegate(newValue: id); message 'setNavigationDelegate:';
    procedure setUIDelegate(newValue: id); message 'setUIDelegate:';
    function backForwardList: WKBackForwardList; message 'backForwardList';
    function loadRequest(request: NSURLRequest): WKNavigation; message 'loadRequest:';
    function loadFileURL_allowingReadAccessToURL(URL_: NSURL; readAccessURL: NSURL): WKNavigation; message 'loadFileURL:allowingReadAccessToURL:';
    function loadHTMLString_baseURL(string_: NSString; baseURL: NSURL): WKNavigation; message 'loadHTMLString:baseURL:';
    function goToBackForwardListItem(item: WKBackForwardListItem): WKNavigation; message 'goToBackForwardListItem:';
    function title: NSString; message 'title';
    function URL: NSURL; message 'URL';
    function isLoading: ObjCBOOL; message 'isLoading';
    function estimatedProgress: Double; message 'estimatedProgress';
    function reload: WKNavigation; message 'reload';
    procedure stopLoading; message 'stopLoading';
    procedure evaluateJavaScript_completionHandler(javaScriptString: NSString;
      completionHandler: TWKScriptBlock); message 'evaluateJavaScript:completionHandler:';
    procedure setPageZoom(newValue: Double); message 'setPageZoom:';
    procedure takeSnapshotWithConfiguration_completionHandler(snapshotConfiguration: id;
      completionHandler: TWKImageBlock); message 'takeSnapshotWithConfiguration:completionHandler:';
    function printOperationWithPrintInfo(printInfo: NSPrintInfo): NSPrintOperation; message 'printOperationWithPrintInfo:';
    procedure startDownloadUsingRequest_completionHandler(request: NSURLRequest;
      completionHandler: TWKDownloadBlock); message 'startDownloadUsingRequest:completionHandler:';
  end;

  WKDownload = objcclass external (NSObject)
  public
    function originalRequest: NSURLRequest; message 'originalRequest';
    function webView: WKWebView; message 'webView';
    procedure setDelegate(newValue: id); message 'setDelegate:';
    function progress: NSProgress; message 'progress';
    procedure cancel(completionHandler: id); message 'cancel:';
  end;

  WKNavigationDelegateProtocol = objcprotocol external name 'WKNavigationDelegate' (NSObjectProtocol)
  optional
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
  end;

  WKUIDelegateProtocol = objcprotocol external name 'WKUIDelegate' (NSObjectProtocol)
  optional
    function webView_createWebViewWithConfiguration_forNavigationAction_windowFeatures(webView: WKWebView;
      configuration: WKWebViewConfiguration; navigationAction: WKNavigationAction;
      windowFeatures: WKWindowFeatures): WKWebView; message 'webView:createWebViewWithConfiguration:forNavigationAction:windowFeatures:';
    procedure webView_runJavaScriptAlertPanelWithMessage_initiatedByFrame_completionHandler(webView: WKWebView;
      message_: NSString; frame: WKFrameInfo; completionHandler: TWKVoidBlock); message 'webView:runJavaScriptAlertPanelWithMessage:initiatedByFrame:completionHandler:';
    procedure webView_runJavaScriptConfirmPanelWithMessage_initiatedByFrame_completionHandler(webView: WKWebView;
      message_: NSString; frame: WKFrameInfo; completionHandler: TWKBoolBlock); message 'webView:runJavaScriptConfirmPanelWithMessage:initiatedByFrame:completionHandler:';
    procedure webView_runJavaScriptTextInputPanelWithPrompt_defaultText_initiatedByFrame_completionHandler(webView: WKWebView;
      prompt: NSString; defaultText: NSString; frame: WKFrameInfo; completionHandler: TWKStringBlock); message 'webView:runJavaScriptTextInputPanelWithPrompt:defaultText:initiatedByFrame:completionHandler:';
  end;

  WKDownloadDelegateProtocol = objcprotocol external name 'WKDownloadDelegate' (NSObjectProtocol)
  required
    procedure download_decideDestinationUsingResponse_suggestedFilename_completionHandler(download: WKDownload;
      response: NSURLResponse; suggestedFilename: NSString; completionHandler: TWKURLBlock); message 'download:decideDestinationUsingResponse:suggestedFilename:completionHandler:';
  optional
    procedure downloadDidFinish(download: WKDownload); message 'downloadDidFinish:';
    procedure download_didFailWithError_resumeData(download: WKDownload; error: NSError;
      resumeData: NSData); message 'download:didFailWithError:resumeData:';
  end;

{ NSView has a method called before its context menu opens, which the AppKit
  units of FPC do not declare }

  NSViewMenuHook = objccategory external (NSView)
    procedure willOpenMenu_withEvent(menu: NSMenu; event: NSEvent); message 'willOpenMenu:withEvent:';
  end;

{ Send a message by name. These return False when Obj does not respond to the
  message, and are used for methods which are private or only exist on newer
  versions of macOS. }

function MsgSend(Obj: id; const Name: PChar): Boolean;
function MsgSendId(Obj: id; const Name: PChar; out Value: id): Boolean;
function MsgSendBool(Obj: id; const Name: PChar; Value: Boolean): Boolean;
function MsgSendDouble(Obj: id; const Name: PChar; Value: Double): Boolean;
function MsgSendIds(Obj: id; const Name: PChar; A, B, C: id): Boolean;
function MsgReadBool(Obj: id; const Name: PChar; out Value: Boolean): Boolean;

{ NSString conversion to and from the UTF-8 strings of the LCL }

function ToNSString(const S: string): NSString;
function FromNSString(S: NSString): string;
{$endif}

implementation

{$ifdef lclcocoa}
type
  TSendProc = procedure(Obj: id; Selector: SEL); cdecl;
  TSendIdFunc = function(Obj: id; Selector: SEL): id; cdecl;
  TSendBoolProc = procedure(Obj: id; Selector: SEL; Value: ObjCBOOL); cdecl;
  TSendDoubleProc = procedure(Obj: id; Selector: SEL; Value: Double); cdecl;
  TSendBoolFunc = function(Obj: id; Selector: SEL): ObjCBOOL; cdecl;
  TSendIdsProc = procedure(Obj: id; Selector: SEL; A, B, C: id); cdecl;

{ objc_msgSend is cast to the type of each method, as its arguments are not
  passed the same way as those of a varargs function on every processor }

function Responds(Obj: id; const Name: PChar; out Selector: SEL): Boolean;
begin
  Selector := sel_registerName(Name);
  Result := (Obj <> nil) and NSObject(Obj).respondsToSelector(Selector);
end;

function MsgSend(Obj: id; const Name: PChar): Boolean;
var
  Selector: SEL;
begin
  Result := Responds(Obj, Name, Selector);
  if Result then
    TSendProc(@objc_msgSend)(Obj, Selector);
end;

function MsgSendId(Obj: id; const Name: PChar; out Value: id): Boolean;
var
  Selector: SEL;
begin
  Value := nil;
  Result := Responds(Obj, Name, Selector);
  if Result then
    Value := TSendIdFunc(@objc_msgSend)(Obj, Selector);
end;

function MsgSendBool(Obj: id; const Name: PChar; Value: Boolean): Boolean;
var
  Selector: SEL;
begin
  Result := Responds(Obj, Name, Selector);
  if Result then
    TSendBoolProc(@objc_msgSend)(Obj, Selector, ObjCBOOL(Value));
end;

function MsgSendDouble(Obj: id; const Name: PChar; Value: Double): Boolean;
var
  Selector: SEL;
begin
  Result := Responds(Obj, Name, Selector);
  if Result then
    TSendDoubleProc(@objc_msgSend)(Obj, Selector, Value);
end;

function MsgSendIds(Obj: id; const Name: PChar; A, B, C: id): Boolean;
var
  Selector: SEL;
begin
  Result := Responds(Obj, Name, Selector);
  if Result then
    TSendIdsProc(@objc_msgSend)(Obj, Selector, A, B, C);
end;

function MsgReadBool(Obj: id; const Name: PChar; out Value: Boolean): Boolean;
var
  Selector: SEL;
begin
  Value := False;
  Result := Responds(Obj, Name, Selector);
  if Result then
    Value := TSendBoolFunc(@objc_msgSend)(Obj, Selector);
end;

function ToNSString(const S: string): NSString;
begin
  Result := NSString.stringWithUTF8String(PChar(S));
  if Result = nil then
    Result := NSString.string_;
end;

function FromNSString(S: NSString): string;
begin
  if S = nil then
    Result := ''
  else
    Result := S.UTF8String;
end;
{$endif}

end.
