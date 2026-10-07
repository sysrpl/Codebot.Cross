(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.webkit.controls.gtk3.txt> }
unit Codebot.WebKit.Controls.Gtk3;

{$i webkit.inc}

interface

uses
  Classes, SysUtils, Controls, LCLType, WSControls, WSLCLClasses;

{ IWebBrowserEvents is used to notify a browser control of changes in its web
  view. This unit cannot use the unit of the browser control, as the two would
  then depend on each other, so the events only use simple types.

  LoadEvent, Context, and Dialog are the WEBKIT_LOAD, WEBKIT_HIT_TEST_RESULT,
  and WEBKIT_SCRIPT_DIALOG values declared in Codebot.Interop.WebKit. }

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
    { Download identifies a download. FileName is the suggested name of the
      file and is changed to the full name of the file to write. }
    procedure ViewDownloadStart(Download: Pointer; const Uri: string;
      var FileName: string; var Allow: Boolean);
    procedure ViewDownloadProgress(Download: Pointer; Progress: Integer);
    procedure ViewDownloadFinish(Download: Pointer; Failed: Boolean;
      const ErrorMessage: string);
  end;

{ IWebInspectorEvents is used to notify an inspector control that its
  inspector was closed }

  IWebInspectorEvents = interface
  ['{B27F4C1D-8E36-4A92-B5D0-3F6A9C0E7D18}']
    procedure ViewInspectorClosed;
  end;

{ TWSWebBrowser creates a web view for a browser control. The routines other
  than CreateHandle do nothing when the control has no web view, which is the
  case at design time or when the WebKitGTK library is not installed. }

  TWSWebBrowser = class(TWSWinControl)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND; override;
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

{ TWSWebInspector creates a container which holds the inspector of a browser
  control. ABrowser is the browser control to inspect. }

  TWSWebInspector = class(TWSWinControl)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND; override;
    class procedure Open(AWinControl, ABrowser: TWinControl);
    class procedure Close(AWinControl, ABrowser: TWinControl);
  end;

implementation

uses
  LMessages, LazGLib2, LazGObject2, LazCairo1, LazGdk3, LazGtk3, Gtk3Widgets,
  Codebot.Interop.WebKit;

{ TGtk3WebInspector is a container widget which holds an inspector }

type
  TGtk3WebInspector = class(TGtk3Widget)
  protected
    function CreateWidget(const Params: TCreateParams): PGtkWidget; override;
  end;

{ TGtk3WebView is a widget holding a WebKitGTK web view. InspectorHost is the
  container the inspector of the web view is placed in, or nil if the inspector
  is to be shown in a window of its own. }

  TGtk3WebView = class(TGtk3Widget)
  protected
    function CreateWidget(const Params: TCreateParams): PGtkWidget; override;
  public
    InspectorHost: TGtk3WebInspector;
    function GtkEventKey(Sender: PGtkWidget; Event: PGdkEvent; AKeyPress: Boolean): Boolean; override; cdecl;
    function GetEvents(out Events: IWebBrowserEvents): Boolean;
  end;

function HitTestRead(HitTest: PWebKitHitTestResult; out Link, Media: string): LongWord;
begin
  Result := webkit_hit_test_result_get_context(HitTest);
  Link := webkit_hit_test_result_get_link_uri(HitTest);
  Media := webkit_hit_test_result_get_image_uri(HitTest);
  if Media = '' then
    Media := webkit_hit_test_result_get_media_uri(HitTest);
end;

{ Signals }

procedure WebViewLoadChanged(View: PWebKitWebView; LoadEvent: LongInt;
  Data: TGtk3WebView); cdecl;
var
  Events: IWebBrowserEvents;
begin
  if Data.GetEvents(Events) then
    Events.ViewLoadChange(LoadEvent);
end;

function WebViewLoadFailed(View: PWebKitWebView; LoadEvent: LongInt;
  FailingUri: PChar; Error: PGError; Data: TGtk3WebView): gboolean; cdecl;
var
  Events: IWebBrowserEvents;
  Handled: Boolean;
begin
  Handled := False;
  if Data.GetEvents(Events) and (Error <> nil) then
    Events.ViewError(FailingUri, Error.code, Error.message, Handled);
  Result := Handled;
end;

procedure WebViewNotifyProgress(View: PWebKitWebView; Spec: Pointer;
  Data: TGtk3WebView); cdecl;
var
  Events: IWebBrowserEvents;
begin
  if Data.GetEvents(Events) then
    Events.ViewProgress(Round(webkit_web_view_get_estimated_load_progress(View) * 100));
end;

procedure WebViewNotifyUri(View: PWebKitWebView; Spec: Pointer;
  Data: TGtk3WebView); cdecl;
var
  Events: IWebBrowserEvents;
begin
  if Data.GetEvents(Events) then
    Events.ViewLocationChange(webkit_web_view_get_uri(View));
end;

procedure WebViewNotifyTitle(View: PWebKitWebView; Spec: Pointer;
  Data: TGtk3WebView); cdecl;
var
  Events: IWebBrowserEvents;
begin
  if Data.GetEvents(Events) then
    Events.ViewTitleChange(webkit_web_view_get_title(View));
end;

{ A navigation which asks for a new window, such as a link with a target of
  _blank, would do nothing as the control only has the one web view. When it
  is allowed it is loaded in the web view instead. }

function WebViewDecidePolicy(View: PWebKitWebView; Decision: PWebKitPolicyDecision;
  DecisionType: LongInt; Data: TGtk3WebView): gboolean; cdecl;
var
  Events: IWebBrowserEvents;
  Navigation: PWebKitNavigationAction;
  Allow: Boolean;
  Uri: string;
begin
  Result := False;
  { A response the web view cannot show is downloaded instead }
  if DecisionType = WEBKIT_POLICY_DECISION_TYPE_RESPONSE then
  begin
    if not webkit_response_policy_decision_is_mime_type_supported(Decision) then
    begin
      webkit_policy_decision_download(Decision);
      Result := True;
    end;
    Exit;
  end;
  if (DecisionType <> WEBKIT_POLICY_DECISION_TYPE_NAVIGATION_ACTION) and
    (DecisionType <> WEBKIT_POLICY_DECISION_TYPE_NEW_WINDOW_ACTION) then
    Exit;
  if not Data.GetEvents(Events) then
    Exit;
  Navigation := webkit_navigation_policy_decision_get_navigation_action(Decision);
  Uri := webkit_uri_request_get_uri(webkit_navigation_action_get_request(Navigation));
  Allow := True;
  Events.ViewNavigate(Uri, Allow);
  if not Allow then
    webkit_policy_decision_ignore(Decision)
  else if DecisionType = WEBKIT_POLICY_DECISION_TYPE_NEW_WINDOW_ACTION then
  begin
    webkit_policy_decision_ignore(Decision);
    webkit_web_view_load_uri(View, PChar(Uri));
  end
  else
    webkit_policy_decision_use(Decision);
  Result := True;
end;

procedure WebViewMouseTargetChanged(View: PWebKitWebView; HitTest: PWebKitHitTestResult;
  Modifiers: LongWord; Data: TGtk3WebView); cdecl;
var
  Events: IWebBrowserEvents;
  Link, Media: string;
  Context: LongWord;
begin
  if not Data.GetEvents(Events) then
    Exit;
  Context := HitTestRead(HitTest, Link, Media);
  Events.ViewHitTest(Context, Link, Media);
end;

function WebViewContextMenu(View: PWebKitWebView; Menu: Pointer; Event: PGdkEvent;
  HitTest: PWebKitHitTestResult; Data: TGtk3WebView): gboolean; cdecl;
var
  Events: IWebBrowserEvents;
  Link, Media: string;
  Context: LongWord;
  Handled: Boolean;
  X, Y: Integer;
begin
  Result := False;
  if not Data.GetEvents(Events) then
    Exit;
  X := -1;
  Y := -1;
  if (Event <> nil) and (Event.type_ = GDK_BUTTON_PRESS) then
  begin
    X := Round(Event.button.x);
    Y := Round(Event.button.y);
  end;
  Context := HitTestRead(HitTest, Link, Media);
  Handled := False;
  Events.ViewContextMenu(X, Y, Context, Link, Media, Handled);
  Result := Handled;
end;

{ The dialog shown when leaving a page with unsaved changes is left to the
  web view }

function WebViewScriptDialog(View: PWebKitWebView; Dialog: PWebKitScriptDialog;
  Data: TGtk3WebView): gboolean; cdecl;
var
  Events: IWebBrowserEvents;
  Kind: Integer;
  Input: string;
  Accepted: Boolean;
begin
  Result := False;
  if not Data.GetEvents(Events) then
    Exit;
  Input := '';
  Kind := webkit_script_dialog_get_dialog_type(Dialog);
  case Kind of
    WEBKIT_SCRIPT_DIALOG_ALERT, WEBKIT_SCRIPT_DIALOG_CONFIRM: ;
    WEBKIT_SCRIPT_DIALOG_PROMPT:
      Input := webkit_script_dialog_prompt_get_default_text(Dialog);
  else
    Exit;
  end;
  Accepted := False;
  Events.ViewScriptDialog(Kind, webkit_script_dialog_get_message(Dialog), Input, Accepted);
  case Kind of
    WEBKIT_SCRIPT_DIALOG_CONFIRM: webkit_script_dialog_confirm_set_confirmed(Dialog, Accepted);
    WEBKIT_SCRIPT_DIALOG_PROMPT:
      if Accepted then
        webkit_script_dialog_prompt_set_text(Dialog, PChar(Input));
  end;
  Result := True;
end;

{ The inspector is a widget the LCL knows nothing about, so the LCL is not
  told when it takes or loses the input focus. The inspector host is told
  here, which lets the LCL treat the inspector control as the focused control
  and notify its parents, such as a caption box. }

function InspectorFocus(Widget: PGtkWidget; Event: PGdkEventFocus;
  Host: TGtk3WebInspector): gboolean; cdecl;
var
  Msg: TLMessage;
begin
  Result := False;
  if Host.LCLObject = nil then
    Exit;
  FillChar(Msg{%H-}, SizeOf(Msg), #0);
  if Event.in_ <> 0 then
    Msg.Msg := LM_SETFOCUS
  else
    Msg.Msg := LM_KILLFOCUS;
  Host.DeliverMessage(Msg);
end;

{ The inspector asks to be shown either attached to its web view or in a
  window of its own. In both cases it is placed in the inspector host when
  there is one. Returning true tells the inspector the request was handled. }

function InspectorEmbed(Inspector: PWebKitWebInspector; Data: TGtk3WebView): gboolean; cdecl;
var
  Host, Widget: PGtkWidget;
begin
  Result := False;
  if Data.InspectorHost = nil then
    Exit;
  Host := Data.InspectorHost.Widget;
  Widget := webkit_web_inspector_get_web_view(Inspector);
  if (Host = nil) or (Widget = nil) then
    Exit;
  if gtk_widget_get_parent(Widget) = nil then
  begin
    gtk_container_add(PGtkContainer(Host), Widget);
    g_signal_connect_data(Widget, 'focus-in-event', TGCallback(@InspectorFocus), Data.InspectorHost, nil, G_CONNECT_DEFAULT);
    g_signal_connect_data(Widget, 'focus-out-event', TGCallback(@InspectorFocus), Data.InspectorHost, nil, G_CONNECT_DEFAULT);
  end;
  gtk_widget_show(Widget);
  Result := True;
end;

{ The inspector stays in the inspector host when asked to detach }

function InspectorDetach(Inspector: PWebKitWebInspector; Data: TGtk3WebView): gboolean; cdecl;
begin
  Result := Data.InspectorHost <> nil;
end;

procedure InspectorClosed(Inspector: PWebKitWebInspector; Data: TGtk3WebView); cdecl;
var
  Host: TGtk3WebInspector;
  Events: IWebInspectorEvents;
begin
  Host := Data.InspectorHost;
  Data.InspectorHost := nil;
  if (Host <> nil) and (Host.LCLObject <> nil) and
    Supports(Host.LCLObject, IWebInspectorEvents, Events) then
    Events.ViewInspectorClosed;
end;

{ Downloads belong to the web context shared by every web view, and so their
  signals are not connected to a widget. The widget of the web view which
  started a download is found when a signal arrives. It is not found if that
  web view has been destroyed, in which case the download carries on with
  nothing to notify. }

function DownloadEvents(Download: Pointer; out Events: IWebBrowserEvents): Boolean;
var
  View: PWebKitWebView;
  Widget: TObject;
begin
  Result := False;
  Events := nil;
  View := webkit_download_get_web_view(Download);
  if View = nil then
    Exit;
  Widget := TObject(g_object_get_data(PGObject(View), 'lclwidget'));
  if Widget is TGtk3WebView then
    Result := TGtk3WebView(Widget).GetEvents(Events);
end;

function DownloadDecideDestination(Download: Pointer; SuggestedName: PChar;
  Data: Pointer): gboolean; cdecl;
var
  Events: IWebBrowserEvents;
  FileName: string;
  Allow: Boolean;
  Uri: Pgchar;
begin
  Result := False;
  if not DownloadEvents(Download, Events) then
    Exit;
  Result := True;
  FileName := SuggestedName;
  Allow := True;
  Events.ViewDownloadStart(Download,
    webkit_uri_request_get_uri(webkit_download_get_request(Download)), FileName, Allow);
  Uri := nil;
  if Allow and (FileName <> '') then
    Uri := g_filename_to_uri(PChar(FileName), nil, nil);
  if Uri = nil then
  begin
    webkit_download_cancel(Download);
    Exit;
  end;
  webkit_download_set_destination(Download, Uri);
  g_free(Uri);
end;

procedure DownloadReceivedData(Download: Pointer; Length: QWord; Data: Pointer); cdecl;
var
  Events: IWebBrowserEvents;
begin
  if DownloadEvents(Download, Events) then
    Events.ViewDownloadProgress(Download,
      Round(webkit_download_get_estimated_progress(Download) * 100));
end;

procedure DownloadFailed(Download: Pointer; Error: PGError; Data: Pointer); cdecl;
var
  Events: IWebBrowserEvents;
begin
  if not DownloadEvents(Download, Events) then
    Exit;
  if Error <> nil then
    Events.ViewDownloadFinish(Download, True, Error.message)
  else
    Events.ViewDownloadFinish(Download, True, '');
end;

{ The finished signal follows the failed signal when a download fails }

procedure DownloadFinished(Download: Pointer; Data: Pointer); cdecl;
var
  Events: IWebBrowserEvents;
begin
  if DownloadEvents(Download, Events) then
    Events.ViewDownloadFinish(Download, False, '');
end;

procedure ContextDownloadStarted(Context, Download, Data: Pointer); cdecl;
begin
  g_signal_connect_data(Download, 'decide-destination', TGCallback(@DownloadDecideDestination), nil, nil, G_CONNECT_DEFAULT);
  g_signal_connect_data(Download, 'received-data', TGCallback(@DownloadReceivedData), nil, nil, G_CONNECT_DEFAULT);
  g_signal_connect_data(Download, 'failed', TGCallback(@DownloadFailed), nil, nil, G_CONNECT_DEFAULT);
  g_signal_connect_data(Download, 'finished', TGCallback(@DownloadFinished), nil, nil, G_CONNECT_DEFAULT);
end;

var
  ContextConnected: Boolean;

{ TGtk3WebInspector }

function TGtk3WebInspector.CreateWidget(const Params: TCreateParams): PGtkWidget;
begin
  Result := PGtkWidget(gtk_event_box_new());
end;

{ TGtk3WebView }

function TGtk3WebView.CreateWidget(const Params: TCreateParams): PGtkWidget;
var
  View: PGObject;
  Inspector: PGObject;
begin
  Result := PGtkWidget(webkit_web_view_new());
  View := PGObject(Result);
  { Every web view shares the one web context, which is never destroyed }
  if not ContextConnected then
  begin
    ContextConnected := True;
    g_signal_connect_data(webkit_web_view_get_context(PWebKitWebView(Result)),
      'download-started', TGCallback(@ContextDownloadStarted), nil, nil, G_CONNECT_DEFAULT);
  end;
  Inspector := PGObject(webkit_web_view_get_inspector(PWebKitWebView(Result)));
  g_signal_connect_data(Inspector, 'attach', TGCallback(@InspectorEmbed), Self, nil, G_CONNECT_DEFAULT);
  g_signal_connect_data(Inspector, 'open-window', TGCallback(@InspectorEmbed), Self, nil, G_CONNECT_DEFAULT);
  g_signal_connect_data(Inspector, 'detach', TGCallback(@InspectorDetach), Self, nil, G_CONNECT_DEFAULT);
  g_signal_connect_data(Inspector, 'closed', TGCallback(@InspectorClosed), Self, nil, G_CONNECT_DEFAULT);
  g_signal_connect_data(View, 'load-changed', TGCallback(@WebViewLoadChanged), Self, nil, G_CONNECT_DEFAULT);
  g_signal_connect_data(View, 'load-failed', TGCallback(@WebViewLoadFailed), Self, nil, G_CONNECT_DEFAULT);
  g_signal_connect_data(View, 'notify::estimated-load-progress', TGCallback(@WebViewNotifyProgress), Self, nil, G_CONNECT_DEFAULT);
  g_signal_connect_data(View, 'notify::uri', TGCallback(@WebViewNotifyUri), Self, nil, G_CONNECT_DEFAULT);
  g_signal_connect_data(View, 'notify::title', TGCallback(@WebViewNotifyTitle), Self, nil, G_CONNECT_DEFAULT);
  g_signal_connect_data(View, 'decide-policy', TGCallback(@WebViewDecidePolicy), Self, nil, G_CONNECT_DEFAULT);
  g_signal_connect_data(View, 'mouse-target-changed', TGCallback(@WebViewMouseTargetChanged), Self, nil, G_CONNECT_DEFAULT);
  g_signal_connect_data(View, 'context-menu', TGCallback(@WebViewContextMenu), Self, nil, G_CONNECT_DEFAULT);
  g_signal_connect_data(View, 'script-dialog', TGCallback(@WebViewScriptDialog), Self, nil, G_CONNECT_DEFAULT);
end;

{ GtkEventKey replaces the one inherited from TGtk3Widget, which passes keys
  through the input method of the LCL and stops some keys, such as tab and
  return, from reaching the widget. The web view has its own input method and
  needs every key, so that it is possible to type in a web page. }

function TGtk3WebView.GtkEventKey(Sender: PGtkWidget; Event: PGdkEvent; AKeyPress: Boolean): Boolean; cdecl;
begin
  Result := False;
end;

{ The web view can send signals while it is being destroyed, at which point
  the control might not exist }

function TGtk3WebView.GetEvents(out Events: IWebBrowserEvents): Boolean;
begin
  Events := nil;
  Result := (LCLObject <> nil) and Supports(LCLObject, IWebBrowserEvents, Events);
end;

function WebView(AWinControl: TWinControl): PWebKitWebView;
var
  Widget: TGtk3Widget;
begin
  Result := nil;
  if not AWinControl.HandleAllocated then
    Exit;
  Widget := TGtk3Widget(AWinControl.Handle);
  if Widget is TGtk3WebView then
    Result := PWebKitWebView(Widget.Widget);
end;

{ TWSWebBrowser }

class function TWSWebBrowser.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND;
begin
  if (csDesigning in AWinControl.ComponentState) or (not InitWebKit) then
    Result := {%H-}TLCLHandle(TGtk3WinControlPanel.Create(AWinControl, AParams))
  else
    Result := {%H-}TLCLHandle(TGtk3WebView.Create(AWinControl, AParams));
end;

class procedure TWSWebBrowser.Load(AWinControl: TWinControl; const Uri: string);
var
  View: PWebKitWebView;
begin
  View := WebView(AWinControl);
  if View <> nil then
    webkit_web_view_load_uri(View, PChar(Uri));
end;

class procedure TWSWebBrowser.LoadHtml(AWinControl: TWinControl; const Html: string);
var
  View: PWebKitWebView;
begin
  View := WebView(AWinControl);
  if View <> nil then
    webkit_web_view_load_html(View, PChar(Html), nil);
end;

class procedure TWSWebBrowser.Stop(AWinControl: TWinControl);
var
  View: PWebKitWebView;
begin
  View := WebView(AWinControl);
  if View <> nil then
    webkit_web_view_stop_loading(View);
end;

class procedure TWSWebBrowser.Reload(AWinControl: TWinControl);
var
  View: PWebKitWebView;
begin
  View := WebView(AWinControl);
  if View <> nil then
    webkit_web_view_reload(View);
end;

class function TWSWebBrowser.GetLoading(AWinControl: TWinControl): Boolean;
var
  View: PWebKitWebView;
begin
  View := WebView(AWinControl);
  Result := (View <> nil) and webkit_web_view_is_loading(View);
end;

class procedure TWSWebBrowser.ExecuteScript(AWinControl: TWinControl; const Script: string);
var
  View: PWebKitWebView;
begin
  View := WebView(AWinControl);
  if View <> nil then
    webkit_web_view_evaluate_javascript(View, PChar(Script), Length(Script), nil,
      nil, nil, nil, nil);
end;

class procedure TWSWebBrowser.BackOrForward(AWinControl: TWinControl; Steps: Integer);
var
  View: PWebKitWebView;
  Item: PWebKitBackForwardListItem;
begin
  View := WebView(AWinControl);
  if View = nil then
    Exit;
  Item := webkit_back_forward_list_get_nth_item(webkit_web_view_get_back_forward_list(View), Steps);
  if Item <> nil then
    webkit_web_view_go_to_back_forward_list_item(View, Item);
end;

class function TWSWebBrowser.BackOrForwardExists(AWinControl: TWinControl; Steps: Integer): Boolean;
var
  View: PWebKitWebView;
begin
  View := WebView(AWinControl);
  Result := (View <> nil) and
    (webkit_back_forward_list_get_nth_item(webkit_web_view_get_back_forward_list(View), Steps) <> nil);
end;

class procedure TWSWebBrowser.SetEditable(AWinControl: TWinControl; Value: Boolean);
var
  View: PWebKitWebView;
begin
  View := WebView(AWinControl);
  if View <> nil then
    webkit_web_view_set_editable(View, Value);
end;

class procedure TWSWebBrowser.SetZoomFactor(AWinControl: TWinControl; Value: Double);
var
  View: PWebKitWebView;
begin
  View := WebView(AWinControl);
  if View <> nil then
    webkit_web_view_set_zoom_level(View, Value);
end;

class procedure TWSWebBrowser.SetZoomTextOnly(AWinControl: TWinControl; Value: Boolean);
var
  View: PWebKitWebView;
begin
  View := WebView(AWinControl);
  if View <> nil then
    webkit_settings_set_zoom_text_only(webkit_web_view_get_settings(View), Value);
end;

class procedure TWSWebBrowser.SetDeveloperTools(AWinControl: TWinControl; Value: Boolean);
var
  View: PWebKitWebView;
begin
  View := WebView(AWinControl);
  if View <> nil then
    webkit_settings_set_enable_developer_extras(webkit_web_view_get_settings(View), Value);
end;

class procedure TWSWebBrowser.ShowInspector(AWinControl: TWinControl; Show: Boolean);
var
  View: PWebKitWebView;
begin
  View := WebView(AWinControl);
  if View = nil then
    Exit;
  if Show then
    webkit_web_inspector_show(webkit_web_view_get_inspector(View))
  else
    webkit_web_inspector_close(webkit_web_view_get_inspector(View));
end;

{ The print dialog is modal to the window holding the web view }

class procedure TWSWebBrowser.Print(AWinControl: TWinControl);
var
  View: PWebKitWebView;
  Operation: Pointer;
begin
  View := WebView(AWinControl);
  if View = nil then
    Exit;
  Operation := webkit_print_operation_new(View);
  webkit_print_operation_run_dialog(Operation, gtk_widget_get_toplevel(PGtkWidget(View)));
  g_object_unref(Operation);
end;

{ SnapshotReady is called when a snapshot of a web view is ready, and places
  it on the clipboard as an image }

procedure SnapshotReady(Source, Res, Data: Pointer); cdecl;
var
  Surface: Pcairo_surface_t;
  Pixbuf: Pointer;
  W, H: Integer;
begin
  Surface := webkit_web_view_get_snapshot_finish(Source, Res, nil);
  if Surface = nil then
    Exit;
  W := cairo_image_surface_get_width(Surface);
  H := cairo_image_surface_get_height(Surface);
  { The size of a surface which is not an image is unknown, so the size of
    the web view is used }
  if (W < 1) or (H < 1) then
  begin
    W := gtk_widget_get_allocated_width(Source);
    H := gtk_widget_get_allocated_height(Source);
  end;
  Pixbuf := gdk_pixbuf_get_from_surface(Surface, 0, 0, W, H);
  if Pixbuf <> nil then
  begin
    gtk_clipboard_set_image(gtk_clipboard_get(gdk_atom_intern('CLIPBOARD', False)), Pixbuf);
    g_object_unref(Pixbuf);
  end;
  cairo_surface_destroy(Surface);
end;

class procedure TWSWebBrowser.CaptureToClipboard(AWinControl: TWinControl);
var
  View: PWebKitWebView;
begin
  View := WebView(AWinControl);
  if View <> nil then
    webkit_web_view_get_snapshot(View, WEBKIT_SNAPSHOT_REGION_VISIBLE,
      WEBKIT_SNAPSHOT_OPTIONS_NONE, nil, @SnapshotReady, nil);
end;

class function TWSWebBrowser.HistoryItem(AWinControl: TWinControl; Steps: Integer;
  out Title, Uri: string): Boolean;
var
  View: PWebKitWebView;
  Item: PWebKitBackForwardListItem;
begin
  Title := '';
  Uri := '';
  Result := False;
  View := WebView(AWinControl);
  if View = nil then
    Exit;
  Item := webkit_back_forward_list_get_nth_item(webkit_web_view_get_back_forward_list(View), Steps);
  if Item = nil then
    Exit;
  Title := webkit_back_forward_list_item_get_title(Item);
  Uri := webkit_back_forward_list_item_get_uri(Item);
  Result := True;
end;

{ The download is owned by the web context, and the reference returned here
  is not needed }

class procedure TWSWebBrowser.Download(AWinControl: TWinControl; const Uri: string);
var
  View: PWebKitWebView;
  Item: Pointer;
begin
  View := WebView(AWinControl);
  if View = nil then
    Exit;
  Item := webkit_web_view_download_uri(View, PChar(Uri));
  if Item <> nil then
    g_object_unref(Item);
end;

class procedure TWSWebBrowser.CancelDownload(Download: Pointer);
begin
  if (Download <> nil) and InitWebKit then
    webkit_download_cancel(Download);
end;

{ The download folder of the user, which is empty if there is none }

class function TWSWebBrowser.DownloadFolder: string;
begin
  Result := g_get_user_special_dir(G_USER_DIRECTORY_DOWNLOAD);
end;

{ TWSWebInspector }

function WebViewWidget(AWinControl: TWinControl): TGtk3WebView;
var
  Widget: TGtk3Widget;
begin
  Result := nil;
  if not AWinControl.HandleAllocated then
    Exit;
  Widget := TGtk3Widget(AWinControl.Handle);
  if (Widget is TGtk3WebView) and (Widget.Widget <> nil) then
    Result := TGtk3WebView(Widget);
end;

class function TWSWebInspector.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND;
begin
  if (csDesigning in AWinControl.ComponentState) or (not InitWebKit) then
    Result := {%H-}TLCLHandle(TGtk3WinControlPanel.Create(AWinControl, AParams))
  else
    Result := {%H-}TLCLHandle(TGtk3WebInspector.Create(AWinControl, AParams));
end;

class procedure TWSWebInspector.Open(AWinControl, ABrowser: TWinControl);
var
  View: TGtk3WebView;
  Host: TGtk3Widget;
  Inspector: PWebKitWebInspector;
begin
  View := WebViewWidget(ABrowser);
  if (View = nil) or (not AWinControl.HandleAllocated) then
    Exit;
  Host := TGtk3Widget(AWinControl.Handle);
  if not (Host is TGtk3WebInspector) then
    Exit;
  if View.InspectorHost = Host then
    Exit;
  Inspector := webkit_web_view_get_inspector(PWebKitWebView(View.Widget));
  { An inspector shown elsewhere is closed so that it asks to be shown again }
  if webkit_web_inspector_get_web_view(Inspector) <> nil then
    webkit_web_inspector_close(Inspector);
  View.InspectorHost := TGtk3WebInspector(Host);
  webkit_web_inspector_show(Inspector);
end;

class procedure TWSWebInspector.Close(AWinControl, ABrowser: TWinControl);
var
  View: TGtk3WebView;
begin
  View := WebViewWidget(ABrowser);
  if (View = nil) or (not AWinControl.HandleAllocated) then
    Exit;
  if View.InspectorHost <> TGtk3Widget(AWinControl.Handle) then
    Exit;
  webkit_web_inspector_close(webkit_web_view_get_inspector(PWebKitWebView(View.Widget)));
  View.InspectorHost := nil;
end;

end.
