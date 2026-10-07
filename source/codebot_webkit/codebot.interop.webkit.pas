(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ Codebot.Interop.WebKit declares the part of the C API of the WebKitGTK
  library used by this package. The library is loaded when InitWebKit is
  called rather than linked when a program is built, so a program can start
  and report that WebKitGTK is not installed. }
unit Codebot.Interop.WebKit;

{$i webkit.inc}

interface

{$ifdef lclgtk3}
uses
  Codebot.Core;

const
  WEBKIT_LOAD_STARTED = 0;
  WEBKIT_LOAD_REDIRECTED = 1;
  WEBKIT_LOAD_COMMITTED = 2;
  WEBKIT_LOAD_FINISHED = 3;

  WEBKIT_POLICY_DECISION_TYPE_NAVIGATION_ACTION = 0;
  WEBKIT_POLICY_DECISION_TYPE_NEW_WINDOW_ACTION = 1;
  WEBKIT_POLICY_DECISION_TYPE_RESPONSE = 2;

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
  WEBKIT_SCRIPT_DIALOG_BEFORE_UNLOAD_CONFIRM = 3;

  WEBKIT_SNAPSHOT_REGION_VISIBLE = 0;
  WEBKIT_SNAPSHOT_REGION_FULL_DOCUMENT = 1;

  WEBKIT_SNAPSHOT_OPTIONS_NONE = 0;

type
  TWebKitWebView = record end;
  PWebKitWebView = ^TWebKitWebView;
  TWebKitSettings = record end;
  PWebKitSettings = ^TWebKitSettings;
  TWebKitBackForwardList = record end;
  PWebKitBackForwardList = ^TWebKitBackForwardList;
  TWebKitBackForwardListItem = record end;
  PWebKitBackForwardListItem = ^TWebKitBackForwardListItem;
  TWebKitWebInspector = record end;
  PWebKitWebInspector = ^TWebKitWebInspector;
  TWebKitPolicyDecision = record end;
  PWebKitPolicyDecision = ^TWebKitPolicyDecision;
  TWebKitNavigationAction = record end;
  PWebKitNavigationAction = ^TWebKitNavigationAction;
  TWebKitURIRequest = record end;
  PWebKitURIRequest = ^TWebKitURIRequest;
  TWebKitHitTestResult = record end;
  PWebKitHitTestResult = ^TWebKitHitTestResult;
  TWebKitScriptDialog = record end;
  PWebKitScriptDialog = ^TWebKitScriptDialog;

{ WebKit routines }

var
  webkit_get_major_version: function: LongWord; cdecl;
  webkit_get_minor_version: function: LongWord; cdecl;
  webkit_get_micro_version: function: LongWord; cdecl;

  { webkit_web_view_new returns a GtkWidget }
  webkit_web_view_new: function: PWebKitWebView; cdecl;
  webkit_web_view_load_uri: procedure(view: PWebKitWebView; uri: PChar); cdecl;
  webkit_web_view_load_html: procedure(view: PWebKitWebView; content, base_uri: PChar); cdecl;
  webkit_web_view_stop_loading: procedure(view: PWebKitWebView); cdecl;
  webkit_web_view_reload: procedure(view: PWebKitWebView); cdecl;
  webkit_web_view_is_loading: function(view: PWebKitWebView): LongBool; cdecl;
  webkit_web_view_get_estimated_load_progress: function(view: PWebKitWebView): Double; cdecl;
  webkit_web_view_get_uri: function(view: PWebKitWebView): PChar; cdecl;
  webkit_web_view_get_title: function(view: PWebKitWebView): PChar; cdecl;
  webkit_web_view_go_back: procedure(view: PWebKitWebView); cdecl;
  webkit_web_view_go_forward: procedure(view: PWebKitWebView); cdecl;
  webkit_web_view_can_go_back: function(view: PWebKitWebView): LongBool; cdecl;
  webkit_web_view_can_go_forward: function(view: PWebKitWebView): LongBool; cdecl;
  webkit_web_view_get_back_forward_list: function(view: PWebKitWebView): PWebKitBackForwardList; cdecl;
  webkit_web_view_go_to_back_forward_list_item: procedure(view: PWebKitWebView;
    item: PWebKitBackForwardListItem); cdecl;
  webkit_back_forward_list_get_nth_item: function(list: PWebKitBackForwardList;
    index: LongInt): PWebKitBackForwardListItem; cdecl;
  webkit_back_forward_list_item_get_title: function(item: PWebKitBackForwardListItem): PChar; cdecl;
  webkit_back_forward_list_item_get_uri: function(item: PWebKitBackForwardListItem): PChar; cdecl;
  webkit_web_view_set_zoom_level: procedure(view: PWebKitWebView; zoom_level: Double); cdecl;
  webkit_web_view_get_zoom_level: function(view: PWebKitWebView): Double; cdecl;
  webkit_web_view_set_editable: procedure(view: PWebKitWebView; editable: LongBool); cdecl;
  webkit_web_view_is_editable: function(view: PWebKitWebView): LongBool; cdecl;
  { The cancellable, callback, and user_data arguments may be nil when the
    result of the script is not wanted }
  webkit_web_view_evaluate_javascript: procedure(view: PWebKitWebView; script: PChar;
    length: PtrInt; world_name, source_uri: PChar; cancellable, callback,
    user_data: Pointer); cdecl;
  webkit_web_view_get_settings: function(view: PWebKitWebView): PWebKitSettings; cdecl;
  { The snapshot is taken in the background and callback, a GAsyncReadyCallback,
    is called when it is ready. The finish routine returns a cairo_surface_t. }
  webkit_web_view_get_snapshot: procedure(view: PWebKitWebView; region, options: LongInt;
    cancellable, callback, user_data: Pointer); cdecl;
  webkit_web_view_get_snapshot_finish: function(view: PWebKitWebView; result,
    error: Pointer): Pointer; cdecl;

  { webkit_print_operation_new returns a WebKitPrintOperation, and parent is
    a GtkWindow }
  webkit_print_operation_new: function(view: PWebKitWebView): Pointer; cdecl;
  webkit_print_operation_run_dialog: function(operation, parent: Pointer): LongInt; cdecl;
  webkit_web_view_get_inspector: function(view: PWebKitWebView): PWebKitWebInspector; cdecl;

  webkit_settings_set_zoom_text_only: procedure(settings: PWebKitSettings; zoom_text_only: LongBool); cdecl;
  webkit_settings_get_zoom_text_only: function(settings: PWebKitSettings): LongBool; cdecl;
  webkit_settings_set_enable_developer_extras: procedure(settings: PWebKitSettings; enabled: LongBool); cdecl;

  webkit_web_inspector_show: procedure(inspector: PWebKitWebInspector); cdecl;
  webkit_web_inspector_close: procedure(inspector: PWebKitWebInspector); cdecl;
  { webkit_web_inspector_get_web_view returns a GtkWidget }
  webkit_web_inspector_get_web_view: function(inspector: PWebKitWebInspector): Pointer; cdecl;

  webkit_policy_decision_use: procedure(decision: PWebKitPolicyDecision); cdecl;
  webkit_policy_decision_ignore: procedure(decision: PWebKitPolicyDecision); cdecl;
  webkit_policy_decision_download: procedure(decision: PWebKitPolicyDecision); cdecl;
  webkit_navigation_policy_decision_get_navigation_action: function(
    decision: PWebKitPolicyDecision): PWebKitNavigationAction; cdecl;
  webkit_navigation_action_get_request: function(action: PWebKitNavigationAction): PWebKitURIRequest; cdecl;
  webkit_uri_request_get_uri: function(request: PWebKitURIRequest): PChar; cdecl;

  webkit_response_policy_decision_is_mime_type_supported: function(
    decision: PWebKitPolicyDecision): LongBool; cdecl;

  { The context and download arguments below are a WebKitWebContext and a
    WebKitDownload. A download destination is a file uri. }
  webkit_web_view_get_context: function(view: PWebKitWebView): Pointer; cdecl;
  webkit_web_view_download_uri: function(view: PWebKitWebView; uri: PChar): Pointer; cdecl;
  webkit_download_get_web_view: function(download: Pointer): PWebKitWebView; cdecl;
  webkit_download_get_request: function(download: Pointer): PWebKitURIRequest; cdecl;
  webkit_download_get_destination: function(download: Pointer): PChar; cdecl;
  webkit_download_set_destination: procedure(download: Pointer; destination: PChar); cdecl;
  webkit_download_set_allow_overwrite: procedure(download: Pointer; allowed: LongBool); cdecl;
  webkit_download_get_estimated_progress: function(download: Pointer): Double; cdecl;
  webkit_download_cancel: procedure(download: Pointer); cdecl;

  webkit_hit_test_result_get_context: function(hit_test: PWebKitHitTestResult): LongWord; cdecl;
  webkit_hit_test_result_get_link_uri: function(hit_test: PWebKitHitTestResult): PChar; cdecl;
  webkit_hit_test_result_get_image_uri: function(hit_test: PWebKitHitTestResult): PChar; cdecl;
  webkit_hit_test_result_get_media_uri: function(hit_test: PWebKitHitTestResult): PChar; cdecl;

  webkit_script_dialog_get_dialog_type: function(dialog: PWebKitScriptDialog): LongInt; cdecl;
  webkit_script_dialog_get_message: function(dialog: PWebKitScriptDialog): PChar; cdecl;
  webkit_script_dialog_confirm_set_confirmed: procedure(dialog: PWebKitScriptDialog; confirmed: LongBool); cdecl;
  webkit_script_dialog_prompt_get_default_text: function(dialog: PWebKitScriptDialog): PChar; cdecl;
  webkit_script_dialog_prompt_set_text: procedure(dialog: PWebKitScriptDialog; text: PChar); cdecl;

const
  libwebkit = 'libwebkit2gtk-4.1.so.0';

{ InitWebKit loads the WebKitGTK library the first time it is called and
  returns true if the library and all of the routines above were found }

function InitWebKit(ThrowExceptions: Boolean = False): Boolean;

{$endif}

implementation

{$ifdef lclgtk3}
var
  LoadedWebKit: Boolean;
  InitializedWebKit: Boolean;

function InitWebKit(ThrowExceptions: Boolean = False): Boolean;
var
  FailedModuleName: string;
  FailedProcName: string;
  Module: HModule;

  procedure CheckExceptions;
  begin
    if (not InitializedWebKit) and (ThrowExceptions) then
      LibraryExceptProc(FailedModuleName, FailedProcName);
  end;

  function TryLoad(const ProcName: string; var Proc: Pointer): Boolean;
  begin
    FailedProcName := ProcName;
    Proc := LibraryGetProc(Module, ProcName);
    Result := Proc <> nil;
    if not Result then
    begin
      CheckExceptions;
    end;
  end;

begin
  ThrowExceptions := ThrowExceptions and Assigned(LibraryExceptProc);
  if LoadedWebKit then
  begin
    CheckExceptions;
    Exit(InitializedWebKit);
  end;
  LoadedWebKit := True;
  Result := False;
  FailedModuleName := libwebkit;
  FailedProcName := '';
  Module := LibraryLoad(libwebkit);
  if Module = ModuleNil then
  begin
    CheckExceptions;
    Exit;
  end;
  Result :=
    TryLoad('webkit_get_major_version', @webkit_get_major_version) and
    TryLoad('webkit_get_minor_version', @webkit_get_minor_version) and
    TryLoad('webkit_get_micro_version', @webkit_get_micro_version) and
    TryLoad('webkit_web_view_new', @webkit_web_view_new) and
    TryLoad('webkit_web_view_load_uri', @webkit_web_view_load_uri) and
    TryLoad('webkit_web_view_load_html', @webkit_web_view_load_html) and
    TryLoad('webkit_web_view_stop_loading', @webkit_web_view_stop_loading) and
    TryLoad('webkit_web_view_reload', @webkit_web_view_reload) and
    TryLoad('webkit_web_view_is_loading', @webkit_web_view_is_loading) and
    TryLoad('webkit_web_view_get_estimated_load_progress', @webkit_web_view_get_estimated_load_progress) and
    TryLoad('webkit_web_view_get_uri', @webkit_web_view_get_uri) and
    TryLoad('webkit_web_view_get_title', @webkit_web_view_get_title) and
    TryLoad('webkit_web_view_go_back', @webkit_web_view_go_back) and
    TryLoad('webkit_web_view_go_forward', @webkit_web_view_go_forward) and
    TryLoad('webkit_web_view_can_go_back', @webkit_web_view_can_go_back) and
    TryLoad('webkit_web_view_can_go_forward', @webkit_web_view_can_go_forward) and
    TryLoad('webkit_web_view_get_back_forward_list', @webkit_web_view_get_back_forward_list) and
    TryLoad('webkit_web_view_go_to_back_forward_list_item', @webkit_web_view_go_to_back_forward_list_item) and
    TryLoad('webkit_back_forward_list_get_nth_item', @webkit_back_forward_list_get_nth_item) and
    TryLoad('webkit_back_forward_list_item_get_title', @webkit_back_forward_list_item_get_title) and
    TryLoad('webkit_back_forward_list_item_get_uri', @webkit_back_forward_list_item_get_uri) and
    TryLoad('webkit_response_policy_decision_is_mime_type_supported', @webkit_response_policy_decision_is_mime_type_supported) and
    TryLoad('webkit_web_view_get_context', @webkit_web_view_get_context) and
    TryLoad('webkit_web_view_download_uri', @webkit_web_view_download_uri) and
    TryLoad('webkit_download_get_web_view', @webkit_download_get_web_view) and
    TryLoad('webkit_download_get_request', @webkit_download_get_request) and
    TryLoad('webkit_download_get_destination', @webkit_download_get_destination) and
    TryLoad('webkit_download_set_destination', @webkit_download_set_destination) and
    TryLoad('webkit_download_set_allow_overwrite', @webkit_download_set_allow_overwrite) and
    TryLoad('webkit_download_get_estimated_progress', @webkit_download_get_estimated_progress) and
    TryLoad('webkit_download_cancel', @webkit_download_cancel) and
    TryLoad('webkit_web_view_set_zoom_level', @webkit_web_view_set_zoom_level) and
    TryLoad('webkit_web_view_get_zoom_level', @webkit_web_view_get_zoom_level) and
    TryLoad('webkit_web_view_set_editable', @webkit_web_view_set_editable) and
    TryLoad('webkit_web_view_is_editable', @webkit_web_view_is_editable) and
    TryLoad('webkit_web_view_evaluate_javascript', @webkit_web_view_evaluate_javascript) and
    TryLoad('webkit_web_view_get_settings', @webkit_web_view_get_settings) and
    TryLoad('webkit_web_view_get_inspector', @webkit_web_view_get_inspector) and
    TryLoad('webkit_web_view_get_snapshot', @webkit_web_view_get_snapshot) and
    TryLoad('webkit_web_view_get_snapshot_finish', @webkit_web_view_get_snapshot_finish) and
    TryLoad('webkit_print_operation_new', @webkit_print_operation_new) and
    TryLoad('webkit_print_operation_run_dialog', @webkit_print_operation_run_dialog) and
    TryLoad('webkit_settings_set_zoom_text_only', @webkit_settings_set_zoom_text_only) and
    TryLoad('webkit_settings_get_zoom_text_only', @webkit_settings_get_zoom_text_only) and
    TryLoad('webkit_settings_set_enable_developer_extras', @webkit_settings_set_enable_developer_extras) and
    TryLoad('webkit_web_inspector_show', @webkit_web_inspector_show) and
    TryLoad('webkit_web_inspector_close', @webkit_web_inspector_close) and
    TryLoad('webkit_web_inspector_get_web_view', @webkit_web_inspector_get_web_view) and
    TryLoad('webkit_policy_decision_use', @webkit_policy_decision_use) and
    TryLoad('webkit_policy_decision_ignore', @webkit_policy_decision_ignore) and
    TryLoad('webkit_policy_decision_download', @webkit_policy_decision_download) and
    TryLoad('webkit_navigation_policy_decision_get_navigation_action', @webkit_navigation_policy_decision_get_navigation_action) and
    TryLoad('webkit_navigation_action_get_request', @webkit_navigation_action_get_request) and
    TryLoad('webkit_uri_request_get_uri', @webkit_uri_request_get_uri) and
    TryLoad('webkit_hit_test_result_get_context', @webkit_hit_test_result_get_context) and
    TryLoad('webkit_hit_test_result_get_link_uri', @webkit_hit_test_result_get_link_uri) and
    TryLoad('webkit_hit_test_result_get_image_uri', @webkit_hit_test_result_get_image_uri) and
    TryLoad('webkit_hit_test_result_get_media_uri', @webkit_hit_test_result_get_media_uri) and
    TryLoad('webkit_script_dialog_get_dialog_type', @webkit_script_dialog_get_dialog_type) and
    TryLoad('webkit_script_dialog_get_message', @webkit_script_dialog_get_message) and
    TryLoad('webkit_script_dialog_confirm_set_confirmed', @webkit_script_dialog_confirm_set_confirmed) and
    TryLoad('webkit_script_dialog_prompt_get_default_text', @webkit_script_dialog_prompt_get_default_text) and
    TryLoad('webkit_script_dialog_prompt_set_text', @webkit_script_dialog_prompt_set_text);
  InitializedWebKit := Result;
end;

{$endif}

end.