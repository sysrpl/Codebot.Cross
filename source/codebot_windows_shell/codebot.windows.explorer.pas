(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.windows.explorer.txt> }
unit Codebot.Windows.Explorer;

{$i ../codebot/codebot.inc}

interface

{$ifdef windows}
uses
  Windows, Messages, ActiveX, ShlObj, SysUtils, Classes, Graphics, Controls,
  StdCtrls, Forms, LMessages,
  Codebot.System,
  Codebot.Binding,
  Codebot.Interop.Windows.Shell;

const
  { Posted to a tree to report the checkboxes changed by the user }
  WM_EXPLORERCHECK = WM_USER + $51;
  { Posted to an edit to close its drop down tree }
  WM_EXPLORERCLOSEUP = WM_USER + $52;

{ TExplorerViewMode }

type
  TExplorerViewMode = (evIcon, evSmallIcon, evList, evDetails, evThumbnail,
    evTile, evThumbstrip);

{ TExplorerViewOption is a folder flag of a TExplorerView }

  TExplorerViewOption = (voAutoArrange, voSnapToGrid, voSingleSelect,
    voNoSubfolders, voShowSelectionAlways, voSingleClickActivate,
    voHideFileNames, voCheckBoxes, voAutoCheckSelect, voTriCheckSelect,
    voFullRowSelect, voNoGrouping, voNoFilters, voNoColumnHeader,
    voNoHeaderInAllViews, voExtendedTiles);

  TExplorerViewOptions = set of TExplorerViewOption;

{ TExplorerTreeOption is a style of a TExplorerTree }

  TExplorerTreeOption = (toHasLines, toSingleClickExpand, toSpringExpand,
    toFullRowSelect, toShowSelectionAlways, toHorizontalScroll,
    toAutoHorizontalScroll, toFadeExpandos, toEvenHeight, toNoInfoTips,
    toNoEditLabels, toNoDragDrop, toFavoritesMode, toCheckBoxes);

  TExplorerTreeOptions = set of TExplorerTreeOption;

{ TExplorerEditMode is how a TExplorerEdit shows its folder }

  TExplorerEditMode = (emEdit, emBreadcrumb, emDropDown);

const
  DefaultTreeOptions = [toFullRowSelect, toShowSelectionAlways, toFadeExpandos];

{ TExplorerPathEvent is used by events which can be stopped by setting
  Allow to false }

type
  TExplorerPathEvent = procedure(Sender: TObject; const Path: string;
    var Allow: Boolean) of object;

{ TExplorerFolderEvent is used by events about a folder }

  TExplorerFolderEvent = procedure(Sender: TObject; const Path: string) of object;

{ TExplorerCheckEvent is used when the checkbox of an item changes }

  TExplorerCheckEvent = procedure(Sender: TObject; const Path: string;
    Checked: Boolean) of object;

{ TExplorerControl is the base class of the controls which host windows
  created by the shell. The mouse and keyboard input of those windows is
  passed to the control, so its events such as OnMouseDown, OnKeyDown, and
  OnContextPopup work, and at design time the windows are transparent to the
  mouse so the form designer can select the control }

  TExplorerControl = class(TCustomControl)
  private
    FInContextMenu: Boolean;
  protected
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    { Pass the input of the windows created by the shell to the control }
    function InputMessage(Wnd: HWND; Msg: UINT; WParam: Windows.WPARAM;
      LParam: Windows.LPARAM; var Handled: Boolean): LRESULT; virtual;
    { Hook the windows the shell created inside the control }
    procedure HookChildWindows;
    { Invoke OnContextPopup and show PopupMenu. Returns true if the shell
      menu should not be shown }
    function ShowContextPopup(MousePos: TPoint): Boolean;
    { True while a context menu message is passed on to the shell }
    property InContextMenu: Boolean read FInContextMenu;
  public
    destructor Destroy; override;
  end;

{ TExplorerView hosts the Windows explorer browser, the folder view used by
  Windows Explorer and the file dialogs. Paths are file system paths or shell
  parsing names, and an empty path is the desktop }

  TExplorerView = class(TExplorerControl, IComponentBindingClient,
    IServiceProvider, ICommDlgBrowser, ICommDlgBrowser2, IExplorerBrowserEvents)
  private
    FBinding: IComponentBinding;
    FBrowser: IExplorerBrowser;
    FColumns: TStrings;
    FCookie: DWORD;
    FEmptyText: string;
    FFilter: string;
    FFolder: PItemIDList;
    FGroupBy: string;
    FHistory: TList;
    FHistoryIndex: Integer;
    FHistoryTarget: Integer;
    FIconSize: Integer;
    FOptions: TExplorerViewOptions;
    FPendingFolder: PItemIDList;
    FPath: string;
    FSearching: Boolean;
    FSearchPending: Boolean;
    FSearchScope: PItemIDList;
    FSearchText: string;
    FShowHidden: Boolean;
    FSortColumn: string;
    FSortDescending: Boolean;
    FViewMode: TExplorerViewMode;
    FOnContextMenuClose: TNotifyEvent;
    FOnContextMenuOpen: TNotifyEvent;
    FOnDefaultAction: TExplorerPathEvent;
    FOnIncludeItem: TExplorerPathEvent;
    FOnNavigate: TExplorerPathEvent;
    FOnNavigated: TNotifyEvent;
    FOnNavigationFailed: TExplorerFolderEvent;
    FOnSelectionChanged: TNotifyEvent;
    procedure CreateBrowser;
    procedure DestroyBrowser;
    procedure BrowseTo(pidl: PItemIDList; Flags: UINT);
    procedure ApplyColumns;
    procedure ApplyFolderFlags;
    procedure ApplyGroupBy;
    procedure ApplySort;
    procedure ApplyViewMode;
    procedure ColumnsChange(Sender: TObject);
    procedure InvokeVerb(const Verb: string; Background: Boolean);
    function FolderFlags: DWORD;
    function CountItems(Target: UINT): Integer;
    function GetFocusedItem: string;
    function GetFolderView: IFolderView;
    function GetFolderView2: IFolderView2;
    function GetItemCount: Integer;
    function GetPath: string;
    procedure SetPath(const Value: string);
    function GetSelectedCount: Integer;
    function GetShellView: IShellView;
    procedure SetColumns(Value: TStrings);
    procedure SetEmptyText(const Value: string);
    procedure SetFilter(const Value: string);
    procedure SetGroupBy(const Value: string);
    procedure SetIconSize(Value: Integer);
    procedure SetOptions(Value: TExplorerViewOptions);
    procedure SetShowHidden(Value: Boolean);
    procedure SetSortColumn(const Value: string);
    procedure SetSortDescending(Value: Boolean);
    procedure SetViewMode(Value: TExplorerViewMode);
    procedure ShowIDList(pidl: PItemIDList);
    procedure SelectionChanged;
    function GetCanGoBack: Boolean;
    function GetCanGoForward: Boolean;
    function GetHistory(Index: Integer): string;
    function GetHistoryCount: Integer;
    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
    { IComponentBindingClient }
    function GetBinding: IComponentBinding;
    procedure SetBinding(Value: IComponentBinding);
    { IServiceProvider }
    function QueryService(const rsid, iid: TGuid; out Obj): HResult; stdcall;
    { ICommDlgBrowser }
    function OnDefaultCommand(const ppshv: IShellView): HResult; stdcall;
    function OnStateChange(const ppshv: IShellView; Change: ULONG): HResult; stdcall;
    function IncludeObject(const ppshv: IShellView; pidl: PItemIDList): HResult; stdcall;
    { ICommDlgBrowser2 }
    function Notify(ppshv: IShellView; dwNotifyType: DWORD): HResult; stdcall;
    function GetDefaultMenuText(ppshv: IShellView; pszText: PWideChar;
      cchMax: Integer): HResult; stdcall;
    function GetViewFlags(out pdwFlags: DWORD): HResult; stdcall;
    { IExplorerBrowserEvents }
    function OnNavigationPending(pidlFolder: PItemIDList): HResult; stdcall;
    function OnViewCreated(psv: IShellView): HResult; stdcall;
    function OnNavigationComplete(pidlFolder: PItemIDList): HResult; stdcall;
    { Renamed because the class has an OnNavigationFailed event }
    function IExplorerBrowserEvents.OnNavigationFailed = NavigationFailed;
    function NavigationFailed(pidlFolder: PItemIDList): HResult; stdcall;
  protected
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure Resize; override;
  public
    { Create a new explorer view }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Show the previous folder in the history }
    procedure GoBack;
    { Show the next folder in the history after going back }
    procedure GoForward;
    { Show a folder of the history by its index }
    procedure GoToHistory(Index: Integer);
    { Remove all folders from the history except the current one }
    procedure ClearHistory;
    { Show the parent folder }
    procedure GoUp;
    { Reload the items of the folder }
    procedure Refresh; reintroduce;
    { Invoke a context menu verb on the selected items, or on the folder if
      no items are selected }
    procedure ExecuteVerb(const Verb: string);
    { Edit the name of the focused item }
    procedure Rename;
    { Copy the selected items to the clipboard }
    procedure Copy;
    { Cut the selected items to the clipboard }
    procedure Cut;
    { Paste the clipboard into the folder }
    procedure Paste;
    { Delete the selected items }
    procedure Delete;
    { Show the properties of the selected items }
    procedure Properties;
    { Create a new folder and edit its name }
    procedure NewFolder;
    { Select an item of the folder by its path }
    procedure Select(const ItemPath: string);
    { Select all the items }
    procedure SelectAll;
    { Clear the selection }
    procedure ClearSelection;
    { Get the paths of the selected or of all the items. With the voCheckBoxes
      option the selected items are the checked items }
    procedure GetItems(Strings: TStrings; Selection: Boolean = True);
    { Show the items whose names contain Text, searching Scope and its
      subfolders. Text may hold the wildcards * and ?. When Scope is empty the
      current folder is searched, or the folder of the previous search while
      search results are shown. An empty Text returns to the folder searched.
      Requires Windows 8 or later }
    procedure Search(const Text: string; const Scope: string = '');
    { The explorer browser object, or nil if the window is not created }
    property Browser: IExplorerBrowser read FBrowser;
    { There is a previous folder in the history }
    property CanGoBack: Boolean read GetCanGoBack;
    { There is a next folder in the history }
    property CanGoForward: Boolean read GetCanGoForward;
    { The paths of the folders shown, oldest first }
    property History[Index: Integer]: string read GetHistory;
    { The number of folders in the history }
    property HistoryCount: Integer read GetHistoryCount;
    { The index of the current folder in the history }
    property HistoryIndex: Integer read FHistoryIndex;
    { Search results are shown }
    property Searching: Boolean read FSearching;
    { The text of the search results shown }
    property SearchText: string read FSearchText;
    { The path of the focused item, or an empty string if there is none }
    property FocusedItem: string read GetFocusedItem;
    { The number of items in the folder }
    property ItemCount: Integer read GetItemCount;
    { The number of selected items }
    property SelectedCount: Integer read GetSelectedCount;
    { The shell view of the folder shown, or nil if there is none }
    property ShellView: IShellView read GetShellView;
  published
    { The columns of the details view by canonical property name, such as
      System.ItemNameDisplay or System.Size, each optionally followed by
      =width in pixels. When empty the folder chooses its columns }
    property Columns: TStrings read FColumns write SetColumns;
    { Text shown when a folder has no items }
    property EmptyText: string read FEmptyText write SetEmptyText;
    { File masks separated by semicolons, such as *.pas;*.lpk. Folders are
      always shown. When empty all files are shown }
    property Filter: string read FFilter write SetFilter;
    { The canonical name of the property the items are grouped by, such as
      System.ItemTypeText. When empty the folder chooses its grouping }
    property GroupBy: string read FGroupBy write SetGroupBy;
    { The size of the icons in pixels. When 0 the view mode chooses the size }
    property IconSize: Integer read FIconSize write SetIconSize default 0;
    { The folder flags of the view }
    property Options: TExplorerViewOptions read FOptions write SetOptions default [];
    { The folder shown }
    property Path: string read GetPath write SetPath;
    { Show hidden and system files }
    property ShowHidden: Boolean read FShowHidden write SetShowHidden default False;
    { The canonical name of the property the items are sorted by, such as
      System.DateModified. When empty the folder chooses its sort order }
    property SortColumn: string read FSortColumn write SetSortColumn;
    { Sort from the largest to the smallest value }
    property SortDescending: Boolean read FSortDescending write SetSortDescending default False;
    property ViewMode: TExplorerViewMode read FViewMode write SetViewMode default evDetails;
    { Invoked after the shell context menu of the view is closed }
    property OnContextMenuClose: TNotifyEvent read FOnContextMenuClose write FOnContextMenuClose;
    { Invoked before the shell context menu of the view is shown }
    property OnContextMenuOpen: TNotifyEvent read FOnContextMenuOpen write FOnContextMenuOpen;
    { Invoked when an item is opened. Set Allow to false to stop the default action }
    property OnDefaultAction: TExplorerPathEvent read FOnDefaultAction write FOnDefaultAction;
    { Invoked for each item of a folder which passes the Filter. Set Allow to
      false to hide the item }
    property OnIncludeItem: TExplorerPathEvent read FOnIncludeItem write FOnIncludeItem;
    { Invoked before a folder is shown. Set Allow to false to stay in the current folder }
    property OnNavigate: TExplorerPathEvent read FOnNavigate write FOnNavigate;
    { Invoked after a folder is shown }
    property OnNavigated: TNotifyEvent read FOnNavigated write FOnNavigated;
    { Invoked when a folder could not be shown }
    property OnNavigationFailed: TExplorerFolderEvent read FOnNavigationFailed write FOnNavigationFailed;
    { Invoked when the selected items change }
    property OnSelectionChanged: TNotifyEvent read FOnSelectionChanged write FOnSelectionChanged;
    property Align;
    property Anchors;
    property BorderSpacing;
    property BorderStyle default bsNone;
    property Constraints;
    property DragCursor;
    property Enabled;
    property Hint;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnShowHint;
    property OnUTF8KeyPress;
  end;

{ TExplorerTree hosts the Windows namespace tree control, the navigation pane
  used by Windows Explorer }

  TExplorerTree = class(TExplorerControl, IComponentBindingClient,
    INameSpaceTreeControlEvents, IShellItemFilter)
  private
    FBinding: IComponentBinding;
    FChecks: TList;
    FControl: INameSpaceTreeControl;
    FCookie: DWORD;
    FOptions: TExplorerTreeOptions;
    FWindow: HWND;
    FRoot: string;
    FRoots: TStrings;
    FShowFiles: Boolean;
    FOnChange: TNotifyEvent;
    FOnCheck: TExplorerCheckEvent;
    FOnExpanded: TExplorerFolderEvent;
    FOnExpanding: TExplorerPathEvent;
    FOnIncludeItem: TExplorerPathEvent;
    procedure CreateTree;
    procedure DestroyTree;
    procedure AddRoots;
    function EnumFlags: SHCONTF;
    function GetChecked(const ItemPath: string): Boolean;
    procedure SetChecked(const ItemPath: string; Value: Boolean);
    function GetExpanded(const ItemPath: string): Boolean;
    procedure SetExpanded(const ItemPath: string; Value: Boolean);
    function GetSelectedItem: IShellItem;
    procedure SelectIDList(pidl: PItemIDList);
    function GetPath: string;
    procedure SetPath(const Value: string);
    procedure SetOptions(Value: TExplorerTreeOptions);
    procedure SetRoot(const Value: string);
    procedure SetRoots(Value: TStrings);
    procedure RootsChange(Sender: TObject);
    procedure SetShowFiles(Value: Boolean);
    procedure WMExplorerCheck(var Message: TLMessage); message WM_EXPLORERCHECK;
    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
    { IComponentBindingClient }
    function GetBinding: IComponentBinding;
    procedure SetBinding(Value: IComponentBinding);
    { INameSpaceTreeControlEvents }
    function OnItemClick(psi: IShellItem; nstceHitTest: DWORD; nstceClickType: DWORD): HResult; stdcall;
    function OnPropertyItemCommit(psi: IShellItem): HResult; stdcall;
    function OnItemStateChanging(psi: IShellItem; nstcisMask: DWORD; nstcisState: DWORD): HResult; stdcall;
    function OnItemStateChanged(psi: IShellItem; nstcisMask: DWORD; nstcisState: DWORD): HResult; stdcall;
    function OnSelectionChanged(psiaSelection: IShellItemArray): HResult; stdcall;
    function OnKeyboardInput(uMsg: UINT; wp: WPARAM; lp: LPARAM): HResult; stdcall;
    function OnBeforeExpand(psi: IShellItem): HResult; stdcall;
    function OnAfterExpand(psi: IShellItem): HResult; stdcall;
    function OnBeginLabelEdit(psi: IShellItem): HResult; stdcall;
    function OnEndLabelEdit(psi: IShellItem): HResult; stdcall;
    function OnGetToolTip(psi: IShellItem; pszTip: PWideChar; cchTip: Integer): HResult; stdcall;
    function OnBeforeItemDelete(psi: IShellItem): HResult; stdcall;
    function OnItemAdded(psi: IShellItem; fIsRoot: BOOL): HResult; stdcall;
    function OnItemDeleted(psi: IShellItem; fIsRoot: BOOL): HResult; stdcall;
    function OnBeforeContextMenu(psi: IShellItem; const riid: TGUID; out ppv): HResult; stdcall;
    function OnAfterContextMenu(psi: IShellItem; pcmIn: IContextMenu; const riid: TGUID;
      out ppv): HResult; stdcall;
    function OnBeforeStateImageChange(psi: IShellItem): HResult; stdcall;
    function OnGetDefaultIconIndex(psi: IShellItem; out piDefaultIcon: Integer;
      out piOpenIcon: Integer): HResult; stdcall;
    { IShellItemFilter }
    function IncludeItem(psi: IShellItem): HResult; stdcall;
    function GetEnumFlagsForItem(psi: IShellItem; pgrfFlags: PSHCONTF): HResult; stdcall;
  protected
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure Resize; override;
  public
    { Create a new explorer tree }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Collapse all the items }
    procedure CollapseAll;
    { Scroll the tree so an item is visible }
    procedure EnsureVisible(const ItemPath: string);
    { Get the paths of the checked items which the tree has loaded }
    procedure GetCheckedItems(Strings: TStrings);
    { Returns the path of the item at a point of the control, or an empty
      string if there is none }
    function ItemAtPos(X, Y: Integer): string;
    { Build the tree again }
    procedure Refresh; reintroduce;
    { The checkbox state of an item when the toCheckBoxes option is set }
    property Checked[const ItemPath: string]: Boolean read GetChecked write SetChecked;
    { The namespace tree control object, or nil if the window is not created }
    property Control: INameSpaceTreeControl read FControl;
    { The expanded state of an item }
    property Expanded[const ItemPath: string]: Boolean read GetExpanded write SetExpanded;
    { The selected folder }
    property Path: string read GetPath write SetPath;
  published
    { The styles of the tree. Changing them recreates the tree }
    property Options: TExplorerTreeOptions read FOptions write SetOptions default DefaultTreeOptions;
    { The folder at the top of the tree, which starts expanded. When empty it
      is the desktop }
    property Root: string read FRoot write SetRoot;
    { More folders shown at the top level after Root, such as This PC and
      Network }
    property Roots: TStrings read FRoots write SetRoots;
    { Show files as well as folders }
    property ShowFiles: Boolean read FShowFiles write SetShowFiles default False;
    { Invoked when the selected item changes }
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    { Invoked after the user changes the checkbox of an item }
    property OnCheck: TExplorerCheckEvent read FOnCheck write FOnCheck;
    { Invoked after a folder is expanded }
    property OnExpanded: TExplorerFolderEvent read FOnExpanded write FOnExpanded;
    { Invoked before a folder is expanded. Set Allow to false to stop it }
    property OnExpanding: TExplorerPathEvent read FOnExpanding write FOnExpanding;
    { Invoked for each item of the tree. Set Allow to false to hide the item }
    property OnIncludeItem: TExplorerPathEvent read FOnIncludeItem write FOnIncludeItem;
    property Align;
    property Anchors;
    property BorderSpacing;
    property BorderStyle default bsNone;
    property Constraints;
    property DragCursor;
    property Enabled;
    property Hint;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnShowHint;
    property OnUTF8KeyPress;
  end;

{ TExplorerEdit shows the path of a folder the way the address bar of Windows
  Explorer does. In emEdit mode the path is typed with the autocomplete of
  Windows, in emBreadcrumb mode each parent folder is a button with a menu of
  its subfolders, and in emDropDown mode a button opens a tree of folders.
  Typed paths can be file system paths, shell parsing names, environment
  variables, or the names the edit shows such as This PC\Documents }

  TExplorerEdit = class(TCustomControl, IComponentBindingClient)
  private
    FAutoComplete: Boolean;
    FBinding: IComponentBinding;
    FCloseTime: DWORD;
    FClosing: Boolean;
    FCrumbs: TList;
    FDropDownForm: TCustomForm;
    FDropDownHeight: Integer;
    FDropDownTree: TExplorerTree;
    FEdit: TCustomEdit;
    FEditing: Boolean;
    FEndingEdit: Boolean;
    FFirstArrow: TRect;
    FFolder: PItemIDList;
    FHiddenCount: Integer;
    FHotButton: Boolean;
    FHotIndex: Integer;
    FIcon: HICON;
    FMode: TExplorerEditMode;
    FMouseSelect: Boolean;
    FPath: string;
    FPressedArrow: Boolean;
    FPressedIndex: Integer;
    FText: string;
    FOnChange: TNotifyEvent;
    FOnCloseUp: TNotifyEvent;
    FOnDropDown: TNotifyEvent;
    FOnNavigate: TExplorerPathEvent;
    FOnNavigationFailed: TExplorerFolderEvent;
    procedure AcceptText;
    procedure CancelText;
    procedure ChangeFolder(pidl: PItemIDList; Navigate: Boolean);
    procedure ClearCrumbs;
    procedure BuildCrumbs;
    procedure DoCloseUp(Accept, Refocus: Boolean);
    procedure EditExit;
    procedure EndEdit;
    procedure FolderChanged;
    procedure HandleKey(var Key: Word; Shift: TShiftState);
    function HitTest(X, Y: Integer; out Index: Integer; out Arrow: Boolean): Boolean;
    function ButtonRect: TRect;
    function IconWidth: Integer;
    procedure LayoutCrumbs;
    procedure PaintCrumbs;
    procedure ShowCrumbMenu(Index: Integer);
    procedure ShowIDList(pidl: PItemIDList);
    procedure UpdateLayout;
    procedure DropDownChange(Sender: TObject);
    procedure DropDownDeactivate(Sender: TObject);
    procedure DropDownKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DropDownMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    function GetDroppedDown: Boolean;
    procedure SetDroppedDown(Value: Boolean);
    function GetPath: string;
    procedure SetPath(const Value: string);
    procedure SetAutoComplete(Value: Boolean);
    procedure SetMode(Value: TExplorerEditMode);
    procedure WMExplorerCloseUp(var Message: TLMessage); message WM_EXPLORERCLOSEUP;
    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;
    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
    { IComponentBindingClient }
    function GetBinding: IComponentBinding;
    procedure SetBinding(Value: IComponentBinding);
  protected
    procedure CreateWnd; override;
    procedure FontChanged(Sender: TObject); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure Resize; override;
  public
    { Create a new explorer edit }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Focus the edit and select its text. In emBreadcrumb mode the edit
      replaces the breadcrumb until it loses focus }
    procedure BeginEdit;
    { Open the drop down tree in emDropDown mode }
    procedure DropDown;
    { Close the drop down tree, moving to its selected folder if Accept is true }
    procedure CloseUp(Accept: Boolean = False);
    { Move to the parent folder }
    procedure GoUp;
    { The drop down tree is open }
    property DroppedDown: Boolean read GetDroppedDown write SetDroppedDown;
    { The edit which holds the typed path }
    property Edit: TCustomEdit read FEdit;
  published
    { Suggest folders while a path is typed }
    property AutoComplete: Boolean read FAutoComplete write SetAutoComplete default True;
    { The height in pixels of the drop down tree }
    property DropDownHeight: Integer read FDropDownHeight write FDropDownHeight default 300;
    { How the folder is shown }
    property Mode: TExplorerEditMode read FMode write SetMode default emEdit;
    { The folder shown }
    property Path: string read GetPath write SetPath;
    { Invoked after the folder changes }
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    { Invoked after the drop down tree is closed }
    property OnCloseUp: TNotifyEvent read FOnCloseUp write FOnCloseUp;
    { Invoked after the drop down tree is opened }
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
    { Invoked before the user moves to a folder. Set Allow to false to stay in
      the current folder }
    property OnNavigate: TExplorerPathEvent read FOnNavigate write FOnNavigate;
    { Invoked when typed text is not a folder. The text is restored to the
      current folder }
    property OnNavigationFailed: TExplorerFolderEvent read FOnNavigationFailed write FOnNavigationFailed;
    property Align;
    property Anchors;
    property BorderSpacing;
    property BorderStyle default bsSingle;
    property Color default clWindow;
    property Constraints;
    property DragCursor;
    property Enabled;
    property Font;
    property Hint;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnShowHint;
    property OnUTF8KeyPress;
  end;

{ TExplorerPreview shows a file using the preview handlers of Windows, the
  same previews shown in the preview pane of Windows Explorer. Items without
  a preview handler, such as images and folders, show their thumbnail or
  icon and details drawn in the font Windows Explorer uses for items. The
  details are the name, type, size, and date modified, along with the
  description and version of programs, the width and height of images, the
  duration and artist of audio, the codec, frame size, and duration of video,
  the owner of folders and the access the current user has to them, and the
  total size and free space of drives. Thumbnails are at most 256 by 256
  pixels. When it is bound to a TExplorerBinding it shows the item selected
  in a bound explorer view }

  TExplorerPreview = class(TCustomControl, IComponentBindingClient)
  private
    FBinding: IComponentBinding;
    FEmptyText: string;
    FFileName: string;
    FHandler: IPreviewHandler;
    FImage: HBITMAP;
    FInfoAccess: string;
    FInfoArtist: string;
    FInfoCodec: string;
    FInfoDate: string;
    FInfoDescription: string;
    FInfoDuration: string;
    FInfoFrameSize: string;
    FInfoFree: string;
    FInfoHeight: string;
    FInfoName: string;
    FInfoOwner: string;
    FInfoSize: string;
    FInfoTotal: string;
    FInfoType: string;
    FInfoValid: Boolean;
    FInfoVersion: string;
    FInfoWidth: string;
    FOnPreview: TExplorerPathEvent;
    FOnPreviewFailed: TExplorerFolderEvent;
    procedure ClearInfo;
    procedure LoadInfo(Item: IShellItem);
    procedure PaintInfo;
    procedure LoadPreview;
    procedure StartPreview;
    procedure Unload;
    procedure UpdateVisuals;
    function GetPreviewing: Boolean;
    procedure SetEmptyText(const Value: string);
    procedure SetFileName(const Value: string);
    procedure WMTimer(var Message: TLMessage); message LM_TIMER;
    { IComponentBindingClient }
    function GetBinding: IComponentBinding;
    procedure SetBinding(Value: IComponentBinding);
  protected
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure Paint; override;
    procedure Resize; override;
  public
    { Create a new explorer preview }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Remove the preview }
    procedure Clear;
    { Load the preview again }
    procedure Refresh; reintroduce;
    { The preview handler of the file shown, or nil if there is none }
    property Handler: IPreviewHandler read FHandler;
    { A preview is shown }
    property Previewing: Boolean read GetPreviewing;
  published
    { Text shown when there is no file or it cannot be found }
    property EmptyText: string read FEmptyText write SetEmptyText;
    { The file shown. Previews are loaded shortly after it changes, so moving
      quickly through files does not load each of them }
    property FileName: string read FFileName write SetFileName;
    { Invoked before a preview is loaded. Set Allow to false to show no preview }
    property OnPreview: TExplorerPathEvent read FOnPreview write FOnPreview;
    { Invoked when a file has no preview handler or its preview could not be
      shown. The thumbnail and details of the file are shown instead }
    property OnPreviewFailed: TExplorerFolderEvent read FOnPreviewFailed write FOnPreviewFailed;
    property Align;
    property Anchors;
    property BorderSpacing;
    property BorderStyle default bsNone;
    property Color default clWindow;
    property Constraints;
    property DragCursor;
    property Enabled;
    property Font;
    property Hint;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnShowHint;
  end;

{ TExplorerBinding links explorer views, trees, edits, and previews. Views,
  trees, and edits all show the same folder, and when one of them moves to a
  folder the others follow. Previews show the item selected in a view }

  TExplorerBinding = class(TComponentBinding)
  protected
    { Param is the absolute item list of the new folder }
    procedure DoChange(Sender: TObject; var Param); override;
  public
    { Create a new explorer binding }
    constructor Create(AOwner: TComponent); override;
    { Show a file in the bound previews. An empty file name clears them }
    procedure Preview(Sender: TObject; const FileName: string);
  end;

{ Returns the item list of a path, or nil if the path is not found. Paths
  are file system paths, shell parsing names such as ::{GUID, or known
  folders such as shell:Downloads or shell:Documents, and an empty path is
  the desktop. Free the result with CoTaskMemFree }
function ShellPathToIDList(const Path: string): PItemIDList;
{ Returns the path of an item list, which is its file system path or its
  shell parsing name. The desktop is the empty path }
function ShellIDListToPath(pidl: PItemIDList): string;
{$endif}

implementation

{$ifdef windows}
uses
  CommCtrl, ComObj, Menus, LCLType, ShellApi, Themes;

const
  { The most folders kept in the history of a view }
  MaxHistory = 100;

  ViewModes: array[TExplorerViewMode] of UINT = (FVM_ICON, FVM_SMALLICON,
    FVM_LIST, FVM_DETAILS, FVM_THUMBNAIL, FVM_TILE, FVM_THUMBSTRIP);

  ViewOptionFlags: array[TExplorerViewOption] of DWORD = (FWF_AUTOARRANGE,
    FWF_SNAPTOGRID, FWF_SINGLESEL, FWF_NOSUBFOLDERS, FWF_SHOWSELALWAYS,
    FWF_SINGLECLICKACTIVATE, FWF_HIDEFILENAMES, FWF_CHECKSELECT,
    FWF_AUTOCHECKSELECT, FWF_TRICHECKSELECT, FWF_FULLROWSELECT,
    FWF_NOGROUPING, FWF_NOFILTERS, FWF_NOCOLUMNHEADER, FWF_NOHEADERINALLVIEWS,
    FWF_EXTENDEDTILES);

  TreeOptionStyles: array[TExplorerTreeOption] of DWORD = (NSTCS_HASLINES,
    NSTCS_SINGLECLICKEXPAND, NSTCS_SPRINGEXPAND, NSTCS_FULLROWSELECT,
    NSTCS_SHOWSELECTIONALWAYS, NSTCS_HORIZONTALSCROLL, NSTCS_AUTOHSCROLL,
    NSTCS_FADEINOUTEXPANDOS, NSTCS_EVENHEIGHT, NSTCS_NOINFOTIP,
    NSTCS_NOEDITLABELS, NSTCS_DISABLEDRAGDROP, NSTCS_FAVORITESMODE,
    NSTCS_CHECKBOXES);

  { The styles a tree always has }
  TreeStyles = NSTCS_HASEXPANDOS or NSTCS_ROOTHASEXPANDO or NSTCS_ALLOWJUNCTIONS;

  { The custom states of a checkbox in a tree }
  UncheckedState = 1;
  CheckedState = 2;

{ Returns the item list of a known folder by its canonical name, such as
  Downloads or My Pictures. Common names such as Documents are accepted too }

function KnownFolderToIDList(const Name: string): PItemIDList;
const
  Aliases: array[0..3, 0..1] of string = (
    ('Documents', 'Personal'),
    ('Pictures', 'My Pictures'),
    ('Music', 'My Music'),
    ('Videos', 'My Video'));
var
  Manager: IKnownFolderManager;
  Folder: IKnownFolder;
  S: string;
  W: WideString;
  I: Integer;
begin
  Result := nil;
  S := Name;
  for I := Low(Aliases) to High(Aliases) do
    if SameText(S, Aliases[I, 0]) then
      S := Aliases[I, 1];
  if CoCreateInstance(CLSID_KnownFolderManager, nil, CLSCTX_INPROC_SERVER,
    IKnownFolderManager, Manager) <> S_OK then
    Exit;
  W := StrToWide(S);
  if Manager.GetFolderByName(PWideChar(W), Folder) <> S_OK then
    Exit;
  if Folder.GetIDList(0, Result) <> S_OK then
    Result := nil;
end;

function PathToIDList(const Path: string): PItemIDList;
const
  Prefix = 'shell:';
begin
  Result := nil;
  if Path = '' then
  begin
    if SHGetSpecialFolderLocation(0, CSIDL_DESKTOP, Result) <> S_OK then
      Result := nil;
  end
  else if SHParseDisplayName(PWideChar(StrToWide(Path)), nil, Result, 0, nil) <> S_OK then
  begin
    Result := nil;
    { Known folders such as shell:Downloads }
    if (Length(Path) > Length(Prefix)) and
      SameText(Copy(Path, 1, Length(Prefix)), Prefix) then
      Result := KnownFolderToIDList(Copy(Path, Length(Prefix) + 1, MaxInt));
  end;
end;

function ItemToIDList(Item: IShellItem): PItemIDList;
begin
  Result := nil;
  if (Item = nil) or (SHGetIDListFromObject(Item, Result) <> S_OK) then
    Result := nil;
end;

function IDListToPath(pidl: PItemIDList): string;
var
  Item: IShellItem;
  P: PWideChar;
begin
  Result := '';
  { The desktop is the empty path }
  if (pidl = nil) or (pidl.mkid.cb = 0) then
    Exit;
  if SHCreateItemFromIDList(pidl, IShellItem, Item) <> S_OK then
    Exit;
  P := nil;
  if Item.GetDisplayName(SIGDN(SIGDN_DESKTOPABSOLUTEPARSING), LPWSTR(@P)) = S_OK then
  begin
    Result := WideToStr(WideString(P));
    CoTaskMemFree(P);
  end;
end;

function ItemToPath(Item: IShellItem): string;
var
  pidl: PItemIDList;
begin
  pidl := ItemToIDList(Item);
  Result := IDListToPath(pidl);
  CoTaskMemFree(pidl);
end;

{ Returns the file system path of an item, or its parsing name if it has no
  file system path. Items in search results have their own parsing names }

function ItemToFileName(Item: IShellItem): string;
var
  P: PWideChar;
begin
  Result := '';
  if Item = nil then
    Exit;
  P := nil;
  if Item.GetDisplayName(SIGDN(SIGDN_FILESYSPATH), LPWSTR(@P)) = S_OK then
  begin
    Result := WideToStr(WideString(P));
    CoTaskMemFree(P);
  end
  else
    Result := ItemToPath(Item);
end;

function ShellPathToIDList(const Path: string): PItemIDList;
begin
  Result := PathToIDList(Path);
end;

function ShellIDListToPath(pidl: PItemIDList): string;
begin
  Result := IDListToPath(pidl);
end;

function PathToItem(const Path: string): IShellItem;
var
  pidl: PItemIDList;
begin
  Result := nil;
  pidl := PathToIDList(Path);
  if pidl = nil then
    Exit;
  if SHCreateItemFromIDList(pidl, IShellItem, Result) <> S_OK then
    Result := nil;
  CoTaskMemFree(pidl);
end;

function NameToPropertyKey(const Name: string; out Key: PROPERTYKEY): Boolean;
begin
  FillChar(Key, SizeOf(Key), #0);
  Result := PSGetPropertyKeyFromName(PWideChar(StrToWide(Trim(Name))), Key) = S_OK;
end;

{ Match a file name with a mask of * and ? characters, ignoring case }

function MatchesMask(const Name, Mask: string): Boolean;
var
  S, M: string;
  N, I, StarN, StarI: Integer;
begin
  S := LowerCase(Name);
  M := LowerCase(Mask);
  N := 1;
  I := 1;
  StarN := 0;
  StarI := 0;
  while N <= Length(S) do
    if (I <= Length(M)) and ((M[I] = '?') or (M[I] = S[N])) then
    begin
      Inc(N);
      Inc(I);
    end
    else if (I <= Length(M)) and (M[I] = '*') then
    begin
      StarI := I;
      StarN := N;
      Inc(I);
    end
    else if StarI > 0 then
    begin
      I := StarI + 1;
      Inc(StarN);
      N := StarN;
    end
    else
      Exit(False);
  while (I <= Length(M)) and (M[I] = '*') do
    Inc(I);
  Result := I > Length(M);
end;

{ Match a file name with masks separated by semicolons or commas }

function MatchesFilter(const Name, Filter: string): Boolean;
var
  Masks: TStringList;
  S: string;
  I: Integer;
begin
  Result := True;
  Masks := TStringList.Create;
  try
    Masks.Delimiter := ';';
    Masks.StrictDelimiter := True;
    Masks.DelimitedText := StringReplace(Filter, ',', ';', [rfReplaceAll]);
    for I := 0 to Masks.Count - 1 do
    begin
      S := Trim(Masks[I]);
      if S = '' then
        Continue;
      if MatchesMask(Name, S) then
        Exit(True);
      Result := False;
    end;
  finally
    Masks.Free;
  end;
end;

{ Folders always pass a filter, files pass when their name matches }

function ItemPassesFilter(pidl: PItemIDList; const Filter: string): Boolean;
var
  Item: IShellItem;
  Attributes: SFGAOF;
  P: PWideChar;
begin
  Result := True;
  if SHCreateItemFromIDList(pidl, IShellItem, Item) <> S_OK then
    Exit;
  Attributes := 0;
  Item.GetAttributes(SFGAO_FOLDER or SFGAO_STREAM, @Attributes);
  { A zip file is a folder and a stream, and is filtered as a file }
  if (Attributes and SFGAO_FOLDER <> 0) and (Attributes and SFGAO_STREAM = 0) then
    Exit;
  P := nil;
  if Item.GetDisplayName(SIGDN(SIGDN_PARENTRELATIVEPARSING), LPWSTR(@P)) = S_OK then
  begin
    Result := MatchesFilter(WideToStr(WideString(P)), Filter);
    CoTaskMemFree(P);
  end;
end;

function KeyShiftState: TShiftState;
begin
  Result := [];
  if GetKeyState(VK_SHIFT) < 0 then Include(Result, ssShift);
  if GetKeyState(VK_CONTROL) < 0 then Include(Result, ssCtrl);
  if GetKeyState(VK_MENU) < 0 then Include(Result, ssAlt);
end;

function MouseShiftState(WParam: Windows.WPARAM): TShiftState;
begin
  Result := [];
  if WParam and MK_SHIFT <> 0 then Include(Result, ssShift);
  if WParam and MK_CONTROL <> 0 then Include(Result, ssCtrl);
  if GetKeyState(VK_MENU) < 0 then Include(Result, ssAlt);
  if WParam and MK_LBUTTON <> 0 then Include(Result, ssLeft);
  if WParam and MK_RBUTTON <> 0 then Include(Result, ssRight);
  if WParam and MK_MBUTTON <> 0 then Include(Result, ssMiddle);
end;

{ The windows created by the shell inside an explorer control are subclassed
  so their input reaches the control. A CBT hook subclasses them as they are
  created, including the windows created when a view moves to another folder.
  The hook is installed while an explorer control has a window }

var
  HookedControls: TList;
  InputHook: HHOOK;

function FindHookedControl(Wnd: HWND): TExplorerControl;
var
  Control: TExplorerControl;
  I: Integer;
begin
  Result := nil;
  if (Wnd = 0) or (HookedControls = nil) then
    Exit;
  for I := 0 to HookedControls.Count - 1 do
  begin
    Control := TExplorerControl(HookedControls[I]);
    if Control.HandleAllocated and ((Control.Handle = Wnd) or
      IsChild(Control.Handle, Wnd)) then
      Exit(Control);
  end;
end;

function InputWindowProc(Wnd: HWND; Msg: UINT; WParam: Windows.WPARAM;
  LParam: Windows.LPARAM; Id: UINT_PTR; Data: DWORD_PTR): LRESULT; stdcall;
var
  Control: TExplorerControl;
  Handled: Boolean;
begin
  if Msg = WM_NCDESTROY then
  begin
    RemoveWindowSubclass(Wnd, InputWindowProc, Id);
    Exit(DefSubclassProc(Wnd, Msg, WParam, LParam));
  end;
  Result := 0;
  Handled := False;
  Control := TExplorerControl(Data);
  if (HookedControls <> nil) and (HookedControls.IndexOf(Control) > -1) then
    Result := Control.InputMessage(Wnd, Msg, WParam, LParam, Handled);
  if not Handled then
    Result := DefSubclassProc(Wnd, Msg, WParam, LParam);
end;

function InputHookProc(Code: Integer; WParam: Windows.WPARAM;
  LParam: Windows.LPARAM): LRESULT; stdcall;
var
  Control: TExplorerControl;
begin
  if Code = HCBT_CREATEWND then
  begin
    Control := FindHookedControl(PCBT_CREATEWND(LParam).lpcs.hwndParent);
    if Control <> nil then
      SetWindowSubclass(HWND(WParam), InputWindowProc, 1, DWORD_PTR(Control));
  end;
  Result := CallNextHookEx(InputHook, Code, WParam, LParam);
end;

function SubclassChildProc(Wnd: HWND; Param: Windows.LPARAM): BOOL; stdcall;
begin
  SetWindowSubclass(Wnd, InputWindowProc, 1, DWORD_PTR(Param));
  Result := True;
end;

function UnsubclassChildProc(Wnd: HWND; Param: Windows.LPARAM): BOOL; stdcall;
begin
  RemoveWindowSubclass(Wnd, InputWindowProc, 1);
  Result := True;
end;

procedure AddHookedControl(Control: TExplorerControl);
begin
  if HookedControls = nil then
    HookedControls := TList.Create;
  if HookedControls.IndexOf(Control) < 0 then
    HookedControls.Add(Control);
  if InputHook = 0 then
    InputHook := SetWindowsHookEx(WH_CBT, InputHookProc, 0, GetCurrentThreadId);
end;

procedure RemoveHookedControl(Control: TExplorerControl);
begin
  if HookedControls = nil then
    Exit;
  HookedControls.Remove(Control);
  if HookedControls.Count > 0 then
    Exit;
  if InputHook <> 0 then
    UnhookWindowsHookEx(InputHook);
  InputHook := 0;
  FreeAndNil(HookedControls);
end;

{ TExplorerControl }

destructor TExplorerControl.Destroy;
begin
  RemoveHookedControl(Self);
  inherited Destroy;
end;

procedure TExplorerControl.CreateWnd;
begin
  inherited CreateWnd;
  AddHookedControl(Self);
end;

procedure TExplorerControl.DestroyWnd;
begin
  if HandleAllocated then
    EnumChildWindows(Handle, UnsubclassChildProc, 0);
  RemoveHookedControl(Self);
  inherited DestroyWnd;
end;

procedure TExplorerControl.HookChildWindows;
begin
  if HandleAllocated then
    EnumChildWindows(Handle, SubclassChildProc, Windows.LPARAM(Self));
end;

function TExplorerControl.ShowContextPopup(MousePos: TPoint): Boolean;
var
  P: TPoint;
begin
  Result := False;
  DoContextPopup(MousePos, Result);
  if (not Result) and (PopupMenu <> nil) then
  begin
    if (MousePos.X = -1) and (MousePos.Y = -1) then
      P := Mouse.CursorPos
    else
      P := ClientToScreen(MousePos);
    PopupMenu.PopupComponent := Self;
    PopupMenu.PopUp(P.X, P.Y);
    Result := True;
  end;
end;

function TExplorerControl.InputMessage(Wnd: HWND; Msg: UINT; WParam: Windows.WPARAM;
  LParam: Windows.LPARAM; var Handled: Boolean): LRESULT;

  function MousePos: TPoint;
  begin
    Result := Point(SmallInt(LoWord(LParam)), SmallInt(HiWord(LParam)));
    MapWindowPoints(Wnd, Handle, Result, 1);
  end;

  function ScreenPos: TPoint;
  begin
    Result := ScreenToClient(Point(SmallInt(LoWord(LParam)), SmallInt(HiWord(LParam))));
  end;

var
  Form: TCustomForm;
  P: TPoint;
  Key: Word;
  C: Char;
  U: TUTF8Char;
begin
  Result := 0;
  { At design time the mouse passes to the control }
  if csDesigning in ComponentState then
  begin
    if Msg = WM_NCHITTEST then
    begin
      Handled := True;
      Result := HTTRANSPARENT;
    end;
    Exit;
  end;
  try
    case Msg of
      WM_SETFOCUS:
        begin
          { The form tracks the control as focused, invoking OnEnter }
          Form := GetParentForm(Self);
          if (Form <> nil) and (Form.ActiveControl <> Self) then
            Form.SetFocusedControl(Self);
        end;
      WM_LBUTTONDOWN:
        begin
          P := MousePos;
          MouseDown(mbLeft, MouseShiftState(WParam), P.X, P.Y);
        end;
      WM_RBUTTONDOWN:
        begin
          P := MousePos;
          MouseDown(mbRight, MouseShiftState(WParam), P.X, P.Y);
        end;
      WM_MBUTTONDOWN:
        begin
          P := MousePos;
          MouseDown(mbMiddle, MouseShiftState(WParam), P.X, P.Y);
        end;
      WM_LBUTTONDBLCLK:
        begin
          P := MousePos;
          MouseDown(mbLeft, MouseShiftState(WParam) + [ssDouble], P.X, P.Y);
          DblClick;
        end;
      WM_LBUTTONUP:
        begin
          P := MousePos;
          MouseUp(mbLeft, MouseShiftState(WParam), P.X, P.Y);
          Click;
        end;
      WM_RBUTTONUP:
        begin
          P := MousePos;
          MouseUp(mbRight, MouseShiftState(WParam), P.X, P.Y);
        end;
      WM_MBUTTONUP:
        begin
          P := MousePos;
          MouseUp(mbMiddle, MouseShiftState(WParam), P.X, P.Y);
        end;
      WM_MOUSEMOVE:
        begin
          P := MousePos;
          MouseMove(MouseShiftState(WParam), P.X, P.Y);
        end;
      WM_MOUSEWHEEL:
        Handled := DoMouseWheel(MouseShiftState(LoWord(WParam)),
          SmallInt(HiWord(WParam)), ScreenPos);
      WM_KEYDOWN, WM_SYSKEYDOWN:
        begin
          Key := Word(WParam);
          KeyDown(Key, KeyShiftState);
          Handled := Key = 0;
        end;
      WM_KEYUP, WM_SYSKEYUP:
        begin
          Key := Word(WParam);
          KeyUp(Key, KeyShiftState);
          Handled := Key = 0;
        end;
      WM_CHAR:
        { Surrogate pairs are passed on without events }
        if (WParam < $D800) or (WParam > $DFFF) then
        begin
          U := UTF8Encode(WideString(WideChar(WParam)));
          UTF8KeyPress(U);
          if U = '' then
            Handled := True
          else if Length(U) = 1 then
          begin
            C := U[1];
            KeyPress(C);
            Handled := C = #0;
          end;
        end;
      WM_CONTEXTMENU:
        { The message also travels to the parent windows }
        if not FInContextMenu then
        begin
          if (LoWord(LParam) = $FFFF) and (HiWord(LParam) = $FFFF) then
            P := Point(-1, -1)
          else
            P := ScreenPos;
          if ShowContextPopup(P) then
            Handled := True
          else
          begin
            FInContextMenu := True;
            try
              Result := DefSubclassProc(Wnd, Msg, WParam, LParam);
            finally
              FInContextMenu := False;
            end;
            Handled := True;
          end;
        end;
    end;
  except
    Application.HandleException(Self);
  end;
end;

{ TExplorerView }

constructor TExplorerView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 200;
  Height := 200;
  TabStop := True;
  FViewMode := evDetails;
  FColumns := TStringList.Create;
  TStringList(FColumns).OnChange := ColumnsChange;
  FHistory := TList.Create;
  FHistoryIndex := -1;
  FHistoryTarget := -1;
end;

destructor TExplorerView.Destroy;
begin
  DestroyBrowser;
  CoTaskMemFree(FPendingFolder);
  CoTaskMemFree(FFolder);
  CoTaskMemFree(FSearchScope);
  ClearHistory;
  if FHistoryIndex = 0 then
    CoTaskMemFree(FHistory[0]);
  FHistory.Free;
  FColumns.Free;
  inherited Destroy;
end;

procedure TExplorerView.CreateBrowser;
var
  Site: IObjectWithSite;
  Provider: IServiceProvider;
  Events: IExplorerBrowserEvents;
  Settings: TFolderSettings;
  R: TRect;
  pidl: PItemIDList;
begin
  if FBrowser <> nil then
    Exit;
  OleCheck(CoCreateInstance(CLSID_ExplorerBrowser, nil, CLSCTX_INPROC_SERVER,
    IExplorerBrowser, FBrowser));
  { The site supplies ICommDlgBrowser to the view }
  if Supports(FBrowser, IObjectWithSite, Site) then
  begin
    Provider := Self;
    Site.SetSite(Provider);
  end;
  FBrowser.SetOptions(EBO_NOBORDER);
  Settings.ViewMode := ViewModes[FViewMode];
  Settings.fFlags := FolderFlags;
  R := Rect(0, 0, ClientWidth, ClientHeight);
  OleCheck(FBrowser.Initialize(Handle, @R, @Settings));
  if FEmptyText <> '' then
    FBrowser.SetEmptyText(PWideChar(StrToWide(FEmptyText)));
  Events := Self;
  FBrowser.Advise(Events, FCookie);
  pidl := PathToIDList(FPath);
  if pidl = nil then
    pidl := PathToIDList('');
  try
    BrowseTo(pidl, SBSP_ABSOLUTE);
  finally
    CoTaskMemFree(pidl);
  end;
end;

procedure TExplorerView.DestroyBrowser;
var
  Site: IObjectWithSite;
begin
  if FBrowser = nil then
    Exit;
  if FCookie <> 0 then
    FBrowser.Unadvise(FCookie);
  FCookie := 0;
  if Supports(FBrowser, IObjectWithSite, Site) then
    Site.SetSite(nil);
  Site := nil;
  FBrowser.Destroy;
  FBrowser := nil;
end;

procedure TExplorerView.CreateWnd;
begin
  inherited CreateWnd;
  CreateBrowser;
  HookChildWindows;
end;

procedure TExplorerView.DestroyWnd;
begin
  { Keep the folder so it is shown again when the window is recreated }
  if FFolder <> nil then
    FPath := IDListToPath(FFolder);
  DestroyBrowser;
  inherited DestroyWnd;
end;

procedure TExplorerView.Resize;
begin
  inherited Resize;
  if FBrowser <> nil then
    FBrowser.SetRect(nil, Rect(0, 0, ClientWidth, ClientHeight));
end;

procedure TExplorerView.WMSetFocus(var Message: TLMSetFocus);
var
  View: IShellView;
  Wnd: HWND;
begin
  inherited;
  if csDesigning in ComponentState then
    Exit;
  View := GetShellView;
  Wnd := 0;
  if (View <> nil) and (View.GetWindow(Wnd) = S_OK) and (Wnd <> 0) then
    Windows.SetFocus(Wnd);
end;

procedure TExplorerView.BrowseTo(pidl: PItemIDList; Flags: UINT);
begin
  if FBrowser <> nil then
    FBrowser.BrowseToIDList(pidl, Flags);
end;

procedure TExplorerView.ShowIDList(pidl: PItemIDList);
begin
  if pidl = nil then
    Exit;
  if (FFolder <> nil) and ILIsEqual(FFolder, pidl) then
    Exit;
  if FBrowser = nil then
    FPath := IDListToPath(pidl)
  else
    BrowseTo(pidl, SBSP_ABSOLUTE);
end;

function TExplorerView.FolderFlags: DWORD;
var
  O: TExplorerViewOption;
begin
  Result := FWF_NOCLIENTEDGE;
  for O := Low(O) to High(O) do
    if O in FOptions then
      Result := Result or ViewOptionFlags[O];
end;

{ Apply the folder flags to the views created later and to the current view }

procedure TExplorerView.ApplyFolderFlags;
var
  Settings: TFolderSettings;
  View: IFolderView2;
  Mask: DWORD;
  O: TExplorerViewOption;
begin
  if FBrowser = nil then
    Exit;
  Settings.ViewMode := ViewModes[FViewMode];
  Settings.fFlags := FolderFlags;
  FBrowser.SetFolderSettings(@Settings);
  View := GetFolderView2;
  if View = nil then
    Exit;
  Mask := 0;
  for O := Low(O) to High(O) do
    Mask := Mask or ViewOptionFlags[O];
  View.SetCurrentFolderFlags(Mask, FolderFlags and Mask);
end;

procedure TExplorerView.ApplyViewMode;
var
  View2: IFolderView2;
  View: IFolderView;
begin
  View2 := GetFolderView2;
  if (View2 <> nil) and (FIconSize > 0) then
  begin
    View2.SetViewModeAndIconSize(ViewModes[FViewMode], FIconSize);
    Exit;
  end;
  View := GetFolderView;
  if View <> nil then
    View.SetCurrentViewMode(ViewModes[FViewMode]);
end;

procedure TExplorerView.ApplySort;
var
  View: IFolderView2;
  Column: TSortColumn;
begin
  if FSortColumn = '' then
    Exit;
  View := GetFolderView2;
  if View = nil then
    Exit;
  if not NameToPropertyKey(FSortColumn, Column.Key) then
    Exit;
  if FSortDescending then
    Column.Direction := SORT_DESCENDING
  else
    Column.Direction := SORT_ASCENDING;
  View.SetSortColumns(@Column, 1);
end;

{ An empty GroupBy removes the grouping of the current view }

procedure TExplorerView.ApplyGroupBy;
var
  View: IFolderView2;
  Key: PROPERTYKEY;
begin
  View := GetFolderView2;
  if View = nil then
    Exit;
  FillChar(Key, SizeOf(Key), #0);
  if (FGroupBy = '') or NameToPropertyKey(FGroupBy, Key) then
    View.SetGroupBy(@Key, True);
end;

procedure TExplorerView.ApplyColumns;
var
  Manager: IColumnManager;
  Keys: array of PROPERTYKEY;
  Widths: array of Integer;
  Info: TCMColumnInfo;
  S, N: string;
  I, C, P: Integer;
begin
  if FColumns.Count = 0 then
    Exit;
  if not Supports(GetShellView, IColumnManager, Manager) then
    Exit;
  SetLength(Keys, FColumns.Count);
  SetLength(Widths, FColumns.Count);
  C := 0;
  for I := 0 to FColumns.Count - 1 do
  begin
    S := Trim(FColumns[I]);
    P := Pos('=', S);
    if P > 0 then
    begin
      N := Trim(System.Copy(S, 1, P - 1));
      Widths[C] := StrToIntDef(Trim(System.Copy(S, P + 1, Length(S))), 0);
    end
    else
    begin
      N := S;
      Widths[C] := 0;
    end;
    if (N <> '') and NameToPropertyKey(N, Keys[C]) then
      Inc(C);
  end;
  if C = 0 then
    Exit;
  Manager.SetColumns(@Keys[0], C);
  for I := 0 to C - 1 do
    if Widths[I] > 0 then
    begin
      FillChar(Info, SizeOf(Info), #0);
      Info.cbSize := SizeOf(Info);
      Info.dwMask := CM_MASK_WIDTH;
      Info.uWidth := Widths[I];
      Manager.SetColumnInfo(@Keys[I], @Info);
    end;
end;

procedure TExplorerView.ColumnsChange(Sender: TObject);
begin
  ApplyColumns;
end;

function TExplorerView.GetBinding: IComponentBinding;
begin
  Result := FBinding;
end;

procedure TExplorerView.SetBinding(Value: IComponentBinding);
begin
  FBinding := Value;
end;

function TExplorerView.GetShellView: IShellView;
begin
  Result := nil;
  if FBrowser <> nil then
    if FBrowser.GetCurrentView(IShellView, Result) <> S_OK then
      Result := nil;
end;

function TExplorerView.GetFolderView: IFolderView;
begin
  if not Supports(GetShellView, IFolderView, Result) then
    Result := nil;
end;

function TExplorerView.GetFolderView2: IFolderView2;
begin
  if not Supports(GetShellView, IFolderView2, Result) then
    Result := nil;
end;

function TExplorerView.CountItems(Target: UINT): Integer;
var
  View: IFolderView;
begin
  Result := 0;
  View := GetFolderView;
  if (View = nil) or (View.ItemCount(Target, @Result) <> S_OK) then
    Result := 0;
end;

function TExplorerView.GetItemCount: Integer;
begin
  Result := CountItems(SVGIO_ALLVIEW);
end;

function TExplorerView.GetSelectedCount: Integer;
begin
  Result := CountItems(SVGIO_SELECTION);
end;

function TExplorerView.GetFocusedItem: string;
var
  View: IFolderView;
  I: Integer;
  L, A: PItemIDList;
begin
  Result := '';
  View := GetFolderView;
  if (View = nil) or (FFolder = nil) then
    Exit;
  I := -1;
  L := nil;
  if (View.GetFocussedItem(@I) = S_OK) and (I > -1) and
    (View.Item(I, PItemIDList(@L)) = S_OK) and (L <> nil) then
  begin
    A := ILCombine(FFolder, L);
    Result := IDListToPath(A);
    CoTaskMemFree(A);
    CoTaskMemFree(L);
  end;
end;

function TExplorerView.GetPath: string;
begin
  Result := FPath;
end;

procedure TExplorerView.SetPath(const Value: string);
var
  pidl: PItemIDList;
begin
  { The path is kept as it is given, such as shell:Downloads, while it names
    the folder shown }
  FPath := Value;
  if FBrowser = nil then
    Exit;
  pidl := PathToIDList(Value);
  if pidl = nil then
    Exit;
  try
    BrowseTo(pidl, SBSP_ABSOLUTE);
  finally
    CoTaskMemFree(pidl);
  end;
end;

procedure TExplorerView.SetColumns(Value: TStrings);
begin
  FColumns.Assign(Value);
end;

procedure TExplorerView.SetEmptyText(const Value: string);
begin
  if Value = FEmptyText then
    Exit;
  FEmptyText := Value;
  if FBrowser <> nil then
    FBrowser.SetEmptyText(PWideChar(StrToWide(FEmptyText)));
end;

procedure TExplorerView.SetFilter(const Value: string);
begin
  if Value = FFilter then
    Exit;
  FFilter := Value;
  Refresh;
end;

procedure TExplorerView.SetGroupBy(const Value: string);
begin
  if Value = FGroupBy then
    Exit;
  FGroupBy := Value;
  ApplyGroupBy;
end;

procedure TExplorerView.SetIconSize(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if Value = FIconSize then
    Exit;
  FIconSize := Value;
  ApplyViewMode;
end;

procedure TExplorerView.SetOptions(Value: TExplorerViewOptions);
begin
  if Value = FOptions then
    Exit;
  FOptions := Value;
  ApplyFolderFlags;
end;

procedure TExplorerView.SetShowHidden(Value: Boolean);
begin
  if Value = FShowHidden then
    Exit;
  FShowHidden := Value;
  Refresh;
end;

procedure TExplorerView.SetSortColumn(const Value: string);
begin
  if Value = FSortColumn then
    Exit;
  FSortColumn := Value;
  ApplySort;
end;

procedure TExplorerView.SetSortDescending(Value: Boolean);
begin
  if Value = FSortDescending then
    Exit;
  FSortDescending := Value;
  ApplySort;
end;

procedure TExplorerView.SetViewMode(Value: TExplorerViewMode);
begin
  FViewMode := Value;
  if FBrowser = nil then
    Exit;
  ApplyFolderFlags;
  ApplyViewMode;
end;

procedure TExplorerView.GoBack;
begin
  if GetCanGoBack then
    GoToHistory(FHistoryIndex - 1);
end;

procedure TExplorerView.GoForward;
begin
  if GetCanGoForward then
    GoToHistory(FHistoryIndex + 1);
end;

procedure TExplorerView.GoToHistory(Index: Integer);
begin
  if (Index < 0) or (Index >= FHistory.Count) or (Index = FHistoryIndex) then
    Exit;
  FHistoryTarget := Index;
  BrowseTo(FHistory[Index], SBSP_ABSOLUTE);
end;

procedure TExplorerView.ClearHistory;
var
  Current: Pointer;
  I: Integer;
begin
  Current := nil;
  for I := 0 to FHistory.Count - 1 do
    if I = FHistoryIndex then
      Current := FHistory[I]
    else
      CoTaskMemFree(FHistory[I]);
  FHistory.Clear;
  FHistoryIndex := -1;
  if Current <> nil then
  begin
    FHistory.Add(Current);
    FHistoryIndex := 0;
  end;
end;

function TExplorerView.GetCanGoBack: Boolean;
begin
  Result := FHistoryIndex > 0;
end;

function TExplorerView.GetCanGoForward: Boolean;
begin
  Result := FHistoryIndex < FHistory.Count - 1;
end;

function TExplorerView.GetHistory(Index: Integer): string;
begin
  Result := IDListToPath(FHistory[Index]);
end;

function TExplorerView.GetHistoryCount: Integer;
begin
  Result := FHistory.Count;
end;

procedure TExplorerView.GoUp;
begin
  BrowseTo(nil, SBSP_PARENT);
end;

procedure TExplorerView.Refresh;
var
  View: IShellView;
begin
  View := GetShellView;
  if View <> nil then
    View.Refresh;
end;

{ Invoke a verb on the selected items, or on the folder when Background is
  true or no items are selected }

procedure TExplorerView.InvokeVerb(const Verb: string; Background: Boolean);
var
  View: IShellView;
  ContextMenu: IContextMenu;
  CommandInfo: TCMInvokeCommandInfo;
  Menu: HMENU;
  P: PPointer;
  S: AnsiString;
  Target: UINT;
begin
  S := AnsiString(Trim(Verb));
  if S = '' then
    Exit;
  View := GetShellView;
  if View = nil then
    Exit;
  if (not Background) and (GetSelectedCount > 0) then
    Target := SVGIO_SELECTION
  else
    Target := SVGIO_BACKGROUND;
  P := nil;
  if View.GetItemObject(Target, IContextMenu, P) <> S_OK then
    Exit;
  { GetItemObject returns an interface which already has a reference }
  Pointer(ContextMenu) := P;
  { Some menus only accept verbs after the menu items have been added }
  Menu := Windows.CreatePopupMenu;
  try
    ContextMenu.QueryContextMenu(Menu, 0, 1, $7FFF, CMF_NORMAL);
    FillChar(CommandInfo, SizeOf(CommandInfo), #0);
    CommandInfo.cbSize := SizeOf(CommandInfo);
    CommandInfo.hwnd := Handle;
    CommandInfo.lpVerb := PAnsiChar(S);
    CommandInfo.nShow := SW_SHOWNORMAL;
    ContextMenu.InvokeCommand(CommandInfo);
  finally
    Windows.DestroyMenu(Menu);
  end;
end;

procedure TExplorerView.ExecuteVerb(const Verb: string);
begin
  if SameText(Trim(Verb), 'rename') then
    Rename
  else
    InvokeVerb(LowerCase(Verb), False);
end;

procedure TExplorerView.Rename;
var
  View: IFolderView2;
  Folder: IFolderView;
  I: Integer;
begin
  View := GetFolderView2;
  if (View <> nil) and (View.DoRename = S_OK) then
    Exit;
  Folder := GetFolderView;
  if Folder = nil then
    Exit;
  I := -1;
  if (Folder.GetFocussedItem(@I) = S_OK) and (I > -1) then
    Folder.SelectItem(I, SVSI_EDIT or SVSI_DESELECTOTHERS or
      SVSI_ENSUREVISIBLE or SVSI_FOCUSED);
end;

procedure TExplorerView.Copy;
begin
  ExecuteVerb('copy');
end;

procedure TExplorerView.Cut;
begin
  ExecuteVerb('cut');
end;

procedure TExplorerView.Paste;
begin
  ExecuteVerb('paste');
end;

procedure TExplorerView.Delete;
begin
  ExecuteVerb('delete');
end;

procedure TExplorerView.Properties;
begin
  ExecuteVerb('properties');
end;

procedure TExplorerView.NewFolder;
begin
  InvokeVerb('NewFolder', True);
end;

procedure TExplorerView.Select(const ItemPath: string);
var
  View: IShellView;
  pidl: PItemIDList;
begin
  View := GetShellView;
  if (View = nil) or (FFolder = nil) then
    Exit;
  pidl := PathToIDList(ItemPath);
  if pidl = nil then
    Exit;
  try
    { Only the items of the folder shown can be selected }
    if ILIsParent(FFolder, pidl, True) then
      View.SelectItem(ILFindLastID(pidl), SVSI_SELECT or SVSI_DESELECTOTHERS or
        SVSI_ENSUREVISIBLE or SVSI_FOCUSED);
  finally
    CoTaskMemFree(pidl);
  end;
end;

procedure TExplorerView.SelectAll;
var
  View: IFolderView;
  I, C: Integer;
begin
  View := GetFolderView;
  if View = nil then
    Exit;
  C := CountItems(SVGIO_ALLVIEW);
  for I := 0 to C - 1 do
    View.SelectItem(I, SVSI_SELECT);
end;

procedure TExplorerView.ClearSelection;
var
  View: IShellView;
begin
  View := GetShellView;
  if View <> nil then
    View.SelectItem(nil, SVSI_DESELECTOTHERS);
end;

procedure TExplorerView.Search(const Text: string; const Scope: string = '');
var
  ScopeList, pidl: PItemIDList;
  Item: IShellItem;
  Items: IShellItemArray;
  Conditions: IConditionFactory2;
  Condition: IUnknown;
  Factory: ISearchFolderItemFactory;
  Key: PROPERTYKEY;
  Operation: Integer;
  P: PWideChar;
  S: string;
  W: WideString;
begin
  if FBrowser = nil then
    Exit;
  if Trim(Text) = '' then
  begin
    if FSearching and (FSearchScope <> nil) then
      BrowseTo(FSearchScope, SBSP_ABSOLUTE);
    Exit;
  end;
  if Scope <> '' then
    ScopeList := PathToIDList(Scope)
  else if FSearching and (FSearchScope <> nil) then
    ScopeList := ILClone(FSearchScope)
  else if FFolder <> nil then
    ScopeList := ILClone(FFolder)
  else
    ScopeList := nil;
  if ScopeList = nil then
    Exit;
  try
    OleCheck(SHCreateItemFromIDList(ScopeList, IShellItem, Item));
    OleCheck(SHCreateShellItemArrayFromShellItem(Item, IShellItemArray, Items));
    { A condition on the name of items, using wildcards if the text has them }
    if not NameToPropertyKey('System.ItemNameDisplay', Key) then
      Exit;
    if (Pos('*', Text) > 0) or (Pos('?', Text) > 0) then
      Operation := COP_DOSWILDCARDS
    else
      Operation := COP_VALUE_CONTAINS;
    OleCheck(CoCreateInstance(CLSID_ConditionFactory, nil, CLSCTX_INPROC_SERVER,
      IConditionFactory2, Conditions));
    W := StrToWide(Text);
    OleCheck(Conditions.CreateStringLeaf(@Key, Operation, PWideChar(W), nil,
      CONDITION_CREATION_DEFAULT, IID_ICondition, Condition));
    OleCheck(CoCreateInstance(CLSID_SearchFolderItemFactory, nil,
      CLSCTX_INPROC_SERVER, ISearchFolderItemFactory, Factory));
    S := '';
    P := nil;
    if Item.GetDisplayName(SIGDN(SIGDN_NORMALDISPLAY), LPWSTR(@P)) = S_OK then
    begin
      S := WideToStr(WideString(P));
      CoTaskMemFree(P);
    end;
    W := StrToWide('Search Results in ' + S);
    OleCheck(Factory.SetDisplayName(PWideChar(W)));
    OleCheck(Factory.SetScope(Items));
    OleCheck(Factory.SetCondition(Condition));
    pidl := nil;
    OleCheck(Factory.GetIDList(pidl));
    try
      CoTaskMemFree(FSearchScope);
      FSearchScope := ScopeList;
      ScopeList := nil;
      FSearchText := Text;
      FSearchPending := True;
      BrowseTo(pidl, SBSP_ABSOLUTE);
    finally
      CoTaskMemFree(pidl);
    end;
  finally
    CoTaskMemFree(ScopeList);
  end;
end;

procedure TExplorerView.SelectionChanged;
var
  Folder: IFolderView2;
  Items: IShellItemArray;
  Item: IShellItem;
  FileName: string;
  C: DWORD;
begin
  { Bound previews show the selected item when exactly one is selected }
  if (FBinding = nil) or not (FBinding.Instance is TExplorerBinding) then
    Exit;
  FileName := '';
  Folder := GetFolderView2;
  C := 0;
  if (Folder <> nil) and (Folder.GetSelection(False, Items) = S_OK) and
    (Items <> nil) and (Items.GetCount(C) = S_OK) and (C = 1) and
    (Items.GetItemAt(0, Item) = S_OK) then
    FileName := ItemToFileName(Item);
  TExplorerBinding(FBinding.Instance).Preview(Self, FileName);
end;

procedure TExplorerView.GetItems(Strings: TStrings; Selection: Boolean = True);
var
  Folder: IFolderView;
  Enum: IEnumIDList;
  P: Pointer;
  Target: UINT;
  A, B: PItemIDList;
  I: ULONG;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    Folder := GetFolderView;
    if (Folder = nil) or (FFolder = nil) then
      Exit;
    if Selection then
      Target := SVGIO_SELECTION
    else
      Target := SVGIO_ALLVIEW;
    P := nil;
    if Folder.Items(Target, IEnumIDList, P) <> S_OK then
      Exit;
    { Items returns an interface which already has a reference }
    Pointer(Enum) := P;
    while (Enum.Next(1, A, I) = S_OK) and (I = 1) do
    begin
      B := ILCombine(FFolder, A);
      Strings.Add(IDListToPath(B));
      CoTaskMemFree(B);
      CoTaskMemFree(A);
    end;
  finally
    Strings.EndUpdate;
  end;
end;

{ TExplorerView.IServiceProvider }

function TExplorerView.QueryService(const rsid, iid: TGuid; out Obj): HResult;
begin
  Pointer(Obj) := nil;
  Result := E_NOINTERFACE;
  if IsEqualGUID(rsid, SID_SExplorerBrowserFrame) then
    if GetInterface(iid, Obj) then
      Result := S_OK;
end;

{ TExplorerView.ICommDlgBrowser }

function TExplorerView.OnDefaultCommand(const ppshv: IShellView): HResult;
var
  Allow: Boolean;
begin
  Allow := True;
  if Assigned(FOnDefaultAction) then
  try
    FOnDefaultAction(Self, GetFocusedItem, Allow);
  except
    Application.HandleException(Self);
  end;
  { S_FALSE lets the view perform the default action }
  if Allow then
    Result := S_FALSE
  else
    Result := S_OK;
end;

function TExplorerView.OnStateChange(const ppshv: IShellView; Change: ULONG): HResult;
begin
  if Change = CDBOSC_SELCHANGE then
  try
    SelectionChanged;
    if Assigned(FOnSelectionChanged) then
      FOnSelectionChanged(Self);
  except
    Application.HandleException(Self);
  end;
  Result := S_OK;
end;

function TExplorerView.IncludeObject(const ppshv: IShellView; pidl: PItemIDList): HResult;
var
  Allow: Boolean;
  Folder, A: PItemIDList;
begin
  Allow := True;
  if (FFilter <> '') or Assigned(FOnIncludeItem) then
  try
    { Items are enumerated before the navigation completes }
    Folder := FPendingFolder;
    if Folder = nil then
      Folder := FFolder;
    if Folder <> nil then
    begin
      A := ILCombine(Folder, pidl);
      try
        if FFilter <> '' then
          Allow := ItemPassesFilter(A, FFilter);
        if Allow and Assigned(FOnIncludeItem) then
          FOnIncludeItem(Self, IDListToPath(A), Allow);
      finally
        CoTaskMemFree(A);
      end;
    end;
  except
    Application.HandleException(Self);
  end;
  if Allow then
    Result := S_OK
  else
    Result := S_FALSE;
end;

{ TExplorerView.ICommDlgBrowser2 }

function TExplorerView.Notify(ppshv: IShellView; dwNotifyType: DWORD): HResult;
begin
  Result := S_OK;
  try
    if dwNotifyType = CDB2N_CONTEXTMENU_START then
    begin
      if Assigned(FOnContextMenuOpen) then
        FOnContextMenuOpen(Self);
    end
    else if dwNotifyType = CDB2N_CONTEXTMENU_DONE then
      if Assigned(FOnContextMenuClose) then
        FOnContextMenuClose(Self);
  except
    Application.HandleException(Self);
  end;
end;

function TExplorerView.GetDefaultMenuText(ppshv: IShellView; pszText: PWideChar;
  cchMax: Integer): HResult;
begin
  { S_FALSE keeps the normal text of the default menu item }
  Result := S_FALSE;
end;

function TExplorerView.GetViewFlags(out pdwFlags: DWORD): HResult;
begin
  if FShowHidden then
    pdwFlags := CDB2GVF_SHOWALLFILES
  else
    pdwFlags := 0;
  Result := S_OK;
end;

{ TExplorerView.IExplorerBrowserEvents }

function TExplorerView.OnNavigationPending(pidlFolder: PItemIDList): HResult;
var
  Allow: Boolean;
begin
  Allow := True;
  if Assigned(FOnNavigate) then
  try
    FOnNavigate(Self, IDListToPath(pidlFolder), Allow);
  except
    Application.HandleException(Self);
    Allow := False;
  end;
  if Allow then
  begin
    CoTaskMemFree(FPendingFolder);
    FPendingFolder := ILClone(pidlFolder);
    Result := S_OK;
  end
  else
    Result := E_FAIL;
end;

function TExplorerView.OnViewCreated(psv: IShellView): HResult;
begin
  Result := S_OK;
end;

function TExplorerView.OnNavigationComplete(pidlFolder: PItemIDList): HResult;
var
  Folder: IFolderView;
  M: UINT;
  V: TExplorerViewMode;
  P: PItemIDList;
begin
  Result := S_OK;
  CoTaskMemFree(FPendingFolder);
  FPendingFolder := nil;
  CoTaskMemFree(FFolder);
  FFolder := ILClone(pidlFolder);
  FSearching := FSearchPending;
  FSearchPending := False;
  if not FSearching then
    FSearchText := '';
  { Keep the path as it was given while it names the folder shown }
  P := PathToIDList(FPath);
  if (P = nil) or not ILIsEqual(P, FFolder) then
    FPath := IDListToPath(FFolder);
  CoTaskMemFree(P);
  { Record the folder in the history. Going back or forward with the keys of
    the view is recognized as moving to the folder before or after }
  if FHistoryTarget >= 0 then
    FHistoryIndex := FHistoryTarget
  else if (FHistoryIndex > 0) and ILIsEqual(FHistory[FHistoryIndex - 1], FFolder) then
    Dec(FHistoryIndex)
  else if (FHistoryIndex < FHistory.Count - 1) and
    ILIsEqual(FHistory[FHistoryIndex + 1], FFolder) then
    Inc(FHistoryIndex)
  else if (FHistoryIndex < 0) or not ILIsEqual(FHistory[FHistoryIndex], FFolder) then
  begin
    while FHistory.Count > FHistoryIndex + 1 do
    begin
      CoTaskMemFree(FHistory[FHistory.Count - 1]);
      FHistory.Delete(FHistory.Count - 1);
    end;
    FHistory.Add(ILClone(FFolder));
    if FHistory.Count > MaxHistory then
    begin
      CoTaskMemFree(FHistory[0]);
      FHistory.Delete(0);
    end;
    FHistoryIndex := FHistory.Count - 1;
  end;
  FHistoryTarget := -1;
  HookChildWindows;
  { The folder may have its own saved view mode }
  Folder := GetFolderView;
  if (Folder <> nil) and (Folder.GetCurrentViewMode(@M) = S_OK) then
    for V := Low(V) to High(V) do
      if ViewModes[V] = M then
      begin
        FViewMode := V;
        Break;
      end;
  { The properties which are set replace the saved state of the folder }
  if FIconSize > 0 then
    ApplyViewMode;
  ApplySort;
  if FGroupBy <> '' then
    ApplyGroupBy;
  ApplyColumns;
  try
    if FBinding <> nil then
    begin
      P := FFolder;
      FBinding.Change(Self, P);
    end;
    if Assigned(FOnNavigated) then
      FOnNavigated(Self);
  except
    Application.HandleException(Self);
  end;
end;

function TExplorerView.NavigationFailed(pidlFolder: PItemIDList): HResult;
begin
  CoTaskMemFree(FPendingFolder);
  FPendingFolder := nil;
  FSearchPending := False;
  FHistoryTarget := -1;
  { A path which could not be shown is replaced by the folder shown }
  if FFolder <> nil then
    FPath := IDListToPath(FFolder);
  Result := S_OK;
  if Assigned(FOnNavigationFailed) then
  try
    FOnNavigationFailed(Self, IDListToPath(pidlFolder));
  except
    Application.HandleException(Self);
  end;
end;

{ TExplorerTree }

constructor TExplorerTree.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 200;
  Height := 200;
  TabStop := True;
  FOptions := DefaultTreeOptions;
  FChecks := TList.Create;
  FRoots := TStringList.Create;
  TStringList(FRoots).OnChange := RootsChange;
end;

destructor TExplorerTree.Destroy;
var
  I: Integer;
begin
  DestroyTree;
  for I := 0 to FChecks.Count - 1 do
    CoTaskMemFree(FChecks[I]);
  FChecks.Free;
  FRoots.Free;
  inherited Destroy;
end;

procedure TExplorerTree.CreateTree;
var
  Events: INameSpaceTreeControlEvents;
  Window: IOleWindow;
  Styles: DWORD;
  O: TExplorerTreeOption;
  R: TRect;
begin
  if FControl <> nil then
    Exit;
  OleCheck(CoCreateInstance(CLSID_NamespaceTreeControl, nil, CLSCTX_INPROC_SERVER,
    INameSpaceTreeControl, FControl));
  Styles := TreeStyles;
  for O := Low(O) to High(O) do
    if O in FOptions then
      Styles := Styles or TreeOptionStyles[O];
  R := Rect(0, 0, ClientWidth, ClientHeight);
  OleCheck(FControl.Initialize(Handle, @R, Styles));
  Events := Self;
  FControl.TreeAdvise(Events, FCookie);
  FWindow := 0;
  if Supports(FControl, IOleWindow, Window) then
    Window.GetWindow(FWindow);
  AddRoots;
  HookChildWindows;
end;

procedure TExplorerTree.DestroyTree;
begin
  if FControl = nil then
    Exit;
  if FCookie <> 0 then
    FControl.TreeUnadvise(FCookie);
  FCookie := 0;
  FControl.RemoveAllRoots;
  if IsWindow(FWindow) then
    DestroyWindow(FWindow);
  FWindow := 0;
  FControl := nil;
end;

function TExplorerTree.EnumFlags: SHCONTF;
begin
  Result := SHCONTF_FOLDERS;
  if FShowFiles then
    Result := Result or SHCONTF_NONFOLDERS;
end;

procedure TExplorerTree.AddRoots;
var
  Filter: IShellItemFilter;
  Item: IShellItem;
  S: string;
  I: Integer;
begin
  if FControl = nil then
    Exit;
  FControl.RemoveAllRoots;
  Filter := Self;
  Item := PathToItem(FRoot);
  if Item = nil then
    Item := PathToItem('');
  if Item <> nil then
    FControl.AppendRoot(Item, EnumFlags, NSTCRS_EXPANDED, Filter);
  for I := 0 to FRoots.Count - 1 do
  begin
    S := Trim(FRoots[I]);
    if S = '' then
      Continue;
    Item := PathToItem(S);
    if Item <> nil then
      FControl.AppendRoot(Item, EnumFlags, NSTCRS_VISIBLE, Filter);
  end;
end;

procedure TExplorerTree.CreateWnd;
begin
  inherited CreateWnd;
  CreateTree;
end;

procedure TExplorerTree.DestroyWnd;
begin
  DestroyTree;
  inherited DestroyWnd;
end;

procedure TExplorerTree.Resize;
begin
  inherited Resize;
  if FWindow <> 0 then
    SetWindowPos(FWindow, 0, 0, 0, ClientWidth, ClientHeight,
      SWP_NOZORDER or SWP_NOACTIVATE);
end;

procedure TExplorerTree.WMSetFocus(var Message: TLMSetFocus);
begin
  inherited;
  if csDesigning in ComponentState then
    Exit;
  if FWindow <> 0 then
    Windows.SetFocus(FWindow);
end;

{ The checkbox of an item has changed after OnBeforeStateImageChange, so the
  new states are read when the posted message arrives }

procedure TExplorerTree.WMExplorerCheck(var Message: TLMessage);
var
  Item: IShellItem;
  pidl: PItemIDList;
  State: Integer;
  I: Integer;
begin
  try
    for I := 0 to FChecks.Count - 1 do
    begin
      pidl := FChecks[I];
      State := 0;
      if (FControl <> nil) and Assigned(FOnCheck) and
        (SHCreateItemFromIDList(pidl, IShellItem, Item) = S_OK) and
        (FControl.GetItemCustomState(Item, State) = S_OK) then
        FOnCheck(Self, IDListToPath(pidl), State = CheckedState);
      Item := nil;
    end;
  finally
    for I := 0 to FChecks.Count - 1 do
      CoTaskMemFree(FChecks[I]);
    FChecks.Clear;
  end;
end;

procedure TExplorerTree.CollapseAll;
begin
  if FControl <> nil then
    FControl.CollapseAll;
end;

procedure TExplorerTree.EnsureVisible(const ItemPath: string);
var
  Item: IShellItem;
begin
  if FControl = nil then
    Exit;
  Item := PathToItem(ItemPath);
  if Item <> nil then
    FControl.EnsureItemVisible(Item);
end;

procedure TExplorerTree.GetCheckedItems(Strings: TStrings);

  procedure Visit(Item: IShellItem);
  var
    Child, Next: IShellItem;
    State: Integer;
  begin
    State := 0;
    if (FControl.GetItemCustomState(Item, State) = S_OK) and (State = CheckedState) then
      Strings.Add(ItemToPath(Item));
    Child := nil;
    if FControl.GetNextItem(Item, NSTCGNI_CHILD, Child) <> S_OK then
      Exit;
    while Child <> nil do
    begin
      Visit(Child);
      Next := nil;
      if FControl.GetNextItem(Child, NSTCGNI_NEXT, Next) <> S_OK then
        Next := nil;
      Child := Next;
    end;
  end;

var
  Items: IShellItemArray;
  Item: IShellItem;
  C: DWORD;
  I: Integer;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    if (FControl = nil) or (FControl.GetRootItems(Items) <> S_OK) or (Items = nil) then
      Exit;
    C := 0;
    if Items.GetCount(C) <> S_OK then
      Exit;
    for I := 0 to Integer(C) - 1 do
    begin
      Item := nil;
      if Items.GetItemAt(I, Item) = S_OK then
        Visit(Item);
    end;
  finally
    Strings.EndUpdate;
  end;
end;

function TExplorerTree.ItemAtPos(X, Y: Integer): string;
var
  Item: IShellItem;
  P: TPoint;
begin
  Result := '';
  if (FControl = nil) or (FWindow = 0) then
    Exit;
  P := Point(X, Y);
  MapWindowPoints(Handle, FWindow, P, 1);
  if FControl.HitTest(@P, Item) = S_OK then
    Result := ItemToPath(Item);
end;

procedure TExplorerTree.Refresh;
begin
  AddRoots;
end;

function TExplorerTree.GetChecked(const ItemPath: string): Boolean;
var
  Item: IShellItem;
  State: Integer;
begin
  Result := False;
  if FControl = nil then
    Exit;
  Item := PathToItem(ItemPath);
  State := 0;
  if (Item <> nil) and (FControl.GetItemCustomState(Item, State) = S_OK) then
    Result := State = CheckedState;
end;

procedure TExplorerTree.SetChecked(const ItemPath: string; Value: Boolean);
var
  Item: IShellItem;
begin
  if FControl = nil then
    Exit;
  Item := PathToItem(ItemPath);
  if Item = nil then
    Exit;
  if Value then
    FControl.SetItemCustomState(Item, CheckedState)
  else
    FControl.SetItemCustomState(Item, UncheckedState);
end;

function TExplorerTree.GetExpanded(const ItemPath: string): Boolean;
var
  Item: IShellItem;
  State: DWORD;
begin
  Result := False;
  if FControl = nil then
    Exit;
  Item := PathToItem(ItemPath);
  State := 0;
  if (Item <> nil) and (FControl.GetItemState(Item, NSTCIS_EXPANDED, State) = S_OK) then
    Result := State and NSTCIS_EXPANDED <> 0;
end;

procedure TExplorerTree.SetExpanded(const ItemPath: string; Value: Boolean);
var
  Item: IShellItem;
begin
  if FControl = nil then
    Exit;
  Item := PathToItem(ItemPath);
  if Item = nil then
    Exit;
  if Value then
    FControl.SetItemState(Item, NSTCIS_EXPANDED, NSTCIS_EXPANDED)
  else
    FControl.SetItemState(Item, NSTCIS_EXPANDED, 0);
end;

function TExplorerTree.GetSelectedItem: IShellItem;
var
  Items: IShellItemArray;
  Item: IShellItem;
  C: DWORD;
begin
  Result := nil;
  if FControl = nil then
    Exit;
  C := 0;
  if (FControl.GetSelectedItems(Items) = S_OK) and (Items <> nil) and
    (Items.GetCount(C) = S_OK) and (C > 0) and
    (Items.GetItemAt(0, Item) = S_OK) then
    Result := Item;
end;

procedure TExplorerTree.SelectIDList(pidl: PItemIDList);
var
  Item: IShellItem;
  Selected: PItemIDList;
  Same: Boolean;
begin
  if (FControl = nil) or (pidl = nil) then
    Exit;
  Selected := ItemToIDList(GetSelectedItem);
  Same := (Selected <> nil) and ILIsEqual(Selected, pidl);
  CoTaskMemFree(Selected);
  if Same then
    Exit;
  if SHCreateItemFromIDList(pidl, IShellItem, Item) <> S_OK then
    Exit;
  if FControl.SetItemState(Item, NSTCIS_SELECTED, NSTCIS_SELECTED) = S_OK then
    FControl.EnsureItemVisible(Item);
end;

function TExplorerTree.GetPath: string;
begin
  Result := ItemToPath(GetSelectedItem);
end;

procedure TExplorerTree.SetPath(const Value: string);
var
  pidl: PItemIDList;
begin
  pidl := PathToIDList(Value);
  try
    SelectIDList(pidl);
  finally
    CoTaskMemFree(pidl);
  end;
end;

procedure TExplorerTree.SetOptions(Value: TExplorerTreeOptions);
begin
  if Value = FOptions then
    Exit;
  FOptions := Value;
  { The styles are only read when the tree is created }
  if FControl <> nil then
  begin
    DestroyTree;
    CreateTree;
  end;
end;

procedure TExplorerTree.SetRoot(const Value: string);
var
  pidl: PItemIDList;
begin
  if Value <> FRoot then
  begin
    FRoot := Value;
    AddRoots;
    { The other bound components move to the new root. This is not done
      when the tree is created, so a view keeps its own Path when a form
      is loaded }
    if FBinding <> nil then
    begin
      pidl := PathToIDList(FRoot);
      if pidl = nil then
        pidl := PathToIDList('');
      try
        if pidl <> nil then
          FBinding.Change(Self, pidl);
      finally
        CoTaskMemFree(pidl);
      end;
    end;
  end;
end;

procedure TExplorerTree.SetRoots(Value: TStrings);
begin
  FRoots.Assign(Value);
end;

procedure TExplorerTree.RootsChange(Sender: TObject);
begin
  AddRoots;
end;

procedure TExplorerTree.SetShowFiles(Value: Boolean);
begin
  if Value <> FShowFiles then
  begin
    FShowFiles := Value;
    AddRoots;
  end;
end;

function TExplorerTree.GetBinding: IComponentBinding;
begin
  Result := FBinding;
end;

procedure TExplorerTree.SetBinding(Value: IComponentBinding);
begin
  FBinding := Value;
end;

{ TExplorerTree.INameSpaceTreeControlEvents }

function TExplorerTree.OnItemClick(psi: IShellItem; nstceHitTest: DWORD;
  nstceClickType: DWORD): HResult;
begin
  Result := S_FALSE;
end;

function TExplorerTree.OnPropertyItemCommit(psi: IShellItem): HResult;
begin
  Result := S_FALSE;
end;

function TExplorerTree.OnItemStateChanging(psi: IShellItem; nstcisMask: DWORD;
  nstcisState: DWORD): HResult;
begin
  Result := S_OK;
end;

function TExplorerTree.OnItemStateChanged(psi: IShellItem; nstcisMask: DWORD;
  nstcisState: DWORD): HResult;
begin
  Result := S_OK;
end;

function TExplorerTree.OnSelectionChanged(psiaSelection: IShellItemArray): HResult;
var
  Item: IShellItem;
  pidl: PItemIDList;
  C: DWORD;
begin
  Result := S_OK;
  try
    if (FBinding <> nil) and (psiaSelection <> nil) then
    begin
      C := 0;
      if (psiaSelection.GetCount(C) = S_OK) and (C > 0) and
        (psiaSelection.GetItemAt(0, Item) = S_OK) then
      begin
        pidl := ItemToIDList(Item);
        try
          if pidl <> nil then
            FBinding.Change(Self, pidl);
        finally
          CoTaskMemFree(pidl);
        end;
      end;
    end;
    if Assigned(FOnChange) then
      FOnChange(Self);
  except
    Application.HandleException(Self);
  end;
end;

function TExplorerTree.OnKeyboardInput(uMsg: UINT; wp: WPARAM; lp: LPARAM): HResult;
begin
  { The keys reach OnKeyDown and OnKeyUp through the hooked tree window, and
    S_FALSE lets the tree handle them }
  Result := S_FALSE;
end;

function TExplorerTree.OnBeforeExpand(psi: IShellItem): HResult;
var
  Allow: Boolean;
begin
  Result := S_OK;
  if Assigned(FOnExpanding) then
  try
    Allow := True;
    FOnExpanding(Self, ItemToPath(psi), Allow);
    { A failure stops the folder from expanding }
    if not Allow then
      Result := E_ABORT;
  except
    Application.HandleException(Self);
  end;
end;

function TExplorerTree.OnAfterExpand(psi: IShellItem): HResult;
begin
  Result := S_OK;
  if Assigned(FOnExpanded) then
  try
    FOnExpanded(Self, ItemToPath(psi));
  except
    Application.HandleException(Self);
  end;
end;

function TExplorerTree.OnBeginLabelEdit(psi: IShellItem): HResult;
begin
  Result := S_OK;
end;

function TExplorerTree.OnEndLabelEdit(psi: IShellItem): HResult;
begin
  Result := S_OK;
end;

function TExplorerTree.OnGetToolTip(psi: IShellItem; pszTip: PWideChar;
  cchTip: Integer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TExplorerTree.OnBeforeItemDelete(psi: IShellItem): HResult;
begin
  Result := S_OK;
end;

function TExplorerTree.OnItemAdded(psi: IShellItem; fIsRoot: BOOL): HResult;
begin
  Result := S_OK;
end;

function TExplorerTree.OnItemDeleted(psi: IShellItem; fIsRoot: BOOL): HResult;
begin
  Result := S_OK;
end;

{ The tree may build its menu without a WM_CONTEXTMENU message, so
  OnContextPopup and PopupMenu are handled here too }

function TExplorerTree.OnBeforeContextMenu(psi: IShellItem; const riid: TGUID;
  out ppv): HResult;
begin
  Pointer(ppv) := nil;
  { E_NOTIMPL lets the tree build the shell menu }
  Result := E_NOTIMPL;
  if InContextMenu or (csDesigning in ComponentState) then
    Exit;
  try
    { A failure stops the shell menu }
    if ShowContextPopup(ScreenToClient(Mouse.CursorPos)) then
      Result := E_ABORT;
  except
    Application.HandleException(Self);
  end;
end;

function TExplorerTree.OnAfterContextMenu(psi: IShellItem; pcmIn: IContextMenu;
  const riid: TGUID; out ppv): HResult;
begin
  { Keep the menu built by the tree }
  if pcmIn = nil then
  begin
    Pointer(ppv) := nil;
    Result := E_NOTIMPL;
  end
  else
    Result := pcmIn.QueryInterface(riid, ppv);
end;

function TExplorerTree.OnBeforeStateImageChange(psi: IShellItem): HResult;
var
  pidl: PItemIDList;
begin
  Result := S_OK;
  if (not Assigned(FOnCheck)) or (not HandleAllocated) then
    Exit;
  pidl := ItemToIDList(psi);
  if pidl = nil then
    Exit;
  FChecks.Add(pidl);
  PostMessage(Handle, WM_EXPLORERCHECK, 0, 0);
end;

function TExplorerTree.OnGetDefaultIconIndex(psi: IShellItem; out piDefaultIcon: Integer;
  out piOpenIcon: Integer): HResult;
begin
  piDefaultIcon := 0;
  piOpenIcon := 0;
  Result := E_NOTIMPL;
end;

{ TExplorerTree.IShellItemFilter }

function TExplorerTree.IncludeItem(psi: IShellItem): HResult;
var
  Allow: Boolean;
begin
  Result := S_OK;
  if not Assigned(FOnIncludeItem) then
    Exit;
  Allow := True;
  try
    FOnIncludeItem(Self, ItemToPath(psi), Allow);
  except
    Application.HandleException(Self);
  end;
  if not Allow then
    Result := S_FALSE;
end;

function TExplorerTree.GetEnumFlagsForItem(psi: IShellItem; pgrfFlags: PSHCONTF): HResult;
begin
  if pgrfFlags <> nil then
    pgrfFlags^ := EnumFlags;
  Result := S_OK;
end;

{ Explorer edit helpers }

const
  NoCrumb = -2;

type
  { TCrumb is a folder button of a breadcrumb }

  TCrumb = class
  public
    IDList: PItemIDList;
    Name: string;
    Visible: Boolean;
    TextRect: TRect;
    ArrowRect: TRect;
    destructor Destroy; override;
  end;

  { TPathEdit is the edit of a TExplorerEdit. Its keys pass through the
    explorer edit first }

  TPathEdit = class(TCustomEdit)
  private
    FExplorer: TExplorerEdit;
  protected
    procedure CreateWnd; override;
    procedure DoExit; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure Resize; override;
    procedure UTF8KeyPress(var UTF8Key: TUTF8Char); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

destructor TCrumb.Destroy;
begin
  CoTaskMemFree(IDList);
  inherited Destroy;
end;

constructor TPathEdit.Create(AOwner: TComponent);
begin
  FExplorer := AOwner as TExplorerEdit;
  inherited Create(AOwner);
  BorderStyle := bsNone;
  ParentColor := True;
end;

procedure TPathEdit.CreateWnd;
begin
  inherited CreateWnd;
  if FExplorer.FAutoComplete and not (csDesigning in FExplorer.ComponentState) then
    SHAutoComplete(Handle, SHACF_FILESYS_DIRS or SHACF_AUTOSUGGEST_FORCE_ON or
      SHACF_AUTOAPPEND_FORCE_OFF);
end;

procedure TPathEdit.DoExit;
begin
  inherited DoExit;
  FExplorer.EditExit;
end;

procedure TPathEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  FExplorer.KeyDown(Key, Shift);
  if Key <> 0 then
    inherited KeyDown(Key, Shift);
end;

procedure TPathEdit.KeyPress(var Key: Char);
begin
  FExplorer.KeyPress(Key);
  { Stop the beep of Enter and Escape }
  if Key in [#13, #27] then
    Key := #0;
  if Key <> #0 then
    inherited KeyPress(Key);
end;

procedure TPathEdit.KeyUp(var Key: Word; Shift: TShiftState);
begin
  FExplorer.KeyUp(Key, Shift);
  if Key <> 0 then
    inherited KeyUp(Key, Shift);
end;

procedure TPathEdit.Resize;
begin
  inherited Resize;
  { The height follows the font. Resize can be invoked while the edit is
    being created }
  if (FExplorer <> nil) and (FExplorer.FEdit = Self) then
    FExplorer.UpdateLayout;
end;

procedure TPathEdit.UTF8KeyPress(var UTF8Key: TUTF8Char);
begin
  FExplorer.UTF8KeyPress(UTF8Key);
  if UTF8Key <> '' then
    inherited UTF8KeyPress(UTF8Key);
end;

function ScaleValue(Value: Integer): Integer;
begin
  Result := MulDiv(Value, Screen.PixelsPerInch, 96);
end;

function IDListToName(pidl: PItemIDList; Kind: DWORD): string;
var
  Item: IShellItem;
  P: PWideChar;
begin
  Result := '';
  if pidl = nil then
    Exit;
  if SHCreateItemFromIDList(pidl, IShellItem, Item) <> S_OK then
    Exit;
  P := nil;
  if Item.GetDisplayName(SIGDN(Kind), LPWSTR(@P)) = S_OK then
  begin
    Result := WideToStr(WideString(P));
    CoTaskMemFree(P);
  end;
end;

function IsFolderIDList(pidl: PItemIDList): Boolean;
var
  Item: IShellItem;
  Attributes: SFGAOF;
begin
  Result := False;
  if (pidl = nil) or (SHCreateItemFromIDList(pidl, IShellItem, Item) <> S_OK) then
    Exit;
  Attributes := 0;
  Item.GetAttributes(SFGAO_FOLDER, @Attributes);
  Result := (Attributes and SFGAO_FOLDER) <> 0;
end;

function BindFolder(pidl: PItemIDList): IShellFolder;
var
  Desktop: IShellFolder;
begin
  Result := nil;
  if SHGetDesktopFolder(Desktop) <> S_OK then
    Exit;
  if (pidl = nil) or (pidl.mkid.cb = 0) then
    Result := Desktop
  else if Desktop.BindToObject(pidl, nil, IShellFolder, Result) <> S_OK then
    Result := nil;
end;

{ Add the absolute item lists of the subfolders of a folder }

procedure AddSubfolders(Parent: PItemIDList; Items: TList);
var
  Folder: IShellFolder;
  Enum: IEnumIDList;
  Child: PItemIDList;
  Fetched: ULONG;
begin
  Folder := BindFolder(Parent);
  if Folder = nil then
    Exit;
  if (Folder.EnumObjects(0, SHCONTF_FOLDERS, Enum) <> S_OK) or (Enum = nil) then
    Exit;
  Fetched := 0;
  while Enum.Next(1, Child, Fetched) = S_OK do
  begin
    Items.Add(ILCombine(Parent, Child));
    CoTaskMemFree(Child);
  end;
end;

{ Returns the absolute item list of a child of a folder by its parsing name
  or by the name shown for it }

function FindChild(Parent: PItemIDList; const Name: string): PItemIDList;
var
  Folder: IShellFolder;
  Enum: IEnumIDList;
  Child: PItemIDList;
  Eaten, Attributes, Fetched: ULONG;
  W: WideString;
begin
  Result := nil;
  Folder := BindFolder(Parent);
  if Folder = nil then
    Exit;
  W := StrToWide(Name);
  Child := nil;
  Attributes := 0;
  if (Folder.ParseDisplayName(0, nil, PWideChar(W), Eaten, Child,
    Attributes) = S_OK) and (Child <> nil) then
  begin
    Result := ILCombine(Parent, Child);
    CoTaskMemFree(Child);
    Exit;
  end;
  if (Folder.EnumObjects(0, SHCONTF_FOLDERS or SHCONTF_INCLUDEHIDDEN,
    Enum) <> S_OK) or (Enum = nil) then
    Exit;
  Fetched := 0;
  while Enum.Next(1, Child, Fetched) = S_OK do
  begin
    Result := ILCombine(Parent, Child);
    CoTaskMemFree(Child);
    if WideSameText(StrToWide(IDListToName(Result, SIGDN_NORMALDISPLAY)), W) or
      WideSameText(StrToWide(IDListToName(Result, SIGDN_PARENTRELATIVEEDITING)), W) then
      Exit;
    CoTaskMemFree(Result);
    Result := nil;
  end;
end;

{ Returns the item list of a path of names separated by backslashes,
  starting at a folder }

function NamesToIDList(Start: PItemIDList; const Text: string): PItemIDList;
var
  Name: string;
  Child: PItemIDList;
  I: Integer;
begin
  Result := nil;
  if Start = nil then
    Exit;
  Result := ILClone(Start);
  Name := '';
  for I := 1 to Length(Text) + 1 do
    if (I > Length(Text)) or (Text[I] = '\') then
    begin
      Name := Trim(Name);
      if Name <> '' then
      begin
        Child := FindChild(Result, Name);
        CoTaskMemFree(Result);
        Result := Child;
        if Result = nil then
          Exit;
      end;
      Name := '';
    end
    else
      Name := Name + Text[I];
end;

function ExpandPath(const S: string): string;
var
  W, R: WideString;
  N: DWORD;
begin
  Result := S;
  if Pos('%', S) = 0 then
    Exit;
  W := StrToWide(S);
  N := ExpandEnvironmentStringsW(PWideChar(W), nil, 0);
  if N = 0 then
    Exit;
  SetLength(R, N);
  N := ExpandEnvironmentStringsW(PWideChar(W), PWideChar(R), N);
  if N = 0 then
    Exit;
  SetLength(R, N - 1);
  Result := WideToStr(R);
end;

{ Returns the item list of the folder named by typed text, or nil if the text
  does not name a folder. Names are looked for in the current folder and then
  in the desktop }

function ParsePath(const Text: string; Current: PItemIDList): PItemIDList;
var
  S: string;
  Desktop: PItemIDList;
begin
  Result := nil;
  S := ExpandPath(Trim(Text));
  if S = '' then
    Exit;
  Result := PathToIDList(S);
  if Result = nil then
    Result := NamesToIDList(Current, S);
  if Result = nil then
  begin
    Desktop := PathToIDList('');
    Result := NamesToIDList(Desktop, S);
    CoTaskMemFree(Desktop);
  end;
  if (Result <> nil) and not IsFolderIDList(Result) then
  begin
    CoTaskMemFree(Result);
    Result := nil;
  end;
end;

{ Prepares a 32 bit bitmap from a shell item image factory for AlphaBlend and
  menus, which need its colors premultiplied by alpha. Icons can come back
  with colors which are not premultiplied, and pictures can come back with
  no alpha at all }

procedure PremultiplyBitmap(Bitmap: HBITMAP);
type
  TPixel = packed record
    B, G, R, A: Byte;
  end;
  PPixel = ^TPixel;
var
  Section: TDIBSection;
  P: PPixel;
  Count, I: Integer;
  Straight, Transparent: Boolean;
begin
  if Bitmap = 0 then
    Exit;
  FillChar(Section, SizeOf(Section), #0);
  if GetObject(Bitmap, SizeOf(Section), @Section) <> SizeOf(Section) then
    Exit;
  if (Section.dsBm.bmBitsPixel <> 32) or (Section.dsBm.bmBits = nil) then
    Exit;
  GdiFlush;
  Count := Section.dsBm.bmWidth * Abs(Section.dsBm.bmHeight);
  Straight := False;
  Transparent := True;
  P := Section.dsBm.bmBits;
  for I := 1 to Count do
  begin
    if P.A <> 0 then
      Transparent := False;
    if (P.R > P.A) or (P.G > P.A) or (P.B > P.A) then
      Straight := True;
    Inc(P);
  end;
  if not (Straight or Transparent) then
    Exit;
  P := Section.dsBm.bmBits;
  for I := 1 to Count do
  begin
    if Transparent then
      { A bitmap with no alpha is opaque }
      P.A := 255
    else
    begin
      P.R := P.R * P.A div 255;
      P.G := P.G * P.A div 255;
      P.B := P.B * P.A div 255;
    end;
    Inc(P);
  end;
end;

{ Returns a small icon of an item as a bitmap with alpha, which menus can
  show }

function ItemBitmap(pidl: PItemIDList): HBITMAP;
var
  Factory: IShellItemImageFactory;
  Size: Windows.TSize;
begin
  Result := 0;
  if SHCreateItemFromIDList(pidl, IShellItemImageFactory, Factory) <> S_OK then
    Exit;
  Size.cx := GetSystemMetrics(SM_CXSMICON);
  Size.cy := GetSystemMetrics(SM_CYSMICON);
  if Factory.GetImage(Size, SIIGBF_ICONONLY, Result) <> S_OK then
    Result := 0;
  PremultiplyBitmap(Result);
end;

{ TExplorerEdit }

constructor TExplorerEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption] + [csOpaque];
  Width := 300;
  Height := 25;
  BorderStyle := bsSingle;
  Color := clWindow;
  TabStop := True;
  FAutoComplete := True;
  FDropDownHeight := 300;
  FHotIndex := NoCrumb;
  FPressedIndex := NoCrumb;
  FCrumbs := TList.Create;
  FEdit := TPathEdit.Create(Self);
  FEdit.Parent := Self;
  FFolder := PathToIDList('');
  FolderChanged;
end;

destructor TExplorerEdit.Destroy;
begin
  if FDropDownForm <> nil then
    FDropDownForm.OnDeactivate := nil;
  ClearCrumbs;
  FCrumbs.Free;
  FCrumbs := nil;
  if FIcon <> 0 then
    DestroyIcon(FIcon);
  CoTaskMemFree(FFolder);
  FFolder := nil;
  inherited Destroy;
end;

procedure TExplorerEdit.CreateWnd;
begin
  inherited CreateWnd;
  UpdateLayout;
end;

procedure TExplorerEdit.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);
  UpdateLayout;
  Invalidate;
end;

procedure TExplorerEdit.Resize;
begin
  inherited Resize;
  UpdateLayout;
  Invalidate;
end;

function TExplorerEdit.IconWidth: Integer;
begin
  Result := GetSystemMetrics(SM_CXSMICON) + ScaleValue(8);
end;

function TExplorerEdit.ButtonRect: TRect;
begin
  Result := Rect(ClientWidth - GetSystemMetrics(SM_CXVSCROLL), 0, ClientWidth,
    ClientHeight);
end;

procedure TExplorerEdit.UpdateLayout;
var
  L, R, H: Integer;
begin
  if (FEdit = nil) or (csDestroying in ComponentState) then
    Exit;
  L := IconWidth;
  R := ClientWidth;
  if FMode = emDropDown then
    Dec(R, GetSystemMetrics(SM_CXVSCROLL));
  H := FEdit.Height;
  if R < L then
    R := L;
  FEdit.SetBounds(L, (ClientHeight - H) div 2, R - L, H);
  FEdit.Visible := (FMode <> emBreadcrumb) or FEditing;
end;

procedure TExplorerEdit.ClearCrumbs;
var
  I: Integer;
begin
  if FCrumbs = nil then
    Exit;
  for I := 0 to FCrumbs.Count - 1 do
    TObject(FCrumbs[I]).Free;
  FCrumbs.Clear;
  FHiddenCount := 0;
end;

procedure TExplorerEdit.BuildCrumbs;
var
  Full: PItemIDList;
  Crumb: TCrumb;
begin
  ClearCrumbs;
  if FFolder = nil then
    Exit;
  Full := ILClone(FFolder);
  try
    { The desktop is only a crumb when it is the folder }
    repeat
      Crumb := TCrumb.Create;
      Crumb.IDList := ILClone(Full);
      Crumb.Name := IDListToName(Full, SIGDN_NORMALDISPLAY);
      FCrumbs.Insert(0, Crumb);
    until (Full.mkid.cb = 0) or (not ILRemoveLastID(Full)) or (Full.mkid.cb = 0);
  finally
    CoTaskMemFree(Full);
  end;
end;

procedure TExplorerEdit.FolderChanged;
var
  Info: TSHFileInfoW;
begin
  if FIcon <> 0 then
    DestroyIcon(FIcon);
  FIcon := 0;
  FillChar(Info, SizeOf(Info), #0);
  if SHGetFileInfoW(LPCWSTR(FFolder), 0, Info, SizeOf(Info),
    SHGFI_PIDL or SHGFI_ICON or SHGFI_SMALLICON) <> 0 then
    FIcon := Info.hIcon;
  FText := IDListToName(FFolder, SIGDN_DESKTOPABSOLUTEEDITING);
  FEdit.Text := FText;
  BuildCrumbs;
  Invalidate;
end;

procedure TExplorerEdit.ChangeFolder(pidl: PItemIDList; Navigate: Boolean);
var
  Allow: Boolean;
  P: PItemIDList;
begin
  if pidl = nil then
    Exit;
  if (FFolder <> nil) and ILIsEqual(FFolder, pidl) then
  begin
    FEdit.Text := FText;
    Exit;
  end;
  if Navigate and Assigned(FOnNavigate) then
  begin
    Allow := True;
    FOnNavigate(Self, IDListToPath(pidl), Allow);
    if not Allow then
    begin
      FEdit.Text := FText;
      Exit;
    end;
  end;
  P := ILClone(pidl);
  CoTaskMemFree(FFolder);
  FFolder := P;
  FPath := '';
  FolderChanged;
  if (FBinding <> nil) and not (csLoading in ComponentState) then
  begin
    P := FFolder;
    FBinding.Change(Self, P);
  end;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TExplorerEdit.ShowIDList(pidl: PItemIDList);
begin
  ChangeFolder(pidl, False);
end;

procedure TExplorerEdit.AcceptText;
var
  S: string;
  pidl: PItemIDList;
begin
  S := Trim(FEdit.Text);
  if S = FText then
  begin
    EndEdit;
    FEdit.SelectAll;
    Exit;
  end;
  pidl := ParsePath(S, FFolder);
  if pidl = nil then
  begin
    FEdit.Text := FText;
    FEdit.SelectAll;
    if Assigned(FOnNavigationFailed) then
      FOnNavigationFailed(Self, S);
    Exit;
  end;
  try
    ChangeFolder(pidl, True);
  finally
    CoTaskMemFree(pidl);
  end;
  EndEdit;
  FEdit.SelectAll;
end;

procedure TExplorerEdit.CancelText;
begin
  FEdit.Text := FText;
  FEdit.SelectAll;
  EndEdit;
end;

procedure TExplorerEdit.BeginEdit;
begin
  if csDesigning in ComponentState then
    Exit;
  if (FMode = emBreadcrumb) and not FEditing then
  begin
    FEditing := True;
    FHotIndex := NoCrumb;
    UpdateLayout;
    Invalidate;
  end;
  if FEdit.CanFocus then
    FEdit.SetFocus;
  FEdit.SelectAll;
end;

procedure TExplorerEdit.EndEdit;
begin
  FEdit.Text := FText;
  if not FEditing then
    Exit;
  FEditing := False;
  { Keep the focus on the control when the edit is hidden }
  if FEdit.Focused and CanFocus then
  begin
    FEndingEdit := True;
    try
      SetFocus;
    finally
      FEndingEdit := False;
    end;
  end;
  UpdateLayout;
  Invalidate;
end;

procedure TExplorerEdit.EditExit;
begin
  if not (csDestroying in ComponentState) then
    EndEdit;
end;

procedure TExplorerEdit.HandleKey(var Key: Word; Shift: TShiftState);
begin
  if FEdit.Focused then
    case Key of
      VK_RETURN:
        begin
          AcceptText;
          Key := 0;
        end;
      VK_ESCAPE:
        { An unchanged edit lets Escape pass on, such as to close a dialog }
        if FEditing or (FEdit.Text <> FText) then
        begin
          CancelText;
          Key := 0;
        end;
      VK_DOWN, VK_F4:
        if (FMode = emDropDown) and ((Key = VK_F4) or (ssAlt in Shift)) then
        begin
          DropDown;
          Key := 0;
        end;
    end
  else if (FMode = emBreadcrumb) and (Key in [VK_RETURN, VK_SPACE, VK_F4]) then
  begin
    BeginEdit;
    Key := 0;
  end;
end;

procedure TExplorerEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if Key <> 0 then
    HandleKey(Key, Shift);
end;

procedure TExplorerEdit.WMSetFocus(var Message: TLMSetFocus);
var
  Form: TCustomForm;
begin
  inherited;
  if (csDesigning in ComponentState) or FEndingEdit then
    Exit;
  if FEdit.HandleAllocated and (Message.FocusedWnd = FEdit.Handle) then
  begin
    { Shift+Tab from the edit moves on to the previous control }
    Form := GetParentForm(Self);
    if Form <> nil then
      Form.SelectNext(Self, False, True);
  end
  else if FMode = emBreadcrumb then
  begin
    { Tabbing into a breadcrumb edits the path, clicking it does not }
    if GetKeyState(VK_LBUTTON) >= 0 then
      BeginEdit
    else
      Invalidate;
  end
  else if FEdit.CanFocus then
    FEdit.SetFocus;
end;

procedure TExplorerEdit.WMKillFocus(var Message: TLMKillFocus);
begin
  inherited;
  Invalidate;
end;

function TExplorerEdit.HitTest(X, Y: Integer; out Index: Integer;
  out Arrow: Boolean): Boolean;
var
  P: TPoint;
  Crumb: TCrumb;
  I: Integer;
begin
  Result := False;
  Index := NoCrumb;
  Arrow := False;
  if (FMode <> emBreadcrumb) or FEditing then
    Exit;
  P := Point(X, Y);
  Result := True;
  if PtInRect(FFirstArrow, P) then
  begin
    Index := -1;
    Arrow := True;
    Exit;
  end;
  for I := FHiddenCount to FCrumbs.Count - 1 do
  begin
    Crumb := TCrumb(FCrumbs[I]);
    Index := I;
    if PtInRect(Crumb.TextRect, P) then
      Exit;
    Arrow := True;
    if PtInRect(Crumb.ArrowRect, P) then
      Exit;
    Arrow := False;
  end;
  Index := NoCrumb;
  Result := False;
end;

procedure TExplorerEdit.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  I: Integer;
  A: Boolean;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button <> mbLeft) or (csDesigning in ComponentState) then
    Exit;
  if (FMode = emDropDown) and PtInRect(ButtonRect, Point(X, Y)) then
  begin
    { A click which closed the drop down does not open it again }
    if DroppedDown then
      CloseUp
    else if GetTickCount - FCloseTime > 250 then
      DropDown;
  end
  else if HitTest(X, Y, I, A) then
  begin
    if A then
      ShowCrumbMenu(I)
    else
    begin
      FPressedIndex := I;
      FPressedArrow := False;
      Invalidate;
    end;
  end
  else
    BeginEdit;
end;

procedure TExplorerEdit.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  pidl: PItemIDList;
  I, J: Integer;
  A: Boolean;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if (FPressedIndex < 0) or FPressedArrow then
    Exit;
  I := FPressedIndex;
  FPressedIndex := NoCrumb;
  Invalidate;
  if HitTest(X, Y, J, A) and (J = I) and not A then
  begin
    pidl := ILClone(TCrumb(FCrumbs[I]).IDList);
    try
      ChangeFolder(pidl, True);
    finally
      CoTaskMemFree(pidl);
    end;
  end;
end;

procedure TExplorerEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
  A, B: Boolean;
begin
  inherited MouseMove(Shift, X, Y);
  if not HitTest(X, Y, I, A) then
    I := NoCrumb;
  B := (FMode = emDropDown) and PtInRect(ButtonRect, Point(X, Y));
  if (I <> FHotIndex) or (B <> FHotButton) then
  begin
    FHotIndex := I;
    FHotButton := B;
    Invalidate;
  end;
end;

procedure TExplorerEdit.MouseLeave;
begin
  inherited MouseLeave;
  if (FHotIndex <> NoCrumb) or FHotButton then
  begin
    FHotIndex := NoCrumb;
    FHotButton := False;
    Invalidate;
  end;
end;

procedure TExplorerEdit.LayoutCrumbs;
var
  Crumb: TCrumb;
  Arrow, Pad, Margin, X, Avail, Used, W, I: Integer;
begin
  Canvas.Font := Font;
  Arrow := ScaleValue(16);
  Pad := ScaleValue(6);
  Margin := ScaleValue(2);
  X := IconWidth;
  FFirstArrow := Rect(X, Margin, X + Arrow, ClientHeight - Margin);
  Inc(X, Arrow);
  { Leave room to click for editing }
  Avail := ClientWidth - X - ScaleValue(24);
  Used := 0;
  FHiddenCount := 0;
  { Crumbs which do not fit are hidden from the first, but the last crumb is
    always shown }
  for I := FCrumbs.Count - 1 downto 0 do
  begin
    Crumb := TCrumb(FCrumbs[I]);
    W := Canvas.TextWidth(Crumb.Name) + Pad * 2 + Arrow;
    Crumb.Visible := (FHiddenCount = 0) and ((Used + W <= Avail) or
      (I = FCrumbs.Count - 1));
    if Crumb.Visible then
      Inc(Used, W)
    else
      Inc(FHiddenCount);
  end;
  for I := 0 to FCrumbs.Count - 1 do
  begin
    Crumb := TCrumb(FCrumbs[I]);
    if not Crumb.Visible then
    begin
      Crumb.TextRect := Rect(0, 0, 0, 0);
      Crumb.ArrowRect := Rect(0, 0, 0, 0);
      Continue;
    end;
    W := Canvas.TextWidth(Crumb.Name) + Pad * 2;
    if X + W + Arrow > ClientWidth then
      W := ClientWidth - X - Arrow;
    if W < 0 then
      W := 0;
    Crumb.TextRect := Rect(X, Margin, X + W, ClientHeight - Margin);
    Inc(X, W);
    Crumb.ArrowRect := Rect(X, Margin, X + Arrow, ClientHeight - Margin);
    Inc(X, Arrow);
  end;
end;

procedure TExplorerEdit.PaintCrumbs;
type
  TChevron = (chRight, chDown, chLeft);

  procedure DrawButton(const R: TRect; Pressed: Boolean);
  var
    Details: TThemedElementDetails;
  begin
    if Pressed then
      Details := ThemeServices.GetElementDetails(ttbButtonPressed)
    else
      Details := ThemeServices.GetElementDetails(ttbButtonHot);
    ThemeServices.DrawElement(Canvas.Handle, Details, R);
  end;

  procedure DrawChevron(const R: TRect; Chevron: TChevron);
  var
    X, Y, S: Integer;
  begin
    S := ScaleValue(3);
    X := (R.Left + R.Right) div 2;
    Y := (R.Top + R.Bottom) div 2;
    case Chevron of
      chRight:
        Canvas.Polyline([Point(X - S div 2, Y - S), Point(X + S div 2, Y),
          Point(X - S div 2 - 1, Y + S + 1)]);
      chDown:
        Canvas.Polyline([Point(X - S, Y - S div 2), Point(X, Y + S div 2),
          Point(X + S + 1, Y - S div 2 - 1)]);
      chLeft:
        begin
          Canvas.Polyline([Point(X, Y - S), Point(X - S, Y),
            Point(X + 1, Y + S + 1)]);
          Canvas.Polyline([Point(X + S, Y - S), Point(X, Y),
            Point(X + S + 1, Y + S + 1)]);
        end;
    end;
  end;

var
  Crumb: TCrumb;
  Style: TTextStyle;
  I: Integer;
begin
  LayoutCrumbs;
  if not Enabled then
    Canvas.Font.Color := clGrayText;
  Canvas.Pen.Color := Canvas.Font.Color;
  Canvas.Pen.Width := 1;
  { The first arrow lists the hidden crumbs and the desktop }
  if FPressedIndex = -1 then
    DrawButton(FFirstArrow, True)
  else if FHotIndex = -1 then
    DrawButton(FFirstArrow, False);
  if FHiddenCount > 0 then
    DrawChevron(FFirstArrow, chLeft)
  else if FPressedIndex = -1 then
    DrawChevron(FFirstArrow, chDown)
  else
    DrawChevron(FFirstArrow, chRight);
  Style := Canvas.TextStyle;
  Style.Alignment := taCenter;
  Style.Layout := tlCenter;
  Style.SingleLine := True;
  Style.EndEllipsis := True;
  Style.Clipping := True;
  Style.Opaque := False;
  Style.ShowPrefix := False;
  for I := FHiddenCount to FCrumbs.Count - 1 do
  begin
    Crumb := TCrumb(FCrumbs[I]);
    if FPressedIndex = I then
    begin
      DrawButton(Crumb.TextRect, not FPressedArrow);
      DrawButton(Crumb.ArrowRect, FPressedArrow);
    end
    else if FHotIndex = I then
    begin
      DrawButton(Crumb.TextRect, False);
      DrawButton(Crumb.ArrowRect, False);
    end;
    Canvas.Brush.Style := bsClear;
    Canvas.TextRect(Crumb.TextRect, Crumb.TextRect.Left, Crumb.TextRect.Top,
      Crumb.Name, Style);
    Canvas.Brush.Style := bsSolid;
    if (FPressedIndex = I) and FPressedArrow then
      DrawChevron(Crumb.ArrowRect, chDown)
    else
      DrawChevron(Crumb.ArrowRect, chRight);
  end;
  if Focused then
    Canvas.DrawFocusRect(Rect(IconWidth, 1, ClientWidth - 1, ClientHeight - 1));
end;

procedure TExplorerEdit.Paint;
var
  State: TThemedComboBox;
  S: Integer;
begin
  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);
  S := GetSystemMetrics(SM_CXSMICON);
  if FIcon <> 0 then
    DrawIconEx(Canvas.Handle, ScaleValue(4), (ClientHeight - S) div 2, FIcon,
      S, S, 0, 0, DI_NORMAL);
  if (FMode = emBreadcrumb) and not FEditing then
    PaintCrumbs
  else if FMode = emDropDown then
  begin
    if not Enabled then
      State := tcDropDownButtonDisabled
    else if DroppedDown then
      State := tcDropDownButtonPressed
    else if FHotButton then
      State := tcDropDownButtonHot
    else
      State := tcDropDownButtonNormal;
    ThemeServices.DrawElement(Canvas.Handle,
      ThemeServices.GetElementDetails(State), ButtonRect);
  end;
end;

procedure TExplorerEdit.ShowCrumbMenu(Index: Integer);
var
  Folder, Next, Desktop, pidl: PItemIDList;
  Items, Bitmaps: TList;
  Menu: HMENU;
  Info: TMenuItemInfoW;
  W: WideString;
  R: TRect;
  P: TPoint;
  Hidden, Cmd, I: Integer;
begin
  Desktop := PathToIDList('');
  Items := TList.Create;
  Bitmaps := TList.Create;
  Menu := Windows.CreatePopupMenu;
  try
    Next := nil;
    if Index < 0 then
    begin
      { The hidden crumbs come first, the nearest at the top }
      Folder := Desktop;
      for I := FHiddenCount - 1 downto 0 do
        Items.Add(ILClone(TCrumb(FCrumbs[I]).IDList));
      R := FFirstArrow;
    end
    else
    begin
      Folder := TCrumb(FCrumbs[Index]).IDList;
      if Index < FCrumbs.Count - 1 then
        Next := TCrumb(FCrumbs[Index + 1]).IDList;
      R := TCrumb(FCrumbs[Index]).ArrowRect;
    end;
    Hidden := Items.Count;
    AddSubfolders(Folder, Items);
    if Items.Count = 0 then
      Exit;
    for I := 0 to Items.Count - 1 do
    begin
      if (I = Hidden) and (Hidden > 0) then
        AppendMenuW(Menu, MF_SEPARATOR, 0, nil);
      pidl := Items[I];
      W := StrToWide(IDListToName(pidl, SIGDN_NORMALDISPLAY));
      FillChar(Info, SizeOf(Info), #0);
      Info.cbSize := SizeOf(Info);
      Info.fMask := MIIM_ID or MIIM_STRING or MIIM_BITMAP or MIIM_STATE;
      Info.wID := I + 1;
      Info.dwTypeData := PWideChar(W);
      Info.hbmpItem := ItemBitmap(pidl);
      if Info.hbmpItem <> 0 then
        Bitmaps.Add(Pointer(Info.hbmpItem));
      { The folder of the next crumb is bold }
      if (Next <> nil) and ILIsEqual(pidl, Next) then
        Info.fState := MFS_DEFAULT;
      InsertMenuItemW(Menu, GetMenuItemCount(Menu), True, @Info);
    end;
    FPressedIndex := Index;
    FPressedArrow := True;
    Invalidate;
    Update;
    P := ClientToScreen(Point(R.Left, R.Bottom));
    Cmd := Integer(TrackPopupMenu(Menu, TPM_RETURNCMD or TPM_LEFTALIGN or
      TPM_TOPALIGN or TPM_LEFTBUTTON, P.X, P.Y, 0, Handle, nil));
    FPressedIndex := NoCrumb;
    Invalidate;
    if (Cmd > 0) and (Cmd <= Items.Count) then
      ChangeFolder(Items[Cmd - 1], True);
  finally
    DestroyMenu(Menu);
    for I := 0 to Bitmaps.Count - 1 do
      DeleteObject(HBITMAP(Bitmaps[I]));
    for I := 0 to Items.Count - 1 do
      CoTaskMemFree(Items[I]);
    Bitmaps.Free;
    Items.Free;
    CoTaskMemFree(Desktop);
  end;
end;

procedure TExplorerEdit.DropDown;
var
  Form: TForm;
  R, Work: TRect;
  H: Integer;
begin
  if (FMode <> emDropDown) or DroppedDown or (csDesigning in ComponentState) or
    not HandleAllocated then
    Exit;
  if FDropDownForm = nil then
  begin
    Form := TForm.CreateNew(Self);
    Form.BorderStyle := bsNone;
    Form.ShowInTaskBar := stNever;
    Form.PopupMode := pmExplicit;
    Form.OnDeactivate := DropDownDeactivate;
    FDropDownTree := TExplorerTree.Create(Form);
    FDropDownTree.BorderStyle := bsSingle;
    FDropDownTree.Align := alClient;
    FDropDownTree.OnChange := DropDownChange;
    FDropDownTree.OnKeyDown := DropDownKeyDown;
    FDropDownTree.OnMouseDown := DropDownMouseDown;
    FDropDownTree.Parent := Form;
    FDropDownForm := Form;
  end;
  Form := TForm(FDropDownForm);
  Form.PopupParent := GetParentForm(Self);
  GetWindowRect(Handle, R);
  H := FDropDownHeight;
  { Open above the control when there is no room below }
  Work := Screen.MonitorFromRect(R).WorkareaRect;
  if (R.Bottom + H > Work.Bottom) and (R.Top - H >= Work.Top) then
    Form.SetBounds(R.Left, R.Top - H, R.Right - R.Left, H)
  else
    Form.SetBounds(R.Left, R.Bottom, R.Right - R.Left, H);
  FMouseSelect := False;
  Form.Show;
  FDropDownTree.SelectIDList(FFolder);
  if FDropDownTree.CanFocus then
    FDropDownTree.SetFocus;
  Invalidate;
  if Assigned(FOnDropDown) then
    FOnDropDown(Self);
end;

procedure TExplorerEdit.DoCloseUp(Accept, Refocus: Boolean);
var
  pidl: PItemIDList;
begin
  if FClosing or not DroppedDown then
    Exit;
  FClosing := True;
  pidl := nil;
  try
    if Accept then
      pidl := ItemToIDList(FDropDownTree.GetSelectedItem);
    FCloseTime := GetTickCount;
    FDropDownForm.Hide;
    Invalidate;
    if Refocus and FEdit.CanFocus then
      FEdit.SetFocus;
    if pidl <> nil then
      ChangeFolder(pidl, True);
    if Assigned(FOnCloseUp) then
      FOnCloseUp(Self);
  finally
    CoTaskMemFree(pidl);
    FClosing := False;
  end;
end;

procedure TExplorerEdit.CloseUp(Accept: Boolean = False);
begin
  DoCloseUp(Accept, True);
end;

procedure TExplorerEdit.DropDownChange(Sender: TObject);
begin
  { A folder clicked with the mouse is accepted. The tree is closed after its
    event returns }
  if FMouseSelect and HandleAllocated and not (csDestroying in ComponentState) then
    PostMessage(Handle, WM_EXPLORERCLOSEUP, 1, 0);
end;

procedure TExplorerEdit.DropDownDeactivate(Sender: TObject);
begin
  DoCloseUp(False, False);
end;

procedure TExplorerEdit.DropDownKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  FMouseSelect := False;
  case Key of
    VK_RETURN:
      begin
        PostMessage(Handle, WM_EXPLORERCLOSEUP, 1, 0);
        Key := 0;
      end;
    VK_ESCAPE, VK_F4:
      begin
        PostMessage(Handle, WM_EXPLORERCLOSEUP, 0, 0);
        Key := 0;
      end;
  end;
end;

procedure TExplorerEdit.DropDownMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    FMouseSelect := True;
end;

procedure TExplorerEdit.WMExplorerCloseUp(var Message: TLMessage);
begin
  DoCloseUp(Message.WParam = 1, True);
end;

function TExplorerEdit.GetDroppedDown: Boolean;
begin
  Result := (FDropDownForm <> nil) and FDropDownForm.Visible;
end;

procedure TExplorerEdit.SetDroppedDown(Value: Boolean);
begin
  if Value then
    DropDown
  else
    CloseUp(False);
end;

procedure TExplorerEdit.GoUp;
var
  pidl: PItemIDList;
begin
  if (FFolder = nil) or (FFolder.mkid.cb = 0) then
    Exit;
  pidl := ILClone(FFolder);
  try
    if ILRemoveLastID(pidl) then
      ChangeFolder(pidl, True);
  finally
    CoTaskMemFree(pidl);
  end;
end;

function TExplorerEdit.GetPath: string;
begin
  if FPath <> '' then
    Result := FPath
  else
    Result := IDListToPath(FFolder);
end;

procedure TExplorerEdit.SetPath(const Value: string);
var
  pidl: PItemIDList;
begin
  pidl := PathToIDList(Value);
  if pidl = nil then
    Exit;
  try
    ChangeFolder(pidl, False);
    { The path is kept as it is given, such as shell:Downloads, while it
      names the folder shown }
    if (FFolder <> nil) and ILIsEqual(FFolder, pidl) then
      FPath := Value;
  finally
    CoTaskMemFree(pidl);
  end;
end;

procedure TExplorerEdit.SetAutoComplete(Value: Boolean);
begin
  if Value = FAutoComplete then
    Exit;
  FAutoComplete := Value;
  { Autocomplete can only be removed by creating the edit window again }
  if FEdit.HandleAllocated then
    RecreateWnd(FEdit);
end;

procedure TExplorerEdit.SetMode(Value: TExplorerEditMode);
begin
  if Value = FMode then
    Exit;
  DoCloseUp(False, False);
  FMode := Value;
  FEditing := False;
  FHotIndex := NoCrumb;
  FPressedIndex := NoCrumb;
  FHotButton := False;
  UpdateLayout;
  Invalidate;
end;

function TExplorerEdit.GetBinding: IComponentBinding;
begin
  Result := FBinding;
end;

procedure TExplorerEdit.SetBinding(Value: IComponentBinding);
begin
  FBinding := Value;
end;

{ Explorer preview helpers }

const
  PreviewTimer = 1;
  PreviewDelay = 200;

{ Returns the class of the preview handler of an item }

function FindPreviewHandler(Item: IShellItem; out ClassID: TGUID): Boolean;
var
  Attributes: SFGAOF;
  P: PWideChar;
  Ext, Key: WideString;
  Buffer: array[0..MAX_PATH] of WideChar;
  N: DWORD;
begin
  Result := False;
  Attributes := 0;
  Item.GetAttributes(SFGAO_FOLDER or SFGAO_STREAM, @Attributes);
  { Folders have no preview, but zip files are folders which are also files }
  if (Attributes and SFGAO_STREAM) = 0 then
    Exit;
  P := nil;
  if Item.GetDisplayName(SIGDN(SIGDN_PARENTRELATIVEPARSING), LPWSTR(@P)) <> S_OK then
    Exit;
  Ext := StrToWide(ExtractFileExt(WideToStr(WideString(P))));
  CoTaskMemFree(P);
  if Ext = '' then
    Exit;
  Key := SPreviewHandlerKey;
  N := Length(Buffer);
  if AssocQueryStringW(ASSOCF_INIT_DEFAULTTOSTAR, ASSOCSTR_SHELLEXTENSION,
    PWideChar(Ext), PWideChar(Key), @Buffer[0], N) <> S_OK then
    Exit;
  Result := TryStringToGUID(WideToStr(WideString(PWideChar(@Buffer[0]))), ClassID);
end;

{ Creates a preview handler and gives it an item. Handlers usually run in a
  separate process, which keeps a failing handler from stopping the program }

function CreatePreviewHandler(const ClassID: TGUID; Item: IShellItem): IPreviewHandler;
var
  WithStream: IInitializeWithStream;
  WithItem: IInitializeWithItem;
  WithFile: IInitializeWithFile;
  Stream: IStream;
  P: Pointer;
  W: WideString;
  Done: Boolean;
begin
  Result := nil;
  if CoCreateInstance(ClassID, nil, CLSCTX_LOCAL_SERVER, IPreviewHandler,
    Result) <> S_OK then
    if CoCreateInstance(ClassID, nil, CLSCTX_INPROC_SERVER, IPreviewHandler,
      Result) <> S_OK then
    begin
      Result := nil;
      Exit;
    end;
  Done := False;
  if Supports(Result, IInitializeWithStream, WithStream) then
  begin
    P := nil;
    if (Item.BindToHandler(nil, @BHID_Stream, @IID_IStream, @P) = S_OK) and
      (P <> nil) then
    begin
      { BindToHandler returns an interface which already has a reference }
      Pointer(Stream) := P;
      Done := WithStream.Initialize(Stream, STGM_READ) = S_OK;
    end;
  end
  else if Supports(Result, IInitializeWithItem, WithItem) then
    Done := WithItem.Initialize(Item, STGM_READ) = S_OK
  else if Supports(Result, IInitializeWithFile, WithFile) then
  begin
    W := StrToWide(ItemToFileName(Item));
    Done := WithFile.Initialize(PWideChar(W), STGM_READ) = S_OK;
  end;
  if not Done then
    Result := nil;
end;

{ Formats a number of bytes as a short size such as 12.3 KB }

function ByteSize(Value: Int64): string;
var
  Buffer: array[0..63] of WideChar;
begin
  Result := '';
  if StrFormatByteSizeW(Value, @Buffer[0], Length(Buffer)) <> nil then
    Result := WideToStr(WideString(PWideChar(@Buffer[0])));
end;

{ Returns true if a file system path is the root of a drive such as C:\ }

function IsDriveRoot(const Path: WideString): Boolean;
begin
  Result := (Length(Path) = 3) and (Path[2] = ':') and (Path[3] = '\');
end;

{ Returns a property of an item formatted for display, or an empty string if
  the item does not have it }

function PropertyText(Store: IPropertyStore; const Name: string): string;
var
  Key: PROPERTYKEY;
  Value: TPropVariant;
  P: PWideChar;
begin
  Result := '';
  if not NameToPropertyKey(Name, Key) then
    Exit;
  FillChar(Value, SizeOf(Value), #0);
  if Store.GetValue(@Key, Value) <> S_OK then
    Exit;
  try
    if Value.vt = VT_EMPTY then
      Exit;
    P := nil;
    if PSFormatForDisplayAlloc(@Key, @Value, PDFF_DEFAULT, P) = S_OK then
    begin
      Result := WideToStr(WideString(P));
      CoTaskMemFree(P);
    end;
  finally
    PropVariantClear(Value);
  end;
end;

{ Reads a property of an item which is a 32 bit number }

function PropertyCardinal(Store: IPropertyStore; const Name: string;
  out Value: Cardinal): Boolean;
var
  Key: PROPERTYKEY;
  V: TPropVariant;
begin
  Result := False;
  Value := 0;
  if not NameToPropertyKey(Name, Key) then
    Exit;
  FillChar(V, SizeOf(V), #0);
  if Store.GetValue(@Key, V) <> S_OK then
    Exit;
  try
    Result := V.vt = VT_UI4;
    if Result then
      Value := V.ulVal;
  finally
    PropVariantClear(V);
  end;
end;

{ Returns the text of a four character code such as H264, or an empty
  string if it is not text }

function FourCCText(Code: Cardinal): string;
var
  C: Char;
  I: Integer;
begin
  Result := '';
  for I := 0 to 3 do
  begin
    C := Chr((Code shr (I * 8)) and $FF);
    if not (C in [' '..'~']) then
    begin
      Result := '';
      Exit;
    end;
    Result := Result + C;
  end;
  Result := Trim(Result);
end;

const
  { The largest thumbnail shown when there is no preview }
  MaxImageSize = 256;

  { Access rights of files and folders }
  FileReadData = $1;
  FileWriteData = $2;
  FileAppendData = $4;
  FileGenericRead = $120089;
  FileGenericWrite = $120116;
  FileGenericExecute = $1200A0;
  FileAllAccess = $1F01FF;
  FileDelete = $10000;

{ Returns the name of an account, such as DELL\delluser }

function SidToName(Sid: PSID): string;
var
  Name, Domain: array[0..255] of WideChar;
  NameLength, DomainLength: DWORD;
  Use: SID_NAME_USE;
begin
  Result := '';
  NameLength := Length(Name);
  DomainLength := Length(Domain);
  if not LookupAccountSidW(nil, Sid, @Name[0], NameLength, @Domain[0],
    DomainLength, Use) then
    Exit;
  Result := WideToStr(WideString(PWideChar(@Name[0])));
  if Domain[0] <> #0 then
    Result := WideToStr(WideString(PWideChar(@Domain[0]))) + '\' + Result;
end;

{ Reads the owner of a folder and what the current user can do in it, using
  the same access check Windows makes when the folder is opened }

procedure ReadFolderAccess(const Path: WideString; out Owner, Access: string);
const
  Information = OWNER_SECURITY_INFORMATION or GROUP_SECURITY_INFORMATION or
    DACL_SECURITY_INFORMATION;
  ModifyAccess = FileReadData or FileWriteData or FileAppendData or FileDelete;
  WriteAccess = FileReadData or FileWriteData or FileAppendData;
var
  Descriptor: array of Byte;
  Size: DWORD;
  OwnerSid: PSID;
  Defaulted: BOOL;
  Process, Token: THandle;
  Mapping: GENERIC_MAPPING;
  Privileges: array[0..255] of Byte;
  PrivilegesLength, Granted: DWORD;
  Status: BOOL;
begin
  Owner := '';
  Access := '';
  Size := 0;
  GetFileSecurityW(PWideChar(Path), Information, nil, 0, @Size);
  if Size = 0 then
    Exit;
  SetLength(Descriptor, Size);
  if not GetFileSecurityW(PWideChar(Path), Information, @Descriptor[0], Size,
    @Size) then
    Exit;
  OwnerSid := nil;
  Defaulted := False;
  if GetSecurityDescriptorOwner(@Descriptor[0], OwnerSid, @Defaulted) and
    (OwnerSid <> nil) then
    Owner := SidToName(OwnerSid);
  { The check needs an impersonation token of the current user }
  Process := 0;
  if not OpenProcessToken(GetCurrentProcess, TOKEN_DUPLICATE or TOKEN_QUERY,
    Process) then
    Exit;
  try
    Token := 0;
    if not DuplicateToken(Process, SecurityImpersonation, @Token) then
      Exit;
    try
      Mapping.GenericRead := FileGenericRead;
      Mapping.GenericWrite := FileGenericWrite;
      Mapping.GenericExecute := FileGenericExecute;
      Mapping.GenericAll := FileAllAccess;
      PrivilegesLength := SizeOf(Privileges);
      Granted := 0;
      Status := False;
      if not AccessCheck(@Descriptor[0], Token, MAXIMUM_ALLOWED, @Mapping,
        @Privileges[0], @PrivilegesLength, @Granted, @Status) then
        Exit;
      if not Status then
        Access := 'No access'
      else if (Granted and FileAllAccess) = FileAllAccess then
        Access := 'Full control'
      else if (Granted and ModifyAccess) = ModifyAccess then
        Access := 'Modify'
      else if (Granted and WriteAccess) = WriteAccess then
        Access := 'Read & write'
      else if (Granted and FileReadData) <> 0 then
        Access := 'Read only'
      else
        Access := 'No access';
    finally
      CloseHandle(Token);
    end;
  finally
    CloseHandle(Process);
  end;
end;

const
  { Attributes of files stored only online, such as in OneDrive }
  OnlineAttributes = FILE_ATTRIBUTE_OFFLINE or $400000;

{ Reads the description and version from the version resource of a program
  or library. Returns false if the file has no version resource }

function ReadVersionInfo(const FileName: WideString; out Description,
  Version: string): Boolean;
var
  Data: array of Byte;
  Handle, Size: DWORD;

  function Query(const Name: WideString; out Value: Pointer; out Len: UINT): Boolean;
  var
    S: WideString;
  begin
    S := Name;
    Value := nil;
    Len := 0;
    Result := VerQueryValueW(@Data[0], PWideChar(S), Value, Len) and
      (Value <> nil) and (Len > 0);
  end;

  function QueryString(const Language, Name: string): string;
  var
    P: Pointer;
    Len: UINT;
  begin
    Result := '';
    if Query(StrToWide('\StringFileInfo\' + Language + '\' + Name), P, Len) then
      Result := Trim(WideToStr(WideString(PWideChar(P))));
  end;

var
  Fixed: ^TVSFixedFileInfo;
  Translation: PWord;
  Language: string;
  P: Pointer;
  Len: UINT;
begin
  Description := '';
  Version := '';
  Handle := 0;
  Size := GetFileVersionInfoSizeW(PWideChar(FileName), Handle);
  Result := Size > 0;
  if not Result then
    Exit;
  SetLength(Data, Size);
  Result := GetFileVersionInfoW(PWideChar(FileName), 0, Size, @Data[0]);
  if not Result then
    Exit;
  { The numbers of the file version are preferred to its text }
  if Query('\', P, Len) and (Len >= SizeOf(TVSFixedFileInfo)) then
  begin
    Fixed := P;
    Version := Format('%d.%d.%d.%d', [HiWord(Fixed.dwFileVersionMS),
      LoWord(Fixed.dwFileVersionMS), HiWord(Fixed.dwFileVersionLS),
      LoWord(Fixed.dwFileVersionLS)]);
  end;
  { Strings are stored by language and code page, using the first listed or
    else US English }
  Language := '040904B0';
  if Query('\VarFileInfo\Translation', P, Len) and (Len >= 4) then
  begin
    Translation := P;
    Language := IntToHex(Translation^, 4) + IntToHex(PWord(PtrUInt(P) + 2)^, 4);
  end;
  Description := QueryString(Language, 'FileDescription');
  if Description = '' then
    Description := QueryString('040904E4', 'FileDescription');
  if Version = '' then
    Version := QueryString(Language, 'FileVersion');
end;

{ TExplorerPreview }

constructor TExplorerPreview.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption] + [csOpaque];
  Width := 200;
  Height := 200;
  Color := clWindow;
  FEmptyText := 'No preview available';
end;

destructor TExplorerPreview.Destroy;
begin
  Unload;
  ClearInfo;
  inherited Destroy;
end;

procedure TExplorerPreview.CreateWnd;
begin
  inherited CreateWnd;
  StartPreview;
end;

procedure TExplorerPreview.DestroyWnd;
begin
  if HandleAllocated then
    KillTimer(Handle, PreviewTimer);
  Unload;
  ClearInfo;
  inherited DestroyWnd;
end;

procedure TExplorerPreview.Resize;
var
  R: TRect;
begin
  inherited Resize;
  if FHandler <> nil then
  begin
    R := ClientRect;
    FHandler.SetRect(@R);
  end;
end;

procedure TExplorerPreview.Paint;
var
  Style: TTextStyle;
begin
  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);
  if FHandler <> nil then
    Exit;
  if FInfoValid then
  begin
    PaintInfo;
    Exit;
  end;
  Canvas.Font := Font;
  Canvas.Font.Color := clGrayText;
  Style := Canvas.TextStyle;
  Style.Alignment := taCenter;
  Style.Layout := tlCenter;
  Style.Wordbreak := True;
  Style.SingleLine := False;
  Style.Opaque := False;
  Canvas.TextRect(ClientRect, 0, 0, FEmptyText, Style);
end;

procedure TExplorerPreview.Unload;
begin
  if FHandler = nil then
    Exit;
  try
    FHandler.Unload;
  except
    { The handler process may have ended }
  end;
  FHandler := nil;
  Invalidate;
end;

procedure TExplorerPreview.ClearInfo;
begin
  if FImage <> 0 then
    DeleteObject(FImage);
  FImage := 0;
  FInfoName := '';
  FInfoType := '';
  FInfoSize := '';
  FInfoDate := '';
  FInfoTotal := '';
  FInfoFree := '';
  FInfoDescription := '';
  FInfoVersion := '';
  FInfoWidth := '';
  FInfoHeight := '';
  FInfoDuration := '';
  FInfoArtist := '';
  FInfoCodec := '';
  FInfoFrameSize := '';
  FInfoOwner := '';
  FInfoAccess := '';
  FInfoValid := False;
end;

procedure TExplorerPreview.LoadInfo(Item: IShellItem);
var
  Factory: IShellItemImageFactory;
  Size: Windows.TSize;
  Info: TSHFileInfoW;
  Data: TWin32FileAttributeData;
  Available, Total: TLargeInteger;
  Mode: UINT;
  Local: TFileTime;
  ST: TSystemTime;
  pidl: PItemIDList;
  Store: IPropertyStore;
  A, B: Cardinal;
  P: PWideChar;
  W: WideString;
begin
  ClearInfo;
  P := nil;
  if Item.GetDisplayName(SIGDN(SIGDN_NORMALDISPLAY), LPWSTR(@P)) = S_OK then
  begin
    FInfoName := WideToStr(WideString(P));
    CoTaskMemFree(P);
  end;
  pidl := ItemToIDList(Item);
  if pidl <> nil then
  try
    FillChar(Info, SizeOf(Info), #0);
    if SHGetFileInfoW(LPCWSTR(pidl), 0, Info, SizeOf(Info),
      SHGFI_PIDL or SHGFI_TYPENAME) <> 0 then
      FInfoType := WideToStr(WideString(PWideChar(@Info.szTypeName[0])));
    { Images show their dimensions, and audio and video their duration and
      artist, formatted as Windows Explorer shows them }
    if SHGetPropertyStoreFromIDList(pidl, GPS_DEFAULT, IPropertyStore,
      Store) <> S_OK then
      Store := nil;
    if Store <> nil then
    begin
      FInfoWidth := PropertyText(Store, 'System.Image.HorizontalSize');
      FInfoHeight := PropertyText(Store, 'System.Image.VerticalSize');
      FInfoDuration := PropertyText(Store, 'System.Media.Duration');
      FInfoArtist := PropertyText(Store, 'System.Music.Artist');
      if PropertyCardinal(Store, 'System.Video.FrameWidth', A) and
        PropertyCardinal(Store, 'System.Video.FrameHeight', B) and (A > 0) then
        FInfoFrameSize := Format('%d x %d', [A, B]);
      { The codec is named by its four character code, which is also the
        start of the compression subtype }
      if PropertyCardinal(Store, 'System.Video.FourCC', A) and (A <> 0) then
        FInfoCodec := FourCCText(A)
      else
      begin
        FInfoCodec := PropertyText(Store, 'System.Video.Compression');
        if (Length(FInfoCodec) = 38) and (FInfoCodec[1] = '{') then
          FInfoCodec := FourCCText(StrToIntDef('$' + System.Copy(FInfoCodec, 2, 8), 0));
      end;
    end;
  finally
    CoTaskMemFree(pidl);
  end;
  { Sizes and dates are read from the file system }
  P := nil;
  if Item.GetDisplayName(SIGDN(SIGDN_FILESYSPATH), LPWSTR(@P)) = S_OK then
  begin
    W := WideString(P);
    CoTaskMemFree(P);
    if IsDriveRoot(W) then
    begin
      { Drives show their sizes when a disk is mounted. Errors are not
        shown, so an empty drive does not ask for a disk }
      Mode := SetErrorMode(SEM_FAILCRITICALERRORS);
      try
        if GetDiskFreeSpaceExW(PWideChar(W), @Available, @Total, nil) then
        begin
          FInfoTotal := ByteSize(Total);
          FInfoFree := ByteSize(Available);
        end;
      finally
        SetErrorMode(Mode);
      end;
    end
    else
    begin
      FillChar(Data, SizeOf(Data), #0);
      if GetFileAttributesExW(PWideChar(W), GetFileExInfoStandard, @Data) then
      begin
        if (Data.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
        begin
          FInfoSize := ByteSize(Int64(Data.nFileSizeHigh) shl 32 or Data.nFileSizeLow);
          { Programs and libraries show their description and version.
            Files stored only online are not read, which would download them }
          if (Data.dwFileAttributes and OnlineAttributes) = 0 then
            ReadVersionInfo(W, FInfoDescription, FInfoVersion);
        end
        else
          { Folders show their owner and the access of the current user }
          ReadFolderAccess(W, FInfoOwner, FInfoAccess);
        if FileTimeToLocalFileTime(Data.ftLastWriteTime, Local) and
          FileTimeToSystemTime(Local, ST) then
          FInfoDate := FormatDateTime('ddddd t', SystemTimeToDateTime(ST));
      end;
    end;
  end;
  { The thumbnail of the item, or its icon when it has none }
  if Supports(Item, IShellItemImageFactory, Factory) then
  begin
    Size.cx := MaxImageSize;
    Size.cy := Size.cx;
    if Factory.GetImage(Size, SIIGBF_RESIZETOFIT, FImage) <> S_OK then
      FImage := 0;
    PremultiplyBitmap(FImage);
  end;
  FInfoValid := True;
end;

procedure TExplorerPreview.PaintInfo;
var
  Lines: array[0..15] of string;
  Count: Integer;

  procedure Add(const Prefix, Value: string);
  begin
    if Value <> '' then
    begin
      Lines[Count] := Prefix + Value;
      Inc(Count);
    end;
  end;

var
  Map: Windows.BITMAP;
  Blend: TBlendFunction;
  LF: TLogFontW;
  Style: TTextStyle;
  DC: HDC;
  Old: HGDIOBJ;
  W, H, DW, DH, MaxW, MaxH, Gap, LineHeight, Total, Y, I: Integer;
begin
  { The font Windows Explorer uses for items }
  Canvas.Font := Font;
  FillChar(LF, SizeOf(LF), #0);
  if SystemParametersInfoW(SPI_GETICONTITLELOGFONT, SizeOf(LF), @LF, 0) then
  begin
    Canvas.Font.Name := WideToStr(WideString(PWideChar(@LF.lfFaceName[0])));
    Canvas.Font.Height := LF.lfHeight;
  end;
  Canvas.Font.Color := clWindowText;
  Count := 0;
  Add('', FInfoName);
  Add('', FInfoType);
  Add('Description: ', FInfoDescription);
  Add('Version: ', FInfoVersion);
  Add('Width: ', FInfoWidth);
  Add('Height: ', FInfoHeight);
  Add('Codec: ', FInfoCodec);
  Add('Frame size: ', FInfoFrameSize);
  Add('Duration: ', FInfoDuration);
  Add('Artist: ', FInfoArtist);
  Add('Size: ', FInfoSize);
  Add('Date modified: ', FInfoDate);
  Add('Owner: ', FInfoOwner);
  Add('Access: ', FInfoAccess);
  Add('Total size: ', FInfoTotal);
  Add('Free space: ', FInfoFree);
  W := 0;
  H := 0;
  if (FImage <> 0) and (GetObject(FImage, SizeOf(Map), @Map) <> 0) then
  begin
    W := Map.bmWidth;
    H := Abs(Map.bmHeight);
  end;
  Gap := ScaleValue(8);
  LineHeight := Canvas.TextHeight('Wg') + ScaleValue(2);
  { The image shrinks to leave room for the details when the control is
    small, keeping its proportions }
  if (W > 0) and (H > 0) then
  begin
    MaxW := ClientWidth - Gap * 2;
    MaxH := ClientHeight - Gap * 3 - LineHeight * Count;
    if MaxH < ScaleValue(32) then
      MaxH := ScaleValue(32);
    DW := W;
    DH := H;
    if DW > MaxW then
    begin
      DH := DH * MaxW div DW;
      DW := MaxW;
    end;
    if DH > MaxH then
    begin
      DW := DW * MaxH div DH;
      DH := MaxH;
    end;
  end
  else
  begin
    DW := 0;
    DH := 0;
  end;
  Total := DH + Gap + LineHeight * Count;
  Y := (ClientHeight - Total) div 2;
  if Y < Gap then
    Y := Gap;
  if (DW > 0) and (DH > 0) then
  begin
    DC := CreateCompatibleDC(Canvas.Handle);
    Old := SelectObject(DC, FImage);
    Blend.BlendOp := AC_SRC_OVER;
    Blend.BlendFlags := 0;
    Blend.SourceConstantAlpha := 255;
    Blend.AlphaFormat := AC_SRC_ALPHA;
    AlphaBlend(Canvas.Handle, (ClientWidth - DW) div 2, Y, DW, DH, DC, 0, 0, W, H, Blend);
    SelectObject(DC, Old);
    DeleteDC(DC);
    Inc(Y, DH + Gap);
  end;
  Style := Canvas.TextStyle;
  Style.Alignment := taCenter;
  Style.Layout := tlTop;
  Style.SingleLine := True;
  Style.EndEllipsis := True;
  Style.Clipping := True;
  Style.Opaque := False;
  Style.ShowPrefix := False;
  for I := 0 to Count - 1 do
  begin
    { The name is bold and the details are gray }
    if I = 0 then
      Canvas.Font.Style := [fsBold]
    else
    begin
      Canvas.Font.Style := [];
      Canvas.Font.Color := clGrayText;
    end;
    Canvas.TextRect(Rect(Gap, Y, ClientWidth - Gap, Y + LineHeight), Gap, Y,
      Lines[I], Style);
    Inc(Y, LineHeight);
  end;
end;

procedure TExplorerPreview.StartPreview;
begin
  { Loading waits for the file name to stop changing }
  if HandleAllocated and not (csDesigning in ComponentState) then
    SetTimer(Handle, PreviewTimer, PreviewDelay, nil);
end;

procedure TExplorerPreview.WMTimer(var Message: TLMessage);
begin
  if Message.WParam = PreviewTimer then
  begin
    KillTimer(Handle, PreviewTimer);
    LoadPreview;
  end
  else
    inherited;
end;

procedure TExplorerPreview.UpdateVisuals;
var
  Visuals: IPreviewHandlerVisuals;
  LF: TLogFontW;
begin
  if not Supports(FHandler, IPreviewHandlerVisuals, Visuals) then
    Exit;
  Visuals.SetBackgroundColor(ColorToRGB(Color));
  Visuals.SetTextColor(ColorToRGB(Font.Color));
  FillChar(LF, SizeOf(LF), #0);
  if GetObjectW(Font.Handle, SizeOf(LF), @LF) > 0 then
    Visuals.SetFont(@LF);
end;

procedure TExplorerPreview.LoadPreview;
var
  Item: IShellItem;
  ClassID: TGUID;
  Handler: IPreviewHandler;
  Allow: Boolean;
  R: TRect;
begin
  Unload;
  ClearInfo;
  if (FFileName = '') or (csDesigning in ComponentState) or not HandleAllocated then
    Exit;
  Allow := True;
  if Assigned(FOnPreview) then
    FOnPreview(Self, FFileName, Allow);
  if not Allow then
    Exit;
  Handler := nil;
  Item := nil;
  try
    Item := PathToItem(FFileName);
    if (Item <> nil) and FindPreviewHandler(Item, ClassID) then
      Handler := CreatePreviewHandler(ClassID, Item);
    if Handler <> nil then
    begin
      R := ClientRect;
      if Handler.SetWindow(Handle, @R) = S_OK then
      begin
        FHandler := Handler;
        UpdateVisuals;
        if FHandler.DoPreview <> S_OK then
          Unload;
      end;
    end;
  except
    FHandler := nil;
  end;
  { Without a preview the thumbnail and details of the item are shown }
  if (FHandler = nil) and (Item <> nil) then
    LoadInfo(Item);
  Invalidate;
  if (FHandler = nil) and Assigned(FOnPreviewFailed) then
    FOnPreviewFailed(Self, FFileName);
end;

procedure TExplorerPreview.Clear;
begin
  FileName := '';
end;

procedure TExplorerPreview.Refresh;
begin
  LoadPreview;
end;

function TExplorerPreview.GetPreviewing: Boolean;
begin
  Result := FHandler <> nil;
end;

procedure TExplorerPreview.SetEmptyText(const Value: string);
begin
  if Value <> FEmptyText then
  begin
    FEmptyText := Value;
    Invalidate;
  end;
end;

procedure TExplorerPreview.SetFileName(const Value: string);
begin
  if Value = FFileName then
    Exit;
  FFileName := Value;
  Unload;
  ClearInfo;
  Invalidate;
  if HandleAllocated then
    KillTimer(Handle, PreviewTimer);
  if FFileName <> '' then
    StartPreview;
end;

function TExplorerPreview.GetBinding: IComponentBinding;
begin
  Result := FBinding;
end;

procedure TExplorerPreview.SetBinding(Value: IComponentBinding);
begin
  FBinding := Value;
end;

{ TExplorerBinding }

constructor TExplorerBinding.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AcceptClass([TExplorerView, TExplorerTree, TExplorerEdit, TExplorerPreview]);
end;

procedure TExplorerBinding.Preview(Sender: TObject; const FileName: string);
var
  C: TComponent;
  I: Integer;
begin
  for I := 0 to NotifyCount - 1 do
  begin
    C := NotifyComponent[I];
    if (C <> Sender) and (C is TExplorerPreview) then
      TExplorerPreview(C).FileName := FileName;
  end;
end;

procedure TExplorerBinding.DoChange(Sender: TObject; var Param);
var
  pidl: PItemIDList;
  C: TComponent;
  I: Integer;
begin
  pidl := PItemIDList(Param);
  if pidl = nil then
    Exit;
  for I := 0 to NotifyCount - 1 do
  begin
    C := NotifyComponent[I];
    if C = Sender then
      Continue;
    if C is TExplorerView then
      TExplorerView(C).ShowIDList(pidl)
    else if C is TExplorerTree then
      TExplorerTree(C).SelectIDList(pidl)
    else if C is TExplorerEdit then
      TExplorerEdit(C).ShowIDList(pidl)
    else if C is TExplorerPreview then
      TExplorerPreview(C).Clear;
  end;
end;
{$endif}

end.
