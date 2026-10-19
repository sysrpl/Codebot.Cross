(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.interop.windows.shell.txt> }
unit Codebot.Interop.Windows.Shell;

{$i ../codebot/codebot.inc}

interface

{$ifdef windows}
uses
  Windows, Messages, ActiveX, ShlObj;

{ Shell declarations missing from the ShlObj unit of Free Pascal }

const
  IID_ICommDlgBrowser: TGUID = '{000214F1-0000-0000-C000-000000000046}';
  IID_ICommDlgBrowser2: TGUID = '{10339516-2894-11D2-9039-00C04F8EEB3E}';
  IID_ICommDlgBrowser3: TGUID = '{C8AD25A1-3294-41EE-8165-71174BD01C57}';

  { Sent by a shell view to its parent window to find the IShellBrowser }
  WM_GETISHELLBROWSER = WM_USER + 7;

  { Folder flags }
  FWF_AUTOARRANGE = $1;
  FWF_ABBREVIATEDNAMES = $2;
  FWF_SNAPTOGRID = $4;
  FWF_OWNERDATA = $8;
  FWF_BESTFITWINDOW = $10;
  FWF_DESKTOP = $20;
  FWF_SINGLESEL = $40;
  FWF_NOSUBFOLDERS = $80;
  FWF_TRANSPARENT = $100;
  FWF_NOCLIENTEDGE = $200;
  FWF_NOSCROLL = $400;
  FWF_ALIGNLEFT = $800;
  FWF_NOICONS = $1000;
  FWF_SHOWSELALWAYS = $2000;
  FWF_NOVISIBLE = $4000;
  FWF_SINGLECLICKACTIVATE = $8000;
  FWF_NOWEBVIEW = $10000;
  FWF_HIDEFILENAMES = $20000;
  FWF_CHECKSELECT = $40000;
  FWF_NOENUMREFRESH = $80000;
  FWF_NOGROUPING = $100000;
  FWF_FULLROWSELECT = $200000;
  FWF_NOFILTERS = $400000;
  FWF_NOCOLUMNHEADER = $800000;
  FWF_NOHEADERINALLVIEWS = $1000000;
  FWF_EXTENDEDTILES = $2000000;
  FWF_TRICHECKSELECT = $4000000;
  FWF_AUTOCHECKSELECT = $8000000;
  FWF_NOBROWSERVIEWSTATE = $10000000;
  FWF_SUBSETGROUPS = $20000000;
  FWF_USESEARCHFOLDER = $40000000;
  FWF_ALLOWRTLREADING = DWORD($80000000);

  { Folder view modes }
  FVM_ICON = 1;
  FVM_SMALLICON = 2;
  FVM_LIST = 3;
  FVM_DETAILS = 4;
  FVM_THUMBNAIL = 5;
  FVM_TILE = 6;
  FVM_THUMBSTRIP = 7;

  { IShellView.UIActivate states }
  SVUIA_DEACTIVATE = 0;
  SVUIA_ACTIVATE_NOFOCUS = 1;
  SVUIA_ACTIVATE_FOCUS = 2;
  SVUIA_INPLACEACTIVATE = 3;

  { Items of a shell view }
  SVGIO_BACKGROUND = $00000000;
  SVGIO_SELECTION = $00000001;
  SVGIO_ALLVIEW = $00000002;

  { ICommDlgBrowser2 notifications and flags }
  CDB2N_CONTEXTMENU_DONE = $00000001;
  CDB2N_CONTEXTMENU_START = $00000002;
  CDB2GVF_SHOWALLFILES = $00000001;

  { SHChangeNotifyRegister sources }
  SHCNRF_InterruptLevel = $0001;
  SHCNRF_ShellLevel = $0002;
  SHCNRF_RecursiveInterrupt = $1000;
  SHCNRF_NewDelivery = $8000;

  { Alt key state passed to IDropTarget methods }
  MK_ALT = $20;

  { Message used by the shell controls to receive change notifications }
  WM_SHELLNOTIFY = WM_USER + $50;

type
  { The item lists sent in the WParam of WM_SHELLNOTIFY }
  TSHNotifyStruct = record
    Item1: PItemIDList;
    Item2: PItemIDList;
  end;
  PSHNotifyStruct = ^TSHNotifyStruct;

  ICommDlgBrowser = interface(IUnknown)
    ['{000214F1-0000-0000-C000-000000000046}']
    function OnDefaultCommand(const ppshv: IShellView): HResult; stdcall;
    function OnStateChange(const ppshv: IShellView; Change: ULONG): HResult; stdcall;
    function IncludeObject(const ppshv: IShellView; pidl: PItemIDList): HResult; stdcall;
  end;

  ICommDlgBrowser2 = interface(ICommDlgBrowser)
    ['{10339516-2894-11D2-9039-00C04F8EEB3E}']
    function Notify(ppshv: IShellView; dwNotifyType: DWORD): HResult; stdcall;
    function GetDefaultMenuText(ppshv: IShellView; pszText: PWideChar;
      cchMax: Integer): HResult; stdcall;
    function GetViewFlags(out pdwFlags: DWORD): HResult; stdcall;
  end;

  ICommDlgBrowser3 = interface(ICommDlgBrowser2)
    ['{C8AD25A1-3294-41EE-8165-71174BD01C57}']
    function OnColumnClicked(ppshv: IShellView; iColumn: Integer): HResult; stdcall;
    function GetCurrentFilter(pszFileSpec: PWideChar; cchFileSpec: Integer): HResult; stdcall;
    function OnPreViewCreated(ppshv: IShellView): HResult; stdcall;
  end;

{ Explorer browser and namespace tree control declarations added in Windows
  Vista }

const
  CLSID_ExplorerBrowser: TGUID = '{71F96385-DDD6-48D3-A0C1-AE06E8B055FB}';
  CLSID_NamespaceTreeControl: TGUID = '{AE054212-3535-4430-83ED-D501AA6680E6}';
  IID_IExplorerBrowser: TGUID = '{DFD3B6B5-C10C-4BE9-85F6-A66969F402F6}';
  IID_IExplorerBrowserEvents: TGUID = '{361BBDC7-E6EE-4E13-BE58-58E2240C810F}';
  IID_INameSpaceTreeControl: TGUID = '{028212A3-B627-47E9-8856-C14265554E4F}';
  IID_INameSpaceTreeControlEvents: TGUID = '{93D77985-B3D8-4484-8318-672CDDA002CE}';

  { The service an explorer browser host provides to supply ICommDlgBrowser }
  SID_SExplorerBrowserFrame: TGUID = '{000214F1-0000-0000-C000-000000000046}';

  { IShellBrowser.BrowseObject and IExplorerBrowser.BrowseToIDList flags }
  SBSP_DEFBROWSER = $0000;
  SBSP_SAMEBROWSER = $0001;
  SBSP_NEWBROWSER = $0002;
  SBSP_ABSOLUTE = $0000;
  SBSP_RELATIVE = $1000;
  SBSP_PARENT = $2000;
  SBSP_NAVIGATEBACK = $4000;
  SBSP_NAVIGATEFORWARD = $8000;

  { IShellView.SelectItem and IFolderView.SelectItem flags }
  SVSI_DESELECT = $0000;
  SVSI_SELECT = $0001;
  SVSI_EDIT = $0003;
  SVSI_DESELECTOTHERS = $0004;
  SVSI_ENSUREVISIBLE = $0008;
  SVSI_FOCUSED = $0010;

  { Explorer browser options }
  EBO_NONE = $0000;
  EBO_NAVIGATEONCE = $0001;
  EBO_SHOWFRAMES = $0002;
  EBO_ALWAYSNAVIGATE = $0004;
  EBO_NOTRAVELLOG = $0008;
  EBO_NOWRAPPERWINDOW = $0010;
  EBO_HTMLSHAREPOINTVIEW = $0020;
  EBO_NOBORDER = $0040;
  EBO_NOPERSISTVIEWSTATE = $0080;

  { Namespace tree control styles }
  NSTCS_HASEXPANDOS = $00000001;
  NSTCS_HASLINES = $00000002;
  NSTCS_SINGLECLICKEXPAND = $00000004;
  NSTCS_FULLROWSELECT = $00000008;
  NSTCS_SPRINGEXPAND = $00000010;
  NSTCS_HORIZONTALSCROLL = $00000020;
  NSTCS_ROOTHASEXPANDO = $00000040;
  NSTCS_SHOWSELECTIONALWAYS = $00000080;
  NSTCS_NOINFOTIP = $00000200;
  NSTCS_EVENHEIGHT = $00000400;
  NSTCS_NOREPLACEOPEN = $00000800;
  NSTCS_DISABLEDRAGDROP = $00001000;
  NSTCS_NOORDERSTREAM = $00002000;
  NSTCS_RICHTOOLTIP = $00004000;
  NSTCS_BORDER = $00008000;
  NSTCS_NOEDITLABELS = $00010000;
  NSTCS_TABSTOP = $00020000;
  NSTCS_FAVORITESMODE = $00080000;
  NSTCS_AUTOHSCROLL = $00100000;
  NSTCS_FADEINOUTEXPANDOS = $00200000;
  NSTCS_EMPTYTEXT = $00400000;
  NSTCS_CHECKBOXES = $00800000;
  NSTCS_PARTIALCHECKBOXES = $01000000;
  NSTCS_EXCLUSIONCHECKBOXES = $02000000;
  NSTCS_DIMMEDCHECKBOXES = $04000000;
  NSTCS_NOINDENTCHECKS = $08000000;
  NSTCS_ALLOWJUNCTIONS = $10000000;
  NSTCS_SHOWTABSBUTTON = $20000000;
  NSTCS_SHOWDELETEBUTTON = $40000000;
  NSTCS_SHOWREFRESHBUTTON = DWORD($80000000);

  { Namespace tree root styles }
  NSTCRS_VISIBLE = $0000;
  NSTCRS_HIDDEN = $0001;
  NSTCRS_EXPANDED = $0002;

  { Namespace tree item states }
  NSTCIS_NONE = $0000;
  NSTCIS_SELECTED = $0001;
  NSTCIS_EXPANDED = $0002;
  NSTCIS_BOLD = $0004;
  NSTCIS_DISABLED = $0008;
  NSTCIS_SELECTEDNOEXPAND = $0010;

  { Namespace tree GetNextItem directions }
  NSTCGNI_NEXT = 0;
  NSTCGNI_NEXTVISIBLE = 1;
  NSTCGNI_PREV = 2;
  NSTCGNI_PREVVISIBLE = 3;
  NSTCGNI_PARENT = 4;
  NSTCGNI_CHILD = 5;
  NSTCGNI_FIRSTVISIBLE = 6;
  NSTCGNI_LASTVISIBLE = 7;

  { Namespace tree click types }
  NSTCECT_LBUTTON = $0001;
  NSTCECT_MBUTTON = $0002;
  NSTCECT_RBUTTON = $0003;
  NSTCECT_BUTTON = $0003;
  NSTCECT_DBLCLICK = $0004;

  { Namespace tree hit test results }
  NSTCEHT_NOWHERE = $0001;
  NSTCEHT_ONITEMICON = $0002;
  NSTCEHT_ONITEMLABEL = $0004;
  NSTCEHT_ONITEMINDENT = $0008;
  NSTCEHT_ONITEMBUTTON = $0010;
  NSTCEHT_ONITEMRIGHT = $0020;
  NSTCEHT_ONITEMSTATEICON = $0040;
  NSTCEHT_ONITEM = $0046;
  NSTCEHT_ONITEMTABBUTTON = $1000;

type
  IExplorerBrowserEvents = interface(IUnknown)
    ['{361BBDC7-E6EE-4E13-BE58-58E2240C810F}']
    function OnNavigationPending(pidlFolder: PItemIDList): HResult; stdcall;
    function OnViewCreated(psv: IShellView): HResult; stdcall;
    function OnNavigationComplete(pidlFolder: PItemIDList): HResult; stdcall;
    function OnNavigationFailed(pidlFolder: PItemIDList): HResult; stdcall;
  end;

  IExplorerBrowser = interface(IUnknown)
    ['{DFD3B6B5-C10C-4BE9-85F6-A66969F402F6}']
    function Initialize(hwndParent: HWND; prc: PRect; pfs: PFolderSettings): HResult; stdcall;
    function Destroy: HResult; stdcall;
    function SetRect(phdwp: Pointer; rcBrowser: TRect): HResult; stdcall;
    function SetPropertyBag(pszPropertyBag: PWideChar): HResult; stdcall;
    function SetEmptyText(pszEmptyText: PWideChar): HResult; stdcall;
    function SetFolderSettings(pfs: PFolderSettings): HResult; stdcall;
    function Advise(psbe: IExplorerBrowserEvents; out pdwCookie: DWORD): HResult; stdcall;
    function Unadvise(dwCookie: DWORD): HResult; stdcall;
    function SetOptions(dwFlag: DWORD): HResult; stdcall;
    function GetOptions(out pdwFlag: DWORD): HResult; stdcall;
    function BrowseToIDList(pidl: PItemIDList; uFlags: UINT): HResult; stdcall;
    function BrowseToObject(punk: IUnknown; uFlags: UINT): HResult; stdcall;
    function FillFromObject(punk: IUnknown; dwFlags: DWORD): HResult; stdcall;
    function RemoveAll: HResult; stdcall;
    function GetCurrentView(const riid: TGUID; out ppv): HResult; stdcall;
  end;

  INameSpaceTreeControl = interface(IUnknown)
    ['{028212A3-B627-47E9-8856-C14265554E4F}']
    function Initialize(hwndParent: HWND; prc: PRect; nsctsFlags: DWORD): HResult; stdcall;
    function TreeAdvise(punk: IUnknown; out pdwCookie: DWORD): HResult; stdcall;
    function TreeUnadvise(dwCookie: DWORD): HResult; stdcall;
    function AppendRoot(psiRoot: IShellItem; grfEnumFlags: SHCONTF; grfRootStyle: DWORD;
      pif: IShellItemFilter): HResult; stdcall;
    function InsertRoot(iIndex: Integer; psiRoot: IShellItem; grfEnumFlags: SHCONTF;
      grfRootStyle: DWORD; pif: IShellItemFilter): HResult; stdcall;
    function RemoveRoot(psiRoot: IShellItem): HResult; stdcall;
    function RemoveAllRoots: HResult; stdcall;
    function GetRootItems(out ppsiaRootItems: IShellItemArray): HResult; stdcall;
    function SetItemState(psi: IShellItem; nstcisMask: DWORD; nstcisFlags: DWORD): HResult; stdcall;
    function GetItemState(psi: IShellItem; nstcisMask: DWORD; out pnstcisFlags: DWORD): HResult; stdcall;
    function GetSelectedItems(out psiaItems: IShellItemArray): HResult; stdcall;
    function GetItemCustomState(psi: IShellItem; out piStateNumber: Integer): HResult; stdcall;
    function SetItemCustomState(psi: IShellItem; iStateNumber: Integer): HResult; stdcall;
    function EnsureItemVisible(psi: IShellItem): HResult; stdcall;
    function SetTheme(pszTheme: PWideChar): HResult; stdcall;
    function GetNextItem(psi: IShellItem; nstcgi: DWORD; out ppsiNext: IShellItem): HResult; stdcall;
    function HitTest(ppt: PPoint; out ppsiOut: IShellItem): HResult; stdcall;
    function GetItemRect(psi: IShellItem; out prect: TRect): HResult; stdcall;
    function CollapseAll: HResult; stdcall;
  end;

  INameSpaceTreeControlEvents = interface(IUnknown)
    ['{93D77985-B3D8-4484-8318-672CDDA002CE}']
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
  end;

{ Folder view and column declarations added in Windows Vista }

const
  IID_IFolderView2: TGUID = '{1AF3A467-214F-4298-908E-06B03E0B39F9}';
  IID_IColumnManager: TGUID = '{D8EC27BB-3F3B-4042-B10A-4ACFD924D453}';

  { IFolderView2.SetText types }
  FVST_EMPTYTEXT = 0;

  { Sort directions }
  SORT_DESCENDING = -1;
  SORT_ASCENDING = 1;

  { IColumnManager enumeration flags }
  CM_ENUM_ALL = $1;
  CM_ENUM_VISIBLE = $2;

  { IColumnManager column info masks }
  CM_MASK_WIDTH = $1;
  CM_MASK_DEFAULTWIDTH = $2;
  CM_MASK_IDEALWIDTH = $4;
  CM_MASK_NAME = $8;
  CM_MASK_STATE = $10;

type
  TSortColumn = record
    Key: PROPERTYKEY;
    Direction: Integer;
  end;
  PSortColumn = ^TSortColumn;

  TCMColumnInfo = record
    cbSize: DWORD;
    dwMask: DWORD;
    dwState: DWORD;
    uWidth: UINT;
    uDefaultWidth: UINT;
    uIdealWidth: UINT;
    wszName: array[0..79] of WideChar;
  end;
  PCMColumnInfo = ^TCMColumnInfo;

  IFolderView2 = interface(IFolderView)
    ['{1AF3A467-214F-4298-908E-06B03E0B39F9}']
    function SetGroupBy(key: REFPROPERTYKEY; fAscending: BOOL): HResult; stdcall;
    function GetGroupBy(out pkey: PROPERTYKEY; out pfAscending: BOOL): HResult; stdcall;
    function SetViewProperty(pidl: PItemIDList; propkey: REFPROPERTYKEY;
      propvar: Pointer): HResult; stdcall;
    function GetViewProperty(pidl: PItemIDList; propkey: REFPROPERTYKEY;
      ppropvar: Pointer): HResult; stdcall;
    function SetTileViewProperties(pidl: PItemIDList; pszPropList: PWideChar): HResult; stdcall;
    function SetExtendedTileViewProperties(pidl: PItemIDList; pszPropList: PWideChar): HResult; stdcall;
    function SetText(iType: Integer; pwszText: PWideChar): HResult; stdcall;
    function SetCurrentFolderFlags(dwMask: DWORD; dwFlags: DWORD): HResult; stdcall;
    function GetCurrentFolderFlags(out pdwFlags: DWORD): HResult; stdcall;
    function GetSortColumnCount(out pcColumns: Integer): HResult; stdcall;
    function SetSortColumns(rgSortColumns: PSortColumn; cColumns: Integer): HResult; stdcall;
    function GetSortColumns(rgSortColumns: PSortColumn; cColumns: Integer): HResult; stdcall;
    function GetItem(iItem: Integer; const riid: TGUID; out ppv): HResult; stdcall;
    function GetVisibleItem(iStart: Integer; fPrevious: BOOL; out piItem: Integer): HResult; stdcall;
    function GetSelectedItem(iStart: Integer; out piItem: Integer): HResult; stdcall;
    function GetSelection(fNoneImpliesFolder: BOOL; out ppsia: IShellItemArray): HResult; stdcall;
    function GetSelectionState(pidl: PItemIDList; out pdwFlags: DWORD): HResult; stdcall;
    function InvokeVerbOnSelection(pszVerb: PAnsiChar): HResult; stdcall;
    function SetViewModeAndIconSize(uViewMode: UINT; iImageSize: Integer): HResult; stdcall;
    function GetViewModeAndIconSize(out puViewMode: UINT; out piImageSize: Integer): HResult; stdcall;
    function SetGroupSubsetCount(cVisibleRows: UINT): HResult; stdcall;
    function GetGroupSubsetCount(out pcVisibleRows: UINT): HResult; stdcall;
    function SetRedraw(fRedrawOn: BOOL): HResult; stdcall;
    function IsMoveInSameFolder: HResult; stdcall;
    function DoRename: HResult; stdcall;
  end;

  IColumnManager = interface(IUnknown)
    ['{D8EC27BB-3F3B-4042-B10A-4ACFD924D453}']
    function SetColumnInfo(propkey: REFPROPERTYKEY; pcmci: PCMColumnInfo): HResult; stdcall;
    function GetColumnInfo(propkey: REFPROPERTYKEY; pcmci: PCMColumnInfo): HResult; stdcall;
    function GetColumnCount(dwFlags: DWORD; out puCount: UINT): HResult; stdcall;
    function GetColumns(dwFlags: DWORD; rgkeyOrder: REFPROPERTYKEY; cColumns: UINT): HResult; stdcall;
    function SetColumns(rgkeyOrder: REFPROPERTYKEY; cVisible: UINT): HResult; stdcall;
  end;

{ Autocomplete and item images }

const
  SHACF_DEFAULT = $0;
  SHACF_FILESYSTEM = $1;
  SHACF_URLHISTORY = $2;
  SHACF_URLMRU = $4;
  SHACF_URLALL = SHACF_URLHISTORY or SHACF_URLMRU;
  SHACF_USETAB = $8;
  SHACF_FILESYS_ONLY = $10;
  SHACF_FILESYS_DIRS = $20;
  SHACF_VIRTUAL_NAMESPACE = $40;
  SHACF_AUTOSUGGEST_FORCE_ON = $10000000;
  SHACF_AUTOSUGGEST_FORCE_OFF = $20000000;
  SHACF_AUTOAPPEND_FORCE_ON = $40000000;
  SHACF_AUTOAPPEND_FORCE_OFF = $80000000;

  SIIGBF_RESIZETOFIT = $0;
  SIIGBF_BIGGERSIZEOK = $1;
  SIIGBF_MEMORYONLY = $2;
  SIIGBF_ICONONLY = $4;
  SIIGBF_THUMBNAILONLY = $8;
  SIIGBF_INCACHEONLY = $10;

  IID_IShellItemImageFactory: TGUID = '{BCC18B79-BA16-442F-80C4-8A59C30C463B}';

type
  IShellItemImageFactory = interface(IUnknown)
    ['{BCC18B79-BA16-442F-80C4-8A59C30C463B}']
    { Returns a 32 bit bitmap with premultiplied alpha }
    function GetImage(size: TSize; flags: DWORD; out phbm: HBITMAP): HResult; stdcall;
  end;

{ Preview handlers }

const
  { The shell extension key of preview handlers in the registry }
  SPreviewHandlerKey = '{8895B1C6-B41F-4C1C-A562-0D564250836F}';

  BHID_Stream: TGUID = '{1CEBB3AB-7C10-499A-A417-92CA16C4CB83}';

  ASSOCF_INIT_DEFAULTTOSTAR = $4;
  ASSOCSTR_SHELLEXTENSION = 16;

type
  IPreviewHandler = interface(IUnknown)
    ['{8895B1C6-B41F-4C1C-A562-0D564250836F}']
    function SetWindow(hwnd: HWND; prc: PRect): HResult; stdcall;
    function SetRect(prc: PRect): HResult; stdcall;
    function DoPreview: HResult; stdcall;
    function Unload: HResult; stdcall;
    function SetFocus: HResult; stdcall;
    function QueryFocus(out phwnd: HWND): HResult; stdcall;
    function TranslateAccelerator(pmsg: PMsg): HResult; stdcall;
  end;

  IPreviewHandlerVisuals = interface(IUnknown)
    ['{196BF9A5-B346-4EF0-AA1E-5DCDB76768B1}']
    function SetBackgroundColor(color: COLORREF): HResult; stdcall;
    function SetFont(plf: PLogFontW): HResult; stdcall;
    function SetTextColor(color: COLORREF): HResult; stdcall;
  end;

  IInitializeWithStream = interface(IUnknown)
    ['{B824B49D-22AC-4161-AC8A-9916E8FA3F7F}']
    function Initialize(pstream: IStream; grfMode: DWORD): HResult; stdcall;
  end;

  IInitializeWithItem = interface(IUnknown)
    ['{7F73BE3F-FB79-493C-A6C7-7EE14E245841}']
    function Initialize(psi: IShellItem; grfMode: DWORD): HResult; stdcall;
  end;

  IInitializeWithFile = interface(IUnknown)
    ['{B7D14566-0509-4CCE-A71F-0A554233BD9B}']
    function Initialize(pszFilePath: PWideChar; grfMode: DWORD): HResult; stdcall;
  end;

{ Searching }

const
  CLSID_SearchFolderItemFactory: TGUID = '{14010E02-BBBD-41F0-88E3-EDA371216584}';
  CLSID_ConditionFactory: TGUID = '{E03E85B0-7BE3-4000-BA98-6C13DE9FA486}';
  IID_ICondition: TGUID = '{0FC988D4-C935-4B97-A973-46282EA175C8}';

  { Condition operations }
  COP_IMPLICIT = 0;
  COP_EQUAL = 1;
  COP_NOTEQUAL = 2;
  COP_LESSTHAN = 3;
  COP_GREATERTHAN = 4;
  COP_LESSTHANOREQUAL = 5;
  COP_GREATERTHANOREQUAL = 6;
  COP_VALUE_STARTSWITH = 7;
  COP_VALUE_ENDSWITH = 8;
  COP_VALUE_CONTAINS = 9;
  COP_VALUE_NOTCONTAINS = 10;
  COP_DOSWILDCARDS = 11;
  COP_WORD_EQUAL = 12;
  COP_WORD_STARTSWITH = 13;
  COP_APPLICATIONSPECIFIC = 14;

  CONDITION_CREATION_DEFAULT = 0;

type
  { Conditions are passed as IUnknown since only their references are used }

  IConditionFactory = interface(IUnknown)
    ['{A5EFE073-B16F-474F-9F3E-9F8B497A3E08}']
    function MakeNot(pcSub: IUnknown; fSimplify: BOOL; out ppcResult: IUnknown): HResult; stdcall;
    function MakeAndOr(ct: Integer; peuSubs: IUnknown; fSimplify: BOOL;
      out ppcResult: IUnknown): HResult; stdcall;
    function MakeLeaf(pszPropertyName: PWideChar; cop: Integer; pszValueType: PWideChar;
      ppropvar: Pointer; pPropertyNameTerm, pOperationTerm, pValueTerm: IUnknown;
      fExpand: BOOL; out ppcResult: IUnknown): HResult; stdcall;
    function Resolve(pc: IUnknown; sqro: Integer; pstReferenceTime: Pointer;
      out ppcResolved: IUnknown): HResult; stdcall;
  end;

  { Available on Windows 8 and later. Only the methods up to CreateStringLeaf
    are declared }

  IConditionFactory2 = interface(IConditionFactory)
    ['{71D222E1-432F-429E-8C13-B6DAFDE5077A}']
    function CreateTrueFalse(fVal: BOOL; cco: Integer; const riid: TGUID; out ppv): HResult; stdcall;
    function CreateNegation(pcSub: IUnknown; cco: Integer; const riid: TGUID; out ppv): HResult; stdcall;
    function CreateCompoundFromObjectArray(ct: Integer; poaSubs: IUnknown; cco: Integer;
      const riid: TGUID; out ppv): HResult; stdcall;
    function CreateCompoundFromArray(ct: Integer; ppcondSubs: Pointer; cSubs: ULONG;
      cco: Integer; const riid: TGUID; out ppv): HResult; stdcall;
    function CreateStringLeaf(propkey: REFPROPERTYKEY; cop: Integer; pszValue: PWideChar;
      pszLocaleName: PWideChar; cco: Integer; const riid: TGUID; out ppv): HResult; stdcall;
  end;

  ISearchFolderItemFactory = interface(IUnknown)
    ['{A0FFBC28-5482-4366-BE27-3E81E78E06C2}']
    function SetDisplayName(pszDisplayName: PWideChar): HResult; stdcall;
    function SetFolderTypeID(const ftid: TGUID): HResult; stdcall;
    function SetFolderLogicalViewMode(flvm: Integer): HResult; stdcall;
    function SetIconSize(iIconSize: Integer): HResult; stdcall;
    function SetVisibleColumns(cVisibleColumns: UINT; rgKey: Pointer): HResult; stdcall;
    function SetSortColumns(cSortColumns: UINT; rgSortColumns: Pointer): HResult; stdcall;
    function SetGroupColumn(keyGroup: REFPROPERTYKEY): HResult; stdcall;
    function SetStacks(cStackKeys: UINT; rgStackKeys: Pointer): HResult; stdcall;
    function SetScope(psiaScope: IShellItemArray): HResult; stdcall;
    function SetCondition(pCondition: IUnknown): HResult; stdcall;
    function GetShellItem(const riid: TGUID; out ppv): HResult; stdcall;
    function GetIDList(out ppidl: PItemIDList): HResult; stdcall;
  end;

{ Creates a shell item array holding one item }
function SHCreateShellItemArrayFromShellItem(psi: IShellItem; const riid: TGUID;
  out ppv): HResult; stdcall; external 'shell32.dll';
{ Returns a string from the file associations, such as the class of a shell
  extension of a file type }
function AssocQueryStringW(flags: DWORD; str: DWORD; pszAssoc, pszExtra,
  pszOut: PWideChar; var pcchOut: DWORD): HResult; stdcall; external 'shlwapi.dll';

{ File operations }

const
  CLSID_FileOperation: TGUID = '{3AD05575-8857-4850-9277-11B85BDB8E09}';

  FOF_SILENT = $4;
  FOF_RENAMEONCOLLISION = $8;
  FOF_NOCONFIRMATION = $10;
  FOF_ALLOWUNDO = $40;
  FOF_FILESONLY = $80;
  FOF_NOCONFIRMMKDIR = $200;
  FOF_NOERRORUI = $400;

  FOFX_NOSKIPJUNCTIONS = $00010000;
  FOFX_PREFERHARDLINK = $00020000;
  FOFX_SHOWELEVATIONPROMPT = $00040000;
  FOFX_RECYCLEONDELETE = $00080000;
  FOFX_EARLYFAILURE = $00100000;
  FOFX_PRESERVEFILEEXTENSIONS = $00200000;
  FOFX_KEEPNEWERFILE = $00400000;
  FOFX_NOCOPYHOOKS = $00800000;
  FOFX_NOMINIMIZEBOX = $01000000;
  FOFX_MOVEACLSACROSSVOLUMES = $02000000;
  FOFX_DONTDISPLAYSOURCEPATH = $04000000;
  FOFX_DONTDISPLAYDESTPATH = $08000000;
  FOFX_REQUIREELEVATION = $10000000;
  FOFX_ADDUNDORECORD = $20000000;

type
  { Progress sinks, property changes, and item arrays are passed as IUnknown
    since only nil or plain references are used }

  IFileOperation = interface(IUnknown)
    ['{947AAB5F-0A5C-4C13-B4D6-4BF7836FC9F8}']
    function Advise(pfops: IUnknown; out pdwCookie: DWORD): HResult; stdcall;
    function Unadvise(dwCookie: DWORD): HResult; stdcall;
    function SetOperationFlags(dwOperationFlags: DWORD): HResult; stdcall;
    function SetProgressMessage(pszMessage: PWideChar): HResult; stdcall;
    function SetProgressDialog(popd: IUnknown): HResult; stdcall;
    function SetProperties(pproparray: IUnknown): HResult; stdcall;
    function SetOwnerWindow(hwndOwner: HWND): HResult; stdcall;
    function ApplyPropertiesToItem(psiItem: IShellItem): HResult; stdcall;
    function ApplyPropertiesToItems(punkItems: IUnknown): HResult; stdcall;
    function RenameItem(psiItem: IShellItem; pszNewName: PWideChar;
      pfopsItem: IUnknown): HResult; stdcall;
    function RenameItems(pUnkItems: IUnknown; pszNewName: PWideChar): HResult; stdcall;
    function MoveItem(psiItem: IShellItem; psiDestinationFolder: IShellItem;
      pszNewName: PWideChar; pfopsItem: IUnknown): HResult; stdcall;
    function MoveItems(punkItems: IUnknown; psiDestinationFolder: IShellItem): HResult; stdcall;
    function CopyItem(psiItem: IShellItem; psiDestinationFolder: IShellItem;
      pszCopyName: PWideChar; pfopsItem: IUnknown): HResult; stdcall;
    function CopyItems(punkItems: IUnknown; psiDestinationFolder: IShellItem): HResult; stdcall;
    function DeleteItem(psiItem: IShellItem; pfopsItem: IUnknown): HResult; stdcall;
    function DeleteItems(punkItems: IUnknown): HResult; stdcall;
    function NewItem(psiDestinationFolder: IShellItem; dwFileAttributes: DWORD;
      pszName: PWideChar; pszTemplateName: PWideChar; pfopsItem: IUnknown): HResult; stdcall;
    function PerformOperations: HResult; stdcall;
    function GetAnyOperationsAborted(out pfAnyOperationsAborted: BOOL): HResult; stdcall;
  end;

{ Known folders }

const
  CLSID_KnownFolderManager: TGUID = '{4DF0C730-DF9D-4AE3-9153-AA6B82E9795A}';

type
  IKnownFolder = interface(IUnknown)
    ['{3AA7AF7E-9B36-420C-A8E3-F77D4674A488}']
    function GetId(out pkfid: TGUID): HResult; stdcall;
    function GetCategory(out pCategory: Integer): HResult; stdcall;
    function GetShellItem(dwFlags: DWORD; const riid: TGUID; out ppv): HResult; stdcall;
    function GetPath(dwFlags: DWORD; out ppszPath: PWideChar): HResult; stdcall;
    function SetPath(dwFlags: DWORD; pszPath: PWideChar): HResult; stdcall;
    function GetIDList(dwFlags: DWORD; out ppidl: PItemIDList): HResult; stdcall;
  end;

  { Only the methods up to GetFolderByName are declared }

  IKnownFolderManager = interface(IUnknown)
    ['{8BE2D872-86AA-4D47-B776-32CCA40C7018}']
    function FolderIdFromCsidl(nCsidl: Integer; out pfid: TGUID): HResult; stdcall;
    function FolderIdToCsidl(const rfid: TGUID; out pnCsidl: Integer): HResult; stdcall;
    function GetFolderIds(out ppKFId: Pointer; var pCount: UINT): HResult; stdcall;
    function GetFolder(const rfid: TGUID; out ppkf: IKnownFolder): HResult; stdcall;
    function GetFolderByName(pszCanonicalName: PWideChar; out ppkf: IKnownFolder): HResult; stdcall;
  end;

{ Change notifications }

type
  TIDListPair = array[0..1] of PItemIDList;
  PIDListPair = ^TIDListPair;

{ Locks the item lists of a change notification delivered with
  SHCNRF_NewDelivery }
function SHChangeNotification_Lock(hChange: THandle; dwProcId: DWORD;
  out pppidl: PIDListPair; out plEvent: LONG): THandle; stdcall; external 'shell32.dll';
{ Unlocks a change notification }
function SHChangeNotification_Unlock(hLock: THandle): BOOL; stdcall; external 'shell32.dll';

{ Properties }

const
  PDFF_DEFAULT = 0;

{ Returns the property store of an item }
function SHGetPropertyStoreFromIDList(pidl: PItemIDList; flags: Integer;
  const riid: TGUID; out ppv): HResult; stdcall; external 'shell32.dll';
{ Formats a property value as Windows Explorer shows it }
function PSFormatForDisplayAlloc(key: REFPROPERTYKEY; propvar: REFPROPVARIANT;
  pdff: DWORD; out ppszDisplay: PWideChar): HResult; stdcall; external 'propsys.dll';
{ Frees the memory held by a property value }
function PropVariantClear(var pvar: TPropVariant): HResult; stdcall;
  external 'ole32.dll';

{ Formats a number of bytes as a short size such as 12.3 KB }
function StrFormatByteSizeW(qdw: Int64; pszBuf: PWideChar; cchBuf: UINT): PWideChar;
  stdcall; external 'shlwapi.dll';
{ Draws a bitmap with premultiplied alpha }
function AlphaBlend(hdcDest: HDC; xoriginDest, yoriginDest, wDest, hDest: Integer;
  hdcSrc: HDC; xoriginSrc, yoriginSrc, wSrc, hSrc: Integer;
  ftn: TBlendFunction): BOOL; stdcall; external 'msimg32.dll';

{ Adds file system autocomplete to an edit window }
function SHAutoComplete(hwndEdit: HWND; dwFlags: DWORD): HResult; stdcall;
  external 'shlwapi.dll';

{ Returns the key of a property from its canonical name such as
  System.DateModified }
function PSGetPropertyKeyFromName(pszName: PWideChar; out ppropkey: PROPERTYKEY): HResult;
  stdcall; external 'propsys.dll';

{ Creates a shell item such as an IShellItem from an absolute item list }
function SHCreateItemFromIDList(pidl: PItemIDList; const riid: TGUID;
  out ppv): HResult; stdcall; external 'shell32.dll';
{ Returns the item list of a shell object such as an IShellItem }
function SHGetIDListFromObject(punk: IUnknown; out ppidl: PItemIDList): HResult; stdcall;
  external 'shell32.dll';
{$endif}

implementation

end.

