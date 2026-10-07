unit Codebot.Render.Widgets;

{$i render.inc}

interface

uses
  SysUtils,
  Classes,
  Codebot.System,
  Codebot.Platform,
  Codebot.Graphics.Types,
  Codebot.Render.Graphics,
  Codebot.Hardware,
  Codebot.Render.Scenes;

{ TSizeF is a width and height }

type
  TSizeF = TPointF;

{ TWidgetRectHelper adds rounding to whole pixels and sector points }

  TWidgetRectHelper = record helper for TRectF
    { Round the position and size to whole pixels, offset by half a pixel
      when Width is odd so lines of that width are sharp }
    function Round(Width: LongWord = 1): TRectF;
    { Returns a point on the rectangle numbered like a keypad, 1 being the
      top left, 5 the center, and 9 the bottom right }
    function Sector(S: Integer): TPointF;
  end;

{ TWidgetPointHelper adds rounding to whole pixels }

  TWidgetPointHelper = record helper for TPointF
    { Round to whole pixels, offset by half a pixel when Width is odd }
    function Round(Width: LongWord = 1): TPointF;
  end;

{ ARGB converts a $AARRGGBB color value to a TColorF }

function ARGB(Color: LongWord): TColorF;

{ The space between the bounds of an edit and its text }

const
  EditPadding = 6;

{ The thickness of the scroll bars of a memo }

  ScrollBarSize = 14;

{ The most items shown by the list of a spin box of the spinDropScroll kind.
  The rest are scrolled. }

  DropScrollItems = 8;

{ While the mouse is held outside of a list box or scroll grid during a drag
  the widget scrolls on a timer. DragScrollInterval is the time between steps,
  and a step is larger when the mouse is further than DragScrollDistance
  outside of the widget. }

  DragScrollInterval = 0.05;
  DragScrollDistance = 40;

{ The right edge of a header cell of a scroll grid can be dragged from within
  HeaderGrabSize pixels of it. A column can not be dragged narrower than
  HeaderMinWidth. }

  HeaderGrabSize = 4;
  HeaderMinWidth = 24;

{ TThemeColor are theme dependent colors }

type
  TThemeColor = LongWord;

const
  colorBase = TThemeColor(0);
  colorFace = TThemeColor(colorBase + 1);
  colorBorder = TThemeColor(colorFace + 1);
  colorActive = TThemeColor(colorBorder + 1);
  colorPressed = TThemeColor(colorActive + 1);
  colorSelected = TThemeColor(colorPressed + 1);
  colorHot = TThemeColor(colorSelected + 1);
  colorCaption = TThemeColor(colorHot + 1);
  colorTitle = TThemeColor(colorCaption + 1);
  colorText = TThemeColor(colorTitle + 1);
  colorHighlight = TThemeColor(colorText + 1);
  colorShadow = TThemeColor(colorHighlight + 1);
  colorDarkShadow = TThemeColor(colorShadow + 1);

{ TModalButton is used with by main widget message methods }

type
  TModalButton = (mbOkay, mbYes, mbNo, mbAccept, mbCancel);
  { A set of modal buttons }
  TModalButtons = set of TModalButton;
  { TModalResult is the value a modal window closes with, one of the modal
    constants below }
  TModalResult = LongWord;

const
  modalNone = 0;
  modalOk = modalNone + 1;
  modalCancel = modalOk + 1;
  modalYes = modalCancel + 1;
  modalNo = modalYes + 1;
  modalAccept = modalNo + 1;
  modalRetry = modalAccept + 1;
  modalError = modalRetry + 1;
  modelQuit = modalError + 1;

{ The widget system is defined by this unit. Rendering is handled by decedents
  of the theme system class. }

type
  TComputedWidget = class;
  TWidget = class;
  TMainWidget = class;
  TSpacer = class;
  TEdit = class;
  TMemo = class;
  TPushButton = class;
  TGlyphButton = class;
  TGlyphImage = class;
  TCheckBox = class;
  TLabel = class;
  TSlider = class;
  TSpinBox = class;
  TCustomWidget = class;
  TContainerWidget = class;
  THBox = class;
  TVBox = class;
  TWindow = class;

{ Modal result event type }

  TModalResultEvent = procedure(Sender: TObject; ModalResult: TModalResult) of object;
  { TPaintStage is when a widget is painted, before its children or after them }
  TPaintStage = (prePaint, postPaint);

{ TTheme draws the widgets and provides widget part sizes }

  { tpBorder is the thickness of the frame a theme draws around a window, X
    for the left and right sides and Y for the bottom }
  TThemePart = (tpEverything, tpIndent, tpCaption, tpNode, tpThumb, tpBorder);

  TTheme = class
  public
    { Calculate the actual theme color }
    function CalcColor(Widget: TWidget; Color: TThemeColor): TColorF; virtual; abstract;
    { Calculate the size of a widget }
    function CalcSize(Widget: TWidget; Part: TThemePart): TSizeF; virtual; abstract;
		{ Calculate the height of text in the widget }
    function CalcTextHeight: Float; virtual; abstract;
    { Calculate how wide text is when drawn in the widget, including spaces }
    function CalcTextWidth(Widget: TWidget; const Text: string): Float; virtual; abstract;
    { Render the widget }
    procedure Render(Widget: TWidget; Stage: TPaintStage); virtual; abstract;
    { Render a hint for the widget }
    procedure RenderHint(Widget: TWidget; Opacity: Float); virtual; abstract;
    { Calculate where the close button of a window is, relative to the top
      left of the window. An empty rectangle means the theme has none. }
    function CalcCloseRect(Window: TWindow): TRectF; virtual;
    { Draw what follows transformed by a matrix, until PopMatrix. The matrix
      is applied before the transform already in use. }
    procedure PushMatrix(Matrix: IMatrix); virtual;
    { Put the transform back as it was before PushMatrix }
    procedure PopMatrix; virtual;
  end;

  { The class of a theme }
  TThemeClass = class of TTheme;

{ TWidgetStatePart tracks the various stats a widget may contain }

  TWidgetStatePart = (
    { Widget is disabled }
    wsDisabled,
    { Mouse is down and the widget has mouse capture }
    wsPressed,
    { Mouse is hovering over the widget }
    wsHot,
    { The widget has input focus }
    wsSelected,
    { The wiget is in a toggle state such as checked }
    wsToggled);

  { TWidgetState is the set of states a widget is in }
  TWidgetState = set of TWidgetStatePart;

{ TComputedWidget is used to calculate widget properties inherited from a
  list of parent widgets }

  TComputedWidget = class
  private
    FWidget: TWidget;
  public
    constructor Create(Widget: TWidget);
    { The theme of the widget, or of the nearest parent which has one }
    function Theme: TTheme;
    { The opacity of the widget combined with that of its parents }
    function Opacity: Float;
    { The bounds of the widget relative to the main widget }
    function Bounds: TRectF;
    { True if the widget and all of its parents are enabled }
    function Enabled: Boolean;
    { True if the widget and all of its parents are visible }
    function Visible: Boolean;
    { The state of the widget, taking its parents into account }
    function State: TWidgetState;
  end;

{ TWidgetAlign is used when packing a widget inside a container }

  TWidgetAlign = (alignNear, alignCenter, alignFar);
  { A list of widgets }
  TWidgetList = TArrayList<TWidget>;

{ TWidget is the base class for ui controls. A widget is created by giving it
  a parent and rectangle using the create constructor. A widget is destroyed by
  freeing its parent, freeing the widget, or deleteing it from its parent.

  Any children of a widget are destroyed when the widget is destroyed. }

  TWidget = class
  public
    type TWidgetEnumerator = IEnumerator<TWidget>;
    { Enumerates the children of the widget }
    function GetEnumerator: TWidgetEnumerator;
  private
    FAlign: TWidgetAlign;
    FTheme: TTheme;
    FSector: Integer;
    FStayOnTop: Boolean;
    FBounds: TRectF;
    FParent: TWidget;
    FChildren: TWidgetList;
    FComputed: TComputedWidget;
    FHint: string;
    FText: string;
    FName: string;
    FOpacity: Float;
    FEnabled: Boolean;
    FVisible: Boolean;
    FModalResult: TModalResult;
    FUnpacked: Boolean;
    FTag: PtrInt;
    FState: TWidgetState;
    FOnChange: TNotifyEvent;
    FOnClick: TNotifyEvent;
    FOnKeyDown: TSceneKeyEvent;
    FOnKeyUp: TSceneKeyEvent;
    FOnMouseDown: TSceneMouseEvent;
    FOnMouseMove: TSceneMouseEvent;
    FOnMouseUp: TSceneMouseEvent;
    FMargin: Float;
    FIndent: Integer;
    FNeedsPack: Boolean;
    { True when the width or height was set in code. A fixed dimension is not
      replaced with the default size of the theme. }
    FFixedWidth: Boolean;
    FFixedHeight: Boolean;
    FMatrix: IMatrix;
    function TopLevel: TWidget;
    procedure ThemeRender(Stage: TPaintStage);
    procedure ThemeChange;
    function GetMain: TMainWidget;
    function GetContainer: TContainerWidget;
    function GetFirstChild: TWidget;
    function GetLastChild: TWidget;
    function GetDim(Index: Integer): Float;
    procedure SetDim(Index: Integer; const Value: Float);
    procedure SetMargin(Value: Float);
    procedure SetIndent(Value: Integer);
    function GetChildCount: Integer;
    function GetChild(Index: Integer): TWidget;
    procedure SetAlign(Value: TWidgetAlign);
    procedure SetTheme(Value: TTheme);
    procedure SetBounds(const Value: TRectF);
    procedure SetText(const Value: string);
    procedure SetVisible(Value: Boolean);
    procedure SetModalResult(Value: TModalResult);
    procedure SetUnpacked(Value: Boolean);
    procedure MoveFirst;
  protected
    procedure Paint(Stage: TPaintStage); virtual;
    function AutoSize: Boolean; virtual;
    procedure Resize; virtual;
    procedure Repack; virtual;
    function GetBorders: TRectF; virtual;
    function CanSelect: Boolean; virtual;
    procedure AddState(Part: TWidgetStatePart);
    procedure RemoveState(Part: TWidgetStatePart);
    procedure Change; virtual;
    procedure DoModalResult; virtual;
    procedure DoClick; virtual;
    procedure DoKeyDown(var Args: TSceneKeyArgs); virtual;
    procedure DoKeyUp(var Args: TSceneKeyArgs); virtual;
    procedure DoMouseDown(var Args: TSceneMouseArgs); virtual;
    procedure DoMouseMove(var Args: TSceneMouseArgs); virtual;
    procedure DoMouseUp(var Args: TSceneMouseArgs); virtual;
    procedure DoTextInput(var Args: TSceneTextArgs); virtual;
    procedure DoMouseWheel(var Args: TSceneWheelArgs); virtual;
    { Events }
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnKeyDown: TSceneKeyEvent read FOnKeyDown write FOnKeyDown;
    property OnKeyUp: TSceneKeyEvent read FOnKeyUp write FOnKeyUp;
    property OnMouseDown: TSceneMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseMove: TSceneMouseEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseUp: TSceneMouseEvent read FOnMouseUp write FOnMouseUp;
  public
    { Create a widget as a child of a parent, optionally giving it a name }
    constructor Create(Parent: TWidget; const Name: string = ''); virtual;
    destructor Destroy; override;
    { Alias for self }
    function This: TWidget;
    { Add a widget of type T as a child of this widget optinally giving it a name }
    function Add<T: TWidget>(const Name: string = ''): T; overload;
    { Add overload ot type T capturing the new child }
    function Add<T: TWidget>(out Child: T; const Name: string = ''): T; overload;

    { Add a widget of type T as the first child of this widget }
    function AddFirst<T: TWidget>(const Name: string = ''): T; overload;
    { AddFirst overload capturing the new child }
    function AddFirst<T: TWidget>(out Child: T; const Name: string = ''): T; overload;

    { Search for a widget at position x, y }
    function FindWidget(X, Y: Float): TWidget;
    { Search for a widget by name }
    function FindWidget<T: TWidget>(const Name: string): T;
    { Pack arranges child controls returning true if a pack was needed }
    function Pack: Boolean; virtual;
    { Invoke a click on the widget }
    procedure Click;
    { Deletes and destroys the child widget at index }
    procedure Delete(Index: Integer);
    { Returns true if the widget is in the child chain of a parent widget }
    function IsParent(Parent: TWidget): Boolean;
    { If the widget is top level activate it }
    procedure Activate;
    { If the widget is top level bring it to the top of the zorder }
    procedure BringToFront;
    { If the widget is top level send it to the bottom of the zorder }
    procedure SendToBack;
    { Move the widget up or down in zorder }
    procedure ZOrder(Delta: Integer);
    { Calculate a theme color for this widget }
    function Color(Color: TThemeColor): TColorF;
    { The computed properties of the widget }
    property Computed: TComputedWidget read FComputed;
    { Themes can be applied down to the widget level }
    property Theme: TTheme read FTheme write SetTheme;
    { Align is used when packing the widget inside a container }
    property Align: TWidgetAlign read FAlign write SetAlign;
    { If the parent is the main widget, then sector applies a fixed placement }
    property Sector: Integer read FSector write FSector;
    { If the parent is the main widget, then put the wiget above all others }
    property StayOnTop: Boolean read FStayOnTop write FStayOnTop;
    { If the parent is the main widget, then the widget and everything inside
      it is drawn transformed by this matrix, and the mouse is mapped back
      through it, so the widget works the same when it is scaled, rotated,
      or skewed. It is nil by default, which is no transform. }
    property Matrix: IMatrix read FMatrix write FMatrix;
    { A rectangle relative to the partent designating size and position of this widget }
    property Bounds: TRectF read FBounds write SetBounds;
    { If hint is set and the main widgets allows hints, then display a tooltip }
    property Hint: string read FHint write FHint;
    { The main widget to which this widget belongs }
    property Main: TMainWidget read GetMain;
    { The immediate parent of this widget }
    property Parent: TWidget read FParent;
    { The top level contaier of this widget }
    property Container: TContainerWidget read GetContainer;
    { The first child of this widget }
    property FirstChild: TWidget read GetFirstChild;
    { The last child of this widget }
    property LastChild: TWidget read GetLastChild;
    { The number of child widgets }
    property ChildCount: Integer read GetChildCount;
    { Access to child widgets }
    property Child[Index: Integer]: TWidget read GetChild;
    { Position and size properties }
    property X: Float index 0 read GetDim write SetDim;
    property Y: Float index 1 read GetDim write SetDim;
    property Width: Float index 2 read GetDim write SetDim;
    property Height: Float index 3 read GetDim write SetDim;
    { The space kept around the widget when it is packed }
    property Margin: Float read FMargin write SetMargin;
    { The number of indents the widget is moved across when it is packed. The
      theme sets the size of an indent. }
    property Indent: Integer read FIndent write SetIndent;
    { Name can be used to seearch for the widget }
    property Name: string read FName write FName;
    { Opacity of the widget in the range from 0 to 1 }
    property Opacity: Float read FOpacity write FOpacity;
    { The text or caption shown by the widget }
    property Text: string read FText write SetText;
    { When false the widget and its children are not shown }
    property Visible: Boolean read FVisible write SetVisible;
    { When false the widget and its children take no input and are drawn
      disabled }
    property Enabled: Boolean read FEnabled write FEnabled;
    { Singal the parent window with this value if the widget is clicked }
    property ModalResult: TModalResult read FModalResult write SetModalResult;
    { See TWidgetStatePart }
    property State: TWidgetState read FState;
    { When unpacked is true the widget ignores all packing rules }
    property Unpacked: Boolean read FUnpacked write SetUnpacked;
    { User data tag property }
    property Tag: PtrInt read FTag write FTag;
  end;

{ TMainWidget should serve as the base parent widget for your layouts }

  TMainWidget = class(TWidget)
  private
    FShowHint: Boolean;
    FCapture: TWidget;
    FSelected: TWidget;
    FActiveWindow: TWindow;
    FHot: TWidget;
    FTime: Double;
    FHover: Double;
    FModalWindow: TWindow;
    { The spin box whose scrolling list is showing }
    FDropBox: TSpinBox;
    FIsDestroying: Boolean;
    FMousePos: TPointF;
    procedure MessageBoxClose(Sender: TObject; ModalResult: TModalResult);
    procedure MessageProxy(Sender: TObject; ModalResult: TModalResult);
    procedure SetActiveWindow(Value: TWindow);
  protected
    procedure SetModal(Window: TWindow);
    procedure UnsetModal(Window: TWindow);
    { Make the position of a mouse event relative to a widget }
    procedure MouseLocal(Widget: TWidget; var Args: TSceneMouseArgs);
  public
    { The main widget using a theme }
    constructor Create(Theme: TTheme); reintroduce;
    destructor Destroy; override;
    { Modal window message related functions }
    procedure MessageBox(const Message: string);
    { Show a modal message with yes and no buttons, calling OnResult with the
      answer }
    procedure MessageConfirm(const Message: string; OnResult: TModalResultEvent);
    { Show a modal message with a title, a glyph, and a choice of buttons,
      calling OnResult with the button chosen }
    procedure MessageDialog(const Title, Glyph, Message: string; Buttons: TModalButtons; OnResult: TModalResultEvent);
    { Commmunicate events with the widgets }
    procedure DispatchKeyDown(var Args: TSceneKeyArgs);
    procedure DispatchKeyUp(var Args: TSceneKeyArgs);
    procedure DispatchMouseDown(var Args: TSceneMouseArgs);
    procedure DispatchMouseMove(var Args: TSceneMouseArgs);
    procedure DispatchMouseUp(var Args: TSceneMouseArgs);
    procedure DispatchTextInput(var Args: TSceneTextArgs);
    procedure DispatchMouseWheel(var Args: TSceneWheelArgs);
    { The current active window }
    property ActiveWindow: TWindow read FActiveWindow write SetActiveWindow;
    { The current modal window }
    property ModalWindow: TWindow read FModalWindow;
    { The current selected widget with input focus }
    property Selected: TWidget read FSelected;
    { Render all the widgets }
    procedure Render(Width, Height: Integer; Time: Double);
    { The time passed in render above }
    property Time: Double read FTime;
    { The position of the last mouse event dispatched to the widgets }
    property MousePos: TPointF read FMousePos;
    { Map a point from the main widget to where a widget is, which is
      different when the widget is inside of one with a Matrix }
    function WidgetPoint(Widget: TWidget; X, Y: Float): TPointF;
    { MousePos mapped to where a widget is. Compare it with the computed
      bounds of the widget. }
    function MouseFor(Widget: TWidget): TPointF;
    { When show hint is true, hints are displayed if a widget has a hint value }
    property ShowHint: Boolean read FShowHint write FShowHint;
  public
    { The widget which has the mouse while a button is held down }
    property Capture: TWidget read FCapture;
    property OnKeyDown;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

{ TSpacer just gives you some empty space }

  TSpacer = class(TWidget)
  end;



{ TEdit is a single line text box. Text can be typed, selected with the mouse
  or with shift and the arrow keys, and copied, cut, and pasted. Control with
  the arrow keys moves by words, Home and End move to the ends, and Control+A
  selects everything. The text scrolls to keep the caret in view, and the
  caret blinks while the edit has focus.

  Positions are byte offsets into the UTF-8 text, always at the start of a
  character. Themes draw the text EditPadding pixels inside the bounds. }

  TEdit = class(TWidget)
  private
    FCaret: Integer;
    FAnchor: Integer;
    FScroll: Float;
    FBlink: Double;
    FClicked: Boolean;
    FClickTime: Double;
    FClickX: Float;
    FWordSelect: Boolean;
    function GetSelStart: Integer;
    function GetSelLength: Integer;
    function GetSelText: string;
    procedure Validate;
    procedure MoveCaret(Position: Integer; Extend: Boolean);
    procedure ReplaceSelection(const S: string);
    function PositionFromX(X: Float): Integer;
  protected
    function AutoSize: Boolean; override;
    function CanSelect: Boolean; override;
    procedure DoKeyDown(var Args: TSceneKeyArgs); override;
    procedure DoKeyUp(var Args: TSceneKeyArgs); override;
    procedure DoTextInput(var Args: TSceneTextArgs); override;
    procedure DoMouseDown(var Args: TSceneMouseArgs); override;
    procedure DoMouseMove(var Args: TSceneMouseArgs); override;
  public
    { Select all of the text }
    procedure SelectAll;
    { Copy the selected text to the clipboard }
    procedure CopyToClipboard;
    { Copy the selected text to the clipboard and remove it }
    procedure CutToClipboard;
    { Replace the selected text with the text on the clipboard }
    procedure PasteFromClipboard;
    { Themes call ScrollToCaret before drawing with the width available for text }
    procedure ScrollToCaret(TextWidth: Float);
    { Returns the width of the text before a position }
    function OffsetOf(Position: Integer): Float;
    { Returns True when the caret should be drawn in the current blink }
    function CaretVisible: Boolean;
    { The position of the caret }
    property Caret: Integer read FCaret;
    { The position where the selection begins }
    property SelStart: Integer read GetSelStart;
    { The number of bytes selected }
    property SelLength: Integer read GetSelLength;
    { The selected text }
    property SelText: string read GetSelText;
    { The number of pixels the text is scrolled to the left }
    property Scroll: Float read FScroll;
    property Text;
    property OnChange;
    property OnClick;
    property OnKeyDown;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

{ TMemo is a multiple line text box. It has the keyboard, mouse, and
  clipboard features of TEdit, and adds Up, Down, Page Up, and Page Down, which
  select when used with Shift. Control with Home and End moves to the ends of
  the text.

  Lines holds the text as a list of strings. When WordWrap is True lines too
  long to fit continue on the next row. When ScrollBars is True the mouse
  wheel scrolls the text, and a scroll bar is shown for each direction in
  which the text does not fit. Scroll bars are drawn by the theme.

  Positions are byte offsets into the text with lines separated by #10. }

  TMemoRow = record
    { The offset of the first character and the number of bytes in the row }
    Start: Integer;
    Length: Integer;
    { True when the row ends a line, False when it wraps onto the next row }
    Hard: Boolean;
    Width: Float;
  end;

  { An array of memo rows }
  TMemoRows = array of TMemoRow;
  { TMemoBar names the vertical or the horizontal scroll bar }
  TMemoBar = (barVert, barHorz);

  TMemo = class(TWidget)
  private type
    TMouseMode = (mmNone, mmSelect, mmVertThumb, mmHorzThumb, mmTrack);
  private
    FData: string;
    FLines: TStringList;
    FLinesDirty: Boolean;
    FSyncing: Integer;
    FCaret: Integer;
    FAnchor: Integer;
    FDesiredX: Float;
    FScrollX: Float;
    FScrollY: Float;
    FBlink: Double;
    FWordWrap: Boolean;
    FScrollBars: Boolean;
    FRows: TMemoRows;
    FRowsValid: Boolean;
    FRowsWidth: Float;
    FRowsTheme: TTheme;
    FRowHeight: Float;
    FContentWidth: Float;
    FRowsHeight: Float;
    FVertBar: Boolean;
    FHorzBar: Boolean;
    FReveal: Boolean;
    FMouseMode: TMouseMode;
    FDragOffset: Float;
    FClicked: Boolean;
    FClickTime: Double;
    FClickX: Float;
    FClickY: Float;
    function GetLines: TStrings;
    procedure SetLines(Value: TStrings);
    procedure SetWordWrap(Value: Boolean);
    procedure SetScrollBars(Value: Boolean);
    function GetSelStart: Integer;
    function GetSelLength: Integer;
    function GetSelText: string;
    procedure LinesChanged;
    procedure SyncLines;
    procedure Validate;
    procedure LayoutRows;
    procedure BuildRows;
    procedure EnsureRows;
    procedure ClampScroll;
    procedure Reveal;
    function Measure(Start, Stop: Integer): Float;
    function RowOf(Position: Integer): Integer;
    function PositionInRow(Row: Integer; X: Float): Integer;
    function PositionAt(X, Y: Float): Integer;
    procedure MoveCaret(Position: Integer; Extend: Boolean; KeepX: Boolean = False);
    procedure MoveRows(Delta: Integer; Extend: Boolean);
    procedure ReplaceSelection(const S: string);
    procedure DragThumb(Bar: TMemoBar; Position: Float);
  protected
    function AutoSize: Boolean; override;
    function CanSelect: Boolean; override;
    procedure Resize; override;
    procedure DoKeyDown(var Args: TSceneKeyArgs); override;
    procedure DoKeyUp(var Args: TSceneKeyArgs); override;
    procedure DoTextInput(var Args: TSceneTextArgs); override;
    procedure DoMouseDown(var Args: TSceneMouseArgs); override;
    procedure DoMouseMove(var Args: TSceneMouseArgs); override;
    procedure DoMouseUp(var Args: TSceneMouseArgs); override;
    procedure DoMouseWheel(var Args: TSceneWheelArgs); override;
  public
    constructor Create(Parent: TWidget; const Name: string = ''); override;
    destructor Destroy; override;
    { Select all of the text }
    procedure SelectAll;
    { Copy the selected text to the clipboard }
    procedure CopyToClipboard;
    { Copy the selected text to the clipboard and remove it }
    procedure CutToClipboard;
    { Replace the selected text with the text on the clipboard }
    procedure PasteFromClipboard;
    { The methods below are used by themes to draw the memo. Rectangles are
      relative to the top left of the memo. Themes call Prepare first. }
    procedure Prepare;
    { The area holding the text, inside the padding and scroll bars }
    function TextArea: TRectF;
    { True if a scroll bar is showing }
    function BarVisible(Bar: TMemoBar): Boolean;
    { The rectangle of a scroll bar }
    function BarRect(Bar: TMemoBar): TRectF;
    { The rectangle of the thumb of a scroll bar }
    function ThumbRect(Bar: TMemoBar): TRectF;
    { The number of rows, which is more than the number of lines when lines
      wrap }
    function RowCount: Integer;
    { The text of a row }
    function RowText(Row: Integer): string;
    { Returns True if any of the row is selected, giving the left and right
      of the selection measured from the start of the row }
    function RowSelection(Row: Integer; out Left, Right: Float): Boolean;
    { The top of the caret measured from the top left of the unscrolled text }
    function CaretPoint: TPointF;
    { Returns True when the caret should be drawn in the current blink }
    function CaretVisible: Boolean;
    { The height of a row }
    property RowHeight: Float read FRowHeight;
    { The number of pixels the text is scrolled left and up }
    property ScrollX: Float read FScrollX;
    property ScrollY: Float read FScrollY;
    { The position of the caret }
    property Caret: Integer read FCaret;
    { The position where the selection begins }
    property SelStart: Integer read GetSelStart;
    { The number of bytes selected }
    property SelLength: Integer read GetSelLength;
    { The selected text }
    property SelText: string read GetSelText;
    { The text of the memo as a list of lines }
    property Lines: TStrings read GetLines write SetLines;
    { Continue long lines on the next row, True by default }
    property WordWrap: Boolean read FWordWrap write SetWordWrap;
    { Show scroll bars when the text does not fit and scroll with the mouse
      wheel, False by default }
    property ScrollBars: Boolean read FScrollBars write SetScrollBars;
    property OnChange;
    property OnClick;
    property OnKeyDown;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

{ TButton is the base class for buttons. A button can be made to toggle,
  staying down when it is clicked. }

  TButton = class(TWidget)
  private
    FCanToggle: Boolean;
    FDown: Boolean;
    FGroup: Integer;
    procedure SetDown(Value: Boolean);
  protected
    procedure DoClick; override;
  public
    { When true the button stays down when clicked and comes up when clicked
      again }
    property CanToggle: Boolean read FCanToggle write FCanToggle;
    { True while a button which can toggle is down }
    property Down: Boolean read FDown write SetDown;
    { Only one of the toggle buttons with the same parent and the same group
      above zero is down at a time }
    property Group: Integer read FGroup write FGroup;
    property OnChange;
    property OnClick;
    property OnKeyDown;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

{ TPushButton is a button with a text caption }

  TPushButton = class(TButton)
  protected
    function CanSelect: Boolean; override;
  end;

{ TGlyphButton is a button which shows a glyph of the icon font, given as its
  text }

  TGlyphButton = class(TButton)
  end;

{ TGlyphImage shows a glyph of the icon font, given as its text, as large as
  the widget }

  TGlyphImage = class(TWidget)
  public
    property OnClick;
    property OnKeyDown;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

{ TCheckBox is a box with a caption which is checked or unchecked by clicking
  it }

  TCheckBox = class(TWidget)
  private
    FChecked: Boolean;
    FRound: Boolean;
    procedure SetChecked(Value: Boolean);
  protected
    procedure DoClick; override;
    function CanSelect: Boolean; override;
  public
    { True while the box is checked }
    property Checked: Boolean read FChecked write SetChecked;
    { Draw the box as a circle, like a radio button }
    property Round: Boolean read FRound write FRound;
    property OnChange;
    property OnClick;
    property OnKeyDown;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

{ TLabel shows text, either on one line or wrapped to MaxWidth }

  TLabel = class(TWidget)
  private
    FMaxWidth: Float;
    FAssociateText: string;
    procedure SetAssociateText(Value: string);
    procedure SetMaxWidth(Value: Float);
  protected
    { Set FOwnerDraw in a descendant which draws its own text in Paint }
    FOwnerDraw: Boolean;
  public
    { A format string, such as 'Volume: %.1f', which a slider this label is
      the associate of fills in with its position }
    property AssociateText: string read FAssociateText write SetAssociateText;
    { When above zero the text is wrapped to this width }
    property MaxWidth: Float read FMaxWidth write SetMaxWidth;
    { When OwnerDraw is true the theme does not draw the text of the label }
    property OwnerDraw: Boolean read FOwnerDraw;
    property OnClick;
    property OnKeyDown;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

{ TSlider chooses a position between Min and Max by dragging its grip }

  TSlider = class(TWidget)
  private
    FAssociate: TLabel;
    FMin: Float;
    FMax: Float;
    FPosition: Float;
    FStep: Float;
    function GetGripRect: TRectF;
    procedure SetAssociate(Value: TLabel);
    procedure SetMin(Value: Float);
    procedure SetMax(Value: Float);
    procedure SetPosition(Value: Float);
    procedure SetStep(Value: Float);
  protected
    procedure Track(X: Float);
    procedure Change; override;
    procedure DoMouseDown(var Args: TSceneMouseArgs); override;
    procedure DoMouseMove(var Args: TSceneMouseArgs); override;
  public
    constructor Create(Parent: TWidget; const Name: string = ''); override;
    { A label which shows the position of the slider using its AssociateText }
    property Associate: TLabel read FAssociate write SetAssociate;
    { The rectangle of the grip }
    property GripRect: TRectF read GetGripRect;
    { The lowest position }
    property Min: Float read FMin write SetMin;
    { The highest position }
    property Max: Float read FMax write SetMax;
    { The position of the grip }
    property Position: Float read FPosition write SetPosition;
    { The step between positions }
    property Step: Float read FStep write SetStep;
    property OnChange;
    property OnClick;
    property OnKeyDown;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

{ TSpinBox }

{ A spin box shows one of its items and chooses another in a way set by its
  kind.

  spinDropDown drops a list of every item below the box while the mouse is
  held down on it, and the item the mouse is released over is chosen.

  spinSlide chooses the item before or after as the mouse is dragged left
  or right.

  spinDropScroll drops a list when the box is clicked, which stays until an
  item is clicked, the box is clicked again, the box loses the input focus,
  or the mouse is pressed anywhere else. The list shows at most
  DropScrollItems items. When there are more it has a scroll bar, and it is
  scrolled with the mouse wheel or by dragging the scroll bar. With the
  input focus the arrow keys choose the item before or after. }

	TSpinBoxKind = (spinDropDown, spinSlide, spinDropScroll);

  TSpinBox = class(TWidget)
  private
    FItems: StringArray;
    FItemIndex: Integer;
    FPrefix: string;
    FKind: TSpinBoxKind;
    FX: Single;
    FDropped: Boolean;
    FDropScroll: Float;
    FDropThumb: Boolean;
    FDropBar: Boolean;
    FDropOffset: Float;
		procedure SetItems(Value: StringArray);
    procedure SetItemIndex(Value: Integer);
    procedure Track(X: Float);
    procedure DropClamp;
    procedure DropDragThumb(Y: Float);
  protected
    function AutoSize: Boolean; override;
    function CanSelect: Boolean; override;
    procedure Paint(Stage: TPaintStage); override;
    procedure DoKeyDown(var Args: TSceneKeyArgs); override;
    procedure DoMouseDown(var Args: TSceneMouseArgs); override;
    procedure DoMouseMove(var Args: TSceneMouseArgs); override;
    procedure DoMouseUp(var Args: TSceneMouseArgs); override;
    procedure DoMouseWheel(var Args: TSceneWheelArgs); override;
  public
    constructor Create(Parent: TWidget; const Name: string = ''); override;
    destructor Destroy; override;
    { The rectangle of an item in the list of a spinDropDown box }
    function ItemRect(Item: Integer): TRectF;
    { The item of that list at a point }
    function ItemFromPoint(const P: TPointF): Integer;
    { The methods below are for a spin box of the spinDropScroll kind.
      Rectangles are relative to the top left of the spin box, and the list
      is below it. }
    { Show the list, scrolled so the chosen item can be seen }
    procedure DropDown;
    { Hide the list }
    procedure CloseUp;
    { Scroll the list so an item can be seen }
    procedure DropScrollTo(Index: Integer);
    { The height of an item in the list }
    function DropItemHeight: Float;
    { The whole list with its frame, empty if the list is not showing }
    function DropRect: TRectF;
    { The area holding the items, inside the frame and scroll bar }
    function DropArea: TRectF;
    { The rectangle of an item, moved by the scroll position }
    function DropItemRect(Index: Integer): TRectF;
    { The item at a point, or -1 if there is none }
    function DropItemFromPoint(X, Y: Float): Integer;
    { True if the list has a scroll bar }
    function DropBarVisible: Boolean;
    { The rectangle of the scroll bar of the list }
    function DropBarRect: TRectF;
    { The rectangle of the thumb of that scroll bar }
    function DropThumbRect: TRectF;
    { True while the list is showing }
    property Dropped: Boolean read FDropped;
    { The number of pixels the list is scrolled }
    property DropScroll: Float read FDropScroll;
    { Text shown before the chosen item }
    property Prefix: string read FPrefix write FPrefix;
  { The items to choose from }
		property Items: StringArray read FItems write SetItems;
    { The index of the chosen item }
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    { How another item is chosen, see TSpinBoxKind }
    property Kind: TSpinBoxKind read FKind write FKind;
    property OnChange;
    property OnClick;
    property OnKeyDown;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;


{ TListBox shows a list of items, one of which can be selected by clicking it
  or with the Up, Down, Home, End, Prior, and Next keys. Dragging the mouse
  over the items selects the item under it. While the mouse is held above or
  below the items during a drag the list scrolls on a timer, whether or not
  the mouse moves. A vertical scroll bar is shown when the items do
  not fit, and the mouse wheel scrolls the list.
  OnChange fires when ItemIndex changes. The list box keeps the size it is
  given, 200 by 200 by default. }

  TListBox = class(TWidget)
  private
    FItems: StringArray;
    FItemIndex: Integer;
    FHotIndex: Integer;
    FScrollY: Float;
    FDragThumb: Boolean;
    FDragItems: Boolean;
    FDragOffset: Float;
    FDragY: Float;
    FDragTime: Double;
    function GetCount: Integer;
    procedure SetItems(const Value: StringArray);
    procedure SetItemIndex(Value: Integer);
    procedure ClampScroll;
    procedure DragThumb(Y: Float);
    procedure DragItems(Y: Float);
    procedure DragScroll;
  protected
    function AutoSize: Boolean; override;
    function CanSelect: Boolean; override;
    procedure Paint(Stage: TPaintStage); override;
    procedure DoKeyDown(var Args: TSceneKeyArgs); override;
    procedure DoMouseDown(var Args: TSceneMouseArgs); override;
    procedure DoMouseMove(var Args: TSceneMouseArgs); override;
    procedure DoMouseUp(var Args: TSceneMouseArgs); override;
    procedure DoMouseWheel(var Args: TSceneWheelArgs); override;
  public
    constructor Create(Parent: TWidget; const Name: string = ''); override;
    { Scroll the list so the item at index can be seen }
    procedure ScrollToItem(Index: Integer);
    { The methods below are used by themes to draw the list box. Rectangles
      are relative to the top left of the list box. }
    function ItemHeight: Float;
    { The area holding the items, inside the frame and scroll bar }
    function ItemArea: TRectF;
    { The rectangle of an item, moved by the scroll position }
    function ItemRect(Index: Integer): TRectF;
    { The item at a point, or -1 if there is none }
    function ItemFromPoint(X, Y: Float): Integer;
    { True if the scroll bar is showing }
    function BarVisible: Boolean;
    { The rectangle of the scroll bar }
    function BarRect: TRectF;
    { The rectangle of the thumb of the scroll bar }
    function ThumbRect: TRectF;
    { The number of pixels the list is scrolled }
    property ScrollY: Float read FScrollY;
    { The item under the mouse, used while the list box is hot }
    property HotIndex: Integer read FHotIndex;
    { The number of items }
    property Count: Integer read GetCount;
    { The items in the list }
    property Items: StringArray read FItems write SetItems;
    { The index of the selected item, or -1 if none is selected }
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    property OnChange;
    property OnClick;
    property OnKeyDown;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

{ TScrollGrid shows cells in rows and columns, all of the same size. The grid
  draws nothing in a cell itself. OnDrawCell is called for each cell which can
  be seen, with the canvas to draw on, the row and column of the cell, and the
  rectangle of the cell on the canvas. Drawing is clipped to the cell.

  One cell can be selected by clicking it, by dragging over the cells, or
  with the arrow, Home, End, Prior, and Next keys, and OnChange fires when
  the selected cell changes. While the mouse is held past an edge of the
  cells during a drag the grid scrolls on a timer, whether or not the mouse
  moves.

  The theme draws the grid with the frame and background of a list box. It
  draws nothing in a cell, not even a box for the selected cell, so that
  OnDrawCell decides how every cell looks. Selected tells OnDrawCell if a
  cell is the selected cell, and SelectColor, SelectTextColor, and TextColor
  are the colors a list box uses, for drawing cells which match one. Lines
  are drawn between the cells if GridLines is set.

  A vertical or horizontal scroll bar is shown when the cells do not fit. The
  mouse wheel scrolls the rows, or the columns with Shift held. The grid keeps
  the size it is given, 300 by 200 by default.

  Every column is ColWidth wide unless it is given a width of its own with
  ColWidths.

  When HeaderRow is true a header is shown above the cells, with a cell above
  each column holding the title of the column from ColTitles. The header is
  not one of the rows, so row 0 is still the first row of cells. It scrolls
  across with the columns but stays in place as the rows are scrolled, and
  the vertical scroll bar begins below it. The theme draws the header, and a
  header cell is drawn hot while the mouse is over it and pressed while the
  mouse is held down on it. Clicking a header cell fires OnHeaderClick. The
  grid does not sort. SortCol and SortDescending only tell the theme where
  to draw the arrow which shows how the rows were sorted.

  When ColSizing is also true a column is resized by dragging the right edge
  of its header cell, and OnColResize fires as its width changes. HeaderRow
  and ColSizing are false by default. }

  TDrawCellEvent = procedure(Sender: TObject; Surface: ICanvas; Row, Col: Integer;
    const Rect: TRectF) of object;

  { TGridColEvent is an event about one column of a scroll grid }
  TGridColEvent = procedure(Sender: TObject; Col: Integer) of object;

  { TScrollGridDrag is what the mouse is dragging in a scroll grid }
  TScrollGridDrag = (gridDragNone, gridDragVert, gridDragHorz, gridDragCells,
    gridDragHeader, gridDragSize);

  TScrollGrid = class(TWidget)
  private
    FColCount: Integer;
    FRowCount: Integer;
    FColWidth: Float;
    FRowHeight: Float;
    FCol: Integer;
    FRow: Integer;
    FHotCol: Integer;
    FHotRow: Integer;
    FScrollX: Float;
    FScrollY: Float;
    FGridLines: Boolean;
    FTextColor: TColorF;
    FSelectColor: TColorF;
    FSelectTextColor: TColorF;
    FDrag: TScrollGridDrag;
    FDragOffset: Float;
    FDragX: Float;
    FDragY: Float;
    FDragTime: Double;
    { A width of zero means the column is ColWidth wide. The array is empty
      until a column is given a width of its own. }
    FColWidths: array of Float;
    FColTitles: array of string;
    FColAligns: array of TWidgetAlign;
    FHeaderRow: Boolean;
    FHeaderHeight: Float;
    FHeaderHot: Integer;
    FHeaderDown: Integer;
    FColSizing: Boolean;
    FSizeHot: Integer;
    FSizeCol: Integer;
    FSizeCursor: Boolean;
    FSortCol: Integer;
    FSortDescending: Boolean;
    FOnDrawCell: TDrawCellEvent;
    FOnHeaderClick: TGridColEvent;
    FOnColResize: TGridColEvent;
    procedure SetColCount(Value: Integer);
    procedure SetRowCount(Value: Integer);
    procedure SetColWidth(Value: Float);
    procedure SetRowHeight(Value: Float);
    procedure SetCol(Value: Integer);
    procedure SetRow(Value: Integer);
    function GetColWidths(Col: Integer): Float;
    procedure SetColWidths(Col: Integer; Value: Float);
    function GetColTitles(Col: Integer): string;
    procedure SetColTitles(Col: Integer; const Value: string);
    function GetColAligns(Col: Integer): TWidgetAlign;
    procedure SetColAligns(Col: Integer; Value: TWidgetAlign);
    procedure SetHeaderRow(Value: Boolean);
    function GetHeaderHeight: Float;
    procedure SetHeaderHeight(Value: Float);
    function GetHeaderPressed: Integer;
    procedure SetSizeCursor(Value: Boolean);
    function ContentSize(Bar: TMemoBar): Float;
    procedure ClampScroll;
    procedure DragThumb(Bar: TMemoBar; Position: Float);
    procedure DragCells(X, Y: Float);
    procedure DragScroll;
  protected
    function AutoSize: Boolean; override;
    function CanSelect: Boolean; override;
    procedure Paint(Stage: TPaintStage); override;
    procedure DoKeyDown(var Args: TSceneKeyArgs); override;
    procedure DoMouseDown(var Args: TSceneMouseArgs); override;
    procedure DoMouseMove(var Args: TSceneMouseArgs); override;
    procedure DoMouseUp(var Args: TSceneMouseArgs); override;
    procedure DoMouseWheel(var Args: TSceneWheelArgs); override;
  public
    constructor Create(Parent: TWidget; const Name: string = ''); override;
    destructor Destroy; override;
    { Select a cell and scroll it into view. A row or column of -1 selects
      no cell. }
    procedure Select(Col, Row: Integer);
    { Scroll the grid so a cell can be seen }
    procedure ScrollToCell(Col, Row: Integer);
    { DrawCell fires OnDrawCell. It is called by the theme for each cell which
      can be seen. }
    procedure DrawCell(Surface: ICanvas; Row, Col: Integer; const Rect: TRectF);
    { The methods below are used by themes to draw the grid. Rectangles are
      relative to the top left of the grid. }
    { The area holding the cells, inside the frame and scroll bars }
    function CellArea: TRectF;
    { The rectangle of a cell, moved by the scroll position }
    function CellRect(Col, Row: Integer): TRectF;
    { The cell at a point, returning false if there is none }
    function CellFromPoint(X, Y: Float; out Col, Row: Integer): Boolean;
    { True if a scroll bar is showing }
    function BarVisible(Bar: TMemoBar): Boolean;
    { The rectangle of a scroll bar }
    function BarRect(Bar: TMemoBar): TRectF;
    { The rectangle of the thumb of a scroll bar }
    function ThumbRect(Bar: TMemoBar): TRectF;
    { The distance from the left of the first column to the left of a column,
      before scrolling }
    function ColOffset(Col: Integer): Float;
    { The column at a distance from the left of the first column, which is
      ColCount or more when the distance is past the last column }
    function ColFromOffset(X: Float): Integer;
    { The whole header, which is as wide as the grid inside its frame. It is
      empty if there is no header row. }
    function HeaderRect: TRectF;
    { The header cell of a column, moved by the scroll position }
    function HeaderCellRect(Col: Integer): TRectF;
    { The column whose header cell is at a point, or -1 if there is none }
    function HeaderFromPoint(X, Y: Float): Integer;
    { The column whose right edge can be dragged at a point to resize it, or
      -1 if there is none }
    function DividerFromPoint(X, Y: Float): Integer;
    { The number of pixels the cells are scrolled left and up }
    property ScrollX: Float read FScrollX;
    property ScrollY: Float read FScrollY;
    { The cell under the mouse, or -1 if there is none }
    property HotCol: Integer read FHotCol;
    property HotRow: Integer read FHotRow;
    { The number of columns and rows }
    property ColCount: Integer read FColCount write SetColCount;
    property RowCount: Integer read FRowCount write SetRowCount;
    { The width of a column which has no width of its own, and the height of
      every row }
    property ColWidth: Float read FColWidth write SetColWidth;
    property RowHeight: Float read FRowHeight write SetRowHeight;
    { The width of one column. Until it is set a column is ColWidth wide. }
    property ColWidths[Col: Integer]: Float read GetColWidths write SetColWidths;
    { The title shown in the header cell of a column }
    property ColTitles[Col: Integer]: string read GetColTitles write SetColTitles;
    { Where the title is placed in the header cell of a column: alignNear for
      the left, which is the default, alignCenter, or alignFar for the right }
    property ColAligns[Col: Integer]: TWidgetAlign read GetColAligns write SetColAligns;
    { Show a header above the cells, false by default }
    property HeaderRow: Boolean read FHeaderRow write SetHeaderRow;
    { The height of the header, which is zero when there is no header row.
      Until it is set the header is as tall as the text of the theme with
      some room around it. Set it to zero to use that height again. }
    property HeaderHeight: Float read GetHeaderHeight write SetHeaderHeight;
    { Allow columns to be resized by dragging the right edge of their header
      cells, false by default }
    property ColSizing: Boolean read FColSizing write FColSizing;
    { The column whose header cell is under the mouse, or -1 if there is
      none. It is used while the grid is hot. }
    property HeaderHot: Integer read FHeaderHot;
    { The column whose header cell is held down with the mouse over it, or
      -1 if there is none }
    property HeaderPressed: Integer read GetHeaderPressed;
    { The column whose header cell shows a sort arrow, or -1 for none, which
      is the default. The arrow points up unless SortDescending is true. }
    property SortCol: Integer read FSortCol write FSortCol;
    property SortDescending: Boolean read FSortDescending write FSortDescending;
    { The selected cell, or -1 if no cell is selected }
    property Col: Integer read FCol write SetCol;
    property Row: Integer read FRow write SetRow;
    { When GridLines is true the theme draws lines between the cells. It is
      false by default. }
    property GridLines: Boolean read FGridLines write FGridLines;
    { The colors a list box has in the current theme: the color of text, the
      color behind a selected item, and the color of text in a selected item.
      They are set by the theme before OnDrawCell is called, for drawing cells
      which match a list box. }
    property TextColor: TColorF read FTextColor write FTextColor;
    property SelectColor: TColorF read FSelectColor write FSelectColor;
    property SelectTextColor: TColorF read FSelectTextColor write FSelectTextColor;
    { True if a cell is the selected cell }
    function Selected(Row, Col: Integer): Boolean;
    { OnDrawCell fires to draw each cell which can be seen }
    property OnDrawCell: TDrawCellEvent read FOnDrawCell write FOnDrawCell;
    { OnHeaderClick fires when a header cell is clicked }
    property OnHeaderClick: TGridColEvent read FOnHeaderClick write FOnHeaderClick;
    { OnColResize fires as a column is resized by dragging its header cell }
    property OnColResize: TGridColEvent read FOnColResize write FOnColResize;
    property OnChange;
    property OnClick;
    property OnKeyDown;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

{ TCustomWidget is a widget which draws itself in OnPaint, or in Paint when a
  descendant overrides it }

  TCustomWidget = class(TWidget)
  private
    FOnPaint: TNotifyEvent;
  protected
    procedure Paint(Stage: TPaintStage); override;
  public
    { OnPaint fires when the widget should draw itself }
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnClick;
    property OnKeyDown;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

{ TContainerWidget is the base class for widgets which hold and arrange other
  widgets }

  TContainerWidget = class(TWidget)
  private
    FFade: Float;
  public
    { Move the input focus to the next widget in the container, or to the one
      before when Dir is -1 }
    procedure SelectNext(Dir: Integer = 1);
    { How far the background of the container is faded out, from 0 to 1 }
    property Fade: Float read FFade write FFade;
  end;

{ THBox arranges its children in a row from left to right }

  THBox = class(TContainerWidget)
  public
    function Pack: Boolean; override;
    property OnClick;
    property OnKeyDown;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

{ TVBox arranges its children in a column from top to bottom }

  TVBox = class(TContainerWidget)
  public
    function Pack: Boolean; override;
    property OnClick;
    property OnKeyDown;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

{ TWindow is a container with a title bar. It can be dragged, closed, resized,
  and shown as a modal window. }

  TWindow = class(TContainerWidget)
  private
    FDrag: Boolean;
    FDragX, FDragY: Float;
    FNextModal: TWindow;
    FShowingModal: Boolean;
    FOnModalProxy: TModalResultEvent;
    FOnModalResult: TModalResultEvent;
    FCloseButton: Boolean;
    FCloseDown: Boolean;
    FCloseClick: Boolean;
    FOnClose: TNotifyEvent;
    FSizeable: Boolean;
    FSizeWidget: TWidget;
    FSizing: Boolean;
    FSizeX, FSizeY: Float;
    FSizeWidth, FSizeHeight: Float;
    FOnResize: TNotifyEvent;
  protected
    function GetBorders: TRectF; override;
    procedure DoModalResult; override;
    procedure DoClick; override;
    procedure DoMouseDown(var Args: TSceneMouseArgs); override;
    procedure DoMouseMove(var Args: TSceneMouseArgs); override;
    procedure DoMouseUp(var Args: TSceneMouseArgs); override;
  public
    constructor Create(Parent: TWidget; const Name: string = ''); override;
    function Pack: Boolean; override;
    { Show the window and make it the active window }
    procedure Show;
    { Hide the window, which ends a modal window with a cancel result }
    procedure Hide;
    { Close fires OnClose then hides the window, which ends a modal window
      with a cancel result. It is called when the close button is clicked. }
    procedure Close; virtual;
    { The close button rectangle relative to the window, empty if there is none }
    function CloseRect: TRectF;
    { True while the mouse is over the close button }
    function CloseHot: Boolean;
    { True while the close button is held down with the mouse over it }
    function ClosePressed: Boolean;
    { Show a close button in the title bar, True by default. The theme decides
      where it is and how it looks. }
    property CloseButton: Boolean read FCloseButton write FCloseButton;
    { OnClose fires when the window is closed. Do not free the window in it. }
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    { The size grip rectangle relative to the window, empty if there is none }
    function SizeRect: TRectF;
    { A window is as large as the widgets packed inside it, so it is resized
      by resizing one of them. When Sizeable is true a grip is shown at the
      bottom right of the window, and dragging the grip changes the width and
      height of SizeWidget, which the window is then packed around. }
    property Sizeable: Boolean read FSizeable write FSizeable;
    { The widget inside the window which is resized by the size grip }
    property SizeWidget: TWidget read FSizeWidget write FSizeWidget;
    { OnResize fires as SizeWidget is resized by the size grip }
    property OnResize: TNotifyEvent read FOnResize write FOnResize;
    { Causes a window to be shown above other windows returning the status on close }
    procedure ShowModal(OnModalResult: TModalResultEvent);
    property OnClick;
    property OnKeyDown;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

implementation

function ARGB(Color: LongWord): TColorF;
begin
  Result.Alpha := (Color shr 24) / $FF;
  Result.Red := ((Color shr 16) and $FF) / $FF;
  Result.Green := ((Color shr 8) and $FF) / $FF;
  Result.Blue := (Color and $FF) / $FF;
end;

{ TWidgetRectHelper }

function TWidgetRectHelper.Round(Width: LongWord = 1): TRectF;
begin
  Result.X := Trunc(X);
  Result.Y := Trunc(Y);
  Result.Width := System.Round(Self.Width);
  Result.Height := System.Round(Height);
  if Width mod 2 = 1 then
  begin
    Result.X := Result.X + 0.5;
    Result.Y := Result.Y + 0.5;
  end;
end;

{ TWidgetPointHelper }

function TWidgetPointHelper.Round(Width: LongWord = 1): TPointF;
begin
  Result.X := Trunc(X);
  Result.Y := Trunc(Y);
  if Width mod 2 = 1 then
  begin
    Result.X := Result.X + 0.5;
    Result.Y := Result.Y + 0.5;
  end;
end;

function TWidgetRectHelper.Sector(S: Integer): TPointF;
begin
  Result.X := 0;
  Result.Y := 0;
  case S of
    1..3: Result.Y := Top;
    4..6: Result.Y := Height / 2 + Y;
    7..9: Result.Y := Bottom;
  end;
  case S of
    1,4,7: Result.X := Left;
    2,5,8: Result.X := Width / 2 + X;
    3,6,9: Result.X := Right;
  end;
end;

{ TComputedWidget }

constructor TComputedWidget.Create(Widget: TWidget);
begin
  inherited Create;
  FWidget := Widget;
end;

function TComputedWidget.Opacity: Float;
var
  W: TWidget;
begin
  Result := FWidget.FOpacity;
  W := FWidget.FParent;
  while W <> nil do
  begin
    if Result = 0 then Exit;
    Result := Result * W.FOpacity;
    W := W.FParent;
  end;
end;

function TComputedWidget.Theme: TTheme;
var
  W: TWidget;
begin
  Result := FWidget.FTheme;
  W := FWidget.FParent;
  while Result = nil do
  begin
    Result := W.Theme;
    W := W.FParent;
  end;
end;

function TComputedWidget.Bounds: TRectF;
var
  W: TWidget;
begin
  Result := FWidget.FBounds;
  W := FWidget.FParent;
  while W <> nil do
  begin
    Result.X := Result.X + W.FBounds.X;
    Result.Y := Result.Y + W.FBounds.Y;
    W := W.FParent;
  end;
end;

function TComputedWidget.Enabled: Boolean;
var
  W: TWidget;
begin
  Result := FWidget.Enabled;
  W := FWidget.FParent;
  while W <> nil do
  begin
    if not Result then Exit;
    Result := Result and W.Enabled;
    W := W.FParent;
  end;
end;

function TComputedWidget.Visible: Boolean;
var
  W: TWidget;
begin
  Result := FWidget.Visible;
  W := FWidget.FParent;
  while W <> nil do
  begin
    if not Result then Exit;
    Result := Result and W.Visible;
    W := W.FParent;
  end;
end;

function TComputedWidget.State: TWidgetState;
begin
  Result := FWidget.FState;
  if not Enabled then
    Include(Result, wsDisabled);
end;

{ TWidget }

function TWidget.GetEnumerator: TWidgetEnumerator;
begin
  Result := FChildren.GetEnumerator;
end;

constructor TWidget.Create(Parent: TWidget; const Name: string = '');
begin
  inherited Create;
  FParent := Parent;
	FParent.FChildren.Push(Self);
  FChildren.Length := 0;
  FName := Name;
  FVisible := True;
  FEnabled := True;
  FOpacity := 1;
  FMargin := 8;
  FNeedsPack := True;
  FComputed := TComputedWidget.Create(Self);
  Resize;
end;

destructor TWidget.Destroy;
var
  M: TMainWidget;
  I: Integer;
begin
  if FParent <> nil then
  begin
    I := FParent.FChildren.IndexOf(Self);
    if I > -1 then
      FParent.FChildren.Delete(I);
  end;
  for I := FChildren.Length - 1 downto 0 do
    FChildren[I].Free;
  M := Main;
  if (M <> nil) and (not M.FIsDestroying) then
  begin
    if M.FCapture = Self then
      M.FCapture := nil;
    if M.FSelected = Self then
      M.FSelected := nil;
    if M.FHot = Self then
      M.FHot := nil;
    Repack;
  end;
  FComputed.Free;
  inherited Destroy;
end;

function TWidget.AutoSize: Boolean;
begin
  Result := True;
end;

procedure TWidget.Resize;
var
  P: TSizeF;
begin
  P := Main.Theme.CalcSize(Self, tpEverything);
  if (P.X > 0) and (P.Y > 0) then
  begin
    if not FFixedWidth then
      FBounds.Width := P.X;
    if not FFixedHeight then
      FBounds.Height := P.Y;
  end;
  Repack;
end;

procedure TWidget.Repack;
var
  P: TWidget;
begin
  if FChildren.Length > 0 then
    FNeedsPack := True;
  P := Parent;
  while P <> nil do
  begin
    if P is TMainWidget then
      Break;
    P.Repack;
    P := P.Parent;
  end;
end;

{ The widget this one is inside of whose parent is the main widget }

function TWidget.TopLevel: TWidget;
begin
  Result := Self;
  while (Result <> nil) and not (Result.FParent is TMainWidget) do
    Result := Result.FParent;
end;

procedure TWidget.ThemeRender(Stage: TPaintStage);
var
  W: TWidget;
  T: TTheme;
  Transformed: Boolean;
begin
  if Opacity = 0 then Exit;
  if not Visible then Exit;
  if Self is TMainWidget then Exit;
  Pack;
  FNeedsPack := False;
  if Parent is TMainWidget then
    case Sector of
      1:
        begin
          FBounds.X := Margin;
          FBounds.Y := Margin;
        end;
      2:
        begin
          FBounds.X := (Parent.Width - Width) / 2;
          FBounds.Y := Margin;
        end;
      3:
        begin
          FBounds.X := Parent.Width - Width - Margin;
          FBounds.Y := Margin;
        end;
      4:
        begin
          FBounds.X := Margin;
          FBounds.Y := (Parent.Height - Height) / 2;
        end;
      5:
        begin
          FBounds.X := (Parent.Width - Width) / 2;
          FBounds.Y := (Parent.Height - Height) / 2;
        end;
      6:
        begin
          FBounds.X := Parent.Width - Width - Margin;
          FBounds.Y := (Parent.Height - Height) / 2;
        end;
      7:
        begin
          FBounds.X := Margin;
          FBounds.Y := Parent.Height - Height - Margin;
        end;
      8:
        begin
          FBounds.X := (Parent.Width - Width) / 2;
          FBounds.Y := Parent.Height - Height - Margin;
        end;
      9:
        begin
          FBounds.X := Parent.Width - Width - Margin;
          FBounds.Y := Parent.Height - Height - Margin;
        end;
    end;
  T := Computed.Theme;
  Transformed := (FMatrix <> nil) and (Parent is TMainWidget);
  if Transformed then
    T.PushMatrix(FMatrix);
  try
    T.Render(Self, Stage);
    { Widgets which draw themselves do so after the theme has drawn them }
    Paint(Stage);
    for W in FChildren do
      W.ThemeRender(Stage);
  finally
    if Transformed then
      T.PopMatrix;
  end;
end;

procedure TWidget.Paint;
begin
end;

function TWidget.CanSelect: Boolean;
begin
  Result := False; //Computed.Enabled and Computed.Visible;
end;

function TWidget.GetBorders: TRectF;
begin
  Result.X := 0;
  Result.Y := 0;
  Result.Width := 0;
  Result.Height := 0;
end;

procedure TWidget.AddState(Part: TWidgetStatePart);
begin
  Include(FState, Part);
end;

procedure TWidget.RemoveState(Part: TWidgetStatePart);
begin
  Exclude(FState, Part);
end;

procedure TWidget.Change;
begin
  if Assigned(OnChange) then
    FOnChange(Self);
end;

procedure TWidget.DoModalResult;
begin
end;

procedure TWidget.DoClick;
var
  W: TWidget;
begin
  if ModalResult > modalNone then
  begin
    W := Parent;
    while W <> nil do
      if W is TWindow then
      begin
        W.ModalResult := ModalResult;
        Break;
      end
      else
        W := W.Parent;
  end;
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

procedure TWidget.DoKeyDown(var Args: TSceneKeyArgs);
var
  C: TContainerWidget;
begin
  if Assigned(FOnKeyDown) then
    FOnKeyDown(Self, Args);
  if Args.Handled then
    Exit;
  C := Container;
  if C = nil then
    Exit;
  if Args.Key = VK_TAB then
    if skShift in Args.Shift then
      C.SelectNext(-1)
    else
      C.SelectNext(1)
  else if Args.Key = VK_LEFT then
    C.SelectNext(-1)
  else if Args.Key = VK_RIGHT then
    C.SelectNext(1)
  else if Args.Key = VK_UP then
    C.SelectNext(-1)
  else if Args.Key = VK_DOWN then
    C.SelectNext(1);
end;

procedure TWidget.DoKeyUp(var Args: TSceneKeyArgs);
begin
  if Assigned(FOnKeyUp) then
    FOnKeyUp(Self, Args);
  if Args.Handled then
    Exit;
  if Args.Key = VK_RETURN then
    Click
  else if Args.Key = VK_SPACE then
    Click;
end;

procedure TWidget.DoMouseDown(var Args: TSceneMouseArgs);
begin
  if Args.Button = buttonLeft then
    Activate;
  if Assigned(FOnMouseDown) then
    FOnMouseDown(Self, Args);
end;

procedure TWidget.DoMouseMove(var Args: TSceneMouseArgs);
begin
  if Assigned(FOnMouseMove) then
    FOnMouseMove(Self, Args);
end;

procedure TWidget.DoMouseUp(var Args: TSceneMouseArgs);
begin
  if Assigned(FOnMouseUp) then
    FOnMouseUp(Self, Args);
end;

procedure TWidget.DoTextInput(var Args: TSceneTextArgs);
begin
end;

procedure TWidget.DoMouseWheel(var Args: TSceneWheelArgs);
begin
end;

procedure TWidget.Click;
begin
  DoClick;
end;

function TWidget.This: TWidget;
begin
  Result := Self;
end;

function TWidget.Add<T>(const Name: string): T;
begin
  Result := T.Create(Self, Name);
end;

function TWidget.Add<T>(out Child: T; const Name: string = ''): T;
begin
  Child := T.Create(Self, Name);
  Result := Child;
end;

function TWidget.FindWidget(X, Y: Float): TWidget;

  { A widget with a matrix is searched using where the point is before the
    widget is transformed }
  function Find(W: TWidget): TWidget;
  var
    P: TPointF;
  begin
    if (W.FMatrix <> nil) and (Self is TMainWidget) then
    begin
      P := W.FMatrix.Inverse.Multiply(NewPointF(X, Y));
      Result := W.FindWidget(P.X, P.Y);
    end
    else
      Result := W.FindWidget(X, Y);
  end;

var
  W: TWidget;
  R: TRectF;
  I: Integer;
begin
  Result := nil;
  if not FComputed.Visible then
    Exit;
  if not FComputed.Enabled then
    Exit;
  if Self <> Main then
    if Main.FModalWindow <> nil then
      if Self <> Main.FModalWindow then
        if not IsParent(Main.FModalWindow) then
          Exit;
  { The scrolling list of a spin box is outside of the spin box and over
    everything else, so it is found first }
  if (Self is TMainWidget) and (TMainWidget(Self).FDropBox <> nil) then
  begin
    W := TMainWidget(Self).FDropBox;
    if TSpinBox(W).Dropped and W.FComputed.Visible and W.FComputed.Enabled then
    begin
      R := W.FComputed.Bounds;
      with TMainWidget(Self).WidgetPoint(W, X, Y) do
        if TSpinBox(W).DropRect.Contains(X - R.X, Y - R.Y) then
          Exit(W);
    end;
  end;
  { The size grip of a window is found before the widgets inside the window,
    since a widget can reach into the corner where the grip is }
  if Self is TWindow then
  begin
    R := FComputed.Bounds;
    if TWindow(Self).SizeRect.Contains(X - R.X, Y - R.Y) then
      Exit(Self);
  end;
  if FChildren.Length > 0 then
    for I := FChildren.Length - 1 downto 0 do
    begin
      W := FChildren[I];
      if not W.FStayOnTop then
        Continue;
      Result := Find(W);
      if Result <> nil then
        Exit;
    end;
  if FChildren.Length > 0 then
    for I := FChildren.Length - 1 downto 0 do
    begin
      W := FChildren[I];
      if W.FStayOnTop then
        Continue;
      Result := Find(W);
      if Result <> nil then
        Exit;
    end;
  if (not (Self is TMainWidget)) and FComputed.Bounds.Contains(X, Y) then
    Result := Self;
end;

function TWidget.AddFirst<T>(const Name: string = ''): T;
begin
  Result := T.Create(Self, Name);
  MoveFirst;
end;

function TWidget.AddFirst<T>(out Child: T; const Name: string = ''): T;
begin
  Child := T.Create(Self, Name);
  Result := Child;
  MoveFirst;
end;

{ MoveFirst moves the last child, which was just added, to the front }

procedure TWidget.MoveFirst;
var
  I: Integer;
begin
  for I := FChildren.Length - 1 downto 1 do
    FChildren.Exchange(I, I - 1);
end;

function TWidget.FindWidget<T>(const Name: string): T;
var
  W: TWidget;
begin
  Result := Default(T);
  if FChildren.Length > 0 then
    for W in FChildren do
    begin
      Result := W.FindWidget<T>(Name);
      if TObject(Result) <> nil then
        Exit;
    end;
  if (Self.Name = Name) and (Self is T) then
    Result := T(Self);
end;

function TWidget.Pack: Boolean;
var
  W: TWidget;
begin
  Result := FNeedsPack;
  if not Result then
    Exit;
  FNeedsPack := False;
  if FChildren.Length > 1 then
    for W in Self do
      W.Pack;
end;

procedure TWidget.Delete(Index: Integer);
begin
  if Index < 0 then Exit;
  if Index > FChildren.Length - 1 then Exit;
  FChildren[Index].Free;
end;

function TWidget.IsParent(Parent: TWidget): Boolean;
var
  W: TWidget;
begin
  W := Self.Parent;
  while W <> nil do
    if W = Parent then
      Exit(True)
    else
      W := W.Parent;
  Result := False;
end;

procedure TWidget.Activate;
var
  M: TMainWidget;

  function Select(W: TWidget): Boolean;
  var
    C: TWidget;
  begin
    if W.CanSelect then
    begin
      if M.FSelected <> nil then
        M.FSelected.RemoveState(wsSelected);
      M.FSelected := W;
      W.AddState(wsSelected);
      Result := True;
    end
    else for C in W do
      if Select(C) then
        Break;
  end;

  procedure ContainerActivate(C: TWidget);
  begin
    if C.Parent = nil then
      Exit;
    C.Visible := True;
    if C.Parent = M then
      if C is TWindow then
      begin
        C.BringToFront;
        if M.ActiveWindow = TWindow(C) then
          Exit;
        M.ActiveWindow := TWindow(C);
        if M.FSelected <> nil then
          if M.FSelected.IsParent(C) then
            Exit;
        Select(C);
      end
      else
        C.BringToFront
    else
      ContainerActivate(C.Parent)
  end;

begin
  M := Main;
  ContainerActivate(Self);
  if CanSelect then
    Select(Self);
end;

procedure TWidget.BringToFront;
begin
  if Parent = Main then
    ZOrder(Parent.ChildCount)
  else
    Parent.BringToFront;
end;

procedure TWidget.SendToBack;
begin
  if Parent = Main then
    ZOrder(-Parent.ChildCount)
  else
    Parent.SendToBack;
end;

procedure TWidget.ZOrder(Delta: Integer);
var
  M, I: Integer;
begin
  if FParent = nil then Exit;
  if Delta < 0 then
  begin
    I := FParent.FChildren.IndexOf(Self);
    while (I > 0) and (Delta < 0) do
    begin
      FParent.FChildren.Exchange(I, I - 1);
      Dec(I);
      Inc(Delta);
    end;
    Repack;
  end
  else if Delta > 0 then
  begin
    M := FParent.FChildren.Length - 1;
    I := FParent.FChildren.IndexOf(Self);
    while (I < M) and (Delta > 0) do
    begin
      FParent.FChildren.Exchange(I, I + 1);
      Inc(I);
      Dec(Delta);
    end;
    Repack;
  end;
end;

function TWidget.Color(Color: TThemeColor): TColorF;
begin
  Result := Main.Theme.CalcColor(Self, Color);
end;

function TWidget.GetMain: TMainWidget;
var
  W: TWidget;
begin
  Result := nil;
  W := Self;
  while W <> nil do
  begin
    if W is TMainWidget then
      Exit(TMainWidget(W));
    W := W.FParent;
  end;
end;

function TWidget.GetContainer: TContainerWidget;
var
  W: TWidget;
begin
  Result := nil;
  W := Self;
  while W <> nil do
  begin
    if (W is TContainerWidget) and (W.FParent is TMainWidget) then
      Exit(TContainerWidget(W));
    W := W.FParent;
  end;
end;

function TWidget.GetFirstChild: TWidget;
begin
  if FChildren.Length > 0 then
    Result := FChildren.First
  else
    Result := nil;
end;

function TWidget.GetLastChild: TWidget;
begin
  if FChildren.Length > 0 then
    Result := FChildren.Last
  else
    Result := nil;
end;

function TWidget.GetDim(Index: Integer): Float;
begin
  case Index of
    0: Result := FBounds.X;
    1: Result := FBounds.Y;
    2: Result := FBounds.Width;
    3: Result := FBounds.Height;
  else
    Result := 0;
  end;
end;

procedure TWidget.SetDim(Index: Integer; const Value: Float);
begin
  if Self is TMainWidget then Exit;
  { A size set in code is kept when the text or the theme changes }
  if Index = 2 then
    FFixedWidth := True
  else if Index = 3 then
    FFixedHeight := True;
  if Value = GetDim(Index) then Exit;
  case Index of
    0: FBounds.X := Value;
    1: FBounds.Y := Value;
    2:
      begin
        FBounds.Width := Value;
        Repack;
      end;
    3:
      begin
        FBounds.Height := Value;
        Repack;
      end;
  end;
end;

procedure TWidget.SetMargin(Value: Float);
begin
  if Value = FMargin then Exit;
  FMargin := Value;
  Repack;
end;

procedure TWidget.SetIndent(Value: Integer);
begin
  if Value = FIndent then Exit;
  FIndent := Value;
  Repack;
end;

function TWidget.GetChildCount: Integer;
begin
  Result := FChildren.Length;
end;

function TWidget.GetChild(Index: Integer): TWidget;
begin
  Result := FChildren[Index];
end;

procedure TWidget.SetAlign(Value: TWidgetAlign);
begin
  if Value = FAlign then Exit;
  FAlign := Value;
  Repack;
end;

{ ThemeChange resizes the widget and all of its descendants, since every
  size they have came from the previous theme }

procedure TWidget.ThemeChange;
var
  W: TWidget;
begin
  Resize;
  for W in Self do
    W.ThemeChange;
end;

procedure TWidget.SetTheme(Value: TTheme);
begin
  if Value = FTheme then Exit;
  FTheme := Value;
  ThemeChange;
end;

procedure TWidget.SetBounds(const Value: TRectF);
begin
  FBounds := Value;
  FFixedWidth := True;
  FFixedHeight := True;
  Repack;
end;

procedure TWidget.SetText(const Value: string);
begin
  if Value = FText then Exit;
  FText := Value;
  if AutoSize then
	  Resize;
end;

procedure TWidget.SetVisible(Value: Boolean);
var
  W: TWindow;
  C: TWidget;
  I: Integer;
begin
  if Value = FVisible then Exit;
  FVisible := Value;
  Repack;
  if Parent <> Main then
    Exit;
  if not (Self is TWindow) then
    Exit;
  if Main.ModalWindow <> nil then
  begin
    Main.ModalWindow.Activate;
    Exit;
  end;
  W := TWindow(Self);
  if FVisible and (Main.ActiveWindow = nil) then
    W.Activate
  else if (not FVisible) and (Main.ActiveWindow = W) then
  begin
    Main.ActiveWindow := nil;
    for I := Parent.FChildren.Length - 1 downto 0 do
    begin
      C := Parent.FChildren[I];
      if (C is TWindow) and C.Enabled and C.Visible then
      begin
        C.Activate;
        Break;
      end;
    end;
  end;
end;

procedure TWidget.SetModalResult(Value: TModalResult);
begin
  if Value = FModalResult then Exit;
  FModalResult := Value;
  if FModalResult > modalNone then
    DoModalResult;
end;

procedure TWidget.SetUnpacked(Value: Boolean);
begin
  if Value = FUnpacked then Exit;
  FUnpacked := Value;
  Repack;
end;

{ TMainWidget }

constructor TMainWidget.Create(Theme: TTheme);
begin
  FTheme := Theme;
  FShowHint := True;
  FVisible := True;
  FEnabled := True;
  FOpacity := 1;
  FComputed := TComputedWidget.Create(Self);
end;

destructor TMainWidget.Destroy;
begin
  FIsDestroying := True;
  inherited Destroy;
end;

procedure TMainWidget.MessageBoxClose(Sender: TObject; ModalResult: TModalResult);
begin
  Sender.Free;
end;

procedure TMainWidget.MessageProxy(Sender: TObject; ModalResult: TModalResult);
var
  Proxy: TModalResultEvent;
begin
  Proxy := (Sender as TWindow).FOnModalProxy;
  Sender.Free;
  if Assigned(Proxy) then
    Proxy(Self, ModalResult);
end;

procedure TMainWidget.MessageBox(const Message: string);
begin
  with (Add<TWindow>) do
  begin
    with (This.Add<THBox>) do
    begin
      Align := alignCenter;
      with (This.Add<TGlyphImage>) do
      begin
        Align := alignCenter;
        Text := '';
      end;
      with (This.Add<TLabel>) do
      begin
        Align := alignCenter;
        Margin := 20;
        MaxWidth := 400;
        Text := Message;
      end;
    end;
    with (This.Add<TPushButton>) do
    begin
      Align := alignCenter;
      Text := 'OK';
      ModalResult := modalOk;
    end;
    Text := 'Message';
    Pack;
    X := (Self.Width - This.Width) / 2;
    Y := (Self.Height - This.Height) / 2;
    ShowModal(MessageBoxClose);
  end;
end;

procedure TMainWidget.MessageConfirm(const Message: string; OnResult: TModalResultEvent);
begin
  with (Add<TWindow>) do
  begin
    with (This.Add<THBox>) do
    begin
      Align := alignCenter;
      with (This.Add<TGlyphImage>) do
      begin
        Align := alignCenter;
        Text := '󰠗';
      end;
      with (This.Add<TLabel>) do
      begin
        Align := alignCenter;
        Margin := 20;
        MaxWidth := 400;
        Text := Message;
      end;
    end;
    with (This.Add<THBox>) do
    begin
      Align := alignCenter;
      with (This.Add<TPushButton>) do
      begin
        Text := 'Yes';
        ModalResult := modalYes;
      end;
      with (This.Add<TPushButton>) do
      begin
        Text := 'No';
        ModalResult := modalNo;
      end;
    end;
    Text := 'Confirmation';
    Pack;
    X := (Self.Width - This.Width) / 2;
    Y := (Self.Height - This.Height) / 2;
    FOnModalProxy := OnResult;
    ShowModal(MessageProxy);
  end;
end;

procedure TMainWidget.MessageDialog(const Title, Glyph, Message: string;
  Buttons: TModalButtons; OnResult: TModalResultEvent);
var
  B: TModalButton;
begin
  if Buttons = [] then
    Buttons := [mbOkay];
  with (Add<TWindow>) do
  begin
    with (This.Add<THBox>) do
    begin
      Align := alignCenter;
      if Glyph <> '' then
        with (This.Add<TGlyphImage>) do
        begin
          Align := alignCenter;
          Text := Glyph;
        end;
      with (This.Add<TLabel>) do
      begin
        Align := alignCenter;
        Margin := 20;
        MaxWidth := 400;
        Text := Message;
      end;
    end;
    with (This.Add<THBox>) do
    begin
      Align := alignCenter;
      for B := Low(TModalButton) to High(TModalButton) do
        if B in Buttons then
          case B of
            mbOkay:
              with (This.Add<TPushButton>) do
              begin
                Text := 'OK';
                ModalResult := modalok;
              end;
            mbYes:
              with (This.Add<TPushButton>) do
              begin
                Text := 'Yes';
                ModalResult := modalYes;
              end;
            mbNo:
              with (This.Add<TPushButton>) do
              begin
                Text := 'No';
                ModalResult := modalNo;
              end;
            mbAccept:
              with (This.Add<TPushButton>) do
              begin
                Text := 'Accept';
                ModalResult := modalAccept;
              end;
            mbCancel:
              with (This.Add<TPushButton>) do
              begin
                Text := 'Cancel';
                ModalResult := modalCancel;
              end;
          end;
    end;
    Text := Title;
    Pack;
    X := (Self.Width - This.Width) / 2;
    Y := (Self.Height - This.Height) / 2;
    FOnModalProxy := OnResult;
    ShowModal(MessageProxy);
  end;
end;

function TMainWidget.WidgetPoint(Widget: TWidget; X, Y: Float): TPointF;
var
  T: TWidget;
begin
  Result := NewPointF(X, Y);
  if Widget = nil then
    Exit;
  T := Widget.TopLevel;
  if (T <> nil) and (T.FMatrix <> nil) then
    Result := T.FMatrix.Inverse.Multiply(Result);
end;

function TMainWidget.MouseFor(Widget: TWidget): TPointF;
begin
  Result := WidgetPoint(Widget, FMousePos.X, FMousePos.Y);
end;

procedure TMainWidget.MouseLocal(Widget: TWidget; var Args: TSceneMouseArgs);
var
  P: TPointF;
  R: TRectF;
begin
  P := WidgetPoint(Widget, Args.X, Args.Y);
  R := Widget.FComputed.Bounds;
  Args.X := P.X - R.X;
  Args.Y := P.Y - R.Y;
end;

procedure TMainWidget.SetModal(Window: TWindow);
begin
  Window.FShowingModal := True;
  Window.FNextModal := FModalWindow;
  Window.Activate;
  FModalWindow := Window;
end;

procedure TMainWidget.UnsetModal(Window: TWindow);
begin
  Window.FShowingModal := False;
  FModalWindow := Window.FNextModal;
  Window.Visible := False;
end;

procedure TMainWidget.SetActiveWindow(Value: TWindow);
begin
  if Value = FActiveWindow then
    Exit;
  if Value <> nil then
    if Value.Parent <> Self then
      Exit
    else
    begin
      Value.FVisible := True;
      Value.FEnabled := True;
    end;
  FActiveWindow := Value;
  if (FSelected <> FActiveWindow) and (FSelected <> nil) then
  begin
    FSelected.RemoveState(wsSelected);
    FSelected := nil;
  end;
  if (FHot <> FActiveWindow) and (FHot <> nil) then
  begin
    FHot.RemoveState(wsHot);
    FHot := nil;
  end;
  if (FCapture <> FActiveWindow) and (FCapture <> nil) then
  begin
    FCapture.RemoveState(wsPressed);
    FCapture := nil;
  end;
  if (FSelected <> FActiveWindow) and (FSelected <> nil) then
  begin
    FSelected.RemoveState(wsPressed);
    FSelected := nil;
  end;
  if FActiveWindow <> nil then
    FActiveWindow.BringToFront;
end;

procedure TMainWidget.DispatchKeyDown(var Args: TSceneKeyArgs);
begin
  DoKeyDown(Args);
  if Args.Handled then Exit;
  if FSelected <> nil then
    FSelected.DoKeyDown(Args);
end;

procedure TMainWidget.DispatchKeyUp(var Args: TSceneKeyArgs);
begin
  DoKeyUp(Args);
  if Args.Handled then Exit;
  if FSelected <> nil then
    FSelected.DoKeyUp(Args);
end;

procedure TMainWidget.DispatchTextInput(var Args: TSceneTextArgs);
begin
  if FSelected <> nil then
    FSelected.DoTextInput(Args);
end;

{ The wheel goes to the widget under the mouse, then up through its parents
  until one of them handles it }

procedure TMainWidget.DispatchMouseWheel(var Args: TSceneWheelArgs);
var
  W: TWidget;
begin
  W := FindWidget(Args.X, Args.Y);
  while (W <> nil) and not Args.Handled do
  begin
    W.DoMouseWheel(Args);
    W := W.Parent;
  end;
end;

procedure TMainWidget.DispatchMouseDown(var Args: TSceneMouseArgs);
var
  X0, Y0: Float;
  C, S, H: TWidget;
  R: TRectF;
begin
  FMousePos.X := Args.X;
  FMousePos.Y := Args.Y;
  X0 := Args.X;
  Y0 := Args.Y;
  try
    DoMouseDown(Args);
    if Args.Handled then Exit;
    C := nil;
    S := nil;
    H := FindWidget(Args.X, Args.Y);
    { Pressing anywhere else hides the scrolling list of a spin box }
    if (FDropBox <> nil) and (H <> FDropBox) then
      FDropBox.CloseUp;
    if Args.Button = buttonLeft then
    begin
      C := H;
      if (C <> nil) and (C.CanSelect) then
        S := C;
      if S <> nil then
      begin
        if FSelected <> nil then
          FSelected.RemoveState(wsSelected);
        FSelected := S;
        FSelected.AddState(wsSelected);
      end;
    end;
    if FCapture <> nil then
      FCapture.RemoveState(wsPressed);
    if FHot <> nil then
      FHot.RemoveState(wsHot);
    FCapture := C;
    FHot := H;
    if FHot <> nil then
    begin
      R := H.FComputed.Bounds;
      MouseLocal(H, Args);
      H.DoMouseDown(Args);
      FHot := H;
      FHot.AddState(wsHot);
    end;
    FCapture := C;
    if FCapture <> nil then
    begin
      Args.Handled := True;
      FCapture.AddState(wsPressed);
    end;
    if (FActiveWindow <> nil) and (S <> nil) and S.IsParent(FActiveWindow) then
    begin
      if FSelected <> nil then
        FSelected.RemoveState(wsSelected);
      FSelected := S;
      FSelected.AddState(wsSelected);
    end;
  finally
    Args.X := X0;
    Args.Y := Y0;
  end;
end;

procedure TMainWidget.DispatchMouseMove(var Args: TSceneMouseArgs);
var
  X0, Y0: Float;
  H: TWidget;
  R: TRectF;
begin
  FMousePos.X := Args.X;
  FMousePos.Y := Args.Y;
  X0 := Args.X;
  Y0 := Args.Y;
  try
    DoMouseMove(Args);
    if Args.Handled then Exit;
    FHover := Time;
    if FCapture <> nil then
    begin
      Args.Handled := True;
      R := FCapture.FComputed.Bounds;
      H := FindWidget(Args.X, Args.Y);
      if H <> FCapture then
      begin
        if FHot <> nil then
          FHot.RemoveState(wsHot);
        FHot := nil;
      end
      else
      begin
        FHot := FCapture;
        FCapture.AddState(wsHot);
      end;
      MouseLocal(FCapture, Args);
      FCapture.DoMouseMove(Args);
      Exit;
    end;
    H := FindWidget(Args.X, Args.Y);
    if FHot <> nil then
      FHot.RemoveState(wsHot);
    FHot := H;
    if FHot <> nil then
      FHot.AddState(wsHot);
    if H <> nil then
    begin
      R := H.FComputed.Bounds;
      MouseLocal(H, Args);
      H.DoMouseMove(Args);
    end;
  finally
    Args.X := X0;
    Args.Y := Y0;
  end;
end;

procedure TMainWidget.DispatchMouseUp(var Args: TSceneMouseArgs);
var
  X0, Y0: Float;
  H, C: TWidget;
  R: TRectF;
begin
  FMousePos.X := Args.X;
  FMousePos.Y := Args.Y;
  X0 := Args.X;
  Y0 := Args.Y;
  try
    DoMouseUp(Args);
    if Args.Handled then Exit;
    H := FindWidget(Args.X, Args.Y);
    if FHot <> nil then
      FHot.RemoveState(wsHot);
    FHot := H;
    if FHot <> nil then
      FHot.AddState(wsHot);
    if (Args.Button = buttonLeft) and (FCapture <> nil) then
    begin
      Args.Handled := True;
      C := FCapture;
      FCapture := nil;
      C.RemoveState(wsPressed);
      R := C.FComputed.Bounds;
      MouseLocal(C, Args);
      C.DoMouseUp(Args);
      if C = H then
        C.Click;
    end
    else if H <> nil then
    begin
      R := H.FComputed.Bounds;
      MouseLocal(H, Args);
      H.DoMouseUp(Args);
    end;
  finally
    Args.X := X0;
    Args.Y := Y0;
  end;
end;

procedure TMainWidget.Render(Width, Height: Integer; Time: Double);
const
  HintTime = 0.5;
var
  W: TWidget;
  H: Double;
begin
  FTime := Time;
  FBounds.X := 0;
  FBounds.Y := 0;
  FBounds.Width := Width;
  FBounds.Height := Height;
  if FOpacity = 0 then Exit;
  if not FVisible then Exit;
  for W in FChildren do
    if (not W.FStayOnTop) and (W <> FModalWindow) then
      W.ThemeRender(prePaint);
  for W in FChildren do
    if W.FStayOnTop  and (W <> FModalWindow) then
      W.ThemeRender(prePaint);
  for W in FChildren do
    if W = FModalWindow then
      W.ThemeRender(prePaint);

  for W in FChildren do
    if (not W.FStayOnTop) and (W <> FModalWindow) then
      W.ThemeRender(postPaint);
  for W in FChildren do
    if W.FStayOnTop  and (W <> FModalWindow) then
      W.ThemeRender(postPaint);
  for W in FChildren do
    if W = FModalWindow then
      W.ThemeRender(postPaint);

  if (not ShowHint) or (FHot = nil) then
    Exit;
  H := Time - FHover;
  if H < HintTime then
    Exit;
  H := (H - HintTime) / 0.3;
  if H > 1 then
    H := 1;
  if FHot.Hint <> '' then
  begin
    { The hint of a widget inside a transformed widget is transformed too }
    W := FHot.TopLevel;
    if (W <> nil) and (W.FMatrix <> nil) then
    begin
      FHot.Computed.Theme.PushMatrix(W.FMatrix);
      try
        FHot.Computed.Theme.RenderHint(FHot, H);
      finally
        FHot.Computed.Theme.PopMatrix;
      end;
    end
    else
      FHot.Computed.Theme.RenderHint(FHot, H);
  end;
end;

{ TEdit }

const
  BlinkPeriod = 1.0;
  { Clicks this close in time and distance are counted together. Some hosts
    send two mouse downs for the second click of a double click, the click
    itself and a double click notice, so every click after the first one
    selects the word. }
  DoubleClickTime = 0.4;
  DoubleClickDistance = 4;

var
  { The clipboard used when there is no scene host }
  LocalClipboard: string;

function NextChar(const S: string; I: Integer): Integer;
begin
  Result := I;
  if Result >= Length(S) then
    Exit(Length(S));
  Inc(Result);
  while (Result < Length(S)) and (Ord(S[Result + 1]) and $C0 = $80) do
    Inc(Result);
end;

function PrevChar(const S: string; I: Integer): Integer;
begin
  Result := I;
  if Result <= 0 then
    Exit(0);
  Dec(Result);
  while (Result > 0) and (Ord(S[Result + 1]) and $C0 = $80) do
    Dec(Result);
end;

{ CharKind groups the character at a position into spaces (0), word
  characters (1), punctuation (2), and line breaks (3) for moving by words }

function CharKind(const S: string; I: Integer): Integer;
var
  C: Char;
begin
  C := S[I + 1];
  if C in [' ', #9] then
    Result := 0
  else if (C in ['A'..'Z', 'a'..'z', '0'..'9', '_']) or (Ord(C) >= $80) then
    Result := 1
  else if C = #10 then
    Result := 3
  else
    Result := 2;
end;

function NextWord(const S: string; I: Integer): Integer;
var
  K: Integer;
begin
  Result := I;
  if Result >= Length(S) then
    Exit(Length(S));
  K := CharKind(S, Result);
  if K > 0 then
    while (Result < Length(S)) and (CharKind(S, Result) = K) do
      Result := NextChar(S, Result);
  while (Result < Length(S)) and (CharKind(S, Result) = 0) do
    Result := NextChar(S, Result);
end;

function PrevWord(const S: string; I: Integer): Integer;
var
  K: Integer;
begin
  Result := I;
  while (Result > 0) and (CharKind(S, PrevChar(S, Result)) = 0) do
    Result := PrevChar(S, Result);
  if Result = 0 then
    Exit;
  K := CharKind(S, PrevChar(S, Result));
  while (Result > 0) and (CharKind(S, PrevChar(S, Result)) = K) do
    Result := PrevChar(S, Result);
end;

{ WordRange finds the word around a position, which is the run of characters
  of the same kind. A position in the gap after a word belongs to that word. }

procedure WordRange(const S: string; Position: Integer; out Start, Stop: Integer);
var
  L, I, K: Integer;
begin
  L := Length(S);
  I := Position;
  if I > L then
    I := L;
  if I < 0 then
    I := 0;
  Start := I;
  Stop := I;
  if L = 0 then
    Exit;
  if (I >= L) or (CharKind(S, I) in [0, 3]) then
    if (I > 0) and not (CharKind(S, PrevChar(S, I)) in [0, 3]) then
      I := PrevChar(S, I);
  if I >= L then
    Exit;
  K := CharKind(S, I);
  { A line break is not part of any word }
  if K = 3 then
    Exit;
  Start := I;
  while (Start > 0) and (CharKind(S, PrevChar(S, Start)) = K) do
    Start := PrevChar(S, Start);
  Stop := I;
  while (Stop < L) and (CharKind(S, Stop) = K) do
    Stop := NextChar(S, Stop);
end;

function SingleLine(const S: string): string;
begin
  Result := StringReplace(S, #13#10, ' ', [rfReplaceAll]);
  Result := StringReplace(Result, #10, ' ', [rfReplaceAll]);
  Result := StringReplace(Result, #13, ' ', [rfReplaceAll]);
end;

function TEdit.AutoSize: Boolean;
begin
  Result := False;
end;

function TEdit.CanSelect: Boolean;
begin
  Result := Computed.Enabled and Computed.Visible;
end;

function TEdit.GetSelStart: Integer;
begin
  Validate;
  if FCaret < FAnchor then
    Result := FCaret
  else
    Result := FAnchor;
end;

function TEdit.GetSelLength: Integer;
begin
  Validate;
  Result := Abs(FCaret - FAnchor);
end;

function TEdit.GetSelText: string;
begin
  Result := Copy(Text, SelStart + 1, SelLength);
end;

{ Validate keeps the caret and anchor inside the text, which can be changed
  through the Text property at any time }

procedure TEdit.Validate;
begin
  if FCaret > Length(Text) then
    FCaret := Length(Text);
  if FAnchor > Length(Text) then
    FAnchor := Length(Text);
  if FCaret < 0 then
    FCaret := 0;
  if FAnchor < 0 then
    FAnchor := 0;
end;

procedure TEdit.MoveCaret(Position: Integer; Extend: Boolean);
begin
  FCaret := Position;
  if not Extend then
    FAnchor := Position;
  Validate;
  if Main <> nil then
    FBlink := Main.Time;
end;

procedure TEdit.ReplaceSelection(const S: string);
var
  Start: Integer;
  T: string;
begin
  Start := SelStart;
  T := Text;
  System.Delete(T, Start + 1, SelLength);
  System.Insert(S, T, Start + 1);
  Text := T;
  MoveCaret(Start + Length(S), False);
  Change;
end;

function TEdit.PositionFromX(X: Float): Integer;
var
  Target, Left, Right: Float;
  I, N: Integer;
begin
  Target := X - EditPadding + FScroll;
  if Target <= 0 then
    Exit(0);
  I := 0;
  Left := 0;
  while I < Length(Text) do
  begin
    N := NextChar(Text, I);
    Right := OffsetOf(N);
    if Target < Right then
    begin
      if Target - Left < Right - Target then
        Exit(I)
      else
        Exit(N);
    end;
    I := N;
    Left := Right;
  end;
  Result := Length(Text);
end;

function TEdit.OffsetOf(Position: Integer): Float;
begin
  if Position <= 0 then
    Exit(0);
  Result := Computed.Theme.CalcTextWidth(Self, Copy(Text, 1, Position));
end;

procedure TEdit.ScrollToCaret(TextWidth: Float);
var
  C, T: Float;
begin
  Validate;
  C := OffsetOf(FCaret);
  if C - FScroll > TextWidth then
    FScroll := C - TextWidth;
  if C - FScroll < 0 then
    FScroll := C;
  { Do not leave empty space at the end when the text can fill the box }
  T := OffsetOf(Length(Text));
  if T - FScroll < TextWidth then
    FScroll := T - TextWidth;
  if FScroll < 0 then
    FScroll := 0;
end;

function TEdit.CaretVisible: Boolean;
var
  T: Double;
begin
  Result := (wsSelected in State) and Computed.Enabled and (Main <> nil);
  if Result then
  begin
    T := (Main.Time - FBlink) / BlinkPeriod;
    Result := T - Int(T) < 0.5;
  end;
end;

procedure TEdit.SelectAll;
begin
  FAnchor := 0;
  MoveCaret(Length(Text), True);
end;

procedure TEdit.CopyToClipboard;
begin
  if SelLength = 0 then
    Exit;
  if SceneHost <> nil then
    SceneHost.Clipboard := SelText
  else
    LocalClipboard := SelText;
end;

procedure TEdit.CutToClipboard;
begin
  if SelLength = 0 then
    Exit;
  CopyToClipboard;
  ReplaceSelection('');
end;

procedure TEdit.PasteFromClipboard;
var
  S: string;
begin
  if SceneHost <> nil then
    S := SceneHost.Clipboard
  else
    S := LocalClipboard;
  S := SingleLine(S);
  if (S <> '') or (SelLength > 0) then
    ReplaceSelection(S);
end;

procedure TEdit.DoKeyDown(var Args: TSceneKeyArgs);
var
  Ctrl, Extend: Boolean;
  C: TContainerWidget;
begin
  if Assigned(OnKeyDown) then
    OnKeyDown(Self, Args);
  if Args.Handled then
    Exit;
  Ctrl := skCtrl in Args.Shift;
  Extend := skShift in Args.Shift;
  Args.Handled := True;
  case Args.Key of
    VK_LEFT:
      if Ctrl then
        MoveCaret(PrevWord(Text, FCaret), Extend)
      else if (SelLength > 0) and not Extend then
        MoveCaret(SelStart, False)
      else
        MoveCaret(PrevChar(Text, FCaret), Extend);
    VK_RIGHT:
      if Ctrl then
        MoveCaret(NextWord(Text, FCaret), Extend)
      else if (SelLength > 0) and not Extend then
        MoveCaret(SelStart + SelLength, False)
      else
        MoveCaret(NextChar(Text, FCaret), Extend);
    VK_HOME: MoveCaret(0, Extend);
    VK_END: MoveCaret(Length(Text), Extend);
    VK_BACK:
      begin
        if SelLength = 0 then
          if Ctrl then
            FAnchor := PrevWord(Text, FCaret)
          else
            FAnchor := PrevChar(Text, FCaret);
        if SelLength > 0 then
          ReplaceSelection('');
      end;
    VK_DELETE:
      if Extend and not Ctrl then
        CutToClipboard
      else
      begin
        if SelLength = 0 then
          if Ctrl then
            FAnchor := NextWord(Text, FCaret)
          else
            FAnchor := NextChar(Text, FCaret);
        if SelLength > 0 then
          ReplaceSelection('');
      end;
    VK_INSERT:
      if Ctrl then
        CopyToClipboard
      else if Extend then
        PasteFromClipboard;
    VK_A:
      if Ctrl then
        SelectAll
      else
        Args.Handled := False;
    VK_C:
      if Ctrl then
        CopyToClipboard
      else
        Args.Handled := False;
    VK_X:
      if Ctrl then
        CutToClipboard
      else
        Args.Handled := False;
    VK_V:
      if Ctrl then
        PasteFromClipboard
      else
        Args.Handled := False;
    VK_TAB, VK_UP, VK_DOWN:
      begin
        { Move focus between widgets like other widgets do }
        C := Container;
        if C <> nil then
          if (Args.Key = VK_UP) or ((Args.Key = VK_TAB) and Extend) then
            C.SelectNext(-1)
          else
            C.SelectNext(1);
      end;
  else
    Args.Handled := False;
  end;
end;

procedure TEdit.DoKeyUp(var Args: TSceneKeyArgs);
begin
  { Space is typed into the text, so only return clicks the edit }
  if Assigned(OnKeyUp) then
    OnKeyUp(Self, Args);
  if Args.Handled then
    Exit;
  if Args.Key = VK_RETURN then
    Click;
end;

procedure TEdit.DoTextInput(var Args: TSceneTextArgs);
begin
  inherited DoTextInput(Args);
  if Args.Handled or (Args.Text = '') then
    Exit;
  ReplaceSelection(SingleLine(Args.Text));
  Args.Handled := True;
end;

procedure TEdit.DoMouseDown(var Args: TSceneMouseArgs);
var
  Time: Double;
  A, B: Integer;
begin
  inherited DoMouseDown(Args);
  if Args.Button <> buttonLeft then
    Exit;
  Time := 0;
  if Main <> nil then
    Time := Main.Time;
  if FClicked and (Time - FClickTime < DoubleClickTime) and
    (Abs(Args.X - FClickX) < DoubleClickDistance) and not (skShift in Args.Shift) then
  begin
    { A double click selects the word under the mouse }
    WordRange(Text, PositionFromX(Args.X), A, B);
    FAnchor := A;
    MoveCaret(B, True);
    FWordSelect := True;
  end
  else
  begin
    FWordSelect := False;
    MoveCaret(PositionFromX(Args.X), skShift in Args.Shift);
    FClickX := Args.X;
  end;
  FClicked := True;
  FClickTime := Time;
end;

procedure TEdit.DoMouseMove(var Args: TSceneMouseArgs);
begin
  inherited DoMouseMove(Args);
  { The word selected by a double click is kept until the next click }
  if (wsPressed in State) and not FWordSelect then
    MoveCaret(PositionFromX(Args.X), True);
end;

{ TMemo }

type
  TMemoStrings = class(TStringList)
  private
    FMemo: TMemo;
  protected
    procedure Changed; override;
  end;

procedure TMemoStrings.Changed;
begin
  inherited Changed;
  if FMemo.FSyncing = 0 then
    FMemo.LinesChanged;
end;

function MultiLine(const S: string): string;
begin
  Result := StringReplace(S, #13#10, #10, [rfReplaceAll]);
  Result := StringReplace(Result, #13, #10, [rfReplaceAll]);
end;

constructor TMemo.Create(Parent: TWidget; const Name: string = '');
begin
  inherited Create(Parent, Name);
  FLines := TMemoStrings.Create;
  TMemoStrings(FLines).FMemo := Self;
  FWordWrap := True;
  FDesiredX := -1;
end;

destructor TMemo.Destroy;
begin
  FLines.Free;
  inherited Destroy;
end;

function TMemo.AutoSize: Boolean;
begin
  Result := False;
end;

function TMemo.CanSelect: Boolean;
begin
  Result := Computed.Enabled and Computed.Visible;
end;

procedure TMemo.Resize;
begin
  inherited Resize;
  FRowsValid := False;
end;

{ The list of lines is only brought up to date when it is asked for, since
  the text changes with every key typed }

function TMemo.GetLines: TStrings;
begin
  if FLinesDirty then
    SyncLines;
  Result := FLines;
end;

procedure TMemo.SetLines(Value: TStrings);
begin
  Lines.Assign(Value);
end;

procedure TMemo.SyncLines;
var
  I, S: Integer;
begin
  Inc(FSyncing);
  try
    FLines.BeginUpdate;
    try
      FLines.Clear;
      if FData <> '' then
      begin
        S := 1;
        for I := 1 to Length(FData) do
          if FData[I] = #10 then
          begin
            FLines.Add(Copy(FData, S, I - S));
            S := I + 1;
          end;
        FLines.Add(Copy(FData, S, Length(FData) - S + 1));
      end;
    finally
      FLines.EndUpdate;
    end;
  finally
    Dec(FSyncing);
  end;
  FLinesDirty := False;
end;

{ LinesChanged is called when the list of lines is changed by the user }

procedure TMemo.LinesChanged;
var
  S: string;
  I: Integer;
begin
  S := '';
  for I := 0 to FLines.Count - 1 do
  begin
    if I > 0 then
      S := S + #10;
    S := S + FLines[I];
  end;
  FData := S;
  FLinesDirty := False;
  FRowsValid := False;
  FReveal := True;
  Validate;
  Change;
end;

procedure TMemo.SetWordWrap(Value: Boolean);
begin
  if Value = FWordWrap then Exit;
  FWordWrap := Value;
  FRowsValid := False;
  FScrollX := 0;
  FReveal := True;
end;

procedure TMemo.SetScrollBars(Value: Boolean);
begin
  if Value = FScrollBars then Exit;
  FScrollBars := Value;
  FRowsValid := False;
  FReveal := True;
end;

function TMemo.GetSelStart: Integer;
begin
  Validate;
  if FCaret < FAnchor then
    Result := FCaret
  else
    Result := FAnchor;
end;

function TMemo.GetSelLength: Integer;
begin
  Validate;
  Result := Abs(FCaret - FAnchor);
end;

function TMemo.GetSelText: string;
begin
  Result := Copy(FData, SelStart + 1, SelLength);
end;

procedure TMemo.Validate;
begin
  if FCaret > Length(FData) then
    FCaret := Length(FData);
  if FAnchor > Length(FData) then
    FAnchor := Length(FData);
  if FCaret < 0 then
    FCaret := 0;
  if FAnchor < 0 then
    FAnchor := 0;
end;

function TMemo.Measure(Start, Stop: Integer): Float;
begin
  if Stop <= Start then
    Exit(0);
  Result := Computed.Theme.CalcTextWidth(Self, Copy(FData, Start + 1, Stop - Start));
end;

{ A scroll bar is only visible when the text does not fit, which is worked
  out when the rows are built }

function TMemo.BarVisible(Bar: TMemoBar): Boolean;
begin
  if Bar = barVert then
    Result := FVertBar
  else
    Result := FHorzBar;
end;

function TMemo.TextArea: TRectF;
begin
  Result := NewRectF(EditPadding, 4, Width - EditPadding * 2, Height - 8);
  if BarVisible(barVert) then
    Result.Width := Result.Width - ScrollBarSize;
  if BarVisible(barHorz) then
    Result.Height := Result.Height - ScrollBarSize;
  if Result.Width < 1 then
    Result.Width := 1;
  if Result.Height < 1 then
    Result.Height := 1;
end;

function TMemo.BarRect(Bar: TMemoBar): TRectF;
begin
  if Bar = barVert then
  begin
    Result := NewRectF(Width - ScrollBarSize - 2, 2, ScrollBarSize, Height - 4);
    if BarVisible(barHorz) then
      Result.Height := Result.Height - ScrollBarSize;
  end
  else
  begin
    Result := NewRectF(2, Height - ScrollBarSize - 2, Width - 4, ScrollBarSize);
    if BarVisible(barVert) then
      Result.Width := Result.Width - ScrollBarSize;
  end;
end;

function TMemo.ThumbRect(Bar: TMemoBar): TRectF;
var
  Area: TRectF;
  Len, View, Content, Scroll, Thumb: Float;
begin
  Result := BarRect(Bar);
  Area := TextArea;
  if Bar = barVert then
  begin
    Len := Result.Height;
    View := Area.Height;
    Content := RowCount * FRowHeight;
    Scroll := FScrollY;
  end
  else
  begin
    Len := Result.Width;
    View := Area.Width;
    Content := FContentWidth + 2;
    Scroll := FScrollX;
  end;
  { The thumb fills the bar when there is nothing to scroll }
  if (Content <= View) or (Content <= 0) then
    Exit;
  Thumb := Len * View / Content;
  if Thumb < 20 then
    Thumb := 20;
  if Thumb > Len then
    Thumb := Len;
  if Bar = barVert then
  begin
    Result.Y := Result.Y + (Len - Thumb) * Scroll / (Content - View);
    Result.Height := Thumb;
  end
  else
  begin
    Result.X := Result.X + (Len - Thumb) * Scroll / (Content - View);
    Result.Width := Thumb;
  end;
end;

{ LayoutRows splits the text into the rows which are drawn. Without word wrap
  every line is one row. With it a line is broken after the last word which
  fits, or inside a word when the word alone is too wide. }

procedure TMemo.LayoutRows;
var
  Count: Integer;

  procedure Add(Start, Stop: Integer; Hard: Boolean);
  begin
    if Count = Length(FRows) then
      SetLength(FRows, Count * 2 + 16);
    FRows[Count].Start := Start;
    FRows[Count].Length := Stop - Start;
    FRows[Count].Hard := Hard;
    FRows[Count].Width := Measure(Start, Stop);
    if FRows[Count].Width > FContentWidth then
      FContentWidth := FRows[Count].Width;
    Inc(Count);
  end;

var
  Avail: Float;
  S, E, P, Q, N, Fit, L: Integer;
begin
  FRowHeight := Round(FRowsTheme.CalcTextHeight) + 2;
  if FRowHeight < 8 then
    FRowHeight := 8;
  FContentWidth := 0;
  Avail := TextArea.Width;
  Count := 0;
  L := Length(FData);
  S := 0;
  while True do
  begin
    E := S;
    while (E < L) and (FData[E + 1] <> #10) do
      Inc(E);
    if (not FWordWrap) or (E = S) then
      Add(S, E, True)
    else
    begin
      P := S;
      while True do
      begin
        if Measure(P, E) <= Avail then
        begin
          Add(P, E, True);
          Break;
        end;
        { Find the last break after a word which fits. Spaces after the word
          stay on the row and are allowed to hang past the edge. }
        Fit := P;
        Q := P;
        while Q < E do
        begin
          while (Q < E) and (FData[Q + 1] <> ' ') do
            Inc(Q);
          N := Q;
          while (Q < E) and (FData[Q + 1] = ' ') do
            Inc(Q);
          if Measure(P, N) > Avail then
            Break;
          Fit := Q;
        end;
        if Fit = P then
        begin
          { A single word is too wide, so break it between characters }
          Fit := NextChar(FData, P);
          while (Fit < E) and (Measure(P, NextChar(FData, Fit)) <= Avail) do
            Fit := NextChar(FData, Fit);
        end;
        if Fit >= E then
        begin
          Add(P, E, True);
          Break;
        end;
        Add(P, Fit, False);
        P := Fit;
      end;
    end;
    if E >= L then
      Break;
    S := E + 1;
  end;
  SetLength(FRows, Count);
end;

{ BuildRows lays out the rows and decides which scroll bars are needed. A
  scroll bar takes space from the text, which can change how it wraps or
  make the other scroll bar needed, so the rows are laid out again after a
  scroll bar is added. }

procedure TMemo.BuildRows;
var
  I: Integer;
begin
  FRowsTheme := Computed.Theme;
  FRowsWidth := Width;
  FRowsHeight := Height;
  FRowsValid := True;
  FVertBar := False;
  FHorzBar := False;
  LayoutRows;
  if not FScrollBars then
    Exit;
  if FWordWrap then
  begin
    if RowCount * FRowHeight > TextArea.Height then
    begin
      FVertBar := True;
      LayoutRows;
    end;
  end
  else
    { Without wrapping the rows do not change, so only the bars are decided.
      Each bar can make the other one needed, so check them twice. }
    for I := 1 to 2 do
    begin
      if FContentWidth + 2 > TextArea.Width then
        FHorzBar := True;
      if RowCount * FRowHeight > TextArea.Height then
        FVertBar := True;
    end;
end;

{ The rows depend on the text, the size of the memo, and the font of the theme }

procedure TMemo.EnsureRows;
begin
  if FRowsValid and (FRowsWidth = Width) and (FRowsHeight = Height) and
    (FRowsTheme = Computed.Theme) then
    Exit;
  BuildRows;
end;

function TMemo.RowCount: Integer;
begin
  Result := Length(FRows);
end;

function TMemo.RowText(Row: Integer): string;
begin
  Result := Copy(FData, FRows[Row].Start + 1, FRows[Row].Length);
end;

function TMemo.RowOf(Position: Integer): Integer;
var
  I: Integer;
begin
  EnsureRows;
  for I := High(FRows) downto 0 do
    if FRows[I].Start <= Position then
      Exit(I);
  Result := 0;
end;

function TMemo.PositionInRow(Row: Integer; X: Float): Integer;
var
  Start, Stop, I, N: Integer;
  Left, Right: Float;
begin
  Start := FRows[Row].Start;
  Stop := Start + FRows[Row].Length;
  Result := Stop;
  if X <= 0 then
    Result := Start
  else
  begin
    I := Start;
    Left := 0;
    while I < Stop do
    begin
      N := NextChar(FData, I);
      Right := Measure(Start, N);
      if X < Right then
      begin
        if X - Left < Right - X then
          Result := I
        else
          Result := N;
        Break;
      end;
      I := N;
      Left := Right;
    end;
  end;
  { The end of a wrapped row is the start of the next row, so stay before it }
  if (not FRows[Row].Hard) and (Result = Stop) and (Stop > Start) then
    Result := PrevChar(FData, Result);
end;

function TMemo.PositionAt(X, Y: Float): Integer;
var
  Row: Integer;
begin
  EnsureRows;
  Row := Trunc((Y + FScrollY) / FRowHeight);
  if (Y + FScrollY < 0) or (Row < 0) then
    Row := 0;
  if Row > High(FRows) then
    Row := High(FRows);
  Result := PositionInRow(Row, X + FScrollX);
end;

function TMemo.CaretPoint: TPointF;
var
  Row: Integer;
begin
  Validate;
  Row := RowOf(FCaret);
  Result.X := Measure(FRows[Row].Start, FCaret);
  Result.Y := Row * FRowHeight;
end;

function TMemo.CaretVisible: Boolean;
var
  T: Double;
begin
  Result := (wsSelected in State) and Computed.Enabled and (Main <> nil);
  if Result then
  begin
    T := (Main.Time - FBlink) / BlinkPeriod;
    Result := T - Int(T) < 0.5;
  end;
end;

procedure TMemo.ClampScroll;
var
  Area: TRectF;
  Max: Float;
begin
  Area := TextArea;
  Max := RowCount * FRowHeight - Area.Height;
  if FScrollY > Max then
    FScrollY := Max;
  if FScrollY < 0 then
    FScrollY := 0;
  if FWordWrap then
    FScrollX := 0
  else
  begin
    Max := FContentWidth + 2 - Area.Width;
    if FScrollX > Max then
      FScrollX := Max;
    if FScrollX < 0 then
      FScrollX := 0;
  end;
end;

{ Reveal scrolls the least amount needed to bring the caret into view }

procedure TMemo.Reveal;
var
  Area: TRectF;
  P: TPointF;
begin
  FReveal := False;
  Area := TextArea;
  P := CaretPoint;
  if P.Y < FScrollY then
    FScrollY := P.Y;
  if P.Y + FRowHeight > FScrollY + Area.Height then
    FScrollY := P.Y + FRowHeight - Area.Height;
  if not FWordWrap then
  begin
    if P.X < FScrollX then
      FScrollX := P.X;
    if P.X + 2 > FScrollX + Area.Width then
      FScrollX := P.X + 2 - Area.Width;
  end;
end;

procedure TMemo.Prepare;
begin
  EnsureRows;
  if FReveal then
    Reveal;
  ClampScroll;
end;

function TMemo.RowSelection(Row: Integer; out Left, Right: Float): Boolean;
var
  S0, S1, Start, Stop, A, B: Integer;
begin
  Left := 0;
  Right := 0;
  Result := False;
  if SelLength = 0 then
    Exit;
  S0 := SelStart;
  S1 := S0 + SelLength;
  Start := FRows[Row].Start;
  Stop := Start + FRows[Row].Length;
  if (S1 <= Start) or (S0 > Stop) then
    Exit;
  A := S0;
  if A < Start then
    A := Start;
  B := S1;
  if B > Stop then
    B := Stop;
  Left := Measure(Start, A);
  Right := Measure(Start, B);
  { Show a little extra when the selection continues past the end of the row }
  if S1 > Stop then
    Right := Right + 6;
  Result := Right > Left;
end;

procedure TMemo.MoveCaret(Position: Integer; Extend: Boolean; KeepX: Boolean = False);
begin
  FCaret := Position;
  if not Extend then
    FAnchor := Position;
  Validate;
  { Moving sideways forgets the column that up and down try to keep }
  if not KeepX then
    FDesiredX := -1;
  FReveal := True;
  if Main <> nil then
    FBlink := Main.Time;
end;

procedure TMemo.MoveRows(Delta: Integer; Extend: Boolean);
var
  Row, Target: Integer;
begin
  Validate;
  Row := RowOf(FCaret);
  if FDesiredX < 0 then
    FDesiredX := Measure(FRows[Row].Start, FCaret);
  Target := Row + Delta;
  if Target < 0 then
  begin
    { Moving up from the first row goes to the start of the text }
    if Row = 0 then
    begin
      MoveCaret(0, Extend, True);
      Exit;
    end;
    Target := 0;
  end;
  if Target > High(FRows) then
  begin
    { Moving down from the last row goes to the end of the text }
    if Row = High(FRows) then
    begin
      MoveCaret(Length(FData), Extend, True);
      Exit;
    end;
    Target := High(FRows);
  end;
  MoveCaret(PositionInRow(Target, FDesiredX), Extend, True);
end;

procedure TMemo.ReplaceSelection(const S: string);
var
  Start: Integer;
begin
  Start := SelStart;
  System.Delete(FData, Start + 1, SelLength);
  System.Insert(S, FData, Start + 1);
  FLinesDirty := True;
  FRowsValid := False;
  MoveCaret(Start + Length(S), False);
  Change;
end;

procedure TMemo.SelectAll;
begin
  FAnchor := 0;
  MoveCaret(Length(FData), True);
end;

procedure TMemo.CopyToClipboard;
begin
  if SelLength = 0 then
    Exit;
  if SceneHost <> nil then
    SceneHost.Clipboard := SelText
  else
    LocalClipboard := SelText;
end;

procedure TMemo.CutToClipboard;
begin
  if SelLength = 0 then
    Exit;
  CopyToClipboard;
  ReplaceSelection('');
end;

procedure TMemo.PasteFromClipboard;
var
  S: string;
begin
  if SceneHost <> nil then
    S := SceneHost.Clipboard
  else
    S := LocalClipboard;
  S := MultiLine(S);
  if (S <> '') or (SelLength > 0) then
    ReplaceSelection(S);
end;

procedure TMemo.DoKeyDown(var Args: TSceneKeyArgs);
var
  Ctrl, Extend: Boolean;
  C: TContainerWidget;
  Row, Page, P: Integer;
begin
  if Assigned(OnKeyDown) then
    OnKeyDown(Self, Args);
  if Args.Handled then
    Exit;
  Ctrl := skCtrl in Args.Shift;
  Extend := skShift in Args.Shift;
  Args.Handled := True;
  Validate;
  EnsureRows;
  case Args.Key of
    VK_LEFT:
      if Ctrl then
        MoveCaret(PrevWord(FData, FCaret), Extend)
      else if (SelLength > 0) and not Extend then
        MoveCaret(SelStart, False)
      else
        MoveCaret(PrevChar(FData, FCaret), Extend);
    VK_RIGHT:
      if Ctrl then
        MoveCaret(NextWord(FData, FCaret), Extend)
      else if (SelLength > 0) and not Extend then
        MoveCaret(SelStart + SelLength, False)
      else
        MoveCaret(NextChar(FData, FCaret), Extend);
    VK_UP: MoveRows(-1, Extend);
    VK_DOWN: MoveRows(1, Extend);
    VK_PRIOR, VK_NEXT:
      begin
        Page := Trunc(TextArea.Height / FRowHeight) - 1;
        if Page < 1 then
          Page := 1;
        if Args.Key = VK_PRIOR then
          Page := -Page;
        { Scroll by the same amount so the caret stays in place on screen }
        FScrollY := FScrollY + Page * FRowHeight;
        MoveRows(Page, Extend);
      end;
    VK_HOME:
      if Ctrl then
        MoveCaret(0, Extend)
      else
        MoveCaret(FRows[RowOf(FCaret)].Start, Extend);
    VK_END:
      if Ctrl then
        MoveCaret(Length(FData), Extend)
      else
      begin
        Row := RowOf(FCaret);
        P := FRows[Row].Start + FRows[Row].Length;
        if (not FRows[Row].Hard) and (FRows[Row].Length > 0) then
          P := PrevChar(FData, P);
        MoveCaret(P, Extend);
      end;
    VK_RETURN: ReplaceSelection(#10);
    VK_BACK:
      begin
        if SelLength = 0 then
          if Ctrl then
            FAnchor := PrevWord(FData, FCaret)
          else
            FAnchor := PrevChar(FData, FCaret);
        if SelLength > 0 then
          ReplaceSelection('');
      end;
    VK_DELETE:
      if Extend and not Ctrl then
        CutToClipboard
      else
      begin
        if SelLength = 0 then
          if Ctrl then
            FAnchor := NextWord(FData, FCaret)
          else
            FAnchor := NextChar(FData, FCaret);
        if SelLength > 0 then
          ReplaceSelection('');
      end;
    VK_INSERT:
      if Ctrl then
        CopyToClipboard
      else if Extend then
        PasteFromClipboard;
    VK_A:
      if Ctrl then
        SelectAll
      else
        Args.Handled := False;
    VK_C:
      if Ctrl then
        CopyToClipboard
      else
        Args.Handled := False;
    VK_X:
      if Ctrl then
        CutToClipboard
      else
        Args.Handled := False;
    VK_V:
      if Ctrl then
        PasteFromClipboard
      else
        Args.Handled := False;
    VK_TAB:
      begin
        { Tab moves focus between widgets like other widgets do }
        C := Container;
        if C <> nil then
          if Extend then
            C.SelectNext(-1)
          else
            C.SelectNext(1);
      end;
  else
    Args.Handled := False;
  end;
end;

procedure TMemo.DoKeyUp(var Args: TSceneKeyArgs);
begin
  { Return and space are typed into the text, so neither clicks the memo }
  if Assigned(OnKeyUp) then
    OnKeyUp(Self, Args);
end;

procedure TMemo.DoTextInput(var Args: TSceneTextArgs);
begin
  inherited DoTextInput(Args);
  if Args.Handled or (Args.Text = '') then
    Exit;
  ReplaceSelection(MultiLine(Args.Text));
  Args.Handled := True;
end;

procedure TMemo.DragThumb(Bar: TMemoBar; Position: Float);
var
  Track, Thumb, Area: TRectF;
  Len, Size, View, Content, Value: Float;
begin
  Track := BarRect(Bar);
  Thumb := ThumbRect(Bar);
  Area := TextArea;
  if Bar = barVert then
  begin
    Len := Track.Height;
    Size := Thumb.Height;
    View := Area.Height;
    Content := RowCount * FRowHeight;
    Value := Position - FDragOffset - Track.Y;
  end
  else
  begin
    Len := Track.Width;
    Size := Thumb.Width;
    View := Area.Width;
    Content := FContentWidth + 2;
    Value := Position - FDragOffset - Track.X;
  end;
  if (Content <= View) or (Len <= Size) then
    Exit;
  Value := Value / (Len - Size) * (Content - View);
  if Bar = barVert then
    FScrollY := Value
  else
    FScrollX := Value;
  ClampScroll;
end;

procedure TMemo.DoMouseDown(var Args: TSceneMouseArgs);
var
  Area, Thumb: TRectF;
  Time: Double;
  A, B: Integer;
begin
  inherited DoMouseDown(Args);
  if Args.Button <> buttonLeft then
    Exit;
  EnsureRows;
  Area := TextArea;
  if BarVisible(barVert) and BarRect(barVert).Contains(Args.X, Args.Y) then
  begin
    Thumb := ThumbRect(barVert);
    if Thumb.Contains(Args.X, Args.Y) then
    begin
      FMouseMode := mmVertThumb;
      FDragOffset := Args.Y - Thumb.Y;
    end
    else
    begin
      { Clicking beside the thumb scrolls by a page }
      FMouseMode := mmTrack;
      if Args.Y < Thumb.Y then
        FScrollY := FScrollY - Area.Height
      else
        FScrollY := FScrollY + Area.Height;
      ClampScroll;
    end;
    Exit;
  end;
  if BarVisible(barHorz) and BarRect(barHorz).Contains(Args.X, Args.Y) then
  begin
    Thumb := ThumbRect(barHorz);
    if Thumb.Contains(Args.X, Args.Y) then
    begin
      FMouseMode := mmHorzThumb;
      FDragOffset := Args.X - Thumb.X;
    end
    else
    begin
      FMouseMode := mmTrack;
      if Args.X < Thumb.X then
        FScrollX := FScrollX - Area.Width
      else
        FScrollX := FScrollX + Area.Width;
      ClampScroll;
    end;
    Exit;
  end;
  Time := 0;
  if Main <> nil then
    Time := Main.Time;
  if FClicked and (Time - FClickTime < DoubleClickTime) and
    (Abs(Args.X - FClickX) < DoubleClickDistance) and
    (Abs(Args.Y - FClickY) < DoubleClickDistance) and not (skShift in Args.Shift) then
  begin
    { A double click selects the word under the mouse, and it is kept until
      the next click }
    WordRange(FData, PositionAt(Args.X - Area.X, Args.Y - Area.Y), A, B);
    FAnchor := A;
    MoveCaret(B, True);
    FMouseMode := mmTrack;
  end
  else
  begin
    FMouseMode := mmSelect;
    MoveCaret(PositionAt(Args.X - Area.X, Args.Y - Area.Y), skShift in Args.Shift);
    FClickX := Args.X;
    FClickY := Args.Y;
  end;
  FClicked := True;
  FClickTime := Time;
end;

procedure TMemo.DoMouseMove(var Args: TSceneMouseArgs);
var
  Area: TRectF;
begin
  inherited DoMouseMove(Args);
  if not (wsPressed in State) then
    Exit;
  case FMouseMode of
    mmSelect:
      begin
        Area := TextArea;
        MoveCaret(PositionAt(Args.X - Area.X, Args.Y - Area.Y), True);
      end;
    mmVertThumb: DragThumb(barVert, Args.Y);
    mmHorzThumb: DragThumb(barHorz, Args.X);
  end;
end;

procedure TMemo.DoMouseUp(var Args: TSceneMouseArgs);
begin
  inherited DoMouseUp(Args);
  FMouseMode := mmNone;
end;

procedure TMemo.DoMouseWheel(var Args: TSceneWheelArgs);
begin
  { The wheel only scrolls a memo which shows scroll bars }
  if not FScrollBars then
    Exit;
  EnsureRows;
  FScrollY := FScrollY - Args.Delta * FRowHeight * 3;
  ClampScroll;
  Args.Handled := True;
end;

{ TButton }

procedure TButton.DoClick;
begin
  if FCanToggle then
    if FGroup > 0 then
      Down := True
    else
      Down := not Down;
  inherited DoClick;
end;

procedure TButton.SetDown(Value: Boolean);
var
  W: TWidget;
  B: TGlyphButton absolute W;
begin
  if Value = FDown then Exit;
  FDown := Value;
  if FDown then
  begin
    AddState(wsToggled);
    if FGroup > 0 then
      for W in Parent do
        if W = Self then
          Continue
        else if (W is TButton) and B.CanToggle and (B.Group = Group) then
          B.Down := False;
  end
  else
    RemoveState(wsToggled);
  Change;
end;

{ TPushButton }

function TPushButton.CanSelect: Boolean;
begin
  Result := Computed.Enabled and Computed.Visible;
end;

{ TCheckBox }

function TCheckBox.CanSelect: Boolean;
begin
  Result := Computed.Enabled and Computed.Visible;
end;

procedure TCheckBox.DoClick;
begin
  Checked := not Checked;
  inherited DoClick;
end;

procedure TCheckBox.SetChecked(Value: Boolean);
begin
  if Value = FChecked then Exit;
  FChecked := Value;
  if FChecked then
    AddState(wsToggled)
  else
    RemoveState(wsToggled);
  Change;
end;

{ TLabel }

procedure TLabel.SetMaxWidth(Value: Float);
begin
  if FMaxWidth = Value then Exit;
  FMaxWidth := Value;
  Resize;
end;

procedure TLabel.SetAssociateText(Value: string);
begin
  FAssociateText := Value;
  Text := FAssociateText;
end;

{ TSlider }

constructor TSlider.Create(Parent: TWidget; const Name: string);
begin
  inherited Create(Parent, Name);
  FMin := 0;
  FMax := 100;
  FStep := 1;
end;

procedure TSlider.Track(X: Float);
var
  S: TSizeF;
  H, P: Float;
begin
  if Width < 1 then
    Exit;
  S := Main.Theme.CalcSize(Self, tpThumb);
  H := S.X / 2;
  if X < H then
    Position := FMin
  else if X > Width - H then
    Position := FMax
  else
  begin
    X := X - H;
    P := X / (Width - S.X);
    P := FMin + P * (FMax - FMin);
    Position := P;
  end;
end;

procedure TSlider.Change;
begin
  if FAssociate <> nil then
  	if FAssociate.AssociateText <> '' then
    	FAssociate.Text := Format(FAssociate.AssociateText, [Position]);
  inherited Change;
end;

procedure TSlider.DoMouseDown(var Args: TSceneMouseArgs);
begin
  inherited DoMouseDown(Args);
  if wsPressed in State then
    Track(Args.X);
end;

procedure TSlider.DoMouseMove(var Args: TSceneMouseArgs);
begin
  inherited DoMouseMove(Args);
  if wsPressed in State then
    Track(Args.X);
end;

function TSlider.GetGripRect: TRectF;
var
  S: TSizeF;
  P: Float;
begin
  S := Main.Theme.CalcSize(Self, tpThumb);
  P := (Position - FMin) / (FMax - FMin);
  P := Round(P * (Width - S.X));
  Result.X := P;
  Result.Y := Height / 2 - S.Y / 2;
  Result.Width := S.X;
  Result.Height := S.Y;
end;

procedure TSlider.SetAssociate(Value: TLabel);
begin
  FAssociate := Value;
  if FAssociate <> nil then
  	if FAssociate.AssociateText <> '' then
    	FAssociate.Text := Format(FAssociate.AssociateText, [Position]);
end;

procedure TSlider.SetMin(Value: Float);
begin
  FMin := Value;
  SetPosition(FPosition);
end;

procedure TSlider.SetMax(Value: Float);
begin
  FMax := Value;
  SetPosition(FPosition);
end;

procedure TSlider.SetPosition(Value: Float);
var
  V: Double;
begin
  if Value <= FMin then
    Value := FMin
  else if Value >= FMax then
    Value := FMax
  else if FStep > 0 then
  begin
    V := Divide(Value, FStep);
    if Abs(Value - V) > 0.01 then
      Value := V;
  end;
  if Value = FPosition then
    Exit;
  FPosition := Value;
  Change;
end;

procedure TSlider.SetStep(Value: Float);
begin
  if Value < 0 then
    Exit;
  FStep := Value;
end;

{ TSpinBox }

constructor TSpinBox.Create(Parent: TWidget; const Name: string = '');
begin
  inherited Create(Parent, Name);
  FItemIndex := -1;
end;

destructor TSpinBox.Destroy;
begin
  CloseUp;
  inherited Destroy;
end;

{ The list of a spinDropScroll spin box }

procedure TSpinBox.DropDown;
var
  M: TMainWidget;
begin
  if FDropped or (FKind <> spinDropScroll) or (FItems.Length < 1) then
    Exit;
  M := Main;
  if M = nil then
    Exit;
  if (M.FDropBox <> nil) and (M.FDropBox <> Self) then
    M.FDropBox.CloseUp;
  M.FDropBox := Self;
  FDropped := True;
  FDropThumb := False;
  FDropBar := False;
  DropClamp;
  DropScrollTo(FItemIndex);
end;

procedure TSpinBox.CloseUp;
var
  M: TMainWidget;
begin
  FDropped := False;
  FDropThumb := False;
  FDropBar := False;
  M := Main;
  if (M <> nil) and (M.FDropBox = Self) then
    M.FDropBox := nil;
end;

function TSpinBox.DropItemHeight: Float;
var
  T: TTheme;
begin
  T := Computed.Theme;
  if T = nil then
    Result := 20
  else
    Result := Round(T.CalcTextHeight) + 6;
end;

function TSpinBox.DropRect: TRectF;
var
  Count: Integer;
begin
  if (not FDropped) or (FItems.Length < 1) then
    Exit(Default(TRectF));
  Count := FItems.Length;
  if Count > DropScrollItems then
    Count := DropScrollItems;
  Result := NewRectF(0, Height + 2, Width, Count * DropItemHeight + 2);
end;

function TSpinBox.DropBarVisible: Boolean;
begin
  Result := FDropped and (FItems.Length > DropScrollItems);
end;

function TSpinBox.DropArea: TRectF;
begin
  Result := DropRect;
  if Result.Empty then
    Exit;
  Result.Inflate(-1, -1);
  if DropBarVisible then
    Result.Width := Result.Width - ScrollBarSize;
  if Result.Width < 1 then
    Result.Width := 1;
end;

function TSpinBox.DropItemRect(Index: Integer): TRectF;
var
  Area: TRectF;
  H: Float;
begin
  Area := DropArea;
  H := DropItemHeight;
  Result := NewRectF(Area.X, Area.Y + Index * H - FDropScroll, Area.Width, H);
end;

function TSpinBox.DropItemFromPoint(X, Y: Float): Integer;
var
  Area: TRectF;
begin
  Result := -1;
  if not FDropped then
    Exit;
  Area := DropArea;
  if not Area.Contains(X, Y) then
    Exit;
  Result := Trunc((Y - Area.Y + FDropScroll) / DropItemHeight);
  if (Result < 0) or (Result > FItems.Length - 1) then
    Result := -1;
end;

function TSpinBox.DropBarRect: TRectF;
var
  R: TRectF;
begin
  R := DropRect;
  Result := NewRectF(R.X + R.Width - ScrollBarSize - 1, R.Y + 1, ScrollBarSize,
    R.Height - 2);
end;

function TSpinBox.DropThumbRect: TRectF;
var
  View, Content, Thumb: Float;
begin
  Result := DropBarRect;
  View := DropArea.Height;
  Content := FItems.Length * DropItemHeight;
  if Content <= View then
    Exit;
  Thumb := Result.Height * View / Content;
  if Thumb < 20 then
    Thumb := 20;
  if Thumb > Result.Height then
    Thumb := Result.Height;
  Result.Y := Result.Y + (Result.Height - Thumb) * FDropScroll / (Content - View);
  Result.Height := Thumb;
end;

procedure TSpinBox.DropClamp;
var
  Count: Integer;
  Max: Float;
begin
  Count := FItems.Length;
  if Count > DropScrollItems then
    Count := DropScrollItems;
  Max := (FItems.Length - Count) * DropItemHeight;
  if FDropScroll > Max then
    FDropScroll := Max;
  if FDropScroll < 0 then
    FDropScroll := 0;
end;

procedure TSpinBox.DropScrollTo(Index: Integer);
var
  Area, R: TRectF;
begin
  if (not FDropped) or (Index < 0) or (Index > FItems.Length - 1) then
    Exit;
  Area := DropArea;
  R := DropItemRect(Index);
  if R.Top < Area.Top then
    FDropScroll := FDropScroll - (Area.Top - R.Top)
  else if R.Bottom > Area.Bottom then
    FDropScroll := FDropScroll + (R.Bottom - Area.Bottom);
  DropClamp;
end;

procedure TSpinBox.DropDragThumb(Y: Float);
var
  Track, Thumb: TRectF;
  View, Content: Float;
begin
  Track := DropBarRect;
  Thumb := DropThumbRect;
  View := DropArea.Height;
  Content := FItems.Length * DropItemHeight;
  if (Content <= View) or (Track.Height <= Thumb.Height) then
    Exit;
  FDropScroll := (Y - FDropOffset - Track.Y) / (Track.Height - Thumb.Height) *
    (Content - View);
  DropClamp;
end;

{ The list is hidden when the spin box loses the input focus, which is seen
  the next time it is painted }

procedure TSpinBox.Paint(Stage: TPaintStage);
begin
  if (Stage = prePaint) and FDropped and not (wsSelected in State) then
    CloseUp;
  inherited Paint(Stage);
end;

procedure TSpinBox.DoMouseWheel(var Args: TSceneWheelArgs);
begin
  if not FDropped then
    Exit;
  FDropScroll := FDropScroll - Args.Delta * DropItemHeight * 3;
  DropClamp;
  Args.Handled := True;
end;

function TSpinBox.ItemRect(Item: Integer): TRectF;
var
  H: Float;
begin
  Result := TRectF.Create(0, 0);
  if (FKind <> spinDropDown) or (not (wsPressed in State)) or (Items.Length < 1) then
  begin
		Result.Bottom := Result.Top;
    Exit;
  end;
  Result := Computed.Bounds;
  Result.Y := Result.Bottom + 2;
  H := Computed.Theme.CalcTextHeight + 4;
  Result.Bottom := Result.Top + Items.Length + H * Items.Length + 2;
  if Item > -1 then
  begin
		Result.Inflate(-1, -1);
    Result.Y := Result.Top + Item * H;
		Result.Bottom := Result.Top + H;
  end;
end;

function TSpinBox.ItemFromPoint(const P: TPointF): Integer;
var
  R: TRectF;
  I: Integer;
begin
	Result := -1;
  if (not (wsPressed in State)) or (Items.Length < 1) then
    Exit;
  R := Computed.Bounds;
	R.Y := R.Bottom + 1;
  R.Bottom := R.Top + Computed.Theme.CalcTextHeight + 4;
  for I := 0 to Items.Length - 1 do
  begin
    if R.Contains(P.X, P.Y) then
    	Exit(I);
    R.Y := R.Top + R.Height + 1;
  end;
end;

procedure TSpinBox.SetItems(Value: StringArray);
var
  I: Integer;
begin
	FItems.Length := 0;
  FItems.Length := Value.Length;
  for I := 0 to FItems.Length - 1 do
  	FItems[I] := Value[I];
  if FItemIndex > FItems.Length - 1 then
		ItemIndex := -1;
  if FItems.Length < 1 then
    CloseUp;
  DropClamp;
end;

procedure TSpinBox.SetItemIndex(Value: Integer);
begin
	if Value > FItems.Length - 1 then
  	Value := FItems.Length - 1;
	if Value <> FItemIndex then
  begin
    FItemIndex := Value;
    if FItemIndex < 0 then
	    Text := ''
		else
    	Text := FItems[FItemIndex];
    Change;
  end;
end;

procedure TSpinBox.Track(X: Float);
var
  I: Integer;
begin
  if FKind <> spinSlide then
  	Exit;
  if Items.Length = 0 then
	begin
    ItemIndex := -1;
    Exit;
	end;
  if Items.Length = 1 then
	begin
    ItemIndex := 0;
    Exit;
	end;
  FX := FX + X;
  I := ItemIndex;
  if FX < -30 then
  begin
    FX := 0;
    Dec(I);
  end
  else if FX > 30 then
  begin
    FX := 0;
    Inc(I);
  end;
  if I > FItems.Length - 1 then
  	I := 0
	else if I < 0 then
  	I := FItems.Length - 1;
  ItemIndex := I;
end;

function TSpinBox.AutoSize: Boolean;
begin
  Result := False;
end;

{ A spin box takes the input focus when clicked or reached with Tab }

function TSpinBox.CanSelect: Boolean;
begin
  Result := Computed.Enabled and Computed.Visible;
end;

{ With the focus on a drop down spin box, Left and Right choose the item
  before or after the current one. With a scrolling list Up and Down do too.
  Other keys move the focus as usual. }

procedure TSpinBox.DoKeyDown(var Args: TSceneKeyArgs);
var
  Step: Boolean;
  I: Integer;
begin
  Step := False;
  if FKind <> spinSlide then
    Step := (Args.Key = VK_LEFT) or (Args.Key = VK_RIGHT);
  if FKind = spinDropScroll then
    Step := Step or (Args.Key = VK_UP) or (Args.Key = VK_DOWN);
  if not Step then
  begin
    inherited DoKeyDown(Args);
    Exit;
  end;
  if Assigned(OnKeyDown) then
    OnKeyDown(Self, Args);
  if Args.Handled then
    Exit;
  Args.Handled := True;
  if FItems.Length = 0 then
    Exit;
  if (Args.Key = VK_LEFT) or (Args.Key = VK_UP) then
    I := FItemIndex - 1
  else
    I := FItemIndex + 1;
  if I < 0 then
    I := 0;
  if I > FItems.Length - 1 then
    I := FItems.Length - 1;
  ItemIndex := I;
  DropScrollTo(FItemIndex);
end;

{ A press on the box of a scrolling spin box shows or hides its list. A
  press on the scroll bar of the list drags the thumb, or scrolls by a page
  if it is beside the thumb. An item is chosen when the mouse is released
  over it, in DoMouseUp. }

procedure TSpinBox.DoMouseDown(var Args: TSceneMouseArgs);
var
  Thumb: TRectF;
begin
  inherited DoMouseDown(Args);
  if wsPressed in State then
    Track(Args.XRel);
  if (FKind <> spinDropScroll) or (Args.Button <> buttonLeft) then
    Exit;
  FDropThumb := False;
  FDropBar := False;
  if (Args.Y >= 0) and (Args.Y < Height) then
  begin
    if FDropped then
      CloseUp
    else
      DropDown;
    { The release which follows this press does not choose an item }
    FDropBar := True;
  end
  else if DropBarVisible and DropBarRect.Contains(Args.X, Args.Y) then
  begin
    FDropBar := True;
    Thumb := DropThumbRect;
    if Thumb.Contains(Args.X, Args.Y) then
    begin
      FDropThumb := True;
      FDropOffset := Args.Y - Thumb.Y;
    end
    else
    begin
      if Args.Y < Thumb.Y then
        FDropScroll := FDropScroll - DropArea.Height
      else
        FDropScroll := FDropScroll + DropArea.Height;
      DropClamp;
    end;
  end;
end;

procedure TSpinBox.DoMouseMove(var Args: TSceneMouseArgs);
begin
  inherited DoMouseMove(Args);
  if wsPressed in State then
    Track(Args.XRel);
  if FDropThumb and (wsPressed in State) then
    DropDragThumb(Args.Y);
end;

procedure TSpinBox.DoMouseUp(var Args: TSceneMouseArgs);
var
  P: TPointF;
  I: Integer;
begin
  inherited DoMouseUp(Args);
  if (Args.Button = buttonLeft) and (Kind = spinDropScroll) then
  begin
    { Releasing over an item chooses it and hides the list, unless the press
      was on the box or the scroll bar }
    if FDropped and not FDropBar then
    begin
      I := DropItemFromPoint(Args.X, Args.Y);
      if I > -1 then
      begin
        CloseUp;
        ItemIndex := I;
      end;
    end;
    FDropThumb := False;
    FDropBar := False;
    Exit;
  end;
  if Args.Button = buttonLeft then
    if Kind = spinDropDown then
    begin
      FState := FState + [wsPressed];
  	  P := Main.MouseFor(Self);
      I := ItemFromPoint(P);
      if I > -1 then
    	  ItemIndex := I;
      FState := FState - [wsPressed];
    end;
end;

{ TListBox }

constructor TListBox.Create(Parent: TWidget; const Name: string = '');
begin
  inherited Create(Parent, Name);
  FItemIndex := -1;
  FHotIndex := -1;
  FBounds.Width := 200;
  FBounds.Height := 200;
end;

function TListBox.AutoSize: Boolean;
begin
  Result := False;
end;

function TListBox.CanSelect: Boolean;
begin
  Result := Computed.Enabled and Computed.Visible;
end;

function TListBox.GetCount: Integer;
begin
  Result := FItems.Length;
end;

procedure TListBox.SetItems(const Value: StringArray);
var
  I: Integer;
begin
  FItems.Length := 0;
  FItems.Length := Value.Length;
  for I := 0 to FItems.Length - 1 do
    FItems[I] := Value[I];
  FHotIndex := -1;
  if FItemIndex > FItems.Length - 1 then
    ItemIndex := -1;
  ClampScroll;
end;

procedure TListBox.SetItemIndex(Value: Integer);
begin
  if Value > FItems.Length - 1 then
    Value := FItems.Length - 1;
  if Value < -1 then
    Value := -1;
  if Value = FItemIndex then
    Exit;
  FItemIndex := Value;
  if FItemIndex > -1 then
    ScrollToItem(FItemIndex);
  Change;
end;

function TListBox.ItemHeight: Float;
var
  T: TTheme;
begin
  T := Computed.Theme;
  if T = nil then
    Result := 20
  else
    Result := Round(T.CalcTextHeight) + 6;
end;

function TListBox.ItemArea: TRectF;
begin
  Result := NewRectF(2, 2, Width - 4, Height - 4);
  if BarVisible then
    Result.Width := Result.Width - ScrollBarSize;
  if Result.Width < 1 then
    Result.Width := 1;
  if Result.Height < 1 then
    Result.Height := 1;
end;

function TListBox.ItemRect(Index: Integer): TRectF;
var
  Area: TRectF;
  H: Float;
begin
  Area := ItemArea;
  H := ItemHeight;
  Result := NewRectF(Area.X, Area.Y + Index * H - FScrollY, Area.Width, H);
end;

function TListBox.ItemFromPoint(X, Y: Float): Integer;
var
  Area: TRectF;
begin
  Area := ItemArea;
  if not Area.Contains(X, Y) then
    Exit(-1);
  Result := Trunc((Y - Area.Y + FScrollY) / ItemHeight);
  if Result > FItems.Length - 1 then
    Result := -1;
end;

function TListBox.BarVisible: Boolean;
begin
  Result := FItems.Length * ItemHeight > Height - 4;
end;

function TListBox.BarRect: TRectF;
begin
  Result := NewRectF(Width - ScrollBarSize - 2, 2, ScrollBarSize, Height - 4);
end;

function TListBox.ThumbRect: TRectF;
var
  View, Content, Thumb: Float;
begin
  Result := BarRect;
  View := ItemArea.Height;
  Content := FItems.Length * ItemHeight;
  { The thumb fills the bar when there is nothing to scroll }
  if Content <= View then
    Exit;
  Thumb := Result.Height * View / Content;
  if Thumb < 20 then
    Thumb := 20;
  if Thumb > Result.Height then
    Thumb := Result.Height;
  Result.Y := Result.Y + (Result.Height - Thumb) * FScrollY / (Content - View);
  Result.Height := Thumb;
end;

procedure TListBox.ClampScroll;
var
  Max: Float;
begin
  Max := FItems.Length * ItemHeight - ItemArea.Height;
  if FScrollY > Max then
    FScrollY := Max;
  if FScrollY < 0 then
    FScrollY := 0;
end;

procedure TListBox.ScrollToItem(Index: Integer);
var
  Area, R: TRectF;
begin
  if (Index < 0) or (Index > FItems.Length - 1) then
    Exit;
  Area := ItemArea;
  R := ItemRect(Index);
  if R.Top < Area.Top then
    FScrollY := FScrollY - (Area.Top - R.Top)
  else if R.Bottom > Area.Bottom then
    FScrollY := FScrollY + (R.Bottom - Area.Bottom);
  ClampScroll;
end;

procedure TListBox.DragThumb(Y: Float);
var
  Track, Thumb: TRectF;
  View, Content: Float;
begin
  Track := BarRect;
  Thumb := ThumbRect;
  View := ItemArea.Height;
  Content := FItems.Length * ItemHeight;
  if (Content <= View) or (Track.Height <= Thumb.Height) then
    Exit;
  FScrollY := (Y - FDragOffset - Track.Y) / (Track.Height - Thumb.Height) *
    (Content - View);
  ClampScroll;
end;

{ DragItems selects the item at the height of the mouse while the mouse is
  dragged from an item. The mouse can be to the side of the items. Nothing is
  done while the mouse is above or below the items, which is left to
  DragScroll. }

procedure TListBox.DragItems(Y: Float);
var
  Area: TRectF;
  I: Integer;
begin
  if FItems.Length = 0 then
    Exit;
  Area := ItemArea;
  if (Y < Area.Top) or (Y >= Area.Bottom) then
    Exit;
  I := Trunc((Y - Area.Y + FScrollY) / ItemHeight);
  if I > FItems.Length - 1 then
    I := FItems.Length - 1;
  ItemIndex := I;
end;

{ DragScroll is called every frame. While the mouse is held above or below
  the items during a drag it selects the next item out of view each time the
  interval passes, and selecting an item scrolls it into view. This scrolls
  the list while the mouse is still. More items are stepped over when the
  mouse is further away. }

procedure TListBox.DragScroll;
var
  Area: TRectF;
  H, Distance: Float;
  Step, I: Integer;
begin
  if (not FDragItems) or (not (wsPressed in State)) or (Main = nil) or (FItems.Length = 0) then
    Exit;
  Area := ItemArea;
  if FDragY < Area.Top then
    Distance := Area.Top - FDragY
  else if FDragY >= Area.Bottom then
    Distance := FDragY - Area.Bottom
  else
    Exit;
  if Main.Time - FDragTime < DragScrollInterval then
    Exit;
  FDragTime := Main.Time;
  Step := 1;
  if Distance > DragScrollDistance then
    Step := 3;
  H := ItemHeight;
  if FDragY < Area.Top then
    I := Trunc(FScrollY / H) - Step
  else
    I := Trunc((FScrollY + Area.Height - 1) / H) + Step;
  if I < 0 then
    I := 0;
  if I > FItems.Length - 1 then
    I := FItems.Length - 1;
  ItemIndex := I;
end;

procedure TListBox.Paint(Stage: TPaintStage);
begin
  if Stage = prePaint then
    DragScroll;
  inherited Paint(Stage);
end;

procedure TListBox.DoKeyDown(var Args: TSceneKeyArgs);
var
  Page, I: Integer;
begin
  case Args.Key of
    VK_UP, VK_DOWN, VK_HOME, VK_END, VK_PRIOR, VK_NEXT: ;
  else
    inherited DoKeyDown(Args);
    Exit;
  end;
  if Assigned(OnKeyDown) then
    OnKeyDown(Self, Args);
  if Args.Handled then
    Exit;
  Args.Handled := True;
  if FItems.Length = 0 then
    Exit;
  Page := Trunc(ItemArea.Height / ItemHeight) - 1;
  if Page < 1 then
    Page := 1;
  I := FItemIndex;
  case Args.Key of
    VK_UP: Dec(I);
    VK_DOWN: Inc(I);
    VK_HOME: I := 0;
    VK_END: I := FItems.Length - 1;
    VK_PRIOR: Dec(I, Page);
    VK_NEXT: Inc(I, Page);
  end;
  if I < 0 then
    I := 0;
  ItemIndex := I;
end;

procedure TListBox.DoMouseDown(var Args: TSceneMouseArgs);
var
  Thumb: TRectF;
  I: Integer;
begin
  inherited DoMouseDown(Args);
  if Args.Button <> buttonLeft then
    Exit;
  if BarVisible and BarRect.Contains(Args.X, Args.Y) then
  begin
    Thumb := ThumbRect;
    if Thumb.Contains(Args.X, Args.Y) then
    begin
      FDragThumb := True;
      FDragOffset := Args.Y - Thumb.Y;
    end
    else
    begin
      { Clicking beside the thumb scrolls by a page }
      if Args.Y < Thumb.Y then
        FScrollY := FScrollY - ItemArea.Height
      else
        FScrollY := FScrollY + ItemArea.Height;
      ClampScroll;
    end;
    Exit;
  end;
  I := ItemFromPoint(Args.X, Args.Y);
  if I > -1 then
    ItemIndex := I;
  { Pressing in the items begins dragging over them, even below the last }
  FDragItems := ItemArea.Contains(Args.X, Args.Y);
  FDragY := Args.Y;
end;

procedure TListBox.DoMouseMove(var Args: TSceneMouseArgs);
begin
  inherited DoMouseMove(Args);
  FHotIndex := ItemFromPoint(Args.X, Args.Y);
  if not (wsPressed in State) then
    Exit;
  if FDragThumb then
    DragThumb(Args.Y)
  else if FDragItems then
  begin
    { The height of the mouse is kept for DragScroll }
    FDragY := Args.Y;
    DragItems(Args.Y);
  end;
end;

procedure TListBox.DoMouseUp(var Args: TSceneMouseArgs);
begin
  inherited DoMouseUp(Args);
  FDragThumb := False;
  FDragItems := False;
end;

procedure TListBox.DoMouseWheel(var Args: TSceneWheelArgs);
begin
  FScrollY := FScrollY - Args.Delta * ItemHeight * 3;
  ClampScroll;
  Args.Handled := True;
end;

{ TScrollGrid }

constructor TScrollGrid.Create(Parent: TWidget; const Name: string = '');
begin
  inherited Create(Parent, Name);
  FColWidth := 80;
  FRowHeight := 24;
  FCol := -1;
  FRow := -1;
  FHotCol := -1;
  FHotRow := -1;
  FHeaderHot := -1;
  FHeaderDown := -1;
  FSizeHot := -1;
  FSizeCol := -1;
  FSortCol := -1;
  FBounds.Width := 300;
  FBounds.Height := 200;
end;

destructor TScrollGrid.Destroy;
begin
  SetSizeCursor(False);
  inherited Destroy;
end;

function TScrollGrid.AutoSize: Boolean;
begin
  Result := False;
end;

function TScrollGrid.CanSelect: Boolean;
begin
  Result := Computed.Enabled and Computed.Visible;
end;

procedure TScrollGrid.SetColCount(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if Value = FColCount then
    Exit;
  FColCount := Value;
  FHotCol := -1;
  FHeaderHot := -1;
  FHeaderDown := -1;
  FSizeHot := -1;
  if Length(FColWidths) > FColCount then
    SetLength(FColWidths, FColCount);
  if Length(FColTitles) > FColCount then
    SetLength(FColTitles, FColCount);
  if Length(FColAligns) > FColCount then
    SetLength(FColAligns, FColCount);
  if FSortCol > FColCount - 1 then
    FSortCol := -1;
  if FCol > FColCount - 1 then
    Select(FColCount - 1, FRow);
  ClampScroll;
end;

procedure TScrollGrid.SetRowCount(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if Value = FRowCount then
    Exit;
  FRowCount := Value;
  FHotRow := -1;
  if FRow > FRowCount - 1 then
    Select(FCol, FRowCount - 1);
  ClampScroll;
end;

procedure TScrollGrid.SetColWidth(Value: Float);
begin
  if Value < 4 then
    Value := 4;
  FColWidth := Value;
  ClampScroll;
end;

procedure TScrollGrid.SetRowHeight(Value: Float);
begin
  if Value < 4 then
    Value := 4;
  FRowHeight := Value;
  ClampScroll;
end;

procedure TScrollGrid.SetCol(Value: Integer);
begin
  Select(Value, FRow);
end;

procedure TScrollGrid.SetRow(Value: Integer);
begin
  Select(FCol, Value);
end;

function TScrollGrid.GetColWidths(Col: Integer): Float;
begin
  if (Col > -1) and (Col < Length(FColWidths)) and (FColWidths[Col] > 0) then
    Result := FColWidths[Col]
  else
    Result := FColWidth;
end;

procedure TScrollGrid.SetColWidths(Col: Integer; Value: Float);
begin
  if (Col < 0) or (Col > FColCount - 1) then
    Exit;
  if Value < 4 then
    Value := 4;
  if Length(FColWidths) < FColCount then
    SetLength(FColWidths, FColCount);
  FColWidths[Col] := Value;
  ClampScroll;
end;

function TScrollGrid.GetColTitles(Col: Integer): string;
begin
  if (Col > -1) and (Col < Length(FColTitles)) then
    Result := FColTitles[Col]
  else
    Result := '';
end;

procedure TScrollGrid.SetColTitles(Col: Integer; const Value: string);
begin
  if (Col < 0) or (Col > FColCount - 1) then
    Exit;
  if Length(FColTitles) < FColCount then
    SetLength(FColTitles, FColCount);
  FColTitles[Col] := Value;
end;

function TScrollGrid.GetColAligns(Col: Integer): TWidgetAlign;
begin
  if (Col > -1) and (Col < Length(FColAligns)) then
    Result := FColAligns[Col]
  else
    Result := alignNear;
end;

procedure TScrollGrid.SetColAligns(Col: Integer; Value: TWidgetAlign);
begin
  if (Col < 0) or (Col > FColCount - 1) then
    Exit;
  if Length(FColAligns) < FColCount then
    SetLength(FColAligns, FColCount);
  FColAligns[Col] := Value;
end;

procedure TScrollGrid.SetHeaderRow(Value: Boolean);
begin
  if Value = FHeaderRow then
    Exit;
  FHeaderRow := Value;
  FHeaderHot := -1;
  FHeaderDown := -1;
  FSizeHot := -1;
  SetSizeCursor(False);
  ClampScroll;
end;

function TScrollGrid.GetHeaderHeight: Float;
begin
  if not FHeaderRow then
    Result := 0
  else if FHeaderHeight > 0 then
    Result := FHeaderHeight
  else
    Result := Round(Computed.Theme.CalcTextHeight) + 8;
end;

procedure TScrollGrid.SetHeaderHeight(Value: Float);
begin
  if Value < 0 then
    Value := 0;
  FHeaderHeight := Value;
  ClampScroll;
end;

{ A header cell is pressed like a button, only while the mouse is over it }

function TScrollGrid.GetHeaderPressed: Integer;
begin
  if (FDrag = gridDragHeader) and (wsPressed in State) and (FHeaderHot = FHeaderDown) then
    Result := FHeaderDown
  else
    Result := -1;
end;

{ The cursor shows that a column can be resized while the mouse is over the
  edge of a header cell. The cursor is only put back if the grid changed it. }

procedure TScrollGrid.SetSizeCursor(Value: Boolean);
begin
  if Value = FSizeCursor then
    Exit;
  FSizeCursor := Value;
  if Value then
    Mouse.Cursor := cursorSizeWE
  else
    Mouse.Cursor := cursorDefault;
end;

function TScrollGrid.ColOffset(Col: Integer): Float;
var
  I: Integer;
begin
  if Length(FColWidths) = 0 then
    Exit(Col * FColWidth);
  Result := 0;
  for I := 0 to Col - 1 do
    Result := Result + GetColWidths(I);
end;

function TScrollGrid.ColFromOffset(X: Float): Integer;
begin
  if Length(FColWidths) = 0 then
    Exit(Trunc(X / FColWidth));
  Result := 0;
  while Result < FColCount do
  begin
    X := X - GetColWidths(Result);
    if X < 0 then
      Exit;
    Inc(Result);
  end;
end;

function TScrollGrid.HeaderRect: TRectF;
begin
  if FHeaderRow then
    Result := NewRectF(2, 2, Width - 4, GetHeaderHeight)
  else
    Result := NewRectF(0, 0, 0, 0);
end;

function TScrollGrid.HeaderCellRect(Col: Integer): TRectF;
begin
  Result := NewRectF(2 + ColOffset(Col) - FScrollX, 2, GetColWidths(Col), GetHeaderHeight);
end;

function TScrollGrid.HeaderFromPoint(X, Y: Float): Integer;
begin
  Result := -1;
  if (not FHeaderRow) or (not HeaderRect.Contains(X, Y)) then
    Exit;
  Result := ColFromOffset(X - 2 + FScrollX);
  if Result > FColCount - 1 then
    Result := -1;
end;

function TScrollGrid.DividerFromPoint(X, Y: Float): Integer;
var
  Edge: Float;
  I: Integer;
begin
  Result := -1;
  if (not FHeaderRow) or (not FColSizing) or (not HeaderRect.Contains(X, Y)) then
    Exit;
  Edge := 2 - FScrollX;
  for I := 0 to FColCount - 1 do
  begin
    Edge := Edge + GetColWidths(I);
    if Abs(X - Edge) <= HeaderGrabSize then
      Exit(I);
    if Edge > X + HeaderGrabSize then
      Exit;
  end;
end;

{ The height of all of the rows, or the width of all of the columns }

function TScrollGrid.ContentSize(Bar: TMemoBar): Float;
begin
  if Bar = barVert then
    Result := FRowCount * FRowHeight
  else
    Result := ColOffset(FColCount);
end;

{ A scroll bar takes space from the cells, which can make the other scroll
  bar needed, so the bars are checked twice }

function TScrollGrid.BarVisible(Bar: TMemoBar): Boolean;
var
  W, H: Float;
  Vert, Horz: Boolean;
  I: Integer;
begin
  Vert := False;
  Horz := False;
  for I := 1 to 2 do
  begin
    W := Width - 4;
    H := Height - 4 - GetHeaderHeight;
    if Vert then
      W := W - ScrollBarSize;
    if Horz then
      H := H - ScrollBarSize;
    if ContentSize(barVert) > H then
      Vert := True;
    if ContentSize(barHorz) > W then
      Horz := True;
  end;
  if Bar = barVert then
    Result := Vert
  else
    Result := Horz;
end;

{ The header takes its height from the top of the cells }

function TScrollGrid.CellArea: TRectF;
var
  H: Float;
begin
  H := GetHeaderHeight;
  Result := NewRectF(2, 2 + H, Width - 4, Height - 4 - H);
  if BarVisible(barVert) then
    Result.Width := Result.Width - ScrollBarSize;
  if BarVisible(barHorz) then
    Result.Height := Result.Height - ScrollBarSize;
  if Result.Width < 1 then
    Result.Width := 1;
  if Result.Height < 1 then
    Result.Height := 1;
end;

function TScrollGrid.CellRect(Col, Row: Integer): TRectF;
var
  Area: TRectF;
begin
  Area := CellArea;
  Result := NewRectF(Area.X + ColOffset(Col) - FScrollX,
    Area.Y + Row * FRowHeight - FScrollY, GetColWidths(Col), FRowHeight);
end;

function TScrollGrid.CellFromPoint(X, Y: Float; out Col, Row: Integer): Boolean;
var
  Area: TRectF;
begin
  Col := -1;
  Row := -1;
  Area := CellArea;
  Result := Area.Contains(X, Y);
  if not Result then
    Exit;
  Col := ColFromOffset(X - Area.X + FScrollX);
  Row := Trunc((Y - Area.Y + FScrollY) / FRowHeight);
  Result := (Col < FColCount) and (Row < FRowCount);
  if not Result then
  begin
    Col := -1;
    Row := -1;
  end;
end;

{ The vertical scroll bar begins below the header }

function TScrollGrid.BarRect(Bar: TMemoBar): TRectF;
var
  H: Float;
begin
  if Bar = barVert then
  begin
    H := GetHeaderHeight;
    Result := NewRectF(Width - ScrollBarSize - 2, 2 + H, ScrollBarSize, Height - 4 - H);
    if BarVisible(barHorz) then
      Result.Height := Result.Height - ScrollBarSize;
  end
  else
  begin
    Result := NewRectF(2, Height - ScrollBarSize - 2, Width - 4, ScrollBarSize);
    if BarVisible(barVert) then
      Result.Width := Result.Width - ScrollBarSize;
  end;
end;

function TScrollGrid.ThumbRect(Bar: TMemoBar): TRectF;
var
  Area: TRectF;
  Len, View, Content, Scroll, Thumb: Float;
begin
  Result := BarRect(Bar);
  Area := CellArea;
  Content := ContentSize(Bar);
  if Bar = barVert then
  begin
    Len := Result.Height;
    View := Area.Height;
    Scroll := FScrollY;
  end
  else
  begin
    Len := Result.Width;
    View := Area.Width;
    Scroll := FScrollX;
  end;
  { The thumb fills the bar when there is nothing to scroll }
  if (Content <= View) or (Content <= 0) then
    Exit;
  Thumb := Len * View / Content;
  if Thumb < 20 then
    Thumb := 20;
  if Thumb > Len then
    Thumb := Len;
  if Bar = barVert then
  begin
    Result.Y := Result.Y + (Len - Thumb) * Scroll / (Content - View);
    Result.Height := Thumb;
  end
  else
  begin
    Result.X := Result.X + (Len - Thumb) * Scroll / (Content - View);
    Result.Width := Thumb;
  end;
end;

procedure TScrollGrid.ClampScroll;
var
  Area: TRectF;
  Max: Float;
begin
  Area := CellArea;
  Max := ContentSize(barHorz) - Area.Width;
  if FScrollX > Max then
    FScrollX := Max;
  if FScrollX < 0 then
    FScrollX := 0;
  Max := ContentSize(barVert) - Area.Height;
  if FScrollY > Max then
    FScrollY := Max;
  if FScrollY < 0 then
    FScrollY := 0;
end;

procedure TScrollGrid.ScrollToCell(Col, Row: Integer);
var
  Area, R: TRectF;
begin
  Area := CellArea;
  if (Col > -1) and (Col < FColCount) then
  begin
    R := CellRect(Col, 0);
    if R.Left < Area.Left then
      FScrollX := FScrollX - (Area.Left - R.Left)
    else if R.Right > Area.Right then
      FScrollX := FScrollX + (R.Right - Area.Right);
  end;
  if (Row > -1) and (Row < FRowCount) then
  begin
    R := CellRect(0, Row);
    if R.Top < Area.Top then
      FScrollY := FScrollY - (Area.Top - R.Top)
    else if R.Bottom > Area.Bottom then
      FScrollY := FScrollY + (R.Bottom - Area.Bottom);
  end;
  ClampScroll;
end;

procedure TScrollGrid.Select(Col, Row: Integer);
begin
  if Col > FColCount - 1 then
    Col := FColCount - 1;
  if Col < -1 then
    Col := -1;
  if Row > FRowCount - 1 then
    Row := FRowCount - 1;
  if Row < -1 then
    Row := -1;
  if (Col = FCol) and (Row = FRow) then
    Exit;
  FCol := Col;
  FRow := Row;
  ScrollToCell(FCol, FRow);
  Change;
end;

function TScrollGrid.Selected(Row, Col: Integer): Boolean;
begin
  Result := (Row = FRow) and (Col = FCol);
end;

procedure TScrollGrid.DrawCell(Surface: ICanvas; Row, Col: Integer; const Rect: TRectF);
begin
  if Assigned(FOnDrawCell) then
    FOnDrawCell(Self, Surface, Row, Col, Rect);
end;

procedure TScrollGrid.DragThumb(Bar: TMemoBar; Position: Float);
var
  Track, Thumb, Area: TRectF;
  Len, Size, View, Content, Value: Float;
begin
  Track := BarRect(Bar);
  Thumb := ThumbRect(Bar);
  Area := CellArea;
  Content := ContentSize(Bar);
  if Bar = barVert then
  begin
    Len := Track.Height;
    Size := Thumb.Height;
    View := Area.Height;
    Value := Position - FDragOffset - Track.Y;
  end
  else
  begin
    Len := Track.Width;
    Size := Thumb.Width;
    View := Area.Width;
    Value := Position - FDragOffset - Track.X;
  end;
  if (Content <= View) or (Len <= Size) then
    Exit;
  Value := Value / (Len - Size) * (Content - View);
  if Bar = barVert then
    FScrollY := Value
  else
    FScrollX := Value;
  ClampScroll;
end;

{ DragCells selects the cell under the mouse while the mouse is dragged from
  a cell. When the mouse is past an edge of the cells the row or column at
  that edge is kept, and scrolling past the edge is left to DragScroll. }

procedure TScrollGrid.DragCells(X, Y: Float);
var
  Area: TRectF;
  C, R: Integer;
begin
  if (FColCount = 0) or (FRowCount = 0) then
    Exit;
  Area := CellArea;
  C := FCol;
  R := FRow;
  if (X >= Area.Left) and (X < Area.Right) then
    C := ColFromOffset(X - Area.X + FScrollX);
  if (Y >= Area.Top) and (Y < Area.Bottom) then
    R := Trunc((Y - Area.Y + FScrollY) / FRowHeight);
  if C < 0 then
    C := 0;
  if R < 0 then
    R := 0;
  Select(C, R);
end;

{ DragScroll is called every frame. While the mouse is held past an edge of
  the cells during a drag it selects the next row or column out of view on
  that side each time the interval passes, and selecting a cell scrolls it
  into view. This scrolls the grid while the mouse is still. More cells are
  stepped over when the mouse is further away. }

procedure TScrollGrid.DragScroll;
var
  Area: TRectF;
  C, R: Integer;

  function Step(Distance: Float): Integer;
  begin
    if Distance > DragScrollDistance then
      Result := 3
    else
      Result := 1;
  end;

begin
  if (FDrag <> gridDragCells) or (not (wsPressed in State)) or (Main = nil) then
    Exit;
  if (FColCount = 0) or (FRowCount = 0) then
    Exit;
  Area := CellArea;
  if (FDragX >= Area.Left) and (FDragX < Area.Right) and (FDragY >= Area.Top) and
    (FDragY < Area.Bottom) then
    Exit;
  if Main.Time - FDragTime < DragScrollInterval then
    Exit;
  FDragTime := Main.Time;
  C := FCol;
  R := FRow;
  if FDragX < Area.Left then
    C := ColFromOffset(FScrollX) - Step(Area.Left - FDragX)
  else if FDragX >= Area.Right then
    C := ColFromOffset(FScrollX + Area.Width - 1) + Step(FDragX - Area.Right);
  if FDragY < Area.Top then
    R := Trunc(FScrollY / FRowHeight) - Step(Area.Top - FDragY)
  else if FDragY >= Area.Bottom then
    R := Trunc((FScrollY + Area.Height - 1) / FRowHeight) + Step(FDragY - Area.Bottom);
  if C < 0 then
    C := 0;
  if R < 0 then
    R := 0;
  Select(C, R);
end;

procedure TScrollGrid.Paint(Stage: TPaintStage);
begin
  if Stage = prePaint then
  begin
    DragScroll;
    { The mouse left the grid from the edge of a header cell }
    if FSizeCursor and (FDrag <> gridDragSize) and (not (wsHot in State)) then
    begin
      FSizeHot := -1;
      SetSizeCursor(False);
    end;
  end;
  inherited Paint(Stage);
end;

procedure TScrollGrid.DoKeyDown(var Args: TSceneKeyArgs);
var
  Page, C, R: Integer;
begin
  case Args.Key of
    VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN, VK_HOME, VK_END, VK_PRIOR, VK_NEXT: ;
  else
    inherited DoKeyDown(Args);
    Exit;
  end;
  if Assigned(OnKeyDown) then
    OnKeyDown(Self, Args);
  if Args.Handled then
    Exit;
  Args.Handled := True;
  if (FColCount = 0) or (FRowCount = 0) then
    Exit;
  Page := Trunc(CellArea.Height / FRowHeight) - 1;
  if Page < 1 then
    Page := 1;
  C := FCol;
  R := FRow;
  case Args.Key of
    VK_LEFT: Dec(C);
    VK_RIGHT: Inc(C);
    VK_UP: Dec(R);
    VK_DOWN: Inc(R);
    VK_HOME: C := 0;
    VK_END: C := FColCount - 1;
    VK_PRIOR: Dec(R, Page);
    VK_NEXT: Inc(R, Page);
  end;
  if C < 0 then
    C := 0;
  if R < 0 then
    R := 0;
  Select(C, R);
end;

procedure TScrollGrid.DoMouseDown(var Args: TSceneMouseArgs);
var
  Area, Thumb: TRectF;
  C, R: Integer;
begin
  inherited DoMouseDown(Args);
  if Args.Button <> buttonLeft then
    Exit;
  FDrag := gridDragNone;
  FHeaderDown := -1;
  { Pressing the edge of a header cell begins resizing its column, and
    pressing anywhere else in a header cell presses it }
  if FHeaderRow and HeaderRect.Contains(Args.X, Args.Y) then
  begin
    C := DividerFromPoint(Args.X, Args.Y);
    if C > -1 then
    begin
      FDrag := gridDragSize;
      FSizeCol := C;
      FDragOffset := Args.X - HeaderCellRect(C).Right;
      FHeaderHot := -1;
    end
    else
    begin
      FHeaderDown := HeaderFromPoint(Args.X, Args.Y);
      FHeaderHot := FHeaderDown;
      if FHeaderDown > -1 then
        FDrag := gridDragHeader;
    end;
    Exit;
  end;
  Area := CellArea;
  if BarVisible(barVert) and BarRect(barVert).Contains(Args.X, Args.Y) then
  begin
    Thumb := ThumbRect(barVert);
    if Thumb.Contains(Args.X, Args.Y) then
    begin
      FDrag := gridDragVert;
      FDragOffset := Args.Y - Thumb.Y;
    end
    else
    begin
      { Clicking beside the thumb scrolls by a page }
      if Args.Y < Thumb.Y then
        FScrollY := FScrollY - Area.Height
      else
        FScrollY := FScrollY + Area.Height;
      ClampScroll;
    end;
    Exit;
  end;
  if BarVisible(barHorz) and BarRect(barHorz).Contains(Args.X, Args.Y) then
  begin
    Thumb := ThumbRect(barHorz);
    if Thumb.Contains(Args.X, Args.Y) then
    begin
      FDrag := gridDragHorz;
      FDragOffset := Args.X - Thumb.X;
    end
    else
    begin
      if Args.X < Thumb.X then
        FScrollX := FScrollX - Area.Width
      else
        FScrollX := FScrollX + Area.Width;
      ClampScroll;
    end;
    Exit;
  end;
  if CellFromPoint(Args.X, Args.Y, C, R) then
    Select(C, R);
  { Pressing in the cells begins dragging over them, even past the last }
  if Area.Contains(Args.X, Args.Y) then
    FDrag := gridDragCells;
  FDragX := Args.X;
  FDragY := Args.Y;
end;

procedure TScrollGrid.DoMouseMove(var Args: TSceneMouseArgs);
var
  W: Float;
begin
  inherited DoMouseMove(Args);
  CellFromPoint(Args.X, Args.Y, FHotCol, FHotRow);
  FHeaderHot := HeaderFromPoint(Args.X, Args.Y);
  if not (wsPressed in State) then
  begin
    { The edge of a header cell is not part of the cell while it can be
      dragged }
    FSizeHot := DividerFromPoint(Args.X, Args.Y);
    if FSizeHot > -1 then
      FHeaderHot := -1;
    SetSizeCursor(FSizeHot > -1);
    Exit;
  end;
  { While a header cell is held down no other header cell is hot }
  if (FDrag <> gridDragHeader) or (FHeaderHot <> FHeaderDown) then
    FHeaderHot := -1;
  case FDrag of
    gridDragVert: DragThumb(barVert, Args.Y);
    gridDragHorz: DragThumb(barHorz, Args.X);
    gridDragSize:
      begin
        { The edge of the column follows the mouse }
        W := Round(Args.X - FDragOffset - HeaderCellRect(FSizeCol).X);
        if W < HeaderMinWidth then
          W := HeaderMinWidth;
        if W <> GetColWidths(FSizeCol) then
        begin
          SetColWidths(FSizeCol, W);
          if Assigned(FOnColResize) then
            FOnColResize(Self, FSizeCol);
        end;
      end;
    gridDragCells:
      begin
        { The place of the mouse is kept for DragScroll }
        FDragX := Args.X;
        FDragY := Args.Y;
        DragCells(Args.X, Args.Y);
      end;
  end;
end;

procedure TScrollGrid.DoMouseUp(var Args: TSceneMouseArgs);
var
  Drag: TScrollGridDrag;
  C: Integer;
begin
  inherited DoMouseUp(Args);
  Drag := FDrag;
  C := FHeaderDown;
  FDrag := gridDragNone;
  FHeaderDown := -1;
  FHeaderHot := HeaderFromPoint(Args.X, Args.Y);
  FSizeHot := DividerFromPoint(Args.X, Args.Y);
  if FSizeHot > -1 then
    FHeaderHot := -1;
  SetSizeCursor(FSizeHot > -1);
  { A header cell is clicked when the mouse is released over the cell it
    was pressed on }
  if (Drag = gridDragHeader) and (C > -1) and (HeaderFromPoint(Args.X, Args.Y) = C) then
    if Assigned(FOnHeaderClick) then
      FOnHeaderClick(Self, C);
end;

procedure TScrollGrid.DoMouseWheel(var Args: TSceneWheelArgs);
begin
  if skShift in Args.Shift then
    FScrollX := FScrollX - Args.Delta * FColWidth
  else
    FScrollY := FScrollY - Args.Delta * FRowHeight * 3;
  ClampScroll;
  Args.Handled := True;
end;

{ TCustomWidget }

procedure TCustomWidget.Paint;
begin
  if Assigned(FOnPaint) then
    FOnPaint(Self);
end;

{ TContainerWidget }

var
  SelectStack: TArrayList<TWidget>;

procedure TContainerWidget.SelectNext(Dir: Integer = 1);
var
  S: TWidget;
  I, J: Integer;

  procedure Add(W: TWidget);
  var
    C: TWidget;
  begin
    for C in W do
    begin
      if C.CanSelect then
      begin
        SelectStack[I] := C;
        if wsSelected in C.State then
          S := C;
        Inc(I);
      end;
      Add(C);
    end;
  end;

begin
  if SelectStack.Length < 1000 then
    SelectStack.Length := 1000;
  S := nil;
  I := 0;
  if Dir < 0 then
    Dir := -1
  else
    Dir := 1;
  Add(Self);
  if I < 2 then
    Exit;
  if S = nil then
  begin
    SelectStack[0].Activate;
    Exit;
  end;
  J := SelectStack.IndexOf(S) + Dir;
  if J < 0 then
    J := I - 1
  else if J = I then
    J := 0;
  SelectStack[J].Activate;
end;

{ THBox }

function THBox.Pack: Boolean;
var
  Child, Last: TWidget;
  S: TSizeF;
  H, X, Y: Float;
  A, B: Float;
begin
  Result := inherited Pack;
  if not Result then
    Exit;
  S := Main.Theme.CalcSize(Self, tpEverything);
  H := S.Y;
  for Child in Self do
  begin
    if Child.Unpacked then
      Continue;
    if not Child.Computed.Visible then
      Continue;
    if Child.FNeedsPack then
      Child.Pack;
  end;
  for Child in Self do
  begin
    if Child.Unpacked then
      Continue;
    if not Child.Computed.Visible then
      Continue;
    Y := Child.Margin * 2 + Child.Height;
    if Y > H then
      H := Y;
  end;
  X := 0;
  A := 0;
  B := 0;
  Last := nil;
  for Child in Self do
  begin
    if Child.Unpacked then
      Continue;
    if not Child.Computed.Visible then
      Continue;
    B := Child.Margin;
    if B > A then
      X := X + B
    else
      X := X + A;
    A := Child.Margin;
    Child.X := X;
    case Child.Align of
      alignNear: Child.Y := Child.Margin;
      alignCenter: Child.Y := (H - Child.Height) / 2;
      alignFar: Child.Y := H - Child.Height - Child.Margin;
    end;
    X := X + Child.Width;
    Last := Child;
  end;
  if Last <> nil then
    X := X + LastChild.Margin;
  if X < S.X then
    X := S.X;
  FBounds.Width := X;
  FBounds.Height := H;
end;

function TVBox.Pack: Boolean;
var
  Child, Last: TWidget;
  S: TSizeF;
  W, X, Y: Float;
  A, B: Float;
begin
  Result := inherited Pack;
  if not Result then
    Exit;
  S := Main.Theme.CalcSize(Self, tpEverything);
  W := S.X;
  X := 0;
  for Child in Self do
  begin
    if Child.Unpacked then
      Continue;
    if not Child.Computed.Visible then
      Continue;
    if Child.FNeedsPack then
      Child.Pack;
  end;
  for Child in Self do
  begin
    if Child.Unpacked then
      Continue;
    if not Child.Computed.Visible then
      Continue;
    X := Child.Margin * 2 + Child.Width;
    if X > W then
      W := X;
  end;
  Y := 0;
  A := 0;
  B := 0;
  Last := nil;
  for Child in Self do
  begin
    if Child.Unpacked then
      Continue;
    if not Child.Computed.Visible then
      Continue;
    B := Child.Margin;
    if B > A then
      Y := Y + B
    else
      Y := Y + A;
    A := Child.Margin;
    Child.Y := Y;
    case Child.Align of
      alignNear: Child.X := Child.Margin;
      alignCenter: Child.X := (W - Child.Width) / 2;
      alignFar: Child.X := W - Child.Width - Child.Margin;
    end;
    Y := Y + Child.Height;
    Last := Child;
  end;
  if Last <> nil then
    Y := Y + Last.Margin;
  if Y < S.Y then
    Y := S.Y;
  FBounds.Width := W;
  FBounds.Height := Y;
end;

{ TTheme }

function TTheme.CalcCloseRect(Window: TWindow): TRectF;
begin
  Result := Default(TRectF);
end;

procedure TTheme.PushMatrix(Matrix: IMatrix);
begin
end;

procedure TTheme.PopMatrix;
begin
end;

{ TWindow }

constructor TWindow.Create(Parent: TWidget; const Name: string = '');
begin
  inherited Create(Parent, Name);
  FCloseButton := True;
end;

function TWindow.CloseRect: TRectF;
begin
  if FCloseButton then
    Result := Computed.Theme.CalcCloseRect(Self)
  else
    Result := Default(TRectF);
end;

function TWindow.CloseHot: Boolean;
var
  B: TRectF;
  P: TPointF;
begin
  Result := False;
  if (not (wsHot in State)) or (Main = nil) then
    Exit;
  B := Computed.Bounds;
  P := Main.MouseFor(Self);
  Result := CloseRect.Contains(P.X - B.X, P.Y - B.Y);
end;

function TWindow.ClosePressed: Boolean;
begin
  Result := FCloseDown and CloseHot;
end;

procedure TWindow.Close;
var
  Event: TNotifyEvent;
begin
  Event := FOnClose;
  if Assigned(Event) then
    Event(Self);
  { Hiding a modal window can free it, so nothing may follow this }
  Hide;
end;

{ The close button closes the window from the click which follows the mouse
  up, because that is the last thing done with the window by the main widget
  and closing a modal window can free it }

procedure TWindow.DoClick;
begin
  inherited DoClick;
  if FCloseClick then
  begin
    FCloseClick := False;
    Close;
  end;
end;

function TWindow.GetBorders: TRectF;
var
  Frame: TSizeF;
begin
  { X and Width are the left and right borders, Y and Height the top and bottom }
  Frame := Main.Theme.CalcSize(Self, tpBorder);
  Result := Default(TRectF);
  Result.X := Frame.X;
  Result.Width := Frame.X;
  Result.Y := Main.Theme.CalcSize(Self, tpCaption).Y;
  Result.Height := Frame.Y;
end;

function TWindow.Pack: Boolean;
var
  Y: Float;
  I: Float;
  W: TWidget;
  A, B: Float;
  Borders: TRectF;
  Side: Float;
begin
  Result := inherited Pack;
  if not Result then
    Exit;
  { Widgets are kept inside the frame the theme draws around the window }
  Borders := GetBorders;
  Side := Borders.X;
  Y := Borders.Y;
  I := Main.Theme.CalcSize(Self, tpIndent).X;
  A := 100;
  B := 100;
  for W in Self do
  begin
    if W.Unpacked then
      Continue;
    if not W.Computed.Visible then
      Continue;
    B := W.Width + W.Margin * 2 + W.Indent * I + Side * 2;
    if B > A then
      A := B;
  end;
  FBounds.Width := A;
  A := 0;
  B := 0;
  for W in Self do
  begin
    if W.Unpacked then
      Continue;
    if not W.Computed.Visible then
      Continue;
    case W.Align of
      alignNear: W.X := Side + W.Margin + I * W.Indent;
      alignCenter: W.X := (Width - W.Width) / 2;
      alignFar: W.X := Width - W.Width - W.Margin - I * W.Indent - Side;
    end;
    B := W.Margin;
    if B > A then
      Y := Y + B
    else
      Y := Y + A;
    A := W.Margin;
    W.Y := Y;
    Y := Y + W.Height;
  end;
  { The height comes from packing, so it stays free to change }
  Height := Y + B + Borders.Height;
  FFixedHeight := False;
end;

procedure TWindow.DoModalResult;
var
  R: TModalResult;
begin
  R := ModalResult;
  FModalResult := modalNone;
  if FShowingModal then
  begin
    Main.UnsetModal(Self);
    if Assigned(FOnModalResult) then
      FOnModalResult(Self, R);
  end;
end;

procedure TWindow.Show;
begin
  Visible := True;
end;

procedure TWindow.Hide;
begin
  if FShowingModal then
    ModalResult := modalCancel
  else
    Visible := False;
end;

procedure TWindow.ShowModal(OnModalResult: TModalResultEvent);
begin
  if FShowingModal then
    Exit;
  FModalResult := modalNone;
  FOnModalResult := OnModalResult;
  Main.SetModal(Self);
end;

const
  { The size of the size grip, and the smallest a widget can be resized to }
  SizeGrip = 16;
  SizeMinimum = 80;

function TWindow.SizeRect: TRectF;
begin
  if FSizeable and (FSizeWidget <> nil) then
    Result := NewRectF(Width - SizeGrip, Height - SizeGrip, SizeGrip, SizeGrip)
  else
    Result := Default(TRectF);
end;

procedure TWindow.DoMouseDown(var Args: TSceneMouseArgs);
begin
  inherited DoMouseDown(Args);
  if Args.Button = buttonLeft then
    if CloseRect.Contains(Args.X, Args.Y) then
      FCloseDown := True
    else if SizeRect.Contains(Args.X, Args.Y) then
    begin
      { The mouse and the size of the widget are kept from when the grip was
        pressed, as the top left of the window stays where it is }
      FSizing := True;
      FSizeX := Args.X;
      FSizeY := Args.Y;
      FSizeWidth := FSizeWidget.Width;
      FSizeHeight := FSizeWidget.Height;
    end
    else if Args.Y < Main.Theme.CalcSize(Self, tpCaption).Y then
    begin
      FDrag := True;
      FDragX := Args.X;
      FDragY := Args.Y;
    end;
end;

procedure TWindow.DoMouseMove(var Args: TSceneMouseArgs);
var
  W, H: Float;
begin
  inherited DoMouseMove(Args);
  if FDrag then
  begin
    FBounds.X := FBounds.X + Args.X - FDragX;
    FBounds.Y := FBounds.Y + Args.Y - FDragY;
  end;
  if FSizing and (FSizeWidget <> nil) then
  begin
    W := FSizeWidth + Args.X - FSizeX;
    if W < SizeMinimum then
      W := SizeMinimum;
    H := FSizeHeight + Args.Y - FSizeY;
    if H < SizeMinimum then
      H := SizeMinimum;
    if (W <> FSizeWidget.Width) or (H <> FSizeWidget.Height) then
    begin
      FSizeWidget.Width := W;
      FSizeWidget.Height := H;
      if Assigned(FOnResize) then
        FOnResize(Self);
    end;
  end;
end;

procedure TWindow.DoMouseUp(var Args: TSceneMouseArgs);
begin
  inherited DoMouseUp(Args);
  if Args.Button = buttonLeft then
  begin
    FDrag := False;
    FSizing := False;
    { The button closes the window if the mouse is released over it }
    FCloseClick := FCloseDown and CloseRect.Contains(Args.X, Args.Y);
    FCloseDown := False;
  end;
end;

function CompareWidgets(constref A, B: TWidget): Integer;
begin
  if PtrUInt(A) < PtrUInt(B) then
    Result := -1
  else if PtrUInt(A) > PtrUInt(B) then
    Result := 1
  else
    Result := 0;
end;

initialization
  TWidgetList.DefaultCompare := CompareWidgets;
end.

