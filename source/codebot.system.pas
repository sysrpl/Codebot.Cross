(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified July 2017                                  *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.system.txt> }
unit Codebot.System;

{$i codebot.inc}

interface

uses
  { Codebot core unit }
  Codebot.Core,
  { Free pascal units }
  SysUtils, Classes, Process, FileUtil;

{$region types}
type
  Float = Single;
  PFloat = ^Float;
  LargeInt = Int64;
  PLargeInt = ^LargeInt;
  LargeWord = QWord;
  PLargeWord = ^LargeWord;
  SysInt = NativeInt;
  PSysInt = ^SysInt;
  HFile = Pointer;
{$endregion}

{$region dynamic library support}
type
  HModule = Codebot.Core.HModule;

const
  ModuleNil = Codebot.Core.ModuleNil;
  SharedSuffix = Codebot.Core.SharedSuffix;
  HiByte = High(Byte);

function LibraryLoad(const Name: string): HModule;
function LibraryUnload(Module: HModule): Boolean;
function LibraryGetProc(Module: HModule; const ProcName: string): Pointer;

type
  ELibraryException = class(Exception)
  private
    FModuleName: string;
    FProcName: string;
  public
    constructor Create(const ModuleName, ProcName: string);
    property ModuleName: string read FModuleName;
    property ProcName: string read FProcName;
  end;
{$endregion}

{$region system}
procedure FillZero(out Buffer; Size: UIntPtr); inline;
{$endregion}

{$region generic containers}
{ TArray<T> is a shortcut to a typed dynamic array }

type
  TArray<T> = array of T;

{ TCompare\<T\> is used to compare two items }
  TCompare<T> = function(constref A, B: T): Integer;
{ TConvert\<Source, Target\> is used to convert from one type to another }
  // TConvert<TItem, T> = function(constref Item: TItem): T; see issue #28766
{ TConvertString\<T\> is used to convert a type to a string }
 TConvertString<TItem> = function(constref Item: TItem): string;

{ TFilterFunc\<T\> is used to test if and item passes a test }

  TFilterFunc<T> = function(constref Value: T): Boolean;

{ ICloneable\<T\> represents an object which can clone T
  See also
  <link Overview.Codebot.System.ICloneable, ICloneable members> }

  ICloneable<T> = interface
  ['{2AF4D64F-3CA2-4777-AAAC-0CDC42B8C34A}']
    { Create a clone of T }
    function Clone: T;
  end;

{doc off}
  TArrayEnumerator<T> = class(TInterfacedObject, IEnumerator<T>)
  private
    FItems: TArray<T>;
    FPosition: Integer;
    FCount: Integer;
  public
    constructor Create(Items: TArray<T>; Count: Integer = -1);
    { IEnumerator<T> }
    function GetCurrent: T;
    function MoveNext: Boolean;
    procedure Reset;
    property Current: T read GetCurrent;
  end;
{doc on}

{ TSortingOrder can be used to a sort items forward, backwards, or not at all }

  TSortingOrder = (soAscend, soDescend, soNone);

{ TArrayList\<T\> is a simple extension to dynamic arrays
  See also
  <link Overview.Codebot.System.TArrayList, TArrayList\<T\> members> }

  TArrayList<T> = record
  public type
    {doc ignore}
    TArrayListEnumerator = class(TArrayEnumerator<T>) end;
    TCompareFunc = TCompare<T>;
    { Get the enumerator for the list }
    function GetEnumerator: IEnumerator<T>;
  private
    function CompareExists: Boolean;
    procedure QuickSort(Order: TSortingOrder; Compare: TCompare<T>; L, R: Integer);
    function GetIsEmpty: Boolean;
    function GetFirst: T;
    procedure SetFirst(const Value: T);
    function GetLast: T;
    procedure SetLast(const Value: T);
    function GetLength: Integer;
    procedure SetLength(Value: Integer);
    function GetData: Pointer;
    function GetItem(Index: Integer): T;
    procedure SetItem(Index: Integer; const Value: T);
  public
    class var DefaultCompare: TCompare<T>;
    class var DefaultConvertString: TConvertString<T>;
    { The array acting as a list }
    var Items: TArray<T>;
    class function ArrayOf(const Items: array of T): TArrayList<T>; static;
    class function Convert: TArrayList<T>; static;
    { Convert a list to an array }
    class operator Implicit(const Value: TArrayList<T>): TArray<T>;
    { Convert an array to a list }
    class operator Implicit(const Value: TArray<T>): TArrayList<T>;
    { Convert an open array to a list }
    class operator Implicit(const Value: array of T): TArrayList<T>;
    { Performs a simple safe copy of up to N elements }
    procedure Copy(out List: TArrayList<T>; N: Integer);
    { Performs a fast unsafe copy of up to N elements }
    procedure CopyFast(out List: TArrayList<T>; N: Integer);
    { Returns the lower bounds of the list }
    function Lo: Integer;
    { Returns the upper bounds of the list }
    function Hi: Integer;
    { Reverses theitems in the list }
    procedure Reverse;
    { Swap two items in the list }
    procedure Exchange(A, B: Integer);
    { Adds and item to the end of the list }
    procedure Push(const Item: T);
    { Appends an array of items to the list }
    procedure PushRange(const Collection: array of T);
    { Remove an item from the end of the list }
    function Pop: T;
    { Remove an item randomly from the list }
    function PopRandom: T;
    { Return a copy of the list with items passing through a filter }
    function Filter(Func: TFilterFunc<T>): TArrayList<T>;
    { Resurn the first item matching a condition }
    function FirstOf(Func: TFilterFunc<T>): T;
    { Removes an item by index from the list and decresaes the count by one }
    procedure Delete(Index: Integer);
    { Removes all items setting the count of the list to 0 }
    procedure Clear;
    { Sort the items using a comparer }
    procedure Sort(Order: TSortingOrder = soAscend; Comparer: TCompare<T> = nil);
    { Attempt to find the item using DefaultCompare }
    function IndexOf(const Item: T): Integer; overload;
    { Attempt to find the item using a supplied comparer }
    function IndexOf(const Item: T; Comparer: TCompare<T>): Integer; overload;
    { Join a the array into a string using a separator }
    function Join(const Separator: string; Convert: TConvertString<T> = nil): string;
    { Returns true if ther are no items in the list }
    property IsEmpty: Boolean read GetIsEmpty;
    { First item in the list }
    property First: T read GetFirst write SetFirst;
    { Last item in the list }
    property Last: T read GetLast write SetLast;
    { Number of items in the list }
    property Length: Integer read GetLength write SetLength;
    { Address where to the first item is located }
    property Data: Pointer read GetData;
    { Get or set an item }
    property Item[Index: Integer]: T read GetItem write SetItem; default;
  end;

{ TMap\<K, V\> is a array like simple dictionary
  See also
  <link Overview.Codebot.System.TMap, TMap\<K, V\> members> }

  TMap<K, V> = record
  private
    FKeys: TArrayList<K>;
    FValues: TArrayList<V>;
    function GetItem(const Key: K): V;
    procedure SetItem(const Key: K; const Value: V);
  public
    { Get or set and item using a key }
    property Item[const Key: K]: V read GetItem write SetItem;
  end;

{ TBaseGrowList }

  TBaseList = class
  public
    constructor Create(N: Integer = 0); virtual;
  end;

  TBaseListClass = class of TBaseList;

{ TGrowList\<T\> is a class for incrementally adding large amounts of growing data
  See also
  <link Overview.Codebot.System.TGrowList\<T\>, TGrowList\<T\> members> }

  TGrowList<T> = class(TBaseList)
  private
    FBuffer: TArrayList<T>;
    FCount: Integer;
    FLength: Integer;
    procedure Grow(N: Integer);
    function GetData(Index: Integer): Pointer;
    function GetItem(Index: Integer): T;
    procedure SetItem(Index: Integer; Value: T);
  protected
    procedure Added(N: Integer); virtual;
  public
    { Create a new dynamic buffer optionally allocating room for a N number
      of future items }
    constructor Create(N: Integer = 0); override;
    { Remove any extra data allocated by the previous grow }
    procedure Pack;
    { Create a copy of the list }
    function Clone: TObject; virtual;
    { Add a range of items to the list }
    procedure AddRange(const Range: array of T);
    { Add a single item to the list }
    procedure AddItem(const Item: T);
    { Clear the buffer optionally allocating room for a N number
      of future items }
    procedure Clear(N: Integer = 0);
    { Pointer to the data at a specified index }
    property Data[Index: Integer]: Pointer read GetData; default;
    { Item at specified index }
    property Item[Index: Integer]: T read GetItem write SetItem;
    { The number of items in the list }
    property Count: Integer read FCount;
  end;

{doc off}
  StringArray = TArrayList<string>;
  WordArray = TArrayList<Word>;
  IntArray = TArrayList<Integer>;
  Int64Array = TArrayList<Int64>;
  FloatArray = TArrayList<Float>;
  BoolArray = TArrayList<Boolean>;
  PointerArray = TArrayList<Pointer>;
  ObjectArray = TArrayList<TObject>;
  InterfaceArray = TArrayList<IInterface>;

function DefaultStringCompare(constref A, B: string): Integer;
function DefaultStringConvertString(constref Item: string): string;
function DefaultWordCompare(constref A, B: Word): Integer;
function DefaultWordConvertString(constref Item: Word): string;
function DefaultIntCompare(constref A, B: Integer): Integer;
function DefaultIntConvertString(constref Item: Integer): string;
function DefaultInt64Compare(constref A, B: Int64): Integer;
function DefaultInt64ConvertString(constref Item: Int64): string;
function DefaultFloatCompare(constref A, B: Float): Integer;
function DefaultFloatConvertString(constref Item: Float): string;
function DefaultObjectCompare(constref A, B: TObject): Integer;
function DefaultInterfaceCompare(constref A, B: IInterface): Integer;
function DefaultCompare8(constref A, B: Byte): Integer;
function DefaultCompare16(constref A, B: Word): Integer;
function DefaultCompare32(constref A, B: LongWord): Integer;
function DefaultCompare64(constref A, B: LargeWord): Integer;
{doc on}
{$endregion}

{$region math routines}
const
  Infinity = High(Integer);

{ A positive wrapping modulus }
function Modulus(Value, Divisor: Integer): Integer;
{ Return the even division of a quotient }
function Divide(const Quotient, Divisor: Extended): Extended;
{ Return the remainder of an even division }
function Remainder(const Quotient, Divisor: Extended): Extended;
{ Return the upper most value }
function Ceil(const Value: Extended): Extended;
{ Return the lower most value }
function Floor(const Value: Extended): Extended;
{ Tanget trigometric function }
function Tan(const X: Extended): Extended;
{ Combined sine and cosine single trigometric function }
procedure SinCos(const X: Single; out S, C: Single); overload;
{ Combined sine and cosine dobule trigometric function }
procedure SinCos(constref X: Double; out S, C: Double); overload;
{ Bind a value between 0 and 1 }
function Clamp(Percent: Float): Float;
{ Convert a float to a byte }
function FloatToByte(F: Float): Byte;
{ Convert degrees to radians }
function DegToRad(D: Float): Float;
{ Convert radians to degrees }
function RadToDeg(R: Float): Float;
{ Raise a float to an integer power }
function IntPower(Base: Float; Exponent: Integer): Float;

{$endregion}

{$region time routines}
{ Access to highly accurate time }
function TimeQuery: Double;

{ IStopwatch represents a highly accurate way to measure time
  See also
  <link Overview.Codebot.System.IStopwatch, IStopwatch members>
  <link Codebot.System.StopwatchCreate, StopwatchCreate function> }

type
  IStopwatch = interface(IInterface)
  ['{8E3ACC66-EE90-4289-B8C9-DF1F26E016A9}']
    {doc off}
    function GetTime: Double;
    function GetPaused: Boolean;
    procedure SetPaused(Value: Boolean);
    {doc on}
    { Update the time by querying for an accurate time }
    function Calculate: Double;
    { Reset the timer to zero }
    procedure Reset;
    { Time expired between the last calculate and reset }
    property Time: Double read GetTime;
    { Pauses time calculation }
    property Paused: Boolean read GetPaused write SetPaused;
  end;

{ Create a highly accurate stopwatch
  See also
  <link Codebot.System.IStopwatch, IStopwatch interface> }
function StopwatchCreate: IStopwatch;
{$endregion}

{$region string routines}
{ These string routines support UTF8 text (needs testing) }

const
  { End of line characters used by various operating systems [group string] }
  LineBreakStyles: array[TTextLineBreakStyle] of string = (#10, #13#10, #13);
  { The character used to begin command line switches [group string] }
  SwitchChar = '-';

{ Returns the line break sequence for the current operating system [group string] }
function LineBreak: string; inline;
{ Convert a string to uppercase [group string] }
function StrUpper(const S: string): string;
{ Convert a string to lowercase [group string] }
function StrLower(const S: string): string;
{ Copies a substring given a start and length [group string] }
function StrCopy(const S: string; Start: Integer; Len: Integer = 0): string;
{ Copy a memory buffer into a string [group string] }
function StrCopyData(P: Pointer; Len: Integer): string;
{ Inserts a substring into a string at a position [group string] }
function StrInsert(const S, SubStr: string; Position: Integer): string;
{ Compares two strings optionally ignoring case returning -1 if A comes before
  before B, 1 if A comes after b, ord 0 if A and B are equal [group string] }
function StrCompare(const A, B: string; IgnoreCase: Boolean = False): Integer;
{ Searches a string for a substring optionally ignoring case [group string] }
function StrFind(const S, SubStr: string; IgnoreCase: Boolean = False): Integer; overload;
{ Searches a string for a substring from a start position optionally ignoring case [group string] }
function StrFind(const S, SubStr: string; Start: Integer; IgnoreCase: Boolean = False): Integer; overload;
{ Returns the number of a substring matches within a string [group string] }
function StrFindCount(const S, SubStr: string; IgnoreCase: Boolean = False): Integer;
{ Returns an array of indices of a substring matches within a string [group string] }
function StrFindIndex(const S, SubStr: string; IgnoreCase: Boolean = False): IntArray;
{ Replaces every instance of a pattern in a string [group string] }
function StrReplace(const S, OldPattern, NewPattern: string; IgnoreCase: Boolean = False): string;
{ Replaces the first instance of a pattern in a string [group string] }
function StrReplaceOne(const S, OldPattern, NewPattern: string; IgnoreCase: Boolean = False): string;
{ Replaces everything aftger the first instance of a pattern in a string [group string] }
function StrReplaceAfter(const S, OldPattern, NewPattern: string; IgnoreCase: Boolean = False): string;
{ Trims white space from both sides of a string [group string] }
function StrTrim(const S: string): string;
{ Returns true if a case insensitive string matches a value [group string] }
function StrEquals(const S: string; Value: string): Boolean; overload;
{ Returns true if a case insensitive string matches a set of value [group string] }
function StrEquals(const S: string; const Values: array of string): Boolean; overload;
{ Returns the index of a string in a string array or -1 if there is no match [group string] }
function StrIndex(const S: string; const Values: array of string): Integer;
{ Splits a string into a string array using a separator [group string] }
function StrSplit(const S, Separator: string): StringArray;
{ Splits a string into a int array using a separator [group string] }
function StrSplitInt(const S, Separator: string): IntArray;
{ Splits a string into a int64 array using a separator [group string] }
function StrSplitInt64(const S, Separator: string): Int64Array;
{ Join a string array into a string using a separator [group string] }
function StrJoin(const A: StringArray; const Separator: string): string;
{ Join an int array into a string using a separator [group string] }
function StrJoinInt(const A: IntArray; const Separator: string): string;
{ Returns the first subsection of a string if it were split using a separator [group string] }
function StrFirstOf(const S, Separator: string): string;
{ Returns the second subsection of a string if it were split using a separator [group string] }
function StrSecondOf(const S, Separator: string): string;
{ Returns the last subsection of a string if it were split using a separator [group string] }
function StrLastOf(const S, Separator: string): string;
{ Search a string for a substring optionally ignoring case [group string] }
function StrContains(const S, SubStr: string; IgnoreCase: Boolean = False): Boolean;
{ Returns true if a string begins with a substring while optionally ignoring case [group string] }
function StrBeginsWith(const S, SubStr: string; IgnoreCase: Boolean = False): Boolean;
{ Returns true if a string end with a substring while optionally ignoring case [group string] }
function StrEndsWith(const S, SubStr: string; IgnoreCase: Boolean = False): Boolean;
{ Returns a string of a given length filled with one repeating character [group string] }
function StrOf(C: Char; Len: Integer): string;
{ Returns a string made to fit a given length padded on the left with a character [group string] }
function StrPadLeft(const S: string; C: Char; Len: Integer): string;
{ Returns a string made to fit a given length padded on the right with a character [group string] }
function StrPadRight(const S: string; C: Char; Len: Integer): string; overload;
{ Returns a string made to fit a given length padded on the right zeros [group string] }
function StrPadRight(const I: Integer; Len: Integer): string; overload;
{ Returns a string surrounded by quotes if it contains whitespace [group string] }
function StrQuote(const S: string): string;
{ Returns true if a string contains only whitespace characters [group string] }
function StrIsBlank(const S: string): Boolean;
{ Returns true if a string matches to rules of an identifier [group string] }
function StrIsIdent(const S: string): Boolean;
{ Returns true if a string matches to rules of an attribute [group string] }
function StrIsAttr(const S: string): Boolean;
{ Returns the line break style for a block of text [group string] }
function StrLineBreakStyle(const S: string): TTextLineBreakStyle;
{ Converts the line break style of a block of text using the desired style [group string] }
function StrAdjustLineBreaks(const S: string; Style: TTextLineBreakStyle): string; overload;
{ Converts the line break style of a block of text using the system defined style [group string] }
function StrAdjustLineBreaks(const S: string): string; overload;
{ Convert a string to a wide string }
function StrToWide(const S: string): WideString;
{ Convert a wide string to string }
function WideToStr(const S: WideString): string;

{ Returns true if a program has a matching switch
  See also
  <link Codebot.System.SwitchIndex, SwitchIndex function>
  <link Codebot.System.SwitchValue, SwitchValue function> [group string] }
function SwitchExists(const Switch: string): Boolean;
{ Returns the index if of a program's matching switch or -1 if no match was found
  See also
  <link Codebot.System.SwitchExists, SwitchExists function>
  <link Codebot.System.SwitchValue, SwitchValue function> [group string] }
function SwitchIndex(const Switch: string): Integer;
{ Returns the value if of a program's switch
  See also
  <link Codebot.System.SwitchExists, SwitchExists function>
  <link Codebot.System.SwitchIndex, SwitchIndex function> [group string] }
function SwitchValue(const Switch: string): string;
{ Convert an integer to a string [group string] }
function IntToStr(Value: Int64): string;
{ Convert an integer to a byte storage string [group string] }
function IntToStorage(Bytes: Int64): string;
{ Convert a string to an integer. Can throw an EConvertError exception. [group string] }
function StrToInt(const S: string): Integer;
{ Convert a string an integer. Returns a default value if conversion cannot be done. [group string] }
function StrToIntDef(const S: string; Default: Integer): Integer;
{ Convert a float to a string [group string] }
function FloatToStr(Value: Extended): string; overload;
{ Convert a float to a string with a given number of decimals [group string] }
function FloatToStr(Value: Extended; Decimals: Integer): string; overload;
{ Convert a float to a comma string with a given number of decimals [group string] }
function FloatToCommas(Value: Extended; Decimals: Integer = 0): string;
{ Convert a string to a float. Can throw an EConvertError exception. [group string] }
function StrToFloat(const S: string): Extended;
{ Convert a string a float. Returns a default value if conversion cannot be done. [group string] }
function StrToFloatDef(const S: string; Default: Extended): Extended;
{ Search for and return a named environment variable }
function StrEnvironmentVariable(const Name: string): string;
{ Formats a series of argument into a string [group string] }
function StrFormat(const S: string; Args: array of const): string;
{ Retrieve the compoent heirarchy [group string] }
function StrCompPath(Component: TComponent): string;
{$endregion}

{$region helpers}
{ StringHelper }

type
  StringHelper = record helper for string
  private
    function GetIsEmpty: Boolean;
    function GetIsWhitespace: Boolean;
    function GetIsIdentifier: Boolean;
    function GetIsAttribute: Boolean;
    function GetLength: Integer;
    procedure SetLength(Value: Integer);
  public
    { Convert to a string representation }
    function ToString: string;
    { Make a string unique, reducing its reference count to one }
    procedure Unique;
    { Repeat a character a given length a into string }
    procedure CharInto(C: Char; Len: Integer);
    { Copy a memory buffer into string }
    procedure CopyInto(P: Pointer; Len: Integer);
    { Inserts a substring at a position into string }
    procedure InsertInto(const SubStr: string; Position: Integer);
    { Returns true if a string matches a case insensitive value }
    function Equals(const Value: string; IgnoreCase: Boolean = False): Boolean; overload;
    { Returns true if a string matches any in a set of case insensitive values }
    function Equals(const Values: array of string; IgnoreCase: Boolean = False): Boolean; overload;
    { Compares two strings optionally ignoring case returning -1 if string comes before
      before value, 1 if string comes after value, ord 0 if string and value are equal }
    function Compare(const Value: string; IgnoreCase: Boolean = False): Integer;
    { Convert a string to uppercase }
    function ToUpper: string;
    { Convert a string to lowercase }
    function ToLower: string;
    { Copies a substring given a start and length }
    function Copy(Start: Integer; Len: Integer = 0): string;
    { Insert a substring given a start and length }
    function Insert(const SubStr: string; Position: Integer): string;
    { Searches a string for a substring optionally ignoring case }
    function IndexOf(const SubStr: string; IgnoreCase: Boolean = False): Integer; overload;
    { Searches a string for a substring from a start position optionally ignoring case }
    function IndexOf(const SubStr: string; Start: Integer; IgnoreCase: Boolean = False): Integer; overload;
    { Returns the number of a substring matches within a string }
    function MatchCount(const SubStr: string; IgnoreCase: Boolean = False): Integer;
    { Returns an array of indices of a substring matches within a string }
    function Matches(const SubStr: string; IgnoreCase: Boolean = False): IntArray;
    { Replaces every instance of a pattern in a string }
    function Replace(const OldPattern, NewPattern: string; IgnoreCase: Boolean = False): string;
    { Replaces the first instance of a pattern in a string }
    function ReplaceOne(const OldPattern, NewPattern: string; IgnoreCase: Boolean = False): string;
    { Replaces everything aftger the first instance of a pattern in a string }
    function ReplaceAfter(const OldPattern, NewPattern: string; IgnoreCase: Boolean = False): string;
    { Trims white space from both sides of a string }
    function Trim: string;
    { Returns the index of a string in a string array or -1 if there is no match }
    function ArrayIndex(const Values: array of string): Integer;
    { Break a string into lines separated by the break style  }
    function Lines: StringArray;
    { Returns the first entire line containing SubStr }
    function LineWith(const SubStr: string; IgnoreCase: Boolean = False): string;
    { Splits a string into a string array using a separator }
    function Split(Separator: string): StringArray;
    { Splits a string into a int array using a separator }
    function SplitInt(const Separator: string): IntArray;
    { Splits a string into a int64 array using a separator }
    function SplitInt64(const Separator: string): Int64Array;
    { Splits a string into word separated by whitespace }
    function Words(MaxColumns: Integer = 0): StringArray;
    { Returns the first subsection of a string if it were split using a separator }
    function FirstOf(const Separator: string): string;
    { Returns the second subsection of a string if it were split using a separator }
    function SecondOf(const Separator: string): string;
    { Returns the last subsection of a string if it were split using a separator }
    function LastOf(const Separator: string): string;
    { Returns the text exclusive between markers A and B }
    function Between(const MarkerA, MarkerB: string): string;
    { Search a string for a substring optionally ignoring case }
    function Contains(const SubStr: string; IgnoreCase: Boolean = False): Boolean;
    { Returns true if a string begins with a substring while optionally ignoring case }
    function BeginsWith(const SubStr: string; IgnoreCase: Boolean = False): Boolean; overload;
    { Returns true if a string begins with any substring while optionally ignoring case }
    function BeginsWith(const SubStrs: StringArray; IgnoreCase: Boolean = False): Boolean; overload;
    { Returns true if a string end with a substring while optionally ignoring case }
    function EndsWith(const SubStr: string; IgnoreCase: Boolean = False): Boolean; overload;
    { Returns true if a string end with any substring while optionally ignoring case }
    function EndsWith(const SubStrs: StringArray; IgnoreCase: Boolean = False): Boolean; overload;
    { Returns a string made to fit a given length padded on the left with a character }
    function PadLeft(C: Char; Len: Integer): string;
    { Returns a string made to fit a given length padded on the right with a character }
    function PadRight(C: Char; Len: Integer): string;
    { Returns a string surrounded by quotes if it contains whitespace }
    function Quote: string;
    { Formats a series of argument into a string }
    function Format(Args: array of const): string;
    { Analyze a string and find its line break style }
    function LineBreakStyle: TTextLineBreakStyle;
    { Converts the line break style of a string to a the desired style }
    function AdjustLineBreaks(Style: TTextLineBreakStyle): string; overload;
    { Converts the line break style of a string to the system preferred defined style }
    function AdjustLineBreaks: string; overload;
    { Gets true if a string contains only whitespace characters }
    property IsEmpty: Boolean read GetIsEmpty;
    { Gets true if a string contains only whitespace characters }
    property IsWhitespace: Boolean read GetIsWhitespace;
    { Gets true if a string matches to rules of an identifier }
    property IsIdentifier: Boolean read GetIsIdentifier;
    { Gets true if a string matches to rules of an attribute }
    property IsAttribute: Boolean read GetIsAttribute;
    {  Gets or sets the length allocated for the string }
    property Length: Integer read GetLength write SetLength;
  end;

{ IntHelper }

  IntHelper = record helper for Integer
  public
    { Convert to a string representation }
    function ToString: string;
    { Check if a number is inclusively between a range}
    function Between(Low, High: Integer): Boolean;
  end;

{ TDateTimeHelper }

  TDateTimeHelper = record helper for TDateTime
  public
    { Convert to a string representation }
    function ToString(Format: string = ''): string;
    { Convert to a string representation }
    function AddMinutes(const A: Integer): TDateTime;
    { Return the year portion of the date }
    function Year: Word;
    { Return the month portion of the date }
    function Month: Word;
    { Return the day  portion of the date }
    function Day: Word;
  end;

{ TStringsHelper }

  TStringsHelper = class helper for TStrings
  public
    procedure AddLine;
    procedure AddFormat(const S: string; const Args: array of const);
    function Contains(const S: string; IgnoreCase: Boolean = False): Boolean;
  end;
{$endregion}

{$region file management routines}
{ These file management routines support UTF8 file operations (needs testing) }

{ Delete a file }
function FileDelete(const FileName: string): Boolean;
{ Copy a file optionally preserving file time }
function FileCopy(const SourceName, DestName: string; PreserveTime: Boolean = False): Boolean;
{ Rename a file }
function FileRename(const OldName, NewName: String): Boolean;
{ Determine if a file exists }
function FileExists(const FileName: string): Boolean;
{ Get the size of a file in bytes }
function FileSize(const FileName: string): LargeWord;
{ Get the modified date of a file in bytes }
function FileDate(const FileName: string): TDateTime;
{ Extract the name portion of a file name [group files] }
function FileExtractName(const FileName: string): string;
{ Extract the extension portion of a file name [group files] }
function FileExtractExt(const FileName: string): string;
{ Change the extension portion of a file name [group files] }
function FileChangeExt(const FileName, Extension: string): string;
{ Extract the path of a file or directory }
function FileExtractPath(const FileName: string): string;
{ Write the contents of a file }
procedure FileWriteStr(const FileName: string; const Contents: string);
{ Read the contents of a file }
function FileReadStr(const FileName: string): string;
{ Write a line to a file }
procedure FileWriteLine(const FileName: string; const Line: string);
{ Create a directory }
function DirCreate(const Dir: string): Boolean;
{ Change to a new directory }
procedure DirChange(const Dir: string);
{ Get the current working directory }
function DirGetCurrent: string;
{ Set the current working directory }
function DirSetCurrent(const Dir: string): Boolean;
{ Get the temporary directory }
function DirGetTemp(Global: Boolean = False): string;
{ Delete a directory or optionaly only its contents }
function DirDelete(const Dir: string; OnlyContents: Boolean = False): Boolean;
{ Determine if a directory exists }
function DirExists(const Dir: string): Boolean;
{ Force a directory to exist }
function DirForce(const Dir: string): Boolean;
{ Change path delimiter to match system settings [group files] }
function PathAdjustDelimiters(const Path: string): string;
{ Combine two paths }
function PathCombine(const A, B: string; IncludeDelimiter: Boolean = False): string;
{ Expand a path to the absolute path }
function PathExpand(const Path: string): string;
{ Include the end delimiter for a path }
function PathIncludeDelimiter(const Path: string): string;
{ Exclude the end delimiter for a path }
function PathExcludeDelimiter(const Path: string): string;
{ Combine a uri }
function UriCombine(const A, B: string; IncludeDelimiter: Boolean = False): string;
{ Include a slash at the end of a uri }
function UriIncludeDelimiter(const Uri: string): string;
{ Exclude a slash from the end of a uri }
function UriExcludeDelimiter(const Uri: string): string;
{ Exclude a path element from a uri }
function UriExtractPath(const Uri: string): string;
{ Returns the location of the application configuration file }
function ConfigAppFile(Global: Boolean; CreateDir: Boolean = False): string;
{ Returns the location of the application configuration directory }
function ConfigAppDir(Global: Boolean; CreateDir: Boolean = False): string;
{ Find files from ParamStr at start index returning a strings object }
procedure FindFileParams(StartIndex: Integer; out FileParams: TStrings);

const
  faReadOnly  = $00000001;
  faHidden    = $00000002;
  faSysFile   = $00000004;
  faVolumeId  = $00000008;
  faDirectory = $00000010;
  faArchive   = $00000020;
  faSymLink   = $00000040;
  faAnyFile   = $0000003f;

{ FindOpen corrects path delimiters and convert search to an output parameter }
function FindOpen(const Path: string; Attr: Longint; out Search: TSearchRec): LongInt;
{ Find file system items from a path outputting to a TStrings object }
procedure FindFiles(const Path: string; out FileSearch: TStrings; Attributes: Integer = 0); overload;

type
  TFileSearchItem = record
  public
    { The full path to the file or folder }
    Name: string;
    { The size in bytes of the item }
    Size: LargeInt;
    { The last modified date and time }
    Modified: TDateTime;
    { Details about the type of item found }
    Attributes: Integer;
  end;

  TFileSearch = type TArrayList<TFileSearchItem>;

{ Find file system items from a path outputting to a TFileSearch array }
procedure FindFiles(const Path: string; out FileSearch: TFileSearch; Attributes: Integer = 0); overload;

type
  TFileSearchHelper = record helper for TFileSearch
  public
    { Sort search items by file name }
    procedure SortName(Order: TSortingOrder = soAscend);
    { Sort search items by file size }
    procedure SortSize(Order: TSortingOrder = soAscend);
    { Sort search items by file modified date }
    procedure SortModified(Order: TSortingOrder = soAscend);
  end;
{$endregion}

{ TNamedValues\<T\> is a simple case insensitive string based dictionary
  See also
  <link Overview.Codebot.System.TNamedValues, TNamedValues\<T\> members> }

type
  TNamedValues<T> = record
  public
    { Get the enumerator for the dictionary names }
    function GetEnumerator: IEnumerator<string>;
  private
    FNames: TArrayList<string>;
    FValues: TArrayList<T>;
    function GetCount: Integer;
    function GetEmpty: Boolean;
    function GetName(Index: Integer): string;
    function GetValue(const Name: string): T;
    function GetValueByIndex(Index: Integer): T;
  public
    { Adds or replace a named value in the dictionary }
    procedure Add(const Name: string; const Value: T);
    { Removed a named value from the dictionary }
    procedure Remove(const Name: string);
    { Removes an item by index from the dictionary and decresaes the count by one }
    procedure Delete(Index: Integer);
    { Removes all named values setting the count of the dictionary to 0 }
    procedure Clear;
    { The number of key value pairs in the dictionary }
    property Count: Integer read GetCount;
    { Returns true if ther are no named values in the dictionary }
    property Empty: Boolean read GetEmpty;
    { Names indexed by an integer }
    property Names[Index: Integer]: string read GetName;
    { Values indexed by a name }
    property Values[Name: string]: T read GetValue; default;
    { Values indexed by an integer }
    property ValueByIndex[Index: Integer]: T read GetValueByIndex;
  end;

{ TNamedEnumerable is a shortcut to expose an enumerable }

  TNamedEnumerable = record
  public
    { The enumerator }
    Enumerator: IEnumerator<string>;
    { Get the enumerator }
    function GetEnumerator: IEnumerator<string>;
  end;

{ INamedValues<T> is a reference type for TNamedValues<T> }

  INamedValues<T> = interface(IEnumerable<T>)
    ['{D228ADD8-4C4E-4C6C-A6F6-FA17FC307253}']
    function GetCount: Integer;
    function GetEmpty: Boolean;
    function GetName(Index: Integer): string;
    function GetValue(const Name: string): T;
    function GetValueByIndex(Index: Integer): T;
    procedure Add(const Name: string; const Value: T);
    procedure Remove(const Name: string);
    procedure Delete(Index: Integer);
    procedure Clear;
    property Count: Integer read GetCount;
    property Empty: Boolean read GetEmpty;
    property Names[Index: Integer]: string read GetName;
    property Values[Name: string]: T read GetValue; default;
    property ValueByIndex[Index: Integer]: T read GetValueByIndex;
  end;

{ TNamedStrings is a dictionary of string name value pairs }

  TNamedStrings = TNamedValues<string>;

{ INamedStrings is a reference type for TNamedStrings }

  INamedStrings = interface(INamedValues<string>)
    ['{C03EF776-46AC-4757-8654-F31EC34E67A7}']
  end;

{ TNamedValuesIntf<T> exposes INamedValues<T> }

  TNamedValuesIntf<T> = class(TInterfacedObject, IEnumerable<T>, INamedValues<T>)
  private
    FData: TNamedValues<T>;
  public
    { IEnumerable<T> }
    function GetEnumerator: IEnumerator<string>;
    { INamedValues<T> }
    function GetCount: Integer;
    function GetEmpty: Boolean;
    function GetName(Index: Integer): string;
    function GetValue(const Name: string): T;
    function GetValueByIndex(Index: Integer): T;
    procedure Add(const Name: string; const Value: T);
    procedure Remove(const Name: string);
    procedure Delete(Index: Integer);
    procedure Clear;
  end;

{ TNamedStringsIntf exposes INamedStrings }

  TNamedStringsIntf = class(TNamedValuesIntf<string>, INamedStrings)
  end;

{ IDelegate\<T\> allows event subscribers to add or remove their event handlers
  See also
  <link Overview.Codebot.System.IDelegate, IDelegate\<T\> members> }

  IDelegate<T> = interface
  ['{ADBC29C1-4F3D-4E4C-9A79-C805E8B9BD92}']
    { Check if there are no subscribers }
    function GetIsEmpty: Boolean;
    { A subscriber calls add to register an event handler }
    procedure Add(const Handler: T);
    { A subscriber calls remove to unregister an event handler }
    procedure Remove(const Handler: T);
    { Empty is true when there are no subscribers }
    property IsEmpty: Boolean read GetIsEmpty;
  end;

{doc off}
  IDelegateContainer<T> = interface
  ['{ED255F00-3112-4315-9E25-3C1B3064C932}']
    function GetEnumerator: IEnumerator<T>;
    function GetDelegate: IDelegate<T> ;
    property Delegate: IDelegate<T> read GetDelegate;
  end;

  TDelegateImpl<T> = class(TInterfacedObject, IDelegate<T>)
  private
    FList: TArrayList<T>;
    function IndexOf(Event: T): Integer;
  protected
    function GetIsEmpty: Boolean;
    procedure Add(const Event: T);
    procedure Remove(const Event: T);
  end;

  TDelegateContainerImpl<T> = class(TInterfacedObject, IDelegateContainer<T>)
  private
    type TDelegateClass = TDelegateImpl<T>;
    var FDelegateClass: TDelegateClass;
    var FDelegate: IDelegate<T>;
  protected
    { IDelegateContainer<T> }
    function GetEnumerator: IEnumerator<T>;
    function GetDelegate: IDelegate<T>;
  end;
{doc on}

{ TDelegate\<T\> allows an event publisher accept multiple subscribers
  See also
  <link Overview.Codebot.System.TDelegate, TDelegate\<T\> members> }

  TDelegate<T> = record
  private
    type TDelegateContainer = TDelegateContainerImpl<T>;
    var FContainer: IDelegateContainer<T>;
    function GetContainer: IDelegateContainer<T>;
  public
    { Convert a delegate into an interface suitable for subscribers }
    class operator Implicit(var Delegate: TDelegate<T>): IDelegate<T>;
    { Get the enumerator of subscriber's events }
    function GetEnumerator: IEnumerator<T>;
    { Check is there are no subscribers }
    function GetIsEmpty: Boolean;
    { Add an event handler }
    procedure Add(const Handler: T);
    { Remove an event handler }
    procedure Remove(const Handler: T);
    { Returns true is there a no subscribers }
    property IsEmpty: Boolean read GetIsEmpty;
  end;

  { Notify event publisher }
  TNotifyDelegate = TDelegate<TNotifyEvent>;
  { Notify event subscriber }
  INotifyDelegate = IDelegate<TNotifyEvent>;
  { Vanilla method }
  TMethodEvent = procedure of object;
  { Method event publisher }
  TMethodDelegate = TDelegate<TMethodEvent>;
  { Method event subscriber }
  IMethodDelegate = IDelegate<TMethodEvent>;

{ TChangeNotifier allows components to be notified of changes to owned classes
  See also
  <link Overview.Codebot.System.TChangeNotifier, TChangeNotifier members> }

  TChangeNotifier = class(TPersistent)
  private
    FOnChange: TNotifyDelegate;
    function GetOnChange: INotifyDelegate;
  protected
    { Notify component subscribers of changes }
    procedure Change; virtual;
  public
    { Allow component subscribers to add or remove change notification }
    property OnChange: INotifyDelegate read GetOnChange;
  end;

{ IFloatPropertyNotify is alerted when a property is updated
  See also
  <link Overview.Codebot.System.IFloatPropertyNotify, IFloatPropertyNotify members> }

  IFloatPropertyNotify = interface
  ['{52093327-66EF-4909-BEE6-91DB39E08C6C}']
    { Notify an object one of its float properties has changed }
    procedure PropChange(Prop: PFloat);
  end;

{ Compare two block of memory returning true if they are the same }
function MemCompare(const A, B; Size: LongWord): Boolean;
{$endregion}

{$region classes}
{ TNullResult holds bytes written each second to a null stream
  See also
  <link Codebot.System.TNullStream, TNullStream class> }

type
  TNullResult = TArrayList<LongWord>;

{ TNullInfo holds information related to bytes read or written
  See also
  <link Overview.Codebot.System.TNullInfo, TNullInfo members>
  <link Codebot.System.TNullStream, TNullStream class> }

  TNullInfo = class(TObject)
  private
    FTime: Double;
    FCount: LongWord;
    FBytes: LongWord;
    FRate: LongWord;
    FRateBytes: LongWord;
    FRateTime: Double;
    FAvergage: LongWord;
    FAvergageTotal: LongWord;
    FAvergageCount: LongWord;
    FSeconds: LongWord;
    FResult: TNullResult;
  public
    { Resets the counting and return a recording of bytes transfered per second }
    function Reset: TNullResult;
    { Thread safe total bytes transfered  }
    property Bytes: LongWord read FBytes;
    { Thread safe realtime estimate of bytes transfered this second }
    property Rate: LongWord read FRate;
    { Thread safe realtime average bytes transfered in total }
    property Avergage: LongWord read FAvergage;
    { Thread safe number of seconds since the last reset }
    property Seconds: LongWord read FSeconds;
  end;

{ TNullStream does nothing other than records bytes read or written per second
  See also
  <link Overview.Codebot.System.TNullStream, TNullStream members> }

  TNullStream = class(TStream)
  private
    FReadInfo: TNullInfo;
    FWriteInfo: TNullInfo;
    procedure RecordInfo(Info: TNullInfo; Count: LongWord);
  protected
    {doc off}
    procedure SetSize(NewSize: Longint); override;
    procedure SetSize(const NewSize: Int64); override;
    {doc on}
  public
    { Create a new null stream }
    constructor Create;
    destructor Destroy; override;
    { Ignores buffer and records count read bytes
      Remarks
      If two seconds or more have passed since the last read the null stream
      will automatically read reset }
    function Read(var Buffer; Count: Longint): Longint; override;
    { Ignores buffer and records count written bytes
      Remarks
      If two seconds or more have passed since the last write the null stream
      will automatically write reset }
    function Write(const Buffer; Count: Longint): Longint; override;
    { Does nothing and returns zero }
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    { A log of read information }
    property ReadInfo: TNullInfo read FReadInfo;
    { A log of write information }
    property WriteInfo: TNullInfo read FWriteInfo;
  end;
{$endregion}


{$region threading}
{ IMutex allows threads to wait for an exclusive locked ownership of a mutex ibject
  Note
  On unix systems cthreads must be the first unit in your program source if you want thread support
  See also
  <link Overview.Codebot.System.IMutex, IMutex members> }

  IMutex = interface
    { Lock causes the current thread to wait for exclusive ownership of a mutex object }
    function Lock: LongInt;
    { Unlock releases exclusive access to a mutex object, allowing the next waiting thread to take ownership }
    function Unlock: LongInt;
  end;

{ IEvent allows many threads to wait until an event object signals them to continue
  Note
  On unix systems cthreads must be the first unit in your program source if you want thread support
  See also
  <link Overview.Codebot.System.IEvent, IEvent members> }

  IEvent = interface
    { Reset reverts an event object to a non signaled state }
    procedure Reset;
    { Signals threads waiting for the event object to continue }
    procedure Signal;
    { Wait causes the current thread to suspsend execution until the event object is signaled }
    procedure Wait;
  end;

{ Create a new mutex object }
function MutexCreate: IMutex;
{ Create a new event object }
function EventCreate: IEvent;

type
  {doc off}
  TSimpleThread = class;
  {doc on}

{ TThreadExecuteMethod is the method invoked when a TSimpleThread is created }

  TThreadExecuteMethod = procedure(Thread: TSimpleThread) of object;

{ TSimpleThread allows objects to create a threading method without defining a new thread class
  See also
  <link Overview.Codebot.System.TSimpleThread, TSimpleThread members> }

  TSimpleThread = class(TThread)
  private
    FExecuteMethod: TThreadExecuteMethod;
    FTempStatus: string;
    FStatus: string;
    FOnStatus: TNotifyEvent;
    procedure DoStatus;
    procedure SetStatus(const Value: string);
  protected
    { Sets FreeOnTerminate to True and executes the method }
    procedure Execute; override;
  public
    { Create immediately starts ExecuteMethod on a new thread }
    constructor Create(ExecuteMethod: TThreadExecuteMethod;
      OnStatus: TNotifyEvent = nil; OnTerminate: TNotifyEvent = nil);
    { Synchronize can be used by ExecuteMethod to invoke a method
      on the main thread }
    procedure Synchronize(Method: TThreadMethod);
    { You should only set status inside ExecuteMethod }
    property Status: string read FStatus write SetStatus;
    { Terminated is set to True after the Terminate method is called }
    property Terminated;
  end;

{ Execute a process, wait for it to terminate, and return its output }
function ProcessExecute(const Command: string; const Arguments: string = ''): string;
{$endregion}

{$region waiting routines}
{ Definable message pump }
var
  PumpMessagesProc: procedure;

{ Retrieve messages from a queue while waiting }
procedure PumpMessages;
{$endregion}

implementation

{$region dynamic library support}

function LibraryLoad(const Name: string): HModule;
begin
  Result := Codebot.Core.LibraryLoad(Name);
end;

function LibraryUnload(Module: HModule): Boolean;
begin
  Result := Codebot.Core.LibraryUnload(Module);
end;

function LibraryGetProc(Module: HModule; const ProcName: string): Pointer;
begin
  Result := Codebot.Core.LibraryGetProc(Module, ProcName);
end;

{ TNamedEnumerable }

function TNamedEnumerable.GetEnumerator: IEnumerator<string>;
begin
  Result := Enumerator;
end;

{ ELibraryException }

constructor ELibraryException.Create(const ModuleName, ProcName: string);
const
  SLibraryModuleError = 'The dynamic library "%s" could not be located';
  SLibraryProcError = 'The function "%s" in dynamic library "%s" could not be loaded';
var
  S: string;
begin
  FModuleName := ModuleName;
  FProcName := ProcName;
  if FProcName <> '' then
    S := Format(SLibraryProcError, [FModuleName, FProcName])
  else
    S := Format(SLibraryModuleError, [FModuleName]);
  inherited Create(S);
end;

procedure LibraryExcept(const ModuleName: string; ProcName: string);
begin
  raise ELibraryException.Create(ModuleName, ProcName);
end;
{$endregion}

{$region system}
{$hints off}
procedure FillZero(out Buffer; Size: UIntPtr);
begin
  FillChar(Buffer, Size, 0);
end;
{$hints on}
{$endregion}

{$region math routines}
function Modulus(Value, Divisor: Integer): Integer;
begin
  Result := Value mod Divisor;
  if Result < 0 then
    Result := Value + Divisor;
end;

function Divide(const Quotient, Divisor: Extended): Extended;
begin
  if Divisor = 0 then
    Result := 0
  else
    Result := Trunc(Quotient / Divisor) * Divisor;
end;

function Remainder(const Quotient, Divisor: Extended): Extended;
begin
  if Divisor = 0 then
    Result := 0
  else
    Result := Quotient - Divisor * Trunc(Quotient / Divisor);
end;

function Ceil(const Value: Extended): Extended;
begin
  Result := Trunc(Value);
  if Result >= 0 then
    Result := Result + 1;
end;

function Floor(const Value: Extended): Extended;
begin
  Result := Trunc(Value);
  if Result < 0 then
    Result := Result - 1;
end;

function Tan(const X: Extended): Extended;
begin
  Result := Sin(X) / Cos(X);
end;

procedure SinCos(const X: Single; out S, C: Single);
begin
  S := Sin(X);
  C := Cos(X);
end;

procedure SinCos(constref X: Double; out S, C: Double);
begin
  S := Sin(X);
  C := Cos(X);
end;

function Clamp(Percent: Float): Float;
begin
  if Percent < 0 then
    Result := 0
  else if Percent > 1 then
    Result := 1
  else
    Result := Percent;
end;

function FloatToByte(F: Float): Byte;
begin
  if F < 0 then
    F := 0;
  if F > 1 then
    Result := 1
  else
    Result := Round(F * $FF);
end;

function DegToRad(D: Float): Float;
begin
  Result := D / 180 * Pi;
end;

function RadToDeg(R: Float): Float;
begin
  Result := R * 180 / Pi;
end;

function IntPower(Base: Float; Exponent: Integer): Float;
var
  I: LongInt;
begin
 if (Base = 0.0) and (Exponent = 0) then
   Exit(1);
  I := Abs(Exponent);
  Result := 1.0;
  while I > 0 do
  begin
    while (I and 1)=0 do
    begin
      I := I shr 1;
      Base := Sqr(Base);
    end;
    I := I - 1;
    Result := Result * Base;
  end;
  if Exponent < 0 then
    Result := 1 / Result;
end;
{$endregion}

{$region time routines}
{$ifdef unix}

type
  TTimeVal = packed record
    Sec: SysInt;  { Seconds }
    MSec: SysInt; { Microseconds }
  end;
  PTimeVal = ^TTimeVal;

const
{$ifdef linux}
  libc = 'libc.so.6';
{$endif}
{$ifdef darwin}
  libc = 'libSystem.dylib';
{$endif}
function gettimeofday(out TimeVal: TTimeVal; TimeZone: PTimeVal): Integer; apicall; external libc;

var
  TimeSec: SysInt;

function TimeQuery: Double;
var
  TimeVal: TTimeVal;
begin
  gettimeofday(TimeVal, nil);
  if TimeSec = 0 then
    TimeSec := TimeVal.Sec;
  TimeVal.Sec := TimeVal.Sec - TimeSec;
  Result := TimeVal.Sec + TimeVal.MSec / 1000000;
end;
{$endif}

{$ifdef windows}
const
  kernel32  = 'kernel32.dll';

function QueryPerformanceCounter(var Counter: Int64): LongBool; apicall; external kernel32;
function QueryPerformanceFrequency(var Frequency: Int64): LongBool; apicall; external kernel32;

function TimeQuery: Double;
var
  C, F: Int64;
begin
  F := 0;
  C := 0;
  if QueryPerformanceFrequency(F) and QueryPerformanceCounter(C) then
    Result := C / F
  else
    Result := 0;
end;
{$endif}

{ TStopwatchImpl }

type
  TStopwatchImpl = class(TInterfacedObject, IStopwatch)
  private
    FPaused: Boolean;
    FTime: Double;
    FStart: Double;
    FStop: Double;
  public
    constructor Create;
    function GetTime: Double;
    function GetPaused: Boolean;
    procedure SetPaused(Value: Boolean);
    function Calculate: Double;
    procedure Reset;
  end;

constructor TStopwatchImpl.Create;
begin
  inherited Create;
  Reset;
end;

function TStopwatchImpl.GetTime: Double;
begin
  Result := FTime;
end;

function TStopwatchImpl.GetPaused: Boolean;
begin
  Result := FPaused;
end;

procedure TStopwatchImpl.SetPaused(Value: Boolean);
var
  Last: Double;
begin
  if Value <> FPaused then
  begin
    FPaused := Value;
    if not FPaused then
    begin
      Last := FStop;
      Calculate;
      FStart := FStart + (FStop - Last);
      FTime := FStop - FStart;
    end;
  end;
end;

function TStopwatchImpl.Calculate: Double;
begin
  if not FPaused then
  begin
    FStop := TimeQuery;
    FTime := FStop - FStart;
  end;
  Result := FTime;
end;

procedure TStopwatchImpl.Reset;
begin
  FStart := TimeQuery;
  FStop := FStart;
  FTime := 0;
end;

function StopwatchCreate: IStopwatch;
begin
  Result := TStopwatchImpl.Create;
end;
{$endregion}

{$region string routines}
function LineBreak: string;
begin
  Result := LineBreakStyles[DefaultTextLineBreakStyle];
end;

function StrUpper(const S: string): string;
begin
  Result := UpCase(S);
end;

function StrLower(const S: string): string;
begin
  Result := LowerCase(S);
end;

function StrBufCompareI(A, B: PChar): Integer;
const
  CharA = Ord('A');
  CharZ = Ord('Z');
  CharDelta = Ord('a') - Ord('A');
var
  B1: PByte absolute A;
  B2: PByte absolute B;
  C1, C2: Byte;
begin
  repeat
    C1 := B1^;
    C2 := B2^;
    if (C1 >= CharA) and (C1 <= CharZ) then
      Inc(C1, CharDelta);
    if (C2 >= CharA) and (C2 <= CharZ) then
      Inc(C2, CharDelta);
    Inc(B1);
    Inc(B2);
  until (C1 <> C2) or (C1 = 0);
  if C1 < C2 then
    Exit(-1);
  if C1 > C2 then
    Exit(1);
  Exit(0);
end;

function StrBufCompare(A, B: PChar): Integer;
var
  B1: PByte absolute A;
  B2: PByte absolute B;
  C1, C2: Byte;
begin
  repeat
    C1 := B1^;
    C2 := B2^;
    Inc(B1);
    Inc(B2);
  until (C1 <> C2) or (C1 = 0);
  if C1 < C2 then
    Exit(-1);
  if C1 > C2 then
    Exit(1);
  Exit(0);
end;

function StrCompare(const A, B: string; IgnoreCase: Boolean = False): Integer;
begin
  if (Length(A) = 0) and (Length(B) = 0) then
    Exit(0);
  if Length(A) = 0 then
    Exit(-1);
  if Length(B) = 0 then
    Exit(1);
  if IgnoreCase then
    Result := StrBufCompareI(PChar(A), PChar(B))
  else
    Result := StrBufCompare(PChar(A), PChar(B));
end;

function StrCopy(const S: string; Start: Integer; Len: Integer = 0): string;
  var
  A, B: PChar;
  I: Integer;
begin
  Result := '';
  if S = '' then
    Exit;
  if Start < 1 then
    Exit;
  I := Length(S);
  if Start > I then
    Exit;
  if Len < 1 then
    Len := Length(S);
  Dec(Start);
  if Start + Len > I then
    Len := I - Start;
  Setlength(Result, Len);
  A := PChar(S);
  B := PChar(Result);
  Inc(A, Start);
  Move(A^, B^, Len);
end;

function StrCopyData(P: Pointer; Len: Integer): string;
begin
  if Len < 1 then
    Exit('');
  SetLength(Result, Len);
  Move(P^, PChar(Result)^, Len);
end;

function StrInsert(const S, SubStr: string; Position: Integer): string;
begin
  if Position < 1 then
    Position := 1
  else if Position > Length(S) then
    Position := Length(S);
  if Position = 1 then
    Exit(SubStr + S);
  if Position = Length(S) then
    Exit(S + SubStr);
  Result := StrCopy(S, 1, Position - 1) + SubStr + StrCopy(S, Position);
end;

function StrFindBuffer(S, SubStr: PChar; SLen, SubStrLen: Integer): Integer;
var
  Current, Last: Char;
  Lookup: array[Low(Byte)..High(Byte)] of Integer;
  B: Byte;
  I, J, K: Integer;
begin
  Result := 0;
  if  (SLen = 0) or (SubStrLen = 0) then
    Exit;
  Dec(S);
  Dec(SubStr);
  for I := Low(Lookup) to High(Lookup) do
    Lookup[I] := SubStrLen;
  for I := 1 to SubStrLen - 1 do
  begin
    B := Ord(SubStr[I]);
    Lookup[B] := SubStrLen - I;
  end;
  Last := SubStr[SubStrLen];
  I := SubStrLen;
  while I <= SLen do
  begin
    Current := S[I];
    if Current = Last then
    begin
      J := I - SubStrLen;
      K := 1;
      while K < SubStrLen do
      begin
        if SubStr[K] <> S[J + K] then
          Break;
        Inc(K);
      end;
      if K = SubStrLen then
      begin
        Result := J + 1;
        Exit;
      end;
      B := Ord(Current);
      Inc(I, Lookup[B]);
    end
    else
    begin
      B := Ord(Current);
      Inc(I, Lookup[B]);
    end;
  end;
end;

function StrFindBufferI(S, SubStr: PChar; SLen, SubStrLen: Integer): Integer;
var
  Current, Last: Char;
  Lookup: array[Low(Byte)..High(Byte)] of Integer;
  B: Byte;
  I, J, K: Integer;
begin
  Result := 0;
  if (SubStrLen = 0) or (SLen = 0) then
    Exit;
  Dec(SubStr);
  Dec(S);
  for I := Low(Lookup) to High(Lookup) do
    Lookup[I] := SubStrLen;
  for I := 1 to SubStrLen - 1 do
  begin
    B := Ord(UpCase(SubStr[I]));
    Lookup[B] := SubStrLen - I;
  end;
  Last := UpCase(SubStr[SubStrLen]);
  I := SubStrLen;
  while I <= SLen do
  begin
    Current := UpCase(S[I]);
    if Current = Last then
    begin
      J := I - SubStrLen;
      K := 1;
      while K < SubStrLen do
      begin
        if UpCase(SubStr[K]) <> UpCase(S[J + K]) then
          Break;
        Inc(K);
      end;
      if K = SubStrLen then
      begin
        Result := J + 1;
        Exit;
      end;
      B := Ord(Current);
      Inc(I, Lookup[B]);
    end
    else
    begin
      B := Ord(Current);
      Inc(I, Lookup[B]);
    end;
  end;
end;

function StrTrim(const S: string): string;
const
  WhiteSpace = [#0..' '];
var
  Len, I: Integer;
begin
  Len := Length(S);
  while (Len > 0) and (S[Len] in WhiteSpace) do
   Dec(Len);
  I := 1;
  while ( I <= Len) and (S[I] in WhiteSpace) do
    Inc(I);
  Result := Copy(S, I, 1 + Len - I);
end;

function StrFind(const S, SubStr: string; IgnoreCase: Boolean = False): Integer;
begin
  if IgnoreCase then
    Result := StrFindBufferI(PChar(S), PChar(SubStr), Length(S), Length(SubStr))
  else
    Result := StrFindBuffer(PChar(S), PChar(SubStr), Length(S), Length(SubStr));
end;

function StrFind(const S, SubStr: string; Start: Integer; IgnoreCase: Boolean = False): Integer;
var
  P: PChar;
  I: Integer;
begin
  P := PChar(S);
  I := Length(S);
  if (Start < 1) or (Start > I) then
  begin
    Result := 0;
    Exit;
  end;
  Dec(Start);
  Inc(P, Start);
  Dec(I, Start);
  if IgnoreCase then
    Result := StrFindBufferI(P, PChar(SubStr), I, Length(SubStr))
  else
    Result := StrFindBuffer(P, PChar(SubStr), I, Length(SubStr));
  if Result > 0 then
    Inc(Result, Start);
end;

function StrFindCount(const S, SubStr: string; IgnoreCase: Boolean = False): Integer;
var
  Start, Index, Len: Integer;
begin
  Result := 0;
  Start := 1;
  Len := Length(SubStr);
  repeat
    Index := StrFind(S, SubStr, Start, IgnoreCase);
    if Index > 0 then
    begin
      Inc(Result);
      Start := Index + Len;
    end;
  until Index = 0;
end;

function StrFindIndex(const S, SubStr: string; IgnoreCase: Boolean = False): IntArray;
var
  Start, Index, Len: Integer;
begin
  Result.Length := StrFindCount(S, SubStr, IgnoreCase);
  Start := 1;
  Index := 0;
  Len := Length(SubStr);
  while Index < Result.Length do
  begin
    Start := StrFind(S, SubStr, Start, IgnoreCase);
    Result[Index] := Start;
    Inc(Start, Len);
    Inc(Index);
  end;
end;

function StrReplace(const S, OldPattern, NewPattern: string; IgnoreCase: Boolean = False): string;
var
  PosIndex: IntArray;
  FindIndex, FindLen, OldIndex, OldLen, NewIndex, NewLen, I: Integer;
begin
  PosIndex := StrFindIndex(S, OldPattern, IgnoreCase);
  FindLen := PosIndex.Length;
  if FindLen = 0 then
  begin
    Result := S;
    Exit;
  end;
  OldLen := S.Length;
  NewLen := OldLen + NewPattern.Length * FindLen - OldPattern.Length * FindLen;
  SetLength(Result, NewLen);
  OldIndex := 1;
  NewIndex := 1;
  FindIndex := 0;
  while OldIndex <= OldLen do
  begin
    if (FindIndex < FindLen) and (OldIndex = PosIndex[FindIndex]) then
    begin
      Inc(OldIndex, OldPattern.Length);
      for I := 0 to NewPattern.Length - 1 do
        Result[NewIndex + I] := NewPattern[I + 1];
      Inc(NewIndex, NewPattern.Length);
      Inc(FindIndex);
    end
    else
    begin
      Result[NewIndex] := S[OldIndex];
      Inc(OldIndex);
      Inc(NewIndex);
    end;
  end;
end;

function StrReplaceOne(const S, OldPattern, NewPattern: string; IgnoreCase: Boolean = False): string;
var
  I: Integer;
begin
  I := StrFind(S, OldPattern, IgnoreCase);
  if I > 0 then
    Result := Copy(S, 1, I - 1) + NewPattern + Copy(S, I + Length(OldPattern), Length(S))
  else
    Result := S;
end;

function StrReplaceAfter(const S, OldPattern, NewPattern: string; IgnoreCase: Boolean = False): string;
var
  I: Integer;
begin
  I := StrFind(S, OldPattern, IgnoreCase);
  if I > 0 then
    Result := Copy(S, 1, I - 1) + NewPattern
  else
    Result := S;
end;

function StrEquals(const S: string; Value: string): Boolean;
begin
  Result := StrCompare(S, Value, True) = 0;
end;

function StrEquals(const S: string; const Values: array of string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := Low(Values) to High(Values) do
    if StrCompare(S, Values[I], True) = 0 then
    begin
      Result := True;
      Break;
    end;
end;

function StrIndex(const S: string; const Values: array of string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := Low(Values) to High(Values) do
    if S = Values[I] then
    begin
      Result := I;
      Break;
    end;
end;

function StrSplit(const S, Separator: string): StringArray;
var
  Splits: IntArray;
  Pos: Integer;
  I: Integer;
begin
  if S.Length < 1 then
    Exit;
  if Separator.Length < 1 then
    Exit;
  if StrFind(S, Separator) < 1 then
  begin
    Result.Length := 1;
    Result[0] := S;
    Exit;
  end;
  Splits := StrFindIndex(S, Separator);
  Result.Length := Splits.Length + 1;
  Pos := 1;
  for I := Splits.Lo to Splits.Hi do
  begin
    Result[I] := Copy(S, Pos, Splits[I] - Pos);
    Pos := Splits[I] + Separator.Length;
  end;
  Result.Items[Splits.Length] := Copy(S, Pos, S.Length);
end;

function StrSplitInt(const S, Separator: string): IntArray;
var
  Data: StringArray;
  I: Integer;
begin
  Data := StrSplit(S, Separator);
  Result.Length := Data.Length;
  try
    for I := Data.Lo to Data.Hi do
      Result[I] := SysUtils.StrToInt(Data[I]);
  except
    Result.Clear;
  end;
end;

function StrSplitInt64(const S, Separator: string): Int64Array;
var
  Data: StringArray;
  I: Integer;
begin
  Data := StrSplit(S, Separator);
  Result.Length := Data.Length;
  try
    for I := Data.Lo to Data.Hi do
      Result[I] := SysUtils.StrToInt64(Data[I]);
  except
    Result.Clear;
  end;
end;

function StrJoin(const A: StringArray; const Separator: string): string;
var
  I: Integer;
begin
  Result := '';
  if A.Length < 1 then
    Exit;
  Result := A.First;
  for I := A.Lo + 1 to A.Hi do
    Result := Result + Separator + A[I];
end;

function StrJoinInt(const A: IntArray; const Separator: string): string;
var
  I: Integer;
begin
  Result := '';
  if A.Length < 1 then
    Exit;
  Result := SysUtils.IntToStr(A.First);
  for I := A.Lo + 1 to A.Hi do
    Result := Result + Separator + SysUtils.IntToStr(A[I]);
end;

function StrFirstOf(const S, Separator: string): string;
var
  I: Integer;
begin
  I := StrFind(S, Separator);
  if I > 0 then
    if I = 1 then
      Result := ''
    else
      Result := StrCopy(S, 1, I - 1)
  else
    Result := S;
end;

function StrSecondOf(const S, Separator: string): string;
var
  I: Integer;
begin
  I := StrFind(S, Separator);
  if I > 0 then
    Result := StrCopy(S, I + Length(Separator))
  else
    Result := '';
end;

function StrLastOf(const S, Separator: string): string;
var
  A: StringArray;
begin
  A := StrSplit(S, Separator);
  if A.Length > 0 then
    Result := A.Last
  else
    Result := '';
end;

function StrContains(const S, SubStr: string; IgnoreCase: Boolean = False): Boolean;
begin
  if Length(S) < 1 then
    Exit(False);
  if Length(SubStr) < 1 then
    Exit(False);
  if Length(SubStr) > Length(S) then
    Exit(False);
  Result := StrFind(S, SubStr, IgnoreCase) > 0;
end;

function StrBeginsWith(const S, SubStr: string; IgnoreCase: Boolean = False): Boolean;
var
  C: string;
begin
  if Length(S) < 1 then
    Exit(False);
  if Length(SubStr) < 1 then
    Exit(False);
  if Length(SubStr) > Length(S) then
    Exit(False);
  C := StrCopy(S, 1, Length(SubStr));
  Result := StrCompare(C, SubStr, IgnoreCase) = 0;
end;

function StrEndsWith(const S, SubStr: string; IgnoreCase: Boolean = False): Boolean;
var
  C: string;
begin
  if Length(S) < 1 then
    Exit(False);
  if Length(SubStr) < 1 then
    Exit(False);
  if Length(SubStr) > Length(S) then
    Exit(False);
  C := StrCopy(S, Length(S) - Length(SubStr) + 1, Length(SubStr));
  Result := StrCompare(C, SubStr, IgnoreCase) = 0;
end;

function StrOf(C: Char; Len: Integer): string;
var
  I: Integer;
begin
  if Len < 1 then
    Exit;
  SetLength(Result, Len);
  for I := 1 to Len do
    Result[I] := C;
end;

function StrPadLeft(const S: string; C: Char; Len: Integer): string;
var
  I: Integer;
begin
  Result := '';
  I := Length(S);
  if I < 1 then
    Exit;
  if Len < 1 then
    Exit;
  if I > Len then
  begin
    Result := Copy(S, 1, Len);
    Exit;
  end;
  Result := S + StrOf(C, Len - I);
end;

function StrPadRight(const S: string; C: Char; Len: Integer): string;
var
  I: Integer;
begin
  Result := '';
  I := Length(S);
  if I > Len then
  begin
    Result := Copy(S, Len - I, Len);
    Exit;
  end;
  Result := StrOf(C,  Len - I) + S;
end;

function StrPadRight(const I: Integer; Len: Integer): string;
begin
  Result := IntToStr(I);
  Result := StrPadRight(Result, '0', Len);
end;

function StrQuote(const S: string): string;
begin
  if StrContains(S, ' ' ) then
    Result := '"' + StrReplace(S, '"', '''') + '"'
  else
    Result := S;
end;

function IsAlpha(C: Char): Boolean;
begin
  Result := (C >= 'A') and (C <= 'Z');
  if Result then Exit;
  Result := (C >= 'a') and (C <= 'z');
end;

function IsUnderscore(C: Char): Boolean;
begin
  Result := C = '_';
end;

function IsNumeric(C: Char): Boolean;
begin
  Result := (C >= '0') and (C <= '9');
end;

function StrIsBlank(const S: string): Boolean;
var
  I: Integer;
begin
  for I := 1 to Length(S) do
    if S[I] > ' ' then
      Exit(False);
  Result := True;
end;

function StrIsIdent(const S: string): Boolean;
var
  AlphaFound: Boolean;
  C: Char;
  I: Integer;
begin
  Result := False;
  if Length(S) < 1 then
    Exit;
  C := S[1];
  AlphaFound := IsAlpha(C);
  if (not AlphaFound) and (not IsUnderscore(C)) then
    Exit;
  for I := 2 to Length(S) do
  begin
    C := S[I];
    AlphaFound := AlphaFound or IsAlpha(C);
    if IsAlpha(C) or IsUnderscore(C) or IsNumeric(C) then
      Continue;
    Exit;
  end;
  Result := AlphaFound;
end;

function StrIsAttr(const S: string): Boolean;
begin
  Result := False;
  if Length(S) < 2 then
    Exit;
  if S[1] <> '@' then
    Exit;
  Result := StrIsIdent(Copy(S, 2, Length(S) - 1));
end;

function StrLineBreakStyle(const S: string): TTextLineBreakStyle;
var
  Count: array[TTextLineBreakStyle] of Integer;
  I: TTextLineBreakStyle;
begin
  for I := Low(Count) to High(Count) do
    Count[I] := StrFindCount(S, LineBreakStyles[I]);
  Result := DefaultTextLineBreakStyle;
  for I := Low(Count) to High(Count) do
    if Count[I] > Count[Result] then
      Result := I;
end;

function StrAdjustLineBreaks(const S: string; Style: TTextLineBreakStyle): string;
var
  Source, Dest: PChar;
  DestLen: Integer;
  I, J, L: Longint;
begin
  Source := Pointer(S);
  L := Length(S);
  DestLen := L;
  I := 1;
  while I <= L do
  begin
    case S[I] of
      #10:
        if Style = tlbsCRLF then Inc(DestLen);
      #13:
        if Style = tlbsCRLF then
          if (I < L) and (S[I + 1] = #10) then
            Inc(I)
          else
            Inc(DestLen)
          else if (I < L) and (S[I + 1] = #10) then
            Dec(DestLen);
    end;
    Inc(I);
  end;
  if DestLen = L then
    Result := S
  else
  begin
    SetLength(Result, DestLen);
    FillChar(Result[1], DestLen, 0);
    Dest := Pointer(Result);
    J := 0;
    I := 0;
    while I < L do
      case Source[I] of
        #10:
          begin
            if Style = tlbsCRLF then
            begin
              Dest[J] := #13;
              Inc(J);
            end;
            Dest[J] := #10;
            Inc(J);
            Inc(I);
          end;
        #13:
          begin
             if Style = tlbsCRLF then
             begin
               Dest[J] := #13;
               Inc(J);
             end;
             Dest[J] := #10;
             Inc(J);
             Inc(I);
             if Source[I] = #10 then
               Inc(I);
          end;
      else
        Dest[J] := Source[I];
        Inc(J);
        Inc(I);
      end;
    end;
end;

function StrAdjustLineBreaks(const S: string): string;
begin
  Result := StrAdjustLineBreaks(S, DefaultTextLineBreakStyle);
end;

function StrToWide(const S: string): WideString;
var
  I: Integer;
begin
  I := Length(S);
  if I < 1 then
    Exit('');
  SetLength(Result, I);
  StringToWideChar(S, PWideChar(Result), I + 1);
end;

function WideToStr(const S: WideString): string;
begin
  if Length(S) < 1 then
    Exit('');
  WideCharToStrVar(PWideChar(S), Result);
end;

function SwitchExists(const Switch: string): Boolean;
begin
  Result := SwitchIndex(Switch) > 0;
end;

function SwitchIndex(const Switch: string): Integer;
var
  S: string;
  I: Integer;
begin
  for I := 1 to ParamCount do
  begin
    S := ParamStr(I);
    if S = SwitchChar + Switch then
      Exit(I)
  end;
  Result := -1;
end;

function SwitchValue(const Switch: string): string;
var
  F: Boolean;
  S: string;
  I: Integer;
begin
  F := False;
  for I := 1 to ParamCount do
  begin
    S := ParamStr(I);
    if F then
      Exit(S);
    if S = SwitchChar + Switch then
      F := True;
  end;
  Result := '';
end;

function IntToStr(Value: Int64): string;
const
  Digits: PChar = '0123456789';
var
  P: PChar;
  I, J: Int64;
begin
  if Value < 0 then
    I := -Value
  else
    I := Value;
  J := 0;
  while I > 0 do
  begin
    I := I div 10;
    Inc(J);
  end;
  if J = 0 then
    Exit('0');
  if Value < 0 then
  begin
    SetLength(Result, J + 1);
    Result[1] := '-'
  end
  else
    SetLength(Result, J);
  P := @Result[Length(Result)];
  if Value < 0 then
    I := -Value
  else
    I := Value;
  while J > 0 do
  begin
    P^ := Digits[I mod 10];
    I := I div 10;
    Dec(P);
    Dec(J);
  end;
end;

function IntToStorage(Bytes: Int64): string;
var
  B, I: Int64;
  D: Double;
begin
  if Bytes <= 0 then
    Exit('0 bytes');
  Result := 'Unknown';
  B := 1024;
  I := 0;
  while Bytes > B do
  begin
    B := B * 1024;
    Inc(I);
    if I > 5 then
      Exit;
  end;
  B := B div 1024;
  D := Round((Bytes * 10) / B) / 10;
  case I of
    0: Result := IntToStr(Bytes) + ' bytes';
    1: Result := FloatToStr(D) + ' KB';
    2: Result := FloatToStr(D) + ' MB';
    3: Result := FloatToStr(D) + ' GB';
    4: Result := FloatToStr(D) + ' TB';
    5: Result := FloatToStr(D) + ' PB';
  end;
end;

function StrToInt(const S: string): Integer;
begin
  Result := SysUtils.StrToInt(S);
end;

function StrToIntDef(const S: string; Default: Integer): Integer;
begin
  Result := SysUtils.StrToIntDef(S, Default);
end;

function FloatToStr(Value: Extended): string;
const
  Epsilon = 0.0001;
var
  E: Extended;
  I: Integer;
begin
  E := Value - Trunc(Value);
  I := 0;
  while E > Epsilon do
  begin
    E := E * 10;
    E := E - Trunc(E);
    Inc(I);
  end;
  Str(Value:0:I, Result);
end;

function FloatToStr(Value: Extended; Decimals: Integer): string;
begin
  Str(Value:0:Decimals, Result);
end;

function FloatToCommas(Value: Extended; Decimals: Integer = 0): string;
begin
  Result := FloatToStrF(Value, ffNumber, 15 , Decimals);
end;

function StrToFloat(const S: string): Extended;
begin
  Result := SysUtils.StrToFloat(S);
end;

function StrToFloatDef(const S: string; Default: Extended): Extended;
begin
  Result := SysUtils.StrToFloatDef(S, Default);
end;

function StrEnvironmentVariable(const Name: string): string;
begin
  Result := GetEnvironmentVariable(Name);
end;

function StrFormat(const S: string; Args: array of const): string;
begin
  Result := Format(S, Args);
end;

function StrCompPath(Component: TComponent): string;
var
  S: string;
begin
  if Component = nil then
    Exit('nil');
  Result := '';
  while Component <> nil do
  begin
    if Component.ClassName = 'TApplication' then Exit;
    S := Component.Name;
    if S = '' then
      S := '(' + Component.ClassName + ')';
    if Result <> '' then
      Result := '.' + Result;
    Result := S + Result;
    Component := Component.Owner;
  end;
end;

{ StringHelper }

function StringHelper.ToString: string;
begin
  Result := Self;
end;

procedure StringHelper.Unique;
begin
  System.UniqueString(Self);
end;

procedure StringHelper.CharInto(C: Char; Len: Integer);
begin
  Self := StrOf(C, Len);
end;

procedure StringHelper.CopyInto(P: Pointer; Len: Integer);
begin
  Self := StrCopyData(P, Len);
end;

procedure StringHelper.InsertInto(const SubStr: string; Position: Integer);
begin
  Self := StrInsert(Self, SubStr, Position);
end;

function StringHelper.Equals(const Value: string; IgnoreCase: Boolean = False): Boolean;
begin
  Result := StrCompare(Self, Value, IgnoreCase) = 0;
end;

function StringHelper.Equals(const Values: array of string; IgnoreCase: Boolean = False): Boolean;
var
  S: string;
begin
  for S in Values do
    if StrCompare(Self, S, IgnoreCase) = 0 then
      Exit(True);
  Result := False;
end;

function StringHelper.Compare(const Value: string; IgnoreCase: Boolean = False): Integer;
begin
  Result := StrCompare(Self, Value, IgnoreCase);
end;

function StringHelper.ToUpper: string;
begin
  Result := StrUpper(Self);
end;

function StringHelper.ToLower: string;
begin
  Result := StrLower(Self);
end;

function StringHelper.Copy(Start: Integer; Len: Integer = 0): string;
begin
  Result := StrCopy(Self, Start, Len);
end;

function StringHelper.Insert(const SubStr: string; Position: Integer): string;
begin
  Result := StrInsert(Self, SubStr, Position);
end;

function StringHelper.IndexOf(const SubStr: string; IgnoreCase: Boolean = False): Integer;
begin
  Result := StrFind(Self, SubStr, IgnoreCase);
end;

function StringHelper.IndexOf(const SubStr: string; Start: Integer; IgnoreCase: Boolean = False): Integer;
begin
  Result := StrFind(Self, SubStr, Start, IgnoreCase);
end;

function StringHelper.MatchCount(const SubStr: string; IgnoreCase: Boolean = False): Integer;
begin
  Result := StrFindCount(Self, SubStr, IgnoreCase);
end;

function StringHelper.Matches(const SubStr: string; IgnoreCase: Boolean = False): IntArray;
begin
  Result := StrFindIndex(Self, SubStr, IgnoreCase);
end;

function StringHelper.Replace(const OldPattern, NewPattern: string; IgnoreCase: Boolean = False): string;
begin
  Result := StrReplace(Self, OldPattern, NewPattern, IgnoreCase);
end;

function StringHelper.ReplaceOne(const OldPattern, NewPattern: string; IgnoreCase: Boolean = False): string;
begin
  Result := StrReplaceOne(Self, OldPattern, NewPattern, IgnoreCase);
end;

function StringHelper.ReplaceAfter(const OldPattern, NewPattern: string; IgnoreCase: Boolean = False): string;
begin
  Result := StrReplaceAfter(Self, OldPattern, NewPattern, IgnoreCase);
end;

function StringHelper.Trim: string;
begin
  Result := StrTrim(Self);
end;

function StringHelper.ArrayIndex(const Values: array of string): Integer;
begin
  Result := StrIndex(Self, Values);
end;

function StringHelper.Lines: StringArray;
var
  S: string;
begin
  S := StrAdjustLineBreaks(Self, DefaultTextLineBreakStyle);
  Result := StrSplit(S, LineBreak);
end;

function StringHelper.LineWith(const SubStr: string; IgnoreCase: Boolean = False): string;
var
  A: StringArray;
  S: string;
begin
  A := Lines;
  for S in A do
    if S.Contains(SubStr, IgnoreCase) then
      Exit(S);
  Result := '';
end;

function StringHelper.Split(Separator: string): StringArray;
begin
  Result := StrSplit(Self, Separator);
end;

function StringHelper.SplitInt(const Separator: string): IntArray;
begin
  Result := StrSplitInt(Self, Separator);
end;

function StringHelper.SplitInt64(const Separator: string): Int64Array;
begin
  Result := StrSplitInt64(Self, Separator);
end;

function StringHelper.Words(MaxColumns: Integer = 0): StringArray;
var
  W: string;
  C, I: Integer;
begin
  if MaxColumns < 1 then
    MaxColumns := High(Integer);
  C := 0;
  for I := 1 to Length do
  begin
    if C >= MaxColumns then
      W := W + Self[I]
    else if Self[I] <= ' ' then
    begin
      if W.Length > 0 then
      begin
        Result.Push(W);
        Inc(C);
      end;
      W := '';
    end
    else
      W := W + Self[I];
  end;
  if W.Length > 0 then
    Result.Push(W)
end;

function StringHelper.FirstOf(const Separator: string): string;
begin
  Result := StrFirstOf(Self, Separator);
end;

function StringHelper.SecondOf(const Separator: string): string;
begin
  Result := StrSecondOf(Self, Separator);
end;

function StringHelper.LastOf(const Separator: string): string;
begin
  Result := StrLastOf(Self, Separator);
end;

function StringHelper.Between(const MarkerA, MarkerB: string): string;
begin
  Result := Self.SecondOf(MarkerA).FirstOf(MarkerB);
end;

function StringHelper.Contains(const SubStr: string; IgnoreCase: Boolean = False): Boolean;
begin
  Result := StrContains(Self, SubStr, IgnoreCase);
end;

function StringHelper.BeginsWith(const SubStr: string; IgnoreCase: Boolean = False): Boolean;
begin
  Result := StrBeginsWith(Self, SubStr, IgnoreCase);
end;

function StringHelper.BeginsWith(const SubStrs: StringArray; IgnoreCase: Boolean = False): Boolean;
var
  S: string;
begin
  Result := False;
  for S in SubStrs do
  begin
    Result := StrBeginsWith(Self, S, IgnoreCase);
    if Result then
      Exit;
  end;
end;

function StringHelper.EndsWith(const SubStr: string; IgnoreCase: Boolean = False): Boolean;
begin
  Result := StrEndsWith(Self, SubStr, IgnoreCase);
end;

function StringHelper.EndsWith(const SubStrs: StringArray; IgnoreCase: Boolean = False): Boolean;
var
  S: string;
begin
  Result := False;
  for S in SubStrs do
  begin
    Result := StrEndsWith(Self, S, IgnoreCase);
    if Result then
      Exit;
  end;
end;

function StringHelper.PadLeft(C: Char; Len: Integer): string;
begin
  Result := StrPadLeft(Self, C, Len);
end;

function StringHelper.PadRight(C: Char; Len: Integer): string;
begin
  Result := StrPadRight(Self, C, Len);
end;

function StringHelper.Quote: string;
begin
  Result := StrQuote(Self);
end;

function StringHelper.Format(Args: array of const): string;
begin
  Result := SysUtils.Format(Self, Args);
end;

function StringHelper.LineBreakStyle: TTextLineBreakStyle;
begin
  Result := StrLineBreakStyle(Self);
end;

function StringHelper.AdjustLineBreaks(Style: TTextLineBreakStyle): string;
begin
  Result := StrAdjustLineBreaks(Self, Style);
end;

function StringHelper.AdjustLineBreaks: string;
begin
  Result := StrAdjustLineBreaks(Self);
end;

function StringHelper.GetIsEmpty: Boolean;
begin
  Result := Length = 0;
end;

function StringHelper.GetIsWhitespace: Boolean;
begin
  Result := StrIsBlank(Self);
end;

function StringHelper.GetIsIdentifier: Boolean;
begin
  Result := StrIsIdent(Self);
end;

function StringHelper.GetIsAttribute: Boolean;
begin
  Result := StrIsAttr(Self);
end;

function StringHelper.GetLength: Integer;
begin
  Result := System.Length(Self);
end;

procedure StringHelper.SetLength(Value: Integer);
begin
  System.SetLength(Self, Value);
end;

{ IntHelper }

function IntHelper.ToString: string;
begin
  Result := IntToStr(Self);
end;

function IntHelper.Between(Low, High: Integer): Boolean;
begin
  Result := (Self >= Low) and (Self <= High);
end;


{ TDateTimeHelper }

function TDateTimeHelper.ToString(Format: string = ''): string;
begin
  if Format = 'GMT' then
    Result := FormatDateTime('ddd, d mmm yyyy hh:nn:ss', Self) + ' GMT'
  else if Format = 'UTC' then
    Result := FormatDateTime('ddd, d mmm yyyy hh:nn:ss', Self) + ' UTC'
  else
    Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', Self);
end;

function TDateTimeHelper.AddMinutes(const A: Integer): TDateTime;
const
  Minute = 1 / (24 * 60);
begin
  Result := Self + A * Minute;
end;

function TDateTimeHelper.Year: Word;
var
  Y, M, D: Word;
begin
  DecodeDate(Self, Y, M, D);
  Result := Y;
end;

function TDateTimeHelper.Month: Word;
var
  Y, M, D: Word;
begin
  DecodeDate(Self, Y, M, D);
  Result := M;
end;

function TDateTimeHelper.Day: Word;
var
  Y, M, D: Word;
begin
  DecodeDate(Self, Y, M, D);
  Result := D;
end;

{ TStringsHelper }

procedure TStringsHelper.AddLine;
begin
  Self.Add('');
end;

procedure TStringsHelper.AddFormat(const S: string; const Args: array of const);
begin
  Self.Add(Format(S, Args));
end;

function TStringsHelper.Contains(const S: string; IgnoreCase: Boolean = False): Boolean;
begin
  Result := StrContains(Text, S, IgnoreCase);
end;

{$endregion}

{$region file management routines}
function FileDelete(const FileName: string): Boolean;
begin
  Result := DeleteFile(FileName);
end;

function FileCopy(const SourceName, DestName: string;
  PreserveTime: Boolean = False): Boolean;
begin
  Result := CopyFile(SourceName, DestName, PreserveTime);
end;

function FileRename(const OldName, NewName: String): Boolean;
begin
  Result := RenameFile(OldName, NewName);
end;

function FileExists(const FileName: string): Boolean;
begin
  Result := SysUtils.FileExists(FileName);
end;

function FileSize(const FileName: string): LargeWord;
begin
  Result := FileUtil.FileSize(FileName);
end;

function FileDate(const FileName: string): TDateTime;
begin
  SysUtils.FileAge(FileName, Result, False);
end;

function FileExtractName(const FileName: string): string;
begin
  Result := StrLastOf(PathAdjustDelimiters(FileName), DirectorySeparator);
end;

function FileExtractExt(const FileName: string): string;
begin
  Result := StrLastOf(PathAdjustDelimiters(FileName), DirectorySeparator);
  if StrFind(Result, '.') > 0 then
    Result := '.' + StrLastOf(Result, '.')
  else
    Result := '';
end;

function FileChangeExt(const FileName, Extension: string): string;
var
  S: string;
begin
  S := FileExtractExt(FileName);
  if S = '' then
    Result := FileName + Extension
  else
    Result := StrCopy(FileName, 1, Length(FileName) - Length(S)) + Extension;
end;

function FileExtractPath(const FileName: string): string;
var
  S: string;
begin
  S := StrLastOf(FileName, DirectorySeparator);
  if S = '' then
    Result := ''
  else
    Result := StrCopy(FileName, 1, Length(FileName) - Length(S) - 1);
end;

procedure FileWriteStr(const FileName: string; const Contents: string);
var
  F: TFileStream;
begin
  F := TFileStream.Create(FileName, fmCreate);
  try
    if Length(Contents) > 0 then
      F.Write(Contents[1], Length(Contents));
  finally
    F.Free;
  end;
end;

function FileReadStr(const FileName: string): string;
const
  BufferSize = 1024 * 10;
  MaxGrow = 1 shl 29;
var
  F: TFileStream;
  BytesRead: Integer;
  BufferLength, BufferDelta: Integer;
  I: Integer;
begin
  Result := '';
  if FileExists(FileName) then
  begin
    F := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      Result := '';
      BufferLength := 0;
      I := 1;
      repeat
        BufferDelta := BufferSize * I;
        SetLength(Result, BufferLength + BufferDelta);
        BytesRead := F.Read(Result[BufferLength + 1], BufferDelta);
        Inc(BufferLength, BufferDelta);
        if I < MaxGrow then
          I := I shl 1;
      until BytesRead <> BufferDelta;
      SetLength(Result, BufferLength - BufferDelta + BytesRead);
    finally
      F.Free;
    end;
  end;
end;

procedure FileWriteLine(const FileName: string; const Line: string);
var
  F: TFileStream;
  S: string;
begin
  if FileExists(FileName) then
    F := TFileStream.Create(FileName, fmOpenWrite)
  else
    F := TFileStream.Create(FileName, fmCreate);
  F.Seek(0, soFromEnd);
  try
    if Length(Line) > 0 then
      F.Write(Line[1], Length(Line));
    S := LineBreakStyles[DefaultTextLineBreakStyle];
    F.Write(S[1], Length(S));
  finally
    F.Free;
  end;
end;

function DirCreate(const Dir: string): Boolean;
begin
  Result := ForceDirectories(Dir);
end;

procedure DirChange(const Dir: string);
begin
  ChDir(Dir);
end;

function DirGetCurrent: string;
begin
  Result := GetCurrentDir;
end;

function DirSetCurrent(const Dir: string): Boolean;
begin
  Result := SetCurrentDir(Dir);
end;

function DirGetTemp(Global: Boolean = False): string;
begin
  Result := SysUtils.GetTempDir(Global);
end;

function DirDelete(const Dir: string; OnlyContents: Boolean = False): Boolean;
begin
  Result := DeleteDirectory(Dir, OnlyContents);
end;

function DirExists(const Dir: string): Boolean;
begin
  Result := DirectoryExists(Dir);
end;

function DirForce(const Dir: string): Boolean;
begin
  Result := ForceDirectories(Dir);
end;

function PathAdjustDelimiters(const Path: string): string;
begin
  {$warnings off}
  if DirectorySeparator = '/' then
    Result := StrReplace(Path, '\', DirectorySeparator)
  else
    Result := StrReplace(Path, '/', DirectorySeparator);
  {$warnings on}
end;

function PathCombine(const A, B: string; IncludeDelimiter: Boolean = False): string;
begin
  if IncludeDelimiter then
    Result := PathIncludeDelimiter(A) + PathIncludeDelimiter(B)
  else
    Result := PathIncludeDelimiter(A) + PathExcludeDelimiter(B);
  Result := PathAdjustDelimiters(Result);
end;

function PathExpand(const Path: string): string;
begin
  Result := ExpandFileName(Path);
end;

function PathIncludeDelimiter(const Path: string): string;
begin
  Result := IncludeTrailingPathDelimiter(Path);
end;

function PathExcludeDelimiter(const Path: string): string;
begin
  Result := ExcludeTrailingPathDelimiter(Path);
end;

function UriCombine(const A, B: string; IncludeDelimiter: Boolean = False): string;
begin
  if (Length(B) > 0) and (B[1] <> '/') then
    Result := UriIncludeDelimiter(A) + B
  else
    Result := A + B;
  if IncludeDelimiter then
    Result := UriIncludeDelimiter(Result);
end;

function UriIncludeDelimiter(const Uri: string): string;
begin
  Result := Uri;
  if Result = '' then
    Exit('/');
  if Result[Length(Result)] <> '/' then
    Result := Result + '/';
end;

function UriExcludeDelimiter(const Uri: string): string;
begin
  Result := Uri;
  if Result = '' then
    Exit;
  if Result[Length(Result)] = '/' then
    SetLength(Result, Length(Result) - 1);
end;

function UriExtractPath(const Uri: string): string;
var
  S: string;
begin
  S := StrLastOf(Uri, '/');
  if S = '' then
    Result := ''
  else
    Result := StrCopy(Uri, 1, Length(Uri) - Length(S) - 1);
end;

function ConfigAppFile(Global: Boolean; CreateDir: Boolean = False): string;
begin
  Result := GetAppConfigFile(Global, CreateDir);
end;

function ConfigAppDir(Global: Boolean; CreateDir: Boolean = False): string;
begin
  Result := GetAppConfigDir(Global);
  if CreateDir and (not DirExists(Result)) then
    DirCreate(Result);
end;

procedure FindFileParams(StartIndex: Integer; out FileParams: TStrings);
var
  Search: TStrings;
  S: string;
  I: Integer;
begin
  FileParams := TStringList.Create;
  if StartIndex < 1 then
    Exit;
  for I := StartIndex to ParamCount do
  begin
    S := ParamStr(I);
    if FileExists(S) then
      FileParams.Add(S)
    else
    begin
      FindFiles(S, Search);
      FileParams.AddStrings(Search);
      Search.Free;
    end;
  end;
end;

function FindOpen(const Path: string; Attr: Longint; out Search: TSearchRec): LongInt;
begin
  Result := FindFirst(PathAdjustDelimiters(Path), Attr, Search);
end;

procedure FindFiles(const Path: string; out FileSearch: TStrings; Attributes: Integer = 0);
var
  Name, Folder: string;
  Search: TSearchRec;
begin
  if DirectoryExists(Path) then
  begin
    Name := '*';
    Folder := Path;
  end
  else
  begin
    Name := FileExtractName(Path);
    Folder := FileExtractPath(Path);
    if Folder = Path then
      Folder := '.'
  end;
  FileSearch := TStringList.Create;
  if FindOpen(PathCombine(Folder, Name), Attributes, Search) = 0 then
  begin
    repeat
      FileSearch.Add(PathCombine(Folder, Search.Name));
    until FindNext(Search) <> 0;
    FindClose(Search);
  end;
end;

procedure FindFiles(const Path: string; out FileSearch: TFileSearch; Attributes: Integer = 0);
var
  Name, Folder: string;
  Search: TSearchRec;
  Item: TFileSearchItem;
begin
  if DirectoryExists(Path) then
  begin
    Name := '*';
    Folder := Path;
  end
  else
  begin
    Name := FileExtractName(Path);
    Folder := FileExtractPath(Path);
    if Folder = Path then
      Folder := '.'
  end;
  FileSearch.Length := 0;
  if FindOpen(PathCombine(Folder, Name), Attributes, Search) = 0 then
  begin
    repeat
      Item.Name := PathCombine(Folder, Search.Name);
      Item.Attributes := Search.Attr;
      Item.Size := Search.Size;
      Item.Modified := FileDate(Item.Name);
      FileSearch.Push(Item);
    until FindNext(Search) <> 0;
    FindClose(Search);
  end;
end;

function FileItemSortName(constref A, B: TFileSearchItem): Integer;
begin
  {$ifdef windows}
  Result := StrCompare(A.Name, B.Name, True);
  {$else}
  Result := StrCompare(A.Name, B.Name);
  {$endif}
end;

procedure TFileSearchHelper.SortName(Order: TSortingOrder = soAscend);
begin
  Self.Sort(Order, FileItemSortName);
end;

function FileItemSortSize(constref A, B: TFileSearchItem): Integer;
begin
  Result := A.Size - B.Size;
end;


procedure TFileSearchHelper.SortSize(Order: TSortingOrder = soAscend);
begin
  Self.Sort(Order, FileItemSortSize);
end;

function FileItemSortModified(constref A, B: TFileSearchItem): Integer;
begin
  if A.Modified < B.Modified then
    Result := 1
  else if A.Modified > B.Modified then
    Result := -1
  else
    Result := 0;
end;

procedure TFileSearchHelper.SortModified(Order: TSortingOrder = soAscend);
begin
  Self.Sort(Order, FileItemSortModified);
end;


{$endregion}

{$region generic containers}
{ TArrayEnumerator<T> }

constructor TArrayEnumerator<T>.Create(Items: TArray<T>; Count: Integer = -1);
begin
  inherited Create;
  FItems := Items;
  FPosition := -1;
  if Count < 0 then
    FCount := Length(Items)
  else
    FCount := Count;
end;

function TArrayEnumerator<T>.GetCurrent: T;
begin
  Result := FItems[FPosition];
end;

function TArrayEnumerator<T>.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FCount;
end;

procedure TArrayEnumerator<T>.Reset;
begin
  FPosition := -1;
end;

{ TArrayList<T> }

function TArrayList<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TArrayListEnumerator.Create(Items);
end;

class operator TArrayList<T>.Implicit(const Value: TArrayList<T>): TArray<T>;
begin
  Result := Value.Items;
end;

class operator TArrayList<T>.Implicit(const Value: TArray<T>): TArrayList<T>;
begin
  Result.Items := Value;
end;

class operator TArrayList<T>.Implicit(const Value: array of T): TArrayList<T>;
var
  I: T;
begin
  for I in Value do
    Result.Push(I);
end;

class function TArrayList<T>.ArrayOf(const Items: array of T): TArrayList<T>;
var
  I: T;
begin
  for I in Items do
    Result.Push(I);
end;

procedure TArrayList<T>.Copy(out List: TArrayList<T>; N: Integer);
var
  I: Integer;
begin
  if N < 1 then
    N := Length
  else if N > Length then
    N := Length;
  List.Length := N;
  if N < 1 then
    Exit;
  for I := 0 to N - 1 do
    List.Items[I] := Items[I];
end;

procedure TArrayList<T>.CopyFast(out List: TArrayList<T>; N: Integer);
begin
  if N < 1 then
    N := Length
  else if N > Length then
    N := Length;
  List.Length := N;
  if N < 1 then
    Exit;
  System.Move(Items[0], List.Items[0], N * SizeOf(T));
end;

procedure TArrayList<T>.Reverse;
var
  Swap: T;
  I, J: Integer;
begin
  I := 0;
  J := Length;
  while I < J do
  begin
    Swap := Items[I];
    Items[I] := Items[J];
    Items[J] := Swap;
    Inc(I);
    Dec(J);
  end;
end;

function TArrayList<T>.Lo: Integer;
begin
  Result := Low(Items);
end;

function TArrayList<T>.Hi: Integer;
begin
  Result := High(Items);
end;

procedure TArrayList<T>.Exchange(A, B: Integer);
var
  Item: T;
begin
  if A <> B then
  begin
    Item := Items[A];
    Items[A] := Items[B];
    Items[B] := Item;
  end;
end;

procedure TArrayList<T>.Push(const Item: T);
var
  I: Integer;
begin
  I := Length;
  Length := I + 1;
  Items[I] := Item;
end;

procedure TArrayList<T>.PushRange(const Collection: array of T);
var
  I, J: Integer;
begin
  I := Length;
  J := High(Collection) - Low(Collection) + 1;
  if J < 1 then
    Exit;
  Length := I + J;
  for J := Low(Collection) to High(Collection) do
  begin
    Items[I] := Collection[J];
    Inc(I);
  end;
end;

function TArrayList<T>.Pop: T;
var
  I: Integer;
begin
  I := Length - 1;
  if I < 0 then
  begin
    Result := Default(T);
    Length := 0;
  end
  else
  begin
    Result := Items[I];
    Length := I;
  end;
end;

function TArrayList<T>.PopRandom: T;
var
  I: Integer;
begin
  I := Length;
  if I < 2 then
    Result := Pop
  else
  begin
    I := System.Random(I);
    Result := Items[I];
    Delete(I);
  end;
end;

function TArrayList<T>.Filter(Func: TFilterFunc<T>): TArrayList<T>;
var
  I, J: Integer;
begin
  J := System.Length(Items);
  System.SetLength(Result.Items, J);
  J := 0;
  for I := 0 to System.Length(Items) - 1 do
    if Func(Items[I]) then
    begin
   Result.Items[J] := Items[I];
   Inc(J);
    end;
  System.SetLength(Result.Items, J);
end;

function TArrayList<T>.FirstOf(Func: TFilterFunc<T>): T;
var
  I: Integer;
begin
  for I := 0 to System.Length(Items) - 1 do
    if Func(Items[I]) then
   Exit(Items[I]);
  Result := Default(T);
end;

procedure TArrayList<T>.Delete(Index: Integer);
var
  I, J: Integer;
begin
  I := Length - 1;
  for J := Index + 1 to I do
    Items[J - 1] := Items[J];
  Length := I;
end;

procedure TArrayList<T>.Clear;
begin
  Length := 0;
end;

{ TMap<K, V> }

function TMap<K, V>.GetItem(const Key: K): V;
var
  I: Integer;
begin
  I := FKeys.IndexOf(Key);
  if I > -1 then
    Result := FValues.Items[I]
  else
    Result := Default(V);
end;

procedure TMap<K, V>.SetItem(const Key: K; const Value: V);
var
  I: Integer;
begin
  I := FKeys.IndexOf(Key);
  if I > -1 then
    FValues.Items[I] := Value
  else
  begin
    FKeys.Push(Key);
    FValues.Push(Value);
  end;
end;

constructor TBaseList.Create(N: Integer = 0);
begin
  inherited Create;
end;

{ TGrowList<T> }

constructor TGrowList<T>.Create(N: Integer = 0);
begin
  inherited Create(N);
  Clear(N);
end;

procedure TGrowList<T>.Pack;
begin
  if FCount < FLength then
  begin
    FLength := FCount;
    FBuffer.Length := FLength;
  end;
end;

function TGrowList<T>.Clone: TObject;
var
  Copy: TGrowList<T>;
begin
  Copy := TBaseListClass(ClassType).Create as TGrowList<T>;
  if FCount = 0 then
    Exit(Copy);
  Copy.FCount := FCount;
  Copy.FLength := FCount;
  FBuffer.CopyFast(Copy.FBuffer, FCount);
  Result := Copy;
end;

procedure TGrowList<T>.Added(N: Integer);
begin
end;

procedure TGrowList<T>.Grow(N: Integer);
const
  MaxGrowSize = 50000;
var
  C: Integer;
begin
  if N < 1 then
    Exit;
  if N < 16 then
    N := 16;
  if N + FCount > FLength then
    if FLength = 0 then
    begin
      FLength := N;
      FBuffer.Length := N;
    end
    else
    begin
      if FLength > MaxGrowSize then
      begin
        if N < MaxGrowSize then
        C := MaxGrowSize
        else
        C := N;
        C := FLength + C;
      end
      else
      begin
        C := FLength + MaxGrowSize;
        C := FLength * 2;
        FLength := FLength + N;
        while C < FLength do
          C := C * 2;
      end;
     FLength := C;
     FBuffer.Length := C;
    end;
end;

procedure TGrowList<T>.Clear(N: Integer = 0);
begin
  FCount := 0;
  if N = 0 then
  begin
    FLength := 0;
    FBuffer.Length := 0
  end
  else if N > FBuffer.Length then
    Grow(N - FBuffer.Length);
  Added(0);
end;

procedure TGrowList<T>.AddRange(const Range: array of T);
var
  I, J: Integer;
begin
  I := Length(Range);
  if I < 1 then
    Exit;
  Grow(I);
  for J := 0 to I - 1 do
    FBuffer.Items[FCount + J] := Range[J];
  Inc(FCount, I);
  Added(I);
end;

procedure TGrowList<T>.AddItem(const Item: T);
begin
  Grow(1);
  FBuffer.Items[FCount] := Item;
  Inc(FCount);
  Added(1);
end;

function TGrowList<T>.GetData(Index: Integer): Pointer;
begin
  Result := @FBuffer.Items[Index];
end;

function TGrowList<T>.GetItem(Index: Integer): T;
begin
  Result := FBuffer.Items[Index];
end;

procedure TGrowList<T>.SetItem(Index: Integer; Value: T);
begin
  FBuffer.Items[Index] := Value;
end;

{ Compare functions }

function DefaultCompare8(constref A, B: Byte): Integer;
begin
  Result := B - A;
end;

function DefaultCompare16(constref A, B: Word): Integer;
begin
  Result := B - A;
end;

function DefaultCompare32(constref A, B: LongWord): Integer;
begin
  Result := B - A;
end;

function DefaultCompare64(constref A, B: LargeWord): Integer;
begin
  Result := B - A;
end;

function TArrayList<T>.CompareExists: Boolean;
begin
  if Assigned(DefaultCompare) then
    Exit(True);
  case SizeOf(T) of
    8: DefaultCompare := TCompareFunc(DefaultCompare8);
    16: DefaultCompare := TCompareFunc(DefaultCompare16);
    32: DefaultCompare := TCompareFunc(DefaultCompare32);
    64: DefaultCompare := TCompareFunc(DefaultCompare64);
  end;
  Result := Assigned(DefaultCompare);
end;

procedure TArrayList<T>.QuickSort(Order: TSortingOrder; Compare: TCompare<T>; L, R: Integer);
var
  F, I, J, P: Integer;
begin
  repeat
    if Order = soDescend then
   F := -1
    else
   F := 1;
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
   while Compare(Items[I], Items[P]) * F < 0 do Inc(I);
   while Compare(Items[J], Items[P]) * F > 0 do Dec(J);
   if I <= J then
   begin
  Exchange(I, J);
  if P = I then
    P := J
  else if P = J then
    P := I;
  Inc(I);
  Dec(J);
   end;
    until I > J;
    if L < J then QuickSort(Order, Compare, L, J);
    L := I;
  until I >= R;
end;

procedure TArrayList<T>.Sort(Order: TSortingOrder = soAscend; Comparer: TCompare<T> = nil);
var
  I: Integer;
begin
  if Order = soNone then
    Exit;
  I := Length;
  if I < 2 then
    Exit;
  if Assigned(Comparer) then
    QuickSort(Order, Comparer, 0, I - 1)
  else if CompareExists then
    QuickSort(Order, DefaultCompare, 0, I - 1);
end;

function TArrayList<T>.IndexOf(const Item: T): Integer;
var
  I: Integer;
begin
  Result := -1;
  I := Length;
  if (I > 0) and CompareExists then
    for I := Lo to Hi do
      if DefaultCompare(Item, Items[I]) = 0 then
  Exit(I);
end;

function TArrayList<T>.IndexOf(const Item: T; Comparer: TCompare<T>): Integer;
var
  I: Integer;
begin
  Result := -1;
  I := Length;
  if I > 0 then
    for I := Lo to Hi do
   if Comparer(Item, Items[I]) = 0 then
  Exit(I);
end;

function TArrayList<T>.Join(const Separator: string; Convert: TConvertString<T> = nil): string;
var
  I: Integer;
begin
  Result := '';
  if Length < 1 then
    Exit;
  if Assigned(Convert) then
  begin
    Result := Convert(First);
    for I := Low(Items) + 1 to High(Items) do
   Result := Result + Separator + Convert(Items[I]);
  end
  else if Assigned(DefaultConvertString) then
  begin
    Result := DefaultConvertString(First);
    for I := Low(Items) + 1 to High(Items) do
   Result := Result + Separator + DefaultConvertString(Items[I]);
  end;
end;

function TArrayList<T>.GetIsEmpty: Boolean;
begin
  Result := Length = 0;
end;

function TArrayList<T>.GetFirst: T;
begin
  if Length > 0 then
    Result := Items[0]
  else
    Result := Default(T);
end;

procedure TArrayList<T>.SetFirst(const Value: T);
begin
  if Length > 0 then
    Items[0] := Value;
end;

function TArrayList<T>.GetLast: T;
begin
  if Length > 0 then
    Result := Items[Length - 1]
  else
    Result := Default(T);
end;

procedure TArrayList<T>.SetLast(const Value: T);
begin
  if Length > 0 then
    Items[Length - 1] := Value;
end;

function TArrayList<T>.GetLength: Integer;
begin
  Result := System.Length(Items);
end;

procedure TArrayList<T>.SetLength(Value: Integer);
begin
  System.SetLength(Items, Value);
end;

function TArrayList<T>.GetData: Pointer;
begin
  Result := @Items[0];
end;

function TArrayList<T>.GetItem(Index: Integer): T;
begin
  Result := Items[Index];
end;

procedure TArrayList<T>.SetItem(Index: Integer; const Value: T);
begin
  Items[Index] := Value;
end;

class function TArrayList<T>.Convert: TArrayList<T>;
begin
  Result.Length := 0;
end;

function DefaultStringCompare(constref A, B: string): Integer;
begin
  Result := StrCompare(A, B);
end;

function DefaultStringConvertString(constref Item: string): string;
begin
  Result := Item;
end;

function DefaultWordCompare(constref A, B: Word): Integer;
begin
  Result := B - A;
end;

function DefaultWordConvertString(constref Item: Word): string;
begin
  Result := IntToStr(Item);
end;

function DefaultIntCompare(constref A, B: Integer): Integer;
begin
  Result := B - A;
end;

function DefaultIntConvertString(constref Item: Integer): string;
begin
  Result := IntToStr(Item);
end;

function DefaultInt64Compare(constref A, B: Int64): Integer;
begin
  Result := B - A;
end;

function DefaultInt64ConvertString(constref Item: Int64): string;
begin
  Result := IntToStr(Item);
end;

function DefaultFloatCompare(constref A, B: Float): Integer;
begin
  if A < B then
    Result := -1
  else if A > B then
    Result := 1
  else
    Result := 0;
end;

function DefaultFloatConvertString(constref Item: Float): string;
begin
  Result := FloatToStr(Item);
end;

function DefaultObjectCompare(constref A, B: TObject): Integer;
begin
  Result := IntPtr(A) - IntPtr(B);
end;

function DefaultInterfaceCompare(constref A, B: IInterface): Integer;
begin
  Result := IntPtr(A) - IntPtr(B);
end;

{$endregion}

{ TNamedValues<T> }

function TNamedValues<T>.GetEnumerator: IEnumerator<string>;
begin
  Result := FNames.GetEnumerator;
end;

procedure TNamedValues<T>.Add(const Name: string; const Value: T);
var
  S: string;
  I: Integer;
begin
  if Name = '' then
    Exit;
  S := StrUpper(Name);
  for I := 0 to FNames.Length - 1 do
    if S = StrUpper(FNames[I]) then
    begin
      FValues[I] := Value;
      Exit;
    end;
  FNames.Push(Name);
  FValues.Push(Value);
end;

procedure TNamedValues<T>.Remove(const Name: string);
var
  S: string;
  I: Integer;
begin
  if Name = '' then
    Exit;
  S := Name.ToUpper;
  for I := FNames.Lo to FNames.Hi - 1 do
    if S = FNames[I].ToUpper then
    begin
      Delete(I);
      Exit;
    end;
end;

procedure TNamedValues<T>.Delete(Index: Integer);
begin
  if (Index > -1) and (Index < FNames.Length) then
  begin
    FNames.Delete(Index);
    FValues.Delete(Index);
  end;
end;

procedure TNamedValues<T>.Clear;
begin
  FNames.Clear;
  FValues.Clear;
end;

function TNamedValues<T>.GetCount: Integer;
begin
  Result := FNames.Length;
end;

function TNamedValues<T>.GetEmpty: Boolean;
begin
  Result := FNames.Length < 1;
end;

function TNamedValues<T>.GetName(Index: Integer): string;
begin
  if (Index > -1) and (Index < FNames.Length) then
    Result := FNames[Index]
  else
    Result := '';
end;

function TNamedValues<T>.GetValue(const Name: string): T;
var
  S: string;
  I: Integer;
begin
  Result := default(T);
  if Name = '' then
    Exit;
  S := Name.ToUpper;
  for I := FNames.Lo to FNames.Hi do
    if S = FNames[I].ToUpper then
    begin
      Result := FValues[I];
      Exit;
    end;
end;

function TNamedValues<T>.GetValueByIndex(Index: Integer): T;
begin
  if (Index > -1) and (Index < FValues.Length) then
    Result := FValues[Index]
  else
    Result := default(T);
end;

{ TNamedValuesIntf<T>.IEnumerable<T> }

function TNamedValuesIntf<T>.GetEnumerator: IEnumerator<string>;
begin
  Result := FData.GetEnumerator;
end;

{ TNamedValuesIntf<T>.INamedValues<T> }

function TNamedValuesIntf<T>.GetCount: Integer;
begin
  Result := FData.GetCount;
end;

function TNamedValuesIntf<T>.GetEmpty: Boolean;
begin
  Result := FData.GetEmpty;
end;

function TNamedValuesIntf<T>.GetName(Index: Integer): string;
begin
  Result := FData.GetName(Index);
end;

function TNamedValuesIntf<T>.GetValue(const Name: string): T;
begin
  Result := FData.GetValue(Name);
end;

function TNamedValuesIntf<T>.GetValueByIndex(Index: Integer): T;
begin
  Result := FData.GetValueByIndex(Index);
end;

procedure TNamedValuesIntf<T>.Add(const Name: string; const Value: T);
begin
  FData.Add(Name, Value);
end;

procedure TNamedValuesIntf<T>.Remove(const Name: string);
begin
  FData.Remove(Name);
end;

procedure TNamedValuesIntf<T>.Delete(Index: Integer);
begin
  FData.Delete(Index);
end;

procedure TNamedValuesIntf<T>.Clear;
begin
  FData.Clear;
end;

function MemCompare(const A, B; Size: LongWord): Boolean;
var
  C, D: PByte;
begin
  C := @A;
  D := @B;
  if (C = nil) or (D = nil) then
    Exit(False);
  while Size > 0 do
  begin
    if C^ <> D^ then
      Exit(False);
    Inc(C);
    Inc(D);
    Dec(Size);
  end;
  Result := True;
end;

{ TDelegateImpl<T> }

function TDelegateImpl<T>.IndexOf(Event: T): Integer;
var
  Item: T;
  I: Integer;
begin
  I := 0;
  for Item in FList do
    if MemCompare(Item, Event, SizeOf(T)) then
      Exit(I)
    else
      Inc(I);
  Result := -1;
end;

{ TDelegateImpl<T>.IDelegate<T> }

function TDelegateImpl<T>.GetIsEmpty: Boolean;
begin
  Result := FList.IsEmpty;
end;

procedure TDelegateImpl<T>.Add(const Event: T);
var
  I: Integer;
begin
  I := IndexOf(Event);
  if I < 0 then
    FList.Push(Event);
end;

procedure TDelegateImpl<T>.Remove(const Event: T);
var
  I: Integer;
begin
  I := IndexOf(Event);
  if I > -1 then
    FList.Delete(I);
end;

{ TDelegateContainerImpl<T>.IDelegateContainer<T> }

function TDelegateContainerImpl<T>.GetDelegate: IDelegate<T>;
begin
  if FDelegate = nil then
  begin
    FDelegate := TDelegateImpl<T>.Create;
    FDelegateClass := FDelegate as TDelegateClass;
  end;
  Result := FDelegate;
end;

function TDelegateContainerImpl<T>.GetEnumerator: IEnumerator<T>;
begin
  GetDelegate;
  Result := FDelegateClass.FList.GetEnumerator;
end;

{ TDelegate<T> }

class operator TDelegate<T>.Implicit(var Delegate: TDelegate<T>): IDelegate<T>;
begin
  Result := Delegate.GetContainer.Delegate;
end;

function TDelegate<T>.GetContainer: IDelegateContainer<T>;
begin
  if FContainer = nil then
    FContainer := TDelegateContainer.Create;
  Result := FContainer;
end;

function TDelegate<T>.GetEnumerator: IEnumerator<T>;
begin
  if FContainer = nil then
    FContainer := TDelegateContainer.Create;
  Result := FContainer.GetEnumerator;
end;

function TDelegate<T>.GetIsEmpty: Boolean;
begin
  Result := GetContainer.Delegate.IsEmpty;
end;

procedure TDelegate<T>.Add(const Handler: T);
begin
  GetContainer.Delegate.Add(Handler);
end;

procedure TDelegate<T>.Remove(const Handler: T);
begin
  GetContainer.Delegate.Remove(Handler);
end;

{ TChangeNotifier }

function TChangeNotifier.GetOnChange: INotifyDelegate;
begin
  Result := FOnChange;
end;

procedure TChangeNotifier.Change;
var
  Event: TNotifyEvent;
begin
  for Event in FOnChange do
    Event(Self);
end;

{$endregion}

{$region classes}
{ TNullInfo }

function TNullInfo.Reset: TNullResult;
begin
  FCount := 0;
  InterLockedExchange(FBytes, 0);
  InterLockedExchange(FRate, 0);
  InterLockedExchange(FSeconds, 0);
  InterLockedExchange(FAvergage, 0);
  FTime := 0;
  FRateTime := 0;
  FRateBytes := 0;
  FAvergageTotal := 0;
  FAvergageCount := 0;
  Result := FResult.Items;
  FResult.Clear;
end;

{ TNullStream }

constructor TNullStream.Create;
begin
  inherited Create;
  FReadInfo := TNullInfo.Create;
  FWriteInfo := TNullInfo.Create;
end;

destructor TNullStream.Destroy;
begin
  FReadInfo.Free;
  FWriteInfo.Free;
  inherited Destroy;
end;

procedure TNullStream.SetSize(NewSize: Longint);
begin
  { Do nothing }
end;

procedure TNullStream.SetSize(const NewSize: Int64);
begin
  { Do nothing }
end;

procedure TNullStream.RecordInfo(Info: TNullInfo; Count: LongWord);
const
  Poll = 1 / 10;
  TwoPoll = Poll * 2;
var
  Time: Double;
  Compliment, Section: LongWord;
begin
  Time := TimeQuery;
  if Info.FTime = 0 then
  begin
    Info.FTime := Time;
    Info.FCount := Count;
    InterLockedExchange(Info.FBytes, Info.FCount);
  end
  else if Time - Info.FTime < 1 then
  begin
    Info.FCount += Count;
    InterLockedExchange(Info.FBytes, Info.FBytes + Info.FCount);
  end
  else if Time - Info.FTime < 2 then
  begin
    Info.FResult.Push(Info.FCount);
    Info.FCount := Count;
    InterLockedExchange(Info.FBytes, Info.FBytes + Info.FCount);
    InterLockedIncrement(Info.FSeconds);
    Info.FTime += 1;
  end
  else
  begin
    Info.Reset;
    Info.FTime := Time;
    Info.FCount := Count;
    InterLockedExchange(Info.FBytes, Info.FCount);
  end;
  if Info.FRateTime = 0 then
    Info.FRateTime := Time;
  Time := Time - Info.FRateTime;
  if Time <= Poll then
    Info.FRateBytes += Count
  else if Time < TwoPoll then
  begin
    Compliment := Round(Count * ((Time - Poll) / Time));
    Info.FRateBytes += Count - Compliment;
    Section := Round(Info.FRateBytes / Poll);
    InterLockedExchange(Info.FRate, Section);
    Info.FRateBytes := Compliment;
    Info.FRateTime += Poll;
    Info.FAvergageTotal += Section;
    Inc(Info.FAvergageCount);
    Section := Round(Info.FAvergageTotal / Info.FAvergageCount);
    InterLockedExchange(Info.FAvergage, Section);
  end
  else
  begin
    Info.FAvergageTotal += Info.FRateBytes + Count;
    while Time > Poll do
    begin
      Time -= Poll;
      Inc(Info.FAvergageCount);
      Info.FRateTime += Poll;
    end;
    Section := Round(Info.FAvergageTotal / Info.FAvergageCount);
    InterLockedExchange(Info.FAvergage, Section);
    InterLockedExchange(Info.FRate, 0);
    Info.FRateBytes := 0;
    Info.FRateTime := 0;
  end;
end;

function TNullStream.Read(var Buffer; Count: Longint): Longint;
begin
  if Count > 0 then
    RecordInfo(FReadInfo, Abs(Count));
  Result := Count;
end;

function TNullStream.Write(const Buffer; Count: Longint): Longint;
begin
  if Count > 0 then
    RecordInfo(FWriteInfo, Abs(Count));
  Result := Count;
end;

function TNullStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := 0;
end;
{$endregion}

{$region threading}
var
  SemaphoreInit: TCriticalSectionHandler;
  SemaphoreDestroy: TCriticalSectionHandler;
  SemaphoreWait: TCriticalSectionHandler;
  SemaphoreLeave: TCriticalSectionHandler;

procedure ThreadsInit;
var
  M: TThreadManager;
begin
  GetThreadManager(M{%H-});
  SemaphoreInit := @M.InitCriticalSection;
  SemaphoreDestroy := @M.DoneCriticalSection;
  SemaphoreWait := @M.EnterCriticalSection;
  SemaphoreLeave := @M.LeaveCriticalSection;
end;

{ TODO: Remove TMuxtex and use RTL critical sections only }

{ TMutexObject }

type
  TMutexObject = class(TInterfacedObject, IMutex)
  private
    FSemaphore: Pointer;
  public
    constructor Create;
    destructor Destroy; override;
    function Lock: LongInt;
    function Unlock: LongInt;
  end;

{ TEventObject }

  TEventObject = class(TInterfacedObject, IEvent)
  private
    FEvent: Pointer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Reset;
    procedure Signal;
    procedure Wait;
  end;

constructor TMutexObject.Create;
begin
  inherited Create;
  if @SemaphoreInit = nil then
    ThreadsInit;
  SemaphoreInit(FSemaphore);
end;

destructor TMutexObject.Destroy;
begin
  SemaphoreDestroy(FSemaphore);
  inherited Destroy;
end;

function TMutexObject.Lock: LongInt;
begin
  SemaphoreWait(FSemaphore);
  Result := 0;
end;

function TMutexObject.Unlock: LongInt;
begin
  SemaphoreLeave(FSemaphore);
  Result := 0;
end;

constructor TEventObject.Create;
begin
  inherited Create;
  FEvent := RTLEventCreate;
end;

destructor TEventObject.Destroy;
begin
  RTLEventDestroy(FEvent);
  inherited Destroy;
end;

procedure TEventObject.Reset;
begin
  RTLEventResetEvent(FEvent);
end;

procedure TEventObject.Signal;
begin
  RTLEventSetEvent(FEvent);
end;

procedure TEventObject.Wait;
begin
  RTLEventWaitFor(FEvent);
end;

function MutexCreate: IMutex;
begin
  Result := TMutexObject.Create;;
end;

function EventCreate: IEvent;
begin
  Result := TEventObject.Create;
end;

{ TSimpleThread }

constructor TSimpleThread.Create(ExecuteMethod: TThreadExecuteMethod;
  OnStatus: TNotifyEvent = nil; OnTerminate: TNotifyEvent = nil);
begin
  FExecuteMethod := ExecuteMethod;
  FOnStatus := OnStatus;
  Self.OnTerminate := OnTerminate;
  inherited Create(False);
end;

procedure TSimpleThread.Execute;
begin
  FreeOnTerminate := True;
  FExecuteMethod(Self);
end;

procedure TSimpleThread.Synchronize(Method: TThreadMethod);
begin
  inherited Synchronize(Method);
end;

procedure TSimpleThread.DoStatus;
begin
  FStatus := FTempStatus;
  if Assigned(FOnStatus) then
    FOnStatus(Self);
end;

procedure TSimpleThread.SetStatus(const Value: string);
begin
  if Handle = GetCurrentThreadId then
  begin
    FTempStatus := Value;
    if Assigned(FOnStatus) then
      Synchronize(DoStatus);
  end;
end;

function ProcessExecute(const Command: string; const Arguments: string = ''): string;
const
  BUFFER_SIZE = 2048;
var
  Process: TProcess;
  Stream: TStringStream;
  BytesRead: LongInt;
  Buffer: array[0..BUFFER_SIZE - 1] of Byte;
begin
  Process := TProcess.Create(nil);
  Stream := TStringStream.Create;
  try
    Process.Executable := Command;
    if Arguments <> '' then
      Process.Parameters.Text := Arguments;
    Process.Options := [poUsePipes];
    Process.Execute;
    repeat
      BytesRead := Process.Output.Read(Buffer{%H-}, BUFFER_SIZE);
      Stream.Write(Buffer, BytesRead)
    until BytesRead = 0;
    Process.Terminate(0);
    Process.WaitOnExit;
    Result := Stream.DataString;
  finally
    Stream.Free;
    Process.Free;
  end;
end;

{$endregion}

{$region waiting routines}
procedure PumpMessages;
begin
  if Assigned(PumpMessagesProc) then
    PumpMessagesProc;
end;
{$endregion}

initialization
  @LibraryExceptproc := @LibraryExcept;
  StringArray.DefaultCompare := DefaultStringCompare;
  StringArray.DefaultConvertString := DefaultStringConvertString;
  WordArray.DefaultCompare := DefaultWordCompare;
  WordArray.DefaultConvertString := DefaultWordConvertString;
  IntArray.DefaultCompare := DefaultIntCompare;
  IntArray.DefaultConvertString := DefaultIntConvertString;
  Int64Array.DefaultCompare := DefaultInt64Compare;
  Int64Array.DefaultConvertString := DefaultInt64ConvertString;
  FloatArray.DefaultCompare := DefaultFloatCompare;
  FloatArray.DefaultConvertString := DefaultFloatConvertString;
end.

