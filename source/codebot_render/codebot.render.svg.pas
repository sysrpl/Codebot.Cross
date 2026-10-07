unit Codebot.Render.SVG;

{$i render.inc}
{$warn 6060 off : case statement does not handle all possible cases}

interface

uses
  SysUtils,
  Codebot.System,
  Codebot.Collections,
  Codebot.Graphics.Types,
  Codebot.Text.Xml,
  Codebot.Render.Graphics;

{ Forward declarations }

type
  TSvgNode = class;
  TSvgShape = class;
  TSvgLine = class;
  TSvgPolyline = class;
  TSvgPolygon = class;
  TSvgCircle = class;
  TSvgEllipse = class;
  TSvgRect = class;
  TSvgPath = class;
  TSvgCollection = class;
  TSvgGroup = class;
  TSvgDocument = class;

  { TSvgColor is a color packed into 32 bits }
  TSvgColor = LongWord;

  { TSvgCap is how the ends of stroked lines are drawn }
  TSvgCap = (svgCapRound, svgCapButt, svgCapSquare);
  { TSvgJoin is how stroked lines are joined at corners }
  TSvgJoin = (svgJoinRound, svgJoinBevel, svgJoinMiter);
  { TSvgFillRule decides which areas of a shape with overlapping parts are
    filled }
  TSvgFillRule = (svgFillNonZero, svgFillEvenOdd);

  { TSvgStyle is the fill and stroke of a node }
  TSvgStyle = record
  public
    Fill: TSvgColor;
    FillOpacity: Float;
    Stroke: TSvgColor;
    StrokeOpacity: Float;
    StrokeWidth: Float;
    StrokeCap: TSvgCap;
    StrokeJoin: TSvgJoin;
    StrokeMiter: Float;
    FillRule: TSvgFillRule;
  end;

  { TSvgStyleEx is the base class for reading custom style properties into a
    node. Descend from it and pass the class when parsing a document. }
  TSvgStyleEx = class
  protected
    procedure Copy(StyleEx: TSvgStyleEx); virtual; abstract;
    procedure Read(const Name, Value: string); virtual; abstract;
  public
    constructor Create; virtual;
  end;

  { The class of a custom style }
  TSvgStyleExClass = class of TSvgStyleEx;

{ TSvgTransform holds the six values of a 2D transform }

  TSvgTransform = array[0..5] of Float;

  { A node keeps the attributes and style properties the parser does not
    know, so that a program can give the shapes of a document properties of
    its own. A custom property can be written as an attribute of an element,
    inside its style attribute, or in a rule of the style sheet for its
    class or id. Attribute reads one by name. }

  TSvgNode = class
  private
    FCustomNames: array of string;
    FCustomValues: array of string;
    procedure DefaultStyle;
    procedure FindStyle(Ident: string);
    procedure SetCustom(const Name, Value: string);
    function GetAttributeCount: Integer;
    function GetAttributeName(Index: Integer): string;
    function GetAttributeValue(Index: Integer): string;
  protected
    function ApplyProperty(const Name, Value: string): Boolean;
    procedure ApplyStyle(S: string); virtual;
    procedure Parse(N: INode); virtual;
  public
    Doc: TSvgDocument;
    Parent: TSvgNode;
    Id: string;
    ClassId: string;
    Transform: string;
    Opacity: Float;
    Removed: Boolean;
    Style: TSvgStyle;
    StyleEx: TSvgStyleEx;
    constructor Create(ParentNode: TSvgNode); virtual;
    destructor Destroy; override;
    { Build the complete transform of the node }
    procedure BuildTransform(out T: TSvgTransform);
    { Return True if the node has a custom attribute or style property. With
      Inherit the nodes which contain this one are looked in as well. }
    function HasAttribute(const Name: string; Inherit: Boolean = True): Boolean;
    { Read a custom attribute or style property, or return Default if there
      is none. With Inherit a node without one takes it from the nearest
      node which contains it and has one. }
    function Attribute(const Name: string; const Default: string = '';
      Inherit: Boolean = True): string;
    { Read a custom attribute as a number }
    function AttributeFloat(const Name: string; Default: Float = 0;
      Inherit: Boolean = True): Float;
    { The custom attributes and style properties of this node alone }
    property AttributeCount: Integer read GetAttributeCount;
    property AttributeNames[Index: Integer]: string read GetAttributeName;
    property AttributeValues[Index: Integer]: string read GetAttributeValue;
  end;
  { The class of a node }
  TSvgNodeClass = class of TSvgNode;

  { A list of nodes }
  TSvgNodes = TList<TSvgNode>;
  { An enumerator of nodes }
  TSvgEnumerator = IEnumerator<TSvgNode>;

{ TSvgShape is the base class of the nodes which draw a shape }

  TSvgShape = class(TSvgNode)
  end;

{ TSvgLine is a line between two points }

  TSvgLine = class(TSvgShape)
  protected
    procedure Parse(N: INode); override;
  public
    X1, Y1, X2, Y2: Float;
  end;

{ A list of points }

  TSvgPoints = TArrayList<TPointF>;

  { TSvgPolyline is a line through a list of points }
  TSvgPolyline = class(TSvgShape)
  protected
    procedure Parse(N: INode); override;
  public
    Points: TSvgPoints;
  end;

{ TSvgPolygon is a closed shape through a list of points }

  TSvgPolygon = class(TSvgPolyline)
  end;

{ TSvgCircle is a circle }

  TSvgCircle = class(TSvgShape)
  protected
    procedure Parse(N: INode); override;
  public
    X, Y, R: Float;
  end;

{ TSvgEllipse is an ellipse }

  TSvgEllipse = class(TSvgShape)
  protected
    procedure Parse(N: INode); override;
  public
    X, Y, W, H: Float;
  end;

{ TSvgRect is a rectangle, which can have rounded corners }

  TSvgRect = class(TSvgShape)
  protected
    procedure Parse(N: INode); override;
  public
    X, Y, W, H, R: Float;
  end;

  { TSvgPathAction is the kind of step a path command takes }
  TSvgPathAction = (svgMove, svgLine, svgHLine, svgVLine, svgCubic, svgQuadratic, svgClose);

  { TSvgCommand is one step of a path }
  TSvgCommand = record
    Action: TSvgPathAction;
    Compressed: Boolean;
    X, Y, X1, Y1, X2, Y2: Float;
  end;

  { A list of path commands }
  TSvgCommands = TArrayList<TSvgCommand>;

{ TSvgPath is a shape made of lines and curves }

  TSvgPath = class(TSvgShape)
  protected
    procedure Parse(N: INode); override;
  public
    Commands: TSvgCommands;
  end;

{ TSvgCollection is a node which contains other nodes }

  TSvgCollection = class(TSvgNode)
  private
    FNodes: TSvgNodes;
    procedure Cleanup;
    function GetCount: Integer;
    function GetNode(Index: Integer): TSvgNode;
  public
    { Enumerates the nodes in the collection }
    function GetEnumerator: TSvgEnumerator;
  public
    constructor Create(ParentNode: TSvgNode); override;
    destructor Destroy; override;
    { The number of nodes in the collection }
    property Count: Integer read GetCount;
    { The nodes in the collection by index }
    property Node[Index: Integer]: TSvgNode read GetNode; default;
  end;

{ TSvgGroup is a group of nodes which share a transform and a style }

  TSvgGroup = class(TSvgCollection)
  end;

{ TSvgDocument is an SVG document read from text or from a file }

  TSvgDocument = class(TSvgCollection)
  private
    FViewBox: TRectF;
    FStyleSection: string;
    FStyleExClass: TSvgStyleExClass;
  public
    { Read the document from SVG text }
    procedure ParseText(const Xml: string; StyleExClass: TSvgStyleExClass = nil);
    { Read the document from an SVG file }
    procedure ParseFile(const FileName: string; StyleExClass: TSvgStyleExClass = nil);
    property NodeCount: Integer read GetCount;
    property Node[Index: Integer]: TSvgNode read GetNode;
    { The view box of the document }
    property ViewBox: TRectF read FViewBox;
  end;

{ Create an empty SVG document }
function NewSvgDocument: TSvgDocument;

{ TSvgRenderOptions are the parts of a document which DrawAt draws }

type
  TSvgRenderOptions = set of (renderColor, renderOutlines, renderNodes);

  { TSvgRender reads an SVG document and draws it on a canvas }
  TSvgRender = class
  private
    FCanvas: ICanvas;
    FDoc: TSvgDocument;
    FPen: IPen;
    FDocumentSize: Integer;
    FNodeCount: Integer;
    function GetViewBox: TRectF;
  public
    constructor Create(Canvas: ICanvas);
    destructor Destroy; override;
    { Read the document from SVG text }
    procedure Parse(const Xml: string);
    { Read the document from an SVG file }
    procedure ParseFile(const FileName: string);
    { Draw the document in color }
    procedure Draw;
    { Draw the outlines of the shapes }
    procedure DrawOutline(PenWidth: Float);
    { Draw the nodes of the shapes }
    procedure DrawNodes(PenWidth: Float);
    { Draw the document at a point with a scale and an angle }
    procedure DrawAt(X, Y, Scale, Angle, Size: Float; Options: TSvgRenderOptions = [renderColor]; PenWidth: Float = 1);
    property DocumentSize: Integer read FDocumentSize;
    property NodeCount: Integer read FNodeCount;
    { The view box of the document }
    property ViewBox: TRectF read GetViewBox;
  end;

implementation

const
  ColorNames: array of string = [
    'black', 'white', 'aliceblue', 'antiquewhite', 'aqua',
    'aquamarine', 'azure', 'beige', 'bisque', 'blanchedalmond',
    'blue', 'blueviolet', 'brown', 'burlywood', 'cadetblue',
    'chartreuse', 'chocolate', 'coral', 'cornflowerblue', 'cornsilk',
    'crimson', 'cyan', 'darkblue', 'darkcyan', 'darkgoldenrod',
    'darkgray', 'darkgrey', 'darkgreen', 'darkkhaki', 'darkmagenta',
    'darkolivegreen', 'darkorange', 'darkorchid', 'darkred', 'darksalmon',
    'darkseagreen', 'darkslateblue', 'darkslategray', 'darkslategrey', 'darkturquoise',
    'darkviolet', 'deeppink', 'deepskyblue', 'dimgray', 'dimgrey',
    'dodgerblue', 'firebrick', 'floralwhite', 'forestgreen', 'fuchsia',
    'gainsboro', 'ghostwhite', 'gold', 'goldenrod', 'gray',
    'grey', 'green', 'greenyellow', 'honeydew', 'hotpink',
    'indianred', 'indigo', 'ivory', 'khaki', 'lavender',
    'lavenderblush', 'lawngreen', 'lemonchiffon', 'lightblue', 'lightcoral',
    'lightcyan', 'lightgoldenrodyellow', 'lightgray', 'lightgrey', 'lightgreen',
    'lightpink', 'lightsalmon', 'lightseagreen', 'lightskyblue', 'lightslategray',
    'lightslategrey', 'lightsteelblue', 'lightyellow', 'lime', 'limegreen',
    'linen', 'magenta', 'maroon', 'mediumaquamarine', 'mediumblue',
    'mediumorchid', 'mediumpurple', 'mediumseagreen', 'mediumslateblue', 'mediumspringgreen',
    'mediumturquoise', 'mediumvioletred', 'midnightblue', 'mintcream', 'mistyrose',
    'moccasin', 'navajowhite', 'navy', 'oldlace', 'olive',
    'olivedrab', 'orange', 'orangered', 'orchid', 'palegoldenrod',
    'palegreen', 'paleturquoise', 'palevioletred', 'papayawhip', 'peachpuff',
    'peru', 'pink', 'plum', 'powderblue', 'purple',
    'rebeccapurple', 'red', 'rosybrown', 'royalblue', 'saddlebrown',
    'salmon', 'sandybrown', 'seagreen', 'seashell', 'sienna',
    'silver', 'skyblue', 'slateblue', 'slategray', 'slategrey',
    'snow', 'springgreen', 'steelblue', 'tan', 'teal',
    'thistle', 'tomato', 'turquoise', 'violet', 'wheat',
    'whitesmoke', 'yellow', 'yellowgreen'];

  ColorValues: array of TSvgColor = [
    $FF000000, $FFFFFFFF, $FFF0F8FF, $FFFAEBD7, $FF00FFFF,
    $FF7FFFD4, $FFF0FFFF, $FFF5F5DC, $FFFFE4C4, $FFFFEBCD,
    $FF0000FF, $FF8A2BE2, $FFA52A2A, $FFDEB887, $FF5F9EA0,
    $FF7FFF00, $FFD2691E, $FFFF7F50, $FF6495ED, $FFFFF8DC,
    $FFDC143C, $FF00FFFF, $FF00008B, $FF008B8B, $FFB8860B,
    $FFA9A9A9, $FFA9A9A9, $FF006400, $FFBDB76B, $FF8B008B,
    $FF556B2F, $FFFF8C00, $FF9932CC, $FF8B0000, $FFE9967A,
    $FF8FBC8F, $FF483D8B, $FF2F4F4F, $FF2F4F4F, $FF00CED1,
    $FF9400D3, $FFFF1493, $FF00BFFF, $FF696969, $FF696969,
    $FF1E90FF, $FFB22222, $FFFFFAF0, $FF228B22, $FFFF00FF,
    $FFDCDCDC, $FFF8F8FF, $FFFFD700, $FFDAA520, $FF808080,
    $FF808080, $FF008000, $FFADFF2F, $FFF0FFF0, $FFFF69B4,
    $FFCD5C5C, $FF4B0082, $FFFFFFF0, $FFF0E68C, $FFE6E6FA,
    $FFFFF0F5, $FF7CFC00, $FFFFFACD, $FFADD8E6, $FFF08080,
    $FFE0FFFF, $FFFAFAD2, $FFD3D3D3, $FFD3D3D3, $FF90EE90,
    $FFFFB6C1, $FFFFA07A, $FF20B2AA, $FF87CEFA, $FF778899,
    $FF778899, $FFB0C4DE, $FFFFFFE0, $FF00FF00, $FF32CD32,
    $FFFAF0E6, $FFFF00FF, $FF800000, $FF66CDAA, $FF0000CD,
    $FFBA55D3, $FF9370DB, $FF3CB371, $FF7B68EE, $FF00FA9A,
    $FF48D1CC, $FFC71585, $FF191970, $FFF5FFFA, $FFFFE4E1,
    $FFFFE4B5, $FFFFDEAD, $FF000080, $FFFDF5E6, $FF808000,
    $FF6B8E23, $FFFFA500, $FFFF4500, $FFDA70D6, $FFEEE8AA,
    $FF98FB98, $FFAFEEEE, $FFDB7093, $FFFFEFD5, $FFFFDAB9,
    $FFCD853F, $FFFFC0CB, $FFDDA0DD, $FFB0E0E6, $FF800080,
    $FF663399, $FFFF0000, $FFBC8F8F, $FF4169E1, $FF8B4513,
    $FFFA8072, $FFF4A460, $FF2E8B57, $FFFFF5EE, $FFA0522D,
    $FFC0C0C0, $FF87CEEB, $FF6A5ACD, $FF708090, $FF708090,
    $FFFFFAFA, $FF00FF7F, $FF4682B4, $FFD2B48C, $FF008080,
    $FFD8BFD8, $FFFF6347, $FF40E0D0, $FFEE82EE, $FFF5DEB3,
    $FFF5F5F5, $FFFFFF00, $FF9ACD32];

{ NumberEnd returns the end of the number starting at P, or P itself if there
  is no number there. A number may have a sign, a fraction and an exponent,
  so '1.5.5' scans as '1.5' and '1e5', '+5' and '.5' are all numbers. }

function IsAlpha(C: Char): Boolean;
begin
  Result := C in ['A'..'Z', 'a'..'z'];
end;

function NumberEnd(P: PChar): PChar;
var
  Digits: Boolean;
begin
  Result := P;
  if P^ in ['+', '-'] then
    Inc(P);
  Digits := False;
  while P^ in ['0'..'9'] do
  begin
    Inc(P);
    Digits := True;
  end;
  if P^ = '.' then
  begin
    Inc(P);
    while P^ in ['0'..'9'] do
    begin
      Inc(P);
      Digits := True;
    end;
  end;
  if not Digits then
    Exit;
  if (P^ in ['e', 'E']) and ((P[1] in ['0'..'9']) or
    ((P[1] in ['+', '-']) and (P[2] in ['0'..'9']))) then
  begin
    Inc(P);
    if P^ in ['+', '-'] then
      Inc(P);
    while P^ in ['0'..'9'] do
      Inc(P);
  end;
  Result := P;
end;

{ StrToNumber converts the number at the start of S, ignoring any unit that
  follows it such as 'px'. It always uses '.' as the decimal separator and
  returns Default instead of raising an exception when there is no number. }

function StrToNumber(const S: string; Default: Float = 0): Float;
const
  MaxExponent = 38;
var
  P, E: PChar;
  Value, Fraction, Scale: Double;
  Negative, NegativeExponent: Boolean;
  Exponent, I: Integer;
begin
  Result := Default;
  if S = '' then
    Exit;
  P := PChar(S);
  while (P^ > #0) and (P^ <= ' ') do
    Inc(P);
  E := NumberEnd(P);
  if E = P then
    Exit;
  Negative := P^ = '-';
  if P^ in ['+', '-'] then
    Inc(P);
  Value := 0;
  while P^ in ['0'..'9'] do
  begin
    Value := Value * 10 + Ord(P^) - Ord('0');
    Inc(P);
  end;
  if P^ = '.' then
  begin
    Inc(P);
    Fraction := 0.1;
    while P^ in ['0'..'9'] do
    begin
      Value := Value + (Ord(P^) - Ord('0')) * Fraction;
      Fraction := Fraction / 10;
      Inc(P);
    end;
  end;
  if P < E then
  begin
    { Skip the 'e' and read the exponent }
    Inc(P);
    NegativeExponent := P^ = '-';
    if P^ in ['+', '-'] then
      Inc(P);
    Exponent := 0;
    while (P < E) and (Exponent <= MaxExponent) do
    begin
      Exponent := Exponent * 10 + Ord(P^) - Ord('0');
      Inc(P);
    end;
    if Exponent > MaxExponent then
      Exponent := MaxExponent;
    Scale := 1;
    for I := 1 to Exponent do
      Scale := Scale * 10;
    if NegativeExponent then
      Value := Value / Scale
    else
      Value := Value * Scale;
  end;
  if Negative then
    Value := -Value;
  Result := Value;
end;

function HexDigit(C: Char): Integer;
begin
  case C of
    '0'..'9': Result := Ord(C) - Ord('0');
    'a'..'f': Result := Ord(C) - Ord('a') + 10;
    'A'..'F': Result := Ord(C) - Ord('A') + 10;
  else
    Result := -1;
  end;
end;

function MakeColor(R, G, B, A: Integer): TSvgColor;
begin
  Result := (LongWord(A) shl 24) or (LongWord(R) shl 16) or (LongWord(G) shl 8) or LongWord(B);
end;

{ StrToColor accepts color names, #rgb, #rgba, #rrggbb, #rrggbbaa and the
  rgb() and rgba() functions. Their channels may be numbers, decimals or
  percentages, separated by commas or spaces with an optional '/' before the
  alpha. Values it cannot read return 0, which means no color. }

function StrToColor(S: string): TSvgColor;

  function Channel(const S: string): Integer;
  var
    V: Float;
  begin
    V := StrToNumber(S);
    if S.EndsWith('%') then
      V := V * $FF / 100;
    Result := Round(V);
    if Result < 0 then
      Result := 0
    else if Result > $FF then
      Result := $FF;
  end;

  function Alpha(const S: string): Integer;
  var
    V: Float;
  begin
    V := StrToNumber(S, 1);
    if S.EndsWith('%') then
      V := V / 100;
    Result := Round(V * $FF);
    if Result < 0 then
      Result := 0
    else if Result > $FF then
      Result := $FF;
  end;

var
  Items, Values: StringArray;
  Digits: array[0..7] of Integer;
  A: Integer;
  I: Integer;
begin
  S := S.Trim.ToLower;
  if S.Length < 1 then
    Exit(0);
  if S = 'none' then
    Exit(0);
  for I := Low(ColorNames) to High(ColorNames) do
    if S = ColorNames[I] then
      Exit(ColorValues[I]);
  if S.BeginsWith('rgb(') or S.BeginsWith('rgba(') then
  begin
    S := S.SecondOf('(').FirstOf(')').Replace(',', ' ').Replace('/', ' ');
    Items := S.Split(' ');
    for I := 0 to Items.Length - 1 do
      if Items[I] <> '' then
        Values.Push(Items[I]);
    if Values.Length < 3 then
      Exit(0);
    A := $FF;
    if Values.Length > 3 then
      A := Alpha(Values[3]);
    Exit(MakeColor(Channel(Values[0]), Channel(Values[1]), Channel(Values[2]), A));
  end;
  if S[1] <> '#' then
    Exit(0);
  S := System.Copy(S, 2, S.Length - 1);
  if (S.Length <> 3) and (S.Length <> 4) and (S.Length <> 6) and (S.Length <> 8) then
    Exit(0);
  for I := 1 to S.Length do
  begin
    Digits[I - 1] := HexDigit(S[I]);
    if Digits[I - 1] < 0 then
      Exit(0);
  end;
  case S.Length of
    3: Result := MakeColor(Digits[0] * $11, Digits[1] * $11, Digits[2] * $11, $FF);
    4: Result := MakeColor(Digits[0] * $11, Digits[1] * $11, Digits[2] * $11, Digits[3] * $11);
    6: Result := MakeColor(Digits[0] * 16 + Digits[1], Digits[2] * 16 + Digits[3],
      Digits[4] * 16 + Digits[5], $FF);
  else
    Result := MakeColor(Digits[0] * 16 + Digits[1], Digits[2] * 16 + Digits[3],
      Digits[4] * 16 + Digits[5], Digits[6] * 16 + Digits[7]);
  end;
end;

{ TSvgStyleEx }

constructor TSvgStyleEx.Create;
begin
  inherited Create;
end;

{ TSvgNode }

constructor TSvgNode.Create(ParentNode: TSvgNode);
var
  N: TSvgNode;
begin
  inherited Create;
  N := ParentNode;
  if N <> nil then
  begin
    while N.Parent <> nil do
      N := N.Parent;
    if N is TSvgDocument then
    begin
      Doc := N as TSvgDocument;
      if Doc.FStyleExClass <> nil then
        StyleEx := Doc.FStyleExClass.Create;
    end;
  end;
  Parent := ParentNode;
  if Parent = nil then
    DefaultStyle
  else
  begin
    Style := Parent.Style;
    if (StyleEx <> nil) and (Parent.StyleEx <> nil) then
      StyleEx.Copy(Parent.StyleEx);
  end;
  Opacity := 1;
end;

procedure TSvgNode.DefaultStyle;
begin
  Style.Fill := $FF000000;
  Style.FillOpacity := 1;
  Style.Stroke := 0;
  Style.StrokeWidth := 1;
  Style.StrokeCap := svgCapButt;
  Style.StrokeJoin := svgJoinMiter;
  Style.StrokeMiter := 4;
  Style.StrokeOpacity := 1;
  Style.FillRule := svgFillNonZero;
end;

destructor TSvgNode.Destroy;
begin
  if StyleEx <> nil then
    StyleEx.Free;
  inherited Destroy;
end;

procedure TSvgNode.FindStyle(Ident: string);
var
  S: string;
begin
  S := Doc.FStyleSection;
  if S.IndexOf(Ident) < 1 then
    Exit;
  S := S.SecondOf(Ident);
  if S = '' then
    Exit;
  S := S.SecondOf('{');
  if S = '' then
    Exit;
  S := S.FirstOf('}');
  ApplyStyle(S);
end;

{ ApplyProperty sets one style property, given either as a presentation
  attribute or inside a style. It returns False for names it does not know. }

function TSvgNode.ApplyProperty(const Name, Value: string): Boolean;
var
  R: string;
begin
  Result := True;
  R := Value.Trim;
  if Name = 'fill' then
    Style.Fill := StrToColor(R)
  else if Name = 'fill-opacity' then
    Style.FillOpacity := StrToNumber(R)
  else if Name = 'stroke' then
    Style.Stroke := StrToColor(R)
  else if Name = 'stroke-opacity' then
    Style.StrokeOpacity := StrToNumber(R)
  else if Name = 'stroke-width' then
    Style.StrokeWidth := StrToNumber(R)
  else if Name = 'stroke-linecap' then
  begin
    if R = 'round' then
      Style.StrokeCap := svgCapRound
    else if R = 'butt' then
      Style.StrokeCap := svgCapButt
    else
      Style.StrokeCap := svgCapSquare;
  end
  else if Name = 'stroke-linejoin' then
  begin
    if R = 'round' then
      Style.StrokeJoin := svgJoinRound
    else if R = 'miter' then
      Style.StrokeJoin := svgJoinMiter
    else
      Style.StrokeJoin := svgJoinBevel;
  end
  else if Name = 'fill-rule' then
  begin
    if R = 'evenodd' then
      Style.FillRule := svgFillEvenOdd
    else
      Style.FillRule := svgFillNonZero;
  end
  else if Name = 'stroke-miterlimit' then
    Style.StrokeMiter := StrToNumber(R)
  else if Name = 'opacity' then
    Opacity := StrToNumber(R)
  else if Name = 'display' then
    Removed := R = 'none'
  else if Name = 'transform' then
    Transform := R
  else
    Result := False;
end;

procedure TSvgNode.ApplyStyle(S: string);
var
  Items: StringArray;
  L, R: string;
begin
  Items := S.Split(';');
  for S in Items do
  begin
    L := S.FirstOf(':').Trim;
    R := S.SecondOf(':').Trim;
    if L = '' then
      Continue;
    if not ApplyProperty(L, R) then
    begin
      SetCustom(L, R);
      if StyleEx <> nil then
        StyleEx.Read(L, R);
    end;
  end;
end;

{ A later value for a name takes the place of an earlier one, which gives
  custom properties the same priority as styles }

procedure TSvgNode.SetCustom(const Name, Value: string);
var
  I: Integer;
begin
  for I := 0 to Length(FCustomNames) - 1 do
    if FCustomNames[I] = Name then
    begin
      FCustomValues[I] := Value;
      Exit;
    end;
  I := Length(FCustomNames);
  SetLength(FCustomNames, I + 1);
  SetLength(FCustomValues, I + 1);
  FCustomNames[I] := Name;
  FCustomValues[I] := Value;
end;

function TSvgNode.GetAttributeCount: Integer;
begin
  Result := Length(FCustomNames);
end;

function TSvgNode.GetAttributeName(Index: Integer): string;
begin
  Result := FCustomNames[Index];
end;

function TSvgNode.GetAttributeValue(Index: Integer): string;
begin
  Result := FCustomValues[Index];
end;

function TSvgNode.HasAttribute(const Name: string; Inherit: Boolean = True): Boolean;
var
  N: TSvgNode;
  I: Integer;
begin
  N := Self;
  while N <> nil do
  begin
    for I := 0 to Length(N.FCustomNames) - 1 do
      if N.FCustomNames[I] = Name then
        Exit(True);
    if not Inherit then
      Break;
    N := N.Parent;
  end;
  Result := False;
end;

function TSvgNode.Attribute(const Name: string; const Default: string = '';
  Inherit: Boolean = True): string;
var
  N: TSvgNode;
  I: Integer;
begin
  N := Self;
  while N <> nil do
  begin
    for I := 0 to Length(N.FCustomNames) - 1 do
      if N.FCustomNames[I] = Name then
        Exit(N.FCustomValues[I]);
    if not Inherit then
      Break;
    N := N.Parent;
  end;
  Result := Default;
end;

{ A number is read as the parser reads its own, with a decimal point
  whatever the locale of the program }

function TSvgNode.AttributeFloat(const Name: string; Default: Float = 0;
  Inherit: Boolean = True): Float;
var
  S: string;
begin
  S := Attribute(Name, '', Inherit).Trim;
  if S = '' then
    Result := Default
  else
    Result := StrToNumber(S, Default);
end;

procedure TSvgNode.Parse(N: INode);

  { The attributes which name, place or shape a node are read by the parser
    and are not custom attributes, nor are the namespaces of the document }
  function IsReserved(const Name: string): Boolean;
  const
    Names: array[0..22] of string = ('id', 'class', 'style', 'transform',
      'd', 'points', 'x', 'y', 'width', 'height', 'cx', 'cy', 'r', 'rx', 'ry',
      'x1', 'y1', 'x2', 'y2', 'viewBox', 'version', 'xmlns',
      'preserveAspectRatio');
  var
    I: Integer;
  begin
    for I := Low(Names) to High(Names) do
      if Name = Names[I] then
        Exit(True);
    Result := Name.BeginsWith('xmlns:') or Name.BeginsWith('xml:');
  end;

var
  F: IFiler;
  S: string;
  A: INodeList;
  C: INode;
  I: Integer;
begin
  F := N.Filer;
  { Styles are applied from lowest to highest priority: presentation
    attributes, then class rules, then id rules, then the style attribute }
  A := N.Attributes;
  for I := 0 to A.Count - 1 do
  begin
    C := A.ByIndex[I];
    { Attributes the parser does not know are kept as custom attributes }
    if (C.Text.Trim = '') or (not ApplyProperty(C.Name, C.Text)) then
      if not IsReserved(C.Name) then
        SetCustom(C.Name, C.Text.Trim);
    if StyleEx <> nil then
      StyleEx.Read(C.Name, C.Text);
  end;
  ClassId := F.ReadStr('@class').Trim;
  if ClassId <> '' then
    FindStyle('.' + ClassId);
  Id := F.ReadStr('@id').Trim;
  if Id <> '' then
    FindStyle('#' + Id);
  S := F.ReadStr('@style');
  if S <> '' then
    ApplyStyle(S);
end;

procedure TSvgNode.BuildTransform(out T: TSvgTransform);
var
  Items: StringArray;
  S: string;
  I: Integer;
begin
  T[0] := 1; T[1] := 0; T[2] := 0;
  T[3] := 1; T[4] := 0; T[5] := 0;
  if Transform.BeginsWith('matrix(') then
  begin
    S := Transform.Replace('matrix(', '').Replace(')', '').Replace(',', ' ');
    Items := S.Split(' ');
    I := 0;
    for S in Items do
    begin
      if S = '' then
        Continue;
      T[I] := StrToNumber(S);
      Inc(I);
      if I = 6 then
        Break;
    end;
  end
  else if Transform.BeginsWith('translate(') then
  begin
    S := Transform.Replace('translate(', '').Replace(')', '').Replace(',', ' ');
    Items := S.Split(' ');
    I := 0;
    for S in Items do
    begin
      if S = '' then
        Continue;
      T[4 + I] := StrToNumber(S);
      Inc(I);
      if I = 2 then
        Break;
    end;
  end
  else if Transform.BeginsWith('scale(') then
  begin
    S := Transform.Replace('scale(', '').Replace(')', '').Replace(',', ' ');
    Items := S.Split(' ');
    I := 0;
    for S in Items do
    begin
      if S = '' then
        Continue;
      T[I * 3] := StrToNumber(S);
      Inc(I);
      if I = 2 then
        Break;
    end;
    { A single scale value applies to both axes }
    if I = 1 then
      T[3] := T[0];
  end;
end;

{ TSvgLine }

procedure TSvgLine.Parse(N: INode);
var
  F: IFiler;
begin
  inherited Parse(N);
  F := N.Filer;
  X1 := StrToNumber(F.ReadStr('@x1'));
  Y1 := StrToNumber(F.ReadStr('@y1'));
  X2 := StrToNumber(F.ReadStr('@x2'));
  Y2 := StrToNumber(F.ReadStr('@y2'));
end;

{ TSvgPolyline }

{ Points are numbers separated by commas, spaces or nothing at all when a
  sign starts the next number, as in '10,20 30,40', '10 20 30 40' or
  '10-20-30-40'. A trailing odd number is ignored. }

procedure TSvgPolyline.Parse(N: INode);
var
  S: string;
  Seek, E: PChar;
  Values: array[0..1] of Float;
  Count: Integer;
  P: TPointF;
begin
  inherited Parse(N);
  S := N.Filer.ReadStr('@points');
  if S = '' then
    Exit;
  Seek := PChar(S);
  Count := 0;
  while True do
  begin
    while (Seek^ > #0) and ((Seek^ <= ' ') or (Seek^ = ',')) do
      Inc(Seek);
    E := NumberEnd(Seek);
    if E = Seek then
      Break;
    Values[Count] := StrToNumber(System.Copy(S, Seek - PChar(S) + 1, E - Seek));
    Seek := E;
    Inc(Count);
    if Count = 2 then
    begin
      P.X := Values[0];
      P.Y := Values[1];
      Points.Push(P);
      Count := 0;
    end;
  end;
end;

{ TSvgCircle }

procedure TSvgCircle.Parse(N: INode);
var
  F: IFiler;
begin
  inherited Parse(N);
  F := N.Filer;
  X := StrToNumber(F.ReadStr('@cx'));
  Y := StrToNumber(F.ReadStr('@cy'));
  R := StrToNumber(F.ReadStr('@r'));
end;

{ TSvgEllipse }

procedure TSvgEllipse.Parse(N: INode);
var
  F: IFiler;
begin
  inherited Parse(N);
  F := N.Filer;
  W := StrToNumber(F.ReadStr('@rx'));
  H := StrToNumber(F.ReadStr('@ry'));
  X := StrToNumber(F.ReadStr('@cx')) - W;
  Y := StrToNumber(F.ReadStr('@cy')) - H;
  W := W * 2;
  H := H * 2;
end;

{ TSvgRect }

procedure TSvgRect.Parse(N: INode);
var
  F: IFiler;
begin
  inherited Parse(N);
  F := N.Filer;
  X := StrToNumber(F.ReadStr('@x'));
  Y := StrToNumber(F.ReadStr('@y'));
  W := StrToNumber(F.ReadStr('@width'));
  H := StrToNumber(F.ReadStr('@height'));
  R := StrToNumber(F.ReadStr('@rx'));
  if R = 0 then
    R := StrToNumber(F.ReadStr('@ry'));
end;

type
  TSvgPathData = record
  private
    Data: string;
    Seek: PChar;
  public
    Item: string;
    procedure Init(const D: string);
    function Next: Boolean;
    function NextFlag: Boolean;
  end;

procedure TSvgPathData.Init(const D: string);
begin
  Data := D;
  Seek := nil;
  if Data <> '' then
    Seek := PChar(Data);
end;

function TSvgPathData.Next: Boolean;
var
  E: PChar;
begin
  Result := False;
  Item := '';
  if Seek = nil then
    Exit;
  while (Seek[0] > #0) and ((Seek[0] <= ' ') or (Seek[0] = ',')) do
    Inc(Seek);
  if Seek[0] = #0 then
    Exit;
  E := NumberEnd(Seek);
  if E = Seek then
  begin
    Item := Seek[0];
    Inc(Seek);
  end
  else
  begin
    SetString(Item, Seek, E - Seek);
    Seek := E;
  end;
  Result := True;
end;

{ Arc flags are a single 0 or 1 and may be written without separators, as in
  'a10 10 0 0110 10' where the flags are 0 and 1 followed by the number 10 }

function TSvgPathData.NextFlag: Boolean;
begin
  Result := False;
  Item := '';
  if Seek = nil then
    Exit;
  while (Seek[0] > #0) and ((Seek[0] <= ' ') or (Seek[0] = ',')) do
    Inc(Seek);
  if Seek[0] in ['0', '1'] then
  begin
    Item := Seek[0];
    Inc(Seek);
    Result := True;
  end
  else
    Result := Next;
end;

function ArcTan2(Y, X: Float): Float;
begin
  if X > 0 then
    Result := ArcTan(Y / X)
  else if X < 0 then
  begin
    if Y >= 0 then
      Result := ArcTan(Y / X) + Pi
    else
      Result := ArcTan(Y / X) - Pi;
  end
  else if Y > 0 then
    Result := Pi / 2
  else if Y < 0 then
    Result := -Pi / 2
  else
    Result := 0;
end;

{ AddArc converts an SVG elliptical arc from X1, Y1 to X2, Y2 into cubic
  bezier commands, each covering at most a quarter turn. The center of the
  ellipse is found as described in the SVG implementation notes, F.6.5. }

procedure AddArc(var Commands: TSvgCommands; X1, Y1, RX, RY, Angle: Float;
  LargeArc, Sweep: Boolean; X2, Y2: Float);
var
  C: TSvgCommand;
  CosPhi, SinPhi, DX, DY, X1P, Y1P, L, Num, Den, K: Float;
  CXP, CYP, CX, CY, UX, UY, VX, VY, Theta, Delta, Step, Kappa: Float;
  A1, A2, Cos1, Sin1, Cos2, Sin2: Float;
  Segments, I: Integer;

  procedure Map(U, V: Float; out X, Y: Float);
  begin
    X := CX + CosPhi * RX * U - SinPhi * RY * V;
    Y := CY + SinPhi * RX * U + CosPhi * RY * V;
  end;

begin
  C := Default(TSvgCommand);
  { An arc to the current point draws nothing }
  if (X1 = X2) and (Y1 = Y2) then
    Exit;
  RX := Abs(RX);
  RY := Abs(RY);
  { An arc with a zero radius is a straight line }
  if (RX = 0) or (RY = 0) then
  begin
    C.Action := svgLine;
    C.X := X2;
    C.Y := Y2;
    Commands.Push(C);
    Exit;
  end;
  CosPhi := Cos(Angle * Pi / 180);
  SinPhi := Sin(Angle * Pi / 180);
  DX := (X1 - X2) / 2;
  DY := (Y1 - Y2) / 2;
  X1P := CosPhi * DX + SinPhi * DY;
  Y1P := -SinPhi * DX + CosPhi * DY;
  { Radii too small to reach the end point are scaled up }
  L := Sqr(X1P) / Sqr(RX) + Sqr(Y1P) / Sqr(RY);
  if L > 1 then
  begin
    L := Sqrt(L);
    RX := RX * L;
    RY := RY * L;
  end;
  Num := Sqr(RX) * Sqr(RY) - Sqr(RX) * Sqr(Y1P) - Sqr(RY) * Sqr(X1P);
  Den := Sqr(RX) * Sqr(Y1P) + Sqr(RY) * Sqr(X1P);
  if (Num <= 0) or (Den = 0) then
    K := 0
  else
    K := Sqrt(Num / Den);
  if LargeArc = Sweep then
    K := -K;
  CXP := K * RX * Y1P / RY;
  CYP := -K * RY * X1P / RX;
  CX := CosPhi * CXP - SinPhi * CYP + (X1 + X2) / 2;
  CY := SinPhi * CXP + CosPhi * CYP + (Y1 + Y2) / 2;
  UX := (X1P - CXP) / RX;
  UY := (Y1P - CYP) / RY;
  VX := (-X1P - CXP) / RX;
  VY := (-Y1P - CYP) / RY;
  Theta := ArcTan2(UY, UX);
  Delta := ArcTan2(UX * VY - UY * VX, UX * VX + UY * VY);
  if Sweep and (Delta < 0) then
    Delta := Delta + 2 * Pi
  else if (not Sweep) and (Delta > 0) then
    Delta := Delta - 2 * Pi;
  Segments := Trunc(Abs(Delta) / (Pi / 2) - 0.001) + 1;
  Step := Delta / Segments;
  Kappa := 4 / 3 * Sin(Step / 4) / Cos(Step / 4);
  C.Action := svgCubic;
  A1 := Theta;
  for I := 1 to Segments do
  begin
    A2 := A1 + Step;
    Cos1 := Cos(A1); Sin1 := Sin(A1);
    Cos2 := Cos(A2); Sin2 := Sin(A2);
    Map(Cos1 - Kappa * Sin1, Sin1 + Kappa * Cos1, C.X1, C.Y1);
    Map(Cos2 + Kappa * Sin2, Sin2 - Kappa * Cos2, C.X2, C.Y2);
    if I = Segments then
    begin
      C.X := X2;
      C.Y := Y2;
    end
    else
      Map(Cos2, Sin2, C.X, C.Y);
    Commands.Push(C);
    A1 := A2;
  end;
end;

procedure TSvgPath.Parse(N: INode);
var
  F: IFiler;
  Data: TSvgPathData;
  Command: TSvgCommand;
  Relative: Boolean;
  WasCurve: Boolean;
  Arc: Boolean;
  P, Start: TPointF;

  procedure ReadArc(Advance: Boolean);
  var
    RX, RY, Angle, X, Y: Float;
    LargeArc, Sweep: Boolean;
  begin
    if Advance then
      Data.Next;
    RX := StrToNumber(Data.Item);
    Data.Next;
    RY := StrToNumber(Data.Item);
    Data.Next;
    Angle := StrToNumber(Data.Item);
    Data.NextFlag;
    LargeArc := Data.Item = '1';
    Data.NextFlag;
    Sweep := Data.Item = '1';
    Data.Next;
    X := StrToNumber(Data.Item);
    Data.Next;
    Y := StrToNumber(Data.Item);
    if Relative then
    begin
      X := X + Command.X;
      Y := Y + Command.Y;
    end;
    AddArc(Commands, Command.X, Command.Y, RX, RY, Angle, LargeArc, Sweep, X, Y);
    { An arc is not a curve that a following S or T can reflect }
    Command.Action := svgLine;
    Command.X := X;
    Command.Y := Y;
  end;

begin
  inherited Parse(N);
  F := N.Filer;
  Data.Init(F.ReadStr('@d').Trim);
  Command := Default(TSvgCommand);
  Start.X := 0;
  Start.Y := 0;
  Relative := False;
  Arc := False;
  while Data.Next do
  begin
    P.X := 0;
    P.Y := 0;
    if IsAlpha(Data.Item[1]) then
      Arc := Data.Item[1] in ['A', 'a'];
    if Arc then
    begin
      { Numbers after an arc are more arcs }
      if IsAlpha(Data.Item[1]) then
      begin
        Relative := Data.Item[1] = 'a';
        ReadArc(True);
      end
      else
        ReadArc(False);
      Continue;
    end;
    if Data.Item[1] in ['M', 'm'] then
    begin
      Command.Action := svgMove;
      Relative := Data.Item[1] = 'm';
      if Relative then
      begin
        P.X := Command.X;
        P.Y := Command.Y;
      end;
      Data.Next;
      Command.X := P.X + StrToNumber(Data.Item);
      Data.Next;
      Command.Y := P.Y + StrToNumber(Data.Item);
      Start.X := Command.X;
      Start.Y := Command.Y;
      Commands.Push(Command);
      Command.Action := svgLine;
      Continue;
    end;
    if Data.Item[1] in ['L', 'l'] then
    begin
      Command.Action := svgLine;
      Relative := Data.Item[1] = 'l';
      if Relative then
      begin
        P.X := Command.X;
        P.Y := Command.Y;
      end;
      Data.Next;
      Command.X := P.X + StrToNumber(Data.Item);
      Data.Next;
      Command.Y := P.Y + StrToNumber(Data.Item);
      Commands.Push(Command);
      Continue;
    end;
    if Data.Item[1] in ['H', 'h'] then
    begin
      Command.Action := svgHLine;
      Relative := Data.Item[1] = 'h';
      if Relative then
        P.X := Command.X;
      Data.Next;
      Command.X := P.X + StrToNumber(Data.Item);
      Commands.Push(Command);
      Continue;
    end;
    if Data.Item[1] in ['V', 'v'] then
    begin
      Command.Action := svgVLine;
      Relative := Data.Item[1] = 'v';
      if Relative then
        P.Y := Command.Y;
      Data.Next;
      Command.Y := P.Y + StrToNumber(Data.Item);
      Commands.Push(Command);
      Continue;
    end;
    if Data.Item[1] in ['C','c'] then
    begin
      Command.Action := svgCubic;
      Command.Compressed := False;
      Relative := Data.Item[1] = 'c';
      if Relative then
      begin
        P.X := Command.X;
        P.Y := Command.Y;
      end;
      Data.Next;
      Command.X1 := P.X + StrToNumber(Data.Item);
      Data.Next;
      Command.Y1 := P.Y + StrToNumber(Data.Item);
      Data.Next;
      Command.X2 := P.X + StrToNumber(Data.Item);
      Data.Next;
      Command.Y2 := P.Y + StrToNumber(Data.Item);
      Data.Next;
      Command.X := P.X + StrToNumber(Data.Item);
      Data.Next;
      Command.Y := P.Y + StrToNumber(Data.Item);
      Commands.Push(Command);
      Continue;
    end;
    if Data.Item[1] in ['S','s'] then
    begin
      WasCurve := Command.Action = svgCubic;
      Command.Action := svgCubic;
      Command.Compressed := True;
      Relative := Data.Item[1] = 's';
      if Relative then
      begin
        P.X := Command.X;
        P.Y := Command.Y;
      end;
      if WasCurve then
      begin
        Command.X1 := Command.X * 2 - Command.X2;
        Command.Y1 := Command.Y * 2 - Command.Y2;
      end
      else
      begin
        Command.X1 := Command.X;
        Command.Y1 := Command.Y;
      end;
      Data.Next;
      Command.X2 := P.X + StrToNumber(Data.Item);
      Data.Next;
      Command.Y2 := P.Y + StrToNumber(Data.Item);
      Data.Next;
      Command.X := P.X + StrToNumber(Data.Item);
      Data.Next;
      Command.Y := P.Y + StrToNumber(Data.Item);
      Commands.Push(Command);
      Continue;
    end;
    if Data.Item[1] in ['Q','q'] then
    begin
      Command.Action := svgQuadratic;
      Command.Compressed := False;
      Relative := Data.Item[1] = 'q';
      if Relative then
      begin
        P.X := Command.X;
        P.Y := Command.Y;
      end;
      Data.Next;
      Command.X1 := P.X + StrToNumber(Data.Item);
      Data.Next;
      Command.Y1 := P.Y + StrToNumber(Data.Item);
      Data.Next;
      Command.X := P.X + StrToNumber(Data.Item);
      Data.Next;
      Command.Y := P.Y + StrToNumber(Data.Item);
      Commands.Push(Command);
      Continue;
    end;
    if Data.Item[1] in ['T','t'] then
    begin
      WasCurve := Command.Action = svgQuadratic;
      Command.Action := svgQuadratic;
      Command.Compressed := True;
      Relative := Data.Item[1] = 't';
      if Relative then
      begin
        P.X := Command.X;
        P.Y := Command.Y;
      end;
      if WasCurve then
      begin
        Command.X1 := Command.X * 2 - Command.X1;
        Command.Y1 := Command.Y * 2 - Command.Y1;
      end
      else
      begin
        { Without a previous quadratic the control point is the current point }
        Command.X1 := Command.X;
        Command.Y1 := Command.Y;
      end;
      Data.Next;
      Command.X := P.X +StrToNumber(Data.Item);
      Data.Next;
      Command.Y := P.Y + StrToNumber(Data.Item);
      Commands.Push(Command);
      Continue;
    end;
    if Data.Item[1] in ['Z','z'] then
    begin
      Command.Action := svgClose;
      { Closing a path returns the current point to the start of the sub-path }
      Command.X := Start.X;
      Command.Y := Start.Y;
      Commands.Push(Command);
      Continue;
    end;
    { Abort if unsupported commands are encountered }
    if IsAlpha(Data.Item[1]) then
      Break;
    case Command.Action of
      svgLine:
        begin
          if Relative then
          begin
            P.X := Command.X;
            P.Y := Command.Y;
          end;
          Command.X := P.X + StrToNumber(Data.Item);
          Data.Next;
          Command.Y := P.Y + StrToNumber(Data.Item);
          Commands.Push(Command);
        end;
      svgHLine:
        begin
          if Relative then
            P.X := Command.X;
          Command.X := P.X + StrToNumber(Data.Item);
          Commands.Push(Command);
        end;
      svgVLine:
        begin
          if Relative then
            P.Y := Command.Y;
          Command.Y := P.Y + StrToNumber(Data.Item);
          Commands.Push(Command);
        end;
      svgCubic:
        if Command.Compressed then
        begin
          if Relative then
          begin
            P.X := Command.X;
            P.Y := Command.Y;
          end;
          Command.X1 := Command.X * 2 - Command.X2;
          Command.Y1 := Command.Y * 2 - Command.Y2;
          Command.X2 := P.X + StrToNumber(Data.Item);
          Data.Next;
          Command.Y2 := P.Y + StrToNumber(Data.Item);
          Data.Next;
          Command.X := P.X + StrToNumber(Data.Item);
          Data.Next;
          Command.Y := P.Y + StrToNumber(Data.Item);
          Commands.Push(Command);
        end
        else
        begin
          if Relative then
          begin
            P.X := Command.X;
            P.Y := Command.Y;
          end;
          Command.X1 := P.X + StrToNumber(Data.Item);
          Data.Next;
          Command.Y1 := P.Y + StrToNumber(Data.Item);
          Data.Next;
          Command.X2 := P.X + StrToNumber(Data.Item);
          Data.Next;
          Command.Y2 := P.Y + StrToNumber(Data.Item);
          Data.Next;
          Command.X := P.X + StrToNumber(Data.Item);
          Data.Next;
          Command.Y := P.Y + StrToNumber(Data.Item);
          Commands.Push(Command);
        end;
      svgQuadratic:
        if Command.Compressed then
        begin
          if Relative then
          begin
            P.X := Command.X;
            P.Y := Command.Y;
          end;
          Command.X1 := Command.X * 2 - Command.X1;
          Command.Y1 := Command.Y * 2 - Command.Y1;
          Command.X := P.X +StrToNumber(Data.Item);
          Data.Next;
          Command.Y := P.Y + StrToNumber(Data.Item);
          Commands.Push(Command);
        end
        else
        begin
          if Relative then
          begin
            P.X := Command.X;
            P.Y := Command.Y;
          end;
          Command.X1 := P.X + StrToNumber(Data.Item);
          Data.Next;
          Command.Y1 := P.Y + StrToNumber(Data.Item);
          Data.Next;
          Command.X := P.X + StrToNumber(Data.Item);
          Data.Next;
          Command.Y := P.Y + StrToNumber(Data.Item);
          Commands.Push(Command);
        end;
    else
      Break;
    end;
  end;
end;

{ TSvgCollection }

constructor TSvgCollection.Create(ParentNode: TSvgNode);
begin
  inherited Create(ParentNode);
  FNodes := TSvgNodes.Create;
end;

destructor TSvgCollection.Destroy;
begin
  Cleanup;
  FNodes.Free;
  inherited Destroy;
end;

function TSvgCollection.GetEnumerator: TSvgEnumerator;
begin
  Result := FNodes.GetEnumerator;
end;

procedure TSvgCollection.Cleanup;
var
  I: Integer;
begin
  for I := 0 to FNodes.Count - 1 do
    FNodes[I].Free;
  FNodes.Clear;
end;

function TSvgCollection.GetNode(Index: Integer): TSvgNode;
begin
  Result := FNodes[Index];
end;

function TSvgCollection.GetCount: Integer;
begin
  Result := FNodes.Count;
end;

{ TSvgDocument }

procedure TSvgDocument.ParseText(const Xml: string; StyleExClass: TSvgStyleExClass = nil);
var
  D: IDocument;

  procedure Add(Parent: TSvgNode; NodeClass: TSvgNodeClass; Nodes: TSvgNodes; N: INode);
  var
    S: TSvgNode;
  begin
    S := NodeClass.Create(Parent);
    S.Parse(N);
    Nodes.Add(S);
  end;

  procedure AddNodes(Parent: TSvgNode; Nodes: TSvgNodes; Items: INodeList);
  var
    Group: TSvgGroup;
    N: INode;
    S: string;
    I: Integer;
  begin
    for I := 0 to Items.Count - 1 do
    begin
      N := Items[I];
      S := N.Name;
      if S = 'line' then
        Add(Parent, TSvgLine, Nodes, N)
      else if S = 'polyline' then
        Add(Parent, TSvgPolyline, Nodes, N)
      else if S = 'polygon' then
        Add(Parent, TSvgPolygon, Nodes, N)
      else if S = 'circle' then
        Add(Parent, TSvgCircle, Nodes, N)
      else if S = 'ellipse' then
        Add(Parent, TSvgEllipse, Nodes, N)
      else if S = 'rect' then
        Add(Parent, TSvgRect, Nodes, N)
      else if S = 'path' then
        Add(Parent, TSvgPath, Nodes, N)
      else if S = 'g' then
      begin
        Group := TSvgGroup.Create(Parent);
        Group.Parse(N);
        Nodes.Add(Group);
        AddNodes(Group, Group.FNodes, N.Nodes);
      end;
    end;
  end;

  procedure GenerateErrorImage;
  var
    R: TSvgRect;
    L: TSvgLine;
  begin
    R := TSvgRect.Create(Self);
    R.X := 0; R.Y := 0;
    R.W := 100; R.H := 100; R.R := 10;
    R.Style.Fill := $FF900000;
    FNodes.Add(R);
    L := TSvgLine.Create(Self);
    L.X1 := 25; L.X2 := 75;
    L.Y1 := 50; L.Y2 := 50;
    L.Style.Fill := 0;
    L.Style.Stroke := $FFC0C0C0;
    L.Style.StrokeWidth := 20;
    L.Style.StrokeCap := svgCapRound;
    FNodes.Add(L);
    FViewBox.X := 0; FViewBox.Y := 0;
    FViewBox.Width := 100; FViewBox.Height := 100;
  end;

  procedure FindViewBox;
  var
    Items: StringArray;
    F: IFiler;
    S: string;
    I: Integer;
  begin
    FViewBox.X := 0; FViewBox.Y := 0;
    FViewBox.Width := 0; FViewBox.Height := 0;
    F := D.Root.Filer;
    S := F.ReadStr('@viewBox');
    { Without a viewBox the document size comes from its width and height }
    if S = '' then
    begin
      FViewBox.Width := StrToNumber(F.ReadStr('@width'));
      FViewBox.Height := StrToNumber(F.ReadStr('@height'));
      Exit;
    end;
    Items := S.Replace(',', ' ').Split(' ');
    I := 0;
    for S in Items do
    begin
      if S = '' then
        Continue;
      case I of
        0: FViewBox.X := StrToNumber(S);
        1: FViewBox.Y := StrToNumber(S);
        2: FViewBox.Width := StrToNumber(S);
        3: FViewBox.Height := StrToNumber(S);
      end;
      Inc(I);
      if I = 4 then
        Break;
    end;
  end;

  procedure FindStyle;
  var
    N: INode;
  begin
    FStyleSection := '';
    N := D.Root.Nodes.ByName['style'];
    if N <> nil then
      FStyleSection := N.Text;
  end;

begin
  if StyleEx <> nil then
    StyleEx.Free;
  StyleEx := nil;
  if StyleExClass <> nil then
  begin
    FStyleExClass := StyleExClass;
    StyleEx := StyleExClass.Create;
  end;
  D := NewDocument;
  Cleanup;
  try
    D.Xml := Xml;
    if (D.Root = nil) or (D.Root.Name <> 'svg') then
      GenerateErrorImage
    else
    begin
      FindViewBox;
      FindStyle;
      { Styles on the root svg element are inherited by everything in it }
      Doc := Self;
      DefaultStyle;
      Opacity := 1;
      Removed := False;
      Transform := '';
      Parse(D.Root);
      AddNodes(Self, FNodes, D.Root.Nodes)
    end;
  except
    Cleanup;
    GenerateErrorImage;
  end;
end;

procedure TSvgDocument.ParseFile(const FileName: string; StyleExClass: TSvgStyleExClass = nil);
var
  S: string;
begin
  S := FileReadStr(FileName);
  ParseText(S, StyleExClass);
end;

function NewSvgDocument: TSvgDocument;
begin
  Result := TSvgDocument.Create(nil);
end;

{ TSvgRender }

const
  OutlineColor: TColorF = (Blue: $B4 / $FF; Green: $82 / $FF; Red: $46 / $FF; Alpha: 1);

function SvgToColor(C: TSvgColor; Opacity: Float): TColorF;
begin
  Result := NewColorB((C shr 16) and $FF, (C shr 8) and $FF, C and $FF);
  Result.Alpha := ((C shr 24) and $FF) / $FF * Opacity;
end;

constructor TSvgRender.Create(Canvas: ICanvas);
begin
  FCanvas := Canvas;
  FDoc := TSvgDocument.Create(nil);
  FPen := NewPen;
end;

destructor TSvgRender.Destroy;
begin
  FDoc.Free;
  inherited Destroy;
end;

procedure TSvgRender.Parse(const Xml: string);

  procedure CountNodes(N: TSvgNode);
  var
    G: TSvgGroup absolute N;
    S: TSvgNode;
  begin
    if N is TSvgLine then
      Inc(FNodeCount, 2)
    else if N is TSvgPolyLine then
      Inc(FNodeCount, TSvgPolyline(N).Points.Length)
    else if N is TSvgCircle then
      Inc(FNodeCount)
    else if N is TSvgEllipse then
      Inc(FNodeCount)
    else if N is TSvgRect then
      Inc(FNodeCount)
    else if N is TSvgPath then
      Inc(FNodeCount, TSvgPath(N).Commands.Length)
    else if N is TSvgGroup then
    begin
      Inc(FNodeCount);
      for S in G do
        CountNodes(S);
    end;
  end;

var
  N: TSvgNode;
begin
  FDoc.ParseText(Xml);
  FDocumentSize := Xml.Length;
  FNodeCount := 0;
  for N in FDoc do
    CountNodes(N);
end;

procedure TSvgRender.ParseFile(const FileName: string);
begin
  Parse(FileReadStr(FileName));
end;

procedure TSvgRender.Draw;

  function BuildBrush(Shape: TSvgShape; Opacity: Float): TColorF;
  begin
    Result := SvgToColor(Shape.Style.Fill, Shape.Style.FillOpacity * Opacity);
  end;

  function BuildPen(Shape: TSvgShape; Opacity: Float): IPen;
  begin
    FPen.Width := Shape.Style.StrokeWidth;
    FPen.Color := SvgToColor(Shape.Style.Stroke, Shape.Style.StrokeOpacity * Opacity);
    FPen.MiterLimit := Shape.Style.StrokeMiter;
    case Shape.Style.StrokeCap of
      svgCapRound: FPen.LineCap := capRound;
      svgCapButt: FPen.LineCap := capButt;
      svgCapSquare: FPen.LineCap := capSquare;
    end;
    case Shape.Style.StrokeJoin of
      svgJoinRound: FPen.LineJoin := joinRound;
      svgJoinBevel: FPen.LineJoin := joinBevel;
      svgJoinMiter: FPen.LineJoin := joinMiter;
    end;
    Result := FPen;
  end;

  procedure RenderPath(Path: TSvgPath);
  var
    C: TSvgCommand;
    I: Integer;
  begin
    for I := 0 to Path.Commands.Length - 1 do
    begin
      C := Path.Commands[I];
      case C.Action of
        svgMove: FCanvas.MoveTo(C.X, C.Y);
        svgLine,
        svgHLine,
        svgVLine:
          FCanvas.LineTo(C.X, C.Y);
        svgCubic: FCanvas.BezierTo(C.X1, C.Y1, C.X2, C.Y2, C.X, C.Y);
        svgQuadratic: FCanvas.QuadTo(C.X1, C.Y1, C.X, C.Y);
        svgClose: FCanvas.ClosePath;
      end;
    end;
  end;

  procedure Polyline(N: TSvgPolyline);
  var
    P: TPointF;
    I: Integer;
  begin
    for I := 0 to N.Points.Length - 1 do
    begin
      P := N.Points[I];
      if I = 0 then
        FCanvas.MoveTo(P.X, P.Y)
      else
        FCanvas.LineTo(P.X, P.Y);
    end;
    if N is TSvgPolygon then
      FCanvas.ClosePath;
  end;

  procedure Render(N: TSvgNode; Opacity: Float);
  var
    L: TSvgLine absolute N;
    C: TSvgCircle absolute N;
    E: TSvgEllipse absolute N;
    R: TSvgRect absolute N;
    P: TSvgPath absolute N;
    G: TSvgGroup absolute N;
    S: TSvgNode;
    T: TSvgTransform;
    M: IMatrix;
  begin
    if N.Removed then Exit;
    M := nil;
    if N.Transform <> '' then
    begin
      N.BuildTransform(T);
      M := NewMatrix;
      M.Copy(T[0], T[1], T[2], T[3], T[4], T[5]);
      M.Transform(FCanvas.Matrix);
      FCanvas.Matrix.Push;
      FCanvas.Matrix := M;
    end;
    if N is TSvgLine then
    begin
      FCanvas.MoveTo(L.X1, L.Y1);
      FCanvas.LineTo(L.X2, L.Y2);
    end
    else if N is TSvgPolyLine then
      Polyline(TSvgPolyline(N))
    else if N is TSvgCircle then
      FCanvas.Circle(C.X, C.Y, C.R)
    else if N is TSvgEllipse then
      FCanvas.Ellipse(E.X, E.Y, E.W, E.H)
    else if N is TSvgRect then
      if R.R > 0 then
        FCanvas.RoundRect(R.X, R.Y, R.W, R.H, R.R)
      else
        FCanvas.Rect(R.X, R.Y, R.W, R.H)
    else if N is TSvgPath then
      RenderPath(P)
    else if N is TSvgGroup then
    begin
      for S in G do
        Render(S, Opacity * G.Opacity);
      if M <> nil then
        FCanvas.Matrix.Pop;
      Exit;
    end;
    if not (N is TSvgLine) then
      if R.Style.Fill <> 0 then
      begin
        if R.Style.FillRule = svgFillEvenOdd then
          FCanvas.FillRule := fillEvenOdd
        else
          FCanvas.FillRule := fillNonZero;
        FCanvas.Fill(BuildBrush(R, Opacity * R.Opacity), True);
      end;
    if R.Style.Stroke <> 0 then
      FCanvas.Stroke(BuildPen(R, Opacity * R.Opacity), True);
    FCanvas.BeginPath;
    if M <> nil then
      FCanvas.Matrix.Pop;
  end;

var
  N: TSvgNode;
  Rule: TFillRule;
begin
  Rule := FCanvas.FillRule;
  try
    for N in FDoc do
      Render(N, FDoc.Opacity);
  finally
    FCanvas.FillRule := Rule;
  end;
end;

procedure TSvgRender.DrawOutline(PenWidth: Float);

  procedure RenderPath(Path: TSvgPath);
  var
    C: TSvgCommand;
    I: Integer;
  begin
    for I := 0 to Path.Commands.Length - 1 do
    begin
      C := Path.Commands[I];
      case C.Action of
        svgMove: FCanvas.MoveTo(C.X, C.Y);
        svgLine,
        svgHLine,
        svgVLine:
          FCanvas.LineTo(C.X, C.Y);
        svgCubic: FCanvas.BezierTo(C.X1, C.Y1, C.X2, C.Y2, C.X, C.Y);
        svgQuadratic: FCanvas.QuadTo(C.X1, C.Y1, C.X, C.Y);
        svgClose: FCanvas.ClosePath;
      end;
    end;
  end;

  procedure Polyline(N: TSvgPolyline);
  var
    P: TPointF;
    I: Integer;
  begin
    for I := 0 to N.Points.Length - 1 do
    begin
      P := N.Points[I];
      if I = 0 then
        FCanvas.MoveTo(P.X, P.Y)
      else
        FCanvas.LineTo(P.X, P.Y);
    end;
    if N is TSvgPolygon then
      FCanvas.ClosePath;
  end;

  procedure Render(N: TSvgNode; Opacity: Float);
  var
    L: TSvgLine absolute N;
    C: TSvgCircle absolute N;
    E: TSvgEllipse absolute N;
    R: TSvgRect absolute N;
    P: TSvgPath absolute N;
    G: TSvgGroup absolute N;
    S: TSvgNode;
    T: TSvgTransform;
    M: IMatrix;
  begin
    if N.Removed then Exit;
    M := nil;
    if N.Transform <> '' then
    begin
      N.BuildTransform(T);
      M := NewMatrix;
      M.Copy(T[0], T[1], T[2], T[3], T[4], T[5]);
      M.Transform(FCanvas.Matrix);
      FCanvas.Matrix.Push;
      FCanvas.Matrix := M;
    end;
    if N is TSvgLine then
    begin
      FCanvas.MoveTo(L.X1, L.Y1);
      FCanvas.LineTo(L.X2, L.Y2);
    end
    else if N is TSvgPolyLine then
      Polyline(TSvgPolyline(N))
    else if N is TSvgCircle then
      FCanvas.Circle(C.X, C.Y, C.R)
    else if N is TSvgEllipse then
      FCanvas.Ellipse(E.X, E.Y, E.W, E.H)
    else if N is TSvgRect then
      if R.R > 0 then
        FCanvas.RoundRect(R.X, R.Y, R.W, R.H, R.R)
      else
        FCanvas.Rect(R.X, R.Y, R.W, R.H)
    else if N is TSvgPath then
      RenderPath(P)
    else if N is TSvgGroup then
    begin
      for S in G do
        Render(S, Opacity * G.Opacity);
      if M <> nil then
        FCanvas.Matrix.Pop;
      Exit;
    end;
    FCanvas.Stroke(OutlineColor, PenWidth);
    if M <> nil then
      FCanvas.Matrix.Pop;
  end;

var
  N: TSvgNode;
begin
  for N in FDoc do
    Render(N, 1);
end;

procedure TSvgRender.DrawNodes(PenWidth: Float);
const
  NodeSize = 4;

  procedure RenderPath(Path: TSvgPath);
  var
    F, H: Float;
    C: TSvgCommand;
    I: Integer;
  begin
    F := PenWidth * NodeSize;
    H := F / 2;
    for I := 0 to Path.Commands.Length - 1 do
    begin
      C := Path.Commands[I];
      case C.Action of
        svgMove, svgLine: FCanvas.Rect(C.X - H, C.Y - H, F, F);
        svgCubic:
          begin
            FCanvas.Rect(C.X1 - H, C.Y1 - H, F, F);
            FCanvas.Rect(C.X2 - H, C.Y2 - H, F, F);
            FCanvas.Rect(C.X - H, C.Y - H, F, F);
            FCanvas.MoveTo(C.X1, C.Y1);
            FCanvas.LineTo(C.X, C.Y);
            FCanvas.LineTo(C.X2, C.Y2);
          end;
        svgQuadratic:
          begin
            FCanvas.Rect(C.X1 - H, C.Y1 - H, F, F);
            FCanvas.Rect(C.X - H, C.Y - H, F, F);
            FCanvas.MoveTo(C.X1, C.Y1);
            FCanvas.LineTo(C.X, C.Y);
          end;
      end;
    end;
  end;

  procedure Polyline(N: TSvgPolyline);
  var
    P: TPointF;
    I: Integer;
  begin
    for I := 0 to N.Points.Length - 1 do
    begin
      P := N.Points[I];
      FCanvas.Rect(P.X - PenWidth * NodeSize / 2, P.Y - PenWidth * NodeSize / 2, PenWidth * NodeSize, PenWidth * NodeSize);
    end;
  end;

  procedure Render(N: TSvgNode; Opacity: Float);
  var
    L: TSvgLine absolute N;
    C: TSvgCircle absolute N;
    E: TSvgEllipse absolute N;
    R: TSvgRect absolute N;
    P: TSvgPath absolute N;
    G: TSvgGroup absolute N;
    S: TSvgNode;
    T: TSvgTransform;
    M: IMatrix;
  begin
    if N.Removed then Exit;
    M := nil;
    if N.Transform <> '' then
    begin
      N.BuildTransform(T);
      M := NewMatrix;
      M.Copy(T[0], T[1], T[2], T[3], T[4], T[5]);
      M.Transform(FCanvas.Matrix);
      FCanvas.Matrix.Push;
      FCanvas.Matrix := M;
    end;
    if N is TSvgLine then
    begin
      FCanvas.MoveTo(L.X1, L.Y1);
      FCanvas.LineTo(L.X2, L.Y2);
    end
    else if N is TSvgPolyLine then
      Polyline(TSvgPolyline(N))
    else if N is TSvgCircle then
      FCanvas.Circle(C.X, C.Y, C.R)
    else if N is TSvgEllipse then
      FCanvas.Ellipse(E.X, E.Y, E.W, E.H)
    else if N is TSvgRect then
      if R.R > 0 then
        FCanvas.RoundRect(R.X, R.Y, R.W, R.H, R.R)
      else
        FCanvas.Rect(R.X, R.Y, R.W, R.H)
    else if N is TSvgPath then
      RenderPath(P)
    else if N is TSvgGroup then
    begin
      for S in G do
        Render(S, Opacity * G.Opacity);
      if M <> nil then
        FCanvas.Matrix.Pop;
      Exit;
    end;
    FCanvas.Stroke(OutlineColor, PenWidth);
    FCanvas.BeginPath;
    if M <> nil then
      FCanvas.Matrix.Pop;
  end;

var
  N: TSvgNode;
begin
  for N in FDoc do
    Render(N, 1);
end;

procedure TSvgRender.DrawAt(X, Y, Scale, Angle, Size: Float; Options: TSvgRenderOptions = [renderColor]; PenWidth: Float = 1);
var
  M: IMatrix;
  P: TPointF;
  S: Float;
begin
  { A document without a size cannot be scaled to fit }
  if FDoc.ViewBox.Width <= 0 then
    Exit;
  P := FDoc.ViewBox.MidPoint;
  S := Scale / (FDoc.ViewBox.Width / Size);
  { Build the placement in its own matrix, then apply the current canvas
    matrix after it so the document is placed within the canvas transform }
  M := NewMatrix;
  if Angle <> 0 then
    M.RotateAt(Angle, P.X, P.Y);
  M.Translate(X - P.X, Y - P.Y);
  M.ScaleAt(S, S, X, Y);
  M.Transform(FCanvas.Matrix);
  FCanvas.Matrix.Push;
  FCanvas.Matrix := M;
  if renderColor in Options then
    Draw;
  if renderOutlines in Options then
    DrawOutline(PenWidth);
  if renderNodes in Options then
    DrawNodes(PenWidth);
  FCanvas.Matrix.Pop;
end;

function TSvgRender.GetViewBox: TRectF;
begin
  Result := FDoc.ViewBox;
end;

end.
