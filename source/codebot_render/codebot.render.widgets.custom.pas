unit Codebot.Render.Widgets.Custom;

{$i render.inc}

interface

uses
  SysUtils,
  Codebot.System,
  Codebot.Graphics.Types,
  Codebot.Render.Graphics,
  Codebot.Render.Contexts,
  Codebot.Render.Widgets,
  Codebot.Render.Widgets.Themes;

{ Custom widgets draw themselves in Paint instead of being drawn by the
  theme. They take their colors and font from the theme, and measure time
  using the time passed to TMainWidget.Render.

  TCustomGraph draws a scrolling line graph with a label to its left. A new
  data point is added every FrameMeasure seconds, Step pixels apart. Descend
  from it and override DataPoint, DefaultMax, and InfoText to graph a value.

  TPerformanceGraph graphs the number of frames drawn each second. Add one to
  a window and give it a Width and Height; it needs nothing else.

  TMarkDownLabel is a label whose text is markdown. See its declaration for
  the markdown it understands. }

{ TCustomGraph is the base class for scrolling line graphs }

type
  TCustomGraph = class(TCustomWidget)
  private
    FData: TArrayList<Double>;
    FTime: Double;
    FStart: Double;
    FLast: Double;
    FMax: Double;
    FFrameMeasure: Double;
    FStep: Float;
    FLabelMargin: Float;
    procedure RenderData(Canvas: ICanvas; Rect: TRectF);
  protected
    function DefaultColor: TColorF; virtual;
    function DefaultMax: Double; virtual;
    function DataPoint: Double; virtual;
    function InfoText: string; virtual;
    procedure Paint(Stage: TPaintStage); override;
    property Max: Double read FMax;
  public
    constructor Create(Parent: TWidget; const Name: string = ''); override;
    { Update takes the next data points. It is called each time the graph is
      painted, so it does not need to be called by a scene. }
    procedure Update; virtual;
    { The time in seconds between data points }
    property FrameMeasure: Double read FFrameMeasure write FFrameMeasure;
    { The distance in pixels between data points }
    property Step: Float read FStep write FStep;
    { The room kept at the left of the graph for its label }
    property LabelMargin: Float read FLabelMargin write FLabelMargin;
  end;

{ TPerformanceGraph }

const
  { The frame rate is averaged over this many of the most recent frames }
  AverageFrames = 10;

type
  { TPerformanceGraph graphs the number of frames drawn each second }
  TPerformanceGraph = class(TCustomGraph)
	private
    FFrameRate: LongWord;
    { The times of the most recent frames, kept in a ring. One more time than
      frames is kept because ten frames lie between eleven times. }
    FTimes: array[0..AverageFrames] of Double;
    FTimeCount: Integer;
    FTimeIndex: Integer;
	protected
    function InfoText: string; override;
  public
    procedure Update; override;
  end;


(* TMarkDownLabel is a label which draws its text as markdown. Set MaxWidth to
  wrap the text to a width, which is then the width of the label. Without a
  MaxWidth each paragraph is one line. The height of the label fits the text.

  These parts of markdown are drawn:

    # Heading, ## Heading, ### Heading (and deeper levels)
    Paragraphs, which are separated by blank lines. Lines next to each other
    are joined. End a line with two spaces or a backslash to break it.
    **bold**, __bold__, *italic*, _italic_, ***bold italic***, `code`
    [links](url), drawn underlined
    - bullets, * bullets, + bullets, and 1. numbered lists, indented two
    spaces for each level
    > block quotes
    ``` fenced code blocks ```
    --- horizontal rules
    \ escapes the next character

  A line holding only ![](name){left 240x180} reserves a rectangle which is
  drawn by OnDrawRect. The name tells the handler what to draw, and the size
  is in the units of the label. A left or right rectangle floats at that
  side, and paragraphs, headings, and lists flow around it. A center
  rectangle sits on its own between the blocks around it, which is also
  where a float goes when the label has no MaxWidth or the float would leave
  too little room for text. Code blocks, quotes, and rules start below any
  floats, and a line holding only {clear} does the same for what follows.

  The text uses the NotoSans fonts and DejaVuSansMono from the fonts folder
  of the render context assets. A font which can not be loaded is replaced
  by the theme font. FontSize is the size of plain text, and when it is zero
  the size of the theme font is used. *)

  TMarkDownStyle = (mdRegular, mdBold, mdItalic, mdBoldItalic, mdCode);

  { TMarkDownFragment is a piece of text placed by the layout of a markdown
    label }
  TMarkDownFragment = record
    X, Y, Width: Float;
    Text: string;
    Style: TMarkDownStyle;
    Size: Float;
    Link: Boolean;
    Code: Boolean;
    Faded: Boolean;
  end;

  { TMarkDownBoxAlign is where a rectangle in markdown is placed }
  TMarkDownBoxAlign = (boxLeft, boxCenter, boxRight);

  { TMarkDownBox is a rectangle placed by the layout of a markdown label }
  TMarkDownBox = record
    Name: string;
    Rect: TRectF;
  end;

  { TMarkDownRectEvent draws a rectangle of a markdown label. Rect is on the
    canvas, which is clipped to it. }
  TMarkDownRectEvent = procedure(Sender: TObject; Surface: ICanvas;
    const Name: string; const Rect: TRectF) of object;

  { TMarkDownDecorKind are the things drawn around markdown text }
  TMarkDownDecorKind = (decorCodeBlock, decorQuote, decorRule, decorBullet);

  { TMarkDownDecor is one of those things and where it is drawn }
  TMarkDownDecor = record
    Kind: TMarkDownDecorKind;
    Rect: TRectF;
  end;

  TMarkDownLabel = class(TLabel)
  private
    FFonts: array[TMarkDownStyle] of IFont;
    FFontSize: Float;
    FFragments: array of TMarkDownFragment;
    FFragmentCount: Integer;
    FDecors: array of TMarkDownDecor;
    FDecorCount: Integer;
    FBoxes: array of TMarkDownBox;
    FBoxCount: Integer;
    FOnDrawRect: TMarkDownRectEvent;
    procedure SetFontSize(Value: Float);
    function GetFont(Canvas: ICanvas; Style: TMarkDownStyle): IFont;
    function BaseSize: Float;
    procedure Layout;
  protected
    procedure Resize; override;
    procedure Paint(Stage: TPaintStage); override;
  public
    constructor Create(Parent: TWidget; const Name: string = ''); override;
    { The size of plain text, or zero for the size of the theme font }
    property FontSize: Float read FFontSize write SetFontSize;
    { OnDrawRect is called for each rectangle in the markdown when the label
      is drawn, after the backgrounds of code blocks and quotes and before
      the text }
    property OnDrawRect: TMarkDownRectEvent read FOnDrawRect write FOnDrawRect;
  end;

implementation

{ TCustomGraph }

constructor TCustomGraph.Create(Parent: TWidget; const Name: string = '');
begin
  inherited Create(Parent, Name);
  FFrameMeasure := 1 / 6;
  FStep := 7.5;
  FLabelMargin := 50;
end;

procedure TCustomGraph.Update;
var
  Current: Double;
begin
  if FStart = 0 then
  begin
    FStart := Main.Time;
    FLast := FStart;
  end;
  Current := Main.Time;
  if Current - FLast > FFrameMeasure then
  begin
    FData.Push(DataPoint);
    FLast := FLast + FFrameMeasure;
    while (FLast + FFrameMeasure < Current) do
    begin
      FLast := FLast + FFrameMeasure;
      FData.Push(DataPoint);
    end;
  end;
  FTime := Current - FStart;
  while FData.Length > 5000 do
    FData.Delete(0);
end;

procedure TCustomGraph.RenderData(Canvas: ICanvas; Rect: TRectF);
var
  Offset, X, Y, X0, Y0: Float;
  I, J: Integer;
  C: TColorF;
begin
  Canvas.RoundRect(Rect, 4);
  C := Color(colorFace);
  Canvas.Fill(C);
  Rect.Inflate(0, -4);
  Offset := (FTime - FLast + FStart) / FFrameMeasure * FStep - FStep;
  X := Rect.Left;
  Y := Rect.Bottom;
  if FData.Length < 2 then
    Exit;
  FMax := DefaultMax;
  I := 0;
  J := FData.Length;
  for I := 0 to FData.Length  - 1 do
  begin
    Dec(J);
    if FData[J] > FMax then
      FMax := FData[J];
    if I * FStep > Rect.Width then
      Break;
  end;
  Canvas.MoveTo(X, Y - FData.Last / FMax * Rect.Height);
  J := FData.Length - 1;
  for I := 1 to FData.Length  - 1 do
  begin
    Dec(J);
    X0 := X + I * FStep + Offset;
    if X0 < Rect.Left then
      X0 := Rect.Left;
    if X0 > Rect.Right then
      X0 := Rect.Right;
    Y0 := Y - FData[J] / FMax * Rect.Height;
    if Y0 < Rect.Top then
      Y0 := Rect.Top;
    Canvas.LineTo(X0, Y0);
    X0 := X + I * FStep + Offset;
    if X0 > Rect.Right then
      Break;
  end;
  Canvas.Stroke(DefaultColor);
  Rect.Inflate(0, 4);
  Canvas.RoundRect(Rect, 4);
  Canvas.Stroke(Color(colorBorder));
end;

function TCustomGraph.DefaultColor: TColorF;
begin
  Result := Color(colorActive);
end;

function TCustomGraph.DefaultMax: Double;
begin
  Result := 120;
end;

function TCustomGraph.DataPoint: Double;
begin
  Result := 0;
end;

function TCustomGraph.InfoText: string;
begin
  Result := '';
end;

procedure TCustomGraph.Paint(Stage: TPaintStage);
var
  T: TCanvasTheme;
  C: ICanvas;
  R: TRectF;
  F: IFont;
  S: StringArray;
  Size: Float;
  I: Integer;
begin
  if Stage = postPaint then Exit;
  { The graph is painted once a frame, which is when it takes its data }
  Update;
  if Computed.Theme is TCanvasTheme then
  begin
    T := TCanvasTheme(Computed.Theme);
    C := T.Canvas;
    R := Computed.Bounds;
    R.X := R.X + FLabelMargin;
    R.Width := R.Width - FLabelMargin;
    RenderData(C, R);
    { The label uses the theme font at a smaller size, which is put back
      because the font is shared with the other widgets }
    F := T.Font;
    Size := F.Size;
    F.Size := 11;
    F.Align := fontRight;
    F.Layout := fontMiddle;
    F.Color := Color(colorText);
    with R.Sector(4) do
    begin
      S := InfoText.Split(#10);
      for I := 0 to S.Length - 1 do
        C.DrawText(F, S[I] , X - 4, Y - 15 + (I * 15));
    end;
    F.Size := Size;
    F.Align := fontCenter;
  end;
end;

{ TPerformanceGraph }

function TPerformanceGraph.InfoText: string;
begin
  Result :=
  	IntToStr(System.Round(FMax)) + ' max'#10 +
    IntToStr(FFrameRate) + ' fps';
end;

procedure TPerformanceGraph.Update;
var
  Current, Oldest: Double;
  Frames: Integer;
begin
  if FStart = 0 then
  begin
    FStart := Main.Time;
    FLast := FStart;
  end;
  Current := Main.Time;
  { Remember the time of this frame, replacing the oldest time when full }
  FTimes[FTimeIndex] := Current;
  FTimeIndex := (FTimeIndex + 1) mod Length(FTimes);
  if FTimeCount < Length(FTimes) then
    Inc(FTimeCount);
  { The frame rate is the number of frames between the oldest time kept and
    this one, divided by the time they took }
  Frames := FTimeCount - 1;
  if Frames > 0 then
  begin
    if FTimeCount < Length(FTimes) then
      Oldest := FTimes[0]
    else
      Oldest := FTimes[FTimeIndex];
    if Current > Oldest then
      FFrameRate := Round(Frames / (Current - Oldest));
  end;
  if Current - FLast > FFrameMeasure then
  begin
    FData.Push(FFrameRate);
    FLast := FLast + FFrameMeasure;
    { A stall longer than one measure shows as a gap in the graph }
    while (FLast + FFrameMeasure < Current) do
    begin
      FLast := FLast + FFrameMeasure;
      FData.Push(0);
    end;
  end;
  FTime := Current - FStart;
  while FData.Length > 5000 do
    FData.Delete(0);
end;


{ TMarkDownLabel }

type
  TMarkDownRun = record
    Text: string;
    Style: TMarkDownStyle;
    Link: Boolean;
    Code: Boolean;
  end;

  TMarkDownRuns = array of TMarkDownRun;

const
  MarkDownFontFiles: array[TMarkDownStyle] of string = (
    'NotoSans-Regular.ttf', 'NotoSans-Bold.ttf', 'NotoSans-Italic.ttf',
    'NotoSans-BoldItalic.ttf', 'DejaVuSansMono.ttf');
  { Sizes and spaces as a part of the font size }
  MarkDownLineHeight = 1.4;
  MarkDownGap = 0.6;
  MarkDownItemGap = 0.25;
  MarkDownCodeSize = 0.9;
  MarkDownIndent = 1.4;
  MarkDownHeadings: array[1..6] of Float = (1.6, 1.35, 1.15, 1, 1, 1);
  { The space around the text of a code block in pixels }
  MarkDownCodePad = 8;
  { The space between a floating rectangle and the text beside it, the
    narrowest a line beside a float can be, both as a part of the font size,
    and the widest a float can be as a part of the width of the label }
  MarkDownBoxGap = 1;
  MarkDownMinLine = 8;
  MarkDownMaxFloat = 0.6;

(* BoxLine reads a line holding only ![](name){align WxH}. The alignment is
  left, right, or center, and center when it is left out. *)

function BoxLine(const S: string; out Name: string; out Align: TMarkDownBoxAlign;
  out Width, Height: Float): Boolean;
var
  Words: StringArray;
  U: string;
  A, B, X: Integer;
begin
  Result := False;
  Name := '';
  Align := boxCenter;
  Width := 0;
  Height := 0;
  if (Copy(S, 1, 4) <> '![](') or (S[Length(S)] <> '}') then
    Exit;
  A := Pos(')', S);
  if (A < 6) or (A + 1 > Length(S)) or (S[A + 1] <> '{') then
    Exit;
  Name := Trim(Copy(S, 5, A - 5));
  U := Copy(S, A + 2, Length(S) - A - 2);
  Words := U.Split(' ');
  for A := 0 to Words.Length - 1 do
  begin
    U := LowerCase(Trim(Words[A]));
    if U = '' then
      Continue;
    if U = 'left' then
      Align := boxLeft
    else if U = 'right' then
      Align := boxRight
    else if U = 'center' then
      Align := boxCenter
    else
    begin
      X := Pos('x', U);
      if X < 2 then
        Exit;
      Val(Copy(U, 1, X - 1), Width, B);
      if B <> 0 then
        Exit;
      Val(Copy(U, X + 1, Length(U)), Height, B);
      if B <> 0 then
        Exit;
    end;
  end;
  Result := (Name <> '') and (Width > 0) and (Height > 0);
end;

function StyleOf(Bold, Italic: Boolean): TMarkDownStyle;
begin
  if Bold and Italic then
    Result := mdBoldItalic
  else if Bold then
    Result := mdBold
  else if Italic then
    Result := mdItalic
  else
    Result := mdRegular;
end;

{ ParseInline splits a paragraph into runs of text which share a style. A
  delimiter opens only when it is followed by a non space and closes only
  when it follows a non space, so a lone star or underscore is drawn as it
  is. Underscores inside a word are left alone. A #10 in the text is a line
  break. }

function ParseInline(const Text: string; ForceBold: Boolean): TMarkDownRuns;
var
  Runs: TMarkDownRuns;
  Count: Integer;
  Buffer: string;
  Bold, Italic, Link: Boolean;
  I, J, K, E, N: Integer;

  procedure Flush(Code: Boolean = False);
  begin
    if Buffer = '' then
      Exit;
    if Count = Length(Runs) then
      SetLength(Runs, Count * 2 + 8);
    Runs[Count].Text := Buffer;
    if Code then
      Runs[Count].Style := mdCode
    else
      Runs[Count].Style := StyleOf(Bold or ForceBold, Italic);
    Runs[Count].Link := Link;
    Runs[Count].Code := Code;
    Inc(Count);
    Buffer := '';
  end;

  function Ch(Index: Integer): Char;
  begin
    if (Index < 1) or (Index > N) then
      Result := ' '
    else
      Result := Text[Index];
  end;

  function IsSpace(C: Char): Boolean;
  begin
    Result := C in [' ', #9, #10];
  end;

  function IsWord(C: Char): Boolean;
  begin
    Result := C in ['A'..'Z', 'a'..'z', '0'..'9'];
  end;

  { Toggle a delimiter Len characters long at I if it opens or closes }
  function Toggle(Len: Integer; var Flag: Boolean): Boolean;
  var
    D: Char;
  begin
    Result := False;
    D := Text[I];
    if Flag then
    begin
      if IsSpace(Ch(I - 1)) then
        Exit;
      if (D = '_') and IsWord(Ch(I + Len)) then
        Exit;
    end
    else
    begin
      if IsSpace(Ch(I + Len)) then
        Exit;
      if (D = '_') and IsWord(Ch(I - 1)) then
        Exit;
    end;
    Flush;
    Flag := not Flag;
    Inc(I, Len);
    Result := True;
  end;

begin
  Runs := nil;
  Count := 0;
  Buffer := '';
  Bold := False;
  Italic := False;
  Link := False;
  N := Length(Text);
  I := 1;
  while I <= N do
  begin
    case Text[I] of
      '\':
        if (I < N) and (Text[I + 1] in ['\', '`', '*', '_', '{', '}', '[', ']',
          '(', ')', '#', '+', '-', '.', '!', '>', '|']) then
        begin
          Buffer := Buffer + Text[I + 1];
          Inc(I, 2);
          Continue;
        end;
      '`':
        begin
          { Code is drawn as it is up to the closing backtick }
          J := Pos('`', Copy(Text, I + 1, N));
          if J > 0 then
          begin
            Flush;
            Buffer := Copy(Text, I + 1, J - 1);
            Flush(True);
            Inc(I, J + 1);
            Continue;
          end;
        end;
      '*', '_':
        begin
          { Three delimiters toggle bold and italic together }
          if (Ch(I + 1) = Text[I]) and (Ch(I + 2) = Text[I]) and (Bold = Italic) then
            if Toggle(3, Bold) then
            begin
              Italic := Bold;
              Continue;
            end;
          if Ch(I + 1) = Text[I] then
          begin
            if Toggle(2, Bold) then
              Continue;
          end
          else if Toggle(1, Italic) then
            Continue;
        end;
      '[':
        begin
          { A link is [text](url) and only its text is drawn }
          J := Pos('](', Copy(Text, I + 1, N));
          if J > 0 then
          begin
            E := I + J;
            K := Pos(')', Copy(Text, E + 2, N));
            if K > 0 then
            begin
              Flush;
              Link := True;
              Buffer := Copy(Text, I + 1, E - I - 1);
              Flush;
              Link := False;
              I := E + 2 + K;
              Continue;
            end;
          end;
        end;
    end;
    Buffer := Buffer + Text[I];
    Inc(I);
  end;
  Flush;
  SetLength(Runs, Count);
  Result := Runs;
end;

constructor TMarkDownLabel.Create(Parent: TWidget; const Name: string = '');
begin
  inherited Create(Parent, Name);
  FOwnerDraw := True;
end;

procedure TMarkDownLabel.SetFontSize(Value: Float);
begin
  if Value = FFontSize then Exit;
  FFontSize := Value;
  Resize;
end;

{ Each label keeps its own copies of the fonts, so changing their sizes does
  not change the fonts of other widgets }

function TMarkDownLabel.GetFont(Canvas: ICanvas; Style: TMarkDownStyle): IFont;
var
  T: TTheme;
  S: string;
begin
  Result := FFonts[Style];
  if Result <> nil then
    Exit;
  S := MarkDownFontFiles[Style];
  try
    Result := Canvas.LoadFont(ChangeFileExt(S, ''), Ctx.GetAssetFile(FontRes + '/' + S));
  except
    Result := nil;
  end;
  if Result = nil then
  begin
    T := Computed.Theme;
    if T is TCanvasTheme then
      Result := Canvas.LoadFont(TCanvasTheme(T).Font.Name);
  end;
  FFonts[Style] := Result;
end;

function TMarkDownLabel.BaseSize: Float;
var
  T: TTheme;
begin
  Result := FFontSize;
  if Result > 0 then
    Exit;
  T := Computed.Theme;
  if (T is TCanvasTheme) and (TCanvasTheme(T).Font <> nil) then
    Result := TCanvasTheme(T).Font.Size;
  if Result <= 0 then
    Result := 14;
end;

procedure TMarkDownLabel.Resize;
begin
  if Computed.Theme is TCanvasTheme then
    Layout
  else
    inherited Resize;
end;

{ Layout breaks the text into blocks, then flows the runs of each block into
  lines of fragments. Fragments and decorations are placed relative to the
  top left of the label. }

procedure TMarkDownLabel.Layout;
type
  TBlockKind = (blockNone, blockParagraph, blockHeading, blockList, blockOther);
  { A rectangle which text flows around, at the left or the right }
  TFloatBox = record
    Rect: TRectF;
    Left: Boolean;
  end;
var
  Canvas: ICanvas;
  Lines: StringArray;
  Floats: array of TFloatBox;
  Size, Wrap, Y, Right: Float;
  LastBlock: TBlockKind;
  Para: string;
  Line, T: string;
  I: Integer;

  procedure AddDecor(Kind: TMarkDownDecorKind; X, Y, W, H: Float);
  begin
    if FDecorCount = Length(FDecors) then
      SetLength(FDecors, FDecorCount * 2 + 8);
    FDecors[FDecorCount].Kind := Kind;
    FDecors[FDecorCount].Rect := NewRectF(X, Y, W, H);
    Inc(FDecorCount);
  end;

  function AddFragment(X, Y: Float; const Text: string; Style: TMarkDownStyle;
    FontSize: Float; Link, Code, Faded: Boolean): Integer;
  begin
    if FFragmentCount = Length(FFragments) then
      SetLength(FFragments, FFragmentCount * 2 + 16);
    FFragments[FFragmentCount].X := X;
    FFragments[FFragmentCount].Y := Y;
    FFragments[FFragmentCount].Width := 0;
    FFragments[FFragmentCount].Text := Text;
    FFragments[FFragmentCount].Style := Style;
    FFragments[FFragmentCount].Size := FontSize;
    FFragments[FFragmentCount].Link := Link;
    FFragments[FFragmentCount].Code := Code;
    FFragments[FFragmentCount].Faded := Faded;
    Result := FFragmentCount;
    Inc(FFragmentCount);
  end;

  procedure Extend(X: Float);
  begin
    if X > Right then
      Right := X;
  end;

  { The left and right of the room for a line at Y, between the floats
    beside it }
  function Beside(const F: TFloatBox; LineHeight: Float): Boolean;
  begin
    Result := (Y < F.Rect.Bottom) and (Y + LineHeight > F.Rect.Top);
  end;

  procedure LineBounds(LineHeight: Float; out L, R: Float);
  var
    J: Integer;
    G: Float;
  begin
    L := 0;
    R := Wrap;
    G := Size * MarkDownBoxGap;
    for J := 0 to Length(Floats) - 1 do
      if Beside(Floats[J], LineHeight) then
        if Floats[J].Left then
        begin
          if Floats[J].Rect.Right + G > L then
            L := Floats[J].Rect.Right + G;
        end
        else if Floats[J].Rect.X - G < R then
          R := Floats[J].Rect.X - G;
  end;

  { Find where a line begins and the right it wraps at. When the room beside
    the floats is too narrow, the line moves down below a float. }
  procedure StartLine(Indent, LineHeight: Float; out X, WrapAt: Float);
  var
    L, Next: Float;
    J: Integer;
  begin
    repeat
      LineBounds(LineHeight, L, WrapAt);
      X := L + Indent;
      if WrapAt - X >= Size * MarkDownMinLine then
        Exit;
      Next := 0;
      for J := 0 to Length(Floats) - 1 do
        if Beside(Floats[J], LineHeight) then
          if (Next = 0) or (Floats[J].Rect.Bottom < Next) then
            Next := Floats[J].Rect.Bottom;
      if Next <= Y then
        Exit;
      Y := Next;
    until False;
  end;

  { Move below every float }
  procedure ClearFloats;
  var
    J: Integer;
  begin
    for J := 0 to Length(Floats) - 1 do
      if Floats[J].Rect.Bottom > Y then
        Y := Floats[J].Rect.Bottom;
    Floats := nil;
  end;

  procedure AddBox(const Name: string; const Rect: TRectF);
  begin
    if FBoxCount = Length(FBoxes) then
      SetLength(FBoxes, FBoxCount * 2 + 4);
    FBoxes[FBoxCount].Name := Name;
    FBoxes[FBoxCount].Rect := Rect;
    Inc(FBoxCount);
  end;

  { Space before a block. Items of a list are closer together. }
  procedure BeginBlock(Kind: TBlockKind);
  begin
    if LastBlock <> blockNone then
      if (Kind = blockList) and (LastBlock = blockList) then
        Y := Y + Size * MarkDownItemGap
      else
        Y := Y + Size * MarkDownGap;
    LastBlock := Kind;
  end;

  { Flow runs into lines between Left and Wrap, beside any floats. Words are
    kept whole and the spaces at the start of a line are dropped. }
  procedure Flow(const Runs: TMarkDownRuns; Left, FontSize: Float; Faded: Boolean);
  var
    Font: IFont;
    LineHeight, X, S, WordWidth, FullWidth, WrapAt: Float;
    Started: Boolean;
    Fragment, R, A, B, L: Integer;
    Token, Word: string;
  begin
    if Length(Runs) = 0 then
      Exit;
    LineHeight := FontSize * MarkDownLineHeight;
    StartLine(Left, LineHeight, X, WrapAt);
    Started := False;
    for R := 0 to Length(Runs) - 1 do
    begin
      Fragment := -1;
      S := FontSize;
      if Runs[R].Code then
        S := FontSize * MarkDownCodeSize;
      Font := GetFont(Canvas, Runs[R].Style);
      if Font = nil then
        Continue;
      Font.Size := S;
      L := Length(Runs[R].Text);
      A := 1;
      while A <= L do
      begin
        { A token is a line break, or a word and the spaces after it }
        if Runs[R].Text[A] = #10 then
        begin
          Y := Y + LineHeight;
          StartLine(Left, LineHeight, X, WrapAt);
          Started := False;
          Fragment := -1;
          Inc(A);
          Continue;
        end;
        B := A;
        while (B <= L) and not (Runs[R].Text[B] in [' ', #10]) do
          Inc(B);
        Word := Copy(Runs[R].Text, A, B - A);
        while (B <= L) and (Runs[R].Text[B] = ' ') do
          Inc(B);
        Token := Copy(Runs[R].Text, A, B - A);
        A := B;
        if (Word = '') and not Started then
          Continue;
        WordWidth := Canvas.MeasureAdvance(Font, Word);
        FullWidth := Canvas.MeasureAdvance(Font, Token);
        if Started and (Word <> '') and (X + WordWidth > WrapAt) then
        begin
          Y := Y + LineHeight;
          StartLine(Left, LineHeight, X, WrapAt);
          Started := False;
          Fragment := -1;
        end;
        { Code is moved down a little to line up with the text around it }
        if Fragment < 0 then
          if Runs[R].Code then
            Fragment := AddFragment(X, Y + (FontSize - S) * 0.6, '', Runs[R].Style, S,
              Runs[R].Link, True, Faded)
          else
            Fragment := AddFragment(X, Y, '', Runs[R].Style, S, Runs[R].Link, False, Faded);
        FFragments[Fragment].Text := FFragments[Fragment].Text + Token;
        if Word <> '' then
        begin
          FFragments[Fragment].Width := X + WordWidth - FFragments[Fragment].X;
          Extend(X + WordWidth);
        end;
        X := X + FullWidth;
        Started := True;
      end;
    end;
    Y := Y + LineHeight;
  end;

  procedure EndParagraph;
  begin
    if Para = '' then
      Exit;
    BeginBlock(blockParagraph);
    Flow(ParseInline(Para, False), 0, Size, False);
    Para := '';
  end;

  procedure Heading(const Text: string; Level: Integer);
  begin
    BeginBlock(blockHeading);
    Flow(ParseInline(Text, True), 0, Size * MarkDownHeadings[Level], False);
  end;

  function IsFence(const S: string): Boolean;
  begin
    Result := Copy(Trim(S), 1, 3) = '```';
  end;

  { A rule is three or more of the same character with only spaces between }
  function IsRule(const S: string; Chars: TSysCharSet): Boolean;
  var
    C: Char;
    J, K: Integer;
  begin
    Result := False;
    if S = '' then
      Exit;
    C := S[1];
    if not (C in Chars) then
      Exit;
    K := 0;
    for J := 1 to Length(S) do
      if S[J] = C then
        Inc(K)
      else if S[J] <> ' ' then
        Exit;
    Result := K >= 3;
  end;

  function HeadingLevel(const S: string): Integer;
  begin
    Result := 0;
    while (Result < Length(S)) and (S[Result + 1] = '#') do
      Inc(Result);
    if (Result > 6) or (Result = Length(S)) or (S[Result + 1] <> ' ') then
      Result := 0;
  end;

  { Find a list item. Marker is the bullet or number and Text the rest. }
  function ListItem(const S: string; out Indent: Integer; out Marker, Text: string): Boolean;
  var
    J, K: Integer;
  begin
    Result := False;
    Indent := 0;
    while (Indent < Length(S)) and (S[Indent + 1] = ' ') do
      Inc(Indent);
    J := Indent + 1;
    if J > Length(S) then
      Exit;
    if S[J] in ['-', '*', '+'] then
      K := J + 1
    else
    begin
      K := J;
      while (K <= Length(S)) and (S[K] in ['0'..'9']) do
        Inc(K);
      if (K = J) or (K - J > 9) or (K > Length(S)) or not (S[K] in ['.', ')']) then
        Exit;
      Inc(K);
    end;
    if (K > Length(S)) or (S[K] <> ' ') then
      Exit;
    Marker := Copy(S, J, K - J);
    Text := Trim(Copy(S, K + 1, Length(S)));
    Result := True;
  end;

  function StartsBlock(const S: string): Boolean;
  var
    D: Integer;
    M, X, U: string;
    A: TMarkDownBoxAlign;
    W, H: Float;
  begin
    U := Trim(S);
    Result := (U = '') or IsFence(S) or (HeadingLevel(U) > 0) or (U[1] = '>') or
      IsRule(U, ['-', '*', '_']) or ListItem(S, D, M, X) or (U = '{clear}') or
      BoxLine(U, M, A, W, H);
  end;

  { A line ending in two spaces or a backslash breaks the line }
  procedure AddLine(var Text: string; const S: string);
  var
    HardBreak: Boolean;
    U: string;
  begin
    HardBreak := (Length(S) > 2) and (Copy(S, Length(S) - 1, 2) = '  ');
    U := Trim(S);
    if (U <> '') and (U[Length(U)] = '\') and not HardBreak then
    begin
      HardBreak := True;
      SetLength(U, Length(U) - 1);
    end;
    if (Text <> '') and (Text[Length(Text)] <> #10) then
      Text := Text + ' ';
    Text := Text + U;
    if HardBreak then
      Text := Text + #10;
  end;

  procedure CodeBlock;
  var
    Code: StringArray;
    Font: IFont;
    LineHeight, Top, W: Float;
    J: Integer;
  begin
    Inc(I);
    while (I < Lines.Length) and not IsFence(Lines[I]) do
    begin
      Code.Push(Lines[I]);
      Inc(I);
    end;
    ClearFloats;
    BeginBlock(blockOther);
    Font := GetFont(Canvas, mdCode);
    if Font = nil then
      Exit;
    Font.Size := Size * MarkDownCodeSize;
    LineHeight := Font.Size * MarkDownLineHeight;
    Top := Y;
    Y := Y + MarkDownCodePad;
    for J := 0 to Code.Length - 1 do
    begin
      W := Canvas.MeasureAdvance(Font, Code[J]);
      Extend(MarkDownCodePad * 2 + W);
      if Code[J] <> '' then
        AddFragment(MarkDownCodePad, Y, Code[J], mdCode, Font.Size, False, False, False);
      Y := Y + LineHeight;
    end;
    Y := Y + MarkDownCodePad;
    AddDecor(decorCodeBlock, 0, Top, Wrap, Y - Top);
  end;

  procedure Quote;
  var
    Text: string;
    Top: Float;
    First: Boolean;

    procedure FlowQuote;
    begin
      if Text = '' then
        Exit;
      if not First then
        Y := Y + Size * MarkDownGap;
      First := False;
      Flow(ParseInline(Text, False), Size, Size, True);
      Text := '';
    end;

  begin
    ClearFloats;
    BeginBlock(blockOther);
    Top := Y;
    Text := '';
    First := True;
    while I < Lines.Length do
    begin
      T := TrimLeft(Lines[I]);
      if (T = '') or (T[1] <> '>') then
        Break;
      System.Delete(T, 1, 1);
      if (T <> '') and (T[1] = ' ') then
        System.Delete(T, 1, 1);
      if Trim(T) = '' then
        FlowQuote
      else
        AddLine(Text, T);
      Inc(I);
    end;
    FlowQuote;
    AddDecor(decorQuote, 0, Top, 3, Y - Top);
    Dec(I);
  end;

  procedure List(Indent: Integer; const Marker, First: string);
  var
    Text: string;
    Left, LineHeight, X, W: Float;
    Font: IFont;
    Level: Integer;
  begin
    Text := '';
    AddLine(Text, First);
    { Lines after the item which do not start a block belong to it }
    while (I + 1 < Lines.Length) and not StartsBlock(Lines[I + 1]) do
    begin
      Inc(I);
      AddLine(Text, Lines[I]);
    end;
    BeginBlock(blockList);
    Level := Indent div 2;
    if Level > 4 then
      Level := 4;
    Left := Size * MarkDownIndent * (Level + 1);
    LineHeight := Size * MarkDownLineHeight;
    { The marker goes where the first line begins, beside any floats }
    StartLine(Left, LineHeight, X, W);
    { A bullet is a dot whose center and radius are kept in the rectangle }
    if Marker[1] in ['-', '*', '+'] then
      AddDecor(decorBullet, X - Size * 0.8, Y + LineHeight / 2, Size * 0.18, 0)
    else
    begin
      Font := GetFont(Canvas, mdRegular);
      if Font <> nil then
      begin
        Font.Size := Size;
        W := Canvas.MeasureAdvance(Font, Marker);
        AddFragment(X - Size * 0.4 - W, Y, Marker, mdRegular, Size, False, False, False);
      end;
    end;
    Flow(ParseInline(Text, False), Left, Size, False);
  end;

  { A float begins where the next block would, and goes below a float
    already at its side. A centered rectangle is a block of its own. }
  procedure PlaceBox(const Name: string; Align: TMarkDownBoxAlign; W, H: Float);
  var
    R: TRectF;
    G: Float;
    J: Integer;
  begin
    if (MaxWidth <= 0) or (W > Wrap * MarkDownMaxFloat) then
      Align := boxCenter;
    if Align = boxCenter then
    begin
      ClearFloats;
      BeginBlock(blockOther);
      if MaxWidth > 0 then
        R := NewRectF((Wrap - W) / 2, Y, W, H)
      else
        R := NewRectF(0, Y, W, H);
      AddBox(Name, R);
      Extend(R.Right);
      Y := Y + H;
      Exit;
    end;
    G := Size * MarkDownBoxGap;
    R.Y := Y;
    if LastBlock <> blockNone then
      R.Y := R.Y + Size * MarkDownGap;
    for J := 0 to Length(Floats) - 1 do
      if (Floats[J].Left = (Align = boxLeft)) and (Floats[J].Rect.Bottom + G > R.Y) then
        R.Y := Floats[J].Rect.Bottom + G;
    if Align = boxLeft then
      R.X := 0
    else
      R.X := Wrap - W;
    R.Width := W;
    R.Height := H;
    AddBox(Name, R);
    J := Length(Floats);
    SetLength(Floats, J + 1);
    Floats[J].Rect := R;
    Floats[J].Left := Align = boxLeft;
    { The space below a float keeps text from touching it }
    Floats[J].Rect.Height := H + G;
  end;

var
  Indent: Integer;
  BoxName: string;
  BoxAlign: TMarkDownBoxAlign;
  BoxWidth, BoxHeight: Float;
  Marker, Rest, S: string;
  Level: Integer;
  W: Float;
begin
  FFragmentCount := 0;
  FDecorCount := 0;
  FBoxCount := 0;
  Floats := nil;
  Canvas := TCanvasTheme(Computed.Theme).Canvas;
  if Canvas = nil then
    Exit;
  Size := BaseSize;
  if MaxWidth > 0 then
    Wrap := MaxWidth
  else
    Wrap := 1000000;
  Y := 0;
  Right := 0;
  LastBlock := blockNone;
  Para := '';
  S := StringReplace(Text, #13, '', [rfReplaceAll]);
  Lines := S.Split(#10);
  I := 0;
  while I < Lines.Length do
  begin
    Line := Lines[I];
    T := Trim(Line);
    if IsFence(Line) then
    begin
      EndParagraph;
      CodeBlock;
    end
    else if T = '' then
      EndParagraph
    else if HeadingLevel(T) > 0 then
    begin
      EndParagraph;
      Level := HeadingLevel(T);
      Heading(Trim(Copy(T, Level + 1, Length(T))), Level);
    end
    else if (Para <> '') and IsRule(T, ['=']) then
    begin
      { A paragraph underlined with = or - is a heading }
      Rest := Para;
      Para := '';
      Heading(Rest, 1);
    end
    else if (Para <> '') and IsRule(T, ['-']) and (Pos(' ', T) = 0) then
    begin
      Rest := Para;
      Para := '';
      Heading(Rest, 2);
    end
    else if T = '{clear}' then
    begin
      EndParagraph;
      ClearFloats;
    end
    else if BoxLine(T, BoxName, BoxAlign, BoxWidth, BoxHeight) then
    begin
      EndParagraph;
      PlaceBox(BoxName, BoxAlign, BoxWidth, BoxHeight);
    end
    else if IsRule(T, ['-', '*', '_']) then
    begin
      EndParagraph;
      ClearFloats;
      BeginBlock(blockOther);
      AddDecor(decorRule, 0, Y + Size / 2, Wrap, 1);
      Y := Y + Size;
    end
    else if T[1] = '>' then
    begin
      EndParagraph;
      Quote;
    end
    else if ListItem(Line, Indent, Marker, Rest) then
    begin
      EndParagraph;
      List(Indent, Marker, Rest);
    end
    else
      AddLine(Para, Line);
    Inc(I);
  end;
  EndParagraph;
  { The label reaches down past the last float, without the space kept
    below it }
  for I := 0 to Length(Floats) - 1 do
    if Floats[I].Rect.Bottom - Size * MarkDownBoxGap > Y then
      Y := Floats[I].Rect.Bottom - Size * MarkDownBoxGap;
  { Without a MaxWidth the label is as wide as its widest line }
  if MaxWidth > 0 then
    W := MaxWidth
  else
  begin
    W := Right;
    for I := 0 to FDecorCount - 1 do
      if FDecors[I].Kind in [decorCodeBlock, decorRule] then
        FDecors[I].Rect.Width := W;
  end;
  Width := W;
  Height := System.Round(Y);
end;

procedure TMarkDownLabel.Paint(Stage: TPaintStage);
var
  Canvas: ICanvas;
  Font: IFont;
  Bounds: TRectF;
  TextColor, C: TColorF;
  R: TRectF;
  X, Y: Float;
  I: Integer;
begin
  if Stage = postPaint then Exit;
  if not (Computed.Theme is TCanvasTheme) then Exit;
  Canvas := TCanvasTheme(Computed.Theme).Canvas;
  Bounds := Computed.Bounds;
  TextColor := Color(colorText);
  for I := 0 to FDecorCount - 1 do
  begin
    R := FDecors[I].Rect;
    R.X := R.X + Bounds.X;
    R.Y := R.Y + Bounds.Y;
    C := TextColor;
    case FDecors[I].Kind of
      decorCodeBlock:
        begin
          C.Alpha := C.Alpha * 0.1;
          Canvas.RoundRect(R, 4);
        end;
      decorQuote:
        begin
          C.Alpha := C.Alpha * 0.4;
          Canvas.Rect(R);
        end;
      decorRule:
        begin
          C.Alpha := C.Alpha * 0.3;
          Canvas.Rect(R);
        end;
      decorBullet:
        Canvas.Circle(R.X, R.Y, R.Width);
    end;
    Canvas.Fill(C);
  end;
  if Assigned(FOnDrawRect) then
    for I := 0 to FBoxCount - 1 do
    begin
      R := FBoxes[I].Rect;
      R.X := R.X + Bounds.X;
      R.Y := R.Y + Bounds.Y;
      Canvas.Push;
      try
        Canvas.Clip(R);
        FOnDrawRect(Self, Canvas, FBoxes[I].Name, R);
      finally
        Canvas.Pop;
      end;
    end;
  for I := 0 to FFragmentCount - 1 do
  begin
    Font := GetFont(Canvas, FFragments[I].Style);
    if Font = nil then
      Continue;
    X := Bounds.X + FFragments[I].X;
    Y := Bounds.Y + FFragments[I].Y;
    C := TextColor;
    if FFragments[I].Faded then
      C.Alpha := C.Alpha * 0.75;
    if FFragments[I].Code then
    begin
      Canvas.RoundRect(X - 3, Y - 1, FFragments[I].Width + 6,
        FFragments[I].Size * 1.3, 3);
      Canvas.Fill(NewColorF(C.Red, C.Green, C.Blue, C.Alpha * 0.12));
    end;
    Font.Size := FFragments[I].Size;
    Font.Align := fontLeft;
    Font.Layout := fontTop;
    Font.Color := C;
    Canvas.DrawText(Font, FFragments[I].Text, X, Y);
    if FFragments[I].Link then
    begin
      Y := Y + FFragments[I].Size * 1.2;
      Canvas.Rect(X, Y, FFragments[I].Width, 1);
      Canvas.Fill(C);
    end;
  end;
end;

end.
