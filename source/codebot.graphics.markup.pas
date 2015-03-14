(********************************************************)
(*                                                      *)
(*  Codebot.Cross Pascal Library                        *)
(*  http://cross.codebot.org                            *)
(*  Modified March 2015                                 *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.graphics.markup.txt> }
unit Codebot.Graphics.Markup;

{$i codebot.inc}

interface

uses
  SysUtils, Classes, Graphics,
  Codebot.System,
  Codebot.Graphics,
  Codebot.Graphics.Types,
  Codebot.Text.Xml;

type
  TPenData = record
    Name: string;
    Pen: IPen;
  end;

  TBrushData = record
    Name: string;
    Brush: IBrush;
  end;

  TFontData = record
    Name: string;
    Font: IFont;
  end;

  TCommandKind = (
    ckNone,
    ckMoveTo,
    ckLineTo,
    ckArcTo,
    ckCurveTo,
    ckEllipse,
    ckRectangle,
    ckRoundRectangle,
    ckText,
    ckPathOpen,
    ckPathClose,
    ckClip,
    ckUnclip,
    ckStatePush,
    ckStatePop,
    ckIdentity,
    ckRotate,
    ckScale,
    ckTranslate,
    ckStroke,
    ckFill
  );

  { TCommandData }

  TCommandData = record
  public
    procedure Reset;
  public
    Kind: TCommandKind;
    Expr: array[0..3] of string;
    case TCommandKind of
      ckMoveTo,
      ckLineTo,
      ckScale,
      ckTranslate:(
        X: Float; Y: Float;
      );
      ckArcTo: (
        Area: TRectF; BeginAngle: Float; EndAngle: Float;
      );
      ckCurveTo: (
        P: TPointF; C1: TPointF; C2: TPointF;
      );
      ckEllipse,
      ckRectangle: (
        Rect: TRectF;
      );
      ckRoundRectangle: (
        RoundRect: TRectF; Radius: Float;
      );
      ckText: (
        Font: ShortString;
        Text: ShortString;
        Insert: TPointF;
      );
      ckRotate: (
        Angle: Float;
      );
      ckStroke,
      ckFill: (
        Resource: ShortString;
        Preserve: Boolean;
      );
  end;

  TExpressionKind = (
    ekNone,
    ekColor,
    ekFloat,
    ekPoint,
    ekRect
  );

  TExpression = record
  public
    procedure Reset;
    procedure Resolve(var Data: TCommandData);
  public
    Name: string;
    Path: string;
    Kind: TExpressionKind;
    case TExpressionKind of
      ekColor: ( Color: TColorB; );
      ekFloat: ( Value: Float; );
      ekPoint: ( Point: TPointF; );
      ekRect: ( Rect: TRectF; );
  end;

  TExpressionArray = TArrayList<TExpression>;

{ TSurfaceHeader }

  TSurfaceHeader = record
  public
    procedure Reset;
    procedure Normalize;
  public
    Title: string;
    Width: Integer;
    Height: Integer;
    Opacity: Byte;
    Scale: Float;
    Display: string;
  end;

{ TSurfaceData }

  TSurfaceData = record
  private
    function GetScale: Float;
  private
    function FindBrush(const Name: string): IBrush;
    function FindPen(const Name: string): IPen;
    function FindFont(const Name: string): IFont;
    property Scale: Float read GetScale;
  public
    Doc: IDocument;
    Valid: Boolean;
    Header: TSurfaceHeader;
    Expressions: TExpressionArray;
    Brushes: TArrayList<TBrushData>;
    Pens: TArrayList<TPenData>;
    Fonts: TArrayList<TFontData>;
    Commands: TArrayList<TCommandData>;
    procedure Process(Document: IDocument; constref Defaults: TSurfaceHeader);
    procedure Render(Surface: ISurface);
  end;

implementation

{ TCommandData }

procedure TCommandData.Reset;
begin
  Kind := ckNone;
  Expr[0] := '';
  Expr[1] := '';
  Expr[2] := '';
  Expr[3] := '';
  FillZero(Font, SizeOf(Font));
  FillZero(Text, SizeOf(Text));
  FillZero(Insert, SizeOf(Insert));
end;

{ TExpression }

procedure TExpression.Reset;
begin
  Name := '';
  Path := '';
  Kind := ekNone;
  FillZero(Rect, SizeOf(Rect));
end;

procedure TExpression.Resolve(var Data: TCommandData);
begin
  if Data.Expr[0] = Name then
    case Data.Kind of
      ckMoveTo,
      ckLineTo,
      ckScale,
      ckTranslate:
        if Kind = ekFloat then
          Data.X := Value;
      ckRotate:
        if Kind = ekFloat then
          Data.Angle := DegToRad(Value);
      ckArcTo,
      ckRectangle,
      ckRoundRectangle:
        if Kind = ekRect then
          Data.Rect := Rect;
      ckCurveTo:
        if Kind = ekPoint then
          Data.P := Point;
      ckText:
        if Kind = ekPoint then
          Data.Insert := Point;
    end;
  if Data.Expr[1] = Name then
    case Data.Kind of
      ckMoveTo,
      ckLineTo,
      ckScale,
      ckTranslate:
        if Kind = ekFloat then
          Data.Y := Value;
      ckArcTo,
      ckRoundRectangle:
        if Kind = ekFloat then
          Data.BeginAngle := Value;
      ckCurveTo:
        if Kind = ekPoint then
          Data.C1:= Point;
    end;
  if Data.Expr[2] = Name then
    case Data.Kind of
      ckArcTo:
        if Kind = ekFloat then
          Data.EndAngle := Value;
      ckCurveTo:
        if Kind = ekPoint then
          Data.C2:= Point;
    end;
end;


procedure TSurfaceHeader.Reset;
begin
  Title := '(untitled)';
  Width := 256;
  Height := 256;
  Opacity := 255;
  Scale := 1;
  Display := 'fit';
end;

procedure TSurfaceHeader.Normalize;
begin
  if Scale > 5 then
    Scale := 5
  else if Scale < 0.25 then
    Scale := 0.25;
  if Width < 32 then
    Width := 32
  else if Width > 512 then
    Width := 512;
  if Height < 32 then
    Height := 32
  else if Height > 512 then
    Height := 512;
  if Display.ArrayIndex(['fit', 'tile', 'overlay']) < 0 then
    Display := 'fit';
end;

{ TSurfaceData }

procedure TSurfaceData.Process(Document: IDocument; constref Defaults: TSurfaceHeader);
var
  Added: TExpressionArray;

  procedure AddExpression(Name: string; Kind: TExpressionKind);
  var
    Expr: TExpression;
    I: Integer;
  begin
    for I := Expressions.Lo to Expressions.Hi do
    begin
      Expr := Expressions[I];
      if Expr.Name = Name then
      begin
        if Expr.Kind <> Kind then
          Expr.Reset;
        Expr.Kind := Kind;
        Expressions[I] := Expr;
        if Added.IndexOf(Expr) < 0 then
          Added.Push(Expr);
        Exit;
      end;
    end;
    Expr.Reset;
    Expr.Name := Name;
    Expr.Kind := Kind;
    Expressions.Push(Expr);
    Added.Push(Expr);
  end;

  function ParseBrush(F: IFiler): TBrushData;
  var
    C: TColorB;
    S: string;
    I: Integer;
  begin
    Result.Name := F.ReadStr('@name');
    S := F.ReadStr('@color');
    if S <> '' then
    begin
      C := StrToColor(S);
      S := F.ReadStr('@alpha');
      if S <> '' then
      begin
        I := StrToIntDef(S, 255);
        if I < 0 then
          I := 0;
        if I > 255 then
          I := 255;
        C.Alpha := I;
      end;
    end
    else
      C := clBlack;
    Result.Brush := NewBrush(C);
  end;

  function ParsePen(F: IFiler; var Data: TSurfaceData): TPenData;
  var
    Brush: IBrush;
    C: TColorB;
    W: Float;
    S: string;
    I: Integer;
  begin
    Result.Name := F.ReadStr('@name');
    S := F.ReadStr('@color');
    if S <> '' then
    begin
      C := StrToColor(S);
      S := F.ReadStr('@alpha');
      if S <> '' then
      begin
        I := StrToIntDef(S, 255);
        if I < 0 then
          I := 0;
        if I > 255 then
          I := 255;
        C.Alpha := I;
      end;
    end
    else
      C := clBlack;
    S := F.ReadStr('@width');
    if S <> '' then
    begin
      W := StrToFloatDef(S, 1);
      if W < 0.1 then
        W := 0.1;
    end
    else
      W := 1;
    S := F.ReadStr('@brush');
    if S <> '' then
      Brush := Data.FindBrush(S)
    else
      Brush := nil;
    if Brush <> nil then
      Result.Pen := NewPen(Brush, W * Scale)
    else
      Result.Pen := NewPen(C, W * Scale);
  end;

  function ParseFont(F: IFiler): TFontData;
  var
    Font: TFont;
    Style: TFontStyles;
    C: TColorB;
    S: string;
    I: Integer;
  begin
    Font := TFont.Create;
    Result.Name := F.ReadStr('@name');
    S := F.ReadStr('@color');
    if S <> '' then
    begin
      C := StrToColor(S);
      S := F.ReadStr('@alpha');
      if S <> '' then
      begin
        I := StrToIntDef(S, 255);
        if I < 0 then
          I := 0;
        if I > 255 then
          I := 255;
        C := I;
      end;
    end
    else
      C := clBlack;
    S := F.ReadStr('@face');
    if S <> '' then
      Font.Name := S;
    S := F.ReadStr('@size');
    if S <> '' then
    begin
      I := StrToIntDef(S, 0);
      if I > 0 then
        Font.Height := I;
    end;
    Style := [];
    if F.ReadBool('@bold') then
      Include(Style, fsBold);
    if F.ReadBool('@italic') then
      Include(Style, fsItalic);
    if F.ReadBool('@underline') then
      Include(Style, fsUnderline);
    Font.Style := Style;
    Result.Font := NewFont(Font);
    Result.Font.Color := C;
    Font.Free;
  end;

  function Command(Kind: TCommandKind): TCommandData;
  begin
    Result.Reset;
    Result.Kind := Kind;
  end;

  function ParseMoveTo(Kind: TCommandKind; F: IFiler): TCommandData;
  var
    S: string;
  begin
    Result.Reset;
    Result.Kind := Kind;
    S := F.ReadStr('@x');
    if S.IsIdentifier then
    begin
      AddExpression(S, ekFloat);
      Result.Expr[0] := S;
    end
    else
      Result.X := F.ReadFloat('@x');
    S := F.ReadStr('@y');
    if S.IsIdentifier then
    begin
      AddExpression(S, ekFloat);
      Result.Expr[1] := S;
    end
    else
      Result.Y := F.ReadFloat('@y');
  end;

  function ParseRect(Rect: string): TRectF;
  var
    Words: StringArray;
    I: Integer;
  begin
    Result := TRectF.Create;
    Words := Rect.Words;
    for I := 0 to Words.Length - 1 do
      case I of
        0: Result.X := StrToFloatDef(Words[I], 0);
        1: Result.Y := StrToFloatDef(Words[I], 0);
        2: Result.Width := StrToFloatDef(Words[I], 0);
        3: Result.Height := StrToFloatDef(Words[I], 0);
      else
        Break;
      end;
  end;

  function ParseArcTo(Kind: TCommandKind; F: IFiler): TCommandData;
  begin
    Result.Reset;
    Result.Kind := Kind;
    Result.Area := ParseRect(F.ReadStr('@rect'));
    Result.BeginAngle := DegToRad(F.ReadFloat('@beginAngle'));
    Result.EndAngle := DegToRad(F.ReadFloat('@endAngle'));
  end;

  function ParsePoint(Rect: string): TPointF;
  var
    Words: StringArray;
    I: Integer;
  begin
    Result := TPointF.Create;
    Words := Rect.Words;
    for I := 0 to Words.Length - 1 do
      case I of
        0: Result.X := StrToFloatDef(Words[I], 0);
        1: Result.Y := StrToFloatDef(Words[I], 0);
      else
        Break;
      end;
  end;

  function ParseCurveTo(Kind: TCommandKind; F: IFiler): TCommandData;
  begin
    Result.Reset;
    Result.Kind := Kind;
    Result.P := ParsePoint(F.ReadStr('@p'));
    Result.C1 := ParsePoint(F.ReadStr('@c1'));
    Result.C2 := ParsePoint(F.ReadStr('@c2'));
  end;

  function ParseEllipse(Kind: TCommandKind; F: IFiler): TCommandData;
  begin
    Result.Reset;
    Result.Kind := Kind;
    Result.Rect := ParseRect(F.ReadStr('@rect'));
  end;

  function ParseRoundRect(Kind: TCommandKind; F: IFiler): TCommandData;
  begin
    Result.Reset;
    Result.Kind := Kind;
    Result.RoundRect := ParseRect(F.ReadStr('@rect'));
    Result.Radius := F.ReadFloat('@radius');
    if Result.Radius < 0 then
      Result.Radius := 0;
  end;

  function ParseText(Kind: TCommandKind; F: IFiler): TCommandData;
  begin
    Result.Reset;
    Result.Kind := Kind;
    Result.Font := F.ReadStr('@font');
    Result.Text := F.ReadStr('@text');
    Result.Insert := ParsePoint(F.ReadStr('@insert'));
  end;

  function ParseRotate(Kind: TCommandKind; F: IFiler): TCommandData;
  var
    S: string;
  begin
    Result.Reset;
    Result.Kind := Kind;
    S := F.ReadStr('@angle');
    if S.IsIdentifier then
    begin
      AddExpression(S, ekFloat);
      Result.Expr[0] := S;
    end
    else
      Result.Angle := DegToRad(F.ReadFloat('@angle'));
  end;

  function ParseStroke(Kind: TCommandKind; F: IFiler): TCommandData;
  begin
    Result.Reset;
    Result.Kind := Kind;
    if Kind = ckFill then
      Result.Resource := F.ReadStr('@brush')
    else
      Result.Resource := F.ReadStr('@pen');
    Result.Preserve := F.ReadBool('@preserve');
  end;

var
  R: INode;
  N: INode;
  L: INodeList;
  F: IFiler;
  S: string;
begin
  Doc := Document;
  Header := Defaults;
  Brushes.Length := 0;
  Pens.Length := 0;
  Fonts.Length := 0;
  Commands.Length := 0;
  R := Doc.Root;
  Valid := (R <> nil) and (R.Name = 'surface');
  if not Valid then
    Exit;
  F := R.Filer;
  Header.Title := F.ReadStr('@title', Defaults.Title, True).Trim;
  Header.Width := F.ReadInt('@width', Defaults.Width, True);
  Header.Height := F.ReadInt('@height', Defaults.Height, True);
  Header.Opacity := F.ReadInt('@opacity', Defaults.Opacity, True);
  Header.Scale := F.ReadFloat('@scale', Defaults.Scale, True);
  Header.Display := F.ReadStr('@display', Defaults.Display, True);
  Header.Normalize;
  L := R.SelectList('resources/brush');
  if L <> nil then
    for N in L do
    begin
      F := N.Filer;
      if F.ReadStr('@name') = '' then
        Continue
      else
        Brushes.Push(ParseBrush(F));
    end;
  L := R.SelectList('resources/pen');
  if L <> nil then
    for N in L do
    begin
      F := N.Filer;
      if F.ReadStr('@name') = '' then
        Continue
      else
        Pens.Push(ParsePen(F, Self));
    end;
  L := R.SelectList('resources/font');
  if L <> nil then
    for N in L do
    begin
      F := N.Filer;
      if F.ReadStr('@name') = '' then
        Continue
      else
        Fonts.Push(ParseFont(F));
    end;
  N := R.SelectNode('commands');
  if N <> nil then
    L := N.Nodes
  else
    L := nil;
  if L <> nil then
    for N in L do
    begin
      S := N.Name;
      F := N.Filer;
      if S = 'pathOpen' then
        Commands.Push(Command(ckPathOpen))
      else if S = 'pathClose' then
        Commands.Push(Command(ckPathClose))
      else if S = 'clip' then
        Commands.Push(Command(ckClip))
      else if S = 'unclip' then
        Commands.Push(Command(ckUnclip))
      else if S = 'statePush' then
        Commands.Push(Command(ckStatePush))
      else if S = 'statePop' then
        Commands.Push(Command(ckStatePop))
      else if S = 'identity' then
        Commands.Push(Command(ckIdentity))
      else if S = 'moveTo' then
        Commands.Push(ParseMoveTo(ckMoveTo, F))
      else if S = 'lineTo' then
        Commands.Push(ParseMoveTo(ckLineTo, F))
      else if S = 'scale' then
        Commands.Push(ParseMoveTo(ckScale, F))
      else if S = 'translate' then
        Commands.Push(ParseMoveTo(ckTranslate, F))
      else if S = 'translate' then
        Commands.Push(ParseMoveTo(ckTranslate, F))
      else if S = 'arcTo' then
        Commands.Push(ParseArcTo(ckArcTo, F))
      else if S = 'curveTo' then
        Commands.Push(ParseCurveTo(ckCurveTo, F))
      else if S = 'ellipse' then
        Commands.Push(ParseEllipse(ckEllipse, F))
      else if S = 'rectangle' then
        Commands.Push(ParseEllipse(ckRectangle, F))
      else if S = 'roundRectangle' then
        Commands.Push(ParseRoundRect(ckRoundRectangle, F))
      else if S = 'text' then
        Commands.Push(ParseText(ckText, F))
      else if S = 'rotate' then
        Commands.Push(ParseRotate(ckRotate, F))
      else if S = 'stroke' then
        Commands.Push(ParseStroke(ckStroke, F))
      else if S = 'fill' then
        Commands.Push(ParseStroke(ckFill, F));
    end;
  Expressions := Added;
end;

function TSurfaceData.GetScale: Float;
begin
  Result := Header.Scale;
end;

function TSurfaceData.FindBrush(const Name: string): IBrush;
var
  I: Integer;
begin
  for I := Brushes.Lo to Brushes.Hi do
    if Name = Brushes[I].Name then
      Exit(Brushes[I].Brush);
  Result := nil;
end;

function TSurfaceData.FindPen(const Name: string): IPen;
var
  I: Integer;
begin
  for I := Pens.Lo to Pens.Hi do
    if Name = Pens[I].Name then
      Exit(Pens[I].Pen);
  Result := nil;
end;

function TSurfaceData.FindFont(const Name: string): IFont;
var
  I: Integer;
begin
  for I := Fonts.Lo to Fonts.Hi do
    if Name = Fonts[I].Name then
      Exit(Fonts[I].Font);
  Result := nil;
end;

procedure TSurfaceData.Render(Surface: ISurface);

  procedure Resolve(var Data: TCommandData);
  var
    I: Integer;
  begin
    if (Data.Expr[0] = '') and (Data.Expr[1] = '') and
      (Data.Expr[2] = '') and (Data.Expr[3] = '') then
      Exit;
    for I := Expressions.Lo to Expressions.Hi do
      Expressions[I].Resolve(Data);
  end;

var
  Data: TCommandData;
  F: IFont;
  B: IBrush;
  P: IPen;
  R: TRectF;
  I: Integer;
begin
  for I := Commands.Lo to Commands.Hi do
  begin
    Data := Commands[I];
    Resolve(Data);
    case Data.Kind of
      ckMoveTo: Surface.MoveTo(Data.X * Scale, Data.Y * Scale);
      ckLineTo: Surface.LineTo(Data.X * Scale, Data.Y * Scale);
      ckArcTo: Surface.ArcTo(Data.Area * Scale, Data.BeginAngle, Data.EndAngle);
      ckCurveTo: Surface.CurveTo(Data.P.X * Scale, Data.P.Y * Scale, Data.C1 * Scale, Data.C2 * Scale);
      ckEllipse: Surface.Ellipse(Data.Rect * Scale);
      ckRectangle: Surface.Rectangle(Data.Rect * Scale);
      ckRoundRectangle: Surface.RoundRectangle(Data.RoundRect * Scale, Data.Radius * Scale);
      ckText:
        begin
          F := FindFont(Data.Font);
          if F = nil then
            Continue;
          R := TRectF.Create(1000, 1000);
          R.Center(Data.Insert);
          Surface.TextOut(F, Data.Text, R, drCenter);
        end;
      ckPathOpen: Surface.Path.Add;
      ckPathClose: Surface.Path.Close;
      ckClip: Surface.Path.Clip;
      ckUnclip: Surface.Path.Unclip;
      ckStatePush: Surface.Save;
      ckStatePop: Surface.Restore;
      ckIdentity: Surface.Matrix.Identity;
      ckRotate: Surface.Matrix.Rotate(Data.Angle);
      ckScale: Surface.Matrix.Scale(Data.X * Scale, Data.Y * Scale);
      ckTranslate: Surface.Matrix.Translate(Data.X * Scale, Data.Y * Scale);
      ckStroke:
        begin
          P := FindPen(Data.Resource);
          if P <> nil then
            Surface.Stroke(P, Data.Preserve);
        end;
      ckFill:
        begin
          B := FindBrush(Data.Resource);
          if B <> nil then
            Surface.Fill(B, Data.Preserve);
        end;
      end;
  end;
end;

function DefaultExpressionCompare(constref A, B: TExpression): Integer;
begin
  Result := StrCompare(A.Name, B.Name);
end;

initialization
  TExpressionArray.DefaultCompare := DefaultExpressionCompare;
end.

