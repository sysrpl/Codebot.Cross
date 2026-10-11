unit Codebot.Render.Widgets.Themes;

{$i render.inc}

interface

uses
  Codebot.System,
  Codebot.Graphics.Types,
  Codebot.Render.Graphics,
  Codebot.Render.Contexts,
  Codebot.Render.Widgets;

{ Themes load their fonts from the fonts folder of the render context assets }

const
  FontRes = 'fonts';

  colorBlack: TColorF = (Blue: 0; Green: 0; Red: 0; Alpha: 1);
  colorWhite: TColorF = (Blue: 1; Green: 1; Red: 1; Alpha: 1);
  colorSilver: TColorF = (Blue: $C0 / $FF; Green: $C0 / $FF; Red: $C0 / $FF; Alpha: 1);

{ TCanvasTheme is the base class of the themes, which draw widgets on a
  canvas. It draws what the themes share and leaves the look of each widget to
  its descendants. }

type
  TCanvasTheme = class(TTheme)
  private
    procedure DrawWindowClose(Window: TWindow);
    procedure DrawWindowGrip(Window: TWindow);
  private
    FCanvas: ICanvas;
    FFont: IFont;
    FTitle: IFont;
    FGlyph: IFont;
    FPen: IPen;
    FTitleHeight: Float;
    FFontHeight: Float;
    FGlyphHeight: Float;
    FHintBrush: ILinearGradientBrush;
  protected
    function MeasureText(Font: IFont; const Text: string): TPointF;
    function MeasureMemo(Font: IFont; const Text: string; Width: Float): Float;
    procedure DrawText(Font: IFont; const Text: string; X, Y: Float);
    procedure DrawTextMemo(Font: IFont; const Text: string; X, Y, Width: Float);
    procedure DrawCaption(Widget: TWidget; const Rect: TRectF; Text: string = ''); virtual;
    { Draw the text, selection, and caret of an edit inside Rect }
    procedure DrawEditText(Edit: TEdit; const Rect: TRectF; const TextColor, SelectColor,
      SelectTextColor: TColorF);
    procedure DrawEdit(Edit: TEdit); virtual; abstract;
    procedure DrawMemo(Memo: TMemo); virtual; abstract;
    { Draw a scroll bar given the rectangle of the bar and of its thumb }
    procedure DrawScrollBar(Widget: TWidget; const Track, Thumb: TRectF;
      Horizontal: Boolean); virtual; abstract;
    { Draw the rows, selection, caret, and scroll bars of a memo }
    procedure DrawMemoText(Memo: TMemo; const TextColor, SelectColor,
      SelectTextColor: TColorF);
    procedure DrawButton(Button: TPushButton); virtual; abstract;
    procedure DrawGlyphButton(GlyphButton: TGlyphButton); virtual; abstract;
    procedure DrawGlyphImage(Widget: TGlyphImage); virtual; abstract;
    procedure DrawCheckBox(CheckBox: TCheckBox); virtual; abstract;
    procedure DrawSlider(Widget: TSlider); virtual; abstract;
    procedure DrawSpinBox(Widget: TSpinBox); virtual; abstract;
    { Draw the frame and background shared by a list box and a scroll grid,
      which themes draw the same as a memo. The colors of text, of the
      selection, and of text in the selection are returned for drawing what
      is inside the frame. }
    procedure DrawListFrame(Widget: TWidget; out TextColor, SelectColor,
      SelectTextColor: TColorF); virtual; abstract;
    { Draw a list box, which is its frame followed by its items }
    procedure DrawListBox(Widget: TListBox);
    { Draw the items, selection, and scroll bar of a list box. The item under
      the mouse is shaded with a fainter selection color. }
    procedure DrawListBoxItems(ListBox: TListBox; const TextColor, SelectColor,
      SelectTextColor: TColorF);
    { Draw a scroll grid. The frame and background are those of a list box,
      each cell which can be seen is drawn by the grid, and the scroll bars
      are drawn last. The theme draws nothing in a cell, selected or not,
      other than the lines between cells when the grid has GridLines set. }
    procedure DrawScrollGrid(Grid: TScrollGrid); virtual;
    { Draw a scroll box, which is the frame and background of a list box when
      it is framed, and its scroll bars. The widgets inside are drawn after it,
      clipped to its client area. }
    procedure DrawScrollBox(Box: TScrollBox); virtual;
    { Draw the header of a scroll grid over the top of its cells, which is
      one header cell for each column which can be seen }
    procedure DrawGridHeader(Grid: TScrollGrid);
    { Draw one header cell of a scroll grid. Rect is on whole pixels. Hot is
      true while the mouse is over the cell and Pressed while it is held
      down on it. Col is -1 for the part of the header past the last column,
      which has no title and is never hot or pressed. The default is a flat
      cell shaded with the active color. Themes draw the cell to match their
      own look, then its title with DrawHeaderText. }
    procedure DrawHeaderCell(Grid: TScrollGrid; const Rect: TRectF; Col: Integer;
      Hot, Pressed: Boolean); virtual;
    { Draw the title of a header cell where the column aligns it, and the
      sort arrow at the right of the cell if the grid is sorted by the
      column. Nothing is drawn when Col is -1. }
    procedure DrawHeaderText(Grid: TScrollGrid; const Rect: TRectF; Col: Integer;
      const Color: TColorF);
    { Draw a line down the right and along the bottom of a header cell }
    procedure DrawHeaderLines(const Rect: TRectF; const Color: TColorF);
    procedure PostDrawSpinBox(Widget: TSpinBox); virtual; abstract;
    { The colors of the scrolling list of a spin box: its frame, its
      background, the item under the mouse, text, and text in the item under
      the mouse }
    procedure DropColors(Widget: TSpinBox; out Frame, Back, Hot, Text,
      HotText: TColorF); virtual;
    { Draw the scrolling list of a spin box of the spinDropScroll kind, with
      the scroll bar of the theme. The chosen item is shaded with a fainter
      color than the item under the mouse. }
    procedure DrawDropScroll(Widget: TSpinBox); virtual;
    procedure DrawLabel(ALabel: TLabel); virtual; abstract;
    procedure DrawWindow(Window: TWindow); virtual; abstract;
    { Draw the close button of a window in Rect, which is CalcCloseRect moved
      to where the window is. The default is a cross on a red circle when the
      mouse is over it. }
    procedure DrawClose(Window: TWindow; const Rect: TRectF; Hot, Pressed: Boolean); virtual;
    { Draw two crossing lines inside a rectangle }
    procedure DrawCross(const Rect: TRectF; const Color: TColorF; Width, Inset: Float);
    { A $RRGGBB color faded by the opacity of the widget }
    function Tint(Widget: TWidget; RGB: LongWord; Alpha: Float = 1): TColorF;
    procedure DrawContainer(Widget: TContainerWidget); virtual; abstract;
  protected
    function FontSize: Float; virtual; abstract;
    function GlyphSize: Float; virtual; abstract;
    function TitleSize: Float; virtual; abstract;
    procedure Init(Canvas: ICanvas); virtual;
    procedure Fixup;
    { A fixed size of the theme multiplied by the text scale of the canvas,
      so widgets grow with their text. Sizes measured from text are already
      scaled and must not be passed. }
    function Scaled(Value: Float): Float; overload;
    function Scaled(const Size: TSizeF): TSizeF; overload;
    property Pen: IPen read FPen;
  public
    procedure Render(Widget: TWidget; Stage: TPaintStage); override;
    procedure RenderHint(Widget: TWidget; Opacity: Float); override;
    function CalcTextHeight: Float; override;
    function CalcTextWidth(Widget: TWidget; const Text: string): Float; override;
    { By default the close button is a square at the right of the title bar }
    function CalcCloseRect(Window: TWindow): TRectF; override;
    procedure PushMatrix(Matrix: IMatrix); override;
    procedure PopMatrix; override;
    procedure PushClip(const Rect: TRectF); override;
    procedure PopClip; override;
    { The canvas the theme draws on }
    property Canvas: ICanvas read FCanvas;
    { The height of a line of text in the theme font }
    property FontHeight: Float read FFontHeight;
    { The font for text }
    property Font: IFont read FFont write FFont;
    { The icon font for glyphs }
    property Glyph: IFont read FGlyph write FGlyph;
    { The font for window titles }
    property Title: IFont read FTitle write FTitle;
  end;

{ TArcDarkTheme is a flat dark theme, and the default theme of a widget scene }

  TArcDarkTheme = class(TCanvasTheme)
  protected
    function FontSize: Float; override;
    function GlyphSize: Float; override;
    function TitleSize: Float; override;
    procedure Init(Canvas: ICanvas); override;
    procedure DrawEdit(Widget: TEdit); override;
    procedure DrawMemo(Widget: TMemo); override;
    procedure DrawListFrame(Widget: TWidget; out TextColor, SelectColor,
      SelectTextColor: TColorF); override;
    procedure DrawScrollBar(Widget: TWidget; const Track, Thumb: TRectF;
      Horizontal: Boolean); override;
    procedure DrawHeaderCell(Grid: TScrollGrid; const Rect: TRectF; Col: Integer;
      Hot, Pressed: Boolean); override;
    procedure DrawButton(Widget: TPushButton); override;
    procedure DrawGlyphButton(Widget: TGlyphButton); override;
    procedure DrawGlyphImage(Widget: TGlyphImage); override;
    procedure DrawCheckBox(Widget: TCheckBox); override;
    procedure DrawSlider(Widget: TSlider); override;
    procedure DrawSpinBox(Widget: TSpinBox); override;
    procedure PostDrawSpinBox(Widget: TSpinBox); override;
    procedure DropColors(Widget: TSpinBox; out Frame, Back, Hot, Text,
      HotText: TColorF); override;
    procedure DrawLabel(Widget: TLabel); override;
    procedure DrawWindow(Widget: TWindow); override;
    procedure DrawClose(Window: TWindow; const Rect: TRectF; Hot, Pressed: Boolean); override;
    procedure DrawContainer(Widget: TContainerWidget); override;
  public
    { The color of a theme color as $AARRGGBB }
    function Palette(Widget: TWidget; Color: TThemeColor): LongWord;
    function CalcColor(Widget: TWidget; Color: TThemeColor): TColorF; override;
    function CalcSize(Widget: TWidget; Part: TThemePart): TSizeF; override;
    function CalcCloseRect(Window: TWindow): TRectF; override;
  end;

{ TChicagoTheme looks like Windows 95 }

  TChicagoTheme = class(TCanvasTheme)
  private
    FFocus: IPen;
  protected
    function FontSize: Float; override;
    function GlyphSize: Float; override;
    function TitleSize: Float; override;
    procedure DrawCaption(Widget: TWidget; const Rect: TRectF; Text: string = ''); override;
    procedure DrawFocus(Widget: TWidget; const Rect: TRectF);
    procedure DrawThinBorder(Widget: TWidget; const Rect: TRectF);
    procedure DrawSunken(Widget: TWidget; const Rect: TRectF);
    procedure DrawThickBorder(Widget: TWidget);
    procedure Init(Canvas: ICanvas); override;
    procedure DrawEdit(Widget: TEdit); override;
    procedure DrawMemo(Widget: TMemo); override;
    procedure DrawListFrame(Widget: TWidget; out TextColor, SelectColor,
      SelectTextColor: TColorF); override;
    procedure DrawScrollBar(Widget: TWidget; const Track, Thumb: TRectF;
      Horizontal: Boolean); override;
    procedure DrawHeaderCell(Grid: TScrollGrid; const Rect: TRectF; Col: Integer;
      Hot, Pressed: Boolean); override;
    procedure DrawButton(Widget: TPushButton); override;
    procedure DrawGlyphButton(Widget: TGlyphButton); override;
    procedure DrawGlyphImage(Widget: TGlyphImage); override;
    procedure DrawCheckBox(Widget: TCheckBox); override;
    procedure DrawSlider(Widget: TSlider); override;
    procedure DrawSpinBox(Widget: TSpinBox); override;
    procedure PostDrawSpinBox(Widget: TSpinBox); override;
    procedure DropColors(Widget: TSpinBox; out Frame, Back, Hot, Text,
      HotText: TColorF); override;
    procedure DrawLabel(Widget: TLabel); override;
    procedure DrawWindow(Widget: TWindow); override;
    procedure DrawClose(Window: TWindow; const Rect: TRectF; Hot, Pressed: Boolean); override;
    procedure DrawContainer(Widget: TContainerWidget); override;
  public
    { The color of a theme color as $AARRGGBB }
    function Palette(Widget: TWidget; Color: TThemeColor): LongWord;
    function CalcColor(Widget: TWidget; Color: TThemeColor): TColorF; override;
    function CalcSize(Widget: TWidget; Part: TThemePart): TSizeF; override;
    function CalcCloseRect(Window: TWindow): TRectF; override;
  end;

{ TGraphiteTheme is a light gray theme with soft gradients }

  TGraphiteTheme = class(TCanvasTheme)
  private
    FBrush: ILinearGradientBrush;
  protected
    procedure Init(Canvas: ICanvas); override;
    function FontSize: Float; override;
    function GlyphSize: Float; override;
    function TitleSize: Float; override;
    procedure DrawEdit(Widget: TEdit); override;
    procedure DrawMemo(Widget: TMemo); override;
    procedure DrawListFrame(Widget: TWidget; out TextColor, SelectColor,
      SelectTextColor: TColorF); override;
    procedure DrawScrollBar(Widget: TWidget; const Track, Thumb: TRectF;
      Horizontal: Boolean); override;
    procedure DrawHeaderCell(Grid: TScrollGrid; const Rect: TRectF; Col: Integer;
      Hot, Pressed: Boolean); override;
    procedure DrawButton(Widget: TPushButton); override;
    procedure DrawGlyphButton(Widget: TGlyphButton); override;
    procedure DrawGlyphImage(Widget: TGlyphImage); override;
    procedure DrawCheckBox(Widget: TCheckBox); override;
    procedure DrawSlider(Widget: TSlider); override;
    procedure DrawSpinBox(Widget: TSpinBox); override;
    procedure PostDrawSpinBox(Widget: TSpinBox); override;
    procedure DropColors(Widget: TSpinBox; out Frame, Back, Hot, Text,
      HotText: TColorF); override;
    procedure DrawLabel(Widget: TLabel); override;
    procedure DrawWindow(Widget: TWindow); override;
    procedure DrawClose(Window: TWindow; const Rect: TRectF; Hot, Pressed: Boolean); override;
    procedure DrawContainer(Widget: TContainerWidget); override;
  public
    { The color of a theme color as $AARRGGBB }
    function Palette(Widget: TWidget; Color: TThemeColor): LongWord;
    function CalcColor(Widget: TWidget; Color: TThemeColor): TColorF; override;
    function CalcSize(Widget: TWidget; Part: TThemePart): TSizeF; override;
    function CalcCloseRect(Window: TWindow): TRectF; override;
  end;

{ TDesktopTheme holds what the desktop styled themes below share: sizes,
  fonts, popup lists, and drawing helpers. It is not a theme on its own. }

  TDesktopTheme = class(TCanvasTheme)
  private
    FBrush: ILinearGradientBrush;
  protected
    { The color of a theme color as $RRGGBB }
    function Palette(Widget: TWidget; Color: TThemeColor): LongWord; virtual; abstract;
    function CaptionHeight: Float; virtual; abstract;
    function ButtonHeight: Float; virtual; abstract;
    function ThumbSize: TSizeF; virtual; abstract;
    function WindowRadius: Float; virtual; abstract;
    { The thickness of the window frame, X for the sides and Y for the bottom }
    function WindowBorder: TSizeF; virtual;
    { A $RRGGBB color faded by the opacity of the widget }
    function Shade(Widget: TWidget; RGB: LongWord; Alpha: Float = 1): TColorF;
    { Set up the gradient brush to run down or across a rectangle }
    procedure VertGradient(const R: TRectF; const Top, Bottom: TColorF);
    procedure HorzGradient(const R: TRectF; const Left, Right: TColorF);
    procedure CheckMark(const R: TRectF; const C: TColorF; Width: Float);
    function CheckRect(Widget: TCheckBox; Size: Float): TRectF;
    procedure CheckCaption(Widget: TCheckBox; Size: Float);
    procedure SpinArrows(Widget: TSpinBox; const R: TRectF; const C: TColorF);
    procedure GripLines(const R: TRectF; Horizontal: Boolean; const C: TColorF);
    procedure LoadFonts(const FontName, FontFile, TitleName, TitleFile: string);
    function GlyphSize: Float; override;
    procedure Init(Canvas: ICanvas); override;
    procedure DrawGlyphButton(Widget: TGlyphButton); override;
    procedure DrawGlyphImage(Widget: TGlyphImage); override;
    procedure PostDrawSpinBox(Widget: TSpinBox); override;
    procedure DrawLabel(Widget: TLabel); override;
    procedure DrawContainer(Widget: TContainerWidget); override;
  public
    function CalcColor(Widget: TWidget; Color: TThemeColor): TColorF; override;
    function CalcSize(Widget: TWidget; Part: TThemePart): TSizeF; override;
  end;

{ TExperienceTheme looks like Windows XP }

  TExperienceTheme = class(TDesktopTheme)
  protected
    function Palette(Widget: TWidget; Color: TThemeColor): LongWord; override;
    function CaptionHeight: Float; override;
    function ButtonHeight: Float; override;
    function ThumbSize: TSizeF; override;
    function WindowRadius: Float; override;
    function WindowBorder: TSizeF; override;
    function FontSize: Float; override;
    function TitleSize: Float; override;
    procedure Init(Canvas: ICanvas); override;
    procedure DrawEdit(Widget: TEdit); override;
    procedure DrawMemo(Widget: TMemo); override;
    procedure DrawListFrame(Widget: TWidget; out TextColor, SelectColor,
      SelectTextColor: TColorF); override;
    procedure DrawScrollBar(Widget: TWidget; const Track, Thumb: TRectF;
      Horizontal: Boolean); override;
    procedure DrawHeaderCell(Grid: TScrollGrid; const Rect: TRectF; Col: Integer;
      Hot, Pressed: Boolean); override;
    procedure DrawButton(Widget: TPushButton); override;
    procedure DrawCheckBox(Widget: TCheckBox); override;
    procedure DrawSlider(Widget: TSlider); override;
    procedure DrawSpinBox(Widget: TSpinBox); override;
    procedure DrawWindow(Widget: TWindow); override;
    procedure DrawClose(Window: TWindow; const Rect: TRectF; Hot, Pressed: Boolean); override;
  public
    function CalcCloseRect(Window: TWindow): TRectF; override;
  end;

{ TVistaTheme looks like Windows 7 }

  TVistaTheme = class(TDesktopTheme)
  protected
    function Palette(Widget: TWidget; Color: TThemeColor): LongWord; override;
    function CaptionHeight: Float; override;
    function ButtonHeight: Float; override;
    function ThumbSize: TSizeF; override;
    function WindowRadius: Float; override;
    function WindowBorder: TSizeF; override;
    function FontSize: Float; override;
    function TitleSize: Float; override;
    procedure Init(Canvas: ICanvas); override;
    procedure ButtonFace(Widget: TWidget; const R: TRectF);
    procedure DrawEdit(Widget: TEdit); override;
    procedure DrawMemo(Widget: TMemo); override;
    procedure DrawListFrame(Widget: TWidget; out TextColor, SelectColor,
      SelectTextColor: TColorF); override;
    procedure DrawScrollBar(Widget: TWidget; const Track, Thumb: TRectF;
      Horizontal: Boolean); override;
    procedure DrawHeaderCell(Grid: TScrollGrid; const Rect: TRectF; Col: Integer;
      Hot, Pressed: Boolean); override;
    procedure DrawButton(Widget: TPushButton); override;
    procedure DrawCheckBox(Widget: TCheckBox); override;
    procedure DrawSlider(Widget: TSlider); override;
    procedure DrawSpinBox(Widget: TSpinBox); override;
    procedure DrawWindow(Widget: TWindow); override;
    procedure DrawClose(Window: TWindow; const Rect: TRectF; Hot, Pressed: Boolean); override;
  public
    function CalcCloseRect(Window: TWindow): TRectF; override;
  end;

{ TCupertinoTheme looks like macOS }

  TCupertinoTheme = class(TDesktopTheme)
  protected
    function Palette(Widget: TWidget; Color: TThemeColor): LongWord; override;
    function CaptionHeight: Float; override;
    function ButtonHeight: Float; override;
    function ThumbSize: TSizeF; override;
    function WindowRadius: Float; override;
    function FontSize: Float; override;
    function TitleSize: Float; override;
    procedure Init(Canvas: ICanvas); override;
    procedure FocusRing(Widget: TWidget; const R: TRectF; Radius: Float);
    procedure DrawEdit(Widget: TEdit); override;
    procedure DrawMemo(Widget: TMemo); override;
    procedure DrawListFrame(Widget: TWidget; out TextColor, SelectColor,
      SelectTextColor: TColorF); override;
    procedure DrawScrollBar(Widget: TWidget; const Track, Thumb: TRectF;
      Horizontal: Boolean); override;
    procedure DrawHeaderCell(Grid: TScrollGrid; const Rect: TRectF; Col: Integer;
      Hot, Pressed: Boolean); override;
    procedure DrawButton(Widget: TPushButton); override;
    procedure DrawCheckBox(Widget: TCheckBox); override;
    procedure DrawSlider(Widget: TSlider); override;
    procedure DrawSpinBox(Widget: TSpinBox); override;
    procedure DrawWindow(Widget: TWindow); override;
    procedure DrawClose(Window: TWindow; const Rect: TRectF; Hot, Pressed: Boolean); override;
  public
    function CalcCloseRect(Window: TWindow): TRectF; override;
  end;

{ Create a theme of a class which draws on a canvas }
function NewTheme(Canvas: ICanvas; ThemeClass: TThemeClass): TTheme;

implementation

function NewTheme(Canvas: ICanvas; ThemeClass: TThemeClass): TTheme;
begin
  Result := ThemeClass.Create;
  if Result is TCanvasTheme then
  begin
    TCanvasTheme(Result).Init(Canvas);
    TCanvasTheme(Result).Fixup;
  end;
end;

{ TCanvasTheme }

procedure TCanvasTheme.Init(Canvas: ICanvas);
begin
  FCanvas := Canvas;
  FPen := NewPen(colorBlack);
  FHintBrush := NewBrush(NewPointF(0, 0), NewPointF(10, 0));
end;

procedure TCanvasTheme.Fixup;
begin
  Font.Size := FontSize;
  Title.Size := TitleSize;
  Glyph.Size := GlyphSize;
  FFontHeight := Round(Canvas.MeasureText(Font, 'Wg').Y);
  FTitleHeight := Round(Canvas.MeasureText(Title, 'Wg').Y);
  FGlyphHeight := Round(Canvas.MeasureText(Glyph, 'Wg').Y);
end;

function TCanvasTheme.Scaled(Value: Float): Float;
begin
  Result := Value * Canvas.TextScale;
end;

function TCanvasTheme.Scaled(const Size: TSizeF): TSizeF;
begin
  Result := NewPointF(Size.X * Canvas.TextScale, Size.Y * Canvas.TextScale);
end;

function TCanvasTheme.MeasureText(Font: IFont; const Text: string): TPointF;
begin
  Result := Canvas.MeasureText(Font, Text);
end;

function TCanvasTheme.MeasureMemo(Font: IFont; const Text: string; Width: Float): Float;
begin
  Result := Canvas.MeasureMemo(Font, Text, Width);
end;

procedure TCanvasTheme.DrawText(Font: IFont; const Text: string; X, Y: Float);
begin
  Canvas.DrawText(Font, Text, X, Y);
end;

procedure TCanvasTheme.DrawTextMemo(Font: IFont; const Text: string; X, Y, Width: Float);
begin
  Canvas.DrawTextMemo(Font, Text, X, Y, Width);
end;

procedure TCanvasTheme.DrawCaption(Widget: TWidget; const Rect: TRectF; Text: string = '');
var
  S, W: Float;
  F: IFont;
  P: TPointF;
begin
  if Text = '' then
  	Text := Widget.Text;
  S := 0;
  W := 0;
  Font.Align := fontCenter;
  Font.Layout := fontMiddle;
  Font.Size := FontSize;
  Title.Align := fontCenter;
  Title.Layout := fontMiddle;
  Title.Size := TitleSize;
  Glyph.Align := fontCenter;
  Glyph.Layout := fontMiddle;
  Glyph.Size := GlyphSize;
  if Widget is TWindow then
  begin
    F := Title;
    F.Color := CalcColor(Widget, colorTitle);
  end
  else if Widget is TGlyphButton then
  begin
    F := Glyph;
    F.Color := CalcColor(Widget, colorText);
  end
  else if Widget is TGlyphImage then
  begin
    F := Glyph;
    F.Color := CalcColor(Widget, colorText);
    S := F.Size;
    F.Size := Rect.Height / Canvas.TextScale;
  end
  else
  begin
    F := Font;
    F.Color := CalcColor(Widget, colorText);
  end;
  if Widget is TLabel then
  begin
    F.Align := fontLeft;
    W := TLabel(Widget).MaxWidth;
    if W < 1 then
      P.Y := Rect.MidPoint.Y
    else
      P.Y := Rect.Top + Round(FFontHeight / 2) - 5;
    P.X := Rect.X;
  end
  else
  begin
    F.Align := fontCenter;
    P := Rect.MidPoint;
  end;
  if W < 1 then
    DrawText(F, Text, P.X, P.Y)
  else
  begin
    F.Layout := fontTop;
    DrawTextMemo(F, Text, P.X, P.Y, W);
  end;
  if S > 0 then
    F.Size := S;
end;

procedure TCanvasTheme.Render(Widget: TWidget; Stage: TPaintStage);
begin
  if Stage = prePaint then
  begin
    if Widget is TEdit then
      DrawEdit(TEdit(Widget))
    else if Widget is TMemo then
      DrawMemo(TMemo(Widget))
    else if Widget is TPushButton then
      DrawButton(TPushButton(Widget))
    else if Widget is TGlyphButton then
      DrawGlyphButton(TGlyphButton(Widget))
    else if Widget is TGlyphImage then
      DrawGlyphImage(TGlyphImage(Widget))
    else if Widget is TCheckBox then
      DrawCheckBox(TCheckBox(Widget))
    else if Widget is TSlider then
      DrawSlider(TSlider(Widget))
    else if Widget is TLabel then
    begin
      if not TLabel(Widget).OwnerDraw then
        DrawLabel(TLabel(Widget));
    end
    else if Widget is TSpinBox then
      DrawSpinBox(TSpinBox(Widget))
    else if Widget is TListBox then
      DrawListBox(TListBox(Widget))
    else if Widget is TScrollGrid then
      DrawScrollGrid(TScrollGrid(Widget))
    else if Widget is TScrollBox then
      DrawScrollBox(TScrollBox(Widget))
    else if Widget is TWindow then
    begin
      DrawWindow(TWindow(Widget));
      DrawWindowClose(TWindow(Widget));
      DrawWindowGrip(TWindow(Widget));
    end
    else if Widget is TContainerWidget then
      DrawContainer(TContainerWidget(Widget));
	end;
  if Stage = postPaint then
  begin
    if Widget is TSpinBox then
      if TSpinBox(Widget).Kind = spinDropScroll then
        DrawDropScroll(TSpinBox(Widget))
      else
        PostDrawSpinBox(TSpinBox(Widget));
	end;
end;

{ The matrix is applied first and the transform of the canvas after it, so a
  transformed widget is still inside the transform of the scene }

procedure TCanvasTheme.PushMatrix(Matrix: IMatrix);
var
  M: IMatrix;
begin
  M := NewMatrix;
  M.Copy(Matrix);
  M.Transform(Canvas.Matrix);
  Canvas.Matrix.Push;
  Canvas.Matrix.Copy(M);
end;

procedure TCanvasTheme.PopMatrix;
begin
  Canvas.Matrix.Pop;
end;

procedure TCanvasTheme.PushClip(const Rect: TRectF);
begin
  Canvas.Push;
  Canvas.Clip(Rect);
end;

procedure TCanvasTheme.PopClip;
begin
  Canvas.Pop;
end;

procedure TCanvasTheme.DrawScrollBox(Box: TScrollBox);
var
  Bounds, Track, Thumb: TRectF;
  TextColor, SelectColor, SelectTextColor: TColorF;
  Bar: TMemoBar;
begin
  if Box.Framed then
    DrawListFrame(Box, TextColor, SelectColor, SelectTextColor);
  Bounds := Box.Computed.Bounds;
  for Bar := Low(TMemoBar) to High(TMemoBar) do
    if Box.BarVisible(Bar) then
    begin
      Track := Box.BarRect(Bar);
      Thumb := Box.ThumbRect(Bar);
      Track.Offset(Bounds.X, Bounds.Y);
      Thumb.Offset(Bounds.X, Bounds.Y);
      DrawScrollBar(Box, Track, Thumb, Bar = barHorz);
    end;
end;

procedure TCanvasTheme.DropColors(Widget: TSpinBox; out Frame, Back, Hot, Text,
  HotText: TColorF);
begin
  Frame := CalcColor(Widget, colorDarkShadow);
  Back := CalcColor(Widget, colorHighlight);
  Hot := CalcColor(Widget, colorActive);
  Text := CalcColor(Widget, colorText);
  HotText := CalcColor(Widget, colorSelected);
end;

procedure TCanvasTheme.DrawDropScroll(Widget: TSpinBox);
var
  Bounds, R, Area, Track, Thumb: TRectF;
  Frame, Back, Hot, Text, HotText, C: TColorF;
  P: TPointF;
  H: Float;
  First, Last, Over, I: Integer;
begin
  if not Widget.Dropped then
    Exit;
  R := Widget.DropRect;
  if R.Empty then
    Exit;
  H := Widget.DropItemHeight;
  if H < 1 then
    Exit;
  DropColors(Widget, Frame, Back, Hot, Text, HotText);
  Bounds := Widget.Computed.Bounds.Round;
  R.Offset(Bounds.X, Bounds.Y);
  Canvas.Rect(R);
  Canvas.Fill(Frame);
  R.Inflate(-1, -1);
  Canvas.Rect(R);
  Canvas.Fill(Back);
  Area := Widget.DropArea;
  { The item under the mouse, unless the scroll bar is being dragged }
  P := Widget.Main.MouseFor(Widget);
  R := Widget.Computed.Bounds;
  Over := Widget.DropItemFromPoint(P.X - R.X, P.Y - R.Y);
  Font.Size := FontSize;
  Font.Align := fontCenter;
  Font.Layout := fontMiddle;
  First := Trunc(Widget.DropScroll / H);
  Last := Trunc((Widget.DropScroll + Area.Height) / H);
  if Last > Widget.Items.Length - 1 then
    Last := Widget.Items.Length - 1;
  Area.Offset(Bounds.X, Bounds.Y);
  Canvas.Push;
  try
    Canvas.Clip(Area);
    for I := First to Last do
    begin
      R := Widget.DropItemRect(I);
      R.Offset(Bounds.X, Bounds.Y);
      Font.Color := Text;
      if I = Over then
      begin
        Canvas.Rect(R);
        Canvas.Fill(Hot);
        Font.Color := HotText;
      end
      else if I = Widget.ItemIndex then
      begin
        C := Hot;
        C.Alpha := C.Alpha * 0.35;
        Canvas.Rect(R);
        Canvas.Fill(C);
      end;
      DrawText(Font, Widget.Items[I], R.MidPoint.X, R.MidPoint.Y);
    end;
  finally
    Canvas.Pop;
  end;
  if Widget.DropBarVisible then
  begin
    Track := Widget.DropBarRect;
    Track.Offset(Bounds.X, Bounds.Y);
    Thumb := Widget.DropThumbRect;
    Thumb.Offset(Bounds.X, Bounds.Y);
    DrawScrollBar(Widget, Track, Thumb, False);
  end;
end;

function TCanvasTheme.Tint(Widget: TWidget; RGB: LongWord; Alpha: Float = 1): TColorF;
begin
  Result := ARGB($FF000000 or (RGB and $FFFFFF));
  Result.Alpha := Alpha * Widget.Computed.Opacity;
end;

function TCanvasTheme.CalcCloseRect(Window: TWindow): TRectF;
const
  Size = 16;
var
  H: Float;
begin
  H := CalcSize(Window, tpCaption).Y;
  Result := NewRectF(Window.Width - Size - Trunc((H - Size) / 2), Trunc((H - Size) / 2),
    Size, Size);
end;

{ The size grip of a window which can be resized is three slanted lines in
  the bottom right corner }

procedure TCanvasTheme.DrawWindowGrip(Window: TWindow);
var
  R, B: TRectF;
  C: TColorF;
  I: Integer;
begin
  R := Window.SizeRect;
  if R.Empty then
    Exit;
  B := Window.Computed.Bounds;
  R.Offset(Trunc(B.X), Trunc(B.Y));
  for I := 1 to 3 do
  begin
    Canvas.MoveTo(R.X + R.Width - 3, R.Y + R.Height - 3 - I * 4 + 2);
    Canvas.LineTo(R.X + R.Width - 3 - I * 4 + 2, R.Y + R.Height - 3);
  end;
  C := CalcColor(Window, colorText);
  C.Alpha := C.Alpha * 0.5;
  Canvas.Stroke(C, 1.5);
end;

procedure TCanvasTheme.DrawWindowClose(Window: TWindow);
var
  R, B: TRectF;
begin
  if not Window.CloseButton then
    Exit;
  R := Window.CloseRect;
  if R.Empty then
    Exit;
  B := Window.Computed.Bounds;
  R.Offset(Trunc(B.X), Trunc(B.Y));
  DrawClose(Window, R, Window.CloseHot, Window.ClosePressed);
end;

procedure TCanvasTheme.DrawCross(const Rect: TRectF; const Color: TColorF; Width, Inset: Float);
var
  R: TRectF;
begin
  R := Rect;
  R.Inflate(-Inset, -Inset);
  Pen.Color := Color;
  Pen.Width := Width;
  Pen.LineCap := capRound;
  Pen.LineJoin := joinRound;
  Canvas.MoveTo(R.Left, R.Top);
  Canvas.LineTo(R.Right, R.Bottom);
  Canvas.MoveTo(R.Right, R.Top);
  Canvas.LineTo(R.Left, R.Bottom);
  Canvas.Stroke(Pen);
end;

procedure TCanvasTheme.DrawClose(Window: TWindow; const Rect: TRectF; Hot, Pressed: Boolean);
var
  P: TPointF;
begin
  if Hot then
  begin
    P := Rect.MidPoint;
    Canvas.Circle(P.X, P.Y, Rect.Width / 2 + 1);
    if Pressed then
      Canvas.Fill(Tint(Window, $B03030))
    else
      Canvas.Fill(Tint(Window, $E04040));
    DrawCross(Rect, Tint(Window, $FFFFFF), 1.5, 4.5);
  end
  else
    DrawCross(Rect, CalcColor(Window, colorTitle), 1.5, 4.5);
end;

function TCanvasTheme.CalcTextWidth(Widget: TWidget; const Text: string): Float;
begin
  Font.Size := FontSize;
  Result := Canvas.MeasureAdvance(Font, Text);
end;

procedure TCanvasTheme.DrawEditText(Edit: TEdit; const Rect: TRectF; const TextColor,
  SelectColor, SelectTextColor: TColorF);
var
  Inner, Clip, Sel: TRectF;
  X, Y, A, B: Float;
  Selected: Boolean;
begin
  Inner := Rect;
  Inner.Inflate(-EditPadding, 0);
  Font.Size := FontSize;
  Font.Align := fontLeft;
  Font.Layout := fontMiddle;
  Edit.ScrollToCaret(Inner.Width);
  { The selection is only shown while the edit has focus }
  Selected := (Edit.SelLength > 0) and (wsSelected in Edit.State);
  X := Inner.X - Edit.Scroll;
  Y := Inner.MidPoint.Y;
  Sel := Default(TRectF);
  Canvas.Push;
  try
    { Text scrolled out of view is clipped to the inside of the edit }
    Clip := Rect;
    Clip.Inflate(-2, -2);
    Canvas.Clip(Clip);
    if Selected then
    begin
      A := X + Edit.OffsetOf(Edit.SelStart);
      B := X + Edit.OffsetOf(Edit.SelStart + Edit.SelLength);
      Sel := NewRectF(A, Rect.Y + 4, B - A, Rect.Height - 8);
      Canvas.Rect(Sel);
      Canvas.Fill(SelectColor);
    end;
    Font.Color := TextColor;
    DrawText(Font, Edit.Text, X, Y);
    if Selected then
    begin
      { Draw the selected part of the text again in the selection color }
      Canvas.Clip(Sel);
      Font.Color := SelectTextColor;
      DrawText(Font, Edit.Text, X, Y);
    end;
  finally
    Canvas.Pop;
  end;
  if Edit.CaretVisible then
  begin
    A := Round(X + Edit.OffsetOf(Edit.Caret)) + 0.5;
    Canvas.MoveTo(A, Rect.Y + 5);
    Canvas.LineTo(A, Rect.Bottom - 5);
    Canvas.Stroke(TextColor, 1);
  end;
end;

function TCanvasTheme.CalcTextHeight: Float;
begin
  Font.Size := FontSize;
  Result := Canvas.MeasureText(Font, 'Wg').Y;
end;

procedure TCanvasTheme.DrawMemoText(Memo: TMemo; const TextColor, SelectColor,
  SelectTextColor: TColorF);
var
  Bounds, Area, Sel, Track, Thumb: TRectF;
  First, Last, I: Integer;
  X, Y, H, A, B: Float;
  P: TPointF;
  Recolor, Selected: Boolean;
begin
  Memo.Prepare;
  H := Memo.RowHeight;
  if H < 1 then
    Exit;
  Bounds := Memo.Computed.Bounds;
  Bounds.X := Trunc(Bounds.X);
  Bounds.Y := Trunc(Bounds.Y);
  Area := Memo.TextArea;
  Area.Offset(Bounds.X, Bounds.Y);
  Font.Size := FontSize;
  Font.Align := fontLeft;
  Font.Layout := fontMiddle;
  { Only the rows which can be seen are drawn }
  First := Trunc(Memo.ScrollY / H);
  Last := Trunc((Memo.ScrollY + Area.Height) / H);
  if Last > Memo.RowCount - 1 then
    Last := Memo.RowCount - 1;
  X := Area.X - Memo.ScrollX;
  { The selection is only shown while the memo has focus }
  Selected := wsSelected in Memo.State;
  Recolor := (SelectTextColor.Red <> TextColor.Red) or (SelectTextColor.Green <> TextColor.Green) or
    (SelectTextColor.Blue <> TextColor.Blue);
  Canvas.Push;
  try
    Canvas.Clip(Area);
    for I := First to Last do
    begin
      Y := Area.Y + I * H - Memo.ScrollY;
      if Selected and Memo.RowSelection(I, A, B) then
      begin
        Canvas.Rect(X + A, Y, B - A, H);
        Canvas.Fill(SelectColor);
      end;
      Font.Color := TextColor;
      DrawText(Font, Memo.RowText(I), X, Y + H / 2);
    end;
    { Draw the selected text again in the selection text color }
    if Recolor and Selected then
      for I := First to Last do
        if Memo.RowSelection(I, A, B) then
        begin
          Y := Area.Y + I * H - Memo.ScrollY;
          Sel := NewRectF(X + A, Y, B - A, H);
          Canvas.Push;
          try
            Canvas.Clip(Sel);
            Font.Color := SelectTextColor;
            DrawText(Font, Memo.RowText(I), X, Y + H / 2);
          finally
            Canvas.Pop;
          end;
        end;
  finally
    Canvas.Pop;
  end;
  if Memo.CaretVisible then
  begin
    P := Memo.CaretPoint;
    A := Round(X + P.X) + 0.5;
    Y := Area.Y + P.Y - Memo.ScrollY;
    if (Y + H > Area.Top) and (Y < Area.Bottom) and (A >= Area.Left - 1) and (A <= Area.Right + 1) then
    begin
      B := Y + H;
      if Y < Area.Top then
        Y := Area.Top;
      if B > Area.Bottom then
        B := Area.Bottom;
      Canvas.MoveTo(A, Y + 1);
      Canvas.LineTo(A, B - 1);
      Canvas.Stroke(TextColor, 1);
    end;
  end;
  if Memo.BarVisible(barVert) then
  begin
    Track := Memo.BarRect(barVert);
    Thumb := Memo.ThumbRect(barVert);
    Track.Offset(Bounds.X, Bounds.Y);
    Thumb.Offset(Bounds.X, Bounds.Y);
    DrawScrollBar(Memo, Track, Thumb, False);
  end;
  if Memo.BarVisible(barHorz) then
  begin
    Track := Memo.BarRect(barHorz);
    Thumb := Memo.ThumbRect(barHorz);
    Track.Offset(Bounds.X, Bounds.Y);
    Thumb.Offset(Bounds.X, Bounds.Y);
    DrawScrollBar(Memo, Track, Thumb, True);
  end;
end;

procedure TCanvasTheme.DrawListBox(Widget: TListBox);
var
  TextColor, SelectColor, SelectTextColor: TColorF;
begin
  DrawListFrame(Widget, TextColor, SelectColor, SelectTextColor);
  DrawListBoxItems(Widget, TextColor, SelectColor, SelectTextColor);
end;

procedure TCanvasTheme.DrawListBoxItems(ListBox: TListBox; const TextColor,
  SelectColor, SelectTextColor: TColorF);
var
  Bounds, Area, R, Track, Thumb: TRectF;
  First, Last, I: Integer;
  H: Float;
  C: TColorF;
begin
  H := ListBox.ItemHeight;
  if H < 1 then
    Exit;
  Bounds := ListBox.Computed.Bounds;
  Bounds.X := Trunc(Bounds.X);
  Bounds.Y := Trunc(Bounds.Y);
  Area := ListBox.ItemArea;
  Area.Offset(Bounds.X, Bounds.Y);
  Font.Size := FontSize;
  Font.Align := fontLeft;
  Font.Layout := fontMiddle;
  { Only the items which can be seen are drawn }
  First := Trunc(ListBox.ScrollY / H);
  Last := Trunc((ListBox.ScrollY + Area.Height) / H);
  if Last > ListBox.Count - 1 then
    Last := ListBox.Count - 1;
  Canvas.Push;
  try
    Canvas.Clip(Area);
    for I := First to Last do
    begin
      R := ListBox.ItemRect(I);
      R.Offset(Bounds.X, Bounds.Y);
      if I = ListBox.ItemIndex then
      begin
        Canvas.Rect(R);
        Canvas.Fill(SelectColor);
        Font.Color := SelectTextColor;
      end
      else
      begin
        if (I = ListBox.HotIndex) and (wsHot in ListBox.State) then
        begin
          C := SelectColor;
          C.Alpha := C.Alpha * 0.3;
          Canvas.Rect(R);
          Canvas.Fill(C);
        end;
        Font.Color := TextColor;
      end;
      DrawText(Font, ListBox.Items[I], R.X + 4, R.Y + H / 2);
    end;
  finally
    Canvas.Pop;
  end;
  if ListBox.BarVisible then
  begin
    Track := ListBox.BarRect;
    Thumb := ListBox.ThumbRect;
    Track.Offset(Bounds.X, Bounds.Y);
    Thumb.Offset(Bounds.X, Bounds.Y);
    DrawScrollBar(ListBox, Track, Thumb, False);
  end;
end;

procedure TCanvasTheme.DrawScrollGrid(Grid: TScrollGrid);
var
  Bounds, Area, R, Track, Thumb: TRectF;
  TextColor, SelectColor, SelectTextColor, C: TColorF;
  FirstCol, LastCol, FirstRow, LastRow, Col, Row: Integer;
  P: Float;
begin
  { The frame and background are those of a list box, so a cell which is not
    selected looks the same as an item which is not selected }
  DrawListFrame(Grid, TextColor, SelectColor, SelectTextColor);
  { Nothing is drawn in a cell by the theme, not even for the selected cell.
    The grid is given the colors of a list box so that the cells can be drawn
    to match one. }
  Grid.TextColor := TextColor;
  Grid.SelectColor := SelectColor;
  Grid.SelectTextColor := SelectTextColor;
  Bounds := Grid.Computed.Bounds.Round;
  Area := Grid.CellArea;
  Area.Offset(Bounds.X, Bounds.Y);
  if (Grid.ColCount > 0) and (Grid.RowCount > 0) then
  begin
    { Only the cells which can be seen are drawn }
    FirstCol := Grid.ColFromOffset(Grid.ScrollX);
    LastCol := Grid.ColFromOffset(Grid.ScrollX + Area.Width);
    if LastCol > Grid.ColCount - 1 then
      LastCol := Grid.ColCount - 1;
    FirstRow := Trunc(Grid.ScrollY / Grid.RowHeight);
    LastRow := Trunc((Grid.ScrollY + Area.Height) / Grid.RowHeight);
    if LastRow > Grid.RowCount - 1 then
      LastRow := Grid.RowCount - 1;
    Canvas.Push;
    try
      Canvas.Clip(Area);
      for Row := FirstRow to LastRow do
        for Col := FirstCol to LastCol do
        begin
          R := Grid.CellRect(Col, Row);
          R.Offset(Bounds.X, Bounds.Y);

          { Drawing by the grid is clipped to the cell }
          Canvas.Push;
          try
            Canvas.Clip(R);
            Grid.DrawCell(Canvas, Row, Col, R);
          finally
            Canvas.Pop;
          end;
        end;
      if Grid.GridLines then
      begin
        C := CalcColor(Grid, colorBorder);
        C.Alpha := C.Alpha * 0.6;
        for Col := FirstCol + 1 to LastCol + 1 do
        begin
          P := Round(Area.X + Grid.ColOffset(Col) - Grid.ScrollX) + 0.5;
          Canvas.MoveTo(P, Area.Y);
          Canvas.LineTo(P, Area.Y + Area.Height);
        end;
        for Row := FirstRow + 1 to LastRow + 1 do
        begin
          P := Round(Area.Y + Row * Grid.RowHeight - Grid.ScrollY) + 0.5;
          Canvas.MoveTo(Area.X, P);
          Canvas.LineTo(Area.X + Area.Width, P);
        end;
        Canvas.Stroke(C, 1);
      end;
    finally
      Canvas.Pop;
    end;
  end;
  if Grid.HeaderRow then
    DrawGridHeader(Grid);
  if Grid.BarVisible(barVert) then
  begin
    Track := Grid.BarRect(barVert);
    Thumb := Grid.ThumbRect(barVert);
    Track.Offset(Bounds.X, Bounds.Y);
    Thumb.Offset(Bounds.X, Bounds.Y);
    DrawScrollBar(Grid, Track, Thumb, False);
  end;
  if Grid.BarVisible(barHorz) then
  begin
    Track := Grid.BarRect(barHorz);
    Thumb := Grid.ThumbRect(barHorz);
    Track.Offset(Bounds.X, Bounds.Y);
    Thumb.Offset(Bounds.X, Bounds.Y);
    DrawScrollBar(Grid, Track, Thumb, True);
  end;
end;

{ The header is placed on whole pixels so its cells and lines are sharp. The
  whole header is drawn first as a cell without a column, which is what shows
  past the last column. }

procedure TCanvasTheme.DrawGridHeader(Grid: TScrollGrid);
var
  Bounds, Header, R: TRectF;
  First, Last, Col: Integer;
  Hot: Boolean;
begin
  Bounds := Grid.Computed.Bounds;
  Bounds.X := Trunc(Bounds.X);
  Bounds.Y := Trunc(Bounds.Y);
  Header := Grid.HeaderRect;
  Header.Offset(Bounds.X, Bounds.Y);
  Header.Width := Round(Header.Width);
  Header.Height := Round(Header.Height);
  if (Header.Width < 1) or (Header.Height < 1) then
    Exit;
  Hot := wsHot in Grid.State;
  Canvas.Push;
  try
    Canvas.Clip(Header);
    DrawHeaderCell(Grid, Header, -1, False, False);
    if Grid.ColCount > 0 then
    begin
      { Only the header cells which can be seen are drawn }
      First := Grid.ColFromOffset(Grid.ScrollX);
      Last := Grid.ColFromOffset(Grid.ScrollX + Header.Width);
      if Last > Grid.ColCount - 1 then
        Last := Grid.ColCount - 1;
      for Col := First to Last do
      begin
        R := Grid.HeaderCellRect(Col);
        R.Offset(Bounds.X, Bounds.Y);
        R.X := Round(R.X);
        R.Width := Round(R.Width);
        R.Height := Header.Height;
        DrawHeaderCell(Grid, R, Col, Hot and (Grid.HeaderHot = Col),
          Grid.HeaderPressed = Col);
      end;
    end;
  finally
    Canvas.Pop;
  end;
end;

procedure TCanvasTheme.DrawHeaderCell(Grid: TScrollGrid; const Rect: TRectF; Col: Integer;
  Hot, Pressed: Boolean);
var
  C: TColorF;
begin
  Canvas.Rect(Rect);
  Canvas.Fill(CalcColor(Grid, colorFace));
  if Hot or Pressed then
  begin
    C := CalcColor(Grid, colorActive);
    if Pressed then
      C.Alpha := C.Alpha * 0.4
    else
      C.Alpha := C.Alpha * 0.2;
    Canvas.Rect(Rect);
    Canvas.Fill(C);
  end;
  DrawHeaderLines(Rect, CalcColor(Grid, colorBorder));
  DrawHeaderText(Grid, Rect, Col, CalcColor(Grid, colorText));
end;

procedure TCanvasTheme.DrawHeaderLines(const Rect: TRectF; const Color: TColorF);
begin
  Canvas.MoveTo(Rect.Right - 0.5, Rect.Top);
  Canvas.LineTo(Rect.Right - 0.5, Rect.Bottom - 0.5);
  Canvas.LineTo(Rect.Left, Rect.Bottom - 0.5);
  Canvas.Stroke(Color, 1);
end;

procedure TCanvasTheme.DrawHeaderText(Grid: TScrollGrid; const Rect: TRectF; Col: Integer;
  const Color: TColorF);
const
  Padding = 6;
  ArrowSize = 4;
var
  R: TRectF;
  C: TColorF;
  X, Y: Float;
  S: string;
begin
  if (Col < 0) or (Col > Grid.ColCount - 1) then
    Exit;
  R := Rect;
  R.Inflate(-Padding, 0);
  Y := Trunc(Rect.Y + Rect.Height / 2);
  { The arrow points up when the rows are sorted from first to last }
  if Grid.SortCol = Col then
  begin
    X := Trunc(R.Right - ArrowSize);
    if Grid.SortDescending then
    begin
      Canvas.MoveTo(X - ArrowSize, Y - 2);
      Canvas.LineTo(X, Y + 2);
      Canvas.LineTo(X + ArrowSize, Y - 2);
    end
    else
    begin
      Canvas.MoveTo(X - ArrowSize, Y + 2);
      Canvas.LineTo(X, Y - 2);
      Canvas.LineTo(X + ArrowSize, Y + 2);
    end;
    C := Color;
    C.Alpha := C.Alpha * 0.8;
    Canvas.Stroke(C, 1.5);
    R.Width := R.Width - ArrowSize * 2 - Padding;
  end;
  S := Grid.ColTitles[Col];
  if (S = '') or (R.Width < 1) then
    Exit;
  Font.Size := FontSize;
  Font.Layout := fontMiddle;
  Font.Color := Color;
  case Grid.ColAligns[Col] of
    alignCenter:
      begin
        Font.Align := fontCenter;
        X := R.X + R.Width / 2;
      end;
    alignFar:
      begin
        Font.Align := fontRight;
        X := R.Right;
      end;
  else
    Font.Align := fontLeft;
    X := R.X;
  end;
  { The title is clipped so it does not run into the arrow or the next cell }
  Canvas.Push;
  try
    Canvas.Clip(R);
    DrawText(Font, S, X, Y);
  finally
    Canvas.Pop;
  end;
end;

procedure TCanvasTheme.RenderHint(Widget: TWidget; Opacity: Float);
const
  MaxWidth = 400;
var
  R: TRectF;
  P: TPointF;
  S: TSizeF;
  Brush, Pen: TColorF;
	SingleLine, MultiLine: TPointF;

  procedure NearAbove;
  begin
    R.Y := R.Top - 34;
    R.X := R.X + S.X / 2 - 12;
    if R.X < 0 then
      R.X := 0;
    R.Height := 20;
    R.X := R.MidPoint.X;
    R.X := R.X - S.X / 2 - 5;
    R.Width := S.X + 10;
    R.Height := S.Y + 20;
    Canvas.MoveTo(R.Right, R.Top);
    Canvas.LineTo(R.Right, R.Bottom);
    Canvas.LineTo(R.Left + 24, R.Bottom);
    Canvas.LineTo(R.Left + 16, R.Bottom + 8);
    Canvas.LineTo(R.Left + 8, R.Bottom);
    Canvas.LineTo(R.Left, R.Bottom);
    Canvas.LineTo(R.Left, R.Top);
  end;

  procedure NearBelow;
  begin
    R.Y := R.Bottom + 5;
    R.X := R.X + S.X / 2 - 12;
    if R.X < 0 then
      R.X := 0;
    R.Height := 20;
    R.X := R.MidPoint.X;
    R.X := R.X - S.X / 2 - 5;
    R.Width := S.X + 10;
    R.Height := S.Y + 20;
    Canvas.MoveTo(R.Right, R.Top);
    Canvas.LineTo(R.Right, R.Bottom);
    Canvas.LineTo(R.Left, R.Bottom);
    Canvas.LineTo(R.Left, R.Top);
    Canvas.LineTo(R.Left + 8, R.Top);
    Canvas.LineTo(R.Left + 16, R.Top - 8);
    Canvas.LineTo(R.Left + 24, R.Top);
  end;

  procedure CenterAbove;
  begin
    R.Y := R.Top - 34;
    R.Height := 20;
    R.X := R.MidPoint.X;
    R.X := R.X - S.X / 2 - 5;
    R.Width := S.X + 10;
    R.Height := S.Y + 20;
    Canvas.MoveTo(R.Right, R.Top);
    Canvas.LineTo(R.Right, R.Bottom);
    Canvas.LineTo(R.MidPoint.X + 8, R.Bottom);
    Canvas.LineTo(R.MidPoint.X, R.Bottom + 8);
    Canvas.LineTo(R.MidPoint.X - 8, R.Bottom);
    Canvas.LineTo(R.Left, R.Bottom);
    Canvas.LineTo(R.Left, R.Top);
  end;

  procedure CenterBelow;
  begin
    R.Y := R.Bottom + 5;
    R.Height := 20;
    R.X := R.MidPoint.X;
    R.X := R.X - S.X / 2 - 5;
    R.Width := S.X + 10;
    R.Height := S.Y + 20;
    Canvas.MoveTo(R.Right, R.Top);
    Canvas.LineTo(R.Right, R.Bottom);
    Canvas.LineTo(R.Left, R.Bottom);
    Canvas.LineTo(R.Left, R.Top);
    Canvas.LineTo(R.MidPoint.X - 8, R.Top);
    Canvas.LineTo(R.MidPoint.X, R.Top - 8);
    Canvas.LineTo(R.MidPoint.X + 8, R.Top);
  end;

  procedure FarAbove;
  begin
    R.Y := R.Top - 34;
    R.X := R.X - S.X / 2 + 12;
    if R.Right > Widget.Main.Width - 12 then
      R.X := Widget.Main.Width - S.X - 28;
    R.Height := 20;
    R.X := R.MidPoint.X;
    R.X := R.X - S.X / 2 - 5;
    R.Width := S.X + 10;
    R.Height := S.Y + 20;
    Canvas.MoveTo(R.Right, R.Top);
    Canvas.LineTo(R.Right, R.Bottom);
    Canvas.LineTo(R.Right - 8, R.Bottom);
    Canvas.LineTo(R.Right - 16, R.Bottom + 8);
    Canvas.LineTo(R.Right - 24, R.Bottom);
    Canvas.LineTo(R.Left, R.Bottom);
    Canvas.LineTo(R.Left, R.Top);
  end;

  procedure FarBelow;
  begin
    R.Y := R.Bottom + 5;
    R.X := R.X - S.X / 2 + 12;
    if R.Right > Widget.Main.Width - 12 then
      R.X := Widget.Main.Width - S.X - 28;
    R.Height := 20;
    R.X := R.MidPoint.X;
    R.X := R.X - S.X / 2 - 5;
    R.Width := S.X + 10;
    R.Height := S.Y + 20;
    Canvas.MoveTo(R.Right, R.Top);
    Canvas.LineTo(R.Right, R.Bottom);
    Canvas.LineTo(R.Left, R.Bottom);
    Canvas.LineTo(R.Left, R.Top);
    Canvas.LineTo(R.Right - 24, R.Top);
    Canvas.LineTo(R.Right - 16, R.Top - 8);
    Canvas.LineTo(R.Right - 8, R.Top);
  end;

var
  Align: TWidgetAlign;
  Layout: TWidgetAlign;
  A: TRectF;
begin
  Font.Size := FontSize;
  Font.Align := fontCenter;
  R := Widget.Computed.Bounds.Round;
  P := R.MidPoint;
  SingleLine := MeasureText(Font, Widget.Hint);
  MultiLine.X := SingleLine.X;
  MultiLine.Y := MeasureMemo(Font, Widget.Hint, MaxWidth);
  if MultiLine.X > MaxWidth then
	  MultiLine.X := MaxWidth;
  S := MultiLine;
  Align := alignCenter;
  if P.X - S.X / 2 < 20 then
    Align := alignNear
  else if P.X + S.X / 2 > Widget.Main.Width - 20 then
    Align := alignFar;
  Layout := alignFar;
  if P.Y + Widget.Height / 2 + S.Y > Widget.Main.Height - 20 then
    Layout := alignNear;
  Font.Size := FontSize;
  A := R;
  R.Offset(6, 4);
  case Align of
    alignNear:
      if Layout = alignFar then
        NearBelow
      else
        NearAbove;
    alignCenter:
      if Layout = alignFar then
        CenterBelow
      else
        CenterAbove;
    alignFar:
      if Layout = alignFar then
        FarBelow
      else
        FarAbove;
  end;
  Canvas.ClosePath;
  Brush := ARGB($30000000);
  Brush.Alpha := Brush.Alpha * Opacity;
  Canvas.Fill(Brush);
  R := A;
  case Align of
    alignNear:
      if Layout = alignFar then
        NearBelow
      else
        NearAbove;
    alignCenter:
      if Layout = alignFar then
        CenterBelow
      else
        CenterAbove;
    alignFar:
      if Layout = alignFar then
        FarBelow
      else
        FarAbove;
  end;
  Canvas.ClosePath;
  Pen := ARGB($404040);
  Pen.Alpha := Opacity;
  Brush := ARGB($EDEDAF);
  Brush.Alpha := Opacity;
  FHintBrush.A := R.Sector(2);
  FHintBrush.NearStop.Color := Brush;
  Brush := ARGB($C0C080);
  Brush.Alpha := Opacity;
  FHintBrush.B := R.Sector(8);
  FHintBrush.FarStop.Color := Brush;
  Canvas.Fill(FHintBrush, True);
  Canvas.Stroke(Pen);
  Font.Align := fontCenter;
  Font.Layout := fontMiddle;
  Font.Color := Pen;
  with R.MidPoint do
    if MultiLine.Y > SingleLine.Y then
		begin
  	  Font.Align := fontLeft;
      Font.Layout := fontTop;
	    DrawTextMemo(Font, Widget.Hint, R.Left + 10, Y - S.Y / 2, S.X);
  	  Font.Align := fontCenter;
	  end
  	else
    begin
    	DrawText(Font, Widget.Hint, X, Y + 1);
    end;
end;

{ TArcDarkTheme }

function TArcDarkTheme.FontSize: Float;
begin
  Result := 14;
end;

function TArcDarkTheme.GlyphSize: Float;
begin
  Result := 22;
end;

function TArcDarkTheme.TitleSize: Float;
begin
  Result := 15;
end;

procedure TArcDarkTheme.Init(Canvas: ICanvas);
begin
  inherited Init(Canvas);
  Font := Canvas.LoadFontAsset('Ubuntu', FontRes + '/Ubuntu-R.ttf');
  Font.Size := FontSize;
  Font.Color := colorWhite;
  Font.Align := fontCenter;
  Font.Layout := fontMiddle;
  Glyph := Canvas.LoadFontAsset('glyph', FontRes + '/materialdesignicons-webfont.ttf');
  Glyph.Size := GlyphSize;
  Glyph.Color := colorWhite;
  Glyph.Align := fontCenter;
  Glyph.Layout := fontMiddle;
  Title := Canvas.LoadFontAsset('Ubuntu-M', FontRes + '/Ubuntu-M.ttf');
  Title.Size := TitleSize;
  Title.Color := colorWhite;
  Title.Align := fontCenter;
  Title.Layout := fontMiddle;
end;

function TArcDarkTheme.CalcColor(Widget: TWidget; Color: TThemeColor): TColorF;
begin
  Result := ARGB(Palette(Widget, Color));
end;

function TArcDarkTheme.Palette(Widget: TWidget; Color: TThemeColor): LongWord;
var
  A: LongWord;
begin
  if Widget.Computed.Enabled then
    case Color of
      colorBase: Result := $383C4A;
      colorFace: Result := $444A58;
      colorBorder: Result := $2B2E39;
      colorActive: Result := $5294E2;
      colorPressed: Result := $5294E2;
      colorSelected: Result := $FFFFFF;
      colorHot: Result := $4E5467;
      colorCaption: Result := $2F343F;
      colorTitle: Result := $C0C0C0;
      colorText: Result := $D0D0D0;
    else
      Result := 0;
    end
  else
    case Color of
      colorBase: Result := $383C4A;
      colorFace: Result := $3E4350;
      colorBorder: Result := $313541;
      colorActive: Result := $5294E2;
      colorPressed: Result := $466C9D;
      colorSelected: Result := $FFFFFF;
      colorHot: Result := $3E4350;
      colorCaption: Result := $2F343F;
      colorTitle: Result := $7D818B;
      colorText: Result := $7D818B;
    end;
  if Color = colorSelected then
    A := Round($FF * Widget.Computed.Opacity * 0.2) shl 24
  else
    A := Round($FF * Widget.Computed.Opacity) shl 24;
  Result := Result or A;
end;

function TArcDarkTheme.CalcSize(Widget: TWidget; Part: TThemePart): TSizeF;
var
  M: Float;
begin
  Result := NewPointF(0, 0);
  { Entire widget default sizes }
  if Part = tpEverything then
  begin
    if Widget is TSpacer then
      Result := NewPointF(8, 8)
    else if Widget is TWindow then
      Result := NewPointF(400, 300)
    else if Widget is TContainerWidget then
      Result := NewPointF(10, 10)
    else if Widget is TMemo then
      Result := NewPointF(260, 100)
    else if Widget is TEdit then
      Result := Scaled(NewPointF(160, 30))
    else if Widget is TPushButton then
    begin
      Result.X := MeasureText(Font, '[ ' + Widget.Text + ' ]').X;
      Result.Y := Scaled(30);
      if Result.X < Scaled(80) then
        Result.X := Scaled(80);
    end
    else if Widget is TGlyphButton then
      Result := Scaled(NewPointF(30, 30))
    else if Widget is TGlyphImage then
      Result := Scaled(NewPointF(48, 48))
    else if Widget is TCheckBox then
    begin
      Result.X := MeasureText(Font, '[ ' + Widget.Text + ' ]').X + 24;
      Result.Y := Scaled(24);
    end
    else if Widget is TSlider then
      Result := Scaled(NewPointF(150, 20))
    else if Widget is TSpinBox then
      Result := Scaled(NewPointF(150, 20))
    else if Widget is TLabel then
    begin
      M := TLabel(Widget).MaxWidth;
      { Realted to issue detailed in TCanvas.MeasureMemo }
      Font.Size := FontSize;
      Font.Align := fontLeft;
      Font.Layout := fontMiddle;
      if M < 1 then
      begin
        Result := MeasureText(Font, Widget.Text);
        Result.X := Result.X + 4;
        Result.Y := Result.Y + 4;
      end
      else
      begin
        Result := MeasureText(Font, Widget.Text);
        Result.X := Result.X + 4;
        if Result.X > M then
        begin
          Result.X := M + 4;
          Result.Y := MeasureMemo(Font, Widget.Text, M);
        end;
        Result.Y := Result.Y + 4;
      end;
    end;
    Exit;
  end;
  { Indentation }
  if Part = tpIndent then
  begin
    Result := Scaled(NewPointF(16, 0));
    Exit;
  end;
  { TWindow parts }
  if Widget is TWindow then
    case Part of
      tpCaption:
        begin
          Result.X := Widget.Width;
          Result.Y := Scaled(22);
        end;
		else
    end;
  { TSlider parts }
  if Widget is TSlider then
    case Part of
      tpThumb: Result := Scaled(NewPointF(14, 14));
    else
    end;
  { TCheckBox parts }
  if Widget is TCheckBox then
    case Part of
      tpNode:
          Result := NewPointF(18, 18);
      tpCaption:
        begin
          Result := MeasureText(Font, '[ ' + Widget.Text + ' ]');
          Result.X := Result.X + 6;
          Result.Y := Result.Y + 6;
        end;
	    else
	    end;
end;

procedure TArcDarkTheme.DrawMemo(Widget: TMemo);
var
  R: TRectF;
  C: TColorF;
begin
  R := Widget.Computed.Bounds.Round;
  Canvas.RoundRect(R, 3);
  Canvas.Fill(CalcColor(Widget, colorCaption), True);
  if wsSelected in Widget.State then
    Canvas.Stroke(CalcColor(Widget, colorActive))
  else
    Canvas.Stroke(CalcColor(Widget, colorBorder));
  C := CalcColor(Widget, colorActive);
  C.Alpha := C.Alpha * 0.6;
  DrawMemoText(Widget, CalcColor(Widget, colorText), C, CalcColor(Widget, colorText));
end;

procedure TArcDarkTheme.DrawListFrame(Widget: TWidget; out TextColor, SelectColor,
  SelectTextColor: TColorF);
var
  R: TRectF;
  C: TColorF;
begin
  R := Widget.Computed.Bounds.Round;
  Canvas.RoundRect(R, 3);
  Canvas.Fill(CalcColor(Widget, colorCaption), True);
  if wsSelected in Widget.State then
    Canvas.Stroke(CalcColor(Widget, colorActive))
  else
    Canvas.Stroke(CalcColor(Widget, colorBorder));
  C := CalcColor(Widget, colorActive);
  C.Alpha := C.Alpha * 0.6;
  TextColor := CalcColor(Widget, colorText);
  SelectColor := C;
  SelectTextColor := CalcColor(Widget, colorText);
end;

{ A flat dark header cell which lightens under the mouse }

procedure TArcDarkTheme.DrawHeaderCell(Grid: TScrollGrid; const Rect: TRectF; Col: Integer;
  Hot, Pressed: Boolean);
begin
  Canvas.Rect(Rect);
  if Pressed then
    Canvas.Fill(CalcColor(Grid, colorPressed))
  else if Hot then
    Canvas.Fill(CalcColor(Grid, colorHot))
  else
    Canvas.Fill(CalcColor(Grid, colorBase));
  DrawHeaderLines(Rect, CalcColor(Grid, colorBorder));
  DrawHeaderText(Grid, Rect, Col, CalcColor(Grid, colorText));
end;

{ A dark track with a slim rounded thumb }

procedure TArcDarkTheme.DrawScrollBar(Widget: TWidget; const Track, Thumb: TRectF;
  Horizontal: Boolean);
var
  R: TRectF;
begin
  Canvas.Rect(Track);
  Canvas.Fill(CalcColor(Widget, colorBase));
  R := Thumb;
  if Horizontal then
    R.Inflate(-2, -4)
  else
    R.Inflate(-4, -2);
  Canvas.RoundRect(R, 3);
  Canvas.Fill(CalcColor(Widget, colorHot));
end;

procedure TArcDarkTheme.DrawEdit(Widget: TEdit);
var
  R: TRectF;
  C: TColorF;
begin
  R := Widget.Computed.Bounds.Round;
  Canvas.RoundRect(R, 3);
  Canvas.Fill(CalcColor(Widget, colorCaption), True);
  if wsSelected in Widget.State then
    Canvas.Stroke(CalcColor(Widget, colorActive))
  else
    Canvas.Stroke(CalcColor(Widget, colorBorder));
  C := CalcColor(Widget, colorActive);
  C.Alpha := C.Alpha * 0.6;
  DrawEditText(Widget, R, CalcColor(Widget, colorText), C, CalcColor(Widget, colorText));
end;

procedure TArcDarkTheme.DrawButton(Widget: TPushButton);
var
  R: TRectF;
begin
  R := Widget.Computed.Bounds.Round;
  Canvas.RoundRect(R, 3);
  if wsHot in Widget.State then
    if wsPressed in Widget.State then
      Canvas.Fill(CalcColor(Widget, colorPressed), True)
    else
      Canvas.Fill(CalcColor(Widget, colorHot), True)
  else
    Canvas.Fill(CalcColor(Widget, colorFace), True);
  Canvas.Stroke(CalcColor(Widget, colorBorder));
  if wsSelected in Widget.State then
  begin
    R.Inflate(-3, -3);
    Canvas.RoundRect(R, 3);
    Canvas.Stroke(CalcColor(Widget, colorSelected));
  end;
  DrawCaption(Widget, R);
end;

procedure TArcDarkTheme.DrawGlyphButton(Widget: TGlyphButton);
var
  R: TRectF;
begin
  R := Widget.Computed.Bounds.Round;
  Canvas.RoundRect(R, 3);
  if Widget.Down then
    Canvas.Fill(CalcColor(Widget, colorPressed), True)
  else if wsHot in Widget.State then
    if wsPressed in Widget.State then
      Canvas.Fill(CalcColor(Widget, colorPressed), True)
    else
      Canvas.Fill(CalcColor(Widget, colorHot), True)
  else
    Canvas.Fill(CalcColor(Widget, colorFace), True);
  Canvas.Stroke(CalcColor(Widget, colorBorder));
  DrawCaption(Widget, R);
end;

procedure TArcDarkTheme.DrawGlyphImage(Widget: TGlyphImage);
begin
  DrawCaption(Widget, Widget.Computed.Bounds);
end;

procedure TArcDarkTheme.DrawCheckBox(Widget: TCheckBox);
var
  B, R: TRectF;
  P: TPointF;
begin
  B := Widget.Computed.Bounds.Round;
  R := B;
  R.Y := B.MidPoint.Y - 7;
  R.Height := 14;
  R.Width := 14;
  if Widget.Round then
    Canvas.RoundRect(R, 7)
  else
    Canvas.RoundRect(R, 3);
  if wsToggled in Widget.State then
  begin
    Canvas.Fill(CalcColor(Widget, colorPressed));
    if Widget.Round then
    begin
      R.Inflate(-4, -4);
      Canvas.RoundRect(R, 7);
      Canvas.Fill(CalcColor(Widget, colorFace));
    end
    else
    begin
      Pen.Color := CalcColor(Widget, colorFace);
      Pen.Width := 3;
      Pen.LineCap := capButt;
      Pen.LineJoin := joinMiter;
      P := R.MidPoint;
      P.Offset(-1, 1);
      Canvas.MoveTo(P.X - 2.5, P.Y - 2);
      Canvas.LineTo(P.X, P.Y + 2);
      Canvas.LineTo(P.X + 5, P.Y - 5);
      Canvas.Stroke(Pen);
    end
  end
  else
    Canvas.Fill(CalcColor(Widget, colorBorder));
  R := B;
  R.Y := B.MidPoint.Y - 7;
  R.Height := 14;
  R.Width := 14;
  if Widget.Round then
    Canvas.RoundRect(R, 7)
  else
    Canvas.RoundRect(R, 3);
  Canvas.Stroke(CalcColor(Widget, colorBorder), 2);
  R := B;
  R.X := R.X + 16;
  P := MeasureText(Font, Widget.Text);
  R.Width := P.X + 8;
  R.Inflate(-2, 2);
  if wsSelected in Widget.State then
  begin
    B.Inflate(-3, -3);
    Canvas.RoundRect(R, 3);
    Canvas.Stroke(CalcColor(Widget, colorSelected), 1);
  end;
  DrawCaption(Widget, R);
end;

procedure TArcDarkTheme.DrawSlider(Widget: TSlider);
var
  B, G, R: TRectF;
begin
  B := Widget.Computed.Bounds.Round;
  G := Widget.GripRect;
  R.Y := B.MidPoint.Y - 2;
  R.X := B.X + G.Width / 2;
  R.Width := Widget.Width - G.Width;
  R.Height := 4;
  Canvas.RoundRect(R, 2);
  Canvas.Fill(CalcColor(Widget, colorSelected), True);
  Canvas.Stroke(CalcColor(Widget, colorBorder));
  G.Offset(B.X, B.Y);
  with G.MidPoint do
    Canvas.Circle(X, Y, G.Width / 2);
  if wsPressed in Widget.State then
    Canvas.Fill(CalcColor(Widget, colorPressed), True)
  else if wsHot in Widget.State then
    Canvas.Fill(CalcColor(Widget, colorHot), True)
  else
    Canvas.Fill(CalcColor(Widget, colorFace), True);
  Canvas.Stroke(CalcColor(Widget, colorBorder), 1);
end;

procedure TArcDarkTheme.DrawSpinBox(Widget: TSpinBox);
var
  R: TRectF;
begin
  R := Widget.Computed.Bounds.Round;
  Canvas.RoundRect(R, 3);
  if wsPressed in Widget.State then
    Canvas.Fill(CalcColor(Widget, colorPressed), True)
  else
    Canvas.Fill(CalcColor(Widget, colorHot), True);
  Canvas.Stroke(CalcColor(Widget, colorBorder));
  if wsSelected in Widget.State then
  begin
    R.Inflate(-3, -3);
    Canvas.RoundRect(R, 3);
    Canvas.Stroke(CalcColor(Widget, colorSelected));
  end;
  DrawCaption(Widget, R, Widget.Prefix + Widget.Text);
  Glyph.Color := Font.Color;
  Glyph.Size := Font.Size + 2;
  DrawText(Glyph, '󰅁', R.Left + 10, R.MidPoint.Y);
  DrawText(Glyph, '󰅂', R.Right - 10, R.MidPoint.Y);
end;

procedure TArcDarkTheme.DropColors(Widget: TSpinBox; out Frame, Back, Hot, Text,
  HotText: TColorF);
begin
  Frame := ARGB($40000000);
  Back := CalcColor(Widget, colorFace);
  Hot := CalcColor(Widget, colorActive);
  Text := CalcColor(Widget, colorText);
  HotText := Text;
end;

procedure TArcDarkTheme.PostDrawSpinBox(Widget: TSpinBox);
var
  R: TRectF;
  P: TPointF;
  S: Integer;
  I: Integer;
begin
	R := Widget.ItemRect(-1);
  if R.Empty then
  	Exit;
  Canvas.Rect(R);
  Canvas.Fill(ARGB($40000000));
	R.Inflate(-1, -1);
  Canvas.Rect(R);
  Canvas.Fill(CalcColor(Widget, colorFace));
	R := Widget.ItemRect(0);
  P := Widget.Main.MouseFor(Widget);
  S := Widget.ItemFromPoint(P);
  for I := 0 to Widget.Items.Length - 1 do
  begin
    if I = S then
    begin
      Canvas.Rect(R);
      Canvas.Fill(CalcColor(Widget, colorActive));
    end;
    with R.MidPoint do
	    DrawText(Font, Widget.Items[I], X, Y);
    R.Y := R.Bottom + 1;
  end;
end;

procedure TArcDarkTheme.DrawLabel(Widget: TLabel);
begin
  DrawCaption(Widget, Widget.Computed.Bounds);
end;

{ A circle with a cross at the left of the title bar. It is gray, and turns
  red with a white cross while the mouse is over it. }

function TArcDarkTheme.CalcCloseRect(Window: TWindow): TRectF;
const
  Size = 16;
var
  H: Float;
begin
  H := CalcSize(Window, tpCaption).Y;
  Result := NewRectF(Trunc((H - Size) / 2) + 1, Trunc((H - Size) / 2), Size, Size);
end;

procedure TArcDarkTheme.DrawClose(Window: TWindow; const Rect: TRectF; Hot, Pressed: Boolean);
var
  P: TPointF;
begin
  P := Rect.MidPoint;
  Canvas.Circle(P.X, P.Y, Rect.Width / 2);
  if Pressed then
  begin
    Canvas.Fill(Tint(Window, $C93A40));
    DrawCross(Rect, Tint(Window, $FFFFFF), 1.8, 5);
  end
  else if Hot then
  begin
    Canvas.Fill(Tint(Window, $F04A50));
    DrawCross(Rect, Tint(Window, $FFFFFF), 1.8, 5);
  end
  else
  begin
    Canvas.Fill(CalcColor(Window, colorHot));
    DrawCross(Rect, CalcColor(Window, colorTitle), 1.8, 5);
  end;
end;

procedure TArcDarkTheme.DrawWindow(Widget: TWindow);
var
  C: TColorF;
  R: TRectF;
begin
  if Widget = Widget.Main.ModalWindow then
  begin
    { The overlay covers the scene behind the window, so it is drawn without
      the transform of the window }
    if Widget.Matrix <> nil then
      PopMatrix;
    Canvas.Rect(Widget.Main.Bounds.Round);
    Canvas.Fill(ARGB($80000000));
    if Widget.Matrix <> nil then
      PushMatrix(Widget.Matrix);
  end;
  R := Widget.Computed.Bounds.Round;
  Canvas.RoundRectVarying(R, 4, 4, 0, 0);
  C := CalcColor(Widget, colorBase);
  C.Alpha := C.Alpha * (1 - Clamp(Widget.Fade));
  Canvas.Fill(C, True);
  Canvas.Stroke(CalcColor(Widget, colorBorder));
  R.Height := CalcSize(Widget, tpCaption).Y;
  R.Y := R.Y + 1;
  C := CalcColor(Widget, colorCaption);
  Canvas.RoundRectVarying(R, 4, 4, 0, 0);
  Canvas.Fill(C);
  DrawCaption(Widget, R);
end;

procedure TArcDarkTheme.DrawContainer(Widget: TContainerWidget);
var
  C: TColorF;
begin
  if Widget.Parent is TMainWidget then
  begin
    Canvas.RoundRect(Widget.Computed.Bounds.Round, 4);
    C := CalcColor(Widget, colorBase);
    C.Alpha := C.Alpha * (1 - Clamp(Widget.Fade));
    Canvas.Fill(C, True);
    Canvas.Stroke(CalcColor(Widget, colorBorder));
  end;
end;

{ TChicagoTheme }

function TChicagoTheme.FontSize: Float;
begin
  Result := 14;
end;

function TChicagoTheme.GlyphSize: Float;
begin
  Result := 22;
end;

function TChicagoTheme.TitleSize: Float;
begin
  Result := 15;
end;

procedure TChicagoTheme.Init(Canvas: ICanvas);
var
  B: IRenderBitmap;
begin
  inherited Init(Canvas);
  Canvas.Matrix.Push;
  Canvas.Matrix.Identity;
  B := Canvas.NewBitmap('chicago-focus', 2, 2);
  B.Bind;
  Canvas.Clear;
  Canvas.Rect(0, 0, 1, 1);
  Canvas.Fill(colorBlack);
  Canvas.Rect(1, 1, 1, 1);
  Canvas.Fill(colorBlack);
  B.Unbind;
  Canvas.Matrix.Pop;
  FFocus := NewPen;
  FFocus.Brush := NewBrush(B);
  Font := Canvas.LoadFontAsset('NotoSans', FontRes + '/NotoSans-Regular.ttf');
  Font.Size := FontSize;
  Font.Color := colorWhite;
  Font.Align := fontCenter;
  Font.Layout := fontMiddle;
  Glyph := Canvas.LoadFontAsset('glyph', FontRes + '/materialdesignicons-webfont.ttf');
  Glyph.Size := GlyphSize;
  Glyph.Color := colorWhite;
  Glyph.Align := fontCenter;
  Glyph.Layout := fontMiddle;
  Title := Canvas.LoadFontAsset('NotoSans-Bold', FontRes + '/NotoSans-Bold.ttf');
  Title.Size := TitleSize;
  Title.Color := colorSilver;
  Title.Align := fontCenter;
  Title.Layout := fontMiddle;
end;

function TChicagoTheme.CalcColor(Widget: TWidget; Color: TThemeColor): TColorF;
begin
  Result := ARGB(Palette(Widget, Color));
end;

function TChicagoTheme.Palette(Widget: TWidget; Color: TThemeColor): LongWord;
var
  A: LongWord;
begin
  if Widget.Computed.Enabled then
    case Color of
      colorBase: Result := $C3C6CC;
      colorFace: Result := $C3C6CC;
      colorBorder: Result := $2B2E39;
      colorActive: Result := $0101A9;
      colorPressed: Result := $5294E2;
      colorSelected: Result := $FFFFFF;
      colorHot: Result := $4E5467;
      colorCaption: Result := $0101A9;
      colorTitle: Result := $FFFFFF;
      colorText: Result := $000000;
      colorHighlight: Result := $FEFEFE;
      colorShadow: Result := $808080;
      colorDarkShadow: Result := $101010;
  else
      Result := 0;
    end
  else
    case Color of
      colorBase: Result := $C3C6CC;
      colorFace: Result := $C3C6CC;
      colorBorder: Result := $2B2E39;
      colorActive: Result := $0101A9;
      colorPressed: Result := $5294E2;
      colorSelected: Result := $FFFFFF;
      colorHot: Result := $4E5467;
      colorCaption: Result := $0101A9;
      colorTitle: Result := $FFFFFF;
      colorText: Result := $606060;
      colorHighlight: Result := $B0B0B0;
      colorShadow: Result := $808080;
      colorDarkShadow: Result := $606060;
    end;
  if Color = colorSelected then
    A := Round($FF * Widget.Computed.Opacity * 0.2) shl 24
  else
    A := Round($FF * Widget.Computed.Opacity) shl 24;
  Result := Result or A;
end;

function TChicagoTheme.CalcSize(Widget: TWidget; Part: TThemePart): TSizeF;
var
  M: Float;
begin
  Result := NewPointF(0, 0);
  { Entire widget defaiult sizes }
  if Part = tpEverything then
  begin
    if Widget is TSpacer then
      Result := NewPointF(8, 8)
    else if Widget is TWindow then
      Result := NewPointF(400, 300)
    else if Widget is TContainerWidget then
      Result := NewPointF(10, 10)
    else if Widget is TMemo then
      Result := NewPointF(260, 100)
    else if Widget is TEdit then
      Result := Scaled(NewPointF(160, 24))
    else if Widget is TPushButton then
    begin
      Result.X := MeasureText(Font, '[ ' + Widget.Text + ' ]').X;
      Result.Y := Scaled(25);
      if Result.X < Scaled(75) then
        Result.X := Scaled(75);
    end
    else if Widget is TGlyphButton then
      Result := Scaled(NewPointF(30, 30))
    else if Widget is TGlyphImage then
      Result := Scaled(NewPointF(48, 48))
    else if Widget is TCheckBox then
    begin
      Result.X := MeasureText(Font, '[ ' + Widget.Text + ' ]').X + 24;
      Result.Y := Scaled(24);
    end
    else if Widget is TSlider then
      Result := Scaled(NewPointF(150, 20))
    else if Widget is TSpinBox then
      Result := Scaled(NewPointF(150, 24))
    else if Widget is TLabel then
    begin
      M := TLabel(Widget).MaxWidth;
      { Realted to issue detailed in TCanvas.MeasureMemo }
      Font.Size := FontSize;
      Font.Align := fontLeft;
      Font.Layout := fontMiddle;
      if M < 1 then
      begin
        Result := MeasureText(Font, Widget.Text);
        Result.X := Result.X + 4;
        Result.Y := Result.Y + 4;
      end
      else
      begin
        Result := MeasureText(Font, Widget.Text);
        Result.X := Result.X + 4;
        if Result.X > M then
        begin
          Result.X := M + 4;
          Result.Y := Canvas.MeasureMemo(Font, Widget.Text, M);
        end;
        Result.Y := Result.Y + 4;
      end;
    end;
    Exit;
  end;
  { Indentation }
  if Part = tpIndent then
  begin
    Result := Scaled(NewPointF(16, 0));
    Exit;
  end;
  { TWindow parts }
  if Widget is TWindow then
    case Part of
      tpCaption:
        begin
          Result.X := Widget.Width;
          Result.Y := Scaled(26);
        end;
    else
    end
  { TSlider parts }
  else if Widget is TSlider then
    case Part of
      tpThumb: Result := Scaled(NewPointF(8, 16));
    else
    end
  { TCheckBox parts }
  else if Widget is TCheckBox then
    case Part of
      tpNode:
          Result := NewPointF(18, 18);
      tpCaption:
        begin
          Result := MeasureText(Font, '[ ' + Widget.Text + ' ]');
          Result.X := Result.X + 6;
          Result.Y := Result.Y + 6;
        end;
    else
    end;
end;

procedure TChicagoTheme.DrawCaption(Widget: TWidget; const Rect: TRectF; Text: string = '');
begin
  if Text = '' then
  	Text := Widget.Text;
  if Widget is TWindow then
  begin
    Title.Size := TitleSize;
    Title.Align := fontLeft;
    Title.Layout := fontMiddle;
    Title.Color := CalcColor(Widget, colorTitle);
    DrawText(Title, Text, Rect.X + 8, Rect.Y + Rect.Height / 2);
  end
  else
    inherited DrawCaption(Widget, Rect, Text);
end;

procedure TChicagoTheme.DrawThinBorder(Widget: TWidget; const Rect: TRectF);
var
  R: TRectF;
begin
  R := Rect.Round;
  if Widget is TSlider then
  begin
    Canvas.MoveTo(R.Right, R.Top);
    Canvas.LineTo(R.Right, R.Bottom);
    Canvas.LineTo(R.Left, R.Bottom);
    Canvas.Stroke(CalcColor(Widget, colorShadow));
    Canvas.MoveTo(R.Left, R.Bottom - 1);
    Canvas.LineTo(R.Left, R.Top);
    Canvas.LineTo(R.Right - 1, R.Top);
    Canvas.Stroke(CalcColor(Widget, colorHighlight));
  end
  else if (wsPressed in Widget.State) or (wsToggled in Widget.State) then
  begin
    Canvas.MoveTo(R.Right, R.Top);
    Canvas.LineTo(R.Right, R.Bottom);
    Canvas.LineTo(R.Left, R.Bottom);
    Canvas.Stroke(CalcColor(Widget, colorHighlight));
    Canvas.MoveTo(R.Left, R.Bottom - 1);
    Canvas.LineTo(R.Left, R.Top);
    Canvas.LineTo(R.Right - 1, R.Top);
    Canvas.Stroke(CalcColor(Widget, colorShadow));
  end
  else if wsHot in Widget.State then
  begin
    Canvas.MoveTo(R.Right, R.Top);
    Canvas.LineTo(R.Right, R.Bottom);
    Canvas.LineTo(R.Left, R.Bottom);
    Canvas.Stroke(CalcColor(Widget, colorShadow));
    Canvas.MoveTo(R.Left, R.Bottom - 1);
    Canvas.LineTo(R.Left, R.Top);
    Canvas.LineTo(R.Right - 1, R.Top);
    Canvas.Stroke(CalcColor(Widget, colorHighlight));
  end;
end;

procedure TChicagoTheme.DrawSunken(Widget: TWidget; const Rect: TRectF);
var
  R: TRectF;
begin
  R := Rect.Round;
  Canvas.MoveTo(R.Right, R.Top);
  Canvas.LineTo(R.Right, R.Bottom);
  Canvas.LineTo(R.Left, R.Bottom);
  Canvas.Stroke(CalcColor(Widget, colorHighlight));
  Canvas.MoveTo(R.Left, R.Bottom - 1);
  Canvas.LineTo(R.Left, R.Top);
  Canvas.LineTo(R.Right - 1, R.Top);
  Canvas.Stroke(CalcColor(Widget, colorDarkShadow));
  Canvas.MoveTo(R.Right - 1, R.Top + 1);
  Canvas.LineTo(R.Right - 1, R.Bottom - 1);
  Canvas.LineTo(R.Left + 1, R.Bottom - 1);
  Canvas.Stroke(CalcColor(Widget, colorFace));
  Canvas.MoveTo(R.Left + 1, R.Bottom - 2);
  Canvas.LineTo(R.Left + 1, R.Top + 1);
  Canvas.LineTo(R.Right - 2, R.Top + 1);
  Canvas.Stroke(CalcColor(Widget, colorShadow));
end;

procedure TChicagoTheme.DrawThickBorder(Widget: TWidget);
var
  R: TRectF;
begin
  R := Widget.Computed.Bounds.Round;
  if (wsPressed in Widget.State) and (Widget is TPushButton) then
    DrawSunken(Widget, R)
  else
  begin
    Canvas.MoveTo(R.Right, R.Top);
    Canvas.LineTo(R.Right, R.Bottom);
    Canvas.LineTo(R.Left, R.Bottom);
    Canvas.Stroke(CalcColor(Widget, colorDarkShadow));
    Canvas.MoveTo(R.Left, R.Bottom - 1);
    Canvas.LineTo(R.Left, R.Top);
    Canvas.LineTo(R.Right - 1, R.Top);
    Canvas.Stroke(CalcColor(Widget, colorHighlight));
    Canvas.MoveTo(R.Right - 1, R.Top + 1);
    Canvas.LineTo(R.Right - 1, R.Bottom - 1);
    Canvas.LineTo(R.Left + 1, R.Bottom - 1);
    Canvas.Stroke(CalcColor(Widget, colorShadow));
    Canvas.MoveTo(R.Left + 1, R.Bottom - 2);
    Canvas.LineTo(R.Left + 1, R.Top + 1);
    Canvas.LineTo(R.Right - 2, R.Top + 1);
    Canvas.Stroke(CalcColor(Widget, colorFace));
  end;
end;

procedure TChicagoTheme.DrawMemo(Widget: TMemo);
var
  R: TRectF;
begin
  R := Widget.Computed.Bounds.Round;
  Canvas.Rect(R);
  Canvas.Fill(CalcColor(Widget, colorHighlight));
  DrawSunken(Widget, R);
  DrawMemoText(Widget, CalcColor(Widget, colorText), CalcColor(Widget, colorActive),
    CalcColor(Widget, colorHighlight));
end;

procedure TChicagoTheme.DrawListFrame(Widget: TWidget; out TextColor, SelectColor,
  SelectTextColor: TColorF);
var
  R: TRectF;
begin
  R := Widget.Computed.Bounds.Round;
  Canvas.Rect(R);
  Canvas.Fill(CalcColor(Widget, colorHighlight));
  DrawSunken(Widget, R);
  TextColor := CalcColor(Widget, colorText);
  SelectColor := CalcColor(Widget, colorActive);
  SelectTextColor := CalcColor(Widget, colorHighlight);
end;

{ A raised header cell like a button, which goes flat with its title moved
  when it is pressed }

procedure TChicagoTheme.DrawHeaderCell(Grid: TScrollGrid; const Rect: TRectF; Col: Integer;
  Hot, Pressed: Boolean);
var
  R, T: TRectF;
  C: TColorF;
begin
  Canvas.Rect(Rect);
  Canvas.Fill(CalcColor(Grid, colorFace));
  if Hot and (not Pressed) then
  begin
    C := CalcColor(Grid, colorHighlight);
    C.Alpha := C.Alpha * 0.4;
    Canvas.Rect(Rect);
    Canvas.Fill(C);
  end;
  { The lines are drawn through the middle of the pixels at the edges }
  R := Rect;
  R.Inflate(-0.5, -0.5);
  T := Rect;
  if Pressed then
  begin
    Canvas.Rect(R);
    Canvas.Stroke(CalcColor(Grid, colorShadow));
    T.Offset(1, 1);
  end
  else
  begin
    Canvas.MoveTo(R.Right, R.Top);
    Canvas.LineTo(R.Right, R.Bottom);
    Canvas.LineTo(R.Left, R.Bottom);
    Canvas.Stroke(CalcColor(Grid, colorDarkShadow));
    Canvas.MoveTo(R.Left, R.Bottom - 1);
    Canvas.LineTo(R.Left, R.Top);
    Canvas.LineTo(R.Right - 1, R.Top);
    Canvas.Stroke(CalcColor(Grid, colorHighlight));
    Canvas.MoveTo(R.Right - 1, R.Top + 1);
    Canvas.LineTo(R.Right - 1, R.Bottom - 1);
    Canvas.LineTo(R.Left + 1, R.Bottom - 1);
    Canvas.Stroke(CalcColor(Grid, colorShadow));
  end;
  DrawHeaderText(Grid, T, Col, CalcColor(Grid, colorText));
end;

{ A light track with a raised square thumb }

procedure TChicagoTheme.DrawScrollBar(Widget: TWidget; const Track, Thumb: TRectF;
  Horizontal: Boolean);
var
  R: TRectF;
  C: TColorF;
begin
  Canvas.Rect(Track);
  Canvas.Fill(CalcColor(Widget, colorHighlight));
  C := CalcColor(Widget, colorFace);
  C.Alpha := C.Alpha * 0.5;
  Canvas.Rect(Track);
  Canvas.Fill(C);
  Canvas.Rect(Thumb);
  Canvas.Fill(CalcColor(Widget, colorFace));
  R := Thumb;
  R.Width := R.Width - 1;
  R.Height := R.Height - 1;
  R := R.Round;
  Canvas.MoveTo(R.Right, R.Top);
  Canvas.LineTo(R.Right, R.Bottom);
  Canvas.LineTo(R.Left, R.Bottom);
  Canvas.Stroke(CalcColor(Widget, colorDarkShadow));
  Canvas.MoveTo(R.Left, R.Bottom - 1);
  Canvas.LineTo(R.Left, R.Top);
  Canvas.LineTo(R.Right - 1, R.Top);
  Canvas.Stroke(CalcColor(Widget, colorHighlight));
  Canvas.MoveTo(R.Right - 1, R.Top + 1);
  Canvas.LineTo(R.Right - 1, R.Bottom - 1);
  Canvas.LineTo(R.Left + 1, R.Bottom - 1);
  Canvas.Stroke(CalcColor(Widget, colorShadow));
end;

procedure TChicagoTheme.DrawEdit(Widget: TEdit);
var
  R: TRectF;
begin
  R := Widget.Computed.Bounds.Round;
  Canvas.Rect(R);
  Canvas.Fill(CalcColor(Widget, colorHighlight));
  DrawSunken(Widget, R);
  DrawEditText(Widget, R, CalcColor(Widget, colorText), CalcColor(Widget, colorActive),
    CalcColor(Widget, colorHighlight));
end;

procedure TChicagoTheme.DrawFocus(Widget: TWidget; const Rect: TRectF);
var
  R: TRectF;
begin
  R := Rect.Round;
  Canvas.Rect(R);
  FFocus.Width := 1;
  (FFocus.Brush as IBitmapBrush).Opacity := Widget.Computed.Opacity;
  Canvas.Stroke(FFocus);
end;

procedure TChicagoTheme.DrawButton(Widget: TPushButton);
var
  R: TRectF;
begin
  R := Widget.Computed.Bounds.Round;
  Canvas.Rect(R);
  Canvas.Fill(CalcColor(Widget, colorFace));
  DrawThickBorder(Widget);
  if wsPressed in Widget.State then
    R.Y := R.Y + 1;
  DrawCaption(Widget, R);
  R.Inflate(-4, -4);
  if wsSelected in Widget.State then
    DrawFocus(Widget, R);
end;

procedure TChicagoTheme.DrawGlyphButton(Widget: TGlyphButton);
var
  R: TRectF;
begin
  R := Widget.Computed.Bounds.Round;
  DrawThinBorder(Widget, R);
  DrawCaption(Widget, R);
end;

procedure TChicagoTheme.DrawGlyphImage(Widget: TGlyphImage);
begin
  DrawCaption(Widget, Widget.Computed.Bounds);
end;

procedure TChicagoTheme.DrawCheckBox(Widget: TCheckBox);
var
  B, R: TRectF;
  P: TPointF;
begin
  B := Widget.Computed.Bounds.Round;
  R := B;
  R.Y := B.MidPoint.Y - 7;
  R.Height := 14;
  R.Width := 14;
  if Widget.Round then
    Canvas.RoundRect(R, 7)
  else
    Canvas.RoundRect(R, 3);
  if wsToggled in Widget.State then
  begin
    if Widget.Round then
    begin
      Canvas.Fill(CalcColor(Widget, colorHighlight));
      if Widget.Checked then
      begin
        R.Inflate(-4, -4);
        Canvas.RoundRect(R, 7);
        Canvas.Fill(CalcColor(Widget, colorText));
      end;
    end
    else
    begin
      Canvas.Fill(CalcColor(Widget, colorHighlight));
      Pen.Color := CalcColor(Widget, colorDarkShadow);
      Pen.Width := 3;
      Pen.LineCap := capButt;
      Pen.LineJoin := joinMiter;
      P := R.MidPoint;
      P.Offset(-1, 1);
      Canvas.MoveTo(P.X - 2.5, P.Y - 2);
      Canvas.LineTo(P.X, P.Y + 1);
      Canvas.LineTo(P.X + 4, P.Y - 4);
      Canvas.Stroke(Pen);
    end
  end
  else
    Canvas.Fill(CalcColor(Widget, colorHighlight));
  R := B;
  R.Y := B.MidPoint.Y - 7;
  R.Height := 14;
  R.Width := 14;
  if Widget.Round then
  begin
    Canvas.RoundRect(R, 7);
    Canvas.Stroke(CalcColor(Widget, colorDarkShadow));
    R.Inflate(-1, -1);
    Canvas.RoundRect(R, 6);
    Canvas.Stroke(CalcColor(Widget, colorShadow));
  end
  else
    DrawSunken(Widget, R);
  R := B;
  R.X := R.X + 16;
  P := MeasureText(Font, Widget.Text);
  R.Width := P.X + 8;
  R.Inflate(-2, 2);
  if wsSelected in Widget.State then
    DrawFocus(Widget, R);
  DrawCaption(Widget, R);
end;

procedure TChicagoTheme.DrawSlider(Widget: TSlider);
var
  R: TRectF;
begin
  R := Widget.Computed.Bounds.Round;
  with R.MidPoint.Round do
    begin
      Canvas.MoveTo(R.Left, Y);
      Canvas.LineTo(R.Right, Y);
    end;
  Canvas.Stroke(CalcColor(Widget, colorShadow));
  R := Widget.GripRect.Round;
  with Widget.Computed.Bounds do
    R.Offset(X, Y);
  Canvas.Rect(R);
  Canvas.Fill(CalcColor(Widget, colorFace));
  DrawThinBorder(Widget, R);
end;

procedure TChicagoTheme.DrawSpinBox(Widget: TSpinBox);
var
  R: TRectF;
begin
  R := Widget.Computed.Bounds.Round;
  Canvas.Rect(R);
  Canvas.Fill(CalcColor(Widget, colorFace));
  DrawThickBorder(Widget);
  DrawCaption(Widget, R, Widget.Prefix + Widget.Text);
  Glyph.Color := Font.Color;
  Glyph.Size := Font.Size + 2;
  DrawText(Glyph, '󰅁', R.Left + 10, R.MidPoint.Y);
  DrawText(Glyph, '󰅂', R.Right - 10, R.MidPoint.Y);
end;

procedure TChicagoTheme.DropColors(Widget: TSpinBox; out Frame, Back, Hot, Text,
  HotText: TColorF);
begin
  Frame := colorBlack;
  Back := colorWhite;
  Hot := ARGB($FF0000D0);
  Text := colorBlack;
  HotText := colorWhite;
end;

procedure TChicagoTheme.PostDrawSpinBox(Widget: TSpinBox);
var
  R: TRectF;
  P: TPointF;
  S: Integer;
  I: Integer;
  C: TColorF;
begin
	R := Widget.ItemRect(-1);
  if R.Empty then
  	Exit;
  Canvas.Rect(R);
  Canvas.Fill(colorBlack);
	R.Inflate(-1, -1);
  Canvas.Rect(R);
  Canvas.Fill(colorWhite);
	R := Widget.ItemRect(0);
  P := Widget.Main.MouseFor(Widget);
  S := Widget.ItemFromPoint(P);
  C := Font.Color;
  for I := 0 to Widget.Items.Length - 1 do
  begin
    if I = S then
    begin
      Canvas.Rect(R);
      Canvas.Fill(ARGB($FF0000D0));
      Font.Color := colorWhite;
    end;
    with R.MidPoint do
	    DrawText(Font, Widget.Items[I], X, Y);
    R.Y := R.Bottom + 1;
    Font.Color := C;
  end;
  Font.Color := C;
end;

procedure TChicagoTheme.DrawLabel(Widget: TLabel);
begin
  DrawCaption(Widget, Widget.Computed.Bounds);
end;

{ A raised gray button with a black cross at the right of the title bar }

function TChicagoTheme.CalcCloseRect(Window: TWindow): TRectF;
begin
  Result := NewRectF(Window.Width - 16 - 5, 6, 16, 14);
end;

procedure TChicagoTheme.DrawClose(Window: TWindow; const Rect: TRectF; Hot, Pressed: Boolean);
var
  R: TRectF;
begin
  Canvas.Rect(Rect);
  Canvas.Fill(CalcColor(Window, colorFace));
  R := Rect;
  R.Width := R.Width - 1;
  R.Height := R.Height - 1;
  R := R.Round;
  { The edges swap when the button is pressed so it looks sunken }
  Canvas.MoveTo(R.Right, R.Top);
  Canvas.LineTo(R.Right, R.Bottom);
  Canvas.LineTo(R.Left, R.Bottom);
  if Pressed then
    Canvas.Stroke(CalcColor(Window, colorHighlight))
  else
    Canvas.Stroke(CalcColor(Window, colorDarkShadow));
  Canvas.MoveTo(R.Left, R.Bottom - 1);
  Canvas.LineTo(R.Left, R.Top);
  Canvas.LineTo(R.Right - 1, R.Top);
  if Pressed then
    Canvas.Stroke(CalcColor(Window, colorDarkShadow))
  else
    Canvas.Stroke(CalcColor(Window, colorHighlight));
  R := Rect;
  if Pressed then
    R.Offset(1, 1);
  R.Inflate(-1, 0);
  DrawCross(R, CalcColor(Window, colorText), 1.5, 4);
end;

procedure TChicagoTheme.DrawWindow(Widget: TWindow);
var
  C: TColorF;
  R: TRectF;
begin
  R := Widget.Computed.Bounds.Round;
  Canvas.Rect(R);
  C := CalcColor(Widget, colorBase);
  C.Alpha := C.Alpha * (1 - Clamp(Widget.Fade));
  Canvas.Fill(C);
  DrawThickBorder(Widget);
  R.Height := CalcSize(Widget, tpCaption).Y;
  R.Inflate(-2, -2);
  Canvas.Rect(R);
  C := CalcColor(Widget, colorCaption);
  if Widget.Main.ActiveWindow <> Widget then
    C.Alpha := C.Alpha * 0.5;
  Canvas.Fill(C);
  DrawCaption(Widget, R);
end;

procedure TChicagoTheme.DrawContainer(Widget: TContainerWidget);
var
  C: TColorF;
begin
  if Widget.Parent is TMainWidget then
  begin
    Canvas.Rect(Widget.Computed.Bounds.Round);
    C := CalcColor(Widget, colorBase);
    C.Alpha := C.Alpha * (1 - Clamp(Widget.Fade));
    Canvas.Fill(C, True);
    DrawThickBorder(Widget);
  end;
end;

{ TGraphiteTheme }

procedure TGraphiteTheme.Init(Canvas: ICanvas);
begin
  inherited Init(Canvas);
  Font := Canvas.LoadFontAsset('NotoSans', FontRes + '/NotoSans-Regular.ttf');
  Font.Size := FontSize;
  Font.Color := colorWhite;
  Font.Align := fontCenter;
  Font.Layout := fontMiddle;
  Glyph := Canvas.LoadFontAsset('glyph', FontRes + '/materialdesignicons-webfont.ttf');
  Glyph.Size := GlyphSize;
  Glyph.Color := colorWhite;
  Glyph.Align := fontCenter;
  Glyph.Layout := fontMiddle;
  Title := Canvas.LoadFontAsset('NotoSans-Bold', FontRes + '/NotoSans-Bold.ttf');
  Title.Size := TitleSize;
  Title.Color := colorWhite;
  Title.Align := fontCenter;
  Title.Layout := fontMiddle;
  FBrush := NewBrush(NewPointF(0, 0), NewPointF(0, 0));
end;

function TGraphiteTheme.FontSize: Float;
begin
  Result := 14;
end;

function TGraphiteTheme.GlyphSize: Float;
begin
  Result := 22;
end;

function TGraphiteTheme.TitleSize: Float;
begin
  Result := 15;
end;

function TGraphiteTheme.CalcColor(Widget: TWidget; Color: TThemeColor): TColorF;
begin
  Result := ARGB(Palette(Widget, Color));
end;

function TGraphiteTheme.Palette(Widget: TWidget; Color: TThemeColor): LongWord;
var
  A: LongWord;
begin
  if Widget.Computed.Enabled then
    case Color of
      colorBase: Result := $CCCCCC;
      colorFace: Result := $DDDDDD;
      colorBorder: Result := $222222;
      colorActive: Result := $D79852;
      colorShadow: Result := $AAAAAA;
      colorDarkShadow: Result := $999999;
      colorPressed: Result := $707070;
      colorSelected: Result := $D79852;
      colorHot: Result := $D09050;
      colorCaption: Result := $909090;
      colorTitle: Result := $404040;
      colorText: Result := $303030;
    else
      Result := 0;
    end
  else
    case Color of
      colorBase: Result := $CCCCCC;
      colorFace: Result := $DDDDDD;
      colorBorder: Result := $222222;
      colorActive: Result := $D79852;
      colorShadow: Result := $CCCCCC;
      colorDarkShadow: Result := $BBBBBB;
      colorPressed: Result := $707070;
      colorSelected: Result := $D79852;
      colorHot: Result := $C0A070;
      colorCaption: Result := $999999;
      colorTitle: Result := $505050;
      colorText: Result := $888888;
    end;
  if Color = colorBorder then
    A := Round($FF * Widget.Computed.Opacity * 0.5) shl 24
  else if Color = colorSelected then
    A := Round($FF * Widget.Computed.Opacity * 0.8) shl 24
  else
    A := Round($FF * Widget.Computed.Opacity) shl 24;
  Result := Result or A;
end;

function TGraphiteTheme.CalcSize(Widget: TWidget; Part: TThemePart): TSizeF;
var
  M: Float;
begin
  Result := NewPointF(0, 0);
  { Entire widget defaiult sizes }
  if Part = tpEverything then
  begin
    if Widget is TSpacer then
      Result := NewPointF(8, 8)
    else if Widget is TWindow then
      Result := NewPointF(400, 300)
    else if Widget is TContainerWidget then
      Result := NewPointF(10, 10)
    else if Widget is TMemo then
      Result := NewPointF(260, 100)
    else if Widget is TEdit then
      Result := Scaled(NewPointF(160, 30))
    else if Widget is TPushButton then
    begin
      Result.X := MeasureText(Font, '[ ' + Widget.Text + ' ]').X;
      Result.Y := Scaled(30);
      if Result.X < Scaled(80) then
        Result.X := Scaled(80);
    end
    else if Widget is TGlyphButton then
      Result := Scaled(NewPointF(30, 30))
    else if Widget is TGlyphImage then
      Result := Scaled(NewPointF(48, 48))
    else if Widget is TCheckBox then
    begin
      Result.X := MeasureText(Font, '[ ' + Widget.Text + ' ]').X + 24;
      Result.Y := Scaled(24);
    end
    else if Widget is TSlider then
      Result := Scaled(NewPointF(150, 20))
    else if Widget is TSpinBox then
      Result := Scaled(NewPointF(150, 24))
    else if Widget is TLabel then
    begin
      M := TLabel(Widget).MaxWidth;
      { Realted to issue detailed in TCanvas.MeasureMemo }
      Font.Size := FontSize;
      Font.Align := fontLeft;
      Font.Layout := fontMiddle;
      if M < 1 then
      begin
        Result := MeasureText(Font, Widget.Text);
        Result.X := Result.X + 4;
        Result.Y := Result.Y + 4;
      end
      else
      begin
        Result := MeasureText(Font, Widget.Text);
        Result.X := Result.X + 4;
        if Result.X > M then
        begin
          Result.X := M + 4;
          Result.Y := Canvas.MeasureMemo(Font, Widget.Text, M);
        end;
        Result.Y := Result.Y + 4;
      end;
    end;
    Exit;
  end;
  { Indentation }
  if Part = tpIndent then
  begin
    Result := Scaled(NewPointF(16, 0));
    Exit;
  end;
  { TWindow parts }
  if Widget is TWindow then
    case Part of
      tpCaption:
        begin
          Result.X := Widget.Width;
          Result.Y := Scaled(30);
        end;
    else
    end
  { TSlider parts }
  else if Widget is TSlider then
    case Part of
      tpThumb: Result := Scaled(NewPointF(14, 14));
    else
    end
  { TCheckBox parts }
  else if Widget is TCheckBox then
    case Part of
      tpNode:
          Result := NewPointF(18, 18);
      tpCaption:
        begin
          Result := MeasureText(Font, '[ ' + Widget.Text + ' ]');
          Result.X := Result.X + 6;
          Result.Y := Result.Y + 6;
        end;
    else
    end;
end;

procedure TGraphiteTheme.DrawMemo(Widget: TMemo);
var
  R: TRectF;
  C: TColorF;
begin
  R := Widget.Computed.Bounds.Round;
  Canvas.RoundRect(R, 4);
  Canvas.Fill(Widget.Color(colorFace), True);
  if wsSelected in Widget.State then
    Canvas.Stroke(Widget.Color(colorActive), 1)
  else
    Canvas.Stroke(Widget.Color(colorDarkShadow), 1);
  C := Widget.Color(colorActive);
  C.Alpha := C.Alpha * 0.5;
  DrawMemoText(Widget, Widget.Color(colorText), C, Widget.Color(colorText));
end;

procedure TGraphiteTheme.DrawListFrame(Widget: TWidget; out TextColor, SelectColor,
  SelectTextColor: TColorF);
var
  R: TRectF;
  C: TColorF;
begin
  R := Widget.Computed.Bounds.Round;
  Canvas.RoundRect(R, 4);
  Canvas.Fill(Widget.Color(colorFace), True);
  if wsSelected in Widget.State then
    Canvas.Stroke(Widget.Color(colorActive), 1)
  else
    Canvas.Stroke(Widget.Color(colorDarkShadow), 1);
  C := Widget.Color(colorActive);
  C.Alpha := C.Alpha * 0.5;
  TextColor := Widget.Color(colorText);
  SelectColor := C;
  SelectTextColor := Widget.Color(colorText);
end;

{ A header cell with the gradient of a button, tinted under the mouse and
  flat when it is pressed }

procedure TGraphiteTheme.DrawHeaderCell(Grid: TScrollGrid; const Rect: TRectF; Col: Integer;
  Hot, Pressed: Boolean);
var
  C: TColorF;
begin
  Canvas.Rect(Rect);
  if Pressed then
    Canvas.Fill(Grid.Color(colorShadow))
  else
  begin
    FBrush.A := Rect.Sector(2);
    FBrush.B := Rect.Sector(8);
    FBrush.NearStop.Color := Grid.Color(colorFace);
    FBrush.FarStop.Color := Grid.Color(colorShadow);
    FBrush.FarStop.Offset := 0.8;
    Canvas.Fill(FBrush);
    if Hot then
    begin
      C := Grid.Color(colorHot);
      C.Alpha := C.Alpha * 0.3;
      Canvas.Rect(Rect);
      Canvas.Fill(C);
    end;
  end;
  DrawHeaderLines(Rect, Grid.Color(colorDarkShadow));
  DrawHeaderText(Grid, Rect, Col, Grid.Color(colorText));
end;

{ A recessed track with a rounded gradient thumb }

procedure TGraphiteTheme.DrawScrollBar(Widget: TWidget; const Track, Thumb: TRectF;
  Horizontal: Boolean);
var
  R: TRectF;
begin
  Canvas.RoundRect(Track, 4);
  Canvas.Fill(Widget.Color(colorShadow));
  R := Thumb;
  R.Inflate(-2, -2);
  R := R.Round;
  { The gradient runs across the thumb }
  if Horizontal then
  begin
    FBrush.A := R.Sector(2);
    FBrush.B := R.Sector(8);
  end
  else
  begin
    FBrush.A := R.Sector(4);
    FBrush.B := R.Sector(6);
  end;
  FBrush.NearStop.Color := Widget.Color(colorFace);
  FBrush.FarStop.Color := Widget.Color(colorShadow);
  FBrush.FarStop.Offset := 0.8;
  Canvas.RoundRect(R, 4);
  Canvas.Fill(FBrush, True);
  Canvas.Stroke(Widget.Color(colorDarkShadow), 1);
end;

procedure TGraphiteTheme.DrawEdit(Widget: TEdit);
var
  R: TRectF;
  C: TColorF;
begin
  R := Widget.Computed.Bounds.Round;
  Canvas.RoundRect(R, 4);
  Canvas.Fill(Widget.Color(colorFace), True);
  if wsSelected in Widget.State then
    Canvas.Stroke(Widget.Color(colorActive), 1)
  else
    Canvas.Stroke(Widget.Color(colorDarkShadow), 1);
  C := Widget.Color(colorActive);
  C.Alpha := C.Alpha * 0.5;
  DrawEditText(Widget, R, Widget.Color(colorText), C, Widget.Color(colorText));
end;

procedure TGraphiteTheme.DrawButton(Widget: TPushButton);
var
  R: TRectF;
begin
  R := Widget.Computed.Bounds.Round;
  FBrush.A := R.Sector(2);
  FBrush.B := R.Sector(8);
  FBrush.NearStop.Color := Widget.Color(colorFace);
  FBrush.FarStop.Color := Widget.Color(colorShadow);
  FBrush.FarStop.Offset := 0.8;
  Canvas.RoundRect(R, 6);
  if wsHot in Widget.State then
    if wsPressed in Widget.State then
    begin
      Canvas.Fill(Widget.Color(colorShadow), True);
      Canvas.Stroke(Widget.Color(colorDarkShadow), 1, True);
    end
    else
    begin
      Canvas.Fill(FBrush, True);
      Canvas.Stroke(Widget.Color(colorHot), 1, True)
    end
  else
  begin
    Canvas.Fill(FBrush, True);
    Canvas.Stroke(Widget.Color(colorDarkShadow), 1);
    if wsSelected in Widget.State then
    begin
      R.Inflate(-3, -3);
      Canvas.RoundRect(R, 4);
      Canvas.Stroke(Widget.Color(colorSelected), 1);
      R.Inflate(3, 3);
    end;
  end;
  R.Offset(0, 1);
  DrawCaption(Widget, R);
end;

procedure TGraphiteTheme.DrawGlyphButton(Widget: TGlyphButton);
var
  R: TRectF;
begin
  R := Widget.Computed.Bounds.Round;
  FBrush.A := R.Sector(2);
  FBrush.B := R.Sector(8);
  FBrush.NearStop.Color := Widget.Color(colorFace);
  FBrush.FarStop.Color := Widget.Color(colorShadow);
  FBrush.FarStop.Offset := 0.8;
  Canvas.RoundRect(R, 6);
  if (wsPressed in Widget.State) or (wsToggled in Widget.State) then
  begin
    Canvas.Fill(Widget.Color(colorDarkShadow), True);
    Canvas.Stroke(Widget.Color(colorPressed), 1, True);
  end
  else if wsHot in Widget.State then
  begin
    Canvas.Fill(FBrush, True);
    Canvas.Stroke(Widget.Color(colorHot), 1, True)
  end;
  R.Offset(0, 1);
  DrawCaption(Widget, R);
end;

procedure TGraphiteTheme.DrawGlyphImage(Widget: TGlyphImage);
var
  R: TRectF;
begin
  R := Widget.Computed.Bounds.Round;
  DrawCaption(Widget, R);
end;

procedure TGraphiteTheme.DrawCheckBox(Widget: TCheckBox);
var
  B, R: TRectF;
  P: TPointF;
begin
  B := Widget.Computed.Bounds.Round;
  R := B;
  R.Y := B.MidPoint.Y - 7;
  R.Height := 14;
  R.Width := 14;
  if Widget.Round then
    Canvas.RoundRect(R, 7)
  else
    Canvas.RoundRect(R, 3);
  if wsToggled in Widget.State then
  begin
    Canvas.Fill(CalcColor(Widget, colorBase));
    if Widget.Round then
    begin
      R.Inflate(-4, -4);
      Canvas.RoundRect(R, 7);
      Canvas.Fill(CalcColor(Widget, colorHot));
    end
    else
    begin
      Pen.Color := CalcColor(Widget, colorHot);
      Pen.Width := 3;
      Pen.LineCap := capButt;
      Pen.LineJoin := joinMiter;
      P := R.MidPoint;
      P.Offset(-1, 1);
      Canvas.MoveTo(P.X - 2.5, P.Y - 2);
      Canvas.LineTo(P.X, P.Y + 2);
      Canvas.LineTo(P.X + 5, P.Y - 5);
      Canvas.Stroke(Pen);
    end;
  end
  else
    Canvas.Fill(CalcColor(Widget, colorDarkShadow));
  R := B;
  R.Y := B.MidPoint.Y - 7;
  R.Height := 14;
  R.Width := 14;
  if Widget.Round then
    Canvas.RoundRect(R, 7)
  else
    Canvas.RoundRect(R, 3);
  Canvas.Stroke(CalcColor(Widget, colorDarkShadow), 1);
  R := B;
  R.X := R.X + 16;
  P := MeasureText(Font, Widget.Text);
  R.Width := P.X + 8;
  R.Inflate(-2, 2);
  if wsSelected in Widget.State then
  begin
    Canvas.RoundRect(R, 3);
    Canvas.Stroke(CalcColor(Widget, colorSelected), 1);
  end;
  DrawCaption(Widget, R);
end;

procedure TGraphiteTheme.DrawSlider(Widget: TSlider);
var
  R, S: TRectF;
begin
  R := Widget.GripRect.Round;
  with Widget.Computed.Bounds do
    R.Offset(X, Y);
  R := R.Round;
  S := Widget.Computed.Bounds.Round;
  S.Y := R.MidPoint.Y;
  S.Height := 0;
  S.Inflate(2, R.Height / 2 + 1);
  Canvas.RoundRect(S, S.Height / 2);
  Canvas.Fill(Widget.Color(colorShadow), True);
  Canvas.Stroke(Widget.Color(colorDarkShadow));
  with R.MidPoint do
    Canvas.Circle(X, Y, R.Width / 2);
  Canvas.Fill(Widget.Color(colorFace), True);
  if wsPressed in Widget.State then
    Canvas.Fill(Widget.Color(colorHot))
  else if wsHot in Widget.State then
    Canvas.Stroke(Widget.Color(colorHot))
  else
    Canvas.Stroke(Widget.Color(colorDarkShadow));
end;

procedure TGraphiteTheme.DrawSpinBox(Widget: TSpinBox);
var
  R: TRectF;
begin
  R := Widget.Computed.Bounds.Round;
  FBrush.A := R.Sector(2);
  FBrush.B := R.Sector(8);
  FBrush.NearStop.Color := Widget.Color(colorFace);
  FBrush.FarStop.Color := Widget.Color(colorShadow);
  FBrush.FarStop.Offset := 0.8;
  Canvas.RoundRect(R, 6);
  if wsPressed in Widget.State then
  begin
    Canvas.Fill(Widget.Color(colorShadow), True);
    Canvas.Stroke(Widget.Color(colorDarkShadow), 1, True);
  end
  else if wsHot in Widget.State then
  begin
    Canvas.Fill(FBrush, True);
    Canvas.Stroke(Widget.Color(colorHot), 1, True)
  end
  else
  begin
    Canvas.Fill(FBrush, True);
    Canvas.Stroke(Widget.Color(colorDarkShadow), 1);
    if wsSelected in Widget.State then
    begin
      R.Inflate(-3, -3);
      Canvas.RoundRect(R, 4);
      Canvas.Stroke(Widget.Color(colorSelected), 1);
      R.Inflate(3, 3);
    end;
  end;
  R.Offset(0, 1);
  DrawCaption(Widget, R, Widget.Prefix + Widget.Text);
  Glyph.Color := Font.Color;
  Glyph.Size := Font.Size + 2;
  DrawText(Glyph, '󰅁', R.Left + 10, R.MidPoint.Y);
  DrawText(Glyph, '󰅂', R.Right - 10, R.MidPoint.Y);
end;

procedure TGraphiteTheme.DropColors(Widget: TSpinBox; out Frame, Back, Hot, Text,
  HotText: TColorF);
begin
  Frame := ARGB($40000000);
  Back := CalcColor(Widget, colorFace);
  Hot := CalcColor(Widget, colorActive);
  Text := CalcColor(Widget, colorText);
  HotText := Text;
end;

procedure TGraphiteTheme.PostDrawSpinBox(Widget: TSpinBox);
var
  R: TRectF;
  P: TPointF;
  S: Integer;
  I: Integer;
begin
	R := Widget.ItemRect(-1);
  if R.Empty then
  	Exit;
  Canvas.Rect(R);
  Canvas.Fill(ARGB($40000000));
	R.Inflate(-1, -1);
  Canvas.Rect(R);
  Canvas.Fill(CalcColor(Widget, colorFace));
	R := Widget.ItemRect(0);
  P := Widget.Main.MouseFor(Widget);
  S := Widget.ItemFromPoint(P);
  for I := 0 to Widget.Items.Length - 1 do
  begin
    if I = S then
    begin
      Canvas.Rect(R);
      Canvas.Fill(CalcColor(Widget, colorActive));
    end;
    with R.MidPoint do
	    DrawText(Font, Widget.Items[I], X, Y);
    R.Y := R.Bottom + 1;
  end;
end;

procedure TGraphiteTheme.DrawLabel(Widget: TLabel);
begin
  DrawCaption(Widget, Widget.Computed.Bounds);
end;

{ A round gray button with a dark cross at the right of the title bar }

function TGraphiteTheme.CalcCloseRect(Window: TWindow): TRectF;
const
  Size = 20;
var
  H: Float;
begin
  { The title bar is drawn 4 pixels shorter than the caption size. The window
    border covers its top pixel, so the middle of what is seen is a pixel
    lower than the middle of the bar. }
  H := CalcSize(Window, tpCaption).Y - 4;
  Result := NewRectF(Window.Width - Size - 6, (H - Size) / 2 + 1, Size, Size);
end;

procedure TGraphiteTheme.DrawClose(Window: TWindow; const Rect: TRectF; Hot, Pressed: Boolean);
var
  P: TPointF;
begin
  P := Rect.MidPoint;
  Canvas.Circle(P.X, P.Y, Rect.Width / 2);
  if Pressed then
    Canvas.Fill(Window.Color(colorDarkShadow), True)
  else if Hot then
    Canvas.Fill(Window.Color(colorHot), True)
  else
    Canvas.Fill(Window.Color(colorFace), True);
  Canvas.Stroke(Window.Color(colorDarkShadow), 1);
  DrawCross(Rect, Window.Color(colorTitle), 1.5, 6.5);
end;

procedure TGraphiteTheme.DrawWindow(Widget: TWindow);
var
  R: TRectF;
  C: TColorF;
begin
  R := Widget.Computed.Bounds.Round;
  Canvas.RoundRect(R, 8);
  C := CalcColor(Widget, colorBase);
  C.Alpha := C.Alpha * (1 - Clamp(Widget.Fade));
  Canvas.Fill(C);
  R.Height := CalcSize(Widget, tpCaption).Y - 4;
  Canvas.RoundRectVarying(R, 8, 8, 0, 0);
  C := Widget.Color(colorCaption);
  if Widget.Main.ActiveWindow <> Widget then
    C.Alpha := C.Alpha * 0.25;
  Canvas.Fill(C);
  R.Y := R.Y + 2;
  DrawCaption(Widget, R);
  R.Y := R.Y - 2;
  R.Y := R.Bottom;
  R.Height := 1;
  Canvas.Rect(R);
  Canvas.Fill(Widget.Color(colorBorder));
  R := Widget.Computed.Bounds.Round;
  Canvas.RoundRect(R, 8);
  Canvas.Stroke(Widget.Color(colorBorder));
end;

procedure TGraphiteTheme.DrawContainer(Widget: TContainerWidget);
var
  C: TColorF;
begin
  if Widget.Parent is TMainWidget then
  begin
    Canvas.RoundRect(Widget.Computed.Bounds.Round, 8);
    C := CalcColor(Widget, colorBase);
    C.Alpha := C.Alpha * (1 - Clamp(Widget.Fade));
    Canvas.Fill(C, True);
    Canvas.Stroke(CalcColor(Widget, colorBorder));
  end;
end;

{ TDesktopTheme }

procedure TDesktopTheme.Init(Canvas: ICanvas);
begin
  inherited Init(Canvas);
  FBrush := NewBrush(NewPointF(0, 0), NewPointF(0, 0));
end;

procedure TDesktopTheme.LoadFonts(const FontName, FontFile, TitleName, TitleFile: string);
begin
  Font := Canvas.LoadFontAsset(FontName, FontRes + '/' + FontFile);
  Font.Size := FontSize;
  Font.Color := colorBlack;
  Font.Align := fontCenter;
  Font.Layout := fontMiddle;
  Glyph := Canvas.LoadFontAsset('glyph', FontRes + '/materialdesignicons-webfont.ttf');
  Glyph.Size := GlyphSize;
  Glyph.Color := colorBlack;
  Glyph.Align := fontCenter;
  Glyph.Layout := fontMiddle;
  Title := Canvas.LoadFontAsset(TitleName, FontRes + '/' + TitleFile);
  Title.Size := TitleSize;
  Title.Color := colorBlack;
  Title.Align := fontCenter;
  Title.Layout := fontMiddle;
end;

function TDesktopTheme.WindowBorder: TSizeF;
begin
  Result := NewPointF(0, 0);
end;

function TDesktopTheme.GlyphSize: Float;
begin
  Result := 22;
end;

function TDesktopTheme.Shade(Widget: TWidget; RGB: LongWord; Alpha: Float = 1): TColorF;
begin
  Result := ARGB($FF000000 or (RGB and $FFFFFF));
  Result.Alpha := Alpha * Widget.Computed.Opacity;
end;

function TDesktopTheme.CalcColor(Widget: TWidget; Color: TThemeColor): TColorF;
begin
  Result := Shade(Widget, Palette(Widget, Color));
end;

procedure TDesktopTheme.VertGradient(const R: TRectF; const Top, Bottom: TColorF);
begin
  FBrush.A := R.Sector(2);
  FBrush.B := R.Sector(8);
  FBrush.NearStop.Offset := 0;
  FBrush.NearStop.Color := Top;
  FBrush.FarStop.Offset := 1;
  FBrush.FarStop.Color := Bottom;
end;

procedure TDesktopTheme.HorzGradient(const R: TRectF; const Left, Right: TColorF);
begin
  FBrush.A := R.Sector(4);
  FBrush.B := R.Sector(6);
  FBrush.NearStop.Offset := 0;
  FBrush.NearStop.Color := Left;
  FBrush.FarStop.Offset := 1;
  FBrush.FarStop.Color := Right;
end;

procedure TDesktopTheme.CheckMark(const R: TRectF; const C: TColorF; Width: Float);
var
  P: TPointF;
begin
  Pen.Color := C;
  Pen.Width := Width;
  Pen.LineCap := capRound;
  Pen.LineJoin := joinRound;
  P := R.MidPoint;
  Canvas.MoveTo(P.X - R.Width * 0.28, P.Y);
  Canvas.LineTo(P.X - R.Width * 0.07, P.Y + R.Height * 0.22);
  Canvas.LineTo(P.X + R.Width * 0.3, P.Y - R.Height * 0.25);
  Canvas.Stroke(Pen);
end;

{ The box of a check box sits at the left, centered from top to bottom }

function TDesktopTheme.CheckRect(Widget: TCheckBox; Size: Float): TRectF;
var
  B: TRectF;
begin
  B := Widget.Computed.Bounds;
  Result.X := Trunc(B.X) + 0.5;
  Result.Y := Trunc(B.MidPoint.Y - Size / 2) + 0.5;
  Result.Width := Size;
  Result.Height := Size;
end;

procedure TDesktopTheme.CheckCaption(Widget: TCheckBox; Size: Float);
var
  R, F: TRectF;
  P: TPointF;
begin
  Font.Size := FontSize;
  R := Widget.Computed.Bounds.Round;
  R.X := R.X + Size + 2;
  P := MeasureText(Font, Widget.Text);
  R.Width := P.X + 8;
  if wsSelected in Widget.State then
  begin
    F := R;
    F.Inflate(0, -2);
    Canvas.RoundRect(F, 2);
    Canvas.Stroke(Shade(Widget, Palette(Widget, colorText), 0.3), 1);
  end;
  DrawCaption(Widget, R);
end;

procedure TDesktopTheme.SpinArrows(Widget: TSpinBox; const R: TRectF; const C: TColorF);
begin
  Glyph.Color := C;
  Glyph.Size := FontSize + 2;
  Glyph.Align := fontCenter;
  Glyph.Layout := fontMiddle;
  DrawText(Glyph, '󰅁', R.Left + 10, R.MidPoint.Y);
  DrawText(Glyph, '󰅂', R.Right - 10, R.MidPoint.Y);
end;

{ Three short lines across the middle of a scroll bar thumb }

procedure TDesktopTheme.GripLines(const R: TRectF; Horizontal: Boolean; const C: TColorF);
var
  P: TPointF;
  V: Float;
  I: Integer;
begin
  if (Horizontal and (R.Width < 18)) or ((not Horizontal) and (R.Height < 18)) then
    Exit;
  P := R.MidPoint;
  for I := -1 to 1 do
    if Horizontal then
    begin
      V := Trunc(P.X) + I * 3 + 0.5;
      Canvas.MoveTo(V, P.Y - 3);
      Canvas.LineTo(V, P.Y + 3);
    end
    else
    begin
      V := Trunc(P.Y) + I * 3 + 0.5;
      Canvas.MoveTo(P.X - 3, V);
      Canvas.LineTo(P.X + 3, V);
    end;
  Canvas.Stroke(C, 1);
end;

function TDesktopTheme.CalcSize(Widget: TWidget; Part: TThemePart): TSizeF;
var
  M: Float;
begin
  Result := NewPointF(0, 0);
  if Part = tpEverything then
  begin
    if Widget is TSpacer then
      Result := NewPointF(8, 8)
    else if Widget is TWindow then
      Result := NewPointF(400, 300)
    else if Widget is TContainerWidget then
      Result := NewPointF(10, 10)
    else if Widget is TMemo then
      Result := NewPointF(260, 100)
    else if Widget is TEdit then
      Result := NewPointF(Scaled(160), ButtonHeight)
    else if Widget is TPushButton then
    begin
      Font.Size := FontSize;
      Result.X := MeasureText(Font, '[ ' + Widget.Text + ' ]').X + 8;
      Result.Y := ButtonHeight;
      if Result.X < Scaled(80) then
        Result.X := Scaled(80);
    end
    else if Widget is TGlyphButton then
      Result := Scaled(NewPointF(30, 30))
    else if Widget is TGlyphImage then
      Result := Scaled(NewPointF(48, 48))
    else if Widget is TCheckBox then
    begin
      Font.Size := FontSize;
      Result.X := MeasureText(Font, '[ ' + Widget.Text + ' ]').X + 24;
      Result.Y := Scaled(22);
    end
    else if Widget is TSlider then
      Result := Scaled(NewPointF(150, 22))
    else if Widget is TSpinBox then
      Result := NewPointF(Scaled(150), ButtonHeight)
    else if Widget is TLabel then
    begin
      M := TLabel(Widget).MaxWidth;
      Font.Size := FontSize;
      Font.Align := fontLeft;
      Font.Layout := fontMiddle;
      Result := MeasureText(Font, Widget.Text);
      Result.X := Result.X + 4;
      if (M >= 1) and (Result.X > M) then
      begin
        Result.X := M + 4;
        Result.Y := Canvas.MeasureMemo(Font, Widget.Text, M);
      end;
      Result.Y := Result.Y + 4;
    end;
    Exit;
  end;
  if Part = tpIndent then
  begin
    Result := Scaled(NewPointF(16, 0));
    Exit;
  end;
  if Widget is TWindow then
  begin
    if Part = tpCaption then
    begin
      Result.X := Widget.Width;
      Result.Y := CaptionHeight;
    end
    else if Part = tpBorder then
      Result := WindowBorder;
  end
  else if Widget is TSlider then
  begin
    if Part = tpThumb then
      Result := ThumbSize;
  end
  else if Widget is TCheckBox then
    case Part of
      tpNode: Result := NewPointF(18, 18);
      tpCaption:
        begin
          Font.Size := FontSize;
          Result := MeasureText(Font, '[ ' + Widget.Text + ' ]');
          Result.X := Result.X + 6;
          Result.Y := Result.Y + 6;
        end;
    else
    end;
end;

procedure TDesktopTheme.DrawGlyphButton(Widget: TGlyphButton);
var
  R: TRectF;
begin
  R := Widget.Computed.Bounds.Round;
  if (wsPressed in Widget.State) or (wsToggled in Widget.State) then
  begin
    Canvas.RoundRect(R, 3);
    Canvas.Fill(Shade(Widget, Palette(Widget, colorShadow), 0.6), True);
    Canvas.Stroke(CalcColor(Widget, colorDarkShadow));
  end
  else if wsHot in Widget.State then
  begin
    Canvas.RoundRect(R, 3);
    Canvas.Fill(Shade(Widget, $FFFFFF, 0.5), True);
    Canvas.Stroke(CalcColor(Widget, colorDarkShadow));
  end;
  DrawCaption(Widget, R);
end;

procedure TDesktopTheme.DrawGlyphImage(Widget: TGlyphImage);
begin
  DrawCaption(Widget, Widget.Computed.Bounds.Round);
end;

procedure TDesktopTheme.DrawLabel(Widget: TLabel);
begin
  DrawCaption(Widget, Widget.Computed.Bounds);
end;

procedure TDesktopTheme.DrawContainer(Widget: TContainerWidget);
var
  C: TColorF;
begin
  if Widget.Parent is TMainWidget then
  begin
    Canvas.RoundRect(Widget.Computed.Bounds.Round, WindowRadius);
    C := CalcColor(Widget, colorBase);
    C.Alpha := C.Alpha * (1 - Clamp(Widget.Fade));
    Canvas.Fill(C, True);
    Canvas.Stroke(CalcColor(Widget, colorBorder));
  end;
end;

{ The list which drops down from a spin box, with the item under the mouse
  highlighted }

procedure TDesktopTheme.PostDrawSpinBox(Widget: TSpinBox);
var
  R: TRectF;
  S, I: Integer;
begin
  R := Widget.ItemRect(-1);
  if R.Empty then
    Exit;
  Canvas.Rect(R);
  Canvas.Fill(CalcColor(Widget, colorDarkShadow));
  R.Inflate(-1, -1);
  Canvas.Rect(R);
  Canvas.Fill(CalcColor(Widget, colorHighlight));
  R := Widget.ItemRect(0);
  S := Widget.ItemFromPoint(Widget.Main.MouseFor(Widget));
  Font.Size := FontSize;
  Font.Align := fontCenter;
  Font.Layout := fontMiddle;
  for I := 0 to Widget.Items.Length - 1 do
  begin
    if I = S then
    begin
      Canvas.Rect(R);
      Canvas.Fill(CalcColor(Widget, colorActive));
      Font.Color := CalcColor(Widget, colorSelected);
    end
    else
      Font.Color := CalcColor(Widget, colorText);
    DrawText(Font, Widget.Items[I], R.MidPoint.X, R.MidPoint.Y);
    R.Y := R.Bottom + 1;
  end;
end;

{ TExperienceTheme }

procedure TExperienceTheme.Init(Canvas: ICanvas);
begin
  inherited Init(Canvas);
  LoadFonts('Tahoma', 'Tahoma-Regular.ttf', 'NotoSans-Bold', 'NotoSans-Bold.ttf');
end;

function TExperienceTheme.FontSize: Float;
begin
  Result := 13;
end;

function TExperienceTheme.TitleSize: Float;
begin
  Result := 14;
end;

function TExperienceTheme.CaptionHeight: Float;
begin
  Result := Scaled(28);
end;

function TExperienceTheme.ButtonHeight: Float;
begin
  Result := Scaled(24);
end;

function TExperienceTheme.ThumbSize: TSizeF;
begin
  Result := Scaled(NewPointF(11, 20));
end;

{ The thick blue frame }

function TExperienceTheme.WindowBorder: TSizeF;
begin
  Result := NewPointF(3, 3);
end;

function TExperienceTheme.WindowRadius: Float;
begin
  Result := 8;
end;

function TExperienceTheme.Palette(Widget: TWidget; Color: TThemeColor): LongWord;
begin
  case Color of
    colorBase: Result := $ECE9D8;
    colorFace: Result := $FFFFFF;
    colorBorder: Result := $0831D9;
    colorActive: Result := $316AC5;
    colorSelected: Result := $FFFFFF;
    colorHot: Result := $F8B330;
    colorCaption: Result := $0A5FDB;
    colorTitle: Result := $FFFFFF;
    colorText:
      if Widget.Computed.Enabled then
        Result := $000000
      else
        Result := $ACA899;
    colorHighlight: Result := $FFFFFF;
    colorShadow: Result := $ACA899;
    colorDarkShadow: Result := $7F9DB9;
  else
    Result := 0;
  end;
end;

{ A red rounded square with a white cross at the right of the title bar }

function TExperienceTheme.CalcCloseRect(Window: TWindow): TRectF;
begin
  Result := NewRectF(Window.Width - 21 - 6, 4, 21, 21);
end;

procedure TExperienceTheme.DrawClose(Window: TWindow; const Rect: TRectF; Hot, Pressed: Boolean);
var
  R: TRectF;
begin
  R := Rect.Round;
  if Pressed then
    VertGradient(R, Shade(Window, $B5341C), Shade(Window, $D9573B))
  else if Hot then
    VertGradient(R, Shade(Window, $F58468), Shade(Window, $D8492B))
  else
    VertGradient(R, Shade(Window, $E5694E), Shade(Window, $C13A20));
  Canvas.RoundRect(R, 3);
  Canvas.Fill(FBrush, True);
  Canvas.Stroke(Shade(Window, $FFFFFF));
  DrawCross(Rect, Shade(Window, $FFFFFF), 2, 6);
end;

{ A blue gradient title bar with rounded top corners and a thick blue frame }

procedure TExperienceTheme.DrawWindow(Widget: TWindow);
var
  R, T: TRectF;
  C: TColorF;
  Active: Boolean;
begin
  R := Widget.Computed.Bounds.Round;
  Active := Widget.Main.ActiveWindow = Widget;
  Canvas.RoundRectVarying(R, 8, 8, 0, 0);
  C := CalcColor(Widget, colorBase);
  C.Alpha := C.Alpha * (1 - Clamp(Widget.Fade));
  Canvas.Fill(C);
  T := R;
  T.Height := CaptionHeight;
  if Active then
    VertGradient(T, Shade(Widget, $2B84F5), Shade(Widget, $0A4FD3))
  else
    VertGradient(T, Shade(Widget, $9DB9EB), Shade(Widget, $7A96DF));
  Canvas.RoundRectVarying(T, 8, 8, 0, 0);
  Canvas.Fill(FBrush);
  { The title is on the left with a dark shadow under it }
  Title.Size := TitleSize;
  Title.Align := fontLeft;
  Title.Layout := fontMiddle;
  Title.Color := Shade(Widget, $0A1883, 0.6);
  DrawText(Title, Widget.Text, T.X + 10, T.MidPoint.Y + 1);
  if Active then
    Title.Color := CalcColor(Widget, colorTitle)
  else
    Title.Color := Shade(Widget, $D8E4F8);
  DrawText(Title, Widget.Text, T.X + 9, T.MidPoint.Y);
  R.Inflate(-1, -1);
  Canvas.RoundRectVarying(R, 7, 7, 0, 0);
  if Active then
    Canvas.Stroke(Shade(Widget, $0831D9), 3)
  else
    Canvas.Stroke(Shade(Widget, $7A96DF), 3);
end;

{ A rounded button with a cream gradient, an orange glow when the mouse is
  over it, and a blue glow when it has focus }

procedure TExperienceTheme.DrawButton(Widget: TPushButton);
var
  R, I: TRectF;
  Pressed: Boolean;
begin
  R := Widget.Computed.Bounds.Round;
  Pressed := (wsPressed in Widget.State) and (wsHot in Widget.State);
  if Pressed then
    VertGradient(R, Shade(Widget, $CDCAC1), Shade(Widget, $E9E7DE))
  else
    VertGradient(R, Shade(Widget, $FFFFFF), Shade(Widget, $DDD9CC));
  Canvas.RoundRect(R, 3);
  Canvas.Fill(FBrush, True);
  if Widget.Computed.Enabled then
    Canvas.Stroke(Shade(Widget, $003C74))
  else
    Canvas.Stroke(Shade(Widget, $C9C7BA));
  I := R;
  I.Inflate(-1.5, -1.5);
  if Pressed then
    R.Offset(1, 1)
  else if wsHot in Widget.State then
  begin
    Canvas.RoundRect(I, 2);
    Canvas.Stroke(Shade(Widget, $F8B330), 2);
  end
  else if wsSelected in Widget.State then
  begin
    Canvas.RoundRect(I, 2);
    Canvas.Stroke(Shade(Widget, $8EB5F2), 2);
  end;
  DrawCaption(Widget, R);
end;

{ A square box with a green check mark, or a circle with a green dot }

procedure TExperienceTheme.DrawCheckBox(Widget: TCheckBox);
const
  Size = 13;
var
  R, I: TRectF;
  P: TPointF;
begin
  R := CheckRect(Widget, Size);
  P := R.MidPoint;
  VertGradient(R, Shade(Widget, $DCDCD7), Shade(Widget, $FFFFFF));
  if Widget.Round then
    Canvas.Circle(P.X, P.Y, Size / 2)
  else
    Canvas.Rect(R);
  Canvas.Fill(FBrush, True);
  Canvas.Stroke(Shade(Widget, $1C5180));
  if wsHot in Widget.State then
  begin
    if Widget.Round then
      Canvas.Circle(P.X, P.Y, Size / 2 - 1.5)
    else
    begin
      I := R;
      I.Inflate(-1.5, -1.5);
      Canvas.Rect(I);
    end;
    Canvas.Stroke(Shade(Widget, $F8B330), 2);
  end;
  if wsToggled in Widget.State then
    if Widget.Round then
    begin
      Canvas.Circle(P.X, P.Y, 2.5);
      Canvas.Fill(Shade(Widget, $21A121));
    end
    else
      CheckMark(R, Shade(Widget, $21A121), 2);
  CheckCaption(Widget, Size + 4);
end;

{ A thin groove with a rounded thumb which has colored top and bottom edges }

procedure TExperienceTheme.DrawSlider(Widget: TSlider);
var
  B, S, T: TRectF;
  Accent: TColorF;
begin
  B := Widget.Computed.Bounds;
  S := NewRectF(B.X, B.MidPoint.Y - 2, B.Width, 4).Round;
  Canvas.RoundRect(S, 2);
  Canvas.Fill(Shade(Widget, $FFFFFF), True);
  Canvas.Stroke(Shade(Widget, $9D9C99));
  T := Widget.GripRect;
  T.Offset(B.X, B.Y);
  T := T.Round;
  VertGradient(T, Shade(Widget, $FFFFFF), Shade(Widget, $C4CBD4));
  Canvas.RoundRect(T, 2);
  Canvas.Fill(FBrush, True);
  Canvas.Stroke(Shade(Widget, $778892));
  if (wsHot in Widget.State) or (wsPressed in Widget.State) then
    Accent := Shade(Widget, $F8B330)
  else
    Accent := Shade(Widget, $2DC22D);
  Canvas.Rect(T.X + 1, T.Y + 1, T.Width - 2, 2);
  Canvas.Fill(Accent);
  Canvas.Rect(T.X + 1, T.Bottom - 3, T.Width - 2, 2);
  Canvas.Fill(Accent);
end;

procedure TExperienceTheme.DrawSpinBox(Widget: TSpinBox);
var
  R: TRectF;
begin
  R := Widget.Computed.Bounds.Round;
  Canvas.Rect(R);
  Canvas.Fill(Shade(Widget, $FFFFFF), True);
  if (wsHot in Widget.State) or (wsSelected in Widget.State) then
    Canvas.Stroke(Shade(Widget, $316AC5))
  else
    Canvas.Stroke(Shade(Widget, $7F9DB9));
  DrawCaption(Widget, R, Widget.Prefix + Widget.Text);
  SpinArrows(Widget, R, Shade(Widget, $4D6185));
end;

procedure TExperienceTheme.DrawEdit(Widget: TEdit);
var
  R: TRectF;
begin
  R := Widget.Computed.Bounds.Round;
  Canvas.Rect(R);
  Canvas.Fill(CalcColor(Widget, colorHighlight), True);
  Canvas.Stroke(CalcColor(Widget, colorDarkShadow));
  DrawEditText(Widget, R, CalcColor(Widget, colorText), CalcColor(Widget, colorActive),
    CalcColor(Widget, colorSelected));
end;

procedure TExperienceTheme.DrawMemo(Widget: TMemo);
var
  R: TRectF;
begin
  R := Widget.Computed.Bounds.Round;
  Canvas.Rect(R);
  Canvas.Fill(CalcColor(Widget, colorHighlight), True);
  Canvas.Stroke(CalcColor(Widget, colorDarkShadow));
  DrawMemoText(Widget, CalcColor(Widget, colorText), CalcColor(Widget, colorActive),
    CalcColor(Widget, colorSelected));
end;

procedure TExperienceTheme.DrawListFrame(Widget: TWidget; out TextColor, SelectColor,
  SelectTextColor: TColorF);
var
  R: TRectF;
begin
  R := Widget.Computed.Bounds.Round;
  Canvas.Rect(R);
  Canvas.Fill(CalcColor(Widget, colorHighlight), True);
  Canvas.Stroke(CalcColor(Widget, colorDarkShadow));
  TextColor := CalcColor(Widget, colorText);
  SelectColor := CalcColor(Widget, colorActive);
  SelectTextColor := CalcColor(Widget, colorSelected);
end;

{ A cream header cell with a shaded bottom edge and a short divider. Under
  the mouse it turns pale with an orange bar along the bottom, and pressed it
  is gray with its title moved. }

procedure TExperienceTheme.DrawHeaderCell(Grid: TScrollGrid; const Rect: TRectF; Col: Integer;
  Hot, Pressed: Boolean);
var
  T: TRectF;
begin
  T := Rect;
  Canvas.Rect(Rect);
  if Pressed then
  begin
    Canvas.Fill(Shade(Grid, $DEDFD8));
    Canvas.Rect(Rect.X, Rect.Y, 1, Rect.Height);
    Canvas.Fill(Shade(Grid, $A5A295));
    Canvas.Rect(Rect.X, Rect.Bottom - 1, Rect.Width, 1);
    Canvas.Fill(Shade(Grid, $A5A295));
    T.Offset(1, 1);
  end
  else if Hot then
  begin
    Canvas.Fill(Shade(Grid, $FAF9F4));
    Canvas.Rect(Rect.X, Rect.Bottom - 3, Rect.Width, 3);
    Canvas.Fill(Shade(Grid, $F9A900));
  end
  else
  begin
    Canvas.Fill(Shade(Grid, $EBEADB));
    Canvas.Rect(Rect.X, Rect.Bottom - 3, Rect.Width, 1);
    Canvas.Fill(Shade(Grid, $E2DECD));
    Canvas.Rect(Rect.X, Rect.Bottom - 2, Rect.Width, 1);
    Canvas.Fill(Shade(Grid, $D6D2C2));
    Canvas.Rect(Rect.X, Rect.Bottom - 1, Rect.Width, 1);
    Canvas.Fill(Shade(Grid, $CBC7B8));
    Canvas.Rect(Rect.Right - 2, Rect.Y + 3, 1, Rect.Height - 8);
    Canvas.Fill(Shade(Grid, $C7C5B2));
    Canvas.Rect(Rect.Right - 1, Rect.Y + 3, 1, Rect.Height - 8);
    Canvas.Fill(Shade(Grid, $FFFFFF));
  end;
  DrawHeaderText(Grid, T, Col, CalcColor(Grid, colorText));
end;

{ A pale track with a light blue rounded thumb and grip lines }

procedure TExperienceTheme.DrawScrollBar(Widget: TWidget; const Track, Thumb: TRectF;
  Horizontal: Boolean);
var
  R: TRectF;
begin
  Canvas.Rect(Track);
  Canvas.Fill(Shade(Widget, $F3F1EC));
  R := Thumb;
  R.Inflate(-1, -1);
  R := R.Round;
  if Horizontal then
    VertGradient(R, Shade(Widget, $C9D7FC), Shade(Widget, $A8BFF5))
  else
    HorzGradient(R, Shade(Widget, $C9D7FC), Shade(Widget, $A8BFF5));
  Canvas.RoundRect(R, 2);
  Canvas.Fill(FBrush, True);
  Canvas.Stroke(Shade(Widget, $7B9FE0));
  GripLines(R, Horizontal, Shade(Widget, $EEF4FE));
end;

{ TVistaTheme }

procedure TVistaTheme.Init(Canvas: ICanvas);
begin
  inherited Init(Canvas);
  LoadFonts('NotoSans', 'NotoSans-Regular.ttf', 'NotoSans-Title', 'NotoSans-Regular.ttf');
end;

function TVistaTheme.FontSize: Float;
begin
  Result := 14;
end;

function TVistaTheme.TitleSize: Float;
begin
  Result := 14;
end;

function TVistaTheme.CaptionHeight: Float;
begin
  Result := Scaled(30);
end;

function TVistaTheme.ButtonHeight: Float;
begin
  Result := Scaled(24);
end;

function TVistaTheme.ThumbSize: TSizeF;
begin
  Result := Scaled(NewPointF(11, 19));
end;

{ The glass frame around the client area }

function TVistaTheme.WindowBorder: TSizeF;
begin
  Result := NewPointF(7, 7);
end;

function TVistaTheme.WindowRadius: Float;
begin
  Result := 7;
end;

function TVistaTheme.Palette(Widget: TWidget; Color: TThemeColor): LongWord;
begin
  case Color of
    colorBase: Result := $F0F0F0;
    colorFace: Result := $FFFFFF;
    colorBorder: Result := $5A7CA8;
    colorActive: Result := $3399FF;
    colorSelected: Result := $FFFFFF;
    colorHot: Result := $3C7FB1;
    colorCaption: Result := $BCD3EE;
    colorTitle: Result := $1E1E1E;
    colorText:
      if Widget.Computed.Enabled then
        Result := $1E1E1E
      else
        Result := $838383;
    colorHighlight: Result := $FFFFFF;
    colorShadow: Result := $ABADB3;
    colorDarkShadow: Result := $707070;
  else
    Result := 0;
  end;
end;

{ A wide red button with a white cross which hangs from the top edge at the
  right of the title bar }

function TVistaTheme.CalcCloseRect(Window: TWindow): TRectF;
begin
  Result := NewRectF(Window.Width - 44 - 7, 1, 44, 18);
end;

procedure TVistaTheme.DrawClose(Window: TWindow; const Rect: TRectF; Hot, Pressed: Boolean);
var
  R: TRectF;
  P: TPointF;
begin
  R := Rect.Round;
  if Pressed then
    VertGradient(R, Shade(Window, $C0503F), Shade(Window, $9B2B20))
  else if Hot then
    VertGradient(R, Shade(Window, $F5A89C), Shade(Window, $E0503F))
  else
    VertGradient(R, Shade(Window, $E08E82), Shade(Window, $C8463B));
  Canvas.RoundRectVarying(R, 0, 0, 4, 4);
  Canvas.Fill(FBrush, True);
  Canvas.Stroke(Shade(Window, $5D1F17, 0.8));
  P := Rect.MidPoint;
  DrawCross(NewRectF(P.X - 5, P.Y - 4, 10, 8), Shade(Window, $FFFFFF), 2, 0);
end;

{ A translucent glass frame around a flat client area, with the title on
  the left of the glass }

procedure TVistaTheme.DrawWindow(Widget: TWindow);
var
  R, I, C: TRectF;
  Body: TColorF;
  Glass: Float;
  Active: Boolean;
begin
  R := Widget.Computed.Bounds.Round;
  Active := Widget.Main.ActiveWindow = Widget;
  Glass := 0.9 * (1 - Clamp(Widget.Fade));
  if Active then
    VertGradient(R, Shade(Widget, $C9DDF6, Glass), Shade(Widget, $A6C3E8, Glass))
  else
    VertGradient(R, Shade(Widget, $E3EAF4, Glass), Shade(Widget, $D5DEEA, Glass));
  Canvas.RoundRect(R, 7);
  Canvas.Fill(FBrush, True);
  if Active then
    Canvas.Stroke(Shade(Widget, $42596F))
  else
    Canvas.Stroke(Shade(Widget, $7F8B99));
  I := R;
  I.Inflate(-1, -1);
  Canvas.RoundRect(I, 6);
  Canvas.Stroke(Shade(Widget, $FFFFFF, 0.6));
  { The client area is inset from the glass on the sides and bottom }
  C := Widget.Computed.Bounds;
  C.X := C.X + 7;
  C.Width := C.Width - 14;
  C.Y := C.Y + CaptionHeight;
  C.Height := C.Height - CaptionHeight - 7;
  C := C.Round;
  Canvas.Rect(C);
  Body := CalcColor(Widget, colorBase);
  Body.Alpha := Body.Alpha * (1 - Clamp(Widget.Fade));
  Canvas.Fill(Body, True);
  Canvas.Stroke(Shade(Widget, $8195AF));
  Title.Size := TitleSize;
  Title.Align := fontLeft;
  Title.Layout := fontMiddle;
  Title.Color := CalcColor(Widget, colorTitle);
  DrawText(Title, Widget.Text, R.X + 10, R.Y + CaptionHeight / 2);
end;

{ ButtonFace draws the glossy gray button shape, which turns blue under the
  mouse and deeper blue when pressed }

procedure TVistaTheme.ButtonFace(Widget: TWidget; const R: TRectF);
var
  I: TRectF;
  Border: TColorF;
begin
  if not Widget.Computed.Enabled then
  begin
    VertGradient(R, Shade(Widget, $F4F4F4), Shade(Widget, $F4F4F4));
    Border := Shade(Widget, $ADB2B5);
  end
  else if (wsPressed in Widget.State) and (wsHot in Widget.State) then
  begin
    VertGradient(R, Shade(Widget, $E5F4FC), Shade(Widget, $98D1EF));
    Border := Shade(Widget, $2C628B);
  end
  else if wsHot in Widget.State then
  begin
    VertGradient(R, Shade(Widget, $EAF6FD), Shade(Widget, $BEE6FD));
    Border := Shade(Widget, $3C7FB1);
  end
  else
  begin
    VertGradient(R, Shade(Widget, $F2F2F2), Shade(Widget, $CFCFCF));
    Border := Shade(Widget, $707070);
  end;
  Canvas.RoundRect(R, 3);
  Canvas.Fill(FBrush, True);
  Canvas.Stroke(Border);
  I := R;
  I.Inflate(-1, -1);
  Canvas.RoundRect(I, 2);
  if (wsSelected in Widget.State) and not (wsHot in Widget.State) then
    Canvas.Stroke(Shade(Widget, $48D8FB))
  else
    Canvas.Stroke(Shade(Widget, $FFFFFF, 0.7));
end;

procedure TVistaTheme.DrawButton(Widget: TPushButton);
var
  R: TRectF;
begin
  R := Widget.Computed.Bounds.Round;
  ButtonFace(Widget, R);
  DrawCaption(Widget, R);
end;

{ A square box with a dark blue check mark, or a circle with a dark blue dot,
  tinted blue under the mouse }

procedure TVistaTheme.DrawCheckBox(Widget: TCheckBox);
const
  Size = 13;
var
  R: TRectF;
  P: TPointF;
begin
  R := CheckRect(Widget, Size);
  P := R.MidPoint;
  if wsHot in Widget.State then
    VertGradient(R, Shade(Widget, $DEF9FA), Shade(Widget, $F6FCFD))
  else
    VertGradient(R, Shade(Widget, $F0F0F0), Shade(Widget, $FFFFFF));
  if Widget.Round then
    Canvas.Circle(P.X, P.Y, Size / 2)
  else
    Canvas.Rect(R);
  Canvas.Fill(FBrush, True);
  if wsHot in Widget.State then
    Canvas.Stroke(Shade(Widget, $3C7FB1))
  else
    Canvas.Stroke(Shade(Widget, $8E8F8F));
  if wsToggled in Widget.State then
    if Widget.Round then
    begin
      Canvas.Circle(P.X, P.Y, 3);
      Canvas.Fill(Shade(Widget, $1F4F7E));
    end
    else
      CheckMark(R, Shade(Widget, $4A5F97), 2);
  CheckCaption(Widget, Size + 4);
end;

{ A thin rounded track with a glossy thumb }

procedure TVistaTheme.DrawSlider(Widget: TSlider);
var
  B, S, T: TRectF;
begin
  B := Widget.Computed.Bounds;
  S := NewRectF(B.X, B.MidPoint.Y - 2, B.Width, 4).Round;
  Canvas.RoundRect(S, 2);
  Canvas.Fill(Shade(Widget, $E7EAEA), True);
  Canvas.Stroke(Shade(Widget, $B0B0B0));
  T := Widget.GripRect;
  T.Offset(B.X, B.Y);
  ButtonFace(Widget, T.Round);
end;

procedure TVistaTheme.DrawSpinBox(Widget: TSpinBox);
var
  R: TRectF;
begin
  R := Widget.Computed.Bounds.Round;
  ButtonFace(Widget, R);
  DrawCaption(Widget, R, Widget.Prefix + Widget.Text);
  SpinArrows(Widget, R, CalcColor(Widget, colorText));
end;

procedure TVistaTheme.DrawEdit(Widget: TEdit);
var
  R: TRectF;
begin
  R := Widget.Computed.Bounds.Round;
  Canvas.Rect(R);
  Canvas.Fill(CalcColor(Widget, colorHighlight), True);
  if (wsSelected in Widget.State) or (wsHot in Widget.State) then
    Canvas.Stroke(Shade(Widget, $3D7BAD))
  else
    Canvas.Stroke(CalcColor(Widget, colorShadow));
  DrawEditText(Widget, R, CalcColor(Widget, colorText), CalcColor(Widget, colorActive),
    CalcColor(Widget, colorSelected));
end;

procedure TVistaTheme.DrawMemo(Widget: TMemo);
var
  R: TRectF;
begin
  R := Widget.Computed.Bounds.Round;
  Canvas.Rect(R);
  Canvas.Fill(CalcColor(Widget, colorHighlight), True);
  if (wsSelected in Widget.State) or (wsHot in Widget.State) then
    Canvas.Stroke(Shade(Widget, $3D7BAD))
  else
    Canvas.Stroke(CalcColor(Widget, colorShadow));
  DrawMemoText(Widget, CalcColor(Widget, colorText), CalcColor(Widget, colorActive),
    CalcColor(Widget, colorSelected));
end;

procedure TVistaTheme.DrawListFrame(Widget: TWidget; out TextColor, SelectColor,
  SelectTextColor: TColorF);
var
  R: TRectF;
begin
  R := Widget.Computed.Bounds.Round;
  Canvas.Rect(R);
  Canvas.Fill(CalcColor(Widget, colorHighlight), True);
  if (wsSelected in Widget.State) or (wsHot in Widget.State) then
    Canvas.Stroke(Shade(Widget, $3D7BAD))
  else
    Canvas.Stroke(CalcColor(Widget, colorShadow));
  TextColor := CalcColor(Widget, colorText);
  SelectColor := CalcColor(Widget, colorActive);
  SelectTextColor := CalcColor(Widget, colorSelected);
end;

{ A white header cell with faint lines, which turns glossy blue under the
  mouse and deeper blue when it is pressed }

procedure TVistaTheme.DrawHeaderCell(Grid: TScrollGrid; const Rect: TRectF; Col: Integer;
  Hot, Pressed: Boolean);
begin
  if Pressed then
    VertGradient(Rect, Shade(Grid, $C2E4F6), Shade(Grid, $91CEEF))
  else if Hot then
    VertGradient(Rect, Shade(Grid, $F1FAFE), Shade(Grid, $C9EBFB))
  else
    VertGradient(Rect, Shade(Grid, $FFFFFF), Shade(Grid, $F4F5F7));
  Canvas.Rect(Rect);
  Canvas.Fill(FBrush);
  if Pressed then
    DrawHeaderLines(Rect, Shade(Grid, $7A9EB1))
  else if Hot then
    DrawHeaderLines(Rect, Shade(Grid, $93C9E3))
  else
    DrawHeaderLines(Rect, Shade(Grid, $D5D5D5));
  DrawHeaderText(Grid, Rect, Col, CalcColor(Grid, colorText));
end;

{ A light track with a glossy gray thumb and grip lines }

procedure TVistaTheme.DrawScrollBar(Widget: TWidget; const Track, Thumb: TRectF;
  Horizontal: Boolean);
var
  R: TRectF;
begin
  Canvas.Rect(Track);
  Canvas.Fill(Shade(Widget, $F0F0F0));
  R := Thumb;
  R.Inflate(-1, -1);
  R := R.Round;
  if Horizontal then
    VertGradient(R, Shade(Widget, $F4F4F4), Shade(Widget, $D4D4D4))
  else
    HorzGradient(R, Shade(Widget, $F4F4F4), Shade(Widget, $D4D4D4));
  Canvas.RoundRect(R, 2);
  Canvas.Fill(FBrush, True);
  Canvas.Stroke(Shade(Widget, $979797));
  GripLines(R, Horizontal, Shade(Widget, $8A8A8A));
end;

{ TCupertinoTheme }

procedure TCupertinoTheme.Init(Canvas: ICanvas);
begin
  inherited Init(Canvas);
  LoadFonts('Roboto', 'roboto.ttf', 'Roboto-Medium', 'Roboto-Medium.ttf');
end;

function TCupertinoTheme.FontSize: Float;
begin
  Result := 14;
end;

function TCupertinoTheme.TitleSize: Float;
begin
  Result := 14;
end;

function TCupertinoTheme.CaptionHeight: Float;
begin
  Result := Scaled(28);
end;

function TCupertinoTheme.ButtonHeight: Float;
begin
  Result := Scaled(24);
end;

function TCupertinoTheme.ThumbSize: TSizeF;
begin
  Result := Scaled(NewPointF(18, 18));
end;

function TCupertinoTheme.WindowRadius: Float;
begin
  Result := 10;
end;

function TCupertinoTheme.Palette(Widget: TWidget; Color: TThemeColor): LongWord;
begin
  case Color of
    colorBase: Result := $ECECEC;
    colorFace: Result := $FFFFFF;
    colorBorder: Result := $B4B4B4;
    colorActive: Result := $0A82FF;
    colorSelected: Result := $FFFFFF;
    colorHot: Result := $007AFF;
    colorCaption: Result := $E3E1E3;
    colorTitle: Result := $4D4D4D;
    colorText:
      if Widget.Computed.Enabled then
        Result := $262626
      else
        Result := $A0A0A0;
    colorHighlight: Result := $FFFFFF;
    colorShadow: Result := $C8C8C8;
    colorDarkShadow: Result := $A0A0A0;
  else
    Result := 0;
  end;
end;

{ The soft blue ring drawn around the control with focus }

procedure TCupertinoTheme.FocusRing(Widget: TWidget; const R: TRectF; Radius: Float);
var
  O: TRectF;
begin
  O := R;
  O.Inflate(1.5, 1.5);
  Canvas.RoundRect(O, Radius + 1.5);
  Canvas.Stroke(Shade(Widget, $007AFF, 0.45), 3);
end;

{ A red circle at the left of the title bar, which shows a cross when the
  mouse is over it and is gray on a window which is not active }

function TCupertinoTheme.CalcCloseRect(Window: TWindow): TRectF;
begin
  Result := NewRectF(9, 8, 12, 12);
end;

procedure TCupertinoTheme.DrawClose(Window: TWindow; const Rect: TRectF; Hot, Pressed: Boolean);
var
  P: TPointF;
begin
  P := Rect.MidPoint;
  Canvas.Circle(P.X, P.Y, 6);
  if Pressed then
  begin
    Canvas.Fill(Shade(Window, $C9463F), True);
    Canvas.Stroke(Shade(Window, $A8342E));
  end
  else if Hot or (Window.Main.ActiveWindow = Window) then
  begin
    Canvas.Fill(Shade(Window, $FF5F57), True);
    Canvas.Stroke(Shade(Window, $E0443E));
  end
  else
  begin
    Canvas.Fill(Shade(Window, $CFCFCF), True);
    Canvas.Stroke(Shade(Window, $B8B8B8));
  end;
  if Hot then
    DrawCross(Rect, Shade(Window, $4D0000, 0.7), 1.2, 3.5);
end;

{ A window with large rounded corners, a soft gray title bar, and a centered
  title }

procedure TCupertinoTheme.DrawWindow(Widget: TWindow);
var
  R, T: TRectF;
  C: TColorF;
  Active: Boolean;
begin
  R := Widget.Computed.Bounds.Round;
  Active := Widget.Main.ActiveWindow = Widget;
  Canvas.RoundRect(R, 10);
  C := CalcColor(Widget, colorBase);
  C.Alpha := C.Alpha * (1 - Clamp(Widget.Fade));
  Canvas.Fill(C);
  T := R;
  T.Height := CaptionHeight;
  if Active then
    VertGradient(T, Shade(Widget, $E9E7E9), Shade(Widget, $D2D0D2))
  else
    VertGradient(T, Shade(Widget, $F6F6F6), Shade(Widget, $F6F6F6));
  Canvas.RoundRectVarying(T, 10, 10, 0, 0);
  Canvas.Fill(FBrush);
  Canvas.Rect(T.X, T.Bottom - 0.5, T.Width, 1);
  Canvas.Fill(Shade(Widget, $000000, 0.15));
  DrawCaption(Widget, T);
  Canvas.RoundRect(R, 10);
  Canvas.Stroke(Shade(Widget, $000000, 0.25));
end;

{ A white rounded button with a hairline border }

procedure TCupertinoTheme.DrawButton(Widget: TPushButton);
var
  R: TRectF;
begin
  R := Widget.Computed.Bounds.Round;
  if wsSelected in Widget.State then
    FocusRing(Widget, R, 5);
  Canvas.RoundRect(R, 5);
  if (wsPressed in Widget.State) and (wsHot in Widget.State) then
    Canvas.Fill(Shade(Widget, $E0E0E0), True)
  else
    Canvas.Fill(Shade(Widget, $FFFFFF), True);
  Canvas.Stroke(Shade(Widget, $000000, 0.2));
  DrawCaption(Widget, R);
end;

{ A rounded box or circle which fills with blue when checked, with a white
  check mark or dot }

procedure TCupertinoTheme.DrawCheckBox(Widget: TCheckBox);
const
  Size = 14;
var
  R: TRectF;
  P: TPointF;
begin
  R := CheckRect(Widget, Size);
  P := R.MidPoint;
  if Widget.Round then
    Canvas.Circle(P.X, P.Y, Size / 2)
  else
    Canvas.RoundRect(R, 3.5);
  if wsToggled in Widget.State then
  begin
    VertGradient(R, Shade(Widget, $3F9BFC), Shade(Widget, $0A7BFF));
    Canvas.Fill(FBrush, True);
    Canvas.Stroke(Shade(Widget, $0862D0, 0.6));
    if Widget.Round then
    begin
      Canvas.Circle(P.X, P.Y, 2.5);
      Canvas.Fill(Shade(Widget, $FFFFFF));
    end
    else
      CheckMark(R, Shade(Widget, $FFFFFF), 1.8);
  end
  else
  begin
    Canvas.Fill(Shade(Widget, $FFFFFF), True);
    Canvas.Stroke(Shade(Widget, $000000, 0.22));
  end;
  CheckCaption(Widget, Size + 4);
end;

{ A thin track which is blue up to a round white thumb }

procedure TCupertinoTheme.DrawSlider(Widget: TSlider);
var
  B, S, T: TRectF;
  P: TPointF;
begin
  B := Widget.Computed.Bounds;
  T := Widget.GripRect;
  T.Offset(B.X, B.Y);
  P := T.MidPoint;
  S := NewRectF(B.X, Trunc(B.MidPoint.Y) - 2, B.Width, 4);
  Canvas.RoundRect(S, 2);
  Canvas.Fill(Shade(Widget, $000000, 0.12));
  S.Width := P.X - S.X;
  if S.Width > 0 then
  begin
    Canvas.RoundRect(S, 2);
    Canvas.Fill(CalcColor(Widget, colorHot));
  end;
  Canvas.Circle(P.X, Trunc(B.MidPoint.Y), 8.5);
  Canvas.Fill(Shade(Widget, $FFFFFF), True);
  Canvas.Stroke(Shade(Widget, $000000, 0.25));
end;

procedure TCupertinoTheme.DrawSpinBox(Widget: TSpinBox);
var
  R: TRectF;
begin
  R := Widget.Computed.Bounds.Round;
  if wsSelected in Widget.State then
    FocusRing(Widget, R, 5);
  Canvas.RoundRect(R, 5);
  Canvas.Fill(Shade(Widget, $FFFFFF), True);
  Canvas.Stroke(Shade(Widget, $000000, 0.2));
  DrawCaption(Widget, R, Widget.Prefix + Widget.Text);
  SpinArrows(Widget, R, CalcColor(Widget, colorHot));
end;

{ Text boxes are white with a hairline border, and select in light blue }

procedure TCupertinoTheme.DrawEdit(Widget: TEdit);
var
  R: TRectF;
begin
  R := Widget.Computed.Bounds.Round;
  if wsSelected in Widget.State then
    FocusRing(Widget, R, 3);
  Canvas.RoundRect(R, 3);
  Canvas.Fill(CalcColor(Widget, colorHighlight), True);
  Canvas.Stroke(Shade(Widget, $000000, 0.2));
  DrawEditText(Widget, R, CalcColor(Widget, colorText), Shade(Widget, $B3D7FF),
    CalcColor(Widget, colorText));
end;

procedure TCupertinoTheme.DrawMemo(Widget: TMemo);
var
  R: TRectF;
begin
  R := Widget.Computed.Bounds.Round;
  if wsSelected in Widget.State then
    FocusRing(Widget, R, 3);
  Canvas.RoundRect(R, 3);
  Canvas.Fill(CalcColor(Widget, colorHighlight), True);
  Canvas.Stroke(Shade(Widget, $000000, 0.2));
  DrawMemoText(Widget, CalcColor(Widget, colorText), Shade(Widget, $B3D7FF),
    CalcColor(Widget, colorText));
end;

procedure TCupertinoTheme.DrawListFrame(Widget: TWidget; out TextColor, SelectColor,
  SelectTextColor: TColorF);
var
  R: TRectF;
begin
  R := Widget.Computed.Bounds.Round;
  if wsSelected in Widget.State then
    FocusRing(Widget, R, 3);
  Canvas.RoundRect(R, 3);
  Canvas.Fill(CalcColor(Widget, colorHighlight), True);
  Canvas.Stroke(Shade(Widget, $000000, 0.2));
  TextColor := CalcColor(Widget, colorText);
  SelectColor := Shade(Widget, $B3D7FF);
  SelectTextColor := CalcColor(Widget, colorText);
end;

{ A plain white header cell with a hairline under it and a short divider,
  which darkens a little under the mouse and more when it is pressed }

procedure TCupertinoTheme.DrawHeaderCell(Grid: TScrollGrid; const Rect: TRectF; Col: Integer;
  Hot, Pressed: Boolean);
begin
  Canvas.Rect(Rect);
  Canvas.Fill(Shade(Grid, $FFFFFF));
  if Pressed or Hot then
  begin
    Canvas.Rect(Rect);
    if Pressed then
      Canvas.Fill(Shade(Grid, $000000, 0.12))
    else
      Canvas.Fill(Shade(Grid, $000000, 0.05));
  end;
  Canvas.Rect(Rect.X, Rect.Bottom - 1, Rect.Width, 1);
  Canvas.Fill(Shade(Grid, $000000, 0.15));
  Canvas.Rect(Rect.Right - 1, Rect.Y + 4, 1, Rect.Height - 9);
  Canvas.Fill(Shade(Grid, $000000, 0.12));
  DrawHeaderText(Grid, Rect, Col, CalcColor(Grid, colorText));
end;

{ An overlay scroll bar: no track, just a translucent dark pill }

procedure TCupertinoTheme.DrawScrollBar(Widget: TWidget; const Track, Thumb: TRectF;
  Horizontal: Boolean);
var
  R: TRectF;
begin
  R := Thumb;
  if Horizontal then
  begin
    R.Inflate(-2, -4);
    Canvas.RoundRect(R, R.Height / 2);
  end
  else
  begin
    R.Inflate(-4, -2);
    Canvas.RoundRect(R, R.Width / 2);
  end;
  Canvas.Fill(Shade(Widget, $000000, 0.35));
end;

end.
