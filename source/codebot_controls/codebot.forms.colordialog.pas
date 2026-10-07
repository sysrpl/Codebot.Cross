(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.forms.colordialog.txt> }
unit Codebot.Forms.ColorDialog;

{$i ../codebot/codebot.inc}

interface

uses
  Classes, SysUtils, Graphics, Controls, StdCtrls, Forms, LCLType,
  Codebot.System,
  Codebot.Graphics,
  Codebot.Graphics.Types,
  Codebot.Controls,
  Codebot.Controls.Buttons,
  Codebot.Controls.Edits,
  Codebot.Controls.Colors;

{ TColorSwatchGrid shows a grid of colors which can be clicked to select one.
  Transparent colors are drawn over a checkerboard.
  See also
  <link Overview.Codebot.Forms.ColorDialog.TColorSwatchGrid, TColorSwatchGrid members> }

type
  TColorSwatchGrid = class(TSurfaceGraphicControl)
  private
    FColors: TArrayList<TColorB>;
    FColumns: Integer;
    FCellWidth: Integer;
    FCellHeight: Integer;
    FSpacing: Integer;
    FFitWidth: Integer;
    FSelected: Integer;
    FHot: Integer;
    FSelectable: Boolean;
    FOnSelect: TNotifyIndexEvent;
    FOnSwatchRightClick: TNotifyIndexEvent;
    function GetCount: Integer;
    procedure SetCount(Value: Integer);
    function GetColor(Index: Integer): TColorB;
    procedure SetColor(Index: Integer; const Value: TColorB);
    procedure SetColumns(Value: Integer);
    procedure SetFitWidth(Value: Integer);
    procedure SetSelected(Value: Integer);
    procedure UpdateSize;
  protected
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseLeave; override;
    procedure Draw; override;
  public
    { Create an empty swatch grid }
    constructor Create(AOwner: TComponent); override;
    { Replace all colors in the grid }
    procedure SetColors(const Colors: array of TColorB);
    { Set the size of each cell and the space between cells }
    procedure SetCellSize(CellWidth, CellHeight, Spacing: Integer);
    { Return the index of the cell at a point or -1 if there is none }
    function IndexAt(X, Y: Integer): Integer;
    { Return the bounds of a cell }
    function CellRect(Index: Integer): TRectI;
    { The number of colors }
    property Count: Integer read GetCount write SetCount;
    { The colors indexed by cell }
    property Colors[Index: Integer]: TColorB read GetColor write SetColor; default;
    { The number of cells in each row }
    property Columns: Integer read FColumns write SetColumns;
    { When above zero the grid is this wide, and the space between columns
      is adjusted to fill the width }
    property FitWidth: Integer read FFitWidth write SetFitWidth;
    { The selected cell or -1 }
    property Selected: Integer read FSelected write SetSelected;
    { When false cells cannot be selected or hot tracked }
    property Selectable: Boolean read FSelectable write FSelectable;
    { OnSelect is invoked when a cell is clicked with the left mouse button }
    property OnSelect: TNotifyIndexEvent read FOnSelect write FOnSelect;
    { OnSwatchRightClick is invoked when a cell is clicked with the right mouse button }
    property OnSwatchRightClick: TNotifyIndexEvent read FOnSwatchRightClick write FOnSwatchRightClick;
  end;

{ TAdvancedColorDialogForm lets the user pick a color with alpha using a hue
  and saturation picker, an alpha picker, and red, green, blue, and alpha
  slide edits. A button under the slide edits switches them between red,
  green, and blue or hue, saturation, and lightness. A grid of common colors
  is shown on the right, and below it the color is shown as text which can be
  edited. The button next to the text cycles between Delphi, HTML, and css
  rgba text. A row of custom colors along the bottom are saved between uses. Click a custom color to use it, or
  right click it to store the current color there.
  See also
  <link Overview.Codebot.Forms.ColorDialog.TAdvancedColorDialogForm, TAdvancedColorDialogForm members>
  <link Codebot.Forms.ColorDialog.TAdvancedColorDialog, TAdvancedColorDialog class> }

  TColorTextMode = (ctmDelphi, ctmHtml, ctmRgba);
  TColorSlideMode = (csmRgb, csmHsl);

  TAdvancedColorDialogForm = class(TForm)
  private
    FColor: TColorB;
    FTextMode: TColorTextMode;
    FSlideMode: TColorSlideMode;
    FUpdating: Boolean;
    FCustomColorsFile: string;
    FNextCustom: Integer;
    FArranging: Boolean;
    FHue: THuePicker;
    FSat: TSaturationPicker;
    FAlpha: TAlphaPicker;
    FSlideLabel: TLabel;
    FSlides: array[0..2] of TColorSlideEdit;
    FAlphaEdit: TColorSlideEdit;
    FSlideButton: TThinButton;
    FPreview: TColorSwatchGrid;
    FTextEdit: TEdit;
    FModeButton: TThinButton;
    FCommonLabel: TLabel;
    FCommon: TColorSwatchGrid;
    FCustomLabel: TLabel;
    FCustom: TColorSwatchGrid;
    FAddButton: TThinButton;
    FOkButton: TButton;
    FCancelButton: TButton;
    function NewLabel(const Caption: string; X, Y: Integer): TLabel;
    function NewSlide(Kind: TColorSlideKind): TColorSlideEdit;
    procedure Arrange;
    procedure ArrangeResize(Sender: TObject);
    function NewIconButton(const Hint: string): TThinButton;
    function ColorText(const C: TColorB): string;
    procedure UpdateSlides(const Value: TColorB; FromSlide: Boolean);
    procedure UpdateColor(const Value: TColorB; Source: TObject);
    procedure SetColorValue(const Value: TColorB);
    function GetCustomColorsFile: string;
    procedure HueChange(Sender: TObject);
    procedure SatChange(Sender: TObject);
    procedure AlphaChange(Sender: TObject);
    procedure SlideChange(Sender: TObject);
    procedure TextExit(Sender: TObject);
    procedure TextKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CommonSelect(Sender: TObject; Index: Integer);
    procedure CustomSelect(Sender: TObject; Index: Integer);
    procedure CustomStore(Sender: TObject; Index: Integer);
    procedure AddClick(Sender: TObject);
    procedure ModeClick(Sender: TObject);
    procedure SlideModeClick(Sender: TObject);
    procedure DrawModeButton(Sender: TObject; Surface: ISurface; Rect: TRectI;
      State: TDrawState);
    procedure DrawAddButton(Sender: TObject; Surface: ISurface; Rect: TRectI;
      State: TDrawState);
    procedure DrawSlideButton(Sender: TObject; Surface: ISurface; Rect: TRectI;
      State: TDrawState);
  protected
    procedure DoShow; override;
  public
    { Create the dialog form and its controls }
    constructor Create(AOwner: TComponent); override;
    { Load the custom colors from CustomColorsFile }
    procedure LoadCustomColors;
    { Save the custom colors to CustomColorsFile }
    procedure SaveCustomColors;
    { The selected color including alpha. Setting it also sets the original
      color shown in the preview. }
    property ColorValue: TColorB read FColor write SetColorValue;
    { The file where custom colors are saved. When empty a file named
      customcolors.txt in the application config directory is used. }
    property CustomColorsFile: string read GetCustomColorsFile write FCustomColorsFile;
  end;

{ TAdvancedColorDialog shows a TAdvancedColorDialogForm to pick a color with
  alpha. Set Color and Alpha before calling Execute, and read them after
  Execute returns true.
  See also
  <link Overview.Codebot.Forms.ColorDialog.TAdvancedColorDialog, TAdvancedColorDialog members>
  <link Codebot.Forms.ColorDialog.TAdvancedColorDialogForm, TAdvancedColorDialogForm class> }

  TAdvancedColorDialog = class(TComponent)
  private
    FColor: TColor;
    FAlpha: Byte;
    FTitle: string;
    FCustomColorsFile: string;
    function GetColorValue: TColorB;
    procedure SetColorValue(const Value: TColorB);
  public
    { Create a dialog with black as the color }
    constructor Create(AOwner: TComponent); override;
    { Show the dialog returning true if the user chose a color }
    function Execute: Boolean;
    { The color including alpha }
    property ColorValue: TColorB read GetColorValue write SetColorValue;
  published
    { The color without alpha }
    property Color: TColor read FColor write FColor default clBlack;
    { The alpha of the color from 0 for transparent to 255 for opaque }
    property Alpha: Byte read FAlpha write FAlpha default 255;
    { The dialog caption, or the default caption when empty }
    property Title: string read FTitle write FTitle;
    { The file where custom colors are saved, or a file in the application
      config directory when empty }
    property CustomColorsFile: string read FCustomColorsFile write FCustomColorsFile;
  end;

{ Convert a color to Delphi TColor text such as $00332B3B, ignoring alpha }
function ColorToDelphiText(const C: TColorB): string;
{ Convert a color to html text such as #3B2B33, or #3B2B3380 when the color
  is not opaque }
function ColorToHtmlText(const C: TColorB): string;
{ Convert a color to css text such as rgba(59, 43, 51, 0.5) }
function ColorToRgbaText(const C: TColorB): string;
{ Convert Delphi, html, or css rgb or rgba text to a color returning false if
  the text could not be converted. Delphi text has no alpha, so the alpha of
  C is kept. }
function TextToColor(const S: string; var C: TColorB): Boolean;

implementation

{ Text conversion }

var
  InvariantFormat: TFormatSettings;

function ColorToDelphiText(const C: TColorB): string;
begin
  Result := Format('$00%.2X%.2X%.2X', [C.Blue, C.Green, C.Red]);
end;

function ColorToHtmlText(const C: TColorB): string;
begin
  Result := Format('#%.2X%.2X%.2X', [C.Red, C.Green, C.Blue]);
  if C.Alpha < HiByte then
    Result := Result + Format('%.2X', [C.Alpha]);
end;

function ColorToRgbaText(const C: TColorB): string;
begin
  Result := Format('rgba(%d, %d, %d, %s)', [C.Red, C.Green, C.Blue,
    FormatFloat('0.##', C.Alpha / HiByte, InvariantFormat)]);
end;

function HexValue(const S: string; out Value: Int64): Boolean;
var
  I: Integer;
begin
  Result := S <> '';
  if not Result then
    Exit;
  for I := 1 to Length(S) do
    if not (S[I] in ['0'..'9', 'a'..'f', 'A'..'F']) then
      Exit(False);
  Value := SysUtils.StrToInt64('$' + S);
end;

function TextToColor(const S: string; var C: TColorB): Boolean;
var
  T: string;
  Parts: StringArray;
  V: Int64;
  I: Integer;
  A: Double;
begin
  Result := False;
  T := Trim(S);
  if T = '' then
    Exit;
  if T[1] = '$' then
  begin
    { Delphi TColor text is $00BBGGRR }
    T := Copy(T, 2, Length(T));
    if (Length(T) > 8) or not HexValue(T, V) then
      Exit;
    C.Red := V and $FF;
    C.Green := (V shr 8) and $FF;
    C.Blue := (V shr 16) and $FF;
    Exit(True);
  end;
  if T[1] = '#' then
  begin
    T := Copy(T, 2, Length(T));
    if Length(T) = 3 then
      T := T[1] + T[1] + T[2] + T[2] + T[3] + T[3];
    if not (Length(T) in [6, 8]) or not HexValue(T, V) then
      Exit;
    if Length(T) = 8 then
    begin
      C.Red := (V shr 24) and $FF;
      C.Green := (V shr 16) and $FF;
      C.Blue := (V shr 8) and $FF;
      C.Alpha := V and $FF;
    end
    else
    begin
      C.Red := (V shr 16) and $FF;
      C.Green := (V shr 8) and $FF;
      C.Blue := V and $FF;
      C.Alpha := HiByte;
    end;
    Exit(True);
  end;
  T := LowerCase(T);
  if T.BeginsWith('rgba(') then
    T := Copy(T, 6, Length(T))
  else if T.BeginsWith('rgb(') then
    T := Copy(T, 5, Length(T))
  else
    Exit;
  if (T = '') or (T[Length(T)] <> ')') then
    Exit;
  SetLength(T, Length(T) - 1);
  Parts := T.Split(',');
  if not (Parts.Length in [3, 4]) then
    Exit;
  for I := 0 to 2 do
  begin
    V := SysUtils.StrToIntDef(Trim(Parts[I]), -1);
    if (V < 0) or (V > HiByte) then
      Exit;
    case I of
      0: C.Red := V;
      1: C.Green := V;
      2: C.Blue := V;
    end;
  end;
  C.Alpha := HiByte;
  if Parts.Length = 4 then
  begin
    A := SysUtils.StrToFloatDef(Trim(Parts[3]), -1, InvariantFormat);
    if (A < 0) or (A > 1) then
      Exit;
    C.Alpha := Round(A * HiByte);
  end;
  Result := True;
end;

{ TColorSwatchGrid }

constructor TColorSwatchGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColumns := 6;
  FCellWidth := 20;
  FCellHeight := 20;
  FSpacing := 2;
  FSelected := -1;
  FHot := -1;
  FSelectable := True;
  UpdateSize;
end;

function TColorSwatchGrid.GetCount: Integer;
begin
  Result := FColors.Length;
end;

procedure TColorSwatchGrid.SetCount(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  FColors.Length := Value;
  if FSelected >= Value then
    FSelected := -1;
  UpdateSize;
  Invalidate;
end;

function TColorSwatchGrid.GetColor(Index: Integer): TColorB;
begin
  Result := FColors[Index];
end;

procedure TColorSwatchGrid.SetColor(Index: Integer; const Value: TColorB);
begin
  FColors[Index] := Value;
  Invalidate;
end;

procedure TColorSwatchGrid.SetColors(const Colors: array of TColorB);
var
  I: Integer;
begin
  FColors.Length := Length(Colors);
  for I := 0 to High(Colors) do
    FColors[I] := Colors[I];
  FSelected := -1;
  UpdateSize;
  Invalidate;
end;

procedure TColorSwatchGrid.SetCellSize(CellWidth, CellHeight, Spacing: Integer);
begin
  FCellWidth := CellWidth;
  FCellHeight := CellHeight;
  FSpacing := Spacing;
  UpdateSize;
  Invalidate;
end;

procedure TColorSwatchGrid.SetColumns(Value: Integer);
begin
  if Value < 1 then
    Value := 1;
  if FColumns = Value then Exit;
  FColumns := Value;
  UpdateSize;
  Invalidate;
end;

procedure TColorSwatchGrid.SetFitWidth(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if FFitWidth = Value then Exit;
  FFitWidth := Value;
  UpdateSize;
  Invalidate;
end;

procedure TColorSwatchGrid.SetSelected(Value: Integer);
begin
  if (Value < -1) or (Value >= Count) then
    Value := -1;
  if FSelected = Value then Exit;
  FSelected := Value;
  Invalidate;
end;

procedure TColorSwatchGrid.UpdateSize;
var
  Rows, W: Integer;
begin
  Rows := (Count + FColumns - 1) div FColumns;
  if Rows < 1 then
    Rows := 1;
  if (FFitWidth > 0) and (FColumns > 1) then
    W := FFitWidth
  else
    W := FColumns * (FCellWidth + FSpacing) - FSpacing;
  SetBounds(Left, Top, W, Rows * (FCellHeight + FSpacing) - FSpacing);
end;

function TColorSwatchGrid.CellRect(Index: Integer): TRectI;
var
  X: Integer;
begin
  X := Index mod FColumns;
  if (FFitWidth > 0) and (FColumns > 1) then
    X := X * (FFitWidth - FCellWidth) div (FColumns - 1)
  else
    X := X * (FCellWidth + FSpacing);
  Result := TRectI.Create(X, (Index div FColumns) * (FCellHeight + FSpacing),
    FCellWidth, FCellHeight);
end;

function TColorSwatchGrid.IndexAt(X, Y: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if CellRect(I).Contains(X, Y) then
      Exit(I);
end;

procedure TColorSwatchGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
begin
  inherited MouseMove(Shift, X, Y);
  if not FSelectable then
    Exit;
  I := IndexAt(X, Y);
  if I <> FHot then
  begin
    FHot := I;
    Invalidate;
  end;
end;

procedure TColorSwatchGrid.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  I: Integer;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if not FSelectable then
    Exit;
  I := IndexAt(X, Y);
  if I < 0 then
    Exit;
  if Button = mbLeft then
  begin
    Selected := I;
    if Assigned(FOnSelect) then
      FOnSelect(Self, I);
  end
  else if Button = mbRight then
  begin
    if Assigned(FOnSwatchRightClick) then
      FOnSwatchRightClick(Self, I);
  end;
end;

procedure TColorSwatchGrid.MouseLeave;
begin
  inherited MouseLeave;
  if FHot > -1 then
  begin
    FHot := -1;
    Invalidate;
  end;
end;

procedure TColorSwatchGrid.Draw;
var
  Checker: IBrush;
  R: TRectI;
  C: TColorB;
  I: Integer;
begin
  Checker := Brushes.Checker(clSilver, clWhite, 1, 4);
  C := clBlack;
  for I := 0 to Count - 1 do
  begin
    R := CellRect(I);
    if FColors[I].Alpha < HiByte then
      Surface.FillRect(Checker, R);
    FillRectColor(Surface, R, FColors[I]);
    StrokeRectColor(Surface, R, C.Fade(0.4));
  end;
  { Outline the hot and selected cells. The outlines are kept inside of the
    cells, as cells on the edges have no room around them. }
  if (FHot > -1) and (FHot <> FSelected) then
    StrokeRectColor(Surface, CellRect(FHot), clHighlight);
  if FSelected > -1 then
  begin
    R := CellRect(FSelected);
    StrokeRectColor(Surface, R, clHighlight);
    R.Inflate(-1, -1);
    StrokeRectColor(Surface, R, clWhite);
  end;
end;

{ TAdvancedColorDialogForm }

const
  { The most common colors as html text, six shades for each of eight hues }
  CommonColors: array[0..47] of string = (
    '#000000', '#404040', '#808080', '#C0C0C0', '#E0E0E0', '#FFFFFF',
    '#800000', '#8B0000', '#FF0000', '#DC143C', '#FA8072', '#FFC0CB',
    '#8B4513', '#D2691E', '#FF8C00', '#FFA500', '#FFD700', '#F5DEB3',
    '#808000', '#FFFF00', '#9ACD32', '#00FF00', '#32CD32', '#98FB98',
    '#006400', '#008000', '#2E8B57', '#3CB371', '#00FF7F', '#7FFFD4',
    '#008080', '#008B8B', '#20B2AA', '#40E0D0', '#00FFFF', '#E0FFFF',
    '#000080', '#0000CD', '#0000FF', '#4169E1', '#1E90FF', '#87CEEB',
    '#4B0082', '#800080', '#9400D3', '#FF00FF', '#DA70D6', '#DDA0DD');
  CommonShades = 6;
  CommonColumns = Length(CommonColors) div CommonShades;
  CommonCell = 20;
  CustomCount = 20;
  CustomCell = 22;
  Margin = 12;
  Gap = 6;
  LabelGap = 4;
  IconSize = 24;
  SlideLeft = 228;
  SlideWidth = 84;
  TextLeft = 330;
  ButtonWidth = 88;
  ButtonHeight = 30;
  SlideCaptions: array[TColorSlideMode] of string = ('RGBA:', 'HSLA:');

constructor TAdvancedColorDialogForm.Create(AOwner: TComponent);
var
  C: TColorB;
  I: Integer;
begin
  inherited CreateNew(AOwner);
  FTextMode := ctmHtml;
  Caption := 'Select Color';
  BorderStyle := bsDialog;
  Position := poOwnerFormCenter;
  { Hue ring with the saturation box inside it }
  FHue := THuePicker.Create(Self);
  FHue.SetBounds(Margin, Margin, 200, 200);
  FHue.Style := hsRadial;
  FHue.Parent := Self;
  FSat := TSaturationPicker.Create(Self);
  FSat.SetBounds(Margin + 16, Margin + 16, 168, 168);
  FSat.Parent := Self;
  FHue.SaturationPicker := FSat;
  FHue.OnChange := HueChange;
  FSat.OnChange := SatChange;
  { Alpha picker below the hue ring }
  FAlpha := TAlphaPicker.Create(Self);
  FAlpha.SetBounds(Margin, Margin + 208, 200, 22);
  FAlpha.Parent := Self;
  FAlpha.OnChange := AlphaChange;
  { Red, green, blue, and alpha slide edits under a label }
  FSlideLabel := NewLabel(SlideCaptions[FSlideMode], SlideLeft, Margin);
  FSlides[0] := NewSlide(cskRed);
  FSlides[1] := NewSlide(cskGreen);
  FSlides[2] := NewSlide(cskBlue);
  FAlphaEdit := NewSlide(cskAlpha);
  { The original and new colors beside the alpha picker }
  FPreview := TColorSwatchGrid.Create(Self);
  FPreview.Selectable := False;
  FPreview.Columns := 2;
  FPreview.SetCellSize((SlideWidth - IconSize - LabelGap) div 2, FAlpha.Height, 0);
  FPreview.Count := 2;
  FPreview.Left := SlideLeft;
  FPreview.Top := FAlpha.Top;
  FPreview.Parent := Self;
  { A button next to the preview to switch the kinds of slide edits }
  FSlideButton := NewIconButton('Switch between RGB and HSL');
  FSlideButton.SetBounds(SlideLeft + SlideWidth - IconSize, FAlpha.Top,
    IconSize, FAlpha.Height);
  FSlideButton.OnClick := SlideModeClick;
  FSlideButton.OnDrawButton := DrawSlideButton;
  { Common colors on the right with a column for each hue }
  FCommonLabel := NewLabel('Common colors', TextLeft, Margin);
  FCommon := TColorSwatchGrid.Create(Self);
  FCommon.Columns := CommonColumns;
  FCommon.SetCellSize(CommonCell, CommonCell, 2);
  FCommon.Count := Length(CommonColors);
  for I := 0 to High(CommonColors) do
  begin
    C := clBlack;
    TextToColor(CommonColors[I], C);
    FCommon[(I mod CommonShades) * CommonColumns + I div CommonShades] := C;
  end;
  FCommon.Left := TextLeft;
  FCommon.Parent := Self;
  FCommon.OnSelect := CommonSelect;
  { The color as text below the common colors, with a button to cycle
    between the kinds of text }
  FTextEdit := TEdit.Create(Self);
  FTextEdit.Left := TextLeft;
  FTextEdit.Parent := Self;
  FTextEdit.OnExit := TextExit;
  FTextEdit.OnKeyDown := TextKeyDown;
  FModeButton := NewIconButton('Next color format');
  FModeButton.OnClick := ModeClick;
  FModeButton.OnDrawButton := DrawModeButton;
  { A row of custom colors along the bottom }
  FCustomLabel := NewLabel('Custom colors (right click to store)', Margin, 0);
  FCustom := TColorSwatchGrid.Create(Self);
  FCustom.Columns := CustomCount;
  FCustom.SetCellSize(CustomCell, CustomCell, 2);
  FCustom.Count := CustomCount;
  for I := 0 to CustomCount - 1 do
    FCustom[I] := clWhite;
  FCustom.Left := Margin;
  FCustom.Parent := Self;
  FCustom.OnSelect := CustomSelect;
  FCustom.OnSwatchRightClick := CustomStore;
  FAddButton := NewIconButton('Add to custom colors');
  FAddButton.OnClick := AddClick;
  FAddButton.OnDrawButton := DrawAddButton;
  { Ok and cancel buttons in the bottom right }
  FCancelButton := TButton.Create(Self);
  FCancelButton.Caption := 'Cancel';
  FCancelButton.ModalResult := mrCancel;
  FCancelButton.Cancel := True;
  FCancelButton.Parent := Self;
  FOkButton := TButton.Create(Self);
  FOkButton.Caption := 'OK';
  FOkButton.ModalResult := mrOK;
  FOkButton.Default := True;
  FOkButton.Parent := Self;
  { The heights of labels and edits can change when their windows are
    created, so arrange again when they do }
  FSlideLabel.OnResize := ArrangeResize;
  FCommonLabel.OnResize := ArrangeResize;
  FCustomLabel.OnResize := ArrangeResize;
  FAlphaEdit.OnResize := ArrangeResize;
  FTextEdit.OnResize := ArrangeResize;
  Arrange;
  ColorValue := TColorB.Create(0, 0, 0);
end;

{ Place the controls which depend on the heights of labels and edits. The
  preview and the text edit share the top and bottom of the alpha picker, and
  the spacing of the slide edits and color grids is adjusted to line up with
  them. }

procedure TAdvancedColorDialogForm.Arrange;
var
  RowTop, RowBottom, Right, Space, H, Y, I: Integer;
begin
  if FArranging then
    Exit;
  FArranging := True;
  try
    RowTop := FAlpha.Top;
    RowBottom := RowTop + FAlpha.Height;
    { Slide edits are spaced evenly between their label and the preview }
    H := FAlphaEdit.Height;
    Y := FSlideLabel.Top + FSlideLabel.Height + LabelGap;
    Space := (RowTop - Gap - Y - H * 4) div 3;
    if Space < 2 then
      Space := 2;
    Y := RowTop - Gap - H;
    FAlphaEdit.Top := Y;
    for I := High(FSlides) downto Low(FSlides) do
    begin
      Y := Y - H - Space;
      FSlides[I].Top := Y;
    end;
    { The bottom of the text edit meets the bottom of the alpha picker, and
      the common colors are spaced to fill the room above it }
    FTextEdit.Top := RowBottom - FTextEdit.Height;
    Y := FCommonLabel.Top + FCommonLabel.Height + LabelGap;
    Space := (FTextEdit.Top - Gap - Y - CommonCell * CommonShades) div
      (CommonShades - 1);
    if Space < 2 then
      Space := 2;
    FCommon.SetCellSize(CommonCell, CommonCell, Space);
    FCommon.Top := FTextEdit.Top - Gap - FCommon.Height;
    Right := TextLeft + FCommon.Width;
    FTextEdit.Width := FCommon.Width - IconSize - LabelGap;
    FModeButton.Left := Right - IconSize;
    FModeButton.Top := FTextEdit.Top + (FTextEdit.Height - IconSize) div 2;
    { Custom colors are spaced so the add button ends under the right side
      of the common colors }
    FCustomLabel.Top := RowBottom + Gap * 2;
    FCustom.Top := FCustomLabel.Top + FCustomLabel.Height + LabelGap;
    FCustom.FitWidth := Right - IconSize - Gap - Margin;
    FAddButton.Left := Right - IconSize;
    FAddButton.Top := FCustom.Top + (FCustom.Height - IconSize) div 2;
    Y := FCustom.Top + FCustom.Height + Gap * 2;
    FCancelButton.SetBounds(Right - ButtonWidth, Y, ButtonWidth, ButtonHeight);
    FOkButton.SetBounds(Right - ButtonWidth * 2 - Gap, Y, ButtonWidth, ButtonHeight);
    ClientWidth := Right + Margin;
    ClientHeight := Y + ButtonHeight + Margin;
  finally
    FArranging := False;
  end;
end;

procedure TAdvancedColorDialogForm.ArrangeResize(Sender: TObject);
begin
  Arrange;
end;

procedure TAdvancedColorDialogForm.DoShow;
begin
  Arrange;
  inherited DoShow;
end;

function TAdvancedColorDialogForm.NewLabel(const Caption: string; X, Y: Integer): TLabel;
begin
  Result := TLabel.Create(Self);
  Result.Caption := Caption;
  Result.Left := X;
  Result.Top := Y;
  Result.Parent := Self;
end;

function TAdvancedColorDialogForm.NewSlide(Kind: TColorSlideKind): TColorSlideEdit;
begin
  Result := TColorSlideEdit.Create(Self);
  Result.Kind := Kind;
  Result.Left := SlideLeft;
  Result.Width := SlideWidth;
  Result.Top := Margin;
  Result.Parent := Self;
  Result.OnValueChange := SlideChange;
end;

function TAdvancedColorDialogForm.NewIconButton(const Hint: string): TThinButton;
begin
  Result := TThinButton.Create(Self);
  Result.SetBounds(0, 0, IconSize, IconSize);
  Result.Hint := Hint;
  Result.ShowHint := True;
  Result.Parent := Self;
end;

function TAdvancedColorDialogForm.ColorText(const C: TColorB): string;
begin
  case FTextMode of
    ctmDelphi: Result := ColorToDelphiText(C);
    ctmHtml: Result := ColorToHtmlText(C);
  else
    Result := ColorToRgbaText(C);
  end;
end;

{ Update the slide edits. When a slide edit changed the color the positions
  of the hue, saturation, and lightness slide edits are kept. }

procedure TAdvancedColorDialogForm.UpdateSlides(const Value: TColorB;
  FromSlide: Boolean);
var
  C: TColorB;
  I: Integer;
begin
  C := Value;
  C.Alpha := HiByte;
  for I := Low(FSlides) to High(FSlides) do
    if FSlideMode = csmRgb then
      FSlides[I].UpdateColor(C)
    else
    begin
      FSlides[I].UpdateHue(FHue.Hue);
      if not FromSlide then
        FSlides[I].UpdateHSL(THSL.Create(FHue.Hue, FSat.Saturation, FSat.Lightness));
    end;
  FAlphaEdit.UpdateColor(C);
  FAlphaEdit.UpdateAlpha(Value.Alpha);
end;

{ Update every control except the one which changed the color }

procedure TAdvancedColorDialogForm.UpdateColor(const Value: TColorB; Source: TObject);
var
  C: TColorB;
  HSL: THSL;
begin
  if FUpdating then
    Exit;
  FUpdating := True;
  try
    FColor := Value;
    C := Value;
    C.Alpha := HiByte;
    if (Source is TColorSlideEdit) and (FSlideMode = csmHsl) then
    begin
      { Take the positions as they are, since converting from the color
        would lose the hue of grays and the saturation of black and white }
      FHue.Hue := FSlides[0].Position / HiByte;
      FSat.Saturation := FSlides[1].Position / HiByte;
      FSat.Lightness := FSlides[2].Position / HiByte;
    end
    else if (Source <> FHue) and (Source <> FSat) then
    begin
      HSL := THSL(C);
      { Grays have no hue, so keep the hue the user last picked }
      if HSL.Saturation > 0 then
        FHue.Hue := HSL.Hue;
      FSat.Saturation := HSL.Saturation;
      FSat.Lightness := HSL.Lightness;
    end;
    FAlpha.Color := C.Color;
    if Source <> FAlpha then
      FAlpha.ColorAlpha := Value.Alpha / HiByte;
    UpdateSlides(Value, Source is TColorSlideEdit);
    if Source <> FTextEdit then
      FTextEdit.Text := ColorText(Value);
    FPreview[1] := Value;
  finally
    FUpdating := False;
  end;
end;

procedure TAdvancedColorDialogForm.SetColorValue(const Value: TColorB);
begin
  FPreview[0] := Value;
  UpdateColor(Value, nil);
end;

procedure TAdvancedColorDialogForm.HueChange(Sender: TObject);
var
  C: TColorB;
begin
  if FUpdating then
    Exit;
  { The hue picker has already passed its hue to the saturation picker }
  C := FSat.ColorValue;
  C.Alpha := FColor.Alpha;
  UpdateColor(C, FHue);
end;

procedure TAdvancedColorDialogForm.SatChange(Sender: TObject);
var
  C: TColorB;
begin
  if FUpdating then
    Exit;
  C := FSat.ColorValue;
  C.Alpha := FColor.Alpha;
  UpdateColor(C, FSat);
end;

procedure TAdvancedColorDialogForm.AlphaChange(Sender: TObject);
var
  C: TColorB;
begin
  if FUpdating then
    Exit;
  C := FColor;
  C.Alpha := Round(FAlpha.ColorAlpha * HiByte);
  UpdateColor(C, FAlpha);
end;

procedure TAdvancedColorDialogForm.SlideChange(Sender: TObject);
var
  C: TColorB;
begin
  if FUpdating then
    Exit;
  C := FColor;
  if FSlideMode = csmRgb then
  begin
    C.Red := Round(FSlides[0].Position);
    C.Green := Round(FSlides[1].Position);
    C.Blue := Round(FSlides[2].Position);
  end
  else
    C := THSL.Create(FSlides[0].Position / HiByte, FSlides[1].Position / HiByte,
      FSlides[2].Position / HiByte);
  C.Alpha := Round(FAlphaEdit.Position);
  UpdateColor(C, Sender);
end;

procedure TAdvancedColorDialogForm.TextExit(Sender: TObject);
var
  E: TEdit absolute Sender;
  C: TColorB;
begin
  C := FColor;
  if TextToColor(E.Text, C) then
    UpdateColor(C, nil)
  else
    UpdateColor(FColor, nil);
end;

procedure TAdvancedColorDialogForm.TextKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  { Enter applies the text without closing the dialog }
  if Key = VK_RETURN then
  begin
    TextExit(Sender);
    TEdit(Sender).SelectAll;
    Key := 0;
  end;
end;

procedure TAdvancedColorDialogForm.CommonSelect(Sender: TObject; Index: Integer);
begin
  FCustom.Selected := -1;
  UpdateColor(FCommon[Index], FCommon);
end;

procedure TAdvancedColorDialogForm.CustomSelect(Sender: TObject; Index: Integer);
begin
  FCommon.Selected := -1;
  FNextCustom := (Index + 1) mod CustomCount;
  UpdateColor(FCustom[Index], FCustom);
end;

procedure TAdvancedColorDialogForm.CustomStore(Sender: TObject; Index: Integer);
begin
  FCustom[Index] := FColor;
  FCustom.Selected := Index;
  FNextCustom := (Index + 1) mod CustomCount;
  SaveCustomColors;
end;

procedure TAdvancedColorDialogForm.AddClick(Sender: TObject);
begin
  CustomStore(FCustom, FNextCustom);
end;

procedure TAdvancedColorDialogForm.ModeClick(Sender: TObject);
begin
  { Apply any text which was typed before it is replaced }
  TextExit(FTextEdit);
  if FTextMode = High(TColorTextMode) then
    FTextMode := Low(TColorTextMode)
  else
    FTextMode := Succ(FTextMode);
  FTextEdit.Text := ColorText(FColor);
end;

procedure TAdvancedColorDialogForm.SlideModeClick(Sender: TObject);
const
  Kinds: array[TColorSlideMode, 0..2] of TColorSlideKind = (
    (cskRed, cskGreen, cskBlue), (cskHue, cskSaturation, cskLightness));
var
  I: Integer;
begin
  if FSlideMode = csmRgb then
    FSlideMode := csmHsl
  else
    FSlideMode := csmRgb;
  for I := Low(FSlides) to High(FSlides) do
    FSlides[I].Kind := Kinds[FSlideMode, I];
  FSlideLabel.Caption := SlideCaptions[FSlideMode];
  UpdateSlides(FColor, False);
end;

{ Draw two chevrons pointing right }

procedure TAdvancedColorDialogForm.DrawModeButton(Sender: TObject;
  Surface: ISurface; Rect: TRectI; State: TDrawState);
const
  Size = 4;
var
  M: TPointI;
  I: Integer;
begin
  Theme.DrawButtonThin(Rect);
  M := Rect.MidPoint;
  for I := 0 to 1 do
  begin
    Surface.MoveTo(M.X - Size + I * 5 - 1, M.Y - Size);
    Surface.LineTo(M.X + I * 5 - 1, M.Y);
    Surface.LineTo(M.X - Size + I * 5 - 1, M.Y + Size);
  end;
  Surface.Stroke(NewPen(clBtnText, 1.5));
end;

{ Draw an arrow pointing right above an arrow pointing left }

procedure TAdvancedColorDialogForm.DrawSlideButton(Sender: TObject;
  Surface: ISurface; Rect: TRectI; State: TDrawState);
const
  Size = 6;
  Head = 3;
var
  M: TPointI;
  Y: Integer;
begin
  Theme.DrawButtonThin(Rect);
  M := Rect.MidPoint;
  Y := M.Y - 3;
  Surface.MoveTo(M.X - Size, Y);
  Surface.LineTo(M.X + Size, Y);
  Surface.MoveTo(M.X + Size - Head, Y - Head);
  Surface.LineTo(M.X + Size, Y);
  Surface.LineTo(M.X + Size - Head, Y + Head);
  Y := M.Y + 4;
  Surface.MoveTo(M.X + Size, Y);
  Surface.LineTo(M.X - Size, Y);
  Surface.MoveTo(M.X - Size + Head, Y - Head);
  Surface.LineTo(M.X - Size, Y);
  Surface.LineTo(M.X - Size + Head, Y + Head);
  Surface.Stroke(NewPen(clBtnText, 1.5));
end;

{ Draw a plus sign }

procedure TAdvancedColorDialogForm.DrawAddButton(Sender: TObject;
  Surface: ISurface; Rect: TRectI; State: TDrawState);
const
  Size = 5;
var
  M: TPointI;
begin
  Theme.DrawButtonThin(Rect);
  M := Rect.MidPoint;
  Surface.MoveTo(M.X - Size, M.Y);
  Surface.LineTo(M.X + Size, M.Y);
  Surface.MoveTo(M.X, M.Y - Size);
  Surface.LineTo(M.X, M.Y + Size);
  Surface.Stroke(NewPen(clBtnText, 2));
end;

function TAdvancedColorDialogForm.GetCustomColorsFile: string;
begin
  Result := FCustomColorsFile;
  if Result = '' then
    Result := PathCombine(ConfigAppDir(False), 'customcolors.txt');
end;

procedure TAdvancedColorDialogForm.LoadCustomColors;
var
  Lines: StringArray;
  C: TColorB;
  I: Integer;
begin
  if not FileExists(CustomColorsFile) then
    Exit;
  Lines := FileReadStr(CustomColorsFile).Lines;
  for I := 0 to CustomCount - 1 do
  begin
    if I >= Lines.Length then
      Break;
    C := clWhite;
    if TextToColor(Lines[I], C) then
      FCustom[I] := C;
  end;
end;

procedure TAdvancedColorDialogForm.SaveCustomColors;
var
  S: string;
  C: TColorB;
  I: Integer;
begin
  S := '';
  for I := 0 to CustomCount - 1 do
  begin
    C := FCustom[I];
    { Always write the alpha so each line is #RRGGBBAA }
    S := S + Format('#%.2X%.2X%.2X%.2X', [C.Red, C.Green, C.Blue, C.Alpha]) + LineEnding;
  end;
  try
    DirForce(FileExtractPath(CustomColorsFile));
    FileWriteStr(CustomColorsFile, S);
  except
    { Custom colors are a convenience, so a failed save is ignored }
  end;
end;

{ TAdvancedColorDialog }

constructor TAdvancedColorDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColor := clBlack;
  FAlpha := HiByte;
end;

function TAdvancedColorDialog.GetColorValue: TColorB;
begin
  Result := FColor;
  Result.Alpha := FAlpha;
end;

procedure TAdvancedColorDialog.SetColorValue(const Value: TColorB);
var
  C: TColorB;
begin
  C := Value;
  C.Alpha := HiByte;
  FColor := C.Color;
  FAlpha := Value.Alpha;
end;

function TAdvancedColorDialog.Execute: Boolean;
var
  Form: TAdvancedColorDialogForm;
begin
  Form := TAdvancedColorDialogForm.Create(nil);
  try
    if FTitle <> '' then
      Form.Caption := FTitle;
    Form.CustomColorsFile := FCustomColorsFile;
    Form.LoadCustomColors;
    Form.ColorValue := ColorValue;
    Result := Form.ShowModal = mrOK;
    if Result then
      ColorValue := Form.ColorValue;
  finally
    Form.Free;
  end;
end;

initialization
  InvariantFormat := DefaultFormatSettings;
  InvariantFormat.DecimalSeparator := '.';
end.
