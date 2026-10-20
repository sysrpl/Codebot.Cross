(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ Codebot.Themes.Dialog defines the dialog which lists the themes and the
  color schemes of the source editor, with a preview of the ones selected }

unit Codebot.Themes.Dialog;

{$mode delphi}

interface

{ TThemeChoice is what the dialog chooses: the index of a theme in
  ThemeList, and the color scheme and the font of the source editor }

type
  TThemeChoice = record
    Theme: Integer;
    Scheme: string;
    FontName: string;
    FontSize: Integer;
  end;

{ TThemeApplyProc applies a choice }

  TThemeApplyProc = procedure(const Choice: TThemeChoice);

{ Shows the dialog with Choice selected, returning true and the choice made
  if the user pressed OK. The Apply button calls Apply with the choice made
  so far, leaving the dialog open. }
function ShowThemesDialog(var Choice: TThemeChoice;
  Apply: TThemeApplyProc): Boolean;

implementation

uses
  Classes, SysUtils, Types, Forms, Controls, Graphics, StdCtrls, ExtCtrls,
  ComCtrls, Spin, LCLType,
  Codebot.Themes.Palettes,
  Codebot.Themes.Editor;

const
  { The range of the font size of the source editor }
  MinFontSize = 6;
  MaxFontSize = 72;
  DefaultFontSize = 10;

{ TThemesDialog has no form file, so its controls are created in code }

type
  TThemesDialog = class(TForm)
  private
    FApply: TThemeApplyProc;
    FPages: TPageControl;
    FList: TListBox;
    FPreview: TPaintBox;
    FSchemes: TListBox;
    FFonts: TComboBox;
    FSize: TSpinEdit;
    FCode: TPaintBox;
    function GetChoice: TThemeChoice;
    procedure SetChoice(const Value: TThemeChoice);
    procedure ApplyClick(Sender: TObject);
    procedure ListSelectionChange(Sender: TObject; User: Boolean);
    procedure ListDblClick(Sender: TObject);
    procedure PreviewPaint(Sender: TObject);
    procedure SchemesSelectionChange(Sender: TObject; User: Boolean);
    procedure FontChange(Sender: TObject);
    procedure CodePaint(Sender: TObject);
  public
    constructor CreateDialog(Apply: TThemeApplyProc);
    property Choice: TThemeChoice read GetChoice write SetChoice;
  end;

{ Adds a label and a control to the left of the bar, after the controls
  added before them }

procedure AddToBar(Bar: TPanel; const Caption: string; Control: TControl);
var
  Text: TLabel;
  X: Integer;
begin
  X := Bar.ControlCount * 1000;
  Text := TLabel.Create(Bar.Owner);
  Text.Parent := Bar;
  Text.Caption := Caption;
  Text.Layout := tlCenter;
  Text.Left := X;
  Text.Align := alLeft;
  Text.BorderSpacing.Left := 8;
  Text.BorderSpacing.Right := 6;
  Control.Parent := Bar;
  Control.Left := X + 1;
  Control.Align := alLeft;
  Control.BorderSpacing.Top := 6;
  Control.BorderSpacing.Bottom := 6;
end;

constructor TThemesDialog.CreateDialog(Apply: TThemeApplyProc);
var
  Panel: TPanel;
  Button: TButton;
  Sheet: TTabSheet;
  I: Integer;
begin
  inherited CreateNew(nil);
  FApply := Apply;
  Caption := 'Themes';
  BorderIcons := [biSystemMenu];
  Position := poMainFormCenter;
  ClientWidth := 680;
  ClientHeight := 480;
  Constraints.MinWidth := 500;
  Constraints.MinHeight := 320;
  Panel := TPanel.Create(Self);
  Panel.Parent := Self;
  Panel.Align := alBottom;
  Panel.Height := 44;
  Panel.BevelOuter := bvNone;
  { The buttons are aligned to the right of the panel, so that they stay in
    the corner whatever the width. Their left positions put them in the
    order Apply, OK, and Cancel. }
  Button := TButton.Create(Self);
  Button.Parent := Panel;
  Button.Caption := 'Cancel';
  Button.ModalResult := mrCancel;
  Button.Cancel := True;
  Button.Width := 88;
  Button.Left := 600;
  Button.Align := alRight;
  Button.BorderSpacing.Around := 8;
  Button := TButton.Create(Self);
  Button.Parent := Panel;
  Button.Caption := 'OK';
  Button.ModalResult := mrOK;
  Button.Default := True;
  Button.Width := 88;
  Button.Left := 500;
  Button.Align := alRight;
  Button.BorderSpacing.Top := 8;
  Button.BorderSpacing.Bottom := 8;
  Button := TButton.Create(Self);
  Button.Parent := Panel;
  Button.Caption := 'Apply';
  Button.Width := 88;
  Button.Left := 400;
  Button.Align := alRight;
  Button.BorderSpacing.Around := 8;
  Button.OnClick := ApplyClick;
  Button.Enabled := Assigned(Apply);
  FPages := TPageControl.Create(Self);
  FPages.Parent := Self;
  FPages.Align := alClient;
  FPages.BorderSpacing.Left := 8;
  FPages.BorderSpacing.Top := 8;
  FPages.BorderSpacing.Right := 8;
  { The themes of the windows of the IDE }
  Sheet := FPages.AddTabSheet;
  Sheet.Caption := 'Theme';
  FList := TListBox.Create(Self);
  FList.Parent := Sheet;
  FList.Align := alLeft;
  FList.Width := 180;
  FList.BorderSpacing.Around := 8;
  for I := Low(ThemeList) to High(ThemeList) do
    FList.Items.Add(ThemeList[I].Name);
  FList.OnSelectionChange := ListSelectionChange;
  FList.OnDblClick := ListDblClick;
  FPreview := TPaintBox.Create(Self);
  FPreview.Parent := Sheet;
  FPreview.Align := alClient;
  FPreview.BorderSpacing.Top := 8;
  FPreview.BorderSpacing.Right := 8;
  FPreview.BorderSpacing.Bottom := 8;
  FPreview.OnPaint := PreviewPaint;
  { The color schemes and the font of the source editor, with only the fonts
    whose characters have the same width. The tab is hidden when the IDE has
    no color schemes. }
  Sheet := FPages.AddTabSheet;
  Sheet.Caption := 'Editor';
  Panel := TPanel.Create(Self);
  Panel.Parent := Sheet;
  Panel.Align := alTop;
  Panel.Height := 36;
  Panel.BevelOuter := bvNone;
  FFonts := TComboBox.Create(Self);
  FFonts.Style := csDropDownList;
  FFonts.Sorted := True;
  FFonts.Width := 220;
  EditorMonospaceFonts(FFonts.Items);
  FFonts.OnChange := FontChange;
  AddToBar(Panel, 'Font', FFonts);
  FSize := TSpinEdit.Create(Self);
  FSize.MinValue := MinFontSize;
  FSize.MaxValue := MaxFontSize;
  FSize.Value := DefaultFontSize;
  FSize.Width := 64;
  FSize.OnChange := FontChange;
  AddToBar(Panel, 'Size', FSize);
  FSchemes := TListBox.Create(Self);
  FSchemes.Parent := Sheet;
  FSchemes.Align := alLeft;
  FSchemes.Width := 180;
  FSchemes.BorderSpacing.Around := 8;
  FSchemes.Sorted := True;
  EditorSchemeNames(FSchemes.Items);
  FSchemes.OnSelectionChange := SchemesSelectionChange;
  FSchemes.OnDblClick := ListDblClick;
  FCode := TPaintBox.Create(Self);
  FCode.Parent := Sheet;
  FCode.Align := alClient;
  FCode.BorderSpacing.Top := 8;
  FCode.BorderSpacing.Right := 8;
  FCode.BorderSpacing.Bottom := 8;
  FCode.OnPaint := CodePaint;
  Sheet.TabVisible := FSchemes.Count > 0;
  FPages.ActivePageIndex := 0;
end;

function TThemesDialog.GetChoice: TThemeChoice;
begin
  Result.Theme := FList.ItemIndex;
  if FSchemes.ItemIndex >= 0 then
    Result.Scheme := FSchemes.Items[FSchemes.ItemIndex]
  else
    Result.Scheme := '';
  if FFonts.ItemIndex >= 0 then
    Result.FontName := FFonts.Items[FFonts.ItemIndex]
  else
    Result.FontName := '';
  Result.FontSize := FSize.Value;
end;

{ A font of the editor which is not in the list, such as one which is not
  installed, is added to it, so that it stays chosen }

procedure TThemesDialog.SetChoice(const Value: TThemeChoice);
begin
  if (Value.Theme >= 0) and (Value.Theme < FList.Count) then
    FList.ItemIndex := Value.Theme;
  FSchemes.ItemIndex := FSchemes.Items.IndexOf(Value.Scheme);
  if (Value.FontName <> '') and (FFonts.Items.IndexOf(Value.FontName) < 0) then
    FFonts.Items.Add(Value.FontName);
  FFonts.ItemIndex := FFonts.Items.IndexOf(Value.FontName);
  if (Value.FontSize >= MinFontSize) and (Value.FontSize <= MaxFontSize) then
    FSize.Value := Value.FontSize
  else
    FSize.Value := DefaultFontSize;
end;

procedure TThemesDialog.ApplyClick(Sender: TObject);
begin
  if Assigned(FApply) and (FList.ItemIndex >= 0) then
    FApply(Choice);
end;

procedure TThemesDialog.ListSelectionChange(Sender: TObject; User: Boolean);
begin
  FPreview.Invalidate;
end;

procedure TThemesDialog.FontChange(Sender: TObject);
begin
  FCode.Invalidate;
end;

procedure TThemesDialog.ListDblClick(Sender: TObject);
begin
  if (Sender as TListBox).ItemIndex >= 0 then
    ModalResult := mrOK;
end;

procedure TThemesDialog.SchemesSelectionChange(Sender: TObject; User: Boolean);
begin
  FCode.Invalidate;
end;

{ Draws a window of the IDE in the colors and the font of the theme: a title
  bar, a menu bar, tabs, a list, an edit, buttons, a check box, and a status
  bar }

procedure TThemesDialog.PreviewPaint(Sender: TObject);
var
  Theme: Integer;
  Canvas: TCanvas;

  function C(Index: Integer): TColor;
  begin
    Result := ThemeColor(Theme, Index);
  end;

  procedure Fill(const R: TRect; Index: Integer);
  begin
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := C(Index);
    Canvas.FillRect(R);
  end;

  procedure Frame(const R: TRect; Index: Integer);
  begin
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := C(Index);
    Canvas.FrameRect(R);
  end;

  procedure Text(X, Y: Integer; const S: string; Index: Integer);
  begin
    Canvas.Brush.Style := bsClear;
    Canvas.Font.Color := C(Index);
    Canvas.TextOut(X, Y, S);
  end;

  procedure CenterText(const R: TRect; const S: string; Index: Integer);
  begin
    Text((R.Left + R.Right - Canvas.TextWidth(S)) div 2,
      (R.Top + R.Bottom - Canvas.TextHeight(S)) div 2, S, Index);
  end;

  procedure Button(const R: TRect; const S: string; Edge: Integer);
  begin
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := C(COLOR_BTNHIGHLIGHT);
    Canvas.Pen.Color := C(Edge);
    Canvas.RoundRect(R.Left, R.Top, R.Right, R.Bottom, 6, 6);
    CenterText(R, S, COLOR_BTNTEXT);
  end;

const
  MenuItems: array[0..4] of string = ('File', 'Edit', 'Search', 'View', 'Tools');
  ListItems: array[0..5] of string = ('TForm1', 'Button1', 'Edit1', 'Label1',
    'ListBox1', 'Panel1');
var
  R, Bar, Page, Item: TRect;
  H, X, Y, I, Middle: Integer;
begin
  Theme := FList.ItemIndex;
  if Theme < 0 then
    Exit;
  Canvas := FPreview.Canvas;
  Canvas.Font.Name := ThemeList[Theme].FontName;
  Canvas.Font.Size := ThemeList[Theme].FontSize;
  R := FPreview.ClientRect;
  H := Canvas.TextHeight('Wg');
  { The window }
  Fill(R, COLOR_BTNFACE);
  { The title bar }
  Bar := Rect(R.Left, R.Top, R.Right, R.Top + H + 12);
  Fill(Bar, COLOR_ACTIVECAPTION);
  Text(Bar.Left + 10, Bar.Top + 6, 'Lazarus IDE', COLOR_CAPTIONTEXT);
  { The menu bar, with the item under the mouse highlighted }
  Bar := Rect(R.Left, Bar.Bottom, R.Right, Bar.Bottom + H + 8);
  Fill(Bar, COLOR_MENUBAR);
  X := Bar.Left + 4;
  for I := Low(MenuItems) to High(MenuItems) do
  begin
    Item := Rect(X, Bar.Top, X + Canvas.TextWidth(MenuItems[I]) + 16, Bar.Bottom);
    if I = 2 then
      Fill(Item, COLOR_MENUHILIGHT);
    CenterText(Item, MenuItems[I], COLOR_MENUTEXT);
    X := Item.Right;
  end;
  { The status bar }
  Bar := Rect(R.Left, R.Bottom - H - 8, R.Right, R.Bottom);
  Fill(Bar, COLOR_BTNFACE);
  Fill(Rect(Bar.Left, Bar.Top, Bar.Right, Bar.Top + 1), COLOR_3DLIGHT);
  Text(Bar.Left + 8, Bar.Top + 4, 'Ready', COLOR_BTNTEXT);
  { The tabs, the first one selected }
  Y := R.Top + (H + 12) + (H + 8) + 8;
  Item := Rect(R.Left + 8, Y, R.Left + 8 + Canvas.TextWidth('unit1.pas') + 24, Y + H + 10);
  Fill(Item, COLOR_BTNHIGHLIGHT);
  Frame(Item, COLOR_3DLIGHT);
  CenterText(Item, 'unit1.pas', COLOR_BTNTEXT);
  Item := Rect(Item.Right - 1, Y + 2, Item.Right - 1 + Canvas.TextWidth('project1.lpr') + 24, Y + H + 10);
  Frame(Item, COLOR_3DLIGHT);
  CenterText(Item, 'project1.lpr', COLOR_BTNTEXT);
  Page := Rect(R.Left + 8, Item.Bottom - 1, R.Right - 8, Bar.Top - 8);
  Frame(Page, COLOR_3DLIGHT);
  InflateRect(Page, -10, -10);
  { A list with one item selected }
  Middle := Page.Left + (Page.Right - Page.Left) * 2 div 5;
  Item := Rect(Page.Left, Page.Top, Middle, Page.Bottom);
  Fill(Item, COLOR_WINDOW);
  Frame(Item, COLOR_3DLIGHT);
  Y := Item.Top + 2;
  for I := Low(ListItems) to High(ListItems) do
  begin
    if Y + H + 4 > Item.Bottom - 2 then
      Break;
    if I = 2 then
    begin
      Fill(Rect(Item.Left + 2, Y, Item.Right - 2, Y + H + 4), COLOR_HIGHLIGHT);
      Text(Item.Left + 8, Y + 2, ListItems[I], COLOR_HIGHLIGHTTEXT);
    end
    else
      Text(Item.Left + 8, Y + 2, ListItems[I], COLOR_WINDOWTEXT);
    Inc(Y, H + 4);
  end;
  { An edit, two buttons, a check box, and text which is disabled }
  X := Middle + 16;
  Y := Page.Top;
  Item := Rect(X, Y, Page.Right, Y + H + 10);
  Fill(Item, COLOR_WINDOW);
  Frame(Item, COLOR_3DLIGHT);
  Text(Item.Left + 6, Item.Top + 5, 'An edit', COLOR_WINDOWTEXT);
  Y := Item.Bottom + 12;
  Button(Rect(X, Y, X + 96, Y + H + 12), 'Button', COLOR_3DLIGHT);
  Button(Rect(X + 108, Y, X + 204, Y + H + 12), 'Default', COLOR_HOTLIGHT);
  Y := Y + H + 24;
  Item := Rect(X, Y + 2, X + H - 2, Y + H);
  Fill(Item, COLOR_HOTLIGHT);
  Text(Item.Right + 8, Y, 'A check box', COLOR_BTNTEXT);
  Y := Y + H + 12;
  Text(X, Y, 'Disabled text', COLOR_GRAYTEXT);
  Y := Y + H + 8;
  Text(X, Y, 'An accent', COLOR_HOTLIGHT);
end;

{ Draws Pascal source in the colors of the scheme selected, with a gutter of
  line numbers on its left. The source is split into the kinds of text the
  highlighter of the IDE knows, such as comments, strings, and reserved
  words, and each kind is drawn with its attribute in the scheme. }

const
  CodeLines: array[0..24] of string = (
    'unit Shapes;',
    '',
    '{$mode delphi}',
    '',
    'interface',
    '',
    'type',
    '  { A shape which draws its name }',
    '  TShape = class',
    '  private',
    '    FName: string;',
    '  public',
    '    procedure Draw(Canvas: TCanvas); virtual;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TShape.Draw(Canvas: TCanvas);',
    'var',
    '  I: Integer;',
    'begin',
    '  // Draw the name ten times',
    '  for I := 1 to 10 do',
    '    Canvas.TextOut(I * 12, 8, ''Shape '' + FName);',
    'end;');
  ReservedWords = ' begin class do end for implementation interface private ' +
    'procedure public string to type unit var virtual ';

procedure TThemesDialog.CodePaint(Sender: TObject);
var
  Scheme: string;
  Canvas: TCanvas;
  Default: TEditorStyle;

  { The look of an attribute, with the colors the scheme leaves to the
    default text filled in }
  function Look(const Attribute: string): TEditorStyle;
  begin
    if not EditorSchemeStyle(Scheme, Attribute, Result) then
      Result := Default;
    if (Result.Fore = clNone) or (Result.Fore = clDefault) then
      Result.Fore := Default.Fore;
    if (Result.Back = clNone) or (Result.Back = clDefault) then
      Result.Back := Default.Back;
  end;

  procedure Draw(var X: Integer; Y: Integer; const S: string;
    const Attribute: string);
  var
    Style: TEditorStyle;
    W: Integer;
  begin
    Style := Look(Attribute);
    Canvas.Font.Style := Style.Style;
    W := Canvas.TextWidth(S);
    if Style.Back <> Default.Back then
    begin
      Canvas.Brush.Style := bsSolid;
      Canvas.Brush.Color := Style.Back;
      Canvas.FillRect(Rect(X, Y, X + W, Y + Canvas.TextHeight('Wg')));
    end;
    Canvas.Brush.Style := bsClear;
    Canvas.Font.Color := Style.Fore;
    Canvas.TextOut(X, Y, S);
    Inc(X, W);
  end;

  { Splits a line into its kinds of text. The comments of the source never
    span lines, so each line is split on its own. }
  procedure DrawLine(X, Y: Integer; const Line: string);
  var
    P, J: Integer;
    Word, Attribute: string;
  begin
    P := 1;
    while P <= Length(Line) do
    begin
      J := P + 1;
      if Line[P] = '{' then
      begin
        while (J <= Length(Line)) and (Line[J] <> '}') do
          Inc(J);
        Inc(J);
        if Copy(Line, P, 2) = '{$' then
          Attribute := 'Directive'
        else
          Attribute := 'Comment';
      end
      else if Copy(Line, P, 2) = '//' then
      begin
        J := Length(Line) + 1;
        Attribute := 'Comment';
      end
      else if Line[P] = '''' then
      begin
        while (J <= Length(Line)) and (Line[J] <> '''') do
          Inc(J);
        Inc(J);
        Attribute := 'String';
      end
      else if Line[P] in ['0'..'9'] then
      begin
        while (J <= Length(Line)) and (Line[J] in ['0'..'9']) do
          Inc(J);
        Attribute := 'Number';
      end
      else if Line[P] in ['A'..'Z', 'a'..'z', '_'] then
      begin
        while (J <= Length(Line)) and (Line[J] in ['A'..'Z', 'a'..'z', '_', '0'..'9']) do
          Inc(J);
        Word := ' ' + LowerCase(Copy(Line, P, J - P)) + ' ';
        if Pos(Word, ReservedWords) > 0 then
          Attribute := 'Reserved word'
        else
          Attribute := 'Identifier';
      end
      else if Line[P] = ' ' then
      begin
        while (J <= Length(Line)) and (Line[J] = ' ') do
          Inc(J);
        Attribute := 'Space';
      end
      else
        Attribute := 'Symbol';
      Draw(X, Y, Copy(Line, P, J - P), Attribute);
      P := J;
    end;
  end;

var
  R: TRect;
  Gutter, Numbers: TEditorStyle;
  H, X, Y, W, I: Integer;
begin
  if FSchemes.ItemIndex < 0 then
    Exit;
  Scheme := FSchemes.Items[FSchemes.ItemIndex];
  Canvas := FCode.Canvas;
  if FFonts.ItemIndex >= 0 then
    Canvas.Font.Name := FFonts.Items[FFonts.ItemIndex]
  else
    Canvas.Font.Name := {$ifdef windows}'Consolas'{$else}'Monospace'{$endif};
  Canvas.Font.Size := FSize.Value;
  if not EditorSchemeStyle(Scheme, 'ahaDefault', Default) then
    Default.Style := [];
  if (Default.Fore = clNone) or (Default.Fore = clDefault) then
    Default.Fore := clWindowText;
  if (Default.Back = clNone) or (Default.Back = clDefault) then
    Default.Back := clWindow;
  Gutter := Look('ahaGutter');
  Numbers := Look('ahaLineNumber');
  R := FCode.ClientRect;
  H := Canvas.TextHeight('Wg');
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Default.Back;
  Canvas.FillRect(R);
  { The gutter, wide enough for two digits }
  W := Canvas.TextWidth('00') + 16;
  Canvas.Brush.Color := Gutter.Back;
  Canvas.FillRect(Rect(R.Left, R.Top, R.Left + W, R.Bottom));
  Y := R.Top + 4;
  for I := Low(CodeLines) to High(CodeLines) do
  begin
    if Y + H > R.Bottom then
      Break;
    Canvas.Font.Style := Numbers.Style;
    Canvas.Font.Color := Numbers.Fore;
    Canvas.Brush.Style := bsClear;
    X := R.Left + W - 8 - Canvas.TextWidth(IntToStr(I + 1));
    Canvas.TextOut(X, Y, IntToStr(I + 1));
    DrawLine(R.Left + W + 8, Y, CodeLines[I]);
    Inc(Y, H);
  end;
end;

function ShowThemesDialog(var Choice: TThemeChoice;
  Apply: TThemeApplyProc): Boolean;
var
  Dialog: TThemesDialog;
begin
  Dialog := TThemesDialog.CreateDialog(Apply);
  try
    Dialog.Choice := Choice;
    Result := (Dialog.ShowModal = mrOK) and (Dialog.FList.ItemIndex >= 0);
    if Result then
      Choice := Dialog.Choice;
  finally
    Dialog.Free;
  end;
end;

end.
