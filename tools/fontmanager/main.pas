unit Main;

{$mode delphi}

interface

uses
  FontData,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, LCLType,
  Codebot.System,
  Codebot.Text,
  Codebot.Text.Xml,
  Codebot.Controls.Scrolling, Codebot.Controls.Grids,
  Codebot.Graphics,
  Codebot.Graphics.Types;

{ TMaterialIconForm }

type
  TMaterialIconForm = class(TForm)
    GlyphList: TDrawList;
    SearchLabel: TLabel;
    NameEdit: TEdit;
    SearchEdit: TEdit;
    NameLabel: TLabel;
    CharEdit: TEdit;
    CharLabel: TLabel;
    GlyphGrid: TContentGrid;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GlyphListDrawItem(Sender: TObject; Surface: ISurface;
      Index: Integer; Rect: TRectI; State: TDrawState);
    procedure GlyphListSelectItem(Sender: TObject);
    procedure SearchEditChange(Sender: TObject);
    procedure SearchEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure GlyphGridDrawCell(Sender: TObject; Surface: ISurface; Col,
      Row: Integer; Rect: TRectI; State: TDrawState);
    procedure GlyphGridDrawBackground(Sender: TObject; Surface: ISurface;
      Rect: TRectI);
    procedure GlyphGridSelection(Sender: TObject; Col, Row: Integer;
      var Allow: Boolean);
  private
    FFont: IFont;
    FGlyph: IFont;
    FLargeGlyph: IFont;
  end;

var
  MaterialIconForm: TMaterialIconForm;

implementation

{$R *.lfm}

{ TMaterialIconForm }

function UpConvert(A, B: string): string;
var
  NeedsUp: Boolean;
  S: string;
  I: Integer;
begin
  S := 'glyph' + UpCase(A[1]);
  NeedsUp := False;
  for I := 2 to Length(A) do
  begin
    if A[I] = '-' then
    begin
      NeedsUp := True;
      Continue
    end;
    if NeedsUp then
      S := S + UpCase(A[I])
    else
      S := S + A[I];
    NeedsUp := False;
  end;
  B := B.Replace('0x', ' $').ToUpper;
  Result := S + ' = ' + B;
end;

procedure Convert;
var
  Strings, Names: TStrings;
  A, B: string;
  I: Integer;
  Doc: IDocument;
  N: INode;
begin
  Strings := TStringList.Create;
  Strings.Add('const');
  Strings.Add('  Icons: array of string = [');
  Names :=  TStringList.Create;;
  Doc := DocumentCreate;
  Doc.Load('/home/gigauser/tmp/maps.xml');
  A := '   ';
  I := 0;
  for N in Doc.Force('maps') .Nodes do
  begin
    if Names.IndexOf(N.Attributes.ByName['name'].Text) > -1 then
      Continue;
    Names.Add(N.Attributes.ByName['name'].Text);
    B := N.Attributes.ByName['code'].Text;
    B := B.Replace('0x', ' $').ToUpper;
    A := A + ' ''' + N.Attributes.ByName['name'].Text + B + ''',';
    Inc(I);
    if I = 3 then
    begin
      Strings.Add(A);
      A := ' ';
      I := 0;
    end;
  end;
  if A <> '   ' then
    Strings.Add(A);
  Strings.Add('  ];');
  Strings.Add('');
  Strings.Add('const');
  Names.Clear;
  I := 0;
  A := '  ';
  for N in Doc.Force('maps') .Nodes do
  begin
    if Names.IndexOf(N.Attributes.ByName['name'].Text) > -1 then
      Continue;
    Names.Add(N.Attributes.ByName['name'].Text);
    B := UpConvert(N.Attributes.ByName['name'].Text, N.Attributes.ByName['code'].Text);
    A := A + ' ' + B + '; ';
    Inc(I);
    if I = 3 then
    begin
      Strings.Add(A);
      A := ' ';
      I := 0;
    end;
  end;
  if A <> '   ' then
    Strings.Add(A);
  Strings.SaveToFile('/home/gigauser/tmp/maps.pas');
  Strings.Free;
  Names.Free;
end;

procedure TMaterialIconForm.Button1Click(Sender: TObject);
begin
end;

procedure TMaterialIconForm.FormCreate(Sender: TObject);
begin
  GlyphList.Count := Length(MaterialGlyphs);
  GlyphGrid.RowCount := Length(MaterialGlyphs) div GlyphGrid.ColCount + 1;
end;

procedure TMaterialIconForm.GlyphListSelectItem(Sender: TObject);
var
  NeedsUp: Boolean;
  S, T: string;
  Col, Row, I: Integer;
  R: TRectI;
begin
  if GlyphList.ItemIndex < 0 then
    Exit;
  I := GlyphList.ItemIndex;
  Col := I mod GlyphGrid.ColCount;
  Row := I div GlyphGrid.ColCount;
  GlyphGrid.OnSelection := nil;
  GlyphGrid.Selection := Point(Col, Row);
  GlyphGrid.OnSelection := GlyphGridSelection;
  R := GlyphList.ItemRect(GlyphList.ItemIndex);
  if R.Bottom > GlyphList.ClientHeight then
    GlyphList.TopIndex := GlyphList.TopIndex + 1;
  S := MaterialGlyphs[GlyphList.ItemIndex];
  S :=  S.Split(' ')[1];
  CharEdit.Text := UnicodeToStr(StrToInt(S));
  S := MaterialGlyphs[GlyphList.ItemIndex];
  S := S.Split(' ')[0];
  T := 'glyph' + UpCase(S[1]);
  NeedsUp := False;
  for I := 2 to Length(S) do
  begin
    if S[I] = '-' then
    begin
      NeedsUp := True;
      Continue
    end;
    if NeedsUp then
      T := T + UpCase(S[I])
    else
      T := T + S[I];
    NeedsUp := False;
  end;
  S := MaterialGlyphs[GlyphList.ItemIndex];
  S := S.Split(' ')[1];
  S[1] := ' ';
  NameEdit.Text := T + ' \u' + S.Trim;
end;

procedure TMaterialIconForm.SearchEditChange(Sender: TObject);
var
  S: string;
  I: Integer;
begin
  S := SearchEdit.Text;
  S := S.trim;
  for I := Low(MaterialGlyphs) to High(MaterialGlyphs) do
    if MaterialGlyphs[I].Contains(S, True) then
    begin
      GlyphList.ItemIndex := I;
      GlyphList.TopIndex := I;
      Break;
    end;
end;

procedure TMaterialIconForm.SearchEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  S: string;
  I: Integer;
begin
  if Key = VK_DOWN then
  begin
    S := SearchEdit.Text;
    S := S.Trim;
    for I := GlyphList.ItemIndex + 1 to High(MaterialGlyphs) do
      if MaterialGlyphs[I].Contains(S, True) then
      begin
        GlyphList.ItemIndex := I;
        GlyphList.TopIndex := I;
        Exit;
      end;
    for I := 0 to GlyphList.ItemIndex - 1 do
      if MaterialGlyphs[I].Contains(S, True) then
      begin
        GlyphList.ItemIndex := I;
        GlyphList.TopIndex := I;
        Exit;
      end;
  end;
  if Key = VK_UP then
  begin
    S := SearchEdit.Text;
    S := S.Trim;
    for I := GlyphList.ItemIndex - 1 downto 0 do
      if MaterialGlyphs[I].Contains(S, True) then
      begin
        GlyphList.ItemIndex := I;
        GlyphList.TopIndex := I;
        Exit;
      end;
    for I := High(MaterialGlyphs) downto GlyphList.ItemIndex + 1 do
      if MaterialGlyphs[I].Contains(S, True) then
      begin
        GlyphList.ItemIndex := I;
        GlyphList.TopIndex := I;
        Exit;
      end;
  end;
end;

procedure TMaterialIconForm.GlyphGridDrawBackground(Sender: TObject;
  Surface: ISurface; Rect: TRectI);
begin
  Surface.FillRect(NewBrush(Self.CurrentColor), Rect);
end;

procedure TMaterialIconForm.GlyphGridSelection(Sender: TObject; Col,
  Row: Integer; var Allow: Boolean);
var
  I: Integer;
begin
  I := Col + Row * GlyphGrid.ColCount;
  Allow := I < Length(MaterialGlyphs);
  if Allow then
  begin
    GlyphList.ItemIndex := I;
    GlyphListSelectItem(GlyphList);
  end;
end;

procedure TMaterialIconForm.GlyphGridDrawCell(Sender: TObject;
  Surface: ISurface; Col, Row: Integer; Rect: TRectI; State: TDrawState);
var
  S: string;
  I: Integer;
begin
  if FFont = nil then
    FFont := NewFont(Self.Font);
  if FLargeGlyph = nil then
  begin
    FLargeGlyph := NewFont(CharEdit.Font);
    FLargeGlyph.Size := 30;
  end;
  DrawRectState(Surface, Rect, State);
  I := Col + Row * GlyphGrid.ColCount;
  if I < Length(MaterialGlyphs) then
  begin
    S := MaterialGlyphs[I];
    S :=  S.Split(' ')[1];
    Surface.TextOut(FLargeGlyph, UnicodeToStr(StrToInt(S)), Rect, drCenter);
  end;
end;

procedure TMaterialIconForm.GlyphListDrawItem(Sender: TObject; Surface: ISurface;
  Index: Integer; Rect: TRectI; State: TDrawState);
var
  S: string;
begin
  if FFont = nil then
    FFont := NewFont(Self.Font);
  if FGlyph = nil then
  begin
    FGlyph := NewFont(CharEdit.Font);
    FGlyph.Size := 24;
  end;
  DrawRectState(Surface, Rect, State);
  S := MaterialGlyphs[Index];
  S :=  S.Split(' ')[1];
  Surface.TextOut(FGlyph, UnicodeToStr(StrToInt(S)), Rect, drLeft);
  Rect.Left := Rect.Left + 50;
  Surface.TextOut(FFont, MaterialGlyphs[Index], Rect, drLeft);
end;

end.

