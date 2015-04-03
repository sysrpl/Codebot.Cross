unit Main;

{$mode delphi}

interface

uses
  Classes, SysUtils, Graphics, Controls, Forms, StdCtrls,
  Codebot.System,
  Codebot.Graphics,
  Codebot.Graphics.Types,
  Codebot.Controls.Banner,
  Codebot.Controls.Scrolling,
  Codebot.Controls.Buttons,
  Codebot.Controls.Highlighter;

{ THighlightForm }

type
  THighlightForm = class(TBannerForm)
    CloseButton: TButton;
    OptionBox: TCheckBox;
    BrushList: TDetailsList;
    ImageStrip: TImageStrip;
    PasteButton: TThinButton;
    ClearButton: TThinButton;
    procedure BrushListDrawItem(Sender: TObject; Surface: ISurface;
      Index: Integer; Rect: TRectI; State: TDrawState);
    procedure FormCreate(Sender: TObject);
    procedure OptionBoxChange(Sender: TObject);
    procedure ButtonClick(Sender: TObject);
  private
    FHighlight: TControlHighlighter;
    FBrushes: StringArray;
  public
    { public declarations }
  end;

var
  HighlightForm: THighlightForm;

implementation

{$R *.lfm}

{ THighlightForm }

procedure THighlightForm.FormCreate(Sender: TObject);
var
  S: string;
begin
  FHighlight := TControlHighlighter.Create(Self);
  for S in EnumBrushStyles do
    FBrushes.Push(S);
  BrushList.Count := FBrushes.Length;
end;

procedure THighlightForm.BrushListDrawItem(Sender: TObject;
  Surface: ISurface; Index: Integer; Rect: TRectI; State: TDrawState);
var
  R: TRectI;
  B: IBrush;
begin
  FillRectColor(Surface, Rect, clWindow);
  if Index = BrushList.ItemIndex then
    FillRectSelected(Surface, Rect);
  R := Rect;
  R.Right := BrushList.GetColumnRect(0).Right;
  R.Inflate(-8, -8);
  Surface.StrokeRect(NewPen(clBlack, 1), R);
  B := StrToBrush(FBrushes[Index], clBlack, clTransparent);
  B.Matrix.Rotate(Pi / 4);
  Surface.FillRect(B, R);
  R := Rect;
  R.Left := BrushList.GetColumnRect(1).Left;
  R.Inflate(-4, -4);
  Surface.TextOut(Theme.Font, FBrushes[Index], R, drLeft);
end;

procedure THighlightForm.OptionBoxChange(Sender: TObject);
begin
  FHighlight.Visible := OptionBox.Checked;
end;

procedure THighlightForm.ButtonClick(Sender: TObject);
begin
  FHighlight.Control := Sender as TControl;
end;

end.

