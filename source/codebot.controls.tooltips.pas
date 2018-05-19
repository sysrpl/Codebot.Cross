(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2015                               *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.controls.tooltips.txt> }
unit Codebot.Controls.Tooltips;

{$mode delphi}

interface

{$if defined(linuxgtk)} // or defined(windows)}
uses
  Classes, SysUtils, Graphics, Controls, ExtCtrls, Forms,
  Codebot.System,
  Codebot.Graphics,
  Codebot.Graphics.Types,
  Codebot.Forms.Management;

var
  UseTipify: Boolean = False;

procedure Tipify(HintControl: TControl);
{$endif}

implementation

{$if defined(linuxgtk)} // or defined(windows)}
{ TTipMaster }

type
  TTipMaster = class(TComponent)
  public
    Form: TCustomForm;
    Control: TControl;
    Timer: TTimer;
    Tip: ISplash;
    TipFont: IFont;
    MouseAt: TPoint;
    constructor Create(AOwner: TComponent); override;
    procedure DoTipify(HintControl: TControl);
    procedure DoActive(Sender: TObject);
  end;

constructor TTipMaster.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Timer := TTimer.Create(Self);
  Timer.Enabled := False;
  Tip := NewSplash;
  Tip.Bitmap.SetSize(100, 100);
  Tip.Opacity := 0;
  Tip.Visible := True;
  TipFont := NewFont(Screen.HintFont);
  TipFont.Color := TColor($0D4848);
end;

procedure TTipMaster.DoTipify(HintControl: TControl);
var
  Surface: ISurface;
  Shape: TRectF;
  Rect: TRectI;
  P: IPen;
  G: IGradientBrush;
  Size, Point: TPointI;
  S: string;
begin
  if Control = HintControl then
    Exit;
  Timer.Enabled := False;
  Timer.Interval := 30;
  Timer.Tag := 0;
  Timer.OnTimer := nil;
  if HintControl = nil then
  begin
    Tip.Opacity := 0;
    Exit;
  end;
  Control := HintControl;
  Form := FormManager.ParentForm(Control);
  if Form <> FormManager.Current then
  begin
    Control := nil;
    Form := nil;
    Exit;
  end;
  S := HintControl.Hint;
  S := S.Trim;
  if S.Length < 1 then
    Exit;
  Size := TPointI(Tip.Bitmap.Surface.TextSize(TipFont, S));
  Tip.Bitmap.SetSize(Size.X + 30, Size.Y + 40);
  Surface := Tip.Bitmap.Surface;
  Surface.Clear(clTransparent);
  Rect := Tip.Bitmap.ClientRect;
  Shape := Rect;
  Shape.Inflate(-8, -8);
  Shape.Bottom := Shape.Bottom - 16;
  Surface.RoundRectangle(Shape, 6);
  Surface.Path.Add;
  Surface.MoveTo(Shape.MidPoint.X, Shape.Bottom + 12);
  Surface.LineTo(Shape.MidPoint.X - 16, Shape.Bottom - 4);
  Surface.LineTo(Shape.MidPoint.X + 16, Shape.Bottom - 4);
  Surface.Path.Close;
  P := NewPen(TColorB(clBlack).Fade(0.05), 10);
  while P.Width > 1 do
  begin
    Surface.Stroke(P, True);
    P.Width := P.Width - 2;
  end;
  Surface.Stroke(NewPen(TColor($259494), 2), True);
  G := NewBrush(Shape.TopLeft, Shape.BottomLeft);
  G.AddStop(TColor($D5F2F2), 0);
  G.AddStop(TColor($92E9E9), 0.4);
  Surface.Fill(G);
  Surface.TextOut(TipFont, S, Shape, drCenter);
  Point := TPointI.Create(HintControl.ClientWidth div 2, -Size.Y - 4);
  Point := HintControl.ClientToScreen(Point);
  Rect := Tip.Bitmap.ClientRect;
  Rect.Center(Point);
  Tip.Move(Rect.X, Rect.Y + 8);
  Tip.Update;
  Tip.Opacity := $FF;
  Timer.OnTimer := DoActive;
  Timer.Enabled := True;
  MouseAt := Mouse.CursorPos;
end;

procedure TTipMaster.DoActive(Sender: TObject);
var
  Changed: Boolean;
  P: TPoint;
begin
  P := Mouse.CursorPos;
  Changed := (P.X <> MouseAt.X) and (P.Y <> MouseAt.Y);
  if Changed or (Form <> FormManager.Current) or (not Form.Enabled) or (not Control.Enabled) then
  begin
    Form := nil;
    Control := nil;
    Timer.Enabled := False;
    Tip.Opacity := 0;
  end;
end;

var
  TipMaster: TTipMaster;

procedure Tipify(HintControl: TControl);
begin
  if TipMaster = nil then
    TipMaster := TTipMaster.Create(Application);
  TipMaster.DoTipify(HintControl);
end;
{$endif}

end.

