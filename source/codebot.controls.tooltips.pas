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

uses
  Classes, SysUtils, Graphics, Controls, ExtCtrls, Forms,
  Codebot.System,
  Codebot.Graphics,
  Codebot.Graphics.Types,
  Codebot.Forms.Management;

var
  UseTipify: Boolean = True;

procedure Tipify(HintControl: TControl);

implementation

{ TTipMaster }

type
  TTipMaster = class(TComponent)
  public
    Form: TCustomForm;
    Control: TControl;
    Timer: TTimer;
    Tip: ISplash;
    TipFont: IFont;
    constructor Create(AOwner: TComponent); override;
    procedure DoTipify(HintControl: TControl);
    procedure DoActive(Sender: TObject);
    procedure DoWaitShowTimer(Sender: TObject);
    procedure DoShowTimer(Sender: TObject);
    procedure DoHideTimer(Sender: TObject);
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
    if Control <> nil then
    begin
      Timer.OnTimer := DoHideTimer;
      Timer.Enabled := True;
    end;
    Exit;
  end;
  Control := HintControl;
  Form := FormParent(Control);
  if Form <> FormCurrent then
  begin
    Control := nil;
    Form := nil;
    Exit;
  end;
  Tip.Opacity := 0;
  Tip.Visible := False;
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
  Tip.Move(Rect.X,Rect.Y);
  Tip.Update;
  Timer.OnTimer := DoWaitShowTimer;
  Timer.Enabled := True;
end;

procedure TTipMaster.DoActive(Sender: TObject);
begin
  if (Form <> FormCurrent) or (not Form.Enabled) or (not Control.Enabled) then
  begin
    Form := nil;
    Control := nil;
    Timer.Enabled := False;
    Tip.Opacity := 0;
    Tip.Visible := True;
  end;
end;

procedure TTipMaster.DoWaitShowTimer(Sender: TObject);
var
  F: Float;
begin
  Timer.Tag := Timer.Tag + 1;
  F := Timer.Tag / 5;
  if F >= 1 then
  begin
    Tip.Opacity := 0;
    Tip.Visible := True;
    Timer.Tag := 0;
    Timer.OnTimer := DoShowTimer;
  end;
end;

procedure TTipMaster.DoShowTimer(Sender: TObject);
var
  F: Float;
begin
  Timer.Tag := Timer.Tag + 1;
  F := Timer.Tag / 10;
  if F >= 1 then
  begin
    Tip.Opacity := $FF;
    Timer.Enabled := False;
    Timer.Interval := 100;
    Timer.OnTimer := DoActive;
    Timer.Enabled := True;
  end
  else
    Tip.Opacity := Round(F * $FF);
end;

procedure TTipMaster.DoHideTimer(Sender: TObject);
var
  F: Float;
begin
  Timer.Tag := Timer.Tag + 1;
  F := Timer.Tag / 6;
  if F >= 1 then
  begin
    Control := nil;
    Form :=  nil;
    Tip.Opacity := 0;
    Tip.Visible := False;
    Timer.Enabled := False;
  end
  else
  begin
    F := 1 - F;
    Tip.Opacity := Round(F * $FF);
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

end.

