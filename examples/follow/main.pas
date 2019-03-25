unit Main;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, LCLIntf, ExtCtrls,
  Codebot.System,
  Codebot.Graphics,
  Codebot.Graphics.Types,
  Codebot.Forms.Widget;

{ TFollowForm }

type
  TFollowForm = class(TWidget)
    MoveTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure MoveTimerTimer(Sender: TObject);
	private
    FPoint: TPoint;
    FColor: TColorB;
    FOpacity: Single;
    FSize: Integer;
    FPen: Single;
  protected
    procedure Render; override;
	public
    procedure HandleCommand(const Message: string);
  end;

var
  FollowForm: TFollowForm;

implementation

{$R *.lfm}

{ TFollowForm }

procedure TFollowForm.FormCreate(Sender: TObject);
var
  S: string;
  I: Integer;
begin
  S := SwitchValue('size');
  FSize := StrToIntDef(S, 0);
  if FSize < 20 then
    FSize := 20
  else if FSize > 200 then
    FSize := 200;
  SetBounds(0, 0, FSize, FSize);
  S := SwitchValue('color');
  I := StrToIntDef(S, -1);
  if I < 0 then
  begin
    FColor := StrToColor(S);
    if FColor = clTransparent then
      FColor := clYellow;
  end
  else
    FColor := TColor(I);
  S := SwitchValue('opacity');
  FOpacity := StrToFloatDef(S, 0);
  if FOpacity < 0.1 then
  	FOpacity := 0.1
  else if FOpacity > 1 then
    FOpacity := 1;
  FColor := FColor.Fade(FOpacity);
  S := SwitchValue('pen');
  FPen := StrToFloatDef(S, 0);
  if FPen < 0.1 then
  	FPen := 0
  else if FPen > FSize div 2 - 3 then
    FPen := 0;
  EdgeSizable := [];
  Interactive := False;
end;

procedure TFollowForm.HandleCommand(const Message: string);
var
  M, V: string;
  C: TColorB;
  F: Single;
  I: Integer;
begin
	M := Message.FirstOf(' ');
	V := Message.SecondOf(' ');
  if M = 'color' then
  begin
    C := StrToColor(V);
    if FColor = clTransparent then
      C := FColor;
		if C <> FColor then
    begin
      FColor := C;
      FColor := FColor.Fade(FOpacity);
      Invalidate;
    end;
  end
  else if M = 'opacity' then
  begin
    F := StrToFloatDef(V, 0);
    if F < 0.1 then
    	F := 0.1
    else if F > 1 then
      F := 1;
    if F <> FOpacity then
    begin
      FOpacity := F;
      FColor.Alpha := High(Byte);;
      FColor := FColor.Fade(FOpacity);
      Invalidate;
    end;
  end
  else if M = 'pen' then
  begin
    F := StrToFloatDef(V, 0);
    if F < 0.1 then
    	F := 0
    else if F > 1000 then
      F := 1000;
    if F <> FPen then
    begin
      FPen := F;
      Invalidate;
    end;
  end
  else if M = 'size' then
  begin
    I := StrToIntDef(V, 0);
    if I < 20 then
    	I := 20
    else if I > 200 then
      I := 200;
    if I <> FSize then
    begin
      FSize := I;
      Width := FSize;
      Height := FSize;
      with Mouse.CursorPos do
	      SetBounds(X - FSize div 2, Y - FSize div 2, FSize, FSize);
      Invalidate;
    end;
  end
  else if M = 'stop' then
  	Close;
end;

procedure TFollowForm.MoveTimerTimer(Sender: TObject);
var
  P: TPoint;
begin
	P := Mouse.CursorPos;
  if P <> FPoint then
  begin
    FPoint := P;
    Left := P.X - Width div 2;
    Top := P.Y - Height div 2;
  end;
end;

procedure TFollowForm.Render;
var
  B: IBrush;
  R: TRectF;
begin
  Surface.Clear(clTransparent);
  B := NewBrush(FColor);
  R := ClientRect;
  R.Inflate(-1, -1);
  if FPen > 0 then
  begin
    R.Inflate(FPen / -2, FPen / -2);
    Surface.Ellipse(R);
    Surface.Stroke(NewPen(B, FPen));
  end
  else
  begin
    Surface.Ellipse(R);
    Surface.Fill(B);
  end;
end;

end.

