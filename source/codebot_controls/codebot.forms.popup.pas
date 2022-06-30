(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified March 2015                                 *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.forms.popup.txt> }
unit Codebot.Forms.Popup;

{$i codebot.inc}

interface

uses
  Classes, SysUtils, Controls, Graphics, Forms,
  LCLType, LCLIntf,
  Codebot.System,
  Codebot.Graphics,
  Codebot.Graphics.Types,
  Codebot.Input.MouseMonitor;

{ TPopupForm }

type
  TPopupForm = class(TCustomForm)
  private
    FAssociate: TControl;
    FForm: TCustomForm;
    FKeyPreview: Boolean;
    FKeyDown: TKeyEvent;
    FKeyUp: TKeyEvent;
    procedure AlignFull(Control: TControl);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MouseNotify(Kind: TMouseNotifyKind; Button: TMouseButton; X,
      Y: Integer);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Popup(Associate: TControl);
    procedure Dismiss;
  end;

implementation

{ TPopupForm }

constructor TPopupForm.Create(AOwner: TComponent);
begin
  inherited CreateNew(AOwner);
  Visible := False;
  FormStyle := fsSystemStayOnTop;
  BorderStyle := bsNone;
end;

procedure TPopupForm.AlignFull(Control: TControl);
var
  R: TRectI;
begin
  Control.Parent := Self;
  R := ClientRect;
  R.Inflate(-1, -1);
  Control.BoundsRect := R;
  Control.Anchors := [akLeft, akTop, akRight, akBottom];
end;

procedure TPopupForm.Popup(Associate: TControl);
var
  W: TWinControl;
  R: TRectI;
  P: TPointI;
begin
  if Visible then
    Exit;
  W := Associate.Parent;
  while W <> nil do
  begin
    if W is TCustomForm then
      FForm := W as TCustomForm;
    W := W.Parent;
  end;
  if FForm = nil then
    Exit;
  FAssociate := Associate;
  FKeyPreview := FForm.KeyPreview;
  FForm.KeyPreview := True;
  FKeyDown := FForm.OnKeyDown;
  FForm.OnKeyDown := FormKeyDown;
  FKeyUp := FForm.OnKeyUp;
  FForm.OnKeyUp := FormKeyUp;
  P := TPointI.Create;
  P := FAssociate.ClientToScreen(P);
  R := FAssociate.BoundsRect;
  R.Offset(-R.TopLeft);
  R.Offset(P);
  R.Offset(-1, R.Height);
  R.Height := 100;
  BoundsRect := R;
  W := FForm.ActiveControl;
  Visible := True;
  FForm.BringToFront;
  FForm.SetFocus;
  if W <> nil then
    W.SetFocus;
  MouseMonitor.Add(MouseNotify);
end;

procedure TPopupForm.MouseNotify(Kind: TMouseNotifyKind;
  Button: TMouseButton; X, Y: Integer);
var
  R: TRectI;
begin
  if Kind = nkButtonDown then
  begin
    R := BoundsRect;
    if not R.Contains(X, Y) then
      Dismiss;
  end;
end;

procedure TPopupForm.Dismiss;
begin
  if not Visible then
    Exit;
  MouseMonitor.Remove(MouseNotify);
  FForm.OnKeyDown := FKeyDown;
  FForm.OnKeyUp := FKeyUp;
  FForm.KeyPreview := FKeyPreview;
  FAssociate := nil;
  Mouse.Capture := 0;
  Visible := False;
end;

procedure TPopupForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Dismiss
  else if Key = VK_RETURN then
    Dismiss
  else if Key = VK_TAB then
    Dismiss;
  {else
    TDrawListHack(FList).KeyDown(Key, Shift);}
end;

procedure TPopupForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  //TDrawListHack(FList).KeyUp(Key, Shift);
end;

procedure TPopupForm.Paint;
var
  S: ISurface;
begin
  S := NewSurface(Canvas);
  StrokeRectColor(S, ClientRect, CurrentColor.Darken(0.5));
end;

end.

