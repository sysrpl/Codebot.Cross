(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified March 2015                                 *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.edits.txt> }
unit Codebot.Controls.Edits;

{$i ../codebot/codebot.inc}

interface

uses
  Classes, SysUtils, Graphics, Controls, StdCtrls,
  Codebot.System,
  Codebot.Graphics,
  Codebot.Graphics.Types,
  Codebot.Controls;

{ TCustomRenderEdit }

type
  TCustomRenderEdit = class(TRenderCustomControl)
  protected
    procedure Render; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;


{ TCustomSlideEdit }

  TSlideMode = (smByte, smFloat);

  TCustomSlideEdit = class(TCustomEdit)
  private
    type TSlider = class(TRenderGraphicControl);
    procedure DrawSlider(Sender: TObject; Surface: ISurface);
    procedure SliderMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SliderMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure SliderMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FMixColor: TColor;
    FSlider: TSlider;
    FSlideMode: TSlideMode;
    FDown: Boolean;
    FPosition: Single;
    FOnChange: TNotifyEvent;
    procedure SetMixColor(Value: TColor);
    procedure SetPosition(Value: Single);
    procedure SetSlideMode(Value: TSlideMode);
  protected
    procedure BoundsChanged; override;
    procedure SetParent(NewParent: TWinControl); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Mode: TSlideMode read FSlideMode write SetSlideMode;
    property MixColor: TColor read FMixColor write SetMixColor;
    property Position: Single read FPosition write SetPosition;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;


  { THotShiftState }

  {THotkeyName = type string;
  THotkeyValue = type Word;

  THotkeyModifiers = set of (ssShift, ssAlt, ssCtrl, ssSuper);

  THotkey = class(TComponent)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure Apply;
    procedure Cancel;
    property Valid: Boolean read GetValid;
    property Editing: Boolean read GetEditing;
    property KeyValue: THotkeyValue read GetValue write SetValue;
  published
    property AssociateEdit:
    property Active: Boolean read FActive write SetActive;
    property Editing: Boolean read FEditing write SetEditing;
    property KeyName: THotkeyKey string read GetKeyName write SetKeyName;
    property Modifiers: THotkeyModifiers read GetModifiers write SetModifiers;
    property OnExecute: TNotifyEvent read FOnExecute write FOnExecute;
  end;}

implementation

{ TCustomRenderEdit }

constructor TCustomRenderEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color := clWhite;
  Width := 100;
  Height := TextHeight + 8;
end;

procedure TCustomRenderEdit.Render;
begin
end;

{ TCustomSlideEdit }



constructor TCustomSlideEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSlider := TSlider.Create(Self);
  FSlider.OnRender := DrawSlider;
  FSlider.OnMouseDown := SliderMouseDown;
  FSlider.OnMouseMove := SliderMouseMove;
  FSlider.OnMouseUp := SliderMouseUp;
end;

procedure TCustomSlideEdit.DrawSlider(Sender: TObject; Surface: ISurface);
var
  R: TRectI;
  B: ILinearGradientBrush;
begin
  R := FSlider.ClientRect;
  B := NewBrush(R.TopLeft, R.TopRight);
  B.AddStop(clBlack, 0);
  B.AddStop(FMixColor, 1);
  Surface.Rectangle(R);
  Surface.Fill(B, True);
  Surface.Stroke(NewPen(clBlack));
  R.Left := Round(FPosition * R.Width);
  if R.Left < R.Right - 2 then
    R.Right := R.Left + 2
  else
    R.Left := R.Right - 2;
  Surface.FillRect(NewBrush(clWhite), R);
end;

procedure TCustomSlideEdit.SliderMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FDown := Button = mbLeft;
  SliderMouseMove(Sender, Shift, X, Y);
end;

procedure TCustomSlideEdit.SliderMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  R: TRectF;
  P: Single;
begin
  if FDown then
  begin
    R := FSlider.BoundsRect;
    if X < 0 then
      P := 0
    else if X > R.Width then
      P := 1
    else
      P := X / R.Width;
    Position := P;
  end;
end;

procedure TCustomSlideEdit.SliderMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    FDown := False;
end;

procedure TCustomSlideEdit.SetMixColor(Value: TColor);
begin
  if FMixColor = Value then
    Exit;
  FMixColor := Value;
  FSlider.Invalidate;
end;

procedure TCustomSlideEdit.SetPosition(Value: Single);
begin
  if FPosition = Value then
    Exit;
  FPosition := Value;
  if FSlideMode = smByte then
    Text := Format('%d', [Round(FPosition * $FF)])
  else
    Text := Format('%.3f', [FPosition]);
  FSlider.Invalidate;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TCustomSlideEdit.SetSlideMode(Value: TSlideMode);
begin
  if FSlideMode = Value then
    Exit;
  FSlideMode := Value;
  if FSlideMode = smByte then
    Text := Format('%d', [Round(FPosition * $FF)])
  else
    Text := Format('%.3f', [FPosition]);
end;

procedure TCustomSlideEdit.BoundsChanged;
var
  R: TRectI;
begin
  if not HandleAllocated then
    Exit;
  R := BoundsRect;
  R.Y := R.Bottom + 8;
  R.Height := 10;
  FSlider.BoundsRect := R;
  inherited BoundsChanged;
end;

procedure TCustomSlideEdit.SetParent(NewParent: TWinControl);
begin
  inherited SetParent(NewParent);
  FSlider.Parent := NewParent;
end;

end.

