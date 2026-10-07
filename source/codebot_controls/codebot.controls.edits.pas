(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.edits.txt> }
unit Codebot.Controls.Edits;

{$i ../codebot/codebot.inc}

interface

uses
  Classes, SysUtils, Graphics, Controls, StdCtrls, Forms, LCLType,
  Codebot.System,
  Codebot.Graphics,
  Codebot.Graphics.Types,
  Codebot.Controls,
  Codebot.Controls.Sliders,
  Codebot.Forms.Popup;

{ TCustomRenderEdit is an unfinished edit control drawn using a surface }

type
  TCustomRenderEdit = class(TSurfaceCustomControl)
  protected
    { Draw the edit }
    procedure Draw; override;
  public
    { Create a new render edit }
    constructor Create(AOwner: TComponent); override;
  end;

{ TPopupSlideForm is the popup holding the slide bar of a TCustomSlideEdit }

  TPopupSlideForm = class(TPopupForm)
  private
    FSlide: TSlideBar;
  public
    { Create a popup with a horizontal slide bar }
    constructor Create(AOwner: TComponent); override;
    { The slide bar shown in the popup }
    property Slide: TSlideBar read FSlide;
  end;

{ TCustomSlideEdit is an edit for a number between Min and Max with a spin
  button on its right. Pressing the button pops up a slide bar under the
  mouse, and dragging while the button is held moves the slide bar. The up
  and down arrow keys change the value by Step. Text typed in the edit is
  converted to a number, and the text is formatted when the edit loses focus.
  See also
  <link Overview.Codebot.Controls.Edits.TCustomSlideEdit, TCustomSlideEdit members> }

  TCustomSlideEdit = class(TSurfaceCustomControl)
  private
    FEdit: TEdit;
    FPopup: TPopupSlideForm;
    FPopped: Boolean;
    FJustPopped: Boolean;
    FChanging: Boolean;
    FAutoHeight: Boolean;
    FButtonDown: Boolean;
    FButtonHot: Boolean;
    FAdjusting: Boolean;
    FOnFormat: TFormatTextEvent;
    FOnValueChange: TNotifyEvent;
    function PopupValid: Boolean;
    function GetMin: Double;
    procedure SetMin(const Value: Double);
    function GetMax: Double;
    procedure SetMax(const Value: Double);
    function GetPosition: Double;
    procedure SetPosition(const Value: Double);
    function GetStep: Double;
    procedure SetStep(const Value: Double);
    function GetOnDrawBackground: TDrawStateEvent;
    procedure SetOnDrawBackground(Value: TDrawStateEvent);
    function GetOnDrawThumb: TDrawStateEvent;
    procedure SetOnDrawThumb(Value: TDrawStateEvent);
    procedure SetAutoHeight(Value: Boolean);
    procedure SetButtonHot(Value: Boolean);
    procedure EditChange(Sender: TObject);
    procedure EditResize(Sender: TObject);
    procedure EditExit(Sender: TObject);
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SlideChange(Sender: TObject);
  protected
    { Return the text of the inner edit }
    function RealGetText: TCaption; override;
    { Set the text of the inner edit }
    procedure RealSetText(const Value: TCaption); override;
    procedure CreateWnd; override;
    procedure FontChanged(Sender: TObject); override;
    procedure Loaded; override;
    procedure Resize; override;
    { The height the inner edit needs to show its text }
    function EditHeight: Integer;
    { Extra height reserved below the inner edit by descendants }
    function ExtraHeight: Integer; virtual;
    { The bounds of the spin button }
    function GetButtonRect: TRectI; virtual;
    { The bounds of the inner edit }
    function GetEditRect: TRectI; virtual;
    { Position the inner edit inside the edit rect }
    procedure AdjustEdit; virtual;
    { Set the height to fit the font when AutoHeight is true }
    procedure AdjustHeight; virtual;
    { Pop up the slide bar under the mouse }
    procedure DoButtonPress; virtual;
    { Convert the text typed by the user to a position }
    procedure DoChange; virtual;
    { Return the text shown for a position, invoking OnFormat }
    function DoFormat(const Value: Double): string; virtual;
    { Invoke OnValueChange }
    procedure DoValueChange; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseLeave; override;
    procedure Draw; override;
    { The edit where text is typed }
    property InnerEdit: TEdit read FEdit;
    { The popup holding the slide bar }
    property Popup: TPopupSlideForm read FPopup;
    { Popped is true while the slide bar is shown }
    property Popped: Boolean read FPopped;
    { ButtonDown is true while the spin button is pressed }
    property ButtonDown: Boolean read FButtonDown;
    { ButtonHot is true while the mouse is over the spin button }
    property ButtonHot: Boolean read FButtonHot;
    { When true the height follows the font }
    property AutoHeight: Boolean read FAutoHeight write SetAutoHeight default True;
    { The smallest value }
    property Min: Double read GetMin write SetMin;
    { The largest value }
    property Max: Double read GetMax write SetMax;
    { The current value }
    property Position: Double read GetPosition write SetPosition;
    { The amount the arrow keys change the value, and when greater than zero
      the value snaps to multiples of step }
    property Step: Double read GetStep write SetStep;
    { OnDrawBackground allows custom drawing of the popup slide bar track }
    property OnDrawBackground: TDrawStateEvent read GetOnDrawBackground write SetOnDrawBackground;
    { OnDrawThumb allows custom drawing of the popup slide bar thumb }
    property OnDrawThumb: TDrawStateEvent read GetOnDrawThumb write SetOnDrawThumb;
    { OnFormat allows the text shown for a value to be changed }
    property OnFormat: TFormatTextEvent read FOnFormat write FOnFormat;
    { OnValueChange is invoked when the position changes }
    property OnValueChange: TNotifyEvent read FOnValueChange write FOnValueChange;
  public
    { Create a new slide edit }
    constructor Create(AOwner: TComponent); override;
    { The text in the edit }
    property Text;
  end;

{ TSlideEdit publishes the properties of TCustomSlideEdit }

  TSlideEdit = class(TCustomSlideEdit)
  published
    property AutoHeight;
    property Min;
    property Max;
    property Position;
    property Step;
    property OnDrawBackground;
    property OnDrawThumb;
    property OnFormat;
    property OnValueChange;
    property Align;
    property Anchors;
    property BorderSpacing;
    property Constraints;
    property Enabled;
    property Font;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property Visible;
    property OnEnter;
    property OnExit;
    property OnResize;
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

procedure TCustomRenderEdit.Draw;
begin
end;

{ TPopupSlideForm }

const
  SlideMargin = 4;
  SlidePopupWidth = 150;
  SlidePopupHeight = 30;
  SpinButtonWidth = 17;
  EditMargin = 3;

constructor TPopupSlideForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := SlidePopupWidth;
  Height := SlidePopupHeight;
  FSlide := TSlideBar.Create(Self);
  FSlide.Kind := sbHorizontal;
  FSlide.Max := 1000;
  FSlide.SetBounds(SlideMargin, SlideMargin, SlidePopupWidth - SlideMargin * 2,
    SlidePopupHeight - SlideMargin * 2);
  FSlide.Anchors := [akLeft, akTop, akRight, akBottom];
  FSlide.Parent := Self;
end;

{ TCustomSlideEdit }

constructor TCustomSlideEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption];
  FAutoHeight := True;
  FPopup := TPopupSlideForm.Create(Self);
  FPopup.Slide.OnChange := SlideChange;
  FEdit := TEdit.Create(Self);
  FEdit.BorderStyle := bsNone;
  FEdit.ParentFont := True;
  FEdit.Color := clWindow;
  FEdit.OnChange := EditChange;
  FEdit.OnResize := EditResize;
  FEdit.OnExit := EditExit;
  FEdit.OnKeyDown := EditKeyDown;
  FEdit.Parent := Self;
  Width := 80;
  Height := 26;
  FChanging := True;
  FEdit.Text := DoFormat(Position);
  FChanging := False;
end;

function TCustomSlideEdit.PopupValid: Boolean;
begin
  Result := FPopup.Slide.Max > FPopup.Slide.Min;
end;

function TCustomSlideEdit.RealGetText: TCaption;
begin
  if FEdit = nil then
    Result := ''
  else
    Result := FEdit.Text;
end;

procedure TCustomSlideEdit.RealSetText(const Value: TCaption);
begin
  if FEdit <> nil then
    FEdit.Text := Value;
end;

procedure TCustomSlideEdit.CreateWnd;
begin
  inherited CreateWnd;
  { The inner edit knows its real height once the window exists }
  AdjustHeight;
end;

procedure TCustomSlideEdit.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);
  AdjustHeight;
end;

procedure TCustomSlideEdit.Loaded;
begin
  inherited Loaded;
  FChanging := True;
  FEdit.Text := DoFormat(Position);
  FChanging := False;
  AdjustHeight;
end;

procedure TCustomSlideEdit.Resize;
begin
  inherited Resize;
  AdjustEdit;
end;

function TCustomSlideEdit.GetButtonRect: TRectI;
begin
  Result := ClientRect;
  Result.Left := Result.Right - SpinButtonWidth - 1;
  Result.Inflate(0, -1);
  Result.Width := SpinButtonWidth;
end;

function TCustomSlideEdit.GetEditRect: TRectI;
begin
  Result := ClientRect;
  Result.Inflate(-EditMargin, -EditMargin);
  Result.Right := GetButtonRect.Left - 1;
end;

function TCustomSlideEdit.EditHeight: Integer;
begin
  { The inner edit sizes its own height to fit its text, which on some
    widgetsets such as gtk3 is much taller than the font. Until its window
    exists use the text height. }
  if (FEdit <> nil) and FEdit.HandleAllocated then
    Result := FEdit.Height
  else
    Result := TextHeight + 4;
end;

procedure TCustomSlideEdit.AdjustEdit;
var
  R: TRectI;
  H: Integer;
begin
  if FEdit = nil then
    Exit;
  R := GetEditRect;
  { Keep the height the inner edit chose and center it vertically }
  H := FEdit.Height;
  if H < R.Height then
    R.Y := R.Y + (R.Height - H) div 2;
  FEdit.SetBounds(R.X, R.Y, R.Width, H);
end;

procedure TCustomSlideEdit.AdjustHeight;
begin
  if FAdjusting then
    Exit;
  FAdjusting := True;
  try
    if FAutoHeight and (not (csLoading in ComponentState)) then
      Height := EditHeight + EditMargin * 2 + ExtraHeight;
    AdjustEdit;
  finally
    FAdjusting := False;
  end;
end;

function TCustomSlideEdit.ExtraHeight: Integer;
begin
  Result := 0;
end;

procedure TCustomSlideEdit.EditResize(Sender: TObject);
begin
  { Grow or shrink to fit when the inner edit changes its own height }
  AdjustHeight;
end;

procedure TCustomSlideEdit.DoButtonPress;
var
  P: TPointI;
  Thumb, A: Float;
begin
  if not PopupValid then
    Exit;
  Thumb := Theme.MeasureThumbThin(toHorizontal).X;
  P := Mouse.CursorPos;
  { Place the popup so the thumb is under the mouse }
  A := (FPopup.Slide.Position - FPopup.Slide.Min) / (FPopup.Slide.Max - FPopup.Slide.Min) *
    (FPopup.Slide.Width - Thumb) + Thumb / 2 + SlideMargin;
  FJustPopped := True;
  FPopup.Popup(Self);
  FPopup.SetBounds(P.X - Round(A), ClientToScreen(TPointI.Create(0, Height)).Y,
    SlidePopupWidth, SlidePopupHeight);
  FPopped := FPopup.Visible;
  { Showing the popup may take the mouse capture, so take it back to keep
    receiving mouse moves while the button is held }
  MouseCapture := True;
end;

procedure TCustomSlideEdit.DoChange;
var
  S: string;
begin
  if FChanging or FPopped then
    Exit;
  S := Trim(FEdit.Text);
  if (S = '') or (S = '.') or (S = '-') then
    Exit;
  FChanging := True;
  try
    Position := StrToFloatDef(S, Position);
  finally
    FChanging := False;
  end;
end;

function TCustomSlideEdit.DoFormat(const Value: Double): string;
var
  I: Integer;
begin
  { Show up to two decimals without trailing zeros }
  Result := Format('%.2f', [Value]);
  I := Length(Result);
  while (I > 0) and (Result[I] = '0') do
    Dec(I);
  if (I > 0) and (Result[I] = DefaultFormatSettings.DecimalSeparator) then
    Dec(I);
  SetLength(Result, I);
  if Assigned(FOnFormat) then
    FOnFormat(Self, Result);
end;

procedure TCustomSlideEdit.DoValueChange;
begin
  if Assigned(FOnValueChange) then
    FOnValueChange(Self);
end;

procedure TCustomSlideEdit.SlideChange(Sender: TObject);
begin
  { Show the new value unless it came from the text being typed }
  if not FChanging then
  begin
    FChanging := True;
    try
      FEdit.Text := DoFormat(Position);
    finally
      FChanging := False;
    end;
  end;
  DoValueChange;
end;

procedure TCustomSlideEdit.EditChange(Sender: TObject);
begin
  DoChange;
end;

procedure TCustomSlideEdit.EditExit(Sender: TObject);
begin
  FChanging := True;
  try
    FEdit.Text := DoFormat(Position);
  finally
    FChanging := False;
  end;
end;

procedure TCustomSlideEdit.EditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_UP then
  begin
    Position := Position + Step;
    FEdit.SelStart := Length(FEdit.Text);
    Key := 0;
  end
  else if Key = VK_DOWN then
  begin
    Position := Position - Step;
    FEdit.SelStart := Length(FEdit.Text);
    Key := 0;
  end;
end;

procedure TCustomSlideEdit.SetButtonHot(Value: Boolean);
begin
  if FButtonHot = Value then Exit;
  FButtonHot := Value;
  Invalidate;
end;

procedure TCustomSlideEdit.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and Enabled and GetButtonRect.Contains(X, Y) then
  begin
    FButtonDown := True;
    Invalidate;
    DoButtonPress;
  end;
end;

procedure TCustomSlideEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TPointI;
  Thumb, R: Float;
begin
  inherited MouseMove(Shift, X, Y);
  SetButtonHot(GetButtonRect.Contains(X, Y));
  if FPopped and PopupValid then
  begin
    if FJustPopped then
    begin
      FJustPopped := False;
      Exit;
    end;
    Thumb := Theme.MeasureThumbThin(toHorizontal).X;
    P := ClientToScreen(TPointI.Create(X, Y));
    P := FPopup.Slide.ScreenToClient(P);
    R := (P.X - Thumb / 2) / (FPopup.Slide.Width - Thumb) *
      (FPopup.Slide.Max - FPopup.Slide.Min) + FPopup.Slide.Min;
    FPopup.Slide.Position := R;
  end;
end;

procedure TCustomSlideEdit.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    if FPopped then
    begin
      FPopup.Dismiss;
      FPopped := False;
    end;
    if FButtonDown then
    begin
      FButtonDown := False;
      Invalidate;
    end;
  end;
end;

procedure TCustomSlideEdit.MouseLeave;
begin
  inherited MouseLeave;
  SetButtonHot(False);
end;

procedure TCustomSlideEdit.Draw;
var
  R: TRectI;
  C: TColorB;
  X, Y: Float;
begin
  R := ClientRect;
  FillRectColor(Surface, R, clWindow);
  C := clWindowText;
  StrokeRectColor(Surface, R, C.Fade(0.35));
  R := GetButtonRect;
  if FButtonDown then
    FillRectColor(Surface, R, C.Fade(0.2))
  else if FButtonHot then
    FillRectColor(Surface, R, C.Fade(0.1));
  if not Enabled then
    C := clGrayText;
  { Draw a horizontal spin glyph of a left and right arrow }
  X := R.X + R.Width / 2;
  Y := R.Y + R.Height / 2;
  Surface.MoveTo(X - 1, Y - 4);
  Surface.LineTo(X - 5, Y);
  Surface.LineTo(X - 1, Y + 4);
  Surface.Path.Close;
  Surface.MoveTo(X + 1, Y - 4);
  Surface.LineTo(X + 5, Y);
  Surface.LineTo(X + 1, Y + 4);
  Surface.Path.Close;
  Surface.Fill(NewBrush(C));
end;

function TCustomSlideEdit.GetMin: Double;
begin
  Result := FPopup.Slide.Min;
end;

procedure TCustomSlideEdit.SetMin(const Value: Double);
begin
  FPopup.Slide.Min := Value;
end;

function TCustomSlideEdit.GetMax: Double;
begin
  Result := FPopup.Slide.Max;
end;

procedure TCustomSlideEdit.SetMax(const Value: Double);
begin
  FPopup.Slide.Max := Value;
end;

function TCustomSlideEdit.GetPosition: Double;
begin
  Result := FPopup.Slide.Position;
end;

procedure TCustomSlideEdit.SetPosition(const Value: Double);
begin
  FPopup.Slide.Position := Value;
end;

function TCustomSlideEdit.GetStep: Double;
begin
  Result := FPopup.Slide.Step;
end;

procedure TCustomSlideEdit.SetStep(const Value: Double);
begin
  FPopup.Slide.Step := Value;
end;

function TCustomSlideEdit.GetOnDrawBackground: TDrawStateEvent;
begin
  Result := FPopup.Slide.OnDrawBackground;
end;

procedure TCustomSlideEdit.SetOnDrawBackground(Value: TDrawStateEvent);
begin
  FPopup.Slide.OnDrawBackground := Value;
end;

function TCustomSlideEdit.GetOnDrawThumb: TDrawStateEvent;
begin
  Result := FPopup.Slide.OnDrawThumb;
end;

procedure TCustomSlideEdit.SetOnDrawThumb(Value: TDrawStateEvent);
begin
  FPopup.Slide.OnDrawThumb := Value;
end;

procedure TCustomSlideEdit.SetAutoHeight(Value: Boolean);
begin
  if FAutoHeight = Value then Exit;
  FAutoHeight := Value;
  AdjustHeight;
end;

end.
