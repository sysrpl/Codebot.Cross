(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ Codebot.Themes.Picker defines the dialog which chooses the themes stored in
  a TThemeManager and the theme the program uses, with a preview of the theme
  selected }

{ <include docs/codebot.themes.picker.txt> }
unit Codebot.Themes.Picker;

{$mode delphi}

interface

uses
  Classes;

{ Shows the dialog with the themes in Themes checked and Theme chosen,
  returning true and the choice made if the user pressed OK }
function ShowThemePicker(Themes: TStrings; var Theme: string): Boolean;

implementation

uses
  SysUtils, Graphics, Forms, Controls, StdCtrls, ExtCtrls, CheckLst,
  Codebot.Themes.Palettes,
  Codebot.Themes.Dialog;

const
  SystemName = 'System Default';

{ TThemePickerDialog has no form file, so its controls are created in code.
  The list holds every theme but System Default, which a program always has,
  and the object of each item is its index in ThemeList. }

type
  TThemePickerDialog = class(TForm)
  private
    FList: TCheckListBox;
    FTheme: TComboBox;
    FPreview: TPaintBox;
    procedure FillThemes;
    procedure ListSelectionChange(Sender: TObject; User: Boolean);
    procedure ListClickCheck(Sender: TObject);
    procedure ThemeChange(Sender: TObject);
    procedure PreviewPaint(Sender: TObject);
  public
    constructor CreateDialog;
    procedure GetChoice(Themes: TStrings; out Theme: string);
    procedure SetChoice(Themes: TStrings; const Theme: string);
  end;

constructor TThemePickerDialog.CreateDialog;
var
  Panel: TPanel;
  Button: TButton;
  Text: TLabel;
  I: Integer;
begin
  inherited CreateNew(nil);
  Caption := 'Themes';
  BorderIcons := [biSystemMenu];
  Position := poMainFormCenter;
  ClientWidth := 680;
  ClientHeight := 440;
  Constraints.MinWidth := 500;
  Constraints.MinHeight := 320;
  { The buttons are aligned to the right of the panel, so that they stay in
    the corner whatever the width. Their left positions put them in the
    order OK and Cancel. }
  Panel := TPanel.Create(Self);
  Panel.Parent := Self;
  Panel.Align := alBottom;
  Panel.Height := 44;
  Panel.BevelOuter := bvNone;
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
  { The theme the program uses, chosen from the themes checked }
  Panel := TPanel.Create(Self);
  Panel.Parent := Self;
  Panel.Align := alTop;
  Panel.Height := 40;
  Panel.BevelOuter := bvNone;
  Text := TLabel.Create(Self);
  Text.Parent := Panel;
  Text.Caption := 'Theme';
  Text.Layout := tlCenter;
  Text.Align := alLeft;
  Text.BorderSpacing.Left := 8;
  Text.BorderSpacing.Right := 6;
  FTheme := TComboBox.Create(Self);
  FTheme.Parent := Panel;
  FTheme.Style := csDropDownList;
  FTheme.Width := 220;
  FTheme.Left := 1000;
  FTheme.Align := alLeft;
  FTheme.BorderSpacing.Top := 8;
  FTheme.BorderSpacing.Bottom := 8;
  FTheme.OnChange := ThemeChange;
  { The themes, checked when they are stored in the form }
  FList := TCheckListBox.Create(Self);
  FList.Parent := Self;
  FList.Align := alLeft;
  FList.Width := 180;
  FList.BorderSpacing.Left := 8;
  FList.BorderSpacing.Bottom := 8;
  for I := Low(ThemeList) to High(ThemeList) do
    if not SameText(ThemeList[I].Name, SystemName) then
      FList.Items.AddObject(ThemeList[I].Name, TObject(PtrInt(I)));
  FList.OnSelectionChange := ListSelectionChange;
  FList.OnClickCheck := ListClickCheck;
  FPreview := TPaintBox.Create(Self);
  FPreview.Parent := Self;
  FPreview.Align := alClient;
  FPreview.BorderSpacing.Left := 8;
  FPreview.BorderSpacing.Right := 8;
  FPreview.BorderSpacing.Bottom := 8;
  FPreview.OnPaint := PreviewPaint;
end;

{ The themes the program can use are System Default and the themes checked.
  The theme chosen stays chosen while it is still one of them. }

procedure TThemePickerDialog.FillThemes;
var
  Theme: string;
  I: Integer;
begin
  if FTheme.ItemIndex >= 0 then
    Theme := FTheme.Items[FTheme.ItemIndex]
  else
    Theme := SystemName;
  FTheme.Items.BeginUpdate;
  try
    FTheme.Items.Clear;
    FTheme.Items.Add(SystemName);
    for I := 0 to FList.Count - 1 do
      if FList.Checked[I] then
        FTheme.Items.Add(FList.Items[I]);
  finally
    FTheme.Items.EndUpdate;
  end;
  FTheme.ItemIndex := FTheme.Items.IndexOf(Theme);
  if FTheme.ItemIndex < 0 then
    FTheme.ItemIndex := 0;
end;

procedure TThemePickerDialog.GetChoice(Themes: TStrings; out Theme: string);
var
  I: Integer;
begin
  Themes.BeginUpdate;
  try
    Themes.Clear;
    for I := 0 to FList.Count - 1 do
      if FList.Checked[I] then
        Themes.Add(FList.Items[I]);
  finally
    Themes.EndUpdate;
  end;
  if FTheme.ItemIndex > 0 then
    Theme := FTheme.Items[FTheme.ItemIndex]
  else
    Theme := '';
end;

{ The theme chosen is checked, as the program can only use a theme stored in
  the form. A theme stored in the form which the IDE does not have is listed
  without a preview, so that it stays stored. The theme chosen is selected
  so that its preview is shown. }

procedure TThemePickerDialog.SetChoice(Themes: TStrings; const Theme: string);
var
  I: Integer;
begin
  for I := 0 to Themes.Count - 1 do
    if (Themes[I] <> '') and (FList.Items.IndexOf(Themes[I]) < 0) then
      FList.Items.AddObject(Themes[I], TObject(PtrInt(-1)));
  for I := 0 to FList.Count - 1 do
    FList.Checked[I] := (Themes.IndexOf(FList.Items[I]) >= 0) or
      SameText(FList.Items[I], Theme);
  FTheme.ItemIndex := -1;
  FillThemes;
  I := FTheme.Items.IndexOf(Theme);
  if I > 0 then
    FTheme.ItemIndex := I;
  FList.ItemIndex := FList.Items.IndexOf(Theme);
  if (FList.ItemIndex < 0) and (FList.Count > 0) then
    FList.ItemIndex := 0;
end;

procedure TThemePickerDialog.ListSelectionChange(Sender: TObject; User: Boolean);
begin
  FPreview.Invalidate;
end;

procedure TThemePickerDialog.ListClickCheck(Sender: TObject);
begin
  FillThemes;
end;

{ Choosing a theme shows its preview }

procedure TThemePickerDialog.ThemeChange(Sender: TObject);
var
  I: Integer;
begin
  if FTheme.ItemIndex < 0 then
    Exit;
  I := FList.Items.IndexOf(FTheme.Items[FTheme.ItemIndex]);
  if I >= 0 then
    FList.ItemIndex := I;
end;

procedure TThemePickerDialog.PreviewPaint(Sender: TObject);
begin
  if FList.ItemIndex >= 0 then
    ThemePaintPreview(FPreview.Canvas, FPreview.ClientRect,
      PtrInt(FList.Items.Objects[FList.ItemIndex]), 'Form1');
end;

function ShowThemePicker(Themes: TStrings; var Theme: string): Boolean;
var
  Dialog: TThemePickerDialog;
begin
  Dialog := TThemePickerDialog.CreateDialog;
  try
    Dialog.SetChoice(Themes, Theme);
    Result := Dialog.ShowModal = mrOK;
    if Result then
      Dialog.GetChoice(Themes, Theme);
  finally
    Dialog.Free;
  end;
end;

end.
