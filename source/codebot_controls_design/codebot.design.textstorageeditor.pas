(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

unit Codebot.Design.TextStorageEditor;

{$mode delphi}

interface

uses
  Classes, SysUtils, Controls, Forms, StdCtrls, Dialogs, LCLType,
  Codebot.System,
  Codebot.Text.Store,
  Codebot.Controls.Banner;

{ TTextStorageEditor edits a copy of the named items in a TTextStorage }

type
  TTextStorageEditor = class(TBannerForm)
    NameList: TListBox;
    AddButton: TButton;
    DeleteButton: TButton;
    NameLabel: TLabel;
    NameEdit: TEdit;
    TextMemo: TMemo;
    OKButton: TButton;
    CancelButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure NameListSelectionChange(Sender: TObject; User: Boolean);
    procedure NameEditChange(Sender: TObject);
    procedure TextMemoChange(Sender: TObject);
    procedure AddButtonClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
  private
    FItems: TTextItems;
    FUpdating: Boolean;
    function Selected: TTextItem;
    procedure LoadList(Index: Integer);
    procedure LoadSelected;
    function Validate: Boolean;
  end;

{ EditTextStorage shows the editor and returns True if the items were changed }

function EditTextStorage(Storage: TTextStorage): Boolean;

implementation

{$R *.lfm}

function EditTextStorage(Storage: TTextStorage): Boolean;
var
  F: TTextStorageEditor;
begin
  F := TTextStorageEditor.Create(nil);
  try
    F.FItems.Assign(Storage.Items);
    F.Caption := 'Editing: ' + StrCompPath(Storage);
    F.LoadList(0);
    Result := F.ShowModal = mrOk;
    if Result then
      Storage.Items.Assign(F.FItems);
  finally
    F.Free;
  end;
end;

{ TTextStorageEditor }

procedure TTextStorageEditor.FormCreate(Sender: TObject);
begin
  FItems := TTextItems.Create(nil);
  {$ifdef windows}
  TextMemo.Font.Name := 'Consolas';
  {$else}
  TextMemo.Font.Name := 'Monospace';
  {$endif}
end;

procedure TTextStorageEditor.FormDestroy(Sender: TObject);
begin
  FItems.Free;
end;

procedure TTextStorageEditor.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if ModalResult = mrOk then
    CanClose := Validate;
end;

procedure TTextStorageEditor.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    ModalResult := mrCancel;
end;

function TTextStorageEditor.Selected: TTextItem;
begin
  if NameList.ItemIndex < 0 then
    Result := nil
  else
    Result := FItems[NameList.ItemIndex];
end;

{ Fill the list with the item names and select an item by index }

procedure TTextStorageEditor.LoadList(Index: Integer);
var
  I: Integer;
begin
  NameList.Items.BeginUpdate;
  try
    NameList.Items.Clear;
    for I := 0 to FItems.Count - 1 do
      NameList.Items.Add(FItems[I].Name);
  finally
    NameList.Items.EndUpdate;
  end;
  if Index > FItems.Count - 1 then
    Index := FItems.Count - 1;
  NameList.ItemIndex := Index;
  LoadSelected;
end;

{ Show the name and text of the selected item }

procedure TTextStorageEditor.LoadSelected;
var
  Item: TTextItem;
begin
  Item := Selected;
  FUpdating := True;
  try
    if Item = nil then
    begin
      NameEdit.Text := '';
      TextMemo.Lines.Clear;
    end
    else
    begin
      NameEdit.Text := Item.Name;
      TextMemo.Lines.Assign(Item.Text);
    end;
  finally
    FUpdating := False;
  end;
  NameEdit.Enabled := Item <> nil;
  TextMemo.Enabled := Item <> nil;
  DeleteButton.Enabled := Item <> nil;
end;

{ Names must be unique and not blank so they can be found using Values }

function TTextStorageEditor.Validate: Boolean;
var
  I, J: Integer;
  S: string;
begin
  for I := 0 to FItems.Count - 1 do
  begin
    S := FItems[I].Name.Trim;
    if S = '' then
    begin
      NameList.ItemIndex := I;
      LoadSelected;
      MessageDlg('Every item must have a name', mtError, [mbOK], 0);
      NameEdit.SetFocus;
      Exit(False);
    end;
    for J := 0 to I - 1 do
      if SameText(FItems[J].Name.Trim, S) then
      begin
        NameList.ItemIndex := I;
        LoadSelected;
        MessageDlg('More than one item is named ''' + S + '''', mtError, [mbOK], 0);
        NameEdit.SetFocus;
        Exit(False);
      end;
  end;
  Result := True;
end;

procedure TTextStorageEditor.NameListSelectionChange(Sender: TObject; User: Boolean);
begin
  LoadSelected;
end;

procedure TTextStorageEditor.NameEditChange(Sender: TObject);
var
  Item: TTextItem;
begin
  if FUpdating then
    Exit;
  Item := Selected;
  if Item = nil then
    Exit;
  Item.Name := NameEdit.Text;
  NameList.Items[NameList.ItemIndex] := Item.Name;
end;

procedure TTextStorageEditor.TextMemoChange(Sender: TObject);
var
  Item: TTextItem;
begin
  if FUpdating then
    Exit;
  Item := Selected;
  if Item <> nil then
    Item.Text.Assign(TextMemo.Lines);
end;

procedure TTextStorageEditor.AddButtonClick(Sender: TObject);
var
  Item: TTextItem;
  S: string;
  I: Integer;
begin
  I := FItems.Count + 1;
  repeat
    S := 'item' + IntToStr(I);
    Inc(I);
  until FItems.Find(S) = nil;
  Item := FItems.Add;
  Item.Name := S;
  LoadList(Item.Index);
  NameEdit.SetFocus;
  NameEdit.SelectAll;
end;

procedure TTextStorageEditor.DeleteButtonClick(Sender: TObject);
var
  I: Integer;
begin
  I := NameList.ItemIndex;
  if I < 0 then
    Exit;
  FItems.Delete(I);
  LoadList(I);
end;

end.
