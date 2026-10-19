(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.windows.explorer.bindingeditor.txt> }
unit Codebot.Windows.Explorer.BindingEditor;

{$i ../codebot/codebot.inc}

interface

uses
  Classes, SysUtils, Controls, Forms, StdCtrls, LCLType, ComponentEditors,
  Codebot.Windows.Explorer;

{ TExplorerBindingForm chooses which explorer views and trees of the owner of
  a TExplorerBinding are bound to it }

type
  TExplorerBindingForm = class(TForm)
    SourceLabel: TLabel;
    SourceList: TListBox;
    DestLabel: TLabel;
    DestList: TListBox;
    AddButton: TButton;
    AddAllButton: TButton;
    RemoveAllButton: TButton;
    RemoveButton: TButton;
    OKButton: TButton;
    CancelButton: TButton;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure AddButtonClick(Sender: TObject);
    procedure AddAllButtonClick(Sender: TObject);
    procedure RemoveAllButtonClick(Sender: TObject);
    procedure RemoveButtonClick(Sender: TObject);
  private
    procedure MoveItems(Source, Dest: TListBox; All: Boolean);
  end;

{ TExplorerBindingEditor opens the binding editor when a TExplorerBinding is
  double clicked in the form designer }

  TExplorerBindingEditor = class(TComponentEditor)
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

{ EditExplorerBinding shows the editor and returns True if the bound
  components were changed }

function EditExplorerBinding(Binding: TExplorerBinding): Boolean;

implementation

{$R *.lfm}

{ Only the new explorer controls can be bound }

function IsExplorerControl(C: TComponent): Boolean;
begin
  Result := (C is TExplorerView) or (C is TExplorerTree) or (C is TExplorerEdit) or
    (C is TExplorerPreview);
end;

function EditExplorerBinding(Binding: TExplorerBinding): Boolean;
var
  F: TExplorerBindingForm;
  O, C: TComponent;
  I: Integer;
begin
  Result := False;
  O := Binding.Owner;
  if O = nil then
    Exit;
  F := TExplorerBindingForm.Create(nil);
  try
    F.SourceList.Items.BeginUpdate;
    F.DestList.Items.BeginUpdate;
    try
      for I := 0 to O.ComponentCount - 1 do
      begin
        C := O.Components[I];
        if IsExplorerControl(C) and Binding.Accept(C) then
          if Binding.Contains(C) then
            F.DestList.Items.Add(C.Name)
          else
            F.SourceList.Items.Add(C.Name);
      end;
    finally
      F.DestList.Items.EndUpdate;
      F.SourceList.Items.EndUpdate;
    end;
    F.Caption := 'Editing: ' + Binding.Name;
    Result := F.ShowModal = mrOk;
    if Result then
    begin
      Binding.BeginUpdate;
      try
        Binding.Clear;
        for I := 0 to F.DestList.Items.Count - 1 do
        begin
          C := O.FindComponent(F.DestList.Items[I]);
          if IsExplorerControl(C) then
            Binding.Add(C);
        end;
      finally
        Binding.EndUpdate;
      end;
    end;
  finally
    F.Free;
  end;
end;

{ TExplorerBindingForm }

procedure TExplorerBindingForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    ModalResult := mrCancel;
end;

{ Move the selected or all items from one list to the other, keeping the
  destination sorted }

procedure TExplorerBindingForm.MoveItems(Source, Dest: TListBox; All: Boolean);
var
  S: TStringList;
  I: Integer;
begin
  S := TStringList.Create;
  try
    S.Sorted := True;
    S.AddStrings(Dest.Items);
    Source.Items.BeginUpdate;
    Dest.Items.BeginUpdate;
    try
      for I := Source.Items.Count - 1 downto 0 do
        if All or Source.Selected[I] then
        begin
          S.Add(Source.Items[I]);
          Source.Items.Delete(I);
        end;
      Dest.Items.Assign(S);
    finally
      Dest.Items.EndUpdate;
      Source.Items.EndUpdate;
    end;
  finally
    S.Free;
  end;
end;

procedure TExplorerBindingForm.AddButtonClick(Sender: TObject);
begin
  MoveItems(SourceList, DestList, False);
end;

procedure TExplorerBindingForm.AddAllButtonClick(Sender: TObject);
begin
  MoveItems(SourceList, DestList, True);
end;

procedure TExplorerBindingForm.RemoveAllButtonClick(Sender: TObject);
begin
  MoveItems(DestList, SourceList, True);
end;

procedure TExplorerBindingForm.RemoveButtonClick(Sender: TObject);
begin
  MoveItems(DestList, SourceList, False);
end;

{ TExplorerBindingEditor }

procedure TExplorerBindingEditor.Edit;
begin
  if (Component is TExplorerBinding) and
    EditExplorerBinding(TExplorerBinding(Component)) then
    Designer.Modified;
end;

procedure TExplorerBindingEditor.ExecuteVerb(Index: Integer);
begin
  Edit;
end;

function TExplorerBindingEditor.GetVerb(Index: Integer): string;
begin
  Result := 'Edit bindings ...';
end;

function TExplorerBindingEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.
