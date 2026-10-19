unit Main;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, ComCtrls, LCLType,
  Codebot.Windows.Explorer;

{ TMainForm shows the explorer controls working together. The tree, the
  path edit, the view, and the preview are linked by a TExplorerBinding, so
  moving to a folder in any of them moves the others, and the preview shows
  the item selected in the view }

type
  TMainForm = class(TForm)
    BackButton: TButton;
    Binding: TExplorerBinding;
    ForwardButton: TButton;
    PathEdit: TExplorerEdit;
    Preview: TExplorerPreview;
    PreviewSplitter: TSplitter;
    SearchEdit: TEdit;
    StatusBar: TStatusBar;
    ToolPanel: TPanel;
    Tree: TExplorerTree;
    TreeSplitter: TSplitter;
    UpButton: TButton;
    View: TExplorerView;
    procedure FormCreate(Sender: TObject);
    procedure BackButtonClick(Sender: TObject);
    procedure ForwardButtonClick(Sender: TObject);
    procedure UpButtonClick(Sender: TObject);
    procedure SearchEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ViewNavigated(Sender: TObject);
    procedure ViewSelectionChanged(Sender: TObject);
  private
    procedure UpdateStatus;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  UpdateStatus;
end;

procedure TMainForm.BackButtonClick(Sender: TObject);
begin
  View.GoBack;
end;

procedure TMainForm.ForwardButtonClick(Sender: TObject);
begin
  View.GoForward;
end;

procedure TMainForm.UpButtonClick(Sender: TObject);
begin
  View.GoUp;
end;

procedure TMainForm.SearchEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_RETURN:
      begin
        View.Search(SearchEdit.Text);
        Key := 0;
      end;
    VK_ESCAPE:
      begin
        { An empty search returns to the folder searched }
        SearchEdit.Text := '';
        View.Search('');
        Key := 0;
      end;
  end;
end;

procedure TMainForm.ViewNavigated(Sender: TObject);
begin
  if not View.Searching then
    SearchEdit.Text := '';
  UpdateStatus;
end;

procedure TMainForm.ViewSelectionChanged(Sender: TObject);
begin
  UpdateStatus;
end;

procedure TMainForm.UpdateStatus;
begin
  BackButton.Enabled := View.CanGoBack;
  ForwardButton.Enabled := View.CanGoForward;
  if View.SelectedCount > 0 then
    StatusBar.SimpleText := Format('%d items, %d selected',
      [View.ItemCount, View.SelectedCount])
  else
    StatusBar.SimpleText := Format('%d items', [View.ItemCount]);
end;

end.
