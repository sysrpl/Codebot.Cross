unit Main;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Codebot.System,
  Codebot.Debug,
  Codebot.Controls.Containers,
  Codebot.Controls.Buttons, Codebot.Graphics;

{ TMainForm }

type
  TMainForm = class(TForm)
    ImageStrip: TImageStrip;
    Memo: TMemo;
    ToolBar: TSizingPanel;
    RunButton: TThinButton;
    ClearButton: TThinButton;
    procedure ClearButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure RunButtonClick(Sender: TObject);
  private
    FDebugServer: TDebugServer;
    procedure HandleDebug(const Text: string);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FDebugServer := TDebugServer.Create;
  FDebugServer.Running := True;
  RunButton.ImageIndex := 1;
  FDebugServer.OnDebug.Add(HandleDebug);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FDebugServer.Free;
end;

procedure TMainForm.HandleDebug(const Text: string);
begin
  Memo.SelStart := 0;
  Memo.SelLength := 0;
  Memo.SelText := Text;
end;

procedure TMainForm.RunButtonClick(Sender: TObject);
begin
  FDebugServer.Running := not FDebugServer.Running;
  if FDebugServer.Running then
    RunButton.ImageIndex := 1
  else
    RunButton.ImageIndex := 0;
end;

procedure TMainForm.ClearButtonClick(Sender: TObject);
begin
  Memo.Lines.Clear;
end;

end.

