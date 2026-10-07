(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ Codebot.Terminal.EmulatorDialog is a window of the IDE which holds a
  terminal. The shell is started again whenever it ends. }

unit Codebot.Terminal.EmulatorDialog;

{$i ../codebot_terminal/terminal.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  Codebot.Terminal.Controls;

{ TTerminalEmulatorDlg }

type
  TTerminalEmulatorDlg = class(TForm)
    Terminal: TTerminal;
    procedure TerminalTerminate(Sender: TObject);
  end;

procedure InitTerminalEmulatorDialog;
procedure ShowTerminalEmulatorDialog(Sender: TObject);

implementation

{$R *.lfm}

uses
  IDEWindowIntf, LazIDEIntf;

{ TTerminalEmulatorDlg }

procedure TTerminalEmulatorDlg.TerminalTerminate(Sender: TObject);
begin
  Terminal.Restart;
end;

var
  TerminalEmulatorWindowCreator: TIDEWindowCreator;
  TerminalEmulatorDlg: TTerminalEmulatorDlg;

procedure CreateTerminalEmulator(Sender: TObject; {%H-}aFormName: string; var AForm: TCustomForm; DoDisableAutoSizing: Boolean);
begin
  if TerminalEmulatorDlg = nil then
    IDEWindowCreators.CreateForm(TerminalEmulatorDlg, TTerminalEmulatorDlg,
    DoDisableAutoSizing, LazarusIDE.OwningComponent);
  AForm := TerminalEmulatorDlg;
end;

procedure InitTerminalEmulatorDialog;
begin
  if TerminalEmulatorDlg = nil then
  begin
    TerminalEmulatorWindowCreator := IDEWindowCreators.Add('TerminalEmulatorDlg');
    TerminalEmulatorWindowCreator.OnCreateFormProc := CreateTerminalEmulator;
    TerminalEmulatorWindowCreator.CreateSimpleLayout;
  end;
end;

procedure ShowTerminalEmulatorDialog(Sender: TObject);
begin
  IDEWindowCreators.ShowForm('TerminalEmulatorDlg', True);
end;

end.

