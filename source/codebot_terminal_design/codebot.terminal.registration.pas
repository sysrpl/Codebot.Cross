(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ Codebot.Terminal.Registration adds TTerminal to the component palette, and
  a Terminal window to the view menu of the IDE }

unit Codebot.Terminal.Registration;

{$i ../codebot_terminal/terminal.inc}

interface

uses
  Classes, MenuIntf,
  Codebot.Terminal.Controls,
  Codebot.Terminal.EmulatorDialog;

procedure Register;

implementation

{$r palette_icons.res}

procedure Register;
begin
  { Components }
  RegisterComponents('Codebot Terminal', [TTerminal]);
  { The terminal window of the IDE }
  RegisterIDEMenuCommand(itmViewSecondaryWindows, 'TerminalEmulatorItem', 'Terminal',
    nil, ShowTerminalEmulatorDialog, nil, 'menu_information');
  InitTerminalEmulatorDialog;
end;

end.
