(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ Codebot.Explorer.Registration adds the Application Explorer to the
  information items of the IDE menu }

{ <include docs/codebot.explorer.registration.txt> }
unit Codebot.Explorer.Registration;

{$mode delphi}

interface

uses
  Classes, MenuIntf,
  Codebot.Design.AppExplorer;

procedure Register;

implementation

procedure AppExplorerItemClick(Sender: TObject);
begin
  ShowAppExplorer;
end;

procedure Register;
begin
  { The application explorer is added to the information items of the IDE menu }
  RegisterIDEMenuCommand(itmInfoHelps, 'AppExplorerItem', 'Application Explorer',
    nil, AppExplorerItemClick, nil, 'menu_information');
end;

end.
