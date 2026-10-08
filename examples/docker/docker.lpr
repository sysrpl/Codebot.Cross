  program docker;

{$mode delphi}

uses
	Codebot.System,
  Interfaces, // this includes the LCL widgetset
  Forms, main, Docker.Spacing;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TDockForm, DockForm);
  Application.Run;
end.

