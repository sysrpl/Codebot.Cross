program clock;

{$mode delphi}

uses
	Codebot.System,
  Interfaces, // this includes the LCL widgetset
  Forms, Main
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TClockWidget, ClockWidget);
  Application.Run;
end.

