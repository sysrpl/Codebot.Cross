  program easer;

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
  Application.CreateForm(TEasingForm, EasingForm);
  Application.Run;
end.

