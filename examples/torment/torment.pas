program torment;

{$mode delphi}

uses
	Codebot.System,
  Interfaces, // this includes the LCL widgetset
  Forms, Main, Downloads
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TDetailsForm, DetailsForm);
  Application.Run;
end.

