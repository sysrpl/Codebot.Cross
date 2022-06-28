program fontman;

{$mode delphi}

uses
  Codebot.System,
  Interfaces, // this includes the LCL widgetset
  Classes, Forms, Dialogs, LCLIntf, UITypes,
  Main, FontTools, MaterialFont, ExportFrm, MaterialTools
  { you can add units after this };

{$R *.res}
{$R fonts.res}

function CheckFont: Boolean;
var
  S: string;
begin
  Result := Screen.Fonts.IndexOf('Material Design Icons') > -1;
  if not Result then
  begin
    if MessageDlg('The required "Material Design Icons" font was not found on your system. Would you like help installing it?',
      mtConfirmation, [mbyes, mbNo], 0) = mrYes then
    begin
      S := ConfigAppDir(False, True);
      S := PathCombine(S, MaterialFontFile);
      ResSaveData('materialdesignicons', S);
      OpenDocument(S);
      Sleep(100);
    end;
  end;
end;

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  if CheckFont then
  begin
    Application.CreateForm(TMaterialIconForm, MaterialIconForm);
    Application.Run;
  end;
end.

