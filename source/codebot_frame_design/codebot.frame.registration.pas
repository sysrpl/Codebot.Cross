(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ Codebot.Frame.Registration adds the Convert to Control command to the
  designer menu and to the tools menu of the IDE }

unit Codebot.Frame.Registration;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, LazFileUtils, MenuIntf,
  LazIDEIntf, SrcEditorIntf, PackageIntf, IDEMsgIntf, IDEExternToolIntf,
  Codebot.Frame.ConvertDialog,
  Codebot.Frame.Converter,
  Codebot.Frame.Resources,
  Codebot.Frame.Packages;

procedure Register;

implementation

{ Finds the form or frame shown by the designer of the active editor tab, and
  the file of its unit }

function FindDesignRoot(out Root: TComponent; out FileName: string): Boolean;
var
  Editor: TSourceEditorInterface;
  Designer: TIDesigner;
begin
  Root := nil;
  FileName := '';
  if (SourceEditorManagerIntf = nil) or (LazarusIDE = nil) then
    Exit(False);
  Editor := SourceEditorManagerIntf.ActiveEditor;
  if Editor = nil then
    Exit(False);
  Designer := LazarusIDE.GetDesignerForProjectEditor(Editor, False);
  if Designer = nil then
    Exit(False);
  Root := Designer.LookupRoot;
  if not ((Root is TCustomForm) or (Root is TCustomFrame)) then
    Exit(False);
  FileName := Editor.FileName;
  Result := FileName <> '';
end;

const
  Title = 'Convert to Control';

{ Shows the messages of a conversion in the messages window of the IDE }

procedure ShowMessages(const Messages: TConvertMessages);
var
  Urgency: TMessageLineUrgency;
  I: Integer;
begin
  if (IDEMessagesWindow = nil) or (Length(Messages) = 0) then
    Exit;
  for I := 0 to Length(Messages) - 1 do
  begin
    if Messages[I].IsWarning then
      Urgency := mluWarning
    else
      Urgency := mluHint;
    IDEMessagesWindow.AddCustomMessage(Urgency, Messages[I].Text, Messages[I].FileName,
      Messages[I].Line, 0, Title);
  end;
  LazarusIDE.DoShowMessagesView;
end;

{ Writes the unit and resource file of the control, and adds them and the
  packages they require to the package }

function WriteControl(Converter: TFrameConverter; const Options: TConvertOptions): Boolean;
var
  ResFile: string;
  Missing: TStringList;
  I: Integer;
begin
  Result := False;
  if not WriteTextFile(Options.UnitFile, Converter.UnitSource, 'unit') then
    Exit;
  { The resource file of an earlier conversion goes when it is not needed }
  ResFile := ChangeFileExt(Options.UnitFile, '.res');
  if Length(Converter.Resources) > 0 then
  try
    WriteResourceFile(ResFile, Converter.Resources);
  except
    on E: Exception do
    begin
      MessageDlg(Title, 'The resource file ' + ResFile + ' could not be written.'#10#10 +
        E.Message, mtError, [mbOK], 0);
      Exit;
    end;
  end
  else if FileExists(ResFile) then
    DeleteFile(ResFile);
  if not AddUnitToPackage(Options.PackageFile, Options.UnitFile, Options.UnitName) then
    Exit;
  Missing := TStringList.Create;
  try
    if not AddRequiredPackages(Options.PackageFile, Converter.ComponentUnits, Missing) then
      Exit;
    if not AddRequiredPackages(Options.PackageFile, Converter.UsedUnits, nil) then
      Exit;
    if IDEMessagesWindow <> nil then
      for I := 0 to Missing.Count - 1 do
        IDEMessagesWindow.AddCustomMessage(mluHint, 'The unit ' + Missing[I] + ' of a ' +
          'component was not found in an installed package. If it is not part of Free ' +
          'Pascal, add the package which has it to ' + Options.PackageName + '.', '', 0, 0, Title);
  finally
    Missing.Free;
  end;
  Result := True;
end;

procedure ConvertToControlClick(Sender: TObject);
var
  Root: TComponent;
  FileName: string;
  Options: TConvertOptions;
  Converter: TFrameConverter;
  Package: TIDEPackage;
begin
  if not FindDesignRoot(Root, FileName) then
  begin
    MessageDlg(Title, 'Open a form or frame in the designer first.',
      mtInformation, [mbOK], 0);
    Exit;
  end;
  if not FilenameIsAbsolute(FileName) then
  begin
    MessageDlg(Title, 'Save the unit of the form or frame first.',
      mtInformation, [mbOK], 0);
    Exit;
  end;
  if not ShowConvertDialog(Root, FileName, Options) then
    Exit;
  { The form file on disk must match the designer }
  if LazarusIDE.DoSaveEditorFile(FileName, []) <> mrOK then
    Exit;
  Converter := TFrameConverter.Create(Root, FileName, Options);
  try
    if not Converter.Execute then
    begin
      MessageDlg(Title, 'The form or frame could not be converted.'#10#10 + Converter.Error,
        mtError, [mbOK], 0);
      Exit;
    end;
    if Options.NewPackage then
    begin
      if not CreatePackageFile(Options.PackageFile) then
        Exit;
    end
    else if not PackageIsSaved(Options.PackageFile) then
      Exit;
    if not WriteControl(Converter, Options) then
      Exit;
    Package := OpenPackage(Options.PackageFile);
    if Package = nil then
      Exit;
    ShowMessages(Converter.Messages);
    if Options.OpenUnit then
      LazarusIDE.DoOpenEditorFile(Options.UnitFile, -1, -1, [ofAddToRecent, ofRevert]);
    if Options.InstallPackage then
      InstallPackage(Package);
  finally
    Converter.Free;
  end;
end;

{ TConvertMenu enables the menu items when the active editor tab changes, and
  when the tools menu is shown }

type
  TConvertMenu = class
  public
    DesignerCommand: TIDEMenuCommand;
    ToolsCommand: TIDEMenuCommand;
    procedure UpdateCommands(Sender: TObject);
  end;

const
  ConvertCaption = 'Convert form or frame to control ...';

var
  ConvertMenu: TConvertMenu;

procedure TConvertMenu.UpdateCommands(Sender: TObject);
var
  Root: TComponent;
  FileName: string;
  Enabled: Boolean;
begin
  Enabled := FindDesignRoot(Root, FileName);
  DesignerCommand.Enabled := Enabled;
  ToolsCommand.Enabled := Enabled;
end;

procedure Register;
begin
  ConvertMenu := TConvertMenu.Create;
  ConvertMenu.DesignerCommand := RegisterIDEMenuCommand(DesignerMenuSectionMisc,
    'ConvertToControlItem', ConvertCaption, nil, ConvertToControlClick);
  ConvertMenu.ToolsCommand := RegisterIDEMenuCommand(itmConversion,
    'ConvertFormToControlItem', ConvertCaption, nil, ConvertToControlClick);
  ConvertMenu.UpdateCommands(nil);
  mnuTools.AddHandlerOnShow(ConvertMenu.UpdateCommands);
  if SourceEditorManagerIntf <> nil then
    SourceEditorManagerIntf.RegisterChangeEvent(semEditorActivate,
      ConvertMenu.UpdateCommands);
end;

finalization
  if ConvertMenu <> nil then
  begin
    if mnuTools <> nil then
      mnuTools.RemoveHandlerOnShow(ConvertMenu.UpdateCommands);
    if SourceEditorManagerIntf <> nil then
      SourceEditorManagerIntf.UnRegisterChangeEvent(semEditorActivate,
        ConvertMenu.UpdateCommands);
    ConvertMenu.Free;
  end;
end.
