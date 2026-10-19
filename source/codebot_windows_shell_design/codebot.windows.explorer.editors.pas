(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.windows.explorer.editors.txt> }
unit Codebot.Windows.Explorer.Editors;

{$i ../codebot/codebot.inc}

interface

uses
  Classes, SysUtils, PropEdits;

{ TExplorerPathPropertyEditor edits the Path of a TExplorerView,
  TExplorerEdit, or TShellNotify, or the Root of a TExplorerTree. It lists
  common shell places and special folders, opens the Windows folder picker,
  and rejects text which is not a folder }

type
  TExplorerPathPropertyEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: ansistring; override;
    procedure SetValue(const NewValue: ansistring); override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure Edit; override;
  end;

implementation

uses
  Windows, ActiveX, ShlObj, ComObj,
  Codebot.System,
  Codebot.Interop.Windows.Shell,
  Codebot.Windows.Explorer;

{ Places are stored by their shell parsing name, special folders by their
  known folder name such as shell:Downloads, and the desktop is the empty
  path }

type
  TShellPlace = record
    Name: string;
    Path: string;
  end;

const
  Places: array[0..22] of TShellPlace = (
    (Name: 'Desktop'; Path: ''),
    (Name: 'Home'; Path: '::{F874310E-B6B7-47DC-BC84-B9E6B38F5903}'),
    (Name: 'This PC'; Path: '::{20D04FE0-3AEA-1069-A2D8-08002B30309D}'),
    (Name: 'Libraries'; Path: '::{031E4825-7B94-4DC3-B131-E946B44C8DD5}'),
    (Name: 'Network'; Path: '::{F02C1A0D-BE21-4350-88B0-7367FC96EF3C}'),
    (Name: 'Control Panel'; Path: '::{26EE0668-A00A-44D7-9371-BEB064C98683}'),
    (Name: 'Recycle Bin'; Path: '::{645FF040-5081-101B-9F08-00AA002F954E}'),
    (Name: 'User Folder'; Path: 'shell:Profile'),
    (Name: 'Documents'; Path: 'shell:Personal'),
    (Name: 'Downloads'; Path: 'shell:Downloads'),
    (Name: 'Pictures'; Path: 'shell:My Pictures'),
    (Name: 'Music'; Path: 'shell:My Music'),
    (Name: 'Videos'; Path: 'shell:My Video'),
    (Name: 'Favorites'; Path: 'shell:Favorites'),
    (Name: 'OneDrive'; Path: 'shell:OneDrive'),
    (Name: 'AppData'; Path: 'shell:AppData'),
    (Name: 'Local AppData'; Path: 'shell:Local AppData'),
    (Name: 'Program Files'; Path: 'shell:ProgramFiles'),
    (Name: 'Windows'; Path: 'shell:Windows'),
    (Name: 'Fonts'; Path: 'shell:Fonts'),
    (Name: 'Start Menu'; Path: 'shell:Start Menu'),
    (Name: 'Startup'; Path: 'shell:Startup'),
    (Name: 'Recent'; Path: 'shell:Recent'));

function IsFolder(const Path: string): Boolean;
var
  pidl: PItemIDList;
  Item: IShellItem;
  Attributes: SFGAOF;
begin
  Result := False;
  pidl := ShellPathToIDList(Path);
  if pidl = nil then
    Exit;
  try
    if SHCreateItemFromIDList(pidl, IShellItem, Item) = S_OK then
    begin
      Attributes := 0;
      Item.GetAttributes(SFGAO_FOLDER, @Attributes);
      Result := (Attributes and SFGAO_FOLDER) <> 0;
    end;
  finally
    CoTaskMemFree(pidl);
  end;
end;

{ TExplorerPathPropertyEditor }

function TExplorerPathPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paValueList, paRevertable];
end;

function TExplorerPathPropertyEditor.GetValue: ansistring;
var
  S: string;
  I: Integer;
begin
  S := GetStrValue;
  for I := Low(Places) to High(Places) do
    if SameText(S, Places[I].Path) then
      Exit(Places[I].Name);
  Result := S;
end;

procedure TExplorerPathPropertyEditor.SetValue(const NewValue: ansistring);
var
  S: string;
  I: Integer;
begin
  S := Trim(NewValue);
  for I := Low(Places) to High(Places) do
    if SameText(S, Places[I].Name) then
    begin
      SetStrValue(Places[I].Path);
      Exit;
    end;
  if not IsFolder(S) then
    raise Exception.CreateFmt('"%s" is not a folder', [S]);
  SetStrValue(S);
end;

procedure TExplorerPathPropertyEditor.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := Low(Places) to High(Places) do
    Proc(Places[I].Name);
end;

procedure TExplorerPathPropertyEditor.Edit;
var
  Dialog: IFileDialog;
  Current, Item: IShellItem;
  Options: FILEOPENDIALOGOPTIONS;
  pidl: PItemIDList;
  P: PWideChar;
begin
  OleCheck(CoCreateInstance(CLSID_FileOpenDialog, nil, CLSCTX_INPROC_SERVER,
    IFileDialog, Dialog));
  Options := 0;
  Dialog.GetOptions(@Options);
  { Virtual folders such as This PC can be picked too }
  Dialog.SetOptions(Options or FOS_PICKFOLDERS or FOS_ALLNONSTORAGEITEMS);
  Dialog.SetTitle('Select a Folder');
  pidl := ShellPathToIDList(GetStrValue);
  if pidl <> nil then
  begin
    if SHCreateItemFromIDList(pidl, IShellItem, Current) = S_OK then
      Dialog.SetFolder(Current);
    CoTaskMemFree(pidl);
  end;
  if Dialog.Show(GetActiveWindow) <> S_OK then
    Exit;
  if (Dialog.GetResult(@Item) <> S_OK) or (Item = nil) then
    Exit;
  P := nil;
  if Item.GetDisplayName(SIGDN(SIGDN_DESKTOPABSOLUTEPARSING), LPWSTR(@P)) = S_OK then
  try
    SetStrValue(WideToStr(WideString(P)));
  finally
    CoTaskMemFree(P);
  end;
end;

end.
