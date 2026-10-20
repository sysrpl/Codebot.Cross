(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ Codebot.Frame.Packages creates the package of a converted control, adds the
  unit of the control and the packages it requires to the package, and opens
  and installs the package in the IDE }

{ <include docs/codebot.frame.packages.txt> }
unit Codebot.Frame.Packages;

{$mode delphi}

interface

uses
  Classes, SysUtils, Contnrs, Controls, Dialogs, PackageIntf;

{ Writes a new run and design time package which requires the LCL to FileName.
  The name of the package is the name of the file without its extension.
  Returns False after telling the user if the file could not be written. }
function CreatePackageFile(const FileName: string): Boolean;
{ Returns False after telling the user if the package FileName is open in the
  IDE with changes which have not been saved, as its file is about to change }
function PackageIsSaved(const FileName: string): Boolean;
{ Writes Text to FileName, creating its folder if needed. What names the kind
  of file, such as unit. Returns False after telling the user if it failed. }
function WriteTextFile(const FileName, Text, What: string): Boolean;
{ Adds the unit UnitName in UnitFile to the package file PackageFile, as a unit
  with a Register procedure. Returns False after telling the user if it failed. }
function AddUnitToPackage(const PackageFile, UnitFile, UnitName: string): Boolean;
{ Opens the package FileName in the IDE. A package which is already open is
  loaded again from its file. Returns nil after telling the user if the
  package could not be opened. }
function OpenPackage(const FileName: string): TIDEPackage;
{ Adds to the package file PackageFile the installed packages which hold
  Units. Units which are not in an installed package are added to Missing,
  if it is not nil. Returns False after telling the user if it failed. }
function AddRequiredPackages(const PackageFile: string; Units, Missing: TStrings): Boolean;
{ Installs the package in the IDE. An IDE built from dynamic packages loads
  the package without being rebuilt. Returns False if it was not installed. }
function InstallPackage(Package: TIDEPackage): Boolean;

implementation

uses
  LazFileUtils, Laz2_XMLCfg;

const
  Title = 'Convert to Control';

  PackageTemplate =
    '<?xml version="1.0" encoding="UTF-8"?>'#10 +
    '<CONFIG>'#10 +
    '  <Package Version="6">'#10 +
    '    <Name Value="%name%"/>'#10 +
    '    <Type Value="RunAndDesignTime"/>'#10 +
    '    <CompilerOptions>'#10 +
    '      <Version Value="11"/>'#10 +
    '      <SearchPaths>'#10 +
    '        <UnitOutputDirectory Value="lib/$(TargetCPU)-$(TargetOS)"/>'#10 +
    '      </SearchPaths>'#10 +
    '    </CompilerOptions>'#10 +
    '    <RequiredPkgs>'#10 +
    '      <Item>'#10 +
    '        <PackageName Value="LCL"/>'#10 +
    '      </Item>'#10 +
    '    </RequiredPkgs>'#10 +
    '    <UsageOptions>'#10 +
    '      <UnitPath Value="$(PkgOutDir)"/>'#10 +
    '    </UsageOptions>'#10 +
    '    <PublishOptions>'#10 +
    '      <Version Value="2"/>'#10 +
    '      <UseFileFilters Value="True"/>'#10 +
    '    </PublishOptions>'#10 +
    '  </Package>'#10 +
    '</CONFIG>'#10;

function PackageNameOf(const FileName: string): string;
begin
  Result := ChangeFileExt(ExtractFileName(FileName), '');
end;

function WriteTextFile(const FileName, Text, What: string): Boolean;
var
  Stream: TFileStream;
begin
  Result := False;
  try
    if not ForceDirectories(ExtractFileDir(FileName)) then
      raise EInOutError.Create('The folder could not be created.');
    Stream := TFileStream.Create(FileName, fmCreate);
    try
      if Text <> '' then
        Stream.WriteBuffer(Text[1], Length(Text));
    finally
      Stream.Free;
    end;
    Result := True;
  except
    on E: Exception do
      MessageDlg(Title, 'The ' + What + ' ' + FileName +
        ' could not be written.'#10#10 + E.Message, mtError, [mbOK], 0);
  end;
end;

{ Finds the package FileName if it is open in the IDE }

function FindOpenPackage(const FileName: string): TIDEPackage;
begin
  Result := PackageEditingInterface.FindPackageWithName(PackageNameOf(FileName));
  if (Result <> nil) and (CompareFilenames(Result.Filename, FileName) <> 0) then
    Result := nil;
end;

function CreatePackageFile(const FileName: string): Boolean;
begin
  Result := WriteTextFile(FileName, StringReplace(PackageTemplate, '%name%',
    PackageNameOf(FileName), []), 'package');
end;

function PackageIsSaved(const FileName: string): Boolean;
var
  Package: TIDEPackage;
begin
  Package := FindOpenPackage(FileName);
  Result := (Package = nil) or not Package.Modified;
  if not Result then
    MessageDlg(Title, 'The package ' + Package.Name + ' has changes which ' +
      'have not been saved. Save the package first.', mtWarning, [mbOK], 0);
end;

{ Package files list their items either with numbered names and a count, in
  older files, or with repeated Item elements }

function AddUnitToPackage(const PackageFile, UnitFile, UnitName: string): Boolean;
const
  FilesPath = 'Package/Files/';
var
  Config: TXMLConfig;
  Legacy: Boolean;
  Count, I: Integer;
  ItemPath, Name: string;
begin
  Result := False;
  Name := ExtractRelativePath(ExtractFilePath(PackageFile), UnitFile);
  try
    Config := TXMLConfig.Create(PackageFile);
    try
      Legacy := (Config.GetValue('Package/Version', 0) <= 4) or Config.IsLegacyList(FilesPath);
      Count := Config.GetListItemCount(FilesPath, 'Item', Legacy);
      for I := 0 to Count - 1 do
      begin
        ItemPath := FilesPath + Config.GetListItemXPath('Item', I, Legacy, True) + '/';
        if CompareFilenames(Config.GetValue(ItemPath + 'Filename/Value', ''), Name) = 0 then
          Exit(True);
      end;
      ItemPath := FilesPath + Config.GetListItemXPath('Item', Count, Legacy, True) + '/';
      Config.SetValue(ItemPath + 'Filename/Value', Name);
      Config.SetValue(ItemPath + 'HasRegisterProc/Value', True);
      Config.SetValue(ItemPath + 'UnitName/Value', UnitName);
      if Legacy then
        Config.SetValue(FilesPath + 'Count', Count + 1);
      Config.Flush;
      Result := True;
    finally
      Config.Free;
    end;
  except
    on E: Exception do
      MessageDlg(Title, 'The unit ' + UnitName + ' could not be added to the package ' +
        PackageFile + '.'#10#10 + E.Message, mtError, [mbOK], 0);
  end;
end;

function OpenPackage(const FileName: string): TIDEPackage;
var
  Flags: TPkgOpenFlags;
begin
  Result := nil;
  Flags := [pofAddToRecent];
  if FindOpenPackage(FileName) <> nil then
    Include(Flags, pofRevert);
  if PackageEditingInterface.DoOpenPackageFile(FileName, Flags, False) = mrOK then
    Result := FindOpenPackage(FileName);
  if Result = nil then
    MessageDlg(Title, 'The package ' + FileName + ' could not be opened.',
      mtError, [mbOK], 0);
end;

{ The packages every converted control already requires, directly or through
  the LCL }

function IsBasePackage(const Name: string): Boolean;
const
  BasePackages: array[0..3] of string = ('LCL', 'LCLBase', 'LazUtils', 'FCL');
var
  S: string;
begin
  for S in BasePackages do
    if SameText(Name, S) then
      Exit(True);
  Result := False;
end;

function AddRequiredPackages(const PackageFile: string; Units, Missing: TStrings): Boolean;
const
  RequiredPath = 'Package/RequiredPkgs/';
var
  Names: TStringList;
  Package: TIDEPackage;
  Config: TXMLConfig;
  Legacy: Boolean;
  Count, I, J: Integer;
  S: string;
begin
  Result := False;
  Names := TStringList.Create;
  try
    for I := 0 to Units.Count - 1 do
    begin
      Package := PackageEditingInterface.FindInstalledPackageWithUnit(Units[I]);
      if Package = nil then
      begin
        if Missing <> nil then
          Missing.Add(Units[I]);
        Continue;
      end;
      S := Package.Name;
      if IsBasePackage(S) or SameText(S, PackageNameOf(PackageFile)) or
        (Names.IndexOf(S) > -1) then
        Continue;
      Names.Add(S);
    end;
    if Names.Count = 0 then
      Exit(True);
    try
      Config := TXMLConfig.Create(PackageFile);
      try
        Legacy := (Config.GetValue('Package/Version', 0) <= 4) or Config.IsLegacyList(RequiredPath);
        Count := Config.GetListItemCount(RequiredPath, 'Item', Legacy);
        { Packages which are already required are not added again }
        for I := 0 to Count - 1 do
        begin
          S := Config.GetValue(RequiredPath + Config.GetListItemXPath('Item', I, Legacy, True) +
            '/PackageName/Value', '');
          J := Names.IndexOf(S);
          if J > -1 then
            Names.Delete(J);
        end;
        for I := 0 to Names.Count - 1 do
        begin
          Config.SetValue(RequiredPath + Config.GetListItemXPath('Item', Count, Legacy, True) +
            '/PackageName/Value', Names[I]);
          Inc(Count);
        end;
        if Legacy then
          Config.SetValue(RequiredPath + 'Count', Count);
        Config.Flush;
        Result := True;
      finally
        Config.Free;
      end;
    except
      on E: Exception do
        MessageDlg(Title, 'The required packages could not be added to the package ' +
          PackageFile + '.'#10#10 + E.Message, mtError, [mbOK], 0);
    end;
  finally
    Names.Free;
  end;
end;

function InstallPackage(Package: TIDEPackage): Boolean;
var
  List: TObjectList;
  ID: TLazPackageID;
begin
  { An installed package is loaded again when it is compiled in its editor }
  if (Package.AutoInstall <> pitNope) or (Package.Installed <> pitNope) then
  begin
    MessageDlg(Title, 'The package ' + Package.Name + ' is already installed. Compile it ' +
      'in the package editor to load the new control.', mtInformation, [mbOK], 0);
    Exit(False);
  end;
  List := TObjectList.Create(True);
  try
    ID := TLazPackageID.Create;
    ID.AssignID(Package);
    List.Add(ID);
    Result := PackageEditingInterface.InstallPackages(List, [piiifQuiet, piiifRebuildIDE]) = mrOK;
  finally
    List.Free;
  end;
end;

end.
