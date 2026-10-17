(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ Codebot.Frame.ConvertDialog asks for the name of the control a form or frame
  is converted into, and for the package the control is added to }

unit Codebot.Frame.ConvertDialog;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

{ TConvertOptions holds the choices made in the convert dialog }

type
  TConvertOptions = record
    { The class name of the new control, such as TMyControl }
    ControlClass: string;
    { The name of the new unit, such as MyControlCtrls }
    UnitName: string;
    { The full path of the new unit, which is in the folder of the package }
    UnitFile: string;
    { The page of the component palette the control is registered on }
    PalettePage: string;
    { True if the unit is added to a new package }
    NewPackage: Boolean;
    { The name of the new or existing package }
    PackageName: string;
    { The full path of the .lpk file of the new or existing package, whose
      folder is also where the new unit is written }
    PackageFile: string;
    { True if the package is installed once the control is created }
    InstallPackage: Boolean;
    { True if the new unit is opened in the source editor }
    OpenUnit: Boolean;
  end;

{ TConvertDialog }

  TConvertDialog = class(TForm)
    SourceLabel: TLabel;
    ClassNameLabel: TLabel;
    ClassNameEdit: TEdit;
    UnitNameLabel: TLabel;
    UnitNameEdit: TEdit;
    PageLabel: TLabel;
    PageCombo: TComboBox;
    PackageGroup: TGroupBox;
    NewPackageRadio: TRadioButton;
    PackageFileLabel: TLabel;
    PackageFileEdit: TEdit;
    PackageFileButton: TButton;
    ExistingPackageRadio: TRadioButton;
    PackageCombo: TComboBox;
    PackageButton: TButton;
    InstallCheck: TCheckBox;
    OpenCheck: TCheckBox;
    ConvertButton: TButton;
    CancelButton: TButton;
    PackageSaveDialog: TSaveDialog;
    PackageDialog: TOpenDialog;
    procedure ClassNameEditChange(Sender: TObject);
    procedure UnitNameEditChange(Sender: TObject);
    procedure PackageRadioChange(Sender: TObject);
    procedure PackageFileButtonClick(Sender: TObject);
    procedure PackageButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FSourceFolder: string;
    FPackageFiles: TStringList;
    FUpdating: Boolean;
    FUnitNameEdited: Boolean;
    function NewPackageFile: string;
    function PackageFolder: string;
    procedure LoadPages;
    procedure LoadPackages;
    procedure UpdateControls;
    function Validate: Boolean;
  public
    destructor Destroy; override;
    procedure Init(Root: TComponent; const FileName: string);
    procedure GetOptions(out Options: TConvertOptions);
  end;

{ Shows the convert dialog for the form or frame Root, whose unit is FileName,
  and returns True with the choices in Options if the user clicked Convert }
function ShowConvertDialog(Root: TComponent; const FileName: string;
  out Options: TConvertOptions): Boolean;

implementation

{$R *.lfm}

uses
  ComponentReg, PackageIntf;

const
  DefaultPage = 'Custom';

{ Makes a default control class name from the class name of the form or frame,
  so that TSettingsFrame becomes TSettingsControl }

function DefaultControlClass(Root: TComponent): string;
const
  Suffixes: array[0..1] of string = ('Frame', 'Form');
var
  S: string;
begin
  Result := Root.ClassName;
  for S in Suffixes do
    if (Length(Result) > Length(S) + 1) and Result.EndsWith(S) then
    begin
      SetLength(Result, Length(Result) - Length(S));
      Break;
    end;
  Result := Result + 'Control';
end;

{ Removes the T from the start of a class name, so that TMyControl becomes
  MyControl }

function ClassBaseName(const ControlClass: string): string;
begin
  Result := ControlClass;
  if (Length(Result) > 1) and (Result[1] in ['T', 't']) then
    Delete(Result, 1, 1);
end;

{ Makes a unit name from a control class name, so that TMyCrazyGrid becomes
  MyCrazyGridCtrls }

function DefaultUnitName(const ControlClass: string): string;
begin
  Result := ClassBaseName(ControlClass) + 'Ctrls';
end;

{ The file of a unit is its name in lowercase, so that MyCrazyGridCtrls is
  written to mycrazygridctrls.pas }

function UnitFileName(const UnitName: string): string;
begin
  Result := LowerCase(UnitName) + '.pas';
end;

{ Makes a package file name from a control class name, so that TMyControl
  becomes mycontrols.lpk }

function DefaultPackageFile(const ControlClass: string): string;
begin
  Result := LowerCase(ClassBaseName(ControlClass)) + 's.lpk';
end;

{ TConvertDialog }

destructor TConvertDialog.Destroy;
begin
  FPackageFiles.Free;
  inherited Destroy;
end;

procedure TConvertDialog.Init(Root: TComponent; const FileName: string);
begin
  FPackageFiles := TStringList.Create;
  FSourceFolder := ExtractFilePath(FileName);
  SourceLabel.Caption := 'Source: ' + Root.ClassName + ' (' +
    ExtractFileName(FileName) + ')';
  FUpdating := True;
  try
    ClassNameEdit.Text := DefaultControlClass(Root);
    UnitNameEdit.Text := DefaultUnitName(ClassNameEdit.Text);
    PackageFileEdit.Text := '';
  finally
    FUpdating := False;
  end;
  LoadPages;
  LoadPackages;
  if PackageCombo.Items.Count > 0 then
    ExistingPackageRadio.Checked := True
  else
    NewPackageRadio.Checked := True;
  UpdateControls;
end;

procedure TConvertDialog.LoadPages;
var
  Pages: TStringList;
  I: Integer;
begin
  Pages := TStringList.Create;
  try
    Pages.Sorted := True;
    Pages.Duplicates := dupIgnore;
    Pages.Add(DefaultPage);
    if IDEComponentPalette <> nil then
      for I := 0 to IDEComponentPalette.Pages.Count - 1 do
        if IDEComponentPalette.Pages[I].PageName <> '' then
          Pages.Add(IDEComponentPalette.Pages[I].PageName);
    PageCombo.Items.Assign(Pages);
  finally
    Pages.Free;
  end;
  PageCombo.Text := DefaultPage;
end;

{ Lists the open packages which can register components and can be written to }

procedure TConvertDialog.LoadPackages;
var
  Package: TIDEPackage;
  I: Integer;
begin
  PackageCombo.Items.Clear;
  FPackageFiles.Clear;
  if PackageEditingInterface = nil then
    Exit;
  for I := 0 to PackageEditingInterface.GetPackageCount - 1 do
  begin
    Package := PackageEditingInterface.GetPackages(I);
    if Package.IsVirtual or Package.ReadOnly then
      Continue;
    if Package.PackageType = lptRunTime then
      Continue;
    PackageCombo.Items.Add(Package.Name);
    FPackageFiles.Add(Package.Filename);
  end;
  if PackageCombo.Items.Count > 0 then
    PackageCombo.ItemIndex := 0;
end;

procedure TConvertDialog.UpdateControls;
var
  NewPackage: Boolean;
begin
  NewPackage := NewPackageRadio.Checked;
  PackageFileLabel.Enabled := NewPackage;
  PackageFileEdit.Enabled := NewPackage;
  PackageFileButton.Enabled := NewPackage;
  PackageCombo.Enabled := not NewPackage;
  PackageButton.Enabled := not NewPackage;
end;

procedure TConvertDialog.ClassNameEditChange(Sender: TObject);
begin
  if FUpdating or FUnitNameEdited then
    Exit;
  FUpdating := True;
  try
    UnitNameEdit.Text := DefaultUnitName(Trim(ClassNameEdit.Text));
  finally
    FUpdating := False;
  end;
end;

procedure TConvertDialog.UnitNameEditChange(Sender: TObject);
begin
  if not FUpdating then
    FUnitNameEdited := True;
end;

procedure TConvertDialog.PackageRadioChange(Sender: TObject);
begin
  UpdateControls;
end;

{ The new package file typed by the user, with an .lpk extension added if it
  has none }

function TConvertDialog.NewPackageFile: string;
begin
  Result := Trim(PackageFileEdit.Text);
  if Result = '' then
    Exit;
  if ExtractFileExt(Result) = '' then
    Result := Result + '.lpk';
  Result := ExpandFileName(Result);
end;

{ The folder of the chosen package, where the new unit is written }

function TConvertDialog.PackageFolder: string;
begin
  if NewPackageRadio.Checked then
    Result := ExtractFilePath(NewPackageFile)
  else if PackageCombo.ItemIndex > -1 then
    Result := ExtractFilePath(FPackageFiles[PackageCombo.ItemIndex])
  else
    Result := '';
end;

procedure TConvertDialog.PackageFileButtonClick(Sender: TObject);
begin
  if NewPackageFile <> '' then
  begin
    PackageSaveDialog.InitialDir := ExtractFileDir(NewPackageFile);
    PackageSaveDialog.FileName := ExtractFileName(NewPackageFile);
  end
  else
  begin
    PackageSaveDialog.InitialDir := FSourceFolder;
    PackageSaveDialog.FileName := DefaultPackageFile(ClassNameEdit.Text);
  end;
  if PackageSaveDialog.Execute then
    PackageFileEdit.Text := PackageSaveDialog.FileName;
end;

procedure TConvertDialog.PackageButtonClick(Sender: TObject);
var
  I: Integer;
begin
  if not PackageDialog.Execute then
    Exit;
  I := FPackageFiles.IndexOf(PackageDialog.FileName);
  if I < 0 then
  begin
    PackageCombo.Items.Add(ChangeFileExt(ExtractFileName(PackageDialog.FileName), ''));
    I := FPackageFiles.Add(PackageDialog.FileName);
  end;
  PackageCombo.ItemIndex := I;
end;

function TConvertDialog.Validate: Boolean;

  function Fail(Control: TWinControl; const Message: string): Boolean;
  begin
    MessageDlg('Convert to Control', Message, mtWarning, [mbOK], 0);
    Control.SetFocus;
    Result := False;
  end;

var
  S: string;
begin
  S := Trim(ClassNameEdit.Text);
  if not IsValidIdent(S) then
    Exit(Fail(ClassNameEdit, 'The class name must be a valid identifier.'));
  if (IDEComponentPalette <> nil) and (IDEComponentPalette.FindRegComponent(S) <> nil) then
    Exit(Fail(ClassNameEdit, 'A component named ' + S + ' is already registered.'));
  S := Trim(UnitNameEdit.Text);
  if not IsValidIdent(S, True) then
    Exit(Fail(UnitNameEdit, 'The unit name must be a valid identifier.'));
  if Trim(PageCombo.Text) = '' then
    Exit(Fail(PageCombo, 'Enter the palette page of the control.'));
  if NewPackageRadio.Checked then
  begin
    S := NewPackageFile;
    if S = '' then
      Exit(Fail(PackageFileEdit, 'Choose where the new package is saved.'));
    if LowerCase(ExtractFileExt(S)) <> '.lpk' then
      Exit(Fail(PackageFileEdit, 'The package file must end in .lpk.'));
    if FileExists(S) then
      Exit(Fail(PackageFileEdit, 'The package ' + S + ' already exists. ' +
        'Choose Existing package to add the control to it.'));
    S := ChangeFileExt(ExtractFileName(S), '');
    if not IsValidIdent(S) then
      Exit(Fail(PackageFileEdit, 'The name of the package file must be a valid identifier.'));
    if (PackageEditingInterface <> nil) and (PackageEditingInterface.FindPackageWithName(S) <> nil) then
      Exit(Fail(PackageFileEdit, 'A package named ' + S + ' is already open.'));
  end
  else if PackageCombo.ItemIndex < 0 then
    Exit(Fail(PackageCombo, 'Choose the package the control is added to.'));
  { Converting a changed form or frame again replaces the unit made before }
  S := PackageFolder + UnitFileName(Trim(UnitNameEdit.Text));
  if FileExists(S) and (MessageDlg('Convert to Control', 'The unit ' + S +
    ' already exists. Do you want to replace it?', mtConfirmation, [mbYes, mbNo], 0) <> mrYes) then
  begin
    UnitNameEdit.SetFocus;
    Exit(False);
  end;
  Result := True;
end;

procedure TConvertDialog.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if ModalResult = mrOK then
    CanClose := Validate;
end;

procedure TConvertDialog.GetOptions(out Options: TConvertOptions);
begin
  Options.ControlClass := Trim(ClassNameEdit.Text);
  Options.UnitName := Trim(UnitNameEdit.Text);
  Options.PalettePage := Trim(PageCombo.Text);
  Options.NewPackage := NewPackageRadio.Checked;
  if Options.NewPackage then
  begin
    Options.PackageFile := NewPackageFile;
    Options.PackageName := ChangeFileExt(ExtractFileName(Options.PackageFile), '');
  end
  else
  begin
    Options.PackageName := PackageCombo.Items[PackageCombo.ItemIndex];
    Options.PackageFile := FPackageFiles[PackageCombo.ItemIndex];
  end;
  Options.UnitFile := PackageFolder + UnitFileName(Options.UnitName);
  Options.InstallPackage := InstallCheck.Checked;
  Options.OpenUnit := OpenCheck.Checked;
end;

function ShowConvertDialog(Root: TComponent; const FileName: string;
  out Options: TConvertOptions): Boolean;
var
  Dialog: TConvertDialog;
begin
  Options := Default(TConvertOptions);
  Dialog := TConvertDialog.Create(nil);
  try
    Dialog.Init(Root, FileName);
    Result := Dialog.ShowModal = mrOK;
    if Result then
      Dialog.GetOptions(Options);
  finally
    Dialog.Free;
  end;
end;

end.
