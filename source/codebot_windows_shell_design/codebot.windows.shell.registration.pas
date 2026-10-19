(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.windows.shell.registration.txt> }
unit Codebot.Windows.Shell.Registration;

{$i ../codebot/codebot.inc}

interface

uses
  Classes, PropEdits, ComponentEditors
  {$ifdef windows},
  Codebot.Windows.Explorer,
  Codebot.Windows.Shell.Tools,
  Codebot.Windows.Explorer.Editors,
  Codebot.Windows.Explorer.BindingEditor
  {$endif};

procedure Register;

implementation

{$r palette_icons.res}

procedure Register;
begin
  {$ifdef windows}
  { Components }
  RegisterComponents('Codebot Windows Shell', [TExplorerTree, TExplorerView,
    TExplorerEdit, TExplorerPreview, TExplorerBinding, TShellFileOperation,
    TShellNotify, TShellLink]);
  { Property editors }
  RegisterPropertyEditor(TypeInfo(string), TExplorerView, 'Path',
    TExplorerPathPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TExplorerTree, 'Root',
    TExplorerPathPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TExplorerEdit, 'Path',
    TExplorerPathPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TShellNotify, 'Path',
    TExplorerPathPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TShellLink, 'FileName',
    TFileNamePropertyEditor);
  { Component editors }
  RegisterComponentEditor(TExplorerBinding, TExplorerBindingEditor);
  {$endif}
end;

end.
