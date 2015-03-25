(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified September 2013                             *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.graphics.design.registration.txt> }
unit Codebot.Design.Forms;

{$i codebot.inc}

interface

uses
  Classes, SysUtils, Forms, ProjectIntf, NewItemIntf;

type
  TCustomFormClass = class of TCustomForm;

procedure RegisterForm(FormClass: TCustomFormClass; const Caption, Description, UnitName: string);

implementation

{ TCustomFormDescriptor }

type
  TCustomFormDescriptor = class(TFileDescPascalUnitWithResource)
  private
    FCaption: string;
    FDescription: string;
    FUnitName: string;
  public
    constructor Create(FormClass: TCustomFormClass; const Caption, Description, UnitName: string);
    function GetResourceType: TResourceType; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function GetInterfaceUsesSection: string; override;
  end;

var
  Created: Boolean;

procedure RegisterForm(FormClass: TCustomFormClass; const Caption, Description, UnitName: string);
const
  CodebotForm = 'Codebot Form';
begin
  RegisterNoIcon([FormClass]);
  if not Created then
    RegisterNewItemCategory(TNewIDEItemCategory.Create(CodebotForm));
  Created := True;
  RegisterProjectFileDescriptor(TCustomFormDescriptor.Create(FormClass, Caption,
    Description, UnitName), CodebotForm);
end;

constructor TCustomFormDescriptor.Create(FormClass: TCustomFormClass;
  const Caption, Description, UnitName: string);
begin
  inherited Create;
  FCaption := Caption;
  FDescription := Description;
  FUnitName := UnitName;
  ResourceClass := FormClass;
  Name := Caption;
  RequiredPackages := 'LCL;codebot';
  UseCreateFormStatements := True;
end;

function TCustomFormDescriptor.GetResourceType: TResourceType;
begin
  Result := rtRes;
end;

function TCustomFormDescriptor.GetLocalizedName: string;
begin
  Result := FCaption;
end;

function TCustomFormDescriptor.GetLocalizedDescription: string;
begin
  Result:= FDescription;
end;

function TCustomFormDescriptor.GetInterfaceUsesSection: string;
begin
  Result := inherited GetInterfaceUsesSection;
  Result := Result + ', Forms,'#13#10 +
    '  Codebot.System,'#13#10 +
    '  Codebot.Graphics,'#13#10 +
    '  Codebot.Graphics.Types,'#13#10 +
    '  ' + FUnitName;
end;

end.

