(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://www.codebot.org                              *)
(*  Modified September 2013                             *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.graphics.design.registration.txt> }
unit Codebot.Design.Editors;

{$i codebot.inc}

interface

uses
  Classes, PropEdits, ComponentEditors, ProjectIntf, NewItemIntf,
  Codebot.Graphics,
  Codebot.Controls.Extras,
  Codebot.Design.SurfaceBitmapEditor,
  Codebot.Design.ImageListEditor;

{ TThemeNamePropertyEditor }

type
  TThemeNamePropertyEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;


{ TSurfaceBitmapPropertyEditor }

  TSurfaceBitmapPropertyEditor = class(TPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: AnsiString; override;
  end;

{ TRenderImageEditor }

  TRenderImageEditor = class(TComponentEditor)
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

{ TImageStripEditor }

  TImageStripEditor = class(TComponentEditor)
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

implementation

{ TThemeNamePropertyEditor }

function TThemeNamePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes;
  Result := Result + [paValueList];
end;

procedure TThemeNamePropertyEditor.GetValues(Proc: TGetStrProc);
var
  S: TStrings;
  I: Integer;
begin
  S := TStringList.Create;
  try
    ThemeNames(S);
    for I := 0 to S.Count - 1 do
      Proc(S[I]);
  finally
    S.Free;
  end;
end;

{ TSurfaceBitmapPropertyEditor }

function TSurfaceBitmapPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TSurfaceBitmapPropertyEditor.GetValue: AnsiString;
begin
  Result := '(' + GetPropType^.Name + ')';
end;

procedure TSurfaceBitmapPropertyEditor.Edit;
var
  Instance: TObject;
  Bitmap: TSurfaceBitmap;
begin
  Instance := GetObjectValue;
  if Instance is TSurfaceBitmap then
  begin
    Bitmap := Instance as TSurfaceBitmap;
    if EditSurfaceBitmap(Bitmap) then
      Modified;
  end;
end;

{ TRenderImageEditor }

function TRenderImageEditor.GetVerb(Index: Integer): string;
begin
  Result := 'Edit render image ...';
end;

function TRenderImageEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

procedure TRenderImageEditor.Edit;
var
  Image: TRenderImage;
begin
  if Component is TRenderImage then
  begin
    Image := Component as TRenderImage;
    if EditRenderImage(Image) then
      Designer.Modified;
  end;
end;

procedure TRenderImageEditor.ExecuteVerb(Index: Integer);
begin
  Edit;
end;

{ TImageStripEditor }

function TImageStripEditor.GetVerb(Index: Integer): string;
begin
  Result := 'Edit images ...';
end;

function TImageStripEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

procedure TImageStripEditor.Edit;
var
  Images: TImageStrip;
begin
  if Component is TImageStrip then
  begin
    Images := Component as TImageStrip;
    if EditImageStrip(Images) then
      Designer.Modified;
  end;
end;

procedure TImageStripEditor.ExecuteVerb(Index: Integer);
begin
  Edit;
end;

end.

