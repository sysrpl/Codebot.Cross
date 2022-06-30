(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified September 2013                             *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.graphics.design.registration.txt> }
unit Codebot.Design.Editors;

{$i codebot.inc}

interface

uses
  SysUtils, Classes, Graphics, PropEdits, ComponentEditors, ProjectIntf,
  TypInfo,
  Codebot.Graphics,
  Codebot.Graphics.Types,
  Codebot.Controls.Containers,
  Codebot.Controls.Extras,
  Codebot.Design.SurfaceBitmapEditor,
  Codebot.Design.ImageListEditor;

{ TImageStripIndexPropertyEditor }

type
  TImageStripIndexPropertyEditor = class(TIntegerPropertyEditor)
  private
    FImages: TImageStrip;
    FFont: IFont;
    FBrush: IBrush;
  public
    procedure Activate; override;
    procedure Deactivate; override;
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure ListDrawValue(const AValue: string; Index: Integer;
      ACanvas: TCanvas; const ARect: TRect; AState: TPropEditDrawState); override;
    procedure ListMeasureHeight(const AValue: string; Index: Integer;
      ACanvas:TCanvas; var AHeight: Integer); override;
  end;

{ TThemeNamePropertyEditor }

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

{ TSizingPanelEditor }

  TSizingPanelEditor = class(TComponentEditor)
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
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

{ TImageStripIndexPropertyEditor }

procedure TImageStripIndexPropertyEditor.Activate;
var
  Obj: TObject;
begin
  inherited Activate;
  Obj := GetComponent(0);
  Obj := GetObjectProp(Obj, 'Images');
  if Obj is TImageStrip then
    FImages := Obj as TImageStrip;
end;

procedure TImageStripIndexPropertyEditor.Deactivate;
begin
  FImages := nil;
  FFont := nil;
  FBrush := nil;
  inherited Deactivate;
end;

function TImageStripIndexPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paNotNestable, paCustomDrawn];
end;

procedure TImageStripIndexPropertyEditor.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  Proc('-1');
  if FImages <> nil then
    for I := 0 to FImages.Count - 1 do
      Proc(IntToStr(I));
end;

procedure TImageStripIndexPropertyEditor.ListDrawValue(const AValue: string; Index: Integer;
  ACanvas: TCanvas; const ARect: TRect; AState: TPropEditDrawState);
var
  Surface: ISurface;
  C: TColorB;
  R: TRectI;
begin
  if (FImages <> nil) and (pedsInComboList in AState) and not (pedsInEdit in AState) then
  begin
    Surface := NewSurface(ACanvas);
    R := ARect;
    if pedsSelected in AState then
    begin
      C := clHighlight;
      Surface.FillRect(NewBrush(C.Blend(clWindow, 0.3)), R);
      Surface.StrokeRect(NewPen(C), R);
    end
    else
      Surface.FillRect(FBrush, R);
    if Index < 1 then
    begin
      R.Left := R.Height;
      R.Inflate(-6, 0);
      Surface.TextOut(FFont, 'No image', R, drLeft);
    end
    else
    begin
      FImages.Draw(Surface, Index - 1, 2, R.MidPoint.Y - FImages.Size div 2);
      R.Left := R.Height;
      R.Inflate(-6, 0);
      Surface.TextOut(FFont, IntToStr(Index - 1), R, drLeft);
    end;
  end
  else
    inherited ListDrawValue(AValue, Index, ACanvas, ARect, AState);
end;

procedure TImageStripIndexPropertyEditor.ListMeasureHeight(
  const AValue: string; Index: Integer; ACanvas: TCanvas; var AHeight: Integer);
begin
  if (Index > -1) and (FImages <> nil) then
  begin
    AHeight := FImages.Size + 6;
    if AHeight < 20 then
      AHeight := 20;
    if FFont = nil then
    begin
      FFont := NewFont(ACanvas.Font);
      FFont.Color := clWindowText;
    end;
    if FBrush = nil then
      FBrush := NewBrush(clWindow);
  end;
end;

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

{ TSizingPanelEditor }

function TSizingPanelEditor.GetVerb(Index: Integer): string;
begin
  Result := 'Arrange controls';
end;

function TSizingPanelEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

procedure TSizingPanelEditor.Edit;
var
  Panel: TSizingPanel;
begin
  if Component is TSizingPanel then
  begin
    Panel := Component as TSizingPanel;
    Panel.ArrangeControls;
    Designer.Modified;
  end;
end;

procedure TSizingPanelEditor.ExecuteVerb(Index: Integer);
begin
  Edit;
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

