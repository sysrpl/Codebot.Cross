(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified March 2015                                 *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.design.imagelisteditor.txt> }
unit Codebot.Design.SurfaceBitmapEditor;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Controls, Forms, ExtCtrls, StdCtrls,
  ExtDlgs, LCLType,
  Codebot.System,
  Codebot.Graphics,
  Codebot.Graphics.Types,
  Codebot.Controls.Banner,
  Codebot.Controls.Containers,
  Codebot.Controls.Extras;

{ TSurfaceBitmapEditor }

type
  TSurfaceBitmapEditor = class(TBannerForm)
    BorderContainer: TSizingPanel;
    RenderImage: TRenderImage;
    OKButton: TButton;
    CancelButton: TButton;
    LoadButton: TButton;
    SaveButton: TButton;
    ClearButton: TButton;
    OpenPictureDialog: TOpenPictureDialog;
    SavePictureDialog: TSavePictureDialog;
    procedure BorderContainerRender(Sender: TObject; Surface: ISurface);
    procedure LoadButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure ClearButtonClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  end;

function EditSurfaceBitmap(Bitmap: TSurfaceBitmap): Boolean;
function EditRenderImage(Image: TRenderImage): Boolean;

implementation

{$R *.lfm}

function EditSurfaceBitmap(Bitmap: TSurfaceBitmap): Boolean;
var
  F: TSurfaceBitmapEditor;
begin
  F := TSurfaceBitmapEditor.Create(nil);
  try
    F.RenderImage.Image.Assign(Bitmap);
    F.Caption := 'Editing: TSurfaceBitmap';
    Result := F.ShowModal = mrOk;
    if Result then
      Bitmap.Assign(F.RenderImage.Image);
  finally
    F.Free;
  end;
end;

function EditRenderImage(Image: TRenderImage): Boolean;
var
  F: TSurfaceBitmapEditor;
begin
  F := TSurfaceBitmapEditor.Create(nil);
  try
    F.RenderImage.Image.Assign(Image.Image);
    F.Caption := 'Editing: ' + StrCompPath(Image);
    F.Title.Text := 'Render Image Editor';
    F.TitleSub.Text := 'A render image can fit, stretch, or tile an ' +
      'image depending on its mode';
    Result := F.ShowModal = mrOk;
    if Result then
      Image.Image.Assign(F.RenderImage.Image);
  finally
    F.Free;
  end;
end;

{ TSurfaceBitmapEditor }

procedure TSurfaceBitmapEditor.BorderContainerRender(Sender: TObject;
  Surface: ISurface);
begin
  Surface.FillRect(Brushes.Transparent, BorderContainer.ClientRect);
end;

procedure TSurfaceBitmapEditor.LoadButtonClick(Sender: TObject);
begin
  if OpenPictureDialog.Execute then
    RenderImage.Image.LoadFromFile(OpenPictureDialog.FileName);
end;

procedure TSurfaceBitmapEditor.SaveButtonClick(Sender: TObject);
begin
  if SavePictureDialog.Execute then
    RenderImage.Image.SaveToFile(SavePictureDialog.FileName);
end;

procedure TSurfaceBitmapEditor.ClearButtonClick(Sender: TObject);
begin
  RenderImage.Image.Clear;
end;

procedure TSurfaceBitmapEditor.FormKeyPress(Sender: TObject; var Key: char);
begin
end;

procedure TSurfaceBitmapEditor.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    ModalResult := mrCancel;
end;

end.

