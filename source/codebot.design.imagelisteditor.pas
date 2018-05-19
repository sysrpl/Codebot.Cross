(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified March 2015                                 *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.design.imagelisteditor.txt> }
unit Codebot.Design.ImageListEditor;

{$mode delphi}

interface

uses
  Classes, SysUtils, Graphics, Controls, Forms, StdCtrls,
  LCLType, LCLIntf, ExtDlgs,
  Codebot.System,
  Codebot.Graphics,
  Codebot.Controls.Banner,
  Codebot.Controls.Grids;

{ TImageListEditor }

type
  TImageListEditor = class(TBannerForm)
    OKButton: TButton;
    CancelButton: TButton;
    AddButton: TButton;
    RemoveButton: TButton;
    SaveButton: TButton;
    ClearButton: TButton;
    OpenPictureDialog: TOpenPictureDialog;
    SavePictureDialog: TSavePictureDialog;
    Grid: TContentGrid;
    Images: TImageStrip;
    procedure ImagesChange(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure AddButtonClick(Sender: TObject);
    procedure RemoveButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure ClearButtonClick(Sender: TObject);
    procedure GridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GridMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  private
    FProvider: TImageListGridProvider;
    FDragging: Boolean;
    FDragCoord: TGridCoord;
    FDragIndex: Integer;
    function IndexFromCoord(const Coord: TGridCoord): Integer;
  end;

function EditImageStrip(Images: TImageStrip; Caption: string = ''): Boolean;

implementation

{$R *.lfm}

function EditImageStrip(Images: TImageStrip; Caption: string = ''): Boolean;
var
  F: TImageListEditor;
begin
  F := TImageListEditor.Create(nil);
  try
    F.Images.Assign(Images);
    if Caption <> '' then
      F.Caption := Caption
    else
      F.Caption := 'Editing: ' + StrCompPath(Images);
    Result := F.ShowModal = mrOk;
    if Result then
      Images.Assign(F.Images);
  finally
    F.Free;
  end;
end;

{ TImageListEditor }

procedure TImageListEditor.FormCreate(Sender: TObject);
begin
  Images.OnChange.Add(ImagesChange);
  FProvider := TImageListGridProvider.Create(Self);
  FProvider.ImageList := Images;
  FProvider.Grid := Grid;
end;

procedure TImageListEditor.ImagesChange(Sender: TObject);
begin
end;

procedure TImageListEditor.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    ModalResult := mrCancel;
end;

procedure TImageListEditor.AddButtonClick(Sender: TObject);
begin
  if OpenPictureDialog.Execute then
    Images.LoadRange(OpenPictureDialog.Files);
end;

procedure TImageListEditor.RemoveButtonClick(Sender: TObject);
var
  G: TGridCoord;
  I: Integer;
begin
  G := Grid.Selection;
  I := G.X + G.Y * Grid.ColCount;
  Images.Remove(I);
end;

procedure TImageListEditor.SaveButtonClick(Sender: TObject);
begin
  if Images.Count < 1 then
    Exit;
  if SavePictureDialog.Execute then
    Images.SaveToFile(SavePictureDialog.FileName);
end;

procedure TImageListEditor.ClearButtonClick(Sender: TObject);
begin
  Images.Clear;
end;

function TImageListEditor.IndexFromCoord(const Coord: TGridCoord): Integer;
begin
  if (Coord.X < 0) or (Coord.Y < 0) then
    Exit(-1);
  Result := Coord.X + Grid.ColCount * Coord.Y;
  if Result > Images.Count - 1 then
    Result := -1;
end;

procedure TImageListEditor.GridMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and (ssShift in Shift) then
  begin
    FDragCoord := Grid.CoordFromPoint(X, Y);
    FDragIndex := IndexFromCoord(FDragCoord);
    FDragging := FDragIndex > -1;
  end;
end;

procedure TImageListEditor.GridMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  C: TGridCoord;
  I: Integer;
begin
  if FDragging then
  begin
    C := Grid.CoordFromPoint(X, Y);
    I := IndexFromCoord(C);
    if I < 0 then
      Exit;
    if I = FDragIndex then
      Exit;
    Images.Move(FDragIndex, I);
    FDragIndex := I;
    Grid.InvalidateCoord(FDragCoord.X, FDragCoord.Y);
    FDragCoord := C;
    Grid.InvalidateCoord(FDragCoord.X, FDragCoord.Y);
  end;
end;

procedure TImageListEditor.GridMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FDragging := False;
end;

end.

