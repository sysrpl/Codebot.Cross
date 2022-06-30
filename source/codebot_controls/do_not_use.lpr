program do_not_use;

{$mode delphi}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  Codebot.Design.ImageListEditor,
  Codebot.Design.SurfaceBitmapEditor
  { you can add units after this };

{$R *.res}

var
  ImageListEditor: TImageListEditor;
  SurfaceBitmapEditor: TSurfaceBitmapEditor;

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TImageListEditor, ImageListEditor);
  Application.CreateForm(TSurfaceBitmapEditor, SurfaceBitmapEditor);
  Application.Run;
end.

