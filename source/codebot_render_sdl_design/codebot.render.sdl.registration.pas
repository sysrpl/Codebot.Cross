(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.render.sdl.registration.txt> }
unit Codebot.Render.Sdl.Registration;

{$i ../codebot/codebot.inc}

interface

uses
  Classes, SysUtils, System.UITypes, Controls, Forms, StdCtrls, ButtonPanel,
  ProjectIntf, LazIDEIntf;

const
  { The name of the custom data of a project which says if a release build
    makes a dat file from the assets. Its value is BuildDatOn or nothing. }
  BuildDatName = 'Codebot.BuildDat';
  BuildDatOn = 'True';

{ TRenderProjectDescriptor is the Codebot rendering project of the new project
  dialog. It creates a program which runs a scene in an SDL window. }

type
  TRenderProjectDescriptor = class(TProjectDescriptor)
  private
    FUseKey: Boolean;
  protected
    function DoInitDescriptor: TModalResult; override;
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles(AProject: TLazProject): TModalResult; override;
  end;

procedure Register;

implementation

resourcestring
  SProjectName = 'Codebot rendering project';
  SProjectDescription = 'A program which runs a scene in an SDL window using ' +
    'the Codebot rendering packages, without the LCL.';
  SOptionsCaption = 'New Codebot rendering project';
  SOptionsUseKey = 'Encrypt the assets with a key';
  SOptionsHint = 'With a key the program reads its assets from a dat file, ' +
    'which it builds when started with --build-dat.';

const
  SceneUnitName = 'MainScene';
  SceneClassName = 'TMainScene';

{ Ask if the assets of the new project are encrypted with a key }

function AskOptions(var UseKey: Boolean): Boolean;
var
  Form: TForm;
  Check: TCheckBox;
  Hint: TLabel;
  Buttons: TButtonPanel;
begin
  Form := TForm.CreateNew(nil);
  try
    Form.Caption := SOptionsCaption;
    Form.BorderStyle := bsDialog;
    Form.Position := poScreenCenter;
    Form.ClientWidth := 420;
    Form.ClientHeight := 150;
    Check := TCheckBox.Create(Form);
    Check.Parent := Form;
    Check.Caption := SOptionsUseKey;
    Check.Checked := UseKey;
    Check.SetBounds(16, 16, 388, 24);
    Hint := TLabel.Create(Form);
    Hint.Parent := Form;
    Hint.AutoSize := False;
    Hint.WordWrap := True;
    Hint.Caption := SOptionsHint;
    Hint.SetBounds(16, 48, 388, 48);
    Buttons := TButtonPanel.Create(Form);
    Buttons.Parent := Form;
    Buttons.ShowButtons := [pbOK, pbCancel];
    Result := Form.ShowModal = mrOk;
    if Result then
      UseKey := Check.Checked;
  finally
    Form.Free;
  end;
end;

{ Make up a key of four bytes, which is never zero as zero means no key }

function NewKey: LongWord;
var
  G: TGUID;
begin
  if CreateGUID(G) = 0 then
    Result := G.D1 xor ((LongWord(G.D2) shl 16) or G.D3)
  else
    Result := LongWord(Trunc(Frac(Now) * MSecsPerDay)) xor $9E3779B9;
  if Result = 0 then
    Result := $A1B2C3D4;
end;

function ProgramSource(UseKey: Boolean): string;
const
  LE = LineEnding;
begin
  Result :=
    'program Project1;' + LE +
    LE +
    '{$mode delphi}' + LE +
    LE +
    'uses' + LE +
    '  { Codebot.System is first so threads are supported before other units start }' + LE +
    '  Codebot.System,' + LE +
    '  Codebot.Render.Application' + LE +
    '  { you can add units after this };' + LE +
    LE +
    'begin' + LE +
    '  Application.Title := ''Project1'';' + LE;
  if UseKey then
    Result := Result +
      '  { With a key the assets are read from the dat file beside the program,' + LE +
      '    and from the assets folder when it is not there. The program builds' + LE +
      '    that file and ends when it is started with --build-dat. }' + LE +
      '  Application.AssetKey := $' + IntToHex(NewKey, 8) + ';' + LE;
  Result := Result +
    '  Application.Run(' + SceneClassName + ');' + LE +
    'end.' + LE;
end;

function SceneSource: string;
const
  LE = LineEnding;
begin
  Result :=
    'unit ' + SceneUnitName + ';' + LE +
    LE +
    '{$mode delphi}' + LE +
    LE +
    'interface' + LE +
    LE +
    'uses' + LE +
    '  Classes, SysUtils,' + LE +
    '  Codebot.System,' + LE +
    '  Codebot.Render.Graphics,' + LE +
    '  Codebot.Render.Scenes;' + LE +
    LE +
    '{ ' + SceneClassName + ' is the scene the program starts with. It draws a rose, the curve' + LE +
    '  traced by a point on a circle which rolls around the inside of another. The' + LE +
    '  rose turns, opens and closes, and changes color over time. }' + LE +
    LE +
    'type' + LE +
    '  ' + SceneClassName + ' = class(TScene)' + LE +
    '  public' + LE +
    '    procedure Initialize; override;' + LE +
    '    procedure Finalize; override;' + LE +
    '    procedure Logic; override;' + LE +
    '    procedure Render; override;' + LE +
    '  end;' + LE +
    LE +
    'implementation' + LE +
    LE +
    'procedure ' + SceneClassName + '.Initialize;' + LE +
    'begin' + LE +
    '  inherited Initialize;' + LE +
    '  { Load the fonts, images, and sounds of the scene here. Assets are named' + LE +
    '    by their path below the assets folder, such as ''images/logo.png''. }' + LE +
    'end;' + LE +
    LE +
    'procedure ' + SceneClassName + '.Finalize;' + LE +
    'begin' + LE +
    '  { Free what Initialize created here }' + LE +
    '  inherited Finalize;' + LE +
    'end;' + LE +
    LE +
    'procedure ' + SceneClassName + '.Logic;' + LE +
    'begin' + LE +
    '  inherited Logic;' + LE +
    '  { Update the scene here, once a frame before it is drawn }' + LE +
    'end;' + LE +
    LE +
    'procedure ' + SceneClassName + '.Render;' + LE +
    'const' + LE +
    '  { The rose is drawn as pieces, each of its own color and made of lines }' + LE +
    '  Pieces = 48;' + LE +
    '  Lines = 20;' + LE +
    '  { The small circle is this much of the large one, and rolls around it' + LE +
    '    seven times before the curve meets its start }' + LE +
    '  Ratio = 7 / 16;' + LE +
    '  Turns = 7;' + LE +
    'var' + LE +
    '  Buffer: IBackBuffer;' + LE +
    '  Size, Reach, Spin: Float;' + LE +
    LE +
    '  { Give a point of the rose, where Index counts the lines from its start }' + LE +
    '  procedure Place(Index: Integer; out X, Y: Float);' + LE +
    '  var' + LE +
    '    T, A, B: Float;' + LE +
    '  begin' + LE +
    '    T := Index / (Pieces * Lines) * Turns * 2 * Pi;' + LE +
    '    A := (1 - Ratio) * Cos(T) + Reach * Ratio * Cos((1 - Ratio) / Ratio * T);' + LE +
    '    B := (1 - Ratio) * Sin(T) - Reach * Ratio * Sin((1 - Ratio) / Ratio * T);' + LE +
    '    X := Width / 2 + Size * (A * Cos(Spin) - B * Sin(Spin));' + LE +
    '    Y := Height / 2 + Size * (A * Sin(Spin) + B * Cos(Spin));' + LE +
    '  end;' + LE +
    LE +
    'var' + LE +
    '  X, Y: Float;' + LE +
    '  I, J: Integer;' + LE +
    'begin' + LE +
    '  inherited Render;' + LE +
    '  { The rose fills most of the window, whatever its shape }' + LE +
    '  if Width < Height then' + LE +
    '    Size := Width * 0.45' + LE +
    '  else' + LE +
    '    Size := Height * 0.45;' + LE +
    '  { How far the point is from the middle of the small circle, which opens' + LE +
    '    and closes the petals, and how far the rose has turned }' + LE +
    '  Reach := 0.65 + 0.35 * Sin(Time * 0.4);' + LE +
    '  Spin := Time * 0.15;' + LE +
    '  { Drawing with the canvas is done between two flips of its buffer }' + LE +
    '  Buffer := Canvas as IBackBuffer;' + LE +
    '  Buffer.Flip(Width, Height);' + LE +
    '  try' + LE +
    '    Canvas.Rect(0, 0, Width, Height);' + LE +
    '    Canvas.Fill(NewColorF(0.04, 0.04, 0.07));' + LE +
    '    for I := 0 to Pieces - 1 do' + LE +
    '    begin' + LE +
    '      Place(I * Lines, X, Y);' + LE +
    '      Canvas.MoveTo(X, Y);' + LE +
    '      for J := 1 to Lines do' + LE +
    '      begin' + LE +
    '        Place(I * Lines + J, X, Y);' + LE +
    '        Canvas.LineTo(X, Y);' + LE +
    '      end;' + LE +
    '      { The hue goes once around the rose and moves along it with time }' + LE +
    '      Canvas.Stroke(NewHSL(Frac(Time * 0.1 + I / Pieces), 1, 0.6), 2.5);' + LE +
    '    end;' + LE +
    '  finally' + LE +
    '    Buffer.Flip(Width, Height);' + LE +
    '  end;' + LE +
    'end;' + LE +
    LE +
    'end.' + LE;
end;

{ TRenderProjectDescriptor }

constructor TRenderProjectDescriptor.Create;
begin
  inherited Create;
  Name := 'Codebot rendering project';
  { The program has no forms, so the IDE does not keep statements of the LCL
    application in it }
  Flags := Flags - [pfMainUnitHasCreateFormStatements,
    pfMainUnitHasTitleStatement, pfMainUnitHasScaledStatement];
  FUseKey := True;
end;

function TRenderProjectDescriptor.GetLocalizedName: string;
begin
  Result := SProjectName;
end;

function TRenderProjectDescriptor.GetLocalizedDescription: string;
begin
  Result := SProjectDescription;
end;

function TRenderProjectDescriptor.DoInitDescriptor: TModalResult;
begin
  if AskOptions(FUseKey) then
    Result := mrOk
  else
    Result := mrCancel;
end;

function TRenderProjectDescriptor.InitProject(AProject: TLazProject): TModalResult;
var
  MainFile: TLazProjectFile;
  {$ifdef darwin}
  Command: string;
  {$endif}
begin
  Result := inherited InitProject(AProject);
  if Result <> mrOk then
    Exit;
  MainFile := AProject.CreateProjectFile('project1.lpr');
  MainFile.IsPartOfProject := True;
  AProject.AddFile(MainFile, False);
  AProject.MainFileID := 0;
  AProject.UseAppBundle := True;
  AProject.AddPackageDependency('codebot');
  AProject.AddPackageDependency('codebot_render');
  AProject.AddPackageDependency('codebot_render_sdl');
  AProject.LazCompilerOptions.UnitOutputDirectory := 'lib' + PathDelim + '$(TargetCPU)-$(TargetOS)';
  AProject.LazCompilerOptions.TargetFilename := 'project1';
  AProject.LazCompilerOptions.Win32GraphicApp := True;
  { A release build makes the dat file of a project which has a key }
  if FUseKey then
    AProject.CustomData[BuildDatName] := BuildDatOn;
  {$ifdef darwin}
  { After each build the bundle tool makes the application bundle. A build
    mode with release in its name makes a bundle which stands by itself. }
  Command := '"$PkgDir(codebot_render)../../tools/bundle/bundle" "$(BuildMode)"';
  if FUseKey then
    Command := Command + ' --dat';
  AProject.LazCompilerOptions.ExecuteAfter.Command := Command + ' "$(TargetFile)"';
  {$endif}
  AProject.MainFile.SetSourceText(ProgramSource(FUseKey), True);
end;

function TRenderProjectDescriptor.CreateStartFiles(AProject: TLazProject): TModalResult;
begin
  { The IDE adds the unit to the uses clause of the program }
  Result := LazarusIDE.DoNewEditorFile(FileDescriptorUnit,
    LowerCase(SceneUnitName) + '.pas', SceneSource,
    [nfIsPartOfProject, nfOpenInEditor]);
end;

procedure Register;
begin
  RegisterProjectDescriptor(TRenderProjectDescriptor.Create);
end;

end.
