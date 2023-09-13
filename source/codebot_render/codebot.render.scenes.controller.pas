unit Codebot.Render.Scenes.Controller;

{$i ../codebot/codebot.inc}

interface

uses
  SysUtils, Classes, Controls, Forms, OpenGLContext,
  Codebot.System,
  Codebot.Animation,
  Codebot.Render.Contexts,
  Codebot.Render.Scenes;

{ TSceneController }

type
  TSceneController = class(TComponent)
  private
    FTimer: TAnimationTimer;
    FControl: TOpenGLControl;
    FScene: TScene;
    FOnKeyUp: TKeyEvent;
    FSecond: Int64;
    FFrame: Integer;
    FFrameRate: Integer;
    FOnMouseDown: TMouseEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseUp: TMouseEvent;
    FOnPaint: TNotifyEvent;
    procedure Animate(Sender: TObject);
    procedure ControlKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ControlMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ControlMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ControlMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ControlPaint(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure OpenScene(Control: TOpenGLControl; SceneClass: TSceneClass);
    procedure UpdateScene;
    procedure CloseScene;
    property Scene: TScene read FScene;
    property FrameRate: Integer read FFrameRate;
  end;

implementation

{ TSceneController }

constructor TSceneController.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimer := TAnimationTimer.Create(Self);
  FTimer.OnTimer := Animate;
end;

procedure TSceneController.Animate(Sender: TObject);
begin
  if FScene <> nil then
    UpdateScene
  else
    FTimer.Enabled := False;
end;

destructor TSceneController.Destroy;
begin
  FTimer.Free;
  CloseScene;
  inherited Destroy;
end;

procedure TSceneController.OpenScene(Control: TOpenGLControl;
  SceneClass: TSceneClass);
begin
  FTimer.Enabled := False;
  CloseScene;
  if (Control = nil) or (SceneClass = nil) then
    Exit;
  FControl := Control;
  FControl.MakeCurrent;
  FScene := SceneClass.Create(Control.ClientWidth, Control.ClientHeight);
  FControl.ReleaseContext;
  FOnKeyUp := FControl.OnKeyUp;
  FOnMouseDown := FControl.OnMouseDown;
  FOnMouseMove := FControl.OnMouseMove;
  FOnMouseUp := FControl.OnMouseUp;
  FOnPaint := FControl.OnPaint;
  FControl.OnKeyUp := ControlKeyUp;
  FControl.OnMouseDown := ControlMouseDown;
  FControl.OnMouseMove := ControlMouseMove;
  FControl.OnMouseUp := ControlMouseUp;
  FControl.OnPaint := ControlPaint;
  FControl.Invalidate;
  FTimer.Enabled := FScene.Animated;
end;

procedure TSceneController.UpdateScene;
begin
  if (FControl = nil) or (FScene = nil) then
    Exit;
  FControl.Invalidate;
end;

procedure TSceneController.CloseScene;
var
  Obj: TObject;
begin
  if (FControl = nil) or (FScene = nil) then
    Exit;
  FTimer.Enabled := False;
  FControl.OnKeyUp := FOnKeyUp;
  FControl.OnMouseDown := FOnMouseDown;
  FControl.OnMouseMove := FOnMouseMove;
  FControl.OnMouseUp := FOnMouseUp;
  FControl.OnPaint := FOnPaint;
  Obj := FScene;
  FScene := nil;
  FControl.MakeCurrent;
  Obj.Free;
  FControl.ReleaseContext;
  FControl := nil;
end;

procedure TSceneController.ControlKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  S: TShiftKeys;
begin
  if Assigned(FOnKeyUp) then
    FOnKeyUp(Sender, Key, Shift);
  if (FControl = nil) or (FScene = nil) then
    Exit;
  S := [];
  if ssAlt in Shift then
    Include(S, skAlt);
  if ssCtrl in Shift then
    Include(S, skCtrl);
  if ssShift in Shift then
    Include(S, skShift);
  FScene.KeyEvent(Key, S);
  FTimer.Enabled := FScene.Animated;
end;

procedure TSceneController.ControlMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseDown) then
    FOnMouseDown(Sender, Button, Shift, X, Y);
  if (FControl = nil) or (FScene = nil) then
    Exit;
  FScene.MouseEvent(X, Y, maPress);
  FTimer.Enabled := FScene.Animated;
end;

procedure TSceneController.ControlMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if Assigned(FOnMouseMove) then
    FOnMouseMove(Sender, Shift, X, Y);
  if (FControl = nil) or (FScene = nil) then
    Exit;
  FScene.MouseEvent(X, Y, maMove);
  FTimer.Enabled := FScene.Animated;
end;

procedure TSceneController.ControlMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseUp) then
    FOnMouseUp(Sender, Button, Shift, X, Y);
  if (FControl = nil) or (FScene = nil) then
    Exit;
  FScene.MouseEvent(X, Y, maRelease);
  FTimer.Enabled := FScene.Animated;
end;

procedure TSceneController.ControlPaint(Sender: TObject);
var
  I: Int64;
begin
  I := Trunc(TimeQuery);
  if I > FSecond then
  begin
    FSecond := I;
    FFrameRate := FFrame;
    FFrame := 1;
  end
  else
    Inc(FFrame);
  if Assigned(FOnPaint) then
    FOnPaint(Sender);
  if (FControl = nil) or (FScene = nil) then
    Exit;
  FScene.Context.MakeCurrent(True);
  FScene.Update(FControl.Width, FControl.Height, TimeQuery);
  FScene.Context.MakeCurrent(False);
  FControl.SwapBuffers;
  FTimer.Enabled := FScene.Animated;
end;

end.

