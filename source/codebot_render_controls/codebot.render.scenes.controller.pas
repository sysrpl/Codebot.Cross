unit Codebot.Render.Scenes.Controller;

{$i ../codebot_render/render.inc}

interface

uses
  SysUtils, Classes, Types, Controls, LCLType, SyncObjs,
  Codebot.System,
  Codebot.Platform,
  Codebot.Platform.LCL,
  Codebot.Interop.SDL2,
  Codebot.Render.Graphics,
  Codebot.Render.Contexts,
  Codebot.Hardware,
  Codebot.Render.Scenes,
  Codebot.Render.Controls;

{ TSceneController runs a scene in a TGraphicsBox.

  The controller hooks the render events of the box, calling any handlers
  that were assigned before it. Scenes are created, updated and destroyed
  on the render thread with the context current:

    1. OpenScene asks for a scene class from the main thread
    2. The next OnRender frees the previous scene and creates the new one
    3. The box reads the events of SDL and calls OnInput for each one, which
       delivers key and mouse input to the scene
    4. Every OnRender calculates the shared Timer once, then calls Update
       with the frame time
    5. OnRenderStop or CloseScene frees the scene

  When the scene has a StepInterval the box runs a step thread which calls Step
  on the scene. The step thread is stopped before the scene is freed.

  The controller is the scene host. It is current on the render thread while
  the box renders, giving scenes the box Canvas and a default font loaded
  from 'fonts/roboto.ttf' in the assets folder.

  Key and mouse events come from the SDL window of the box, the same as
  they do in an SDL application, and arrive on the render thread, so scene
  methods only ever run on that thread one at a time. The box is given focus
  when it is clicked so it receives key events. The keyboard, mouse and
  joysticks in Codebot.Hardware are scanned by the box before each frame.

  Scenes use the clipboard and dialogs from Codebot.Platform.LCL, which reach
  the main thread themselves. Window is the form holding the box. Dialogs
  executed by a scene deliver OnClose on the render thread when the
  controller calls PlatformDispatch at the start of each frame.

  The box must not be rendering when OpenScene first attaches it or when the
  controller is destroyed. Both are the case when the controller is created
  in a form's OnCreate and owned by the same form, because a form destroys
  its window, which stops the render thread, before it frees its components. }

type
  TSceneController = class(TSceneHost)
  private
    FBox: TGraphicsBox;
    FLock: TCriticalSection;
    { Guarded by FLock }
    FSceneClass: TSceneClass;
    FChanged: Boolean;
    { Used only on the render thread }
    FScene: TScene;
    FFrames: Integer;
    FSecond: Double;
    { The handlers which were assigned to the box before it was hooked }
    FOnRenderStart: TNotifyEvent;
    FOnRender: TGraphicsRenderEvent;
    FOnRenderStop: TNotifyEvent;
    FOnInput: TGraphicsInputEvent;
    procedure Hook;
    procedure Unhook;
    procedure BoxInput(Sender: TObject; var Event: TSDL_Event);
    procedure BoxRenderStart(Sender: TObject);
    procedure BoxRender(Sender: TObject; Width, Height: Integer);
    procedure BoxRenderStop(Sender: TObject);
    procedure SceneStep(Sender: TObject; DeltaTime: Double);
    procedure FreeScene;
  protected
    function GetVSync: Boolean; override;
    procedure SetVSync(Value: Boolean); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { OpenScene attaches the box if needed and runs a new scene of the class
      in it. It can be called again to switch scenes. }
    procedure OpenScene(Box: TGraphicsBox; SceneClass: TSceneClass);
    { CloseScene frees the running scene on the next frame }
    procedure CloseScene;
    { The box the controller is attached to }
    property Box: TGraphicsBox read FBox;
  end;

implementation

uses
  Codebot.Render.Assets;

{ Input conversion }

function ShiftKeys: TShiftKeys;
var
  M: Uint32;
begin
  Result := [];
  M := SDL_GetModState;
  if M and KMOD_ALT <> 0 then
    Include(Result, skAlt);
  if M and KMOD_CTRL <> 0 then
    Include(Result, skCtrl);
  if M and KMOD_SHIFT <> 0 then
    Include(Result, skShift);
end;

function SceneButton(Button: Uint8): TSceneButton;
begin
  case Button of
    SDL_BUTTON_LEFT: Result := buttonLeft;
    SDL_BUTTON_RIGHT: Result := buttonRight;
    SDL_BUTTON_MIDDLE: Result := buttonMiddle;
    SDL_BUTTON_X1: Result := buttonExtra1;
    SDL_BUTTON_X2: Result := buttonExtra2;
  else
    Result := buttonNone;
  end;
end;

{ TSceneController }

constructor TSceneController.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLock := TCriticalSection.Create;
end;

destructor TSceneController.Destroy;
begin
  if FBox <> nil then
  begin
    Unhook;
    FBox.RemoveFreeNotification(Self);
    FBox := nil;
  end;
  FLock.Free;
  inherited Destroy;
end;

procedure TSceneController.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  { A box being freed has already stopped rendering, and its events go with it }
  if (Operation = opRemove) and (AComponent = FBox) then
    FBox := nil;
end;

procedure TSceneController.Hook;
begin
  FOnRenderStart := FBox.OnRenderStart;
  FOnRender := FBox.OnRender;
  FOnRenderStop := FBox.OnRenderStop;
  FOnInput := FBox.OnInput;
  FBox.OnRenderStart := BoxRenderStart;
  FBox.OnRender := BoxRender;
  FBox.OnRenderStop := BoxRenderStop;
  FBox.OnInput := BoxInput;
end;

procedure TSceneController.Unhook;
begin
  FBox.OnRenderStart := FOnRenderStart;
  FBox.OnRender := FOnRender;
  FBox.OnRenderStop := FOnRenderStop;
  FBox.OnInput := FOnInput;
end;

procedure TSceneController.OpenScene(Box: TGraphicsBox; SceneClass: TSceneClass);
begin
  { Started with --build-dat the program builds its dat file and ends
    without showing a scene }
  if BuildDat then
    Halt(ExitCode);
  if Box <> FBox then
  begin
    { The render thread reads the box events, so they are only changed while
      the box is not rendering }
    if ((FBox <> nil) and FBox.Rendering) or ((Box <> nil) and Box.Rendering) then
      raise EInvalidOperation.Create('A scene controller cannot change its box while rendering');
    if FBox <> nil then
    begin
      Unhook;
      FBox.RemoveFreeNotification(Self);
    end;
    FBox := Box;
    if FBox <> nil then
    begin
      FBox.FreeNotification(Self);
      Hook;
    end;
  end;
  FLock.Enter;
  try
    FSceneClass := SceneClass;
    FChanged := True;
  finally
    FLock.Leave;
  end;
end;

procedure TSceneController.CloseScene;
begin
  FLock.Enter;
  try
    FSceneClass := nil;
    FChanged := True;
  finally
    FLock.Leave;
  end;
end;

{ VSync belongs to the context of the box, which is current on the render
  thread }

function TSceneController.GetVSync: Boolean;
begin
  if (FBox <> nil) and FBox.Rendering and (FBox.Context <> nil) then
    FVSync := FBox.Context.VSync;
  Result := FVSync;
end;

procedure TSceneController.SetVSync(Value: Boolean);
begin
  FVSync := Value;
  if (FBox <> nil) and FBox.Rendering and (FBox.Context <> nil) then
    FBox.Context.VSync := Value;
end;

procedure TSceneController.BoxRenderStart(Sender: TObject);
const
  DefaultFontAsset = 'fonts/roboto.ttf';
begin
  if Assigned(FOnRenderStart) then
    FOnRenderStart(Sender);
  FCanvas := FBox.Canvas;
  { Scenes may have no assets folder, so the default font is optional }
  FFont := nil;
  if AssetExists(DefaultFontAsset) then
  try
    FFont := FCanvas.LoadFontAsset('default', DefaultFontAsset);
  except
    FFont := nil;
  end;
  FTimer := StopwatchCreate;
  FTime := 0;
  FFrames := 0;
  FSecond := 0;
  FWindow := NewWindowLCL(FBox);
  SetSceneHost(Self);
  { Create the requested scene on the first frame }
  FLock.Enter;
  try
    FChanged := True;
  finally
    FLock.Leave;
  end;
end;

{ BoxInput is called by the box on the render thread for each event of its
  SDL window, before BoxRender. Input which arrives before the first frame
  has made a scene is dropped. }

procedure TSceneController.BoxInput(Sender: TObject; var Event: TSDL_Event);
var
  Key: TSceneKeyArgs;
  Text: TSceneTextArgs;
  Mouse: TSceneMouseArgs;
  Wheel: TSceneWheelArgs;
  S: string;
begin
  if Assigned(FOnInput) then
    FOnInput(Sender, Event);
  case Event.type_ of
    SDL_KEYDOWN, SDL_KEYUP:
      begin
        Key := Default(TSceneKeyArgs);
        Key.Key := VirtualKey(Event.key.keysym.sym);
        if (Key.Key = 0) or (FScene = nil) then
          Exit;
        Key.Shift := ShiftKeys;
        Key.Repeated := Event.key.repeat_ <> 0;
        if Event.type_ = SDL_KEYUP then
          FScene.DoKeyUp(Key)
        else
          FScene.DoKeyDown(Key);
      end;
    SDL_TEXTINPUT:
      begin
        S := PAnsiChar(@Event.text.text[0]);
        { Control characters such as backspace arrive as key down events }
        if (S = '') or (S[1] < ' ') or (S[1] = #127) or (FScene = nil) then
          Exit;
        Text := Default(TSceneTextArgs);
        Text.Text := S;
        FScene.DoTextInput(Text);
      end;
    SDL_MOUSEBUTTONDOWN, SDL_MOUSEBUTTONUP, SDL_MOUSEMOTION:
      begin
        Mouse := Default(TSceneMouseArgs);
        if Event.type_ = SDL_MOUSEMOTION then
        begin
          Mouse.Button := buttonNone;
          Mouse.X := Event.motion.x;
          Mouse.Y := Event.motion.y;
        end
        else
        begin
          Mouse.Button := SceneButton(Event.button.button);
          Mouse.X := Event.button.x;
          Mouse.Y := Event.button.y;
        end;
        { Track the mouse for scenes and widgets which ask where it is }
        Mouse.XRel := Mouse.X - FMouseX;
        Mouse.YRel := Mouse.Y - FMouseY;
        FMouseX := Mouse.X;
        FMouseY := Mouse.Y;
        Mouse.Shift := ShiftKeys;
        if FScene = nil then
          Exit;
        case Event.type_ of
          SDL_MOUSEBUTTONDOWN: FScene.DoMouseDown(Mouse);
          SDL_MOUSEBUTTONUP: FScene.DoMouseUp(Mouse);
        else
          FScene.DoMouseMove(Mouse);
        end;
      end;
    SDL_MOUSEWHEEL:
      begin
        if FScene = nil then
          Exit;
        Wheel := Default(TSceneWheelArgs);
        Wheel.Delta := Event.wheel.y;
        { The wheel turns under the last known mouse position }
        Wheel.X := FMouseX;
        Wheel.Y := FMouseY;
        Wheel.Shift := ShiftKeys;
        FScene.DoMouseWheel(Wheel);
      end;
  end;
end;

procedure TSceneController.BoxRender(Sender: TObject; Width, Height: Integer);
var
  SceneClass: TSceneClass;
  Changed: Boolean;
begin
  { The time is calculated once and used for the whole frame }
  FTime := FTimer.Calculate;
  Inc(FFrames);
  if FTime - FSecond >= 1 then
  begin
    FFrameRate := FFrames;
    FFrames := 0;
    FSecond := FTime;
  end;
  FLock.Enter;
  try
    SceneClass := FSceneClass;
    Changed := FChanged;
    FChanged := False;
  finally
    FLock.Leave;
  end;
  { Dialogs which closed on the main thread deliver their events here }
  PlatformDispatch;
  if Changed then
  begin
    FreeScene;
    if SceneClass <> nil then
    begin
      FScene := SceneClass.Create(Width, Height);
      if FScene.StepInterval > 0 then
        FBox.StartStepping(FScene.StepInterval, SceneStep);
    end;
  end;
  if FScene <> nil then
    FScene.Update(Width, Height, FTime);
  { Handlers assigned before the controller draw on top of the scene }
  if Assigned(FOnRender) then
    FOnRender(Sender, Width, Height);
end;

{ The step thread must be stopped before the scene it steps is freed }

procedure TSceneController.FreeScene;
begin
  if FScene = nil then
    Exit;
  FBox.StopStepping;
  FreeAndNil(FScene);
end;

procedure TSceneController.SceneStep(Sender: TObject; DeltaTime: Double);
begin
  FScene.Step(DeltaTime);
end;

procedure TSceneController.BoxRenderStop(Sender: TObject);
begin
  FreeScene;
  SetSceneHost(nil);
  FWindow := nil;
  FTimer := nil;
  FFont := nil;
  FCanvas := nil;
  if Assigned(FOnRenderStop) then
    FOnRenderStop(Sender);
end;

end.
