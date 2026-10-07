(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ Codebot.Render.Application runs scenes in an SDL2 window without the LCL. It
  is based on the application class of Tiny Sim and runs the same TScene
  classes as TSceneController, so a scene written for a TGraphicsBox can also
  run full screen in its own window.

  A program creates no forms. It sets the window properties of the global
  Application and calls Run with a scene class:

    begin
      Application.Title := 'Joint Demo';
      Application.Run(TJointScene);
    end.

  Run returns when the window is closed, Terminate is called, or the scene
  class is set to nil. }
unit Codebot.Render.Application;

{$i ../codebot_render/render.inc}

interface

uses
  SysUtils, Classes,
  Codebot.System,
  Codebot.Platform,
  Codebot.OpenGL,
  Codebot.OpenGL.SDL,
  Codebot.Render.Graphics,
  Codebot.Render.Contexts,
  Codebot.Hardware,
  Codebot.Render.Scenes,
  Codebot.Interop.SDL2;

{ TApplication is the scene host of an SDL2 window.

  Everything runs on the thread which calls Run. The window, OpenGL context,
  render context and Canvas are created first, then each frame:

    1. The scene is created, or replaced when SceneClass has changed
    2. Window events are delivered to the scene as key, text, mouse and wheel
       input
    3. The shared Timer is calculated once and the scene is updated with the
       size of the window
    4. The window buffers are swapped

  When the scene has a StepInterval a step thread calls Step on the scene, as
  TGraphicsBox does. It is stopped before the scene is freed. If Step raises
  an exception the application stops and Run raises it again.

  Scenes get the Canvas, the default font loaded from 'fonts/roboto.ttf' in
  the assets folder, and a Window for the SDL window. The keyboard, clipboard,
  bitmaps, and dialogs are provided by Codebot.Platform.SDL. Each frame calls
  CheckSynchronize, so methods queued with TThread.Queue run between frames,
  and PlatformDispatch, so dialogs deliver their OnClose events. F1 toggles
  full screen and F2 toggles vertical sync unless the scene handles those
  keys. }

type
  EApplicationError = class(Exception);

  TApplication = class(TSceneHost)
  private
    FSDLWindow: PSDL_Window;
    FContext: IOpenGLContext;
    FScene: TScene;
    FSceneClass: TSceneClass;
    FStepThread: TThread;
    FRunning: Boolean;
    FTerminated: Boolean;
    FTitle: string;
    FWidth: Integer;
    FHeight: Integer;
    FFullscreen: Boolean;
    FSizeable: Boolean;
    FDecorated: Boolean;
    FDepthBits: Integer;
    FStencilBits: Integer;
    FMultiSamples: Integer;
    FFrames: Integer;
    FSecond: Double;
    FFilesDropped: StringArray;
    procedure CreateWindow;
    procedure DestroyWindow;
    procedure FreeScene;
    procedure CheckStepping;
    procedure HandleEvent(var Event: TSDL_Event);
    function GetFullscreen: Boolean;
    procedure SetFullscreen(Value: Boolean);
    function GetTitle: string;
    procedure SetTitle(const Value: string);
    procedure SetWidth(Value: Integer);
    procedure SetHeight(Value: Integer);
  protected
    procedure SetVSync(Value: Boolean); override;
  public
    { Do not create an application. Use the global Application function. }
    constructor Create(AOwner: TComponent); override;
    { Run opens the window and runs a scene of the class until the application
      is terminated. Calling Run again while running switches to a new scene
      on the next frame. If no class is given SceneClass is used. }
    procedure Run(SceneClass: TSceneClass = nil);
    { Terminate asks the application to stop after the current frame }
    procedure Terminate;
    { FilesDropped returns true if files were dropped on the window since the
      last frame }
    function FilesDropped(out Files: StringArray): Boolean;
    { The scene running in the window. Setting it while running switches
      scenes on the next frame, and setting it to nil terminates. }
    property SceneClass: TSceneClass read FSceneClass write FSceneClass;
    { The scene currently running }
    property Scene: TScene read FScene;
    { True while Run is running }
    property Running: Boolean read FRunning;
    { The window caption }
    property Title: string read GetTitle write SetTitle;
    { The size of the window. While running it is the current size. }
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
    { Fullscreen covers the desktop with the window }
    property Fullscreen: Boolean read GetFullscreen write SetFullscreen;
    { Sizeable and Decorated are used when the window is created }
    property Sizeable: Boolean read FSizeable write FSizeable;
    property Decorated: Boolean read FDecorated write FDecorated;
    { Buffer options used when the window is created. MultiSamples of 0 or 1
      turns multisampling off. If the window cannot be created with
      multisampling it is created without it. }
    property DepthBits: Integer read FDepthBits write FDepthBits;
    property StencilBits: Integer read FStencilBits write FStencilBits;
    property MultiSamples: Integer read FMultiSamples write FMultiSamples;
  end;

{ Application returns the single application, creating it on first use }

function Application: TApplication;

resourcestring
  SSDLInitFailed = 'SDL could not be started: %s';
  SSDLWindowFailed = 'The window could not be created: %s';
  SSDLContextFailed = 'The OpenGL context could not be created: %s';
  SSDLOpenGLVersion = 'OpenGL %d.%d is required but the hardware provides %s';

implementation

{ Codebot.Platform.SDL is initialized after the units used in the interface,
  so NewBitmapData creates SDL bitmaps in programs using TApplication }

uses
  Math,
  Codebot.Platform.SDL;

{ TStepThread calls Step on a scene at a fixed rate. An exception raised by
  Step stops the thread and is kept in FatalException. }

type
  TStepThread = class(TThread)
  private
    FScene: TScene;
    FInterval: Double;
  protected
    procedure Execute; override;
  public
    constructor Create(Scene: TScene; Interval: Double);
  end;

constructor TStepThread.Create(Scene: TScene; Interval: Double);
begin
  FScene := Scene;
  FInterval := Interval;
  inherited Create(False);
end;

procedure TStepThread.Execute;
const
  { The most time the thread will catch up on, such as after a debugger break }
  MaxCatchUp = 0.25;
var
  Stopwatch: IStopwatch;
  StepTime, Time: Double;
begin
  Stopwatch := StopwatchCreate;
  StepTime := 0;
  while not Terminated do
  begin
    Time := Stopwatch.Calculate;
    if Time - StepTime > MaxCatchUp then
      StepTime := Time - MaxCatchUp;
    while (not Terminated) and (StepTime + FInterval <= Time) do
    begin
      FScene.Step(FInterval);
      StepTime := StepTime + FInterval;
    end;
    Sleep(1);
  end;
end;

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

{ TApplication }

constructor TApplication.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTitle := 'Scene';
  FWidth := 960;
  FHeight := 540;
  FSizeable := True;
  FDecorated := True;
  FVSync := True;
  FDepthBits := 24;
  FStencilBits := 8;
  FMultiSamples := 4;
end;

{ The OpenGL attributes are set before the window is created, as SDL chooses
  the pixel format of a window when it is created }

procedure TApplication.CreateWindow;
var
  Params: TOpenGLParams;
  Flags: Uint32;
begin
  if SDL_Init(SDL_INIT_VIDEO or SDL_INIT_TIMER) < 0 then
    raise EApplicationError.CreateFmt(SSDLInitFailed, [SDL_GetError]);
  Params := TOpenGLParams.Create;
  Params.Depth := FDepthBits;
  Params.Stencil := FStencilBits;
  Params.MultiSampling := FMultiSamples > 1;
  if Params.MultiSampling then
    Params.MultiSamples := FMultiSamples;
  OpenGLSetAttributes(Params);
  Flags := SDL_WINDOW_OPENGL or SDL_WINDOW_SHOWN;
  if FSizeable then
    Flags := Flags or SDL_WINDOW_RESIZABLE;
  if not FDecorated then
    Flags := Flags or SDL_WINDOW_BORDERLESS;
  FSDLWindow := SDL_CreateWindow(PChar(FTitle), SDL_WINDOWPOS_CENTERED,
    SDL_WINDOWPOS_CENTERED, FWidth, FHeight, Flags);
  if (FSDLWindow = nil) and Params.MultiSampling then
  begin
    { Try again without multisampling }
    Params.MultiSampling := False;
    OpenGLSetAttributes(Params);
    FSDLWindow := SDL_CreateWindow(PChar(FTitle), SDL_WINDOWPOS_CENTERED,
      SDL_WINDOWPOS_CENTERED, FWidth, FHeight, Flags);
  end;
  if FSDLWindow = nil then
    raise EApplicationError.CreateFmt(SSDLWindowFailed, [SDL_GetError]);
  if not OpenGLInfo.IsValid then
    raise EApplicationError.CreateFmt(SSDLOpenGLVersion,
      [OpenGLMajor, OpenGLMinor, OpenGLInfo.Version]);
  FContext := OpenGLContextCreate(GLwindow(FSDLWindow), Params);
  if FContext = nil then
    raise EApplicationError.CreateFmt(SSDLContextFailed, [SDL_GetError]);
  FContext.VSync := FVSync;
  FContext.MakeCurrent(True);
  FWindow := NewWindowSDL(FSDLWindow);
  if FFullscreen then
    FWindow.Fullscreen := True;
  FContext.GetSize(FWidth, FHeight);
  SDL_StartTextInput;
end;

procedure TApplication.DestroyWindow;
begin
  FWindow := nil;
  if FContext <> nil then
    FContext.MakeCurrent(False);
  FContext := nil;
  if FSDLWindow <> nil then
    SDL_DestroyWindow(FSDLWindow);
  FSDLWindow := nil;
  SDL_Quit;
end;

procedure TApplication.Run(SceneClass: TSceneClass = nil);
var
  RenderContext: TRenderContext;
  Current: TSceneClass;
  Event: TSDL_Event;
  FontFile: string;
begin
  if SceneClass <> nil then
    FSceneClass := SceneClass;
  { A running application switches scenes on the next frame }
  if FRunning or (FSceneClass = nil) then
    Exit;
  FRunning := True;
  FTerminated := False;
  try
    try
      CreateWindow;
      { The render context becomes Ctx for this thread }
      RenderContext := TRenderContext.Create;
      try
        FCanvas := NewCanvas;
        try
          { Scenes may have no assets folder, so the default font is optional }
          FFont := nil;
          if Ctx.FindAssetFile('fonts/roboto.ttf', FontFile) then
          try
            FFont := FCanvas.LoadFont('default', FontFile);
          except
            FFont := nil;
          end;
          FTimer := StopwatchCreate;
          FTime := 0;
          FFrames := 0;
          FSecond := 0;
          SetSceneHost(Self);
          Current := nil;
          try
            while not FTerminated do
            begin
              FContext.GetSize(FWidth, FHeight);
              { Create the scene, or replace it when the class has changed }
              if FSceneClass <> Current then
              begin
                FreeScene;
                Current := FSceneClass;
                if Current = nil then
                  Break;
                FScene := Current.Create(FWidth, FHeight);
                if FScene.StepInterval > 0 then
                  FStepThread := TStepThread.Create(FScene, FScene.StepInterval);
              end;
              FFilesDropped.Clear;
              while SDL_PollEvent(Event) <> 0 do
                HandleEvent(Event);
              if FTerminated then
                Break;
              { Read the keyboard, mouse and joysticks a scene has used }
              ScanHardware;
              { Run methods queued to the main thread, and deliver the events
                of dialogs which have closed }
              CheckSynchronize;
              PlatformDispatch;
              CheckStepping;
              { The time is calculated once and used for the whole frame }
              FTime := FTimer.Calculate;
              Inc(FFrames);
              if FTime - FSecond >= 1 then
              begin
                FFrameRate := FFrames;
                FFrames := 0;
                FSecond := FTime;
              end;
              FContext.GetSize(FWidth, FHeight);
              FScene.Update(FWidth, FHeight, FTime);
              FContext.Flip;
            end;
          finally
            FreeScene;
            SetSceneHost(nil);
          end;
        finally
          FTimer := nil;
          FFont := nil;
          FCanvas := nil;
        end;
      finally
        RenderContext.Free;
      end;
    finally
      DestroyWindow;
    end;
  finally
    FRunning := False;
  end;
end;

{ The step thread must be stopped before the scene it steps is freed }

procedure TApplication.FreeScene;
begin
  if FStepThread <> nil then
  begin
    FStepThread.Terminate;
    FStepThread.WaitFor;
    FreeAndNil(FStepThread);
  end;
  FreeAndNil(FScene);
end;

{ An exception raised by the step thread stops the application }

procedure TApplication.CheckStepping;
var
  S: string;
begin
  if (FStepThread = nil) or (not FStepThread.Finished) then
    Exit;
  if FStepThread.FatalException is Exception then
    S := Exception(FStepThread.FatalException).Message
  else
    S := 'The step thread stopped';
  raise EApplicationError.Create(S);
end;

procedure TApplication.HandleEvent(var Event: TSDL_Event);
var
  Key: TSceneKeyArgs;
  Text: TSceneTextArgs;
  Mouse: TSceneMouseArgs;
  Wheel: TSceneWheelArgs;
  S: string;
begin
  case Event.type_ of
    SDL_QUIT_EVENT:
      FTerminated := True;
    SDL_KEYDOWN, SDL_KEYUP:
      begin
        Key := Default(TSceneKeyArgs);
        Key.Key := VirtualKey(Event.key.keysym.sym);
        if Key.Key = 0 then
          Exit;
        Key.Shift := ShiftKeys;
        Key.Repeated := Event.key.repeat_ <> 0;
        if Event.type_ = SDL_KEYUP then
        begin
          FScene.DoKeyUp(Key);
          Exit;
        end;
        FScene.DoKeyDown(Key);
        if (not Key.Handled) and (not Key.Repeated) then
          case Key.Key of
            VK_F1: Fullscreen := not Fullscreen;
            VK_F2: VSync := not VSync;
          end;
      end;
    SDL_TEXTINPUT:
      begin
        S := PAnsiChar(@Event.text.text[0]);
        { Control characters such as backspace arrive as key down events }
        if (S = '') or (S[1] < ' ') or (S[1] = #127) then
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
        case Event.type_ of
          SDL_MOUSEBUTTONDOWN: FScene.DoMouseDown(Mouse);
          SDL_MOUSEBUTTONUP: FScene.DoMouseUp(Mouse);
        else
          FScene.DoMouseMove(Mouse);
        end;
      end;
    SDL_MOUSEWHEEL:
      begin
        Wheel := Default(TSceneWheelArgs);
        Wheel.Delta := Event.wheel.y;
        { The wheel turns under the last known mouse position }
        Wheel.X := FMouseX;
        Wheel.Y := FMouseY;
        Wheel.Shift := ShiftKeys;
        FScene.DoMouseWheel(Wheel);
      end;
    SDL_DROPFILE:
      begin
        FFilesDropped.Push(string(Event.drop._file));
        SDL_free(Event.drop._file);
      end;
  end;
end;

procedure TApplication.Terminate;
begin
  FTerminated := True;
end;

function TApplication.FilesDropped(out Files: StringArray): Boolean;
begin
  Files := FFilesDropped;
  Result := Files.Length > 0;
end;

{ While running the title and full screen state belong to the window, which
  scenes can also change }

function TApplication.GetFullscreen: Boolean;
begin
  if FWindow <> nil then
    FFullscreen := FWindow.Fullscreen;
  Result := FFullscreen;
end;

procedure TApplication.SetFullscreen(Value: Boolean);
begin
  FFullscreen := Value;
  if FWindow <> nil then
    FWindow.Fullscreen := Value;
end;

function TApplication.GetTitle: string;
begin
  if FWindow <> nil then
    FTitle := FWindow.Title;
  Result := FTitle;
end;

procedure TApplication.SetTitle(const Value: string);
begin
  FTitle := Value;
  if FWindow <> nil then
    FWindow.Title := Value;
end;

procedure TApplication.SetWidth(Value: Integer);
begin
  if (Value < 1) or (Value = FWidth) then
    Exit;
  FWidth := Value;
  if (FSDLWindow <> nil) and (not Fullscreen) then
    SDL_SetWindowSize(FSDLWindow, FWidth, FHeight);
end;

procedure TApplication.SetHeight(Value: Integer);
begin
  if (Value < 1) or (Value = FHeight) then
    Exit;
  FHeight := Value;
  if (FSDLWindow <> nil) and (not Fullscreen) then
    SDL_SetWindowSize(FSDLWindow, FWidth, FHeight);
end;

procedure TApplication.SetVSync(Value: Boolean);
begin
  if Value = FVSync then
    Exit;
  FVSync := Value;
  if FContext <> nil then
    FContext.VSync := FVSync;
end;

var
  ApplicationInstance: TApplication;

function Application: TApplication;
begin
  if ApplicationInstance = nil then
    ApplicationInstance := TApplication.Create(nil);
  Result := ApplicationInstance;
end;

initialization
  { C libraries such as libxml2, Chipmunk2D, and OpenGL drivers expect floating
    point exceptions to be masked, as the LCL widgetsets do when they start.
    Without this libxml2 raises a division by zero when it first parses. }
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow,
    exUnderflow, exPrecision]);
finalization
  FreeAndNil(ApplicationInstance);
end.
