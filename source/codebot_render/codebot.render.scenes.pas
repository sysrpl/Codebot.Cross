unit Codebot.Render.Scenes;

{$i render.inc}

interface

uses
  Classes,
  Codebot.System,
  Codebot.Platform,
  Codebot.Hardware,
  Codebot.Render.Contexts,
  Codebot.Render.Graphics;

{ TShiftKeys is the set of modifier keys held down during an input event }

type
  TShiftKeys = set of (skAlt, skCtrl, skShift);
  { TMouseAction is what the mouse did in a mouse event }
  TMouseAction = (maPress, maMove, maRelease);

{ Input arguments sent to scenes. Key codes are the VK_ virtual key codes
  declared in Codebot.Platform. Setting Handled to True stops an event from being passed on. }

  TSceneKeyArgs = record
    { The virtual key code }
    Key: Word;
    { State of the modifier keys }
    Shift: TShiftKeys;
    { True when the key is repeating while held down }
    Repeated: Boolean;
    Handled: Boolean;
  end;

  { The arguments of a mouse button or mouse move event }
  TSceneMouseArgs = record
    { The button causing the event, buttonNone for moves }
    Button: TSceneButton;
    { The mouse position and its change since the last mouse event }
    X, Y, XRel, YRel: Float;
    { State of the modifier keys }
    Shift: TShiftKeys;
    Handled: Boolean;
  end;

  { The arguments of a mouse wheel event }
  TSceneWheelArgs = record
    { The number of wheel notches turned, positive away from the user }
    Delta: Integer;
    { The mouse position }
    X, Y: Float;
    { State of the modifier keys }
    Shift: TShiftKeys;
    Handled: Boolean;
  end;

  { The arguments of a text input event }
  TSceneTextArgs = record
    { The UTF-8 text typed, without control characters }
    Text: string;
    Handled: Boolean;
  end;

  { TSceneKeyEvent is an event for a key going down or up }
  TSceneKeyEvent = procedure(Sender: TObject; var Args: TSceneKeyArgs) of object;
  { TSceneMouseEvent is an event for a mouse button or a mouse move }
  TSceneMouseEvent = procedure(Sender: TObject; var Args: TSceneMouseArgs) of object;

{ TSceneHost runs scenes and provides what they share. A host makes itself
  current on its render thread with SetSceneHost, and scenes created on that
  thread use it. All of its properties are used from the render thread.

  A host calls PlatformDispatch from Codebot.Platform once a frame before
  updating its scene, so dialogs executed by the scene deliver their OnClose
  events on the render thread.

  A program which sets AssetKey keeps its assets in a dat file beside the
  program, when one is there, instead of in an assets folder. Such a program
  makes its dat file when it is started with --build-dat, which a host
  handles with BuildDat before it opens a window. See Codebot.Render.Assets. }

  TSceneHost = class(TComponent)
  protected
    FCanvas: ICanvas;
    FFont: IFont;
    FTimer: IStopwatch;
    FTime: Double;
    FFrameRate: Integer;
    FMouseX: Float;
    FMouseY: Float;
    FWindow: IWindow;
    FVSync: Boolean;
    { The clipboard is PlatformClipboard unless a host overrides it }
    function GetClipboard: string; virtual;
    procedure SetClipboard(const Value: string); virtual;
    { Hosts which own an OpenGL context apply VSync to it }
    function GetVSync: Boolean; virtual;
    procedure SetVSync(Value: Boolean); virtual;
    function GetAssetKey: LongWord;
    procedure SetAssetKey(Value: LongWord);
    { BuildDat returns true if the program was started with --build-dat. The
      dat file has then been built from the assets folder and ExitCode set,
      and the host must end the program without running a scene. A host
      calls this before it creates a window. }
    function BuildDat: Boolean;
  public
    { The canvas used for vector graphics, sharing Ctx with the scenes }
    property Canvas: ICanvas read FCanvas;
    { The default font, which may be nil if it could not be loaded }
    property Font: IFont read FFont;
    { The timer shared by all scenes, calculated once at the start of a frame }
    property Timer: IStopwatch read FTimer;
    { The time of the current frame }
    property Time: Double read FTime;
    { Frames rendered in the last second }
    property FrameRate: Integer read FFrameRate;
    { The position of the last mouse event }
    property MouseX: Float read FMouseX;
    property MouseY: Float read FMouseY;
    { Text on the clipboard, read and written from the render thread }
    property Clipboard: string read GetClipboard write SetClipboard;
    { The window showing the scenes, which may be nil if the host has none }
    property Window: IWindow read FWindow;
    { When VSync is True each frame waits for the vertical sync of the
      display. It is used from the render thread. }
    property VSync: Boolean read GetVSync write SetVSync;
    { The key of the dat file holding the assets of the program, which is
      four bytes such as $A1B2C3D4. It is the same for every host, and is
      set before a scene is run. Zero, the default, means there is no key,
      and assets are then files in the assets folder. }
    property AssetKey: LongWord read GetAssetKey write SetAssetKey;
  end;

{ The scene host which is current on the calling thread }

function SceneHost: TSceneHost;
{ Make a scene host current on the calling thread }
procedure SetSceneHost(Host: TSceneHost);

{ TScene is the base class of everything a host shows. Override Initialize,
  Logic, and Render to make a scene. }

type
  TScene = class
  private
    FAnimated: Boolean;
    FHost: TSceneHost;
    FContext: TRenderContext;
    FBaseTime: Double;
    FTime: Double;
    FWidth: Integer;
    FHeight: Integer;
    FLogicPhase: Boolean;
    FLogicTime: Double;
    function GetTime: Double;
    function GetCanvas: ICanvas;
    function GetFont: IFont;
  protected
    { Resize sets the viewport but you might use it to change the perspective matrix }
    procedure Resize; virtual;
  public
    { Create a scene of a size using the scene host of the calling thread }
    constructor Create(Width, Height: Integer); virtual;
    destructor Destroy; override;
    { Name of the scene }
    function Name: string; virtual;
    { KeyEvent is fired when a keyboard action occurs }
    procedure KeyEvent(KeyCode: Integer; Shift: TShiftKeys); virtual;
    { MouseEvent is fired when a mouse action occurs }
    procedure MouseEvent(X, Y: Integer; Action: TMouseAction); virtual;
    { Input events sent by the host. By default key up calls KeyEvent and the
      mouse button and move events call MouseEvent. }
    procedure DoKeyDown(var Args: TSceneKeyArgs); virtual;
    procedure DoKeyUp(var Args: TSceneKeyArgs); virtual;
    procedure DoMouseDown(var Args: TSceneMouseArgs); virtual;
    procedure DoMouseMove(var Args: TSceneMouseArgs); virtual;
    procedure DoMouseUp(var Args: TSceneMouseArgs); virtual;
    procedure DoMouseWheel(var Args: TSceneWheelArgs); virtual;
    procedure DoTextInput(var Args: TSceneTextArgs); virtual;
    { Update causes a and Logic, Resize, and Render methods to be invoked in
      that order }
    procedure Update(Width, Height: Integer; Time: Double);
    { Initialize has an active context and is called during Create }
    procedure Initialize; virtual;
    { Finalize has an active context and is called during Destroy }
    procedure Finalize; virtual;
    { Logic phase allows you to calculate logic and has no current context }
    procedure Logic; virtual;
    { Render phase allows you render and has a current context }
    procedure Render; virtual;
    { StepInterval is the time in seconds between calls to Step made by a step
      thread the host runs for the scene, or 0 when the scene does not need a
      step thread. It is read once after the scene is created. }
    function StepInterval: Double; virtual;
    { Step is called on the step thread at a fixed rate when StepInterval is
      greater than 0. It runs at the same time as Logic and Render, so data
      they share must be protected. It has no current context. }
    procedure Step(DeltaTime: Double); virtual;
    { When Animated is True update is called continously }
    property Animated: Boolean read FAnimated write FAnimated;
    { Context associated with the scene }
    property Context: TRenderContext read FContext;
    { The host running the scene, or nil if the scene was created without one }
    property Host: TSceneHost read FHost;
    { The canvas and default font of the host }
    property Canvas: ICanvas read GetCanvas;
    property Font: IFont read GetFont;
    { Time that can be used during Logic or Render }
    property Time: Double read GetTime;
    { Width is updated immediately before Render }
    property Width: Integer read FWidth;
    { Height is updated immediately before Render }
    property Height: Integer read FHeight;
  end;

  { The class of a scene }
  TSceneClass = class of TScene;

const
  SceneLogicStep = Double(1 / 100);

implementation

uses
  Codebot.Render.Assets;

threadvar
  CurrentHost: TSceneHost;

function SceneHost: TSceneHost;
begin
  Result := CurrentHost;
end;

procedure SetSceneHost(Host: TSceneHost);
begin
  CurrentHost := Host;
end;

{ TSceneHost }

function TSceneHost.GetAssetKey: LongWord;
begin
  Result := Codebot.Render.Assets.AssetKey;
end;

procedure TSceneHost.SetAssetKey(Value: LongWord);
begin
  Codebot.Render.Assets.AssetKey := Value;
end;

function TSceneHost.BuildDat: Boolean;
begin
  Result := AssetBuildRequested;
end;

function TSceneHost.GetClipboard: string;
begin
  Result := PlatformClipboard.Text;
end;

procedure TSceneHost.SetClipboard(const Value: string);
begin
  PlatformClipboard.Text := Value;
end;

function TSceneHost.GetVSync: Boolean;
begin
  Result := FVSync;
end;

procedure TSceneHost.SetVSync(Value: Boolean);
begin
  FVSync := Value;
end;

{ TScene }

constructor TScene.Create(Width, Height: Integer);
begin
  inherited Create;
  FHost := CurrentHost;
  FAnimated := True;
  FWidth := Width;
  FHeight := Height;
  { Scenes use the render context of the render thread }
  FContext := Ctx;
  FContext.SetViewport(0, 0, Width, Height);
  Initialize;
  Resize;
end;

destructor TScene.Destroy;
begin
  Finalize;
  inherited Destroy;
end;

procedure TScene.Resize;
begin
  FContext.SetViewport(0, 0, FWidth, FHeight);
end;

function TScene.Name: string;
begin
  Result := 'Empty Scene';
end;

procedure TScene.KeyEvent(KeyCode: Integer; Shift: TShiftKeys);
begin
end;

procedure TScene.MouseEvent(X, Y: Integer; Action: TMouseAction);
begin
end;

procedure TScene.DoKeyDown(var Args: TSceneKeyArgs);
begin
end;

procedure TScene.DoKeyUp(var Args: TSceneKeyArgs);
begin
  KeyEvent(Args.Key, Args.Shift);
end;

procedure TScene.DoMouseDown(var Args: TSceneMouseArgs);
begin
  MouseEvent(Round(Args.X), Round(Args.Y), maPress);
end;

procedure TScene.DoMouseMove(var Args: TSceneMouseArgs);
begin
  MouseEvent(Round(Args.X), Round(Args.Y), maMove);
end;

procedure TScene.DoMouseUp(var Args: TSceneMouseArgs);
begin
  MouseEvent(Round(Args.X), Round(Args.Y), maRelease);
end;

procedure TScene.DoMouseWheel(var Args: TSceneWheelArgs);
begin
end;

procedure TScene.DoTextInput(var Args: TSceneTextArgs);
begin
end;

function TScene.GetCanvas: ICanvas;
begin
  if FHost <> nil then
    Result := FHost.Canvas
  else
    Result := nil;
end;

function TScene.GetFont: IFont;
begin
  if FHost <> nil then
    Result := FHost.Font
  else
    Result := nil;
end;

procedure TScene.Update(Width, Height: Integer; Time: Double);
begin
  if FBaseTime = 0 then
    FBaseTime := Time;
  FTime := Time - FBaseTime;
  FLogicPhase := True;
  if FAnimated then
    { Logic runs once for every whole step of time that has passed }
    while FLogicTime < FTime do
    begin
      FLogicTime := FLogicTime + SceneLogicStep;
      Logic;
    end
  else
  begin
    FLogicTime := FTime;
    Logic;
  end;
  FLogicPhase := False;
  if (Width <> FWidth) or (FHeight <> Height) then
  begin
    FWidth := Width;
    FHeight := Height;
    Resize;
  end;
  Render;
end;

procedure TScene.Logic;
begin
end;

function TScene.StepInterval: Double;
begin
  Result := 0;
end;

procedure TScene.Step(DeltaTime: Double);
begin
end;

procedure TScene.Initialize;
begin
  FContext.SetClearColor(0, 0, 0, 0);
end;

procedure TScene.Finalize;
begin
end;

procedure TScene.Render;
begin
  FContext.Clear;
  FContext.Identity;
end;

function TScene.GetTime: Double;
begin
  if FLogicPhase then
    Result := FLogicTime
  else
    Result := FTime;
end;

end.

