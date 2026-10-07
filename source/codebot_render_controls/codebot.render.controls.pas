(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified July 2022                                  *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.render.controls.txt> }
unit Codebot.Render.Controls;

{$i ../codebot_render/render.inc}

interface

uses
  Classes, SysUtils, Graphics, Controls, LMessages, LCLType,
  Codebot.OpenGL,
  Codebot.Interop.SDL2,
  Codebot.Render.Contexts,
  Codebot.Render.Graphics;

{ TGraphicsBoxOptions control the options used when creating an OpenGL context.
  Changes to the options are only used once immediately before creation of a
  context and have no effect afterwards. }

type
  TGraphicsBoxOptions = class(TPersistent)
  private
    FDepthBits: Integer;
    FStencilBits: Integer;
    FMultiSampling: Boolean;
    FMultiSamples: Integer;
    procedure SetDepthBits(Value: Integer);
    procedure SetStencilBits(Value: Integer);
    procedure SetMultiSamples(Value: Integer);
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    { DepthBits determines the number of bits used to store pixel Z depth.
      Acceptable values are 16, 24, and 32. }
    property DepthBits: Integer read FDepthBits write SetDepthBits default 24;
    { StencilBits determines the number of bits used to store stencil buffer data.
      Acceptable values are 0, 1, and 8. }
    property StencilBits: Integer read FStencilBits write SetStencilBits default 8;
    { MultiSampling allows polygon edges to be smoothed with anti aliasing }
    property MultiSampling: Boolean read FMultiSampling write FMultiSampling default True;
    { MultiSamples controls how many samples are taken along polygon edges when smoothing.
      Acceptable values are 1, 2, 4, 8 and 16. Higher values greatly effect performance. }
    property MultiSamples: Integer read FMultiSamples write SetMultiSamples default 4;
  end;

{ TGraphicsRenderEvent is called by the render thread with the context current
  and the size of the rendering area in pixels }

  TGraphicsRenderEvent = procedure(Sender: TObject; Width, Height: Integer) of object;

{ TGraphicsStepEvent is called by the step thread with the time of the step in
  seconds }

  TGraphicsStepEvent = procedure(Sender: TObject; DeltaTime: Double) of object;

{ TGraphicsInputEvent is called by the render thread for each key, text,
  mouse and wheel event SDL read for the control }

  TGraphicsInputEvent = procedure(Sender: TObject; var Event: TSDL_Event) of object;

{ TGraphicsBox is a windowed control for hosting OpenGL graphics.

  When its window is first shown the control creates an SDL window which
  fills it, and SDL creates the OpenGL context for that window. The SDL
  window covers the control, so the keyboard and mouse over it are read by
  SDL and not by the LCL. Before each frame the render thread reads the
  events of SDL and passes them on in three ways:

    The LCL key and mouse events of the control, such as OnKeyDown and
    OnMouseMove, are called on the main thread. The render thread waits for
    them to finish before it goes on, so they see the same input as the
    frame which follows. It only waits when one of the events has a handler.

    OnInput is called on the render thread with each SDL event.

    Keyboard, Mouse and Joysticks in Codebot.Hardware are scanned.

  OnMouseEnter and OnMouseLeave do not occur.

  The SDL window is given the keyboard focus when the control is given the
  focus or is clicked, and gives it back to the form when the control loses
  the focus. While it has the focus the LCL does not see keys, so the tab
  key and the shortcuts of the form are not processed.

  The control runs its own render thread once its window is first shown. All
  rendering happens on that thread with the context current:

    1. The thread makes the context current and creates a TRenderContext,
       which Ctx returns on that thread, and the Canvas
    2. OnRenderStart is called to create OpenGL resources
    3. The events of SDL are read, the hardware is scanned, OnInput is
       called for each event, then OnRender is called, and the buffers are
       flipped. This repeats.
    4. OnRenderStop is called to destroy OpenGL resources. It is only called
       if OnRenderStart completed.
    5. The Canvas and the render context are destroyed, freeing any objects
       the render context manages, and the context is released

  The thread is stopped and waited for before the window handle is destroyed.

  Because the events run on the render thread they must not access LCL
  controls directly. Use TThread.Queue or TThread.Synchronize to reach the
  main thread.

  If any of the events raise an exception the thread stops, Failed becomes
  True, ErrorMessage holds the message, and OnFailed is called on the main
  thread.

  The control can also run a step thread with StartStepping, which calls a
  step event at a fixed rate apart from rendering, such as to simulate physics.
  The step thread is stopped before the render thread when the window handle
  is destroyed, and an exception raised by the step event stops it in the same
  way as the render thread. }

  TGraphicsBox = class(TWinControl)
  private
    FCanvas: TControlCanvas;
    FContext: IOpenGLContext;
    FChild: PSDL_Window;
    FChildID: LongWord;
    FEvents: array of TSDL_Event;
    FEventLock: TRTLCriticalSection;
    FFrameEvents: array of TSDL_Event;
    FRenderCanvas: ICanvas;
    FThread: TThread;
    FStepThread: TThread;
    FLogo: TBitmap;
    FRendering: Boolean;
    FFailed: Boolean;
    FErrorMessage: string;
    FOptions: TGraphicsBoxOptions;
    FOnFailed: TNotifyEvent;
    FOnRenderStart: TNotifyEvent;
    FOnRenderStop: TNotifyEvent;
    FOnRender: TGraphicsRenderEvent;
    FOnInput: TGraphicsInputEvent;
    function CanRender: Boolean;
    function GetContext: IOpenGLContext;
    function GetStepping: Boolean;
    procedure TryRenderStart;
    procedure RenderFailed;
    procedure StopThread;
    procedure StopRendering;
    procedure DestroyChild;
    procedure AddEvent(constref Event: TSDL_Event);
    procedure ReadInput;
    function WantsEvent(constref Event: TSDL_Event): Boolean;
    procedure FireEvents;
    procedure FireEvent(constref Event: TSDL_Event);
    procedure TakeFocus;
    procedure SetOptions(Value: TGraphicsBoxOptions);
    procedure WMPaint(var Message: TLMPaint); message LM_PAINT;
    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;
  protected
    class procedure WSRegisterClass; override;
    procedure DestroyWnd; override;
    procedure Resize; override;
    procedure PaintWindow(DC: HDC); override;
    procedure Paint; virtual;
    { ControlCanvas is used to paint the control when it cannot render }
    property ControlCanvas: TControlCanvas read FCanvas;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground(DC: HDC); override;
    { BeginFrame begins canvas drawing using the canvas back buffer }
    procedure BeginFrame;
    { EndFrame ends canvas drawing using the canvas back buffer }
    procedure EndFrame;
    { Flip alternates between beginning and ending a frame of canvas drawing
      sized to the rendering area of the context }
    procedure Flip;
    { StartStepping runs a step thread which calls OnStep every Interval
      seconds. Steps missed while the thread was busy are caught up, up to a
      quarter of a second. A running step thread is stopped first. Call it from
      the render thread or from the main thread while not rendering. }
    procedure StartStepping(Interval: Double; OnStep: TGraphicsStepEvent);
    { StopStepping stops and waits for the step thread }
    procedure StopStepping;
    { Canvas draws vector graphics using the context. It is created on the
      render thread immediately before OnRenderStart and destroyed
      immediately after OnRenderStop. It is nil outside of that period and
      should only be used from the render thread. }
    property Canvas: ICanvas read FRenderCanvas;
    { Context is only valid while Rendering is True. It is current on the
      render thread and must not be made current on other threads. }
    property Context: IOpenGLContext read GetContext;
    { Rendering is True from when the render thread is started, immediately
      after a window is first shown, until the render thread is stopped,
      immediately before the window handle is destroyed }
    property Rendering: Boolean read FRendering;
    { Stepping is True while the step thread is running }
    property Stepping: Boolean read GetStepping;
    { Failed is True when a context failed the creation step or when the
      render thread stopped because of an exception. Creation failure is
      caused by unsupported options and is distinctly different from
      OpenGLInfo.IsValid. }
    property Failed: Boolean read FFailed;
    { ErrorMessage holds the message of the exception which stopped the
      render thread }
    property ErrorMessage: string read FErrorMessage;
    { OnInput fires on the render thread before OnRender for each key, text,
      mouse and wheel event of the control. Assign it while not rendering. }
    property OnInput: TGraphicsInputEvent read FOnInput write FOnInput;
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property Constraints;
    property Enabled;
    property TabStop;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnKeyDown;
    property OnKeyUp;
    property OnUTF8KeyPress;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnResize;
    { Options are used once immediately before a context is created }
    property Options: TGraphicsBoxOptions read FOptions write SetOptions;
    { OnFailed fires on the main thread after a context failed the creation
      step or the render thread stopped because of an exception }
    property OnFailed: TNotifyEvent read FOnFailed write FOnFailed;
    { OnRenderStart fires on the render thread with the context current after
      the render context and Canvas are created }
    property OnRenderStart: TNotifyEvent read FOnRenderStart write FOnRenderStart;
    { OnRenderStop fires on the render thread with the context current before
      the Canvas and render context are destroyed }
    property OnRenderStop: TNotifyEvent read FOnRenderStop write FOnRenderStop;
    { OnRender fires repeatedly on the render thread with the context current.
      Do not access other controls from OnRender. Use TThread.Queue to send
      information to the main thread. }
    property OnRender: TGraphicsRenderEvent read FOnRender write FOnRender;
  end;

implementation

{$r opengl.res}

{ Codebot.Platform.LCL is used so programs with a TGraphicsBox use the LCL
  platform routines }

uses
  Forms, LCLIntf, WSLCLClasses,
  Codebot.System,
  Codebot.Platform.LCL,
  Codebot.Hardware,
  Codebot.OpenGL.SDL,
  Codebot.Render.Controls.Child
  {$ifdef gtk2gl}
  , Codebot.Render.Controls.Gtk2
  {$endif}
  {$ifdef gtk3gl}
  , Codebot.Render.Controls.Gtk3
  {$endif}
  {$ifdef win32gl}
  , Codebot.Render.Controls.Windows
  {$endif};

{ The boxes which have an SDL window. SDL has one event queue for every
  window, so a filter gives each event to the box it belongs to and keeps it
  out of the queue, which nothing else reads. The filter is called on the
  thread which reads the events of SDL. }

var
  Boxes: array of TGraphicsBox;
  BoxLock: TRTLCriticalSection;

function EventFilter(userdata: Pointer; constref event: TSDL_Event): LongInt; cdecl;
var
  Id: LongWord;
  I: Integer;
begin
  Result := 0;
  case event.type_ of
    SDL_KEYDOWN, SDL_KEYUP: Id := event.key.windowID;
    SDL_TEXTINPUT: Id := event.text.windowID;
    SDL_MOUSEMOTION: Id := event.motion.windowID;
    SDL_MOUSEBUTTONDOWN, SDL_MOUSEBUTTONUP: Id := event.button.windowID;
    SDL_MOUSEWHEEL: Id := event.wheel.windowID;
    SDL_DROPFILE:
      begin
        { Files dropped are not supported, and SDL expects the name to be freed }
        SDL_free(event.drop._file);
        Exit;
      end;
  else
    Exit;
  end;
  EnterCriticalSection(BoxLock);
  try
    for I := 0 to Length(Boxes) - 1 do
      if Boxes[I].FChildID = Id then
      begin
        Boxes[I].AddEvent(event);
        Break;
      end;
  finally
    LeaveCriticalSection(BoxLock);
  end;
end;

procedure BoxAdd(Box: TGraphicsBox);
begin
  EnterCriticalSection(BoxLock);
  try
    SetLength(Boxes, Length(Boxes) + 1);
    Boxes[Length(Boxes) - 1] := Box;
    if Length(Boxes) = 1 then
      SDL_SetEventFilter(@EventFilter, nil);
  finally
    LeaveCriticalSection(BoxLock);
  end;
end;

procedure BoxRemove(Box: TGraphicsBox);
var
  I, J: Integer;
begin
  EnterCriticalSection(BoxLock);
  try
    for I := 0 to Length(Boxes) - 1 do
      if Boxes[I] = Box then
      begin
        for J := I to Length(Boxes) - 2 do
          Boxes[J] := Boxes[J + 1];
        SetLength(Boxes, Length(Boxes) - 1);
        Break;
      end;
    if Length(Boxes) = 0 then
      SDL_SetEventFilter(nil, nil);
  finally
    LeaveCriticalSection(BoxLock);
  end;
end;

{ TGraphicsBoxOptions }

constructor TGraphicsBoxOptions.Create;
begin
  inherited Create;
  FDepthBits := 24;
  FStencilBits := 8;
  FMultiSampling := True;
  FMultiSamples := 4;
end;

procedure TGraphicsBoxOptions.Assign(Source: TPersistent);
var
  O: TGraphicsBoxOptions absolute Source;
begin
  if Source is TGraphicsBoxOptions then
  begin
    FDepthBits := O.FDepthBits;
    FStencilBits := O.FStencilBits;
    FMultiSampling := O.FMultiSampling;
    FMultiSamples := O.FMultiSamples;
  end
  else
    inherited Assign(Source);
end;

procedure TGraphicsBoxOptions.SetDepthBits(Value: Integer);
begin
  if Value < 24 then
    FDepthBits := 16
  else if Value < 32 then
    FDepthBits := 24
  else
    FDepthBits := 32;
end;

procedure TGraphicsBoxOptions.SetStencilBits(Value: Integer);
begin
  if Value < 1 then
    FStencilBits := 0
  else if Value < 8 then
    FStencilBits := 1
  else
    FStencilBits := 8;
end;

procedure TGraphicsBoxOptions.SetMultiSamples(Value: Integer);
begin
  if Value < 2 then
    FMultiSamples := 1
  else if Value < 4 then
    FMultiSamples := 2
  else if Value < 8 then
    FMultiSamples := 4
  else if Value < 16 then
    FMultiSamples := 8
  else
    FMultiSamples := 16;
end;

{ TGraphicsRenderThread owns all rendering for a TGraphicsBox. It calls
  OnRenderStart, then OnRender until the context can no longer render, then
  OnRenderStop. }

type
  TGraphicsRenderThread = class(TThread)
  private
    FBox: TGraphicsBox;
  protected
    procedure Execute; override;
  public
    constructor Create(Box: TGraphicsBox);
  end;

constructor TGraphicsRenderThread.Create(Box: TGraphicsBox);
begin
  FBox := Box;
  inherited Create(False);
end;

procedure TGraphicsRenderThread.Execute;
var
  Context: IOpenGLContext;
  RenderContext: TRenderContext;
  Started: Boolean;
  W, H: Integer;
begin
  Context := FBox.FContext;
  Context.MakeCurrent(True);
  try
    try
      { The render context becomes Ctx for this thread }
      RenderContext := TRenderContext.Create;
      try
        FBox.FRenderCanvas := NewCanvas;
        try
          Started := False;
          try
            if Assigned(FBox.FOnRenderStart) then
              FBox.FOnRenderStart(FBox);
            Started := True;
            while (not Terminated) and Context.CanRender do
            begin
              FBox.ReadInput;
              if Assigned(FBox.FOnRender) then
              begin
                Context.GetSize(W, H);
                FBox.FOnRender(FBox, W, H);
              end;
              Context.Flip;
            end;
          finally
            if Started and Assigned(FBox.FOnRenderStop) then
              FBox.FOnRenderStop(FBox);
          end;
        finally
          FBox.FRenderCanvas := nil;
        end;
      finally
        RenderContext.Free;
      end;
    except
      on E: Exception do
      begin
        FBox.FErrorMessage := E.Message;
        FBox.FFailed := True;
        { Queued calls are removed if the thread is freed before they run }
        Queue(FBox.RenderFailed);
      end;
    end;
  finally
    Context.MakeCurrent(False);
  end;
end;

{ TGraphicsStepThread calls a step event at a fixed rate }

type
  TGraphicsStepThread = class(TThread)
  private
    FBox: TGraphicsBox;
    FInterval: Double;
    FOnStep: TGraphicsStepEvent;
  protected
    procedure Execute; override;
  public
    constructor Create(Box: TGraphicsBox; Interval: Double; OnStep: TGraphicsStepEvent);
  end;

constructor TGraphicsStepThread.Create(Box: TGraphicsBox; Interval: Double; OnStep: TGraphicsStepEvent);
begin
  FBox := Box;
  FInterval := Interval;
  FOnStep := OnStep;
  inherited Create(False);
end;

procedure TGraphicsStepThread.Execute;
const
  { The most time the thread will catch up on, such as after a debugger break }
  MaxCatchUp = 0.25;
var
  Stopwatch: IStopwatch;
  StepTime, Time: Double;
begin
  Stopwatch := StopwatchCreate;
  StepTime := 0;
  try
    while not Terminated do
    begin
      Time := Stopwatch.Calculate;
      if Time - StepTime > MaxCatchUp then
        StepTime := Time - MaxCatchUp;
      while (not Terminated) and (StepTime + FInterval <= Time) do
      begin
        FOnStep(FBox, FInterval);
        StepTime := StepTime + FInterval;
      end;
      Sleep(1);
    end;
  except
    on E: Exception do
    begin
      FBox.FErrorMessage := E.Message;
      FBox.FFailed := True;
      { Queued calls are removed if the thread is freed before they run }
      Queue(FBox.RenderFailed);
    end;
  end;
end;

{ TGraphicsBox }

constructor TGraphicsBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  InitCriticalSection(FEventLock);
  FOptions := TGraphicsBoxOptions.Create;
  { Double clicks are sent as a mouse down with ssDouble and as OnDblClick }
  ControlStyle := ControlStyle - [csSetCaption] + [csCaptureMouse, csClickEvents,
    csDoubleClicks];
  DoubleBuffered := False;
  ParentDoubleBuffered := False;
  SetInitialBounds(0, 0, 400, 300);
end;

destructor TGraphicsBox.Destroy;
begin
  StopStepping;
  inherited Destroy;
  FLogo.Free;
  FOptions.Free;
  DoneCriticalSection(FEventLock);
end;

procedure TGraphicsBox.DestroyWnd;
begin
  { Wait for and free the render thread before the window handle is
    destroyed. The render thread may stop the step thread itself, such as
    when a scene is freed, so the step thread is stopped after it. }
  StopThread;
  StopStepping;
  StopRendering;
  { The SDL window is destroyed before the window it is inside }
  DestroyChild;
  FreeAndNil(FCanvas);
  inherited DestroyWnd;
end;

{ The render thread must be finished before the context or window is
  destroyed. The thread calls OnRenderStop before it finishes. }

procedure TGraphicsBox.StopThread;
begin
  if FThread = nil then
    Exit;
  if FContext <> nil then
    FContext.CanRender := False;
  FThread.Terminate;
  FThread.WaitFor;
  FreeAndNil(FThread);
end;

procedure TGraphicsBox.StartStepping(Interval: Double; OnStep: TGraphicsStepEvent);
begin
  StopStepping;
  if (Interval <= 0) or (not Assigned(OnStep)) then
    Exit;
  FStepThread := TGraphicsStepThread.Create(Self, Interval, OnStep);
end;

procedure TGraphicsBox.StopStepping;
begin
  if FStepThread = nil then
    Exit;
  FStepThread.Terminate;
  FStepThread.WaitFor;
  FreeAndNil(FStepThread);
end;

function TGraphicsBox.GetStepping: Boolean;
begin
  Result := FStepThread <> nil;
end;

procedure TGraphicsBox.StopRendering;
begin
  if not FRendering then
    Exit;
  StopThread;
  FRendering := False;
  FContext := nil;
  DestroyChild;
end;

{ The context is released before the SDL window it belongs to is destroyed }

procedure TGraphicsBox.DestroyChild;
begin
  if FChild = nil then
    Exit;
  FContext := nil;
  BoxRemove(Self);
  ChildDestroy(FChild);
  FChild := nil;
  FChildID := 0;
  EnterCriticalSection(FEventLock);
  FEvents := nil;
  LeaveCriticalSection(FEventLock);
end;

{ AddEvent is called by the event filter on the thread reading the events of
  SDL }

procedure TGraphicsBox.AddEvent(constref Event: TSDL_Event);
begin
  EnterCriticalSection(FEventLock);
  try
    SetLength(FEvents, Length(FEvents) + 1);
    FEvents[Length(FEvents) - 1] := Event;
  finally
    LeaveCriticalSection(FEventLock);
  end;
end;

{ ReadInput is called by the render thread before each frame. Nothing else
  reads the events of SDL in an LCL program, which is also how SDL learns of
  changes to the size of its window. }

procedure TGraphicsBox.ReadInput;
var
  Wanted: Boolean;
  I: Integer;
begin
  PumpHardware;
  ScanHardware;
  EnterCriticalSection(FEventLock);
  FFrameEvents := FEvents;
  FEvents := nil;
  LeaveCriticalSection(FEventLock);
  if Length(FFrameEvents) = 0 then
    Exit;
  Wanted := False;
  for I := 0 to Length(FFrameEvents) - 1 do
    if WantsEvent(FFrameEvents[I]) then
    begin
      Wanted := True;
      Break;
    end;
  { Synchronize runs FireEvents on the main thread and waits for it. The
    main thread keeps running synchronized methods while it waits for this
    thread to stop, so this cannot lock. }
  if Wanted and (not TThread.CheckTerminated) then
    TThread.Synchronize(TThread.CurrentThread, FireEvents);
  for I := 0 to Length(FFrameEvents) - 1 do
    if Assigned(FOnInput) then
      FOnInput(Self, FFrameEvents[I]);
  FFrameEvents := nil;
end;

{ Return True if an event must be passed to the main thread, which is the
  case when it has a handler. A button going down always is, as it gives the
  control the focus. }

function TGraphicsBox.WantsEvent(constref Event: TSDL_Event): Boolean;
begin
  case Event.type_ of
    SDL_KEYDOWN: Result := Assigned(OnKeyDown);
    SDL_KEYUP: Result := Assigned(OnKeyUp);
    SDL_TEXTINPUT: Result := Assigned(OnUTF8KeyPress);
    SDL_MOUSEMOTION: Result := Assigned(OnMouseMove);
    SDL_MOUSEBUTTONDOWN: Result := True;
    SDL_MOUSEBUTTONUP: Result := Assigned(OnMouseUp) or Assigned(OnClick);
    SDL_MOUSEWHEEL: Result := Assigned(OnMouseWheel);
  else
    Result := False;
  end;
end;

{ FireEvents runs on the main thread while the render thread waits. An
  exception raised by a handler is shown by the application and does not
  stop the render thread. }

procedure TGraphicsBox.FireEvents;
var
  I: Integer;
begin
  if (csDestroying in ComponentState) or (not HandleAllocated) then
    Exit;
  for I := 0 to Length(FFrameEvents) - 1 do
  try
    FireEvent(FFrameEvents[I]);
  except
    Application.HandleException(Self);
  end;
end;

{ Call the LCL event which matches an SDL event }

procedure TGraphicsBox.FireEvent(constref Event: TSDL_Event);

  function Shift(Buttons: LongWord): TShiftState;
  var
    M: LongWord;
  begin
    Result := [];
    M := SDL_GetModState;
    if M and KMOD_SHIFT <> 0 then
      Include(Result, ssShift);
    if M and KMOD_CTRL <> 0 then
      Include(Result, ssCtrl);
    if M and KMOD_ALT <> 0 then
      Include(Result, ssAlt);
    if Buttons and (1 shl (SDL_BUTTON_LEFT - 1)) <> 0 then
      Include(Result, ssLeft);
    if Buttons and (1 shl (SDL_BUTTON_RIGHT - 1)) <> 0 then
      Include(Result, ssRight);
    if Buttons and (1 shl (SDL_BUTTON_MIDDLE - 1)) <> 0 then
      Include(Result, ssMiddle);
  end;

  function MouseButton(Button: Byte): TMouseButton;
  begin
    case Button of
      SDL_BUTTON_RIGHT: Result := mbRight;
      SDL_BUTTON_MIDDLE: Result := mbMiddle;
      SDL_BUTTON_X1: Result := mbExtra1;
      SDL_BUTTON_X2: Result := mbExtra2;
    else
      Result := mbLeft;
    end;
  end;

var
  State: TShiftState;
  Key: Word;
  Text: string;
  Ch: TUTF8Char;
  X, Y: LongInt;
  I, N: Integer;
begin
  case Event.type_ of
    SDL_KEYDOWN, SDL_KEYUP:
      begin
        Key := VirtualKey(Event.key.keysym.sym);
        if Key = 0 then
          Exit;
        if Event.type_ = SDL_KEYDOWN then
          KeyDown(Key, Shift(0))
        else
          KeyUp(Key, Shift(0));
      end;
    SDL_TEXTINPUT:
      begin
        Text := PAnsiChar(@Event.text.text[0]);
        { The text is passed on one UTF8 character at a time }
        I := 1;
        while I <= Length(Text) do
        begin
          case Ord(Text[I]) of
            $C0..$DF: N := 2;
            $E0..$EF: N := 3;
            $F0..$F7: N := 4;
          else
            N := 1;
          end;
          Ch := Copy(Text, I, N);
          Inc(I, N);
          { Control characters such as backspace arrive as key down events }
          if (Ch <> '') and (Ch[1] >= ' ') and (Ch[1] <> #127) then
            UTF8KeyPress(Ch);
        end;
      end;
    SDL_MOUSEMOTION:
      MouseMove(Shift(Event.motion.state), Event.motion.x, Event.motion.y);
    SDL_MOUSEBUTTONDOWN:
      begin
        TakeFocus;
        State := Shift(SDL_GetMouseState(X, Y) or (1 shl (Event.button.button - 1)));
        { The field after state holds the number of clicks }
        if Event.button.padding1 = 2 then
          Include(State, ssDouble);
        MouseDown(MouseButton(Event.button.button), State, Event.button.x, Event.button.y);
        if (Event.button.button = SDL_BUTTON_LEFT) and (ssDouble in State) then
          DblClick;
      end;
    SDL_MOUSEBUTTONUP:
      begin
        State := Shift(SDL_GetMouseState(X, Y));
        MouseUp(MouseButton(Event.button.button), State, Event.button.x, Event.button.y);
        if (Event.button.button = SDL_BUTTON_LEFT) and
          PtInRect(ClientRect, Point(Event.button.x, Event.button.y)) then
          Click;
      end;
    SDL_MOUSEWHEEL:
      begin
        SDL_GetMouseState(X, Y);
        { The LCL reports 120 for each notch of the wheel }
        DoMouseWheel(Shift(0), Event.wheel.y * 120, Point(X, Y));
      end;
  end;
end;

{ TakeFocus gives the control and its SDL window the focus when the SDL
  window is clicked. It runs on the main thread. }

procedure TGraphicsBox.TakeFocus;
begin
  if (csDestroying in ComponentState) or (not HandleAllocated) then
    Exit;
  if (FChild = nil) or ChildFocused(FChild) then
    Exit;
  if CanFocus and not Focused then
    SetFocus;
  ChildFocus(FChild);
end;

{ The LCL gives the focus to the control, which passes it on to its SDL
  window so that SDL receives the keys }

procedure TGraphicsBox.WMSetFocus(var Message: TLMSetFocus);
begin
  inherited;
  if FChild <> nil then
    ChildFocus(FChild);
end;

procedure TGraphicsBox.WMKillFocus(var Message: TLMKillFocus);
begin
  inherited;
  if (FChild <> nil) and HandleAllocated then
    ChildUnfocus(FChild, TWSOpenGLWindow.TopLevelWindow(Self));
end;

procedure TGraphicsBox.Resize;
begin
  inherited Resize;
  if FChild <> nil then
    ChildResize(FChild, ClientWidth, ClientHeight);
end;

{ RenderFailed is queued to the main thread when the render thread stops
  because of an exception }

procedure TGraphicsBox.RenderFailed;
begin
  Invalidate;
  if Assigned(FOnFailed) then
    FOnFailed(Self);
end;

procedure TGraphicsBox.BeginFrame;
begin
  if FRenderCanvas <> nil then
    (FRenderCanvas as IBackBuffer).BeginFrame;
end;

procedure TGraphicsBox.EndFrame;
begin
  if FRenderCanvas <> nil then
    (FRenderCanvas as IBackBuffer).EndFrame;
end;

procedure TGraphicsBox.Flip;
var
  W, H: Integer;
begin
  if (FRenderCanvas = nil) or (FContext = nil) then
    Exit;
  { The context size is safe to read from the render thread }
  FContext.GetSize(W, H);
  (FRenderCanvas as IBackBuffer).Flip(W, H);
end;

var
  Registered: Boolean;

class procedure TGraphicsBox.WSRegisterClass;
begin
  if Registered then
    Exit;
  Registered := True;
  RegisterWSComponent(TGraphicsBox, TWSOpenGLWindow)
end;

procedure TGraphicsBox.TryRenderStart;
begin
  if FRendering or (not CanRender) then
    Exit;
  { GetContext only creates a context while FRendering is True }
  FRendering := True;
  if Context = nil then
  begin
    FRendering := False;
    if Assigned(FOnFailed) then
      FOnFailed(Self);
    Exit;
  end;
  Context.CanRender := True;
  FThread := TGraphicsRenderThread.Create(Self);
end;

procedure TGraphicsBox.EraseBackground(DC: HDC);
begin
  TryRenderStart;
  if not CanRender then
    inherited EraseBackground(DC);
end;

procedure TGraphicsBox.WMPaint(var Message: TLMPaint);
begin
  if (csDestroying in ComponentState) or (not HandleAllocated) then
    Exit;
  Include(FControlState, csCustomPaint);
  inherited WMPaint(Message);
  Exclude(FControlState, csCustomPaint);
end;

procedure TGraphicsBox.PaintWindow(DC: HDC);
var
  Changed: Boolean;
begin
  TryRenderStart;
  if not CanRender then
  begin
    if FCanvas = nil then
    begin
      FCanvas := TControlCanvas.Create;
      FCanvas.Control := Self;
    end;
    Changed := (not FCanvas.HandleAllocated) or (FCanvas.Handle <> DC);
    if Changed then
      FCanvas.Handle := DC;
    Paint;
    if Changed then
      FCanvas.Handle := 0;
  end;
end;

procedure TGraphicsBox.Paint;

  procedure Colorize;
  var
    Color: TColor;
    W, H, X, Y: Integer;
    Source, Dest: PByte;
    A: Single;
  begin
    if FLogo.PixelFormat <> pf32bit then
      Exit;
    W := FLogo.Width;
    H := FLogo.Height;
    if (W < 1) or (H < 1) then
      Exit;
    Color := clWhite;
    Source := @Color;
    FLogo.BeginUpdate;
    for Y := 0 to H - 1 do
    begin
      Dest := FLogo.RawImage.GetLineStart(Y);
      for X := 0 to W - 1 do
      begin
        A := Dest[3] / 255;
        Dest^ := Trunc(Source[2] * A);
        Inc(Dest);
        Dest^ := Trunc(Source[1] * A);
        Inc(Dest);
        Dest^ := Trunc(Source[0] * A);
        Inc(Dest);
        Inc(Dest);
      end;
    end;
    FLogo.EndUpdate;
  end;

  procedure LoadBitmap;
  var
    P: TPicture;
  begin
    if FLogo <> nil then
      Exit;
    P := TPicture.Create;
    try
      P.LoadFromResourceName(Hinstance, 'opengl.png');
      FLogo := TBitmap.Create;
      FLogo.Assign(P.Bitmap);
    finally
      P.Free;
    end;
    Colorize;
  end;

var
  S: string;
  H, X, Y: Integer;
begin
  FCanvas.Brush.Color := 0;
  FCanvas.Pen.Color := clWhite;
  FCanvas.Pen.Style := psDash;
  FCanvas.Rectangle(ClientRect);
  LoadBitmap;
  FCanvas.Draw((Width - FLogo.Width) shr 1, (Height - FLogo.Height) shr 1, FLogo);
  if csDesigning in ComponentState then
    Exit;
  FCanvas.Font.Color := clWhite;
  H := FCanvas.TextHeight('Wg');
  X := 5;
  Y := 5;
  if FErrorMessage <> '' then
  begin
    FCanvas.TextOut(X, Y, 'Rendering stopped because of an error');
    Inc(Y, H);
    FCanvas.TextOut(X, Y, FErrorMessage);
  end
  else if FFailed then
    FCanvas.TextOut(X, Y, 'Options failed to create a context')
  else
  begin
    S := 'Your video driver does not support ' + OpenGLApiName;
    FCanvas.TextOut(5, Y, S);
    Inc(Y, H);
    FCanvas.TextOut(5, Y, 'Try selecting a lower version in render.inc');
    Inc(Y, H * 2);
    FCanvas.TextOut(5, Y, 'Renderer: ' + OpenGLInfo.Renderer);
    Inc(Y, H);
    FCanvas.TextOut(5, Y, 'Version: ' + OpenGLInfo.Version);
  end;
end;

function TGraphicsBox.CanRender: Boolean;
begin
  Result := (not (csDesigning in ComponentState)) and OpenGLInfo.IsValid and (not Failed);
end;

function TGraphicsBox.GetContext: IOpenGLContext;
var
  Params: TOpenGLParams;
begin
  if CanRender and FRendering and (FContext = nil) then
  begin
    Params := TOpenGLParams.Create;
    Params.Depth := FOptions.DepthBits;
    Params.Stencil := FOptions.StencilBits;
    Params.MultiSampling := FOptions.MultiSampling;
    Params.MultiSamples := FOptions.MultiSamples;
    { SDL creates a window inside the control and the context for it }
    FChild := ChildCreate(TWSOpenGLWindow.NativeWindow(Self), ClientWidth,
      ClientHeight, Params);
    if FChild <> nil then
    begin
      FChildID := SDL_GetWindowID(FChild);
      BoxAdd(Self);
      FContext := OpenGLContextCreate(GLwindow(FChild), Params);
      if FContext = nil then
        DestroyChild
      else if Focused then
        ChildFocus(FChild);
    end;
    FFailed := FContext = nil;
  end;
  Result := FContext;
end;

procedure TGraphicsBox.SetOptions(Value: TGraphicsBoxOptions);
begin
  FOptions.Assign(Value);
end;

initialization
  InitCriticalSection(BoxLock);
  { An LCL program has no event loop of SDL, so the hardware scan reads the
    events of SDL itself }
  ScanHardwarePumps := True;
finalization
  DoneCriticalSection(BoxLock);
end.
