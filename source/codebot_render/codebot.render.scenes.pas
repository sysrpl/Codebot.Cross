unit Codebot.Render.Scenes;

{$i codebot.inc}

interface

uses
  LCLIntf,
  Codebot.System,
  Codebot.Render.Contexts;

{ TScene }

type
  TShiftKeys = set of (skAlt, skCtrl, skShift);
  TMouseAction = (maPress, maMove, maRelease);

  TScene = class
  private
    FAnimated: Boolean;
    FContext: TContext;
    FBaseTime: Double;
    FTime: Double;
    FWidth: Integer;
    FHeight: Integer;
    FLogicPhase: Boolean;
    FLogicTime: Double;
    function GetTime: Double;
  protected
    { Resize sets the viewport but you might use it to change the perspective matrix }
    procedure Resize; virtual;
  public
    constructor Create(Width, Height: Integer); virtual;
    destructor Destroy; override;
    { Name of the scene }
    function Name: string; virtual;
    { KeyEvent is fired when a keyboard action occurs }
    procedure KeyEvent(KeyCode: Integer; Shift: TShiftKeys); virtual;
    { MouseEvent is fired when a mouse action occurs }
    procedure MouseEvent(X, Y: Integer; Action: TMouseAction); virtual;
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
    { When Animated is True update is called continously }
    property Animated: Boolean read FAnimated write FAnimated;
    { Context associated with the scene }
    property Context: TContext read FContext;
    { Time that can be used during Logic or Render }
    property Time: Double read GetTime;
    { Width is updated immediately before Render }
    property Width: Integer read FWidth;
    { Height is updated immediately before Render }
    property Height: Integer read FHeight;
  end;

  TSceneClass = class of TScene;

const
  VK_LBUTTON    = 1;
  VK_RBUTTON    = 2;
  VK_CANCEL     = 3;
  VK_MBUTTON    = 4;
  VK_XBUTTON1   = 5;
  VK_XBUTTON2   = 6;
  VK_BACK       = 8;
  VK_TAB        = 9;
  VK_CLEAR      = 12;
  VK_RETURN     = 13;
  VK_SHIFT      = 16;
  VK_CONTROL    = 17;
  VK_MENU       = 18;
  VK_PAUSE      = 19;
  VK_CAPITAL    = 20;
  VK_KANA       = 21;
  VK_HANGUL     = 21;
  VK_JUNJA      = 23;
  VK_FINAL      = 24;
  VK_HANJA      = 25;
  VK_KANJI      = 25;
  VK_ESCAPE     = 27;
  VK_CONVERT    = 28;
  VK_NONCONVERT = 29;
  VK_ACCEPT     = 30;
  VK_MODECHANGE = 31;
  VK_SPACE      = 32;
  VK_PRIOR      = 33;
  VK_NEXT       = 34;
  VK_END        = 35;
  VK_HOME       = 36;
  VK_LEFT       = 37;
  VK_UP         = 38;
  VK_RIGHT      = 39;
  VK_DOWN       = 40;
  VK_SELECT     = 41;
  VK_PRINT      = 42;
  VK_EXECUTE    = 43;
  VK_SNAPSHOT   = 44;
  VK_INSERT     = 45;
  VK_DELETE     = 46;
  VK_HELP       = 47;
  VK_0          = $30;
  VK_1          = $31;
  VK_2          = $32;
  VK_3          = $33;
  VK_4          = $34;
  VK_5          = $35;
  VK_6          = $36;
  VK_7          = $37;
  VK_8          = $38;
  VK_9          = $39;
  VK_A          = $41;
  VK_B          = $42;
  VK_C          = $43;
  VK_D          = $44;
  VK_E          = $45;
  VK_F          = $46;
  VK_G          = $47;
  VK_H          = $48;
  VK_I          = $49;
  VK_J          = $4A;
  VK_K          = $4B;
  VK_L          = $4C;
  VK_M          = $4D;
  VK_N          = $4E;
  VK_O          = $4F;
  VK_P          = $50;
  VK_Q          = $51;
  VK_R          = $52;
  VK_S          = $53;
  VK_T          = $54;
  VK_U          = $55;
  VK_V          = $56;
  VK_W          = $57;
  VK_X          = $58;
  VK_Y          = $59;
  VK_Z          = $5A;
  VK_LWIN       = $5B;
  VK_RWIN       = $5C;
  VK_APPS       = $5D;
  VK_SLEEP      = $5F;
  VK_NUMPAD0    = 96;
  VK_NUMPAD1    = 97;
  VK_NUMPAD2    = 98;
  VK_NUMPAD3    = 99;
  VK_NUMPAD4    = 100;
  VK_NUMPAD5    = 101;
  VK_NUMPAD6    = 102;
  VK_NUMPAD7    = 103;
  VK_NUMPAD8    = 104;
  VK_NUMPAD9    = 105;
  VK_MULTIPLY   = 106;
  VK_ADD        = 107;
  VK_SEPARATOR  = 108;
  VK_SUBTRACT   = 109;
  VK_DECIMAL    = 110;
  VK_DIVIDE     = 111;
  VK_F1         = 112;
  VK_F2         = 113;
  VK_F3         = 114;
  VK_F4         = 115;
  VK_F5         = 116;
  VK_F6         = 117;
  VK_F7         = 118;
  VK_F8         = 119;
  VK_F9         = 120;
  VK_F10        = 121;
  VK_F11        = 122;
  VK_F12        = 123;
  VK_NUMLOCK    = $90;
  VK_SCROLL     = $91;
  VK_LSHIFT     = $A0;
  VK_RSHIFT     = $A1;
  VK_LCONTROL   = $A2;
  VK_RCONTROL   = $A3;
  VK_LMENU      = $A4;
  VK_RMENU      = $A5;

{ IsKeyDown returns true if the virtual key is down }

function IsKeyDown(KeyCode: Integer): Boolean;

const
  SceneLogicStep = Double(1 / 100);

implementation

{ TScene }

constructor TScene.Create(Width, Height: Integer);
begin
  inherited Create;
  FAnimated := True;
  FWidth := Width;
  FHeight := Height;
  FContext := TContext.Create;
  FContext.MakeCurrent(True);
  FContext.SetViewport(0, 0, Width, Height);
  Initialize;
  Resize;
  FContext.MakeCurrent(False);
end;

destructor TScene.Destroy;
begin
  FContext.MakeCurrent(True);
  Finalize;
  FContext.MakeCurrent(False);
  FContext.Free;
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

procedure TScene.Update(Width, Height: Integer; Time: Double);
begin
  if FBaseTime = 0 then
    FBaseTime := Time;
  FTime := Time - FBaseTime;
  FLogicPhase := True;
  if FAnimated then
    while FLogicTime < FTime do
    begin
      FLogicTime := FTime + SceneLogicStep;
      Logic;
    end
  else
  begin
    FLogicTime := FTime;
    Logic;
  end;
  FLogicPhase := False;
  FContext.MakeCurrent(True);
  if (Width <> FWidth) or (FHeight <> Height) then
  begin
    FWidth := Width;
    FHeight := Height;
    Resize;
  end;
  Render;
  FContext.MakeCurrent(True);
end;

procedure TScene.Logic;
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

function IsKeyDown(KeyCode: Integer): Boolean;
begin
  Result := GetKeyState(KeyCode) and $80 <> 0;
end;

end.

