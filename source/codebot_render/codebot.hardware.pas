(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ Codebot.Hardware plays audio and reads the keyboard, mouse and joysticks
  using SDL alone. It is the same on every backend and is not part of
  Codebot.Platform.

  Audio

  Audio is added as named sources, which are held in memory in their
  original format. A source is played by loading it into one of a fixed
  number of banks. Each bank decodes its source a block at a time while it
  plays and has its own volume, pan, position and loop count. The banks
  which are playing are mixed together on the audio thread of SDL.

  The formats supported are mp3, ogg vorbis, uncompressed wav and tracker
  music such as mod, s3m, xm and it. Every source must be 2 channel stereo
  with signed 16 bit samples at a rate of 44100 Hz. Tracker music is always
  played in that format. A source in any other format is refused when it is
  added. If no audio device can be opened sources and banks still work but
  nothing is heard.

  Keyboard, mouse and joysticks

  Keyboard, Mouse and Joysticks hold the state of the hardware as it was
  when it was last scanned. The hosts which run scenes scan once a frame by
  calling ScanHardware, so a scene only reads the state. It gives the same
  answer for the whole frame, and Pressed and Released compare it with the
  frame before.

  A joystick which SDL recognizes as a gamepad also has the same named
  buttons and axes as every other gamepad, whatever its make. Use
  GamepadButtons and GamepadAxes to read those, and Buttons and Axes to read
  the parts of any joystick by number. A scan also finds joysticks which
  were connected and removes those which were disconnected, so the objects
  in Joysticks should not be kept between frames.

  What needs a window made by SDL

  Audio and joysticks use only the audio, joystick and game controller
  subsystems of SDL and work in any program. SDL only learns of keys and of
  the mouse from the events of a window it created, so in a program without
  one no key or button is ever down and the mouse never moves. }

unit Codebot.Hardware;

{$i render.inc}
{$pointermath on}

interface

uses
  SysUtils, Classes;

{ Audio }

const
  { The number of banks in TAudio }
  DefAudioBankCount = 32;
  { Two channel stereo }
  DefChannels = 2;
  { Signed 16 bit samples }
  DefBitsPerSample = 16;
  { The sample rate of CD quality audio }
  DefSampleRate = 44100;
  { The number of samples in a mixing block, which is about 46ms of audio }
  DefMixingSamples = 2048;
  { The size in bytes of a mixing block }
  DefMixingSize = DefMixingSamples * 4;

type
  EAudioException = class(Exception);
  { EAudioFormatException is raised when an audio source is not in a
    supported format }
  EAudioFormatException = class(EAudioException);

  { TAudioFormat is the format an audio source is stored in }
  TAudioFormat = (
    afUnsupported,
    { Mp3 audio }
    afMp3,
    { Mod, s3m, xm, it or other tracker music }
    afTracker,
    { Ogg vorbis audio }
    afVorbis,
    { Uncompressed pcm wav audio }
    afWave);

const
  AudioFormatNames: array[TAudioFormat] of string = (
    'unsupported audio format',
    'mp3 audio',
    'tracker music',
    'ogg vorbis audio',
    'wav pcm audio');

{ Detect the audio format of a file. An EAudioFormatException is raised if
  the format is known but the channels, sample size or sample rate are not
  supported. }
function AudioFormatDetect(const FileName: string): TAudioFormat; overload;
{ Detect the audio format of a stream, which is read from its start }
function AudioFormatDetect(Stream: TStream): TAudioFormat; overload;
{ Detect the audio format of a file and its duration in seconds }
function AudioFormatTime(const FileName: string; out Duration: Single): TAudioFormat; overload;
{ Detect the audio format of a stream and its duration in seconds }
function AudioFormatTime(Stream: TStream; out Duration: Single): TAudioFormat; overload;

type
  TAudio = class;

  { The offset in bytes of each frame of an mp3 source }
  TAudioOffsets = array of LongWord;

  { TAudioSource is audio held in memory in its original format. Sources
    are created by TAudio.Add, which raises an EAudioFormatException unless
    the audio is

      in a supported format
      2 channel stereo
      signed 16 bit samples
      a sample rate of 44100 Hz

    The source owns the memory it is created with. }

  TAudioSource = class
  private
    FName: string;
    FId: Integer;
    FSource: PByte;
    FSize: LongWord;
    FFormat: TAudioFormat;
    FDuration: Single;
    FOffsets: TAudioOffsets;
  public
    constructor Create(const Name: string; Source: PByte; Size: LongWord);
    destructor Destroy; override;
    { Name is unique among the sources of TAudio }
    property Name: string read FName;
    { Id is unique and is generated when the source is created }
    property Id: Integer read FId;
    { Duration is the length of the audio in seconds }
    property Duration: Single read FDuration;
    { The format of the source }
    property Format: TAudioFormat read FFormat;
  end;

  { TAudioDecoder is used privately by TAudioBank to decode its source one
    mixing block at a time }

  TAudioDecoder = class
  private
    FSource: TAudioSource;
    FPosition: LongWord;
    FMaxPosition: LongWord;
    FDecoded: LongWord;
  protected
    function GetPosition: Single;
    procedure SetPosition(Value: Single); virtual; abstract;
  public
    constructor Create(Source: TAudioSource); virtual;
    { Decode the next mixing block to Samples, which was filled with
      silence, and return False if the end was already reached. FDecoded is
      set to the number of samples decoded, which is fewer than a block at
      the end. }
    function Decode(Samples: PByte): Boolean; virtual; abstract;
    { Position in seconds }
    property Position: Single read GetPosition write SetPosition;
  end;

  { TAudioBank plays one audio source. Banks belong to TAudio. }

  TAudioBank = class
  private
    FAudio: TAudio;
    FTouched: Int64;
    FSource: TAudioSource;
    FSamples: PByte;
    FBlock: PByte;
    FCarry: PByte;
    FCarryCount: Integer;
    FDecoder: TAudioDecoder;
    FCompleted: Boolean;
    FReserved: Boolean;
    FPaused: Boolean;
    FLoop: Integer;
    FLoopCount: Integer;
    FMuted: Boolean;
    FPan: Single;
    FVolume: Single;
    FFade: Single;
    FFadeTarget: Single;
    FFadeRate: Single;
    function Decode: Boolean;
    procedure ApplyFade;
    procedure SetPaused(Value: Boolean);
    procedure SetLoopCount(Value: Integer);
    procedure SetMuted(Value: Boolean);
    procedure SetPan(Value: Single);
    function GetDuration: Single;
    function GetPosition: Single;
    procedure SetPosition(Value: Single);
    function GetVolume: Single;
    procedure SetVolume(Value: Single);
  public
    constructor Create(Audio: TAudio);
    destructor Destroy; override;
    { Load a source, which can be nil. The bank is paused and reset. }
    procedure Load(Source: TAudioSource); overload;
    { Load a source by name. The bank is paused and reset. }
    procedure Load(const Name: string); overload;
    { Load a source by id. The bank is paused and reset. }
    procedure Load(Id: Integer); overload;
    { Unload is the same as Load(nil) }
    procedure Unload;
    { Play the bank, raising it from silence to its volume over a number of
      seconds. A paused bank starts from silence where it was paused, and a
      completed bank starts again from the beginning. A bank which is fading
      out turns around from where the fade had reached. }
    procedure FadeIn(Seconds: Single);
    { Lower the bank to silence over a number of seconds and then pause it.
      Its position is kept, so FadeIn or Paused continues from there. }
    procedure FadeOut(Seconds: Single);
    { Pause the bank and set its properties to their defaults. Source,
      Duration and Reserved are not changed. }
    procedure Reset;
    { The source loaded in the bank }
    property Source: TAudioSource read FSource;
    { The duration of the source in seconds }
    property Duration: Single read GetDuration;
    { Completed is True once the bank has played to the end LoopCount times.
      The bank is then paused. }
    property Completed: Boolean read FCompleted;
    { A paused bank is not decoded or mixed. The default is True. A bank
      without a source is always paused. Setting Paused to False plays at
      the volume of the bank at once, ending any fade. }
    property Paused: Boolean read FPaused write SetPaused;
    { The number of times to play before the bank is completed. A value of
      less than 1 plays until it is stopped, and reads back as 0. The default
      is 1. }
    property LoopCount: Integer read FLoopCount write SetLoopCount;
    { The number of times the bank has played to the end }
    property Loop: Integer read FLoop;
    { A muted bank advances its position but is not heard }
    property Muted: Boolean read FMuted write SetMuted;
    { Pan moves the audio to the left at -1 or to the right at 1. The
      default is 0. }
    property Pan: Single read FPan write SetPan;
    { Position is the playback point in seconds. Setting it clears
      Completed and sets Loop to 0. Most formats move the position to the
      start of the nearest block they can decode from, so the value read
      back can differ from the value set. }
    property Position: Single read GetPosition write SetPosition;
    { Volume ranges from 0 for silence to 1. The default is 1. Volume
      reads as 0 while the bank is muted. }
    property Volume: Single read GetVolume write SetVolume;
    { A reserved bank is never chosen by TAudio.Play or TAudio.Next and can
      only be loaded directly }
    property Reserved: Boolean read FReserved write FReserved;
  end;

  { One sample of stereo sound, where each side ranges from -1 to 1 }
  TAudioSample = record
    Left: Single;
    Right: Single;
  end;
  { Pointer to a TAudioSample }
  PAudioSample = ^TAudioSample;

  { TAudioMixEvent is called by the mixer for a block of samples a program
    makes itself. Samples holds Count samples of silence, to be filled in or
    left alone. The samples are at DefSampleRate, and each block follows on
    from the one before, so a generator keeps its own phase between calls.

    The event is called on the audio thread. It must be quick, and must not
    raise exceptions, allocate memory, or use strings or other managed
    types. Values it shares with the rest of the program should be simple
    ones which are written whole, such as a number or a Boolean. }
  TAudioMixEvent = procedure(Sender: TObject; Samples: PAudioSample;
    Count: Integer) of object;

  { TAudio owns the audio sources and banks and mixes the banks which are
    playing. Use the Audio function to get the one instance. Audio is
    paused until Paused is set to False. }

  TAudio = class
  private
    FDevice: LongWord;
    FInitialized: Boolean;
    FPaused: Boolean;
    FVolume: Single;
    FMixer: PSingle;
    FBlock: PByte;
    FBlockBytes: Integer;
    FGenerated: PAudioSample;
    FOnMix: TAudioMixEvent;
    FTouchCount: Int64;
    FSources: array of TAudioSource;
    FBanks: array[0..DefAudioBankCount - 1] of TAudioBank;
    procedure Lock;
    procedure Unlock;
    procedure MixBlock;
    function NewBank(Index: Integer): TAudioBank;
    function GetAvailable: Boolean;
    procedure SetPaused(Value: Boolean);
    procedure SetVolume(Value: Single);
    procedure SetOnMix(Value: TAudioMixEvent);
    function GetSource(Index: Integer): TAudioSource;
    function GetSourceCount: Integer;
    function GetBank(Index: Integer): TAudioBank;
    function GetBankCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    { Banks are created when first used. Return True if one has been. }
    function BankExists(Index: Integer): Boolean;
    { Return True if audio is not paused and any bank is playing }
    function IsPlaying: Boolean;
    { Add a source from a file with a name which is not in use }
    function Add(const Name: string; const FileName: string): TAudioSource; overload;
    { Add a source from a stream, which is read from its start }
    function Add(const Name: string; Stream: TStream): TAudioSource; overload;
    { Remove and destroy a source, unloading it from any bank }
    procedure Remove(Source: TAudioSource);
    { Find a source by name or return nil }
    function Source(const Name: string): TAudioSource; overload;
    { Find a source by id or return nil }
    function Source(Id: Integer): TAudioSource; overload;
    { Return a bank which is not reserved, preferring a completed bank, then
      an unused one, then the bank loaded or reset the longest ago }
    function Next: TAudioBank;
    { Load a source in a bank chosen as by Next and play it. LoopCount is
      the number of times it is played, where a value less than 1 plays
      until it is stopped. With FadeIn above 0 it is raised from silence
      over that many seconds. Use FadeOut on the bank returned to end it
      the same way. }
    function Play(Source: TAudioSource; LoopCount: Integer = 1;
      FadeIn: Single = 0): TAudioBank; overload;
    { Play a source by name }
    function Play(const Name: string; LoopCount: Integer = 1;
      FadeIn: Single = 0): TAudioBank; overload;
    { Play a source by id }
    function Play(Id: Integer; LoopCount: Integer = 1;
      FadeIn: Single = 0): TAudioBank; overload;
    { Available is False if no audio device could be opened }
    property Available: Boolean read GetAvailable;
    { Paused stops all mixing. The default is True. }
    property Paused: Boolean read FPaused write SetPaused;
    { Volume of all banks from 0 for silence to 1. The default is 1. }
    property Volume: Single read FVolume write SetVolume;
    { The audio sources by index }
    property Sources[Index: Integer]: TAudioSource read GetSource;
    { The number of audio sources }
    property SourceCount: Integer read GetSourceCount;
    { The banks by index }
    property Banks[Index: Integer]: TAudioBank read GetBank;
    { The number of banks }
    property BankCount: Integer read GetBankCount;
    { OnMix lets a program generate sound itself. It is called for every
      block which is mixed while audio is not paused, and what it writes is
      mixed with the banks and follows Volume. Assign nil to stop. }
    property OnMix: TAudioMixEvent read FOnMix write SetOnMix;
  end;

{ The audio instance, which is created when first used }
function Audio: TAudio;

{ Keyboard }

type
  { The state of every key by the scancode of SDL }
  TKeyStates = array[0..511] of Boolean;

  { TKeyboard holds the state of the keys at the last scan. Keys are VK_
    codes from Codebot.Platform. VK_SHIFT, VK_CONTROL and VK_MENU are down if
    the key on either side of the keyboard is down. A code which is not a
    key, such as a mouse button, is never down. Use the Keyboard function to
    get the one instance. }

  TKeyboard = class
  private
    FKeys: TKeyStates;
    FPrior: TKeyStates;
    FScanned: Boolean;
    function State(KeyCode: Integer; Prior: Boolean): Boolean;
    function GetKey(KeyCode: Integer): Boolean;
    function GetPressed(KeyCode: Integer): Boolean;
    function GetReleased(KeyCode: Integer): Boolean;
    function GetShiftState: TShiftState;
    { Copy the state of the keys from SDL }
    procedure Scan;
  public
    { True while a key is held down }
    property Key[KeyCode: Integer]: Boolean read GetKey; default;
    { True if a key went down since the frame before }
    property Pressed[KeyCode: Integer]: Boolean read GetPressed;
    { True if a key went up since the frame before }
    property Released[KeyCode: Integer]: Boolean read GetReleased;
    { The shift, control and alt keys which are down }
    property ShiftState: TShiftState read GetShiftState;
  end;

{ The keyboard instance, which is created and scanned when first used }
function Keyboard: TKeyboard;
{ Convert an SDL key code to a VK_ code, or 0 if there is none. Punctuation
  keys use the VK_OEM codes of a US layout. Hosts use it for key events. }
function VirtualKey(Sym: LongWord): Word;
{ Return True if a key or mouse button is down at the time it is asked,
  rather than at the last scan. KeyCode is a VK_ code from Codebot.Platform,
  including the mouse buttons VK_LBUTTON, VK_RBUTTON, VK_MBUTTON,
  VK_XBUTTON1 and VK_XBUTTON2. VK_SHIFT, VK_CONTROL and VK_MENU are down if
  the key on either side of the keyboard is down. It can be called from any
  thread. }
function IsKeyDown(KeyCode: Integer): Boolean;

{ Mouse }

type
  { The buttons of a mouse. buttonNone is the button of a mouse event which
    no button caused, such as a move. }
  TSceneButton = (buttonNone, buttonLeft, buttonRight, buttonMiddle,
    buttonExtra1, buttonExtra2);
  { A set of mouse buttons }
  TSceneButtons = set of TSceneButton;

  { The mouse cursors. cursorDefault is the arrow and cursorNone hides the
    cursor. }
  TMouseCursor = (cursorDefault, cursorNone, cursorArrow, cursorIBeam,
    cursorWait, cursorCross, cursorHand, cursorNo, cursorSizeAll, cursorSizeNS,
    cursorSizeWE, cursorSizeNWSE, cursorSizeNESW);

  { TMouse holds the state of the mouse at the last scan. Use the Mouse
    function to get the one instance. }

  TMouse = class
  private
    FButtons: TSceneButtons;
    FPrior: TSceneButtons;
    FX: Integer;
    FY: Integer;
    FXDelta: Integer;
    FYDelta: Integer;
    FScanned: Boolean;
    FCursor: TMouseCursor;
    FCursors: array[TMouseCursor] of Pointer;
    procedure NeedScan;
    function GetButtons: TSceneButtons;
    function GetButton(Index: TSceneButton): Boolean;
    function GetPressed(Index: TSceneButton): Boolean;
    function GetReleased(Index: TSceneButton): Boolean;
    function GetX: Integer;
    function GetY: Integer;
    function GetXDelta: Integer;
    function GetYDelta: Integer;
    function GetCaptured: Boolean;
    procedure SetCaptured(Value: Boolean);
    function GetVisible: Boolean;
    procedure SetVisible(Value: Boolean);
    procedure SetCursor(Value: TMouseCursor);
    { Copy the state of the mouse from SDL }
    procedure Scan;
  public
    destructor Destroy; override;
    { The buttons which are held down }
    property Buttons: TSceneButtons read GetButtons;
    { True while a button is held down. buttonNone is never down. }
    property Button[Index: TSceneButton]: Boolean read GetButton; default;
    { True if a button went down since the frame before }
    property Pressed[Index: TSceneButton]: Boolean read GetPressed;
    { True if a button went up since the frame before }
    property Released[Index: TSceneButton]: Boolean read GetReleased;
    { The position of the mouse inside the window with the mouse focus, in
      the units of the window before any scaling of the scene }
    property X: Integer read GetX;
    property Y: Integer read GetY;
    { The distance the mouse moved since the frame before. This keeps
      changing while the mouse is captured. }
    property XDelta: Integer read GetXDelta;
    property YDelta: Integer read GetYDelta;
    { A captured mouse has its cursor hidden and is kept inside the window,
      and only XDelta and YDelta report its movement }
    property Captured: Boolean read GetCaptured write SetCaptured;
    { Show or hide the mouse cursor }
    property Visible: Boolean read GetVisible write SetVisible;
    { The cursor shown while the mouse is over the scene. Setting it to
      cursorNone hides the cursor and to any other shows it. }
    property Cursor: TMouseCursor read FCursor write SetCursor;
  end;

{ The mouse instance, which is created and scanned when first used }
function Mouse: TMouse;

{ Joysticks }

type
  { The positions of a hat, which is a switch such as a directional pad }
  TJoystickHat = (hatCenter, hatUp, hatRight, hatDown, hatLeft, hatRightUp,
    hatRightDown, hatLeftUp, hatLeftDown);

  { The buttons every gamepad has, named by their place on an Xbox gamepad.
    A is the bottom face button, B the right, X the left and Y the top. }
  TGamepadButton = (gbA, gbB, gbX, gbY, gbBack, gbGuide, gbStart, gbLeftStick,
    gbRightStick, gbLeftShoulder, gbRightShoulder, gbUp, gbDown, gbLeft,
    gbRight);

  { The axes every gamepad has. A stick ranges from -1 to 1, where x is 1 to
    the right and y is 1 when pulled down. A trigger ranges from 0 to 1. }
  TGamepadAxis = (gaLeftX, gaLeftY, gaRightX, gaRightY, gaLeftTrigger,
    gaRightTrigger);

  { The distance a trackball moved since the frame before }
  TJoystickTrackball = record
    XDelta: Integer;
    YDelta: Integer;
  end;

  { TJoystick is a device with any number of axes, buttons, hats and
    trackballs. Reading a part which does not exist returns its resting
    value. }

  TJoystick = class
  private
    FJoystick: Pointer;
    FController: Pointer;
    FName: string;
    FGamepadButtons: array[TGamepadButton] of Boolean;
    FGamepadAxes: array[TGamepadAxis] of Single;
    FAxes: array of Single;
    FButtons: array of Boolean;
    FHats: array of TJoystickHat;
    FTrackballs: array of TJoystickTrackball;
    function GetAttached: Boolean;
    function GetIsGamepad: Boolean;
    function GetGamepadButton(Button: TGamepadButton): Boolean;
    function GetGamepadAxis(Axis: TGamepadAxis): Single;
    function GetAxisCount: Integer;
    function GetAxis(Index: Integer): Single;
    function GetButtonCount: Integer;
    function GetButton(Index: Integer): Boolean;
    function GetHatCount: Integer;
    function GetHat(Index: Integer): TJoystickHat;
    function GetTrackballCount: Integer;
    function GetTrackball(Index: Integer): TJoystickTrackball;
    { Read the state of the joystick }
    procedure Scan;
  public
    { Create takes ownership of a joystick opened by SDL, and of its game
      controller if it is a gamepad }
    constructor Create(Joystick, Controller: Pointer);
    destructor Destroy; override;
    { The name of the joystick }
    property Name: string read FName;
    { Attached is False once the joystick is disconnected }
    property Attached: Boolean read GetAttached;
    { IsGamepad is True if the joystick has the named buttons and axes }
    property IsGamepad: Boolean read GetIsGamepad;
    { A named button is True while it is held down, and is always False if
      the joystick is not a gamepad }
    property GamepadButtons[Button: TGamepadButton]: Boolean read GetGamepadButton;
    { A named axis is always 0 if the joystick is not a gamepad }
    property GamepadAxes[Axis: TGamepadAxis]: Single read GetGamepadAxis;
    property AxisCount: Integer read GetAxisCount;
    { An axis ranges from -1 to 1 and rests at 0, other than a trigger which
      rests at -1 on many gamepads. Typically axes 0 and 1 are x and y of the
      first stick, and y is 1 when the stick is pulled down. The layout
      differs by joystick. }
    property Axes[Index: Integer]: Single read GetAxis;
    property ButtonCount: Integer read GetButtonCount;
    { A button is True while it is held down }
    property Buttons[Index: Integer]: Boolean read GetButton;
    { The number of hats, and the position of each }
    property HatCount: Integer read GetHatCount;
    property Hats[Index: Integer]: TJoystickHat read GetHat;
    { The number of trackballs, and the movement of each }
    property TrackballCount: Integer read GetTrackballCount;
    property Trackballs[Index: Integer]: TJoystickTrackball read GetTrackball;
  end;

  { TJoysticks is the collection of joysticks which are connected. Use the
    Joysticks function to get the one instance. }

  TJoysticks = class
  private
    FInitialized: Boolean;
    FItems: array of TJoystick;
    function GetCount: Integer;
    function GetJoystick(Index: Integer): TJoystick;
    function Find(Handle: Pointer): TJoystick;
    procedure Connect;
    { Find joysticks which were connected or disconnected and read the state
      of each one }
    procedure Scan;
  public
    constructor Create;
    destructor Destroy; override;
    { Return True if a button of a joystick is down, or False if either does
      not exist }
    function ButtonDown(Index, Button: Integer): Boolean;
    { Return the value of an axis of a joystick, or 0 if either does not exist }
    function Axis(Index, AxisIndex: Integer): Single;
    { Return True if a named button of a gamepad is down, or False if the
      joystick does not exist or is not a gamepad }
    function GamepadDown(Index: Integer; Button: TGamepadButton): Boolean;
    { Return the value of a named axis of a gamepad, or 0 if the joystick
      does not exist or is not a gamepad }
    function GamepadAxis(Index: Integer; Named: TGamepadAxis): Single;
    { The number of joysticks }
    property Count: Integer read GetCount;
    { The joysticks by index }
    property Joystick[Index: Integer]: TJoystick read GetJoystick; default;
  end;

{ The joysticks instance, which is created and scanned when first used }
function Joysticks: TJoysticks;

{ Scan the keyboard, mouse and joysticks. Each is only scanned once it has
  been used, so SDL is not asked for hardware a program has no use for.
  Hosts call this once a frame, after the events of SDL were read. Scenes
  do not call it. }
procedure ScanHardware;
{ Read the events of SDL, which updates what it knows of the keyboard and
  mouse. It can be called from any thread. An SDL application reads its own
  events and does not call it. On Windows it does nothing, as the windows of
  SDL get their messages from the message loop of the thread which created
  them. }
procedure PumpHardware;

var
  { Set by a host which has no event loop of SDL, such as the LCL host, to
    have ScanHardware call PumpHardware first. It only does so once the
    keyboard, mouse or joysticks have been used. }
  ScanHardwarePumps: Boolean;

implementation

uses
  Math, CTypes, Codebot.Platform, Codebot.Interop.SDL2, Codebot.Interop.MiniMp3,
  Codebot.Interop.Vorbis, Codebot.Interop.Xmp;

{ Audio }

const
  MinDuration = 0.01;
  MinPosition = 0.001;
  MinVolume = 0.001;
  MaxVolume = 0.999;

{ The decoders are C code which expects floating point exceptions to be
  masked, as they are on the audio thread. These mask them around calls
  made from other threads. }

const
  AllExceptions = [exInvalidOp, exDenormalized, exZeroDivide, exOverflow,
    exUnderflow, exPrecision];

function MaskExceptions: TFPUExceptionMask;
begin
  Result := GetExceptionMask;
  SetExceptionMask(AllExceptions);
end;

procedure UnmaskExceptions(Mask: TFPUExceptionMask);
begin
  ClearExceptions(False);
  SetExceptionMask(Mask);
end;

function CharEquals(P: PByte; Offset: LongWord; const S: string): Boolean;
var
  I: Integer;
begin
  Inc(P, Offset);
  for I := 1 to Length(S) do
  begin
    if P^ <> Ord(S[I]) then
      Exit(False);
    Inc(P);
  end;
  Result := True;
end;

function ReadWord(P: PByte; Offset: LongWord): Word;
begin
  Inc(P, Offset);
  Result := P[0] or (P[1] shl 8);
end;

function ReadLong(P: PByte; Offset: LongWord): LongWord;
begin
  Inc(P, Offset);
  Result := P[0] or (P[1] shl 8) or (P[2] shl 16) or (LongWord(P[3]) shl 24);
end;

{ Wav files }

const
  WaveFormatPcm = 1;
  WaveFormatExtensible = $FFFE;

{ Find the samples of a wav file. The result is False if the memory is not
  a wav file. An exception is raised if it is one but its audio is not
  uncompressed stereo 16 bit samples at 44100 Hz. }

function WaveFind(Source: PByte; Size: LongWord; out Data: PByte; out DataSize: LongWord): Boolean;
var
  Offset, Len: LongWord;
  Tag: Word;
  Found: Boolean;
begin
  Result := False;
  Data := nil;
  DataSize := 0;
  if Size < 12 then
    Exit;
  if not (CharEquals(Source, 0, 'RIFF') and CharEquals(Source, 8, 'WAVE')) then
    Exit;
  Found := False;
  Offset := 12;
  while Size - Offset >= 8 do
  begin
    Len := ReadLong(Source, Offset + 4);
    Inc(Offset, 8);
    if Len > Size - Offset then
      Len := Size - Offset;
    if CharEquals(Source, Offset - 8, 'fmt ') then
    begin
      if Len < 16 then
        raise EAudioFormatException.Create('Invalid wav format');
      Tag := ReadWord(Source, Offset);
      { The extensible format holds the real format at the start of a guid }
      if (Tag = WaveFormatExtensible) and (Len >= 40) then
        Tag := ReadWord(Source, Offset + 24);
      if Tag <> WaveFormatPcm then
        raise EAudioFormatException.Create('Compressed wav audio is not supported');
      if (ReadWord(Source, Offset + 2) <> DefChannels) or
        (ReadLong(Source, Offset + 4) <> DefSampleRate) or
        (ReadWord(Source, Offset + 14) <> DefBitsPerSample) then
        raise EAudioFormatException.Create('Invalid wav channels or sample rate');
      Found := True;
    end
    else if CharEquals(Source, Offset - 8, 'data') then
    begin
      if not Found then
        raise EAudioFormatException.Create('Invalid wav format');
      Data := Source;
      Inc(Data, Offset);
      DataSize := Len and not LongWord(3);
      Exit(True);
    end;
    { Chunks are padded to an even size }
    Inc(Len, Len and 1);
    if Len > Size - Offset then
      Break;
    Inc(Offset, Len);
  end;
  raise EAudioFormatException.Create('Invalid wav data');
end;

function WaveTime(Source: PByte; Size: LongWord; out Duration: Single): Boolean;
var
  Data: PByte;
  DataSize: LongWord;
begin
  Duration := 0;
  Result := WaveFind(Source, Size, Data, DataSize);
  if Result then
    Duration := (DataSize div 4) / DefSampleRate;
end;

{ Ogg vorbis files }

type
  { The memory the vorbis callbacks read from }
  TMemoryReader = record
    Data: PByte;
    Size: Int64;
    Position: Int64;
  end;
  PMemoryReader = ^TMemoryReader;

function VorbisRead(mem: Pointer; size, count: csize_t; datasource: Pointer): csize_t; cdecl;
var
  R: PMemoryReader absolute datasource;
  N: Int64;
begin
  if size = 0 then
    Exit(0);
  N := (R.Size - R.Position) div size;
  if N > count then
    N := count;
  if N < 1 then
    Exit(0);
  Move(R.Data[R.Position], mem^, N * size);
  Inc(R.Position, N * size);
  Result := N;
end;

function VorbisSeek(datasource: Pointer; offset: cint64; whence: cint): cint; cdecl;
var
  R: PMemoryReader absolute datasource;
begin
  case whence of
    0: ;
    1: offset := R.Position + offset;
    2: offset := R.Size + offset;
  else
    Exit(-1);
  end;
  if (offset < 0) or (offset > R.Size) then
    Exit(-1);
  R.Position := offset;
  Result := 0;
end;

function VorbisTell(datasource: Pointer): clong; cdecl;
var
  R: PMemoryReader absolute datasource;
begin
  Result := R.Position;
end;

{ Reader must stay at the same address until VorbisClose }

function VorbisOpen(var Reader: TMemoryReader; Source: PByte; Size: LongWord;
  out V: TOggVorbisFile): Boolean;
var
  C: TOVCallbacks;
begin
  Reader.Data := Source;
  Reader.Size := Size;
  Reader.Position := 0;
  C.read_func := @VorbisRead;
  C.seek_func := @VorbisSeek;
  C.close_func := nil;
  C.tell_func := @VorbisTell;
  V := ov_create;
  Result := ov_open_callbacks(@Reader, V, nil, 0, C) = 0;
  if not Result then
  begin
    ov_destroy(V);
    V := nil;
  end;
end;

procedure VorbisClose(V: TOggVorbisFile);
begin
  if V = nil then
    Exit;
  ov_clear(V);
  ov_destroy(V);
end;

function VorbisTime(Source: PByte; Size: LongWord; out Duration: Single): Boolean;
var
  R: TMemoryReader;
  V: TOggVorbisFile;
  I: PVorbisInfo;
begin
  Result := False;
  Duration := 0;
  if Size < $40 then
    Exit;
  if not (CharEquals(Source, 0, 'OggS') and CharEquals(Source, $1D, 'vorbis')) then
    Exit;
  if not VorbisOpen(R, Source, Size, V) then
    Exit;
  try
    I := ov_info(V, -1);
    if I = nil then
      raise EAudioFormatException.Create('Invalid ogg vorbis information');
    if (I.channels <> DefChannels) or (I.rate <> DefSampleRate) then
      raise EAudioFormatException.Create('Invalid ogg vorbis channels or sample rate');
    Duration := ov_time_total(V, -1);
    Result := True;
  finally
    VorbisClose(V);
  end;
end;

{ Mp3 files }

{ Find the duration of an mp3 file by decoding every frame. If Offsets is
  given it receives the offset of the data each frame is decoded from. }

function Mp3Time(Source: PByte; Size: LongWord; out Duration: Single;
  Offsets: Pointer = nil): Boolean;
type
  PAudioOffsets = ^TAudioOffsets;
var
  Decoder: PMp3Dec;
  PCM: PMp3Sample;
  Data: PByte;
  DataSize, Frames, Offset: LongWord;
  Samples: Integer;
  Info: TMp3DecFrameInfo;
  List: PAudioOffsets absolute Offsets;
begin
  Result := False;
  Duration := 0;
  if Size < $10 then
    Exit;
  if not (CharEquals(Source, 0, 'ID3') or
    ((Source[0] = $FF) and (Source[1] and $E0 = $E0))) then
    Exit;
  Decoder := GetMem(SizeOf(TMp3Dec));
  PCM := GetMem(MINIMP3_BYTES_PER_FRAME);
  try
    Data := Source;
    DataSize := Size;
    Offset := 0;
    Frames := 0;
    mp3dec_init(Decoder);
    while DataSize > 0 do
    begin
      Samples := mp3dec_decode_frame(Decoder, Data, DataSize, PCM, Info);
      if Info.frame_bytes < 1 then
        Break;
      if Samples = MINIMP3_SAMPLES_PER_FRAME then
      begin
        if (Info.channels <> DefChannels) or (Info.hz <> DefSampleRate) then
          raise EAudioFormatException.Create('Invalid mp3 channels or sample rate');
        if List <> nil then
        begin
          if Frames = LongWord(Length(List^)) then
            SetLength(List^, Frames * 2 + 1024);
          List^[Frames] := Offset;
        end;
        Inc(Frames);
      end;
      Inc(Offset, Info.frame_bytes);
      Inc(Data, Info.frame_bytes);
      if LongWord(Info.frame_bytes) < DataSize then
        Dec(DataSize, Info.frame_bytes)
      else
        DataSize := 0;
    end;
    if List <> nil then
      SetLength(List^, Frames);
    Result := Frames > 0;
    if Result then
      Duration := (Frames * MINIMP3_SAMPLES_PER_FRAME) / DefSampleRate;
  finally
    FreeMem(PCM);
    FreeMem(Decoder);
  end;
end;

{ Tracker music }

function TrackerTest(Source: PByte; Size: LongWord): Boolean;
var
  Info: TXmpTestInfo;
begin
  Result := False;
  if Size < $10 then
    Exit;
  Result := xmp_test_module_from_memory(Source, Size, Info) = 0;
end;

{ Find the duration of a module by playing it once without its samples }

function TrackerTime(Source: PByte; Size: LongWord; out Duration: Single): Boolean;
const
  BufferSize = 1024;
var
  Buffer: PByte;
  C: TXmpContext;
  I: Integer;
begin
  Duration := 0;
  Result := TrackerTest(Source, Size);
  if not Result then
    Exit;
  Result := False;
  Buffer := GetMem(BufferSize);
  C := xmp_create_context;
  try
    xmp_set_player(C, XMP_PLAYER_SMPCTL, XMP_SMPCTL_SKIP);
    if xmp_load_module_from_memory(C, Source, Size) <> 0 then
      Exit;
    if xmp_start_player(C, DefSampleRate, 0) = 0 then
    begin
      I := 0;
      while xmp_play_buffer(C, Buffer, BufferSize, 1) = 0 do
        Inc(I);
      Duration := I * (BufferSize div 4) / DefSampleRate;
      xmp_end_player(C);
      Result := True;
    end;
    xmp_release_module(C);
  finally
    xmp_free_context(C);
    FreeMem(Buffer);
  end;
end;

{ Format detection }

function DetectTime(Source: PByte; Size: LongWord; out Duration: Single;
  Offsets: Pointer = nil): TAudioFormat;
var
  Mask: TFPUExceptionMask;
begin
  Mask := MaskExceptions;
  try
    if WaveTime(Source, Size, Duration) then
      Result := afWave
    else if VorbisTime(Source, Size, Duration) then
      Result := afVorbis
    else if Mp3Time(Source, Size, Duration, Offsets) then
      Result := afMp3
    else if TrackerTime(Source, Size, Duration) then
      Result := afTracker
    else
      Result := afUnsupported;
  finally
    UnmaskExceptions(Mask);
  end;
end;

{ Read a whole stream into memory which the caller frees }

function StreamRead(Stream: TStream; out Size: LongWord): PByte;
begin
  Stream.Position := 0;
  Size := Stream.Size;
  Result := GetMem(Size + 1);
  try
    Stream.ReadBuffer(Result^, Size);
  except
    FreeMem(Result);
    raise;
  end;
end;

function AudioFormatDetect(const FileName: string): TAudioFormat;
var
  Duration: Single;
begin
  Result := AudioFormatTime(FileName, Duration);
end;

function AudioFormatDetect(Stream: TStream): TAudioFormat;
var
  Duration: Single;
begin
  Result := AudioFormatTime(Stream, Duration);
end;

function AudioFormatTime(const FileName: string; out Duration: Single): TAudioFormat;
var
  S: TStream;
begin
  Result := afUnsupported;
  Duration := 0;
  if not FileExists(FileName) then
    Exit;
  S := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := AudioFormatTime(S, Duration);
  finally
    S.Free;
  end;
end;

function AudioFormatTime(Stream: TStream; out Duration: Single): TAudioFormat;
var
  Source: PByte;
  Size: LongWord;
begin
  Source := StreamRead(Stream, Size);
  try
    Result := DetectTime(Source, Size, Duration);
  finally
    FreeMem(Source);
  end;
end;

{ TAudioSource }

var
  InternalSourceId: Integer;

constructor TAudioSource.Create(const Name: string; Source: PByte; Size: LongWord);
begin
  inherited Create;
  FSource := Source;
  FSize := Size;
  FFormat := DetectTime(Source, Size, FDuration, @FOffsets);
  if FFormat = afUnsupported then
    raise EAudioFormatException.Create('Invalid audio source format');
  if FDuration < MinDuration then
    raise EAudioException.Create('Invalid audio source duration');
  Inc(InternalSourceId);
  FId := InternalSourceId;
  FName := Name;
end;

destructor TAudioSource.Destroy;
begin
  FreeMem(FSource);
  inherited Destroy;
end;

{ TAudioDecoder }

constructor TAudioDecoder.Create(Source: TAudioSource);
begin
  inherited Create;
  FSource := Source;
  FMaxPosition := Round(Source.FDuration * DefSampleRate);
end;

function TAudioDecoder.GetPosition: Single;
begin
  Result := FPosition / DefSampleRate;
end;

{ TWaveDecoder }

type
  TWaveDecoder = class(TAudioDecoder)
  private
    FData: PByte;
  protected
    procedure SetPosition(Value: Single); override;
  public
    constructor Create(Source: TAudioSource); override;
    function Decode(Samples: PByte): Boolean; override;
  end;

constructor TWaveDecoder.Create(Source: TAudioSource);
var
  Size: LongWord;
begin
  inherited Create(Source);
  WaveFind(Source.FSource, Source.FSize, FData, Size);
  FMaxPosition := Size div 4;
end;

function TWaveDecoder.Decode(Samples: PByte): Boolean;
var
  C: LongWord;
begin
  FDecoded := 0;
  Result := FPosition < FMaxPosition;
  if not Result then
    Exit;
  C := FMaxPosition - FPosition;
  if C > DefMixingSamples then
    C := DefMixingSamples;
  Move(FData[FPosition * 4], Samples^, C * 4);
  Inc(FPosition, C);
  FDecoded := C;
end;

procedure TWaveDecoder.SetPosition(Value: Single);
begin
  if Value < MinPosition then
    FPosition := 0
  else
  begin
    FPosition := Round(Value * DefSampleRate);
    if FPosition > FMaxPosition then
      FPosition := FMaxPosition;
  end;
end;

{ TVorbisDecoder }

type
  TVorbisDecoder = class(TAudioDecoder)
  private
    FReader: TMemoryReader;
    FVorbis: TOggVorbisFile;
  protected
    procedure SetPosition(Value: Single); override;
  public
    constructor Create(Source: TAudioSource); override;
    destructor Destroy; override;
    function Decode(Samples: PByte): Boolean; override;
  end;

constructor TVorbisDecoder.Create(Source: TAudioSource);
begin
  inherited Create(Source);
  if not VorbisOpen(FReader, Source.FSource, Source.FSize, FVorbis) then
    raise EAudioFormatException.Create('Invalid ogg vorbis information');
end;

destructor TVorbisDecoder.Destroy;
begin
  VorbisClose(FVorbis);
  inherited Destroy;
end;

function TVorbisDecoder.Decode(Samples: PByte): Boolean;
var
  S, I: Integer;
begin
  S := DefMixingSize;
  I := ov_read(FVorbis, Samples, S, 0, 2, 1, nil);
  Result := I > 0;
  while I > 0 do
  begin
    Inc(FPosition, I div 4);
    Inc(Samples, I);
    Dec(S, I);
    if S < 4 then
      Break;
    I := ov_read(FVorbis, Samples, S, 0, 2, 1, nil);
  end;
  FDecoded := (DefMixingSize - S) div 4;
end;

procedure TVorbisDecoder.SetPosition(Value: Single);
begin
  if Value < MinPosition then
    FPosition := 0
  else
  begin
    FPosition := Round(Value * DefSampleRate);
    if FPosition > FMaxPosition then
      FPosition := FMaxPosition;
  end;
  ov_pcm_seek_page(FVorbis, FPosition);
  FPosition := ov_pcm_tell(FVorbis);
end;

{ TMp3Decoder }

type
  TMp3Decoder = class(TAudioDecoder)
  private
    FCursor: PByte;
    FCursorSize: Integer;
    FDecode: TMp3Dec;
    FFrame: PByte;
    FDecodeBuffer: PByte;
    FDecodeBytes: Integer;
  protected
    procedure SetPosition(Value: Single); override;
  public
    constructor Create(Source: TAudioSource); override;
    destructor Destroy; override;
    function Decode(Samples: PByte): Boolean; override;
  end;

constructor TMp3Decoder.Create(Source: TAudioSource);
begin
  inherited Create(Source);
  FMaxPosition := Length(Source.FOffsets) * MINIMP3_SAMPLES_PER_FRAME;
  FFrame := GetMem(MINIMP3_BYTES_PER_FRAME);
  FCursor := Source.FSource;
  FCursorSize := Source.FSize;
  mp3dec_init(@FDecode);
end;

destructor TMp3Decoder.Destroy;
begin
  FreeMem(FFrame);
  inherited Destroy;
end;

function TMp3Decoder.Decode(Samples: PByte): Boolean;
var
  Info: TMp3DecFrameInfo;
  Count, I: Integer;
begin
  FDecoded := 0;
  Result := FPosition < FMaxPosition;
  if not Result then
    Exit;
  Count := DefMixingSize;
  while Count > 0 do
  begin
    if FDecodeBytes = 0 then
    begin
      if FCursorSize < 1 then
      begin
        FPosition := FMaxPosition;
        FDecoded := (DefMixingSize - Count) div 4;
        Exit;
      end;
      I := mp3dec_decode_frame(@FDecode, FCursor, FCursorSize, PMp3Sample(FFrame), Info);
      if Info.frame_bytes < 1 then
      begin
        FCursorSize := 0;
        FPosition := FMaxPosition;
        FDecoded := (DefMixingSize - Count) div 4;
        Exit;
      end;
      Inc(FCursor, Info.frame_bytes);
      Dec(FCursorSize, Info.frame_bytes);
      { Tags and frames which are not whole are skipped }
      if I <> MINIMP3_SAMPLES_PER_FRAME then
        Continue;
      FDecodeBuffer := FFrame;
      FDecodeBytes := MINIMP3_BYTES_PER_FRAME;
    end;
    I := Min(FDecodeBytes, Count);
    Move(FDecodeBuffer^, Samples^, I);
    Inc(FDecodeBuffer, I);
    Inc(Samples, I);
    Dec(FDecodeBytes, I);
    Dec(Count, I);
  end;
  FDecoded := DefMixingSamples;
  Inc(FPosition, DefMixingSamples);
  if FPosition > FMaxPosition then
    FPosition := FMaxPosition;
end;

{ An mp3 can only be decoded from the start of a frame }

procedure TMp3Decoder.SetPosition(Value: Single);
var
  I: Integer;
begin
  FCursor := FSource.FSource;
  FCursorSize := FSource.FSize;
  FDecodeBytes := 0;
  mp3dec_init(@FDecode);
  if Value < MinPosition then
    I := 0
  else
    I := Round(Value * DefSampleRate) div MINIMP3_SAMPLES_PER_FRAME;
  if I < 1 then
    FPosition := 0
  else if I > Length(FSource.FOffsets) - 1 then
  begin
    FPosition := FMaxPosition;
    FCursorSize := 0;
  end
  else
  begin
    FPosition := I * MINIMP3_SAMPLES_PER_FRAME;
    Inc(FCursor, FSource.FOffsets[I]);
    Dec(FCursorSize, FSource.FOffsets[I]);
  end;
end;

{ TTrackerDecoder }

type
  TTrackerDecoder = class(TAudioDecoder)
  private
    FInfo: TXmpFrameInfo;
    FContext: TXmpContext;
    FLoaded: Boolean;
  protected
    procedure SetPosition(Value: Single); override;
  public
    constructor Create(Source: TAudioSource); override;
    destructor Destroy; override;
    function Decode(Samples: PByte): Boolean; override;
  end;

constructor TTrackerDecoder.Create(Source: TAudioSource);
begin
  inherited Create(Source);
  FContext := xmp_create_context;
  if xmp_load_module_from_memory(FContext, Source.FSource, Source.FSize) <> 0 then
    raise EAudioFormatException.Create('Invalid tracker music');
  FLoaded := True;
  xmp_start_player(FContext, DefSampleRate, 0);
end;

destructor TTrackerDecoder.Destroy;
begin
  if FLoaded then
  begin
    xmp_end_player(FContext);
    xmp_release_module(FContext);
  end;
  if FContext <> nil then
    xmp_free_context(FContext);
  inherited Destroy;
end;

function TTrackerDecoder.Decode(Samples: PByte): Boolean;
var
  I: Integer;
begin
  FDecoded := 0;
  Result := FPosition < FMaxPosition;
  if not Result then
    Exit;
  I := DefMixingSize;
  if FMaxPosition - FPosition < DefMixingSamples then
    I := (FMaxPosition - FPosition) * 4;
  if xmp_play_buffer(FContext, Samples, I, 0) = 0 then
  begin
    Inc(FPosition, I div 4);
    FDecoded := I div 4;
  end
  else
    FPosition := FMaxPosition;
end;

{ A module can only be played from the start of a pattern position }

procedure TTrackerDecoder.SetPosition(Value: Single);
var
  Sample: LongWord;
begin
  if Value < MinPosition then
  begin
    FPosition := 0;
    xmp_seek_time(FContext, 0);
    Exit;
  end;
  FPosition := Round(Value * DefSampleRate);
  if FPosition >= FMaxPosition then
  begin
    FPosition := FMaxPosition;
    Exit;
  end;
  xmp_seek_time(FContext, Round(FPosition / DefSampleRate * 1000));
  { The time seeked to is known after a frame is played }
  xmp_play_buffer(FContext, @Sample, SizeOf(Sample), 0);
  xmp_get_frame_info(FContext, @FInfo);
  FPosition := Round(FInfo.time / 1000 * DefSampleRate);
  if FPosition > FMaxPosition then
    FPosition := FMaxPosition;
end;

{ TAudioBank }

constructor TAudioBank.Create(Audio: TAudio);
begin
  inherited Create;
  FAudio := Audio;
  FSamples := GetMem(DefMixingSize);
  FBlock := GetMem(DefMixingSize);
  FCarry := GetMem(DefMixingSize);
  FPaused := True;
  Reset;
end;

destructor TAudioBank.Destroy;
begin
  FDecoder.Free;
  FreeMem(FCarry);
  FreeMem(FBlock);
  FreeMem(FSamples);
  inherited Destroy;
end;

{ Decode is called on the audio thread while audio is locked }

function TAudioBank.Decode: Boolean;
var
  Fill, Take, N, Tries: Integer;
begin
  Result := False;
  if (FDecoder = nil) or FPaused or FCompleted then
    Exit;
  FillChar(FSamples^, DefMixingSize, 0);
  { Samples decoded for the block before which did not fit come first }
  Fill := FCarryCount;
  if Fill > 0 then
    Move(FCarry^, FSamples^, Fill * 4);
  FCarryCount := 0;
  { A source ends part way through a block. When the bank is to play it
    again the rest of the block is filled from its start, so a loop has no
    gap. }
  Tries := 0;
  while Fill < DefMixingSamples do
  begin
    Inc(Tries);
    if Tries > 8 then
      Break;
    FillChar(FBlock^, DefMixingSize, 0);
    if not FDecoder.Decode(FBlock) then
    begin
      Inc(FLoop);
      FCompleted := (FLoopCount > 0) and (FLoop >= FLoopCount);
      if FCompleted then
      begin
        FPaused := True;
        Break;
      end;
      FDecoder.Position := 0;
      if not FDecoder.Decode(FBlock) then
        Break;
    end;
    N := FDecoder.FDecoded;
    if N > DefMixingSamples then
      N := DefMixingSamples;
    Take := DefMixingSamples - Fill;
    if Take > N then
      Take := N;
    Move(FBlock^, FSamples[Fill * 4], Take * 4);
    Inc(Fill, Take);
    if N > Take then
    begin
      FCarryCount := N - Take;
      Move(FBlock[Take * 4], FCarry^, FCarryCount * 4);
    end;
  end;
  Result := Fill > 0;
end;

procedure TAudioBank.Load(Source: TAudioSource);
var
  Mask: TFPUExceptionMask;
begin
  FAudio.Lock;
  try
    Reset;
    FSource := nil;
    FCarryCount := 0;
    FreeAndNil(FDecoder);
    if Source = nil then
      Exit;
    Mask := MaskExceptions;
    try
      case Source.Format of
        afWave: FDecoder := TWaveDecoder.Create(Source);
        afVorbis: FDecoder := TVorbisDecoder.Create(Source);
        afMp3: FDecoder := TMp3Decoder.Create(Source);
        afTracker: FDecoder := TTrackerDecoder.Create(Source);
      else
        raise EAudioFormatException.Create('Audio format is unsupported');
      end;
    finally
      UnmaskExceptions(Mask);
    end;
    FSource := Source;
  finally
    FAudio.Unlock;
  end;
end;

procedure TAudioBank.Load(const Name: string);
begin
  Load(FAudio.Source(Name));
end;

procedure TAudioBank.Load(Id: Integer);
begin
  Load(FAudio.Source(Id));
end;

procedure TAudioBank.Unload;
begin
  Load(nil);
end;

{ The fade is a gain from 0 to 1 which moves toward its target by a fixed
  amount for every sample }

function FadeRate(Seconds: Single): Single;
begin
  Result := 1 / (Seconds * DefSampleRate);
end;

procedure TAudioBank.FadeIn(Seconds: Single);
begin
  if FDecoder = nil then
    Exit;
  FAudio.Lock;
  try
    if FCompleted then
      Position := 0;
    if FPaused then
      FFade := 0;
    FFadeTarget := 1;
    if Seconds < MinDuration then
      FFade := 1
    else
      FFadeRate := FadeRate(Seconds);
    FPaused := False;
  finally
    FAudio.Unlock;
  end;
end;

procedure TAudioBank.FadeOut(Seconds: Single);
begin
  if FPaused then
    Exit;
  FAudio.Lock;
  try
    FFadeTarget := 0;
    if Seconds < MinDuration then
    begin
      FFade := 0;
      FPaused := True;
    end
    else
      FFadeRate := FadeRate(Seconds);
  finally
    FAudio.Unlock;
  end;
end;

{ ApplyFade is called on the audio thread after a block is decoded. While
  the bank is fading the samples of the block are scaled by the fade as it
  moves, and a fade which reaches silence pauses the bank. }

procedure TAudioBank.ApplyFade;
var
  S: PSmallInt;
  G: Single;
  I: Integer;
begin
  if FFade = FFadeTarget then
    Exit;
  S := PSmallInt(FSamples);
  G := FFade;
  for I := 1 to DefMixingSamples do
  begin
    if G < FFadeTarget then
    begin
      G := G + FFadeRate;
      if G > FFadeTarget then
        G := FFadeTarget;
    end
    else if G > FFadeTarget then
    begin
      G := G - FFadeRate;
      if G < FFadeTarget then
        G := FFadeTarget;
    end;
    S[0] := Trunc(S[0] * G);
    S[1] := Trunc(S[1] * G);
    Inc(S, 2);
  end;
  FFade := G;
  if (FFade = 0) and (FFadeTarget = 0) then
    FPaused := True;
end;

procedure TAudioBank.Reset;
begin
  FAudio.Lock;
  try
    FPaused := True;
    FLoopCount := 1;
    FMuted := False;
    FPan := 0;
    FVolume := 1;
    FFade := 1;
    FFadeTarget := 1;
    FFadeRate := 0;
    Position := 0;
    FCompleted := False;
    FLoop := 0;
    Inc(FAudio.FTouchCount);
    FTouched := FAudio.FTouchCount;
  finally
    FAudio.Unlock;
  end;
end;

procedure TAudioBank.SetPaused(Value: Boolean);
begin
  if FDecoder = nil then
    Value := True;
  if Value = FPaused then
    Exit;
  FAudio.Lock;
  FPaused := Value;
  if not Value then
  begin
    FFade := 1;
    FFadeTarget := 1;
  end;
  FAudio.Unlock;
end;

procedure TAudioBank.SetLoopCount(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if Value = FLoopCount then
    Exit;
  FAudio.Lock;
  FLoopCount := Value;
  FAudio.Unlock;
end;

procedure TAudioBank.SetMuted(Value: Boolean);
begin
  FMuted := Value;
end;

procedure TAudioBank.SetPan(Value: Single);
begin
  if Value < -1 then
    Value := -1
  else if Value > 1 then
    Value := 1;
  FPan := Value;
end;

function TAudioBank.GetDuration: Single;
begin
  if FSource = nil then
    Result := 0
  else
    Result := FSource.Duration;
end;

function TAudioBank.GetPosition: Single;
begin
  if FDecoder = nil then
    Result := 0
  else
    Result := FDecoder.Position;
end;

procedure TAudioBank.SetPosition(Value: Single);
var
  Mask: TFPUExceptionMask;
begin
  if FDecoder = nil then
    Exit;
  if Value < MinPosition then
    Value := 0
  else if Value > FSource.Duration then
    Value := FSource.Duration;
  FAudio.Lock;
  Mask := MaskExceptions;
  try
    FCompleted := False;
    FLoop := 0;
    FCarryCount := 0;
    FDecoder.Position := Value;
  finally
    UnmaskExceptions(Mask);
    FAudio.Unlock;
  end;
end;

function TAudioBank.GetVolume: Single;
begin
  if FMuted then
    Result := 0
  else
    Result := FVolume;
end;

procedure TAudioBank.SetVolume(Value: Single);
begin
  if Value < MinVolume then
    Value := 0
  else if Value > MaxVolume then
    Value := 1;
  FVolume := Value;
end;

{ Mixing }

{ Add the block a bank decoded to the mixer using its volume and pan }

procedure BankMix(Bank: TAudioBank; Mixer: PSingle);
var
  F: PSingle;
  S: PSmallInt;
  P, V, LL, LR, RL, RR: Single;
  I: Integer;
begin
  F := Mixer;
  S := PSmallInt(Bank.FSamples);
  P := Bank.FPan;
  V := Bank.Volume;
  I := DefMixingSamples;
  if P = 0 then
    while I > 0 do
    begin
      F[0] := F[0] + S[0] * V;
      F[1] := F[1] + S[1] * V;
      Inc(F, 2);
      Inc(S, 2);
      Dec(I);
    end
  else
  begin
    if P < 0 then
    begin
      LL := 1;
      LR := -P;
      RL := 0;
      RR := 1 + P;
    end
    else
    begin
      LL := 1 - P;
      LR := 0;
      RL := P;
      RR := 1;
    end;
    LL := LL * V;
    LR := LR * V;
    RL := RL * V;
    RR := RR * V;
    while I > 0 do
    begin
      F[0] := F[0] + S[0] * LL + S[1] * LR;
      F[1] := F[1] + S[1] * RR + S[0] * RL;
      Inc(F, 2);
      Inc(S, 2);
      Dec(I);
    end;
  end;
end;

{ Mix the next block from every bank which is playing. This is called on
  the audio thread while audio is locked. }

procedure TAudio.MixBlock;
const
  H = High(SmallInt);
  L = Low(SmallInt);
var
  Bank: TAudioBank;
  F: PSingle;
  S: PSmallInt;
  V, A: Single;
  I: Integer;
begin
  V := FVolume;
  FillChar(FMixer^, DefMixingSamples * DefChannels * SizeOf(Single), 0);
  for I := 0 to DefAudioBankCount - 1 do
  begin
    Bank := FBanks[I];
    if Bank = nil then
      Continue;
    if Bank.Decode then
    begin
      Bank.ApplyFade;
      if (Bank.Volume > MinVolume) and (V > MinVolume) then
        BankMix(Bank, FMixer);
    end;
  end;
  { Sound the program generates is added to that of the banks. The mixer
    works at the scale of 16 bit samples. }
  if Assigned(FOnMix) then
  begin
    FillChar(FGenerated^, DefMixingSamples * SizeOf(TAudioSample), 0);
    FOnMix(Self, FGenerated, DefMixingSamples);
    F := FMixer;
    A := High(SmallInt);
    for I := 0 to DefMixingSamples - 1 do
    begin
      F[0] := F[0] + FGenerated[I].Left * A;
      F[1] := F[1] + FGenerated[I].Right * A;
      Inc(F, 2);
    end;
  end;
  F := FMixer;
  S := PSmallInt(FBlock);
  for I := 1 to DefMixingSamples * DefChannels do
  begin
    A := F^ * V;
    if A > H then
      S^ := H
    else if A < L then
      S^ := L
    else
      S^ := Trunc(A);
    Inc(F);
    Inc(S);
  end;
end;

{ SDL asks for any number of bytes, which are taken from mixed blocks }

procedure AudioCallback(userdata: Pointer; stream: PUint8; len: LongInt); cdecl;
var
  A: TAudio absolute userdata;
  I: Integer;
begin
  while len > 0 do
  begin
    if A.FBlockBytes = 0 then
    begin
      A.MixBlock;
      A.FBlockBytes := DefMixingSize;
    end;
    I := Min(len, A.FBlockBytes);
    Move(A.FBlock[DefMixingSize - A.FBlockBytes], stream^, I);
    Dec(A.FBlockBytes, I);
    Inc(stream, I);
    Dec(len, I);
  end;
end;

{ TAudio }

constructor TAudio.Create;
var
  Spec, Obtained: TSDL_AudioSpec;
begin
  inherited Create;
  FPaused := True;
  FVolume := 1;
  FMixer := GetMem(DefMixingSamples * DefChannels * SizeOf(Single));
  FBlock := GetMem(DefMixingSize);
  FGenerated := GetMem(DefMixingSamples * SizeOf(TAudioSample));
  FInitialized := SDL_InitSubSystem(SDL_INIT_AUDIO) = 0;
  if not FInitialized then
    Exit;
  FillChar(Spec, SizeOf(Spec), 0);
  Spec.freq := DefSampleRate;
  Spec.format := AUDIO_S16;
  Spec.channels := DefChannels;
  Spec.samples := DefMixingSamples;
  Spec.callback := @AudioCallback;
  Spec.userdata := Self;
  { SDL converts to the format of the device, and the device is opened paused }
  FDevice := SDL_OpenAudioDevice(nil, 0, @Spec, @Obtained, 0);
end;

destructor TAudio.Destroy;
var
  I: Integer;
begin
  if FDevice <> 0 then
    SDL_CloseAudioDevice(FDevice);
  FDevice := 0;
  if FInitialized then
    SDL_QuitSubSystem(SDL_INIT_AUDIO);
  for I := 0 to DefAudioBankCount - 1 do
    FBanks[I].Free;
  for I := 0 to Length(FSources) - 1 do
    FSources[I].Free;
  FreeMem(FGenerated);
  FreeMem(FBlock);
  FreeMem(FMixer);
  inherited Destroy;
end;

{ Lock keeps the audio thread from mixing and can be nested }

procedure TAudio.Lock;
begin
  if FDevice <> 0 then
    SDL_LockAudioDevice(FDevice);
end;

procedure TAudio.Unlock;
begin
  if FDevice <> 0 then
    SDL_UnlockAudioDevice(FDevice);
end;

function TAudio.NewBank(Index: Integer): TAudioBank;
begin
  Result := TAudioBank.Create(Self);
  Lock;
  FBanks[Index] := Result;
  Unlock;
end;

function TAudio.GetAvailable: Boolean;
begin
  Result := FDevice <> 0;
end;

function TAudio.BankExists(Index: Integer): Boolean;
begin
  Result := (Index >= 0) and (Index < DefAudioBankCount) and (FBanks[Index] <> nil);
end;

function TAudio.IsPlaying: Boolean;
var
  I: Integer;
begin
  Result := False;
  if FPaused then
    Exit;
  for I := 0 to DefAudioBankCount - 1 do
    if (FBanks[I] <> nil) and not FBanks[I].Paused then
      Exit(True);
end;

function TAudio.Add(const Name: string; const FileName: string): TAudioSource;
var
  S: TStream;
begin
  S := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := Add(Name, S);
  finally
    S.Free;
  end;
end;

function TAudio.Add(const Name: string; Stream: TStream): TAudioSource;
var
  Size: LongWord;
  Data: PByte;
begin
  if Name = '' then
    raise EAudioException.Create('Invalid audio source name');
  if Source(Name) <> nil then
    raise EAudioException.Create('Duplicate audio source name');
  Data := StreamRead(Stream, Size);
  { The source frees the memory, even when its constructor fails }
  Result := TAudioSource.Create(Name, Data, Size);
  SetLength(FSources, Length(FSources) + 1);
  FSources[Length(FSources) - 1] := Result;
end;

procedure TAudio.Remove(Source: TAudioSource);
var
  I, J: Integer;
begin
  if Source = nil then
    Exit;
  for I := 0 to DefAudioBankCount - 1 do
    if (FBanks[I] <> nil) and (FBanks[I].Source = Source) then
      FBanks[I].Unload;
  for I := 0 to Length(FSources) - 1 do
    if FSources[I] = Source then
    begin
      for J := I to Length(FSources) - 2 do
        FSources[J] := FSources[J + 1];
      SetLength(FSources, Length(FSources) - 1);
      Source.Free;
      Break;
    end;
end;

function TAudio.Source(const Name: string): TAudioSource;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Length(FSources) - 1 do
    if FSources[I].Name = Name then
      Exit(FSources[I]);
end;

function TAudio.Source(Id: Integer): TAudioSource;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Length(FSources) - 1 do
    if FSources[I].Id = Id then
      Exit(FSources[I]);
end;

function TAudio.Next: TAudioBank;
var
  B: TAudioBank;
  I: Integer;
begin
  for I := 0 to DefAudioBankCount - 1 do
  begin
    B := FBanks[I];
    if (B <> nil) and B.Completed and not B.Reserved then
      Exit(B);
  end;
  for I := 0 to DefAudioBankCount - 1 do
    if FBanks[I] = nil then
      Exit(NewBank(I));
  Result := nil;
  for I := 0 to DefAudioBankCount - 1 do
  begin
    B := FBanks[I];
    if B.Reserved then
      Continue;
    if (Result = nil) or (B.FTouched < Result.FTouched) then
      Result := B;
  end;
  if Result = nil then
    raise EAudioException.Create('Out of audio banks');
end;

function TAudio.Play(Source: TAudioSource; LoopCount: Integer = 1;
  FadeIn: Single = 0): TAudioBank;
var
  B: TAudioBank;
  I: Integer;
begin
  if Source = nil then
    raise EAudioException.Create('Audio source not found');
  { A completed bank which holds the source does not need to load it }
  for I := 0 to DefAudioBankCount - 1 do
  begin
    B := FBanks[I];
    if (B <> nil) and B.Completed and (B.Source = Source) and not B.Reserved then
    begin
      B.Reset;
      B.LoopCount := LoopCount;
      B.FadeIn(FadeIn);
      Exit(B);
    end;
  end;
  Result := Next;
  Result.Load(Source);
  Result.LoopCount := LoopCount;
  Result.FadeIn(FadeIn);
end;

function TAudio.Play(const Name: string; LoopCount: Integer = 1;
  FadeIn: Single = 0): TAudioBank;
begin
  Result := Play(Source(Name), LoopCount, FadeIn);
end;

function TAudio.Play(Id: Integer; LoopCount: Integer = 1;
  FadeIn: Single = 0): TAudioBank;
begin
  Result := Play(Source(Id), LoopCount, FadeIn);
end;

procedure TAudio.SetPaused(Value: Boolean);
begin
  if Value = FPaused then
    Exit;
  FPaused := Value;
  if FDevice <> 0 then
    SDL_PauseAudioDevice(FDevice, Ord(Value));
end;

{ The event is changed while the audio thread is not mixing }

procedure TAudio.SetOnMix(Value: TAudioMixEvent);
begin
  Lock;
  FOnMix := Value;
  Unlock;
end;

procedure TAudio.SetVolume(Value: Single);
begin
  if Value < MinVolume then
    Value := 0
  else if Value > MaxVolume then
    Value := 1;
  FVolume := Value;
end;

function TAudio.GetSource(Index: Integer): TAudioSource;
begin
  if (Index < 0) or (Index >= Length(FSources)) then
    raise ERangeError.CreateFmt('Audio source index %d is out of range', [Index]);
  Result := FSources[Index];
end;

function TAudio.GetSourceCount: Integer;
begin
  Result := Length(FSources);
end;

function TAudio.GetBank(Index: Integer): TAudioBank;
begin
  if (Index < 0) or (Index >= DefAudioBankCount) then
    raise ERangeError.CreateFmt('Audio bank index %d is out of range', [Index]);
  Result := FBanks[Index];
  if Result = nil then
    Result := NewBank(Index);
end;

function TAudio.GetBankCount: Integer;
begin
  Result := DefAudioBankCount;
end;

var
  InternalAudio: TAudio;

function Audio: TAudio;
begin
  if InternalAudio = nil then
    InternalAudio := TAudio.Create;
  Result := InternalAudio;
end;

{ Keyboard }

{ Convert a virtual key code to an SDL key code, or 0 if there is none }

function SDLKey(KeyCode: Integer): Uint32;
begin
  case KeyCode of
    VK_A..VK_Z: Result := SDLK_a + KeyCode - VK_A;
    VK_0..VK_9: Result := SDLK_0 + KeyCode - VK_0;
    VK_F1..VK_F12: Result := SDLK_F1 + KeyCode - VK_F1;
    VK_NUMPAD1..VK_NUMPAD9: Result := SDLK_KP_1 + KeyCode - VK_NUMPAD1;
    VK_NUMPAD0: Result := SDLK_KP_0;
    VK_DECIMAL: Result := SDLK_KP_PERIOD;
    VK_DIVIDE: Result := SDLK_KP_DIVIDE;
    VK_MULTIPLY: Result := SDLK_KP_MULTIPLY;
    VK_SUBTRACT: Result := SDLK_KP_MINUS;
    VK_ADD: Result := SDLK_KP_PLUS;
    VK_RETURN: Result := SDLK_RETURN;
    VK_BACK: Result := SDLK_BACKSPACE;
    VK_TAB: Result := SDLK_TAB;
    VK_ESCAPE: Result := SDLK_ESCAPE;
    VK_SPACE: Result := SDLK_SPACE;
    VK_PRIOR: Result := SDLK_PAGEUP;
    VK_NEXT: Result := SDLK_PAGEDOWN;
    VK_END: Result := SDLK_END;
    VK_HOME: Result := SDLK_HOME;
    VK_LEFT: Result := SDLK_LEFT;
    VK_UP: Result := SDLK_UP;
    VK_RIGHT: Result := SDLK_RIGHT;
    VK_DOWN: Result := SDLK_DOWN;
    VK_INSERT: Result := SDLK_INSERT;
    VK_DELETE: Result := SDLK_DELETE;
    VK_PAUSE: Result := SDLK_PAUSE;
    VK_SNAPSHOT: Result := SDLK_PRINTSCREEN;
    VK_CAPITAL: Result := SDLK_CAPSLOCK;
    VK_NUMLOCK: Result := SDLK_NUMLOCKCLEAR;
    VK_SCROLL: Result := SDLK_SCROLLLOCK;
    VK_LSHIFT: Result := SDLK_LSHIFT;
    VK_RSHIFT: Result := SDLK_RSHIFT;
    VK_LCONTROL: Result := SDLK_LCTRL;
    VK_RCONTROL: Result := SDLK_RCTRL;
    VK_LMENU: Result := SDLK_LALT;
    VK_RMENU: Result := SDLK_RALT;
    VK_LWIN: Result := SDLK_LGUI;
    VK_RWIN: Result := SDLK_RGUI;
    VK_APPS: Result := SDLK_APPLICATION;
    VK_OEM_1: Result := SDLK_SEMICOLON;
    VK_OEM_PLUS: Result := SDLK_EQUALS;
    VK_OEM_COMMA: Result := SDLK_COMMA;
    VK_OEM_MINUS: Result := SDLK_MINUS;
    VK_OEM_PERIOD: Result := SDLK_PERIOD;
    VK_OEM_2: Result := SDLK_SLASH;
    VK_OEM_3: Result := SDLK_BACKQUOTE;
    VK_OEM_4: Result := SDLK_LEFTBRACKET;
    VK_OEM_5: Result := SDLK_BACKSLASH;
    VK_OEM_6: Result := SDLK_RIGHTBRACKET;
    VK_OEM_7: Result := SDLK_QUOTE;
  else
    Result := 0;
  end;
end;

function VirtualKey(Sym: LongWord): Word;
begin
  case Sym of
    SDLK_a..SDLK_z: Result := VK_A + Sym - SDLK_a;
    SDLK_0..SDLK_9: Result := VK_0 + Sym - SDLK_0;
    SDLK_F1..SDLK_F12: Result := VK_F1 + Sym - SDLK_F1;
    SDLK_KP_1..SDLK_KP_9: Result := VK_NUMPAD1 + Sym - SDLK_KP_1;
    SDLK_KP_0: Result := VK_NUMPAD0;
    SDLK_KP_PERIOD: Result := VK_DECIMAL;
    SDLK_KP_DIVIDE: Result := VK_DIVIDE;
    SDLK_KP_MULTIPLY: Result := VK_MULTIPLY;
    SDLK_KP_MINUS: Result := VK_SUBTRACT;
    SDLK_KP_PLUS: Result := VK_ADD;
    SDLK_KP_ENTER, SDLK_RETURN: Result := VK_RETURN;
    SDLK_BACKSPACE: Result := VK_BACK;
    SDLK_TAB: Result := VK_TAB;
    SDLK_ESCAPE: Result := VK_ESCAPE;
    SDLK_SPACE: Result := VK_SPACE;
    SDLK_PAGEUP: Result := VK_PRIOR;
    SDLK_PAGEDOWN: Result := VK_NEXT;
    SDLK_END: Result := VK_END;
    SDLK_HOME: Result := VK_HOME;
    SDLK_LEFT: Result := VK_LEFT;
    SDLK_UP: Result := VK_UP;
    SDLK_RIGHT: Result := VK_RIGHT;
    SDLK_DOWN: Result := VK_DOWN;
    SDLK_INSERT: Result := VK_INSERT;
    SDLK_DELETE: Result := VK_DELETE;
    SDLK_PAUSE: Result := VK_PAUSE;
    SDLK_PRINTSCREEN: Result := VK_SNAPSHOT;
    SDLK_CAPSLOCK: Result := VK_CAPITAL;
    SDLK_NUMLOCKCLEAR: Result := VK_NUMLOCK;
    SDLK_SCROLLLOCK: Result := VK_SCROLL;
    SDLK_LSHIFT, SDLK_RSHIFT: Result := VK_SHIFT;
    SDLK_LCTRL, SDLK_RCTRL: Result := VK_CONTROL;
    SDLK_LALT, SDLK_RALT: Result := VK_MENU;
    SDLK_LGUI: Result := VK_LWIN;
    SDLK_RGUI: Result := VK_RWIN;
    SDLK_APPLICATION: Result := VK_APPS;
    SDLK_SEMICOLON: Result := VK_OEM_1;
    SDLK_EQUALS: Result := VK_OEM_PLUS;
    SDLK_COMMA: Result := VK_OEM_COMMA;
    SDLK_MINUS: Result := VK_OEM_MINUS;
    SDLK_PERIOD: Result := VK_OEM_PERIOD;
    SDLK_SLASH: Result := VK_OEM_2;
    SDLK_BACKQUOTE: Result := VK_OEM_3;
    SDLK_LEFTBRACKET: Result := VK_OEM_4;
    SDLK_BACKSLASH: Result := VK_OEM_5;
    SDLK_RIGHTBRACKET: Result := VK_OEM_6;
    SDLK_QUOTE: Result := VK_OEM_7;
  else
    Result := 0;
  end;
end;

function IsKeyDown(KeyCode: Integer): Boolean;

  function Down(Sym: Uint32): Boolean;
  var
    Keys: PUint8;
    Count: Integer;
    Code: Uint32;
  begin
    Result := False;
    if Sym = 0 then
      Exit;
    Count := 0;
    Keys := SDL_GetKeyboardState(Count);
    Code := SDL_GetScancodeFromKey(Sym);
    if (Keys <> nil) and (Code > 0) and (Code < Count) then
      Result := Keys[Code] <> 0;
  end;

  function Button(Index: Integer): Boolean;
  var
    X, Y: LongInt;
  begin
    Result := SDL_GetMouseState(X, Y) and (1 shl (Index - 1)) <> 0;
  end;

begin
  case KeyCode of
    VK_LBUTTON: Result := Button(SDL_BUTTON_LEFT);
    VK_RBUTTON: Result := Button(SDL_BUTTON_RIGHT);
    VK_MBUTTON: Result := Button(SDL_BUTTON_MIDDLE);
    VK_XBUTTON1: Result := Button(SDL_BUTTON_X1);
    VK_XBUTTON2: Result := Button(SDL_BUTTON_X2);
    VK_SHIFT: Result := Down(SDLK_LSHIFT) or Down(SDLK_RSHIFT);
    VK_CONTROL: Result := Down(SDLK_LCTRL) or Down(SDLK_RCTRL);
    VK_MENU: Result := Down(SDLK_LALT) or Down(SDLK_RALT);
  else
    Result := Down(SDLKey(KeyCode));
  end;
end;

{ TKeyboard }

{ SDL keeps the state of every key by scancode, updated as its events are
  read }

procedure TKeyboard.Scan;
var
  Keys: PUint8;
  Count, I: Integer;
begin
  FScanned := True;
  FPrior := FKeys;
  FillChar(FKeys, SizeOf(FKeys), 0);
  Count := 0;
  Keys := SDL_GetKeyboardState(Count);
  if Keys = nil then
    Exit;
  if Count > Length(FKeys) then
    Count := Length(FKeys);
  for I := 0 to Count - 1 do
  begin
    FKeys[I] := Keys^ <> 0;
    Inc(Keys);
  end;
end;

function TKeyboard.State(KeyCode: Integer; Prior: Boolean): Boolean;

  function Down(Sym: Uint32): Boolean;
  var
    Code: Uint32;
  begin
    Result := False;
    if Sym = 0 then
      Exit;
    Code := SDL_GetScancodeFromKey(Sym);
    if (Code < 1) or (Code > High(FKeys)) then
      Exit;
    if Prior then
      Result := FPrior[Code]
    else
      Result := FKeys[Code];
  end;

begin
  if not FScanned then
    Scan;
  case KeyCode of
    VK_SHIFT: Result := Down(SDLK_LSHIFT) or Down(SDLK_RSHIFT);
    VK_CONTROL: Result := Down(SDLK_LCTRL) or Down(SDLK_RCTRL);
    VK_MENU: Result := Down(SDLK_LALT) or Down(SDLK_RALT);
  else
    Result := Down(SDLKey(KeyCode));
  end;
end;

function TKeyboard.GetKey(KeyCode: Integer): Boolean;
begin
  Result := State(KeyCode, False);
end;

function TKeyboard.GetPressed(KeyCode: Integer): Boolean;
begin
  Result := State(KeyCode, False) and not State(KeyCode, True);
end;

function TKeyboard.GetReleased(KeyCode: Integer): Boolean;
begin
  Result := State(KeyCode, True) and not State(KeyCode, False);
end;

function TKeyboard.GetShiftState: TShiftState;
begin
  Result := [];
  if Key[VK_SHIFT] then
    Include(Result, ssShift);
  if Key[VK_CONTROL] then
    Include(Result, ssCtrl);
  if Key[VK_MENU] then
    Include(Result, ssAlt);
end;

var
  InternalKeyboard: TKeyboard;

function Keyboard: TKeyboard;
begin
  if InternalKeyboard = nil then
    InternalKeyboard := TKeyboard.Create;
  Result := InternalKeyboard;
end;

procedure ScanKeyboard;
begin
  if InternalKeyboard <> nil then
    InternalKeyboard.Scan;
end;

{ Mouse }

{ TMouse }

procedure TMouse.Scan;
const
  Masks: array[TSceneButton] of Integer = (0, SDL_BUTTON_LEFT, SDL_BUTTON_RIGHT,
    SDL_BUTTON_MIDDLE, SDL_BUTTON_X1, SDL_BUTTON_X2);
var
  State: Uint32;
  B: TSceneButton;
begin
  FScanned := True;
  FPrior := FButtons;
  FButtons := [];
  State := SDL_GetMouseState(FX, FY);
  for B := buttonLeft to High(B) do
    if State and (1 shl (Masks[B] - 1)) <> 0 then
      Include(FButtons, B);
  { The relative state is the movement since it was last asked for }
  SDL_GetRelativeMouseState(FXDelta, FYDelta);
end;

procedure TMouse.NeedScan;
begin
  if not FScanned then
    Scan;
end;

function TMouse.GetButtons: TSceneButtons;
begin
  NeedScan;
  Result := FButtons;
end;

function TMouse.GetButton(Index: TSceneButton): Boolean;
begin
  NeedScan;
  Result := Index in FButtons;
end;

function TMouse.GetPressed(Index: TSceneButton): Boolean;
begin
  NeedScan;
  Result := (Index in FButtons) and not (Index in FPrior);
end;

function TMouse.GetReleased(Index: TSceneButton): Boolean;
begin
  NeedScan;
  Result := (Index in FPrior) and not (Index in FButtons);
end;

function TMouse.GetX: Integer;
begin
  NeedScan;
  Result := FX;
end;

function TMouse.GetY: Integer;
begin
  NeedScan;
  Result := FY;
end;

function TMouse.GetXDelta: Integer;
begin
  NeedScan;
  Result := FXDelta;
end;

function TMouse.GetYDelta: Integer;
begin
  NeedScan;
  Result := FYDelta;
end;

function TMouse.GetCaptured: Boolean;
begin
  Result := SDL_GetRelativeMouseMode;
end;

procedure TMouse.SetCaptured(Value: Boolean);
begin
  SDL_SetRelativeMouseMode(Value);
end;

function TMouse.GetVisible: Boolean;
begin
  Result := SDL_ShowCursor(SDL_QUERY) = SDL_ENABLE;
end;

procedure TMouse.SetVisible(Value: Boolean);
begin
  if Value then
    SDL_ShowCursor(SDL_ENABLE)
  else
    SDL_ShowCursor(SDL_DISABLE);
end;

const
  SystemCursors: array[TMouseCursor] of Uint32 = (SDL_SYSTEM_CURSOR_ARROW,
    SDL_SYSTEM_CURSOR_ARROW, SDL_SYSTEM_CURSOR_ARROW, SDL_SYSTEM_CURSOR_IBEAM,
    SDL_SYSTEM_CURSOR_WAIT, SDL_SYSTEM_CURSOR_CROSSHAIR, SDL_SYSTEM_CURSOR_HAND,
    SDL_SYSTEM_CURSOR_NO, SDL_SYSTEM_CURSOR_SIZEALL, SDL_SYSTEM_CURSOR_SIZENS,
    SDL_SYSTEM_CURSOR_SIZEWE, SDL_SYSTEM_CURSOR_SIZENWSE, SDL_SYSTEM_CURSOR_SIZENESW);

{ The cursors of SDL are made when first used and kept }

procedure TMouse.SetCursor(Value: TMouseCursor);
begin
  if Value = FCursor then
    Exit;
  FCursor := Value;
  if FCursor = cursorNone then
  begin
    SDL_ShowCursor(SDL_DISABLE);
    Exit;
  end;
  SDL_ShowCursor(SDL_ENABLE);
  if FCursors[FCursor] = nil then
    FCursors[FCursor] := SDL_CreateSystemCursor(SystemCursors[FCursor]);
  if FCursors[FCursor] <> nil then
    SDL_SetCursor(FCursors[FCursor]);
end;

{ Cursors can only be freed while SDL video is running }

destructor TMouse.Destroy;
var
  C: TMouseCursor;
begin
  if SDL_WasInit(SDL_INIT_VIDEO) <> 0 then
    for C := Low(FCursors) to High(FCursors) do
      if FCursors[C] <> nil then
        SDL_FreeCursor(FCursors[C]);
  inherited Destroy;
end;

var
  InternalMouse: TMouse;

function Mouse: TMouse;
begin
  if InternalMouse = nil then
    InternalMouse := TMouse.Create;
  Result := InternalMouse;
end;

procedure ScanMouse;
begin
  if InternalMouse <> nil then
    InternalMouse.Scan;
end;

{ Joysticks }

{ TJoystick }

{ Values this close to rest or to the ends are moved onto them, and a change
  this small is taken to be jitter }

function AxisValue(Value: SmallInt; Prior: Single): Single;
const
  Sigma = 0.01;
  MaxAxis = 32767;
begin
  Result := Value / MaxAxis;
  if Abs(Result) < Sigma then
    Result := 0
  else if Result > 1 - Sigma then
    Result := 1
  else if Result < Sigma - 1 then
    Result := -1
  else if Abs(Prior - Result) < Sigma then
    Result := Prior;
end;

constructor TJoystick.Create(Joystick, Controller: Pointer);
begin
  inherited Create;
  FJoystick := Joystick;
  FController := Controller;
  if FController <> nil then
    FName := SDL_GameControllerName(FController);
  if FName = '' then
    FName := SDL_JoystickName(FJoystick);
  SetLength(FAxes, SDL_JoystickNumAxes(FJoystick));
  SetLength(FButtons, SDL_JoystickNumButtons(FJoystick));
  SetLength(FHats, SDL_JoystickNumHats(FJoystick));
  SetLength(FTrackballs, SDL_JoystickNumBalls(FJoystick));
  Scan;
end;

destructor TJoystick.Destroy;
begin
  if FController <> nil then
    SDL_GameControllerClose(FController);
  SDL_JoystickClose(FJoystick);
  inherited Destroy;
end;

{ TGamepadButton and TGamepadAxis are in the order of the SDL constants }

procedure TJoystick.Scan;
var
  B: TGamepadButton;
  A: TGamepadAxis;
  I: Integer;
begin
  if FController <> nil then
  begin
    for B := Low(B) to High(B) do
      FGamepadButtons[B] := SDL_GameControllerGetButton(FController, Ord(B)) <> 0;
    for A := Low(A) to High(A) do
      FGamepadAxes[A] := AxisValue(SDL_GameControllerGetAxis(FController, Ord(A)),
        FGamepadAxes[A]);
  end;
  for I := 0 to Length(FAxes) - 1 do
    FAxes[I] := AxisValue(SDL_JoystickGetAxis(FJoystick, I), FAxes[I]);
  for I := 0 to Length(FButtons) - 1 do
    FButtons[I] := SDL_JoystickGetButton(FJoystick, I) <> 0;
  for I := 0 to Length(FHats) - 1 do
    case SDL_JoystickGetHat(FJoystick, I) of
      SDL_HAT_UP: FHats[I] := hatUp;
      SDL_HAT_RIGHT: FHats[I] := hatRight;
      SDL_HAT_DOWN: FHats[I] := hatDown;
      SDL_HAT_LEFT: FHats[I] := hatLeft;
      SDL_HAT_RIGHTUP: FHats[I] := hatRightUp;
      SDL_HAT_RIGHTDOWN: FHats[I] := hatRightDown;
      SDL_HAT_LEFTUP: FHats[I] := hatLeftUp;
      SDL_HAT_LEFTDOWN: FHats[I] := hatLeftDown;
    else
      FHats[I] := hatCenter;
    end;
  for I := 0 to Length(FTrackballs) - 1 do
    if SDL_JoystickGetBall(FJoystick, I, FTrackballs[I].XDelta, FTrackballs[I].YDelta) <> 0 then
    begin
      FTrackballs[I].XDelta := 0;
      FTrackballs[I].YDelta := 0;
    end;
end;

function TJoystick.GetAttached: Boolean;
begin
  Result := SDL_JoystickGetAttached(FJoystick);
end;

function TJoystick.GetIsGamepad: Boolean;
begin
  Result := FController <> nil;
end;

function TJoystick.GetGamepadButton(Button: TGamepadButton): Boolean;
begin
  Result := FGamepadButtons[Button];
end;

function TJoystick.GetGamepadAxis(Axis: TGamepadAxis): Single;
begin
  Result := FGamepadAxes[Axis];
end;

function TJoystick.GetAxisCount: Integer;
begin
  Result := Length(FAxes);
end;

function TJoystick.GetAxis(Index: Integer): Single;
begin
  if (Index < 0) or (Index >= Length(FAxes)) then
    Result := 0
  else
    Result := FAxes[Index];
end;

function TJoystick.GetButtonCount: Integer;
begin
  Result := Length(FButtons);
end;

function TJoystick.GetButton(Index: Integer): Boolean;
begin
  if (Index < 0) or (Index >= Length(FButtons)) then
    Result := False
  else
    Result := FButtons[Index];
end;

function TJoystick.GetHatCount: Integer;
begin
  Result := Length(FHats);
end;

function TJoystick.GetHat(Index: Integer): TJoystickHat;
begin
  if (Index < 0) or (Index >= Length(FHats)) then
    Result := hatCenter
  else
    Result := FHats[Index];
end;

function TJoystick.GetTrackballCount: Integer;
begin
  Result := Length(FTrackballs);
end;

function TJoystick.GetTrackball(Index: Integer): TJoystickTrackball;
begin
  if (Index < 0) or (Index >= Length(FTrackballs)) then
  begin
    Result.XDelta := 0;
    Result.YDelta := 0;
  end
  else
    Result := FTrackballs[Index];
end;

{ TJoysticks }

constructor TJoysticks.Create;
begin
  inherited Create;
  { The game controller subsystem includes the joystick subsystem }
  FInitialized := SDL_InitSubSystem(SDL_INIT_GAMECONTROLLER) = 0;
  { Without a window made by SDL nothing reads the events of SDL, so stop
    the joysticks from adding to them }
  if FInitialized and (SDL_WasInit(SDL_INIT_VIDEO) = 0) then
  begin
    SDL_JoystickEventState(0);
    SDL_GameControllerEventState(0);
  end;
  Scan;
end;

destructor TJoysticks.Destroy;
var
  I: Integer;
begin
  for I := 0 to Length(FItems) - 1 do
    FItems[I].Free;
  FItems := nil;
  if FInitialized then
    SDL_QuitSubSystem(SDL_INIT_GAMECONTROLLER);
  inherited Destroy;
end;

function TJoysticks.Find(Handle: Pointer): TJoystick;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Length(FItems) - 1 do
    if FItems[I].FJoystick = Handle then
      Exit(FItems[I]);
end;

{ Remove the joysticks which were disconnected and add those which were
  connected, keeping the others in the same order }

procedure TJoysticks.Connect;
var
  J, C: Pointer;
  I, N: Integer;
begin
  N := 0;
  for I := 0 to Length(FItems) - 1 do
    if FItems[I].Attached then
    begin
      FItems[N] := FItems[I];
      Inc(N);
    end
    else
      FItems[I].Free;
  SetLength(FItems, N);
  for I := 0 to SDL_NumJoysticks - 1 do
  begin
    J := SDL_JoystickOpen(I);
    if J = nil then
      Continue;
    if Find(J) <> nil then
      { A joystick which is already open is returned again by SDL with one
        more reference, which is released }
      SDL_JoystickClose(J)
    else
    begin
      C := nil;
      if SDL_IsGameController(I) then
        C := SDL_GameControllerOpen(I);
      SetLength(FItems, Length(FItems) + 1);
      FItems[Length(FItems) - 1] := TJoystick.Create(J, C);
    end;
  end;
end;

procedure TJoysticks.Scan;
var
  Changed: Boolean;
  I: Integer;
begin
  if not FInitialized then
    Exit;
  { Without the event loop of SDL the joysticks are only read when asked }
  SDL_JoystickUpdate;
  Changed := SDL_NumJoysticks <> Length(FItems);
  for I := 0 to Length(FItems) - 1 do
    if not FItems[I].Attached then
      Changed := True;
  if Changed then
    Connect;
  for I := 0 to Length(FItems) - 1 do
    FItems[I].Scan;
end;

function TJoysticks.ButtonDown(Index, Button: Integer): Boolean;
begin
  Result := (Index >= 0) and (Index < Length(FItems)) and
    FItems[Index].Buttons[Button];
end;

function TJoysticks.Axis(Index, AxisIndex: Integer): Single;
begin
  if (Index < 0) or (Index >= Length(FItems)) then
    Result := 0
  else
    Result := FItems[Index].Axes[AxisIndex];
end;

function TJoysticks.GamepadDown(Index: Integer; Button: TGamepadButton): Boolean;
begin
  Result := (Index >= 0) and (Index < Length(FItems)) and
    FItems[Index].GamepadButtons[Button];
end;

function TJoysticks.GamepadAxis(Index: Integer; Named: TGamepadAxis): Single;
begin
  if (Index < 0) or (Index >= Length(FItems)) then
    Result := 0
  else
    Result := FItems[Index].GamepadAxes[Named];
end;

function TJoysticks.GetCount: Integer;
begin
  Result := Length(FItems);
end;

function TJoysticks.GetJoystick(Index: Integer): TJoystick;
begin
  if (Index < 0) or (Index >= Length(FItems)) then
    raise ERangeError.CreateFmt('Joystick index %d is out of range', [Index]);
  Result := FItems[Index];
end;

var
  InternalJoysticks: TJoysticks;

function Joysticks: TJoysticks;
begin
  if InternalJoysticks = nil then
    InternalJoysticks := TJoysticks.Create;
  Result := InternalJoysticks;
end;

procedure ScanJoysticks;
begin
  if InternalJoysticks <> nil then
    InternalJoysticks.Scan;
end;

var
  PumpLock: TRTLCriticalSection;

procedure PumpHardware;
begin
  {$ifndef windows}
  EnterCriticalSection(PumpLock);
  try
    SDL_PumpEvents;
  finally
    LeaveCriticalSection(PumpLock);
  end;
  {$endif}
end;

procedure ScanHardware;
begin
  if ScanHardwarePumps and ((InternalKeyboard <> nil) or (InternalMouse <> nil) or
    (InternalJoysticks <> nil)) then
    PumpHardware;
  ScanKeyboard;
  ScanMouse;
  ScanJoysticks;
end;

initialization
  InitCriticalSection(PumpLock);
finalization
  InternalAudio.Free;
  InternalKeyboard.Free;
  InternalMouse.Free;
  InternalJoysticks.Free;
  DoneCriticalSection(PumpLock);
end.
