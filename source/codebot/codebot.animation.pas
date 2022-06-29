(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified March 2015                                 *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.animation.txt> }
unit Codebot.Animation;

{$i codebot.inc}

interface

uses
  SysUtils, Classes,
  Codebot.System,
  Codebot.Collections,
  Codebot.Graphics.Types,
  Codebot.Geometry;

{ TEasing is the function prototype for change over time [group animation]
  See also
  <link Codebot.Animation.Easings, Easings function>
  <link Codebot.Animation.TEasingDefaults, TEasingDefaults class>
  <exref target="http://easings.net/">External: Easing functions on easings.net</exref> }

type
  TEasing = function(Percent: Float): Float;

{ TEasingDefaults provides some default easing functions which conform to
  the <link Codebot.Animation.TEasing, TEasing prototype> [group animation]
  See also
  <link Overview.Codebot.Animation.TEasingDefaults, TEasingDefaults members>
  <link Codebot.Animation.Easings, Easings function>
  <exref target="http://easings.net/">External: Easing functions on easings.net</exref> }

  TEasingDefaults = record
  public
    { The default easing function with no interpolation }
    class function Linear(Percent: Float): Float; static;
    { Slow, fast, then slow }
    class function Easy(Percent: Float): Float; static;
    { Real slow, fast, then real slow }
    class function EasySlow(Percent: Float): Float; static;
    { Wind up slow, fast, then overshoot and wind down slow }
    class function Extend(Percent: Float): Float; static;
    { Slow then fast }
    class function Drop(Percent: Float): Float; static;
    { Real slow then fast }
    class function DropSlow(Percent: Float): Float; static;
    { Real slow then fast }
    class function Snap(Percent: Float): Float; static;
    { Slow, fast, then bounce a few times }
    class function Bounce(Percent: Float): Float; static;
    { Slow, fast, then bounce a few more times }
    class function Bouncy(Percent: Float): Float; static;
    { Fast, then rebound slowly down   }
    class function Rubber(Percent: Float): Float; static;
    { Fast, then rebound fast }
    class function Spring(Percent: Float): Float; static;
    { Fast, then rebound realy fast }
    class function Boing(Percent: Float): Float; static;
  end;

{ TEasings is a dictionary which stores easings by name [group animation]
  See also
  <link Codebot.Animation.Easings, Easings function> }

  TEasings = class(TDictionary<string, TEasing>)
  protected
    {doc off}
    function DefaultValue: TEasing; override;
  public
    procedure RegisterDefaults;
    {doc on}
  end;

{ Shortcut to easings key value type }

  TEasingKeyValue = TEasings.TKeyValue;

{ Calculates the percent change of an easing, optionally reversing the curve [group animation] }
function Interpolate(Easing: TEasing; Percent: Float; Reverse: Boolean = False): Float; overload;
{ Calculates the effect of an easing on values, optionally reversing the curve [group animation] }
function Interpolate(Easing: TEasing; Percent: Float; Start, Finish: Float; Reverse: Boolean = False): Float; overload;
{ Provides access to <link Codebot.Animation.TEasings, TEasings class> [group animation] }
function Easings: TEasings;

{ IDependencyProperty allows vector properties to be dettached from their owning
  objects [group animation]
  <link Overview.Bare.Animation.IDependencyProperty, IDependencyProperty members> }

type
  IDependencyProperty = interface
  ['{E021AD95-9985-48AB-B29F-8D25A7BBE10E}']
    {doc ignore}
    function GetCount: Integer;
    { Get a component value }
    function GetValue(Index: Integer): Float;
    { Set a component value }
    procedure SetValue(Value: Float; Index: Integer);
    { Returns the number of component values }
    property Count: Integer read GetCount;
  end;

{ TDependencyChangeNotify allows objects which own dependency properties to be
  notified when the property values are updated other code [group animation] }

  TDependencyChangeNotify = procedure(Prop: IDependencyProperty; Index: Integer) of object;

{ TVec1Prop is a 1 component dependency property [group animation]
  <link Overview.Bare.Animation.TVec1Prop, TVec1Prop members> }

  TVec1Prop = record
  private
    {doc off}
    function GetValue: TVec1;
    procedure SetValue(Value: TVec1);
    function GetVec({%H-}Index: Integer): TVec1Prop;
    procedure SetVec({%H-}Index: Integer; const Value: TVec1Prop);
  public
    class operator Implicit(const Value: TVec1): TVec1Prop;
    class operator Implicit(const Value: TVec1Prop): TVec1;
    class operator Negative(const A: TVec1Prop): TVec1;
    class operator Positive(const A: TVec1Prop): TVec1;
    class operator Equal(const A, B: TVec1Prop) : Boolean;
    class operator NotEqual(const A, B: TVec1Prop): Boolean;
    class operator GreaterThan(const A, B: TVec1Prop): Boolean;
    class operator GreaterThanOrEqual(const A, B: TVec1Prop): Boolean;
    class operator LessThan(const A, B: TVec1Prop): Boolean;
    class operator LessThanOrEqual(const A, B: TVec1Prop): Boolean;
    class operator Add(const A, B: TVec1Prop): TVec1;
    class operator Subtract(const A, B: TVec1Prop): TVec1;
    class operator Multiply(const A, B: TVec1Prop): TVec1;
    class operator Divide(const A, B: TVec1Prop): TVec1;
    procedure Link(OnChange: TDependencyChangeNotify = nil); overload;
    procedure Link(Prop: IDependencyProperty; Index: LongInt); overload;
    procedure Unlink;
    function Linked: Boolean;
    function Equals(const A: TVec1Prop): Boolean;
    function Same(const A: TVec1Prop): Boolean;
    property X: TVec1Prop index 0 read GetVec write SetVec;
    property Value: TVec1 read GetValue write SetValue;
    property Vec[Index: Integer]: TVec1Prop read GetVec write SetVec;
  private
    FProp: IDependencyProperty;
    case Boolean of
      True: (FIndex: LongInt);
      False: (FValue: TVec1);
    {doc on}
  end;

{ TVec2Prop is a 2 component dependency property [group animation]
  <link Overview.Bare.Animation.TVec2Prop, TVec2Prop members> }

  TVec2Prop = record
  private
    {doc off}
    function GetValue: TVec2;
    procedure SetValue(const Value: TVec2);
    function GetVec(Index: Integer): TVec1Prop;
    procedure SetVec(Index: Integer; const Value: TVec1Prop);
  public
    class operator Implicit(const Value: TVec2Prop): TVec2;
    class operator Implicit(const Value: TVec2): TVec2Prop;
    class operator Implicit(const Value: TPoint): TVec2Prop;
    class operator Explicit(const Value: TVec2Prop): TPoint;
    class operator Implicit(const Value: TPointI): TVec2Prop;
    class operator Explicit(const Value: TVec2Prop): TPointI;
    class operator Implicit(const Value: TPointF): TVec2Prop;
    class operator Implicit(const Value: TVec2Prop): TPointF;
    class operator Negative(const A: TVec2Prop): TVec2;
    class operator Add(const A, B: TVec2Prop): TVec2;
    class operator Subtract(const A, B: TVec2Prop): TVec2;
    class operator Multiply(const A: TVec2Prop; B: Float): TVec2;
    class operator Divide(const A: TVec2Prop; B: Float): TVec2;
    procedure Link(OnChange: TDependencyChangeNotify = nil); overload;
    procedure Link(Prop: IDependencyProperty; Index: LongInt); overload;
    procedure Unlink;
    function Linked: Boolean;
    property X: TVec1Prop index 0 read GetVec write SetVec;
    property Y: TVec1Prop index 1 read GetVec write SetVec;
    property AsVec1: TVec1Prop index 0 read GetVec write SetVec;
    property Value: TVec2 read GetValue write SetValue;
    property Vec[Index: Integer]: TVec1Prop read GetVec write SetVec;
  private
    FProp: IDependencyProperty;
    case Boolean of
      True: (FIndex: LongInt);
      False: (FValue: TVec2);
    {doc on}
  end;

{ TVec3Prop is a 3 component dependency property
  <link Overview.Bare.Animation.TVec3Prop, TVec3Prop members> }

  TVec3Prop = record
  private
    {doc off}
    function GetValue: TVec3;
    procedure SetValue(const Value: TVec3);
    function GetVec(Index: Integer): TVec1Prop;
    procedure SetVec(Index: Integer; const Value: TVec1Prop);
    function GetAsVec2: TVec2Prop;
    procedure SetAsVec2(const Value: TVec2Prop);
  public
    class operator Implicit(const Value: TVec3Prop): TVec3;
    class operator Implicit(const Value: TVec3): TVec3Prop;
    class operator Negative(const A: TVec3Prop): TVec3;
    class operator Add(const A, B: TVec3Prop): TVec3;
    class operator Subtract(const A, B: TVec3Prop): TVec3;
    class operator Multiply(const A: TVec3Prop; B: Float): TVec3;
    class operator Divide(const A: TVec3Prop; B: Float): TVec3;
    procedure Link(OnChange: TDependencyChangeNotify = nil); overload;
    procedure Link(Prop: IDependencyProperty; Index: LongInt); overload;
    procedure Unlink;
    function Linked: Boolean;
    property X: TVec1Prop index 0 read GetVec write SetVec;
    property Y: TVec1Prop index 1 read GetVec write SetVec;
    property Z: TVec1Prop index 2 read GetVec write SetVec;
    property Pitch: TVec1Prop index 0 read GetVec write SetVec;
    property Heading: TVec1Prop index 1 read GetVec write SetVec;
    property Roll: TVec1Prop index 2 read GetVec write SetVec;
    property XY: TVec2Prop read GetAsVec2 write SetAsVec2;
    property AsVec1: TVec1Prop index 0 read GetVec write SetVec;
    property AsVec2: TVec2Prop read GetAsVec2 write SetAsVec2;
    property Value: TVec3 read GetValue write SetValue;
    property Vec[Index: Integer]: TVec1Prop read GetVec write SetVec;
  private
    FProp: IDependencyProperty;
    case Boolean of
      True: (FIndex: LongInt);
      False: (FValue: TVec3);
    {doc on}
  end;

{ TVec4Prop is a 4 component dependency property [group animation]
  <link Overview.Bare.Animation.TVec4Prop, TVec4Prop members> }

  TVec4Prop = record
  private
    {doc off}
    function GetValue: TVec4;
    procedure SetValue(const Value: TVec4);
    function GetVec(Index: Integer): TVec1Prop;
    procedure SetVec(Index: Integer; const Value: TVec1Prop);
    function GetAsVec2: TVec2Prop;
    procedure SetAsVec2(const Value: TVec2Prop);
    function GetAsVec3: TVec3Prop;
    procedure SetAsVec3(const Value: TVec3Prop);
  public
    class operator Implicit(const Value: TVec4): TVec4Prop;
    class operator Implicit(const Value: TVec4Prop): TVec4;
    class operator Implicit(Value: TColorB): TVec4Prop;
    class operator Explicit(const Value: TVec4Prop): TColorB;
    class operator Implicit(const Value: TColorF): TVec4Prop;
    class operator Implicit(const Value: TVec4Prop): TColorF;
    procedure Link(OnChange: TDependencyChangeNotify = nil); overload;
    procedure Link(Prop: IDependencyProperty; Index: LongInt); overload;
    procedure Unlink;
    function Linked: Boolean;
    property X: TVec1Prop index 0 read GetVec write SetVec;
    property Y: TVec1Prop index 1 read GetVec write SetVec;
    property Z: TVec1Prop index 2 read GetVec write SetVec;
    property W: TVec1Prop index 3 read GetVec write SetVec;
    property Red: TVec1Prop index 0 read GetVec write SetVec;
    property Green: TVec1Prop index 1 read GetVec write SetVec;
    property Blue: TVec1Prop index 2 read GetVec write SetVec;
    property Alpha: TVec1Prop index 3 read GetVec write SetVec;
    property S0: TVec1Prop index 0 read GetVec write SetVec;
    property T0: TVec1Prop index 1 read GetVec write SetVec;
    property S1: TVec1Prop index 2 read GetVec write SetVec;
    property T1: TVec1Prop index 3 read GetVec write SetVec;
    property XY: TVec2Prop read GetAsVec2 write SetAsVec2;
    property XYZ: TVec3Prop read GetAsVec3 write SetAsVec3;
    property RGB: TVec3Prop read GetAsVec3 write SetAsVec3;
    property AsVec1: TVec1Prop index 0 read GetVec write SetVec;
    property AsVec2: TVec2Prop read GetAsVec2 write SetAsVec2;
    property AsVec3: TVec3Prop read GetAsVec3 write SetAsVec3;
    property Value: TVec4 read GetValue write SetValue;
    property Vec[Index: Integer]: TVec1Prop read GetVec write SetVec;
  private
    FProp: IDependencyProperty;
    case Boolean of
      True: (FIndex: LongInt);
      False: (FValue: TVec4);
    {doc on}
  end;

{ Link a dependency property [group animation] }
procedure DependencyLink(var Prop: IDependencyProperty; Count: Integer; OnChange: TDependencyChangeNotify);
{ Unlink a dependency property  [group animation] }
procedure DependencyUnlink(var Prop: IDependencyProperty);

type
  {doc ignore}
  IPropertyResolver = interface;

{ TVectorProperty is the result of resolved vector properties
  <link Overview.Bare.Animation.TVectorProperty, TVectorProperty members> }

  TVectorProperty = record
    Vec1Prop: TVec1Prop;
    Vec2Prop: TVec2Prop;
    Vec3Prop: TVec3Prop;
    Vec4Prop: TVec4Prop;
    Resolver: IPropertyResolver;
  end;


{ IPropertyResolver is used to convert a name to a vector property
  <link Overview.Bare.Animation.IPropertyResolver, IPropertyResolver members> }

  IPropertyResolver = interface
  ['{1638C795-D894-4B7F-9491-47F57A88F622}']
    { Ask the object to resolve a name  }
    function Resolve(const Name: string; out Prop: TVectorProperty): Boolean;
  end;

{ Return false while clearing a vector property }

function VectorPropertyEmpty(out Prop: TVectorProperty): Boolean;

{ TAnimationTimer is a high performance timer fixed at 30 frames per second [group animation]
  See also
  <link Overview.Codebot.Animation.TAnimationTimer, TAnimationTimer members> }

type
  TAnimationTimer = class(TComponent)
  private
    FEnabled: Boolean;
    FOnTimer: TNotifyEvent;
    procedure Timer(Sender: TObject);
    procedure SetEnabled(Value: Boolean);
  public
    { Create a new aniamtion timer }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Start or stop the timer using enabled }
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    { OnTimer is fired every 1/30 of a second when enabled }
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
  end;

{ TAnimator }

  TAnimator = class
  private
  type
    TAnimationItem = record
      Notify: IFloatPropertyNotify;
      Prop: PFloat;
      StartTarget: Float;
      StopTarget: Float;
      StartTime: Double;
      StopTime: Double;
      Easing: TEasing;
    end;
    PAnimationItem = ^TAnimationItem;

    TAnimations = TArrayList<TAnimationItem>;

  var
    FAnimations: TAnimations;
    FAnimated: Boolean;
    FOnStart: TNotifyDelegate;
    FOnStop: TNotifyDelegate;
  public
    { Animate adds a property to the list of animated items }
    procedure Animate(var Prop: Float; Target: Float; const Easing: string; Duration: Double = 0.25); overload;
    procedure Animate(var Prop: Float; Target: Float; Easing: TEasing = nil; Duration: Double = 0.25); overload;
    procedure Animate(NotifyObject: TObject; var Prop: Float; Target: Float; const Easing: string; Duration: Double = 0.25); overload;
    procedure Animate(NotifyObject: TObject; var Prop: Float; Target: Float; Easing: TEasing = nil; Duration: Double = 0.25); overload;
    { Stop removes a property animation }
    procedure Stop(var Prop: Float);
    { Step causes all animated properties to be evaluated }
    procedure Step;
    { Animated is True is if a property value changed when Step was last invoked }
    property Animated: Boolean read FAnimated;
    { OnStart is invoked if an animated property requires steps }
    property OnStart: TNotifyDelegate read FOnStart;
    { OnStop is when there are no more properties to animate }
    property OnStop: TNotifyDelegate read FOnStop;
  end;

function Animator: TAnimator;

implementation

{ Easings }

var
  InternalEasings: TObject;

function Easings: TEasings;
begin
  if InternalEasings = nil then
  begin
    InternalEasings := TEasings.Create;
    TEasings(InternalEasings).RegisterDefaults;
  end;
  Result := TEasings(InternalEasings);
end;

{ TVec1Prop }

class operator TVec1Prop.Implicit(const Value: TVec1Prop): TVec1;
begin
  Result := Value.Value;
end;

class operator TVec1Prop.Implicit(const Value: TVec1): TVec1Prop;
begin
  UIntPtr(Result.FProp) := 0;
  Result.FValue := Value;
end;

class operator TVec1Prop.Negative(const A: TVec1Prop): TVec1;
begin
  Result := -A.Value;
end;

class operator TVec1Prop.Positive(const A: TVec1Prop): TVec1;
begin
  Result := A.Value;
end;

class operator TVec1Prop.Equal(const A, B: TVec1Prop) : Boolean;
begin
  Result := A.Value = B.Value;
end;

class operator TVec1Prop.NotEqual(const A, B: TVec1Prop): Boolean;
begin
  Result := A.Value <> B.Value;
end;

class operator TVec1Prop.GreaterThan(const A, B: TVec1Prop): Boolean;
begin
  Result := A.Value > B.Value;
end;

class operator TVec1Prop.GreaterThanOrEqual(const A, B: TVec1Prop): Boolean;
begin
  Result := A.Value >= B.Value;
end;

class operator TVec1Prop.LessThan(const A, B: TVec1Prop): Boolean;
begin
  Result := A.Value < B.Value;
end;

class operator TVec1Prop.LessThanOrEqual(const A, B: TVec1Prop): Boolean;
begin
  Result := A.Value <= B.Value;
end;

class operator TVec1Prop.Add(const A, B: TVec1Prop): TVec1;
begin
  Result := A.Value + B.Value;
end;

class operator TVec1Prop.Subtract(const A, B: TVec1Prop): TVec1;
begin
  Result := A.Value - B.Value;
end;

class operator TVec1Prop.Multiply(const A, B: TVec1Prop): TVec1;
begin
  Result := A.Value * B.Value;
end;

class operator TVec1Prop.Divide(const A, B: TVec1Prop): TVec1;
begin
  Result := A.Value / B.Value;
end;

procedure TVec1Prop.Link(OnChange: TDependencyChangeNotify = nil);
begin
  DependencyLink(FProp, 1, OnChange);
  FIndex := 0;
end;

procedure TVec1Prop.Link(Prop: IDependencyProperty; Index: LongInt);
begin
  FProp := Prop;
  FIndex := Index;
end;

procedure TVec1Prop.Unlink;
begin
  FProp := nil;
end;

function TVec1Prop.Linked: Boolean;
var
  B: Boolean;
begin
  B := FProp <> nil;
  Result := B;
end;

function TVec1Prop.Equals(const A: TVec1Prop): Boolean;
begin
  Result := Value = A.Value;
end;

function TVec1Prop.Same(const A: TVec1Prop): Boolean;
begin
  if FProp = nil then
    Result := False
  else if FProp = A.FProp then
    Result := FIndex = A.FIndex
  else
    Result := False;
end;

function TVec1Prop.GetValue: TVec1;
begin
  if FProp = nil then
    Result := FValue
  else
    Result := FProp.GetValue(FIndex);
end;

procedure TVec1Prop.SetValue(Value: TVec1);
begin
  if FProp = nil then
    FValue := Value
  else
    FProp.SetValue(Value, FIndex);
end;

function TVec1Prop.GetVec(Index: Integer): TVec1Prop;
begin
  Exit(Self);
end;

procedure TVec1Prop.SetVec(Index: Integer; const Value: TVec1Prop);
begin
  if not Same(Value) then
    SetValue(Value.Value);
end;

{ TVec2Prop }

class operator TVec2Prop.Implicit(const Value: TVec2Prop): TVec2;
begin
  Result := Value.Value;
end;

class operator TVec2Prop.Implicit(const Value: TVec2): TVec2Prop;
begin
  UIntPtr(Result.FProp) := 0;
  Result.FValue := Value;
end;

class operator TVec2Prop.Implicit(const Value: TPoint): TVec2Prop;
begin
  UIntPtr(Result.FProp) := 0;
  Result.FValue.X := Value.X;
  Result.FValue.Y := Value.Y;
end;

class operator TVec2Prop.Explicit(const Value: TVec2Prop): TPoint;
var
  V: TVec2;
begin
  V := Value.Value;
  Result.X := Round(V.X);
  Result.Y := Round(V.Y);
end;

class operator TVec2Prop.Implicit(const Value: TPointI): TVec2Prop;
begin
  UIntPtr(Result.FProp) := 0;
  Result.FValue.X := Value.X;
  Result.FValue.Y := Value.Y;
end;

class operator TVec2Prop.Explicit(const Value: TVec2Prop): TPointI;
var
  V: TVec2;
begin
  V := Value.Value;
  Result.X := Round(V.X);
  Result.Y := Round(V.Y);
end;

class operator TVec2Prop.Implicit(const Value: TPointF): TVec2Prop;
begin
  UIntPtr(Result.FProp) := 0;
  Result.FValue.X := Value.X;
  Result.FValue.Y := Value.Y;
end;

class operator TVec2Prop.Implicit(const Value: TVec2Prop): TPointF;
var
  V: TVec2;
begin
  V := Value.Value;
  Result.X := V.X;
  Result.Y := V.Y;
end;

class operator TVec2Prop.Negative(const A: TVec2Prop): TVec2;
begin
  Result := -A.Value;
end;

class operator TVec2Prop.Add(const A, B: TVec2Prop): TVec2;
begin
  Result := A.Value + B.Value;
end;

class operator TVec2Prop.Subtract(const A, B: TVec2Prop): TVec2;
begin
  Result := A.Value - B.Value;
end;

class operator TVec2Prop.Multiply(const A: TVec2Prop; B: Float): TVec2;
begin
  Result := A.Value * B;
end;

class operator TVec2Prop.Divide(const A: TVec2Prop; B: Float): TVec2;
begin
  Result := A.Value / B;
end;

procedure TVec2Prop.Link(OnChange: TDependencyChangeNotify = nil);
begin
  DependencyLink(FProp, 2, OnChange);
  FIndex := 0;
end;

procedure TVec2Prop.Link(Prop: IDependencyProperty; Index: LongInt);
begin
  FProp := Prop;
  FIndex := Index;
end;

procedure TVec2Prop.Unlink;
begin
  FProp := nil;
end;

function TVec2Prop.Linked: Boolean;
begin
  Result := FProp <> nil;
end;

function TVec2Prop.GetValue: TVec2;
begin
  if FProp = nil then
    Result := FValue
  else
  begin
    Result.X := FProp.GetValue(FIndex);
    Result.Y := FProp.GetValue(FIndex + 1);
  end;
end;

procedure TVec2Prop.SetValue(const Value: TVec2);
begin
  if FProp = nil then
    FValue := Value
  else
  begin
    FProp.SetValue(Value.X, FIndex);
    FProp.SetValue(Value.Y, FIndex + 1);
  end;
end;

function TVec2Prop.GetVec(Index: Integer): TVec1Prop;
var
  V: TVec1Prop;
begin
  UIntPtr(V.FProp) := 0;
  if FProp = nil then
  begin
    if Index < 1 then
      V.FValue := FValue.X
    else
      V.FValue := FValue.Y;
  end
  else
  begin
    V.FProp := FProp;
    if Index < 1 then
      V.FIndex := FIndex
    else
      V.FIndex := FIndex + 1;
  end;
  Exit(V);
end;

procedure TVec2Prop.SetVec(Index: Integer; const Value: TVec1Prop);
begin
  if FProp = nil then
  begin
    FProp := nil;
    if Index < 1 then
      FValue.X := Value.Value
    else
      FValue.Y := Value.Value;
  end
  else
  begin
    if Index < 1 then
      FProp.SetValue(Value.Value, FIndex)
    else
      FProp.SetValue(Value.Value, FIndex + 1);
  end;
end;

{ TVec3Prop }

class operator TVec3Prop.Implicit(const Value: TVec3): TVec3Prop;
begin
  UIntPtr(Result.FProp) := 0;
  Result.FValue := Value;
end;

class operator TVec3Prop.Implicit(const Value: TVec3Prop): TVec3;
begin
  Result := Value.Value;
end;

class operator TVec3Prop.Negative(const A: TVec3Prop): TVec3;
begin
  Result := -A.Value;
end;

class operator TVec3Prop.Add(const A, B: TVec3Prop): TVec3;
begin
  Result := A.Value + B.Value;
end;

class operator TVec3Prop.Subtract(const A, B: TVec3Prop): TVec3;
begin
  Result := A.Value - B.Value;
end;

class operator TVec3Prop.Multiply(const A: TVec3Prop; B: Float): TVec3;
begin
  Result := A.Value * B;
end;

class operator TVec3Prop.Divide(const A: TVec3Prop; B: Float): TVec3;
begin
  Result := A.Value / B;
end;

procedure TVec3Prop.Link(OnChange: TDependencyChangeNotify = nil);
begin
  DependencyLink(FProp, 3, OnChange);
  FIndex := 0;
end;

procedure TVec3Prop.Link(Prop: IDependencyProperty; Index: LongInt);
begin
  FProp := Prop;
  FIndex := Index;
end;

procedure TVec3Prop.Unlink;
begin
  FProp := nil;
end;

function TVec3Prop.Linked: Boolean;
begin
  Result := FProp <> nil;
end;

function TVec3Prop.GetValue: TVec3;
begin
  if FProp = nil then
    Result := FValue
  else
  begin
    Result.X := FProp.GetValue(FIndex);
    Result.Y := FProp.GetValue(FIndex + 1);
    Result.Z := FProp.GetValue(FIndex + 2);
  end;
end;

procedure TVec3Prop.SetValue(const Value: TVec3);
begin
  if FProp = nil then
    FValue := Value
  else
  begin
    FProp.SetValue(Value.X, FIndex);
    FProp.SetValue(Value.Y, FIndex + 1);
    FProp.SetValue(Value.Z, FIndex + 2);
  end;
end;

function TVec3Prop.GetVec(Index: Integer): TVec1Prop;
var
  V: TVec1Prop;
begin
  UIntPtr(V.FProp) := 0;
  if FProp = nil then
  begin
    if Index < 1 then
      V.FValue := FValue.X
    else if Index < 2 then
      V.FValue := FValue.Y
    else
      V.FValue := FValue.Z;
  end
  else
  begin
    V.FProp := FProp;
    if Index < 1 then
      V.FIndex := FIndex
    else if Index < 2 then
      V.FIndex := FIndex + 1
    else
      V.FIndex := FIndex + 2;
  end;
  Exit(V);
end;

procedure TVec3Prop.SetVec(Index: Integer; const Value: TVec1Prop);
begin
  if FProp = nil then
  begin
    FProp := nil;
    if Index < 1 then
      FValue.X := Value.Value
    else if Index < 2 then
      FValue.Y := Value.Value
    else
      FValue.Z := Value.Value;
  end
  else
  begin
    if Index < 1 then
      FProp.SetValue(Value.Value, FIndex)
    else if Index < 2 then
      FProp.SetValue(Value.Value, FIndex + 1)
    else
      FProp.SetValue(Value.Value, FIndex + 2);
  end;
end;

function TVec3Prop.GetAsVec2: TVec2Prop;
begin
  Result.Link(FProp, 0);
end;

procedure TVec3Prop.SetAsVec2(const Value: TVec2Prop);
var
  V: TVec2;
begin
  V := Value.Value;
  X := V.X;
  Y := V.Y;
end;

{ TVec4Prop }

class operator TVec4Prop.Implicit(const Value: TVec4): TVec4Prop;
begin
  UIntPtr(Result.FProp) := 0;
  Result.FValue := Value;
end;

class operator TVec4Prop.Implicit(const Value: TVec4Prop): TVec4;
begin
  Result := Value.Value;
end;

class operator TVec4Prop.Implicit(Value: TColorB): TVec4Prop;
begin
  UIntPtr(Result.FProp) := 0;
  Result.FValue := Value;
end;

class operator TVec4Prop.Explicit(const Value: TVec4Prop): TColorB;
begin
  Result := TColorB(Value.Value);
end;

class operator TVec4Prop.Implicit(const Value: TColorF): TVec4Prop;
begin
  UIntPtr(Result.FProp) := 0;
  Result.FValue := TVec4(Value);
end;

class operator TVec4Prop.Implicit(const Value: TVec4Prop): TColorF;
begin
  Result := TColorF(Value.Value);
end;

procedure TVec4Prop.Link(OnChange: TDependencyChangeNotify = nil);
begin
  DependencyLink(FProp, 4, OnChange);
  FIndex := 0;
end;

procedure TVec4Prop.Link(Prop: IDependencyProperty; Index: LongInt);
begin
  FProp := Prop;
  FIndex := Index;
end;

procedure TVec4Prop.Unlink;
begin
  FProp := nil;
end;

function TVec4Prop.Linked: Boolean;
begin
  Result := FProp <> nil;
end;

function TVec4Prop.GetValue: TVec4;
begin
  if FProp = nil then
    Result := FValue
  else
  begin
    Result.X := FProp.GetValue(FIndex);
    Result.Y := FProp.GetValue(FIndex + 1);
    Result.Z := FProp.GetValue(FIndex + 2);
    Result.W := FProp.GetValue(FIndex + 3);
  end;
end;

procedure TVec4Prop.SetValue(const Value: TVec4);
begin
  if FProp = nil then
    FValue := Value
  else
  begin
    FProp.SetValue(Value.X, FIndex);
    FProp.SetValue(Value.Y, FIndex + 1);
    FProp.SetValue(Value.Z, FIndex + 2);
    FProp.SetValue(Value.W, FIndex + 3);
  end;
end;

function TVec4Prop.GetVec(Index: Integer): TVec1Prop;
var
  V: TVec1Prop;
begin
  UIntPtr(V.FProp) := 0;
  if FProp = nil then
  begin
    if Index < 1 then
      V.FValue := FValue.X
    else if Index < 2 then
      V.FValue := FValue.Y
    else if Index < 3 then
      V.FValue := FValue.Z
    else
      V.FValue := FValue.W;
  end
  else
  begin
    V.FProp := FProp;
    if Index < 1 then
      V.FIndex := FIndex
    else if Index < 2 then
      V.FIndex := FIndex + 1
    else if Index < 3 then
      V.FIndex := FIndex + 2
    else
      V.FIndex := FIndex + 3;
  end;
  Exit(V);
end;

procedure TVec4Prop.SetVec(Index: Integer; const Value: TVec1Prop);
begin
  if FProp = nil then
  begin
    FProp := nil;
    if Index < 1 then
      FValue.X := Value.Value
    else if Index < 2 then
      FValue.Y := Value.Value
    else if Index < 3 then
      FValue.Z := Value.Value
    else
      FValue.W := Value.Value;
  end
  else
  begin
    if Index < 1 then
      FProp.SetValue(Value.Value, FIndex)
    else if Index < 2 then
      FProp.SetValue(Value.Value, FIndex + 1)
    else if Index < 3 then
      FProp.SetValue(Value.Value, FIndex + 2)
    else
      FProp.SetValue(Value.Value, FIndex + 3);
  end;
end;

function TVec4Prop.GetAsVec2: TVec2Prop;
begin
  Result.Link(FProp, 0);
end;

procedure TVec4Prop.SetAsVec2(const Value: TVec2Prop);
var
  V: TVec2;
begin
  V := Value.Value;
  X := V.X;
  Y := V.Y;
end;

function TVec4Prop.GetAsVec3: TVec3Prop;
begin
  Result.Link(FProp, 0);
end;

procedure TVec4Prop.SetAsVec3(const Value: TVec3Prop);
var
  V: TVec3;
begin
  V := Value.Value;
  X := V.X;
  Y := V.Y;
  Z := V.Z;
end;

{ TDependencyProperty }

type
  TPropertyValues = TArray<Float>;

  TDependencyProperty = class(TInterfacedObject, IDependencyProperty)
  private
    FValues: TPropertyValues;
    FOnChange: TDependencyChangeNotify;
  public
    function GetCount: Integer;
    function GetValue(Index: Integer): Float;
    procedure SetValue(Value: Float; Index: Integer);
  end;

function TDependencyProperty.GetCount: Integer;
begin
  Result := Length(FValues);
end;

function TDependencyProperty.GetValue(Index: Integer): Float;
begin
  Result := FValues[Index];
end;

procedure TDependencyProperty.SetValue(Value: Float; Index: Integer);
begin
  if FValues[Index] <> Value then
  begin
    FValues[Index] := Value;
    if Assigned(FOnChange) then
      FOnChange(Self, Index);
  end;
end;

procedure DependencyLink(var Prop: IDependencyProperty; Count: Integer; OnChange: TDependencyChangeNotify);
var
  Dependency: TDependencyProperty;
begin
  if Prop = nil then
    Dependency := TDependencyProperty.Create
  else
    Dependency := Prop as TDependencyProperty;
  SetLength(Dependency.FValues, Count);
  Dependency.FOnChange := OnChange;
  Prop := Dependency;
end;

procedure DependencyUnlink(var Prop: IDependencyProperty);
var
  Dependency: TDependencyProperty;
begin
  if Prop = nil then
    Exit;
  Dependency := Prop as TDependencyProperty;
  Dependency.FOnChange := nil;
  Prop := nil;
end;

function VectorPropertyEmpty(out Prop: TVectorProperty): Boolean;
begin
  Prop.Vec1Prop.Value := 0;
  Prop.Vec2Prop.Value := Vec2(0);
  Prop.Vec3Prop.Value := Vec3(0);
  Prop.Vec4Prop.Value := Vec4(0);
  Prop.Resolver := nil;
  Result := False;
end;

{ TAnimator }

var
  InternalAnimator: TObject;

function Animator: TAnimator;
begin
  if InternalAnimator = nil then
    InternalAnimator := TAnimator.Create;
  Result := TAnimator(InternalAnimator);
end;

const
  NegCosPi = 1.61803398874989; { 2 / -Cos(Pi * 1.2) }

class function TEasingDefaults.Linear(Percent: Float): Float;
begin
  Result := Percent;
end;

class function TEasingDefaults.Easy(Percent: Float): Float;
begin
  Result := Percent * Percent * (3 - 2 * Percent);
end;

class function TEasingDefaults.EasySlow(Percent: Float): Float;
begin
  Percent := Easy(Percent);
  Result := Percent * Percent * (3 - 2 * Percent);
end;

class function TEasingDefaults.Extend(Percent: Float): Float;
begin
  Percent := (Percent * 1.4) - 0.2;
  Result := 0.5 - Cos(Pi * Percent) / NegCosPi;
end;

class function Power(const Base, Exponent: Float): Float;
begin
  if Exponent = 0 then
    Result := 1
  else if (Base = 0) and (Exponent > 0) then
    Result := 0
  else
    Result := Exp(Exponent * Ln(Base));
end;

class function TEasingDefaults.Drop(Percent: Float): Float;
begin
  Result := Percent * Percent;
end;

class function TEasingDefaults.DropSlow(Percent: Float): Float;
begin
  Result := Percent * Percent * Percent * Percent * Percent;
end;

class function TEasingDefaults.Snap(Percent: Float): Float;
begin
  Percent := Percent * Percent;
  Percent := (Percent * 1.4) - 0.2;
  Result := 0.5 - Cos(Pi * Percent) / NegCosPi;
end;

class function TEasingDefaults.Bounce(Percent: Float): Float;
begin
  if Percent > 0.9 then
  begin
    Result := Percent - 0.95;
    Result := 1 + Result * Result * 20 - (0.05 * 0.05 * 20);
  end
  else if Percent > 0.75 then
  begin
    Result := Percent - 0.825;
    Result := 1 + Result * Result * 16 - (0.075 * 0.075 * 16);
  end
  else if Percent > 0.5 then
  begin
    Result := Percent - 0.625;
    Result := 1 + Result * Result * 12 - (0.125 * 0.125 * 12);
  end
  else
  begin
    Percent := Percent * 2;
    Result := Percent * Percent;
  end;
end;

class function TEasingDefaults.Bouncy(Percent: Float): Float;
var
  Scale, Start, Step: Float;
begin
  Result := 1;
  Scale := 5;
  Start := 0.5;
  Step := 0.2;
  if Percent < Start then
  begin
    Result := Percent / Start;
    Result :=  Result * Result;
  end
  else
  while Step > 0.01 do
    if Percent < Start + Step then
    begin
      Step := Step / 2;
      Result := (Percent - (Start + Step)) * Scale;
      Result :=  Result * Result;
      Result := Result + 1 - Power(Step * Scale, 2);
      Break;
    end
    else
    begin
      Start := Start + Step;
      Step := Step * 0.6;
    end;
end;

class function TEasingDefaults.Rubber(Percent: Float): Float;
begin
  if Percent > 0.9 then
  begin
    Result := Percent - 0.95;
    Result := 1 - Result * Result * 20 + (0.05 * 0.05 * 20);
  end
  else if Percent > 0.75 then
  begin
    Result := Percent - 0.825;
    Result := 1 + Result * Result * 18 - (0.075 * 0.075 * 18);
  end
  else if Percent > 0.5 then
  begin
    Result := Percent - 0.625;
    Result := 1 - Result * Result * 14 + (0.125 * 0.125 * 14);
  end
  else
  begin
    Percent := Percent * 2;
    Result := Percent * Percent;
  end;
end;

class function TEasingDefaults.Spring(Percent: Float): Float;
begin
  Percent := Percent * Percent;
  Result := Sin(PI * Percent * Percent * 10 - PI / 2) / 4;
  Result := Result * (1 - Percent) + 1;
  if Percent < 0.3 then
    Result := Result * Easy(Percent / 0.3);
end;

class function TEasingDefaults.Boing(Percent: Float): Float;
begin
  Percent := Power(Percent, 1.5);
  Result := Sin(PI * Power(Percent, 2) * 20 - PI / 2) / 4;
  Result := Result * (1 - Percent) + 1;
  if Percent < 0.2 then
    Result := Result * Easy(Percent / 0.2);
end;

function TEasings.DefaultValue: TEasing;
begin
  Result := @TEasingDefaults.Linear;
end;

function EasingKeyCompare(constref A, B: string): Integer;
begin
  Result := StrCompare(A, B, True);
end;


procedure TEasings.RegisterDefaults;
begin
  Comparer := EasingKeyCompare;
  Self['Linear'] := @TEasingDefaults.Linear;
  Self['Easy'] := @TEasingDefaults.Easy;
  Self['EasySlow'] := @TEasingDefaults.EasySlow;
  Self['Extend'] := @TEasingDefaults.Extend;
  Self['Drop'] := @TEasingDefaults.Drop;
  Self['DropSlow'] := @TEasingDefaults.DropSlow;
  Self['Snap'] := @TEasingDefaults.Snap;
  Self['Bounce'] := @TEasingDefaults.Bounce;
  Self['Bouncy'] := @TEasingDefaults.Bouncy;
  Self['Rubber'] := @TEasingDefaults.Rubber;
  Self['Spring'] := @TEasingDefaults.Spring;
  Self['Boing'] := @TEasingDefaults.Boing;
end;

function Interpolate(Easing: TEasing; Percent: Float; Reverse: Boolean = False): Float;
begin
  if Percent < 0 then
    Result := 0
  else if Percent > 1 then
    Result := 1
  else if Reverse then
    Result := 1 - Easing(1 - Percent)
  else
    Result := Easing(Percent);
end;

function Interpolate(Easing: TEasing; Percent: Float; Start, Finish: Float; Reverse: Boolean = False): Float;
begin
  if Percent < 0 then
    Result := Start
  else if Percent > 1 then
    Result := Finish
  else
  begin
    if Reverse then
      Percent := 1 - Easing(1 - Percent)
    else
      Percent := Easing(Percent);
    Result := Start * (1 - Percent) + Finish * Percent;
  end;
end;

{ TAnimationThread }

type
  TAnimationThread = class(TThread)
  private
    procedure Animate;
  protected
    procedure Execute; override;
  public
    constructor Create;
  end;

{ TThreadedTimer }

  TThreadedTimer = class(TObject)
  private
    FTimerCount: Integer;
    FOnTimer: TNotifyDelegate;
    function GetOnTimer: INotifyDelegate;
  public
    destructor Destroy; override;
    property OnTimer: INotifyDelegate read GetOnTimer;
    procedure Enable;
    procedure Disable;
  end;

{ TThreadedTimer }

var
  InternalThreadedTimer: TObject;

function ThreadedTimer: TThreadedTimer;
begin
  if InternalThreadedTimer = nil then
    InternalThreadedTimer := TThreadedTimer.Create;
  Result := TThreadedTimer(InternalThreadedTimer);
end;

var
  InternalThread: TObject;

destructor TThreadedTimer.Destroy;
begin
  InternalThread := nil;
  inherited Destroy;
end;

function TThreadedTimer.GetOnTimer: INotifyDelegate;
begin
  Result := FOnTimer;
end;

procedure TThreadedTimer.Enable;
begin
  if InterLockedIncrement(FTimerCount) = 1 then
    TAnimationThread.Create;
end;

procedure TThreadedTimer.Disable;
begin
  if InterLockedDecrement(FTimerCount) = 0 then
    InternalThread := nil;
end;

{ TAnimationThread }

constructor TAnimationThread.Create;
begin
  InternalThread := Self;
  inherited Create(False);
end;

procedure TAnimationThread.Animate;
var
  Event: TNotifyEvent;
begin
  if InternalThread <> Self then
    Exit;
  if InternalThreadedTimer = nil then
    Exit;
  for Event in TThreadedTimer(InternalThreadedTimer).FOnTimer do
    Event(TThreadedTimer(InternalThreadedTimer));
end;

const
  TimerRate = 30;

procedure TAnimationThread.Execute;
const
  Delay = 1 / TimerRate;
var
  A, B: Double;
begin
  A := TimeQuery;
  FreeOnTerminate := True;
  while InternalThread = Self do
  begin
    Synchronize(Animate);
    if InternalThread <> Self then
      Exit;
    B := TimeQuery - A;
    while B < Delay do
    begin
      B := (Delay - B)  * 1000;
      Sleep(Round(B));
      B := TimeQuery - A;
    end;
    A := TimeQuery - (B - Delay);
  end;
end;

{ TAnimationTimer }

constructor TAnimationTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ThreadedTimer.OnTimer.Add(Timer);
end;

destructor TAnimationTimer.Destroy;
begin
  Enabled := False;
  ThreadedTimer.OnTimer.Remove(Timer);
  inherited Destroy;
end;

procedure TAnimationTimer.Timer(Sender: TObject);
begin
  if FEnabled and Assigned(FOnTimer) then
    FOnTimer(Self);
end;

procedure TAnimationTimer.SetEnabled(Value: Boolean);
begin
  if FEnabled = Value then Exit;
  FEnabled := Value;
  if csDesigning in ComponentState then Exit;
  if FEnabled then
    ThreadedTimer.Enable
  else
    ThreadedTimer.Disable;
end;

{ TAnimator }

procedure TAnimator.Animate(var Prop: Float; Target: Float; const Easing: string; Duration: Double = 0.25);
var
  E: TEasing;
begin
  E := nil;
  if Easings.KeyExists(Easing) then
    E := Easings[Easing];
  Animate(nil, Prop, Target, E, Duration);
end;

procedure TAnimator.Animate(var Prop: Float; Target: Float; Easing: TEasing = nil; Duration: Double = 0.25);
begin
  Animate(nil, Prop, Target, Easing, Duration);
end;

procedure TAnimator.Animate(NotifyObject: TObject; var Prop: Float; Target: Float;
  const Easing: string; Duration: Double = 0.25);
var
  E: TEasing;
begin
  E := nil;
  if Easings.KeyExists(Easing) then
    E := Easings[Easing];
  Animate(NotifyObject, Prop, Target, E, Duration);
end;

procedure TAnimator.Animate(NotifyObject: TObject; var Prop: Float; Target: Float;
  Easing: TEasing = nil; Duration: Double = 0.25);
var
  Notify: IFloatPropertyNotify;
  Event: TNotifyEvent;
  Item: TAnimationItem;
begin
  Stop(Prop);
  if (NotifyObject <> nil) and (NotifyObject is IFloatPropertyNotify) then
    Notify := NotifyObject as IFloatPropertyNotify
  else
    Notify := nil;
  if Duration <= 0 then
  begin
    Prop := Target;
    if Notify <> nil then
      Notify.PropChange(@Prop);
    Exit;
  end;
  Item.Notify := Notify;
  Item.Prop := @Prop;
  Item.StartTarget := Prop;
  Item.StopTarget := Target;
  Item.StartTime := TimeQuery;
  Item.StopTime := Item.StartTime + Duration;
  if @Easing = nil then
    Easing := TEasingDefaults.Easy;
  Item.Easing := Easing;
  if FAnimations.Length = 0 then
    for Event in FOnStart do
      Event(Self);
  FAnimations.Push(Item);
end;

procedure TAnimator.Stop(var Prop: Float);
var
  Item: PAnimationItem;
  I: Integer;
begin
  FAnimated := True;
  for I := FAnimations.Length - 1 downto 0 do
  begin
    Item := @FAnimations.Items[I];
    if Item.Prop = @Prop then
    begin
      if Item.Notify <> nil then
        Item.Notify.PropChange(Item.Prop);
      FAnimations.Delete(I);
      Exit;
    end;
  end;
end;

procedure TAnimator.Step;
var
  Event: TNotifyEvent;
  Time: Double;
  Percent: Float;
  Item: PAnimationItem;
  I: Integer;
begin
  Time := TimeQuery;
  FAnimated := FAnimations.Length > 0;
  if not FAnimated then
  begin
    for Event in FOnStop do
      Event(Self);
    Exit;
  end;
  for I := FAnimations.Length - 1 downto 0 do
  begin
    Item := @FAnimations.Items[I];
    if Time >= Item.StopTime then
    begin
      Item.Prop^ := Item.StopTarget;
      if Item.Notify <> nil then
        Item.Notify.PropChange(Item.Prop);
      FAnimations.Delete(I);
      Continue;
    end;
    Percent := (Time - Item.StartTime) / (Item.StopTime - Item.StartTime);
    Item.Prop^ := Interpolate(Item.Easing, Percent, Item.StartTarget, Item.StopTarget);
    if Item.Notify <> nil then
      Item.Notify.PropChange(Item.Prop);
  end;
end;

finalization
  InternalThreadedTimer.Free;
  InternalEasings.Free;
  InternalAnimator.Free;
end.

