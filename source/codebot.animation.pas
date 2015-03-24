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
  Codebot.System,
  Codebot.Collections;

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

implementation

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
  Result := Percent * Percent * Percent;
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

var
  InternalEasings: TEasings;

function Easings: TEasings;
begin
  if InternalEasings = nil then
  begin
    InternalEasings := TEasings.Create;
    InternalEasings.RegisterDefaults;
  end;
  Result := InternalEasings;
end;

finalization
  InternalEasings.Free;
end.


