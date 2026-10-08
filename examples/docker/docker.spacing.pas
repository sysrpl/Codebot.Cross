unit Docker.Spacing;

{$mode delphi}

interface

uses
  Codebot.System;

type
  TDockItem = record
    Offset: Float;
    Size: Float;
  end;

  TDockItems = TArray<TDockItem>;

  { TDock }

  TDock = class
  private
    FNames: StringArray;
    FCount: Integer;
    FLargeSize: Integer;
    FSmallSize: Integer;
    FStretch: Integer;
    function GetName(Index: Integer): string;
    procedure SetLargeSize(Value: Integer);
    procedure SetSmallSize(Value: Integer);
    procedure SetStretch(Value: Integer);
  public
    constructor Create(const Names: string = '');
    function MouseMove(MouseCoord, Area: Integer; out Items: TDockItems): Integer;
    property Name[Index: Integer]: string read GetName;
    property Count: Integer read FCount write FCount;
    property SmallSize: Integer read FSmallSize write SetSmallSize;
    property LargeSize: Integer read FLargeSize write SetLargeSize;
    property Stretch: Integer read FStretch write SetStretch;
  end;

{ DockerQuery returns the current item }

function DockerQuery(MouseCoord, SmallSize, LargeSize, Stretch, Area, Count: Integer;
  out Items: TDockItems): Integer;

implementation


function DockerQuery(MouseCoord, SmallSize, LargeSize, Stretch, Area, Count: Integer;
  out Items: TDockItems): Integer;
var
  X, J: Float;
  I: Integer;
begin
  Result := -1;
  if Count < 1 then
  begin
    SetLength(Items, 0);
    Exit;
  end;
  SetLength(Items, Count);
  J := Area / 2 - (SmallSize * Count) / 2;
  for I := 0 to Count - 1 do
  begin
    Items[I].Offset := J;
    Items[I].Size := SmallSize;
    if (MouseCoord >= J) and (MouseCoord < J + SmallSize) then
    begin
      Result := I;
      X := (MouseCoord - J) / SmallSize;
      Items[I].Offset := J - LargeSize * X + SmallSize * X;
      Items[I].Size := LargeSize;
    end
    else if MouseCoord < J then
    begin
      X := J - MouseCoord;
      if X > Stretch then
        X := 1
      else
        X := X / Stretch;
      X := 1 - X;
      Items[I].Size := X * LargeSize + (1 - X) * SmallSize;
    end
    else if MouseCoord > J then
    begin
      X := MouseCoord - (J + SmallSize);
      if X > Stretch then
        X := 1
      else
        X := X / Stretch;
      X := 1 - X;
      Items[I].Size := X * LargeSize + (1 - X) * SmallSize;
    end;
    J := J + SmallSize;
  end;
  if Result > -1 then
  begin
    J := Items[Result].Offset;
    for I := Result - 1 downto 0 do
    begin
      Items[I].Offset := J - Items[I].Size;
      J := Items[I].Offset;
    end;
    J := Items[Result].Offset + Items[Result].Size;
    for I := Result + 1 to Count - 1 do
    begin
      Items[I].Offset := J;
      J := J + Items[I].Size;
    end;
  end
  else if MouseCoord < Area / 2 then
  begin
    J := Items[0].Offset + Items[0].Size;
    for I := 1 to Count - 1 do
    begin
      Items[I].Offset := J;
      J := J + Items[I].Size;
    end;
  end
  else if MouseCoord > Area / 2 then
  begin
    Items[Count - 1].Offset := Round(Area / 2 + (SmallSize * Count) / 2) - Items[Count - 1].Size;
    J := Items[Count - 1].Offset;
    for I := Count - 2 downto 0 do
    begin
      J := J - Items[I].Size;
      Items[I].Offset := J;
    end;
  end;
end;

{ TDock }

constructor TDock.Create(const Names: string = '');
begin
  inherited Create;
  FNames := Names.Split(',');
  FCount := FNames.Length;
  FSmallSize := 32;
  FLargeSize := 96;
  FStretch := 64;
end;

function TDock.MouseMove(MouseCoord, Area: Integer; out Items: TDockItems): Integer;
begin
  Result := DockerQuery(MouseCoord, FSmallSize, FLargeSize, FStretch, Area, FCount, Items);
end;

function TDock.GetName(Index: Integer): string;
begin
  if (Index < 0) or (Index > FNames.Length - 1) then
    Result := ''
  else
    Result := FNames[Index];
end;

procedure TDock.SetLargeSize(Value: Integer);
begin
  if Value < 1 then Value := 1;
  if Value < FSmallSize then Value := FSmallSize;
  FLargeSize := Value;
end;

procedure TDock.SetSmallSize(Value: Integer);
begin
  if Value < 1 then Value := 1;
  FSmallSize := Value;
  if FSmallSize > FLargeSize then
    FLargeSize := FSmallSize;
end;

procedure TDock.SetStretch(Value: Integer);
begin
  if Value < 1 then Value := 1;
  FStretch := Value;
end;

end.

