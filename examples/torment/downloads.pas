unit Downloads;

{$mode delphi}

interface

uses
  SysUtils,
  Codebot.System;

{ IFileDownload }

type
  IFileDownload = interface
  ['{7F235DB8-56FD-4538-9020-4B7E37F4C98A}']
    function GetAddedOn: TDateTime;
    function GetCompletedOn: TDateTime;
    function GetDownloadSpeed: Integer;
    function GetEta: Integer;
    function GetHealth: Integer;
    function GetKind: string;
    function GetName: string;
    function GetPeers: Integer;
    function GetProgress: Integer;
    function GetRating: Integer;
    function GetSeeds: Integer;
    function GetSize: Integer;
    function GetUploadSpeed: Integer;
    procedure Step;
    property Name: string read GetName;
    property Size: Integer read GetSize;
    property Progress: Integer read GetProgress;
    property AddedOn: TDateTime read GetAddedOn;
    property CompletedOn: TDateTime read GetCompletedOn;
    property Kind: string read GetKind;
    property UploadSpeed: Integer read GetUploadSpeed;
    property DownloadSpeed: Integer read GetDownloadSpeed;
    property Peers: Integer read GetPeers;
    property Seeds: Integer read GetSeeds;
    property Health: Integer read GetHealth;
    property Eta: Integer read GetEta;
    property Rating: Integer read GetRating;
  end;

function NewDownload: IFileDownload;

type
  TFileDownloadList = TArrayList<IFileDownload>;
  TFileDownloadCompare = TCompare<IFileDownload>;

const
  tagName = 0;
  tagSize = tagName + 1;
  tagProgress = tagSize + 1;
  tagEta = tagProgress + 1;
  tagHealth = tagEta + 1;
  tagKind = tagHealth + 1;
  tagUp = tagKind + 1;
  tagDown = tagUp + 1;
  tagRating = tagDown + 1;
  tagSeeds = tagRating + 1;
  tagPeers = tagSeeds + 1;

function DownloadCompare(Tag: Integer): TFileDownloadCompare;

const
  FieldNames = 'Torrent,Size,Progress,Eta,Health,Type,Up Speed,Down Speed,Rating,Seeds,Peers';
  FieldWidths = '200,75,125,100,100,100,75,75,100,50,50';
  FieldAligns = '0,1,2,1,2,0,1,1,0,1,1';

function BytesToStr(I: Integer): string;
function EtaToStr(I: Integer): string;

implementation

{ TFileDownload }

type
  TFileDownload = class(TInterfacedObject, IFileDownload)
  private
    FAddedOn: TDateTime;
    FCompletedOn: TDateTime;
    FDownloadSpeed: Integer;
    FName: string;
    FKind: string;
    FPeers: Integer;
    FProgress: Integer;
    FRating: Integer;
    FSeeds: Integer;
    FSize: Integer;
    FEta: Integer;
    FUploadSpeed: Integer;
  public
    constructor Create;
    procedure Step;
    function GetAddedOn: TDateTime;
    function GetCompletedOn: TDateTime;
    function GetKind: string;
    function GetDownloadSpeed: Integer;
    function GetEta: Integer;
    function GetHealth: Integer;
    function GetName: string;
    function GetPeers: Integer;
    function GetProgress: Integer;
    function GetRating: Integer;
    function GetSeeds: Integer;
    function GetSize: Integer;
    function GetUploadSpeed: Integer;
  end;

const
  Nouns =
    'Dream,Dreamer,Dreams,Waves,' +
    'Sword,Kiss,Sex,Lover,' +
    'Slave,Slaves,Pleasure,Servant,' +
    'Servants,Snake,Soul,Touch,' +
    'Men,Women,Gift,Scent,' +
    'Ice,Snow,Night,Silk,Secret,Secrets,' +
    'Game,Fire,Flame,Flames,' +
    'Husband,Wife,Man,Woman,Boy,Girl,' +
    'Truth,Edge,Boyfriend,Girlfriend,' +
    'Body,Captive,Male,Wave,Predator,' +
    'Female,Healer,Trainer,Teacher,' +
    'Hunter,Obsession,Hustler,Consort,' +
    'Dream, Dreamer, Dreams,Rainbow,' +
    'Dreaming,Flight,Flying,Soaring,' +
    'Wings,Mist,Sky,Wind,' +
    'Winter,Misty,River,Door,' +
    'Gate,Cloud,Fairy,Dragon,' +
    'End,Blade,Beginning,Tale,' +
    'Tales,Emperor,Prince,Princess,' +
    'Willow,Birch,Petals,Destiny,' +
    'Theft,Thief,Legend,Prophecy,' +
    'Spark,Sparks,Stream,Streams,Waves,' +
    'Sword,Darkness,Swords,Silence,Kiss,' +
    'Butterfly,Shadow,Ring,Rings,Emerald,' +
    'Storm,Storms,Mists,World,Worlds,' +
    'Alien,Lord,Lords,Ship,Ships,Star,' +
    'Stars,Force,Visions,Vision,Magic,' +
    'Wizards,Wizard,Heart,Heat,Twins,' +
    'Twilight,Moon,Moons,Planet,Shores,' +
    'Pirates,Courage,Time,Academy,' +
    'School,Rose,Roses,Stone,Stones,' +
    'Sorcerer,Shard,Shards,Slave,Slaves,' +
    'Servant,Servants,Serpent,Serpents,' +
    'Snake,Soul,Souls,Savior,Spirit,' +
    'Spirits,Voyage,Voyages,Voyager,Voyagers,' +
    'Return,Legacy,Birth,Healer,Healing,' +
    'Year,Years,Death,Dying,Luck,Elves,' +
    'Tears,Touch,Son,Sons,Child,Children,' +
    'Illusion,Sliver,Destruction,Crying,Weeping,' +
    'Gift,Word,Words,Thought,Thoughts,Scent,' +
    'Ice,Snow,Night,Silk,Guardian,Angel,' +
    'Angels,Secret,Secrets,Search,Eye,Eyes,' +
    'Danger,Game,Fire,Flame,Flames,Bride,' +
    'Husband,Wife,Time,Flower,Flowers,' +
    'Light,Lights,Door,Doors,Window,Windows,' +
    'Bridge,Bridges,Ashes,Memory,Thorn,' +
    'Thorns,Name,Names,Future,Past,' +
    'History,Something,Nothing,Someone,' +
    'Nobody,Person,Man,Woman,Boy,Girl,' +
    'Way,Mage,Witch,Witches,Lover,' +
    'Tower,Valley,Abyss,Hunter,' +
    'Truth,Edge';

const
  Adjectives =
    'Lost,Only,Last,First,' +
    'Third,Sacred,Bold,Lovely,' +
    'Final,Missing,Shadowy,Seventh,' +
    'Dwindling,Missing,Absent,' +
    'Vacant,Cold,Hot,Burning,Forgotten,' +
    'Weeping,Dying,Lonely,Silent,' +
    'Laughing,Whispering,Forgotten,Smooth,' +
    'Silken,Rough,Frozen,Wild,' +
    'Trembling,Fallen,Ragged,Broken,' +
    'Cracked,Splintered,Slithering,Silky,' +
    'Wet,Magnificent,Luscious,Swollen,' +
    'Erect,Bare,Naked,Stripped,' +
    'Captured,Stolen,Sucking,Licking,' +
    'Growing,Kissing,Green,Red,Blue,' +
    'Azure,Rising,Falling,Elemental,' +
    'Bound,Prized,Obsessed,Unwilling,' +
    'Hard,Eager,Ravaged,Sleeping,' +
    'Wanton,Professional,Willing,Devoted,' +
    'Misty,Lost,Only,Last,First,' +
    'Final,Missing,Shadowy,Seventh,' +
    'Dark,Darkest,Silver,Silvery,Living,' +
    'Black,White,Hidden,Entwined,Invisible,' +
    'Next,Seventh,Red,Green,Blue,' +
    'Purple,Grey,Bloody,Emerald,Diamond,' +
    'Frozen,Sharp,Delicious,Dangerous,' +
    'Deep,Twinkling,Dwindling,Missing,Absent,' +
    'Vacant,Cold,Hot,Burning,Forgotten,' +
    'Some,No,All,Every,Each,Which,What,' +
    'Playful,Silent,Weeping,Dying,Lonely,Silent,' +
    'Laughing,Whispering,Forgotten,Smooth,Silken,' +
    'Rough,Frozen,Wild,Trembling,Fallen,' +
    'Ragged,Broken,Cracked,Splintered';

var
  NounList: StringArray;
  AdjectiveList: StringArray;

function RandomName: string;
var
  A, B: string;
begin
  if NounList.Length = 0 then
  begin
    Randomize;
    NounList := Nouns.Split(',');
    AdjectiveList := Adjectives.Split(',');
  end;
  A := AdjectiveList[Random(AdjectiveList.Length)];
  B := NounList[Random(NounList.Length)];
  case Random(6) of
    0: Result := A + ' ' + B;
    1: Result := 'The ' + A + ' ' + B;
    2: Result := A + ' of ' + B;
    3: Result := 'The ' + A + '''s ' + B;
    4: Result := 'The ' + A + ' of the ' + B;
    5: Result := A + ' in the ' + B;
  end;
end;

constructor TFileDownload.Create;
begin
  inherited Create;
  FName := RandomName;
  FAddedOn := Now - Random(100000) * 30;
  FDownloadSpeed := Random(100000) + 20000;
  FPeers := Random(200) + 1;
  FSeeds := FPeers div 8 + 2 + Random(100);
  FUploadSpeed := Round(FDownloadSpeed / 10) + Random(10000);
  FRating := Random(11);
  if Random(10) < 5 then
    FRating := 0;
  FSize := 1000 + Random(10000000);
  if Random(10) < 8 then
    FSize := FSize * 100;
  if FSize < 1024 * 500 then
  begin
    FKind := 'text/plain';
    FName := FName + '.txt';
  end
  else if FSize < 1024 * 1024 * 8 then
  begin
    FKind := 'audio/mp3';
    FName := FName + '.mp3';
  end
  else if FSize < 1024 * 1024 * 75 then
  begin
    FKind := 'application/zip';
    FName := FName + '.zip';
  end
  else if FSize < 1024 * 1024 * 350 then
  begin
    FKind := 'video/mp4';
    FName := FName + '.mp4';
  end
  else
  begin
    FKind := 'video/avi';
    FName := FName + '.avi';
  end;
  FEta := FSize div FDownloadSpeed;
end;

procedure TFileDownload.Step;
begin
  FProgress := FProgress + FDownloadSpeed;
  FSeeds := FSeeds + Random(2) - 1;
  if FSeeds < 0 then FSeeds := 0;
  FPeers := FPeers + Random(10) - 5;
  if FPeers < 0 then FPeers := 0;
  FUploadSpeed := FUploadSpeed + Random(10000) - 5000;
  if FUploadSpeed < 0 then FUploadSpeed := 0;
  FDownloadSpeed := FDownloadSpeed + Random(30000) - 15000;
  if FDownloadSpeed < 1 then FDownloadSpeed := 1;
  if FProgress >= FSize then
  begin
    FProgress := FSize;
    FDownloadSpeed := 0;
    FEta := 0;
  end
  else
    FEta := (FSize - FProgress) div FDownloadSpeed;
end;

function TFileDownload.GetAddedOn: TDateTime;
begin
  Result := FAddedOn;
end;

function TFileDownload.GetCompletedOn: TDateTime;
begin
  Result := FCompletedOn;
end;

function TFileDownload.GetKind: string;
begin
  Result := FKind;
end;

function TFileDownload.GetDownloadSpeed: Integer;
begin
  Result := FDownloadSpeed;
end;

function TFileDownload.GetEta: Integer;
begin
  Result := FEta;
end;

function TFileDownload.GetHealth: Integer;
begin
  Result := Round((FSeeds + FPeers) / 25);
  if Result > 5 then
    Result := 5;
end;

function TFileDownload.GetName: string;
begin
  Result := FName;
end;

function TFileDownload.GetPeers: Integer;
begin
  Result := FPeers;
end;

function TFileDownload.GetProgress: Integer;
begin
  Result := FProgress;
end;

function TFileDownload.GetRating: Integer;
begin
  Result := FRating;
end;

function TFileDownload.GetSeeds: Integer;
begin
  Result := FSeeds;
end;

function TFileDownload.GetSize: Integer;
begin
  Result := FSize;
end;

function TFileDownload.GetUploadSpeed: Integer;
begin
  Result := FUploadSpeed;
end;

function SortName(constref A, B: IFileDownload): Integer;
begin
  Result := A.Name.Compare(B.Name);
end;

function SortSize(constref A, B: IFileDownload): Integer;
begin
  Result := A.Size - B.Size;
  if Result = 0 then Result := SortName(A, B);
end;

function SortProgress(constref A, B: IFileDownload): Integer;
var
  D: Double;
begin
  D := (B.Progress / B.Size) - (A.Progress / A.Size);
  Result := Round(D * 10000);
  if Result = 0 then Result := SortName(A, B);
end;

function SortEta(constref A, B: IFileDownload): Integer;
begin
  Result := A.Eta - B.Eta;
  if Result = 0 then Result := SortName(A, B);
end;

function SortHealth(constref A, B: IFileDownload): Integer;
begin
  Result := B.Health - A.Health;
  if Result = 0 then Result := SortName(A, B);
end;

function SortKind(constref A, B: IFileDownload): Integer;
begin
  Result := A.Kind.Compare(B.Kind);
  if Result = 0 then Result := SortName(A, B);
end;

function SortUp(constref A, B: IFileDownload): Integer;
begin
  Result := B.UploadSpeed - A.UploadSpeed;
  if Result = 0 then Result := SortName(A, B);
end;

function SortDown(constref A, B: IFileDownload): Integer;
begin
  Result := B.DownloadSpeed - A.DownloadSpeed;
  if Result = 0 then Result := SortName(A, B);
end;

function SortRating(constref A, B: IFileDownload): Integer;
begin
  Result := B.Rating - A.Rating;
  if Result = 0 then Result := SortName(A, B);
end;

function SortSeeds(constref A, B: IFileDownload): Integer;
begin
  Result := A.Seeds - B.Seeds;
  if Result = 0 then Result := SortName(A, B);
end;

function SortPeers(constref A, B: IFileDownload): Integer;
begin
  Result := A.Peers - B.Peers;
  if Result = 0 then Result := SortName(A, B);
end;

function DownloadCompare(Tag: Integer): TFileDownloadCompare;
begin
  case Tag of
    tagName: Result := SortName;
    tagSize: Result := SortSize;
    tagProgress: Result := SortProgress;
    tagEta: Result := SortEta;
    tagHealth: Result := SortHealth;
    tagKind: Result := SortKind;
    tagUp: Result := SortUp;
    tagDown: Result := SortDown;
    tagRating: Result := SortRating;
    tagSeeds: Result := SortSeeds;
    tagPeers: Result := SortPeers;
  else
  end;
end;

function BytesToStr(I: Integer): string;
var
  V: Float;
begin
  if I < 1024 then
    Exit(IntToStr(I) + ' B');
  V := I / 1024;
  if V < 1024 then
    Exit('%.1f KB'.Format([V]));
  V := V / 1024;
  if V < 1024 then
    Exit('%.1f MB'.Format([V]));
  V := V / 1024;
  Result := '%.1f GB'.Format([V]);
end;

function EtaToStr(I: Integer): string;
begin
  if I = 0 then
    Exit('done');
  if I < 60 then
    Exit(IntToStr(I) + ' seconds');
  if I < 3600 then
    Exit(IntToStr(I div 60 ) + 'min ' + IntToStr(I mod 60) + 'sec');
  if I > 3600 * 48 then
    Exit('many days');
  Exit(IntToStr(I div 3600) + 'hr ' + IntToStr((I mod 3600) div 60) + 'min');
end;

function NewDownload: IFileDownload;
begin
  Result := TFileDownload.Create;
end;

end.

