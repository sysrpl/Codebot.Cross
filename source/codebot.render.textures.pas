unit Codebot.Render.Textures;

{$mode delphi}

interface

uses
  SysUtils, Classes,
  Codebot.System,
  Codebot.Geometry,
  Codebot.Render.Contexts;

{ TTexture }

type
  TTexFilter = (tfNearest, tfLinear);

  TTexture = class(TContextManagedObject)
  private
    FHandle: Integer;
    FWidth: Integer;
    FHeight: Integer;
    FMagFilter: TTexFilter;
    FMinFilter: TTexFilter;
    FWrap: Boolean;
    { TODO: Consider adding mipmap property }
    function GetActive: Boolean;
    procedure SetActive(Value: Boolean);
    procedure SetMagFilter(Value: TTexFilter);
    procedure SetMinFilter(Value: TTexFilter);
    procedure SetWrap(Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    { Generate mipmaps for the texture }
    procedure GenerateMipmaps;
    { Load a texture from a stream }
    procedure LoadFromStream(Stream: TStream);
    { Load a texture from a file }
    procedure LoadFromFile(const FileName: string);
    { Output texture coords given x and y pixels }
    procedure Coord(X, Y: Integer; out V: TVec2);
    { Make the texture current optionally at a texture slot }
    procedure Push(Slot: Integer = 0);
    { Restore the previous texture }
    procedure Pop;
    { The width of the texture }
    property Width: Integer read FWidth;
    { The height of the texture }
    property Height: Integer read FHeight;
    { Flag to turn the texture on or off }
    property Active: Boolean read GetActive write SetActive;
    { Maginify filter }
    property MagFilter: TTexFilter read FMagFilter write SetMagFilter;
    { Minify filter }
    property MinFilter: TTexFilter read FMinFilter write SetMinFilter;
    { Texture wrapping }
    property Wrap: Boolean read FWrap write SetWrap;
    { The underlying handle of the texture }
    property Handle: Integer read FHandle;
  end;

{ TTextureCollection holds a collection of textures by name. If you create a
  texture without a name, then its life will still be managed by this collection. }

  TTextureCollection = class(TContextCollection)
  private
    function GetTexture(const AName: string): TTexture;
  public
    constructor Create;
    { Return a texture by name or nil if not found }
    property Texture[AName: string]: TTexture read GetTexture;
  end;

{ TTextureExtension adds the function Textures to the current context }

  TTextureExtension = class helper for TContext
  public
    { Returns the shader collection for the current context }
    function Textures: TTextureCollection;
  end;

implementation

uses
  Codebot.GLES,
  Codebot.Graphics,
  Codebot.Graphics.Types;

constructor TTexture.Create;
begin
  inherited Create(Ctx.Textures);
  glGenTextures(1, @FHandle);
end;

destructor TTexture.Destroy;
begin
  glDeleteTextures(1, @FHandle);
  inherited Destroy;
end;

procedure TTexture.GenerateMipmaps;
begin
  Push;
  glGenerateMipmap(GL_TEXTURE_2D);
  Pop;
end;

procedure TTexture.LoadFromStream(Stream: TStream);
var
  B: IBitmap;
begin
  B := NewBitmap;
  B.LoadFromStream(Stream);
  FWidth := B.Width;
  FHeight := B.Height;
  Push;
  if FMagFilter = tfLinear then
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
  else
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  if FMinFilter = tfLinear then
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
  else
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  if FWrap then
  begin
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  end
  else
  begin
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  end;
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, Width, Height, 0, GL_RGBA,
    GL_UNSIGNED_BYTE, B.Pixels);
  Pop;
end;

procedure TTexture.LoadFromFile(const FileName: string);
var
  S: TStream;
begin
  S := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(S);
  finally
    S.Free;
  end;
end;

procedure TTexture.Coord(X, Y: Integer; out V: TVec2);
begin
  if FWidth < 1 then
  begin
    V.X := 0;
    V.Y := 0;
  end
  else
  begin
    V.X := X / FWidth;
    V.Y := Y / FHeight;
  end;
end;

procedure TTexture.Push(Slot: Integer = 0);
begin
  Ctx.PushTexture(Handle, Slot);
end;

procedure TTexture.Pop;
begin
  Ctx.PopTexture;
end;

function TTexture.GetActive: Boolean;
begin
  Result := Ctx.GetTexture = FHandle;
end;

procedure TTexture.SetActive(Value: Boolean);
begin
  if Value <> GetActive then
    if Value then
      Push
    else
      Pop;
end;

procedure TTexture.SetMagFilter(Value: TTexFilter);
begin
  if FMagFilter = Value then Exit;
  FMagFilter := Value;
  if FWidth = 0 then
    Exit;
  Push;
  if FMagFilter = tfLinear then
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
  else
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  Pop;
end;

procedure TTexture.SetMinFilter(Value: TTexFilter);
begin
  if FMinFilter = Value then Exit;
  FMinFilter := Value;
  if FWidth = 0 then
    Exit;
  Push;
  if FMinFilter = tfLinear then
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
  else
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  Pop;
end;

procedure TTexture.SetWrap(Value: Boolean);
begin
  if FWrap = Value then Exit;
  FWrap := Value;
  if FWidth = 0 then
    Exit;
  Push;
  if FWrap then
  begin
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  end
  else
  begin
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  end;
  Pop;
end;

{ TTextureCollection }

const
  STextureCollection = 'textures';

constructor TTextureCollection.Create;
begin
  inherited Create(STextureCollection);
end;

function TTextureCollection.GetTexture(const AName: string): TTexture;
var
  Item: TContextManagedObject;
begin
  Item := GetObject(Name);
  if Item <> nil then
    Result := TTexture(Item)
  else
    Result := nil;
end;

{ TTextureExtension }

function TTextureExtension.Textures: TTextureCollection;
begin
  Result := TTextureCollection(GetCollection(STextureCollection));
  if Result = nil then
    Result := TTextureCollection.Create;
end;

end.

