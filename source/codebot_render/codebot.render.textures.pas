unit Codebot.Render.Textures;

{$i render.inc}
{$pointermath on}

interface

uses
  SysUtils, Classes,
  Codebot.System,
  Codebot.Platform,
  Codebot.Graphics.Types,
  Codebot.Geometry,
  Codebot.Render.Contexts;

{ TTexFilter is how a texture is sampled between its pixels }

type
  TTexFilter = (tfNearest, tfLinear);

  { TTexture is an OpenGL texture owned by a render context }
  TTexture = class(TContextManagedObject)
  private
    FHandle: Integer;
    FWidth: Integer;
    FHeight: Integer;
    FMagFilter: TTexFilter;
    FMinFilter: TTexFilter;
    FWrap: Boolean;
    FMipmaps: Boolean;
    procedure ApplyFilters;
    procedure ApplyWrap;
    function GetActive: Boolean;
    procedure SetActive(Value: Boolean);
    procedure SetMagFilter(Value: TTexFilter);
    procedure SetMinFilter(Value: TTexFilter);
    procedure SetWrap(Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    { Generate mipmaps for the texture. Until the texture is loaded again the
      minify filter samples from the mipmaps. }
    procedure GenerateMipmaps;
    { Load a texture from straight alpha RGBA pixels, four bytes per pixel,
      with the first row at the top of the image }
    procedure LoadFromData(Width, Height: Integer; Pixels: Pointer);
    { Load a texture from a bitmap. Bitmap pixels are converted from
      premultiplied BGRA to the straight alpha RGBA used by the render
      context blend function. }
    procedure LoadFromBitmap(Bitmap: IBitmapData);
    { Load a texture from a stream }
    procedure LoadFromStream(Stream: TStream);
    { Load a texture from a file }
    procedure LoadFromFile(const FileName: string);
    { Load a texture from an asset of the program, which is in its dat file
      or is a file in its assets folder. Name is the path of the image below
      the assets folder, such as 'cards/back.png'. }
    procedure LoadFromAsset(const Name: string);
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
    { Minify filter, which also blends between mipmaps when they exist }
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
    { Return a texture by name or locate and create the texture from an asset }
    property Texture[AName: string]: TTexture read GetTexture; default;
  end;

{ TTextureExtension adds the function Textures to the current context }

  TTextureExtension = class helper for TRenderContext
  public
    { Returns the shader collection for the current context }
    function Textures: TTextureCollection;
  end;

implementation

uses
  Codebot.OpenGL;

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

procedure TTexture.ApplyFilters;
begin
  if FMagFilter = tfLinear then
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
  else
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  if FMipmaps then
    if FMinFilter = tfLinear then
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR)
    else
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST_MIPMAP_NEAREST)
  else if FMinFilter = tfLinear then
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
  else
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
end;

procedure TTexture.ApplyWrap;
begin
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
end;

procedure TTexture.GenerateMipmaps;
begin
  if FWidth = 0 then
    Exit;
  Push;
  glGenerateMipmap(GL_TEXTURE_2D);
  FMipmaps := True;
  ApplyFilters;
  Pop;
end;

procedure TTexture.LoadFromData(Width, Height: Integer; Pixels: Pointer);
begin
  FWidth := Width;
  FHeight := Height;
  FMipmaps := False;
  Push;
  ApplyFilters;
  ApplyWrap;
  { Rows of any width are tightly packed }
  glPixelStorei(GL_UNPACK_ALIGNMENT, 4);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, Width, Height, 0, GL_RGBA,
    GL_UNSIGNED_BYTE, Pixels);
  Pop;
end;

procedure TTexture.LoadFromBitmap(Bitmap: IBitmapData);
var
  Data: array of Byte;
  Source: PPixel;
  Dest: PByte;
  A, I: Integer;
begin
  if (Bitmap.Width < 1) or (Bitmap.Height < 1) then
  begin
    LoadFromData(0, 0, nil);
    Exit;
  end;
  SetLength(Data, Bitmap.Width * Bitmap.Height * 4);
  Source := Bitmap.Pixels;
  Dest := @Data[0];
  for I := 0 to Bitmap.Width * Bitmap.Height - 1 do
  begin
    A := Source.Alpha;
    if A = 0 then
    begin
      Dest[0] := 0;
      Dest[1] := 0;
      Dest[2] := 0;
    end
    else if A = $FF then
    begin
      Dest[0] := Source.Red;
      Dest[1] := Source.Green;
      Dest[2] := Source.Blue;
    end
    else
    begin
      Dest[0] := (Source.Red * $FF + A div 2) div A;
      Dest[1] := (Source.Green * $FF + A div 2) div A;
      Dest[2] := (Source.Blue * $FF + A div 2) div A;
    end;
    Dest[3] := A;
    Inc(Source);
    Inc(Dest, 4);
  end;
  LoadFromData(Bitmap.Width, Bitmap.Height, @Data[0]);
end;

procedure TTexture.LoadFromStream(Stream: TStream);
var
  B: IBitmapData;
begin
  B := NewBitmapData;
  B.LoadFromStream(Stream);
  LoadFromBitmap(B);
end;

procedure TTexture.LoadFromFile(const FileName: string);
var
  B: IBitmapData;
begin
  B := NewBitmapData;
  B.LoadFromFile(FileName);
  LoadFromBitmap(B);
end;

procedure TTexture.LoadFromAsset(const Name: string);
var
  S: TStream;
begin
  S := Ctx.GetAssetStream(Name);
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
  ApplyFilters;
  Pop;
end;

procedure TTexture.SetMinFilter(Value: TTexFilter);
begin
  if FMinFilter = Value then Exit;
  FMinFilter := Value;
  if FWidth = 0 then
    Exit;
  Push;
  ApplyFilters;
  Pop;
end;

procedure TTexture.SetWrap(Value: Boolean);
begin
  if FWrap = Value then Exit;
  FWrap := Value;
  if FWidth = 0 then
    Exit;
  Push;
  ApplyWrap;
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
  S: TStream;
begin
  Item := GetObject(AName);
  if (Item <> nil) and (Item is TTexture) then
    Result := TTexture(Item)
  else
    Result := nil;
  if Result = nil then
  begin
    { The asset is found before the texture is made, so a name which is not
      found leaves no texture behind }
    S := Ctx.GetAssetStream(PathCombine('textures', AName));
    try
      Result := TTexture.Create;
      Result.Name := AName;
      Result.LoadFromStream(S);
    finally
      S.Free;
    end;
  end;
end;

{ TTextureExtension }

function TTextureExtension.Textures: TTextureCollection;
begin
  Result := TTextureCollection(GetCollection(STextureCollection));
  if Result = nil then
    Result := TTextureCollection.Create;
end;

end.

