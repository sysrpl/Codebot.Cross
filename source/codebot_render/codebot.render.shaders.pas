unit Codebot.Render.Shaders;

{$i render.inc}

interface

uses
  Codebot.System,
  Codebot.Render.Contexts,
  Classes;

{ TShaderObject is the base class of shaders and shader programs }

type
  TShaderObject = class(TContextManagedObject)
  private
    FHandle: Integer;
    FValid: Boolean;
    FErrorObject: TShaderObject;
    FErrorString: string;
  public
    constructor Create;
    { ErrorString contains the error if any generated when a shader is compiled
      or linked }
    property ErrorString: string read FErrorString;
    { ErrorObject referrs to the shader or program generating the error }
    property ErrorObject: TShaderObject read FErrorObject;
    { Valid is true if the shader is compiled and linked }
    property Valid: Boolean read FValid;
    { The underlying handle of the object }
    property Handle: Integer read FHandle;
  end;

{ TShaderSource is a shader compiled from source }

  TShaderSource = class(TShaderObject)
  private
    FCompiled: Boolean;
    FSource: string;
  public
    destructor Destroy; override;
    { Compile the shader and return the compile status. If the source does not
      begin with a #version directive one is added to match the OpenGL API
      selected in render.inc, so the same source can target desktop and ES. }
    function Compile(Source: string): Boolean;
    { The source code as stored by the compile method }
    property Source: string read FSource;
  end;

{ TVertexShader is a shader which places vertices }

  TVertexShader = class(TShaderSource)
  public
    constructor Create;
  end;

{ TFragmentShader is a shader which colors pixels }

  TFragmentShader = class(TShaderSource)
  public
    constructor Create;
  end;

{ TShaderProgram is vertex and fragment shaders linked together for drawing }

  TShaderProgram = class(TShaderObject)
  private
    FAttachCount: Integer;
    FLinked: Boolean;
    function GetActive: Boolean;
    procedure SetActive(Value: Boolean);
  public
    { Create an empty shader program with nothing attached }
    constructor Create;
    { Create a shader given a vertex and fragement source }
    constructor CreateFromSource(const VertSource, FragSource: string);
    { Create a shader program from a resource or asset }
    constructor CreateFromAsset(const Name: string); overload;
    { Create a shader program with two files ending in .vert and .frag }
    constructor CreateFromFile(const ProgramName: string); overload;
    { Create a shader program two specific vert and frag files }
    constructor CreateFromFile(const VertFileName, FragFileName: string); overload;
    destructor Destroy; override;
    { Add the shader to the program stack and activate it }
    procedure Push;
    { Remove the shader to from the program stack and deactivate it }
    procedure Pop;
    { Attach a vertex or fragment source }
    procedure Attach(Source: TShaderSource);
    { Perform linking of attached sources returning true if there were no errors }
    function Link: Boolean;
    { Update the modelview and projection matrix uniforms for this program }
    procedure UpdateMatrix;
    { Active is the same as pushing or popping }
    property Active: Boolean read GetActive write SetActive;
  end;

{ TShaderCollection holds a collection of shaders by name. If you create a
  shader without a name, then its life will still be managed by this collection. }

  TShaderCollection = class(TContextCollection)
  private
    function GetSource(const AName: string): TShaderSource;
    function GetProg(const AName: string): TShaderProgram;
  public
    constructor Create;
    { Return a shader source object by name or locate and create the shader from an asset }
    property Source[AName: string]: TShaderSource read GetSource;
    { Return a shader program by name or locate and create the program from an asset }
    property Prog[AName: string]: TShaderProgram read GetProg; default;
  end;

{ TShaderExtension adds the function Shaders to the current context }

  TShaderExtension = class helper for TRenderContext
  public
    { Returns the shader collection for the current context }
    function Shaders: TShaderCollection;
  end;

{ ShadowLighting is GLSL source for a fragment shader which reads a shadow
  map recorded by a TShadowBuffer. It defines the function

    float shadowLit(sampler2DShadow map, vec4 c)

  which returns how lit a point is from 0 to 1, where c is the point in the
  clip space of the light. Points outside of the map are lit. Put it before
  the uniforms of the shader, as on OpenGL ES it also gives sampler2DShadow
  the precision it needs.

  The map is filtered, so each lookup compares four texels and blends them.
  The shadow is softened by nine lookups a texel apart, except on the
  Raspberry Pi, which has too little fill rate for that at 1080p and uses
  four lookups half a texel apart, covering the same area with a little less
  softening. }

const
{$if defined(linux) and (defined(cpuarm) or defined(cpuaarch64))}
  ShadowPrecision = 'precision highp sampler2DShadow;'#10;
  ShadowSamples =
    '  for (int x = 0; x < 2; x++)'#10 +
    '    for (int y = 0; y < 2; y++)'#10 +
    '      lit += texture(map, vec3(p.xy + (vec2(x, y) - 0.5) * texel, p.z));'#10 +
    '  return lit / 4.0;'#10;
{$else}
  ShadowPrecision = '';
  ShadowSamples =
    '  for (int x = -1; x <= 1; x++)'#10 +
    '    for (int y = -1; y <= 1; y++)'#10 +
    '      lit += texture(map, vec3(p.xy + vec2(x, y) * texel, p.z));'#10 +
    '  return lit / 9.0;'#10;
{$endif}
  ShadowLighting =
    ShadowPrecision +
    'float shadowLit(sampler2DShadow map, vec4 c) {'#10 +
    '  vec3 p = c.xyz / c.w * 0.5 + 0.5;'#10 +
    '  if (p.x < 0.0 || p.x > 1.0 || p.y < 0.0 || p.y > 1.0 || p.z > 1.0)'#10 +
    '    return 1.0;'#10 +
    '  vec2 texel = 1.0 / vec2(textureSize(map, 0));'#10 +
    '  float lit = 0.0;'#10 +
    ShadowSamples +
    '}'#10;

implementation

uses
  Codebot.OpenGL;

{ TShaderObject }

constructor TShaderObject.Create;
begin
  inherited Create(Ctx.Shaders);
end;

{ TShaderSource }

{ The version header added to shader sources which do not declare a version.
  Shaders written in GLSL 3.30 core syntax using in, out, and layout locations
  compile unchanged on desktop OpenGL 3.3 and later and on OpenGL ES 3.0. }

const
{$if defined(gles30)}
  ShaderHeader = '#version 300 es'#10'precision highp float;'#10;
{$elseif defined(glesapi)}
  ShaderHeader = '#version 100'#10'precision mediump float;'#10;
{$elseif defined(gl33)}
  ShaderHeader = '#version 330 core'#10;
{$else}
  ShaderHeader = '#version 140'#10'#extension GL_ARB_explicit_attrib_location : require'#10;
{$endif}

destructor TShaderSource.Destroy;
begin
  inherited Destroy;
  glDeleteShader(FHandle);
end;

function TShaderSource.Compile(Source: string): boolean;
var
  S: string;
  P: PChar;
  I: Integer;
begin
  if FCompiled then
    Exit(False);
  if Source.IsWhitespace then
    Exit(False);
  FCompiled := True;
  FSource := Source;
  S := Source;
  if not S.Trim.BeginsWith('#version') then
    S := ShaderHeader + S;
  P := PChar(S);
  glShaderSource(FHandle, 1, @P, nil);
  glCompileShader(FHandle);
  glGetShaderiv(FHandle, GL_COMPILE_STATUS, @I);
  FValid := I = GL_TRUE;
  if not FValid then
  begin
    glGetShaderiv(FHandle, GL_INFO_LOG_LENGTH, @I);
    if I > 0 then
    begin
      SetLength(S, I);
      glGetShaderInfoLog(FHandle, I, @I, PChar(S));
    end
    else
      S := 'Unkown error';
    FErrorObject := Self;
    FErrorString := S;
  end;
  Result := FValid;
end;

{ TVertexShader }

constructor TVertexShader.Create;
begin
  inherited Create;
  FHandle := glCreateShader(GL_VERTEX_SHADER);
end;

{ TFragmentShader }

constructor TFragmentShader.Create;
begin
  inherited Create;
  FHandle := glCreateShader(GL_FRAGMENT_SHADER);
end;

constructor TShaderProgram.Create;
begin
  inherited Create;
  FHandle := glCreateProgram;
end;

destructor TShaderProgram.Destroy;
begin
  glDeleteProgram(FHandle);
  inherited Destroy;
end;

constructor TShaderProgram.CreateFromSource(const VertSource, FragSource: string);
var
  V, F: TShaderSource;
begin
  Create;
  V := TVertexShader.Create;
  F := TFragmentShader.Create;
  try
    V.Compile(VertSource);
    F.Compile(FragSource);
    Attach(V);
    Attach(F);
    Link;
  finally
    V.Free;
    F.Free;
  end;
end;

constructor TShaderProgram.CreateFromAsset(const Name: string);
var
  V, F: string;
  S: TStream;
begin
  S := Ctx.GetAssetStream(Name + '.vert');
  try
    V := StreamReadStr(S);
  finally
    S.Free;
  end;
  S := Ctx.GetAssetStream(Name + '.frag');
  try
    F := StreamReadStr(S);
  finally
    S.Free;
  end;
  CreateFromSource(V, F);
end;

constructor TShaderProgram.CreateFromFile(const ProgramName: string);
begin
  CreateFromFile(ProgramName + '.vert', ProgramName + '.frag');
end;

constructor TShaderProgram.CreateFromFile(const VertFileName, FragFileName: string);
var
  V, F: string;
begin
  V := VertFileName;
  F := FragFileName;
  if not FileExists(V) then
    V := Ctx.GetAssetFile(V);
  if not FileExists(F) then
    F := Ctx.GetAssetFile(F);
  CreateFromSource(FileReadStr(V), FileReadStr(F));
end;

procedure TShaderProgram.Push;
begin
  if Valid then
    Ctx.PushProgram(FHandle);
end;

procedure TShaderProgram.Pop;
begin
  if Valid then
    Ctx.PopProgram;
end;

function TShaderProgram.GetActive: boolean;
begin
  Result := Valid and (Ctx.GetProgram = FHandle);
end;

procedure TShaderProgram.SetActive(Value: boolean);
begin
  if Value <> GetActive then
    if Value then
      Push
    else
      Pop;
end;

procedure TShaderProgram.UpdateMatrix;
begin
  if Active then
    Ctx.SetProgramMatrix;
end;

procedure TShaderProgram.Attach(Source: TShaderSource);
begin
  if FLinked then
    Exit;
  if Source.Valid then
  begin
    Inc(FAttachCount);
    glAttachShader(FHandle, Source.FHandle);
  end
  else
  begin
    FLinked := True;
    FValid := False;
    FErrorObject := Source;
    FErrorString := Source.ClassName + ' invalid - ' + Source.ErrorString;
  end;
end;

function TShaderProgram.Link: boolean;
var
  I: Integer;
  S: string;
begin
  if Flinked then
    Exit(False);
  if FAttachCount < 2 then
    Exit(False);
  FLinked := True;
  glLinkProgram(FHandle);
  glGetProgramiv(FHandle, GL_LINK_STATUS, @I);
  FValid := I = GL_TRUE;
  if not FValid then
  begin
    FErrorObject := Self;
    glGetProgramiv(FHandle, GL_INFO_LOG_LENGTH, @I);
    if I > 0 then
    begin
      S := '';
      SetLength(S, I);
      glGetProgramInfoLog(FHandle, I, @I, PChar(S));
    end
    else
      S := 'Unknown error';
    FErrorString := S;
  end;
  Result := FValid;
end;

{ TShaderCollection }

const
  SShaderCollection = 'shaders';

constructor TShaderCollection.Create;
begin
  inherited Create(SShaderCollection);
end;

function TShaderCollection.GetSource(const AName: string): TShaderSource;
var
  Item: TContextManagedObject;
  S: string;
begin
  Item := GetObject(AName);
  if (Item <> nil) and (Item is TShaderSource) then
    Result := TShaderSource(Item)
  else
    Result := nil;
  if Result = nil then
  begin
    S := Ctx.GetAssetFile(PathCombine('shaders', AName));
    if AName.EndsWith('.vert') then
      Result := TVertexShader.Create
    else if AName.EndsWith('.frag') then
      Result := TFragmentShader.Create
    else
      raise EContextAssetError.Create(SAssetNotUnderstood);
    Result.Compile(FileReadStr(S));
    Result.Name := AName;
  end;
end;

function TShaderCollection.GetProg(const AName: string): TShaderProgram;
var
  Item: TContextManagedObject;
  S: string;
begin
  Item := GetObject(AName);
  if (Item <> nil) and (Item is TShaderProgram) then
    Result := TShaderProgram(Item)
  else
    Result := nil;
  if Result = nil then
  begin
    { A program is a pair of files named after the program ending in .vert
      and .frag }
    S := Ctx.GetAssetFile(PathCombine('shaders', AName + '.vert'));
    Result := TShaderProgram.CreateFromFile(FileChangeExt(S, ''));
    Result.Name := AName;
  end;
end;

{ TShaderExtension }

function TShaderExtension.Shaders: TShaderCollection;
begin
  Result := TShaderCollection(GetCollection(SShaderCollection));
  if Result = nil then
    Result := TShaderCollection.Create;
end;

end.

