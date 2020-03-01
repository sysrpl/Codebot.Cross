unit Codebot.Render.Shaders;

{$i codebot.inc}

interface

uses
  Codebot.System,
  Codebot.Render.Contexts;

{ TShaderObject }

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

{ TShaderSource }

  TShaderSource = class(TShaderObject)
  private
    FCompiled: Boolean;
    FSource: string;
  public
    destructor Destroy; override;
    { Compile the shader adn return the compile status }
    function Compile(Source: string): Boolean;
    { The source code as stored by the compile method }
    property Source: string read FSource;
  end;

{ TVertexShader }

  TVertexShader = class(TShaderSource)
  public
    constructor Create;
  end;

{ TFragmentShader }

  TFragmentShader = class(TShaderSource)
  public
    constructor Create;
  end;

{ TShaderProgram }

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

  TShaderExtension = class helper for TContext
  public
    { Returns the shader collection for the current context }
    function Shaders: TShaderCollection;
  end;

implementation

uses
  Codebot.GLES;

{ TShaderObject }

constructor TShaderObject.Create;
begin
  inherited Create(Ctx.Shaders);
end;

{ TShaderSource }

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
  Item := GetObject(Name);
  if Item <> nil then
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
  Item := GetObject(Name);
  if (Item <> nil) and (Item is TShaderProgram) then
    Result := TShaderProgram(Item)
  else
    Result := nil;
  if Result = nil then
  begin
    S := Ctx.GetAssetFile(PathCombine('shaders', AName));
    Result := TShaderProgram.CreateFromFile(S);
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

