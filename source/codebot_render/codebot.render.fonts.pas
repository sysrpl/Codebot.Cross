unit Codebot.Render.Fonts;

{$i codebot.inc}

interface

uses
  Codebot.System,
  Codebot.Render.Contexts,
  Codebot.Render.Buffers,
  Codebot.Render.Textures;

(*type
  TFontStore = class(TObject)

  end;

  TFont = class(TContextManagedObject)
  private
    FTexture: TTexture;
    FStore: TFontStore;
  public
    constructor Create(const FontName: string = '');
  end;

  TFontCollection = class(TContextCollection)
  private
    FDefaultFont: TFont;
    function GetFont(const ANameFont: string): TFont;
  public
    property Fonts[ANameFont: string]: TFont read GetFont;
    property DefaultFont: TFont read FDefaultFont;
  end;

{ TFontExtension adds the function Fonts to the current context }

  TFontExtension = class helper for TContext
  public
    { Returns the font collection for the current context }
    function Fonts: TFontCollection;
  end;

  TTextBlock = class(TContextManagedObject)
  private
    FVertexBuffer: TObject;
  public
    procedure Draw(const Text: string = '');
    property FontName: string read FFontName write SetFontName;
    property Text: string read FText write SetText;
    property Scale: Float read FScale write FScale;
    property X: Float read FX write FX;
    property Y: Float read FY write FY;
  end; *)

implementation

end.

