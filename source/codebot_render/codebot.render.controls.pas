(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified July 2022                                  *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.render.controls.txt> }
unit Codebot.Render.Controls;

{$i render.inc}

interface

uses
  Classes, SysUtils, Graphics, Controls, LMessages, LCLType,
  Codebot.GLES;

{ TGraphicsBoxOptions control the options used when creating an OpenGL context.
  Changes to the options are only used once immediately before creation of a
  context and have no effect afterwards. }

type
  TGraphicsBoxOptions = class(TPersistent)
  private
    FDepthBits: Integer;
    FStencilBits: Integer;
    FMultiSampling: Boolean;
    FMultiSamples: Integer;
    procedure SetDepthBits(Value: Integer);
    procedure SetStencilBits(Value: Integer);
    procedure SetMultiSamples(Value: Integer);
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    { DepthBits determines the number of bits used to store pixel Z depth.
      Acceptable values are 16, 24, and 32. }
    property DepthBits: Integer read FDepthBits write SetDepthBits default 24;
    { StencilBits determines the number of bits used to store stencil buffer data.
      Acceptable values are 0, 1, and 8. }
    property StencilBits: Integer read FStencilBits write SetStencilBits default 8;
    { MultiSampling allows polygon edges to be smoothed with anti aliasing }
    property MultiSampling: Boolean read FMultiSampling write FMultiSampling default True;
    { MultiSamples controls how many samples are taken along polygon edges when smoothing.
      Acceptable values are 1, 2, 4, 8 and 16. Higher values greatly effect performance. }
    property MultiSamples: Integer read FMultiSamples write SetMultiSamples default 4;
  end;

{ TGraphicsBox is a windowed control for hosting OpenGL graphics. The
  Context property can be made current inside a thread for realtime
  graphics rendering. Threads should check the context CanRender property
  to determine when to exit. Main forms can check then Rendering property
  to wait before closing. }

  TGraphicsBox = class(TWinControl)
  private
    FCanvas: TControlCanvas;
    FContext: IOpenGLContext;
    FLogo: TBitmap;
    FRendering: Boolean;
    FFailed: Boolean;
    FOptions: TGraphicsBoxOptions;
    FOnFailed: TNotifyEvent;
    FOnRenderStart: TNotifyEvent;
    FOnRenderStop: TNotifyEvent;
    function CanRender: Boolean;
    function GetContext: IOpenGLContext;
    procedure TryRenderStart;
    procedure SetOptions(Value: TGraphicsBoxOptions);
    procedure WMPaint(var Message: TLMPaint); message LM_PAINT;
  protected
    class procedure WSRegisterClass; override;
    procedure DestroyWnd; override;
    procedure PaintWindow(DC: HDC); override;
    procedure Paint; virtual;
    property Canvas: TControlCanvas read FCanvas;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground(DC: HDC); override;
    { Context is only valid between OnRenderStart and OnRenderStop.
    	Context can be used in threads, but can be current in only
      one thread at a time. A good strategy is to spawn a rendering thread
      in response to OnRenderStart, and exiting that thread in response
      to OnRenderStop. }
    property Context: IOpenGLContext read GetContext;
    { Rendering is True only while a context is allowed to render.
    	That is the property is True between OnRenderStart and OnRenderStop.

      OnRenderStart occurs immediately after a window is first shown.
      OnRenderStop occurs immediately before a window is destroyed. }
    property Rendering: Boolean read FRendering;
    { Failed is True when a context failed the creation step.
    	Failure is caused by unsupported options and is distinctly
      different from OpenGLInfo.IsValid. }
    property Failed: Boolean read FFailed;
	published
    { Options are used once immediately before a context is created }
    property Options: TGraphicsBoxOptions read FOptions write SetOptions;
    { OnFailed fires after a context failed the creation step }
    property OnFailed: TNotifyEvent read FOnFailed write FOnFailed;
    { OnRenderStart fires after a context is created }
    property OnRenderStart: TNotifyEvent read FOnRenderStart write FOnRenderStart;
    { OnRenderStop fires before a context is destroyed }
    property OnRenderStop: TNotifyEvent read FOnRenderStop write FOnRenderStop;
  end;

implementation

{$r opengl.res}

uses
  WSLCLClasses,
  {$ifdef gtk2gl}
  Codebot.Render.Controls.Gtk2;
  {$endif}
  {$ifdef gtk3gl}
  Codebot.Render.Controls.Gtk3;
  {$endif}
  {$ifdef win32gl}
  Codebot.Render.Controls.Windows;
  {$endif}

{ TGraphicsBoxOptions }

constructor TGraphicsBoxOptions.Create;
begin
  inherited Create;
  FDepthBits := 24;
  FStencilBits := 8;
  FMultiSampling := True;
  FMultiSamples := 4;
end;

procedure TGraphicsBoxOptions.Assign(Source: TPersistent);
var
  O: TGraphicsBoxOptions absolute Source;
begin
  if Source is TGraphicsBoxOptions then
  begin
    FDepthBits := O.FDepthBits;
    FStencilBits := O.FStencilBits;
    FMultiSampling := O.FMultiSampling;
    FMultiSamples := O.FMultiSamples;
  end
  else
    inherited Assign(Source);
end;

procedure TGraphicsBoxOptions.SetDepthBits(Value: Integer);
begin
  if Value < 24 then
    FDepthBits := 16
  else if Value < 32 then
    FDepthBits := 24
  else
    FDepthBits := 32;
end;

procedure TGraphicsBoxOptions.SetStencilBits(Value: Integer);
begin
  if Value < 1 then
    FStencilBits := 0
  else if Value < 8 then
    FStencilBits := 1
  else
    FStencilBits := 8;
end;

procedure TGraphicsBoxOptions.SetMultiSamples(Value: Integer);
begin
  if Value < 2 then
    FMultiSamples := 1
  else if Value < 4 then
    FMultiSamples := 2
  else if Value < 8 then
    FMultiSamples := 4
  else if Value < 16 then
    FMultiSamples := 8
  else
    FMultiSamples := 16;
end;

{ TGraphicsBox }

constructor TGraphicsBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptions := TGraphicsBoxOptions.Create;
  ControlStyle := ControlStyle - [csSetCaption];
  DoubleBuffered := False;
	ParentDoubleBuffered := False;
  SetInitialBounds(0, 0, 400, 300);
end;

destructor TGraphicsBox.Destroy;
begin
  FContext := nil;
  FLogo.Free;
  FOptions.Free;
  inherited Destroy;
end;

procedure TGraphicsBox.DestroyWnd;
begin
  FreeAndNil(FCanvas);
  if FRendering then
  begin
    if FContext <> nil then
      FContext.CanRender := False;
    try
      if Assigned(FOnRenderStop) then
        FOnRenderStop(Self);
    finally
      FRendering := False;
      FContext := nil;
    end;
  end;
  inherited DestroyWnd;
end;

var
  Registered: Boolean;

class procedure TGraphicsBox.WSRegisterClass;
begin
  if Registered then
    Exit;
  Registered := True;
  RegisterWSComponent(TGraphicsBox, TWSOpenGLWindow)
end;

procedure TGraphicsBox.TryRenderStart;
begin
  if not CanRender then
  	Exit;
  if Context = nil then
  begin
    if Assigned(FOnFailed) then
    	FOnFailed(Self);
  end;
  if not FRendering then
  begin
    FRendering := True;
    Context.CanRender := True;
    if Assigned(FOnRenderStart) then
      FOnRenderStart(Self);
  end
end;

procedure TGraphicsBox.EraseBackground(DC: HDC);
begin
  TryRenderStart;
  if not CanRender then
    inherited EraseBackground(DC);
end;

procedure TGraphicsBox.WMPaint(var Message: TLMPaint);
begin
  if (csDestroying in ComponentState) or (not HandleAllocated) then
    Exit;
  Include(FControlState, csCustomPaint);
  inherited WMPaint(Message);
  Exclude(FControlState, csCustomPaint);
end;

procedure TGraphicsBox.PaintWindow(DC: HDC);
var
  Changed: Boolean;
begin
  TryRenderStart;
  if not CanRender then
  begin
    if FCanvas = nil then
    begin
      FCanvas := TControlCanvas.Create;
      FCanvas.Control := Self;
    end;
    Changed := (not FCanvas.HandleAllocated) or (FCanvas.Handle <> DC);
    if Changed then
      FCanvas.Handle := DC;
    Paint;
    if Changed then
      FCanvas.Handle := 0;
  end;
end;

procedure TGraphicsBox.Paint;

  procedure Colorize;
  var
    Color: TColor;
    W, H, X, Y: Integer;
    Source, Dest: PByte;
    A: Single;
  begin
    if FLogo.PixelFormat <> pf32bit then
      Exit;
    W := FLogo.Width;
    H := FLogo.Height;
    if (W < 1) or (H < 1) then
      Exit;
    Color := clWhite;
    Source := @Color;
    FLogo.BeginUpdate;
    for Y := 0 to H - 1 do
    begin
      Dest := FLogo.RawImage.GetLineStart(Y);
      for X := 0 to W - 1 do
      begin
        A := Dest[3] / 255;
        Dest^ := Trunc(Source[2] * A);
        Inc(Dest);
        Dest^ := Trunc(Source[1] * A);
        Inc(Dest);
        Dest^ := Trunc(Source[0] * A);
        Inc(Dest);
        Inc(Dest);
      end;
    end;
    FLogo.EndUpdate;
  end;

  procedure LoadBitmap;
  var
    P: TPicture;
  begin
    if FLogo <> nil then
      Exit;
    P := TPicture.Create;
    try
      P.LoadFromResourceName(Hinstance, 'opengl.png');
      FLogo := TBitmap.Create;
      FLogo.Assign(P.Bitmap);
    finally
      P.Free;
    end;
    Colorize;
  end;

var
  S: string;
  H, X, Y: Integer;
begin
  Canvas.Brush.Color := 0;
  Canvas.Pen.Color := clWhite;
  Canvas.Pen.Style := psDash;
  Canvas.Rectangle(ClientRect);
  LoadBitmap;
  Canvas.Draw((Width - FLogo.Width) shr 1, (Height - FLogo.Height) shr 1, FLogo);
  if csDesigning in ComponentState then
  	Exit;
  Canvas.Font.Color := clWhite;
  H := Canvas.TextHeight('Wg');
  X := 5;
  Y := 5;
  if FFailed then
    Canvas.TextOut(X, Y, 'Options failed to create a context')
  else
	begin
    S := 'Your video driver does not support ' + glapi;
    Canvas.TextOut(5, Y, S);
    Inc(Y, H);
    Canvas.TextOut(5, Y, 'Try rebuilding this program with -dgles2');
    Inc(Y, H * 2);
    Canvas.TextOut(5, Y, 'Renderer: ' + OpenGLInfo.Renderer);
    Inc(Y, H);
    Canvas.TextOut(5, Y, 'Version: ' + OpenGLInfo.Version);
  end;
end;

function TGraphicsBox.CanRender: Boolean;
begin
  Result := (not (csDesigning in ComponentState)) and OpenGLInfo.IsValid and (not Failed);
end;

function TGraphicsBox.GetContext: IOpenGLContext;
var
  Params: TOpenGLParams;
begin
  if CanRender and FRendering and (FContext = nil) then
  begin
    Params := TOpenGLParams.Create;
    Params.Depth := FOptions.DepthBits;
    Params.Stencil := FOptions.StencilBits;
    Params.MultiSampling := FOptions.MultiSampling;
    Params.MultiSamples := FOptions.MultiSamples;
    FContext := TWSOpenGLWindow.CreateContext(Self, Params);
    FFailed := FContext = nil;
  end;
  Result := FContext;
end;

procedure TGraphicsBox.SetOptions(Value: TGraphicsBoxOptions);
begin
  FOptions.Assign(Value);
end;

end.

