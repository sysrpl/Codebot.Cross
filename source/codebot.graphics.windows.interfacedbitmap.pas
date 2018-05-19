(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified September 2013                             *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.graphics.windows.interfacedbitmap.txt> }
unit Codebot.Graphics.Windows.InterfacedBitmap;

{$i codebot.inc}

interface

{$ifdef windows}
uses
  SysUtils, Classes, Graphics, Windows,
  Codebot.System,
  Codebot.Graphics.Types,
  Codebot.Graphics.Windows.ImageBitmap,
  Codebot.Interop.Windows.ImageCodecs,
  Codebot.Interop.Windows.GdiPlus;

type
  TInterfacedBitmap = class(TInterfacedObject, IBitmap)
  private
    FWidth: Integer;
    FHeight: Integer;
    FFormat: TImageFormat;
  protected
    FBitmap: TFastBitmap;
    FSurface: ISurface;
    function HandleAvailable: Boolean; virtual;
    procedure HandleRelease; virtual;
    procedure Flush;
  public
    destructor Destroy; override;
    function Clone: IBitmap;
    function GetEmpty: Boolean;
    function GetSurface: ISurface; virtual;
    function GetClientRect: TRectI;
    function GetFormat: TImageFormat;
    procedure SetFormat(Value: TImageFormat);
    function GetHeight: Integer;
    function GetWidth: Integer;
    function GetPixels: PPixel; virtual;
    procedure Clear; virtual;
    function Resample(Width, Height: Integer; Quality: TResampleQuality = rqNormal): IBitmap;
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(Stream: TStream);
    procedure SetSize(Width, Height: Integer);
    property Surface: ISurface read GetSurface;
    property Empty: Boolean read GetEmpty;
    property ClientRect: TRectI read GetClientRect;
    property Format: TImageFormat read GetFormat write SetFormat;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property Pixels: PPixel read GetPixels;
  end;

  TInterfacedBitmapClass = class of TInterfacedBitmap;

var
  NewBitmapProc: function: IBitmap;

function NewSplashWin: ISplash;

function Dpi: Integer;
{$endif}

implementation

{$ifdef windows}
destructor TInterfacedBitmap.Destroy;
begin
  HandleRelease;
  inherited Destroy;
end;

procedure TInterfacedBitmap.Flush;
begin
  if FSurface <> nil then
    FSurface.Flush;
end;

function TInterfacedBitmap.HandleAvailable: Boolean;
begin
  if Empty then
    Exit(False);
  if FBitmap.DC = 0 then
    FBitmap := CreateFastBitmap(FWidth, -FHeight, pd32);
  Result := True;
end;

procedure TInterfacedBitmap.HandleRelease;
begin
  FBitmap.Destroy;
end;

function TInterfacedBitmap.Clone: IBitmap;
var
  BitmapClass: TInterfacedBitmapClass;
  B: TInterfacedBitmap;
  S, D: PPixel;
begin
  BitmapClass := TInterfacedBitmapClass(ClassType);
  Result := BitmapClass.Create;
  B := Result as TInterfacedBitmap;
  B.FWidth := FWidth;
  B.FHeight := FHeight;
  B.FFormat := FFormat;
  S := Pixels;
  D := B.Pixels;
  if (S <> nil) and (D <> nil) then
    Move(S^, D^, FWidth * FHeight * PixelSize);
end;

function TInterfacedBitmap.GetEmpty: Boolean;
begin
  Result := (FWidth < 1) or (FHeight < 1);
end;

function TInterfacedBitmap.GetSurface: ISurface;
begin
  Result := FSurface;
end;

function TInterfacedBitmap.GetClientRect: TRectI;
begin
  Result := TRectI.Create(FWidth, FHeight);
end;

function TInterfacedBitmap.GetFormat: TImageFormat;
begin
  Result := FFormat;
end;

procedure TInterfacedBitmap.SetFormat(Value: TImageFormat);
begin
  FFormat := Value;
end;

function TInterfacedBitmap.GetHeight: Integer;
begin
  Result := FHeight;
end;

function TInterfacedBitmap.GetWidth: Integer;
begin
  Result := FWidth;
end;

function TInterfacedBitmap.GetPixels: PPixel;
begin
  if HandleAvailable then
  begin
    Flush;
    Result := FBitmap.Bits;
  end;
end;

procedure TInterfacedBitmap.Clear;
begin
  SetSize(0, 0);
end;

function TInterfacedBitmap.Resample(Width, Height: Integer; Quality: TResampleQuality = rqNormal): IBitmap;
var
  C: TInterfacedBitmapClass;
  B: TInterfacedBitmap;
begin
  if not HandleAvailable then
    Exit(TInterfacedBitmap.Create);
  if (Width = FWidth) or (Height = FHeight) then
    Exit(Clone);
  Flush;
  C := TInterfacedBitmapClass(ClassType);
  Result := C.Create;
  B := Result as TInterfacedBitmap;
  B.FBitmap := BitmapResize(FBitmap, Width, Height, Ord(Quality));
  B.FWidth := B.FBitmap.Width;
  B.FHeight := B.FBitmap.Height;
  B.FFormat := FFormat;
end;

procedure TInterfacedBitmap.LoadFromFile(const FileName: string);
var
  Image: TImageBitmap;
begin
  HandleRelease;
  Image := TImageBitmap.Create;
  try
    Image.LoadFromFile(FileName);
    Image.RequestBitmap(FBitmap, True);
    FWidth := Image.Width;
    FHeight := Image.Height;
    FFormat := StrToImageFormat(Image.Format);
  finally
    Image.Free;
  end;
end;

procedure TInterfacedBitmap.LoadFromStream(Stream: TStream);
var
  Image: TImageBitmap;
begin
  HandleRelease;
  Image := TImageBitmap.Create;
  try
    Image.LoadFromStream(Stream);
    Image.RequestBitmap(FBitmap, True);
    FWidth := Image.Width;
    FHeight := Image.Height;
    FFormat := StrToImageFormat(Image.Format);
  finally
    Image.Free;
  end;
end;

procedure TInterfacedBitmap.SaveToFile(const FileName: string);
var
  Image: TImageBitmap;
begin
  if not HandleAvailable then
    Exit;
  Flush;
  Image := TImageBitmap.Create(FBitmap);
  try
    Image.SaveToFile(FileName);
    Image.RequestBitmap(FBitmap, True);
    FFormat := StrToImageFormat(Image.Format);
  finally
    Image.Free;
  end;
end;

procedure TInterfacedBitmap.SaveToStream(Stream: TStream);
var
  Image: TImageBitmap;
begin
  if not HandleAvailable then
    Exit;
  Flush;
  Image := TImageBitmap.Create(FBitmap);
  try
    Image.Format := ImageFormatToStr(FFormat);
    Image.SaveToStream(Stream);
    Image.RequestBitmap(FBitmap, True);
  finally
    Image.Free;
  end;
end;

procedure TInterfacedBitmap.SetSize(Width, Height: Integer);
begin
  if Width < 1 then
    Width := 0;
  if Height < 1 then
    Height := 0;
  if (Width <> FWidth) or (Height <> FHeight) then
  begin
    HandleRelease;
    FWidth := Width;
    FHeight := Height;
  end;
end;

{ TSplashWin }

type
  TSplashWin = class(TInterfacedObject, ISplash)
  private
    FBitmap: IBitmap;
    FWindow: HWND;
    FOpacity: Byte;
    FVisible: Boolean;
    FX, FY: Integer;
    public
    constructor Create;
    destructor Destroy; override;
    function GetBitmap: IBitmap;
    function GetOpacity: Byte;
    procedure SetOpacity(Value: Byte);
    function GetVisible: Boolean;
    procedure SetVisible(Value: Boolean);
    function GetHandle: IntPtr;
    procedure Move(X, Y: Integer);
    procedure Update;
  end;

function AlphaSplashProc(hwnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
begin
  if uMsg = WM_NCHITTEST then
    Result := HTTRANSPARENT
  else if uMsg = WM_TIMER then
  begin
    { A big FU to Microsoft for changing how HWND_TOPMOST works on Windows 7 and above }
    SetWindowPos(hwnd, HWND_TOPMOST, 0, 0, 0, 0,
    SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE);
    Result := 0;
  end
  else
    Result := DefWindowProc(hwnd, uMsg, wParam, lParam);
end;

constructor TSplashWin.Create;
const
  Registered: Boolean  = False;
    WindowClass = 'AlphaSplashWindow';

  procedure RegisterWindow;
  var
    WndClass: TWndClass;
  begin
    Registered := True;
    FillChar(WndClass, SizeOf(WndClass), #0);
    WndClass.lpfnWndProc := AlphaSplashProc;
    WndClass.hInstance := hInstance;
        WndClass.lpszClassName := WindowClass;
    RegisterClass(WndClass);
  end;

const
  ExStyle = WS_EX_LAYERED or WS_EX_TRANSPARENT or WS_EX_TOPMOST or WS_EX_NOACTIVATE;
begin
  inherited Create;
  FBitmap := NewBitmapProc;
  FOpacity := $FF;
  if not Registered then
    RegisterWindow;
  FWindow := CreateWindowEx(ExStyle, WindowClass, nil, WS_POPUP,
    0, 0, 100, 100, 0, 0, hInstance, nil);
  SetWindowLong(FWindow, GWL_USERDATA, PtrInt(Self));
  SetTimer(FWindow, 1, 1000, nil);
end;

destructor TSplashWin.Destroy;
begin
  KillTimer(FWindow, 1);
  DestroyWindow(FWindow);
  inherited Destroy;
end;

function TSplashWin.GetBitmap: IBitmap;
begin
    Result := FBitmap;
end;

function TSplashWin.GetOpacity: Byte;
begin
  Result := FOpacity;
end;

procedure TSplashWin.SetOpacity(Value: Byte);
begin
    if Value <> FOpacity then
  begin
    FOpacity := Value;
        Update;
  end;
end;

function TSplashWin.GetVisible: Boolean;
begin
    Result := FVisible;
end;

procedure TSplashWin.SetVisible(Value: Boolean);
begin
  Value := Value and (not FBitmap.Empty);
  if Value <> FVisible then
  begin
    FVisible := Value;
    if FVisible then
    begin
      Update;
      SetWindowPos(FWindow, 0, FX, FY, FBitmap.Width, FBitmap.Height,
        SWP_NOACTIVATE or SWP_SHOWWINDOW or SWP_NOZORDER);
    end
    else
        ShowWindow(FWindow, SW_HIDE);
  end;
end;

function TSplashWin.GetHandle: IntPtr;
begin
  Result := IntPtr(FWindow);
end;

procedure TSplashWin.Move(X, Y: Integer);
begin
  FX := X;
  FY := Y;
  SetWindowPos(FWindow, 0, FX, FY, 0, 0,
    SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOZORDER);
end;

function UpdateLayeredWindow(hwd: HWND; hdcDest: HDC; pptDst: PPoint; _psize: PSize;
    hdcSrc: HDC; pptSrc: PPoint; crKey: COLORREF; pblend: PBLENDFUNCTION;
  dwFlags: DWORD): Boolean; stdcall; external user32;

procedure TSplashWin.Update;
var
  Bitmap: TInterfacedBitmap;
    Blend: TBlendFunction;
  Rect: TRect;
    P1, P2: TPoint;
  S: TSize;
  DC: HDC;
begin
  Bitmap := FBitmap as TInterfacedBitmap;
  if not Bitmap.HandleAvailable then
    SetVisible(False)
  else if FVisible then
  begin
    GetWindowRect(FWindow, Rect);
    P1.X := Rect.Left;
    P1.Y := Rect.Top;
    with Blend do
    begin
      BlendOp := AC_SRC_OVER;
      BlendFlags := 0;
      SourceConstantAlpha := FOpacity;
      AlphaFormat := AC_SRC_ALPHA;
    end;
    DC := GetDC(0);
    P2.x := 0;
    P2.y := 0;
    Bitmap.Flush;
    S.cx := Bitmap.Width;
    S.cy := Bitmap.Height;
    UpdateLayeredWindow(FWindow, DC, @P1, @S, Bitmap.FBitmap.DC,
      @P2, 0, @Blend, ULW_ALPHA);
    ReleaseDC(0, DC);
  end;
end;

function NewSplashWin: ISplash;
begin
  Result := TSplashWin.Create;
end;

function Dpi: Integer;
begin
  case GetSystemMetrics(SM_CXICON) of
    32: Result := 96;
    40: Result := 120;
    48: Result := 144;
    64: Result := 192;
  else
    Result := 96;
  end;
end;
{$endif}

end.

