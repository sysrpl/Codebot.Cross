(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ Codebot.Platform.SDL provides the Codebot.Platform interfaces using SDL2.
  Bitmaps are loaded and saved with SDL_image, so programs using this unit need
  the SDL2_image library. Message dialogs are SDL message boxes. SDL has no
  file dialogs, so file and picture dialogs are the ones made from widgets in
  Codebot.Render.Widgets.Dialogs, shown in a widget scene. It takes the place of
  Codebot.Platform.LCL in programs which use SDL windows.

  SDL is used on the thread which runs the SDL window.

  The platform routines are assigned when this unit is initialized. If a
  program also uses Codebot.Platform.LCL, the unit initialized last decides
  which backend is used. }

unit Codebot.Platform.SDL;

{$i ../codebot_render/render.inc}

interface

uses
  SysUtils, Classes,
  Codebot.Platform,
  Codebot.Render.Widgets.Dialogs,
  Codebot.Interop.SDL2;

{ TBitmapDataSDL holds the pixels of a bitmap in memory. Pixels are stored as
  described by IBitmapData, premultiplied blue, green, red, and alpha with rows
  from top to bottom.

  Images are decoded by SDL_image in any format it supports. SaveToFile writes
  a jpeg if the file name ends with .jpg or .jpeg, a bmp if it ends with .bmp,
  and a png otherwise. SaveToStream always writes a png. }

type
  TBitmapDataSDL = class(TInterfacedObject, IBitmapData)
  private
    FWidth: Integer;
    FHeight: Integer;
    FPixels: TBytes;
  public
    function GetWidth: Integer;
    function GetHeight: Integer;
    function GetPixels: Pointer;
    { Resize the bitmap and clear its pixels to transparent black }
    procedure SetSize(Width, Height: Integer);
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(Stream: TStream);
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property Pixels: Pointer read GetPixels;
  end;

{ Create an empty SDL bitmap }
function NewBitmapDataSDL: IBitmapData;

{ Create a window for an SDL window. The SDL window must outlive it. }
function NewWindowSDL(Window: PSDL_Window): IWindow;

implementation

uses
  Codebot.System;

{ SDL_image routines }

function IMG_Load_RW(src: PSDL_RWops; freesrc: LongInt): PSDL_Surface; cdecl; external 'SDL2_image';
function IMG_SavePNG_RW(surface: PSDL_Surface; dst: PSDL_RWops; freedst: LongInt): LongInt; cdecl; external 'SDL2_image';
function IMG_SaveJPG_RW(surface: PSDL_Surface; dst: PSDL_RWops; freedst: LongInt; quality: LongInt): LongInt; cdecl; external 'SDL2_image';

const
  { SDL_PIXELFORMAT_ARGB8888 stores pixels in memory as blue, green, red, and
    alpha on little endian machines, matching the IBitmapData layout }
  PixelFormat = SDL_PIXELFORMAT_ARGB8888;
  MaskA = $FF000000;
  MaskR = $00FF0000;
  MaskG = $0000FF00;
  MaskB = $000000FF;
  JpegQuality = 90;

procedure SDLCheck(Failed: Boolean; const Action: string);
begin
  if Failed then
    raise EInOutError.CreateFmt('Could not %s bitmap: %s', [Action, string(SDL_GetError)]);
end;

{ TStreamOps lets SDL read and write a TStream. The record starts with the SDL
  operations so a pointer to it can be passed as a PSDL_RWops. SDL does not
  free it because it is closed by the routines in this unit. }

type
  TStreamOps = record
    Ops: TSDL_RWops;
    Stream: TStream;
  end;
  PStreamOps = ^TStreamOps;

function StreamSize(context: PSDL_RWops): Sint64; cdecl;
begin
  try
    Result := PStreamOps(context).Stream.Size;
  except
    Result := -1;
  end;
end;

function StreamSeek(context: PSDL_RWops; offset: Sint64; whence: LongInt): Sint64; cdecl;
begin
  try
    case whence of
      RW_SEEK_SET: Result := PStreamOps(context).Stream.Seek(offset, soBeginning);
      RW_SEEK_CUR: Result := PStreamOps(context).Stream.Seek(offset, soCurrent);
      RW_SEEK_END: Result := PStreamOps(context).Stream.Seek(offset, soEnd);
    else
      Result := -1;
    end;
  except
    Result := -1;
  end;
end;

function StreamRead(context: PSDL_RWops; ptr: Pointer; size, maxnum: IntPtr): IntPtr; cdecl;
begin
  Result := 0;
  if (size < 1) or (maxnum < 1) then
    Exit;
  try
    Result := PStreamOps(context).Stream.Read(ptr^, size * maxnum) div size;
  except
    Result := 0;
  end;
end;

function StreamWrite(context: PSDL_RWops; ptr: Pointer; size, num: IntPtr): IntPtr; cdecl;
begin
  Result := 0;
  if (size < 1) or (num < 1) then
    Exit;
  try
    Result := PStreamOps(context).Stream.Write(ptr^, size * num) div size;
  except
    Result := 0;
  end;
end;

function StreamClose(context: PSDL_RWops): LongInt; cdecl;
begin
  Result := 0;
end;

procedure StreamOpsInit(out Ops: TStreamOps; Stream: TStream);
begin
  Ops := Default(TStreamOps);
  Ops.Ops.size := StreamSize;
  Ops.Ops.seek := StreamSeek;
  Ops.Ops.read := StreamRead;
  Ops.Ops.write := StreamWrite;
  Ops.Ops.close := StreamClose;
  Ops.Ops.type_ := SDL_RWOPS_UNKNOWN;
  Ops.Stream := Stream;
end;

{ TBitmapDataSDL }

function TBitmapDataSDL.GetWidth: Integer;
begin
  Result := FWidth;
end;

function TBitmapDataSDL.GetHeight: Integer;
begin
  Result := FHeight;
end;

function TBitmapDataSDL.GetPixels: Pointer;
begin
  if Length(FPixels) = 0 then
    Result := nil
  else
    Result := @FPixels[0];
end;

procedure TBitmapDataSDL.SetSize(Width, Height: Integer);
begin
  if (Width < 1) or (Height < 1) then
  begin
    Width := 0;
    Height := 0;
  end;
  FWidth := Width;
  FHeight := Height;
  FPixels := nil;
  SetLength(FPixels, Width * Height * 4);
  if Length(FPixels) > 0 then
    FillChar(FPixels[0], Length(FPixels), 0);
end;

procedure TBitmapDataSDL.LoadFromFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

{ The image is converted to the bitmap pixel format and its colors are
  premultiplied by alpha as the rows are copied }

procedure TBitmapDataSDL.LoadFromStream(Stream: TStream);
var
  Ops: TStreamOps;
  Image, Converted: PSDL_Surface;
  Source, Dest: PByte;
  X, Y, A: Integer;
begin
  StreamOpsInit(Ops, Stream);
  Image := IMG_Load_RW(@Ops.Ops, 0);
  SDLCheck(Image = nil, 'load');
  try
    Converted := SDL_ConvertSurfaceFormat(Image, PixelFormat, 0);
    SDLCheck(Converted = nil, 'convert');
  finally
    SDL_FreeSurface(Image);
  end;
  try
    SetSize(Converted.w, Converted.h);
    if FWidth = 0 then
      Exit;
    SDL_LockSurface(Converted);
    try
      Dest := @FPixels[0];
      for Y := 0 to FHeight - 1 do
      begin
        Source := PByte(Converted.pixels) + Y * Converted.pitch;
        for X := 0 to FWidth - 1 do
        begin
          A := Source[3];
          Dest[0] := (Source[0] * A + $7F) div $FF;
          Dest[1] := (Source[1] * A + $7F) div $FF;
          Dest[2] := (Source[2] * A + $7F) div $FF;
          Dest[3] := A;
          Inc(Source, 4);
          Inc(Dest, 4);
        end;
      end;
    finally
      SDL_UnlockSurface(Converted);
    end;
  finally
    SDL_FreeSurface(Converted);
  end;
end;

{ CreateSurface makes a surface with the colors divided by alpha, which is
  what image files store. The caller frees the surface. }

function CreateSurface(Bitmap: TBitmapDataSDL): PSDL_Surface;
var
  Source, Dest: PByte;
  X, Y, A: Integer;
begin
  if Bitmap.FWidth = 0 then
    raise EInOutError.Create('Could not save bitmap: the bitmap is empty');
  Result := SDL_CreateRGBSurface(0, Bitmap.FWidth, Bitmap.FHeight, 32,
    MaskR, MaskG, MaskB, MaskA);
  SDLCheck(Result = nil, 'create');
  SDL_LockSurface(Result);
  try
    Source := @Bitmap.FPixels[0];
    for Y := 0 to Bitmap.FHeight - 1 do
    begin
      Dest := PByte(Result.pixels) + Y * Result.pitch;
      for X := 0 to Bitmap.FWidth - 1 do
      begin
        A := Source[3];
        if A = 0 then
        begin
          Dest[0] := 0;
          Dest[1] := 0;
          Dest[2] := 0;
        end
        else if A = $FF then
        begin
          Dest[0] := Source[0];
          Dest[1] := Source[1];
          Dest[2] := Source[2];
        end
        else
        begin
          Dest[0] := (Source[0] * $FF + A div 2) div A;
          Dest[1] := (Source[1] * $FF + A div 2) div A;
          Dest[2] := (Source[2] * $FF + A div 2) div A;
        end;
        Dest[3] := A;
        Inc(Source, 4);
        Inc(Dest, 4);
      end;
    end;
  finally
    SDL_UnlockSurface(Result);
  end;
end;

type
  TSaveFormat = (sfPng, sfJpeg, sfBmp);

procedure SaveSurface(Bitmap: TBitmapDataSDL; Stream: TStream; Format: TSaveFormat);
var
  Ops: TStreamOps;
  Surface: PSDL_Surface;
  R: LongInt;
begin
  Surface := CreateSurface(Bitmap);
  try
    StreamOpsInit(Ops, Stream);
    case Format of
      sfJpeg: R := IMG_SaveJPG_RW(Surface, @Ops.Ops, 0, JpegQuality);
      sfBmp: R := SDL_SaveBMP_RW(Surface, @Ops.Ops, 0);
    else
      R := IMG_SavePNG_RW(Surface, @Ops.Ops, 0);
    end;
    SDLCheck(R <> 0, 'save');
  finally
    SDL_FreeSurface(Surface);
  end;
end;

procedure TBitmapDataSDL.SaveToFile(const FileName: string);
var
  Ext: string;
  Format: TSaveFormat;
  Stream: TStream;
begin
  Ext := LowerCase(ExtractFileExt(FileName));
  if (Ext = '.jpg') or (Ext = '.jpeg') then
    Format := sfJpeg
  else if Ext = '.bmp' then
    Format := sfBmp
  else
    Format := sfPng;
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveSurface(Self, Stream, Format);
  finally
    Stream.Free;
  end;
end;

procedure TBitmapDataSDL.SaveToStream(Stream: TStream);
begin
  SaveSurface(Self, Stream, sfPng);
end;

function NewBitmapDataSDL: IBitmapData;
begin
  Result := TBitmapDataSDL.Create;
end;

{ TClipboardSDL uses the SDL clipboard, which needs SDL video to be running.
  Until it is the text is kept in memory. }

type
  TClipboardSDL = class(TInterfacedObject, IClipboard)
  private
    FText: string;
    function VideoRunning: Boolean;
  public
    function GetText: string;
    procedure SetText(const Value: string);
    function HasText: Boolean;
  end;

function TClipboardSDL.VideoRunning: Boolean;
begin
  Result := SDL_WasInit(SDL_INIT_VIDEO) <> 0;
end;

function TClipboardSDL.GetText: string;
var
  P: PChar;
begin
  if not VideoRunning then
    Exit(FText);
  P := SDL_GetClipboardText;
  if P = nil then
    Exit('');
  Result := P;
  SDL_free(P);
end;

procedure TClipboardSDL.SetText(const Value: string);
begin
  if VideoRunning then
    SDL_SetClipboardText(PChar(Value))
  else
    FText := Value;
end;

function TClipboardSDL.HasText: Boolean;
begin
  if VideoRunning then
    Result := SDL_HasClipboardText
  else
    Result := FText <> '';
end;

{ TWindowSDL }

{ SDL_SetWindowFullscreen takes window flags, which the interop unit declares
  as a boolean }

function SDL_SetWindowFullscreenFlags(window: PSDL_Window; flags: Uint32): LongInt;
  cdecl; external 'SDL2' name 'SDL_SetWindowFullscreen';
procedure SDL_GL_GetDrawableSize(window: PSDL_Window; out w, h: LongInt);
  cdecl; external 'SDL2';

type
  TWindowSDL = class(TInterfacedObject, IWindow)
  private
    FWindow: PSDL_Window;
  public
    constructor Create(Window: PSDL_Window);
    function GetTitle: string;
    procedure SetTitle(const Value: string);
    function GetFullscreen: Boolean;
    procedure SetFullscreen(Value: Boolean);
    function GetScale: Single;
    procedure Close;
  end;

constructor TWindowSDL.Create(Window: PSDL_Window);
begin
  inherited Create;
  FWindow := Window;
end;

function TWindowSDL.GetTitle: string;
begin
  Result := SDL_GetWindowTitle(FWindow);
end;

procedure TWindowSDL.SetTitle(const Value: string);
begin
  SDL_SetWindowTitle(FWindow, PChar(Value));
end;

function TWindowSDL.GetFullscreen: Boolean;
begin
  Result := SDL_GetWindowFlags(FWindow) and SDL_WINDOW_FULLSCREEN <> 0;
end;

procedure TWindowSDL.SetFullscreen(Value: Boolean);
begin
  if Value = GetFullscreen then
    Exit;
  if Value then
    SDL_SetWindowFullscreenFlags(FWindow, SDL_WINDOW_FULLSCREEN_DESKTOP)
  else
    SDL_SetWindowFullscreenFlags(FWindow, 0);
end;

{ The scale is the size of the drawable area in pixels over the size of the
  window in screen coordinates }

function TWindowSDL.GetScale: Single;
var
  W, H, DW, DH: LongInt;
begin
  Result := 1;
  SDL_GetWindowSize(FWindow, W, H);
  SDL_GL_GetDrawableSize(FWindow, DW, DH);
  if (W > 0) and (DW > 0) then
    Result := DW / W;
end;

{ Closing the window posts a quit event, which the application handles the
  same as the user closing the window }

procedure TWindowSDL.Close;
var
  Event: TSDL_Event;
begin
  Event := Default(TSDL_Event);
  Event.type_ := SDL_QUIT_EVENT;
  SDL_PushEvent(Event);
end;

function NewWindowSDL(Window: PSDL_Window): IWindow;
begin
  Result := TWindowSDL.Create(Window);
end;

{ TMessageDialogSDL shows an SDL message box, which waits for the user before
  Show returns }

type
  TMessageDialogSDL = class(TCustomMessageDialog)
  protected
    procedure Show; override;
  end;

procedure TMessageDialogSDL.Show;
const
  Flags: array[TMessageKind] of Uint32 = (SDL_MESSAGEBOX_INFORMATION,
    SDL_MESSAGEBOX_WARNING, SDL_MESSAGEBOX_ERROR);
var
  Data: TSDL_MessageBoxData;
  Items: array of TSDL_MessageBoxButtonData;
  Title, S: string;
  R: LongInt;
  I: Integer;
begin
  Title := GetTitle;
  if FButtons.Length = 0 then
  begin
    if SDL_ShowSimpleMessageBox(Flags[FKind], PChar(Title), PChar(FMessage),
      SDL_GetKeyboardFocus) < 0 then
      CloseButton(-1)
    else
      CloseButton(0);
    Exit;
  end;
  SetLength(Items, FButtons.Length);
  for I := 0 to High(Items) do
  begin
    Items[I] := Default(TSDL_MessageBoxButtonData);
    { Buttons named cancel, close, exit, or quit are chosen by escape }
    S := LowerCase(FButtons.Items[I]);
    if (Pos('cancel', S) > 0) or (Pos('close', S) > 0) or (Pos('exit', S) > 0) or
      (Pos('quit', S) > 0) then
      Items[I].flags := SDL_MESSAGEBOX_BUTTON_ESCAPEKEY_DEFAULT
    else if I = 0 then
      Items[I].flags := SDL_MESSAGEBOX_BUTTON_RETURNKEY_DEFAULT;
    Items[I].buttonid := I;
    Items[I].text := PChar(FButtons.Items[I]);
  end;
  Data := Default(TSDL_MessageBoxData);
  Data.flags := Flags[FKind];
  Data.window := SDL_GetKeyboardFocus;
  Data.title := PChar(Title);
  Data.message := PChar(FMessage);
  Data.numbuttons := Length(Items);
  Data.buttons := @Items[0];
  R := -1;
  if SDL_ShowMessageBox(Data, R) < 0 then
    R := -1;
  CloseButton(R);
end;

function NewMessageDialogSDL: IMessageDialog;
begin
  Result := TMessageDialogSDL.Create;
end;

{ SDL has no file dialogs, so the dialogs made from widgets are used. They are
  shown in the main widget of a widget scene, and close without being accepted
  if there is none. }

function NewFileDialogSDL(Kind: TFileDialogKind): IFileDialog;
begin
  Result := NewWidgetFileDialog(Kind);
end;

function NewPictureDialogSDL(Kind: TFileDialogKind): IPictureDialog;
begin
  Result := NewWidgetPictureDialog(Kind);
end;

initialization
  NewBitmapData := NewBitmapDataSDL;
  NewMessageDialog := NewMessageDialogSDL;
  NewFileDialog := NewFileDialogSDL;
  NewPictureDialog := NewPictureDialogSDL;
  PlatformClipboard := TClipboardSDL.Create;
end.
