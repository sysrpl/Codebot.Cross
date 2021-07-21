(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified July 2021                                 *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.io.serialport.txt> }
unit Codebot.IO.SerialPort;

{$i codebot.inc}

interface

const
  Baud300 = 300;
  Baud1200 = 1200;
  Baud2400 = 2400;
  Baud4800 = 4800;
  Baud9600 = 9600;
  Baud19200 = 19200;
  Baud38400 = 38400;
  Baud57600 = 57600;
  Baud115200 = 115200;
  Baud230400 = 230400;

  Bits5 = 5;
  Bits6 = 6;
  Bits7 = 7;
  Bits8 = 8;

type
  TParity = (prNone, prOdd, prEven);
  TStopBits = (sbOne, sbTwo);
  TFlowControl = set of (fcXOn, fcXOff, fcRequestToSend);

{ TSerialPortOptions }

  TSerialPortOptions = record
  public
    Baud: Integer;
    DataBits: Integer;
    Parity: TParity;
    StopBits: TStopBits;
    FlowControl: TFlowControl;
    Min: Byte;
    Timeout: Byte;
    class function Create(Baud: Integer = Baud9600; DataBits: Integer = Bits8;
      Parity: TParity = prNone): TSerialPortOptions; overload; static;
    function ToString: string;
  end;

{ TSerialPort }

  TSerialPort = class
  private
    FDevice: string;
    FHandle: THandle;
    FReadBuffer: array[0..1023] of Byte;
    procedure UpdatePort(const Options: TSerialPortOptions);
    procedure CheckOpened;
    function GetOpened: Boolean;
  public
    constructor Create(const Device: string);
    destructor Destroy; override;
    function Open: Boolean; overload;
    function Open(const Options: TSerialPortOptions): Boolean; overload;
    procedure Close;
    function Read: string;
    function ReadBinary(var Buffer; BufferSize: Integer): Integer;
    procedure Write(const S: string);
    procedure WriteBinary(var Buffer; BufferSize: Integer);
    property Opened: Boolean read GetOpened;
  end;

implementation

uses
  SysUtils,
  TypInfo;

{ TSerialPortOptions }

class function TSerialPortOptions.Create(Baud: Integer; DataBits: Integer;
  Parity: TParity): TSerialPortOptions;
begin
  Result.Baud := Baud;
  Result.DataBits := DataBits;
  Result.Parity := Parity;
  Result.StopBits := sbOne;
  Result.FlowControl := [];
  Result.Min := 0;
  Result.Timeout := 0;
end;

function TSerialPortOptions.ToString: string;
begin
  Result :=
    'Baud: ' + IntToStr(Baud) + #10 +
    'DataBits: ' + IntToStr(DataBits) + #10 +
    'Parity: ' + GetEnumName(TypeInfo(TParity), Ord(Parity)) + #10 +
    'StopBits: ' + GetEnumName(TypeInfo(TStopBits), Ord(StopBits)) + #10 +
    'FlowControl: ' + SetToString(PTypeInfo(TypeInfo(TFlowControl)), Pointer(@FlowControl), True) + #10 +
    'Min: ' + IntToStr(Min) + #10 +
    'Timeout: ' + IntToStr(Timeout);
end;

const
  O_RDWR = $02;
  O_NOCTTY = $100;
  TCSANOW = $00;
  CBAUD = 4111;
  CBAUDEX = 4096;

  B300 = 7;
  B1200 = 9;
  B2400 = 11;
  B4800 = 12;
  B9600 = 13;
  B19200 = 14;
  B38400 = 15;
  B57600 = 4097;
  B115200 = 4098;
  B230400 = 4099;

  CS5 = 0;
  CS6 = 16;
  CS7 = 32;
  CS8 = 48;

  CSTOPB = 64;

  PARENB = 256;
  PARODD = 512;

  CLOCAL = 2048;
  CREAD = 128;
  CSIZE = 48;
  ECHO = 8;
  ECHOE = 16;
  ECHOK = 32;
  ECHONL = 64;
  ICANON = 2;
  ICRNL = 256;
  IEXTEN = 32768;
  IGNBRK = 1;
  IGNCR = 128;
  INLCR = 64;
  INPCK = 16;
  ISIG = 1;
  ISTRIP = 32;
  OCRNL = 8;
  ONLCR = 4;
  OPOST = 1;
  IXON = 1024;
  IXOFF = 4096;
  CRTSCTS = 2147483648;

  VTIME = 5;
  VMIN = 6;

type
  termios = record
    c_iflag: LongWord;
    c_oflag: LongWord;
    c_cflag: LongWord;
    c_lflag: LongWord;
    c_line: Byte;
    c_cc: array[0..34] of Byte;
    c_ispeed: LongWord;
    c_ospeed: LongWord;
  end;
  TTermios = termios;

{$ifdef unix}
const
  libc = 'c';

function _open(path: PChar; flags: Integer): Integer; cdecl; external libc name 'open';
function _close(fd: THandle): Integer; cdecl; external libc name 'close';
function _write(fd: THandle; var buffer; numBytes: Integer): Integer; cdecl; external libc name 'write';
function _read(fd: THandle; var buffer; numBytes: Integer): Integer; cdecl; external libc name 'read';
function _ioctl(fd: THandle; request: DWord; value: Integer): Integer; cdecl; external libc name 'ioctl';
function _tcgetattr(fd: THandle; out term: TTermios): Integer; cdecl; external libc name 'tcgetattr';
function _tcsetattr(fd: THandle; actions: Integer; var term: TTermios): Integer; cdecl; external libc name 'tcsetattr';
{$else}
function _open(path: PChar; flags: Integer): Integer;
begin
  Result := 0;
end;
function _close(fd: THandle): Integer;
begin
  Result := 0;
end;
function _write(fd: THandle; var buffer; numBytes: Integer): Integer;
begin
  Result := 0;
end;
function _read(fd: THandle; var buffer; numBytes: Integer): Integer;
begin
  Result := 0;
end;
function _ioctl(fd: THandle; request: DWord; value: Integer): Integer;
begin
  Result := 0;
end;

function _tcgetattr(fd: THandle; out term: TTermios): Integer;
begin
  Result := 0;
end;

function _tcsetattr(fd: THandle; actions: Integer; var term: TTermios): Integer;
begin
  Result := 0;
end;
{$endif}

{ TSerialPort }

constructor TSerialPort.Create(const Device: string);
begin
  FDevice := Device;
  inherited Create;
end;

destructor TSerialPort.Destroy;
begin
  Close;
  inherited Destroy;
end;

function TSerialPort.Open: Boolean;
begin
  Result := Open(TSerialPortOptions.Create);
end;

function TSerialPort.Open(const Options: TSerialPortOptions): Boolean;
begin
  Result := False;
  if Opened then
    Exit;
  if not FileExists(FDevice) then
    Exit;
  FHandle := _open(PChar(FDevice), O_RDWR or O_NOCTTY);
  if not Opened then
    Exit;
  UpdatePort(Options);
  Result := True;
end;

procedure TSerialPort.UpdatePort(const Options: TSerialPortOptions);
var
  T: TTermios;
begin
  _tcgetattr(FHandle, T);
  T.c_cflag := T.c_cflag or CLOCAL or CREAD;
  T.c_lflag := T.c_lflag and (not (ICANON or ECHO or ECHOE or ECHOK or ECHONL or ISIG or IEXTEN));
  T.c_oflag := T.c_oflag and (not (OPOST or ONLCR or OCRNL));
  T.c_iflag := T.c_iflag and (not (INLCR or IGNCR or ICRNL or IGNBRK or INPCK or ISTRIP or IXON or IXOFF));
  T.c_cflag := T.c_cflag and (not (CBAUD or CBAUDEX or CSIZE or CRTSCTS));
  case Options.Baud of
    Baud300: T.c_cflag := T.c_cflag or B300;
    Baud1200: T.c_cflag := T.c_cflag or B1200;
    Baud2400: T.c_cflag := T.c_cflag or B2400;
    Baud4800: T.c_cflag := T.c_cflag or B4800;
    Baud9600: T.c_cflag := T.c_cflag or B9600;
    Baud19200: T.c_cflag := T.c_cflag or B19200;
    Baud38400: T.c_cflag := T.c_cflag or B38400;
    Baud57600: T.c_cflag := T.c_cflag or B57600;
    Baud115200: T.c_cflag := T.c_cflag or B115200;
    Baud230400: T.c_cflag := T.c_cflag or B230400;
  else
    T.c_cflag := T.c_cflag or B9600;
  end;
  case Options.DataBits of
    Bits5: T.c_cflag := T.c_cflag or CS5;
    Bits6: T.c_cflag := T.c_cflag or CS6;
    Bits7: T.c_cflag := T.c_cflag or CS7;
    Bits8: T.c_cflag := T.c_cflag or CS8;
  else
    T.c_cflag := T.c_cflag or CS8;
  end;
  if Options.Parity = prOdd then
      T.c_cflag := T.c_cflag or PARENB or PARODD
  else if Options.Parity = prEven then
  begin
    T.c_cflag := T.c_cflag and (not PARODD);
    T.c_cflag := T.c_cflag or PARENB;
  end
  else
    T.c_cflag := T.c_cflag and (not (PARENB or PARODD));
  if Options.StopBits = sbOne then
    T.c_cflag := T.c_cflag and (not CSTOPB)
  else
    T.c_cflag := T.c_cflag or CSTOPB;
  if fcXOn in Options.FlowControl then
      T.c_iflag := T.c_iflag or IXON;
  if fcXOff in Options.FlowControl then
      T.c_iflag := T.c_iflag or IXOFF;
  if fcRequestToSend in Options.FlowControl then
      T.c_cflag := T.c_cflag or CRTSCTS;
  T.c_cc[VTIME] := Options.Timeout;
  T.c_cc[VMIN] := Options.Min;
  _tcsetattr(FHandle, TCSANOW, T);
end;

procedure TSerialPort.Close;
var
  H: THandle;
begin
  if not Opened then
    Exit;
  H := FHandle;
  FHandle := 0;
  _close(H);
end;

procedure TSerialPort.CheckOpened;
begin
  if not Opened then
    raise EInOutError.Create('Port is not opened');
end;

function TSerialPort.Read: string;
var
  B: TBytes;
  I: Integer;
begin
  Result := '';
  I := ReadBinary(FReadBuffer, SizeOf(FReadBuffer));
  if I < 1 then
    Exit;
  B := nil;
  SetLength(B, I);
  Move(FReadBuffer[0], B[0], I);
  Result := TEncoding.ANSI.GetAnsiString(B);
end;

function TSerialPort.ReadBinary(var Buffer; BufferSize: Integer): Integer;
begin
  CheckOpened;
  Result := _read(FHandle, Buffer, BufferSize);
end;

procedure TSerialPort.Write(const S: string);
var
  B: TBytes;
begin
  CheckOpened;
  if S = '' then
    Exit;
  B := TEncoding.UTF8.GetAnsiBytes(S);
  WriteBinary(B[0], Length(B));
end;

procedure TSerialPort.WriteBinary(var Buffer; BufferSize: Integer);
begin
  CheckOpened;
  _write(FHandle, Buffer, BufferSize);
end;

function TSerialPort.GetOpened: Boolean;
begin
  Result := FHandle > 0;
end;

end.

