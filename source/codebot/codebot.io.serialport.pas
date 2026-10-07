(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified June 2022                                  *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.io.serialport.txt> }
unit Codebot.IO.SerialPort;

{$i codebot.inc}

interface

{$if defined(linux) or defined(windows)}
uses
  SysUtils, Classes, TypInfo;

{ Common serial port baud rates }

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

  { Number of data bits per character }
  Bits5 = 5;
  Bits6 = 6;
  Bits7 = 7;
  Bits8 = 8;

type
  { Parity checking mode }
  TParity = (prNone, prOdd, prEven);
  { Number of stop bits }
  TStopBits = (sbOne, sbTwo);
  { Software and hardware flow control options }
  TFlowControl = set of (fcXOn, fcXOff, fcRequestToSend);

{ TSerialPortOptions holds the line settings of a serial port }

  TSerialPortOptions = record
  public
    { Speed in bits per second }
    Baud: Integer;
    { Number of data bits per character }
    DataBits: Integer;
    { Parity checking mode }
    Parity: TParity;
    { Number of stop bits }
    StopBits: TStopBits;
    { Flow control options }
    FlowControl: TFlowControl;
    { Minimum number of bytes a read waits for }
    Min: Byte;
    { Read timeout in tenths of a second }
    Timeout: Byte;
    { Read the current options of a serial port device }
    class function Create(const Device: string): TSerialPortOptions; overload; static;
    { Create options with a baud rate, data bits, and parity }
    class function Create(Baud: Integer = Baud9600; DataBits: Integer = Bits8;
      Parity: TParity = prNone): TSerialPortOptions; overload; static;
    { Convert the options to a readable string }
    function ToString: string;
  end;

{ TSerialPort reads and writes data to a serial port device on Linux and Windows }

  TSerialPort = class
  private
    FDevice: string;
    FHandle: THandle;
    FReadBuffer: array[0..1023] of Byte;
    function UpdatePort(const Options: TSerialPortOptions): Boolean;
    procedure CheckOpened;
    function GetOpened: Boolean;
  public
    { Create a serial port given a device path such as /dev/ttyUSB0 on Linux
      or a port name such as COM3 on Windows }
    constructor Create(const Device: string);
    { Close the port and destroy the object }
    destructor Destroy; override;
    { Open the port using its current options }
    function Open: Boolean; overload;
    { Open the port and apply options }
    function Open(const Options: TSerialPortOptions): Boolean; overload;
    { Close the port }
    procedure Close;
    { Read available data as a string }
    function Read: string;
    { Read up to BufferSize bytes returning the number of bytes read }
    function ReadBinary(var Buffer; BufferSize: Integer): Integer;
    { Write a string to the port }
    procedure Write(const S: string);
    { Write a block of memory to the port }
    procedure WriteBinary(var Buffer; BufferSize: Integer);
    { Send an XOn character to resume transmission }
    procedure XOn;
    { Send an XOff character to pause transmission }
    procedure XOff;
    { Opened is true while the port is open }
    property Opened: Boolean read GetOpened;
    { The device path of the port }
    property Device: string read FDevice;
  end;

{ Fill a strings object with the device paths of available serial ports }
procedure EnumSerialPorts(Ports: TStrings);
{$endif}

implementation

{$if defined(linux) or defined(windows)}
{$ifdef linux}
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

  { TCIFLUSH = 0; TCOFLUSH = 1;}
  TCIOFLUSH = 2;

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
function _tcflush(fd: THandle; queue: Integer): Integer; cdecl; external libc name 'tcflush';
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

{ TSerialPortOptions }

class function TSerialPortOptions.Create(const Device: string): TSerialPortOptions;
var
  F: THandle;
  T: TTermios;
begin
  FillChar(Result{%H-}, SizeOf(Result), 0);
  if not FileExists(Device) then
    Exit;
  F := _open(PChar(Device), O_RDWR or O_NOCTTY);
  if F > 0 then
  try
    if _tcgetattr(F, T) = 0 then
    begin
      if (T.c_cflag and B230400) = B230400 then
        Result.Baud := Baud230400
      else if (T.c_cflag and B115200) = B115200 then
        Result.Baud := Baud115200
      else if (T.c_cflag and B57600) = B57600 then
        Result.Baud := Baud57600
      else if (T.c_cflag and B38400) = B38400 then
        Result.Baud := Baud38400
      else if (T.c_cflag and B19200) = B19200 then
        Result.Baud := Baud19200
      else if (T.c_cflag and B9600) = B9600 then
        Result.Baud := Baud9600
      else if (T.c_cflag and B4800) = B4800 then
        Result.Baud := Baud4800
      else if (T.c_cflag and B2400) = B2400 then
        Result.Baud := Baud2400
      else if (T.c_cflag and B1200) = B1200 then
        Result.Baud := Baud1200
      else if (T.c_cflag and B300) = B300 then
        Result.Baud := Baud300
      else
        Exit;
      if (T.c_cflag and CS8) = CS8 then
        Result.DataBits := Bits8
      else if (T.c_cflag and CS7) = CS7 then
        Result.DataBits := Bits7
      else if (T.c_cflag and CS6) = CS6 then
        Result.DataBits := Bits6
      else
        Result.DataBits := Bits5;
      if (T.c_cflag and (PARENB or PARODD)) = PARENB or PARODD then
        Result.Parity := prOdd
      else if (T.c_cflag and PARENB) = PARENB then
        Result.Parity := prEven
      else
        Result.Parity := prNone;
      if (T.c_cflag and CSTOPB) = CSTOPB then
        Result.StopBits := sbTwo
      else
        Result.StopBits := sbOne;
      if (T.c_iflag and IXON) = IXON then
        Include(Result.FlowControl, fcXOn);
      if (T.c_iflag and IXOFF) = IXOFF then
        Include(Result.FlowControl, fcXOff);
      if (T.c_iflag and CRTSCTS) = CRTSCTS then
        Include(Result.FlowControl, fcRequestToSend);
      Result.Timeout := T.c_cc[VTIME] ;
      Result.Min := T.c_cc[VMIN];
    end;
  finally
    _close(F);
  end;
end;
{$else}
uses
  Windows;

const
  { DCB flag bits }
  DCB_BINARY = $0001;
  DCB_PARITY = $0002;
  DCB_OUTX_CTS_FLOW = $0004;
  DCB_OUTX_DSR_FLOW = $0008;
  DCB_DTR_CONTROL_MASK = $0030;
  DCB_DTR_CONTROL_ENABLE = $0010;
  DCB_DSR_SENSITIVITY = $0040;
  DCB_OUTX = $0100;
  DCB_INX = $0200;
  DCB_RTS_CONTROL_MASK = $3000;
  DCB_RTS_CONTROL_ENABLE = $1000;
  DCB_RTS_CONTROL_HANDSHAKE = $2000;
  DCB_ABORT_ON_ERROR = $4000;

{ Open a port by name. The \\.\ prefix is required for COM10 and above. }

function OpenPort(const Device: string): THandle;
var
  S: string;
begin
  S := Device;
  if Copy(S, 1, 4) <> '\\.\' then
    S := '\\.\' + S;
  Result := CreateFile(PChar(S), GENERIC_READ or GENERIC_WRITE, 0, nil,
    OPEN_EXISTING, 0, 0);
  if Result = INVALID_HANDLE_VALUE then
    Result := 0;
end;

class function TSerialPortOptions.Create(const Device: string): TSerialPortOptions;
var
  F: THandle;
  D: TDCB;
  T: TCommTimeouts;
begin
  FillChar(Result{%H-}, SizeOf(Result), 0);
  F := OpenPort(Device);
  if F = 0 then
    Exit;
  try
    FillChar(D, SizeOf(D), 0);
    D.DCBlength := SizeOf(D);
    if not GetCommState(F, D) then
      Exit;
    Result.Baud := D.BaudRate;
    Result.DataBits := D.ByteSize;
    case D.Parity of
      ODDPARITY: Result.Parity := prOdd;
      EVENPARITY: Result.Parity := prEven;
    else
      Result.Parity := prNone;
    end;
    if D.StopBits = TWOSTOPBITS then
      Result.StopBits := sbTwo
    else
      Result.StopBits := sbOne;
    if D.Flags and DCB_OUTX <> 0 then
      Include(Result.FlowControl, fcXOn);
    if D.Flags and DCB_INX <> 0 then
      Include(Result.FlowControl, fcXOff);
    if D.Flags and DCB_OUTX_CTS_FLOW <> 0 then
      Include(Result.FlowControl, fcRequestToSend);
    { Convert the timeouts back to the Min and Timeout settings, see UpdatePort }
    if GetCommTimeouts(F, T) then
      if T.ReadIntervalTimeout = MAXDWORD then
      begin
        if T.ReadTotalTimeoutMultiplier = MAXDWORD then
          if T.ReadTotalTimeoutConstant >= MAXDWORD - 1 then
            Result.Min := 1
          else if T.ReadTotalTimeoutConstant div 100 > High(Byte) then
            Result.Timeout := High(Byte)
          else
            Result.Timeout := T.ReadTotalTimeoutConstant div 100;
      end
      else if T.ReadIntervalTimeout > 0 then
      begin
        Result.Min := 1;
        if T.ReadIntervalTimeout div 100 > High(Byte) then
          Result.Timeout := High(Byte)
        else
          Result.Timeout := T.ReadIntervalTimeout div 100;
      end
      else
        Result.Min := 1;
  finally
    CloseHandle(F);
  end;
end;
{$endif}

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

{$ifdef linux}
function TSerialPort.Open(const Options: TSerialPortOptions): Boolean;
begin
  Result := False;
  if Opened then
    Exit;
  if not FileExists(FDevice) then
    Exit;
  FHandle := _open(PChar(FDevice), O_RDWR or O_NOCTTY);
  Result := Opened and UpdatePort(Options);
end;

function TSerialPort.UpdatePort(const Options: TSerialPortOptions): Boolean;
var
  T: TTermios;
begin
  Result := False;
  if _tcgetattr(FHandle, T) <> 0 then
  begin
    Close;
    Exit;
  end;
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
  if _tcsetattr(FHandle, TCSANOW, T) <> 0 then
  begin
    Close;
    Exit;
  end;
  _tcflush(FHandle, TCIOFLUSH);
  Result := True;
end;

procedure TSerialPort.Close;
var
  H: THandle;
begin
  if not Opened then
    Exit;
  H := FHandle;
  FHandle := 0;
  _tcflush(H, TCIOFLUSH);
  _close(H);
end;

{$else}
function TSerialPort.Open(const Options: TSerialPortOptions): Boolean;
begin
  Result := False;
  if Opened then
    Exit;
  FHandle := OpenPort(FDevice);
  Result := Opened and UpdatePort(Options);
end;

function TSerialPort.UpdatePort(const Options: TSerialPortOptions): Boolean;
var
  D: TDCB;
  T: TCommTimeouts;
begin
  Result := False;
  FillChar(D, SizeOf(D), 0);
  D.DCBlength := SizeOf(D);
  if not GetCommState(FHandle, D) then
  begin
    Close;
    Exit;
  end;
  case Options.Baud of
    Baud300, Baud1200, Baud2400, Baud4800, Baud9600, Baud19200, Baud38400,
    Baud57600, Baud115200, Baud230400: D.BaudRate := Options.Baud;
  else
    D.BaudRate := Baud9600;
  end;
  case Options.DataBits of
    Bits5, Bits6, Bits7, Bits8: D.ByteSize := Options.DataBits;
  else
    D.ByteSize := Bits8;
  end;
  case Options.Parity of
    prOdd: D.Parity := ODDPARITY;
    prEven: D.Parity := EVENPARITY;
  else
    D.Parity := NOPARITY;
  end;
  if Options.StopBits = sbTwo then
    D.StopBits := TWOSTOPBITS
  else
    D.StopBits := ONESTOPBIT;
  { Raw binary mode with DTR and RTS on, and flow control as requested }
  D.Flags := DCB_BINARY or DCB_DTR_CONTROL_ENABLE;
  if Options.Parity <> prNone then
    D.Flags := D.Flags or DCB_PARITY;
  if fcXOn in Options.FlowControl then
    D.Flags := D.Flags or DCB_OUTX;
  if fcXOff in Options.FlowControl then
    D.Flags := D.Flags or DCB_INX;
  if fcRequestToSend in Options.FlowControl then
    D.Flags := D.Flags or DCB_OUTX_CTS_FLOW or DCB_RTS_CONTROL_HANDSHAKE
  else
    D.Flags := D.Flags or DCB_RTS_CONTROL_ENABLE;
  D.XonChar := #$11;
  D.XoffChar := #$13;
  if not SetCommState(FHandle, D) then
  begin
    Close;
    Exit;
  end;
  { Imitate the Linux VMIN and VTIME read settings. Timeout is in tenths of a
    second.
    Min = 0, Timeout = 0: return immediately with what is available
    Min = 0, Timeout > 0: wait up to Timeout for any data
    Min > 0, Timeout = 0: wait for at least one byte, Windows cannot wait for
      an exact number of bytes without blocking until the buffer is full
    Min > 0, Timeout > 0: wait for the first byte, then return when the gap
      between bytes exceeds Timeout }
  FillChar(T, SizeOf(T), 0);
  if Options.Min = 0 then
  begin
    T.ReadIntervalTimeout := MAXDWORD;
    if Options.Timeout > 0 then
    begin
      T.ReadTotalTimeoutMultiplier := MAXDWORD;
      T.ReadTotalTimeoutConstant := Options.Timeout * 100;
    end;
  end
  else if Options.Timeout = 0 then
  begin
    T.ReadIntervalTimeout := MAXDWORD;
    T.ReadTotalTimeoutMultiplier := MAXDWORD;
    T.ReadTotalTimeoutConstant := MAXDWORD - 1;
  end
  else
    T.ReadIntervalTimeout := Options.Timeout * 100;
  if not SetCommTimeouts(FHandle, T) then
  begin
    Close;
    Exit;
  end;
  PurgeComm(FHandle, PURGE_RXCLEAR or PURGE_TXCLEAR);
  Result := True;
end;

procedure TSerialPort.Close;
var
  H: THandle;
begin
  if not Opened then
    Exit;
  H := FHandle;
  FHandle := 0;
  PurgeComm(H, PURGE_RXCLEAR or PURGE_TXCLEAR);
  CloseHandle(H);
end;
{$endif}

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
{$ifdef windows}
var
  Count: DWORD;
{$endif}
begin
  CheckOpened;
  {$ifdef linux}
  Result := _read(FHandle, Buffer, BufferSize);
  {$else}
  Count := 0;
  if ReadFile(FHandle, Buffer, BufferSize, Count, nil) then
    Result := Count
  else
    Result := -1;
  {$endif}
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
{$ifdef windows}
var
  Count: DWORD;
{$endif}
begin
  CheckOpened;
  {$ifdef linux}
  _write(FHandle, Buffer, BufferSize);
  {$else}
  Count := 0;
  WriteFile(FHandle, Buffer, BufferSize, Count, nil);
  {$endif}
end;

procedure TSerialPort.XOn;
var
  B: Byte;
begin
  B := $11;
  WriteBinary(B, 1);
end;

procedure TSerialPort.XOff;
var
  B: Byte;
begin
  B := $13;
  WriteBinary(B, 1);
end;

{$ifdef linux}
function TSerialPort.GetOpened: Boolean;
begin
  Result := FHandle > 0;
end;

procedure EnumSerialPorts(Ports: TStrings);

  function CheckPort(const Device: string): Boolean;
  var
    F: THandle;
    T: TTermios;
  begin
    Result := False;
    if not FileExists(Device) then
      Exit;
    F := _open(PChar(Device), O_RDWR or O_NOCTTY);
    if F > 0 then
    begin
      Result := _tcgetattr(F, T) = 0;
      _close(F);
    end;
  end;

const
  MaxPorts = 9;
var
  S, D: string;
  I: Integer;
begin
  Ports.BeginUpdate;
  try
    Ports.Clear;
    for I := 0 to MaxPorts do
    begin
      S := 'ttyS' + IntToStr(I);
      D := '/sys/class/tty/' + S + '/device/';
      if DirectoryExists(D) and (FileExists(D +'/id') or DirectoryExists(D + '/of_node')) then
      begin
        S := '/dev/' + S;
        if CheckPort(S) then
          Ports.Add(S);
      end;
    end;
    for I := 0 to MaxPorts do
    begin
      S := 'ttyUSB' + IntToStr(I);
      D := '/sys/class/tty/' + S + '/device/tty';
      if DirectoryExists(D) or DirectoryExists('/sys/bus/usb-serial/devices/' + S) then
      begin
        S := '/dev/' + S;
        if CheckPort(S) then
          Ports.Add(S);
      end;
    end;
  finally
    Ports.EndUpdate;
  end;
end;
{$else}
function TSerialPort.GetOpened: Boolean;
begin
  Result := FHandle <> 0;
end;

{ Serial ports are listed in the registry by the drivers which create them }

procedure EnumSerialPorts(Ports: TStrings);
var
  Key: HKEY;
  Index, NameSize, DataSize, ValueType: DWORD;
  Name: array[0..255] of Char;
  Data: array[0..255] of Char;
  Names: TStringList;
begin
  Ports.BeginUpdate;
  Names := TStringList.Create;
  try
    Ports.Clear;
    if RegOpenKeyEx(HKEY_LOCAL_MACHINE, 'HARDWARE\DEVICEMAP\SERIALCOMM', 0,
      KEY_READ, Key) <> ERROR_SUCCESS then
      Exit;
    try
      Index := 0;
      repeat
        NameSize := Length(Name);
        DataSize := SizeOf(Data) - 1;
        FillChar(Data, SizeOf(Data), 0);
        if RegEnumValue(Key, Index, Name, NameSize, nil, @ValueType,
          @Data, @DataSize) <> ERROR_SUCCESS then
          Break;
        if ValueType = REG_SZ then
          Names.Add(PChar(@Data));
        Inc(Index);
      until False;
    finally
      RegCloseKey(Key);
    end;
    Names.Sort;
    Ports.AddStrings(Names);
  finally
    Names.Free;
    Ports.EndUpdate;
  end;
end;
{$endif}
{$endif}

end.

