(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified March 2015                                 *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.networking.ftp.txt> }
unit Codebot.Networking.Ftp;

{$i codebot.inc}

interface

uses
  Classes,
  SysUtils,
  Codebot.System,
  Codebot.Networking;

{ TFileSystemAttributes is a set of flags attached to a file system object
  See also
  <link Codebot.Networking.Ftp.TFileSystemAttributes, TFileSystemAttributes set> }

type
  TFileSystemAttributes = set of (
    { Item is a directory }
    fsaDirectory,
    { Item is a symbolic link }
    fsaLink,
    { Read permissions are set for the current user }
    fsaRead,
    { Write permissions are set for the current user }
    fsaWrite,
    { Execute permissions are set for the current user }
    fsaExecute);

{ fsaAny is a shortcut to all file system flagsSee also
  See also
  <link Codebot.Networking.Ftp.TFileSystemAttributes, TFileSystemAttributes set> }

const
  fsaAny = [fsaDirectory, fsaLink, fsaRead, fsaWrite, fsaExecute];

{ TRemoteFindData is used  by <link Codebot.Networking.Ftp.TFtpClient.FindFirst, TFtpClient.FindFirst method>
  See also
  <link Codebot.Networking.Ftp.TFtpClient, TFtpClient class>
  <link Codebot.Networking.Ftp.TRemoteFindData, TRemoteFindData members> }

type
  TRemoteFindData = record
    { Path to the file }
    Path: string;
    { Name of the remote item }
    Name: string;
    { Modified date of the remote item }
    Date: TDateTime;
    { Size in bytes of the remote item }
    Size: LargeWord;
    { Attributes describing the remote item }
    Attributes: TFileSystemAttributes;
  end;

{ TTextEvent where Text is a value being processed }

  TTextEvent = procedure(Sender: TObject; const Text: string) of object;

{ TTransferEvent where Size is the amount available and Sent is the amount send or received }

{ TFtpClient provides access to an ftp client
  Remarks
  This class provides strictly synchronous operations
  See also
  <link Codebot.Networking.Ftp.TFtpClient, TFtpClient members>
  <link topic_networking, Accessing the Internet topic> }

  TFtpClient = class(TObject)
  private
    FCommand: TSocket;
    FHost: string;
    FPort: Word;
    FUserName: string;
    FPassword: string;
    FTransfering: Boolean;
    FPath: string;
    FFindMask: TFileSystemAttributes;
    FFindList: StringArray;
    FFindIndex: Integer;
    FOnCommand: TTextEvent;
    FOnResponse: TTextEvent;
    FOnProgress: TTransmitEvent;

    type
      TResponse = record
      public
        Valid: Boolean;
        Raw: string;
        Code: Integer;
        Message: string;
        function IsFail(Low, High: Integer): Boolean;
        function IsPass(Low, High: Integer): Boolean;
      end;

    function FileModeBinary: Boolean;
    function Passive(out Socket: TSocket): Boolean;
    procedure Send(const S: string; out R: TResponse);
    procedure Recv(out R: TResponse);
    procedure SetConnected(Value: Boolean);
    function GetConnected: Boolean;
  protected
    { Invoke the OnProgress event }
    procedure DoProgress(const Size, Transmitted: LargeWord); virtual;
  public
    { Create a new file transfer object }
    constructor Create;
    destructor Destroy; override;
    { Attempt to open a file transfer connection using the host, port, username, and password }
    function Connect: Boolean;
    { Close any opened connection }
    procedure Disconnect;
    { Cancel any ongoing transfers }
    procedure Cancel;
    { Returns the current remote directory }
    function GetCurrentDir: string;
    { Returns true if a remote directory exists }
    function DirExists(const Dir: string): Boolean;
    { Change to a new current remote directory }
    function ChangeDir(const Dir: string): Boolean;
    { Create a new remote directory }
    function MakeDir(const Dir: string): Boolean;
    { Delete an existing remote directory }
    function RemoveDir(const Dir: string): Boolean;
    { Delete an existing remote file }
    function FileDelete(const FileName: string): Boolean;
    { Returns true if a remote file exists }
    function FileExists(const FileName: string): Boolean;
    { Rename a remote file, works with directories too }
    function FileRename(const OldName, NewName: string): Boolean;
    { Retrieve the size of a remote file }
    function FileSize(const FileName: string): LargeWord;
    { Retrieve the modified date of a remote file }
    function FileDate(const FileName: string): TDateTime;
    { Initiate an file upload to the remote server }
    function FilePut(const LocalFile, RemoteFile: string; Overwrite: Boolean = True): Boolean;
    { Request a file download from the remote server }
    function FileGet(const RemoteFile, LocalFile: string; Overwrite: Boolean = True): Boolean;
    { Initiate an file upload to the remote server }
    function StreamPut(LocalStream: TStream; const RemoteFile: string; Overwrite: Boolean = True): Boolean;
    { Request a stream download from the remote server }
    function StreamGet(const RemoteFile: string; LocalStream: TStream): Boolean;
    { Retrieve a text mode listing files and folders }
    function FileList(const Path: string = ''): string;
    { Initiate a structured listing files and folders with an optional attribute mask }
    function FindFirst(const Path: string; out FindData: TRemoteFindData;
      Allow: TFileSystemAttributes = fsaAny): Boolean;
    { Continue with the next listing started by FindFirst }
    function FindNext(out FindData: TRemoteFindData): Boolean;
    { Returns true when connected to a remote server, otherwise acts like connect and disconnect }
    property Connected: Boolean read GetConnected write SetConnected;
    { The name of the host to resolve when connecting }
    property Host: string read FHost write FHost;
    { The port used for issuing ftp commands, defaults to 21 }
    property Port: Word read FPort write FPort;
    { The username used when connecting, defaults to anonymous }
    property UserName: string read FUserName write FUserName;
    { The password used when connecting, defaults to an email address }
    property Password: string read FPassword write FPassword;
    { An event invoked echoing ftp commands issued by the client }
    property OnCommand: TTextEvent read FOnCommand write FOnCommand;
    { An event invoked when responses are read from the remote server }
    property OnResponse: TTextEvent read FOnResponse write FOnResponse;
    { An event continuously invoked as file transfers occur }
    property OnProgress: TTransmitEvent read FOnProgress write FOnProgress;
  end;

implementation

{ TResponse }

function TFtpClient.TResponse.IsFail(Low, High: Integer): Boolean;
begin
  if not Valid then
    Result := True
  else
    Result := not Code.Between(Low, High);
end;

function TFtpClient.TResponse.IsPass(Low, High: Integer): Boolean;
begin
  Result := Valid and Code.Between(Low, High);
end;

{ TFtpClient }

constructor TFtpClient.Create;
begin
  inherited Create;
  FCommand := TSocket.Create;
  FHost := 'localhost';
  FUserName := 'anonymous';
  FPassword := 'user@email.com';
  FPort := 21;
end;

destructor TFtpClient.Destroy;
begin
  Disconnect;
  FCommand.Free;
  inherited Destroy;
end;

procedure TFtpClient.Send(const S: string; out R: TResponse);
begin
  R.Valid := False;
  R.Code := 0;
  R.Message := '';
  R.Raw := '';
  if not FCommand.Connected then
    Exit;
  if Assigned(FOnCommand) then
    FOnCommand(Self, S);
  FCommand.Write(S + #13#10);
  Recv(R);
end;

function CheckTerminated(S: string): Boolean;
begin
  if S.EndsWith(#10) then
  begin
    S := S.Trim.Split(#10).Last;
    S := S.Split(' ').First;
    Result := (S.Length = 3) and StrToIntDef(S, 0).Between(100, 600);
  end;
end;

procedure TFtpClient.Recv(out R: TResponse);
var
  Text, S: string;
begin
  R.Valid := False;
  R.Code := 0;
  R.Message := '';
  R.Raw := '';
  if not FCommand.Connected then
    Exit;
  Text := '';
  while FCommand.Read(S, 3000) > 0 do
  begin
    Text := Text + S;
    if CheckTerminated(Text) then
      Break;
  end;
  if Assigned(FOnResponse) then
    FOnResponse(Self, Text);
  R.Raw := Text;
  R.Message := R.Raw.Trim.AdjustLineBreaks(tlbsCRLF);
  R.Message := R.Message.Split(#13#10).Pop;
  R.Code := StrToIntDef(R.Message.FirstOf(' '), 0);
  R.Valid := R.Code > 0;
  if R.Valid then
    R.Message := R.Message.SecondOf(' ').Trim
  else
    R.Message := '';
end;

function TFtpClient.Connect: Boolean;
var
  R: TResponse;
begin
  Result := False;
  Disconnect;
  if FHost.IsWhitespace or FUserName.IsWhitespace or FPassword.IsWhitespace or (FPort = 0) then
    Exit;
  if FCommand.Connect(FHost, Port) then
    Recv(R)
  else
    Exit;
  if R.IsFail(200, 299) then
  begin
    Disconnect;
    Exit;
  end;
  Send('USER ' + FUserName, R);
  if R.IsFail(200, 399) then
  begin
    Disconnect;
    Exit;
  end;
  Send('PASS ' + FPassword, R);
  if R.IsFail(200, 299) then
  begin
    Disconnect;
    Exit;
  end;
  Result := True;
end;

procedure TFtpClient.Disconnect;
var
  R: TResponse;
begin
  Cancel;
  Send('QUIT', R);
  FCommand.Close;
end;

procedure TFtpClient.Cancel;
var
  R: TResponse;
begin
  if FTransfering then
  begin
    Send('ABOR', R);
    FTransfering := False;
  end;
end;

function TFtpClient.GetCurrentDir: string;
var
  R: TResponse;
begin
  Send('PWD', R);
  if R.IsPass(200, 299) then
  begin
    if R.Message.Contains('"') then
      Result := R.Message.Between('"', '"')
    else if R.Message.Contains('''') then
      Result := R.Message.Between('''', '''')
    else
      Result := R.Message.Trim.FirstOf(' ');
  end
  else
    Result := '';
end;

function TFtpClient.DirExists(const Dir: string): Boolean;
var
  R: TResponse;
  S: string;
begin
  Result := False;
  S := GetCurrentDir;
  if S = '' then
    Exit;
  Send('CWD ' + Dir.Quote, R);
  if R.IsPass(200, 299) then
  begin
    Result := True;
    Send('CWD ' + S.Quote, R);
  end;
end;

function TFtpClient.ChangeDir(const Dir: string): Boolean;
var
  R: TResponse;
begin
  Send('CWD ' + Dir.Quote, R);
  Result := R.IsPass(200, 299);
end;

function TFtpClient.MakeDir(const Dir: string): Boolean;
var
  R: TResponse;
begin
  Send('MKD ' + Dir.Quote, R);
  Result := R.IsPass(200, 299);
end;

function TFtpClient.RemoveDir(const Dir: string): Boolean;
var
  R: TResponse;
begin
  Send('RMD ' + Dir.Quote, R);
  Result := R.IsPass(200, 299);
end;

function TFtpClient.FileDelete(const FileName: string): Boolean;
var
  R: TResponse;
begin
  Send('DELE ' + FileName.Quote, R);
  Result := R.IsPass(200, 299);
end;

function TFtpClient.FileExists(const FileName: string): Boolean;
var
  R: TResponse;
begin
  Send('SIZE ' + FileName.Quote, R);
  Result := R.IsPass(200, 299);
end;

function TFtpClient.FileRename(const OldName, NewName: string): Boolean;
var
  R: TResponse;
begin
  Send('RNFR ' + OldName.Quote, R);
  if R.IsPass(200, 299) then
  begin
    Send('RNTO ' + NewName.Quote, R);
    Result := R.IsPass(200, 299);
  end
  else
    Result := False;
end;

function TFtpClient.FileSize(const FileName: string): LargeWord;
var
  R: TResponse;
begin
  Send('SIZE ' + FileName.Quote, R);
  if R.IsPass(200, 299) then
    Result := StrToQWordDef(R.Message, 0)
  else
    Result := 0;
end;

function TFtpClient.FileDate(const FileName: string): TDateTime;
var
  R: TResponse;
  S: string;
  Year, Month, Day, Hour, Minute, Second: Word;
begin
  Result := 0;
  Send('MDTM ' + FileName.Quote, R);
  if R.IsPass(200, 299) then
  begin
    S := R.Message;
    if S.Length <> 'YYYYMMDDhhmmss'.Length then
      Exit;
    Year := StrToIntDef(S.Copy(1, 4), 1970);
    Month := StrToIntDef(S.Copy(5, 2), 1);
    Day := StrToIntDef(S.Copy(7, 2), 1);
    Hour := StrToIntDef(S.Copy(9, 2), 0);
    Minute := StrToIntDef(S.Copy(11, 2), 0);
    Second := StrToIntDef(S.Copy(13, 2), 0);
    Result := EncodeDate(Year, Month, Day) + EncodeTime(Hour, Minute, Second, 0);
  end;
end;

function TFtpClient.Passive(out Socket: TSocket): Boolean;
var
  Host: string;
  Port: word;
  R: TResponse;
  V: IntArray;
begin
  Socket := nil;
  Result := False;
  Send('PASV', R);
  if R.IsPass(200, 299) then
  begin
    R.Message := R.Message.Between('(', ')');
    V := R.Message.SplitInt(',');
    if V.Length <> 6 then
      Exit;
    Socket := TSocket.Create;
    try
      Host := '%d.%d.%d.%d'.Format([V[0], V[1], V[2], V[3]]);
      Port := V[4] * 256 + V[5];
      Result := Socket.Connect(Host, Port);
    finally
      if not Result then
      begin
        Socket.Free;
        Socket := nil;
      end;
    end;
  end;
end;

function TFtpClient.FileModeBinary: Boolean;
var
  R: TResponse;
begin
  Send('TYPE I', R);
  Result := R.IsPass(200, 299);
end;

procedure TFtpClient.DoProgress(const Size, Transmitted: LargeWord);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, Size, Transmitted);
end;

function TFtpClient.FilePut(const LocalFile, RemoteFile: string; Overwrite: Boolean = True): Boolean;
const
  BufferSize = 1024 * 1024;
var
  Socket: TSocket;
  Stream: TStream;
  Buffer: Pointer;
  SourceSize, DestSize: LargeWord;
  Count: LongWord;
  R: TResponse;
begin
  Result := False;
  if not Codebot.System.FileExists(LocalFile) then
    Exit;
  if (not Overwrite) and FileExists(RemoteFile) then
    Exit;
  if not FileModeBinary then
    Exit;
  SourceSize := Codebot.System.FileSize(LocalFile);
  DestSize := 0;
  if Passive(Socket) then
  try
    Send('STOR ' + RemoteFile.Quote, R);
    if R.IsFail(100, 299) then
      Exit;
    Stream := TFileStream.Create(LocalFile, fmOpenRead);
    GetMem(Buffer, BufferSize);
    FTransfering := True;
    try
      repeat
        Count := Stream.Read(Buffer^, BufferSize);
        if Count > 0 then
          if Socket.WriteAll(Buffer^, Count) then
          begin
            DestSize := DestSize + Count;
            DoProgress(SourceSize, DestSize);
          end;
      until (not FTransfering) or (Count < BufferSize);
      Result := DestSize = SourceSize;
    finally
      FTransfering := False;
      FreeMem(Buffer);
      Stream.Free;
    end;
    Recv(R);
  finally
    Socket.Free;
  end;
end;

function TFtpClient.FileGet(const RemoteFile, LocalFile: string; Overwrite: Boolean = True): Boolean;
const
  BufferSize = 1024 * 1024;
var
  Socket: TSocket;
  Stream: TStream;
  Buffer: Pointer;
  SourceSize, DestSize: LargeWord;
  Count: LongInt;
  R: TResponse;
begin
  Result := False;
  if (not Overwrite) and Codebot.System.FileExists(LocalFile) then
    Exit;
  if not FileModeBinary then
    Exit;
  SourceSize := FileSize(RemoteFile);
  DestSize := 0;
  if Passive(Socket) then
  try
    Send('RETR ' + RemoteFile.Quote, R);
    if R.IsFail(100, 299) then
      Exit;
    Stream := TFileStream.Create(LocalFile, fmCreate);
    GetMem(Buffer, BufferSize);
    FTransfering := True;
    try
      repeat
        Count := Socket.Read(Buffer^, BufferSize);
        if Count > 0 then
          if Stream.Write(Buffer^, Count) = Count then
          begin
            DestSize := DestSize + Count;
            DoProgress(SourceSize, DestSize);
          end;
      until (not FTransfering) or (Count < 1);
      Result := DestSize = SourceSize;
    finally
      FTransfering := False;
      FreeMem(Buffer);
      Stream.Free;
    end;
    Recv(R);
  finally
    Socket.Free;
  end;
end;

function TFtpClient.StreamPut(LocalStream: TStream; const RemoteFile: string; Overwrite: Boolean = True): Boolean;
const
  BufferSize = 1024 * 1024;
var
  Socket: TSocket;
  Buffer: Pointer;
  SourceSize, DestSize: LargeWord;
  Count: LongWord;
  R: TResponse;
begin
  Result := False;
  if (not Overwrite) and FileExists(RemoteFile) then
    Exit;
  if not FileModeBinary then
    Exit;
  SourceSize := LocalStream.Size;
  DestSize := 0;
  if Passive(Socket) then
  try
    Send('STOR ' + RemoteFile.Quote, R);
    if R.IsFail(100, 299) then
      Exit;
    GetMem(Buffer, BufferSize);
    FTransfering := True;
    try
      repeat
        Count := LocalStream.Read(Buffer^, BufferSize);
        if Count > 0 then
          if Socket.WriteAll(Buffer^, Count) then
          begin
            DestSize := DestSize + Count;
            DoProgress(SourceSize, DestSize);
          end;
      until (not FTransfering) or (Count < BufferSize);
      Result := DestSize = SourceSize;
    finally
      FTransfering := False;
      FreeMem(Buffer);
    end;
    Recv(R);
  finally
    Socket.Free;
  end;
end;

function TFtpClient.StreamGet(const RemoteFile: string; LocalStream: TStream): Boolean;
const
  BufferSize = 1024 * 1024;
var
  Socket: TSocket;
  Buffer: Pointer;
  SourceSize, DestSize: LargeWord;
  Count: LongInt;
  R: TResponse;
begin
  Result := False;
  if not FileModeBinary then
    Exit;
  SourceSize := FileSize(RemoteFile);
  DestSize := 0;
  if Passive(Socket) then
  try
    Send('RETR ' + RemoteFile.Quote, R);
    if R.IsFail(100, 299) then
      Exit;
    GetMem(Buffer, BufferSize);
    FTransfering := True;
    try
      repeat
        Count := Socket.Read(Buffer^, BufferSize);
        if Count > 0 then
          if LocalStream.Write(Buffer^, Count) = Count then
          begin
            DestSize := DestSize + Count;
            DoProgress(SourceSize, DestSize);
          end;
      until (not FTransfering) or (Count < 1);
      Result := DestSize = SourceSize;
    finally
      FTransfering := False;
      FreeMem(Buffer);
    end;
    Recv(R);
  finally
    Socket.Free;
  end;
end;

function TFtpClient.FileList(const Path: string = ''): string;
var
  Socket: TSocket;
  R: TResponse;
  S: string;
begin
  Result := '';
  if Passive(Socket) then
  try
    if Path.IsWhitespace then
      Send('LIST', R)
    else
      Send('LIST ' + Path.Quote, R);
    if R.IsPass(150, 299) then
    begin
      while Socket.Read(S) > 0 do
        Result := Result + S;
    end;
    if R.IsPass(150, 199) then
      Recv(R);
  finally
    Socket.Free;
  end;
end;

function TFtpClient.FindFirst(const Path: string; out FindData: TRemoteFindData;
  Allow: TFileSystemAttributes = fsaAny): Boolean;
var
  S: string;
begin
  S := FileList(Path).Trim;
  if S.IsEmpty then
  begin
    FindData.Path := '';
    FindData.Name := '';
    FindData.Date := 0;
    FindData.Size := 0;
    FindData.Attributes := [];
    Result := False;
  end
  else
  begin
    FPath := Path;
    S := S.AdjustLineBreaks(tlbsCRLF);
    FFindMask := Allow;
    FFindList := S.Split(#13#10);
    FFindIndex := -1;
    Result := FindNext(FindData);
  end;
end;

function TFtpClient.FindNext(out FindData: TRemoteFindData): Boolean;

  function SafeRead(var Columns: StringArray; Index: Integer): string;
  var
    I: Integer;
  begin
    I := Columns.Length;
    if Index < I then
      Result := Columns[Index]
    else
      Result := '';
  end;

const
  AttributeColumn = 0;
  SizeColumn = 4;
  MonthColumn = 5;
  DayColumn = 6;
  YearColumn = 7;
  FileColumn = 8;
var
  Columns: StringArray;
  Coded: Boolean;
  S: string;
  Y, M, D: Word;
  T: Double;
  I: Integer;
begin
  Result := True;
  FindData.Name := '';
  FindData.Date := 0;
  FindData.Size := 0;
  FindData.Attributes := [];
  Inc(FFindIndex);
  if FFindIndex < FFindList.Length then
  begin
    Columns := FFindList[FFindIndex].Words(FileColumn);
    S := SafeRead(Columns, AttributeColumn);
    if S.Length >= 10 then
    begin
      if S[1] = 'd' then
        Include(FindData.Attributes, fsaDirectory);
      if S[1] = 'l' then
        Include(FindData.Attributes, fsaLink);
      if S[8] = 'r' then
        Include(FindData.Attributes, fsaRead);
      if S[9] = 'w' then
        Include(FindData.Attributes, fsaWrite);
      if S[10] = 'x' then
        Include(FindData.Attributes, fsaExecute);
    end;
    if FindData.Attributes * FFindMask = [] then
    begin
      Result := FindNext(FindData);
      Exit;
    end;
    FindData.Path := FPath;
    FindData.Name := SafeRead(Columns, FileColumn);
    FindData.Size := StrToQWordDef(SafeRead(Columns, SizeColumn), 0);
    M := 1;
    for I := Low(FormatSettings.ShortMonthNames) to High(FormatSettings.ShortMonthNames) do
      if SafeRead(Columns, MonthColumn).Equals(FormatSettings.ShortMonthNames[I], True) then
      begin
        M := I;
        Break;
      end;
    D := StrToIntDef(SafeRead(Columns, DayColumn), 1);
    S := SafeRead(Columns, YearColumn);
    Coded := S.Contains(':');
    if Coded then
    begin
      Y := Now.Year;
      T := StrToTime(S + ':00');
    end
    else
    begin
      Y := StrToIntDef(S, Now.Year);
      T := 0;
    end;
    FindData.Date := EncodeDate(Y, M, D) + T;
    if Coded and (FindData.Date > Now + 1) then
      FindData.Date := EncodeDate(Y - 1, M, D) + T;
    Result := True;
  end
  else
    Result := False;
end;

procedure TFtpClient.SetConnected(Value: Boolean);
begin
  if Value <> FCommand.Connected then
  begin
    if Value then
      Connect
    else
      Disconnect;
  end;
end;

function TFtpClient.GetConnected: Boolean;
begin
  Result := FCommand.Connected;
end;

end.

