program follow;

{$mode delphi}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, Forms, Main,
  Codebot.System, Codebot.Unique;

{$R *.res}

type
  TProgram = class
  private
    function HasSwitches: Boolean;
    procedure HandleMessage(const Message: string);
	public
    procedure Run;
  end;

const
  SwitchNames: array[0..4] of string = (
    'color', 'opacity', 'pen', 'size', 'stop');

function TProgram.HasSwitches: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := Low(SwitchNames) to High(SwitchNames) do
  	Result := Result or SwitchExists(SwitchNames[I]);
	if not Result then
  begin
    WriteLn('Follow the cursor usage:');
    WriteLn('  follow [-color c] [-opacity f] [-pen f] [-size i] [-stop]');
    WriteLn('    color      with c a color name or value');
    WriteLn('    opacity    with f an opacity value between 0.1 and 1.0');
    WriteLn('    pen        with f a pen width making the follower hollow');
    WriteLn('    size       with i an integer denoting window size');
    WriteLn('    stop       terminate the follow cursor program');
  end;
end;

procedure TProgram.HandleMessage(const Message: string);
begin
  FollowForm.HandleCommand(Message);
end;

procedure TProgram.Run;
const
  Name = 'follow-cursor';
var
  S: string;
  I: Integer;
begin
  UniqueInstance(Name);
  if not HasSwitches then
  	Exit;
	if UniqueInstance.Original then
	begin
    if SwitchExists('stop') then
			WriteLn('The follow cursor application was already stopped')
		else
		begin
      UniqueInstance.OnMessage := HandleMessage;
      RequireDerivedFormResource := True;
      Application.Scaled := True;
      Application.Initialize;
      Application.CreateForm(TFollowForm, FollowForm);
      Application.Run;
		end;
  end
  else
  begin
    if SwitchExists('stop') then
    begin
      UniqueInstance.SendMessage('stop');
			WriteLn('The follow cursor application is stopping')
		end
    else for S in SwitchNames do
      if SwitchExists(S) then
	      UniqueInstance.SendMessage(S + ' ' + SwitchValue(S))
  end;
end;

begin
  with TProgram.Create do
	try
		Run;
  finally
    Free;
  end;
end.

