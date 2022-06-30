unit ColorSensor;

{$mode delphi}

interface

uses
  Classes, Graphics, Controls, ExtCtrls,
  Codebot.System,
  Codebot.Graphics.Types,
  Codebot.Controls.Extras,
  Codebot.Networking,
  Codebot.Text.Xml;

{ TColorSensor }

type
  TSensorGain = (Gain1, Gain4, Gain16, Gain60);

  TColorErrorEvent = procedure(Sender: TObject; const ErrorMsg: string) of object;
  TColorReadEvent = procedure(Sender: TObject; const Color: TColorF) of object;

  TColorSensor = class(TComponent)
  private
    FIsCapturing: Boolean;
    FOutput: TShape;
    FProgress: TIndeterminateProgress;
    FServer: string;
    FSuccess: Boolean;
    FGain: TSensorGain;
    FIntegrationTime: Single;
    FErrorMsg: string;
    FColor: TColorF;
    FOnError: TColorErrorEvent;
    FOnRead: TColorReadEvent;
    procedure SetIsCapturing(Value: Boolean);
    procedure SetOutput(Value: TShape);
    procedure SetProgress(Value: TIndeterminateProgress);
    procedure SyncError;
    procedure SyncRead;
    procedure SetIntegrationTime(Value: Single);
    procedure UpdateControls;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Execute(Thread: TSimpleThread);
    procedure DoError(const ErrorMsg: string); virtual;
    procedure DoRead(const Color: TColorF); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Capture;
  published
    property Output: TShape read FOutput write SetOutput;
    property Progress: TIndeterminateProgress read FProgress write SetProgress;
    property IsCapturing: Boolean read FIsCapturing write SetIsCapturing;
    property Server: string read FServer write FServer;
    property Gain: TSensorGain read FGain write FGain;
    property IntegrationTime: Single read FIntegrationTime write SetIntegrationTime;
    property OnError: TColorErrorEvent read FOnError write FOnError;
    property OnRead: TColorReadEvent read FOnRead write FOnRead;
  end;

implementation

{ TColorSensor }

constructor TColorSensor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  IntegrationTime := 100;
  Gain := Gain4;
end;

destructor TColorSensor.Destroy;
begin
  Output := nil;
  inherited Destroy;
end;

procedure TColorSensor.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FOutput then
      FOutput := nil
    else if AComponent = FProgress then
      FProgress := nil;
  end;
end;

procedure TColorSensor.SyncError;
begin
  FIsCapturing := False;
  FSuccess := False;
  UpdateControls;
  DoError(FErrorMsg);
end;

procedure TColorSensor.SyncRead;
begin
  FIsCapturing := False;
  FSuccess := True;
  UpdateControls;
  DoRead(FColor);
end;

procedure TColorSensor.DoError(const ErrorMsg: string);
begin
  if Assigned(FOnError) then
    FOnError(Self, ErrorMsg);
  FIsCapturing := False;
end;

procedure TColorSensor.DoRead(const Color: TColorF);
begin
  if Assigned(FOnRead) then
    FOnRead(Self, Color);
  FIsCapturing := False;
end;

procedure TColorSensor.Execute(Thread: TSimpleThread);
const
  SensorPort = 7050;
  GainValues: array[TSensorGain] of string = ('1', '4', '16', '60');
var
  Client: TSocket;
  Values: StringArray;
  S: string;
begin
  Client := TSocket.Create;
  try
    if not Client.Connect(FServer, SensorPort) then
    begin
      FErrorMsg := 'Could not connect to server: ' + FServer;
      Thread.Synchronize(SyncError);
      Exit;
    end;
    Client.Write('[color] ' + FloatToStr(FIntegrationTime)  + ' ' +  GainValues[FGain]);
    Client.Read(S);
    if S = '' then
    begin
      FErrorMsg := 'No response received from sensor';
      Thread.Synchronize(SyncError);
      Exit;
    end;
    if not S.BeginsWith('[r g b]') then
    begin
      FErrorMsg := 'Unknown response from sensor';
      Thread.Synchronize(SyncError);
      Exit;
    end;
    S := S.Replace('[r g b]', '').Trim;
    Values := S.Split(' ');
    if Values.Length <> 3 then
    begin
      FErrorMsg := 'No values received from sensor';
      Thread.Synchronize(SyncError);
      Exit;
    end;
    FColor.Red := StrToFloatDef(Values[0], -1);
    FColor.Green := StrToFloatDef(Values[1], -1);
    FColor.Blue := StrToFloatDef(Values[2] , -1);
    FColor.Alpha := 1;
    if (FColor.Red < 0) or (FColor.Green < 0) or (FColor.Blue < 0) then
    begin
      FErrorMsg := 'Invalid color values received from sensor: ' + S;
      Thread.Synchronize(SyncError);
      Exit;
    end;
    if (FColor.Red > 1) or (FColor.Green > 1) or (FColor.Blue > 1) then
    begin
      FErrorMsg := 'Invalid color values received from sensor: ' + S;
      Thread.Synchronize(SyncError);
      Exit;
    end;
    Thread.Synchronize(SyncRead);
  finally
    Client.Free;
  end;
end;

procedure TColorSensor.Capture;
begin
  if FIsCapturing then
  begin
    DoError('Capture already in progress');
    Exit;
  end;
  FSuccess := False;
  FIsCapturing := True;
  UpdateControls;
  TSimpleThread.Create(Execute);
end;

procedure TColorSensor.SetIntegrationTime(Value: Single);
const
  MinTime = 2.4;
  MaxTime = 614.4;
begin
  if Value < MinTime then
    Value := MinTime
  else if Value > MaxTime then
    Value := MaxTime;
  FIntegrationTime := Value;
end;

procedure TColorSensor.SetOutput(Value: TShape);
begin
  if FOutput <> Value then
  begin
    if FOutput <> nil then
      FOutput.RemoveFreeNotification(Self);
    FOutput := Value;
    if FOutput <> nil then
      FOutput.FreeNotification(Self);
    UpdateControls;
  end;
end;

procedure TColorSensor.SetProgress(Value: TIndeterminateProgress);
begin
  if FProgress <> Value then
  begin
    if FProgress <> nil then
      FProgress.RemoveFreeNotification(Self);
    FProgress := Value;
    if FProgress <> nil then
      FProgress.FreeNotification(Self);
    UpdateControls;
  end;
end;

procedure TColorSensor.SetIsCapturing(Value: Boolean);
begin
  if not FIsCapturing then
    if Value then
      Capture;
end;

procedure TColorSensor.UpdateControls;
begin
  if FIsCapturing then
  begin
    if FOutput <> nil then
    begin
      FOutput.Brush.Style := bsClear;
      FOutput.Visible := False;
    end;
    if FProgress <> nil then
    begin
      FProgress.Status := psBusy;
      FProgress.Caption := 'Retrieving color data from ' + FServer;
    end;
  end
  else if FErrorMsg <> '' then
  begin
    if FOutput <> nil then
    begin
      FOutput.Brush.Style := bsClear;
      FOutput.Visible := False;
    end;
    if FProgress <> nil then
    begin
      FProgress.Status := psError;
      FProgress.Caption := FErrorMsg;
    end;
  end
  else if FSuccess then
  begin
    if FOutput <> nil then
    begin
      FOutput.Brush.Style := bsSolid;
      FOutput.Brush.Color := FColor.Color;
      FOutput.Visible := True;
    end;
    if FProgress <> nil then
    begin
      FProgress.Status := psReady;
      FProgress.Caption := 'Successfully retrieved color data';
    end;
  end
  else
  begin
    if FOutput <> nil then
    begin
      FOutput.Brush.Style := bsClear;
      FOutput.Visible := False;
    end;
    if FProgress <> nil then
    begin
      FProgress.Status := psReady;
      FProgress.Caption := 'Ready to retrieve color data';
    end;
  end;
end;

end.
