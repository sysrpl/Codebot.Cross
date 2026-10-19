(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.binding.txt> }
unit Codebot.Binding;

{$i codebot.inc}

interface

uses
  Classes, SysUtils;

{ IComponentBinding is implemented by TComponentBinding. A bound component
  calls Change to pass a change on to the binding }

type
  IComponentBinding = interface
    ['{290139B2-AEF0-443A-82AD-7FB8057339CD}']
    function GetInstance: TObject;
    procedure Change(Sender: TObject; var Param);
    { The binding object }
    property Instance: TObject read GetInstance;
  end;

{ IComponentBindingClient is implemented by components which keep a reference
  to the binding they are added to }

  IComponentBindingClient = interface
    ['{BE39BB2B-E3FC-4F47-AAFD-4E4A4529B210}']
    function GetBinding: IComponentBinding;
    procedure SetBinding(Value: IComponentBinding);
    { The binding the component belongs to }
    property Binding: IComponentBinding read GetBinding write SetBinding;
  end;

{ TComponentBinding links components so that a change in one of them can be
  passed on to the others. Descendants decide which classes are accepted
  using AcceptClass and what a change does by overriding DoChange. Bound
  components are stored by name and must have the same owner as the binding }

  TComponentBinding = class(TComponent, IComponentBinding)
  private
    FClasses: TList;
    FComponents: TList;
    FNames: TStrings;
    FChanging: Boolean;
    FUpdateRef: Integer;
    procedure HandleRefresh;
    procedure ReadData(Reader: TReader);
    procedure WriteData(Writer: TWriter);
    function GetNotifyComponent(Index: Integer): TComponent;
    function GetNotifyCount: Integer;
  protected
    { IComponentBinding }
    function GetInstance: TObject;
    procedure Change(Sender: TObject; var Param);
    { TComponentBinding }
    procedure DefineProperties(Filer: TFiler); override;
    procedure Loaded; override;
    { Refresh is invoked after components are added or removed }
    procedure Refresh; virtual;
    { Accept components of a class or its descendants }
    procedure AcceptClass(AClass: TClass); overload;
    { Accept components of several classes or their descendants }
    procedure AcceptClass(const AClassList: array of TClass); overload;
    { DoChange is invoked when a bound component calls Change }
    procedure DoChange(Sender: TObject; var Param); virtual;
    { DoAdd is invoked after a component is added }
    procedure DoAdd(Component: TComponent); virtual;
    { DoRemove is invoked after a component is removed }
    procedure DoRemove(Component: TComponent); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    { The bound components }
    property NotifyComponent[Index: Integer]: TComponent read GetNotifyComponent;
    { The number of bound components }
    property NotifyCount: Integer read GetNotifyCount;
  public
    { Create a new binding }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    { Copy the bound components of another binding of the same class }
    procedure Assign(Source: TPersistent); override;
    { Prevent refreshing until EndUpdate is called }
    procedure BeginUpdate;
    { Allow refreshing and refresh if no updates are pending }
    procedure EndUpdate;
    { Add a component if it is accepted }
    procedure Add(Component: TComponent);
    { Remove a component }
    procedure Remove(Component: TComponent);
    { Remove all components }
    procedure Clear;
    { Returns true if a component can be added }
    function Accept(Component: TComponent): Boolean;
    { Returns true if a component has been added }
    function Contains(Component: TComponent): Boolean;
  end;

  TComponentBindingClass = class of TComponentBinding;

implementation

{ TComponentBinding }

constructor TComponentBinding.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FClasses := TList.Create;
  FComponents := TList.Create;
  FNames := TStringList.Create;
end;

destructor TComponentBinding.Destroy;
begin
  Clear;
  FNames.Free;
  FComponents.Free;
  FClasses.Free;
  inherited Destroy;
end;

procedure TComponentBinding.BeforeDestruction;
begin
  inherited BeforeDestruction;
  { Remove the components before any destructor runs, so DoRemove of
    descendants is invoked on a complete object }
  Clear;
end;

procedure TComponentBinding.HandleRefresh;
begin
  if ComponentState * [csLoading, csDestroying] <> [] then Exit;
  if FUpdateRef < 1 then Refresh;
end;

procedure TComponentBinding.Assign(Source: TPersistent);
var
  B: TComponentBinding;
  I: Integer;
begin
  if (Source <> nil) and (Source.ClassType = ClassType) then
  begin
    B := TComponentBinding(Source);
    BeginUpdate;
    try
      Clear;
      for I := 0 to B.NotifyCount - 1 do
        Add(B.NotifyComponent[I]);
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TComponentBinding.BeginUpdate;
begin
  Inc(FUpdateRef);
end;

procedure TComponentBinding.EndUpdate;
begin
  Dec(FUpdateRef);
  HandleRefresh;
end;

procedure TComponentBinding.ReadData(Reader: TReader);
begin
  Reader.ReadListBegin;
  FNames.Clear;
  while not Reader.EndOfList do
    FNames.Add(Reader.ReadString);
  Reader.ReadListEnd;
end;

procedure TComponentBinding.WriteData(Writer: TWriter);
var
  I: Integer;
begin
  Writer.WriteListBegin;
  for I := 0 to FComponents.Count - 1 do
    Writer.WriteString(TComponent(FComponents[I]).Name);
  Writer.WriteListEnd;
end;

procedure TComponentBinding.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := True
    else
      Result := FComponents.Count > 0;
  end;

begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Bindings', ReadData, WriteData, DoWrite);
end;

procedure TComponentBinding.Loaded;
var
  I: Integer;
begin
  inherited Loaded;
  BeginUpdate;
  try
    if Owner <> nil then
      for I := 0 to FNames.Count - 1 do
        Add(Owner.FindComponent(FNames[I]));
  finally
    EndUpdate;
  end;
  FNames.Clear;
end;

procedure TComponentBinding.Refresh;
begin
end;

procedure TComponentBinding.Add(Component: TComponent);
var
  Client: IComponentBindingClient;
begin
  if Accept(Component) then
    if not Contains(Component) then
    begin
      FComponents.Add(Component);
      Component.FreeNotification(Self);
      if Supports(Component, IComponentBindingClient, Client) then
        Client.Binding := Self;
      DoAdd(Component);
      HandleRefresh;
    end;
end;

procedure TComponentBinding.Remove(Component: TComponent);
var
  Client: IComponentBindingClient;
begin
  if Contains(Component) then
  begin
    FComponents.Remove(Component);
    Component.RemoveFreeNotification(Self);
    if Supports(Component, IComponentBindingClient, Client) then
      if (Client.Binding <> nil) and (Client.Binding.Instance = Self) then
        Client.Binding := nil;
    DoRemove(Component);
    HandleRefresh;
  end;
end;

procedure TComponentBinding.Clear;
begin
  if NotifyCount = 0 then Exit;
  BeginUpdate;
  try
    while NotifyCount > 0 do
      Remove(NotifyComponent[0]);
  finally
    EndUpdate;
  end;
end;

function TComponentBinding.Accept(Component: TComponent): Boolean;
var
  I: Integer;
begin
  Result := False;
  if (Component = nil) or (Component.Name = '') then Exit;
  for I := 0 to FClasses.Count - 1 do
  begin
    Result := Component.InheritsFrom(TClass(FClasses[I]));
    if Result then Exit;
  end;
end;

function TComponentBinding.Contains(Component: TComponent): Boolean;
begin
  Result := FComponents.IndexOf(Component) > -1;
end;

procedure TComponentBinding.AcceptClass(AClass: TClass);
begin
  if FClasses.IndexOf(AClass) < 0 then
    FClasses.Add(AClass);
end;

procedure TComponentBinding.AcceptClass(const AClassList: array of TClass);
var
  I: Integer;
begin
  for I := Low(AClassList) to High(AClassList) do
    AcceptClass(AClassList[I]);
end;

procedure TComponentBinding.DoChange(Sender: TObject; var Param);
begin
end;

procedure TComponentBinding.DoAdd(Component: TComponent);
begin
end;

procedure TComponentBinding.DoRemove(Component: TComponent);
begin
end;

procedure TComponentBinding.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    Remove(AComponent);
end;

function TComponentBinding.GetNotifyComponent(Index: Integer): TComponent;
begin
  Result := TComponent(FComponents[Index]);
end;

function TComponentBinding.GetNotifyCount: Integer;
begin
  Result := FComponents.Count;
end;

{ TComponentBinding.IComponentBinding }

function TComponentBinding.GetInstance: TObject;
begin
  Result := Self;
end;

procedure TComponentBinding.Change(Sender: TObject; var Param);
begin
  if csLoading in ComponentState then Exit;
  if not FChanging then
  begin
    FChanging := True;
    try
      DoChange(Sender, Param);
    finally
      FChanging := False;
    end;
  end;
end;

end.

