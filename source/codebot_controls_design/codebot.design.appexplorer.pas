(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ Codebot.Design.AppExplorer is the Lazarus Application Explorer, a utility to
  browse the hierarchy of components of the IDE or of your applications. It
  is added to the IDE menu by Codebot.Design.Registration, and can be shown in
  a program of your own by calling ShowAppExplorer. }

unit Codebot.Design.AppExplorer;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls, Buttons, RTTIGrids, TypInfo, FGL;

{ TAppExplorerForm is a form that presents a tree of all component
  instances recursively owned by the Application variable.

  The tree holds components which can be destroyed at any time. The form
  asks each one to tell it when it is destroyed, and then removes its node,
  so a node never refers to a component which is gone. A node of a destroyed
  component which still has nodes under it is kept and marked as destroyed. }

type
  { A sorted map from a component to its tree node }
  TComponentNodes = TFPGMap<Pointer, Pointer>;

  TAppExplorerForm = class(TForm)
    ButtonImages: TImageList;
    ComponentEdit: TEdit;
    ComponentPanel: TPanel;
    EditSheet: TTabSheet;
    ComponentMemo: TMemo;
    PageControl: TPageControl;
    ComponentGrid: TTIPropertyGrid;
    SearchEdit: TEdit;
    SearchLabel: TLabel;
    Panel: TPanel;
    PriorButton: TSpeedButton;
    NextButton: TSpeedButton;
    ClearButton: TSpeedButton;
    SearchResults: TLabel;
    Splitter: TSplitter;
    FlashTimer: TTimer;
    TextSheet: TTabSheet;
    TreeImages: TImageList;
    RefreshButton: TButton;
    CloseButton: TButton;
    TreeView: TTreeView;
    procedure ClearButtonClick(Sender: TObject);
    procedure FlashTimerTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MoveButtonClick(Sender: TObject);
    procedure SearchEditChange(Sender: TObject);
    procedure SpeedButtonPaint(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure RefreshButtonClick(Sender: TObject);
    procedure TreeViewChange(Sender: TObject; Node: TTreeNode);
    { Flash when a tree node is double clicked }
    procedure TreeViewDblClick(Sender: TObject);
    { Or when the spacebar is pressed }
    procedure TreeViewKeyPress(Sender: TObject; var Key: char);
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  private
    FSearchList: TList;
    FFilterList: TList;
    FSearchTerm: string;
    FSearchIndex: Integer;
    FFlashWindow: THintWindow;
    FNodes: TComponentNodes;
    { The component shown in the property grid and the text }
    FShown: TComponent;
    { True while the tree is being rebuilt or nodes are being removed }
    FRefreshing: Boolean;
    { Stop being told when the components in the tree are destroyed }
    procedure ReleaseNodes;
    { The ComponentFreed method removes the node of a destroyed component }
    procedure ComponentFreed(Component: TComponent);
    { The Flash method breifly positions FFlashWindow over a control }
    procedure Flash(Instance: TControl);
    { The Pack method aligns the search controls at the bottom of the form }
    procedure Pack;
    { The RefreshSelection method repopulates the ComponentMemo text }
    procedure RefreshView;
    { The RefreshView method repopulates the component tree view }
    procedure RefreshSelection(Node: TTreeNode);
  end;

{ The ShowAppExplorer procedure creates and shows an application explorer form }

procedure ShowAppExplorer;

implementation

{$R *.lfm}

var
  InternalAppExplorerForm: TObject;

procedure ShowAppExplorer;
begin
  if InternalAppExplorerForm = nil then
    InternalAppExplorerForm := TAppExplorerForm.Create(Application);
  TForm(InternalAppExplorerForm).Show;
  TForm(InternalAppExplorerForm).BringToFront;
end;

{ TClassImage is used to lookup the image index of a class }

type
  TClassImage = record
    ClassName: string;
    Image: Integer;
  end;

var
  ClassImages: array[0..30] of TClassImage = (
    (ClassName: 'TMENUITEM'; Image: 19),
    (ClassName: 'TMENU'; Image: 19),
    (ClassName: 'TBASICACTION'; Image: 0),
    (ClassName: 'TDATAMODULE'; Image: 1),
    (ClassName: 'TCUSTOMFORM'; Image: 1),
    (ClassName: 'TSCROLLINGWINCONTROL'; Image: 2),
    (ClassName: 'TCUSTOMFRAME'; Image: 2),
    (ClassName: 'TCONTROLSCROLLBAR'; Image: 3),
    (ClassName: 'TSCROLLBAR'; Image: 4),
    (ClassName: 'TCUSTOMSTATUSBAR'; Image: 5),
    (ClassName: 'TCUSTOMTABCONTROL'; Image: 6),
    (ClassName: 'TCUSTOMGROUPBOX'; Image: 7),
    (ClassName: 'TCUSTOMLISTBOX'; Image: 8),
    (ClassName: 'TCUSTOMCOMBOBOX'; Image: 8),
    (ClassName: 'TPROGRESSBAR'; Image: 9),
    (ClassName: 'TCUSTOMCHECKBOX'; Image: 10),
    (ClassName: 'TRADIOBUTTON'; Image: 11),
    (ClassName: 'TBUTTONCONTROL'; Image: 12),
    (ClassName: 'TCUSTOMRICHEDIT'; Image: 14),
    (ClassName: 'TCUSTOMMEMO'; Image: 15),
    (ClassName: 'TCUSTOMPANEL'; Image: 19),
    (ClassName: 'TCUSTOMEDIT'; Image: 16),
    (ClassName: 'TCUSTOMCONTROL'; Image: 18),
    (ClassName: 'TWINCONTROL'; Image: 17),
    (ClassName: 'TCUSTOMLABEL'; Image: 20),
    (ClassName: 'TIMAGE'; Image: 21),
    (ClassName: 'TGRAPHICCONTROL'; Image: 22),
    (ClassName: 'TCUSTOMIMAGELIST'; Image: 23),
    (ClassName: 'TCOMMONDIALOG'; Image: 24),
    (ClassName: 'TCONTROL'; Image: 25),
    (ClassName: 'TCOMPONENT'; Image: 26)
  );

function GetClassImageIndex(Instance: TObject): Integer;
var
  C: TClass;
  S: string;
  I: Integer;
begin
  Result := ClassImages[High(ClassImages)].Image;
  C := Instance.ClassType;
  while C.ClassType <> TComponent.ClassType do
  begin
    S := UpperCase(C.ClassName);
    for I := Low(ClassImages) to High(ClassImages) do
      if S = ClassImages[I].ClassName then
        Exit(ClassImages[I].Image);
    C := C.ClassParent;
  end;
end;

{ TAppExplorerForm }

procedure TAppExplorerForm.FormCreate(Sender: TObject);
const
  Margin = 8;
begin
  FSearchList := TList.Create;
  FFilterList := TList.Create;
  FNodes := TComponentNodes.Create;
  FNodes.Sorted := True;
  ClientWidth := CloseButton.Left + CloseButton.Width + Margin;
  ClientHeight := CloseButton.Top + CloseButton.Height + Margin;
  Panel.Anchors := [akLeft, akTop, akRight, akBottom];
  RefreshButton.Anchors := [akRight, akBottom];
  CloseButton.Anchors := [akRight, akBottom];
  RefreshView;
end;

procedure TAppExplorerForm.FormDestroy(Sender: TObject);
begin
  { The timer which removes the flash window is destroyed with the form }
  FlashTimer.Enabled := False;
  FreeAndNil(FFlashWindow);
  ComponentGrid.TIObject := nil;
  FShown := nil;
  ReleaseNodes;
  FreeAndNil(FNodes);
  FFilterList.Free;
  FSearchList.Free;
  InternalAppExplorerForm := nil;
end;

procedure TAppExplorerForm.ReleaseNodes;
var
  I: Integer;
begin
  if FNodes = nil then
    Exit;
  for I := 0 to FNodes.Count - 1 do
    if TComponent(FNodes.Keys[I]) <> Self then
      TComponent(FNodes.Keys[I]).RemoveFreeNotification(Self);
  FNodes.Clear;
end;

procedure TAppExplorerForm.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FNodes <> nil) and (not FRefreshing) and
    (not (csDestroying in ComponentState)) then
    ComponentFreed(AComponent);
end;

const
  DestroyedText = ' (destroyed)';

procedure TAppExplorerForm.ComponentFreed(Component: TComponent);
var
  Node, Parent: TTreeNode;
  I: Integer;
begin
  I := FNodes.IndexOf(Component);
  if I < 0 then
    Exit;
  Node := TTreeNode(FNodes.Data[I]);
  FNodes.Delete(I);
  Node.Data := nil;
  if (FShown = Component) or (ComponentGrid.TIObject = Component) then
  begin
    FShown := nil;
    RefreshSelection(nil);
  end;
  { Remove the node, and any nodes above it which were kept only because
    they had this one under them. Changes of the selection made by removing
    nodes are handled afterwards. }
  FRefreshing := True;
  try
    while (Node <> nil) and (Node.Data = nil) and (Node.Count = 0) do
    begin
      Parent := Node.Parent;
      FSearchList.Remove(Node);
      FFilterList.Remove(Node);
      Node.Delete;
      Node := Parent;
    end;
    if (Node <> nil) and (Node.Data = nil) and (Pos(DestroyedText, Node.Text) = 0) then
      Node.Text := Node.Text + DestroyedText;
  finally
    FRefreshing := False;
  end;
  { The matches may have changed, so the next search starts again }
  FSearchIndex := -1;
  PriorButton.Enabled := FFilterList.Count > 0;
  NextButton.Enabled := FFilterList.Count > 0;
  if FFilterList.Count = 0 then
    SearchResults.Caption := '';
  if (FShown = nil) and (TreeView.Selected <> nil) and (TreeView.Selected.Data <> nil) then
    RefreshSelection(TreeView.Selected);
end;

procedure TAppExplorerForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TAppExplorerForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TAppExplorerForm.SpeedButtonPaint(Sender: TObject);
var
  Button: TSpeedButton absolute Sender;
  I: Integer;
begin
  I := 1;
  if csClicked in Button.ControlState then
    Inc(I);
  ButtonImages.Draw(Button.Canvas, I, I, Button.Tag, Button.Enabled);
end;

procedure TAppExplorerForm.FormShow(Sender: TObject);
begin
  { Pack is needed OnShow because some control dimensions differ
    based on widgetset, and this size difference is not realized
    until they are first presented }
  Pack;
end;

procedure TAppExplorerForm.MoveButtonClick(Sender: TObject);
var
  Node: TTreeNode;
begin
  if sender = NextButton then
  begin
    FSearchIndex := FSearchIndex + 1;
    FSearchIndex := FSearchIndex mod FFilterList.Count;
  end
  else
  begin
    if FSearchIndex = -1 then
      FSearchIndex := 0;
    FSearchIndex := FSearchIndex - 1;
    if FSearchIndex < 0 then
      FSearchIndex := FFilterList.Count - 1;
  end;
  Node := TTreeNode(FFilterList[FSearchIndex]);
  TreeView.Selected := Node;
  if FFilterList.Count = 1 then
    SearchResults.Caption := 'Match 1 of 1'
  else
    SearchResults.Caption := 'Match ' + IntToStr(FSearchIndex + 1) + ' of ' +
      IntToStr(FFilterList.Count) ;
end;

procedure TAppExplorerForm.ClearButtonClick(Sender: TObject);
begin
  SearchEdit.Text := '';
end;

procedure TAppExplorerForm.SearchEditChange(Sender: TObject);
var
  Node: TTreeNode;
  S: string;
  I: Integer;
begin
  S := Trim(SearchEdit.Text);
  S := UpperCase(S);
  if S = FSearchTerm then
    Exit;
  FSearchTerm := S;
  FSearchIndex := -1;
  FFilterList.Clear;
  for I := 0 to FSearchList.Count - 1 do
  begin
    Node := TTreeNode(FSearchList[I]);
    S := UpperCase(Node.Text);
    if Pos(FSearchTerm, S) > 0 then
      FFilterList.Add(Node);
  end;
  ClearButton.Enabled := FSearchTerm <> '';
  PriorButton.Enabled := FFilterList.Count > 0;
  NextButton.Enabled := FFilterList.Count > 0;
  if FFilterList.Count > 0 then
    MoveButtonClick(NextButton)
  else
    SearchResults.Caption := '';
  { Pack also hides and shows controls based on their enabled state }
  Pack;
end;

procedure TAppExplorerForm.RefreshButtonClick(Sender: TObject);
begin
  RefreshView;
end;

procedure TAppExplorerForm.TreeViewChange(Sender: TObject; Node: TTreeNode);
begin
  { The selection changes as nodes are removed, which is not acted on until
    the tree is whole again }
  if not FRefreshing then
    RefreshSelection(Node);
end;

{ TFlashWindow displays a red highlight over a visible control }

type
  TFlashWindow = class(THintWindow)
  public
    constructor Create(AOwner: TComponent); override;
  end;

constructor TFlashWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color := clRed;
  Width := 100;
  Height := 100;
  AlphaBlend := True;
  { An opacity of 0.25 }
  AlphaBlendValue := 64;
end;

procedure TAppExplorerForm.FlashTimerTimer(Sender: TObject);
begin
  FFlashWindow.Free;
  FFlashWindow := nil;
  FlashTimer.Enabled := False;
end;

procedure TAppExplorerForm.Flash(Instance: TControl);
var
  WinParent: TControl;
  P: TPoint;
  R: TRect;
begin
  FlashTimer.Enabled := False;
  FFlashWindow.Free;
  FFlashWindow := nil;
  if Instance = nil then
    Exit;
  { Do not flash unless the control and all of its parents are visible }
  if not Instance.IsVisible then
    Exit;
  { Nor if there is no window to find where the control is on the screen }
  if Instance is TWinControl then
    WinParent := Instance
  else
    WinParent := Instance.Parent;
  if (WinParent = nil) or (not TWinControl(WinParent).HandleAllocated) then
    Exit;
  P := Instance.ClientToScreen(Point(0, 0));
  R := Instance.BoundsRect;
  R.Right := R.Right - R.Left + P.X;
  R.Bottom := R.Bottom - R.Top + P.Y;
  R.Left := P.X;
  R.Top := P.Y;
  { The window is not owned, so it is not listed in the tree, and it is freed
    by the timer or when the form is destroyed }
  FFlashWindow := TFlashWindow.Create(nil);
  FFlashWindow.BoundsRect := R;
  FFlashWindow.Show;
  FlashTimer.Enabled := True;
end;

procedure TAppExplorerForm.TreeViewDblClick(Sender: TObject);
var
  Key: Char;
begin
  Key := ' ';
  TreeViewKeyPress(TreeView, Key);
end;

procedure TAppExplorerForm.TreeViewKeyPress(Sender: TObject; var Key: char);
var
  Instance: TObject;
  Node: TTreeNode;
begin
  if Key = ' ' then
  begin
    Node := TreeView.Selected;
    if Node = nil then
      Exit;
    Instance := TObject(Node.Data);
    if (Instance <> nil) and (Instance is TControl) then
      Flash(Instance as TControl);
  end;
end;

procedure TAppExplorerForm.Loaded;
begin
  inherited Loaded;
  ComponentGrid.SplitterX := ComponentGrid.Width div 2;
end;

procedure TAppExplorerForm.Pack;

  procedure PackControls(const Controls: array of TControl; Middle: Integer);
  const
    Margin = 8;
  var
    Left: Integer;
    C: TControl;
  begin
    Left := Margin;
    for C in Controls do
    begin
      C.Anchors := [akLeft, akBottom];
      C.Visible := C.Enabled;
      if C.Visible then
      begin
        C.Left := Left;
        Left := Left + C.Width + Margin div 2;
        C.Top := Middle - C.Height div 2;
      end;
    end;
  end;

begin
  PackControls([ClearButton, SearchLabel, SearchEdit, PriorButton, NextButton,
    SearchResults], CloseButton.Top + CloseButton.Height div 2);
end;

{ A component is shown once. A control is shown under its parent, and other
  components under their owner. A control whose parent is not reached from
  the application is shown under its owner instead, after everything else
  has been placed. }

procedure TAppExplorerForm.RefreshView;
var
  Deferred: TList;

  function AttachComponent(Parent: TTreeNode; Component: TComponent): TTreeNode;
  var
    S: string;
  begin
    if Component.Name <> '' then
      S := Component.Name + ': ' + Component.ClassName
    else
      S := Component.ClassName;
    Result := TreeView.Items.AddChild(Parent, S);
    Result.Data := Component;
    Result.ImageIndex := GetClassImageIndex(Component);
    Result.SelectedIndex := Result.ImageIndex;
    FSearchList.Add(Result);
    FNodes.Add(Component, Result);
    if Component <> Self then
      Component.FreeNotification(Self);
  end;

  procedure AddNode(Parent: TTreeNode; Component: TComponent);
  var
    Container: TWinControl;
    SubComponent: TComponent;
    Item: TTreeNode;
    I: Integer;
  begin
    if FNodes.IndexOf(Component) > -1 then
      Exit;
    Item := AttachComponent(Parent, Component);
    if Component is TWinControl then
    begin
      Container := Component as TWinControl;
      for I := 0 to Container.ControlCount - 1 do
        AddNode(Item, Container.Controls[I]);
    end;
    for I := 0 to Component.ComponentCount - 1 do
    begin
      SubComponent := Component.Components[I];
      if FNodes.IndexOf(SubComponent) > -1 then
        Continue;
      if (SubComponent is TControl) and (TControl(SubComponent).Parent <> nil) then
        Deferred.Add(SubComponent)
      else
        AddNode(Item, SubComponent);
    end;
  end;

var
  Component: TComponent;
  Root: TTreeNode;
  I, J: Integer;
begin
  SearchEdit.Text := '';
  Deferred := TList.Create;
  FRefreshing := True;
  TreeView.Items.BeginUpdate;
  try
    ComponentGrid.TIObject := nil;
    FShown := nil;
    ReleaseNodes;
    TreeView.Items.Clear;
    FSearchList.Clear;
    FFilterList.Clear;
    AddNode(nil, Application);
    Root := TreeView.Items.GetFirstNode;
    { Adding a deferred control can defer more, so the count is read each time }
    I := 0;
    while I < Deferred.Count do
    begin
      Component := TComponent(Deferred[I]);
      Inc(I);
      if FNodes.IndexOf(Component) > -1 then
        Continue;
      J := FNodes.IndexOf(Component.Owner);
      if J > -1 then
        AddNode(TTreeNode(FNodes.Data[J]), Component)
      else
        AddNode(Root, Component);
    end;
    if Root <> nil then
      Root.Expand(False);
  finally
    TreeView.Items.EndUpdate;
    FRefreshing := False;
    Deferred.Free;
  end;
  RefreshSelection(nil);
end;

procedure TAppExplorerForm.RefreshSelection(Node: TTreeNode);

  procedure AddClassInfo(Instance: TComponent);
  var
    TypeInfo: PTypeInfo;
    TypeData: PTypeData;
    S: string;
  begin
    TypeInfo := PTypeInfo(Instance.ClassInfo);
    TypeData := GetTypeData(TypeInfo);
    S := TypeData.ClassType.ClassName + ' declared in ' + TypeData.UnitName;
    ComponentMemo.Lines.Add(S);
    ComponentMemo.Lines.Add('');
  end;

  procedure AddParentOwner(Instance: TComponent);

    function Name: string;
    begin
      if Instance.Name = '' then
        Result := Instance.ClassName
      else
        Result := Instance.ClassName + '(' + Instance.Name + ')';
      Result := '  ' + Result;
    end;

  var
    Parent: TWinControl;
    S: string;
  begin
    ComponentMemo.Lines.Add('Hierarchy:');
    S := Name;
    while Instance <> nil do
    begin
      if Instance is TControl then
        Parent := (Instance as TControl).Parent
      else
        Parent := nil;
      if Parent <> nil then
        Instance := Parent
      else
        Instance := Instance.Owner;
      if Instance = nil then
        Break;
      ComponentMemo.Lines.Add(S);
      S := Name;
    end;
    ComponentMemo.Lines.Add(S);
    ComponentMemo.Lines.Add('');
  end;

  procedure AddInheritance(Instance: TComponent);
  var
    S: string;
    C: TClass;
  begin
    ComponentMemo.Lines.Add('ComponentState:');
    S := SetToString(PTypeInfo(TypeInfo(TComponentState)), Integer(Instance.ComponentState), True);
    ComponentMemo.Lines.Add(S);
    ComponentMemo.Lines.Add('');
    ComponentMemo.Lines.Add('Inheritance:');
    C := Instance.ClassType;
    while C <> nil do
    begin
      ComponentMemo.Lines.Add('  ' + C.ClassName);
      C := C.ClassParent;
    end;
    ComponentMemo.Lines.Add('');
  end;

  procedure AddPropertyList(Instance: TComponent);
  var
    Info: PTypeInfo;
    Data: PTypeData;
    List: PPropList;
    Prop: PPropInfo;
    StrProp: string;
    IntProp: Int64;
    FloatProp: Double;
    ObjProp: IntPtr;
    S: string;
    I: Integer;
  begin
    ComponentMemo.Lines.Add('Properties:');
    Info := PTypeInfo(Instance.ClassInfo);
    Data := GetTypeData(Info);
    if Data.PropCount < 1 then
      Exit;
    List := GetMem(Data.PropCount * SizeOf(Pointer));
    try
      GetPropList(Info, tkAny, List);
      for I := 0 to Data.PropCount - 1 do
      begin
        Prop := List^[I];
        S := '';
        { Reading a property can raise an exception, which is shown in place
          of the value }
        try
        case Prop.PropType.Kind of
          tkSString, tkLString, tkAString, tkWString, tkUString:
            begin
              StrProp := GetStrProp(Instance, Prop);
              if StrProp <> '' then
                S := Prop.Name + ': ' + StrProp;
            end;
          tkBool:
            begin
              IntProp := GetOrdProp(Instance, Prop);
              S := Prop.Name + ': ' + BooleanIdents[IntProp <> 0];
            end;
          tkChar, tkWChar, tkUChar:
            begin
              IntProp := GetOrdProp(Instance, Prop);
              if IntProp <= Ord(' ') then
                S := Prop.Name + ': #' + IntToStr(IntProp)
              else
                S := Prop.Name + ': ' + Chr(IntProp);
            end;
          tkInt64, tkQWord:
            begin
              IntProp := GetInt64Prop(Instance, Prop);
              if Prop.PropType.Kind = tkQWord then
                S := Prop.Name + ': ' + IntToStr(QWord(IntProp))
              else
                S := Prop.Name + ': ' + IntToStr(IntProp);
            end;
          tkInteger:
            begin
              IntProp := GetOrdProp(Instance, Prop);
              if Prop.PropType = TypeInfo(TColor) then
                S := Prop.Name + ': ' + ColorToString(IntProp)
              else if Prop.PropType = TypeInfo(TCursor) then
                S := Prop.Name + ': ' + CursorToString(IntProp)
              else
                S := Prop.Name + ': ' + IntToStr(IntProp);
            end;
          tkEnumeration:
            begin
              IntProp := GetOrdProp(Instance, Prop);
              S := Prop.Name + ': ' + GetEnumName(Prop.PropType, IntProp);
            end;
          tkSet:
            begin
              StrProp := GetSetProp(Instance, Prop, True);
              S := Prop.Name + ': ' + StrProp;
            end;
          tkFloat:
            begin
              FloatProp := GetFloatProp(Instance, Prop);
              if Prop.PropType = TypeInfo(TDateTime) then
                S := Prop.Name + ': ' + DateTimeToStr(FloatProp)
              else
                S := Prop.Name + ': ' + FloatToStr(FloatProp);
            end;
          tkClass:
            begin
              if Copy(Prop.Name, 1, 10) = 'AnchorSide' then
                Continue;
              if Prop.Name = 'BorderSpacing' then
                Continue;
              if Prop.Name = 'Constraints' then
                Continue;
              if Prop.Name = 'ChildSizing' then
                Continue;
              if Prop.Name = 'Font' then
                Continue;
              ObjProp := GetOrdProp(Instance, Prop);
              if ObjProp <> 0 then
                S := Prop.Name + ': (' + TObject(ObjProp).ClassName + ')';
            end
        else
          S := '';
        end;
        except
          on E: Exception do
            S := Prop.Name + ': (' + E.ClassName + ': ' + E.Message + ')';
        end;
        if S <> '' then
          ComponentMemo.Lines.Add('  ' + S);
      end;
      ComponentMemo.Lines.Add('');
    finally
      FreeMem(List);
    end;
  end;

var
  Component: TComponent;
  S: string;
begin
  ComponentMemo.Lines.BeginUpdate;
  try
    ComponentMemo.Lines.Clear;
    ComponentEdit.Text := '';
    ComponentGrid.TIObject := nil;
    FShown := nil;
    if Node = nil then
      Exit;
    Component := TComponent(Node.Data);
    { The node of a destroyed component which still has nodes under it }
    if Component = nil then
    begin
      ComponentEdit.Text := Node.Text;
      Exit;
    end;
    FShown := Component;
    S := Component.Name;
    if S = '' then
      S := '(unnamed)';
    ComponentEdit.Text := S + ': ' + Component.ClassName;
    ComponentGrid.TIObject := nil;
    ComponentGrid.TIObject := Component;
    ComponentGrid.SplitterX := ComponentGrid.Width div 2;
    AddClassInfo(Component);
    AddParentOwner(Component);
    AddInheritance(Component);
    AddPropertyList(Component);
    ComponentMemo.SelStart := 0;
  finally
    ComponentMemo.Lines.EndUpdate;
  end;
end;

end.

