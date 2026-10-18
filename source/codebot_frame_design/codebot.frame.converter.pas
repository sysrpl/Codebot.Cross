(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ Codebot.Frame.Converter converts a form or frame into the source of a control.
  The unit of the form is copied and changed so that the form class becomes a
  TCustomControl, and the layout in the form file is written as code which a
  constructor runs. Properties which cannot be written as code are stored as
  resources. }

unit Codebot.Frame.Converter;

{$mode delphi}

interface

uses
  Classes, SysUtils, TypInfo, Controls, Menus, ActnList,
  Codebot.Frame.ConvertDialog,
  Codebot.Frame.Lfm,
  Codebot.Frame.Source,
  Codebot.Frame.Resources;

{ TConvertMessage is something the user is told about a conversion }

type
  TConvertMessage = record
    { True if the message is a warning, otherwise it is a hint }
    IsWarning: Boolean;
    Text: string;
    { The file and line the message is about, or an empty file name }
    FileName: string;
    Line: Integer;
  end;

  TConvertMessages = array of TConvertMessage;

{ TFrameConverter converts a form or frame of the designer into the source
  of a control }

  TFrameConverter = class
  private
    FRoot: TComponent;
    FFileName: string;
    FOptions: TConvertOptions;
    FLineEnd: string;
    FLfm: TLfmObject;
    FSource: TFormSource;
    FNames: TStringList;
    FFields: TStringList;
    FLayout: TStringList;
    FLater: TStringList;
    FCreateHandler: string;
    FDestroyHandler: string;
    FUnitSource: string;
    FResources: TPropertyResources;
    FMessages: TConvertMessages;
    FComponentUnits: TStringList;
    FUsedUnits: TStringList;
    FError: string;
    function Fail(const Message: string): Boolean;
    procedure AddMessage(IsWarning: Boolean; const Text: string; Line: Integer = 0);
    procedure AddUnit(AClass: TClass);
    procedure GatherNames(Obj: TLfmObject);
    function IsEvent(Instance: TObject; const Path: string): Boolean;
    function HandlerAddress(const Handler: string): string;
    procedure WriteProperty(Instance: TObject; Prop: TLfmProperty;
      const Prefix, Target, Indent: string; Code, Refs: TStrings);
    procedure WriteResource(Prop: TLfmProperty; const Target, Indent: string; Code: TStrings);
    procedure WriteCollection(Instance: TObject; Prop: TLfmProperty;
      const Prefix, Indent: string; Code: TStrings);
    procedure WriteAttach(const Name: string; Live, ParentLive: TComponent;
      const ParentName: string);
    procedure WriteObject(Obj: TLfmObject; const ParentName: string; ParentLive: TComponent);
    procedure WriteRoot;
    procedure WriteUnit;
    procedure CheckCode;
  public
    { Creates a converter for the form or frame Root of the designer, whose
      unit is FileName, using the choices of the convert dialog }
    constructor Create(Root: TComponent; const FileName: string;
      const Options: TConvertOptions);
    destructor Destroy; override;
    { Converts the form or frame. Returns False if it could not be converted,
      with the reason in Error. }
    function Execute: Boolean;
    { The source of the new unit }
    property UnitSource: string read FUnitSource;
    { The properties which are stored in the resource file of the unit }
    property Resources: TPropertyResources read FResources;
    { What the user is told about the conversion }
    property Messages: TConvertMessages read FMessages;
    { The units of the classes of the components, which must be found in an
      installed package }
    property ComponentUnits: TStringList read FComponentUnits;
    { The units in the uses clauses of the new unit }
    property UsedUnits: TStringList read FUsedUnits;
    property Error: string read FError;
  end;

implementation

const
  { The indent of the statements of the layout }
  Body = '    ';

{ Reads a file into a string without changing it }

function ReadFile(const FileName: string): string;
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(Result, Stream.Size);
    if Length(Result) > 0 then
      Stream.ReadBuffer(Result[1], Length(Result));
  finally
    Stream.Free;
  end;
end;

{ Returns the part of a property path before the first dot }

function FirstPart(const Path: string): string;
var
  I: Integer;
begin
  I := Pos('.', Path);
  if I > 0 then
    Result := Copy(Path, 1, I - 1)
  else
    Result := Path;
end;

function InList(const S: string; const List: array of string): Boolean;
var
  Item: string;
begin
  for Item in List do
    if SameText(S, Item) then
      Exit(True);
  Result := False;
end;

{ Finds the property information of a property path such as Font.Style,
  returning nil if it cannot be found. Owner is the object which has the
  last property of the path. }

function PropInfoOf(Instance: TObject; const Path: string; out Owner: TObject): PPropInfo;
var
  Parts: TStringArray;
  I: Integer;
begin
  Result := nil;
  Owner := Instance;
  if Owner = nil then
    Exit;
  Parts := Path.Split(['.']);
  for I := 0 to High(Parts) do
  begin
    Result := GetPropInfo(Owner, Parts[I]);
    if Result = nil then
      Exit;
    if I < High(Parts) then
    begin
      if Result^.PropType^.Kind <> tkClass then
        Exit(nil);
      Owner := GetObjectProp(Owner, Result);
      if Owner = nil then
        Exit(nil);
    end;
  end;
end;

{ TFrameConverter }

constructor TFrameConverter.Create(Root: TComponent; const FileName: string;
  const Options: TConvertOptions);
begin
  inherited Create;
  FRoot := Root;
  FFileName := FileName;
  FOptions := Options;
  FLineEnd := #10;
  FNames := TStringList.Create;
  FFields := TStringList.Create;
  FLayout := TStringList.Create;
  FLater := TStringList.Create;
  FComponentUnits := TStringList.Create;
  FUsedUnits := TStringList.Create;
end;

destructor TFrameConverter.Destroy;
begin
  FUsedUnits.Free;
  FComponentUnits.Free;
  FLater.Free;
  FLayout.Free;
  FFields.Free;
  FNames.Free;
  FSource.Free;
  FLfm.Free;
  inherited Destroy;
end;

function TFrameConverter.Fail(const Message: string): Boolean;
begin
  FError := Message;
  Result := False;
end;

procedure TFrameConverter.AddMessage(IsWarning: Boolean; const Text: string; Line: Integer = 0);
var
  I: Integer;
begin
  I := Length(FMessages);
  SetLength(FMessages, I + 1);
  FMessages[I].IsWarning := IsWarning;
  FMessages[I].Text := Text;
  if Line > 0 then
    FMessages[I].FileName := FFileName;
  FMessages[I].Line := Line;
end;

procedure TFrameConverter.AddUnit(AClass: TClass);
var
  S: string;
begin
  S := AClass.UnitName;
  if (S <> '') and (FComponentUnits.IndexOf(S) < 0) then
    FComponentUnits.Add(S);
end;

{ The names of the components the control creates. The components inside a
  frame placed on the form are created by the frame. }

procedure TFrameConverter.GatherNames(Obj: TLfmObject);
var
  Child: TLfmObject;
  I: Integer;
begin
  for I := 0 to Obj.Children.Count - 1 do
  begin
    Child := TLfmObject(Obj.Children[I]);
    if Child.IsInherited or (Child.Name = '') then
      Continue;
    FNames.Add(Child.Name);
    if not Child.IsInline then
      GatherNames(Child);
  end;
end;

{ An event property has a method type. When the component is not known, a
  property whose name starts with On is taken to be an event. }

function TFrameConverter.IsEvent(Instance: TObject; const Path: string): Boolean;
var
  Info: PPropInfo;
  Owner: TObject;
  S: string;
begin
  Info := PropInfoOf(Instance, Path, Owner);
  if Info <> nil then
    Exit(Info^.PropType^.Kind = tkMethod);
  S := Path;
  while Pos('.', S) > 0 do
    Delete(S, 1, Pos('.', S));
  Result := (Length(S) > 2) and SameText(Copy(S, 1, 2), 'On');
end;

function TFrameConverter.HandlerAddress(const Handler: string): string;
begin
  if FSource.IsDelphiMode then
    Result := Handler
  else
    Result := '@' + Handler;
end;

{ Writes the code which sets the property Prop. Prefix is placed before the
  property name, such as Button1. for a component or nothing for the control
  itself. Target is the object a resource is loaded into. References to other
  components are written to Refs, as they are set once every component has
  been created. }

procedure TFrameConverter.WriteProperty(Instance: TObject; Prop: TLfmProperty;
  const Prefix, Target, Indent: string; Code, Refs: TStrings);
const
  StringsSuffix = '.Strings';
var
  Path, S: string;
begin
  case Prop.Kind of
    lvSimple:
      if Prop.IsIdent and IsEvent(Instance, Prop.Name) then
        Code.Add(Indent + Prefix + Prop.Name + ' := ' + HandlerAddress(Prop.Value) + ';')
      else if Prop.IsIdent and (FNames.IndexOf(Prop.Value) > -1) then
        Refs.Add(Indent + Prefix + Prop.Name + ' := ' + Prop.Value + ';')
      else if Prop.IsIdent and SameText(Prop.Value, FLfm.Name) then
        Refs.Add(Indent + Prefix + Prop.Name + ' := Self;')
      else if Prop.IsIdent and (Pos('.', Prop.Value) > 0) then
        AddMessage(True, 'The property ' + Prefix + Prop.Name + ' refers to ' + Prop.Value +
          ', which is outside of the form, and was skipped.')
      else
        Code.Add(Indent + Prefix + Prop.Name + ' := ' + Prop.Value + ';');
    lvList:
      if (Length(Prop.Name) > Length(StringsSuffix)) and Prop.Name.EndsWith(StringsSuffix, True) then
      begin
        { The lines of a TStrings are added one at a time }
        Path := Copy(Prop.Name, 1, Length(Prop.Name) - Length(StringsSuffix));
        for S in Prop.Items do
          Code.Add(Indent + Prefix + Path + '.Add(' + S + ');');
      end
      else
        WriteResource(Prop, Target, Indent, Code);
    lvBinary:
      WriteResource(Prop, Target, Indent, Code);
    lvCollection:
      WriteCollection(Instance, Prop, Prefix, Indent, Refs);
  end;
end;

{ Stores a property which cannot be written as code in a resource, and writes
  the code which loads it }

procedure TFrameConverter.WriteResource(Prop: TLfmProperty; const Target, Indent: string;
  Code: TStrings);
var
  Resource: TPropertyResource;
  I: Integer;
begin
  if Target = '' then
  begin
    AddMessage(True, 'The property ' + Prop.Name + ' of a collection item holds data ' +
      'which cannot be converted, and was skipped.');
    Exit;
  end;
  Resource.Name := UpperCase(StringReplace(FOptions.UnitName + '_' + Target + '_' + Prop.Name,
    '.', '_', [rfReplaceAll]));
  try
    Resource.Data := PropertyToBinary(Prop.Source);
  except
    on E: Exception do
    begin
      AddMessage(True, 'The property ' + Prop.Name + ' of ' + Target +
        ' could not be stored as a resource, and was skipped. ' + E.Message);
      Exit;
    end;
  end;
  I := Length(FResources);
  SetLength(FResources, I + 1);
  FResources[I] := Resource;
  Code.Add(Indent + 'LoadProperty(' + Target + ', ''' + Resource.Name + ''');');
end;

{ Writes the code which adds the items of a collection. The class of the
  items is taken from the collection of the component in the designer. }

procedure TFrameConverter.WriteCollection(Instance: TObject; Prop: TLfmProperty;
  const Prefix, Indent: string; Code: TStrings);
var
  Info: PPropInfo;
  Owner, Obj, LiveItem: TObject;
  Collection: TCollection;
  Item: TLfmItem;
  I, J: Integer;
begin
  Collection := nil;
  Info := PropInfoOf(Instance, Prop.Name, Owner);
  if (Info <> nil) and (Info^.PropType^.Kind = tkClass) then
  begin
    Obj := GetObjectProp(Owner, Info);
    if Obj is TCollection then
      Collection := TCollection(Obj);
  end;
  if Collection = nil then
  begin
    AddMessage(True, 'The collection ' + Prefix + Prop.Name + ' could not be found in the ' +
      'designer, so its items were skipped.');
    Exit;
  end;
  AddUnit(Collection.ItemClass);
  for I := 0 to Prop.CollectionItems.Count - 1 do
  begin
    Item := TLfmItem(Prop.CollectionItems[I]);
    LiveItem := nil;
    if I < Collection.Count then
      LiveItem := Collection.Items[I];
    Code.Add(Indent + 'with ' + Collection.ItemClass.ClassName + '(' + Prefix + Prop.Name +
      '.Add) do');
    Code.Add(Indent + 'begin');
    for J := 0 to Item.Properties.Count - 1 do
      WriteProperty(LiveItem, TLfmProperty(Item.Properties[J]), '', '', Indent + '  ', Code, Code);
    Code.Add(Indent + 'end;');
  end;
end;

{ Writes the code which places a component inside its parent }

procedure TFrameConverter.WriteAttach(const Name: string; Live, ParentLive: TComponent;
  const ParentName: string);
begin
  if Live = nil then
    Exit;
  if (Live is TControl) and (ParentLive is TWinControl) then
    FLayout.Add(Body + Name + '.Parent := ' + ParentName + ';')
  else if (Live is TMenuItem) and (ParentLive is TMenu) then
    FLayout.Add(Body + ParentName + '.Items.Add(' + Name + ');')
  else if (Live is TMenuItem) and (ParentLive is TMenuItem) then
    FLayout.Add(Body + ParentName + '.Add(' + Name + ');')
  else if (Live is TContainedAction) and (ParentLive is TCustomActionList) then
    FLayout.Add(Body + Name + '.ActionList := ' + ParentName + ';')
  else if ParentLive <> FRoot then
    AddMessage(True, 'The component ' + Name + ' is inside ' + ParentName + ', which is not ' +
      'supported, so it is only owned by the control.');
end;

procedure TFrameConverter.WriteObject(Obj: TLfmObject; const ParentName: string;
  ParentLive: TComponent);
var
  Live: TComponent;
  Prop: TLfmProperty;
  I: Integer;
begin
  if Obj.IsInherited or (Obj.Name = '') then
  begin
    AddMessage(True, 'The component ' + Obj.Name + ': ' + Obj.TypeName + ' is inherited ' +
      'and was skipped.');
    Exit;
  end;
  Live := FRoot.FindComponent(Obj.Name);
  if Live <> nil then
    AddUnit(Live.ClassType)
  else
    AddMessage(True, 'The component ' + Obj.Name + ' was not found in the designer, so ' +
      'its unit and the kinds of its properties could not be checked.');
  FFields.Add(Obj.Name + ': ' + Obj.TypeName + ';');
  FLayout.Add('');
  FLayout.Add(Body + '{ ' + Obj.Name + ' }');
  FLayout.Add(Body + Obj.Name + ' := ' + Obj.TypeName + '.Create(Self);');
  WriteAttach(Obj.Name, Live, ParentLive, ParentName);
  for I := 0 to Obj.Properties.Count - 1 do
  begin
    Prop := TLfmProperty(Obj.Properties[I]);
    { A component which is not a control has a Left and Top in the form file
      which are only its position in the designer, so they are skipped when
      the component has no such properties }
    if (Live <> nil) and (SameText(Prop.Name, 'Left') or SameText(Prop.Name, 'Top')) and
      (GetPropInfo(Live, Prop.Name) = nil) then
      Continue;
    WriteProperty(Live, Prop, Obj.Name + '.', Obj.Name, Body, FLayout, FLater);
  end;
  if Obj.IsInline then
  begin
    if Obj.Children.Count > 0 then
      AddMessage(True, 'Changes made to the components inside the frame ' + Obj.Name +
        ' were skipped.');
    Exit;
  end;
  for I := 0 to Obj.Children.Count - 1 do
    WriteObject(TLfmObject(Obj.Children[I]), Obj.Name, Live);
end;

{ Writes the properties of the form or frame itself which a control has. The
  client size of the form becomes the size of the control. }

procedure TFrameConverter.WriteRoot;
const
  Skipped: array[0..16] of string = ('Left', 'Top', 'Width', 'Height', 'ClientWidth',
    'ClientHeight', 'LCLVersion', 'DesignTimePPI', 'PixelsPerInch', 'DesignLeft',
    'DesignTop', 'TabOrder', 'Visible', 'Scaled', 'OldCreateOrder', 'TextHeight',
    'Name');
  Allowed: array[0..55] of string = ('Align', 'Anchors', 'AutoSize', 'BiDiMode',
    'BorderSpacing', 'BorderWidth', 'ChildSizing', 'Color', 'Constraints', 'Cursor',
    'DoubleBuffered', 'DragCursor', 'DragKind', 'DragMode', 'Enabled', 'Font',
    'HelpContext', 'HelpKeyword', 'HelpType', 'Hint', 'ParentBiDiMode', 'ParentColor',
    'ParentDoubleBuffered', 'ParentFont', 'ParentShowHint', 'PopupMenu', 'ShowHint',
    'TabStop', 'Tag', 'OnClick', 'OnContextPopup', 'OnDblClick', 'OnDragDrop',
    'OnDragOver', 'OnEndDrag', 'OnEnter', 'OnExit', 'OnKeyDown', 'OnKeyPress', 'OnKeyUp',
    'OnMouseDown', 'OnMouseEnter', 'OnMouseLeave', 'OnMouseMove', 'OnMouseUp',
    'OnMouseWheel', 'OnMouseWheelDown', 'OnMouseWheelUp', 'OnPaint', 'OnResize',
    'OnChangeBounds', 'OnShowHint', 'OnStartDrag', 'OnUTF8KeyPress', 'OnTripleClick',
    'OnQuadClick');
var
  Prop: TLfmProperty;
  I: Integer;
begin
  FLayout.Add(Body + '{ ' + FOptions.ControlClass + ' }');
  Prop := FLfm.FindProperty('ClientWidth');
  if Prop = nil then
    Prop := FLfm.FindProperty('Width');
  if Prop <> nil then
    FLayout.Add(Body + 'Width := ' + Prop.Value + ';');
  Prop := FLfm.FindProperty('ClientHeight');
  if Prop = nil then
    Prop := FLfm.FindProperty('Height');
  if Prop <> nil then
    FLayout.Add(Body + 'Height := ' + Prop.Value + ';');
  Prop := FLfm.FindProperty('DesignTimePPI');
  if (Prop <> nil) and (Prop.Value <> '96') then
    AddMessage(False, 'The form was designed at ' + Prop.Value + ' pixels per inch. The ' +
      'layout of the control uses the same sizes and is not scaled.');
  for I := 0 to FLfm.Properties.Count - 1 do
  begin
    Prop := TLfmProperty(FLfm.Properties[I]);
    if InList(FirstPart(Prop.Name), Skipped) then
      Continue;
    if SameText(Prop.Name, 'OnCreate') then
      FCreateHandler := Prop.Value
    else if SameText(Prop.Name, 'OnDestroy') then
      FDestroyHandler := Prop.Value
    else if InList(FirstPart(Prop.Name), Allowed) then
      WriteProperty(FRoot, Prop, '', 'Self', Body, FLayout, FLater)
    else
      AddMessage(True, 'The property ' + Prop.Name + ' of the form is not used by a control, ' +
        'and was skipped.');
  end;
end;

{ Changes the unit of the form into the unit of the control }

procedure TFrameConverter.WriteUnit;
var
  LE, ControlClass, ResFile, S: string;
  Lines, NewUnits: TStringList;
  AddCreate, AddDestroy: Boolean;
  I, J: Integer;
begin
  LE := FLineEnd;
  ControlClass := FOptions.ControlClass;
  { The unit name, which can have dots }
  I := FSource.UnitNameIndex;
  J := I;
  while (J + 1 < FSource.TokenCount) and not FSource.IsSymbol(J + 1, ';') do
    Inc(J);
  FSource.AddEdit(FSource.Token(I).StartPos, FSource.Token(J).EndPos - FSource.Token(I).StartPos,
    FOptions.UnitName);
  { The form class becomes the control class }
  FSource.ReplaceIdent(FLfm.TypeName, ControlClass);
  FSource.ReplaceToken(FSource.AncestorIndex, 'TCustomControl');
  { The units of the components }
  NewUnits := TStringList.Create;
  try
    { The control needs Classes and Controls along with the component units }
    NewUnits.Add('Classes');
    NewUnits.Add('Controls');
    for S in FComponentUnits do
      if NewUnits.IndexOf(S) < 0 then
        NewUnits.Add(S);
    for I := NewUnits.Count - 1 downto 0 do
      if (FSource.InterfaceUses.IndexOf(NewUnits[I]) > -1) or
        (FSource.ImplementationUses.IndexOf(NewUnits[I]) > -1) then
        NewUnits.Delete(I);
    if NewUnits.Count > 0 then
    begin
      S := '';
      for I := 0 to NewUnits.Count - 1 do
        if I = 0 then
          S := NewUnits[I]
        else
          S := S + ', ' + NewUnits[I];
      if FSource.InterfaceUsesEnd > -1 then
        FSource.AddEdit(FSource.Token(FSource.InterfaceUsesEnd).StartPos, 0, ', ' + S)
      else
        FSource.InsertAfter(FSource.InterfaceIndex, LE + LE + 'uses' + LE + '  ' + S + ';');
    end;
    FUsedUnits.AddStrings(FSource.InterfaceUses);
    FUsedUnits.AddStrings(FSource.ImplementationUses);
    FUsedUnits.AddStrings(NewUnits);
  finally
    NewUnits.Free;
  end;
  { The members added to the class }
  AddCreate := FSource.ClassMembers.IndexOf('create') < 0;
  if not AddCreate then
    AddMessage(True, 'The class already declares Create, so a constructor was not added. ' +
      'Call CreateLayout from Create to create the components.');
  AddDestroy := FDestroyHandler <> '';
  if AddDestroy and (FSource.ClassMembers.IndexOf('destroy') > -1) then
  begin
    AddDestroy := False;
    AddMessage(True, 'The class already declares Destroy, so call ' + FDestroyHandler +
      ' from it.');
  end;
  if FSource.ClassMembers.IndexOf('createlayout') > -1 then
    AddMessage(True, 'The class already has a member named CreateLayout, which the control ' +
      'uses to create its components. Rename one of them.');
  Lines := TStringList.Create;
  try
    Lines.Add('  private');
    for S in FFields do
      Lines.Add('    ' + S);
    Lines.Add('    procedure CreateLayout;');
    if AddCreate or AddDestroy then
      Lines.Add('  public');
    if AddCreate then
      Lines.Add('    constructor Create(AOwner: TComponent); override;');
    if AddDestroy then
      Lines.Add('    destructor Destroy; override;');
    S := '';
    for I := 0 to Lines.Count - 1 do
      S := S + Lines[I] + LE;
    FSource.InsertBeforeLine(FSource.ClassEndIndex, S);
    { The register procedure in the interface }
    FSource.InsertBeforeLine(FSource.ImplementationIndex, 'procedure Register;' + LE + LE);
    { The resource file and the routine which loads its properties }
    if Length(FResources) > 0 then
    begin
      ResFile := LowerCase(FOptions.UnitName) + '.res';
      S := LE + LE +
        '{$R ' + ResFile + '}' + LE + LE +
        'type' + LE +
        '  TPropertyReader = class(TReader);' + LE + LE +
        '{ Loads a property which could not be written as code, such as an image, from' + LE +
        '  the resource ResName }' + LE + LE +
        'procedure LoadProperty(Instance: TPersistent; const ResName: string);' + LE +
        'var' + LE +
        '  Stream: TResourceStream;' + LE +
        '  Reader: TPropertyReader;' + LE +
        'begin' + LE +
        '  Stream := TResourceStream.Create(HInstance, ResName, RT_RCDATA);' + LE +
        '  try' + LE +
        '    Reader := TPropertyReader.Create(Stream, 4096);' + LE +
        '    try' + LE +
        '      Reader.ReadProperty(Instance);' + LE +
        '    finally' + LE +
        '      Reader.Free;' + LE +
        '    end;' + LE +
        '  finally' + LE +
        '    Stream.Free;' + LE +
        '  end;' + LE +
        'end;';
      if FSource.ImplementationUsesEnd > -1 then
        FSource.InsertAfter(FSource.ImplementationUsesEnd, S)
      else
        FSource.InsertAfter(FSource.ImplementationIndex, S);
    end;
    { The constructor, destructor, layout, and register procedure }
    Lines.Clear;
    Lines.Add('{ ' + ControlClass + ' }');
    Lines.Add('');
    if AddCreate then
    begin
      Lines.Add('constructor ' + ControlClass + '.Create(AOwner: TComponent);');
      Lines.Add('begin');
      Lines.Add('  inherited Create(AOwner);');
      Lines.Add('  ControlStyle := ControlStyle - [csAcceptsControls];');
      Lines.Add('  CreateLayout;');
      if FCreateHandler <> '' then
        Lines.Add('  ' + FCreateHandler + '(Self);');
      Lines.Add('end;');
      Lines.Add('');
    end
    else if FCreateHandler <> '' then
      AddMessage(True, 'Call ' + FCreateHandler + ' from Create, as the form did when it ' +
        'was created.');
    if AddDestroy then
    begin
      Lines.Add('destructor ' + ControlClass + '.Destroy;');
      Lines.Add('begin');
      Lines.Add('  ' + FDestroyHandler + '(Self);');
      Lines.Add('  inherited Destroy;');
      Lines.Add('end;');
      Lines.Add('');
    end;
    Lines.Add('procedure ' + ControlClass + '.CreateLayout;');
    Lines.Add('begin');
    Lines.Add('  DisableAutoSizing;');
    Lines.Add('  try');
    Lines.AddStrings(FLayout);
    if FLater.Count > 0 then
    begin
      Lines.Add('');
      Lines.Add(Body + '{ References to components, and collections }');
      Lines.AddStrings(FLater);
    end;
    Lines.Add('  finally');
    Lines.Add('    EnableAutoSizing;');
    Lines.Add('  end;');
    Lines.Add('end;');
    Lines.Add('');
    Lines.Add('procedure Register;');
    Lines.Add('begin');
    Lines.Add('  RegisterComponents(''' + StringReplace(FOptions.PalettePage, '''', '''''',
      [rfReplaceAll]) + ''', [' + ControlClass + ']);');
    Lines.Add('end;');
    Lines.Add('');
    S := '';
    for I := 0 to Lines.Count - 1 do
      S := S + Lines[I] + LE;
    FSource.InsertBeforeLine(FSource.UnitEndIndex, S);
  finally
    Lines.Free;
  end;
  FUnitSource := FSource.Apply;
end;

{ Looks for code which uses members a form has and a control does not, or the
  global variable of the form, which the control does not have }

procedure TFrameConverter.CheckCode;
const
  FormMembers: array[0..13] of string = ('Close', 'ModalResult', 'ShowModal',
    'BorderStyle', 'BorderIcons', 'WindowState', 'FormStyle', 'ActiveControl',
    'KeyPreview', 'Release', 'ShowInTaskBar', 'AlphaBlend', 'AlphaBlendValue',
    'CloseQuery');
var
  I: Integer;
  S: string;
begin
  for I := FSource.ImplementationIndex + 1 to FSource.TokenCount - 1 do
  begin
    if (FSource.Token(I).Kind <> ptIdent) or FSource.IsSymbol(I - 1, '.') then
      Continue;
    S := FSource.Token(I).Text;
    if InList(S, FormMembers) and (FSource.ClassMembers.IndexOf(LowerCase(S)) < 0) then
      AddMessage(False, S + ' is a member of a form which a control does not have.',
        FSource.LineOf(I))
    else if FSource.FormVariables.IndexOf(S) > -1 then
      AddMessage(True, S + ' is the global variable of the form, which is removed.',
        FSource.LineOf(I));
  end;
end;

function TFrameConverter.Execute: Boolean;
var
  LfmFile, Text: string;
  I: Integer;
begin
  LfmFile := ChangeFileExt(FFileName, '.lfm');
  if not FileExists(LfmFile) then
    Exit(Fail('The form file ' + LfmFile + ' could not be found.'));
  try
    FLfm := ParseLfm(ReadFile(LfmFile));
    Text := ReadFile(FFileName);
  except
    on E: Exception do
      Exit(Fail(E.Message));
  end;
  if Pos(#13#10, Text) > 0 then
    FLineEnd := #13#10;
  if FLfm.IsInherited then
    Exit(Fail('The form inherits from another form, which is not supported.'));
  FSource := TFormSource.Create(Text);
  GatherNames(FLfm);
  if not FSource.Analyze(FLfm.TypeName, FNames) then
    Exit(Fail(FSource.Error));
  WriteRoot;
  for I := 0 to FLfm.Children.Count - 1 do
    WriteObject(TLfmObject(FLfm.Children[I]), 'Self', FRoot);
  WriteUnit;
  CheckCode;
  Result := True;
end;

end.
