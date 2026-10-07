(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified August 2019                                *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.collections.txt> }
unit Codebot.Collections;

{$i codebot.inc}

interface

uses
  SysUtils, Classes,
  Codebot.System;

{doc off}
type
  IIndexedEnumerator<T> = interface(IEnumerator<T>)
  ['{4F6365A5-B833-4E35-BD2B-9C64C363CC4B}']
    function GetEnumerator: IIndexedEnumerator<T>;
    function GetCount: Integer;
    function GetItem(Index: Integer): T;
    property Count: Integer read GetCount;
    property Item[Index: Integer]: T read GetItem; default;
  end;

  TListEnumerator<T> = class(TInterfacedObject, IEnumerator<T>, IIndexedEnumerator<T>)
  private
    FItems: TArrayList<T>;
    FPosition: Integer;
    FCount: Integer;
  public
    constructor Create(Items: TArrayList<T>; Count: Integer = -1);
    function GetCurrent: T;
    function MoveNext: Boolean;
    procedure Reset;
    function GetEnumerator: IIndexedEnumerator<T>;
    function GetCount: Integer;
    function GetItem(Index: Integer): T;
    property Count: Integer read GetCount;
    property Item[Index: Integer]: T read GetItem; default;
  end;
{doc on}

{ TList\<TItem\> is a generic growable list of items
  See also
  <link Overview.Codebot.Collections.TList, TList\<TItem\> members> }

  TList<TItem> = class(TObject)
  public
    { The type of items held by the list }
    type ItemType = TItem;
    { A pointer to an item }
    type PItemType = ^TItem;
    { The function used to compare items when sorting or searching }
    type TListCompare = TCompare<TItem>;
    { Get the enumerator for the list }
    function GetEnumerator: IEnumerator<ItemType>;
  private
    FCount: Integer;
    FCapacity: Integer;
    procedure QuickSort(Compare: TListCompare; L, R: Integer);
    procedure CheckBounds(const Method: string; Index: Integer);
    function GetHead: PItemType;
    function GetFirst: ItemType;
    function GetLast: ItemType;
    function GetCapacity: Integer;
    procedure SetCapacity(Value: Integer);
    function GetItem(Index: Integer): ItemType;
    procedure SetItem(Index: Integer; const Value: ItemType);
    function GetDirect(Index: Integer): PItemType;
  protected
    FItems: TArrayList<ItemType>;
    { Allows list types to take action on add }
    procedure AddItem(constref Item: ItemType); virtual;
    { Allows list types to take action on delete }
    procedure DeleteItem(var Item: ItemType); virtual;
    { If require delete is true delete item will be called for every item on clear }
    function RequiresDelete: Boolean; virtual;
    { Search the list for an equal item }
    function Find(Comparer: TListCompare; const Item: ItemType): Integer;
    { Request room for at least a minimum number of items }
    procedure Grow(MinCapacity: Integer);
  public
    { Clear the list and destroy it }
    destructor Destroy; override;
    { Add an item to the end of the list }
    procedure Add(const Item: ItemType);
    { Delete an item by index from the list }
    procedure Delete(Index: Integer);
    { Move item at old index to new index }
    procedure Move(OldIndex, NewIndex: Integer);
    { Exchange positions of two items in the list }
    procedure Exchange(A, B: Integer);
    { Remove all items from the list setting capacity and count to zero }
    procedure Clear;
    { Reclaim unused capacity }
    procedure Compact;
    { Sort the list }
    procedure Sort(Compare: TListCompare = nil); virtual;
    { A reference to the first item in the list }
    property Head: PItemType read GetHead;
    { The first item in the list }
    property First: ItemType read GetFirst;
    { The last item in the list }
    property Last: ItemType read GetLast;
    { Number of items in the list }
    property Count: Integer read FCount;
    { Allocated space in terms of number of items }
    property Capacity: Integer read GetCapacity write SetCapacity;
    { Get or set an item in the list
      Remarks
      When setting the existing item will be deleted }
    property Item[Index: Integer]: ItemType read GetItem write SetItem; default;
    { Retrieve a direct reference to the item }
    property Direct[Index: Integer]: PItemType read GetDirect;
  end;

{ TListDuplicates allows, ignores, or generates errors when a matching value is
  added to an indexed list }

  TListDuplicates = (duplicateAllow, duplicateIgnore, duplicateError);

{ TIndexedList\<TItem\> allows for search and removing of items by value
  See also
  <link Overview.Codebot.Collections.TIndexedList, TIndexedList\<TItem\> members> }

  TIndexedList<TItem> = class(TList<TItem>)
  private
    FDuplicates: TListDuplicates;
  protected
    { AddItem checks for duplicates }
    procedure AddItem(constref Item: ItemType); override;
    { Allow, ignore, or error on adding a matching item }
    property Duplicates: TListDuplicates read FDuplicates write FDuplicates;
  public
    { Returns true if the list contains item }
    function Contains(const Item: ItemType): Boolean;
    { Returns the index of the item or -1 if it cannot be found }
    function IndexOf(const Item: ItemType): Integer; virtual; abstract;
    { Remove the item by value returning true if it was found }
    function Remove(const Item: ItemType): Boolean;
  end;

{ TObjectList\<TItem\> holds objects and can optionally be set to manage their life
  See also
  <link Overview.Codebot.Collections.TObjectList, TObjectList\<TItem\> members> }

  TObjectList<TItem: TObject> = class(TIndexedList<TItem>)
  private
    FOwnsObjects: Boolean;
  protected
    { Prevents items from being added twice }
    procedure AddItem(constref Item: ItemType); override;
    { Frees the items if the list owns the objects }
    procedure DeleteItem(var Item: ItemType); override;
    { Returns true if the list owns the objects }
    function RequiresDelete: Boolean; override;
  public
    { Create the list optionally owning objects added to it }
    constructor Create(OwnsObjects: Boolean);
    { Returns the index of the object or -1 if it cannot be found }
    function IndexOf(const Item: ItemType): Integer; override;
    { Allow, ignore, or error on adding a duplicate object }
    property Duplicates;
  end;

{ TDictionary\<K, V\> holds key value pairs allowing items to be indexed by a key
  See also
  <link Overview.Codebot.Collections.TDictionary, TDictionary\<K, V\> members> }

  TDictionary<K, V> = class(TObject)
  public
    { TDictionary\<K, V\>.TKeyValue holds a key value pair
      See also
      <link Overview.Codebot.Collections.TDictionary.TKeyValue, TDictionary\<K, V\>.TKeyValue members> }

    type
      TKeyValue = class
      private
        FKey: K;
        FValue: V;
      public
        { Create a key value pair given a key }
        constructor Create(const Key: K);
        { The key }
        property Key: K read FKey;
        { The value }
        property Value: V read FValue write FValue;
      end;

  public
    { Get the enumerator for the dictionary }
    function GetEnumerator: IEnumerator<TKeyValue>;
  private
    FAutoKey: Boolean;
    FList: TList<TKeyValue>;
    FComparer: TCompare<K>;
    function KeyEquals(const A, B: K): Boolean;
    function GetCount: Integer;
    function GetItem(Index: Integer): TKeyValue;
    function GetKey(Index: Integer): K;
    function GetValue(const Key: K): V;
    procedure SetValue(const Key: K; const Value: V);
  protected
    { Add a new key value pair to the dictionary }
    procedure AddKeyValue(KeyValue: TKeyValue); virtual;
    { Create a new key value pair for a key }
    function CreateKeyValue(const Key: K): TKeyValue; virtual;
    { Destroy a key value pair when it is removed }
    procedure DestroyKeyValue(KeyValue: TKeyValue); virtual;
    { Change the value of an existing key value pair }
    procedure ChangeKeyValue(KeyValue: TKeyValue; Value: V); virtual;
    { Return a default value if no key exists }
    function DefaultValue: V; virtual;
    { When true key values will automatically be created when a value is requested
      if it doesn't exist }
    property AutoKey: Boolean read FAutoKey write FAutoKey;
  public
    { Create the dictionary }
    constructor Create;
    { Clear the dictionary and destroy it }
    destructor Destroy; override;
    { Remove an item by key }
    procedure Remove(const Key: K);
    { Remove all items from the dictionary }
    procedure Clear;
    { Returns true if the key is in the dictionary }
    function KeyExists(const Key: K): Boolean;
    { Used to compare keys }
    property Comparer: TCompare<K> read FComparer write FComparer;
    { Items indexed by integer }
    property Items[Index: Integer]: TKeyValue read GetItem;
    { Keys indexed by integer }
    property Keys[Index: Integer]: K read GetKey;
    { Values indexed by key }
    property Values[const Key: K]: V read GetValue write SetValue; default;
    { Number of key value pairs }
    property Count: Integer read GetCount;
  end;

{ TFindProc\<T\> is used by IList\<T\>.Find to test if an item matches }

  TFindProc<T> = function(Item: T; var Match): Boolean;

{ IList\<T\> is a reference counted list of objects or interfaces
  See also
  <link Overview.Codebot.Collections.IList, IList\<T\> members> }

  IList<T> = interface(IEnumerable<T>)
  ['{79BFA1EC-6CEA-42FA-A602-2FC727436CC0}']
    {doc off}
    function GetCapacity: Integer;
    procedure SetCapacity(NewCapacity: Integer);
    function GetCount: Integer;
    function Get(I: Integer): T;
    procedure Put(I: Integer; Item: T);
    {doc on}
    { Remove all items from the list }
    procedure Clear;
    { Delete an item by index }
    procedure Delete(Index: Integer);
    { Sort the list using a comparer }
    procedure Sort(Compare: TCompare<T>);
    { Exchange positions of two items in the list }
    procedure Exchange(Index1, Index2: Integer);
    { The first item in the list }
    function First: T;
    { Return the first item for which FindProc returns true }
    function Find(FindProc: TFindProc<T>; var Match): T;
    { Returns the index of the item or -1 if it cannot be found }
    function IndexOf(Item: T): Integer;
    { Add an item returning its index }
    function Add(Item: T): Integer;
    { The last item in the list }
    function Last: T;
    { Remove an item returning the index it had or -1 if it was not found }
    function Remove(Item: T): Integer;
    { Address of the first item }
    function Data: Pointer;
    { Allocated space in terms of number of items }
    property Capacity: Integer read GetCapacity write SetCapacity;
    { Number of items in the list }
    property Count: Integer read GetCount;
    { Get or set an item in the list }
    property Items[Index: Integer]: T read Get write Put; default;
  end;

{ TReferences\<T\> is the base class implementing IList\<T\> for reference
  types. Descendants decide what happens when items are added or removed. }

  TReferences<T> = class(TInterfacedObject, IList<T>)
  private
    FList: TList<T>;
  protected
    { Convert an item to a pointer }
    function AsPointer(Item: T): Pointer; virtual; abstract;
    { Invoked when an item is added }
    procedure AddItem(P: Pointer); virtual; abstract;
    { Invoked when an item is removed }
    procedure RemoveItem(P: Pointer); virtual; abstract;
  public
    { Create an empty list }
    constructor Create;
    { Remove all items and destroy the list }
    destructor Destroy; override;
    {doc off}
    function GetEnumerator: IEnumerator<T>;
    function GetCapacity: Integer;
    procedure SetCapacity(NewCapacity: Integer);
    function GetCount: Integer;
    function Get(I: Integer): T;
    procedure Put(I: Integer; Item: T);
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Sort(Compare: TCompare<T>);
    procedure Exchange(Index1, Index2: Integer);
    function First: T;
    function IndexOf(Item: T): Integer; virtual; abstract;
    function Find(FindProc: TFindProc<T>; var Match): T;
    function Add(Item: T): Integer;
    function Last: T;
    function Remove(Item: T): Integer;
    function Data: Pointer;
    {doc on}
    { Get or set an item in the list }
    property Item[Index: Integer]: T read Get write Put; default;
  end;

{ TObjects\<T\> is an IList\<T\> of objects which optionally frees items
  when they are removed }

  TObjects<T: TObject> = class(TReferences<T>)
  private
    FOwnsObjects: Boolean;
  protected
    function AsPointer(Item: T): Pointer; override;
    procedure AddItem(P: Pointer); override;
    procedure RemoveItem(P: Pointer); override;
  public
    { Create the list optionally owning objects added to it }
    constructor Create(OwnsObjects: Boolean = False);
    { Returns the index of the object or -1 if it cannot be found }
    function IndexOf(Item: T): Integer; override;
  end;

{ TInterfaces\<T\> is an IList\<T\> of interfaces which holds a reference
  to each item until it is removed }

  TInterfaces<T: IInterface> = class(TReferences<T>)
  protected
    function AsPointer(Item: T): Pointer; override;
    procedure AddItem(P: Pointer); override;
    procedure RemoveItem(P: Pointer); override;
  public
    { Returns the index of the interface or -1 if it cannot be found }
    function IndexOf(Item: T): Integer; override;
  end;

{ TAggregateStream reads a series of text, files, and streams as if they
  were one continuous stream }

  TAggregateStream = class(TStream)
  private
    FStreams: TList<TStream>;
    FOwns: TList<Boolean>;
    FIndex: Integer;
    FSize: LargeInt;
    procedure Reset;
  protected
    function  GetSize: Int64; override;
  public
    { Create an empty aggregate stream }
    constructor Create;
    { Destroy the aggregate stream and any streams it owns }
    destructor Destroy; override;
    { Remove all streams }
    procedure Clear;
    { Add a block of text to the end }
    procedure AddText(const Text: string);
    { Add the contents of a file to the end }
    procedure AddFile(const FileName: string);
    { Add a stream to the end optionally taking ownership of it }
    procedure AddStream(Stream: TStream; OwnsStream: Boolean = True);
    { Read bytes moving through each stream in order }
    function Read(var Buffer; Count: Longint): Longint; override;
  end;

{doc off}
function FindObject(constref A, B: TObject): Integer;
function FindInterface(constref A, B: IInterface): Integer;
{doc on}

implementation

uses
  Codebot.Constants;

constructor TListEnumerator<T>.Create(Items: TArrayList<T>; Count: Integer = -1);
begin
  inherited Create;
  FItems := Items;
  FPosition := -1;
  if Count < 0 then
    FCount := Items.Length
  else
    FCount := Count;
end;

function TListEnumerator<T>.GetCurrent: T;
begin
  Result := FItems[FPosition];
end;

function TListEnumerator<T>.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FCount;
end;

procedure TListEnumerator<T>.Reset;
begin
  FPosition := -1;
end;

function TListEnumerator<T>.GetEnumerator: IIndexedEnumerator<T>;
begin
  Result := Self;
end;

function TListEnumerator<T>.GetCount: Integer;
begin
  Result := FCount;
end;

function TListEnumerator<T>.GetItem(Index: Integer): T;
begin
  Result := FItems[Index];
end;

{ TList<TItem> }

function TList<TItem>.GetEnumerator: IEnumerator<ItemType>;
begin
  Result := TListEnumerator<ItemType>.Create(FItems, FCount);
end;

procedure TList<TItem>.AddItem(constref Item: ItemType);
begin
  Grow(FCount + 1);
  FItems[FCount] := Item;
  Inc(FCount);
end;

procedure TList<TItem>.DeleteItem(var Item: ItemType);
begin
  Item := default(ItemType);
end;

function TList<TItem>.RequiresDelete: Boolean;
begin
  Result := False;
end;

function TList<TItem>.Find(Comparer: TListCompare; const Item: ItemType): Integer;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    if Comparer(Item, FItems[I]) = 0 then
      Exit(I);
  Result := -1;
end;

procedure TList<TItem>.Grow(MinCapacity: Integer);
const
  ActualMinCapacity = 10;
begin
  if MinCapacity > FCapacity then
  begin
    if MinCapacity < ActualMinCapacity then
      MinCapacity := ActualMinCapacity;
    FCapacity := MinCapacity + FCapacity div 4;
    FItems.Length := FCapacity;
  end;
end;

destructor TList<TItem>.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TList<TItem>.CheckBounds(const Method: string; Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then
    raise ERangeError.CreateFmt(SRangeMethodError, [ClassName, Method]);
end;

procedure TList<TItem>.Clear;
var
  I: Integer;
begin
  if RequiresDelete then
    for I := 0 to FCount - 1 do
      DeleteItem(FItems.Items[I]);
  FCount := 0;
  Compact;
end;

procedure TList<TItem>.Compact;
const
  ActualMinCapacity = 10;
var
  I: Integer;
begin
  I := FCount;
  if I < ActualMinCapacity then
    I := ActualMinCapacity;
  if FCount = 0 then
    I := 0;
  if I < FCapacity then
  begin
    FCapacity := I;
    FItems.Length := FCapacity;
  end;
end;

procedure TList<TItem>.QuickSort(Compare: TListCompare; L, R: Integer);
var
  I, J, P: Integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while Compare(Item[I], Item[P]) < 0 do Inc(I);
      while Compare(Item[J], Item[P]) > 0 do Dec(J);
      if I <= J then
      begin
        Exchange(I, J);
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(Compare, L, J);
    L := I;
  until I >= R;
end;

procedure TList<TItem>.Sort(Compare: TListCompare = nil);
begin
  if Count < 2 then
    Exit;
  if Assigned(Compare) then
    QuickSort(Compare, 0, Count - 1)
  else if Assigned(TArrayList<ItemType>.DefaultCompare) then
    QuickSort(TArrayList<ItemType>.DefaultCompare, 0, Count - 1);
end;

procedure TList<TItem>.Add(const Item: ItemType);
begin
  AddItem(Item);
end;

procedure TList<TItem>.Delete(Index: Integer);
var
  I: Integer;
begin
  CheckBounds('Delete', Index);
  if RequiresDelete then
    DeleteItem(FItems.Items[Index]);
  for I := Index + 1 to FCount - 1 do
    FItems[I - 1] := FItems[I];
  Dec(FCount);
  FItems[FCount] := default(ItemType);
end;

procedure TList<TItem>.Move(OldIndex, NewIndex: Integer);
var
  I: ItemType;
  J: Integer;
begin
  CheckBounds('Move', OldIndex);
  CheckBounds('Move', NewIndex);
  {TODO: Consider using System.Move}
  if OldIndex < NewIndex then
  begin
    Inc(OldIndex);
    for J := OldIndex to NewIndex do
    begin
      I := FItems[J - 1];
      FItems[J - 1] :=  FItems[J];
      FItems[J] := I;
    end
  end
  else if OldIndex > NewIndex then
  begin
    Dec(OldIndex);
    for J := OldIndex downto NewIndex do
    begin
      I := FItems[J + 1];
      FItems[J + 1] :=  FItems[J];
      FItems[J] := I;
    end;
  end;
end;

procedure TList<TItem>.Exchange(A, B: Integer);
var
  I: ItemType;
begin
  CheckBounds('Exchange', A);
  CheckBounds('Exchange', B);
  if A <> B then
  begin
    I := FItems[A];
    FItems[A] := FItems[B];
    FItems[B] := I;
  end;
end;

function TList<TItem>.GetHead: PItemType;
begin
  if FCount > 0 then
    Result := PItemType(@FItems.Items[0])
  else
    Result := nil;
end;

function TList<TItem>.GetFirst: ItemType;
begin
  if FCount > 0 then
    Result := FItems[0]
  else
    Result := default(ItemType);
end;

function TList<TItem>.GetLast: ItemType;
begin
  if FCount > 0 then
    Result := FItems[FCount - 1]
  else
    Result := default(ItemType);
end;

function TList<TItem>.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

procedure TList<TItem>.SetCapacity(Value: Integer);
begin
  if Value > FCapacity then
    Grow(Value);
end;

function TList<TItem>.GetItem(Index: Integer): ItemType;
begin
  CheckBounds('GetItem', Index);
  Result := FItems[Index];
end;

function TList<TItem>.GetDirect(Index: Integer): PItemType;
begin
  CheckBounds('GetDirect', Index);
  Result := @FItems.Items[Index];
end;

procedure TList<TItem>.SetItem(Index: Integer; const Value: ItemType);
begin
  CheckBounds('SetItem', Index);
  if RequiresDelete then
    DeleteItem(FItems.Items[Index]);
  FItems[Index] := Value;
end;

{ TIndexedList<TItem> }

procedure TIndexedList<TItem>.AddItem(constref Item: ItemType);
var
  I: Integer;
begin
  if FDuplicates = duplicateAllow then
    inherited AddItem(Item)
  else
  begin
    I := IndexOf(Item);
    if I < 0 then
      inherited AddItem(Item)
    else if FDuplicates = duplicateError then
      raise EListError.Create(SListDuplicateError);
  end;
end;

function TIndexedList<TItem>.Contains(const Item: ItemType): Boolean;
begin
  Result := IndexOf(Item) > -1;
end;

function TIndexedList<TItem>.Remove(const Item: ItemType): Boolean;
var
  I: Integer;
begin
  I := IndexOf(Item);
  Result := I > -1;
  if Result then
    Self.Delete(I);
end;

{ TObjectList<TItem> }

constructor TObjectList<TItem>.Create(OwnsObjects: Boolean);
begin
  inherited Create;
  FOwnsObjects := OwnsObjects;
  if FOwnsObjects then
    Duplicates := duplicateError;
end;

procedure TObjectList<TItem>.AddItem(constref Item: ItemType);
var
  CanAdd: Boolean;
begin
  if FOwnsObjects then
    CanAdd := IndexOf(Item) < 0
  else
    CanAdd := True;
  if CanAdd then
    inherited AddItem(Item);
end;

procedure TObjectList<TItem>.DeleteItem(var Item: ItemType);
begin
  if FOwnsObjects then
    Item.Free;
  Item := TObject(nil);
end;

function TObjectList<TItem>.RequiresDelete: Boolean;
begin
  Result := FOwnsObjects;
end;

function FindObject(constref A, B: TObject): Integer;
begin
  Result := PtrInt(A) - PtrInt(B);
end;

function FindInterface(constref A, B: IInterface): Integer;
begin
  Result := PtrInt(A) - PtrInt(B);
end;

function TObjectList<TItem>.IndexOf(const Item: ItemType): Integer;
begin
  Result := Find(TCompare<Titem>(@FindObject), Item);
end;

{ TDictionary<K, V>.TKeyValue }

constructor TDictionary<K, V>.TKeyValue.Create(const Key: K);
begin
  inherited Create;
  FKey := Key;
end;

{ TDictionary<K, V> }

function TDictionary<K, V>.GetEnumerator: IEnumerator<TKeyValue>;
begin
  Result := FList.GetEnumerator;
end;

constructor TDictionary<K, V>.Create;
begin
  inherited Create;
  FList := TList<TKeyValue>.Create;
end;

destructor TDictionary<K, V>.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TDictionary<K, V>.AddKeyValue(KeyValue: TKeyValue);
begin
  FList.Add(KeyValue);
end;

function TDictionary<K, V>.CreateKeyValue(const Key: K): TKeyValue;
begin
  Result := TKeyValue.Create(Key);
end;

procedure TDictionary<K, V>.DestroyKeyValue(KeyValue: TKeyValue);
begin
  KeyValue.Free;
end;

procedure TDictionary<K, V>.ChangeKeyValue(KeyValue: TKeyValue; Value: V);
begin
  KeyValue.Value := Value;
end;

function TDictionary<K, V>.DefaultValue: V;
begin
  Result := default(V);
end;

function TDictionary<K, V>.KeyEquals(const A, B: K): Boolean;
begin
  if Assigned(FComparer) then
    Result := FComparer(A, B) = 0
  else
    Result := A = B;
end;

procedure TDictionary<K, V>.Remove(const Key: K);
var
  KeyValue: TKeyValue;
  I: Integer;
begin
  I := 0;
  repeat
    if I = FList.Count then
      Exit;
    KeyValue := TKeyValue(FList[I]);
    if KeyEquals(KeyValue.Key, Key) then
    begin
      DestroyKeyValue(KeyValue);
      FList.Delete(I);
      Exit;
    end;
    Inc(I);
  until False;
end;

procedure TDictionary<K, V>.Clear;
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
    DestroyKeyValue(FList[I]);
  FList.Clear;
end;

function TDictionary<K, V>.KeyExists(const Key: K): Boolean;
var
  KeyValue: TKeyValue;
  I: Integer;
begin
  Result := True;
  for I := 0 to FList.Count - 1 do
  begin
    KeyValue := TKeyValue(FList[I]);
    if KeyEquals(KeyValue.Key, Key) then
      Exit;
  end;
  Result := False;
end;

function TDictionary<K, V>.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TDictionary<K, V>.GetItem(Index: Integer): TKeyValue;
begin
  Result := FList[Index];
end;

function TDictionary<K, V>.GetKey(Index: Integer): K;
begin
  Result := FList[Index].Key;
end;

function TDictionary<K, V>.GetValue(const Key: K): V;
var
  KeyValue: TKeyValue;
  I: Integer;
begin
  I := 0;
  repeat
    if I = FList.Count then
    begin
      if AutoKey then
      begin
        KeyValue := CreateKeyValue(Key);
        if KeyValue <> nil then
        begin
          AddKeyValue(KeyValue);
          Result := KeyValue.Value;
        end;
      end
      else
        Result := DefaultValue;
      Exit;
    end;
    KeyValue := FList[I];
    if KeyEquals(KeyValue.Key, Key) then
      Exit(KeyValue.Value);
    Inc(I);
  until False;
end;

procedure TDictionary<K, V>.SetValue(const Key: K; const Value: V);
var
  KeyValue: TKeyValue;
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
  begin
    KeyValue := TKeyValue(FList[I]);
    if KeyEquals(KeyValue.Key, Key) then
    begin
      ChangeKeyValue(KeyValue, Value);
      Exit;
    end;
  end;
  KeyValue := CreateKeyValue(Key);
  if KeyValue <> nil then
  begin
    KeyValue.FValue := Value;
    AddKeyValue(KeyValue);
  end;
end;

{ TReferences<T> }

constructor TReferences<T>.Create;
begin
  inherited Create;
  FList := TList<T>.Create;;
end;

destructor TReferences<T>.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

function TReferences<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := FList.GetEnumerator;
end;

function TReferences<T>.GetCapacity: Integer;
begin
  Result := FList.Capacity;
end;

procedure TReferences<T>.SetCapacity(NewCapacity: Integer);
begin
  FList.Capacity := NewCapacity;
end;

function TReferences<T>.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TReferences<T>.Get(I: Integer): T;
begin
  Result := FList[I];
end;

procedure TReferences<T>.Put(I: Integer; Item: T);
begin
  RemoveItem(AsPointer(FList[I]));
  FList[I] := Default(T);
  AddItem(AsPointer(Item));
  FList[I] := Item;
end;

procedure TReferences<T>.Clear;
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
  begin
    RemoveItem(AsPointer(FList[I]));
    FList[I] := Default(T);
  end;
  FList.Clear;
end;

procedure TReferences<T>.Delete(Index: Integer);
begin
  RemoveItem(AsPointer(FList[Index]));
  FList[Index] := Default(T);
  FList.Delete(Index);
end;

procedure TReferences<T>.Sort(Compare: TCompare<T>);
begin
  FList.Sort(Compare);
end;

procedure TReferences<T>.Exchange(Index1, Index2: Integer);
begin
  FList.Exchange(Index1, Index2);
end;

function TReferences<T>.First: T;
begin
  Result := FList.First;
end;

function TReferences<T>.Find(FindProc: TFindProc<T>; var Match): T;
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
    if FindProc(FList[I], Match) then
      Exit(FList[I]);
  Result := Default(T);
end;

function TReferences<T>.Add(Item: T): Integer;
begin
  AddItem(AsPointer(Item));
  FList.Add(Item);
end;

function TReferences<T>.Last: T;
begin
  Result := FList.Last;
end;

function TReferences<T>.Remove(Item: T): Integer;
begin
  Result := IndexOf(Item);
  if Result > -1 then
  begin
    RemoveItem(AsPointer(Item));
    FList[Result] := Default(T);
    FList.Delete(Result);
  end;
end;

function TReferences<T>.Data: Pointer;
begin
  if FList.Count < 1 then
    Result := nil
  else
    Result := @FList.FItems.Items[0];
end;

{ TObjects<T> }

constructor TObjects<T>.Create(OwnsObjects: Boolean);
begin
  inherited Create;
  FOwnsObjects := OwnsObjects;
end;

function TObjects<T>.AsPointer(Item: T): Pointer;
begin
  Result := Pointer(Item);
end;

procedure TObjects<T>.AddItem(P: Pointer);
begin
end;

procedure TObjects<T>.RemoveItem(P: Pointer);
begin
  if FOwnsObjects then
    TObject(P).Free;
end;

function TObjects<T>.IndexOf(Item: T): Integer;
begin
  Result := FList.Find(TCompare<T>(@FindObject), Item);
end;

{ TInterfaces<T> }

function TInterfaces<T>.AsPointer(Item: T): Pointer;
begin
  Result := Pointer(Item);
end;

procedure TInterfaces<T>.AddItem(P: Pointer);
var
  I: IInterface;
begin
  I := IInterface(P);
  if I <> nil then
    I._AddRef;
end;

procedure TInterfaces<T>.RemoveItem(P: Pointer);
var
  I: IUnknown;
begin
  I := IInterface(P);
  if I <> nil then
    I._Release;
end;

function TInterfaces<T>.IndexOf(Item: T): Integer;
begin
  Result := FList.Find(TCompare<T>(@FindInterface), Item);
end;

{ TAggregateStream }

constructor TAggregateStream.Create;
begin
	inherited Create;
	FStreams := TList<TStream>.Create;
	FOwns := TList<Boolean>.Create;
	Reset;
end;

destructor TAggregateStream.Destroy;
begin
	Clear;
	FStreams.Free;
	FOwns.Free;
	inherited Destroy;
end;

procedure TAggregateStream.Reset;
begin
	FIndex := -1;
	FSize := -1;
end;

function TAggregateStream.GetSize: Int64;
var
  S: TStream;
begin
  if FSize > -1 then
    Exit(FSize);
  FSize := 0;
  for S in FStreams do
    Inc(FSize, S.Size - S.Position);
  Result := FSize;
end;

procedure TAggregateStream.Clear;
var
	I: Integer;
begin
  Reset;
	for I := 0 to FOwns.Count - 1 do
		if FOwns[I] then
			FStreams[I].Free;
	FStreams.Clear;
	FOwns.Clear;
end;

procedure TAggregateStream.AddText(const Text: string);
begin
	AddStream(TStringStream.Create(Text));
end;

procedure TAggregateStream.AddFile(const FileName: string);
begin
	AddStream(TFileStream.Create(FileName, fmOpenRead));
end;

procedure TAggregateStream.AddStream(Stream: TStream; OwnsStream: Boolean = True);
begin
  Reset;
	FStreams.Add(Stream);
	FOwns.Add(OwnsStream);
end;

function TAggregateStream.Read(var Buffer; Count: Integer): Integer;
var
	S: TStream;
begin
	Result := 0;
	if Count < 1 then
		Exit;
	if FIndex < 0 then
		Inc(FIndex);
	if FIndex > FStreams.Count - 1 then
		Exit;
	S := FStreams[FIndex];
	Result := S.Read(Buffer, Count);
	if Result < 1 then
	begin
		Inc(FIndex);
		Result := Read(Buffer, Count);
	end;
end;

end.

