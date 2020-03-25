{
  Delphi DUnit Test Case for the UContainers Unit
}


unit TestUContainers;

interface

uses
  Classes, // must come before Generics.Collections
  Generics.Collections,
  TestFramework, UContainers;

type

  // Test methods for class TSortedList
  TestTSortedList = class(TTestCase)
  strict private
    fList: TSortedList<string>;
    fNotifyList: TList<TPair<string,TCollectionNotification>>;
    procedure ClearAll;
    procedure Populate;
    procedure ErrorAddDuplicate;
    procedure ErrorPermitDuplicates;
    procedure OnNotifyHandler(Sender: TObject; const Item: string;
      Action: TCollectionNotification);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAdd;
    procedure TestCount;
    procedure TestClear;
    procedure TestFind;
    procedure TestIndexOf;
    procedure TestContains;
    procedure TestIsEmpty;
    procedure TestGetEnumerator;
    procedure TestItemsProp;
    procedure TestDelete; // indirectly uses Items[]
    procedure TestRemove; // indirectly uses Items[]
    procedure TestContainsDuplicates;
    procedure TestPermitDuplicates;
    procedure TestAddDuplicates;
    procedure TestCreateNoParams;
    procedure TestOnNotify;
  end;

  // Test methods for class TSortedObjectList
  // We only test new properties over and above parent TSortedList class.
  // We assume that any inherited methods called in test have already been
  // tested, so run this test *after* TestTSortedList tests
  TestTSortedObjectList = class(TTestCase)
  published
    procedure TestOwnsObjects;
  end;

  // Test methods for class TSortedDictionary
  TestTSortedDictionary = class(TTestCase)
  strict private
    fDict: TSortedDictionary<string,Integer>;  // constructor with comparer
    fLastKeyNotification: TPair<string,TCollectionNotification>;
    fLastValueNotification: TPair<Integer,TCollectionNotification>;
  public
    procedure SetUp; override;
    procedure TearDown; override;
    procedure ClearAll;
    procedure Populate;
    procedure ErrorAdd;
    procedure ErrorAddPair;
    procedure ErrorValues;
    procedure ErrorKeys;
    procedure ErrorItems;
    procedure OnKeyNotifyHandler(Sender: TObject; const Key: string;
      Action: TCollectionNotification);
    procedure OnValueNotifyHandler(Sender: TObject; const Value: Integer;
      Action: TCollectionNotification);
  published
    {Order of calling is important due to dependencies:
      Test/Method           Dependencies
      ----------------------+---------------------------------------------------
      SetUp                 | -
      TearDown              | -
      ClearAll              | Clear
      Populate              | Add(Key,Value)
      TestAdd               | -
      TestAddPair           | -
      TestClear             | Count, Populate
      TestCount             | Populate
      TestFind              | Populate, ClearAll
      TestIndexOf           | Populate, ClearAll
      TestContains          | Populate, ClearAll
      TestIsEmpty           | Populate, ClearAll
      TestGetEnumerator     | Populate, ClearAll
      TestKeysProp          | Populate, ClearAll
      TestValuesProp        | Populate, ClearAll
      TestDelete;           | Populate, ClearAll, Keys[] (indirectly)
      TestRemove;           | Populate, ClearAll, Keys[] (indirectly)
      TestCreateNoParams    | Count, Keys[]
    }
    // Test the Adds first: no dependencies, but required by everything else
    procedure TestAdd;
    procedure TestAddPair;
    // Test Count next: depends on Add and required by most below
    procedure TestCount;
    // Test Clear next: depends on Add and Count and required by most below
    procedure TestClear;
    // This section in any order: no mutual dependencies
    procedure TestFind;
    procedure TestIndexOf;
    procedure TestContains;
    procedure TestIsEmpty;
    procedure TestGetEnumerator;
    procedure TestKeysProp;
    procedure TestDelete; // indirectly uses Keys[]
    procedure TestRemove; // indirectly uses Keys[]
    procedure TestValuesProp;
    procedure TestItemsProp;  // indirectly uses GetEnumerator
    procedure TestItemsByIndexProp;
    // CreateNoParams requires Count and Keys[] properties
    procedure TestCreateNoParams;
    procedure TestNotifications;
  end;

  // Test methods for class TSortedObjectDictionary
  // We only test new properties and constructors over and above parent
  // TSortedDictionary class. We assume that any inherited methods called in
  // test have already been tested, so run this test *after*
  // TestTSortedDictionary tests
  TestTSortedObjectDictionary = class(TTestCase)
  published
    procedure TestCreate1;
    procedure TestCreate2;
    procedure TestCreate3;
    procedure TestCreate4;
    procedure TestOwnerships;

  end;

implementation

uses
  SysUtils, Types, Generics.Defaults;

const
  // number of entries in list (count)
  cNumEntries = 6;
  // list items
  cItems: array[0..Pred(cNumEntries)] of string = (
    'glo', 'peter', 'donna', 'keith', 'wendy', 'ann'
  );
  // dictionary keys
  cKeys: array[0..Pred(cNumEntries)] of string = (
    'glo', 'peter', 'donna', 'keith', 'wendy', 'ann'
  );
  // matching values
  cValues: array[0..Pred(cNumEntries)] of Integer = (
    60, 51, 32, 30, 40, 67
  );

  // items in list order
  cSortedItems: array[0..Pred(cNumEntries)] of string = (
    'ann', 'donna', 'glo', 'keith', 'peter', 'wendy'
  );
  // keys and values in dictionary order
  cSortedKeys: array[0..Pred(cNumEntries)] of string = (
    'ann', 'donna', 'glo', 'keith', 'peter', 'wendy'
  );
  cSortedValues: array[0..Pred(cNumEntries)] of Integer = (
    67, 32, 60, 30, 51, 40
  );

  // maps index into cItems to index in list
  cIndexInList: array[0..Pred(cNumEntries)] of Integer = (
    2, 4, 1, 3, 5, 0
  );
  // maps index into cKeys to index in dictionary
  cIndexInDict: array[0..Pred(cNumEntries)] of Integer = (
    2, 4, 1, 3, 5, 0
  );
  // list of indexes of items / keys from cItems / cKeys as they are entered
  // into list / dictionary : i.e. index returned by Add method: assumes items
  // are entered in order from index 0 to cNumEntries - 1
  cSortMapAsEntered: array[0..Pred(cNumEntries)] of Integer = (
    0, 1, 0, 2, 4, 0
  );

  // duplicate item: item value followed by expected position(s) in list if
  // added last: could be one of two positions: before or after existing item
  cDupItem = 'donna';
  cDupItemIndexInList1 = 1;
  cDupItemIndexInList2 = 2;
  // item not in list
  cMissingItem = 'gwen';

  // duplicate key
  cDupKey = 'donna';
  // key not in dictionary
  cMissingKey = 'gwen';

  // order of deletion in delete test
  cDeleteOrder: array[0..Pred(cNumEntries)] of string = (
    'wendy', 'ann', 'glo', 'peter', 'donna', 'keith'
  );

type
  TArrayEx = class(TArray)
  public
    class function CloneArray<T>(const A: array of T): TArray<T>;
    class function IndexOf<T>(const Elem: T; const Items: array of T): Integer;
    class function RemoveItem<T>(const Elem: T; const Items: array of T):
      TArray<T>;
  end;

{ TArrayEx<T> }

class function TArrayEx.CloneArray<T>(const A: array of T): TArray<T>;
var
  Idx: Integer;
begin
  SetLength(Result, Length(A));
  for Idx := Low(A) to High(A) do
    Result[Idx - Low(A)] := A[Idx];
end;

class function TArrayEx.IndexOf<T>(const Elem: T;
  const Items: array of T): Integer;
var
  Idx: Integer;
begin
  for Idx := Low(Items) to High(Items) do
  begin
    if TComparer<T>.Default.Compare(Elem, Items[Idx]) = 0 then
      Exit(Idx);
  end;
  Result := -1;
end;

class function TArrayEx.RemoveItem<T>(const Elem: T;
  const Items: array of T): TArray<T>;
var
  Idx1: Integer;
  Idx2: Integer;
begin
  SetLength(Result, Length(Items) - 1);
  Idx2 := 0;
  for Idx1 := 0 to Pred(Length(Items)) do
  begin
    if TComparer<T>.Default.Compare(Items[Idx1], Elem) <> 0 then
    begin
      Result[Idx2] := Items[Idx1];
      Inc(Idx2);
    end;
  end;
end;


function SameListAndArray(const L: TSortedList<string>;
  const A: array of string): Boolean;
var
  Idx: Integer;
begin
  for Idx := 0 to Pred(L.Count) do
  begin
    if L[Idx] <> A[Idx] then
      Exit(False);
  end;
  Result := True;
end;

function SameDictAndArray(const D: TSortedDictionary<string,Integer>;
  const A: array of string): Boolean;
var
  Idx: Integer;
begin
  for Idx := 0 to Pred(D.Count) do
  begin
    if D.Keys[Idx] <> A[Idx] then
      Exit(False);
  end;
  Result := True;
end;

type
  // Used for testing automatic freeing of owned objects
  TRefCountedStringObject = class(TObject)
  strict private
    fValue: string;
  public
    class var Instances: Integer;
    constructor Create(const AValue: string = '');
    destructor Destroy; override;
    property Value: string read fValue write fValue;
  end;

{ TRefCountedStringObject }

constructor TRefCountedStringObject.Create(const AValue: string);
begin
  inherited Create;
  fValue := AValue;
  Inc(Instances);
end;

destructor TRefCountedStringObject.Destroy;
begin
  Dec(Instances);
  inherited;
end;

{ TestTSortedList }

procedure TestTSortedList.ClearAll;
begin
  fList.Clear;
end;

procedure TestTSortedList.ErrorAddDuplicate;
begin
  fList.Add(cDupItem);
end;

procedure TestTSortedList.ErrorPermitDuplicates;
begin
  // should be called with fList.PermitDuplicates = True and duplicates in list.
  Assert(fList.PermitDuplicates);
  fList.PermitDuplicates := False;
end;

procedure TestTSortedList.OnNotifyHandler(Sender: TObject; const Item: string;
  Action: TCollectionNotification);
begin
  fNotifyList.Add(TPair<string,TCollectionNotification>.Create(Item, Action));
end;

procedure TestTSortedList.Populate;
var
  Idx: Integer;
begin
  for Idx := 0 to Pred(cNumEntries) do
    fList.Add(cItems[Idx]);
end;

procedure TestTSortedList.SetUp;
begin
  fList :=  TSortedList<string>.Create(
    TComparer<string>.Construct(
      function(const Left, Right: string): Integer
      begin
        Result := AnsiCompareText(Left, Right);
      end
    )
  );
  fList.PermitDuplicates := False;
end;

procedure TestTSortedList.TearDown;
begin
  FreeAndNil(fList);
end;

procedure TestTSortedList.TestAdd;
var
  ReturnValue: Integer;
  Idx: Integer;
begin
  // Get fresh, empty list without using Populate
  TearDown;
  SetUp;
  for Idx := 0 to Pred(cNumEntries) do
  begin
    ReturnValue := fList.Add(cItems[Idx]);
    Check(ReturnValue = cSortMapAsEntered[Idx],
      Format('fList.Add(%s): Expected return value %d, got %d',
        [cItems[Idx], cSortMapAsEntered[Idx], ReturnValue]));
  end;
end;

procedure TestTSortedList.TestAddDuplicates;
var
  ReturnValue: Integer;
begin
  // check adding duplicate to list with no dups where dups are permitted:
  // check index where item added in list
  ClearAll;
  Populate;
  fList.PermitDuplicates := True;
  ReturnValue := fList.Add(cDupItem);
  Check(ReturnValue in [cDupItemIndexInList1, cDupItemIndexInList2],
    Format('fList.Add(dup of %s): Expected return of %d or %d, got %d',
      [cDupItem, cDupItemIndexInList1, cDupItemIndexInList2, ReturnValue]));

  // check adding dup to list with no dups where dups are not permitted
  ClearAll;
  Populate;
  fList.PermitDuplicates := False;
  CheckException(ErrorAddDuplicate, EListError);
end;

procedure TestTSortedList.TestClear;
begin
  // Create fresh, empty dictionary without using ClearAll
  TearDown;
  SetUp;
  Populate;
  Assert(fList.Count > 0);
  fList.Clear;
  CheckEquals(0, fList.Count,
    Format('fList.Count: Expected 0, got %d', [fList.Count]));
end;

procedure TestTSortedList.TestContains;
var
  Item: string;
begin
  ClearAll;
  Check(not fList.Contains(cItems[0]),
    Format('fList.Contains(%s): Expected False, got True', [cItems[0]]));
  Populate;
  Check(fList.Contains(cItems[0]),
    Format('fList.Contains(%s): Expected True, got False', [cItems[0]]));
  Item := cMissingItem;
  Check(not fList.Contains(Item),
    Format('fList.Contains(%s): Expected False, got True', [Item]));
end;

procedure TestTSortedList.TestContainsDuplicates;
begin
  ClearAll;
  fList.PermitDuplicates := True;
  Populate; // no dupes
  Check(not fList.ContainsDuplicates,
    'fList.ContainsDuplicates: Expected False, got True');
  fList.Add(cDupItem);
  Check(fList.ContainsDuplicates,
    'fList.ContainsDuplicates: Expected True, got False');
end;

procedure TestTSortedList.TestCount;
begin
  // Create fresh, empty dictionary without using ClearAll
  TearDown;
  SetUp;
  CheckEquals(0, fList.Count,
    Format('fList.Count: Expected 0, got %d', [fList.Count]));
  Populate;
  CheckEquals(cNumEntries, fList.Count,
    Format('fList.Count: Expected %d, got %d', [cNumEntries, fList.Count]));
end;

procedure TestTSortedList.TestCreateNoParams;
var
  L: TSortedList<Integer>;
  Idx: Integer;
begin
  // Testing constructor with parameters => use default ordering, so we keep
  // type simple so that ordering is likely to be as expected.
  L := TSortedList<Integer>.Create;
  try
    // here item is expected position in list
    // (we add 0..6 out of order and expect it to be sorted)
    L.Add(4);
    L.Add(2);
    L.Add(1);
    L.Add(3);
    L.Add(5);
    L.Add(0);
    for Idx := 0 to Pred(L.Count) do
      CheckEquals(Idx, L[Idx]);
  finally
    L.Free;
  end;
end;

procedure TestTSortedList.TestDelete;
var
  Remaining: TArray<string>;
  RemoveStr: string;
  Idx: Integer;
begin
  ClearAll;
  Populate;
  Remaining := TArrayEx.CloneArray<string>(cSortedItems);
  Assert(SameListAndArray(fList, Remaining), 'Initial result in error');
  for RemoveStr in cSortedItems do
  begin
    Idx := TArrayEx.IndexOf<string>(RemoveStr, Remaining);
    fList.Delete(Idx);
    Remaining := TArrayEx.RemoveItem<string>(RemoveStr, Remaining);
    Check(SameListAndArray(fList, Remaining),
      Format('fList.Delete: Error removing %s, index %d', [RemoveStr, Idx]));
  end;
end;

procedure TestTSortedList.TestFind;
var
  ReturnValue: Boolean;
  Index: Integer;
  Item: string;
  Idx: Integer;
begin
  ClearAll;
  Populate;
  for Idx := 0 to Pred(cNumEntries) do
  begin
    Item := cItems[Idx];
    ReturnValue := fList.Find(Item, Index);
    Check(ReturnValue,
      Format('fList.Find(%s,Index): Expected return of True, got False',
        [Item]));
    CheckEquals(cIndexInList[Idx], Index,
      Format('fList.Find(%s,Index): Expected Index = %d, got %d',
        [Item, cIndexInList[Idx], Index]));
  end;
  Item := cMissingItem;
  ReturnValue := fList.Find(Item, Index);
  Check(not ReturnValue,
    Format('fList.Find(%s,Index): Expected return of False, got True', [Item]));
end;

procedure TestTSortedList.TestGetEnumerator;
var
  Idx: Integer;
  Item: string;
begin
  ClearAll;
  Populate;
  Idx := 0;
  for Item in fList do
  begin
    CheckEquals(cSortedItems[Idx], Item,
      Format('fList.GetEnumerator: Expected item %s, got %s',
        [cSortedItems[Idx], Item]));
    Inc(Idx);
  end;
end;

procedure TestTSortedList.TestIndexOf;
var
  ReturnValue: Integer;
  Item: string;
  Idx: Integer;
begin
  ClearAll;
  Populate;
  for Idx := 0 to Pred(cNumEntries) do
  begin
    Item := cItems[Idx];
    ReturnValue := fList.IndexOf(Item);
    CheckEquals(cIndexInList[Idx], ReturnValue,
      Format('fList.IndexOf(%s): Expected %d, got %d',
        [Item, cIndexInList[Idx], ReturnValue]));
  end;
  Item := cMissingItem;
  ReturnValue := fList.IndexOf(Item);
  CheckEquals(-1, ReturnValue,
    Format('fList.IndexOf(%s): Expected -1, got %d', [Item, ReturnValue]));
end;

procedure TestTSortedList.TestIsEmpty;
begin
  ClearAll;
  Check(fList.IsEmpty, 'fList.IsEmpty: Expected True, got False');
  Populate;
  Check(not fList.IsEmpty, 'fList.IsEmpty: Expected False, got True');
end;

procedure TestTSortedList.TestItemsProp;
var
  Idx: Integer;
begin
  ClearAll;
  Populate;
  for Idx := 0 to Pred(cNumEntries) do
  begin
    CheckEqualsString(cSortedItems[Idx], fList[Idx],
      Format('fList[%d]: Expected %s, got %s',
        [Idx, cSortedItems[Idx], fList[Idx]]));
  end;
end;

procedure TestTSortedList.TestOnNotify;

  function CheckNotifyListItem(const Idx: Integer; const S: string;
    A: TCollectionNotification): Boolean;
  begin
    Result := (fNotifyList[Idx].Key = S) and (fNotifyList[Idx].Value = A);
  end;

var
  Idx: Integer;
begin
  fNotifyList := TList<TPair<string,TCollectionNotification>>.Create;
  try
    ClearAll;
    fList.OnNotify := OnNotifyHandler;
    Populate;
    for Idx := Low(cItems) to High(cItems) do
      Check(CheckNotifyListItem(Idx - Low(cItems), cItems[Idx], cnAdded),
        Format('Expected "%s" and cnAdded', [cItems[Idx]]));
    fNotifyList.Clear;
    fList.Delete(0);
    Check(CheckNotifyListItem(0, cSortedItems[0], cnRemoved),
      Format('Expected "%s" and cnRemoved', [cSortedItems[0]]));
    // we don't check for cnExtract since extraction is not supported by
    // TSortedList at present
  finally
    fList.OnNotify := nil;
    FreeAndNil(fNotifyList);
  end;
end;

procedure TestTSortedList.TestPermitDuplicates;
begin
  // check setting false and true both work with empty list
  ClearAll;
  fList.PermitDuplicates := False;
  Check(not fList.PermitDuplicates, 'Expected fList.PermitDuplicates = False');
  fList.PermitDuplicates := True;
  Check(fList.PermitDuplicates, 'Expected fList.PermitDuplicates = True');

  // check setting false and true both work with populated list with no dups
  Populate;
  fList.PermitDuplicates := False;
  Check(not fList.PermitDuplicates, 'Expected fList.PermitDuplicates = False');
  fList.PermitDuplicates := True;
  Check(fList.PermitDuplicates, 'Expected fList.PermitDuplicates = True');

  // check setting false triggers exception with populated list with dups
  Assert(fList.PermitDuplicates);
  fList.Add(cDupItem);
  CheckException(ErrorPermitDuplicates, EListError);
end;

procedure TestTSortedList.TestRemove;
var
  Remaining: TArray<string>;
  RemoveStr: string;
  ExpectedIdx: Integer;
  Idx: Integer;
begin
  ClearAll;
  Populate;
  Remaining := TArrayEx.CloneArray<string>(cSortedItems);
  Assert(SameListAndArray(fList, Remaining), 'Initial result in error');
  for RemoveStr in cSortedItems do
  begin
    ExpectedIdx := TArrayEx.IndexOf<string>(RemoveStr, Remaining);
    Idx := fList.Remove(RemoveStr);
    CheckEquals(ExpectedIdx, Idx,
      Format('fList.Remove: Expected return value %d, got %d',
        [ExpectedIdx, Idx]));
    Remaining := TArrayEx.RemoveItem<string>(RemoveStr, Remaining);
    Check(SameListAndArray(fList, Remaining),
      Format('fList.Remove: Error removing %s', [RemoveStr]));
  end;
end;

{ TestTSortedObjectList }

procedure TestTSortedObjectList.TestOwnsObjects;
var
  L: TSortedObjectList<TRefCountedStringObject>;
  O1, O2, O3: TRefCountedStringObject;
begin
  // Check OwnsObjects = True
  L := TSortedObjectList<TRefCountedStringObject>.Create(
    TDelegatedComparer<TRefCountedStringObject>.Create(
      function(const Left, Right: TRefCountedStringObject): Integer
      begin
        Result := AnsiCompareText(Left.Value, Right.Value);
      end
    ),
    True
  );
  try
    CheckEquals(True, L.OwnsObjects);
    L.Add(TRefCountedStringObject.Create('one'));
    L.Add(TRefCountedStringObject.Create('two'));
    L.Add(TRefCountedStringObject.Create('three'));
    CheckEquals(3, TRefCountedStringObject.Instances);
  finally
    L.Free;
    CheckEquals(0, TRefCountedStringObject.Instances);
  end;

  // Check OwnsObjects = False
  O1 := nil;
  O2 := nil;
  O3 := nil;
  L := TSortedObjectList<TRefCountedStringObject>.Create(
    TDelegatedComparer<TRefCountedStringObject>.Create(
      function(const Left, Right: TRefCountedStringObject): Integer
      begin
        Result := AnsiCompareText(Left.Value, Right.Value);
      end
    ),
    False
  );
  try
    CheckEquals(False, L.OwnsObjects);
    O1 := TRefCountedStringObject.Create('one');
    O2 := TRefCountedStringObject.Create('two');
    O3 := TRefCountedStringObject.Create('three');
    L.Add(O1);
    L.Add(O2);
    L.Add(O3);
    CheckEquals(3, TRefCountedStringObject.Instances);
  finally
    L.Free;
    CheckEquals(3, TRefCountedStringObject.Instances);
    O3.Free;
    O2.Free;
    O1.Free;
    CheckEquals(0, TRefCountedStringObject.Instances);
  end;

  // Check that OwnsObjects defaults to False when constructors that don't take
  // AOwnsObject parameter are called
  L := TSortedObjectList<TRefCountedStringObject>.Create(
    TDelegatedComparer<TRefCountedStringObject>.Create(
      function(const Left, Right: TRefCountedStringObject): Integer
      begin
        Result := AnsiCompareText(Left.Value, Right.Value);
      end
    )
  );
  try
    CheckEquals(False, L.OwnsObjects);
  finally
    L.Free;
  end;
  L := TSortedObjectList<TRefCountedStringObject>.Create;
  try
    CheckEquals(False, L.OwnsObjects);
  finally
    L.Free;
  end;
end;

{ TestTSortedDictionary }

procedure TestTSortedDictionary.ClearAll;
begin
  fDict.Clear;
end;

procedure TestTSortedDictionary.ErrorAdd;
begin
  fDict.Add(cDupKey, 3); // duplicate key, value irrelevant
end;

procedure TestTSortedDictionary.ErrorAddPair;
begin
  fDict.Add(TPair<string,Integer>.Create(cDupKey, 3)); // duplicate key
end;

procedure TestTSortedDictionary.ErrorItems;
begin
  fDict['xxx'];
end;

procedure TestTSortedDictionary.ErrorKeys;
begin
  fDict.Keys[23];
end;

procedure TestTSortedDictionary.ErrorValues;
begin
  fDict.Values[23];
end;

procedure TestTSortedDictionary.OnKeyNotifyHandler(Sender: TObject;
  const Key: string; Action: TCollectionNotification);
begin
  fLastKeyNotification.Key := Key;
  fLastKeyNotification.Value := Action;
end;

procedure TestTSortedDictionary.OnValueNotifyHandler(Sender: TObject;
  const Value: Integer; Action: TCollectionNotification);
begin
  fLastValueNotification.Key := Value;
  fLastValueNotification.Value := Action;
end;

procedure TestTSortedDictionary.Populate;
var
  Idx: Integer;
begin
  for Idx := 0 to Pred(cNumEntries) do
    fDict.Add(cKeys[Idx], cValues[Idx]);
end;

procedure TestTSortedDictionary.SetUp;
begin
  fDict := TSortedDictionary<string,Integer>.Create(
    TComparer<string>.Construct(
      function(const Left, Right: string): Integer
      begin
        Result := AnsiCompareText(Left, Right);
      end
    )
  );
end;

procedure TestTSortedDictionary.TearDown;
begin
  FreeAndNil(fDict);
end;

procedure TestTSortedDictionary.TestAdd;
var
  ReturnValue: Integer;
  Idx: Integer;
begin
  // Get fresh, empty dictionary without using Populate
  TearDown;
  SetUp;
  for Idx := 0 to Pred(cNumEntries) do
  begin
    ReturnValue := fDict.Add(cKeys[Idx], cValues[Idx]);
    Check(ReturnValue = cSortMapAsEntered[Idx],
      Format('fDict.Add(%s,%d): Expected return value %d, got %d',
        [cKeys[Idx], cValues[Idx], cSortMapAsEntered[Idx], ReturnValue]));
  end;

  CheckException(ErrorAdd, EListError);
end;

procedure TestTSortedDictionary.TestAddPair;
var
  ReturnValue: Integer;
  Idx: Integer;
begin
  // Get fresh, empty dictionary without using Populate
  TearDown;
  SetUp;
  for Idx := 0 to Pred(cNumEntries) do
  begin
    ReturnValue := fDict.Add(
      TPair<string,Integer>.Create(cKeys[Idx], cValues[Idx])
    );
    Check(ReturnValue = cSortMapAsEntered[Idx],
      Format('fDict.Add(TPair<%s,%d>): Expected return value %d, got %d',
        [cKeys[Idx], cValues[Idx], cSortMapAsEntered[Idx], ReturnValue]));
  end;

  CheckException(ErrorAddPair, EListError);
end;

procedure TestTSortedDictionary.TestClear;
begin
  // Create fresh, empty dictionary without using ClearAll
  TearDown;
  SetUp;
  Populate;
  Assert(fDict.Count > 0);
  fDict.Clear;
  CheckEquals(0, fDict.Count,
    Format('fDict.Count: Expected 0, got %d', [fDict.Count]));
end;

procedure TestTSortedDictionary.TestContains;
var
  Key: string;
begin
  ClearAll;
  Check(not fDict.Contains(cKeys[0]),
    Format('fDict.Contains(%s): Expected False, got True', [cKeys[0]]));
  Populate;
  Check(fDict.Contains(cKeys[0]),
    Format('fDict.Contains(%s): Expected True, got False', [cKeys[0]]));
  Key := cMissingKey;
  Check(not fDict.Contains(Key),
    Format('fDict.Contains(%s): Expected False, got True', [Key]));
end;

procedure TestTSortedDictionary.TestCount;
begin
  // Create fresh, empty dictionary without using ClearAll
  TearDown;
  SetUp;
  CheckEquals(0, fDict.Count,
    Format('fDict.Count: Expected 0, got %d', [fDict.Count]));
  Populate;
  CheckEquals(cNumEntries, fDict.Count,
    Format('fDict.Count: Expected %d, got %d', [cNumEntries, fDict.Count]));
end;

procedure TestTSortedDictionary.TestCreateNoParams;
var
  D: TSortedDictionary<Integer,Integer>;
  Idx: Integer;
begin
  // Testing constructor with parameters => use default ordering, so we keep
  // type simple so that ordering is likely to be as expected.
  D := TSortedDictionary<Integer,Integer>.Create;
  try
    // here key is expected position in dictionary, value is order added
    D.Add(4, 1);
    D.Add(2, 2);
    D.Add(1, 3);
    D.Add(3, 4);
    D.Add(5, 5);
    D.Add(0, 6);
    for Idx := 0 to Pred(D.Count) do
      CheckEquals(Idx, D.Keys[Idx]);
  finally
    D.Free;
  end;
end;

procedure TestTSortedDictionary.TestDelete;
var
  Remaining: TArray<string>;
  RemoveStr: string;
  Idx: Integer;
begin
  ClearAll;
  Populate;
  Remaining := TArrayEx.CloneArray<string>(cSortedKeys);
  Assert(SameDictAndArray(fDict, Remaining), 'Initial result in error');
  for RemoveStr in cSortedKeys do
  begin
    Idx := TArrayEx.IndexOf<string>(RemoveStr, Remaining);
    fDict.Delete(Idx);
    Remaining := TArrayEx.RemoveItem<string>(RemoveStr, Remaining);
    Check(SameDictAndArray(fDict, Remaining),
      Format('fDict.Delete(%1:d): Error removing %0:s from index %1:d',
        [RemoveStr, Idx]));
  end;
end;

procedure TestTSortedDictionary.TestFind;
var
  ReturnValue: Boolean;
  Index: Integer;
  Key: string;
  Idx: Integer;
begin
  ClearAll;
  Populate;
  for Idx := 0 to Pred(cNumEntries) do
  begin
    Key := cKeys[Idx];
    ReturnValue := fDict.Find(Key, Index);
    Check(ReturnValue,
      Format('fDict.Find(%s,Index): Expected return of True, got False',
        [Key]));
    CheckEquals(cIndexInDict[Idx], Index,
      Format('fDict.Find(%s,Index): Expected Index = %d, got %d',
        [Key, cIndexInDict[Idx], Index]));
  end;
  Key := cMissingKey;
  ReturnValue := fDict.Find(Key, Index);
  Check(not ReturnValue,
    Format('fDict.Find(%s,Index): Expected return of False, got True', [Key]));
end;

procedure TestTSortedDictionary.TestGetEnumerator;
var
  Idx: Integer;
  Pair: TPair<string,Integer>;
begin
  ClearAll;
  Populate;
  Idx := 0;
  for Pair in fDict do
  begin
    CheckEquals(cSortedKeys[Idx], Pair.Key,
      Format('fDict.Enumerator: Expected key %s, got %s',
        [cSortedKeys[Idx], Pair.Key]));
    CheckEquals(cSortedValues[Idx], Pair.Value,
      Format('fDict.Enumerator: Expected value %d, got %d',
        [cSortedValues[Idx], Pair.Value]));
    Inc(Idx);
  end;
end;

procedure TestTSortedDictionary.TestIndexOf;
var
  ReturnValue: Integer;
  Key: string;
  Idx: Integer;
begin
  ClearAll;
  Populate;
  for Idx := 0 to Pred(cNumEntries) do
  begin
    Key := cKeys[Idx];
    ReturnValue := fDict.IndexOf(Key);
    CheckEquals(cIndexInDict[Idx], ReturnValue,
      Format('fDict.IndexOf(%s): Expected %d, got %d',
        [Key, cIndexInDict[Idx], ReturnValue]));
  end;
  Key := cMissingKey;
  ReturnValue := fDict.IndexOf(Key);
  CheckEquals(-1, ReturnValue,
    Format('fDict.IndexOf(%s): Expected -1, got %d', [Key, ReturnValue]));
end;

procedure TestTSortedDictionary.TestIsEmpty;
begin
  ClearAll;
  Check(fDict.IsEmpty, 'fDict.IsEmpty: Expected True, got False');
  Populate;
  Check(not fDict.IsEmpty, 'fDict.IsEmpty: Expected False, got True');
end;

procedure TestTSortedDictionary.TestItemsByIndexProp;
var
  Pair: TPair<string,Integer>;
  Idx: Integer;
begin
  ClearAll;
  Populate;
  for Idx := 0 to Pred(fDict.Count) do
  begin
    Pair := fDict.ItemsByIndex[Idx];
    Check(Pair.Key = cSortedKeys[Idx],
      Format('Expected key %s, got %s', [cSortedKeys[Idx], Pair.Key]));
    Check(Pair.Value = cSortedValues[Idx],
      Format('Expected value %d, got %d', [cSortedValues[Idx], Pair.Value]));
  end;
end;

procedure TestTSortedDictionary.TestItemsProp;
var
  Value: Integer;
  Pair: TPair<string,Integer>;
begin
  ClearAll;
  Populate;
  for Pair in fDict do
  begin
    Value := fDict[Pair.Key];
    Check(Value = Pair.Value,
      Format('Value %d expected for key %s but got %d',
        [Pair.Value, Pair.Key, Value]));
  end;
  // unknown key
  CheckException(ErrorItems, EListError);
end;

procedure TestTSortedDictionary.TestKeysProp;
var
  Idx: Integer;
  TestValues: TArray<string>;
  Value: string;
begin
  // First test accessing by index
  ClearAll;
  Populate;
  for Idx := 0 to Pred(cNumEntries) do
  begin
    CheckEqualsString(cSortedKeys[Idx], fDict.Keys[Idx],
    Format('fDict.Keys[%d]: Expected %s, got %s',
      [Idx, cSortedKeys[Idx], fDict.Keys[Idx]]));
  end;
  // index out of bounds
  CheckException(ErrorKeys, EArgumentOutOfRangeException);

  // Second test access by enumerator
  SetLength(TestValues, Length(cKeys));
  for Idx := Low(cKeys) to High(cKeys) do
    TestValues[Idx - Low(cKeys)] := cKeys[Idx];
  for Value in fDict.Keys do
  begin
    Check(TArrayEx.IndexOf<string>(Value, TestValues) >= 0,
      Format('Keys enumeration contains unexpected value %s', [Value]));
    TestValues := TArrayEx.RemoveItem<string>(Value, TestValues);
  end;
  Check(Length(TestValues) = 0,
    'Keys enumeration failed: not all values were enumerated');
end;

procedure TestTSortedDictionary.TestNotifications;

  function CheckNotification(const S: string; const I: Integer;
    const A: TCollectionNotification): Boolean;
  begin
    Result := (fLastKeyNotification.Key = S) and
      (fLastKeyNotification.Value = A) and
      (fLastValueNotification.Key = I) and
      (fLastValueNotification.Value = A);
  end;

begin
  ClearAll;
  try
    fDict.OnKeyNotify := OnKeyNotifyHandler;
    fDict.OnValueNotify := OnValueNotifyHandler;
    fDict.Add('one', 1);
    Check(CheckNotification('one', 1, cnAdded),
      'Expected "one" => 1 with cnAdded');
    fDict.Add('two', 2);
    Check(CheckNotification('two', 2, cnAdded),
      'Expected "two" => 2 with cnAdded');
    fDict.Remove('two');
    Check(CheckNotification('two', 2, cnRemoved),
      'Expected "two" => 2 with cnRemoved');
    fDict.Delete(0);
    Check(CheckNotification('one', 1, cnRemoved),
      'Expected "one" => 1 with cnRemoved');
    // we don't check for cnExtract since extraction is not supported by
    // TSortedDictionary at present
  finally
    fDict.OnKeyNotify := nil;
    fDict.OnValueNotify := nil;
    ClearAll;
  end;
end;

procedure TestTSortedDictionary.TestRemove;
var
  Remaining: TArray<string>;
  RemoveStr: string;
  ExpectedIdx: Integer;
  Idx: Integer;
begin
  ClearAll;
  Populate;
  Remaining := TArrayEx.CloneArray<string>(cSortedKeys);
  Assert(SameDictAndArray(fDict, Remaining), 'Initial result in error');
  for RemoveStr in cSortedKeys do
  begin
    ExpectedIdx := TArrayEx.IndexOf<string>(RemoveStr, Remaining);
    Idx := fDict.Remove(RemoveStr);
    CheckEquals(ExpectedIdx, Idx,
      Format('fDict.Remove(%s): Expected return value %d, got %d',
        [RemoveStr, ExpectedIdx, Idx]));
    Remaining := TArrayEx.RemoveItem<string>(RemoveStr, Remaining);
    Check(SameDictAndArray(fDict, Remaining),
      Format('fDict.Remove(%0:s): Error removing %0:s', [RemoveStr]));
  end;
end;

procedure TestTSortedDictionary.TestValuesProp;
var
  Idx: Integer;
  Value: Integer;
  TestValues: TArray<Integer>;
begin
  // First test accessing by index
  ClearAll;
  Populate;
  for Idx := 0 to Pred(cNumEntries) do
  begin
    Value := fDict.Values[Idx];
    CheckEquals(cSortedValues[Idx], Value,
      Format('fDict[%d]: Expected %d, got %d',
        [Idx, cSortedValues[Idx], Value]));
  end;
  // index out of bounds
  CheckException(ErrorValues, EArgumentOutOfRangeException);

  // Second test access by enumerator
  TestValues := TArrayEx.CloneArray<Integer>(cValues);
  for Value in fDict.Values do
  begin
    Check(TArrayEx.IndexOf<Integer>(Value, TestValues) >= 0,
      Format('Values enumeration contains unexpected value %d', [Value]));
    TestValues := TArrayEx.RemoveItem<Integer>(Value, TestValues);
  end;
  Check(Length(TestValues) = 0,
    'Values enumeration failed: not all values were enumerated');
end;

{ TestTSortedObjectDictionary }

procedure TestTSortedObjectDictionary.TestCreate1;
var
  D: TSortedObjectDictionary<string,Integer>;
begin
  D := TSortedObjectDictionary<string,Integer>.Create;
  try
    Check(D.Ownerships = [], 'Ownerships expected to be []');
  finally
    D.Free;
  end;
end;

procedure TestTSortedObjectDictionary.TestCreate2;
var
  D: TSortedObjectDictionary<TRefCountedStringObject,Integer>;
begin
  D := TSortedObjectDictionary<TRefCountedStringObject,Integer>.Create(
    TDelegatedComparer<TRefCountedStringObject>.Create(
      function(const Left, Right: TRefCountedStringObject): Integer
      begin
        Result := AnsiCompareText(Left.Value, Right.Value);
      end
    )
  );
  try
    Check(D.Ownerships = [], 'Ownerships expected to be []');
  finally
    D.Free;
  end;
end;

procedure TestTSortedObjectDictionary.TestCreate3;
var
  D1: TSortedObjectDictionary<TObject,TObject>;
  D2: TSortedObjectDictionary<TObject,string>;
  D3: TSortedObjectDictionary<string,TObject>;
  D4: TSortedObjectDictionary<string,string>;
begin
  D1 := TSortedObjectDictionary<TObject,TObject>.Create([]);
  try
    Check(D1.Ownerships = [],
      'Ownerships expected to be []');
  finally
    D1.Free;
  end;
  D1 := TSortedObjectDictionary<TObject,TObject>.Create([doOwnsKeys]);
  try
    Check(D1.Ownerships = [doOwnsKeys],
      'Ownerships expected to be [doOwnsKeys]');
  finally
    D1.Free;
  end;
  D1 := TSortedObjectDictionary<TObject,TObject>.Create([doOwnsValues]);
  try
    Check(D1.Ownerships = [doOwnsValues],
      'Ownerships expected to be [doOwnsValues]');
  finally
    D1.Free;
  end;
  D1 := TSortedObjectDictionary<TObject,TObject>.Create(
    [doOwnsKeys,doOwnsValues]
  );
  try
    Check(D1.Ownerships = [doOwnsKeys,doOwnsValues],
      'Ownerships expected to be [doOwnsKeys,doOwnsValues]');
  finally
    D1.Free;
  end;

  D2 := TSortedObjectDictionary<TObject,string>.Create([]);
  try
    Check(D2.Ownerships = [],
      'Ownerships expected to be []');
  finally
    D2.Free;
  end;
  D2 := TSortedObjectDictionary<TObject,string>.Create([doOwnsKeys]);
  try
    Check(D2.Ownerships = [doOwnsKeys],
      'Ownerships expected to be [doOwnsKeys]');
  finally
    D2.Free;
  end;
  try
    D2 := nil;
    try
      D2 := TSortedObjectDictionary<TObject,string>.Create([doOwnsValues]);
      Fail('EInvalidCast Exception expected for [doOwnsValues]');
    finally
      D2.Free;
    end;
  except
    on E: Exception do
      Check(E is EInvalidCast, 'EInvalid cast exception expected');
  end;
  try
    D2 := nil;
    try
      D2 := TSortedObjectDictionary<TObject,string>.Create(
        [doOwnsKeys,doOwnsValues]
      );
      Fail('EInvalidCast Exception expected for [doOwnsKeys,doOwnsValues]');
    finally
      D2.Free;
    end;
  except
    on E: Exception do
      Check(E is EInvalidCast, 'EInvalid cast exception expected');
  end;

  D3 := TSortedObjectDictionary<string,TObject>.Create([]);
  try
    Check(D3.Ownerships = [],
      'Ownerships expected to be []');
  finally
    D3.Free;
  end;
  try
    D3 := nil;
    try
      D3 := TSortedObjectDictionary<string,TObject>.Create([doOwnsKeys]);
      Fail('EInvalidCast Exception expected for [doOwnsKeys]');
    finally
      D3.Free;
    end;
  except
    on E: Exception do
      Check(E is EInvalidCast, 'EInvalid cast exception expected');
  end;
  D3 := TSortedObjectDictionary<string,TObject>.Create([doOwnsValues]);
  try
    Check(D3.Ownerships = [doOwnsValues],
      'Ownerships expected to be [doOwnsValues]');
  finally
    D3.Free;
  end;
  try
    D3 := nil;
    try
      D3 := TSortedObjectDictionary<string,TObject>.Create(
        [doOwnsKeys,doOwnsValues]
      );
      Fail('EInvalidCast Exception expected for [doOwnsKeys,doOwnsValues]');
    finally
      D3.Free;
    end;
  except
    on E: Exception do
      Check(E is EInvalidCast, 'EInvalid cast exception expected');
  end;

  D4 := TSortedObjectDictionary<string,string>.Create([]);
  try
    Check(D4.Ownerships = [],
      'Ownerships expected to be []');
  finally
    D4.Free;
  end;
  try
    D4 := nil;
    try
      D4 := TSortedObjectDictionary<string,string>.Create([doOwnsKeys]);
      Fail('EInvalidCast Exception expected for [doOwnsKeys]');
    finally
      D4.Free;
    end;
  except
    on E: Exception do
      Check(E is EInvalidCast, 'EInvalid cast exception expected');
  end;
  try
    D4 := nil;
    try
      D4 := TSortedObjectDictionary<string,string>.Create([doOwnsValues]);
      Fail('EInvalidCast Exception expected for [doOwnsValues]');
    finally
      D4.Free;
    end;
  except
    on E: Exception do
      Check(E is EInvalidCast, 'EInvalid cast exception expected');
  end;
  try
    D4 := nil;
    try
      D4 := TSortedObjectDictionary<string,string>.Create(
        [doOwnsKeys,doOwnsValues]
      );
      Fail('EInvalidCast Exception expected for [doOwnsKeys,doOwnsValues]');
    finally
      D4.Free;
    end;
  except
    on E: Exception do
      Check(E is EInvalidCast, 'EInvalid cast exception expected');
  end;
end;

procedure TestTSortedObjectDictionary.TestCreate4;
var
  D1: TSortedObjectDictionary<TObject,TObject>;
  D2: TSortedObjectDictionary<TObject,string>;
  D3: TSortedObjectDictionary<string,TObject>;
  D4: TSortedObjectDictionary<string,string>;
begin
  D1 := TSortedObjectDictionary<TObject,TObject>.Create(
    TComparer<TObject>.Default, []
  );
  try
    Check(D1.Ownerships = [],
      'Ownerships expected to be []');
  finally
    D1.Free;
  end;
  D1 := TSortedObjectDictionary<TObject,TObject>.Create(
    TComparer<TObject>.Default, [doOwnsKeys]
  );
  try
    Check(D1.Ownerships = [doOwnsKeys],
      'Ownerships expected to be [doOwnsKeys]');
  finally
    D1.Free;
  end;
  D1 := TSortedObjectDictionary<TObject,TObject>.Create(
    TComparer<TObject>.Default, [doOwnsValues]
  );
  try
    Check(D1.Ownerships = [doOwnsValues],
      'Ownerships expected to be [doOwnsValues]');
  finally
    D1.Free;
  end;
  D1 := TSortedObjectDictionary<TObject,TObject>.Create(
    TComparer<TObject>.Default, [doOwnsKeys,doOwnsValues]
  );
  try
    Check(D1.Ownerships = [doOwnsKeys,doOwnsValues],
      'Ownerships expected to be [doOwnsKeys,doOwnsValues]');
  finally
    D1.Free;
  end;

  D2 := TSortedObjectDictionary<TObject,string>.Create(
    TComparer<TObject>.Default, []
  );
  try
    Check(D2.Ownerships = [],
      'Ownerships expected to be []');
  finally
    D2.Free;
  end;
  D2 := TSortedObjectDictionary<TObject,string>.Create(
    TComparer<TObject>.Default, [doOwnsKeys])
  ;
  try
    Check(D2.Ownerships = [doOwnsKeys],
      'Ownerships expected to be [doOwnsKeys]');
  finally
    D2.Free;
  end;
  try
    D2 := nil;
    try
      D2 := TSortedObjectDictionary<TObject,string>.Create(
        TComparer<TObject>.Default, [doOwnsValues]
      );
      Fail('EInvalidCast Exception expected for [doOwnsValues]');
    finally
      D2.Free;
    end;
  except
    on E: Exception do
      Check(E is EInvalidCast, 'EInvalid cast exception expected');
  end;
  try
    D2 := nil;
    try
      D2 := TSortedObjectDictionary<TObject,string>.Create(
        TComparer<TObject>.Default, [doOwnsKeys,doOwnsValues]
      );
      Fail('EInvalidCast Exception expected for [doOwnsKeys,doOwnsValues]');
    finally
      D2.Free;
    end;
  except
    on E: Exception do
      Check(E is EInvalidCast, 'EInvalid cast exception expected');
  end;

  D3 := TSortedObjectDictionary<string,TObject>.Create(
    TComparer<string>.Default, []
  );
  try
    Check(D3.Ownerships = [],
      'Ownerships expected to be []');
  finally
    D3.Free;
  end;
  try
    D3 := nil;
    try
      D3 := TSortedObjectDictionary<string,TObject>.Create(
        TComparer<string>.Default, [doOwnsKeys]
      );
      Fail('EInvalidCast Exception expected for [doOwnsKeys]');
    finally
      D3.Free;
    end;
  except
    on E: Exception do
      Check(E is EInvalidCast, 'EInvalid cast exception expected');
  end;
  D3 := TSortedObjectDictionary<string,TObject>.Create(
    TComparer<string>.Default, [doOwnsValues]
  );
  try
    Check(D3.Ownerships = [doOwnsValues],
      'Ownerships expected to be [doOwnsValues]');
  finally
    D3.Free;
  end;
  try
    D3 := nil;
    try
      D3 := TSortedObjectDictionary<string,TObject>.Create(
        TComparer<string>.Default, [doOwnsKeys,doOwnsValues]
      );
      Fail('EInvalidCast Exception expected for [doOwnsKeys,doOwnsValues]');
    finally
      D3.Free;
    end;
  except
    on E: Exception do
      Check(E is EInvalidCast, 'EInvalid cast exception expected');
  end;

  D4 := TSortedObjectDictionary<string,string>.Create(
    TComparer<string>.Default, []
  );
  try
    Check(D4.Ownerships = [],
      'Ownerships expected to be []');
  finally
    D4.Free;
  end;
  try
    D4 := nil;
    try
      D4 := TSortedObjectDictionary<string,string>.Create(
        TComparer<string>.Default, [doOwnsKeys]
      );
      Fail('EInvalidCast Exception expected for [doOwnsKeys]');
    finally
      D4.Free;
    end;
  except
    on E: Exception do
      Check(E is EInvalidCast, 'EInvalid cast exception expected');
  end;
  try
    D4 := nil;
    try
      D4 := TSortedObjectDictionary<string,string>.Create(
        TComparer<string>.Default, [doOwnsValues]
      );
      Fail('EInvalidCast Exception expected for [doOwnsValues]');
    finally
      D4.Free;
    end;
  except
    on E: Exception do
      Check(E is EInvalidCast, 'EInvalid cast exception expected');
  end;
  try
    D4 := nil;
    try
      D4 := TSortedObjectDictionary<string,string>.Create(
        TComparer<string>.Default, [doOwnsKeys,doOwnsValues]
      );
      Fail('EInvalidCast Exception expected for [doOwnsKeys,doOwnsValues]');
    finally
      D4.Free;
    end;
  except
    on E: Exception do
      Check(E is EInvalidCast, 'EInvalid cast exception expected');
  end;
end;

procedure TestTSortedObjectDictionary.TestOwnerships;
var
  K1, K2: TRefCountedStringObject;
  V1, V2: TRefCountedStringObject;
  D1: TSortedObjectDictionary<TRefCountedStringObject,TRefCountedStringObject>;

  procedure CreateObjs;
  begin
    K1 := TRefCountedStringObject.Create('K1');
    K2 := TRefCountedStringObject.Create('K2');
    V1 := TRefCountedStringObject.Create('V1');
    V2 := TRefCountedStringObject.Create('V2');
  end;

begin
  CreateObjs;
  try
    Check(TRefCountedStringObject.Instances = 4, 'Expected 4 objects');
    D1 := TSortedObjectDictionary<
      TRefCountedStringObject,TRefCountedStringObject
    >.Create([]);
    try
      D1.Add(K1, V1);
      D1.Add(K2, V2);
    finally
      D1.Free;
    end;
    Check(TRefCountedStringObject.Instances = 4, 'Expected 4 objects');
  finally
    K1.Free;
    K2.Free;
    V1.Free;
    V2.Free;
  end;

  CreateObjs;
  try
    Check(TRefCountedStringObject.Instances = 4, 'Expected 4 objects');
    D1 := TSortedObjectDictionary<
      TRefCountedStringObject,TRefCountedStringObject
    >.Create([doOwnsKeys]);
    try
      D1.Add(K1, V1);
      D1.Add(K2, V2);
    finally
      D1.Free;
    end;
    Check(TRefCountedStringObject.Instances = 2, 'Expected 2 objects');
  finally
    V1.Free;
    V2.Free;
  end;

  CreateObjs;
  try
    Check(TRefCountedStringObject.Instances = 4, 'Expected 4 objects');
    D1 := TSortedObjectDictionary<
      TRefCountedStringObject,TRefCountedStringObject
    >.Create([doOwnsValues]);
    try
      D1.Add(K1, V1);
      D1.Add(K2, V2);
    finally
      D1.Free;
    end;
    Check(TRefCountedStringObject.Instances = 2, 'Expected 2 objects');
  finally
    K1.Free;
    K2.Free;
  end;

  CreateObjs;
  try
    Check(TRefCountedStringObject.Instances = 4, 'Expected 4 objects');
    D1 := TSortedObjectDictionary<
      TRefCountedStringObject,TRefCountedStringObject
    >.Create([doOwnsKeys, doOwnsValues]);
    try
      D1.Add(K1, V1);
      D1.Add(K2, V2);
    finally
      D1.Free;
    end;
    Check(TRefCountedStringObject.Instances = 0, 'Expected 0 objects');
  finally
    // do nothing - all of K1, K2, V1 and V2 already freed
  end;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTSortedList.Suite);
  RegisterTest(TestTSortedObjectList.Suite);
  RegisterTest(TestTSortedDictionary.Suite);
  RegisterTest(TestTSortedObjectDictionary.Suite);
end.

