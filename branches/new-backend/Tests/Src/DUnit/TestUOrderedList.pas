{
  Delphi DUnit Test Case for the UOrderedList Unit
  ------------------------------------------------

  $Rev$
  $Date$
}


unit TestUOrderedList;

interface

uses
  Classes, // must come before Generics.Collections
  Generics.Collections,
  TestFramework, UContainers;

type

  // Test methods for class TOrderedList
  TestTOrderedList = class(TTestCase)
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

  // items in list order
  cSortedItems: array[0..Pred(cNumEntries)] of string = (
    'ann', 'donna', 'glo', 'keith', 'peter', 'wendy'
  );

  // maps index into cItems to index in list
  cIndexInList: array[0..Pred(cNumEntries)] of Integer = (
    2, 4, 1, 3, 5, 0
  );
  // list of indexes of items from cItems as they are entered into list: i.e.
  // index returned by Add method: assumes items are entered in order from index
  // 0 to cNumEntries - 1
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

  // order of deletion in delete test
  cDeleteOrder: array[0..Pred(cNumEntries)] of string = (
    'wendy', 'ann', 'glo', 'peter', 'donna', 'keith'
  );

function IndexOf(const S: string; const Items: array of string): Integer;
var
  Idx: Integer;
begin
  for Idx := 0 to Pred(Length(Items)) do
  begin
    if S = Items[Idx] then
      Exit(Idx);
  end;
  Result := -1;
end;

function RemoveItem(const S: string;
  const Items: array of string): TStringDynArray;
var
  Idx1: Integer;
  Idx2: Integer;
begin
  SetLength(Result, Length(Items) - 1);
  Idx2 := 0;
  for Idx1 := 0 to Pred(Length(Items)) do
  begin
    if Items[Idx1] <> S then
    begin
      Result[Idx2] := Items[Idx1];
      Inc(Idx2);
    end;
  end;
end;

function CopyArray(const Items: array of string): TStringDynArray;
var
  Idx: Integer;
begin
  SetLength(Result, Length(Items));
  for Idx := 0 to Pred(Length(Items)) do
    Result[Idx] := Items[Idx];
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

procedure TestTOrderedList.ClearAll;
begin
  fList.Clear;
end;

procedure TestTOrderedList.ErrorAddDuplicate;
begin
  fList.Add(cDupItem);
end;

procedure TestTOrderedList.ErrorPermitDuplicates;
begin
  // should be called with fList.PermitDuplicates = True and duplicates in list.
  Assert(fList.PermitDuplicates);
  fList.PermitDuplicates := False;
end;

procedure TestTOrderedList.OnNotifyHandler(Sender: TObject; const Item: string;
  Action: TCollectionNotification);
begin
  fNotifyList.Add(TPair<string,TCollectionNotification>.Create(Item, Action));
end;

procedure TestTOrderedList.Populate;
var
  Idx: Integer;
begin
  for Idx := 0 to Pred(cNumEntries) do
    fList.Add(cItems[Idx]);
end;

procedure TestTOrderedList.SetUp;
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

procedure TestTOrderedList.TearDown;
begin
  FreeAndNil(fList);
end;

procedure TestTOrderedList.TestAdd;
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

procedure TestTOrderedList.TestAddDuplicates;
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

procedure TestTOrderedList.TestClear;
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

procedure TestTOrderedList.TestContains;
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

procedure TestTOrderedList.TestContainsDuplicates;
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

procedure TestTOrderedList.TestCount;
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

procedure TestTOrderedList.TestCreateNoParams;
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

procedure TestTOrderedList.TestDelete;
var
  Remaining: TStringDynArray;
  RemoveStr: string;
  Idx: Integer;
begin
  ClearAll;
  Populate;
  Remaining := CopyArray(cSortedItems);
  Assert(SameListAndArray(fList, Remaining), 'Initial result in error');
  for RemoveStr in cSortedItems do
  begin
    Idx := IndexOf(RemoveStr, Remaining);
    fList.Delete(Idx);
    Remaining := RemoveItem(RemoveStr, Remaining);
    Check(SameListAndArray(fList, Remaining),
      Format('fList.Delete: Error removing %s, index %d', [RemoveStr, Idx]));
  end;
end;

procedure TestTOrderedList.TestFind;
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

procedure TestTOrderedList.TestGetEnumerator;
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

procedure TestTOrderedList.TestIndexOf;
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

procedure TestTOrderedList.TestIsEmpty;
begin
  ClearAll;
  Check(fList.IsEmpty, 'fList.IsEmpty: Expected True, got False');
  Populate;
  Check(not fList.IsEmpty, 'fList.IsEmpty: Expected False, got True');
end;

procedure TestTOrderedList.TestItemsProp;
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

procedure TestTOrderedList.TestOnNotify;

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
    // TOrderedList at present
  finally
    fList.OnNotify := nil;
    FreeAndNil(fNotifyList);
  end;
end;

procedure TestTOrderedList.TestPermitDuplicates;
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

procedure TestTOrderedList.TestRemove;
var
  Remaining: TStringDynArray;
  RemoveStr: string;
  ExpectedIdx: Integer;
  Idx: Integer;
begin
  ClearAll;
  Populate;
  Remaining := CopyArray(cSortedItems);
  Assert(SameListAndArray(fList, Remaining), 'Initial result in error');
  for RemoveStr in cSortedItems do
  begin
    ExpectedIdx := Indexof(RemoveStr, Remaining);
    Idx := fList.Remove(RemoveStr);
    CheckEquals(ExpectedIdx, Idx,
      Format('fList.Remove: Expected return value %d, got %d',
        [ExpectedIdx, Idx]));
    Remaining := RemoveItem(RemoveStr, Remaining);
    Check(SameListAndArray(fList, Remaining),
      Format('fList.Remove: Error removing %s', [RemoveStr]));
  end;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTOrderedList.Suite);
end.

