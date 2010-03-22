{
  Delphi DUnit Test Case for the UOrderedList Unit
  ------------------------------------------------

  $Rev$
  $Date$
}


unit TestUOrderedList;

interface

uses
  Generics.Collections,
  TestFramework, UOrderedList;

type

  TTestObject = class(TObject)
  public
    constructor Create(const S: string);
    function Compare(Other: TTestObject): Integer;
    var Str: string;
  end;

  TTestPair = TPair<string,Integer>;

  // Test methods for class TOrderedList
  TestTOrderedList = class(TTestCase)
  strict private
    fList: TOrderedList<string>;
    procedure ClearAll;
    procedure Populate;
    procedure ErrorAddDuplicate;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAdd;
    procedure TestCount;
    procedure TestClear;
    procedure TestFind;
    procedure TestGetEnumerator;
    procedure TestItemsProp;
    procedure TestPermitDuplicates;
    procedure TestCreateNoParams;
  end;

implementation

uses
  WINDOWS,
  SysUtils, Classes, Generics.Defaults;

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


procedure TestTOrderedList.ClearAll;
begin
  fList.Clear;
end;

procedure TestTOrderedList.ErrorAddDuplicate;
begin
  fList.Add(cDupItem);
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
  fList :=  TOrderedList<string>.Create(
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

  Assert(not fList.PermitDuplicates);
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

procedure TestTOrderedList.TestCount;
begin
  // Create fresh, empty dictionary without using ClearAll
  TearDown;
  SetUp;
  CheckEquals(0, fList.Count,
    Format('fDict.Count: Expected 0, got %d', [fList.Count]));
  Populate;
  CheckEquals(cNumEntries, fList.Count,
    Format('fDict.Count: Expected %d, got %d', [cNumEntries, fList.Count]));
end;

procedure TestTOrderedList.TestCreateNoParams;
var
  L: TOrderedList<Integer>;
  Idx: Integer;
begin
  // Testing constructor with parameters => use default ordering, so we keep
  // type simple so that ordering is likely to be as expected.
  L := TOrderedList<Integer>.Create;
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

procedure TestTOrderedList.TestPermitDuplicates;
var
  Idx: string;
begin
  // TODO: Check that setting PermitDuplicates False with duplicates in list raises exception
  ClearAll;
  fList.PermitDuplicates := False;
  Check(not fList.PermitDuplicates, 'Expected fList.PermitDuplicates = False');
  Populate;
  CheckException(ErrorAddDuplicate, EListError);

  ClearAll;
  fList.PermitDuplicates := True;
  Check(fList.PermitDuplicates, 'Expected fList.PermitDuplicates = True');
  Populate;
  try
    fList.Add(cDupItem);
  except
    Fail('No exception expected adding duplicate entry');
  end;
end;

{ TTestObject }

function TTestObject.Compare(Other: TTestObject): Integer;
begin
  Result := AnsiCompareText(Str, Other.Str);
end;

constructor TTestObject.Create(const S: string);
begin
  inherited Create;
  Str := S;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTOrderedList.Suite);
end.

