{
  Delphi DUnit Test Case for the UOrderedDictionary Unit
  ------------------------------------------------------

  $Rev$
  $Date$
}


unit TestUOrderedDictionary;

interface

uses
  TestFramework,
  Classes,  // must come before Generics.Collections
  Generics.Defaults, Generics.Collections, UContainers;

type

  // Test methods for class TOrderedDictionary
  TestTOrderedDictionary = class(TTestCase)
  strict private
    fDict: TOrderedDictionary<string,Integer>;  // constructor with comparer
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

implementation

uses
  SysUtils, Types;

const
  // number of entries in list (count)
  cNumEntries = 6;
  // dictionary keys
  cKeys: array[0..Pred(cNumEntries)] of string = (
    'glo', 'peter', 'donna', 'keith', 'wendy', 'ann'
  );
  // matching values
  cValues: array[0..Pred(cNumEntries)] of Integer = (
    60, 51, 32, 30, 40, 67
  );

  // keys and values in dictionary order
  cSortedKeys: array[0..Pred(cNumEntries)] of string = (
    'ann', 'donna', 'glo', 'keith', 'peter', 'wendy'
  );
  cSortedValues: array[0..Pred(cNumEntries)] of Integer = (
    67, 32, 60, 30, 51, 40
  );

  // maps index into cKeys to index in dictionary
  cIndexInDict: array[0..Pred(cNumEntries)] of Integer = (
    2, 4, 1, 3, 5, 0
  );
  // list of indexes of keys from cKeys as they are entered into dictionay: i.e.
  // index returned by Add method: assumes keys are entered in order from index
  // 0 to cNumEntries - 1
  cSortMapAsEntered: array[0..Pred(cNumEntries)] of Integer = (
    0, 1, 0, 2, 4, 0
  );

  // duplicate key
  cDupKey = 'donna';
  // key not in dictionary
  cMissingKey = 'gwen';

  cDeleteOrder: array[0..Pred(cNumEntries)] of string = (
    'wendy', 'ann', 'glo', 'peter', 'donna', 'keith'
  );

function IndexOf(const S: string; const Items: array of string): Integer;
  overload;
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

function IndexOf(const I: Integer; const Items: array of Integer): Integer;
  overload;
var
  Idx: Integer;
begin
  for Idx := 0 to Pred(Length(Items)) do
  begin
    if I = Items[Idx] then
      Exit(Idx);
  end;
  Result := -1;
end;

function RemoveItem(const S: string;
  const Items: array of string): TStringDynArray; overload;
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

function RemoveItem(const I: Integer;
  const Items: array of Integer): TIntegerDynArray; overload;
var
  Idx1: Integer;
  Idx2: Integer;
begin
  SetLength(Result, Length(Items) - 1);
  Idx2 := 0;
  for Idx1 := 0 to Pred(Length(Items)) do
  begin
    if Items[Idx1] <> I then
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

function SameDictAndArray(const D: TOrderedDictionary<string,Integer>;
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

procedure TestTOrderedDictionary.ClearAll;
begin
  fDict.Clear;
end;

procedure TestTOrderedDictionary.ErrorAdd;
begin
  fDict.Add(cDupKey, 3); // duplicate key, value irrelevant
end;

procedure TestTOrderedDictionary.ErrorAddPair;
begin
  fDict.Add(TPair<string,Integer>.Create(cDupKey, 3)); // duplicate key
end;

procedure TestTOrderedDictionary.ErrorItems;
begin
  fDict['xxx'];
end;

procedure TestTOrderedDictionary.ErrorKeys;
begin
  fDict.Keys[23];
end;

procedure TestTOrderedDictionary.ErrorValues;
begin
  fDict.Values[23];
end;

procedure TestTOrderedDictionary.OnKeyNotifyHandler(Sender: TObject;
  const Key: string; Action: TCollectionNotification);
begin
  fLastKeyNotification.Key := Key;
  fLastKeyNotification.Value := Action;
end;

procedure TestTOrderedDictionary.OnValueNotifyHandler(Sender: TObject;
  const Value: Integer; Action: TCollectionNotification);
begin
  fLastValueNotification.Key := Value;
  fLastValueNotification.Value := Action;
end;

procedure TestTOrderedDictionary.Populate;
var
  Idx: Integer;
begin
  for Idx := 0 to Pred(cNumEntries) do
    fDict.Add(cKeys[Idx], cValues[Idx]);
end;

procedure TestTOrderedDictionary.SetUp;
begin
  fDict := TOrderedDictionary<string,Integer>.Create(
    TComparer<string>.Construct(
      function(const Left, Right: string): Integer
      begin
        Result := AnsiCompareText(Left, Right);
      end
    )
  );
end;

procedure TestTOrderedDictionary.TearDown;
begin
  FreeAndNil(fDict);
end;

procedure TestTOrderedDictionary.TestAdd;
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

procedure TestTOrderedDictionary.TestAddPair;
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

procedure TestTOrderedDictionary.TestClear;
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

procedure TestTOrderedDictionary.TestContains;
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

procedure TestTOrderedDictionary.TestCount;
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

procedure TestTOrderedDictionary.TestCreateNoParams;
var
  D: TOrderedDictionary<Integer,Integer>;
  Idx: Integer;
begin
  // Testing constructor with parameters => use default ordering, so we keep
  // type simple so that ordering is likely to be as expected.
  D := TOrderedDictionary<Integer,Integer>.Create;
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

procedure TestTOrderedDictionary.TestDelete;
var
  Remaining: TStringDynArray;
  RemoveStr: string;
  Idx: Integer;
begin
  ClearAll;
  Populate;
  Remaining := CopyArray(cSortedKeys);
  Assert(SameDictAndArray(fDict, Remaining), 'Initial result in error');
  for RemoveStr in cSortedKeys do
  begin
    Idx := IndexOf(RemoveStr, Remaining);
    fDict.Delete(Idx);
    Remaining := RemoveItem(RemoveStr, Remaining);
    Check(SameDictAndArray(fDict, Remaining),
      Format('fDict.Delete(%1:d): Error removing %0:s from index %1:d',
        [RemoveStr, Idx]));
  end;
end;

procedure TestTOrderedDictionary.TestFind;
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

procedure TestTOrderedDictionary.TestGetEnumerator;
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

procedure TestTOrderedDictionary.TestIndexOf;
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

procedure TestTOrderedDictionary.TestIsEmpty;
begin
  ClearAll;
  Check(fDict.IsEmpty, 'fDict.IsEmpty: Expected True, got False');
  Populate;
  Check(not fDict.IsEmpty, 'fDict.IsEmpty: Expected False, got True');
end;

procedure TestTOrderedDictionary.TestItemsByIndexProp;
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

procedure TestTOrderedDictionary.TestItemsProp;
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

procedure TestTOrderedDictionary.TestKeysProp;
var
  Idx: Integer;
  TestValues: TStringDynArray;
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
    Check(IndexOf(Value, TestValues) >= 0,
      Format('Keys enumeration contains unexpected value %s', [Value]));
    TestValues := RemoveItem(Value, TestValues);
  end;
  Check(Length(TestValues) = 0,
    'Keys enumeration failed: not all values were enumerated');
end;

procedure TestTOrderedDictionary.TestNotifications;

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
    // TOrderedDictionary at present
  finally
    fDict.OnKeyNotify := nil;
    fDict.OnValueNotify := nil;
    ClearAll;
  end;
end;

procedure TestTOrderedDictionary.TestRemove;
var
  Remaining: TStringDynArray;
  RemoveStr: string;
  ExpectedIdx: Integer;
  Idx: Integer;
begin
  ClearAll;
  Populate;
  Remaining := CopyArray(cSortedKeys);
  Assert(SameDictAndArray(fDict, Remaining), 'Initial result in error');
  for RemoveStr in cSortedKeys do
  begin
    ExpectedIdx := Indexof(RemoveStr, Remaining);
    Idx := fDict.Remove(RemoveStr);
    CheckEquals(ExpectedIdx, Idx,
      Format('fDict.Remove(%s): Expected return value %d, got %d',
        [RemoveStr, ExpectedIdx, Idx]));
    Remaining := RemoveItem(RemoveStr, Remaining);
    Check(SameDictAndArray(fDict, Remaining),
      Format('fDict.Remove(%0:s): Error removing %0:s', [RemoveStr]));
  end;
end;

procedure TestTOrderedDictionary.TestValuesProp;
var
  Idx: Integer;
  Value: Integer;
  TestValues: TIntegerDynArray;
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
  SetLength(TestValues, Length(cValues));
  for Idx := Low(cValues) to High(cValues) do
    TestValues[Idx - Low(cValues)] := cValues[Idx];
  for Value in fDict.Values do
  begin
    Check(IndexOf(Value, TestValues) >= 0,
      Format('Values enumeration contains unexpected value %d', [Value]));
    TestValues := RemoveItem(Value, TestValues);
  end;
  Check(Length(TestValues) = 0,
    'Values enumeration failed: not all values were enumerated');
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTOrderedDictionary.Suite);
end.

