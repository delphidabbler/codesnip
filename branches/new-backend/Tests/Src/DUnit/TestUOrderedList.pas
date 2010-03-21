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
    fObjects: array[0..4] of TTestObject;
    fSpareObj: TTestObject;
    fIntList: TOrderedList<Integer>;
    fStrList1: TOrderedList<string>;
    fStrList2: TOrderedList<string>;
    fObjList: TOrderedList<TTestObject>;
    fPairList: TOrderedList<TTestPair>;
    procedure ClearAll;
    procedure AddSomeValidItems;
    procedure AddDuplicate;
    procedure ItemsBoundsError;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAdd;
    procedure TestPermitDuplicates;
    procedure TestCount;
    procedure TestClear;
    procedure TestFind;
    procedure TestItems;
    procedure TestEnumerator;
  end;

implementation

uses
  WINDOWS,
  SysUtils, Classes, Generics.Defaults;

procedure TestTOrderedList.AddDuplicate;
begin
  // Assumes values have been added to list
  fIntList.Add(20);
end;

procedure TestTOrderedList.AddSomeValidItems;
begin
                                                  // Count   Index
  fIntList.Add(10);                               // 1       1
  fIntList.Add(20);                               // 2       3
  fIntList.Add(5);                                // 3       0
  fIntList.Add(15);                               // 4       2
  fIntList.Add(50);                               // 5       5
  fIntList.Add(35);                               // 6       4

  fStrList1.Add('bonnie');                        // 1       1
  fStrList1.Add('clyde');                         // 2       2
  fStrList1.Add('glo');                           // 3       4
  fStrList1.Add('peter');                         // 4       7
  fStrList1.Add('donna');                         // 5       3
  fStrList1.Add('glo');                           // 6       5    ** duplicate
  fStrList1.Add('keith');                         // 7       6
  fStrList1.Add('wendy');                         // 8       8
  fStrList1.Add('ann');                           // 9       0

  fStrList2.Add('glo');                           // 1       2
  fStrList2.Add('peter');                         // 2       4
  fStrList2.Add('donna');                         // 3       1
  fStrList2.Add('keith');                         // 4       3
  fStrList2.Add('wendy');                         // 5       5
  fStrList2.Add('ann');                           // 6       0

  fObjList.Add(fObjects[0]);                      // 1       2
  fObjList.Add(fObjects[1]);                      // 2       0
  fObjList.Add(fObjects[2]);                      // 3       3
  fObjList.Add(fObjects[3]);                      // 4       1
  fObjList.Add(fObjects[4]);                      // 5       4

  fPairList.Add(TTestPair.Create('fred', 42));    // 1       1
  fPairList.Add(TTestPair.Create('joe', 56));     // 2       2
  fPairList.Add(TTestPair.Create('bert', 666));   // 3       0
end;

procedure TestTOrderedList.ClearAll;
begin
  fIntList.Clear;
  fStrList1.Clear;
  fStrList2.Clear;
  fObjList.Clear;
  fPairList.Clear;
end;

procedure TestTOrderedList.ItemsBoundsError;
begin
  // Assumes standard items added to lists
  fIntList[6];
end;

procedure TestTOrderedList.SetUp;
begin
  // specify comparers
  fIntList := TOrderedList<Integer>.Create(
    TComparer<Integer>.Construct(
      function(const Left, Right: Integer): Integer
      begin
        Result := Left - Right;
      end
    )
  );
  fIntList.PermitDuplicates := False;

  fStrList1 := TOrderedList<string>.Create(
    TComparer<string>.Construct(
      function(const Left, Right: string): Integer
      begin
        Result := AnsiCompareText(Left, Right);
      end
    )
  );
  fStrList1.PermitDuplicates := True;

  // default comparer
  fStrList2 := TOrderedList<string>.Create;
  fStrList2.PermitDuplicates := False;

  // objects
  fObjList := TOrderedList<TTestObject>.Create(
    TComparer<TTestObject>.Construct(
      function(const Left, Right: TTestObject): Integer
      begin
        Result := Left.Compare(Right);
      end
    )
  );
  fObjList.PermitDuplicates := True;
  fObjects[0] := TTestObject.Create('JJJJJ');
  fObjects[1] := TTestObject.Create('AA');
  fObjects[2] := TTestObject.Create('KKK');
  fObjects[3] := TTestObject.Create('CCC');
  fObjects[4] := TTestObject.Create('ZZZZZ');
  fSpareObj := TTestObject.Create('Spare');

  // pairs
  fPairList := TOrderedList<TTestPair>.Create(
    TComparer<TTestPair>.Construct(
      function(const Left, Right: TTestPair): Integer
      begin
        Result := AnsiCompareText(Left.Key, Right.Key)
      end
    )
  );
end;

procedure TestTOrderedList.TearDown;
begin
  FreeAndNil(fPairList);
  FreeAndNil(fSpareObj);
  FreeAndNil(fObjects[4]);
  FreeAndNil(fObjects[3]);
  FreeAndNil(fObjects[2]);
  FreeAndNil(fObjects[1]);
  FreeAndNil(fObjects[0]);
  FreeAndNil(fObjList);
  FreeAndNil(fIntList);
  FreeAndNil(fStrList1);
  FreeAndNil(fStrList2);
end;

procedure TestTOrderedList.TestAdd;
begin
  ClearAll;
  AddSomeValidItems;

  Check(fIntList.Count = 6, 'Expected <Integer>.Count = 6');
  Check(fStrList1.Count = 9, 'Expected 1st <string>.Count = 9');
  Check(fStrList2.Count = 6, 'Expected 2nd <string>.Count = 6');
  Check(fObjList.Count = 5, 'Expected <TTestObject>.Count = 5');
  Check(fPairList.Count = 3, 'Expected <TTestPair>.Count = 3');

  Check(fIntList.Add(21) = 4, 'Expected <Integer>.Add(21) to return 4');
  Check(fIntList.Add(1) = 0, 'Expected <Integer>.Add(1) to return 0');
  Check(fIntList.Add(99) = 8, 'Expected <Integer>.Add(99) to return 8');

  Check(fObjList.Add(fSpareObj) = 4, 'Expected <TTestObject>.Add to return 4');

  CheckException(AddDuplicate, EListError, 'Duplicate item error expected');
end;

procedure TestTOrderedList.TestClear;
begin
  ClearAll;
  AddSomeValidItems;
  ClearAll;
  Check(fIntList.Count = 0, 'Expected zero count in <Integer>');
  Check(fStrList2.Count = 0, 'Expected zero count in <string>');
  Check(fObjList.Count = 0, 'Expected zero count in <TTestObject>');
  Check(fPairList.Count = 0, 'Expected zero count in <TTestPair>');
end;

procedure TestTOrderedList.TestCount;
begin
  ClearAll;
  Check(fIntList.Count = 0, 'Expected <Integer>.Count = 0');
  Check(fStrList1.Count = 0, 'Expected 1st <string>.Count = 0');
  Check(fStrList2.Count = 0, 'Expected 2nd <string>.Count = 0');
  Check(fObjList.Count = 0, 'Expected <TTestObject>.Count = 0');
  Check(fPairList.Count = 0, 'Expected <TTestPair>.Count = 0');

  AddSomeValidItems;
  Check(fIntList.Count = 6, 'Expected <Integer>.Count = = 6');
  Check(fStrList1.Count = 9, 'Expected 1st <string>.Count = 9');
  Check(fStrList2.Count = 6, 'Expected 2nd <string>.Count = 6');
  Check(fObjList.Count = 5, 'Expected <TTestObject>.Count = 5');
  Check(fPairList.Count = 3, 'Expected <TTestObject>.Count = 3');
end;

procedure TestTOrderedList.TestEnumerator;
var
  O: TTestObject;
  Idx: Integer;
  S: string;
  P: TTestPair;
const
  // expected order of objects
  OResMap: array[0..4] of Integer = (1, 3, 0, 2, 4);
  // expected order of string 2
  SResMap: array[0..5] of string = (
    'ann', 'donna', 'glo', 'keith', 'peter', 'wendy'
  );
  // expected order of pairs
  PKeyResMap: array[0..2] of string = ('bert', 'fred', 'joe');
  PValueResMap: array[0..2] of Integer = (666, 42, 56);
begin
  ClearAll;
  AddSomeValidItems;

  Idx := 0;
  for O in fObjList do
  begin
    Check(O = fObjects[OResMap[Idx]], 'Enumeration order unexpected');
    Inc(Idx);
  end;

  Idx := 0;
  for S in fStrList2 do
  begin
    CheckEqualsString(SResMap[Idx], S,
      Format('Expected "%s" got "%s"', [SResMap[Idx], S]));
    Inc(Idx);
  end;

  Idx := 0;
  for P in fPairList do
  begin
    CheckEqualsString(PKeyResMap[Idx], P.Key,
      Format('Expected "%s" got "%s"', [PKeyResMap[Idx], P.Key]));
    CheckEquals(PValueResMap[Idx], P.Value,
      Format('Expected %d got %d', [PValueResMap[Idx], P.Value]));
    Inc(Idx);
  end;
end;

procedure TestTOrderedList.TestFind;
var
  Index: Integer;
begin
  ClearAll;
  AddSomeValidItems;

  Check(not fIntList.Find(42, Index), 'Expected <Integer>.Find to fail');
  Check(Index = 5, 'Expected <Integer>.Find(42) insert index = 5');
  Check(fIntList.Find(5, Index), 'Expected <Integer>.Find(5) to succeed');
  Check(Index = 0, 'Expected <Integer>.Find(5) index = 0');
  Check(fIntList.Find(50, Index), 'Expected <Integer>.Find(50) to succeed');
  Check(Index = 5, 'Expected <Integer>.Find(50) index = 5');
  Check(fIntList.Find(20, Index), 'Expected <Integer>.Find(20) to succeed');
  Check(Index = 3, 'Expected <Integer>.Find(20) index = 3');

  Check(fStrList1.Find('donna', Index), '1st <string>.Find(donna) failed');
  Check(Index = 3, 'Expected 1st <string>.Find(donna) index = 3');
  Check(fStrList1.Find('glo', Index), '1st <string>.Find(glo) failed');
  Check(Index in [4,5], 'Expected 1st <string>.Find(glo) index = 3 or 4');
  Check(not fStrList1.Find('ken', Index), '1st <string>.Find(ken) succeeded');
  Check(Index = 7, 'Expected 1st <string>.Find(ken) insert index = 7');

  Check(fStrList2.Find('donna', Index), '2nd <string>.Find(donna) failed');
  Check(fStrList2.Find('ann', Index), '2nd <string>.Find(donna) failed');
  Check(fStrList2.Find('wendy', Index), '2nd <string>.Find(donna) failed');
  Check(not fStrList2.Find('ken', Index), '2nd <string>.Find(ken) succeeded');

  Check(fObjList.Find(fObjects[2], Index), '<TTestObject>.Find([2]) failed');
  Check(not fObjList.Find(fSpareObj, Index),
    '<TTestObject>.Find([spare]) failed');

  Check(fPairList.Find(TTestPair.Create('joe', 0), Index),
    '<TTestPair.Find(joe) failed');
  Check(not fPairList.Find(TTestPair.Create('oops', 0), Index),
    '<TTestPair.Find(oops) succeeded');
end;

procedure TestTOrderedList.TestItems;
begin
  ClearAll;
  AddSomeValidItems;

  Check(fIntList[0] = 5, 'Expected <integer>.Items[0] = 5');
  Check(fIntList[5] = 50, 'Expected <integer>.Items[5] = 50');
  Check(fIntList[3] = 20, 'Expected <integer>.Items[3] = 20');
  CheckException(ItemsBoundsError, EArgumentOutOfRangeException,
    'Index out of bounds');

  Check(fStrList1[3] = 'donna', 'Expected 1st <string>.Items[3] = donna');
  Check(fStrList1[4] = 'glo', 'Expected 1st <string>.Items[4] = glo');
  Check(fStrList1[5] = 'glo', 'Expected 1st <string>.Items[5] = glo');

  Check(fObjList[3] = fObjects[2], 'Expected [2] in <TTestObject>.Items[3]');

  Check((fPairList[1].Key = 'fred') and (fPairList[1].Value = 42),
    'Expected <TTestPair>.Items[1] = <fred,42>');
end;

procedure TestTOrderedList.TestPermitDuplicates;
begin
  Check(not fIntList.PermitDuplicates,
    'Expected <integer>.PermitDuplicates = False');
  Check(fStrList1.PermitDuplicates,
    'Expected 1st <string>.PermitDuplicates = True');
  Check(not fStrList2.PermitDuplicates,
    'Expected 2nd <string>.PermitDuplicates = False');
  Check(fObjList.PermitDuplicates,
    'Expected <TTestObject>.PermitDuplicates = True');
  Check(not fPairList.PermitDuplicates,
    'Expected <TTestPair>.PermitDuplicates = False');
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

