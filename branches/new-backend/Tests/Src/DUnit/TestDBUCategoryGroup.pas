{
  Delphi DUnit Test Cases for the DB.UCategoryGroup Unit
  ------------------------------------------------------

  $Rev$
  $Date$
}

unit TestDBUCategoryGroup;

interface

uses
  TestFramework, DB.UBaseGroup, DB.UCategoryGroup;

type

  // Test methods for record TDBCategoryGroupKey
  TestTDBCategoryGroupKey = class(TTestCase)
  published
    procedure TestCreate;
    procedure TestEqualityOperators;
    procedure TestCompareTo;
    procedure TestImplicitStringCast;
  end;

  // Test methods for record TDBCategoryGroupData
  TestTDBCategoryGroupData = class(TTestCase)
    procedure TestClone;
  end;

  // Test methods for class TDBCategoryGroup
  TestTDBCategoryGroup = class(TTestCase)
  strict private
    function CreateObject(const Data: TDBCategoryGroupData): TDBCategoryGroup;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreate;
  end;

implementation

uses
  UTestHelpers;

{ TestTDBCategoryGroupKey }

procedure TestTDBCategoryGroupKey.TestCompareTo;
var
  K1, K2, K3, K4: TDBCategoryGroupKey;
begin
  K1 := TDBCategoryGroupKey.Create('AAA');
  K2 := TDBCategoryGroupKey.Create('BBB');
  K3 := TDBCategoryGroupKey.Create('BBB');
  K4 := TDBCategoryGroupKey.Create('CCC');

  Check(K1.CompareTo(K2) < 0);
  Check(K2.CompareTo(K1) > 0);
  Check(K1.CompareTo(K4) < 0);
  Check(K4.CompareTo(K1) > 0);
  Check(K2.CompareTo(K4) < 0);
  Check(K4.CompareTo(K2) > 0);
  Check(K2.CompareTo(K3) = 0);
  Check(K3.CompareTo(K2) = 0);
end;

procedure TestTDBCategoryGroupKey.TestCreate;
var
  K: TDBCategoryGroupKey;
begin
  K := TDBCategoryGroupKey.Create('AnIDString');
  Check(K.ID = 'AnIDString');
end;

procedure TestTDBCategoryGroupKey.TestEqualityOperators;
var
  K1, K2, K3, K4: TDBCategoryGroupKey;
begin
  K1 := TDBCategoryGroupKey.Create('AAA');
  K2 := TDBCategoryGroupKey.Create('BBB');
  K3 := TDBCategoryGroupKey.Create('BBB');
  K4 := TDBCategoryGroupKey.Create('CCC');

  CheckFalse(K1 = K4);
  CheckFalse(K1 > K4);
  CheckFalse(K1 >= K4);
  CheckTrue(K1 < K4);
  CheckTrue(K1 <= K4);
  CheckTrue(K1 <> K4);

  CheckFalse(K4 = K3);
  CheckTrue(K4 > K3);
  CheckTrue(K4 >= K3);
  CheckFalse(K4 < K3);
  CheckFalse(K4 <= K3);
  CheckTrue(K4 <> K3);

  CheckTrue(K2 = K3);
  CheckFalse(K2 > K3);
  CheckTrue(K2 >= K3);
  CheckFalse(K2 < K3);
  CheckTrue(K2 <= K3);
  CheckFalse(K2 <> K3);
end;

procedure TestTDBCategoryGroupKey.TestImplicitStringCast;
var
  K: TDBCategoryGroupKey;
  S: string;
begin
  K := TDBCategoryGroupKey.Create('AnIDString');
  S := K; // implicit cast
  Check(S = 'AnIDString');
end;

{ TestTDBCategoryGroupData }

procedure TestTDBCategoryGroupData.TestClone;
var
  A, B: TDBCategoryGroupData;
begin
  A.ID := 'AnID';
  A.DisplayName := 'A Display Name';
  // ...

  B.ID := 'AnotherID';
  B.DisplayName := 'Another Display Name';
  // ...

  Check(B.ID = 'AnotherID');
  Check(B.DisplayName = 'Another Display Name');
  // ...

  B := A.Clone;

  Check(B.ID = 'AnID');
  Check(B.DisplayName = 'A Display Name');
  // ...
end;

{ TestTDBCategoryGroup }

function TestTDBCategoryGroup.CreateObject(
  const Data: TDBCategoryGroupData): TDBCategoryGroup;
begin
  Result := TDBCategoryGroup.Create(Data);
  Result.FreeController := TAlwaysFreeController.Create;
end;

procedure TestTDBCategoryGroup.SetUp;
begin
end;

procedure TestTDBCategoryGroup.TearDown;
begin
end;

procedure TestTDBCategoryGroup.TestCreate;
var
  Data: TDBCategoryGroupData;
  Obj: TDBCategoryGroup;
begin
  Data.ID := 'AnID';
  Data.DisplayName := 'A Display Name';
  // ...

  Obj := CreateObject(Data);
  try
    Check(Obj.Key.ID = 'AnID');
    Check(Obj.DisplayName = 'A Display Name');
    // ...
  finally
    Obj.Free;
  end;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTDBCategoryGroupKey.Suite);
  RegisterTest(TestTDBCategoryGroupData.Suite);
  RegisterTest(TestTDBCategoryGroup.Suite);
end.

