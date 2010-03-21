{
  Delphi DUnit Test Case for the NsDatabase.UDataPool Unit
  --------------------------------------------------------

  $Rev$
  $Date$
}

unit TestNsDatabaseUDataPool;

interface

uses
  SysUtils, TestFramework,
  NsDatabase.UCookies, NsDatabase.UDataItem, NsDatabase.UDataPool;

type

  TTestObject = class(TDBDataItem)
  strict private
    fID: Integer;
  strict protected
    procedure Finalize; override;
  public
    constructor Create(ID: Integer; Cookie: TDBCookie);
    class var InstanceCount: Integer;
    property ID: Integer read fID;
    function ToString: string; override;
  end;

  TestTDBDataPool = class(TTestCase)
  strict private
    O1, O2, O3, O4, ONotInPool: TTestObject;
    Pool: TDBDataPool<TTestObject>;
  public
    procedure SetUp; override;
    procedure TearDown; override;
    procedure ErrorAdd;
    procedure ErrorRemove;
    procedure ErrorItemsProp;
  published
    procedure TestAddCountAndFree;
    procedure TestIsInPool;
    procedure TestRemove;
    procedure TestClear;
    procedure TestItemsProp;
    procedure TestEnumerator;
    procedure TestCookiesEnumerator;
  end;

implementation


{ TTestObject }

constructor TTestObject.Create(ID: Integer; Cookie: TDBCookie);
begin
  inherited Create(Cookie);
  fID := ID;
  Inc(InstanceCount);
end;

procedure TTestObject.Finalize;
begin
  inherited;
  Dec(InstanceCount);
end;

function TTestObject.ToString: string;
begin
  Result := Format('%s %d', [ClassName, ID]);
end;

{ TestTDBDataPool }

procedure TestTDBDataPool.ErrorAdd;
begin
  Pool.Add(O3); // O3 is already in pool
end;

procedure TestTDBDataPool.ErrorItemsProp;
begin
  Pool.Items[ONotInPool.Cookie];
end;

procedure TestTDBDataPool.ErrorRemove;
begin
  Pool.Remove(ONotInPool.Cookie);
end;

procedure TestTDBDataPool.SetUp;
begin
  inherited;
  Pool := TDBDataPool<TTestObject>.Create(8);
  O1 := TTestObject.Create(100, TDBCookie.Create);
  Pool.Add(O1);
  O2 := TTestObject.Create(200, TDBCookie.Create);
  Pool.Add(O2);
  O3 := TTestObject.Create(300, TDBCookie.Create);
  Pool.Add(O3);
  O4 := TTestObject.Create(400, TDBCookie.Create);
  Pool.Add(O4);
  ONotInPool := TTestObject.Create(500, TDBCookie.Create);
end;

procedure TestTDBDataPool.TearDown;
begin
  inherited;
  ONotInPool.Free;
  Pool.Free;
end;

procedure TestTDBDataPool.TestAddCountAndFree;
var
  APool: TDBDataPool<TTestObject>;
  O1, O2: TTestObject;
  SavedInstCount: Integer;
begin
  SavedInstCount := TTestObject.InstanceCount;
  APool := TDBDataPool<TTestObject>.Create;
  try
    Check(APool.Count = 0, 'Expected TDBDataPool.Count = 0');
    O1 := TTestObject.Create(10, TDBCookie.Create);
    APool.Add(O1);
    Check(APool.Count = 1, 'Expected TDBDataPool.Count = 1');
    O2 := TTestObject.Create(20, TDBCookie.Create);
    APool.Add(O2);
    Check(APool.Count = 2, 'Expected TDBDataPool.Count = 2');
    Check(TTestObject.InstanceCount = SavedInstCount + 2,
      Format('Expected %d TTestObject instances, got %d',
        [SavedInstCount + 2, TTestObject.InstanceCount]));
  finally
    APool.Free;
  end;
  Check(TTestObject.InstanceCount = SavedInstCount,
    Format('Expected %d TTestObject instances, got %d',
      [SavedInstCount, TTestObject.InstanceCount]));
  CheckException(ErrorAdd, EDBDataPoolError);
end;

procedure TestTDBDataPool.TestClear;
begin
  Check(Pool.Count = 4, 'Expected 4 items in initialised pool');
  Pool.Clear;
  Check(Pool.Count = 0, 'Expected 0 items in pool after .Clear');
  // Re-initialise
  TearDown;
  Setup;
end;

procedure TestTDBDataPool.TestCookiesEnumerator;
var
  Cookie: TDBCookie;
  Idx: Integer;
  Objs: array[1..4] of TTestObject;
begin
  Objs[1] := O1;
  Objs[2] := O2;
  Objs[3] := O3;
  Objs[4] := O4;
  Idx := 0;
  for Cookie in Pool.Cookies do
  begin
    Inc(Idx);
    Check(Cookie = Objs[Idx].Cookie,
      Format('Expected cookie of %s, got $s', [Objs[Idx].ToString]));
  end;
end;

procedure TestTDBDataPool.TestEnumerator;
var
  DataItem: TTestObject;
  Idx: Integer;
  Objs: array[1..4] of TTestObject;
begin
  Objs[1] := O1;
  Objs[2] := O2;
  Objs[3] := O3;
  Objs[4] := O4;
  Idx := 0;
  for DataItem in Pool do
  begin
    Inc(Idx);
    Check(DataItem = Objs[Idx],
      Format('Expected %s, got $s', [Objs[Idx].ToString, DataItem.ToString]));
  end;
end;

procedure TestTDBDataPool.TestIsInPool;
begin
  Check(Pool.IsInPool(O1.Cookie), 'Expected O1 to be in pool');
  Check(Pool.IsInPool(O2.Cookie), 'Expected O1 to be in pool');
  Check(Pool.IsInPool(O3.Cookie), 'Expected O1 to be in pool');
  Check(Pool.IsInPool(O4.Cookie), 'Expected O1 to be in pool');
  Check(not Pool.IsInPool(ONotInPool.Cookie),
    'Expected ONotInPool not to be in pool');
end;

procedure TestTDBDataPool.TestItemsProp;
var
  ObjFound: TTestObject;
begin
  ObjFound := Pool.Items[O1.Cookie];
  Check(ObjFound = O1, 'ObjFound <> O1');
  ObjFound := Pool.Items[O2.Cookie];
  Check(ObjFound = O2, 'ObjFound <> O2');
  ObjFound := Pool.Items[O3.Cookie];
  Check(ObjFound = O3, 'ObjFound <> O3');
  ObjFound := Pool[O4.Cookie];
  Check(ObjFound = O4, 'ObjFound <> O4');
  CheckException(ErrorItemsProp, EDBDataPoolError);
end;

procedure TestTDBDataPool.TestRemove;
var
  APool: TDBDataPool<TTestObject>;
  Cookie1, Cookie2: TDBCookie;
  SavedInstCount: Integer;
begin
  SavedInstCount := TTestObject.InstanceCount;
  APool := TDBDataPool<TTestObject>.Create;
  try
    Cookie1 := TDBCookie.Create;
    Cookie2 := TDBCookie.Create;
    APool.Add(TTestObject.Create(10, Cookie1));
    APool.Add(TTestObject.Create(20, Cookie2));
    Check(APool.Count = 2, 'Expected TDBDataPool.Count = 2');
    Check(TTestObject.InstanceCount = SavedInstCount + 2,
      Format('Expected %d TTestObject instances, got %d',
        [SavedInstCount + 2, TTestObject.InstanceCount]));

    APool.Remove(Cookie2);
    APool.Remove(Cookie1);
    Check(APool.Count = 0, 'Expected TDBDataPool.Count = 2');
    Check(TTestObject.InstanceCount = SavedInstCount,
      Format('Expected %d TTestObject instances, got %d',
        [SavedInstCount, TTestObject.InstanceCount]));
    CheckException(ErrorRemove, EDBDataPoolError);
  finally
    APool.Free;
  end;
end;

initialization
  // Register test case with the test runner
  RegisterTest(TestTDBDataPool.Suite);

end.
