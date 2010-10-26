{
  Delphi DUnit Test Case for the NsDatabase.UDataPool Unit
  --------------------------------------------------------

  $Rev$
  $Date$
}

unit TestNsDatabaseUDataPool;

interface

uses
  SysUtils, TestFramework, Generics.Collections,
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
    procedure ErrorAddNulCookie;
    procedure ErrorRemove;
    procedure ErrorItemsProp;
    function CheckOffFromList<T>(const List: TList<T>; const Item: T): Boolean;
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

function TestTDBDataPool.CheckOffFromList<T>(const List: TList<T>;
  const Item: T): Boolean;
begin
  Result := List.Contains(Item);
  if Result then
    List.Remove(Item);
end;

procedure TestTDBDataPool.ErrorAdd;
begin
  Pool.Add(O3); // O3 is already in pool
end;

procedure TestTDBDataPool.ErrorAddNulCookie;
var
  ONul: TTestObject;
begin
  ONul := TTestObject.Create(999, TDBCookie.CreateNul);
  try
    Pool.Add(ONul);   // can't add date item with nul cookie
  finally
    ONul.Free;
  end;
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
  CheckException(ErrorAdd, EDBDataPoolError,
    'Added duplicate object');
  CheckException(ErrorAddNulCookie, EDBDataPoolError,
    'Added object with nul cookie');
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
  List: TList<TDBCookie>;
begin
  List := TList<TDBCookie>.Create;
  try
    List.AddRange([O1.Cookie, O2.Cookie, O3.Cookie, O4.Cookie]);
    for Cookie in Pool.Cookies do
    begin
      if not CheckOffFromList<TDBCookie>(List, Cookie) then
        Fail('Cookie unexpected');
    end;
    Check(List.Count = 0, 'Enumeration was incomplete');
  finally
    List.Free;
  end;
end;

procedure TestTDBDataPool.TestEnumerator;
var
  DataItem: TTestObject;
  List: TList<TTestObject>;
begin
  List := TList<TTestObject>.Create;
  try
    List.AddRange([O1, O2, O3, O4]);
    for DataItem in Pool do
    begin
      if not CheckOffFromList<TTestObject>(List, DataItem) then
        Fail(Format('DataItem %s unexpected', [DataItem.ToString]));
    end;
    Check(List.Count = 0, 'Enumeration was incomplete');
  finally
    List.Free;
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

