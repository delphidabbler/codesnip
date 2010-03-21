{
  Delphi DUnit Test Case for the NsDatabase.UDataItem Unit
  --------------------------------------------------------

  $Rev$
  $Date$
}

unit TestNsDatabaseUDataItem;

interface

uses
  TestFramework, NsDatabase.UCookies, NsDatabase.UDataItem, SysUtils;

type

  TTestObject = class(TDBDataItem)
  strict private
    fID: Integer;
  strict protected
    procedure Finalize; override;
  public
    constructor Create(ID: Integer; Cookie: TDBCookie);
    property ID: Integer read fID;
    class var InstanceCount: Integer;
    function ToString: string; override;
  end;

  // Not testing ownership / freeing since that tested for parent object
  TestTDBDataItem = class(TTestCase)
  public
    procedure SetUp; override;
    procedure TearDown; override;
    procedure FreeAndNil(var O: TTestObject);
  published
    procedure TestFree;
    procedure TestCookieProp;
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

{ TestTDBDataItem }

procedure TestTDBDataItem.FreeAndNil(var O: TTestObject);
begin
  if Assigned(O) then
  begin
    O.Owner := nil;
    O.Free;
    O := nil;
  end;
end;

procedure TestTDBDataItem.SetUp;
begin
  inherited;
end;

procedure TestTDBDataItem.TearDown;
begin
  inherited;
end;

procedure TestTDBDataItem.TestCookieProp;
var
  O1, O2, O0: TTestObject;
  Cookie2: TDBCookie;
begin
  Assert(TTestObject.InstanceCount = 0,
    'TestTDBDataItem.TestFree: TTestObject.InstanceCount <> 0');

  O1 := nil; O2 := nil; O0 := nil;
  try
    O1 := TTestObject.Create(100, TDBCookie.Create);
    O2 := TTestObject.Create(200, TDBCookie.Create);

    Check(O1.Cookie <> O2.Cookie,
      Format('Expected %s cookie <> %s cookie', [O1.ToString, O2.ToString]));
    Cookie2 := O2.Cookie;
    O2.Free;  // can do because not owned
    O2 := nil;

    O2 := TTestObject.Create(200, TDBCookie.Create);
    Check(O2.Cookie <> Cookie2,
      Format('Expected %s cookie to have changed', [O2.ToString]));

    O0 := TTestObject.Create(0, TDBCookie.CreateNul);
    Check(O0.Cookie.IsNul,
      Format('Expected %s cookie to be nul', [O0.ToString]));
  finally
    FreeAndNil(O1);
    FreeAndNil(O2);
    FreeAndNil(O0);
  end;

  Assert(TTestObject.InstanceCount = 0,
    'TestTDBDataItem.TestCookieProp: TTestObject.InstanceCount <> 0');
end;

procedure TestTDBDataItem.TestFree;
var
  Obj: TTestObject;
begin
  Assert(TTestObject.InstanceCount = 0,
    'TestTDBDataItem.TestFree: TTestObject.InstanceCount <> 0');

  // Check owned object
  Obj := TTestObject.Create(100, TDBCookie.Create);
  Obj.Owner := Self;
  Check(TTestObject.InstanceCount = 1,
    Format('Expected Instance Count of 1, got %d',
      [TTestObject.InstanceCount]));
  Obj.Free; // should not have been allowed
  Check(TTestObject.InstanceCount = 1,
    Format('Expected Instance Count of 1, got %d',
      [TTestObject.InstanceCount]));

  // Check object after removing owner
  Obj.Owner := nil;
  Obj.Free; // should have been allowed
  Check(TTestObject.InstanceCount = 0,
    Format('Expected Instance Count of 0, got %d',
      [TTestObject.InstanceCount]));

  // Checking object is not owned when created
  Obj := TTestObject.Create(100, TDBCookie.Create);
  Check(TTestObject.InstanceCount = 1,
    Format('Expected Instance Count of 1, got %d',
      [TTestObject.InstanceCount]));
  Obj.Free; // should have been allowed
  Check(TTestObject.InstanceCount = 0,
    Format('Expected Instance Count of 0, got %d',
      [TTestObject.InstanceCount]));

  Assert(TTestObject.InstanceCount = 0,
    'TestTDBDataItem.TestFree: TTestObject.InstanceCount <> 0');
end;

initialization
  // Register test case with the test runner
  RegisterTest(TestTDBDataItem.Suite);

end.

