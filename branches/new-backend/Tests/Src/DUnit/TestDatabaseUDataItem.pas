{
  Delphi DUnit Test Case for the Database.UDataItem Unit
  ------------------------------------------------------

  $Rev$
  $Date$
}

unit TestDatabaseUDataItem;

interface

uses
  TestFramework, Database.UCookies, Database.UDataItem, SysUtils,
  UBaseObjects;

type

  TTestObject = class(TDBDataItem)
  strict private
    fID: Integer;
  strict protected
    procedure Finalize; override;
  public
    constructor Create(ID: Integer; Cookie: TDBCookie;
      Controller: IConditionalFreeController = nil);
    property ID: Integer read fID;
    class var InstanceCount: Integer;
    function ToString: string; override;
  end;

  // Free controller for TDBDataItem objects - always permits freeing
  TTestFreeController = class(TInterfacedObject, IConditionalFreeController)
  public
    function PermitDestruction(const Obj: TObject): Boolean;
  end;

  TestTDBDataItem = class(TTestCase)
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestFree;
    procedure TestCookieProp;
  end;


implementation


{ TTestObject }

constructor TTestObject.Create(ID: Integer; Cookie: TDBCookie;
  Controller: IConditionalFreeController = nil);
begin
  inherited Create(Cookie);
  fID := ID;
  Inc(InstanceCount);
  FreeController := Controller;
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
    'TestTDBDataItem.TestFree: TTestObject.InstanceCount <> 0 '
    + 'at start of TestCookieProp');

  O1 := nil; O2 := nil; O0 := nil;
  try
    O1 := TTestObject.Create(100, TDBCookie.Create, TTestFreeController.Create);
    O2 := TTestObject.Create(200, TDBCookie.Create, TTestFreeController.Create);

    Check(O1.Cookie <> O2.Cookie,
      Format('Expected %s cookie <> %s cookie', [O1.ToString, O2.ToString]));
    Cookie2 := O2.Cookie;
    O2.Free;  // can do because of controller
    O2 := nil;

    O2 := TTestObject.Create(200, TDBCookie.Create, TTestFreeController.Create);
    Check(O2.Cookie <> Cookie2,
      Format('Expected %s cookie to have changed', [O2.ToString]));

    O0 := TTestObject.Create(
      0, TDBCookie.CreateNul, TTestFreeController.Create
    );
    Check(O0.Cookie.IsNul,
      Format('Expected %s cookie to be nul', [O0.ToString]));
  finally
    O1.Free;
    O2.Free;
    O0.Free;
  end;

  Assert(TTestObject.InstanceCount = 0,
    'TestTDBDataItem.TestCookieProp: 2: TTestObject.InstanceCount <> 0 '
    + 'at end of TestCookieProp');
end;

procedure TestTDBDataItem.TestFree;
var
  Obj: TTestObject;
  Controller: IConditionalFreeController;
begin
  Assert(TTestObject.InstanceCount = 0,
    'TestTDBDataItem.TestFree: TTestObject.InstanceCount <> 0');

  // Check no controller: shouldn't free
  Obj := TTestObject.Create(100, TDBCookie.Create);
  Check(Obj.FreeController = nil, 'Expected Obj.FreeController = nil');
  Check(TTestObject.InstanceCount = 1,
    Format('1: Expected Instance Count of 1, got %d',
      [TTestObject.InstanceCount]));
  Obj.Free;
  Check(TTestObject.InstanceCount = 1,
    Format('2: Expected Instance Count of 1, got %d',
      [TTestObject.InstanceCount]));

  // Add controller: should not allow free
  Controller := TTestFreeController.Create;
  Obj.FreeController := Controller;
  Obj.Free;
  Check(TTestObject.InstanceCount = 0,
    Format('3: Expected Instance Count of 0, got %d',
      [TTestObject.InstanceCount]));
end;

{ TTestFreeController }

function TTestFreeController.PermitDestruction(const Obj: TObject): Boolean;
begin
  Result := True;
end;

initialization
  // Register test case with the test runner
  RegisterTest(TestTDBDataItem.Suite);

end.

