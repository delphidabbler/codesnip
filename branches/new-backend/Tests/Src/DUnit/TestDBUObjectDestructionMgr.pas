{
  Delphi DUnit Test Case for the DB.UObjectDestructionMgr Unit
  ------------------------------------------------------------

  $Rev$
  $Date$
}

unit TestDBUObjectDestructionMgr;


interface


uses
  TestFramework, UBaseObjects, DB.UObjectDestructionMgr, SysUtils,
  DB.UCookies, DB.UDataItem;

type
  // Test methods for class TObjectDestructionMgr

  TestTObjectDestructionMgr = class(TTestCase)
  strict private
    var
      fObjectDestructionMgr: TObjectDestructionMgr;
    type
      TTestDataItem = class(TDBDataItem)
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
      TAlwaysFreeController = class(TInterfacedObject,
        IConditionalFreeController
      )
      public
        function PermitDestruction(const Obj: TObject): Boolean;
      end;
    procedure ForceFree(Obj: TDBDataItem);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreate;
    procedure TestHookController;   // must before TestAllowXXXX tests
    procedure TestAllowDestroyNone;
    procedure TestAllowDestroyAll;
    procedure TestAllowDestroyCookie;
  end;

implementation

procedure TestTObjectDestructionMgr.ForceFree(Obj: TDBDataItem);
begin
  Obj.FreeController := TAlwaysFreeController.Create;
  Obj.Free;
end;

procedure TestTObjectDestructionMgr.SetUp;
begin
  fObjectDestructionMgr := TObjectDestructionMgr.Create;
end;

procedure TestTObjectDestructionMgr.TearDown;
begin
  fObjectDestructionMgr.Free;
  fObjectDestructionMgr := nil;
end;

procedure TestTObjectDestructionMgr.TestAllowDestroyNone;
var
  Item1, Item2: TDBDataItem;
begin
  Item1 := TTestDataItem.Create(100, TDBCookie.Create);
  fObjectDestructionMgr.HookController(Item1);
  Item2 := TTestDataItem.Create(200, TDBCookie.Create);
  fObjectDestructionMgr.HookController(Item2);
  Check(TTestDataItem.InstanceCount = 2,
    Format('<TEST CHECK 1>: Expected TTestDateItem.InstanceCount = 2, got %d',
      [TTestDataItem.InstanceCount]));

  fObjectDestructionMgr.AllowDestroyNone;
  Item1.Free;   // should fail
  Item2.Free;   // should fail
  Check(TTestDataItem.InstanceCount = 2,
    Format('Expected TTestDateItem.InstanceCount = 2, got %d',
      [TTestDataItem.InstanceCount]));

  ForceFree(Item1);
  ForceFree(Item2);
  Check(TTestDataItem.InstanceCount = 0,
    Format('<TEST CHECK 2>: Expected TTestDateItem.InstanceCount = 0, got %d',
      [TTestDataItem.InstanceCount]));
end;

procedure TestTObjectDestructionMgr.TestCreate;
var
  Item: TDBDataItem;
begin
  Item := TTestDataItem.Create(100, TDBCookie.Create);
  Check(TTestDataItem.InstanceCount = 1,
    Format('<TEST CHECK 1>: Expected TTestDateItem.InstanceCount = 1, got %d',
      [TTestDataItem.InstanceCount]));

  Check(Item.FreeController = nil, 'Expected Item.FreeController = nil');

  Item.Free;    // should fail - no controller
  Check(TTestDataItem.InstanceCount = 1,
    Format('Expected TTestDateItem.InstanceCount = 1, got %d',
      [TTestDataItem.InstanceCount]));

  ForceFree(Item);
  Check(TTestDataItem.InstanceCount = 0,
    Format('<TEST CHECK 2>: Expected TTestDateItem.InstanceCount = 0, got %d',
      [TTestDataItem.InstanceCount]));
end;

procedure TestTObjectDestructionMgr.TestHookController;
var
  Item: TDBDataItem;
begin
  Item := TTestDataItem.Create(100, TDBCookie.Create);
  Check(TTestDataItem.InstanceCount = 1,
    Format('<TEST CHECK 1>: Expected TTestDateItem.InstanceCount = 1, got %d',
      [TTestDataItem.InstanceCount]));

  fObjectDestructionMgr.HookController(Item);
  Check(Item.FreeController <> nil, 'Expected Item.FreeController <> nil');

  ForceFree(Item);
  Check(TTestDataItem.InstanceCount = 0,
    Format('<TEST CHECK 2>: Expected TTestDateItem.InstanceCount = 0, got %d',
      [TTestDataItem.InstanceCount]));
end;

procedure TestTObjectDestructionMgr.TestAllowDestroyAll;
var
  Item1, Item2: TDBDataItem;
begin
  Item1 := TTestDataItem.Create(100, TDBCookie.Create);
  fObjectDestructionMgr.HookController(Item1);
  Item2 := TTestDataItem.Create(200, TDBCookie.Create);
  fObjectDestructionMgr.HookController(Item2);
  Check(TTestDataItem.InstanceCount = 2,
    Format('<TEST CHECK>: Expected TTestDateItem.InstanceCount = 2, got %d',
      [TTestDataItem.InstanceCount]));

  fObjectDestructionMgr.AllowDestroyAll;
  Item1.Free;   // should succeed
  Item2.Free;   // should succeed
  Check(TTestDataItem.InstanceCount = 0,
    Format('Expected TTestDateItem.InstanceCount = 0, got %d',
      [TTestDataItem.InstanceCount]));
end;

procedure TestTObjectDestructionMgr.TestAllowDestroyCookie;
var
  Item1, Item2: TDBDataItem;
begin
  Item1 := TTestDataItem.Create(100, TDBCookie.Create);
  fObjectDestructionMgr.HookController(Item1);
  Item2 := TTestDataItem.Create(200, TDBCookie.Create);
  fObjectDestructionMgr.HookController(Item2);
  Check(TTestDataItem.InstanceCount = 2,
    Format('<TEST CHECK 1>: Expected TTestDateItem.InstanceCount = 2, got %d',
      [TTestDataItem.InstanceCount]));

  fObjectDestructionMgr.AllowDestroyCookie(Item1.Cookie);
  Item1.Free;   // should succeed
  Item2.Free;   // should fail
  Check(TTestDataItem.InstanceCount = 1,
    Format('1: Expected TTestDateItem.InstanceCount = 1, got %d',
      [TTestDataItem.InstanceCount]));

  fObjectDestructionMgr.AllowDestroyCookie(Item2.Cookie);
  Item2.Free;   // should now succeed
  Check(TTestDataItem.InstanceCount = 0,
    Format('2: Expected TTestDateItem.InstanceCount = 0, got %d',
      [TTestDataItem.InstanceCount]));
end;

{ TestTObjectDestructionMgr.TTestDataItem }

constructor TestTObjectDestructionMgr.TTestDataItem.Create(ID: Integer;
  Cookie: TDBCookie);
begin
  inherited Create(Cookie);
  fID := ID;
  Inc(InstanceCount);
end;

procedure TestTObjectDestructionMgr.TTestDataItem.Finalize;
begin
  inherited;
  Dec(InstanceCount);
end;

function TestTObjectDestructionMgr.TTestDataItem.ToString: string;
begin
  Result := Format('%s %d', [ClassName, ID]);
end;

{ TestTObjectDestructionMgr.TAlwaysFreeController }

function TestTObjectDestructionMgr.TAlwaysFreeController.PermitDestruction(
  const Obj: TObject): Boolean;
begin
  Result := True;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTObjectDestructionMgr.Suite);
end.

