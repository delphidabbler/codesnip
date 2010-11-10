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
      strict protected
        procedure Finalize; override;
      public
        constructor Create(Cookie: TDBCookie);
        class var InstanceCount: Integer;
      end;
    procedure ForceFree(Obj: TDBDataItem);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreate;
    procedure TestHookController;       // must before TestAllowXXXX tests
    procedure TestAllowDestroyNone;
    procedure TestAllowDestroyAll;
    procedure TestAllowDestroyCookie;
    procedure TestHookControllerLock;   // must be after TestAllowDerstroyAll
  end;

implementation

uses
  UTestHelpers;

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
  Item1 := TTestDataItem.Create(TDBCookie.Create);
  fObjectDestructionMgr.HookController(Item1);
  Item2 := TTestDataItem.Create(TDBCookie.Create);
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
  Item := TTestDataItem.Create(TDBCookie.Create);
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
  Item := TTestDataItem.Create(TDBCookie.Create);
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

procedure TestTObjectDestructionMgr.TestHookControllerLock;
var
  Item: TDBDataItem;
begin
  fObjectDestructionMgr.AllowDestroyAll;

  Item := TTestDataItem.Create(TDBCookie.Create);
  Check(TTestDataItem.InstanceCount = 1,
    Format('<TEST CHECK 1>: Expected TTestDateItem.InstanceCount = 1, got %d',
      [TTestDataItem.InstanceCount]));
  try
    fObjectDestructionMgr.HookController(Item, True);
  except
    Fail('Unexpected exception first time calling HookController(Item, True)');
  end;
  try
    fObjectDestructionMgr.HookController(Item);
    Fail('Expected exception second time calling HookController(Item)');
  except
    on E: Exception do
      Check(E is ELocked, 'ELocked exception expected');
  end;
  Item.Free;
  Check(TTestDataItem.InstanceCount = 0,
    Format('<TEST CHECK 2>: Expected TTestDateItem.InstanceCount = 0, got %d',
      [TTestDataItem.InstanceCount]));
end;

procedure TestTObjectDestructionMgr.TestAllowDestroyAll;
var
  Item1, Item2: TDBDataItem;
begin
  Item1 := TTestDataItem.Create(TDBCookie.Create);
  fObjectDestructionMgr.HookController(Item1);
  Item2 := TTestDataItem.Create(TDBCookie.Create);
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
  Item1 := TTestDataItem.Create(TDBCookie.Create);
  fObjectDestructionMgr.HookController(Item1);
  Item2 := TTestDataItem.Create(TDBCookie.Create);
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

constructor TestTObjectDestructionMgr.TTestDataItem.Create(Cookie: TDBCookie);
begin
  inherited Create(Cookie);
  Inc(InstanceCount);
end;

procedure TestTObjectDestructionMgr.TTestDataItem.Finalize;
begin
  inherited;
  Dec(InstanceCount);
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTObjectDestructionMgr.Suite);
end.

