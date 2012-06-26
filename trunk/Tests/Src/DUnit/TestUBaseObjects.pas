{
  Delphi DUnit Test Case for the UBaseObjects Unit
  ------------------------------------------------

  $Rev$
  $Date$
}

unit TestUBaseObjects;

interface

uses
  TestFramework, UBaseObjects, SysUtils;

type

  // Test methods for class TNoConstructObject
  TestTNoConstructObject = class(TTestCase)
  public
    procedure ConstructError;
  published
    procedure TestConstruct;
  end;

  // Test methods for class TNoPublicConstructObject
  TestTNoPublicConstructObject = class(TTestCase)
  strict private
    type
      TTestObject = class(TNoPublicConstructObject)
      public
        class function Instance: TTestObject;
      end;
  public
    procedure ConstructError;
  published
    procedure TestContruct;
  end;

  // Test methods for class TNoPublicConstructIntfObject
  TestTNoPublicConstructIntfObject = class(TTestCase)
  strict private
    type
      TTestObject = class(TNoPublicConstructIntfObject, IInterface)
      public
        class function Instance: IInterface;
      end;
  public
    procedure ConstructError;
  published
    procedure TestContruct;
  end;

  // Test methods for class TNonRefCountedObject
  TestTNonRefCountedObject = class(TTestCase)
  strict private
    type
      TTestObject = class(TNonRefCountedObject)
      public
        constructor Create;
        destructor Destroy; override;
        class var InstanceCount: Integer;
      end;
  published
    procedure TestAll;
  end;

  // Test methods for class TAggregatedOrLoneObject
  TestTAggregatedOrLoneObject = class(TTestCase)
  strict private
    type
      ITest = interface(IInterface)
        ['{18CBCC27-A5DC-4BA6-BD30-DF16D270801D}']
        function GetInstanceCount: Integer;
      end;
      IMain = interface(IInterface)
        ['{C9D1F469-D073-4BCF-B111-B605A7B1058F}']
        function GetRefCount: Integer;
      end;
      TTestAggregated = class(TAggregatedOrLoneObject, ITest)
      public
        constructor Create(const Controller: IInterface); override;
        destructor Destroy; override;
        class var fInstanceCount: Integer;
        function GetInstanceCount: Integer;
      end;
      TTestController = class(TInterfacedObject, IInterface, ITest, IMain)
      strict private
        fAgg: TTestAggregated;
      public
        constructor Create;
        destructor Destroy; override;
        property Agg: TTestAggregated read FAgg implements ITest;
        class var fInstanceCount: Integer;
        function GetRefCount: Integer;
      end;
  published
    procedure TestStandAlone;
    procedure TestAggregated;
  end;

  // Test methods for class TOwnedConditionalFreeObject
  TestTOwnedConditionalFreeObject = class(TTestCase)
    type
      TTestObject = class(TOwnedConditionalFreeObject)
      strict protected
        procedure Finalize; override;
      public
        constructor Create(const AOwner: TObject);
        class var InstanceCount: Integer;
        class function Details: string;
      end;
  published
    procedure TestFree;
  end;

  // Test methods for class TDelegatedConditionalFreeObject
  TestTDelegatedConditionalFreeObject = class(TTestCase)
  strict private
    type
      TTestObject = class(TDelegatedConditionalFreeObject)
      strict protected
        procedure Finalize; override;
      public
        constructor Create(const CanFreeFn: TFunc<TObject,Boolean>);
        class var InstanceCount: Integer;
        class function Details: string;
      end;
  published
    procedure TestFree;
  end;

  // Test methods for class TControlledConditionalFreeObject
  TestTControlledConditionalFreeObject = class(TTestCase)
  public
    type
      TController = class(TNonRefCountedObject, IConditionalFreeController)
      private
        fObjectToFree: TObject;
      public
        constructor Create(AObjectToFree: TObject = nil);
        function PermitDestruction(const Obj: TObject): Boolean;
        property ObjectToFree: TObject read fObjectToFree write fObjectToFree;
      end;
      TTestObject = class(TControlledConditionalFreeObject)
      strict protected
        procedure Finalize; override;
      public
        constructor Create(AController: IConditionalFreeController = nil);
        class var InstanceCount: Integer;
        class function Details: string;
      end;
  published
    procedure TestFreeObject;
    procedure TestControllerChange;
    procedure TestLock;
  end;

implementation

uses
  Windows;

{ TestTNoConstructObject }

procedure TestTNoConstructObject.ConstructError;
begin
  TNoConstructObject.Create;
end;

procedure TestTNoConstructObject.TestConstruct;
begin
  CheckException(ConstructError, ENoConstructException,
    'Expected assertion failure');
end;

{ TestTNoPublicConstructObject }

procedure TestTNoPublicConstructObject.ConstructError;
begin
  TTestObject.Create; // should cause assertion failure
end;

procedure TestTNoPublicConstructObject.TestContruct;
var
  Obj: TTestObject;
begin
  CheckException(ConstructError, ENoConstructException,
    'Expected assertion failure calling constructor');
  Obj := nil;
  try
    try
      Obj := TTestObject.Instance;
    except
      Fail('Got an exception trying to create via InternalCreate');
    end;
  finally
    Obj.Free;
  end;
end;

{ TestTNoPublicConstructObject.TTestObject }

class function TestTNoPublicConstructObject.TTestObject.Instance: TTestObject;
begin
  Result := inherited InternalCreate;
end;

{ TestTNoPublicConstructIntfObject }

procedure TestTNoPublicConstructIntfObject.ConstructError;
begin
  TTestObject.Create; // should cause assertion failure
end;

procedure TestTNoPublicConstructIntfObject.TestContruct;
var
  Obj: IInterface;
begin
  CheckException(ConstructError, ENoConstructException,
    'Expected assertion failure calling constructor');
  try
    Obj := TTestObject.Instance;
  except
    Fail('Got an exception trying to create via InternalCreate');
  end;
end;

{ TestTNoPublicConstructIntfObject.TTestObject }

class function TestTNoPublicConstructIntfObject.TTestObject.Instance:
  IInterface;
begin
  Result := inherited InternalCreate;
end;

{ TestTNonRefCountedObject }

procedure TestTNonRefCountedObject.TestAll;
var
  Obj: TTestObject;
begin
  Obj := TTestObject.Create;
  try
    Check(Obj.InstanceCount = 1,
      Format('Expected instance after construction of 1, got %d',
        [Obj.InstanceCount]));
    Check((Obj as IInterface)._Release = -1, '_Release didn''t return -1');
    // with ref counted interface, we would now have freed object
    Check(Obj.InstanceCount = 1,
      Format('Expected instance after _Release of 1, got %d',
        [Obj.InstanceCount]));
  finally
    Obj.Free;
  end;
end;

{ TestTNonRefCountedObject.TTestObject }

constructor TestTNonRefCountedObject.TTestObject.Create;
begin
  inherited;
  Inc(InstanceCount);
end;

destructor TestTNonRefCountedObject.TTestObject.Destroy;
begin
  Dec(InstanceCount);
  inherited;
end;

{ TestTAggregatedOrLoneObject }

procedure TestTAggregatedOrLoneObject.TestAggregated;
var
  ObjMain: IMain;
  ObjTest: ITest;
begin
  ObjMain := TTestController.Create;
  Check(TTestController.fInstanceCount = 1,
    'Expecting one instance of controller after construction');
  Check(TTestAggregated.fInstanceCount = 1,
    'Expecting one instance of aggregated object after construction');
  Check(ObjMain.GetRefCount = 1,
    Format('Expecting ref count of 1 after construction, got %d',
      [ObjMain.GetRefCount]));
  ObjTest := ObjMain as ITest;
  Check(ObjMain.GetRefCount = 2,
    Format('Expecting ref count of 2 after assigment of ITest, got %d',
      [ObjMain.GetRefCount]));
  ObjMain._Release;
  Check(ObjMain.GetRefCount = 1,
    Format('Expecting ref count of 1 after releasing IMain, got %d',
      [ObjMain.GetRefCount]));
  ObjTest._Release;
  Check(TTestController.fInstanceCount = 0,
    'Expecting controller to have been freed after releasing main object');
  Check(TTestAggregated.fInstanceCount = 0,
    'Expecting aggregated object to have been freed during destruction');
end;

procedure TestTAggregatedOrLoneObject.TestStandAlone;
var
  Obj: TTestAggregated;
  ObjI: ITest;
begin
  Obj := TTestAggregated.Create;  // no param if stand alone
  ObjI := Obj;
  Check(ObjI.GetInstanceCount = 1,
    Format('Expected instance count of 1, got %d', [Obj.GetInstanceCount]));
  Check(Supports(Obj, IInterface), 'Expected object to support IInterface');
  Check(Supports(Obj, ITest), 'Expected object to support ITest');
  ObjI := nil; // should have decrement instance count and freed this object
  Check(TTestAggregated.fInstanceCount = 0,
    Format('Expected instance count of 0, got %d',
      [TTestAggregated.fInstanceCount]));
end;

{ TestTAggregatedOrLoneObject.TTestAggregated }

constructor TestTAggregatedOrLoneObject.TTestAggregated.Create(
  const Controller: IInterface);
begin
  inherited;
  Inc(fInstanceCount);
end;

destructor TestTAggregatedOrLoneObject.TTestAggregated.Destroy;
begin
  Dec(fInstanceCount);
  inherited;
end;

function TestTAggregatedOrLoneObject.TTestAggregated.GetInstanceCount: Integer;
begin
  Result := fInstanceCount;
end;

{ TestTAggregatedOrLoneObject.TTestController }

constructor TestTAggregatedOrLoneObject.TTestController.Create;
begin
  inherited Create;
  fAgg := TTestAggregated.Create(Self);
  Inc(fInstanceCount);
end;

destructor TestTAggregatedOrLoneObject.TTestController.Destroy;
begin
  fAgg.Free;
  Dec(fInstanceCount);
  inherited;
end;

function TestTAggregatedOrLoneObject.TTestController.GetRefCount: Integer;
begin
  Result := RefCount;
end;

{ TestTOwnedConditionalFreeObject }

procedure TestTOwnedConditionalFreeObject.TestFree;
var
  Obj: TTestObject;
begin
  Obj := TTestObject.Create(Self);
  Check(TTestObject.InstanceCount = 1,
    Format('Expected Instance Count, got %d', [TTestObject.InstanceCount]));
  Obj.Free; // should not have been allowed
  Check(TTestObject.InstanceCount = 1,
    Format('Expected Instance Count of 1, got %d',
      [TTestObject.InstanceCount]));

  Obj.Owner := nil;
  Obj.Free; // should have been allowed
  Check(TTestObject.InstanceCount = 0,
    Format('Expected Instance Count of 0, got %d',
      [TTestObject.InstanceCount]));
end;

{ TestTOwnedConditionalFreeObject.TTestObject }

constructor TestTOwnedConditionalFreeObject.TTestObject.Create(
  const AOwner: TObject);
begin
  inherited;
  Inc(InstanceCount);
end;

class function TestTOwnedConditionalFreeObject.TTestObject.Details: string;
begin
  Result := Format('%s: instances = %d', [ClassName, InstanceCount]);
end;

procedure TestTOwnedConditionalFreeObject.TTestObject.Finalize;
begin
  Dec(InstanceCount);
end;

{ TestTDelegatedConditionalFreeObject }

procedure TestTDelegatedConditionalFreeObject.TestFree;
var
  Obj: TTestObject;
  Allow: Boolean;
begin
  Allow := False;
  Obj := TTestObject.Create(
    function(Sender: TObject): Boolean begin
      Check(Obj = Sender, 'Expected Sender = Obj');
      Result := Allow;
    end
  );
  Check(TTestObject.InstanceCount = 1,
    Format('Expected Instance Count, got %d', [TTestObject.InstanceCount]));
  Obj.Free; // should not have been allowed
  Check(TTestObject.InstanceCount = 1,
    Format('Expected Instance Count of 1, got %d',
      [TTestObject.InstanceCount]));

  Allow := True;
  Obj.Free; // should have been allowed
  Check(TTestObject.InstanceCount = 0,
    Format('Expected Instance Count of 0, got %d',
      [TTestObject.InstanceCount]));
end;

{ TestTDelegatedConditionalFreeObject.TTestObject }

constructor TestTDelegatedConditionalFreeObject.TTestObject.Create(
  const CanFreeFn: TFunc<TObject,Boolean>);
begin
  inherited;
  Inc(InstanceCount);
end;

class function TestTDelegatedConditionalFreeObject.TTestObject.Details: string;
begin
  Result := Format('%s: instances = %d', [ClassName, InstanceCount]);
end;

procedure TestTDelegatedConditionalFreeObject.TTestObject.Finalize;
begin
  Dec(InstanceCount);
end;

{ TControlledConditionalFreeObject }

procedure TestTControlledConditionalFreeObject.TestControllerChange;
var
  Controller1, Controller2: IConditionalFreeController;
  Obj: TTestObject;
begin
  Obj := TTestObject.Create(nil);   // create Obj with no controller
  Check(Obj.FreeController = nil, 'Obj.FreeController <> nil');

  Controller1 := nil;
  Controller2 := nil;
  try
    Controller1 := TController.Create;
    Controller2 := TController.Create(Obj);

    // In this test Obj should not be freed since it has no controller
    Check(TTestObject.InstanceCount = 1,
      Format('1: Expected Instance Count of 1, got %d',
        [TTestObject.InstanceCount]));
    Obj.Free;
    Check(TTestObject.InstanceCount = 1,
      Format('2: Expected Instance Count of 1, got %d',
        [TTestObject.InstanceCount]));

    // In this test Obj should not be freed since Controller1.ObjectToFree = nil
    Obj.FreeController := Controller1;
    Check(Obj.FreeController = Controller1,
      'Obj.FreeController <> Controller1');
    Check(TTestObject.InstanceCount = 1,
      Format('3: Expected Instance Count of 1, got %d',
        [TTestObject.InstanceCount]));
    Obj.Free;
    Check(TTestObject.InstanceCount = 1,
      Format('4: Expected Instance Count of 1, got %d',
        [TTestObject.InstanceCount]));

    // In this test Obj should be freed since Controller2.ObjectToFree = nil
    Obj.FreeController := Controller2;
    Check(Obj.FreeController = Controller2,
      'Obj.FreeController <> Controller2');
    Check(TTestObject.InstanceCount = 1,
      Format('5: Expected Instance Count of 1, got %d',
        [TTestObject.InstanceCount]));
    Obj.Free;
    Check(TTestObject.InstanceCount = 0,
      Format('6: Expected Instance Count of 0, got %d',
        [TTestObject.InstanceCount]));

  finally
    if Assigned(Controller1) then (Controller1 as TController).Free;
    if Assigned(Controller2) then (Controller2 as TController).Free;
  end;

end;

procedure TestTControlledConditionalFreeObject.TestFreeObject;
var
  ControllerObj: TController;
  Controller: IConditionalFreeController;
  Obj: TTestObject;
  AnotherObj: TObject;
begin
  AnotherObj := nil;
  ControllerObj := TController.Create;
  try
    Controller := ControllerObj as IConditionalFreeController;
    Check(ControllerObj.ObjectToFree = nil,
      'ControllerObj.ObjectToFree <> nil');

    Obj := TTestObject.Create(Controller);
    AnotherObj := TObject.Create;

    // In this test ControllerObj.ObjectToFree = nil, so Obj should not be freed
    Check(TTestObject.InstanceCount = 1,
      Format('1: Expected Instance Count of 1, got %d',
        [TTestObject.InstanceCount]));
    Obj.Free;
    Check(TTestObject.InstanceCount = 1,
      Format('2: Expected Instance Count of 1, got %d',
        [TTestObject.InstanceCount]));

    // In this test ControllerObj.ObjectToFree = AnotherObj, which is not the
    // same as Obj, so Obj should not be freed
    ControllerObj.ObjectToFree := AnotherObj;
    Check(TTestObject.InstanceCount = 1,
      Format('3: Expected Instance Count of 1, got %d',
        [TTestObject.InstanceCount]));
    Obj.Free;
    Check(TTestObject.InstanceCount = 1,
      Format('4: Expected Instance Count of 1, got %d',
        [TTestObject.InstanceCount]));

    // In this check Obj should be freed since ControllerObj.ObjectToFree = Obj
    ControllerObj.ObjectToFree := Obj;
    Check(TTestObject.InstanceCount = 1,
      Format('5: Expected Instance Count of 1, got %d',
        [TTestObject.InstanceCount]));
    Obj.Free;
    Check(TTestObject.InstanceCount = 0,
      Format('6: Expected Instance Count of 0, got %d',
        [TTestObject.InstanceCount]));

  finally
    AnotherObj.Free;
    ControllerObj.Free;
  end;
end;

procedure TestTControlledConditionalFreeObject.TestLock;
var
  Controller1a, Controller1b,
  Controller2a, Controller2b: IConditionalFreeController;
  Obj1, Obj2: TTestObject;
begin
  Obj1 := TTestObject.Create(nil);   // create Obj with no controller
  Check(Obj1.FreeController = nil, 'Obj.FreeController <> nil');
  Obj2 := TTestObject.Create(nil);   // create Obj with no controller
  Check(Obj2.FreeController = nil, 'Obj.FreeController <> nil');

  Controller1a := nil;
  Controller1b := nil;
  Controller2a := nil;
  Controller2b := nil;
  try
    Controller1a := TController.Create(Obj1);
    Controller1b := TController.Create(Obj1);
    Controller2a := TController.Create(Obj2);
    Controller2b := TController.Create(Obj2);

    Obj1.FreeController := Controller1a;
    Check(not Obj1.Locked, 'Expected Obj1.Locked = False');
    Obj1.Lock;
    Check(Obj1.Locked, 'Expected Obj1.Locked = True');
    try
      Obj1.FreeController := Controller1b;
      Fail('Exception expected on assignment '
        + 'Obj1.FreeController := Controller1b');
    except
      on E: Exception do
        Check(E is ELocked, 'ELocked exception expected');
    end;
    Check(Obj1.FreeController = Controller1a,
      'Expected Obj1.FreeController = Controller1a');
    Obj1.Free;

    Assert(Obj2.FreeController = nil, 'Expected Obj2.FreeController = nil');
    Obj2.Lock;
    try
      Obj2.FreeController := Controller2a;
    except
      Fail('Unexpected exception on assigment '
         + 'Obj2.FreeController := Controller2a');
    end;
    try
      Obj2.FreeController := Controller2b;
      Fail('Exception expected on assignment '
        + 'Obj2.FreeController := Controller2b');
    except
      on E: Exception do
        Check(E is ELocked, 'ELocked exception expected');
    end;
    Check(Obj2.FreeController = Controller2a,
      'Expected Obj2.FreeController = Controller2a');
    Obj2.Free;

  finally
    if Assigned(Controller1a) then (Controller1a as TController).Free;
    if Assigned(Controller1b) then (Controller1b as TController).Free;
    if Assigned(Controller2a) then (Controller2a as TController).Free;
    if Assigned(Controller2b) then (Controller2b as TController).Free;
  end;

end;

{ TestTControlledConditionalFreeObject.TController }

constructor TestTControlledConditionalFreeObject.TController.Create(
  AObjectToFree: TObject);
begin
  inherited Create;
  fObjectToFree := AObjectToFree;
end;

function TestTControlledConditionalFreeObject.TController.PermitDestruction(
  const Obj: TObject): Boolean;
begin
  Result := Obj = fObjectToFree;
end;

{ TestTControlledConditionalFreeObject.TTestObject }

constructor TestTControlledConditionalFreeObject.TTestObject.Create(
  AController: IConditionalFreeController);
begin
  inherited Create(AController);
  Inc(InstanceCount);
end;

class function TestTControlledConditionalFreeObject.TTestObject.Details: string;
begin
  Result := Format('%s: instances = %d', [ClassName, InstanceCount]);
end;

procedure TestTControlledConditionalFreeObject.TTestObject.Finalize;
begin
  inherited;
  Dec(InstanceCount);
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTNoConstructObject.Suite);
  RegisterTest(TestTNoPublicConstructObject.Suite);
  RegisterTest(TestTNoPublicConstructIntfObject.Suite);
  RegisterTest(TestTNonRefCountedObject.Suite);
  RegisterTest(TestTAggregatedOrLoneObject.Suite);
  RegisterTest(TestTOwnedConditionalFreeObject.Suite);
  RegisterTest(TestTDelegatedConditionalFreeObject.Suite);
  RegisterTest(TestTControlledConditionalFreeObject.Suite);
end.

