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

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTNoConstructObject.Suite);
  RegisterTest(TestTNoPublicConstructObject.Suite);
  RegisterTest(TestTNoPublicConstructIntfObject.Suite);
  RegisterTest(TestTNonRefCountedObject.Suite);
  RegisterTest(TestTAggregatedOrLoneObject.Suite);
end.

