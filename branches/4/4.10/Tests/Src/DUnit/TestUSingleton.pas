{
  Delphi DUnit Test Case for the USingleton Unit
  ----------------------------------------------

  To use all the tests in this unit you will need to define the TESTING symbol.

  $Rev$
  $Date$
}

unit TestUSingleton;

interface

uses
  TestFramework, USingleton;

type

  // Test methods for class TSingleton
  TestTSingleton = class(TTestCase)
  published
    procedure TestSingletonBehaviour;
    {$IFDEF TESTING}
    procedure TestFreeSingletons;
    {$ENDIF}
  end;

implementation

uses
  SysUtils;

type

  TTestSingleton1 = class(TSingleton)
  strict protected
    procedure Initialize; override;
    procedure Finalize; override;
  public
    class var InstanceCount: Integer;
  end;

  TTestSingleton2 = class(TSingleton)
  strict protected
    procedure Initialize; override;
    procedure Finalize; override;
  public
    class var InstanceCount: Integer;
  end;

  TTestSingleton2A = class(TTestSingleton2)
  strict protected
    procedure Initialize; override;
    procedure Finalize; override;
  public
    class var InstanceCount: Integer;
  end;


{$IFDEF TESTING}
procedure TestTSingleton.TestFreeSingletons;
begin
  // Make sure we have some singletons
  if TTestSingleton1.InstanceCount = 0 then
    TestSingletonBehaviour;
  Check(TTestSingleton1.InstanceCount = 1,
    Format('Expected instance count of 1 for TTestSingleton1, got %d',
      [TTestSingleton1.InstanceCount]));
  Check(TTestSingleton2.InstanceCount = 1,
    Format('Expected instance count of 1 for TTestSingleton2, got %d',
      [TTestSingleton2.InstanceCount]));
  Check(TTestSingleton2A.InstanceCount = 1,
    Format('Expected instance count of 1 for TTestSingleton2A, got %d',
      [TTestSingleton2A.InstanceCount]));

  // Free all the singletons: all should now have 0 instance count
  USingleton.FreeAllSingletons;
  Check(TTestSingleton1.InstanceCount = 0,
    Format('Expected instance count of 0 for TTestSingleton1, got %d',
      [TTestSingleton1.InstanceCount]));
  Check(TTestSingleton2.InstanceCount = 0,
    Format('Expected instance count of 0 for TTestSingleton2, got %d',
      [TTestSingleton2.InstanceCount]));
  Check(TTestSingleton2A.InstanceCount = 0,
    Format('Expected instance count of 0 for TTestSingleton2A, got %d',
      [TTestSingleton2A.InstanceCount]));
end;
{$ENDIF}

procedure TestTSingleton.TestSingletonBehaviour;
var
  S1a, S1b: TTestSingleton1;
  S2a, S2b, S2c: TTestSingleton2;
  S2Aa, S2Ab: TTestSingleton2A;
begin
  S1a := nil;   S1b := nil;
  S2a := nil;   S2b := nil;   S2c := nil;
  S2Aa := nil;  S2Ab := nil;
  try
    // These two contructors should return same instance
    S1a := TTestSingleton1.Create;    // creates new instance
    S1b := TTestSingleton1.Create;    // returns same instance as above
    CheckSame(S1a, S1b, 'Expected S1a and S1b to be same');
    Check(S1a.InstanceCount = 1,
      Format('Expected instance count of 1 for S1x, got %d',
        [S1a.InstanceCount]));

    // These three contructors should return same instance
    S2a := TTestSingleton2.Create;    // creates new instance
    S2b := TTestSingleton2.Create;    // returns same instance as above
    S2c := TTestSingleton2.Create;    // returns same instance as above
    CheckSame(S2a, S2b, 'Expected S2a and S2b to be same');
    CheckSame(S2b, S2c, 'Expected S2b and S2c to be same');
    Check(S2a.InstanceCount = 1,
      Format('Expected instance count of 1 for S2x, got %d',
        [S2a.InstanceCount]));

    // These two contructors should return same instance
    S2Aa := TTestSingleton2A.Create;  // creates new instance
    S2Ab := TTestSingleton2A.Create;  // returns same instance as above
    CheckSame(S2Aa, S2Ab, 'Expected S2Aa and S2Ab to be same');
    Check(S2Aa.InstanceCount = 1,
      Format('Expected instance count of 1 for S2Ax, got %d',
        [S2Aa.InstanceCount]));

    // None of the instances created from different classes should be same
    Check(Pointer(S1a) <> Pointer(S2a),
       'Expected S1x and S2x to be different');
    Check(Pointer(S2a) <> Pointer(S2Aa),
       'Expected S2x and S2Ax to be different');
    Check(Pointer(S1a) <> Pointer(S2Aa),
       'Expected S1x and S2Ax to be different');

  finally
    // These calls to Free should fail to free the objects: instance counts
    // should remain at 1
    S1a.Free;
    S1b.Free;
    Check(S1a.InstanceCount = 1,
      Format('Expected instance count of 1 for S1x, got %d',
        [S1a.InstanceCount]));
    S2a.Free;
    S2a.Free;
    S2b.Free;
    S2c.Free;
    Check(S2a.InstanceCount = 1,
      Format('Expected instance count of 1 for S2x, got %d',
        [S2a.InstanceCount]));
    S2Aa.Free;
    S2Ab.Free;
    Check(S2Aa.InstanceCount = 1,
      Format('Expected instance count of 1 for S2Ax, got %d',
        [S2Aa.InstanceCount]));
  end;
end;

{ TTestSingleton1 }

procedure TTestSingleton1.Finalize;
begin
  Dec(InstanceCount);
end;

procedure TTestSingleton1.Initialize;
begin
  Inc(InstanceCount);
end;

{ TTestSingleton2 }

procedure TTestSingleton2.Finalize;
begin
  Dec(InstanceCount);
end;

procedure TTestSingleton2.Initialize;
begin
  Inc(InstanceCount);
end;

{ TTestSingleton2A }

procedure TTestSingleton2A.Finalize;
begin
  Dec(InstanceCount);
end;

procedure TTestSingleton2A.Initialize;
begin
  Inc(InstanceCount);
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTSingleton.Suite);
end.
