{
  Delphi DUnit Test Case for the DB.UDatabase Unit
  ------------------------------------------------

  $Rev$
  $Date$
}

unit TestDBUDatabase;

interface

uses
  TestFramework, DB.UDatabase;

type
  // Test methods for class TCSDatabase

  TestTCSDatabase = class(TTestCase)
  strict private
    FCSDatabase: TCSDatabase;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInstance;
  end;

implementation

procedure TestTCSDatabase.SetUp;
begin
  FCSDatabase := TCSDatabase.Instance;
end;

procedure TestTCSDatabase.TearDown;
begin
  FCSDatabase.Free;
  FCSDatabase := nil;
end;

procedure TestTCSDatabase.TestInstance;
begin
  Check(Assigned(FCSDatabase), 'Instance did not create database in SetUp');
  Check(FCSDatabase = TCSDatabase.Instance,
    'Instance did not return same value for database after first call');
  Check(Database = FCSDatabase,
    '"Database" function did not return expected instance');
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTCSDatabase.Suite);
end.
