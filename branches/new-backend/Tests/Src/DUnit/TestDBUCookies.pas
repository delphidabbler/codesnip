{
  Delphi DUnit Test Case for the DB.UCookies Unit
  -----------------------------------------------

  $Rev$
  $Date$
}

unit TestDBUCookies;

interface

uses
  TestFramework, DB.UCookies, SysUtils;

type

  TestTDBCookie = class(TTestCase)
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreate;
    procedure TestNul;
    procedure TestHash;
    procedure TestReset;
  end;

implementation

{ TestTDBCookie }

procedure TestTDBCookie.SetUp;
begin
  TDBCookie.Reset;
end;

procedure TestTDBCookie.TearDown;
begin
  TDBCookie.Reset;
end;

procedure TestTDBCookie.TestCreate;
var
  C1, C2, C3: TDBCookie;
begin
  C1 := TDBCookie.Create;
  C2 := C1;
  C3 := TDBCookie.Create;
  Check(C1 = C2, 'C1 and C2 expected to be same');
  Check(C1 <> C3, 'C1 and C3 expected to be different');
end;

procedure TestTDBCookie.TestHash;
var
  C1, C2: TDBCookie;
begin
  C1 := TDBCookie.Create;
  C2 := C1;
  Check(C1.Hash = C2.Hash, 'Expected C1.Hash to be same as C2.Hash');
end;

procedure TestTDBCookie.TestNul;
var
  C, CN1, CN2: TDBCookie;
begin
  C := TDBCookie.Create;
  CN1 := TDBCookie.CreateNul;
  CN2 := TDBCookie.CreateNul;
  Check(C <> CN1, 'Expected C and CN1 to be different');
  Check(CN1 = CN2, 'Expected CN1 and CN2 to be same');
  Check(CN1.IsNul, 'Expected CN1 to be nul');
  Check(not C.IsNul, 'Expected C not to be nul');
end;

procedure TestTDBCookie.TestReset;
var
  C1a, C1b, C2a, C2b: TDBCookie;
begin
  TDBCookie.Reset;
  C1a := TDBCookie.Create;
  C2a := TDBCookie.Create;
  TDBCookie.Reset;
  C1b := TDBCookie.Create;
  C2b := TDBCookie.Create;
  Check(C1a = C1b, 'Expected C1a = C1b');
  Check(C2a = C2b, 'Expected C2a = C2b');
end;

initialization
  // Register test case with the test runner
  RegisterTest(TestTDBCookie.Suite);
end.

