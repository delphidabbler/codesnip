{
  Delphi DUnit Test Case for the NsDatabase.UCookies Unit
  -------------------------------------------------------

  $Rev$
  $Date$
}

unit TestNsDatabaseUCookies;

interface

uses
  TestFramework, NsDatabase.UCookies, SysUtils;

type

  TestTDBCookieGenerator = class(TTestCase)
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGetCookie;
  end;

implementation

{ TestTDBCookieGenerator }

procedure TestTDBCookieGenerator.SetUp;
begin
  TDBCookieGenerator.Reset;
end;

procedure TestTDBCookieGenerator.TearDown;
begin
  TDBCookieGenerator.Reset;
end;

procedure TestTDBCookieGenerator.TestGetCookie;
begin
  TDBCookieGenerator.Reset;
  Check(TDBCookieGenerator.GetCookie = 1, 'Cookie 1 expected');
  Check(TDBCookieGenerator.GetCookie = 2, 'Cookie 2 expected');
  Check(TDBCookieGenerator.GetCookie = 3, 'Cookie 3 expected');
  TDBCookieGenerator.Reset;
  Check(TDBCookieGenerator.GetCookie = 1, 'Cookie 1 expected');
  TDBCookieGenerator.Reset;
end;

initialization
  // Register test case with the test runner
  RegisterTest(TestTDBCookieGenerator.Suite);
end.
