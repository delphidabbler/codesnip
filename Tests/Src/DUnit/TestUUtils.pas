{
  Delphi DUnit Test Case for the UUtils Unit
  ------------------------------------------

  $Rev$
  $Date$
}

unit TestUUtils;


interface


uses
  // DUnit
  TestFramework,
  // Project
  UUtils;

type
  // Test methods for some routines in interface of UUtils
  TTestUtilsRoutines = class(TTestCase)
  published
    procedure TestFloatToInt;
    procedure TestIsBaseFileName;
    procedure TestIsHexDigit;
    procedure TestIsValidDriveLetter;
  end;


implementation


uses
  // Delphi
  SysUtils;


{ TTestUtilsRoutines }

procedure TTestUtilsRoutines.TestFloatToInt;
begin
  CheckEquals(42, FloatToInt(42.0), 'Test 1');
  CheckEquals(42, FloatToInt(41.9999999), 'Test 2');
  CheckEquals(42, FloatToInt(42.0000001), 'Test 3');
  CheckEquals(42, FloatToInt(41.5), 'Test 4');
  CheckEquals(41, FloatToInt(41.4999999), 'Test 5');
  CheckEquals(-42, FloatToInt(-42.0), 'Test 6');
  CheckEquals(-42, FloatToInt(-41.9999999), 'Test 7');
  CheckEquals(-42, FloatToInt(-42.0000001), 'Test 8');
  CheckEquals(-42, FloatToInt(-41.5), 'Test 9');
  CheckEquals(-41, FloatToInt(-41.4999999), 'Test 10');
  CheckEquals(0, FloatToInt(0.0), 'Test 11');
  CheckEquals(0, FloatToInt(0.4999999), 'Test 12');
  CheckEquals(1, FloatToInt(0.5), 'Test 13');
  CheckEquals(0, FloatToInt(-0.2), 'Test 14');
  CheckEquals(0, FloatToInt(-0.4999999), 'Test 15');
  CheckEquals(-1, FloatToInt(-0.5), 'Test 16');
  CheckEquals(-1, FloatToInt(-1.499999), 'Test 17');
  CheckEquals(1234567, FloatToInt(1234566.5), 'Test 18');
  CheckEquals(1234566, FloatToInt(1234566.499999), 'Test 19');
  CheckEquals(-1234567, FloatToInt(-1234566.5), 'Test 20');
  CheckEquals(-1234566, FloatToInt(-1234566.499999), 'Test 21');
  CheckEquals(-1234567, FloatToInt(-1234567.0), 'Test 22');
  CheckEquals(1234568, FloatToInt(1234567.5), 'Test 23');
  CheckEquals(1234567, FloatToInt(1234567.499999), 'Test 24');
  CheckEquals(-1234567, FloatToInt(-1234567.499999), 'Test 25');
  CheckEquals(-1234568, FloatToInt(-1234567.5), 'Test 26');
end;

procedure TTestUtilsRoutines.TestIsBaseFileName;
begin
  CheckTrue(IsBaseFileName('Foo'), 'Test 1');
  CheckTrue(IsBaseFileName('Foo.bar'), 'Test 2');
  CheckTrue(IsBaseFileName('Foo.bar.txt'), 'Test 3');
  CheckFalse(IsBaseFileName('C:\Foo.bar'), 'Test4');
  CheckFalse(IsBaseFileName('C:\Foo\Bar.txt'), 'Test5');
  CheckFalse(IsBaseFileName('Foo\Bar'), 'Test 6');
  CheckFalse(IsBaseFileName('Foo\Bar.txt'), 'Test 7');
  CheckFalse(IsBaseFileName('\\Foo\Bar\42.txt'), 'Test 8');
  CheckFalse(IsBaseFileName(''), 'Test 9');
end;

procedure TTestUtilsRoutines.TestIsHexDigit;
const
  GoodChars = '1234567890ABCDEFabcdef';
  BadChars = 'ghijklmnopqrstuvwxyzGHIJKLMNOPQRSTUVWXYZ |-_=+][{}@:><'#13#10#0#9;
var
  Idx: Integer;
begin
  for Idx := 1 to Length(GoodChars) do
    CheckTrue(IsHexDigit(GoodChars[Idx]), 'Good Test ' + IntToStr(Idx));
  for Idx := 1 to Length(BadChars) do
    CheckFalse(IsHexDigit(BadChars[Idx]), 'Bad Test ' + IntToStr(Idx));
end;

procedure TTestUtilsRoutines.TestIsValidDriveLetter;
const
  GoodChars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';
  BadChars = '1234567890 |-_=+][{}@:><'#13#10#0#9;
var
  Idx: Integer;
begin
  for Idx := 1 to Length(GoodChars) do
    CheckTrue(IsValidDriveLetter(GoodChars[Idx]), 'Good Test ' + IntToStr(Idx));
  for Idx := 1 to Length(BadChars) do
    CheckFalse(IsValidDriveLetter(BadChars[Idx]), 'Bad Test ' + IntToStr(Idx));
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TTestUtilsRoutines.Suite);
end.
