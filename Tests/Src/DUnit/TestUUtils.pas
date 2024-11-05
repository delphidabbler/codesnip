{                    \
  Delphi DUnit Test Case for the UUtils Unit
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
    procedure TestURIBaseName;
    procedure TestTryStrToCardinal;
    procedure TestIsEqualBytes_NoCount_overload;
    procedure TestIsEqualBytes_Count_overload;
    procedure TestBytesToHexString;
    procedure TestTryHexStringToBytes;
  end;


implementation


uses
  // Delphi
  SysUtils;


{ TTestUtilsRoutines }

procedure TTestUtilsRoutines.TestBytesToHexString;
const
  B0Str = '';
  B1Str = 'FA';
  B2Str = '010EAA42B656';
var
  BA0, BA1, BA2: TBytes;
begin
  BA0 := TBytes.Create();
  BA1 := TBytes.Create($fa);
  BA2 := TBytes.Create($01, $0E, $AA, $42, $B6, $56);
  CheckEquals(B0Str, BytesToHexString(BA0), 'BA0');
  CheckEquals(B1Str, BytesToHexString(BA1), 'BA1');
  CheckEquals(B2Str, BytesToHexString(BA2), 'BA2');
end;

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

procedure TTestUtilsRoutines.TestIsEqualBytes_Count_overload;
var
  B0, B0a, B1, B1a, B2, B3, B4: TBytes;
begin
  B0 := TBytes.Create();
  B0a := TBytes.Create();
  B1 := TBytes.Create(1,2,3,4,5,6,7,8,9,10);
  B1a := TBytes.Create(1,2,3,4,5,6,7,8,9,10);
  B2 := TBytes.Create(1,2,3,4,5,6,7,8,9,11);
  B3 := TBytes.Create(1,2,3,4,5,6,7,8,9);
  B4 := TBytes.Create(0,1,2,3,4,5,6,7,8,9,10);
  CheckTrue(IsEqualBytes(B1, B1a, 10), 'B1 = B1a (10)');
  CheckTrue(IsEqualBytes(B1, B2, 9), 'B1 = B2 (9)');
  CheckFalse(IsEqualBytes(B1, B2, 10), 'B1 <> B2 (10)');
  CheckFalse(IsEqualBytes(B1, B1a, 15), 'B1 <> B1a (15)');
  CheckFalse(IsEqualBytes(B3, B4, 1), 'B3 <> B4 (1)');
  CheckTrue(IsEqualBytes(B2, B3, 1), 'B2 <> B3 (1)');
  CheckFalse(IsEqualBytes(B0, B0a, 1), 'B0 <> B0a (0)');
end;

procedure TTestUtilsRoutines.TestIsEqualBytes_NoCount_overload;
var
  B0, B0a, B1, B1a, B2, B3, B4, B5, B5a, B6: TBytes;
begin
  B0 := TBytes.Create();
  B0a := TBytes.Create();
  B1 := TBytes.Create(1,2,3,4,5,6,7,8,9,10);
  B1a := TBytes.Create(1,2,3,4,5,6,7,8,9,10);
  B2 := TBytes.Create(1,2,3,4,5,6,7,8,9,11);
  B3 := TBytes.Create(1,2,3,4,5,6,7,8,9);
  B4 := TBytes.Create(0,1,2,3,4,5,6,7,8,9,10);
  B5 := TBytes.Create(7);
  B5a := TBytes.Create(7);
  B6 := TBytes.Create(6);
  CheckTrue(IsEqualBytes(B1, B1a), 'B1 = B1a');
  CheckTrue(IsEqualBytes(B5, B5a), 'B5 = B5a');
  CheckTrue(IsEqualBytes(B0, B0a), 'B0 = B0a');
  CheckFalse(IsEqualBytes(B1, B2), 'B1 <> B2');
  CheckFalse(IsEqualBytes(B5, B6), 'B5 <> B6');
  CheckFalse(IsEqualBytes(B2, B4), 'B2 <> B4');
  CheckFalse(IsEqualBytes(B0, B4), 'B0 <> B4');
  CheckFalse(IsEqualBytes(B0, B6), 'B0 <> B6');
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

procedure TTestUtilsRoutines.TestTryHexStringToBytes;
const
  B0Str = '';
  B1Str = 'FA';
  B2Str = '010EAa42b656';
  Bad1Str = '3abc7';
  Bad2Str = '010EAbx2b656';
var
  BA0, BA1, BA2, Res: TBytes;
begin
  BA0 := TBytes.Create();
  BA1 := TBytes.Create($fa);
  BA2 := TBytes.Create($01, $0E, $AA, $42, $B6, $56);
  CheckTrue(TryHexStringToBytes(B0Str, Res), '#1: Boolean');
  CheckTrue(IsEqualBytes(BA0, Res), '#1: Array');
  Checktrue(TryHexStringToBytes(B1Str, Res), '#2: Boolean');
  CheckTrue(IsEqualBytes(BA1, Res), '#2: Array');
  Checktrue(TryHexStringToBytes(B2Str, Res), '#3: Boolean');
  CheckTrue(IsEqualBytes(BA2, Res), '#3: Array');
  CheckFalse(TryHexStringToBytes(Bad1Str, Res), '#4: Bad length');
  CheckFalse(TryHexStringToBytes(Bad2Str, Res), '#4: Bad digits');
end;

procedure TTestUtilsRoutines.TestTryStrToCardinal;
var
  V: Cardinal;
  I: Int64;
  S: string;
begin
  CheckTrue(TryStrToCardinal('42', V), 'Test 1a');
  CheckEquals(42, V, 'Test 1');
  CheckTrue(TryStrToCardinal('$F', V), 'Test 2a');
  CheckEquals($F, V, 'Test 2b');
  CheckFalse(TryStrToCardinal('', V), 'Test 3');
  CheckFalse(TryStrToCardinal('z42', V), 'Test 4');
  CheckFalse(TryStrToCardinal('42.5', V), 'Test 5');
  CheckFalse(TryStrToCardinal('42z', V), 'Test 6');
  CheckTrue(TryStrToCardinal('0', V), 'Test 7a');
  CheckEquals(0, V, 'Test 7b');
  I := Int64(High(Cardinal));
  S := IntToStr(I);
  CheckTrue(TryStrToCardinal(S, V), 'Test 8a');
  CheckEquals(I, V, 'Test 8b');
  CheckFalse(TryStrToCardinal('-1', V), 'Test 9');
  I := Int64(High(Cardinal)) + 2;
  S := IntToStr(I);
  CheckFalse(TryStrToCardinal(S, V), 'Test 10');
  CheckTrue(TryStrToCardinal('x10', V), 'Test 11a');
  CheckEquals($10, V, 'Test 11b');
  CheckTrue(TryStrToCardinal('0xFFFF', V), 'Test 12a');
  CheckEquals($FFFF, V, 'Test 12b');
end;

procedure TTestUtilsRoutines.TestURIBaseName;
begin
  CheckEquals('', URIBaseName(''), 'Test 1');
  CheckEquals('foo', URIBaseName('foo'), 'Test 2');
  CheckEquals('', URIBaseName('foo/'), 'Test 3');
  CheckEquals('bar', URIBaseName('foo/bar'), 'Test 4');
  CheckEquals(
    'foo.php',
    URIBaseName('https://example.com/foo.php'),
    'Test 5'
  );
  CheckEquals(
    'bar',
    URIBaseName('https://example.com/foo/bar'),
    'Test 6'
  );
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TTestUtilsRoutines.Suite);
end.
