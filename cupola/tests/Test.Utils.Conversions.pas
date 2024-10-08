{
  This unit is dedicated to public domain under the CC0 license.
  See https://creativecommons.org/public-domain/cc0/
}

unit Test.Utils.Conversions;

interface

uses
  DUnitX.TestFramework,

  System.SysUtils,

  CSLE.Utils.Conversions;

type
  [TestFixture]
  TTestConversionRoutines = class
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    [TestCase('0','0,0')]
    [TestCase('42','42,42')]
    [TestCase('Max','65535,65535')]
    [TestCase('Plus 56','+56,56')]
    [TestCase('Hex','$face,64206')]
    procedure TryStrToUInt16_succeeds_on_strings_valid_valid_UInt16_values(const Str: string; Expected: UInt16);
    [Test]
    [TestCase('Empty str','')]
    [TestCase('Punctuation','*&^%$')]
    [TestCase('Letters','FooBar')]
    [TestCase('Digits then letters','999aaa')]
    procedure TryStrToUInt16_fails_on_non_numeric_strings(const Str: string);
    [Test]
    [TestCase('-1','-1')]
    [TestCase('Max+1','65536')]
    [TestCase('Massive +ve','68719476720')]
    [TestCase('Massive -ve','-68719476720')]
    procedure TryStrToUInt16_fails_on_strings_with_out_of_bounds_values(const Str: string);
  end;

implementation

procedure TTestConversionRoutines.Setup;
begin
end;

procedure TTestConversionRoutines.TearDown;
begin
end;

procedure TTestConversionRoutines.TryStrToUInt16_fails_on_non_numeric_strings(const Str: string);
begin
  var Value: UInt16;
  Assert.IsFalse(TryStrToUint16(Str, Value));
end;

procedure TTestConversionRoutines.TryStrToUInt16_fails_on_strings_with_out_of_bounds_values(
  const Str: string);
begin
  var Value: UInt16;
  Assert.IsFalse(TryStrToUint16(Str, Value));
end;

procedure TTestConversionRoutines.TryStrToUInt16_succeeds_on_strings_valid_valid_UInt16_values(
  const Str: string; Expected: UInt16);
begin
  var Actual: UInt16;
  var Res := TryStrToUint16(Str, Actual);
  Assert.IsTrue(Res, 'Returns True');
  Assert.AreEqual(Expected, Actual, 'Value');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestConversionRoutines);

end.
