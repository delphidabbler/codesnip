{
  This unit is dedicated to public domain under the CC0 license.
  See https://creativecommons.org/public-domain/cc0/
}

unit Test.TextData;

interface

uses
  DUnitX.TestFramework,

  System.SysUtils,

  CSLE.TextData;

type
  [TestFixture]
  TTestTextData = class
  strict private
    const

      EmptyArray: TBytes = [];

      ASCIIData: TBytes = [
        Ord('F'), Ord('o'), Ord('o'), Ord(' '),
        Ord('B'), Ord('a'), Ord('r'), Ord(' '),
        Ord('4'), Ord('2'), Ord(' '),
        Ord('A'), Ord('l'), Ord('i'), Ord('c'), Ord('e'), Ord(' '),
        Ord('&'), Ord(' '),
        Ord('B'), Ord('o'), Ord('b'), Ord('.')
      ];
      ASCIIStr     = 'Foo Bar 42 Alice & Bob.';
      ASCIIEQStr   = 'Foo Bar 42 Alice & Bob.';
      ASCIINEQStr1 = 'Foo Bar 56 Alice & Bob.';
      ASCIINEQStr2 = 'Foo Bar Alice & Bob';

      ASCIIToASCIIStr: ASCIIString = 'Foo Bar 42 Alice & Bob.';
      ASCIIToANSIStr: AnsiString = 'Foo Bar 42 Alice & Bob.';
      ASCIIToUTF8Str: UTF8String = 'Foo Bar 42 Alice & Bob.';

      ANSIData: TBytes = [
        // Alice ¼, Bob ½. ©2023
        Ord('A'), Ord('l'), Ord('i'), Ord('c'), Ord('e'), Ord(' '),
        $BC {quarter in latin-1}, Ord(','), Ord(' '),
        Ord('B'), Ord('o'), Ord('b'), Ord(' '),
        $BD {half in latin-1}, Ord('.'), Ord(' '),
        $A9 {(c) mark in latin-1}, Ord('2'), Ord('0'), Ord('2'), Ord('3')
      ];
      ANSIStr     = 'Alice ¼, Bob ½. ©2023';
      ANSIEQStr   = 'Alice ¼, Bob ½. ©2023';
      ANSINEQStr1 = 'Alice 4, Bob 2. ©2023';
      ANSINEQStr2 = 'Alice ¼, Bob ½.';

      ANSIToASCIIStr: ASCIIString = 'Alice ~, Bob ~. ~2023';
      ANSIToANSIStr: AnsiString = 'Alice ¼, Bob ½. ©2023';
      ANSIToUTF8Str: UTF8String = 'Alice ¼, Bob ½. ©2023';

      UTF8Data: TBytes = [
        // Alice ℅ Bob ¶ ©2023.
        $41, $6c, $69, $63, $65, $20, $e2, $84, $85, $20, $42, $6f, $62, $20,
        $c2, $b6, $20, $c2, $a9, $32, $30, $32, $33, $2e
      ];
      UTF8Str     = 'Alice ℅ Bob ¶ ©2023.';
      UTF8EQStr   = 'Alice ℅ Bob ¶ ©2023.';
      UTF8NEQStr1 = 'Alice & Bob ¶ ©2023.';
      UTF8NEQStr2 = 'Alice, Bob copyright 2023.';

      UTF8ToASCIIStr: ASCIIString = 'Alice ~ Bob ~ ~2023.';
      UTF8ToANSIStr: AnsiString = 'Alice ~ Bob ¶ ©2023.';
      UTF8ToUTF8Str: UTF8String = 'Alice ℅ Bob ¶ ©2023.';

      UTF8DoublePaddedStream: TBytes = [
        // 8 bytes of garbage before text
        $f1, $f2, $f3, $f4, $f5, $f6, $f7, Ord('8'),
        // Alice ℅ Bob ¶ ©2023. (24 bytes)
        $41, $6c, $69, $63, $65, $20, $e2, $84,
        $85, $20, $42, $6f, $62, $20, $c2, $b6,
        $20, $c2, $a9, $32, $30, $32, $33, $2e,
        // 5 bytes of garbage after text
        Ord('1'), Ord('2'), Ord('3'), Ord('4'), Ord('5')
      ];

      UTF8StartPaddedStream: TBytes = [
        // 8 bytes of garbage before text
        $f1, $f2, $f3, $f4, $f5, $f6, $f7, Ord('8'),
        // Alice ℅ Bob ¶ ©2023. (24 bytes)
        $41, $6c, $69, $63, $65, $20, $e2, $84,
        $85, $20, $42, $6f, $62, $20, $c2, $b6,
        $20, $c2, $a9, $32, $30, $32, $33, $2e
      ];

    function CompareWithStringLoss(Expected, Actual: RawByteString): Boolean;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    // NOTE: Read accessors of Data and DataType properties are tested as side
    //       effects of other tests, starting with the ctor tests

    [Test]
    procedure Default_ctor_creates_empty_UTF8_data;

    [Test]
    procedure ctor_for_data_array_works_for_ANSI;
    [Test]
    procedure ctor_for_data_array_works_for_ASCII;
    [Test]
    procedure ctor_for_data_array_works_for_UTF8;

    [Test]
    procedure ctor_for_unicode_strings_works_for_ANSI;
    [Test]
    procedure ctor_for_unicode_strings_works_for_ASCII;
    [Test]
    procedure ctor_for_unicode_strings_works_for_UTF8;
    [Test]
    procedure ctor_for_unicode_strings_passed_empty_string_works_for_ANSI;
    [Test]
    procedure ctor_for_unicode_strings_passed_empty_string_works_for_ASCII;
    [Test]
    procedure ctor_for_unicode_strings_passed_empty_string_works_for_UTF8;

    [Test]
    procedure ctor_for_RawByteString_string_works_for_ANSI;
    [Test]
    procedure ctor_for_RawByteString_string_works_for_ASCII;
    [Test]
    procedure ctor_for_RawByteString_string_works_for_UTF8;
    [Test]
    procedure ctor_for_RawByteString_string_treats_empty_string_as_UTF8;
    [Test]
    procedure ctor_for_RawBytString_raises_exception_for_unsupported_code_page;

    [Test]
    procedure ctor_for_stream_works_for_ANSI_stream;
    [Test]
    procedure ctor_for_stream_works_for_ASCII_stream;
    [Test]
    procedure ctor_for_stream_works_for_UTF8_stream;
    [Test]
    procedure ctor_for_stream_works_for_UTF8_stream_with_leading_bytes;
    [Test]
    procedure ctor_for_stream_works_for_UTF8_stream_with_padding_bytes;

    [Test]
    procedure Assign_op_works_for_ANSI;
    [Test]
    procedure Assign_op_works_for_ASCII;
    [Test]
    procedure Assign_op_works_for_UTF8;

    [Test]
    procedure DataLength_returns_correct_size_of_data_after_array_ctor;
    [Test]
    procedure DataLength_returns_correct_size_of_data_after_string_ctor;

    [Test]
    procedure ToString_works_for_ANSI_data_array;
    [Test]
    procedure ToString_works_for_ASCII_string;
    [Test]
    procedure ToString_works_for_UTF8_data_array;

    [Test]
    procedure ToASCIIString_works_with_ASCII_source;
    [Test]
    procedure ToASCIIString_works_with_ANSI_source_with_character_loss;
    [Test]
    procedure ToASCIIString_works_with_UTF8_source_with_character_loss;

    [Test]
    procedure ToANSIString_works_with_ASCII_source;
    [Test]
    procedure ToANSIString_works_with_ANSI_source;
    [Test]
    procedure ToANSIString_works_with_UTF8_source_with_character_loss;

    [Test]
    procedure ToUTF8String_works_with_ASCII_source;
    [Test]
    procedure ToUTF8String_works_with_ANSI_source;
    [Test]
    procedure ToUTF8String_works_with_UTF8_source;

    [Test]
    procedure Encoding_works_with_ASCII_source;
    [Test]
    procedure Encoding_works_with_ANSI_source;

    [Test]
    procedure SupportsString_returns_true_with_valid_ASCII_string;
    [Test]
    procedure SupportsString_returns_false_for_bad_ASCII_string;
    [Test]
    procedure SupportsString_returns_false_for_bad_ANSI_string;
    [Test]
    procedure SupportsString_returns_true_for_valid_UTF8_string;

    [Test]
    procedure Equal_op_returns_true_with_equal_ASCII_strings;
    [Test]
    procedure Equal_op_returns_true_with_equal_ANSI_strings;
    [Test]
    procedure Equal_op_returns_true_with_equal_UTF8_strings;
    [Test]
    procedure Equal_op_returns_false_with_unequal_ASCII_strings;
    [Test]
    procedure Equal_op_returns_false_with_unequal_ANSI_strings;
    [Test]
    procedure Equal_op_returns_false_with_unequal_UTF8_strings;

    [Test]
    procedure NotEqual_op_returns_true_with_unequal_ASCII_strings;
    [Test]
    procedure NotEqual_op_returns_true_with_unequal_ANSI_strings;
    [Test]
    procedure NotEqual_op_returns_true_with_unequal_UTF8_strings;
    [Test]
    procedure NotEqual_op_returns_false_with_equal_ASCII_strings;
    [Test]
    procedure NotEqual_op_returns_false_with_equal_ANSI_strings;
    [Test]
    procedure NotEqual_op_returns_false_with_equal_UTF8_strings;

    [Test]
    procedure IsEmpty_returns_false_with_non_empty_ASCII_string;
    [Test]
    procedure IsEmpty_returns_false_with_non_empty_ANSI_string;
    [Test]
    procedure IsEmpty_returns_false_with_non_empty_UTF8_string;
    [Test]
    procedure IsEmpty_returns_true_with_empty_ASCII_string;
    [Test]
    procedure IsEmpty_returns_true_with_empty_ANSI_string;
    [Test]
    procedure IsEmpty_returns_true_with_empty_UTF8_string;
  end;

implementation

uses
  System.AnsiStrings,
  System.Classes,
  Winapi.Windows {for inline expansion};

procedure TTestTextData.Assign_op_works_for_ANSI;
begin
  var T0 := TTextData.Create(ANSIData, TTextDataType.ANSI);
  var T1 := T0; // assignment
  Assert.AreEqual(ANSIData, T1.Data, 'Check Data');
  Assert.AreEqual(TTextDataType.ANSI, T1.DataType, 'Check Type');
end;

procedure TTestTextData.Assign_op_works_for_ASCII;
begin
  var T0 := TTextData.Create(ASCIIData, TTextDataType.ASCII);
  var T1 := T0; // asignment
  Assert.AreEqual(ASCIIData, T1.Data, 'Check Data');
  Assert.AreEqual(TTextDataType.ASCII, T1.DataType, 'Check Type');
end;

procedure TTestTextData.Assign_op_works_for_UTF8;
begin
  var T0 := TTextData.Create(UTF8Data, TTextDataType.UTF8);
  var T1 := T0; // asignment
  Assert.AreEqual(UTF8Data, T1.Data, 'Check Data');
  Assert.AreEqual(TTextDataType.UTF8, T1.DataType, 'Check Type');
end;

function TTestTextData.CompareWithStringLoss(Expected,
  Actual: RawByteString): Boolean;
begin
  // Every instance of ~ in Expected, same character at same index in actual
  // gets replaced by a ~. This is to make sure that uncoverted characters are
  // ignored.
  if Length(Expected) <> Length(Actual) then
    Exit(False);
  for var I := 1 to Length(Expected) do
    if Expected[I] = '~' then
      Actual[I] := '~';
  Result := AnsiSameStr(Expected, Actual);
end;

procedure TTestTextData.ctor_for_data_array_works_for_ANSI;
begin
  var T := TTextData.Create(ANSIData, TTextDataType.ANSI);
  Assert.AreEqual(ANSIData, T.Data, 'Check Data');
  Assert.AreEqual(TTextDataType.ANSI, T.DataType, 'Check Type');
end;

procedure TTestTextData.ctor_for_data_array_works_for_ASCII;
begin
  var T := TTextData.Create(ASCIIData, TTextDataType.ASCII);
  Assert.AreEqual(ASCIIData, T.Data, 'Check Data');
  Assert.AreEqual(TTextDataType.ASCII, T.DataType, 'Check Type');
end;

procedure TTestTextData.ctor_for_data_array_works_for_UTF8;
begin
  var T := TTextData.Create(UTF8Data, TTextDataType.UTF8);
  Assert.AreEqual(UTF8Data, T.Data, 'Check Data');
  Assert.AreEqual(TTextDataType.UTF8, T.DataType, 'Check Type');
end;

procedure TTestTextData.ctor_for_RawByteString_string_treats_empty_string_as_UTF8;
begin
  var T := TTextData.Create('');
  Assert.AreEqual(EmptyArray, T.Data, 'Check Data');
  Assert.AreEqual(TTextDataType.UTF8, T.DataType, 'Check Data Type');
end;

procedure TTestTextData.ctor_for_RawByteString_string_works_for_ANSI;
begin
  var T := TTextData.Create(ANSIToANSIStr);
  Assert.AreEqual(ANSIData, T.Data, 'Check Data');
  Assert.AreEqual(TTextDataType.ANSI, T.DataType, 'Check Type');
end;

procedure TTestTextData.ctor_for_RawByteString_string_works_for_ASCII;
begin
  var T := TTextData.Create(ASCIIToASCIIStr);
  Assert.AreEqual(ASCIIData, T.Data, 'Check Data');
  Assert.AreEqual(TTextDataType.ASCII, T.DataType, 'Check Type');
end;

procedure TTestTextData.ctor_for_RawByteString_string_works_for_UTF8;
begin
  var T := TTextData.Create(UTF8ToUTF8Str);
  Assert.AreEqual(UTF8Data, T.Data, 'Check Data');
  Assert.AreEqual(TTextDataType.UTF8, T.DataType, 'Check Type');
end;

procedure TTestTextData.ctor_for_RawBytString_raises_exception_for_unsupported_code_page;
begin
  Assert.WillRaise(
    procedure
    type
      CyrillicString = type Ansistring(1251);
      Latin1String = type AnsiString(1252);
    begin
      var SC: CyrillicString := 'Test Cyrillic';
      var SL: Latin1String := 'Test Latin-1';
      // Try two different code pages in case one is default code page for
      // system. Default code page will not cause exception.
      var TL := TTextData.Create(SL);
      var TC := TTextData.Create(SC);
    end,
    Exception
  );
end;

procedure TTestTextData.ctor_for_stream_works_for_ANSI_stream;
begin
  var Stm := TBytesStream.Create(ANSIData);
  Stm.Position := 0;
  var T := TTextData.Create(Stm, TTextDataType.ANSI); // read whole stream
  Assert.AreEqual(ANSIData, T.Data, 'Check Data');
  Assert.AreEqual(TTextDataType.ANSI, T.DataType, 'Check Type');
end;

procedure TTestTextData.ctor_for_stream_works_for_ASCII_stream;
begin
  var Stm := TBytesStream.Create(ASCIIData);
  var T := TTextData.Create(Stm, TTextDataType.ASCII, -9); // read whole stream
  Assert.AreEqual(ASCIIData, T.Data, 'Check Data');
  Assert.AreEqual(TTextDataType.ASCII, T.DataType, 'Check Type');
end;

procedure TTestTextData.ctor_for_stream_works_for_UTF8_stream;
begin
  var Stm := TBytesStream.Create(UTF8Data);
  var T := TTextData.Create(Stm, TTextDataType.UTF8, 0); // read whole stream
  Assert.AreEqual(UTF8Data, T.Data, 'Check Data');
  Assert.AreEqual(TTextDataType.UTF8, T.DataType, 'Check Type');
end;

procedure TTestTextData.ctor_for_stream_works_for_UTF8_stream_with_leading_bytes;
begin
  var Stm := TBytesStream.Create(UTF8StartPaddedStream);
  Stm.Position := 8;    // skip 8 leading bytes
  var T := TTextData.Create(Stm, TTextDataType.UTF8); // read all remaining stream
  Assert.AreEqual(UTF8Data, T.Data, 'Check Data');
  Assert.AreEqual(TTextDataType.UTF8, T.DataType, 'Check Type');
end;

procedure TTestTextData.ctor_for_stream_works_for_UTF8_stream_with_padding_bytes;
begin
  var Stm := TBytesStream.Create(UTF8DoublePaddedStream);
  Stm.Position := 8;    // skip 8 leading bytes
  var T := TTextData.Create(Stm, TTextDataType.UTF8, Length(UTF8Data)); // read just required data
  Assert.AreEqual(UTF8Data, T.Data, 'Check Data');
  Assert.AreEqual(TTextDataType.UTF8, T.DataType, 'Check Type');
end;

procedure TTestTextData.ctor_for_unicode_strings_passed_empty_string_works_for_ANSI;
begin
  var T := TTextData.Create('', TTextDataType.ANSI);
  Assert.AreEqual(0, Integer(Length(T.Data)), 'Check Data Size');
  Assert.AreEqual(EmptyArray, T.Data, 'Check Data');
  Assert.AreEqual(TTextDataType.ANSI, T.DataType, 'Check Type');
end;

procedure TTestTextData.ctor_for_unicode_strings_passed_empty_string_works_for_ASCII;
begin
  var T := TTextData.Create('', TTextDataType.ASCII);
  Assert.AreEqual(0, Integer(Length(T.Data)), 'Check Data Size');
  Assert.AreEqual(EmptyArray, T.Data, 'Check Data');
  Assert.AreEqual(TTextDataType.ASCII, T.DataType, 'Check Type');
end;

procedure TTestTextData.ctor_for_unicode_strings_passed_empty_string_works_for_UTF8;
begin
  var T := TTextData.Create('', TTextDataType.UTF8);
  Assert.AreEqual(0, Integer(Length(T.Data)), 'Check Data Size');
  Assert.AreEqual(EmptyArray, T.Data, 'Check Data');
  Assert.AreEqual(TTextDataType.UTF8, T.DataType, 'Check Type');
end;

procedure TTestTextData.ctor_for_unicode_strings_works_for_ANSI;
begin
  var T := TTextData.Create(ANSIStr, TTextDataType.ANSI);
  Assert.AreEqual(ANSIData, T.Data, 'Check Data');
  Assert.AreEqual(TTextDataType.ANSI, T.DataType, 'Check Type');
end;

procedure TTestTextData.ctor_for_unicode_strings_works_for_ASCII;
begin
  var T := TTextData.Create(ASCIIStr, TTextDataType.ASCII);
  Assert.AreEqual(ASCIIData, T.Data, 'Check Data');
  Assert.AreEqual(TTextDataType.ASCII, T.DataType, 'Check Type');
end;

procedure TTestTextData.ctor_for_unicode_strings_works_for_UTF8;
begin
  var T := TTextData.Create(UTF8Str, TTextDataType.UTF8);
  Assert.AreEqual(UTF8Data, T.Data, 'Check Data');
  Assert.AreEqual(TTextDataType.UTF8, T.DataType, 'Check Type');
end;

procedure TTestTextData.DataLength_returns_correct_size_of_data_after_array_ctor;
begin
  var T := TTextData.Create(ASCIIData, TTextDataType.ASCII);
  Assert.AreEqual(NativeUInt(Length(ASCIIData)), T.DataLength);
end;

procedure TTestTextData.DataLength_returns_correct_size_of_data_after_string_ctor;
begin
  var T := TTextData.Create(UTF8Str, TTextDataType.UTF8);
  Assert.AreEqual(NativeUInt(Length(UTF8Data)), T.DataLength);
end;

procedure TTestTextData.Default_ctor_creates_empty_UTF8_data;
begin
  var T: TTextData;   // calls default ctor
  Assert.AreEqual(0, Integer(Length(T.Data)), 'Check Data Size');
  Assert.AreEqual(EmptyArray, T.Data, 'Check Data');
  Assert.AreEqual(TTextDataType.UTF8, T.DataType, 'Check DataType is UTF8');
end;

procedure TTestTextData.Encoding_works_with_ANSI_source;
begin
  var T := TTextData.Create(ANSIData, TTextDataType.ANSI);
  Assert.AreEqual(TEncoding.ANSI.CodePage, T.Encoding.CodePage);
end;

procedure TTestTextData.Encoding_works_with_ASCII_source;
begin
  var T := TTextData.Create(ASCIIData, TTextDataType.ASCII);
  Assert.AreEqual(TEncoding.ASCII.CodePage, T.Encoding.CodePage);
end;

procedure TTestTextData.Equal_op_returns_false_with_unequal_ANSI_strings;
begin
  var T1 := TTextData.Create(ANSIStr, TTextDataType.ANSI);
  var T2 := TTextData.Create(ANSINEQStr1, TTextDataType.ANSI);
  var T3 := TTextData.Create(ANSINEQStr2, TTextDataType.ANSI);
  var T4 := TTextData.Create(ASCIIStr, TTextDataType.ASCII);
  Assert.IsFalse(T1 = T2, 'T1 = T2 (same length)');
  Assert.IsFalse(T3 = T1, 'T3 = T1 (different length)');
  Assert.IsFalse(T1 = T4, 'T1 = T4 (different type)');
end;

procedure TTestTextData.Equal_op_returns_false_with_unequal_ASCII_strings;
begin
  var T1 := TTextData.Create(ASCIIStr, TTextDataType.ASCII);
  var T2 := TTextData.Create(ASCIINEQStr1, TTextDataType.ASCII);
  var T3 := TTextData.Create(ASCIINEQStr2, TTextDataType.ASCII);
  var T4 := TTextData.Create(ANSIStr, TTextDataType.ANSI);
  Assert.IsFalse(T1 = T2, 'T1 = T2 (same length)');
  Assert.IsFalse(T3 = T1, 'T3 = T1 (different length)');
  Assert.IsFalse(T1 = T4, 'T1 = T4 (different type)');
end;

procedure TTestTextData.Equal_op_returns_false_with_unequal_UTF8_strings;
begin
  var T1 := TTextData.Create(UTF8Str, TTextDataType.UTF8);
  var T2 := TTextData.Create(UTF8NEQStr1, TTextDataType.UTF8);
  var T3 := TTextData.Create(UTF8NEQStr2, TTextDataType.UTF8);
  var T4 := TTextData.Create(ANSIStr, TTextDataType.ANSI);
  Assert.IsFalse(T1 = T2, 'T1 = T2 (same length)');
  Assert.IsFalse(T3 = T1, 'T3 = T1 (different length)');
  Assert.IsFalse(T1 = T4, 'T1 = T4 (different type)');
end;

procedure TTestTextData.Equal_op_returns_true_with_equal_ANSI_strings;
begin
  var T1 := TTextData.Create(ANSIStr, TTextDataType.ANSI);
  var T2 := TTextData.Create(ANSIEQStr, TTextDataType.ANSI);
  Assert.IsTrue(T1 = T2, 'T1 = T2');
  Assert.IsTrue(T2 = T1, 'T2 = T1 (commutative)');
  var E1 := TTextData.Create('', TTextDataType.ANSI);
  var E2 := TTextData.Create('', TTextDataType.ANSI);
  Assert.IsTrue(E1 = E2, 'E1 = E2 (empty)');
end;

procedure TTestTextData.Equal_op_returns_true_with_equal_ASCII_strings;
begin
  var T1 := TTextData.Create(ASCIIStr, TTextDataType.ASCII);
  var T2 := TTextData.Create(ASCIIEQStr, TTextDataType.ASCII);
  Assert.IsTrue(T1 = T2, 'T1 = T2');
  Assert.IsTrue(T2 = T1, 'T2 = T1 (commutative)');
  var E1 := TTextData.Create('', TTextDataType.ASCII);
  var E2 := TTextData.Create('', TTextDataType.ASCII);
  Assert.IsTrue(E1 = E2, 'E1 = E2 (empty)');
end;

procedure TTestTextData.Equal_op_returns_true_with_equal_UTF8_strings;
begin
  var T1 := TTextData.Create(UTF8Str, TTextDataType.UTF8);
  var T2 := TTextData.Create(UTF8EQStr, TTextDataType.UTF8);
  Assert.IsTrue(T1 = T2, 'T1 = T2');
  Assert.IsTrue(T2 = T1, 'T2 = T1 (commutative)');
  var E1 := TTextData.Create('', TTextDataType.UTF8);
  var E2 := TTextData.Create('', TTextDataType.UTF8);
  Assert.IsTrue(E1 = E2, 'E1 = E2 (empty)');
end;

procedure TTestTextData.IsEmpty_returns_false_with_non_empty_ANSI_string;
begin
  var T := TTextData.Create(ANSIStr, TTextDataType.ANSI);
  Assert.IsFalse(T.IsEmpty);
end;

procedure TTestTextData.IsEmpty_returns_false_with_non_empty_ASCII_string;
begin
  var T := TTextData.Create(ASCIIStr, TTextDataType.ASCII);
  Assert.IsFalse(T.IsEmpty);
end;

procedure TTestTextData.IsEmpty_returns_false_with_non_empty_UTF8_string;
begin
  var T := TTextData.Create(UTF8Str, TTextDataType.UTF8);
  Assert.IsFalse(T.IsEmpty);
end;

procedure TTestTextData.IsEmpty_returns_true_with_empty_ANSI_string;
begin
  var T := TTextData.Create(string.Empty, TTextDataType.ANSI);
  Assert.IsTrue(T.IsEmpty);
end;

procedure TTestTextData.IsEmpty_returns_true_with_empty_ASCII_string;
begin
  var T := TTextData.Create(string.Empty, TTextDataType.ASCII);
  Assert.IsTrue(T.IsEmpty);
end;

procedure TTestTextData.IsEmpty_returns_true_with_empty_UTF8_string;
begin
  var T := TTextData.Create(string.Empty, TTextDataType.UTF8);
  Assert.IsTrue(T.IsEmpty);
end;

procedure TTestTextData.NotEqual_op_returns_false_with_equal_ANSI_strings;
begin
  var T1 := TTextData.Create(ANSIStr, TTextDataType.ANSI);
  var T2 := TTextData.Create(ANSIEQStr, TTextDataType.ANSI);
  Assert.IsFalse(T1 <> T2, 'T1 <> T2');
  Assert.IsFalse(T2 <> T1, 'T2 <> T1 (commutative)');
  var E1 := TTextData.Create('', TTextDataType.ANSI);
  var E2 := TTextData.Create('', TTextDataType.ANSI);
  Assert.IsFalse(E1 <> E2, 'E1 <> E2 (empty)');
end;

procedure TTestTextData.NotEqual_op_returns_false_with_equal_ASCII_strings;
begin
  var T1 := TTextData.Create(ASCIIStr, TTextDataType.ASCII);
  var T2 := TTextData.Create(ASCIIEQStr, TTextDataType.ASCII);
  Assert.IsFalse(T1 <> T2, 'T1 <> T2');
  Assert.IsFalse(T2 <> T1, 'T2 <> T1 (commutative)');
  var E1 := TTextData.Create('', TTextDataType.ASCII);
  var E2 := TTextData.Create('', TTextDataType.ASCII);
  Assert.IsFalse(E1 <> E2, 'E1 <> E2 (empty)');
end;

procedure TTestTextData.NotEqual_op_returns_false_with_equal_UTF8_strings;
begin
  var T1 := TTextData.Create(UTF8Str, TTextDataType.UTF8);
  var T2 := TTextData.Create(UTF8EQStr, TTextDataType.UTF8);
  Assert.IsFalse(T1 <> T2, 'T1 <> T2');
  Assert.IsFalse(T2 <> T1, 'T2 <> T1 (commutative)');
  var E1 := TTextData.Create('', TTextDataType.UTF8);
  var E2 := TTextData.Create('', TTextDataType.UTF8);
  Assert.IsFalse(E1 <> E2, 'E1 <> E2 (empty)');
end;

procedure TTestTextData.NotEqual_op_returns_true_with_unequal_ANSI_strings;
begin
  var T1 := TTextData.Create(ANSIStr, TTextDataType.ANSI);
  var T2 := TTextData.Create(ANSINEQStr1, TTextDataType.ANSI);
  var T3 := TTextData.Create(ANSINEQStr2, TTextDataType.ANSI);
  var T4 := TTextData.Create(UTF8Str, TTextDataType.UTF8);
  Assert.IsTrue(T1 <> T2, 'T1 <> T2 (same length)');
  Assert.IsTrue(T3 <> T1, 'T3 <> T1 (different length)');
  Assert.IsTrue(T1 <> T4, 'T1 <> T4 (different type)');
end;

procedure TTestTextData.NotEqual_op_returns_true_with_unequal_ASCII_strings;
begin
  var T1 := TTextData.Create(ASCIIStr, TTextDataType.ASCII);
  var T2 := TTextData.Create(ASCIINEQStr1, TTextDataType.ASCII);
  var T3 := TTextData.Create(ASCIINEQStr2, TTextDataType.ASCII);
  var T4 := TTextData.Create(ANSIStr, TTextDataType.ANSI);
  Assert.IsTrue(T1 <> T2, 'T1 <> T2 (same length)');
  Assert.IsTrue(T3 <> T1, 'T3 <> T1 (different length)');
  Assert.IsTrue(T1 <> T4, 'T1 <> T4 (different type)');
end;

procedure TTestTextData.NotEqual_op_returns_true_with_unequal_UTF8_strings;
begin
  var T1 := TTextData.Create(UTF8Str, TTextDataType.UTF8);
  var T2 := TTextData.Create(UTF8NEQStr1, TTextDataType.UTF8);
  var T3 := TTextData.Create(UTF8NEQStr2, TTextDataType.UTF8);
  var T4 := TTextData.Create(ANSIStr, TTextDataType.ANSI);
  Assert.IsTrue(T1 <> T2, 'T1 <> T2 (same length)');
  Assert.IsTrue(T3 <> T1, 'T3 <> T1 (different length)');
  Assert.IsTrue(T1 <> T4, 'T1 <> T4 (different type)');
end;

procedure TTestTextData.Setup;
begin
end;

procedure TTestTextData.SupportsString_returns_false_for_bad_ANSI_string;
begin
  var Res := TTextData.SupportsString(TTextDataType.ANSI, UTF8Str);
  Assert.IsFalse(Res, 'UTF8 string invalid');
end;

procedure TTestTextData.SupportsString_returns_false_for_bad_ASCII_string;
begin
  var Res := TTextData.SupportsString(TTextDataType.ASCII, ANSIStr);
  Assert.IsFalse(Res, 'ANSI string invalid');
  Res := TTextData.SupportsString(TTextDataType.ASCII, UTF8Str);
  Assert.IsFalse(Res, 'UTF8 string invalid');
end;

procedure TTestTextData.SupportsString_returns_true_for_valid_UTF8_string;
begin
  var Res := TTextData.SupportsString(TTextDataType.UTF8, ASCIIStr);
  Assert.IsTrue(Res, 'ASCII string valid');
  Res := TTextData.SupportsString(TTextDataType.UTF8, ANSIStr);
  Assert.IsTrue(Res, 'ANSI string valid');
  Res := TTextData.SupportsString(TTextDataType.UTF8, UTF8Str);
  Assert.IsTrue(Res, 'UTF8 string valid');
end;

procedure TTestTextData.SupportsString_returns_true_with_valid_ASCII_string;
begin
  var Res := TTextData.SupportsString(TTextDataType.ASCII, ASCIIStr);
  Assert.IsTrue(Res, 'ASCII string valid');
end;

procedure TTestTextData.TearDown;
begin
end;

procedure TTestTextData.ToANSIString_works_with_ANSI_source;
begin
  var T := TTextData.Create(ANSIStr, TTextDataType.ANSI);
  var S: AnsiString := T.ToANSIString;
  Assert.AreEqual(ANSIToANSIStr, S, 'Check equal');
  Assert.AreEqual(TEncoding.ANSI.CodePage, Cardinal(StringCodePage(S)), 'Check code page');
end;

procedure TTestTextData.ToANSIString_works_with_ASCII_source;
begin
  var T := TTextData.Create(ASCIIStr, TTextDataType.ASCII);
  var S: AnsiString := T.ToANSIString;
  Assert.AreEqual(ASCIIToANSIStr, S, 'Check equal');
  Assert.AreEqual(TEncoding.ANSI.CodePage, Cardinal(StringCodePage(S)), 'Check code page');
end;

procedure TTestTextData.ToANSIString_works_with_UTF8_source_with_character_loss;
begin
  var T := TTextData.Create(UTF8Str, TTextDataType.UTF8);
  var S: AnsiString := T.ToANSIString;
  Assert.IsTrue(CompareWithStringLoss(UTF8ToANSIStr, S), 'Check equal but for loss');
  Assert.AreEqual(TEncoding.ANSI.CodePage, Cardinal(StringCodePage(S)), 'Check code page');
end;

procedure TTestTextData.ToASCIIString_works_with_ANSI_source_with_character_loss;
begin
  var T := TTextData.Create(ANSIStr, TTextDataType.ANSI);
  var S: ASCIIString := T.ToASCIIString;
  Assert.IsTrue(CompareWithStringLoss(ANSIToASCIIStr, S), 'Check equal but for loss');
  Assert.AreEqual(TEncoding.ASCII.CodePage, Cardinal(StringCodePage(S)), 'Check code page');
end;

procedure TTestTextData.ToASCIIString_works_with_ASCII_source;
begin
  var T := TTextData.Create(ASCIIStr, TTextDataType.ASCII);
  var S: ASCIIString := T.ToASCIIString;
  Assert.AreEqual(ASCIIToASCIIStr, S, 'Check equal');
  Assert.AreEqual(TEncoding.ASCII.CodePage, Cardinal(StringCodePage(S)), 'Check code page');
end;

procedure TTestTextData.ToASCIIString_works_with_UTF8_source_with_character_loss;
begin
  var T := TTextData.Create(UTF8Str, TTextDataType.UTF8);
  var S: ASCIIString := T.ToASCIIString;
  Assert.IsTrue(CompareWithStringLoss(UTF8ToASCIIStr, S), 'Check equal but for loss');
  Assert.AreEqual(TEncoding.ASCII.CodePage, Cardinal(StringCodePage(S)), 'Check code page');
end;

procedure TTestTextData.ToString_works_for_ANSI_data_array;
begin
  var T := TTextData.Create(ANSIData, TTextDataType.ANSI);
  Assert.AreEqual(ANSIStr, T.ToString);
end;

procedure TTestTextData.ToString_works_for_ASCII_string;
begin
  var T := TTextData.Create(ASCIIStr, TTextDataType.ASCII);
  Assert.AreEqual(ASCIIStr, T.ToString);
end;

procedure TTestTextData.ToString_works_for_UTF8_data_array;
begin
  var T := TTextData.Create(UTF8Data, TTextDataType.UTF8);
  Assert.AreEqual(UTF8Str, T.ToString);
end;

procedure TTestTextData.ToUTF8String_works_with_ANSI_source;
begin
  var T := TTextData.Create(ANSIData, TTextDataType.ANSI);
  var S: UTF8String := T.ToUTF8String;
  Assert.AreEqual(ANSIToUTF8Str, S, 'Check equal');
  Assert.AreEqual(TEncoding.UTF8.CodePage, Cardinal(StringCodePage(S)), 'Check code page');
end;

procedure TTestTextData.ToUTF8String_works_with_ASCII_source;
begin
  var T := TTextData.Create(ASCIIData, TTextDataType.ASCII);
  var S: UTF8String := T.ToUTF8String;
  Assert.AreEqual(ASCIIToUTF8Str, S, 'Check equal');
  Assert.AreEqual(TEncoding.UTF8.CodePage, Cardinal(StringCodePage(S)), 'Check code page');
end;

procedure TTestTextData.ToUTF8String_works_with_UTF8_source;
begin
  var T := TTextData.Create(UTF8Data, TTextDataType.UTF8);
  var S: UTF8String := T.ToUTF8String;
  Assert.AreEqual(UTF8ToUTF8Str, S, 'Check equal');
  Assert.AreEqual(TEncoding.UTF8.CodePage, Cardinal(StringCodePage(S)), 'Check code page');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestTextData);

end.
