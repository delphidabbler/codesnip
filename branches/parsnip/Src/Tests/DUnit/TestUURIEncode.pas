{
  Delphi DUnit Test Case for the UStrUtils Unit
  ---------------------------------------------

  $Rev$
  $Date$

  NOTE: This unit *must* be saved in UTF-8 format.
}



unit TestUURIEncode;


interface


uses
  TestFramework, UURIEncode;


type
  // Test methods for UURIEncode unit
  UURIEncodeTests = class(TTestCase)
  protected
    // Index of error to be processed by URIDecodeError method. Must be a valid
    // index into UnicodeEncodedErrors const array.
    fURIDecodeErrorNum: Integer;
    // Calls URIDecode with an erroneous encoded URI segment. Which particular
    // error is determined by fURIDecodeErrorNum which must be set before this
    // method is called.
    procedure URIDecodeError;
  published
    procedure TestURIEncodeUTF8;
    procedure TestURIEncodeUnicode;
    procedure TestURIEncodeAnsi;
    procedure TestEncodeQueryStringUTF8;
    procedure TestEncodeQueryStringUnicode;
    procedure TestEncodeQueryStringAnsi;
    procedure TestURIDecode;
    procedure TestURIDecodeQueryString;
  end;


implementation


uses
  SysUtils;


const

  // URIEncode tests

  // Tests empty string
  UTF8Test0 = UTF8String('');
  UnicodeTest0 = UnicodeString('');
  AnsiTest0 = RawByteString('');
  Result0 = '';

  // Tests only unreserved characters
  UTF8Test1 = UTF8String('abcDEF123_-.~');
  UnicodeTest1 = UnicodeString('abcDEF123_-.~');
  AnsiTest1 = RawByteString('abcDEF123_-.~');
  Result1 = 'abcDEF123_-.~';

  // Tests ASCII character that is neither reserved nor unreserved
  UTF8Test2 = UTF8String('"hello world"');
  UnicodeTest2 = UnicodeString('"hello world"');
  AnsiTest2 = AnsiString('"hello world"');
  Result2 = '%22hello%20world%22';

  // Tests reserved and unreserved characters
  UTF8Test3 = UTF8String('abc#DEF /123%_-.~$');
  UnicodeTest3 = UnicodeString('abc#DEF /123%_-.~$');
  AnsiTest3 = AnsiString('abc#DEF /123%_-.~$');
  Result3 = 'abc%23DEF%20%2F123%25_-.~%24';

  // Tests an ANSI character > $80
  UTF8Test4 = UTF8String('Copyright © 2010.');
  UnicodeTest4 = UnicodeString('Copyright © 2010.');
  AnsiTest4 = AnsiString('Copyright © 2010.');
  Result4 = 'Copyright%20%C2%A9%202010.';

  // Tests string containing characters outside ANSI
  UTF8Test5 = UTF8String('a bÃǄ');
  UnicodeTest5 = UnicodeString('a bÃǄ');
  // Replace above with following if this unit is not saved in UTF-8
  // UTF8Test5 = UTF8String('a b'#$C3#$01C4);
  // UnicodeTest5 = UnicodeString('a b'#$C3#$01C4);
  Result5 = 'a%20b%C3%83%C7%84';

  // URIDecode tests
  // first 6 tests are reverse of URIEncode tests
  UnicodeEncoded0 = UnicodeString(Result0);
  UnicodeEncoded1 = UnicodeString(Result1);
  UnicodeEncoded2 = UnicodeString(Result2);
  UnicodeEncoded3 = UnicodeString(Result3);
  UnicodeEncoded4 = UnicodeString(Result4);
  UnicodeEncoded5 = UnicodeString(Result5);
  // tests for unexpected non-unserserved non-ascii char
  UnicodeEncoded6 = UnicodeString('%22€%22');

  UnicodeEncodedErrors: array[0..2] of UnicodeString = (
    'A%5GB',  // %5G is bad hex
    'AB%5',   // %5 is too short: two digits required
    'AB%'     // % is at end of string
  );

  UnicodeDecoded0 = UnicodeTest0;
  UnicodeDecoded1 = UnicodeTest1;
  UnicodeDecoded2 = UnicodeTest2;
  UnicodeDecoded3 = UnicodeTest3;
  UnicodeDecoded4 = UnicodeTest4;
  UnicodeDecoded5 = UnicodeTest5;
  UnicodeDecoded6 = UnicodeString('"€"');

  // Query string encoding tests

  // Tests for URIEncodeQueryString

  // Unreserved chars only
  QSUTF8Test0 = UTF8String('hello');
  QSUnicodeTest0 = UnicodeString('hello');
  QSAnsiTest0 = AnsiString('hello');
  QSResult0 = 'hello';

  // ASCII chars only
  QSUTF8Test1 = UTF8String('hello world');
  QSUnicodeTest1 = UnicodeString('hello world');
  QSAnsiTest1 = AnsiString('hello world');
  QSResult1 = 'hello+world';

  // Tests an ANSI character > $80
  QSUTF8Test2 = UTF8String('Copyright © 2010.');
  QSUnicodeTest2 = UnicodeString('Copyright © 2010.');
  QSAnsiTest2 = AnsiString('Copyright © 2010.');
  QSResult2 = 'Copyright+%C2%A9+2010.';

  // Test string from http://www.albionresearch.com/misc/urlencode.php
  QSUTF8Test3 = UTF8String('"Aardvarks lurk, OK?"');
  QSUnicodeTest3 = UnicodeString('"Aardvarks lurk, OK?"');
  QSAnsiTest3 = AnsiString('"Aardvarks lurk, OK?"');
  QSResult3 = '%22Aardvarks+lurk%2C+OK%3F%22';

  // Tests a string that contains a '+' sign: modification of test string from
  // http://www.albionresearch.com/misc/urlencode.php
  QSUTF8Test4 = UTF8String('"Aardvarks + lurk = OK?"');
  QSUnicodeTest4 = UnicodeString('"Aardvarks + lurk = OK?"');
  QSAnsiTest4 = AnsiString('"Aardvarks + lurk = OK?"');
  QSResult4 = '%22Aardvarks+%2B+lurk+%3D+OK%3F%22';

  // Tests for URIDecodeQueryString

  QSUnicodeEncoded0 = UnicodeString(QSResult0);
  QSUnicodeEncoded1 = UnicodeString(QSResult1);
  QSUnicodeEncoded2 = UnicodeString(QSResult2);
  QSUnicodeEncoded3 = UnicodeString(QSResult3);
  QSUnicodeEncoded4 = UnicodeString(QSResult4);

  QSUnicodeDecoded0 = QSUnicodeTest0;
  QSUnicodeDecoded1 = QSUnicodeTest1;
  QSUnicodeDecoded2 = QSUnicodeTest2;
  QSUnicodeDecoded3 = QSUnicodeTest3;
  QSUnicodeDecoded4 = QSUnicodeTest4;


{ UURIEncodeTests }

procedure UURIEncodeTests.TestEncodeQueryStringAnsi;
begin
  CheckEquals(QSResult0, URIEncodeQueryString(QSAnsiTest0), 'Test 0');
  CheckEquals(QSResult1, URIEncodeQueryString(QSAnsiTest1), 'Test 1');
  CheckEquals(QSResult2, URIEncodeQueryString(QSAnsiTest2), 'Test 2');
  CheckEquals(QSResult3, URIEncodeQueryString(QSAnsiTest3), 'Test 3');
  CheckEquals(QSResult4, URIEncodeQueryString(QSAnsiTest4), 'Test 4');
end;

procedure UURIEncodeTests.TestEncodeQueryStringUnicode;
begin
  CheckEquals(QSResult0, URIEncodeQueryString(QSUnicodeTest0),
    'Unicode failure 0');
  CheckEquals(QSResult1, URIEncodeQueryString(QSUnicodeTest1),
    'Unicode failure 1');
  CheckEquals(QSResult2, URIEncodeQueryString(QSUnicodeTest2),
    'Unicode failure 2');
  CheckEquals(QSResult3, URIEncodeQueryString(QSUnicodeTest3),
    'Unicode failure 3');
  CheckEquals(QSResult4, URIEncodeQueryString(QSUnicodeTest4),
    'Unicode failure 4');
end;

procedure UURIEncodeTests.TestEncodeQueryStringUTF8;
begin
  CheckEquals(QSResult0, URIEncodeQueryString(QSUTF8Test0), 'Test 0');
  CheckEquals(QSResult1, URIEncodeQueryString(QSUTF8Test1), 'Test 1');
  CheckEquals(QSResult2, URIEncodeQueryString(QSUTF8Test2), 'Test 2');
  CheckEquals(QSResult3, URIEncodeQueryString(QSUTF8Test3), 'Test 3');
  CheckEquals(QSResult4, URIEncodeQueryString(QSUTF8Test4), 'Test 4');
end;

procedure UURIEncodeTests.TestURIDecode;
begin
  CheckEquals(UnicodeDecoded0, URIDecode(UnicodeEncoded0), 'Test 0');
  CheckEquals(UnicodeDecoded1, URIDecode(UnicodeEncoded1), 'Test 1');
  CheckEquals(UnicodeDecoded2, URIDecode(UnicodeEncoded2), 'Test 2');
  CheckEquals(UnicodeDecoded3, URIDecode(UnicodeEncoded3), 'Test 3');
  CheckEquals(UnicodeDecoded4, URIDecode(UnicodeEncoded4), 'Test 4');
  CheckEquals(UnicodeDecoded5, URIDecode(UnicodeEncoded5), 'Test 5');
  CheckEquals(UnicodeDecoded6, URIDecode(UnicodeEncoded6), 'Test 6');
  fURIDecodeErrorNum := 0;
  CheckException(URIDecodeError, EConvertError);
  fURIDecodeErrorNum := 1;
  CheckException(URIDecodeError, EConvertError);
  fURIDecodeErrorNum := 2;
  CheckException(URIDecodeError, EConvertError);
end;

procedure UURIEncodeTests.TestURIDecodeQueryString;
begin
  CheckEquals(QSUnicodeDecoded0, URIDecodeQueryString(QSUnicodeEncoded0),
    'Test 0');
  CheckEquals(QSUnicodeDecoded1, URIDecodeQueryString(QSUnicodeEncoded1),
    'Test 1');
  CheckEquals(QSUnicodeDecoded2, URIDecodeQueryString(QSUnicodeEncoded2),
    'Test 2');
  CheckEquals(QSUnicodeDecoded3, URIDecodeQueryString(QSUnicodeEncoded3),
    'Test 3');
  CheckEquals(QSUnicodeDecoded4, URIDecodeQueryString(QSUnicodeEncoded4),
    'Test 4');
end;

procedure UURIEncodeTests.TestURIEncodeAnsi;
begin
  CheckEquals(Result0, URIEncode(AnsiTest0), 'Test 0');
  CheckEquals(Result1, URIEncode(AnsiTest1), 'Test 1');
  CheckEquals(Result2, URIEncode(AnsiTest2), 'Test 2');
  CheckEquals(Result3, URIEncode(AnsiTest3), 'Test 3');
  CheckEquals(Result4, URIEncode(AnsiTest4), 'Test 4');
end;

procedure UURIEncodeTests.TestURIEncodeUnicode;
begin
  CheckEquals(Result0, URIEncode(UnicodeTest0), 'Test 0');
  CheckEquals(Result1, URIEncode(UnicodeTest1), 'Test 1');
  CheckEquals(Result2, URIEncode(UnicodeTest2), 'Test 2');
  CheckEquals(Result3, URIEncode(UnicodeTest3), 'Test 3');
  CheckEquals(Result4, URIEncode(UnicodeTest4), 'Test 4');
  CheckEquals(Result5, URIEncode(UnicodeTest5), 'Test 5');
end;

procedure UURIEncodeTests.TestURIEncodeUTF8;
begin
  CheckEquals(Result0, URIEncode(UTF8Test0), 'Test 0');
  CheckEquals(Result1, URIEncode(UTF8Test1), 'Test 1');
  CheckEquals(Result2, URIEncode(UTF8Test2), 'Test 2');
  CheckEquals(Result3, URIEncode(UTF8Test3), 'Test 3');
  CheckEquals(Result4, URIEncode(UTF8Test4), 'Test 4');
  CheckEquals(Result5, URIEncode(UTF8Test5), 'Test 5');
end;

procedure UURIEncodeTests.URIDecodeError;
begin
  URIDecode(UnicodeEncodedErrors[fURIDecodeErrorNum]);
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(UURIEncodeTests.Suite);
end.

