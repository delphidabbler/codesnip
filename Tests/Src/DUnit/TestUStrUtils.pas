{
  Delphi DUnit Test Case for the UStrUtils Unit
  ---------------------------------------------

  $Rev$
  $Date$
}

unit TestUStrUtils;


interface


uses
  // Delphi
  Classes,
  // DUnit
  TestFramework,
  // Project
  UStrUtils;


type

  // Test methods for each routine in interface of UStrUtils
  TTestRoutines = class(TTestCase)
  strict private
    fStrings: TStringList;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestStrContainsStr;
    procedure TestStrPos_overload1;
    procedure TestStrPos_overload2;
    procedure TestStrLastPos;
    procedure TestStrCompareText;
    procedure TestStrSameStr;
    procedure TestStrSameText;
    procedure TestStrToUpper;
    procedure TestStrToLower;
    procedure TestStrSlice;
    procedure TestStrSliceLeft;
    procedure TestStrSliceRight;
    procedure TestStrStartsStr;
    procedure TestStrStartsText;
    procedure TestStrReplace;
    procedure TestStrTrim;
    procedure TestStrTrimLeft;
    procedure TestStrTrimRight;
    procedure TestStrTrimChars;
    procedure TestStrTrimLeftChars;
    procedure TestStrTrimRightChars;
    procedure TestStrWindowsLineBreaks;
    procedure TestStrUnixLineBreaks;
    procedure TestStrIsDelimiter;
    procedure TestStrCountDelims;
    procedure TestStrLastDelimiterPos;
    procedure TestStrCapitaliseWords;
    procedure TestStrCompressWhiteSpace;
    procedure TestStrStripWhiteSpace;
    procedure TestStrContainsWhiteSpace;
    procedure TestStrQuoteSpaced;
    procedure TestStrJoin;
    procedure TestStrExplode;
    procedure TestStrSplit;
    procedure TestStrWrap;
    procedure TestStrMakeSentence;
    procedure TestStrBackslashEscape;
  end;


implementation


uses
  // Project
  UConsts;


{ TTestRoutines }

procedure TTestRoutines.SetUp;
begin
  fStrings := TStringList.Create;
end;

procedure TTestRoutines.TearDown;
begin
  fStrings.Free;
end;

procedure TTestRoutines.TestStrBackslashEscape;
var
  Orig, Expected, Res, Escapable, Escapes: string;
begin
  Escapable := '\"' + TAB + LF + FF;
  Escapes := '\"tnf';
  Orig := '';
  Expected := '';
  Res := StrBackslashEscape(Orig, Escapable, Escapes);
  CheckEquals(Expected, Res, 'Test 1');

  Orig := '"fred' + TAB + LF + 'blogg\s"';
  Expected := '\"fred\t\nblogg\\s\"';
  Res := StrBackslashEscape(Orig, Escapable, Escapes);
  CheckEquals(Expected, Res, 'Test 2');

  Orig := '"fred' + TAB + LF + 'blogg\s"';
  Expected := '"fred\t\nblogg\s"';
  Escapable := TAB + LF;
  Escapes := 'tn';
  Res := StrBackslashEscape(Orig, Escapable, Escapes);
  CheckEquals(Expected, Res, 'Test 3');

  Escapable := '\"' + TAB + LF + FF;
  Escapes := '\"tnf';
  Orig := '\\\\\';
  Expected := '\\\\\\\\\\';
  Res := StrBackslashEscape(Orig, Escapable, Escapes);
  CheckEquals(Expected, Res, 'Test 4');

  Orig := FF;
  Expected := '\f';
  Res := StrBackslashEscape(Orig, Escapable, Escapes);
  CheckEquals(Expected, Res, 'Test 5');

  Orig := 'No escaping here';
  Expected := Orig;
  Res := StrBackslashEscape(Orig, Escapable, Escapes);
  CheckEquals(Expected, Res, 'Test 6');
end;

procedure TTestRoutines.TestStrCapitaliseWords;
begin
  CheckEquals('', StrCapitaliseWords(''), 'Test 1');
  CheckEquals('Hello', StrCapitaliseWords('hello'), 'Test 2');
  CheckEquals('Hello', StrCapitaliseWords('Hello'), 'Test 3');
  CheckEquals('HELLO', StrCapitaliseWords('HELLO'), 'Test 4');
  CheckEquals('Hello World', StrCapitaliseWords('hello world'), 'Test 5');
  CheckEquals('Hello World', StrCapitaliseWords('Hello World'), 'Test 6');
  CheckEquals('HELLO WORLD', StrCapitaliseWords('HELLO WORLD'), 'Test 7');
  CheckEquals('42,the Question.', StrCapitaliseWords('42,the question.'),
    'Test 8');
  CheckEquals('The $5million Question',
    StrCapitaliseWords('the $5million question'), 'Test 9');
  CheckEquals(' Hello  World ', StrCapitaliseWords(' hello  world '),
    'Test 10');
  CheckEquals('42'#9'The'#13#10'Question',
    StrCapitaliseWords('42'#9'the'#13#10'question'), 'Test 11');
  CheckEquals(' ', StrCapitaliseWords(' '), 'Test 12');
end;

procedure TTestRoutines.TestStrCompareText;
begin
  CheckTrue(StrCompareText('Iñtërnâtiônàlizætiøn', 'IñtërnÂtiônàlizætiøn') = 0,
    'Test 1');
  CheckTrue(StrCompareText('Iñtërnâtiônàlizætiøn', 'Iñtërnâtiônàlizætiøn') = 0,
    'Test 2');
  CheckTrue(StrCompareText('Foo', 'Bar') > 0, 'Test 3');
  CheckTrue(StrCompareText('Bar', 'Foo') < 0, 'Test 4');
  CheckTrue(StrCompareText('Foo', 'Fo') > 0, 'Test 5');
  CheckTrue(StrCompareText('Fo', 'Foo') < 0, 'Test 6');
  CheckTrue(StrCompareText('FO', 'Foo') < 0, 'Test 7');
  CheckTrue(StrCompareText('Fo', 'FOO') < 0, 'Test 8');
  CheckTrue(StrCompareText('foo', 'FOO') = 0, 'Test 9');
  CheckTrue(StrCompareText('', '') = 0, 'Test 10');
  CheckTrue(StrCompareText('Foo', '') > 0, 'Test 11');
  CheckTrue(StrCompareText('', 'Bar') < 0, 'Test 12');
end;

procedure TTestRoutines.TestStrCompressWhiteSpace;
begin
  CheckEquals('', StrCompressWhiteSpace(''), 'Test 1');
  CheckEquals(' ', StrCompressWhiteSpace(' '), 'Test 2');
  CheckEquals(' ', StrCompressWhiteSpace(#13#10), 'Test 3');
  CheckEquals(' ', StrCompressWhiteSpace(#9'  '#11' '#9#9#9), 'Test 4');
  CheckEquals(' ', StrCompressWhiteSpace('     '), 'Test 5');
  CheckEquals('Foo', StrCompressWhiteSpace('Foo'), 'Test 6');
  CheckEquals('Foo Bar', StrCompressWhiteSpace('Foo Bar'), 'Test 7');
  CheckEquals(' Foo Bar ', StrCompressWhiteSpace(' Foo Bar '), 'Test 8');
  CheckEquals(' Foo Bar ', StrCompressWhiteSpace('  Foo Bar  '), 'Test 9');
  CheckEquals('Foo Bar', StrCompressWhiteSpace('Foo   Bar'), 'Test 10');
  CheckEquals('Foo Bar', StrCompressWhiteSpace('Foo'#13#10'Bar'), 'Test 11');
  CheckEquals(' Foo Bar ', StrCompressWhiteSpace('  Foo   Bar  '), 'Test 12');
  CheckEquals('Foo Bar', StrCompressWhiteSpace('Foo'#9'Bar'), 'Test 13');
end;

procedure TTestRoutines.TestStrContainsStr;
begin
  CheckFalse(StrContainsStr('Fo', 'Bar'), 'Test 1');
  CheckFalse(StrContainsStr('Ar', 'Bar'), 'Test 2');
  CheckTrue(StrContainsStr('Fo', 'Foo'), 'Test 3');
  CheckTrue(StrContainsStr('ar', 'Bar'), 'Test 4');
  CheckTrue(StrContainsStr('o', 'Foo'), 'Test 5');
  CheckTrue(StrContainsStr('Bar', 'Bar'), 'Test 6');
  CheckFalse(StrContainsStr('BAR', 'bar'), 'Test 7');
  CheckTrue(StrContainsStr('nâtiôn', 'Iñtërnâtiônàlizætiøn'), 'Test 8');
  CheckFalse(StrContainsStr('nâTiôn', 'Iñtërnâtiônàlizætiøn'), 'Test 9');
  CheckFalse(StrContainsStr('nÂtiôn', 'Iñtërnâtiônàlizætiøn'), 'Test 10');
  CheckFalse(StrContainsStr('', 'Bar'), 'Test 11');
  CheckFalse(StrContainsStr('Bar', ''), 'Test 12');
end;

procedure TTestRoutines.TestStrContainsWhiteSpace;
begin
  CheckFalse(StrContainsWhiteSpace(''), 'Test 1');
  CheckFalse(StrContainsWhiteSpace('Foo'), 'Test 2');
  CheckTrue(StrContainsWhiteSpace(' '), 'Test 3');
  CheckTrue(StrContainsWhiteSpace('   '), 'Test 4');
  CheckTrue(StrContainsWhiteSpace(#13#10#9#12), 'Test 5');
  CheckTrue(StrContainsWhiteSpace('Foo Bar'), 'Test 6');
  CheckTrue(StrContainsWhiteSpace(' Foo'), 'Test 7');
  CheckTrue(StrContainsWhiteSpace('Foo '), 'Test 8');
  CheckTrue(StrContainsWhiteSpace(#9'Foo'#9'Bar'#13#10), 'Test 9');
end;

procedure TTestRoutines.TestStrCountDelims;
const
  Str = 'foo,bar;42,56.';
begin
  CheckEquals(0, StrCountDelims('', ''), 'Test 1');
  CheckEquals(0, StrCountDelims(',;.', ''), 'Test 2');
  CheckEquals(1, StrCountDelims('.', Str), 'Test 3');
  CheckEquals(2, StrCountDelims(',', Str), 'Test 4');
  CheckEquals(1, StrCountDelims(';', Str), 'Test 5');
  CheckEquals(2, StrCountDelims('.;', Str), 'Test 6');
  CheckEquals(4, StrCountDelims(',;.', Str), 'Test 7');
  CheckEquals(4, StrCountDelims('.,;', Str), 'Test 8');
  CheckEquals(2, StrCountDelims('o', Str), 'Test 9');
  CheckEquals(0, StrCountDelims('O', Str), 'Test 10');
  CheckEquals(2, StrCountDelims('42', Str), 'Test 11');
  CheckEquals(0, StrCountDelims('', Str), 'Test 12');
  CheckEquals(1, StrCountDelims(';;', Str), 'Test 13');
  CheckEquals(4, StrCountDelims(',;,.', Str), 'Test 14');
end;

procedure TTestRoutines.TestStrExplode;

  function ArrayToStr(const A: array of string): string;
  var
    S: string;
  begin
    Result := '';
    for S in A do
    begin
      if Result <> '' then
        Result := Result + ',';
      Result := Result + '''' + S + '''';
    end;
    Result := '[' + Result + ']';
  end;

  function ListToStr(const List: TStrings): string;
  var
    Idx: Integer;
    A: array of string;
  begin
    SetLength(A, List.Count);
    for Idx := 0 to Pred(List.Count) do
      A[Idx] := List[Idx];
    Result := ArrayToStr(A);
  end;

  function ErrMessage(const Expected: array of string; const List: TStrings):
    string;
  begin
    Result := 'Expected: '
      + ArrayToStr(Expected)
      + '  Got: '
      + ListToStr(List);
  end;

  procedure CheckList(const Expected: array of string; const List: TStrings;
    const Test: string);
  var
    Passed: Boolean;
    Idx: Integer;
  begin
    Passed := True;
    if Length(Expected) <> List.Count then
      Passed := False
    else
    begin
      for Idx := 0 to Pred(List.Count) do
      begin
        if Expected[Idx] <> List[Idx] then
        begin
          Passed := False;
          Break;
        end;
      end;
    end;
    Check(Passed, Test + ' ' + ErrMessage(Expected, List));
  end;

var
  Count: Integer;
  Str: string;
begin
  Str := 'Foo|| Bar||  ||42 |||| 56 ';
  fStrings.Clear;
  Count := StrExplode(Str, '||', fStrings, True, False);
  CheckList(['Foo', ' Bar', '  ', '42 ', '', ' 56 '], fStrings, 'Test 1.1.1');
  CheckEquals(6, Count, 'Test 1.1.2');
  fStrings.Clear;
  Count := StrExplode(Str, '||', fStrings, False, False);
  CheckList(['Foo', ' Bar', '  ', '42 ', ' 56 '], fStrings, 'Test 1.2.1');
  CheckEquals(5, Count, 'Test 1.2.2');
  fStrings.Clear;
  Count := StrExplode(Str, '||', fStrings, True, True);
  CheckList(['Foo', 'Bar', '', '42', '', '56'], fStrings, 'Test 1.3.1');
  CheckEquals(6, Count, 'Test 1.3.2');
  fStrings.Clear;
  Count := StrExplode(Str, '||', fStrings, False, True);
  CheckList(['Foo', 'Bar', '42', '56'], fStrings, 'Test 1.4.1');
  CheckEquals(4, Count, 'Test 1.4.2');

  Str := '||Foo|| Bar||  ||42 |||| 56 ||';
  fStrings.Clear;
  Count := StrExplode(Str, '||', fStrings, True, False);
  CheckList(['', 'Foo', ' Bar', '  ', '42 ', '', ' 56 ', ''], fStrings,
    'Test 2.1.1');
  CheckEquals(8, Count, 'Test 2.1.2');
  fStrings.Clear;
  Count := StrExplode(Str, '||', fStrings, False, False);
  CheckList(['Foo', ' Bar', '  ', '42 ', ' 56 '], fStrings, 'Test 2.2.1');
  CheckEquals(5, Count, 'Test 2.2.2');
  fStrings.Clear;
  Count := StrExplode(Str, '||', fStrings, True, True);
  CheckList(['', 'Foo', 'Bar', '', '42', '', '56', ''], fStrings, 'Test 2.3.1');
  CheckEquals(8, Count, 'Test 2.3.2');
  fStrings.Clear;
  Count := StrExplode(Str, '||', fStrings, False, True);
  CheckList(['Foo', 'Bar', '42', '56'], fStrings, 'Test 2.4.1');
  CheckEquals(4, Count, 'Test 2.4.2');

  Str := '||Foo|| Bar||  ||42 |||| 56 ||';
  fStrings.Clear;
  Count := StrExplode(Str, '||', fStrings, True, False);
  CheckList(['', 'Foo', ' Bar', '  ', '42 ', '', ' 56 ', ''], fStrings,
    'Test 2.1.1');
  CheckEquals(8, Count, 'Test 2.1.2');
  fStrings.Clear;
  Count := StrExplode(Str, '||', fStrings, False, False);
  CheckList(['Foo', ' Bar', '  ', '42 ', ' 56 '], fStrings, 'Test 2.2.1');
  CheckEquals(5, Count, 'Test 2.2.2');
  fStrings.Clear;
  Count := StrExplode(Str, '||', fStrings, True, True);
  CheckList(['', 'Foo', 'Bar', '', '42', '', '56', ''], fStrings, 'Test 2.3.1');
  CheckEquals(8, Count, 'Test 2.3.2');
  fStrings.Clear;
  Count := StrExplode(Str, '||', fStrings, False, True);
  CheckList(['Foo', 'Bar', '42', '56'], fStrings, 'Test 2.4.1');
  CheckEquals(4, Count, 'Test 2.4.2');

  Str := '';
  fStrings.Clear;
  Count := StrExplode(Str, '||', fStrings, True, False);
  CheckList([], fStrings, 'Test 3.1.1');
  CheckEquals(0, Count, 'Test 3.1.2');
  fStrings.Clear;
  Count := StrExplode(Str, '||', fStrings, False, False);
  CheckList([], fStrings, 'Test 3.2.1');
  CheckEquals(0, Count, 'Test 3.2.2');
  fStrings.Clear;
  Count := StrExplode(Str, '||', fStrings, True, True);
  CheckList([], fStrings, 'Test 3.3.1');
  CheckEquals(0, Count, 'Test 3.3.2');
  fStrings.Clear;
  Count := StrExplode(Str, '||', fStrings, False, True);
  CheckList([], fStrings, 'Test 3.4.1');
  CheckEquals(0, Count, 'Test 3.4.2');

  Str := '||Foo||';
  fStrings.Clear;
  Count := StrExplode(Str, '||', fStrings, True, False);
  CheckList(['', 'Foo', ''], fStrings, 'Test 4.1.1');
  CheckEquals(3, Count, 'Test 4.1.2');
  fStrings.Clear;
  Count := StrExplode(Str, '||', fStrings, False, False);
  CheckList(['Foo'], fStrings, 'Test 4.2.1');
  CheckEquals(1, Count, 'Test 4.2.2');
  fStrings.Clear;
  Count := StrExplode(Str, '||', fStrings, True, True);
  CheckList(['', 'Foo', ''], fStrings, 'Test 4.3.1');
  CheckEquals(3, Count, 'Test 4.3.2');
  fStrings.Clear;
  Count := StrExplode(Str, '||', fStrings, False, True);
  CheckList(['Foo'], fStrings, 'Test 4.4.1');
  CheckEquals(1, Count, 'Test 4.4.2');

  Str := '||||';
  fStrings.Clear;
  Count := StrExplode(Str, '||', fStrings, True, False);
  CheckList(['', '', ''], fStrings, 'Test 5.1.1');
  CheckEquals(3, Count, 'Test 5.1.2');
  fStrings.Clear;
  Count := StrExplode(Str, '||', fStrings, False, False);
  CheckList([], fStrings, 'Test 5.2.1');
  CheckEquals(0, Count, 'Test 5.2.2');
  fStrings.Clear;
  Count := StrExplode(Str, '||', fStrings, True, True);
  CheckList(['', '', ''], fStrings, 'Test 5.3.1');
  CheckEquals(3, Count, 'Test 5.3.2');
  fStrings.Clear;
  Count := StrExplode(Str, '||', fStrings, False, True);
  CheckList([], fStrings, 'Test 5.4.1');
  CheckEquals(0, Count, 'Test 5.4.2');

  Str := 'Foo';
  fStrings.Clear;
  Count := StrExplode(Str, '||', fStrings, True, False);
  CheckList(['Foo'], fStrings, 'Test 6.1.1');
  CheckEquals(1, Count, 'Test 6.1.2');
  fStrings.Clear;
  Count := StrExplode(Str, '||', fStrings, False, False);
  CheckList(['Foo'], fStrings, 'Test 6.2.1');
  CheckEquals(1, Count, 'Test 6.2.2');
  fStrings.Clear;
  Count := StrExplode(Str, '||', fStrings, True, True);
  CheckList(['Foo'], fStrings, 'Test 6.3.1');
  CheckEquals(1, Count, 'Test 6.3.2');
  fStrings.Clear;
  Count := StrExplode(Str, '||', fStrings, False, True);
  CheckList(['Foo'], fStrings, 'Test 6.4.1');
  CheckEquals(1, Count, 'Test 6.4.2');

  Str := ' ';
  fStrings.Clear;
  Count := StrExplode(Str, '||', fStrings, True, False);
  CheckList([' '], fStrings, 'Test 7.1.1');
  CheckEquals(1, Count, 'Test 7.1.2');
  fStrings.Clear;
  Count := StrExplode(Str, '||', fStrings, False, False);
  CheckList([' '], fStrings, 'Test 7.2.1');
  CheckEquals(1, Count, 'Test 7.2.2');
  fStrings.Clear;
  Count := StrExplode(Str, '||', fStrings, True, True);
  CheckList([''], fStrings, 'Test 7.3.1');
  CheckEquals(1, Count, 'Test 7.3.2');
  fStrings.Clear;
  Count := StrExplode(Str, '||', fStrings, False, True);
  CheckList([], fStrings, 'Test 7.4.1');
  CheckEquals(0, Count, 'Test 7.4.2');

  Str := 'Foo'#9'Bar';
  fStrings.Clear;
  Count := StrExplode(Str, #9, fStrings, True, False);
  CheckList(['Foo', 'Bar'], fStrings, 'Test 8.1');
  CheckEquals(2, Count, 'Test 8.2');

  // check that string list cleared before assigning
  Str := 'Foo'#9'Bar';
  fStrings.Clear;
  fStrings.Add('Test');
  StrExplode(Str, #9, fStrings, True, False);
  CheckList(['Foo', 'Bar'], fStrings, 'Test 9');
end;

procedure TTestRoutines.TestStrIsDelimiter;
const
  Delims = ':;,.';
  Str = 'ab:;cd,.ef';
begin
  CheckFalse(StrIsDelimiter('', '', 1), 'Test 1');
  CheckFalse(StrIsDelimiter('', '', 0), 'Test 2');
  CheckFalse(StrIsDelimiter(Delims, Str, 1), 'Test 3');
  CheckFalse(StrIsDelimiter(Delims, Str, 2), 'Test 4');
  CheckTrue(StrIsDelimiter(Delims, Str, 3), 'Test 5');
  CheckTrue(StrIsDelimiter(Delims, Str, 4), 'Test 6');
  CheckFalse(StrIsDelimiter(Delims, Str, 5), 'Test 7');
  CheckFalse(StrIsDelimiter(Delims, Str, 6), 'Test 8');
  CheckTrue(StrIsDelimiter(Delims, Str, 7), 'Test 9');
  CheckTrue(StrIsDelimiter(Delims, Str, 8), 'Test 10');
  CheckFalse(StrIsDelimiter(Delims, Str, 9), 'Test 11');
  CheckFalse(StrIsDelimiter(Delims, Str, 10), 'Test 12');
  CheckFalse(StrIsDelimiter(Delims, Str, 11), 'Test 13');
  CheckFalse(StrIsDelimiter('', 'A', 1), 'Test 14');
  CheckTrue(StrIsDelimiter(Delims + Delims, Str, 8), 'Test 15');
end;

procedure TTestRoutines.TestStrJoin;

  procedure SetStrings(const A: array of string);
  var
    S: string;
  begin
    fStrings.Clear;
    for S in A do
      fStrings.Add(S);
  end;

begin
  SetStrings(['The', 'quick', 'brown', 'fox']);
  CheckEquals('The||quick||brown||fox', StrJoin(fStrings, '||', True),
    'Test 1a');
  CheckEquals('The||quick||brown||fox', StrJoin(fStrings, '||', False),
    'Test 1b');

  SetStrings(['The', 'quick', '', 'brown', 'fox']);
  CheckEquals('The||quick||||brown||fox', StrJoin(fStrings, '||', True),
    'Test 2a');
  CheckEquals('The||quick||brown||fox', StrJoin(fStrings, '||', False),
    'Test 2b');

  SetStrings(['', 'The', 'quick', '', '', 'brown', '', 'fox', '']);
  CheckEquals('||The||quick||||||brown||||fox||', StrJoin(fStrings, '||', True),
    'Test 3a');
  CheckEquals('The||quick||brown||fox', StrJoin(fStrings, '||', False),
    'Test 3b');

  SetStrings(['', '', '']);
  CheckEquals('||||', StrJoin(fStrings, '||', True), 'Test 4a');
  CheckEquals('', StrJoin(fStrings, '||', False), 'Test 4b');

  SetStrings(['Foo']);
  CheckEquals('Foo', StrJoin(fStrings, '||', True), 'Test 5a');
  CheckEquals('Foo', StrJoin(fStrings, '||', False), 'Test 5b');

  SetStrings(['']);
  CheckEquals('', StrJoin(fStrings, '||', True), 'Test 6a');
  CheckEquals('', StrJoin(fStrings, '||', False), 'Test 6b');

  SetStrings([]);
  CheckEquals('', StrJoin(fStrings, '||', True), 'Test 7a');
  CheckEquals('', StrJoin(fStrings, '||', False), 'Test 7b');

  SetStrings(['Foo', 'Bar']);
  CheckEquals('Foo'#9'Bar', StrJoin(fStrings, #9, True), 'Test 8a');
  CheckEquals('Foo'#9'Bar', StrJoin(fStrings, #9, False), 'Test 8a');

  // check default parameter value for AllowEmpty is True
  SetStrings(['Foo', '', 'Bar']);
  CheckEquals(StrJoin(fStrings, ' '), StrJoin(fStrings, ' ', True), 'Test 9a');
  CheckNotEquals(StrJoin(fStrings, ' '), StrJoin(fStrings, ' ', False),
    'Test 9b');
end;

procedure TTestRoutines.TestStrLastDelimiterPos;
const
  Str = 'foo,bar;42,56.';
begin
  CheckEquals(0, StrLastDelimiterPos('', ''), 'Test 1');
  CheckEquals(0, StrLastDelimiterPos(',;.', ''), 'Test 2');
  CheckEquals(0, StrLastDelimiterPos('', Str), 'Test 3');
  CheckEquals(14, StrLastDelimiterPos(',;.', Str), 'Test 4');
  CheckEquals(11, StrLastDelimiterPos(',;', Str), 'Test 5');
  CheckEquals(8, StrLastDelimiterPos(';', Str), 'Test 6');
  CheckEquals(11, StrLastDelimiterPos(',', Str), 'Test 7');
  CheckEquals(14, StrLastDelimiterPos('.', Str), 'Test 8');
  CheckEquals(0, StrLastDelimiterPos('?', Str), 'Test 9');
  CheckEquals(11, StrLastDelimiterPos(',;;,', Str), 'Test 10');
end;

procedure TTestRoutines.TestStrLastPos;
begin
  CheckEquals(0, StrLastPos('Fo', 'Bar'), 'Test 1');
  CheckEquals(5, StrLastPos('a', 'FooBar'), 'Test 2');
  CheckEquals(3, StrLastPos('o', 'FooBar'), 'Test 3');
  CheckEquals(0, StrLastPos('O', 'FooBar'), 'Test 4');
  CheckEquals(19, StrLastPos('øn', 'Iñtërnâtiônàlizætiøn'), 'Test 5');
  CheckEquals(10, StrLastPos('ôn', 'Iñtërnâtiônàlizætiøn'), 'Test 6');
  CheckEquals(0, StrLastPos('', 'Foo'), 'Test 7');
  CheckEquals(0, StrLastPos('Foo', ''), 'Test 8');
end;

procedure TTestRoutines.TestStrMakeSentence;
begin
  CheckEquals('', StrMakeSentence(''), 'Test 1');
  CheckEquals('.', StrMakeSentence('.'), 'Test 2');
  CheckEquals('!', StrMakeSentence('!'), 'Test 3');
  CheckEquals('?', StrMakeSentence('?'), 'Test 4');
  CheckEquals('&.', StrMakeSentence('&'), 'Test 5');
  CheckEquals('Foo.', StrMakeSentence('Foo'), 'Test 6');
  CheckEquals('Foo.', StrMakeSentence('Foo.'), 'Test 7');
  CheckEquals('Foo!', StrMakeSentence('Foo!'), 'Test 8');
  CheckEquals('Foo?', StrMakeSentence('Foo?'), 'Test 9');
  CheckEquals('Foo%.', StrMakeSentence('Foo%'), 'Test 10');
  CheckEquals(' ', StrMakeSentence(' '), 'Test 11');
  CheckEquals('Foo. ', StrMakeSentence('Foo '), 'Test 12');
  CheckEquals('Foo. ', StrMakeSentence('Foo. '), 'Test 13');
  CheckEquals('Foo! ', StrMakeSentence('Foo! '), 'Test 14');
  CheckEquals('Foo? ', StrMakeSentence('Foo? '), 'Test 15');
  CheckEquals(' Foo. ', StrMakeSentence(' Foo '), 'Test 16');
  CheckEquals(' Foo. ', StrMakeSentence(' Foo. '), 'Test 17');
  CheckEquals(' Foo! ', StrMakeSentence(' Foo! '), 'Test 18');
  CheckEquals(' Foo? ', StrMakeSentence(' Foo? '), 'Test 19');
  CheckEquals('Foo.'#13#10, StrMakeSentence('Foo'#13#10), 'Test 20');
  CheckEquals('Foo.'#13#10, StrMakeSentence('Foo.'#13#10), 'Test 21');
  CheckEquals('Foo!'#13#10, StrMakeSentence('Foo!'#13#10), 'Test 22');
  CheckEquals('Foo?'#13#10, StrMakeSentence('Foo?'#13#10), 'Test 23');
end;

procedure TTestRoutines.TestStrPos_overload1;
begin
  CheckEquals(0, StrPos('Fo', 'Bar'), 'Test 1');
  CheckEquals(1, StrPos('Ba', 'Bar'), 'Test 2');
  CheckEquals(2, StrPos('a', 'Bar'), 'Test 3');
  CheckEquals(0, StrPos('A', 'Bar'), 'Test 4');
  CheckEquals(6, StrPos('nâtiôn', 'Iñtërnâtiônàlizætiøn'), 'Test 5');
  CheckEquals(0, StrPos('nÂtiôn', 'Iñtërnâtiônàlizætiøn'), 'Test 6');
  CheckEquals(0, StrPos('', 'Foo'), 'Test 7');
  CheckEquals(0, StrPos('Foo', ''), 'Test 8');
end;

procedure TTestRoutines.TestStrPos_overload2;
begin
  CheckEquals(0, StrPos('Fo', 'Bar', 1), 'Test 1');
  CheckEquals(0, StrPos('Fo', 'Bar', 3), 'Test 2');
  CheckEquals(1, StrPos('Foo', 'FooBar', 1), 'Test 3');
  CheckEquals(0, StrPos('Foo', 'FooBar', 2), 'Test 4');
  CheckEquals(4, StrPos('Bar', 'FooBar', 1), 'Test 5');
  CheckEquals(4, StrPos('Bar', 'FooBar', 4), 'Test 6');
  CheckEquals(0, StrPos('Bar', 'FooBar', 5), 'Test 7');
  CheckEquals(2, StrPos('o', 'FooBar', 2), 'Test 8');
  CheckEquals(3, StrPos('o', 'FooBar', 3), 'Test 9');
  CheckEquals(0, StrPos('R', 'Bar', 2), 'Test 10');
  CheckEquals(6, StrPos('n', 'Iñtërnâtiônàlizætiøn', 1), 'Test 11');
  CheckEquals(11, StrPos('n', 'Iñtërnâtiônàlizætiøn', 7), 'Test 12');
  CheckEquals(0, StrPos('', 'Foo', 3), 'Test 13');
  CheckEquals(0, StrPos('Foo', '', 1), 'Test 14');
end;

procedure TTestRoutines.TestStrQuoteSpaced;
begin
  CheckEquals('', StrQuoteSpaced(''), 'Test 1');
  CheckEquals('', StrQuoteSpaced('', ''''), 'Test 2');
  CheckEquals('" "', StrQuoteSpaced(' '), 'Test 3');
  CheckEquals(':  :', StrQuoteSpaced('  ', ':'), 'Test 4');
  CheckEquals('Foo', StrQuoteSpaced('Foo', '"'), 'Test 5');
  CheckEquals('" Foo"', StrQuoteSpaced(' Foo', '"'), 'Test 6');
  CheckEquals('"Foo "', StrQuoteSpaced('Foo ', '"'), 'Test 7');
  CheckEquals(''' Foo ''', StrQuoteSpaced(' Foo ', ''''), 'Test 8');
  CheckEquals('|Foo Bar|', StrQuoteSpaced('Foo Bar', '|'), 'Test 9');
  CheckEquals('"Foo'#9'Bar"', StrQuoteSpaced('Foo'#9'Bar'), 'Test 10');
end;

procedure TTestRoutines.TestStrReplace;
begin
  CheckEquals('', StrReplace('', '', 'yyy'), 'Test 1');
  CheckEquals('', StrReplace('', 'xxx', 'yyy'), 'Test 2');
  CheckEquals('IñtërnYYYlizætiøn',
    StrReplace('Iñtërnâtiônàlizætiøn', 'âtiônà', 'YYY'), 'Test 3');
  CheckEquals('Iñtërnâtiônàlizætiøn',
    StrReplace('Iñtërnâtiônàlizætiøn', 'Âtiônà', 'YYY'), 'Test 4');
  CheckEquals('Iñtërnlizætiøn',
    StrReplace('Iñtërnâtiônàlizætiøn', 'âtiônà', ''), 'Test 5');
  CheckEquals('Iñtër©âtiô©àlizætiø©',
    StrReplace('Iñtërnâtiônàlizætiøn', 'n', '©'), 'Test 6');
  CheckEquals('bar', StrReplace('foo', 'foo', 'bar'), 'Test 7');
end;

procedure TTestRoutines.TestStrSameStr;
begin
  CheckTrue(StrSameStr('', ''), 'Test 1');
  CheckFalse(StrSameStr('', 'Foo'), 'Test 2');
  CheckFalse(StrSameStr('Foo', ''));
  CheckTrue(StrSameStr('Iñtërnâtiônàlizætiøn', 'Iñtërnâtiônàlizætiøn'),
    'Test 3');
  CheckFalse(StrSameStr('Iñtërn', 'IÑTËRN'), 'Test 4');
  CheckFalse(StrSameStr('Ba', 'Bar'), 'Test 5');
  CheckFalse(StrSameStr('Foo', 'foo'), 'Test 6');
  CheckFalse(StrSameStr('+=¶', '+¶='), 'Test 7');
  CheckTrue(StrSameStr('+=¶', '+=¶'), 'Test 8');
end;

procedure TTestRoutines.TestStrSameText;
begin
  CheckTrue(StrSameText('', ''), 'Test 1');
  CheckFalse(StrSameText('', 'Foo'), 'Test 2');
  CheckFalse(StrSameText('Foo', ''), 'Test 3');
  CheckTrue(StrSameText('Iñtërnâtiônàlizætiøn', 'Iñtërnâtiônàlizætiøn'),
    'Test 4');
  CheckTrue(StrSameText('Iñtërn', 'IÑTËRN'), 'Test 5');
  CheckFalse(StrSameText('Ba', 'Bar'), 'Test 6');
  CheckTrue(StrSameText('Foo', 'foo'), 'Test 7');
  CheckFalse(StrSameText('+=¶', '+¶='), 'Test 8');
  CheckTrue(StrSameText('+=¶', '+=¶'), 'Test 9');
end;

procedure TTestRoutines.TestStrSlice;
begin
  CheckEquals('', StrSlice('', 5, 8), 'Test 1');
  CheckEquals('Iñtërn', StrSlice('Iñtërnâtiônàlizætiøn', 1, 6), 'Test 2');
  CheckEquals('tërnât', StrSlice('Iñtërnâtiônàlizætiøn', 3, 6), 'Test 3');
  CheckEquals('nât', StrSlice('Iñtërnâtiônàlizætiøn', 6, 3), 'Test 4');
  CheckEquals('tërnâtiônàlizætiøn', StrSlice('Iñtërnâtiônàlizætiøn', 3, 40),
    'Test 5');
  CheckEquals('tërnâtiônàlizætiøn', StrSlice('Iñtërnâtiônàlizætiøn', 3, 18),
    'Test 6');
  CheckEquals('tërnâtiônàlizætiø', StrSlice('Iñtërnâtiônàlizætiøn', 3, 17),
    'Test 7');
  CheckEquals('', StrSlice('Iñtërnâtiônàlizætiøn', 40, 2), 'Test 8');
  CheckEquals('', StrSlice('Iñtërnâtiônàlizætiøn', 3, 0), 'Test 9');
  CheckEquals('', StrSlice('Iñtërnâtiônàlizætiøn', 3, -1), 'Test 10');
  CheckEquals('Iñt', StrSlice('Iñtërnâtiônàlizætiøn', 0, 3), 'Test 11');
  CheckEquals('Iñt', StrSlice('Iñtërnâtiônàlizætiøn', -4, 3), 'Test 12');
end;

procedure TTestRoutines.TestStrSliceLeft;
begin
  CheckEquals('', StrSliceLeft('', 5), 'Test 1');
  CheckEquals('', StrSliceLeft('Iñtërnâtiônàlizætiøn', 0), 'Test 2');
  CheckEquals('', StrSliceLeft('Iñtërnâtiônàlizætiøn', -1), 'Test 3');
  CheckEquals('Iñtërn', StrSliceLeft('Iñtërnâtiônàlizætiøn', 6), 'Test 4');
  CheckEquals('Iñtërnâtiônàlizætiøn', StrSliceLeft('Iñtërnâtiônàlizætiøn', 20),
    'Test 5');
  CheckEquals('Iñtërnâtiônàlizætiøn', StrSliceLeft('Iñtërnâtiônàlizætiøn', 40),
    'Test 6');
  CheckEquals('Iñtërnâtiônàlizætiø', StrSliceLeft('Iñtërnâtiônàlizætiøn', 19),
    'Test 7');
end;

procedure TTestRoutines.TestStrSliceRight;
begin
  CheckEquals('', StrSliceRight('', 5), 'Test 1');
  CheckEquals('', StrSliceRight('Iñtërnâtiônàlizætiøn', 0), 'Test 2');
  CheckEquals('', StrSliceRight('Iñtërnâtiônàlizætiøn', -1), 'Test 3');
  CheckEquals('zætiøn', StrSliceRight('Iñtërnâtiônàlizætiøn', 6), 'Test 4');
  CheckEquals('Iñtërnâtiônàlizætiøn', StrSliceRight('Iñtërnâtiônàlizætiøn', 20),
    'Test 5');
  CheckEquals('Iñtërnâtiônàlizætiøn', StrSliceRight('Iñtërnâtiônàlizætiøn', 40),
    'Test 6');
  CheckEquals('ñtërnâtiônàlizætiøn', StrSliceRight('Iñtërnâtiônàlizætiøn', 19),
    'Test 7');
end;

procedure TTestRoutines.TestStrSplit;
var
  Left, Right: string;
begin
  CheckTrue(StrSplit('Foo||Bar', '||', Left, Right), 'Test 1a');
  CheckEquals('Foo', Left, 'Test 1b');
  CheckEquals('Bar', Right, 'Test 1c');

  CheckTrue(StrSplit('Foo||', '||', Left, Right), 'Test 2a');
  CheckEquals('Foo', Left, 'Test 2b');
  CheckEquals('', Right, 'Test 2c');

  CheckTrue(StrSplit('||Foo', '||', Left, Right), 'Test 3a');
  CheckEquals('', Left, 'Test 3b');
  CheckEquals('Foo', Right, 'Test 3c');

  CheckTrue(StrSplit('||', '||', Left, Right), 'Test 4a');
  CheckEquals('', Left, 'Test 4b');
  CheckEquals('', Right, 'Test 4c');

  CheckFalse(StrSplit('Foo', '||', Left, Right), 'Test 5a');
  CheckEquals('Foo', Left, 'Test 5b');
  CheckEquals('', Right, 'Test 5c');

  CheckFalse(StrSplit('', '', Left, Right), 'Test 6a');
  CheckEquals('', Left, 'Test 6b');
  CheckEquals('', Right, 'Test 6c');

  CheckFalse(StrSplit('Foo', '', Left, Right), 'Test 7a');
  CheckEquals('Foo', Left, 'Test 7b');
  CheckEquals('', Right, 'Test 7c');

  CheckTrue(StrSplit('Foo'#9'Bar', #9, Left, Right), 'Test 8a');
  CheckEquals('Foo', Left, 'Test 8b');
  CheckEquals('Bar', Right, 'Test 8c');

  CheckTrue(StrSplit('Foo||Bar||42', '||', Left, Right), 'Test 9a');
  CheckEquals('Foo', Left, 'Test 9b');
  CheckEquals('Bar||42', Right, 'Test 9c');

  CheckFalse(StrSplit('', '||', Left, Right), 'Test 10a');
  CheckEquals('', Left, 'Test 10b');
  CheckEquals('', Right, 'Test 10c');
end;

procedure TTestRoutines.TestStrStartsStr;
begin
  CheckFalse(StrStartsStr('Iñtërnâtiônàlizætiøn', ''), 'Test 1');
  CheckFalse(StrStartsStr('', 'Iñtërnâtiônàlizætiøn'), 'Test 2');
  CheckFalse(StrStartsStr('', ''), 'Test 3');
  CheckTrue(StrStartsStr('Iñtërn', 'Iñtërnâtiônàlizætiøn'), 'Test 4');
  CheckFalse(StrStartsStr('iñtërn', 'Iñtërnâtiônàlizætiøn'), 'Test 5');
  CheckFalse(StrStartsStr('IÑTËRN', 'Iñtërnâtiônàlizætiøn'), 'Test 6');
  CheckTrue(StrStartsStr('Iñtërnâtiônàlizætiøn', 'Iñtërnâtiônàlizætiøn'),
    'Test 7');
  CheckFalse(StrStartsStr('ñtërnâ', 'Iñtërnâtiônàlizætiøn'),
    'Test 8');
end;

procedure TTestRoutines.TestStrStartsText;
begin
  CheckFalse(StrStartsText('Iñtërnâtiônàlizætiøn', ''), 'Test 1');
  CheckFalse(StrStartsText('', 'Iñtërnâtiônàlizætiøn'), 'Test 2');
  CheckFalse(StrStartsText('', ''), 'Test 3');
  CheckTrue(StrStartsText('Iñtërn', 'Iñtërnâtiônàlizætiøn'), 'Test 4');
  CheckTrue(StrStartsText('iñtërn', 'Iñtërnâtiônàlizætiøn'), 'Test 5');
  CheckTrue(StrStartsText('IÑTËRN', 'Iñtërnâtiônàlizætiøn'), 'Test 6');
  CheckTrue(StrStartsText('Iñtërnâtiônàlizætiøn', 'Iñtërnâtiônàlizætiøn'),
    'Test 7');
  CheckFalse(StrStartsText('ñtërnâ', 'Iñtërnâtiônàlizætiøn'), 'Test 8');
end;

procedure TTestRoutines.TestStrStripWhiteSpace;
begin
  CheckEquals('', StrStripWhiteSpace(''), 'Test 1');
  CheckEquals('', StrStripWhiteSpace(' '), 'Test 2');
  CheckEquals('', StrStripWhiteSpace(#13#10' '#9#11), 'Test 3');
  CheckEquals('FooBar', StrStripWhiteSpace('Foo Bar'), 'Test 4');
  CheckEquals('FooBar', StrStripWhiteSpace(' Foo Bar '), 'Test 5');
  CheckEquals('FooBar', StrStripWhiteSpace('FooBar'), 'Test 6');
  CheckEquals('FooBar', StrStripWhiteSpace('Foo'#13#10'Bar'), 'Test 7');
  CheckEquals('FooBar', StrStripWhiteSpace(#12'Foo'#9#9#9'Bar'#13#10),
    'Test 8');
end;

procedure TTestRoutines.TestStrToLower;
begin
  CheckEquals('', StrToLower(''), 'Test 1');
  CheckEquals('356', StrToLower('356'), 'Test 2');
  CheckEquals('foo', StrToLower('foo'), 'Test 3');
  CheckEquals('foo', StrToLower('FOO'), 'Test 4');
  CheckEquals('foo', StrToLower('Foo'), 'Test 5');
  CheckEquals('iñtërn', StrToLower('IÑTËRN'), 'Test 6');
  CheckEquals('iñtërn', StrToLower('Iñtërn'), 'Test 7');
  CheckEquals('iñtërn', StrToLower('iñtërn'), 'Test 8');
  CheckEquals('+=¶', StrToLower('+=¶'), 'Test 9');
end;

procedure TTestRoutines.TestStrToUpper;
begin
  CheckEquals('', StrToUpper(''), 'Test 1');
  CheckEquals('356', StrToUpper('356'), 'Test 2');
  CheckEquals('FOO', StrToUpper('foo'), 'Test 3');
  CheckEquals('FOO', StrToUpper('FOO'), 'Test 4');
  CheckEquals('FOO', StrToUpper('Foo'), 'Test 5');
  CheckEquals('IÑTËRN', StrToUpper('IÑTËRN'), 'Test 6');
  CheckEquals('IÑTËRN', StrToUpper('Iñtërn'), 'Test 7');
  CheckEquals('IÑTËRN', StrToUpper('iñtërn'), 'Test 8');
  CheckEquals('+=¶', StrToUpper('+=¶'), 'Test 9');
end;

procedure TTestRoutines.TestStrTrim;
begin
  CheckEquals('Foo', StrTrim('Foo'), 'Test 1');
  CheckEquals('Foo', StrTrim('  Foo'), 'Test 2');
  CheckEquals('Foo', StrTrim('Foo    '), 'Test 3');
  CheckEquals('Foo', StrTrim(' Foo   '), 'Test 4');
  CheckEquals('- Foo -', StrTrim(' - Foo - '), 'Test 5');
  CheckEquals('', StrTrim('    '), 'Test 6');
  CheckEquals('', StrTrim(''), 'Test 7');
  CheckEquals('Foo', StrTrim('  '#13#10' Foo'#13#10'    '#9), 'Test 8');
  // test multi-byte Unicode character (MUSICAL SYMBOL G CLEF)
  CheckEquals(#$D834#$DD1E, StrTrim('  '#$D834#$DD1E'  '), 'Test 9');
end;

procedure TTestRoutines.TestStrTrimChars;
begin
  CheckEquals('', StrTrimChars('', 'X'), 'Test 1');
  CheckEquals('Foo', StrTrimChars('XXFooX', 'X'), 'Test 2');
  CheckEquals('xxFoox', StrTrimChars('xxFoox', 'X'), 'Test 3');
  CheckEquals(' Foo  ', StrTrimChars('X Foo  XX', 'X'), 'Test 4');
  CheckEquals(' XFooXX ', StrTrimChars(' XFooXX ', 'X'), 'Test 5');
  CheckEquals('Foo', StrTrimChars('XFoo', 'X'), 'Test 6');
  CheckEquals('Foo', StrTrimChars('FooXX', 'X'), 'Test 7');
  CheckEquals('Foo', StrTrimChars('Foo', 'X'), 'Test 8');
  CheckEquals('F', StrTrimChars('Foo', 'o'), 'Test 9');
  CheckEquals('oo', StrTrimChars('Foo', 'F'), 'Test 10');
  // test multi-byte Unicode character (MUSICAL SYMBOL G CLEF)
  CheckEquals('#$D834#$DD1E', StrTrimChars('XX#$D834#$DD1EXX', 'X'), 'Test 11');
end;

procedure TTestRoutines.TestStrTrimLeft;
begin
  CheckEquals('Foo', StrTrimLeft('Foo'), 'Test 1');
  CheckEquals('Foo', StrTrimLeft('  Foo'), 'Test 2');
  CheckEquals('Foo    ', StrTrimLeft('Foo    '), 'Test 3');
  CheckEquals('Foo   ', StrTrimLeft(' Foo   '), 'Test 4');
  CheckEquals('- Foo - ', StrTrimLeft(' - Foo - '), 'Test 5');
  CheckEquals('', StrTrimLeft('    '), 'Test 6');
  CheckEquals('', StrTrimLeft(''), 'Test 7');
  CheckEquals('Foo'#13#10'   '#9,
    StrTrimLeft('  '#13#10' Foo'#13#10'   '#9), 'Test 8');
  // test multi-byte Unicode character (MUSICAL SYMBOL G CLEF)
  CheckEquals(#$D834#$DD1E'  ', StrTrimLeft('  '#$D834#$DD1E'  '), 'Test 9');
end;

procedure TTestRoutines.TestStrTrimLeftChars;
begin
  CheckEquals('', StrTrimLeftChars('', 'X'), 'Test 1');
  CheckEquals('FooX', StrTrimLeftChars('XXFooX', 'X'), 'Test 2');
  CheckEquals('xxFoox', StrTrimLeftChars('xxFoox', 'X'), 'Test 3');
  CheckEquals(' Foo  XX', StrTrimLeftChars('X Foo  XX', 'X'), 'Test 4');
  CheckEquals(' XFooXX ', StrTrimLeftChars(' XFooXX ', 'X'), 'Test 5');
  CheckEquals('Foo', StrTrimLeftChars('XFoo', 'X'), 'Test 6');
  CheckEquals('FooXX', StrTrimLeftChars('FooXX', 'X'), 'Test 7');
  CheckEquals('Foo', StrTrimLeftChars('Foo', 'X'), 'Test 8');
  CheckEquals('Foo', StrTrimLeftChars('Foo', 'o'), 'Test 9');
  CheckEquals('oo', StrTrimLeftChars('Foo', 'F'), 'Test 10');
  // test multi-byte Unicode character (MUSICAL SYMBOL G CLEF)
  CheckEquals('#$D834#$DD1EXX', StrTrimLeftChars('XX#$D834#$DD1EXX', 'X'),
    'Test 11');
end;

procedure TTestRoutines.TestStrTrimRight;
begin
  CheckEquals('Foo', StrTrimRight('Foo'), 'Test 1');
  CheckEquals('  Foo', StrTrimRight('  Foo'), 'Test 2');
  CheckEquals('Foo', StrTrimRight('Foo    '), 'Test 3');
  CheckEquals(' Foo', StrTrimRight(' Foo   '), 'Test 4');
  CheckEquals(' - Foo -', StrTrimRight(' - Foo - '), 'Test 5');
  CheckEquals('', StrTrimRight('    '), 'Test 6');
  CheckEquals('', StrTrimRight(''), 'Test 7');
  CheckEquals('  '#13#10' Foo',
    StrTrimRight('  '#13#10' Foo'#13#10'   '#9), 'Test 8');
  // test multi-byte Unicode character (MUSICAL SYMBOL G CLEF)
  CheckEquals('  '#$D834#$DD1E, StrTrimRight('  '#$D834#$DD1E'  '), 'Test 9');
end;

procedure TTestRoutines.TestStrTrimRightChars;
begin
  CheckEquals('', StrTrimRightChars('', 'X'), 'Test 1');
  CheckEquals('XXFoo', StrTrimRightChars('XXFooX', 'X'), 'Test 2');
  CheckEquals('xxFoox', StrTrimRightChars('xxFoox', 'X'), 'Test 3');
  CheckEquals('X Foo  ', StrTrimRightChars('X Foo  XX', 'X'), 'Test 4');
  CheckEquals(' XFooXX ', StrTrimRightChars(' XFooXX ', 'X'), 'Test 5');
  CheckEquals('XFoo', StrTrimRightChars('XFoo', 'X'), 'Test 6');
  CheckEquals('Foo', StrTrimRightChars('FooXX', 'X'), 'Test 7');
  CheckEquals('Foo', StrTrimRightChars('Foo', 'X'), 'Test 8');
  CheckEquals('F', StrTrimRightChars('Foo', 'o'), 'Test 9');
  CheckEquals('Foo', StrTrimRightChars('Foo', 'F'), 'Test 10');
  // test multi-byte Unicode character (MUSICAL SYMBOL G CLEF)
  CheckEquals('XX#$D834#$DD1E', StrTrimRightChars('XX#$D834#$DD1EXX', 'X'),
    'Test 11');
end;

procedure TTestRoutines.TestStrUnixLineBreaks;
begin
  CheckEquals('', StrUnixLineBreaks(''), 'Test 1');
  CheckEquals('xxx', StrUnixLineBreaks('xxx'), 'Test 2');
  CheckEquals('aaa'#10'bbb', StrUnixLineBreaks('aaa'#10'bbb'), 'Test 3');
  CheckEquals('aaa'#10'bbb', StrUnixLineBreaks('aaa'#13'bbb'), 'Test 4');
  CheckEquals('aaa'#10'bbb', StrUnixLineBreaks('aaa'#13#10'bbb'), 'Test 5');
  CheckEquals('aaa'#10'bbb'#10'ccc'#10'ddd',
    StrUnixLineBreaks('aaa'#13#10'bbb'#13'ccc'#10'ddd'), 'Test 6');
  CheckEquals(#10#10#10, StrUnixLineBreaks(#13#10#10#13), 'Test 7');
  CheckEquals(#10#10#10, StrUnixLineBreaks(#10#13#13#10), 'Test 8');
end;

procedure TTestRoutines.TestStrWindowsLineBreaks;
begin
  CheckEquals('', StrWindowsLineBreaks(''), 'Test 1');
  CheckEquals('xxx', StrWindowsLineBreaks('xxx'), 'Test 2');
  CheckEquals('aaa'#13#10'bbb', StrWindowsLineBreaks('aaa'#10'bbb'), 'Test 3');
  CheckEquals('aaa'#13#10'bbb', StrWindowsLineBreaks('aaa'#13'bbb'), 'Test 4');
  CheckEquals('aaa'#13#10'bbb', StrWindowsLineBreaks('aaa'#13#10'bbb'),
    'Test 5');
  CheckEquals('aaa'#13#10'bbb'#13#10'ccc'#13#10'ddd',
    StrWindowsLineBreaks('aaa'#13#10'bbb'#13'ccc'#10'ddd'), 'Test 6');
  CheckEquals(#13#10#13#10#13#10, StrWindowsLineBreaks(#13#10#10#13), 'Test 7');
  CheckEquals(#13#10#13#10#13#10, StrWindowsLineBreaks(#10#13#13#10), 'Test 8');
end;

procedure TTestRoutines.TestStrWrap;
const
  Text = 'The quick brown fox jumped-over-the lazy dog.';
  //      123456789012345678901234567890123456789012345
  //               1         2         3         4
  ResA = 'The quick' + EOL
       + 'brown fox' + EOL
       + 'jumped-over-the' + EOL
       + 'lazy dog.';
  ResB = '  The quick' + EOL
       + '  brown fox' + EOL
       + '  jumped-over-the' + EOL
       + '  lazy dog.';
  ResC = 'The quick brown' + EOL
       + 'fox' + EOL
       + 'jumped-over-the' + EOL
       + 'lazy dog.';
  ResD = 'The' + EOL
       + 'quick' + EOL
       + 'brown' + EOL
       + 'fox' + EOL
       + 'jumped-over-the' + EOL
       + 'lazy' + EOL
       + 'dog.';
begin
  CheckEquals('', StrWrap('', 10, 0), 'Test 1');
  CheckEquals('', StrWrap('', 10, 4), 'Test 2');
  CheckEquals('X', StrWrap('X', 10, 0), 'Test 3');
  CheckEquals('    X', StrWrap('X', 10, 4), 'Test 4');
  CheckEquals(ResA, StrWrap(Text, 10, 0), 'Test 5');
  CheckEquals(ResB, StrWrap(Text, 10, 2), 'Test 6');
  CheckEquals(ResC, StrWrap(Text, 15, 0), 'Test 7');
  CheckEquals(ResC, StrWrap(Text, 15, -2), 'Test 8');
  CheckEquals(ResD, StrWrap(Text, 1, 0), 'Test 9');
  CheckEquals(ResD, StrWrap(Text, 0, 0), 'Test 10');
  CheckEquals(ResD, StrWrap(Text, -1, 0), 'Test 11');
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TTestRoutines.Suite);
end.

