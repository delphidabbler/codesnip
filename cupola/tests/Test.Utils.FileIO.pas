unit Test.Utils.FileIO;

interface

uses
  DUnitX.TestFramework,

  System.SysUtils,
  System.Classes,

  CSLE.Utils.FileIO;

type
  [TestFixture]
  TTestFileIO = class
  strict private
    const
      TestInFilePath = '..\..\..\..\tests\data\Test.Utils.FileIO\';
      TestOutFilePath = TestInFilePath + '~output\';
      EmptyFile = 'empty.txt';
      EmptyUTF8BOMFile = 'empty-utf8-with-bom.txt';
      ASCIIFile = 'ascii.txt';
      UTF8BOMFile = 'utf8-with-bom.txt';
      UTF8NoBOMFile = 'utf8-without-bom.txt';
      UTF16BEBOMFile = 'utf16BE-with-bom.txt';
      UTF16LEBOMFile = 'utf16LE-with-bom.txt';
      BadFile = '~~missing-dir~~\bad';

      OutFile = 'out';

      FileBytes: TBytes = [0,1,2,3,4,5,6,7,8,9,42,56,$FF];

      ASCIIFileContent =
        '''
        The cat
        sat on
        the mat

        ''';

      UnicodeFileContent =
        '''
        ϰightly
        ⅔rd of everything
        The quick brown fox jumped
        over the lazy dog

        ''';

    function InFilePath(const AFileName: string): string;
    function OutFilePath(const AFileName: string): string;
    function CreateByteArrayFromText(const AText: string; const AEncoding: TEncoding;
      const AWriteBOM: Boolean): TBytes;
    function CreateStreamFromText(const AText: string; const AEncoding: TEncoding;
      const AWriteBOM: Boolean): TStream;
    function SameFiles(const LeftName, RightName: string): Boolean;
    function SameBytes(const Left, Right: TBytes): Boolean;
    function SameStrings(const Left, Right: array of string): Boolean;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure CheckBOM_TBytes_fails_assertion_if_encoding_is_nil;
    [Test]
    procedure CheckBOM_TBytes_returns_true_for_matching_UTF8_BOM;
    [Test]
    procedure CheckBOM_TBytes_returns_true_for_matching_Unicode_BOM;
    [Test]
    procedure CheckBOM_TBytes_returns_false_for_non_matching_UTF8_BOM;
    [Test]
    procedure CheckBOM_TBytes_returns_false_for_empty_byte_array;
    [Test]
    procedure CheckBOM_TBytes_returns_false_for_encoding_with_no_BOM;
    [Test]
    procedure CheckBOM_TBytes_returns_false_for_UTF8_bytes_with_no_BOM;

    [Test]
    procedure CheckBOM_TStream_fails_assertion_if_encoding_is_nil;
    [Test]
    procedure CheckBOM_TStream_returns_true_for_matching_UTF8_BOM;
    [Test]
    procedure CheckBOM_TStream_returns_true_for_matching_UnicodeBE_BOM;
    [Test]
    procedure CheckBOM_TStream_returns_false_for_non_matching_UnicodeBE_BOM;
    [Test]
    procedure CheckBOM_TStream_returns_false_for_empty_stream;
    [Test]
    procedure CheckBOM_TStream_returns_false_for_encoding_with_no_BOM;
    [Test]
    procedure CheckBOM_TStream_returns_false_for_Unicode_stream_with_no_BOM;

    [Test]
    procedure CheckBOM_file_fails_assertion_if_encoding_is_nil;
    [Test]
    procedure CheckBOM_file_returns_true_for_matching_file_with_UTF8_BOM;
    [Test]
    procedure CheckBOM_file_returns_true_for_matching_file_with_UnicodeBE_BOM;
    [Test]
    procedure CheckBOM_file_returns_true_for_matching_file_with_UnicodeLE_BOM;
    [Test]
    procedure CheckBOM_file_returns_false_for_empty_file;
    [Test]
    procedure CheckBOM_file_returns_false_for_file_with_ASCII_encoding;
    [Test]
    procedure CheckBOM_file_returns_false_for_UTF8_file_with_no_BOM;
    [Test]
    procedure CheckBOM_file_returns_false_for_UTF8_file_tested_against_UnicodeLE_BOM;

    [Test]
    procedure WriteAllBytes_creates_empty_file_for_empty_byte_array;
    [Test]
    procedure WriteAllBytes_creates_file_with_expected_content;
    [Test]
    procedure WriteAllBytes_raises_exception_for_invalid_file_name;

    [Test]
    procedure ReadAllBytes_returns_empty_array_for_empty_file;
    [Test]
    procedure ReadAllBytes_returns_expected_array_non_empty_file;
    [Test]
    procedure ReadAllBytes_raises_exception_for_invalid_file_name;

    [Test]
    procedure WriteAllText_creates_expected_Unicode_text_file_with_BOM;
    [Test]
    procedure WriteAllText_creates_expected_UTF8_text_file_with_BOM;
    [Test]
    procedure WriteAllText_creates_expected_UTF8_text_file_without_BOM;
    [Test]
    procedure WriteAllText_creates_expected_ASCII_text_file_without_BOM;
    [Test]
    procedure WriteAllText_creates_empty_text_file_without_BOM;
    [Test]
    procedure WriteAllText_creates_empty_text_file_with_UTF8_BOM;
    [Test]
    procedure WriteAllText_raises_exception_for_invalid_file_name;
    [Test]
    procedure WriteAllText_fails_assertion_if_encoding_is_nil;

    [Test]
    procedure ReadAllText_reads_expected_text_from_Unicode_text_file_with_BOM;
    [Test]
    procedure ReadAllText_reads_expected_text_from_UTF8_text_file_with_BOM;
    [Test]
    procedure ReadAllText_reads_expected_text_from_UTF8_text_file_without_BOM;
    [Test]
    procedure ReadAllText_reads_expected_text_from_ASCII_text_file_without_BOM;
    [Test]
    procedure ReadAllText_reads_empty_text_file_without_BOM;
    [Test]
    procedure ReadAllText_reads_empty_text_file_with_UTF8_BOM;
    [Test]
    procedure ReadAllText_raises_exception_for_invalid_file_name;
    [Test]
    procedure ReadAllText_raises_exception_for_mismatched_BOMs;
    [Test]
    procedure ReadAllText_fails_assertion_if_encoding_is_nil;

    [Test]
    procedure WriteAllLines_creates_expected_Unicode_text_file_with_BOM;
    [Test]
    procedure WriteAllLines_creates_expected_UTF8_text_file_with_BOM;
    [Test]
    procedure WriteAllLines_creates_expected_UTF8_text_file_without_BOM;
    [Test]
    procedure WriteAllLines_creates_expected_ASCII_text_file_without_BOM;
    [Test]
    procedure WriteAllLines_creates_empty_text_file_without_BOM;
    [Test]
    procedure WriteAllLines_creates_empty_text_file_with_UTF8_BOM;
    [Test]
    procedure WriteAllLines_raises_exception_for_invalid_file_name;
    [Test]
    procedure WriteAllLines_fails_assertion_if_encoding_is_nil;

    [Test]
    procedure ReadAllLines_reads_expected_lines_from_Unicode_text_file_with_BOM;
    [Test]
    procedure ReadAllLines_reads_expected_lines_from_UTF8_text_file_with_BOM;
    [Test]
    procedure ReadAllLines_reads_expected_line_from_UTF8_text_file_without_BOM;
    [Test]
    procedure ReadAllLines_reads_expected_lines_from_ASCII_text_file_without_BOM;
    [Test]
    procedure ReadAllLines_reads_empty_text_file_without_BOM;
    [Test]
    procedure ReadAllLines_reads_empty_text_file_with_UTF8_BOM;
    [Test]
    procedure ReadAllLines_raises_exception_for_invalid_file_name;
    [Test]
    procedure ReadAllLines_raises_exception_for_mismatched_BOMs;
    [Test]
    procedure ReadAllLines_fails_assertion_if_encoding_is_nil;

    [Test]
    procedure CopyFile_copies_file_successfully;
    [Test]
    procedure CopyFile_raises_exeception_for_missing_source_file;
    [Test]
    procedure CopyFile_raises_exeception_for_invalid_destination_file;

  end;

implementation

uses
  System.IOUtils;

procedure TTestFileIO.CheckBOM_file_fails_assertion_if_encoding_is_nil;
begin
  Assert.WillRaise(
    procedure
    begin
      TFileIO.CheckBOM(InFilePath(UTF8BOMFile), nil);
    end,
    EAssertionFailed
  );
end;

procedure TTestFileIO.CheckBOM_file_returns_false_for_empty_file;
begin
  Assert.IsFalse(TFileIO.CheckBOM(InFilePath(EmptyFile), TEncoding.UTF8));
end;

procedure TTestFileIO.CheckBOM_file_returns_false_for_file_with_ASCII_encoding;
begin
  Assert.IsFalse(TFileIO.CheckBOM(InFilePath(ASCIIFile), TEncoding.ASCII));
end;

procedure TTestFileIO.CheckBOM_file_returns_false_for_UTF8_file_tested_against_UnicodeLE_BOM;
begin
  Assert.IsFalse(TFileIO.CheckBOM(InFilePath(UTF8BOMFile), TEncoding.Unicode));
end;

procedure TTestFileIO.CheckBOM_file_returns_false_for_UTF8_file_with_no_BOM;
begin
  Assert.IsFalse(TFileIO.CheckBOM(InFilePath(UTF8NoBOMFile), TEncoding.UTF8));
end;

procedure TTestFileIO.CheckBOM_file_returns_true_for_matching_file_with_UnicodeBE_BOM;
begin
  Assert.IsTrue(TFileIO.CheckBOM(InFilePath(UTF16BEBOMFile), TEncoding.BigEndianUnicode));
end;

procedure TTestFileIO.CheckBOM_file_returns_true_for_matching_file_with_UnicodeLE_BOM;
begin
  Assert.IsTrue(TFileIO.CheckBOM(InFilePath(UTF16LEBOMFile), TEncoding.Unicode));
end;

procedure TTestFileIO.CheckBOM_file_returns_true_for_matching_file_with_UTF8_BOM;
begin
  Assert.IsTrue(TFileIO.CheckBOM(InFilePath(UTF8BOMFile), TEncoding.UTF8));
end;

procedure TTestFileIO.CheckBOM_TBytes_fails_assertion_if_encoding_is_nil;
begin
  Assert.WillRaise(
    procedure
    begin
      TFileIO.CheckBOM(TBytes.Create(1,2,3,4,5,6,7), nil);
    end,
    EAssertionFailed
  );
end;

procedure TTestFileIO.CheckBOM_TBytes_returns_false_for_empty_byte_array;
begin
  Assert.IsFalse(TFileIO.CheckBOM([], TEncoding.UTF8));
end;

procedure TTestFileIO.CheckBOM_TBytes_returns_false_for_encoding_with_no_BOM;
begin
  // check that ASCII encoding has zero length (i.e. no) BOM
  Assert.AreEqual(NativeInt(0), Length(TEncoding.ASCII.GetPreamble), 'Pre-check');
  var Bytes := CreateByteArrayFromText('Foo bar', TEncoding.ASCII, True);
  Assert.IsFalse(TFileIO.CheckBOM(Bytes, TEncoding.ASCII), 'Check');
end;

procedure TTestFileIO.CheckBOM_TBytes_returns_false_for_non_matching_UTF8_BOM;
begin
  // we set up Bytes to have a Unicode BOM, which is not same as UTF8 BOM
  var Bytes := CreateByteArrayFromText('Hellow world', TEncoding.Unicode, True);
  Assert.IsFalse(TFileIO.CheckBOM(Bytes, TEncoding.UTF8));
end;

procedure TTestFileIO.CheckBOM_TBytes_returns_false_for_UTF8_bytes_with_no_BOM;
begin
  var Bytes := CreateByteArrayFromText('hello world!', TEncoding.UTF8, False);
  Assert.IsFalse(TFileIO.CheckBOM(Bytes, TEncoding.UTF8));
end;

procedure TTestFileIO.CheckBOM_TBytes_returns_true_for_matching_Unicode_BOM;
begin
  var Bytes := CreateByteArrayFromText('Hello Alice', TEncoding.Unicode, True);
  Assert.IsTrue(TFileIO.CheckBOM(Bytes, TEncoding.Unicode));
end;

procedure TTestFileIO.CheckBOM_TBytes_returns_true_for_matching_UTF8_BOM;
begin
  var Bytes := CreateByteArrayFromText('Hello Bob', TEncoding.UTF8, True);
  Assert.IsTrue(TFileIO.CheckBOM(Bytes, TEncoding.UTF8));
end;

procedure TTestFileIO.CheckBOM_TStream_fails_assertion_if_encoding_is_nil;
begin
  Assert.WillRaise(
    procedure
    begin
      var Stream := CreateStreamFromText('Hi ho'#13#10'silver lining', TEncoding.Unicode, True);
      try
        TFileIO.CheckBOM(Stream, nil);
      finally
        Stream.Free;
      end;
    end,
    EAssertionFailed
  );
end;

procedure TTestFileIO.CheckBOM_TStream_returns_false_for_empty_stream;
begin
  var Stream := TMemoryStream.Create;
  try
    Assert.IsFalse(TFileIO.CheckBOM(Stream, TEncoding.UTF8));
  finally
    Stream.Free;
  end;
end;

procedure TTestFileIO.CheckBOM_TStream_returns_false_for_encoding_with_no_BOM;
begin
  // check that ASCII encoding has zero length (i.e. no) BOM
  Assert.AreEqual(NativeInt(0), Length(TEncoding.ASCII.GetPreamble), 'Pre-check');
  var Stream := CreateStreamFromText('Meet me in the morning', TEncoding.ASCII,False);
  try
    Assert.IsFalse(TFileIO.CheckBOM(Stream, TEncoding.ASCII), 'Check');
  finally
    Stream.Free;
  end;
end;

procedure TTestFileIO.CheckBOM_TStream_returns_false_for_non_matching_UnicodeBE_BOM;
begin
  // we set up stream to have a Unicode BOM, which is not same as Unicode BE BOM
  var Stream := CreateStreamFromText('Woof, woof', TEncoding.Unicode, True);
  try
    Assert.IsFalse(TFileIO.CheckBOM(Stream, TEncoding.BigEndianUnicode));
  finally
    Stream.Free;
  end;
end;

procedure TTestFileIO.CheckBOM_TStream_returns_false_for_Unicode_stream_with_no_BOM;
begin
  var Stream := CreateStreamFromText('I woke up this'#13#10'morning', TEncoding.Unicode, False);
  try
    Assert.IsFalse(TFileIO.CheckBOM(Stream, TEncoding.Unicode));
  finally
    Stream.Free;
  end;
end;

procedure TTestFileIO.CheckBOM_TStream_returns_true_for_matching_UnicodeBE_BOM;
begin
  var Stream := CreateStreamFromText('Foo bar baz', TEncoding.BigEndianUnicode, True);
  try
    Assert.IsTrue(TFileIO.CheckBOM(Stream, TEncoding.BigEndianUnicode));
  finally
    Stream.Free;
  end;
end;

procedure TTestFileIO.CheckBOM_TStream_returns_true_for_matching_UTF8_BOM;
begin
  var Stream := CreateStreamFromText('Foo bar baz', TEncoding.UTF8, True);
  try
    Assert.IsTrue(TFileIO.CheckBOM(Stream, TEncoding.UTF8));
  finally
    Stream.Free;
  end;
end;

procedure TTestFileIO.CopyFile_copies_file_successfully;
begin
  var SrcFile := InFilePath(UTF16LEBOMFile);
  var DestFile := OutFilePath(OutFile);
  TFileIO.CopyFile(SrcFile, DestFile);
  Assert.IsTrue(SameFiles(SrcFile, DestFile));
end;

procedure TTestFileIO.CopyFile_raises_exeception_for_invalid_destination_file;
begin
  Assert.WillRaise(
    procedure
    begin
      TFileIO.CopyFile(InFilePath(UTF16BEBOMFile), OutFilePath(BadFile));
    end,
    EFCreateError
  );
end;

procedure TTestFileIO.CopyFile_raises_exeception_for_missing_source_file;
begin
  Assert.WillRaise(
    procedure
    begin
      TFileIO.CopyFile(InFilePath(BadFile), OutFilePath(OutFile));
    end,
    EFOpenError
  );
end;

function TTestFileIO.CreateByteArrayFromText(const AText: string;
  const AEncoding: TEncoding; const AWriteBOM: Boolean): TBytes;
begin
  var Bytes := AEncoding.GetBytes(AText);
  if AWriteBOM then //and (Length(AEncoding.GetPreamble) > 0) then
    Result := Concat(AEncoding.GetPreamble, Bytes)
  else
    Result := Bytes;
end;

function TTestFileIO.CreateStreamFromText(const AText: string;
  const AEncoding: TEncoding; const AWriteBOM: Boolean): TStream;
begin
  var Bytes := CreateByteArrayFromText(AText, AEncoding, AWriteBOM);
  Result := TMemoryStream.Create;
  if Length(Bytes) > 0 then
    Result.Write(Bytes, Length(Bytes));
end;

function TTestFileIO.InFilePath(const AFileName: string): string;
begin
  Result := TestInFilePath + AFileName;
end;

function TTestFileIO.OutFilePath(const AFileName: string): string;
begin
  Result := TestOutFilePath + AFileName;
end;

procedure TTestFileIO.ReadAllBytes_raises_exception_for_invalid_file_name;
begin
  Assert.WillRaise(
    procedure
    begin
      TFileIO.ReadAllBytes(InFilePath(BadFile));
    end,
    EFOpenError
  );
end;

procedure TTestFileIO.ReadAllBytes_returns_empty_array_for_empty_file;
begin
  var Bytes := TFileIO.ReadAllBytes(InFilePath(EmptyFile));
  Assert.AreEqual(NativeInt(0), Length(Bytes));
end;

procedure TTestFileIO.ReadAllBytes_returns_expected_array_non_empty_file;
begin
  TFile.WriteAllBytes(OutFilePath(OutFile), FileBytes);
  var Bytes := TFileIO.ReadAllBytes(OutFilePath(OutFile));
  Assert.IsTrue(SameBytes(FileBytes, Bytes));
end;

procedure TTestFileIO.ReadAllLines_fails_assertion_if_encoding_is_nil;
begin
  Assert.WillRaise(
    procedure
    begin
      TFileIO.ReadAllLines(InFilePath(ASCIIFile), nil);
    end,
    EAssertionFailed
  );
end;

procedure TTestFileIO.ReadAllLines_raises_exception_for_invalid_file_name;
begin
  Assert.WillRaise(
    procedure
    begin
      TFileIO.ReadAllLines(InFilePath(BadFile), TEncoding.UTF8, True);
    end,
    EFOpenError
  );
end;

procedure TTestFileIO.ReadAllLines_raises_exception_for_mismatched_BOMs;
begin
  Assert.WillRaise(
    procedure
    begin
      TFileIO.ReadAllLines(InFilePath(UTF16BEBOMFile), TEncoding.UTF8, True);
    end,
    EFileIO
  );
end;

procedure TTestFileIO.ReadAllLines_reads_empty_text_file_without_BOM;
begin
  var Lines := TFileIO.ReadAllLines(InFilePath(EmptyFile), TEncoding.UTF8, False);
  Assert.AreEqual(NativeInt(0), Length(Lines));
end;

procedure TTestFileIO.ReadAllLines_reads_empty_text_file_with_UTF8_BOM;
begin
  var Lines := TFileIO.ReadAllLines(InFilePath(EmptyUTF8BOMFile), TEncoding.UTF8, True);
  Assert.AreEqual(NativeInt(0), Length(Lines), 'Text');
  Assert.IsTrue(TFileIO.CheckBOM(InFilePath(EmptyUTF8BOMFile), TEncoding.UTF8), 'BOM');
end;

procedure TTestFileIO.ReadAllLines_reads_expected_lines_from_ASCII_text_file_without_BOM;
begin
  var LinesOut := ASCIIFileContent.Trim.Replace(#13, '').Split([#10]);
  TFileIO.WriteAllLines(OutFilePath(OutFile), LinesOut, TEncoding.ASCII);
  var LinesIn := TFileIO.ReadAllLines(OutFilePath(OutFile), TEncoding.ASCII);
  Assert.IsTrue(SameStrings(LinesOut, LinesIn));
end;

procedure TTestFileIO.ReadAllLines_reads_expected_lines_from_Unicode_text_file_with_BOM;
begin
  var LinesOut := ASCIIFileContent.Trim.Replace(#13, '').Split([#10]);
  TFileIO.WriteAllLines(OutFilePath(OutFile), LinesOut, TEncoding.Unicode, True);
  var LinesIn := TFileIO.ReadAllLines(OutFilePath(OutFile), TEncoding.Unicode, True);
  Assert.IsTrue(SameStrings(LinesOut, LinesIn));
end;

procedure TTestFileIO.ReadAllLines_reads_expected_lines_from_UTF8_text_file_with_BOM;
begin
  var LinesOut := ASCIIFileContent.Trim.Replace(#13, '').Split([#10]);
  TFileIO.WriteAllLines(OutFilePath(OutFile), LinesOut, TEncoding.UTF8, True);
  var LinesIn := TFileIO.ReadAllLines(OutFilePath(OutFile), TEncoding.UTF8, True);
  Assert.IsTrue(SameStrings(LinesOut, LinesIn));
end;

procedure TTestFileIO.ReadAllLines_reads_expected_line_from_UTF8_text_file_without_BOM;
begin
  var LinesOut := ASCIIFileContent.Trim.Replace(#13, '').Split([#10]);
  TFileIO.WriteAllLines(OutFilePath(OutFile), LinesOut, TEncoding.UTF8, False);
  var LinesIn := TFileIO.ReadAllLines(OutFilePath(OutFile), TEncoding.UTF8, False);
  Assert.IsTrue(SameStrings(LinesOut, LinesIn));
end;

procedure TTestFileIO.ReadAllText_fails_assertion_if_encoding_is_nil;
begin
  Assert.WillRaise(
    procedure
    begin
      TFileIO.ReadAllText(InFilePath(UTF16BEBOMFile), nil, True);
    end,
    EAssertionFailed
  );
end;

procedure TTestFileIO.ReadAllText_raises_exception_for_invalid_file_name;
begin
  Assert.WillRaise(
    procedure
    begin
      TFileIO.ReadAllText(InFilePath(BadFile), TEncoding.UTF8, True);
    end,
    EFOpenError
  );
end;

procedure TTestFileIO.ReadAllText_raises_exception_for_mismatched_BOMs;
begin
  Assert.WillRaise(
    procedure
    begin
      TFileIO.ReadAllText(InFilePath(UTF16BEBOMFile), TEncoding.UTF8, True);
    end,
    EFileIO
  );
end;

procedure TTestFileIO.ReadAllText_reads_empty_text_file_without_BOM;
begin
  var Text := TFileIO.ReadAllText(InFilePath(EmptyFile), TEncoding.ASCII, False);
  Assert.AreEqual('', Text, 'Text');
end;

procedure TTestFileIO.ReadAllText_reads_empty_text_file_with_UTF8_BOM;
begin
  var Text := TFileIO.ReadAllText(InFilePath(EmptyUTF8BOMFile), TEncoding.UTF8, True);
  Assert.AreEqual('', Text, 'Text');
  Assert.IsTrue(TFileIO.CheckBOM(InFilePath(EmptyUTF8BOMFile), TEncoding.UTF8), 'BOM');
end;

procedure TTestFileIO.ReadAllText_reads_expected_text_from_ASCII_text_file_without_BOM;
begin
  TFileIO.WriteAllText(OutFilePath(OutFile), ASCIIFileContent, TEncoding.ASCII, False);
  var Text := TFileIO.ReadAllText(OutFilePath(OutFile), TEncoding.ASCII);
  Assert.AreEqual(ASCIIFileContent, Text);
end;

procedure TTestFileIO.ReadAllText_reads_expected_text_from_Unicode_text_file_with_BOM;
begin
  TFileIO.WriteAllText(OutFilePath(OutFile), UnicodeFileContent, TEncoding.Unicode, True);
  var Text := TFileIO.ReadAllText(OutFilePath(OutFile), TEncoding.Unicode, True);
  Assert.AreEqual(UnicodeFileContent, Text);
end;

procedure TTestFileIO.ReadAllText_reads_expected_text_from_UTF8_text_file_without_BOM;
begin
  TFileIO.WriteAllText(OutFilePath(OutFile), UnicodeFileContent, TEncoding.UTF8, False);
  var Text := TFileIO.ReadAllText(OutFilePath(OutFile), TEncoding.UTF8, False);
  Assert.AreEqual(UnicodeFileContent, Text);
end;

procedure TTestFileIO.ReadAllText_reads_expected_text_from_UTF8_text_file_with_BOM;
begin
  TFileIO.WriteAllText(OutFilePath(OutFile), UnicodeFileContent, TEncoding.UTF8, True);
  var Text := TFileIO.ReadAllText(OutFilePath(OutFile), TEncoding.UTF8, True);
  Assert.AreEqual(UnicodeFileContent, Text);
end;

function TTestFileIO.SameBytes(const Left, Right: TBytes): Boolean;
begin
  Result := False;
  if Length(Left) <> Length(Right) then
    Exit;
  for var I := Low(Left) to High(Left) do
    if Left[I] <> Right[I] then
      Exit;
  Result := True;
end;

function TTestFileIO.SameFiles(const LeftName, RightName: string): Boolean;
begin
  var LeftBytes := TFile.ReadAllBytes(LeftName);
  var RightBytes := TFile.ReadAllBytes(RightName);
  Result := SameBytes(LeftBytes, RightBytes);
end;

function TTestFileIO.SameStrings(const Left, Right: array of string): Boolean;
begin
  Result := False;
  if Length(Left) <> Length(Right) then
    Exit;
  for var I := Low(Left) to High(Left) do
    if Left[I] <> Right[I] then
      Exit;
  Result := True;
end;

procedure TTestFileIO.Setup;
begin
  if not TDirectory.Exists(TestOutFilePath) then
    TDirectory.CreateDirectory(TestOutFilePath);
end;

procedure TTestFileIO.TearDown;
begin
  if TDirectory.Exists(TestOutFilePath) then
    TDirectory.Delete(TestOutFilePath, True);
end;

procedure TTestFileIO.WriteAllBytes_creates_empty_file_for_empty_byte_array;
begin
  TFileIO.WriteAllBytes(OutFilePath(OutFile), []);
  Assert.IsTrue(SameFiles(InFilePath(EmptyFile), OutFilePath(OutFile)));
end;

procedure TTestFileIO.WriteAllBytes_creates_file_with_expected_content;
begin
  var OutBytes := TFile.ReadAllBytes(InFilePath(ASCIIFile));
  TFileIO.WriteAllBytes(OutFilePath(OutFile), OutBytes);
  var InBytes := TFile.ReadAllBytes(OutFilePath(OutFile));
  Assert.IsTrue(SameBytes(OutBytes, InBytes));
end;

procedure TTestFileIO.WriteAllBytes_raises_exception_for_invalid_file_name;
begin
  Assert.WillRaise(
    procedure
    begin
      TFileIO.WriteAllBytes(InFilePath(BadFile), FileBytes)
    end,
    EFCreateError
  );
end;

procedure TTestFileIO.WriteAllLines_creates_empty_text_file_without_BOM;
begin
  TFileIO.WriteAllLines(OutFilePath(OutFile), [], TEncoding.ASCII, False);
  Assert.IsTrue(SameFiles(InFilePath(EmptyFile), OutFilePath(OutFile)));
end;

procedure TTestFileIO.WriteAllLines_creates_empty_text_file_with_UTF8_BOM;
begin
  TFileIO.WriteAllLines(OutFilePath(OutFile), [], TEncoding.UTF8, True);
  Assert.IsTrue(SameFiles(InFilePath(EmptyUTF8BOMFile), OutFilePath(OutFile)), 'Text');
  Assert.IsTrue(TFileIO.CheckBOM(OutFilePath(OutFile), TEncoding.UTF8), 'BOM');
end;

procedure TTestFileIO.WriteAllLines_creates_expected_ASCII_text_file_without_BOM;
begin
  var Lines := ASCIIFileContent.Trim.Replace(#13, '').Split([#10]);
  TFileIO.WriteAllLines(OutFilePath(OutFile), Lines, TEncoding.ASCII);
  var Text := TFileIO.ReadAllText(OutFilePath(OutFile), TEncoding.ASCII, False);
  Assert.AreEqual(ASCIIFileContent, Text, 'Text');
  Assert.IsFalse(TFileIO.CheckBOM(OutFilePath(OutFile), TEncoding.ASCII), 'BOM');
end;

procedure TTestFileIO.WriteAllLines_creates_expected_Unicode_text_file_with_BOM;
begin
  var Lines := UnicodeFileContent.Trim.Replace(#13, '').Split([#10]);
  TFileIO.WriteAllLines(OutFilePath(OutFile), Lines, TEncoding.Unicode, True);
  var Text := TFileIO.ReadAllText(OutFilePath(OutFile), TEncoding.Unicode, True);
  Assert.AreEqual(UnicodeFileContent, Text, 'Text');
  Assert.IsTrue(TFileIO.CheckBOM(OutFilePath(OutFile), TEncoding.Unicode), 'BOM');
end;

procedure TTestFileIO.WriteAllLines_creates_expected_UTF8_text_file_without_BOM;
begin
  var Lines := UnicodeFileContent.Trim.Replace(#13, '').Split([#10]);
  TFileIO.WriteAllLines(OutFilePath(OutFile), Lines, TEncoding.UTF8, False);
  var Text := TFileIO.ReadAllText(OutFilePath(OutFile), TEncoding.UTF8, False);
  Assert.AreEqual(UnicodeFileContent, Text, 'Text');
  Assert.IsFalse(TFileIO.CheckBOM(OutFilePath(OutFile), TEncoding.UTF8), 'BOM');
end;

procedure TTestFileIO.WriteAllLines_creates_expected_UTF8_text_file_with_BOM;
begin
  var Lines := UnicodeFileContent.Trim.Replace(#13, '').Split([#10]);
  TFileIO.WriteAllLines(OutFilePath(OutFile), Lines, TEncoding.UTF8, True);
  var Text := TFileIO.ReadAllText(OutFilePath(OutFile), TEncoding.UTF8, True);
  Assert.AreEqual(UnicodeFileContent, Text, 'Text');
  Assert.IsTrue(TFileIO.CheckBOM(OutFilePath(OutFile), TEncoding.UTF8), 'BOM');
end;

procedure TTestFileIO.WriteAllLines_fails_assertion_if_encoding_is_nil;
begin
  Assert.WillRaise(
    procedure
    begin
      TFileIO.WriteAllLines(InFilePath(BadFile), ['a','b'], nil, False);
    end,
    EAssertionFailed
  );
end;

procedure TTestFileIO.WriteAllLines_raises_exception_for_invalid_file_name;
begin
  Assert.WillRaise(
    procedure
    begin
      TFileIO.WriteAllLines(InFilePath(BadFile), ['a','b'], TEncoding.UTF8, True);
    end,
    EFCreateError
  );
end;

procedure TTestFileIO.WriteAllText_creates_empty_text_file_without_BOM;
begin
  TFileIO.WriteAllText(OutFilePath(OutFile), '', TEncoding.ASCII, False);
  var Bytes := TFileIO.ReadAllBytes(OutFilePath(OutFile));
  Assert.AreEqual(NativeInt(0), Length(Bytes));
end;

procedure TTestFileIO.WriteAllText_creates_empty_text_file_with_UTF8_BOM;
begin
  TFileIO.WriteAllText(OutFilePath(OutFile), '', TEncoding.UTF8, True);
  var Bytes := TFileIO.ReadAllBytes(OutFilePath(OutFile));
  Assert.IsTrue(SameBytes(TEncoding.UTF8.GetPreamble, Bytes));
end;

procedure TTestFileIO.WriteAllText_creates_expected_ASCII_text_file_without_BOM;
begin
  TFileIO.WriteAllText(OutFilePath(OutFile), ASCIIFileContent, TEncoding.ASCII, False);
  var SL := TStringList.Create;
  try
    SL.LoadFromFile(OutFilePath(OutFile), TEncoding.ASCII);
    Assert.AreEqual(ASCIIFileContent, SL.Text);
  finally
    SL.Free;
  end;
end;

procedure TTestFileIO.WriteAllText_creates_expected_Unicode_text_file_with_BOM;
begin
  TFileIO.WriteAllText(OutFilePath(OutFile), UnicodeFileContent, TEncoding.Unicode, True);
  var SL := TStringList.Create;
  try
    SL.LoadFromFile(OutFilePath(OutFile), TEncoding.Unicode);
    Assert.AreEqual(UnicodeFileContent, SL.Text, 'Text');
    Assert.IsTrue(TFileIO.CheckBOM(OutFilePath(OutFile), TEncoding.Unicode), 'BOM');
  finally
    SL.Free;
  end;
end;

procedure TTestFileIO.WriteAllText_creates_expected_UTF8_text_file_without_BOM;
begin
  TFileIO.WriteAllText(OutFilePath(OutFile), UnicodeFileContent, TEncoding.UTF8, False);
  var SL := TStringList.Create;
  try
    SL.LoadFromFile(OutFilePath(OutFile), TEncoding.UTF8);
    Assert.AreEqual(UnicodeFileContent, SL.Text, 'Text');
    Assert.IsFalse(TFileIO.CheckBOM(OutFilePath(OutFile), TEncoding.UTF8), 'BOM');
  finally
    SL.Free;
  end;
end;

procedure TTestFileIO.WriteAllText_creates_expected_UTF8_text_file_with_BOM;
begin
  TFileIO.WriteAllText(OutFilePath(OutFile), UnicodeFileContent, TEncoding.UTF8, True);
  var SL := TStringList.Create;
  try
    SL.LoadFromFile(OutFilePath(OutFile), TEncoding.UTF8);
    Assert.AreEqual(UnicodeFileContent, SL.Text, 'Text');
    Assert.IsTrue(TFileIO.CheckBOM(OutFilePath(OutFile), TEncoding.UTF8), 'BOM');
  finally
    SL.Free;
  end;
end;

procedure TTestFileIO.WriteAllText_fails_assertion_if_encoding_is_nil;
begin
  Assert.WillRaise(
    procedure
    begin
      TFileIO.WriteAllText(OutFilePath(BadFile), UnicodeFileContent, nil, True);
    end,
    EAssertionFailed
  );
end;

procedure TTestFileIO.WriteAllText_raises_exception_for_invalid_file_name;
begin
  Assert.WillRaise(
    procedure
    begin
      TFileIO.WriteAllText(OutFilePath(BadFile), UnicodeFileContent, TEncoding.UTF8, True);
    end,
    EFCreateError
  );
end;

initialization

  TDUnitX.RegisterTestFixture(TTestFileIO);

end.
