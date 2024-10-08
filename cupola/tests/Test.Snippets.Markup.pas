{
  This unit is dedicated to public domain under the CC0 license.
  See https://creativecommons.org/public-domain/cc0/
}

unit Test.Snippets.Markup;

interface

uses
  DUnitX.TestFramework,

  System.SysUtils,

  CSLE.Snippets.Markup;

type
  [TestFixture]
  TTestSnippetMarkup = class
  strict private
    const
      PlainText = 'Alice ℅ Bob ¶ ©2023.';
      REMLText = '<p>Alice ℅ <strong>Bob</strong> ¶ &copy;2023.</p>';
      RTFText = '\pard Alice & Bob. (c)2023.\par';
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    // NOTE: default and standard ctor tests also test Kind & Content props

    [Test]
    procedure default_ctor_creates_empty_plain_text_record;

    [Test]
    procedure ctor_creates_valid_plain_text_record;
    [Test]
    procedure ctor_creates_valid_REML_record;
    [Test]
    procedure ctor_creates_valid_RTF_record;

    [Test]
    procedure Assign_op_copies_default_record_correctly;
    [Test]
    procedure Assign_op_copies_plain_text_record_correctly;
    [Test]
    procedure Assign_op_copies_REML_record_correctly;
    [Test]
    procedure Assign_op_copies_RTF_record_correctly;

    [Test]
    procedure Equal_op_returns_true_for_default_records;
    [Test]
    procedure Equal_op_returns_true_for_equal_records;
    [Test]
    procedure Equal_op_returns_false_for_unequal_content_of_same_type;
    [Test]
    procedure Equal_op_returns_false_for_equal_content_and_unequal_type;
    [Test]
    procedure Equal_op_returns_false_for_unequal_content_and_unequal_type;
    [Test]
    procedure Equal_op_returns_false_for_equal_content_and_type_but_unequal_extra;

    [Test]
    procedure NotEqual_op_returns_false_for_default_records;
    [Test]
    procedure NotEqual_op_returns_false_for_equal_records;
    [Test]
    procedure NotEqual_op_returns_true_for_unequal_content_of_same_type;
    [Test]
    procedure NotEqual_op_returns_true_for_equal_content_and_unequal_type;
    [Test]
    procedure NotEqual_op_returns_true_for_unequal_content_and_unequal_type;
    [Test]
    procedure NotEqual_op_returns_true_for_equal_content_and_type_but_unequal_extra;
  end;

implementation

uses
  CSLE.TextData;

procedure TTestSnippetMarkup.Assign_op_copies_default_record_correctly;
begin
  var M: TSnippetMarkup;  // default, empty record
  var N := M;
  Assert.AreEqual(TSnippetMarkupKind.Plain, N.Kind, 'Kind');
  Assert.IsTrue(TTextData.Create('', TTextDataType.UTF8) = N.Content, 'Content');
  Assert.AreEqual('', M.Content.ToString, 'Text');
end;

procedure TTestSnippetMarkup.Assign_op_copies_plain_text_record_correctly;
begin
  var M := TSnippetMarkup.Create(PlainText, TSnippetMarkupKind.Plain);
  var N := M;
  Assert.AreEqual(TSnippetMarkupKind.Plain, N.Kind, 'Kind');
  Assert.IsTrue(M.Content = N.Content, 'Content');
  Assert.AreEqual(PlainText, M.Content.ToString, 'Text');
end;

procedure TTestSnippetMarkup.Assign_op_copies_REML_record_correctly;
begin
  var M := TSnippetMarkup.Create(REMLText, TSnippetMarkupKind.REML, 5);
  var N := M;
  Assert.AreEqual(TSnippetMarkupKind.REML, N.Kind, 'Kind');
  Assert.IsTrue(M.Content = N.Content, 'Content');
  Assert.AreEqual(REMLText, M.Content.ToString, 'Text');
end;

procedure TTestSnippetMarkup.Assign_op_copies_RTF_record_correctly;
begin
  var M := TSnippetMarkup.Create(RTFText, TSnippetMarkupKind.RTF);
  var N := M;
  Assert.AreEqual(TSnippetMarkupKind.RTF, N.Kind, 'Kind');
  Assert.IsTrue(M.Content = N.Content, 'Content');
  Assert.AreEqual(RTFText, M.Content.ToString, 'Text');
end;

procedure TTestSnippetMarkup.ctor_creates_valid_plain_text_record;
begin
  var Data := TEncoding.UTF8.GetBytes(PlainText);
  var ExpectedContent := TTextData.Create(PlainText, TTextDataType.UTF8);
  var M := TSnippetMarkup.Create(PlainText, TSnippetMarkupKind.Plain);
  Assert.AreEqual(TSnippetMarkupKind.Plain, M.Kind, 'Kind');
  Assert.IsTrue(ExpectedContent = M.Content, 'Content');
end;

procedure TTestSnippetMarkup.ctor_creates_valid_REML_record;
begin
  var Data := TEncoding.UTF8.GetBytes(REMLText);
  var ExpectedContent := TTextData.Create(REMLText, TTextDataType.UTF8);
  var M := TSnippetMarkup.Create(REMLText, TSnippetMarkupKind.REML, 5);
  Assert.AreEqual(TSnippetMarkupKind.REML, M.Kind, 'Kind');
  Assert.IsTrue(ExpectedContent = M.Content, 'Content');
end;

procedure TTestSnippetMarkup.ctor_creates_valid_RTF_record;
begin
  var Data := TEncoding.ASCII.GetBytes(RTFText);
  var ExpectedContent := TTextData.Create(RTFText, TTextDataType.ASCII);
  var M := TSnippetMarkup.Create(RTFText, TSnippetMarkupKind.RTF);
  Assert.AreEqual(TSnippetMarkupKind.RTF, M.Kind, 'Kind');
  Assert.IsTrue(ExpectedContent = M.Content, 'Content');
end;

procedure TTestSnippetMarkup.default_ctor_creates_empty_plain_text_record;
begin
  var M: TSnippetMarkup;  // default ctor called
  var ExpectedContent := TTextData.Create('', TTextDataType.UTF8);
  Assert.AreEqual(TSnippetMarkupKind.Plain, M.Kind, 'Kind');
  Assert.AreEqual(NativeUint(0), M.Content.DataLength, '0 length');
  Assert.IsTrue(ExpectedContent = M.Content, 'Content');
end;

procedure TTestSnippetMarkup.Equal_op_returns_false_for_equal_content_and_type_but_unequal_extra;
begin
  const K = TSnippetMarkupKind.REML;
  const E1 = 4;
  const E2 = 5;
  var Left := TSnippetMarkup.Create(REMLText, K, E1);
  var Right := TSnippetMarkup.Create(REMLText, K, E2);
  Assert.IsFalse(Left = Right);
end;

procedure TTestSnippetMarkup.Equal_op_returns_false_for_equal_content_and_unequal_type;
begin
  const Text = 'Text that is same in ASCII & UTF8 encodings';
  const K1 = TSnippetMarkupKind.RTF;
  const K2 = TSnippetMarkupKind.REML;
  const E1 = 0;
  const E2 = 4;
  var Left := TSnippetMarkup.Create(Text, K1, E1);
  var Right := TSnippetMarkup.Create(Text, K2, E2);
  Assert.IsFalse(Left = Right);
end;

procedure TTestSnippetMarkup.Equal_op_returns_false_for_unequal_content_and_unequal_type;
begin
  const K1 = TSnippetMarkupKind.RTF;
  const K2 = TSnippetMarkupKind.Plain;
  var Left := TSnippetMarkup.Create(RTFText, K1, 0);
  var Right := TSnippetMarkup.Create(PlainText, K2, 0);
  Assert.IsFalse(Left = Right);
end;

procedure TTestSnippetMarkup.Equal_op_returns_false_for_unequal_content_of_same_type;
begin
  const T1 = REMLText;
  const T2 = PlainText;
  const K = TSnippetMarkupKind.Plain;
  // T1 & T2 are different, but both UTF-8 compatible. Both also valid plain text
  var Left := TSnippetMarkup.Create(T1, K, 0);
  var Right := TSnippetMarkup.Create(T2, K, 0);
  Assert.IsFalse(Left = Right);
end;

procedure TTestSnippetMarkup.Equal_op_returns_true_for_default_records;
begin
  var A, B: TSnippetMarkup;   // two default records
  Assert.IsTrue(A = B);
end;

procedure TTestSnippetMarkup.Equal_op_returns_true_for_equal_records;
begin
  const T = REMLText;
  const V = 5;
  const K = TSnippetMarkupKind.REML;
  var Left := TSnippetMarkup.Create(T, K, V);
  var Right := TSnippetMarkup.Create(T, K, V);
  Assert.IsTrue(Left = Right);
end;

procedure TTestSnippetMarkup.NotEqual_op_returns_false_for_default_records;
begin
  var A, B: TSnippetMarkup;   // two default records
  Assert.IsFalse(A <> B);
end;

procedure TTestSnippetMarkup.NotEqual_op_returns_false_for_equal_records;
begin
  const T = REMLText;
  const K = TSnippetMarkupKind.REML;
  const V = 5;
  var Left := TSnippetMarkup.Create(T, K, V);
  var Right := TSnippetMarkup.Create(T, K, V);
  Assert.IsFalse(Left <> Right);
end;

procedure TTestSnippetMarkup.NotEqual_op_returns_true_for_equal_content_and_type_but_unequal_extra;
begin
  const K = TSnippetMarkupKind.REML;
  const E1 = 4;
  const E2 = 5;
  var Left := TSnippetMarkup.Create(REMLText, K, E1);
  var Right := TSnippetMarkup.Create(REMLText, K, E2);
  Assert.IsTrue(Left <> Right);
end;

procedure TTestSnippetMarkup.NotEqual_op_returns_true_for_equal_content_and_unequal_type;
begin
  const Text = 'Text that is same in ASCII & UTF8 encodings';
  const K1 = TSnippetMarkupKind.RTF;
  const K2 = TSnippetMarkupKind.REML;
  const V1 = 0;
  const V2 = 4;
  var Left := TSnippetMarkup.Create(Text, K1, V1);
  var Right := TSnippetMarkup.Create(Text, K2, V2);
  Assert.IsTrue(Left <> Right);
end;

procedure TTestSnippetMarkup.NotEqual_op_returns_true_for_unequal_content_and_unequal_type;
begin
  const K1 = TSnippetMarkupKind.RTF;
  const K2 = TSnippetMarkupKind.Plain;
  var Left := TSnippetMarkup.Create(RTFText, K1);
  var Right := TSnippetMarkup.Create(PlainText, K2);
  Assert.IsTrue(Left <> Right);
end;

procedure TTestSnippetMarkup.NotEqual_op_returns_true_for_unequal_content_of_same_type;
begin
  const T1 = REMLText;
  const T2 = PlainText;
  const K = TSnippetMarkupKind.Plain;
  // T1 & T2 are different, but both UTF-8 compatible. Both also valid plain text
  var Left := TSnippetMarkup.Create(T1, K);
  var Right := TSnippetMarkup.Create(T2, K);
  Assert.IsTrue(Left <> Right);
end;

procedure TTestSnippetMarkup.Setup;
begin
end;

procedure TTestSnippetMarkup.TearDown;
begin
end;

initialization
  TDUnitX.RegisterTestFixture(TTestSnippetMarkup);

end.
