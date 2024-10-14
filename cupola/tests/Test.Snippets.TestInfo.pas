{
 * This unit is dedicated to public domain under the CC0 license.
 * See https://creativecommons.org/public-domain/cc0/
}

unit Test.Snippets.TestInfo;

interface

uses
  DUnitX.TestFramework,

  System.SysUtils,

  CSLE.Snippets.TestInfo;

type
  [TestFixture]
  TTestSnippetTestInfo = class
  private
    // Splits a string in form [ident{+ident}] into set where ident is a name of
    // a member of the TTestInfoAdvanced enumeration
    function AdvancedSetFromStr(const S: string): TTestInfoAdvancedSet;
    // Converts a name of a member of the TTestInfoGeneral enumeration into the
    // matching value from the enumeration
    function GeneralTestFromStr(const S: string): TTestInfoGeneral;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    [TestCase('Unknown','Unknown')]
    [TestCase('None','None')]
    [TestCase('Basic','Basic')]
    [TestCase('Advanced','Advanced')]
    procedure ctor_with_only_one_param_always_succeeds(const AKindStr: string);

    [Test]
    [TestCase('Unknown/[]','Unknown,')]
    [TestCase('None/[UnitTests,DemoCode]','None,UnitTests+DemoCode')]
    [TestCase('Basic/[OtherTests]','Basic,OtherTests')]
    [TestCase('Advanced/[]','Advanced,')]
    [TestCase('Advanced/[UnitTests]','Advanced,UnitTests')]
    [TestCase('Advanced/[UnitTests,OtherTests]','Advanced,UnitTests+OtherTests')]
    [TestCase('Advanced/[UnitTests,DemoCode,OtherTests]','Advanced,UnitTests+DemoCode+OtherTests')]
    procedure ctor_with_2_params_always_succeeds(const AGeneralStr, AAdvancedStr: string);

    [Test]
    [TestCase('Unknown/[]/<good-url>','Unknown,,http://example.com')]
    [TestCase('None/[UnitTests,DemoCode]/<good-url>','None,UnitTests+DemoCode,http://www.example.com')]
    [TestCase('Basic/[OtherTests]/<good-url>','Basic,OtherTests','ftp://42.56.com/tests')]
    [TestCase('Advanced/[]/<good-url>','Advanced,,mailto:foo@bar.com')]
    [TestCase('Advanced/[UnitTests]/<good-url>','Advanced,UnitTests,http://example.com')]
    [TestCase('Advanced/[UnitTests,OtherTests]/<good-url>','Advanced,UnitTests+OtherTests,https://example.com:5678/42')]
    [TestCase('Advanced/[UnitTests+DemoCode+OtherTests]/<good-url>','Advanced,UnitTests+DemoCode+OtherTests,http://example.com#56')]
    procedure ctor_with_3_params_and_good_url_succeeds(const AGeneralStr, AAdvancedStr, AURLStr: string);

    [Test]
    [TestCase('Unknown/[]','Unknown,')]
    [TestCase('Unknown/[UnitTests]','Unknown,UnitTests')]
    [TestCase('None/[UnitTests,DemoCode]','None,UnitTests+DemoCode')]
    [TestCase('Basic/[OtherTests]','Basic,OtherTests')]
    [TestCase('Advanced/[]','Advanced,')]
    procedure ctor_with_3_params_that_ignore_url_always_succeeds_with_bad_url(const AGeneralStr, AAdvancedStr: string);

    [Test]
    [TestCase('Advanced/[UnitTests]','Advanced,UnitTests')]
    [TestCase('Advanced/[UnitTests,OtherTests]','Advanced,UnitTests+OtherTests')]
    procedure ctor_with_advanced_params_that_use_url_raises_exception_with_bad_url(const AGeneralStr, AAdvancedStr: string);

    [Test]
    procedure props_have_expected_default_values_when_created_by_default_ctor;

    [Test]
    [TestCase('Unknown','Unknown')]
    [TestCase('None','None')]
    [TestCase('Basic','Basic')]
    [TestCase('Advanced','Advanced')]
    procedure props_have_expected_values_when_created_by_1_param_ctor(const AGeneralStr: string);

    [Test]
    [TestCase('Unknown/[]','Unknown,,Unknown,')]
    [TestCase('None/[]','None,,None,')]
    [TestCase('Basic/[]','Basic,,Basic,')]
    [TestCase('Advanced/[]','Advanced,,Advanced,')]
    [TestCase('Unknown/[UnitTests]','Unknown,UnitTests,Unknown,')]
    [TestCase('None/[DemoCode,OtherTests]','None,DemoCode+OtherTests,None,')]
    [TestCase('Basic/[DemoCode,UnitTests]','Basic,DemoCode+UnitTests,Basic,')]
    [TestCase('Advanced/[DemoCode,OtherTests,UnitTests]','Advanced,DemoCode+OtherTests+UnitTests,Advanced,DemoCode+OtherTests+UnitTests')]
    procedure props_have_expected_values_when_created_by_2_param_ctor(
      const AGeneralParamStr, AAdvancedParamStr, AGeneralExpectedStr,AAdvancedExpectedStr: string);

    [Test]
    [TestCase('Unknown/[]/<no-url>','Unknown,,,Unknown,,')]
    [TestCase('Unknown/[]/<url>','Unknown,,http://example.com,Unknown,,')]
    [TestCase('None/[]/<no-url>','None,,,None,,')]
    [TestCase('None/[]/<url>','None,,https://example.com,None,,')]
    [TestCase('Basic/[]/<no-url>','Basic,,,Basic,,')]
    [TestCase('Basic/[]/<url>','Basic,,http://example.com,Basic,,')]
    [TestCase('Advanced/[]/<no-url>','Advanced,,,Advanced,,')]
    [TestCase('Advanced/[]/<url>','Advanced,,mailto:foo@bar.com,Advanced,,')]
    [TestCase('Unknown/[UnitTests]/<no-url>','Unknown,UnitTests,,Unknown,,')]
    [TestCase('Unknown/[UnitTests]/<url>','Unknown,UnitTests,http://www.example.com/,Unknown,,')]
    [TestCase('None/[DemoCode,OtherTests]/<no-url>','None,DemoCode+OtherTests,,None,,')]
    [TestCase('None/[DemoCode,OtherTests]/<url>','None,DemoCode+OtherTests,http://example.com/,None,,')]
    [TestCase('Basic/[DemoCode,UnitTests]/<no-url>','Basic,DemoCode+UnitTests,,Basic,,')]
    [TestCase('Basic/[DemoCode,UnitTests]/<url>','Basic,DemoCode+UnitTests,ftp://foo.bar.com/test,Basic,,')]
    [TestCase('Advanced/[DemoCode,OtherTests,UnitTests]/<no-url>','Advanced,DemoCode+OtherTests+UnitTests,,Advanced,DemoCode+OtherTests+UnitTests,')]
    [TestCase('Advanced/[DemoCode,OtherTests,UnitTests]/<url>','Advanced,DemoCode+OtherTests+UnitTests,http://example.com,Advanced,DemoCode+OtherTests+UnitTests,http://example.com')]
    procedure props_have_expected_values_when_created_by_3_param_ctor(
      const AGeneralParamStr, AAdvancedParamStr, AURLParam, AGeneralExpectedStr, AAdvancedExpectedStr, AURLExpected: string);

    [Test]
    [TestCase('Unknown <> Basic','Unknown,,,Basic,,,False')]
    [TestCase('None = None','None,,,None,,,True')]
    [TestCase('None/[]/<no-url>','None,,')]
    [TestCase('Basic/[]/<no-url>','Basic,,')]
    [TestCase('Advanced/[]/<no-url>','Advanced,,')]
    [TestCase('Advanced/[DemoCode,OtherTests,UnitTests]/<no-url>','Advanced,DemoCode+OtherTests+UnitTests,')]
    [TestCase('Advanced/[DemoCode,OtherTests,UnitTests]/<url>','Advanced,DemoCode+OtherTests+UnitTests,http://example.com')]
    procedure Assign_op_preserves_props(const AGeneralParam, AAdvancedParam, AURL: string);

    [Test]
    [TestCase('Unknown <> Basic','Unknown,,,Basic,,,False')]
    [TestCase('Basic <> Unknown','Basic,,,Unknown,,,False')]
    [TestCase('None/[]/<url> = None','None,,http://example.com,None,,,True')]
    [TestCase('Basic = Basic','Basic,,,Basic,,,True')]
    [TestCase('Basic = Basic/[DemoCode]/<url>','Basic,,,Basic,DemoCode,https://example.com,True')]
    [TestCase('Advanced/[]/<url> = Advanced/[]/<no-url>','Advanced,,http://example.com,Advanced,,,True')]
    [TestCase('Basic <> Advanced/[DemoCode,OtherTests,UnitTests]/<url>','Basic,,,Advanced,DemoCode+OtherTests+UnitTests,http://example.com,False')]
    [TestCase('Advanced/[DemoCode,OtherTests,UnitTests]/<url> = Self','Advanced,DemoCode+OtherTests+UnitTests,http://example.com,Advanced,DemoCode+OtherTests+UnitTests,http://example.com,True')]
    [TestCase('Advanced/[OtherTests,UnitTests]/<url> <> Advanced/[OtherTests,UnitTests]/<no-url>','Advanced,OtherTests+UnitTests,http://example.com,Advanced,OtherTests+UnitTests,,False')]
    [TestCase('Advanced/[OtherTests,UnitTests]/<url> <> Advanced/[OtherTests]/<url>','Advanced,OtherTests+UnitTests,http://example.com,Advanced,OtherTests,http://example.com,False')]
    procedure Equal_op_has_expected_results(const AGeneralLeft, AAdvancedLeft, AURLLeft,
      AGeneralRight, AAdvancedRight, AURLRight: string; const Expected: Boolean);

    [Test]
    [TestCase('Unknown <> Basic','Unknown,,,Basic,,,True')]
    [TestCase('Basic <> Unknown','Basic,,,Unknown,,,True')]
    [TestCase('None/[]/<url> = None','None,,http://example.com,None,,,False')]
    [TestCase('Basic = Basic','Basic,,,Basic,,,False')]
    [TestCase('Basic = Basic/[DemoCode]/<url>','Basic,,,Basic,DemoCode,https://example.com,False')]
    [TestCase('Advanced/[]/<url> = Advanced/[]/<no-url>','Advanced,,http://example.com,Advanced,,,False')]
    [TestCase('Basic <> Advanced/[DemoCode,OtherTests,UnitTests]/<url>','Basic,,,Advanced,DemoCode+OtherTests+UnitTests,http://example.com,True')]
    [TestCase('Advanced/[DemoCode,OtherTests,UnitTests]/<url> = Self','Advanced,DemoCode+OtherTests+UnitTests,http://example.com,Advanced,DemoCode+OtherTests+UnitTests,http://example.com,False')]
    [TestCase('Advanced/[OtherTests,UnitTests]/<url> <> Advanced/[OtherTests,UnitTests]/<no-url>','Advanced,OtherTests+UnitTests,http://example.com,Advanced,OtherTests+UnitTests,,True')]
    [TestCase('Advanced/[OtherTests,UnitTests]/<url> <> Advanced/[OtherTests]/<url>','Advanced,OtherTests+UnitTests,http://example.com,Advanced,OtherTests,http://example.com,True')]
    procedure NotEqual_op_has_expected_results(const AGeneralLeft, AAdvancedLeft, AURLLeft,
      AGeneralRight, AAdvancedRight, AURLRight: string; const Expected: Boolean);
  end;

implementation

uses
  System.Classes,
  System.RTTI;

function TTestSnippetTestInfo.AdvancedSetFromStr(
  const S: string): TTestInfoAdvancedSet;
begin
  var AdvArray := S.Split(['+']);
  Result := [];
  for var ElemStr in AdvArray do
  begin
    var AdvElem := TRttiEnumerationType.GetValue<TTestInfoAdvanced>(ElemStr);
    Include(Result, AdvElem);
  end;
end;

procedure TTestSnippetTestInfo.Assign_op_preserves_props(const AGeneralParam,
  AAdvancedParam, AURL: string);
begin
  var General := GeneralTestFromStr(AGeneralParam);
  var Advanced := AdvancedSetFromStr(AAdvancedParam);
  var TRight := TSnippetTestInfo.Create(General, Advanced, AURL);
  var TLeft := TRight;
  Assert.AreEqual(General, TLeft.General, '.General');
  Assert.IsTrue(Advanced = TLeft.Advanced, '.Advanced');
  Assert.AreEqual(AURL, TLeft.URL, '.URL');
end;

procedure TTestSnippetTestInfo.ctor_with_2_params_always_succeeds(
  const AGeneralStr, AAdvancedStr: string);
begin
  var General := GeneralTestFromStr(AGeneralStr);
  var Advanced := AdvancedSetFromStr(AAdvancedStr);
  Assert.WillNotRaise(
    procedure
    begin
      var T := TSnippetTestInfo.Create(General, Advanced);
    end
  );
end;

procedure TTestSnippetTestInfo.ctor_with_3_params_and_good_url_succeeds(
  const AGeneralStr, AAdvancedStr, AURLStr: string);
begin
  var General := GeneralTestFromStr(AGeneralStr);
  var Advanced := AdvancedSetFromStr(AAdvancedStr);
  Assert.WillNotRaise(
    procedure
    begin
      var T := TSnippetTestInfo.Create(General, Advanced, AURLStr);
    end
  );
end;

procedure TTestSnippetTestInfo.ctor_with_3_params_that_ignore_url_always_succeeds_with_bad_url(
  const AGeneralStr, AAdvancedStr: string);
begin
  var General := GeneralTestFromStr(AGeneralStr);
  var Advanced := AdvancedSetFromStr(AAdvancedStr);
  Assert.WillNotRaise(
    procedure
    begin
      var T := TSnippetTestInfo.Create(General, Advanced, 'BAD URL');
    end
  );
end;

procedure TTestSnippetTestInfo.ctor_with_advanced_params_that_use_url_raises_exception_with_bad_url(
  const AGeneralStr, AAdvancedStr: string);
begin
  var General := GeneralTestFromStr(AGeneralStr);
  var Advanced := AdvancedSetFromStr(AAdvancedStr);
  Assert.WillRaise(
    procedure
    begin
      var T := TSnippetTestInfo.Create(General, Advanced, 'BAD URL');
    end,
    ESnippetTestInfo
  );
end;

procedure TTestSnippetTestInfo.ctor_with_only_one_param_always_succeeds(
  const AKindStr: string);

begin
  Assert.WillNotRaise(
    procedure
    begin
      var T := TSnippetTestInfo.Create(GeneralTestFromStr(AKindStr));
    end
  );
end;

procedure TTestSnippetTestInfo.Equal_op_has_expected_results(const AGeneralLeft,
  AAdvancedLeft, AURLLeft, AGeneralRight, AAdvancedRight, AURLRight: string;
  const Expected: Boolean);
begin
  var GeneralLeft := GeneralTestFromStr(AGeneralLeft);
  var AdvancedLeft := AdvancedSetFromStr(AAdvancedLeft);
  var GeneralRight := GeneralTestFromStr(AGeneralRight);
  var AdvancedRight := AdvancedSetFromStr(AAdvancedRight);
  var TLeft := TSnippetTestInfo.Create(GeneralLeft, AdvancedLeft, AURLLeft);
  var TRight := TSnippetTestInfo.Create(GeneralRight, AdvancedRight, AURLRight);
  Assert.AreEqual(TLeft = TRight, Expected);
end;

function TTestSnippetTestInfo.GeneralTestFromStr(
  const S: string): TTestInfoGeneral;
begin
  Result := TRttiEnumerationType.GetValue<TTestInfoGeneral>(S);
end;

procedure TTestSnippetTestInfo.NotEqual_op_has_expected_results(
  const AGeneralLeft, AAdvancedLeft, AURLLeft, AGeneralRight, AAdvancedRight,
  AURLRight: string; const Expected: Boolean);
begin
  var GeneralLeft := GeneralTestFromStr(AGeneralLeft);
  var AdvancedLeft := AdvancedSetFromStr(AAdvancedLeft);
  var GeneralRight := GeneralTestFromStr(AGeneralRight);
  var AdvancedRight := AdvancedSetFromStr(AAdvancedRight);
  var TLeft := TSnippetTestInfo.Create(GeneralLeft, AdvancedLeft, AURLLeft);
  var TRight := TSnippetTestInfo.Create(GeneralRight, AdvancedRight, AURLRight);
  Assert.AreEqual(TLeft <> TRight, Expected);
end;

procedure TTestSnippetTestInfo.props_have_expected_default_values_when_created_by_default_ctor;
begin
  var T: TSnippetTestInfo;  // calls default ctor
  Assert.AreEqual(TTestInfoGeneral.Unknown, T.General, '.General');
  Assert.IsTrue([] = T.Advanced, '.Advanced');
  Assert.IsTrue(T.URL.IsEmpty, '.URL');
end;

procedure TTestSnippetTestInfo.props_have_expected_values_when_created_by_1_param_ctor(
  const AGeneralStr: string);
begin
  var General := GeneralTestFromStr(AGeneralStr);
  var T := TSnippetTestInfo.Create(General);
  Assert.AreEqual(General, T.General, '.General');
  Assert.IsTrue([] = T.Advanced, '.Advanced');
  Assert.IsTrue(T.URL.IsEmpty);
end;

procedure TTestSnippetTestInfo.props_have_expected_values_when_created_by_2_param_ctor(
  const AGeneralParamStr, AAdvancedParamStr, AGeneralExpectedStr,
  AAdvancedExpectedStr: string);
begin
  var GeneralParam := GeneralTestFromStr(AGeneralParamStr);
  var GeneralExpected := GeneralTestFromStr(AGeneralExpectedStr);
  var AdvancedParam := AdvancedSetFromStr(AAdvancedParamStr);
  var AdvancedExpected := AdvancedSetFromStr(AAdvancedExpectedStr);
  var T := TSnippetTestInfo.Create(GeneralParam, AdvancedParam);
  Assert.AreEqual(GeneralExpected, T.General, '.General');
  Assert.IsTrue(AdvancedExpected = T.Advanced, '.Advanced');
  Assert.IsTrue(T.URL.IsEmpty, '.URL');
end;

procedure TTestSnippetTestInfo.props_have_expected_values_when_created_by_3_param_ctor(
  const AGeneralParamStr, AAdvancedParamStr, AURLParam, AGeneralExpectedStr,
  AAdvancedExpectedStr, AURLExpected: string);
begin
  var GeneralParam := GeneralTestFromStr(AGeneralParamStr);
  var GeneralExpected := GeneralTestFromStr(AGeneralExpectedStr);
  var AdvancedParam := AdvancedSetFromStr(AAdvancedParamStr);
  var AdvancedExpected := AdvancedSetFromStr(AAdvancedExpectedStr);
  var T := TSnippetTestInfo.Create(GeneralParam, AdvancedParam, AURLParam);
  Assert.AreEqual(GeneralExpected, T.General, '.General');
  Assert.IsTrue(AdvancedExpected = T.Advanced, '.Advanced');
  Assert.AreEqual(AURLExpected, T.URL, '.URL');
end;

procedure TTestSnippetTestInfo.Setup;
begin
end;

procedure TTestSnippetTestInfo.TearDown;
begin
end;

initialization

  TDUnitX.RegisterTestFixture(TTestSnippetTestInfo);

end.
