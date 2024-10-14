{
 * This unit is dedicated to public domain under the CC0 license.
 * See https://creativecommons.org/public-domain/cc0/
}

unit Test.Snippets.Snippet;

interface

uses
  DUnitX.TestFramework,

  System.SysUtils,

  CSLE.Snippets.Snippet;

const
  SourceCode =
  '''
  function CloneCursorAsBitmap(const Cursor: Controls.TCursor;
    const PixelFmt: Graphics.TPixelFormat;
    const TransparentColor: Graphics.TColor): Graphics.TBitmap;
  begin
    Result := CloneCursorHandleAsBitmap(
      Forms.Screen.Cursors[Cursor], PixelFmt, TransparentColor
    );
  end;
  ''';

type

  [TestFixture]
  TTestSnippet = class
  strict private
    const
      PlainText = 'Alice ℅ Bob ¶ ©2023.';
      REMLText = '<p>Alice ℅ <strong>Bob</strong> ¶ &copy;2023.</p>';
      RTFText = '\pard Alice & Bob. (c)2023.\par';
  private
    // ** NOTE: As new properties are added to TSnippet, add a test for
    //          default-ness to the folowing test method
    procedure CheckForDefaultProperties(const S: TSnippet; const CreatedDate: TDateTime);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    // TSnippet.Created prop is tested as part of ctor tests

    [Test]
    procedure default_ctor_creates_empty_snippet_with_null_id;

    [Test]
    procedure ctor_creates_valid_default_record_with_given_id;

    [Test]
    procedure ctor_fails_creating_record_with_null_snippet_id;

    [Test]
    procedure CreateUnique_creates_record_with_non_null_id;

    [Test]
    procedure ID_prop_returns_value_passed_to_ctor;

    [Test]
    [TestCase('#A: some text','This is a title')]
    [TestCase('#B: no text','')]
    procedure Title_prop_get_reflects_set(const ATitle: string);

    [Test]
    procedure Description_prop_get_reflects_set;

    [Test]
    [TestCase('#A: some code',SourceCode)]
    [TestCase('#B: no code','')]
    procedure SourceCode_prop_get_reflects_set(const ACode: string);

    [Test]
    [TestCase('Pascal','Pascal')]
    [TestCase('C++','C++')]
    [TestCase('Default','')]
    procedure LanguageID_prop_get_reflects_set(const AIDName: string);

    [Test]
    [TestCase('#1','1959-01-03T18:25:00+0000')]
    [TestCase('#2','2024-10-09T14:17:41+0000')]
    procedure Modified_prop_get_reflects_set(const ADateStr: string);

    [Test]
    procedure Modified_prop_setter_assertion_fail_with_null_date;

    [Test]
    procedure RequiredModules_prop_get_reflects_set;

    [Test]
    procedure RequiredSnippets_prop_get_reflects_set;

    [Test]
    procedure XRefs_prop_get_reflects_set;

    [Test]
    procedure Notes_prop_get_reflects_set;

    [Test]
    procedure Format_prop_get_reflects_set;

    [Test]
    procedure Tags_prop_get_reflects_set;

    [Test]
    procedure Hash_same_as_snippet_id_hash;

    [Test]
    [TestCase('True','True')]
    [TestCase('False','False')]
    procedure Starred_prop_get_reflects_set(AValue: Boolean);

    [Test]
    procedure TestInfo_prop_get_reflects_set;
  end;

implementation

uses
  System.DateUtils,
  CSLE.Snippets.ID,
  CSLE.Snippets.Format,
  CSLE.Snippets.Markup,
  CSLE.Snippets.Tag,
  CSLE.Snippets.TestInfo,
  CSLE.SourceCode.Language, // for inlining
  CSLE.TextData,            // for inlining
  CSLE.Utils.Dates;         // for inlining

procedure TTestSnippet.CheckForDefaultProperties(const S: TSnippet; const CreatedDate: TDateTime);
begin
  // Increase the TDateTime (i.e. Extended) equality comparison epsilon a bit to
  // allow for time difference between the above to statements being executed.
  var CreatedDateEpsilon := 5 * Extended.Epsilon;

  // Create default test info record: there is no method of TSnippetTestInfo to
  // do this check
  var DefaultTestInfo: TSnippetTestInfo;

  // Check all properties except .ID have their default values
  Assert.IsTrue(S.Title.IsEmpty, '.Title is empty string');
  Assert.IsTrue(S.Description.IsDefault, '.Description markup has default value');
  Assert.IsTrue(S.SourceCode.IsEmpty, '.SourceCode is empty string');
  Assert.IsTrue(S.LanguageID.IsDefault, '.LanguageID has default value');
  Assert.IsTrue(S.Modified.IsNull, '.Modifid is null');
  Assert.AreEqual(CreatedDate, S.Created.ToDateTime, CreatedDateEpsilon, '.Created is close to now');
  Assert.AreEqual(0, Integer(Length(S.RequiredModules)), '.RequiredModules array is empty');
  Assert.AreEqual(0, Integer(Length(S.RequiredSnippets)), '.RequiredSnippets array is empty');
  Assert.AreEqual(0, Integer(Length(S.XRefs)), '.XRefs array is empty');
  Assert.IsTrue(S.Notes.IsDefault, '.Notes markup has default value');
  Assert.AreEqual(TSnippetFormatID.FreeForm, S.Format, '.Format is freeform');
  Assert.IsTrue(S.Tags.IsEmpty, '.Tags is empty');
  Assert.IsFalse(S.Starred, '.Starred is false');
  Assert.IsTrue(DefaultTestInfo = S.TestInfo, '.TestInfo has default value');
end;

procedure TTestSnippet.CreateUnique_creates_record_with_non_null_id;
begin
  var S := TSnippet.CreateUnique;
  Assert.IsFalse(S.ID.IsNull, 'CreateUnique ID is not null');
end;

procedure TTestSnippet.ctor_creates_valid_default_record_with_given_id;
begin
  var GivenID := TSnippetID.Create(TEncoding.UTF8.GetBytes('FooBar42'));
  var NewID := TSnippetID.CreateNew;
  var DateNow := TDateTime.NowUTC;
  var SGiven := TSnippet.Create(GivenID);
  var SNew := TSnippet.Create(NewID);

  Assert.AreEqual(GivenID.ToByteArray, SGiven.ID.ToByteArray, 'Given ID');
  CheckForDefaultProperties(SGiven, DateNow);

  Assert.AreEqual(NewID.ToByteArray, SNew.ID.ToByteArray, 'New ID');
  CheckForDefaultProperties(SNew, DateNow);
end;

procedure TTestSnippet.ctor_fails_creating_record_with_null_snippet_id;
begin
  var NullID: TSnippetID; // TSnippetID is initialised to null ID
  Assert.WillRaise(
    procedure
    begin
      var S := TSnippet.Create(NullID);
    end,
    EAssertionFailed
  )
end;

procedure TTestSnippet.default_ctor_creates_empty_snippet_with_null_id;
begin
  // Need to ensure that Date and the S are as close to each other as possible
  // to ensure that S.Created and DateNow are as near equal as possible.
  var DateNow := TDateTime.NowUTC;
  var S: TSnippet;    // calls default ctor
  Assert.IsTrue(S.ID.IsNull, '.ID is null');
  CheckForDefaultProperties(S, DateNow);
end;

procedure TTestSnippet.Description_prop_get_reflects_set;
begin
  var S := TSnippet.CreateUnique;

  var MPlain := TSnippetMarkup.Create(PlainText, TSnippetMarkupKind.Plain);
  S.Description := MPlain;
  Assert.IsTrue(MPlain = S.Description, 'Plain');

  var MREML := TSnippetMarkup.Create(REMLText, TSnippetMarkupKind.REML, 6);
  S.Description := MREML;
  Assert.IsTrue(MREML = S.Description, 'REML');

  var MRTF := TSnippetMarkup.Create(RTFText, TSnippetMarkupKind.RTF);
  S.Description := MRTF;
  Assert.IsTrue(MRTF = S.Description, 'RTF');

  var MEmpty: TSnippetMarkup;     // null, empty record
  S.Description := MEmpty;
  Assert.IsTrue(MEmpty = S.Description, 'Null');
end;

procedure TTestSnippet.Format_prop_get_reflects_set;
begin
  var F1 := TSnippetFormatID.Freeform;
  var F2 := TSnippetFormatID.PascalClass;
  var F3 := TSnippetFormatID.PascalRoutine;
  var S := TSnippet.CreateUnique;

  S.Format := F1;
  Assert.AreEqual(F1, S.Format, '#F1');
  S.Format := F2;
  Assert.AreEqual(F2, S.Format, '#F2');
  S.Format := F3;
  Assert.AreEqual(F3, S.Format, '#F3');
end;

procedure TTestSnippet.Hash_same_as_snippet_id_hash;
begin
  var ID := TSnippetID.Create([1,2,3,4,5,6,7,8,9,0]);
  var S := TSnippet.Create(ID);
  Assert.IsTrue(ID.Hash = S.Hash);
end;

procedure TTestSnippet.ID_prop_returns_value_passed_to_ctor;
begin
  var ID := TSnippetID.Create([1,2,3,4,5,6,7]);
  var S := TSnippet.Create(ID);
  Assert.IsTrue(ID = S.ID);
end;

procedure TTestSnippet.LanguageID_prop_get_reflects_set(const AIDName: string);
begin
  var LangID := TSourceCodeLanguageID.Create(AIDName);
  var S := TSnippet.CreateUnique;
  S.LanguageID := LangID;
  Assert.IsTrue(LangID = S.LanguageID);
end;

procedure TTestSnippet.Modified_prop_get_reflects_set(const ADateStr: string);
begin
  var D := TUTCDateTime.CreateFromISO8601String(ADateStr);
  var S := TSnippet.CreateUnique;
  S.Modified := D;
  Assert.IsTrue(D = S.Modified, ADateStr);
end;

procedure TTestSnippet.Modified_prop_setter_assertion_fail_with_null_date;
begin
  Assert.WillRaise(
    procedure
    begin
      var D := TUTCDateTime.CreateNull;
      var S := TSnippet.CreateUnique;
      S.Modified := D;
    end,
    EAssertionFailed
  )
end;

procedure TTestSnippet.Notes_prop_get_reflects_set;
begin
  var S := TSnippet.CreateUnique;

  var MPlain := TSnippetMarkup.Create(PlainText, TSnippetMarkupKind.Plain);
  S.Notes := MPlain;
  Assert.IsTrue(MPlain = S.Notes, 'Plain');

  var MREML := TSnippetMarkup.Create(REMLText, TSnippetMarkupKind.REML, 6);
  S.Notes := MREML;
  Assert.IsTrue(MREML = S.Notes, 'REML');

  var MRTF := TSnippetMarkup.Create(RTFText, TSnippetMarkupKind.RTF);
  S.Notes := MRTF;
  Assert.IsTrue(MRTF = S.Notes, 'RTF');

  var MEmpty: TSnippetMarkup;     // null, empty record
  S.Notes := MEmpty;
  Assert.IsTrue(MEmpty = S.Notes, 'Null');
end;

procedure TTestSnippet.RequiredModules_prop_get_reflects_set;
begin
  var A: TArray<string> := ['SysUtils', 'DateUtils', 'Windows', 'Forms'];
  var S := TSnippet.CreateUnique;
  S.RequiredModules := A;
  Assert.AreEqual(A, S.RequiredModules, 'RequiredModules: Non-empty');

  S.RequiredModules := [];
  Assert.AreEqual(0, Integer(Length(S.RequiredModules)), 'RequiredModules: Empty');
end;

procedure TTestSnippet.RequiredSnippets_prop_get_reflects_set;
begin
  const B1: TBytes = [42,56,42,56];
  const B2: TBytes = [1,2,3,4,5,6,7,8,9,10,11,12,13,14];
  const B3: TBytes = [$49, $74, $27, $73, $20, $61, $20, $6c, $6f, $6e,
                      $67, $20, $77, $61, $79, $20, $68, $6f, $6d, $65];
  var ID1: TSnippetID := TSnippetID.Create(B1);
  var ID2: TSnippetID := TSnippetID.Create(B2);
  var ID3: TSnippetID := TSnippetID.Create(B3);
  var S := TSnippet.CreateUnique;
  var RQSIn := TArray<TSnippetID>.Create(ID1, ID2, ID3);
  S.RequiredSnippets := RQSIn;
  var RQSOut := S.RequiredSnippets;
  Assert.IsTrue(Length(RQSIn) = Length(RQSOut), '#A Same length');
  var Eq: Boolean := True;
  for var I := Low(RQSIn) to High(RQSIn) do
    if RQSIn[I] <> RQSOut[I] then
    begin
      Eq := False;
      Break;
    end;
  Assert.IsTrue(Eq, '#A Same values');

  S.RequiredSnippets := [];
  Assert.AreEqual(0, Integer(Length(S.RequiredSnippets)), '#B Both empty');
end;

procedure TTestSnippet.Setup;
begin
end;

procedure TTestSnippet.SourceCode_prop_get_reflects_set(const ACode: string);
begin
  var S := TSnippet.CreateUnique;
  S.SourceCode := ACode;
  Assert.AreEqual(ACode, S.SourceCode);
end;

procedure TTestSnippet.Starred_prop_get_reflects_set(AValue: Boolean);
begin
  var S := TSnippet.CreateUnique;
  S.Starred := AValue;
  Assert.AreEqual(AValue, S.Starred);
end;

procedure TTestSnippet.Tags_prop_get_reflects_set;
begin

  var S := TSnippet.CreateUnique;
  var T1 := TTag.Create('Alice');
  var T2 := TTag.Create('Bob');
  var T3 := TTag.Create('Charlie');

  var TagsIn: ITagSet := TTagSet.Create([T1, T2, T3]);
  S.Tags := TagsIn;
  var TagsOut := S.Tags;
  Assert.IsTrue(TagsIn.SameAs(TagsOut), '#A sets equal');

  S.Tags.Clear;
  var TagsExpected := TTagSet.Create; // empty set
  Assert.IsTrue(S.Tags.SameAs(TagsExpected), '#B empty sets equal');

end;

procedure TTestSnippet.TearDown;
begin
end;

procedure TTestSnippet.TestInfo_prop_get_reflects_set;
begin
  var T1 := TSnippetTestInfo.Create(TTestInfoGeneral.Basic);
  var T2 := TSnippetTestInfo.Create(TTestInfoGeneral.Advanced);
  var T3 := TSnippetTestInfo.Create(
    TTestInfoGeneral.Advanced,
    [TTestInfoAdvanced.UnitTests, TTestInfoAdvanced.DemoCode]
  );
  var T4 := TSnippetTestInfo.Create(
    TTestInfoGeneral.Advanced, [TTestInfoAdvanced.UnitTests], 'http://example.com'
  );
  var S := TSnippet.CreateUnique;

  S.TestInfo := T1;
  Assert.IsTrue(T1 = S.TestInfo, 'T1');
  S.TestInfo := T2;
  Assert.IsTrue(T2 = S.TestInfo, 'T2');
  S.TestInfo := T3;
  Assert.IsTrue(T3 = S.TestInfo, 'T3');
  S.TestInfo := T4;
  Assert.IsTrue(T4 = S.TestInfo, 'T4');
end;

procedure TTestSnippet.Title_prop_get_reflects_set(const ATitle: string);
begin
  var S := TSnippet.CreateUnique;
  S.Title := ATitle;
  Assert.AreEqual(ATitle, S.Title);
end;

procedure TTestSnippet.XRefs_prop_get_reflects_set;
begin
  const B1: TBytes = [42,56,42,56];
  const B2: TBytes = [1,2,3,4,5,6,7,8,9,10,11,12,13,14];
  const B3: TBytes = [$49, $74, $27, $73, $20, $61, $20, $6c, $6f, $6e,
                      $67, $20, $77, $61, $79, $20, $68, $6f, $6d, $65];
  var ID1: TSnippetID := TSnippetID.Create(B1);
  var ID2: TSnippetID := TSnippetID.Create(B2);
  var ID3: TSnippetID := TSnippetID.Create(B3);
  var S := TSnippet.CreateUnique;
  var RQSIn := TArray<TSnippetID>.Create(ID1, ID2, ID3);
  S.XRefs := RQSIn;
  var RQSOut := S.XRefs;
  Assert.IsTrue(Length(RQSIn) = Length(RQSOut), '#A Same length');
  var Eq: Boolean := True;
  for var I := Low(RQSIn) to High(RQSIn) do
    if RQSIn[I] <> RQSOut[I] then
    begin
      Eq := False;
      Break;
    end;
  Assert.IsTrue(Eq, '#A Same values');

  S.XRefs := [];
  Assert.AreEqual(0, Integer(Length(S.XRefs)), '#B Both empty');
end;

initialization

  TDUnitX.RegisterTestFixture(TTestSnippet);

end.
