{
  This unit is dedicated to public domain under the CC0 license.
  See https://creativecommons.org/public-domain/cc0/
}

unit Test.Snippets.Tag;

interface

uses
  DUnitX.TestFramework,

  System.SysUtils,
  System.Classes,

  CSLE.Snippets.Tag;

type
  [TestFixture]
  TTestTTag = class
  public

    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    [TestCase('Empty','')]
    [TestCase('Begins with space',' Foo')]
    [TestCase('Invalid CRLF','Foo'#13#10'Bar')]
    [TestCase('Contains tab','Foo'#9'Bar')]
    procedure IsValidTagString_returns_false(const Str: string);
    [Test]
    [TestCase('Single letter','A')]
    [TestCase('Single number','9')]
    [TestCase('Contains space','Foo Bar')]
    [TestCase('Single dash','-')]
    [TestCase('All valid punctuation','-_:()')]
    [TestCase('Realistic','String & Character Functions')]
    [TestCase('Unusual','ƛ functions')]
    procedure IsValidTagString_returns_true(const Str: string);
    [Test]
    procedure IsValidTagString_returns_false_if_valid_string_too_long;
    [Test]
    procedure IsValidTagString_returns_true_for_max_size_valid_string;

    [Test]
    [TestCase('#1','Foo & Bar')]
    [TestCase('#2','_^&_')]
    procedure MakeValidTagString_doesnt_change_valid_tag_string(const Str: string);
    [Test]
    [TestCase('#1','Foo'#9'Bar,Foo_Bar')]
    [TestCase('#2',#10',_')]
    [TestCase('#3',' Foo Bar,_Foo Bar')]
    procedure MakeValidTagString_changes_invalid_chars_to_underscores(const Str, Expected: string);
    [Test]
    procedure MakeValidTagString_raises_exception_on_empty_string;

    [Test]
    [TestCase('Realistic','String & Character Functions')]
    [TestCase('Unusual','ƛ functions')]
    procedure ctor_with_valid_tag_string_succeeds(const Str: string);
    [Test]
    [TestCase('Empty','')]
    [TestCase('Begins with space',' Foo')]
    [TestCase('Invalid CRLF','Foo'#13#10'Bar')]
    [TestCase('Contains tab','Foo'#9'Bar')]
    procedure ctor_with_invalid_tag_string_raises_exception(const Str: string);

    // Next method tests IsNull & CreateNull
    [Test]
    procedure IsNull_is_true_for_tag_created_by_CreateNull;
    [Test]
    procedure IsNull_is_false_for_tag_created_by_ctor;

    [Test]
    [TestCase('Single char','£')]
    [TestCase('Realistic','String & Character Functions')]
    [TestCase('Unusual','ƛ functions')]
    procedure ToString_returns_tag_string_passed_ctor(const Str: string);
    [Test]
    procedure ToString_returns_empty_string_for_tag_created_by_CreateNull;

    [Test]
    [TestCase('Equal same case','True,Foo Bar,Foo Bar')]
    [TestCase('Equal different case','True,AaAA,aaaa')]
    [TestCase('Less than','False,A99-99,b12-bsb')]
    [TestCase('Greater than','False,Foo,Bar')]
    procedure Equal_op_gives_expected_results(Expected: Boolean; SL, SR: string);
    [Test]
    procedure Equal_op_returns_true_comparing_2_null_tags;
    [Test]
    procedure Equal_op_returns_false_comparing_null_and_non_null_tags;

    [Test]
    [TestCase('Equal same case','False,Foo Bar,Foo Bar')]
    [TestCase('Equal different case','False,AaAA,aaaa')]
    [TestCase('Less than','True,A99-99,b12-bsb')]
    [TestCase('Greater than','True,Foo,Bar')]
    procedure NotEqual_op_gives_expected_results(Expected: Boolean; SL, SR: string);
    [Test]
    procedure NotEqual_op_returns_false_comparing_2_null_tags;
    [Test]
    procedure NotEqual_op_returns_true_comparing_null_and_non_null_tags;

    // TTag.TComparator tests
    // can't think of a sensible way to test Hash function without simply
    // replicating its internals
    [Test]
    [TestCase('Equal same case','0,Foo Bar,Foo Bar')]
    [TestCase('Equal different case','0,AaAA,aaaa')]
    [TestCase('Less than','-1,A99-99,b12-bsb')]
    [TestCase('Greater than','1,Foo,Bar')]
    procedure comparator_Compare_gives_expected_results(Expected: Integer; SL, SR: string);
    [Test]
    procedure comparator_Compare_returns_0_comparing_2_null_tags;
    [Test]
    procedure comparator_Compare_returns_less_than_value_comparing_null_tag_to_non_null_tag;

    [Test]
    [TestCase('Equal same case','True,Foo Bar,Foo Bar')]
    [TestCase('Equal different case','True,AaAA,aaaa')]
    [TestCase('Less than','False,A99-99,b12-bsb')]
    [TestCase('Greater than','False,Foo,Bar')]
    procedure comparator_Equals_gives_expected_results(Expected: Boolean; SL, SR: string);
    [Test]
    procedure comparator_Equals_returns_true_comparing_2_null_tags;
    [Test]
    procedure comparator_Equals_returns_false_comparing_null_and_non_null_tags;
  end;

  [TestFixture]
  TTestITagSet = class
  strict private
    const
      TagStr1 = '1';
      TagStr2 = 'Tag 2';
      TagStr3 = 'Foo3';
      TagStr4 = '[4]';
      TagStr5 = '$Foo-bar-5';
      TagStr6 = '6.6.6';
      TagStr7 = '7. Strings & Characters';
      TagStr8 = 'Number "8"';
    var
      Tag1, Tag2, Tag3, Tag4, Tag5, Tag6, Tag7, Tag8: TTag;

      EmptyTagArray, OneTagArray1, OneTagArray2, FiveTagArray, SixTagArray, DupArray: TArray<TTag>;
      EmptyTagSL, OneTagSL1, OneTagSL2, FiveTagSL, SixTagSL, DupSL: TStringList;
      EmptySet, OneTagSet1, OneTagSet2, FiveTagSet, SixTagSet: ITagSet;
    function TagSetToStr(ASet: ITagSet): string;
//    function TagArrayToStr(const A: array of TTag): string;
    function TagArrayToSL(const A: array of TTag): TStringList; // caller must free TStringList
    function TagSetToSL(S: ITagSet): TStringList; // caller must free TStringList
    function ElemsMatch(ATags: ITagSet; ElemList: TStringList): Boolean; overload;
    function ElemsMatch(Left, Right: ITagSet): Boolean; overload;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    // NOTE: enumerator is implicitly tested in many of the actual tests

    [Test]
    procedure parameterless_ctor_create_empty_set;

    [Test]
    procedure array_ctor_without_dups_creates_expected_set_for_5_elem_array;
    [Test]
    procedure array_ctor_without_dups_creates_expected_set_for_0_elem_array;
    [Test]
    procedure array_ctor_without_dups_creates_expected_set_for_1_elem_array;
    [Test]
    procedure array_ctor_with_dups_raises_exception;

    [Test]
    procedure tagset_ctor_creates_expected_set_for_5_elem_set;
    [Test]
    procedure tagset_ctor_creates_expected_set_for_0_elem_set;
    [Test]
    procedure tagset_ctor_creates_expected_set_for_1_elem_set;

    [Test]
    procedure Count_prop_returns_0_for_empty_set;
    [Test]
    procedure Count_prop_returns_6_for_6_elem_set;

    [Test]
    procedure IsEmpty_returns_false_for_1_elem_set;
    [Test]
    procedure IsEmpty_returns_false_for_5_elem_set;
    [Test]
    procedure IsEmpty_returns_true_for_0_elem_set;

    [Test]
    procedure Assign_copies_6_elem_tag_set_to_1_elem_tag_set;
    [Test]
    procedure Assign_copies_empty_tag_set_to_5_elem_tag_set;

    [Test]
    procedure Clear_makes_5_elem_tag_set_empty;
    [Test]
    procedure Clear_keeps_empty_tag_set_empty;

    [Test]
    procedure Contains_elem_is_true_when_elem_in_5_elem_tag_set;
    [Test]
    procedure Contains_elem_is_true_when_elem_in_1_elem_tag_set;
    [Test]
    procedure Contains_elem_is_false_when_elem_not_in_5_elem_tag_set;
    [Test]
    procedure Contains_elem_is_false_when_elem_not_in_1_elem_tag_set;
    [Test]
    procedure Contains_elem_is_always_false_for_empty_tag_set;

    [Test]
    procedure Contains_subset_is_true_for_empty_set_and_empty_subset;
    [Test]
    procedure Contains_subset_is_true_for_non_empty_set_and_empty_subset;
    [Test]
    procedure Contains_subset_is_false_for_empty_set_and_non_empty_subset;
    [Test]
    procedure Contains_subset_is_true_for_same_non_empty_set_and_subset;
    [Test]
    procedure Contains_subset_is_true_for_non_empty_set_and_proper_subset;
    [Test]
    procedure Contains_subset_is_false_for_non_empty_set_and_superset;
    [Test]
    procedure Contains_subset_is_false_for_two_different_1_elem_sets;
    [Test]
    procedure Contains_subset_is_false_for_two_different_5_and_6_elem_sets;

    [Test]
    procedure SameAs_is_true_for_two_empty_sets;
    [Test]
    procedure SameAs_is_true_for_identical_4_element_sets;
    [Test]
    procedure SameAs_is_true_when_2_element_set_compared_to_itself;
    [Test]
    procedure SameAs_is_false_for_overlapping_4_element_sets;
    [Test]
    procedure SameAs_is_false_for_2_element_and_empty_sets;
    [Test]
    procedure SameAs_is_false_for_2_element_subset_of_3_element_set;
    [Test]
    procedure SameAs_is_false_for_disjoint_3_element_sets;

    [Test]
    procedure Include_new_elem_results_in_larger_set_containing_elem;
    [Test]
    procedure Include_existing_elem_results_in_unchanged_set;
    [Test]
    procedure Include_elem_in_empty_set_results_in_set_containing_only_that_elem;

    [Test]
    procedure Include_disjoint_set_adds_all_elems_to_set;
    [Test]
    procedure Include_overlapping_set_adds_elems_not_in_set_to_set;
    [Test]
    procedure Include_non_empty_subset_leaves_set_unchanged;
    [Test]
    procedure Include_empty_set_leaves_set_unchanged;
    [Test]
    procedure Include_non_empty_set_to_empty_set_results_same_set_as_included_set;
    [Test]
    procedure Include_empty_set_to_empty_set_yields_empty_set;

    [Test]
    procedure Exclude_elem_from_set_containing_elem_results_in_set_without_elem;
    [Test]
    procedure Exclude_elem_from_set_without_elem_leaves_set_unchanged;
    [Test]
    procedure Exclude_elem_from_set_containing_only_the_elem_result_in_empty_set;
    [Test]
    procedure Exclude_elem_from_empty_set_leaves_set_unchanged;

    [Test]
    procedure Exclude_disjoint_set_from_set_leaves_set_less_intersection;
    [Test]
    procedure Exclude_proper_subset_from_non_empty_set_leaves_difference;
    [Test]
    procedure Exclude_proper_superset_from_non_empty_set_leaves_empty_set;
    [Test]
    procedure Exclude_set_from_itself_leaves_empty_set;
    [Test]
    procedure Exclude_non_empty_set_from_empty_set_leaves_empty_set;
    [Test]
    procedure Exclude_empty_set_from_itself_leaves_empty_set;

    [Test]
    procedure Filter_on_set_of_all_tags_selects_tags_1_to_4;
    [Test]
    procedure Filter_always_true_selects_same_set;
    [Test]
    procedure Filter_always_false_selects_empty_set;
    [Test]
    procedure Filter_on_empty_selects_empty_set;

  end;

implementation

uses
  System.Types,
  CSLE.Exceptions;

{ TTestTTag }

procedure TTestTTag.comparator_Compare_gives_expected_results(Expected: Integer;
  SL, SR: string);

  function SignOf(X: Integer): Integer;
  begin
    if X < 0 then
      Result := LessThanValue
    else if X > 0 then
      Result := GreaterThanValue
    else
      Result := EqualsValue;
  end;

begin
  var Comp := TTag.TComparator.Create;
  var Left := TTag.Create(SL);
  var Right := TTag.Create(SR);
  Assert.AreEqual(Expected, SignOf(Comp.Compare(Left, Right)));
end;

procedure TTestTTag.comparator_Compare_returns_0_comparing_2_null_tags;
begin
  var Comp := TTag.TComparator.Create;
  var Left := TTag.CreateNull;
  var Right := TTag.CreateNull;
  Assert.AreEqual(EqualsValue, Comp.Compare(Left, Right));
end;

procedure TTestTTag.comparator_Compare_returns_less_than_value_comparing_null_tag_to_non_null_tag;
begin
  var Comp := TTag.TComparator.Create;
  var Left := TTag.CreateNull;
  var Right := TTag.Create('A');
  Assert.IsTrue(Comp.Compare(Left, Right) < 0);
end;

procedure TTestTTag.comparator_Equals_gives_expected_results(Expected: Boolean;
  SL, SR: string);
begin
  var Comp := TTag.TComparator.Create;
  var Left := TTag.Create(SL);
  var Right := TTag.Create(SR);
  Assert.AreEqual(Expected, Comp.Equals(Left, Right));
end;

procedure TTestTTag.comparator_Equals_returns_false_comparing_null_and_non_null_tags;
begin
  var Comp := TTag.TComparator.Create;
  var Left := TTag.CreateNull;
  var Right := TTag.Create('Foo');
  Assert.IsFalse(Comp.Equals(Left, Right));
end;

procedure TTestTTag.comparator_Equals_returns_true_comparing_2_null_tags;
begin
  var Comp := TTag.TComparator.Create;
  var Left := TTag.CreateNull;
  var Right := TTag.CreateNull;
  Assert.IsTrue(Comp.Equals(Left, Right));
end;

procedure TTestTTag.ctor_with_invalid_tag_string_raises_exception(
  const Str: string);
begin
  Assert.WillRaise(
    procedure
    begin
      var T := TTag.Create(Str);
    end,
    EUnexpected
  );
end;

procedure TTestTTag.ctor_with_valid_tag_string_succeeds(const Str: string);
begin
  Assert.WillNotRaise(
    procedure
    begin
      var T := TTag.Create(Str);
    end,
    Exception
  );
end;

procedure TTestTTag.Equal_op_gives_expected_results(Expected: Boolean; SL,
  SR: string);
begin
  var Left := TTag.Create(SL);
  var Right := TTag.Create(SR);
  Assert.AreEqual(Expected, Left = Right);
end;

procedure TTestTTag.Equal_op_returns_false_comparing_null_and_non_null_tags;
begin
  var Left := TTag.CreateNull;
  var Right := TTag.Create('A');
  Assert.IsFalse(Left = Right);
end;

procedure TTestTTag.Equal_op_returns_true_comparing_2_null_tags;
begin
  var Left := TTag.CreateNull;
  var Right := TTag.CreateNull;
  Assert.IsTrue(Left = Right);
end;

procedure TTestTTag.IsNull_is_false_for_tag_created_by_ctor;
begin
  var T := TTag.Create('Foo Bar');
  Assert.IsFalse(T.IsNull);
end;

procedure TTestTTag.IsNull_is_true_for_tag_created_by_CreateNull;
begin
  var T := TTag.CreateNull;
  Assert.IsTrue(T.IsNull);
end;

procedure TTestTTag.IsValidTagString_returns_false(const Str: string);
begin
  Assert.IsFalse(TTag.IsValidTagString(Str));
end;

procedure TTestTTag.IsValidTagString_returns_false_if_valid_string_too_long;
begin
  var Str := StringOfChar('a', TTag.MaxTagStringLength + 1);
  Assert.IsFalse(TTag.IsValidTagString(Str));
end;

procedure TTestTTag.IsValidTagString_returns_true(const Str: string);
begin
  Assert.IsTrue(TTag.IsValidTagString(Str));
end;

procedure TTestTTag.IsValidTagString_returns_true_for_max_size_valid_string;
begin
  var Str := StringOfChar('a', TTag.MaxTagStringLength);
  Assert.IsTrue(TTag.IsValidTagString(Str));
end;

procedure TTestTTag.MakeValidTagString_changes_invalid_chars_to_underscores(
  const Str, Expected: string);
begin
  Assert.AreEqual(Expected, TTag.MakeValidTagString(Str));
end;

procedure TTestTTag.MakeValidTagString_doesnt_change_valid_tag_string(
  const Str: string);
begin
  Assert.AreEqual(Str, TTag.MakeValidTagString(Str));
end;

procedure TTestTTag.MakeValidTagString_raises_exception_on_empty_string;
begin
  Assert.WillRaise(
    procedure
    begin
      var S := TTag.MakeValidTagString(string.Empty);
    end,
    EUnexpected
  );
end;

procedure TTestTTag.NotEqual_op_gives_expected_results(Expected: Boolean; SL,
  SR: string);
begin
  var Left := TTag.Create(SL);
  var Right := TTag.Create(SR);
  Assert.AreEqual(Expected, Left <> Right);
end;

procedure TTestTTag.NotEqual_op_returns_false_comparing_2_null_tags;
begin
  var Left := TTag.CreateNull;
  var Right := TTag.CreateNull;
  Assert.IsFalse(Left <> Right);
end;

procedure TTestTTag.NotEqual_op_returns_true_comparing_null_and_non_null_tags;
begin
  var Left := TTag.CreateNull;
  var Right := TTag.Create('A');
  Assert.IsTrue(Left <> Right);
end;

procedure TTestTTag.Setup;
begin

end;

procedure TTestTTag.TearDown;
begin
end;

procedure TTestTTag.ToString_returns_empty_string_for_tag_created_by_CreateNull;
begin
  var T := TTag.CreateNull;
  Assert.IsTrue(T.ToString.IsEmpty);
end;

procedure TTestTTag.ToString_returns_tag_string_passed_ctor(const Str: string);
begin
  var T := TTag.Create(Str);
  Assert.AreEqual(Str, T.ToString);
end;

{ TTestITagSet }

procedure TTestITagSet.array_ctor_without_dups_creates_expected_set_for_0_elem_array;
begin
  var Tags: ITagSet := TTagSet.Create(EmptyTagArray);
  Assert.IsTrue(TagSetToStr(Tags).IsEmpty);
end;

procedure TTestITagSet.array_ctor_without_dups_creates_expected_set_for_1_elem_array;
begin
  var Tags: ITagSet := TTagSet.Create(OneTagArray2);
  Assert.IsTrue(ElemsMatch(Tags, OneTagSL2));
end;

procedure TTestITagSet.array_ctor_without_dups_creates_expected_set_for_5_elem_array;
begin
  var Tags: ITagSet := TTagSet.Create(FiveTagArray);
  Assert.IsTrue(ElemsMatch(Tags, FiveTagSL));
end;

procedure TTestITagSet.array_ctor_with_dups_raises_exception;
begin
  Assert.WillRaise(
    procedure
    begin
      var Tags: ITagSet := TTagSet.Create(DupArray);
      Tags.Count;   // keeps compiler warning quiet
    end,
    EListError
  );
end;

procedure TTestITagSet.Assign_copies_6_elem_tag_set_to_1_elem_tag_set;
begin
  var S := OneTagSet1;
  Assert.AreEqual(NativeUInt(1), S.Count, 'Setup 1');
  Assert.AreEqual(NativeUInt(6), SixTagSet.Count, 'Setup 2');
  S.Assign(SixTagSet);
  Assert.AreEqual(NativeUInt(6), S.Count, 'Expected count');
  Assert.IsTrue(ElemsMatch(S, SixTagSL), 'Expected elements');
end;

procedure TTestITagSet.Assign_copies_empty_tag_set_to_5_elem_tag_set;
begin
  var S := FiveTagSet;
  Assert.AreEqual(NativeUInt(5), S.Count, 'Setup 1');
  Assert.AreEqual(NativeUInt(0), EmptySet.Count, 'Setup 2');
  S.Assign(EmptySet);
  Assert.AreEqual(NativeUInt(0), S.Count, 'Expected count');
  Assert.IsTrue(ElemsMatch(S, EmptyTagSL), 'Expected elements');
end;

procedure TTestITagSet.Clear_keeps_empty_tag_set_empty;
begin
  Assert.AreEqual(NativeUInt(0), EmptySet.Count, 'Setup');
  EmptySet.Clear;
  Assert.IsTrue(EmptySet.IsEmpty);
end;

procedure TTestITagSet.Clear_makes_5_elem_tag_set_empty;
begin
  Assert.AreEqual(NativeUInt(5), FiveTagSet.Count, 'Setup');
  FiveTagSet.Clear;
  Assert.IsTrue(FiveTagSet.IsEmpty);
end;

procedure TTestITagSet.Contains_elem_is_always_false_for_empty_tag_set;
begin
  Assert.IsFalse(EmptySet.Contains(Tag1), 'Tag 1');
  Assert.IsFalse(EmptySet.Contains(Tag1), 'Tag 2');
  Assert.IsFalse(EmptySet.Contains(Tag1), 'Tag 3');
  Assert.IsFalse(EmptySet.Contains(Tag1), 'Tag 4');
  Assert.IsFalse(EmptySet.Contains(Tag1), 'Tag 5');
  Assert.IsFalse(EmptySet.Contains(Tag1), 'Tag 6');
  Assert.IsFalse(EmptySet.Contains(Tag1), 'Tag 7');
  Assert.IsFalse(EmptySet.Contains(Tag8), 'Tag 8');
end;

procedure TTestITagSet.Contains_elem_is_false_when_elem_not_in_1_elem_tag_set;
begin
  Assert.IsFalse(OneTagSet1.Contains(Tag4));
end;

procedure TTestITagSet.Contains_elem_is_false_when_elem_not_in_5_elem_tag_set;
begin
  Assert.IsFalse(FiveTagSet.Contains(Tag1));
end;

procedure TTestITagSet.Contains_elem_is_true_when_elem_in_1_elem_tag_set;
begin
  Assert.IsTrue(OneTagSet1.Contains(Tag1));
end;

procedure TTestITagSet.Contains_elem_is_true_when_elem_in_5_elem_tag_set;
begin
  Assert.IsTrue(FiveTagSet.Contains(Tag5));
end;

procedure TTestITagSet.Contains_subset_is_false_for_empty_set_and_non_empty_subset;
begin
  Assert.IsFalse(EmptySet.Contains(OneTagSet1), '1 elem set');
  Assert.IsFalse(EmptySet.Contains(SixTagSet), '6 elem set');
end;

procedure TTestITagSet.Contains_subset_is_false_for_non_empty_set_and_superset;
begin
  var SuperSet: ITagSet := TTagSet.Create([Tag1, Tag2, Tag3, Tag4, Tag5, Tag6]);
  var BaseSet: ITagSet := TTagSet.Create([Tag1, Tag2, Tag3, Tag4]);
  Assert.IsFalse(BaseSet.Contains(SuperSet));
end;

procedure TTestITagSet.Contains_subset_is_false_for_two_different_1_elem_sets;
begin
  Assert.IsFalse(OneTagSet1.Contains(OneTagSet2));
end;

procedure TTestITagSet.Contains_subset_is_false_for_two_different_5_and_6_elem_sets;
begin
  Assert.IsFalse(SixTagSet.Contains(FiveTagSet));
end;

procedure TTestITagSet.Contains_subset_is_true_for_empty_set_and_empty_subset;
begin
  Assert.IsTrue(EmptySet.Contains(EmptySet));
end;

procedure TTestITagSet.Contains_subset_is_true_for_non_empty_set_and_empty_subset;
begin
  Assert.IsTrue(FiveTagSet.Contains(EmptySet));
end;

procedure TTestITagSet.Contains_subset_is_true_for_non_empty_set_and_proper_subset;
begin
  var BaseSet: ITagSet := TTagSet.Create([Tag1, Tag2, Tag3, Tag4, Tag5, Tag6]);
  var SubSet: ITagSet := TTagSet.Create([Tag1, Tag2, Tag3, Tag4]);
  Assert.IsTrue(BaseSet.Contains(SubSet));
end;

procedure TTestITagSet.Contains_subset_is_true_for_same_non_empty_set_and_subset;
begin
  Assert.IsTrue(OneTagSet1.Contains(OneTagSet1), '1 elem');
  Assert.IsTrue(FiveTagSet.Contains(FiveTagSet), '5 elem');
end;

procedure TTestITagSet.Count_prop_returns_0_for_empty_set;
begin
  Assert.AreEqual(NativeUInt(0), EmptySet.Count);
end;

procedure TTestITagSet.Count_prop_returns_6_for_6_elem_set;
begin
  Assert.AreEqual(NativeUInt(6), SixTagSet.Count);
end;

function TTestITagSet.ElemsMatch(Left, Right: ITagSet): Boolean;
begin
  var RightSL := TagSetToSL(Right);
  try
    Result := ElemsMatch(Left, RightSL);
  finally
    RightSL.Free;
  end;
end;

procedure TTestITagSet.Exclude_disjoint_set_from_set_leaves_set_less_intersection;
begin
  var S1 := SixTagSet;  // contains Tag1 Tag2  Tag3      Tag5      Tag7 Tag8
  var S2 := FiveTagSet; // contains      Tag2       Tag4 Tag5 Tag6 Tag7
  var Expected: ITagSet := TTagSet.Create([Tag1, Tag3, Tag8]);
  S1.Exclude(S2);
  Assert.IsTrue(ElemsMatch(Expected, S1));
end;

procedure TTestITagSet.Exclude_elem_from_empty_set_leaves_set_unchanged;
begin
  var S := EmptySet;
  S.Exclude(Tag1);
  Assert.IsTrue(S.IsEmpty);
end;

procedure TTestITagSet.Exclude_elem_from_set_containing_elem_results_in_set_without_elem;
begin
  var S := FiveTagSet; // contains Tag2, Tag4, Tag5, Tag6, Tag7
  var Expected: ITagSet := TTagSet.Create([Tag2, Tag4, Tag5, Tag7]);
  S.Exclude(Tag6);
  Assert.IsTrue(ElemsMatch(Expected, S));
end;

procedure TTestITagSet.Exclude_elem_from_set_containing_only_the_elem_result_in_empty_set;
begin
  var S := OneTagSet1; // contains Tag1
  Assert.IsFalse(S.IsEmpty, 'Setup');
  S.Exclude(Tag1);
  Assert.IsTrue(S.IsEmpty, 'After exclude');
end;

procedure TTestITagSet.Exclude_elem_from_set_without_elem_leaves_set_unchanged;
begin
  var S := FiveTagSet; // contains Tag2, Tag4, Tag5, Tag6, Tag7
  var Expected: ITagSet := TTagSet.Create(FiveTagSet);
  S.Exclude(Tag8);
  Assert.IsTrue(ElemsMatch(Expected, S));
end;

procedure TTestITagSet.Exclude_empty_set_from_itself_leaves_empty_set;
begin
  var S1 := EmptySet;
  var S2: ITagSet := TTagSet.Create(EmptySet);
  S1.Exclude(S2);
  Assert.IsTrue(S1.IsEmpty);
end;

procedure TTestITagSet.Exclude_non_empty_set_from_empty_set_leaves_empty_set;
begin
  var S1 := EmptySet;
  S1.Exclude(FiveTagSet);
  Assert.IsTrue(S1.IsEmpty);
end;

procedure TTestITagSet.Exclude_proper_subset_from_non_empty_set_leaves_difference;
begin
  var S1 := FiveTagSet; // contains Tag2, Tag4, Tag5, Tag6, Tag7
  var S2: ITagSet := TTagSet.Create([Tag2, Tag5, Tag7]);  // proper subset of S1
  var Expected: ITagSet := TTagSet.Create([Tag4, Tag6]);
  S1.Exclude(S2);
  Assert.IsTrue(ElemsMatch(Expected, S1));
end;

procedure TTestITagSet.Exclude_proper_superset_from_non_empty_set_leaves_empty_set;
begin
  var S1 := FiveTagSet; // contains Tag2, Tag4, Tag5, Tag6, Tag7
  var S2: ITagSet := TTagSet.Create([Tag2, Tag4, Tag5, Tag6, Tag7, Tag1, Tag8]);  // proper superset of S1
  S1.Exclude(S2);
  Assert.IsTrue(S1.IsEmpty);
end;

procedure TTestITagSet.Exclude_set_from_itself_leaves_empty_set;
begin
  var S1 := OneTagSet1;
  var S2: ITagSet := TTagSet.Create(S1);
  S1.Exclude(S2);
  Assert.IsTrue(S1.IsEmpty);
end;

procedure TTestITagSet.Filter_always_false_selects_empty_set;
begin
  var Res := SixTagSet.Filter(
    function(const ATag: TTag): Boolean
    begin
      Result := False;
    end
  );
  Assert.IsTrue(Res.IsEmpty);
end;

procedure TTestITagSet.Filter_always_true_selects_same_set;
begin
  var Res := SixTagSet.Filter(
    function(const ATag: TTag): Boolean
    begin
      Result := True;
    end
  );
  Assert.IsTrue(ElemsMatch(SixTagSet, Res));
end;

procedure TTestITagSet.Filter_on_empty_selects_empty_set;
begin
  var Res := EmptySet.Filter(
    function(const ATag: TTag): Boolean
    begin
      Result := True;
    end
  );
  Assert.IsTrue(Res.IsEmpty);
end;

procedure TTestITagSet.Filter_on_set_of_all_tags_selects_tags_1_to_4;
begin
  var S: ITagSet := TTagSet.Create([Tag1, Tag2, Tag3, Tag4, Tag5, Tag6, Tag7, Tag8]);
  var Expected: ITagSet := TTagSet.Create([Tag1, Tag2, Tag3, Tag4]);
  var Res := S.Filter(
    function(const ATag: TTag): Boolean
    begin
    {
      Tag1.ToString = '1';
      Tag2.ToString = 'Tag 2';
      Tag3.ToString = 'Foo3';
      Tag4.ToString = '[4]';
      ^^^^^^^^^^^^^^^^^^^^^^^ Select above
      Tag5.ToString = '$Foo-bar-5';
      Tag6.ToString = '6.6.6';
      Tag7.ToString = '7. Strings & Characters';
      Tag8.ToString = 'Number "8"';
    }
      Result := ATag.ToString.IndexOfAny(['1','2','3','4']) >= 0;
    end
  );

  Assert.IsTrue(ElemsMatch(Expected, Res));
end;

function TTestITagSet.ElemsMatch(ATags: ITagSet;
  ElemList: TStringList): Boolean;
begin
  Result := True;
  for var Tag in ATags do
    if ElemList.IndexOf(Tag.ToString) = -1 then
      Exit(False);
end;

procedure TTestITagSet.Include_disjoint_set_adds_all_elems_to_set;
begin
  var A1 := TArray<TTag>.Create(Tag1, Tag2, Tag3);
  var A2 := TArray<TTag>.Create(Tag5, Tag6);
  var AExpected := Concat(A1, A2);
  var S1: ITagSet := TTagSet.Create(A1);
  var S2: ITagSet := TTagSet.Create(A2);
  var SExpected: ITagSet := TTagSet.Create(AExpected);

  S1.Include(S2);
  Assert.IsTrue(ElemsMatch(SExpected, S1));
end;

procedure TTestITagSet.Include_elem_in_empty_set_results_in_set_containing_only_that_elem;
begin
  var S := EmptySet;
  Assert.IsFalse(S.Contains(Tag7), 'Setup: set doesn''t contain new tag');
  Assert.IsTrue(S.IsEmpty, 'Setup: empty set empty');
  S.Include(Tag7);
  Assert.AreEqual(NativeUInt(1), S.Count, 'Set size now 1');
  Assert.IsTrue(S.Contains(Tag7), 'Result set contains new tag');
end;

procedure TTestITagSet.Include_empty_set_leaves_set_unchanged;
begin
  var A1 := TArray<TTag>.Create(Tag1, Tag2, Tag3);
  var S1: ITagSet := TTagSet.Create(A1);
  var SExpected: ITagSet := TTagSet.Create(S1);

  S1.Include(EmptySet);
  Assert.IsTrue(ElemsMatch(SExpected, S1));
end;

procedure TTestITagSet.Include_empty_set_to_empty_set_yields_empty_set;
begin
  var S1 := EmptySet;
  var S2: ITagSet := TTagSet.Create(EmptySet);
  S1.Include(S2);
  Assert.IsTrue(S1.IsEmpty);
end;

procedure TTestITagSet.Include_existing_elem_results_in_unchanged_set;
begin
  var S := FiveTagSet; // contains Tag2, Tag4, Tag5, Tag6, Tag7
  Assert.IsTrue(ElemsMatch(S, FiveTagSL), 'Setup check');
  S.Include(Tag4);
  Assert.IsTrue(ElemsMatch(S, FiveTagSL), 'Set unchanged');
end;

procedure TTestITagSet.Include_new_elem_results_in_larger_set_containing_elem;
begin
  var S := FiveTagSet; // contains Tag2, Tag4, Tag5, Tag6, Tag7
  var NewTag := Tag3;
  var Expected := TagArrayToSL(FiveTagArray);
  try
    Expected.Add(NewTag.ToString);
    Assert.IsTrue(ElemsMatch(S, FiveTagSL), 'Setup: check elements');
    Assert.IsFalse(S.Contains(NewTag), 'Setup: NewTag not in set');
    S.Include(NewTag);
    Assert.IsTrue(ElemsMatch(S, Expected), 'Updated set content');
    Assert.IsTrue(S.Contains(NewTag), 'NewTag now in set');
  finally
    Expected.Free;
  end;
end;

procedure TTestITagSet.Include_non_empty_set_to_empty_set_results_same_set_as_included_set;
begin
  var S1: ITagSet := TTagSet.Create(EmptySet);
  S1.Include(FiveTagSet);
  Assert.IsTrue(ElemsMatch(S1, FiveTagSet));
end;

procedure TTestITagSet.Include_non_empty_subset_leaves_set_unchanged;
begin
  var A1 := TArray<TTag>.Create(Tag1, Tag2, Tag3, Tag4);
  var A2 := TArray<TTag>.Create(Tag2, Tag4);
  var AExpected := A1;
  var S1: ITagSet := TTagSet.Create(A1);
  var S2: ITagSet := TTagSet.Create(A2);
  var SExpected: ITagSet := TTagSet.Create(AExpected);

  S1.Include(S2);
  Assert.IsTrue(ElemsMatch(SExpected, S1));
end;

procedure TTestITagSet.Include_overlapping_set_adds_elems_not_in_set_to_set;
begin
  var A1 := TArray<TTag>.Create(Tag1, Tag2, Tag3);
  var A2 := TArray<TTag>.Create(Tag2, Tag4, Tag5);
  var AExpected := TArray<TTag>.Create(Tag1, Tag2, Tag3, Tag4, Tag5);
  var S1: ITagSet := TTagSet.Create(A1);
  var S2: ITagSet := TTagSet.Create(A2);
  var SExpected: ITagSet := TTagSet.Create(AExpected);

  S1.Include(S2);
  Assert.IsTrue(ElemsMatch(SExpected, S1));
end;

procedure TTestITagSet.IsEmpty_returns_false_for_1_elem_set;
begin
  Assert.IsFalse(OneTagSet1.IsEmpty);
end;

procedure TTestITagSet.IsEmpty_returns_false_for_5_elem_set;
begin
  Assert.IsFalse(FiveTagSet.IsEmpty);
end;

procedure TTestITagSet.IsEmpty_returns_true_for_0_elem_set;
begin
  Assert.IsTrue(EmptySet.IsEmpty);
end;

procedure TTestITagSet.parameterless_ctor_create_empty_set;
begin
  var Tags: ITagSet := TTagSet.Create;
  Assert.IsTrue(TagSetToStr(EmptySet).IsEmpty);
end;

procedure TTestITagSet.SameAs_is_false_for_2_element_and_empty_sets;
begin
  var T0: ITagSet := TTagSet.Create;  // empty
  var T2: ITagSet := TTagSet.Create([TTag.Create('A'), TTag.Create('B')]);
  // check symmetry
  Assert.IsFalse(T0.SameAs(T2), 'T0 <> T2');
  Assert.IsFalse(T2.SameAs(T0), 'T2 <> T0');
end;

procedure TTestITagSet.SameAs_is_false_for_2_element_subset_of_3_element_set;
begin
  var T2: ITagSet := TTagSet.Create([TTag.Create('A'), TTag.Create('B')]);
  var T3: ITagSet := TTagSet.Create([TTag.Create('A'), TTag.Create('B'), TTag.Create('C')]);
  // check symmetry
  Assert.IsFalse(T2.SameAs(T3), 'T2 <> T3');
  Assert.IsFalse(T3.SameAs(T2), 'T3 <> T2');
end;

procedure TTestITagSet.SameAs_is_false_for_disjoint_3_element_sets;
begin
  var T3a: ITagSet := TTagSet.Create([TTag.Create('A'), TTag.Create('B'), TTag.Create('C')]);
  var T3b: ITagSet := TTagSet.Create([TTag.Create('D'), TTag.Create('E'), TTag.Create('F')]);
  // check symmetry
  Assert.IsFalse(T3a.SameAs(T3b), 'T3a <> T3b');
  Assert.IsFalse(T3b.SameAs(T3a), 'T3b <> T3a');
end;

procedure TTestITagSet.SameAs_is_false_for_overlapping_4_element_sets;
begin
  var T4a: ITagSet := TTagSet.Create([TTag.Create('A'), TTag.Create('B'), TTag.Create('C'), TTag.Create('D')]);
  var T4b: ITagSet := TTagSet.Create([TTag.Create('F'), TTag.Create('E'), TTag.Create('C'), TTag.Create('D')]);
  // check symmetry
  Assert.IsFalse(T4a.SameAs(T4b), 'T4a <> T4b');
  Assert.IsFalse(T4b.SameAs(T4a), 'T4b <> T4a');
end;

procedure TTestITagSet.SameAs_is_true_for_identical_4_element_sets;
begin
  var T4a: ITagSet := TTagSet.Create([TTag.Create('A'), TTag.Create('B'), TTag.Create('C'), TTag.Create('D')]);
  var T4b: ITagSet := TTagSet.Create([TTag.Create('A'), TTag.Create('B'), TTag.Create('C'), TTag.Create('D')]);
  // check symmetry
  Assert.IsTrue(T4a.SameAs(T4b), 'T4a = T4b');
  Assert.IsTrue(T4b.SameAs(T4a), 'T4b = T4a');
end;

procedure TTestITagSet.SameAs_is_true_for_two_empty_sets;
begin
  var TA: ITagSet := TTagSet.Create;
  var TB: ITagSet := TTagSet.Create;
  // check symmetry
  Assert.IsTrue(TA.SameAs(TB), 'TA = TB');
  Assert.IsTrue(TB.SameAs(TA), 'TB = TA');
end;

procedure TTestITagSet.SameAs_is_true_when_2_element_set_compared_to_itself;
begin
  var T2: ITagSet := TTagSet.Create([TTag.Create('A'), TTag.Create('B')]);
  Assert.IsTrue(T2.SameAs(T2), 'T2 = T2');
end;

procedure TTestITagSet.Setup;
begin
  Tag1 := TTag.Create(TagStr1);
  Tag2 := TTag.Create(TagStr2);
  Tag3 := TTag.Create(TagStr3);
  Tag4 := TTag.Create(TagStr4);
  Tag5 := TTag.Create(TagStr5);
  Tag6 := TTag.Create(TagStr6);
  Tag7 := TTag.Create(TagStr7);
  Tag8 := TTag.Create(TagStr8);

  EmptyTagSL := TStringList.Create;
  OneTagSL1 := TStringList.Create;
  OneTagSL1.Add(TagStr1);
  OneTagSL2 := TStringList.Create;
  OneTagSL2.Add(TagStr2);
  FiveTagSL := TStringList.Create;
  FiveTagSL.AddStrings(TArray<string>.Create(TagStr2, TagStr4, TagStr5, TagStr6, TagStr7));
  SixTagSL := TStringList.Create;
  SixTagSL.AddStrings(TArray<string>.Create(TagStr1, TagStr7, TagStr8, TagStr5, TagStr2, TagStr3));
  DupSL := TStringList.Create;
  DupSL.AddStrings(TArray<string>.Create(TagStr1, TagStr6, TagStr8, TagStr6));

  SetLength(EmptyTagArray, 0);
  OneTagArray1 := TArray<TTag>.Create(Tag1);
  OneTagArray2 := TArray<TTag>.Create(Tag2);
  FiveTagArray := TArray<TTag>.Create(Tag2, Tag4, Tag5, Tag6, Tag7);
  SixTagArray := TArray<TTag>.Create(Tag1, Tag7, Tag8, Tag5, Tag2, Tag3);
  DupArray := TArray<TTag>.Create(Tag1, Tag6, Tag8, Tag6);

  EmptySet := TTagSet.Create;
  OneTagSet1 := TTagSet.Create(OneTagArray1);
  OneTagSet2 := TTagSet.Create(OneTagArray2);
  FiveTagSet := TTagSet.Create(FiveTagArray);
  SixTagSet := TTagSet.Create(SixTagArray);
end;

function TTestITagSet.TagArrayToSL(const A: array of TTag): TStringList;
begin
  Result := TStringList.Create;
  for var Tag in A do
    Result.Add(Tag.ToString);
end;

//function TTestITagSet.TagArrayToStr(const A: array of TTag): string;
//begin
//  Result := '';
//  for var Tag in A do
//    Result := Result + Tag.ToString + ' ';
//  Result := Result.Trim;
//end;

function TTestITagSet.TagSetToSL(S: ITagSet): TStringList;
begin
  Result := TStringList.Create;
  for var Tag in S do
    Result.Add(Tag.ToString);
end;

function TTestITagSet.TagSetToStr(ASet: ITagSet): string;
begin
  Result := string.Empty;
  for var Tag in ASet do
    Result := Result + Tag.ToString + ' ';
  Result := Result.Trim;
end;

procedure TTestITagSet.tagset_ctor_creates_expected_set_for_0_elem_set;
begin
  var Tags: ITagSet := TTagSet.Create(EmptySet);
  Assert.IsTrue(TagSetToStr(Tags).IsEmpty);
end;

procedure TTestITagSet.tagset_ctor_creates_expected_set_for_1_elem_set;
begin
  var Tags: ITagSet := TTagSet.Create(OneTagSet1);
  Assert.IsTrue(ElemsMatch(Tags, OneTagSL1));
end;

procedure TTestITagSet.tagset_ctor_creates_expected_set_for_5_elem_set;
begin
  var Tags: ITagSet := TTagSet.Create(FiveTagSet);
  Assert.IsTrue(ElemsMatch(Tags, FiveTagSL));
end;

procedure TTestITagSet.TearDown;
begin
  DupSL.Free;
  SixTagSL.Free;
  FiveTagSL.Free;
  OneTagSL2.Free;
  OneTagSL1.Free;
  EmptyTagSL.Free;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestTTag);
  TDUnitX.RegisterTestFixture(TTestITagSet);
end.
