{
  This unit is dedicated to public domain under the CC0 license.
  See https://creativecommons.org/public-domain/cc0/
}

unit Test.SourceCode.Language;

interface

uses
  DUnitX.TestFramework,

  System.Sysutils,

  CSLE.SourceCode.Language;

type
  [TestFixture]
  TTestSourceCodeLanguageID = class
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    // Order of following tests is significant

    [Test]
    [TestCase('T1: Single letter','A')]
    [TestCase('T2: Single digit','9')]
    [TestCase('T3: All alnum','aBC657def')]
    [TestCase('T4: All digits','4256')]
    [TestCase('T5: Contains dash','abc-42')]
    [TestCase('T6: C++','C++')]
    [TestCase('T7: Lots of symbols & punct','A!"£$%^&*()_+=')]
    procedure IsValidIDString_returns_true_for_valid_ID_strings_(const AID: string);
    [Test]
    [TestCase('F1: Empty string','')]
    [TestCase('F2: Punct start char','+')]
    [TestCase('F3: Contains space','A B')]
    [TestCase('F4: Leading space','  AB')]
    [TestCase('F5: Trailing space','AB  ')]
    [TestCase('F6: All spaces','   ')]
    procedure IsValidIDString_returns_false_for_invalid_ID_strings_(const AID: string);
    [Test]
    procedure IsValidIDString_returns_true_for_max_length_ID;
    [Test]
    procedure IsValidIDString_returns_false_for_too_long_ID;

    [Test]
    [TestCase('#1: Single letter','A')]
    [TestCase('#2: Single digit','9')]
    [TestCase('#3: All alnum','aBC657def')]
    [TestCase('#4: All digits','4256')]
    [TestCase('#5: Contains dash','abc-42')]
    [TestCase('#6: C++','C++')]
    [TestCase('#7: Lots of symbols & punct','A!"£$%^&*()_+=')]
    procedure ctor_succeeds_for_valid_IDs(const AID: string);
    [Test]
    procedure ctor_succeeds_for_empty_ID;
    [Test]
    procedure ctor_raises_exception_for_invalid_ID_string;
    [Test]
    procedure ctor_raises_exception_for_too_long_ID_string;

    [Test]
    [TestCase('#1: Single letter','A')]
    [TestCase('#2: Single digit','9')]
    [TestCase('#3: All alnum','aBC657def')]
    [TestCase('#4: All digits','4256')]
    [TestCase('#5: Contains dash','abc-42')]
    [TestCase('#6: C++','C++')]
    [TestCase('#7: Lots of symbols & punct','A!"£$%^&*()_+=')]
    procedure ToString_returns_correct_ID_string(const AID: string);

    [Test]
    [TestCase('Empty IDs','True,,')]
    [TestCase('Identical IDs','True,AB+CD,AB+CD')]
    [TestCase('Same IDs, but for case','True,ab+cd,AB+CD')]
    [TestCase('Different IDs','False,42,56')]
    [TestCase('Different IDs, one empty','False,,56')]
    procedure Equal_op(Expected: Boolean; ID1, ID2: string);

    [Test]
    [TestCase('Empty IDs','False,,')]
    [TestCase('Identical IDs','False,AB+CD,AB+CD')]
    [TestCase('Same IDs, but for case','False,ab+cd,AB+CD')]
    [TestCase('Different IDs','True,42,56')]
    [TestCase('Different IDs, one empty','True,,56')]
    procedure NotEqual_op(Expected: Boolean; ID1, ID2: string);

    [Test]
    [TestCase('#1 (non-empty ID)','False,Not_Default')]
    [TestCase('#2 (empty ID)','True,')]
    procedure IsDefault(Expected: Boolean; AID: string);

    [Test]
    procedure CreateDefault_creates_default_ID;

    [Test]
    procedure IsPascal_returns_true_for_PascalLanguageID;
    [Test]
    procedure IsPascal_returns_true_for_different_case_Pascal_ID;
    [Test]
    procedure IsPascal_returns_false_for_non_Pascal_ID;

    // TSourceCodeLanguageID.TComparator tests
    // can't think of a sensible way to test Hash function without simply
    // replicating its internals
    [Test]
    [TestCase('Equal, same case', '0,Java,Java')]
    [TestCase('Equal, different case', '0,c++,C++')]
    [TestCase('Less than', '-1,C++,Java')]
    [TestCase('Greater than', '1,Pascal,Java')]
    procedure comparator_Compare_gives_expected_results(Expected: Integer; const A, B: string);
    [TestCase('Equal, same case', 'True,Java,Java')]
    [TestCase('Equal, different case', 'True,c++,C++')]
    [TestCase('Less than', 'False,C++,Java')]
    [TestCase('Greater than', 'False,Pascal,Java')]
    procedure comparator_Equals_gives_expected_results(Expected: Boolean; const A, B: string);
  end;

implementation

uses
  System.Generics.Defaults,
  CSLE.Exceptions;

procedure TTestSourceCodeLanguageID.comparator_Compare_gives_expected_results(
  Expected: Integer; const A, B: string);

  function SignOf(X: Integer): Integer;
  begin
    if X = 0 then
      Result := 0
    else if X < 0 then
      Result := -1
    else
      Result := 1;
  end;

begin
  var Comparer: IComparer<TSourceCodeLanguageID> := TSourceCodeLanguageID.TComparator.Create;
  var L := TSourceCodeLanguageID.Create(A);
  var R := TSourceCodeLanguageID.Create(B);
  Assert.AreEqual(Expected, SignOf(Comparer.Compare(L, R)));
end;

procedure TTestSourceCodeLanguageID.comparator_Equals_gives_expected_results(
  Expected: Boolean; const A, B: string);
begin
  var Comparer: IEqualityComparer<TSourceCodeLanguageID> := TSourceCodeLanguageID.TComparator.Create;
  var L := TSourceCodeLanguageID.Create(A);
  var R := TSourceCodeLanguageID.Create(B);
  Assert.AreEqual(Expected, Comparer.Equals(L, R));
end;

procedure TTestSourceCodeLanguageID.CreateDefault_creates_default_ID;
begin
  var S := TSourceCodeLanguageID.CreateDefault;
  Assert.IsTrue(S.IsDefault);
end;

procedure TTestSourceCodeLanguageID.ctor_raises_exception_for_invalid_ID_string;
begin
  Assert.WillRaise(
    procedure
    begin
      var S := TSourceCodeLanguageID.Create('+C');
    end,
    EUnexpected
  );
end;

procedure TTestSourceCodeLanguageID.ctor_raises_exception_for_too_long_ID_string;
begin
  Assert.WillRaise(
    procedure
    begin
      var IDStr := StringOfChar('A', TSourceCodeLanguageID.MaxLength + 1);
      var S := TSourceCodeLanguageID.Create(IDStr);
    end,
    EUnexpected
  );
end;

procedure TTestSourceCodeLanguageID.ctor_succeeds_for_empty_ID;
begin
  Assert.WillNotRaise(
    procedure
    begin
      var S := TSourceCodeLanguageID.Create('');
    end,
    Exception
  );
end;

procedure TTestSourceCodeLanguageID.ctor_succeeds_for_valid_IDs(
  const AID: string);
begin
  Assert.WillNotRaise(
    procedure
    begin
      var S := TSourceCodeLanguageID.Create(AID);
    end,
    Exception
  );
end;

procedure TTestSourceCodeLanguageID.Equal_op(Expected: Boolean; ID1,
  ID2: string);
begin
  var Left := TSourceCodeLanguageID.Create(ID1);
  var Right := TSourceCodeLanguageID.Create(ID2);
  Assert.AreEqual(Expected, Left = Right);
end;

procedure TTestSourceCodeLanguageID.IsDefault(Expected: Boolean; AID: string);
begin
  var S := TSourceCodeLanguageID.Create(AID);
  Assert.AreEqual(Expected, S.IsDefault);
end;

procedure TTestSourceCodeLanguageID.IsPascal_returns_false_for_non_Pascal_ID;
begin
  var S := TSourceCodeLanguageID.Create('C++');
  Assert.IsFalse(S.IsPascal);
end;

procedure TTestSourceCodeLanguageID.IsPascal_returns_true_for_different_case_Pascal_ID;
begin
  var S := TSourceCodeLanguageID.Create('PASCAL');
  Assert.IsTrue(S.IsPascal);
end;

procedure TTestSourceCodeLanguageID.IsPascal_returns_true_for_PascalLanguageID;
begin
  var S := TSourceCodeLanguageID.Create(TSourceCodeLanguageID.PascalLanguageID);
  Assert.IsTrue(S.IsPascal);
end;

procedure TTestSourceCodeLanguageID.IsValidIDString_returns_false_for_invalid_ID_strings_(
  const AID: string);
begin
  Assert.IsFalse(TSourceCodeLanguageID.IsValidIDString(AID));
end;

procedure TTestSourceCodeLanguageID.IsValidIDString_returns_false_for_too_long_ID;
begin
  var IDStr := StringOfChar('A', TSourceCodeLanguageID.MaxLength + 1);
  Assert.IsFalse(TSourceCodeLanguageID.IsValidIDString(IDStr));
end;

procedure TTestSourceCodeLanguageID.IsValidIDString_returns_true_for_max_length_ID;
begin
  var IDStr := StringOfChar('A', TSourceCodeLanguageID.MaxLength);
  Assert.IsTrue(TSourceCodeLanguageID.IsValidIDString(IDStr));
end;

procedure TTestSourceCodeLanguageID.IsValidIDString_returns_true_for_valid_ID_strings_(
  const AID: string);
begin
  Assert.IsTrue(TSourceCodeLanguageID.IsValidIDString(AID));
end;

procedure TTestSourceCodeLanguageID.NotEqual_op(Expected: Boolean; ID1,
  ID2: string);
begin
  var Left := TSourceCodeLanguageID.Create(ID1);
  var Right := TSourceCodeLanguageID.Create(ID2);
  Assert.AreEqual(Expected, Left <> Right);
end;

procedure TTestSourceCodeLanguageID.Setup;
begin
end;

procedure TTestSourceCodeLanguageID.TearDown;
begin
end;

procedure TTestSourceCodeLanguageID.ToString_returns_correct_ID_string(
  const AID: string);
begin
  var S := TSourceCodeLanguageID.Create(AID);
  Assert.AreEqual(AID, S.ToString);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestSourceCodeLanguageID);

end.
