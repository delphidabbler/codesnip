{
  Delphi DUnit Test Case for the DB.UInitialLetterGroup Unit
  ----------------------------------------------------------

  $Rev$
  $Date$
}

unit TestDBUInitialLetterGroup;

interface

uses
  TestFramework, DB.UBaseGroup, DB.UInitialLetterGroup;

type
  // Test TDBInitialLetterGroupKey record
  TestTDBInitialLetterGroupKey = class(TTestCase)
  published
    procedure TestCreate;
    procedure TestEqualityOperators;
    procedure TestCompareTo;
    procedure TestImplicitStringCast;
  end;

  // Test methods for class TDBInitialLetterGroup
  TestTDBInitialLetterGroup = class(TTestCase)
  strict private
    function CreateObject(Letter: Char): TDBInitialLetterGroup;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreate;
  end;

implementation

uses
  UTestHelpers;

const
  CapitalAe = #$00C6;
  SmallCCircumflex = #$0109;
  CapitalCCircumflex = #$0108;
  Underscore = '_';

{ TestTDBInitialLetterGroupKey }

procedure TestTDBInitialLetterGroupKey.TestCompareTo;
var
  KA, KB1, KB2, KC: TDBInitialLetterGroupKey;
begin
  KA := TDBInitialLetterGroupKey.Create('A');
  KB1 := TDBInitialLetterGroupKey.Create('B');
  KB2 := TDBInitialLetterGroupKey.Create('B');
  KC := TDBInitialLetterGroupKey.Create('C');

  Check(KA.CompareTo(KB1) < 0);
  Check(KB1.CompareTo(KA) > 0);
  Check(KA.CompareTo(KC) < 0);
  Check(KC.CompareTo(KA) > 0);
  Check(KB1.CompareTo(KB2) = 0);
  Check(KB2.CompareTo(KB1) = 0);
end;

procedure TestTDBInitialLetterGroupKey.TestCreate;
var
  K: TDBInitialLetterGroupKey;
begin
  K := TDBInitialLetterGroupKey.Create('A');
  Check(K.Letter = 'A');
  K := TDBInitialLetterGroupKey.Create('a');
  Check(K.Letter = 'A');
  K := TDBInitialLetterGroupKey.Create(CapitalAe);
  Check(K.Letter = CapitalAe);
  K := TDBInitialLetterGroupKey.Create(SmallCCircumflex);
  Check(K.Letter = CapitalCCircumflex);
  K := TDBInitialLetterGroupKey.Create(Underscore);
  Check(K.Letter = Underscore);
end;

procedure TestTDBInitialLetterGroupKey.TestEqualityOperators;
var
  KA, KB1, KB2, KC: TDBInitialLetterGroupKey;
begin
  KA := TDBInitialLetterGroupKey.Create('A');
  KB1 := TDBInitialLetterGroupKey.Create('B');
  KB2 := TDBInitialLetterGroupKey.Create('B');
  KC := TDBInitialLetterGroupKey.Create('C');

  CheckTrue(KB1 = KB2);
  CheckFalse(KB1 > KB2);
  CheckTrue(KB1 >= KB2);
  CheckFalse(KB1 < KB2);
  CheckTrue(KB1 <= KB2);
  CheckFalse(KB1 <> KB2);

  CheckFalse(KA = KC);
  CheckFalse(KA > KC);
  CheckFalse(KA >= KC);
  CheckTrue(KA < KC);
  CheckTrue(KA <= KC);
  CheckTrue(KA <> KC);

  CheckFalse(KC = KA);
  CheckTrue(KC > KA);
  CheckTrue(KC >= KA);
  CheckFalse(KC < KA);
  CheckFalse(KC <= KA);
  CheckTrue(KC <> KA);
end;

procedure TestTDBInitialLetterGroupKey.TestImplicitStringCast;
var
  K: TDBInitialLetterGroupKey;
  C: Char;
begin
  K := TDBInitialLetterGroupKey.Create('A');
  C := K;
  Check(C = 'A');
end;

{ TestTDBInitialLetterGroup }

function TestTDBInitialLetterGroup.CreateObject(
  Letter: Char): TDBInitialLetterGroup;
begin
  Result := TDBInitialLetterGroup.Create(Letter);
  Result.FreeController := TAlwaysFreeController.Create;
end;

procedure TestTDBInitialLetterGroup.SetUp;
begin
end;

procedure TestTDBInitialLetterGroup.TearDown;
begin
end;

procedure TestTDBInitialLetterGroup.TestCreate;
var
  O1, O2, O3, O4, O5: TDBInitialLetterGroup;
begin
  O1 := nil;  O2 := nil;  O3 := nil;  O4 := nil;  O5 := nil;
  try
    O1 := CreateObject('A');
    O2 := CreateObject('a');
    O3 := CreateObject(CapitalAe);
    O4 := CreateObject(SmallCCircumflex);
    O5 := CreateObject(Underscore);

    Check(O1.Key.Letter = 'A');
    Check(O2.Key.Letter = 'A');
    Check(O3.Key.Letter = CapitalAe);
    Check(O4.Key.Letter = CapitalCCircumflex);
    Check(O5.Key.Letter = Underscore);

    Check(O1.DisplayName = 'A');
    Check(O2.DisplayName = 'A');
    Check(O3.DisplayName = CapitalAe);
    Check(O4.DisplayName = CapitalCCircumflex);
    Check(O5.DisplayName = Underscore);
  finally
    O1.Free;  O2.Free;  O3.Free;  O4.Free;  O5.Free;
  end;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTDBInitialLetterGroupKey.Suite);
  RegisterTest(TestTDBInitialLetterGroup.Suite);
end.

