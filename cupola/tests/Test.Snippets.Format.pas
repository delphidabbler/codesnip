{
 * This unit is dedicated to public domain under the CC0 license.
 * See https://creativecommons.org/public-domain/cc0/
}

unit Test.Snippets.Format;

interface

uses
  DUnitX.TestFramework,

  System.SysUtils,

  CSLE.Snippets.Format;

type
  [TestFixture]
  TTestTSnippetFormat = class
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    // TSnippetFormat is a simple advanced record that exposes fields as
    // properties and provides a constructor and = and <> operators.
    [Test]
    procedure ctor_sets_properties_correctly;

    [Test]
    procedure Equal_op_returns_true_for_records_with_same_ID;
    [Test]
    procedure Equal_op_returns_false_for_records_with_different_IDs;

    [Test]
    procedure NotEqual_op_returns_false_for_records_with_same_ID;
    [Test]
    procedure NotEqual_op_returns_true_for_records_with_different_IDs;
  end;

  [TestFixture]
  TTestISnippetFormatList = class
  strict private
    var
      List: ISnippetFormatList;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure AllIDs_prop_returns_full_set_of_IDs;

    [Test]
    procedure Items_default_prop_returns_correct_FreeForm_record;
    [Test]
    procedure Items_default_prop_returns_correct_PascalClass_record;
    [Test]
    procedure Items_default_prop_returns_correct_PascalUnit_record;

    [Test]
    procedure First_returns_first_item_in_list;

    [Test]
    procedure Last_returns_last_item_in_list;

    [Test]
    procedure enumerator_iterates_all_items;
  end;

implementation

{ TTestTSnippetFormat }

procedure TTestTSnippetFormat.ctor_sets_properties_correctly;
begin
  const DisplayName = 'Pascal Routine';
  const Depends: TSnippetFormatIDs = [
    TSnippetFormatID.PascalRoutine, TSnippetFormatID.PascalConst,
    TSnippetFormatID.PascalType, TSnippetFormatID.PascalClass
  ];
  var F := TSnippetFormat.Create(
    TSnippetFormatID.PascalRoutine, DisplayName, Depends
  );
  Assert.AreEqual(TSnippetFormatID.PascalRoutine, F.ID, 'ID');
  Assert.AreEqual(DisplayName, F.DisplayName, 'DisplayName');
  Assert.AreEqual(Depends, F.ValidDependIDs, 'ValidDependIDs');
end;

procedure TTestTSnippetFormat.Equal_op_returns_false_for_records_with_different_IDs;
begin
  // equality is based only on ID property: other properties are ignored
  var Left := TSnippetFormat.Create(TSnippetFormatID.PascalConst, 'Freeform', []);
  var Right := TSnippetFormat.Create(TSnippetFormatID.Freeform, 'FreeForm', []);
  Assert.IsFalse(Left = Right);
end;

procedure TTestTSnippetFormat.Equal_op_returns_true_for_records_with_same_ID;
begin
  // equality is based only on ID property: other properties are ignored
  var Left := TSnippetFormat.Create(TSnippetFormatID.PascalConst, 'Pascal Const', [TSnippetFormatID.PascalRoutine]);
  var Right := TSnippetFormat.Create(TSnippetFormatID.PascalConst, 'Const', []);
  Assert.IsTrue(Left = Right);
end;

procedure TTestTSnippetFormat.NotEqual_op_returns_false_for_records_with_same_ID;
begin
  var Left := TSnippetFormat.Create(TSnippetFormatID.PascalConst, 'Pascal Const', [TSnippetFormatID.PascalRoutine]);
  var Right := TSnippetFormat.Create(TSnippetFormatID.PascalConst, 'Const', []);
  Assert.IsFalse(Left <> Right);
end;

procedure TTestTSnippetFormat.NotEqual_op_returns_true_for_records_with_different_IDs;
begin
  var Left := TSnippetFormat.Create(TSnippetFormatID.PascalConst, 'Pascal Const', [TSnippetFormatID.PascalRoutine]);
  var Right := TSnippetFormat.Create(TSnippetFormatID.Freeform, 'FreeForm', []);
  Assert.IsTrue(Left <> Right);
end;

procedure TTestTSnippetFormat.Setup;
begin
end;

procedure TTestTSnippetFormat.TearDown;
begin
end;

{ TTestISnippetFormatList }

procedure TTestISnippetFormatList.AllIDs_prop_returns_full_set_of_IDs;
begin
  var Res := List.AllIDs;
  var Expected: TSnippetFormatIDs := [
    TSnippetFormatID.Freeform, TSnippetFormatID.PascalRoutine,
    TSnippetFormatID.PascalConst, TSnippetFormatID.PascalType,
    TSnippetFormatID.PascalUnit, TSnippetFormatID.PascalClass
  ];
  Assert.AreEqual(Expected, Res);
end;

procedure TTestISnippetFormatList.enumerator_iterates_all_items;
begin
  var Expected: TSnippetFormatIds := List.AllIDs;
  var Result: TSnippetFormatIds := [];
  for var Rec in List do  // calls enumerator
    Include(Result, Rec.ID);
  Assert.AreEqual(Expected, Result);
end;

procedure TTestISnippetFormatList.First_returns_first_item_in_list;
begin
  Assert.AreEqual(List.Items[Low(TSnippetFormatID)], List.First);
end;

procedure TTestISnippetFormatList.Items_default_prop_returns_correct_FreeForm_record;
begin
  var R := List[TSnippetFormatID.FreeForm];
  Assert.AreEqual(TSnippetFormatID.FreeForm, R.ID, 'ID');
  Assert.AreEqual('Freeform', R.DisplayName, 'DisplayName');
end;

procedure TTestISnippetFormatList.Items_default_prop_returns_correct_PascalClass_record;
begin
  var R := List[TSnippetFormatID.PascalClass];
  Assert.AreEqual(TSnippetFormatID.PascalClass, R.ID);
end;

procedure TTestISnippetFormatList.Items_default_prop_returns_correct_PascalUnit_record;
begin
  var R := List[TSnippetFormatID.PascalUnit];
  Assert.AreEqual(TSnippetFormatID.PascalUnit, R.ID);
end;

procedure TTestISnippetFormatList.Last_returns_last_item_in_list;
begin
  Assert.AreEqual(List.Items[High(TSnippetFormatID)], List.Last);
end;

procedure TTestISnippetFormatList.Setup;
begin
  List := TSnippetFormatList.Create;
end;

procedure TTestISnippetFormatList.TearDown;
begin

end;

initialization
  TDUnitX.RegisterTestFixture(TTestTSnippetFormat);
  TDUnitX.RegisterTestFixture(TTestISnippetFormatList);
end.
