{
 * This unit is dedicated to public domain under the CC0 license.
 * See https://creativecommons.org/public-domain/cc0/
}

unit Test.Snippets.ID;

interface

uses
  System.SysUtils,

  DUnitX.TestFramework,

  CSLE.Snippets.ID;

type
  [TestFixture]
  TTestSnippetID = class
  strict private
    var
      EmptyArray, TooBigArray, MaximumArray: TBytes;
      NullID, MaximumSizeID: TSnippetID;
      ID1, ID1Eq, ID2, ID3, ID4, ID5, ID6, ID7, ID7Eq, ID8, ID9, ID10: TSnippetID;
      ID1A, ID2A, ID3A, ID4A, ID5A, ID6A, ID7A, ID8A, ID9A, ID10A: TBytes;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    // *** The order of the Test*** methods is significant

    [Test]
    procedure Create_ctor_with_non_empty_byte_array_succeeds;
    [Test]
    procedure Create_ctor_with_empty_byte_array_succeeds;
    [Test]
    procedure Create_ctor_with_too_large_byte_array_raises_exception;


    // This also tests default ctor
    [Test]
    procedure IsNull_true_for_default_ctor;
    [Test]
    procedure IsNull_false_following_Create_with_non_empty_byte_array;

    [Test]
    procedure CreateNew_creates_valid_non_null_id;

    [Test]
    procedure Compare_works_as_expected;

    [Test]
    procedure Equal_op_with_non_null_IDs;
    [Test]
    procedure Equal_op_with_null_IDs_is_true;
    [Test]
    procedure Equal_op_with_new_IDs_is_false;

    [Test]
    procedure NotEqual_op_with_non_null_IDs;
    [Test]
    procedure NotEqual_op_with_null_IDs_is_false;
    [Test]
    procedure NotEqual_op_with_new_IDs_is_true;

    [Test]
    procedure ToByteArray_returns_expected_value;

    [Test]
    procedure ToString_returns_expected_value;

    [Test]
    procedure Assign_op_creates_equal_IDs;

    // TSnippetID.TComparator tests
    // can't think of a sensible way to test Hash function without simply
    // replicating its internals
    [Test]
    procedure comparator_Compare_gives_expected_results;
    [Test]
    procedure comparator_Equals_with_non_null_IDs_gives_expected_results;
    [Test]
    procedure comparator_Equals_with_null_IDs_is_true;
    [Test]
    procedure comparator_Equals_with_new_IDs_is_false;
  end;

implementation

uses
  System.Types,

  CSLE.Exceptions;

procedure TTestSnippetID.Assign_op_creates_equal_IDs;
begin
  var A := ID2;
  Assert.IsTrue(ID2 = A);
  var B := NullID;
  Assert.IsTrue(NullID = B);
  var C := ID7; // 1 byte
  Assert.IsTrue(ID7 = C);
end;

procedure TTestSnippetID.comparator_Compare_gives_expected_results;
begin
  var Comp := TSnippetID.TComparator.Create;
  Assert.AreEqual(1, Comp.Compare(ID2, ID1), 'ID2 > ID1');
  Assert.AreEqual(-1, Comp.Compare(ID2, ID3), 'ID2 < ID3');
  Assert.AreEqual(0, Comp.Compare(ID1, ID1Eq), 'ID1 = ID1Eq');
  Assert.AreEqual(1, Comp.Compare(ID1, ID5), 'ID1 > ID5');
  Assert.AreEqual(0, Comp.Compare(ID7, ID7Eq), 'ID7 = ID7Eq');
  Assert.AreEqual(1, Comp.Compare(ID10, ID9), 'ID10 > ID9');
  Assert.AreEqual(-1, Comp.Compare(ID9, ID10), 'ID9 < ID10');
  Assert.AreEqual(0, Comp.Compare(NullID, NullID), 'NullID = NullID');
  Assert.AreEqual(1, Comp.Compare(ID6, ID7), 'ID6 > ID7');
end;

procedure TTestSnippetID.comparator_Equals_with_new_IDs_is_false;
begin
  var Comp := TSnippetID.TComparator.Create;
  var Left := TSnippetID.CreateNew;
  var Right := TSnippetID.CreateNew;
  Assert.IsFalse(Comp.Equals(Left, Right));
end;

procedure TTestSnippetID.comparator_Equals_with_non_null_IDs_gives_expected_results;
begin
  var Comp := TSnippetID.TComparator.Create;
  Assert.IsTrue(Comp.Equals(ID1, ID1Eq), 'ID1=ID1Eq');
  Assert.IsTrue(Comp.Equals(ID7, ID7Eq), 'ID7=ID7Eq (1 byte array)');
  Assert.IsFalse(Comp.Equals(ID7, ID6), 'ID7<>ID6 (ID7 1 byte, ID6 2 bytes, same 1st byte)');
  Assert.IsFalse(Comp.Equals(ID1, ID2), 'ID1<>ID2 (same length)');
  Assert.IsFalse(Comp.Equals(ID1, ID3), 'ID1<>ID3 (same length');
  Assert.IsFalse(Comp.Equals(ID1, ID4), 'ID1<>ID4 (4 longer than 1 but equal to end');
  Assert.IsFalse(Comp.Equals(ID1, ID5), 'ID1<>ID5 (5 shorter than 1 but equal to end');
  Assert.IsFalse(Comp.Equals(ID8, ID9), 'ID8<>ID9 (same length - 2 GUIDs)');
end;

procedure TTestSnippetID.comparator_Equals_with_null_IDs_is_true;
begin
  var Comp := TSnippetID.TComparator.Create;
  var Left, Right: TSnippetID;   // null IDs
  Assert.IsTrue(Comp.Equals(Left, Right));
end;

procedure TTestSnippetID.Compare_works_as_expected;
begin
  Assert.AreEqual(1, TSnippetID.Compare(ID2, ID1), 'ID2 > ID1');
  Assert.AreEqual(-1, TSnippetID.Compare(ID2, ID3), 'ID2 < ID3');
  Assert.AreEqual(0, TSnippetID.Compare(ID1, ID1Eq), 'ID1 = ID1Eq');
  Assert.AreEqual(1, TSnippetID.Compare(ID1, ID5), 'ID1 > ID5');
  Assert.AreEqual(0, TSnippetID.Compare(ID7, ID7Eq), 'ID7 = ID7Eq');
  Assert.AreEqual(1, TSnippetID.Compare(ID10, ID9), 'ID10 > ID9');
  Assert.AreEqual(-1, TSnippetID.Compare(ID9, ID10), 'ID9 < ID10');
  Assert.AreEqual(0, TSnippetID.Compare(NullID, NullID), 'NullID = NullID');
  Assert.AreEqual(1, TSnippetID.Compare(ID6, ID7), 'ID6 > ID7');
end;

procedure TTestSnippetID.CreateNew_creates_valid_non_null_id;
begin
  var ID := TSnippetID.CreateNew;
  Assert.IsFalse(ID.IsNull);
end;

procedure TTestSnippetID.Create_ctor_with_empty_byte_array_succeeds;
begin
  {$OPTIMIZATION OFF}
  var ID: TSnippetID;
  Assert.WillNotRaise(
    procedure
    begin
      ID := TSnippetID.Create(EmptyArray);
    end,
    Exception
  );
  {$OPTIMIZATION ON}
end;

procedure TTestSnippetID.Create_ctor_with_non_empty_byte_array_succeeds;
begin
  {$OPTIMIZATION OFF}
  var ID: TSnippetID;
  Assert.WillNotRaise(
    procedure
    begin
      ID := TSnippetID.Create(MaximumArray);
    end,
    Exception
  );
  {$OPTIMIZATION ON}
end;

procedure TTestSnippetID.Create_ctor_with_too_large_byte_array_raises_exception;
begin
  {$OPTIMIZATION OFF}
  var ID: TSnippetID;
  Assert.WillRaise(
    procedure
    begin
      ID := TSnippetID.Create(TooBigArray);
    end,
    EUnexpected
  );
  {$OPTIMIZATION ON}
end;

procedure TTestSnippetID.Equal_op_with_new_IDs_is_false;
begin
  var Left := TSnippetID.CreateNew;
  var Right := TSnippetID.CreateNew;
  Assert.IsFalse(Left = Right);
end;

procedure TTestSnippetID.Equal_op_with_non_null_IDs;
begin
  Assert.IsTrue(ID1 = ID1Eq, 'ID1=ID1Eq');
  Assert.IsTrue(ID7 = ID7Eq, 'ID7=ID7Eq (1 byte array)');
  Assert.IsFalse(ID7 = ID6, 'ID7<>ID6 (ID7 1 byte, ID6 2 bytes, same 1st byte)');
  Assert.IsFalse(ID1 = ID2, 'ID1<>ID2 (same length)');
  Assert.IsFalse(ID1 = ID3, 'ID1<>ID3 (same length');
  Assert.IsFalse(ID1 = ID4, 'ID1<>ID4 (4 longer than 1 but equal to end');
  Assert.IsFalse(ID1 = ID5, 'ID1<>ID5 (5 shorter than 1 but equal to end');
  Assert.IsFalse(ID8 = ID9, 'ID8<>ID9 (same length - 2 GUIDs)');
end;

procedure TTestSnippetID.Equal_op_with_null_IDs_is_true;
begin
  var Left, Right: TSnippetID;   // null IDs
  Assert.IsTrue(Left = Right);
end;

procedure TTestSnippetID.IsNull_false_following_Create_with_non_empty_byte_array;
begin
  Assert.IsFalse(ID1.IsNull);
end;

procedure TTestSnippetID.IsNull_true_for_default_ctor;
begin
  var ID: TSnippetID; // should create empty ID
  Assert.IsTrue(ID.IsNull);
end;

procedure TTestSnippetID.NotEqual_op_with_new_IDs_is_true;
begin
  var Left := TSnippetID.CreateNew;
  var Right := TSnippetID.CreateNew;
  Assert.IsTrue(Left <> Right);
end;

procedure TTestSnippetID.NotEqual_op_with_non_null_IDs;
begin
  Assert.IsFalse(ID1 <> ID1Eq, 'ID1=ID1Eq');
  Assert.IsFalse(ID7 <> ID7Eq, 'ID7=ID7Eq (1 byte array)');
  Assert.IsTrue(ID7 <> ID6, 'ID7<>ID6 (ID7 1 byte, ID6 2 bytes, same 1st byte)');
  Assert.IsTrue(ID1 <> ID2, 'ID1<>ID2 (same length)');
  Assert.IsTrue(ID1 <> ID3, 'ID1<>ID3 (same length');
  Assert.IsTrue(ID1 <> ID4, 'ID1<>ID4 (4 longer than 1 but equal to end');
  Assert.IsTrue(ID1 <> ID5, 'ID1<>ID5 (5 shorter than 1 but equal to end');
  Assert.IsTrue(ID8 <> ID9, 'ID8<>ID9 (same length - 2 GUIDs)');
end;

procedure TTestSnippetID.NotEqual_op_with_null_IDs_is_false;
begin
  var Left, Right: TSnippetID;   // null IDs
  Assert.IsFalse(Left <> Right);
end;

procedure TTestSnippetID.Setup;
begin
  SetLength(EmptyArray, 0);
  SetLength(TooBigArray, TSnippetID.MaxIDSize + 1);
  for var X := 0 to Pred(Length(TooBigArray)) do
    TooBigArray[X] := X;
  SetLength(MaximumArray, TSnippetID.MaxIDSize);
  for var X := 0 to Pred(Length(MaximumArray)) do
    MaximumArray[X] := X;
  ID1A := TBytes.Create(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16);
  ID2A := TBytes.Create(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,17);
  ID3A := TBytes.Create(1,2,3,4,5,6,7,9,9,10,11,12,13,14,15,16);
  ID4A := TBytes.Create(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18);
  ID5A := TBytes.Create(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15);
  ID6A := TBytes.Create($7F, $FF);
  ID7A := TBytes.Create($7F);
  ID8A := TGUID.Create('{24FA0E57-9B46-4811-ADA9-701A7EBE18B2}').ToByteArray(TEndian.Big);
  ID9A := TGUID.Create('{56F06640-2E6D-4D38-B4CF-BC92BE52DAE1}').ToByteArray(TEndian.Big);
  ID10A := TGUID.Create('{EAC0C556-26B7-4910-B85E-79310183AA74}').ToByteArray(TEndian.Big);

  ID1 := TSnippetID.Create(ID1A);
  ID1Eq := TSnippetID.Create(ID1A);
  ID2 := TSnippetID.Create(ID2A);
  ID3 := TSnippetID.Create(ID3A);
  ID4 := TSnippetID.Create(ID4A);
  ID5 := TSnippetID.Create(ID5A);
  ID6 := TSnippetID.Create(ID6A);
  ID7 := TSnippetID.Create(ID7A);
  ID7Eq := TSnippetID.Create(ID7A);
  ID8 := TSnippetID.Create(ID8A);
  ID9 := TSnippetID.Create(ID9A);
  ID10 := TSnippetID.Create(ID10A);

  NullID := TSnippetID.Create(EmptyArray);
  MaximumSizeID := TSnippetID.Create(MaximumArray);
end;

procedure TTestSnippetID.TearDown;
begin
end;

procedure TTestSnippetID.ToByteArray_returns_expected_value;
begin
  Assert.AreEqual(ID1A, ID1.ToByteArray, 'ID1');
  Assert.AreEqual(ID7A, ID7.ToByteArray, 'ID7');
  Assert.AreEqual(ID8A, ID8.ToByteArray, 'ID8');
  Assert.AreEqual(EmptyArray, NullID.ToByteArray, 'NullID');
  Assert.AreEqual(MaximumArray, MaximumSizeID.ToByteArray, 'MaximumSizeID');
end;

procedure TTestSnippetID.ToString_returns_expected_value;
begin
  Assert.AreEqual('0102030405060708090A0B0C0D0E0F10', ID1.ToString, 'ID1');
  Assert.AreEqual('24FA0E579B464811ADA9701A7EBE18B2', ID8.ToString, 'ID8');
  Assert.AreEqual('7F', ID7.ToString, 'ID7');
  Assert.AreEqual('', NullID.ToString, 'NullID');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestSnippetID);
end.
