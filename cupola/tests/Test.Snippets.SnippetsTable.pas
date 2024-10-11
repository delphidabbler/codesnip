{
  This unit is dedicated to public domain under the CC0 license.
  See https://creativecommons.org/public-domain/cc0/
}

unit Test.Snippets.SnippetsTable;

interface

uses
  DUnitX.TestFramework,

  System.SysUtils,
  System.DateUtils,

  CSLE.Exceptions,
  CSLE.Utils.Dates,
  CSLE.SourceCode.Language,
  CSLE.Snippets.ID,
  CSLE.Snippets.Snippet,
  CSLE.Snippets.Markup,
  CSLE.Snippets.Tag,
  CSLE.Snippets.Format,
  CSLE.Snippets.SnippetsTable;

type
  [TestFixture]
  TTestSnippetsTable = class
  strict private
    var
      S1, S2, S3, S4, SExtra: TSnippet;
      ID1, ID2, ID3, ID4, IDExtra: TSnippetID;
      Table0: TSnippetsTable;  // empty table
      Table4: TSnippetsTable;  // 4 item table
    const
      IDB1: TBytes = [1,2,3,4,5,6,7,8,9,10];
      IDB2: TBytes = [8,9,8,9,8,9,8,9,8,9,8,9,8,9,8];
      IDB3: TBytes = [42,56,142,156,250];
      IDB4: TBytes = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20];
      IDBExtra: TBytes = [100,101,102,103];
      IDBNotInTable4: TBytes = [2,2,3,4,5,6,7,8,9];
      S1Title = 'Snippet 1: Pascal function';
      S2Title = 'Snippet 2: C function';
      S3Title = 'Snippet 3: Pascal const';
      S4Title = 'Snippet 4: Pascal type';
      SExtraTitle  = 'Extra snippet not in 4 item table';
    function IDArraysMatch(const A, B: TArray<TSnippetID>): Boolean;
    function GetSnippetIDs(const A: TArray<TSnippet>): TArray<TSnippetID>;
  public

    [Test]
    procedure Count_is_zero_for_empty_table;
    // Following test depends on .Add working correctly in setup
    [Test]
    procedure Count_is_4_for_4_item_table;

    [Test]
    procedure IsEmpty_is_true_for_empty_table;
    // Following test depends on .Add working correctly in setup
    [Test]
    procedure IsEmpty_is_false_for_4_item_table;

    [Test]
    procedure Clear_leaves_empty_table_unchanged;
    [Test]
    procedure Clear_remove_all_entries_from_4_item_table;

    [Test]
    procedure enumerator_does_nothing_on_empty_table;
    [Test]
    procedure enumerator_processes_all_items_in_4_item_table;

    [Test]
    procedure Contains_always_returns_false_on_empty_table;
    [Test]
    procedure Contains_returns_false_when_snippet_id_not_in_4_item_table;
    [Test]
    procedure Contains_returns_true_when_snippet_id_is_in_4_item_table;

    [Test]
    procedure TryGet_gets_required_snippet_from_4_item_table_and_returns_true;
    [Test]
    procedure TryGet_returns_false_when_snippet_id_not_in_4_item_table;
    
    [Test]
    procedure Get_returns_required_snippet_from_4_item_table;
    [Test]
    procedure Get_raises_exception_when_snippet_id_not_in_4_item_table;

    [Test]
    procedure TryAdd_adding_item_missing_from_table_adds_item_to_table_and_returns_true;
    [Test]
    procedure TryAdd_adding_item_already_in_table_does_nothing_except_return_false;

    [Test]
    procedure Add_adding_item_missing_from_table_adds_item_to_table;
    [Test]
    procedure Add_adding_item_already_in_table_raises_exception;

    [Test]
    procedure TryDelete_existing_item_in_table_removes_it_and_returns_true;
    [Test]
    procedure TryDelete_an_item_not_in_table_does_nothing_except_return_false;
    
    [Test]
    procedure Delete_existing_item_from_table_removes_it;
    [Test]
    procedure Delete_all_items_from_table_leaves_table_empty;
    [Test]
    procedure Delete_an_item_missing_from_table_raises_exception;

    [Test]
    procedure TryUpdate_an_existing_item_in_a_table_updates_its_properties_and_return_true;
    [Test]
    procedure TryUpdate_an_item_not_in_a_table_does_nothing_except_return_false;
    
    [Test]
    procedure Update_an_existing_item_in_a_table_updates_its_properites;
    [Test]
    procedure Update_an_item_not_in_a_table_raises_exception;

    [Test]
    procedure AddOrUpdate_an_existing_item_in_table_updates_its_properties;
    [Test]
    procedure AddOrUpdate_an_item_not_in_table_adds_it;

    [Test]
    procedure FilterIDs_returns_ids_of_starred_snippets_in_4_item_table;
    [Test]
    procedure FilterIDs_returns_ids_of_freeform_snippets_in_4_item_table;
    [Test]
    procedure FilterIDs_returns_ids_of_snippets_where_language_is_not_C_in_4_item_table;
    [Test]
    procedure FilterIDs_returns_empty_array_for_snippets_with_5_xrefs_in_4_items_table;
    [Test]
    procedure FilterIDs_returns_empty_array_for_any_predicate_in_empty_table;

    [Test]
    procedure FilterSnippets_returns_starred_snippets_in_4_item_table;
    [Test]
    procedure FilterSnippets_returns_freeform_snippets_in_4_item_table;
    [Test]
    procedure FilterSnippets_returns_empty_array_where_language_is_Python_in_4_item_table;
    [Test]
    procedure FilterSnippets_returns_empty_array_for_any_predicate_in_empty_table;

    [Test]
    procedure GetAllIDs_returns_correct_ids_for_4_item_table;
    [Test]
    procedure GetAllIDs_returns_empty_array_for_empty_table;

    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
  end;

implementation

uses
  System.Generics.Collections,
  CSLE.TextData;

procedure TTestSnippetsTable.AddOrUpdate_an_existing_item_in_table_updates_its_properties;
begin
  var PreAddOrUpdateCount := Table4.Count;
  
  const ChangedTitle = 'Changed title';
  const ChangedDescriptionText = '<p>Changed description</p>';
  const ChangedDescription = TSnippetMarkup.Create(ChangedDescriptionText, TSnippetMarkupKind.REML);
  const ChangedStarred = True;

  var S := Table4.Get(ID2);

  Assert.AreNotEqual(ChangedTitle, S.Title, 'Pre-test: Snippet 2 .Title not same as changed');
  Assert.IsTrue(ChangedDescription <> S.Description, 'Pre-test: Snippet 2 .Description not same as changed');
  Assert.AreNotEqual(ChangedStarred, S.Starred, 'Pre-test: Snippet 2 .Starred not same as changed');

  S.Title := ChangedTitle;
  S.Description := ChangedDescription;
  S.Starred := ChangedStarred;

  Table4.AddOrUpdate(S);

  var SChanged := Table4.Get(ID2);

  Assert.AreEqual(ChangedTitle, SChanged.Title, '.Title changed');
  Assert.IsTrue(ChangedDescription = S.Description, '.Description changed');
  Assert.AreEqual(ChangedStarred, S.Starred, '.Starred changed');
  Assert.IsTrue(Table4.Contains(SChanged.ID), 'Updated snippet in table');
  Assert.AreEqual(PreAddOrUpdateCount, Table4.Count, 'Table size unchanged');
end;

procedure TTestSnippetsTable.AddOrUpdate_an_item_not_in_table_adds_it;
begin
  var PreAddOrUpdateCount := Table4.Count;
  Assert.IsFalse(Table4.Contains(IDExtra), 'Pre-test check: snippet not in table');
  Table4.AddOrUpdate(SExtra);
  Assert.IsTrue(Table4.Contains(SExtra.ID), 'Table now contains SExtra');
  Assert.AreEqual(PreAddOrUpdateCount + 1, Table4.Count, 'Table size increased by 1');
end;

procedure TTestSnippetsTable.Add_adding_item_already_in_table_raises_exception;
begin
  Assert.WillRaise(
    procedure
    begin
      Table4.Add(S2);
    end,
    ESnippetsTable
  );
end;

procedure TTestSnippetsTable.Add_adding_item_missing_from_table_adds_item_to_table;
begin
  Assert.IsFalse(Table4.Contains(IDExtra), 'Pre-test check: snippet not in table');
  var PreAddCount := Table4.Count;
  Table4.Add(SExtra);
  Assert.IsTrue(Table4.Contains(SExtra.ID), 'Table now contains SExtra');
  Assert.AreEqual(PreAddCount + 1, Table4.Count, 'Table size inceased by 1');
end;

procedure TTestSnippetsTable.Clear_leaves_empty_table_unchanged;
begin
  Assert.IsTrue(Table0.IsEmpty, 'Before .Clear check');
  Table0.Clear;
  Assert.IsTrue(Table0.IsEmpty, 'After .Clear check');
end;

procedure TTestSnippetsTable.Clear_remove_all_entries_from_4_item_table;
begin
  Assert.IsFalse(Table4.IsEmpty, 'Before .Clear check');
  Table4.Clear;
  Assert.IsTrue(Table4.IsEmpty, 'After .Clear check');
end;

procedure TTestSnippetsTable.Contains_always_returns_false_on_empty_table;
begin
  Assert.IsFalse(Table0.Contains(ID1), 'ID1 not in Empty table');
  Assert.IsFalse(Table0.Contains(ID4), 'ID4 not in Empty table');
end;

procedure TTestSnippetsTable.Contains_returns_false_when_snippet_id_not_in_4_item_table;
begin
  var ID := TSnippetID.Create(IDBNotInTable4);
  Assert.IsFalse(Table4.Contains(ID));
end;

procedure TTestSnippetsTable.Contains_returns_true_when_snippet_id_is_in_4_item_table;
begin
  Assert.IsTrue(Table4.Contains(ID1), 'ID1 in 4 item table');
  Assert.IsTrue(Table4.Contains(ID3), 'ID3 in 4 item table');
end;

procedure TTestSnippetsTable.Count_is_4_for_4_item_table;
begin
  Assert.AreEqual(NativeInt(4), Table4.Count);
end;

procedure TTestSnippetsTable.Count_is_zero_for_empty_table;
begin
  Assert.AreEqual(NativeInt(0), Table0.Count);
end;

procedure TTestSnippetsTable.Delete_all_items_from_table_leaves_table_empty;
begin
  var AllIDs := TList<TSnippetID>.Create;
  try
    for var S in Table4 do
      AllIDs.Add(S.ID);
    Assert.AreEqual(NativeInt(4), Table4.Count, 'Pre-test check: 4 items in table');
    Assert.AreEqual(NativeInt(4), AllIDs.Count, 'Pre-test check: 4 IDs copied from table');

    for var ID in AllIDs do
      Table4.Delete(ID);
    Assert.IsTrue(Table4.IsEmpty, 'Table now empty after deleting all entries');
  finally
    AllIDs.Free;
  end;
end;

procedure TTestSnippetsTable.Delete_an_item_missing_from_table_raises_exception;
begin
  Assert.WillRaise(
    procedure
    begin
      Table4.Delete(IDExtra);
    end,
    ESnippetsTable
  );
end;

procedure TTestSnippetsTable.Delete_existing_item_from_table_removes_it;
begin
  Assert.IsTrue(Table4.Contains(ID3), 'Pre-test check that ID3 in array');
  var PreDeleteCount := Table4.Count;
  Table4.Delete(ID3);
  Assert.IsFalse(Table4.Contains(ID3), 'ID3 no longer in array');
  Assert.AreEqual(PreDeleteCount - 1, Table4.Count, 'Removing ID3 reduces count');
end;

procedure TTestSnippetsTable.enumerator_does_nothing_on_empty_table;
begin
  var Count: Integer := 0;
  for var S in Table0 do
    Inc(Count);
  Assert.AreEqual(0, Count);
end;

procedure TTestSnippetsTable.enumerator_processes_all_items_in_4_item_table;
begin
  var ExpectedIDs: TArray<TSnippetID> := [S1.ID, S2.Id, S3.ID, S4.ID];
  var IDFound: TArray<Boolean> := [False, False, False, False];
  var Count: Integer := 0;

  var Success: Boolean;
  var ActualIds := TList<TSnippetID>.Create;
  try
    for var S in Table4 do
    begin
      ActualIds.Add(S.ID);
      Inc(Count);
    end;
    Success := IDArraysMatch(ExpectedIDs, ActualIds.ToArray);
  finally
    ActualIDs.Free;
  end;
  Assert.AreEqual(4, Count, 'Check 4 items enumerated');
  Assert.IsTrue(Success, 'Check all 4 snippets enumerated');
end;

procedure TTestSnippetsTable.FilterIDs_returns_empty_array_for_any_predicate_in_empty_table;
begin
  var IDs := Table0.FilterIDs(
    function (const S: TSnippet): Boolean
    begin
      Result := True;
    end
  );
  Assert.AreEqual(NativeInt(0), Length(IDs));
end;

procedure TTestSnippetsTable.FilterIDs_returns_empty_array_for_snippets_with_5_xrefs_in_4_items_table;
begin
  var IDs := Table4.FilterIDs(
    function (const S: TSnippet): Boolean
    begin
      Result := (NativeInt(5) = Length(S.XRefs));
    end
  );
  Assert.AreEqual(NativeInt(0), Length(IDs));
end;

procedure TTestSnippetsTable.FilterIDs_returns_ids_of_freeform_snippets_in_4_item_table;
begin
  var IDs := Table4.FilterIDs(
    function (const S: TSnippet): Boolean
    begin
      Result := S.Format = TSnippetFormatID.Freeform;
    end
  );
  // Snippet 2 is only one with Freeform format
  var Success := IDArraysMatch([ID2], IDs);
  Assert.IsTrue(Success);
end;

procedure TTestSnippetsTable.FilterIDs_returns_ids_of_snippets_where_language_is_not_C_in_4_item_table;
begin
  var IDs := Table4.FilterIDs(
    function (const S: TSnippet): Boolean
    begin
      Result := S.LanguageID <> TSourceCodeLanguageID.Create('C');
    end
  );
  // Only snippet 2 is C
  var Success := IDArraysMatch([ID1,ID3,ID4], IDs);
  Assert.IsTrue(Success);
end;

procedure TTestSnippetsTable.FilterIDs_returns_ids_of_starred_snippets_in_4_item_table;
begin
  var IDs := Table4.FilterIDs(
    function (const S: TSnippet): Boolean
    begin
      Result := S.Starred;
    end
  );
  // Snippets 1 & 4 are starred
  var Success := IDArraysMatch([ID1, ID4], IDs);
  Assert.IsTrue(Success);
end;

procedure TTestSnippetsTable.FilterSnippets_returns_empty_array_for_any_predicate_in_empty_table;
begin
  var Snippets := Table0.FilterSnippets(
    function (const S: TSnippet): Boolean
    begin
      Result := True;
    end
  );
  Assert.AreEqual(NativeInt(0), Length(Snippets));
end;

procedure TTestSnippetsTable.FilterSnippets_returns_empty_array_where_language_is_Python_in_4_item_table;
begin
  var Snippets := Table4.FilterSnippets(
    function (const S: TSnippet): Boolean
    begin
      Result := S.LanguageID <> TSourceCodeLanguageID.Create('Python');
    end
  );
  // No snippets are Python
  var Success := IDArraysMatch([ID1,ID2,ID3,ID4], GetSnippetIDs(Snippets));
  Assert.IsTrue(Success);
end;

procedure TTestSnippetsTable.FilterSnippets_returns_freeform_snippets_in_4_item_table;
begin
  var Snippets := Table4.FilterSnippets(
    function (const S: TSnippet): Boolean
    begin
      Result := S.Format = TSnippetFormatID.Freeform;
    end
  );
  // Snippet 2 is only one with Freeform format
  var Success := IDArraysMatch([ID2], GetSnippetIDs(Snippets));
  Assert.IsTrue(Success);
end;

procedure TTestSnippetsTable.FilterSnippets_returns_starred_snippets_in_4_item_table;
begin
  var Snippets := Table4.FilterSnippets(
    function (const S: TSnippet): Boolean
    begin
      Result := S.Starred;
    end
  );
  // Snippets 1 & 4 are starred
  var Success := IDArraysMatch([ID1, ID4], GetSnippetIDs(Snippets));
  Assert.IsTrue(Success);
end;

procedure TTestSnippetsTable.GetAllIDs_returns_correct_ids_for_4_item_table;
begin
  const KnownIDs: TArray<TSnippetID> = [ID1, ID2, ID3, ID4];
  var AllIDs := Table4.GetAllIDs;
  Assert.IsTrue(IDArraysMatch(KnownIDs, AllIDs));
end;

procedure TTestSnippetsTable.GetAllIDs_returns_empty_array_for_empty_table;
begin
  var A := Table0.GetAllIDs;
  Assert.AreEqual(NativeInt(0), Length(A));
end;

function TTestSnippetsTable.GetSnippetIDs(
  const A: TArray<TSnippet>): TArray<TSnippetID>;
begin
  var IDs := TList<TSnippetID>.Create;
  try
    for var S in A do
      IDs.Add(S.ID);
    Result := IDs.ToArray;
  finally
    IDs.Free;
  end;
end;

procedure TTestSnippetsTable.Get_raises_exception_when_snippet_id_not_in_4_item_table;
begin
  Assert.WillRaise(
    procedure
    begin
      var S := Table4.Get(IDExtra);
    end,
    ESnippetsTable
  );
end;

procedure TTestSnippetsTable.Get_returns_required_snippet_from_4_item_table;
begin
  var GotS2 := Table4.Get(ID2);
  var GotS4 := Table4.Get(ID4);
  Assert.IsTrue(ID2 = GotS2.ID, 'Get succeeds for ID2');
  Assert.IsTrue(ID4 = GotS4.ID, 'Get succeeds for ID4');
  Assert.AreEqual(S2Title, GotS2.Title, 'Title property as expected for snippet 2');
  Assert.AreEqual(S4Title, GotS4.Title, 'Title property as expected for snippet 4');
end;

function TTestSnippetsTable.IDArraysMatch(const A,
  B: TArray<TSnippetID>): Boolean;
begin
  if Length(A) <> Length(B) then
    Exit(False);
  var IDMatches: TArray<Boolean>;
  SetLength(IDMatches, Length(A));
  for var I := Low(IDMatches) to High(IDMatches) do
    IDMatches[I] := False;
  for var ID in B do
  begin
    for var I := Low(IDMatches) to High(IDMatches) do
    begin
      if ID = A[I] then
      begin
        IDMatches[I] := True;
        Break;
      end;
    end;
  end;
  Result := True;
  for var Flag in IDMatches do
  begin
    if not Flag then
    begin
      Result := False;
      Break;
    end;
  end;
end;

procedure TTestSnippetsTable.IsEmpty_is_false_for_4_item_table;
begin
  Assert.IsFalse(Table4.IsEmpty);
end;

procedure TTestSnippetsTable.IsEmpty_is_true_for_empty_table;
begin
  Assert.IsTrue(Table0.IsEmpty);
end;

procedure TTestSnippetsTable.Setup;
begin
  ID1 := TSnippetID.Create(IDB1);
  S1 := TSnippet.Create(ID1);
  S1.Title := S1Title;
  S1.Description := TSnippetMarkup.Create('The description of snippet 1 as plain text', TSnippetMarkupKind.Plain);
  S1.SourceCode :=
    '''
    function f1(P1: string): Boolean;
    begin
      Result := P1.Length = 0;
    end;
    ''';
  S1.LanguageID := TSourceCodeLanguageID.Create(TSourceCodeLanguageID.PascalLanguageID);
  S1.RequiredModules := TArray<string>.Create('Windows','SysUtils');
  S1.Notes := TSnippetMarkup.Create('Some notes for snippet 1', TSnippetMarkupKind.Plain);
  S1.Format := TSnippetFormatID.PascalRoutine;
  S1.Tags.Include(TTag.Create('Tag1'));
  S1.Tags.Include(TTag.Create('Tag2'));
  S1.Starred := True;


  ID2 := TSnippetID.Create(IDB2);
  S2 := TSnippet.Create(ID2);
  S2.Title := S2Title;
  S2.Description := TSnippetMarkup.Create('<p>The description of snippet 2 as REML 4</p>', TSnippetMarkupKind.REML, 4);
  S2.SourceCode :=
    '''
    int foo()
    {
      puts("Hello world");
    };
    ''';
  S2.LanguageID := TSourceCodeLanguageID.Create('C');
  S2.Modified := TUTCDateTime.Create(TDateTime.NowUTC.IncMonth(1), True);
  S2.Format := TSnippetFormatID.Freeform;
  S2.Tags.Include(TTag.Create('Tag1'));

  ID3 := TSnippetID.Create(IDB3);
  S3 := TSnippet.Create(ID3);
  S3.Title := S3Title;
  S3.Description := TSnippetMarkup.Create('<p>The description of snippet 2 as REML 6</p>', TSnippetMarkupKind.REML, 6);
  S3.SourceCode :=
    '''
    const
      X = 42;
    ''';
  S3.LanguageID := TSourceCodeLanguageID.Create(TSourceCodeLanguageID.PascalLanguageID);
  S3.Format := TSnippetFormatID.PascalConst;

  ID4 := TSnippetID.Create(IDB4);
  S4 := TSnippet.Create(ID4);
  S4.Title := S4Title;
  S4.Description := TSnippetMarkup.Create('The description of snippet 2 as plain text', TSnippetMarkupKind.Plain);
  S4.SourceCode :=
    '''
    type
      TFoo = type Int32;
    ''';
  S4.LanguageID := TSourceCodeLanguageID.Create(TSourceCodeLanguageID.PascalLanguageID);
  S4.Format := TSnippetFormatID.PascalType;

  S4.RequiredSnippets := TArray<TSnippetID>.Create(S3.ID, S4.ID);
  S4.XRefs := TArray<TSnippetID>.Create(S3.ID);
  S4.Starred := True;

  IDExtra := TSnippetID.Create(IDBExtra);
  SExtra := TSnippet.Create(IDExtra);
  SExtra.Title := SExtraTitle;
  SExtra.Description := TSnippetMarkup.Create('This snippet is not contained in 4 item table', TSnippetMarkupKind.Plain);
  SExtra.SourceCode :=
    '''
    def my_function():
      print("Hello from a function")
    ''';
  SExtra.LanguageID := TSourceCodeLanguageID.Create('Python');
  SExtra.Format := TSnippetFormatID.Freeform;

  S1.RequiredSnippets := TArray<TSnippetID>.Create(S3.ID, SExtra.ID);
  SExtra.XRefs := TArray<TSnippetID>.Create(S3.ID);
  SExtra.Starred := True;


  Table0 := TSnippetsTable.Create;

  Table4 := TSnippetsTable.Create;
  Table4.Add(S1);
  Table4.Add(S2);
  Table4.Add(S3);
  Table4.Add(S4);
end;

procedure TTestSnippetsTable.TearDown;
begin
  Table4.Free;
  Table0.Free;
end;

procedure TTestSnippetsTable.TryAdd_adding_item_already_in_table_does_nothing_except_return_false;
begin
  var PreTryAddCount := Table4.Count;
  var AddResult := Table4.TryAdd(S3);
  Assert.IsFalse(AddResult, 'TryAdd fails to add snippet 3'); 
  Assert.IsTrue(Table4.Contains(S3.ID), 'TryAdd table leaves snippet 3 in table');
  Assert.AreEqual(PreTryAddCount, Table4.Count, 'Number of snippets in table remains unchanged');
end;

procedure TTestSnippetsTable.TryAdd_adding_item_missing_from_table_adds_item_to_table_and_returns_true;
begin
  var PreTryAddCount := Table4.Count;
  var AddResult := Table4.TryAdd(SExtra);
  Assert.IsTrue(Table4.Contains(SExtra.ID), 'TryAdd adds item to table');
  Assert.IsTrue(AddResult, 'TryAdd returns true');
  Assert.AreEqual(PreTryAddCount + 1, Table4.Count, 'Number of snippets in table increases by 1');
end;

procedure TTestSnippetsTable.TryDelete_an_item_not_in_table_does_nothing_except_return_false;
begin
  var PreTryDeleteCount := Table4.Count;
  var DeleteResult := Table4.TryDelete(IDExtra);
  Assert.IsFalse(DeleteResult, 'TryDelete fails to delete'); 
  Assert.AreEqual(PreTryDeleteCount, Table4.Count, 'Number of snippets in table remains unchanged');
end;

procedure TTestSnippetsTable.TryDelete_existing_item_in_table_removes_it_and_returns_true;
begin
  Assert.IsTrue(Table4.Contains(ID2), 'Pre-test check that ID2 in array');
  var OrigCount := Table4.Count;
  
  var DeleteRes := Table4.TryDelete(ID2);

  Assert.IsTrue(DeleteRes, 'True returned');
  Assert.IsFalse(Table4.Contains(ID2), 'ID2 no longer in array');
  Assert.AreEqual(OrigCount - 1, Table4.Count, 'Removing ID3 reduces count');
end;

procedure TTestSnippetsTable.TryGet_gets_required_snippet_from_4_item_table_and_returns_true;
begin
  var GotS3: TSnippet;
  var S3Res := Table4.TryGet(ID3, GotS3);
  Assert.IsTrue(S3Res, 'TryGet succeeds for ID3');
  Assert.IsTrue(ID3 = GotS3.ID, 'TryGet gets snippet 3 as expected');
  Assert.AreEqual(S3Title, GotS3.Title, 'Title property as expected for snippet 3');
end;

procedure TTestSnippetsTable.TryGet_returns_false_when_snippet_id_not_in_4_item_table;
begin
  var S: TSnippet;
  Assert.IsFalse(Table4.TryGet(IDExtra, S)); // S is not defined, so can't be checked
end;

procedure TTestSnippetsTable.TryUpdate_an_existing_item_in_a_table_updates_its_properties_and_return_true;
begin
  const ChangedTitle = 'Changed title';
  const ChangedDescriptionText = '<p>Changed description</p>';
  const ChangedDescription = TSnippetMarkup.Create(ChangedDescriptionText, TSnippetMarkupKind.REML);
  const ChangedStarred = True;

  var S := Table4.Get(ID2);
  Assert.AreNotEqual(ChangedTitle, S.Title, 'Pre-test: Snippet 2 .Title not same as changed');
  Assert.IsTrue(ChangedDescription <> S.Description, 'Pre-test: Snippet 2 .Description not same as changed');
  Assert.AreNotEqual(ChangedStarred, S.Starred, 'Pre-test: Snippet 2 .Starred not same as changed');

  S.Title := ChangedTitle;
  S.Description := ChangedDescription;
  S.Starred := ChangedStarred;

  var PreTryUpdateCount := Table4.Count;
  var TryUpdateResult := Table4.TryUpdate(S);
  var SChanged := Table4.Get(ID2);

  Assert.AreEqual(ChangedTitle, SChanged.Title, '.Title changed');
  Assert.IsTrue(ChangedDescription = S.Description, '.Description changed');
  Assert.AreEqual(ChangedStarred, S.Starred, '.Starred changed');
  Assert.IsTrue(TryUpdateResult, 'TryUpdate returned True');
  Assert.AreEqual(PreTryUpdateCount, Table4.Count, 'Table size unchanged');
end;

procedure TTestSnippetsTable.TryUpdate_an_item_not_in_a_table_does_nothing_except_return_false;
begin
  var PreTryUpdateCount := Table4.Count;
  var TryUpdateResult := Table4.TryUpdate(SExtra);
  Assert.IsFalse(TryUpdateResult, 'TryUpdate returns False');
  Assert.AreEqual(PreTryUpdateCount, Table4.Count, 'Table size unchanged');
end;

procedure TTestSnippetsTable.Update_an_existing_item_in_a_table_updates_its_properites;
begin
  const ChangedTitle = 'Changed title';
  const ChangedDescriptionText = '<p>Changed description</p>';
  const ChangedDescription = TSnippetMarkup.Create(ChangedDescriptionText, TSnippetMarkupKind.REML);
  const ChangedStarred = True;

  var S := Table4.Get(ID2);
  Assert.AreNotEqual(ChangedTitle, S.Title, 'Pre-test: Snippet 2 .Title not same as changed');
  Assert.IsTrue(ChangedDescription <> S.Description, 'Pre-test: Snippet 2 .Description not same as changed');
  Assert.AreNotEqual(ChangedStarred, S.Starred, 'Pre-test: Snippet 2 .Starred not same as changed');

  S.Title := ChangedTitle;
  S.Description := ChangedDescription;
  S.Starred := ChangedStarred;

  var PreTryUpdateCount := Table4.Count;
  Table4.Update(S);
  var SChanged := Table4.Get(ID2);

  Assert.AreEqual(ChangedTitle, SChanged.Title, '.Title changed');
  Assert.IsTrue(ChangedDescription = S.Description, '.Description changed');
  Assert.AreEqual(ChangedStarred, S.Starred, '.Starred changed');
  Assert.AreEqual(PreTryUpdateCount, Table4.Count, 'Table size unchanged');
end;

procedure TTestSnippetsTable.Update_an_item_not_in_a_table_raises_exception;
begin
  Assert.WillRaise(
    procedure
    begin
      Table4.Update(SExtra);
    end,
    ESnippetsTable
  )
end;

initialization

  TDUnitX.RegisterTestFixture(TTestSnippetsTable);

end.
