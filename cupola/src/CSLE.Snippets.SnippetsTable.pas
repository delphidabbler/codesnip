{
  This Source Code Form is subject to the terms of the Mozilla Public License,
  v. 2.0. If a copy of the MPL was not distributed with this file, You can
  obtain one at https://mozilla.org/MPL/2.0/

  Copyright (C) 2024, Peter Johnson (gravatar.com/delphidabbler).

  Encapsulates a table of snippets indexed by snippet ID.

  NOTE:
    This unit is adapted and extended from code taken from the CodeSnip Pavilion
    branch's CS.Database.SnippetsTable unit.
    See https://tinyurl.com/yc3tvzdu
}

unit CSLE.Snippets.SnippetsTable;

interface

uses
  System.Generics.Collections,
  CSLE.Exceptions,
  CSLE.Snippets.ID,
  CSLE.Snippets.Snippet;

type
  TSnippetsTableFilterPredicate = reference to function(
    const ASnippet: TSnippet): Boolean;

  TSnippetsTable = class(TObject)
  strict private
    var
      fTable: TDictionary<TSnippetID,TSnippet>;
  public
    type
      TSnippetEnumerator = TDictionary<TSnippetID,TSnippet>.TValueEnumerator;
  public
    constructor Create;

    destructor Destroy; override;

    ///  <summary>Returns an enumerator that iterates over all the snippets in
    ///  the table.</summary>
    function GetEnumerator: TSnippetEnumerator;

    ///  <summary>Checks if the table contains a snippet with the given ID.
    ///  </summary>
    function Contains(const ASnippetID: TSnippetID): Boolean;

    ///  <summary>Gets the snippet with the given ID from the table.</summary>
    ///  <exception><c>ESnippetsTable</c> raised if <c>ASnippetID</c> is not in
    ///  the table.</exception>
    function Get(const ASnippetID: TSnippetID): TSnippet;

    ///  <summary>Checks if the snippet with the given ID is in the table. If so
    ///  the snippet is passed out in <c>ASnippet</c> and <c>True</c> is
    ///  returned. If there is no such snippet then <c>ASnippet</c> is not
    ///  defined and <c>False</c> is returned.</summary>
    function TryGet(const ASnippetID: TSnippetID; out ASnippet: TSnippet):
      Boolean;

    ///  <summary>Gets an array of the IDs of all snippets in the table.
    ///  </summary>
    function GetAllIDs: TArray<TSnippetID>;

    ///  <summary>Gets an array of the IDs of snippets in the table for which
    ///  the given predicate returns true.</summary>
    function FilterIDs(const APredicate: TSnippetsTableFilterPredicate):
      TArray<TSnippetID>;

    ///  <summary>Gets an array of snippets in the table for which the given
    ///  predicate returns true.</summary>
    function FilterSnippets(const APredicate: TSnippetsTableFilterPredicate):
      TArray<TSnippet>;

    ///  <summary>Adds the given snippet to the table.</summary>
    ///  <exception><c>ESnippetsTable</c> raised if a snippet with the same ID
    ///  is already in the table.</exception>
    procedure Add(const ASnippet: TSnippet);

    ///  <summary>Attempts to add <c>ASnippet</c> to the table. Succeeds and
    ///  returns <c>True</c> if a snippet with the same ID as <c>ASnippet</c> is
    ///  not already in the table. Returns <c>False</c> and does nothing
    ///  otherwise.</summary>
    function TryAdd(const ASnippet: TSnippet): Boolean;

    ///  <summary>Updates the properties of the given snippet in the table.
    ///  </summary>
    ///  <exception><c>ESnippetsTable</c> if a snippet with the same <c>ID</c>
    ///  property as <c>ASnippet</c> is not in the table.</exception>
    procedure Update(const ASnippet: TSnippet);

    ///  <summary>Attempts to add updata the properties of the given snippet in
    ///  the table. If the snippet exists in the table then it is updated and
    ///  <c>True</c> is returned. Returns <c>False</c> and does nothing
    ///  otherwise.</summary>
    function TryUpdate(const ASnippet: TSnippet): Boolean;

    ///  <summary>Ensures that an up to date entry exists in the table for the
    ///  given snippet. If a snippet with the same ID is present in the table
    ///  then its properties are update to those of the give snippet. If there
    ///  is no such snippet in the table the given snippet is added.</summary>
    procedure AddOrUpdate(const ASnippet: TSnippet);

    ///  <summary>Deletes the snippet with the given ID from the table.
    ///  </summary>
    ///  <exception><c>ESnippetsTable</c> raised if <c>ASnippetID</c> is not in
    ///  the table.</exception>
    procedure Delete(const ASnippetID: TSnippetID);

    ///  <summary>Attempts to delete snippert with the given ID from the table.
    ///  If such a snippet is in the table it is deleted and <c>True</c> is
    ///  returned, otherwise the table is left unchanged and <c>False</c> is
    ///  returned.</summary>
    function TryDelete(const ASnippetID: TSnippetID): Boolean;

    ///  <summary>Clears the table.</summary>
    procedure Clear;

    ///  <summary>Returns the number of snippets in the table.</summary>
    function Count: NativeInt;

    ///  <summary>Checks whether the table is empty.</summary>
    function IsEmpty: Boolean;
  end;

  ESnippetsTable = class(EExpected);

implementation

uses
  System.SysUtils;

{ TSnippetsTable }

procedure TSnippetsTable.Add(const ASnippet: TSnippet);
begin
  if not TryAdd(ASnippet) then
    raise ESnippetsTable.Create(
      'Attempt to add duplicate snippet to table'
    );
end;

procedure TSnippetsTable.AddOrUpdate(const ASnippet: TSnippet);
begin
  if not TryAdd(ASnippet) then
    Update(ASnippet);
end;

procedure TSnippetsTable.Clear;
begin
  fTable.Clear;
end;

function TSnippetsTable.Contains(const ASnippetID: TSnippetID): Boolean;
begin
  Result := fTable.ContainsKey(ASnippetID);
end;

function TSnippetsTable.Count: NativeInt;
begin
  Result := fTable.Count;
end;

constructor TSnippetsTable.Create;
begin
  inherited Create;
  fTable := TDictionary<TSnippetID,TSnippet>.Create(
    TSnippetID.TComparator.Create
  );
end;

procedure TSnippetsTable.Delete(const ASnippetID: TSnippetID);
begin
  if not TryDelete(ASnippetID) then
    raise ESnippetsTable.Create(
      'Attempt to delete snippet not contained in table'
    );
end;

destructor TSnippetsTable.Destroy;
begin
  fTable.Free;
  inherited;
end;

function TSnippetsTable.FilterIDs(
  const APredicate: TSnippetsTableFilterPredicate): TArray<TSnippetID>;
begin
  var IDs := TList<TSnippetID>.Create;
  try
    for var Snippet in fTable.Values do
      if APredicate(Snippet) then
        IDs.Add(Snippet.ID);
    Result := IDs.ToArray;
  finally
    IDs.Free;
  end;
end;

function TSnippetsTable.FilterSnippets(
  const APredicate: TSnippetsTableFilterPredicate): TArray<TSnippet>;
begin
  var Snippets := TList<TSnippet>.Create;
  try
    for var Snippet in fTable.Values do
      if APredicate(Snippet) then
        Snippets.Add(Snippet);
    Result := Snippets.ToArray;
  finally
    Snippets.Free;
  end;
end;

function TSnippetsTable.Get(const ASnippetID: TSnippetID): TSnippet;
begin
  if not TryGet(ASnippetID, Result) then
    raise ESnippetsTable.Create(
      'Attempt to get snippet that doesn''t exist in table'
    );
end;

function TSnippetsTable.GetAllIDs: TArray<TSnippetID>;
begin
  Result := FilterIDs(
    function(const ASnippet: TSnippet): Boolean
    begin
      Result := True;
    end
  );
end;

function TSnippetsTable.GetEnumerator: TSnippetEnumerator;
begin
  Result := fTable.Values.GetEnumerator;
end;

function TSnippetsTable.IsEmpty: Boolean;
begin
  Result := fTable.IsEmpty;
end;

function TSnippetsTable.TryAdd(const ASnippet: TSnippet): Boolean;
begin
  Result := not fTable.ContainsKey(ASnippet.ID);
  if Result then
    fTable.Add(ASnippet.ID, ASnippet);
end;

function TSnippetsTable.TryDelete(const ASnippetID: TSnippetID): Boolean;
begin
  Result := fTable.ContainsKey(ASnippetID);
  if Result then
    fTable.Remove(ASnippetID);
end;

function TSnippetsTable.TryGet(const ASnippetID: TSnippetID;
  out ASnippet: TSnippet): Boolean;
begin
  Result := fTable.ContainsKey(ASnippetID);
  if Result then
    ASnippet := fTable[ASnippetID];
end;

function TSnippetsTable.TryUpdate(const ASnippet: TSnippet): Boolean;
begin
  Result := fTable.ContainsKey(ASnippet.ID);
  if Result then
    fTable[ASnippet.ID] := ASnippet;
end;

procedure TSnippetsTable.Update(const ASnippet: TSnippet);
begin
  if not TryUpdate(ASnippet) then
    raise ESnippetsTable.Create(
      'Attempt to update snippet not contained in table'
    );
end;

end.
