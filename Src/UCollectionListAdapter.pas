{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2024, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a class that adapts a list of snippet collections by providing an
 * alternative interface to the list, sorted by description. Designed for use
 * with GUI controls.
}

unit UCollectionListAdapter;

interface

uses
  // Delphi
  Classes,
  // Project
  DB.UCollections,
  UContainers;

type

  ///  <summary>Class that adapts a list of snippet collections by providing an
  ///  alternative interface to the list, sorted by description. Designed for
  ///  use with GUI controls.</summary>
  TCollectionListAdapter = class(TObject)
  strict private
    var
      fCollectionList: TSortedList<TCollection>;

  public

    ///  <summary>Object constructor. Sets up object with sorted list of
    ///  collections.</summary>
    constructor Create;

    ///  <summary>Object destructor. Tears down object.</summary>
    destructor Destroy; override;

    ///  <summary>Copies collection descriptions to a string list.</summary>
    ///  <param name="AStrings"><c>TStrings</c> [in] String list that receives
    ///  collection descriptions.</param>
    procedure ToStrings(const AStrings: TStrings);

    ///  <summary>Gets the collection at a specified index in the sorted list.
    ///  </summary>
    ///  <param name="AIndex"><c>Integer</c> [in] Index of required collection.
    ///  </param>
    ///  <returns><c>TCollection</c>. Required collection.</returns>
    function Collection(const AIndex: Integer): TCollection;

    ///  <summary>Gets list index of the collection with the specified UID.
    ///  </summary>
    function IndexOfUID(const AUID: TCollectionID): Integer;
  end;

implementation

uses
  // Delphi
//  Windows {for inlining},
  Generics.Defaults,
  // Project
  UStrUtils;

{ TCollectionListAdapter }

function TCollectionListAdapter.Collection(const AIndex: Integer): TCollection;
begin
  Result := fCollectionList[AIndex];
end;

constructor TCollectionListAdapter.Create;
var
  Collection: TCollection;
begin
  inherited Create;
  fCollectionList := TSortedList<TCollection>.Create(
    TDelegatedComparer<TCollection>.Create(
      function (const Left, Right: TCollection): Integer
      begin
        Result := StrCompareText(Left.Name, Right.Name)
      end
    )
  );
  for Collection in TCollections.Instance do
    fCollectionList.Add(Collection);
end;

destructor TCollectionListAdapter.Destroy;
begin
  fCollectionList.Free;
  inherited;
end;

function TCollectionListAdapter.IndexOfUID(const AUID: TCollectionID): Integer;
var
  Idx: Integer;
begin
  Result := -1;
  for Idx := 0 to Pred(fCollectionList.Count) do
    if fCollectionList[Idx].UID = AUID then
      Exit(Idx);
end;

procedure TCollectionListAdapter.ToStrings(const AStrings: TStrings);
var
  Collection: TCollection;
begin
  for Collection in fCollectionList do
    AStrings.Add(Collection.Name);
end;

end.
