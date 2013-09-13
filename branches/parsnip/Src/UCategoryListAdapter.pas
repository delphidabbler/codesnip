{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a class that adapts a list of snippets categories by providing an
 * alternative interface to the list sorted by description. Designed for use
 * with GUI controls.
}


unit UCategoryListAdapter;


interface


uses
  // Delphi
  Classes,
  // 3rd party
  Collections.Base,
  Collections.Lists,
  // Project
  DB.UCategory;


type

  {
  TCategoryListAdapter:
    Adapts a list of snippets categories by providing an alternative interface
    to the list sorted by description. Designed for use with GUI controls.
  }
  TCategoryListAdapter = class(TObject)
  strict private
    fCatList: TObjectSortedList<TCategory>; // Sorted list of categories
  public
    constructor Create(const CatList: TCategoryList);
      {Object constructor. Sets up object with sorted list of categories.
        @param CatList [in] List of categories.
      }
    destructor Destroy; override;
      {Object destructor. Tears down object.
      }
    procedure ToStrings(const Strings: TStrings);
      {Copies category description to a string list.
        @param Strings [in] String list to receive information.
      }
    function CatID(const Index: Integer): string;
      {Gets id of category at a specified index in the sorted list.
        @param Index [in] Index of category for which id is required.
      }
    function IndexOf(const CatID: string): Integer;
      {Gets index of a specified category in sorted list.
        @param CatID [in] Id of category.
        @return Index of category in list or -1 if not found.
      }
  end;


implementation


uses
  // Delphi
  Windows {for inlining}, Generics.Defaults,
  // Project
  CS.Utils.Hashes,
  UStrUtils;


{ TCategoryListAdapter }

function TCategoryListAdapter.CatID(const Index: Integer): string;
  {Gets name (id) of category at a specified index in the sorted list.
    @param Index [in] Index of category for which id is required.
  }
begin
  Result := fCatList[Index].ID;
end;

constructor TCategoryListAdapter.Create(const CatList: TCategoryList);
  {Object constructor. Sets up object with sorted list of categories.
    @param CatList [in] List of categories.
  }
var
  Cat: TCategory; // each category in CatList
begin
  inherited Create;
  // create list of categories, sorted by description
  fCatList := TObjectSortedList<TCategory>.Create(
    TRules<TCategory>.Create(
      TDelegatedComparer<TCategory>.Create(
        function (const Left, Right: TCategory): Integer
        begin
          Result := Left.CompareDescriptionTo(Right);
        end
      ),
      TDelegatedEqualityComparer<TCategory>.Create(
        function (const Left, Right: TCategory): Boolean
        begin
          Result := Left.CompareDescriptionTo(Right) = 0;
        end,
        function (const Cat: TCategory): Integer
        begin
          Result := PaulLarsonHash(Cat.Description)
        end
      )
    )
  );
  fCatList.OwnsObjects := False;
  for Cat in CatList do
    fCatList.Add(Cat);
end;

destructor TCategoryListAdapter.Destroy;
  {Object destructor. Tears down object.
  }
begin
  fCatList.Free;  // does not free list items
  inherited;
end;

function TCategoryListAdapter.IndexOf(const CatID: string): Integer;
  {Gets index of a specified category in sorted list.
    @param CatID [in] Id of category.
    @return Index of category in list or -1 if not found.
  }
var
  Idx: Integer; // loops through items of sorted list
begin
  Result := -1;
  for Idx := 0 to Pred(fCatList.Count) do
  begin
    if StrSameText(fCatList[Idx].ID, CatID) then
    begin
      Result := Idx;
      Break;
    end;
  end;
end;

procedure TCategoryListAdapter.ToStrings(const Strings: TStrings);
  {Copies category description to a string list.
    @param Strings [in] String list to receive information.
  }
var
  Cat: TCategory; // each category in sorted list
begin
  for Cat in fCatList do
    Strings.Add(Cat.Description);
end;

end.

