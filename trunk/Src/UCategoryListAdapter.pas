{
 * UCategoryListAdapter.pas
 *
 * Implements a class that adapts a list of snippets categories by providing an
 * alternative interface to the list sorted by description. Designed for use
 * with GUI controls.
 *
 * $Rev$
 * $Date$
 *
 * ***** BEGIN LICENSE BLOCK *****
 *
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with the
 * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * The Original Code is UCategoryListAdapter.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009-2012 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UCategoryListAdapter;


interface


uses
  // Delphi
  Classes,
  // Project
  DB.UCategory, UContainers;


type

  {
  TCategoryListAdapter:
    Adapts a list of snippets categories by providing an alternative interface
    to the list sorted by description. Designed for use with GUI controls.
  }
  TCategoryListAdapter = class(TObject)
  strict private
    fCatList: TSortedObjectList<TCategory>; // Sorted list of categories
  public
    constructor Create(const CatList: TCategoryList);
      {Object constructor. Sets up object with sorted list of categories.
        @param CatList [in] List of categories.
      }
    destructor Destroy; override;
      {Object destructor. Tears down object.
      }
    procedure ToStrings(const Strings: TStrings);
      {Copies category description and related object to a string list.
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
  fCatList := TSortedObjectList<TCategory>.Create(
    TDelegatedComparer<TCategory>.Create(
      function (const Left, Right: TCategory): Integer
      begin
        Result := StrCompareText(Left.Description, Right.Description);
        if Result = 0 then
          Result := Ord(Left.UserDefined) - Ord(Right.UserDefined);
      end
    ),
    False
  );
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
  {Copies category description and related object to a string list.
    @param Strings [in] String list to receive information.
  }
var
  Cat: TCategory; // each category in sorted list
begin
  for Cat in fCatList do
    Strings.AddObject(Cat.Description, Cat);
end;

end.

