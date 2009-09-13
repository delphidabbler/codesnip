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
 * Portions created by the Initial Developer are Copyright (C) 2009 Peter
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
  ULists, USnippets;


type

  {
  TCategoryListAdapter:
    Adapts a list of snippets categories by providing an alternative interface
    to the list sorted by description. Designed for use with GUI controls.
  }
  TCategoryListAdapter = class(TObject)
  strict private
    fCatList: TSortedObjectList;  // sorted list of categories
    function CompareCategories(const Obj1, Obj2: TObject): Integer;
      {Callback comparison method for passing to sorted list object. Compares
      two category objects.
        @param Obj1 [in] First category object to be compared.
        @param Obj2 [in] Second category object to be compared.
        @return -ve if Obj1 < Obj2, 0 if Obj1 = Obj2 or +ve if Obj1 > Obj2.
      }
  public
    constructor Create(const CatList: TCategoryList);
      {Class constructor. Sets up object with sorted list of categories.
        @param CatList [in] List of categories.
      }
    destructor Destroy; override;
      {Class desctrutor. Tears down object.
      }
    procedure ToStrings(const Strings: TStrings);
      {Copies category description and related object to a string list.
        @param Strings [in] String list to receive information.
      }
    function CatName(const Index: Integer): string;
      {Gets name (id) of category at a specified index in the sorted list.
        @param Index [in] Index of category for which name (id) is required.
      }
    function IndexOf(const CatName: string): Integer;
      {Gets index of a named category in sorted list.
        @param CatName [in] Name (id) of category.
        @return Index of category in list or -1 if not found.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, Windows {for inlining};


{ TCategoryListAdapter }

function TCategoryListAdapter.CatName(const Index: Integer): string;
  {Gets name (id) of category at a specified index in the sorted list.
    @param Index [in] Index of category for which name (id) is required.
  }
begin
  Result := (fCatList[Index] as TCategory).Category;
end;

function TCategoryListAdapter.CompareCategories(const Obj1, Obj2: TObject): Integer;
  {Callback comparison method for passing to sorted list object. Compares two
  category objects.
    @param Obj1 [in] First category object to be compared.
    @param Obj2 [in] Second category object to be compared.
    @return -ve if Obj1 < Obj2, 0 if Obj1 = Obj2 or +ve if Obj1 > Obj2.
  }
var
  Cat1, Cat2: TCategory;  // categoryies to be compared
begin
  Cat1 := Obj1 as TCategory;
  Cat2 := Obj2 as TCategory;
  // sort by description first
  Result := AnsiCompareText(Cat1.Description, Cat2.Description);
  if Result = 0 then
    // two descriptions are same: sort user-defined after main database versions
    Result := Ord(Cat1.UserDefined) - Ord(Cat2.UserDefined);
end;

constructor TCategoryListAdapter.Create(const CatList: TCategoryList);
  {Class constructor. Sets up object with sorted list of categories.
    @param CatList [in] List of categories.
  }
var
  Cat: TCategory; // each category in CatList
begin
  inherited Create;
  // make a sorted reference to CatList, sorted by description
  fCatList := TSortedObjectList.Create(False, CompareCategories);
  for Cat in CatList do
    fCatList.Add(Cat);
end;

destructor TCategoryListAdapter.Destroy;
  {Class desctrutor. Tears down object.
  }
begin
  FreeAndNil(fCatList); // does not free list items
  inherited;
end;

function TCategoryListAdapter.IndexOf(const CatName: string): Integer;
  {Gets index of a named category in sorted list.
    @param CatName [in] Name (id) of category.
    @return Index of category in list or -1 if not found.
  }
var
  Idx: Integer; // loops through items of sorted list
begin
  Result := -1;
  for Idx := 0 to Pred(fCatList.Count) do
  begin
    if AnsiSameText((fCatList[Idx] as TCategory).Category, CatName) then
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
  Cat: TObject; // each category in sorted list
begin
  for Cat in fCatList do
    Strings.AddObject((Cat as TCategory).Description, Cat);
end;

end.
