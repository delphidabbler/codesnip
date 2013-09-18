{
 * UReservedCategories.pas
 *
 * Implements a static class that provides information about reserved user
 * defined categories.
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
 * The Original Code is UReservedCategories.pas
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


unit UReservedCategories;


interface


uses
  // Project
  UBaseObjects, USnippets;


type

  {
  TReservedCategoryInfo:
    Record that fully describes a category, i.e. contains all the required data.
  }
  TReservedCategoryInfo = record
    Name: string;         // Name (unique id) of category
    Data: TCategoryData;  // Category's properties
    procedure Assign(const Src: TReservedCategoryInfo);
      {Sets this record's fields to be same as another TReservedCategoryInfo
      record.
        @param Src [in] Record containing fields to be copied.
      }
    procedure Init;
      {Initialises record to nul values.
      }
  end;

  {
  TReservedCategories:
    Static class that provides information about reserved user defined
    categories.
  }
  TReservedCategories = class(TNoConstructObject)
  strict private
    class function IsReservedName(const CatName: string): Boolean;
      {Checks if a category name is the id of a reserved category.
        @param CatName [in] Name to be checked.
        @return True if category is reserved, False if not.
      }
  public
    const UserCatName = 'user';       // default category for user snippets
    const ImportsCatName = 'imports'; // category where imported snippets placed
    class function IsReserved(const Cat: TCategory): Boolean;
      {Checks if a category is reserved.
        @param Cat [in] Category to be checked.
        @return True if category is reserved, False if not.
      }
    class function Info(Idx: Integer): TReservedCategoryInfo;
      {Gets information about a reserved category.
        @param Idx [in] Index of required category.
        @return Record containing information about category.
      }
    class function Count: Integer;
      {Gets number of reserved categories.
        @return Number of reserved categories.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, Windows {for inlining};


resourcestring
  // Default reserved category descriptions
  sUserDesc = 'User Defined Snippets';
  sImportsDesc = 'Imported Snippets';

const
  // Maps reserved category ids onto info that describes category
  cReservedCats: array[0..1] of TReservedCategoryInfo = (
    (Name: TReservedCategories.UserCatName;     Data: (Desc: sUserDesc)),
    (Name: TReservedCategories.ImportsCatName;  Data: (Desc: sImportsDesc))
  );

{ TReservedCategories }

class function TReservedCategories.Count: Integer;
  {Gets number of reserved categories.
    @return Number of reserved categories.
  }
begin
  Result := Length(cReservedCats);
end;

class function TReservedCategories.Info(Idx: Integer): TReservedCategoryInfo;
  {Gets information about a reserved category.
    @param Idx [in] Index of required category.
    @return Record containing information about category.
  }
begin
  Result.Assign(cReservedCats[Idx]);
end;

class function TReservedCategories.IsReserved(const Cat: TCategory): Boolean;
  {Checks if a category is reserved.
    @param Cat [in] Category to be checked.
    @return True if category is reserved, False if not.
  }
begin
  Result := IsReservedName(Cat.Category);
end;

class function TReservedCategories.IsReservedName(
  const CatName: string): Boolean;
  {Checks if a category name is the id of a reserved category.
    @param CatName [in] Name to be checked.
    @return True if category is reserved, False if not.
  }
var
  Idx: Integer; // loops thru all reserved categories
begin
  Result := False;
  for Idx := 0 to Pred(Count) do
    if AnsiSameText(CatName, Info(Idx).Name) then
    begin
      Result := True;
      Exit;
    end;
end;

{ TReservedCategoryInfo }

procedure TReservedCategoryInfo.Assign(const Src: TReservedCategoryInfo);
  {Sets this record's fields to be same as another TReservedCategoryInfo record.
    @param Src [in] Record containing fields to be copied.
  }
begin
  Name := Src.Name;
  Data.Assign(Src.Data);
end;

procedure TReservedCategoryInfo.Init;
  {Initialises record to nul values.
  }
begin
  Name := '';
  Data.Init;
end;

end.

