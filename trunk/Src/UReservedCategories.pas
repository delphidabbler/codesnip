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
 * Implements a static class that provides information about reserved user
 * defined categories.
}


unit UReservedCategories;


interface


uses
  // Project
  DB.UCategory, UBaseObjects;


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
    class function IsReservedName(const CatID: string): Boolean;
      {Checks if a category name is the id of a reserved category.
        @param CatID [in] ID to be checked.
        @return True if category is reserved, False if not.
      }
  public
    const UserCatID = 'user';       // default category for user snippets
    const ImportsCatID = 'imports'; // category for imported user snippets
    const SWAGCatID = '_swag_';     // category for imported SWAG snippets
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
  Windows {for inlining},
  // Project
  UStrUtils;


resourcestring
  // Default reserved category descriptions
  sUserDesc = 'User Defined Snippets';
  sImportsDesc = 'Imported Snippets';
  sSWAGDesc = 'SWAG Imports';

const
  // Maps reserved category ids onto info that describes category
  cReservedCats: array[0..2] of TReservedCategoryInfo = (
    (Name: TReservedCategories.UserCatID;     Data: (Desc: sUserDesc)),
    (Name: TReservedCategories.ImportsCatID;  Data: (Desc: sImportsDesc)),
    (Name: TReservedCategories.SWAGCatID;     Data: (Desc: sSWAGDesc))
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
  Result := IsReservedName(Cat.ID);
end;

class function TReservedCategories.IsReservedName(
  const CatID: string): Boolean;
  {Checks if a category name is the id of a reserved category.
    @param CatID [in] ID to be checked.
    @return True if category is reserved, False if not.
  }
var
  Idx: Integer; // loops thru all reserved categories
begin
  Result := False;
  for Idx := 0 to Pred(Count) do
    if StrSameText(CatID, Info(Idx).Name) then
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

