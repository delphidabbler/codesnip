{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2011-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Objects, records etc that encapsulate a category, its data and lists of
 * categories.
}


unit DB.UCategory;


interface


uses
  // Delphi
  Generics.Collections,
  // Project
  DB.USnippet;


type
  {
  TCategoryData:
    Provides information about a category's properties.
  }
  TCategoryData = record
    Desc: string;     // Description of categpry
    procedure Assign(const Src: TCategoryData);
      {Sets this record's fields to be same as another TCategoryData record.
        @param Src [in] Record containing fields to be copied.
      }
    procedure Init;
      {Initialises record to nul values.
      }
  end;

  TCategoryList = class;

  {
  TCategory:
    Class that encapsulates a codesnip database category and list of all
    snippets in it.
  }
  TCategory = class(TObject)
  strict private
    fSnippets: TSnippetList;  // List of snippet objects in category
    fID: string;              // Category id
    fDescription: string;     // Category description
    fUserDefined: Boolean;    // Whether this is a user-defined snippet
    function CompareIDTo(const Cat: TCategory): Integer;
      {Compares this category's ID to that of a given category. The check is not
      case sensitive.
        @param Cat [in] Category being compared.
        @return -1 if this category's ID is less than that of Cat, 0 if both IDs
          are equal or +1 if this category's ID is greater than Cat's.
      }
  public
    constructor Create(const CatID: string; const UserDefined: Boolean;
      const Data: TCategoryData);
      {Class contructor. Sets up category object with given property values.
        @param Data [in] Contains required property values.
      }
    destructor Destroy; override;
      {Destructor. Tears down object.
      }
    function IsEqual(const Cat: TCategory): Boolean;
      {Checks if this category is same as another category. Categories are
      considered equal if they have the same ID.
        @param Cat [in] Category being compared.
        @return True if categories are equal, False if not.
      }
    function CompareDescriptionTo(const Cat: TCategory): Integer;
      {Compares this category's description to that of a given category. The
      check is not case sensitive. If both categories have the same description
      the comparison uses the category ID to ensure that the result does
      represent equality.
        @param Cat [in] Category being compared.
        @return -1 if this category's description is less than Cat's, 0 if they
          are equal or +1 if this category's description is greater than Cat's.
      }
    function CanDelete: Boolean;
      {Checks if category can be deleted.
        @return True if deletion allowed, False if not.
      }
    property ID: string read fID;
      {Category id. Must be unique}
    property Description: string read fDescription;
      {Description of category}
    property Snippets: TSnippetList read fSnippets;
      {List of snippets in this category}
    property UserDefined: Boolean read fUserDefined;
      {Flag that indicates if this is a user defined category}
  end;

  {
  TCategoryEx:
    Private extension of TCategory for use internally by Snippets object.
  }
  TCategoryEx = class(TCategory)
  public
    function GetEditData: TCategoryData;
      {Gets details of all editable data of category.
        @return Required editable data.
      }
  end;

  {
  TCategoryList:
    Class that implements a list of TCategory objects.
  }
  TCategoryList = class(TObject)
  strict private
    function GetCount: Integer;
      {Read accessor for Count property.
        @return Number of categories in list.
      }
    function GetItem(Idx: Integer): TCategory;
      {Read accessor for Items property.
        @param Idx [in] Index of required category in list.
        @return Category at specified index in list.
      }
  strict protected
    var fList: TObjectList<TCategory>;  // Stores list of categories
  public
    constructor Create(const OwnsObjects: Boolean = False);
      {Constructor. Creates a new empty list.
        @param OwnsObjects [in] Specifies whether list owns the category objects
          it contains. If True the categories are freed when deleted from list.
      }
    destructor Destroy; override;
      {Destructor. Tears down object.
      }
    procedure Clear;
      {Clears the list.
      }
    function Add(const Category: TCategory): Integer;
      {Adds new category to list.
        @param Category [in] Category to be added.
        @return Index where category inserted in list.
      }
    function Find(const CatID: string): TCategory;
      {Finds a named category in list.
        @param CatID [in] ID of required category.
        @return Reference to object representing category in list or nil if not
          in list.
      }
    function Contains(const Category: TCategory): Boolean;
      {Checks if a category is in list. Checks object reference.
        @param Category [in] Category to be checked.
        @return True if category object is in list.
      }
    function GetEnumerator: TEnumerator<TCategory>;
      {Gets an intialised category list enumerator.
        @return Required enumerator.
      }
    property Items[Idx: Integer]: TCategory read GetItem; default;
      {List of categories}
    property Count: Integer read GetCount;
      {Number of categories in list}
  end;

  {
  TCategoryListEx:
    Private extension of TCategoryList for use internally by snippets object.
  }
  TCategoryListEx = class(TCategoryList)
  public
    procedure Delete(const Category: TCategory);
      {Deletes a category from the list.
        @param Category [in] Category to be deleted.
      }
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UReservedCategories, UStrUtils;


{ TCategory }

function TCategory.CanDelete: Boolean;
  {Checks if category can be deleted.
    @return True if deletion allowed, False if not.
  }
begin
  Result := fUserDefined and fSnippets.IsEmpty
    and not TReservedCategories.IsReserved(Self);
end;

function TCategory.CompareDescriptionTo(const Cat: TCategory): Integer;
  {Compares this category's description to that of a given category. The check
  is not case sensitive. If both categories have the same description the
  comparison uses the category ID to ensure that the result does represent
  equality.
    @param Cat [in] Category being compared.
    @return -1 if this category's description is less than Cat's, 0 if they
      are equal or +1 if this category's description is greater than Cat's.
  }
begin
  Result := StrCompareText(Self.fDescription, Cat.fDescription);
  if Result = 0 then
    Result := CompareIDTo(Cat);
end;

function TCategory.CompareIDTo(const Cat: TCategory): Integer;
  {Compares this category's ID to that of a given category. The check is not
  case sensitive.
    @param Cat [in] Category being compared.
    @return -1 if this category's ID is less than that of Cat, 0 if both IDs
      are equal or +1 if this category's ID is greater than Cat's.
  }
begin
  Result := StrCompareText(Self.ID, Cat.ID);
end;

constructor TCategory.Create(const CatID: string; const UserDefined: Boolean;
  const Data: TCategoryData);
  {Class contructor. Sets up category object with given property values.
    @param Data [in] Contains required property values.
  }
begin
  Assert(ClassType <> TCategory,
    ClassName + '.Create: must only be called from descendants.');
  inherited Create;
  fID := CatID;
  fDescription := Data.Desc;
  fUserDefined := UserDefined;
  // Create list to store snippets in category
  fSnippets := TSnippetListEx.Create;
end;

destructor TCategory.Destroy;
  {Destructor. Tears down object.
  }
begin
  FreeAndNil(fSnippets);
  inherited;
end;

function TCategory.IsEqual(const Cat: TCategory): Boolean;
  {Checks if this category is same as another category. Categories are
  considered equal if they have the same ID.
    @param Cat [in] Category being compared.
    @return True if categories are equal, False if not.
  }
begin
  Result := CompareIDTo(Cat) = 0;
end;

{ TCategoryEx }

function TCategoryEx.GetEditData: TCategoryData;
  {Gets details of all editable data of category.
    @return Required editable data.
  }
begin
  Result.Desc := Self.Description;
end;

{ TCategoryList }

function TCategoryList.Add(const Category: TCategory): Integer;
  {Adds new category to list.
    @param Category [in] Category to be added.
    @return Index where category inserted in list.
  }
begin
  Assert(not fList.Contains(Category),
    ClassName + '.Add: Category already in list');
  Result := fList.Add(Category);
end;

procedure TCategoryList.Clear;
  {Clears the list.
  }
begin
  fList.Clear;
end;

function TCategoryList.Contains(const Category: TCategory): Boolean;
  {Checks if a category is in list. Checks object reference.
    @param Category [in] Category to be checked.
    @return True if category object is in list.
  }
begin
  Result := fList.Contains(Category);
end;

constructor TCategoryList.Create(const OwnsObjects: Boolean);
  {Constructor. Creates a new empty list.
    @param OwnsObjects [in] Specifies whether list owns the category objects it
      contains. If True the categories are freed when deleted from list.
  }
begin
  inherited Create;
  fList := TObjectList<TCategory>.Create(OwnsObjects);
end;

destructor TCategoryList.Destroy;
  {Destructor. Tears down object.
  }
begin
  FreeAndNil(fList);
  inherited;
end;

function TCategoryList.Find(const CatID: string): TCategory;
  {Finds a named category in list.
    @param CatID [in] ID of required category.
    @return Reference to object representing category in list or nil if not in
      list.
  }
var
  Idx: Integer;   // loops thru categories
begin
  Result := nil;
  for Idx := 0 to Pred(Count) do
  begin
    if StrSameText(CatID, Items[Idx].ID) then
    begin
      Result := Items[Idx];
      Break;
    end;
  end;
end;

function TCategoryList.GetCount: Integer;
  {Read accessor for Count property.
    @return Number of categories in list.
  }
begin
  Result := fList.Count;
end;

function TCategoryList.GetEnumerator: TEnumerator<TCategory>;
  {Gets an intialised category list enumerator.
    @return Required enumerator.
  }
begin
  Result := fList.GetEnumerator;
end;

function TCategoryList.GetItem(Idx: Integer): TCategory;
  {Read accessor for Items property.
    @param Idx [in] Index of required category in list.
    @return Category at specified index in list.
  }
begin
  Result := fList[Idx];
end;

{ TCategoryListEx }

procedure TCategoryListEx.Delete(const Category: TCategory);
  {Deletes a category from the list.
    @param Category [in] Category to be deleted.
  }
var
  Idx: Integer; // index of snippet in list.
begin
  Idx := fList.IndexOf(Category);
  if Idx = -1 then
    Exit;
  fList.Delete(Idx);  // this frees category if list owns objects
end;

{ TCategoryData }

procedure TCategoryData.Assign(const Src: TCategoryData);
  {Sets this record's fields to be same as another TCategoryData record.
    @param Src [in] Record containing fields to be copied.
  }
begin
  Desc := Src.Desc;
end;

procedure TCategoryData.Init;
  {Initialises record to nul values.
  }
begin
  Desc := '';
end;

end.

