{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2011-2021, Peter Johnson (gravatar.com/delphidabbler).
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
  DB.USnippet,
  DB.Vaults;


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
    function CompareIDTo(const Cat: TCategory): Integer;
      {Compares this category's ID to that of a given category. The check is not
      case sensitive.
        @param Cat [in] Category being compared.
        @return -1 if this category's ID is less than that of Cat, 0 if both IDs
          are equal or +1 if this category's ID is greater than Cat's.
      }
  public
    const
      ///  <summary>ID of default category.</summary>
      DefaultID = '__default__';

  public
    ///  <summary>Object constructor. Sets up category object with given
    ///  property values.</summary>
    ///  <param name="CatID"><c>CatID</c> [in] Category ID.</param>
    ///  <param name="Data"><c>TCategoryData</c> [in] category properties.
    ///  </param>
    constructor Create(const CatID: string; const Data: TCategoryData);

    ///  <summary>Creates the default category with its default description.
    ///  </summary>
    class function CreateDefault: TCategory;

    destructor Destroy; override;
      {Destructor. Tears down object.
      }

    ///  <summary>Updates category properties.</summary>
    ///  <param name="Data"><c>TCategoryData</c> [in] Updated category
    ///  properties.</param>
    procedure Update(const Data: TCategoryData);

    ///  <summary>Returns a record containing all editable data of a category.
    ///  </summary>
    function GetEditData: TCategoryData;

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

    ///  <summary>Deletes a category from the list.</summary>
    ///  <param name="Category"><c>TCategory</c> [in] Category to be deleted.
    ///  </param>
    procedure Delete(const Category: TCategory);

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


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UStrUtils;


{ TCategory }

function TCategory.CanDelete: Boolean;
  {Checks if category can be deleted.
    @return True if deletion allowed, False if not.
  }
begin
  Result := fSnippets.IsEmpty;
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

constructor TCategory.Create(const CatID: string; const Data: TCategoryData);
begin
  {TODO -cVault: Add a simpler contructor that takes only the category ID and
          description and creates does all the convoluted TCategoryData setting!
  }
  inherited Create;
  fID := CatID;
  fDescription := Data.Desc;
  // Create list to store snippets in category
  fSnippets := TSnippetList.Create;
end;

class function TCategory.CreateDefault: TCategory;
var
  Data: TCategoryData;
resourcestring
  sDefCatDesc = 'My Snippets';
begin
  Data.Init;
  Data.Desc := sDefCatDesc;
  Result := Create(DefaultID, Data);
end;

destructor TCategory.Destroy;
  {Destructor. Tears down object.
  }
begin
  FreeAndNil(fSnippets);
  inherited;
end;

function TCategory.GetEditData: TCategoryData;
begin
  Result.Desc := Self.Description;
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

procedure TCategory.Update(const Data: TCategoryData);
begin
  fDescription := Data.Desc;
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

procedure TCategoryList.Delete(const Category: TCategory);
var
  Idx: Integer; // index of snippet in list.
begin
  Idx := fList.IndexOf(Category);
  if Idx = -1 then
    Exit;
  fList.Delete(Idx);  // this frees category if list owns objects
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

