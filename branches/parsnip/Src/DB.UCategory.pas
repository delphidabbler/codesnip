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
  CS.Database.Types,
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
    fSnippetIDs: ISnippetIDList;  // List of snippet ids in category
    fID: string;                  // Category id
    fDescription: string;         // Category description
  public
    constructor Create(const CatID: string; const UserDefined: Boolean;
      const Data: TCategoryData);
      {Class contructor. Sets up category object with given property values.
        @param Data [in] Contains required property values.
      }
    destructor Destroy; override;
      {Destructor. Tears down object.
      }
    property ID: string read fID;
      {Category id. Must be unique}
    property Description: string read fDescription;
      {Description of category}
    property SnippetIDs: ISnippetIDList read fSnippetIDs;
      {List of snippets in this category}
  end;

  {
  TCategoryEx:
    Private extension of TCategory for use internally by Snippets object.
  }
  TCategoryEx = class(TCategory)
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
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  CS.Database.Snippets,
  UReservedCategories,
  UStrUtils;


{ TCategory }

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
  // Create list to store snippet IDs in category
  fSnippetIDs := TSnippetIDList.Create;
end;

destructor TCategory.Destroy;
  {Destructor. Tears down object.
  }
begin
  inherited;
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
  fList.Free;
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

