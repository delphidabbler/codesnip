{
 * DB.UCategory.pas
 *
 * Objects, records etc that encapsulate a category, its data and lists of
 * categories.
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
 * The Original Code is DB.UCategory.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
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
    fCategory: string;        // Category name
    fDescription: string;     // Category description
    fUserDefined: Boolean;    // Whether this is a user-defined snippet
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
      considered equal if they have the same name.
        @param Cat [in] Category being compared.
        @return True if categories are equal, False if not.
      }
    property Category: string read fCategory;
      {Category name. Must be unique}
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
    function Find(const CatName: string): TCategory;
      {Finds a named category in list.
        @param CatName [in] Name of required category.
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
  SysUtils;


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
  fCategory := CatID;
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
  considered equal if they have the same name.
    @param Cat [in] Category being compared.
    @return True if categories are equal, False if not.
  }
begin
  Result := AnsiSameText(Self.Category, Cat.Category);
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

function TCategoryList.Find(const CatName: string): TCategory;
  {Finds a named category in list.
    @param CatName [in] Name of required category.
    @return Reference to object representing category in list or nil if not in
      list.
  }
var
  Idx: Integer;   // loops thru categories
begin
  Result := nil;
  for Idx := 0 to Pred(Count) do
  begin
    if AnsiSameText(CatName, Items[Idx].Category) then
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
