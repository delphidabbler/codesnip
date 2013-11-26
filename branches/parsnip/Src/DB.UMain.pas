{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Defines a singleton object and subsidiary classes that encapsulate the
 * snippets and categories in the CodeSnip database and user defined databases.
}


unit DB.UMain;


interface


uses
  // Project
  CS.Database.Types,
  DB.UCategory,
  DB.USnippet,
  UIStringList,
  UMultiCastEvents,
  USnippetIDs;


type

  {
  TDatabaseChangeEventKind:
    Enumeration that specifies the different kind of change events triggered by
    the user database.
  }
  TDatabaseChangeEventKind = (
    evChangeBegin,          // a change to the database is about to take place
    evChangeEnd,            // a change to the database has completed
    evSnippetAdded,         // a snippet has been added
    evBeforeSnippetDelete,  // a snippet is about to be deleted
    evSnippetDeleted,       // a snippet has been deleted
    evBeforeSnippetChange,  // a snippet is about to be changed
    evSnippetChanged,       // a snippet's properties / references have changed
    evCategoryAdded,        // a category has been added
    evBeforeCategoryDelete, // a category is about to be deleted
    evCategoryDeleted,      // a category has been deleted
    evBeforeCategoryChange, // a category is about to be changed
    evCategoryChanged       // a category's properties have changed
  );

  {
  IDBDataProvider:
    Interface supported by objects that provides data about the categories and
    snippets in the database.
  }
  IDBDataProvider = interface(IInterface)
    ['{D2D57A0D-DB29-4012-891E-E817E0EED8C8}']
    function GetCategoryProps(const Cat: TCategory): TCategoryData;
      {Retrieves all the properties of a category.
        @param Cat [in] Category for which data is requested.
        @return Record containing property data.
      }
    function GetCategorySnippets(const Cat: TCategory): IStringList;
      {Retrieves names of all snippets that belong to a category.
        @param Cat [in] Category for which snippet names are requested.
        @return Required list of snippet names.
      }
    function GetSnippetProps(const Snippet: TSnippet): TSnippetData;
      {Retrieves all the properties of a snippet.
        @param Snippet [in] Snippet for which data is requested.
        @return Record containing property data.
      }
    function GetSnippetRefs(const Snippet: TSnippet): TSnippetReferences;
      {Retrieves information about all the references of a snippet.
        @param Snippet [in] Snippet for which information is requested.
        @return Record containing references.
      }
  end;

  {
  IDatabaseChangeEventInfo:
    Interface supported by objects passed to Database object's change event
    handler that provides information about a change event. Some properites
    are not defined for certain event types. Kind property always defined.
  }
  IDatabaseChangeEventInfo = interface(IInterface)
    ['{80DEE62F-DC23-4EE7-A0B1-5DE46F483CE1}']
    function GetKind: TDatabaseChangeEventKind;
      {Gets kind (type) of event.
        @return Event kind.
      }
    function GetInfo: TObject;
      {Gets additional information about event.
        @return Object that provides required information.
      }
    property Kind: TDatabaseChangeEventKind read GetKind;
      {Identifies kind (type) of an event. Always defined}
    property Info: TObject read GetInfo;
      {Provides additional information about the event. Actual type of object
      depends on Kind. May be nil}
  end;

  {
  IDataItemFactory:
    Interface to factory object that creates snippet and category objects. For
    use by database loader objects.
  }
  IDBDataItemFactory = interface(IInterface)
    ['{C6DD85BD-E649-4A90-961C-4011D2714B3E}']
    function CreateCategory(const CatID: string; const UserDefined: Boolean;
      const Data: TCategoryData): TCategory;
      {Creates a new category object.
        @param CatID [in] ID of new category. Must be unique.
        @param UserDefined [in] True if category is user defined, False if not.
        @param Data [in] Record describing category's properties.
        @return Instance of new category object.
      }
    function CreateSnippet(const Name: string; const Props: TSnippetData):
      TSnippet;
      {Creates a new snippet object.
        @param Name [in] Name of new snippet. Must not exist in database
          specified by UserDefined parameter.
        @param Props [in] Record describing snippet's properties.
        @return Instance of new snippet with no references.
      }
  end;

  {
  IDatabase:
    Interface to object that encapsulates the whole (main and user) databases
    and provides access to all snippets and all categories.
  }
  IDatabase = interface(IInterface)
    ['{A280DEEF-0336-4264-8BD0-7CDFBB207D2E}']
    procedure Load;
      {Loads data from main and user databases.
      }
    procedure Clear;
      {Clears all data.
      }
    function Contains(const SnippetID: TSnippetID): Boolean;
    function Lookup(const SnippetID: TSnippetID): TSnippet;
    function TryLookup(const SnippetID: TSnippetID; out Snippet: TSnippet):
      Boolean;
    procedure AddChangeEventHandler(const Handler: TNotifyEventInfo);
      {Adds a change event handler to list of listeners.
        @param Handler [in] Event handler to be added.
      }
    procedure RemoveChangeEventHandler(const Handler: TNotifyEventInfo);
      {Removes a change event handler from list of listeners.
        @param Handler [in] Handler to remove from list.
      }
    function GetSnippets: TSnippetList;
      {Gets list of snippets in main and user databases.
        @return Required list.
      }
    function GetCategories: TCategoryList;
      {Gets list of categories in main and user databases.
        @return Required list.
      }
    property Categories: TCategoryList read GetCategories;
      {List of categories in main and user databases}
    property _Snippets: TSnippetList read GetSnippets;
      {List of snippets in main and user databases}
  end;

  {
  IDatabaseEdit:
    Interface to object that can be used to edit the user database.
  }
  IDatabaseEdit = interface(IInterface)
    ['{CBF6FBB0-4C18-481F-A378-84BB09E5ECF4}']
    function GetEditableSnippetInfo(const Snippet: TSnippet = nil):
      TSnippetEditData;
      {Provides details of all a snippet's data (properties and references) that
      may be edited.
        @param Snippet [in] Snippet for which data is required. May be nil in
          which case a blank record is returned.
        @return Required data.
      }
    function GetDependents(const Snippet: TSnippet): ISnippetIDList;
      {Builds an ID list of all snippets that depend on a specified snippet.
        @param Snippet [in] Snippet for which dependents are required.
        @return List of IDs of dependent snippets.
      }
    function GetReferrers(const Snippet: TSnippet): ISnippetIDList;
      {Builds an ID list of all snippets that cross reference a specified
      snippet.
        @param Snippet [in] Snippet for which cross referers are required.
        @return List of IDs of referring snippets.
      }
    procedure UpdateSnippet(const Snippet: TSnippet;
      const Data: TSnippetEditData);
      {Updates a user defined snippet's properties and references using provided
      data.
        @param Snippet [in] Snippet to be updated. Must be user-defined.
        @param Data [in] Record containing revised data.
      }
    function AddSnippet(const Data: TSnippetEditData): TSnippet;
      {Adds a new snippet to the user database.
        @param Data [in] Record storing new snippet's properties and references.
        @return Reference to new snippet.
      }
    ///  <summary>Creates new snippet that duplicates the given snippet, except
    ///  that its is a given a new unique name and the given title and category.
    ///  </summary>
    ///  <param name="Snippet">TSnippet [in] Snippet to be duplicated.</param>
    ///  <param name="Title">string [in] Title of duplicated snippet.</param>
    ///  <param name="CatID">string [in] ID of new snippet's category.</param>
    ///  <return>TSnippet. Duplicated snippet.</return>
    function DuplicateSnippet(const Snippet: TSnippet;
      const Title: string; const CatID: string): TSnippet;
    function CreateTempSnippet(const Data: TSnippetEditData;
      const Name: string = ''): TSnippet; overload;
      {Creates a new temporary snippet without adding it to the Snippets
      object's snippets list. The new instance may not be added to the
      Snippets object.
        @param Data [in] Record storing new snippet's properties and references.
        @param Name [in] Required snippet name. If ommitted then the snippet is
          created with a new, unique, name.
        @return Reference to new snippet.
      }
    function CreateTempSnippet(const Snippet: TSnippet): TSnippet; overload;
      {Creates a new temporary copy of a snippet without adding it to the
      Snippets object's snippets list. The new instance may not be added to the
      Snippets object.
        @param Snippet [in] Snippet to be copied.
        @return Reference to new copied snippet.
      }
    procedure DeleteSnippet(const Snippet: TSnippet);
      {Deletes a snippet from the user database.
        @param Snippet [in] Snippet to be deleted.
      }
    function GetEditableCategoryInfo(
      const Category: TCategory = nil): TCategoryData;
      {Provides details of all a category's data that may be edited.
        @param Category [in] Category for which data is required. May be nil in
          whih case a blank record is returned.
        @return Required data.
      }
    function AddCategory(const CatID: string;
      const Data: TCategoryData): TCategory;
      {Adds a new category to the user database.
        @param CatID [in] ID of new category.
        @param Data [in] Record storing new category's properties.
        @return Reference to new category.
      }
    function UpdateCategory(const Category: TCategory;
      const Data: TCategoryData): TCategory;
      {Updates a user defined category's properties.
        @param Category [in] Category to be updated. Must be user-defined.
        @param Data [in] Record containing revised data.
        @return Reference to updated category. Will have changed.
      }
    procedure DeleteCategory(const Category: TCategory);
      {Deletes a category and all its snippets from the user database.
        @param Category [in] Category to be deleted.
      }
    function Updated: Boolean;
      {Checks if user database has been updated since last save.
        @return True if database has been updated, False otherwise.
      }
    procedure Save;
      {Saves user database.
      }
  end;


function Database: IDatabase;
  {Returns singleton instance of object that encapsulates main and user
  databases.
    @return Singleton object.
  }


implementation


uses
  // Delphi
  SysUtils,
  // Project
  DB.UDatabaseIO, UExceptions, UQuery, UStrUtils, UUniqueID;


var
  // Private global snippets singleton object
  PvtDatabase: IDatabase = nil;


type

  {
  TDBDataItemFactory:
    Class that can create category and snippet objects.
  }
  TDBDataItemFactory = class(TInterfacedObject, IDBDataItemFactory)
  public
    function CreateCategory(const CatID: string; const UserDefined: Boolean;
      const Data: TCategoryData): TCategory;
      {Creates a new category object.
        @param CatID [in] ID of new category. Must be unique.
        @param UserDefined [in] True if category is user defined, False if not.
        @param Data [in] Record describing category's properties.
        @return Instance of new category object.
      }
    function CreateSnippet(const Name: string; const Props: TSnippetData):
      TSnippet;
      {Creates a new snippet object.
        @param Name [in] Name of new snippet. Must not exist in database
          specified by UserDefined parameter.
        @param Props [in] Record describing snippet's properties.
        @return Instance of new snippet with no references.
      }
  end;

  {
  TDatabase:
    Class that encapsulates the main and user databases. Provides access to all
    snippets and all categories via the IDatabase interface. Also enables user
    defined database to be modified via IDatabaseEdit interface.
  }
  TDatabase = class(TInterfacedObject,
    IDatabase,
    IDatabaseEdit
  )
  strict private
    fUpdated: Boolean;                // Flags if user database has been updated
    fCategories: TCategoryList;       // List of categories
    fSnippets: TSnippetList;          // List of snippets
    fChangeEvents: TMulticastEvents;  // List of change event handlers
    type
      {
      TEventInfo:
        Class that provides information about a change event.
      }
      TEventInfo = class(TInterfacedObject, IDatabaseChangeEventInfo)
      strict private
        fKind: TDatabaseChangeEventKind;  // Kind of event
        fInfo: TObject;                   // Extra info about event
      public
        constructor Create(const Kind: TDatabaseChangeEventKind;
          const Info: TObject = nil);
          {Constructor. Creates an event information object.
            @param Kind [in] Kind of event.
            @param Info [in] Reference to further information about the event.
               May be nil if event doesn't have additional information.
          }
        { IDatabaseChangeEventInfo methods }
        function GetKind: TDatabaseChangeEventKind;
          {Gets kind (type) of event.
            @return Event kind.
          }
        function GetInfo: TObject;
          {Gets additional information about event.
            @return Object that provides required information.
          }
      end;
    procedure TriggerEvent(const Kind: TDatabaseChangeEventKind;
      const Info: TObject = nil);
      {Triggers a change event. Notifies all registered listeners.
        @param Kind [in] Kind of event.
        @param Info [in] Reference to any further information for event. May be
          nil.
      }
    function InternalAddSnippet(const SnippetID: TSnippetID;
      const Data: TSnippetEditData): TSnippet;
      {Adds a new snippet to the user database. Assumes snippet not already in
      user database.
        @param SnippetID [in] ID of new snippet.
        @param Data [in] Properties and references of new snippet.
        @return Reference to new snippet object.
        @except Exception raised if snippet's category does not exist.
      }
    procedure InternalDeleteSnippet(const Snippet: TSnippet);
      {Deletes a snippet from the user database.
        @param Snippet [in] Snippet to delete from database.
      }
    function InternalAddCategory(const CatID: string;
      const Data: TCategoryData): TCategory;
      {Adds a new category to the user database. Assumes category not already in
      user database.
        @param CatID [in] ID of new category.
        @param Data [in] Properties of new category.
        @return Reference to new category object.
      }
    procedure InternalDeleteCategory(const Cat: TCategory);
      {Deletes a category from the user database.
        @param Cat [in] Category to delete from database.
      }
    function UniqueSnippetName: string;
      {Generates a snippet "name" that is unique in the database.
        @return Required unique snippet "name".
      }
  public
    constructor Create;
      {Constructor. Sets up new empty object.
      }
    destructor Destroy; override;
      {Destructor. Tidies up and tears down object.
      }
    { IDatabase methods }
    function GetCategories: TCategoryList;
      {Gets list of all categories in database.
        @return Required list.
      }
    function GetSnippets: TSnippetList;
      {Gets list of all snippets in database.
        @return Required list.
      }
    procedure Load;
      {Loads object's data from main and user defined databases.
      }
    procedure Clear;
      {Clears the object's data.
      }
    procedure AddChangeEventHandler(const Handler: TNotifyEventInfo);
      {Adds a change event handler to list of listeners.
        @param Handler [in] Event handler to be added.
      }
    procedure RemoveChangeEventHandler(const Handler: TNotifyEventInfo);
      {Removes a change event handler from list of listeners.
        @param Handler [in] Handler to remove from list.
      }
    function Contains(const SnippetID: TSnippetID): Boolean;
    function Lookup(const SnippetID: TSnippetID): TSnippet;
    function TryLookup(const SnippetID: TSnippetID; out Snippet: TSnippet):
      Boolean;
    { IDatabaseEdit methods }
    function GetEditableSnippetInfo(const Snippet: TSnippet = nil):
      TSnippetEditData;
      {Provides details of all a snippet's data (properties and references) that
      may be edited.
        @param Snippet [in] Snippet for which data is required. May be nil in
          which case a blank record is returned.
        @return Required data.
      }
    function GetDependents(const ASnippet: TSnippet): ISnippetIDList;
      {Builds an ID list of all snippets that depend on a specified snippet.
        @param ASnippet [in] Snippet for which dependents are required.
        @return List of IDs of dependent snippets.
      }
    function GetReferrers(const ASnippet: TSnippet): ISnippetIDList;
      {Builds an ID list of all snippets that cross reference a specified
      snippet.
        @param ASnippet [in] Snippet which is cross referenced.
        @return List of IDs of referring snippets.
      }
    procedure UpdateSnippet(const Snippet: TSnippet;
      const Data: TSnippetEditData);
      {Updates a user defined snippet's properties and references using provided
      data.
        @param Snippet [in] Snippet to be updated. Must be user-defined.
        @param Data [in] Record containing revised data.
      }
    function AddSnippet(const Data: TSnippetEditData): TSnippet;
      {Adds a new snippet to the user database.
        @param Data [in] Record storing new snippet's properties and references.
        @return Reference to new snippet.
      }
    ///  <summary>Creates new snippet that duplicates the given snippet, except
    ///  that its is a given a new unique name and the given title and category.
    ///  </summary>
    ///  <param name="Snippet">TSnippet [in] Snippet to be duplicated.</param>
    ///  <param name="Title">string [in] Title of duplicated snippet.</param>
    ///  <param name="CatID">string [in] ID of new snippet's category.</param>
    ///  <return>TSnippet. Duplicated snippet.</return>
    ///  <remarks>Method of IDatabaseEdit.</remarks>
    function DuplicateSnippet(const Snippet: TSnippet;
      const Title: string; const CatID: string): TSnippet;
    function CreateTempSnippet(const Data: TSnippetEditData;
      const Name: string = ''): TSnippet; overload;
      {Creates a new temporary snippet without adding it to the Snippets
      object's snippets list. The new instance may not be added to the
      Snippets object.
        @param Data [in] Record storing new snippet's properties and references.
        @param Name [in] Required snippet name. If ommitted then the snippet is
          created with a new, unique, name.
        @return Reference to new snippet.
      }
    function CreateTempSnippet(const Snippet: TSnippet): TSnippet; overload;
      {Creates a new temporary copy of a snippet without adding it to the
      Snippets object's snippets list. The new instance may not be added to the
      Snippets object.
        @param Snippet [in] Snippet to be copied.
        @return Reference to new snippet.
      }
    procedure DeleteSnippet(const Snippet: TSnippet);
      {Deletes a snippet from the user database.
        @param Snippet [in] Snippet to be deleted.
      }
    function GetEditableCategoryInfo(
      const Category: TCategory = nil): TCategoryData;
      {Provides details of all a category's data that may be edited.
        @param Category [in] Category for which data is required. May be nil in
          which case a blank record is returned.
        @return Required data.
      }
    function AddCategory(const CatID: string;
      const Data: TCategoryData): TCategory;
      {Adds a new category to the user database.
        @param CatID [in] ID of new category.
        @param Data [in] Record storing new category's properties.
        @return Reference to new category.
      }
    function UpdateCategory(const Category: TCategory;
      const Data: TCategoryData): TCategory;
      {Updates a user defined category's properties.
        @param Category [in] Category to be updated. Must be user-defined.
        @param Data [in] Record containing revised data.
        @return Reference to updated category. Will have changed.
      }
    procedure DeleteCategory(const Category: TCategory);
      {Deletes a category and all its snippets from the user database.
        @param Category [in] Category to be deleted.
      }
    function Updated: Boolean;
      {Checks if user database has been updated since last save.
        @return True if database has been updated, False otherwise.
      }
    procedure Save;
      {Saves user defined snippets and all categories to user database.
      }
  end;

  {
  TUserDataProvider:
    Class that provides data about the categories and snippets in the user-
    defined database.
  }
  TUserDataProvider = class(TInterfacedObject, IDBDataProvider)
  strict private
    fSnippets: TSnippetList;    // All snippets in the whole database
    fCategories: TCategoryList; // All categories in the whole database
  public
    constructor Create(const SnipList: TSnippetList;
      const Categories: TCategoryList);
      {Constructor. Records list of all snippets and categories in both
      databases.
        @param SnipList [in] List of all snippets in whole database.
        @param Categories [in] List of all categories in whole database.
      }
    { IDBDataProvider methods }
    function GetCategoryProps(const Cat: TCategory): TCategoryData;
      {Retrieves all the properties of a category.
        @param Cat [in] Category for which data is requested.
        @return Record containing property data.
      }
    function GetCategorySnippets(const Cat: TCategory): IStringList;
      {Retrieves names of all user-defined snippets that belong to a category.
        @param Cat [in] Category for which snippet names are requested.
        @return Required list of snippet names.
      }
    function GetSnippetProps(const Snippet: TSnippet): TSnippetData;
      {Retrieves all the properties of a snippet.
        @param Snippet [in] Snippet for which data is requested.
        @return Record containing property data.
      }
    function GetSnippetRefs(const Snippet: TSnippet): TSnippetReferences;
      {Retrieves information about all the references of a snippet.
        @param Snippet [in] Snippet for which information is requested.
        @return Record containing references.
      }
  end;

function Database: IDatabase;
  {Returns singleton instance of object that encapsulates main and user
  databases.
    @return Singleton object.
  }
begin
  if not Assigned(PvtDatabase) then
    PvtDatabase := TDatabase.Create;
  Result := PvtDatabase;
end;

{ TDatabase }

function TDatabase.AddCategory(const CatID: string;
  const Data: TCategoryData): TCategory;
  {Adds a new category to the user database.
    @param CatID [in] ID of new category.
    @param Data [in] Record storing new category's properties.
    @return Reference to new category.
  }
resourcestring
  // Error message
  sNameExists = 'Category %s already exists in user database';
begin
  Result := nil;
  TriggerEvent(evChangeBegin);
  try
    // Check if category with same id exists in user database: error if so
    if fCategories.Find(CatID) <> nil then
      raise ECodeSnip.CreateFmt(sNameExists, [CatID]);
    Result := InternalAddCategory(CatID, Data);
    Query.Update;
    TriggerEvent(evCategoryAdded, Result);
  finally
    fUpdated := True;
    TriggerEvent(evChangeEnd);
  end;
end;

procedure TDatabase.AddChangeEventHandler(const Handler: TNotifyEventInfo);
  {Adds a change event handler to list of listeners.
    @param Handler [in] Event handler to be added.
  }
begin
  fChangeEvents.AddHandler(Handler);
end;

function TDatabase.AddSnippet(const Data: TSnippetEditData): TSnippet;
  {Adds a new snippet to the user database.
    @param Data [in] Record storing new snippet's properties and references.
    @return Reference to new snippet.
  }
resourcestring
  // Error message
  sNameExists = 'Snippet "%s" already exists in user database';
var
  NewSnippetID: TSnippetID;
begin
  Result := nil;  // keeps compiler happy
  TriggerEvent(evChangeBegin);
  try
    NewSnippetID := TSnippetID.Create(UniqueSnippetName);
    // Check if snippet with same name exists in user database: error if so
    if fSnippets.Find(NewSnippetID) <> nil then
      raise ECodeSnip.CreateFmt(sNameExists, [NewSnippetID.ToString]);
    Result := InternalAddSnippet(NewSnippetID, Data);
    Query.Update;
    TriggerEvent(evSnippetAdded, Result);
  finally
    fUpdated := True;
    TriggerEvent(evChangeEnd);
  end;
end;

procedure TDatabase.Clear;
  {Clears the object's data.
  }
begin
  fCategories.Clear;
  fSnippets.Clear;
end;

function TDatabase.Contains(const SnippetID: TSnippetID): Boolean;
var
  Dummy: TSnippet;
begin
  Result := TryLookup(SnippetID, Dummy);
end;

constructor TDatabase.Create;
  {Constructor. Sets up new empty object.
  }
begin
  inherited Create;
  fSnippets := TSnippetListEx.Create(True);
  fCategories := TCategoryListEx.Create(True);
  fChangeEvents := TMultiCastEvents.Create(Self);
end;

function TDatabase.CreateTempSnippet(const Snippet: TSnippet): TSnippet;
  {Creates a new temporary copy of a snippet without adding it to the
  Snippets object's snippets list. The new instance may not be added to the
  Snippets object.
    @param Snippet [in] Snippet to be copied.
    @return Reference to new snippet.
  }
begin
  Assert(Assigned(Snippet), ClassName + '.CreateTempSnippet: Snippet is nil');
  Assert(Snippet is TSnippetEx,
    ClassName + '.CreateTempSnippet: Snippet is a TSnippetEx');
  Result := TTempSnippet.Create(Snippet.ID, (Snippet as TSnippetEx).GetProps);
  (Result as TTempSnippet).UpdateRefs(
    (Snippet as TSnippetEx).GetReferences, fSnippets
  );
end;

function TDatabase.CreateTempSnippet(const Data: TSnippetEditData;
  const Name: string = ''): TSnippet;
  {Creates a new temporary snippet without adding it to the Snippets object's
  snippets list. The new instance may not be added to the Snippets object.
    @param Data [in] Record storing new snippet's properties and references.
    @param Name [in] Required snippet name. If ommitted then the snippet is
      created with a new, unique, name.
    @return Reference to new snippet.
  }
begin
  Result := TTempSnippet.Create(
    TSnippetID.Create(StrIf(Name <> '', Name, UniqueSnippetName)), Data.Props
  );
  (Result as TTempSnippet).UpdateRefs(Data.Refs, fSnippets);
end;

procedure TDatabase.DeleteCategory(const Category: TCategory);
  {Deletes a category and all its snippets from the user database.
    @param Category [in] Category to be deleted.
  }
begin
  Assert(Category.CanDelete,
    ClassName + '.DeleteCategory: Category can''t be deleted');
  Assert(fCategories.Contains(Category),
    ClassName + '.DeleteCategory: Category is not in the database');
  TriggerEvent(evChangeBegin);
  TriggerEvent(evBeforeCategoryDelete, Category);
  try
    InternalDeleteCategory(Category);
    Query.Update;
  finally
    TriggerEvent(evCategoryDeleted);
    TriggerEvent(evChangeEnd);
    fUpdated := True;
  end;
end;

procedure TDatabase.DeleteSnippet(const Snippet: TSnippet);
  {Deletes a snippet from the user database.
    @param Snippet [in] Snippet to be deleted.
  }
var
  Dependent: TSnippetID;      // loops thru each snippet that depends on Snippet
  Dependents: ISnippetIDList; // list of dependent snippets
  Referrer: TSnippetID;       // loops thru snippets that cross references Snippet
  Referrers: ISnippetIDList;  // list of referencing snippets
begin
  Assert(fSnippets.Contains(Snippet),
    ClassName + '.DeleteSnippet: Snippet is not in the database');
  TriggerEvent(evChangeBegin);
  TriggerEvent(evBeforeSnippetDelete, Snippet);
  // Get list of referencing and dependent snippets
  Dependents := nil;
  Referrers := nil;
  try
    Dependents := GetDependents(Snippet);
    Referrers := GetReferrers(Snippet);
    // TODO: scan all snippets and remove references that match snippet ID
    // Delete snippet for XRef or Depends list of referencing snippets
    for Referrer in Referrers do
      fSnippets.Find(Referrer).XRefs.Remove(Snippet.ID);
    for Dependent in Dependents do
      fSnippets.Find(Dependent).RequiredSnippets.Remove(Snippet.ID);
    // Delete snippet itself
    InternalDeleteSnippet(Snippet);
    Query.Update;
  finally
    fUpdated := True;
    TriggerEvent(evSnippetDeleted);
    TriggerEvent(evChangeEnd);
  end;
end;

destructor TDatabase.Destroy;
  {Destructor. Tidies up and tears down object.
  }
begin
  fChangeEvents.Free;
  fCategories.Free;
  fSnippets.Free;
  inherited;
end;

function TDatabase.DuplicateSnippet(const Snippet: TSnippet;
  const Title: string; const CatID: string): TSnippet;
var
  Data: TSnippetEditData;
begin
  Data := (Snippet as TSnippetEx).GetEditData;
  Data.Props.Cat := CatID;
  Data.Props.Title := Title;
  Result := AddSnippet(Data);
end;

function TDatabase.GetCategories: TCategoryList;
  {Gets list of all categories in database.
    @return Required list.
  }
begin
  Result := fCategories;
end;

function TDatabase.GetDependents(const ASnippet: TSnippet): ISnippetIDList;
  {Builds an ID list of all snippets that depend on a specified snippet.
    @param ASnippet [in] Snippet for which dependents are required.
    @return List of IDs of dependent snippets.
  }
var
  Snippet: TSnippet;  // references each snippet in database
begin
  Result := TSnippetIDList.Create;
  for Snippet in fSnippets do
    if not Snippet.IsEqual(ASnippet)
      and Snippet.RequiredSnippets.Contains(ASnippet.ID) then
      Result.Add(Snippet.ID);
end;

function TDatabase.GetEditableCategoryInfo(
  const Category: TCategory): TCategoryData;
  {Provides details of all a category's data that may be edited.
    @param Category [in] Category for which data is required. May be nil in
      whih case a blank record is returned.
    @return Required data.
  }
begin
  Assert(not Assigned(Category) or Category.UserDefined,
    ClassName + '.GetEditableCategoryInfo: Category is not user-defined');
  if Assigned(Category) then
    Result := (Category as TCategoryEx).GetEditData
  else
    Result.Init;
end;

function TDatabase.GetEditableSnippetInfo(
  const Snippet: TSnippet): TSnippetEditData;
  {Provides details of all a snippet's data (properties and references) that may
  be edited.
    @param Snippet [in] Snippet for which data is required. May be nil in which
      case a blank record is returned.
    @return Required data.
  }
begin
  if Assigned(Snippet) then
    Result := (Snippet as TSnippetEx).GetEditData
  else
    Result.Init;
end;

function TDatabase.GetReferrers(const ASnippet: TSnippet): ISnippetIDList;
  {Builds an ID list of all snippets that cross reference a specified
  snippet.
    @param Snippet [in] Snippet which is cross referenced.
    @return List of IDs of referring snippets.
  }
var
  Snippet: TSnippet;  // references each snippet in database
begin
  Result := TSnippetIDList.Create;
  for Snippet in fSnippets do
    if not Snippet.IsEqual(ASnippet)
      and Snippet.XRefs.Contains(ASnippet.ID) then
      Result.Add(Snippet.ID);
end;

function TDatabase.GetSnippets: TSnippetList;
  {Gets list of all snippets in database.
    @return Required list.
  }
begin
  Result := fSnippets;
end;

function TDatabase.InternalAddCategory(const CatID: string;
  const Data: TCategoryData): TCategory;
  {Adds a new category to the user database. Assumes category not already in
  user database.
    @param CatID [in] ID of new category.
    @param Data [in] Properties of new category.
    @return Reference to new category object.
  }
begin
  Result := TCategoryEx.Create(CatID, True, Data);
  fCategories.Add(Result);
end;

function TDatabase.InternalAddSnippet(const SnippetID: TSnippetID;
  const Data: TSnippetEditData): TSnippet;
  {Adds a new snippet to the user database. Assumes snippet not already in user
  database.
    @param SnippetID [in] ID of new snippet.
    @param Data [in] Properties and references of new snippet.
    @return Reference to new snippet object.
    @except Exception raised if snippet's category does not exist.
  }
var
  Cat: TCategory; // category object containing new snippet
resourcestring
  // Error message
  sCatNotFound = 'Category "%0:s" referenced by new snippet named "%1:s" does '
    + 'not exist';
begin
  Result := TSnippetEx.Create(SnippetID, Data.Props);
  (Result as TSnippetEx).UpdateRefs(Data.Refs, fSnippets);
  Cat := fCategories.Find(Result.Category);
  if not Assigned(Cat) then
    raise ECodeSnip.CreateFmt(
      sCatNotFound, [Result.Category, Result.ID.ToString]
    );
  Cat.Snippets.Add(Result);
  fSnippets.Add(Result);
end;

procedure TDatabase.InternalDeleteCategory(const Cat: TCategory);
  {Deletes a category from the user database.
    @param Cat [in] Category to delete from database.
  }
begin
  (fCategories as TCategoryListEx).Delete(Cat);
end;

procedure TDatabase.InternalDeleteSnippet(const Snippet: TSnippet);
  {Deletes a snippet from the user database.
    @param Snippet [in] Snippet to delete from database.
  }
var
  Cat: TCategory; // category containing snippet
begin
  // Delete from category if found
  Cat := fCategories.Find(Snippet.Category);
  if Assigned(Cat) then
    (Cat.Snippets as TSnippetListEx).Delete(Snippet);
  // Delete from "master" list: this frees Snippet
  (fSnippets as TSnippetListEx).Delete(Snippet);
end;

procedure TDatabase.Load;
  {Loads object's data from main and user defined databases.
  }
var
  Factory: IDBDataItemFactory;  // object reader uses to create snippets objects
begin
  Clear;
  // Create factory that reader calls into to create category and snippet
  // objects. This is done to keep updating of snippet and categories private
  // to this unit
  Factory := TDBDataItemFactory.Create;
  try
    // Load any user database
    with TDatabaseIOFactory.CreateUserDBLoader do
      Load(fSnippets, fCategories, Factory);
    fUpdated := False;
  except
    // If an exception occurs clear the database
    Clear;
    raise;
  end;
end;

function TDatabase.Lookup(const SnippetID: TSnippetID): TSnippet;
begin
  if not TryLookup(SnippetID, Result) then
    Result := nil;
end;

procedure TDatabase.RemoveChangeEventHandler(const Handler: TNotifyEventInfo);
  {Removes a change event handler from list of listeners.
    @param Handler [in] Handler to remove from list.
  }
begin
  fChangeEvents.RemoveHandler(Handler);
end;

procedure TDatabase.Save;
  {Saves user defined snippets and all categories to user database.
  }
var
  Provider: IDBDataProvider;  // object that supplies info to writer
begin
  // Create object that can provide required information about user database
  Provider := TUserDataProvider.Create(fSnippets, fCategories);
  // Use a writer object to write out the database
  with TDatabaseIOFactory.CreateWriter do
    Write(fSnippets, fCategories, Provider);
  fUpdated := False;
end;

procedure TDatabase.TriggerEvent(const Kind: TDatabaseChangeEventKind;
  const Info: TObject);
  {Triggers a change event. Notifies all registered listeners.
    @param Kind [in] Kind of event.
    @param Info [in] Reference to any further information for event. May be nil.
  }
var
  EvtInfo: IDatabaseChangeEventInfo;  // event information object
begin
  EvtInfo := TEventInfo.Create(Kind, Info);
  fChangeEvents.TriggerEvents(EvtInfo);
end;

function TDatabase.TryLookup(const SnippetID: TSnippetID;
  out Snippet: TSnippet): Boolean;
begin
  Snippet := fSnippets.Find(SnippetID);
  Result := Assigned(Snippet);
end;

function TDatabase.UniqueSnippetName: string;
begin
  repeat
    Result := 'Snippet' + TUniqueID.Generate;
  until fSnippets.Find(Result) = nil;
end;

function TDatabase.UpdateCategory(const Category: TCategory;
  const Data: TCategoryData): TCategory;
  {Updates a user defined category's properties.
    @param Category [in] Category to be updated. Must be user-defined.
    @param Data [in] Record containing revised data.
    @return Reference to updated category. Will have changed.
  }
var
  SnippetList: TSnippetList;
  Snippet: TSnippet;
  CatID: string;
begin
  TriggerEvent(evChangeBegin);
  TriggerEvent(evBeforeCategoryChange, Category);
  try
    SnippetList := TSnippetList.Create;
    try
      for Snippet in Category.Snippets do
        SnippetList.Add(Snippet);
      CatID := Category.ID;
      InternalDeleteCategory(Category);
      Result := InternalAddCategory(CatID, Data);
      for Snippet in SnippetList do
        Result.Snippets.Add(Snippet);
    finally
      SnippetList.Free;
    end;
    Query.Update;
    TriggerEvent(evCategoryChanged, Result);
  finally
    fUpdated := True;
    TriggerEvent(evChangeEnd);
  end;
end;

function TDatabase.Updated: Boolean;
  {Checks if user database has been updated since last save.
    @return True if database has been updated, False otherwise.
  }
begin
  Result := fUpdated;
end;

procedure TDatabase.UpdateSnippet(const Snippet: TSnippet;
  const Data: TSnippetEditData);
  {Updates a user defined snippet's properties and references using provided
  data.
    @param Snippet [in] Snippet to be updated. Must be user-defined.
    @param Data [in] Record containing revised data.
  }

  procedure UpdateCategories(const OldCatID: string; const Snippet: TSnippet);
  resourcestring
    // Error message
    sCatNotFound = 'Category "%0:s" referenced by new snippet named "%1:s" '
      + 'does not exist';
  var
    OldCat: TCategory;
    NewCat: TCategory;
  begin
    if StrSameText(OldCatID, Snippet.Category) then
      Exit;
    OldCat := fCategories.Find(OldCatID);
    if Assigned(OldCat) then
      (OldCat.Snippets as TSnippetListEx).Delete(Snippet);
    NewCat := fCategories.Find(Snippet.Category);
    if not Assigned(NewCat) then
      raise ECodeSnip.CreateFmt(
        sCatNotFound, [Snippet.Category, Snippet.ID.ToString]
      );
    NewCat.Snippets.Add(Snippet);
  end;

var
  OldCatID: string;
begin
  TriggerEvent(evChangeBegin);
  TriggerEvent(evBeforeSnippetChange, Snippet);
  try
    OldCatID := Snippet.Category;
    (Snippet as TSnippetEx).Update(Data, fSnippets);
    UpdateCategories(OldCatID, Snippet);
    Query.Update;
    TriggerEvent(evSnippetChanged, Snippet);
  finally
    fUpdated := True;
    TriggerEvent(evChangeEnd);
  end;
end;

{ TSnippets.TEventInfo }

constructor TDatabase.TEventInfo.Create(const Kind: TDatabaseChangeEventKind;
  const Info: TObject);
  {Constructor. Creates an event information object.
    @param Kind [in] Kind of event.
    @param Info [in] Reference to further information about the event. May be
      nil if event doesn't have additional information.
  }
begin
  inherited Create;
  fKind := Kind;
  fInfo := Info;
end;

function TDatabase.TEventInfo.GetInfo: TObject;
  {Gets additional information about event.
    @return Object that provides required information.
  }
begin
  Result := fInfo;
end;

function TDatabase.TEventInfo.GetKind: TDatabaseChangeEventKind;
  {Gets kind (type) of event.
    @return Event kind.
  }
begin
  Result := fKind;
end;

{ TDBDataItemFactory }

function TDBDataItemFactory.CreateCategory(const CatID: string;
  const UserDefined: Boolean; const Data: TCategoryData): TCategory;
  {Creates a new category object.
    @param CatID [in] ID of new category. Must be unique.
    @param UserDefined [in] True if category is user defined, False if not.
    @param Data [in] Record describing category's properties.
    @return Instance of new category object.
  }
begin
  Result := TCategoryEx.Create(CatID, UserDefined, Data);
end;

function TDBDataItemFactory.CreateSnippet(const Name: string;
  const Props: TSnippetData): TSnippet;
  {Creates a new snippet object.
    @param Name [in] Name of new snippet. Must not exist in database specified
      by UserDefined parameter.
    @param Props [in] Record describing snippet's properties.
    @return Instance of new snippet with no references.
  }
begin
  Result := TSnippetEx.Create(TSnippetID.Create(Name), Props);
end;

{ TUserDataProvider }

constructor TUserDataProvider.Create(const SnipList: TSnippetList;
  const Categories: TCategoryList);
  {Constructor. Records list of all snippets and categories in both databases.
    @param SnipList [in] List of all snippets in whole database.
    @param Categories [in] List of all categories in whole database.
  }
begin
  inherited Create;
  fSnippets := SnipList;
  fCategories := Categories;
end;

function TUserDataProvider.GetCategoryProps(
  const Cat: TCategory): TCategoryData;
  {Retrieves all the properties of a category.
    @param Cat [in] Category for which data is requested.
    @return Record containing property data.
  }
begin
  Result.Desc := Cat.Description;
end;

function TUserDataProvider.GetCategorySnippets(
  const Cat: TCategory): IStringList;
  {Retrieves names of all user-defined snippets that belong to a category.
    @param Cat [in] Category for which snippet names are requested.
    @return Required list of snippet names.
  }
var
  Snippet: TSnippet;  // references each snippet in category
begin
  Result := TIStringList.Create;
  for Snippet in Cat.Snippets do
    Result.Add(Snippet.ID.ToString);
end;

function TUserDataProvider.GetSnippetProps(
  const Snippet: TSnippet): TSnippetData;
  {Retrieves all the properties of a snippet.
    @param Snippet [in] Snippet for which data is requested.
    @return Record containing property data.
  }
begin
  Result := (Snippet as TSnippetEx).GetProps;
end;

function TUserDataProvider.GetSnippetRefs(
  const Snippet: TSnippet): TSnippetReferences;
  {Retrieves information about all the references of a snippet.
    @param Snippet [in] Snippet for which information is requested.
    @return Record containing references.
  }
begin
  Result := (Snippet as TSnippetEx).GetReferences;
end;

initialization


finalization

// Free the singleton
PvtDatabase := nil;

end.

