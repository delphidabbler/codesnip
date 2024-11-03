{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2023, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Defines a singleton object and subsidiary classes that encapsulate the
 * snippets and categories in the CodeSnip database and user defined databases.
}


unit DB.UMain;


interface


uses
  // Delphi
  Classes,
  Generics.Collections,
  // Project
  ActiveText.UMain,
  Compilers.UGlobals,
  DB.UCategory,
  DB.UCollections,
  DB.USnippet,
  UContainers,
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
//    function CreateCategory(const CatID: string; const UserDefined: Boolean;
//      const Data: TCategoryData): TCategory;
//      {Creates a new category object.
//        @param CatID [in] ID of new category. Must be unique.
//        @param UserDefined [in] True if category is user defined, False if not.
//        @param Data [in] Record describing category's properties.
//        @return Instance of new category object.
//      }
//    function CreateSnippet(const Name: string; const UserDefined: Boolean;
//      const Props: TSnippetData): TSnippet;
//      {Creates a new snippet object.
//        @param Name [in] Name of new snippet. Must not exist in database
//          specified by UserDefined parameter.
//        @param UserDefined [in] True if snippet is user defined, False if not.
//        @param Props [in] Record describing snippet's properties.
//        @return Instance of new snippet with no references.
//      }

    ///  <summary>Creates a new category object.</summary>
    ///  <param name="CatID"><c>string</c> [in] ID of new category. Must be
    ///  unique.</param>
    ///  <param name="ACollectionID"><c>TCollectionID</c> [in] Collection with
    ///  which the category is associated.</param>
    ///  <param name="Data"><c>TCategoryData</c> [in] Record describing
    ///  category's properties.</param>
    ///  <returns><c>TCategory</c>. Instance of new category object.</returns>
    function CreateCategory(const CatID: string;
      const ACollectionID: TCollectionID; const Data: TCategoryData): TCategory;

    ///  <summary>Creates a new snippet object.</summary>
    ///  <param name="Name"><c>string</c> [in] Name of new snippet. Must not
    ///  exist in database</param>
    ///  <param name="ACollectionID"><c>TCollectionID</c> [in] Collection
    ///  containing the snippet.</param>
    ///  <param name="Props"><c>TSnippetData</c> [in] Record describing
    ///  snippet's properties.</param>
    ///  <returns>Instance of new snippet with no references.</returns>
    function CreateSnippet(const Name: string;
      const ACollectionID: TCollectionID; const Props: TSnippetData): TSnippet;

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
    property Snippets: TSnippetList read GetSnippets;
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
    function UpdateSnippet(const Snippet: TSnippet;
      const Data: TSnippetEditData; const NewName: string = ''): TSnippet;
      {Updates a user defined snippet's properties and references using provided
      data.
        @param Snippet [in] Snippet to be updated. Must be user-defined.
        @param Data [in] Record containing revised data.
        @param NewName [in] New name of snippet. Set to '' or Snippet.Name if
          name is not to change.
        @return Reference to updated snippet. Will have changed.
      }
    function AddSnippet(const SnippetName: string;
      const Data: TSnippetEditData): TSnippet;
      {Adds a new snippet to the user database.
        @param SnippetName [in] Name of new snippet.
        @param Data [in] Record storing new snippet's properties and references.
        @return Reference to new snippet.
      }
    function DuplicateSnippet(const Snippet: TSnippet;
      const UniqueName, DisplayName: string; const CatID: string): TSnippet;
    function CreateTempSnippet(const SnippetName: string;
      const Data: TSnippetEditData): TSnippet; overload;
      {Creates a new temporary snippet without adding it to the Snippets
      object's snippets list. The new instance may not be added to the
      Snippets object.
        @param SnippetName [in] Name of new snippet.
        @param Data [in] Record storing new snippet's properties and references.
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
  Generics.Defaults,
  // Project
  DB.UDatabaseIO,
  IntfCommon,
  UExceptions,
  UQuery,
  UStrUtils;


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
//    function CreateCategory(const CatID: string; const UserDefined: Boolean;
//      const Data: TCategoryData): TCategory;
//      {Creates a new category object.
//        @param CatID [in] ID of new category. Must be unique.
//        @param UserDefined [in] True if category is user defined, False if not.
//        @param Data [in] Record describing category's properties.
//        @return Instance of new category object.
//      }
//    function CreateSnippet(const Name: string; const UserDefined: Boolean;
//      const Props: TSnippetData): TSnippet;
//      {Creates a new snippet object.
//        @param Name [in] Name of new snippet. Must not exist in database
//          specified by UserDefined parameter.
//        @param UserDefined [in] True if snippet is user defined, False if not.
//        @param Props [in] Record describing snippet's properties.
//        @return Instance of new snippet with no references.
//      }

    ///  <summary>Creates a new category object.</summary>
    ///  <param name="CatID"><c>string</c> [in] ID of new category. Must be
    ///  unique.</param>
    ///  <param name="ACollectionID"><c>TCollectionID</c> [in] Collection with
    ///  which the category is associated.</param>
    ///  <param name="Data"><c>TCategoryData</c> [in] Record describing
    ///  category's properties.</param>
    ///  <returns><c>TCategory</c>. Instance of new category object.</returns>
    function CreateCategory(const CatID: string;
      const ACollectionID: TCollectionID; const Data: TCategoryData): TCategory;

    ///  <summary>Creates a new snippet object.</summary>
    ///  <param name="Name"><c>string</c> [in] Name of new snippet. Must not
    ///  exist in database</param>
    ///  <param name="ACollectionID"><c>TCollectionID</c> [in] Collection
    ///  containing the snippet.</param>
    ///  <param name="Props"><c>TSnippetData</c> [in] Record describing
    ///  snippet's properties.</param>
    ///  <returns>Instance of new snippet with no references.</returns>
    function CreateSnippet(const Name: string;
      const ACollectionID: TCollectionID; const Props: TSnippetData): TSnippet;

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
    function InternalAddSnippet(const SnippetName: string;
      const Data: TSnippetEditData): TSnippet;
      {Adds a new snippet to the user database. Assumes snippet not already in
      user database.
        @param SnippetName [in] Name of new snippet.
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
    procedure GetDependentList(const ASnippet: TSnippet;
      const List: TSnippetList);
      {Builds a list of all snippets that depend on a specified snippet.
        @param ASnippet [in] Snippet for which dependents are required.
        @param List [in] Receives list of dependent snippets.
      }
    procedure GetReferrerList(const ASnippet: TSnippet;
      const List: TSnippetList);
      {Builds list of all snippets that cross reference a specified snippet.
        @param ASnippet [in] The cross referenced snippet.
        @param List [in] Receives list of cross referencing snippets.
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
    { IDatabaseEdit methods }
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
        @param Snippet [in] Snippet which is cross referenced.
        @return List of IDs of referring snippets.
      }
    function UpdateSnippet(const Snippet: TSnippet;
      const Data: TSnippetEditData; const NewName: string = ''): TSnippet;
      {Updates a user defined snippet's properties and references using provided
      data.
        @param Snippet [in] Snippet to be updated. Must be user-defined.
        @param Data [in] Record containing revised data.
        @param NewName [in] New name of snippet. Set to '' or Snippet.Name if
          name is not to change.
        @return Reference to updated snippet. Will have changed.
      }
    function AddSnippet(const SnippetName: string;
      const Data: TSnippetEditData): TSnippet;
      {Adds a new snippet to the user database.
        @param SnippetName [in] Name of new snippet.
        @param Data [in] Record storing new snippet's properties and references.
        @return Reference to new snippet.
      }
    function DuplicateSnippet(const Snippet: TSnippet;
      const UniqueName, DisplayName: string; const CatID: string): TSnippet;
    function CreateTempSnippet(const SnippetName: string;
      const Data: TSnippetEditData): TSnippet; overload;
      {Creates a new temporary user defined snippet without adding it to the
      Snippets object's snippets list. The new instance may not be added to the
      Snippets object.
        @param SnippetName [in] Name of new snippet.
        @param Data [in] Record storing new snippet's properties and references.
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

function TDatabase.AddSnippet(const SnippetName: string;
  const Data: TSnippetEditData): TSnippet;
  {Adds a new snippet to the user database.
    @param SnippetName [in] Name of new snippet.
    @param Data [in] Record storing new snippet's properties and references.
    @return Reference to new snippet.
  }
resourcestring
  // Error message
  sNameExists = 'Snippet "%s" already exists in user database';
begin
  Result := nil;  // keeps compiler happy
  TriggerEvent(evChangeBegin);
  try
    // Check if snippet with same name exists in user database: error if so
//    if fSnippets.Find(SnippetName, True) <> nil then
    if fSnippets.Find(SnippetName, TCollectionID.__TMP__DBCollectionID(True)) <> nil then
      raise ECodeSnip.CreateFmt(sNameExists, [SnippetName]);
    Result := InternalAddSnippet(SnippetName, Data);
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
var
  Data: TSnippetEditData; // data describing snippet's properties and references
begin
  Assert(Assigned(Snippet), ClassName + '.CreateTempSnippet: Snippet is nil');
  Assert(Snippet is TSnippetEx,
    ClassName + '.CreateTempSnippet: Snippet is a TSnippetEx');
  Data := (Snippet as TSnippetEx).GetEditData;
//  Result := TTempSnippet.Create(
//    Snippet.Name, Snippet.UserDefined, (Snippet as TSnippetEx).GetProps);
  Result := TTempSnippet.Create(
    Snippet.Name, Snippet.CollectionID, (Snippet as TSnippetEx).GetProps);
  (Result as TTempSnippet).UpdateRefs(
    (Snippet as TSnippetEx).GetReferences, fSnippets
  );
end;

function TDatabase.CreateTempSnippet(const SnippetName: string;
  const Data: TSnippetEditData): TSnippet;
  {Creates a new temporary user defined snippet without adding it to the
  Snippets object's snippets list. The new instance may not be added to the
  Snippets object.
    @param SnippetName [in] Name of new snippet.
    @param Data [in] Record storing new snippet's properties and references.
    @return Reference to new snippet.
  }
begin
//  Result := TTempSnippet.Create(SnippetName, True, Data.Props);
  Result := TTempSnippet.Create(SnippetName, TCollectionID.__TMP__UserDBCollectionID, Data.Props);
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
  Dependent: TSnippet;      // loops thru each snippet that depends on Snippet
  Dependents: TSnippetList; // list of dependent snippets
  Referrer: TSnippet;       // loops thru snippets that cross references Snippet
  Referrers: TSnippetList;  // list of referencing snippets
begin
//  Assert(Snippet.UserDefined,
//    ClassName + '.DeleteSnippet: Snippet is not user-defined');
  Assert(Snippet.CollectionID <> TCollectionID.__TMP__MainDBCollectionID,
    ClassName + '.DeleteSnippet: Snippet is not user-defined');
  Assert(fSnippets.Contains(Snippet),
    ClassName + '.DeleteSnippet: Snippet is not in the database');
  TriggerEvent(evChangeBegin);
  TriggerEvent(evBeforeSnippetDelete, Snippet);
  // Get list of referencing and dependent snippets
  Dependents := nil;
  Referrers := nil;
  try
    Dependents := TSnippetList.Create;
    GetDependentList(Snippet, Dependents);
    Referrers := TSnippetList.Create;
    GetReferrerList(Snippet, Referrers);
    // Delete snippet for XRef or Depends list of referencing snippets
    for Referrer in Referrers do
      (Referrer.XRef as TSnippetListEx).Delete(Snippet);
    for Dependent in Dependents do
      (Dependent.Depends as TSnippetListEx).Delete(Snippet);
    // Delete snippet itself
    InternalDeleteSnippet(Snippet);
    Query.Update;
  finally
    FreeAndNil(Referrers);
    FreeAndNil(Dependents);
    fUpdated := True;
    TriggerEvent(evSnippetDeleted);
    TriggerEvent(evChangeEnd);
  end;
end;

destructor TDatabase.Destroy;
  {Destructor. Tidies up and tears down object.
  }
begin
  FreeAndNil(fChangeEvents);
  FreeAndNil(fCategories);
  FreeAndNil(fSnippets);
  inherited;
end;

function TDatabase.DuplicateSnippet(const Snippet: TSnippet;
  const UniqueName, DisplayName: string; const CatID: string): TSnippet;
var
  Data: TSnippetEditData;
begin
  Data := (Snippet as TSnippetEx).GetEditData;
  Data.Props.Cat := CatID;
  Data.Props.DisplayName := DisplayName;
  Result := AddSnippet(UniqueName, Data);
end;

function TDatabase.GetCategories: TCategoryList;
  {Gets list of all categories in database.
    @return Required list.
  }
begin
  Result := fCategories;
end;

procedure TDatabase.GetDependentList(const ASnippet: TSnippet;
  const List: TSnippetList);
  {Builds a list of all snippets that depend on a specified snippet.
    @param ASnippet [in] Snippet for which dependents are required.
    @param List [in] Receives list of dependent snippets.
  }
var
  Snippet: TSnippet;  // references each snippet in database
begin
  List.Clear;
  for Snippet in fSnippets do
    if not Snippet.IsEqual(ASnippet) and Snippet.Depends.Contains(ASnippet) then
      List.Add(Snippet);
end;

function TDatabase.GetDependents(const Snippet: TSnippet): ISnippetIDList;
  {Builds an ID list of all snippets that depend on a specified snippet.
    @param Snippet [in] Snippet for which dependents are required.
    @return List of IDs of dependent snippets.
  }
var
  List: TSnippetList; // list of dependent snippets
begin
  List := TSnippetList.Create;
  try
    GetDependentList(Snippet, List);
    Result := TSnippetIDListEx.Create(List);
  finally
    FreeAndNil(List);
  end;
end;

function TDatabase.GetEditableCategoryInfo(
  const Category: TCategory): TCategoryData;
  {Provides details of all a category's data that may be edited.
    @param Category [in] Category for which data is required. May be nil in
      whih case a blank record is returned.
    @return Required data.
  }
begin
//  Assert(not Assigned(Category) or Category.UserDefined,
//    ClassName + '.GetEditableCategoryInfo: Category is not user-defined');
  Assert(not Assigned(Category) or (Category.CollectionID <> TCollectionID.__TMP__MainDBCollectionID),
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
  Assert(not Assigned(Snippet) or (Snippet.CollectionID <> TCollectionID.__TMP__MainDBCollectionID),
    ClassName + '.GetEditableSnippetInfo: Snippet is not user-defined');
//  Assert(not Assigned(Snippet) or Snippet.UserDefined,
//    ClassName + '.GetEditableSnippetInfo: Snippet is not user-defined');
  if Assigned(Snippet) then
    Result := (Snippet as TSnippetEx).GetEditData
  else
    Result.Init;
end;

function TDatabase.GetReferrers(const Snippet: TSnippet): ISnippetIDList;
  {Builds an ID list of all snippets that cross reference a specified
  snippet.
    @param Snippet [in] Snippet which is cross referenced.
    @return List of IDs of referring snippets.
  }
var
  List: TSnippetList; // list of referring snippets
begin
  List := TSnippetList.Create;
  try
    GetReferrerList(Snippet, List);
    Result := TSnippetIDListEx.Create(List);
  finally
    FreeAndNil(List);
  end;
end;

procedure TDatabase.GetReferrerList(const ASnippet: TSnippet;
  const List: TSnippetList);
  {Builds list of all snippets that cross reference a specified snippet.
    @param ASnippet [in] The cross referenced snippet.
    @param List [in] Receives list of cross referencing snippets.
  }
var
  Snippet: TSnippet;  // references each snippet in database
begin
  List.Clear;
  for Snippet in fSnippets do
    if not Snippet.IsEqual(ASnippet) and Snippet.XRef.Contains(ASnippet) then
      List.Add(Snippet);
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
//  Result := TCategoryEx.Create(CatID, True, Data);
  Result := TCategoryEx.Create(CatID, TCollectionID.__TMP__DBCollectionID(True), Data);
  fCategories.Add(Result);
end;

function TDatabase.InternalAddSnippet(const SnippetName: string;
  const Data: TSnippetEditData): TSnippet;
  {Adds a new snippet to the user database. Assumes snippet not already in user
  database.
    @param SnippetName [in] Name of new snippet.
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
//  Result := TSnippetEx.Create(SnippetName, True, Data.Props);
  Result := TSnippetEx.Create(SnippetName, TCollectionID.__TMP__UserDBCollectionID, Data.Props);
  (Result as TSnippetEx).UpdateRefs(Data.Refs, fSnippets);
  Cat := fCategories.Find(Result.Category);
  if not Assigned(Cat) then
    raise ECodeSnip.CreateFmt(sCatNotFound, [Result.Category, Result.Name]);
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
    // Load main database: MUST do this first since user database can
    // reference objects in main database
    TDatabaseIOFactory.CreateMainDBLoader.Load(fSnippets, fCategories, Factory);
    // Load any user database
    TDatabaseIOFactory.CreateUserDBLoader.Load(fSnippets, fCategories, Factory);
    fUpdated := False;
  except
    // If an exception occurs clear the database
    Clear;
    raise;
  end;
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
  TDatabaseIOFactory.CreateWriter.Write(fSnippets, fCategories, Provider);
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
      FreeAndNil(SnippetList);
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

function TDatabase.UpdateSnippet(const Snippet: TSnippet;
  const Data: TSnippetEditData; const NewName: string): TSnippet;
  {Updates a user defined snippet's properties and references using provided
  data.
    @param Snippet [in] Snippet to be updated. Must be user-defined.
    @param Data [in] Record containing revised data.
    @param NewName [in] New name of snippet. Set to '' or Snippet.Name if name
      is not to change.
    @return Reference to updated snippet. Will have changed.
  }
var
  SnippetName: string;      // name of snippet
  Dependent: TSnippet;      // loops thru each snippetthat depends on Snippet
  Dependents: TSnippetList; // list of dependent snippets
  Referrer: TSnippet;       // loops thru snippets that cross references Snippet
  Referrers: TSnippetList;  // list of referencing snippets
resourcestring
  // Error message
  sCantRename = 'Can''t rename snippet named %0:s to %1:s: Snippet with name '
    + '%1:s already exists in user database';
begin
  Result := Snippet;      // keeps compiler happy
  Assert(Snippet.CollectionID <> TCollectionID.__TMP__MainDBCollectionID,
    ClassName + '.UpdateSnippet: Snippet is not user-defined');
//  Assert(Snippet.UserDefined,
//    ClassName + '.UpdateSnippet: Snippet is not user-defined');
  Referrers := nil;
  Dependents := nil;
  TriggerEvent(evChangeBegin);
  TriggerEvent(evBeforeSnippetChange, Snippet);
  try
    // Calculate new name
    if NewName <> '' then
      SnippetName := NewName
    else
      SnippetName := Snippet.Name;
    // If name has changed then new name musn't exist in user database
    if not StrSameText(SnippetName, Snippet.Name) then
//      if fSnippets.Find(SnippetName, True) <> nil then
      if fSnippets.Find(SnippetName, TCollectionID.__TMP__DBCollectionID(True)) <> nil then
        raise ECodeSnip.CreateFmt(sCantRename, [Snippet.Name, SnippetName]);
    // We update by deleting old snippet and inserting new one
    // get lists of snippets that cross reference or depend on this snippet
    Dependents := TSnippetList.Create;
    GetDependentList(Snippet, Dependents);
    Referrers := TSnippetList.Create;
    GetReferrerList(Snippet, Referrers);
    // remove invalid references from referring snippets
    for Referrer in Referrers do
      (Referrer.XRef as TSnippetListEx).Delete(Snippet);
    for Dependent in Dependents do
      (Dependent.Depends as TSnippetListEx).Delete(Snippet);
    // delete the snippet
    InternalDeleteSnippet(Snippet);
    // add new snippet
    Result := InternalAddSnippet(SnippetName, Data);
    // add new snippet to referrer list of referring snippets
    for Referrer in Referrers do
      Referrer.XRef.Add(Result);
    for Dependent in Dependents do
      Dependent.Depends.Add(Result);
    Query.Update;
    TriggerEvent(evSnippetChanged, Result);
  finally
    fUpdated := True;
    Referrers.Free;
    Dependents.Free;
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

//function TDBDataItemFactory.CreateCategory(const CatID: string;
//  const UserDefined: Boolean; const Data: TCategoryData): TCategory;
//  {Creates a new category object.
//    @param CatID [in] ID of new category. Must be unique.
//    @param UserDefined [in] True if category is user defined, False if not.
//    @param Data [in] Record describing category's properties.
//    @return Instance of new category object.
//  }
//begin
//  Result := TCategoryEx.Create(CatID, TCollectionID.__TMP__DBCollectionID(UserDefined), Data);
//end;
//
//function TDBDataItemFactory.CreateSnippet(const Name: string;
//  const UserDefined: Boolean; const Props: TSnippetData): TSnippet;
//  {Creates a new snippet object.
//    @param Name [in] Name of new snippet. Must not exist in database specified
//      by UserDefined parameter.
//    @param UserDefined [in] True if snippet is user defined, False if not.
//    @param Props [in] Record describing snippet's properties.
//    @return Instance of new snippet with no references.
//  }
//begin
//  Result := TSnippetEx.Create(Name, UserDefined, Props);
//end;

function TDBDataItemFactory.CreateCategory(const CatID: string;
  const ACollectionID: TCollectionID; const Data: TCategoryData): TCategory;
begin
  Result := TCategoryEx.Create(CatID, ACollectionID, Data);
end;

function TDBDataItemFactory.CreateSnippet(const Name: string;
  const ACollectionID: TCollectionID; const Props: TSnippetData): TSnippet;
begin
  Result := TSnippetEx.Create(Name, ACollectionID, Props);
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
//    if Snippet.UserDefined then
    if Snippet.CollectionID <> TCollectionID.__TMP__MainDBCollectionID then
      Result.Add(Snippet.Name);
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

