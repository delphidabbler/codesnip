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
  CS.Database.SnippetsTable,
  CS.Database.Types,
  DB.UCategory,
  DB.USnippet,
  UIStringList,
  UMultiCastEvents,
  USingleton;


type

  TDatabase = class(TSingleton)
  strict private
    class var
      fInstance: TDatabase;
    class function GetInstance: TDatabase; static;
  strict private
    var
      fSnippetsTable: TDBSnippetsTable;
      fAllTags: ITagSet;
  strict protected
    procedure Initialize; override;
    procedure Finalize; override;
  protected
    { TODO: Remove these properties when _TDatabase no longer needs access to
            them }
    // NOTE: These properties have been made protected so that _TDatabase can
    //       directly access them using unit scope.
    ///      DON'T make this section strict.
    property __SnippetsTable: TDBSnippetsTable read fSnippetsTable;
    property __AllTags: ITagSet read fAllTags;
  public
    class property Instance: TDatabase read GetInstance;
    function LookupEditableSnippet(const ASnippetID: TSnippetID):
      IEditableSnippet;
    function LookupReadOnlySnippet(const ASnippetID: TSnippetID;
      const ARequiredProperties: TDBSnippetProps): IReadOnlySnippet;
    function SelectSnippets(Filter: IDBFilter): ISnippetIDList;
    function GetAllSnippets: ISnippetIDList;
    function GetAllTags: ITagSet;
    function SnippetExists(const ASnippetID: TSnippetID): Boolean;
    function SnippetCount: Integer;
    function IsEmpty: Boolean;
  end;

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
    function CreateCategory(const CatID: string; const Data: TCategoryData):
      TCategory;
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

  TDatabaseFilterFn = reference to function (const Snippet: TSnippet): Boolean;

  {
  _IDatabase:
    Interface to object that encapsulates the whole (main and user) databases
    and provides access to all snippets and all categories.
  }
  _IDatabase = interface(IInterface)
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
    function SelectAll: ISnippetIDList;
    function Select(FilterFn: TDatabaseFilterFn): ISnippetIDList;
    procedure AddChangeEventHandler(const Handler: TNotifyEventInfo);
      {Adds a change event handler to list of listeners.
        @param Handler [in] Event handler to be added.
      }
    procedure RemoveChangeEventHandler(const Handler: TNotifyEventInfo);
      {Removes a change event handler from list of listeners.
        @param Handler [in] Handler to remove from list.
      }
    // Returns a list of all supported tags
    function GetAllTags: ITagSet;
    // Returns a list of IDs of all snippets that depend on the snippet with
    // the given ID.
    function GetDependentsOf(const ASnippetID: TSnippetID): ISnippetIDList;
    // Returns a list of IDs of all snippet that refer to (i.e. cross-reference)
    // the snippet with the given ID.
    function GetReferrersTo(const ASnippetID: TSnippetID): ISnippetIDList;
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
    procedure UpdateSnippet(const Snippet: TSnippet; Data: TSnippetEditData);
      {Updates a user defined snippet's properties and references using provided
      data.
        @param Snippet [in] Snippet to be updated. Must be user-defined.
        @param Data [in] Record containing revised data.
      }
    function AddSnippet(Data: TSnippetEditData): TSnippet;
      {Adds a new snippet to the user database.
        @param Data [in] Record storing new snippet's properties and references.
        @return Reference to new snippet.
      }
    ///  <summary>Creates new snippet that duplicates the given snippet, except
    ///  that its is a given a new unique name and the given title.</summary>
    ///  <param name="Snippet">TSnippet [in] Snippet to be duplicated.</param>
    ///  <param name="Title">string [in] Title of duplicated snippet.</param>
    ///  <return>TSnippet. Duplicated snippet.</return>
    function DuplicateSnippet(const Snippet: TSnippet; const Title: string):
      TSnippet;
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
    function Updated: Boolean;
      {Checks if user database has been updated since last save.
        @return True if database has been updated, False otherwise.
      }
    procedure Save;
      {Saves user database.
      }
  end;


function _Database: _IDatabase;
  {Returns singleton instance of object that encapsulates main and user
  databases.
    @return Singleton object.
  }

function Database: TDatabase;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  CS.Database.Snippets,
  CS.Database.Tags,
  DB.UDatabaseIO,
  UExceptions,
  UQuery,
  UStrUtils,
  UUniqueID;


var
  // Private global snippets singleton object
  _PvtDatabase: _IDatabase = nil;


type

  {
  TDBDataItemFactory:
    Class that can create category and snippet objects.
  }
  TDBDataItemFactory = class(TInterfacedObject, IDBDataItemFactory)
  public
    function CreateCategory(const CatID: string;
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
  _TDatabase:
    Class that encapsulates the main and user databases. Provides access to all
    snippets and all categories via the IDatabase interface. Also enables user
    defined database to be modified via IDatabaseEdit interface.
  }
  _TDatabase = class(TInterfacedObject,
    _IDatabase,
    IDatabaseEdit
  )
  strict private
    fUpdated: Boolean;                // Flags if user database has been updated
    fCategories: TCategoryList;       // List of categories
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
    function UniqueSnippetName: string;
      {Generates a snippet "name" that is unique in the database.
        @return Required unique snippet "name".
      }
    procedure CleanUpRefs(var Refs: TSnippetReferences);
      {Cleans any references non-existant snippets from snippet references.
        @param [in/out] Snippet references to be cleaned: .RequiredSnippets and
          .XRefs fields are modified.
      }
    function MakeValidRefs(const Refs: TSnippetReferences): TSnippetReferences;
  public
    constructor Create;
      {Constructor. Sets up new empty object.
      }
    destructor Destroy; override;
      {Destructor. Tidies up and tears down object.
      }
    { IDatabase methods }
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
    function SelectAll: ISnippetIDList;
    function Select(FilterFn: TDatabaseFilterFn): ISnippetIDList;
    // Returns a list of all supported tags
    function GetAllTags: ITagSet;
    // Returns a list of IDs of all snippets that depend on the snippet with
    // the given ID.
    function GetDependentsOf(const ASnippetID: TSnippetID): ISnippetIDList;
    // Returns a list of IDs of all snippet that refer to (i.e. cross-reference)
    // the snippet with the given ID.
    function GetReferrersTo(const ASnippetID: TSnippetID): ISnippetIDList;
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
    procedure UpdateSnippet(const Snippet: TSnippet; Data: TSnippetEditData);
      {Updates a user defined snippet's properties and references using provided
      data.
        @param Snippet [in] Snippet to be updated. Must be user-defined.
        @param Data [in] Record containing revised data.
      }
    function AddSnippet(Data: TSnippetEditData): TSnippet;
      {Adds a new snippet to the user database.
        @param Data [in] Record storing new snippet's properties and references.
        @return Reference to new snippet.
      }
    ///  <summary>Creates new snippet that duplicates the given snippet, except
    ///  that its is a given a new unique name and the given title.</summary>
    ///  <param name="Snippet">TSnippet [in] Snippet to be duplicated.</param>
    ///  <param name="Title">string [in] Title of duplicated snippet.</param>
    ///  <return>TSnippet. Duplicated snippet.</return>
    ///  <remarks>Method of IDatabaseEdit.</remarks>
    function DuplicateSnippet(const Snippet: TSnippet; const Title: string):
      TSnippet;
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
  public
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

function Database: TDatabase;
begin
  Result := TDatabase.Instance;
end;

function _Database: _IDatabase;
  {Returns singleton instance of object that encapsulates main and user
  databases.
    @return Singleton object.
  }
begin
  if not Assigned(_PvtDatabase) then
    _PvtDatabase := _TDatabase.Create;
  Result := _PvtDatabase;
end;

{ _TDatabase }

procedure _TDatabase.AddChangeEventHandler(const Handler: TNotifyEventInfo);
  {Adds a change event handler to list of listeners.
    @param Handler [in] Event handler to be added.
  }
begin
  fChangeEvents.AddHandler(Handler);
end;

function _TDatabase.AddSnippet(Data: TSnippetEditData): TSnippet;
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
    if Database.__SnippetsTable.Contains(NewSnippetID) then
      raise ECodeSnip.CreateFmt(sNameExists, [NewSnippetID.ToString]);
    CleanUpRefs(Data.Refs);
    Result := InternalAddSnippet(NewSnippetID, Data);
    Query.Update;
    TriggerEvent(evSnippetAdded, Result);
  finally
    fUpdated := True;
    TriggerEvent(evChangeEnd);
  end;
end;

procedure _TDatabase.CleanUpRefs(var Refs: TSnippetReferences);

  function CleanSnippetList(const SnipList: ISnippetIDList): ISnippetIDList;
  var
    SnippetID: TSnippetID;
  begin
    Result := TSnippetIDList.Create;
    for SnippetID in SnipList do
      if Database.__SnippetsTable.Contains(SnippetID) then
        Result.Add(SnippetID);
  end;

begin
  Refs.RequiredSnippets := CleanSnippetList(Refs.RequiredSnippets);
  Refs.XRefs := CleanSnippetList(Refs.XRefs);
end;

procedure _TDatabase.Clear;
  {Clears the object's data.
  }
begin
  fCategories.Clear;
  Database.__AllTags.Clear;
  Database.__SnippetsTable.Clear;
end;

function _TDatabase.Contains(const SnippetID: TSnippetID): Boolean;
var
  Dummy: TSnippet;
begin
  Result := TryLookup(SnippetID, Dummy);
end;

constructor _TDatabase.Create;
  {Constructor. Sets up new empty object.
  }
begin
  inherited Create;
  fCategories := TCategoryListEx.Create(True);
  fChangeEvents := TMultiCastEvents.Create(Self);
end;

function _TDatabase.CreateTempSnippet(const Snippet: TSnippet): TSnippet;
  {Creates a new temporary copy of a snippet without adding it to the
  Snippets object's snippets list. The new instance may not be added to the
  Snippets object.
    @param Snippet [in] Snippet to be copied.
    @return Reference to new snippet.
  }
begin
  Assert(Assigned(Snippet), ClassName + '.CreateTempSnippet: Snippet is nil');
  Result := TSnippet.Create(Snippet.ID);
  Result.SetProps(Snippet.GetProps);
  Result.UpdateRefs(MakeValidRefs(Snippet.GetReferences));
end;

function _TDatabase.CreateTempSnippet(const Data: TSnippetEditData;
  const Name: string = ''): TSnippet;
  {Creates a new temporary snippet without adding it to the Snippets object's
  snippets list. The new instance may not be added to the Snippets object.
    @param Data [in] Record storing new snippet's properties and references.
    @param Name [in] Required snippet name. If ommitted then the snippet is
      created with a new, unique, name.
    @return Reference to new snippet.
  }
begin
  Result := TSnippet.Create(
    TSnippetID.Create(StrIf(Name <> '', Name, UniqueSnippetName))
  );
  Result.SetProps(Data.Props);
  Result.UpdateRefs(MakeValidRefs(Data.Refs));
end;

procedure _TDatabase.DeleteSnippet(const Snippet: TSnippet);
  {Deletes a snippet from the user database.
    @param Snippet [in] Snippet to be deleted.
  }
var
  Dependent: TSnippetID;      // each snippet that depends on Snippet
  Dependents: ISnippetIDList; // list of dependent snippets
  Referrer: TSnippetID;       // each snippet that cross references Snippet
  Referrers: ISnippetIDList;  // list of referencing snippets
begin
  Assert(Database.__SnippetsTable.Contains(Snippet.ID),
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
      Database.__SnippetsTable.Get(Referrer).XRefs.Remove(Snippet.ID);
    for Dependent in Dependents do
      Database.__SnippetsTable.Get(Dependent).RequiredSnippets.Remove(
        Snippet.ID
      );
    // Delete snippet itself
    InternalDeleteSnippet(Snippet);
    Query.Update;
  finally
    fUpdated := True;
    TriggerEvent(evSnippetDeleted);
    TriggerEvent(evChangeEnd);
  end;
end;

destructor _TDatabase.Destroy;
  {Destructor. Tidies up and tears down object.
  }
begin
  fChangeEvents.Free;
  fCategories.Free;
  inherited;
end;

function _TDatabase.DuplicateSnippet(const Snippet: TSnippet;
  const Title: string): TSnippet;
var
  Data: TSnippetEditData;
begin
  Data := Snippet.GetEditData;
  Data.Props.Title := Title;
  Result := AddSnippet(Data);
end;

function _TDatabase.GetAllTags: ITagSet;
begin
  Result := TTagSet.Create(Database.__AllTags);
end;

function _TDatabase.GetDependents(const ASnippet: TSnippet): ISnippetIDList;
  {Builds an ID list of all snippets that depend on a specified snippet.
    @param ASnippet [in] Snippet for which dependents are required.
    @return List of IDs of dependent snippets.
  }
var
  Snippet: TSnippet;  // references each snippet in database
begin
  Result := TSnippetIDList.Create;
  for Snippet in Database.__SnippetsTable do
    if not Snippet.IsEqual(ASnippet)
      and Snippet.RequiredSnippets.Contains(ASnippet.ID) then
      Result.Add(Snippet.ID);
end;

function _TDatabase.GetDependentsOf(const ASnippetID: TSnippetID):
  ISnippetIDList;
var
  Snippet: TSnippet;
begin
  Result := TSnippetIDList.Create;
  for Snippet in Database.__SnippetsTable do
    if (Snippet.ID <> ASnippetID)
      and Snippet.RequiredSnippets.Contains(ASnippetID) then
      Result.Add(Snippet.ID);
end;

function _TDatabase.GetEditableSnippetInfo(
  const Snippet: TSnippet): TSnippetEditData;
  {Provides details of all a snippet's data (properties and references) that may
  be edited.
    @param Snippet [in] Snippet for which data is required. May be nil in which
      case a blank record is returned.
    @return Required data.
  }
begin
  if Assigned(Snippet) then
    Result := Snippet.GetEditData
  else
    Result.Init;
end;

function _TDatabase.GetReferrers(const ASnippet: TSnippet): ISnippetIDList;
  {Builds an ID list of all snippets that cross reference a specified
  snippet.
    @param Snippet [in] Snippet which is cross referenced.
    @return List of IDs of referring snippets.
  }
var
  Snippet: TSnippet;  // references each snippet in database
begin
  Result := TSnippetIDList.Create;
  for Snippet in Database.__SnippetsTable do
    if not Snippet.IsEqual(ASnippet)
      and Snippet.XRefs.Contains(ASnippet.ID) then
      Result.Add(Snippet.ID);
end;

function _TDatabase.GetReferrersTo(const ASnippetID: TSnippetID):
  ISnippetIDList;
var
  Snippet: TSnippet;  // references each snippet in database
begin
  Result := TSnippetIDList.Create;
  for Snippet in Database.__SnippetsTable do
    if (Snippet.ID <> ASnippetID)
      and Snippet.XRefs.Contains(ASnippetID) then
      Result.Add(Snippet.ID);
end;

function _TDatabase.InternalAddSnippet(const SnippetID: TSnippetID;
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
  Result := TSnippet.Create(SnippetID);
  Result.SetProps(Data.Props);
  Result.UpdateRefs(MakeValidRefs(Data.Refs));
  Cat := fCategories.Find(Result.Category);
  if not Assigned(Cat) then
    raise ECodeSnip.CreateFmt(
      sCatNotFound, [Result.Category, Result.ID.ToString]
    );
  Cat.SnippetIDs.Add(Result.ID);
  // ensure any unknown tags are added to set of all known tags
  Database.__AllTags.Include(Result.Tags);
  Database.__SnippetsTable.Add(Result);
end;

procedure _TDatabase.InternalDeleteSnippet(const Snippet: TSnippet);
  {Deletes a snippet from the user database.
    @param Snippet [in] Snippet to delete from database.
  }
var
  Cat: TCategory; // category containing snippet
begin
  // Don't remove tags from tag list - leave them there
  // Delete from category if found
  Cat := fCategories.Find(Snippet.Category);
  if Assigned(Cat) then
    Cat.SnippetIDs.Remove(Snippet.ID);
  // Delete from "master" list: this frees Snippet
  Database.__SnippetsTable.Delete(Snippet.ID);
end;

procedure _TDatabase.Load;
  {Loads object's data from main and user defined databases.
  }
var
  Factory: IDBDataItemFactory;  // object reader uses to create snippets objects
  SnipList: _TSnippetList;
  Snippet: TSnippet;
begin
  Clear;
  // Create factory that reader calls into to create category and snippet
  // objects. This is done to keep updating of snippet and categories private
  // to this unit
  Factory := TDBDataItemFactory.Create;
  try
    SnipList := _TSnippetList.Create(False);
    try
      // Load any user database
      with TDatabaseIOFactory.CreateUserDBLoader do
        Load(SnipList, fCategories, Database.__AllTags, Factory);
      for Snippet in SnipList do
        Database.__SnippetsTable.Add(Snippet);
    finally
      SnipList.Free;
    end;
    fUpdated := False;
  except
    // If an exception occurs clear the database
    Clear;
    raise;
  end;
end;

function _TDatabase.Lookup(const SnippetID: TSnippetID): TSnippet;
begin
  if not TryLookup(SnippetID, Result) then
    raise EBug.Create(ClassName + '.Lookup: SnippetID not found in database');
end;

function _TDatabase.MakeValidRefs(const Refs: TSnippetReferences):
  TSnippetReferences;
begin
  Result.Assign(Refs);
  CleanUpRefs(Result);
end;

procedure _TDatabase.RemoveChangeEventHandler(const Handler: TNotifyEventInfo);
  {Removes a change event handler from list of listeners.
    @param Handler [in] Handler to remove from list.
  }
begin
  fChangeEvents.RemoveHandler(Handler);
end;

procedure _TDatabase.Save;
  {Saves user defined snippets and all categories to user database.
  }
var
  Provider: IDBDataProvider;  // object that supplies info to writer
  SnipList: _TSnippetList;
  Snippet: TSnippet;
begin
  // Create object that can provide required information about user database
  Provider := TUserDataProvider.Create;
  SnipList := _TSnippetList.Create(False);
  try
    for Snippet in Database.__SnippetsTable do
      SnipList.Add(Snippet);
    // Use a writer object to write out the database
    with TDatabaseIOFactory.CreateWriter do
      Write(SnipList, fCategories, Provider);
  finally
    SnipList.Free;
  end;
  fUpdated := False;
end;

function _TDatabase.Select(FilterFn: TDatabaseFilterFn): ISnippetIDList;
var
  Snippet: TSnippet;
begin
  Result := TSnippetIDList.Create;
  for Snippet in Database.__SnippetsTable do
    if FilterFn(Snippet) then
      Result.Add(Snippet.ID)
end;

function _TDatabase.SelectAll: ISnippetIDList;
var
  Snippet: TSnippet;
begin
  Result := TSnippetIDList.Create(Database.__SnippetsTable.Size);
  for Snippet in Database.__SnippetsTable do
    Result.Add(Snippet.ID);
end;

procedure _TDatabase.TriggerEvent(const Kind: TDatabaseChangeEventKind;
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

function _TDatabase.TryLookup(const SnippetID: TSnippetID;
  out Snippet: TSnippet): Boolean;
begin
  Result := Database.__SnippetsTable.Contains(SnippetID);
  if Result then
    Snippet := Database.__SnippetsTable.Get(SnippetID);
end;

function _TDatabase.UniqueSnippetName: string;
begin
  repeat
    Result := 'Snippet' + TUniqueID.Generate;
  until not Database.__SnippetsTable.Contains(TSnippetID.Create(Result));
end;

function _TDatabase.Updated: Boolean;
  {Checks if user database has been updated since last save.
    @return True if database has been updated, False otherwise.
  }
begin
  Result := fUpdated;
end;

procedure _TDatabase.UpdateSnippet(const Snippet: TSnippet;
  Data: TSnippetEditData);
  {Updates a user defined snippet's properties and references using provided
  data.
    @param Snippet [in] Snippet to be updated. Must be user-defined.
    @param Data [in] Record containing revised data.
  }
var
  OldCatID: string;
begin
  TriggerEvent(evChangeBegin);
  TriggerEvent(evBeforeSnippetChange, Snippet);
  try
    OldCatID := Snippet.Category;
    CleanUpRefs(Data.Refs);
    Snippet.Update(Data);
    // ensure any new, unknown, tags are added to set of all tags
    Database.__AllTags.Include(Snippet.Tags);
    // NOTE: since categories can no longer be changed, there is no need to
    // update TCategory.SnippetIDs like we used to.
    Query.Update;
    TriggerEvent(evSnippetChanged, Snippet);
  finally
    fUpdated := True;
    TriggerEvent(evChangeEnd);
  end;
end;

{ TSnippets.TEventInfo }

constructor _TDatabase.TEventInfo.Create(const Kind: TDatabaseChangeEventKind;
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

function _TDatabase.TEventInfo.GetInfo: TObject;
  {Gets additional information about event.
    @return Object that provides required information.
  }
begin
  Result := fInfo;
end;

function _TDatabase.TEventInfo.GetKind: TDatabaseChangeEventKind;
  {Gets kind (type) of event.
    @return Event kind.
  }
begin
  Result := fKind;
end;

{ TDBDataItemFactory }

function TDBDataItemFactory.CreateCategory(const CatID: string;
  const Data: TCategoryData): TCategory;
  {Creates a new category object.
    @param CatID [in] ID of new category. Must be unique.
    @param UserDefined [in] True if category is user defined, False if not.
    @param Data [in] Record describing category's properties.
    @return Instance of new category object.
  }
begin
  Result := TCategoryEx.Create(CatID, Data);
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
  Result := TSnippet.Create(TSnippetID.Create(Name));
  Result.SetProps(Props);
end;

{ TUserDataProvider }

function TUserDataProvider.GetCategoryProps(const Cat: TCategory):
  TCategoryData;
  {Retrieves all the properties of a category.
    @param Cat [in] Category for which data is requested.
    @return Record containing property data.
  }
begin
  Result.Desc := Cat.Description;
end;

function TUserDataProvider.GetCategorySnippets(const Cat: TCategory):
  IStringList;
  {Retrieves names of all user-defined snippets that belong to a category.
    @param Cat [in] Category for which snippet names are requested.
    @return Required list of snippet names.
  }
var
  SnippetID: TSnippetID;  // references ID of each snippet in category
begin
  Result := TIStringList.Create;
  for SnippetID in Cat.SnippetIDs do
    Result.Add(SnippetID.ToString);
end;

function TUserDataProvider.GetSnippetProps(const Snippet: TSnippet):
  TSnippetData;
  {Retrieves all the properties of a snippet.
    @param Snippet [in] Snippet for which data is requested.
    @return Record containing property data.
  }
begin
  Result := Snippet.GetProps;
end;

function TUserDataProvider.GetSnippetRefs(const Snippet: TSnippet):
  TSnippetReferences;
  {Retrieves information about all the references of a snippet.
    @param Snippet [in] Snippet for which information is requested.
    @return Record containing references.
  }
begin
  Result := Snippet.GetReferences;
end;

{ TDatabase }

procedure TDatabase.Finalize;
begin
  fSnippetsTable.Free;
  inherited;
end;

function TDatabase.GetAllSnippets: ISnippetIDList;
var
  Snippet: TDBSnippet;
begin
  Result := TSnippetIDList.Create(fSnippetsTable.Size);
  for Snippet in fSnippetsTable do
    Result.Add(Snippet.ID);
end;

function TDatabase.GetAllTags: ITagSet;
begin
  Result := TTagSet.Create(fAllTags);
end;

class function TDatabase.GetInstance: TDatabase;
begin
  if not Assigned(fInstance) then
    fInstance := TDatabase.Create;
  Result := fInstance;
end;

procedure TDatabase.Initialize;
begin
  inherited;
  fSnippetsTable := TDBSnippetsTable.Create;
  fAllTags := TTagSet.Create;
end;

function TDatabase.IsEmpty: Boolean;
begin
  Result := fSnippetsTable.Size = 0;
end;

function TDatabase.LookupEditableSnippet(const ASnippetID: TSnippetID):
  IEditableSnippet;
var
  Row: TDBSnippet;
begin
  Row := fSnippetsTable.Get(ASnippetID);
  Result := Row.CloneAsEditable;
end;

function TDatabase.LookupReadOnlySnippet(const ASnippetID: TSnippetID;
  const ARequiredProperties: TDBSnippetProps): IReadOnlySnippet;
var
  Row: TDBSnippet;
begin
  Row := fSnippetsTable.Get(ASnippetID);
  Result := Row.CloneAsReadOnly(ARequiredProperties);
end;

function TDatabase.SelectSnippets(Filter: IDBFilter): ISnippetIDList;
var
  Snippet: TDBSnippet;
begin
  Result := TSnippetIDList.Create;
  for Snippet in fSnippetsTable do
    if Filter.Match(Snippet.CloneAsReadOnly(Filter.RequiredProperties)) then
      Result.Add(Snippet.ID)
end;

function TDatabase.SnippetCount: Integer;
begin
  Result := fSnippetsTable.Size;
end;

function TDatabase.SnippetExists(const ASnippetID: TSnippetID): Boolean;
begin
  Result := fSnippetsTable.Contains(ASnippetID);
end;

initialization


finalization

// Free the singleton
_PvtDatabase := nil;

end.

