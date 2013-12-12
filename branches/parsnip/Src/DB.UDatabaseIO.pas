{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements objects that can load data into the Database object from both the
 * user and main databases. Also provides a class that can write the user
 * database to storage.
 *
 * Uses file I/O interface implementations to read / write the physical files.
}


unit DB.UDatabaseIO;


interface


uses
  // Project
  CS.Database.IO.Types,
  CS.Database.Types,
  DB.UCategory,
  DB.UMain,
  DB.USnippet,
  UBaseObjects,
  UExceptions;


type

  {
  IDatabaseLoader:
    Interface to object that can load data into the Database object from
    storage.
  }
  _IDatabaseLoader = interface(IInterface)
    ['{C6AF94FC-F56F-44AE-9E79-3B0CD0BB21D4}']
    procedure Load(const SnipList: _TSnippetList;
      const Categories: TCategoryList;
      Tags: ITagSet;
      const DBDataItemFactory: IDBDataItemFactory);
      {Loads data from storage and updates database object.
        @param SnipList [in] Receives information about each snippet in the
          database.
        @param Categories [in] Receives information about each category in the
          database.
        @param DBDataItemFactory [in] Object used to create new categories and
          snippets.
      }
  end;

  {
  IDatabaseWriter:
    Interface to object that can write data from the user-defined component of
    the Database object to storage.
  }
  _IDatabaseWriter = interface(IInterface)
    ['{F46EE2E3-68A7-4877-9E04-192D15D29BB1}']
    procedure Write(const SnipList: _TSnippetList;
      const Categories: TCategoryList; const Provider: IDBDataProvider);
      {Writes data from Database object to storage.
        @param SnipList [in] Contains information about each snippet in the
          database.
        @param Categories [in] Contains information about each category in the
          database.
        @param Privider [in] Object used to obtain details of the data to be
          written to the Database object
      }
  end;

  {
  TDatabaseIOFactory:
    Factory class that can create instances of writer and loader objects for the
    Database object.
  }
  TDatabaseIOFactory = class(TNoConstructObject)
  public
    ///  <summary>Creates and returns an instance of a database loader object
    ///  that can be used to load the database.</summary>
    ///  <remarks>If a database in native format is present a loader for that
    ///  database is returned. If there is no native database then any legacy
    ///  XML format database is looked for and a loader for that is returned if
    ///  found. If there is no database present at all then a special "empty
    ///  database" loader is returned.</remarks>
    class function CreateLoader: IDatabaseLoader;
    class function CreateUserDBLoader: _IDatabaseLoader;
      {Creates an object to use to load the user database.
        @return Required object instance.
      }
    ///  <summary>Creates and returns an instance of a an object that can be
    ///  used to save the database.</summary>
    ///  <remarks>This method always returns an writer object that saves in
    ///  native format. No other output formats are supported.</remarks>
    class function CreateWriter: IDatabaseWriter;
  end;

  {
  EDatabaseLoader:
    Class of exception raised by database loader objects.
  }
  EDatabaseLoader = class(ECodeSnip);


implementation


uses
  // Delphi
  SysUtils,
  // Project
  CS.Database.IO.Empty,
  CS.Database.IO.Legacy,
  CS.Database.IO.Native,
  CS.Database.Snippets,
  CS.Database.Tags,
  DBIO.UFileIOIntf,
  DBIO.UNulDataReader,
  DBIO.UXMLDataIO,
  IntfCommon,
  UAppInfo,
  UConsts,
  UIStringList,
  UReservedCategories;


type

  {
  TDatabaseLoaderClass:
    Class reference to TDatabaseLoader descendants.
  }
  TDatabaseLoaderClass = class of TDatabaseLoader;

  {
  TDatabaseLoader:
    Abstract base class for objects that can load data into the Database object
    from storage.
  }
  TDatabaseLoader = class(TInterfacedObject, _IDatabaseLoader)
  strict private
    fReader: IDataReader;         // Object used to read data from storage
    fSnipList: _TSnippetList;      // Receives list of snippets
    fCategories: TCategoryList;   // Receives list of categories
    fFactory: IDBDataItemFactory; // Object creates new categories and snippets
    procedure LoadSnippets(const Cat: TCategory);
      {Loads all snippets in a category.
        @param Cat [in] Category to be loaded.
      }
    procedure LoadReferences(const Snippet: TSnippet);
      {Loads all of a snippet's references.
        @param Snippet [in] Snippet for which references are required.
      }
    procedure HandleException(const E: Exception);
      {Handles exceptions generated by loader and converts ECodeSnip and
      descendant exceptions into EDatabaseLoader exceptions.
        @param E [in] Exception to be handled.
        @except Exception always raised.
      }
  strict protected
    function CreateReader: IDataReader; virtual; abstract;
      {Creates reader object for the database. If database doesn't exist a nul
      reader must be created.
        @return Reader object instance.
      }
    function FindSnippet(const SnippetName: string;
      const SnipList: _TSnippetList): TSnippet; virtual; abstract;
      {Finds the snippet object with a specified name.
        @param SnippetName [in] Name of required snippet.
        @param SnipList [in] List of snippets to search.
        @return Reference to required snippet object or nil if snippet is not
          found.
      }
    function ErrorMessageHeading: string; virtual; abstract;
      {Returns heading to use in error messages. Should identify the database.
        @return Required heading.
      }
    procedure LoadCategories; virtual;
      {Loads all categories from storage.
      }
    procedure CreateCategory(const CatID: string;
      const CatData: TCategoryData);
      {Creates a new category and adds it to the categories list.
        @param CatID [in] ID of category.
        @param CatData [in] Properties of category.
      }
    property Categories: TCategoryList read fCategories;
      {Reference to category list}
  public
    { IDatabaseLoader method }
    procedure Load(const SnipList: _TSnippetList;
      const Categories: TCategoryList;
      Tags: ITagSet;
      const DBDataItemFactory: IDBDataItemFactory);
      {Loads data from storage and updates database object.
        @param SnipList [in] Receives information about each snippet in the
          database.
        @param Categories [in] Receives information about each category in the
          database.
        @param DBDataItemFactory [in] Object used to create new categories and
          snippets.
      }
  end;

  {
  TUserDatabaseLoader:
    Class that updates Database object with data read from user database.
  }
  TUserDatabaseLoader = class(TDatabaseLoader, _IDatabaseLoader)
  strict protected
    function CreateReader: IDataReader; override;
      {Creates reader object. If user database doesn't exist a nul reader is
      created.
        @return Reader object instance.
      }
    function FindSnippet(const SnippetName: string;
      const SnipList: _TSnippetList): TSnippet; override;
      {Finds the snippet object with a specified name. If snippet is not in this
      (user) database the main database is searched.
        @param SnippetName [in] Name of required snippet.
        @param SnipList [in] List of snippets to search.
        @return Reference to required snippet object or nil if snippet is not
          found.
      }
    function ErrorMessageHeading: string; override;
      {Returns heading to use in error messages. Identifies main database.
        @return Required heading.
      }
    procedure LoadCategories; override;
      {Loads all categories from storage and adds user category if not present.
      }
  end;

  {
  TDatabaseWriter:
    Object used to write data from user database to storage.
  }
  TDatabaseWriter = class(TInterfacedObject,
    _IDatabaseWriter
  )
  strict private
    fWriter: IDataWriter;             // Object used to write to storage
    fSnipList: _TSnippetList;          // List of snippets to be written
    fCategories: TCategoryList;       // List of categories to be written
    fProvider: IDBDataProvider;       // Object used to get data to be written
    function CreateWriter: IDataWriter;
      {Creates object that can write data for user-defined database to storage.
        @return Requied writer object.
      }
    procedure WriteCategories;
      {Writes information about categories to storage.
      }
    procedure WriteSnippets;
      {Writes information about all snippets to storage.
      }
  public
    { IDatabaseWriter method }
    procedure Write(const SnipList: _TSnippetList;
      const Categories: TCategoryList;
      const Provider: IDBDataProvider);
      {Writes data from Database object to storage.
        @param SnipList [in] Contains information about each snippet in the
          database.
        @param Categories [in] Contains information about each category in the
          database.
        @param Provider [in] Object used to obtain details of the data to be
          written from the Database object
      }
  end;

{ TDatabaseIOFactory }

class function TDatabaseIOFactory.CreateLoader: IDatabaseLoader;
begin
  Result := TDBNativeReader.Create(TAppInfo.UserDataDir);
  if Result.DatabaseExists then
    Exit;
  Result := TDBLegacyUserDBReader.Create(TAppInfo.UserDataDir);
  if not Result.DatabaseExists then
    Exit;
  Result := TEmptyDBReader.Create;
end;

class function TDatabaseIOFactory.CreateUserDBLoader: _IDatabaseLoader;
  {Creates an object to use to load the user database.
    @return Required object instance.
  }
begin
  Result := TUserDatabaseLoader.Create;
end;

class function TDatabaseIOFactory.CreateWriter: IDatabaseWriter;
begin
  Result := TDBNativeWriter.Create(TAppInfo.UserDataDir);
end;

{ TDatabaseLoader }

procedure TDatabaseLoader.CreateCategory(const CatID: string;
  const CatData: TCategoryData);
  {Creates a new category and adds it to the categories list.
    @param CatID [in] ID of category.
    @param CatData [in] Properties of category.
  }
begin
  fCategories.Add(fFactory.CreateCategory(CatID, CatData));
end;

procedure TDatabaseLoader.HandleException(const E: Exception);
  {Handles exceptions generated by loader and converts ECodeSnip and descendant
  exceptions into EDatabaseLoader exceptions.
    @param E [in] Exception to be handled.
    @except Exception always raised.
  }
begin
  if E is ECodeSnip then
    // add message header identifying database to existing message
    raise EDatabaseLoader.Create(ErrorMessageHeading + EOL2 + E.Message)
  else
    raise E;
end;

procedure TDatabaseLoader.Load(const SnipList: _TSnippetList;
  const Categories: TCategoryList; Tags: ITagSet;
  const DBDataItemFactory: IDBDataItemFactory);
  {Loads data from storage and updates database object.
    @param SnipList [in] Receives information about each snippet in the
      database.
    @param Categories [in] Receives information about each category in the
      database.
    @param DBDataItemFactory [in] Object used to create new categories and
      snippets.
  }
var
  Category: TCategory;  // a category
  Snippet: TSnippet;    // a snippet

  function TrimBadSnippets(IDList: ISnippetIDList;
    const AllSnippets: _TSnippetList): ISnippetIDList;
  var
    SnippetID: TSnippetID;
  begin
    Result := TSnippetIDList.Create;
    for SnippetID in IDList do
      if AllSnippets.Find(SnippetID) <> nil then
        Result.Add(SnippetID);
    Result := (IDList as IClonable).Clone as ISnippetIDList;
  end;

var
  TagSet: ITagSet;
begin
  // Create reader object that can access data storage
  fReader := CreateReader;
  // Record snippets and categories list in fields
  // Do not clear snippet or category lists: may already contain data
  fSnipList := SnipList;
  fCategories := Categories;
  fFactory := DBDataItemFactory;
  try
    // Load categories
    LoadCategories;
    // Load snippets in each category
    for Category in fCategories do
    begin
      Tags.Add(TTag.Create(TTag.MakeValidTagString(Category.Description)));
      LoadSnippets(Category);
    end;
    // Build XRef, Depends and Units reference list of each snippet for this
    // database
    for Snippet in fSnipList do
      LoadReferences(Snippet);
    // Trim out in invalid references and add tag based on category
    for Snippet in fSnipList do
    begin
      Snippet.SetRequiredSnippets(
        TrimBadSnippets(Snippet.RequiredSnippets, fSnipList)
      );
      Snippet.SetXRefs(
        TrimBadSnippets(Snippet.XRefs, fSnipList)
      );
      Category := fCategories.Find(Snippet.Category);
      if Assigned(Category) then
      begin
        TagSet := TTagSet.Create;
        TagSet.Add(TTag.Create(Category.Description));
        Snippet.SetTags(TagSet);
      end;
    end;
  except
    on E: Exception do
      HandleException(E);
  end;
end;

procedure TDatabaseLoader.LoadCategories;
  {Loads all categories from storage
  }
var
  CatIDs: IStringList;      // list of ids of categories
  CatID: string;            // name of each category
  Category: TCategory;      // a category object
  CatData: TCategoryData;   // properties of a category
begin
  // Get name of all categories
  CatIDs := fReader.GetAllCatIDs;
  // Loop through each category by name
  for CatID in CatIDs do
  begin
    // Check if category exists, creating it if not
    Category := fCategories.Find(CatID);
    if not Assigned(Category) then
    begin
      fReader.GetCatProps(CatID, CatData);
      CreateCategory(CatID, CatData);
    end;
  end;
end;

procedure TDatabaseLoader.LoadReferences(const Snippet: TSnippet);
  {Loads all of a snippet's references.
    @param Snippet [in] Snippet for which references are required.
  }

  function BuildReferences(Src: IStringList): ISnippetIDList;
  var
    IDStr: string;
  begin
    Result := TSnippetIDList.Create;
    for IDStr in Src do
      Result.Add(TSnippetID.Create(IDStr));
  end;

begin
  Snippet.SetRequiredSnippets(
    BuildReferences(fReader.GetSnippetDepends(Snippet.ID.ToString))
  );
  Snippet.SetXRefs(
    BuildReferences(fReader.GetSnippetXRefs(Snippet.ID.ToString))
  );
  Snippet.SetRequiredModules(fReader.GetSnippetUnits(Snippet.ID.ToString));
end;

procedure TDatabaseLoader.LoadSnippets(const Cat: TCategory);
  {Loads all snippets in a category.
    @param Cat [in] Category to be loaded.
  }
var
  SnippetNames: IStringList;    // list of names of snippets in category
  SnippetProps: TSnippetData;   // properties of a snippet
  SnippetName: string;          // each name in name list
  Snippet: TSnippet;            // references a snippet object
begin
  SnippetProps.Init;
  // Get names of all snippets in category
  SnippetNames := fReader.GetCatSnippets(Cat.ID);
  // Process each snippet name in list
  for SnippetName in SnippetNames do
  begin
    // Check if snippet exists in current database and add it to list if not
    Snippet := fSnipList.Find(SnippetName);
    if not Assigned(Snippet) then
    begin
      fReader.GetSnippetProps(SnippetName, SnippetProps);
      Snippet := fFactory.CreateSnippet(SnippetName, SnippetProps);
      fSnipList.Add(Snippet);
    end;
    Cat.SnippetIDs.Add(Snippet.ID);
  end;
end;

{ TUserDatabaseLoader }

function TUserDatabaseLoader.CreateReader: IDataReader;
  {Creates reader object. If user database doesn't exist a nul reader is
  created.
    @return Reader object instance.
  }
begin
  Result := TXMLDataReader.Create(TAppInfo.UserDataDir);
  if not Result.DatabaseExists then
    Result := TNulDataReader.Create;
end;

function TUserDatabaseLoader.ErrorMessageHeading: string;
  {Returns heading to use in error messages. Identifies main database.
    @return Required heading.
  }
resourcestring
  sError = 'Error loading the user defined database:';
begin
  Result := sError;
end;

function TUserDatabaseLoader.FindSnippet(const SnippetName: string;
  const SnipList: _TSnippetList): TSnippet;
  {Finds the snippet object with a specified name.
    @param SnippetName [in] Name of required snippet.
    @param SnipList [in] List of snippets to search.
    @return Reference to required snippet object or nil if snippet is not found.
  }
begin
  // Search in user database
  Result := SnipList.Find(SnippetName);
end;

procedure TUserDatabaseLoader.LoadCategories;
  {Loads all categories from storage and adds user and imports categories if not
  present.
  }
var
  ResCatIdx: Integer;                 // loops thru all reserved categories
  ResCatInfo: TReservedCategoryInfo;  // info about a reserved category
begin
  // Get all categories from storage
  inherited;
  // Add default user-defined categories if not present
  for ResCatIdx := 0 to Pred(TReservedCategories.Count) do
  begin
    ResCatInfo := TReservedCategories.Info(ResCatIdx);
    if Categories.Find(ResCatInfo.Name) = nil then
      CreateCategory(ResCatInfo.Name, ResCatInfo.Data);
  end;
end;

{ TDatabaseWriter }

function TDatabaseWriter.CreateWriter: IDataWriter;
  {Creates object that can write data for user-defined items from Database to
  storage.
    @return Requied writer object.
  }
begin
  Result := TXMLDataWriter.Create(TAppInfo.UserDataDir);
end;

procedure TDatabaseWriter.Write(const SnipList: _TSnippetList;
  const Categories: TCategoryList; const Provider: IDBDataProvider);
  {Writes data from Database object to storage.
    @param SnipList [in] Contains information about each snippet in the
      database.
    @param Categories [in] Contains information about each category in the
      database.
    @param Provider [in] Object used to obtain details of the Database data to
      be written.
  }
begin
  fSnipList := SnipList;
  fCategories := Categories;
  fProvider := Provider;
  fWriter := CreateWriter;
  fWriter.Initialise;
  WriteCategories;
  WriteSnippets;
  fWriter.Finalise;
end;

procedure TDatabaseWriter.WriteCategories;
  {Writes information about categories to storage.
  }
var
  Cat: TCategory;         // loops through each category
  Props: TCategoryData;   // category properties
  SnipList: IStringList;  // list of names of snippets in a category
begin
  for Cat in fCategories do
  begin
    Props := fProvider.GetCategoryProps(Cat);
    fWriter.WriteCatProps(Cat.ID, Props);
    SnipList := fProvider.GetCategorySnippets(Cat);
    fWriter.WriteCatSnippets(Cat.ID, SnipList);
  end;
end;

procedure TDatabaseWriter.WriteSnippets;
  {Writes information about all snippets to storage.
  }

  // ---------------------------------------------------------------------------
  function IDListToStrings(const IDList: ISnippetIDList): IStringList;
    {Copies snippet names from a snippet ID list to a string list.
      @param IDList [in] Snippet ID List to be copied.
      @return String list containing names.
    }
  var
    ID: TSnippetID; // each id in snippet id list
  begin
    Result := TIStringList.Create;
    for ID in IDList do
      Result.Add(ID.ToString);
  end;
  // ---------------------------------------------------------------------------

var
  Snippet: TSnippet;        // loops through each snippet in list
  Props: TSnippetData;      // snippet properties
  Refs: TSnippetReferences; // snippet references
begin
  for Snippet in fSnipList do
  begin
    // Get and write a snippet's properties
    Props := fProvider.GetSnippetProps(Snippet);
    fWriter.WriteSnippetProps(Snippet.ID.ToString, Props);
    // Get and write a snippet's references
    Refs := fProvider.GetSnippetRefs(Snippet);
    fWriter.WriteSnippetUnits(Snippet.ID.ToString, Refs.RequiredModules);
    fWriter.WriteSnippetDepends(
      Snippet.ID.ToString, IDListToStrings(Refs.RequiredSnippets)
    );
    fWriter.WriteSnippetXRefs(Snippet.ID.ToString, IDListToStrings(Refs.XRefs));
  end;
end;

end.


