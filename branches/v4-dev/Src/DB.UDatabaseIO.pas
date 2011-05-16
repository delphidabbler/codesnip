{
 * DB.UDatabaseIO.pas
 *
 * Implements objects that can load data into the Database object from both the
 * user and main databases. Also provides a class that can write the user
 * database to storage.
 *
 * Uses file I/O interface implementations to read / write the physical files.
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
 * The Original Code is DB.UDatabaseIO.pas, formerly USnippetsIO.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit DB.UDatabaseIO;


interface


uses
  // Project
  DB.UCategory, DB.UMain, DB.USnippet, UBaseObjects, UExceptions;


type

  {
  IDatabaseLoader:
    Interface to object that can load data into the Database object from
    storage.
  }
  IDatabaseLoader = interface(IInterface)
    ['{C6AF94FC-F56F-44AE-9E79-3B0CD0BB21D4}']
    procedure Load(const SnipList: TSnippetList;
      const Categories: TCategoryList;
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
  IDatabaseWriter = interface(IInterface)
    ['{F46EE2E3-68A7-4877-9E04-192D15D29BB1}']
    procedure Write(const SnipList: TSnippetList;
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
    class function CreateMainDBLoader: IDatabaseLoader;
      {Creates an object to use to load the main database.
        @return Required object instance.
      }
    class function CreateUserDBLoader: IDatabaseLoader;
      {Creates an object to use to load the user database.
        @return Required object instance.
      }
    class function CreateWriter: IDatabaseWriter;
      {Create an object that can write user defined data from the Database
      object to storage.
        @return Required object instance.
      }
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
  DB.UFileIOIntf, DB.UIniDataReader, DB.UNulDataReader, DB.UXMLDataIO, UAppInfo,
  UConsts, UIStringList, UReservedCategories, USnippetIDs;


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
  TDatabaseLoader = class(TInterfacedObject, IDatabaseLoader)
  strict private
    fReader: IDataReader;         // Object used to read data from storage
    fSnipList: TSnippetList;      // Receives list of snippets
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
      const SnipList: TSnippetList): TSnippet; virtual; abstract;
      {Finds the snippet object with a specified name.
        @param SnippetName [in] Name of required snippet.
        @param SnipList [in] List of snippets to search.
        @return Reference to required snippet object or nil if snippet is not
          found.
      }
    function IsNativeSnippet(const Snippet: TSnippet): Boolean;
      virtual; abstract;
      {Checks if a snippet is native (belongs) to the database being read.
        @param Snippet [in] Snippet to test.
        @return True if snippet is native, False if not.
      }
    function IsUserDatabase: Boolean; virtual; abstract;
      {Checks if the database is the user database.
        @return True if the database is the user database, False if not.
      }
    function ErrorMessageHeading: string; virtual; abstract;
      {Returns heading to use in error messages. Should identify the database.
        @return Required heading.
      }
    procedure LoadCategories; virtual;
      {Loads all categories from storage.
      }
    procedure CreateCategory(const CatName: string;
      const CatData: TCategoryData);
      {Creates a new category and adds it to the categories list.
        @param CatName [in] Name of category.
        @param CatData [in] Properties of category.
      }
    property Categories: TCategoryList read fCategories;
      {Reference to category list}
  public
    { IDatabaseLoader method }
    procedure Load(const SnipList: TSnippetList;
      const Categories: TCategoryList;
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
  TMainDatabaseLoader:
    Class that updates Database object with data read from main database.
  }
  TMainDatabaseLoader = class(TDatabaseLoader, IDatabaseLoader)
  strict protected
    function CreateReader: IDataReader; override;
      {Creates reader object. If main database doesn't exist a nul reader is
      created.
        @return Reader object instance.
      }
    function FindSnippet(const SnippetName: string;
      const SnipList: TSnippetList): TSnippet; override;
      {Finds the snippet object with a specified name in the main database.
        @param SnippetName [in] Name of required snippet.
        @param SnipList [in] List of snippets to search.
        @return Reference to required snippet object or nil if snippet is not
          found.
      }
    function IsNativeSnippet(const Snippet: TSnippet): Boolean; override;
      {Checks if a snippet is native (belongs) to the main database.
        @param Snippet [in] Snippet to test.
        @return True if snippet is native, False if not.
      }
    function IsUserDatabase: Boolean; override;
      {Checks if the database is the user database.
        @return False - this is not the user database.
      }
    function ErrorMessageHeading: string; override;
      {Returns heading to use in error messages. Identifies main database.
        @return Required heading.
      }
  end;

  {
  TUserDatabaseLoader:
    Class that updates Database object with data read from user database.
  }
  TUserDatabaseLoader = class(TDatabaseLoader, IDatabaseLoader)
  strict protected
    function CreateReader: IDataReader; override;
      {Creates reader object. If user database doesn't exist a nul reader is
      created.
        @return Reader object instance.
      }
    function FindSnippet(const SnippetName: string;
      const SnipList: TSnippetList): TSnippet; override;
      {Finds the snippet object with a specified name. If snippet is not in this
      (user) database the main database is searched.
        @param SnippetName [in] Name of required snippet.
        @param SnipList [in] List of snippets to search.
        @return Reference to required snippet object or nil if snippet is not
          found.
      }
    function IsNativeSnippet(const Snippet: TSnippet): Boolean; override;
      {Checks if a snippet is native (belongs) to the user database.
        @param Snippet [in] Snippet to test.
        @return True if snippet is native, False if not.
      }
    function IsUserDatabase: Boolean; override;
      {Checks if the database is the user database.
        @return True - this is the user database.
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
    IDatabaseWriter
  )
  strict private
    fWriter: IDataWriter;             // Object used to write to storage
    fSnipList: TSnippetList;          // List of snippets to be written
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
    procedure Write(const SnipList: TSnippetList;
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

class function TDatabaseIOFactory.CreateMainDBLoader: IDatabaseLoader;
  {Creates an object to use to load the main database.
    @return Required object instance.
  }
begin
  Result := TMainDatabaseLoader.Create;
end;

class function TDatabaseIOFactory.CreateUserDBLoader: IDatabaseLoader;
  {Creates an object to use to load the user database.
    @return Required object instance.
  }
begin
  Result := TUserDatabaseLoader.Create;
end;

class function TDatabaseIOFactory.CreateWriter: IDatabaseWriter;
  {Create an object that can write user defined data from the Database object to
  storage.
    @return Required object instance.
  }
begin
  Result := TDatabaseWriter.Create;
end;

{ TDatabaseLoader }

procedure TDatabaseLoader.CreateCategory(const CatName: string;
  const CatData: TCategoryData);
  {Creates a new category and adds it to the categories list.
    @param CatName [in] Name of category.
    @param CatData [in] Properties of category.
  }
begin
  fCategories.Add(fFactory.CreateCategory(CatName, IsUserDatabase, CatData));
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

procedure TDatabaseLoader.Load(const SnipList: TSnippetList;
  const Categories: TCategoryList; const DBDataItemFactory: IDBDataItemFactory);
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
      LoadSnippets(Category);
    // Build XRef, Depends and Units reference list of each snippet for this
    // database
    for Snippet in fSnipList do
    begin
      if IsNativeSnippet(Snippet) then
        LoadReferences(Snippet);
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
  CatNames: IStringList;    // list of names of categories
  CatName: string;          // name of each category
  Category: TCategory;      // a category object
  CatData: TCategoryData;   // properties of a category
begin
  // Get name of all categories
  CatNames := fReader.GetAllCatNames;
  // Loop through each category by name
  for CatName in CatNames do
  begin
    // Check if category exists, creating it if not
    Category := fCategories.Find(CatName);
    if not Assigned(Category) then
    begin
      fReader.GetCatProps(CatName, CatData);
      CreateCategory(CatName, CatData);
    end;
  end;
end;

procedure TDatabaseLoader.LoadReferences(const Snippet: TSnippet);
  {Loads all of a snippet's references.
    @param Snippet [in] Snippet for which references are required.
  }

  // ---------------------------------------------------------------------------
  procedure LoadSnippetReferences(const RefList: TSnippetList;
    const RefNames: IStringList);
    {Creates a snippet list from names of snippets in a string list. If no
    snippet with a given name is found no matching entry is added to snippet
    list.
      @param RefList [in] List to receive referenced snippets.
      @param RefNames [in] List of snippet names.
    }
  var
    RefName: string;        // referenced snippet name
    Reference: TSnippet;    // referenced snippet object
  begin
    for RefName in RefNames do
    begin
      Reference := FindSnippet(RefName, fSnipList);
      if Assigned(Reference) then
        RefList.Add(Reference);
    end;
  end;
  // ---------------------------------------------------------------------------

begin
  LoadSnippetReferences(
    Snippet.Depends, fReader.GetSnippetDepends(Snippet.Name)
  );
  LoadSnippetReferences(
    Snippet.XRef, fReader.GetSnippetXRefs(Snippet.Name)
  );
  fReader.GetSnippetUnits(Snippet.Name).CopyTo(Snippet.Units);
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
  FillChar(SnippetProps, SizeOf(SnippetProps), 0);
  // Get names of all snippets in category
  SnippetNames := fReader.GetCatSnippets(Cat.Category);
  // Process each snippet name in list
  for SnippetName in SnippetNames do
  begin
    // Check if snippet exists in current database and add it to list if not
    Snippet := fSnipList.Find(SnippetName, IsUserDatabase);
    if not Assigned(Snippet) then
    begin
      fReader.GetSnippetProps(SnippetName, SnippetProps);
      Snippet := fFactory.CreateSnippet(
        SnippetName, IsUserDatabase, SnippetProps
      );
      fSnipList.Add(Snippet);
    end;
    // Add snippet to database only if it belongs to this database
    if IsNativeSnippet(Snippet) then
      Cat.Snippets.Add(Snippet);
  end;
end;

{ TMainDatabaseLoader }

function TMainDatabaseLoader.CreateReader: IDataReader;
  {Creates reader object. If main database doesn't exist a nul reader is
  created.
    @return Reader object instance.
  }
begin
  Result := TIniDataReader.Create(TAppInfo.AppDataDir);
  if not Result.DatabaseExists then
    Result := TNulDataReader.Create;
end;

function TMainDatabaseLoader.ErrorMessageHeading: string;
  {Returns heading to use in error messages. Identifies main database.
    @return Required heading.
  }
resourcestring
  sError = 'Error loading the CodeSnip database:';
begin
  Result := sError;
end;

function TMainDatabaseLoader.FindSnippet(const SnippetName: string;
  const SnipList: TSnippetList): TSnippet;
  {Finds the snippet object with a specified name in the main database.
    @param SnippetName [in] Name of required snippet.
    @param SnipList [in] List of snippets to search.
    @return Reference to required snippet object or nil if snippet is not found.
  }
begin
  // We only search main database
  Result := SnipList.Find(SnippetName, False);
end;

function TMainDatabaseLoader.IsNativeSnippet(const Snippet: TSnippet): Boolean;
  {Checks if a snippet is native (belongs) to the main database.
    @param Snippet [in] Snippet to test.
    @return True if snippet is native, False if not.
  }
begin
  Result := not Snippet.UserDefined;
end;

function TMainDatabaseLoader.IsUserDatabase: Boolean;
  {Checks if the database is the user database.
    @return False - this is not the user database.
  }
begin
  Result := False;
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
  const SnipList: TSnippetList): TSnippet;
  {Finds the snippet object with a specified name. If snippet is not in this
  (user) database the main database is searched.
    @param SnippetName [in] Name of required snippet.
    @param SnipList [in] List of snippets to search.
    @return Reference to required snippet object or nil if snippet is not found.
  }
begin
  // Search in user database
  Result := SnipList.Find(SnippetName, True);
  if not Assigned(Result) then
    // Not in user database: try main database
    Result := SnipList.Find(SnippetName, False);
end;

function TUserDatabaseLoader.IsNativeSnippet(const Snippet: TSnippet): Boolean;
  {Checks if a snippet is native (belongs) to the user database.
    @param Snippet [in] Snippet to test.
    @return True if snippet is native, False if not.
  }
begin
  Result := Snippet.UserDefined;
end;

function TUserDatabaseLoader.IsUserDatabase: Boolean;
  {Checks if the database is the user database.
    @return True - this is the user database.
  }
begin
  Result := True;
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

procedure TDatabaseWriter.Write(const SnipList: TSnippetList;
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
  Props: TCategoryData;   // categpry properties
  SnipList: IStringList;  // list of names of snippets in a category
begin
  for Cat in fCategories do
  begin
    Props := fProvider.GetCategoryProps(Cat);
    fWriter.WriteCatProps(Cat.Category, Props);
    SnipList := fProvider.GetCategorySnippets(Cat);
    fWriter.WriteCatSnippets(Cat.Category, SnipList);
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
      Result.Add(ID.Name);
  end;
  // ---------------------------------------------------------------------------

var
  Snippet: TSnippet;        // loops through each snippet in list
  Props: TSnippetData;      // snippet properties
  Refs: TSnippetReferences; // snippet references
begin
  for Snippet in fSnipList do
  begin
    // Only write user-defined snippets
    if Snippet.UserDefined then
    begin
      // Get and write a snippet's properties
      Props := fProvider.GetSnippetProps(Snippet);
      fWriter.WriteSnippetProps(Snippet.Name, Props);
      // Get and write a snippet's references
      Refs := fProvider.GetSnippetRefs(Snippet);
      fWriter.WriteSnippetUnits(Snippet.Name, Refs.Units);
      fWriter.WriteSnippetDepends(Snippet.Name, IDListToStrings(Refs.Depends));
      fWriter.WriteSnippetXRefs(Snippet.Name, IDListToStrings(Refs.XRef));
    end;
  end;
end;

end.

